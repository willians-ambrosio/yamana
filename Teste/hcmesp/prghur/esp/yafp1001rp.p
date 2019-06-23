/*************************************************************/
{include/i-prgvrs.i yafp1001RP 1.00.00.000}  /*** 010000 ***/
{include/i_fnctrad.i}
{include/i-rpvar.i}

{utp/ut-glob.i}

{prghur/esp/yafp1001tt.i}  /* Parametro */
    
def temp-table tt-raw-digita
    field raw-digita as raw.
def input parameter  raw-param as raw no-undo.
def input parameter  table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

{include/i_dbvers.i}

def var h-acomp as handle no-undo.
run utp/ut-acomp.p persistent set h-acomp.  



{include/i-rpout.i &tofile=tt-param.arquivo}

def stream s-rem.

def temp-table tt-fp0820 no-undo
    field cdn_empresa           like cta_mdo_efp.cdn_empresa 
    field cdn_estab             like cta_mdo_efp.cdn_estab   
    field cdn_event_fp          like cta_mdo_efp.cdn_event_fp
    field des_event_fp          like event_fp.des_event_fp
    field chave                 as integer
    field cod_tip_mdo           like cta_mdo_efp.cod_tip_mdo
    field cod_rh_ccusto         like movto_calcul_func.cod_rh_ccusto
    field cod_rh_cta_ctbl       like cta_mdo_efp.cod_rh_cta_ctbl_db
    field valor                as dec format 99999999999.99
    field idi_tipo             as int /* 1 - Folha, 2 - Provis∆o, 3 - Encargos */
    index id-tt-fp0820 as primary unique cdn_empresa /*cdn_estab*/ cdn_event_fp /*cod_tip_mdo*/ cod_rh_ccusto cod_rh_cta_ctbl idi_tipo.



/**** variaveis ****/

def var ind                   as int                                no-undo.
def var v_chave               as integer                            no-undo.
def var v_cod_rh_cta_ctbl   like cta_mdo_efp.cod_rh_cta_ctbl_db     no-undo.
def var v_cod_rh_ccusto     like movto_calcul_func.cod_rh_ccusto    no-undo.
def var v_dat_fim_mes         as date  format '99/99/9999'          no-undo.
def var c-arq-gerado          as character  format 'x(300)'         no-undo.

def var i-ev-prov                      as char format "x(87)" no-undo
    initial "901,902,903,904,905,906,907,908,909,910,911,912,913,914,915,916,917,918,921,922,923,925,926,927,928,929,937,939,934,935,919,920,932,936,958,959,960,938".

find tt-param no-lock no-error. 
run pi-inicializar in h-acomp (input "Exportaá∆o em andamento").

/***************************** Inicio do Programa ***************************/
assign v_dat_fim_mes = (date(tt-param.v_mes_refer,15,tt-param.v_ano_refer) + 30) - 1.

/* là o c†lculo da folha do màs\ano refer informado no programa */
for each funcionario fields(cdn_empresa cdn_estab cdn_funcionario) no-lock 
   where funcionario.cdn_empresa      = tt-param.v_cdn_empres_usuar
     and funcionario.cdn_estab       >= tt-param.cdn_estab_ini
     and funcionario.cdn_estab       <= tt-param.cdn_estab_fim
     and funcionario.cod_rh_ccusto   >= tt-param.cod_rh_ccusto_ini
     and funcionario.cod_rh_ccusto   <= tt-param.cod_rh_ccusto_fim
     and funcionario.cod_unid_lotac  >= tt-param.cod_unid_lotac_ini
     and funcionario.cod_unid_lotac  <= tt-param.cod_unid_lotac_fim,
    first func_tip_mdo fields(cod_tip_mdo) of funcionario no-lock 
    where func_tip_mdo.dat_inic_lotac_func <= v_dat_fim_mes 
      and func_tip_mdo.dat_fim_lotac_func  >= v_dat_fim_mes,
    each movto_calcul_func fields(cdn_empresa cdn_estab cdn_funcionario qti_efp cdn_event_fp val_calcul_efp cod_rh_ccusto) 
                           of funcionario no-lock 
   where movto_calcul_func.num_mes_refer_fp         = tt-param.v_mes_refer 
     and movto_calcul_func.num_ano_refer_fp         = tt-param.v_ano_refer 
     and ((movto_calcul_func.idi_tip_fp               = 1 /*Normal*/ and
           movto_calcul_func.qti_parc_habilit_calc_fp = 9) 
          or
          movto_calcul_func.idi_tip_fp               = 3) /*13o salario*/:
  
    run pi-acompanhar in h-acomp (input 'Funcionario: ' + string(funcionario.cdn_estab) + '\' + string(funcionario.cdn_funcionario)).

    evento:
    do ind = 1 to movto_calcul_func.qti_efp: 

        find first cta_mdo_efp no-lock 
             where cta_mdo_efp.cdn_empresa  = movto_calcul_func.cdn_empresa       
               and cta_mdo_efp.cdn_estab    = movto_calcul_func.cdn_estab         
               and cta_mdo_efp.cdn_event_fp = movto_calcul_func.cdn_event_fp[ind] 
               and cta_mdo_efp.cod_tip_mdo  = func_tip_mdo.cod_tip_mdo no-error.
        if not avail cta_mdo_efp then do:
            find first cta_mdo_efp no-lock where
                       cta_mdo_efp.cdn_empresa  = movto_calcul_func.cdn_empresa       and
                       cta_mdo_efp.cdn_estab    = '*'                                 and
                       cta_mdo_efp.cdn_event_fp = movto_calcul_func.cdn_event_fp[ind] and
                       cta_mdo_efp.cod_tip_mdo  = func_tip_mdo.cod_tip_mdo no-error.
            if not avail cta_mdo_efp then
                next.
        end.

        assign v_cod_rh_ccusto = ''.

        FIND FIRST event_fp 
             WHERE event_fp.cdn_empresa  = v_cdn_empresa_evento 
               AND event_fp.cdn_event_fp = movto_calcul_func.cdn_event_fp[ind] NO-LOCK NO-ERROR.
                  
        if event_fp.cdn_event_fp  = '900' or
           event_fp.idi_ident_efp = 1     or /* vencimento */
           event_fp.idi_ident_efp = 2        /* descontos */ then do:

           if event_fp.idi_ident_efp = 2 /* descontos */ then
              assign v_cod_rh_cta_ctbl = cta_mdo_efp.cod_rh_cta_ctbl_cr
                     v_cod_rh_ccusto   = if cta_mdo_efp.cod_rh_ccusto_cr = 'xxxxxxxx' 
                                            then movto_calcul_func.cod_rh_ccusto  
                                            else cta_mdo_efp.cod_rh_ccusto_cr.
           else
              assign v_cod_rh_cta_ctbl = cta_mdo_efp.cod_rh_cta_ctbl_db
                     v_cod_rh_ccusto   = if cta_mdo_efp.cod_rh_ccusto_db = 'xxxxxxxx' 
                                         then movto_calcul_func.cod_rh_ccusto  
                                         else cta_mdo_efp.cod_rh_ccusto_db.

           assign v_chave = if event_fp.cdn_event_fp  = '900' or
                               event_fp.idi_ident_efp = 1  
                            then 40
                            else 50.
           
           run pi-cria-fp0820(input "1" /* folha */,
                              input v_cod_rh_cta_ctbl,
                              input v_chave).
        end.
        else do:
            /* provis∆o */
            if lookup(string(movto_calcul_func.cdn_event_fp[ind],"999"),i-ev-prov) > 0 then do:
               assign v_cod_rh_ccusto   = if cta_mdo_efp.cod_rh_ccusto_db = 'xxxxxxxx' 
                                          then movto_calcul_func.cod_rh_ccusto  
                                          else cta_mdo_efp.cod_rh_ccusto_db.
               run pi-cria-fp0820(input "2" /* provis∆o */,
                                  input cta_mdo_efp.cod_rh_cta_ctbl_db,
                                  input 40 ).

               assign v_cod_rh_ccusto   = if cta_mdo_efp.cod_rh_ccusto_cr = 'xxxxxxxx' 
                                          then movto_calcul_func.cod_rh_ccusto  
                                          else cta_mdo_efp.cod_rh_ccusto_cr.
               run pi-cria-fp0820(input "2" /* provis∆o */,
                                  input cta_mdo_efp.cod_rh_cta_ctbl_cr,
                                  input 50).
            end.
        end.

        /* encargos */
        if movto_calcul_func.cdn_event_fp[ind] = 'y01' or
           movto_calcul_func.cdn_event_fp[ind] = 'y02' or
           movto_calcul_func.cdn_event_fp[ind] = 'y08' or
           movto_calcul_func.cdn_event_fp[ind] = 'y09' then do:

           assign v_cod_rh_ccusto   = if cta_mdo_efp.cod_rh_ccusto_db = 'xxxxxxxx' 
                                      then movto_calcul_func.cod_rh_ccusto  
                                      else cta_mdo_efp.cod_rh_ccusto_db.
           run pi-cria-fp0820(input "3" /* encargos */,
                              input cta_mdo_efp.cod_rh_cta_ctbl_db,
                              input 40 ).

           assign v_cod_rh_ccusto   = if cta_mdo_efp.cod_rh_ccusto_cr = 'xxxxxxxx' 
                                      then movto_calcul_func.cod_rh_ccusto  
                                      else cta_mdo_efp.cod_rh_ccusto_cr.
           run pi-cria-fp0820(input "3" /* encargos */,
                              input cta_mdo_efp.cod_rh_cta_ctbl_cr,
                              input 50).
        end.
    end.
end.
    
if substr(tt-param.arquivo-gerado,length(tt-param.arquivo-gerado),1) <> "\" and 
   substr(tt-param.arquivo-gerado,length(tt-param.arquivo-gerado),1) <> "/" then
   assign tt-param.arquivo-gerado = tt-param.arquivo-gerado + "\".

for each tt-fp0820
   where tt-fp0820.valor > 0
     break by tt-fp0820.idi_tipo
           by tt-fp0820.cod_rh_cta_ctbl
           by tt-fp0820.cod_rh_ccusto:

   if first-of(tt-fp0820.idi_tipo) then do:

      assign c-arq-gerado = if tt-fp0820.idi_tipo = 1
                            then tt-param.arquivo-gerado + 'Folha' + string(tt-param.v_mes_refer,'99') + string(tt-param.v_ano_refer,'9999') + '.csv'
                            else if tt-fp0820.idi_tipo = 2
                                 then tt-param.arquivo-gerado + 'Provisao' + string(tt-param.v_mes_refer,'99') + string(tt-param.v_ano_refer,'9999') + '.csv'
                                 else tt-param.arquivo-gerado + 'Encargos' + string(tt-param.v_mes_refer,'99') + string(tt-param.v_ano_refer,'9999') + '.csv'.

      output stream s-rem to value(c-arq-gerado) no-convert.

      put stream s-rem unformatted
          'Data lanáamento;Data documento;Referància;Txt. cad. doc.;Tp. doc.;Per°odo;Empresa;Moeda;ChvLát;Cod Evento;Desc Evento;Conta;Montante;Centro de custo;Elemento PEP;Ordem interna;Texto' skip.
   end.

   put stream s-rem unformatted 
       replace(string(v_dat_fim_mes,'99/99/9999'),'/','')                             ';' /* Data lanáamento */
       replace(string(v_dat_fim_mes,'99/99/9999'),'/','')                             ';' /* Data documento  */
       tt-param.v_mes_refer '/' tt-param.v_ano_refer                                  ';' /* Referància      */
       'FOLHA PAGTO ' string(tt-param.v_mes_refer,'99') '/' tt-param.v_ano_refer      ';' /* Txt. cad. doc.  */
       'SA'                                                                           ';' /* Tp. doc         */
       tt-param.v_mes_refer                                                           ';' /* Per°odo         */  
       '1000'                                                                         ';' /* Empresa         */
       'BRL'                                                                          ';' /* Moeda           */
       tt-fp0820.chave                                                                ';' /* ChvLát          */
       tt-fp0820.cdn_event_fp                                                         ';' /* Cod Evento      */
       tt-fp0820.des_event_fp                                                         ';' /* Desc Evento     */
       tt-fp0820.cod_rh_cta_ctbl                                                      ';' /* Conta           */
       tt-fp0820.valor                                                                ';' /* Montante        */
       tt-fp0820.cod_rh_ccusto                                                        ';' /* centro de custo */
       ''                                                                             ';' /* Elemento PEP    */
       ''                                                                             ';' /* Ordem interna   */
       'SALµRIO FOPAG ' string(tt-param.v_mes_refer,'99') '/' tt-param.v_ano_refer        /* Texto           */ skip.

   if last-of(tt-fp0820.idi_tipo) then do:
      output stream s-rem close.
   end.
end.

output stream s-rem close.

MESSAGE "Processo conclu°do, arquivos gerados em: " tt-param.arquivo-gerado
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


run pi-finalizar in h-acomp.

return "OK":U.

procedure pi-cria-fp0820:
    def input param p-idi-tipo     as integer no-undo.
    def input param p-conta-ctbl like cta_mdo_efp.cod_rh_cta_ctbl_db no-undo.
    def input param p-chave        as integer no-undo.

    find first tt-fp0820 
         where tt-fp0820.cdn_empresa      = movto_calcul_func.cdn_empresa       
           and tt-fp0820.cdn_event_fp     = movto_calcul_func.cdn_event_fp[ind] 
           and tt-fp0820.cod_rh_ccusto    = v_cod_rh_ccusto
           and tt-fp0820.cod_rh_cta_ctbl  = p-conta-ctbl
           and tt-fp0820.idi_tipo         = p-idi-tipo no-error.
    if avail tt-fp0820 then do:
       assign tt-fp0820.valor = tt-fp0820.valor + movto_calcul_func.val_calcul_efp[ind].
    end.
    else do:
         create tt-fp0820.
         assign tt-fp0820.cdn_empresa        = movto_calcul_func.cdn_empresa
                tt-fp0820.cdn_estab          = movto_calcul_func.cdn_estab
                tt-fp0820.cdn_event_fp       = movto_calcul_func.cdn_event_fp[ind]
                tt-fp0820.des_event_fp       = event_fp.des_event_fp
                tt-fp0820.chave              = p-chave
                tt-fp0820.cod_rh_ccusto      = v_cod_rh_ccusto
                tt-fp0820.idi_tipo           = p-idi-tipo
                tt-fp0820.cod_rh_cta_ctbl    = p-conta-ctbl
                tt-fp0820.valor              = movto_calcul_func.val_calcul_efp[ind] .
    end.
    
end procedure.

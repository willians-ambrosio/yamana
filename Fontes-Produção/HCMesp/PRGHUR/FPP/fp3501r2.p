/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/buffers_RH.i}

{include/i-prgvrs.i FP3501R2 1.02.06.027}  /*** 010627 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i fp3501r2 MFP}
&ENDIF

/*******************************************************************************
**
**        Programa: FP3501-2.
**
**        Data....: Fevereiro/1994.
**
**        Autor...: DATASUL S.A.
**
**        Objetivo: Emissao de Envelope de Pagamento Individual
**                  Formulario    = 2 (Padrao Duplo)
**
*******************************************************************************/

{include/i-rpvar.i}
{utp/utapi019.i}

define     shared var c-forma   like funcionario.idi_forma_pagto label "Forma de Pagamento"      no-undo.
define     shared var i-formul  as int format "9"                label "Tipo do Formulario"      no-undo .
define     shared var i-empresa like empresa.ep-codigo                 no-undo.
define     shared var i-ord-aux as int                                 no-undo.
define     shared var l-origem  as log format "Coletiva/Individual"    no-undo.

define var i-index             as int                                  no-undo.
define var i-inx               as int                                  no-undo.
define var l-tem-movto         as log                                  no-undo.
define var i-cont-linha        as int                                  no-undo.
define var c-tab-identif       as cha initial "V,D,O"                  no-undo.
define var d-valor-vencto      as dec format ">>>>>,>>9.99+"           no-undo.
define var d-valor-descto      as dec format ">>>>>,>>9.99+"           no-undo.
define var d-valor-outros      as dec format ">>>>>,>>9.99+"           no-undo.
define var d-salar-atual       as dec                                  no-undo.
define var c-dep-sec           as cha format "x(07)"                   no-undo.
define var c-mes-folha         as cha format "x(03)"                   no-undo.
define var c-categoria         as cha format "x(07)"                   no-undo.
define var i-cod-banco         as int format "ZZZ"                     no-undo.
define var i-cod-agencia       as int format "ZZZZ"                    no-undo.
define var i-cta-corrente      as int format "ZZZZZZZZZ"               no-undo.
define var c-dig-conta         as cha format "x(02)"                   no-undo.
define var d-liquido-pagar     as dec format ">>>,>>>,>>9.99"          no-undo.
define var c-mensagem          as cha format "x(50)"   extent 2        no-undo.
define var c-mens-contr        as cha format "x(50)"   extent 2        no-undo.
define var c-hifen             as cha format "x(01)"                   no-undo.
define var c-parabens          as cha format "x(50)"                   no-undo.
define var l-prim-vez          as log                                  no-undo.
define var c-prenome           as cha format "x(31)"                   no-undo.
define var c-sobrenome         as cha format "x(31)"                   no-undo.
define var c-nome-compl        as cha format "x(31)"                   no-undo.
define var i-index1            as int                                  no-undo.
define var c-byte              as cha format "x(01)"                   no-undo.          
define var c-inc-liquido       as char format "x(01)"                  no-undo.
define var v_cdn_func        like funcionario.cdn_funcionario          no-undo.
define var i-impr-func       like funcionario.cdn_funcionario          no-undo.
define var v_cont_func         as int  initial 0                       no-undo.
define var v_log_folha_educnal as log  initial no                      no-undo.
define var c-lit-mes           as cha format "x(03)" extent 12         no-undo
                               initial ["Jan","Fev","Mar","Abr","Mai","Jun",
                                        "Jul","Ago","Set","Out","Nov","Dez"].
def    var v_cdn_cargo_basic like funcionario.cdn_cargo_basic          no-undo.
def    var v_cdn_niv_cargo   like funcionario.cdn_niv_cargo            no-undo.
def    var v_val_sal_mensal  like histor_sal_func.val_salario_mensal   no-undo.
def    var v_val_sal_hora    like histor_sal_func.val_salario_hora     no-undo.
def    var d-hrs-categ       like histor_sal_func.qtd_hrs_categ_sal    no-undo.
def    var d-sal-cat         like histor_sal_func.val_salario_categ    no-undo.
def    var d-sal-mes         like histor_sal_func.val_salario_mensal   no-undo.
def    var v_cdn_turno_trab  like funcionario.cdn_turno_trab           no-undo.
def    var v_cdn_turma_trab  like funcionario.cdn_turma_trab           no-undo.
def    var v_cdn_categ_sal   like funcionario.cdn_categ_sal            no-undo.
DEF    VAR v_status_enviado  AS CHAR FORMAT "x(11)"                    NO-UNDO.
DEF    VAR v_status_nao_enviado  AS CHAR FORMAT "x(11)"                NO-UNDO.
DEF    VAR v_num_sem         AS INT                                    NO-UNDO.
def    var v_dat_fim_mes     as date                                   no-undo.
DEF    var c-carga-horaria   as char format "x(21)"                    no-undo.

def var v_val_sal_cat            like histor_sal_func.val_salario_categ     no-undo.
def var v_cdn_tab_sal            like histor_sal_func.cdn_tab_sal           no-undo.
def var v_num_fx_sal             like histor_sal_func.num_faixa_sal         no-undo.
def var v_num_niv_sal            like histor_sal_func.num_niv_sal           no-undo.
def var v_cdn_cargo_basic_funcao like funcionario.cdn_cargo_basic           no-undo.
def var v_cdn_niv_cargo_funcao   like funcionario.cdn_niv_cargo             no-undo.
def var v_val_sal_mes_funcao     like histor_sal_func.val_salario_mensal    no-undo.
def var v_val_sal_hor_funcao     like histor_sal_func.val_salario_hora      no-undo.
def var v_val_sal_cat_funcao     like histor_sal_func.val_salario_categ     no-undo.
def var v_cdn_tab_sal_funcao     like histor_sal_func.cdn_tab_sal           no-undo.
def var v_num_fx_sal_funcao      like histor_sal_func.num_faixa_sal         no-undo.
def var v_num_niv_sal_funcao     like histor_sal_func.num_niv_sal           no-undo.
DEF VAR d-horas-semanais         LIKE apont_hora_aula.qtd_hrs_trab_semanal  NO-UNDO.
def var v_cdn_funcionario        as int                                     no-undo.

DEF VAR c_destino  AS CHAR FORMAT "x(60)" NO-UNDO.
DEF VAR c_anexo    AS CHAR FORMAT "x(60)" NO-UNDO.
DEF VAR c_assunto  AS CHAR FORMAT "x(21)" NO-UNDO.
DEF VAR c_senha    AS CHAR FORMAT "x(16)" NO-UNDO.
DEF VAR c_msg_erro AS CHAR FORMAT "x(60)" NO-UNDO.
DEF VAR l-ErroZip  AS LOGICAL             NO-UNDO.
DEF VAR l-compactar  AS LOGICAL INIT YES   NO-UNDO.
DEF VAR c_status_zip AS CHAR FORMAT "x(3)" NO-UNDO.

def var i-cont-mes as int no-undo.
def var i-cont-ano as int no-undo.

def        var dt-ini-per        as date no-undo.
def        var dt-fim-per        as date no-undo.
def var v_remetente       as char format "x(50)"                no-undo.
def buffer b2funcionario for funcionario.
DEFINE BUFFER bfunc_turno_trab FOR func_turno_trab.
DEFINE BUFFER bfunciona FOR funcionario.
def buffer bfuncionario for funcionario.
def buffer cfuncionario for funcionario.
def var v_log_sem_salario as logical no-undo.

define temp-table w-mvtocalc
       field fc-codigo   like funcionario.cdn_funcionario
       field ev-codigo   like event_fp.cdn_event_fp
       field descricao   like event_fp.des_event_fp     format "x(25)"
       field identif     as integer
       field unidades    as decimal
       field horas       as decimal                format ">>9.999"
       field salar-hora  as decimal                format ">>9.9999"
       field base        as decimal
       field valor       as decimal                format ">>>>,>>9.99-"
       field inc-liquido like event_fp.idi_tip_inciden_liq.

DEFINE SHARED TEMP-TABLE tt-rel-erros NO-UNDO
    FIELD cdn_empresa      LIKE funcionario.cdn_empresa
    FIELD cdn_estab        LIKE funcionario.cdn_estab
    FIELD cdn_funcionario  LIKE funcionario.cdn_funcionario
    FIELD nom_pessoa_fisic LIKE funcionario.nom_pessoa_fisic
    FIELD status_email     AS CHAR FORMAT "x(11)"
    FIELD email            AS CHAR FORMAT "x(60)".

DEFINE TEMP-TABLE tt-listFiles NO-UNDO
    FIELD cFile     AS CHAR    FORMAT "x(200)"
    FIELD lSearch   AS LOGICAL.

DEFINE TEMP-TABLE tt-erros-zip NO-UNDO
    FIELD cod-erro  AS INTEGER FORMAT ">>>>9"
    FIELD desc-erro AS CHAR    FORMAT "x(70)".


DEFINE VARIABLE h-zip AS HANDLE     NO-UNDO.

RUN utp/ut-zip.p PERSISTENT SET h-zip.
run utp/utapi019.p persistent set h-utapi019.


{prghur/fpp/fp3501tt.i shared}  /* Parametro */
{prghur/fpp/fp9400.i}

find first tt-param no-lock no-error.


/* chamada epc */
{include/i-epc200.i fp3501r2}

def shared var v_han_acomp as handle no-undo.  
def shared var l-imprime   as logical                          no-undo.

find empresa no-lock where empresa.ep-codigo = tt-param.v_cdn_empres_usuar no-error.
find param_empres_rh no-lock where param_empres_rh.cdn_empresa = tt-param.v_cdn_empres_usuar no-error.
find param_folha_educnal no-lock where
     param_folha_educnal.cdn_empresa = tt-param.v_cdn_empres_usuar no-error.
if avail param_folha_educnal then
   assign v_log_folha_educnal = yes.



for each tt-digita                          
    break by tt-digita.forma-pgto
          by tt-digita.v_cdn_estab
          by tt-digita.v_cdn_funcionario: 


    if v_log_folha_educnal then
       assign v_cdn_func        = {prghur/dop/eng005.i &VAR="(tt-digita.v_cdn_func_centr * 100)"}
              i-impr-func       = {prghur/dop/eng005.i &VAR="(tt-digita.v_cdn_func_centr)"}.
    else
       assign v_cdn_func  = tt-digita.v_cdn_funcionario
              i-impr-func = tt-digita.v_cdn_funcionario.  

    /*******************************************
     **** Percorrer todos os meses na faixa ****
     *******************************************/
    assign i-cont-ano = v_ano_ini
           i-cont-mes = v_mes_ini.
    
    do while i-cont-ano <=   v_ano_fim:
        if i-cont-mes > 12 then
            assign i-cont-mes = 1
                   i-cont-ano = i-cont-ano + 1.

        {prghur/fpp/fp9200.i15 i-cont-mes i-cont-ano v_dat_fim_mes}
        
        assign tt-param.v_dat_valid = v_dat_fim_mes
               tt-param.i-mes-ref   = i-cont-mes
               tt-param.i-ano-ref   = i-cont-ano.

        bloco-funciona:
        do:
            /* chamada epc protege */
            empty temp-table tt-epc no-error.
        
            create tt-epc.
            assign tt-epc.cod-event      = "imp_func_deslig"
                   tt-epc.val-parameter  = string(tt-param.v_cdn_empres_usuar) + ';' +
                                           string(tt-digita.v_cdn_estab) + ';' +
                                           string(v_cdn_func).
            
            {include/i-epc201.i "imp_func_deslig"}
        
            if return-value = "funcionario_nok" then
                leave bloco-funciona.
            /*-----*/
            find funcionario  no-lock where
                 funcionario.cdn_empresa        = tt-param.v_cdn_empres_usuar and
                 funcionario.cdn_estab          = tt-digita.v_cdn_estab       and
                 funcionario.cdn_funcionario    = v_cdn_func                  and
                 funcionario.dat_admis_func    <= tt-param.v_dat_valid        and
                (funcionario.dat_desligto_func  = ?                           or
                 funcionario.dat_desligto_func >= tt-param.v_dat_valid        or
                (tt-param.l-emite-demi          = yes))                       no-error.
            if avail funcionario then do:

               assign v_cont_func = v_cont_func + 1.
               run pi-acompanhar in v_han_acomp (input v_cont_func).        
               /******** Chamada EPC - Santa Casa **********/
               RUN pi-valida-retencao-ir.
               IF RETURN-VALUE = "NOK" THEN leave bloco-funciona.
               /******** Fim Chamada EPC - Santa Casa **********/
               find rh_estab of funcionario no-lock.
               if funcionario.dat_desligto_func <> ? and
                  (year(funcionario.dat_desligto_func) < tt-param.i-ano-ref or
                  (year(funcionario.dat_desligto_func) = tt-param.i-ano-ref and
                  month(funcionario.dat_desligto_func) <= tt-param.i-mes-ref)) and
                  tt-param.l-emite-demi = no then next.

            if not v_log_folha_educnal then do:
           /***** n∆o substituir o find abaixo pela chamada da procedure pi_histor_sal_func *******/
           find last histor_sal_func no-lock where
                     histor_sal_func.cdn_empresa      = funcionario.cdn_empresa and
                     histor_sal_func.cdn_estab        = funcionario.cdn_estab       and
                     histor_sal_func.cdn_funcionario  = funcionario.cdn_funcionario and
                     histor_sal_func.dat_liber_sal   <= tt-param.v_dat_valid no-error. 
            end. 
            else do:
               assign v_log_sem_salario = no.
                  find first bfuncionario where
                             bfuncionario.cdn_empresa          = funcionario.cdn_empresa       and
                             bfuncionario.cdn_estab            = funcionario.cdn_estab         and
                          bfuncionario.cdn_func_centrdor    = funcionario.cdn_func_centrdor and
                          bfuncionario.cdn_tip_contrat_func > 0 no-lock no-error.
               if avail bfuncionario then do:
                  if can-find(first cfuncionario where
                                    cfuncionario.cdn_empresa          =  bfuncionario.cdn_empresa       and
                                    cfuncionario.cdn_estab            =  bfuncionario.cdn_estab         and
                                    cfuncionario.cdn_func_centrdor    =  bfuncionario.cdn_func_centrdor and
                                    cfuncionario.cdn_tip_contrat_func >  0                              and
                                    cfuncionario.cdn_funcionario      <> bfuncionario.cdn_funcionario) then do:
                      /***** n∆o substituir o find abaixo pela chamada da procedure pi_histor_sal_func *******/
                      find last histor_sal_func no-lock where
                                histor_sal_func.cdn_empresa      = funcionario.cdn_empresa and
                                histor_sal_func.cdn_estab        = funcionario.cdn_estab       and
                                histor_sal_func.cdn_funcionario  = funcionario.cdn_funcionario and
                                histor_sal_func.dat_liber_sal   <= tt-param.v_dat_valid no-error. 
                      assign v_log_sem_salario = yes.                                            
                  end.
                  else do:
                      /***** n∆o substituir o find abaixo pela chamada da procedure pi_histor_sal_func *******/
                      find last histor_sal_func no-lock where
                                histor_sal_func.cdn_empresa      = bfuncionario.cdn_empresa and
                                histor_sal_func.cdn_estab        = bfuncionario.cdn_estab       and
                                histor_sal_func.cdn_funcionario  = bfuncionario.cdn_funcionario and
                                histor_sal_func.dat_liber_sal   <= tt-param.v_dat_valid no-error.                               
                  end.
               end.
            end.       
           /*if avail histor_sal_func then           */
            assign v_cdn_cargo_basic = histor_sal_func.cdn_cargo_basic
                   v_cdn_niv_cargo   = histor_sal_func.cdn_niv_cargo
                   v_val_sal_mensal  = histor_sal_func.val_salario_mensal 
                   v_val_sal_hora    = histor_sal_func.val_salario_hora.

               run pi_func_turno_trab (input funcionario.cdn_estab,
                                       input funcionario.cdn_funcionario,
                                       output v_cdn_turno_trab,
                                       output v_cdn_turma_trab).
        
               run pi_func_categ_sal (input funcionario.cdn_estab,
                                      input funcionario.cdn_funcionario,
                                      output v_cdn_categ_sal).
        
               assign l-tem-movto      = no
                      d-valor-vencto   = 0
                      d-valor-descto   = 0
                      d-valor-outros   = 0
                      d-liquido-pagar  = 0
                      i-cont-linha     = 0
                      d-salar-atual    = histor_sal_func.val_salario_categ.
               for each movto_calcul_func of funcionario no-lock where
                        movto_calcul_func.num_ano_refer_fp  = tt-param.i-ano-ref and
                        movto_calcul_func.num_mes_refer_fp  = tt-param.i-mes-ref and
                        movto_calcul_func.idi_tip_fp = tt-param.i-tipo-folha  and
                        movto_calcul_func.qti_parc_habilit_calc_fp = tt-param.i-parcela:
        
                   if movto_calcul_func.num_seq_movto_calcul_fp = 0 then do:
                       if not v_log_folha_educnal then 
                          find last histor_sal_func of funcionario where
                                     histor_sal_func.dat_liber_sal <= movto_calcul_func.dat_term_parc_calcula no-lock no-error.
                       else do:
                          if v_log_sem_salario then 
                             find last histor_sal_func of funcionario where
                                       histor_sal_func.dat_liber_sal <= movto_calcul_func.dat_term_parc_calcula no-lock no-error.
                          else 
                             find last histor_sal_func of bfuncionario where
                                       histor_sal_func.dat_liber_sal <= movto_calcul_func.dat_term_parc_calcula no-lock no-error.
                       end.
                       if available histor_sal_func then
                          assign d-salar-atual     = histor_sal_func.val_salario_categ.

                       if movto_calcul_func.num_ano_refer_fp <> param_empres_rh.num_ano_refer_calc_efetd or
                          movto_calcul_func.num_mes_refer_fp <> param_empres_rh.num_mes_refer_calc_efetd then do:
                          
                          find turno_trab no-lock where
                               turno_trab.cdn_turno_trab = v_cdn_turno_trab no-error.
                          if available turno_trab and not v_log_folha_educnal then
                             assign d-salar-atual = if v_cdn_categ_sal = 2
                                                  then movto_calcul_func.val_salario_hora
                                                  else d-salar-atual.
                       end.
                   end.
                       
                  assign l-tem-movto = yes.
                  do i-inx = 1 to movto_calcul_func.qti_efp:
                     find event_fp no-lock where
                          event_fp.cdn_event_fp = movto_calcul_func.cdn_event_fp[i-inx].
                     
                  if movto_calcul_func.cdn_event_fp[i-inx] > "900" and
                     not event_fp.log_impr_envel_fp or
                     movto_calcul_func.cdn_event_fp[i-inx] = "000" then
                     next.
                     if movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 52 then do:
                        assign d-liquido-pagar = movto_calcul_func.val_calcul_efp[i-inx].
                        next.
                     end.
                     if movto_calcul_func.val_calcul_efp[i-inx] = 0 and
                        event_fp.idi_ident_efp <> 3 then
                        next.
                     if not event_fp.log_impr_envel_fp then
                        next.

                     create w-mvtocalc.  
        
                     assign w-mvtocalc.fc-codigo  = movto_calcul_func.cdn_funcionario
                            w-mvtocalc.ev-codigo  = event_fp.cdn_event_fp
                            w-mvtocalc.descricao  = event_fp.des_event_fp
                            w-mvtocalc.identif    = event_fp.idi_ident_efp
                            w-mvtocalc.unidades   = movto_calcul_func.qtd_unid_event_fp[i-inx] 
                            w-mvtocalc.horas      = movto_calcul_func.qtd_hrs_demonst_efp[i-inx]
                            w-mvtocalc.base       = movto_calcul_func.val_base_calc_fp[i-inx]
                            w-mvtocalc.salar-hora = if movto_calcul_func.qtd_hrs_demonst_efp[i-inx] > 0
                                                    then truncate(movto_calcul_func.val_calcul_efp[i-inx]
                                                         / movto_calcul_func.qtd_hrs_demonst_efp[i-inx],4)
                                                    else 0
                            w-mvtocalc.salar-hora = if w-mvtocalc.salar-hora > 999.9999
                                                    then 999.9999
                                                    else w-mvtocalc.salar-hora
                            w-mvtocalc.valor       = movto_calcul_func.val_calcul_efp[i-inx]
                            w-mvtocalc.inc-liquido = event_fp.idi_tip_inciden_liq.
                     
                     /******** Chamada EPC - Protege **********/
                     for each tt-epc exclusive-lock where tt-epc.cod-event = "base_dias_horas":
                         delete tt-epc.
                     end.
                     create tt-epc.
                     assign tt-epc.cod-event     = "base_dias_horas"
                            tt-epc.val-parameter = string(rowid(funcionario)) + ";" +
                                                   string(w-mvtocalc.horas)
                            tt-epc.cod-parameter = ?.

                     {include/i-epc201.i "base_dias_horas"}
                     find first tt-epc where
                                tt-epc.cod-event = "base_dias_horas" and
                                tt-epc.cod-parameter <> ? no-lock no-error.
                     if avail tt-epc then
                         assign w-mvtocalc.horas = dec(tt-epc.cod-parameter).
                     /*****/
                     if v_log_folha_educnal then do:
                        if w-mvtocalc.identif = 1 then do:                          
                          if v_log_sem_salario then
                              assign d-salar-atual = 0.
                        end.
                     end.
                  end.
               end.
    
           if not l-tem-movto then
              leave.
    
               find habilit_calc_fp no-lock where
                    habilit_calc_fp.cdn_empresa    = funcionario.cdn_empresa and
                    habilit_calc_fp.cdn_estab      = funcionario.cdn_estab and
                    habilit_calc_fp.cdn_categ_sal  = v_cdn_categ_sal and
                    habilit_calc_fp.idi_tip_fp               = tt-param.i-tipo-folha  and
                    habilit_calc_fp.num_ano_refer_fp_calcula = tt-param.i-ano-ref and
                    habilit_calc_fp.num_mes_refer_fp_calcula = tt-param.i-mes-ref and
                    habilit_calc_fp.qti_parc_habilit_calc_fp = tt-param.i-parcela no-error.
               if available habilit_calc_fp then
                  assign c-mens-contr[1]=substring(habilit_calc_fp.des_msg_envel_fp,1,50)
                         c-mens-contr[2]=substring(habilit_calc_fp.des_msg_envel_fp,51,28).
        
               find cargo no-lock where
                    cargo.cdn_cargo_basic = v_cdn_cargo_basic and
                    cargo.cdn_niv_cargo   = v_cdn_niv_cargo no-error.       
        
               find rh_pessoa_fisic of funcionario no-lock no-error.
        
               assign i-index     = v_cdn_categ_sal
                      c-categoria = {database/inpy/i03py029.i 04 v_cdn_categ_sal}
                      c-mes-folha = c-lit-mes[tt-param.i-mes-ref]
                      c-parabens  = if tt-param.i-tipo-folha = 1 and
                                       (month(rh_pessoa_fisic.dat_nascimento) = (tt-param.i-mes-ref + 1) or
                                       (month(rh_pessoa_fisic.dat_nascimento) = 1 and
                                       tt-param.i-mes-ref = 12)) then
                                       "* * *  F e l i z   A n i v e r s a r i o  * * *"
                                    else "".
               if funcionario.dat_desligto_func <> ? and
                  (year(funcionario.dat_desligto_func) < tt-param.i-ano-ref or
                  (year(funcionario.dat_desligto_func) = tt-param.i-ano-ref and
                  month(funcionario.dat_desligto_func) <= tt-param.i-mes-ref)) then
                  assign c-parabens    = "*******************************"
                         c-mensagem[1] = "* * *   D e m i t i d o   * * *"
                         c-mensagem[2] = "*******************************".
               else
                  assign c-mensagem[1] = c-mens-contr[1]
                         c-mensagem[2] = c-mens-contr[2].
        
               if length(funcionario.nom_pessoa_fisic) > 31 then do:
                  assign l-prim-vez   = yes
                         c-prenome    = ""
                         c-sobrenome  = ""
                         c-nome-compl = "".
                  do i-index1 = 1 to 40 by 1:
                     assign c-byte = substring(funcionario.nom_pessoa_fisic,i-index1,1).
                     if c-byte = "" then
                        leave.
                     else
                        assign c-prenome = c-prenome + c-byte.
                  end.
                  do i-index1 = 40 to 1 by -1:
                     assign c-byte = substring(funcionario.nom_pessoa_fisic,i-index1,1).
                     if c-byte = "" then
                        if l-prim-vez = yes then
                           next.
                        else
                           leave.
                     else
                        assign c-sobrenome = c-byte + c-sobrenome
                               l-prim-vez  = no.
                  end.
                  assign c-nome-compl = c-prenome + " " + c-sobrenome.
               end.
               else
                  assign c-nome-compl = funcionario.nom_pessoa_fisic.
        
               if funcionario.idi_forma_pagto = 1 or
                  funcionario.idi_forma_pagto = 2 then
                  assign i-cod-banco    = funcionario.cdn_bco_liq
                         i-cod-agencia  = funcionario.cdn_agenc_bcia_liq
                         i-cta-corrente = funcionario.cdn_cta_corren
                         c-hifen        = "-"
                         c-dig-conta    = funcionario.cod_digito_cta_corren.
               else
                  assign i-cod-banco    = 0
                         i-cod-agencia  = 0
                         i-cta-corrente = 0
                         c-hifen        = ""
                         c-dig-conta    = "".
        
               for each w-mvtocalc break by w-mvtocalc.fc-codigo
                                         by w-mvtocalc.identif
                                         by w-mvtocalc.ev-codigo:
                   if first-of(w-mvtocalc.fc-codigo) or
                      i-cont-linha > 23 then do:
                      if i-cont-linha > 23 then do:
                         do while i-cont-linha < 28:
                            put "" at 01 skip.
                            i-cont-linha = i-cont-linha + 1.
                         end.
                         put "Continua..." at 54
                             "Continua..." at 122 skip(4).
                      end.
                      assign c-dep-sec = funcionario.cod_unid_lotac.
                      assign l-imprime = yes.
                         put empresa.razao-social               at  02 format "x(30)"     
                             funcionario.cdn_estab              at  32
                             c-dep-sec                          at  35
                             c-mes-folha                        at  45
                             "/"                                at  48
                             tt-param.i-ano-ref                 at  49
                             i-impr-func                        to  60 space(0)
                             "-"                                       space(0)
                             funcionario.num_digito_verfdor_func
                             empresa.razao-social               at  76 format "x(23)"
                             funcionario.cdn_estab              at  99
                             c-dep-sec                          at 102
                             c-mes-folha                        at 113
                             "/"                                at 116
                             tt-param.i-ano-ref                 at 117
                             i-impr-func                        to 130 space(0)
                             "-"                                       space(0)
                             funcionario.num_digito_verfdor_func       skip(1)
                             rh_pessoa_fisic.nom_ender_rh       at  02 format "x(33)"
                             c-nome-compl                       at  36 format "x(31)"
                             c-nome-compl                       at 100 format "x(31)" skip(1)
                             cargo.des_envel_pagto              at  02
                             d-salar-atual                      at  21 format ">>>>>,>>9.99"
                             c-categoria                        at  34
                             funcionario.qti_depend_irf         at  42 format "zz"
                             funcionario.qti_depend_salfam      at  45 format "zz"
                             i-cod-banco                        at  48
                             i-cod-agencia                      at  51
                             i-cta-corrente                     at  56
                             c-hifen                            at  65
                             c-dig-conta                        at  66
                             cargo.des_envel_pagto              at  76
                             d-salar-atual                      at  87 format ">>>>>,>>9.99"
                             c-categoria                        at 100
                             funcionario.qti_depend_irf         at 108 format "zz"
                             funcionario.qti_depend_salfam      at 111 format "zz"
                             i-cod-banco                        at 114 format "zzz"
                             i-cod-agencia                      at 117 format "zzzz"
                             i-cta-corrente                     at 121 format "zzzzzzzzz"
                             c-hifen                            at 130
                             funcionario.cod_digito_cta_corren  at 131                skip(2).
                      assign i-cont-linha = 7.
                   end.              
                    if w-mvtocalc.inc-liquido = 1 then
                           assign c-inc-liquido = "+".
                        if w-mvtocalc.inc-liquido = 2 then
                           assign c-inc-liquido = "-".
                        if w-mvtocalc.inc-liquido = 3 then
                           assign c-inc-liquido = " ".
                        if w-mvtocalc.inc-liquido = 4 then
                           assign c-inc-liquido = "P".
                        if w-mvtocalc.inc-liquido = 5 then
                           assign c-inc-liquido = "N".
                   if w-mvtocalc.horas > 0 then  do:
                      assign l-imprime = yes.
                      put w-mvtocalc.descricao   at  02
                          w-mvtocalc.ev-codigo   at  28
                          w-mvtocalc.horas       at  31
                          w-mvtocalc.salar-hora  at  38
                          w-mvtocalc.valor       at  46
                          c-inc-liquido at  58
                          w-mvtocalc.descricao   at  76
                          w-mvtocalc.ev-codigo   at 103
                          w-mvtocalc.horas       at 106
                          w-mvtocalc.salar-hora  at 113
                          w-mvtocalc.valor       at 121 format ">>>,>>9.99-"
                          c-inc-liquido at 132 skip.
                   end.
                   else do:
                      assign l-imprime = yes.
                      put w-mvtocalc.descricao   at  02
                          w-mvtocalc.ev-codigo   at  28
                          w-mvtocalc.valor       at  46
                          c-inc-liquido at  58
                          w-mvtocalc.descricao   at  76
                          w-mvtocalc.ev-codigo   at 103
                          w-mvtocalc.valor       at 121 format ">>>,>>9.99-"
                          c-inc-liquido at 132 skip.
                   end.
                   assign i-cont-linha = i-cont-linha + 1.
                   if w-mvtocalc.identif = 1 then
                      assign d-valor-vencto =
                                  if w-mvtocalc.inc-liquido = 1
                                  then d-valor-vencto + w-mvtocalc.valor
                                  else if w-mvtocalc.inc-liquido = 2
                                       then d-valor-vencto - w-mvtocalc.valor
                                       else d-valor-vencto.
                   else
                      if w-mvtocalc.identif = 2 then
                         assign d-valor-descto =
                                     if w-mvtocalc.inc-liquido = 1
                                     then d-valor-descto + w-mvtocalc.valor
                                     else if w-mvtocalc.inc-liquido = 2
                                          then d-valor-descto - w-mvtocalc.valor
                                          else d-valor-descto.
                      else
                         assign d-valor-outros =
                                     if w-mvtocalc.inc-liquido = 1
                                     then d-valor-outros + w-mvtocalc.valor
                                     else if w-mvtocalc.inc-liquido = 2
                                          then d-valor-outros - w-mvtocalc.valor
                                          else d-valor-outros.
                   if last-of(w-mvtocalc.identif) then do:
                      if w-mvtocalc.identif = 1 then
                         put "Total Vencimentos:" at  09
                             d-valor-vencto       at  46
                             "Total Vencimentos:" at  83
                             d-valor-vencto       at 120 skip(1).
                      else
                         if w-mvtocalc.identif = 2 then
                            put "Total Descontos:" at  11
                                d-valor-descto     at  46
                                "Total Descontos:" at  85
                                d-valor-descto     at 120 skip(1).
                         else
                            put "Total Outros:" at  14
                                d-valor-outros  at  46
                                "Total Outros:" at  88
                                d-valor-outros  at 120 skip(1).
                       assign i-cont-linha = i-cont-linha + 2.
                   end.
                   if last-of(w-mvtocalc.fc-codigo) then do:
                          do  while i-cont-linha < 26:
                             put "" at 01 skip.
                             assign i-cont-linha = i-cont-linha + 1.
                          end.
                      put c-parabens      at 02 skip
                          c-mensagem[1]   at 02 skip
                          c-mensagem[2]   at 02
                          d-liquido-pagar at 53
                          d-liquido-pagar at 119 skip(4).
                   end.
                   delete w-mvtocalc.
                end.
             end.
        end.
         /****************************************
          ****Percorrer màs a màs na faixa *******
          ****************************************/
         assign i-cont-mes = i-cont-mes + 1.
        
         if i-cont-mes > v_mes_fim and
            i-cont-ano = v_ano_fim  OR
            i-cont-ano > v_ano_fim then
             leave.
    end.
    delete tt-digita.
end.


delete procedure h-utapi019.
DELETE PROCEDURE h-zip.

return.

procedure pi-valida-retencao-ir :
    /******** Chamada EPC - Santa Casa **********/
    for each tt-epc exclusive-lock where tt-epc.cod-event = "valida_retencao_ir":
        delete tt-epc.
    end.
    create tt-epc.
    assign tt-epc.cod-event     = "valida_retencao_ir"
           tt-epc.val-parameter = string(funcionario.cdn_empresa)     + "|" +
                                  string(funcionario.cdn_estab)       + "|" +
                                  string(funcionario.cdn_funcionario) + "|" +
                                  string(tt-param.i-ano-ref)          + "|" +
                                  string(tt-param.i-mes-ref)          + "|" +
                                  string(tt-param.i-tipo-folha)       + "|" +
                                  string(tt-param.i-parcela)
           tt-epc.cod-parameter = "".
    {include/i-epc201.i "valida_retencao_ir"}
    if return-value = "OK-STA-CASA-FP3501R1-NEXT" then return "NOK".
    /******** Fim Chamada EPC - Santa Casa **********/
    
    return "OK".

end procedure.

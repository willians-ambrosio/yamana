/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i esFP1661RP 1.02.03.079 } /*** 010379 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i esfp1661rp MFP}
&ENDIF

{include/i_fnctrad.i}
/******************************************************************************
**
**       Programa: prghur/esp/esfp1661.P
**
**       Data....: Marco/1991.
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: Listagem dos Historicos de Salarios e Funcoes
*******************************************************************************/

{include/i-rpvar.i}
{include/i_cdrel_hr.i}
&GLOBAL-DEFINE RTF YES
{prghur/esp/esfp1661tt.i shared}
{prghur/fpp/fp9200.i10 new shared}
&if "{&cd_rel_hr}" >= "2.11" &then 
	{prghur/fpp/fp9200.i8 &EspacoExtra=14}
&else 
	{prghur/fpp/fp9200.i8}
&endif

def temp-table tt-raw-digita field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* chamada epc */             
{include/i-epc200.i esfp1661rp} 

/******** Chamada EPC - SENAC **********/          
for each tt-epc where tt-epc.cod-event = "senac-epc1661":
    delete tt-epc. 
end.
create tt-epc.
assign tt-epc.cod-event     = 'senac-epc1661':U.

{include/i-epc201.i "senac-epc1661"}
IF RETURN-VALUE = "senac-epc1661-ok" THEN do:
   RETURN "NOK".
END.
/* Fim Chamada EPC SENAC */


/* inicio sergio */
   DEFINE VARIABLE c-narrativa      AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE c-arquivo        AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE ch-excel         AS    COMPONENT-HANDLE      NO-UNDO.
   DEFINE VARIABLE i-cont-linha     AS    INTEGER               NO-UNDO.
/* fi sergio */


DEFINE BUFFER bf_histor_sal_func FOR histor_sal_func.
DEFINE BUFFER bf_cargo FOR cargo.

/* ++++++++++++++++++ (Definicao Variaveis) ++++++++++++++++++ */

define new shared var i-ordem2              as int                       no-undo.
define new shared var v_log_folha_educnal   as log                       no-undo.
define new shared var v_cod_matr            as char format "x(11)"       no-undo.
define new shared var v_des_motiv_liber_sal as char format "x(19)"       no-undo.

define var c-ordem-v1         as character format "x(16)"              no-undo.
define var c-codigo-v1        as character format "x(08)"              no-undo.
define var c-descri-v1        as character format "x(30)"              no-undo.
define var c-hifem-v1         as character format "x(01)"              no-undo.
define var c-ordem-v2         as character format "x(16)"              no-undo.
define var c-codigo-v2        as character format "x(03)"              no-undo.
define var c-descri-v2        as character format "x(23)"              no-undo.
define var c-hifem-v2         as character format "x(01)"              no-undo.
define var l-lista            as logical                               no-undo. 
define var c-hifem            as character format "x(01)" initial "-"  no-undo.
define var d-salar-anter      as decimal   format "zzzzz,zz9.9999"   no-undo.
define var d-salar-prim       as decimal   format "zzzzz,zz9.9999"   no-undo.
define var d-perc-aumento     as decimal   /*format "zzz9.99" */           no-undo.
define var d-dt-corte         as date      initial "08011993"          no-undo.
define var l-real             as logical   initial no                  no-undo.
define var d-dt-urv           as date      initial "03011994"          no-undo.
define var l-urv              as logical   initial no                  no-undo.
define var d-val-urv          as dec       initial "647.50"            no-undo.
define var r-rec-hist         as rowid                                 no-undo.
DEFINE VAR V_LABEL1           AS CHAR      FORMAT "X(4)"               NO-UNDO.
DEFINE VAR V_LABEL2           AS CHAR      FORMAT "X(50)"              NO-UNDO.
DEFINE VAR V_LABEL3           AS CHAR      FORMAT "X(11)"              NO-UNDO.
DEFINE VAR V_TRACO1           AS CHAR      FORMAT "X(4)"               NO-UNDO.
DEFINE VAR V_TRACO2           AS CHAR      FORMAT "X(50)"              NO-UNDO.
DEFINE VAR V_TRACO3           AS CHAR      FORMAT "X(11)"              NO-UNDO.

def var v_cdn_cargo           like histor_sal_func.cdn_cargo_basic     no-undo.
def var v_cdn_func            like histor_sal_func.cdn_funcionario     no-undo.
def var v_val_sal_hist        like histor_sal_func.val_salario_mensal  no-undo.
def var v_tip_class_motivo    as char format "x(20)"                   no-undo. 
def var v_idi_imp_ultim_mov   as char format "x(20)"                   no-undo.
def var v_idi_tipo            as char format "x(20)"                   no-undo.


def var v_cdn_cargo_basic     as int  format ">>,>>9"                  no-undo. 
def var v_cdn_niv_cargo       as int  format ">>9"                     no-undo. 
def var v_des_cargo           as char format "x(27)"                   no-undo. 
def var v_log_imprime         as log  init yes                         no-undo.   
def var v_log_imprime_2       as log                                   no-undo.   
def var v_cargo_niv_des       as char format "x(30)"                   no-undo.

def var v_des_tip_cargo_func    as char format "x(6)"                  no-undo. 
def var v_des_sit_histor_funcao as char format "x(4)"                  no-undo.


DEF VAR i_tipo_ini             AS INT FORMAT 9                          NO-UNDO.
DEF VAR i_tipo_fim             AS INT FORMAT 9                          NO-UNDO.


/*  Cristina */
DEF VAR de_trunc_val_salario_categ    AS DECIMAL  NO-UNDO.
DEF VAR de_trunc_val_salario_mensal   AS DECIMAL  NO-UNDO.
/* ******** */

/**** d-val-urv =  URV do dia 01/03/1994 ****/

def new shared buffer bhistsal for histor_sal_func.

&global-define only_clt_rp yes
{prghur/fpp/fp9200.i11}

/* ++++++++++++++++++ (Definicao Form) ++++++++++++++++++ */
form header
    c-ordem-v1                                        at 01
    c-codigo-v1                                       at 18
    c-hifem-v1                                        AT 29
    c-descri-v1                                       AT 30
    c-ordem-v2                                        at 60
    c-codigo-v2                                       at 76
    c-hifem-v2                                        at 82
    c-descri-v2                                       at 83
    "Cargo/Funá∆o:"                                   at 110
    tt-param.des_tip_cargo_func         format "x(6)" AT 124 SKIP(1)
    "-------------Data-------------"                  AT 01
    "Cargo/"                                          AT 54
    "----------- Sal†rio ----------- Perc"            at 93
    "Alteraá∆o"                                       at 01
    "Vigància"                                        at 12
    "Validade"                                        at 23  
    "Mot"                                             at 34
    "Descriá∆o"                                       at 38
    "Funá∆o"                                          AT 54
    "N°v"                                             AT 61
    "Descriá∆o"                                       AT 65
    "Categoria"                                       at 93
    "Padr∆o Màs"                                      at 109
    "Aumento"                                         at 125
    "---------- ---------- ---------- --- --------------- ------ --- --------------------------- --------------- --------------- --------"
    with no-labels no-attr-space no-box page-top stream-io width 132 1 down frame f-cab.

form
    "Matricula:"                            AT 01
    v_cod_matr                              at 11
    funcionario.nom_pessoa_fisic            AT 23 SKIP
    with no-labels no-attr-space no-box stream-io width 132 57 down frame f-matric.

form
    histor_sal_func.dat_liber_sal           at 01                 
    histor_sal_func.dat_livre_1             AT 12
    histor_sal_func.dat_term_valid_funcao   AT 23
    histor_sal_func.cdn_motiv_liber_sal     AT 34
    v_des_motiv_liber_sal   format "x(10)"  AT 38
    cargo.cdn_cargo_basic   format "zzzz9"  AT 55
    cargo.cdn_niv_cargo     format "zz9"    AT 61
    cargo.des_cargo         format "x(30)"  AT 65
    de_trunc_val_salario_categ              AT 95
    de_trunc_val_salario_mensal              
    d-perc-aumento                           
    with no-labels no-attr-space no-box stream-io width 132 57 down frame f-dados.

form
    histor_sal_func.dat_liber_sal        at  13                 space(1)
    histor_sal_func.cdn_motiv_liber_sal                         space(1)
    v_des_motiv_liber_sal                format "x(10)"         space(1)
    v_cargo_niv_des                      format "x(24)"         space(1)
    v_des_tip_cargo_func                 format "x(6)"
    v_des_sit_histor_funcao              format "x(4)"
    histor_sal_func.dat_term_valid_funcao
    de_trunc_val_salario_categ           TO 99
    de_trunc_val_salario_mensal          TO 113
    d-perc-aumento                       TO 121
    histor_sal_func.dat_livre_1          TO 132
    with no-labels no-attr-space no-box stream-io width 132 57 down frame f-dados-epc.

FORM  
    tt-motivo-func.cdn_motivo  AT 35 
    tt-motivo-func.qtde-func   AT 37                               
    WITH SIDE-LABELS NO-ATTR-SPACE NO-BOX STREAM-IO WIDTH 132 57 DOWN FRAME f-totais. 

FORM
    skip
    HISTOR_DET_REMUN_VAR_FUNC.CDN_REMUN_VAR  at 30
    REMUN_VAR.DES_REMUN_VAR                  at 39
    HISTOR_DET_REMUN_VAR_FUNC.VAL_REMUN_VAR  at 84 SKIP (1)
    WITH NO-LABELS NO-ATTR-SPACE NO-BOX STREAM-IO WIDTH 132 57 DOWN FRAME F-EXPLODE.

FORM   
    V_LABEL1  AT 30
    V_LABEL2  AT 39
    V_LABEL3  AT 95
    WITH NO-LABELS NO-ATTR-SPACE NO-BOX STREAM-IO WIDTH 132 57 DOWN FRAME F-LABELS.

FORM
    V_TRACO1  AT 30
    V_TRACO2  AT 39
    V_TRACO3  AT 89
    WITH NO-LABELS NO-ATTR-SPACE NO-BOX STREAM-IO WIDTH 132 57 DOWN FRAME F-TRACO.

assign c-versao       = "D.00"
       c-revisao      = "001"
       c-programa     = "FP/1661"
       c-sistema      = "FOLHA DE PAGAMENTO"
       c-titulo-relat = "Listagem dos Hist¢ricos de Sal†rios e Funá‰es".


ASSIGN V_TRACO1 = "----"      
       V_TRACO2 = "----------------------------------------------"
       V_TRACO3 = "-------------"
       V_LABEL1 = "Item"
       V_LABEL2 = "Descriá∆o"
       V_LABEL3 = "Valor".

FORM 
    SKIP (01)
    WITH NO-LABELS NO-ATTR-SPACE NO-BOX STREAM-IO WIDTH 132 57 DOWN FRAME F-PULA .

{utp/ut-liter.i Motivo mbs L}
ASSIGN tt-motivo-func.cdn_motivo:LABEL IN FRAME f-totais = trim(return-value).

{utp/ut-liter.i Total_Funcion†rios mbs L}
ASSIGN tt-motivo-func.qtde-func:LABEL IN FRAME f-totais = trim(RETURN-VALUE).

find param_empres_rh where param_empres_rh.cdn_empresa = tt-param.v_cdn_empres_usuar no-error.       
find MGCAD.empresa  where  empresa.ep-codigo = tt-param.v_cdn_empres_usuar no-error.
assign c-empresa = string(empresa.ep-codigo) + " - " + empresa.razao-social.

find param_folha_educnal no-lock where
     param_folha_educnal.cdn_empresa = v_cdn_empres_usuar no-error.
assign v_log_folha_educnal = if avail param_folha_educnal then yes else no.


   def new shared var v_han_acomp as handle no-undo.
   run utp/ut-acomp.p persistent set v_han_acomp.

   {utp/ut-liter.i Imprimindo._Aguarde *}
   run pi-inicializar in v_han_acomp (input return-value). 

run utp/ut-trfrrp.p (input frame F-PULA:handle).
run utp/ut-trfrrp.p (input frame F-TRACO:handle).
run utp/ut-trfrrp.p (input frame F-LABELS:handle).
run utp/ut-trfrrp.p (input frame F-EXPLODE:handle).
run utp/ut-trfrrp.p (input frame f-totais:handle).
run utp/ut-trfrrp.p (input frame f-dados:handle).
run utp/ut-trfrrp.p (input frame f-dados-epc:handle).
run utp/ut-trfrrp.p (input frame f-matric:handle).
run utp/ut-trfrrp.p (input frame f-cab:handle).
{include/i-rpcab.i}
{include/i-rpout.i}

if  not tt-param.l-habilitaRtf then
    view frame f-cabec.


if  not tt-param.l-habilitaRtf then
    view frame f-rodape.

assign i-ordem2 = classifica.

if i-ordem2 < 05 then
   view frame f-cab.

if v_log_folha_educnal then
      assign tt-param.i-fc-ini = {prghur/dop/eng005.i &VAR="((tt-param.cdn_func_centrdor_ini * 100) + tt-param.cdn_tip_contrat_ini)"} 
             tt-param.i-fc-fim = {prghur/dop/eng005.i &VAR="((tt-param.cdn_func_centrdor_fim * 100) + tt-param.cdn_tip_contrat_fim)"}.



if classifica = 01
or classifica = 02 then do:
/*"Por Centro de Custo/Estabelecimento/Matr°cula", 1,
 * "Por Centro de Custo/Estabelecimento/Nome", 2,*/
    RUN pi-abre-excel.

    for each func_ccusto no-lock where
             func_ccusto.cdn_empresa             = tt-param.v_cdn_empres_usuar         and
             func_ccusto.cdn_estab              >= tt-param.i-es-ini                   and
             func_ccusto.cdn_estab              <= tt-param.i-es-fim                   and
             func_ccusto.cdn_funcionario        >= tt-param.i-fc-ini                   and
             func_ccusto.cdn_funcionario        <= tt-param.i-fc-fim                   and
             func_ccusto.dat_inic_lotac_func    <= tt-param.v_dat_valid                and
             func_ccusto.dat_fim_lotac_func     >= tt-param.v_dat_valid                and
             func_ccusto.cod_rh_ccusto          >= c-cc-ini                            and
             func_ccusto.cod_rh_ccusto          <= c-cc-fim,
        each funcionario of func_ccusto no-lock where
             funcionario.nom_pessoa_fisic  >= c-nm-ini                                 and
             funcionario.nom_pessoa_fisic  <= c-nm-fim                                 and
             /*funcionario.cdn_cargo_basic   >= tt-param.cdn_cargo_ini                   and
             funcionario.cdn_cargo_basic   <= tt-param.cdn_cargo_fim                   and*/
            (funcionario.dat_desligto_func  = ?                                        or
            (funcionario.dat_desligto_func <> ?                                        and
             tt-param.l-imp-desl            = yes))                                    and
           ((v_log_folha_educnal and funcionario.cdn_tip_contrat_func         > 0)     or
            (not v_log_folha_educnal))
        break by func_ccusto.cod_rh_ccusto
              by funcionario.cdn_estab
              by if classifica = 1 then
                    string(funcionario.cdn_funcionario,"99999999")
                 else
                    funcionario.nom_pessoa_fisic:
                    
           find rh_ccusto of func_ccusto no-lock.
           assign c-ordem-v1   = "Centro de Custo:"
                  c-codigo-v1  = rh_ccusto.cod_rh_ccusto
                  c-hifem-v1   = "-"
                  c-descri-v1  = rh_ccusto.des_rh_ccusto.

           if not v_log_folha_educnal then 
              run pi-acompanhar in v_han_acomp (input string(funcionario.cdn_funcionario)).            
           else 
              run pi-acompanhar in v_han_acomp (input string(funcionario.cdn_func_centrdor)).            

           assign l-real       = no
                  l-urv        = no
                  d-salar-prim = 0.

           if  first-of(func_ccusto.cod_rh_ccusto)
               or first-of(funcionario.cdn_estab)
               or line-counter > 63 then do:

               find rh_estab of funcionario NO-LOCK where
                    rh_estab.cdn_estab = funcionario.cdn_estab no-error.                 

               if first-of(funcionario.cdn_estab) then
                   assign c-ordem-v2   = "Estabelecimento:"
                          c-codigo-v2  = string(rh_estab.cdn_estab,{prghur/dop/eng006.i})
                          c-hifem-v2   = "-"
                          c-descri-v2  = rh_estab.nom_pessoa_jurid.
               page.
           end.

           if classifica = 1 then do:
               if v_log_folha_educnal then do:
                   if funcionario.cdn_func_centrdor < tt-param.cdn_func_centrdor_ini or
                      funcionario.cdn_func_centrdor > tt-param.cdn_func_centrdor_fim then 
                       next.

                   if funcionario.cdn_tip_contrat_func < tt-param.cdn_tip_contrat_ini or
                      funcionario.cdn_tip_contrat_func > tt-param.cdn_tip_contrat_fim then
                       next. 
               end.
           end.

           assign l-real = no
                  l-urv  = no.

           if  not tt-param.v_log_imp_ult_movto then do:
               
               
               ASSIGN i_tipo_ini = tt-param.idi_tip_cargo_funcao 
                      i_tipo_fim = tt-param.idi_tip_cargo_funcao.
               IF tt-param.idi_tip_cargo_funcao  = 3  THEN 
                   ASSIGN i_tipo_ini = 1.
               


               
               assign l-lista = no.

               for each histor_sal_func of funcionario no-lock where
                        histor_sal_func.dat_liber_sal >= tt-param.d-dt-ini and
                        histor_sal_func.dat_liber_sal <= tt-param.d-dt-fim and
                        histor_sal_func.cdn_cargo_basic >= tt-param.cdn_cargo_ini and 
                        histor_sal_func.cdn_cargo_basic <= tt-param.cdn_cargo_fim AND
                        histor_sal_func.idi_tip_cargo_funcao >= i_tipo_ini AND
                        histor_sal_func.idi_tip_cargo_funcao <= i_tipo_fim
                        break by histor_sal_func.cdn_funcionario
                              by histor_sal_func.dat_liber_sal
                              by histor_sal_func.num_seq_histor_sal
                              by histor_sal_func.cdn_motiv_liber_sal:

                   if histor_sal_func.idi_sit_histor_funcao <> 0 then
                      assign v_des_sit_histor_funcao = {database/inpy/i03py102.i 04 histor_sal_func.idi_sit_histor_funcao}.
                   else
                      assign v_des_sit_histor_funcao = {database/inpy/i03py102.i 04 1}.

                   /*if histor_sal_func.idi_tip_cargo_funcao <> 0 then
                      assign v_des_tip_cargo_func = {database/inpy/i02py102.i 04 histor_sal_func.idi_tip_cargo_funcao}.
                   else
                      assign v_des_tip_cargo_func = {database/inpy/i02py102.i 04 1}.*/


                   if first-of(histor_sal_func.cdn_funcionario) then do:
                       assign d-perc-aumento = 0
                              d-salar-anter  = 0
                              r-rec-hist = rowid(histor_sal).
                       find bhistsal where rowid(bhistsal) = r-rec-hist NO-LOCK NO-ERROR.
                       find prev bhistsal of funcionario no-lock no-error.

                       assign d-salar-prim = if available bhistsal
                                             then bhistsal.val_salario_mensal
                                             else 0.
                
                       if histor_sal_func.dat_liber_sal = d-dt-corte    and
                          histor_sal_func.val_salario_mensal  < d-salar-anter and
                          l-real             = no then
                          assign l-real        = yes
                                 d-salar-anter = if d-salar-prim <> 0
                                                    then trunc(d-salar-prim / 1000,3)
                                                 else trunc(histor_sal_func.val_salario_mensal / 1000,3)
                                 d-salar-prim  = 0.
                       else
                           if histor_sal_func.dat_liber_sal   = d-dt-urv            and
                              histor_sal_func.val_salario_mensal    < d-salar-anter and
                              l-urv                = no then
                               assign l-urv         = yes
                                      d-salar-anter = if  d-salar-prim <> 0
                                                          then trunc(d-salar-prim / d-val-urv,3)
                                                      else trunc(histor_sal_func.val_salario_mensal / d-val-urv,3)
                                      d-salar-prim  = 0.
                           else
                               assign d-salar-anter = if  d-salar-prim <> 0 then 
                                                          d-salar-prim
                                                      else histor_sal_func.val_salario_mensal.

                       assign d-perc-aumento = truncate(round((((histor_sal_func.val_salario_mensal / d-salar-anter) - 1) * 100),4),4)
                              d-salar-anter = histor_sal_func.val_salario_mensal.

                       if  not v_log_folha_educnal then
                           assign v_cod_matr = string(funcionario.cdn_funcionario,"zzzzzzz9") + "-" + string(funcionario.num_digito_verfdor_func,"9"). 
                       else 
                           assign v_cod_matr = string(funcionario.cdn_func_centrdor,"zzzzz9") + "/" + string(funcionario.cdn_tip_contrat_func,"99") + "-" + string(funcionario.num_digito_verfdor_func,"9").

                   end.
                   else do:
                       if  histor_sal_func.dat_liber_sal      = d-dt-corte    and
                           histor_sal_func.val_salario_mensal < d-salar-anter and
                           l-real               = no then
                           assign l-real        = yes
                                  d-salar-anter = truncate(d-salar-anter / 1000,3).

                       if  histor_sal_func.dat_liber_sal      = d-dt-urv      and
                           histor_sal_func.val_salario_mensal < d-salar-anter and
                           l-urv                = no then
                           assign l-urv         = yes
                                  d-salar-anter = truncate(d-salar-anter / d-val-urv,3).

                       assign d-perc-aumento = truncate(round(
                                               (((histor_sal_func.val_salario_mensal /
                                                  d-salar-anter) - 1) * 100),4),4)
                              d-salar-anter = histor_sal_func.val_salario_mensal.
                   end.

                   run pi-imprime-historicos.

               end. /* for each histor_sal_func */

               if  l-lista then
                   put "".
           end. /* not tt-param.v_log_imp_ult_movto */

           else
               run pi-imprime-ult-movto.
             
           if  last-of(funcionario.cdn_estab) then do:
               for each tt-motivo-func no-lock:
                   view frame  f-cab-tot.
                   disp tt-motivo-func.cdn_motivo
                        tt-motivo-func.qtde-func with frame f-totais.
                                                 down with frame f-totais.
                   delete tt-motivo-func.
               end.
           end.
    end.

    RUN pi-encerra-excel.
end.

if classifica = 03
or classifica = 04 then do:
 /* "Por Estabelecimento/Matr°cula", 3, *
  * "Por Estabelecimento/Nome", 4,      */
    RUN pi-abre-excel.

    for each func_ccusto no-lock where
             func_ccusto.cdn_empresa             = tt-param.v_cdn_empres_usuar         and
             func_ccusto.cdn_estab              >= tt-param.i-es-ini                   and
             func_ccusto.cdn_estab              <= tt-param.i-es-fim                   and
             func_ccusto.cdn_funcionario        >= tt-param.i-fc-ini                   and
             func_ccusto.cdn_funcionario        <= tt-param.i-fc-fim                   and
             func_ccusto.dat_inic_lotac_func    <= tt-param.v_dat_valid                and
             func_ccusto.dat_fim_lotac_func     >= tt-param.v_dat_valid                AND
             func_ccusto.cod_rh_ccusto          >= c-cc-ini                            and
             func_ccusto.cod_rh_ccusto          <= c-cc-fim,
        each funcionario of func_ccusto no-lock where
            /*funcionario.cdn_cargo_basic      >= tt-param.cdn_cargo_ini              and
            funcionario.cdn_cargo_basic      <= tt-param.cdn_cargo_fim              and*/
            funcionario.nom_pessoa_fisic     >= c-nm-ini                            and
            funcionario.nom_pessoa_fisic     <= c-nm-fim                            and
            funcionario.cdn_tip_contrat_func >= tt-param.cdn_tip_contrat_ini        and
            funcionario.cdn_tip_contrat_func <= tt-param.cdn_tip_contrat_fim        and
           (funcionario.dat_desligto_func     = ?                                   or
           (funcionario.dat_desligto_func    <> ?                                   and
            tt-param.l-imp-desl               = yes))                               and
           (funcionario.dat_admis_transf_func = ?                                   or
            funcionario.dat_admis_transf_func <= tt-param.v_dat_valid)              and
          ((v_log_folha_educnal and funcionario.cdn_tip_contrat_func         > 0)   or
           (not v_log_folha_educnal))
      break by funcionario.cdn_estab
            by if classifica = 3 then
                  string(funcionario.cdn_funcionario,"99999999")
               else
                  funcionario.nom_pessoa_fisic:

      find rh_estab of funcionario no-lock.                          
      assign c-ordem-v1   = "Estabelecimento:"
             c-codigo-v1  = string(rh_estab.cdn_estab,{prghur/dop/eng006.i})
             c-hifem-v1   = "-"
             c-descri-v1  = rh_estab.nom_pessoa_jurid
             c-ordem-v2   = ""
             c-codigo-v2  = ""
             c-hifem-v2   = ""
             c-descri-v2  = "".

      find rh_ccusto of func_ccusto no-lock.

          if not v_log_folha_educnal then 
             run pi-acompanhar in v_han_acomp (input string(funcionario.cdn_funcionario)).            
          else 
             run pi-acompanhar in v_han_acomp (input string(funcionario.cdn_func_centrdor)).            

          if first-of(funcionario.cdn_estab)
          or line-counter > 63 then do:
             page.
          end.
          if classifica = 3 then do:
             if v_log_folha_educnal then do:
                if funcionario.cdn_func_centrdor < tt-param.cdn_func_centrdor_ini or
                   funcionario.cdn_func_centrdor > tt-param.cdn_func_centrdor_fim then 
                   next.

                if funcionario.cdn_tip_contrat_func < tt-param.cdn_tip_contrat_ini or
                   funcionario.cdn_tip_contrat_func > tt-param.cdn_tip_contrat_fim then
                   next. 
             end.      
          end.

          assign l-real = no
                 l-urv  = no.
          
          if  not tt-param.v_log_imp_ult_movto then do:

              ASSIGN i_tipo_ini = tt-param.idi_tip_cargo_funcao
                     i_tipo_fim = tt-param.idi_tip_cargo_funcao.

              IF tt-param.idi_tip_cargo_funcao = 3  THEN
                  ASSIGN i_tipo_ini = 1.

              assign l-lista = no.

              for each histor_sal_func of funcionario no-lock where
                       histor_sal_func.dat_liber_sal >= tt-param.d-dt-ini and
                       histor_sal_func.dat_liber_sal <= tt-param.d-dt-fim and
                       histor_sal_func.cdn_cargo_basic >= tt-param.cdn_cargo_ini and 
                       histor_sal_func.cdn_cargo_basic <= tt-param.cdn_cargo_fim AND
                       histor_sal_func.idi_tip_cargo_funcao >= i_tipo_ini AND
                       histor_sal_func.idi_tip_cargo_funcao <= i_tipo_fim
                       break by histor_sal_func.cdn_funcionario
                             by histor_sal_func.dat_liber_sal
                             by histor_sal_func.num_seq_histor_sal
                             by histor_sal_func.cdn_motiv_liber_sal:

                  if histor_sal_func.idi_sit_histor_funcao <> 0 then
                     assign v_des_sit_histor_funcao = {database/inpy/i03py102.i 04 histor_sal_func.idi_sit_histor_funcao}.
                  else
                     assign v_des_sit_histor_funcao = {database/inpy/i03py102.i 04 1}.

                  /*if histor_sal_func.idi_tip_cargo_funcao <> 0 then
                     assign v_des_tip_cargo_func = {database/inpy/i02py102.i 04 histor_sal_func.idi_tip_cargo_funcao}.
                  else
                     assign v_des_tip_cargo_func = {database/inpy/i02py102.i 04 1}.
                    */
                  
                  if first-of(histor_sal_func.cdn_funcionario) then do:
                      assign d-perc-aumento = 0
                             d-salar-anter  = 0
                             r-rec-hist = rowid(histor_sal).
                      find bhistsal where rowid(bhistsal) = r-rec-hist NO-LOCK NO-ERROR.
                      find prev bhistsal of funcionario no-lock no-error.

                      assign d-salar-prim = if available bhistsal
                                            then bhistsal.val_salario_mensal
                                            else 0.
             
                      if histor_sal_func.dat_liber_sal       = d-dt-corte    and
                         histor_sal_func.val_salario_mensal  < d-salar-anter and
                         l-real                              = no then

                         assign l-real        = yes
                                d-salar-anter = if d-salar-prim <> 0
                                                   then trunc(d-salar-prim / 1000,3)
                                                else trunc(histor_sal_func.val_salario_mensal / 1000,3)
                                d-salar-prim  = 0.
                      else
                          if histor_sal_func.dat_liber_sal      = d-dt-urv            and
                             histor_sal_func.val_salario_mensal < d-salar-anter and
                             l-urv                              = no then

                              assign l-urv         = yes
                                     d-salar-anter = if d-salar-prim <> 0 then trunc(d-salar-prim / d-val-urv,3)
                                                     else trunc(histor_sal_func.val_salario_mensal / d-val-urv,3)
                                     d-salar-prim  = 0.
                          else
                              assign d-salar-anter = if  d-salar-prim <> 0 then d-salar-prim
                                                     else histor_sal_func.val_salario_mensal.

                      assign d-perc-aumento = truncate(round((((histor_sal_func.val_salario_mensal / d-salar-anter) - 1) * 100),4),4)
                             d-salar-anter = histor_sal_func.val_salario_mensal.

                      if  not v_log_folha_educnal then
                          assign v_cod_matr = string(funcionario.cdn_funcionario,"zzzzzzz9") + "-" + string(funcionario.num_digito_verfdor_func,"9"). 
                      else 
                          assign v_cod_matr = string(funcionario.cdn_func_centrdor,"zzzzz9") + "/" + string(funcionario.cdn_tip_contrat_func,"99") + "-" + string(funcionario.num_digito_verfdor_func,"9").
                   end.

                   else do:
                       if histor_sal_func.dat_liber_sal   = d-dt-corte    and
                          histor_sal_func.val_salario_mensal    < d-salar-anter and
                          l-real               = no then
                           assign l-real        = yes
                                  d-salar-anter = truncate(d-salar-anter / 1000,3).
                       if histor_sal_func.dat_liber_sal   = d-dt-urv      and
                          histor_sal_func.val_salario_mensal    < d-salar-anter and
                          l-urv                = no then
                           assign l-urv         = yes
                                  d-salar-anter = truncate(d-salar-anter / d-val-urv,3).
                       assign d-perc-aumento = truncate(round(
                                               (((histor_sal_func.val_salario_mensal /
                                                  d-salar-anter) - 1) * 100),4),4)
                              d-salar-anter = histor_sal_func.val_salario_mensal.
                   end.

                   run pi-imprime-historicos.

              end. /* for each histor_sal_func */

              if l-lista then
                  put "".
          end. /* not tt-param.v_log_imp_ult_movto */

          else
              run pi-imprime-ult-movto.
   end.
   RUN pi-encerra-excel.
end.

if  classifica > 4 THEN
    run "prghur/esp/esfp1661r1.p".

{prghur/fpp/fp9240.i11 prghur/esp/esfp1661rt.p}



if tt-param.parametro then do: 
   def var c-destino-impressao as char format "x(15)" no-undo.
   &if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
       DEFINE VARIABLE cAuxTraducao001 AS CHARACTER NO-UNDO.
       ASSIGN cAuxTraducao001 = {varinc/var00002.i 04 tt-param.destino}.
       run utp/ut-liter.p (INPUT REPLACE(TRIM(cAuxTraducao001)," ","_"),
                           INPUT "",
                           INPUT "").
       ASSIGN  c-destino-impressao = RETURN-VALUE.
   &else
       ASSIGN c-destino-impressao = {varinc/var00002.i 04 tt-param.destino}.
   &endif
   page.
   hide all.
   IF NOT tt-param.l-habilitaRtf THEN
    view frame f-cabec.
   IF NOT tt-param.l-habilitaRtf THEN
    view frame f-rodape.

  if not v_log_folha_educnal then do:
     run pi_report_selec.
     put  "Centro de Custo:"       to 40 space(1)
           c-cc-ini 
           "     |<  >|  "
           c-cc-fim    skip
           "Turno:"              to 40 space(1)
           tt-param.cdn_turno_ini  format "9999" &if "{&cd_rel_hr}" >= "2.11" &then  space(12)  &endif
           "         |<  >|  "
           tt-param.cdn_turno_fim  format "9999"  skip
           "Cargo B†sico:"       to 40 space(1)
           tt-param.cdn_cargo_ini        &if "{&cd_rel_hr}" >= "2.11" &then  space(12)  &endif
           "       |<  >|  "
           tt-param.cdn_cargo_fim    skip
           "Nome Abreviado:"     to 40 space(1)
           c-nm-ini format "x(10)"       &if "{&cd_rel_hr}" >= "2.11" &then  space(12)  &endif
           "   |<  >|  "
           c-nm-fim format "x(10)"   skip
           "Motivo:"             to 40 space(1)
           i-mt-ini format "999"         &if "{&cd_rel_hr}" >= "2.11" &then  space(12)  &endif
           "          |<  >|  "
           i-mt-fim format "999" skip
           "Data de Corte:"      to 40 space(1)
           d-dt-ini format "99/99/9999"  &if "{&cd_rel_hr}" >= "2.11" &then  space(12)  &endif
           "   |<  >|  "
           d-dt-fim format "99/99/9999" skip(2).
  end.
  else
     put   "Seleá∆o:"               to 20 skip(1)
           "Estabelecimento:"       to 40 space(1)
           i-es-ini
           "          |<  >|  "
           i-es-fim                  skip
           "Matr°cula:"             to 40 space(1)
           cdn_func_centrdor_ini
           "      |<  >|"
           cdn_func_centrdor_fim     skip
           "N°vel Unidade Lotaá∆o:" to 40 space(1)
           v_num_opcao               skip
           "Unidade Lotaá∆o:"       to 40 space(1)
           v_cod_unid_lotac_ini 
           "  |<  >|    "
           v_cod_unid_lotac_fim      skip
           "Centro de Custo:"       to 40 space(1)
           c-cc-ini    
           "     |<  >|  "
           c-cc-fim    skip
           "Turno:"              to 40 space(1)
           tt-param.cdn_turno_ini  format "9999"
           "         |<  >|  "
           tt-param.cdn_turno_fim  format "9999"  skip
           "Cargo B†sico:"       to 40 space(1)
           tt-param.cdn_cargo_ini
           "       |<  >|  "
           tt-param.cdn_cargo_fim    skip
            "Nome Abreviado:"     to 40 space(1)
           c-nm-ini format "x(10)"   
           "   |<  >|  "
           c-nm-fim format "x(10)"   skip
           "Motivo:"             to 40 space(1)
           i-mt-ini format "999"
           "          |<  >|  "
           i-mt-fim format "999" skip
           "Data de Corte:"      to 40 space(1)
           d-dt-ini format "99/99/9999"      
           "   |<  >|  "
           d-dt-fim format "99/99/9999" 
           "Tipo:"               to 40 space(1)
           cdn_tip_contrat_ini
           "           |<  >|   " 
           cdn_tip_contrat_fim       skip(2).      

  run pi_report_param.

  case tt-param.idi_imp_ult_mov_tip:
      when 1 then
          {utp/ut-liter.i Cargo MFP R}
      when 2 then
          {utp/ut-liter.i Sal†rio MFP R}
      otherwise
          {utp/ut-liter.i Ambos MFP R}
  end case.
  assign v_idi_imp_ultim_mov = if tt-param.v_log_imp_ult_movto then return-value
                               else " - ".

  case tt-param.idi_tip_cargo_funcao :
      when 1 then
          {utp/ut-liter.i Cargo MFP R}
      when 2 then
          {utp/ut-liter.i Funá∆o MFP R}
      otherwise
          {utp/ut-liter.i Ambos MFP R}
  end case.
  assign v_idi_tipo = return-value.

  
  put "Imprime ultima movimentaá∆o:" to 40 space(1)
      tt-param.v_log_imp_ult_movto format "sim/n∆o"
      "Tipo Ultima movimentaá∆o:"    to 40 space(1)
      v_idi_imp_ultim_mov
      "Tipo:"                        to 40 space(1)
      v_idi_tipo.

  if tt-param.idi_tip_cla_motiv = 1 then
      {utp/ut-liter.i Seleá∆o  MFP R}
  else
      {utp/ut-liter.i Digitaá∆o MFP R}
  assign v_tip_class_motivo = return-value.

  put "Classificaá∆o Motivo:" to 40 space(1)
      v_tip_class_motivo.

  run pi_report_impres.
end.

   {include/i-rpclo.i}
run pi-finalizar in v_han_acomp.

return "OK".


procedure pi-imprime-historicos:
    
    if  tt-param.idi_tip_cla_motiv = 2 then do:
        find tt-digita no-lock where
             tt-digita.cdn_motiv_liber_sal = histor_sal_func.cdn_motiv_liber_sal no-error.
        if  not avail tt-digita then
            next.
        else
            assign v_des_motiv_liber_sal = tt-digita.des_motiv_liber_sal.
    end.
    else do:
        if histor_sal_func.cdn_motiv_liber_sal  >= i-mt-ini and
           histor_sal_func.cdn_motiv_liber_sal  <= i-mt-fim then do:
            find motiv_liber_sal no-lock where
                 motiv_liber_sal.cdn_motiv_liber_sal = histor_sal_func.cdn_motiv_liber_sal no-error.
            if  avail motiv_liber_sal then
                assign v_des_motiv_liber_sal = motiv_liber_sal.des_motiv_liber_sal.
            else
                next.
        end.
        else
            next.
    end.
    /***/

    find tt-motivo-func where
         tt-motivo-func.cdn_empresa     = histor_sal_func.cdn_empresa     and
         tt-motivo-func.cdn_estab       = histor_sal_func.cdn_estab       and
         tt-motivo-func.cdn_motivo      = histor_sal_func.cdn_motiv_liber_sal no-lock no-error.
    if  not available tt-motivo-func then do:
        create tt-motivo-func.
        assign tt-motivo-func.cdn_empresa     = histor_sal_func.cdn_empresa
               tt-motivo-func.cdn_estab       = histor_sal_func.cdn_estab
               tt-motivo-func.cdn_motivo      = histor_sal_func.cdn_motiv_liber_sal
               tt-motivo-func.qtde-func       = tt-motivo-func.qtde-func + 1.
    end.
    else
        assign tt-motivo-func.qtde-func = tt-motivo-func.qtde-func + 1.
    
    
    find first cargo 
         where cargo.cdn_cargo_basic      = histor_sal_func.cdn_cargo_basic
           and cargo.cdn_niv_cargo        = histor_sal_func.cdn_niv_cargo
           and (cargo.idi_tip_cargo_funcao = histor_sal_func.idi_tip_cargo_funcao or
                cargo.idi_tip_cargo_funcao = 0) no-lock no-error.
    if not avail cargo then
       next.

    if   histor_sal_func.dat_liber_sal > d-dt-corte then
         assign l-real = yes.
    if   histor_sal_func.dat_liber_sal > d-dt-urv then
         assign l-urv = yes.

    if d-perc-aumento < 0
    or d-perc-aumento = ?
    or d-perc-aumento > 9999 then
       assign d-perc-aumento = 0.

    if not l-lista and 
       line-counter <= 63
       then do:
       display v_cod_matr
               funcionario.nom_pessoa_fisic
               with frame f-matric.
       down with frame f-matric.
    end.   
    if line-counter > 63 then do:
       page.
       display v_cod_matr
               funcionario.nom_pessoa_fisic
               with frame f-matric.
       down with frame f-matric.
    end.   

    ASSIGN de_trunc_val_salario_categ    = 0
           de_trunc_val_salario_mensal   = 0.


    CASE tt-param.num_casas_dec:
        WHEN 2 THEN DO:
            ASSIGN  de_trunc_val_salario_categ                          = trunc(histor_sal_func.val_salario_categ,2)
                    de_trunc_val_salario_mensal                         = TRUNC(histor_sal_func.val_salario_mensal,2)    
                    de_trunc_val_salario_categ:FORMAT  IN FRAME f-dados = "zzzzzz,zz9.99"
                    de_trunc_val_salario_mensal:FORMAT IN FRAME f-dados = "zzzzzz,zz9.99".


           if v_log_imprime then do:
                if tt-param.idi_tip_cargo_funcao >= 1 and
                   tt-param.idi_tip_cargo_funcao <= 3 then do:
                   RUN pi-dados-excel.
                   
                   ASSIGN ch-excel:Range( "I" + STRING(i-cont-linha,'99999')):VALUE = histor_sal_func.dat_liber_sal                                         
                          ch-excel:Range( "J" + STRING(i-cont-linha,'99999')):VALUE = histor_sal_func.dat_livre_1                                           
                          ch-excel:Range( "K" + STRING(i-cont-linha,'99999')):VALUE = histor_sal_func.dat_term_valid_funcao                                   
                          ch-excel:Range( "L" + STRING(i-cont-linha,'99999')):VALUE = histor_sal_func.cdn_motiv_liber_sal                          
                          ch-excel:Range( "M" + STRING(i-cont-linha,'99999')):VALUE = v_des_motiv_liber_sal                              
                          ch-excel:Range( "N" + STRING(i-cont-linha,'99999')):VALUE = cargo.cdn_cargo_basic       
                          ch-excel:Range( "O" + STRING(i-cont-linha,'99999')):VALUE = cargo.cdn_niv_cargo         
                          ch-excel:Range( "P" + STRING(i-cont-linha,'99999')):VALUE = cargo.des_cargo                                    
                          ch-excel:Range( "Q" + STRING(i-cont-linha,'99999')):VALUE = de_trunc_val_salario_categ  
                          ch-excel:Range( "R" + STRING(i-cont-linha,'99999')):VALUE = de_trunc_val_salario_mensal 
                          ch-excel:Range( "S" + STRING(i-cont-linha,'99999')):VALUE = d-perc-aumento
                       .

                   RUN pi-salario-anterior.

                   ASSIGN i-cont-linha = i-cont-linha + 1.
                end.

            end. /*v_log_imprime*/
        END.
        WHEN 3 THEN DO:
            ASSIGN  de_trunc_val_salario_categ                          = trunc(histor_sal_func.val_salario_categ,3)
                    de_trunc_val_salario_mensal                         = TRUNC(histor_sal_func.val_salario_mensal,3)    
                    de_trunc_val_salario_categ:FORMAT IN FRAME f-dados  = "zz,zzz,zz9.999"
                    de_trunc_val_salario_mensal:FORMAT IN FRAME f-dados = "zz,zzz,zz9.999".

            if v_log_imprime then do:
                if tt-param.idi_tip_cargo_funcao >= 1 and
                   tt-param.idi_tip_cargo_funcao <= 3 then do:
                    RUN pi-dados-excel.
                    ASSIGN ch-excel:Range( "I" + STRING(i-cont-linha,'99999')):VALUE = histor_sal_func.dat_liber_sal                         
                           ch-excel:Range( "J" + STRING(i-cont-linha,'99999')):VALUE = histor_sal_func.dat_livre_1                           
                           ch-excel:Range( "K" + STRING(i-cont-linha,'99999')):VALUE = histor_sal_func.dat_term_valid_funcao                 
                           ch-excel:Range( "L" + STRING(i-cont-linha,'99999')):VALUE = histor_sal_func.cdn_motiv_liber_sal                   
                           ch-excel:Range( "M" + STRING(i-cont-linha,'99999')):VALUE = v_des_motiv_liber_sal                                 
                           ch-excel:Range( "N" + STRING(i-cont-linha,'99999')):VALUE = cargo.cdn_cargo_basic                     
                           ch-excel:Range( "O" + STRING(i-cont-linha,'99999')):VALUE = cargo.cdn_niv_cargo                       
                           ch-excel:Range( "P" + STRING(i-cont-linha,'99999')):VALUE = cargo.des_cargo                                       
                           ch-excel:Range( "Q" + STRING(i-cont-linha,'99999')):VALUE = de_trunc_val_salario_categ                
                           ch-excel:Range( "R" + STRING(i-cont-linha,'99999')):VALUE = de_trunc_val_salario_mensal               
                           ch-excel:Range( "S" + STRING(i-cont-linha,'99999')):VALUE = d-perc-aumento                            
                        .                                                                

                    RUN pi-salario-anterior.

                    ASSIGN i-cont-linha = i-cont-linha + 1.
                end.
            end. /*v_log_imprime*/
        END.
        WHEN 4 THEN DO:
            ASSIGN  de_trunc_val_salario_categ                          = trunc(histor_sal_func.val_salario_categ,4)
                    de_trunc_val_salario_mensal                         = TRUNC(histor_sal_func.val_salario_mensal,4)
                    de_trunc_val_salario_categ:FORMAT IN FRAME f-dados  = "zz,zzz,zz9.9999"
                    de_trunc_val_salario_mensal:FORMAT IN FRAME f-dados = "zz,zzz,zz9.9999".


            if v_log_imprime then do:
                if tt-param.idi_tip_cargo_funcao >= 1 and
                   tt-param.idi_tip_cargo_funcao <= 3 then do:
                    RUN pi-dados-excel.
                    ASSIGN ch-excel:Range( "I" + STRING(i-cont-linha,'99999')):VALUE = histor_sal_func.dat_liber_sal                                       
                           ch-excel:Range( "J" + STRING(i-cont-linha,'99999')):VALUE = histor_sal_func.dat_livre_1                                         
                           ch-excel:Range( "K" + STRING(i-cont-linha,'99999')):VALUE = histor_sal_func.dat_term_valid_funcao                               
                           ch-excel:Range( "L" + STRING(i-cont-linha,'99999')):VALUE = histor_sal_func.cdn_motiv_liber_sal                                 
                           ch-excel:Range( "M" + STRING(i-cont-linha,'99999')):VALUE = v_des_motiv_liber_sal                                               
                           ch-excel:Range( "N" + STRING(i-cont-linha,'99999')):VALUE = cargo.cdn_cargo_basic         
                           ch-excel:Range( "O" + STRING(i-cont-linha,'99999')):VALUE = cargo.cdn_niv_cargo           
                           ch-excel:Range( "P" + STRING(i-cont-linha,'99999')):VALUE = cargo.des_cargo                           
                           ch-excel:Range( "Q" + STRING(i-cont-linha,'99999')):VALUE = de_trunc_val_salario_categ    
                           ch-excel:Range( "R" + STRING(i-cont-linha,'99999')):VALUE = de_trunc_val_salario_mensal   
                           ch-excel:Range( "S" + STRING(i-cont-linha,'99999')):VALUE = d-perc-aumento                
                        .                                                                

                    RUN pi-salario-anterior.

                    ASSIGN i-cont-linha = i-cont-linha + 1.
                end.
                                                  
            end. /*v_log_imprime*/
        END.

    END CASE.

    FOR EACH HISTOR_DET_REMUN_VAR_FUNC NO-LOCK WHERE 
             HISTOR_DET_REMUN_VAR_FUNC.CDN_EMPRESA         = HISTOR_SAL_FUNC.CDN_EMPRESA          AND 
             HISTOR_DET_REMUN_VAR_FUNC.CDN_ESTAB           = HISTOR_SAL_FUNC.CDN_ESTAB            AND
             HISTOR_DET_REMUN_VAR_FUNC.CDN_FUNCIONARIO     = HISTOR_SAL_FUNC.CDN_FUNCIONARIO      AND
             HISTOR_DET_REMUN_VAR_FUNC.DAT_LIBER_SAL       = HISTOR_SAL_FUNC.DAT_LIBER_SAL        AND
             HISTOR_DET_REMUN_VAR_FUNC.NUM_SEQ_HISTOR_SAL  = HISTOR_SAL_FUNC.NUM_SEQ_HISTOR_SAL   AND
             HISTOR_DET_REMUN_VAR_FUNC.CDN_MOTIV_LIBER_SAL = HISTOR_SAL_FUNC.CDN_MOTIV_LIBER_SAL,
       FIRST REMUN_VAR NO-LOCK OF HISTOR_DET_REMUN_VAR_FUNC
        BREAK BY HISTOR_DET_REMUN_VAR_FUNC.DAT_LIBER_SAL:

        IF FIRST-OF (HISTOR_DET_REMUN_VAR_FUNC.DAT_LIBER_SAL) THEN DO:

           DISP V_LABEL1
                V_LABEL2
                V_LABEL3
                WITH FRAME F-LABELS.
                DOWN WITH FRAME F-LABELS.

           DISP V_TRACO1
                V_TRACO2
                V_TRACO3    
                WITH FRAME F-TRACO.
                DOWN WITH FRAME F-TRACO.
        END.    

        DISP HISTOR_DET_REMUN_VAR_FUNC.CDN_REMUN_VAR
             REMUN_VAR.DES_REMUN_VAR
             HISTOR_DET_REMUN_VAR_FUNC.VAL_REMUN_VAR
             WITH FRAME F-EXPLODE.
             DOWN WITH FRAME F-EXPLODE.
    END.

    assign l-lista = yes.

end procedure.

procedure pi-imprime-ult-movto:

    assign l-lista = no.

    for each bhistsal of funcionario no-lock where
             bhistsal.dat_liber_sal >= tt-param.d-dt-ini and
             bhistsal.dat_liber_sal <= tt-param.d-dt-fim
    break by bhistsal.cdn_funcionario
          by bhistsal.dat_liber_sal
          by bhistsal.num_seq_histor_sal
          by bhistsal.cdn_motiv_liber_sal:

        if first-of(bhistsal.cdn_funcionario) then do:
            assign d-perc-aumento = 0
                   d-salar-anter  = 0
                   r-rec-hist     = rowid(bhistsal).
            find histor_sal_func where rowid(histor_sal_func) = r-rec-hist no-lock no-error.
            find prev histor_sal_func of funcionario no-lock no-error.

            assign d-salar-prim = if avail histor_sal_func then
                                           histor_sal_func.val_salario_mensal
                                  else 0.
                
            if bhistsal.dat_liber_sal       = d-dt-corte    and
               bhistsal.val_salario_mensal  < d-salar-anter and
               l-real = no then
                assign l-real        = yes
                       d-salar-anter = if d-salar-prim <> 0
                                          then trunc(d-salar-prim / 1000,3)
                                       else trunc(bhistsal.val_salario_mensal / 1000,3)
                       d-salar-prim  = 0.
            else
                if bhistsal.dat_liber_sal      = d-dt-urv      and
                   bhistsal.val_salario_mensal < d-salar-anter and
                   l-urv = no then
                    assign l-urv         = yes
                           d-salar-anter = if  d-salar-prim <> 0
                                               then trunc(d-salar-prim / d-val-urv,3)
                                           else trunc(bhistsal.val_salario_mensal / d-val-urv,3)
                           d-salar-prim  = 0.
                else
                    assign d-salar-anter = if  d-salar-prim <> 0
                                               then d-salar-prim
                                           else bhistsal.val_salario_mensal.

            assign d-perc-aumento = truncate(round((((bhistsal.val_salario_mensal / d-salar-anter) - 1) * 100),4),4)
                   d-salar-anter = bhistsal.val_salario_mensal.

            if  not v_log_folha_educnal then
                assign v_cod_matr = string(funcionario.cdn_funcionario,"zzzzzzz9") + "-" + string(funcionario.num_digito_verfdor_func,"9"). 
            else
                assign v_cod_matr = string(funcionario.cdn_func_centrdor,"zzzzz9") + "/" + string(funcionario.cdn_tip_contrat_func,"99") + "-" + string(funcionario.num_digito_verfdor_func,"9").

        end. /* first-of(bhistsal.cdn_funcionario) */
        else do:
            if bhistsal.dat_liber_sal      = d-dt-corte    and
               bhistsal.val_salario_mensal < d-salar-anter and
               l-real = no then
                assign l-real        = yes
                       d-salar-anter = truncate(d-salar-anter / 1000,3).

             if bhistsal.dat_liber_sal      = d-dt-urv      and
                bhistsal.val_salario_mensal < d-salar-anter and
                l-urv = no then
                 assign l-urv         = yes
                        d-salar-anter = truncate(d-salar-anter / d-val-urv,3).

             assign d-perc-aumento = truncate(round((((bhistsal.val_salario_mensal / d-salar-anter) - 1) * 100),4),4)
                    d-salar-anter = bhistsal.val_salario_mensal.
        end.
        
        case tt-param.idi_imp_ult_mov_tip:
            when 1 then do: /* ultimo cargo */
                if  last-of(bhistsal.cdn_funcionario) then do:
                    find first histor_sal_func of bhistsal no-lock no-error.
                    if  avail histor_sal_func then do:
                        repeat:
                            assign v_cdn_func = histor_sal_func.cdn_funcionario.
                            find prev histor_sal_func no-lock no-error.
                            if  avail histor_sal_func and histor_sal_func.cdn_funcionario <> v_cdn_func then leave.
                            if  avail histor_sal_func and histor_sal_func.cdn_cargo_basic <> bhistsal.cdn_cargo_basic then do:
                                find next histor_sal_func no-lock no-error.
                                run pi-imprime-historicos.
                                leave.
                            end.
                        end. /*repeat*/
                    end. /*avail bhistsal*/
                end.
            end.
            when 2 then do: /* ultimo salario */
                if  last-of(bhistsal.cdn_funcionario) then do:
                    find first histor_sal_func of bhistsal no-lock no-error.
                    if   avail histor_sal_func then do:
                        repeat:
                            assign v_cdn_func = histor_sal_func.cdn_funcionario.
                            find prev histor_sal_func no-lock no-error.
                            if  avail histor_sal_func and histor_sal_func.cdn_funcionario    <> v_cdn_func then leave.
                            if  avail histor_sal_func and histor_sal_func.val_salario_mensal <> bhistsal.val_salario_mensal then do:
                                find next histor_sal_func no-lock no-error.
                                if  avail histor_sal_func then do:
                                    run pi-imprime-historicos.
                                    leave.
                                end.
                            end.
                        end. /*repeat*/
                    end. /*avail bhistsal*/
                end.
            end.
            when 3 then do: /* ambos */
                if  last-of(bhistsal.cdn_funcionario) then do:
                    find first histor_sal_func of bhistsal no-lock no-error.
                    if  avail histor_sal_func then do:
                        repeat:
                            assign v_cdn_func = histor_sal_func.cdn_funcionario.
                            find prev histor_sal_func no-lock no-error.
                            if  avail histor_sal_func and histor_sal_func.cdn_funcionario <> v_cdn_func then leave.
                            if  avail histor_sal_func and histor_sal_func.cdn_cargo_basic <> bhistsal.cdn_cargo_basic then do:
                                find next histor_sal_func no-lock no-error.
                                run pi-imprime-historicos.
                                leave.
                            end.
                        end. /*repeat*/
                    end.
                    find first histor_sal_func of bhistsal no-lock no-error.
                    if  avail histor_sal_func then do:
                        repeat:
                            assign v_cdn_func = histor_sal_func.cdn_funcionario.
                            find prev histor_sal_func no-lock no-error.
                            if  avail histor_sal_func and histor_sal_func.cdn_funcionario    <> v_cdn_func then leave.
                            if  avail histor_sal_func and histor_sal_func.val_salario_mensal <> bhistsal.val_salario_mensal then do:
                                find next histor_sal_func no-lock no-error.
                                if  avail histor_sal_func then do:
                                    run pi-imprime-historicos.
                                    leave.
                                end.
                            end.
                        end. /*repeat*/
                    end. /*avail bhistsal*/
                end.
            end.
        end case.     
    end.

end procedure.


procedure pi-abre-excel:
   ASSIGN c-arquivo = ENTRY(1,tt-param.arquivo,".") + ".xls".
   
   FIND FIRST param-global
        NO-LOCK NO-ERROR.

   FILE-INFO:FILE-NAME = c-arquivo.

   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
      OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).

   /* Abro o excel neste ponto */   
   CREATE "Excel.Application" ch-excel.

   ch-excel:VISIBLE=FALSE.
   ch-excel:workbooks:ADD().
   ch-excel:ActiveSheet:NAME = "ESFP1661".

   ch-excel:Range("A2"):select.

   ch-excel:ActiveWindow:FreezePanes = true.

   ASSIGN i-cont-linha = 1.

   ASSIGN ch-excel:columns( "A"):NumberFormat = "@"
          ch-excel:columns( "B"):NumberFormat = "@"
          ch-excel:columns( "C"):NumberFormat = "@"
          ch-excel:columns( "D"):NumberFormat = "@"
          ch-excel:columns( "E"):NumberFormat = "@"
          ch-excel:columns( "F"):NumberFormat = "@"           
          ch-excel:columns( "G"):NumberFormat = "@"           
          ch-excel:columns( "H"):NumberFormat = "@"    
          ch-excel:columns( "I"):NumberFormat = "@"    
          ch-excel:columns( "J"):NumberFormat = "@"    
          ch-excel:columns( "K"):NumberFormat = "@"
          ch-excel:columns( "L"):NumberFormat = "@"
          ch-excel:columns( "M"):NumberFormat = "@"
          ch-excel:columns( "N"):NumberFormat = "@"
          ch-excel:columns( "O"):NumberFormat = "#.##0"
          ch-excel:columns( "P"):NumberFormat = "@"           
          ch-excel:columns( "Q"):NumberFormat = "#.##0,00"           
          ch-excel:columns( "R"):NumberFormat = "#.##0,00"           
          ch-excel:columns( "S"):NumberFormat = "#.##0,00"           
          ch-excel:columns( "T"):NumberFormat = "@"           
          ch-excel:columns( "U"):NumberFormat = "@"           
          ch-excel:columns( "V"):NumberFormat = "@"    
          ch-excel:columns( "W"):NumberFormat = "#.##0,00"
          ch-excel:columns( "X"):NumberFormat = "@"
          ch-excel:columns( "Y"):NumberFormat = "@"
          ch-excel:columns( "Z"):NumberFormat = "@"
          ch-excel:columns("AA"):NumberFormat = "@"
          ch-excel:columns("AB"):NumberFormat = "@"
          ch-excel:columns("AC"):NumberFormat = "@"
          ch-excel:columns("AD"):NumberFormat = "@"
          ch-excel:columns("AE"):NumberFormat = "@"
          ch-excel:columns("AF"):NumberFormat = "@"
          ch-excel:columns("AG"):NumberFormat = "@"
          ch-excel:columns("AH"):NumberFormat = "@"
          ch-excel:columns("AI"):NumberFormat = "@"
          ch-excel:columns("AJ"):NumberFormat = "@"
          ch-excel:columns("AK"):NumberFormat = "@".


   ASSIGN ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = "Empresa" 
          ch-excel:Range( "B" + STRING(i-cont-linha,'99999')):VALUE = "Centro Custo" 
          ch-excel:Range( "C" + STRING(i-cont-linha,'99999')):VALUE = "Descriá∆o" 
          ch-excel:Range( "D" + STRING(i-cont-linha,'99999')):VALUE = "Estabelecimento" 
          ch-excel:Range( "E" + STRING(i-cont-linha,'99999')):VALUE = "Descriá∆o" 
          ch-excel:Range( "F" + STRING(i-cont-linha,'99999')):VALUE = "Matr°cula" 
          ch-excel:Range( "G" + STRING(i-cont-linha,'99999')):VALUE = "Funcion†rio"
          ch-excel:Range( "H" + STRING(i-cont-linha,'99999')):VALUE = "CPF"
          ch-excel:Range( "I" + STRING(i-cont-linha,'99999')):VALUE = "Alteraá∆o"    
          ch-excel:Range( "J" + STRING(i-cont-linha,'99999')):VALUE = "Vigància"     
          ch-excel:Range( "K" + STRING(i-cont-linha,'99999')):VALUE = "Validade"     
          ch-excel:Range( "L" + STRING(i-cont-linha,'99999')):VALUE = "Motivo"       
          ch-excel:Range( "M" + STRING(i-cont-linha,'99999')):VALUE = "Descriá∆o"     
          ch-excel:Range( "N" + STRING(i-cont-linha,'99999')):VALUE = "Cargo/Funá∆o" 
          ch-excel:Range( "O" + STRING(i-cont-linha,'99999')):VALUE = "Nivel"        
          ch-excel:Range( "P" + STRING(i-cont-linha,'99999')):VALUE = "Descriá∆o"    
          ch-excel:Range( "Q" + STRING(i-cont-linha,'99999')):VALUE = "Categoria"    
          ch-excel:Range( "R" + STRING(i-cont-linha,'99999')):VALUE = "Padr∆o Màs"   
          ch-excel:Range( "S" + STRING(i-cont-linha,'99999')):VALUE = "Perc Aumento"
          ch-excel:Range( "T" + STRING(i-cont-linha,'99999')):VALUE = "Cargo/Funá∆o Ant" 
          ch-excel:Range( "U" + STRING(i-cont-linha,'99999')):VALUE = "Nivel Ant"        
          ch-excel:Range( "V" + STRING(i-cont-linha,'99999')):VALUE = "Descriá∆o Ant"    
          ch-excel:Range( "W" + STRING(i-cont-linha,'99999')):VALUE = "Padr∆o Màs Ant"    
       .           

   ASSIGN i-cont-linha = i-cont-linha + 1.
END PROCEDURE.

PROCEDURE pi-dados-excel:  
   ASSIGN ch-excel:Range( "A" + STRING(i-cont-linha,'99999')):VALUE = tt-param.v_cdn_empres_usuar
          ch-excel:Range( "B" + STRING(i-cont-linha,'99999')):VALUE = rh_ccusto.cod_rh_ccusto
          ch-excel:Range( "C" + STRING(i-cont-linha,'99999')):VALUE = rh_ccusto.des_rh_ccusto
          ch-excel:Range( "D" + STRING(i-cont-linha,'99999')):VALUE = string(rh_estab.cdn_estab,{prghur/dop/eng006.i})
          ch-excel:Range( "E" + STRING(i-cont-linha,'99999')):VALUE = rh_estab.nom_pessoa_jurid                       
          ch-excel:Range( "F" + STRING(i-cont-linha,'99999')):VALUE = v_cod_matr                    
          ch-excel:Range( "G" + STRING(i-cont-linha,'99999')):VALUE = funcionario.nom_pessoa_fisic
          ch-excel:Range( "H" + STRING(i-cont-linha,'99999')):VALUE = funcionario.cod_id_feder.
END PROCEDURE.

PROCEDURE pi-encerra-excel:
   /* Encerra o excel */
   ch-excel:application:DisplayAlerts = false.

   ch-excel:Cells:select.
   ch-excel:Cells:EntireColumn:AutoFit.

   ch-excel:ActiveSheet:PageSetup:orientation = 2. 

   ch-excel:Range("A1:W" + string(i-cont-linha - 1)):autofilter(,,,).

   ch-excel:Range("A1:W" + string(i-cont-linha - 1)):select.

   case tt-param.destino:
       when 1 then do:
          ch-excel:worksheets:item(1):select.
          ch-excel:Sheets:PrintOut.
          ch-excel:application:DisplayAlerts = false.
          ch-excel:quit().
       end.
       when 2 then do:
          ch-excel:visible = false.
          ch-excel:ActiveWorkbook:close(yes,c-arquivo).
       end.
       when 3 then do:
          ch-excel:visible = true.
       end.
   end case.

   release object ch-excel no-error.
END PROCEDURE.


PROCEDURE pi-salario-anterior:
   FOR EACH bf_histor_sal_func OF funcionario
            WHERE ROWID(bf_histor_sal_func) <> ROWID(histor_sal_func)
            NO-LOCK,
            FIRST bf_cargo 
                  WHERE bf_cargo.cdn_cargo_basic      = bf_histor_sal_func.cdn_cargo_basic     AND
                        bf_cargo.cdn_niv_cargo        = bf_histor_sal_func.cdn_niv_cargo       AND
                       (bf_cargo.idi_tip_cargo_funcao = bf_histor_sal_func.idi_tip_cargo_funcao OR bf_cargo.idi_tip_cargo_funcao = 0) 
                  NO-LOCK:
      ASSIGN ch-excel:Range( "T" + STRING(i-cont-linha,'99999')):VALUE = bf_cargo.cdn_cargo_basic       
             ch-excel:Range( "U" + STRING(i-cont-linha,'99999')):VALUE = bf_cargo.cdn_niv_cargo         
             ch-excel:Range( "V" + STRING(i-cont-linha,'99999')):VALUE = bf_cargo.des_cargo                                    
             ch-excel:Range( "W" + STRING(i-cont-linha,'99999')):VALUE = TRUNC(bf_histor_sal_func.val_salario_mensal,3).
   END.
END PROCEDURE.

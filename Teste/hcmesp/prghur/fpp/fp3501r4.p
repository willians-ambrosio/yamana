/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i FP3500R4 1.02.11.099 } /*** 0101199 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i fp3500r4 MFP}
&ENDIF

/*******************************************************************************
**
**        Programa: FP3500-4.
**
**        Data....: Abril/1994.
**
**        Autor...: DATASUL S.A.
**
**        Objetivo: Emissao dos Envelopes de Pagamento
**                  Formulario    = 4 (Padrao Individual)
**                  Modelo DE 69154921-10
**
*******************************************************************************/
DEF BUFFER empresa FOR mgcad.empresa.

{include/i-rpvar.i}
{utp/utapi019.i}
def shared var c-imp as cha                      no-undo.
def shared var c-emp as cha  format "x(40)"      no-undo.
def shared var c-tit as cha  format "x(50)"      no-undo.
def shared var i-num as int  format "ZZ"         no-undo.
def shared var da-in as date format "99/99/9999" no-undo.
def shared var da-fi as date format "99/99/9999" no-undo.
def shared var c-rod as cha                      no-undo.
def shared var c-sis as cha  format "x(25)"      no-undo.
def shared var c-lay as cha                      no-undo.
def shared var v_num as int                      no-undo.
def shared var c-arq as cha                      no-undo.
def shared var i-pag as int                      no-undo.


def var v_cdn_turno_trab  like funcionario.cdn_turno_trab       no-undo.
def var v_cdn_turma_trab  like funcionario.cdn_turma_trab       no-undo.
def var v_remetente       as char format "x(50)"                no-undo.
def buffer b2funcionario for funcionario.

DEF BUFFER bfunc_turno_trab FOR func_turno_trab.
DEF BUFFER bfunciona FOR funcionario.
def buffer bfuncionario for funcionario.
def buffer cfuncionario for funcionario.
def var v_log_sem_salario as logical no-undo.

assign c-impressora    = c-imp
       c-empresa       = c-emp
       c-titulo-relat  = c-tit
       i-numper-x      = i-num
       da-iniper-x     = da-in
       da-fimper-x     = da-fi
       c-rodape        = c-rod
       c-sistema       = c-sis
       c-layout        = c-lay
       v_num_count     = v_num
       c-arq-control   = c-arq
       i-page-size-rel = i-pag.

{include/i_dbvers.i}
{prghur/fpp/fp3501tt.i shared}
{prghur/fpp/fp9200.i10 shared}
{prghur/fpp/fp9200.i8}
{prghur/fpp/fp9400.i}

define buffer b-tt-digita for tt-digita.
find first tt-param.

/* chamada epc */             
{include/i-epc200.i fp3500r4}

def shared var v_han_acomp as handle no-undo.

define     shared var l-imprime  as logical                            no-undo.
define     var c-inc-liquido     as char format "x(01)"                no-undo.
define     shared var i-empresa like empresa.ep-codigo                 no-undo.
define     shared var i-ord-aux as int                                 no-undo.
define     shared var l-origem  as log format "Coletiva/Individual"    no-undo.
define var v_log_folha_educnal as log  initial no                      no-undo.
define var i-ind               as int                                  no-undo.
define var l-dep-cru           as log                                  no-undo.
define var d-val-urv           as dec format "z,zz9.99"                no-undo.
define var d-val-crs           as dec format "zzzzz,zzz,zz9.99"        no-undo.
define var i-index             as int                                  no-undo.
define var i-inx               as int                                  no-undo.
define var l-tem-movto         as log                                  no-undo.
define var i-cont-linha        as int                                  no-undo.
define var c-tab-identif       as cha initial "V,D,O"                  no-undo.
define var d-total-vencto      as dec format ">>>>>>,>>9.99+"          no-undo.
define var d-total-descto      as dec format ">>>>>>,>>9.99"           no-undo.
define var c-dep-sec           as cha format "x(07)"                   no-undo.
define var c-mes-folha         as cha format "x(09)"                   no-undo.
define var c-categoria         as cha format "x(07)"                   no-undo.
define var c-lab-bco           as cha format "x(10)"                   no-undo.
define var i-cod-banco         as int format "ZZZ"                     no-undo.
define var i-cod-agencia       as int format "ZZZZ"                    no-undo.
define var i-cta-corrente      as int format "ZZZZZZZZZ"               no-undo.
define var c-dig-conta         as cha format "x(02)"                   no-undo.
define var d-liquido-pagar     as dec format ">>>>>>>,>>9.99"          no-undo.
define var d-base-iapas        as dec format ">>>>>>,>>9.99"           no-undo.
define var d-limite-inss       as dec format ">>>>>>,>>9.99"           no-undo.
define var d-base-fgts         as dec format ">>>>>>,>>9.99"           no-undo.
define var d-valor-fgts        as dec format ">>>>>,>>9.99"            no-undo.
define var d-base-irrf         as dec format ">>>>>>,>>9.99"           no-undo.
define var d-salar-atual       as dec                                  no-undo.
define var c-mensagem          as cha format "x(50)"   extent 2        no-undo.
define var c-mens-contr        as cha format "x(50)"   extent 2        no-undo.
define var c-hifen             as cha format "x(01)"                   no-undo.
define var c-parabens          as cha format "x(40)"                   no-undo.
define var l-prim-vez          as log                                  no-undo.
define var c-prenome           as cha format "x(31)"                   no-undo.
define var c-sobrenome         as cha format "x(31)"                   no-undo.
define var c-nome-compl        as cha format "x(31)"                   no-undo.
define var i-index1            as int                                  no-undo.
define var c-byte              as cha format "x(01)"                   no-undo.
define var c-lit-mes           as cha format "x(09)" extent 12         no-undo
                               initial ["  Janeiro","Fevereiro","    Marco",
                                        "    Abril","     Maio","    Junho",
                                        "    Julho","   Agosto"," Setembro",
                                        "  Outubro"," Novembro"," Dezembro"].
define var i-impr-func         as char format "x(11)"                  no-undo.
define var i-contador          as int  initial 0                       no-undo.
define var i-matr-ini        like funcionario.cdn_funcionario          no-undo.
define var i-matr-fim        like funcionario.cdn_funcionario          no-undo.
define var c-hifem             as char format "x" initial "-"          no-undo.
define var c-barra             as char format "x" initial "/"          no-undo.
DEF    VAR v_status_enviado  AS CHAR FORMAT "x(11)"                    NO-UNDO.
DEF    VAR v_status_nao_enviado  AS CHAR FORMAT "x(11)"                NO-UNDO.

DEF VAR c_destino  AS CHAR FORMAT "x(60)" NO-UNDO.
DEF VAR c_anexo    AS CHAR FORMAT "x(60)" NO-UNDO.
DEF VAR c_assunto  AS CHAR FORMAT "x(21)" NO-UNDO.
DEF VAR c_senha    AS CHAR FORMAT "x(16)" NO-UNDO.
DEF VAR c_msg_erro AS CHAR FORMAT "x(60)" NO-UNDO.
DEF VAR l-ErroZip  AS LOGICAL             NO-UNDO.
DEF VAR l-compactar  AS LOGICAL INIT YES   NO-UNDO.
DEF VAR c_status_zip AS CHAR FORMAT "x(3)" NO-UNDO.

/* Variòveis utilizadas na pi_histor_sal_func */
def var v_val_sal_cat             like histor_sal_func.val_salario_categ  no-undo.
def var v_cdn_tab_sal             like histor_sal_func.cdn_tab_sal        no-undo.
def var v_num_fx_sal              like histor_sal_func.num_faixa_sal      no-undo.
def var v_num_niv_sal             like histor_sal_func.num_niv_sal        no-undo.
def var v_cdn_cargo_basic_funcao  like funcionario.cdn_cargo_basic        no-undo.
def var v_cdn_niv_cargo_funcao    like funcionario.cdn_niv_cargo          no-undo.
def var v_val_sal_mes_funcao      like histor_sal_func.val_salario_mensal no-undo.
def var v_val_sal_hor_funcao      like histor_sal_func.val_salario_hora   no-undo.
def var v_val_sal_cat_funcao      like histor_sal_func.val_salario_categ  no-undo.
def var v_cdn_tab_sal_funcao      like histor_sal_func.cdn_tab_sal        no-undo.
def var v_num_fx_sal_funcao       like histor_sal_func.num_faixa_sal      no-undo.
def var v_num_niv_sal_funcao      like histor_sal_func.num_niv_sal        no-undo.
def var d-total-descto-autoriz      as dec format ">>>>>>,>>9.99"         no-undo.

def var v_cdn_cargo_basic like funcionario.cdn_cargo_basic        no-undo.
def var v_cdn_niv_cargo   like funcionario.cdn_niv_cargo          no-undo.
def var v_val_sal_mensal  like histor_sal_func.val_salario_mensal no-undo.
def var v_val_sal_hora    like histor_sal_func.val_salario_hora   no-undo.
def var d-hrs-categ       like histor_sal_func.qtd_hrs_categ_sal  no-undo.
def var d-sal-cat         like histor_sal_func.val_salario_categ  no-undo.
def var d-sal-mes         like histor_sal_func.val_salario_mensal no-undo.
def var v_cdn_categ_sal   like funcionario.cdn_categ_sal          no-undo.
DEF VAR d-horas-semanais  LIKE apont_hora_aula.qtd_hrs_trab_semanal NO-UNDO.
def var c-carga-horaria     as CHAR format "x(21)"                  no-undo.

def        var dt-ini-per        as date no-undo.
def        var dt-fim-per        as date no-undo.


define temp-table w-mvtocalc no-undo
       field fc-codigo   like funcionario.cdn_funcionario
       field ev-codigo   LIKE event_fp.cdn_event_fp
       field descricao   like event_fp.des_event_fp     format "x(25)"
       field identif     as integer
       field unidades    as decimal
       field horas       as decimal                format ">>9.999"
       field salar-hora  as decimal                format ">>9.9999"
       field base        as decimal
       field valor       as decimal                format ">>>>>,>>9.99-"
       field inc-liquido like event_fp.idi_tip_inciden_liq.

DEFINE SHARED TEMP-TABLE tt-rel-erros no-undo
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


find empresa no-lock where empresa.ep-codigo = tt-param.v_cdn_empres_usuar  no-error.
find param_empres_rh no-lock where param_empres_rh.cdn_empresa = tt-param.v_cdn_empres_usuar no-error.
find param_folha_educnal no-lock where
     param_folha_educnal.cdn_empresa = v_cdn_empres_usuar no-error.
if avail param_folha_educnal then
   assign v_log_folha_educnal = yes.

{utp/ut-liter.i Carga_Hor†ria_Semanal *}
ASSIGN c-carga-horaria = RETURN-VALUE.

find tab_irf_inss where
     tab_irf_inss.num_ano_refer_tab_irf_inss = tt-param.i-ano-ref and
     tab_irf_inss.num_mes_refer_tab_irf_inss = tt-param.i-mes-ref no-lock no-error.

if  l-origem then do:
    for each tt-digita:
       delete tt-digita.
    end.
    create tt-digita.
    assign tt-digita.num_forma_pagto = tt-param.r-forma-pgto.
end.
else 
    assign tt-param.v_cod_unid_lotac_ini = ""
          tt-param.v_cod_unid_lotac_fim = "Zzzzzzzzzzz"
          tt-param.i-cc-codigo-1        = ""
          tt-param.i-cc-codigo-2        = "Zzzzzzzz"
          tt-param.v_nom_func_ini       = ""
          tt-param.v_nom_func_fim       = "Zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"
          tt-param.l-mensal             = yes
          tt-param.l-horista            = yes
          tt-param.l-semanal            = yes
          tt-param.l-quinzenal          = yes
          tt-param.l-tarefa             = yes
          tt-param.l-diarista           = yes
          tt-param.i-bc-codigo-1        = 1
          tt-param.i-bc-codigo-2        = 999
          tt-param.i-ag-codigo-1        = 1
          tt-param.i-ag-codigo-2        = 9999
          tt-param.r-forma-pgto         = 4
          tt-param.cdn_local_pagto_ini  = 0 
          tt-param.cdn_local_pagto_fim  = 99999.

{utp/ut-liter.i N∆o_Enviado *}
ASSIGN v_status_nao_enviado = RETURN-VALUE.
{utp/ut-liter.i Enviado *}
ASSIGN v_status_enviado = RETURN-VALUE.
{utp/ut-liter.i Pagto_ *}
ASSIGN c_assunto = RETURN-VALUE + string(tt-param.v_dat_valid, "99/99/9999").

{include/i-rpcab.i}

    if i-ord-aux = 3 or
       i-ord-aux = 4 or
       i-ord-aux = 5 or
       i-ord-aux = 6 then do:

       {prghur/fpp/fp9200.i6}
       for each tt_lotac_funcionario no-lock,
           each func_ccusto no-lock where
                func_ccusto.cdn_empresa          = tt_lotac_funcionario.cdn_empresa and
                func_ccusto.cdn_estab            = tt_lotac_funcionario.cdn_estab and
                func_ccusto.cdn_funcionario      = tt_lotac_funcionario.cdn_funcionario and
                func_ccusto.dat_inic_lotac_func <= tt-param.v_dat_valid and
                func_ccusto.dat_fim_lotac_func  >= tt-param.v_dat_valid and
                func_ccusto.cod_rh_ccusto       >= tt-param.i-cc-codigo-1 and
                func_ccusto.cod_rh_ccusto       <= tt-param.i-cc-codigo-2,
           each func_categ_sal no-lock where
                func_categ_sal.cdn_empresa          = func_ccusto.cdn_empresa and
                func_categ_sal.cdn_estab            = func_ccusto.cdn_estab and
                func_categ_sal.cdn_funcionario      = func_ccusto.cdn_funcionario and
                func_categ_sal.dat_inic_lotac_func <= tt-param.v_dat_valid and
                func_categ_sal.dat_fim_lotac_func  >= tt-param.v_dat_valid and
              ((func_categ_sal.cdn_categ_sal     = 1 and tt-param.l-mensal    = yes)  or
               (func_categ_sal.cdn_categ_sal     = 2 and tt-param.l-horista   = yes)  or
               (func_categ_sal.cdn_categ_sal     = 3 and tt-param.l-semanal   = yes)  or
               (func_categ_sal.cdn_categ_sal     = 4 and tt-param.l-quinzenal = yes)  or
               (func_categ_sal.cdn_categ_sal     = 5 and tt-param.l-tarefa    = yes)  or
               (func_categ_sal.cdn_categ_sal     = 6 and tt-param.l-diarista  = yes)),
           each funcionario use-index fncnr_py08504 of func_categ_sal no-lock where
                funcionario.nom_pessoa_fisic >= tt-param.v_nom_func_ini            and
                funcionario.nom_pessoa_fisic <= tt-param.v_nom_func_fim            and
                funcionario.cod_rh_ccusto    >= tt-param.i-cc-codigo-1             and
                funcionario.cod_rh_ccusto    <= tt-param.i-cc-codigo-2             and
                funcionario.cdn_local_pagto  >= tt-param.cdn_local_pagto_ini       and
                funcionario.cdn_local_pagto  <= tt-param.cdn_local_pagto_fim       and
              ((tt-param.r-forma-pgto         = 4                                  and
                funcionario.idi_forma_pagto  <> 0)                                 or
                funcionario.idi_forma_pagto   = tt-param.r-forma-pgto )
           break by if i-ord-aux = 3 or
                       i-ord-aux = 4
                    then funcionario.idi_forma_pagto
                    else 0
                 by tt_lotac_funcionario.cdn_estab
                 by tt_lotac_funcionario.num_seq_unid_lotac
                 by tt_lotac_funcionario.num_niv_unid_lotac
                 by tt_lotac_funcionario.cod_unid_lotac
                 by if  i-ord-aux = 3 or
                        i-ord-aux = 5 
                    then string(funcionario.cdn_funcionario,"99999999")
                    else funcionario.nom_pessoa_fisic:

               IF tt-param.v_log_enviar_email THEN DO:
                   {include/i-rpout.i &pagesize=0}
               END.

               if v_log_folha_educnal THEN DO:
                  if funcionario.cdn_tip_contrat_func <> 0 THEN DO:
                     IF tt-param.v_log_enviar_email THEN DO:
                       {include/i-rpclo.i}
                     END.
                     next.
                  END.
               END.
               assign i-contador = i-contador + 1.        
               run pi-acompanhar in v_han_acomp (input i-contador).   
               /******** Chamada EPC - Santa Casa **********/
               RUN pi-valida-retencao-ir.
               IF RETURN-VALUE = "NOK" THEN NEXT.
               /******** Fim Chamada EPC - Santa Casa **********/
               find rh_pessoa_fisic of funcionario no-lock no-error.
               find rh_estab of funcionario no-lock no-error.
                if funcionario.idi_forma_pagto       <> 3 then do:
                   if funcionario.cdn_bco_liq         < tt-param.i-bc-codigo-1  or
                      funcionario.cdn_bco_liq         > tt-param.i-bc-codigo-2  or
                      funcionario.cdn_agenc_bcia_liq  < tt-param.i-ag-codigo-1  or
                      funcionario.cdn_agenc_bcia_liq  > tt-param.i-ag-codigo-2  then do:
                       IF tt-param.v_log_enviar_email THEN DO:
                         {include/i-rpclo.i}
                       END.
                       next.
                   END.
                end.              

                if tt-param.i-mes-ref = 12 then do:
                  if  funcionario.dat_desligto_func <> ?                                           and
                     (funcionario.dat_desligto_func <= (date(01,01,(tt-param.i-ano-ref + 1)) - 1)) and
                      tt-param.l-emite-demi          = no then do:
                      IF tt-param.v_log_enviar_email THEN DO:
                        {include/i-rpclo.i}
                      END.
                      next.
                  END.
                end.
                else do:
                  if  funcionario.dat_desligto_func <> ?                                                           and
                     (funcionario.dat_desligto_func <= (date((tt-param.i-mes-ref + 1),01,tt-param.i-ano-ref) - 1)) and
                      tt-param.l-emite-demi = no then do:
                      IF tt-param.v_log_enviar_email THEN DO:
                        {include/i-rpclo.i}
                      END.
                      next.
                  END.
                end.

                if not v_log_folha_educnal then do:
                    run pi_histor_sal_func (input funcionario.cdn_estab,
                                            input funcionario.cdn_funcionario,
                                            input 1, /*historico de Cargo*/
                                            output v_cdn_cargo_basic,
                                            output v_cdn_niv_cargo,
                                            output v_val_sal_mensal,
                                            output v_val_sal_hora,
                                            output v_val_sal_cat,
                                            output v_cdn_tab_sal,
                                            output v_num_fx_sal,
                                            output v_num_niv_sal,
                                            output v_cdn_cargo_basic_funcao,
                                            output v_cdn_niv_cargo_funcao,
                                            output v_val_sal_mes_funcao,
                                            output v_val_sal_hor_funcao,
                                            output v_val_sal_cat_funcao,
                                            output v_cdn_tab_sal_funcao,
                                            output v_num_fx_sal_funcao,
                                            output v_num_niv_sal_funcao).
                   /***** n∆o substituir o find abaixo pela chamada da procedure pi_histor_sal_func *******/
                   find last histor_sal_func no-lock where
                           histor_sal_func.cdn_empresa          = funcionario.cdn_empresa     and
                           histor_sal_func.cdn_estab            = funcionario.cdn_estab       and
                           histor_sal_func.cdn_funcionario      = funcionario.cdn_funcionario and
                           histor_sal_func.idi_tip_cargo_funcao = 1 /* cargo */               and
                           histor_sal_func.dat_liber_sal       <= tt-param.v_dat_valid no-error. 

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
                          run pi_histor_sal_func (input funcionario.cdn_estab,
                                                  input funcionario.cdn_funcionario,
                                                  input 1, /*historico de Cargo*/
                                                  output v_cdn_cargo_basic,
                                                  output v_cdn_niv_cargo,
                                                  output v_val_sal_mensal,
                                                  output v_val_sal_hora,
                                                  output v_val_sal_cat,
                                                  output v_cdn_tab_sal,
                                                  output v_num_fx_sal,
                                                  output v_num_niv_sal,
                                                  output v_cdn_cargo_basic_funcao,
                                                  output v_cdn_niv_cargo_funcao,
                                                  output v_val_sal_mes_funcao,
                                                  output v_val_sal_hor_funcao,
                                                  output v_val_sal_cat_funcao,
                                                  output v_cdn_tab_sal_funcao,
                                                  output v_num_fx_sal_funcao,
                                                  output v_num_niv_sal_funcao).
                          /***** n∆o substituir o find abaixo pela chamada da procedure pi_histor_sal_func *******/
                          find last histor_sal_func no-lock where
                                    histor_sal_func.cdn_empresa          = bfuncionario.cdn_empresa     and
                                    histor_sal_func.cdn_estab            = bfuncionario.cdn_estab       and
                                    histor_sal_func.cdn_funcionario      = bfuncionario.cdn_funcionario and
                                    histor_sal_func.idi_tip_cargo_funcao = 1 /* cargo */               and
                                    histor_sal_func.dat_liber_sal       <= tt-param.v_dat_valid no-error. 
                          /*assign v_log_sem_salario = yes.                                            */
                      end.
                      else do:
                          run pi_histor_sal_func (input funcionario.cdn_estab,
                                                  input funcionario.cdn_funcionario,
                                                  input 1, /*historico de Cargo*/
                                                  output v_cdn_cargo_basic,
                                                  output v_cdn_niv_cargo,
                                                  output v_val_sal_mensal,
                                                  output v_val_sal_hora,
                                                  output v_val_sal_cat,
                                                  output v_cdn_tab_sal,
                                                  output v_num_fx_sal,
                                                  output v_num_niv_sal,
                                                  output v_cdn_cargo_basic_funcao,
                                                  output v_cdn_niv_cargo_funcao,
                                                  output v_val_sal_mes_funcao,
                                                  output v_val_sal_hor_funcao,
                                                  output v_val_sal_cat_funcao,
                                                  output v_cdn_tab_sal_funcao,
                                                  output v_num_fx_sal_funcao,
                                                  output v_num_niv_sal_funcao).
                          /***** n∆o substituir o find abaixo pela chamada da procedure pi_histor_sal_func *******/
                          find last histor_sal_func no-lock where
                                    histor_sal_func.cdn_empresa          = bfuncionario.cdn_empresa     and
                                    histor_sal_func.cdn_estab            = bfuncionario.cdn_estab       and
                                    histor_sal_func.cdn_funcionario      = bfuncionario.cdn_funcionario and
                                    histor_sal_func.idi_tip_cargo_funcao = 1 /* cargo */                and
                                    histor_sal_func.dat_liber_sal       <= tt-param.v_dat_valid no-error.                               
                      end.
                   end.
                end.

                /*if avail histor_sal_func then           */
                assign v_val_sal_mensal  = histor_sal_func.val_salario_mensal 
                       v_val_sal_hora    = histor_sal_func.val_salario_hora.

               run pi_func_turno_trab (input funcionario.cdn_estab,
                                input funcionario.cdn_funcionario,
                                output v_cdn_turno_trab,
                                output v_cdn_turma_trab).

               run pi_func_categ_sal (input funcionario.cdn_estab,
                               input funcionario.cdn_funcionario,
                               output v_cdn_categ_sal).

                assign l-tem-movto      = no
                       d-base-iapas     = 0
                       d-base-fgts      = 0
                       d-valor-fgts     = 0
                       d-base-irrf      = 0
                       d-liquido-pagar  = 0
                       d-total-vencto   = 0
                       d-total-descto   = 0
                       i-cont-linha     = 0
                       d-horas-semanais = 0
                       d-salar-atual    = histor_sal_func.val_salario_categ.

                for each movto_calcul_func of funcionario no-lock where
                         movto_calcul_func.num_ano_refer_fp  = tt-param.i-ano-ref and
                         movto_calcul_func.num_mes_refer_fp  = tt-param.i-mes-ref and
                         movto_calcul_func.idi_tip_fp = tt-param.i-tipo-folha and
                         movto_calcul_func.qti_parc_habilit_calc_fp    = tt-param.i-parcela:
                    if movto_calcul_func.num_seq_movto_calcul_fp = 0 then do:
                        if not v_log_folha_educnal then 
                           find last histor_sal_func of funcionario where
                                histor_sal_func.dat_liber_sal <= movto_calcul_func.dat_term_parc_calcula no-lock no-error.
                        else do:
                           if v_log_sem_salario then 
                              find last histor_sal_func of funcionario where
                                        histor_sal_func.idi_tip_cargo_funcao = 1 /* cargo */ and
                                        histor_sal_func.dat_liber_sal <= movto_calcul_func.dat_term_parc_calcula no-lock no-error.
                           else 
                              find last histor_sal_func of bfuncionario where
                                        histor_sal_func.idi_tip_cargo_funcao = 1 /* cargo */ and
                                        histor_sal_func.dat_liber_sal <= movto_calcul_func.dat_term_parc_calcula no-lock no-error.
                        end.
                        if available histor_sal_func then
                           assign d-salar-atual = histor_sal_func.val_salario_categ.
                        if movto_calcul_func.num_ano_refer_fp <> param_empres_rh.num_ano_refer_calc_efetd or
                           movto_calcul_func.num_mes_refer_fp <> param_empres_rh.num_mes_refer_calc_efetd then do:
                           find turno_trab no-lock where
                                turno_trab.cdn_turno_trab = v_cdn_turno_trab no-error.
                           if available turno_trab and not v_log_folha_educnal then 
                              assign d-salar-atual = if funcionario.cdn_categ_sal = 2
                                                     then movto_calcul_func.val_salario_hora
                                                     else d-salar-atual.
    
                        end.
                    end.
                    assign i-inx       = 0
                           l-tem-movto = yes.
                    repeat:
                       assign i-inx = i-inx + 1.
                       if  i-inx > movto_calcul_func.qti_efp then
                          leave.
                       if movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 20 and
                          movto_calcul_func.val_calcul_efp[i-inx] = 0 then
                          next.
                       if movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 56 and
                          movto_calcul_func.val_calcul_efp[i-inx] = 0 then
                          next.
                       find event_fp no-lock where
                            event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                            event_fp.cdn_event_fp = movto_calcul_func.cdn_event_fp[i-inx] no-error.
                       if not avail event_fp then next.
                       if movto_calcul_func.cdn_event_fp[i-inx] > "900" and
                          not event_fp.log_impr_envel_fp then
                          next.
                       if  movto_calcul_func.val_calcul_efp[i-inx] = 0 and
                           event_fp.idi_ident_efp <> 3 then
                           next.   
                       if  movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 52 then do:
                          assign d-liquido-pagar = movto_calcul_func.val_calcul_efp[i-inx].
                          next.
                       end.
                       if movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 18 and
                          d-base-iapas              = 0  then
                          assign d-base-iapas = movto_calcul_func.val_base_calc_fp[i-inx].
                       if movto_calcul_func.idi_tip_fp = 3 and
                          movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 9 then
                          assign d-base-iapas = movto_calcul_func.val_base_calc_fp[i-inx].
                       if movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 20 or
                          movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 56 OR
                          movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 138 then do:
                          assign d-base-fgts  = d-base-fgts + movto_calcul_func.val_base_calc_fp[i-inx]
                                 d-valor-fgts = d-valor-fgts + movto_calcul_func.val_calcul_efp[i-inx].
                          if not event_fp.log_impr_envel_fp then       
                             next.
                       end.
                       if movto_calcul_func.idi_tip_fp = 3 and
                          movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 14 then
                          assign d-base-irrf = movto_calcul_func.val_base_calc_fp[i-inx].
                       if movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 12 then
                           assign d-base-irrf = movto_calcul_func.val_base_calc_fp[i-inx].
                       if  movto_calcul_func.val_calcul_efp[i-inx] = 0 and
                           event_fp.idi_ident_efp <> 1 then
                          next.
                       if  not event_fp.log_impr_envel_fp then
                          next.
                       create w-mvtocalc.
                       assign w-mvtocalc.fc-codigo   = movto_calcul_func.cdn_funcionario
                              w-mvtocalc.ev-codigo   = event_fp.cdn_event_fp
                              w-mvtocalc.descricao   = event_fp.des_event_fp
                              w-mvtocalc.identif     = event_fp.idi_ident_efp 
                              w-mvtocalc.unidades    = movto_calcul_func.qtd_unid_event_fp[i-inx]
                              w-mvtocalc.horas       = movto_calcul_func.qtd_hrs_demonst_efp[i-inx]
                              w-mvtocalc.base        = movto_calcul_func.val_base_calc_fp[i-inx]
                              w-mvtocalc.salar-hora  =
                                            if  movto_calcul_func.qtd_hrs_demonst_efp[i-inx] > 0
                                            then truncate(movto_calcul_func.val_calcul_efp[i-inx] /      
                                                         movto_calcul_func.qtd_hrs_demonst_efp[i-inx],4)
                                            else 0
                              w-mvtocalc.salar-hora  =
                                            if w-mvtocalc.salar-hora > 999.9999
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
                if  not l-tem-movto THEN DO:
                    IF tt-param.v_log_enviar_email THEN DO:
                      {include/i-rpclo.i}
                    END.
                    next.
                END.

                assign dt-ini-per = date(tt-param.i-mes-ref,01,tt-param.i-ano-ref).
                
                run prghur/fpp/fpapi002.p (input tt-param.i-mes-ref,
                                           input tt-param.i-ano-ref,
                                           input-output dt-fim-per).
        
               /*Se possuir afastamento para o màs e ano de referància, n∆o possuir l°quido a receber e 
                 o campo "Emite Afastado" n∆o estiver selecionado, n∆o imprimir o envelope*/
                if d-liquido-pagar = 0 then do:
                    if  can-find(first sit_afast_func of funcionario where
                                 sit_afast_func.dat_inic_sit_afast <= dt-ini-per and
                                 sit_afast_func.dat_term_sit_afast >= dt-fim-per) and
                                 not tt-param.l-emite-afast then do:
                        if  tt-param.l-emite-ferias then do:
                            find first sit_afast_func of funcionario where
                                       sit_afast_func.dat_inic_sit_afast <= dt-ini-per and
                                       sit_afast_func.dat_term_sit_afast >= dt-fim-per no-lock no-error.
                            find first sit_afast of sit_afast_func no-lock where
                                       sit_afast.idi_signif_sit = 5 no-error.
                            if  not avail sit_afast then do:
                                for each w-mvtocalc:
                                    delete w-mvtocalc.
                                end.
                                next.
                            end.
                        end.
                        else do:
                            for each w-mvtocalc:
                                delete w-mvtocalc.
                            end.
                            next.
                        end.
                    end.
                end.

                find habilit_calc_fp use-index hbltclcf_py04002 no-lock where
                     habilit_calc_fp.cdn_empresa  = funcionario.cdn_empresa and
                     habilit_calc_fp.cdn_estab  = funcionario.cdn_estab and
                     habilit_calc_fp.cdn_categ_sal  = func_categ_sal.cdn_categ_sal and
                     &if "{&dthrpyc_version}" >= "2.06" &then
                        habilit_calc_fp.idi_orig_contratac_func  = funcionario.idi_orig_contratac_func and
                        habilit_calc_fp.cdn_prestdor_serv        = funcionario.cdn_prestdor_serv       and
                     &endif
                     habilit_calc_fp.idi_tip_fp = tt-param.i-tipo-folha  and
                     habilit_calc_fp.num_ano_refer_fp_calcula  = tt-param.i-ano-ref     and
                     habilit_calc_fp.num_mes_refer_fp_calcula  = tt-param.i-mes-ref     and
                     habilit_calc_fp.qti_parc_habilit_calc_fp    = tt-param.i-parcela
                     no-error.
                if  available habilit_calc_fp then
                   assign c-mens-contr[1]=substring(habilit_calc_fp.des_msg_envel_fp,1,40)
                          c-mens-contr[2]=substring(habilit_calc_fp.des_msg_envel_fp,41,38).
                find cargo_basic no-lock where
                     cargo_basic.cdn_cargo_basic = v_cdn_cargo_basic no-error.
                assign i-index     =  func_categ_sal.cdn_categ_sal 
                       c-categoria = {database/inpy/i03py029.i 04 func_categ_sal.cdn_categ_sal}
                       c-mes-folha = c-lit-mes[tt-param.i-mes-ref]
                       c-parabens  =
                              if  tt-param.i-tipo-folha = 1 and
                                  (month(rh_pessoa_fisic.dat_nascimento) = (tt-param.i-mes-ref + 1) or
                                   (month(rh_pessoa_fisic.dat_nascimento) = 1 and
                                    tt-param.i-mes-ref = 12))
                              then "* * *  F e l i z   A n i v e r s a r i o  * * *"
                              else "".
                if  funcionario.dat_desligto_func <> ? and
                    (year(funcionario.dat_desligto_func) < tt-param.i-ano-ref or
                     (year(funcionario.dat_desligto_func) = tt-param.i-ano-ref and
                     month(funcionario.dat_desligto_func) <= tt-param.i-mes-ref)) then
                   assign c-parabens =    "*******************************"
                          c-mensagem[1] = "* * *   D e m i t i d o   * * *"
                          c-mensagem[2] = "*******************************".
                else
                   assign c-mensagem[1] = c-mens-contr[1]
                          c-mensagem[2] = c-mens-contr[2].

                assign c-nome-compl = funcionario.nom_pessoa_fisic.

                if  funcionario.idi_forma_pagto = 1 then do:
                   assign i-cta-corrente = funcionario.cdn_cta_corren
                          c-hifen        = "-"
                          c-dig-conta    = funcionario.cod_digito_cta_corren.
                   find rh_bco no-lock where
                        rh_bco.cdn_banco = funcionario.cdn_bco_liq no-error.
                   find rh_agenc_bcia of rh_bco no-lock where
                        rh_agenc_bcia.cdn_agenc_bcia = funcionario.cdn_agenc_bcia_liq no-error.
                   assign l-dep-cru = if substr(rh_agenc_bcia.des_cargo_pessoa_contat[3],26,3) = "urv"
                                      then no
                                      else yes.
                end.
                else
                   assign i-cta-corrente = 0
                          c-hifen        = ""
                          c-dig-conta    = ""
                          l-dep-cru      = yes.

                assign i-inx = 1.
                repeat:
                   if tab_irf_inss.val_lim_faixa_irf_inss[i-inx] = 0 then do:
                      assign i-inx = i-inx - 1.
                      leave.
                   end.
                   assign i-inx = i-inx + 1.
                end.

                assign d-limite-inss = tab_irf_inss.val_lim_faixa_irf_inss[i-inx].
                if d-base-iapas > d-limite-inss then
                   assign d-base-iapas = d-limite-inss.

                if v_log_folha_educnal then
                   assign i-impr-func = string(funcionario.cdn_func_centrdor, "zzzzz9") + c-barra + 
                                        string(funcionario.cdn_tip_contrat_func, "99")  + c-hifem +
                                        string(funcionario.num_digito_verfdor_func, "9").
                else 
                   assign i-impr-func = string(funcionario.cdn_funcionario, "zzzzzzz9") + c-hifem +
                                        string(funcionario.num_digito_verfdor_func, "9").
                for each w-mvtocalc break by w-mvtocalc.fc-codigo
                                          by w-mvtocalc.identif
                                          by w-mvtocalc.ev-codigo:
                    if  first-of(w-mvtocalc.fc-codigo) or
                        i-cont-linha > 22 then do:
                       if  i-cont-linha > 22 then do:
                          do while i-cont-linha < 26:
                             put "" at 01 skip.
                             assign i-cont-linha = i-cont-linha + 1.
                          end.
                          {utp/ut-liter.i Continua... *}
                          put  Return-value format "x(11)" at 58  skip(07).
                       end. 
                       assign l-imprime = yes.
                       put c-empresa                           AT  02
                           c-mes-folha                         at  46
                           "/"                                 at  55
                           tt-param.i-ano-ref                  at  56
                           c-categoria                         at  61 skip(1)
                           i-impr-func                         at  01
                           funcionario.nom_pessoa_fisic        at  13    skip
                           i-cta-corrente                      at  08
                           c-hifen                             at  17
                           c-dig-conta                         at  18
                           cargo_basic.cod_classif_ocupac      at  36
                           empresa.ep-codigo                   at  43.
                           if funcionario.cdn_local_pagto <> 0 THEN
                              PUT funcionario.cdn_local_pagto      at  47.
                           else 
                              PUT funcionario.cdn_estab            at  47.
                       PUT funcionario.cod_unid_lotac          at  53 skip(2).
                       assign i-cont-linha = 7.
                    end.

                    put w-mvtocalc.ev-codigo   at  01
                        w-mvtocalc.descricao   at  05.
                    if  w-mvtocalc.horas > 0 then
                       put w-mvtocalc.horas    at 35.
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

                    if  w-mvtocalc.inc-liquido = 2 then
                       put w-mvtocalc.valor       at 56
                           c-inc-liquido at 69.
                    else
                       put w-mvtocalc.valor       at  42
                           c-inc-liquido at  55.

                    assign i-cont-linha = i-cont-linha + 1.
                    if  w-mvtocalc.inc-liquido = 1 then
                       assign d-total-vencto = d-total-vencto + w-mvtocalc.valor.
                    else
                       if  w-mvtocalc.inc-liquido = 2 then
                          d-total-descto = d-total-descto + w-mvtocalc.valor.
                       else.
                    if  last-of(w-mvtocalc.fc-codigo) then do:
                       IF v_log_folha_educnal and
                          tt-param.l-imp-carga-horar-sem THEN DO:
                           
                           FOR EACH bfunciona NO-LOCK
                              WHERE bfunciona.cdn_empresa       = funcionario.cdn_empresa
                                AND bfunciona.cdn_estab         = funcionario.cdn_estab
                                AND bfunciona.cdn_func_centrdor = funcionario.cdn_func_centrdor
                                AND bfunciona.cdn_tip_contrat_func > 0:
                               FIND FIRST tip_contrat NO-LOCK
                                    WHERE tip_contrat.cdn_tip_contrat_func = bfunciona.cdn_tip_contrat_func NO-ERROR.
                               IF AVAIL tip_contrat THEN DO:
                                   IF tip_contrat.log_categ_contrat_educnal THEN DO:
                                       FOR EACH apont_hora_aula no-lock of bfunciona
                                          WHERE (MONTH(apont_hora_aula.dat_inic_valid)  = month(tt-param.v_dat_valid) /*mes_aux */
                                            AND YEAR(apont_hora_aula.dat_inic_valid)    = year(tt-param.v_dat_valid)) /*ano_aux */
                                             OR  (apont_hora_aula.dat_inic_valid       <= tt-param.v_dat_valid        /*ultimo dia DO mes aux*/
                                             AND  apont_hora_aula.dat_fim_valid        >= tt-param.v_dat_valid)       /*ultimo dia DO mes aux*/  :
                                           IF param_folha_educnal.num_livre_1 = 1 /*Mensal*/ THEN
                                               ASSIGN d-horas-semanais = d-horas-semanais + (apont_hora_aula.qtd_hrs_trab_semanal / 5).
                                           ELSE
                                               ASSIGN d-horas-semanais = d-horas-semanais + apont_hora_aula.qtd_hrs_trab_semanal.
                                       END.
                                   END.
                               END.
                           END.
    
                           put SKIP(1)
                               c-carga-horaria  AT 05
                               d-horas-semanais AT 47 SKIP.
    
                           do  while i-cont-linha < 22:
                              put "" at 01 skip.
                              assign i-cont-linha = i-cont-linha + 1.
                           end.
                       END.
                       ELSE DO:
                           do  while i-cont-linha < 24:
                              put "" at 01 skip.
                              assign i-cont-linha = i-cont-linha + 1.
                           end.
                       END.

                       put c-parabens            at 01
                           d-total-vencto        at 41
                           d-total-descto        at 56 "-" skip
                           c-mensagem[1]         at 01 skip
                           c-mensagem[2]         at 01
                           d-liquido-pagar       at 55 skip(1)
                           d-salar-atual         at 01 format ">>>>>,>>9.99"
                           d-base-iapas          at 14
                           d-base-fgts           at 28
                           d-valor-fgts          at 42
                           d-base-irrf           at 55 skip(5).
                    end.
                    delete w-mvtocalc.
                end.
                IF tt-param.v_log_enviar_email THEN DO:
                    {include/i-rpclo.i}
                    FIND rh_pessoa_fisic NO-LOCK WHERE
                         rh_pessoa_fisic.num_pessoa_fisic = funcionario.num_pessoa_fisic NO-ERROR.
                    IF AVAIL rh_pessoa_fisic THEN
                        ASSIGN c_destino = rh_pessoa_fisic.nom_e_mail
                               c_senha   = rh_pessoa_fisic.cod_id_feder.
                    ELSE
                        ASSIGN c_destino = "":U
                               c_senha   = "":U.
        
                    assign c_anexo = session:TEMP-DIRECTORY + "extrato.zip":U
                           l-compactar = YES. /*YES.*/
                    /* O programa envia e-mail, mas tem problema na senha, por causa do cod_senha da PF, que Ç gravado com encode.
                       se o l-compactar estiver com o valor no entao nao coloca senha no arquivo */
        
                    {prghur/fpp/fp3500.i}
    
                    IF l-ErroZip THEN DO:
                        FIND FIRST tt-erros-zip NO-ERROR.
                        IF AVAIL tt-erros-zip THEN
                            ASSIGN c_msg_erro = tt-erros-zip.desc-erro.
                        ELSE
                            ASSIGN c_msg_erro = "":U.
                    END.
                    ELSE DO:
                        FIND FIRST tt-erros NO-ERROR.
                        IF AVAIL tt-erros THEN
                            ASSIGN c_msg_erro = tt-erros.desc-erro.
                        ELSE
                            ASSIGN c_msg_erro = "":U.
                    END.
                    
                    CREATE tt-rel-erros.
                    ASSIGN tt-rel-erros.cdn_empresa      = funcionario.cdn_empresa
                           tt-rel-erros.cdn_estab        = funcionario.cdn_estab
                           tt-rel-erros.cdn_funcionario  = funcionario.cdn_funcionario
                           tt-rel-erros.nom_pessoa_fisic = funcionario.nom_pessoa_fisic
                           tt-rel-erros.status_email     = IF RETURN-VALUE = "NOK" THEN v_status_nao_enviado ELSE v_status_enviado
                           tt-rel-erros.email            = IF RETURN-VALUE = "OK" THEN c_destino ELSE c_msg_erro.
                    
                END.
       END.
    end.
    else do:
        for each tt-digita:
            if v_log_folha_educnal 
                then assign i-matr-ini = {prghur/dop/eng005.i &VAR="(tt-digita.v_cdn_func_centr * 100)"}
                            i-matr-fim = {prghur/dop/eng005.i &VAR="(tt-digita.v_cdn_func_centr * 100)"}.
                else assign i-matr-ini = tt-digita.v_cdn_funcionario
                            i-matr-fim = tt-digita.v_cdn_funcionario.
            if tt-param.v_log_origem = no then 
               assign tt-param.i-es-ini = tt-digita.v_cdn_estab
                      tt-param.i-es-fim = tt-digita.v_cdn_estab
                      tt-param.i-fc-ini = i-matr-ini
                      tt-param.i-fc-fim = i-matr-fim.
            else
               if v_log_folha_educnal then
                  assign i-matr-ini = {prghur/dop/eng005.i &VAR="(tt-param.i-fc-ini * 100)"}
                         i-matr-fim = {prghur/dop/eng005.i &VAR="(tt-param.i-fc-fim * 100)"}.
               else 
                  assign i-matr-ini = tt-param.i-fc-ini 
                         i-matr-fim = tt-param.i-fc-fim.
            for each rh_estab  no-lock where     
                     rh_estab.cdn_empresa  = param_empres_rh.cdn_empresa and
                     rh_estab.cdn_estab >= tt-param.i-es-ini and
                     rh_estab.cdn_estab <= tt-param.i-es-fim:
                for each func_ccusto no-lock where
                         func_ccusto.cdn_empresa          = rh_estab.cdn_empresa and
                         func_ccusto.cdn_estab            = rh_estab.cdn_estab and
                         func_ccusto.cdn_funcionario     >= i-matr-ini and
                         func_ccusto.cdn_funcionario     <= i-matr-fim and
                         func_ccusto.dat_inic_lotac_func <= tt-param.v_dat_valid and
                         func_ccusto.dat_fim_lotac_func  >= tt-param.v_dat_valid and
                         func_ccusto.cod_rh_ccusto       >= tt-param.i-cc-codigo-1 and
                         func_ccusto.cod_rh_ccusto       <= tt-param.i-cc-codigo-2,
                    each func_turno_trab no-lock where
                         func_turno_trab.cdn_empresa                     = func_ccusto.cdn_empresa and
                         func_turno_trab.cdn_estab                       = func_ccusto.cdn_estab and
                         func_turno_trab.cdn_funcionario                 = func_ccusto.cdn_funcionario and
                         func_turno_trab.dat_inic_lotac_func_turno_trab <= tt-param.v_dat_valid and
                         func_turno_trab.dat_term_lotac_func            >= tt-param.v_dat_valid ,
                    each func_categ_sal no-lock where
                         func_categ_sal.cdn_empresa          = func_turno_trab.cdn_empresa and
                         func_categ_sal.cdn_estab            = func_turno_trab.cdn_estab and
                         func_categ_sal.cdn_funcionario      = func_turno_trab.cdn_funcionario and
                         func_categ_sal.dat_inic_lotac_func <= tt-param.v_dat_valid and
                         func_categ_sal.dat_fim_lotac_func  >= tt-param.v_dat_valid and
                       ((func_categ_sal.cdn_categ_sal     = 1 and tt-param.l-mensal    = yes)  or
                        (func_categ_sal.cdn_categ_sal     = 2 and tt-param.l-horista   = yes)  or
                        (func_categ_sal.cdn_categ_sal     = 3 and tt-param.l-semanal   = yes)  or
                        (func_categ_sal.cdn_categ_sal     = 4 and tt-param.l-quinzenal = yes)  or
                        (func_categ_sal.cdn_categ_sal     = 5 and tt-param.l-tarefa    = yes)  or
                        (func_categ_sal.cdn_categ_sal     = 6 and tt-param.l-diarista  = yes)),
                    each funcionario use-index fncnr_py08504 of func_categ_sal no-lock where
                         funcionario.nom_pessoa_fisic >= tt-param.v_nom_func_ini          and
                         funcionario.nom_pessoa_fisic <= tt-param.v_nom_func_fim          and
                         funcionario.cod_unid_lotac   >= tt-param.v_cod_unid_lotac_ini    and 
                         funcionario.cod_unid_lotac   <= tt-param.v_cod_unid_lotac_fim    and                         
                         funcionario.cdn_local_pagto  >= tt-param.cdn_local_pagto_ini     and
                         funcionario.cdn_local_pagto  <= tt-param.cdn_local_pagto_fim     and  
                       ((tt-param.r-forma-pgto        = 4                                 and
                         funcionario.idi_forma_pagto <> 0)                               or
                         funcionario.idi_forma_pagto = tt-param.r-forma-pgto )
                    break by if i-ord-aux = 1 or 
                                i-ord-aux = 2 
                             then funcionario.idi_forma_pagto
                             else 0
                          by if i-ord-aux <> 15 and
                                i-ord-aux <> 16 
                             then funcionario.cdn_estab
                             else {prghur/dop/eng012.i}
                          by if i-ord-aux = 99
                             then funcionario.idi_orig_contratac_func
                             else 0
                          by if i-ord-aux = 99
                             then funcionario.cdn_prestdor_serv
                             else 0                                 
                          by if i-ord-aux = 9 or 
                                i-ord-aux = 10 
                             then func_turno_trab.cdn_turno_trab
                             else 0
                          by if i-ord-aux = 11 or 
                                i-ord-aux = 12 or
                                i-ord-aux = 17 or
                                i-ord-aux = 18 then
                                func_ccusto.cod_rh_ccusto
                             else ""
                          by if  i-ord-aux = 13 or
                                 i-ord-aux = 14 
                             then funcionario.cdn_bco_liq 
                             else 0
                          by if  i-ord-aux = 13 or 
                                 i-ord-aux = 14
                             then funcionario.cdn_agenc_bcia_liq
                             else 0 
                          by if i-ord-aux = 15 or
                                i-ord-aux = 16
                             then funcionario.cdn_local_pagto
                             else 0
                          by if i-ord-aux = 17 or 
                                i-ord-aux = 18 then
                                func_turno_trab.cdn_turno_trab
                             else 0
                          by if  i-ord-aux = 1  or
                                 i-ord-aux = 5  or
                                 i-ord-aux = 7  or
                                 i-ord-aux = 9  or
                                 i-ord-aux = 11 or
                                 i-ord-aux = 13 or
                                 i-ord-aux = 15 or
                                 i-ord-aux = 17 or 
                                 tt-param.num_classif_terc = 1 then
                                 string(funcionario.cdn_funcionario,"99999999")
                              else
                                 funcionario.nom_pessoa_fisic
                           by funcionario.idi_orig_contratac_func
                           by funcionario.cdn_prestdor_serv :

                        IF tt-param.v_log_enviar_email THEN DO:
                            {include/i-rpout.i &pagesize=0}
                        END.

                        if first-of(funcionario.cdn_prestdor_serv) and
                          tt-param.num_razao_social = 2 then do:
                          find prestdor_serv no-lock where
                               prestdor_serv.cdn_empresa = funcionario.cdn_empresa and
                               prestdor_serv.cdn_prestdor_serv = funcionario.cdn_prestdor_serv no-error.
                          if avail prestdor_serv then
                             assign c-empresa = string(funcionario.cdn_prestdor_serv, "zzzzz9") + " - " +
                                                prestdor_serv.nom_pessoa.
                          else do:
                             find empresa no-lock where
                                  empresa.ep-codigo = tt-param.v_cdn_empres_usuar no-error.
                             assign c-empresa = string(funcionario.cdn_empresa, "zz9") + " - " +
                                                empresa.razao-social.

                          end.
                          page {&STREAM}.                      
                       end.

                        if v_log_folha_educnal then do:
                          if funcionario.cdn_tip_contrat_func <> 0 THEN DO:
                              IF tt-param.v_log_enviar_email THEN DO:
                                {include/i-rpclo.i}
                              END.
                              next.
                          END.

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

                             end.
                          end.
                        end.

                        assign i-contador = i-contador + 1.                  
                        run pi-acompanhar in v_han_acomp (input i-contador).   

                        /******** Chamada EPC - Santa Casa **********/
                        RUN pi-valida-retencao-ir.
                        IF RETURN-VALUE = "NOK" THEN NEXT.
                        /******** Fim Chamada EPC - Santa Casa **********/
                        if funcionario.idi_forma_pagto       <> 3 then do:
                           if funcionario.cdn_bco_liq         < tt-param.i-bc-codigo-1  or
                              funcionario.cdn_bco_liq         > tt-param.i-bc-codigo-2  or
                              funcionario.cdn_agenc_bcia_liq  < tt-param.i-ag-codigo-1  or
                              funcionario.cdn_agenc_bcia_liq  > tt-param.i-ag-codigo-2  then do:
                               IF tt-param.v_log_enviar_email THEN DO:
                                 {include/i-rpclo.i}
                               END.
                               next.
                           END.
                        end.              

                        if tt-param.i-mes-ref = 12 then do:
                          if  funcionario.dat_desligto_func <> ?                                           and
                             (funcionario.dat_desligto_func <= (date(01,01,(tt-param.i-ano-ref + 1)) - 1)) and
                              tt-param.l-emite-demi          = no then do:
                              IF tt-param.v_log_enviar_email THEN DO:
                                {include/i-rpclo.i}
                              END.
                              next.
                          END.
                        end.
                        else do:
                          if  funcionario.dat_desligto_func <> ?                                                           and
                             (funcionario.dat_desligto_func <= (date((tt-param.i-mes-ref + 1),01,tt-param.i-ano-ref) - 1)) and
                             tt-param.l-emite-demi = no then do:
                              IF tt-param.v_log_enviar_email THEN DO:
                                {include/i-rpclo.i}
                              END.
                              next.
                          END.
                        end.


                        if not v_log_folha_educnal then do:
                            run pi_histor_sal_func (input funcionario.cdn_estab,
                                                    input funcionario.cdn_funcionario,
                                                    input 1, /*historico de Cargo*/
                                                    output v_cdn_cargo_basic,
                                                    output v_cdn_niv_cargo,
                                                    output v_val_sal_mensal,
                                                    output v_val_sal_hora,
                                                    output v_val_sal_cat,
                                                    output v_cdn_tab_sal,
                                                    output v_num_fx_sal,
                                                    output v_num_niv_sal,
                                                    output v_cdn_cargo_basic_funcao,
                                                    output v_cdn_niv_cargo_funcao,
                                                    output v_val_sal_mes_funcao,
                                                    output v_val_sal_hor_funcao,
                                                    output v_val_sal_cat_funcao,
                                                    output v_cdn_tab_sal_funcao,
                                                    output v_num_fx_sal_funcao,
                                                    output v_num_niv_sal_funcao).
                           /***** n∆o substituir o find abaixo pela chamada da procedure pi_histor_sal_func *******/
                           find last histor_sal_func no-lock where
                                   histor_sal_func.cdn_empresa          = funcionario.cdn_empresa     and
                                   histor_sal_func.cdn_estab            = funcionario.cdn_estab       and
                                   histor_sal_func.cdn_funcionario      = funcionario.cdn_funcionario and
                                   histor_sal_func.idi_tip_cargo_funcao = 1 /* cargo */               and
                                   histor_sal_func.dat_liber_sal       <= tt-param.v_dat_valid no-error. 

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
                                  run pi_histor_sal_func (input funcionario.cdn_estab,
                                                          input funcionario.cdn_funcionario,
                                                          input 1, /*historico de Cargo*/
                                                          output v_cdn_cargo_basic,
                                                          output v_cdn_niv_cargo,
                                                          output v_val_sal_mensal,
                                                          output v_val_sal_hora,
                                                          output v_val_sal_cat,
                                                          output v_cdn_tab_sal,
                                                          output v_num_fx_sal,
                                                          output v_num_niv_sal,
                                                          output v_cdn_cargo_basic_funcao,
                                                          output v_cdn_niv_cargo_funcao,
                                                          output v_val_sal_mes_funcao,
                                                          output v_val_sal_hor_funcao,
                                                          output v_val_sal_cat_funcao,
                                                          output v_cdn_tab_sal_funcao,
                                                          output v_num_fx_sal_funcao,
                                                          output v_num_niv_sal_funcao).
                                  /***** n∆o substituir o find abaixo pela chamada da procedure pi_histor_sal_func *******/
                                  find last histor_sal_func no-lock where
                                            histor_sal_func.cdn_empresa          = bfuncionario.cdn_empresa     and
                                            histor_sal_func.cdn_estab            = bfuncionario.cdn_estab       and
                                            histor_sal_func.cdn_funcionario      = bfuncionario.cdn_funcionario and
                                            histor_sal_func.idi_tip_cargo_funcao = 1 /* cargo */               and
                                            histor_sal_func.dat_liber_sal       <= tt-param.v_dat_valid no-error. 
                                 /* assign v_log_sem_salario = yes.                                            */
                              end.
                              else do:
                                  run pi_histor_sal_func (input funcionario.cdn_estab,
                                                          input funcionario.cdn_funcionario,
                                                          input 1, /*historico de Cargo*/
                                                          output v_cdn_cargo_basic,
                                                          output v_cdn_niv_cargo,
                                                          output v_val_sal_mensal,
                                                          output v_val_sal_hora,
                                                          output v_val_sal_cat,
                                                          output v_cdn_tab_sal,
                                                          output v_num_fx_sal,
                                                          output v_num_niv_sal,
                                                          output v_cdn_cargo_basic_funcao,
                                                          output v_cdn_niv_cargo_funcao,
                                                          output v_val_sal_mes_funcao,
                                                          output v_val_sal_hor_funcao,
                                                          output v_val_sal_cat_funcao,
                                                          output v_cdn_tab_sal_funcao,
                                                          output v_num_fx_sal_funcao,
                                                          output v_num_niv_sal_funcao).
                                  /***** n∆o substituir o find abaixo pela chamada da procedure pi_histor_sal_func *******/
                                  find last histor_sal_func no-lock where
                                            histor_sal_func.cdn_empresa          = bfuncionario.cdn_empresa     and
                                            histor_sal_func.cdn_estab            = bfuncionario.cdn_estab       and
                                            histor_sal_func.cdn_funcionario      = bfuncionario.cdn_funcionario and
                                            histor_sal_func.idi_tip_cargo_funcao = 1 /* cargo */                and
                                            histor_sal_func.dat_liber_sal       <= tt-param.v_dat_valid no-error.                               
                              end.
                           end.

                        end.


                       /*if avail histor_sal_func then           */
                         assign v_val_sal_mensal  = histor_sal_func.val_salario_mensal 
                                v_val_sal_hora    = histor_sal_func.val_salario_hora.

                        assign l-tem-movto      = no
                               d-base-iapas     = 0
                               d-base-fgts      = 0
                               d-valor-fgts     = 0
                               d-base-irrf      = 0
                               d-liquido-pagar  = 0
                               d-total-vencto   = 0
                               d-total-descto   = 0
                               i-cont-linha     = 0
                               d-horas-semanais = 0
                               d-salar-atual    = histor_sal_func.val_salario_categ.

                        for each movto_calcul_func of funcionario no-lock where
                                 movto_calcul_func.num_ano_refer_fp  = tt-param.i-ano-ref and
                                 movto_calcul_func.num_mes_refer_fp  = tt-param.i-mes-ref and
                                 movto_calcul_func.idi_tip_fp = tt-param.i-tipo-folha and
                                 movto_calcul_func.qti_parc_habilit_calc_fp    = tt-param.i-parcela:

                            if not v_log_folha_educnal then 
                               find last histor_sal_func of funcionario where
                                    histor_sal_func.dat_liber_sal <= movto_calcul_func.dat_term_parc_calcula no-lock no-error.
                            else do:
                               if v_log_sem_salario then 
                                  find last histor_sal_func of funcionario where
                                            histor_sal_func.idi_tip_cargo_funcao = 1 /* cargo */ and
                                            histor_sal_func.dat_liber_sal <= movto_calcul_func.dat_term_parc_calcula no-lock no-error.
                               else 
                                  find last histor_sal_func of bfuncionario where
                                            histor_sal_func.idi_tip_cargo_funcao = 1 /* cargo */ and
                                            histor_sal_func.dat_liber_sal <= movto_calcul_func.dat_term_parc_calcula no-lock no-error.
                            end.
                            if available histor_sal_func then
                              assign d-salar-atual = histor_sal_func.val_salario_categ.

                            if movto_calcul_func.num_ano_refer_fp <> param_empres_rh.num_ano_refer_calc_efetd or
                               movto_calcul_func.num_mes_refer_fp <> param_empres_rh.num_mes_refer_calc_efetd then do:
                               find turno_trab no-lock where
                                    turno_trab.cdn_turno_trab = func_turno_trab.cdn_turno_trab no-error.
                               if available turno_trab and not v_log_folha_educnal then
                                  assign d-salar-atual = if func_categ_sal.cdn_categ_sal = 2
                                                         then movto_calcul_func.val_salario_hora
                                                         else d-salar-atual.
                            end.

                            assign i-inx       = 0
                                   l-tem-movto = yes.
                            repeat:
                               assign i-inx = i-inx + 1.
                               if  i-inx > movto_calcul_func.qti_efp then
                                  leave.
                               if movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 20 and
                                  movto_calcul_func.val_calcul_efp[i-inx] = 0 then
                                  next.
                               if movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 56 and
                                  movto_calcul_func.val_calcul_efp[i-inx] = 0 then
                                  next.
                               find event_fp no-lock where
                                    event_fp.cdn_empresa  = tt-param.v_cdn_empresa_evento and
                                    event_fp.cdn_event_fp = movto_calcul_func.cdn_event_fp[i-inx] no-error.
                               if not avail event_fp then next.
                               if movto_calcul_func.cdn_event_fp[i-inx] > "900" and
                                  not event_fp.log_impr_envel_fp then
                                  next.
                               if  movto_calcul_func.val_calcul_efp[i-inx] = 0 and
                                   event_fp.idi_ident_efp <> 3 then
                                   next.      
                               if movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 52 then do:
                                  assign d-liquido-pagar = movto_calcul_func.val_calcul_efp[i-inx].
                                  next.
                               end.
                               if movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 18 and
                                  d-base-iapas              = 0  then
                                  assign d-base-iapas = movto_calcul_func.val_base_calc_fp[i-inx].
                               if movto_calcul_func.idi_tip_fp = 3 and
                                  movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 9 then
                                  assign d-base-iapas = movto_calcul_func.val_base_calc_fp[i-inx].
                               if movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 20  or
                                  movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 56  or
                                  movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 138 then do:
                                  assign d-base-fgts  = d-base-fgts + movto_calcul_func.val_base_calc_fp[i-inx]
                                         d-valor-fgts = d-valor-fgts + movto_calcul_func.val_calcul_efp[i-inx].
                                  if not event_fp.log_impr_envel_fp then       
                                     next.
                               end.
                               if movto_calcul_func.idi_tip_fp = 3 and
                                  movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 14 then
                                  assign d-base-irrf = movto_calcul_func.val_base_calc_fp[i-inx].
                               if movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 12 then
                                   assign d-base-irrf = movto_calcul_func.val_base_calc_fp[i-inx].
                               if  movto_calcul_func.val_calcul_efp[i-inx] = 0 and
                                   event_fp.idi_ident_efp <> 1 then
                                  next.
                               if  not event_fp.log_impr_envel_fp then
                                  next.

                               create w-mvtocalc.
                               assign w-mvtocalc.fc-codigo   = movto_calcul_func.cdn_funcionario
                                      w-mvtocalc.ev-codigo   = event_fp.cdn_event_fp
                                      w-mvtocalc.descricao   = event_fp.des_event_fp
                                      w-mvtocalc.identif     = event_fp.idi_ident_efp 
                                      w-mvtocalc.unidades    = movto_calcul_func.qtd_unid_event_fp[i-inx]
                                      w-mvtocalc.horas       = movto_calcul_func.qtd_hrs_demonst_efp[i-inx]
                                      w-mvtocalc.base        = movto_calcul_func.val_base_calc_fp[i-inx]
                                      w-mvtocalc.salar-hora  =
                                                    if  movto_calcul_func.qtd_hrs_demonst_efp[i-inx] > 0
                                                    then truncate(movto_calcul_func.val_calcul_efp[i-inx] /
                                                                 movto_calcul_func.qtd_hrs_demonst_efp[i-inx],4)
                                                    else 0
                                      w-mvtocalc.salar-hora  =
                                                    if w-mvtocalc.salar-hora > 999.9999
                                                    then 999.9999
                                                    else w-mvtocalc.salar-hora
                                      w-mvtocalc.valor       = movto_calcul_func.val_calcul_efp[i-inx]
                                      w-mvtocalc.inc-liquido = event_fp.idi_tip_inciden_liq.

                               /******** Chamada EPC - 2 Protege **********/
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
                        if  not l-tem-movto THEN DO:
                            IF tt-param.v_log_enviar_email THEN DO:
                              {include/i-rpclo.i}
                            END.
                            next.
                        END.
                        find habilit_calc_fp use-index hbltclcf_py04002 no-lock where
                             habilit_calc_fp.cdn_empresa    = funcionario.cdn_empresa and
                             habilit_calc_fp.cdn_estab      = funcionario.cdn_estab and
                             habilit_calc_fp.cdn_categ_sal  = func_categ_sal.cdn_categ_sal and
                             &if "{&dthrpyc_version}" >= "2.06" &then
                                habilit_calc_fp.idi_orig_contratac_func  = funcionario.idi_orig_contratac_func and
                                habilit_calc_fp.cdn_prestdor_serv        = funcionario.cdn_prestdor_serv       and
                             &endif
                             habilit_calc_fp.idi_tip_fp = tt-param.i-tipo-folha  and
                             habilit_calc_fp.num_ano_refer_fp_calcula  = tt-param.i-ano-ref     and
                             habilit_calc_fp.num_mes_refer_fp_calcula  = tt-param.i-mes-ref     and
                             habilit_calc_fp.qti_parc_habilit_calc_fp  = tt-param.i-parcela
                             no-error.
                        if  available habilit_calc_fp then
                           assign c-mens-contr[1]=substring(habilit_calc_fp.des_msg_envel_fp,1,40)
                                  c-mens-contr[2]=substring(habilit_calc_fp.des_msg_envel_fp,41,38).

                        find cargo_basic no-lock where
                             cargo_basic.cdn_cargo_basic = v_cdn_cargo_basic no-error.

                        assign i-index     =  func_categ_sal.cdn_categ_sal 
                               c-categoria = {database/inpy/i03py029.i 04 func_categ_sal.cdn_categ_sal}
                               c-mes-folha = c-lit-mes[tt-param.i-mes-ref]
                               c-parabens  =
                                      if  tt-param.i-tipo-folha = 1 and
                                          (month(funcionario.dat_nascimento) = (tt-param.i-mes-ref + 1) or
                                           (month(funcionario.dat_nascimento) = 1 and
                                            tt-param.i-mes-ref = 12))
                                      then "* * *  F e l i z   A n i v e r s a r i o  * * *"
                                      else "".
                        if  funcionario.dat_desligto_func <> ? and
                            (year(funcionario.dat_desligto_func) < tt-param.i-ano-ref or
                             (year(funcionario.dat_desligto_func) = tt-param.i-ano-ref and
                             month(funcionario.dat_desligto_func) <= tt-param.i-mes-ref)) then
                           assign c-parabens =    "*******************************"
                                  c-mensagem[1] = "* * *   D e m i t i d o   * * *"
                                  c-mensagem[2] = "*******************************".
                        else
                           assign c-mensagem[1] = c-mens-contr[1]
                                  c-mensagem[2] = c-mens-contr[2].

                        assign c-nome-compl = funcionario.nom_pessoa_fisic.

                        if  funcionario.idi_forma_pagto = 1 then do:
                           assign i-cta-corrente = funcionario.cdn_cta_corren
                                  c-hifen        = "-"
                                  c-dig-conta    = funcionario.cod_digito_cta_corren.
                           find rh_bco no-lock where
                                rh_bco.cdn_banco = funcionario.cdn_bco_liq no-error.
                           find rh_agenc_bcia of rh_bco no-lock where
                                rh_agenc_bcia.cdn_agenc_bcia = funcionario.cdn_agenc_bcia_liq no-error.
                           assign l-dep-cru = if substr(rh_agenc_bcia.des_cargo_pessoa_contat[3],26,3) = "urv"
                                              then no
                                              else yes.
                        end.
                        else
                           assign i-cta-corrente = 0
                                  c-hifen        = ""
                                  c-dig-conta    = ""
                                  l-dep-cru      = yes.

                        assign i-inx = 1.
                        repeat:
                           if tab_irf_inss.val_lim_faixa_irf_inss[i-inx] = 0 then do:
                              assign i-inx = i-inx - 1.
                              leave.
                           end.
                           assign i-inx = i-inx + 1.
                        end.

                        assign d-limite-inss = tab_irf_inss.val_lim_faixa_irf_inss[i-inx].
                        if d-base-iapas > d-limite-inss then
                           assign d-base-iapas = d-limite-inss.

                        if v_log_folha_educnal then
                           assign i-impr-func = string(funcionario.cdn_func_centrdor, "zzzzz9") + c-barra + 
                                                string(funcionario.cdn_tip_contrat_func, "99")  + c-hifem +
                                                string(funcionario.num_digito_verfdor_func, "9").
                        else 
                           assign i-impr-func = string(funcionario.cdn_funcionario, "zzzzzzz9") + c-hifem +
                                                string(funcionario.num_digito_verfdor_func, "9").
                        for each w-mvtocalc break by w-mvtocalc.fc-codigo
                                                  by w-mvtocalc.identif
                                                  by w-mvtocalc.ev-codigo:
                            if  first-of(w-mvtocalc.fc-codigo) or
                                i-cont-linha > 22 then do:
                               if  i-cont-linha > 22 then do:
                                  do while i-cont-linha < 26:
                                     put "" at 01 skip.
                                     assign i-cont-linha = i-cont-linha + 1.
                                  end.
                                  {utp/ut-liter.i Continua... *}
                                  put  Return-value format "x(11)" at 58  skip(07).
                               end. 
                               assign l-imprime = yes.
                               put c-empresa                           AT  02
                                   c-mes-folha                         at  46
                                   "/"                                 at  55
                                   tt-param.i-ano-ref                  at  56
                                   c-categoria                         at  61 skip(1)
                                   i-impr-func                         at  01
                                   funcionario.nom_pessoa_fisic        at  13    skip
                                   i-cta-corrente                      at  08
                                   c-hifen                             at  17
                                   c-dig-conta                         at  18
                                   cargo_basic.cod_classif_ocupac      at  36
                                   empresa.ep-codigo                   at  43.
                                   if funcionario.cdn_local_pagto <> 0 THEN
                                      PUT funcionario.cdn_local_pagto      at  47.
                                   else 
                                      PUT funcionario.cdn_estab            at  47.
                               PUT funcionario.cod_unid_lotac          at  53 skip(2).
                               assign i-cont-linha = 7.
                            end.

                            put w-mvtocalc.ev-codigo   at  01
                                w-mvtocalc.descricao   at  05.
                            if  w-mvtocalc.horas > 0 then
                               put w-mvtocalc.horas    at 35.
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

                            if  w-mvtocalc.inc-liquido = 2 then
                               put w-mvtocalc.valor       at 56
                                   c-inc-liquido at 69.
                            else
                               put w-mvtocalc.valor       at  42
                                   c-inc-liquido at  55.

                            assign i-cont-linha = i-cont-linha + 1.
                            if  w-mvtocalc.inc-liquido = 1 then
                               assign d-total-vencto = d-total-vencto + w-mvtocalc.valor.
                            else
                               if  w-mvtocalc.inc-liquido = 2 then
                                  d-total-descto = d-total-descto + w-mvtocalc.valor.
                               else.
                            if  last-of(w-mvtocalc.fc-codigo) then do:
                               IF v_log_folha_educnal and
                                  tt-param.l-imp-carga-horar-sem THEN DO:
                                   
                                   FOR EACH bfunciona NO-LOCK
                                      WHERE bfunciona.cdn_empresa       = funcionario.cdn_empresa
                                        AND bfunciona.cdn_estab         = funcionario.cdn_estab
                                        AND bfunciona.cdn_func_centrdor = funcionario.cdn_func_centrdor
                                        AND bfunciona.cdn_tip_contrat_func > 0:
                                       FIND FIRST tip_contrat NO-LOCK
                                            WHERE tip_contrat.cdn_tip_contrat_func = bfunciona.cdn_tip_contrat_func NO-ERROR.
                                       IF AVAIL tip_contrat THEN DO:
                                           IF tip_contrat.log_categ_contrat_educnal THEN DO:
                                               FOR EACH apont_hora_aula no-lock of bfunciona
                                                  WHERE (MONTH(apont_hora_aula.dat_inic_valid)  = month(tt-param.v_dat_valid) /*mes_aux */
                                                    AND YEAR(apont_hora_aula.dat_inic_valid)    = year(tt-param.v_dat_valid)) /*ano_aux */
                                                     OR  (apont_hora_aula.dat_inic_valid       <= tt-param.v_dat_valid        /*ultimo dia DO mes aux*/
                                                     AND  apont_hora_aula.dat_fim_valid        >= tt-param.v_dat_valid)       /*ultimo dia DO mes aux*/  :
                                                   IF param_folha_educnal.num_livre_1 = 1 /*Mensal*/ THEN
                                                       ASSIGN d-horas-semanais = d-horas-semanais + (apont_hora_aula.qtd_hrs_trab_semanal / 5).
                                                   ELSE
                                                       ASSIGN d-horas-semanais = d-horas-semanais + apont_hora_aula.qtd_hrs_trab_semanal.
                                               END.
                                           END.
                                       END.
                                   END.
            
                                   put SKIP(1)
                                       c-carga-horaria  AT 05
                                       d-horas-semanais AT 47 SKIP.
            
                                   do  while i-cont-linha < 22:
                                      put "" at 01 skip.
                                      assign i-cont-linha = i-cont-linha + 1.
                                   end.
                               END.
                               ELSE DO:
                                   do  while i-cont-linha < 24:
                                      put "" at 01 skip.
                                      assign i-cont-linha = i-cont-linha + 1.
                                   end.
                               END.
        
                               put c-parabens            at 01
                                   d-total-vencto        at 41
                                   d-total-descto        at 56 "-" skip
                                   c-mensagem[1]         at 01 skip
                                   c-mensagem[2]         at 01
                                   d-liquido-pagar       at 55 skip(1)
                                   d-salar-atual         at 01 format ">>>>>,>>9.99"
                                   d-base-iapas          at 14
                                   d-base-fgts           at 28
                                   d-valor-fgts          at 42
                                   d-base-irrf           at 55 skip(5).
                            end.
                            delete w-mvtocalc.
                        end.
                        IF tt-param.v_log_enviar_email THEN DO:
                            {include/i-rpclo.i}
                            FIND rh_pessoa_fisic NO-LOCK WHERE
                                 rh_pessoa_fisic.num_pessoa_fisic = funcionario.num_pessoa_fisic NO-ERROR.
                            IF AVAIL rh_pessoa_fisic THEN
                                ASSIGN c_destino = rh_pessoa_fisic.nom_e_mail
                                       c_senha   = rh_pessoa_fisic.cod_id_feder.
                            ELSE
                                ASSIGN c_destino = "":U
                                       c_senha   = "":U.
                
                            assign c_anexo = session:TEMP-DIRECTORY + "extrato.zip":U
                                   l-compactar = YES. /*YES.*/
                            /* O programa envia e-mail, mas tem problema na senha, por causa do cod_senha da PF, que Ç gravado com encode.
                               se o l-compactar estiver com o valor no entao nao coloca senha no arquivo */
                
                            {prghur/fpp/fp3500.i}
            
                            IF l-ErroZip THEN DO:
                                FIND FIRST tt-erros-zip NO-ERROR.
                                IF AVAIL tt-erros-zip THEN
                                    ASSIGN c_msg_erro = tt-erros-zip.desc-erro.
                                ELSE
                                    ASSIGN c_msg_erro = "":U.
                            END.
                            ELSE DO:
                                FIND FIRST tt-erros NO-ERROR.
                                IF AVAIL tt-erros THEN
                                    ASSIGN c_msg_erro = tt-erros.desc-erro.
                                ELSE
                                    ASSIGN c_msg_erro = "":U.
                            END.
                            
                            CREATE tt-rel-erros.
                            ASSIGN tt-rel-erros.cdn_empresa      = funcionario.cdn_empresa
                                   tt-rel-erros.cdn_estab        = funcionario.cdn_estab
                                   tt-rel-erros.cdn_funcionario  = funcionario.cdn_funcionario
                                   tt-rel-erros.nom_pessoa_fisic = funcionario.nom_pessoa_fisic
                                   tt-rel-erros.status_email     = IF RETURN-VALUE = "NOK" THEN v_status_nao_enviado ELSE v_status_enviado
                                   tt-rel-erros.email            = IF RETURN-VALUE = "OK" THEN c_destino ELSE c_msg_erro.
                            
                        END.
                    end.
                end.
            end.
    end.

IF NOT tt-param.v_log_enviar_email THEN DO:
    {include/i-rpclo.i}
END.

delete procedure h-utapi019.
DELETE PROCEDURE h-zip.

return "ok".

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
    if return-value = "OK-STA-CASA-FP3500R1-NEXT" then return "NOK".
    /******** Fim Chamada EPC - Santa Casa **********/
    
    return "OK".

end procedure.

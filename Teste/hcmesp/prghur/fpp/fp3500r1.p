/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/buffers_RH.i}

{include/i-prgvrs.i FP3500R1 1.02.11.015}  /*** 011115 ***/
/*******************************************************************************
**
**     Programa..: FP3500R1.P
**
**     Data......: Abril/1994
**
**     Autor.....: DATASUL S.A
**
**     Objetivo..: Emissao dos Envelopes de Pagamento - Especifico da Yamana 
**                 Formulario    = 4 (Padrao Individual)
**                 Modelo DE 69154921-10
**     
**     Alteraá‰es: 11/03/2006 - Sidnei Barbosa - Datasul SP.
**                 - Ajustes no programa para rodar no HCM209.
**
*******************************************************************************/

{include/i-rpvar.i}

DEF VAR d-margem      AS DEC format ">>>>>,>>9.99".
def var d-consignacao as dec format ">>>>>,>>9.99".

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

/*-----Inicio-(bisneto)---definicoes da pi-zipa-pdf---------------------------*/
  DEF VAR h-zip        AS HANDLE              NO-UNDO.    
  DEF VAR c_anexo      AS CHAR FORMAT "x(60)" NO-UNDO.
  DEF VAR c_senha      AS CHAR FORMAT "x(16)" NO-UNDO.
  DEF VAR c_status_zip AS CHAR FORMAT "x(3)"  NO-UNDO.
  DEFINE TEMP-TABLE tt-listFiles NO-UNDO
    FIELD cFile     AS CHAR    FORMAT "x(200)"
    FIELD lSearch   AS LOGICAL.
  DEFINE TEMP-TABLE tt-erros-zip NO-UNDO
      FIELD cod-erro  AS INTEGER FORMAT ">>>>9"
      FIELD desc-erro AS CHAR    FORMAT "x(70)".
/*-----Fim----(bisneto)---definicoes da pi-zipa-pdf---------------------------*/
/*-----Inicio-(bisneto)---definicoes da customizaá∆o envio em PDF/Email--------------*/
{utp/ut-glob.i} /* v_cdn_empres_usuar v_cod_usuar_corren */
def var c-tipo-folha       as char   no-undo.
DEF VAR c-nome-arq-pdf     AS CHAR   NO-UNDO.
DEF VAR c-nome-arq-pdf-aux AS CHAR NO-UNDO.
DEF VAR c-nome-arq-zip     AS CHAR   NO-UNDO.
DEF VAR c-ender            AS CHAR   NO-UNDO. 
DEF VAR c-temp-dir         AS CHAR   NO-UNDO.
/* DEF VAR l-email-pdf        AS LOG    NO-UNDO. */
DEF VAR l-erro             AS LOG    NO-UNDO.
DEF VAR h-fp3500r1a        AS HANDLE NO-UNDO.    
/*-----Fim----(bisneto)---definicoes da customizaá∆o envio em PDF/Email--------------*/
/*-----Inicio-(bineto)---definicoes da pi-envia-zip-email----------------------------*/
{utp/utapi019.i}
DEFINE VARIABLE c-email-empresa AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-assunto       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-destino-email AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cArquivoFinal   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-texto-email   AS CHARACTER  NO-UNDO.

DEF TEMP-TABLE tt-email         LIKE tt-envio2.
DEFINE TEMP-TABLE tt-erros-aux LIKE tt-erros
    FIELD email AS CHARACTER.
/*-----Fim----(bisneto)---definicoes da pi-envia-zip-email---------------------------*/
def var v_cdn_turno_trab  like funcionario.cdn_turno_trab       no-undo.
def var v_cdn_turma_trab  like funcionario.cdn_turma_trab       no-undo.
DEF NEW GLOBAL SHARED VAR c-seg-usuario        AS CHAR FORMAT "x(12)" NO-UNDO.
/* DEF NEW GLOBAL SHARED VAR g-arquivo-imagem     AS CHAR FORMAT "x(12)" NO-UNDO. */
def buffer bfuncionario for funcionario.
def buffer cfuncionario for funcionario.
def var v_log_sem_salario as logical no-undo.

def buffer b2funcionario for funcionario.

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
{prghur/fpp/fp3500tt.i shared} /* definicao de tt-param */
{prghur/fpp/fp9200.i10 shared}
{prghur/fpp/fp9200.i8}
{prghur/fpp/fp9400.i}

define buffer b-tt-digita for tt-digita.
find first tt-param NO-LOCK.
DEF TEMP-TABLE tt-aux-param NO-UNDO LIKE tt-param.
def shared var v_han_acomp as handle no-undo.

/* DEF NEW GLOBAL SHARED VAR g-texto-email AS CHAR NO-UNDO.                 */
/*                                                                          */
/* /*Begins: 03072018 - Quando for rpw n∆o dever† mostrar esta mensagem */  */
/* IF tt-param.tipo-execucao = 1 THEN                                       */
/*     MESSAGE                                                              */
/*       "Deseja Enviar PDF por E-mail?"                                    */
/*       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-email-pdf.      */
/* ELSE                                                                     */
/*     ASSIGN l-email-pdf    = YES                                          */
/*            g-texto-email  = "ENVIO DE EMAIL AUTOMµTICO VIA ROTINA RPW.". */
/*                                                                          */
/* IF l-email-pdf = YES THEN                                                */
/* DO:                                                                      */
/*     IF VALID-HANDLE(v_han_acomp) THEN                                    */
/*       RUN pi-finalizar IN v_han_acomp.                                   */
/*                                                                          */
/*     IF tt-param.tipo-execucao = 1 THEN                                   */
/*        RUN prghur\fpp\fp3500r1b.p.                                       */
/*                                                                          */
/*     RUN utp/ut-acomp.p PERSISTENT SET v_han_acomp.                       */
/*     RUN pi-inicializar IN v_han_acomp (INPUT "Processando").             */
/*     run pi-acompanhar in v_han_acomp (input "Processando").              */
/* END.                                                                     */

FOR EACH tt-aux-param: DELETE tt-aux-param. END.
FOR FIRST tt-param
  NO-LOCK:
  
  CREATE tt-aux-param.
  BUFFER-COPY tt-param TO tt-aux-param.
END.


&global-define task_clt yes
{prghur/fpp/fp9200.i11}



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
define var v_cdn_local         as char  format "x(5)"                  no-undo.

def var v_cdn_cargo_basic like funcionario.cdn_cargo_basic        no-undo.
def var v_cdn_niv_cargo   like funcionario.cdn_niv_cargo          no-undo.
def var v_val_sal_mensal  like histor_sal_func.val_salario_mensal no-undo.
def var v_val_sal_hora    like histor_sal_func.val_salario_hora   no-undo.
def var d-hrs-categ       like histor_sal_func.qtd_hrs_categ_sal  no-undo.
def var d-sal-cat         like histor_sal_func.val_salario_categ  no-undo.
def var d-sal-mes         like histor_sal_func.val_salario_mensal no-undo.
def var v_cdn_categ_sal   like funcionario.cdn_categ_sal          no-undo.

define temp-table w-mvtocalc
       field fc-codigo   like funcionario.cdn_funcionario
/*        field ev-codigo   as int format ">>9" */
       field ev-codigo   AS CHARACTER
       field descricao   like event_fp.des_event_fp     format "x(25)"
       field identif     as integer
       field unidades    as decimal
       field horas       as decimal                format ">>9.999"
       field salar-hora  as decimal                format ">>9.9999"
       field base        as decimal
       field valor       as decimal                format ">>>>>,>>9.99-"
       field inc-liquido like event_fp.idi_tip_inciden_liq.


find empresa no-lock where empresa.ep-codigo = tt-aux-param.v_cdn_empres_usuar  no-error. 

find param_empres_rh no-lock where param_empres_rh.cdn_empresa = tt-aux-param.v_cdn_empres_usuar no-error.
find param_folha_educnal no-lock where
     param_folha_educnal.cdn_empresa = tt-aux-param.v_cdn_empres_usuar no-error.
if avail param_folha_educnal then
   assign v_log_folha_educnal = yes.

find tab_irf_inss where
     tab_irf_inss.num_ano_refer_tab_irf_inss = tt-aux-param.i-ano-ref and
     tab_irf_inss.num_mes_refer_tab_irf_inss = tt-aux-param.i-mes-ref no-lock no-error.

if  l-origem then do:
    for each tt-digita:
       delete tt-digita.
    end.
    create tt-digita.
    assign tt-digita.num_forma_pagto = tt-aux-param.r-forma-pgto.
end.
else 
    assign tt-aux-param.v_cod_unid_lotac_ini = ""
          tt-aux-param.v_cod_unid_lotac_fim = "Zzzzzzzzzzz"
          tt-aux-param.i-cc-codigo-1        = ""
          tt-aux-param.i-cc-codigo-2        = "Zzzzzzzz"
          tt-aux-param.v_nom_func_ini       = ""
          tt-aux-param.v_nom_func_fim       = "Zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"
          tt-aux-param.l-mensal             = yes
          tt-aux-param.l-horista            = yes
          tt-aux-param.l-semanal            = yes
          tt-aux-param.l-quinzenal          = yes
          tt-aux-param.l-tarefa             = yes
          tt-aux-param.l-diarista           = yes
          tt-aux-param.i-bc-codigo-1        = 1
          tt-aux-param.i-bc-codigo-2        = 999
          tt-aux-param.i-ag-codigo-1        = 1
          tt-aux-param.i-ag-codigo-2        = 9999.

{include/i-rpcab.i}
/* {include/i-rpout.i &pagesize=0} */
    /* Ini Bisneto */
    FOR FIRST param-global FIELDS(log-1 serv-mail porta-mail) NO-LOCK:
    END.
    /*     IF AVAIL usuar_mestre THEN */
    ASSIGN c-email-empresa = "rh@yamana.com" /* usuar_mestre.cod_e_mail_local */.
    FIND FIRST param_glob_rh NO-LOCK NO-ERROR.
    FOR EACH tt-envio2: DELETE tt-envio2. END.
    FOR EACH tt-email:  DELETE tt-email.  END.
    /* Fim Bisneto */
    if i-ord-aux = 3 or
       i-ord-aux = 4 or
       i-ord-aux = 5 or
       i-ord-aux = 6 then do:

       RUN pi-inicializar IN v_han_acomp (INPUT "Processando, Rotina 1").

       ASSIGN i-contador = 0.

       {prghur/fpp/fp9200.i6}

       for each tt_lotac_funcionario no-lock,
           each func_ccusto no-lock where
                func_ccusto.cdn_empresa          = tt_lotac_funcionario.cdn_empresa and
                func_ccusto.cdn_estab            = tt_lotac_funcionario.cdn_estab and
                func_ccusto.cdn_funcionario      = tt_lotac_funcionario.cdn_funcionario and
                func_ccusto.dat_inic_lotac_func <= tt-aux-param.v_dat_valid and
                func_ccusto.dat_fim_lotac_func  >= tt-aux-param.v_dat_valid and
                func_ccusto.cod_rh_ccusto       >= tt-aux-param.i-cc-codigo-1 and
                func_ccusto.cod_rh_ccusto       <= tt-aux-param.i-cc-codigo-2,
           each func_categ_sal no-lock where
                func_categ_sal.cdn_empresa          = func_ccusto.cdn_empresa and
                func_categ_sal.cdn_estab            = func_ccusto.cdn_estab and
                func_categ_sal.cdn_funcionario      = func_ccusto.cdn_funcionario and
                func_categ_sal.dat_inic_lotac_func <= tt-aux-param.v_dat_valid and
                func_categ_sal.dat_fim_lotac_func  >= tt-aux-param.v_dat_valid and
              ((func_categ_sal.cdn_categ_sal     = 1 and tt-aux-param.l-mensal    = yes)  or
               (func_categ_sal.cdn_categ_sal     = 2 and tt-aux-param.l-horista   = yes)  or
               (func_categ_sal.cdn_categ_sal     = 3 and tt-aux-param.l-semanal   = yes)  or
               (func_categ_sal.cdn_categ_sal     = 4 and tt-aux-param.l-quinzenal = yes)  or
               (func_categ_sal.cdn_categ_sal     = 5 and tt-aux-param.l-tarefa    = yes)  or
               (func_categ_sal.cdn_categ_sal     = 6 and tt-aux-param.l-diarista  = yes)),
           each funcionario use-index fncnr_py08504 of func_categ_sal no-lock where
                funcionario.nom_pessoa_fisic >= tt-aux-param.v_nom_func_ini            and
                funcionario.nom_pessoa_fisic <= tt-aux-param.v_nom_func_fim            and
                funcionario.cod_rh_ccusto    >= tt-aux-param.i-cc-codigo-1             and
                funcionario.cod_rh_ccusto    <= tt-aux-param.i-cc-codigo-2             and
                funcionario.cdn_local_pagto  >= tt-aux-param.cdn_local_pagto_ini       and
                funcionario.cdn_local_pagto  <= tt-aux-param.cdn_local_pagto_fim       and
              ((tt-aux-param.r-forma-pgto         = 4                                  and
                funcionario.idi_forma_pagto  <> 0)                                 or
                funcionario.idi_forma_pagto   = tt-aux-param.r-forma-pgto )
               break  BY   if i-ord-aux = 3 or
                           i-ord-aux = 4
                        then funcionario.idi_forma_pagto
                        else 0
                 by funcionario.cdn_estab
                 by tt_lotac_funcionario.num_seq_unid_lotac
                 by tt_lotac_funcionario.num_niv_unid_lotac 
                 by tt_lotac_funcionario.cod_unid_lotac
                 by if  i-ord-aux = 3 or
                        i-ord-aux = 5 
                    then string(funcionario.cdn_funcionario,"99999999")
                    else funcionario.nom_pessoa_fisic:

               if v_log_folha_educnal then
                  if funcionario.cdn_tip_contrat_func <> 0 then
                     next.                  
               assign i-contador = i-contador + 1.        
               run pi-acompanhar in v_han_acomp (input STRING(i-contador) + " - Func: " + STRING(funcionario.cdn_funcionario)). 

               find rh_pessoa_fisic of funcionario no-lock.
               find rh_estab of funcionario no-lock.
               find local_pagto_func no-lock where
                    local_pagto_func.nom_local_pagto = funcionario.cdn_local_pagto no-error.
                if funcionario.cdn_local_pagto <> 0 
                then assign v_cdn_local = string(funcionario.cdn_local_pagto).
                else assign v_cdn_local = string(funcionario.cdn_estab).
                if funcionario.idi_forma_pagto       <> 3 then do:
                   if funcionario.cdn_bco_liq         < tt-aux-param.i-bc-codigo-1  or
                      funcionario.cdn_bco_liq         > tt-aux-param.i-bc-codigo-2  or
                      funcionario.cdn_agenc_bcia_liq  < tt-aux-param.i-ag-codigo-1  or
                      funcionario.cdn_agenc_bcia_liq  > tt-aux-param.i-ag-codigo-2  then next.
                end.              

                if tt-aux-param.i-mes-ref = 12 then do:
                  if  funcionario.dat_desligto_func <> ?                                           and
                     (funcionario.dat_desligto_func <= (date(01,01,(tt-aux-param.i-ano-ref + 1)) - 1)) and
                      tt-aux-param.l-emite-demi          = no then next.
                end.
                else do:
                  if  funcionario.dat_desligto_func <> ?                                                           and
                     (funcionario.dat_desligto_func <= (date((tt-aux-param.i-mes-ref + 1),01,tt-aux-param.i-ano-ref) - 1)) and
                      tt-aux-param.l-emite-demi = no then next.
                end.

                if not v_log_folha_educnal then do:
                   /***** n∆o substituir o find abaixo pela chamada da procedure pi_histor_sal_func *******/
                   find last histor_sal_func no-lock where
                           histor_sal_func.cdn_empresa      = funcionario.cdn_empresa and
                           histor_sal_func.cdn_estab        = funcionario.cdn_estab       and
                           histor_sal_func.cdn_funcionario  = funcionario.cdn_funcionario and
                           histor_sal_func.dat_liber_sal   <= tt-aux-param.v_dat_valid no-error. 

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
                                    histor_sal_func.dat_liber_sal   <= tt-aux-param.v_dat_valid no-error. 
                          assign v_log_sem_salario = yes.                                            
                      end.
                      else do:
                          /***** n∆o substituir o find abaixo pela chamada da procedure pi_histor_sal_func *******/
                          find last histor_sal_func no-lock where
                                    histor_sal_func.cdn_empresa      = bfuncionario.cdn_empresa and
                                    histor_sal_func.cdn_estab        = bfuncionario.cdn_estab       and
                                    histor_sal_func.cdn_funcionario  = bfuncionario.cdn_funcionario and
                                    histor_sal_func.dat_liber_sal   <= tt-aux-param.v_dat_valid no-error.                               
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

                assign l-tem-movto     = no
                       d-base-iapas    = 0
                       d-base-fgts     = 0
                       d-valor-fgts    = 0
                       d-base-irrf     = 0
                       d-liquido-pagar = 0
                       d-total-vencto  = 0
                       d-total-descto  = 0
                       i-cont-linha    = 0
                       d-salar-atual   = histor_sal_func.val_salario_categ.

                for each movto_calcul_func of funcionario no-lock where
                         movto_calcul_func.num_ano_refer_fp  = tt-aux-param.i-ano-ref and
                         movto_calcul_func.num_mes_refer_fp  = tt-aux-param.i-mes-ref and
                         movto_calcul_func.idi_tip_fp = tt-aux-param.i-tipo-folha and
                         movto_calcul_func.qti_parc_habilit_calc_fp    = tt-aux-param.i-parcela:

                  run pi-acompanhar in v_han_acomp (input STRING(i-contador) + " - Func: " + STRING(funcionario.cdn_funcionario) + " - DT.Atualiz: " + STRING(movto_calcul_func.dat_ult_atualiz)). 

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
                  END.
                  assign i-inx       = 0
                         l-tem-movto = yes.
                    repeat:

                       run pi-acompanhar in v_han_acomp (input STRING(i-contador) + " - Func: " + STRING(funcionario.cdn_funcionario) + " - DT.Atualiz: " + STRING(movto_calcul_func.dat_ult_atualiz) + " - " + STRING(i-inx)). 

                       assign i-inx = i-inx + 1.
                       if  i-inx > movto_calcul_func.qti_efp then
                          leave.
                       if movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 20 and
                          movto_calcul_func.val_calcul_efp[i-inx] = 0 then
                          next.
                       if movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 56 and
                          movto_calcul_func.val_calcul_efp[i-inx] = 0 then
                          next.
                       find FIRST event_fp no-lock where
                            event_fp.cdn_event_fp = movto_calcul_func.cdn_event_fp[i-inx].
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
                       if movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 17 then
                          assign d-base-iapas = movto_calcul_func.val_base_calc_fp[i-inx].
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
/*                               w-mvtocalc.ev-codigo   = int(event_fp.cdn_event_fp) */
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

                       if v_log_folha_educnal then do:
                          if w-mvtocalc.identif = 1 then do:                          
                             if v_log_sem_salario then
                                assign d-salar-atual = 0.
                          end.
                       end.                                                 
                    end.
                end.
                if  not l-tem-movto then
                   next.
                find habilit_calc_fp use-index hbltclcf_py04002 no-lock where
                     habilit_calc_fp.cdn_empresa  = funcionario.cdn_empresa and
                     habilit_calc_fp.cdn_estab  = funcionario.cdn_estab and
                     habilit_calc_fp.cdn_categ_sal  = func_categ_sal.cdn_categ_sal and
                     &if "{&dthrpyc_version}" >= "2.06" &then
                        habilit_calc_fp.idi_orig_contratac_func  = funcionario.idi_orig_contratac_func and
                        habilit_calc_fp.cdn_prestdor_serv        = funcionario.cdn_prestdor_serv       and
                     &endif
                     habilit_calc_fp.idi_tip_fp = tt-aux-param.i-tipo-folha  and
                     habilit_calc_fp.num_ano_refer_fp_calcula  = tt-aux-param.i-ano-ref     and
                     habilit_calc_fp.num_mes_refer_fp_calcula  = tt-aux-param.i-mes-ref     and
                     habilit_calc_fp.qti_parc_habilit_calc_fp    = tt-aux-param.i-parcela
                     no-error.
                if  available habilit_calc_fp then
                   assign c-mens-contr[1]=substring(habilit_calc_fp.des_msg_envel_fp,1,40)
                          c-mens-contr[2]=substring(habilit_calc_fp.des_msg_envel_fp,41,38).
                find cargo_basic no-lock where
                     cargo_basic.cdn_cargo_basic = v_cdn_cargo_basic no-error.
                assign i-index     =  func_categ_sal.cdn_categ_sal 
                       c-categoria = {database/inpy/i03py029.i 04 func_categ_sal.cdn_categ_sal}
                       c-mes-folha = c-lit-mes[tt-aux-param.i-mes-ref]
                       c-parabens  =
                              if  tt-aux-param.i-tipo-folha = 1 and
                                  (month(rh_pessoa_fisic.dat_nascimento) = (tt-aux-param.i-mes-ref + 1) or
                                   (month(rh_pessoa_fisic.dat_nascimento) = 1 and
                                    tt-aux-param.i-mes-ref = 12))
                              then "* * *  F e l i z   A n i v e r s a r i o  * * *"
                              else "".
                if  funcionario.dat_desligto_func <> ? and
                    (year(funcionario.dat_desligto_func) < tt-aux-param.i-ano-ref or
                     (year(funcionario.dat_desligto_func) = tt-aux-param.i-ano-ref and
                     month(funcionario.dat_desligto_func) <= tt-aux-param.i-mes-ref)) then
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
                          c-dig-conta    = funcionario.cod_digito_cta_corren
                          i-cod-banco    = funcionario.cdn_bco_liq
                          i-cod-agencia  = funcionario.cdn_agenc_bcia_liq.
                   find rh_bco   no-lock where
                        rh_bco.cdn_banco = funcionario.cdn_bco_liq.
                   find rh_agenc_bcia of rh_bco no-lock where
                        rh_agenc_bcia.cdn_agenc_bcia = funcionario.cdn_agenc_bcia_liq.
                   assign l-dep-cru = if substr(rh_agenc_bcia.des_cargo_pessoa_contat[3],26,3) = "urv"
                                      then no
                                      else yes.
                end.
                else
                   assign i-cta-corrente = 0
                          c-hifen        = ""
                          c-dig-conta    = ""
                          i-cod-banco    = 0
                          i-cod-agencia  = 0
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

                  
                ASSIGN d-consignacao = 0.
                
                for each w-mvtocalc NO-LOCK:
                
                    if w-mvtocalc.ev-codigo = '461' then 
                       ASSIGN d-consignacao = w-mvtocalc.valor.

                    if w-mvtocalc.inc-liquido = 1 then
                       assign d-total-vencto = d-total-vencto + w-mvtocalc.valor.
                    ELSE if  w-mvtocalc.inc-liquido = 2 then
                       ASSIGN d-total-descto = d-total-descto + w-mvtocalc.valor.

                END.

                /*PUT FILL("1234567890",8) FORMAT "x(150)" AT 01.**/
                    

                for each w-mvtocalc break by w-mvtocalc.fc-codigo
                                          by w-mvtocalc.identif
                                          by w-mvtocalc.ev-codigo:

                    if  first-of(w-mvtocalc.fc-codigo) OR i-cont-linha > 22 then do:
                       if i-cont-linha > 22 then do:

                          do while i-cont-linha < 24:
                             put "" at 01 skip.
                             assign i-cont-linha = i-cont-linha + 1.
                          end.

                          {utp/ut-liter.i Continua... *}
                          /* FELIPE SILVESTRE - KRAFT CONSULTING 29/07/2010 - SE FOR A OPÄ«O DO COMBO 5 OU 6 IMPRIMI 5 LINHAS DEPOIS DO CONTINUA */  
                          IF i-ord-aux = 5 OR i-ord-aux = 6 THEN
                          DO:
                              if  first-of(w-mvtocalc.fc-codigo) THEN  
                                put  Return-value  at 58  skip(07).
                              else 
                                  put  Return-value  at 58  skip(05).  
                          END.
                          ELSE
                          DO:
                              if  first-of(w-mvtocalc.fc-codigo) THEN  
                                put  Return-value  at 58  skip(07).
                              else 
                                  put  Return-value  at 58  skip(04).  
                          END.
                          
                          
                       end. 

                       assign l-imprime = yes.
                       FIND FIRST rh_pessoa_jurid NO-LOCK 
                            WHERE rh_pessoa_jurid.num_pessoa_jurid = rh_estab.num_pessoa_jurid
                            NO-ERROR.
                       FIND FIRST unid_lotac NO-LOCK
                            WHERE unid_lotac.cod_unid_lotac = tt_lotac_funcionario.cod_unid_lotac
                            NO-ERROR.

                       FIND FIRST cargo NO-LOCK
                            WHERE cargo.cdn_cargo_basic =   cargo_basic.cdn_cargo_basic
                            NO-ERROR.

                        /*find rh_pessoa_fisic of funcionario no-lock.*/

                       /*raimundo*/
                       FIND FIRST func_emprest_financ OF funcionario 
                            NO-LOCK NO-ERROR.

                       FIND rh_ccusto WHERE rh_ccusto.cdn_empresa   = funcionario.cdn_empresa
                                        AND rh_ccusto.cod_rh_ccusto = funcionario.cod_rh_ccusto NO-LOCK NO-ERROR.

                       /*FIND FIRST rh_pessoa_jurid 
                            WHERE rh_pessoa_jurid.nom_pessoa_jurid = empresa.razao-social NO-LOCK NO-ERROR.*/
                       
                       FIND FIRST rh_pessoa_jurid 
                            WHERE rh_pessoa_jurid.cod_id_feder = empresa.cgc NO-LOCK NO-ERROR.

                       
                       PUT /*SKIP*/   /*                            cargo.des_envel_pagto               at  65 */
                           /* cida - dados da empresa */
                           substr(empresa.razao-social,1,40) FORMAT "X(40)"   AT  01 
                           /*rh_pessoa_jurid.cod_id_feder                         AT  48 */
                           empresa.cgc                                          AT  48

                           funcionario.nom_pessoa_fisic  FORMAT "X(33)"          AT  01 
                           funcionario.cdn_funcionario                           AT  37
                           /*funcionario.cod_unid_lotac                            AT  48*/
                           substring(TRIM(c-mes-folha),1,3)  FORMAT "x(3)"             AT  60
                           "/"                                 
                           tt-aux-param.i-ano-ref.       

                       PUT funcionario.cdn_bco_liq                                         AT 01
                           funcionario.cdn_agenc_bcia_liq                                  AT 08
                           funcionario.cdn_cta_corren                                      AT 20
                           "-"                                                             AT 29
                           funcionario.cod_digito_cta_corren                               AT 30
                           /*SKIP (1)*/

                           rh_pessoa_fisic.cod_id_feder     FORMAT "x(20)"                     AT 01 /* "cpf"                   */ 
                           funcionario.qti_depend_irf                            AT 30 /* "Dependente I renda"    */ 
                           funcionario.qti_depend_salfam                         AT 42 /* "Depende S.F. Legal"    */ 
                           /**** ccusto e descricao */

                           substr(funcionario.cod_unid_lotac,1,14) FORMAT "x(14)" AT 01 /* lotacao */
                           unid_lotac.des_unid_lotac                             AT 15
                           /*
                           funcionario.cod_rh_ccusto                             AT 01  /* "Centro de custo"       */ 
                           rh_ccusto.des_rh_ccusto                               AT 15  /* desc ccusto */

                              */
                           /* SKIP */

                           cargo.des_cargo                 FORMAT "X(26)"        AT 05
                           d-salar-atual format ">>>>>,>>9.99"                   AT 33  /* "Salario Basico"      */ 
                           d-base-iapas  format ">>>>>,>>9.99"                   AT 59  /* "Salario contra Inss" */ /*nao tenho certeza*/

                           SKIP(2).

                           PUT 
                             d-base-irrf                                         AT 01 /* "Base de C†lculo IRRF"          */
                             d-consignacao                                       at 21.
                             
                       /*
                       IF AVAIL func_emprest_financ THEN
                          PUT func_emprest_financ.val_dispon_emprest             AT 25 /* "Margem consignaá∆o"            */.
                       ELSE PUT 0 FORMAT "9.99".
                        */
                        
                         ASSIGN d-margem =  d-salar-atual * 0.3.
                           PUT 
                             d-base-fgts                                         AT 40 /* "Base calculo FGTS"             */
                             d-valor-fgts                                        AT 59 /* "FGTS MES"                      */

                        SKIP (1). 
                        
                        if funcionario.cdn_estab = "1" or funcionario.cdn_estab = "2" then 
                         put d-margem                                            AT 08 /* "Proximo adiantamento ???"  fixo 30% do salario atual     */.
                         else 
                         put "" at 08.
                             
                         put d-total-vencto                                      AT 20 /* "total pagto "                  */
                             d-total-descto                                      AT 40 /* "total descto"                  */
                             d-liquido-pagar                                     AT 58. /* "Liquido a receber"             */
                             
                             /*SKIP(1).*/
                         /* banco/agenc/ccorrente */
                       



                      assign i-cont-linha = 7.
                    end.

                    put trim(w-mvtocalc.ev-codigo)   at  01 FORMAT "X(05)".
                       
                    if  w-mvtocalc.horas > 0 then
                    put w-mvtocalc.horas    at 08. /*frequencia*/
                    
                    PUT w-mvtocalc.descricao   AT  17.

                    if w-mvtocalc.inc-liquido = 1 THEN assign c-inc-liquido = "+".
                    if w-mvtocalc.inc-liquido = 2 then assign c-inc-liquido = "-".
                    if w-mvtocalc.inc-liquido = 3 then assign c-inc-liquido = " ".
                    if w-mvtocalc.inc-liquido = 4 then assign c-inc-liquido = "P".
                    if w-mvtocalc.inc-liquido = 5 then assign c-inc-liquido = "N".

                    if  w-mvtocalc.inc-liquido = 2 then
                        put w-mvtocalc.valor  AT 57   
                           c-inc-liquido FORMAT "X" .
                    else
                       put w-mvtocalc.valor   at  57 
                           c-inc-liquido FORMAT "X".


                    assign i-cont-linha = i-cont-linha + 1.

                    if  last-of(w-mvtocalc.fc-codigo) then do:
                       do  while i-cont-linha < 23:
                          put "" at 01 skip.
                          assign i-cont-linha = i-cont-linha + 1.
                       end.

                       
                       put 
                           c-parabens            at 01
                           c-mensagem[1]          SKIP
                           c-mensagem[2]         at 01.

                       put skip(4).
                       assign i-cont-linha = i-cont-linha + 4.
                    end.

                    delete w-mvtocalc.

                END.
       END.
    end.
    else do:
        RUN pi-inicializar IN v_han_acomp (INPUT "Processando, Rotina 2").

        /* ini bisneto */
        RUN utp/ut-zip.p PERSISTENT SET h-zip. 
        /* fim bisneto */
        blk_digita:
        for each tt-digita:

            ASSIGN i-contador = 0.

            if v_log_folha_educnal 
                then assign i-matr-ini = {PRGHUR/DOP/ENG005.I &VAR="(tt-digita.v_cdn_func_centr * 100)"}
                            i-matr-fim = {PRGHUR/DOP/ENG005.I &VAR="(tt-digita.v_cdn_func_centr * 100)"}.
                else assign i-matr-ini = tt-digita.v_cdn_funcionario
                            i-matr-fim = tt-digita.v_cdn_funcionario.
            if tt-aux-param.v_log_origem = no then 
               assign tt-aux-param.i-es-ini = tt-digita.v_cdn_estab
                      tt-aux-param.i-es-fim = tt-digita.v_cdn_estab
                      tt-aux-param.i-fc-ini = i-matr-ini
                      tt-aux-param.i-fc-fim = i-matr-fim.
            else
               if v_log_folha_educnal then
                  assign i-matr-ini = {PRGHUR/DOP/ENG005.I &VAR="(tt-aux-param.i-fc-ini * 100)"}
                         i-matr-fim = {PRGHUR/DOP/ENG005.I &VAR="(tt-aux-param.i-fc-fim * 100)"}.
               else 
                  assign i-matr-ini = tt-aux-param.i-fc-ini 
                         i-matr-fim = tt-aux-param.i-fc-fim.            

            for each rh_estab  no-lock where     
                     rh_estab.cdn_empresa  = param_empres_rh.cdn_empresa and
                     rh_estab.cdn_estab >= tt-aux-param.i-es-ini and
                     rh_estab.cdn_estab <= tt-aux-param.i-es-fim:
                for each func_ccusto no-lock where
                         func_ccusto.cdn_empresa          = rh_estab.cdn_empresa and
                         func_ccusto.cdn_estab            = rh_estab.cdn_estab and
                         func_ccusto.cdn_funcionario     >= i-matr-ini and
                         func_ccusto.cdn_funcionario     <= i-matr-fim and
                         func_ccusto.dat_inic_lotac_func <= tt-aux-param.v_dat_valid and
                         func_ccusto.dat_fim_lotac_func  >= tt-aux-param.v_dat_valid and
                         func_ccusto.cod_rh_ccusto       >= tt-aux-param.i-cc-codigo-1 and
                         func_ccusto.cod_rh_ccusto       <= tt-aux-param.i-cc-codigo-2,
                    each func_turno_trab no-lock where
                         func_turno_trab.cdn_empresa                     = func_ccusto.cdn_empresa and
                         func_turno_trab.cdn_estab                       = func_ccusto.cdn_estab and
                         func_turno_trab.cdn_funcionario                 = func_ccusto.cdn_funcionario and
                         func_turno_trab.dat_inic_lotac_func_turno_trab <= tt-aux-param.v_dat_valid and
                         func_turno_trab.dat_term_lotac_func            >= tt-aux-param.v_dat_valid ,
                    each func_categ_sal no-lock where
                         func_categ_sal.cdn_empresa          = func_turno_trab.cdn_empresa and
                         func_categ_sal.cdn_estab            = func_turno_trab.cdn_estab and
                         func_categ_sal.cdn_funcionario      = func_turno_trab.cdn_funcionario and
                         func_categ_sal.dat_inic_lotac_func <= tt-aux-param.v_dat_valid and
                         func_categ_sal.dat_fim_lotac_func  >= tt-aux-param.v_dat_valid and
                       ((func_categ_sal.cdn_categ_sal     = 1 and tt-aux-param.l-mensal    = yes)  or
                        (func_categ_sal.cdn_categ_sal     = 2 and tt-aux-param.l-horista   = yes)  or
                        (func_categ_sal.cdn_categ_sal     = 3 and tt-aux-param.l-semanal   = yes)  or
                        (func_categ_sal.cdn_categ_sal     = 4 and tt-aux-param.l-quinzenal = yes)  or
                        (func_categ_sal.cdn_categ_sal     = 5 and tt-aux-param.l-tarefa    = yes)  or
                        (func_categ_sal.cdn_categ_sal     = 6 and tt-aux-param.l-diarista  = yes)),
                    each funcionario use-index fncnr_py08504 of func_categ_sal no-lock where
                         funcionario.nom_pessoa_fisic >= tt-aux-param.v_nom_func_ini          and
                         funcionario.nom_pessoa_fisic <= tt-aux-param.v_nom_func_fim          and
                         funcionario.cod_unid_lotac   >= tt-aux-param.v_cod_unid_lotac_ini    and 
                         funcionario.cod_unid_lotac   <= tt-aux-param.v_cod_unid_lotac_fim    and                         
                         funcionario.cdn_local_pagto  >= tt-aux-param.cdn_local_pagto_ini     and
                         funcionario.cdn_local_pagto  <= tt-aux-param.cdn_local_pagto_fim     and  
                       ((tt-aux-param.r-forma-pgto        = 4                                 and
                         funcionario.idi_forma_pagto <> 0)                               or
                         funcionario.idi_forma_pagto = tt-aux-param.r-forma-pgto )
                    break by if i-ord-aux = 1 or 
                                i-ord-aux = 2 
                             then funcionario.idi_forma_pagto
                             else 0
                          by if i-ord-aux <> 15 and
                                i-ord-aux <> 16                                
                             then funcionario.cdn_estab
                             else ""
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
                                 tt-aux-param.num_classif_terc = 1 then
                                 string(funcionario.cdn_funcionario,"99999999")
                              else
                                 funcionario.nom_pessoa_fisic
                           by funcionario.idi_orig_contratac_func
                           by funcionario.cdn_prestdor_serv :


                        run pi-acompanhar in v_han_acomp (input STRING(i-contador) + " - Func: " + STRING(funcionario.cdn_funcionario)). 

                        if first-of(funcionario.cdn_prestdor_serv) and
                          tt-aux-param.num_razao_social = 2 then do:
                          find prestdor_serv no-lock where
                               prestdor_serv.cdn_empresa = funcionario.cdn_empresa and
                               prestdor_serv.cdn_prestdor_serv = funcionario.cdn_prestdor_serv no-error.
                          if avail prestdor_serv then
                             assign c-empresa = string(funcionario.cdn_prestdor_serv, "zzzzz9") + " - " +
                                                prestdor_serv.nom_pessoa.
                          else do:
                             find empresa no-lock where
                                  empresa.ep-codigo = tt-aux-param.v_cdn_empres_usuar no-error.
                             assign c-empresa = string(funcionario.cdn_empresa, "zz9") + " - " +
                                                empresa.razao-social.

                          end.
                          page {&STREAM}.                      
                       end.

                        if v_log_folha_educnal then do:

                          if funcionario.cdn_tip_contrat_func <> 0 then
                             next.

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

                        if funcionario.idi_forma_pagto       <> 3 then do:
                           if funcionario.cdn_bco_liq         < tt-aux-param.i-bc-codigo-1  or
                              funcionario.cdn_bco_liq         > tt-aux-param.i-bc-codigo-2  or
                              funcionario.cdn_agenc_bcia_liq  < tt-aux-param.i-ag-codigo-1  or
                              funcionario.cdn_agenc_bcia_liq  > tt-aux-param.i-ag-codigo-2  then next.
                        end.              

                        if tt-aux-param.i-mes-ref = 12 then do:
                          if  funcionario.dat_desligto_func <> ?                                           and
                             (funcionario.dat_desligto_func <= (date(01,01,(tt-aux-param.i-ano-ref + 1)) - 1)) and
                              tt-aux-param.l-emite-demi          = no then next.
                        end.
                        else do:
                          if  funcionario.dat_desligto_func <> ?                                                           and
                             (funcionario.dat_desligto_func <= (date((tt-aux-param.i-mes-ref + 1),01,tt-aux-param.i-ano-ref) - 1)) and
                             tt-aux-param.l-emite-demi = no then next.
                        end.


                        if not v_log_folha_educnal then do:
                           /***** n∆o substituir o find abaixo pela chamada da procedure pi_histor_sal_func *******/
                           find last histor_sal_func no-lock where
                                   histor_sal_func.cdn_empresa      = funcionario.cdn_empresa and
                                   histor_sal_func.cdn_estab        = funcionario.cdn_estab       and
                                   histor_sal_func.cdn_funcionario  = funcionario.cdn_funcionario and
                                   histor_sal_func.dat_liber_sal   <= tt-aux-param.v_dat_valid no-error. 

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
                                            histor_sal_func.dat_liber_sal   <= tt-aux-param.v_dat_valid no-error. 
                                  assign v_log_sem_salario = yes.                                            
                              end.
                              else do:
                                  /***** n∆o substituir o find abaixo pela chamada da procedure pi_histor_sal_func *******/
                                  find last histor_sal_func no-lock where
                                            histor_sal_func.cdn_empresa      = bfuncionario.cdn_empresa and
                                            histor_sal_func.cdn_estab        = bfuncionario.cdn_estab       and
                                            histor_sal_func.cdn_funcionario  = bfuncionario.cdn_funcionario and
                                            histor_sal_func.dat_liber_sal   <= tt-aux-param.v_dat_valid no-error.                               
                              end.
                           end.

                        end.


                       /*if avail histor_sal_func then           */
                         assign v_cdn_cargo_basic = histor_sal_func.cdn_cargo_basic
                                v_cdn_niv_cargo   = histor_sal_func.cdn_niv_cargo
                                v_val_sal_mensal  = histor_sal_func.val_salario_mensal 
                                v_val_sal_hora    = histor_sal_func.val_salario_hora.

                        assign l-tem-movto     = no
                               d-base-iapas    = 0
                               d-base-fgts     = 0
                               d-valor-fgts    = 0
                               d-base-irrf     = 0
                               d-liquido-pagar = 0
                               d-total-vencto  = 0
                               d-total-descto  = 0
                               i-cont-linha    = 0
                               d-salar-atual   = histor_sal_func.val_salario_categ.

                        for each movto_calcul_func of funcionario no-lock where
                                 movto_calcul_func.num_ano_refer_fp  = tt-aux-param.i-ano-ref and
                                 movto_calcul_func.num_mes_refer_fp  = tt-aux-param.i-mes-ref and
                                 movto_calcul_func.idi_tip_fp = tt-aux-param.i-tipo-folha and
                                 movto_calcul_func.qti_parc_habilit_calc_fp    = tt-aux-param.i-parcela:

                            run pi-acompanhar in v_han_acomp (input STRING(i-contador) + " - Func: " + STRING(funcionario.cdn_funcionario) + " - DT.Atualiz: " + STRING(movto_calcul_func.dat_ult_atualiz)). 

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

                               run pi-acompanhar in v_han_acomp (input STRING(i-contador) + " - Func: " + STRING(funcionario.cdn_funcionario) + " - DT.Atualiz: " + STRING(movto_calcul_func.dat_ult_atualiz) + " - " + STRING(i-inx)). 

                               assign i-inx = i-inx + 1.
                               if  i-inx > movto_calcul_func.qti_efp then
                                  leave.
                               if movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 20 and
                                  movto_calcul_func.val_calcul_efp[i-inx] = 0 then
                                  next.
                               if movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 56 and
                                  movto_calcul_func.val_calcul_efp[i-inx] = 0 then
                                  next.
                               find FIRST event_fp no-lock where
                                    event_fp.cdn_event_fp = movto_calcul_func.cdn_event_fp[i-inx].
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
                               if movto_calcul_func.cdn_idx_efp_funcao_espcif[i-inx] = 17 then
                                  assign d-base-iapas = movto_calcul_func.val_base_calc_fp[i-inx].
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
/*                                       w-mvtocalc.ev-codigo   = int(event_fp.cdn_event_fp) */
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

                               if v_log_folha_educnal then do:
                                  if w-mvtocalc.identif = 1 then do:
                                     if v_log_sem_salario then
                                        assign d-salar-atual = 0.
                                  end.
                               end.
                            end.
                        end.
                        if  not l-tem-movto then
                           next.
                        find habilit_calc_fp use-index hbltclcf_py04002 no-lock where
                             habilit_calc_fp.cdn_empresa    = funcionario.cdn_empresa and
                             habilit_calc_fp.cdn_estab      = funcionario.cdn_estab and
                             habilit_calc_fp.cdn_categ_sal  = func_categ_sal.cdn_categ_sal and
                             &if "{&dthrpyc_version}" >= "2.06" &then
                                habilit_calc_fp.idi_orig_contratac_func  = funcionario.idi_orig_contratac_func and
                                habilit_calc_fp.cdn_prestdor_serv        = funcionario.cdn_prestdor_serv       and
                             &endif
                             habilit_calc_fp.idi_tip_fp = tt-aux-param.i-tipo-folha  and
                             habilit_calc_fp.num_ano_refer_fp_calcula  = tt-aux-param.i-ano-ref     and
                             habilit_calc_fp.num_mes_refer_fp_calcula  = tt-aux-param.i-mes-ref     and
                             habilit_calc_fp.qti_parc_habilit_calc_fp  = tt-aux-param.i-parcela
                             no-error.
                        if  available habilit_calc_fp then
                           assign c-mens-contr[1]=substring(habilit_calc_fp.des_msg_envel_fp,1,40)
                                  c-mens-contr[2]=substring(habilit_calc_fp.des_msg_envel_fp,41,38).

                        find cargo_basic no-lock where
                             cargo_basic.cdn_cargo_basic = v_cdn_cargo_basic.

                        find local_pagto_func no-lock where
                             local_pagto_func.nom_local_pagto = funcionario.cdn_local_pagto no-error.
                        if funcionario.cdn_local_pagto <> 0 
                        then assign v_cdn_local = string(funcionario.cdn_local_pagto).
                        else assign v_cdn_local = string(funcionario.cdn_estab).
                        assign i-index     =  func_categ_sal.cdn_categ_sal 
                               c-categoria = {database/inpy/i03py029.i 04 func_categ_sal.cdn_categ_sal}
                               c-mes-folha = c-lit-mes[tt-aux-param.i-mes-ref]
                               c-parabens  =
                                      if  tt-aux-param.i-tipo-folha = 1 and
                                          (month(funcionario.dat_nascimento) = (tt-aux-param.i-mes-ref + 1) or
                                           (month(funcionario.dat_nascimento) = 1 and
                                            tt-aux-param.i-mes-ref = 12))
                                      then "* * *  F e l i z   A n i v e r s a r i o  * * *"
                                      else "".
                        if  funcionario.dat_desligto_func <> ? and
                            (year(funcionario.dat_desligto_func) < tt-aux-param.i-ano-ref or
                             (year(funcionario.dat_desligto_func) = tt-aux-param.i-ano-ref and
                             month(funcionario.dat_desligto_func) <= tt-aux-param.i-mes-ref)) then
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
                                  c-dig-conta    = funcionario.cod_digito_cta_corren
                                  i-cod-banco    = funcionario.cdn_bco_liq
                                  i-cod-agencia  = funcionario.cdn_agenc_bcia_liq.
                           find rh_bco   no-lock where
                                rh_bco.cdn_banco = funcionario.cdn_bco_liq.
                           find rh_agenc_bcia of rh_bco no-lock where
                                rh_agenc_bcia.cdn_agenc_bcia = funcionario.cdn_agenc_bcia_liq.
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

                        ASSIGN d-consignacao = 0.
                        for each w-mvtocalc NO-LOCK:
                        
                            if w-mvtocalc.ev-codigo = '461' then 
                               ASSIGN d-consignacao = w-mvtocalc.valor.

                            if w-mvtocalc.inc-liquido = 1 then
                               assign d-total-vencto = d-total-vencto + w-mvtocalc.valor.
                            ELSE if  w-mvtocalc.inc-liquido = 2 then
                               ASSIGN d-total-descto = d-total-descto + w-mvtocalc.valor.
                             
                        END.

                        /**PUT FILL("1234567890",8) FORMAT "x(150)" AT 01.**/
                        for each w-mvtocalc break by w-mvtocalc.fc-codigo 
                                                  by w-mvtocalc.identif
                                                  by w-mvtocalc.ev-codigo:

                            if  first-of(w-mvtocalc.fc-codigo) OR i-cont-linha > 22 then do:
                               if  i-cont-linha > 22 then do:
                                  do while i-cont-linha < 24:

                                     put "" at 01 skip.

                                     assign i-cont-linha = i-cont-linha + 1.
                                  end.

                                  {utp/ut-liter.i Continua... *}
                                  
                                   if  first-of(w-mvtocalc.fc-codigo) then 
                                 put  Return-value  at 58  skip(07).
                                 
                                 else 
                                 put  Return-value  at 58  skip(05).

                               end. 
                               assign l-imprime = yes.
                                 
                               FIND FIRST rh_pessoa_jurid NO-LOCK 
                                    WHERE rh_pessoa_jurid.num_pessoa_jurid = rh_estab.num_pessoa_jurid
                                    NO-ERROR.
                               FIND FIRST unid_lotac NO-LOCK
                                    WHERE unid_lotac.cod_unid_lotac = funcionario.cod_unid_lotac
                                    NO-ERROR.

                               FIND FIRST cargo NO-LOCK
                                    WHERE cargo.cdn_cargo_basic =   cargo_basic.cdn_cargo_basic
                                    NO-ERROR.

                               
                               /*raimundo*/
                       FIND FIRST func_emprest_financ OF funcionario 
                            NO-LOCK NO-ERROR.

                       find rh_pessoa_fisic of funcionario no-lock.

                       FIND rh_ccusto WHERE rh_ccusto.cdn_empresa   = funcionario.cdn_empresa
                                       AND rh_ccusto.cod_rh_ccusto = funcionario.cod_rh_ccusto NO-LOCK NO-ERROR.

                       /*FIND FIRST rh_pessoa_jurid 
                            WHERE rh_pessoa_jurid.nom_pessoa_jurid = empresa.razao-social NO-LOCK NO-ERROR.*/

                       FIND FIRST rh_pessoa_jurid 
                            WHERE rh_pessoa_jurid.cod_id_feder = empresa.cgc NO-LOCK NO-ERROR.
                       /* Ini- Bisneto - Rotina de Enviar em PDF Zipado por Email */
                       FIND FIRST usuar_mestre
                         NO-LOCK
                         WHERE usuar_mestre.cod_usuario = c-seg-usuario
                         NO-ERROR.
/*                        IF AVAIL usuar_mestre THEN                     */
/*                          ASSIGN                                       */
/*                            c-ender = TRIM(usuar_mestre.nom_dir_spool) */
/*                            c-ender = REPLACE(c-ender,"/","\").        */
/*                        ELSE                                           */
                         ASSIGN c-ender = TRIM(SESSION:TEMP-DIRECTORY).


                       IF SUBSTRING(c-ender,LENGTH(c-ender),1) <> "\" THEN
                         ASSIGN c-ender = c-ender + "\".

                       ASSIGN c-temp-dir = c-ender.
                       /*---------------------------------------------*/
                       ASSIGN c-tipo-folha = "".
                       CASE tt-aux-param.i-tipo-folha:
                         WHEN 1 THEN
                             ASSIGN c-tipo-folha = "-Normal". 
                         WHEN 2 THEN
                             ASSIGN c-tipo-folha = "Adt-Normal". 
                         WHEN 3 THEN                         
                             ASSIGN c-tipo-folha = "-13-Sal†rio". 
                         WHEN 4 THEN                         
                             ASSIGN c-tipo-folha = "-Adt-13-Sal†rio". 
                       END CASE.
                       ASSIGN c-tipo-folha = "".
                       CASE tt-aux-param.i-tipo-folha:
                         WHEN 1 THEN
                             ASSIGN c-assunto = "Recibo Pagamento Mensal (" + STRING(tt-aux-param.v_dat_valid,"99/99/9999") + ")".
                         WHEN 2 THEN
                             ASSIGN c-assunto = "Recibo Pagamento Adt Mensal ("  + STRING(tt-aux-param.v_dat_valid,"99/99/9999") + ")".
                         WHEN 3 THEN                         
                             ASSIGN c-assunto = "Recibo Pagamento 13. Sal†rio ("  + STRING(tt-aux-param.v_dat_valid,"99/99/9999") + ")".
                         WHEN 4 THEN                         
                             ASSIGN c-assunto = "Recibo Pagamento Adt. 13-Sal†rio (" + STRING(tt-aux-param.v_dat_valid,"99/99/9999") + ")".
                       END CASE.
                       /*---------------------------------------------*/
                       ASSIGN 
                         c-nome-arq-pdf   = "" 
                         c-nome-arq-zip   = ""
                         tt-param.arquivo = "".
                       FIND FIRST tt-param NO-ERROR.
                       IF AVAIL tt-param THEN
                         ASSIGN
                           c-nome-arq-pdf          = 
                             "RecPag" +
                             TRIM(STRING(funcionario.cdn_empresa)) +
                             TRIM(STRING(funcionario.cdn_estab)) +
                             TRIM(STRING(funcionario.cdn_funcionario))
                           c-nome-arq-zip          = c-ender + c-nome-arq-pdf + c-tipo-folha
                           c-nome-arq-pdf          = c-ender + c-nome-arq-pdf + c-tipo-folha
                           tt-param.i-fc-ini       = funcionario.cdn_funcionario
                           tt-param.i-fc-fim       = funcionario.cdn_funcionario
                           tt-param.v_nom_func_ini = ""
                           tt-param.v_nom_func_fim = "Zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"
                           c-nome-arq-pdf          = c-nome-arq-pdf + ".pdf"
                           tt-param.arquivo-pdf    = c-nome-arq-pdf
                           c-nome-arq-zip          = c-nome-arq-zip + ".zip".

                       ASSIGN c-nome-arq-pdf-aux = c-nome-arq-pdf.

                       IF tt-param.l-email-pdf = YES THEN
                       DO:
                           RUN pi-inicializar IN v_han_acomp (INPUT "Processando, FP3500R7...").

                           RUN prghur/fpp/fp3500r7.p ( INPUT ROWID(funcionario)).
                           ASSIGN l-erro = NO.

                           RUN pi-inicializar IN v_han_acomp (INPUT "Processando, pi-zipa-pdf...").

                           RUN pi-zipa-pdf(OUTPUT l-erro).
                           IF l-erro = YES THEN
                             LEAVE blk_digita.
                           ASSIGN l-erro = NO.


                           RUN pi-inicializar IN v_han_acomp (INPUT "Processando, pi-envia-zip-email...").
                           RUN pi-envia-zip-email.
                       END.
                       /* Fim - Bisneto - Rotina de Enviar em PDF Zipado por Email */
                       /* Impressa0 */
                       PUT /*SKIP*/  /*                            cargo.des_envel_pagto               at  65 */
                           substr(empresa.razao-social,1,40)  FORMAT "x(40)" AT  01 
                           /*rh_pessoa_jurid.cod_id_feder                         AT  48*/ 
                           empresa.cgc                                          AT  48
                           funcionario.nom_pessoa_fisic  FORMAT "X(33)"          AT  01 
                           funcionario.cdn_funcionario                           AT  35
                           /*funcionario.cod_unid_lotac    FORMAT "X(10)"          AT  46*/

                            substring(trim(c-mes-folha),1,3)   FORMAT "x(3)"           AT  60
                           "/"                                 
                           tt-aux-param.i-ano-ref.                   

                       /* banco/agenc/ccorrente */
                       PUT funcionario.cdn_bco_liq                                         AT 01
                           funcionario.cdn_agenc_bcia_liq                                  AT 08
                           funcionario.cdn_cta_corren                                      AT 20
                           "-"                                                             AT 29
                           funcionario.cod_digito_cta_corren                               AT 30

                           /*SKIP (1)*/

                           rh_pessoa_fisic.cod_id_feder     FORMAT "x(20)"                     AT 01 /* "cpf"                   */ 
                           funcionario.qti_depend_irf                            AT 30 /* "Dependente I renda"    */ 
                           funcionario.qti_depend_salfam                         AT 42 /* "Depende S.F. Legal"    */ 

                           substr(funcionario.cod_rh_ccusto,1,14) FORMAT "x(14)" AT 01  /* "Centro de custo"   - cida - pula 1 linha    */ 
                           rh_ccusto.des_rh_ccusto                               AT 15  /* desc ccusto */


                            /*SKIP*/ 

                           cargo.des_cargo                 FORMAT "X(20)"        AT 05
                           d-salar-atual format ">>>>>,>>9.99"                   AT 33  /* "Salario Basico"      */ 
                           d-base-iapas  format ">>>>>,>>9.99"                   AT 59  /* "Salario contra Inss" */ /*nao tenho certeza*/

                           SKIP(2).

                           PUT
                             d-base-irrf                                         AT 01 /* "Base de C†lculo IRRF"          */
                             d-consignacao                                       at 25.
                             
                             
                       /* IF AVAIL func_emprest_financ THEN */
/*                           PUT func_emprest_financ.val_dispon_emprest              AT 25 /* "Margem consignaá∆o"            */. */
/*                        ELSE PUT  0.*/
                       
                         ASSIGN d-margem =  d-salar-atual * 0.3.
                           PUT 
                             d-base-fgts                                         AT 40 /* "Base calculo FGTS"             */
                             d-valor-fgts                                        AT 59 /* "FGTS MES"                      */

                             SKIP (1).

                       if funcionario.cdn_estab = "1" or funcionario.cdn_estab = "2" then 
                         put d-margem                                            AT 08 /* "Proximo adiantamento ???"  fixo 30% do salario atual     */.
                       else 
                       put "" at 08.
                             
                         put d-total-vencto                                      AT 20 /* "total pagto "                  */
                             d-total-descto                                      AT 40 /* "total descto"                  */
                             d-liquido-pagar                                     AT 58. /* "Liquido a receber"             */

                         /*SKIP(1).*/
                        

                               assign i-cont-linha = 7.
                            end.


                            put trim(w-mvtocalc.ev-codigo)   at  01 FORMAT "X(05)".
                       
                            if  w-mvtocalc.horas > 0 then
                            put w-mvtocalc.horas    at 8. /*frequencia*/
                            
                            PUT w-mvtocalc.descricao   at  17.
                            
                            if w-mvtocalc.inc-liquido = 1 THEN assign c-inc-liquido = "+".
                            if w-mvtocalc.inc-liquido = 2 then assign c-inc-liquido = "-".
                            if w-mvtocalc.inc-liquido = 3 then assign c-inc-liquido = " ".
                            if w-mvtocalc.inc-liquido = 4 then assign c-inc-liquido = "P".
                            if w-mvtocalc.inc-liquido = 5 then assign c-inc-liquido = "N".
                            
                            if  w-mvtocalc.inc-liquido = 2 then
                                put w-mvtocalc.valor       AT 57   
                                    c-inc-liquido FORMAT "X".
                            else
                               put w-mvtocalc.valor       at  57 
                                   c-inc-liquido FORMAT "X".

/*                             if  w-mvtocalc.inc-liquido = 2 THEN   */
/*                                put w-mvtocalc.valor       TO 115  */
/*                                    c-inc-liquido          AT 116. */
/*                             else                                  */
/*                                put w-mvtocalc.valor       TO  90  */
/*                                    c-inc-liquido          at  91. */

                            assign i-cont-linha = i-cont-linha + 1.
                            
                            if  last-of(w-mvtocalc.fc-codigo) then do:

                               do  while i-cont-linha < 23:
                                  put "" at 01 SKIP.
                                  assign i-cont-linha = i-cont-linha + 1.
                               end.

                               

                               PUT c-parabens            at 01
                                   c-mensagem[1]         
                                   c-mensagem[2]         at 01
                                   skip(4). 

                            end.
                            delete w-mvtocalc.
                        end.
                      
                    end.
                end.
            end.
    end.

/* {include/i-rpclo.i} */
RUN pi-inicializar IN v_han_acomp (INPUT "Processando, Envio de Email...").
IF tt-param.l-email-pdf = YES THEN
DO:
   RUN pi-envia-todos-emails(OUTPUT l-erro).
END.

return "ok".

PROCEDURE pi-zipa-pdf:
  DEF OUTPUT PARAM l-erro-pdf AS LOG NO-UNDO.
  ASSIGN l-erro-pdf = NO.
  FOR EACH tt-listFiles: DELETE tt-listFiles. END.
  FOR EACH tt-erros-zip: DELETE tt-erros-zip. END.   
  ASSIGN 
    c_anexo = c-nome-arq-zip
    c_senha = TRIM(rh_pessoa_fisic.cod_id_feder).
  CREATE tt-listFiles.
  ASSIGN 
    tt-listFiles.cFile = c-nome-arq-pdf
    c_status_zip       = "OK".
/*   RUN utp/ut-zip.p PERSISTENT SET h-zip. */
  IF VALID-HANDLE(h-zip) THEN
    DO:
      RUN zipFilesEncrypt IN h-zip (
        INPUT c_anexo,
        INPUT TABLE tt-listFiles,
        INPUT c_senha,
        INPUT TRUE,
        OUTPUT TABLE tt-erros-zip).
      ASSIGN c_status_zip = RETURN-VALUE.
      FOR EACH tt-erros-zip:
        ASSIGN l-erro-pdf = YES.

        IF tt-param.l-email-pdf AND tt-param.tipo-execucao = 1 THEN
        MESSAGE "ZIP: " tt-erros-zip.cod-erro tt-erros-zip.desc-erro
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      END.
    END.  
END PROCEDURE.

PROCEDURE pi-envia-zip-email:

  ASSIGN cArquivoFinal = "" /* arquivo para anexar */ /* tt-param.arquivo-pdf */.

  IF AVAIL rh_pessoa_fisic THEN 
    ASSIGN c-destino-email =  rh_pessoa_fisic.nom_e_mail.

  FIND FIRST tt-email WHERE 
             tt-email.destino           = c-destino-email AND
             tt-email.arq-anexo         = c-nome-arq-zip  EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL tt-email THEN
  DO:             
      CREATE tt-email.
      ASSIGN tt-email.versao-integracao = 1
             tt-email.exchange          = param_glob_rh.log_livre_1
             tt-email.servidor          = trim(substr(param_glob_rh.cod_livre_2,01,40))
             tt-email.porta             = param_glob_rh.num_livre_1
             tt-email.destino           = c-destino-email
             tt-email.assunto           = c-assunto
             tt-email.remetente         = c-email-empresa
             tt-email.copia             = "" /* tt-param.c-arquivo-copia */
             tt-email.mensagem          = tt-param.texto-email /* "":U */
             tt-email.importancia       = 1
             tt-email.log-enviada       = NO 
             tt-email.log-lida          = NO
             tt-email.acomp             = NO
             tt-email.arq-anexo         = c-nome-arq-zip.
  END.

END PROCEDURE.
PROCEDURE pi-envia-todos-emails:

  DEF OUTPUT PARAM l-erro-zip AS LOG NO-UNDO.

  DEFINE VARIABLE i-cont-email AS INTEGER   NO-UNDO.
  DEFINE VARIABLE i-cont-tot   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cDirTempAux  AS CHARACTER NO-UNDO.

  IF VALID-HANDLE(h-zip) THEN
    DELETE PROCEDURE h-zip.

  RUN utp/utapi019.p PERSISTENT SET h-utapi019.
  ASSIGN l-erro-zip = NO.
  /* API de envio eletrÀnico */    
  CREATE tt-mensagem.
  ASSIGN tt-mensagem.seq-mensagem = 1
         tt-mensagem.mensagem     = tt-param.texto-email.
 
  ASSIGN i-cont-email = 0.

  FOR EACH tt-email:

    ASSIGN i-cont-email = i-cont-email + 1.

  END.

  ASSIGN i-cont-tot = i-cont-email. 

  FOR EACH tt-erros-aux: DELETE tt-erros-aux. END.


  FOR EACH tt-email
        BY tt-email.destino:

    run pi-acompanhar in v_han_acomp (input "Restam: " + STRING(i-cont-email) + " de --> " + STRING(i-cont-tot)).

    EMPTY TEMP-TABLE tt-envio2.
    EMPTY TEMP-TABLE tt-erros.
    
    CREATE tt-envio2.
    BUFFER-COPY tt-email TO tt-envio2.

    RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                   INPUT  TABLE tt-mensagem,
                                   OUTPUT TABLE tt-erros).

    IF CAN-FIND(FIRST tt-erros) THEN
    DO:
       CREATE tt-erros-aux.
       BUFFER-COPY tt-erros         TO tt-erros-aux
                                ASSIGN tt-erros-aux.email = tt-email.destino. 

       ASSIGN l-erro-zip = YES.
    END.           

    ASSIGN i-cont-email = i-cont-email - 1.    

  END.

  IF CAN-FIND(FIRST tt-erros-aux) AND tt-param.tipo-execucao = 2 THEN
  DO:
     PUT UNFORMATTED "Erros de Integraá∆o" FORMAT "X(20)" SKIP
                     "Email                                                          Codigo       Descriá∆o" SKIP
                     "-------------------------------------------------------------- -----------  ---------------------------------------------------------------------------------------" SKIP.

     FOR EACH tt-erros-aux:
         
         PUT UNFORMATTED  tt-erros-aux.email                        AT 001 FORMAT "X(62)"
                          tt-erros-aux.cod-erro                     AT 064 FORMAT ">>>,>>>,>>9"
                          SUBSTRING(tt-erros-aux.desc-erro,001,87)  AT 077 FORMAT "X(87)" SKIP
                          SUBSTRING(tt-erros-aux.desc-erro,088,175) AT 077 FORMAT "X(87)" SKIP
                          SUBSTRING(tt-erros-aux.desc-erro,176,256) AT 077 FORMAT "X(87)" SKIP.
     END.
  END.
  
  /* Begins Rotina para eliminar os arquivos gerados no diret¢rio tempor†rio do rpw */
  IF SEARCH(c-nome-arq-pdf-aux) <> ? THEN
  DO: 
      ASSIGN c-ender    = c-ender + "RecPag*.*".
    
      DOS SILENT DEL VALUE(c-ender).
    
      ASSIGN c-temp-dir = SESSION:TEMP-DIRECTORY + "dvrender*.*".
    
      DOS SILENT DEL VALUE(c-temp-dir).
        
      ASSIGN cDirTempAux = "rmdir " + SESSION:TEMP-DIRECTORY  + "dv3 /s / q".
        
      DOS SILENT VALUE(cDirTempAux). 
  END.
  /* end Rotina */

  DELETE PROCEDURE h-utapi019.  
END PROCEDURE.




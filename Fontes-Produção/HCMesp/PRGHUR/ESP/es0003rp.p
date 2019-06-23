/*****************************************************************************
*   Programa: es0003rp.p
*   Data....: abril/06
*   Autor...: Raimundo C. Soares
*   Cliente : Fazenda Brasileiro
*   Objetivo: Relat¢rio de ProvisÆo de F‚rias
*   VersÆo..: 2.09.001                            
*   OBS.....: 
*******************************************************************************/
/*---------------- Include de controle de Versão ------------------*/ 
{include/buffers_RH.i}

{include/i-prgvrs.i es0003rp 2.09.00.000}

/******** Definição Temp-table para Recebimento de Parametro **********************/
def temp-table tt-raw-digita
    field raw-digita as raw.

DEF BUFFER b-movto_calcul_func FOR movto_calcul_func.


define temp-table tt-param no-undo
    field destino              as integer
    field arquivo              as char format "x(35)"
    field usuario              as char format "x(12)"
    field v_cdn_empres_usuar   like param_empres_rh.cdn_empresa
    field v_num_tip_aces_usuar as integer format "9" 
    field data-exec            as date
    field hora-exec            as integer
    field classific            as integer
    field i-empresa            as char
    field i-est-ini            like funcionario.cdn_estab
    field i-est-fim            like funcionario.cdn_estab
    FIELD i-mes-ref            AS INTEGER
    FIELD i-ano-ref            AS INTEGER
    field i-tipo               as INTEGER.

DEF VAR i-cont AS INT NO-UNDO.    

DEF TEMP-TABLE tt-dados NO-UNDO
    FIELD cdn_funcionario  LIKE   movto_calcul_func.cdn_funcionario
    FIELD nom_pessoa_fisic like   funcionario.nom_pessoa_fisic
    FIELD cdn_empresa      LIKE   movto_calcul_func.cdn_empresa 
    FIELD cdn_estab        LIKE   movto_calcul_func.cdn_estab   
    FIELD i-mes            AS INT
    FIELD i-ano            AS INT
    FIELD centro-custo     LIKE   movto_calcul_func.cod_rh_ccusto
    FIELD vl-inicial       AS DEC
    FIELD vl-910           AS DEC
    FIELD vl-912           AS DEC
    FIELD vl-914           AS DEC
    FIELD vl-916           AS DEC
    FIELD vl-918           AS DEC
    FIELD vl-saldo-final   AS DEC.
    

/****************** Defini‡ao de Vari veis do Relat¢rio NÆo Pedidas em Tela ******************/ 
def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.


create tt-param.
raw-transfer raw-param to tt-param.


/****************** Definiçao de Variáveis do Relatório Não Pedidas em Tela ******************/ 
DEF VAR d-vl-inicial            AS DEC NO-UNDO FORMAT "->>,>>>,>>9.99".
DEF VAR d-total-vl-inicial      AS DEC NO-UNDO FORMAT "->>,>>>,>>9.99".
DEF VAR d-total-vl-910          AS DEC NO-UNDO FORMAT "->>,>>>,>>9.99".
DEF VAR d-total-vl-912          AS DEC NO-UNDO FORMAT "->>,>>>,>>9.99".
DEF VAR d-total-vl-914          AS DEC NO-UNDO FORMAT "->>,>>>,>>9.99".
DEF VAR d-total-vl-916          AS DEC NO-UNDO FORMAT "->>,>>>,>>9.99".
DEF VAR d-total-vl-918          AS DEC NO-UNDO FORMAT "->>,>>>,>>9.99".
DEF VAR d-total-vl-saldo-final  AS DEC NO-UNDO FORMAT "->>,>>>,>>9.99".
DEF VAR c-date-ref AS DATE NO-UNDO.
DEF VAR i-mes-anterior AS INT NO-UNDO.
DEF VAR i-ano-anterior AS INT NO-UNDO.
DEF VAR d-f-risco      AS DEC NO-UNDO.


/****************** Definiçao de Variáveis de Processamento do Relatório *********************/

def var h-acomp              as handle no-undo.
def var v-cod-destino-impres as char   no-undo.

/****************** Definiçao de  Frames e Forms do Relatório 132 Colunas ***************************************/ 
/*-------- include padrão para variaveis de relatorio--------- */
  {include/i-rpvar.i}
/*------------------------------------------------------------ */
 DEF VAR c-periodo AS CHAR FORMAT "x(7)" NO-UNDO .
 FIND FIRST tt-param NO-ERROR.

FORM HEADER 
     "Referente:" c-periodo FORMAT "x(7)" SKIP (1)

    WITH NO-LABELS STREAM-IO PAGE-TOP FRAME f-periodo.

find empresa no-lock where
     empresa.ep-codigo = tt-param.v_cdn_empres_usuar no-error.

FIND param_empres_rh WHERE param_empres_rh.cdn_empresa = tt-param.v_cdn_empres_usuar NO-LOCK NO-ERROR.

assign c-programa     = "es0003RP"
       c-versao       = "2.09"
       c-revisao      = "000"
       c-titulo-relat = "Relat¢rio de ProvisÆo de F‚rias "
       c-sistema      = "Folha de Pagamento"
       c-empresa      = empresa.razao-social.

/*---------------- Abertura do arquivo de saida ----------------------*/

/*------------ Definição parametro &stream para os includes
               da frame de cabeçalho e rodape para 132 colunas ---------*/
{include/i-rpcab.i}
/*--------------------------------------------------------------------*/

/*------------ Include Padrao para output de relatorios --------------*/
{include/i-rpout.i}
/*--------------------------------------------------------------------*/

view frame f-cabec.
view frame f-rodape.
VIEW FRAME f-periodo.

ASSIGN c-periodo = string(tt-param.i-mes-ref, "99") + "/" + string(tt-param.i-ano-ref, "9999") .

run utp/ut-acomp.p persistent set h-acomp.

{utp/ut-liter.i Imprimindo * I}

run pi-inicializar in h-acomp (input "Processando").  

EMPTY TEMP-TABLE tt-dados.

FIND FIRST tt-param NO-ERROR.

RUN pi-acha-dados.
RUN pi-inicial.

IF tt-param.i-tipo  = 1 THEN
   RUN pi-funcionario.

ELSE IF tt-param.i-tipo  = 2 THEN
   RUN pi-centro-custo.

ELSE 
   RUN pi-total.
/*----------------- Fechamento do output do relatorio -----------------*/
{include/i-rpclo.i}

run pi-finalizar in h-acomp.

return "OK":U.

PROCEDURE pi-acha-dados:

   c-date-ref = DATE ( tt-param.i-mes-ref, 01,  tt-param.i-ano-ref).
   c-date-ref  = c-date-ref - 1 .
   
   IF tt-param.i-empresa  = "201" THEN
       d-f-risco = (0.268 ).
   ELSE 
       d-f-risco = (0.288).

   ASSIGN i-mes-anterior     = MONTH(c-date-ref)
          i-ano-anterior     = YEAR(c-date-ref).

   FIND FIRST tt-param.
   FOR EACH  movto_calcul_func
       WHERE movto_calcul_func.cdn_empresa        =  tt-param.i-empresa
       AND   movto_calcul_func.cdn_estab         >= tt-param.i-est-ini 
       AND   movto_calcul_func.cdn_estab         <= tt-param.i-est-fim 
       AND   movto_calcul_func.num_ano_refer_fp   = tt-param.i-ano-ref
       AND   movto_calcul_func.num_mes_refer_fp   = tt-param.i-mes-ref
       AND   movto_calcul_func.idi_tip_fp         = 1
       NO-LOCK:
   
       FIND FIRST funcionario OF movto_calcul_func NO-LOCK NO-ERROR.
   
       run pi-acompanhar in h-acomp(input "Lendo Funcion rio: " + STRING(movto_calcul_func.cdn_funcionario)).
   
       FIND FIRST tt-dados
            WHERE tt-dados.cdn_funcionario   = movto_calcul_func.cdn_funcionario
            NO-ERROR.
   

       IF NOT AVAIL tt-dados THEN DO:
          CREATE tt-dados.
          ASSIGN tt-dados.cdn_funcionario   = movto_calcul_func.cdn_funcionario   
                 tt-dados.centro-custo      = movto_calcul_func.cod_rh_ccusto     
                 tt-dados.i-mes             = movto_calcul_func.num_ano_refer_fp  
                 tt-dados.i-ano             = movto_calcul_func.num_mes_refer_fp 
                 tt-dados.cdn_empresa       = movto_calcul_func.cdn_empresa 
                 tt-dados.cdn_estab         = movto_calcul_func.cdn_estab   
                 tt-dados.nom_pessoa_fisic  = IF AVAIL funcionario THEN funcionario.nom_pessoa_fisic ELSE "NÆo cadastrado". 
       END.
   
       ASSIGN i-cont = 1.
       IF tt-param.i-empresa = "211" THEN DO:
          IF movto_calcul_func.cdn_estab = "211" THEN
          ASSIGN d-f-risco = 0.268.
       END.


       DO WHILE i-cont <= 30:
          
          CASE movto_calcul_func.cdn_event_fp[i-cont]: 
               WHEN "910" THEN ASSIGN tt-dados.vl-910 = tt-dados.vl-910 + movto_calcul_func.val_calcul_efp [i-cont].
               WHEN "912" THEN ASSIGN tt-dados.vl-912 = tt-dados.vl-912 + movto_calcul_func.val_calcul_efp [i-cont].
               WHEN "914" THEN ASSIGN tt-dados.vl-914 = tt-dados.vl-914 + movto_calcul_func.val_calcul_efp [i-cont].
               WHEN "916" THEN ASSIGN tt-dados.vl-916 = tt-dados.vl-916 + movto_calcul_func.val_calcul_efp [i-cont].
                             
               WHEN "918" THEN ASSIGN tt-dados.vl-918 = tt-dados.vl-918 + movto_calcul_func.val_calcul_efp [i-cont].
          
               /*912*/
               WHEN "221" THEN ASSIGN tt-dados.vl-912 = tt-dados.vl-912 - movto_calcul_func.val_calcul_efp [i-cont]
                                    tt-dados.vl-916 = tt-dados.vl-916 -(movto_calcul_func.val_calcul_efp [i-cont]* d-f-risco).
                   
               WHEN "222" THEN ASSIGN tt-dados.vl-912 = tt-dados.vl-912 - movto_calcul_func.val_calcul_efp [i-cont]
                                    tt-dados.vl-916 = tt-dados.vl-916 -(movto_calcul_func.val_calcul_efp [i-cont]* d-f-risco).

               WHEN "224" THEN ASSIGN tt-dados.vl-912 = tt-dados.vl-912 - movto_calcul_func.val_calcul_efp [i-cont]
                                    tt-dados.vl-916 = tt-dados.vl-916 -(movto_calcul_func.val_calcul_efp [i-cont]* d-f-risco).

               WHEN "226" THEN ASSIGN tt-dados.vl-912 = tt-dados.vl-912 - movto_calcul_func.val_calcul_efp [i-cont]
                                    tt-dados.vl-916 = tt-dados.vl-916 -(movto_calcul_func.val_calcul_efp [i-cont]* d-f-risco).

               WHEN "230" THEN ASSIGN tt-dados.vl-912 = tt-dados.vl-912 - movto_calcul_func.val_calcul_efp [i-cont]
                                    tt-dados.vl-916 = tt-dados.vl-916 -(movto_calcul_func.val_calcul_efp [i-cont]* d-f-risco).

               WHEN "231" THEN ASSIGN tt-dados.vl-912 = tt-dados.vl-912 - movto_calcul_func.val_calcul_efp [i-cont]
                                    tt-dados.vl-916 = tt-dados.vl-916 -(movto_calcul_func.val_calcul_efp [i-cont]* d-f-risco).

               WHEN "233" THEN ASSIGN tt-dados.vl-912 = tt-dados.vl-912 - movto_calcul_func.val_calcul_efp [i-cont]
                                    tt-dados.vl-916 = tt-dados.vl-916 -(movto_calcul_func.val_calcul_efp [i-cont]* d-f-risco).

               WHEN "234" THEN ASSIGN tt-dados.vl-912 = tt-dados.vl-912 - movto_calcul_func.val_calcul_efp [i-cont]
                                    tt-dados.vl-916 = tt-dados.vl-916 -(movto_calcul_func.val_calcul_efp [i-cont]* d-f-risco).
          
               /*914*/
               WHEN "227" THEN ASSIGN tt-dados.vl-914 = tt-dados.vl-914 - movto_calcul_func.val_calcul_efp [i-cont]
                                    tt-dados.vl-916 = tt-dados.vl-916 -(movto_calcul_func.val_calcul_efp [i-cont]* d-f-risco).

               WHEN "229" THEN ASSIGN tt-dados.vl-914 = tt-dados.vl-914 - movto_calcul_func.val_calcul_efp [i-cont]
                                    tt-dados.vl-916 = tt-dados.vl-916 -(movto_calcul_func.val_calcul_efp [i-cont]* d-f-risco).

               WHEN "232" THEN ASSIGN tt-dados.vl-914 = tt-dados.vl-914 - movto_calcul_func.val_calcul_efp [i-cont]
                                    tt-dados.vl-916 = tt-dados.vl-916 -(movto_calcul_func.val_calcul_efp [i-cont]* d-f-risco). 

               WHEN "236" THEN ASSIGN tt-dados.vl-914 = tt-dados.vl-914 - movto_calcul_func.val_calcul_efp [i-cont]
                                    tt-dados.vl-916 = tt-dados.vl-916 -(movto_calcul_func.val_calcul_efp [i-cont]* d-f-risco).

               /*918*/
               WHEN "412" THEN ASSIGN tt-dados.vl-918 = tt-dados.vl-918 - movto_calcul_func.val_calcul_efp [i-cont].
               WHEN "551" THEN ASSIGN tt-dados.vl-918 = tt-dados.vl-918 - movto_calcul_func.val_calcul_efp [i-cont].
               WHEN "552" THEN ASSIGN tt-dados.vl-918 = tt-dados.vl-918 - movto_calcul_func.val_calcul_efp [i-cont].
          
               /*acumulado*/
               WHEN "925" THEN ASSIGN tt-dados.vl-saldo-final = tt-dados.vl-saldo-final + movto_calcul_func.val_calcul_efp [i-cont].
               WHEN "927" THEN ASSIGN tt-dados.vl-saldo-final = tt-dados.vl-saldo-final + movto_calcul_func.val_calcul_efp [i-cont].
               WHEN "926" THEN ASSIGN tt-dados.vl-saldo-final = tt-dados.vl-saldo-final + movto_calcul_func.val_calcul_efp [i-cont].
               WHEN "928" THEN ASSIGN tt-dados.vl-saldo-final = tt-dados.vl-saldo-final + movto_calcul_func.val_calcul_efp [i-cont].
          END CASE.
          
          ASSIGN i-cont = i-cont + 1.
       END.
   END.
END PROCEDURE.


PROCEDURE pi-funcionario:

    FOR EACH tt-dados 
        WHERE  (tt-dados.vl-inicial <> 0 OR tt-dados.vl-910  <> 0 OR
                tt-dados.vl-912     <> 0 OR tt-dados.vl-914  <> 0 OR
                tt-dados.vl-916     <> 0 OR tt-dados.vl-916  <> 0 OR tt-dados.vl-saldo-final <> 0)
        BREAK BY tt-dados.cdn_funcionario
              BY  tt-dados.centro-custo:

        run pi-acompanhar in h-acomp(input "Imprimindo Funcion rio: " + STRING(tt-dados.cdn_funcionario) + tt-dados.nom_pessoa_fisic ).

        DISP tt-dados.cdn_funcionario  COLUMN-LABEL "C¢digo"
             tt-dados.nom_pessoa_fisic COLUMN-LABEL "Funcion rio"
             tt-dados.centro-custo     COLUMN-LABEL "C. Custo"
             tt-dados.vl-inicial       COLUMN-LABEL "Saldo Inicial" (TOTAL) FORMAT "->>,>>>,>>9.99"
             tt-dados.vl-912           COLUMN-LABEL "F‚rias"        (TOTAL) FORMAT "->>,>>>,>>9.99"
             tt-dados.vl-914           COLUMN-LABEL "F‚rias 1/3"    (TOTAL) FORMAT "->>,>>>,>>9.99"
             tt-dados.vl-918           COLUMN-LABEL "FGTS"          (TOTAL) FORMAT "->>,>>>,>>9.99"
             tt-dados.vl-916           COLUMN-LABEL "INSS"          (TOTAL) FORMAT "->>,>>>,>>9.99"
             tt-dados.vl-910           COLUMN-LABEL "PIS"           (TOTAL) FORMAT "->>,>>>,>>9.99"
             tt-dados.vl-saldo-final   COLUMN-LABEL "Saldo Final"   (TOTAL) FORMAT "->>,>>>,>>9.99"
             WITH WIDTH 200 STREAM-IO.
    END.
END.

PROCEDURE pi-centro-custo:
    
    FOR EACH tt-dados 
        WHERE  (tt-dados.vl-inicial     <> 0 OR tt-dados.vl-910  <> 0 OR
                tt-dados.vl-912         <> 0 OR tt-dados.vl-914  <> 0 OR
                tt-dados.vl-916         <> 0 OR tt-dados.vl-916  <> 0 OR tt-dados.vl-saldo-final <> 0)
        BREAK BY tt-dados.centro-custo
              BY tt-dados.cdn_funcionario:

       run pi-acompanhar in h-acomp(input "Imprimindo Funcion rio: " + STRING(tt-dados.cdn_funcionario) + tt-dados.nom_pessoa_fisic ).

        DISP tt-dados.cdn_funcionario   COLUMN-LABEL "C¢digo"
             tt-dados.nom_pessoa_fisic  COLUMN-LABEL "Funcion rio"
             tt-dados.centro-custo      COLUMN-LABEL "C. Custo"
             tt-dados.vl-inicial        COLUMN-LABEL "Saldo Inicial" (TOTAL BY tt-dados.centro-custo) FORMAT "->>,>>>,>>9.99"
             tt-dados.vl-912            COLUMN-LABEL "F‚rias"        (TOTAL BY tt-dados.centro-custo) FORMAT "->>,>>>,>>9.99"
             tt-dados.vl-914            COLUMN-LABEL "F‚rias 1/3"    (TOTAL BY tt-dados.centro-custo) FORMAT "->>,>>>,>>9.99"
             tt-dados.vl-918            COLUMN-LABEL "FGTS"          (TOTAL BY tt-dados.centro-custo) FORMAT "->>,>>>,>>9.99"
             tt-dados.vl-916            COLUMN-LABEL "INSS"          (TOTAL BY tt-dados.centro-custo) FORMAT "->>,>>>,>>9.99"
             tt-dados.vl-910            COLUMN-LABEL "PIS"           (TOTAL BY tt-dados.centro-custo) FORMAT "->>,>>>,>>9.99"
             tt-dados.vl-saldo-final    COLUMN-LABEL "Saldo Final"   (TOTAL BY tt-dados.centro-custo) FORMAT "->>,>>>,>>9.99"
             WITH WIDTH 150 STREAM-IO.
                                         
    END.
END PROCEDURE.
/* fim do programa */


PROCEDURE pi-total :

   ASSIGN d-total-vl-inicial     = 0 
          d-total-vl-910         = 0 
          d-total-vl-912         = 0 
          d-total-vl-914         = 0 
          d-total-vl-916         = 0 
          d-total-vl-918         = 0
          d-total-vl-saldo-final = 0.

   FOR EACH tt-dados 
       WHERE  (tt-dados.vl-inicial <> 0 OR tt-dados.vl-910  <> 0 OR
               tt-dados.vl-912     <> 0 OR tt-dados.vl-914  <> 0 OR
               tt-dados.vl-916     <> 0 OR tt-dados.vl-916  <> 0 OR tt-dados.vl-saldo-final <> 0):

       run pi-acompanhar in h-acomp(input "Imprimindo Funcion rio: " + STRING(tt-dados.cdn_funcionario) + tt-dados.nom_pessoa_fisic ).

       ASSIGN d-total-vl-inicial     = d-total-vl-inicial     + tt-dados.vl-inicial           
              d-total-vl-910         = d-total-vl-910         + tt-dados.vl-912        
              d-total-vl-912         = d-total-vl-912         + tt-dados.vl-914        
              d-total-vl-914         = d-total-vl-914         + tt-dados.vl-918        
              d-total-vl-916         = d-total-vl-916         + tt-dados.vl-916        
              d-total-vl-918         = d-total-vl-918         + tt-dados.vl-910        
              d-total-vl-saldo-final = d-total-vl-saldo-final + tt-dados.vl-saldo-final.
   END.

   DISP d-total-vl-inicial     COLUMN-LABEL "Saldo Inicial"  FORMAT "->>,>>>,>>9.99"
        d-total-vl-910         COLUMN-LABEL "F‚rias"         FORMAT "->>,>>>,>>9.99"
        d-total-vl-912         COLUMN-LABEL "F‚rias 1/3"     FORMAT "->>,>>>,>>9.99"
        d-total-vl-914         COLUMN-LABEL "FGTS"           FORMAT "->>,>>>,>>9.99"
        d-total-vl-916         COLUMN-LABEL "INSS"           FORMAT "->>,>>>,>>9.99"
        d-total-vl-918         COLUMN-LABEL "PIS"            FORMAT "->>,>>>,>>9.99"
        d-total-vl-saldo-final COLUMN-LABEL "Saldo Final"    FORMAT "->>,>>>,>>9.99"
        WITH WIDTH 150 STREAM-IO.        

END PROCEDURE.


PROCEDURE pi-inicial:

   FOR EACH  movto_calcul_func
       WHERE movto_calcul_func.cdn_empresa       =  tt-param.i-empresa
       AND   movto_calcul_func.cdn_estab         >= tt-param.i-est-ini 
       AND   movto_calcul_func.cdn_estab         <= tt-param.i-est-fim 
       AND   movto_calcul_func.num_ano_refer_fp   = i-ano-anterior
       AND   movto_calcul_func.num_mes_refer_fp   = i-mes-anterior
       AND   movto_calcul_func.idi_tip_fp         = 1
       NO-LOCK:
   
       run pi-acompanhar in h-acomp(input "Lendo Funcion rio: " + STRING(movto_calcul_func.cdn_funcionario)).
   
       FIND FIRST tt-dados
            WHERE tt-dados.cdn_funcionario   = movto_calcul_func.cdn_funcionario
            NO-ERROR.
       IF NOT AVAIL tt-dados THEN NEXT.
   
       ASSIGN i-cont = 1.
       DO WHILE i-cont <= 30:
          
          CASE movto_calcul_func.cdn_event_fp[i-cont]: 
              
               WHEN "925" THEN ASSIGN tt-dados.vl-inicial = tt-dados.vl-inicial + movto_calcul_func.val_calcul_efp [i-cont].
               WHEN "927" THEN ASSIGN tt-dados.vl-inicial = tt-dados.vl-inicial + movto_calcul_func.val_calcul_efp [i-cont].
               WHEN "926" THEN ASSIGN tt-dados.vl-inicial = tt-dados.vl-inicial + movto_calcul_func.val_calcul_efp [i-cont].
               WHEN "928" THEN ASSIGN tt-dados.vl-inicial = tt-dados.vl-inicial + movto_calcul_func.val_calcul_efp [i-cont].
          END CASE.
          
          ASSIGN i-cont = i-cont + 1.
       END.
   END.
END PROCEDURE.

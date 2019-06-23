/********************************************************************************************
**  Programa: ESUT0012
**  Data....: 28 de Dezembro de 2016
**  Autor...: Sergio Luiz Neto da Silveira - DSC
**  Objetivo: RelatΩrio de Saldo Fisicos dos Itens por per°odo
**  Vers∆o..: 2.06.00.001 - Versao Inicial
*********************************************************************************************/

/************************************* INCLUDES PADRAO **************************************/
/* include de controle de vers∆o */
{include/i-prgvrs.i ESUT0012RP 2.06.00.001}
/********************************************************************************************/

/********************************* DEFINICAO DE TEMP-TABLES *********************************/
/* definiå„o das temp-tables para recebimento de parÒmetros */
define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG
    FIELD cod-estabel-ini  LIKE estabelec.cod-estabel
    FIELD cod-estabel-fim  LIKE estabelec.cod-estabel
    FIELD it-codigo-ini    LIKE ITEM.it-codigo
    FIELD it-codigo-fim    LIKE ITEM.it-codigo
    FIELD periodo-ini      AS   CHARACTER
    FIELD periodo-fim      AS   CHARACTER
    FIELD rs-tipo-saida    AS   INTEGER.
    
DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita       AS RAW.

DEFINE TEMP-TABLE tt-periodo
       FIELD dt-inicial AS DATE
       FIELD dt-final   AS DATE
    INDEX data IS PRIMARY UNIQUE dt-inicial dt-final.

DEFINE TEMP-TABLE tt-periodo-item
       FIELD it-codigo  LIKE ITEM.it-codigo
       FIELD dt-inicial AS   DATE
       FIELD dt-final   AS   DATE
       FIELD qt-inicial LIKE saldo-estoq.qtidade-atu COLUMN-LABEL "Saldo Inicial"
       FIELD qt-entrada LIKE saldo-estoq.qtidade-atu COLUMN-LABEL "Entradas"
       FIELD qt-saida   LIKE saldo-estoq.qtidade-atu COLUMN-LABEL "Saidas"
       FIELD qt-final   LIKE saldo-estoq.qtidade-atu COLUMN-LABEL "Saldo Final"
    INDEX periodo-item IS PRIMARY UNIQUE it-codigo dt-inicial dt-final.



DEFINE STREAM st-excel.
/********************************************************************************************/

/********************************* DEFINICAO DE PARAMETROS **********************************/
/* recebimento de parÒmetros */
DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FIND FIRST tt-param.
/********************************************************************************************/

/********************************** DEFINICAO DE VARIAVEIS **********************************/
define new global shared variable v_cod_usuar_corren as character no-undo.
define variable h-acomp        as handle    no-undo.

DEFINE VARIABLE i-cont AS INTEGER NO-UNDO.

&IF "{&mguni_version}" >= "2.071" &THEN
def new global shared var i-ep-codigo-usuario  LIKE ems2cadme.empresa.ep-codigo format "x(03)" no-undo.
&ELSE
def new global shared var i-ep-codigo-usuario  as integer format ">>9" no-undo.
&ENDIF
def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.

{include/i-rpvar.i}

DEFINE VARIABLE d-saldo-atual     LIKE saldo-estoq.qtidade-atu.
DEFINE VARIABLE de-qtde           LIKE saldo-estoq.qtidade-atu.


def new shared var da-data-ini as date initial &IF "{&ems_dbtype}":U = "MSS":U &THEN "01/01/1800":U &ELSE "01/01/0001":U &ENDIF.
def new shared var da-data-fin as date initial "12/31/2999".
def new shared var c-depo-ini as character initial "".
def new shared var c-depo-fin as character initial "ZZZ".
def new shared var c-loca-ini as character initial "".
def new shared var c-loca-fin as character initial "ZZZZZZZZZZ". 
def new shared var c-estabelec as character initial "".
def new shared var i-espe-ini as integer initial 1.
def new shared var i-espe-fin as integer initial 37.
def new shared var c-seri-ini as character initial "".
def new shared var c-seri-fin as character initial "ZZZZZ".
def new shared var c-nume-ini as character initial "".
def new shared var c-nume-fin as character initial "ZZZZZZZZZZZZZZZZ".
def new shared var c-refer-ini as character initial "".
def new shared var c-refer-fim as character initial "ZZZZZZZZ".
def new shared var c-lot-ini as character initial "".
def new shared var c-lot-fim as character initial "ZZZZZZZZZZ".


/********************************************************************************************/

/************************************ DEFINICAO DE FUNCOES **********************************/
/********************************************************************************************/

/************************************ DEFINICAO DE FRAMES ***********************************/
FIND FIRST  ems2cadme.empresa 
     NO-LOCK NO-ERROR.

/* bloco principal do programa */
ASSIGN c-programa 	    = "ESUT0012RP"
	   c-versao	        = "2.06"
	   c-revisao	    = "00.001"
	   c-empresa 	    = empresa.razao-social
	   c-sistema	    = "UTP"
	   c-titulo-relat   = "Relatorio de Saldo Fisico do Itens por Per°odo".

FORM HEADER 
     FILL("-", 132)        FORMAT "x(132)" SKIP 
     c-empresa 
     c-titulo-relat AT 050
     "Pagina:":U    AT 120 
     page-number    AT 128 FORMAT ">>>>9" SKIP 
     FILL("-", 112)        FORMAT "x(110)" 
     TODAY                 FORMAT "99/99/9999"
     "-" 
     STRING(TIME,"HH:MM:SS":U) SKIP 
     "                       " SKIP
     "-----------------------"
WITH STREAM-IO NO-BOX NO-LABEL OVERLAY PAGE-TOP WIDTH 132 FRAME f-cabec.

ASSIGN c-rodape = "DATASUL - " + c-sistema + " - " + c-programa + " - V:" + c-versao + "." + c-revisao
       c-rodape = FILL("-", 132 - LENGTH(c-rodape)) + c-rodape.

FORM HEADER 
     c-rodape   FORMAT "x(132)"
  WITH STREAM-IO WIDTH 132 NO-LABELS NO-BOX PAGE-BOTTOM FRAME f-rodape.
/********************************************************************************************/
      
/************************************* BLOCO PRINCIPAL **************************************/
run utp/ut-acomp.p PERSISTENT SET h-acomp.

{utp/ut-liter.i Imprimindo *}

run pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

/* include padr∆o para output de relat∏rios */
{include/i-rpout.i}

CASE tt-param.rs-tipo-saida:
     WHEN 1 THEN DO:
        RUN pi-cria-periodos-mensal.   
     END.
     WHEN 2 THEN DO:
        RUN pi-cria-periodos-anual.
     END.
     WHEN 3 THEN DO:
        RUN pi-cria-periodos-geral.
     END.
END CASE.

RUN pi-dados.
RUN pi-impressao.



/* fechamento do output do relat∏rio  */
{include/i-rpclo.i}

run pi-finalizar IN h-acomp.
RETURN "OK":U.
/********************************************************************************************/

/********************************* DEFINICAO DE procedureS **********************************/
PROCEDURE pi-cria-periodos-mensal:
   DEFINE VARIABLE i-ano-ini AS INTEGER NO-UNDO.
   DEFINE VARIABLE i-ano-fim AS INTEGER NO-UNDO.
   DEFINE VARIABLE i-mes-ini AS INTEGER NO-UNDO.
   DEFINE VARIABLE i-mes-fim AS INTEGER NO-UNDO.
   DEFINE VARIABLE ano       AS INTEGER NO-UNDO.
   DEFINE VARIABLE mes       AS INTEGER NO-UNDO.

   EMPTY TEMP-TABLE tt-periodo.

   ASSIGN i-ano-ini = INTEGER(SUBSTRING(tt-param.periodo-ini,1,4))
          i-ano-fim = INTEGER(SUBSTRING(tt-param.periodo-fim,1,4)).

   ASSIGN i-mes-ini = INTEGER(SUBSTRING(tt-param.periodo-ini,5,2))
          i-mes-fim = INTEGER(SUBSTRING(tt-param.periodo-fim,5,2)).

   DO ano = i-ano-ini TO i-ano-fim:
      DO mes = (IF ano = i-ano-ini THEN i-mes-ini ELSE 1) TO (IF ano = i-ano-fim THEN i-mes-fim ELSE 12):
         CREATE tt-periodo.
         ASSIGN tt-periodo.dt-inicial = DATE(mes,1,ano).

         IF mes = 12 THEN
            ASSIGN tt-periodo.dt-final = DATE(12,31,ano).
         ELSE
            ASSIGN tt-periodo.dt-final = DATE(mes + 1,1,ano) - 1.
      END.
   END.
END PROCEDURE.

PROCEDURE pi-cria-periodos-geral:
   DEFINE VARIABLE i-ano-ini AS INTEGER NO-UNDO.
   DEFINE VARIABLE i-ano-fim AS INTEGER NO-UNDO.
   DEFINE VARIABLE i-mes-ini AS INTEGER NO-UNDO.
   DEFINE VARIABLE i-mes-fim AS INTEGER NO-UNDO.
   DEFINE VARIABLE ano       AS INTEGER NO-UNDO.
   DEFINE VARIABLE mes       AS INTEGER NO-UNDO.

   EMPTY TEMP-TABLE tt-periodo.

   ASSIGN i-ano-ini = INTEGER(SUBSTRING(tt-param.periodo-ini,1,4))
          i-ano-fim = INTEGER(SUBSTRING(tt-param.periodo-fim,1,4)).

   ASSIGN i-mes-ini = INTEGER(SUBSTRING(tt-param.periodo-ini,5,2))
          i-mes-fim = INTEGER(SUBSTRING(tt-param.periodo-fim,5,2)).

   CREATE tt-periodo.
   ASSIGN tt-periodo.dt-inicial = DATE(i-mes-ini,1,i-ano-ini).

   IF i-mes-fim = 12 THEN
      ASSIGN tt-periodo.dt-final = DATE(12,31,i-ano-fim).
   ELSE
      ASSIGN tt-periodo.dt-final = DATE(i-mes-fim + 1,1,i-ano-fim) - 1.   
END PROCEDURE.

PROCEDURE pi-cria-periodos-anual:
   DEFINE VARIABLE i-ano-ini AS INTEGER NO-UNDO.
   DEFINE VARIABLE i-ano-fim AS INTEGER NO-UNDO.
   DEFINE VARIABLE i-mes-ini AS INTEGER NO-UNDO.
   DEFINE VARIABLE i-mes-fim AS INTEGER NO-UNDO.
   DEFINE VARIABLE ano       AS INTEGER NO-UNDO.
   DEFINE VARIABLE mes       AS INTEGER NO-UNDO.

   EMPTY TEMP-TABLE tt-periodo.

   ASSIGN i-ano-ini = INTEGER(SUBSTRING(tt-param.periodo-ini,1,4))
          i-ano-fim = INTEGER(SUBSTRING(tt-param.periodo-fim,1,4)).

   ASSIGN i-mes-ini = INTEGER(SUBSTRING(tt-param.periodo-ini,5,2))
          i-mes-fim = INTEGER(SUBSTRING(tt-param.periodo-fim,5,2)).

   IF i-ano-ini = i-ano-fim THEN DO:
      CREATE tt-periodo.
      ASSIGN tt-periodo.dt-inicial = DATE(i-mes-ini,1,i-ano-ini).

      IF i-mes-fim = 12 THEN
         ASSIGN tt-periodo.dt-final = DATE(12,31,i-ano-fim).
      ELSE
         ASSIGN tt-periodo.dt-final = DATE(i-mes-fim + 1,1,i-ano-fim) - 1.   
   END.
   ELSE DO:
      DO ano = i-ano-ini TO i-ano-fim:
         IF ano = i-ano-ini THEN DO:
            CREATE tt-periodo.
            ASSIGN tt-periodo.dt-inicial = DATE(i-mes-ini,1,ano)
                   tt-periodo.dt-final   = DATE(12,31,ano).
         END.
         ELSE
            IF ano = i-ano-fim THEN DO:
               CREATE tt-periodo.
               ASSIGN tt-periodo.dt-inicial = DATE(1,1,ano).
                   
               IF i-mes-fim = 12 THEN
                  ASSIGN tt-periodo.dt-final = DATE(12,31,ano).
               ELSE
                  ASSIGN tt-periodo.dt-final = DATE(i-mes-fim + 1,1,ano) - 1.  
            END.
            ELSE DO:
               CREATE tt-periodo.
               ASSIGN tt-periodo.dt-inicial = DATE(1,1,ano)
                      tt-periodo.dt-final   = DATE(12,31,ano).
            END.
      END.
   END.
END PROCEDURE.


procedure pi-impressao:
   DEFINE VARIABLE c-narrativa      AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE c-arquivo        AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE ch-excel         AS    COMPONENT-HANDLE      NO-UNDO.
   DEFINE VARIABLE i-cont-linha     AS    INTEGER               NO-UNDO.
   DEFINE VARIABLE c-tipo           AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE c-ordem          AS    CHARACTER             NO-UNDO FORMAT "x(300)".

   ASSIGN c-arquivo = ENTRY(1,tt-param.arquivo,".") + REPLACE(STRING(TODAY,'99/99/9999'),'/','') + REPLACE(STRING(TIME,'hh:mm:ss'),':','') + ".csv".

   IF i-num-ped-exec-rpw <> 0 Then 
      Assign c-arquivo = c-dir-spool-servid-exec + "~/" + ENTRY(1,tt-param.arquivo,".") + REPLACE(STRING(TODAY,'99/99/9999'),'/','') + REPLACE(STRING(TIME,'hh:mm:ss'),':','') + ".csv".

   FIND FIRST param-global
        NO-LOCK NO-ERROR.

   FILE-INFO:FILE-NAME = c-arquivo.

   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
      OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).

   OUTPUT STREAM st-excel TO VALUE(c-arquivo) NO-CONVERT NO-MAP.

   
   PUT STREAM st-excel
       "Item;Descriá∆o;".

   FOR EACH tt-periodo
            NO-LOCK:
       PUT STREAM st-excel
           "Saldo em " + STRING(tt-periodo.dt-inicial,'99/99/9999') FORMAT "x(22)" ";"
           "Adiáoes/Compras" ";"
           "Baixa/Consumo" ";"
           "Saldo em " + STRING(tt-periodo.dt-final  ,'99/99/9999') FORMAT "x(22)" ";".

   END.

   PUT STREAM st-excel SKIP.

   FOR EACH tt-periodo-item
            BREAK BY tt-periodo-item.it-codigo:

      RUN pi-acompanhar IN h-acomp (INPUT "Imprimindo Item: " + tt-periodo-item.it-codigo).

      IF FIRST-OF(tt-periodo-item.it-codigo) THEN DO:
         FIND FIRST ITEM
              WHERE ITEM.it-codigo = tt-periodo-item.it-codigo
              NO-LOCK NO-ERROR.

         PUT STREAM st-excel 
             tt-periodo-item.it-codigo ";"
             ITEM.desc-item ";".
      END.

      PUT STREAM st-excel 
          tt-periodo-item.qt-inicial ";"
          tt-periodo-item.qt-entrada ";"
          tt-periodo-item.qt-saida   ";"
          tt-periodo-item.qt-final   ";". 

      IF LAST-OF(tt-periodo-item.it-codigo) THEN DO:
         PUT STREAM st-excel 
             SKIP.
      END.
   END.

   PUT STREAM st-excel SKIP.
       
   OUTPUT STREAM st-excel CLOSE.

   IF i-num-ped-exec-rpw = 0 THEN
      DOS SILENT START excel.exe VALUE(c-arquivo).
END PROCEDURE.




PROCEDURE pi-dados:
   DEFINE VARIABLE de-ultimo-saldo LIKE saldo-estoq.qtidade-atu.
   DEFINE VARIABLE d-total         AS   DECIMAL NO-UNDO.

   EMPTY TEMP-TABLE tt-periodo-item.
      
   blk:
   FOR EACH ITEM
            WHERE ITEM.it-codigo >= tt-param.it-codigo-ini AND
                  ITEM.it-codigo <= tt-param.it-codigo-fim
            NO-LOCK:

      RUN pi-acompanhar IN h-acomp (INPUT "Lendo Item: " + ITEM.it-codigo).

      FOR EACH saldo-estoq USE-INDEX estabel-item
               WHERE saldo-estoq.cod-estabel >= tt-param.cod-estabel-ini AND
                     saldo-estoq.cod-estabel <= tt-param.cod-estabel-fim AND
                     saldo-estoq.it-codigo    = ITEM.it-codigo
               NO-LOCK:
          ASSIGN d-saldo-atual = saldo-estoq.qtidade-atu.
      END.

      FOR EACH tt-periodo
               NO-LOCK:
         CREATE tt-periodo-item.
         ASSIGN tt-periodo-item.it-codigo  = ITEM.it-codigo
                tt-periodo-item.dt-inicial = tt-periodo.dt-inicial
                tt-periodo-item.dt-final   = tt-periodo.dt-final.

         ASSIGN da-data-ini  = tt-periodo.dt-inicial
                da-data-fin  = tt-periodo.dt-final. 

         RUN pi-saldo-item (INPUT  ITEM.it-codigo,
                            INPUT  tt-param.cod-estabel-ini,
                            INPUT  tt-param.cod-estabel-fim,
                            OUTPUT tt-periodo-item.qt-inicial,
                            OUTPUT tt-periodo-item.qt-final).

         /* Saldo inicial do item no periodo */
         FOR EACH movto-estoq use-index item-data
                  WHERE movto-estoq.it-codigo    = ITEM.it-codigo            AND
                        movto-estoq.cod-estabel >= tt-param.cod-estabel-ini  AND
                        movto-estoq.cod-estabel <= tt-param.cod-estabel-fim  AND
                        movto-estoq.dt-trans    >= tt-periodo.dt-inicial     AND
                        movto-estoq.dt-trans    <= tt-periodo.dt-final
                  NO-LOCK:

            IF movto-estoq.tipo-trans = 1 THEN
               ASSIGN tt-periodo-item.qt-entrada = tt-periodo-item.qt-entrada + movto-estoq.quantidade.
            ELSE
               ASSIGN tt-periodo-item.qt-saida   = tt-periodo-item.qt-saida   + movto-estoq.quantidade.
         END.
      END.

      ASSIGN d-total = 0.

      blk1:
      FOR EACH tt-periodo-item
               WHERE tt-periodo-item.it-codigo = ITEM.it-codigo
               NO-LOCK:
         ASSIGN d-total = d-total + (tt-periodo-item.qt-inicial + tt-periodo-item.qt-entrada + tt-periodo-item.qt-saida + tt-periodo-item.qt-final).

         IF d-total > 0 THEN
            LEAVE blk1.
      END.

      IF d-total = 0 THEN DO:
         FOR EACH tt-periodo-item
                  WHERE tt-periodo-item.it-codigo = ITEM.it-codigo
                  EXCLUSIVE-LOCK:
            DELETE tt-periodo-item.
         END.
      END.


   END.


END PROCEDURE.

PROCEDURE pi-saldo-item:
   DEFINE INPUT  PARAMETER ip-it-codigo       LIKE ITEM.it-codigo          NO-UNDO.
   DEFINE INPUT  PARAMETER ip-cod-estabel-ini LIKE estabelec.cod-estabel   NO-UNDO.
   DEFINE INPUT  PARAMETER ip-cod-estabel-fim LIKE estabelec.cod-estabel   NO-UNDO.
   DEFINE OUTPUT PARAMETER de-qtidade-ini     LIKE saldo-estoq.qtidade-atu NO-UNDO.
   DEFINE OUTPUT PARAMETER de-qtidade-fin     LIKE saldo-estoq.qtidade-atu NO-UNDO.


   /* calcula o saldo inicial e final do item. */
   assign de-qtidade-ini = 0
          de-qtidade-fin = 0.
            
   for each saldo-estoq fields (saldo-estoq.qtidade-atu)
        where saldo-estoq.it-codigo    = ip-it-codigo 
          and saldo-estoq.cod-estabel  >= ip-cod-estabel-ini
          and saldo-estoq.cod-estabel  <= ip-cod-estabel-fim
          and saldo-estoq.cod-depos   >= c-depo-ini
          and saldo-estoq.cod-depos   <= c-depo-fin
          and saldo-estoq.lote   >= c-lot-ini
          and saldo-estoq.lote   <= c-lot-fim
          and saldo-estoq.cod-localiz >= c-loca-ini
          and saldo-estoq.cod-localiz <= c-loca-fin
          and saldo-estoq.cod-refer >= c-refer-ini
          and saldo-estoq.cod-refer <= c-refer-fim
        no-lock:       
        assign de-qtidade-ini = de-qtidade-ini + saldo-estoq.qtidade-atu
               de-qtidade-fin = de-qtidade-fin + saldo-estoq.qtidade-atu.
    end.

    /* Calcula o saldo inicial e final -------------------------------------------*/
    find first saldo-estoq
        where saldo-estoq.it-codigo    = ip-it-codigo
          and saldo-estoq.cod-estabel >= ip-cod-estabel-ini
          and saldo-estoq.cod-estabel <= ip-cod-estabel-fim
          and saldo-estoq.cod-depos   >= c-depo-ini
          and saldo-estoq.cod-depos   <= c-depo-fin
          and saldo-estoq.lote   >= c-lot-ini
          and saldo-estoq.lote   <= c-lot-fim
          and saldo-estoq.cod-localiz >= c-loca-ini
          and saldo-estoq.cod-localiz <= c-loca-fin
          and saldo-estoq.cod-refer >= c-refer-ini
          and saldo-estoq.cod-refer <= c-refer-fim
        no-lock no-error.

    if not avail saldo-estoq then do:
       assign de-qtidade-ini = 0
              de-qtidade-fin = 0.
    end.    
    else do:
        for each item-estab fields (cod-estabel)
            where item-estab.it-codigo = ip-it-codigo
              and item-estab.cod-estabel >= ip-cod-estabel-ini 
              and item-estab.cod-estabel <= ip-cod-estabel-fim 
            no-lock
            with 1 down:

            for each movto-estoq fields (movto-estoq.tipo-trans
                                         movto-estoq.dt-trans
                                         movto-estoq.quantidade) use-index item-data
                where movto-estoq.cod-estabel  = item-estab.cod-estabel
                  and movto-estoq.it-codigo    = ip-it-codigo
                  and movto-estoq.dt-trans    >= da-data-ini
                  and movto-estoq.cod-depos   >= c-depo-ini
                  and movto-estoq.cod-depos   <= c-depo-fin
                  and movto-estoq.lote   >= c-lot-ini
                  and movto-estoq.lote   <= c-lot-fim
                  and movto-estoq.cod-localiz >= c-loca-ini
                  and movto-estoq.cod-localiz <= c-loca-fin
                  and movto-estoq.cod-refer >= c-refer-ini
                  and movto-estoq.cod-refer <= c-refer-fim
                no-lock:

                if movto-estoq.tipo-trans = 1 then do:         /* Movimento de Entrada */
                    /* Se a data for maior que o per≠odo especificado pelo usuˇrio,
                       altera a quantidade do saldo inicial/final ---------------------*/
                    if movto-estoq.dt-trans > da-data-fin then
                        assign
                            de-qtidade-fin = de-qtidade-fin - movto-estoq.quantidade
                            de-qtidade-ini = de-qtidade-ini - movto-estoq.quantidade.
                    else
                        assign de-qtidade-ini = de-qtidade-ini - movto-estoq.quantidade.
                end.
                else do:
                    if movto-estoq.dt-trans > da-data-fin then
                       assign de-qtidade-fin = de-qtidade-fin + movto-estoq.quantidade
                              de-qtidade-ini = de-qtidade-ini + movto-estoq.quantidade.
                    else
                      assign de-qtidade-ini = de-qtidade-ini + movto-estoq.quantidade.
                end.
            end.
        end.
    end.
END PROCEDURE.

/********************************************************************************************/
/************************************** FIM DO PROGRAMA *************************************/
/********************************************************************************************/


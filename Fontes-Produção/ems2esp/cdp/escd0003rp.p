/********************************************************************************************
**  Programa: ESCD0003
**  Data....: 25 de Agosto de 2016
**  Autor...: Sergio Luiz Neto da Silveira - DSC
**  Objetivo: Replica‡Æo de Itens Marac 
**  Versão..: 2.06.00.001 - Versao Inicial
*********************************************************************************************/

/************************************* INCLUDES PADRAO **************************************/
/* include de controle de versão */
{include/i-prgvrs.i ESCD0003RP 2.06.00.001}
/********************************************************************************************/

/********************************* DEFINICAO DE TEMP-TABLES *********************************/
/* definiîÒo das temp-tables para recebimento de par±metros */
define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG.
    
DEFINE TEMP-TABLE tt-es-fila-rep-item LIKE es-fila-rep-item. 

DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita       AS RAW.
/********************************************************************************************/

/********************************* DEFINICAO DE PARAMETROS **********************************/
/* recebimento de par±metros */
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


{include/i-rpvar.i}

/********************************************************************************************/

/************************************ DEFINICAO DE FUNCOES **********************************/
/********************************************************************************************/

/************************************ DEFINICAO DE FRAMES ***********************************/
FIND FIRST  ems2cadme.empresa 
     NO-LOCK NO-ERROR.

/* bloco principal do programa */
ASSIGN c-programa 	    = "ESCD0003RP"
	   c-versao	        = "2.06"
	   c-revisao	    = "00.001"
	   c-empresa 	    = empresa.razao-social
	   c-sistema	    = "CDP"
	   c-titulo-relat   = "Replica‡Æo de Itens Marac ".

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
WITH STREAM-IO NO-BOX NO-LABEL OVERLAY PAGE-TOP WIDTH 132 FRAME f-cabec.

ASSIGN c-rodape = "DATASUL - " + c-sistema + " - " + c-programa + " - V:" + c-versao + "." + c-revisao
       c-rodape = FILL("-", 132 - LENGTH(c-rodape)) + c-rodape.

FORM HEADER 
     c-rodape   FORMAT "x(132)"
  WITH STREAM-IO WIDTH 132 NO-LABELS NO-BOX PAGE-BOTTOM FRAME f-rodape.

DEFINE STREAM st-excel.
/********************************************************************************************/
      
/************************************* BLOCO PRINCIPAL **************************************/
run utp/ut-acomp.p PERSISTENT SET h-acomp.

{utp/ut-liter.i Imprimindo *}

run pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

/* include padrão para output de relat©rios */
{include/i-rpout.i}

RUN pi-itens.
RUN pi-impressao.

/* fechamento do output do relat©rio  */
{include/i-rpclo.i}

run pi-finalizar IN h-acomp.
RETURN "OK":U.
/********************************************************************************************/

/********************************* DEFINICAO DE procedureS **********************************/
procedure pi-impressao:
   DEFINE VARIABLE c-narrativa      AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE c-arquivo        AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE ch-excel         AS    COMPONENT-HANDLE      NO-UNDO.
   DEFINE VARIABLE i-cont-linha     AS    INTEGER               NO-UNDO.

   ASSIGN c-arquivo = ENTRY(1,tt-param.arquivo,".") + ".csv".

   OUTPUT STREAM st-excel TO VALUE(c-arquivo) NO-CONVERT NO-MAP.

   PUT STREAM st-excel
       "Empresa"     ";"
       "Item"        ";"
       "Descri‡Æo"   ";"
       "Narrativa"   ";"
       "Un"          ";"
       "GE"          ";"
       "FM"          ";"
       "Class Fisc"  ";"
       "Cod Refer"   ";"
       "Cod Orig"    ";"
       "Dec-1"       ";"
       "Tp Contr"    ";"
       "Quant Segur" ";"
       "Cod Servi‡o" ";"
       "Fm Cod Com"  ";"
       "Char-2"      ";"
       "Depto"       ";"
       "Dt Impl"     ";"
       "Dt Cria‡Æo"  ";"
       "Dt Ult Tent" ";"
       "Nr Tent"     ";"
       "Msg Erro"    ";"
       "Estabelec"   ";" SKIP.

   blk1:
   FOR EACH tt-es-fila-rep-item 
            NO-LOCK:

      ASSIGN i-cont = i-cont + 1.

      RUN pi-acompanhar IN h-acomp ("Listando: " + STRING(i-cont)).

      CREATE es-fila-rep-item.
      BUFFER-COPY tt-es-fila-rep-item TO es-fila-rep-item NO-ERROR.

      IF ERROR-STATUS:ERROR THEN 
         NEXT blk1.

      PUT STREAM st-excel
          tt-es-fila-rep-item.ep-codigo        ";"
          tt-es-fila-rep-item.it-codigo        ";"
          tt-es-fila-rep-item.desc-item        ";"
          replace(replace(tt-es-fila-rep-item.narrativa,CHR(10),""),CHR(13),"")    FORMAT "x(200)"    ";"
          tt-es-fila-rep-item.un               ";"
          tt-es-fila-rep-item.ge-codigo        ";"
          tt-es-fila-rep-item.fm-codigo        ";"
          tt-es-fila-rep-item.class-fiscal     ";"
          tt-es-fila-rep-item.codigo-refer     ";"
          tt-es-fila-rep-item.codigo-orig      ";"
          tt-es-fila-rep-item.dec-1            ";"
          tt-es-fila-rep-item.tipo-contr       ";"
          tt-es-fila-rep-item.quant-segur      ";"
          tt-es-fila-rep-item.cod-servico      ";"
          tt-es-fila-rep-item.fm-cod-com       ";"
          tt-es-fila-rep-item.char-2           ";"
          tt-es-fila-rep-item.cod-depto        ";"
          tt-es-fila-rep-item.data-implant     ";"
          tt-es-fila-rep-item.dt-criacao       ";"
          tt-es-fila-rep-item.dt-ult-tentativa ";"
          tt-es-fila-rep-item.nr-tentativas    ";"
          tt-es-fila-rep-item.mensagem-erro    ";"
          tt-es-fila-rep-item.cod-estabel      ";" SKIP.
   END.

   OUTPUT STREAM st-excel CLOSE.
   DOS SILENT START excel.exe VALUE(c-arquivo).
END PROCEDURE.

PROCEDURE pi-itens:
   DEFINE VARIABLE i-codigo AS INTEGER NO-UNDO.

   EMPTY TEMP-TABLE tt-es-fila-rep-item.

   ASSIGN i-cont = 0.

   blk:
   FOR EACH ITEM
       WHERE NO-LOCK:

      ASSIGN i-cont = i-cont + 1.

      RUN pi-acompanhar IN h-acomp ("Montando: " + STRING(i-cont)).
/*                                      */
/*       IF ITEM.cod-obsoleto <> 1 THEN */
/*          NEXT blk.                   */
          
      IF ITEM.it-codigo BEGINS "XX" THEN 
         NEXT blk.

      ASSIGN i-codigo = INTEGER(SUBSTRING(ITEM.it-codigo,1,2)) NO-ERROR.

      IF NOT ERROR-STATUS:ERROR THEN NEXT blk.

      FIND FIRST narrativa OF ITEM
           NO-LOCK NO-ERROR.

      FIND FIRST es-it-depto
           WHERE es-it-depto.it-codigo = ITEM.it-codigo
           NO-LOCK NO-ERROR.

      FIND FIRST es-de-para-it-padr
           WHERE es-de-para-it-padr.it-codigo-orig = ITEM.it-codigo
           NO-LOCK NO-ERROR.

      CREATE tt-es-fila-rep-item.
      ASSIGN tt-es-fila-rep-item.ep-codigo         = "610"
	      tt-es-fila-rep-item.it-codigo         = item.it-codigo
             tt-es-fila-rep-item.desc-item         = item.desc-item
             tt-es-fila-rep-item.narrativa         = IF AVAILABLE(narrativa) THEN narrativa.descricao ELSE ""
             tt-es-fila-rep-item.un                = item.un
             tt-es-fila-rep-item.ge-codigo         = item.ge-codigo
             tt-es-fila-rep-item.fm-codigo         = item.fm-codigo
             tt-es-fila-rep-item.class-fiscal      = item.class-fiscal
             tt-es-fila-rep-item.codigo-refer      = item.cod-refer
             tt-es-fila-rep-item.codigo-orig       = item.codigo-orig
             tt-es-fila-rep-item.dec-1             = item.dec-1
             tt-es-fila-rep-item.tipo-contr        = item.tipo-contr
             tt-es-fila-rep-item.quant-segur       = item.quant-segur
             tt-es-fila-rep-item.cod-servico       = item.cod-servico
             tt-es-fila-rep-item.fm-cod-com        = item.fm-cod-com
             tt-es-fila-rep-item.char-2            = item.char-2
             tt-es-fila-rep-item.cod-depto         = IF AVAILABLE(es-it-depto) THEN es-it-depto.cod-depto ELSE 0
             tt-es-fila-rep-item.data-implant      = TODAY
             tt-es-fila-rep-item.dt-criacao        = TODAY
             tt-es-fila-rep-item.dt-ult-tentativa  = TODAY
             tt-es-fila-rep-item.nr-tentativas     = 0
             tt-es-fila-rep-item.mensagem-erro     = ""
             tt-es-fila-rep-item.cod-estabel       = ""
                 NO-ERROR.
   END.
END PROCEDURE.
/********************************************************************************************/
/************************************** FIM DO PROGRAMA *************************************/
/********************************************************************************************/


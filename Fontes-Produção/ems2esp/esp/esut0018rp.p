/********************************************************************************************
**  Programa: ESUT0018
**  Data....: 06 de Fevereiro de 2017
**  Autor...: Sergio Luiz Neto da Silveira - DSC
**  Objetivo: Relat¢rio de Emitentes
**  Vers∆o..: 2.06.00.000 - Versao Inicial
*********************************************************************************************/

/************************************* INCLUDES PADRAO **************************************/
/* include de controle de vers∆o */
{include/i-prgvrs.i ESUT0018RP 2.06.00.000}
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
    FIELD cod-emitente-ini LIKE emitente.cod-emitente
    FIELD cod-emitente-fim LIKE emitente.cod-emitente
    FIELD nome-abrev-ini   LIKE emitente.nome-abrev
    FIELD nome-abrev-fim   LIKE emitente.nome-abrev
    FIELD cliente          AS   LOGICAL
    FIELD fornecedor       AS   LOGICAL
    FIELD ambos            AS   LOGICAL.
    
DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita       AS RAW.

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



/********************************************************************************************/

/************************************ DEFINICAO DE FUNCOES **********************************/
/********************************************************************************************/

/************************************ DEFINICAO DE FRAMES ***********************************/
FIND FIRST  ems2cadme.empresa 
     NO-LOCK NO-ERROR.

/* bloco principal do programa */
ASSIGN c-programa 	    = "ESUT0018RP"
	   c-versao	        = "2.06"
	   c-revisao	    = "00.002"
	   c-empresa 	    = empresa.razao-social
	   c-sistema	    = "UTP"
	   c-titulo-relat   = "Relat¢rio de Emitentes".

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

RUN pi-impressao.

/* fechamento do output do relat∏rio  */
{include/i-rpclo.i}

run pi-finalizar IN h-acomp.
RETURN "OK":U.
/********************************************************************************************/

/********************************* DEFINICAO DE procedureS **********************************/
procedure pi-impressao:
   DEFINE VARIABLE c-cgc            LIKE  emitente.cgc        NO-UNDO.
   DEFINE VARIABLE c-arquivo        AS    CHARACTER           NO-UNDO.
   DEFINE VARIABLE ch-excel         AS    COMPONENT-HANDLE    NO-UNDO.
   DEFINE VARIABLE i-cont-linha     AS    INTEGER             NO-UNDO.

define variable c-form1             like param-global.formato-id-pessoal.
define variable c-form2             like param-global.formato-id-federal.
define variable c-form3             AS   CHARACTER NO-UNDO.
define variable c-nome-emit         AS   CHARACTER FORMAT "x(80)" NO-UNDO.
   
   ASSIGN c-arquivo = ENTRY(1,tt-param.arquivo,".") + REPLACE(STRING(TODAY,'99/99/9999'),'/','') + REPLACE(STRING(TIME,'hh:mm:ss'),':','') + ".csv".

   FIND FIRST param-global
        NO-LOCK NO-ERROR.
   assign c-form1 = param-global.formato-id-pessoal
          c-form2 = param-global.formato-id-federal
          c-form3 = "x(20)".

   FIND FIRST ems2cadme.empresa
        NO-LOCK NO-ERROR.

   FILE-INFO:FILE-NAME = c-arquivo.

   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
      OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).

   OUTPUT STREAM st-excel TO VALUE(c-arquivo) NO-CONVERT NO-MAP.

   PUT STREAM st-excel 
       "Emitente;Nome Abrev;Nome Emit;CNPJ/CPF;Matriz;Gr Forn;Gr Cli;Identif;ESTMA;" SKIP.
   
   
   PUT STREAM st-excel SKIP.

   blk1:
   FOR EACH emitente
            WHERE emitente.cod-emitente >= tt-param.cod-emitente-ini AND
                  emitente.cod-emitente <= tt-param.cod-emitente-fim AND
                  emitente.nome-abrev   >= tt-param.nome-abrev-ini   AND
                  emitente.nome-abrev   <= tt-param.nome-abrev-fim
            NO-LOCK:

      RUN pi-acompanhar IN h-acomp ("Emitente : " + STRING(emitente.cod-emitente)).

      CASE emitente.identif:
          WHEN 1 THEN IF tt-param.cliente    = NO THEN NEXT.
          WHEN 2 THEN IF tt-param.fornecedor = NO THEN NEXT.
          WHEN 3 THEN IF tt-param.ambos      = NO THEN NEXT.
      END CASE.

      CASE emitente.natureza:
          WHEN 1 THEN ASSIGN c-cgc = "'" + string(emitente.cgc, c-form1).
          WHEN 2 THEN ASSIGN c-cgc = "'" + string(emitente.cgc, c-form2).
          OTHERWISE   ASSIGN c-cgc = "'" + string(emitente.cgc, c-form3).
      END CASE.

      ASSIGN c-nome-emit = replace(emitente.nome-emit,";",":")
             c-nome-emit = replace(c-nome-emit,CHR(08),"")
             c-nome-emit = replace(c-nome-emit,CHR(10),"")
             c-nome-emit = replace(c-nome-emit,CHR(13),"").

      PUT STREAM st-excel
          emitente.cod-emitente                        ";"
          emitente.nome-abrev                          ";"
          c-nome-emit                                  ";"
          c-cgc                                        ";"
          emitente.nome-matriz                         ";"
          emitente.cod-gr-forn                         ";"
          emitente.cod-gr-cli                          ";"
          ENTRY(emitente.identif,{adinc/i02ad098.i 3}) FORMAT "x(20)" ";".


      IF CAN-FIND(FIRST es-relat-cat
                  WHERE es-relat-cat.cdn_fornecedor = emitente.cod-emitente
                  NO-LOCK) THEN DO:
         PUT STREAM st-excel "SIM;".
      END.
      ELSE DO:
         PUT STREAM st-excel "N«O;".
      END.
          
      PUT STREAM st-excel SKIP.

   END.
     

   OUTPUT STREAM st-excel CLOSE.
   DOS SILENT START excel.exe VALUE(c-arquivo).
END PROCEDURE.
/********************************************************************************************/
/************************************** FIM DO PROGRAMA *************************************/
/********************************************************************************************/


/********************************************************************************************
**  Programa: ESUT0019
**  Data....: 10 de Mar‡o de 2017
**  Autor...: Sergio Luiz Neto da Silveira - DSC
**  Objetivo: Manuten‡Æo Formato Conta/Agencia bancaria
**  Vers’o..: 2.06.00.000 - Versao Inicial
*********************************************************************************************/

/************************************* INCLUDES PADRAO **************************************/
/* include de controle de vers’o */
{include/i-prgvrs.i ESUT0019RP 2.06.00.000}
/********************************************************************************************/

/********************************* DEFINICAO DE TEMP-TABLES *********************************/
/* definiÂÆo das temp-tables para recebimento de par¤metros */
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
    
DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita       AS RAW.

DEFINE STREAM st-excel.
/********************************************************************************************/

/********************************* DEFINICAO DE PARAMETROS **********************************/
/* recebimento de par¤metros */
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
ASSIGN c-programa 	    = "ESUT0019RP"
	   c-versao	        = "2.06"
	   c-revisao	    = "00.000"
	   c-empresa 	    = empresa.razao-social
	   c-sistema	    = "UTP"
	   c-titulo-relat   = "Manuten‡Æo Formato Conta/Agencia bancaria".

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

/* include padr’o para output de relat÷rios */
{include/i-rpout.i}

RUN pi-impressao.

/* fechamento do output do relat÷rio  */
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

   ASSIGN c-arquivo = ENTRY(1,tt-param.arquivo,".") + REPLACE(STRING(TODAY,'99/99/9999'),'/','') + REPLACE(STRING(TIME,'hh:mm:ss'),':','') + ".csv".

   FILE-INFO:FILE-NAME = c-arquivo.

   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
      OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).

   OUTPUT STREAM st-excel TO VALUE(c-arquivo) NO-CONVERT NO-MAP.

   PUT STREAM st-excel 
       "Banco;Nome;Formato Agencia (ant.);Formato Conta (ant.);Formato Agencia (atual);Formato Conta (atual);" SKIP.
   
   PUT STREAM st-excel SKIP.

   FOR EACH ems5.banco
            EXCLUSIVE-LOCK:

      RUN pi-acompanhar IN h-acomp (INPUT "Banco:" + STRING(ems5.banco.cod_banco) + "-" + ems5.banco.nom_banco).

      PUT STREAM st-excel
          ems5.banco.cod_banco              ";" 
          ems5.banco.nom_banco              ";" 
          ems5.banco.cod_format_agenc_bcia  ";" 
          ems5.banco.cod_format_cta_corren  ";".

      ASSIGN ems5.banco.cod_format_agenc_bcia = "XXXXXXXX"
             ems5.banco.cod_format_cta_corren = "XXXXXXXXXXXXXXXXXXXX".

      PUT STREAM st-excel
          ems5.banco.cod_format_agenc_bcia ";" 
          ems5.banco.cod_format_cta_corren ";".

      PUT STREAM st-excel SKIP.
   END.
   
   OUTPUT STREAM st-excel CLOSE.
   DOS SILENT START excel.exe VALUE(c-arquivo).
END PROCEDURE.
/********************************************************************************************/
/************************************** FIM DO PROGRAMA *************************************/
/********************************************************************************************/


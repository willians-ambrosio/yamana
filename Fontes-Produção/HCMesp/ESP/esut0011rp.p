/********************************************************************************************
**  Programa: ESUT0011
**  Data....: 29 de Setembro de 2016
**  Autor...: Sergio Luiz Neto da Silveira - DSC
**  Objetivo: Atualiza‡Æo S‚rie da Carteira de Trabalho do Funcionario
**  Versão..: 2.06.00.000 - Versao Inicial
*********************************************************************************************/

/************************************* INCLUDES PADRAO **************************************/
/* include de controle de versão */
{include/i-prgvrs.i ESUT0011RP 2.06.00.001}
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
    
DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita       AS RAW.

DEFINE STREAM st-excel.
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

&IF "{&mguni_version}" >= "2.071" &THEN
def new global shared var i-ep-codigo-usuario  LIKE mgcad.empresa.ep-codigo format "x(03)" no-undo.
&ELSE
def new global shared var i-ep-codigo-usuario  as integer format ">>9" no-undo.
&ENDIF
def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.

{include/i-rpvar.i}



/********************************************************************************************/

/************************************ DEFINICAO DE FUNCOES **********************************/
/********************************************************************************************/

/************************************ DEFINICAO DE FRAMES ***********************************/
FIND FIRST  mgcad.empresa 
     NO-LOCK NO-ERROR.

/* bloco principal do programa */
ASSIGN c-programa 	    = "ESUT0011RP"
	   c-versao	        = "2.06"
	   c-revisao	    = "00.000"
	   c-empresa 	    = empresa.razao-social
	   c-sistema	    = "UTP"
	   c-titulo-relat   = "Atualiza‡Æo S‚rie da Carteira de Trabalho do Funcionario".

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

/* include padrão para output de relat©rios */
{include/i-rpout.i}

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
   
   FIND FIRST param-global
        NO-LOCK NO-ERROR.

   FILE-INFO:FILE-NAME = c-arquivo.

   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
      OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).

   OUTPUT STREAM st-excel TO VALUE(c-arquivo) NO-CONVERT NO-MAP.

   PUT STREAM st-excel
       "Empresa;"
       "Est;"
       "C¢digo;"
       "Nome Abrev;"
       "S‚rie;"
       "S‚rie TRCT;"
       SKIP.

   DEFINE VARIABLE v_ser_cart_trct AS CHARACTER FORMAT "x(8)" NO-UNDO.

   blk:
   FOR EACH funcionario
            EXCLUSIVE-LOCK:

      ASSIGN v_ser_cart_trct = SUBSTRING(funcionario.cod_livre_3,37,08).

      IF v_ser_cart_trct <> "" THEN 
         NEXT blk.

      ASSIGN i-cont = i-cont + 1.

      RUN pi-acompanhar IN h-acomp ("Atualizando: " + STRING(i-cont)).

      OVERLAY(funcionario.cod_livre_3,37,08,'character') = STRING(funcionario.cod_ser_cart_trab,"x(8)").

      PUT STREAM st-excel 
          funcionario.cdn_empresa              ";"
          funcionario.cdn_estab                ";"
          funcionario.cdn_funcionario          ";"
          funcionario.nom_abrev_pessoa_fisic   ";'"
          funcionario.cod_ser_cart_trab        ";'"
          SUBSTRING(funcionario.cod_livre_3,37,08) SKIP.
   END.
     
   OUTPUT STREAM st-excel CLOSE.
   DOS SILENT START excel.exe VALUE(c-arquivo).
END PROCEDURE.
/********************************************************************************************/
/************************************** FIM DO PROGRAMA *************************************/
/********************************************************************************************/


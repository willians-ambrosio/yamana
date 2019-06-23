/********************************************************************************************
**  Programa: YG0003
**  Data....: 05 de Maio de 2017
**  Autor...: Sergio Luiz Neto da Silveira - DSC
**  Objetivo: Alteraá∆o Plano de Manutená∆o
**  Vers∆o..: 2.06.00.001 - Versao Inicial
*********************************************************************************************/

/************************************* INCLUDES PADRAO **************************************/
/* include de controle de vers∆o */
{include/i-prgvrs.i YG0003RP 2.06.00.001}
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
    field l-habilitaRtf    as LOG.
    
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
ASSIGN c-programa 	    = "YG0003RP"
	   c-versao	        = "2.06"
	   c-revisao	    = "00.002"
	   c-empresa 	    = empresa.razao-social
	   c-sistema	    = "UTP"
	   c-titulo-relat   = "Alteraá∆o Plano MAnutená∆o".

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

{utp/ut-liter.i Alterando *}

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
   DEFINE VARIABLE c-narrativa      AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE c-arquivo        AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE ch-excel         AS    COMPONENT-HANDLE      NO-UNDO.
   DEFINE VARIABLE i-cont-linha     AS    INTEGER               NO-UNDO.
   DEFINE VARIABLE c-tipo           AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE c-ordem          AS    CHARACTER             NO-UNDO FORMAT "x(300)".

   ASSIGN c-arquivo = ENTRY(1,tt-param.arquivo,".") + REPLACE(STRING(TODAY,'99/99/9999'),'/','') + REPLACE(STRING(TIME,'hh:mm:ss'),':','') + ".csv".

   FIND FIRST param-global
        NO-LOCK NO-ERROR.

   FILE-INFO:FILE-NAME = c-arquivo.

   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
      OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).

   OUTPUT STREAM st-excel TO VALUE(c-arquivo) NO-CONVERT NO-MAP.

   {utp/ut-field.i ems2cademp man-equip cd-manut     1}. PUT STREAM st-excel RETURN-VALUE               FORMAT "X(25)" ";".              
   {utp/ut-field.i ems2cademp manut descricao        1}. PUT STREAM st-excel RETURN-VALUE               FORMAT "X(25)" ";".              
   {utp/ut-field.i ems2cademp man-equip cd-equipto   1}. PUT STREAM st-excel RETURN-VALUE               FORMAT "X(25)" ";".              
   {utp/ut-field.i ems2cademp equipto descricao      1}. PUT STREAM st-excel RETURN-VALUE               FORMAT "X(25)" ";".              
   {utp/ut-field.i ems2cademp man-equip controle     1}. PUT STREAM st-excel RETURN-VALUE + " (antes)"  FORMAT "X(25)" ";".              
   {utp/ut-field.i ems2cademp man-equip faixa-utiliz 1}. PUT STREAM st-excel RETURN-VALUE + " (antes)"  FORMAT "X(25)" ";". 
   {utp/ut-field.i ems2cademp man-equip controle     1}. PUT STREAM st-excel RETURN-VALUE + " (depois)" FORMAT "X(25)" ";".              
   {utp/ut-field.i ems2cademp man-equip faixa-utiliz 1}. PUT STREAM st-excel RETURN-VALUE + " (depois)" FORMAT "X(25)" ";". 
   
   PUT STREAM st-excel SKIP.

   blk1:
   FOR EACH manut
            WHERE manut.controle = 3
            EXCLUSIVE-LOCK:

      RUN pi-acompanhar IN h-acomp ("Manutená∆o : " + manut.cd-manut + "-" + manut.descricao).

      blk2:
      FOR EACH man-equip OF manut 
               EXCLUSIVE-LOCK,
               FIRST equipto OF man-equip 
               NO-LOCK: 

          /* antes */
          PUT STREAM st-excel
              man-equip.cd-manut     ";"
              manut.descricao        ";"
              man-equip.cd-equipto   ";"
              equipto.descricao      ";"
              ENTRY(man-equip.controle,{ininc/i01in143.i 3}) FORMAT "X(20)" ";"
              man-equip.faixa-utiliz ";".

          ASSIGN man-equip.controle     = 1  
                 man-equip.faixa-utiliz = 0. 

          /* depois */
          PUT STREAM st-excel
              ENTRY(man-equip.controle,{ininc/i01in143.i 3}) FORMAT "X(20)" ";"
              man-equip.faixa-utiliz ";" SKIP.
      END.     

      ASSIGN manut.controle     = 1
             manut.faixa-utiliz = 0.
   END.
     
   OUTPUT STREAM st-excel CLOSE.
   DOS SILENT START excel.exe VALUE(c-arquivo).
END PROCEDURE.
/********************************************************************************************/
/************************************** FIM DO PROGRAMA *************************************/
/********************************************************************************************/


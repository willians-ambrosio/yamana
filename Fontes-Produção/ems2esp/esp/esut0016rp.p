/********************************************************************************************
**  Programa: ESUT0016
**  Data....: 06 de Fevereiro de 2017
**  Autor...: Sergio Luiz Neto da Silveira - DSC
**  Objetivo: Elimina‡Æo de Grupos de Usuarios de Usuarios Inativos
**  Versão..: 2.06.00.001 - Versao Inicial
*********************************************************************************************/

/************************************* INCLUDES PADRAO **************************************/
/* include de controle de versão */
{include/i-prgvrs.i ESUT0016RP 2.06.00.001}
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
    field l-habilitaRtf    as LOG
    FIELD cod_usuario-ini  LIKE usuar_mestre.cod_usuario
    FIELD cod_usuario-fim  LIKE usuar_mestre.cod_usuario
    FIELD dt-corte         LIKE usuar_mestre.dat_fim_valid
    FIELD l-elimina        AS   LOGICAL.
    
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
ASSIGN c-programa 	    = "ESUT0016RP"
	   c-versao	        = "2.06"
	   c-revisao	    = "00.002"
	   c-empresa 	    = empresa.razao-social
	   c-sistema	    = "UTP"
	   c-titulo-relat   = "Elimina‡Æo de Grupos de Usuarios de Usuarios Inativos".

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
   DEFINE VARIABLE c-tipo           AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE c-ordem          AS    CHARACTER             NO-UNDO FORMAT "x(300)".

   ASSIGN c-arquivo = ENTRY(1,tt-param.arquivo,".") + REPLACE(STRING(TODAY,'99/99/9999'),'/','') + REPLACE(STRING(TIME,'hh:mm:ss'),':','') + ".csv".

   FIND FIRST param-global
        NO-LOCK NO-ERROR.

   FILE-INFO:FILE-NAME = c-arquivo.

   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
      OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).

   OUTPUT STREAM st-excel TO VALUE(c-arquivo) NO-CONVERT NO-MAP.

   {utp/ut-field.i emsfnd usuar_mestre cod_usuario      1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i emsfnd usuar_mestre cod_usuario      1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i emsfnd usuar_mestre dat_inic_valid   1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i emsfnd usuar_mestre dat_fim_valid    1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i emsfnd usuar_grp_usuar cod_grp_usuar 1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i emsfnd grp_usuar des_grp_usuar       1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   
   
   PUT STREAM st-excel SKIP.

   blk1:
   FOR EACH usuar_mestre
            WHERE usuar_mestre.cod_usuario   >= tt-param.cod_usuario-ini AND
                  usuar_mestre.cod_usuario   <= tt-param.cod_usuario-fim AND
                  usuar_mestre.dat_fim_valid <= tt-param.dt-corte
            NO-LOCK:

      RUN pi-acompanhar IN h-acomp ("Usuario : " + usuar_mestre.cod_usuario + "-" + usuar_mestre.nom_usuario).

      blk2:
      FOR EACH usuar_grp_usuar OF usuar_mestre 
               EXCLUSIVE-LOCK,                                          
               EACH grp_usuar 
                    WHERE grp_usuar.cod_grp_usuar = usuar_grp_usuar.cod_grp_usuar
                    NO-LOCK: 

          IF grp_usuar.cod_grp_usuar = "*" THEN
             NEXT blk2.

          PUT STREAM st-excel
              "'" usuar_mestre.cod_usuario       ";"
              usuar_mestre.nom_usuario       ";"
              usuar_mestre.dat_inic_valid    ";"
              usuar_mestre.dat_fim_valid     ";"
              usuar_grp_usuar.cod_grp_usuar  ";"
              grp_usuar.des_grp_usuar        ";" 
              SKIP.

         IF tt-param.l-elimina THEN
            DELETE usuar_grp_usuar.
      END.     
   END.
     

   OUTPUT STREAM st-excel CLOSE.
   DOS SILENT START excel.exe VALUE(c-arquivo).
END PROCEDURE.
/********************************************************************************************/
/************************************** FIM DO PROGRAMA *************************************/
/********************************************************************************************/


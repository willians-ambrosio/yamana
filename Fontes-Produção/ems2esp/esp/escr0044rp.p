&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*---------------------------------------------------------------------------------
    Programa    : escr0004rp.p
    Objetivo    : Geracao automatica de implantacao de titulos no Contas a Receber

    Autor       : Marcio Sacramoni - Kraft Consulting
    Data        : 07/07/2009
    Observacoes :
-----------------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.                 */
/*---------------------------------------------------------------------------------*/
def buffer empresa for ems2cadme.empresa.

/* ***************************  Definitions  ************************** */
{include/i-prgvrs.i escr0044rp 2.06.00.000} 
{utp/ut-glob.i} 
DEFINE VARIABLE i-tipo               AS INTEGER                     NO-UNDO.
DEFINE VARIABLE de-valor             AS DECIMAL                     NO-UNDO.
DEFINE VARIABLE ac-port              AS DECIMAL                     NO-UNDO.
DEFINE VARIABLE ac-portj             AS DECIMAL                     NO-UNDO.
DEFINE VARIABLE ac-portd             AS DECIMAL                     NO-UNDO.
DEFINE VARIABLE ac-esp               AS DECIMAL                     NO-UNDO.
DEFINE VARIABLE ac-espj              AS DECIMAL                     NO-UNDO.
DEFINE VARIABLE ac-espd              AS DECIMAL                     NO-UNDO.
DEFINE VARIABLE ac-qtde              AS INTEGER                     NO-UNDO.
DEFINE VARIABLE ac-val-r             AS DECIMAL                     NO-UNDO.
DEFINE VARIABLE h_acomp_rp           AS HANDLE                      NO-UNDO.

DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino          AS INTEGER
    FIELD arquivo          AS CHAR FORMAT "x(35)"
    FIELD usuario          AS CHAR FORMAT "x(12)"
    FIELD data-exec        AS DATE
    FIELD hora-exec        AS INTEGER
    FIELD classifica       AS INTEGER
    FIELD desc-classifica  AS CHAR FORMAT "x(40)"
    FIELD portador-ini     AS INTEGER
    FIELD portador-fim     AS INTEGER
    FIELD data-cre-ini     AS DATE
    FIELD data-cre-fim     AS DATE
    FIELD cod-esp-ini      AS CHAR
    FIELD cod-esp-fim      AS CHAR
    FIELD tipo             AS CHAR.



DEFINE TEMP-TABLE tt-digita NO-UNDO
    FIELD ordem            AS INTEGER   FORMAT ">>>>9"
    FIELD exemplo          AS CHARACTER FORMAT "x(30)"
    INDEX id ordem.

DEFINE TEMP-TABLE tt-raw-digita
    FIELD raw-digita AS RAW.

DEFINE  SHARED TEMP-TABLE tt-erro
    FIELD ITEM                  LIKE ITEM.it-codigo
    FIELD forn                  AS  CHAR
    FIELD item-forn             AS  CHAR
    FIELD menssagem             AS  CHAR.

DEFINE INPUT PARAM raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAM TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

{include/i-rpvar.i} 
                      


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 7.88
         WIDTH              = 38.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

find first empresa no-lock no-error.

ASSIGN c-programa     = "ESCR0044rp"
       c-versao       = "2.06"
       c-revisao      = "00.000"
       c-empresa      =  empresa.razao-social
       c-sistema      = "Especifico"
       c-titulo-relat = "Relatoro de Titulos Implantados/Baixados por Periodo".
                 
/****************DEFINI°€O DE STREANS********************/
DEF STREAM str-rp.

{include/i-rpout.i &STREAM="stream str-rp"}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */

{include/i-rpcab.i &STREAM="str-rp"}

FIND FIRST param-global NO-LOCK NO-ERROR.

ASSIGN c-titulo-relat = "RELATORIO DE PROCESSAMENTO ALTERA€AO ITEM-FORNECEDOR"
       c-sistema      = "ESPECIFICO"
       c-empresa      = (IF AVAIL param-global THEN param-global.grupo ELSE "")
       c-programa     = "ESCR0044RP"
       c-versao       = "I.00"
       c-revisao      = "001".
                                                                      
RUN utp/ut-acomp.p PERSISTENT SET h_acomp_rp.

RUN pi-inicializar IN h_acomp_rp (INPUT "Imprimindo ...").

run pi-desabilita-cancela in h_acomp_rp.

RUN pi-imprime.

RUN pi-finalizar in h_acomp_rp.

DOS SILENT START VALUE(tt-param.arquivo).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-imprime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime Procedure 
PROCEDURE pi-imprime :

VIEW STREAM str-rp FRAME f-cabec.
VIEW STREAM str-rp FRAME f-rodape.
FOR EACH tt-erro NO-LOCK:

    RUN pi-acompanhar IN h_acomp_rp (INPUT "Montando Relatorio: " + STRING(tt-erro.ITEM)).

    
    DISPL STREAM str-rp 
          tt-erro.ITEM
          tt-erro.forn      FORMAT "x(20)"
          tt-erro.item-forn FORMAT "x(20)"
          tt-erro.menssagem FORMAT "x(150)"
          WITH WIDTH 300 DOWN.

END.



/*------ Fechamento do Output de Relat¢rio ------------- */
{include/i-rpclo.i &STREAM="stream str-rp"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


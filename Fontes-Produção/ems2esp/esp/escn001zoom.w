

/* Connected Databases 
          mgesp            PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-ext-medicao-contrat NO-UNDO LIKE ext-medicao-contrat
       field r-rowid as rowid.




/*:T*******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/************************************************************************
**
**  i-prgvrs.i - Programa para criacao do log de todos os programas 
**               e objetos do EMS 2.0 para objetos
**  {1} = objeto   provido pelo Roundtable
**  {2} = versao   provido pelo Roundtable
************************************************************************/

/*Altera‡Æo - 08/09/2006 - tech1007 - Alterado para possuir a defini‡Æo dos pr‚processadores logo no in¡cio do programa*/
/**** Altera‡Æo efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
/*************************************************
* i_dbvers.i - Include de versÆo de banco de dados   
**************************************************/

/* Preprocessadores que identificam os bancos do Produto EMS 5 */

/* Preprocessadores que identificam os bancos do Produto EMS 2 */
/*RAC Incorporado na 2.04*/

/* Preprocessadores que identificam os bancos do Produto HR 2 */

/* Fim */

 
/*Fim altera‡Æo 08/09/2006*/

def new global shared var c-arquivo-log    as char  format "x(60)" no-undo.
def var c-prg-vrs as char init "[[[1.00.00.000[[[" no-undo.
def var c-prg-obj as char no-undo.
assign c-prg-vrs = "1.00.00.000"
       c-prg-obj = "ESPCN001ZOOM".

/* Alteracao - 02/10/2006 - Nakamura - Incluida chamada a include que verifica a integridade dos programas do registro de produto */

 

/*Altera‡Æo - 08/09/2006 - tech1007 - Altera‡Æo para exibir o nome do programa que executou o programa que ser  exibido no extrato de versÆo
                                      Solicita‡Æo realizada na FO 1239827*/

/*Fim altera‡Æo 08/09/2006*/


if  c-arquivo-log <> "" and c-arquivo-log <> ? then do:
    find prog_dtsul
        where prog_dtsul.cod_prog_dtsul = "ESPCN001ZOOM"
        no-lock no-error.
        
   if not avail prog_dtsul then do:
          if  c-prg-obj begins "btb":U then
              assign c-prg-obj = "btb~/":U + c-prg-obj.
          else if c-prg-obj begins "men":U then
                  assign c-prg-obj = "men~/":U + c-prg-obj.
          else if c-prg-obj begins "sec":U then
                  assign c-prg-obj = "sec~/":U + c-prg-obj.
          else if c-prg-obj begins "utb":U then
                  assign c-prg-obj = "utb~/":U + c-prg-obj.
          find prog_dtsul where
               prog_dtsul.nom_prog_ext begins c-prg-obj no-lock no-error.
   end .            /*if*/
    
    output to value(c-arquivo-log) append.

    /*Altera‡Æo - 08/09/2006 - tech1007 - Altera‡Æo para exibir o nome do programa que executou o programa que ser  exibido no extrato de versÆo
                                      Solicita‡Æo realizada na FO 1239827*/
    
        /*FO 1329.898 - tech1139 - 01/08/2006 */
        PUT "ESPCN001ZOOM" AT 1 "1.00.00.000" AT 69 STRING(TODAY,'99/99/99') AT 84 STRING(TIME,'HH:MM:SS':U) AT 94 SKIP.
        /*FO 1329.898 - tech1139 - 01/08/2006 */
    
    /*Fim altera‡Æo 08/09/2006*/
                                                  
    if  avail prog_dtsul then do:
        if  prog_dtsul.nom_prog_dpc <> "" then
            put "DPC : ":U at 5 prog_dtsul.nom_prog_dpc  at 12 skip.
        if  prog_dtsul.nom_prog_appc <> "" then
            put "APPC: ":U at 5 prog_dtsul.nom_prog_appc at 12 skip.
        if  prog_dtsul.nom_prog_upc <> "" then
            put "UPC : ":U at 5 prog_dtsul.nom_prog_upc  at 12 skip.
    end.
    output close.        
end.  
error-status:error = no.
/***************************************************
** i_dbtype.i - Tipo de Gerenciadores utilizados
***************************************************/


            
    /* Preprocessadores que identificam os bancos do Produto EMS 5 */
                    
    /* Preprocessadores que identificam os bancos do Produto EMS 2 */
                                                                                                                                        
    
    /* Preprocessadores que identificam os bancos do Produto HR 2 */
            

/* Fim */

 

/*alteracao Anderson(tech540) em 04/02/2003 Include com a definicao 
da temp table utilizada nas includes btb008za.i1 e btb008za.i2 para 
execucao de programas via rpc*/
def temp-table tt-control-prog NO-UNDO
    field cod-versao-integracao as integer       format '999'
    field cod-erro              as integer       format '99999'
    field desc-erro             as character     format 'x(60)'
    field wgh-servid-rpc        as widget-handle format '>>>>>>9'.
 
 
/*fim alteracao Anderson 04/02/2003*/


/* altera‡Æo feita para atender ao WebEnabler - Marcilene Oliveira - 18/12/2003 */

DEFINE NEW GLOBAL SHARED VARIABLE hWenController AS HANDLE     NO-UNDO.
/*Constante utilizada apenas para verificar se a vari vel acima foi definida */ 

/* fim da alatera‡Æo */

/* Altera‡Æo realizada por tech38629 - 19/07/2006 - Defini‡Æo do pre-processador para o facelift */
/****************************************************************/
/* i_fclpreproc.i                                               */
/* Criado por: tech38629                                        */
/* Data de cria‡Æo: 19/07/2006                                  */
/* Descri‡Æo: Define o pr‚-processador que indica a utiliza‡Æo  */
/*            do facelift                                       */
/****************************************************************/

 
/* Fim da altera‡Æo */

 

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */



CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */







/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* Local Variable Definitions (DBOs Handles) --- */
DEFINE VARIABLE h-esbocn001 AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */


/* Name of designated FRAME-NAME and/or first browse and/or first query */

/* Internal Tables (found by Frame, Query & Browse Queries)             */

/* Definitions for BROWSE brTable1                                      */


/* Definitions for FRAME fPage1                                         */

/* Standard List Definitions                                            */

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */




/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wZoom AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.42
     BGCOLOR 7 .

DEFINE BUTTON btImplant1 
     LABEL "Implantar" 
     SIZE 10 BY 1.

/* Query definitions                                                    */

DEFINE QUERY brTable1 FOR 
      tt-ext-medicao-contrat SCROLLING.


/* Browse definitions                                                   */
DEFINE BROWSE brTable1

  QUERY brTable1 NO-LOCK DISPLAY
      tt-ext-medicao-contrat.nr-contrato FORMAT ">>>>>>>>9":U WIDTH 14.43
      tt-ext-medicao-contrat.numero-ordem FORMAT "zzzzz9,99":U
            WIDTH 12.43
      tt-ext-medicao-contrat.num-seq-item FORMAT ">,>>9":U
      tt-ext-medicao-contrat.num-seq-event FORMAT ">,>>9":U
      tt-ext-medicao-contrat.num-seq-medicao FORMAT ">,>>9":U
      tt-ext-medicao-contrat.nr-trans FORMAT "->>>,>>>,>>9":U WIDTH 18.86
/* _UIB-CODE-BLOCK-END */

    WITH NO-ROW-MARKERS SEPARATORS SIZE 82 BY 10
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btOK AT ROW 16.71 COL 2
     btCancel AT ROW 16.71 COL 13
     btHelp AT ROW 16.71 COL 80
     rtToolBar AT ROW 16.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 16.98
         FONT 1 WIDGET-ID 100.

DEFINE FRAME fPage1
     brTable1 AT ROW 3 COL 2
     btImplant1 AT ROW 13 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.5 ROW 2.45
         SIZE 84.43 BY 13.29
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Zoom
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-ext-medicao-contrat T "?" NO-UNDO mgesp ext-medicao-contrat
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
   END-TABLES.
 */


/* *************************  Create Window  ************************** */


IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wZoom ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 17
         WIDTH              = 90
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 90
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE wZoom = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */



/* ************************* Included-Libraries *********************** */


/* Procedure Description
"Method Library principal para Zoom Template, que cont‚m defini‡äes e chamadas a outras Method Libraries."
*/


/*--------------------------------------------------------------------------
    Library    : zoom/Zoom.i
    Purpose    : Method Library principal para Zoom Template, que cont‚m 
                 defini‡äes e chamadas a outras Method Libraries 

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* Global Variable Definitions ---                                        */
DEFINE NEW GLOBAL SHARED VARIABLE hWindowStyles AS HANDLE NO-UNDO.


        
    DEFINE NEW GLOBAL SHARED VARIABLE gr-ext-medicao-contrat AS ROWID NO-UNDO.


/* Global Variable Definitions ---                                        */


/* Global Variable Definitions ---                                        */


/* Global Variable Definitions ---                                        */


/* Global Variable Definitions ---                                        */


/* Global Variable Definitions ---                                        */


/* Global Variable Definitions ---                                        */


/* Global Variable Definitions ---                                        */


/* Definir utilização de folder */

    

/* Local Temp-Table Definitions ---                                       */
/*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/

    
/* Procedure Description
"Include com defini‡Æo da temptable RowErrors."
*/


/*--------------------------------------------------------------------------
    Library    : method/dbotterr.i
    Purpose    : Include com defini‡Æo da temptable RowErrors

    Author     : John Cleber Jaraceski

    Notes      :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  **************************** */

DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER.

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */



 


/*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
/*&IF DEFINED(DBOVersion) <> 0 &THEN*/
    
        DEFINE TEMP-TABLE ttTable1Aux NO-UNDO LIKE tt-ext-medicao-contrat.
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
/*&ELSE*/
    
/* Procedure Description
"Include com defini‡Æo da temptable RowRaw."
*/


/*--------------------------------------------------------------------------
    Library    : method/dbottraw.i
    Purpose    : Include com defini‡Æo da temptable RowRaw

    Author     : John Cleber Jaraceski

    Notes      :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  **************************** */

DEFINE TEMP-TABLE RowRaw NO-UNDO
    FIELD RawRecord AS RAW.

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */



 
/*&ENDIF*/

/* Local Variable Definitions ---                                         */
DEFINE VARIABLE cEventBrowse          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldNames           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldHandles         AS CHARACTER NO-UNDO.
DEFINE VARIABLE hFolder               AS HANDLE    NO-UNDO.
DEFINE VARIABLE hProgramImplant       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hRange                AS HANDLE    NO-UNDO.
DEFINE VARIABLE hShowMsg              AS HANDLE    NO-UNDO.
DEFINE VARIABLE hWindowParent         AS HANDLE    NO-UNDO.
DEFINE VARIABLE iConstraintPageNumber AS INTEGER   NO-UNDO.
DEFINE VARIABLE iRepositionPageNumber AS INTEGER   NO-UNDO.
DEFINE VARIABLE iRowsReturned         AS INTEGER   NO-UNDO.
DEFINE VARIABLE lCustomExecuted       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lOverrideExecuted     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE rRepositionTable      AS ROWID     NO-UNDO.
DEFINE VARIABLE hFirstField           AS handle     NO-UNDO.


DEFINE VARIABLE c-nom-prog-dpc-mg97  AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-nom-prog-appc-mg97 AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-nom-prog-upc-mg97  AS CHARACTER NO-UNDO.

DEFINE VARIABLE epc-rowid1           AS ROWID     NO-UNDO.
DEFINE VARIABLE epc-rowid2           AS ROWID     NO-UNDO.
DEFINE VARIABLE epc-rowid3           AS ROWID     NO-UNDO.
DEFINE VARIABLE epc-rowid4           AS ROWID     NO-UNDO.
DEFINE VARIABLE epc-rowid5           AS ROWID     NO-UNDO.
DEFINE VARIABLE epc-rowid6           AS ROWID     NO-UNDO.
DEFINE VARIABLE epc-rowid7           AS ROWID     NO-UNDO.
DEFINE VARIABLE epc-rowid8           AS ROWID     NO-UNDO.


/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
    define new global shared variable h-facelift as handle no-undo.
    if not valid-handle(h-facelift) then run btb/btb901zo.p persistent set h-facelift.


/*--- Vari veis utilizadas nos eventos de CURSOR-DOWN, END, OFF-END e PAGE-DOWN ---*/

    DEFINE VARIABLE rCurrent1 AS ROWID NO-UNDO.
















/* Function Definitions ---                                               */

    FUNCTION fnIniRangeChar RETURN CHAR    (INPUT pPageNumber AS INTEGER) IN hRange.
    FUNCTION fnEndRangeChar RETURN CHAR    (INPUT pPageNumber AS INTEGER) IN hRange.
    FUNCTION fnIniRangeInt  RETURN INTEGER (INPUT pPageNumber AS INTEGER) IN hRange.
    FUNCTION fnEndRangeInt  RETURN INTEGER (INPUT pPageNumber AS INTEGER) IN hRange.
    FUNCTION fnIniRangeDate RETURN DATE    (INPUT pPageNumber AS INTEGER) IN hRange.
    FUNCTION fnEndRangeDate RETURN DATE    (INPUT pPageNumber AS INTEGER) IN hRange.
    FUNCTION fnIniRangeDec  RETURN DECIMAL (INPUT pPageNumber AS INTEGER) IN hRange.
    FUNCTION fnEndRangeDec  RETURN DECIMAL (INPUT pPageNumber AS INTEGER) IN hRange.

    FUNCTION fnIniRangeCharPage RETURN CHAR    (INPUT pPageNumber AS INTEGER, INPUT pFieldNum AS INTEGER) IN hRange.
    FUNCTION fnEndRangeCharPage RETURN CHAR    (INPUT pPageNumber AS INTEGER, INPUT pFieldNum AS INTEGER) IN hRange.
    FUNCTION fnIniRangeIntPage  RETURN INTEGER (INPUT pPageNumber AS INTEGER, INPUT pFieldNum AS INTEGER) IN hRange.
    FUNCTION fnEndRangeIntPage  RETURN INTEGER (INPUT pPageNumber AS INTEGER, INPUT pFieldNum AS INTEGER) IN hRange.
    FUNCTION fnIniRangeDatePage RETURN DATE    (INPUT pPageNumber AS INTEGER, INPUT pFieldNum AS INTEGER) IN hRange.
    FUNCTION fnEndRangeDatePage RETURN DATE    (INPUT pPageNumber AS INTEGER, INPUT pFieldNum AS INTEGER) IN hRange.
    FUNCTION fnIniRangeDecPage  RETURN DECIMAL (INPUT pPageNumber AS INTEGER, INPUT pFieldNum AS INTEGER) IN hRange.
    FUNCTION fnEndRangeDecPage  RETURN DECIMAL (INPUT pPageNumber AS INTEGER, INPUT pFieldNum AS INTEGER) IN hRange.



/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 



/* ************************* Included-Libraries *********************** */

{utp/ut-glob.i}

/********************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

/********************************************************************************
** Programa : include/i-sysvar.i
**
** Data : 02/06/1999
**
** Cria‡Æo : John Cleber Jaraceski
**
** Objetivo : Definicao das System Variables
**
** Ultima Alt : ?
*******************************************************************************/


        
    DEFINE VARIABLE c-programa-mg97       AS CHARACTER FORMAT "x(08)":U NO-UNDO.
    DEFINE VARIABLE c-versao-mg97         AS CHARACTER FORMAT "x(08)":U NO-UNDO.
    DEFINE VARIABLE c-modulo-mg97         AS CHARACTER FORMAT "x(08)":U NO-UNDO.
    DEFINE VARIABLE c-titulo-prog-mg97    AS CHARACTER FORMAT "x(08)":U NO-UNDO.
    DEFINE VARIABLE c-nom-manual-hlp-mg97 AS CHARACTER FORMAT "x(06)":U NO-UNDO.
    DEFINE VARIABLE c-cod-mod-mg97        AS CHARACTER                  NO-UNDO.
    DEFINE VARIABLE d-data-contrato       AS DATE                       NO-UNDO.
    DEFINE VARIABLE i-num-topico-hlp-mg97 AS INTEGER                    NO-UNDO.
    DEFINE VARIABLE i-user-conectados     AS INTEGER                    NO-UNDO.
    DEFINE VARIABLE i-licenca-usuar       AS INTEGER                    NO-UNDO.
    DEFINE VARIABLE l-acesso-livre        AS LOGICAL                    NO-UNDO.


/* include/i-sysvar.i ---                                                     */

 
/***************************************************************************
**
**   btb008za.i0  -  Include para defini‡Æo de fun‡äes do RPC.
**
***************************************************************************/

FUNCTION rpc_exec         RETURNS logical   (input p_cod_program as character) in h-servid-rpc.
FUNCTION rpc_server       RETURNS handle    (input p_cod_program as character) in h-servid-rpc.
FUNCTION rpc_program      RETURNS character (input p_cod_program as character) in h-servid-rpc.
FUNCTION rpc_tip_exec     RETURNS logical   (input p_cod_program as character) in h-servid-rpc.
FUNCTION rpc_exec_set     RETURNS logical   (input p_cod_program as character, 
                                             input p_log_value as logical)     in h-servid-rpc.
                                             
DEFINE VARIABLE c-lst-prg-rpc as char    no-undo.
DEFINE VARIABLE l-inf-prg     as logical no-undo.
DEFINE VARIABLE i-prg-rpc     as integer no-undo.
 

/* Procedure Description
"Method Library que cont‚m as l¢gicas dos botäes do ToolBar."
*/


/*--------------------------------------------------------------------------
    Library    : zoom/Buttons.i
    Purpose    : Method Library que cont‚m as l¢gicas dos botäes do ToolBar 

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 2
         WIDTH              = 39.86.
/* END WINDOW DEFINITION */
                                                                        */

 



/* ************************* Included-Libraries *********************** */

/* _UIB-CODE-BLOCK-END */







/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */




PROCEDURE returnValues :
/*------------------------------------------------------------------------------
  Purpose:     Executa m‚todo para retorna de campos conforme a p gina 
               selecionada
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE cFieldValueAux  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hFieldHandleAux AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iNumFieldAux    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iNumPageAux     AS INTEGER   NO-UNDO.

    IF  VALID-HANDLE(hWindowParent) THEN
        ASSIGN hWindowParent:SENSITIVE = YES.
    
    /*--- Retorna o n£mero da p gina corrente ---*/
    
        RUN getCurrentFolder IN hFolder (OUTPUT iNumPageAux).
    
    
    /*--- Verifica se deve ser retornado o rowid do registro corrente, 
          neste caso ser  retornado o rowid da p gina corrente ---*/
    IF cFieldNames = "ROWID":U THEN DO:
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
            IF iNumPageAux = 1 THEN DO:
                IF AVAILABLE tt-ext-medicao-contrat THEN
                    RUN repositionRecord IN WIDGET-HANDLE(cFieldHandles) (INPUT tt-ext-medicao-contrat.r-Rowid).
                
                RETURN "OK":U.
            END.
        
    END.
    
    DO iNumFieldAux = 1 TO NUM-ENTRIES(cFieldNames):
        /*--- Executa m‚todo para retorna do valor do campo ---*/
        RUN VALUE("returnFieldsPage":U + STRING(iNumPageAux)) IN THIS-PROCEDURE ( INPUT ENTRY(iNumFieldAux, cFieldNames),
                                                                                 OUTPUT cFieldValueAux).
        
        /*--- Seta propriedade SCREEN-VALUE com os valores dos campos a 
              serem retornados ---*/
        ASSIGN hFieldHandleAux = WIDGET-HANDLE(ENTRY(iNumFieldAux, cFieldHandles))
               hFieldHandleAux:SCREEN-VALUE = cFieldValueAux.
    END.
    
    /*--- Aplica ENTRY para o primeiro campo a ser retornado ---*/
    ASSIGN hFieldHandleAux = WIDGET-HANDLE(ENTRY(1, cFieldHandles)).
    APPLY "ENTRY":U TO hFieldHandleAux.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */




 
/*** Alterado por Farley - em 23/07/2003 ***/
 

/* _UIB-CODE-BLOCK-END */







/* ***************************  Main Block  *************************** */


/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
run pi_aplica_facelift in this-procedure.


/*Alterado por Valdir (tech264) para fazer tratamento da trigger de help*/
ON HELP OF wZoom DO:
    apply "choose":U to btHelp in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */

PROCEDURE GetSystemTime EXTERNAL "KERNEL32.DLL":U PERSISTENT:
    DEFINE OUTPUT PARAMETER lpSystemTime AS MEMPTR NO-UNDO.
END.




PROCEDURE applyCursorDown :
/*------------------------------------------------------------------------------
  Purpose:     L¢gica para trigger de CURSOR-DOWN
  Parameters:  recebe o n£mero da p gina
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    /*--- Seta vari vel cEventBrowse com o valor CURSOR-DOWN ---*/
    ASSIGN cEventBrowse = "CURSOR-DOWN":U.
    
    CASE pPageNumber:
        
            WHEN 1 THEN
                /*--- Seta vari vel rCurrent1 com o valor do rowid corrente ---*/
                ASSIGN rCurrent1 = IF AVAILABLE tt-ext-medicao-contrat
                                       THEN ROWID(tt-ext-medicao-contrat)
                                       ELSE ?.
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
    END CASE.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE applyCursorUp :
/*------------------------------------------------------------------------------
  Purpose:     L¢gica para trigger de CURSOR-UP
  Parameters:  recebe o n£mero da p gina
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    /*--- Seta vari vel cEventBrowse com o valor CURSOR-UP ---*/
    ASSIGN cEventBrowse = "CURSOR-UP":U.
    
    CASE pPageNumber:
        
            WHEN 1 THEN
                /*--- Seta vari vel rCurrent1 com o valor do rowid corrente ---*/
                ASSIGN rCurrent1 = IF AVAILABLE tt-ext-medicao-contrat
                                       THEN ROWID(tt-ext-medicao-contrat)
                                       ELSE ?.
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
    END CASE.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE applyValueChanged :
/*------------------------------------------------------------------------------
  Purpose:     L¢gica para trigger de VALUE-CHANGED
  Parameters:  recebe o n£mero da p gina
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    CASE pPageNumber:
        
            WHEN 1 THEN DO:

                IF AVAILABLE tt-ext-medicao-contrat THEN 
                    ASSIGN epc-rowid1 = tt-ext-medicao-contrat.r-Rowid.
                ELSE 
                    ASSIGN epc-rowid1 = ?.

                /*--- Executa programas de customiza‡Æo (before/after) ---*/
                
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "BEFORE-VALUE-CHANGED":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "ext-medicao-contrat":U,
                                    INPUT epc-rowid1).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-VALUE-CHANGED":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "ext-medicao-contrat":U,
                                    INPUT epc-rowid1).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-VALUE-CHANGED":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "ext-medicao-contrat":U,
                                    INPUT epc-rowid1).
END.

/* _UIB-CODE-BLOCK-END */



 
                
                /*--- Executa programas de customiza‡Æo (before/after) ---*/
                
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "AFTER-VALUE-CHANGED":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "ext-medicao-contrat":U,
                                    INPUT epc-rowid1).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-VALUE-CHANGED":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "ext-medicao-contrat":U,
                                    INPUT epc-rowid1).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-VALUE-CHANGED":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "ext-medicao-contrat":U,
                                    INPUT epc-rowid1).
END.

/* _UIB-CODE-BLOCK-END */



 
            END.
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
    END CASE.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE applyDblClick :
/*------------------------------------------------------------------------------
  Purpose:     L¢gica para trigger de DBL-CLICK
  Parameters:  recebe o n£mero da p gina
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    /*nao foi possivel efetuar um apply choose to btok por  
     que o programa estava perdendo o foco qdo chave estrangeira*/
    
    RUN returnValues IN THIS-PROCEDURE.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    
    RETURN RETURN-VALUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE applyEnd :
/*------------------------------------------------------------------------------
  Purpose:     L¢gica para trigger de END
  Parameters:  recebe o n£mero da p gina
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE rLastCreated AS ROWID NO-UNDO.
    DEFINE VARIABLE rLast     AS ROWID NO-UNDO.
    
    /*--- Testa o pre-processador RowNumDefinedN para todas as p ginas para
          definir iLastSeq que ‚ utilizada no c lculo do valor do rowNum    ---*/
    
    
    CASE pPageNumber:
        
            WHEN 1 THEN DO:
                /*--- Seta vari vel rLast com o valor do £ltimo rowid da 
                      tabela {&ttTable1} existente no browse ---*/
                GET LAST brTable1 NO-LOCK.
                IF AVAILABLE tt-ext-medicao-contrat THEN
                    ASSIGN rLast = tt-ext-medicao-contrat.r-Rowid.
                ELSE
                    ASSIGN rLast = ?.
                
                /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                
                    /*--- Retorna todos os registros do DBO filho, na temp-table ttTable1Aux ---*/
                    RUN getBatchRecords IN h-esbocn001 (INPUT rLast,
                                                        INPUT IF rLast = ? THEN NO ELSE YES,
                                                        INPUT ?,
                                                        OUTPUT iRowsReturned,
                                                        OUTPUT TABLE ttTable1Aux).
                
                
                /*--- Cancela trigger caso nÆo existam mais registros no DBO filho ---*/
                IF CAN-FIND(FIRST ttTable1Aux) THEN DO:
                    /*--- Transfere dados da temp-table ttTable1Aux para a temp-table {&ttTable1} ---*/
                    
                    FOR EACH ttTable1Aux:
                        CREATE tt-ext-medicao-contrat.

                        
                            BUFFER-COPY ttTable1Aux TO tt-ext-medicao-contrat.
                        
                        ASSIGN rLastCreated = ROWID(tt-ext-medicao-contrat).
                    END.
                
                    /*--- Abre o browse filho, para atualiza‡Æo dos dados ---*/
                    OPEN QUERY brTable1 FOR EACH tt-ext-medicao-contrat NO-LOCK.
                    
                    /*--- Seta view-port do browse filho para reposicionamento ---*/
                    brTable1:SET-REPOSITIONED-ROW(brTable1:DOWN IN FRAME fPage1) IN FRAME fPage1.
                    
                    /*--- Reposiciona browse filho no £ltimo registro ---*/
                    REPOSITION brTable1 TO ROWID rLastCreated NO-ERROR.
                    GET NEXT brTable1 NO-LOCK.
                    
                    /*--- Posiciona browse filho no pr¢ximo registro ---*/
                    brTable1:SELECT-FOCUSED-ROW().
                END. /*--- CAN-FIND FIRST {&ttTable1} ---*/
            END.
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
    END CASE.
    
    /*--- Seta vari vel cEventBrowse com o valor "" ---*/
    ASSIGN cEventBrowse = "":U.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE applyHome :
/*------------------------------------------------------------------------------
  Purpose:     L¢gica para trigger de HOME
  Parameters:  recebe o n£mero da p gina
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    /*INSERIDO TESTE DO PRE-PROCESSADOR &DBOVersion PARA QUE O CàDIGO ABAIXO SEJA EXECUTADO*/
    /*SOMENTE QUANDO O DBO FOR 2.O*/
    /*ALTERA€ÇO FEITA POR ANDERSON (TECH485) EM 04/04/2001 PARA EVITAR QUE QUANDO SE NAVEGUE PARA*/
    /*CIMA EM UM BROWSE O MESMO NÇO DE O ERRO DE REGISTRO DUPLICADO*/
    /*TESTE PRE-PROCESSADOR(VERSÇO DO BO)*/
    

        DEFINE VARIABLE rLastCreated AS ROWID NO-UNDO.
        DEFINE VARIABLE rFirst    AS ROWID NO-UNDO.
        
        /*--- Testa o pre-processador RowNumDefinedN para todas as p ginas para
              definir iFirstSeq que ‚ utilizada no c lculo do valor do rowNum    ---*/
        

        CASE pPageNumber:
    
            
                WHEN 1 THEN DO:
                    /*--- Seta vari vel rFirst com o valor do primeiro rowid 
                          da tabela {&ttTable1} existente no browse               ---*/
                    GET FIRST brTable1 NO-LOCK.
                    IF AVAILABLE tt-ext-medicao-contrat THEN DO:
                        ASSIGN rFirst = tt-ext-medicao-contrat.r-Rowid.
                            
                        /*--- Seta ponteiro do browse no registro correto ---*/
                        BROWSE brTable1:SELECT-FOCUSED-ROW().
                    END.
                    ELSE
                        ASSIGN rFirst = ?.
                        
                    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                    
                        /*--- Retorna todos os registros do DBO filho, na temp-table ttTable1Aux ---*/
                        RUN getBatchRecordsPrev IN h-esbocn001 (INPUT rFirst,
                                                                INPUT IF rFirst = ? THEN NO ELSE YES,
                                                                INPUT ?,
                                                                OUTPUT iRowsReturned,
                                                                OUTPUT TABLE ttTable1Aux).
                    
                        
                    /*--- Cancela trigger caso nÆo existam mais registros no DBO filho ---*/
                    IF CAN-FIND(FIRST ttTable1Aux) THEN DO:
    
                        /*--- Transfere dados da temp-table ttTable1Aux para a temp-table {&ttTable1} ---*/
                        
                        FOR EACH ttTable1Aux:
                            CREATE tt-ext-medicao-contrat.
                            
                                BUFFER-COPY ttTable1Aux TO tt-ext-medicao-contrat.
                            
                            ASSIGN rLastCreated = ROWID(tt-ext-medicao-contrat).
                        END.
                        
                        /*--- Abre o browse filho, para atualiza‡Æo dos dados ---*/
                        OPEN QUERY brTable1 FOR EACH tt-ext-medicao-contrat NO-LOCK.
                        
                        /*--- Seta view-port do browse filho para reposicionamento ---*/
                        brTable1:SET-REPOSITIONED-ROW(brTable1:DOWN IN FRAME fPage1) IN FRAME fPage1.
                        
                        /*--- Reposiciona browse filho no £ltimo registro ---*/
                        REPOSITION brTable1 TO ROWID rLastCreated NO-ERROR.
                        GET NEXT brTable1 NO-LOCK.
                        
                        /*--- Posiciona browse filho no pr¢ximo registro ---*/
                        brTable1:SELECT-FOCUSED-ROW().
                    END. /*--- END DO CAN-FIND {&ttTable1} ---*/
                END.
            
            
            
            
            

            
            

            
            

            
            

            
            

            
            
    
        END CASE.
    
    
    /*FIM DO TESTE DO PREPROCESSADOR &DBOVERSION(VERSÇO DO DBO)*/
    /*FIM ALTERA€ÇO ANDERSON (TECH485) 04/04/2001*/
    
    
    /*--- Seta vari vel cEventBrowse com o valor "" ---*/
    ASSIGN cEventBrowse = "":U.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE applyOffEnd :
/*------------------------------------------------------------------------------
  Purpose:     L¢gica para trigger de OFF-END
  Parameters:  recebe o n£mero da p gina
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    
    DEFINE VARIABLE rLast AS ROWID NO-UNDO.
    DEFINE VARIABLE rReposition AS ROWID NO-UNDO.
    
    /*--- Testa o pre-processador RowNumDefinedN para todas as p ginas para
          definir iLastSeq que ‚ utilizada no c lculo do valor do rowNum    ---*/
    

    /*--- Tratar evento de SCROLL-NOTIFY do Browse ---*/
    IF cEventBrowse = "":U THEN
        ASSIGN cEventBrowse = "SCROLL-NOTIFY":U.
    
    CASE pPageNumber:
        
            WHEN 1 THEN DO WITH FRAME fPage1:
                /*--- Seta vari vel rLast com o valor do £ltimo rowid 
                      da tabela {&ttTable1} existente no browse            ---*/
                GET LAST brTable1 NO-LOCK.
                IF AVAILABLE tt-ext-medicao-contrat THEN
                    ASSIGN rLast = tt-ext-medicao-contrat.r-Rowid
                           rReposition = rowid(tt-ext-medicao-contrat).
                ELSE
                    ASSIGN rLast = ?
                           rReposition = ?.
                
                IF  brTable1:MULTIPLE AND rReposition <> ? THEN
                    ASSIGN rCurrent1 = rReposition.
                
                /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                
                    /*--- Retorna registros do DBO filho, na temp-table ttTable1Aux ---*/
                    RUN getBatchRecords IN h-esbocn001 (INPUT rLast,
                                                        INPUT IF rLast = ? THEN NO ELSE YES,
                                                        INPUT 25
                                                              ,
                                                        OUTPUT iRowsReturned,
                                                        OUTPUT TABLE ttTable1Aux).
                
                
                /*--- Posiciona no £ltimo registro caso nÆo existam mais registros no DBO filho ---*/
                IF CAN-FIND(FIRST ttTable1Aux) THEN DO:
                    /*--- Transfere dados da temp-table ttTable1Aux 
                          para a temp-table {&ttTable1}   ---*/
                    
                    FOR EACH ttTable1Aux:
                        CREATE tt-ext-medicao-contrat.

                        
                            BUFFER-COPY ttTable1Aux TO tt-ext-medicao-contrat.
                        
                    END.

                    /*--- Abre o browse filho, para atualiza»’o dos dados ---*/
                    OPEN QUERY brTable1 FOR EACH tt-ext-medicao-contrat NO-LOCK.
                END.
                
                /*--- Posiciona browse, no pr¢ximo registro ---*/
                CASE cEventBrowse:
                    WHEN "PAGE-DOWN":U THEN DO:
                        /*--- Seta view-port do browse para reposicionamento ---*/
                        brTable1:SET-REPOSITIONED-ROW(1).
                        
                        IF  rCurrent1 <> ? THEN DO:
                            /*--- Reposiciona browse no registro atual ---*/
                            REPOSITION brTable1 TO ROWID rCurrent1 NO-ERROR.
                            GET NEXT brTable1 NO-LOCK.
                            
                            /*--- Seleciona o £ltimo registro do view-port do browse ---*/
                            brTable1:SELECT-ROW(brTable1:NUM-ITERATIONS IN FRAME fPage1).
                        END.
                    END.
                    OTHERWISE DO:
                        /*--- Seta view-port do browse para reposicionamento ---*/
                        brTable1:SET-REPOSITIONED-ROW(brTable1:DOWN IN FRAME fPage1 - 1) IN FRAME fPage1. 
                        
                        IF  rReposition <> ? THEN DO:
                            /*--- Reposiciona browse no registro atual ---*/
                            REPOSITION brTable1 TO ROWID rReposition NO-ERROR.
                            GET NEXT brTable1 NO-LOCK.
                            
                            /*--- Posiciona browse no pr¢ximo registro ---*/
                            brTable1:SELECT-FOCUSED-ROW().
                            brTable1:SELECT-NEXT-ROW().
                        END.
                    END.
                END CASE.
            END.
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
    END CASE.
    
    
    /*--- Seta vari vel cEventBrowse com o valor "" ---*/
    ASSIGN cEventBrowse = "":U.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */








PROCEDURE applyOffHome :
/*------------------------------------------------------------------------------
  Purpose:     L¢gica para trigger de OFF-HOME
  Parameters:  recebe o n£mero da p gina
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    
    /*INSERIDO TESTE DO PRE-PROCESSADOR &DBOVersion PARA QUE O CàDIGO ABAIXO SEJA EXECUTADO*/
    /*SOMENTE QUANDO O DBO FOR 2.O*/
    /*ALTERA€ÇO FEITA POR ANDERSON (TECH485) EM 04/04/2001 PARA EVITAR QUE QUANDO SE NAVEGUE PARA*/
    /*CIMA EM UM BROWSE O MESMO NÇO DE O ERRO DE REGISTRO DUPLICADO*/
    /*TESTE PRE-PROCESSADOR(VERSÇO DO BO)*/
    
    
        DEFINE VARIABLE rFirst AS ROWID NO-UNDO.
        DEFINE VARIABLE rReposition AS ROWID NO-UNDO.
    
        /*--- Testa o pre-processador RowNumDefinedN para todas as p ginas para
          definir iLastSeq que ‚ utilizada no c lculo do valor do rowNum    ---*/
        
    
        /*--- Tratar evento de SCROLL-NOTIFY do Browse ---*/
        IF cEventBrowse = "":U THEN
            ASSIGN cEventBrowse = "SCROLL-NOTIFY":U.


        
        CASE pPageNumber:
            
                WHEN 1 THEN DO WITH FRAME fPage1:
                    /*--- Seta vari vel rFirst com o valor do primeiro rowid da 
                          tabela {&ttTable1} existente no browse ---*/
                    GET FIRST brTable1 NO-LOCK.
                    IF AVAILABLE tt-ext-medicao-contrat THEN
                        ASSIGN rFirst = tt-ext-medicao-contrat.r-Rowid
                               rReposition = ROWID(tt-ext-medicao-contrat).
                    ELSE
                        ASSIGN rFirst = ?
                               rReposition = ?.
                
                    IF  brTable1:MULTIPLE AND rReposition <> ? THEN
                        ASSIGN rCurrent1 = rReposition.
            
                    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
                    
                        /*--- Retorna registros do DBO filho, na temp-table ttTable1Aux ---*/
                        RUN getBatchRecordsPrev IN h-esbocn001 (INPUT rFirst,
                                                                INPUT IF rFirst = ? THEN NO ELSE YES,
                                                                INPUT 25
                                                                      ,
                                                                OUTPUT iRowsReturned,
                                                                OUTPUT TABLE ttTable1Aux).
                    
                    
                    IF  CAN-FIND(FIRST tt-ext-medicao-contrat) THEN DO:
                        /*--- Transfere dados da temp-table ttTable1Aux para a temp-table {&ttTable1} ---*/
                        
                        FOR EACH ttTable1Aux:
                            CREATE tt-ext-medicao-contrat.
                            
                                BUFFER-COPY ttTable1Aux TO tt-ext-medicao-contrat.
                            
                        END.
                    
                        /*--- Abre o browse filho, para atualiza‡Æo dos dados ---*/
                        OPEN QUERY brTable1 FOR EACH tt-ext-medicao-contrat NO-LOCK.
                    END.    /*--- CAN FIND FIRST {&ttTable1} ---*/
                
                    /*--- Posiciona browse, no pr¢ximo registro ---*/
                    CASE cEventBrowse:
                        WHEN "PAGE-UP":U THEN DO:
                            /*--- Seta view-port do browse para reposicionamento ---*/
                            brTable1:SET-REPOSITIONED-ROW(brTable1:DOWN IN FRAME fPage1).
                        
                            IF  rCurrent1 <> ? THEN DO:
                                /*--- Reposiciona browse no registro atual ---*/
                                REPOSITION brTable1 TO ROWID rCurrent1 NO-ERROR.
                                GET NEXT brTable1 NO-LOCK.
                            
                                /*--- Seleciona o £ltimo registro do view-port do browse ---*/
                                brTable1:SELECT-ROW(1).
                            END.
                        END.
                        OTHERWISE DO:
                            /*--- Seta view-port do browse para reposicionamento ---*/
                            brTable1:SET-REPOSITIONED-ROW(1) IN FRAME fPage1.
                        
                            IF  rReposition <> ? THEN DO:
                                /*--- Reposiciona browse no registro atual ---*/
                                REPOSITION brTable1 TO ROWID rReposition NO-ERROR.
                                GET NEXT brTable1 NO-LOCK.
                            
                                /*--- Posiciona browse no pr¢ximo registro ---*/
                                brTable1:SELECT-FOCUSED-ROW().
                                brTable1:SELECT-PREV-ROW().
                            END.
                        END.
                    END CASE.
                END.
            
        
            
        
            
        
            
        
            
        
            
        
            
        
            
        END CASE.
    
    
    /*FIM ALTERA€ÇO ANDERSON (TECH485) 04/04/2001*/
   
   
    /*--- Seta vari vel cEventBrowse com o valor "" ---*/
    ASSIGN cEventBrowse = "":U.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE applyPageDown :
/*------------------------------------------------------------------------------
  Purpose:     L¢gica para trigger de PAGE-DOWN
  Parameters:  recebe o n£mero da p gina
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    /*--- Seta vari vel cEventBrowse com o valor PAGE-DOWN ---*/
    ASSIGN cEventBrowse = "PAGE-DOWN":U.
    
    CASE pPageNumber:
        
            WHEN 1 THEN DO:
                /*--- Seta vari vel rCurrent1 com o valor do rowid corrente ---*/
                ASSIGN rCurrent1 = IF AVAILABLE tt-ext-medicao-contrat
                                       THEN ROWID(tt-ext-medicao-contrat)
                                       ELSE ?.
            END.
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
    END CASE.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE applyPageUp :
/*------------------------------------------------------------------------------
  Purpose:     L¢gica para trigger de PAGE-UP
  Parameters:  recebe o n£mero da p gina
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    /*--- Seta vari vel cEventBrowse com o valor PAGE-UP ---*/
    ASSIGN cEventBrowse = "PAGE-UP":U.
    
    CASE pPageNumber:
        
            WHEN 1 THEN DO:
                /*--- Seta vari vel rCurrent1 com o valor do rowid corrente ---*/
                ASSIGN rCurrent1 = IF AVAILABLE tt-ext-medicao-contrat
                                       THEN ROWID(tt-ext-medicao-contrat)
                                       ELSE ?.
            END.
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
    END CASE.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE destroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     Destr¢i programa
  Parameters:  
  Notes:       Destr¢i programas de: Folder e DBOs
------------------------------------------------------------------------------*/
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("BeforedestroyInterface":U) <> "":U THEN DO:
    
        RUN BeforedestroyInterface IN THIS-PROCEDURE NO-ERROR.
    

    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "BEFORE-DESTROY-INTERFACE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-DESTROY-INTERFACE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-DESTROY-INTERFACE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

/* _UIB-CODE-BLOCK-END */



 
                     
    /*Inserida a chamada da include i-logfin1.i
    devido aos thintemplates nÆo gerarem log de 
    execucao de programas*/
    /*************************************************************************
**
** I-LOGFIN1.I - Encerra o Log de Execucao
**
**************************************************************************/

run btb/btb918zb.p (input c-programa-mg97,
                    input-output rw-log-exec,
                    input no).
 
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Destr¢i DBO Table1 ---*/
        
            IF VALID-HANDLE(h-esbocn001) THEN
                RUN destroy IN h-esbocn001.
        
    
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Destr¢i DBO Table2 ---*/
        
    
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Destr¢i DBO Table3 ---*/
        
    
        
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Destr¢i DBO Table4 ---*/
        
    
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Destr¢i DBO Table5 ---*/
        
    
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Destr¢i DBO Table6 ---*/
        
    
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Destr¢i DBO Table7 ---*/
        
    
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Destr¢i DBO Table8 ---*/
        
    
    
    /*--- Destr¢i programa de range ---*/
    
        IF VALID-HANDLE(hRange) THEN
            DELETE PROCEDURE hRange.
    
    
    /*--- Destr¢i programa de folder ---*/
    
        IF VALID-HANDLE(hFolder) THEN
            DELETE PROCEDURE hFolder.
    
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "AFTER-DESTROY-INTERFACE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-DESTROY-INTERFACE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-DESTROY-INTERFACE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("AfterdestroyInterface":U) <> "":U THEN DO:
    
        RUN AfterdestroyInterface IN THIS-PROCEDURE NO-ERROR.
    

    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    /*--- Retira estilo de janela (modal) para thinMaintenace ---*/
    IF VALID-HANDLE(hWindowParent) THEN
        ASSIGN hWindowParent:SENSITIVE = YES.
    
    /*--- Destr¢i os Servidores RPC inicializados pelos DBOs ---*/
    /***************************************************************************
**
**   btb008za.i3  -  Include para finaliza‡Æo RPC.
**
***************************************************************************/


        assign l-inf-prg = no.    



    



if l-inf-prg = yes then do:
    run pi_destroy_rpc in h-servid-rpc ("").
end.
else do i-prg-rpc = 1 to num-entries(c-lst-prg-rpc):
    run pi_destroy_rpc in h-servid-rpc (entry(i-prg-rpc,c-lst-prg-rpc)).
end.
 

    /*Alteracao para deletar da mem¢ria o WindowStyles e o btb008za.p*/
    IF VALID-HANDLE(h-servid-rpc) THEN
    DO:
       DELETE PROCEDURE h-servid-rpc.
       ASSIGN h-servid-rpc = ?. /*Garantir que a vari vel nÆo vai mais apontar para nenhum handle de outro objeto - este problema apareceu na v9.1B com Windows2000*/
    END.

    /*--- Destr¢i janela associada ao programa ---*/
    IF VALID-HANDLE(wZoom) THEN
        DELETE WIDGET wZoom.
    
    /*--- Destr¢i programa ---*/
    IF THIS-PROCEDURE:PERSISTENT THEN
        DELETE PROCEDURE THIS-PROCEDURE.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE dispatch :
/*------------------------------------------------------------------------------
  Purpose:     Manter compatibilidade com SmartObjects
  Parameters:  recebe m‚todo a ser executado
  Notes:       Somente haver  tratamento para o m‚todo initialize, quando a 
               execu‡Æo deste m‚todo for solicitada ser  executado o m‚todo
               initializeInterface
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pMethod AS CHARACTER NO-UNDO.
    
    IF pMethod = "INITIALIZE":U THEN
        RUN initializeInterface IN THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE enableImplant :
/*------------------------------------------------------------------------------
  Purpose:     Habilita botäes de implantar
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    /*--- Habilita botÆo btImplant1 ---*/
    
        DO WITH FRAME fPage1:
            ENABLE btImplant1.
        END.
    
    
    /*--- Habilita botÆo btImplant2 ---*/
    
    
    /*--- Habilita botÆo btImplant3 ---*/
    
    
    /*--- Habilita botÆo btImplant4 ---*/
    
    
    /*--- Habilita botÆo btImplant5 ---*/
    
    
    /*--- Habilita botÆo btImplant6 ---*/
    
    
    /*--- Habilita botÆo btImplant7 ---*/
    
    
    /*--- Habilita botÆo btImplant8 ---*/
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE initializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     Inicialize programa
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    /*Alterado por Anderson (tech485) para o template mostrar o nome da empresa*/

    def var c_cod_empres_usuar as char no-undo.
    def var c_nom_razao_social as char no-undo.

    /*fim alteracao Anderson*/


    /*--- Inicializa‡Æo de OCXs ---*/
    IF THIS-PROCEDURE:GET-SIGNATURE("control_load":U) <> "":U THEN DO:
        RUN control_load IN THIS-PROCEDURE NO-ERROR.
        VIEW FRAME fPage0 IN WINDOW wZoom.
    END.
    
    /*--- Executa valida‡äes de inicializa‡Æo ---*/
    ASSIGN c-programa-mg97 = CAPS("espcn001zoom":U)
           c-versao-mg97   = "1.00.00.000":U.
    /*--------------------------------------------------------------------------
    File        : UTP/UT-VFSEC.I
    Purpose     : Verifica‡Æo da Seguran‡a

    Syntax      :

    Description : Verificar a seguran‡a

    Author(s)   : Fabiano
    Created     : 31/12/1997
    Notes       :
------------------------------------------------------------------------*/
/* NÆo faz a valida‡Æo para programas do tipo V  Para */
if index(replace(program-name(1),"~\","~/"),"go/g") = 0 then do:
  run men/men901za.p (input c-programa-mg97).
  if  return-value = "2012" then do:
      run utp/ut-msgs.p (input "show",
                         input 2858,
                         input c-programa-mg97).
      if "Zoom" = "SmartDialog" or this-procedure:persistent = no then
        return "adm-error".
      else do:     
        delete procedure this-procedure.
        return.
      end.  
  end.                       

  if  return-value = "2014" then do:
      run utp/ut-msgs.p (input "show",
                         input 3045,
                         input c-programa-mg97).
      if "Zoom" = "SmartDialog" then
        return "adm-error".
      else do:
        delete procedure this-procedure.
        return.
      end.  
  end.
end.  

/* ut-vfsec.i */
 

    /*Inserida a chamada da include i-logini.i
    devido aos thintemplates nÆo gerarem log de 
    execucao de programas*/
    /*************************************************************************
**                                                                        
**  I-LOGINI.I - Inicia Log de Execu‡Æo
**
*************************************************************************/

run btb/btb918zb.p (input c-programa-mg97,
                    input-output rw-log-exec,
                    input yes).

/* i-logini */

     

    /*Alterado por Anderson (tech485) para o template mostrar o nome da empresa*/
    /*{utp/ut9000.i "{&Program}" "{&Version}"} *//*esta foi comentada por estar causando erro nos thintemplates*/
    /*rodar pi-rsocial persistent para verifica»’o empresa usuario*/
   
    /* Alterado por Valdir (tech264) para novo m‚todo de teste do handle,
       testando tamb‚m os atributos TYPE e FILE-NAME do handle */
       
    if not valid-handle(h-rsocial) or
       h-rsocial:TYPE <> "PROCEDURE":U or
       h-rsocial:FILE-NAME <> "utp/ut-rsocial.p":U then do:
        if l-achou-prog then
            run utp/ut-rsocial.p persistent set h-rsocial.
    end.
    if l-achou-prog then
        run pi-rsocial in h-rsocial (output c_cod_empres_usuar, output c_nom_razao_social).
    
    find prog_dtsul no-lock
        where prog_dtsul.cod_prog_dtsul = c-programa-mg97 no-error.
    if  avail prog_dtsul then do:
        assign c-titulo-prog-mg97    = prog_dtsul.des_prog_dtsul
               c-nom-prog-upc-mg97   = prog_dtsul.nom_prog_upc
               c-nom-prog-appc-mg97  = prog_dtsul.nom_prog_appc
               c-nom-prog-dpc-mg97   = prog_dtsul.nom_prog_dpc
               i-num-topico-hlp-mg97 = prog_dtsul.num_topico.
       
          if session:window-system <> "TTY":U then 
          
             assign i-template          = prog_dtsul.idi_template.
          
       

        
        find procedimento no-lock
            where procedimento.cod_proced = prog_dtsul.cod_proced no-error.
        if  avail procedimento then do:
            find modul_dtsul no-lock
                where modul_dtsul.cod_modul_dtsul = procedimento.cod_modul_dtsul no-error. 
            if  avail modul_dtsul then do:
                assign c-modulo-mg97         = caps(modul_dtsul.nom_modul_dtsul_menu)
                       c-cod-mod-mg97        = caps(modul_dtsul.cod_modul_dtsul)
                       c-nom-manual-hlp-mg97 = "dochlp~/":U + string(modul_dtsul.num_manual_documen, "999999":U) + ".hlp":U.
            end.
        end.
    end.                                                      
    else do:
        assign c-titulo-prog-mg97    = caps(c-programa-mg97)
               c-nom-prog-upc-mg97   = ""
               c-nom-prog-appc-mg97  = ""
               i-num-topico-hlp-mg97 = 0
               c-nom-manual-hlp-mg97 = "dochlp~/000000.hlp":U.
    end.                 

    /* Tradu‡Æo T¡tulo dos Programas */
    /* FO 1354855  - 11/08/2006 - tech1139 */
    run utp/ut-liter.p (input replace(c-titulo-prog-mg97, " ", "_") ,
                        input "*":U,
                        input "":U). 
    /* FO 1354855  - 11/08/2006 - tech1139 */

    Assign c-titulo-prog-mg97 = Return-value.

    
        
             assign wZoom:title = if l-achou-prog then
                                           c-titulo-prog-mg97
                                         + " - ":U 
                                         + c-programa-mg97 
                                         + " - ":U 
                                         + c-versao-mg97  
                                         + " - ":U 
                                         + c_cod_empres_usuar
                                         + " - ":U 
                                         + c_nom_razao_social
                                         else 
                                           c-titulo-prog-mg97
                                         + " - ":U 
                                         + c-programa-mg97 
                                         + " - ":U 
                                         + c-versao-mg97.
        
    
    
    

    /*fim alteracao Anderson(tech485)*/



    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("BeforeinitializeInterface":U) <> "":U THEN DO:
    
        RUN BeforeinitializeInterface IN THIS-PROCEDURE NO-ERROR.
    

    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN DO:
        /*--- Destr¢i programa ---*/
        RUN destroyInterface IN THIS-PROCEDURE.
        
        RETURN "NOK":U.
    END.
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "BEFORE-INITIALIZE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-INITIALIZE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-INITIALIZE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    DO WITH FRAME fPage0:
        
        /*--- Habilitar/Desabilitar botäes ---*/
        ASSIGN
            btOK:SENSITIVE     = YES
            btCancel:SENSITIVE = YES
            btHelp:SENSITIVE   = YES.
        
        /*--- Definir tamanhos e posicionar widgets ---*/
        ASSIGN 
            rtToolBar:WIDTH  = FRAME fPage0:WIDTH
            rtToolBar:HEIGHT = 1.46
            rtToolBar:COL    = 1
            rtToolBar:ROW    = FRAME fPage0:HEIGHT - 0.63
            btOK:WIDTH       = 10
            btOK:HEIGHT      = 1
            btOK:COL         = 2
            btOK:ROW         = rtToolBar:ROW + 0.23
            btCancel:WIDTH   = 10
            btCancel:HEIGHT  = 1
            btCancel:COL     = 13
            btCancel:ROW     = rtToolBar:ROW + 0.23
            btHelp:WIDTH     = 10
            btHelp:HEIGHT    = 1
            btHelp:COL       = 80
            btHelp:ROW       = rtToolBar:ROW + 0.23.
    END.
    
    /*--- Executa programa de range ---*/
    
        RUN utp/makerange.p PERSISTENT SET hRange.
    
    
    /*--- Executa programa de folder ---*/
    
        RUN utp/thinfolder.w PERSISTENT SET hFolder.
        
        RUN setProgramParent IN hFolder (INPUT THIS-PROCEDURE).
        
        RUN setOCX IN hFolder (INPUT THIS-PROCEDURE:GET-SIGNATURE("control_load":U) <> "":U) NO-ERROR.
        
        RUN initializeFolders IN hFolder (INPUT FRAME fPage0:HANDLE,
                                          INPUT 
                                                    STRING(FRAME fPage1:HANDLE)
                                                

                                                

                                                

                                                

                                                
 
                                                
 
                                                
 
                                                ,
                                          INPUT "Contratos":U,
                                          INPUT NUM-ENTRIES("Contratos":U),
                                          INPUT ?,
                                          INPUT ?,
                                          INPUT ?,
                                          
                                          /*--- Est  sendo utilizada a f¢rmula abaixo porquˆ 
                                                a barra de botäes fica na parte inferior da frame ---*/
                                          INPUT FRAME fPage0:HEIGHT - FRAME fPage1:ROW - 0.75 ).

        
            RUN setVariable in hFolder (input hRange).
        

    
    
    /*--- Habilitar browse contido no {&page1Browse} ---*/
    
        DO WITH FRAME fPage1:
            ENABLE brTable1.
        END.
    
    
    /*--- Habilitar browse contido no {&page2Browse} ---*/
    
    
    /*--- Habilitar browse contido no {&page3Browse} ---*/
    
    
    /*--- Habilitar browse contido no {&page4Browse} ---*/
    
    
    /*--- Habilitar browse contido no {&page5Browse} ---*/
    
    
    /*--- Habilitar browse contido no {&page6Browse} ---*/
    
    
    /*--- Habilitar browse contido no {&page7Browse} ---*/
    
    
    /*--- Habilitar browse contido no {&page8Browse} ---*/
    
    
    /*--- Executa programa de estilo de janelas ---*/
    IF  VALID-HANDLE(hWindowStyles) = NO OR
        hWindowStyles:TYPE <> "PROCEDURE":U OR
        (hWindowStyles:FILE-NAME <> "utp/WindowStyles.p":U AND
        hWindowStyles:FILE-NAME <> "utp/WindowStyles.r":U) THEN
        RUN utp/windowstyles.p PERSISTENT SET hWindowStyles.
    
    /*--- Seta estilo de janela para thinMaintenance ---*/
    RUN deleteMinMax IN hWindowStyles (INPUT wZoom:hWnd).

    /*Colocado esta l¢gica para caso o desenvolvedor passar o handle da bo j  
     estartado o mesmo nÆo mostrar erros de execu‡äes antigas sa inicializa‡Æo 
     do programa Feita por Anderson tech485 28/08/2202*/
    
        
             IF VALID-HANDLE(h-esbocn001) THEN
                 RUN emptyrowerrors IN h-esbocn001 NO-ERROR.
        
        
        
        
        
        
        
        
    
    /*fim altera‡Æo Anderson tech485 28/08/2202*/
    
    /*--- Inicializa DBOs ---*/
    if session:set-wait-state("general":U) then.
    RUN initializeDBOs IN THIS-PROCEDURE.
    if session:set-wait-state("") then.
    
    /*--- Verifica se a inicializa‡Æo dos DBOs foi feita com sucesso ---*/
    IF NO
        
            OR NOT VALID-HANDLE(h-esbocn001) 
        
            
        
        
        
        
        
        
        
        
        
        
        
        
        
        
    THEN DO:
        /*--- Exibir mensagem de finaliza‡Æo do programa ---*/
        RUN utp/ut-msgs.p (INPUT "SHOW":U, 
                           INPUT 18881, 
                           INPUT CAPS("espcn001zoom":U) + "~~":U + CAPS("1.00.00.000":U)).
        
        /*--- Destr¢i programa ---*/
        RUN destroyInterface IN THIS-PROCEDURE.
        
        RETURN "NOK":U.
    END.
    
    ELSE DO:
        /*Foi inclu¡da esta l¢gica para verifica‡Æo de erros relacionados com 
         a permissÆo de execu‡Æo do programa de dbo Altera‡Æo em 17/06/2002 por Anderson(tech540)*/
         
              RUN getrowerrors IN h-esbocn001 (OUTPUT TABLE rowerrors).
                IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorNumber <> 3 AND
                                                  RowErrors.ErrorNumber <> 8 AND
                                                  RowErrors.ErrorNumber <> 10 AND
                                                  RowErrors.ErrorSubType = "ERROR":U) THEN DO:
                  
/* Procedure Description
"Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do utilit rio ut-show-msgs.w.

Este include faz somente a instƒncia do utilit rio."
*/


/*--------------------------------------------------------------------------
    File       : method/ShowMessage.i1
    Purpose    : Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do
                 utilit rio ut-show-msgs.w

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : Este include faz somente a instƒncia do utilit rio
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 1
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Inicializa tela de mensagens de erros ---*/
IF NOT VALID-HANDLE(hShowMsg) or
   hShowMsg:TYPE <> "PROCEDURE":U or
   hShowMsg:FILE-NAME <> "utp/ShowMessage.w":U THEN
        RUN utp/showmessage.w PERSISTENT SET hShowMsg.

/* _UIB-CODE-BLOCK-END */



 
                  
/* Procedure Description
"Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do utilit rio ut-show-msgs.w.

Este include faz a transferˆncia dos erros da temp-table RowErrors para o utilit rio."
*/


/*--------------------------------------------------------------------------
    File       : method/ShowMessage.i2
    Purpose    : Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do
                 utilit rio ut-show-msgs.w

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : Este include faz a transferˆncia dos erros da temp-table 
                 RowErrors para o utilit rio
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Transferˆncia dos erros da temp-table RowErrors para o utilit rio ---*/
IF VALID-HANDLE(hShowMsg) and
   hShowMsg:TYPE = "PROCEDURE":U and
   hShowMsg:FILE-NAME = "utp/ShowMessage.w":U THEN DO:
    
        RUN setModal IN hShowMsg (INPUT NO) NO-ERROR.
    

    RUN showMessages IN hShowMsg (INPUT TABLE RowErrors).
END.

/* _UIB-CODE-BLOCK-END */



 
                   /****
                    TECH14187 - 1538059
                    Problema da ShowMessage Modal
                    ****/
                   
                        WAIT-FOR CLOSE OF hShowMsg.
                   
                  
/* Procedure Description
"Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do utilit rio ut-show-msgs.w.

Este include faz somente a elimina‡Æo da instƒncia do utilit rio."
*/


/*--------------------------------------------------------------------------
    File       : method/ShowMessage.i3
    Purpose    : Exibir mensagens de ERROR/INFORMATION/WARNING atrav‚s do
                 utilit rio ut-show-msgs.w

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : Este include faz somente a elimina‡Æo da instƒncia do 
                 utilit rio
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Destr¢i tela de mensagens de erros ---*/
IF VALID-HANDLE(hShowMsg) and
  hShowMsg:TYPE = "PROCEDURE":U and
  hShowMsg:FILE-NAME = "utp/ShowMessage.w":U THEN DO:
       RUN destroyInterface IN hShowMsg.
end.
/* _UIB-CODE-BLOCK-END */



 
                  RUN destroyInterface IN THIS-PROCEDURE.
                  RETURN "NOK":U.
              END.
         
         
         
         
         
          
         
          
         
            
         
          
         
          
         
    END.
    

    /*--- Atualiza browsers ---*/
    RUN openQueries IN THIS-PROCEDURE.
    
    
        
        
        IF VALID-HANDLE(hRange) THEN 
            run setahandlefolder in hRange (input hFolder).
        
        
            RUN initializeMultipleRange IN hRange (INPUT THIS-PROCEDURE:HANDLE,
                                                   INPUT STRING(FRAME fPage1:HANDLE),
                                                   INPUT "mgesp.ext-medicao-contrat.nr-contrato":U,
                                                   INPUT "NO",
                                                   OUTPUT hFirstField).
        
        
        
        
        
        
        
        
    
    
    
        RUN setFolder IN hFolder (INPUT 1).
    
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "AFTER-INITIALIZE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-INITIALIZE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-INITIALIZE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "?":U,
                                    INPUT ?).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("AfterinitializeInterface":U) <> "":U THEN DO:
    
        RUN AfterinitializeInterface IN THIS-PROCEDURE NO-ERROR.
    

    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN DO:
        /*--- Destr¢i programa ---*/
        RUN destroyInterface IN THIS-PROCEDURE.
        
        RETURN "NOK":U.
    END.

    /*** Alterado por Farley - em 23/07/2003 ***/
    
    
    /*--- Visualiza janela ---*/
    VIEW wZoom.

    /*Alteracao 24/01/2006 - tech14207 - Alterado para desabilitar o programa pai*/
    /*O c¢digo foi alterado para permitir que o mesmo programa fosse executado mais de uma vez corretamente*/
    
        DEFINE VAR h-source AS HANDLE     NO-UNDO.
        DEFINE VARIABLE h-this AS HANDLE     NO-UNDO.

        ASSIGN h-source = SOURCE-PROCEDURE.
        ASSIGN h-this = THIS-PROCEDURE.
        
        IF (THIS-PROCEDURE <> SOURCE-PROCEDURE) THEN DO:
            ASSIGN h-source = SOURCE-PROCEDURE.
            /*26/09/2006 - tech30713 - FO: 1309021*/
            IF VALID-HANDLE(h-source) AND  VALID-HANDLE(h-source:CURRENT-WINDOW) THEN DO:
                ASSIGN hWindowParent           = h-source:CURRENT-WINDOW
                       hWindowParent:SENSITIVE = NO.
            END.
        END.
    
    /*Fim da altera‡Æo 24/01/2006 - tech14207*/

    
    IF  VALID-HANDLE(hFirstField) THEN
        APPLY "ENTRY":U TO hFirstField.

    /*Alteracao 24/01/2006 - tech14207 - Alterado para desabilitar o programa pai*/
    /*O c¢digo foi alterado para permitir que o mesmo programa fosse executado mais de uma vez corretamente*/    
    /*--- Seta estilo de janela modal para thinMaintenance ---*/
/*     ASSIGN hWindowParent           = SESSION:FIRST-CHILD        */
/*            hWindowParent           = hWindowParent:NEXT-SIBLING */
/*            hWindowParent:SENSITIVE = NO.                        */
    /*Fim da altera‡Æo 24/01/2006 - tech14207*/
    

    /*Altera‡Æo 09/03/2007 - tech1007/tech30713 - FO 1465451 - Alterado para setar o foco corretamente quando for Progress 9.1B*/
    
        
    
        APPLY "ENTRY":U TO FRAME fPage0.
    .    

    APPLY "ENTRY":U TO wZoom.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE repositionRecordInPage :
/*------------------------------------------------------------------------------
  Purpose:     Reposiciona DBO table atrav‚s de um rowid
  Parameters:  recebe rowid
               recebe n£mero da p gina
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pRowid      AS ROWID   NO-UNDO.
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    /*--- Seta vari vel iRepositionPageNumber com o n£mero da p gina na qual
          o browse filho ser  reposicionado ---*/
    /*--- Seta vari vel rReposition com o rowid a ser reposicionado no 
          browse filho ---*/
    ASSIGN iRepositionPageNumber = pPageNumber
           rRepositionTable      = pRowid.
    
    /*--- Atualiza browse mas somente da p gina iRepositionPageNumber ---*/
    RUN openQueries IN THIS-PROCEDURE.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE setFieldNamesHandles :
/*------------------------------------------------------------------------------
  Purpose:     Seta nome dos campos a serem retornados
  Parameters:  recebe nome dos campos
               recebe handle dos campos
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pFieldNames   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pFieldHandles AS CHARACTER NO-UNDO.
    
    /*--- Atualiza vari vel cFieldNames com o nome dos campos a serem 
          retornados ---*/
    ASSIGN cFieldNames   = pFieldNames
           cFieldHandles = pFieldHandles.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE changePage :
/*------------------------------------------------------------------------------
  Purpose:     M‚todo executado pelo programa de Folder, quando h  troca de 
               p gina
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
def var pageN as integer no-undo.
    
    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("BeforechangePage":U) <> "":U THEN DO:
    
        RUN BeforechangePage IN THIS-PROCEDURE NO-ERROR.
    

    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.

    RUN GetCurrentFolder IN hFolder (OUTPUT PageN).
    
    Case pageN:
    
    when 1 then do:
        IF AVAILABLE tt-ext-medicao-contrat THEN 
            ASSIGN epc-rowid1 = tt-ext-medicao-contrat.r-Rowid.
        ELSE 
            ASSIGN epc-rowid1 = ?.
    
        /*--- Executa programas de customiza‡Æo (before/after) ---*/
        
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "BEFORE-CHANGE-PAGE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "tt-ext-medicao-contrat":U,
                                    INPUT epc-rowid1).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-CHANGE-PAGE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "tt-ext-medicao-contrat":U,
                                    INPUT epc-rowid1).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-CHANGE-PAGE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "tt-ext-medicao-contrat":U,
                                    INPUT epc-rowid1).
END.

/* _UIB-CODE-BLOCK-END */



 
        
        /*--- Executa programas de customiza‡Æo (before/after) ---*/
        
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "AFTER-CHANGE-PAGE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "tt-ext-medicao-contrat":U,
                                    INPUT epc-rowid1).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-CHANGE-PAGE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "tt-ext-medicao-contrat":U,
                                    INPUT epc-rowid1).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-CHANGE-PAGE":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "tt-ext-medicao-contrat":U,
                                    INPUT epc-rowid1).
END.

/* _UIB-CODE-BLOCK-END */



 
    end.
    
    
    
    
    
    
    
    
    end case.

    /*--- Executa m‚todos sobrepostos (before/after) ---*/
    
/* Procedure Description
"Executar métodos sobrepostos (Before/After)."
*/


/*--------------------------------------------------------------------------
    File       : method/svc/override/override.i
    Purpose    : Executar métodos sobrepostos (before/after)

    Parameters : 
        &Position   : indica a posição de execução, os valores aceitos são 
                      "Before" e "After"
        &Procedure  : nome genérico da procedure, será concatenado com o 
                      parâmetro &Position
        &Parameters : parâmetros a serem transferidos

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Executa métodos sobrepostos (before/after) ---*/
IF THIS-PROCEDURE:GET-SIGNATURE("AfterchangePage":U) <> "":U THEN DO:
    
        RUN AfterchangePage IN THIS-PROCEDURE NO-ERROR.
    

    ASSIGN lOverrideExecuted = YES.
END.
ELSE
    ASSIGN lOverrideExecuted = NO.

/* _UIB-CODE-BLOCK-END */



 
    IF lOverrideExecuted AND RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */




/*Alteracao 27/07/2005 - tech1007 - Alterado para fazer traducao de itens de tela*/



PROCEDURE translate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*Os botäes desta tela nÆo tem pr‚-processadores, entÆo eles podem ser exclu¡dos
     por isso, para traduzir o label deles estamos buscando os que estiverem na tela
     e traduzindo o label*/
  DEFINE VARIABLE h_Frame AS HANDLE     NO-UNDO.

  ASSIGN h_Frame = FRAME fPage0:FIRST-CHILD. /* pegando o Field-Group */
  ASSIGN h_Frame = h_Frame:FIRST-CHILD.       /* pegando o 1o. Campo */
  DO WHILE h_Frame <> ? :
       if h_Frame:type <> "field-group" then do:  
          IF h_Frame:TYPE = "button" THEN
          DO :
             RUN utp/ut-liter.p (INPUT h_Frame:LABEL, "*", "C").
             ASSIGN h_Frame:LABEL = RETURN-VALUE
                    h_Frame:TOOLTIP = RETURN-VALUE.
          END.
          ASSIGN h_Frame = h_Frame:NEXT-SIBLING.
       end. 
       else do:
         assign h_frame = h_frame:first-child.
       end.    
  END.

  RUN utp/ut-liter.p (INPUT "Implantar", "*", "C").
  
     ASSIGN btImplant1:LABEL IN FRAME fPage1   = RETURN-VALUE
            btImplant1:TOOLTIP IN FRAME fPage1 = RETURN-VALUE.
  
  
  
  
  
  
  
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE pi_aplica_facelift :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    

/**** Alteracao efetuada por tech14187/tech1007/tech38629 para o projeto Facelift ****/
    run pi_aplica_facelift_thin in h-facelift ( input frame fPage0:handle ).
    
    run pi_aplica_facelift_thin in h-facelift ( input frame fPage1:handle ).
    
    
    
    
    
    
    
    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



/*Fim Alteracao 27/07/2005*/
 

/* _UIB-CODE-BLOCK-END */





/* ***********  Runtime Attributes and AppBuilder Settings  *********** */


/* SETTINGS FOR WINDOW wZoom
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME fPage1:FRAME = FRAME fpage0:HANDLE.

/* SETTINGS FOR FRAME fpage0
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME fPage1
                                                                        */
/* BROWSE-TAB brTable1 1 fPage1 */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wZoom)
THEN wZoom:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */



/* Setting information for Queries and Browse Widgets fields            */


/* Query rebuild information for BROWSE brTable1
     _TblList          = "Temp-Tables.tt-ext-medicao-contrat"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.tt-ext-medicao-contrat.nr-contrato
"nr-contrato" ? ? "integer" ? ? ? ? ? ? no ? no no "14.43" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.tt-ext-medicao-contrat.numero-ordem
"numero-ordem" ? ? "integer" ? ? ? ? ? ? no ? no no "12.43" yes no no "U" "" ""
     _FldNameList[3]   = Temp-Tables.tt-ext-medicao-contrat.num-seq-item
     _FldNameList[4]   = Temp-Tables.tt-ext-medicao-contrat.num-seq-event
     _FldNameList[5]   = Temp-Tables.tt-ext-medicao-contrat.num-seq-medicao
     _FldNameList[6]   > Temp-Tables.tt-ext-medicao-contrat.nr-trans
"nr-trans" ? ? "integer" ? ? ? ? ? ? no ? no no "18.86" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE brTable1 */



/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */



/* Query rebuild information for FRAME fPage1
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fPage1 */


 



/* ************************  Control Triggers  ************************ */


ON END-ERROR OF wZoom
OR ENDKEY OF wZoom ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */




ON WINDOW-CLOSE OF wZoom
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btCancel IN FRAME fpage0 /* Cancelar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btHelp IN FRAME fpage0 /* Ajuda */
DO:
    /*************************************************************************
**
** AJUDA.I - Include padrÆo para chamada do Help
**
**************************************************************************/


    RUN men/men900za.p (INPUT ?, INPUT THIS-PROCEDURE:HANDLE).


RETURN NO-APPLY.

/* include/ajuda.i */
 
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btImplant1 IN FRAME fPage1 /* Implantar */
DO:
  /*  {zoom/implant.i &ProgramImplant="<ProgramName>"
                    &PageNumber="1"}*/
END.

/* _UIB-CODE-BLOCK-END */




ON CHOOSE OF btOK IN FRAME fpage0 /* OK */
DO:
    RUN returnValues IN THIS-PROCEDURE.
    
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */







/* ***************************  Main Block  *************************** */

/*:T--- L¢gica para inicializa‡Æo do programam ---*/

/* Procedure Description
"Method Library que cont‚m a l¢gica da Main Block."
*/


/*--------------------------------------------------------------------------
    Library    : zoom/MainBlock.i
    Purpose    : Method Library que cont‚m a l¢gica da Main Block 

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 4.5
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */


 





/* ***************************  Main Block  *************************** */

/*--- Seta cursor do mouse para espera ---*/
SESSION:SET-WAIT-STATE("GENERAL":U).

/*--- Evento de CLOSE padrÆo para THIS-PROCEDURE ---*/
ON CLOSE OF THIS-PROCEDURE DO:
   RUN destroyInterface IN THIS-PROCEDURE.
END.

/*--- Evento de CTRL-TAB padrÆo para THIS-PROCEDURE ---*/

    ON CTRL-TAB ANYWHERE
        RUN nextFolder IN hFolder.


/*--- Evento de SHIFT-CTRL-TAB padrÆo para THIS-PROCEDURE ---*/

    ON SHIFT-CTRL-TAB ANYWHERE
        RUN prevFolder IN hFolder.



/*--- Evento de CURSOR-DOWN, PAGE-DOWN, OFF-END e END padrÆo para o browse da p gina 1 ---*/

    ON RETURN  OF BROWSE brTable1  DO:
         RUN returnValues IN THIS-PROCEDURE.
         APPLY "CLOSE":U TO THIS-PROCEDURE.
    END.

    
    ON CURSOR-DOWN OF brTable1 IN FRAME fPage1 DO:
        
/* Procedure Description
"L¢gica para a trigger CURSOR-DOWN de browse."
*/


/*--------------------------------------------------------------------------
    File       : zoom/CursorDown.i
    Purpose    : L¢gica para a trigger CURSOR-DOWN de browse

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Executa procedure applyCursorDown para disparar o evento de CURSOR-DOWN ---*/
RUN applyCursorDown IN THIS-PROCEDURE (INPUT 1).

/* _UIB-CODE-BLOCK-END */



 
    END.
    
    
    
    ON END OF brTable1 IN FRAME fPage1 DO:
        
/* Procedure Description
"L¢gica para a trigger END de browse."
*/


/*--------------------------------------------------------------------------
    File       : zoom/End.i
    Purpose    : L¢gica para a trigger END de browse

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Executa procedure applyEnd para disparar o evento de END ---*/
RUN applyEnd IN THIS-PROCEDURE (INPUT 1).

/* _UIB-CODE-BLOCK-END */



 
    END.
    
    
    
    ON OFF-END OF brTable1 IN FRAME fPage1 DO:
        
/* Procedure Description
"L¢gica para a trigger OFF-END de browse."
*/


/*--------------------------------------------------------------------------
    File       : zoom/OffEnd.i
    Purpose    : L¢gica para a trigger OFF-END de browse

    Parameters : 
        &PageNumber : n£mero da p gina onde est  o browse, a ser utilizado
                      para definir o nome de alguns widgets tais como
                      brTable{&PageNumber}

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Executa procedure applyOffEnd para disparar o evento de OFF-END ---*/
RUN applyOffEnd IN THIS-PROCEDURE (INPUT 1).

/* _UIB-CODE-BLOCK-END */



 
    END.
    
    
    
    ON PAGE-DOWN OF brTable1 IN FRAME fPage1 DO:
        
/* Procedure Description
"L¢gica para a trigger PAGE-DOWN de browse."
*/


/*--------------------------------------------------------------------------
    File       : zoom/PageDown.i
    Purpose    : L¢gica para a trigger PAGE-DOWN de browse

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Executa procedure applyPageDown para disparar o evento de PAGE-DOWN ---*/
RUN applyPageDown IN THIS-PROCEDURE (INPUT 1).

/* _UIB-CODE-BLOCK-END */



 
    END.
    
    
    
    ON VALUE-CHANGED OF brTable1 IN FRAME fPage1 DO:
        
/* Procedure Description
"Evento padrÆo de VALUE-CHANGED para browses."
*/


/*--------------------------------------------------------------------------
    File       : zoom/ValueChanged.i
    Purpose    : Evento padrÆo de VALUE-CHANGED para browses  

    Parameters : 
        &PageNumber : n£mero da p gina onde est  o browse, a ser utilizado
                      para definir o nome de alguns widgets tais como
                      brSon{&PageNumber} btAddSon{&PageNumber}

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Executa procedure applyValueChanged para disparar o evento de VALUE-CHANGED ---*/
RUN applyValueChanged IN THIS-PROCEDURE (INPUT 1).

/* _UIB-CODE-BLOCK-END */



 
    END.
    
    
    
    ON MOUSE-SELECT-DBLCLICK OF brTable1 IN FRAME fPage1 DO:
        
/* Procedure Description
"L¢gica para a trigger DBL-CLICK de browse."
*/


/*--------------------------------------------------------------------------
    File       : zoom/DblClick.i
    Purpose    : L¢gica para a trigger DBL-CLICK de browse

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Executa procedure applyDblClick para disparar o evento de DBL-CLICK ---*/
RUN applyDblClick IN THIS-PROCEDURE (INPUT 1).

/* _UIB-CODE-BLOCK-END */



 
    END.
    
    
    
    ON CURSOR-UP OF brTable1 IN FRAME fPage1 DO:
        
/* Procedure Description
"L¢gica para a trigger CURSOR-DOWN de browse."
*/


/*--------------------------------------------------------------------------
    File       : zoom/CursorUp.i
    Purpose    : L¢gica para a trigger CURSOR-UP de browse

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Executa procedure applyCursorUp para disparar o evento de CURSOR-UP ---*/
RUN applyCursorUp IN THIS-PROCEDURE (INPUT 1).

/* _UIB-CODE-BLOCK-END */



 
    END.
    
    
    
    ON HOME OF brTable1 IN FRAME fPage1 DO:
        
/* Procedure Description
"L¢gica para a trigger END de browse."
*/


/*--------------------------------------------------------------------------
    File       : zoom/Home.i
    Purpose    : L¢gica para a trigger HOME de browse

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Executa procedure applyHome para disparar o evento de HOME ---*/
RUN applyHome IN THIS-PROCEDURE (INPUT 1).

/* _UIB-CODE-BLOCK-END */



 
    END.
    
    
    
    ON OFF-HOME OF brTable1 IN FRAME fPage1 DO:
        
/* Procedure Description
"L¢gica para a trigger OFF-END de browse."
*/


/*--------------------------------------------------------------------------
    File       : zoom/OffHome.i
    Purpose    : L¢gica para a trigger OFF-HOME de browse

    Parameters : 
        &PageNumber : n£mero da p gina onde est  o browse, a ser utilizado
                      para definir o nome de alguns widgets tais como
                      brTable{&PageNumber}

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Executa procedure applyPageDown para disparar o evento de OFF-HOME ---*/
RUN applyOffHome IN THIS-PROCEDURE (INPUT 1).

/* _UIB-CODE-BLOCK-END */



 
    END.
    
    
    
    ON PAGE-UP OF brTable1 IN FRAME fPage1 DO:
        
/* Procedure Description
"L¢gica para a trigger PAGE-DOWN de browse."
*/


/*--------------------------------------------------------------------------
    File       : zoom/PageUp.i
    Purpose    : L¢gica para a trigger PAGE-UP de browse

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

/*--- Executa procedure applyPageUp para disparar o evento de PAGE-UP ---*/
RUN applyPageUp IN THIS-PROCEDURE (INPUT 1).

/* _UIB-CODE-BLOCK-END */



 
    END.
    


/*--- Evento de CURSOR-DOWN, PAGE-DOWN, OFF-END e END padrÆo para o browse da p gina 2 ---*/


/*--- Evento de CURSOR-DOWN, PAGE-DOWN, OFF-END e END padrÆo para o browse da p gina 3 ---*/


/*--- Evento de CURSOR-DOWN, PAGE-DOWN, OFF-END e END padrÆo para o browse da p gina 4 ---*/


/*--- Evento de CURSOR-DOWN, PAGE-DOWN, OFF-END e END padrÆo para o browse da p gina 5 ---*/


/*--- Evento de CURSOR-DOWN, PAGE-DOWN, OFF-END e END padrÆo para o browse da p gina 6 ---*/


/*--- Evento de CURSOR-DOWN, PAGE-DOWN, OFF-END e END padrÆo para o browse da p gina 7 ---*/


/*--- Evento de CURSOR-DOWN, PAGE-DOWN, OFF-END e END padrÆo para o browse da p gina 8 ---*/


/*--- Seta CURRENT-WINDOW como sendo a window atual ---*/
ASSIGN CURRENT-WINDOW                = wZoom
       THIS-PROCEDURE:CURRENT-WINDOW = wZoom.

/*--- PadrÆo para janelas GUI ---*/
PAUSE 0 BEFORE-HIDE.


    IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
        /*--- Inicializa programa ---*/
        RUN initializeInterface IN THIS-PROCEDURE.
        IF RETURN-VALUE = "NOK":U THEN
            RETURN "NOK":U.
    END.


/*Alteracao 27/07/2005 - tech1007 - procedure para traduzir tooltips de botoes e tambem de menu*/
RUN translate IN THIS-PROCEDURE.
/*Fim alteracao 27/07/2005*/

/*--- Block principal do programa ---*/
DO ON ERROR   UNDO, LEAVE
   ON END-KEY UNDO, LEAVE:
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */



 

/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */


PROCEDURE initializeDBOs :
/*:T------------------------------------------------------------------------------
  Purpose:     Inicializa DBOs
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    /*:T--- Verifica se o DBO j  est  inicializado ---*/
    IF NOT VALID-HANDLE(h-esbocn001) OR
       h-esbocn001:TYPE <> "PROCEDURE":U OR
       h-esbocn001:FILE-NAME <> "esbo/esbo003.p":U THEN DO:
       
        /***************************************************************************
**
**   btb008za.i1  -  Include para inicializa‡Æo RPC.
**
***************************************************************************/


        assign c-lst-prg-rpc = c-lst-prg-rpc + min(c-lst-prg-rpc, ",") + "esbo/esbo003.p" /* + ","*/ .



    


    

if  not valid-handle(h-servid-rpc)
or h-servid-rpc:type <> "procedure":U
or h-servid-rpc:file-name <> "btb/btapi008.p":U then do:
    run btb/btapi008.p persistent set h-servid-rpc.
end /* if */.

run pi_connect in h-servid-rpc ("esbo/esbo003.p", yes, "").

if rpc_tip_exec("esbo/esbo003.p") = yes then do:

    for each tt-control-prog:
        delete tt-control-prog.
    end.

    create tt-control-prog.
    assign tt-control-prog.cod-versao-integracao = 1
           tt-control-prog.wgh-servid-rpc        = rpc_server("esbo/esbo003.p").

    run btb/btb923za.p(input-output table tt-control-prog).
end.
 
        /***************************************************************************
**
**   btb008za.i2  -  Include para execu‡Æo RPC.
**
***************************************************************************/


    


    


    

rpc_exec_set("esbo/esbo003.p",yes).
rpc_block:
repeat while rpc_exec("esbo/esbo003.p") on stop undo rpc_block, retry rpc_block:
    if rpc_program("esbo/esbo003.p") = ? then
       leave rpc_block.

    if  retry
    then do:
        run pi_status_error in h-servid-rpc.
        next rpc_block.
    end /* if */.
    if  rpc_tip_exec("esbo/esbo003.p")
    
    
    
    
    
    
    
    
    then do:
    
        run pi_check_server in h-servid-rpc ("esbo/esbo003.p").
        if  return-value = 'yes'
        then do:
            if rpc_program("esbo/esbo003.p") <> ? then do:
                if  "''" = "''"
                then do:
                    
                        run value(rpc_program("esbo/esbo003.p")) persistent set h-esbocn001 on rpc_server("esbo/esbo003.p") transaction distinct no-error.
                    
                end /* if */.
                else do:
                    
                        run value(rpc_program("esbo/esbo003.p")) persistent set h-esbocn001 on rpc_server("esbo/esbo003.p") transaction distinct ('') no-error.
                    
                end /* else */.
            end.     
        end /* if */.
        else do:
            next rpc_block.
        end /* else */.
    end /* if */.
    else do:
        if rpc_program("esbo/esbo003.p") <> ? then do:
            if  "''" = "''"
            then do:
                
                    run value(rpc_program("esbo/esbo003.p")) persistent set h-esbocn001 no-error.
                
            end /* if */.
            else do:
                
                    run value(rpc_program("esbo/esbo003.p")) persistent set h-esbocn001 ('') no-error.
                
            end /* else */.
        end.    
    end /* else */.
    
    run pi_status_error in h-servid-rpc.
end /* repeat rpc_block */.
  
    END.
    
    RUN setConstraintContrato IN h-esbocn001 (INPUT 0,
                                                INPUT 999999999) NO-ERROR.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE openQueries :
/*:T------------------------------------------------------------------------------
  Purpose:     Atualiza browsers
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    
/* Procedure Description
"Atualiza browsers em janelas de Zoom ."
*/


/*--------------------------------------------------------------------------
    File       : zoom/OpenQueries.i
    Purpose    : Atualiza browsers em janelas de Zoom 

    Parameters : 
        &Query        : nome da query, a ser utilizado para passagem de 
                        parƒmetro para o m‚todo openQueryStatic
        &PageNumber   : n£mero da p gina onde est  o browse, a ser utilizado
                        para definir o nome de alguns widgets tais como
                        brTable{&PageNumber}

        Somente para BO 1.1:
        
        &QueryNumber  : n£mero da query, a ser utilizado para passagem de 
                        parƒmetro para o m‚todo openQuery

    Authors    : John Cleber Jaraceski, Sergio Weber

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */

 






/* ***************************  Main Block  *************************** */

IF ((iConstraintPageNumber = 0 OR iConstraintPageNumber = 1) AND iRepositionPageNumber = 0) OR
   ((iRepositionPageNumber = 0 OR iRepositionPageNumber = 1) AND iConstraintPageNumber = 0) THEN DO:

    IF AVAILABLE tt-ext-medicao-contrat THEN 
        ASSIGN epc-rowid1 = tt-ext-medicao-contrat.r-Rowid.
    ELSE 
        ASSIGN epc-rowid1 = ?.

    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "BEFORE-OPEN-QUERY":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "ext-medicao-contrat":U,
                                    INPUT epc-rowid1).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "BEFORE-OPEN-QUERY":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "ext-medicao-contrat":U,
                                    INPUT epc-rowid1).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "BEFORE-OPEN-QUERY":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "ext-medicao-contrat":U,
                                    INPUT epc-rowid1).
END.

/* _UIB-CODE-BLOCK-END */



 
    
    /*--- Manter compatibilidade dos thinTemplates com BO 1.1 e DBO 2.0 ---*/
    
        /*--- Abre query do DBO ---*/
        RUN openQueryStatic IN h-esbocn001 (INPUT "Contrato":U).
        
        /*--- Retorna faixa de registro do DBO ---*/
        RUN getBatchRecords IN h-esbocn001 (INPUT IF iRepositionPageNumber = 1
                                                                  THEN rRepositionTable
                                                                  ELSE ?,
                                                          INPUT NO,
                                                          INPUT 25
                                                                ,
                                                          OUTPUT iRowsReturned,
                                                          OUTPUT TABLE tt-ext-medicao-contrat).
    
    
    /*--- Abre a query associada ao browse da p gina {&PageNumber} ---*/
    OPEN QUERY brTable1 FOR EACH tt-ext-medicao-contrat NO-LOCK.
    APPLY "VALUE-CHANGED":U TO BROWSE brTable1.
    
    /*--- Seta vari vel iConstraintPageNumber com o valor 0 (zero), desta forma
          ‚ impedido que outra p gina seja reposicionada ---*/
    ASSIGN iConstraintPageNumber = 0.
    
    /*--- Seta vari vel iRepositionPageNumber com o valor 0 (zero), desta forma
          ‚ impedido que outra p gina seja reposicionada ---*/
    ASSIGN iRepositionPageNumber = 0.

    IF AVAILABLE tt-ext-medicao-contrat THEN 
        ASSIGN epc-rowid1 = tt-ext-medicao-contrat.r-Rowid.
    ELSE 
        ASSIGN epc-rowid1 = ?.
    
    /*--- Executa programas de customiza‡Æo (before/after) ---*/
    
/* Procedure Description
"Include que cont‚m a implementa‡Æo para Servi‡o de RPC."
*/


/*--------------------------------------------------------------------------
    File       : method/Custom.i
    Purpose    : Include que cont‚m a implementa‡Æo para EPCs em ThinTemplate

    Parameters : 
        &Event        : nome do evento
        &Object       : tipo do objeto
        &ObjectHandle : handle do objeto
        &FrameHandle  : widget-handle da frame principal
        &Table        : nome da tabela principal
        &RowidTable   : rowid da tabela principal
        &StopOnError  : parar caso exista erro em algum dos programas 
                        de customiza‡Æo

    Author     :

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ****************************  Definitions  *************************** */

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 2.01
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */








/* ***************************  Main Block  *************************** */

/*--- Seta vari vel lCustomExecuted a fim de indicar se houve execu‡Æo de
      algum programa de customiza‡Æo ---*/
ASSIGN lCustomExecuted = NO.

IF c-nom-prog-dpc-mg97 <> "":U THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-dpc-mg97) (INPUT "AFTER-OPEN-QUERY":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "ext-medicao-contrat":U,
                                    INPUT epc-rowid1).
END.

IF (c-nom-prog-appc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-appc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-appc-mg97) (INPUT "AFTER-OPEN-QUERY":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "ext-medicao-contrat":U,
                                    INPUT epc-rowid1).
END.

IF (c-nom-prog-upc-mg97 <> "":U AND RETURN-VALUE <> "NOK":U) OR
   (c-nom-prog-upc-mg97 <> "":U AND "":U = "NO":U) THEN DO:
    ASSIGN lCustomExecuted = YES.

    RUN VALUE(c-nom-prog-upc-mg97) (INPUT "AFTER-OPEN-QUERY":U,
                                    INPUT "CONTAINER":U,
                                    INPUT THIS-PROCEDURE:HANDLE,
                                    INPUT FRAME fPage0:HANDLE,
                                    INPUT "ext-medicao-contrat":U,
                                    INPUT epc-rowid1).
END.

/* _UIB-CODE-BLOCK-END */



 
END.

/* _UIB-CODE-BLOCK-END */



 
    

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE returnFieldsPage1 :
/*:T------------------------------------------------------------------------------
  Purpose:     Retorna valores dos campos da p gina 1
  Parameters:  recebe nome do campo
               retorna valor do campo
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE  INPUT PARAMETER pcField      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pcFieldValue AS CHARACTER NO-UNDO.
    
    IF AVAILABLE tt-ext-medicao-contrat THEN DO:
        CASE pcField:
            WHEN "transacao":U THEN
                ASSIGN pcFieldValue = STRING(tt-ext-medicao-contrat.nr-trans).
        END CASE.
    END.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



PROCEDURE setConstraints :
/*:T------------------------------------------------------------------------------
  Purpose:     Seta constraints e atualiza o browse, conforme n£mero da p gina
               passado como parƒmetro
  Parameters:  recebe n£mero da p gina
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE INPUT PARAMETER pPageNumber AS INTEGER NO-UNDO.
    
    /*:T--- Seta constraints conforme n£mero da p gina ---*/
    CASE pPageNumber:
        WHEN 1 THEN
            /*:T--- Seta Constraints para o DBO Table1 ---*/
            RUN setConstraintContrato IN h-esbocn001 (INPUT fnIniRangeIntPage(INPUT 1, INPUT 1),
                                                        INPUT fnEndRangeIntPage(INPUT 1, INPUT 1)).
    END CASE.
    
    /*:T--- Seta vari vel iConstraintPageNumber com o n£mero da p gina atual 
          Esta vari vel ‚ utilizada no m‚todo openQueries ---*/
    ASSIGN iConstraintPageNumber = pPageNumber.
    
    /*:T--- Atualiza browse ---*/
    RUN openQueries IN THIS-PROCEDURE.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */



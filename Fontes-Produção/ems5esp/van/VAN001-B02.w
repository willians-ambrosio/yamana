&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems5             PROGRESS
          ems5_esp         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i VAN001-B02 2.00.00.019}  /*** 010019 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i VAN001-B02 MUT}
&ENDIF

/*------------------------------------------------------------------------
 
  File:  
 
  Description: from BROWSER.W - Basic SmartBrowser Object Template
 
  Input Parameters:
      <none>
 
  Output Parameters:
      <none>
 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/
 
/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
 
CREATE WIDGET-POOL.
 
/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/browserd.w
 
/* Definicao TTs e Variaveis - Validacao Decimais - Chile */

/* miniflexibiliza‡Æo Rac */
  {cdp/cdcfgfin.i}
   
/* Parameters Definitions ---                                           */
 
/* Local Variable Definitions ---                                       */
def var de-perc       as de format ">>9.99" no-undo.
def var de-vl-emissao like titulo.vl-original no-undo.
def var v-row-table   as rowid.
 
DEFINE VARIABLE c-dir AS CHARACTER   NO-UNDO.
DEF BUFFER bf_es_param_van_dir FOR es_param_van_dir.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-4

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES es_param_webserver
&Scoped-define FIRST-EXTERNAL-TABLE es_param_webserver


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es_param_webserver.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES es_param_van_dir

/* Definitions for BROWSE BROWSE-4                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-4 es_param_van_dir.cod_intercambio 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-4 
&Scoped-define QUERY-STRING-BROWSE-4 FOR EACH es_param_van_dir OF es_param_webserver NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-4 OPEN QUERY BROWSE-4 FOR EACH es_param_van_dir OF es_param_webserver NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-4 es_param_van_dir
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-4 es_param_van_dir


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-4}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 BROWSE-4 bt-mod bt-add bt-conf bt-can 
&Scoped-Define DISPLAYED-OBJECTS c_rem c_ret c_extrato c_ok c_erro ~
i_cod_inter 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 BUTTON-1 c_rem BUTTON-4 c_ret BUTTON-5 c_extrato c_ok ~
BUTTON-6 BUTTON-7 c_erro i_cod_inter bt-mod 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
cod-rep|y|y|movadm.rep-tit.cod-rep
cod-emitente||y|movadm.rep-tit.cod-emitente
cod-esp||y|movadm.rep-tit.cod-esp
cod-estabel||y|movadm.rep-tit.cod-estabel
ep-codigo||y|movadm.rep-tit.ep-codigo
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "cod-rep",
     Keys-Supplied = "cod-rep,cod-emitente,cod-esp,cod-estabel,ep-codigo"':U).
 
/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
************************
* Initialize Filter Attributes */
RUN set-attribute-list IN THIS-PROCEDURE ('
  Filter-Value=':U).
/************************
</FILTER-ATTRIBUTES> */   
 
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-add 
     IMAGE-UP FILE "image/im-add.bmp":U
     LABEL "Button 13" 
     SIZE 4 BY 1.13.

DEFINE BUTTON bt-can 
     IMAGE-UP FILE "image/im-canc2.bmp":U
     LABEL "Button 11" 
     SIZE 4 BY 1.13.

DEFINE BUTTON bt-conf 
     IMAGE-UP FILE "image/im-chck1.bmp":U
     LABEL "Button 12" 
     SIZE 4 BY 1.13.

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "image/im-era.bmp":U
     LABEL "Button 14" 
     SIZE 4 BY 1.13.

DEFINE BUTTON bt-mod 
     IMAGE-UP FILE "image/process_icon.jpg":U
     IMAGE-INSENSITIVE FILE "image/ii-mod.bmp":U
     LABEL "Modifica" 
     SIZE 4 BY 1.13 TOOLTIP "Altera registro".

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "image/file.png":U
     LABEL "Button 1" 
     SIZE 4 BY 1.13.

DEFINE BUTTON BUTTON-4 
     IMAGE-UP FILE "image/file.png":U
     LABEL "Button 4" 
     SIZE 4 BY 1.13.

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "image/file.png":U
     LABEL "Button 5" 
     SIZE 4 BY 1.13.

DEFINE BUTTON BUTTON-6 
     IMAGE-UP FILE "image/file.png":U
     LABEL "Button 6" 
     SIZE 4 BY 1.13.

DEFINE BUTTON BUTTON-7 
     IMAGE-UP FILE "image/file.png":U
     LABEL "Button 7" 
     SIZE 4 BY 1.13.

DEFINE VARIABLE c_erro AS CHARACTER FORMAT "X(256)":U 
     LABEL "Dir. Erro" 
     VIEW-AS FILL-IN 
     SIZE 60 BY .88 NO-UNDO.

DEFINE VARIABLE c_extrato AS CHARACTER FORMAT "X(256)":U 
     LABEL "Dir. Extrato" 
     VIEW-AS FILL-IN 
     SIZE 60 BY .88 NO-UNDO.

DEFINE VARIABLE c_ok AS CHARACTER FORMAT "X(256)":U 
     LABEL "Dir. Sucesso" 
     VIEW-AS FILL-IN 
     SIZE 60 BY .88 NO-UNDO.

DEFINE VARIABLE c_rem AS CHARACTER FORMAT "X(256)":U 
     LABEL "Dir. Remessa" 
     VIEW-AS FILL-IN 
     SIZE 60 BY .88 NO-UNDO.

DEFINE VARIABLE c_ret AS CHARACTER FORMAT "X(256)":U 
     LABEL "Dir. Retorno" 
     VIEW-AS FILL-IN 
     SIZE 60 BY .88 NO-UNDO.

DEFINE VARIABLE i_cod_inter AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Cod. Inter." 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 TOOLTIP "Identifica a conta da empresa para Accesstage" NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 8.25.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-4 FOR 
      es_param_van_dir SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-4 B-table-Win _STRUCTURED
  QUERY BROWSE-4 NO-LOCK DISPLAY
      es_param_van_dir.cod_intercambio COLUMN-LABEL "Cod.!Intercambio" FORMAT ">>>>>9":U
            WIDTH 10.29
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 13.43 BY 7.25 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BROWSE-4 AT ROW 1.5 COL 2.57 WIDGET-ID 100
     BUTTON-1 AT ROW 1.5 COL 91.57 WIDGET-ID 16
     c_rem AT ROW 1.75 COL 29 COLON-ALIGNED WIDGET-ID 2
     BUTTON-4 AT ROW 2.75 COL 91.57 WIDGET-ID 22
     c_ret AT ROW 2.92 COL 29 COLON-ALIGNED WIDGET-ID 4
     BUTTON-5 AT ROW 3.96 COL 91.72 WIDGET-ID 24
     c_extrato AT ROW 4.08 COL 29 COLON-ALIGNED WIDGET-ID 6
     c_ok AT ROW 5.21 COL 29.14 COLON-ALIGNED WIDGET-ID 12
     BUTTON-6 AT ROW 5.21 COL 91.86 WIDGET-ID 26
     BUTTON-7 AT ROW 6.25 COL 91.86 WIDGET-ID 28
     c_erro AT ROW 6.38 COL 29 COLON-ALIGNED WIDGET-ID 14
     i_cod_inter AT ROW 7.5 COL 29 COLON-ALIGNED WIDGET-ID 30
     bt-mod AT ROW 7.75 COL 49.14 WIDGET-ID 46
     bt-add AT ROW 7.75 COL 53.14 WIDGET-ID 42
     bt-conf AT ROW 7.75 COL 57 WIDGET-ID 40
     bt-can AT ROW 7.75 COL 61 WIDGET-ID 38
     bt-del AT ROW 7.75 COL 68 WIDGET-ID 44
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: ems5_esp.es_param_webserver
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 8.38
         WIDTH              = 96.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */
 
{src/adm/method/browser.i}
{include/c-browse.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB BROWSE-4 RECT-1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-del IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-mod IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON BUTTON-1 IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON BUTTON-4 IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON BUTTON-5 IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON BUTTON-6 IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON BUTTON-7 IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN c_erro IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN c_extrato IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN c_ok IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN c_rem IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN c_ret IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN i_cod_inter IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-4
/* Query rebuild information for BROWSE BROWSE-4
     _TblList          = "ems5_esp.es_param_van_dir OF ems5_esp.es_param_webserver"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > ems5_esp.es_param_van_dir.cod_intercambio
"es_param_van_dir.cod_intercambio" "Cod.!Intercambio" ? "integer" ? ? ? ? ? ? no ? no no "10.29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME bt-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add B-table-Win
ON CHOOSE OF bt-add IN FRAME F-Main /* Button 13 */
DO:
  ENABLE {&list-1} WITH FRAME {&FRAME-NAME}.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-can
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-can B-table-Win
ON CHOOSE OF bt-can IN FRAME F-Main /* Button 11 */
DO:
  DISABLE {&list-1} WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-conf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-conf B-table-Win
ON CHOOSE OF bt-conf IN FRAME F-Main /* Button 12 */
DO:
  RUN pi-validate.

  IF RETURN-VALUE = "NOK" THEN 
      RETURN NO-APPLY.

/*   IF NOT CAN-FIND(FIRST bf_es_param_van_dir WHERE */
/*                         bf_es_param_van_dir. THEN */
/*                                                  */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del B-table-Win
ON CHOOSE OF bt-del IN FRAME F-Main /* Button 14 */
DO:
  ENABLE {&list-1} WITH FRAME {&FRAME-NAME}.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-mod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-mod B-table-Win
ON CHOOSE OF bt-mod IN FRAME F-Main /* Modifica */
DO:
  ENABLE {&list-1} WITH FRAME {&FRAME-NAME}.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 B-table-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Button 1 */
DO:
  SYSTEM-DIALOG GET-DIR c-dir
       INITIAL-DIR SESSION:TEMP-DIRECTORY
       TITLE "Diret¢rio Arquivos de Remessa".

  ASSIGN c_rem:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-dir.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 B-table-Win
ON CHOOSE OF BUTTON-4 IN FRAME F-Main /* Button 4 */
DO:
  SYSTEM-DIALOG GET-DIR c-dir
       INITIAL-DIR SESSION:TEMP-DIRECTORY
       TITLE "Diret¢rio Arquivos de Retorno".

  ASSIGN c_ret:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-dir.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 B-table-Win
ON CHOOSE OF BUTTON-5 IN FRAME F-Main /* Button 5 */
DO:
  SYSTEM-DIALOG GET-DIR c-dir
       INITIAL-DIR SESSION:TEMP-DIRECTORY
       TITLE "Diret¢rio Arquivos de Extrato".

  ASSIGN c_extrato:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-dir.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 B-table-Win
ON CHOOSE OF BUTTON-6 IN FRAME F-Main /* Button 6 */
DO:
  SYSTEM-DIALOG GET-DIR c-dir
       INITIAL-DIR SESSION:TEMP-DIRECTORY
       TITLE "Diret¢rio Arquivos de Sucesso".

  ASSIGN c_ok:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-dir.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 B-table-Win
ON CHOOSE OF BUTTON-7 IN FRAME F-Main /* Button 7 */
DO:
  SYSTEM-DIALOG GET-DIR c-dir
       INITIAL-DIR SESSION:TEMP-DIRECTORY
       TITLE "Diret¢rio Arquivos de Erro".

  ASSIGN c_erro:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-dir.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-4
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
 
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "es_param_webserver"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es_param_webserver"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
    Purpose:     Override standard ADM method
    Notes:       
------------------------------------------------------------------------------*/
            

    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.
 
  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
                    PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt_tiph_est_contrib_margin_prov NO-UNDO LIKE tiph_est_contrib_margin_prov.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
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

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER p_log_corporativo AS LOGICAL NO-UNDO.
DEF SHARED VAR v_cdn_estab_fp0560ymep    AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE h-pai AS HANDLE      NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-historico

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt_tiph_est_contrib_margin_prov

/* Definitions for BROWSE br-historico                                  */
&Scoped-define FIELDS-IN-QUERY-br-historico ~
tt_tiph_est_contrib_margin_prov.perc_margem ~
tt_tiph_est_contrib_margin_prov.perc_ini ~
tt_tiph_est_contrib_margin_prov.perc_fim ~
tt_tiph_est_contrib_margin_prov.dt_inicio ~
tt_tiph_est_contrib_margin_prov.dt_termino ~
tt_tiph_est_contrib_margin_prov.cod_usuario ~
tt_tiph_est_contrib_margin_prov.dt_inclusao ~
tt_tiph_est_contrib_margin_prov.hr_inclusao 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-historico 
&Scoped-define QUERY-STRING-br-historico FOR EACH tt_tiph_est_contrib_margin_prov NO-LOCK ~
    BY tt_tiph_est_contrib_margin_prov.dt_inicio DESCENDING ~
       BY tt_tiph_est_contrib_margin_prov.dt_inclusao DESCENDING ~
        BY tt_tiph_est_contrib_margin_prov.hr_inclusao DESCENDING INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-historico OPEN QUERY br-historico FOR EACH tt_tiph_est_contrib_margin_prov NO-LOCK ~
    BY tt_tiph_est_contrib_margin_prov.dt_inicio DESCENDING ~
       BY tt_tiph_est_contrib_margin_prov.dt_inclusao DESCENDING ~
        BY tt_tiph_est_contrib_margin_prov.hr_inclusao DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-historico tt_tiph_est_contrib_margin_prov
&Scoped-define FIRST-TABLE-IN-QUERY-br-historico tt_tiph_est_contrib_margin_prov


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-historico}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-data-inicio RECT-19 fi-data-fim rt-button ~
bt-confirma IMAGE-12 br-historico IMAGE-13 bt-ok bt-cancelar 
&Scoped-Define DISPLAYED-OBJECTS fi-data-inicio fi-data-fim 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancelar 
     LABEL "&Cancelar" 
     SIZE 13 BY 1.

DEFINE BUTTON bt-confirma 
     IMAGE-UP FILE "image\im-sav":U
     LABEL "Button 1" 
     SIZE 5.14 BY 1.

DEFINE BUTTON bt-ok 
     LABEL "&OK" 
     SIZE 13 BY 1.

DEFINE VARIABLE fi-data-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-data-inicio AS DATE FORMAT "99/99/9999":U INITIAL 01/01/1900 
     LABEL "Data In°cio" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-12
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-13
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 15.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 79 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-historico FOR 
      tt_tiph_est_contrib_margin_prov
    FIELDS(tt_tiph_est_contrib_margin_prov.perc_margem
      tt_tiph_est_contrib_margin_prov.perc_ini
      tt_tiph_est_contrib_margin_prov.perc_fim
      tt_tiph_est_contrib_margin_prov.dt_inicio
      tt_tiph_est_contrib_margin_prov.dt_termino
      tt_tiph_est_contrib_margin_prov.cod_usuario
      tt_tiph_est_contrib_margin_prov.dt_inclusao
      tt_tiph_est_contrib_margin_prov.hr_inclusao) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-historico
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-historico W-Win _STRUCTURED
  QUERY br-historico NO-LOCK DISPLAY
      tt_tiph_est_contrib_margin_prov.perc_margem FORMAT ">>9.99":U
      tt_tiph_est_contrib_margin_prov.perc_ini COLUMN-LABEL "% Ini" FORMAT ">>9.99":U
      tt_tiph_est_contrib_margin_prov.perc_fim COLUMN-LABEL "% Fim" FORMAT ">>9.99":U
      tt_tiph_est_contrib_margin_prov.dt_inicio COLUMN-LABEL "Data In°cio" FORMAT "99/99/9999":U
      tt_tiph_est_contrib_margin_prov.dt_termino COLUMN-LABEL "Data TÇrmino" FORMAT "99/99/9999":U
      tt_tiph_est_contrib_margin_prov.cod_usuario COLUMN-LABEL "Usu†rio" FORMAT "x(12)":U
      tt_tiph_est_contrib_margin_prov.dt_inclusao COLUMN-LABEL "Data Inclus∆o" FORMAT "99/99/9999":U
      tt_tiph_est_contrib_margin_prov.hr_inclusao COLUMN-LABEL "Hora Inclus∆o" FORMAT "x(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 77 BY 12.58 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-data-inicio AT ROW 1.88 COL 20.57 COLON-ALIGNED WIDGET-ID 8
     fi-data-fim AT ROW 1.88 COL 43.72 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     bt-confirma AT ROW 1.75 COL 74 WIDGET-ID 20
     br-historico AT ROW 3.17 COL 2.72 WIDGET-ID 200
     bt-ok AT ROW 16.63 COL 2.43 WIDGET-ID 4
     bt-cancelar AT ROW 16.63 COL 16.29 WIDGET-ID 6
     RECT-19 AT ROW 1.25 COL 1.57 WIDGET-ID 16
     rt-button AT ROW 16.42 COL 1.57 WIDGET-ID 2
     IMAGE-12 AT ROW 1.88 COL 37.29 WIDGET-ID 12
     IMAGE-13 AT ROW 1.88 COL 42.14 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 17 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Temp-Tables and Buffers:
      TABLE: tt_tiph_est_contrib_margin_prov T "?" NO-UNDO  tiph_est_contrib_margin_prov
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 17
         WIDTH              = 80
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 80
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB br-historico IMAGE-12 F-Main */
ASSIGN 
       tt_tiph_est_contrib_margin_prov.perc_margem:AUTO-RESIZE IN BROWSE br-historico = TRUE
       tt_tiph_est_contrib_margin_prov.perc_ini:AUTO-RESIZE IN BROWSE br-historico = TRUE
       tt_tiph_est_contrib_margin_prov.perc_fim:AUTO-RESIZE IN BROWSE br-historico = TRUE
       tt_tiph_est_contrib_margin_prov.dt_inicio:AUTO-RESIZE IN BROWSE br-historico = TRUE
       tt_tiph_est_contrib_margin_prov.dt_termino:AUTO-RESIZE IN BROWSE br-historico = TRUE
       tt_tiph_est_contrib_margin_prov.cod_usuario:AUTO-RESIZE IN BROWSE br-historico = TRUE
       tt_tiph_est_contrib_margin_prov.dt_inclusao:AUTO-RESIZE IN BROWSE br-historico = TRUE
       tt_tiph_est_contrib_margin_prov.hr_inclusao:AUTO-RESIZE IN BROWSE br-historico = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-historico
/* Query rebuild information for BROWSE br-historico
     _TblList          = "Temp-Tables.tt_tiph_est_contrib_margin_prov"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = "USED,"
     _OrdList          = "Temp-Tables.tt_tiph_est_contrib_margin_prov.dt_inicio|no,Temp-Tables.tt_tiph_est_contrib_margin_prov.dt_inclusao|no,Temp-Tables.tt_tiph_est_contrib_margin_prov.hr_inclusao|no"
     _FldNameList[1]   > Temp-Tables.tt_tiph_est_contrib_margin_prov.perc_margem
"tt_tiph_est_contrib_margin_prov.perc_margem" ? ? "decimal" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tt_tiph_est_contrib_margin_prov.perc_ini
"tt_tiph_est_contrib_margin_prov.perc_ini" "% Ini" ? "decimal" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tt_tiph_est_contrib_margin_prov.perc_fim
"tt_tiph_est_contrib_margin_prov.perc_fim" "% Fim" ? "decimal" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.tt_tiph_est_contrib_margin_prov.dt_inicio
"tt_tiph_est_contrib_margin_prov.dt_inicio" "Data In°cio" ? "date" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.tt_tiph_est_contrib_margin_prov.dt_termino
"tt_tiph_est_contrib_margin_prov.dt_termino" "Data TÇrmino" ? "date" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.tt_tiph_est_contrib_margin_prov.cod_usuario
"tt_tiph_est_contrib_margin_prov.cod_usuario" "Usu†rio" ? "character" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.tt_tiph_est_contrib_margin_prov.dt_inclusao
"tt_tiph_est_contrib_margin_prov.dt_inclusao" "Data Inclus∆o" ? "date" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.tt_tiph_est_contrib_margin_prov.hr_inclusao
"tt_tiph_est_contrib_margin_prov.hr_inclusao" "Hora Inclus∆o" ? "character" ? ? ? ? ? ? no ? no no ? yes yes no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE br-historico */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF VALID-HANDLE(h-pai) THEN DO:
      ASSIGN h-pai:SENSITIVE = YES.
  END.
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  IF VALID-HANDLE(h-pai) THEN DO:
      ASSIGN h-pai:SENSITIVE = YES.
  END.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar W-Win
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
  IF VALID-HANDLE(h-pai) THEN DO:
      ASSIGN h-pai:SENSITIVE = YES.
  END.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma W-Win
ON CHOOSE OF bt-confirma IN FRAME F-Main /* Button 1 */
DO:
  {&OPEN-QUERY-br-historico}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok W-Win
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
  IF VALID-HANDLE(h-pai) THEN DO:
      ASSIGN h-pai:SENSITIVE = YES.
  END.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-historico
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fi-data-inicio fi-data-fim 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE fi-data-inicio RECT-19 fi-data-fim rt-button bt-confirma IMAGE-12 
         br-historico IMAGE-13 bt-ok bt-cancelar 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable W-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  ASSIGN h-pai = SOURCE-PROCEDURE
         h-pai = h-pai:CURRENT-WINDOW.

  IF NOT VALID-HANDLE(h-pai) THEN DO:
      ASSIGN h-pai = SESSION:FIRST-CHILD
             h-pai = h-pai:NEXT-SIBLING.
  END.

  IF VALID-HANDLE(h-pai) THEN DO:
      ASSIGN h-pai:SENSITIVE = NO.
  END.
  /* Code placed here will execute AFTER standard behavior.    */

  IF p_log_corporativo THEN DO:
      ASSIGN CURRENT-WINDOW:TITLE = "Hist¢rico Contribution Margin Provis∆o PLR Corporativo".
  END.
  ELSE DO:
      ASSIGN CURRENT-WINDOW:TITLE = "Hist¢rico Contribution Margin Provis∆o PLR N∆o Corporativo".
  END.

  RUN pi-carrega-dados.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-dados W-Win 
PROCEDURE pi-carrega-dados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE tt_tiph_est_contrib_margin_prov.

FOR EACH tiph_est_contrib_margin_prov
    WHERE tiph_est_contrib_margin_prov.cdn_estab = v_cdn_estab_fp0560ymep 
      AND tiph_est_contrib_margin_prov.LOG_corporativo = p_log_corporativo NO-LOCK:
    CREATE tt_tiph_est_contrib_margin_prov.
    BUFFER-COPY tiph_est_contrib_margin_prov TO tt_tiph_est_contrib_margin_prov.
END.

{&OPEN-QUERY-br-historico}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt_tiph_est_contrib_margin_prov"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


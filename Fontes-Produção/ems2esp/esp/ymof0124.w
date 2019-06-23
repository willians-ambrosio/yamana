&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i YMOF0124 "TOTVS"}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE tt-log-klassmatt NO-UNDO
    FIELD IdKlassmatt LIKE es-integra-retorno.IdKlassmatt
    FIELD codigo      LIKE es-integra-retorno.codigo
    FIELD cod-estabel LIKE es-integra-retorno.cod-estabel
    FIELD dt-carga    LIKE es-integra-retorno.dt-carga
    FIELD log-retorno LIKE es-integra-retorno.log-retorno.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE JanelaDetalhe
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME b-itens-klasmat

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-log-klassmatt

/* Definitions for BROWSE b-itens-klasmat                               */
&Scoped-define FIELDS-IN-QUERY-b-itens-klasmat IdKlassmatt codigo cod-estabel dt-carga log-retorno   
&Scoped-define ENABLED-FIELDS-IN-QUERY-b-itens-klasmat   
&Scoped-define SELF-NAME b-itens-klasmat
&Scoped-define QUERY-STRING-b-itens-klasmat FOR EACH tt-log-klassmatt
&Scoped-define OPEN-QUERY-b-itens-klasmat OPEN QUERY {&SELF-NAME} FOR EACH tt-log-klassmatt.
&Scoped-define TABLES-IN-QUERY-b-itens-klasmat tt-log-klassmatt
&Scoped-define FIRST-TABLE-IN-QUERY-b-itens-klasmat tt-log-klassmatt


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-b-itens-klasmat}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-3 cod-item-ini cod-item-fim ~
cod-estab-ini cod-estab-fim bt-filtrar data-implanta-ini data-implanta-fim ~
tg-tipo bt-excel b-itens-klasmat bt-ok bt-cancelar 
&Scoped-Define DISPLAYED-OBJECTS cod-item-ini cod-item-fim cod-estab-ini ~
cod-estab-fim data-implanta-ini data-implanta-fim tg-tipo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-window AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-excel 
     LABEL "Excel" 
     SIZE 12 BY .88.

DEFINE BUTTON bt-filtrar 
     LABEL "Filtrar" 
     SIZE 12 BY .88.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE cod-estab-fim AS CHARACTER FORMAT "X(256)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE cod-estab-ini AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estab" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE cod-item-fim AS CHARACTER FORMAT "X(256)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE cod-item-ini AS CHARACTER FORMAT "X(256)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE data-implanta-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE data-implanta-ini AS DATE FORMAT "99/99/9999":U INITIAL ? 
     LABEL "Data Implant." 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE tg-tipo AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Com Erros", 1,
"Com Exitos", 2,
"Ambos", 3
     SIZE 36 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 95 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 2.75.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY b-itens-klasmat FOR 
      tt-log-klassmatt SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE b-itens-klasmat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS b-itens-klasmat w-window _FREEFORM
  QUERY b-itens-klasmat DISPLAY
      IdKlassmatt COLUMN-LABEL "ID Klassmatt"
 codigo      COLUMN-LABEL "Item"
 cod-estabel COLUMN-LABEL "Estab"
 dt-carga    COLUMN-LABEL "Dt. Carga"
 log-retorno COLUMN-LABEL "LOG"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 95 BY 7.5
         TITLE "Itens" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     cod-item-ini AT ROW 1.5 COL 13.57 COLON-ALIGNED WIDGET-ID 4
     cod-item-fim AT ROW 1.5 COL 30.72 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     cod-estab-ini AT ROW 1.5 COL 49.72 COLON-ALIGNED WIDGET-ID 22
     cod-estab-fim AT ROW 1.5 COL 67.29 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     bt-filtrar AT ROW 1.67 COL 83.14 WIDGET-ID 10
     data-implanta-ini AT ROW 2.5 COL 13.57 COLON-ALIGNED WIDGET-ID 14
     data-implanta-fim AT ROW 2.5 COL 30.72 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     tg-tipo AT ROW 2.63 COL 45.86 NO-LABEL WIDGET-ID 26
     bt-excel AT ROW 2.67 COL 83.14 WIDGET-ID 12
     b-itens-klasmat AT ROW 4.25 COL 1.72 WIDGET-ID 200
     bt-ok AT ROW 12.21 COL 3
     bt-cancelar AT ROW 12.21 COL 85.57
     "<   >" VIEW-AS TEXT
          SIZE 4 BY .67 AT ROW 1.58 COL 64.57 WIDGET-ID 24
     "<   >" VIEW-AS TEXT
          SIZE 4 BY .67 AT ROW 2.58 COL 28 WIDGET-ID 18
     "<   >" VIEW-AS TEXT
          SIZE 4 BY .67 AT ROW 1.58 COL 28 WIDGET-ID 8
     RECT-1 AT ROW 12 COL 1.86
     RECT-3 AT ROW 1.25 COL 1.43 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96 BY 12.58 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: JanelaDetalhe
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "Monitor Klasmatt"
         HEIGHT             = 12.67
         WIDTH              = 96.29
         MAX-HEIGHT         = 28.33
         MAX-WIDTH          = 195.14
         VIRTUAL-HEIGHT     = 28.33
         VIRTUAL-WIDTH      = 195.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-window 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-window.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* BROWSE-TAB b-itens-klasmat bt-excel F-Main */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE b-itens-klasmat
/* Query rebuild information for BROWSE b-itens-klasmat
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-log-klassmatt.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE b-itens-klasmat */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* Monitor Klasmatt */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* Monitor Klasmatt */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-window
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
  apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-excel w-window
ON CHOOSE OF bt-excel IN FRAME F-Main /* Excel */
DO:
  APPLY "ctrl-alt-E".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-filtrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-filtrar w-window
ON CHOOSE OF bt-filtrar IN FRAME F-Main /* Filtrar */
DO:
  ASSIGN cod-item-ini
         cod-item-fim
         data-implanta-ini
         data-implanta-fim
         cod-estab-ini
         cod-estab-fim
         tg-tipo.

  EMPTY TEMP-TABLE tt-log-klassmatt.

  FOR EACH es-integra-retorno NO-LOCK
     WHERE es-integra-retorno.codigo        >= cod-item-ini     
       AND es-integra-retorno.codigo        <= cod-item-fim     
       AND es-integra-retorno.cod-estabel   >= cod-estab-ini
       AND es-integra-retorno.cod-estabel   <= cod-estab-fim
       AND es-integra-retorno.dt-carga      >= date(data-implanta-ini)     
       AND es-integra-retorno.dt-carga      <= date(data-implanta-fim)  :   

      IF tg-tipo = 1 THEN
          IF es-integra-retorno.statusRetorno <> "N" THEN
              NEXT.
      IF tg-tipo = 2 THEN
          IF es-integra-retorno.statusRetorno <> "S" THEN
              NEXT.

          CREATE tt-log-klassmatt.
          ASSIGN tt-log-klassmatt.IdKlassmatt = es-integra-retorno.IdKlassmatt 
                 tt-log-klassmatt.codigo      = es-integra-retorno.codigo      
                 tt-log-klassmatt.cod-estabel = es-integra-retorno.cod-estabel 
                 tt-log-klassmatt.dt-carga    = es-integra-retorno.dt-carga    
                 tt-log-klassmatt.log-retorno = es-integra-retorno.log-retorno.

  END.

     {&OPEN-QUERY-{&BROWSE-NAME}}
         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
  apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME b-itens-klasmat
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-window  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-window  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-window  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
  THEN DELETE WIDGET w-window.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-window  _DEFAULT-ENABLE
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
  DISPLAY cod-item-ini cod-item-fim cod-estab-ini cod-estab-fim 
          data-implanta-ini data-implanta-fim tg-tipo 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-1 RECT-3 cod-item-ini cod-item-fim cod-estab-ini cod-estab-fim 
         bt-filtrar data-implanta-ini data-implanta-fim tg-tipo bt-excel 
         b-itens-klasmat bt-ok bt-cancelar 
      WITH FRAME F-Main IN WINDOW w-window.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-window.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-window 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-window 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-window 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}
  
  {utp/ut9000.i "YMOF0124" "TOTVS"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-window  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-log-klassmatt"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-window 
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


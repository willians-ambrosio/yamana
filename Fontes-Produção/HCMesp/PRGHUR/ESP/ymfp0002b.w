&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          dthresp          PROGRESS
*/
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/buffers_RH.i}

{include/i-prgvrs.i YMFP0002B 1.00.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

{include/i-frm055.i}
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE v-linhas   AS INTEGER    NO-UNDO.

&Scoped-define table-parent grp_usuar_esp

/* insira a seguir a defini‡Æo da temp-table */
DEFINE TEMP-TABLE tt-origem NO-UNDO
       FIELD cdn_empresa        LIKE empresa.ep-codigo
       FIELD cod_grp_usuar      LIKE grp_usuar.cod_grp_usuar
       FIELD cdn_sit_afast_func LIKE sit_afast.cdn_sit_afast_func
       FIELD des_sit_afast_func LIKE sit_afast.des_sit_afast_func
       INDEX id_tt_origem       IS UNIQUE PRIMARY cod_grp_usuar cdn_sit_afast_func ASCENDING.

DEFINE TEMP-TABLE tt-destino NO-UNDO
       FIELD cdn_empresa        LIKE empresa.ep-codigo
       FIELD cod_grp_usuar      LIKE grp_usuar.cod_grp_usuar
       FIELD cdn_sit_afast_func LIKE sit_afast.cdn_sit_afast_func
       FIELD des_sit_afast_func LIKE sit_afast.des_sit_afast_func
       INDEX id_tt_destino      IS UNIQUE PRIMARY cod_grp_usuar cdn_sit_afast_func ASCENDING.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-form2
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-formation
&Scoped-define BROWSE-NAME br-source-browse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-origem tt-destino

/* Definitions for BROWSE br-source-browse                              */
&Scoped-define FIELDS-IN-QUERY-br-source-browse tt-origem.cdn_sit_afast_func tt-origem.des_sit_afast_func   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-source-browse   
&Scoped-define SELF-NAME br-source-browse
&Scoped-define QUERY-STRING-br-source-browse FOR EACH tt-origem                               BY tt-origem.cdn_sit_afast_func
&Scoped-define OPEN-QUERY-br-source-browse OPEN QUERY {&SELF-NAME} FOR EACH tt-origem                               BY tt-origem.cdn_sit_afast_func.
&Scoped-define TABLES-IN-QUERY-br-source-browse tt-origem
&Scoped-define FIRST-TABLE-IN-QUERY-br-source-browse tt-origem


/* Definitions for BROWSE br-target-browse                              */
&Scoped-define FIELDS-IN-QUERY-br-target-browse tt-destino.cdn_sit_afast_func tt-destino.des_sit_afast_func   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-target-browse   
&Scoped-define SELF-NAME br-target-browse
&Scoped-define QUERY-STRING-br-target-browse FOR EACH tt-destino                               BY tt-destino.cdn_sit_afast_func
&Scoped-define OPEN-QUERY-br-target-browse OPEN QUERY {&SELF-NAME} FOR EACH tt-destino                               BY tt-destino.cdn_sit_afast_func.
&Scoped-define TABLES-IN-QUERY-br-target-browse tt-destino
&Scoped-define FIRST-TABLE-IN-QUERY-br-target-browse tt-destino


/* Definitions for FRAME f-formation                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-formation ~
    ~{&OPEN-QUERY-br-source-browse}~
    ~{&OPEN-QUERY-br-target-browse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-key-parent rt-source-browse ~
rt-source-browse-2 RECT-1 br-source-browse br-target-browse bt-add bt-del ~
bt-marca-todos-source bt-desmarca-todos-source bt-marca-todos-target ~
bt-desmarca-todos-target bt-ok bt-cancela bt-ajuda 
&Scoped-Define DISPLAYED-FIELDS grp_usuar_esp.cdn_empresa ~
grp_usuar_esp.cod_grp_usuar 
&Scoped-define DISPLAYED-TABLES grp_usuar_esp
&Scoped-define FIRST-DISPLAYED-TABLE grp_usuar_esp
&Scoped-Define DISPLAYED-OBJECTS v_des_empresa v_des_grp_usuar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD is-create-allowed w-window 
FUNCTION is-create-allowed RETURNS LOGICAL
  ( v-row-tt as rowid)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD is-delete-allowed w-window 
FUNCTION is-delete-allowed RETURNS LOGICAL
  ( v-row-target as rowid)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-window AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-add 
     IMAGE-UP FILE "adeicon\next-au":U
     IMAGE-INSENSITIVE FILE "adeicon\next-ai":U
     LABEL "" 
     SIZE 7 BY 1.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "adeicon\prev-au":U
     IMAGE-INSENSITIVE FILE "adeicon\prev-ai":U
     LABEL "" 
     SIZE 7 BY 1.

DEFINE BUTTON bt-desmarca-todos-source 
     IMAGE-UP FILE "image/ii-ran_b.bmp":U
     IMAGE-INSENSITIVE FILE "adeicon\prev-ai":U
     LABEL "Desmarca Todos" 
     SIZE 6 BY 1 TOOLTIP "Desmarca Todos".

DEFINE BUTTON bt-desmarca-todos-target 
     IMAGE-UP FILE "image/ii-ran_b.bmp":U
     LABEL "" 
     SIZE 6 BY 1 TOOLTIP "Desmarca Todos".

DEFINE BUTTON bt-marca-todos-source 
     IMAGE-UP FILE "image/ii-ran_a.bmp":U
     LABEL "" 
     SIZE 6 BY 1 TOOLTIP "Marca Todos".

DEFINE BUTTON bt-marca-todos-target 
     IMAGE-UP FILE "image/ii-ran_a.bmp":U
     LABEL "" 
     SIZE 6 BY 1 TOOLTIP "Marca Todos".

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE v_des_empresa AS CHARACTER FORMAT "x(32)" 
     VIEW-AS FILL-IN 
     SIZE 48.43 BY .88.

DEFINE VARIABLE v_des_grp_usuar AS CHARACTER FORMAT "x(32)" 
     VIEW-AS FILL-IN 
     SIZE 48.43 BY .88.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 87.86 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE rt-key-parent
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 87.86 BY 2.42.

DEFINE RECTANGLE rt-source-browse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38.57 BY 10.88.

DEFINE RECTANGLE rt-source-browse-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38.57 BY 10.88.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-source-browse FOR 
      tt-origem SCROLLING.

DEFINE QUERY br-target-browse FOR 
      tt-destino SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-source-browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-source-browse w-window _FREEFORM
  QUERY br-source-browse NO-LOCK DISPLAY
      tt-origem.cdn_sit_afast_func 
      tt-origem.des_sit_afast_func
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 36.43 BY 8.75.

DEFINE BROWSE br-target-browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-target-browse w-window _FREEFORM
  QUERY br-target-browse DISPLAY
      tt-destino.cdn_sit_afast_func 
      tt-destino.des_sit_afast_func
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 36.43 BY 8.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-formation
     grp_usuar_esp.cdn_empresa AT ROW 1.58 COL 22.29 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 7 BY .88
     v_des_empresa AT ROW 1.58 COL 29.57 COLON-ALIGNED HELP
          "Descri‡Æo do Grupo de Usu rios" NO-LABEL WIDGET-ID 2
     grp_usuar_esp.cod_grp_usuar AT ROW 2.58 COL 22.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     v_des_grp_usuar AT ROW 2.58 COL 29.57 COLON-ALIGNED HELP
          "Descri‡Æo do Grupo de Usu rios" NO-LABEL
     br-source-browse AT ROW 4.79 COL 2.86
     br-target-browse AT ROW 4.88 COL 52
     bt-add AT ROW 7.04 COL 42
     bt-del AT ROW 8.67 COL 42
     bt-marca-todos-source AT ROW 13.92 COL 8.57
     bt-desmarca-todos-source AT ROW 13.92 COL 25.43
     bt-marca-todos-target AT ROW 13.92 COL 58.29
     bt-desmarca-todos-target AT ROW 13.92 COL 74.29
     bt-ok AT ROW 15.71 COL 2.72
     bt-cancela AT ROW 15.71 COL 13.72
     bt-ajuda AT ROW 15.71 COL 77.86
     "Situa‡äes Ponto" VIEW-AS TEXT
          SIZE 10.86 BY .67 AT ROW 4.04 COL 4.14
     "Situa‡äes NÇO Autorizadas" VIEW-AS TEXT
          SIZE 19 BY .67 AT ROW 4.04 COL 53.43
     rt-key-parent AT ROW 1.33 COL 1.86
     rt-source-browse AT ROW 4.38 COL 1.86
     rt-source-browse-2 AT ROW 4.42 COL 51
     RECT-1 AT ROW 15.5 COL 1.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 16.17.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-form2
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
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
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "Formar <insert Custom SmartWindow title>"
         HEIGHT             = 16.71
         WIDTH              = 89.86
         MAX-HEIGHT         = 30.17
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 30.17
         VIRTUAL-WIDTH      = 146.29
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
/* SETTINGS FOR FRAME f-formation
   FRAME-NAME                                                           */
/* BROWSE-TAB br-source-browse v_des_grp_usuar f-formation */
/* BROWSE-TAB br-target-browse br-source-browse f-formation */
/* SETTINGS FOR FILL-IN grp_usuar_esp.cdn_empresa IN FRAME f-formation
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN grp_usuar_esp.cod_grp_usuar IN FRAME f-formation
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v_des_empresa IN FRAME f-formation
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v_des_grp_usuar IN FRAME f-formation
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-source-browse
/* Query rebuild information for BROWSE br-source-browse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-origem
                              BY tt-origem.cdn_sit_afast_func.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION SORTBY-PHRASE"
     _TblOptList       = "USED"
     _Query            is OPENED
*/  /* BROWSE br-source-browse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-target-browse
/* Query rebuild information for BROWSE br-target-browse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-destino
                              BY tt-destino.cdn_sit_afast_func.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-target-browse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-formation
/* Query rebuild information for FRAME f-formation
     _Query            is NOT OPENED
*/  /* FRAME f-formation */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* Formar <insert Custom SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* Formar <insert Custom SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-source-browse
&Scoped-define SELF-NAME br-source-browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-source-browse w-window
ON MOUSE-SELECT-DBLCLICK OF br-source-browse IN FRAME f-formation
DO: 
  run pi-ins.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-source-browse w-window
ON VALUE-CHANGED OF br-source-browse IN FRAME f-formation
DO:
  {include/i-frm020.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-target-browse
&Scoped-define SELF-NAME br-target-browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-target-browse w-window
ON MOUSE-SELECT-DBLCLICK OF br-target-browse IN FRAME f-formation
DO:
/*   RUN pi-ins. */
  run pi-del.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-target-browse w-window
ON VALUE-CHANGED OF br-target-browse IN FRAME f-formation
DO:
  {include/i-frm010.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add w-window
ON CHOOSE OF bt-add IN FRAME f-formation
DO:
   run pi-ins.

   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-window
ON CHOOSE OF bt-ajuda IN FRAME f-formation /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela w-window
ON CHOOSE OF bt-cancela IN FRAME f-formation /* Fechar */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del w-window
ON CHOOSE OF bt-del IN FRAME f-formation
DO:
   run pi-del.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desmarca-todos-source
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desmarca-todos-source w-window
ON CHOOSE OF bt-desmarca-todos-source IN FRAME f-formation /* Desmarca Todos */
DO:
  br-source-browse:SELECT-ALL() IN FRAME {&FRAME-NAME}.
  if br-source-browse:num-iterations > 0 then do:
      IF (br-source-browse:deselect-rows()) then.
          reposition br-source-browse TO ROW 1.
  end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desmarca-todos-target
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desmarca-todos-target w-window
ON CHOOSE OF bt-desmarca-todos-target IN FRAME f-formation
DO:
  br-target-browse:SELECT-ALL() IN FRAME {&FRAME-NAME}.
  if br-target-browse:num-iterations > 0 then do:
      if (br-target-browse:deselect-rows()) then.
          reposition br-target-browse TO ROW 1.
  end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-marca-todos-source
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-marca-todos-source w-window
ON CHOOSE OF bt-marca-todos-source IN FRAME f-formation
DO:
  br-source-browse:SELECT-ALL() IN FRAME {&FRAME-NAME}.
  if br-source-browse:num-iterations > 0 then do:
      assign v-linhas = 0.
      reposition br-source-browse backwards 10000.

      repeat:
          repeat v-linhas = 1 to br-source-browse:num-iterations:
              if (br-source-browse:select-row(v-linhas)) then.
          end.

          reposition br-source-browse TO ROW 1.
          if (br-source-browse:is-row-selected(br-source-browse:num-iterations)) then
              leave.
      end.       
  end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-marca-todos-target
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-marca-todos-target w-window
ON CHOOSE OF bt-marca-todos-target IN FRAME f-formation
DO:
  br-target-browse:SELECT-ALL() IN FRAME {&FRAME-NAME}.
  if br-target-browse:num-iterations > 0 then do:
      assign v-linhas = 0.
      reposition br-target-browse backwards 10000.

      repeat:
          repeat v-linhas = 1 to br-target-browse:num-iterations:
              if (br-target-browse:select-row(v-linhas)) then.
          end.

          reposition br-target-browse TO ROW 1.
          if (br-target-browse:is-row-selected(br-target-browse:num-iterations)) then
              leave.
      end.
  end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME f-formation /* OK */
DO:
  run pi-commit.
  
  run dispatch in wh-browse (input 'open-query':U).
  
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-source-browse
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */

{include/i-frm040.i}
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
  DISPLAY v_des_empresa v_des_grp_usuar 
      WITH FRAME f-formation IN WINDOW w-window.
  IF AVAILABLE grp_usuar_esp THEN 
    DISPLAY grp_usuar_esp.cdn_empresa grp_usuar_esp.cod_grp_usuar 
      WITH FRAME f-formation IN WINDOW w-window.
  ENABLE rt-key-parent rt-source-browse rt-source-browse-2 RECT-1 
         br-source-browse br-target-browse bt-add bt-del bt-marca-todos-source 
         bt-desmarca-todos-source bt-marca-todos-target 
         bt-desmarca-todos-target bt-ok bt-cancela bt-ajuda 
      WITH FRAME f-formation IN WINDOW w-window.
  {&OPEN-BROWSERS-IN-QUERY-f-formation}
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
  
  {utp/ut9000.i "YMFP0002B" "1.00.00.000"}

  run pi-show-master-record.

  /* Dispatch standard ADM method.                             */  
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-add-to-target w-window 
PROCEDURE pi-add-to-target :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   define input parameter v-row-select-in-source as rowid no-undo. 
   
  /*:T Jamais remova a definição do include a seguir de dentro da lógica do programa */
  {include/i-frm190.i}

   /*:T rowid da tabela que possui um registro selecionado no browse de origem */
   /*
   find tt-primeira-tabela-browse-origem pelo rowid
   if available primeira-tabela-browse-origem  then do:
      find tabela-pai pelo rowid( v-row-parent )
      create primeira-tt-tabela-browse-destino
      assign campos primeira-tt-tabela-browse-destino
   end. 
   */

  /* Criar o registro no browse de destino */
  FIND FIRST tt-origem WHERE
       rowid(tt-origem) = v-row-select-in-source NO-ERROR.
  IF AVAIL tt-origem THEN DO:

      CREATE tt-destino.
      ASSIGN tt-destino.cdn_empresa        = tt-origem.cdn_empresa
             tt-destino.cod_grp_usuar      = tt-origem.cod_grp_usuar
             tt-destino.cdn_sit_afast_func = tt-origem.cdn_sit_afast_func
             tt-destino.des_sit_afast_func = tt-origem.des_sit_afast_func.

      delete tt-origem. 
  END.
   
   /*:T Jamais remova a definição do include a seguir de dentro da lógica do programa */
  {include/i-frm195.i}
  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-commit w-window 
PROCEDURE pi-commit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Grava os registros do browse de destino na tabela espec¡fica */
  FOR EACH tt-destino WHERE
      NOT CAN-FIND(FIRST sit_afast_nao_autoriz NO-LOCK WHERE
                         sit_afast_nao_autoriz.cdn_empresa        = tt-destino.cdn_empresa AND
                         sit_afast_nao_autoriz.cod_grp_usuar      = tt-destino.cod_grp_usuar AND
                         sit_afast_nao_autoriz.cdn_sit_afast_func = tt-destino.cdn_sit_afast_func):

      CREATE sit_afast_nao_autoriz. 
      ASSIGN sit_afast_nao_autoriz.cdn_empresa        = tt-destino.cdn_empresa
             sit_afast_nao_autoriz.cod_grp_usuar      = tt-destino.cod_grp_usuar
             sit_afast_nao_autoriz.cdn_sit_afast_func = tt-destino.cdn_sit_afast_func
             sit_afast_nao_autoriz.des_sit_afast_func = tt-destino.des_sit_afast_func.
  END.

  /* Elimina o registro da tabela espec¡fica, caso o mesmo seja retirado do browse de destino */
  FOR EACH tt-origem,
      FIRST sit_afast_nao_autoriz EXCLUSIVE-LOCK WHERE
            sit_afast_nao_autoriz.cdn_empresa        = tt-origem.cdn_empresa AND
            sit_afast_nao_autoriz.cod_grp_usuar      = tt-origem.cod_grp_usuar AND
            sit_afast_nao_autoriz.cdn_sit_afast_func = tt-origem.cdn_sit_afast_func:

      DELETE sit_afast_nao_autoriz.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-del w-window 
PROCEDURE pi-del :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*:T Jamais remova a definição do include a seguir de dentro da lógica do programa */
  {include/i-frm167.i}
  
  /*:T Caso o include a seguir nao atenda suas necessidades, apague-o  e crie sua propria
  logica usando-o como modelo. 
  */   
  {include/i-frm170.i} /* Esta include exclui o registro do browse destino */
 
  /*:T Jamais remova a definição do include a seguir de dentro da lógica do programa */
  {include/i-frm175.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-delete-from-target w-window 
PROCEDURE pi-delete-from-target :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   define input parameter v-row-target-browse as rowid no-undo.
   
  /*:T Jamais remova a definição do include a seguir de dentro da lógica do programa */
  {include/i-frm180.i}
   
   /*
   find tt-tabela-destino where rowid( tt-tabela-destino ) = v-row-target-browse
        exclusive-lock no-error.
   if available tt-tabela-destino then
      delete tt-tabela-destino.     
   */   

  /* Retirar o registro do browse de destino e pass -lo para o browse de origem */
  find FIRST tt-destino where 
       rowid(tt-destino) = v-row-target-browse no-error.
  if avail tt-destino THEN DO:
      CREATE tt-origem.
      ASSIGN tt-origem.cdn_empresa        = tt-destino.cdn_empresa
             tt-origem.cod_grp_usuar      = tt-destino.cod_grp_usuar
             tt-origem.cdn_sit_afast_func = tt-destino.cdn_sit_afast_func
             tt-origem.des_sit_afast_func = tt-destino.des_sit_afast_func.

      delete tt-destino. 
  END.
   
  /*:T Jamais remova a definição do include a seguir de dentro da lógica do programa */
  {include/i-frm185.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ins w-window 
PROCEDURE pi-ins :
/*:T ------------------------------------------------------------------------------
  Purpose   : Incluir no browse destiono o registro selecionado no browse origem     
  Parameters: 
------------------------------------------------------------------------------*/
  /*:T Jamais remova a definição do include a seguir de dentro da lógica do programa */
  {include/i-frm157.i}

  /*:T Caso o include a seguir nao atenda suas necessidades, apague-o  e crie sua propria
  logica usando-o como modelo. 
  */ 
   {include/i-frm160.i} /* Esta include inclui o registro no browse destino */ 
  
  /*:T Jamais remova a definição do include a seguir de dentro da lógica do programa */
  {include/i-frm165.i}

  {&OPEN-QUERY-br-source-browse}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-show-master-record w-window 
PROCEDURE pi-show-master-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*:T Jamais remova a definição do include a seguir de dentro da lógica do programa */

  {include/i-frm145.i}

  /*:T Lógica padrão para apresentação do registro da tabela pai caso esta lógica atenda 
     as suas necessidades chame o include a seguir no corpo da procedure (retire-o de
     dentro do comentário). Em caso contrário crie sua própria lógica usando a 
     do include como modelo.
  */   

  /*
  {include/i-frm150.i}
  */

  /* Carregar informa‡äes do pai para o campo chave */
  find FIRST grp_usuar_esp no-lock where 
       rowid(grp_usuar_esp) = v-row-parent no-error.
  IF AVAIL grp_usuar_esp THEN DO:
      display grp_usuar_esp.cdn_empresa
              grp_usuar_esp.cod_grp_usuar with frame {&frame-name}.

      FIND empresa NO-LOCK
          WHERE empresa.ep-codigo = grp_usuar_esp.cdn_empresa NO-ERROR.
      ASSIGN v_des_empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = empresa.razao-social
                 INPUT FRAME {&FRAME-NAME} v_des_empresa.

      FIND FIRST grp_usuar NO-LOCK
           WHERE grp_usuar.cod_grp_usuar = grp_usuar_esp.cod_grp_usuar NO-ERROR.
      IF AVAIL grp_usuar THEN 
          ASSIGN v_des_grp_usuar:SCREEN-VALUE IN FRAME {&FRAME-NAME} = grp_usuar.des_grp_usuar
                 INPUT FRAME {&FRAME-NAME} v_des_grp_usuar.

      /* Carregar os registros para o browse de destino */    
      FOR EACH sit_afast_nao_autoriz NO-LOCK WHERE
               sit_afast_nao_autoriz.cdn_empresa   = grp_usuar_esp.cdn_empresa AND
               sit_afast_nao_autoriz.cod_grp_usuar = grp_usuar_esp.cod_grp_usuar:

          CREATE tt-destino.
          ASSIGN tt-destino.cdn_empresa        = sit_afast_nao_autoriz.cdn_empresa
                 tt-destino.cod_grp_usuar      = sit_afast_nao_autoriz.cod_grp_usuar     
                 tt-destino.cdn_sit_afast_func = sit_afast_nao_autoriz.cdn_sit_afast_func 
                 tt-destino.des_sit_afast_func = sit_afast_nao_autoriz.des_sit_afast_func.
      END.
    
      /* Carregar o browse de origem com os registros que nÆo estÆo no browse de destino */
      FOR EACH /*sit_espcif_ptoelet*/ sit_afast  NO-LOCK WHERE
          NOT can-find(FIRST tt-destino WHERE
                             tt-destino.cdn_sit_afast_func = /*sit_espcif_ptoelet.cdn_sit_marcac_ptoelet*/ sit_afast.cdn_sit_afast):
          CREATE tt-origem.
          ASSIGN tt-origem.cdn_empresa        = grp_usuar_esp.cdn_empresa
                 tt-origem.cod_grp_usuar      = grp_usuar_esp.cod_grp_usuar
                 tt-origem.cdn_sit_afast_func = /*sit_espcif_ptoelet.cdn_sit_marcac_ptoelet*/ sit_afast.cdn_sit_afast
                 tt-origem.des_sit_afast_func = /*sit_espcif_ptoelet.des_sit_marcac_ptoelet*/ sit_afast.des_sit_afast.
      END.
    
      {&OPEN-QUERY-br-source-browse}
  END.
  
  {include/i-frm155.i}

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
  {src/adm/template/snd-list.i "tt-destino"}
  {src/adm/template/snd-list.i "tt-origem"}

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
  
  if p-state = "apply-entry":U then
     apply "entry":U to bt-ok in frame {&frame-name}.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION is-create-allowed w-window 
FUNCTION is-create-allowed RETURNS LOGICAL
  ( v-row-tt as rowid) : /*:T rowid do registro selecionado no origem */
/*:T------------------------------------------------------------------------------
  Purpose:  Insira aqui a l¢gica que deve verificar se o registro corrente pode ou
            nÆo ser incluido no browse de destino
    Notes:  
------------------------------------------------------------------------------*/

  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION is-delete-allowed w-window 
FUNCTION is-delete-allowed RETURNS LOGICAL
  ( v-row-target as rowid) : /*:T rowid do registro selecionado no destino */
/*:T------------------------------------------------------------------------------
  Purpose:  Insira aqui a l¢gica que deve verificar se o registro corrente pode ou
            nÆo ser eliminado do browse de destino
    Notes:  
------------------------------------------------------------------------------*/

  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


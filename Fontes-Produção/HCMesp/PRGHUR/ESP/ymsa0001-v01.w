&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          dthresp          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i YMSA0001-v01 1.00.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE v-row-parent   AS ROWID       NO-UNDO.
DEFINE VARIABLE v_num_hra_ini  AS INTEGER     NO-UNDO.
DEFINE VARIABLE v_num_hra_fim  AS INTEGER     NO-UNDO.

DEFINE TEMP-TABLE tt-digita NO-UNDO
   FIELD cdn_dispositivo    LIKE disposit_control_aces.cdn_disposit_control_aces
   FIELD des_dispositivo    LIKE disposit_control_aces.des_disposit_control_aces.

DEFINE TEMP-TABLE tt-digita-aux LIKE tt-digita.

DEFINE BUFFER b_tip_refeicao FOR tip_refeicao.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main
&Scoped-define BROWSE-NAME br-digita

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES tip_refeicao
&Scoped-define FIRST-EXTERNAL-TABLE tip_refeicao


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR tip_refeicao.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-digita

/* Definitions for BROWSE br-digita                                     */
&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.cdn_dispositivo tt-digita.des_dispositivo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-digita.cdn_dispositivo   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-digita tt-digita
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-digita tt-digita
&Scoped-define SELF-NAME br-digita
&Scoped-define QUERY-STRING-br-digita FOR EACH tt-digita
&Scoped-define OPEN-QUERY-br-digita OPEN QUERY br-digita FOR EACH tt-digita.
&Scoped-define TABLES-IN-QUERY-br-digita tt-digita
&Scoped-define FIRST-TABLE-IN-QUERY-br-digita tt-digita


/* Definitions for FRAME f-main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-main ~
    ~{&OPEN-QUERY-br-digita}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tip_refeicao.val_refeicao 
&Scoped-define ENABLED-TABLES tip_refeicao
&Scoped-define FIRST-ENABLED-TABLE tip_refeicao
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS tip_refeicao.cdn_tip_refeicao ~
tip_refeicao.des_tip_refeicao tip_refeicao.val_refeicao 
&Scoped-define DISPLAYED-TABLES tip_refeicao
&Scoped-define FIRST-DISPLAYED-TABLE tip_refeicao
&Scoped-Define DISPLAYED-OBJECTS v_hra_period_ini v_hra_period_fim 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS tip_refeicao.cdn_tip_refeicao ~
tip_refeicao.des_tip_refeicao 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnHora V-table-Win 
FUNCTION fnHora RETURNS INTEGER
  ( INPUT codHora AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-alterar 
     LABEL "&Alterar" 
     SIZE 10 BY 1.13.

DEFINE BUTTON bt-excluir 
     LABEL "&Excluir" 
     SIZE 10 BY 1.13.

DEFINE BUTTON bt-incluir 
     LABEL "&Incluir" 
     SIZE 10 BY 1.13.

DEFINE VARIABLE v_hra_period_fim AS CHARACTER FORMAT "99:99" INITIAL "0000" 
     VIEW-AS FILL-IN 
     SIZE 8.29 BY .88.

DEFINE VARIABLE v_hra_period_ini AS CHARACTER FORMAT "99:99" INITIAL "0000" 
     LABEL "Per¡odo" 
     VIEW-AS FILL-IN 
     SIZE 8.29 BY .88.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 1.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 2.25.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-digita FOR 
      tt-digita SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-digita V-table-Win _FREEFORM
  QUERY br-digita DISPLAY
      tt-digita.cdn_dispositivo COLUMN-LABEL "C¢digo"  WIDTH 08
      tt-digita.des_dispositivo COLUMN-LABEL "Nome"    WIDTH 50
      ENABLE
      tt-digita.cdn_dispositivo
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 78.57 BY 5.75
         BGCOLOR 15 
         TITLE BGCOLOR 15 "Dispositivos".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     tip_refeicao.cdn_tip_refeicao AT ROW 1.17 COL 21.72 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .88
     tip_refeicao.des_tip_refeicao AT ROW 1.17 COL 26.72 COLON-ALIGNED NO-LABEL WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 52.29 BY .88
     tip_refeicao.val_refeicao AT ROW 2.67 COL 21.72 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .88
     v_hra_period_ini AT ROW 3.67 COL 21.72 COLON-ALIGNED HELP
          "Hora inicial do per¡odo" WIDGET-ID 8
     v_hra_period_fim AT ROW 3.67 COL 34.14 COLON-ALIGNED HELP
          "Hora final do per¡odo" NO-LABEL WIDGET-ID 12
     br-digita AT ROW 5.04 COL 2.43 WIDGET-ID 200
     bt-incluir AT ROW 10.88 COL 2.29 WIDGET-ID 16
     bt-alterar AT ROW 10.88 COL 12.29 WIDGET-ID 18
     bt-excluir AT ROW 10.88 COL 22.29 WIDGET-ID 20
     "at‚" VIEW-AS TEXT
          SIZE 3 BY .67 AT ROW 3.79 COL 32.72 WIDGET-ID 14
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 2.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: dthresp.tip_refeicao
   Allow: Basic,DB-Fields
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 11.29
         WIDTH              = 81.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}
{include/i_dbtype.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br-digita v_hra_period_fim f-main */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE br-digita IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-alterar IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-excluir IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-incluir IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tip_refeicao.cdn_tip_refeicao IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN tip_refeicao.des_tip_refeicao IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN v_hra_period_fim IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v_hra_period_ini IN FRAME f-main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-digita
/* Query rebuild information for BROWSE br-digita
     _START_FREEFORM
OPEN QUERY br-digita FOR EACH tt-digita.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-digita */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-digita
&Scoped-define SELF-NAME br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita V-table-Win
ON DEL OF br-digita IN FRAME f-main /* Dispositivos */
DO:
   APPLY 'choose':U TO bt-excluir IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita V-table-Win
ON END-ERROR OF br-digita IN FRAME f-main /* Dispositivos */
ANYWHERE 
DO:
   IF br-digita:NEW-ROW IN FRAME {&FRAME-NAME} THEN DO:
      IF AVAIL tt-digita THEN
         DELETE tt-digita.
      IF br-digita:DELETE-CURRENT-ROW() IN FRAME {&FRAME-NAME} THEN.
   END.
   ELSE DO:
      GET CURRENT br-digita.
      DISPLAY tt-digita.cdn_dispositivo
              tt-digita.des_dispositivo WITH BROWSE br-digita. 
   END.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita V-table-Win
ON ENTER OF br-digita IN FRAME f-main /* Dispositivos */
ANYWHERE
DO:
   APPLY 'tab':U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita V-table-Win
ON INS OF br-digita IN FRAME f-main /* Dispositivos */
DO:
   APPLY 'choose':U TO bt-incluir IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita V-table-Win
ON OFF-END OF br-digita IN FRAME f-main /* Dispositivos */
DO:
   APPLY 'entry':U TO bt-incluir IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita V-table-Win
ON ROW-ENTRY OF br-digita IN FRAME f-main /* Dispositivos */
DO:
   /*:T trigger para inicializar campos da temp table de digita‡Æo
   IF br-digita:NEW-ROW IN FRAME {&FRAME-NAME} THEN
      ASSIGN tt-digita.exemplo:screen-value in browse br-digita = string(today, "99/99/9999").
   end.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita V-table-Win
ON ROW-LEAVE OF br-digita IN FRAME f-main /* Dispositivos */
DO:
   /*:T  aqui que a grava‡Æo da linha da temp-table ‚ efetivada.
        Por‚m as valida‡äes dos registros devem ser feitas na procedure pi-executar,
        no local indicado pelo coment rio */
    
   IF br-digita:NEW-ROW IN FRAME {&FRAME-NAME} THEN
      DO TRANSACTION ON ERROR UNDO, RETURN NO-APPLY:
         CREATE tt-digita.
         ASSIGN INPUT BROWSE br-digita tt-digita.cdn_dispositivo
                INPUT BROWSE br-digita tt-digita.des_dispositivo.

         br-digita:CREATE-RESULT-LIST-ENTRY() IN FRAME {&FRAME-NAME}.
      END.
      ELSE DO TRANSACTION ON ERROR UNDO, RETURN NO-APPLY:
         IF AVAIL tt-digita THEN
            ASSIGN INPUT BROWSE br-digita tt-digita.cdn_dispositivo
                   INPUT BROWSE br-digita tt-digita.des_dispositivo.
      END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-alterar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-alterar V-table-Win
ON CHOOSE OF bt-alterar IN FRAME f-main /* Alterar */
DO:
   APPLY 'entry':U TO tt-digita.cdn_dispositivo IN BROWSE br-digita.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-excluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-excluir V-table-Win
ON CHOOSE OF bt-excluir IN FRAME f-main /* Excluir */
DO:
   IF br-digita:NUM-SELECTED-ROWS > 0 THEN DO ON ERROR UNDO, RETURN NO-APPLY:
      GET CURRENT br-digita.
      DELETE tt-digita.
      IF br-digita:DELETE-CURRENT-ROW() IN FRAME {&FRAME-NAME} THEN.
    END.

    IF NUM-RESULTS("br-digita":U) = 0 THEN
       ASSIGN bt-alterar:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              bt-excluir:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-incluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-incluir V-table-Win
ON CHOOSE OF bt-incluir IN FRAME f-main /* Incluir */
DO:
   ASSIGN bt-alterar:SENSITIVE IN FRAME {&FRAME-NAME} = YES
          bt-excluir:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

   IF NUM-RESULTS("br-digita":U) > 0 THEN
      br-digita:INSERT-ROW("AFTER":U) IN FRAME {&FRAME-NAME}.
   ELSE DO TRANSACTION:
      CREATE tt-digita.
      OPEN QUERY br-digita FOR EACH tt-digita.
      APPLY "ENTRY":U TO tt-digita.cdn_dispositivo IN BROWSE br-digita. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  ON LEAVE OF tt-digita.cdn_dispositivo IN BROWSE br-digita DO:
     FIND FIRST disposit_control_aces NO-LOCK
          WHERE disposit_control_aces.cdn_disposit_control_aces = INPUT BROWSE br-digita tt-digita.cdn_dispositivo NO-ERROR.
     IF AVAIL disposit_control_aces THEN
        ASSIGN tt-digita.des_dispositivo:SCREEN-VALUE IN BROWSE br-digita = disposit_control_aces.des_disposit_control_aces NO-ERROR.
     ELSE
        ASSIGN tt-digita.des_dispositivo:SCREEN-VALUE IN BROWSE br-digita = "" NO-ERROR.
  END.

  ON "MOUSE-SELECT-DBLCLICK":U OF tt-digita.cdn_dispositivo IN BROWSE br-digita OR
     "F5":U OF tt-digita.cdn_dispositivo IN BROWSE br-digita DO:
     {include/zoomvar.i &prog-zoom="object/sotm/zoom/z01tm078.w"  
                        &campo=tt-digita.cdn_dispositivo
                        &campozoom=cdn_disposit_control_aces
                        &campo2=tt-digita.des_dispositivo
                        &campozoom2=des_disposit_control_aces
                        &browse=br-digita}
  END.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).
  &ENDIF
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "tip_refeicao"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "tip_refeicao"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME f-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN v_hra_period_ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0000"
         v_hra_period_fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0000".

  EMPTY TEMP-TABLE tt-digita NO-ERROR.
  {&OPEN-QUERY-br-digita}

  ASSIGN bt-alterar:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         bt-excluir:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    IF adm-new-record THEN DO:
       IF CAN-FIND(FIRST b_tip_refeicao
                   WHERE b_tip_refeicao.cdn_tip_refeicao = INPUT FRAME {&FRAME-NAME} tip_refeicao.cdn_tip_refeicao) THEN DO:
          RUN utp/ut-msgs.p (INPUT "SHOW":U,
                             INPUT 1,
                             INPUT "Tipo Refei‡Æo":U).
          APPLY "ENTRY":U TO tip_refeicao.cdn_tip_refeicao IN FRAME {&FRAME-NAME}.
          RETURN 'ADM-ERROR':U.
       END.

       IF INPUT FRAME {&FRAME-NAME} tip_refeicao.des_tip_refeicao = "" THEN DO:
          RUN utp/ut-msgs.p (INPUT "SHOW":U,
                             INPUT 17006,
                             INPUT "Descri‡Æo em branco!~~" + 
                                   "Descri‡Æo do Tipo Refei‡Æo deve ser informado!").
          APPLY "ENTRY":U TO tip_refeicao.des_tip_refeicao IN FRAME {&FRAME-NAME}.
          RETURN 'ADM-ERROR':U.
       END.
    END.

    ASSIGN v_num_hra_ini = fnHora(INPUT FRAME {&FRAME-NAME} v_hra_period_ini)
           v_num_hra_fim = fnHora(INPUT FRAME {&FRAME-NAME} v_hra_period_fim).

    EMPTY TEMP-TABLE tt-digita-aux NO-ERROR.
    FOR EACH tt-digita:
       CREATE tt-digita-aux.
       BUFFER-COPY tt-digita TO tt-digita-aux.
    END.

    IF AVAIL tip_refeicao THEN DO:
       FIND FIRST b_tip_refeicao NO-LOCK
            WHERE ROWID(b_tip_refeicao)             <> ROWID(tip_refeicao)
              AND b_tip_refeicao.num_hra_period_ini <= v_num_hra_ini
              AND b_tip_refeicao.num_hra_period_fim >= v_num_hra_ini NO-ERROR.
       IF AVAIL b_tip_refeicao THEN DO:
          RUN utp/ut-msgs.p (INPUT "SHOW":U,
                             INPUT 17006,
                             INPUT "Per¡odo Inicial inv lido!~~" + 
                                   "Per¡odo inicial conflitante com Tipo Refei‡Æo " + 
                                   STRING(b_tip_refeicao.cdn_tip_refeicao)).
          APPLY "ENTRY":U TO v_hra_period_ini IN FRAME {&FRAME-NAME}.
          RETURN 'ADM-ERROR':U.
       END.
       FIND FIRST b_tip_refeicao NO-LOCK
            WHERE ROWID(b_tip_refeicao)             <> ROWID(tip_refeicao)
              AND b_tip_refeicao.num_hra_period_ini <= v_num_hra_fim
              AND b_tip_refeicao.num_hra_period_fim >= v_num_hra_fim NO-ERROR.
       IF AVAIL b_tip_refeicao THEN DO:
          RUN utp/ut-msgs.p (INPUT "SHOW":U,
                             INPUT 17006,
                             INPUT "Per¡odo Final inv lido!~~" + 
                                   "Per¡odo final conflitante com Tipo Refei‡Æo " + 
                                   STRING(b_tip_refeicao.cdn_tip_refeicao)).
          APPLY "ENTRY":U TO v_hra_period_fim IN FRAME {&FRAME-NAME}.
          RETURN 'ADM-ERROR':U.
       END.
    END.
    ELSE DO:
       FIND FIRST b_tip_refeicao NO-LOCK
            WHERE b_tip_refeicao.num_hra_period_ini <= v_num_hra_ini
              AND b_tip_refeicao.num_hra_period_fim >= v_num_hra_ini NO-ERROR.
       IF AVAIL b_tip_refeicao THEN DO:
          RUN utp/ut-msgs.p (INPUT "SHOW":U,
                             INPUT 17006,
                             INPUT "Per¡odo Inicial inv lido!~~" + 
                                   "Per¡odo inicial conflitante com Tipo Refei‡Æo " + 
                                   STRING(b_tip_refeicao.cdn_tip_refeicao)).
          APPLY "ENTRY":U TO v_hra_period_ini IN FRAME {&FRAME-NAME}.
          RETURN 'ADM-ERROR':U.
       END.
       FIND FIRST b_tip_refeicao NO-LOCK
            WHERE b_tip_refeicao.num_hra_period_ini <= v_num_hra_fim
              AND b_tip_refeicao.num_hra_period_fim >= v_num_hra_fim NO-ERROR.
       IF AVAIL b_tip_refeicao THEN DO:
          RUN utp/ut-msgs.p (INPUT "SHOW":U,
                             INPUT 17006,
                             INPUT "Per¡odo Final inv lido!~~" + 
                                   "Per¡odo final conflitante com Tipo Refei‡Æo " + 
                                   STRING(b_tip_refeicao.cdn_tip_refeicao)).
          APPLY "ENTRY":U TO v_hra_period_fim IN FRAME {&FRAME-NAME}.
          RETURN 'ADM-ERROR':U.
       END.
    END.

    /*:T Ponha na pi-validate todas as valida‡äes */
    /*:T NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    {include/i-valid.i}

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.

    /*:T Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */
    IF AVAIL tip_refeicao THEN DO:
       ASSIGN tip_refeicao.num_hra_period_ini = v_num_hra_ini
              tip_refeicao.num_hra_period_fim = v_num_hra_fim.

       FOR EACH tip_refeicao_disp OF tip_refeicao EXCLUSIVE-LOCK:
          DELETE tip_refeicao_disp.
       END.

       FOR EACH tt-digita-aux,
          FIRST disposit_control_aces NO-LOCK
          WHERE disposit_control_aces.cdn_disposit_control_aces = tt-digita-aux.cdn_dispositivo:
          IF NOT CAN-FIND(FIRST tip_refeicao_disp
                          WHERE tip_refeicao_disp.cdn_tip_refeicao          = tip_refeicao.cdn_tip_refeicao
                            AND tip_refeicao_disp.cdn_disposit_control_aces = tt-digita-aux.cdn_dispositivo) THEN DO:
             CREATE tip_refeicao_disp.
             ASSIGN tip_refeicao_disp.cdn_tip_refeicao          = tip_refeicao.cdn_tip_refeicao
                    tip_refeicao_disp.cdn_disposit_control_aces = tt-digita-aux.cdn_dispositivo.
          END.
       END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF AVAIL tip_refeicao THEN DO:
     FOR EACH tip_refeicao_disp OF tip_refeicao EXCLUSIVE-LOCK:
        DELETE tip_refeicao_disp.
     END.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if defined(ADM-MODIFY-FIELDS) &then
        disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
    DISABLE v_hra_period_ini
            v_hra_period_fim
            br-digita
            bt-incluir
            bt-alterar
            bt-excluir
            WITH FRAME {&FRAME-NAME}.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAIL tip_refeicao THEN DO:
     ASSIGN v_hra_period_ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = REPLACE(STRING(tip_refeicao.num_hra_period_ini,"HH:MM"),":","")
            v_hra_period_fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = REPLACE(STRING(tip_refeicao.num_hra_period_fim,"HH:MM"),":","").

     EMPTY TEMP-TABLE tt-digita NO-ERROR.

     FOR EACH tip_refeicao_disp OF tip_refeicao:
        CREATE tt-digita.
        ASSIGN tt-digita.cdn_dispositivo = tip_refeicao_disp.cdn_disposit_control_aces.

        FIND FIRST disposit_control_aces NO-LOCK
             WHERE disposit_control_aces.cdn_disposit_control_aces = tip_refeicao_disp.cdn_disposit_control_aces NO-ERROR.
        IF AVAIL disposit_control_aces THEN
           ASSIGN tt-digita.des_dispositivo = disposit_control_aces.des_disposit_control_aces.
     END.

     {&OPEN-QUERY-br-digita}
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if defined(ADM-MODIFY-FIELDS) &then
    if adm-new-record then
       enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
    ENABLE v_hra_period_ini
           v_hra_period_fim
           br-digita
           bt-incluir WITH FRAME {&FRAME-NAME}.

    IF CAN-FIND(FIRST tt-digita) THEN
       ENABLE bt-alterar
              bt-excluir WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-parent V-table-Win 
PROCEDURE pi-atualiza-parent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define input parameter v-row-parent-externo as rowid no-undo.
    
    assign v-row-parent = v-row-parent-externo.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: NÆo fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida‡äes, pois neste ponto do programa o registro 
  ainda nÆo foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Valida‡Æo de dicion rio */

/*:T    Segue um exemplo de valida‡Æo de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */

      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tip_refeicao"}
  {src/adm/template/snd-list.i "tt-digita"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
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
      {src/adm/template/vstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnHora V-table-Win 
FUNCTION fnHora RETURNS INTEGER
  ( INPUT codHora AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE v_num_hora AS INTEGER     NO-UNDO.

  IF NUM-ENTRIES(codHora,":") > 1 THEN
     ASSIGN v_num_hora = INTEGER(ENTRY(01,codHora,":")) * 3600 + 
                         INTEGER(ENTRY(02,codHora,":")) * 60.
  ELSE
     ASSIGN v_num_hora = INTEGER(SUBSTRING(codHora,01,02)) * 3600 + 
                         INTEGER(SUBSTRING(codHora,03,02)) * 60.

  RETURN v_num_hora.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


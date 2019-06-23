&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME wFormation


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-mmv-especialid-func NO-UNDO LIKE mmv-especialid-func
       fields r-rowid as rowid.
DEFINE TEMP-TABLE tt-mmv-func-ofici NO-UNDO LIKE mmv-func-ofici
       FIELD r-rowid as rowid.
DEFINE TEMP-TABLE tt-mmv-setor-ofici NO-UNDO LIKE mmv-setor-ofici
       FIELD r-rowid as rowid.
DEFINE TEMP-TABLE tt-mmv-tar-ord-manut NO-UNDO LIKE mmv-tar-ord-manut
       FIELD r-rowid as rowid.
DEFINE TEMP-TABLE tt-mmv-tecnico-tarefa-om NO-UNDO LIKE mmv-tecnico-tarefa-om
       FIELD r-rowid as rowid.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wFormation 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESMV0301 2.00.00.000}  /*** 010001 ***/
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
CREATE WIDGET-POOL.
/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESMV0301
&GLOBAL-DEFINE Version        2.00.00.000

&GLOBAL-DEFINE First          YES
&GLOBAL-DEFINE Prev           YES
&GLOBAL-DEFINE Next           YES
&GLOBAL-DEFINE Last           YES
&GLOBAL-DEFINE GoTo           NO
&GLOBAL-DEFINE Search         NO

&GLOBAL-DEFINE DelTarget      YES
&GLOBAL-DEFINE AddTarget      YES
&GLOBAL-DEFINE DelAllTarget   NO
&GLOBAL-DEFINE AddAllTarget   NO

&GLOBAL-DEFINE UpdateTarget   YES

&GLOBAL-DEFINE NumRowsReturnedSource 100000
&GLOBAL-DEFINE NumRowsReturnedTarget 100000

&GLOBAL-DEFINE ttParent       tt-mmv-tar-ord-manut
&GLOBAL-DEFINE hDBOParent     hDBOmmv-tar-ord-manut
&GLOBAL-DEFINE DBOParentTable mmv-tar-ord-manut

&GLOBAL-DEFINE ttSource       tt-mmv-especialid-func 
&GLOBAL-DEFINE hDBOSource     hDBOEspecialid-func
&GLOBAL-DEFINE DBOSourceTable mmv-especialid-func

&GLOBAL-DEFINE ttTarget       tt-mmv-tecnico-tarefa-om
&GLOBAL-DEFINE hDBOTarget     hDBOmmv-tecnico-tarefa-om
&GLOBAL-DEFINE DBOTargetTable mmv-tecnico-tarefa-om

&GLOBAL-DEFINE page0Fields    tt-mmv-tar-ord-manut.nr-ord-produ ~
                              tt-mmv-tar-ord-manut.num-seq ~
                              tt-mmv-tar-ord-manut.cod-evento ~
                              tt-mmv-tar-ord-manut.cod-sub-sist

&GLOBAL-DEFINE SourceBrowse   brSource
&GLOBAL-DEFINE TargetBrowse   brTarget

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE iRowid                        AS INTEGER        NO-UNDO.
DEFINE VARIABLE lprimeira                     AS LOGICAL INIT YES NO-UNDO.
DEFINE VARIABLE rowSelected                   AS ROWID          NO-UNDO.

/* Local Variable Definitions (DBOs Handles) ---                        */
DEFINE VARIABLE {&hDBOParent}                 AS HANDLE NO-UNDO.
DEFINE VARIABLE {&hDBOSource}                 AS HANDLE NO-UNDO.
DEFINE VARIABLE {&hDBOTarget}                 AS HANDLE NO-UNDO.
DEFINE VARIABLE hDBOSetorOficina              AS HANDLE NO-UNDO.
DEFINE VARIABLE hDBOEspecialidade             AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE i-cancel-formation AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE cCodAdd            AS CHARACTER NO-UNDO.
define variable iCounter                      AS INTEGER NO-UNDO.
DEFINE TEMP-TABLE tt-selected-rows NO-UNDO
        FIELD r-row-id                        AS ROWID.
DEFINE VARIABLE cDescSetor                    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDescSetorOfici               AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDescEspecialidade            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDescFunc                     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c-nr-ord-produ  LIKE tt-mmv-tar-ord-manut.nr-ord-produ.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Formation
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fPage0
&Scoped-define BROWSE-NAME brSource

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-mmv-especialid-func ~
tt-mmv-tecnico-tarefa-om

/* Definitions for BROWSE brSource                                      */
&Scoped-define FIELDS-IN-QUERY-brSource tt-mmv-especialid-func.cod-especialid tt-mmv-especialid-func.descricao   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brSource   
&Scoped-define SELF-NAME brSource
&Scoped-define QUERY-STRING-brSource FOR EACH tt-mmv-especialid-func  NO-LOCK
&Scoped-define OPEN-QUERY-brSource OPEN QUERY {&SELF-NAME} FOR EACH tt-mmv-especialid-func  NO-LOCK.
&Scoped-define TABLES-IN-QUERY-brSource tt-mmv-especialid-func
&Scoped-define FIRST-TABLE-IN-QUERY-brSource tt-mmv-especialid-func


/* Definitions for BROWSE brTarget                                      */
&Scoped-define FIELDS-IN-QUERY-brTarget tt-mmv-tecnico-tarefa-om.cod-especialid des-especialidade(tt-mmv-tecnico-tarefa-om.cod-especialid) @ cDescEspecialidade tt-mmv-tecnico-tarefa-om.dt-prevista tt-mmv-tecnico-tarefa-om.tempo-previsto tt-mmv-tecnico-tarefa-om.cod-matr des-func(tt-mmv-tecnico-tarefa-om.cod-matr) @ cDescFunc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brTarget   
&Scoped-define SELF-NAME brTarget
&Scoped-define QUERY-STRING-brTarget for each tt-mmv-tecnico-tarefa-om     where tt-mmv-tecnico-tarefa-om.nr-ord-produ = tt-mmv-tar-ord-manut.nr-ord-produ     and   tt-mmv-tecnico-tarefa-om.num-seq      = tt-mmv-tar-ord-manut.num-seq no-lock
&Scoped-define OPEN-QUERY-brTarget open query {&SELF-NAME} for each tt-mmv-tecnico-tarefa-om     where tt-mmv-tecnico-tarefa-om.nr-ord-produ = tt-mmv-tar-ord-manut.nr-ord-produ     and   tt-mmv-tecnico-tarefa-om.num-seq      = tt-mmv-tar-ord-manut.num-seq no-lock.
&Scoped-define TABLES-IN-QUERY-brTarget tt-mmv-tecnico-tarefa-om
&Scoped-define FIRST-TABLE-IN-QUERY-brTarget tt-mmv-tecnico-tarefa-om


/* Definitions for FRAME fPage1                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fPage1 ~
    ~{&OPEN-QUERY-brSource}~
    ~{&OPEN-QUERY-brTarget}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-mmv-tar-ord-manut.nr-ord-produ ~
tt-mmv-tar-ord-manut.num-seq tt-mmv-tar-ord-manut.cod-evento ~
tt-mmv-tar-ord-manut.cod-sub-sist 
&Scoped-define ENABLED-TABLES tt-mmv-tar-ord-manut
&Scoped-define FIRST-ENABLED-TABLE tt-mmv-tar-ord-manut
&Scoped-Define ENABLED-OBJECTS rtToolBar btFirst btPrev btNext btLast ~
btGoTo btQueryJoins btReportsJoins btExit btHelp fi-des-evento ~
fi-des-subSist 
&Scoped-Define DISPLAYED-FIELDS tt-mmv-tar-ord-manut.nr-ord-produ ~
tt-mmv-tar-ord-manut.num-seq tt-mmv-tar-ord-manut.cod-evento ~
tt-mmv-tar-ord-manut.cod-sub-sist 
&Scoped-define DISPLAYED-TABLES tt-mmv-tar-ord-manut
&Scoped-define FIRST-DISPLAYED-TABLE tt-mmv-tar-ord-manut
&Scoped-Define DISPLAYED-OBJECTS fi-des-evento fi-des-subSist 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD des-especialidade wFormation 
FUNCTION des-especialidade RETURNS CHARACTER
  ( pDescricao1 AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD des-func wFormation 
FUNCTION des-func RETURNS CHARACTER
  ( pFunc as char /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD des-setor wFormation 
FUNCTION des-setor RETURNS CHARACTER
  ( pDescricao AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD des-setor-tarefa wFormation 
FUNCTION des-setor-tarefa RETURNS CHARACTER
  ( pDescricao AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wFormation AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU smFile 
       MENU-ITEM miFirst        LABEL "&Primeiro"      ACCELERATOR "CTRL-HOME"
       MENU-ITEM miPrev         LABEL "&Anterior"      ACCELERATOR "CTRL-CURSOR-LEFT"
       MENU-ITEM miNext         LABEL "&Pr¢ximo"       ACCELERATOR "CTRL-CURSOR-RIGHT"
       MENU-ITEM miLast         LABEL "&éltimo"        ACCELERATOR "CTRL-END"
       RULE
       MENU-ITEM miGoTo         LABEL "&V  Para"       ACCELERATOR "CTRL-T"
       MENU-ITEM miSearch       LABEL "&Pesquisa"      ACCELERATOR "CTRL-F5"
       RULE
       MENU-ITEM miQueryJoins   LABEL "&Consultas"    
       MENU-ITEM miReportsJoins LABEL "&Relat¢rios"   
       RULE
       MENU-ITEM miExit         LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU smHelp 
       MENU-ITEM miContents     LABEL "&Conte£do"     
       RULE
       MENU-ITEM miAbout        LABEL "&Sobre..."     .

DEFINE MENU mbMain MENUBAR
       SUB-MENU  smFile         LABEL "&Arquivo"      
       SUB-MENU  smHelp         LABEL "&Ajuda"        .


/* Definitions of the field level widgets                               */
DEFINE BUTTON btExit 
     IMAGE-UP FILE "image/im-exi":U
     IMAGE-INSENSITIVE FILE "image/ii-exi":U
     LABEL "Exit" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btFirst 
     IMAGE-UP FILE "image/im-fir":U
     IMAGE-INSENSITIVE FILE "image/ii-fir":U
     LABEL "First":L 
     SIZE 4 BY 1.25.

DEFINE BUTTON btGoTo 
     IMAGE-UP FILE "image/im-enter":U
     IMAGE-INSENSITIVE FILE "image/ii-enter":U
     LABEL "Go To" 
     SIZE 4 BY 1.25.

DEFINE BUTTON btHelp 
     IMAGE-UP FILE "image/im-hel":U
     IMAGE-INSENSITIVE FILE "image/ii-hel":U
     LABEL "Help" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btLast 
     IMAGE-UP FILE "image/im-las":U
     IMAGE-INSENSITIVE FILE "image/ii-las":U
     LABEL "Last":L 
     SIZE 4 BY 1.25.

DEFINE BUTTON btNext 
     IMAGE-UP FILE "image/im-nex":U
     IMAGE-INSENSITIVE FILE "image/ii-nex":U
     LABEL "Next":L 
     SIZE 4 BY 1.25.

DEFINE BUTTON btPrev 
     IMAGE-UP FILE "image/im-pre":U
     IMAGE-INSENSITIVE FILE "image/ii-pre":U
     LABEL "Prev":L 
     SIZE 4 BY 1.25.

DEFINE BUTTON btQueryJoins 
     IMAGE-UP FILE "image/im-joi":U
     IMAGE-INSENSITIVE FILE "image/ii-joi":U
     LABEL "Query Joins" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btReportsJoins 
     IMAGE-UP FILE "image/im-pri":U
     IMAGE-INSENSITIVE FILE "image/ii-pri":U
     LABEL "Reports Joins" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE VARIABLE fi-des-evento AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-des-subSist AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY .88 NO-UNDO.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.5
     BGCOLOR 7 .

DEFINE BUTTON btAddTarget 
     IMAGE-UP FILE "adeicon/next-au":U
     IMAGE-INSENSITIVE FILE "adeicon/next-ai":U
     LABEL "" 
     SIZE 7 BY .88 TOOLTIP "Inclui".

DEFINE BUTTON btDelTarget 
     IMAGE-UP FILE "adeicon/prev-au":U
     IMAGE-INSENSITIVE FILE "adeicon/prev-ai":U
     LABEL "" 
     SIZE 7 BY .88 TOOLTIP "Retira".

DEFINE BUTTON btUpdateTarget 
     LABEL "&Alterar" 
     SIZE 10 BY .88.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brSource FOR 
      tt-mmv-especialid-func SCROLLING.

DEFINE QUERY brTarget FOR 
      tt-mmv-tecnico-tarefa-om SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brSource
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brSource wFormation _FREEFORM
  QUERY brSource NO-LOCK DISPLAY
      tt-mmv-especialid-func.cod-especialid  
tt-mmv-especialid-func.descricao
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 32 BY 8
         FONT 1.

DEFINE BROWSE brTarget
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brTarget wFormation _FREEFORM
  QUERY brTarget NO-LOCK DISPLAY
      tt-mmv-tecnico-tarefa-om.cod-especialid FORMAT "X(12)":U
des-especialidade(tt-mmv-tecnico-tarefa-om.cod-especialid) @ cDescEspecialidade FORMAT "x(30)":U label "Descri‡Æo":U
tt-mmv-tecnico-tarefa-om.dt-prevista    FORMAT "99/99/9999":U
tt-mmv-tecnico-tarefa-om.tempo-previsto FORMAT "->>,>>9.99":U
tt-mmv-tecnico-tarefa-om.cod-matr       FORMAT "X(12)":U      
des-func(tt-mmv-tecnico-tarefa-om.cod-matr) @ cDescFunc format "x(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 32 BY 8
         FONT 1 ROW-HEIGHT-CHARS .46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fPage0
     btFirst AT ROW 1.13 COL 1.57 HELP
          "Primeira ocorrˆncia"
     btPrev AT ROW 1.13 COL 5.57 HELP
          "Ocorrˆncia anterior"
     btNext AT ROW 1.13 COL 9.57 HELP
          "Pr¢xima ocorrˆncia"
     btLast AT ROW 1.13 COL 13.57 HELP
          "éltima ocorrˆncia"
     btGoTo AT ROW 1.13 COL 17.57 HELP
          "V  Para"
     btQueryJoins AT ROW 1.13 COL 74.86 HELP
          "Consultas relacionadas"
     btReportsJoins AT ROW 1.13 COL 78.86 HELP
          "Relat¢rios relacionados"
     btExit AT ROW 1.13 COL 82.86 HELP
          "Sair"
     btHelp AT ROW 1.13 COL 86.86 HELP
          "Ajuda"
     tt-mmv-tar-ord-manut.nr-ord-produ AT ROW 3 COL 15.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.57 BY .88
     tt-mmv-tar-ord-manut.num-seq AT ROW 3 COL 33.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.43 BY .88
     tt-mmv-tar-ord-manut.cod-evento AT ROW 4 COL 15.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.57 BY .88
     fi-des-evento AT ROW 4 COL 24.29 COLON-ALIGNED NO-LABEL
     tt-mmv-tar-ord-manut.cod-sub-sist AT ROW 5 COL 15.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.57 BY .88
     fi-des-subSist AT ROW 5 COL 27 COLON-ALIGNED NO-LABEL
     rtToolBar AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17.08
         FONT 1.

DEFINE FRAME fPage1
     brSource AT ROW 1.25 COL 2
     brTarget AT ROW 1.25 COL 52
     btAddTarget AT ROW 4.29 COL 39.57
     btDelTarget AT ROW 5.42 COL 39.57
     btUpdateTarget AT ROW 9.33 COL 52
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 7.75
         SIZE 84.43 BY 9.5
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Formation
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-mmv-especialid-func T "?" NO-UNDO mgesp mmv-especialid-func
      ADDITIONAL-FIELDS:
          fields r-rowid as rowid
      END-FIELDS.
      TABLE: tt-mmv-func-ofici T "?" NO-UNDO mgfro mmv-func-ofici
      ADDITIONAL-FIELDS:
          FIELD r-rowid as rowid
      END-FIELDS.
      TABLE: tt-mmv-setor-ofici T "?" NO-UNDO mgfro mmv-setor-ofici
      ADDITIONAL-FIELDS:
          FIELD r-rowid as rowid
      END-FIELDS.
      TABLE: tt-mmv-tar-ord-manut T "?" NO-UNDO movfro mmv-tar-ord-manut
      ADDITIONAL-FIELDS:
          FIELD r-rowid as rowid
      END-FIELDS.
      TABLE: tt-mmv-tecnico-tarefa-om T "?" NO-UNDO mgesp mmv-tecnico-tarefa-om
      ADDITIONAL-FIELDS:
          FIELD r-rowid as rowid
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wFormation ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 17.08
         WIDTH              = 90
         MAX-HEIGHT         = 27.83
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 27.83
         VIRTUAL-WIDTH      = 146.29
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU mbMain:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wFormation 
/* ************************* Included-Libraries *********************** */

{formation/formation.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wFormation
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME fPage1:FRAME = FRAME fPage0:HANDLE.

/* SETTINGS FOR FRAME fPage0
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME fPage1
                                                                        */
/* BROWSE-TAB brSource 1 fPage1 */
/* BROWSE-TAB brTarget brSource fPage1 */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wFormation)
THEN wFormation:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brSource
/* Query rebuild information for BROWSE brSource
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-mmv-especialid-func  NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE brSource */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brTarget
/* Query rebuild information for BROWSE brTarget
     _START_FREEFORM
open query {&SELF-NAME} for each tt-mmv-tecnico-tarefa-om
    where tt-mmv-tecnico-tarefa-om.nr-ord-produ = tt-mmv-tar-ord-manut.nr-ord-produ
    and   tt-mmv-tecnico-tarefa-om.num-seq      = tt-mmv-tar-ord-manut.num-seq no-lock.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE brTarget */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage0
/* Query rebuild information for FRAME fPage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fPage0 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage1
/* Query rebuild information for FRAME fPage1
     _Query            is NOT OPENED
*/  /* FRAME fPage1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wFormation
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wFormation wFormation
ON END-ERROR OF wFormation
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wFormation wFormation
ON WINDOW-CLOSE OF wFormation
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btAddTarget
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAddTarget wFormation
ON CHOOSE OF btAddTarget IN FRAME fPage1
DO:
    DEFINE VARIABLE contAdd         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cCodDel         AS CHAR      NO-UNDO.

    ASSIGN i-cancel-formation = NO.
    
    FOR EACH tt-selected-rows EXCLUSIVE-LOCK:
        DELETE tt-selected-rows.
    END.

    DO contAdd = 1 TO brsource:NUM-SELECTED-ROWS:
        IF brsource:FETCH-SELECTED-ROW(contAdd) THEN DO:
            CREATE tt-selected-rows.
            ASSIGN tt-selected-rows.r-row-id = {&ttSource}.r-rowid.
        END.
    END.
    FOR EACH tt-selected-rows NO-LOCK:
        FIND FIRST {&ttSource}
            WHERE {&ttSource}.r-rowid EQ tt-selected-rows.r-row-id NO-LOCK NO-ERROR.

            IF AVAILABLE {&ttSource} THEN DO:
                ASSIGN cCodDel = {&ttSource}.cod-especialid.

                {Formation/AddTarget.i}
                {formation/updatetarget.i &ProgramTarget="mvp/esmv0301a.w"}

                WAIT-FOR "CLOSE":U OF hTargetProgram.

                IF i-cancel-formation = YES THEN DO:
                    SESSION:SET-WAIT-STATE("GENERAL":U).
                    FOR FIRST {&ttTarget} 
                        WHERE {&ttTarget}.cod-especialid = cCodDel EXCLUSIVE-LOCK:
                            RUN goToKey IN {&hDBOTarget} (INPUT {&ttTarget}.nr-ord-produ, 
                                                          INPUT {&ttTarget}.num-seq,
                                                          input {&ttTarget}.cod-especialid).
                            RUN deleteRecord IN {&hDBOTarget}.
                    END. /*--- For first ---*/
                    SESSION:SET-WAIT-STATE("":U).
                END.
            END.
        END.
    run openQueries.
    EMPTY TEMP-TABLE tt-selected-rows.
    ASSIGN contAdd = 0.
    brtarget:REFRESHABLE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDelTarget
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelTarget wFormation
ON CHOOSE OF btDelTarget IN FRAME fPage1
DO:
    DEFINE VARIABLE contDel AS INTEGER NO-UNDO.

    brtarget:REFRESHABLE = NO.

    DO contDel = brtarget:NUM-SELECTED-ROWS  TO 1 BY -1 :

       IF brtarget:FETCH-SELECTED-ROW(contDel) THEN DO:
            
           {formation/deltarget.i}
         
       END.
    END.

    EMPTY TEMP-TABLE tt-selected-rows.

    brtarget:REFRESHABLE = YES.

    RUN openQueries.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage0
&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit wFormation
ON CHOOSE OF btExit IN FRAME fPage0 /* Exit */
OR CHOOSE OF MENU-ITEM miExit IN MENU mbMain DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFirst wFormation
ON CHOOSE OF btFirst IN FRAME fPage0 /* First */
OR CHOOSE OF MENU-ITEM miFirst IN MENU mbMain DO:
    RUN getFirst IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btGoTo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btGoTo wFormation
ON CHOOSE OF btGoTo IN FRAME fPage0 /* Go To */
OR CHOOSE OF MENU-ITEM miGoTo IN MENU mbMain DO:
    RUN goToRecord IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btHelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp wFormation
ON CHOOSE OF btHelp IN FRAME fPage0 /* Help */
OR CHOOSE OF MENU-ITEM miContents IN MENU mbMain DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btLast wFormation
ON CHOOSE OF btLast IN FRAME fPage0 /* Last */
OR CHOOSE OF MENU-ITEM miLast IN MENU mbMain DO:
    RUN getLast IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btNext wFormation
ON CHOOSE OF btNext IN FRAME fPage0 /* Next */
OR CHOOSE OF MENU-ITEM miNext IN MENU mbMain DO:
    RUN getNext IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPrev wFormation
ON CHOOSE OF btPrev IN FRAME fPage0 /* Prev */
OR CHOOSE OF MENU-ITEM miPrev IN MENU mbMain DO:
    RUN getPrev IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btQueryJoins
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btQueryJoins wFormation
ON CHOOSE OF btQueryJoins IN FRAME fPage0 /* Query Joins */
OR CHOOSE OF MENU-ITEM miQueryJoins IN MENU mbMain DO:
    RUN showQueryJoins IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btReportsJoins
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btReportsJoins wFormation
ON CHOOSE OF btReportsJoins IN FRAME fPage0 /* Reports Joins */
OR CHOOSE OF MENU-ITEM miReportsJoins IN MENU mbMain DO:
    RUN showReportsJoins IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btUpdateTarget
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdateTarget wFormation
ON CHOOSE OF btUpdateTarget IN FRAME fPage1 /* Alterar */
DO:
    ASSIGN i-cancel-formation = YES.
    
    {formation/updatetarget.i &ProgramTarget="mvp/esmv0301a.w"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage0
&Scoped-define SELF-NAME tt-mmv-tar-ord-manut.cod-evento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-tar-ord-manut.cod-evento wFormation
ON LEAVE OF tt-mmv-tar-ord-manut.cod-evento IN FRAME fPage0 /* Evento */
DO:
    {Include/leave.i 
               &tabela=mab-event
               &atributo-ref=des-evento
               &variavel-ref=fi-des-evento
               &where="mab-event.cod-evento = input frame fPage0 tt-mmv-tar-ord-manut.cod-evento"}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-mmv-tar-ord-manut.cod-sub-sist
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-tar-ord-manut.cod-sub-sist wFormation
ON LEAVE OF tt-mmv-tar-ord-manut.cod-sub-sist IN FRAME fPage0 /* Sub-Sistema */
DO:     
    {Include/leave.i 
               &tabela=mab-sub-sist
               &atributo-ref=des-sub-sist
               &variavel-ref=fi-des-subSist
               &where="mab-sub-sist.cod-sub-sist = input frame fPage0 tt-mmv-tar-ord-manut.cod-sub-sist"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brSource
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wFormation 


/* ***************************  Main Block  *************************** */

/*:T--- L¢gica para inicializa‡Æo do programam ---*/
{formation/mainblock.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterDisplayFields wFormation 
PROCEDURE afterDisplayFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    APPLY "LEAVE":U TO tt-mmv-tar-ord-manut.cod-evento   IN FRAME fPage0.
    APPLY "LEAVE":U TO tt-mmv-tar-ord-manut.cod-sub-sist IN FRAME fPage0.

    {&OPEN-QUERY-brSource}
    {&OPEN-QUERY-brTarget}

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wFormation 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {utp/ut-liter.i Especialidade}
    assign tt-mmv-especialid-func.cod-especialid:label in browse brSource = return-value.
    {utp/ut-liter.i Descri‡Æo}
    assign tt-mmv-especialid-func.descricao:label in browse brSource = return-value
           cDescEspecialidade:label               in browse brTarget = return-value
           cDescFunc:label                        in browse brTarget = return-value.
    {utp/ut-liter.i Funcion rio}
    assign tt-mmv-tecnico-tarefa-om.cod-matr:label in browse brTarget = return-value.


    ASSIGN btUpdateTarget:SENSITIVE   IN FRAME fPage1 = YES
           btAddTarget   :SENSITIVE   IN FRAME fPage1 = YES
           btDelTarget   :SENSITIVE   IN FRAME fPage1 = yes
           btGoTo        :SENSITIVE   IN FRAME fPage0 = YES.

    RUN OpenQueries.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goToRecord wFormation 
PROCEDURE goToRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     
    DEFINE BUTTON btGoToCancel AUTO-END-KEY 
         LABEL "&Cancelar" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE BUTTON btGoToOK AUTO-GO 
         LABEL "&OK" 
         SIZE 10 BY 1
         BGCOLOR 8.
    
    DEFINE RECTANGLE rtGoToButton
         EDGE-PIXELS 2 GRAPHIC-EDGE  
         SIZE 58 BY 1.42
         BGCOLOR 7.
    
    DEFINE VARIABLE rGoTo AS ROWID NO-UNDO.
    
    DEFINE VARIABLE cep-codigo     LIKE {&ttParent}.nr-ord-produ   NO-UNDO.
    DEFINE VARIABLE ccod-eqpto     LIKE {&ttParent}.num-seq     NO-UNDO.
    
    DEFINE FRAME fGoToRecord
        cep-codigo      AT ROW 1.21 COL 17.72 COLON-ALIGNED
        ccod-eqpto        AT ROW 2.21 COL 17.72 COLON-ALIGNED
        btGoToOK          AT ROW 3.63 COL 2.14
        btGoToCancel      AT ROW 3.63 COL 13
        rtGoToButton      AT ROW 3.38 COL 1
        SPACE(0.28)
        WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
             THREE-D SCROLLABLE TITLE "" FONT 1
             DEFAULT-BUTTON btGoToOK CANCEL-BUTTON btGoToCancel.

    RUN utp/ut-trfrrp.p (input Frame fGoToRecord:Handle).
    {utp/ut-liter.i V _Para_Ordem_de_Manuten‡Æo *}
    ASSIGN FRAME fGoToRecord:TITLE = RETURN-VALUE.
    
    ON "CHOOSE":U OF btGoToOK IN FRAME fGoToRecord DO:
        ASSIGN cep-codigo ccod-eqpto.
        
        /*:T Posiciona query, do DBO, atrav‚s dos valores do ¡ndice £nico */
        RUN goToKey IN {&hDBOParent} (INPUT cep-codigo,
                                      input ccod-eqpto).
        IF RETURN-VALUE = "NOK":U THEN DO:
            {utp/ut-liter.i "Equipamento"}
            RUN utp/ut-msgs.p (INPUT "SHOW":U, INPUT 2, INPUT return-value).
            
            RETURN NO-APPLY.
        END.
        
        /*:T Retorna rowid do registro corrente do DBO */
        RUN getRowid IN {&hDBOParent} (OUTPUT rGoTo).
        
        /*:T Reposiciona registro com base em um rowid */
        RUN repositionRecord IN THIS-PROCEDURE (INPUT rGoTo).
        
        APPLY "GO":U TO FRAME fGoToRecord.
    END.
    
    ENABLE cep-codigo  
           ccod-eqpto    
           btGoToOK 
           btGoToCancel 
        WITH FRAME fGoToRecord. 
    
    WAIT-FOR "GO":U OF FRAME fGoToRecord.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeDBOs wFormation 
PROCEDURE initializeDBOs :
/*:T------------------------------------------------------------------------------
  Purpose:     Inicializa DBOs
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    /*:T--- Verifica se o DBO da tabela Pai j  est  inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOParent}) OR
       {&hDBOParent}:TYPE <> "PROCEDURE":U OR
       {&hDBOParent}:FILE-NAME <> "frbo/bofr073.p":U THEN DO:
        RUN frbo/bofr073.p PERSISTENT SET {&hDBOParent}.
    END.
     RUN openQueryStatic IN {&hDBOParent} (INPUT "Main":U) NO-ERROR.
    
    /*:T--- Verifica se o DBO da tabela Origem j  est  inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOSource}) OR
       {&hDBOSource}:TYPE <> "PROCEDURE":U OR
       {&hDBOSource}:FILE-NAME <> "yambo/ydm004.p":U THEN DO:
        RUN yambo/ydm004.p PERSISTENT SET {&hDBOSource}.
    END.
     RUN openQueryStatic IN {&hDBOSource} (INPUT "Main":U) NO-ERROR.

    /*:T--- Verifica se o DBO da tabela Forma‡Æo j  est  inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOTarget}) OR
       {&hDBOTarget}:TYPE <> "PROCEDURE":U OR
       {&hDBOTarget}:FILE-NAME <> "yambo/ydm002.p":U THEN DO:
        RUN yambo/ydm002.p PERSISTENT SET {&hDBOTarget}.
    END.
     RUN openQueryStatic IN {&hDBOTarget} (INPUT "Main":U) NO-ERROR.

        /*--- DBO Setor ---*/
    IF NOT VALID-HANDLE(hDBOSetorOficina) OR
       hDBOSetorOficina:TYPE <> "PROCEDURE":U OR
       hDBOSetorOficina:FILE-NAME <> "frbo/bofr036.p":U THEN DO:
        run frbo/bofr036.p persistent set  hDBOSetorOficina.
    END.

    /*--- DBO Especialidade ---*/
    IF NOT VALID-HANDLE(hDBOEspecialidade) OR
       hDBOEspecialidade:TYPE <> "PROCEDURE":U OR
       hDBOEspecialidade:FILE-NAME <> "yambo/ydm004.p":U THEN DO:
        run yambo/ydm004.p persistent set  hDBOEspecialidade.
    END.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openQueries wFormation 
PROCEDURE openQueries :
/*:T------------------------------------------------------------------------------
  Purpose:     Atualiza browsers 
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

    {formation/openqueriestarget.i &Parent="TarefaOrdem"
                                   &Query="TarefaOrdem"}

    {formation/openqueriessource.i &Query="Main"
                                   &OpenAlways="YES"}

    
    assign rRepositionTarget = ?.
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveParentFields wFormation 
PROCEDURE saveParentFields :
/*:T------------------------------------------------------------------------------
  Purpose:     Salva valores dos campos na tabela forma‡Æo ({&ttTarget}) com base 
               nos campos da tabela pai ({&ttParent}) e tabela origem ({&ttSource})
  Parameters:  
------------------------------------------------------------------------------*/
    ASSIGN {&ttTarget}.nr-ord-produ   = {&ttParent}.nr-ord-produ
           {&ttTarget}.cod-especialid = {&ttSource}.cod-especialid 
           {&ttTarget}.num-seq        = {&ttParent}.num-seq.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setarPosicao wFormation 
PROCEDURE setarPosicao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER i-ordem  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER i-tarefa AS INTEGER NO-UNDO.

DEFINE VARIABLE r-rowid AS ROWID      NO-UNDO.

RUN goToKey IN hDBOmmv-tar-ord-manut (INPUT i-ordem,
                                      INPUT i-tarefa).

RUN getRowid IN hDBOmmv-tar-ord-manut (OUTPUT r-rowid).

RUN repositionRecord IN THIS-PROCEDURE (INPUT r-rowid).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION des-especialidade wFormation 
FUNCTION des-especialidade RETURNS CHARACTER
  ( pDescricao1 AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cRetorno AS CHARACTER  NO-UNDO.

    FOR EACH mmv-especialid-func
        WHERE mmv-especialid-func.cod-especialid = pDescricao1 NO-LOCK:
        ASSIGN cRetorno = mmv-especialid-func.descricao.

    END.

  RETURN cRetorno.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION des-func wFormation 
FUNCTION des-func RETURNS CHARACTER
  ( pFunc as char /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    for first mmv-func-ofici
        where mmv-func-ofici.cod-matr = pFunc no-lock:
    end.
    if avail mmv-func-ofici then
        return mmv-func-ofici.nom-func.
    else
        return "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION des-setor wFormation 
FUNCTION des-setor RETURNS CHARACTER
  ( pDescricao AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cRetorno AS CHARACTER  NO-UNDO.

    FOR FIRST mmv-setor-ofici
        WHERE mmv-setor-ofici.cod-setor-ofici = pDescricao NO-LOCK:
        ASSIGN cRetorno = mmv-setor-ofici.des-setor-ofici.
    END.

    RETURN cRetorno.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION des-setor-tarefa wFormation 
FUNCTION des-setor-tarefa RETURNS CHARACTER
  ( pDescricao AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cRetorno AS CHARACTER  NO-UNDO.
    
    FOR FIRST mmv-setor-ofici
        WHERE mmv-setor-ofici.cod-setor-ofici = pDescricao NO-LOCK:
        ASSIGN cRetorno = mmv-setor-ofici.des-setor-ofici.
    END.

    RETURN cRetorno.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME wFormation


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-mmv-especialid-func NO-UNDO LIKE mmv-especialid-func
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-mmv-func-ofici NO-UNDO LIKE mmv-func-ofici
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-mmv-func-relac-especialid NO-UNDO LIKE mmv-func-relac-especialid
       field r-rowid as rowid.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wFormation 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa for ems2cadme.empresa.

{include/i-prgvrs.i ESMV0111 2.06.00.000}

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program          ESMV0111
&GLOBAL-DEFINE Version          2.06.00.000

&GLOBAL-DEFINE First            YES
&GLOBAL-DEFINE Prev             YES
&GLOBAL-DEFINE Next             YES
&GLOBAL-DEFINE Last             YES
&GLOBAL-DEFINE GoTo             YES
&GLOBAL-DEFINE Search           YES

&GLOBAL-DEFINE DelTarget        YES
&GLOBAL-DEFINE AddTarget        YES
&GLOBAL-DEFINE DelAllTarget     no
&GLOBAL-DEFINE AddAllTarget     no

&GLOBAL-DEFINE UpdateTarget     YES

&GLOBAL-DEFINE ttParent         tt-mmv-func-ofici
&GLOBAL-DEFINE hDBOParent       hDBOFuncOfici
&GLOBAL-DEFINE DBOParentTable   mmv-func-ofici

&GLOBAL-DEFINE ttSource         tt-mmv-especialid-func
&GLOBAL-DEFINE hDBOSource       hDBOEspec
&GLOBAL-DEFINE DBOSourceTable   mmv-especialid-func

&GLOBAL-DEFINE ttTarget         tt-mmv-func-relac-especialid
&GLOBAL-DEFINE hDBOTarget       hDBORelac
&GLOBAL-DEFINE DBOTargetTable   mmv-func-relac-especialid

&GLOBAL-DEFINE page0Fields      tt-mmv-func-ofici.ep-codigo tt-mmv-func-ofici.cod-estabel tt-mmv-func-ofici.cod-matr tt-mmv-func-ofici.nom-func ~
                                c-desc-emp c-desc-estab

&GLOBAL-DEFINE page1Fields      cEspecialidade

&GLOBAL-DEFINE SourceBrowse     brSource
&GLOBAL-DEFINE TargetBrowse     brTarget

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
define variable cDescricao like mi-espec.descricao no-undo.
define variable cAtividade as char format "x(12)"  no-undo.

/* Local Variable Definitions (DBOs Handles) ---                        */
DEFINE VARIABLE {&hDBOParent}  AS HANDLE NO-UNDO.
DEFINE VARIABLE {&hDBOSource}  AS HANDLE NO-UNDO.
DEFINE VARIABLE {&hDBOTarget}  AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE tt-selected-rows NO-UNDO
    FIELD r-row-id        AS ROWID.

DEFINE NEW SHARED VARIABLE l-del-reg-0111 AS LOGICAL NO-UNDO.

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
tt-mmv-func-relac-especialid

/* Definitions for BROWSE brSource                                      */
&Scoped-define FIELDS-IN-QUERY-brSource tt-mmv-especialid-func.cod-especialid tt-mmv-especialid-func.descricao   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brSource   
&Scoped-define SELF-NAME brSource
&Scoped-define QUERY-STRING-brSource for each tt-mmv-especialid-func
&Scoped-define OPEN-QUERY-brSource open query {&SELF-NAME} for each tt-mmv-especialid-func.
&Scoped-define TABLES-IN-QUERY-brSource tt-mmv-especialid-func
&Scoped-define FIRST-TABLE-IN-QUERY-brSource tt-mmv-especialid-func


/* Definitions for BROWSE brTarget                                      */
&Scoped-define FIELDS-IN-QUERY-brTarget tt-mmv-func-relac-especialid.cod-especialid fnDescricao(tt-mmv-func-relac-especialid.cod-especialid) @ cDescricao fnAtividade(tt-mmv-func-relac-especialid.num-livre-1) @ cAtividade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brTarget   
&Scoped-define SELF-NAME brTarget
&Scoped-define QUERY-STRING-brTarget for each tt-mmv-func-relac-especialid
&Scoped-define OPEN-QUERY-brTarget open query {&SELF-NAME} for each tt-mmv-func-relac-especialid.
&Scoped-define TABLES-IN-QUERY-brTarget tt-mmv-func-relac-especialid
&Scoped-define FIRST-TABLE-IN-QUERY-brTarget tt-mmv-func-relac-especialid


/* Definitions for FRAME fPage1                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fPage1 ~
    ~{&OPEN-QUERY-brSource}~
    ~{&OPEN-QUERY-brTarget}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-mmv-func-ofici.ep-codigo ~
tt-mmv-func-ofici.cod-estabel tt-mmv-func-ofici.cod-matr ~
tt-mmv-func-ofici.nom-func 
&Scoped-define ENABLED-TABLES tt-mmv-func-ofici
&Scoped-define FIRST-ENABLED-TABLE tt-mmv-func-ofici
&Scoped-Define ENABLED-OBJECTS rtToolBar rtParent btFirst btPrev btNext ~
btLast btGoTo btSearch btQueryJoins btReportsJoins btExit btHelp c-desc-emp ~
c-desc-estab 
&Scoped-Define DISPLAYED-FIELDS tt-mmv-func-ofici.ep-codigo ~
tt-mmv-func-ofici.cod-estabel tt-mmv-func-ofici.cod-matr ~
tt-mmv-func-ofici.nom-func 
&Scoped-define DISPLAYED-TABLES tt-mmv-func-ofici
&Scoped-define FIRST-DISPLAYED-TABLE tt-mmv-func-ofici
&Scoped-Define DISPLAYED-OBJECTS c-desc-emp c-desc-estab 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnAtividade wFormation 
FUNCTION fnAtividade RETURNS CHARACTER
  ( pAtividade as int /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnDescricao wFormation 
FUNCTION fnDescricao RETURNS CHARACTER
  ( pEspec as char /* parameter-definitions */ )  FORWARD.

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
     IMAGE-UP FILE "image\im-exi":U
     IMAGE-INSENSITIVE FILE "image\ii-exi":U
     LABEL "Exit" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btFirst 
     IMAGE-UP FILE "image\im-fir":U
     IMAGE-INSENSITIVE FILE "image\ii-fir":U
     LABEL "First":L 
     SIZE 4 BY 1.25.

DEFINE BUTTON btGoTo 
     IMAGE-UP FILE "image\im-enter":U
     IMAGE-INSENSITIVE FILE "image\ii-enter":U
     LABEL "Go To" 
     SIZE 4 BY 1.25.

DEFINE BUTTON btHelp 
     IMAGE-UP FILE "image\im-hel":U
     IMAGE-INSENSITIVE FILE "image\ii-hel":U
     LABEL "Help" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btLast 
     IMAGE-UP FILE "image\im-las":U
     IMAGE-INSENSITIVE FILE "image\ii-las":U
     LABEL "Last":L 
     SIZE 4 BY 1.25.

DEFINE BUTTON btNext 
     IMAGE-UP FILE "image\im-nex":U
     IMAGE-INSENSITIVE FILE "image\ii-nex":U
     LABEL "Next":L 
     SIZE 4 BY 1.25.

DEFINE BUTTON btPrev 
     IMAGE-UP FILE "image\im-pre":U
     IMAGE-INSENSITIVE FILE "image\ii-pre":U
     LABEL "Prev":L 
     SIZE 4 BY 1.25.

DEFINE BUTTON btQueryJoins 
     IMAGE-UP FILE "image\im-joi":U
     IMAGE-INSENSITIVE FILE "image\ii-joi":U
     LABEL "Query Joins" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btReportsJoins 
     IMAGE-UP FILE "image\im-pri":U
     IMAGE-INSENSITIVE FILE "image\ii-pri":U
     LABEL "Reports Joins" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btSearch 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "Search" 
     SIZE 4 BY 1.25.

DEFINE VARIABLE c-desc-emp AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-estab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41.43 BY .88 NO-UNDO.

DEFINE RECTANGLE rtParent
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 3.5.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.5
     BGCOLOR 7 .

DEFINE BUTTON bt-confirma 
     IMAGE-UP FILE "image\im-sav":U
     LABEL "Button 1" 
     SIZE 5.14 BY 1.

DEFINE BUTTON btAddTarget 
     IMAGE-UP FILE "adeicon\next-au":U
     IMAGE-INSENSITIVE FILE "adeicon\next-ai":U
     LABEL "" 
     SIZE 7 BY 1 TOOLTIP "Inclui".

DEFINE BUTTON btDelTarget 
     IMAGE-UP FILE "adeicon\prev-au":U
     IMAGE-INSENSITIVE FILE "adeicon\prev-ai":U
     LABEL "" 
     SIZE 7 BY 1 TOOLTIP "Retira".

DEFINE BUTTON btUpdateTarget 
     LABEL "Alterar" 
     SIZE 10 BY 1.

DEFINE VARIABLE cEspecialidade AS CHARACTER FORMAT "X(12)":U 
     LABEL "Especialidade" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brSource FOR 
      tt-mmv-especialid-func SCROLLING.

DEFINE QUERY brTarget FOR 
      tt-mmv-func-relac-especialid SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brSource
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brSource wFormation _FREEFORM
  QUERY brSource DISPLAY
      tt-mmv-especialid-func.cod-especialid
tt-mmv-especialid-func.descricao
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 32 BY 8
         FONT 2.

DEFINE BROWSE brTarget
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brTarget wFormation _FREEFORM
  QUERY brTarget DISPLAY
      tt-mmv-func-relac-especialid.cod-especialid
fnDescricao(tt-mmv-func-relac-especialid.cod-especialid) @ cDescricao format "x(20)":U label "Descri‡Æo":U
fnAtividade(tt-mmv-func-relac-especialid.num-livre-1) @ cAtividade format "x(12)":U label "Atividade":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 32 BY 8
         FONT 2.


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
     btSearch AT ROW 1.13 COL 21.57 HELP
          "Pesquisa"
     btQueryJoins AT ROW 1.13 COL 74.86 HELP
          "Consultas relacionadas"
     btReportsJoins AT ROW 1.13 COL 78.86 HELP
          "Relat¢rios relacionados"
     btExit AT ROW 1.13 COL 82.86 HELP
          "Sair"
     btHelp AT ROW 1.13 COL 86.86 HELP
          "Ajuda"
     tt-mmv-func-ofici.ep-codigo AT ROW 2.96 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY .88
     c-desc-emp AT ROW 2.96 COL 26.29 COLON-ALIGNED NO-LABEL
     tt-mmv-func-ofici.cod-estabel AT ROW 4 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY .88
     c-desc-estab AT ROW 4 COL 26.43 COLON-ALIGNED NO-LABEL
     tt-mmv-func-ofici.cod-matr AT ROW 5 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     tt-mmv-func-ofici.nom-func AT ROW 5 COL 31.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 36.57 BY .88
     rtToolBar AT ROW 1 COL 1
     rtParent AT ROW 2.67 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 16.21
         FONT 1.

DEFINE FRAME fPage1
     bt-confirma AT ROW 1.08 COL 28.86
     cEspecialidade AT ROW 1.13 COL 11 COLON-ALIGNED
     brTarget AT ROW 1.25 COL 52
     brSource AT ROW 2.25 COL 2
     btAddTarget AT ROW 4.42 COL 39.57
     btDelTarget AT ROW 5.54 COL 39.57
     btUpdateTarget AT ROW 9.25 COL 52
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 6.67
         SIZE 84.43 BY 9.46
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
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-mmv-func-ofici T "?" NO-UNDO mgfro mmv-func-ofici
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-mmv-func-relac-especialid T "?" NO-UNDO mgesp mmv-func-relac-especialid
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
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
         HEIGHT             = 16.29
         WIDTH              = 90
         MAX-HEIGHT         = 21.21
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.21
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = yes
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
/* BROWSE-TAB brTarget cEspecialidade fPage1 */
/* BROWSE-TAB brSource brTarget fPage1 */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wFormation)
THEN wFormation:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brSource
/* Query rebuild information for BROWSE brSource
     _START_FREEFORM
open query {&SELF-NAME} for each tt-mmv-especialid-func.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brSource */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brTarget
/* Query rebuild information for BROWSE brTarget
     _START_FREEFORM
open query {&SELF-NAME} for each tt-mmv-func-relac-especialid.
     _END_FREEFORM
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
&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma wFormation
ON CHOOSE OF bt-confirma IN FRAME fPage1 /* Button 1 */
DO:
    RUN setConstraintTpEspecial IN {&hDBOSource} (INPUT cEspecialidade:SCREEN-VALUE IN FRAME fPage1).
    RUN openQueries IN THIS-PROCEDURE.
    {&OPEN-QUERY-brSource}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAddTarget
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAddTarget wFormation
ON CHOOSE OF btAddTarget IN FRAME fPage1
DO:
    
    DEFINE VARIABLE contAdd         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cCodDel         AS CHAR      NO-UNDO.

    ASSIGN l-del-reg-0111 = NO.
    
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

                {formation/updatetarget.i &ProgramTarget="mvp/esmv0111a.w"}

                WAIT-FOR "CLOSE":U OF hTargetProgram.

                IF l-del-reg-0111 = YES THEN DO:

                    SESSION:SET-WAIT-STATE("GENERAL":U).

                    FOR FIRST {&ttTarget} WHERE
                        {&ttTarget}.cod-especialid = cCodDel EXCLUSIVE-LOCK:

                            RUN goToKey IN {&hDBOTarget} (INPUT {&ttTarget}.ep-codigo, 
                                                          INPUT {&ttTarget}.cod-estabel,
                                                          INPUT {&ttTarget}.cod-matr,
                                                          INPUT {&ttTarget}.cod-especialid).
                                     
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
    {formation/deltarget.i}   
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


&Scoped-define SELF-NAME btSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSearch wFormation
ON CHOOSE OF btSearch IN FRAME fPage0 /* Search */
OR CHOOSE OF MENU-ITEM miSearch IN MENU mbMain DO:
    {method/zoomreposition.i &ProgramZoom="frzoom/z01fr032.w"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btUpdateTarget
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdateTarget wFormation
ON CHOOSE OF btUpdateTarget IN FRAME fPage1 /* Alterar */
DO:
    ASSIGN l-del-reg-0111 = YES.

    {formation/updatetarget.i &ProgramTarget="mvp/esmv0111a.w"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage0
&Scoped-define SELF-NAME tt-mmv-func-ofici.cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-func-ofici.cod-estabel wFormation
ON LEAVE OF tt-mmv-func-ofici.cod-estabel IN FRAME fPage0 /* Estabelecimento */
DO:
    for first estabelec
        where estabelec.cod-estabel = tt-mmv-func-ofici.cod-estabel:screen-value in frame fPage0 no-lock:
        assign c-desc-estab = estabelec.nome.
    end.
    display c-desc-estab with frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-mmv-func-ofici.ep-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mmv-func-ofici.ep-codigo wFormation
ON LEAVE OF tt-mmv-func-ofici.ep-codigo IN FRAME fPage0 /* Empresa */
DO:
    for first empresa no-lock
        where empresa.ep-codigo = tt-mmv-func-ofici.ep-codigo:screen-value in frame fPage0:
        assign c-desc-emp = empresa.nome.
    end.
    display c-desc-emp with frame fPage0.
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
    apply "LEAVE":U to tt-mmv-func-ofici.ep-codigo   in frame fPage0.
    apply "LEAVE":U to tt-mmv-func-ofici.cod-estabel in frame fPage0.

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
    {utp/ut-liter.i "Especialidade"}
    ASSIGN  cEspecialidade:LABEL      IN FRAME fPage1 = RETURN-VALUE
            cEspecialidade:SENSITIVE  IN FRAME fPage1 = YES
            bt-Confirma   :SENSITIVE  IN FRAME fPage1 = YES
            btAddTarget   :SENSITIVE  IN FRAME fPage1 = YES
            btUpdateTarget:SENSITIVE  IN FRAME fPage1 = YES.

    /** label Especialidade **/
    assign tt-mmv-especialid-func.cod-especialid:label       in browse brSource = return-value
           tt-mmv-func-relac-especialid.cod-especialid:label in browse brTarget = return-value.

    {utp/ut-liter.i "Descri‡Æo"}
    assign tt-mmv-especialid-func.descricao:label in browse brSource = return-value
           cDescricao:label = return-value.

    {utp/ut-liter.i "Atividade"}
    assign cAtividade:label = return-value.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goToRecord wFormation 
PROCEDURE goToRecord :
/*:T------------------------------------------------------------------------------
  Purpose:     Exibe dialog de V  Para
  Parameters:  
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
    
    DEFINE VARIABLE iEmpFunc  like {&ttParent}.ep-codigo 
           VIEW-AS FILL-IN SIZE 5.00 BY 0.88 NO-UNDO.
   DEFINE VARIABLE cCodEstab  like {&ttParent}.cod-estabel
           VIEW-AS FILL-IN SIZE 5.00 BY 0.88 NO-UNDO.
    DEFINE VARIABLE cCodFunc  LIKE {&ttParent}.cod-matr
           VIEW-AS FILL-IN SIZE 14.00 BY 0.88 NO-UNDO.

    DEFINE FRAME fGoToRecord
        iEmpFunc          AT ROW 1.21 COL 17.72 COLON-ALIGNED
        cCodEstab         AT ROW 2.21 COL 17.72 COLON-ALIGNED
        cCodFunc          AT ROW 3.21 COL 17.72 COLON-ALIGNED
        btGoToOK          AT ROW 4.63 COL 2.14
        btGoToCancel      AT ROW 4.63 COL 13
        rtGoToButton      AT ROW 4.38 COL 1
        SPACE(0.28)
        WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
             THREE-D SCROLLABLE TITLE "V  Para o Funcion rio" FONT 1
             DEFAULT-BUTTON btGoToOK CANCEL-BUTTON btGoToCancel.

    RUN utp/ut-trfrrp.p (input Frame fGoToRecord:Handle).
    {utp/ut-liter.i "V _Para_o_Funcion rio"}
    ASSIGN FRAME fGoToRecord:TITLE = RETURN-VALUE.

    ON "CHOOSE":U OF btGoToOK IN FRAME fGoToRecord DO:
        ASSIGN cCodFunc cCodEstab iEmpFunc.

        /*:T Posiciona query, do DBO, atrav²s dos valores do ­ndice œnico */
        RUN goToKey IN {&hDBOParent} (INPUT iEmpFunc,INPUT cCodEstab, input cCodFunc ).
        IF RETURN-VALUE = "NOK":U THEN DO:
            RUN utp/ut-msgs.p (INPUT "SHOW":U, INPUT 2, INPUT "mmv-func-ofici":U).

            RETURN NO-APPLY.
        END.

        /*:T Retorna rowid do registro corrente do DBO */
        RUN getRowid IN {&hDBOParent} (OUTPUT rGoTo).

        /*:T Reposiciona registro com base em um rowid */
        RUN repositionRecord IN THIS-PROCEDURE (INPUT rGoTo).

        APPLY "GO":U TO FRAME fGoToRecord.
    END.

    ENABLE cCodFunc cCodEstab iEmpFunc btGoToOK btGoToCancel 
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
        {&hDBOParent}:FILE-NAME <> "frbo/bofr032.p":U THEN DO:
        RUN frbo/bofr032.p PERSISTENT SET {&hDBOParent}.
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
        {&hDBOTarget}:FILE-NAME <> "yambo/ydm005.p":U THEN DO:
        RUN yambo/ydm005.p PERSISTENT SET {&hDBOTarget}.
    END.
    RUN openQueryStatic IN {&hDBOTarget} (INPUT "Main":U) NO-ERROR.

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

    {formation/openqueriestarget.i &Parent="Especialid"
                                   &Query="Especialid"}    
                                   
    {formation/openqueriessource.i &Query="TpEspecial"
                                   &OpenAlways="yes"}                                    
                                   
    assign rRepositionTarget = ?.
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveParentFields wFormation 
PROCEDURE saveParentFields :
/*:T------------------------------------------------------------------------------
  Purpose:     Salva valores dos campos da tabela forma‡Æo ({&ttTarget}) com base 
               nos campos da tabela pai ({&ttParent}) e tabela origem ({&ttSource})
  Parameters:  
------------------------------------------------------------------------------*/
    assign {&ttTarget}.ep-codigo      = {&ttParent}.ep-codigo  :SCREEN-VALUE IN FRAME fPage0
           {&ttTarget}.cod-estabel    = {&ttParent}.cod-estabel:SCREEN-VALUE IN FRAME fPage0
           {&ttTarget}.cod-matr       = {&ttParent}.cod-matr   :SCREEN-VALUE IN FRAME fPage0   
           {&ttTarget}.cod-especialid = {&ttSource}.cod-especialid
           {&ttTarget}.num-livre-1    = 1.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnAtividade wFormation 
FUNCTION fnAtividade RETURNS CHARACTER
  ( pAtividade as int /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    RETURN {ininc/i01in097.i 4 pAtividade}.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnDescricao wFormation 
FUNCTION fnDescricao RETURNS CHARACTER
  ( pEspec as char /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    for first mmv-especialid-func
        where mmv-especialid-func.cod-especialid = pEspec no-lock:
    end.
    if avail mmv-especialid-func then
        return mmv-especialid-func.descricao.
    else
        return "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


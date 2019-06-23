&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME wFormation


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-equipe NO-UNDO LIKE equipe
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-mmi-tecnico-tarefa-om NO-UNDO LIKE mmi-tecnico-tarefa-om
       FIELD r-rowid as rowid.
DEFINE TEMP-TABLE tt-ord-taref NO-UNDO LIKE ord-taref
       FIELD r-rowid as rowid.
DEFINE TEMP-TABLE tt-tecn-mi NO-UNDO LIKE tecn-mi
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
{include/i-prgvrs.i ESMI0307 2.06.00.000}  /*** 010001 ***/
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
&GLOBAL-DEFINE Program        ESMI0307
&GLOBAL-DEFINE Version        2.06.00.000

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

&GLOBAL-DEFINE ttParent       tt-ord-taref
&GLOBAL-DEFINE hDBOParent     hDBOord-taref
&GLOBAL-DEFINE DBOParentTable tt-ord-taref

&GLOBAL-DEFINE ttSource       tt-tecn-mi
&GLOBAL-DEFINE hDBOSource     hDBOtecn-mi
&GLOBAL-DEFINE DBOSourceTable tecn-mi

&GLOBAL-DEFINE ttTarget       tt-mmi-tecnico-tarefa-om
&GLOBAL-DEFINE hDBOTarget     hDBOmmi-tecnico-tarefa-om
&GLOBAL-DEFINE DBOTargetTable mmi-tecnico-tarefa-om

&GLOBAL-DEFINE page0Fields    tt-ord-taref.nr-ord-produ ~
                              tt-ord-taref.cd-tarefa
                              
&GLOBAL-DEFINE SourceBrowse   brSource
&GLOBAL-DEFINE TargetBrowse   brTarget

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE iRowid      AS INTEGER        NO-UNDO.
DEFINE VARIABLE lprimeira   AS LOGICAL INIT YES NO-UNDO.
DEFINE VARIABLE rowSelected AS ROWID          NO-UNDO.

/* Local Variable Definitions (DBOs Handles) ---                        */
DEFINE VARIABLE {&hDBOParent} AS HANDLE NO-UNDO.
DEFINE VARIABLE {&hDBOSource} AS HANDLE NO-UNDO.
DEFINE VARIABLE {&hDBOTarget} AS HANDLE NO-UNDO.
DEFINE VARIABLE hDBOEquipe    AS HANDLE NO-UNDO.

define variable iCounter as integer no-undo.

DEFINE TEMP-TABLE tt-selected-rows NO-UNDO
        FIELD r-row-id        AS ROWID.

DEFINE VARIABLE cDescEquipe     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pEquipto LIKE equipto.cd-equipto  NO-UNDO.

DEFINE NEW SHARED VARIABLE i-cancel-formation AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE cCodAdd            AS CHARACTER NO-UNDO.

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
&Scoped-define INTERNAL-TABLES tt-tecn-mi tt-mmi-tecnico-tarefa-om

/* Definitions for BROWSE brSource                                      */
&Scoped-define FIELDS-IN-QUERY-brSource tt-tecn-mi.cd-tecnico tt-tecn-mi.nome-compl tt-tecn-mi.cd-equipe des-equipe(tt-tecn-mi.cd-equipe) @ cDescEquipe   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brSource   
&Scoped-define SELF-NAME brSource
&Scoped-define QUERY-STRING-brSource FOR EACH tt-tecn-mi NO-LOCK
&Scoped-define OPEN-QUERY-brSource OPEN QUERY {&SELF-NAME} FOR EACH tt-tecn-mi NO-LOCK.
&Scoped-define TABLES-IN-QUERY-brSource tt-tecn-mi
&Scoped-define FIRST-TABLE-IN-QUERY-brSource tt-tecn-mi


/* Definitions for BROWSE brTarget                                      */
&Scoped-define FIELDS-IN-QUERY-brTarget tt-mmi-tecnico-tarefa-om.cd-tecnico tt-tecn-mi.nome-compl tt-mmi-tecnico-tarefa-om.dt-prevista tt-tecn-mi.cd-equipe des-nome-equipe(tt-tecn-mi.cd-equipe) @ cDescEquipe   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brTarget   
&Scoped-define SELF-NAME brTarget
&Scoped-define QUERY-STRING-brTarget FOR EACH tt-mmi-tecnico-tarefa-om NO-LOCK, ~
             EACH tt-tecn-mi OF tt-mmi-tecnico-tarefa-om NO-LOCK
&Scoped-define OPEN-QUERY-brTarget OPEN QUERY {&SELF-NAME} FOR EACH tt-mmi-tecnico-tarefa-om NO-LOCK, ~
             EACH tt-tecn-mi OF tt-mmi-tecnico-tarefa-om NO-LOCK.
&Scoped-define TABLES-IN-QUERY-brTarget tt-mmi-tecnico-tarefa-om tt-tecn-mi
&Scoped-define FIRST-TABLE-IN-QUERY-brTarget tt-mmi-tecnico-tarefa-om
&Scoped-define SECOND-TABLE-IN-QUERY-brTarget tt-tecn-mi


/* Definitions for FRAME fPage1                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fPage1 ~
    ~{&OPEN-QUERY-brSource}~
    ~{&OPEN-QUERY-brTarget}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-ord-taref.nr-ord-produ ~
tt-ord-taref.cd-tarefa 
&Scoped-define ENABLED-TABLES tt-ord-taref
&Scoped-define FIRST-ENABLED-TABLE tt-ord-taref
&Scoped-Define ENABLED-OBJECTS rtToolBar btFirst btPrev btNext btLast ~
btGoTo btQueryJoins btReportsJoins btExit btHelp fi-des-ordem fi-des-taref ~
fi-cod-equipto fi-nom-equipto 
&Scoped-Define DISPLAYED-FIELDS tt-ord-taref.nr-ord-produ ~
tt-ord-taref.cd-tarefa 
&Scoped-define DISPLAYED-TABLES tt-ord-taref
&Scoped-define FIRST-DISPLAYED-TABLE tt-ord-taref
&Scoped-Define DISPLAYED-OBJECTS fi-des-ordem fi-des-taref fi-cod-equipto ~
fi-nom-equipto 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD des-Equipe wFormation 
FUNCTION des-Equipe RETURNS CHARACTER
  ( pDescricao AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD des-nome-equipe wFormation 
FUNCTION des-nome-equipe RETURNS CHARACTER
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

DEFINE VARIABLE fi-cod-equipto AS CHARACTER FORMAT "X(256)":U 
     LABEL "Equipamento" 
     VIEW-AS FILL-IN 
     SIZE 14.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-des-ordem AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 44 BY .88 NO-UNDO.

DEFINE VARIABLE fi-des-taref AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 46 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nom-equipto AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39 BY .88 NO-UNDO.

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
      tt-tecn-mi SCROLLING.

DEFINE QUERY brTarget FOR 
      tt-mmi-tecnico-tarefa-om, 
      tt-tecn-mi SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brSource
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brSource wFormation _FREEFORM
  QUERY brSource NO-LOCK DISPLAY
      tt-tecn-mi.cd-tecnico FORMAT "99999-9":U
      tt-tecn-mi.nome-compl FORMAT "x(30)":U
      tt-tecn-mi.cd-equipe FORMAT "x(8)":U
      des-equipe(tt-tecn-mi.cd-equipe) @ cDescEquipe FORMAT "x(30)":U LABEL "Descri‡Æo":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 32 BY 8
         FONT 1.

DEFINE BROWSE brTarget
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brTarget wFormation _FREEFORM
  QUERY brTarget NO-LOCK DISPLAY
      tt-mmi-tecnico-tarefa-om.cd-tecnico FORMAT "99999-9":U
      tt-tecn-mi.nome-compl FORMAT "x(30)":U
      tt-mmi-tecnico-tarefa-om.dt-prevista FORMAT "99/99/9999":U
      tt-tecn-mi.cd-equipe FORMAT "x(8)":U
      des-nome-equipe(tt-tecn-mi.cd-equipe) @ cDescEquipe FORMAT "x(30)":U LABEL "Descri‡Æo":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 32 BY 8
         FONT 1.


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
     tt-ord-taref.nr-ord-produ AT ROW 3 COL 15.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     fi-des-ordem AT ROW 3 COL 25.43 COLON-ALIGNED NO-LABEL
     tt-ord-taref.cd-tarefa AT ROW 4 COL 15.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.57 BY .88
     fi-des-taref AT ROW 4 COL 23 COLON-ALIGNED NO-LABEL
     fi-cod-equipto AT ROW 5 COL 15.43 COLON-ALIGNED
     fi-nom-equipto AT ROW 5 COL 30 COLON-ALIGNED NO-LABEL
     rtToolBar AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17.04
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
      TABLE: tt-equipe T "?" NO-UNDO mgmnt equipe
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-mmi-tecnico-tarefa-om T "?" NO-UNDO mgesp mmi-tecnico-tarefa-om
      ADDITIONAL-FIELDS:
          FIELD r-rowid as rowid
      END-FIELDS.
      TABLE: tt-ord-taref T "?" NO-UNDO movmnt ord-taref
      ADDITIONAL-FIELDS:
          FIELD r-rowid as rowid
      END-FIELDS.
      TABLE: tt-tecn-mi T "?" NO-UNDO mgmnt tecn-mi
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
         HEIGHT             = 17.04
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
ASSIGN 
       brSource:COLUMN-RESIZABLE IN FRAME fPage1       = TRUE
       brSource:COLUMN-MOVABLE IN FRAME fPage1         = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wFormation)
THEN wFormation:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brSource
/* Query rebuild information for BROWSE brSource
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-tecn-mi NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE brSource */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brTarget
/* Query rebuild information for BROWSE brTarget
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-mmi-tecnico-tarefa-om NO-LOCK,
      EACH tt-tecn-mi OF tt-mmi-tecnico-tarefa-om NO-LOCK.
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

                ASSIGN cCodDel = {&ttSource}.cd-tecnico.

                {Formation/AddTarget.i}

                {formation/updatetarget.i &ProgramTarget="mip/esmi0307a.w"}

                WAIT-FOR "CLOSE":U OF hTargetProgram.

                IF i-cancel-formation = YES THEN DO:

                    SESSION:SET-WAIT-STATE("GENERAL":U).

                    FOR FIRST {&ttTarget} WHERE
                        {&ttTarget}.cd-tecnico = cCodDel EXCLUSIVE-LOCK:

                            RUN goToKey IN {&hDBOTarget} (INPUT {&ttTarget}.nr-ord-produ,
                                                          INPUT {&ttTarget}.num-seq,
                                                          INPUT {&ttTarget}.cd-tecnico).

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

    brTarget:REFRESHABLE = NO.

    DO contDel = brTarget:NUM-SELECTED-ROWS  TO 1 BY -1 :
/*     DO cont = brTarget:NUM-SELECTED-ROWS  TO 1 BY -1 : */

       IF brTarget:FETCH-SELECTED-ROW(contDel) THEN DO:
           {Formation/DelTarget.i}
       END.
    END.

    EMPTY TEMP-TABLE tt-selected-rows.
    
    brTarget:REFRESHABLE = YES.

    run openQueries.

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

    DEFINE VARIABLE laranja AS INTEGER NO-UNDO.

    brtarget:REFRESHABLE = NO.

    DO laranja = brtarget:NUM-SELECTED-ROWS  TO 1 BY -1 :

        IF brtarget:FETCH-SELECTED-ROW(laranja) THEN DO:

            {formation/updatetarget.i &ProgramTarget="mip/esmi0307a.w"}

        END.
    END.

   brtarget:REFRESHABLE = YES.
   assign rRepositionTarget = ?.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage0
&Scoped-define SELF-NAME tt-ord-taref.cd-tarefa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-ord-taref.cd-tarefa wFormation
ON LEAVE OF tt-ord-taref.cd-tarefa IN FRAME fPage0 /* Tarefa Manuten‡Æo */
DO:

{Include/leave.i
               &tabela=ord-taref
               &atributo-ref=descricao
               &variavel-ref=fi-des-taref
               &where="ord-taref.nr-ord-produ = input frame fPage0 tt-ord-taref.nr-ord-produ"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-equipto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-equipto wFormation
ON LEAVE OF fi-cod-equipto IN FRAME fPage0 /* Equipamento */
DO:
    FOR FIRST equipto
        WHERE equipto.cd-equipto = fi-cod-equipto:SCREEN-VALUE IN FRAME fPage0 NO-LOCK:
        ASSIGN fi-nom-equipto:SCREEN-VALUE IN FRAME fPage0 = equipto.descricao.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-ord-taref.nr-ord-produ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-ord-taref.nr-ord-produ wFormation
ON LEAVE OF tt-ord-taref.nr-ord-produ IN FRAME fPage0 /* Ordem */
DO:
  {Include/leave.i 
               &tabela=ord-manut
               &atributo-ref=des-man-corr
               &variavel-ref=fi-des-ordem
               &where="ord-manut.nr-ord-produ = input frame fPage0 tt-ord-taref.nr-ord-produ"}
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
    APPLY "LEAVE":U TO tt-ord-taref.nr-ord-produ IN FRAME fPage0.
    APPLY "LEAVE":U TO tt-ord-taref.cd-tarefa    IN FRAME fPage0.
    APPLY "LEAVE":U TO fi-cod-equipto            IN FRAME fPage0.
    
    ASSIGN btAddTarget   :SENSITIVE   IN FRAME fPage1 = YES.

    FOR FIRST equipto
        WHERE equipto.cd-equipto = ord-manut.cd-equipto 
        AND   ord-manut.nr-ord-produ = ord-taref.nr-ord-produ NO-LOCK:

        ASSIGN fi-cod-equipto = equipto.cd-equipto.
        
        DISP fi-cod-equipto WITH FRAME fPage0.
    END.
    ASSIGN btAddTarget   :SENSITIVE   IN FRAME fPage1 = YES
           btGoTo        :SENSITIVE   IN FRAME fPage0 = YES.

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

    APPLY "LEAVE":U TO fi-cod-equipto        IN FRAME fPage0.
    ASSIGN btUpdateTarget:SENSITIVE   IN FRAME fPage1 = YES
           btAddTarget   :SENSITIVE   IN FRAME fPage1 = YES
           btDelTarget   :SENSITIVE   IN FRAME fPage1 = YES.

    FOR FIRST equipto
        WHERE equipto.cd-equipto = ord-manut.cd-equipto 
        AND   ord-manut.nr-ord-produ = ord-taref.nr-ord-produ NO-LOCK:

        ASSIGN fi-cod-equipto = equipto.cd-equipto.
        DISP   fi-cod-equipto WITH FRAME fPage0.

    END.
    RUN OpenQueries.
    ASSIGN btAddTarget   :SENSITIVE   IN FRAME fPage1 = YES
           btGoTo        :SENSITIVE   IN FRAME fPage0 = YES.

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
    DEFINE VARIABLE ccod-eqpto     LIKE {&ttParent}.cd-tarefa     NO-UNDO.
    
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
       {&hDBOParent}:FILE-NAME <> "mnbo/bomn136.p":U THEN DO:
        RUN mnbo/bomn136.p PERSISTENT SET {&hDBOParent}.
    END.
     RUN openQueryStatic IN {&hDBOParent} (INPUT "Main":U) NO-ERROR.
    
    /*:T--- Verifica se o DBO da tabela Origem j  est  inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOSource}) OR
       {&hDBOSource}:TYPE <> "PROCEDURE":U OR
       {&hDBOSource}:FILE-NAME <> "mnbo/bomn157.p":U THEN DO:
        RUN mnbo/bomn157.p PERSISTENT SET {&hDBOSource}.
    END.
     RUN openQueryStatic IN {&hDBOSource} (INPUT "Main":U) NO-ERROR.

    /*:T--- Verifica se o DBO da tabela Forma‡Æo j  est  inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOTarget}) OR
       {&hDBOTarget}:TYPE <> "PROCEDURE":U OR
       {&hDBOTarget}:FILE-NAME <> "yambo/ydm003.p":U THEN DO:
        RUN yambo/ydm003.p PERSISTENT SET {&hDBOTarget}.
    END.
     RUN openQueryStatic IN {&hDBOTarget} (INPUT "Main":U) NO-ERROR.

    /*--------------------- DBO Equipe ----------------------------*/
    IF NOT VALID-HANDLE(hDBOEquipe) OR
       hDBOEquipe:TYPE <> "PROCEDURE":U OR
       hDBOEquipe:FILE-NAME <> "mnbo/mnbo087.p":U THEN DO:
        run mnbo/bomn087.p persistent set  hDBOEquipe.
    END.

/*     RUN openQueryStatic IN hDBOEquipe (INPUT "Default":U) NO-ERROR.  */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piAddTarget wFormation 
PROCEDURE piAddTarget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*     DEFINE INPUT PARAMETER pCdTecnico   LIKE ord-taref.cd-tecnico    NO-UNDO.  */
/*     DEFINE INPUT PARAMETER pNrOrdProdu  LIKE ord-taref.nr-ord-produ  NO-UNDO.  */
/*     DEFINE INPUT PARAMETER pNumSeq      LIKE ord-taref.sequencia     NO-UNDO.  */
/*                                                                                */
/*     FIND FIRST ord-taref where                                                 */
/*                ord-taref.nr-ord-produ = pNrOrdProdu NO-LOCK NO-ERROR.          */
/*                                                                                */
/*     CREATE tt-mmi-tecnico-tarefa-om.                                           */
/*     ASSIGN tt-mmi-tecnico-tarefa-om.cd-tecnico     = pCdTecnico                */
/*            tt-mmi-tecnico-tarefa-om.nr-ord-produ   = pNrOrdProdu               */
/*            tt-mmi-tecnico-tarefa-om.num-seq        = pNumSeq.                  */
/*                                                                                */
/*     RETURN "OK":U.                                                             */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piRemoveTarget wFormation 
PROCEDURE piRemoveTarget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*     DEFINE INPUT PARAMETER pCdTecnico   LIKE ord-taref.cd-tecnico    NO-UNDO.   */
/*     DEFINE INPUT PARAMETER pNrOrdProdu  LIKE ord-taref.nr-ord-produ  NO-UNDO.   */
/*     DEFINE INPUT PARAMETER pNumSeq      LIKE ord-taref.sequencia     NO-UNDO.   */
/*                                                                                 */
/*     FIND FIRST mmi-tecnico-tarefa-om WHERE                                      */
/*                mmi-tecnico-tarefa-om.cd-tecnico = pCdTecnico NO-LOCK NO-ERROR.  */
/*         CREATE tt-mmi-tecnico-tarefa-om.                                        */
/*         ASSIGN tt-mmi-tecnico-tarefa-om.cd-tecnico     = pCdTecnico             */
/*                tt-mmi-tecnico-tarefa-om.nr-ord-produ   = pNrOrdProdu            */
/*                tt-mmi-tecnico-tarefa-om.num-seq        = pNumSeq.               */
/*                                                                                 */
/*                                                                                 */
/*     RETURN "OK":U.                                                              */
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
ASSIGN {&ttTarget}.nr-ord-produ = {&ttParent}.nr-ord-produ
       {&ttTarget}.cd-tecnico   = {&ttSource}.cd-tecnico
       {&ttTarget}.num-seq      = {&ttParent}.cd-tarefa.
       
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

RUN goToKey IN hDBOord-taref (INPUT i-ordem,
                              INPUT i-tarefa).

RUN getRowid IN hDBOord-taref (OUTPUT r-rowid).

RUN repositionRecord IN THIS-PROCEDURE (INPUT r-rowid).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION des-Equipe wFormation 
FUNCTION des-Equipe RETURNS CHARACTER
  ( pDescricao AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE cRetorno AS CHARACTER  NO-UNDO.

FOR FIRST equipe
    WHERE equipe.cd-equipe = pDescricao NO-LOCK:
    ASSIGN cRetorno = equipe.desc-equipe.
END.

    RETURN cRetorno.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION des-nome-equipe wFormation 
FUNCTION des-nome-equipe RETURNS CHARACTER
  ( pDescricao AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE cRetorno AS CHARACTER  NO-UNDO.

FOR FIRST equipe
    WHERE equipe.cd-equipe = pDescricao NO-LOCK:
    ASSIGN cRetorno = equipe.desc-equipe.
END.

    RETURN cRetorno.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


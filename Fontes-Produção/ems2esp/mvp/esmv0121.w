&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME wMasterDetail


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-mmv-cap-esp-turno NO-UNDO LIKE mmv-cap-esp-turno
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-mmv-especialid-func NO-UNDO LIKE mmv-especialid-func
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-mmv-exc-esp-data NO-UNDO LIKE mmv-exc-esp-data
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-mmv-turno-esp NO-UNDO LIKE mmv-turno-esp
       field r-rowid as rowid.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wMasterDetail 
/********************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESMV0121 2.06.00.000}

{cdp/cdcfgmnt.i}

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program          ESMV0121
&GLOBAL-DEFINE Version          2.06.00.000

&GLOBAL-DEFINE Folder           YES
&GLOBAL-DEFINE InitialPage      1
&GLOBAL-DEFINE FolderLabels     Turno,Exce‡Æo,Capacidade

&GLOBAL-DEFINE First            YES
&GLOBAL-DEFINE Prev             YES
&GLOBAL-DEFINE Next             YES
&GLOBAL-DEFINE Last             YES
&GLOBAL-DEFINE GoTo             YES
&GLOBAL-DEFINE Search           YES

&GLOBAL-DEFINE AddSon1          YES
&GLOBAL-DEFINE DeleteSon1       YES
&global-define VALUE-CHANGED1   YES

&GLOBAL-DEFINE AddSon2          YES
&GLOBAL-DEFINE UpdateSon2       YES
&GLOBAL-DEFINE DeleteSon2       YES

&GLOBAL-DEFINE AddSon3          YES
&GLOBAL-DEFINE UpdateSon3       YES
&GLOBAL-DEFINE DeleteSon3       YES

&GLOBAL-DEFINE ttParent         tt-mmv-especialid-func
&GLOBAL-DEFINE hDBOParent       hDBOEspFunc               
&GLOBAL-DEFINE DBOParentTable   mmv-especialid-func 
&GLOBAL-DEFINE DBOParentDestroy YES

&GLOBAL-DEFINE ttSon1           tt-mmv-turno-esp
&GLOBAL-DEFINE hDBOSon1         hDBOTurnoEsp
&GLOBAL-DEFINE DBOSon1Table     mmv-turno-esp
&GLOBAL-DEFINE DBOSon1Destroy   YES

&GLOBAL-DEFINE ttSon2           tt-mmv-exc-esp-data
&GLOBAL-DEFINE hDBOSon2         hDBOExcEsp
&GLOBAL-DEFINE DBOSon2Table     mmv-exc-esp-data
&GLOBAL-DEFINE DBOSon2Destroy   YES

&GLOBAL-DEFINE ttSon3           tt-mmv-cap-esp-turno
&GLOBAL-DEFINE hDBOSon3         hDBOCapEspec
&GLOBAL-DEFINE DBOSon3Table     mmv-cap-esp-turno
&GLOBAL-DEFINE DBOSon3Destroy   YES

&GLOBAL-DEFINE page0Fields      tt-mmv-especialid-func.cod-especialid ~
                                tt-mmv-especialid-func.descricao
&GLOBAL-DEFINE page1Browse      brSon1
&GLOBAL-DEFINE page2Browse      brSon2
&GLOBAL-DEFINE page3Browse      brSon3

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cDescTurno     LIKE mi-turno.descricao.
DEFINE VARIABLE iTurnoIni2     LIKE mi-turno.cd-turno INIT 0. 
DEFINE VARIABLE iTurnoFim2     LIKE mi-turno.cd-turno INIT 999. 
DEFINE VARIABLE iTurnoIni3     LIKE mi-turno.cd-turno INIT 0. 
DEFINE VARIABLE iTurnoFim3     LIKE mi-turno.cd-turno INIT 999. 
DEFINE VARIABLE cListaTipoHora AS CHAR NO-UNDO.
DEFINE VARIABLE cTipoHora      AS CHAR NO-UNDO.
DEFINE VARIABLE lOk            AS LOG  NO-UNDO.
DEFINE VARIABLE cDescCalen     AS CHAR NO-UNDO.

/* Local Variable Definitions (DBOs Handles) ---                        */
DEFINE VARIABLE {&hDBOParent} AS HANDLE NO-UNDO.
DEFINE VARIABLE {&hDBOSon1}   AS HANDLE NO-UNDO.
DEFINE VARIABLE {&hDBOSon2}   AS HANDLE NO-UNDO.
DEFINE VARIABLE {&hDBOSon3}   AS HANDLE NO-UNDO.

DEFINE VARIABLE hDBOMiTurno   AS HANDLE NO-UNDO.


/** Variavel utilizado na BO turno, openQueryCdTurno **/
DEFINE NEW GLOBAL SHARED VARIABLE gTpEspecial-esmv0121 AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE MasterDetail
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fPage0
&Scoped-define BROWSE-NAME brSon1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-mmv-turno-esp tt-mmv-exc-esp-data ~
tt-mmv-cap-esp-turno

/* Definitions for BROWSE brSon1                                        */
&Scoped-define FIELDS-IN-QUERY-brSon1 tt-mmv-turno-esp.cod-turno ~
tt-mmv-turno-esp.desc-turno tt-mmv-turno-esp.cd-calen ~
fnDescCalen(tt-mmv-turno-esp.cd-calen) @ cDescCalen 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brSon1 
&Scoped-define QUERY-STRING-brSon1 FOR EACH tt-mmv-turno-esp NO-LOCK
&Scoped-define OPEN-QUERY-brSon1 OPEN QUERY brSon1 FOR EACH tt-mmv-turno-esp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-brSon1 tt-mmv-turno-esp
&Scoped-define FIRST-TABLE-IN-QUERY-brSon1 tt-mmv-turno-esp


/* Definitions for BROWSE brSon2                                        */
&Scoped-define FIELDS-IN-QUERY-brSon2 tt-mmv-exc-esp-data.cod-turno ~
tt-mmv-exc-esp-data.data tt-mmv-exc-esp-data.hora-inicial ~
tt-mmv-exc-esp-data.hora-termino tt-mmv-exc-esp-data.obs 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brSon2 
&Scoped-define QUERY-STRING-brSon2 FOR EACH tt-mmv-exc-esp-data NO-LOCK
&Scoped-define OPEN-QUERY-brSon2 OPEN QUERY brSon2 FOR EACH tt-mmv-exc-esp-data NO-LOCK.
&Scoped-define TABLES-IN-QUERY-brSon2 tt-mmv-exc-esp-data
&Scoped-define FIRST-TABLE-IN-QUERY-brSon2 tt-mmv-exc-esp-data


/* Definitions for BROWSE brSon3                                        */
&Scoped-define FIELDS-IN-QUERY-brSon3 tt-mmv-cap-esp-turno.cod-turno ~
tt-mmv-cap-esp-turno.dt-efetivacao tt-mmv-cap-esp-turno.nr-tecnico 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brSon3 
&Scoped-define QUERY-STRING-brSon3 FOR EACH tt-mmv-cap-esp-turno NO-LOCK ~
    BY tt-mmv-cap-esp-turno.dt-efetivacao
&Scoped-define OPEN-QUERY-brSon3 OPEN QUERY brSon3 FOR EACH tt-mmv-cap-esp-turno NO-LOCK ~
    BY tt-mmv-cap-esp-turno.dt-efetivacao.
&Scoped-define TABLES-IN-QUERY-brSon3 tt-mmv-cap-esp-turno
&Scoped-define FIRST-TABLE-IN-QUERY-brSon3 tt-mmv-cap-esp-turno


/* Definitions for FRAME fPage1                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fPage1 ~
    ~{&OPEN-QUERY-brSon1}

/* Definitions for FRAME fPage2                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fPage2 ~
    ~{&OPEN-QUERY-brSon2}

/* Definitions for FRAME fPage3                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fPage3 ~
    ~{&OPEN-QUERY-brSon3}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-mmv-especialid-func.cod-especialid ~
tt-mmv-especialid-func.descricao 
&Scoped-define ENABLED-TABLES tt-mmv-especialid-func
&Scoped-define FIRST-ENABLED-TABLE tt-mmv-especialid-func
&Scoped-Define ENABLED-OBJECTS rtParent rtToolBar btFirst btPrev btNext ~
btLast btGoTo btSearch btQueryJoins btReportsJoins btExit btHelp 
&Scoped-Define DISPLAYED-FIELDS tt-mmv-especialid-func.cod-especialid ~
tt-mmv-especialid-func.descricao 
&Scoped-define DISPLAYED-TABLES tt-mmv-especialid-func
&Scoped-define FIRST-DISPLAYED-TABLE tt-mmv-especialid-func


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnDescCalen wMasterDetail 
FUNCTION fnDescCalen RETURNS CHARACTER
  (INPUT pCalen AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnDescTurno wMasterDetail 
FUNCTION fnDescTurno RETURNS CHARACTER
   (INPUT piCdTurno AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wMasterDetail AS WIDGET-HANDLE NO-UNDO.

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
       MENU-ITEM miAdd          LABEL "&Incluir"       ACCELERATOR "CTRL-INS"
       MENU-ITEM miCopy         LABEL "&Copiar"        ACCELERATOR "CTRL-C"
       MENU-ITEM miUpdate       LABEL "&Alterar"       ACCELERATOR "CTRL-A"
       MENU-ITEM miDelete       LABEL "&Eliminar"      ACCELERATOR "CTRL-DEL"
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

DEFINE RECTANGLE rtParent
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 2.25.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.5
     BGCOLOR 7 .

DEFINE BUTTON btAddSon1 
     LABEL "&Incluir" 
     SIZE 10 BY 1.

DEFINE BUTTON btDeleteSon1 
     LABEL "&Eliminar" 
     SIZE 10 BY 1.

DEFINE BUTTON btAddSon2 
     LABEL "&Incluir" 
     SIZE 10 BY 1.

DEFINE BUTTON btDeleteSon2 
     LABEL "&Eliminar" 
     SIZE 10 BY 1.

DEFINE BUTTON btUpdateSon2 
     LABEL "&Alterar" 
     SIZE 10 BY 1.

DEFINE BUTTON btAddSon3 
     LABEL "&Incluir" 
     SIZE 10 BY 1.

DEFINE BUTTON btDeleteSon3 
     LABEL "&Eliminar" 
     SIZE 10 BY 1.

DEFINE BUTTON btUpdateSon3 
     LABEL "&Alterar" 
     SIZE 10 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brSon1 FOR 
      tt-mmv-turno-esp SCROLLING.

DEFINE QUERY brSon2 FOR 
      tt-mmv-exc-esp-data SCROLLING.

DEFINE QUERY brSon3 FOR 
      tt-mmv-cap-esp-turno SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brSon1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brSon1 wMasterDetail _STRUCTURED
  QUERY brSon1 NO-LOCK DISPLAY
      tt-mmv-turno-esp.cod-turno FORMAT "->,>>>,>>9":U
      tt-mmv-turno-esp.desc-turno FORMAT "X(16)":U
      tt-mmv-turno-esp.cd-calen FORMAT "X(9)":U
      fnDescCalen(tt-mmv-turno-esp.cd-calen) @ cDescCalen COLUMN-LABEL "Descri‡Æo" FORMAT "X(30)":U
            WIDTH 30
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 82 BY 9.13
         FONT 2.

DEFINE BROWSE brSon2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brSon2 wMasterDetail _STRUCTURED
  QUERY brSon2 NO-LOCK DISPLAY
      tt-mmv-exc-esp-data.cod-turno FORMAT "->,>>>,>>9":U
      tt-mmv-exc-esp-data.data FORMAT "99/99/9999":U
      tt-mmv-exc-esp-data.hora-inicial FORMAT "X(8)":U
      tt-mmv-exc-esp-data.hora-termino FORMAT "X(8)":U
      tt-mmv-exc-esp-data.obs FORMAT "X(60)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 82 BY 9.13
         FONT 2.

DEFINE BROWSE brSon3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brSon3 wMasterDetail _STRUCTURED
  QUERY brSon3 NO-LOCK DISPLAY
      tt-mmv-cap-esp-turno.cod-turno FORMAT "->,>>>,>>9":U
      tt-mmv-cap-esp-turno.dt-efetivacao FORMAT "99/99/9999":U
      tt-mmv-cap-esp-turno.nr-tecnico FORMAT ">>,>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 82 BY 9.13
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
     tt-mmv-especialid-func.cod-especialid AT ROW 3.25 COL 29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.72 BY .88
     tt-mmv-especialid-func.descricao AT ROW 3.25 COL 39 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 29.72 BY .88
     rtParent AT ROW 2.67 COL 1
     rtToolBar AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17
         FONT 1.

DEFINE FRAME fPage1
     brSon1 AT ROW 1.17 COL 2
     btAddSon1 AT ROW 10.33 COL 2
     btDeleteSon1 AT ROW 10.33 COL 12
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.5 ROW 6.38
         SIZE 84.43 BY 10.63
         FONT 1.

DEFINE FRAME fPage3
     brSon3 AT ROW 1.21 COL 1.86
     btAddSon3 AT ROW 10.38 COL 1.86
     btUpdateSon3 AT ROW 10.38 COL 11.86
     btDeleteSon3 AT ROW 10.38 COL 21.86
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.71 ROW 6.29
         SIZE 84.43 BY 10.71
         FONT 1.

DEFINE FRAME fPage2
     brSon2 AT ROW 1.17 COL 2
     btAddSon2 AT ROW 10.33 COL 2
     btUpdateSon2 AT ROW 10.33 COL 12
     btDeleteSon2 AT ROW 10.33 COL 22
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.5 ROW 6.38
         SIZE 84.43 BY 10.63
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: MasterDetail
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-mmv-cap-esp-turno T "?" NO-UNDO mgesp mmv-cap-esp-turno
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-mmv-especialid-func T "?" NO-UNDO mgesp mmv-especialid-func
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-mmv-exc-esp-data T "?" NO-UNDO mgesp mmv-exc-esp-data
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-mmv-turno-esp T "?" NO-UNDO mgesp mmv-turno-esp
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wMasterDetail ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 17.13
         WIDTH              = 90
         MAX-HEIGHT         = 17.42
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 17.42
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
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU mbMain:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wMasterDetail 
/* ************************* Included-Libraries *********************** */

{masterdetail/masterdetail.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wMasterDetail
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME fPage1:FRAME = FRAME fPage0:HANDLE
       FRAME fPage2:FRAME = FRAME fPage0:HANDLE
       FRAME fPage3:FRAME = FRAME fPage0:HANDLE.

/* SETTINGS FOR FRAME fPage0
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME fPage1
                                                                        */
/* BROWSE-TAB brSon1 1 fPage1 */
/* SETTINGS FOR FRAME fPage2
                                                                        */
/* BROWSE-TAB brSon2 1 fPage2 */
/* SETTINGS FOR FRAME fPage3
                                                                        */
/* BROWSE-TAB brSon3 1 fPage3 */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wMasterDetail)
THEN wMasterDetail:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brSon1
/* Query rebuild information for BROWSE brSon1
     _TblList          = "Temp-Tables.tt-mmv-turno-esp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.tt-mmv-turno-esp.cod-turno
     _FldNameList[2]   = Temp-Tables.tt-mmv-turno-esp.desc-turno
     _FldNameList[3]   = Temp-Tables.tt-mmv-turno-esp.cd-calen
     _FldNameList[4]   > "_<CALC>"
"fnDescCalen(tt-mmv-turno-esp.cd-calen) @ cDescCalen" "Descri‡Æo" "X(30)" ? ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brSon1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brSon2
/* Query rebuild information for BROWSE brSon2
     _TblList          = "Temp-Tables.tt-mmv-exc-esp-data"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.tt-mmv-exc-esp-data.cod-turno
     _FldNameList[2]   = Temp-Tables.tt-mmv-exc-esp-data.data
     _FldNameList[3]   = Temp-Tables.tt-mmv-exc-esp-data.hora-inicial
     _FldNameList[4]   = Temp-Tables.tt-mmv-exc-esp-data.hora-termino
     _FldNameList[5]   = Temp-Tables.tt-mmv-exc-esp-data.obs
     _Query            is OPENED
*/  /* BROWSE brSon2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brSon3
/* Query rebuild information for BROWSE brSon3
     _TblList          = "Temp-Tables.tt-mmv-cap-esp-turno"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.tt-mmv-cap-esp-turno.dt-efetivacao|yes"
     _FldNameList[1]   = Temp-Tables.tt-mmv-cap-esp-turno.cod-turno
     _FldNameList[2]   = Temp-Tables.tt-mmv-cap-esp-turno.dt-efetivacao
     _FldNameList[3]   = Temp-Tables.tt-mmv-cap-esp-turno.nr-tecnico
     _Query            is OPENED
*/  /* BROWSE brSon3 */
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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage2
/* Query rebuild information for FRAME fPage2
     _Query            is NOT OPENED
*/  /* FRAME fPage2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wMasterDetail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wMasterDetail wMasterDetail
ON END-ERROR OF wMasterDetail
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wMasterDetail wMasterDetail
ON WINDOW-CLOSE OF wMasterDetail
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btAddSon1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAddSon1 wMasterDetail
ON CHOOSE OF btAddSon1 IN FRAME fPage1 /* Incluir */
DO:
    {masterdetail/AddSon.i &ProgramSon="mvp/esmv0121a.w"
                           &PageNumber="1"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage2
&Scoped-define SELF-NAME btAddSon2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAddSon2 wMasterDetail
ON CHOOSE OF btAddSon2 IN FRAME fPage2 /* Incluir */
DO:
   IF AVAIL tt-mmv-turno-esp THEN DO:
        SESSION:SET-WAIT-STATE("GENERAL":U).

        /*--- Executa programa de inclus’o de filho ---*/
        RUN mvp/esmv0121b.w PERSISTENT SET hSonProgram (INPUT ?,
                                                        INPUT tt-mmv-turno-esp.r-rowid,
                                                        INPUT "ADD":U,
                                                        INPUT THIS-PROCEDURE,
                                                        INPUT 2).

        IF VALID-HANDLE(hSonProgram) THEN
            /*--- Inicializa programa de inclus’o de filho ---*/
            RUN initializeInterface IN hSonProgram.

        /*--- Seta cursor do mouse para normal ---*/
        SESSION:SET-WAIT-STATE("":U).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage3
&Scoped-define SELF-NAME btAddSon3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAddSon3 wMasterDetail
ON CHOOSE OF btAddSon3 IN FRAME fPage3 /* Incluir */
DO:
   if avail tt-mmv-turno-esp then do:
        SESSION:SET-WAIT-STATE("GENERAL":U).

        /*--- Executa programa de inclus’o de filho ---*/
        RUN mvp/esmv0121c.w PERSISTENT SET hSonProgram (INPUT ?,
                                                        INPUT tt-mmv-turno-esp.r-rowid,
                                                        INPUT "ADD":U,
                                                        INPUT THIS-PROCEDURE,
                                                        INPUT 3).

        IF VALID-HANDLE(hSonProgram) THEN
            /*--- Inicializa programa de inclus’o de filho ---*/
            RUN initializeInterface IN hSonProgram.

        /*--- Seta cursor do mouse para normal ---*/
        SESSION:SET-WAIT-STATE("":U).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage1
&Scoped-define SELF-NAME btDeleteSon1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDeleteSon1 wMasterDetail
ON CHOOSE OF btDeleteSon1 IN FRAME fPage1 /* Eliminar */
DO:
    {masterdetail/DeleteSon.i &PageNumber="1"}

    IF AVAIL {&ttParent} THEN
        /*:T Reposiciona registro com base em um rowid */
        RUN repositionRecord IN THIS-PROCEDURE (INPUT {&ttParent}.r-rowid).
    ELSE
        RUN openQueriesSon IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage2
&Scoped-define SELF-NAME btDeleteSon2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDeleteSon2 wMasterDetail
ON CHOOSE OF btDeleteSon2 IN FRAME fPage2 /* Eliminar */
DO:
    {masterdetail/DeleteSon.i &PageNumber="2"}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage3
&Scoped-define SELF-NAME btDeleteSon3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDeleteSon3 wMasterDetail
ON CHOOSE OF btDeleteSon3 IN FRAME fPage3 /* Eliminar */
DO:
   {masterdetail/DeleteSon.i &PageNumber="3"}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage0
&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit wMasterDetail
ON CHOOSE OF btExit IN FRAME fPage0 /* Exit */
OR CHOOSE OF MENU-ITEM miExit IN MENU mbMain DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFirst wMasterDetail
ON CHOOSE OF btFirst IN FRAME fPage0 /* First */
OR CHOOSE OF MENU-ITEM miFirst IN MENU mbMain DO:
    RUN getFirst IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btGoTo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btGoTo wMasterDetail
ON CHOOSE OF btGoTo IN FRAME fPage0 /* Go To */
OR CHOOSE OF MENU-ITEM miGoTo IN MENU mbMain DO:
    RUN goToRecord IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btHelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp wMasterDetail
ON CHOOSE OF btHelp IN FRAME fPage0 /* Help */
OR CHOOSE OF MENU-ITEM miContents IN MENU mbMain DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btLast wMasterDetail
ON CHOOSE OF btLast IN FRAME fPage0 /* Last */
OR CHOOSE OF MENU-ITEM miLast IN MENU mbMain DO:
    RUN getLast IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btNext wMasterDetail
ON CHOOSE OF btNext IN FRAME fPage0 /* Next */
OR CHOOSE OF MENU-ITEM miNext IN MENU mbMain DO:
    RUN getNext IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPrev wMasterDetail
ON CHOOSE OF btPrev IN FRAME fPage0 /* Prev */
OR CHOOSE OF MENU-ITEM miPrev IN MENU mbMain DO:
    RUN getPrev IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btQueryJoins
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btQueryJoins wMasterDetail
ON CHOOSE OF btQueryJoins IN FRAME fPage0 /* Query Joins */
OR CHOOSE OF MENU-ITEM miQueryJoins IN MENU mbMain DO:
    RUN showQueryJoins IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btReportsJoins
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btReportsJoins wMasterDetail
ON CHOOSE OF btReportsJoins IN FRAME fPage0 /* Reports Joins */
OR CHOOSE OF MENU-ITEM miReportsJoins IN MENU mbMain DO:
    RUN showReportsJoins IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSearch wMasterDetail
ON CHOOSE OF btSearch IN FRAME fPage0 /* Search */
OR CHOOSE OF MENU-ITEM miSearch IN MENU mbMain DO:
    {method/ZoomReposition.i &ProgramZoom="yamzoom/z01yam003.w"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage2
&Scoped-define SELF-NAME btUpdateSon2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdateSon2 wMasterDetail
ON CHOOSE OF btUpdateSon2 IN FRAME fPage2 /* Alterar */
DO:
    IF AVAIL tt-mmv-turno-esp THEN DO:
         SESSION:SET-WAIT-STATE("GENERAL":U).

         /*--- Executa programa de inclus’o de filho ---*/
         RUN mvp/esmv0121b.w PERSISTENT SET hSonProgram (INPUT tt-mmv-exc-esp-data.r-rowid,
                                                         INPUT tt-mmv-turno-esp.r-rowid,
                                                         INPUT "UPDATE":U,
                                                         INPUT THIS-PROCEDURE,
                                                         INPUT 2).

         IF VALID-HANDLE(hSonProgram) THEN
             /*--- Inicializa programa de inclus’o de filho ---*/
             RUN initializeInterface IN hSonProgram.

         /*--- Seta cursor do mouse para normal ---*/
         SESSION:SET-WAIT-STATE("":U).

         RUN openQueriesSon IN THIS-PROCEDURE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage3
&Scoped-define SELF-NAME btUpdateSon3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdateSon3 wMasterDetail
ON CHOOSE OF btUpdateSon3 IN FRAME fPage3 /* Alterar */
DO:
    if avail tt-mmv-turno-esp then do:
         SESSION:SET-WAIT-STATE("GENERAL":U).

        /*--- Executa programa de inclus’o de filho ---*/
         RUN mvp/esmv0121c.w PERSISTENT SET hSonProgram (INPUT tt-mmv-cap-esp-turno.r-rowid,
                                                         INPUT tt-mmv-turno-esp.r-rowid,
                                                         INPUT "UPDATE":U,
                                                         INPUT THIS-PROCEDURE,
                                                         INPUT 3).

         IF VALID-HANDLE(hSonProgram) THEN
             /*--- Inicializa programa de inclus’o de filho ---*/
             RUN initializeInterface IN hSonProgram.

         /*--- Seta cursor do mouse para normal ---*/
         SESSION:SET-WAIT-STATE("":U).
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage0
&Scoped-define BROWSE-NAME brSon1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wMasterDetail 


/*--- L¢gica para inicializa‡Æo do programam ---*/
{masterdetail/MainBlock.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterDestroyInterface wMasterDetail 
PROCEDURE afterDestroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF VALID-HANDLE(hDBOMiTurno) THEN DO:
        RUN destroy IN hDBOMiTurno.
    END.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterDisplayFields wMasterDetail 
PROCEDURE afterDisplayFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN gTpEspecial-esmv0121 = {&ttParent}.cod-especialid.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wMasterDetail 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {utp/ut-liter.i Especialidade}
    ASSIGN {&ttParent}.cod-especialid:LABEL IN FRAME fPage0 = RETURN-VALUE.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goToRecord wMasterDetail 
PROCEDURE goToRecord :
/*------------------------------------------------------------------------------
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

    DEFINE VARIABLE cTpEspecial LIKE {&ttParent}.cod-especialid 
           VIEW-AS FILL-IN SIZE 12.00 BY 0.88 NO-UNDO.


    DEFINE FRAME fGoToRecord
        cTpEspecial       AT ROW 1.75 COL 23.00 COLON-ALIGNED
        btGoToOK          AT ROW 3.63 COL 2.14
        btGoToCancel      AT ROW 3.63 COL 13
        rtGoToButton      AT ROW 3.38 COL 1
        SPACE(0.28)
        WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
             THREE-D SCROLLABLE TITLE "V  Para Especialidade" FONT 1
             DEFAULT-BUTTON btGoToOK CANCEL-BUTTON btGoToCancel.

    ON "CHOOSE":U OF btGoToOK IN FRAME fGoToRecord DO:
        ASSIGN cTpEspecial.

        /* Posiciona query, do DBO, atrav‚s dos valores do ¡ndice £nico */
        RUN goToKey IN {&hDBOParent} (INPUT cTpEspecial).
        IF RETURN-VALUE = "NOK":U THEN DO:
            RUN utp/ut-msgs.p (INPUT "SHOW":U, INPUT 2, INPUT "Especialidade").

            RETURN NO-APPLY.
        END.

        /* Retorna rowid do registro corrente do DBO */
        RUN getRowid IN {&hDBOParent} (OUTPUT rGoTo).

        /* Reposiciona registro com base em um rowid */
        RUN repositionRecord IN THIS-PROCEDURE (INPUT rGoTo).

        APPLY "GO":U TO FRAME fGoToRecord.
    END.

    ENABLE cTpEspecial btGoToOK btGoToCancel 
        WITH FRAME fGoToRecord. 

    WAIT-FOR "GO":U OF FRAME fGoToRecord.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeDBOs wMasterDetail 
PROCEDURE initializeDBOs :
/*------------------------------------------------------------------------------
  Purpose:     Inicializa DBOs
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

    /*--- Verifica se o DBO j  est  inicializado ---*/.
    IF NOT VALID-HANDLE({&hDBOParent})          OR 
       {&hDBOParent}:TYPE      <> "PROCEDURE":U OR
       {&hDBOParent}:FILE-NAME <> "yambo/ydm004.p":U THEN DO:
        RUN yambo/ydm004.p PERSISTENT SET {&hDBOParent}.
    END.
    RUN openQueryStatic IN {&hDBOParent} (INPUT "Main":U) NO-ERROR.

    /*--- Verifica se o DBO j  est  inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOSon1})          OR 
       {&hDBOSon1}:TYPE      <> "PROCEDURE":U OR
       {&hDBOSon1}:FILE-NAME <> "yambo/ydm017.p":U THEN DO:
        RUN yambo/ydm017.p PERSISTENT SET {&hDBOSon1}.
    END.

    /*--- Verifica se o DBO j  est  inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOSon2})          OR 
       {&hDBOSon2}:TYPE      <> "PROCEDURE":U OR
       {&hDBOSon2}:FILE-NAME <> "yambo/ydm016.p":U THEN DO:
        RUN yambo/ydm016.p PERSISTENT SET {&hDBOSon2}.
    END.

    /*--- Verifica se o DBO j  est  inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOSon3})          OR 
       {&hDBOSon3}:TYPE      <> "PROCEDURE":U OR
       {&hDBOSon3}:FILE-NAME <> "yambo/ydm014.p":U THEN DO:
        RUN yambo/ydm014.p PERSISTENT SET {&hDBOSon3}.
    END.

    /*--- Verifica se o DBO j  est  inicializado ---*/
    IF NOT VALID-HANDLE(hDBOMiTurno)           OR 
        hDBOMiTurno:TYPE      <> "PROCEDURE":U OR
        hDBOMiTurno:FILE-NAME <> "mnbo/bomn015.p":U THEN DO:
        RUN mnbo/bomn015.p PERSISTENT SET hDBOMiTurno.
    END.
    RUN openQueryStatic IN hDBOMiTurno (INPUT "Default":U) NO-ERROR.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openQueriesSon wMasterDetail 
PROCEDURE openQueriesSon :
/*------------------------------------------------------------------------------
  Purpose:     Atualiza browsers filhos
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

    RUN pi-OpenQuerySon1.

    RUN pi-OpenQuerySon2.
                                   
    RUN pi-OpenQuerySon3.

    if avail {&ttParent} THEN
        assign btAddSon2:sensitive in frame fPage2 = num-results("brSon1") > 0                                
               btAddSon3:sensitive in frame fPage3 = num-results("brSon1") > 0.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-OpenQuerySon1 wMasterDetail 
PROCEDURE pi-OpenQuerySon1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {masterdetail/OpenQueriesSon.i &Parent="Espec"
                                   &Query="Espec"
                                   &PageNumber="1"}
                                   
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-OpenQuerySon2 wMasterDetail 
PROCEDURE pi-OpenQuerySon2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {masterdetail/OpenQueriesSon.i &Parent="Espec"
                                   &Query="Espec"
                                   &PageNumber="2"}
                                   
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-OpenQuerySon3 wMasterDetail 
PROCEDURE pi-OpenQuerySon3 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {masterdetail/OpenQueriesSon.i &Parent="Espec"
                                   &Query="Espec"
                                   &PageNumber="3"}
                                   
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnDescCalen wMasterDetail 
FUNCTION fnDescCalen RETURNS CHARACTER
  (INPUT pCalen AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDesc AS CHARACTER  NO-UNDO.

    FOR FIRST calen-gener 
        WHERE calen-gener.cd-calen = pCalen NO-LOCK:
    END.
    ASSIGN cDesc = IF AVAIL calen-gener THEN calen-gener.descricao ELSE "":U.

    RETURN cDesc.   /* Function return value. */    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnDescTurno wMasterDetail 
FUNCTION fnDescTurno RETURNS CHARACTER
   (INPUT piCdTurno AS INTEGER) :

DEFINE VARIABLE cDesc AS CHARACTER NO-UNDO.

{method/ReferenceFields.i 
      &HandleDBOLeave="hDBOMiTurno"
      &KeyValue1="piCdTurno"
      &FieldName1="descricao"
      &Variable1="cDesc"}

RETURN cDesc.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


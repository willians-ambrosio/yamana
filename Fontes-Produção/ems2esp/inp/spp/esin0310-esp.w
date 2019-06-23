&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          mginv            PROGRESS
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME wMasterDetail


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-hist-alter-inv NO-UNDO LIKE hist-alter-inv
       field r-rowid as rowid.
DEFINE TEMP-TABLE tt-proj-inv NO-UNDO LIKE proj-inv
       field r-rowid as rowid.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wMasterDetail 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i esin0310-esp 2.00.00.099}

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program          esin0310-esp
&GLOBAL-DEFINE Version          2.00.00.099

&GLOBAL-DEFINE Folder           YES
&GLOBAL-DEFINE InitialPage      1
&GLOBAL-DEFINE FolderLabels     Altera‡äes

&GLOBAL-DEFINE First            YES
&GLOBAL-DEFINE Prev             YES
&GLOBAL-DEFINE Next             YES
&GLOBAL-DEFINE Last             YES
&GLOBAL-DEFINE GoTo             YES
&GLOBAL-DEFINE Search           YES

&GLOBAL-DEFINE AddParent        no 
&GLOBAL-DEFINE CopyParent       no 
&GLOBAL-DEFINE UpdateParent     no 
&GLOBAL-DEFINE DeleteParent     no 

&GLOBAL-DEFINE AddSon1          no 
&GLOBAL-DEFINE CopySon1         no 
&GLOBAL-DEFINE UpdateSon1       no 
&GLOBAL-DEFINE DeleteSon1       no 

&GLOBAL-DEFINE AddSon2          no 
&GLOBAL-DEFINE CopySon2         no 
&GLOBAL-DEFINE UpdateSon2       no 
&GLOBAL-DEFINE DeleteSon2       no 

&GLOBAL-DEFINE ttParent         tt-proj-inv
&GLOBAL-DEFINE hDBOParent       h-Boiv038
&GLOBAL-DEFINE DBOParentTable   proj-inv


&GLOBAL-DEFINE ttSon1           tt-hist-alter-inv
&GLOBAL-DEFINE hDBOSon1         h-espbo0001
&GLOBAL-DEFINE DBOSon1Table     hist-alter-inv

&GLOBAL-DEFINE page0Fields      bt-excel tt-proj-inv.num-projeto tt-proj-inv.cod-est-exec tt-proj-inv.sigla

&GLOBAL-DEFINE page1Browse      brSon1

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* Local Variable Definitions (DBOs Handles) ---                        */
DEFINE VARIABLE {&hDBOParent} AS HANDLE NO-UNDO.
DEFINE VARIABLE {&hDBOSon1}   AS HANDLE NO-UNDO.

define variable wh-pesquisa    as widget-handle no-undo.
define NEW GLOBAL SHARED var adm-broker-hdl                 AS HANDLE        NO-UNDO.


DEF TEMP-TABLE tt-excel LIKE hist-alter-inv
    FIELD linha AS INT
    field sit-proj as char.



def new global shared var gc-c-estab-ini  as CHAR no-undo.
def new global shared var gc-c-estab-fim  as char no-undo.
def new global shared var gc-c-proj-ini   as INT no-undo.
def new global shared var gc-c-proj-fim   as INT no-undo.
def new global shared var gde-i-ordem-ini as INT  no-undo.
def new global shared var gde-i-ordem-fim as INT  no-undo.

def new global shared VAR l-cancela AS LOGICAL NO-UNDO.

DEF STREAM s-planilha.

{utp/ut-glob.i}

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
&Scoped-define INTERNAL-TABLES tt-hist-alter-inv

/* Definitions for BROWSE brSon1                                        */
&Scoped-define FIELDS-IN-QUERY-brSon1 tt-hist-alter-inv.sequencia ~
tt-hist-alter-inv.num-ordem tt-hist-alter-inv.data tt-hist-alter-inv.hora ~
tt-hist-alter-inv.usuario tt-hist-alter-inv.valor-origem ~
tt-hist-alter-inv.valor-destino tt-hist-alter-inv.Motivo ~
tt-hist-alter-inv.tipo 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brSon1 
&Scoped-define QUERY-STRING-brSon1 FOR EACH tt-hist-alter-inv NO-LOCK
&Scoped-define OPEN-QUERY-brSon1 OPEN QUERY brSon1 FOR EACH tt-hist-alter-inv NO-LOCK.
&Scoped-define TABLES-IN-QUERY-brSon1 tt-hist-alter-inv
&Scoped-define FIRST-TABLE-IN-QUERY-brSon1 tt-hist-alter-inv


/* Definitions for FRAME fPage1                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fPage1 ~
    ~{&OPEN-QUERY-brSon1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-proj-inv.cod-est-exec tt-proj-inv.sigla ~
tt-proj-inv.num-projeto 
&Scoped-define ENABLED-TABLES tt-proj-inv
&Scoped-define FIRST-ENABLED-TABLE tt-proj-inv
&Scoped-Define ENABLED-OBJECTS rtParent rtToolBar btFirst btPrev btNext ~
btLast btGoTo btSearch bt-excel btQueryJoins btReportsJoins btExit btHelp ~
c-estabel c-desc-projeto c-situacao 
&Scoped-Define DISPLAYED-FIELDS tt-proj-inv.cod-est-exec tt-proj-inv.sigla ~
tt-proj-inv.num-projeto 
&Scoped-define DISPLAYED-TABLES tt-proj-inv
&Scoped-define FIRST-DISPLAYED-TABLE tt-proj-inv
&Scoped-Define DISPLAYED-OBJECTS c-estabel c-desc-projeto c-situacao 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
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
DEFINE BUTTON bt-excel 
     IMAGE-UP FILE "image/excel.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY 1.25.

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

DEFINE VARIABLE c-desc-projeto AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 31.29 BY .79 NO-UNDO.

DEFINE VARIABLE c-estabel AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 32.72 BY .79 NO-UNDO.

DEFINE VARIABLE c-situacao AS CHARACTER FORMAT "X(256)":U 
     LABEL "Situa‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .79 NO-UNDO.

DEFINE RECTANGLE rtParent
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 2.5.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.5
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brSon1 FOR 
      tt-hist-alter-inv SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brSon1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brSon1 wMasterDetail _STRUCTURED
  QUERY brSon1 NO-LOCK DISPLAY
      tt-hist-alter-inv.sequencia FORMAT ">>>9":U
      tt-hist-alter-inv.num-ordem FORMAT ">>>>9":U
      tt-hist-alter-inv.data FORMAT "99/99/9999":U
      tt-hist-alter-inv.hora FORMAT "X(8)":U
      tt-hist-alter-inv.usuario FORMAT "X(16)":U
      tt-hist-alter-inv.valor-origem FORMAT ">>>>>,>>>,>>>,>>9.99":U
      tt-hist-alter-inv.valor-destino FORMAT ">>>>>,>>>,>>>,>>9.99":U
      tt-hist-alter-inv.Motivo FORMAT "X(72)":U
      tt-hist-alter-inv.tipo FORMAT ">9":U
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
     bt-excel AT ROW 1.13 COL 63.86
     btQueryJoins AT ROW 1.13 COL 74.86 HELP
          "Consultas relacionadas"
     btReportsJoins AT ROW 1.13 COL 78.86 HELP
          "Relat¢rios relacionados"
     btExit AT ROW 1.13 COL 82.86 HELP
          "Sair"
     btHelp AT ROW 1.13 COL 86.86 HELP
          "Ajuda"
     tt-proj-inv.cod-est-exec AT ROW 3.08 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.29 BY .79
     c-estabel AT ROW 3.08 COL 21.43 COLON-ALIGNED NO-LABEL
     tt-proj-inv.sigla AT ROW 3.08 COL 62.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.86 BY .79
     tt-proj-inv.num-projeto AT ROW 3.88 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.72 BY .79
     c-desc-projeto AT ROW 3.88 COL 22.86 COLON-ALIGNED NO-LABEL
     c-situacao AT ROW 3.88 COL 62.29 COLON-ALIGNED
     rtParent AT ROW 2.63 COL 1.14
     rtToolBar AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 16.46
         FONT 1.

DEFINE FRAME fPage1
     brSon1 AT ROW 1.17 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 6.46
         SIZE 84.43 BY 10.63
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: MasterDetail
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Temp-Tables and Buffers:
      TABLE: tt-hist-alter-inv T "?" NO-UNDO mgesp hist-alter-inv
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
      TABLE: tt-proj-inv T "?" NO-UNDO mginv proj-inv
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
         HEIGHT             = 16.46
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
ASSIGN FRAME fPage1:FRAME = FRAME fPage0:HANDLE.

/* SETTINGS FOR FRAME fPage0
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME fPage1
                                                                        */
/* BROWSE-TAB brSon1 1 fPage1 */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wMasterDetail)
THEN wMasterDetail:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brSon1
/* Query rebuild information for BROWSE brSon1
     _TblList          = "Temp-Tables.tt-hist-alter-inv"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.tt-hist-alter-inv.sequencia
     _FldNameList[2]   = Temp-Tables.tt-hist-alter-inv.num-ordem
     _FldNameList[3]   = Temp-Tables.tt-hist-alter-inv.data
     _FldNameList[4]   = Temp-Tables.tt-hist-alter-inv.hora
     _FldNameList[5]   = Temp-Tables.tt-hist-alter-inv.usuario
     _FldNameList[6]   = Temp-Tables.tt-hist-alter-inv.valor-origem
     _FldNameList[7]   = Temp-Tables.tt-hist-alter-inv.valor-destino
     _FldNameList[8]   = Temp-Tables.tt-hist-alter-inv.Motivo
     _FldNameList[9]   = Temp-Tables.tt-hist-alter-inv.tipo
     _Query            is OPENED
*/  /* BROWSE brSon1 */
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


&Scoped-define SELF-NAME bt-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-excel wMasterDetail
ON CHOOSE OF bt-excel IN FRAME fPage0 /* Button 1 */
DO:
  run pi-gera-excel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

  {include/zoomvarreposition.i &prog-zoom="ivzoom\z02iv038.w"
                     &TableName="tt-proj-inv"
                     &campo="tt-proj-inv.cod-est-exec"
                     &campozoom="cod-est-exec"
                     &campo2="tt-proj-inv.num-projeto"
                     &campozoom2="num-projeto"
                     &tipocampo2="integer"
                     &findMethod="goTokey1"
                     &hDBOParent="h-boiv038"}



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brSon1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wMasterDetail 


/*:T--- L¢gica para inicializa‡Æo do programam ---*/
{masterdetail/mainblock.i}

    enable bt-excel with frame fPage0.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterDisplayFields wMasterDetail 
PROCEDURE afterDisplayFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF AVAIL tt-proj-inv THEN DO:
        ASSIGN c-situacao:SCREEN-VALUE IN FRAME     fPage0 = {ivinc/i01iv038.i 4 tt-proj-inv.cod-situacao-proj} 
               c-desc-projeto:SCREEN-VALUE IN FRAME fPage0 = tt-proj-inv.descricao .                           
    
        FIND FIRST estabelec
            WHERE estabelec.cod-estabel = tt-proj-inv.cod-est-exec NO-LOCK NO-ERROR.
        IF AVAIL estabelec then
            ASSIGN c-estabel:SCREEN-VALUE IN FRAME fPage0 = estabelec.nome.
    END.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goToRecord wMasterDetail 
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
    
    DEFINE VARIABLE cod-estabel LIKE {&ttParent}.cod-est-exec 
        VIEW-AS FILL-IN SIZE 6 BY .88 NO-UNDO.
    DEFINE VARIABLE num-proj    LIKE {&ttParent}.num-projeto 
        VIEW-AS FILL-IN SIZE 9 BY .88 NO-UNDO.
    
    DEFINE FRAME fGoToRecord
                 cod-estabel       AT ROW 1.21 COL 17.72 COLON-ALIGNED
                 num-proj          AT ROW 2.21 COL 17.72 COLON-ALIGNED
                 btGoToOK          AT ROW 3.63 COL 2.14
                 btGoToCancel      AT ROW 3.63 COL 13
                 rtGoToButton      AT ROW 3.38 COL 1
                 SPACE(0.28)
                 WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER SIDE-LABELS NO-UNDERLINE 
                      THREE-D SCROLLABLE TITLE "V  Para Projeto investimento" FONT 1
                      DEFAULT-BUTTON btGoToOK CANCEL-BUTTON btGoToCancel.

                 /*tech1139 - FO 1338.917 - 10/07/2006  */
    RUN utp/ut-trfrrp.p (input Frame fGoToRecord:Handle).
    {utp/ut-liter.i "V _Para_proj-inv"}
    ASSIGN FRAME fGoToRecord:TITLE = RETURN-VALUE.
/*tech1139 - FO 1338.917 - 10/07/2006  */
                                         
    ON "CHOOSE":U OF btGoToOK IN FRAME fGoToRecord DO:
        ASSIGN cod-estabel
               num-proj.   
        
        /*:T Posiciona query, do DBO, atrav‚s dos valores do ¡ndice £nico */
        RUN goToKey1 IN {&hDBOParent} (INPUT cod-estabel  ,
                                       INPUT num-proj     ).
        IF RETURN-VALUE = "NOK":U THEN DO:
            RUN utp/ut-msgs.p (INPUT "SHOW":U, INPUT 2, INPUT "Projeto investimento":U).
            
            RETURN NO-APPLY.
        END.
        
        /*:T Retorna rowid do registro corrente do DBO */
        RUN getRowid IN {&hDBOParent} (OUTPUT rGoTo).
        
        /*:T Reposiciona registro com base em um rowid */
        RUN repositionRecord IN THIS-PROCEDURE (INPUT rGoTo).
        
        APPLY "GO":U TO FRAME fGoToRecord.
    END.
    
    ENABLE cod-estabel num-proj  btGoToOK btGoToCancel 
        WITH FRAME fGoToRecord. 
    
    WAIT-FOR "GO":U OF FRAME fGoToRecord.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeDBOs wMasterDetail 
PROCEDURE initializeDBOs :
/*:T------------------------------------------------------------------------------
  Purpose:     Inicializa DBOs
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    /*:T--- Verifica se o DBO j  est  inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOParent}) OR
       {&hDBOParent}:TYPE <> "PROCEDURE":U OR
       {&hDBOParent}:FILE-NAME <> "ivbo\boiv038.p":U THEN DO:
        {btb/btb008za.i1 ivbo\boiv038.p YES}
        {btb/btb008za.i2 ivbo\boiv038.p '' {&hDBOParent}} 
    END.
    
    RUN openQueryStatic IN {&hDBOParent} (INPUT "main":U) NO-ERROR.
    
    /*:T--- Verifica se o DBO j  est  inicializado ---*/
    IF NOT VALID-HANDLE({&hDBOSon1}) OR 
       {&hDBOSon1}:TYPE <> "PROCEDURE":U OR
       {&hDBOSon1}:FILE-NAME <> "espbo\boesp0001.p":U THEN DO:
        {btb/btb008za.i1 espbo\boesp0001.p YES}
        {btb/btb008za.i2 espbo\boesp0001.p '' {&hDBOSon1}} 
    END.
    
        
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeDBOs wMasterDetail 
PROCEDURE AfterInitializeInterface :
/*:T------------------------------------------------------------------------------
  Purpose:     Inicializa DBOs
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN bt-excel:SENSITIVE IN FRAME fPage0 = YES.
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openQueriesSon wMasterDetail 
PROCEDURE openQueriesSon :
/*:T------------------------------------------------------------------------------
  Purpose:     Atualiza browsers filhos
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    
    {masterdetail/openqueriesson.i &Parent="proj-inv"
                                   &Query="ofordem-inv"
                                   &PageNumber="1"}
    
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-excel wMasterDetail 
PROCEDURE pi-gera-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each tt-excel:
   delete tt-excel.
end.

ASSIGN l-cancela = YES.
run inp\spp\esin9001.w.


IF  l-cancela = YES THEN
    RETURN "OK":U.

for each hist-alter-inv
    where hist-alter-inv.cod-estabel-exe >= gc-c-estab-ini 
    and   hist-alter-inv.cod-estabel-exe <= gc-c-estab-fim 
    and   hist-alter-inv.num-proj        >= gc-c-proj-ini  
    and   hist-alter-inv.num-proj        <= gc-c-proj-fim  
    and   hist-alter-inv.num-ordem       >= gde-i-ordem-ini
    and   hist-alter-inv.num-ordem       <= gde-i-ordem-fim:
    
    if not can-find(first tt-excel
                  where tt-excel.sequencia = hist-alter-inv.sequencia
                  and   tt-excel.tipo      = hist-alter-inv.tipo) then do:

        FOR FIRST proj-inv FIELDS (cod-situacao-proj)
            WHERE /*proj-inv.ep-codigo    = tt-proj-inv.ep-codigo 
              AND*/ proj-inv.cod-est-exec = hist-alter-inv.cod-estabel-exe
              AND proj-inv.num-projeto  = hist-alter-inv.num-proj NO-LOCK:
        END.

        create tt-excel.                                         
        assign tt-excel.cod-estabel       =  hist-alter-inv.cod-estabel-exe
               tt-excel.num-proj          =  hist-alter-inv.num-proj
               tt-excel.num-ordem         =  hist-alter-inv.num-ordem
               tt-excel.sit-proj          = {ivinc/i01iv038.i 4 proj-inv.cod-situacao-proj}
               tt-excel.data              =  hist-alter-inv.data
               tt-excel.hora              =  hist-alter-inv.hora
               tt-excel.usuar             =  hist-alter-inv.usuario
               tt-excel.valor-origem      =  hist-alter-inv.valor-origem
               tt-excel.valor-destino     =  hist-alter-inv.valor-destino
               tt-excel.motivo            =  hist-alter-inv.motivo
               tt-excel.sigla             =  hist-alter-inv.sigla
               tt-excel.sequencia         =  hist-alter-inv.sequencia. 
    end.
end.

if can-find (first tt-excel) then do:

   output stream s-planilha to value(session:temp-directory + "Historio Ivestimento.csv") convert target 'iso8859-1'.
   

   put stream s-planilha unformatted "Estabelecimento;Num Proj;Num Ordem;Sit Projeto;Sigla;Data;Hora;Usu rio;Valor Antigo;Valor Novo;Motivo Altera‡Æo" skip.

   for each tt-excel no-lock:
    
           put stream s-planilha unformatted
               tt-excel.cod-estabel ";"
               tt-excel.num-proj ";"
               tt-excel.num-ordem ";" 
               tt-excel.sit-proj ";"
               tt-excel.sigla ";"
               tt-excel.data ";"
               tt-excel.hora ";"
               tt-excel.usuar ";"
               tt-excel.valor-origem ";"
               tt-excel.valor-destino ";"
               tt-excel.motivo skip.

   end.
   output stream s-planilha close.
   os-command silent value('start excel' + " " + chr(34) + (session:temp-directory + "Historio Ivestimento.csv") + chr(34)).
end.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



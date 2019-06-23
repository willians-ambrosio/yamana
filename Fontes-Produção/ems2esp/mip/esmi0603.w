&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME wWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWindow 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i esmi0603 2.00.00.001}  /*** 010001 ***/
/********************************************************************************
** Copyright DATasUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATasUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        esmi0603
&GLOBAL-DEFINE Version        2.00.00.001

&GLOBAL-DEFINE WindowType     Master/Detail

&GLOBAL-DEFINE Folder         no

&GLOBAL-DEFINE page0Widgets   btQueryJoins btReportsJoins btExit btHelp ~
                              btAbrir btSalvar btRefresh btFiltro btParam btExpande ~
                              btRedimensiona brDetalhe ~
                              btExcel  

/* Local Variable Definitions ---             */

define variable c-arquivo         as character                     no-undo.
define variable i-cont            as integer                       no-undo.
define variable iVisao            as integer                       no-undo.
define variable hUTAPI011         as handle                        no-undo.
define variable cSub              as character                     no-undo.
define variable cEvento           as character                     no-undo.
define variable iManut            as integer                       no-undo.
define variable lReal             as logical                       no-undo.
define variable hHandle           as handle                        no-undo.
DEFINE VARIABLE l-executa         AS LOGICAL                       NO-UNDO.
DEFINE VARIABLE i-qtd-interv-ini  AS INTEGER   INITIAL 1           NO-UNDO.
DEFINE VARIABLE i-qtd-interv-fim  AS INTEGER   INITIAL 999999999   NO-UNDO.
DEFINE VARIABLE i-tipo-graf       AS INTEGER   INITIAL 1           NO-UNDO.
DEFINE VARIABLE de-data-ini       AS DECIMAL                       NO-UNDO.
DEFINE VARIABLE de-data-fim       AS DECIMAL                       NO-UNDO.
DEFINE VARIABLE de-km-anterior    AS DECIMAL                       NO-UNDO.
DEFINE VARIABLE de-km-atual       AS DECIMAL                       NO-UNDO.
DEFINE VARIABLE de-tempo-reparo   AS DECIMAL                       NO-UNDO.
/** Busca Valor **/                                            
define variable rRowid        as rowid                         no-undo.
define variable cCodPai       as character format "x(20)"      no-undo.
define variable vCodigo       as character format "x(20)"      no-undo.
define variable vDescricao    as character format "x(32)"      no-undo.
define variable iImage        as integer                       no-undo.
/** OCX **/                                                    
define variable chTreeList    as com-handle                    no-undo.
define variable chImageList   as com-handle                    no-undo.
/** Controles **/                                              
define variable l-expande     as log init no                   no-undo.
define variable l-ok          as log                           no-undo.
define variable i-button-tree as int init 1                    no-undo.
define variable i-result      as int                           no-undo.
/** Pop-Menu **/                                               
define variable pop-menu      as widget-handle                 no-undo.
define variable exp-con       as widget-handle                 no-undo.
define variable detalhe       as widget-handle                 no-undo.
define variable regua         as widget-handle                 no-undo.
/*** Window ***/                                               
define variable dWinXC        as decimal                       no-undo.
define variable dWinYC        as decimal                       no-undo.
/** TXT e Excel **/
define variable cTituloTxt    as character format "x(256)"     no-undo.
define variable cDescPai      as character format "x(256)"     no-undo.
define variable i-cont2       as integer                       no-undo.

/** Vari vel para Apresentar o que est  sendo avaliado no Excel **/
DEFINE VARIABLE cAvaliado AS CHARACTER  NO-UNDO.
/** Vari vel para Verificar se o Tipo de Manuten‡Æo ‚ corretiva **/
DEFINE VARIABLE tpManutCorretiva AS LOGICAL INITIAL YES  NO-UNDO.

{utp/utapi011.i}  /** Gr fico **/
{mip/esmi0603.i}    /** Defini‡Æo da temp-table de parƒmetros **/

/** Classifica‡Æo **/
def temp-table ttVisao no-undo
    field dimensao  as character
    field sequencia as integer
    index codigo is primary unique sequencia.

/** Tabelas auxiliares **/
DEF TEMP-TABLE ttAux
    field ep-codigo             like mab-eqpto.ep-codigo
    field cod-eqpto             like mab-eqpto.cod-eqpto
    field cod-sub-sist          like mmv-tar-ord-manut.cod-sub-sist
    field cod-evento            like mmv-tar-ord-manut.cod-evento
    field nr-ord-prod           like mmv-tar-ord-manut.nr-ord-prod
    field val-dat-invrtda-term  like mmv-tar-ord-manut.val-dat-invrtda-term
    field val-hora-reporte      like mmv-tar-ord-manut.val-hora-reporte
    field un                    like mab-model.un
    field val-km-inicial        like mab-eqpto.val-km-inicial
    field cod-grp-eqpto         like mab-eqpto.cod-grp-eqpto
    field cod-model             like mab-eqpto.cod-model
    field cod-estabel           like mab-eqpto.cod-estabel
    field cod-grp-event         like mab-grp-event.cod-grp-event
    field cod-sistema           like mab-sist.cod-sistema
    field vli-ano-fabric        like mab-eqpto.vli-ano-fabric
    field cd-tipo               like tipo-manut.cd-tipo
    field cc-codigo             like mab-eqpto.cc-codigo
    field cd-tag                like mab-eqpto.cd-tag
    INDEX codigo IS PRIMARY nr-ord-prod
                            ep-codigo  
                            cod-eqpto   
                            cod-sub-sist
                            cod-evento  
                            val-dat-invrtda-term.

/** Dados **/
def temp-table ttDados no-undo
    field sequencia         as   integer
    field cod-dimensao      as   character format "x(300)"
    field cod-oficial       as   character format "X(16)"
    field desc-dimensao     as   character format "x(32)"
    field cod-dimens-pai    as   character format "x(300)"
    field qtd-interv        as   INTEGER   format ">>>,>>>,>>9"     LABEL "Interven‡äes"
    field tempo-reparo      as   decimal   format ">,>>>,>>9.99"    LABEL "Tempo Reparo"
    field r-rowid           as   rowid
    field p-image           as   integer
    field seq-tree          as   integer
    /**/
    FIELD nr-ord-produ  LIKE ord-manut.nr-ord-produ 
    FIELD cd-causa-padr LIKE ord-manut.cd-causa-padr
    FIELD cd-sint-padr  LIKE ord-manut.cd-sint-padr
    FIELD desc-sintoma  LIKE sint-padrao.descricao
    FIELD desc-causa    LIKE causa-padrao.descricao
    /**/
    index id is primary unique sequencia ascending
                               cod-dimensao ascending
                               nr-ord-produ.

define temp-table ttExcel no-undo
    field sequencia     as   integer
    field cod-oficial   as   character format "X(16)"
    field desc-dimensao as   character format "x(300)"
    field qtd-interv    as   integer   format ">>>,>>>,>>9"     LABEL "Interven‡äes" 
    field tempo-reparo  as   decimal   format ">,>>>,>>9.99"    LABEL "Tempo Reparo"
    index id is primary unique sequencia ascending.

define buffer bfttDados  for ttDados.
define buffer bfttDados2 for ttDados.
define buffer bfttVisao  for ttVisao.

define temp-table tt-estrut-tag no-undo
    field cod-tipo       like tag.cod-tipo
    field cd-tag         like tag.cd-tag
    field descricao      like tag.descricao
    field sequencia      as integer
    field ordem          as integer 
    index ch-principal ordem
    index ch-sequencia sequencia.

def buffer bf-tt-estrut-tag for tt-estrut-tag.
def buffer bf-tag           for tag.
def buffer bf-estr-tag      for estr-tag.



/** Horas **/
DEFINE VARIABLE horas   AS DECIMAL      NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0
&Scoped-define BROWSE-NAME brDetalhe

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttDados

/* Definitions for BROWSE brDetalhe                                     */
&Scoped-define FIELDS-IN-QUERY-brDetalhe ttDados.nr-ord-produ ttDados.cod-oficial ttDados.desc-dimensao ttDados.cd-causa-padr ttDados.desc-causa ttDados.cd-sint-padr ttDados.desc-sintoma ttDados.tempo-reparo ttDados.qtd-interv /* /* */ */ /* /* */ */ /* ttDados.cod-oficial */ /* ttDados.desc-dimensao */ /* ttDados.qtd-interv */ /* /* /* ttDados.qtd-reinc */ */ */ /* ttDados.tempo-reparo */ /* /* /* ttDados.per-interv */ */ */ /* /* ttDados.un */ */   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brDetalhe   
&Scoped-define SELF-NAME brDetalhe
&Scoped-define QUERY-STRING-brDetalhe for each ttDados no-lock
&Scoped-define OPEN-QUERY-brDetalhe open query {&SELF-NAME} for each ttDados no-lock.
&Scoped-define TABLES-IN-QUERY-brDetalhe ttDados
&Scoped-define FIRST-TABLE-IN-QUERY-brDetalhe ttDados


/* Definitions for FRAME fpage0                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fpage0 ~
    ~{&OPEN-QUERY-brDetalhe}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rtToolBar-2 btAbrir btSalvar btRefresh ~
btExpande btParam btFiltro btExcel btQueryJoins btReportsJoins btExit ~
btHelp btRedimensiona brDetalhe 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnBrowse wWindow 
FUNCTION fnBrowse RETURNS CHARACTER
  ( pCampo as character,
    iTipo  as integer )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnLabels wWindow 
FUNCTION fnLabels RETURNS CHARACTER
  ( pImage as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWindow AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU miArquivo 
       MENU-ITEM miAbrir        LABEL "&Abrir"         ACCELERATOR "CTRL-A"
       MENU-ITEM miSalvar       LABEL "&Salvar"        ACCELERATOR "CTRL-S"
       MENU-ITEM miAtualizar    LABEL "At&ualizar"     ACCELERATOR "CTRL-R"
       MENU-ITEM miExpande      LABEL "&Expande/Contrai" ACCELERATOR "CTRL-E"
       RULE
       MENU-ITEM miQtdInterv    LABEL "&Qtd Interven‡äes" ACCELERATOR "CTRL-Q"
       MENU-ITEM miSelecao      LABEL "Se&le‡Æo"       ACCELERATOR "CTRL-P"
       MENU-ITEM miFiltro       LABEL "&Filtro"        ACCELERATOR "CTRL-F"
       MENU-ITEM miClassifica   LABEL "&Classifica"    ACCELERATOR "CTRL-C"
       RULE
       MENU-ITEM miGrafico      LABEL "&Gr fico"       ACCELERATOR "CTRL-G"
       MENU-ITEM miImprimir     LABEL "&Imprimir"      ACCELERATOR "CTRL-J"
       MENU-ITEM miExcel        LABEL "E&xcel"         ACCELERATOR "CTRL-L"
       MENU-ITEM miTxt          LABEL "&Texto"         ACCELERATOR "CTRL-T"
       RULE
       MENU-ITEM miQueryJoins   LABEL "C&onsultas"    
       MENU-ITEM miReportsJoins LABEL "Rela&t¢rios"   
       RULE
       MENU-ITEM miExit         LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU smHelp 
       MENU-ITEM miContents     LABEL "&Conte£do"     
       MENU-ITEM miAbout        LABEL "&Sobre..."     .

DEFINE MENU mbMain MENUBAR
       SUB-MENU  miArquivo      LABEL "&Arquivo"      
       SUB-MENU  smHelp         LABEL "Aj&uda"        .


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-2 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-2 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btAbrir 
     IMAGE-UP FILE "image\im-open":U
     IMAGE-INSENSITIVE FILE "image\ii-open":U
     LABEL "" 
     SIZE 4 BY 1.25 TOOLTIP "Abrir"
     FONT 4.

DEFINE BUTTON btExcel 
     IMAGE-UP FILE "image\excel":U
     LABEL "Planilha" 
     SIZE 4 BY 1.25 TOOLTIP "Exportar para Excel".

DEFINE BUTTON btExit 
     IMAGE-UP FILE "image\im-exi":U
     IMAGE-INSENSITIVE FILE "image\ii-exi":U
     LABEL "Exit" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btExpande 
     IMAGE-UP FILE "image\im-crit":U
     IMAGE-INSENSITIVE FILE "image\ii-crit":U
     LABEL "" 
     SIZE 4 BY 1.25 TOOLTIP "Expande / Contrai"
     FONT 4.

DEFINE BUTTON btFiltro 
     IMAGE-UP FILE "image\im-fil":U
     IMAGE-INSENSITIVE FILE "image\ii-fil":U
     LABEL "Filtro" 
     SIZE 4 BY 1.25 TOOLTIP "Filtro"
     FONT 4.

DEFINE BUTTON btHelp 
     IMAGE-UP FILE "image\im-hel":U
     IMAGE-INSENSITIVE FILE "image\ii-hel":U
     LABEL "Help" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btParam 
     IMAGE-UP FILE "image\im-param":U
     IMAGE-INSENSITIVE FILE "image\ii-param":U
     LABEL "Parƒmetros" 
     SIZE 4 BY 1.25 TOOLTIP "Parƒmetros"
     FONT 4.

DEFINE BUTTON btQueryJoins 
     IMAGE-UP FILE "image\im-joi":U
     IMAGE-INSENSITIVE FILE "image\ii-joi":U
     LABEL "Query Joins" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btRedimensiona 
     LABEL "" 
     SIZE 90 BY .29.

DEFINE BUTTON btRefresh 
     IMAGE-UP FILE "image\im-sav":U
     IMAGE-INSENSITIVE FILE "image\ii-sav":U
     LABEL "" 
     SIZE 4 BY 1.25 TOOLTIP "Atualizar"
     FONT 4.

DEFINE BUTTON btReportsJoins 
     IMAGE-UP FILE "image\im-pri":U
     IMAGE-INSENSITIVE FILE "image\ii-pri":U
     LABEL "Reports Joins" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btSalvar 
     IMAGE-UP FILE "image\im-grava":U
     IMAGE-INSENSITIVE FILE "image\ii-grava":U
     LABEL "" 
     SIZE 4 BY 1.25 TOOLTIP "Salvar"
     FONT 4.

DEFINE RECTANGLE rtToolBar-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.5
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brDetalhe FOR 
      ttDados SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brDetalhe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brDetalhe wWindow _FREEFORM
  QUERY brDetalhe NO-LOCK DISPLAY
      ttDados.nr-ord-produ   WIDTH 14
    ttDados.cod-oficial    WIDTH 14
    ttDados.desc-dimensao  WIDTH 30
    ttDados.cd-causa-padr  WIDTH 8
    ttDados.desc-causa     WIDTH 30
    ttDados.cd-sint-padr   WIDTH 8
    ttDados.desc-sintoma   WIDTH 30
    ttDados.tempo-reparo   WIDTH 14
    ttDados.qtd-interv     WIDTH 14

      
/* /*                                              */     */
/* /*                                              */     */
/*       ttDados.cod-oficial      width 14                */
/*  ttDados.desc-dimensao    width 30                     */
/* ttDados.qtd-interv       width 12                      */
/* /* /* ttDados.qtd-reinc        width 12 */      */     */
/* ttDados.tempo-reparo     width 13                      */
/* /* /* ttDados.per-interv       width 14 */      */     */
/* /* ttDados.un               width 03 */                */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 90 BY 8.5
         FONT 1
         TITLE "".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btAbrir AT ROW 1.13 COL 2
     btSalvar AT ROW 1.13 COL 6
     btRefresh AT ROW 1.13 COL 10
     btExpande AT ROW 1.13 COL 16
     btParam AT ROW 1.13 COL 55 HELP
          "Parƒmetros"
     btFiltro AT ROW 1.13 COL 59 HELP
          "Filtro"
     btExcel AT ROW 1.13 COL 63
     btQueryJoins AT ROW 1.13 COL 74.86 HELP
          "Consultas relacionadas"
     btReportsJoins AT ROW 1.13 COL 78.86 HELP
          "Relat¢rios relacionados"
     btExit AT ROW 1.13 COL 82.86 HELP
          "Sair"
     btHelp AT ROW 1.13 COL 86.86 HELP
          "Ajuda"
     btRedimensiona AT ROW 11 COL 1
     brDetalhe AT ROW 11.21 COL 1
     rtToolBar-2 AT ROW 1 COL 1
     SPACE(0.00) SKIP(8.46)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWindow ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 19.17
         WIDTH              = 90
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWindow 
/* ************************* Included-Libraries *********************** */

{utp/ut-glob.i}
{btb/btb008za.i0}
{window/window.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWindow
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME fpage0
   FRAME-NAME Size-to-Fit                                               */
/* BROWSE-TAB brDetalhe btRedimensiona fpage0 */
ASSIGN 
       FRAME fpage0:SCROLLABLE       = FALSE.

ASSIGN 
       brDetalhe:COLUMN-RESIZABLE IN FRAME fpage0       = TRUE
       brDetalhe:COLUMN-MOVABLE IN FRAME fpage0         = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWindow)
THEN wWindow:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brDetalhe
/* Query rebuild information for BROWSE brDetalhe
     _START_FREEFORM
open query {&SELF-NAME} for each ttDados no-lock.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE brDetalhe */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fpage0
/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME fpage0:HANDLE
       ROW             = 2.5
       COLUMN          = 1
       HEIGHT          = 8.46
       WIDTH           = 90
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME fpage0:HANDLE
       ROW             = 8.5
       COLUMN          = 76
       HEIGHT          = 1.58
       WIDTH           = 5.43
       HIDDEN          = yes
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {11334785-F3D7-4E44-8007-DFD19CB08003} type: TreeList */
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {2C247F23-8591-11D1-B16A-00C0F0283628} type: ImageList */
      CtrlFrame:MOVE-AFTER(btHelp:HANDLE IN FRAME fpage0).
      CtrlFrame-2:MOVE-AFTER(CtrlFrame).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWindow wWindow
ON END-ERROR OF wWindow
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWindow wWindow
ON WINDOW-CLOSE OF wWindow
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWindow wWindow
ON WINDOW-MAXIMIZED OF wWindow
DO:
    def var dXC    as dec    no-undo.
    def var dYC    as dec    no-undo.
    DEF VAR hlabel AS HANDLE NO-UNDO.
     
  
    assign /**--------------------------------- Tamanho da tela ----------------------------------------------**/
           dXC                                     = wWindow:width-chars  - dWinXC
           dYC                                     = wWindow:height-chars - dWinYC
           dWinXC                                  = wWindow:width-chars
           dWinYC                                  = wWindow:HEIGHT-CHARS
           /**-------------------------------- Tamanho dos frames --------------------------------------------**/
           frame fPage0:width-chars                = wWindow:width-chars
           frame fPage0:height-chars               = wWindow:height-chars
           /**--------------------------------- Objetos em tela ----------------------------------------------**/
           rtToolBar-2:width-chars in frame fPage0 = (rtToolBar-2:width-chars in frame fPage0 + dXC)
           btHelp:column in frame fPage0           = (btHelp:column in frame fPage0 + dXC)
           btExit:column in frame fPage0           = (btExit:column in frame fPage0 + dXC)
           btReportsJoins:column in frame fPage0   = (btReportsJoins:column in frame fPage0 + dXC)
           btQueryJoins:column in frame fPage0     = (btQueryJoins:column in frame fPage0 + dXC)
           btParam:column in frame fPage0          = (btParam:column in frame fPage0 + dXC)
           btFiltro:column in frame fPage0         = (btFiltro:column in frame fPage0 + dXC)
           btExcel:column in frame fPage0          = (btExcel:column in frame fPage0 + dXC)
           brDetalhe:width-chars in frame fPage0   = (brDetalhe:width-chars in frame fPage0 + dXC)
           btRedimensiona:width-chars in frame fPage0 = (btRedimensiona:width-chars in frame fPage0 + dXC)
           btRedimensiona:row in frame fPage0      = (btRedimensiona:row in frame fPage0 + (dYC / 2))
           /**---------------------------------- OCXs --------------------------------------------------------**/
           CtrlFrame:width                         = CtrlFrame:width   + dXC 
           CtrlFrame:HEIGHT                        = CtrlFrame:HEIGHT  + dYC NO-ERROR.

    apply "END-MOVE":U to btRedimensiona in frame fPage0.
          
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWindow wWindow
ON WINDOW-RESTORED OF wWindow
DO:
    def var dXC    as dec    no-undo.
    def var dYC    as dec    no-undo.
    DEF VAR hlabel AS HANDLE NO-UNDO.
   

    assign /**--------------------------------- Tamanho da tela ----------------------------------------------**/
           dXC                                     = dWinXC - wWindow:width-chars
           dYC                                     = dWinYC - wWindow:height-chars
           dWinXC                                  = wWindow:width-chars
           dWinYC                                  = wWindow:HEIGHT-CHARS
           /**--------------------------------- Objetos em tela ----------------------------------------------**/
           rtToolBar-2:width-chars in frame fPage0 = (rtToolBar-2:width-chars in frame fPage0 - dXC)
           btHelp:column in frame fPage0           = (btHelp:column in frame fPage0 - dXC)
           btExit:column in frame fPage0           = (btExit:column in frame fPage0 - dXC)
           btReportsJoins:column in frame fPage0   = (btReportsJoins:column in frame fPage0 - dXC)
           btQueryJoins:column in frame fPage0     = (btQueryJoins:column in frame fPage0 - dXC)
           btParam:column in frame fPage0          = (btParam:column in frame fPage0 - dXC)
           btFiltro:column in frame fPage0         = (btFiltro:column in frame fPage0 - dXC)
           btExcel:column in frame fPage0          = (btExcel:column in frame fPage0 - dXC)
           brDetalhe:width-chars in frame fPage0   = (brDetalhe:width-chars in frame fPage0 - dXC)
           btRedimensiona:width-chars in frame fPage0 = (btRedimensiona:width-chars in frame fPage0 - dXC)
           btRedimensiona:row in frame fPage0      = 11
           /**---------------------------------- OCXs --------------------------------------------------------**/
           CtrlFrame:width                         = CtrlFrame:width  - dXC
           /**-------------------------------- Tamanho dos frames --------------------------------------------**/
           frame fPage0:width-chars                = wWindow:width-chars.
           frame fPage0:height-chars               = wWindow:HEIGHT-CHARS   NO-ERROR.

    apply "END-MOVE":U to btRedimensiona in frame fPage0.

    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAbrir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAbrir wWindow
ON CHOOSE OF btAbrir IN FRAME fpage0
OR CHOOSE OF MENU-ITEM miAbrir  IN MENU mbMain DO:

   /** Troca as barras **/
   assign c-arquivo = replace(c-arquivo, "/", "~\").
   /** Abre caixa para abrir arquivo **/
   SYSTEM-DIALOG GET-FILE c-arquivo
       FILTERS "*.dat" "*.dat",
               "*.*" "*.*"
       ASK-OVERWRITE
       DEFAULT-EXTENSION "dat"
       INITIAL-DIR entry (1, propath)
       USE-FILENAME
       UPDATE l-ok.
   /** Retorno de OK, abre o arquivo **/
   if l-ok then do:
      if search (c-arquivo) <> ? then
         run carregaFiltro (input c-arquivo).
   end.

   apply "ENTRY":U to btAbrir in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExcel wWindow
ON CHOOSE OF btExcel IN FRAME fpage0 /* Planilha */
OR CHOOSE OF MENU-ITEM miExcel  IN MENU mbMain DO:
    if chTreeList:Nodes:Count > 0 then do:
        run piExcel in this-procedure.
    end.
    apply "ENTRY":U to btExcel in frame fPage0.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit wWindow
ON CHOOSE OF btExit IN FRAME fpage0 /* Exit */
OR CHOOSE OF MENU-ITEM miExit IN MENU mbMain DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExpande
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExpande wWindow
ON CHOOSE OF btExpande IN FRAME fpage0
OR CHOOSE OF MENU-ITEM miExpande  IN MENU mbMain DO:
    /** Verifica se existem dados no tree-view **/
    if chTreeList:nodes:Count > 0 then do:
        /** Controla expansÆo **/
        assign l-expande = not l-expande.
        /** Expande os n¡veis abaixo **/
        do i-cont = 1 to chTreeList:Nodes:Count:
           assign chTreeList:Nodes(i-cont):Expanded = l-expande.
        end.
    end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFiltro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFiltro wWindow
ON CHOOSE OF btFiltro IN FRAME fpage0 /* Filtro */
OR CHOOSE OF MENU-ITEM miFiltro  IN MENU mbMain DO:

  assign {&window-name}:sensitive = no.
  run mip/esmi0603a.w (input-output  table ttSelecao).
  assign {&window-name}:sensitive = yes.
  apply "ENTRY":U to btFiltro in frame fPage0.
  find first ttSelecao no-lock no-error.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btHelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp wWindow
ON CHOOSE OF btHelp IN FRAME fpage0 /* Help */
OR CHOOSE OF MENU-ITEM miContents IN MENU mbMain DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btParam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btParam wWindow
ON CHOOSE OF btParam IN FRAME fpage0 /* Parƒmetros */
OR CHOOSE OF MENU-ITEM miSelecao  IN MENU mbMain DO:
    
  assign {&window-name}:sensitive = no.
  run mip/esmi0603c.w (input-output  table ttSelecao).
  assign {&window-name}:sensitive = yes.
  apply "ENTRY":U to btParam in frame fPage0.
  find first ttSelecao no-lock no-error.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btQueryJoins
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btQueryJoins wWindow
ON CHOOSE OF btQueryJoins IN FRAME fpage0 /* Query Joins */
OR CHOOSE OF MENU-ITEM miQueryJoins IN MENU mbMain DO:
    RUN showQueryJoins IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRedimensiona
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRedimensiona wWindow
ON END-MOVE OF btRedimensiona IN FRAME fpage0
DO:
  
    if btRedimensiona:row < 3.5 then
     assign btRedimensiona:row = 3.5.
     
    if btRedimensiona:row > frame fPage0:height then
       assign btRedimensiona:row = frame fPage0:height - 1.
    
    assign CtrlFrame:height = btRedimensiona:row - 2.5 no-error.         
    chTreelist:refresh().

    assign brDetalhe:row = btRedimensiona:row + btRedimensiona:height no-error.
    assign brDetalhe:height  = frame fPage0:height + 0.5
                                 - (btRedimensiona:row) no-error.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRefresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRefresh wWindow
ON CHOOSE OF btRefresh IN FRAME fpage0
OR CHOOSE OF MENU-ITEM miAtualizar  IN MENU mbMain DO:

/** Controla expansÆo **/
if l-expande then assign l-expande = no.
ASSIGN brDetalhe:title in frame fPage0    = "".

session:set-wait-state ("GENERAL").
run piAtualizar in this-procedure.
session:set-wait-state ("").

APPLY "Entry":U TO CtrlFrame.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btReportsJoins
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btReportsJoins wWindow
ON CHOOSE OF btReportsJoins IN FRAME fpage0 /* Reports Joins */
OR CHOOSE OF MENU-ITEM miReportsJoins IN MENU mbMain DO:
    RUN showReportsJoins IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSalvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSalvar wWindow
ON CHOOSE OF btSalvar IN FRAME fpage0
OR CHOOSE OF MENU-ITEM miSalvar  IN MENU mbMain DO:
   /** Troca os tipos de barras **/ 
   assign c-arquivo = replace(c-arquivo, "/", "~\").

   /** Abre caixa de escolha de arquivos **/
   SYSTEM-DIALOG GET-FILE c-arquivo
       FILTERS "*.dat" "*.dat",
               "*.*" "*.*"
       ASK-OVERWRITE
       DEFAULT-EXTENSION "dat"
       INITIAL-DIR entry (1, propath)
       SAVE-AS
       USE-FILENAME
       UPDATE l-ok.
   /** Retorno de OK **/
   if l-ok then do:
      output to value (c-arquivo).
      /** Salva Sele‡Æo **/
      for first ttSelecao:
          export delimiter ";"
                 ttSelecao.periodo-ini   
                 ttSelecao.periodo-fim   
                 ttSelecao.empresa-ini   
                 ttSelecao.empresa-fim   
                 ttSelecao.equipto-ini   
                 ttSelecao.familia-ini
                 ttSelecao.familia-fim
                 ttSelecao.estab-ini     
                 ttSelecao.estab-fim     
                 ttSelecao.ccusto-ini    
                 ttSelecao.ccusto-fim    
                 ttSelecao.tag-ini
                 ttSelecao.tag-fim
                 ttSelecao.sintoma-ini
                 ttSelecao.sintoma-fim
                 ttSelecao.causa-ini  
                 ttSelecao.causa-fim  
                 ttSelecao.lAtivos       
                 ttSelecao.lSuspenso     
                 ttSelecao.lVendido     
                 ttSelecao.lInutilizado
                 ttSelecao.lSomenteCorretivas
                 ttSelecao.iAvalia
                 ttSelecao.nivel-tag
                 ttSelecao.i-disponibilidade
                 .       
      end.
      /** Salva Visäes **/
      for each ttVisao:
          export delimiter ";"
                 ttVisao.dimensao
                 ttVisao.sequencia.
      end.
      output close.
   end.
  
   apply "ENTRY":U to btSalvar in frame fPage0.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame wWindow OCX.MouseDown
PROCEDURE CtrlFrame.TreeList.MouseDown .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Button
    Shift
    X
    Y
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT-OUTPUT PARAMETER p-Button AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-Shift  AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-X      AS DECIMAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-Y      AS DECIMAL NO-UNDO.


assign i-button-tree = p-Button.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame wWindow OCX.NodeClick
PROCEDURE CtrlFrame.TreeList.NodeClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Node
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Node AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE cExpande AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cContrai AS CHARACTER  NO-UNDO.
DEFINE VARIABLE rRow     AS ROWID      NO-UNDO.

{utp/ut-liter.i "Expande"}
assign cExpande = return-value.
{utp/ut-liter.i "Contrai"}
assign cContrai = return-value.

if chTreeList:Nodes:Count > 0 then do:
    /** Sele‡Æo de linha **/
    case i-button-tree:
        /** BotÆo esquerdo do mouse **/
        when 1 then do:
            /** Guarda rowid do tree-view **/
            assign rRow = to-rowid(entry(1,chTreeList:SelectedItem:Tag)).
            /** Busca Registro com rowid **/
            for first bfttDados 
                where rowid(bfttDados) = rRow no-lock:
            end.
            /** Verifica se ele cont‚m filhos **/
            for first ttDados
                where ttDados.cod-dimens-pai = bfttDados.cod-dimensao no-lock:
            end.
            if avail ttDados then do:
                /** Mostra registros no browse **/
                run atualizaBrowse in this-procedure.
            end.



        end.
        /** BotÆo direito do mouse **/
        when 2 then do:
           /** Pop Menu Expande-Contrai **/
           case int(chTreeList:SelectedItem:image):
              when 1  then assign exp-con:sensitive = TRUE.  
              when 2  then assign exp-con:sensitive = TRUE.  
              when 3  then assign exp-con:sensitive = TRUE.  
              when 4  then assign exp-con:sensitive = TRUE.  
              when 5  then assign exp-con:sensitive = TRUE.  
              when 6  then assign exp-con:sensitive = TRUE.  
              when 7  then assign exp-con:sensitive = TRUE.  
              when 8  then assign exp-con:sensitive = TRUE.  
              when 9  then assign exp-con:sensitive = TRUE.  
              when 10 then assign exp-con:sensitive = TRUE.  
              when 11 then assign exp-con:sensitive = TRUE.
              when 12 then assign exp-con:sensitive = TRUE.
              otherwise    assign exp-con:sensitive = true.
           end case.
           assign exp-con:label = if chTreeList:SelectedItem:Expanded = false then cExpande
                                                                              else cContrai.
           run SendMessageA (self:hwnd, 517, 0, 0).            
        end.
    end case.
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brDetalhe
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWindow 


/*--- L¢gica para inicializa‡Æo do programam ---*/
/** Cria‡Æo do pop menu do tree-view **/
create menu pop-menu
    assign popup-only = true
           title = "POPUP":u.
create menu-item exp-con
    assign parent = pop-menu.
create menu-item regua
    assign parent  = pop-menu
           subtype = "RULE":u.

/** Include padrÆo do template **/
{window/mainblock.i}

/** Procedure que manda mensagens para o sistema operacional **/
PROCEDURE SendMessageA EXTERNAL {&USER}:
    DEFINE INPUT PARAMETER hwnd   AS {&HWND}.
    DEFINE INPUT PARAMETER umsg   AS {&INT}.
    DEFINE INPUT PARAMETER wparam AS {&INT}.
    DEFINE INPUT PARAMETER lparam AS LONG. 
END.
/** define procedure externa para execucao do programa de visualizacao do txt **/
PROCEDURE WinExec EXTERNAL "kernel32.dll":
  DEF INPUT  PARAM prg_name                          AS CHARACTER.
  DEF INPUT  PARAM prg_style                         AS SHORT.
END PROCEDURE.

/** Trigger de choose do pop menu "Expande" **/
on choose of exp-con do:
    if chTreeList:nodes:Count > 0 then do:
        run expandeItem (input chTreeList:SelectedItem,
                         input not chTreeList:SelectedItem:Expanded).
    end.
end.

btRedimensiona:load-mouse-pointer ("image~\size2.cur") in frame fPage0.

{abp/abapi001.i2} /** Cria‡Æo de Erros **/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterDestroyInterface wWindow 
PROCEDURE afterDestroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     afterDestroyInterface
  Parameters:  <none>
  Notes:       Override ap¢s fechar a tela
------------------------------------------------------------------------------*/
/** Elimina handle do programa **/
IF VALID-HANDLE (hHandle) THEN
    DELETE PROCEDURE hHandle.

/** Caixa de mensagens **/
{method/showmessage.i3}
chTreeList:TERMINATE.
RELEASE OBJECT chTreeList NO-ERROR.
RELEASE OBJECT chImageList NO-ERROR.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wWindow 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     afterInitializeInterface
  Parameters:  <none>
  Notes:       Override ap¢s inicializa‡Æo da tela
------------------------------------------------------------------------------*/
/** Labels do browse **/
 assign ttDados.cod-oficial:VISIBLE IN BROWSE   brDetalhe   = YES 
        ttDados.desc-dimensao:VISIBLE IN BROWSE   brDetalhe = YES 
        ttDados.tempo-reparo:VISIBLE IN BROWSE   brDetalhe  = YES  
        ttDados.qtd-interv:VISIBLE IN BROWSE   brDetalhe    = YES
        ttDados.nr-ord-produ:VISIBLE IN BROWSE   brDetalhe   = NO  
        ttDados.cd-causa-padr:VISIBLE IN BROWSE  brDetalhe   = NO  
        ttDados.desc-causa:VISIBLE IN BROWSE     brDetalhe   = NO  
        ttDados.cd-sint-padr:VISIBLE IN BROWSE   brDetalhe   = NO  
        ttDados.desc-sintoma:VISIBLE IN BROWSE   brDetalhe   = NO  
        .

assign btExcel:sensitive in frame fPage0 = no.

/** Labels das Colunas do TreeList **/
chTreeList:ColumnHeaders:Add (, , "", 3500, 0).
chTreeList:ColumnHeaders:Add (, , ttDados.qtd-interv:label in browse brDetalhe, 1200, 1).
chTreeList:ColumnHeaders:Add (, , ttDados.tempo-reparo:label in browse brDetalhe, 1300, 1).

/* chTreeList:ColumnHeaders:Add (, , ttDados.qtd-interv:label in browse brDetalhe, 1200, 1).   */
/* chTreeList:ColumnHeaders:Add (, , ttDados.tempo-reparo:label in browse brDetalhe, 1300, 1). */

{utp/ut-liter.i "C¢digo"}
assign ttDados.cod-oficial:label in browse brDetalhe = return-value.

{utp/ut-liter.i "Descri‡Æo"}
assign ttDados.desc-dimensao:label in browse brDetalhe = return-value.

{utp/ut-liter.i "Inverven‡äes"}
assign ttDados.qtd-interv:label in browse brDetalhe = return-value.

{utp/ut-liter.i "Tempo Reparo"}
assign ttDados.tempo-reparo:label in browse brDetalhe = return-value.

{utp/ut-liter.i "N£mero Ordem"}
assign ttDados.nr-ord-produ:label in browse brDetalhe = return-value.

{utp/ut-liter.i "Causa"}
assign ttDados.cd-causa-padr:label in browse brDetalhe = return-value.

{utp/ut-liter.i "Sintoma"}
assign ttDados.cd-sint-padr:label in browse brDetalhe = return-value.

{utp/ut-liter.i "Descri‡Æo"}
assign ttDados.desc-sintoma:label in browse brDetalhe = return-value.

{utp/ut-liter.i "Descri‡Æo"}
assign ttDados.desc-causa:label in browse brDetalhe = return-value.

.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE atualizaBrowse wWindow 
PROCEDURE atualizaBrowse :
/*------------------------------------------------------------------------------
  Purpose:     atualizaBrowse
  Parameters:  <none>
  Notes:       Mostra registros no browse
------------------------------------------------------------------------------*/
DEFINE VARIABLE iSequencia      AS INTEGER    NO-UNDO.
DEFINE VARIABLE cModelo         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cEquipamento    AS CHARACTER  NO-UNDO.

{utp/ut-liter.i "Equipamento:" *}
assign cEquipamento = trim(return-value).
{utp/ut-liter.i "Modelo:" *}
assign cModelo = trim(return-value).

assign iSequencia = 0.
empty temp-table TTexcel.
/** Se tiver filhos, mostra no browse, os filhos **/
if avail ttDados then do:
    run tituloBrowse in this-procedure (ttDados.p-image).
    open query brDetalhe for each  ttDados
                             where ttDados.cod-dimens-pai = bfttDados.cod-dimensao 
                             BY ttDados.nr-ord-produ DESCENDING.

    IF ttDados.sequencia = 3 THEN DO:
        assign ttDados.nr-ord-produ:VISIBLE IN BROWSE   brDetalhe   = YES  
               ttDados.cd-causa-padr:VISIBLE IN BROWSE  brDetalhe   = YES  
               ttDados.desc-causa:VISIBLE IN BROWSE     brDetalhe   = YES  
               ttDados.cd-sint-padr:VISIBLE IN BROWSE   brDetalhe   = YES  
               ttDados.desc-sintoma:VISIBLE IN BROWSE   brDetalhe   = YES  
               ttDados.tempo-reparo:VISIBLE IN BROWSE   brDetalhe   = YES
               ttDados.nr-ord-produ:label in browse brDetalhe      = fnBrowse("nr-ord-produ":U,3)  
               ttDados.cd-causa-padr:label in browse brDetalhe     = fnBrowse("cd-causa-padr":U,3) 
               ttDados.desc-causa:label in browse brDetalhe        = fnBrowse("desc-causa":U,3)    
               ttDados.cd-sint-padr:label in browse brDetalhe      = fnBrowse("cd-sint-padr":U,3) 
               ttDados.desc-sintoma:label in browse brDetalhe      = fnBrowse("desc-sintoma":U,3) 
               ttDados.tempo-reparo:label in browse brDetalhe      = fnBrowse("tempo-reparo":U,3) 
               ttDados.cod-oficial:VISIBLE IN BROWSE   brDetalhe   = NO  
               ttDados.desc-dimensao:VISIBLE IN BROWSE   brDetalhe = NO  
               ttDados.qtd-interv:VISIBLE IN BROWSE   brDetalhe    = NO.  
    END.
    ELSE DO:
        ASSIGN ttDados.cod-oficial:VISIBLE IN BROWSE   brDetalhe   = YES
               ttDados.desc-dimensao:VISIBLE IN BROWSE   brDetalhe = YES
               ttDados.tempo-reparo:VISIBLE IN BROWSE   brDetalhe  = YES
               ttDados.qtd-interv:VISIBLE IN BROWSE   brDetalhe    = YES
               ttDados.tempo-reparo:VISIBLE IN BROWSE   brDetalhe  = YES
               ttDados.cod-oficial:LABEL IN BROWSE   brDetalhe   = fnBrowse("cod-oficial":U,1)
               ttDados.desc-dimensao:LABEL IN BROWSE brDetalhe   = fnBrowse("desc-dimensao":U,1)
               ttDados.tempo-reparo:LABEL IN BROWSE  brDetalhe   = fnBrowse("tempo-reparo":U,1)  
               ttDados.qtd-interv:LABEL IN BROWSE    brDetalhe   = fnBrowse("qtd-interv":U,1)
               ttDados.nr-ord-produ:VISIBLE IN BROWSE   brDetalhe   = NO  
               ttDados.cd-causa-padr:VISIBLE IN BROWSE  brDetalhe   = NO  
               ttDados.desc-causa:VISIBLE IN BROWSE     brDetalhe   = NO  
               ttDados.cd-sint-padr:VISIBLE IN BROWSE   brDetalhe   = NO  
               ttDados.desc-sintoma:VISIBLE IN BROWSE   brDetalhe   = NO  .
                                                                   
    END.                                                     
    
    CASE ttDados.sequencia:
        WHEN 2 THEN
            ASSIGN brDetalhe:title in frame fPage0 = fnLabels(2).
        WHEN 3 THEN DO:
            {utp/ut-liter.i Ordem_Manuten‡Æo}
            ASSIGN brDetalhe:title in frame fPage0 = TRIM(RETURN-VALUE).
        END.
            
    END CASE.
    


    
end.
/** SenÆo, mostra ele mesmo **/
else do:
    run tituloBrowse in this-procedure (bfttDados.p-image).
    open query brDetalhe for each  ttDados
                             where ttDados.cod-dimensao = bfttDados.cod-dimensao.
end.



/** Verifica a sel»’o parametrizada de avalia»’o do usuÿrio para carregar a ttExcel **/
if bfttDados.sequencia = 1 then do:
    for each ttDados no-lock
        where ttDados.cod-dimens-pai = bfttDados.cod-dimensao:
        assign iSequencia = iSequencia + 1.
        create ttExcel.
        assign ttExcel.sequencia        = iSequencia
               ttExcel.cod-oficial      = ttDados.cod-oficial
               ttExcel.desc-dimensao    = ttDados.desc-dimensao
               ttExcel.qtd-interv       = ttDados.qtd-interv
               ttExcel.tempo-reparo     = ttDados.tempo-reparo
               .
    end.
end.



if bfttDados.sequencia = 1 then
   assign btExcel:sensitive    in frame fPage0 = yes.
else assign btExcel:sensitive    in frame fPage0 = no.





return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beforeInitializeInterface wWindow 
PROCEDURE beforeInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     beforeInitializeInterface
  Parameters:  <none>
  Notes:       Override antes de iniciar a tela
------------------------------------------------------------------------------*/
/*Manut. Industrial*/  
{mip/mi9999.i1}


assign chTreeList               = chCtrlFrame:TreeList
       chImageList              = chCtrlFrame-2:ImageList
       chTreeList:ImageList     = chImageList
       CtrlFrame:popup-menu     = pop-menu
       chTreeList:HideSelection = FALSE
       dWinXC                   = wWindow:width-chars
       dWinYC                   = wWindow:height-chars.

ChTreeList:Nodes:Clear().
chTreelist:FullRowSelect = true.

if ChTreeList:Nodes:Count > 0 then
   assign ChTreeList:SelectedItem = ChTreeList:Nodes(1). 

run criattSelecao in this-procedure.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buscaValor wWindow 
PROCEDURE buscaValor :
/*------------------------------------------------------------------------------
  Purpose:     buscaValor
  Parameters:  entrada pValorDimensao = N£mero da dimensÆo escolhida 
  Notes:       Busca os c¢digos e descri‡Æo das visäes escolhidas
------------------------------------------------------------------------------*/
define input parameter pValorDimensao as character format "x(40)" no-undo.

/** Zera vari veis no in¡cio **/
assign vCodigo    = ""
       vDescricao = ""
       iImage     = 0
       rRowid     = ?.

/** Verifica tipo de visÆo (classifica‡Æo) **/
case substring(trim(pValorDimensao),1,2):
    /** Equipamento **/
    when "02":U then do:
        for first equipto fields(cd-equipto descricao)  
            where equipto.cd-equipto = ord-manut.cd-equipto no-lock:
        END.
        if avail equipto then do:
            assign vCodigo    = string(equipto.cd-equipto)
                   vDescricao = equipto.descricao
                   iImage     = 2
                   rRowid     = rowid(equipto).
        END.
        ELSE DO:
           {utp/ut-liter.i "Equipamento nÆo cadastrado" }              
           assign vCodigo    = string(ord-manut.cd-equipto) 
                  vDescricao = RETURN-VALUE                     
                  iImage     = 2                                
                  rRowid     = ?.                   
        END.
    end.
    /** TAG **/
    when "03":U then do:
        for first tag fields(cd-tag descricao)  
            where tag.cd-tag = tt-estrut-tag.cd-tag no-lock:
        end.
        if avail tag then do:
            assign vCodigo    = tag.cd-tag
                   vDescricao = tag.descricao
                   iImage     = 3
                   rRowid     = rowid(tag).
        end.
        ELSE DO:
           {utp/ut-liter.i "Tag nÆo cadastrado" }              
           assign vCodigo     = ord-manut.cd-tag
                   vDescricao = RETURN-VALUE                     
                   iImage     = ?.                   
        END. 
    end.
END CASE.

RETURN  "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carregaFiltro wWindow 
PROCEDURE carregaFiltro :
/*------------------------------------------------------------------------------
  Purpose:     carregaFiltro
  Parameters:  entrada cNome = Nome do arquivo 
  Notes:       Importa arquivo de configura‡äes da consulta 
------------------------------------------------------------------------------*/
define input parameter cNome  as character no-undo.

define variable c-linha as char no-undo.

/** Limpa as visäes **/
EMPTY TEMP-TABLE ttVisao.

input from value (cNome).
/** Busca os parƒmetros e sele‡äes **/
for first ttSelecao exclusive-lock:
    import unformatted c-linha.
    assign c-linha                      = replace (c-linha, chr(34), "")
           ttSelecao.periodo-ini        = date(entry(1,c-linha,";")) 
           ttSelecao.periodo-fim        = date(entry(2,c-linha,";")) 
           ttSelecao.empresa-ini        = entry(3,c-linha,";")
           ttSelecao.empresa-fim        = entry(4,c-linha,";")
           ttSelecao.equipto-ini        = entry(5,c-linha,";") 
           ttSelecao.equipto-fim        = entry(6,c-linha,";") 
           ttSelecao.familia-ini        = entry(7,c-linha,";") 
           ttSelecao.familia-fim        = entry(8,c-linha,";") 
           ttSelecao.estab-ini          = entry(9,c-linha,";")
           ttSelecao.estab-fim          = entry(10,c-linha,";")
           ttSelecao.ccusto-ini         = entry(11,c-linha,";")
           ttSelecao.ccusto-fim         = entry(12,c-linha,";")
           ttSelecao.tag-ini            = entry(13,c-linha,";")
           ttSelecao.tag-fim            = entry(14,c-linha,";")
           ttSelecao.sintoma-ini        = ENTRY(15,c-linha,";")
           ttSelecao.sintoma-fim        = ENTRY(16,c-linha,";")
           ttSelecao.causa-ini          = ENTRY(17,c-linha,";")
           ttSelecao.causa-ini          = ENTRY(18,c-linha,";")
           ttSelecao.lAtivos            = (entry(19,c-linha,";") = "yes")                        
           ttSelecao.lSuspenso          = (entry(20,c-linha,";") = "yes") 
           ttSelecao.lVendido           = (entry(21,c-linha,";") = "yes")  
           ttSelecao.lInutilizado       = (entry(22,c-linha,";") = "yes")  
           ttSelecao.lSomenteCorretivas = (ENTRY(23,c-linha,";") = "yes")
           ttSelecao.iAvalia            = int(entry(24,c-linha,";"))                          
           ttSelecao.nivel-tag          = INT(entry(25,c-linha,";"))
           ttSelecao.i-disponibilidade  = int(entry(26,c-linha,";"))
           
           .
end.

repeat:
    /** Busca as visäes **/
    import unformatted c-linha.
    assign c-linha = replace (c-linha, chr(34), "").
    if num-entries (c-linha, ";") = 2 then do:
        create ttVisao.
        assign ttVisao.dimensao  = entry (1, c-linha, ";")
               ttVisao.sequencia = int (entry (2, c-linha, ";")).
    end.
end.

input close.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load wWindow  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "esmi0603.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
    chCtrlFrame-2 = CtrlFrame-2:COM-HANDLE
    UIB_S = chCtrlFrame-2:LoadControls( OCXFile, "CtrlFrame-2":U)
    CtrlFrame-2:NAME = "CtrlFrame-2":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "esmi0603.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE criaTreeList wWindow 
PROCEDURE criaTreeList :
/*------------------------------------------------------------------------------
  Purpose:     criaTreeList
  Parameters:  <none>
  Notes:       Cria os n¢s do tree-list
------------------------------------------------------------------------------*/
define variable iQtSeq  as integer initial 0 no-undo.
DEFINE VARIABLE i       AS INTEGER           NO-UNDO.
DEFINE VARIABLE c-texto AS CHARACTER         NO-UNDO.
define buffer bfttDados for ttDados.

assign i-cont = 0.
/** Verifica se existem visäes escolhidas **/
for last ttVisao fields(sequencia) no-lock:
    assign iQtSeq = ttVisao.sequencia.
end.
    if iQtSeq = 0 then return.

/** Busca os dados criados **/
for each  ttDados 
    where ttDados.sequencia  <= iQtSeq 
    BY ttDados.cod-dimens-pai
    BY ttDados.qtd-interv DESCENDING:

    /* Filtra quantidade de interven‡äes conforme escolhido pelo usu rio */
    if ttDados.qtd-interv < i-qtd-interv-ini then next.


  if ttDados.sequencia < iVisao then assign lReal = no.
                                else assign lReal = yes.
  /** Conta sequˆncias de linhas **/ 
  assign i-cont = i-cont + 1.
  assign ttDados.seq-tree = i-cont.
  /** define texto do n¢ **/
  if ttDados.p-image = 10 THEN              /** Ano Fabrica‡Æo **/
      ASSIGN c-texto = ttDados.cod-oficial.
  ELSE
      ASSIGN c-texto = ttDados.cod-oficial + " - " + ttDados.desc-dimensao.
      
  /** Inclui primeira dimensÆo **/
  if ttDados.sequencia = 1 then do:
     if lReal then do:
         chTreeList:Nodes:Add (,, "i" + string(i-cont), c-texto, ttDados.p-image).
         chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.qtd-interv,">>>,>>>,>>9")).   
         chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.tempo-reparo,">,>>>,>>9.99")).
         .
     end.
     else do:
         chTreeList:Nodes:Add (,, "i" + string(i-cont), c-texto, ttDados.p-image).
     end.
     /** Guarda chave (rowid) da temp-table no tree-view **/
     assign chTreeList:Nodes:Item ("i" + string(i-cont)):Tag = string(rowid(ttDados)) + ",1,".
     if ttDados.r-rowid <> ? then
        assign chTreeList:Nodes:Item ("i" + string(i-cont)):Tag = string(rowid(ttDados)) + ",1," + string(ttDados.r-rowid).
  end.
  else do:
      /** Busca filhos da visÆo escolhida **/
      for first bfttDados fields(seq-tree)
          where bfttDados.cod-dimensao = ttDados.cod-dimens-pai no-lock:
          /** Inclui linhas das visäes filhas **/
          if lReal then do:
              chTreeList:Nodes:Add ("i" + string(bfttDados.seq-tree), 4, "i" + string(i-cont), c-texto, ttDados.p-image).
              chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.qtd-interv,">>>,>>>,>>9")).   
              chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.tempo-reparo,">,>>>,>>9.99")).
              .                         
          end.
          else do:
              chTreeList:Nodes:Add ("i" + string(bfttDados.seq-tree), 4, "i" + string(i-cont), c-texto, ttDados.p-image).
          end.
         /** Guarda chave (rowid) da temp-table no tree-view **/
         assign chTreeList:Nodes:Item ("i" + string(i-cont)):Tag = string(rowid(ttDados)) + ",1,". 
         if  ttDados.r-rowid <> ? then
             assign chTreeList:Nodes:Item ("i" + string(i-cont)):Tag = string(rowid(ttDados)) + ",2," + string(ttDados.r-rowid).
      end.
  end.
end.

return "OK":U.

end PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE criaTTSelecao wWindow 
PROCEDURE criaTTSelecao :
/*------------------------------------------------------------------------------
  Purpose:     criaTTSelecao
  Parameters:  <none>
  Notes:       Cria a temp-table de sele‡Æo (parƒmetros) do programa
------------------------------------------------------------------------------*/
create ttSelecao.
assign ttSelecao.periodo-ini            = today - 30 
       ttSelecao.periodo-fim            = today     
       ttSelecao.empresa-ini            = ""                         
       ttSelecao.empresa-fim            = "ZZZ"                        
       ttSelecao.equipto-ini            = ""                         
       ttSelecao.equipto-fim            = "ZZZZZZZZZZZZZZZZ"         
       ttSelecao.familia-ini            = ""                         
       ttSelecao.familia-fim            = "ZZZZZZZZ"                 
       ttSelecao.estab-ini              = ""                         
       ttSelecao.estab-fim              = "ZZZ"                      
       ttSelecao.ccusto-ini             = ""                    
       ttSelecao.ccusto-fim             = "ZZZZZZZZ"            
       ttSelecao.tag-ini                = ""
       ttSelecao.tag-fim                = "ZZZZZZZZZZZZZZZZ"
       ttSelecao.sintoma-ini            = ""
       ttSelecao.sintoma-fim            = "ZZZZZZZZ"
       ttSelecao.causa-ini              = ""
       ttSelecao.causa-fim              = "ZZZZZZZZ"
       ttSelecao.lAtivos                = yes             
       ttSelecao.lSuspenso              = NO                 
       ttSelecao.lVendido               = no                    
       ttSelecao.lInutilizado           = no            
       ttSelecao.iAvalia                = 1
       ttSelecao.lSomenteCorretivas     = yes
       ttSelecao.nivel-tag              = 999
       ttSelecao.i-disponibilidade      = 1
       .                   
        
return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE expandeItem wWindow 
PROCEDURE expandeItem :
/*------------------------------------------------------------------------------
  Purpose:     expandeItem
  Parameters:  <none>
  Notes:       Expande o tree-list
------------------------------------------------------------------------------*/
def input param p-node   as com-handle no-undo.
def input param p-expcon as log        no-undo.

def var h-child as com-handle no-undo.
def var i-aux   as int        no-undo.

assign p-node:Expanded = p-expcon
       h-child         = p-node:Child.

do i-aux = 1 to p-node:Children:
    assign h-child = h-child:next no-error.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE imprimeVisao wWindow 
PROCEDURE imprimeVisao :
/*------------------------------------------------------------------------------
  Purpose:     imprimeVisao
  Parameters:  rRow    = Rowid da temp-table
               cCodPai = C¢digo Pai do registro
  Notes:       Cria os dados no excel para os n¡veis acima da visÆo escolhida
------------------------------------------------------------------------------*/
define input parameter rRow    as rowid     no-undo.
define input parameter cCodPai as character no-undo.

/** Verifica se existe n¡vel acima para chamar recursivamente a procedure **/
if can-find(first bfttDados
            where bfttDados.cod-dimensao = cCodPai no-lock) then do:
    for first bfttDados
        where bfttDados.cod-dimensao = cCodPai no-lock:
        run imprimeVisao (input rowid(bfttDados),
                          input bfttDados.cod-dimens-pai).
    end.
end.
/** Busca os dados a serem impressos **/
for first bfttDados
    where rowid(bfttDados) = rRow no-lock:
end.
if avail bfttDados then do:
    /** Cria os dados na temp-table do excel **/
   {mip/esmi0603.i2 bfttDados.p-image}
   create tt-dados-ex.
   assign tt-dados-ex.celula-coluna        = 1
          tt-dados-ex.celula-linha         = i-cont2
          tt-dados-ex.celula-cor-interior  = 27
          tt-dados-ex.celula-fonte-cor     = 1
          tt-dados-ex.celula-fonte-negrito = yes
          tt-dados-ex.celula-valor         = trim(return-value) + ":" +  bfttDados.cod-oficial + "-" + bfttDados.desc-dimensao
          i-cont2                          = i-cont2 + 1.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piAtualizar wWindow 
PROCEDURE piAtualizar :
/*------------------------------------------------------------------------------
  Purpose:     piAtualizar
  Parameters:  <none>
  Notes:       Busca os dados e atualiza o programa
------------------------------------------------------------------------------*/
DEFINE VARIABLE cErro           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cHelpErro       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE grevento        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE evento          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE eqpto           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE greqpto         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE modeqpto        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE soma            AS DECIMAL    NO-UNDO.


/** Busca Parƒmetros **/
find first ttSelecao no-lock no-error.
if not avail ttSelecao then return "NOK":U.

/** Cria visÆo conforme parƒmetro selecionado pelo usu rio **/
run piCriaVisao in this-procedure.

empty temp-table ttDados.
{&OPEN-QUERY-brDetalhe}
/** Limpa o tree-view **/
chTreeList:Nodes:CLEAR().

EMPTY TEMP-TABLE ttDados.
EMPTY TEMP-TABLE tt-estrut-tag.

blk1:
do on stop undo, return "NOK":U:
    /** Acompanhamento **/
    RUN utp/ut-acomp.p PERSISTENT SET hHandle.
    {utp/ut-liter.i "Reincidˆncias Servi‡os"}
    RUN pi-inicializar IN hHandle (trim(return-value)).
    
    FOR EACH equipto NO-LOCK
        WHERE equipto.cd-equipto >= ttSelecao.equipto-ini 
        AND   equipto.cd-equipto <= ttSelecao.equipto-fim:

        CASE equipto.situacao:
            WHEN 1 THEN
                IF ttSelecao.lAtivos      = NO THEN NEXT.
            WHEN 2 THEN
                IF ttSelecao.lSuspenso    = NO THEN NEXT.
            WHEN 3 THEN
                IF ttSelecao.lVendido     = NO THEN NEXT.
            WHEN 4 THEN
                IF ttSelecao.lInutilizado = NO THEN NEXT.
        END CASE.
        
        IF NOT (equipto.cod-estabel >= ttSelecao.estab-ini AND
                equipto.cod-estabel <= ttSelecao.estab-fim) THEN NEXT.

       
        IF NOT (equipto.ep-codigo >= ttSelecao.empresa-ini AND
               equipto.ep-codigo <= ttSelecao.empresa-fim) THEN NEXT.

        
        IF NOT (equipto.fm-equipto >= ttSelecao.familia-ini AND
                equipto.fm-equipto <= ttSelecao.familia-fim) THEN NEXT.

       
        IF NOT (equipto.cd-tag >= ttSelecao.tag-ini AND
                equipto.cd-tag <= ttSelecao.tag-fim) THEN NEXT.

        RUN piTag (INPUT equipto.cd-tag, 
                   INPUT ttSelecao.nivel-tag, 
                   INPUT 999, 
                   OUTPUT TABLE tt-estrut-tag).
        
        FOR FIRST tt-estrut-tag
            WHERE tt-estrut-tag.cod-tipo = ttSelecao.nivel-tag:
        END.
        IF NOT AVAIL tt-estrut-tag THEN NEXT.

        FOR EACH ord-manut NO-LOCK
            WHERE ord-manut.cd-equipto = equipto.cd-equipto: 
            
            IF NOT (ord-manut.dt-manut >= ttSelecao.periodo-ini AND
                    ord-manut.dt-manut <= ttSelecao.periodo-fim) THEN NEXT.
            
            IF ttSelecao.lSomenteCorretivas THEN DO:
                ASSIGN tpManutCorretiva = YES.
                FOR FIRST tipo-manut FIELDS(cd-tipo tipo)
                    WHERE tipo-manut.cd-tipo = ord-manut.cd-tipo:
                    IF tipo-manut.tipo <> 2 THEN 
                        ASSIGN tpManutCorretiva = NO.
                END.
            END.
            
            IF tpManutCorretiva = NO THEN NEXT.

            IF NOT (ord-manut.cd-causa-padr >= ttSelecao.causa-ini AND
                    ord-manut.cd-causa-padr <= ttSelecao.causa-fim) THEN NEXT.

            IF NOT (ord-manut.cd-sint-padr >= ttSelecao.sintoma-ini AND
                    ord-manut.cd-sint-padr <= ttSelecao.sintoma-fim) THEN NEXT.
            
            RUN piCalcHorasMetricas IN THIS-PROCEDURE.
            ASSIGN cCodPai = "".
            FOR EACH ttVisao NO-LOCK:
                RUN buscaValor IN THIS-PROCEDURE (INPUT ttVisao.dimensao).
                RUN piCarregaSequencias.
                IF ttVisao.sequencia = 2 THEN
                    RUN piCriaDetalhe.
            END.
            

        END.
    END.
end. /** do on stop undo **/

FOR EACH ttDados EXCLUSIVE-LOCK
    WHERE ttDados.sequencia = 2:
    ASSIGN ttDados.tempo-reparo = 0.
    FOR EACH bfttDados
        WHERE bfttDados.cod-dimens-pai BEGINS ttDados.cod-dimensao
        AND   bfttDados.sequencia = 3 NO-LOCK:
        ASSIGN ttDados.tempo-reparo = ttDados.tempo-reparo + bfttDados.tempo-reparo.
    END.
END.
FOR EACH ttDados EXCLUSIVE-LOCK
    WHERE ttDados.sequencia = 1:
    ASSIGN ttDados.tempo-reparo = 0.
    FOR EACH bfttDados
        WHERE bfttDados.cod-dimens-pai = ttDados.cod-dimensao
        AND   bfttDados.sequencia = 2 NO-LOCK:
        ASSIGN ttDados.tempo-reparo = ttDados.tempo-reparo + bfttDados.tempo-reparo.
    END.
END.


IF VALID-HANDLE (hHandle) THEN
    DELETE PROCEDURE hHandle.
/** Mostra o tree-list em tela **/
run criaTreeList in this-procedure.



return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCalcHorasMetricas wWindow 
PROCEDURE piCalcHorasMetricas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dias    AS INTEGER      NO-UNDO.
    

    ASSIGN dias  = 0
           horas = 0.

    IF ttSelecao.i-disponibilidade = 1 THEN DO:
        /** Estado da ordem igual a finalizada ou terminada **/
        IF ord-manut.estado > 6 THEN DO:    
            /** Se horas reportadas igual a zero, busca as informa‡äes de  **
             ** previsÆo da manuten‡Æo menos a previsÆo data de t‚rmino    **
             ** vendo assim os dias que a odem ficou em aberto, somando 24 **
             ** cada dia.                                                   **/
             IF ord-manut.tempo-para <= 0 THEN DO:
                ASSIGN dias = ord-manut.dt-prev - ord-manut.dt-prev-manut + 1.
                ASSIGN horas = dias * 24.
             END.
             /** caso tenha horas paradas, essas horas sÆo somandas para o calculo **/
             ELSE ASSIGN horas = ord-manut.tempo-para.
        END. 
        ELSE DO:
            /** Se a data de previsÆo do t‚rmino for menos que hoje, calcula com a **
             ** data da manuten‡Æo menos a data de previsÆo para ter as horas      ** 
             ** calculando 24 horas para cada dia                                  **/
            IF TODAY > ord-manut.dt-prev THEN DO:
                ASSIGN dias = ord-manut.dt-prev - ord-manut.dt-prev-manut + 1.
                ASSIGN horas = dias * 24.
            END.
            /** Caso o dia de previsÆo de t‚rmino seja maior que hoje, a informa‡Æo **
             ** para horas ‚ menos o dia de hoje, inclusive a hora                  **/
            ELSE DO:
                ASSIGN dias = TODAY - ord-manut.dt-prev-manut.
                ASSIGN horas = (dias * 24) + (TIME / 60 / 60).
            END.
        END.
    END.                                            
    ELSE DO:    
       /** Busca as horas reportadas na movto-ggf **/
       FOR EACH movto-ggf FIELDS (horas-report nr-ord-produ tipo-trans)
           WHERE movto-ggf.nr-ord-produ = ord-manut.nr-ord-produ NO-LOCK:
           ASSIGN horas = horas + movto-ggf.horas-report * (IF movto-ggf.tipo-trans = 1 THEN 1 ELSE -1).  
       END.
    END.     
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCarregaSequencias wWindow 
PROCEDURE piCarregaSequencias :
/*------------------------------------------------------------------------------
  Purpose:     piCarregaSequencias
  Parameters:  <none>
  notes:       Carrega a temp-table que ser  mostrada no tree-view e no browse
------------------------------------------------------------------------------*/

/** Busca registro **/
find first ttDados
     where ttDados.cod-dimensao  = (cCodPai + "#" + vCodigo)
     and   ttDados.sequencia     = ttVisao.sequencia exclusive-lock no-error.
/** Se nÆo estiver criado, cria registro **/
if not avail ttDados then do:
   create ttDados.
   assign ttDados.cod-dimensao   = (cCodPai + "#" + vCodigo)  /** C¢digo encadeado para tree-view **/
          ttDados.cod-oficial    = vCodigo              /** C¢digo Original do registro **/
          ttDados.desc-dimensao  = vDescricao           /** Descri‡Æo do c¢digo **/
          ttDados.sequencia      = ttVisao.sequencia    /** Sequˆncia dos dados **/
          ttDados.p-image        = iImage               /** N£mero da imagem do registro **/
          ttDados.r-rowid        = rRowid               /** Rowid do registro para consultas futuras **/
          ttDados.cod-dimens-pai = cCodPai              /** C¢digo do pai do registro **/
          .
end.


assign ttDados.tempo-reparo  =  horas.


assign cCodPai = ttDados.cod-dimensao.

assign ttDados.qtd-interv    = ttDados.qtd-interv + 1 /** Soma a quantidade de interven‡äes **/
    .

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCriaDetalhe wWindow 
PROCEDURE piCriaDetalhe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    CREATE ttDados.
    ASSIGN ttDados.cod-dimens-pai = cCodPai
           ttDados.nr-ord-produ  = ord-manut.nr-ord-produ
           ttDados.cd-causa-padr = ord-manut.cd-causa-padr
           ttDados.cd-sint-padr  = ord-manut.cd-sint-padr
           ttDados.tempo-reparo  = horas
           ttDados.sequencia     = 3
        .
    
    FOR FIRST causa-padrao FIELDS(cd-causa-padr descricao)
        WHERE causa-padrao.cd-causa-padr = ord-manut.cd-causa-padr NO-LOCK:
    END.
    IF AVAIL causa-padrao THEN 
        ASSIGN ttDados.desc-causa = causa-padrao.descricao.
    ELSE DO:
        {utp/ut-liter.i "Causa nÆo cadastrado" } 
        ASSIGN ttDados.desc-causa = TRIM(RETURN-VALUE).
    END.
    
    FOR FIRST sint-padrao FIELDS(cd-sint-padr descricao)
        WHERE sint-padrao.cd-sint-padr = ord-manut.cd-sint-padr NO-LOCK:
    END.
    IF AVAIL sint-padrao THEN 
        ASSIGN ttDados.desc-sintoma = sint-padrao.descricao.
    ELSE DO:
        {utp/ut-liter.i "Sintoma nÆo cadastrado" } 
        ASSIGN ttDados.desc-sintoma = TRIM(RETURN-VALUE).
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCriaTTtag wWindow 
PROCEDURE piCriaTTtag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c-tag AS CHAR NO-UNDO.

    FIND bf-tag NO-LOCK 
        WHERE bf-tag.cd-tag = c-tag NO-ERROR.
    IF NOT AVAIL bf-tag THEN NEXT.

    FIND LAST bf-tt-estrut-tag USE-INDEX ch-sequencia NO-ERROR.

    CREATE tt-estrut-tag.
    ASSIGN tt-estrut-tag.cod-tipo  = bf-tag.cod-tipo
           tt-estrut-tag.cd-tag    = bf-tag.cd-tag
           tt-estrut-tag.descricao = bf-tag.descricao
           tt-estrut-tag.sequencia = IF AVAIL bf-tt-estrut-tag THEN bf-tt-estrut-tag.sequencia + 1 ELSE 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCriaVisao wWindow 
PROCEDURE piCriaVisao :
/*------------------------------------------------------------------------------
  Purpose:     piCriaVisao
  Parameters:  <none>
  Notes:       Cria as visäes se nÆo foram selecionadas 
------------------------------------------------------------------------------*/
empty temp-table ttVisao.


    create ttVisao.
    assign ttVisao.dimensao  = '03 ' + fnLabels(1)
           ttVisao.sequencia = 1.
    create ttVisao.
    assign ttVisao.dimensao  = '02 ' + fnLabels(2)
           ttVisao.sequencia = 2.


return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piExcel wWindow 
PROCEDURE piExcel :
/*------------------------------------------------------------------------------
  Purpose:     piExcel
  Parameters:  <none>
  Notes:       Exporta os dados do programa para o Excel
------------------------------------------------------------------------------*/
    /** Variÿveis de arquivos **/
    define variable c-arq-mod           as character            no-undo.
    define variable c-caminho           as character            no-undo.
    define variable c-arquivo           as character            no-undo.
    define variable c-novo-arquivo      as character            no-undo.
    define variable c-novo-arquivo-resp as character            no-undo.
    DEFINE VARIABLE i-linha-aux         AS INTEGER              NO-UNDO.

    /** Variÿveis para Excel **/
    define variable chExcel             as com-handle           no-undo.
    define variable chPlanilha          as com-handle           no-undo.
    define variable ch-Arquivo          as com-handle           no-undo.
    define variable ch-excel            AS com-handle           no-undo.
    
    
    
   define variable h-prog               as handle               no-undo.
    
   run utp/ut-perc.p persistent set h-prog.
   
   run pi-inicializar in h-prog(input "Importando arquivos", "").

   /** Salva o arquivo temporÿrio e nome do arquivo modelo 
    Valida»’o da existencia do arquivo modelo ² feita no .w  **/
    assign c-caminho = session:temp-directory
           c-arq-mod = "layout~\modelo-yamana-pareto-MI.xls"
           c-novo-arquivo = session:temp-directory + "PARETO_YAMANA-MI.xls".


    assign i-linha-aux = 0.

    /*** Busca planilha padr’o para copiar formata»’o ****/
    assign c-novo-arquivo-resp = search(c-arq-mod).
    if c-novo-arquivo-resp <> ? then do:
       os-copy value(c-novo-arquivo-resp) value(c-novo-arquivo).
    end.
    else do:
       run utp/ut-msgs.p (input "show",
                          input 1332  ,
                          input c-arq-mod).
       return "NOK":U.
    end.

    /* create a new Excel Application object */
    CREATE "Excel.Application" ch-excel.

    /* launch Excel so it is visible to the user */
    ch-excel:Visible = false.

    /* create a new Workbook */
    ch-Arquivo = ch-excel:Workbooks:OPEN(c-novo-arquivo).

    /** Seleciona a pasta n£mero 2 "Informacoes Pareto" **/
    chPlanilha = ch-excel:Sheets:Item(2).

    {utp/ut-liter.i "Equipamento"}
    ASSIGN chPlanilha:range("A" + string(2)):VALUE  = RETURN-VALUE.

    
    assign i-linha-aux = 2.
    for each ttExcel no-lock
          by ttExcel.tempo-reparo descending:
        run pi-inicializar in h-prog(input "Importando arquivos", ttExcel.Sequencia).
        assign  i-linha-aux = i-linha-aux + 1
                chPlanilha:range("A" + string(i-linha-aux)):VALUE  = string(ttExcel.cod-oficial)
                chPlanilha:range("B" + string(i-linha-aux)):VALUE  = string(ttExcel.desc-dimensao)
                chPlanilha:range("C" + string(i-linha-aux)):VALUE  = decimal(ttExcel.tempo-reparo)
                chPlanilha:range("D" + string(i-linha-aux)):VALUE  = string(ttExcel.qtd-interv).
    end.

    /** Seleciona a pasta planilha para exibir para a o usu rio**/
    chPlanilha = ch-excel:Sheets:Item(1).

    assign  chPlanilha:Range("C" + string(2)):Font:Size                = 14
            chPlanilha:Range("C" + string(2)):Font:Bold                = true.



    assign chPlanilha:range("C" + string(2)):VALUE  = string(cAvaliado).
    run pi-finalizar in h-prog.

    /** Deixa vis¡vel para o usu rio **/
    ch-excel:Visible = TRUE. 

    
    release object chExcel          no-error.
    release object chPlanilha       no-error.
    release object ch-Arquivo       no-error.
    release object ch-excel         no-error.
    

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piExplodeTag wWindow 
PROCEDURE piExplodeTag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c-tag       AS CHAR    NO-UNDO.

    FOR FIRST bf-estr-tag NO-LOCK
        WHERE bf-estr-tag.tag-filho = c-tag:

        RUN piCriaTTtag(INPUT bf-estr-tag.tag-pai).

        RUN piExplodeTag(INPUT bf-estr-tag.tag-pai).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piMostraErros wWindow 
PROCEDURE piMostraErros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /** Instancia aplicativo **/
    {method/showmessage.i1}
    /** Mostra caixa com erros **/
    {method/showmessage.i2 &Modal="YES"}
    
    return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piOverSelecao wWindow 
PROCEDURE piOverSelecao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter pTagIni             like ord-manut.cd-tag.
define input parameter pTagFim             like ord-manut.cd-tag.
define input parameter pDataIni         as date format "99/99/9999" no-undo.
define input parameter pDataFim         as date format "99/99/9999" no-undo.
DEFINE INPUT PARAMETER pNivelTag        AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pDisponibilidade AS INTEGER NO-UNDO.

    if avail ttSelecao then do:
        assign ttSelecao.tag-ini            = pTagIni
               ttSelecao.tag-fim            = pTagFim
               ttSelecao.periodo-ini        = pDataIni
               ttSelecao.periodo-fim        = pDataFim
               ttSelecao.iAvalia            = 2
               ttSelecao.nivel-tag          = pNivelTag
               ttSelecao.i-disponibilidade  = pDisponibilidade
            .

        apply "CHOOSE":U to btRefresh in frame fPage0.
    end.    

    return "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piTag wWindow 
PROCEDURE piTag :
DEF INPUT PARAMETER c-tag       AS CHAR    NO-UNDO.
    DEF INPUT PARAMETER i-nivel     AS INTEGER NO-UNDO.
    DEF INPUT PARAMETER i-num-niv   AS INTEGER NO-UNDO.
    DEF OUTPUT PARAMETER TABLE FOR tt-estrut-tag.

    DEF VAR l-considera AS LOG INIT NO         NO-UNDO.
    DEF VAR i-seq       AS INT INIT 0          NO-UNDO.

    EMPTY TEMP-TABLE tt-estrut-tag.

    RUN piCriaTTtag(INPUT c-tag).

    RUN piExplodeTag(INPUT c-tag).

    FOR EACH tt-estrut-tag BREAK BY tt-estrut-tag.sequencia DESC:
        IF tt-estrut-tag.cod-tipo = i-nivel THEN
            ASSIGN l-considera = YES.
        IF l-considera = NO THEN
        DO: 
            DELETE tt-estrut-tag.
            NEXT.
        END.
        ASSIGN i-seq = i-seq + 1.

        ASSIGN tt-estrut-tag.ordem = i-seq.

        ASSIGN i-num-niv = i-num-niv - 1.

        IF i-num-niv = 0 THEN ASSIGN l-considera = NO.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tituloBrowse wWindow 
PROCEDURE tituloBrowse :
/*------------------------------------------------------------------------------
  Purpose:     labelBrowse
  Parameters:  entrada pImagem = N£mero da imagem da visÆo
  Notes:       Coloca o label no browse conforme escolha
------------------------------------------------------------------------------*/
define input parameter pImagem as integer no-undo.
assign ttDados.nr-ord-produ:visible in browse brDetalhe    = yes
       ttDados.cd-causa-padr:visible in browse brDetalhe    = yes
       ttDados.desc-causa:visible in browse brDetalhe    = yes
       ttDados.cd-sint-padr:visible in browse brDetalhe    = yes
       ttDados.desc-sintoma:visible in browse brDetalhe    = yes 
       ttDados.tempo-reparo:visible in browse brDetalhe    = yes
    
    
    /* ttDados.desc-dimensao:visible in browse brDetalhe    = yes              */
    /*    ttDados.qtd-interv:visible in browse brDetalhe       = yes           */
    /*    /* ttDados.qtd-reinc:visible in browse brDetalhe        = yes */     */
    /*    ttDados.tempo-reparo:visible in browse brDetalhe     = yes           */
    /*     /* ttDados.per-interv:visible in browse brDetalhe       = yes */    */
    /*    /* ttDados.un:visible in browse brDetalhe               = yes   */   */
       .
/* if pImagem = 10 then                                                                       */
/*     assign ttDados.desc-dimensao:visible in browse brDetalhe = no.                         */
/*     ASSIGN brDetalhe:title in frame fPage0 = fnLabels(pImagem).                            */
/*     IF AVAIL ttdados THEN DO:                                                              */
/*         if ttDados.sequencia < iVisao then assign lReal = no.                              */
/*                                       else assign lReal = yes.                             */
/*                                                                                            */
/*         if not(lReal) then                                                                 */
/*             assign ttDados.qtd-interv:visible in browse brDetalhe       = NO               */
/*                    /* ttDados.qtd-reinc:visible in browse brDetalhe        = NO */         */
/*                    ttDados.tempo-reparo:visible in browse brDetalhe     = NO               */
/*                    /* ttDados.per-interv:visible in browse brDetalhe       = NO */         */
/*                    /* ttDados.un:visible in browse brDetalhe               = NO */ .       */
/*     END.                                                                                   */

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnBrowse wWindow 
FUNCTION fnBrowse RETURNS CHARACTER
  ( pCampo as character,
    iTipo  as integer ) :
/*------------------------------------------------------------------------------
  Purpose:  fnBrowse
    Notes:  Busca os t¡tulos dos campos do browse
------------------------------------------------------------------------------*/
DEFINE VARIABLE cRetorno AS CHARACTER  NO-UNDO.

if iTipo = 3 then do:
    case pCampo:
        when "nr-ord-produ":U then do:
            {utp/ut-liter.i "Ordem MI"}
        end.
        when "cd-causa-padr":U then do:
            {utp/ut-liter.i "Causa"}
        end.                                                 
        when "desc-causa":U then do:                       
            {utp/ut-liter.i "Descri‡Æo"}                    
        end.                                                   
        when "cd-sint-padr":U then do:                                           
            {utp/ut-liter.i "Sintoma"}
        end.
        when "desc-sintoma":U then do:
            {utp/ut-liter.i "DescricÆo"}
        end.
        when "tempo-reparo":U then do:
            {utp/ut-liter.i "Tempo Reparo"}
        end.
    end case.
    assign cRetorno = trim(return-value).
end.
else do:
    case pCampo:
        when "cod-oficial":U then do:
            {utp/ut-liter.i "C¢digo"}
            ASSIGN cRetorno = RETURN-VALUE.
        end.
        when "desc-dimensao":U then do:
            {utp/ut-liter.i "Descri‡Æo"}
            ASSIGN cRetorno = RETURN-VALUE.
        end.
        when "tempo-reparo":U then do:
            {utp/ut-liter.i "Tempo Reparo"}
            ASSIGN cRetorno = RETURN-VALUE.
        end.
        WHEN "qtd-interv":U THEN DO:
            {utp/ut-liter.i "Interven‡äes"}
            ASSIGN cRetorno = RETURN-VALUE.
        END.


    end case.
end.

RETURN cRetorno.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnLabels wWindow 
FUNCTION fnLabels RETURNS CHARACTER
  ( pImage as int ) :
/*------------------------------------------------------------------------------
  Purpose:  fnLabels
    Notes:  Busca a label da imagem pasada
------------------------------------------------------------------------------*/

DEFINE VARIABLE cRetorno AS CHARACTER  NO-UNDO.

    /** T¡tulo do browse **/
    {mip/esmi0603.i2 pImage}

    assign cRetorno = trim(return-value).

  RETURN cRetorno.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


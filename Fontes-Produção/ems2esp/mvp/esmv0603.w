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
{include/i-prgvrs.i ESMV0603 2.06.00.000}  /*** 010001 ***/
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
&GLOBAL-DEFINE Program        ESMV0603
&GLOBAL-DEFINE Version        2.06.00.001

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
DEFINE VARIABLE de-horas          AS DECIMAL                       NO-UNDO.
DEFINE VARIABLE cTime             AS CHARACTER                     NO-UNDO.
DEFINE VARIABLE dtData            LIKE mmv-ord-manut.dat-prev      NO-UNDO.
DEFINE VARIABLE d-hr-hora-ini     AS DECIMAL FORMAT ">9.99"        NO-UNDO.
DEFINE VARIABLE d-hr-hora-fim     AS DECIMAL FORMAT ">9.99"        NO-UNDO.
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

/** Vari†vel para Apresentar o que est† sendo avaliado no Excel **/
DEFINE VARIABLE cAvaliado AS CHARACTER  NO-UNDO.

{utp/utapi011.i}  /** Gr†fico **/
{mvp/ESMV0603.i}    /** Definiá∆o da temp-table de parÉmetros **/

/** Classificaá∆o **/
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
    FIELD cd-sint-padr          AS CHAR FORMAT "x(08)"
    FIELD cd-causa-padr         AS CHAR FORMAT "x(08)"
    FIELD desc-causa        AS CHARACTER FORMAT "x(50)"
    FIELD desc-sintoma      AS CHARACTER FORMAT "x(50)"
    INDEX codigo IS PRIMARY nr-ord-prod
                            ep-codigo  
                            cod-eqpto   
                            cod-sub-sist
                            cod-evento  
                            val-dat-invrtda-term.

DEF TEMP-TABLE ttAux2
    field ep-codigo             like mab-eqpto.ep-codigo
    field cod-eqpto             like mab-eqpto.cod-eqpto
    field cod-sub-sist          like mmv-tar-ord-manut.cod-sub-sist
    field cod-evento            like mmv-tar-ord-manut.cod-evento
    field nr-ord-prod           like mmv-tar-ord-manut.nr-ord-prod
    field qtd-reinc             AS INTEGER INITIAL 0
    field tempo-reparo          AS DECIMAL INITIAL 0
    field dif-percurso          AS DECIMAL INITIAL 0
    field un                    like mab-model.un
    field cod-grp-eqpto         like mab-eqpto.cod-grp-eqpto
    field cod-model             like mab-eqpto.cod-model
    field cod-estabel           like mab-eqpto.cod-estabel
    field cod-grp-event         like mab-grp-event.cod-grp-event
    field cod-sistema           like mab-sist.cod-sistema
    field vli-ano-fabric        like mab-eqpto.vli-ano-fabric
    field cd-tipo               like tipo-manut.cd-tipo
    field cc-codigo             like mab-eqpto.cc-codigo
    field cd-tag                like mab-eqpto.cd-tag
    FIELD cd-causa-padr     LIKE causa-padrao.cd-causa-padr
    FIELD cd-sint-padr      LIKE sint-padrao.cd-sint-padr
    FIELD desc-causa        AS CHARACTER FORMAT "x(50)"
    FIELD desc-sintoma      AS CHARACTER FORMAT "x(50)"
    INDEX codigo IS PRIMARY UNIQUE nr-ord-prod
                                   ep-codigo   
                                   cod-eqpto   
                                   cod-sub-sist
                                   cod-evento.

/** ttSoma para Equipamento **/
def temp-table ttSoma no-undo
    field ep-codigo         like mmv-tar-ord-manut.ep-codigo
    field cod-eqpto         like mab-eqpto.cod-eqpto
    field cod-model         like mmv-tar-ord-manut.cod-model 
    field vCodigo           as character format "x(25)"
    field tempo-total       as   decimal.

/** ttSoma para o modelo **/
def temp-table ttSomaMod no-undo
    field cod-dimensao      as char format "x(10)"
    field cod-model         like mmv-tar-ord-manut.cod-model 
    field tempo-total       as decimal.

/** Soma para o SubSistema **/
def temp-table ttSomaSubSist
    field cod-dimensao      as char format "x(10)"           
    field cod-model         like mmv-tar-ord-manut.cod-model 
    field cod-sub-sist      like mmv-tar-ord-manut.cod-sub-sist
    field tempo-total       as decimal.                      

/** Soam para apresentar os valores corretos no Evento **/
def temp-table ttSomaEvento
    field cod-dimensao as char format "x(30)"
    field cod-evento   like mmv-tar-ord-manut.cod-evento
    field tempo-total  as decimal.


/** Dados **/
def temp-table ttDados no-undo
    field sequencia         as   integer
    field cod-dimensao      as   character format "x(300)"
    field cod-oficial       as   character format "X(16)"
    field desc-dimensao     as   character format "x(32)"
    field cod-dimens-pai    as   character format "x(300)"
    field qtd-interv        as   INTEGER   format ">>>,>>>,>>9"     LABEL "Intervená‰es"
    field tempo-reparo      as   decimal   format ">,>>>,>>9.99"    LABEL "Tempo Reparo"
    field r-rowid           as   rowid
    field p-image           as   integer
    field seq-tree          as   integer
    FIELD nr-ord-produ      LIKE mmv-ord-manut.nr-ord-produ
    FIELD cd-causa-padr     LIKE causa-padrao.cd-causa-padr
    FIELD cd-sint-padr      LIKE sint-padrao.cd-sint-padr
    FIELD desc-causa        AS CHARACTER FORMAT "x(50)"
    FIELD desc-sintoma      AS CHARACTER FORMAT "x(50)"
    index id is primary unique sequencia    ASCENDING
                               cod-dimensao ASCENDING
                               nr-ord-produ ASCENDING.

define temp-table ttExcel no-undo
    field sequencia     as   integer
    field cod-oficial   as   character format "X(16)"
    field desc-dimensao as   character format "x(300)"
    field qtd-interv    as   integer   format ">>>,>>>,>>9"     LABEL "Intervená‰es" 
    field tempo-reparo  as   decimal   format ">,>>>,>>9.99"    LABEL "Tempo Reparo"
    index id is primary unique sequencia cod-oficial ascending.

define buffer bfttDados  for ttDados.
define buffer bfttDados2 for ttDados.
define buffer bfttVisao  for ttVisao.

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
&Scoped-define FIELDS-IN-QUERY-brDetalhe ttDados.cod-oficial ttDados.desc-dimensao ttDados.qtd-interv /* ttDados.qtd-reinc */ ttDados.tempo-reparo /* ttDados.per-interv */ /* ttDados.un */ ttDados.nr-ord-produ ttDados.cd-causa-padr ttDados.desc-causa ttDados.cd-sint-padr ttDados.desc-sintoma   
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
       MENU-ITEM miQtdInterv    LABEL "&Qtd Intervená‰es" ACCELERATOR "CTRL-Q"
       MENU-ITEM miSelecao      LABEL "Se&leá∆o"       ACCELERATOR "CTRL-P"
       MENU-ITEM miFiltro       LABEL "&Filtro"        ACCELERATOR "CTRL-F"
       MENU-ITEM miClassifica   LABEL "&Classifica"    ACCELERATOR "CTRL-C"
       RULE
       MENU-ITEM miGrafico      LABEL "&Gr†fico"       ACCELERATOR "CTRL-G"
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
     LABEL "ParÉmetros" 
     SIZE 4 BY 1.25 TOOLTIP "ParÉmetros"
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
      ttDados.cod-oficial      width 14
ttDados.desc-dimensao    width 30
ttDados.qtd-interv       width 12
/* ttDados.qtd-reinc        width 12 */
ttDados.tempo-reparo     width 13
/* ttDados.per-interv       width 14 */
/* ttDados.un               width 03 */
ttDados.nr-ord-produ   WIDTH 14
ttDados.cd-causa-padr  WIDTH 8
ttDados.desc-causa     WIDTH 30
ttDados.cd-sint-padr   WIDTH 8
ttDados.desc-sintoma   WIDTH 30
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
          "ParÉmetros"
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
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWindow ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 19.08
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
        /** Controla expans∆o **/
        assign l-expande = not l-expande.
        /** Expande os n°veis abaixo **/
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
  run mvp/ESMV0603a.w (input-output  table ttSelecao).
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
ON CHOOSE OF btParam IN FRAME fpage0 /* ParÉmetros */
OR CHOOSE OF MENU-ITEM miSelecao  IN MENU mbMain DO:
    
  assign {&window-name}:sensitive = no.
  run mvp/ESMV0603c.w (input-output  table ttSelecao).
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

/** Controla expans∆o **/
if l-expande then assign l-expande = no.
ASSIGN brDetalhe:title in frame fPage0    = "".

find first ttSelecao no-lock no-error.
if not avail ttSelecao then return "NOK":U.

session:set-wait-state ("GENERAL").
/** iTipoDispo:
    2 - Horas Apropriadas
    1 - Encerramento OM **/
IF ttSelecao.iTipoDispo = 2 THEN
    RUN piAtualizar IN THIS-PROCEDURE.
ELSE
    RUN piEncerramento IN THIS-PROCEDURE.
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
      /** Salva Seleá∆o **/
      for first ttSelecao:
          export delimiter ";"
                 ttSelecao.periodo-ini   
                 ttSelecao.periodo-fim   
                 ttSelecao.empresa-ini   
                 ttSelecao.empresa-fim   
                 ttSelecao.equipto-ini   
                 ttSelecao.equipto-fim   
                 ttSelecao.grupo-ini     
                 ttSelecao.grupo-fim     
                 ttSelecao.modelo-ini    
                 ttSelecao.modelo-fim    
                 ttSelecao.estab-ini     
                 ttSelecao.estab-fim     
                 ttSelecao.grp-evento-ini
                 ttSelecao.grp-evento-fim
                 ttSelecao.evento-ini    
                 ttSelecao.evento-fim    
                 ttSelecao.ccusto-ini    
                 ttSelecao.ccusto-fim    
                 ttSelecao.sistema-ini   
                 ttSelecao.sistema-fim   
                 ttSelecao.sub-sist-ini  
                 ttSelecao.sub-sist-fim  
                 ttSelecao.tag-ini
                 ttSelecao.tag-fim
                 ttSelecao.lAtivos       
                 ttSelecao.lProprios     
                 ttSelecao.lInativos     
                 ttSelecao.lTerceiros  
                 ttSelecao.iPareto
                 .       
      end.
      /** Salva Vis‰es **/
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
    /** Seleá∆o de linha **/
    case i-button-tree:
        /** Bot∆o esquerdo do mouse **/
        when 1 then do:
            /** Guarda rowid do tree-view **/
            assign rRow = to-rowid(entry(1,chTreeList:SelectedItem:Tag)).
            /** Busca Registro com rowid **/
            for first bfttDados 
                where rowid(bfttDados) = rRow no-lock:
            end.
            /** Verifica se ele contÇm filhos **/
            for first ttDados
                where ttDados.cod-dimens-pai = bfttDados.cod-dimensao no-lock:
            end.
            if avail ttDados then do:
                /** Mostra registros no browse **/
                run atualizaBrowse in this-procedure.
            end.
        end.
        /** Bot∆o direito do mouse **/
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


/*--- L¢gica para inicializaá∆o do programam ---*/
/** Criaá∆o do pop menu do tree-view **/
create menu pop-menu
    assign popup-only = true
           title = "POPUP":u.
create menu-item exp-con
    assign parent = pop-menu.
create menu-item regua
    assign parent  = pop-menu
           subtype = "RULE":u.

/** Include padr∆o do template **/
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

{abp/abapi001.i2} /** Criaá∆o de Erros **/

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
  Notes:       Override ap¢s inicializaá∆o da tela
------------------------------------------------------------------------------*/
/** Labels do browse **/
assign ttDados.cod-oficial:label in browse brDetalhe     = fnBrowse("cod-oficial":U,1)  
       ttDados.desc-dimensao:label in browse brDetalhe   = fnBrowse("desc-dimensao":U,1) 
       ttDados.qtd-interv:label in browse brDetalhe      = fnBrowse("qtd-interv":U,1)    
       ttDados.tempo-reparo:label in browse brDetalhe    = fnBrowse("tempo-reparo":U,1)  
       ttDados.desc-causa:LABEL IN BROWSE      brDetalhe = fnBrowse("descricao":U,1)
       ttDados.desc-sintoma:LABEL IN BROWSE    brDetalhe = fnBrowse("descricao":U,1).

assign btExcel:sensitive in frame fPage0 = no.

/** Labels das Colunas do TreeList **/
chTreeList:ColumnHeaders:Add (, , "", 3500, 0).
chTreeList:ColumnHeaders:Add (, , ttDados.qtd-interv:label in browse brDetalhe, 1200, 1).
chTreeList:ColumnHeaders:Add (, , ttDados.tempo-reparo:label in browse brDetalhe, 1300, 1).
.

RETURN "OK":U.
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
                             AND   ttDados.qtd-interv   >= i-qtd-interv-ini
                             AND   ttDados.qtd-interv   <= i-qtd-interv-fim
                             BY ttDados.qtd-interv DESCENDING.

        IF ttSelecao.iTipoDispo  = 1 THEN DO:
            IF bfttDados.sequencia = 2 THEN DO:
                {utp/ut-liter.i Ordem_Manutená∆o}
                ASSIGN brDetalhe:TITLE IN FRAME fPage0 = TRIM(RETURN-VALUE).

                ASSIGN ttDados.nr-ord-produ:VISIBLE  IN BROWSE brDetalhe = YES
                       ttDados.cd-causa-padr:VISIBLE IN BROWSE brDetalhe = YES
                       ttDados.desc-causa:VISIBLE    IN BROWSE brDetalhe = YES
                       ttDados.cd-sint-padr:VISIBLE  IN BROWSE brDetalhe = YES
                       ttDados.desc-sintoma:VISIBLE  IN BROWSE brDetalhe = YES
                       ttDados.cod-oficial:VISIBLE   IN BROWSE brDetalhe = NO
                       ttDados.desc-dimensao:VISIBLE IN BROWSE brDetalhe = NO
                       ttDados.qtd-interv:VISIBLE    IN BROWSE brDetalhe = NO.
/*                        ttDados.tempo-reparo:VISIBLE  IN BROWSE brDetalhe = NO. */
            END.
            ELSE DO:
                ASSIGN ttDados.nr-ord-produ:VISIBLE IN BROWSE  brDetalhe  = NO
                       ttDados.cd-causa-padr:VISIBLE IN BROWSE brDetalhe  = NO
                       ttDados.desc-causa:VISIBLE IN BROWSE    brDetalhe  = NO
                       ttDados.cd-sint-padr:VISIBLE IN BROWSE  brDetalhe  = NO
                       ttDados.desc-sintoma:VISIBLE IN BROWSE  brDetalhe  = NO
                       ttDados.cod-oficial:VISIBLE IN BROWSE  brDetalhe   = YES
                       ttDados.desc-dimensao:VISIBLE IN BROWSE  brDetalhe = YES
                       ttDados.qtd-interv:VISIBLE IN BROWSE  brDetalhe    = YES
                       ttDados.tempo-reparo:VISIBLE IN BROWSE  brDetalhe  = YES.
            END.
        END.
        IF ttSelecao.iTipoDispo  = 2 THEN DO:                                         
            IF bfttDados.sequencia = 3 THEN DO:                                       
                {utp/ut-liter.i Ordem_Manutená∆o}                                     
                ASSIGN brDetalhe:TITLE IN FRAME fPage0 = TRIM(RETURN-VALUE).          
                                                                                      
                ASSIGN ttDados.nr-ord-produ:VISIBLE  IN BROWSE brDetalhe = YES        
                       ttDados.cd-causa-padr:VISIBLE IN BROWSE brDetalhe = YES        
                       ttDados.desc-causa:VISIBLE    IN BROWSE brDetalhe = YES        
                       ttDados.cd-sint-padr:VISIBLE  IN BROWSE brDetalhe = YES        
                       ttDados.desc-sintoma:VISIBLE  IN BROWSE brDetalhe = YES        
                       ttDados.cod-oficial:VISIBLE   IN BROWSE brDetalhe = NO         
                       ttDados.desc-dimensao:VISIBLE IN BROWSE brDetalhe = NO         
                       ttDados.qtd-interv:VISIBLE    IN BROWSE brDetalhe = NO.        
                       ttDados.tempo-reparo:VISIBLE  IN BROWSE brDetalhe = NO.   
            END.                                                                      
            ELSE DO:                                                                  
                ASSIGN ttDados.nr-ord-produ:VISIBLE IN BROWSE  brDetalhe  = NO        
                       ttDados.cd-causa-padr:VISIBLE IN BROWSE brDetalhe  = NO        
                       ttDados.desc-causa:VISIBLE IN BROWSE    brDetalhe  = NO        
                       ttDados.cd-sint-padr:VISIBLE IN BROWSE  brDetalhe  = NO        
                       ttDados.desc-sintoma:VISIBLE IN BROWSE  brDetalhe  = NO        
                       ttDados.cod-oficial:VISIBLE IN BROWSE  brDetalhe   = YES       
                       ttDados.desc-dimensao:VISIBLE IN BROWSE  brDetalhe = YES       
                       ttDados.qtd-interv:VISIBLE IN BROWSE  brDetalhe    = YES       
                       ttDados.tempo-reparo:VISIBLE IN BROWSE  brDetalhe  = YES.      
            END.                                                                      
        END.                                                                          

end.
/** Sen∆o, mostra ele mesmo **/
else do:
    run tituloBrowse in this-procedure (bfttDados.p-image).
    open query brDetalhe for each ttDados
                            where ttDados.cod-dimensao = bfttDados.cod-dimensao.
end.

assign cAvaliado = substring(ttDados.cod-dimens-pai,2,23).
assign cAvaliado = cModelo + " " + cAvaliado.

EMPTY TEMP-TABLE ttExcel.

/** Verifica a selªío parametrizada de avaliaªío do usuˇrio para carregar a ttExcel **/
if (ttSelecao.iAvalia = 1 and bfttDados.p-image = 4) then do:
    IF ttSelecao.iPareto = 1 THEN DO:
        for each ttDados no-lock
            where ttDados.cod-dimens-pai = bfttDados.cod-dimensao:
            assign iSequencia = iSequencia + 1. 
            create ttExcel.
            assign ttExcel.sequencia        = iSequencia 
                   ttExcel.cod-oficial      = ttDados.cod-oficial     
                   ttExcel.desc-dimensao    = ttDados.desc-dimensao   
                   ttExcel.qtd-interv       = ttDados.qtd-interv.
            IF NUM-ENTRIES(STRING(ttDados.tempo-reparo),",") > 1 THEN
                  ASSIGN ttExcel.tempo-reparo     = DECIMAL(ENTRY(1,STRING(ttDados.tempo-reparo),",") + "," + SUBSTRING((ENTRY(2,STRING(ttDados.tempo-reparo),",")),1,2))  /*ttDados.tempo-reparo*/ .
            ELSE ASSIGN ttExcel.tempo-reparo = ttDados.tempo-reparo.

        END.
    END.
    ELSE DO:
        for each ttDados no-lock
            where ttDados.cod-dimens-pai BEGINS bfttDados.cod-dimensao
            AND   ttDados.sequencia = 3:
            
            FOR FIRST ttExcel 
                WHERE ttExcel.cod-oficial      = ttDados.cod-oficial:
            END.                                                     
            IF AVAIL ttExcel THEN DO:
                ASSIGN  ttExcel.qtd-interv       = ttExcel.qtd-interv + ttDados.qtd-interv 
                        ttExcel.tempo-reparo     = ttExcel.tempo-reparo + ttDados.tempo-reparo.
            END.
            ELSE DO:
                create ttExcel.
                assign ttExcel.cod-oficial      = ttDados.cod-oficial     
                       ttExcel.desc-dimensao    = ttDados.desc-dimensao   
                       ttExcel.qtd-interv       = ttDados.qtd-interv 
                       ttExcel.tempo-reparo     = ttDados.tempo-reparo 
                       .
            END.
        END.
    END.
    
end.

if (ttSelecao.iAvalia = 1 and bfttDados.p-image = 4) then
    assign btExcel:sensitive  in frame fPage0 = yes.
else assign btExcel:sensitive in frame fPage0 = no.

RETURN "OK":U.
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
/*Manut. Mecanica*/  
run cdp/cd9902.p (input 3).
if return-value = "NOK":U then do:    
   {utp/ut-liter.i "Manutená∆o MecÉnica" * r}
   run piCriaErro in this-procedure (input 2082, 
                                     input "EMS":U,
                                     input return-value).
   run piMostraErros.
   return "NOK":U.
end.

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
  Parameters:  entrada pValorDimensao = N£mero da dimens∆o escolhida 
  Notes:       Busca os c¢digos e descriá∆o das vis‰es escolhidas
------------------------------------------------------------------------------*/
define input parameter pValorDimensao as character format "x(40)" no-undo.

/** Zera vari†veis no in°cio **/
assign vCodigo    = ""
       vDescricao = ""
       iImage     = 0
       rRowid     = ?.

/** Verifica tipo de vis∆o (classificaá∆o) **/
case substring(trim(pValorDimensao),1,2):
    /** Equipamento **/
    when "02":U then do:
        if not avail mab-eqpto or mab-eqpto.ep-codigo <> ttAux2.ep-codigo
                               OR mab-eqpto.cod-eqpto <> ttAux2.cod-eqpto then do:
            for first mab-eqpto fields(ep-codigo cod-eqpto cod-model vli-ano-fabric)  
                where mab-eqpto.ep-codigo = ttAux2.ep-codigo
                AND   mab-eqpto.cod-eqpto = ttAux2.cod-eqpto no-lock:
            end.
        end.
        if avail mab-eqpto then do:
            assign vCodigo    = string(mab-eqpto.ep-codigo) + "-" + mab-eqpto.cod-eqpto
                   iImage     = 2
                   rRowid     = rowid(mab-eqpto).
            if not avail mab-model or mab-model.cod-model <> mab-eqpto.cod-model then do:
                for first mab-model fields(des-model cod-model un)
                    where mab-model.cod-model = mab-eqpto.cod-model no-lock:
                end.
            end.
            if avail mab-model then do:
                assign vDescricao = mab-model.des-model.
            end.
        END.
        ELSE DO:
           {utp/ut-liter.i "Equipamento n∆o cadastrado" }              
           assign vCodigo    = string(ttAux2.ep-codigo) + "-" + ttAux2.cod-eqpto   
                   vDescricao = RETURN-VALUE                     
                   iImage     = 2                                
                   rRowid     = rowid(ttAux2).                   
        END.
    end.
    /** Modelo Equipamento **/
    when "04":U then do:
        if not avail mab-model or mab-model.cod-model <> ttAux2.cod-model then do:
            for first mab-model fields(des-model cod-model un)  
                where mab-model.cod-model = ttAux2.cod-model no-lock:
            end.
        end.
        if avail mab-model then do:
            assign vCodigo    = mab-model.cod-model
                   vDescricao = mab-model.des-model
                   iImage     = 4
                   rRowid     = rowid(mab-model).
        end.
        ELSE DO:
           {utp/ut-liter.i "Modelo Equipamento n∆o cadastrado" }              
           assign vCodigo     = ttAux2.cod-model 
                   vDescricao = RETURN-VALUE                     
                   iImage     = 4                                
                   rRowid     = rowid(ttAux2).                   
        END. 
    end.
    when "07":U then do:
        if not avail mab-event or mab-event.cod-evento <> ttAux2.cod-evento then do:
            for first mab-event fields(des-evento cod-evento)  
                where mab-event.cod-evento = ttAux2.cod-evento no-lock:
            end.
        end.
        if avail mab-event then do:
            assign vCodigo    = mab-event.cod-evento
                   vDescricao = mab-event.des-evento
                   iImage     = 7
                   rRowid     = rowid(mab-event).
        end.
        ELSE DO:
           {utp/ut-liter.i "Evento n∆o cadastrado" }              
           assign vCodigo     = ttAux2.cod-evento 
                   vDescricao = RETURN-VALUE                     
                   iImage     = 7                                
                   rRowid     = rowid(ttAux2).                   
        END. 
    end.
    /** Sub-sistema **/
    when "09":U then do:
        if not avail mab-sub-sist or mab-sub-sist.cod-sub-sist <> ttAux2.cod-sub-sist then do:
            for first mab-sub-sist fields(des-sub-sist cod-sub-sist)  
                where mab-sub-sist.cod-sub-sist = ttAux2.cod-sub-sist no-lock:
            end.
        end.
        if avail mab-sub-sist then do:
            assign vCodigo    = mab-sub-sist.cod-sub-sist
                   vDescricao = mab-sub-sist.des-sub-sist
                   iImage     = 9
                   rRowid     = rowid(mab-sub-sist).
        end.
        ELSE DO:
           {utp/ut-liter.i "Sub-sistema n∆o cadastrado" }              
           assign vCodigo     = ttAux2.cod-sub-sist 
                   vDescricao = RETURN-VALUE                     
                   iImage     = 9                                
                   rRowid     = rowid(ttAux2).                   
        END. 
    end.
END CASE.

RETURN  "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buscaValorAux wWindow 
PROCEDURE buscaValorAux :
/*------------------------------------------------------------------------------
  Purpose:     buscaValor
  Parameters:  entrada pValorDimensao = N£mero da dimens∆o escolhida 
  Notes:       Busca os c¢digos e descriá∆o das vis‰es escolhidas
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pValorDimensao AS CHARACTER FORMAT "x(40)" NO-UNDO.

/** Zera vari†veis no in°cio **/
ASSIGN vCodigo    = ""
       vDescricao = ""
       iImage     = 0
       rRowid     = ?.

/** Verifica tipo de vis∆o (classificaá∆o) **/
CASE SUBSTRING(TRIM(pValorDimensao),1,2):
    /** Equipamento **/
    WHEN "02":U THEN DO:
        IF AVAIL mab-eqpto THEN DO:
            ASSIGN vCodigo    = STRING(mab-eqpto.ep-codigo) + "-" + mab-eqpto.cod-eqpto
                   iImage     = 2
                   rRowid     = ROWID(mab-eqpto).
            IF NOT AVAIL mab-model OR mab-model.cod-model <> mab-eqpto.cod-model THEN DO:
                FOR FIRST mab-model FIELDS(des-model cod-model un)
                    WHERE mab-model.cod-model = mab-eqpto.cod-model NO-LOCK:
                END.
            END.
            IF AVAIL mab-model THEN DO:
                ASSIGN vDescricao = mab-model.des-model.
            END.
        END.
        ELSE DO:
           {utp/ut-liter.i "Equipamento_N∆o_Cadastrado" }              
           ASSIGN vCodigo     = "_":U
                  vDescricao = RETURN-VALUE                     
                  iImage     = 2                                
                  rRowid     = ?.                   
        END.
    END.
    /** Modelo Equipamento **/
    WHEN "04":U THEN DO:
        IF NOT AVAIL mab-model OR mab-model.cod-model <> mab-eqpto.cod-model THEN DO:
            FOR FIRST mab-model FIELDS(des-model cod-model un)  
                WHERE mab-model.cod-model = mab-eqpto.cod-model NO-LOCK:
            END.
        END.
        IF AVAIL mab-model THEN DO:
            ASSIGN vCodigo    = mab-model.cod-model
                   vDescricao = mab-model.des-model
                   iImage     = 4
                   rRowid     = ROWID(mab-model).
        END.
        ELSE DO:
           {utp/ut-liter.i "Modelo_Equipamento_N∆o_Cadastrado" }              
           ASSIGN vCodigo    = "_":U 
                  vDescricao = RETURN-VALUE                     
                  iImage     = 4                                
                  rRowid     = ROWID(mab-eqpto).                   
        END. 
    END.
END CASE.

RETURN  "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcHoras wWindow 
PROCEDURE calcHoras :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input  param p-hra-entr like mmv-ord-manut.hra-entr.
define input  param p-hra-prev like mmv-ord-manut.hra-entr.
define input  param p-dat-entr like mmv-ord-manut.dat-entr. 
define input  param p-dat-prev like mmv-ord-manut.dat-prev.
define output param p-total    as decimal no-undo.

    if  substring(p-hra-entr,1,2) <> "":U and 
        substring(p-hra-entr,3,2) <> "":U then
        assign d-hr-hora-ini = ((int(substring(p-hra-entr,1,2)) * 60) + (int(substring(p-hra-entr,3,2)))) / 60.

    if  substring(p-hra-prev,1,2) <> "":U and 
        substring(p-hra-prev,3,2) <> "":U then
        assign d-hr-hora-fim = ((int(substring(p-hra-prev,1,2)) * 60) + (int(substring(p-hra-prev,3,2)))) / 60.

    if  (p-dat-entr <> ?) OR 
        (p-dat-prev <> ?) then do:
        assign p-total = dec(p-dat-prev  - p-dat-entr).

        if  p-total = 0 then do:
            assign p-total = d-hr-hora-fim - d-hr-hora-ini.
        end.
        else do:
            assign p-total = p-total * 24.
            if  d-hr-hora-fim < d-hr-hora-ini then
                assign p-total = p-total - (d-hr-hora-ini - d-hr-hora-fim).
            else assign p-total = p-total + (d-hr-hora-fim - d-hr-hora-ini).
        end.
    end.
    else do:
        assign p-total = d-hr-hora-fim - d-hr-hora-ini.
    end.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carregaFiltro wWindow 
PROCEDURE carregaFiltro :
/*------------------------------------------------------------------------------
  Purpose:     carregaFiltro
  Parameters:  entrada cNome = Nome do arquivo 
  Notes:       Importa arquivo de configuraá‰es da consulta 
------------------------------------------------------------------------------*/
define input parameter cNome  as character no-undo.

define variable c-linha as char no-undo.

/** Limpa as vis‰es **/
EMPTY TEMP-TABLE ttVisao.

input from value (cNome).
/** Busca os parÉmetros e seleá‰es **/
for first ttSelecao exclusive-lock:
    import unformatted c-linha.
    assign c-linha                  = replace (c-linha, chr(34), "")
           ttSelecao.periodo-ini    = date(entry(1,c-linha,";")) 
           ttSelecao.periodo-fim    = date(entry(2,c-linha,";")) 
           ttSelecao.empresa-ini    = entry(3,c-linha,";")
           ttSelecao.empresa-fim    = entry(4,c-linha,";")
           ttSelecao.equipto-ini    = entry(5,c-linha,";") 
           ttSelecao.equipto-fim    = entry(6,c-linha,";") 
           ttSelecao.grupo-ini      = entry(7,c-linha,";") 
           ttSelecao.grupo-fim      = entry(8,c-linha,";") 
           ttSelecao.modelo-ini     = entry(9,c-linha,";") 
           ttSelecao.modelo-fim     = entry(10,c-linha,";")
           ttSelecao.estab-ini      = entry(11,c-linha,";")
           ttSelecao.estab-fim      = entry(12,c-linha,";")
           ttSelecao.grp-evento-ini = entry(13,c-linha,";")
           ttSelecao.grp-evento-fim = entry(14,c-linha,";")
           ttSelecao.evento-ini     = entry(15,c-linha,";")
           ttSelecao.evento-fim     = entry(16,c-linha,";")
           ttSelecao.ccusto-ini     = entry(17,c-linha,";")
           ttSelecao.ccusto-fim     = entry(18,c-linha,";")
           ttSelecao.sistema-ini    = entry(19,c-linha,";")
           ttSelecao.sistema-fim    = entry(20,c-linha,";")
           ttSelecao.sub-sist-ini   = entry(21,c-linha,";")
           ttSelecao.sub-sist-fim   = entry(22,c-linha,";")
           ttSelecao.tag-ini        = entry(23,c-linha,";")
           ttSelecao.tag-fim        = entry(24,c-linha,";")
           ttSelecao.lAtivos        = (entry(27,c-linha,";") = "yes")                        
           ttSelecao.lProprios      = (entry(28,c-linha,";") = "yes") 
           ttSelecao.lInativos      = (entry(29,c-linha,";") = "yes")  
           ttSelecao.lTerceiros     = (entry(30,c-linha,";") = "yes")  
           ttSelecao.iPareto        = int(ENTRY(31,c-linha,";"))
           .
end.

repeat:
    /** Busca as vis‰es **/
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

OCXFile = SEARCH( "esmv0603.wrx":U ).
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
ELSE MESSAGE "esmv0603.wrx":U SKIP(1)
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
/** Verifica se existem vis‰es escolhidas **/
for last ttVisao fields(sequencia) no-lock:
    assign iQtSeq = ttVisao.sequencia.
end.
    if iQtSeq = 0 then return.

/* IF ttSelecao.iTipoDispo = 1 THEN */
    ASSIGN iQtSeq = iQtSeq + 1.

/** Busca os dados criados **/
for each  ttDados 
    where ttDados.sequencia  < iQtSeq 
    BY ttDados.cod-dimens-pai
    BY ttDados.qtd-interv DESCENDING:

    /* Filtra quantidade de intervená‰es conforme escolhido pelo usu†rio */
    if ttDados.qtd-interv < i-qtd-interv-ini then next.
    
    if ttDados.sequencia < iVisao then assign lReal = no.
                                  else assign lReal = yes.
    /** Conta sequàncias de linhas **/ 
    assign i-cont = i-cont + 1.
    assign ttDados.seq-tree = i-cont.
    /** define texto do n¢ **/
    if ttDados.p-image = 10 THEN              /** Ano Fabricaá∆o **/
        ASSIGN c-texto = ttDados.cod-oficial.
    ELSE
        ASSIGN c-texto = ttDados.cod-oficial + " - " + ttDados.desc-dimensao.

    /** Inclui primeira dimens∆o **/
    if ttDados.sequencia = 1 then do:
        if lReal then do:
             chTreeList:Nodes:Add (,, "i" + string(i-cont), c-texto, ttDados.p-image).
             chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.qtd-interv,">>>,>>>,>>9")).   
             chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.tempo-reparo,">,>>>,>>9.99")).
             .
        END.
        else do:
            chTreeList:Nodes:Add (,, "i" + string(i-cont), c-texto, ttDados.p-image).
        END.
        /** Guarda chave (rowid) da temp-table no tree-view **/
        assign chTreeList:Nodes:Item ("i" + string(i-cont)):Tag = string(rowid(ttDados)) + ",1,".
        if ttDados.r-rowid <> ? then
            assign chTreeList:Nodes:Item ("i" + string(i-cont)):Tag = string(rowid(ttDados)) + ",1," + string(ttDados.r-rowid).
    END.
    else do:
        /** Busca filhos da vis∆o escolhida **/
        for first bfttDados fields(seq-tree)
            where bfttDados.cod-dimensao = ttDados.cod-dimens-pai no-lock:
            /** Inclui linhas das vis‰es filhas **/
            if lReal then do:
                chTreeList:Nodes:Add ("i" + string(bfttDados.seq-tree), 4, "i" + string(i-cont), c-texto, ttDados.p-image).
                chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.qtd-interv,">>>,>>>,>>9")).   
                chTreeList:Nodes("i" + string(i-cont)):ListSubItems:Add (, , string(ttDados.tempo-reparo,">,>>>,>>9.99")).
                .                         
            END.
            else do:
                chTreeList:Nodes:Add ("i" + string(bfttDados.seq-tree), 4, "i" + string(i-cont), c-texto, ttDados.p-image).
            END.
            /** Guarda chave (rowid) da temp-table no tree-view **/
            assign chTreeList:Nodes:Item ("i" + string(i-cont)):Tag = string(rowid(ttDados)) + ",1,". 
            if  ttDados.r-rowid <> ? then
                assign chTreeList:Nodes:Item ("i" + string(i-cont)):Tag = string(rowid(ttDados)) + ",2," + string(ttDados.r-rowid).
        END.
    END.
end.

RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE criaTTSelecao wWindow 
PROCEDURE criaTTSelecao :
/*------------------------------------------------------------------------------
  Purpose:     criaTTSelecao
  Parameters:  <none>
  Notes:       Cria a temp-table de seleá∆o (parÉmetros) do programa
------------------------------------------------------------------------------*/
create ttSelecao.
assign ttSelecao.periodo-ini            = today - 30 
       ttSelecao.periodo-fim            = today     
       ttSelecao.empresa-ini            = ""                          
       ttSelecao.empresa-fim            = "ZZZ"                        
       ttSelecao.equipto-ini            = ""                         
       ttSelecao.equipto-fim            = "ZZZZZZZZZZZZZZZZ"         
       ttSelecao.grupo-ini              = ""                         
       ttSelecao.grupo-fim              = "ZZZZZZZZ"                 
       ttSelecao.modelo-ini             = ""                         
       ttSelecao.modelo-fim             = "ZZZZZZZZ"                 
       ttSelecao.estab-ini              = ""                         
       ttSelecao.estab-fim              = "ZZZ"                      
       ttSelecao.grp-evento-ini         = ""                         
       ttSelecao.grp-evento-fim         = "ZZZZZZZZ"         
       ttSelecao.evento-ini             = ""                    
       ttSelecao.evento-fim             = "ZZZZZZZZ"            
       ttSelecao.ccusto-ini             = ""                    
       ttSelecao.ccusto-fim             = "ZZZZZZZZ"            
       ttSelecao.sistema-ini            = ""                    
       ttSelecao.sistema-fim            = "ZZZZZZZZ"            
       ttSelecao.sub-sist-ini           = ""                    
       ttSelecao.sub-sist-fim           = "ZZZZZZZZ"  
       ttSelecao.tag-ini                =  ""
       ttSelecao.tag-fim                =  "ZZZZZZZZZZZZZZZZ"
       ttSelecao.lAtivos                = yes             
       ttSelecao.lProprios              = yes                 
       ttSelecao.lInativos              = no                    
       ttSelecao.lTerceiros             = no            
       ttSelecao.causa-ini              = ""
       ttSelecao.causa-fim              = "ZZZZZZZZ"
       ttSelecao.sintoma-ini            = ""
       ttSelecao.sintoma-fim            = "ZZZZZZZZ"
       ttSelecao.iAvalia                = 1
       ttSelecao.lSomenteCorretivas     = YES
       ttSelecao.iTipoDispo             = 2
       ttSelecao.iPareto                = 1
    .

RETURN "OK":U.
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
  Notes:       Cria os dados no excel para os n°veis acima da vis∆o escolhida
------------------------------------------------------------------------------*/
define input parameter rRow    as rowid     no-undo.
define input parameter cCodPai as character no-undo.

/** Verifica se existe n°vel acima para chamar recursivamente a procedure **/
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
   {mvp/ESMV0603.i2 bfttDados.p-image}
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

/** Cria vis∆o conforme parÉmetro selecionado pelo usu†rio **/
run piCriaVisao in this-procedure.

empty temp-table ttSomaEvento.
empty temp-table ttSomaSubSist.
empty temp-table ttSoma.
empty temp-table ttSomaMod.
empty temp-table ttAux.
empty temp-table ttAux2.
empty temp-table ttDados.
{&OPEN-QUERY-brDetalhe}
/** Limpa o tree-view **/
chTreeList:Nodes:CLEAR().

blk1:
do on stop undo, return "NOK":U:
    /** Acompanhamento **/
    RUN utp/ut-acomp.p PERSISTENT SET hHandle.
    {utp/ut-liter.i "Reincidàncias Serviáos"}
    RUN pi-inicializar IN hHandle (trim(return-value)).
    
    /** Verifica qual Ç a seqÅància m°nima para mostrar as informaá‰es **/ 
    FOR FIRST ttVisao 
        WHERE ttVisao.dimensao = eqpto      /** Equipamento **/         
        OR    ttVisao.dimensao = greqpto    /** Grupo Equipamento **/   
        OR    ttVisao.dimensao = modeqpto:  /** Modelo Equipamento **/ 
        ASSIGN iVisao = ttVisao.sequencia.
    END.

    ASSIGN de-data-ini = DECIMAL( STRING(YEAR(ttSelecao.periodo-ini),"9999") +
                                  STRING(MONTH(ttSelecao.periodo-ini),"99")  +
                                  STRING(DAY(ttSelecao.periodo-ini),"99")    +
                                  "0000" ).
    ASSIGN de-data-fim = DECIMAL( STRING(YEAR(ttSelecao.periodo-fim),"9999") +
                                  STRING(MONTH(ttSelecao.periodo-fim),"99")  +
                                  STRING(DAY(ttSelecao.periodo-fim),"99")    +
                                  "2359" ).

    /** Primeira Fase da Geraá∆o **/
    for each  mab-eqpto
        where mab-eqpto.ep-codigo         >= ttSelecao.empresa-ini
        and   mab-eqpto.ep-codigo         <= ttSelecao.empresa-fim
        and   mab-eqpto.cod-eqpto         >= ttSelecao.equipto-ini
        and   mab-eqpto.cod-eqpto         <= ttSelecao.equipto-fim
        and   mab-eqpto.cod-model         >= ttSelecao.modelo-ini
        and   mab-eqpto.cod-model         <= ttSelecao.modelo-fim
        and   mab-eqpto.cc-codigo         >= ttSelecao.ccusto-ini 
        and   mab-eqpto.cc-codigo         <= ttSelecao.ccusto-fim
        and   mab-eqpto.cd-tag            >= ttSelecao.tag-ini
        and   mab-eqpto.cd-tag            <= ttSelecao.tag-fim
        and   mab-eqpto.cod-estabel       >= ttSelecao.estab-ini
        and   mab-eqpto.cod-estabel       <= ttSelecao.estab-fim
        and   mab-eqpto.cod-grp-eqpto     >= ttSelecao.grupo-ini
        and   mab-eqpto.cod-grp-eqpto     <= ttSelecao.grupo-fim
        and   ((mab-eqpto.idi-tip-propriet = 1 and ttSelecao.lProprios)           /** Pr¢prios  **/
        or     (mab-eqpto.idi-tip-propriet = 2 and ttSelecao.lTerceiros))         /** Terceiros **/
        and   ((mab-eqpto.dat-situacao     = ? and ttSelecao.lAtivos)             /** Ativos    **/
        or     (mab-eqpto.dat-situacao    <> ? and ttSelecao.lInativos)) no-lock, /** Inativos  **/
        FIRST mab-model FIELDS (cod-model un des-model)
        WHERE mab-model.cod-model = mab-eqpto.cod-model NO-LOCK,
        EACH  mmv-tar-ord-manut                                                    
        WHERE mmv-tar-ord-manut.ep-codigo                       = mab-eqpto.ep-codigo         
        AND   mmv-tar-ord-manut.cod-eqpto                       = mab-eqpto.cod-eqpto 
/*         and   mmv-tar-ord-manut.val-hora-reporte                <> 0 */
        AND   mmv-tar-ord-manut.cod-evento                      >= ttSelecao.evento-ini       
        AND   mmv-tar-ord-manut.cod-evento                      <= ttSelecao.evento-fim
        AND   mmv-tar-ord-manut.cod-sub-sist                    >= ttSelecao.sub-sist-ini
        AND   mmv-tar-ord-manut.cod-sub-sist                    <= ttSelecao.sub-sist-fim
        /* and   substring(mmv-tar-ord-manut.cod-livre-1,1,8)     >= ttSelecao.causa-ini */ 
        /* and   substring(mmv-tar-ord-manut.cod-livre-1,1,8)     <= ttSelecao.causa-fim */ 
        /* and   substring(mmv-tar-ord-manut.cod-livre-1,9,8)     >= ttSelecao.sintoma-ini*/
        /* and   substring(mmv-tar-ord-manut.cod-livre-1,9,8)     <= ttSelecao.sintoma-fim*/  NO-LOCK,
        first tipo-manut fields(tipo cd-tipo)
        where tipo-manut.cd-tipo = mmv-tar-ord-manut.cd-tipo
        and   ((ttSelecao.lSomenteCorretivas and tipo-manut.tipo = 2)
        or    (ttSelecao.lSomenteCorretivas = no and tipo-manut.tipo <> 0)) no-lock,
        first mmv-ord-manut
        where mmv-ord-manut.nr-ord-prod           = mmv-tar-ord-manut.nr-ord-prod
        and   mmv-ord-manut.dat-entr  >= ttSelecao.periodo-ini         
        and   mmv-ord-manut.dat-entr  <= ttSelecao.periodo-fim no-lock,
        /* and   mmv-ord-manut.val-dat-invrtda-prev  >= de-data-ini          */
        /* and   mmv-ord-manut.val-dat-invrtda-prev  <= de-data-fim no-lock, */
        FIRST mab-sub-sist
        WHERE mab-sub-sist.cod-sub-sist =  mmv-tar-ord-manut.cod-sub-sist NO-LOCK,
        FIRST mab-event
        WHERE mab-event.cod-evento      = mmv-tar-ord-manut.cod-evento
        AND   mab-event.cod-grp-event   >= ttSelecao.grp-evento-ini
        AND   mab-event.cod-grp-event   <= ttSelecao.grp-evento-fim NO-LOCK:

        /** Acompanhamento **/
        RUN pi-Acompanhar IN hHandle (fnLabels(2) + ": " + STRING(mab-eqpto.ep-codigo) + "-" + mab-eqpto.cod-eqpto).
        
        CREATE ttAux.
        ASSIGN ttAux.ep-codigo            = mab-eqpto.ep-codigo                   
               ttAux.cod-eqpto            = mab-eqpto.cod-eqpto                   
               ttAux.cod-sub-sist         = mmv-tar-ord-manut.cod-sub-sist        
               ttAux.cod-evento           = mmv-tar-ord-manut.cod-evento 
               ttAux.nr-ord-prod          = mmv-tar-ord-manut.nr-ord-prod
               ttAux.val-dat-invrtda-term = mmv-tar-ord-manut.val-dat-invrtda-term
               ttAux.val-hora-reporte     = mmv-tar-ord-manut.val-hora-reporte    
               ttAux.un                   = mab-model.un
               ttAux.val-km-inicial       = mab-eqpto.val-km-inicial
               ttAux.cod-grp-eqpto        = mab-eqpto.cod-grp-eqpto
               ttAux.cod-model            = mab-eqpto.cod-model
               ttAux.cod-estabel          = mab-eqpto.cod-estabel
               ttAux.cod-grp-event        = mab-event.cod-grp-event
               ttAux.cod-sistema          = mab-sub-sist.cod-sistema
               ttAux.vli-ano-fabric       = mab-eqpto.vli-ano-fabric
               ttAux.cd-tipo              = mmv-tar-ord-manut.cd-tipo
               ttAux.cc-codigo            = mab-eqpto.cc-codigo
               ttAux.cd-tag               = mab-eqpto.cd-tag
               ttAux.cd-causa-padr        = SUBSTRING(mmv-ord-manut.cod-livre-1,13,8)
               ttAux.cd-sint-padr         = SUBSTRING(mmv-ord-manut.cod-livre-1,21,8).

         FOR FIRST causa-padrao FIELDS(cd-causa-padr descricao)
             WHERE causa-padrao.cd-causa-padr = SUBSTRING(mmv-ord-manut.cod-livre-1,13,8) NO-LOCK:
         END.
         IF AVAIL causa-padrao THEN
            ASSIGN ttAux.desc-causa = causa-padrao.descricao.
         ELSE
             ASSIGN ttAux.desc-causa = "":U.
         FOR FIRST sint-padrao FIELDS(cd-sint-padr descricao)
             WHERE sint-padrao.cd-sint-padr = SUBSTRING(mmv-ord-manut.cod-livre-1,21,8) NO-LOCK:
         END.
         IF AVAIL sint-padrao THEN
             ASSIGN ttAux.desc-sintoma = sint-padrao.descricao.
         ELSE
             ASSIGN ttAux.desc-sintoma = "":U.
    END.
    /** Segunda fase, fazendo as alteraá∆o para os calculos totais de cada vis∆o **/
    for each ttAux no-lock: 
        FOR FIRST ttAux2
            WHERE ttAux2.cod-model       = ttAux.cod-model   
            AND   ttAux2.cod-sub-sist    = ttAux.cod-sub-sist
            AND   ttAux2.ep-codigo      = ttAux.ep-codigo  /**/
            AND   ttAux2.cod-eqpto      = ttAux.cod-eqpto   /**/
            :
        END.
        IF AVAIL ttAux2 THEN DO:
            ASSIGN soma = soma + ttAux.val-hora-reporte.
        END.
        ELSE ASSIGN soma = ttAux.val-hora-reporte. 
        
        /* if avail ttAux2 then do:                                          */
        /*     if ttAux2.cod-model          = ttAux.cod-model    and         */
        /*        ttAux2.cod-sub-sist       = ttAux.cod-sub-sist then do:    */
        /*         assign soma = soma + ttAux.val-hora-reporte.              */
        /*     end.                                                          */
        /*     else assign soma = ttAux.val-hora-reporte.                    */
        /* end.                                                              */
        /* else assign soma = ttAux.val-hora-reporte.                        */
        if can-find(ttSomaMod where 
                    ttSomaMod.cod-model       = ttAux.cod-model        and
                    ttSomaMod.cod-dimensao    = "#" + ttAux.cod-model) then do:
            for first  ttSomaMod  
                where  ttSomaMod.cod-dimensao    = "#" + ttAux.cod-model and 
                       ttSomaMod.cod-model       = ttAux.cod-model exclusive-lock:
                    assign ttSomaMod.tempo-total = ttSomaMod.tempo-total + ttAux.val-hora-reporte.
            end.
        end.
        else do:
            create ttSomaMod.
            assign ttSomaMod.cod-dimensao  = "#" + ttAux.cod-model
                   ttSomaMod.cod-model     = ttAux.cod-model 
                   ttSomaMod.tempo-total   = ttAux.val-hora-reporte.
        end.
        if can-find(ttSomaSubSist where 
                    ttSomaSubSist.cod-dimensao = "#" + ttAux.cod-model + "#" + ttAux.cod-sub-sist and                             
                    ttSomaSubSist.cod-model    = ttAux.cod-model       and 
                    ttSomaSubSist.cod-sub-sist = ttAux.cod-sub-sist)   then do:
            for first ttSomaSubSist
                where ttSomaSubSist.cod-dimensao = "#" + ttAux.cod-model + "#" + ttAux.cod-sub-sist and
                      ttSomaSubSist.cod-model    = ttAux.cod-model       and
                      ttSomaSubSist.cod-sub-sist = ttAux.cod-sub-sist exclusive-lock:
               assign ttSomaSubSist.tempo-total  = ttSomaSubSist.tempo-total + ttAux.val-hora-reporte.
            end.
        end.
        else do:
            create ttSomaSubSist.
            assign ttSomaSubSist.cod-dimensao = "#" + ttAux.cod-model + "#" + ttAux.cod-sub-sist
                   ttSomaSubSist.cod-model    = ttAux.cod-model   
                   ttSomaSubSist.cod-sub-sist = ttAux.cod-sub-sist
                   ttSomaSubSist.tempo-total  = ttAux.val-hora-reporte.
        end.
        if can-find(ttSomaEvento where                                                                                            
                    ttSomaEvento.cod-dimensao = "#" + ttAux.cod-model + 
                                                "#" + ttAux.cod-sub-sist + "#" + ttAux.cod-evento   and       
                    ttSomaEvento.cod-evento   = ttAux.cod-evento) then do:                                                        
            for first ttSomaEvento                                                                                                
                where ttSomaEvento.cod-dimensao = "#" + ttAux.cod-model + 
                                                  "#" + ttAux.cod-sub-sist + "#" + ttAux.cod-evento and     
                      ttSomaEvento.cod-evento   = ttAux.cod-evento exclusive-lock:                                                
                assign ttSomaEvento.tempo-total = ttSomaEvento.tempo-total + ttAux.val-hora-reporte.                              
            end.                                                                                                                  
        end.                                                                                                                      
        else do:                                                                                                                  
            create ttSomaEvento.                                                                                                  
            assign ttSomaEvento.cod-dimensao = "#" + ttAux.cod-model +     
                                               "#" + ttAux.cod-sub-sist + "#" + ttAux.cod-evento                                  
                   ttSomaEvento.cod-evento   = ttAux.cod-evento                                                                   
                   ttSomaEvento.tempo-total  = ttAux.val-hora-reporte.                                                            
        end.  
        IF NOT AVAIL ttAux2 THEN DO:
            CREATE ttAux2.                                                
            ASSIGN ttAux2.ep-codigo      = ttAux.ep-codigo                
                   ttAux2.cod-eqpto      = ttAux.cod-eqpto                
                   ttAux2.cod-sub-sist   = ttAux.cod-sub-sist             
                   ttAux2.cod-evento     = ttAux.cod-evento               
                   ttAux2.nr-ord-prod    = ttAux.nr-ord-prod              
                   ttAux2.un             = ttAux.un                       
                   ttAux2.tempo-reparo   = soma                           
                   ttAux2.cod-grp-eqpto  = ttAux.cod-grp-eqpto            
                   ttAux2.cod-model      = ttAux.cod-model                
                   ttAux2.cod-estabel    = ttAux.cod-estabel              
                   ttAux2.cod-grp-event  = ttAux.cod-grp-event            
                   ttAux2.cod-sistema    = ttAux.cod-sistema              
                   ttAux2.vli-ano-fabric = ttAux.vli-ano-fabric           
                   ttAux2.cd-tipo        = ttAux.cd-tipo                  
                   ttAux2.cc-codigo      = ttAux.cd-tag                   
                   ttAux2.cd-tag         = ttAux.cc-codigo
                   ttAux2.cd-causa-padr  = ttAux.cd-causa-padr  
                   ttAux2.cd-sint-padr   = ttAux.cd-sint-padr   
                   ttAux2.desc-causa     = ttAux.desc-causa     
                   ttAux2.desc-sintoma   = ttAux.desc-sintoma   
            .
        END.
    END.

    /** Terceira Fase da Geraá∆o **/
    FOR EACH ttAux2:
        /** Primeiro da lista Ç o Pai, ent∆o n∆o contÇm pai **/
        assign cCodPai = "".
        /** Busca vis‰es escolhidas **/
        for each ttVisao no-lock:
            /** Busca c¢digo e descriá∆o dos registros da vis∆o **/
            run buscaValor in this-procedure (input ttVisao.dimensao).
            /** Verifica se encontrou c¢digo, sen∆o busca pr¢ximo **/
            if vCodigo = "" then next.
            /** Carrega os dados  **/
            run piCarregaSequencias in this-procedure.

            IF ttVisao.sequencia = 3 THEN DO:
               IF NOT CAN-FIND(FIRST ttDados
                           WHERE ttDados.cod-dimens-pai = cCodPai 
                           AND   ttDados.sequencia      = 4 
                           AND   ttDados.nr-ord-produ   = ttAux2.nr-ord-prod) THEN DO:
                   CREATE ttDados.
                   ASSIGN ttDados.cod-dimens-pai = cCodPai
                          ttDados.cod-dimensao   = cCodPai
                          ttDados.nr-ord-produ   = ttAux2.nr-ord-prod
                          ttDados.cd-causa-padr  = ttAux2.cd-causa-padr
                          ttDados.cd-sint-padr   = ttAux2.cd-sint-padr
                          ttDados.tempo-reparo   = de-horas
                          ttDados.sequencia      = 4
                          ttDados.qtd-interv     = 1.

               END.
            END.
            


        END.
    END.

    /** Acumula os valores de cada vis∆o **/
    RUN somaVisoes.

end. /** do on stop undo **/


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
    DEFINE VARIABLE i-dias AS INTEGER NO-UNDO.

    /** cTime - Salva o hor†rio atual no formato de horas sem ":" **/
    ASSIGN cTime  = IF mmv-ord-manut.estado > 6 THEN mmv-ord-manut.hra-term
                    ELSE mmv-ord-manut.hra-prev-term
           dtData = IF mmv-ord-manut.estado > 6 THEN mmv-ord-manut.dat-term
                    ELSE mmv-ord-manut.dat-prev-term.

    /** Valida Periodo **/
    IF ttSelecao.periodo-fim < dtData THEN DO:
        ASSIGN dtData = ttSelecao.periodo-fim
               cTime  = "2359":U.
    END.

    RUN calcHoras IN THIS-PROCEDURE (INPUT mmv-ord-manut.hra-entr,
                                     INPUT cTime,
                                     INPUT mmv-ord-manut.dat-entr,
                                     INPUT dtData,
                                     OUTPUT de-horas).

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCarregaSequencias wWindow 
PROCEDURE piCarregaSequencias :
/*------------------------------------------------------------------------------
  Purpose:     piCarregaSequencias
  Parameters:  <none>
  notes:       Carrega a temp-table que ser† mostrada no tree-view e no browse
------------------------------------------------------------------------------*/

/** Busca registro **/
find first ttDados
     where ttDados.cod-dimensao  = (cCodPai + "#" + vCodigo)
     and   ttDados.sequencia     = ttVisao.sequencia exclusive-lock no-error.
/** Se n∆o estiver criado, cria registro **/
if not avail ttDados then do:
   create ttDados.
   assign ttDados.cod-dimensao   = (cCodPai + "#" + vCodigo)  /** C¢digo encadeado para tree-view **/
          ttDados.cod-oficial    = vCodigo              /** C¢digo Original do registro **/
          ttDados.desc-dimensao  = vDescricao           /** Descriá∆o do c¢digo **/
          ttDados.sequencia      = ttVisao.sequencia    /** Sequància dos dados **/
          ttDados.p-image        = iImage               /** N£mero da imagem do registro **/
          ttDados.r-rowid        = rRowid               /** Rowid do registro para consultas futuras **/
          ttDados.cod-dimens-pai = cCodPai              /** C¢digo do pai do registro **/
          .
end.

assign ttDados.tempo-reparo  =  ttAux2.tempo-reparo.

assign cCodPai = ttDados.cod-dimensao.

assign ttDados.qtd-interv    = ttDados.qtd-interv + 1 /** Soma a quantidade de intervená‰es **/
       .

if ttSelecao.iAvali = 2 then do:
    if can-find(ttSoma where                                        
                ttSoma.vCodigo   = ttDados.cod-oficial     and      
                ttSoma.cod-model = ttDados.cod-dimens-pai) then do: 
        for first ttSoma                                            
            where ttSoma.vCodigo   = ttDados.cod-oficial   and      
                  ttSoma.cod-model = ttDados.cod-dimens-pai :        
           assign ttDados.tempo-reparo  = ttSoma.tempo-total.  
                                                                    
        end.                                                        
     end.                                                           
end.
else do:    
    if can-find(ttSomaMod where
                ttSomaMod.cod-dimensao = ttDados.cod-dimensao and 
                ttSomaMod.cod-model    = ttDados.cod-oficial) then do:
        for first ttSomaMod
            where ttSomaMod.cod-dimensao = ttDados.cod-dimensao and
                  ttSomaMod.cod-model    = ttDados.cod-oficial :
           assign ttDados.tempo-reparo  = ttSomaMod.tempo-total. 
        end.
    end.
    if can-find(ttSomaSubSist where
                ttSomaSubSist.cod-dimensao = ttDados.cod-dimensao and
                ttSomaSubSist.cod-sub-sist = ttDados.cod-oficial) then do:
        for first ttSomaSubSist
            where ttSomaSubSist.cod-dimensao = ttDados.cod-dimensao and
                  ttSomaSubSist.cod-sub-sist = ttDados.cod-oficial:
           assign ttDados.tempo-reparo = ttSomaSubSist.tempo-total. 
        end.
    end.                                                       
end.
if can-find(ttSomaEvento where
            ttSomaEvento.cod-dimensao = ttDados.cod-dimensao and
            ttSomaEvento.cod-evento   = ttDados.cod-oficial) then do:
    for first ttSomaEvento
        where ttSomaEvento.cod-dimensao = ttDados.cod-dimensao and
              ttSomaEvento.cod-evento   = ttDados.cod-oficial:
        assign ttDados.tempo-reparo = ttSomaEvento.tempo-total.
    end.
end.

return "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCarregaSequenciasAux wWindow 
PROCEDURE piCarregaSequenciasAux :
/*------------------------------------------------------------------------------
  Purpose:     piCarregaSequencias
  Parameters:  <none>
  notes:       Carrega a temp-table que ser† mostrada no tree-view e no browse
------------------------------------------------------------------------------*/

/** Busca registro **/
FIND FIRST ttDados
    WHERE ttDados.cod-dimensao  = (cCodPai + "#" + vCodigo)
    AND   ttDados.sequencia     = ttVisao.sequencia EXCLUSIVE-LOCK NO-ERROR.

/** Se n∆o estiver criado, cria registro **/
IF NOT AVAIL ttDados THEN DO:
    CREATE ttDados.
    ASSIGN ttDados.cod-dimensao   = (cCodPai + "#" + vCodigo)  /** C¢digo encadeado para tree-view **/
           ttDados.cod-oficial    = vCodigo              /** C¢digo Original do registro **/
           ttDados.desc-dimensao  = vDescricao           /** Descriá∆o do c¢digo **/
           ttDados.sequencia      = ttVisao.sequencia    /** Sequància dos dados **/
           ttDados.p-image        = iImage               /** N£mero da imagem do registro **/
           ttDados.r-rowid        = rRowid               /** Rowid do registro para consultas futuras **/
           ttDados.cod-dimens-pai = cCodPai              /** C¢digo do pai do registro **/
           .
END.

ASSIGN ttDados.tempo-reparo = de-horas.

ASSIGN cCodPai = ttDados.cod-dimensao.

ASSIGN ttDados.qtd-interv = ttDados.qtd-interv + 1 /** Soma a quantidade de intervená‰es **/
    .

    RETURN "OK":U.
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
           ttDados.nr-ord-produ   = mmv-ord-manut.nr-ord-produ
           ttDados.cd-causa-padr  = SUBSTRING(mmv-ord-manut.cod-livre-1,13,8)
           ttDados.cd-sint-padr   = SUBSTRING(mmv-ord-manut.cod-livre-1,21,8)
           ttDados.tempo-reparo   = de-horas
           ttDados.sequencia      = 3
           ttDados.qtd-interv     = 1.
    
    FOR FIRST causa-padrao FIELDS(cd-causa-padr descricao)
        WHERE causa-padrao.cd-causa-padr = SUBSTRING(mmv-ord-manut.cod-livre-1,13,8) NO-LOCK:
    END.
    IF AVAIL causa-padrao THEN 
        ASSIGN ttDados.desc-causa = causa-padrao.descricao.
    ELSE 
        ASSIGN ttDados.desc-causa = "":U.

    FOR FIRST sint-padrao FIELDS(cd-sint-padr descricao)
        WHERE sint-padrao.cd-sint-padr = SUBSTRING(mmv-ord-manut.cod-livre-1,21,8) NO-LOCK:
    END.
    IF AVAIL sint-padrao THEN 
        ASSIGN ttDados.desc-sintoma = sint-padrao.descricao.
    ELSE 
        ASSIGN ttDados.desc-sintoma = "":U.    

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCriaVisao wWindow 
PROCEDURE piCriaVisao :
/*------------------------------------------------------------------------------
  Purpose:     piCriaVisao
  Parameters:  <none>
  Notes:       Cria as vis‰es se n∆o foram selecionadas 
------------------------------------------------------------------------------*/
empty temp-table ttVisao.

create ttVisao.
assign ttVisao.dimensao  = '04 ' + fnLabels(1)
       ttVisao.sequencia = 1.

CREATE ttVisao.
ASSIGN ttVisao.dimensao  = '02 ' + fnLabels(3)
           ttVisao.sequencia = 2.

create ttVisao.
assign ttVisao.dimensao  = '09 ' + fnLabels(2)
       ttVisao.sequencia = 3.
/* create ttVisao.                                */
/* assign ttVisao.dimensao  = '07 ' + fnLabels(7) */
/*        ttVisao.sequencia = 3.                  */

/** Quando 'Encerramento de OM' considera apenas a visoes:
    - Modelo, -Equipamento **/
IF ttSelecao.iTipoDispo = 1 THEN DO:
    FOR EACH ttVisao:
        DELETE ttVisao.
    END.

    CREATE ttVisao.
    ASSIGN ttVisao.dimensao  = '04 ' + fnLabels(1)
           ttVisao.sequencia = 1.
    CREATE ttVisao.
    ASSIGN ttVisao.dimensao  = '02 ' + fnLabels(3)
           ttVisao.sequencia = 2.
END.

RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piEncerramento wWindow 
PROCEDURE piEncerramento :
/*------------------------------------------------------------------------------
  Purpose:     piAtualizar
  Parameters:  <none>
  Notes:       Busca os dados e atualiza o programa
------------------------------------------------------------------------------*/

/** Cria vis∆o conforme parÉmetro selecionado pelo usu†rio **/
RUN piCriaVisao IN THIS-PROCEDURE.

EMPTY TEMP-TABLE ttSomaEvento.
EMPTY TEMP-TABLE ttSomaSubSist.
EMPTY TEMP-TABLE ttSoma.
EMPTY TEMP-TABLE ttSomaMod.
EMPTY TEMP-TABLE ttAux.
EMPTY TEMP-TABLE ttAux2.
EMPTY TEMP-TABLE ttDados.

{&OPEN-QUERY-brDetalhe}
/** Limpa o tree-view **/
chTreeList:Nodes:CLEAR().

blk1:
DO ON STOP UNDO, RETURN "NOK":U:
    /** Acompanhamento **/
    RUN utp/ut-acomp.p PERSISTENT SET hHandle.
    {utp/ut-liter.i "Reincidàncias Serviáos"}
    RUN pi-inicializar IN hHandle (TRIM(RETURN-VALUE)).

/*     FOR LAST ttVisao:                      */
/*         ASSIGN iVisao = ttVisao.sequencia. */
/*     END.                                   */

    /** Primeira Fase da Geraá∆o **/
    FOR EACH  mab-eqpto
        WHERE mab-eqpto.ep-codigo         >= ttSelecao.empresa-ini
        AND   mab-eqpto.ep-codigo         <= ttSelecao.empresa-fim
        AND   mab-eqpto.cod-eqpto         >= ttSelecao.equipto-ini
        AND   mab-eqpto.cod-eqpto         <= ttSelecao.equipto-fim
        AND   mab-eqpto.cod-model         >= ttSelecao.modelo-ini
        AND   mab-eqpto.cod-model         <= ttSelecao.modelo-fim
        AND   mab-eqpto.cc-codigo         >= ttSelecao.ccusto-ini 
        AND   mab-eqpto.cc-codigo         <= ttSelecao.ccusto-fim
        AND   mab-eqpto.cd-tag            >= ttSelecao.tag-ini
        AND   mab-eqpto.cd-tag            <= ttSelecao.tag-fim
        AND   mab-eqpto.cod-estabel       >= ttSelecao.estab-ini
        AND   mab-eqpto.cod-estabel       <= ttSelecao.estab-fim
        AND   mab-eqpto.cod-grp-eqpto     >= ttSelecao.grupo-ini
        AND   mab-eqpto.cod-grp-eqpto     <= ttSelecao.grupo-fim
        AND   ((mab-eqpto.idi-tip-propriet = 1 AND ttSelecao.lProprios)           /** Pr¢prios  **/
        OR     (mab-eqpto.idi-tip-propriet = 2 AND ttSelecao.lTerceiros))         /** Terceiros **/
        AND   ((mab-eqpto.dat-situacao     = ? AND ttSelecao.lAtivos)             /** Ativos    **/
        OR     (mab-eqpto.dat-situacao    <> ? AND ttSelecao.lInativos)) NO-LOCK: /** Inativos  **/
        
        FOR EACH mmv-ord-manut
            WHERE mmv-ord-manut.ep-codigo  = mab-eqpto.ep-codigo
            AND   mmv-ord-manut.cod-eqpto  = mab-eqpto.cod-eqpto
            AND   mmv-ord-manut.dat-entr  >= ttSelecao.periodo-ini 
            AND   mmv-ord-manut.dat-entr  <= ttSelecao.periodo-fim NO-LOCK:

            FOR FIRST tipo-manut FIELDS(tipo cd-tipo)
                WHERE tipo-manut.cd-tipo = mmv-ord-manut.cd-tipo
                AND   ((ttSelecao.lSomenteCorretivas  AND tipo-manut.tipo = 2)
                OR (NOT(ttSelecao.lSomenteCorretivas) AND tipo-manut.tipo <> 0)) NO-LOCK:
            END.
            IF NOT AVAIL tipo-manut THEN NEXT.

            /** Acompanhamento **/
            RUN pi-Acompanhar IN hHandle (fnLabels(2) + ": " + STRING(mab-eqpto.ep-codigo) + "-" + mab-eqpto.cod-eqpto).
  
            RUN piCalcHorasMetricas IN THIS-PROCEDURE.
            ASSIGN cCodPai = "":U.
            FOR EACH ttVisao NO-LOCK:
                RUN buscaValorAux IN THIS-PROCEDURE (INPUT ttVisao.dimensao).
                RUN piCarregaSequenciasAux.
                IF ttVisao.sequencia = 2 THEN
                    RUN piCriaDetalhe.
            END.
        END.
    END.
END. /** do on stop undo **/

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
RUN criaTreeList IN THIS-PROCEDURE.

RETURN "OK":U.
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
    /** Variˇveis de arquivos **/
    define variable c-arq-mod           as character            no-undo.
    define variable c-caminho           as character            no-undo.
    define variable c-arquivo           as character            no-undo.
    define variable c-novo-arquivo      as character            no-undo.
    define variable c-novo-arquivo-resp as character            no-undo.
    DEFINE VARIABLE i-linha-aux         AS INTEGER              NO-UNDO.

    /** Variˇveis para Excel **/
    define variable chExcel             as com-handle           no-undo.
    define variable chPlanilha          as com-handle           no-undo.
    define variable ch-Arquivo          as com-handle           no-undo.
    define variable ch-excel            AS com-handle           no-undo.
    
    
    
   define variable h-prog               as handle               no-undo.
    
   run utp/ut-perc.p persistent set h-prog.
   
   run pi-inicializar in h-prog(input "Importando arquivos", "").

   /** Salva o arquivo temporˇrio e nome do arquivo modelo 
    Validaªío da existencia do arquivo modelo ≤ feita no .w  **/
    assign c-caminho = session:temp-directory
           c-arq-mod = "layout~\modelo-yamana-pareto.xls"
           c-novo-arquivo = session:temp-directory + "PARETO_YAMANA-Frotas.xls".


    assign i-linha-aux = 0.

    /*** Busca planilha padrío para copiar formataªío ****/
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
    
    assign i-linha-aux = 2.
    for each ttExcel no-lock
          by ttExcel.tempo-reparo descending:
        run pi-inicializar in h-prog(input "Importando arquivos", ttExcel.Sequencia).
        assign  i-linha-aux = i-linha-aux + 1
                chPlanilha:range("A" + string(i-linha-aux)):VALUE  = string(ttExcel.cod-oficial)
                chPlanilha:range("B" + string(i-linha-aux)):VALUE  = string(ttExcel.desc-dimensao)
                chPlanilha:range("C" + string(i-linha-aux)):VALUE  = string(ttExcel.tempo-reparo)
                chPlanilha:range("D" + string(i-linha-aux)):VALUE  = decimal(ttExcel.qtd-interv).       
    end.

    /** Seleciona a pasta planilha para exibir para a o usu†rio**/
    chPlanilha = ch-excel:Sheets:Item(1).
    
    assign  chPlanilha:Range("C" + string(2)):Font:Size                = 14
            chPlanilha:Range("C" + string(2)):Font:Bold                = true.



    assign chPlanilha:range("C" + string(2)):VALUE  = string(cAvaliado).
    run pi-finalizar in h-prog.

    /** Deixa vis°vel para o usu†rio **/
    ch-excel:Visible = TRUE. 

    
    release object chExcel          no-error.
    release object chPlanilha       no-error.
    release object ch-Arquivo       no-error.
    release object ch-excel         no-error.
    

return "OK":U.

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
define input parameter pEqpto   like mab-eqpto.cod-eqpto    NO-UNDO.
define input parameter pDataIni as date format "99/99/9999" no-undo.
define input parameter pDataFim as date format "99/99/9999" no-undo.
DEFINE INPUT PARAMETER pTipo    AS INTEGER                  NO-UNDO.

    if avail ttSelecao then do:
        assign ttSelecao.equipto-ini = pEqpto
               ttSelecao.equipto-fim = pEqpto
               ttSelecao.periodo-ini = pDataIni
               ttSelecao.periodo-fim = pDataFim
               ttSelecao.iAvalia     = 1
               ttSelecao.iTipoDispo  = pTipo.

        apply "CHOOSE":U to btRefresh in frame fPage0.
    end.

    return "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piOverSelecaoModelo wWindow 
PROCEDURE piOverSelecaoModelo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter pModelo   like mab-model.cod-model   NO-UNDO.
define input parameter pDataIni as date format "99/99/9999" no-undo.
define input parameter pDataFim as date format "99/99/9999" no-undo.
DEFINE INPUT PARAMETER pTipo    AS INTEGER                  NO-UNDO.

    if avail ttSelecao then do:
        assign ttSelecao.modelo-ini = pModelo
               ttSelecao.modelo-fim = pModelo
               ttSelecao.periodo-ini = pDataIni
               ttSelecao.periodo-fim = pDataFim
               ttSelecao.iAvalia     = 1
               ttSelecao.iTipoDispo  = pTipo.

        apply "CHOOSE":U to btRefresh in frame fPage0.
    end.

    return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SomaVisoes wWindow 
PROCEDURE SomaVisoes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define buffer bfTtDados for ttDados.
    DEFINE VARIABLE somaIntervcoes      AS DECIMAL NO-UNDO.
    DEFINE VARIABLE SomaTempo           AS integer NO-UNDO.

    for each ttDados 
        where ttDados.sequencia < 3 exclusive-lock:
        assign somaIntervcoes = 0
               SomaTempo      = 0.
        for each bfTtDados no-lock
            where bfTtDados.sequencia = 3  
            and   bfTtDados.cod-dimensao begins(ttDados.cod-dimensao) :
/*             if bfttDados.indicador > 0 then do: */
                assign somaIntervcoes = somaIntervcoes + bfttDados.qtd-interv 
                       SomaTempo      = SomaTempo      + bfttDados.tempo-reparo.
/*             end. */
        end.
        assign ttDados.tempo-reparo = SomaTempo
               ttDados.qtd-interv   = somaIntervcoes.
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tituloBrowse wWindow 
PROCEDURE tituloBrowse :
/*------------------------------------------------------------------------------
  Purpose:     labelBrowse
  Parameters:  entrada pImagem = N£mero da imagem da vis∆o
  Notes:       Coloca o label no browse conforme escolha
------------------------------------------------------------------------------*/
define input parameter pImagem as integer no-undo.

assign ttDados.desc-dimensao:visible in browse brDetalhe = yes
       ttDados.qtd-interv:visible    in browse brDetalhe = yes
       /* ttDados.qtd-reinc:visible  in browse brDetalhe = yes */  
       ttDados.tempo-reparo:visible  in browse brDetalhe = yes 
       /* ttDados.per-interv:visible in browse brDetalhe = yes */  
       /* ttDados.un:visible         in browse brDetalhe = yes */
       ttDados.nr-ord-produ:VISIBLE  IN BROWSE brDetalhe = NO
       ttDados.cd-causa-padr:VISIBLE IN BROWSE brDetalhe = NO
       ttDados.desc-causa:VISIBLE    IN BROWSE brDetalhe = NO
       ttDados.cd-sint-padr:VISIBLE  IN BROWSE brDetalhe = NO
       ttDados.desc-sintoma:VISIBLE  IN BROWSE brDetalhe = NO
       .

if pImagem = 10 then
    assign ttDados.desc-dimensao:visible in browse brDetalhe = no.
    ASSIGN brDetalhe:title in frame fPage0 = fnLabels(pImagem).
    IF AVAIL ttdados THEN DO:
        if ttDados.sequencia < iVisao then assign lReal = no.
                                      else assign lReal = yes.

        if not(lReal) then
            assign ttDados.qtd-interv:visible in browse brDetalhe    = NO
                   /* ttDados.qtd-reinc:visible in browse brDetalhe  = NO */
                   ttDados.tempo-reparo:visible in browse brDetalhe  = NO
                   /* ttDados.per-interv:visible in browse brDetalhe = NO */
                   /* ttDados.un:visible in browse brDetalhe         = NO */ 
                   .
    END.

RETURN "OK":U.
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
    Notes:  Busca os t°tulos dos campos do browse
------------------------------------------------------------------------------*/
DEFINE VARIABLE cRetorno AS CHARACTER  NO-UNDO.

if iTipo = 1 then do:
    case pCampo:
        when "cod-oficial":U then do:
            {utp/ut-liter.i "C¢digo"}
        end.
        when "desc-dimensao":U then do:
            {utp/ut-liter.i "Descriá∆o"}
        end.                                                 
        when "qtd-interv":U then do:                       
            {utp/ut-liter.i "Intervená‰es"}                    
        end.                                                   
        when "qtd-reinc":U then do:                                           
            {utp/ut-liter.i "Reincidàncias"}
        end.
        when "tempo-reparo":U then do:
            {utp/ut-liter.i "Tempo Reparo"}
        end.
        when "per-interv":U then do:
            {utp/ut-liter.i "Per°odo Entre Interv"}
        end.
        when "un":U then do:
            {utp/ut-liter.i "UN"}
        end.
        when "causa":U then do:
            {utp/ut-liter.i "Causa"}
        end.
        when "sintoma":U then do:
            {utp/ut-liter.i "Sintoma"}
        end.
        when "descricao":U then do:
            {utp/ut-liter.i "Descriá∆o"}
        end.
    end case.
    assign cRetorno = trim(return-value).
end.
else do:
    case pCampo:
        when "cod-oficial":U then do:
            assign cRetorno = bfttDados2.cod-oficial.
        end.
        when "desc-dimensao":U then do:
            assign cRetorno = bfttDados2.desc-dimensao.
        end.
        when "qtd-interv":U then do:
            assign cRetorno = string(bfttDados2.qtd-interv,">>>,>>>,>>9").
        end.
        when "tempo-reparo":U then do:
            assign cRetorno = string(bfttDados2.tempo-reparo,">,>>>,>>9.99").
        end.

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

    /** T°tulo do browse **/
    {mvp/ESMV0603.i2 pImage}

    assign cRetorno = trim(return-value).

  RETURN cRetorno.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


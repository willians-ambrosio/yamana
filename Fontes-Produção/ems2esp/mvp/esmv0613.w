&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          mgadm            PROGRESS
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
def buffer empresa     for ems2cadme.empresa.

{include/i-prgvrs.i ESMV0613 2.00.00.001}  /*** 010014 ***/
/********************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESMV0613
&GLOBAL-DEFINE Version        2.00.00.001

&GLOBAL-DEFINE WindowType     Master/Detail

&GLOBAL-DEFINE Folder         no

&GLOBAL-DEFINE page0Widgets   btQueryJoins btReportsJoins btExit btHelp ~
                              btAbrir btSalvar btRefresh btFiltro btParam btVisao btExpande ~
                              btImprimir btRedimensiona brDetalhe ~
                              btEqpto btEvento btPneu btGeracao btHistorico ~
                              btHoras btExcel btTXT btPlano ~
                              btLegenda 
DEFINE VARIABLE DtIni   AS DATE.

{utp/ut-glob.i}

/* Local Variable Definitions ---                                       */
define variable c-arquivo     as character                no-undo.
define variable cUN           as character                no-undo.
define variable c-origem-dad  as character format "x(25)" no-undo.
define variable c-tip-manut   as character format "x(40)" no-undo.
define variable c-marcado     as character format "x"     no-undo.
def    var      c-legenda     as char      label "Legenda" format "x(12)" no-undo.
/** Busca Valor **/
define variable rRowid        as rowid                    no-undo.
define variable cCodPai       as character format "x(20)" no-undo.
define variable vCodigo       as character format "x(20)" no-undo.
define variable vDescricao    as character format "x(32)" no-undo.
define variable iImage        as integer                  no-undo.
define variable cTag          as character                no-undo.
define variable cCCCusto      as character                no-undo.
/** OCX **/
define variable chTreeView    as com-handle               no-undo.
define variable chImageList   as com-handle               no-undo.
define variable chTimer       as com-handle               no-undo.
/** Controles **/
define variable l-expande     as logical init no          no-undo.
define variable l-ok          as logical                  no-undo.
define variable c-tag-tree    as character                no-undo.
define variable i-cont        as integer                  no-undo.
define variable iVisao        as integer                  no-undo.
define variable iVisaoEqpto   as integer                  no-undo.
define variable i-result      as integer                  no-undo.
define variable i-button-tree as integer  initial 1       no-undo.
define variable i-atualiza    as integer  initial 1       no-undo.
define variable dt-consulta   as date format "99/99/9999" no-undo.
define variable i-empresa     as character                no-undo.
define variable c-eqpto       as character                no-undo.
/** Pop-Menu **/
define variable pop-menu      as widget-handle            no-undo.
define variable exp-con       as widget-handle            no-undo.
define variable detalhe       as widget-handle            no-undo.
define variable regua         as widget-handle            no-undo.
/*** Window ***/
define variable dWinXC        as decimal                  no-undo.
define variable dWinYC        as decimal                  no-undo.
/** Busca Informaá‰es (C†lculo) **/
define variable lVencer        as logical                  no-undo.
define variable lVencido       as logical                  no-undo.
DEFINE VARIABLE lGarantia      AS LOGICAL                  NO-UNDO.
define variable lReal          as logical                  no-undo.
define variable iOrig          as integer                  no-undo.
define variable iOrigemTarefa  as integer                  no-undo.
define variable deReal         as decimal                  no-undo.
define variable dePadrao       as decimal                  no-undo.
define variable deKMMedio      as decimal                  no-undo.
define variable deKMMedioSec   as decimal                  no-undo.
define variable cCodSetor      as character                no-undo.
define variable cCodOficina    as character                no-undo.
define variable iOrigOficina   as integer                  no-undo. /** 1 - OM / 2 - Outros **/
define variable cCodPlanejad   as character                no-undo.
define variable cCodUsuario    as character                no-undo.
define variable cCodSubSist    as character                no-undo.
define variable dt-dat-vencto  as date format "99/99/9999" no-undo.
define variable dt-dat-atualiz as date format "99/99/9999" no-undo.
/** Excel **/
define variable i-lin          as integer                  no-undo.
define variable i-lin2         as integer                  no-undo.
define variable i-col          as integer                  no-undo.
define variable i-impressao    as integer                  no-undo.
define variable hHandle        as handle                   no-undo.
define variable c-colunas      as character                no-undo.
/** TXT **/
define variable cCaminho       as character format "x(300)" no-undo. /*Indica o caminho */
define variable cDelimitador   as character format "x(01)"  no-undo. /*Indica o tipo de delimitador */
define variable lCabecalho     as logical   initial no      no-undo. /*Indica se gera cabeáalho */
define variable cTituloTxt     as character format "x(256)" no-undo.
define variable cDescPai       as character format "x(256)" no-undo.
/** Data Hora Invertida Atual **/
define variable deInvertidaAtual   as decimal format "999999999999" no-undo.
define variable deInvertidaAVencer as decimal format "999999999999" no-undo.
define variable c-hora             as character format "99:99:99"   no-undo.
define variable iSeq-tar-plano      as int                           no-undo.

/** Classificaá∆o **/
def temp-table ttVisao no-undo
    field dimensao  as character
    field sequencia as integer
    index codigo is primary unique sequencia.

{mvp/esmv0613.i3 ttDados} /** Definiá∆o da ttDados **/

define buffer ccusto     for emsuni.ccusto.
define buffer bfttDados  for ttDados.
define buffer bfttDados2 for ttDados.
DEFINE BUFFER bfOrdem    FOR mmv-ord-manut.  /*criado por xavier na UC*/

{mvp/esmv0613.i}  /** Definiá∆o da temp-table de parÉmetros **/

/** Criaá∆o das tarefas **/
{frbo/bofr073.i ttTarefa} /** Definiá∆o da TT de tarefas da OM **/
define temp-table ttTarefaAux no-undo like ttTarefa.

/**Temp-table para dados do Excell**/
define temp-table tt-dados-ex no-undo
    field arquivo-num                   as integer format ">9"     initial 1
    field planilha-num                  as integer format ">9"
    field celula-coluna                 as integer format ">>>>9"
    field celula-linha                  as integer format ">>>>9"
    field celula-cor-interior           as integer format ">9"     initial 58 /* None */
    field celula-formato                as char    format "x(255)"
    field celula-formula                as char    format "x(255)"
    field celula-alinhamento-horizontal as integer format "9"      initial 4 /* Left */
    field celula-alinhamento-vertical   as integer format "9"      initial 1 /* Bottom */
    field celula-valor                  as char    format "x(255)"
    field celula-fonte-nome             as char    format "x(255)" initial "Times New Roman"
    field celula-fonte-tamanho          as integer format ">9"     initial 10
    field celula-fonte-negrito          as logical                 initial no
    field celula-fonte-italico          as logical                 initial no
    field celula-fonte-sublinhado       as integer format "9"      initial 3 /* None */
    field celula-fonte-cor              as integer format ">9"     initial 57 /* Automatic */
    field celula-tipo-borda-sup         as integer format "9"      initial 7 /* None */
    field celula-tipo-borda-inf         as integer format "9"      initial 7 /* None */
    field celula-tipo-borda-esq         as integer format "9"      initial 7 /* None */
    field celula-tipo-borda-dir         as integer format "9"      initial 7 /* None */
    index tt-dados-pri is unique primary
          arquivo-num
          planilha-num
          celula-coluna
          celula-linha.

define temp-table tt-tarefa-plano no-undo
    field cod-plano     like mmv-plano-prevent.cod-plano
    field cod-sub-sist  like mmv-tar-plano.cod-sub-sist
    field cod-evento    like mmv-tar-plano.cod-evento
    field num-seq       like mmv-tar-plano.num-seq 
    index i-tt-tarefa is primary unique cod-plano      
                                        cod-sub-sist   
                                        cod-evento     
                                        num-seq.

DEFINE VARIABLE pImagem1 AS INTEGER    NO-UNDO.

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
&Scoped-define FIELDS-IN-QUERY-brDetalhe fnMarcado(ttDados.l-marcado) @ c-marcado no-label ttDados.cod-oficial ttDados.desc-dimensao ttDados.uso-real ttDados.uso-padrao ttDados.diferenca ttDados.un ttDados.dat-vencto ttDados.dat-atualiz fnLegenda() @ c-legenda fnOrigem(ttDados.i-origem) @ c-origem-dad ttDados.num-docto ttDados.cod-plano fnTipoManut(ttDados.cd-tipo) @ c-tip-manut   
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
&Scoped-Define ENABLED-OBJECTS rtToolBar-2 rtToolBar-3 btAbrir btSalvar ~
btRefresh btExpande btParam btFiltro btVisao btExcel btTxt btImprimir ~
btQueryJoins btReportsJoins btExit btHelp btRedimensiona brDetalhe ~
btLegenda btGeracao btPlano btHistorico btHoras btEqpto btEvento btPneu ~
btMarca btTodos 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnDescricao wWindow 
FUNCTION fnDescricao RETURNS CHARACTER
  ( pEvento as character,
    pSub    as character )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnLabels wWindow 
FUNCTION fnLabels RETURNS CHARACTER
  ( pImage as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnLegenda wWindow 
FUNCTION fnLegenda RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnMarcado wWindow 
FUNCTION fnMarcado RETURNS CHARACTER
  ( p-marcado as logical )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnMes wWindow 
FUNCTION fnMes RETURNS CHARACTER
  ( iMes as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnOrigem wWindow 
FUNCTION fnOrigem RETURNS CHARACTER
  ( i-orig as integer )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnTipoManut wWindow 
FUNCTION fnTipoManut RETURNS CHARACTER
  ( pTipo as integer )  FORWARD.

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
       MENU-ITEM miSelecao      LABEL "Se&leá∆o"       ACCELERATOR "CTRL-P"
       MENU-ITEM miFiltro       LABEL "&Filtro"        ACCELERATOR "CTRL-F"
       MENU-ITEM miClassifica   LABEL "&Classifica"    ACCELERATOR "CTRL-C"
       RULE
       MENU-ITEM miExcel        LABEL "E&xcel"         ACCELERATOR "CTRL-N"
       MENU-ITEM miTxt          LABEL "&Texto"         ACCELERATOR "CTRL-T"
       MENU-ITEM miImprimir     LABEL "&Imprimir"      ACCELERATOR "CTRL-J"
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
DEFINE VARIABLE CtrlFrame-3 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-3 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btAbrir 
     IMAGE-UP FILE "image/im-open":U
     IMAGE-INSENSITIVE FILE "image/ii-open":U
     LABEL "" 
     SIZE 4 BY 1.25 TOOLTIP "Abrir"
     FONT 4.

DEFINE BUTTON btEqpto 
     IMAGE-UP FILE "image/mab-eqpto.bmp":U
     LABEL "E&quip." 
     SIZE 4 BY 1.25 TOOLTIP "Consulta Equipamento".

DEFINE BUTTON btEvento 
     IMAGE-UP FILE "image/mab-evento.bmp":U
     LABEL "E&vento" 
     SIZE 4 BY 1.25 TOOLTIP "Consulta Movimentos de Eventos".

DEFINE BUTTON btExcel 
     IMAGE-UP FILE "image/excel":U
     LABEL "Planilha" 
     SIZE 4 BY 1.25 TOOLTIP "Exportar para Excel".

DEFINE BUTTON btExit 
     IMAGE-UP FILE "image/im-exi":U
     IMAGE-INSENSITIVE FILE "image/ii-exi":U
     LABEL "Exit" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btExpande 
     IMAGE-UP FILE "image/im-crit":U
     IMAGE-INSENSITIVE FILE "image/ii-crit":U
     LABEL "" 
     SIZE 4 BY 1.25 TOOLTIP "Expande / Contrai"
     FONT 4.

DEFINE BUTTON btFiltro 
     IMAGE-UP FILE "image/im-fil":U
     IMAGE-INSENSITIVE FILE "image/ii-fil":U
     LABEL "Filtro" 
     SIZE 4 BY 1.25 TOOLTIP "Filtro"
     FONT 4.

DEFINE BUTTON btGeracao 
     IMAGE-UP FILE "image/im-gera.bmp":U
     LABEL "&Gerar" 
     SIZE 4 BY 1.25 TOOLTIP "Geraá∆o da Ordem de Manutená∆o".

DEFINE BUTTON btHelp 
     IMAGE-UP FILE "image/im-hel":U
     IMAGE-INSENSITIVE FILE "image/ii-hel":U
     LABEL "Help" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btHistorico 
     IMAGE-UP FILE "image/im-inisp.bmp":U
     LABEL "&Hist¢rico" 
     SIZE 4 BY 1.25 TOOLTIP "Hist¢rico Manutená‰es".

DEFINE BUTTON btHoras 
     IMAGE-UP FILE "image/im-turno.bmp":U
     LABEL "H&oras" 
     SIZE 4 BY 1.25 TOOLTIP "Consulta Horas Trabalhadas".

DEFINE BUTTON btImprimir 
     IMAGE-UP FILE "image/im-prigr.bmp":U
     IMAGE-INSENSITIVE FILE "image/im-prigr.bmp":U
     LABEL "Impress∆o" 
     SIZE 4 BY 1.25 TOOLTIP "Impress∆o".

DEFINE BUTTON btLegenda 
     IMAGE-UP FILE "image/im-abc.bmp":U
     LABEL "&Legenda" 
     SIZE 4 BY 1.25 TOOLTIP "Legenda das Tarefas".

DEFINE BUTTON btMarca 
     LABEL "Marca" 
     SIZE 10 BY 1.

DEFINE BUTTON btParam 
     IMAGE-UP FILE "image/im-param":U
     IMAGE-INSENSITIVE FILE "image/ii-param":U
     LABEL "ParÉmetros" 
     SIZE 4 BY 1.25 TOOLTIP "ParÉmetros"
     FONT 4.

DEFINE BUTTON btPlano 
     IMAGE-UP FILE "image/im-calen.bmp":U
     LABEL "&Plano" 
     SIZE 4 BY 1.25 TOOLTIP "Consulta Plano Manutená∆o".

DEFINE BUTTON btPneu 
     IMAGE-UP FILE "image/im-pneu.bmp":U
     LABEL "Pneu" 
     SIZE 4 BY 1.25 TOOLTIP "Apontamento Movimento Pneus".

DEFINE BUTTON btQueryJoins 
     IMAGE-UP FILE "image/im-joi":U
     IMAGE-INSENSITIVE FILE "image/ii-joi":U
     LABEL "Query Joins" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btRedimensiona 
     LABEL "" 
     SIZE 90 BY .29.

DEFINE BUTTON btRefresh 
     IMAGE-UP FILE "image/im-sav":U
     IMAGE-INSENSITIVE FILE "image/ii-sav":U
     LABEL "" 
     SIZE 4 BY 1.25 TOOLTIP "Atualizar"
     FONT 4.

DEFINE BUTTON btReportsJoins 
     IMAGE-UP FILE "image/im-pri":U
     IMAGE-INSENSITIVE FILE "image/ii-pri":U
     LABEL "Reports Joins" 
     SIZE 4 BY 1.25
     FONT 4.

DEFINE BUTTON btSalvar 
     IMAGE-UP FILE "image/im-grava":U
     IMAGE-INSENSITIVE FILE "image/ii-grava":U
     LABEL "" 
     SIZE 4 BY 1.25 TOOLTIP "Salvar"
     FONT 4.

DEFINE BUTTON btTodos 
     LABEL "Todos" 
     SIZE 10 BY 1.

DEFINE BUTTON btTxt 
     IMAGE-UP FILE "image/mab-txt.bmp":U
     LABEL "" 
     SIZE 4 BY 1.25 TOOLTIP "Exportar para TXT".

DEFINE BUTTON btVisao 
     IMAGE-UP FILE "image/im-aloca":U
     IMAGE-INSENSITIVE FILE "image/im-aloci":U
     LABEL "Classificar" 
     SIZE 4 BY 1.25 TOOLTIP "Classifica"
     FONT 4.

DEFINE RECTANGLE rtToolBar-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 90 BY 1.5
     BGCOLOR 7 .

DEFINE RECTANGLE rtToolBar-3
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
      fnMarcado(ttDados.l-marcado) @ c-marcado  width 2  no-label
ttDados.cod-oficial                       width 14
ttDados.desc-dimensao                     width 30
ttDados.uso-real
ttDados.uso-padrao
ttDados.diferenca
ttDados.un
ttDados.dat-vencto
ttDados.dat-atualiz
fnLegenda() @ c-legenda width 15
fnOrigem(ttDados.i-origem) @ c-origem-dad width 16
ttDados.num-docto
ttDados.cod-plano
fnTipoManut(ttDados.cd-tipo) @ c-tip-manut width 25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 90 BY 8.29
         FONT 1
         TITLE "" ROW-HEIGHT-CHARS .46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btAbrir AT ROW 1.13 COL 2
     btSalvar AT ROW 1.13 COL 6
     btRefresh AT ROW 1.13 COL 10
     btExpande AT ROW 1.13 COL 16
     btParam AT ROW 1.13 COL 47 HELP
          "ParÉmetros"
     btFiltro AT ROW 1.13 COL 51 HELP
          "Filtro"
     btVisao AT ROW 1.13 COL 55 HELP
          "Classifica"
     btExcel AT ROW 1.13 COL 59
     btTxt AT ROW 1.13 COL 63
     btImprimir AT ROW 1.13 COL 67 HELP
          "Impress∆o"
     btQueryJoins AT ROW 1.13 COL 74.72 HELP
          "Consultas relacionadas"
     btReportsJoins AT ROW 1.13 COL 78.72 HELP
          "Relat¢rios relacionados"
     btExit AT ROW 1.13 COL 82.72 HELP
          "Sair"
     btHelp AT ROW 1.13 COL 86.72 HELP
          "Ajuda"
     btRedimensiona AT ROW 8.5 COL 1
     brDetalhe AT ROW 8.71 COL 1
     btLegenda AT ROW 17.17 COL 22.57 HELP
          "Legenda das Tarefas"
     btGeracao AT ROW 17.17 COL 32.72 HELP
          "Geraá∆o da Ordem de Manutená∆o"
     btPlano AT ROW 17.17 COL 67 HELP
          "Reporte de Horas"
     btHistorico AT ROW 17.17 COL 71 HELP
          "Hist¢rico Manutená‰es"
     btHoras AT ROW 17.17 COL 75 HELP
          "Consulta Horas Trabalhadas"
     btEqpto AT ROW 17.17 COL 79 HELP
          "Consulta Equipamento"
     btEvento AT ROW 17.17 COL 83 HELP
          "Consulta Movimentos de Eventos"
     btPneu AT ROW 17.17 COL 87 HELP
          "Apontamento Movimento Pneus"
     btMarca AT ROW 17.29 COL 1.57
     btTodos AT ROW 17.29 COL 11.57
     rtToolBar-2 AT ROW 1 COL 1
     rtToolBar-3 AT ROW 17 COL 1
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
         HEIGHT             = 17.63
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
       brDetalhe:NUM-LOCKED-COLUMNS IN FRAME fpage0     = 1
       brDetalhe:COLUMN-RESIZABLE IN FRAME fpage0       = TRUE
       brDetalhe:COLUMN-MOVABLE IN FRAME fpage0         = TRUE.

ASSIGN 
       btRedimensiona:MOVABLE IN FRAME fpage0          = TRUE.

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
       HEIGHT          = 6
       WIDTH           = 90
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME fpage0:HANDLE
       ROW             = 6.71
       COLUMN          = 84
       HEIGHT          = 1.58
       WIDTH           = 5.43
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-3 ASSIGN
       FRAME           = FRAME fpage0:HANDLE
       ROW             = 8.25
       COLUMN          = 1
       HEIGHT          = 1.17
       WIDTH           = 4
       HIDDEN          = yes
       SENSITIVE       = yes.
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {0713E8A2-850A-101B-AFC0-4210102A8DA7} type: TreeView */
      CtrlFrame-2:NAME = "CtrlFrame-2":U .
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {58DA8D8F-9D6A-101B-AFC0-4210102A8DA7} type: ImageList */
      CtrlFrame-3:NAME = "CtrlFrame-3":U .
/* CtrlFrame-3 OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-AFTER(btHelp:HANDLE IN FRAME fpage0).
      CtrlFrame-2:MOVE-AFTER(CtrlFrame).
      CtrlFrame-3:MOVE-AFTER(CtrlFrame-2).

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
           rtToolBar-3:width-chars in frame fPage0 = (rtToolBar-3:width-chars in frame fPage0 + dXC)
           rtToolBar-3:row in frame fPage0         = (rtToolBar-3:row in frame fPage0 + dYC)                                           
           btEqpto:row in frame fPage0             = (btEqpto:row in frame fPage0 + dYC)
           btEvento:row in frame fPage0            = (btEvento:row in frame fPage0 + dYC)
           btPneu:row in frame fPage0              = (btPneu:row in frame fPage0 + dYC)
           btGeracao:row in frame fPage0           = (btGeracao:row in frame fPage0 + dYC)
           btHistorico:row in frame fPage0         = (btHistorico:row in frame fPage0 + dYC)
           btHoras:row in frame fPage0             = (btHoras:row in frame fPage0 + dYC)
           btPlano:row in frame fPage0             = (btPlano:row in frame fPage0 + dYC)
           btMarca:row in frame fPage0             = (btMarca:row in frame fPage0 + dYC)
           btTodos:row in frame fPage0             = (btTodos:row in frame fPage0 + dYC)
           btLegenda:row in frame fPage0           = (btLegenda:row in frame fPage0 + dYC)          
           btGeracao:column in frame fPage0        = (btGeracao:column in frame fPage0 + (dXC))                                            
           btPlano:column in frame fPage0          = (btPlano:column in frame fPage0 + (dXC))
           btHistorico:column in frame fPage0      = (btHistorico:column in frame fPage0 + (dXC))
           btHoras:column in frame fPage0          = (btHoras:column in frame fPage0 + (dXC))
           btEqpto:column in frame fPage0          = (btEqpto:column in frame fPage0 + (dXC))
           btEvento:column in frame fPage0         = (btEvento:column in frame fPage0 + (dXC))
           btPneu:column in frame fPage0           = (btPneu:column in frame fPage0 + (dXC))
           btHelp:column in frame fPage0           = (btHelp:column in frame fPage0 + dXC)
           btExit:column in frame fPage0           = (btExit:column in frame fPage0 + dXC)
           btReportsJoins:column in frame fPage0   = (btReportsJoins:column in frame fPage0 + dXC)
           btQueryJoins:column in frame fPage0     = (btQueryJoins:column in frame fPage0 + dXC)
           btParam:column in frame fPage0          = (btParam:column in frame fPage0 + dXC)
           btFiltro:column in frame fPage0         = (btFiltro:column in frame fPage0 + dXC)
           btVisao:column in frame fPage0          = (btVisao:column in frame fPage0 + dXC)
           btExcel:column in frame fPage0          = (btExcel:column in frame fPage0 + dXC)
           btTXT:column in frame fPage0            = (btTXT:column in frame fPage0 + dXC)
           btImprimir:column in frame fPage0       = (btImprimir:column in frame fPage0 + dXC)
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
           rtToolBar-3:width-chars in frame fPage0 = (rtToolBar-3:width-chars in frame fPage0 - dXC)
           rtToolBar-3:row in frame fPage0         = (rtToolBar-3:row in frame fPage0 - dYC)                                 
           btEqpto:row in frame fPage0             = (btEqpto:row in frame fPage0 - dYC)
           btEvento:row in frame fPage0            = (btEvento:row in frame fPage0 - dYC)
           btPneu:row in frame fPage0              = (btPneu:row in frame fPage0 - dYC)
           btGeracao:row in frame fPage0           = (btGeracao:row in frame fPage0 - dYC)
           btHistorico:row in frame fPage0         = (btHistorico:row in frame fPage0 - dYC)
           btHoras:row in frame fPage0             = (btHoras:row in frame fPage0 - dYC)                                           
           btPlano:row in frame fPage0             = (btPlano:row in frame fPage0 - dYC)
           btMarca:row in frame fPage0             = (btMarca:row in frame fPage0 - dYC)
           btTodos:row in frame fPage0             = (btTodos:row in frame fPage0 - dYC)
           btLegenda:row in frame fPage0           = (btLegenda:row in frame fPage0 - dYC)          
           btGeracao:column in frame fPage0        = (btGeracao:column in frame fPage0 - (dXC))                                                       
           btPlano:column in frame fPage0          = (btPlano:column in frame fPage0 - (dXC))
           btHistorico:column in frame fPage0      = (btHistorico:column in frame fPage0 - (dXC))
           btHoras:column in frame fPage0          = (btHoras:column in frame fPage0 - (dXC))           
           btEqpto:column in frame fPage0          = (btEqpto:column in frame fPage0 - (dXC))
           btEvento:column in frame fPage0         = (btEvento:column in frame fPage0 - (dXC))
           btPneu:column in frame fPage0           = (btPneu:column in frame fPage0 - (dXC))
           btHelp:column in frame fPage0           = (btHelp:column in frame fPage0 - dXC)
           btExit:column in frame fPage0           = (btExit:column in frame fPage0 - dXC)
           btReportsJoins:column in frame fPage0   = (btReportsJoins:column in frame fPage0 - dXC)
           btQueryJoins:column in frame fPage0     = (btQueryJoins:column in frame fPage0 - dXC)
           btParam:column in frame fPage0          = (btParam:column in frame fPage0 - dXC)
           btFiltro:column in frame fPage0         = (btFiltro:column in frame fPage0 - dXC)
           btVisao:column in frame fPage0          = (btVisao:column in frame fPage0 - dXC)
           btImprimir:column in frame fPage0       = (btImprimir:column in frame fPage0 - dXC)
           btExcel:column in frame fPage0          = (btExcel:column in frame fPage0 - dXC)
           btTXT:column in frame fPage0            = (btTXT:column in frame fPage0 - dXC)
           brDetalhe:width-chars in frame fPage0   = (brDetalhe:width-chars in frame fPage0 - dXC)
           btRedimensiona:width-chars in frame fPage0 = (btRedimensiona:width-chars in frame fPage0 - dXC)
           btRedimensiona:row in frame fPage0      = 9
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


&Scoped-define BROWSE-NAME brDetalhe
&Scoped-define SELF-NAME brDetalhe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brDetalhe wWindow
ON MOUSE-SELECT-DBLCLICK OF brDetalhe IN FRAME fpage0
DO:
  apply "CHOOSE":U to btMarca in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brDetalhe wWindow
ON RETURN OF brDetalhe IN FRAME fpage0
DO:
  apply "CHOOSE":U to btMarca in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brDetalhe wWindow
ON ROW-DISPLAY OF brDetalhe IN FRAME fpage0
DO:
  if avail ttDados then do:
      /** Vencido = Vermelho **/
      if ttDados.lVencido then do:
          assign ttDados.desc-dimensao:fgcolor in browse brDetalhe = 12
                 ttDados.diferenca:fgcolor     in browse brDetalhe = 12
                 ttDados.dat-vencto:fgcolor    in browse brDetalhe = 12.
      end.
      else do:
          /** A Vencer = Azul **/
          if ttDados.lVencer then do:
          assign ttDados.desc-dimensao:fgcolor in browse brDetalhe = 9
                 ttDados.diferenca:fgcolor     in browse brDetalhe = 9
                 ttDados.dat-vencto:fgcolor    in browse brDetalhe = 9.
          end.
      end.
      /** Tarefas de OM **/
      if ttDados.i-estado = 4 then do:
          assign ttDados.desc-dimensao:fgcolor in browse brDetalhe = 7
                 ttDados.diferenca:fgcolor     in browse brDetalhe = 7
                 ttDados.dat-vencto:fgcolor    in browse brDetalhe = 7.
      end.
      /** Garantia de Sub-Sistema **/
      IF ttDados.i-origem = 10 THEN DO:
          ASSIGN ttDados.desc-dimensao:FGCOLOR IN BROWSE brDetalhe = 2
                 ttDados.diferenca:FGCOLOR     IN BROWSE brDetalhe = 2
                 ttDados.dat-vencto:FGCOLOR    IN BROWSE brDetalhe = 2.
      END.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brDetalhe wWindow
ON VALUE-CHANGED OF brDetalhe IN FRAME fpage0
DO:
  if avail ttDados and ttDados.p-image = 30 then do:
      if c-marcado:screen-value in browse brDetalhe = "*":U then do:
          {utp/ut-liter.i "Desmarcar"}
          assign btMarca:label in frame fPage0 = "&" + trim(return-value).
      end.
      else do:
          {utp/ut-liter.i "Marcar"}
          assign btMarca:label in frame fPage0 = "&" + trim(return-value).
      end.
  end.
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


&Scoped-define SELF-NAME btEqpto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btEqpto wWindow
ON CHOOSE OF btEqpto IN FRAME fpage0 /* Equip. */
DO:
    run chamaPrograma in this-procedure (input "abp/ab0602.w":U,
                                         input 10,
                                         input no).
  apply "ENTRY":U to btEqpto in frame fPage0.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btEvento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btEvento wWindow
ON CHOOSE OF btEvento IN FRAME fpage0 /* Evento */
DO:
  run chamaPrograma in this-procedure (input "abp/ab0303.w":U,
                                       input 5,
                                       input yes).
  apply "ENTRY":U to btEvento in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExcel wWindow
ON CHOOSE OF btExcel IN FRAME fpage0 /* Planilha */
OR CHOOSE OF MENU-ITEM miExcel  IN MENU mbMain DO:
    if chTreeView:Nodes:Count > 0 then do:
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
    if chTreeView:nodes:Count > 0 then do:
        /** Controla expans∆o **/
        assign l-expande = not l-expande.
        /** Expande os n°veis abaixo **/
        do i-cont = 1 to chTreeView:Nodes:Count:
           assign chTreeView:Nodes(i-cont):Expanded = l-expande.
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
  run mvp/esmv0613a.w (input-output  table ttSelecao).
  assign {&window-name}:sensitive = yes.
  apply "ENTRY":U to btFiltro in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btGeracao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btGeracao wWindow
ON CHOOSE OF btGeracao IN FRAME fpage0 /* Gerar */
DO: 
    /** Geraá∆o das Tarefas para uma OM **/
    run piOMGeracao in this-procedure.
    apply "ENTRY":U to btGeracao in frame fPage0.
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


&Scoped-define SELF-NAME btHistorico
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHistorico wWindow
ON CHOOSE OF btHistorico IN FRAME fpage0 /* Hist¢rico */
DO:
  run chamaPrograma in this-procedure (input "mvp/mv0609.w":U,
                                       input 10,
                                       input no).
  apply "ENTRY":U to btHistorico in frame fPage0.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btHoras
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHoras wWindow
ON CHOOSE OF btHoras IN FRAME fpage0 /* Horas */
DO:
  run chamaPrograma in this-procedure (input "mvp/mv0604.w":U,
                                       input 0,
                                       input no).
  apply "ENTRY":U to btHoras in frame fPage0.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btImprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btImprimir wWindow
ON CHOOSE OF btImprimir IN FRAME fpage0 /* Impress∆o */
OR CHOOSE OF MENU-ITEM miImprimir  IN MENU mbMain DO:
    if chTreeView:Nodes:Count > 0 then
        run piImprimir in this-procedure.
    apply "ENTRY":U to btImprimir in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btLegenda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btLegenda wWindow
ON CHOOSE OF btLegenda IN FRAME fpage0 /* Legenda */
DO: 
    assign {&window-name}:sensitive = no.
    run mvp/mv0613h.w.
    assign {&window-name}:sensitive = yes.
    apply "ENTRY":U to btLegenda in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btMarca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btMarca wWindow
ON CHOOSE OF btMarca IN FRAME fpage0 /* Marca */
DO:
    if num-results("brDetalhe":U) > 0 then do:
        get current brDetalhe.
        if avail ttDados then do:
            if c-marcado:screen-value in browse brDetalhe = "*":U then
                assign c-marcado:screen-value in browse brDetalhe = "":U
                       ttDados.l-marcado                          = no.
            else do:
                /** Valida se registro pode gerar tarefas para uma OM **/
                run piOMValidacao in this-procedure (input 1).
                if return-value = "OK":U then
                    assign c-marcado:screen-value in browse brDetalhe = "*":U
                           ttDados.l-marcado                          = yes.
            end.
        end.
        apply "VALUE-CHANGED":U to browse brDetalhe.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btParam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btParam wWindow
ON CHOOSE OF btParam IN FRAME fpage0 /* ParÉmetros */
OR CHOOSE OF MENU-ITEM miSelecao  IN MENU mbMain DO:
    
  assign {&window-name}:sensitive = no.
  run mvp/esmv0613c.w (input-output  table ttSelecao).
  for first ttSelecao no-lock:
    assign chTimer:Interval = (ttSelecao.iMinutos * 60) * 1000.
  end.
  assign {&window-name}:sensitive = yes.
  apply "ENTRY":U to btParam in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPlano
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPlano wWindow
ON CHOOSE OF btPlano IN FRAME fpage0 /* Plano */
DO:
  run chamaPrograma in this-procedure (input "mvp/mv0602.w":U,
                                       input 35,
                                       input no).
  apply "ENTRY":U to btPlano in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPneu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPneu wWindow
ON CHOOSE OF btPneu IN FRAME fpage0 /* Pneu */
DO:
  run chamaPrograma in this-procedure (input "pnp/pn0301.w":U,
                                       input 0,
                                       input yes).
  apply "ENTRY":U to btPneu in frame fPage0.
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

    if btRedimensiona:row > (frame fPage0:height - 4) then
        assign btRedimensiona:row = (frame fPage0:height - 4).
     
    if btRedimensiona:row > frame fPage0:height then
       assign btRedimensiona:row = frame fPage0:height - 1.
    
    assign CtrlFrame:height = btRedimensiona:row - 2.5 no-error.         

    assign brDetalhe:row     = btRedimensiona:row + btRedimensiona:height no-error.
    assign brDetalhe:height  = (frame fPage0:height - 1.5) + 0.5 - (btRedimensiona:row) no-error.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRefresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRefresh wWindow
ON CHOOSE OF btRefresh IN FRAME fpage0
OR CHOOSE OF MENU-ITEM miAtualizar  IN MENU mbMain DO:

/** Controla expans∆o **/
if l-expande then 
    assign l-expande = no.

ASSIGN brDetalhe:title in frame fPage0 = ""
       c-tag-tree                      = "".

session:set-wait-state ("GENERAL").
run piAtualizar in this-procedure (input 1).
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
      for each ttSelecao:
          export delimiter ";"
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
            ttSelecao.centro-ini 
            ttSelecao.centro-fim 
            ttSelecao.planej-ini 
            ttSelecao.planej-fim 
            ttSelecao.setor-ini  
            ttSelecao.setor-fim  
            ttSelecao.oficina-ini
            ttSelecao.oficina-fim
            ttSelecao.evento-ini 
            ttSelecao.evento-fim
            ttSelecao.tag-ini
            ttSelecao.tag-fim
            ttSelecao.dt-corte   
            ttSelecao.dt-perc-ini
            ttSelecao.dt-perc-fim
            ttSelecao.lAtivos    
            ttSelecao.lInativos  
            ttSelecao.lProprios  
            ttSelecao.lTerceiros 
            ttSelecao.lMotor     
            ttSelecao.lNMotor    
            ttSelecao.lAVencer   
            ttSelecao.lServico   
            ttSelecao.lPlanoComp 
            ttSelecao.lPlanoFuturo
            ttSelecao.iPlano     
            ttSelecao.cod-calend 
            ttSelecao.iMinutos
            ttSelecao.idi-durabilidade
            ttSelecao.sub-sist-ini
            ttSelecao.sub-sist-fim.
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


&Scoped-define SELF-NAME btTodos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btTodos wWindow
ON CHOOSE OF btTodos IN FRAME fpage0 /* Todos */
DO:
    define variable l-marca as logical no-undo.
    define variable rRow    as rowid   no-undo.

    if num-results("brDetalhe":U) > 0 then do:
        /** Guarda rowid do tree-view **/
        assign rRow = to-rowid(entry(1,chTreeView:SelectedItem:Tag)).
        /** Busca Registro Pai com rowid **/
        for first bfttDados 
            where rowid(bfttDados) = rRow no-lock:
        end.
        if can-find(first ttDados
                    where ttDados.cod-dimens-pai = bfttDados.cod-dimensao
                    AND   ttDados.lMostra
                    and   ttDados.l-marcado no-lock) then do:
            assign l-marca = no.
        end.
        else 
            assign l-marca = yes.
        /** Percorre registros filhos **/
        for each  ttDados
            where ttDados.cod-dimens-pai = bfttDados.cod-dimensao
            and   ttDados.lMostra no-lock:
            /** Valida se Registro ser† marcado ou n∆o **/
            run piOMValidacao in this-procedure (input 2).
            if return-value = "OK":U then
                /** Atualiza marcaá∆o **/
                assign ttDados.l-marcado = l-marca.
        end.
        /** Reabre a query **/
        open query brDetalhe for each  ttDados
                                 where ttDados.cod-dimens-pai = bfttDados.cod-dimensao 
                                 and   ttDados.lMostra no-lock
                                 by    ttDados.dat-vencto
                                 by    ttDados.cod-oficial
                                 BY    ttDados.desc-dimensao.
        apply "VALUE-CHANGED":U to browse brDetalhe.
    end.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btTxt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btTxt wWindow
ON CHOOSE OF btTxt IN FRAME fpage0
OR CHOOSE OF MENU-ITEM miTxt IN MENU mbMain DO:
    if chTreeView:Nodes:Count > 0 then do:
        run piTexto in this-procedure.
    end.
    apply "ENTRY":U to btTxt in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btVisao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btVisao wWindow
ON CHOOSE OF btVisao IN FRAME fpage0 /* Classificar */
OR CHOOSE OF MENU-ITEM miClassifica  IN MENU mbMain DO:
 
  assign {&window-name}:sensitive = no.
  run mvp/esmv0613b.w (input-output table ttVisao).
  assign {&window-name}:sensitive = yes.
  apply "ENTRY":U to btVisao in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame wWindow OCX.MouseDown
PROCEDURE CtrlFrame.TreeView.MouseDown .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Button
    Shift
    x
    y
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Button AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-Shift  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-x      AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-y      AS INTEGER NO-UNDO.


assign i-button-tree = p-Button.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame wWindow OCX.MouseUp
PROCEDURE CtrlFrame.TreeView.MouseUp .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Button
    Shift
    x
    y
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Button AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-Shift  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-x      AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-y      AS INTEGER NO-UNDO.

/*assign i-button-tree = p-Button.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame wWindow OCX.NodeClick
PROCEDURE CtrlFrame.TreeView.NodeClick .
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

if chTreeView:Nodes:Count > 0 then do:
    /** Guarda rowid do tree-view **/
    assign rRow             = to-rowid(entry(1,chTreeView:SelectedItem:Tag))
           c-tag-tree       = chTreeView:SelectedItem:key().
    /** Busca Registro com rowid **/
    for first bfttDados 
        where rowid(bfttDados) = rRow no-lock:
    end.
    /** Verifica se ele contÇm filhos **/
    for first ttDados
        where ttDados.cod-dimens-pai = bfttDados.cod-dimensao no-lock:
    end.

    /** Mostra registros no browse **/
    run piBrowseAtualiza in this-procedure.
    /** Seleá∆o de linha **/
    case i-button-tree:
        /** Bot∆o direito do mouse **/
        when 2 then do:
           /** Pop Menu Expande-Contrai **/
           case int(chTreeView:SelectedItem:image):
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
              otherwise    assign exp-con:sensitive = TRUE.
           end case.
           assign exp-con:label = if chTreeView:SelectedItem:Expanded = false then cExpande
                                                                              else cContrai.
           run SendMessageA (self:hwnd, 517, 0, 0).            
        end.
    end case.
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-3 wWindow OCX.Tick
PROCEDURE CtrlFrame-3.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

/** Atualiza dados **/
session:set-wait-state ("GENERAL":U).
run piAtualizar in this-procedure (input 1).
session:set-wait-state ("":U).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWindow 


/*--- L¢gica para inicializaá∆o do programam ---*/
define temp-table RowErrorsAux no-undo like RowErrors.
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
    if chTreeView:nodes:Count > 0 then do:
        run expandeItem (input chTreeView:SelectedItem,
                         input not chTreeView:SelectedItem:Expanded).
    end.
end.

ON 'ENTRY':U anywhere DO:
    for first ttSelecao no-lock:
        assign chTimer:Interval = ttSelecao.iMinutos * 60 * 1000.
    end.
END.

btRedimensiona:load-mouse-pointer ("image/size2.cur") in frame fPage0.

{abp/ab9000.i}    /** Convers∆o de horas invertidas **/
{mvp/mv9000.i}    /** Convers∆o hora decimal
                      OBS: A procedure piCriaErro encontra-se definida dentro desta include  **/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterDestroyInterface wWindow 
PROCEDURE afterDestroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     afterDestroyInterface
  Parameters:  <none>
  Notes:       Override ap¢s terminar a tela
------------------------------------------------------------------------------*/
/** Caixa de mensagens **/
{method/showmessage.i3}
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
{utp/ut-liter.i "C¢digo"}
assign ttDados.cod-oficial:label in browse brDetalhe = return-value.
{utp/ut-liter.i "Descriá∆o"}
assign ttDados.desc-dimensao:label in browse brDetalhe = return-value.
{utp/ut-liter.i "Origem"}
assign c-origem-dad:label in browse brDetalhe = return-value.
{utp/ut-liter.i "Uso Real"}
assign ttDados.uso-real:label in browse brDetalhe = return-value.
{utp/ut-liter.i "Uso Padr∆o"}
assign ttDados.uso-padrao:label in browse brDetalhe = return-value.
{utp/ut-liter.i "Diferenáa"}
assign ttDados.diferenca:label in browse brDetalhe = return-value.
{utp/ut-liter.i "UN"}
assign ttDados.un:label in browse brDetalhe = return-value.
{utp/ut-liter.i "Documento"}
assign ttDados.num-docto:label in browse brDetalhe = return-value.
{utp/ut-liter.i "Vencimento"}
assign ttDados.dat-vencto:label in browse brDetalhe = return-value.
{utp/ut-liter.i "Atualizaá∆o"}
assign ttDados.dat-atualiz:label in browse brDetalhe = return-value.
{utp/ut-liter.i "Tipo Manutená∆o"}
assign c-tip-manut:label in browse brDetalhe = return-value.
{utp/ut-liter.i "Todos"}
assign btTodos:label in frame fPage0 = "&" + trim(return-value).
{utp/ut-liter.i "Marcar"}
assign btMarca:label in frame fPage0 = "&" + trim(return-value).

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
assign chTreeView               = chCtrlFrame:TreeView
       chImageList              = chCtrlFrame-2:ImageList
       chTimer                  = chCtrlFrame-3:PSTimer
       chTreeView:ImageList     = chImageList
       CtrlFrame:popup-menu     = pop-menu
       chTreeView:HideSelection = FALSE
       dWinXC                   = wWindow:width-chars
       dWinYC                   = wWindow:height-chars.

ChTreeView:Nodes:Clear().

if chTreeView:Nodes:Count > 0 then
   assign chTreeView:SelectedItem = chTreeView:Nodes(1). 

create ttSelecao.
assign ttSelecao.empresa-ini  = ""
       ttSelecao.empresa-fim  = "ZZZ"
       ttSelecao.equipto-ini  = ""
       ttSelecao.equipto-fim  = "ZZZZZZZZZZZZZZZZ"
       ttSelecao.grupo-ini    = ""
       ttSelecao.grupo-fim    = "ZZZZZZZZ"
       ttSelecao.modelo-ini   = ""
       ttSelecao.modelo-fim   = "ZZZZZZZZ"
       ttSelecao.estab-ini    = ""
       ttSelecao.estab-fim    = "ZZZ"
       ttSelecao.centro-ini   = ""
       ttSelecao.centro-fim   = "ZZZZZZZZZZZZZZZZ"
       ttSelecao.planej-ini   = ""
       ttSelecao.planej-fim   = "ZZZZZZZZ"
       ttSelecao.setor-ini    = ""
       ttSelecao.setor-fim    = "ZZZZZZZZ"
       ttSelecao.oficina-ini  = ""
       ttSelecao.oficina-fim  = "ZZZZZZZZ"
       ttSelecao.evento-ini   = ""        
       ttSelecao.evento-fim   = "ZZZZZZZZ"
       ttSelecao.tag-ini      = ""
       ttSelecao.tag-fim      = "ZZZZZZZZZZZZZZZZ"
       ttSelecao.sub-sist-ini = ""
       ttSelecao.sub-sist-fim = "ZZZZZZZZ"
       ttSelecao.dt-corte     = today
       ttSelecao.dt-perc-ini  = today
       ttSelecao.dt-perc-fim  = today
       ttSelecao.lAtivos      = yes
       ttSelecao.lInativos    = no
       ttSelecao.lProprios    = yes
       ttSelecao.lTerceiros   = no
       ttSelecao.lMotor       = yes
       ttSelecao.lNMotor      = yes
       ttSelecao.lVencido     = yes
       ttSelecao.lAVencer     = yes
       ttSelecao.lOutrosEv    = no
       ttSelecao.lServico     = no
       ttSelecao.lNTerminada  = no
       ttSelecao.lPlanoComp   = no
       ttSelecao.lComp        = no
       ttSelecao.lOleo        = yes
       ttSelecao.lMecanica    = yes
       ttSelecao.lCusto       = yes
       ttSelecao.lOutros      = yes
       ttSelecao.lPlanoFuturo = yes
       ttSelecao.lTpEventos   = yes
       ttSelecao.iPlano       = 1
       ttSelecao.cod-calend   = ""
       ttSelecao.iMinutos     = 15
       ttSelecao.deAntecipa   = 1
       ttSelecao.idi-durabilidade = 4.

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

define variable deKMEqpto  as decimal   no-undo.
define variable cEstabOfic as character no-undo.

/** Zera vari†veis no in°cio **/
assign vCodigo    = "":U
       vDescricao = "":U
       iImage     = 0
       rRowid     = ?.

assign iSeq-tar-plano = 0.

find first plano_ccusto no-lock
     where plano_ccusto.cod_empresa     = v_cod_empres_usuar
       and plano_ccusto.dat_inic_valid <= today
       and plano_ccusto.dat_fim_valid  >= today no-error.

/** Verifica tipo de vis∆o (classificaá∆o) **/
case substring(trim(pValorDimensao),1,2):
    /** Empresa **/
    when "01":U then do:
        if not avail empresa or empresa.ep-codigo <> mab-eqpto.ep-codigo then do:
            for first empresa fields(nome ep-codigo)  
                where empresa.ep-codigo = mab-eqpto.ep-codigo no-lock:
            end.
        end.
        if avail empresa then do:
            assign vCodigo    = string(mab-eqpto.ep-codigo, "999")
                   vDescricao = empresa.nome
                   iImage     = 1
                   rRowid     = rowid(empresa).
        end.
    end.
    /** Estabelecimento Equipamento **/
    when "02":U then do:
        if not avail estabelec or estabelec.cod-estabel <> mab-eqpto.cod-estabel then do:
            for first estabelec fields(nome cod-estabel)  
                where estabelec.cod-estabel = mab-eqpto.cod-estabel no-lock:
            end.
        end.
        if avail estabelec then do:
            assign vCodigo    = mab-eqpto.cod-estabel
                   vDescricao = estabelec.nome
                   iImage     = 2
                   rRowid     = rowid(estabelec).
        end.
    end.
    /** Estabelecimento Oficina **/
    when "03":U then do:
        if cCodOficina <> "":U then do:
            if not avail mmv-ofici or mmv-ofici.cod-ofici <> cCodOficina then do:
                for first mmv-ofici fields(des-ofici cod-ofici cod-estabel)  
                    where mmv-ofici.cod-ofici = cCodOficina no-lock:
                end.
            end.
            if avail mmv-ofici then do:
                /** Verifica se o Estabelecimento Ç da Oficina ou da OM **/
                assign cEstabOfic = if iOrigOficina = 1
                                    then mmv-ord-manut.cod-estabel
                                    else mmv-ofici.cod-estabel.
                if not avail estabelec or estabelec.cod-estabel <> cEstabOfic then do:
                    for first estabelec fields(nome cod-estabel)  
                        where estabelec.cod-estabel = cEstabOfic no-lock:
                    end.
                end.
                if avail estabelec then do:
                    assign vCodigo    = estabelec.cod-estabel
                           vDescricao = estabelec.nome
                           iImage     = 3
                           rRowid     = rowid(estabelec).
                end.
            end.
        end.
        else do:
            {utp/ut-liter.i "N∆o Alocados"}
            assign vCodigo    = "_"
                   vDescricao = trim(return-value)
                   iImage     = 3
                   rRowid     = ?.
        end.
    end.
    /** Centro Custo **/
    when "04":U then do:
        if not avail ccusto or ccusto.cod_ccusto <> cCCCusto then do:
            for first ccusto no-lock
                where ccusto.cod_empresa      = plano_ccusto.cod_empresa
                  and ccusto.cod_plano_ccusto = plano_ccusto.cod_plano_ccusto
                  and ccusto.cod_ccusto       = cCCCusto:
            END.
        END.
        if avail ccusto then do:
            assign vCodigo    = ccusto.cod_ccusto
                   vDescricao = ccusto.des_tit_ctbl
                   iImage     = 4
                   rRowid     = rowid(ccusto).
        end.
        else do:
            {utp/ut-liter.i Centro_Custo_Eqpto_N∆o_Dispon°vel}
                assign vCodigo    = cCCCusto
                       vDescricao = return-value
                       iImage     = 4
                       rRowid     = rowid(mab-eqpto).
        end.
    end.
    /** Oficina **/
    when "05":U then do:
        if cCodOficina <> "":U then do:
            if not avail mmv-ofici or mmv-ofici.cod-ofici <> cCodOficina then do:
                for first mmv-ofici fields(des-ofici cod-ofici cod-estabel)  
                    where mmv-ofici.cod-ofici = cCodOficina no-lock:
                end.
            end.
            if avail mmv-ofici then do:
                assign vCodigo    = cCodOficina
                       vDescricao = mmv-ofici.des-ofici
                       iImage     = 5
                       rRowid     = rowid(mmv-ofici).
            end.
        end.
        else do:
            {utp/ut-liter.i "N∆o Alocados"}
            assign vCodigo    = "_"
                   vDescricao = trim(return-value)
                   iImage     = 5
                   rRowid     = ?.
        end.
    end.
    /** Setor **/
    when "06":U then do:
        if cCodSetor <> "":U then do:
            if not avail mmv-setor-ofici or mmv-setor-ofici.cod-setor-ofici <> cCodSetor then do:
                for first mmv-setor-ofici fields(des-setor-ofici cod-setor-ofici cod-ofici)  
                    where mmv-setor-ofici.cod-setor-ofici = cCodSetor no-lock:
                end.
            end.
            if avail mmv-setor-ofici then do:
                assign vCodigo    = cCodSetor
                       vDescricao = mmv-setor-ofici.des-setor-ofici
                       iImage     = 6
                       rRowid     = rowid(mmv-setor-ofici).
            end.
        end.
        else do:
            {utp/ut-liter.i "N∆o Alocados"}
            assign vCodigo    = "_"
                   vDescricao = trim(return-value)
                   iImage     = 6
                   rRowid     = ?.
        end.
    end.

    /** Màs / Ano **/
    when "07":U then do:
        if dt-dat-vencto <> ? then do:
            assign vCodigo    = string(month(dt-dat-vencto),"99") + "/" + string(year(dt-dat-vencto),"9999")
                   vDescricao = fnMes(month(dt-dat-vencto)) + " " + string(year(dt-dat-vencto),"9999")
                   iImage     = 07
                   rRowid     = rowid(mab-eqpto).
        end.
        else do:
            {utp/ut-liter.i "Sem Vencimento"}
            assign vCodigo    = "_"
                   vDescricao = trim(return-value)
                   iImage     = 07
                   rRowid     = rowid(mab-eqpto).
        end.
    end.
    /** Data **/
    when "08":U then do:
        if dt-dat-vencto <> ? then do:
            {utp/ut-liter.i "Vencimento"}
            assign vCodigo    = string(dt-dat-vencto,'99/99/9999')
                   vDescricao = trim(return-value) + ": " + string(dt-dat-vencto,'99/99/9999')
                   iImage     = 08
                   rRowid     = rowid(mab-eqpto).
        end.
        else do:
            {utp/ut-liter.i "Sem Vencimento"}
            assign vCodigo    = "_"
                   vDescricao = trim(return-value)
                   iImage     = 08
                   rRowid     = rowid(mab-eqpto).
        end.
    end.
    /** Planejador **/
    when "09":U then do:
        if cCodPlanejad <> "":U then do:
            if not avail mmv-plandor or mmv-plandor.cod-plandor <> cCodPlanejad then do:
                for first mmv-plandor fields(nom-plandor cod-plandor)  
                    where mmv-plandor.cod-plandor = cCodPlanejad no-lock:
                end.
            end.
            if avail mmv-plandor then do:
                {utp/ut-liter.i "Em Branco"}
                assign vCodigo    = if cCodPlanejad = "" then "_" else cCodPlanejad
                       vDescricao = if mmv-plandor.nom-plandor = "" then trim(return-value) else mmv-plandor.nom-plandor
                       iImage     = 9
                       rRowid     = rowid(mmv-plandor).
            end.
        end.
        else do:
            {utp/ut-liter.i "Sem Planejador"}
            assign vCodigo    = "_"
                   vDescricao = trim(return-value)
                   iImage     = 9
                   rRowid     = ?.
        end.
    end.
    /** Equipamento **/
    when "10":U then do:
        assign vCodigo    = string(mab-eqpto.ep-codigo) + "-" + mab-eqpto.cod-eqpto
               iImage     = 10
               rRowid     = rowid(mab-eqpto).
        if not avail mab-model or mab-model.cod-model <> mab-eqpto.cod-model then do:
            for first mab-model fields(des-model cod-model un cod-livre-1)  
                where mab-model.cod-model = mab-eqpto.cod-model no-lock:
            end.
        end.
        if avail mab-model then do:
            run piCalculaKM in this-procedure (input  mab-eqpto.ep-codigo,
                                               input  mab-eqpto.cod-eqpto,
                                               input  deInvertidaAtual,
                                               input  2,
                                               output deKMEqpto).
            {utp/ut-liter.i "Contador"}
            assign vDescricao = mab-model.des-model + "       " + trim(return-value) + ": " + trim(string(deKMEqpto,'>>>,>>>,>>9.99')).
            /** Busca £ltima quilometragem do equipamento **/
            run piCalculaKM in this-procedure (input  mab-eqpto.ep-codigo,
                                               input  mab-eqpto.cod-eqpto,
                                               input  deInvertidaAtual,
                                               input  1,
                                               output deKMEqpto).
            {utp/ut-liter.i "acumulado"}
            assign vDescricao = vDescricao + "       " + caps(mab-model.un) + " " + trim(return-value) + ": " + trim(string(deKMEqpto,'>>>,>>>,>>9.99')).
            /** Busca £ltima contador do equipamento **/
            IF mab-eqpto.log-cont-sec THEN DO: /* Validaá∆o Contador Secundario */
                run piCalculaKM in THIS-PROCEDURE (input  mab-eqpto.ep-codigo,
                                                   input  mab-eqpto.cod-eqpto,
                                                   input  deInvertidaAtual,
                                                   input  4,
                                                   output deKMEqpto).
                {utp/ut-liter.i "Contador Secund†rio"}
                assign vDescricao = vDescricao + "       " + trim(return-value) + ": " + trim(string(deKMEqpto,'>>>,>>>,>>9.99')).
                /** Busca £ltima quilometragem do equipamento **/
                run piCalculaKM in this-procedure (input  mab-eqpto.ep-codigo,
                                                   input  mab-eqpto.cod-eqpto,
                                                   input  deInvertidaAtual,
                                                   input  3,
                                                   output deKMEqpto).
                {utp/ut-liter.i "acumulado"}
                assign vDescricao = vDescricao + "       " + caps(SUBSTRING(mab-model.cod-livre-1,1,2)) + " " + trim(return-value) + ": " + trim(string(deKMEqpto,'>>>,>>>,>>9.99')).
                
            END. /*IF mab-eqpto.log-cont-sec*/
        END. /*if avail mab-model*/

    END. /*when "11":U*/
    /** Estrutura MecÉnica **/
    when "11":U then do:
        if not avail mab-estrut-mec or mab-estrut-mec.cod-estrut-mec <> mab-eqpto.cod-estrut-mec then do:
            for first mab-estrut-mec fields(des-estrut-mec cod-estrut-mec)  
                where mab-estrut-mec.cod-estrut-mec = mab-eqpto.cod-estrut-mec no-lock:
            end.
        end.
        if avail mab-estrut-mec then do:
            assign vCodigo    = mab-eqpto.cod-estrut-mec
                   vDescricao = mab-estrut-mec.des-estrut-mec
                   iImage     = 11
                   rRowid     = rowid(mab-estrut-mec).
        end.         
    end.
    /** Grupo Equipamento **/
    when "12":U then do:
        if not avail mab-grp-eqpto or mab-grp-eqpto.cod-grp-eqpto <> mab-eqpto.cod-grp-eqpto then do:
            for first mab-grp-eqpto fields(des-grp-eqpto cod-grp-eqpto)  
                where mab-grp-eqpto.cod-grp-eqpto = mab-eqpto.cod-grp-eqpto no-lock:
            end.
        end.
        if avail mab-grp-eqpto then do:
            assign vCodigo    = mab-eqpto.cod-grp-eqpto
                   vDescricao = mab-grp-eqpto.des-grp-eqpto
                   iImage     = 12
                   rRowid     = rowid(mab-grp-eqpto).
        end.         
    end.
    /** Modelo Equipamento **/
    when "13":U then do:
        if not avail mab-model or mab-model.cod-model <> mab-eqpto.cod-model then do:
            for first mab-model fields(des-model cod-model un)  
                where mab-model.cod-model = mab-eqpto.cod-model no-lock:
            end.
        end.
        if avail mab-model then do:
            assign vCodigo    = mab-eqpto.cod-model
                   vDescricao = mab-model.des-model
                   iImage     = 13
                   rRowid     = rowid(mab-model).
        end.         
    end.
    /** Usu†rio **/
    when "14":U then do:
        if not avail usuar_mestre or usuar_mestre.cod_usuario <> cCodUsuario then do:
            for first usuar_mestre fields(nom_usuario cod_usuario)  
                where usuar_mestre.cod_usuario = cCodUsuario no-lock:
            end.
        end.
        if avail usuar_mestre then do:
            assign vCodigo    = cCodUsuario
                   vDescricao = usuar_mestre.nom_usuario
                   iImage     = 14
                   rRowid     = rowid(usuar_mestre).
        end.         
    end.
    /** Ordem **/
    when "15":U then do:
        if avail mmv-ord-manut then do:
            if mmv-ord-manut.dat-prev-term = ? then do:
                {utp/ut-liter.i "Sem Previs∆o TÇrmino"}
                assign vDescricao = trim(return-value).
            end.
            else do:
                {utp/ut-liter.i "Previs∆o TÇrmino"}
                assign vDescricao = trim(return-value)                  + ": "  + 
                       string(mmv-ord-manut.dat-prev-term,'99/99/9999') + " - " + 
                       string(mmv-ord-manut.hra-prev-term,'99:99:99').
            end.
            assign vCodigo    = string(mmv-ord-manut.nr-ord-produ)
                   iImage     = 15
                   rRowid     = rowid(mmv-ord-manut).
        end.
        else do:
            {utp/ut-liter.i "Ordem Manutená∆o N∆o Criada"}
            assign vCodigo    = "_"
                   vDescricao = trim(return-value)
                   iImage     = 15
                   rRowid     = ?.
        end.
    end.
    /** TAG **/
    WHEN "16":U THEN DO:
        IF NOT AVAIL tag OR tag.cd-tag <> cTag THEN DO:
            FOR FIRST tag WHERE
                tag.cd-tag = cTag NO-LOCK:
            END.
        END.
        IF AVAIL tag THEN DO:
            ASSIGN vCodigo    = IF cTag = "" THEN "_" ELSE cTag
                   iImage     = 16
                   rRowid     = ROWID(tag)
                   vDescricao = tag.descricao.
        END.
        ELSE DO:
            {utp/ut-liter.i Tag_N∆o_Dispon°vel}
                ASSIGN vCodigo    = IF cTag = "" THEN "_" ELSE cTag
                       vDescricao = RETURN-VALUE
                       iImage     = 16
                       rRowid     = ROWID(mab-eqpto).
        END.
    END.

end case.

return "OK":U.

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
           ttSelecao.empresa-ini    = entry(1,c-linha,";")
           ttSelecao.empresa-fim    = entry(2,c-linha,";")
           ttSelecao.equipto-ini    = entry(3,c-linha,";")
           ttSelecao.equipto-fim    = entry(4,c-linha,";")   
           ttSelecao.grupo-ini      = entry(5,c-linha,";")    
           ttSelecao.grupo-fim      = entry(6,c-linha,";")    
           ttSelecao.modelo-ini     = entry(7,c-linha,";")    
           ttSelecao.modelo-fim     = entry(8,c-linha,";")    
           ttSelecao.estab-ini      = entry(9,c-linha,";")          
           ttSelecao.estab-fim      = entry(10,c-linha,";")          
           ttSelecao.centro-ini     = entry(11,c-linha,";")          
           ttSelecao.centro-fim     = entry(12,c-linha,";")          
           ttSelecao.planej-ini     = entry(13,c-linha,";")          
           ttSelecao.planej-fim     = entry(14,c-linha,";")          
           ttSelecao.setor-ini      = entry(15,c-linha,";")          
           ttSelecao.setor-fim      = entry(16,c-linha,";")          
           ttSelecao.oficina-ini    = entry(17,c-linha,";")          
           ttSelecao.oficina-fim    = entry(18,c-linha,";")          
           ttSelecao.evento-ini     = entry(19,c-linha,";")          
           ttSelecao.evento-fim     = entry(20,c-linha,";")
           ttSelecao.tag-ini        = entry(21,c-linha,";")          
           ttSelecao.tag-fim        = entry(22,c-linha,";")
           ttSelecao.dt-corte       = date(entry(23,c-linha,";"))
           ttSelecao.dt-perc-ini    = date(entry(24,c-linha,";"))
           ttSelecao.dt-perc-fim    = date(entry(25,c-linha,";"))
           ttSelecao.lAtivos        = (entry(26,c-linha,";") = "yes")
           ttSelecao.lInativos      = (entry(27,c-linha,";") = "yes")
           ttSelecao.lProprios      = (entry(28,c-linha,";") = "yes")
           ttSelecao.lTerceiros     = (entry(29,c-linha,";") = "yes")
           ttSelecao.lMotor         = (entry(30,c-linha,";") = "yes")
           ttSelecao.lNMotor        = (entry(31,c-linha,";") = "yes")
           ttSelecao.lVencido       = (entry(32,c-linha,";") = "yes")
           ttSelecao.lAVencer       = (entry(33,c-linha,";") = "yes")
           ttSelecao.lOutrosEv      = (entry(34,c-linha,";") = "yes")
           ttSelecao.lServico       = (entry(35,c-linha,";") = "yes")
           ttSelecao.lNTerminada    = (entry(36,c-linha,";") = "yes")
           ttSelecao.lPlanoComp     = (entry(37,c-linha,";") = "yes")
           ttSelecao.lComp          = (entry(38,c-linha,";") = "yes")
           ttSelecao.lOleo          = (entry(39,c-linha,";") = "yes")
           ttSelecao.lMecanica      = (entry(40,c-linha,";") = "yes")
           ttSelecao.lCusto         = (entry(41,c-linha,";") = "yes")
           ttSelecao.lOutros        = (entry(42,c-linha,";") = "yes")
           ttSelecao.lPlanoFuturo   = (entry(43,c-linha,";") = "yes")
           ttSelecao.iPlano         = int(entry(44,c-linha,";"))
           ttSelecao.cod-calend     = entry(45,c-linha,";")
           ttSelecao.iMinutos       = int(entry(46,c-linha,";"))
           ttSelecao.idi-durabilidade = int(entry(47,c-linha,";"))
           ttSelecao.sub-sist-ini     = entry(48,c-linha,";")
           ttSelecao.sub-sist-fim     = entry(49,c-linha,";").
    if ttSelecao.dt-corte < today then do:
        assign ttSelecao.dt-corte    = today
               ttSelecao.dt-perc-ini = today
               ttSelecao.dt-perc-fim = today.
    end.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE chamaPrograma wWindow 
PROCEDURE chamaPrograma :
/*------------------------------------------------------------------------------
  Purpose:     chamaPrograma
  Parameters:  entrada pProgram = Caminho do programa (ex: "abp/ab0303.w")
               entrada pImagem  = Imagem da Vis∆o
               entrada pAtualiz = Atualiza browse
  Notes:       Chama programa de apontamentos de eventos passando o equipamento
               que se deseja incluir o evento.
------------------------------------------------------------------------------*/
define input parameter pProgram as character no-undo.
define input parameter pImagem  as integer   no-undo.
define input parameter pAtualiz as logical   no-undo.

DEFINE VARIABLE iDocto   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iImagem  AS INTEGER    NO-UNDO.
DEFINE VARIABLE rProgram AS ROWID      NO-UNDO.
DEFINE VARIABLE hProgram AS HANDLE     NO-UNDO.

if pImagem = 31 then
    assign iImagem = 15.
ELSE 
    assign iImagem = pImagem.
    
if chTreeView:Nodes:Count > 0 then do:
    if int(chTreeView:SelectedItem:image) = iImagem then do:
        /** Guarda rowid do tree-view **/
        assign rRowid = to-rowid(entry(1,chTreeView:SelectedItem:Tag)).
        for first ttDados
            where rowid(ttDados) = rRowid no-lock:
            assign iDocto   = ttDados.num-docto
                   rProgram = ttDados.r-rowid.
        end.
    end.
    else do:
        if num-results("brDetalhe":U) > 0 then do:
            get current brDetalhe NO-LOCK.
            case ttDados.p-image:
                when iImagem then do:
                    assign iDocto   = ttDados.num-docto
                           rProgram = ttDados.r-rowid.
                end.
                when 30 then do:
                    case ttDados.i-origem:
                        /** Eventos **/
                        when 2 then do:
                            assign iDocto = ttDados.num-docto.
                            for first mab-movto-event
                                where mab-movto-event.num-docto = ttDados.num-docto no-lock:
                                assign rProgram = rowid(mab-movto-event).
                            end.
                        end.
                        /** Ordem Manutená∆o **/
                        when 6 then do:
                            assign iDocto = ttDados.num-docto.
                            if iImagem = 35 then do:
                                for first mmv-plano-prevent
                                    where mmv-plano-prevent.cod-plano = ttDados.cod-plano
                                    and   mmv-plano-prevent.cod-model = ttDados.cod-model no-lock:
                                    assign rProgram = rowid(mmv-plano-prevent).
                                end.
                            end.
                            else do:
                                for first mmv-ord-manut
                                    where mmv-ord-manut.nr-ord-produ = ttDados.num-docto no-lock:
                                    assign rProgram = rowid(mmv-ord-manut).
                                end.
                            end.
                        end.
                        /** Plano Prevená∆o **/
                        when 3 or when 4 or when 7 then do:
                            for first mmv-plano-prevent
                                where mmv-plano-prevent.cod-plano = ttDados.cod-plano
                                and   mmv-plano-prevent.cod-model = ttDados.cod-model no-lock:
                                assign rProgram = rowid(mmv-plano-prevent).
                            end.
                        end.
                    end case.
                end.
            end case.
            if ttDados.p-image <> iImagem and iImagem = 10 then do:
                for first mab-eqpto
                    where mab-eqpto.ep-codigo = ttDados.ep-codigo 
                    and   mab-eqpto.cod-eqpto = ttDados.cod-eqpto no-lock:
                    assign rProgram = rowid(mab-eqpto).
                end.            
            end.
        end.
    end.
end.
if num-results("brDetalhe":U) > 0 then do:
    if pAtualiz then do:
        get current brDetalhe no-lock.
        if avail ttDados and ttDados.sequencia >= iVisaoEqpto then
            assign i-empresa = ttDados.ep-codigo
                   c-eqpto   = ttDados.cod-eqpto.
        else
            assign i-empresa = "":U
                   c-eqpto   = "":U.
    end.
end.
/** Desabilita Timer **/
assign chTimer:Interval = 0.

/*--- Seta cursor do mouse para espera ---*/
SESSION:SET-WAIT-STATE("GENERAL":U).
assign {&window-name}:sensitive = no.
/*--- Executa programa de inclus∆o de filho ---*/
RUN value(pProgram) PERSISTENT SET hProgram.

/* novo mÇtodo de teste do valid-handle */
IF VALID-HANDLE(hProgram) AND
   hProgram:Type = "PROCEDURE":U AND
   hProgram:FILE-NAME = pProgram THEN
    /*--- Inicializa programa de inclus∆o de filho ---*/
    RUN initializeInterface IN hProgram.

IF VALID-HANDLE(hProgram) THEN DO:
    /** Reposiciona programa no registro **/
    if pImagem <> 0 and pImagem <> 31 and rProgram <> ? then
        run repositionRecord in hProgram (input rProgram).
    /** Emiss∆o da Ficha OM **/
    if pImagem = 31 then do:
        if iDocto = 0 AND num-results("brDetalhe":U) > 0 then do:
            get current brDetalhe NO-LOCK.
            IF AVAIL ttdados THEN DO:
                if ttDados.p-image = 30 then
                    assign iDocto   = ttDados.num-docto
                           rProgram = ttDados.r-rowid.
                else 
                    for first mmv-ord-manut
                        where rowid(mmv-ord-manut) = rProgram no-lock:
                        assign iDocto = mmv-ord-manut.nr-ord-produ.
                    end.
            END.
        END.
        if iDocto <> 0 then
            run setaDocumento in hProgram (input iDocto).
    end.
    
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    wait-for "CLOSE":U of hProgram.
END.
assign {&window-name}:sensitive = yes.

if valid-handle(hProgram) then
    delete procedure hProgram.

if num-results("brDetalhe":U) > 0 then do:
    if pAtualiz then do:
        /** Atualiza dados **/
        session:set-wait-state ("GENERAL").
        run piAtualizar in this-procedure (input 2).
        session:set-wait-state ("").
    end.
end.

ASSIGN pImagem = pImagem1.

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

OCXFile = SEARCH( "esmv0613.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    chCtrlFrame-2 = CtrlFrame-2:COM-HANDLE
    UIB_S = chCtrlFrame-2:LoadControls( OCXFile, "CtrlFrame-2":U)
    chCtrlFrame-3 = CtrlFrame-3:COM-HANDLE
    UIB_S = chCtrlFrame-3:LoadControls( OCXFile, "CtrlFrame-3":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "esmv0613.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE criaTreeView wWindow 
PROCEDURE criaTreeView :
/*------------------------------------------------------------------------------
  Purpose:     criaTreeView
  Parameters:  <none>
  Notes:       Cria as linhas do tree-view
------------------------------------------------------------------------------*/
define variable iQtSeq   as integer initial 0 no-undo.
define variable i-img    as integer           no-undo.
define variable TotEqpto as character         no-undo.

define buffer bfttDados for ttDados.
define buffer bfttDadosAux for ttDados.

/** Constroi tree-view, apenas na atualizaá∆o inicial **/
if i-atualiza = 1 then do:
    /** Limpa o tree-view **/
    ChTreeView:Nodes:Clear().

    assign i-cont = 0.
    /** Verifica se existem vis‰es escolhidas **/
    for last ttVisao no-lock:
        assign iQtSeq = ttVisao.sequencia.
    end.
        if iQtSeq = 0 then return.
    /** Label da quantidade de equipamentos **/
    {utp/ut-liter.i "Consumo Real"}
    assign TotEqpto = return-value.

    /** Busca os dados criados **/
    for each  ttDados 
        where ttDados.sequencia <= iQtSeq exclusive-lock 
        by cod-dimensao
        BY desc-dimensao:

      /** Conta sequàncias de linhas **/ 
      assign i-cont           = i-cont + 1
             ttDados.seq-tree = i-cont
             i-img            = ttDados.p-image.
      /** Modifica a Imagem da OM **/
      if i-img = 15 then do:
          if ttDados.lVencido then
              assign i-img = 18.
          else
              if ttDados.lVencer then
                  assign i-img = 17.
              else 
                  assign i-img = 16.
      end.
      /** Inclui primeira linha **/
      if ttDados.sequencia = 1 then do:
         chTreeView:Nodes:Add (,, "i" + string(i-cont), string(ttDados.cod-oficial) + " - " + ttDados.desc-dimensao, i-img) . 
         /** Guarda chave (rowid) da temp-table no tree-view **/
         assign chTreeView:Nodes:Item ("i" + string(i-cont)):Tag = string(rowid(ttDados)) + ",1,".
         if ttDados.r-rowid <> ? then
            assign chTreeView:Nodes:Item ("i" + string(i-cont)):Tag = string(rowid(ttDados)) + ",1," + string(ttDados.r-rowid).
      end.
      else do:
          /** Busca filhos da vis∆o escolhida **/
          find first bfttDados 
               where bfttDados.cod-dimensao = ttDados.cod-dimens-pai no-lock no-error.
          if avail bfttDados then do:
              /** Inclui linhas das vis‰es filhas **/
              chTreeView:Nodes:Add ("i" + string(bfttDados.seq-tree),4 , "i" + string(i-cont), string(ttDados.cod-oficial) + " - " + ttDados.desc-dimensao, i-img) . 
             /** Guarda chave (rowid) da temp-table no tree-view **/
             assign chTreeView:Nodes:Item ("i" + string(i-cont)):Tag = string(rowid(ttDados)) + ",1,". 
             if  ttDados.r-rowid <> ? then
                 assign chTreeView:Nodes:Item ("i" + string(i-cont)):Tag = string(rowid(ttDados)) + ",2," + string(ttDados.r-rowid).
          end.
      end.
    end.
end.

/** Posiciona o Tree-View no £ltimo lido **/
if chTreeView:Nodes:Count > 0 and c-tag-tree <> "":U then do:
    /** Seleciona item **/
    assign chTreeView:Nodes:Item(c-tag-tree):selected = yes no-error.
    /** Atualiza Browse **/
    if not error-status:error then
        run CtrlFrame.TreeView.NodeClick (input chTreeView:Nodes:Item(c-tag-tree)).
end.

return "OK":U.

end PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE criaVisoes wWindow 
PROCEDURE criaVisoes :
/*------------------------------------------------------------------------------
  Purpose:     criaVisoes
  Parameters:  <none>
  Notes:       Cria as vis‰es se n∆o foram selecionadas 
------------------------------------------------------------------------------*/
create ttVisao.
assign ttVisao.dimensao  = '01 ' + fnLabels(1)
       ttVisao.sequencia = 1.
create ttVisao.
assign ttVisao.dimensao =  '02 ' + fnLabels(2)
       ttVisao.sequencia = 2.
create ttVisao.
assign ttVisao.dimensao =  '10 ' + fnLabels(10)
       ttVisao.sequencia = 3.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE expandeItem wWindow 
PROCEDURE expandeItem :
/*------------------------------------------------------------------------------
  Purpose:     expandeItem
  Parameters:  <none>
  Notes:       Expande o tree-view
------------------------------------------------------------------------------*/
def input param p-node   as com-handle no-undo.
def input param p-expcon as log        no-undo.

def var h-child as com-handle no-undo.
def var i-aux   as int        no-undo.

assign p-node:Expanded = p-expcon
       h-child         = p-node:Child.

do i-aux = 1 to p-node:Children:
    assign h-child = h-child:next.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mostraErros wWindow 
PROCEDURE mostraErros :
/*------------------------------------------------------------------------------
  Purpose:     mostraErros
  Parameters:  <none>
  Notes:       Verifica a temp-table de erros da BO e mostra em tela as mensagens
------------------------------------------------------------------------------*/
/** Instancia aplicativo **/
{method/showmessage.i1}
/** Mostra caixa com erros **/
{method/showmessage.i2 &Modal="YES"}
/** Limpa temp-tables de erros **/
empty temp-table RowErrors.

apply "ENTRY":U to {&window-name}.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piAtualizar wWindow 
PROCEDURE piAtualizar :
/*------------------------------------------------------------------------------
  Purpose:     piAtualizar
  Parameters:  entrada p-atualiza = Tipo de atualizaá∆o (1 = Total / 2 = Somente £ltima vis∆o)
  Notes:       Busca os dados e atualiza o programa
------------------------------------------------------------------------------*/
define input parameter p-atualiza as integer no-undo.

define variable i-emp-ini   as character no-undo.
define variable i-emp-fim   as character no-undo.
define variable c-eqpto-ini as character no-undo.
define variable c-eqpto-fim as character no-undo.
define variable rRow        as rowid     no-undo.
DEFINE VARIABLE hAcomp AS HANDLE     NO-UNDO.
assign i-atualiza = p-atualiza.

/** Atualiza todo o tree-view **/
if i-atualiza = 1 then do:
    empty temp-table ttDados.
    {&OPEN-QUERY-brDetalhe}
    
    /** N∆o foram selecionadas as vis‰es **/
    if not can-find(first ttVisao ) then do:
        run criaVisoes in this-procedure.
    end.
    /** Busca ParÉmetros **/
    find first ttSelecao no-lock no-error.
    if not avail ttSelecao then return "NOK":U.
    /** Guarda a £ltima vis∆o **/
    assign iVisao = 0.
    for last ttVisao no-lock:
        assign iVisao = ttVisao.sequencia.
    end.
    for each  ttVisao 
        where substring(ttVisao.dimensao,1,2) = "10":U no-lock:
        assign iVisaoEqpto = ttVisao.sequencia.
    end.
    /** Habilita o Timer **/
    assign chTimer:enabled  = true
           chTimer:Interval = ttSelecao.iMinutos * 60 * 1000
           i-emp-ini        = ttSelecao.empresa-ini
           i-emp-fim        = ttSelecao.empresa-fim
           c-eqpto-ini      = ttSelecao.equipto-ini
           c-eqpto-fim      = ttSelecao.equipto-fim.
end.
/** Atualiza apenas £ltimo n°vel (tarefas) **/
else do:
    if i-empresa = "" then
        assign i-emp-ini   = ttSelecao.empresa-ini
               i-emp-fim   = ttSelecao.empresa-fim
               c-eqpto-ini = ttSelecao.equipto-ini
               c-eqpto-fim = ttSelecao.equipto-fim.
    else 
        assign i-emp-ini   = i-empresa
               i-emp-fim   = i-empresa
               c-eqpto-ini = c-eqpto
               c-eqpto-fim = c-eqpto.
end.

/** Calculas datas necess†rias na consulta **/
run piCalculaInvertidaAVencer in this-procedure.
    /** Acompanhamento **/
    RUN utp~/ut-acomp.p PERSISTENT SET hAcomp.
    {utp~/ut-liter.i "Gerenciador da Manutená∆o"}
    RUN pi-inicializar IN hAcomp (trim(return-value)).
blk_gerenciador:
DO on stop undo, return "NOK":U:
    /** Seleciona os equipamento **/
    for each  mab-eqpto
        where mab-eqpto.ep-codigo         >= i-emp-ini  
        and   mab-eqpto.ep-codigo         <= i-emp-fim  
        and   mab-eqpto.cod-eqpto         >= c-eqpto-ini
        and   mab-eqpto.cod-eqpto         <= c-eqpto-fim
        and   mab-eqpto.cod-model         >= ttSelecao.modelo-ini
        and   mab-eqpto.cod-model         <= ttSelecao.modelo-fim
        and   mab-eqpto.cod-estabel       >= ttSelecao.estab-ini
        and   mab-eqpto.cod-estabel       <= ttSelecao.estab-fim
        and   mab-eqpto.cd-tag            >= ttSelecao.tag-ini
        and   mab-eqpto.cd-tag            <= ttSelecao.tag-fim
        and   mab-eqpto.cc-codigo         >= ttSelecao.centro-ini
        and   mab-eqpto.cc-codigo         <= ttSelecao.centro-fim
        and   mab-eqpto.cod-grp-eqpto     >= ttSelecao.grupo-ini
        and   mab-eqpto.cod-grp-eqpto     <= ttSelecao.grupo-fim
        and ((mab-eqpto.idi-tip-eqpto      = 1 and ttSelecao.lMotor)      /** Motorizados **/
        or   (mab-eqpto.idi-tip-eqpto      = 2 and ttSelecao.lNMotor))    /** N∆o Motorizados **/
        and ((mab-eqpto.idi-tip-propriet   = 1 and ttSelecao.lProprios)   /** Pr¢prios **/
        or   (mab-eqpto.idi-tip-propriet   = 2 and ttSelecao.lTerceiros)) /** Terceiros **/
        and ((mab-eqpto.dat-situacao       = ? and ttSelecao.lAtivos)     /** Ativos **/
        or   (mab-eqpto.dat-situacao      <> ? and ttSelecao.lInativos))  /** Inativos **/
        no-lock:

        assign cTag     = mab-eqpto.cd-tag
               cCCCusto = mab-eqpto.cc-codigo.

        for first mmv-param fields(ep-codigo cod-event-troca log-troca-compon)
            where mmv-param.ep-codigo = mab-eqpto.ep-codigo no-lock:
        end.
        if not avail mmv-param then next.

        /** Atualizaá∆o apenas do £ltimo n°vel **/
        if i-atualiza = 2 then do:
            /** Elimina apenas as tarefas do equipamento **/
            for each  ttDados
                where ttDados.p-image   = 30
                and   ttDados.ep-codigo = mab-eqpto.ep-codigo
                and   ttDados.cod-eqpto = mab-eqpto.cod-eqpto exclusive-lock:
                assign cCodPai = ttDados.cod-dimens-pai.
                delete ttDados.
            end.
        end.
    
        /**********************************************************
        ***       Busca o percurso mÇdio do equipamento          **
        **********************************************************/
        if ttSelecao.dt-corte > today then do:
            run piCalculaKMMedio in this-procedure.
        end.
        /**********************************************************
        ***   Verifica se imprime Eventos Vencidos e a Vencer    **
        **********************************************************/
        if ttSelecao.lVencido or ttSelecao.lAVencer then do:
            assign iOrigemTarefa = 1.
            /** Acompanhamento **/
            {utp~/ut-liter.i "Lubrificaá∆o"}
            RUN pi-Acompanhar IN hAcomp ("Equipamento: " + string(mab-eqpto.ep-codigo) 
                                         + "-" + mab-eqpto.cod-eqpto + ": " + return-value).
            run piBuscaLubrificacoes in this-procedure.
        end.
        /**********************************************************
        **         Verifica se imprime outros eventos            **
        **********************************************************/
        if ttSelecao.lTpEventos then do:
            assign iOrigemTarefa = 2.
            /** Acompanhamento **/
            {utp~/ut-liter.i "Outros Eventos"}
            RUN pi-Acompanhar IN hAcomp ("Equipamento: " + string(mab-eqpto.ep-codigo) 
                                         + "-" + mab-eqpto.cod-eqpto + ": " + return-value).
            run piBuscaOutrosEventos in this-procedure.
        end.
        /**********************************************************
        **               Planos de Prevená∆o                     **
        **********************************************************/
        case ttSelecao.iPlano:
            /*************************************************************
            ** Verifica se h† planos de Manutená∆o Preventiva Vencidos, **
            ** ignorando calend†rio da manutená∆o                       **
            **************************************************************/
            when 1 then do:
                assign iOrigemTarefa = 3.
                /** Acompanhamento **/
                {utp~/ut-liter.i "Plano Equipamento"}
                RUN pi-Acompanhar IN hAcomp ("Equipamento: " + string(mab-eqpto.ep-codigo) 
                                             + "-" + mab-eqpto.cod-eqpto + ": " + RETURN-VALUE).
                run piBuscaPlanosEquipamentos in this-procedure.
            end.
            /*************************************************************
            ** Verifica se h† calend†rio da manutená∆o , ignorando o    **
            ** per°odo de vencimento dos planos de manutená∆o           **
            **************************************************************/
            when 2 then do:
                /**********************************************************
                ***   Verifica se imprime Eventos Vencidos              **
                **********************************************************/
                if ttSelecao.lVencido then do:                       
                    assign iOrigemTarefa = 4.
                    /** Acompanhamento **/
                    {utp~/ut-liter.i "Calend†rio"}
                    RUN pi-Acompanhar IN hAcomp ("Equipamento: " + string(mab-eqpto.ep-codigo) 
                                                 + "-" + mab-eqpto.cod-eqpto + ": " + RETURN-VALUE).
                    run piBuscaCalendario in this-procedure.
                end.
            end.
        end case.
        /**********************************************************
        **           Busca Serviáos em pneus                     **
        **********************************************************/
        if ttSelecao.lServico then do:
            assign iOrigemTarefa = 5.
            /** Acompanhamento **/
            {utp~/ut-liter.i "Serviáos em Pneus"}
            RUN pi-Acompanhar IN hAcomp ("Equipamento: " + string(mab-eqpto.ep-codigo) 
                                         + "-" + mab-eqpto.cod-eqpto + ": " + RETURN-VALUE).
            run piBuscaServicos in this-procedure.
        end.
        /**********************************************************
        **      Busca a Durabilidade do sub-Sistema   * Clovis * **
        **********************************************************/
        if ttSelecao.idi-durabilidade < 4 then do:
            /** Acompanhamento **/
            {utp~/ut-liter.i "Durabilidade"}
            RUN pi-Acompanhar IN hAcomp ("Equipamento: " + string(mab-eqpto.ep-codigo) 
                                         + "-" + mab-eqpto.cod-eqpto + ": " + RETURN-VALUE).
            run piBuscaDurabSubSistema in this-procedure.
        end.
        /** S¢ busca componentes se estiver parametrizado troca ou se forem planos do componente **/
        if ttSelecao.lPlanoComp then do:
            {utp~/ut-liter.i "Componentes"}
            RUN pi-Acompanhar IN hAcomp ("Equipamento: " + string(mab-eqpto.ep-codigo) 
                                         + "-" + mab-eqpto.cod-eqpto + ": " + RETURN-VALUE).
            run piBuscaComponentes in this-procedure.
        end.
    end. /** mab-eqpto **/
END.
/** Mostra o tree-view em tela **/
run criaTreeView in this-procedure.

RUN pi-Finalizar IN hAcomp.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBrowseAtualiza wWindow 
PROCEDURE piBrowseAtualiza :
/*------------------------------------------------------------------------------
  Purpose:     piBrowseAtualiza
  Parameters:  <none>
  Notes:       Mostra registros no browse
------------------------------------------------------------------------------*/
{utp/ut-liter.i "Marcar"}
assign btMarca:sensitive in frame fPage0 = no
       btTodos:sensitive in frame fPage0 = no
       btMarca:label in frame fPage0     = "&" + trim(return-value).

/** Se tiver filhos, mostra no browse, os filhos **/
if avail ttDados then do:
    run piBrowseTitulo in this-procedure (ttDados.p-image).
    /** Retira marcaá∆o da geraá∆o de tarefas **/
    for each  ttDados
        where ttDados.cod-dimens-pai = bfttDados.cod-dimensao no-lock:
        assign ttDados.l-marcado = no.
    end.

  if bfttDados.sequencia = iVisao then do:
      open query brDetalhe for each ttDados use-index vencto
                             where ttDados.cod-dimens-pai = bfttDados.cod-dimensao
                             and   ttDados.lMostra no-lock
                             by    ttDados.dat-vencto
                             by    substring(ttDados.cod-oficial,1,188)
                             BY    substring(ttDados.desc-dimensao,1,188).
  end.
  else
      open query brDetalhe for each ttDados use-index vencto
                             where ttDados.cod-dimens-pai = bfttDados.cod-dimensao no-lock
                             by    ttDados.dat-vencto
                             by    substring(ttDados.cod-oficial,1,188)
                             BY    substring(ttDados.desc-dimensao,1,188).

    if ttDados.p-image = 30 and num-results("brDetalhe":U) > 0 then
        assign btMarca:sensitive in frame fPage0 = yes
               btTodos:sensitive in frame fPage0 = yes.
end.
/** Sen∆o, mostra ele mesmo **/
else do:
    run piBrowseTitulo in this-procedure (bfttDados.p-image).
    open query brDetalhe for each  ttDados
                             where ttDados.cod-dimensao = bfttDados.cod-dimensao no-lock.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBrowseTitulo wWindow 
PROCEDURE piBrowseTitulo :
/*------------------------------------------------------------------------------
  Purpose:     labelBrowse
  Parameters:  entrada pImagem = N£mero da imagem da vis∆o
  Notes:       Coloca o label no browse conforme escolha
------------------------------------------------------------------------------*/
define input parameter pImagem as integer no-undo.

assign c-marcado:visible             in browse brDetalhe = yes
       c-origem-dad:visible          in browse brDetalhe = yes
       c-tip-manut:visible           in browse brDetalhe = yes
       ttDados.cod-plano:visible     in browse brDetalhe = yes
       ttDados.uso-real:visible      in browse brDetalhe = yes
       ttDados.uso-padrao:visible    in browse brDetalhe = yes
       ttDados.diferenca:visible     in browse brDetalhe = yes
       ttDados.un:visible            in browse brDetalhe = yes
       ttDados.dat-vencto:visible    in browse brDetalhe = yes
       ttDados.dat-atualiz:visible   in browse brDetalhe = yes
       ttDados.num-docto:visible     in browse brDetalhe = yes.

ASSIGN brDetalhe:title in frame fPage0 = fnLabels(pImagem)
       ttDados.cod-oficial:label in browse brDetalhe = brDetalhe:title.

if avail ttDados and ttDados.sequencia > iVisao then assign lReal = yes.
                                                else assign lReal = no.

if not(lReal) then
    assign c-marcado:visible             in browse brDetalhe = no
           c-origem-dad:visible          in browse brDetalhe = no
           c-tip-manut:visible           in browse brDetalhe = no
           ttDados.cod-plano:visible     in browse brDetalhe = no
           ttDados.uso-real:visible      in browse brDetalhe = no
           ttDados.uso-padrao:visible    in browse brDetalhe = no
           ttDados.diferenca:visible     in browse brDetalhe = no
           ttDados.un:visible            in browse brDetalhe = no
           ttDados.dat-vencto:visible    in browse brDetalhe = no
           ttDados.dat-atualiz:visible   in browse brDetalhe = no
           ttDados.num-docto:visible     in browse brDetalhe = no.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscaCalendario wWindow 
PROCEDURE piBuscaCalendario :
/*------------------------------------------------------------------------------
  Purpose:     piBuscaCalendario
  Parameters:  <none>
  Notes:       Busca os planos vencidos conforme o calend†rio
------------------------------------------------------------------------------*/
/** Calend†rio sempre vence por dia **/
assign iOrig = 3.

/** Percorre Per°odo entre data atual e a Data de Corte, buscando 
    os eventos vencidos e a vencer em cada data **/
do dt-consulta = today to ttSelecao.dt-corte:
    /* Busca apenas equipamentos vencidos para o dia atual (today) n∆o verificando equipamentos planejados  */
    /* para dia anterior e n∆o realizados.                                                                  */
    /** Calend†rio da Data Atual **/
    for each  mmv-eqpto-calend-plano
        where mmv-eqpto-calend-plano.ep-codigo    = mab-eqpto.ep-codigo
        and   mmv-eqpto-calend-plano.cod-eqpto    = mab-eqpto.cod-eqpto
        and   mmv-eqpto-calend-plano.dat-movto    = dt-consulta 
        and   mmv-eqpto-calend-plano.nr-ord-produ = 0 no-lock:
        if not avail mmv-plano-prevent or 
           mmv-plano-prevent.cod-model <> mab-eqpto.cod-model or 
           mmv-plano-prevent.cod-plano <> mmv-eqpto-calend-plano.cod-plano then do:
           for first mmv-plano-prevent
                where mmv-plano-prevent.cod-model  = mab-eqpto.cod-model
                  and mmv-plano-prevent.cod-plano  = mmv-eqpto-calend-plano.cod-plano     
                  and mmv-plano-prevent.cod-event >= ttSelecao.evento-ini 
                  and mmv-plano-prevent.cod-event <= ttSelecao.evento-fim no-lock:
           end.
        end.
        if not avail mmv-plano-prevent then next.

        if mmv-plano-prevent.cod-sub-sist <> "" then do:
            if (mmv-plano-prevent.cod-sub-sist < ttSelecao.sub-sist-ini
                or mmv-plano-prevent.cod-sub-sist > ttSelecao.sub-sist-fim) then next.
        end.

        /** Filtra Evento e Grupo **/
        run piFiltroEvento in this-procedure (input mmv-plano-prevent.cod-evento).
        if return-value = "NOK":U then next.
    
        /** Busca os planos do Equipamento para saber quando foi a £ltima baixa do mesmo **/
        if not avail mmv-eqpto-plano or mmv-eqpto-plano.ep-codigo <> mab-eqpto.ep-codigo
                                     or mmv-eqpto-plano.cod-eqpto <> mab-eqpto.cod-eqpto 
                                     or mmv-eqpto-plano.cod-plano <> mmv-eqpto-calend-plano.cod-plano then do:
            for first mmv-eqpto-plano 
                where mmv-eqpto-plano.ep-codigo = mab-eqpto.ep-codigo
                and   mmv-eqpto-plano.cod-eqpto = mab-eqpto.cod-eqpto 
                and   mmv-eqpto-plano.cod-plano = mmv-eqpto-calend-plano.cod-plano no-lock:
            end.
        end.
        if not avail mmv-eqpto-plano then next.

        {utp/ut-liter.i "Dia"}
        assign cUN = return-value.

        if mmv-plano-prevent.log-gera-tar-om and 
           can-find(first mmv-tar-plano
                    where mmv-tar-plano.cod-model = mmv-plano-prevent.cod-model
                    and   mmv-tar-plano.cod-plano = mmv-plano-prevent.cod-plano no-lock) then do:
            for each  mmv-tar-plano
                where mmv-tar-plano.cod-model = mmv-plano-prevent.cod-model
                and   mmv-tar-plano.cod-plano = mmv-plano-prevent.cod-plano no-lock:

            if mmv-tar-plano.cod-sub-sist <> "" then do:
                if (mmv-tar-plano.cod-sub-sist < ttSelecao.sub-sist-ini
                    or mmv-tar-plano.cod-sub-sist > ttSelecao.sub-sist-fim) then next.
            end.

                assign iSeq-tar-plano = mmv-tar-plano.num-seq.
                /** Filtra Evento e Grupo **/
                run piFiltroEvento in this-procedure (input mmv-tar-plano.cod-evento).
                if return-value = "NOK":U then next.

                assign lVencido      = yes
                       deReal        = mmv-eqpto-plano.val-km-real
                       dePadrao      = mmv-plano-prevent.val-km-padr
                       cCodSetor     = mmv-tar-plano.cod-setor-ofici
                       cCodOficina   = mmv-eqpto-calend-plano.cod-ofici
                       iOrigOficina  = 2
                       cCodPlanejad  = ""
                       cCodUsuario   = c-seg-usuario
                       cCodSubSist   = mmv-tar-plano.cod-sub-sist
                       dt-dat-vencto = mmv-eqpto-calend-plano.dat-movto.
            
                run piCarrega in this-procedure.
            end.
        end.
        else do:
            assign lVencido      = yes
                   deReal        = mmv-eqpto-plano.val-km-real
                   dePadrao      = mmv-plano-prevent.val-km-padr
                   cCodSetor     = mmv-plano-prevent.cod-setor-ofici
                   cCodOficina   = mmv-eqpto-calend-plano.cod-ofici
                   iOrigOficina  = 2
                   cCodPlanejad  = ""
                   cCodUsuario   = c-seg-usuario
                   cCodSubSist   = mmv-plano-prevent.cod-sub-sist
                   dt-dat-vencto = mmv-eqpto-calend-plano.dat-movto.

            run piCarrega in this-procedure.
        end.
    end.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscaComponentes wWindow 
PROCEDURE piBuscaComponentes :
/*------------------------------------------------------------------------------
  Purpose:     piBuscaComponentes
  Parameters:  <none>
  Notes:       Busca os componentes que est∆o no equipamento
------------------------------------------------------------------------------*/
define variable deKMAcumulado  as decimal                  no-undo.
define variable lEvento        as logical initial yes      no-undo.
DEFINE BUFFER bf-mab-model FOR mab-model.

if not avail mmv-param THEN DO:
    for first mmv-param
        where mmv-param.ep-codigo = mab-eqpto.ep-codigo no-lock:
    END.
END.

IF mmv-param.cod-event-troca >= ttSelecao.evento-ini AND 
    mmv-param.cod-event-troca <= ttSelecao.evento-fim THEN DO:
    if ttSelecao.lPlanoComp then do:
        if avail mmv-param and mmv-param.log-troca-compon and ttSelecao.lComp then do:
            /** Filtra Evento e Grupo **/
            run piFiltroEvento in this-procedure (input mmv-param.cod-event-troca).
            if return-value = "NOK":U then assign lEvento = no.
        end.
    end.
    else do:
        if avail mmv-param and mmv-param.log-troca-compon and ttSelecao.lComp then do:
            /** Filtra Evento e Grupo **/
            run piFiltroEvento in this-procedure (input mmv-param.cod-event-troca).
            if return-value = "NOK":U then return "NOK":U.
        end.
    end.
END.
ELSE
    RETURN "NOK":U.

/** Busca componentes no equipamento **/
for each  mco-compon 
    where mco-compon.ep-codigo-atual = mab-eqpto.ep-codigo
    and   mco-compon.cod-eqpto-atual = mab-eqpto.cod-eqpto no-lock:
    if mco-compon.cod-sub-sist <> "" then do:    
        if (mco-compon.cod-sub-sist < ttSelecao.sub-sist-ini
            or mco-compon.cod-sub-sist > ttSelecao.sub-sist-fim) then next.
    end.
    /**********************************************************
    ***         Busca os planos dos componentes              **
    **********************************************************/
    if ttSelecao.lPlanoComp then do:
        assign iOrigemTarefa = 7.
        /** Busca planos dos componentes  **/
        for each  mco-plano-compon
            where mco-plano-compon.num-seqcial = mco-compon.num-seqcial no-lock:
            /** Calcula se plano est† atrasado e cria tarefas **/
            run piBuscaPlanos in this-procedure (input mco-plano-compon.cod-model,
                                                 input mco-plano-compon.cod-plano,
                                                 input mco-plano-compon.val-km-real,
                                                 input mco-plano-compon.dat-atualiz).
        end.
    end.
    /**********************************************************
    ***  Busca componentes com uso real maior que estimado   **
    **********************************************************/
    if avail mmv-param and mmv-param.log-troca-compon and ttSelecao.lComp and lEvento then do:
        assign iOrigemTarefa = 8.
        for first mco-histor-vida
            where mco-histor-vida.num-seqcial = mco-compon.num-seqcial 
            and   mco-histor-vida.num-vida    = mco-compon.num-vida-atual 
            and   mco-histor-vida.dat-final   = ? no-lock:

            /** Se existir alguma estimativa, calcula **/
            if mco-histor-vida.val-uso-estimad  > 0 or 
               mco-histor-vida.num-dias-estimad > 0 then do:
                
                /** Guarda quilometragem atual **/
                assign deKMAcumulado = mco-histor-vida.val-uso-final.
    
                blk-corte:
                /** Percorre Per°odo entre data atual e a Data de Corte **/
                do dt-consulta = today to ttSelecao.dt-corte:
                    /** Valida as datas posteriores a atual para validar com o calend†rio **/
                    if dt-consulta > today then do:
                        run piFiltroCalendario in this-procedure (input dt-consulta).
                        if return-value = "NOK":U then do:
                            /** Acumula percurso para pr¢xima data **/
                            assign deKMAcumulado = deKMAcumulado + deKMMedio.
                            next.
                        end.
                    end.
                    assign lVencido = no
                           lVencer  = no.
                    /** Verifica se compoenente est† vencido em KM **/
                    if mco-histor-vida.val-uso-estimad > 0 then do:
                        if deKMAcumulado > mco-histor-vida.val-uso-estimad then do:
                            if not(ttSelecao.lVencido) then next.
                            else assign lVencido = yes
                                        deReal   = mco-histor-vida.val-uso-final
                                        dePadrao = mco-histor-vida.val-uso-estimad
                                        iOrig    = 2.
                        end.
                    end.
                    if not(lVencido) then do:
                        /** Verifica em dias **/
                        if mco-histor-vida.num-dias-estimad > 0 then do:
                            if (dt-consulta - mco-histor-vida.dat-inclusao) >= mco-histor-vida.num-dias-estimad then do:
                                if not(ttSelecao.lVencido) then next.
                                else assign lVencido = yes
                                            deReal   = today - mco-histor-vida.dat-inclusao
                                            dePadrao = mco-histor-vida.num-dias-estimad
                                            iOrig    = 3.
                            end.
                        end.
                    end.
                    /* Quando n∆o encontra Vencidos, volta para pr¢ximo registro */
                    if not(lVencido) then do:
                        /** Acumula percurso para pr¢xima data **/
                        assign deKMAcumulado = deKMAcumulado + deKMMedio.
                        next.
                    end.

                    /** Se forem projeá‰es futuras, componentes n∆o estar∆o vencidos **/
                    if dt-consulta > today then do:
                        assign lVencido = no
                               lVencer  = no.
                    end.
        
                    assign cCodSetor     = ""
                           cCodOficina   = ""
                           iOrigOficina  = 2
                           cCodPlanejad  = ""
                           cCodUsuario   = c-seg-usuario
                           cCodSubSist   = mco-compon.cod-sub-sist
                           dt-dat-vencto = today.

                    /** Define a Origem do Vencimento da Tarefa (em dias ou Percurso) **/
                    if iOrig = 3 then do:
                        {utp/ut-liter.i "Dia"}
                        assign cUN = return-value.
                    end.
                    else do:
                        if not avail mab-model or mab-model.cod-model <> mab-eqpto.cod-model then do:
                            for first mab-model
                                where mab-model.cod-model = mab-eqpto.cod-model no-lock:
                            end.
                        end.
                        if avail mab-model THEN DO:
                            assign cUN = mab-model.un.
                            /* Se plano vence pelo secund†rio, busca unidade medida secund†ria */
                            IF mab-eqpto.log-cont-sec THEN DO:
                                FOR FIRST bf-mab-model
                                    WHERE bf-mab-model.cod-model = mco-compon.cod-model NO-LOCK:
                                    /* Somente quando um componente Ç controlado com a mesma unidade de medida 
                                       do contador secund†rio de um equipamento, Ç que utilizar† o uso secund†rio 
                                       deste equipamento para atualizar a durabilidade */
                                    IF bf-mab-model.un = SUBSTRING(mab-model.cod-livre-1,1,2) THEN DO:
                                        assign cUN = SUBSTRING(mab-model.cod-livre-1,1,2).
                                    END.
                                END.
                            END.
                        END.
                    end.
                    run piCarrega in this-procedure.
                    leave blk-corte.
                end.
            end.
        end.
    end.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscaDurabSubSistema wWindow 
PROCEDURE piBuscaDurabSubSistema :
/*------------------------------------------------------------------------------
  Purpose:    piBuscaDurabSubSistema
  Parameters:  <none>
  Notes:     Calcula Durabilidade do Sub-Sistema  - Clovis e Xavier
------------------------------------------------------------------------------*/
DEFINE VARIABLE deKMAcumulado   AS DECIMAL NO-UNDO. 
DEFINE VARIABLE deKMsub-sist    AS DECIMAL NO-UNDO.  
DEFINE VARIABLE deKMeqptoini    AS DECIMAL NO-UNDO.
DEFINE VARIABLE deKMEqptofim    AS DECIMAL NO-UNDO.
DEFINE VARIABLE DtFim           AS DATE    NO-UNDO.
DEFINE VARIABLE iUsoDias        AS INTEGER NO-UNDO.
DEFINE VARIABLE iUsoDiasSubSist AS INTEGER NO-UNDO.


/**Busca evento de troca dos parametros da manutená∆o conforme empresa do eqpto**/
FOR FIRST mmv-param 
    WHERE mmv-param.ep-codigo = mab-eqpto.ep-codigo 
    AND   mmv-param.cod-event-troca >= ttSelecao.evento-ini 
    AND   mmv-param.cod-event-troca <= ttSelecao.evento-fim NO-LOCK: 
END.

    IF NOT AVAIL mmv-param OR not(mmv-param.log-troca-compon) THEN RETURN "NOK":U.
/** Filtra Evento e Grupo **/
    run piFiltroEvento in this-procedure (input mmv-param.cod-event-troca).
        if return-value = "NOK":U then RETURN "NOK":U.
blk-subsist:
FOR EACH mab-relac-eqpto-sub-sist
    WHERE mab-relac-eqpto-sub-sist.ep-codigo = mab-eqpto.ep-codigo 
    AND  mab-relac-eqpto-sub-sist.cod-eqpto = mab-eqpto.cod-eqpto NO-LOCK:

    if mab-relac-eqpto-sub-sist.cod-sub-sist <> "" then do:
        if (mab-relac-eqpto-sub-sist.cod-sub-sist < ttSelecao.sub-sist-ini
            or mab-relac-eqpto-sub-sist.cod-sub-sist > ttSelecao.sub-sist-fim) then next.
    end.

    /** Verifica se o sub-sistema est† controlando a durabilidade **/
    FOR FIRST mab-sub-sist
        WHERE mab-sub-sist.cod-sub-sist = mab-relac-eqpto-sub-sist.cod-sub-sist
        AND mab-sub-sist.log-controla-uso = TRUE NO-LOCK:  
    END.
    /** Se n∆o estiver, pula para pr¢ximo sub-sistema **/
    IF NOT AVAIL mab-sub-sist THEN NEXT blk-subsist.

    IF ttselecao.idi-durabilidade                = 1  AND 
       mab-relac-eqpto-sub-sist.val-uso-estimad  = 0  AND 
       mab-relac-eqpto-sub-sist.num-dias-estimad = 0 THEN NEXT blk-subsist.
    
    IF ttselecao.idi-durabilidade                  = 3 AND 
       mab-relac-eqpto-sub-sist.val-km-hora-gartia = 0 AND 
       mab-relac-eqpto-sub-sist.num-dias-gartia    = 0 THEN NEXT blk-subsist.
    
     IF ttselecao.idi-durabilidade                  = 2 AND 
        mab-relac-eqpto-sub-sist.val-uso-estimad    = 0 AND 
        mab-relac-eqpto-sub-sist.num-dias-estimad   = 0 AND 
        mab-relac-eqpto-sub-sist.val-km-hora-gartia = 0 AND 
        mab-relac-eqpto-sub-sist.num-dias-gartia    = 0 THEN NEXT blk-subsist.


    /** Verifica se plano j† n∆o contÇm tarefas em aberto em OMs **/
    IF CAN-FIND(FIRST mmv-tar-ord-manut
            WHERE mmv-tar-ord-manut.ep-codigo            = mab-eqpto.ep-codigo
            AND   mmv-tar-ord-manut.cod-eqpto            = mab-eqpto.cod-eqpto
            AND   mmv-tar-ord-manut.cod-evento           = mmv-param.cod-event-troca
            AND   mmv-tar-ord-manut.cod-sub-sist         = mab-relac-eqpto-sub-sist.cod-sub-sist
            AND   mmv-tar-ord-manut.val-dat-invrtda-term = 0 NO-LOCK) THEN NEXT blk-subsist.
  
    /** Buscar £ltima troca deste sub-sistema **/
    FOR LAST mmv-tar-ord-manut 
                WHERE mmv-tar-ord-manut.ep-codigo            = mab-eqpto.ep-codigo
                AND   mmv-tar-ord-manut.cod-eqpto            = mab-eqpto.cod-eqpto
                AND   mmv-tar-ord-manut.cod-evento           = mmv-param.cod-event-troca
                AND   mmv-tar-ord-manut.cod-sub-sist         = mab-relac-eqpto-sub-sist.cod-sub-sist
                AND   mmv-tar-ord-manut.val-dat-invrtda-term < deInvertidaAtual NO-LOCK:
    
        /** Busca as Quilometragens do equipamento nas datas do percurso **/
        IF mab-relac-eqpto-sub-sist.log-livre-1 THEN DO:
            RUN piCalculaKM IN THIS-PROCEDURE (INPUT  mab-eqpto.ep-codigo,INPUT  mab-eqpto.cod-eqpto,
                                               INPUT  mmv-tar-ord-manut.val-dat-invrtda-term,
                                               INPUT  3,  /* km secund†rio */
                                               OUTPUT deKMeqptoini).
        END.
        ELSE DO:
            RUN piCalculaKM IN THIS-PROCEDURE (INPUT  mab-eqpto.ep-codigo,INPUT  mab-eqpto.cod-eqpto,
                                               INPUT  mmv-tar-ord-manut.val-dat-invrtda-term,
                                               INPUT  1,  /* km acumulado */
                                               OUTPUT deKMeqptoini).
        END.
        /** Data de termino da ordem de manutená∆o **/
        FOR FIRST bfOrdem 
            WHERE bfOrdem.nr-ord-produ = mmv-tar-ord-manut.nr-ord-produ NO-LOCK:
            ASSIGN DtIni = bfOrdem.dat-term.  
        END.
    END.
    IF NOT AVAIL mmv-tar-ord-manut THEN DO:
       ASSIGN deKMeqptoini = mab-relac-eqpto-sub-sist.val-km-inicial.
              DtIni        = mab-relac-eqpto-sub-sist.dat-ult-troca.   /** Dt Inic do sub-sistema **/
    END.

    /** Buscar vida atual do Equipamento **/
    IF mab-relac-eqpto-sub-sist.log-livre-1 THEN DO:
        RUN piCalculaKM IN THIS-PROCEDURE (INPUT mab-eqpto.ep-codigo,
                                           INPUT mab-eqpto.cod-eqpto,
                                           INPUT deInvertidaAtual,
                                           INPUT 3,  /* km secundario */
                                           OUTPUT deKMeqptofim).
    END.
    ELSE DO:
        RUN piCalculaKM IN THIS-PROCEDURE (INPUT mab-eqpto.ep-codigo,
                                           INPUT mab-eqpto.cod-eqpto,
                                           INPUT deInvertidaAtual,
                                           INPUT 1,  /* km acumulado */
                                           OUTPUT deKMeqptofim).
    END.
    
    /* Grava durabilidade atual do Sub-Sistema */
    ASSIGN DtFim           = TODAY
           deKMsub-sist    = deKMeqptofim - deKMeqptoini   /* durabilidade hoje */
           deKMAcumulado   = deKMeqptofim - deKMeqptoini   /* durabiliade no futuro */
           iUsoDias        = DtFim - DtIni + 1
           iUsoDiasSubSist = DtFim - DtIni + 1.
    
    blk-corte:
    /** Percorre Per°odo entre data atual e a Data de Corte, buscando 
        os eventos vencidos e a vencer em cada data **/
    DO dt-consulta = TODAY TO ttSelecao.dt-corte:
        /** Valida as datas posteriores a atual para validar com o calend†rio **/
        IF dt-consulta > TODAY THEN DO:
           ASSIGN deKMAcumulado = deKMAcumulado + deKMMedio
                  iUsoDias = iUsoDias + 1.
           RUN piFiltroCalendario IN THIS-PROCEDURE (INPUT dt-consulta).
            IF RETURN-VALUE = "NOK":U THEN NEXT blk-corte.
        END.
        ASSIGN lVencido  = NO
               lVencer   = NO
               lGarantia = NO.

        /** Pelo Vencido / ∑ vencer e Todos - Clovis - **/
        IF ttselecao.idi-durabilidade = 1  or
           ttselecao.idi-durabilidade = 2 THEN DO:
            
            IF ttselecao.idi-durabilidade = 2 THEN
                RUN piVerificaGarantia IN THIS-PROCEDURE (INPUT deKMAcumulado,
                                                          INPUT iUsoDias).

            IF lGarantia = NO THEN DO:
            
                /** Verifica se registro est† vencido por km **/
                IF (deKMAcumulado >= mab-relac-eqpto-sub-sist.val-uso-estimad)
                    AND mab-relac-eqpto-sub-sist.val-uso-estimad > 0 THEN DO: /* Xavier na UC*/
                    if not(ttSelecao.lVencido) then next blk-corte.
                    else assign lVencido = yes
                                iOrig    = 2.
                END.
                /** Verifica se registro esta vencido em Dias **/
                ELSE DO:
                    IF (iUsoDias >= mab-relac-eqpto-sub-sist.num-dias-estimad)
                       AND mab-relac-eqpto-sub-sist.num-dias-estimad > 0 THEN DO: /* Xavier na UC*/   
                        if not(ttSelecao.lVencido) then next blk-corte.
                        else assign lVencido = yes
                                    iOrig    = 3.
                    END.
                END.
                /** Verifica se registro est† a vencer **/
                IF NOT (lVencido) THEN DO:
                    IF (deKMAcumulado < mab-relac-eqpto-sub-sist.val-uso-estimad) AND
                       ((deKMAcumulado + mab-relac-eqpto-sub-sist.val-km-aviso) >= mab-relac-eqpto-sub-sist.val-uso-estimad)
                       AND mab-relac-eqpto-sub-sist.val-uso-estimad > 0 THEN DO: /* Xavier na UC*/   
                        if not(ttSelecao.lAVencer) then next blk-corte.
                        else assign lVencer = yes
                                    iOrig   = 2.
                    END.
                    ELSE DO:
                        IF (iUsoDias < mab-relac-eqpto-sub-sist.num-dias-estimad) AND 
                           ((iUsoDias + mab-relac-eqpto-sub-sist.num-dias-antecip) >= mab-relac-eqpto-sub-sist.num-dias-estimad)
                           AND mab-relac-eqpto-sub-sist.num-dias-estimad > 0 THEN DO: /* Xavier na UC*/ 
                            if not(ttSelecao.lAVencer) then next blk-corte.
                            else assign lvencer = yes
                                        iOrig   = 3.
                        END.
                    END.
                END.
                /**Verifica se Vencido e ∑ vencer est∆o nulos vai para o pr¢ximo,
                   quando for todos, n∆o verifica este parÉmetro **/
                IF ttselecao.idi-durabilidade = 1 THEN DO:
                    IF lVencer = NO AND lVencido = NO THEN NEXT blk-corte.
                END.
                ELSE DO:
                    IF lVencer = NO AND lVencido = NO AND lGarantia = NO THEN DO:
                        IF mab-eqpto.idi-tip-eqpto = 1 THEN
                            ASSIGN iOrig = 2.
                        ELSE
                            ASSIGN iOrig = 3.
                    END.
                END.

            END.
            
        END.
        /** Verifica pela Garantia  **/
        IF ttselecao.idi-durabilidade = 3 THEN DO:
            RUN piVerificaGarantia IN THIS-PROCEDURE (INPUT deKMAcumulado,
                                                      INPUT iUsoDias).
            IF lVencer = NO AND lVencido = NO AND lGarantia = NO THEN NEXT blk-corte.
        END.

        IF iOrig = 2 THEN DO:
            ASSIGN deReal        = deKMsub-sist. /* Durabilidade do sistema hoje */
            IF ttSelecao.idi-durabilidade = 3 THEN
                ASSIGN dePadrao = mab-relac-eqpto-sub-sist.val-km-hora-gartia.
            ELSE DO:
                IF ttSelecao.idi-durabilidade = 2 THEN DO:
                    IF lGarantia = YES THEN
                        ASSIGN dePadrao = mab-relac-eqpto-sub-sist.val-km-hora-gartia.
                    ELSE
                        ASSIGN dePadrao = mab-relac-eqpto-sub-sist.val-uso-estimad.
                END.
                ELSE DO:
                    ASSIGN dePadrao = mab-relac-eqpto-sub-sist.val-uso-estimad.
                END.
            END.
        END.
        ELSE DO: 
            ASSIGN deReal       = iUsoDiasSubSist /* Durabilidade do sistema hoje */.
            IF ttSelecao.idi-durabilidade = 3 THEN
                ASSIGN dePadrao = mab-relac-eqpto-sub-sist.num-dias-gartia.
            ELSE DO:
                IF ttSelecao.idi-durabilidade = 2 THEN DO:
                    IF lGarantia = YES THEN
                        ASSIGN dePadrao = mab-relac-eqpto-sub-sist.num-dias-gartia.
                    ELSE
                        ASSIGN dePadrao = mab-relac-eqpto-sub-sist.num-dias-estimad.
                END.
                ELSE DO:
                    ASSIGN dePadrao = mab-relac-eqpto-sub-sist.num-dias-estimad.
                END.
            END.
        END.

        IF lGarantia THEN
            ASSIGN iOrigemTarefa = 10.
        ELSE
            ASSIGN iOrigemTarefa = 9.
                
        ASSIGN cCodSetor     = ""
               cCodOficina   = ""
               iOrigOficina  = 2
               cCodPlanejad  = ""
               cCodUsuario   = c-seg-usuario
               cCodSubSist   = mab-relac-eqpto-sub-sist.cod-sub-sist
               dt-dat-vencto = dt-consulta.

        /** Define a Origem do Vencimento da Tarefa (em dias ou Percurso) **/
        if iOrig = 3 then do:
            {utp/ut-liter.i "Dia"}
            assign cUN = return-value.
        end.
        else do:
            if not avail mab-model or mab-model.cod-model <> mab-eqpto.cod-model then do:
                for first mab-model
                    where mab-model.cod-model = mab-eqpto.cod-model no-lock:
                end.
            end.
            if avail mab-model THEN DO:
                assign cUN = mab-model.un.
                /* Se sub-sistema vence pelo secund†rio, busca unidade medida secund†ria */
                IF mab-relac-eqpto-sub-sist.log-livre-1 THEN
                    assign cUN = SUBSTRING(mab-model.cod-livre-1,1,2).
            END.
        end.

        /** Carrega os dados **/
        RUN piCarrega IN THIS-PROCEDURE.
        LEAVE blk-corte.
    END.
END.
RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscaLubrificacoes wWindow 
PROCEDURE piBuscaLubrificacoes :
/*------------------------------------------------------------------------------
  Purpose:     piBuscaLubrificacoes
  Parameters:  <none>
  Notes:       Busca os registros de Vencidos e a Vencer
------------------------------------------------------------------------------*/
define variable deKMAcumulado  as decimal no-undo.
DEFINE VARIABLE l-existe       AS LOGICAL NO-UNDO.

/** Busca fichas de abastecimento e lubrificaá∆o **/
for each  mab-period-manut
    where mab-period-manut.ep-codigo   = mab-eqpto.ep-codigo
    and   mab-period-manut.cod-eqpto   = mab-eqpto.cod-eqpto
    and   mab-period-manut.cod-evento >= ttSelecao.evento-ini
    and   mab-period-manut.cod-evento <= ttSelecao.evento-fim no-lock:
    /** Filtra Evento e Grupo **/
    run piFiltroEvento in this-procedure (input mab-period-manut.cod-evento).
    if return-value = "NOK":U then next.

    for first mab-comptmento-eqpto
        where mab-comptmento-eqpto.ep-codigo      = mab-eqpto.ep-codigo
        and   mab-comptmento-eqpto.cod-eqpto      = mab-eqpto.cod-eqpto
        and   mab-comptmento-eqpto.cod-comptmento = mab-period-manut.cod-comptmento no-lock:
    END.

    IF NOT AVAIL mab-comptmento-eqpto THEN NEXT.

    IF mab-comptmento-eqpto.cod-sub-sist <> "" then do:
        if (mab-comptmento-eqpto.cod-sub-sist < ttSelecao.sub-sist-ini or 
            mab-comptmento-eqpto.cod-sub-sist > ttSelecao.sub-sist-fim) then next.
    END.

    ASSIGN l-existe = NO.

    /** Verifica se plano j† n∆o contÇm tarefas em aberto em OMs **/
    if can-find(first mmv-tar-ord-manut
                where mmv-tar-ord-manut.ep-codigo            =  mab-eqpto.ep-codigo
                and   mmv-tar-ord-manut.cod-eqpto            =  mab-eqpto.cod-eqpto
                and   mmv-tar-ord-manut.cod-evento           =  mab-period-manut.cod-evento
                and   mmv-tar-ord-manut.cod-sub-sist         =  mab-comptmento-eqpto.cod-sub-sist
                and   mmv-tar-ord-manut.val-dat-invrtda-term =  0
                AND   mmv-tar-ord-manut.estado               <> 3 no-lock) then next.
    ELSE DO:
        FOR EACH  mmv-ord-manut
            WHERE mmv-ord-manut.ep-codigo   = mab-eqpto.ep-codigo
            AND   mmv-ord-manut.cod-eqpto   = mab-eqpto.cod-eqpto
            AND   mmv-ord-manut.idi-tip-ord = 1
            AND   mmv-ord-manut.estado      < 7 NO-LOCK:
            /** Verifica se plano j† n∆o contÇm tarefas em OMs em aberto **/
            if can-find(first mmv-tar-ord-manut
                        where mmv-tar-ord-manut.nr-ord-produ = mmv-ord-manut.nr-ord-produ
                        and   mmv-tar-ord-manut.cod-evento   = mab-period-manut.cod-evento
                        and   mmv-tar-ord-manut.cod-sub-sist = mab-comptmento-eqpto.cod-sub-sist no-lock) THEN DO:
                ASSIGN l-existe = YES.
                LEAVE.
            END.
        END.
        IF l-existe THEN NEXT.
    END.

    assign deKMAcumulado = mab-period-manut.val-km-real.

    blk-corte:
    /** Percorre Per°odo entre data atual e a Data de Corte, buscando os eventos vencidos e a vencer em cada data **/
    do dt-consulta = today to ttSelecao.dt-corte:
        /** Se n∆o visualiza eventos vencidos, deixa o bloco **/
        if not(ttSelecao.lVencido) and (deKMAcumulado > mab-period-manut.val-km-padr) then
            leave blk-corte.

        /** Busca ficha n∆o consistida para equipamento que contenha a lubrificaá∆o do compartimento vencido,
            caso encontre algum n∆o consistido, n∆o mostra a lubrificaá∆o como vencida, pois existem inconsistencias 
            a serem revisadas no AB0402 para mostrar o vencimento correto da lubrificaá∆o no MV0613 **/
        FOR EACH  mab-abastec-lubrific USE-INDEX mbbstclb-02
            WHERE mab-abastec-lubrific.ep-codigo   = mab-comptmento-eqpto.ep-codigo
            AND   mab-abastec-lubrific.cod-eqpto   = mab-comptmento-eqpto.cod-eqpto
            AND   mab-abastec-lubrific.log-consist = NO NO-LOCK:
            FOR FIRST mab-item-lubrific
                WHERE mab-item-lubrific.num-docto      = mab-abastec-lubrific.num-docto
                AND   mab-item-lubrific.cod-comptmento = mab-comptmento-eqpto.cod-comptmento
                AND   mab-item-lubrific.cod-evento     = mab-period-manut.cod-evento NO-LOCK:
                leave blk-corte.
            END.
        END.

        /** Valida as datas posteriores a atual para validar com o calend†rio **/
        if dt-consulta > today then do:
            run piFiltroCalendario in this-procedure (input dt-consulta).
            if return-value = "NOK":U then do:
                assign deKMAcumulado = deKMAcumulado + deKMMedio.
                next.
            END.
        END.

        /** Evento n∆o est† vencido e nem est† a vencer **/
        if (deKMAcumulado + mab-period-manut.val-km-estimad) < mab-period-manut.val-km-padr then next.
        assign lVencido = no
               lVencer  = no.

        /** Verifica se registro est† vencido **/
        if (deKMAcumulado >= mab-period-manut.val-km-padr) then do:
            if not(ttSelecao.lVencido) then do:
                assign deKMAcumulado = deKMAcumulado + deKMMedio.
                next.
            END.
            else assign lVencido = yes.
        END.

        /** Verifica se registro est† a vencer **/
        if not(lVencido) then
            if (deKMAcumulado < mab-period-manut.val-km-padr) and
              ((deKMAcumulado + mab-period-manut.val-km-estimad) >= mab-period-manut.val-km-padr) then do:
                if not(ttSelecao.lAVencer) then do:
                    assign deKMAcumulado = deKMAcumulado + deKMMedio.
                    next.
                END.
                else assign lVencer = yes.
            END.

        assign deReal        = mab-period-manut.val-km-real
               dePadrao      = mab-period-manut.val-km-padr
               iOrig         = 1
               cCodSetor     = ""
               cCodOficina   = ""
               iOrigOficina  = 2
               cCodPlanejad  = ""
               cCodUsuario   = c-seg-usuario
               cCodSubSist   = if avail mab-comptmento-eqpto then mab-comptmento-eqpto.cod-sub-sist else ""
               dt-dat-vencto = dt-consulta.

        if not avail mab-model or mab-model.cod-model <> mab-eqpto.cod-model then do:
            for first mab-model
                where mab-model.cod-model = mab-eqpto.cod-model no-lock:
            END.
        END.
        if avail mab-model THEN DO:
            assign cUN = mab-model.un.
            /* Se sub-sistema vence pelo secund†rio, busca unidade medida secund†ria */
            IF mab-eqpto.log-cont-sec THEN DO:
                FOR FIRST mab-param
                    WHERE mab-param.cdn-param = 50 NO-LOCK:
                    /** 1 = Prim†rio 
                        2 = Secund†rio **/
                    IF mab-param.des-valor = "2" THEN
                        assign cUN = SUBSTRING(mab-model.cod-livre-1,1,2).
                END.
            END.
        END.

        run piCarrega in THIS-PROCEDURE.
        leave blk-corte.
    END.
END.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscaOutrosEventos wWindow 
PROCEDURE piBuscaOutrosEventos :
/*------------------------------------------------------------------------------
  Purpose:     piBuscaOutrosEventos
  Parameters:  <none>
  Notes:       Busca outros eventos que est∆o vencidos
------------------------------------------------------------------------------*/
define variable lListar       as logical no-undo.
define variable deInvIni      as decimal no-undo.
define variable deInvFim      as decimal no-undo.
define variable deKMIni       as decimal no-undo.
define variable deKMFim       as decimal no-undo.
define variable deKMAcumulado as decimal no-undo.

define buffer bfKM for mab-movto-km-eqpto.

for each  mab-movto-event
    where mab-movto-event.ep-codigo    = mab-eqpto.ep-codigo
    and   mab-movto-event.cod-eqpto    = mab-eqpto.cod-eqpto
    and   mab-movto-event.cod-evento  >= ttSelecao.evento-ini
    and   mab-movto-event.cod-evento  <= ttSelecao.evento-fim
    and   mab-movto-event.dat-final    = ? 
    and   mab-movto-event.log-disparad = no no-lock:

    if mab-movto-event.cod-sub-sist <> "" then do:
        if (mab-movto-event.cod-sub-sist < ttSelecao.sub-sist-ini  or
            mab-movto-event.cod-sub-sist > ttSelecao.sub-sist-fim) then next.
    end.

    /** Filtra Evento e Grupo **/
    run piFiltroEvento in this-procedure (input mab-movto-event.cod-evento).
    if return-value = "NOK":U then next.

    blk-corte:
    /** Percorre Per°odo entre data atual e a Data de Corte, **/
    /** buscando os eventos vencidos e a vencer em cada data **/
    do dt-consulta = today to ttSelecao.dt-corte:
        /** Valida as datas posteriores a atual para validar com o calend†rio **/
        if dt-consulta > today then do:
            run piFiltroCalendario in this-procedure (input dt-consulta).
            if return-value = "NOK":U then next.
        end.
        assign lListar = no.

        /** Verifica KM/H **/
        if dt-consulta = today then do:
            /** Converte as datas para Data/Hora Invertidas **/
            run converteParaHoraInvertida (input  string(mab-movto-event.dat-inicial,"99/99/9999"),
                                           input  mab-movto-event.hra-inicial,
                                           output deInvIni).

            run converteParaHoraInvertida (input  string(dt-consulta,"99/99/9999"),
                                           input  "235959",
                                           output deInvFim).
            
            /** Busca as quilometragens do equipamento **/

            run piCalculaKM in this-procedure (input  mab-eqpto.ep-codigo,
                                               input  mab-eqpto.cod-eqpto,
                                               input  deInvIni,
                                               input  1,
                                               output deKMIni).

            run piCalculaKM in this-procedure (input  mab-eqpto.ep-codigo,
                                               input  mab-eqpto.cod-eqpto,
                                               input  deInvFim,
                                               input  1,
                                               output deKMFim).

            assign deKMAcumulado = deKMFim - deKMIni.
            /** Verifica se Quilometragem venceu **/
            if (deKMFim - deKMIni) >= mab-movto-event.val-km-aviso then do:
                assign lListar  = yes
                       deReal   = deKMFim - deKMIni
                       dePadrao = mab-movto-event.val-km-aviso
                       iOrig    = 2.
            end.
            else do:
                /** Verifica por data de aviso **/
                if mab-movto-event.dat-aviso <= dt-consulta then do:
                    assign lListar  = yes
                           deReal   = today - mab-movto-event.dat-inicial
                           dePadrao = mab-movto-event.dat-aviso - mab-movto-event.dat-inicial
                           iOrig    = 3.
                end.
            end.    
        end.
        else do:
            /** Acumula a Quilometragem conforme datas percorridas **/
            assign deKMAcumulado = deKMAcumulado + deKMMedio.
            /** Verifica se Quilometragem venceu com quilometragem acumulada **/
            if deKMAcumulado >= mab-movto-event.val-km-aviso then do:
                assign lListar  = yes
                       deReal   = deKMFim - deKMIni
                       dePadrao = mab-movto-event.val-km-aviso
                       iOrig    = 2.
            end.
            else do:
                /** Verifica por data de aviso **/
                if mab-movto-event.dat-aviso <= dt-consulta then do:
                    assign lListar  = yes
                           deReal   = today - mab-movto-event.dat-inicial
                           dePadrao = mab-movto-event.dat-aviso - mab-movto-event.dat-inicial
                           iOrig    = 3.
                end.
            end.
        end.

        if lListar then do:
            assign lVencido      = no
                   lVencer       = no
                   cCodSetor     = "":U
                   cCodOficina   = mab-movto-event.cod-ofici
                   iOrigOficina  = 2
                   cCodPlanejad  = "":U
                   cCodUsuario   = mab-movto-event.cod-usuar
                   cCodSubSist   = mab-movto-event.cod-sub-sist
                   dt-dat-vencto = if iOrig = 3 then mab-movto-event.dat-aviso else dt-consulta.

            if deReal - dePadrao > 0 then do:
                if not(ttSelecao.lVencido) then next.
                else assign lVencido = yes.
            end.
            else do:
                if deReal - dePadrao = 0 then 
                    if not(ttSelecao.lAVencer) then next.
                    else assign lVencer = yes.
            end.

            /** Define a Origem do Vencimento da Tarefa (em dias ou Percurso) **/
            if iOrig = 3 then do:
                {utp/ut-liter.i "Dia"}
                assign cUN = return-value.
            end.
            else do:
                if not avail mab-model or mab-model.cod-model <> mab-eqpto.cod-model then do:
                    for first mab-model
                        where mab-model.cod-model = mab-eqpto.cod-model no-lock:
                    end.
                end.
                if avail mab-model THEN DO:
                    assign cUN = mab-model.un.
                END.
            end.

            run piCarrega in this-procedure.
            leave blk-corte.
        end.
    end.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscaPlanos wWindow 
PROCEDURE piBuscaPlanos :
/*------------------------------------------------------------------------------
  Purpose:     piBuscaPlanos
  Parameters:  entrada pModelo = C¢digo do modelo do plano
               entrada pPlano  = C¢digo do plano de prevená∆o
               entrada pKM     = Valor da Quilometragem 
               entrada pData   = Data de atualizaá∆o
  Notes:       Busca os planos vencidos para o equipamento / componente
------------------------------------------------------------------------------*/
define input parameter pModelo as character                no-undo.
define input parameter pPlano  as character                no-undo.
define input parameter pKM     as decimal                  no-undo.
define input parameter pData   as date format "99/99/9999" no-undo.

define variable deKMAcumulado  as decimal                  no-undo.
define variable dtAcumulado    as date format "99/99/9999" no-undo.

if not avail mmv-plano-prevent or 
   mmv-plano-prevent.cod-model <> pModelo or 
   mmv-plano-prevent.cod-plano <> pPlano then do:
   for first  mmv-plano-prevent 
        where mmv-plano-prevent.cod-model  = pModelo
          and mmv-plano-prevent.cod-plano  = pPlano no-lock:
   end.
end.
if not avail mmv-plano-prevent then next.

if mmv-plano-prevent.cod-sub-sist <> "" then do:
    if (mmv-plano-prevent.cod-sub-sist < ttSelecao.sub-sist-ini
        or mmv-plano-prevent.cod-sub-sist > ttSelecao.sub-sist-fim) then next.
end.

IF not(mmv-plano-prevent.log-gera-tar-om) THEN DO:
    /** Filtra Evento e Grupo **/
    IF mmv-plano-prevent.cod-evento < ttSelecao.evento-ini OR mmv-plano-prevent.cod-evento > ttSelecao.evento-fim THEN RETURN "OK":U.
    run piFiltroEvento in this-procedure (input mmv-plano-prevent.cod-evento).
    if return-value = "NOK":U then RETURN "OK":U.
END.

assign deKMAcumulado = pKM
       dtAcumulado   = pData.

blk-corte:
/** Percorre Per°odo entre data atual e a Data de Corte, buscando 
    os eventos vencidos e a vencer em cada data **/
do dt-consulta = today to ttSelecao.dt-corte:
    /** Valida as datas posteriores a atual para validar com o calend†rio **/
    if dt-consulta > today then do:
        run piFiltroCalendario in this-procedure (input dt-consulta).
        if return-value = "NOK":U then do:
            /** Acumula percurso para pr¢xima data **/
            IF mmv-plano-prevent.log-cont-sec THEN DO: 
                assign deKMAcumulado = deKMAcumulado + deKMMedioSec.
            next.
            END.
            ELSE DO:
                /** Acumula percurso para pr¢xima data **/
                assign deKMAcumulado = deKMAcumulado + deKMMedio.
                next.
            END.
        end.
    end.
    assign lVencido = no
           lVencer  = no.
    /** Verifica se registro est† vencido **/
    if mmv-plano-prevent.val-km-padr > 0 then do:
        if (deKMAcumulado >= mmv-plano-prevent.val-km-padr) then do:
            if not(ttSelecao.lVencido) then next.
            else assign lVencido = yes.
        end.
        /** Verifica se registro est† a vencer **/
        if not(lVencido) then do:
            if (deKMAcumulado < mmv-plano-prevent.val-km-padr) and
              ((deKMAcumulado + mmv-plano-prevent.val-km-aviso) >= mmv-plano-prevent.val-km-padr) then do:
                if not(ttSelecao.lAVencer) then next.
                else assign lVencer = yes.
            end.
        end.
        assign deReal   = pKM
               dePadrao = mmv-plano-prevent.val-km-padr
               iOrig    = 2.
    end.
    /** Somente calcula se n∆o estiver vencido em KM **/
    if not(lVencido) then do:
        if mmv-plano-prevent.vli-dia > 0 then do:
            if (dt-consulta - dtAcumulado >= mmv-plano-prevent.vli-dia) then do:
                if not(ttSelecao.lVencido) then next.
                else assign lVencido = yes.
            end.
            /** Verifica se registro est† a vencer **/
            if not(lVencido) then do:
                if (dt-consulta - dtAcumulado < mmv-plano-prevent.vli-dia) and
                       ((dt-consulta - dtAcumulado + mmv-plano-prevent.vli-dia-aviso) >= mmv-plano-prevent.vli-dia) then do:
                    if not(ttSelecao.lAVencer) then next.
                    else assign lVencer = yes.
                end.
            end.
            assign deReal   = today - dtAcumulado
                   dePadrao = mmv-plano-prevent.vli-dia
                   iOrig    = 3.
        end.
    end.
    /* Quando n∆o encontra Planos Preventiva Vencidos, volta para pr¢ximo registro */
    if not(lVencer) and not(lVencido) then do:
        IF mmv-plano-prevent.log-cont-sec THEN DO: 
            /** Acumula percurso para pr¢xima data **/
            assign deKMAcumulado = deKMAcumulado + deKMMedioSec.
            next.
        END.
        ELSE DO:
            /** Acumula percurso para pr¢xima data **/
            assign deKMAcumulado = deKMAcumulado + deKMMedio.
            next.
        END.
    end.

    /** Se forem projeá‰es futuras, planos n∆o estar∆o vencidos **/
    if dt-consulta > today then do:
        assign lVencido = no
               lVencer  = no.
    end.

    if mmv-plano-prevent.log-gera-tar-om and 
       can-find(first mmv-tar-plano
                where mmv-tar-plano.cod-model = mmv-plano-prevent.cod-model
                and   mmv-tar-plano.cod-plano = mmv-plano-prevent.cod-plano no-lock) then do:
        blk-tar-plano:
        for each  mmv-tar-plano
            where mmv-tar-plano.cod-model  =  mmv-plano-prevent.cod-model
            and   mmv-tar-plano.cod-plano  =  mmv-plano-prevent.cod-plano
            AND   mmv-tar-plano.cod-event >= ttSelecao.evento-ini
            AND   mmv-tar-plano.cod-event <= ttSelecao.evento-fim NO-LOCK:

        if mmv-tar-plano.cod-sub-sist <> "" then do:
            if (mmv-tar-plano.cod-sub-sist < ttSelecao.sub-sist-ini
                or mmv-tar-plano.cod-sub-sist > ttSelecao.sub-sist-fim) then next.
        end.

            assign iSeq-tar-plano = mmv-tar-plano.num-seq.

            /** Filtra Evento e Grupo **/
            run piFiltroEvento in this-procedure (input mmv-tar-plano.cod-evento).
            if return-value = "NOK":U then NEXT blk-tar-plano.
            /** Verifica se plano j† n∆o contÇm tarefas em aberto em OMs **/
            if can-find(first mmv-tar-ord-manut
                        where mmv-tar-ord-manut.ep-codigo            = mab-eqpto.ep-codigo
                        and   mmv-tar-ord-manut.cod-eqpto            = mab-eqpto.cod-eqpto
                        and   mmv-tar-ord-manut.cod-evento           = mmv-tar-plano.cod-evento
                        and   mmv-tar-ord-manut.cod-sub-sist         = mmv-tar-plano.cod-sub-sist
                        AND   mmv-tar-ord-manut.estado               < 3 
                        and   mmv-tar-ord-manut.val-dat-invrtda-term = 0 no-lock) then do:
                /** Se existir previs∆o futura, busca £ltima data da OM para data acumulada **/
                if ttSelecao.dt-corte > today then
                    run piCalculaDataPlano (input        mmv-tar-plano.cod-evento,
                                            input        mmv-tar-plano.cod-sub-sist,
                                            input-output dtAcumulado).
                
                /**Conforme veficado com o marcio FO da Protege  e Cia-norte 1186044 - 1186935**/
/*                /** Se n∆o visulaliza planos no futuro, sai do loop **/ */
/*                 if not(ttSelecao.lPlanoFuturo) then                    */
/*                     leave blk-corte.                                   */
                   NEXT blk-tar-plano.
            end.
            else do:
                FOR EACH  mmv-ord-manut
                    WHERE mmv-ord-manut.ep-codigo = mab-eqpto.ep-codigo
                    AND   mmv-ord-manut.cod-eqpto = mab-eqpto.cod-eqpto
                    AND   mmv-ord-manut.estado    < 7 NO-LOCK:
                    /** Verifica se plano j† n∆o contÇm tarefas em aberto em OMs **/
                    if can-find(first mmv-tar-ord-manut
                                where mmv-tar-ord-manut.nr-ord-produ = mmv-ord-manut.nr-ord-produ
                                and   mmv-tar-ord-manut.cod-evento   = mmv-tar-plano.cod-evento
                                and   mmv-tar-ord-manut.cod-sub-sist = mmv-tar-plano.cod-sub-sist no-lock) then do:
                        NEXT blk-tar-plano.
                    END.
                END.
                /** Fica sempre com data de vencimento maior **/
                assign dtAcumulado = if dt-consulta > dtAcumulado 
                                     then dt-consulta 
                                     else dtAcumulado.
            end.

            /** Busca Oficina do Setor **/
            if not avail mmv-setor-ofici or mmv-setor-ofici.cod-setor-ofici <> mmv-tar-plano.cod-setor-ofici then do:
                for first mmv-setor-ofici fields(des-setor-ofici cod-setor-ofici cod-ofici)  
                    where mmv-setor-ofici.cod-setor-ofici = mmv-tar-plano.cod-setor-ofici no-lock:
                end.
            end.
            if avail mmv-setor-ofici then
                assign cCodOficina  = mmv-setor-ofici.cod-ofici
                       iOrigOficina = 2.
            assign cCodSetor     = mmv-tar-plano.cod-setor-ofici
                   cCodPlanejad  = ""
                   cCodUsuario   = c-seg-usuario
                   cCodSubSist   = mmv-tar-plano.cod-sub-sist
                   dt-dat-vencto = dt-consulta
                   deKMAcumulado = 0.
        
            /** Define a Origem do Vencimento da Tarefa (em dias ou Percurso) **/
            if iOrig = 3 then do:
                {utp/ut-liter.i "Dia"}
                assign cUN = return-value.
            end.
            else do:
                if not avail mab-model or mab-model.cod-model <> mab-eqpto.cod-model then do:
                    for first mab-model
                        where mab-model.cod-model = mab-eqpto.cod-model no-lock:
                    end.
                end.
                if avail mab-model THEN DO:
                    assign cUN = mab-model.un.
                    /* Se plano vence pelo secund†rio, busca unidade medida secund†ria */
                    IF mmv-plano-prevent.log-cont-sec THEN
                        assign cUN = SUBSTRING(mab-model.cod-livre-1,1,2).
                END.
            end.

            run piCarrega in this-procedure.
        end.
    end.
    else do:
        /* Filtro por plano */
        IF mmv-plano-prevent.cod-evento < ttSelecao.evento-ini OR mmv-plano-prevent.cod-evento > ttSelecao.evento-fim THEN LEAVE blk-corte.
        /** Verifica se plano j† n∆o contÇm tarefas em aberto em OMs **/
        if can-find(first mmv-tar-ord-manut
                    where mmv-tar-ord-manut.ep-codigo            = mab-eqpto.ep-codigo
                    and   mmv-tar-ord-manut.cod-eqpto            = mab-eqpto.cod-eqpto
                    and   mmv-tar-ord-manut.cod-evento           = mmv-plano-prevent.cod-evento
                    and   mmv-tar-ord-manut.cod-sub-sist         = mmv-plano-prevent.cod-sub-sist
                    AND   mmv-tar-ord-manut.estado               < 3
                    and   mmv-tar-ord-manut.val-dat-invrtda-term = 0 no-lock) then do:
            /** Se existir previs∆o futura, busca £ltima data da OM para data acumulada **/
            if ttSelecao.dt-corte > today then
                run piCalculaDataPlano (input        mmv-plano-prevent.cod-evento,
                                        input        mmv-plano-prevent.cod-sub-sist,
                                        input-output dtAcumulado).
            IF mmv-plano-prevent.log-cont-sec THEN DO:
                /** Acumula percurso para pr¢xima data **/
                assign deKMAcumulado = deKMAcumulado + deKMMedioSec.
            END.
            ELSE DO:
                /** Acumula percurso para pr¢xima data **/
                assign deKMAcumulado = deKMAcumulado + deKMMedio.
            END.
            /** Se n∆o visulaliza planos no futuro, sai do loop **/
            if not(ttSelecao.lPlanoFuturo) then
                leave blk-corte.
            next.
        end.
        else do:
            FOR EACH  mmv-ord-manut
                WHERE mmv-ord-manut.ep-codigo = mab-eqpto.ep-codigo
                AND   mmv-ord-manut.cod-eqpto = mab-eqpto.cod-eqpto
                AND   mmv-ord-manut.estado    < 7 NO-LOCK:
                /** Verifica se plano j† n∆o contÇm tarefas em aberto em OMs **/
                if can-find(first mmv-tar-ord-manut
                            where mmv-tar-ord-manut.nr-ord-produ = mmv-ord-manut.nr-ord-produ
                            and   mmv-tar-ord-manut.cod-evento   = mmv-plano-prevent.cod-evento
                            and   mmv-tar-ord-manut.cod-sub-sist = mmv-plano-prevent.cod-sub-sist no-lock) then do:
                    leave blk-corte.
                END.
            END.
            /** Fica sempre com data de vencimento maior **/
            assign dtAcumulado = if dt-consulta > dtAcumulado 
                                 then dt-consulta 
                                 else dtAcumulado.
        end.
        /** Busca Oficina do Setor **/
        if not avail mmv-setor-ofici or mmv-setor-ofici.cod-setor-ofici <> mmv-plano-prevent.cod-setor-ofici then do:
            for first mmv-setor-ofici fields(des-setor-ofici cod-setor-ofici cod-ofici)  
                where mmv-setor-ofici.cod-setor-ofici = mmv-plano-prevent.cod-setor-ofici no-lock:
            end.
        end.
        if avail mmv-setor-ofici then
            assign cCodOficina  = mmv-setor-ofici.cod-ofici
                   iOrigOficina = 2.
        assign cCodSetor     = mmv-plano-prevent.cod-setor-ofici
               cCodPlanejad  = ""
               cCodUsuario   = c-seg-usuario
               cCodSubSist   = mmv-plano-prevent.cod-sub-sist
               dt-dat-vencto = dt-consulta
               dtAcumulado   = dt-consulta
               deKMAcumulado = 0.

        /** Define a Origem do Vencimento da Tarefa (em dias ou Percurso) **/
        if iOrig = 3 then do:
            {utp/ut-liter.i "Dia"}
            assign cUN = return-value.
        end.
        else do:
            if not avail mab-model or mab-model.cod-model <> mab-eqpto.cod-model then do:
                for first mab-model
                    where mab-model.cod-model = mab-eqpto.cod-model no-lock:
                end.
            end.
            if avail mab-model THEN DO:
                assign cUN = mab-model.un.
                /* Se plano vence pelo secund†rio, busca unidade medida secund†ria */
                IF mmv-plano-prevent.log-cont-sec THEN
                    assign cUN = SUBSTRING(mab-model.cod-livre-1,1,2).
            END.
        end.
    
        run piCarrega in this-procedure.
    end.
    
    /** Soma na data de £ltimo vencimento a quantidade de dias que dura o plano **/
    assign dtAcumulado = dtAcumulado + truncate(mmv-plano-prevent.val-tempo-padr / 24,0).

    /** Se n∆o visulaliza planos no futuro, sai do loop **/
    if not(ttSelecao.lPlanoFuturo) then
        leave blk-corte.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscaPlanosEquipamentos wWindow 
PROCEDURE piBuscaPlanosEquipamentos :
/*------------------------------------------------------------------------------
  Purpose:     piBuscaPlanos
  Parameters:  <none>
  Notes:       Busca os planos vencidos para o equipamento
------------------------------------------------------------------------------*/
/** Busca fichas de abastecimento e lubrificaá∆o **/
for each  mmv-eqpto-plano
    where mmv-eqpto-plano.ep-codigo = mab-eqpto.ep-codigo
    and   mmv-eqpto-plano.cod-eqpto = mab-eqpto.cod-eqpto no-lock:
    /** Calcula se plano est† atrasado e cria tarefas **/
    run piBuscaPlanos in this-procedure (input mab-eqpto.cod-model,
                                         input mmv-eqpto-plano.cod-plano,
                                         input mmv-eqpto-plano.val-km-real,
                                         input mmv-eqpto-plano.dat-atualiz).
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscaServicos wWindow 
PROCEDURE piBuscaServicos :
/*------------------------------------------------------------------------------
  Purpose:     piBuscaServicos
  Parameters:  <none>
  Notes:       Busca os serviáos dos pneus do equipamento
------------------------------------------------------------------------------*/
/** Serviáos de Pneus sempre vence por dia **/
assign iOrig = 3.

/** Busca serviáos no equipamento **/
for each  mpn-eqpto-serv
    where mpn-eqpto-serv.ep-codigo   = mab-eqpto.ep-codigo
    and   mpn-eqpto-serv.cod-eqpto   = mab-eqpto.cod-eqpto 
    and   mpn-eqpto-serv.dat-final  >= ttSelecao.dt-corte no-lock:
    IF NOT AVAIL mpn-serv OR  mpn-serv.cod-servico <> mpn-eqpto-serv.cod-servico THEN DO:
        FOR FIRST mpn-serv FIELDS (cod-servico des-servico cod-evento cd-tipo) 
            WHERE mpn-serv.cod-servico = mpn-eqpto-serv.cod-servico 
            AND   mpn-serv.cod-evento >= ttSelecao.evento-ini
            AND   mpn-serv.cod-evento <= ttSelecao.evento-fim NO-LOCK:
        END.
    END.
    IF NOT AVAIL mpn-serv THEN NEXT.
    /** Filtra Evento e Grupo **/
    run piFiltroEvento in this-procedure (input mpn-serv.cod-evento).
    if return-value = "NOK":U then next.

    blk-corte:
    /** Percorre Per°odo entre data atual e a Data de Corte, buscando 
        os eventos vencidos e a vencer em cada data **/
    do dt-consulta = today to ttSelecao.dt-corte:

        /** Valida as datas posteriores a atual para validar com o calend†rio **/
        if dt-consulta > today then do:
            run piFiltroCalendario in this-procedure (input dt-consulta).
            if return-value = "NOK":U then next.
        end.
        assign lVencido = no
               lVencer  = no.

        /** Verifica se registro est† vencido **/
        if mpn-eqpto-serv.num-dias > 0 then do:
            if (dt-consulta - mpn-eqpto-serv.dat-trans >= mpn-eqpto-serv.num-dias) then
                if not(ttSelecao.lVencido) then next.
                else assign lVencido = yes.
            assign deReal   = today - mpn-eqpto-serv.dat-trans
                   dePadrao = mpn-eqpto-serv.num-dias.
        end.
        /* Quando n∆o encontra Planos Preventiva Vencidos, volta para pr¢ximo registro */
        if not(lVencer) and not(lVencido) then next.
        if dt-consulta > today then
            assign lVencido = no
                   lVencer  = no.
        assign cCodSetor     = ""
               cCodOficina   = ""
               iOrigOficina  = 2
               cCodPlanejad  = ""
               cCodUsuario   = c-seg-usuario
               cCodSubSist   = ""
               dt-dat-vencto = dt-consulta.
    
        {utp/ut-liter.i "Dia"}
        assign cUN = return-value.

        run piCarrega in this-procedure.
        leave blk-corte.
    end.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCalculaDataPeriodo wWindow 
PROCEDURE piCalculaDataPeriodo :
/*------------------------------------------------------------------------------
  Purpose:     piCalculaDataPeriodo
  Parameters:  <none>
  Notes:       Busca a data da £ltima atualizaá∆o feita para o evento no sub-sistema
------------------------------------------------------------------------------*/
define output parameter pData as date format "99/99/9999" no-undo.

/** Verifica se existem apontamentos de item de lubrificaá∆o **/
if can-find(first mab-item-lubrific
            where mab-item-lubrific.ep-codigo             = mab-eqpto.ep-codigo
            and   mab-item-lubrific.cod-eqpto             = mab-eqpto.cod-eqpto
            and   mab-item-lubrific.cod-comptmento        = mab-period-manut.cod-comptmento
            and   mab-item-lubrific.cod-evento            = mab-period-manut.cod-evento
            and   mab-item-lubrific.val-dat-hora-invrtda <= deInvertidaAtual no-lock) then do:
    for last  mab-item-lubrific
        where mab-item-lubrific.ep-codigo             = mab-eqpto.ep-codigo
        and   mab-item-lubrific.cod-eqpto             = mab-eqpto.cod-eqpto
        and   mab-item-lubrific.cod-comptmento        = mab-period-manut.cod-comptmento
        and   mab-item-lubrific.cod-evento            = mab-period-manut.cod-evento
        and   mab-item-lubrific.val-dat-hora-invrtda <= deInvertidaAtual no-lock:
        /** Utiliza data do £ltimo apontamento **/
        assign pData = date(integer(substring(string(mab-item-lubrific.val-dat-hora-invrtda),5,2)),  /** Màs **/
                            integer(substring(string(mab-item-lubrific.val-dat-hora-invrtda),7,2)),  /** Dia **/
                            integer(substring(string(mab-item-lubrific.val-dat-hora-invrtda),1,4))). /** Ano **/
    end.
end.
else do:
    /** Busca data de in°cio do campartimento no equipamento **/
    if not avail mab-comptmento-eqpto or mab-comptmento-eqpto.ep-codigo      <> mab-eqpto.ep-codigo
                                      or mab-comptmento-eqpto.cod-eqpto      <> mab-eqpto.cod-eqpto
                                      or mab-comptmento-eqpto.cod-comptmento <> mab-period-manut.cod-comptmento then do:
        for first mab-comptmento-eqpto 
            where mab-comptmento-eqpto.ep-codigo      = mab-eqpto.ep-codigo
            and   mab-comptmento-eqpto.cod-eqpto      = mab-eqpto.cod-eqpto
            and   mab-comptmento-eqpto.cod-comptmento = mab-period-manut.cod-comptmento no-lock:
            assign pData = mab-comptmento-eqpto.dat-inicial.
        end.
    end.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCalculaDataPlano wWindow 
PROCEDURE piCalculaDataPlano :
/*------------------------------------------------------------------------------
  Purpose:     piCalculaDataPlano
  Parameters:  entrada       pEvento = C¢digo do evento
               entrada       pSub    = C¢digo do sub-sistema
               entrada-sa°da pData   = Data acumulada
  Notes:       Calcula a data do plano de manutená∆o futuro
------------------------------------------------------------------------------*/
define input        parameter pEvento  as character                no-undo.
define input        parameter pSub     as character                no-undo.
define input-output parameter pData    as date format "99/99/9999" no-undo.

/** Busca £ltima tarefa do plano **/
for last  mmv-tar-ord-manut
    where mmv-tar-ord-manut.ep-codigo            = mab-eqpto.ep-codigo
    and   mmv-tar-ord-manut.cod-eqpto            = mab-eqpto.cod-eqpto
    and   mmv-tar-ord-manut.cod-evento           = pEvento
    and   mmv-tar-ord-manut.cod-sub-sist         = pSub
    and   mmv-tar-ord-manut.val-dat-invrtda-term = 0 no-lock,
    /** busca OM **/
    first mmv-ord-manut
    where mmv-ord-manut.nr-ord-produ = mmv-tar-ord-manut.nr-ord-produ no-lock:
    if mmv-ord-manut.dat-prev-term <> ? and mmv-ord-manut.dat-prev-term > pData then
        if mmv-ord-manut.dat-prev-term < today then
            assign pData = today.
        else
            assign pData = mmv-ord-manut.dat-prev-term.
end.

if avail mmv-ord-manut then
    release mmv-ord-manut.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCalculaDataTarefa wWindow 
PROCEDURE piCalculaDataTarefa :
/*------------------------------------------------------------------------------
  Purpose:     piCalculaDataTarefa
  Parameters:  sa°da pData = Data da £ltima manutená∆o
  Notes:       Busca a data da £ltima manutená∆o do evento no sub-sistema 
------------------------------------------------------------------------------*/
define output parameter pData as date format "99/99/9999" no-undo.

define buffer bfTarefa for mmv-tar-ord-manut.

if can-find(first bfTarefa
            where bfTarefa.ep-codigo             = mmv-tar-ord-manut.ep-codigo
            and   bfTarefa.cod-eqpto             = mmv-tar-ord-manut.cod-eqpto
            and   bfTarefa.cod-evento            = mmv-tar-ord-manut.cod-evento
            and   bfTarefa.cod-sub-sist          = mmv-tar-ord-manut.cod-sub-sist
            and   bfTarefa.val-dat-invrtda-term <= mmv-ord-manut.val-dat-invrtda-abert
            and   bfTarefa.val-dat-invrtda-term >  0 no-lock) then do:
    for first bfTarefa fields(val-dat-invrtda-term)
        where bfTarefa.ep-codigo             = mmv-tar-ord-manut.ep-codigo
        and   bfTarefa.cod-eqpto             = mmv-tar-ord-manut.cod-eqpto
        and   bfTarefa.cod-evento            = mmv-tar-ord-manut.cod-evento
        and   bfTarefa.cod-sub-sist          = mmv-tar-ord-manut.cod-sub-sist
        and   bfTarefa.val-dat-invrtda-term <= mmv-ord-manut.val-dat-invrtda-abert
        and   bfTarefa.val-dat-invrtda-term >  0 no-lock:
        assign pData = date(integer(substring(string(bfTarefa.val-dat-invrtda-term),5,2)),  /** Màs **/
                            integer(substring(string(bfTarefa.val-dat-invrtda-term),7,2)),  /** Dia **/
                            integer(substring(string(bfTarefa.val-dat-invrtda-term),1,4))). /** Ano **/
    end.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCalculaInvertidaAVencer wWindow 
PROCEDURE piCalculaInvertidaAVencer :
/*------------------------------------------------------------------------------
  Purpose:     piCalculaInvertidaAVencer
  Parameters:  <none>
  Notes:       Calcula as datas necess†rias na consulta
------------------------------------------------------------------------------*/
DEFINE VARIABLE dtAVencer  AS DATE format "99/99/9999" NO-UNDO.
DEFINE VARIABLE deHoraAux  AS DECIMAL                  NO-UNDO.
DEFINE VARIABLE deHora     AS DECIMAL                  NO-UNDO.

assign deHora = ttSelecao.deAntecipa.

/** Busca a Data/hora Invertida Atual para o C†lculo **/
run piFormataSegundoParaHora (input  time,
                              output c-hora).
run converteParaHoraInvertida (input  string(today,'99/99/9999'),
                               input  c-hora,
                               output deInvertidaAtual).
run converteNormalparaDecimal (input  c-hora,
                               output deHora).
assign deInvertidaAVencer = dec(substring(string(deInvertidaAtual),1,8) + "0000")
       deHoraAux          = ttSelecao.deAntecipa + deHora
       dtAVencer          = today.
/** Verifica se hora est† acima de 1 dia **/
repeat:
    if deHoraAux >= 24 then
        assign dtAVencer = dtAVencer + 1
               deHoraAux = deHoraAux - 24.
    else
        leave.
end.
run converteDecimalparaNormal (input  deHoraAux,
                               output c-hora).
/** Converte data encontrada em invertida **/
run converteParaHoraInvertida (input  string(dtAVencer,"99/99/9999"),
                               input  c-hora,
                               output deInvertidaAVencer).
return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCalculaKM wWindow 
PROCEDURE piCalculaKM :
/*------------------------------------------------------------------------------
  Purpose:     buscaKM
  Parameters:  entrada pEmp       = C¢digo da Empresa
               entrada pEqpto     = C¢digo do Equipamento
               entrada pInvertida = Data/Hora Invertida
               entrada pTipo      = Qual campo retornar (1 = Percurso / 2 = Contador / 3 = Percurso Secund†rio / 4 = Contador Secund†rio)
               sa°da   pKM        = Valor da Quilometragem na data
  Notes:       Busca a quilometragem do equipamento em uma data
------------------------------------------------------------------------------*/
define input  parameter pEmp       as character no-undo.
define input  parameter pEqpto     as character no-undo.
define input  parameter pInvertida as decimal   no-undo.
define input  parameter pTipo      as integer   no-undo.
define output parameter pKM        as decimal   no-undo.

/** Busca Quilometragem do Hist¢rico **/
if can-find(first mab-movto-km-eqpto
            where mab-movto-km-eqpto.ep-codigo             = pEmp
            and   mab-movto-km-eqpto.cod-eqpto             = pEqpto
            and   mab-movto-km-eqpto.val-dat-hora-invrtda <= pInvertida no-lock) then do:
    for last  mab-movto-km-eqpto fields(val-km-real val-hodom-horim val-dat-hora-invrtda val-livre-1 val-livre-2)
        where mab-movto-km-eqpto.ep-codigo             = pEmp
        and   mab-movto-km-eqpto.cod-eqpto             = pEqpto
        and   mab-movto-km-eqpto.val-dat-hora-invrtda <= pInvertida no-lock:
        case pTipo:
            when 1 then do:
                assign pKM = mab-movto-km-eqpto.val-km-real.
            end.
            when 2 then do:
                assign pKM = mab-movto-km-eqpto.val-hodom-horim.
            end.
            when 3 then do:
                assign pKM = mab-movto-km-eqpto.val-livre-2. /*Referente a val-km-real-sec*/
            end.
            when 4 then do:
                assign pKM = mab-movto-km-eqpto.val-livre-1. /*Referente a val-hodom-horim-sec*/
            end.
        end case.
    end.
end.
else do:
    /** Utiliza Quilometragem Inicial **/
    case pTipo:
        when 1 then do:
            assign pKM = mab-eqpto.val-km-inicial.
        end.
        when 2 then do:
            assign pKM = mab-eqpto.val-hodom-horim-inicial.
        end.
        when 3 then do:
            assign pKM = mab-eqpto.val-km-sec.
        end.
        when 4 then do:
            assign pKM = mab-eqpto.val-hodom-horim-sec.
        end.
    end case.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCalculaKMMedio wWindow 
PROCEDURE piCalculaKMMedio :
/*------------------------------------------------------------------------------
  Purpose:     piCalculaKMMedio
  Parameters:  <none>
  Notes:       Calcula a quilometragem mÇdia por dia do equipamento para fazer
               a projeá∆o futura do percurso de cada equipamento
------------------------------------------------------------------------------*/
define variable deInvIni as decimal no-undo.
define variable deInvFim as decimal no-undo.
define variable deKMIni  as decimal no-undo.
define variable deKMFim  as decimal no-undo.

/** Converte as datas de percurso para Data/Hora Invertidas **/
run converteParaHoraInvertida (input  string(ttSelecao.dt-perc-ini,"99/99/9999"),
                               input  "000000",
                               output deInvIni).
run converteParaHoraInvertida (input  string(ttSelecao.dt-perc-fim,"99/99/9999"),
                               input  "235959",
                               output deInvFim).

/** Busca as Quilometragens do equipamento na datas do percurso **/
/* Validaá∆o Contador Secundario */
run piCalculaKM in this-procedure (input  mab-eqpto.ep-codigo,
                                   input  mab-eqpto.cod-eqpto,
                                   input  deInvIni,
                                   input  3,
                                   output deKMIni).
run piCalculaKM in this-procedure (input  mab-eqpto.ep-codigo,
                                   input  mab-eqpto.cod-eqpto,
                                   input  deInvFim,
                                   input  3,
                                   output deKMFim).
/** Calcula a mÇdia de Quilometragem do equipamento, 
    verificando o n£mero de dias do percurso **/
assign deKMMedioSec = (deKMFim - deKMIni) / ((ttSelecao.dt-perc-fim - ttSelecao.dt-perc-ini) + 1).

run piCalculaKM in this-procedure (input  mab-eqpto.ep-codigo,
                                   input  mab-eqpto.cod-eqpto,
                                   input  deInvIni,
                                   input  1,
                                   output deKMIni).
run piCalculaKM in this-procedure (input  mab-eqpto.ep-codigo,
                                   input  mab-eqpto.cod-eqpto,
                                   input  deInvFim,
                                   input  1,
                                   output deKMFim).

/** Calcula a mÇdia de Quilometragem do equipamento, 
    verificando o n£mero de dias do percurso **/
assign deKMMedio = (deKMFim - deKMIni) / ((ttSelecao.dt-perc-fim - ttSelecao.dt-perc-ini) + 1).

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCarrega wWindow 
PROCEDURE piCarrega :
/*------------------------------------------------------------------------------
  Purpose:     piCarrega
  Parameters:  entrada pEvento = C¢digo do evento
  Notes:       Chama a criaá∆o das vis‰es.
------------------------------------------------------------------------------*/
/** Criaá∆o total do tree-view **/
if i-atualiza = 1 then do:
    /** Primeiro da lista Ç o Pai, ent∆o n∆o contÇm pai **/
    assign cCodPai  = "".
    /** Busca vis‰es escolhidas **/
    for each ttVisao no-lock:
        /** Busca c¢digo e descriá∆o dos registros da vis∆o **/
        run buscaValor in this-procedure (input ttVisao.dimensao).
        /** Verifica se encontrou c¢digo, sen∆o busca pr¢ximo **/
        if vCodigo = "" then next.
        /** Carrega os dados  **/
        run piCarregaSequencias in this-procedure.
    end.
end.
else do:
    /** Recria as tarefas do equipamento **/
    run piCarregaTarefas in this-procedure.
end.

return "OK":U.

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
define variable c-vCodigo as character no-undo.

case iImage:
    /** Màs/Ano **/
    when 7 then do:
        assign c-vCodigo = substring(vCodigo,4,4) + substring(vCodigo,1,2).
    end.
    /** Data **/
    when 8 then do:
        assign c-vCodigo = substring(vCodigo,7,4) + substring(vCodigo,4,2) + substring(vCodigo,1,2).
    end.
    otherwise
        assign c-vCodigo = vCodigo.
end case.

/** Busca registro **/
find first ttDados 
     where ttDados.cod-dimensao  = (cCodPai + "#" + c-vCodigo) /*usar a # quando o cadastro do eqpto Ç feito 1,10,2,20 sendo que o correto seria fazer 01,10,02,20 */ 
     and   ttDados.sequencia     = ttVisao.sequencia exclusive-lock no-error.
/** Se n∆o estiver criado, cria registro **/
if not avail ttDados then do:
   create ttDados.
   assign ttDados.cod-dimensao   = (cCodPai + "#" + c-vCodigo)  /** C¢digo encadeado para tree-view **/
          ttDados.cod-oficial    = vCodigo                /** C¢digo Original do registro **/
          ttDados.desc-dimensao  = vDescricao             /** Descriá∆o do c¢digo **/
          ttDados.sequencia      = ttVisao.sequencia      /** Sequància dos dados **/
          ttDados.p-image        = iImage                 /** N£mero da imagem do registro **/
          ttDados.r-rowid        = rRowid                 /** Rowid do registro para consultas futuras **/
          ttDados.cod-dimens-pai = cCodPai                /** C¢digo do pai do registro **/
          ttDados.lVencido       = lVencido
          ttDados.lVencer        = lVencer
          ttDados.un             = cUN.

   IF ttDados.p-image = 7 THEN DO:
       ASSIGN ttdados.dat-vencto = DATE (int(substring(vCodigo,1,2)),01,INT(substring(vCodigo,4,4))).
   END.
   /** Guarda Equipamento na TT para vis‰es do equipamento para baaixo **/
   if ttDados.sequencia >= iVisaoEqpto then do:
       assign ttDados.ep-codigo = mab-eqpto.ep-codigo
              ttDados.cod-eqpto = mab-eqpto.cod-eqpto.
   end.
end.

assign cCodPai = ttDados.cod-dimensao.

/* Cria um n°vel a mais quando for a £ltima dimens∆o  */
if ttDados.sequencia = iVisao then do:
    run piCarregaTarefas in this-procedure.
end.



return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCarregaTarefas wWindow 
PROCEDURE piCarregaTarefas :
/*------------------------------------------------------------------------------
  Purpose:     piCarregaTarefas
  Parameters:  <none>
  Notes:       Cria a vis∆o das tarefas 
------------------------------------------------------------------------------*/
DEFINE VARIABLE iDocumento      AS INTEGER                         NO-UNDO.
DEFINE VARIABLE cOrigem         AS CHARACTER                       NO-UNDO.
DEFINE VARIABLE c-cod-evento    AS CHARACTER                       NO-UNDO.
DEFINE VARIABLE c-cod-sub-sist  AS CHARACTER                       NO-UNDO.
DEFINE VARIABLE i-cd-tipo       AS INTEGER                         NO-UNDO.
DEFINE VARIABLE i-num-docto     AS INTEGER                         NO-UNDO.
DEFINE VARIABLE c-cod-plano     AS CHARACTER                       NO-UNDO.
DEFINE VARIABLE c-cod-model     AS CHARACTER                       NO-UNDO.
DEFINE VARIABLE c-narrativa     LIKE mmv-tar-ord-manut.dsl-obs     NO-UNDO.
DEFINE VARIABLE l-mostra        AS LOGICAL                         NO-UNDO.
DEFINE VARIABLE i-val-km-padr   LIKE mmv-plano-prevent.val-km-padr NO-UNDO.
DEFINE VARIABLE cCodDimens      AS CHARACTER FORMAT "x(300)"       NO-UNDO.
DEFINE VARIABLE cCodDimensAnt   AS CHARACTER FORMAT "x(300)"       NO-UNDO.
DEFINE BUFFER   bfTarefaPlano   FOR mmv-tar-plano.

assign iImage = 30.

case iOrigemTarefa:
    /** Lubrificaá‰es (LUB) **/
    when 1 then do:
        assign cOrigem        = "LUB":U + mab-comptmento-eqpto.cod-comptmento
               vCodigo        = mab-period-manut.cod-evento
               c-cod-evento   = mab-period-manut.cod-evento
               c-cod-sub-sist = mab-comptmento-eqpto.cod-sub-sist
               i-cd-tipo      = 0
               i-num-docto    = 0
               i-val-km-padr  = 0
               c-cod-plano    = "":U
               c-cod-model    = "":U               
               rRowid         = rowid(mab-period-manut).
               vDescricao     = fnDescricao(mab-period-manut.cod-evento,mab-comptmento-eqpto.cod-sub-sist) + " Material: ".

               /* Busca quais itens de estoque podem ser aplicados a esta tarefa */
               for each mab-tip-mater-item fields (cod-tip-mater it-codigo)
                   where mab-tip-mater-item.cod-tip-mater = mab-comptmento-eqpto.cod-tip-mater no-lock:
                   assign vDescricao = vDescricao + ", " + trim(mab-tip-mater-item.it-codigo).
               end.

               /*Exibe a capacidade do compartimento como sugest∆o de quantidade de material a ser aplicado */
               assign vDescricao = vDescricao + " Qtd: " + string(mab-comptmento-eqpto.qtd-capac).
               assign c-narrativa = vDescricao.

        run piCalculaDataPeriodo in this-procedure (output dt-dat-atualiz).
    end.
    /** Outros Eventos (EVT) **/
    when 2 then do:
        assign cOrigem        = "EVT":U + string(mab-movto-event.num-docto)
               vCodigo        = mab-movto-event.cod-evento
               c-cod-evento   = mab-movto-event.cod-evento
               c-cod-sub-sist = mab-movto-event.cod-sub-sist
               i-cd-tipo      = mab-movto-event.cd-tipo
               i-num-docto    = mab-movto-event.num-docto
               c-cod-plano    = "":U
               c-cod-model    = "":U
               dt-dat-atualiz = ?
               vDescricao     = fnDescricao(mab-movto-event.cod-evento,mab-movto-event.cod-sub-sist)
               c-narrativa    = mab-movto-event.dsl-obs
               rRowid         = rowid(mab-movto-event).
    end.
    /** Planos Equipamento (PLE) **/
    when 3 then do:
            if not(mmv-plano-prevent.log-gera-tar-om) or 
              (mmv-plano-prevent.log-gera-tar-om      and 
               not can-find(first bfTarefaPlano
                            where bfTarefaPlano.cod-model = mmv-plano-prevent.cod-model
                            and   bfTarefaPlano.cod-plano = mmv-plano-prevent.cod-plano no-lock)) then do:
                assign cOrigem        = "PLE":U + string(dt-dat-vencto)
                       vCodigo        = mmv-plano-prevent.cod-plano
                       c-cod-evento   = mmv-plano-prevent.cod-evento
                       c-cod-sub-sist = mmv-plano-prevent.cod-sub-sist
                       i-cd-tipo      = mmv-plano-prevent.cd-tipo
                       i-num-docto    = 0
                       i-val-km-padr  = mmv-plano-prevent.val-km-padr
                       c-cod-plano    = mmv-plano-prevent.cod-plano
                       c-cod-model    = mmv-plano-prevent.cod-model
                       dt-dat-atualiz = mmv-eqpto-plano.dat-atualiz
                       vDescricao     = fnDescricao(mmv-plano-prevent.cod-evento,mmv-plano-prevent.cod-sub-sist)
                       c-narrativa    = vDescricao
                       rRowid         = rowid(mmv-eqpto-plano).
            end.
            else do:
                assign iSeq-tar-plano  = mmv-tar-plano.num-seq.
                assign cOrigem        = "PLE":U + string(dt-dat-vencto)
                       vCodigo        = mmv-plano-prevent.cod-plano + "-" + string(mmv-tar-plano.num-seq)
                       c-cod-evento   = mmv-tar-plano.cod-evento
                       c-cod-sub-sist = mmv-tar-plano.cod-sub-sist
                       i-cd-tipo      = mmv-plano-prevent.cd-tipo
                       i-num-docto    = 0
                       i-val-km-padr  = mmv-plano-prevent.val-km-padr
                       c-cod-plano    = mmv-plano-prevent.cod-plano
                       c-cod-model    = mmv-plano-prevent.cod-model
                       dt-dat-atualiz = mmv-eqpto-plano.dat-atualiz
                       vDescricao     = fnDescricao(mmv-tar-plano.cod-evento,mmv-tar-plano.cod-sub-sist)
                       c-narrativa    = mmv-tar-plano.des-tarefa
                       rRowid         = rowid(mmv-tar-plano).
            end.
    end.
    /** Calend†rio (CLM) **/
    when 4 then do:
            if not(mmv-plano-prevent.log-gera-tar-om) or 
              (mmv-plano-prevent.log-gera-tar-om      and 
               not can-find(first bfTarefaPlano
                            where bfTarefaPlano.cod-model = mmv-plano-prevent.cod-model
                            and   bfTarefaPlano.cod-plano = mmv-plano-prevent.cod-plano no-lock)) then do:
                assign cOrigem        = "CLM":U
                       vCodigo        = mmv-plano-prevent.cod-plano
                       c-cod-evento   = mmv-plano-prevent.cod-evento
                       c-cod-sub-sist = mmv-plano-prevent.cod-sub-sist
                       i-cd-tipo      = mmv-plano-prevent.cd-tipo
                       i-num-docto    = 0
                       c-cod-plano    = mmv-plano-prevent.cod-plano
                       c-cod-model    = mmv-plano-prevent.cod-model
                       dt-dat-atualiz = mmv-eqpto-plano.dat-atualiz
                       vDescricao     = fnDescricao(mmv-plano-prevent.cod-evento,mmv-plano-prevent.cod-sub-sist)
                       c-narrativa    = vDescricao
                       rRowid         = rowid(mmv-eqpto-calend-plano).
            end.
            else do:
                assign iSeq-tar-plano  = mmv-tar-plano.num-seq.
                assign cOrigem        = "CLM":U
                       vCodigo        = mmv-plano-prevent.cod-plano + "-" + string(mmv-tar-plano.num-seq)
                       c-cod-evento   = mmv-tar-plano.cod-evento
                       c-cod-sub-sist = mmv-tar-plano.cod-sub-sist
                       i-cd-tipo      = mmv-plano-prevent.cd-tipo
                       i-num-docto    = 0
                       c-cod-plano    = mmv-plano-prevent.cod-plano
                       c-cod-model    = mmv-plano-prevent.cod-model
                       dt-dat-atualiz = mmv-eqpto-plano.dat-atualiz
                       vDescricao     = fnDescricao(mmv-tar-plano.cod-evento,mmv-tar-plano.cod-sub-sist)
                       c-narrativa    = mmv-tar-plano.des-tarefa
                       rRowid         = rowid(mmv-eqpto-calend-plano).
            end.
    end.
    /** Serviáos Pneus (SPN) **/
    when 5 then do:
        assign cOrigem        = "SPN":U
               vCodigo        = mpn-serv.cod-evento
               c-cod-evento   = mpn-serv.cod-evento
               c-cod-sub-sist = "":U
               i-cd-tipo      = mpn-serv.cd-tipo
               i-num-docto    = 0
               i-val-km-padr  = 0
               c-cod-plano    = "":U
               c-cod-model    = "":U
               dt-dat-atualiz = mpn-eqpto-serv.dat-trans
               vDescricao     = fnDescricao(mpn-serv.cod-evento,"")
               c-narrativa    = "":U
               rRowid         = rowid(mpn-eqpto-serv).
    end.
    /** Planos Componentes (PLC) **/
    when 7 then do:
        if not(mmv-plano-prevent.log-gera-tar-om) or 
          (mmv-plano-prevent.log-gera-tar-om      and 
           not can-find(first bfTarefaPlano
                        where bfTarefaPlano.cod-model = mmv-plano-prevent.cod-model
                        and   bfTarefaPlano.cod-plano = mmv-plano-prevent.cod-plano no-lock)) then do:
            assign cOrigem        = "PLC":U + string(dt-dat-vencto)
                   vCodigo        = mmv-plano-prevent.cod-plano
                   c-cod-evento   = mmv-plano-prevent.cod-evento
                   c-cod-sub-sist = mmv-plano-prevent.cod-sub-sist
                   i-cd-tipo      = mmv-plano-prevent.cd-tipo
                   i-num-docto    = 0
                   i-val-km-padr  = mmv-plano-prevent.val-km-padr
                   c-cod-plano    = mmv-plano-prevent.cod-plano
                   c-cod-model    = mmv-plano-prevent.cod-model
                   dt-dat-atualiz = mco-plano-compon.dat-atualiz
                   vDescricao     = fnDescricao(mmv-plano-prevent.cod-evento,mmv-plano-prevent.cod-sub-sist)
                   c-narrativa    = vDescricao
                   rRowid         = rowid(mmv-eqpto-plano).
        end.
        else do:
            assign iSeq-tar-plano  = mmv-tar-plano.num-seq.
            assign cOrigem        = "PLC":U + string(dt-dat-vencto)
                   vCodigo        = mmv-plano-prevent.cod-plano + "-" + string(mmv-tar-plano.num-seq)
                   c-cod-evento   = mmv-tar-plano.cod-evento
                   c-cod-sub-sist = mmv-tar-plano.cod-sub-sist
                   i-cd-tipo      = mmv-plano-prevent.cd-tipo
                   i-num-docto    = 0
                   i-val-km-padr  = mmv-plano-prevent.val-km-padr
                   c-cod-plano    = mmv-plano-prevent.cod-plano
                   c-cod-model    = mmv-plano-prevent.cod-model
                   dt-dat-atualiz = mco-plano-compon.dat-atualiz
                   vDescricao     = fnDescricao(mmv-tar-plano.cod-evento,mmv-tar-plano.cod-sub-sist)
                   c-narrativa    = mmv-tar-plano.des-tarefa
                   rRowid         = rowid(mmv-tar-plano).
        end.
    end.
    /** Componentes com uso real maior que estimado (CMP) **/
    when 8 then do:
        assign cOrigem        = "CMP":U
               vCodigo        = mco-compon.cod-compon + " - " + mco-compon.cod-sub-sist /* Xavier na UC */ 
               c-cod-evento   = mmv-param.cod-event-troca
               c-cod-sub-sist = mco-compon.cod-sub-sist
               i-cd-tipo      = 0
               i-num-docto    = 0
               i-val-km-padr  = 0
               c-cod-plano    = "":U
               c-cod-model    = "":U
               dt-dat-atualiz = mco-histor-vida.dat-inclusao
               vDescricao     = fnDescricao(mmv-param.cod-event-troca,mco-compon.cod-sub-sist)
               c-narrativa    = vDescricao
               rRowid         = rowid(mco-histor-vida).
    end.
     /** Calculo da Durabilidade do Sub-Sistema (SUB) **/
    when 9 or when 10 then do:
        assign cOrigem        = "SUB":U + mab-relac-eqpto-sub-sist.cod-sub-sist
               vCodigo        = mmv-param.cod-event-troca
               c-cod-evento   = mmv-param.cod-event-troca
               c-cod-sub-sist = mab-relac-eqpto-sub-sist.cod-sub-sist
               i-cd-tipo      = 0
               i-num-docto    = 0
               i-val-km-padr  = 0
               c-cod-plano    = "":U
               c-cod-model    = "":U
               dt-dat-atualiz = DtIni
               vDescricao     = fnDescricao(mmv-param.cod-event-troca,mab-relac-eqpto-sub-sist.cod-sub-sist)
               c-narrativa    = "":U 
               rRowid         = rowid(mab-relac-eqpto-sub-sist).
    end.
end case.

if not avail mab-grp-event then do:
    for first mab-grp-event 
        where mab-grp-event.cod-grp-event = mab-event.cod-grp-event no-lock:
    end.
end.
/** Busca registro **/
find first ttDados 
     where ttDados.cod-dimensao  = trim(cCodPai + "#" + cOrigem + vCodigo)
     and   ttDados.sequencia     = iVisao + 1 exclusive-lock no-error.
/** Se n∆o estiver criado, cria registro **/
if not avail ttDados then do:
   create ttDados.
   assign ttDados.cod-dimensao   = (cCodPai + "#" + cOrigem + vCodigo) /** C¢digo encadeado para tree-view **/
          ttDados.sequencia      = iVisao + 1                    /** Sequància dos dados **/
          ttDados.cod-oficial    = vCodigo                       /** C¢digo Original do registro **/
          ttDados.desc-dimensao  = vDescricao                    /** Descriá∆o do c¢digo **/
          ttDados.p-image        = iImage                        /** N£mero da imagem do registro **/
          ttDados.r-rowid        = rRowid                        /** Rowid do registro para consultas futuras **/
          ttDados.cod-dimens-pai = cCodPai                       /** C¢digo do pai do registro **/
          ttDados.lVencido       = lVencido
          ttDados.lVencer        = lVencer
          ttDados.un             = cUN
          ttDados.i-origem       = iOrigemTarefa
          ttDados.idi-tip-evento = mab-grp-event.idi-tip
          ttDados.ep-codigo      = mab-eqpto.ep-codigo
          ttDados.cod-eqpto      = mab-eqpto.cod-eqpto
          ttDados.cod-setor      = cCodSetor
          ttDados.cod-ofici      = cCodOficina
          ttDados.cod-evento     = c-cod-evento  
          ttDados.cod-sub-sist   = c-cod-sub-sist
          ttDados.cd-tipo        = i-cd-tipo     
          ttDados.cod-plano      = c-cod-plano   
          ttDados.cod-model      = c-cod-model   
          ttDados.dat-vencto     = dt-dat-vencto 
          ttDados.dat-atualiz    = dt-dat-atualiz
          ttDados.uso-real       = ttDados.uso-real   + deReal
          ttDados.uso-padrao     = ttDados.uso-padrao + dePadrao
          ttDados.diferenca      = ttDados.uso-real   - ttDados.uso-padrao
          ttDados.num-docto      = i-num-docto
          ttDados.narrativa      = c-narrativa
          ttDados.val-km-padr    = i-val-km-padr
          ttDados.l-marcado      = NO
          ttDados.lMostra        = if c-cod-plano <> "":U then no else yes
          ttDados.i-estado       = if iOrigemTarefa = 6
                                   then mmv-tar-ord-manut.estado
                                   else 1.
    assign ttDados.num-seq-plano = iSeq-tar-plano.
    
    assign cCodDimens = (cCodPai + "#" + cOrigem + vCodigo).

    if c-cod-plano <> "":U then do:
        run piMaiorPlano in this-procedure (input mab-eqpto.ep-codigo,
                                            input mab-eqpto.cod-eqpto,
                                            input i-val-km-padr,
                                            output l-mostra,
                                            output cCodDimensAnt).
        for first ttDados
            where ttDados.cod-dimensao = cCodDimens:            
            assign ttDados.lMostra = l-mostra.
        end.
        if cCodDimensAnt <> "":U then do:
            for first ttDados
                where ttDados.cod-dimensao = cCodDimensAnt:
                assign ttDados.lMostra = no.
            end.
        end.
    end.
end.

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
/** Limpa temp-table Excel **/
empty temp-table tt-dados-ex.

/************************************************************
** Chama tela para escolha da impress∆o
** 1 = Somente Dimens∆o Selecionada
** 2 = Total
************************************************************/
/*--- Seta cursor do mouse para espera ---*/
SESSION:SET-WAIT-STATE("GENERAL":U).
assign {&window-name}:sensitive = no.
run mvp/mv0613f.w (output i-impressao,
                   output l-ok).
/*--- Seta cursor do mouse para normal ---*/
SESSION:SET-WAIT-STATE("":U).
assign {&window-name}:sensitive = yes.

/** A tela foi confirmada, ent∆o continua **/
if l-ok then do:
    assign i-col     = 1
           i-lin     = 1
           i-lin2    = 1
           c-colunas = "".

    /** Encontra n£mero total de colunas **/
    do i-col = 1 TO brDetalhe:num-columns in frame fPage0:
        hHandle = brDetalhe:get-browse-column(i-col).
        /** Guarda t°tulos das colunas **/
        assign c-colunas = c-colunas + hHandle:name.
        if i-col <> brDetalhe:num-columns  then
            assign c-colunas = c-colunas + ",".
    end.

    /** Somente vis∆o selecionada **/
    if i-impressao = 1 then do:
        /** Guarda rowid do tree-view **/
        assign rRowid    = to-rowid(entry(1,chTreeView:SelectedItem:Tag)).
        /** Busca Registro **/
        for first bfttDados
            where rowid(bfttDados) = rRowid no-lock:
            run piExcelDados (input bfttDados.cod-dimensao).
        end.
    end.
    /** Completo **/
    else do:
        for each  bfttDados
            where bfttDados.sequencia = 1 no-lock:
              run piExcelDados (input bfttDados.cod-dimensao).
        end.
    end.
         
    /** Programa que gera o XLS **/
    run abp/ab9003.p(input "mv0613":U,
                     input table tt-dados-ex).
    if return-value = "NOK":U then .

    /** Posiciona o Tree-View no £ltimo lido **/
    if chTreeView:Nodes:Count > 0 then do:
        /** Seleciona item **/
        assign chTreeView:Nodes:Item(chTreeView:SelectedItem:key()):selected = yes no-error.
        /** Atualiza Browse **/
        if not error-status:error then
            run CtrlFrame.TreeView.NodeClick (input chTreeView:Nodes:Item(chTreeView:SelectedItem:key())).
    end.

end.
   
return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piExcelDados wWindow 
PROCEDURE piExcelDados :
/*------------------------------------------------------------------------------
  Purpose:     piExcelDados
  Parameters:  entrada pDimensao = Dimensao lida
  Notes:       Impress∆o dos dados em excel
------------------------------------------------------------------------------*/
define input parameter pDimensao as character no-undo.

define variable cPai as character no-undo.

assign cPai = "".

for each  ttDados use-index vencto
    where ttDados.cod-dimens-pai = pDimensao no-lock:
    /** Verifica se existem n°veis abaixo  **/
    if ttDados.sequencia <= iVisao then do:
        run piExcelDados (ttDados.cod-dimensao).
    end.
    else do:
        if cPai <> ttDados.cod-dimens-pai then do:
            /** Imprime os cabeáalhos **/
            run piExcelVisao (input rowid(ttDados),         
                              input ttDados.cod-dimens-pai).
            assign cPai  = ttDados.cod-dimens-pai
                   i-lin = i-lin2.
        end.

       /** In°cio contador de colunas do Browse**/
       do i-col = 2 to brDetalhe:num-columns in frame fPage0:
           /** Contador para linhas **/
           if i-lin = i-lin2 then do:
               create tt-dados-ex.
               assign tt-dados-ex.celula-coluna        = i-col - 1
                      tt-dados-ex.celula-linha         = i-lin
                      tt-dados-ex.celula-cor-interior  = 10
                      tt-dados-ex.celula-fonte-cor     = 19
                      tt-dados-ex.celula-fonte-negrito = yes.
               assign tt-dados-ex.celula-valor         = if i-col = 1 
                                                         then fnLabels(ttDados.p-image)
                                                         else fnBrowse(entry(i-col,c-colunas),1).
           end.
           /** Impress∆o da primeira linha de dados **/
           create tt-dados-ex.
           assign tt-dados-ex.celula-coluna = i-col - 1
                  tt-dados-ex.celula-linha  = i-lin + 1.
           assign tt-dados-ex.celula-valor  = fnBrowse(entry(i-col,c-colunas),2).
           /** Formata a cor das colunas conforme vencimento **/
           if entry(i-col,c-colunas) = "dat-vencto":U    or
              entry(i-col,c-colunas) = "desc-dimensao":U or 
              entry(i-col,c-colunas) = "diferenca":U     then do:
               if ttDados.i-estado = 4 then
                   assign tt-dados-ex.celula-fonte-cor = 15. /** Terminada = Cinza **/
               else do:
                   if ttDados.lVencido then
                       assign tt-dados-ex.celula-fonte-cor = 3.  /** Vencido = Vermelho **/
                   else 
                       if ttDados.lVencer then
                           assign tt-dados-ex.celula-fonte-cor = 5. /** ∑ Vencer = Azul **/
               end.
           end.
           /** Para c¢digo, coloca formato TEXTO **/
           if entry(i-col,c-colunas) = "cod-oficial":U then do:
               assign tt-dados-ex.celula-formato = "@":U.
           end.
       end. /** Colunas **/
       if ttDados.p-image = 30 then
           assign i-lin  = i-lin + 1
                  i-lin2 = i-lin + 1.
    end.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piExcelVisao wWindow 
PROCEDURE piExcelVisao :
/*------------------------------------------------------------------------------
  Purpose:     piExcelVisao
  Parameters:  rRow    = Rowid da temp-table
               cCodPai = C¢digo Pai do registro
  Notes:       Cria os dados no excel para os n°veis acima da vis∆o escolhida
------------------------------------------------------------------------------*/
define input parameter rRow    as rowid     no-undo.
define input parameter cCodPai as character no-undo.

/** Verifica se existe n°vel acima para chamar recursivamente a procedure **/
if can-find(first bfttDados2
            where bfttDados2.cod-dimensao = cCodPai no-lock) then do:
    for first bfttDados2
        where bfttDados2.cod-dimensao = cCodPai no-lock:
        run piExcelVisao (input rowid(bfttDados2),
                          input bfttDados2.cod-dimens-pai).
    end.
end.
/** Busca os dados a serem impressos **/
for first bfttDados2
    where rowid(bfttDados2) = rRow no-lock:
end.
if avail bfttDados2 and bfttDados2.p-image <> 30 then do:
    /** Cria os dados na temp-table do excel **/
   {mvp/esmv0613.i2 bfttDados2.p-image}
   create tt-dados-ex.
   assign tt-dados-ex.celula-coluna        = 1
          tt-dados-ex.celula-linha         = i-lin2
          tt-dados-ex.celula-cor-interior  = 27
          tt-dados-ex.celula-fonte-cor     = 1
          tt-dados-ex.celula-fonte-negrito = yes
          tt-dados-ex.celula-formato       = "@":U
          tt-dados-ex.celula-valor         = trim(return-value) + ":" +  bfttDados2.cod-oficial + "-" + bfttDados2.desc-dimensao
          i-lin2                           = i-lin2 + 1.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piFiltroCalendario wWindow 
PROCEDURE piFiltroCalendario :
/*------------------------------------------------------------------------------
  Purpose:     piFiltroCalendario
  Parameters:  entrada pData = Data do movimento
  Notes:       Valida se a data passada como parÉmetro Ç uma data v†lida ou n∆o
------------------------------------------------------------------------------*/
define input parameter pData as date format "99/99/9999"no-undo.

if ttSelecao.cod-calend <> "":U then do:
    /** Continua somente se data do calend†rio permitir manutená∆o 
        pela oficina (mmv-ofici) ou plano (mmv-plano-prevent) **/
    if not can-find(first calen-data
                    where calen-data.cd-calen = ttSelecao.cod-calend
                    and   calen-data.data     = pData
                    and   calen-data.dia-manut no-lock) then do:
        return "NOK":U.
    end.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piFiltroEvento wWindow 
PROCEDURE piFiltroEvento :
/*------------------------------------------------------------------------------
  Purpose:     piFiltroEvento
  Parameters:  entrada pEvento = C¢digo do evento
  Notes:       Filtra o evento e grupo de evento
------------------------------------------------------------------------------*/
define input parameter pEvento as character no-undo.

if not avail mab-event or mab-event.cod-evento <> pEvento then do:
    for first mab-event fields(des-evento cod-evento cod-grp-event)
        where mab-event.cod-evento = pEvento no-lock:
    end.
END.
    if avail mab-event then do:
        if not avail mab-grp-event or mab-grp-event.cod-grp-event <> mab-event.cod-grp-event then do:
            for first mab-grp-event 
                where mab-grp-event.cod-grp-event = mab-event.cod-grp-event 
                and ((mab-grp-event.idi-tip       = 1 and ttSelecao.lOleo)
                or   (mab-grp-event.idi-tip       = 2 and ttSelecao.lOleo) 
                or   (mab-grp-event.idi-tip       = 3 and ttSelecao.lMecanica)
                or   (mab-grp-event.idi-tip       = 4 and no)
                or   (mab-grp-event.idi-tip       = 5 and ttSelecao.lCusto)
                or   (mab-grp-event.idi-tip       = 6 and ttSelecao.lOutros)) no-lock:
            end.
        end.
    end.

/** Verifica se grupo evento foi encontrado **/
if not avail mab-grp-event then 
    return "NOK":U.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piImprimir wWindow 
PROCEDURE piImprimir :
/*------------------------------------------------------------------------------
  Purpose:     piImprimir
  Parameters:  <none>
  Notes:       Chama tela de impress∆o passando vis∆o a ser impressa
------------------------------------------------------------------------------*/
define variable rRowid     as rowid     no-undo.
define variable iSequencia as integer   no-undo.
define variable cDimensao  as character no-undo.

/** Guarda rowid do tree-view **/
assign rRowid = to-rowid(entry(1,chTreeView:SelectedItem:Tag)).
/** Busca Registro **/
for first ttDados
    where rowid(ttDados) = rRowid no-lock:
    assign iSequencia = ttDados.sequencia
           cDimensao  = ttDados.cod-dimensao.
end.
if avail ttDados then do:
    /** Verifica se registro contÇm filhos **/
    if not can-find(first bfttDados
                    where bfttDados.cod-dimens-pai = ttDados.cod-dimensao no-lock) then do:
        /** Se n∆o tiver, busca o pai do registro **/
        for first bfttDados
            where bfttDados.cod-dimensao = ttDados.cod-dimens-pai no-lock:
            assign iSequencia = bfttDados.sequencia
                   cDimensao  = bfttDados.cod-dimensao.
        end.
    end.
    assign {&window-name}:sensitive = no.
    run mvp/esmv0613d.w (input table ttDados,
                         input iSequencia,
                         input cDimensao,
                         input iVisao).
    assign {&window-name}:sensitive = yes.

    /** Posiciona o Tree-View no £ltimo lido **/
    if chTreeView:Nodes:Count > 0 then do:
        /** Seleciona item **/
        assign chTreeView:Nodes:Item(chTreeView:SelectedItem:key()):selected = yes no-error.
        /** Atualiza Browse **/
        if not error-status:error then
            run CtrlFrame.TreeView.NodeClick (input chTreeView:Nodes:Item(chTreeView:SelectedItem:key())).
    end.

end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piMaiorPlano wWindow 
PROCEDURE piMaiorPlano :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input  param pEmp       like mab-eqpto.ep-codigo           no-undo.
define input  param pEqpto     like mab-eqpto.cod-eqpto           no-undo.
define input  param pKM        like mmv-plano-prevent.val-km-padr no-undo.
define output param pMostra    as logical                         no-undo.
define output param pDimensAnt as character format "x(300)"       no-undo.

define buffer bf-ttDados for ttDados.

assign pDimensAnt = "":U.
for first bf-ttDados
    where bf-ttDados.ep-codigo  = pEmp
    and   bf-ttDados.cod-eqpto  = pEqpto
    and   bf-ttDados.cod-plano <> "":U
    and   bf-ttDados.lMostra exclusive-lock:
end.
if avail bf-ttDados then do:
    if bf-ttDados.val-km-padr < pKM then do:
        assign pMostra    = yes
               pDimensAnt = bf-ttDados.cod-dimensao.
    end.
    else
        assign pMostra = no.
end.
else
    assign pMostra = yes.

    return "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piOMGeracao wWindow 
PROCEDURE piOMGeracao :
/*------------------------------------------------------------------------------
  Purpose:     piOMGeracao
  Parameters:  <none>
  Notes:       Geraá∆o das tarefas para uma OM
------------------------------------------------------------------------------*/
define variable rRow        as rowid     no-undo.
define variable i-tarefa    as integer   no-undo.
define variable hProgram    as handle    no-undo.
define variable c-cod-ofici as character no-undo.
define variable i-ep-codigo as character no-undo.
define variable c-cod-eqpto as character no-undo.
define variable i-cd-tipo   as integer   no-undo.
define variable c-cod-setor as character no-undo.

define buffer bf-ttDadosAux  for ttDados.

if num-results("brDetalhe":U) > 0 then do:
    /** Guarda rowid do tree-view **/
    assign rRow = to-rowid(entry(1,chTreeView:SelectedItem:Tag)).
    /** Busca Registro Pai com rowid **/
    for first bfttDados 
        where rowid(bfttDados) = rRow no-lock:
    end.
    if not can-find(first ttDados
                    where ttDados.cod-dimens-pai = bfttDados.cod-dimensao
                    and   ttDados.l-marcado no-lock) then do:
        /** Nenhuma Tarefa foi selecionada **/
        empty temp-table RowErrors.
        {utp/ut-liter.i "Tarefa"}
        run piCriaErro in this-procedure (input 19177,
                                          input "EMS":U,
                                          input trim(return-value)).
        run mostraErros in this-procedure.
        return "NOK":U.
    end.
    else do:
        empty temp-table ttTarefa.
        empty temp-table tt-tarefa-plano.

        for FIRST  bf-ttDadosAux
            where bf-ttDadosAux.cod-dimens-pai = bfttDados.cod-dimensao 
            and   bf-ttDadosAux.cod-plano     <> "":U
            and   bf-ttDadosAux.l-marcado:
            for each  ttDados
                where ttDados.cod-dimens-pai = bf-ttDadosAux.cod-dimens-pai 
                and   ttDados.i-origem       = bf-ttDadosAux.i-origem
                and   ttDados.cod-plano     <> "":U :
                assign ttDados.l-marcado = yes.
            end.
        end.

        assign i-tarefa = 10.
        /** Percorre registros filhos que foram marcados **/
        for each  ttDados
            where ttDados.cod-dimens-pai = bfttDados.cod-dimensao 
            and   ttDados.l-marcado no-lock:

            /** N∆o cria tarefas duplicadas para manutená∆o de um mesmo sub-sistema com mesmo evento **/
            if can-find(first tt-tarefa-plano
                        where tt-tarefa-plano.cod-evento      = ttDados.cod-evento
                        and   tt-tarefa-plano.cod-sub-sist    = ttDados.cod-sub-sist
                        and   tt-tarefa-plano.cod-plano       = ttDados.cod-plano 
                        and   tt-tarefa-plano.num-seq         = ttDados.num-seq-plano  no-lock) then next.

            if ttDados.num-docto = 0 or 
               not can-find(first ttTarefa
                            where ttTarefa.num-docto-movto-event = ttDados.num-docto no-lock) then do:

                create tt-tarefa-plano.
                assign tt-tarefa-plano.cod-plano      = ttDados.cod-plano
                       tt-tarefa-plano.cod-sub-sist   = ttDados.cod-sub-sist
                       tt-tarefa-plano.cod-evento     = ttDados.cod-evento
                       tt-tarefa-plano.num-seq        = ttDados.num-seq-plano.

                create ttTarefa.
                assign ttTarefa.nr-ord-produ          = 0
                       ttTarefa.num-seq               = i-tarefa
                       i-tarefa                       = i-tarefa + 10
                       ttTarefa.ep-codigo             = ttDados.ep-codigo
                       ttTarefa.cod-eqpto             = ttDados.cod-eqpto
                       ttTarefa.cod-setor-ofici       = ttDados.cod-setor
                       ttTarefa.estado                = 1
                       ttTarefa.val-quant             = 1
                       ttTarefa.cod-plano             = ttDados.cod-plano
                       ttTarefa.cod-model-plano       = ttDados.cod-model
                       ttTarefa.cod-evento            = ttDados.cod-evento
                       ttTarefa.cod-sub-sist          = ttDados.cod-sub-sist
                       ttTarefa.cd-tipo               = ttDados.cd-tipo
                       ttTarefa.num-seqcial-retir     = 0
                       ttTarefa.num-docto-movto-event = ttDados.num-docto
                       ttTarefa.cod-compon-retir      = "":U
                       ttTarefa.cod-sit-retir         = "":U
                       ttTarefa.num-seqcial-reformad  = 0
                       ttTarefa.cod-compon-reformad   = "":U
                       ttTarefa.cod-sit-reformad      = "":U
                       ttTarefa.num-seqcial-colocada  = 0
                       ttTarefa.cod-compon-colocada   = "":U
                       ttTarefa.cod-sit-colocada      = "":U
                       ttTarefa.num-seqcial-plano     = 0
                       ttTarefa.cod-localiz-compon    = "":U
                       ttTarefa.dsl-obs               = ttDados.narrativa.
                /** Guarda a Origem da Tarefa, para que sejam manutenidas tabelas relacionadas necess†rias **/
                case ttDados.i-origem:
                    when 1 then assign ttTarefa.cod-orig-docto = "LUB":U. /** Lubrificaá‰es **/
                    when 2 then assign ttTarefa.cod-orig-docto = "EVT":U. /** Eventos **/
                    when 3 then assign ttTarefa.cod-orig-docto = "PLE":U. /** Planos de Equipamento **/
                    when 4 then assign ttTarefa.cod-orig-docto = "CLM":U. /** Calend†rio Manutená∆o **/
                    when 7 then assign ttTarefa.cod-orig-docto = "PLC":U. /** Planos de Componente **/
                    when 8 then assign ttTarefa.cod-orig-docto = "CMP":U. /** Componente vencido **/
                    WHEN 9 OR WHEN 10 THEN ASSIGN ttTarefa.cod-orig-docto = "SUB":U. /** Durabilidade de Sub-Sistemas **/
                end case.
                /** Guarda os c¢digos para inicializar a inclus∆o da OM **/
                if c-cod-ofici = "":U then assign c-cod-ofici = ttDados.cod-ofici.
                if i-ep-codigo = ""   then assign i-ep-codigo = ttDados.ep-codigo
                                                  c-cod-eqpto = ttDados.cod-eqpto
                                                  i-empresa   = ttDados.ep-codigo
                                                  c-eqpto     = ttDados.cod-eqpto.
                if i-cd-tipo   = 0    then assign i-cd-tipo   = ttDados.cd-tipo.
                if c-cod-setor = "":U then assign c-cod-setor = ttDados.cod-setor.
            end.
        end.
        /** Desabilita Timer **/
        assign chTimer:Interval = 0.
        
        /*--- Seta cursor do mouse para espera ---*/
        SESSION:SET-WAIT-STATE("GENERAL":U).
        assign {&window-name}:sensitive = no.
        /*--- Executa programa de inclus∆o de filho ---*/
        RUN mvp/mv0301.w PERSISTENT SET hProgram.
        
        /* novo mÇtodo de teste do valid-handle */
        IF VALID-HANDLE(hProgram) AND
           hProgram:Type = "PROCEDURE":U AND
           hProgram:FILE-NAME = "mvp/mv0301.w":U THEN
            /*--- Inicializa programa de inclus∆o de filho ---*/
            RUN initializeInterface IN hProgram.

        IF VALID-HANDLE(hProgram) THEN DO:

            run iniciaInclusao in hProgram (input c-cod-ofici,
                                            input i-ep-codigo,
                                            input c-cod-eqpto,
                                            input i-cd-tipo,
                                            input c-cod-setor,
                                            input table ttTarefa).
            
            /*--- Seta cursor do mouse para normal ---*/
            SESSION:SET-WAIT-STATE("":U).
            wait-for "CLOSE":U of hProgram.
        END.
        assign {&window-name}:sensitive = yes.
                
        if valid-handle(hProgram) then
            delete procedure hProgram.

        /** Atualiza dados **/
        session:set-wait-state ("GENERAL").
        run piAtualizar in this-procedure (input 2).
        session:set-wait-state ("").
    end.
end.  



return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piOMValidacao wWindow 
PROCEDURE piOMValidacao :
/*------------------------------------------------------------------------------
  Purpose:     piOMValidacao
  Parameters:  entrada pOrig = Origem (1 = Marca/Desmarca  /  2 = Todos)
  Notes:       Valida se registro pode ser marcado para gerar tarefas para uma OM
------------------------------------------------------------------------------*/
define input parameter pOrig as integer no-undo.

define variable cTexto as character no-undo.

if avail ttDados then do:
    case ttDados.i-origem:
        /** Serviáos Penus **/
        when 5 then do:
            /** Tarefas de pneus s∆o verificadas pelo m¢dulo de Pneus **/
            run piCriaErro in this-procedure (input 29677,
                                              input "EMS":U,
                                              input "":U).
        end.
        /** Ordens de Manutená∆o **/
        when 6 then do:
            /** Tarefa da OM j† cadastrada **/
            {utp/ut-liter.i "Tarefa"}
            assign cTexto = trim(return-value).
            {utp/ut-liter.i "Ordem Manutená∆o"}
            run piCriaErro in this-procedure (input 25237,
                                              input "EMS":U,
                                              input cTexto + '~~' + trim(return-value) + ' ' + string(ttDados.num-docto)).
        end.
    end case.
    /** Eventos de custos n∆o geram Tarefas **/
    if ttDados.idi-tip-evento = 5 then do:
        run piCriaErro in this-procedure (input 29675,
                                          input "EMS":U,
                                          input ttDados.cod-evento).
    end.
end.
/** Mostra erros ocorridos **/
if can-find(first RowErrors no-lock) then do:
    /** Mostra Erros somente no individual **/
    if pOrig = 1 then
        run mostraErros in this-procedure.
    else 
        empty temp-table RowErrors.
    return "NOK":U.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piTexto wWindow 
PROCEDURE piTexto :
/*------------------------------------------------------------------------------
  Purpose:     piTexto
  Parameters:  <none>
  Notes:       Exporta os dados da consulta para um arquivo texto
------------------------------------------------------------------------------*/
define variable c-key-value  as character no-undo.

/** Seta caminho e delimitador **/
ASSIGN cDelimitador = ";":U
       cCaminho     = session:temp-directory + c-programa-mg97 + ".txt":U.

/*--- Seta cursor do mouse para espera ---*/
SESSION:SET-WAIT-STATE("GENERAL":U).
assign {&window-name}:sensitive = no.
/***Chamada do programa de parÉmetros para geraá∆o do arquivo em formato TXT***/
run mvp/mv0613g.w (input-output cCaminho,
                   input-output lCabecalho,
                   input-output cDelimitador,
                   output       i-impressao,
                   output       l-ok).
/*--- Seta cursor do mouse para normal ---*/
SESSION:SET-WAIT-STATE("":U).
assign {&window-name}:sensitive = yes.

/** Se confirmar **/
if l-ok then do:
    /** Troca barras contr†rias pela correta **/
    assign cCaminho = replace(cCaminho, "/":U, "~\":U).
    /** Cria arquivo **/
    output to value(cCaminho) paged page-size 64 convert target "iso8859-1":U.
        /******************************************************************
        ** Inicia impress∆o do TXT
        ******************************************************************/
        /** Somente vis∆o selecionada **/
        if i-impressao = 1 then do:
            /** Guarda rowid do tree-view **/
            assign rRowid = to-rowid(entry(1,chTreeView:SelectedItem:Tag)).
            /** Busca Registro **/
            for first bfttDados
                where rowid(bfttDados) = rRowid no-lock:
                run piTextoDados (input bfttDados.cod-dimensao).
            end.
        end.
        /** Completo **/
        else do:
            for each  bfttDados
                where bfttDados.sequencia = 1 no-lock:
                  run piTextoDados (input bfttDados.cod-dimensao).
            end.
        end.
        /******************************************************************
        ** Finaliza impress∆o do TXT
        ******************************************************************/
    output close.
    /** Abertura do Arquivo **/
    get-key-value section "Datasul_EMS2":U key "Show-Report-Program":U value c-key-value.
    if  c-key-value = "" or  c-key-value = ?  then do:
        assign c-key-value = 'Notepad.exe':U.
        put-key-value section "Datasul_EMS2":U key "Show-Report-Program":U value c-key-value no-error.
    end.

    run WinExec (input c-key-value + chr(32) + cCaminho, input 1).


    /** Posiciona o Tree-View no £ltimo lido **/
    if chTreeView:Nodes:Count > 0 then do:
        /** Seleciona item **/
        assign chTreeView:Nodes:Item(chTreeView:SelectedItem:key()):selected = yes no-error.
        /** Atualiza Browse **/
        if not error-status:error then
            run CtrlFrame.TreeView.NodeClick (input chTreeView:Nodes:Item(chTreeView:SelectedItem:key())).
    end.

end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piTextoDados wWindow 
PROCEDURE piTextoDados :
/*------------------------------------------------------------------------------
  Purpose:     piTextoDados
  Parameters:  entrada pDimensao = Dimensao lida
  Notes:       Impress∆o dos dados em Texto
------------------------------------------------------------------------------*/
define input parameter pDimensao as character no-undo.

define variable cPai          as character                no-undo.
define variable c-dat-vencto  as character format "x(10)" no-undo.
define variable c-dat-atualiz as character format "x(10)" no-undo.
define variable c-tam         as character                no-undo.

assign cPai = "".

for each  ttDados use-index vencto
    where ttDados.cod-dimens-pai = pDimensao no-lock:
    /** Verifica se existem n°veis abaixo  **/
    if ttDados.sequencia <= iVisao then do:
        run piTextoDados (ttDados.cod-dimensao).
    end.
    else do:
        if cPai <> ttDados.cod-dimens-pai then do:
            assign cDescPai = "":U.
            /** Imprime os cabeáalhos **/
            run piTextoVisao (input rowid(ttDados),         
                              input ttDados.cod-dimens-pai).
            assign cPai       = ttDados.cod-dimens-pai
                   cTituloTxt = "":U.
        end.
        assign c-dat-vencto  = if ttDados.dat-vencto = ?  then "":U else string(ttDados.dat-vencto,"99/99/9999")
               c-dat-atualiz = if ttDados.dat-atualiz = ? then "":U else string(ttDados.dat-atualiz,"99/99/9999").

        assign c-tam = "x(" + string(length(trim(trim(cDescPai) +
                                     trim(ttDados.cod-oficial) + "-" + trim(ttDados.desc-dimensao) + cDelimitador +
                                     string(ttDados.uso-real,"->,>>>,>>9.9999")                    + cDelimitador +
                                     string(ttDados.uso-padrao,"->,>>>,>>9.99")                    + cDelimitador +
                                     string(ttDados.diferenca,"->,>>>,>>9.99")                     + cDelimitador +
                                     ttDados.un                                                    + cDelimitador +
                                     c-dat-vencto                                                  + cDelimitador +
                                     c-dat-atualiz                                                 + cDelimitador +
                                     fnOrigem(ttDados.i-origem)                                    + cDelimitador +
                                     string(ttDados.num-docto,">>>,>>>,>>9")                       + cDelimitador +
                                     ttDados.cod-plano                                             + cDelimitador +
                                     fnTipoManut(ttDados.cd-tipo)))) + ")".

        /** Exporta dados para TXT **/
        put trim(trim(cDescPai) +
                 trim(ttDados.cod-oficial) + "-" + trim(ttDados.desc-dimensao) + cDelimitador +
                 string(ttDados.uso-real,"->,>>>,>>9.9999")                    + cDelimitador +
                 string(ttDados.uso-padrao,"->,>>>,>>9.99")                    + cDelimitador +
                 string(ttDados.diferenca,"->,>>>,>>9.99")                     + cDelimitador +
                 ttDados.un                                                    + cDelimitador +
                 c-dat-vencto                                                  + cDelimitador +
                 c-dat-atualiz                                                 + cDelimitador +
                 fnOrigem(ttDados.i-origem)                                    + cDelimitador +
                 string(ttDados.num-docto,">>>,>>>,>>9")                       + cDelimitador +
                 ttDados.cod-plano                                             + cDelimitador +
                 fnTipoManut(ttDados.cd-tipo)) format c-tam at 01.
    end.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piTextoVisao wWindow 
PROCEDURE piTextoVisao :
/*------------------------------------------------------------------------------
  Purpose:     piTextoVisao
  Parameters:  rRow    = Rowid da temp-table
               cCodPai = C¢digo Pai do registro
  Notes:       Cria os dados no excel para os n°veis acima da vis∆o escolhida
------------------------------------------------------------------------------*/
define input parameter rRow     as rowid     no-undo.
define input parameter cCodPai  as character no-undo.

define variable c-tam as character no-undo.
  
/** Verifica se existe n°vel acima para chamar recursivamente a procedure **/
if can-find(first bfttDados2
            where bfttDados2.cod-dimensao = cCodPai no-lock) then do:
    for first bfttDados2
        where bfttDados2.cod-dimensao = cCodPai no-lock:
        run piTextoVisao (input rowid(bfttDados2),
                          input bfttDados2.cod-dimens-pai).
    end.
end.
/** Busca os dados a serem impressos **/
for first bfttDados2
    where rowid(bfttDados2) = rRow no-lock:
    {mvp/esmv0613.i2 bfttDados2.p-image}
    assign cTituloTxt = cTituloTxt + TRIM(RETURN-VALUE) + cDelimitador.
end.
if avail bfttDados2 and bfttDados2.p-image <> 30 then do:
    assign cDescPai  = trim(trim(cDescPai) + trim(bfttDados2.cod-oficial) + "-" + TRIM(bfttDados2.desc-dimensao) + cDelimitador).
    /** Se for o pen£ltimo n°vel, imprime cabeáalho se parametrizado **/
    if bfttDados2.sequencia = iVisao and lCabecalho then do:
        ASSIGN lCabecalho = NO.
        assign c-tam = "x(" + string(length(trim(trim(cTituloTxt)                   +
                                     brDetalhe:TITLE IN FRAME fPage0 + cDelimitador +
                                     fnBrowse("uso-real":U,1)        + cDelimitador +
                                     fnBrowse("uso-padrao":U,1)      + cDelimitador +
                                     fnBrowse("diferenca":U,1)       + cDelimitador +
                                     fnBrowse("un":U,1)              + cDelimitador +
                                     fnBrowse("dat-vencto":U,1)      + cDelimitador +
                                     fnBrowse("dat-atualiz":U,1)     + cDelimitador +
                                     fnBrowse("c-origem-dad":U,1)    + cDelimitador +
                                     fnBrowse("num-docto":U,1)       + cDelimitador +
                                     fnBrowse("cod-plano":U,1)       + cDelimitador +
                                     fnBrowse("c-tip-manut":U,1)))) + ")".
        put trim(trim(cTituloTxt)                               +
                 brDetalhe:TITLE IN FRAME fPage0 + cDelimitador +
                 fnBrowse("uso-real":U,1)        + cDelimitador +
                 fnBrowse("uso-padrao":U,1)      + cDelimitador +
                 fnBrowse("diferenca":U,1)       + cDelimitador +
                 fnBrowse("un":U,1)              + cDelimitador +
                 fnBrowse("dat-vencto":U,1)      + cDelimitador +
                 fnBrowse("dat-atualiz":U,1)     + cDelimitador +
                 fnBrowse("c-origem-dad":U,1)    + cDelimitador +
                 fnBrowse("num-docto":U,1)       + cDelimitador +
                 fnBrowse("cod-plano":U,1)       + cDelimitador +
                 fnBrowse("c-tip-manut":U,1)) format c-tam at 01.
    end.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piVerificaGarantia wWindow 
PROCEDURE piVerificaGarantia :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER deKMAcumulado   AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER iUsoDias        AS INTEGER NO-UNDO.
    

IF (deKMAcumulado >= mab-relac-eqpto-sub-sist.val-km-hora-gartia) 
AND mab-relac-eqpto-sub-sist.val-km-hora-gartia > 0 THEN DO:
    ASSIGN lGarantia = YES
           iOrig     = 2.
END.
/**Verifica se registro esta vencido em Dias **/
ELSE DO:
    /* IF (iUsoDias >= mab-relac-eqpto-sub-sist.num-dias-estimad) THEN DO: */ /*Xavier na UC */
    IF (iUsodias >= mab-relac-eqpto-sub-sist.num-dias-gartia)       /*Xavier na UC*/
       AND mab-relac-eqpto-sub-sist.num-dias-gartia > 0 THEN DO:    /*Xavier na UC*/
       ASSIGN lGarantia = YES
              iOrig    = 3.
    END.
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
            {utp/ut-liter.i "Tarefa"}
        end.
        when "desc-dimensao":U then do:
            {utp/ut-liter.i "Descriá∆o"}
        end.
        when "uso-real":U then do:
            {utp/ut-liter.i "Uso Real"}
        end.
        when "uso-padrao":U then do:
            {utp/ut-liter.i "Uso Padr∆o"}
        end.
        when "diferenca":U then do:
            {utp/ut-liter.i "Diferenáa"}
        end.
        when "un":U then do:
            {utp/ut-liter.i "UN"}
        end.
        when "num-docto":U then do:
            {utp/ut-liter.i "Documento"}
        end.
        when "dat-vencto":U then do:
            {utp/ut-liter.i "Vencimento"}
        end.
        when "dat-atualiz":U then do:
            {utp/ut-liter.i "Atualizaá∆o"}
        end.
        when "c-origem-dad":U then do:
            {utp/ut-liter.i "Origem"}
        end.
        when "c-tip-manut":U then do:
            {utp/ut-liter.i "Tipo Manutená∆o"}
        end.
        when "cod-plano":U then do:
            {utp/ut-liter.i "Plano Prevená∆o"}
        end.
    end case.
    assign cRetorno = trim(return-value).
end.
else do:
    case pCampo:
        when "cod-oficial":U then do:
            assign cRetorno = ttDados.cod-oficial.
        end.
        when "desc-dimensao":U then do:
            assign cRetorno = ttDados.desc-dimensao.
        end.
        when "uso-real":U then do:
            assign cRetorno = string(ttDados.uso-real,"->,>>>,>>9.9999").
        end.
        when "uso-padrao":U then do:
            assign cRetorno = string(ttDados.uso-padrao,"->,>>>,>>9.9999").
        end.
        when "diferenca":U then do:
            assign cRetorno = string(ttDados.diferenca,"->,>>>,>>9.9999").
        end.
        when "un":U then do:
            assign cRetorno = ttDados.un.
        end.
        when "num-docto":U then do:
            assign cRetorno = string(ttDados.num-docto,">>>,>>>,>>9").
        end.
        when "dat-vencto":U then do:
            assign cRetorno = string(ttDados.dat-vencto,"99/99/9999").
        end.
        when "dat-atualiz":U then do:
            assign cRetorno = string(ttDados.dat-atualiz,"99/99/9999").
        end.
        when "c-origem-dad":U then do:
            assign cRetorno = fnOrigem(ttDados.i-origem).
        end.
        when "c-tip-manut":U then do:
            assign cRetorno = fnTipoManut(ttDados.cd-tipo).
        end.
        when "cod-plano":U then do:
            assign cRetorno = ttDados.cod-plano.
        end.
    end case.
end.

  RETURN cRetorno.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnDescricao wWindow 
FUNCTION fnDescricao RETURNS CHARACTER
  ( pEvento as character,
    pSub    as character ) :
/*------------------------------------------------------------------------------
  Purpose:  fnDescricao
    Notes:  Cria a descriá∆o da tarefa
------------------------------------------------------------------------------*/
DEFINE VARIABLE cRetorno AS CHARACTER  NO-UNDO.

if not avail mab-event or mab-event.cod-evento <> pEvento then do:
    for first mab-event fields(des-evento cod-evento cod-grp-event)
        where mab-event.cod-evento = pEvento no-lock:
    end.
end.
if avail mab-event then
    assign cRetorno = mab-event.des-evento.
if not avail mab-sub-sist or mab-sub-sist.cod-sub-sist <> pSub then do:
    for first mab-sub-sist fields(des-sub-sist cod-sub-sist cod-sist)
        where mab-sub-sist.cod-sub-sist = pSub no-lock:
    end.
end.
if avail mab-sub-sist then
    assign cRetorno = cRetorno + " - " + mab-sub-sist.des-sub-sist.

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
    {mvp/esmv0613.i2 pImage}

    assign cRetorno = trim(return-value).

  RETURN cRetorno.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnLegenda wWindow 
FUNCTION fnLegenda RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

def var c-retorno as char        no-undo.

if avail ttDados then do:
          {utp/ut-liter.i N∆o_Atrasadas * }
      /** Vencido = Vermelho **/
      if ttDados.lVencido then do:
          {utp/ut-liter.i Vencidas * }
      end.
      else do:
          /** WBD: BEGIN_NULL_CODE **/
          /** A Vencer = Azul **/
          if ttDados.lVencer then do:
              {utp/ut-liter.i ∑_Vencer * }
          end.
          /** WBD: END_NULL_CODE **/
      end.
      /** Tarefas de OM **/
      if ttDados.i-estado = 4 then do:
          {utp/ut-liter.i Terminadas * }
      end.
      /** Garantia de Sub-Sistema **/
      IF ttDados.i-origem = 10 THEN DO:
          {utp/ut-liter.i Garantia * }
      END.

      assign c-retorno = return-value.
end.

  RETURN c-retorno.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnMarcado wWindow 
FUNCTION fnMarcado RETURNS CHARACTER
  ( p-marcado as logical ) :
/*------------------------------------------------------------------------------
  Purpose:  fnMarcado
    Notes:  Retorna se registro est† marcado
------------------------------------------------------------------------------*/

DEFINE VARIABLE cRetorno AS CHARACTER format "x" NO-UNDO.

if p-marcado then do:
    assign cRetorno = "*":U.
end.

  RETURN cRetorno.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnMes wWindow 
FUNCTION fnMes RETURNS CHARACTER
  ( iMes as int ) :
/*------------------------------------------------------------------------------
  Purpose:  fnMes
    Notes:  Busca o Màs
------------------------------------------------------------------------------*/

DEFINE VARIABLE cRetorno AS CHARACTER  NO-UNDO.

assign cRetorno = "".

case iMes:
    when 1 then do:
        {utp/ut-liter.i Janeiro}
    end.
    when 2 then do:
        {utp/ut-liter.i Fevereiro}
    end.
    when 3 then do:
        {utp/ut-liter.i Maráo}
    end.
    when 4 then do:
        {utp/ut-liter.i Abril}
    end.
    when 5 then do:
        {utp/ut-liter.i Maio}
    end.
    when 6 then do:
        {utp/ut-liter.i Junho}
    end.
    when 7 then do:
        {utp/ut-liter.i Julho}
    end.
    when 8 then do:
        {utp/ut-liter.i Agosto}
    end.
    when 9 then do:
        {utp/ut-liter.i Setembro}
    end.
    when 10 then do:
        {utp/ut-liter.i Outubro}
    end.
    when 11 then do:
        {utp/ut-liter.i Novembro}
    end.
    when 12 then do:
        {utp/ut-liter.i Dezembro}
    end.
end case.

assign cRetorno = trim(return-value).

  RETURN cRetorno.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnOrigem wWindow 
FUNCTION fnOrigem RETURNS CHARACTER
  ( i-orig as integer ) :
/*------------------------------------------------------------------------------
  Purpose:  fnOrigem
    Notes:  Descriá∆o da Origem da Tarefa
------------------------------------------------------------------------------*/
DEFINE VARIABLE cRetorno AS CHARACTER  NO-UNDO.

case i-orig:
    when 1 then do:
        {utp/ut-liter.i "Lubrificaá‰es"}
    end.
    when 2 then do:
        {utp/ut-liter.i "Eventos"}
    end.
    when 3 then do:
        {utp/ut-liter.i "Planos Manutená∆o"}
    end.
    when 4 then do:
        {utp/ut-liter.i "Calend†rio Manutená∆o"}
    end.
    when 5 then do:
        {utp/ut-liter.i "Serviáo Pneus"}
    end.
    when 6 then do:
        {utp/ut-liter.i "Ordens Manutená∆o"}
    end.
    when 7 then do:
        {utp/ut-liter.i "Planos Componentes"}
    end.
    when 8 then do:
        {utp/ut-liter.i "Componente Vencido"}
    end.
    when 9 then do:
        {utp/ut-liter.i "Durab Sub-Sistemas"}
    end.
    WHEN 10 THEN DO:
        {utp/ut-liter.i "Garantia Sub-Sistema"}
    END.
end case.

assign cRetorno = trim(return-value).


  RETURN cRetorno.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnTipoManut wWindow 
FUNCTION fnTipoManut RETURNS CHARACTER
  ( pTipo as integer ) :
/*------------------------------------------------------------------------------
  Purpose:  fnTipoManut
    Notes:  Descriá∆o do Tipo de Manutená∆o
------------------------------------------------------------------------------*/
DEFINE VARIABLE cRetorno AS CHARACTER  NO-UNDO.

if not avail tipo-manut or tipo-manut.cd-tipo <> pTipo then do:
    for first tipo-manut fields(descricao cd-tipo)
        where tipo-manut.cd-tipo = pTipo no-lock:
    end.
end.
if avail tipo-manut then
    assign cRetorno = tipo-manut.descricao.

  RETURN cRetorno.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          mgfro            PROGRESS
*/
&Scoped-define WINDOW-NAME wWindow

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-mab-eqpto NO-UNDO LIKE mab-eqpto
       field r-rowid as rowid.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWindow 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i COL0602 2.03.00.005}  /*** 010003 ***/
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
&GLOBAL-DEFINE Program        COL0602
&GLOBAL-DEFINE Version        2.03.00.005

&GLOBAL-DEFINE WindowType     Master/Detail

&GLOBAL-DEFINE Folder         no

&GLOBAL-DEFINE page0Widgets   btQueryJoins btReportsJoins btExit btHelp ~
                              btAbrir btSalvar btParam btVisao btExpande ~
                              btImprimir btRedimensiona brDetalhe ~
                              btConsulta btMV0613 btEqpto btPneu btHistorico ~
                              btCO0604 btFicha btExcel btTXT ~
                              btLegenda bt1 btModifica btEncerra btReporte btPlano btEvento btFirst btPrev btNext btLast btGoto btSearch btRefresh

/* Local Variable Definitions ---                                       */
define variable c-arquivo     as character                no-undo.
define variable cUN           as character                no-undo.
/** Busca Valor **/
define variable rRowid        as rowid                    no-undo.
define variable cCodPai       as character format "x(20)" no-undo.
define variable vCodigo       as character format "x(20)" no-undo.
define variable vDescricao    as character format "x(32)" no-undo.
define variable iImage        as integer                  no-undo.
/** OCX **/
define variable chTreeView    as com-handle               no-undo.
define variable chImageList   as com-handle               no-undo.
/** Controles **/
define variable l-expande     as logical init no          no-undo.
define variable l-ok          as logical                  no-undo.
define variable c-tag-tree    as character                no-undo.
define variable i-cont        as integer                  no-undo.
define variable iVisao        as integer                  no-undo.
define variable iVisaoEqpto   as integer                  no-undo.
define variable i-button-tree as integer  initial 1       no-undo.
define variable i-empresa     as integer                  no-undo.
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
define variable iOrig          as integer                  no-undo.
define variable cCodSubSist    as character                no-undo.
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
define variable c-hora             as character format "99:99:99"   no-undo.
define variable iSeq-tar-plano      as int                           no-undo.

/* Vari†veis de controle */
DEFINE VARIABLE lVencidoC AS LOGICAL    NO-UNDO.
define variable c-tipo    as character  no-undo.
define variable d-uso-real as decimal format ">>,>>>,>>9.9" no-undo.
DEFINE VARIABLE d-garantia AS DECIMAL    NO-UNDO.
DEFINE VARIABLE d-atualiza AS DATE       NO-UNDO.
DEFINE VARIABLE i-ordem AS INTEGER    NO-UNDO.
DEFINE VARIABLE c-ung AS CHARACTER format "x(3)" NO-UNDO.
DEFINE VARIABLE c-cod-visao AS character format "x(20)"  NO-UNDO.
DEFINE VARIABLE c-des-visao AS CHARACTER format "x(30)" NO-UNDO.
define variable deKMEqpto  as decimal   no-undo.
DEFINE VARIABLE c-estado AS CHARACTER format "x(20)" NO-UNDO.
DEFINE VARIABLE hAcomp AS HANDLE     NO-UNDO.
DEFINE VARIABLE d-uso-padrao LIKE mab-period-manut.val-km-padr NO-UNDO.

/** Classificaá∆o **/
def temp-table ttVisao no-undo
    field dimensao  as character
    field sequencia as integer
    index codigo is primary unique sequencia.

{colp/col0602.i3 ttDados} /** Definiá∆o da ttDados **/

define buffer bfttDados  for ttDados.
define buffer bfttDados2 for ttDados.
DEFINE BUFFER bfOrdem    FOR mmv-ord-manut.  /*criado por xavier na UC*/

{colp/col0602.i}  /** Definiá∆o da temp-table de parÉmetros **/

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

define variable hDBOEqpto as handle no-undo.

define variable c-eqpto-excel as character format "x(20)"             no-undo.
define variable de-cont-excel like mab-movto-km-eqpto.val-hodom-horim no-undo.
define variable c-km-eqpto    as character no-undo.
define variable l-primeiro as logical no-undo.
DEFINE VARIABLE i-cod-image AS INTEGER    NO-UNDO.

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
&Scoped-define FIELDS-IN-QUERY-brDetalhe ttDados.cod-oficial ttDados.desc-dimensao ttDados.uso-real ttDados.val-uso-padr ttDados.un ttDados.garantia ttDados.ung ttDados.atualizacao ttDados.ordem ttDados.tipo fnEstado(ttDados.i-estado) @ c-estado   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brDetalhe   
&Scoped-define SELF-NAME brDetalhe
&Scoped-define OPEN-QUERY-brDetalhe open query {&SELF-NAME} for each ttDados no-lock.
&Scoped-define TABLES-IN-QUERY-brDetalhe ttDados
&Scoped-define FIRST-TABLE-IN-QUERY-brDetalhe ttDados


/* Definitions for FRAME fpage0                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fpage0 ~
    ~{&OPEN-QUERY-brDetalhe}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tt-mab-eqpto.ep-codigo tt-mab-eqpto.cod-eqpto 
&Scoped-define ENABLED-TABLES tt-mab-eqpto
&Scoped-define FIRST-ENABLED-TABLE tt-mab-eqpto
&Scoped-define DISPLAYED-TABLES tt-mab-eqpto
&Scoped-define FIRST-DISPLAYED-TABLE tt-mab-eqpto
&Scoped-Define ENABLED-OBJECTS btFirst btPrev btNext btLast btGoTo btSearch ~
btRefresh btAbrir btSalvar btExpande btParam btVisao btExcel btTxt ~
btImprimir btQueryJoins btReportsJoins btExit btHelp fi-nom-eqpto ~
fi-contador fi-km-eqpto btRedimensiona brDetalhe btLegenda btPneu btMV0613 ~
btModifica btEncerra btConsulta bt1 btFicha btReporte btPlano btHistorico ~
btCO0604 btEqpto btEvento rtToolBar-2 rtToolBar-3 
&Scoped-Define DISPLAYED-FIELDS tt-mab-eqpto.ep-codigo ~
tt-mab-eqpto.cod-eqpto 
&Scoped-Define DISPLAYED-OBJECTS fi-nom-eqpto fi-contador fi-km-eqpto 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnEstado wWindow 
FUNCTION fnEstado RETURNS CHARACTER
  ( iEstado as integer )  FORWARD.

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
       MENU-ITEM miAtualizar    LABEL "At&ualizar"     ACCELERATOR "CTRL-R"
       MENU-ITEM miAbrir        LABEL "&Abrir"         ACCELERATOR "CTRL-A"
       MENU-ITEM miSalvar       LABEL "&Salvar"        ACCELERATOR "CTRL-S"
       MENU-ITEM miExpande      LABEL "&Expande/Contrai" ACCELERATOR "CTRL-E"
       MENU-ITEM m_V_Para       LABEL "V† Para"        ACCELERATOR "CTRL-T"
       RULE
       MENU-ITEM miSelecao      LABEL "Se&leá∆o"       ACCELERATOR "CTRL-P"
       MENU-ITEM miFiltro       LABEL "&Filtro"        ACCELERATOR "CTRL-F"
       MENU-ITEM miClassifica   LABEL "&Classifica"    ACCELERATOR "CTRL-C"
       RULE
       MENU-ITEM miExcel        LABEL "E&xcel"         ACCELERATOR "CTRL-N"
       MENU-ITEM miTxt          LABEL "&Texto"        
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

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt1 
     IMAGE-UP FILE "image/notas-h.bmp":U
     LABEL "" 
     SIZE 4 BY 1.25 TOOLTIP "Narrativas da Ordem".

DEFINE BUTTON btAbrir 
     IMAGE-UP FILE "image/im-open":U
     IMAGE-INSENSITIVE FILE "image/ii-open":U
     LABEL "" 
     SIZE 4 BY 1.25 TOOLTIP "Abrir"
     FONT 4.

DEFINE BUTTON btCO0604 
     IMAGE-UP FILE "image/im-turno.bmp":U
     LABEL "Componentes do Equipamento" 
     SIZE 4 BY 1.25 TOOLTIP "Consulta Componentes Equipamento".

DEFINE BUTTON btConsulta 
     IMAGE-UP FILE "image/mab-docto.bmp":U
     LABEL "&Consultar" 
     SIZE 4 BY 1.25 TOOLTIP "Consulta da Ordem de Manutená∆o".

DEFINE BUTTON btEncerra 
     IMAGE-UP FILE "image/im-check.bmp":U
     LABEL "&Encerrar" 
     SIZE 4 BY 1.25 TOOLTIP "Encerramento da Ordem de Manutená∆o".

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

DEFINE BUTTON btFicha 
     IMAGE-UP FILE "image/im-prigr.bmp":U
     IMAGE-INSENSITIVE FILE "image/im-prigr.bmp":U
     LABEL "&Ficha" 
     SIZE 4 BY 1.25 TOOLTIP "Emiss∆o Ordem Manutená∆o".

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

DEFINE BUTTON btHistorico 
     IMAGE-UP FILE "image/im-inisp.bmp":U
     LABEL "&Hist¢rico" 
     SIZE 4 BY 1.25 TOOLTIP "Hist¢rico Manutená‰es".

DEFINE BUTTON btImprimir 
     IMAGE-UP FILE "image/im-prigr.bmp":U
     IMAGE-INSENSITIVE FILE "image/im-prigr.bmp":U
     LABEL "Impress∆o" 
     SIZE 4 BY 1.25 TOOLTIP "Impress∆o".

DEFINE BUTTON btLast 
     IMAGE-UP FILE "image/im-las":U
     IMAGE-INSENSITIVE FILE "image/ii-las":U
     LABEL "Last":L 
     SIZE 4 BY 1.25.

DEFINE BUTTON btLegenda 
     IMAGE-UP FILE "image/im-abc.bmp":U
     LABEL "&Legenda" 
     SIZE 4 BY 1.25 TOOLTIP "Legenda".

DEFINE BUTTON btModifica 
     IMAGE-UP FILE "image/im-mod.bmp":U
     LABEL "A&lterar" 
     SIZE 4 BY 1.25 TOOLTIP "Alteraá∆o da Ordem de Manutená∆o".

DEFINE BUTTON btMV0613 
     IMAGE-UP FILE "image/im-ajust.bmp":U
     LABEL "&Encerrar" 
     SIZE 4 BY 1.25 TOOLTIP "Gerenciador da Manutená∆o".

DEFINE BUTTON btNext 
     IMAGE-UP FILE "image/im-nex":U
     IMAGE-INSENSITIVE FILE "image/ii-nex":U
     LABEL "Next":L 
     SIZE 4 BY 1.25.

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

DEFINE BUTTON btRedimensiona 
     LABEL "" 
     SIZE 90 BY .29.

DEFINE BUTTON btRefresh 
     IMAGE-UP FILE "image/im-sav":U
     IMAGE-INSENSITIVE FILE "image/ii-sav":U
     LABEL "" 
     SIZE 4 BY 1.25 TOOLTIP "Atualizar"
     FONT 4.

DEFINE BUTTON btReporte 
     IMAGE-UP FILE "image/im-edl.bmp":U
     LABEL "&Reportar" 
     SIZE 4 BY 1.25 TOOLTIP "Reporte de Horas".

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

DEFINE BUTTON btSearch 
     IMAGE-UP FILE "image/im-sea":U
     IMAGE-INSENSITIVE FILE "image/ii-sea":U
     LABEL "Search" 
     SIZE 4 BY 1.25.

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

DEFINE VARIABLE fi-contador AS DECIMAL FORMAT ">,>>>,>>9.9":U INITIAL ? 
     LABEL "Cont" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-km-eqpto AS CHARACTER FORMAT "X(256)":U 
     LABEL "Real" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nom-eqpto AS CHARACTER FORMAT "X(100)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY .88 NO-UNDO.

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
      ttDados.cod-oficial   width 12
ttDados.desc-dimensao width 30
ttDados.uso-real WIDTH 12
ttDados.val-uso-padr WIDTH 12
ttDados.un width 4
ttDados.garantia width 15
ttDados.ung width 4
ttDados.atualizacao WIDTH 10
ttDados.ordem width 12
ttDados.tipo width 15
fnEstado(ttDados.i-estado) @ c-estado width 17
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 90 BY 7.29
         FONT 1
         TITLE "".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btFirst AT ROW 1.13 COL 2 HELP
          "Primeira ocorrància"
     btPrev AT ROW 1.13 COL 6 HELP
          "Ocorrància anterior"
     btNext AT ROW 1.13 COL 10 HELP
          "Pr¢xima ocorrància"
     btLast AT ROW 1.13 COL 14 HELP
          "Èltima ocorrància"
     btGoTo AT ROW 1.13 COL 18.14 HELP
          "V† Para"
     btSearch AT ROW 1.13 COL 22.14 HELP
          "Pesquisa"
     btRefresh AT ROW 1.13 COL 28
     btAbrir AT ROW 1.13 COL 32
     btSalvar AT ROW 1.13 COL 36
     btExpande AT ROW 1.13 COL 40
     btParam AT ROW 1.13 COL 51 HELP
          "ParÉmetros"
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
     tt-mab-eqpto.ep-codigo AT ROW 2.63 COL 6.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY .88
     tt-mab-eqpto.cod-eqpto AT ROW 2.63 COL 12.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY .88
     fi-nom-eqpto AT ROW 2.63 COL 27.57 COLON-ALIGNED NO-LABEL
     fi-contador AT ROW 2.63 COL 61.86 COLON-ALIGNED
     fi-km-eqpto AT ROW 2.63 COL 76.14 COLON-ALIGNED
     btRedimensiona AT ROW 9.5 COL 1
     brDetalhe AT ROW 9.71 COL 1
     btLegenda AT ROW 17.17 COL 1.72 HELP
          "Legenda das Tarefas"
     btPneu AT ROW 17.21 COL 87 HELP
          "Apontamento Movimento Pneus"
     btMV0613 AT ROW 17.25 COL 39 HELP
          "Encerramento da Ordem de Manutená∆o"
     btModifica AT ROW 17.25 COL 43 HELP
          "Alteraá∆o da Ordem de Manutená∆o"
     btEncerra AT ROW 17.25 COL 47 HELP
          "Encerramento da Ordem de Manutená∆o"
     btConsulta AT ROW 17.25 COL 51 HELP
          "Consulta da Ordem de Manutená∆o"
     bt1 AT ROW 17.25 COL 55
     btFicha AT ROW 17.25 COL 59 HELP
          "Emiss∆o Ordem Manutená∆o"
     btReporte AT ROW 17.25 COL 62.86 HELP
          "Reporte de Horas"
     btPlano AT ROW 17.25 COL 67 HELP
          "Reporte de Horas"
     btHistorico AT ROW 17.25 COL 70.72 HELP
          "Hist¢rico Manutená‰es"
     btCO0604 AT ROW 17.25 COL 74.72 HELP
          "Consulta Horas Trabalhadas"
     btEqpto AT ROW 17.25 COL 78.86 HELP
          "Consulta Equipamento"
     btEvento AT ROW 17.25 COL 83 HELP
          "Consulta Movimentos de Eventos"
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
   Temp-Tables and Buffers:
      TABLE: tt-mab-eqpto T "?" NO-UNDO mgfro mab-eqpto
      ADDITIONAL-FIELDS:
          field r-rowid as rowid
      END-FIELDS.
   END-TABLES.
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
       ROW             = 3.75
       COLUMN          = 1
       HEIGHT          = 5.75
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
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {0713E8A2-850A-101B-AFC0-4210102A8DA7} type: TreeView */
      CtrlFrame-2:NAME = "CtrlFrame-2":U .
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {58DA8D8F-9D6A-101B-AFC0-4210102A8DA7} type: ImageList */
      CtrlFrame:MOVE-AFTER(fi-km-eqpto:HANDLE IN FRAME fpage0).
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
           rtToolBar-3:width-chars in frame fPage0 = (rtToolBar-3:width-chars in frame fPage0 + dXC)
           rtToolBar-3:row in frame fPage0         = (rtToolBar-3:row in frame fPage0 + dYC)
           btConsulta:row in frame fPage0          = (btConsulta:row in frame fPage0 + dYC)
           bt1:row in frame fPage0                 = (bt1:row in frame fPage0 + dYC)
           btMV0613:row in frame fPage0           = (btMV0613:row in frame fPage0 + dYC)
           btEqpto:row in frame fPage0             = (btEqpto:row in frame fPage0 + dYC)
           btPneu:row in frame fPage0              = (btPneu:row in frame fPage0 + dYC)
           btModifica:row in frame fPage0              = (btModifica:row in frame fPage0 + dYC)
           btEncerra:row in frame fPage0              = (btEncerra:row in frame fPage0 + dYC)
           btReporte:row in frame fPage0              = (btReporte:row in frame fPage0 + dYC)
           btPlano:row in frame fPage0              = (btPlano:row in frame fPage0 + dYC)
           btEvento:row in frame fPage0              = (btEvento:row in frame fPage0 + dYC)
           btHistorico:row in frame fPage0         = (btHistorico:row in frame fPage0 + dYC)
           btCO0604:row in frame fPage0             = (btCO0604:row in frame fPage0 + dYC)
           btFicha:row in frame fPage0             = (btFicha:row in frame fPage0 + dYC)
           btLegenda:row in frame fPage0           = (btLegenda:row in frame fPage0 + dYC)
           btMV0613:column in frame fPage0        = (btMV0613:column in frame fPage0 + (dXC))
           btConsulta:column in frame fPage0       = (btConsulta:column in frame fPage0 + (dXC))
           bt1:column in frame fPage0              = (bt1:column in frame fPage0 + (dXC))
           btFicha:column in frame fPage0          = (btFicha:column in frame fPage0 + (dXC))
           btHistorico:column in frame fPage0      = (btHistorico:column in frame fPage0 + (dXC))
           btCO0604:column in frame fPage0          = (btCO0604:column in frame fPage0 + (dXC))
           btEqpto:column in frame fPage0          = (btEqpto:column in frame fPage0 + (dXC))
           btPneu:column in frame fPage0           = (btPneu:column in frame fPage0 + (dXC))
           btModifica:column in frame fPage0           = (btModifica:column in frame fPage0 + (dXC))
           btEncerra:column in frame fPage0           = (btEncerra:column in frame fPage0 + (dXC))
           btReporte:column in frame fPage0           = (btReporte:column in frame fPage0 + (dXC))
           btPlano:column in frame fPage0           = (btPlano:column in frame fPage0 + (dXC))
           btEvento:column in frame fPage0           = (btEvento:column in frame fPage0 + (dXC))
           btHelp:column in frame fPage0           = (btHelp:column in frame fPage0 + dXC)
           btExit:column in frame fPage0           = (btExit:column in frame fPage0 + dXC)
           btReportsJoins:column in frame fPage0   = (btReportsJoins:column in frame fPage0 + dXC)
           btQueryJoins:column in frame fPage0     = (btQueryJoins:column in frame fPage0 + dXC)
           btParam:column in frame fPage0          = (btParam:column in frame fPage0 + dXC)
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
           btConsulta:row in frame fPage0          = (btConsulta:row in frame fPage0 - dYC)
           btMV0613:row in frame fPage0           = (btMV0613:row in frame fPage0 - dYC)
           btEqpto:row in frame fPage0             = (btEqpto:row in frame fPage0 - dYC)
           btPneu:row in frame fPage0              = (btPneu:row in frame fPage0 - dYC)
           btModifica:row in frame fPage0              = (btModifica:row in frame fPage0 - dYC)
           btEncerra:row in frame fPage0              = (btEncerra:row in frame fPage0 - dYC)
           btReporte:row in frame fPage0              = (btReporte:row in frame fPage0 - dYC)
           btPlano:row in frame fPage0              = (btPlano:row in frame fPage0 - dYC)
           btEvento:row in frame fPage0              = (btEvento:row in frame fPage0 - dYC)
           btHistorico:row in frame fPage0         = (btHistorico:row in frame fPage0 - dYC)
           btCO0604:row in frame fPage0             = (btCO0604:row in frame fPage0 - dYC)
           bt1:row in frame fPage0                 = (bt1:row in frame fPage0 - dYC)
           btFicha:row in frame fPage0             = (btFicha:row in frame fPage0 - dYC)
           btLegenda:row in frame fPage0           = (btLegenda:row in frame fPage0 - dYC)
           btMV0613:column in frame fPage0        = (btMV0613:column in frame fPage0 - (dXC))
           btConsulta:column in frame fPage0       = (btConsulta:column in frame fPage0 - (dXC))
           btFicha:column in frame fPage0          = (btFicha:column in frame fPage0 - (dXC))
           btHistorico:column in frame fPage0      = (btHistorico:column in frame fPage0 - (dXC))
           btCO0604:column in frame fPage0          = (btCO0604:column in frame fPage0 - (dXC))
           bt1:column in frame fPage0              = (bt1:column in frame fPage0 - (dXC))
           btEqpto:column in frame fPage0          = (btEqpto:column in frame fPage0 - (dXC))
           btPneu:column in frame fPage0           = (btPneu:column in frame fPage0 - (dXC))
           btModifica:column in frame fPage0              = (btModifica:column in frame fPage0 - dXC)
           btEncerra:column in frame fPage0              = (btEncerra:column in frame fPage0 - dXC)
           btReporte:column in frame fPage0              = (btReporte:column in frame fPage0 - dXC)
           btPlano:column in frame fPage0              = (btPlano:column in frame fPage0 - dXC)
           btEvento:column in frame fPage0              = (btEvento:column in frame fPage0 - dXC)
           btHelp:column in frame fPage0           = (btHelp:column in frame fPage0 - dXC)
           btExit:column in frame fPage0           = (btExit:column in frame fPage0 - dXC)
           btReportsJoins:column in frame fPage0   = (btReportsJoins:column in frame fPage0 - dXC)
           btQueryJoins:column in frame fPage0     = (btQueryJoins:column in frame fPage0 - dXC)
           btParam:column in frame fPage0          = (btParam:column in frame fPage0 - dXC)
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
ON ROW-DISPLAY OF brDetalhe IN FRAME fpage0
DO:
  if avail ttDados and ttDados.p-image = 30 then do:
      case ttDados.i-estado:
          when 2 then do:
              /** Em garantia = Verde **/
              assign ttDados.cod-oficial:fgcolor in browse brDetalhe   = 2
                     ttDados.desc-dimensao:fgcolor in browse brDetalhe = 2
                     ttDados.val-uso-padr:fgcolor in browse brDetalhe  = 2
                     ttDados.uso-real:fgcolor in browse brDetalhe      = 2
                     ttDados.un:fgcolor in browse brDetalhe            = 2
                     ttDados.garantia:fgcolor in browse brDetalhe      = 2
                     ttDados.ung:fgcolor in browse brDetalhe           = 2
                     ttDados.atualizacao:fgcolor in browse brDetalhe   = 2
                     ttDados.ordem:fgcolor in browse brDetalhe         = 2
                     ttDados.tipo:fgcolor in browse brDetalhe          = 2
                     c-estado:fgcolor in browse brDetalhe              = 2.
          end.
          when 3 then do:
              /** Vencido = Vermelho **/
              assign ttDados.cod-oficial:fgcolor in browse brDetalhe   = 12
                     ttDados.desc-dimensao:fgcolor in browse brDetalhe = 12
                     ttDados.val-uso-padr:fgcolor in browse brDetalhe  = 12
                     ttDados.uso-real:fgcolor in browse brDetalhe      = 12
                     ttDados.un:fgcolor in browse brDetalhe            = 12
                     ttDados.garantia:fgcolor in browse brDetalhe      = 12
                     ttDados.ung:fgcolor in browse brDetalhe           = 12
                     ttDados.atualizacao:fgcolor in browse brDetalhe   = 12
                     ttDados.ordem:fgcolor in browse brDetalhe         = 12
                     ttDados.tipo:fgcolor in browse brDetalhe          = 12
                     c-estado:fgcolor in browse brDetalhe              = 12.
          end.
      end case.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt1 wWindow
ON CHOOSE OF bt1 IN FRAME fpage0
do:
    if num-results("brDetalhe":U) > 0 then do:
        get current brDetalhe NO-LOCK.
        /*--- Seta cursor do mouse para espera ---*/
        SESSION:SET-WAIT-STATE("GENERAL":U).
        assign {&window-name}:sensitive = no.
        if avail ttDados and ttDados.ordem <> 0 then do:
            RUN piNarrativaOM.
        end.
        assign {&window-name}:sensitive = yes.
        SESSION:SET-WAIT-STATE("":U).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAbrir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAbrir wWindow
ON CHOOSE OF btAbrir IN FRAME fpage0
OR CHOOSE OF MENU-ITEM miAbrir  IN MENU mbMain DO:

   /** Troca as barras **/
   assign c-arquivo = replace(c-arquivo, "/", "/").
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


&Scoped-define SELF-NAME btCO0604
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCO0604 wWindow
ON CHOOSE OF btCO0604 IN FRAME fpage0 /* Componentes do Equipamento */
DO:
    IF brDetalhe:NUM-SELECTED-ROWS > 0 THEN DO:
        run chamaPrograma in this-procedure (input "cop/co0604.w":U).
    END.
  apply "ENTRY":U to btCO0604 in frame fPage0.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btConsulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btConsulta wWindow
ON CHOOSE OF btConsulta IN FRAME fpage0 /* Consultar */
DO:
    define variable hProgram as handle no-undo.

    if avail ttDados then do:
        find first mmv-ord-manut
            where  mmv-ord-manut.nr-ord-produ = ttDados.ordem no-lock no-error.
        /*--- Executa programa de inclus∆o de filho ---*/
        RUN mvp/mv0606.w PERSISTENT SET hProgram.

        IF VALID-HANDLE(hProgram) AND
            hProgram:Type = "PROCEDURE":U AND
            hProgram:FILE-NAME = "mvp/mv0606.w":U THEN
            /*--- Inicializa programa de inclus∆o de filho ---*/
            RUN initializeInterface IN hProgram.

        IF VALID-HANDLE(hProgram) THEN DO:
            if rowid(mmv-ord-manut) <> ? then
                /** Reposiciona programa no registro **/
                run repositionRecord in hProgram (input rowid(mmv-ord-manut)).
        end.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btEncerra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btEncerra wWindow
ON CHOOSE OF btEncerra IN FRAME fpage0 /* Encerrar */
DO:
    define variable hProgram as handle no-undo.

    if avail ttDados then do:
        find first mmv-ord-manut
            where  mmv-ord-manut.nr-ord-produ = ttDados.ordem no-lock no-error.
        /*--- Executa programa de inclus∆o de filho ---*/
        RUN mvp/mv0304.w PERSISTENT SET hProgram.

        IF VALID-HANDLE(hProgram) AND
            hProgram:Type = "PROCEDURE":U AND
            hProgram:FILE-NAME = "mvp/mv0304.w":U THEN
            /*--- Inicializa programa de inclus∆o de filho ---*/
            RUN initializeInterface IN hProgram.

        IF VALID-HANDLE(hProgram) THEN DO:
            if rowid(mmv-ord-manut) <> ? then
                /** Reposiciona programa no registro **/
                run repositionRecord in hProgram (input rowid(mmv-ord-manut)).
        end.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btEqpto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btEqpto wWindow
ON CHOOSE OF btEqpto IN FRAME fpage0 /* Equip. */
DO:
    run chamaPrograma in this-procedure (input "abp/ab0602.w":U).
  apply "ENTRY":U to btEqpto in frame fPage0.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btEvento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btEvento wWindow
ON CHOOSE OF btEvento IN FRAME fpage0 /* Evento */
DO:
    DEFINE VARIABLE hPrograma AS HANDLE NO-UNDO.

    ASSIGN {&WINDOW-NAME}:sensitive = no.

    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).

    /*--- Executa programa de inclus∆o de filho ---*/
    RUN abp/ab0303.w PERSISTENT SET hPrograma.

    /** Validaá∆o do Handle **/
    IF VALID-HANDLE(hPrograma) AND
        hPrograma:Type = "PROCEDURE":U AND
        hPrograma:FILE-NAME = "abp/ab0303.w":U THEN do:
        /*--- Inicializa programa de inclus∆o de filho ---*/
        RUN initializeInterface IN hPrograma.
        /*:T Reposiciona registro com base em um rowid */
    END.

    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    wait-for "CLOSE":U of hPrograma.

    if valid-handle (hPrograma) then
        delete procedure hPrograma.

    assign {&WINDOW-NAME}:sensitive = yes.  

  apply "ENTRY":U to btPneu in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExcel wWindow
ON CHOOSE OF btExcel IN FRAME fpage0 /* Planilha */
OR CHOOSE OF MENU-ITEM miExcel  IN MENU mbMain DO:
    IF brDetalhe:NUM-SELECTED-ROWS > 0 THEN DO:
        if chTreeView:Nodes:Count > 0 then do:
            run piExcel in this-procedure.
        end.
    END.
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


&Scoped-define SELF-NAME btFicha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFicha wWindow
ON CHOOSE OF btFicha IN FRAME fpage0 /* Ficha */
DO:
    run mvp/mv0501.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFirst wWindow
ON CHOOSE OF btFirst IN FRAME fpage0 /* First */
DO:
    run getFirst in hDBOEqpto.
    run displayEqpto in this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btGoTo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btGoTo wWindow
ON CHOOSE OF btGoTo IN FRAME fpage0 /* Go To */
DO:
    RUN goToRecord IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btGoTo wWindow
ON CTRL-T OF btGoTo IN FRAME fpage0 /* Go To */
DO:
  APPLY "CHOOSE" TO btGoto IN FRAME fPage0.
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
    IF brDetalhe:NUM-SELECTED-ROWS > 0 THEN DO:
        run chamaPrograma in this-procedure (input "mvp/mv0609.w":U).
    END.
  apply "ENTRY":U to btHistorico in frame fPage0.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btImprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btImprimir wWindow
ON CHOOSE OF btImprimir IN FRAME fpage0 /* Impress∆o */
OR CHOOSE OF MENU-ITEM miImprimir  IN MENU mbMain DO:
    IF brDetalhe:NUM-SELECTED-ROWS > 0 THEN DO:
        if chTreeView:Nodes:Count > 0 then
            run piImprimir in this-procedure.
    END.
    apply "ENTRY":U to btImprimir in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btLast wWindow
ON CHOOSE OF btLast IN FRAME fpage0 /* Last */
DO:
    run getLast in hDBOEqpto.
    run displayEqpto in this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btLegenda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btLegenda wWindow
ON CHOOSE OF btLegenda IN FRAME fpage0 /* Legenda */
DO: 
    assign {&window-name}:sensitive = no.
    run colp/col0602h.w.
    assign {&window-name}:sensitive = yes.
    apply "ENTRY":U to btLegenda in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btModifica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btModifica wWindow
ON CHOOSE OF btModifica IN FRAME fpage0 /* Alterar */
DO:
    define variable hProgram as handle no-undo.

    if avail ttDados then do:
        find first mmv-ord-manut
            where  mmv-ord-manut.nr-ord-produ = ttDados.ordem no-lock no-error.
        /*--- Executa programa de inclus∆o de filho ---*/
        RUN mvp/mv0301.w PERSISTENT SET hProgram.

        IF VALID-HANDLE(hProgram) AND
            hProgram:Type = "PROCEDURE":U AND
            hProgram:FILE-NAME = "mvp/mv0301.w":U THEN
            /*--- Inicializa programa de inclus∆o de filho ---*/
            RUN initializeInterface IN hProgram.

        IF VALID-HANDLE(hProgram) THEN DO:
            if rowid(mmv-ord-manut) <> ? then
                /** Reposiciona programa no registro **/
                run repositionRecord in hProgram (input rowid(mmv-ord-manut)).
        end.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btMV0613
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btMV0613 wWindow
ON CHOOSE OF btMV0613 IN FRAME fpage0 /* Encerrar */
DO:
    run mvp/mv0613.w.
    apply "ENTRY":U to btMV0613 in frame fPage0.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btNext wWindow
ON CHOOSE OF btNext IN FRAME fpage0 /* Next */
DO:
    run getNext in hDBOEqpto.
    run displayEqpto in this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btParam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btParam wWindow
ON CHOOSE OF btParam IN FRAME fpage0 /* ParÉmetros */
OR CHOOSE OF MENU-ITEM miSelecao  IN MENU mbMain DO:
    
  assign {&window-name}:sensitive = no.
  run colp/col0602c.w (input-output  table ttSelecao).
  assign {&window-name}:sensitive = yes.
  apply "ENTRY":U to btParam in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPlano
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPlano wWindow
ON CHOOSE OF btPlano IN FRAME fpage0 /* Plano */
DO:
    DEFINE VARIABLE hPrograma AS HANDLE NO-UNDO.

    ASSIGN {&WINDOW-NAME}:sensitive = no.

    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).

    /*--- Executa programa de inclus∆o de filho ---*/
    RUN mvp/mv0602.w PERSISTENT SET hPrograma.

    /** Validaá∆o do Handle **/
    IF VALID-HANDLE(hPrograma) AND
        hPrograma:Type = "PROCEDURE":U AND
        hPrograma:FILE-NAME = "mvp/mv0602.w":U THEN do:
        /*--- Inicializa programa de inclus∆o de filho ---*/
        RUN initializeInterface IN hPrograma.
        /*:T Reposiciona registro com base em um rowid */

        FOR FIRST mab-eqpto
            WHERE mab-eqpto.ep-codigo = INPUT FRAME fPage0 tt-mab-eqpto.ep-codigo
            AND   mab-eqpto.cod-eqpto = tt-mab-eqpto.cod-eqpto:SCREEN-VALUE IN FRAME fPage0 NO-LOCK:
            FOR FIRST mmv-eqpto-plano
                WHERE mmv-eqpto-plano.ep-codigo = mab-eqpto.ep-codigo
                AND   mmv-eqpto-plano.cod-eqpto = mab-eqpto.cod-eqpto NO-LOCK:
                FOR FIRST mmv-plano-prevent
                    WHERE mmv-plano-prevent.cod-model = mab-eqpto.cod-model
                    AND   mmv-plano-prevent.cod-plano = mmv-eqpto-plano.cod-plano NO-LOCK:
                END.
            END.
        END.
        IF AVAIL mmv-plano-prevent THEN DO:
            RUN repositionRecord IN hPrograma (INPUT ROWID(mmv-plano-prevent)).
        END.
    END.

    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    wait-for "CLOSE":U of hPrograma.

    if valid-handle (hPrograma) then
        delete procedure hPrograma.

    assign {&WINDOW-NAME}:sensitive = yes.  

    apply "ENTRY":U to btPlano in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPneu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPneu wWindow
ON CHOOSE OF btPneu IN FRAME fpage0 /* Pneu */
DO:
    DEFINE VARIABLE hPrograma AS HANDLE NO-UNDO.

    ASSIGN {&WINDOW-NAME}:sensitive = no.

    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).

    /*--- Executa programa de inclus∆o de filho ---*/
    RUN pnp/pn0301.w PERSISTENT SET hPrograma.

    /** Validaá∆o do Handle **/
    IF VALID-HANDLE(hPrograma) AND
        hPrograma:Type = "PROCEDURE":U AND
        hPrograma:FILE-NAME = "pnp/pn0301.w":U THEN do:
        /*--- Inicializa programa de inclus∆o de filho ---*/
        RUN initializeInterface IN hPrograma.
        /*:T Reposiciona registro com base em um rowid */
    END.

    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    wait-for "CLOSE":U of hPrograma.

    if valid-handle (hPrograma) then
        delete procedure hPrograma.

    assign {&WINDOW-NAME}:sensitive = yes.  

  apply "ENTRY":U to btPneu in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPrev wWindow
ON CHOOSE OF btPrev IN FRAME fpage0 /* Prev */
DO:
    run getPrev in hDBOEqpto.
    run displayEqpto in this-procedure.
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
    
    assign CtrlFrame:height = btRedimensiona:row - 3.7 no-error.         

    assign brDetalhe:row     = btRedimensiona:row + btRedimensiona:height no-error.
    assign brDetalhe:height  = (frame fPage0:height - 1.5) + 0.5 - (btRedimensiona:row) no-error.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRefresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRefresh wWindow
ON CHOOSE OF btRefresh IN FRAME fpage0
OR CHOOSE OF MENU-ITEM miAtualizar  IN MENU mbMain DO:
    session:set-wait-state ("GENERAL").
    run displayEqpto in this-procedure.
    session:set-wait-state ("").
    APPLY "Entry":U TO btRefresh.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btReporte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btReporte wWindow
ON CHOOSE OF btReporte IN FRAME fpage0 /* Reportar */
DO:
    DEFINE VARIABLE hPrograma AS HANDLE NO-UNDO.

    ASSIGN {&WINDOW-NAME}:sensitive = no.

    /*--- Seta cursor do mouse para espera ---*/
    SESSION:SET-WAIT-STATE("GENERAL":U).

    /*--- Executa programa de inclus∆o de filho ---*/
    RUN mvp/mv0302.w PERSISTENT SET hPrograma.

    /** Validaá∆o do Handle **/
    IF VALID-HANDLE(hPrograma) AND
        hPrograma:Type = "PROCEDURE":U AND
        hPrograma:FILE-NAME = "mvp/mv0302.w":U THEN do:
        /*--- Inicializa programa de inclus∆o de filho ---*/
        RUN initializeInterface IN hPrograma.
        /*:T Reposiciona registro com base em um rowid */
    END.

    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    
    wait-for "CLOSE":U of hPrograma.

    if valid-handle (hPrograma) then
        delete procedure hPrograma.

    assign {&WINDOW-NAME}:sensitive = yes.  

  apply "ENTRY":U to btPneu in frame fPage0.
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
   assign c-arquivo = replace(c-arquivo, "/", "/").

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
      for each ttSelecao exclusive-lock:
          export delimiter ";"
            ttSelecao.lCompartimentos
            ttSelecao.lPneus
            ttSelecao.lSubSistemas
            ttSelecao.lComponentes.
      end.
      /** Salva Vis‰es **/
      for each ttVisao exclusive-lock:
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


&Scoped-define SELF-NAME btSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSearch wWindow
ON CHOOSE OF btSearch IN FRAME fpage0 /* Search */
DO:
    {method/zoomfields.i 
        &ProgramZoom="frzoom/z01fr007.w"
        &FieldZoom1="ep-codigo"
        &FieldScreen1="tt-mab-eqpto.ep-codigo"
        &Frame1="fPage0"
        &FieldZoom2="cod-eqpto"
        &FieldScreen2="tt-mab-eqpto.cod-eqpto"
        &Frame2="fPage0"
        &EnableImplant="YES"}
        
    wait-for "CLOSE":U of hProgramZoom.
        
    /*:T Posiciona query, do DBO, atravÇs dos valores do °ndice £nico */
    RUN goToKey IN hDBOEqpto (INPUT input frame fPage0 tt-mab-eqpto.ep-codigo,
                              input input frame fPage0 tt-mab-eqpto.cod-eqpto).
    IF RETURN-VALUE = "NOK":U THEN DO:
        {utp/ut-liter.i "Equipamento"}
        RUN utp/ut-msgs.p (INPUT "SHOW":U, INPUT 2, INPUT return-value).
        RETURN NO-APPLY.
    end.
    else do:
        run displayEqpto in this-procedure.
    end.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btTxt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btTxt wWindow
ON CHOOSE OF btTxt IN FRAME fpage0
OR CHOOSE OF MENU-ITEM miTxt IN MENU mbMain DO:
    IF brDetalhe:NUM-SELECTED-ROWS > 0 THEN DO:
        if chTreeView:Nodes:Count > 0 then do:
            run piTexto in this-procedure.
        end.
    END.
    apply "ENTRY":U to btTxt in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btVisao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btVisao wWindow
ON CHOOSE OF btVisao IN FRAME fpage0 /* Classificar */
OR CHOOSE OF MENU-ITEM miClassifica  IN MENU mbMain DO:
 
  assign {&window-name}:sensitive = no.
  run colp/col0602b.w (input-output table ttVisao).
  assign {&window-name}:sensitive = yes.
  apply "ENTRY":U to btVisao in frame fPage0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tt-mab-eqpto.cod-eqpto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-mab-eqpto.cod-eqpto wWindow
ON LEAVE OF tt-mab-eqpto.cod-eqpto IN FRAME fpage0 /* cod-eqpto */
DO:
    if avail tt-mab-eqpto then do:
        for first mab-model
            where mab-model.cod-model = tt-mab-eqpto.cod-model no-lock:
            assign fi-nom-eqpto:screen-value in frame fPage0 = mab-model.des-model.
        end.
    end.
  
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


&Scoped-define SELF-NAME m_V_Para
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_V_Para wWindow
ON CHOOSE OF MENU-ITEM m_V_Para /* V† Para */
DO:
  APPLY "choose" TO btgoto IN FRAME fPage0.
END.

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

btRedimensiona:load-mouse-pointer ("image/size2.cur") in frame fPage0.

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

if valid-handle(hDBOEqpto) then
    run destroy in hDBOEqpto.

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
{utp/ut-liter.i "UN"}
assign ttDados.ung:label in browse brDetalhe = return-value.
{utp/ut-liter.i "Garantia"}
assign ttDados.garantia:label in browse brDetalhe = return-value.
{utp/ut-liter.i "Èlt Troca"}
assign ttDados.atualizacao:label in browse brDetalhe = return-value.
{utp/ut-liter.i "Ordem"}
assign ttDados.ordem:label in browse brDetalhe = return-value.
{utp/ut-liter.i "Tipo"}
assign ttDados.tipo:label in browse brDetalhe = return-value.
{utp/ut-liter.i "Descriá∆o"}
assign ttDados.desc-dimensao:label in browse brDetalhe = return-value.
{utp/ut-liter.i "Uso Real"}
assign ttDados.uso-real:label in browse brDetalhe = return-value.
{utp/ut-liter.i "UN"}
assign ttDados.un:label in browse brDetalhe = return-value.
{utp/ut-liter.i "Estado"}
assign c-estado:label in browse brDetalhe = return-value.
{utp/ut-liter.i "Uso Padr∆o"}
ASSIGN ttDados.val-uso-padr:LABEL IN BROWSE brDetalhe = RETURN-VALUE.
{utp/ut-liter.i Eqpto}
ASSIGN tt-mab-eqpto.ep-codigo:LABEL IN FRAME fPage0 = RETURN-VALUE.

assign tt-mab-eqpto.ep-codigo:sensitive in frame fPage0 = no
       tt-mab-eqpto.cod-eqpto:sensitive in frame fPage0 = no
       fi-nom-eqpto:sensitive           in frame fPage0 = no
       fi-contador:sensitive in frame fPage0 = no.

apply "CHOOSE":U to btFirst in frame fPage0.

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
run initializeDBOs in this-procedure.

assign chTreeView               = chCtrlFrame:TreeView
       chImageList              = chCtrlFrame-2:ImageList
       chTreeView:ImageList     = chImageList
       CtrlFrame:popup-menu     = pop-menu
       chTreeView:HideSelection = FALSE
       dWinXC                   = wWindow:width-chars
       dWinYC                   = wWindow:height-chars.

ChTreeView:Nodes:Clear().

if chTreeView:Nodes:Count > 0 then
   assign chTreeView:SelectedItem = chTreeView:Nodes(1). 

create ttSelecao.
assign ttSelecao.lCompartimentos   = yes
       ttSelecao.lPneus            = yes
       ttSelecao.lSubSistemas      = yes
       ttSelecao.lComponentes      = yes
       ttSelecao.lDetalharPordata  = NO.

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

define variable cEstabOfic as character no-undo.

/** Zera vari†veis no in°cio **/
assign vCodigo    = "":U
       vDescricao = "":U
       iImage     = 0
       rRowid     = ?.


assign iSeq-tar-plano = 0.

/** Verifica tipo de vis∆o (classificaá∆o) **/
case substring(trim(pValorDimensao),1,2):
    /** Estabelecimento **/
    when "01":U then do:
        if not avail estabelec or estabelec.cod-estabel <> mab-eqpto.cod-estabel then do:
            for first estabelec fields(nome cod-estabel)  
                where estabelec.cod-estabel = mab-eqpto.cod-estabel no-lock:
            end.
        end.
        if avail estabelec then do:
            assign vCodigo    = mab-eqpto.cod-estabel
                   vDescricao = estabelec.nome
                   iImage     = 1
                   rRowid     = rowid(estabelec).
        end.        
    end.
    /** Sistema **/
    when "02":U then do:
        if not avail mab-sub-sist or mab-sub-sist.cod-sub-sist <> cCodSubSist then do:
            for first mab-sub-sist fields(cod-sub-sist des-sub-sist cod-sist)
                where mab-sub-sist.cod-sub-sist = cCodSubSist no-lock:
            end.
        end.
        if avail mab-sub-sist then do:
            if not avail mab-sist or mab-sist.cod-sist <> mab-sub-sist.cod-sist then do:
                for first mab-sist fields(des-sist cod-sist)
                    where mab-sist.cod-sist = mab-sub-sist.cod-sist no-lock:
                end.
            end.
            if avail mab-sist then do:
                assign vCodigo    = mab-sub-sist.cod-sist
                       vDescricao = mab-sist.des-sist
                       iImage     = 2
                       rRowid     = rowid(mab-sist).
            end.
        end.
        else do:
            {utp/ut-liter.i "PNEU"}
            assign vCodigo    = "_"
                   vDescricao = trim(return-value)
                   iImage     = 2
                   rRowid     = ?.
        end.
    end.
    /** Tipo **/
    when "03":U then do:
        assign vCodigo    = trim(c-tipo)
               vDescricao = trim(c-tipo)
               iImage     = 3
               rRowid     = rowid(mab-eqpto).
    end.

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
    assign c-linha                    = replace(c-linha, chr(34), "")
           ttSelecao.lCompartimentos  = (entry(1,c-linha,";") = "yes")
           ttSelecao.lPneus           = (entry(2,c-linha,";") = "yes")
           ttSelecao.lSubSistemas     = (entry(3,c-linha,";") = "yes")
           ttSelecao.lComponentes     = (entry(4,c-linha,";") = "yes")
           ttSelecao.lDetalharPordata = (entry(5,c-linha,";") = "no").
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

DEFINE VARIABLE iDocto   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iImagem  AS INTEGER    NO-UNDO.
DEFINE VARIABLE rProgram AS ROWID      NO-UNDO.
DEFINE VARIABLE hProgram AS HANDLE     NO-UNDO.

if chTreeView:Nodes:Count > 0 then do:
    if int(chTreeView:SelectedItem:image) = iImagem then do:
        /** Guarda rowid do tree-view **/
        assign rRowid = to-rowid(entry(1,chTreeView:SelectedItem:Tag)).
        for first ttDados
            where rowid(ttDados) = rRowid no-lock:
            assign iDocto   = ttDados.ordem
                   rProgram = ttDados.r-rowid.
        end.
    end.
end.

/*--- Seta cursor do mouse para espera ---*/
SESSION:SET-WAIT-STATE("GENERAL":U).
assign {&window-name}:sensitive = no.
/*--- Executa programa de inclus∆o de filho ---*/
RUN value(pProgram) PERSISTENT SET hProgram.

IF VALID-HANDLE(hProgram) THEN DO:
    /** Reposiciona programa no registro **/
    RUN initializeInterface IN hProgram.
    IF AVAIL tt-mab-eqpto THEN DO:
        run repositionRecord in hProgram (input tt-mab-eqpto.r-rowid).
    END.
    /*--- Seta cursor do mouse para normal ---*/
    SESSION:SET-WAIT-STATE("":U).
    wait-for "CLOSE":U of hProgram.
END.
assign {&window-name}:sensitive = yes.

if valid-handle(hProgram) then
    delete procedure hProgram.

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

OCXFile = SEARCH( "col0602.wrx":U ).
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
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "col0602.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE converteInvertidaParaNormal wWindow 
PROCEDURE converteInvertidaParaNormal :
/*------------------------------------------------------------------------------
  Purpose:     converteInvertidaParaNormal
  Parameters:  entrada pInvertida = Valor Decimal da data e hora informada
               sa°da   pData      = Data de apontamento
               sa°da   pHora      = Hora de apontamento
  Notes:       Converte a data invertida doapontamento para data e hora normais
------------------------------------------------------------------------------*/
define input  parameter pInvertida as decimal   format "999999999999" no-undo.
define output parameter pData      as date      format "99/99/9999"   no-undo.
define output parameter pHora      as character format "999999"       no-undo.

assign pData = date(integer(substring(string(pInvertida),5,2)),  /** Màs **/
                    integer(substring(string(pInvertida),7,2)),  /** Dia **/
                    integer(substring(string(pInvertida),1,4))). /** Ano **/

assign pHora = substring(string(pInvertida),9,4) /** Hora + Minuto **/
               + "00":U.                         /** Segundos **/

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE converteParaHoraInvertida wWindow 
PROCEDURE converteParaHoraInvertida :
/*------------------------------------------------------------------------------
  Purpose:     converteParaHoraInvertida
  Parameters:  entrada pData      = Data de apontamento
               entrada pHora      = Hora de apontamento
               sa°da   pInvertida = Valor Decimal da data e hora informada
  Notes:       Converte a data e hora de apontamento para o campo hora invertida
------------------------------------------------------------------------------*/
define input  parameter pData      as character format "99/99/9999"   no-undo.
define input  parameter pHora      as character format "999999"       no-undo.
define output parameter pInvertida as decimal   format "999999999999" no-undo.

assign pInvertida = decimal(trim(substring(pData,7,4)    /** Ano **/
                               + substring(pData,4,2)    /** Màs **/
                               + substring(pData,1,2)    /** Dia **/
                               + substring(pHora,1,2)    /** Hora **/
                               + substring(pHora,3,2))). /** Minuto **/

return "OK":U.

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

/** Limpa o tree-view **/
ChTreeView:Nodes:Clear().

assign i-cont = 0.
/** Verifica se existem vis‰es escolhidas **/
for last ttVisao no-lock:
    assign iQtSeq = ttVisao.sequencia.
end.
if iQtSeq = 0 then return.

IF  ttSelecao.lDetalharPorData THEN
    ASSIGN iQtSeq = 30.

/** Busca os dados criados **/
for each  ttDados 
    where ttDados.sequencia <= iQtSeq exclusive-lock 
    by cod-dimensao
    BY desc-dimensao:

    /** Conta sequàncias de linhas **/ 
    assign i-cont           = i-cont + 1
           ttDados.seq-tree = i-cont
           i-img            = ttDados.p-image.

    /** Inclui primeira linha **/
    if ttDados.sequencia = 1 then do:
        chTreeView:Nodes:Add (,, "i" + string(i-cont), /*string(ttDados.cod-oficial) + " - " +*/ ttDados.desc-dimensao, i-img) . 
        /** Guarda chave (rowid) da temp-table no tree-view **/
        assign chTreeView:Nodes:Item ("i" + string(i-cont)):Tag = string(rowid(ttDados)) + ",1,".
        if ttDados.r-rowid <> ? then
            assign chTreeView:Nodes:Item ("i" + string(i-cont)):Tag = string(rowid(ttDados)) + ",1," + string(ttDados.r-rowid).
    end.
    else do:
        /** Busca filhos da vis∆o escolhida **/
        find first bfttDados 
            where  bfttDados.cod-dimensao = ttDados.cod-dimens-pai no-lock no-error.
        if avail bfttDados then do:
            /** Inclui linhas das vis‰es filhas **/
            chTreeView:Nodes:Add ("i" + string(bfttDados.seq-tree),4 , "i" + string(i-cont), /*string(ttDados.cod-oficial) + " - " +*/ ttDados.desc-dimensao, i-img) . 
            /** Guarda chave (rowid) da temp-table no tree-view **/
            assign chTreeView:Nodes:Item ("i" + string(i-cont)):Tag = string(rowid(ttDados)) + ",1,". 
            if  ttDados.r-rowid <> ? then
                assign chTreeView:Nodes:Item ("i" + string(i-cont)):Tag = string(rowid(ttDados)) + ",2," + string(ttDados.r-rowid).
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
assign ttVisao.dimensao =  '02 ' + fnLabels(2)
       ttVisao.sequencia = 1.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayEqpto wWindow 
PROCEDURE displayEqpto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable km-eqpto as decimal format ">>>,>>>,>>9.99" no-undo.

/** Busca a Data/hora Invertida Atual para o C†lculo **/
run piFormataSegundoParaHora (input  time,
                              output c-hora).
run converteParaHoraInvertida (input  string(today,'99/99/9999'),
                               input  c-hora,
                               output deInvertidaAtual).

run getRecord in hDBOEqpto (output table tt-mab-eqpto).
if return-value = "OK":U then do:
    for first tt-mab-eqpto no-lock:
        disp tt-mab-eqpto.ep-codigo 
             tt-mab-eqpto.cod-eqpto with frame fPage0.
        /** Busca £ltima quilometragem do equipamento **/
        run piCalculaKM in this-procedure (input  tt-mab-eqpto.ep-codigo,
                                           input  tt-mab-eqpto.cod-eqpto,
                                           input  deInvertidaAtual,
                                           input  1,
                                           output km-eqpto).
        FOR LAST  mab-movto-km-eqpto
            WHERE mab-movto-km-eqpto.ep-codigo = tt-mab-eqpto.ep-codigo
            AND   mab-movto-km-eqpto.cod-eqpto = tt-mab-eqpto.cod-eqpto NO-LOCK:
        END.
        if avail mab-movto-km-eqpto then do:
            ASSIGN fi-contador = mab-movto-km-eqpto.val-hodom-horim.
            DISP fi-contador WITH FRAME fPage0.
        end.
        else do:
            ASSIGN fi-contador = 0.
            DISP fi-contador WITH FRAME fPage0.
        end.
        assign fi-km-eqpto:screen-value in frame fPage0 = string(km-eqpto,'>>>,>>>,>>9.99').
        apply "LEAVE":U to tt-mab-eqpto.cod-eqpto in frame fPage0.
        run piAtualizar in this-procedure.
    end.
end.

return "OK":U.

END procedure.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goToRecord wWindow 
PROCEDURE goToRecord :
/*:T------------------------------------------------------------------------------
  Purpose:     Exibe dialog de V† Para
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

DEFINE VARIABLE cep-codigo LIKE tt-mab-eqpto.ep-codigo VIEW-AS FILL-IN SIZE 5.00 BY 0.88  NO-UNDO.
DEFINE VARIABLE ccod-eqpto LIKE tt-mab-eqpto.cod-eqpto VIEW-AS FILL-IN SIZE 16.00 BY 0.88    NO-UNDO.

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
{utp/ut-liter.i V†_Para_Equipamento *}
ASSIGN FRAME fGoToRecord:TITLE = RETURN-VALUE.

ON "CHOOSE":U OF btGoToOK IN FRAME fGoToRecord DO:
    ASSIGN cep-codigo ccod-eqpto.
    
    /*:T Posiciona query, do DBO, atravÇs dos valores do °ndice £nico */
    RUN goToKey IN hDBOEqpto (INPUT cep-codigo,
                              input ccod-eqpto).
    IF RETURN-VALUE = "NOK":U THEN DO:
        {utp/ut-liter.i "Equipamento"}
        RUN utp/ut-msgs.p (INPUT "SHOW":U, INPUT 2, INPUT return-value).
        
        RETURN NO-APPLY.
    END.

    run displayEqpto in this-procedure.
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeDBOs wWindow 
PROCEDURE initializeDBOs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*:T--- Verifica se o DBO j† est† inicializado ---*/
IF NOT VALID-HANDLE(hDBOEqpto) OR
    hDBOEqpto:TYPE <> "PROCEDURE":U OR
    hDBOEqpto:FILE-NAME <> "frbo/bofr007.p":U THEN DO:
    run frbo/bofr007.p persistent set hDBOEqpto.
end.

run openQueryStatic IN hDBOEqpto (INPUT "Main":U) NO-ERROR.

run getFirst in hDBOEqpto.

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
  Parameters:  
  Notes:       Busca os dados e atualiza o programa
------------------------------------------------------------------------------*/
define variable i-emp-ini   as integer   no-undo.
define variable i-emp-fim   as integer   no-undo.
define variable c-eqpto-ini as character no-undo.
define variable c-eqpto-fim as character no-undo.
define variable rRow        as rowid     no-undo.


empty temp-table ttDados.
{&OPEN-QUERY-brDetalhe}

/** N∆o foram selecionadas as vis‰es **/
if not can-find(first ttVisao) then do:
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

/** Acompanhamento **/
RUN utp/ut-acomp.p PERSISTENT SET hAcomp.
{utp/ut-liter.i "Consulta Partes do Equipamento"}
RUN pi-inicializar IN hAcomp (trim(return-value)).

do on stop undo, return "NOK":U:
    /** Seleciona os equipamento **/
    for each  mab-eqpto
        where mab-eqpto.ep-codigo         = tt-mab-eqpto.ep-codigo
        and   mab-eqpto.cod-eqpto         = tt-mab-eqpto.cod-eqpto no-lock:

        for first mab-model
            where mab-model.cod-model = mab-eqpto.cod-model no-lock:
        end.
        if not avail mab-model then next.

        if ttSelecao.lCompartimentos then do:
            {utp/ut-liter.i "Compartimentos"}
            run pi-acompanhar IN hAcomp (INPUT string(mab-eqpto.ep-codigo) + "-" + mab-eqpto.cod-eqpto + ": " + return-value).
            run piCompartimentos in this-procedure.
        end.

        if ttSelecao.lComponentes then do:
            {utp/ut-liter.i "Componentes"}
            run pi-acompanhar IN hAcomp (INPUT string(mab-eqpto.ep-codigo) + "-" + mab-eqpto.cod-eqpto + ": " + return-value).
            run piComponentes in this-procedure.
        end.

        if ttSelecao.lSubSistemas then do:
            {utp/ut-liter.i "Sub-Sistemas"}
            run pi-acompanhar IN hAcomp (INPUT string(mab-eqpto.ep-codigo) + "-" + mab-eqpto.cod-eqpto + ": " + return-value).
            run piSubSistemas in this-procedure.
        end.

        if ttSelecao.lPneus then do:
            {utp/ut-liter.i "Pneus"}
            run pi-acompanhar IN hAcomp (INPUT string(mab-eqpto.ep-codigo) + "-" + mab-eqpto.cod-eqpto + ": " + return-value).
            run piPneus in this-procedure.
        end.
    end.
end.

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
/** Se tiver filhos, mostra no browse, os filhos **/
if avail ttDados then do:
    run piBrowseTitulo in this-procedure (ttDados.p-image).
    /** Retira marcaá∆o da geraá∆o de tarefas **/
    FOR FIRST ttSelecao NO-LOCK:
        IF  ttSelecao.lDetalharPorData = NO THEN DO:
            open query brDetalhe for each  ttDados
                                     where ttDados.cod-dimens-pai = bfttDados.cod-dimensao NO-LOCK 
                                                                              by    substring(ttDados.desc-dimensao,1,188) 
                                                                              by    substring(ttDados.cod-oficial,1,188).
        END.
        ELSE DO:
            open query brDetalhe for each  ttDados
                                     where ttDados.cod-dimens-pai = bfttDados.cod-dimensao NO-LOCK
                                       BY ttDados.atualizacao DESCEND
                                       BY substring(ttDados.desc-dimensao,1,188)
                                       BY substring(ttDados.cod-oficial,1,188).
        END.
    END.

end.
/** Sen∆o, mostra ele mesmo **/
else do:
    run piBrowseTitulo in this-procedure (bfttDados.p-image).
    FOR FIRST ttSelecao NO-LOCK:
        IF  ttSelecao.lDetalharPorData = NO THEN DO:
            open query brDetalhe for each  ttDados
                                     where ttDados.cod-dimensao = bfttDados.cod-dimensao no-lock.
        END.
        ELSE DO:
            open query brDetalhe for each  ttDados
                                     where ttDados.cod-dimensao = bfttDados.cod-dimensao no-lock
                                        BY ttDados.atualizacao DESCEND.
        END.
    END.
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

if     pImagem = 1
    OR pImagem = 2
    OR pImagem = 3 then do:
    assign ttDados.uso-real:visible in browse brDetalhe    = no
           ttDados.un:visible in browse brDetalhe          = no  
           ttDados.garantia:visible in browse brDetalhe    = no
           ttDados.ung:visible in browse brDetalhe         = no
           ttDados.atualizacao:visible in browse brDetalhe = no
           ttDados.ordem:visible in browse brDetalhe       = no
           ttDados.tipo:visible in browse brDetalhe        = no
           c-estado:visible in browse brDetalhe            = no.
           
end.

IF pImagem = 30 then do:
    assign ttDados.uso-real:visible in browse brDetalhe    = yes
           ttDados.un:visible in browse brDetalhe          = yes  
           ttDados.garantia:visible in browse brDetalhe    = yes
           ttDados.ung:visible in browse brDetalhe         = yes
           ttDados.atualizacao:visible in browse brDetalhe = yes
           ttDados.ordem:visible in browse brDetalhe       = yes
           ttDados.tipo:visible in browse brDetalhe        = yes
           c-estado:visible in browse brDetalhe            = yes.
end.

ASSIGN brDetalhe:title in frame fPage0 = fnLabels(pImagem).
if pImagem = 4 then do:
    assign pImagem = 31
           brDetalhe:title in frame fPage0 = fnLabels(pImagem).
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCalculaKM wWindow 
PROCEDURE piCalculaKM :
/*------------------------------------------------------------------------------
  Purpose:     buscaKM
  Parameters:  entrada pEmp       = CΩdigo da Empresa
               entrada pEqpto     = CΩdigo do Equipamento
               entrada pInvertida = Data/Hora Invertida
               entrada pTipo      = Qual campo retornar (1 = Percurso / 2 = Contador)
               sa≠da   pKM        = Valor da Quilometragem na data
  Notes:       Busca a quilometragem do equipamento em uma data
------------------------------------------------------------------------------*/
define input  parameter pEmp       as char no-undo.
define input  parameter pEqpto     as char no-undo.
define input  parameter pInvertida as dec  no-undo.
define input  parameter pTipo      as int  no-undo.
define output parameter pKM        as dec  no-undo.

/** Busca Quilometragem do HistΩrico **/
if can-find(first mab-movto-km-eqpto
            where mab-movto-km-eqpto.ep-codigo             = pEmp
            and   mab-movto-km-eqpto.cod-eqpto             = pEqpto
            and   mab-movto-km-eqpto.val-dat-hora-invrtda <= pInvertida no-lock) then do:
    for last  mab-movto-km-eqpto fields(val-km-real val-hodom-horim)
        where mab-movto-km-eqpto.ep-codigo             = pEmp
        and   mab-movto-km-eqpto.cod-eqpto             = pEqpto
        and   mab-movto-km-eqpto.val-dat-hora-invrtda <= pInvertida no-lock:
        case pTipo:
            when 1 then
                assign pKM = mab-movto-km-eqpto.val-km-real.
            when 2 then
                assign pKM = mab-movto-km-eqpto.val-hodom-horim.
        end case.
    end.
end.
else do:
    /** Utiliza Quilometragem Inicial **/
    case pTipo:
        when 1 then
            assign pKM = tt-mab-eqpto.val-km-inicial.
        when 2 then
            assign pKM = tt-mab-eqpto.val-hodom-horim-inicial.
    end case.
end.

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
DEFINE VARIABLE d-data-garantia as date format "99/99/9999" NO-UNDO.
/** Busca registro **/
find first ttDados 
     where ttDados.cod-dimensao  = (cCodPai + "#" + vCodigo) /*usar a # quando o cadastro do eqpto Ç feito 1,10,2,20 sendo que o correto seria fazer 01,10,02,20 */ 
     and   ttDados.sequencia     = ttVisao.sequencia exclusive-lock no-error.
/** Se n∆o estiver criado, cria registro **/
if not avail ttDados then do:
   create ttDados.
   assign ttDados.cod-dimensao   = (cCodPai + "#" + vCodigo)  /** C¢digo encadeado para tree-view **/
          ttDados.cod-oficial    = vCodigo                      /** C¢digo Original do registro **/
          ttDados.desc-dimensao  = vDescricao                   /** Descriá∆o do c¢digo **/
          ttDados.sequencia      = ttVisao.sequencia            /** Sequància dos dados **/
          ttDados.p-image        = iImage                       /** N£mero da imagem do registro **/
          ttDados.r-rowid        = rRowid                       /** Rowid do registro para consultas futuras **/
          ttDados.cod-dimens-pai = cCodPai                      /** C¢digo do pai do registro **/
          ttDados.ep-codigo      = mab-eqpto.ep-codigo
          ttDados.cod-eqpto      = mab-eqpto.cod-eqpto.

   /** Equipamento **/
   assign ttDados.c-des-eqpto = string(mab-eqpto.ep-codigo) + "-" + mab-eqpto.cod-eqpto.
   if not avail mab-model or mab-model.cod-model <> mab-eqpto.cod-model then do:
       for first mab-model fields(des-model cod-model un)  
           where mab-model.cod-model = mab-eqpto.cod-model no-lock:
       end.
   end.
   if avail mab-model then do:
       /** Busca última contador do equipamento **/
       run piCalculaKM in this-procedure (input  tt-mab-eqpto.ep-codigo,
                                          input  tt-mab-eqpto.cod-eqpto,
                                          input  deInvertidaAtual,
                                          input  2,
                                          output deKMEqpto).
       {utp/ut-liter.i "Contador"}
       assign ttDados.c-des-eqpto = string(tt-mab-eqpto.ep-codigo) + "-" + tt-mab-eqpto.cod-eqpto + " - " + mab-model.des-model + "       " + trim(return-value) + ": " + trim(string(deKMEqpto,'>>>,>>>,>>9.99')).
       /** Busca última quilometragem do equipamento **/
       run piCalculaKM in this-procedure (input  tt-mab-eqpto.ep-codigo,
                                          input  tt-mab-eqpto.cod-eqpto,
                                          input  deInvertidaAtual,
                                          input  1,
                                          output deKMEqpto).
       {utp/ut-liter.i "acumulado"}
       assign ttDados.c-des-eqpto = ttDados.c-des-eqpto + "       " + caps(mab-model.un) + " " + trim(return-value) + ": " + trim(string(deKMEqpto,'>>>,>>>,>>9.99')).
   end.
end.

assign cCodPai = ttDados.cod-dimensao.

if ttVisao.sequencia = iVisao then do:
    /** Busca registro **/
    find first ttDados
        where ttDados.cod-dimensao  = (cCodPai + "#" + c-cod-visao) /*usar a # quando o cadastro do eqpto Ç feito 1,10,2,20 sendo que o correto seria fazer 01,10,02,20 */
        and   ttDados.sequencia     = 30 exclusive-lock no-error.
    /** Se n∆o estiver criado, cria registro **/
    if not avail ttDados then do:
        create ttDados.
        assign ttDados.cod-dimensao   = (cCodPai + "#" + c-cod-visao) /** C¢digo encadeado para tree-view **/
               ttDados.cod-oficial    = c-cod-visao                   /** C¢digo Original do registro **/
               ttDados.desc-dimensao  = c-des-visao                /** Descriá∆o do c¢digo **/
               ttDados.sequencia      = 30                        /** Sequància dos dados **/
               ttDados.p-image        = 30                        /** N£mero da imagem do registro **/
               ttDados.r-rowid        = rRowid                    /** Rowid do registro para consultas futuras **/
               ttDados.cod-dimens-pai = cCodPai                   /** C¢digo do pai do registro **/
               ttDados.tipo           = c-tipo
               ttDados.uso-real       = d-uso-real
               ttDados.un             = cUN
               ttDados.garantia       = d-garantia
               ttDados.ung            = c-ung
               ttDados.atualizacao    = d-atualiza
               ttDados.ordem          = i-ordem
               ttDados.ep-codigo      = mab-eqpto.ep-codigo
               ttDados.cod-eqpto      = mab-eqpto.cod-eqpto
               ttDados.c-cod-sub-sist = cCodSubSist
               ttDados.val-uso-padr   = d-uso-padrao.

        IF  ttSelecao.lDetalharPorData THEN
            ASSIGN ttDados.p-image        = 4.

        if ttDados.garantia > 0 then do:
            {utp/ut-liter.i Dia}
            IF ttDados.ung = RETURN-VALUE THEN DO:
                ASSIGN d-data-garantia = ttDados.atualiza + ttDados.garantia.
                IF d-data-garantia >= TODAY THEN DO:
                    /** Em garantia = Verde **/
                    assign ttDados.i-estado = 2.
                END.
                ELSE DO:
                    /** Vencido = Vermelho **/
                    /* para Usina Colombo, garantias vencidas devem entrar com o mesmo conceito
                    de "Sem Garantia", por isso o estado aqui est† igual a 1 e n∆o igual a 3 */
                    assign ttDados.i-estado = 1.
                END.
            END.
            ELSE DO:
                if ttDados.garantia >= ttDados.uso-real then do:
                    /** Em garantia = Verde **/
                    assign ttDados.i-estado = 2.
                end.
                else do:
                    /** Vencido = Vermelho **/
                    /* para Usina Colombo, garantias vencidas devem entrar com o mesmo conceito
                       de "Sem Garantia", por isso o estado aqui est† igual a 1 e n∆o igual a 3 */
                    assign ttDados.i-estado = 1.
                end.
            END.
        end.
        else do:
            assign ttDados.i-estado = 1.
        end.
    end.
    IF  ttSelecao.lDetalharPorData THEN DO:
        CASE c-tipo:
            WHEN "Pneu":U THEN DO:
                RUN piPneusHistorico IN THIS-PROCEDURE.
            END.
            WHEN "Sub-Sistema":U THEN DO:
                RUN piSubSistemasHistorico IN THIS-PROCEDURE.
            END.
            WHEN "Componente":U THEN DO:
                RUN piComponentesHist IN THIS-PROCEDURE.
            END.
            WHEN "Compartimento":U THEN DO:
                RUN piCompartimentosHist IN THIS-PROCEDURE.
            END.
        END CASE.
    END.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCompartimentos wWindow 
PROCEDURE piCompartimentos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable pData as date      format "99/99/9999" no-undo.
define variable pHora as character format "999999"     no-undo.

for each  mab-comptmento-eqpto
    where mab-comptmento-eqpto.ep-codigo = mab-eqpto.ep-codigo
    and   mab-comptmento-eqpto.cod-eqpto = mab-eqpto.cod-eqpto no-lock:
    ASSIGN d-uso-real = 0.
    for first mab-period-manut
        where mab-period-manut.ep-codigo      = mab-comptmento-eqpto.ep-codigo
        and   mab-period-manut.cod-eqpto      = mab-comptmento-eqpto.cod-eqpto
        and   mab-period-manut.cod-comptmento = mab-comptmento-eqpto.cod-comptmento no-lock:
    end.
    if avail mab-period-manut then do:
        for first mab-comptmento
            where mab-comptmento.cod-comptmento = mab-comptmento-eqpto.cod-comptmento no-lock:
        end.
        assign c-cod-visao = mab-comptmento-eqpto.cod-comptmento.
        if avail mab-comptmento then do:
            assign c-des-visao = mab-comptmento.des-comptmento.
        end.
        else next.
        release mab-item-lubrific.
        release mab-abastec-lubrific.
        for last  mab-item-lubrific use-index mbtmlbrf-02
            where mab-item-lubrific.ep-codigo      = mab-period-manut.ep-codigo
            and   mab-item-lubrific.cod-eqpto      = mab-period-manut.cod-eqpto
            and   mab-item-lubrific.cod-comptmento = mab-comptmento-eqpto.cod-comptmento
            and   mab-item-lubrific.cod-event      = mab-period-manut.cod-event no-lock:
        end.
        if avail mab-item-lubrific then do:
            for first mab-abastec-lubrific
                where mab-abastec-lubrific.num-docto = mab-item-lubrific.num-docto no-lock:
            end.
        END.
        if avail mab-abastec-lubrific then do:
            assign d-atualiza = mab-abastec-lubrific.dat-movto.
        end.
        else do:
            assign d-atualiza = mab-comptmento-eqpto.dat-inicial.
        end.
        {utp/ut-liter.i Compartimento *}
        assign d-uso-real = dec(mab-period-manut.val-km-real)
               cUN        = mab-model.un
               d-garantia = 0
               c-ung = mab-model.un
               i-ordem    = 0
               c-tipo     = return-value
               d-uso-padrao = mab-period-manut.val-km-padr.
        /** Busca garantia da Tarefa da OM **/
        for last  mmv-tar-ord-manut use-index mmvtrrdm-02
            where mmv-tar-ord-manut.ep-codigo    = mab-eqpto.ep-codigo
            and   mmv-tar-ord-manut.cod-eqpto    = mab-eqpto.cod-eqpto
            and   mmv-tar-ord-manut.cod-evento   = mab-period-manut.cod-evento
            and   mmv-tar-ord-manut.cod-sub-sist = mab-comptmento-eqpto.cod-sub-sist no-lock:
            if mmv-tar-ord-manut.val-km-hora-gartia <> 0 and 
               mmv-tar-ord-manut.val-km-hora-gartia <> ? then do:
                assign d-garantia = mmv-tar-ord-manut.val-km-hora-gartia.
            end.
            else do:
                IF mmv-tar-ord-manut.num-dias-gartia <> 0 THEN DO:
                    {utp/ut-liter.i Dia}
                    assign d-garantia = dec(mmv-tar-ord-manut.num-dias-gartia)
                           c-ung = return-value.
                END.
            end.
            for first mmv-ord-manut
                where mmv-ord-manut.nr-ord-produ = mmv-tar-ord-manut.nr-ord-produ no-lock:
            end.
            if avail mmv-ord-manut then do:
                if mmv-ord-manut.dat-term <> ? then do:
                    assign d-atualiza = mmv-ord-manut.dat-term.
                end.
                else do:
                    if mmv-tar-ord-manut.val-dat-invrtda-term <> 0 then do:
                        run converteInvertidaParaNormal (input  mmv-tar-ord-manut.val-dat-invrtda-term,
                                                         output pData,
                                                         output pHora).
                    end.
                    if pData <> ? then do:
                        assign d-atualiza = pData.
                    end.
                end.
            end.
            assign i-ordem = mmv-tar-ord-manut.nr-ord-produ.
        end.
        assign cCodSubSist = mab-comptmento-eqpto.cod-sub-sist. 
        run piCarrega in this-procedure.
    end.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCompartimentosHist wWindow 
PROCEDURE piCompartimentosHist :
/*------------------------------------------------------------------------------
  Purpose:     piCompartimentosHist
  Parameters:  <none>
  Notes:       Popula o hist¢rico do Compartimento
------------------------------------------------------------------------------*/
DEFINE VARIABLE d-data-garantia AS DATE    NO-UNDO.
DEFINE VARIABLE d-km-inicial    AS DECIMAL NO-UNDO.
DEFINE VARIABLE d-km-final      AS DECIMAL NO-UNDO.

DEFINE BUFFER bf-mab-item-lubrific FOR mab-item-lubrific.

for EACH  mab-item-lubrific use-index mbtmlbrf-02
    where mab-item-lubrific.ep-codigo      = mab-period-manut.ep-codigo
    and   mab-item-lubrific.cod-eqpto      = mab-period-manut.cod-eqpto
    and   mab-item-lubrific.cod-comptmento = mab-comptmento-eqpto.cod-comptmento
    and   mab-item-lubrific.cod-event      = mab-period-manut.cod-event no-lock:
    for first mab-abastec-lubrific
        where mab-abastec-lubrific.num-docto = mab-item-lubrific.num-docto no-lock:
    end.

    /** BUSCA KM FINAL **/
    run piCalculaKM in this-procedure (input  mab-item-lubrific.ep-codigo,
                                       input  mab-item-lubrific.cod-eqpto,
                                       input  mab-item-lubrific.val-dat-hora-invrtda,
                                       input  1,
                                       output d-km-final).

    FOR LAST  bf-mab-item-lubrific use-index mbtmlbrf-02
        WHERE bf-mab-item-lubrific.ep-codigo            = mab-item-lubrific.ep-codigo
        AND   bf-mab-item-lubrific.cod-eqpto            = mab-item-lubrific.cod-eqpto
        AND   bf-mab-item-lubrific.cod-comptmento       = mab-item-lubrific.cod-comptmento
        AND   bf-mab-item-lubrific.cod-event            = mab-item-lubrific.cod-event
        AND   bf-mab-item-lubrific.val-dat-hora-invrtda < mab-item-lubrific.val-dat-hora-invrtda NO-LOCK:
    END.
    IF AVAIL bf-mab-item-lubrific THEN DO:
        /* BUSCA KM INICIAL */
        run piCalculaKM in this-procedure (input  bf-mab-item-lubrific.ep-codigo,
                                           input  bf-mab-item-lubrific.cod-eqpto,
                                           input  bf-mab-item-lubrific.val-dat-hora-invrtda,
                                           input  1,
                                           output d-km-inicial).
    END.
    ELSE DO:
        ASSIGN d-km-inicial = mab-eqpto.val-km-inicial + mab-comptmento-eqpto.val-km-inicial.
    END.

    if avail mab-abastec-lubrific then do:
        assign d-atualiza = mab-abastec-lubrific.dat-movto.
    end.
    else do:
        assign d-atualiza = mab-comptmento-eqpto.dat-inicial.
    end.

    {utp/ut-liter.i Compartimento *}
    assign d-uso-real = d-km-final - d-km-inicial
           c-tipo     = return-value
           i-ordem    = 0
           d-uso-padrao = mab-period-manut.val-km-padr.

    FOR FIRST mmv-ord-manut
        WHERE mmv-ord-manut.nr-ord-produ = mab-abastec-lubrific.num-docto-orig NO-LOCK:
        ASSIGN i-ordem = mmv-ord-manut.nr-ord-produ.
        /** Busca garantia da Tarefa da OM **/
        for last  mmv-tar-ord-manut USE-INDEX mmvtrrdm-02
            where mmv-tar-ord-manut.nr-ord-produ = mmv-ord-manut.nr-ord-produ
            AND   mmv-tar-ord-manut.cod-evento   = mab-item-lubrific.cod-event
            and   mmv-tar-ord-manut.cod-sub-sist = mab-comptmento-eqpto.cod-sub-sist no-lock:
            if mmv-tar-ord-manut.val-km-hora-gartia <> 0 and 
               mmv-tar-ord-manut.val-km-hora-gartia <> ? then do:
                assign d-garantia = mmv-tar-ord-manut.val-km-hora-gartia.
            END.
            else do:
                IF mmv-tar-ord-manut.num-dias-gartia <> 0 THEN DO:
                    {utp/ut-liter.i Dia}
                    assign d-garantia = dec(mmv-tar-ord-manut.num-dias-gartia)
                           c-ung = return-value.
                END.
            END.
        END.
    END.

    /** Busca registro **/
    find first ttDados
        where ttDados.cod-dimensao  = (cCodPai + "#" + c-cod-visao + "#" + string(mab-item-lubrific.num-docto) + "#" + STRING(mab-item-lubrific.cod-evento)) /*usar a # quando o cadastro do eqpto Ç feito 1,10,2,20 sendo que o correto seria fazer 01,10,02,20 */
        and   ttDados.sequencia     = 31 exclusive-lock no-error.
    /** Se n∆o estiver criado, cria registro **/
    /** Algumas Variaveis j† vem com o valor da procedure piCompartimentos **/
    if not avail ttDados then do:
        create ttDados.
        assign ttDados.cod-dimensao   = (cCodPai + "#" + c-cod-visao + "#" + string(mab-item-lubrific.num-docto) + "#" + STRING(mab-item-lubrific.cod-evento)) /** C¢digo encadeado para tree-view **/
               ttDados.desc-dimensao  = c-des-visao                /** Descriá∆o do c¢digo **/
               ttDados.sequencia      = 31                        /** Sequància dos dados **/
               ttDados.p-image        = 31                        /** N£mero da imagem do registro **/
               ttDados.r-rowid        = rRowid                    /** Rowid do registro para consultas futuras **/
               ttDados.cod-dimens-pai = cCodPai + "#" + c-cod-visao                   /** C¢digo do pai do registro **/
               ttDados.tipo           = c-tipo
               ttDados.uso-real       = d-uso-real
               ttDados.un             = cUN
               ttDados.garantia       = d-garantia
               ttDados.ung            = c-ung
               ttDados.atualizacao    = d-atualiza
               ttDados.ordem          = i-ordem
               ttDados.ep-codigo      = mab-eqpto.ep-codigo
               ttDados.cod-eqpto      = mab-eqpto.cod-eqpto
               ttDados.c-cod-sub-sist = cCodSubSist
               ttDados.val-uso-padr   = d-uso-padrao.

        IF  mab-item-lubrific.val-dat-hora-invrtda = 0 THEN
            ASSIGN ttDados.cod-oficial = "".
        ELSE
            ASSIGN ttDados.cod-oficial = (substring(string(mab-item-lubrific.val-dat-hora-invrtda, "999999999999"), 7, 2) + "/" + substring(string(mab-item-lubrific.val-dat-hora-invrtda, "999999999999"), 5, 2) + "/" + substring(string(mab-item-lubrific.val-dat-hora-invrtda, "999999999999"), 1, 4)).

        if ttDados.garantia > 0 then do:
            {utp/ut-liter.i Dia}
            IF ttDados.ung = RETURN-VALUE THEN DO:
                ASSIGN d-data-garantia = ttDados.atualiza + ttDados.garantia.
                IF d-data-garantia >= TODAY THEN DO:
                    /** Em garantia = Verde **/
                    assign ttDados.i-estado = 2.
                END.
                ELSE DO:
                    /** Vencido = Vermelho **/
                    /* para Usina Colombo, garantias vencidas devem entrar com o mesmo conceito
                    de "Sem Garantia", por isso o estado aqui est† igual a 1 e n∆o igual a 3 */
                    assign ttDados.i-estado = 1.
                END.
            END.
            ELSE DO:
                if ttDados.garantia >= ttDados.uso-real then do:
                    /** Em garantia = Verde **/
                    assign ttDados.i-estado = 2.
                end.
                else do:
                    /** Vencido = Vermelho **/
                    /* para Usina Colombo, garantias vencidas devem entrar com o mesmo conceito
                       de "Sem Garantia", por isso o estado aqui est† igual a 1 e n∆o igual a 3 */
                    assign ttDados.i-estado = 1.
                end.
            END.
        end.
        else do:
            assign ttDados.i-estado = 1.
        end.
    end.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piComponentes wWindow 
PROCEDURE piComponentes :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
define buffer bf-mab-model for mab-model.

for each  mco-compon
    where mco-compon.ep-codigo-atual = mab-eqpto.ep-codigo
    and   mco-compon.cod-eqpto-atual = mab-eqpto.cod-eqpto no-lock:
    for first mco-histor-vida
        where mco-histor-vida.num-seqcial = mco-compon.num-seqcial
        and   mco-histor-vida.num-vida    = mco-compon.num-vida-atual no-lock:
        for first mab-sub-sist
            where mab-sub-sist.cod-sub-sist = mco-compon.cod-sub-sist no-lock:
        end.
        assign d-garantia   = 0
               d-uso-real   = 0
               d-uso-padrao = 0.
        if not avail mab-sub-sist then next.
        assign c-cod-visao = mco-compon.cod-sub-sist   + " - " + mco-compon.cod-compon
               c-des-visao = mab-sub-sist.des-sub-sist + " - " + mco-compon.des-model.
        for first bf-mab-model
            where bf-mab-model.cod-model = mco-compon.cod-model no-lock:
        end.
        if not avail bf-mab-model then next.
        assign c-ung = bf-mab-model.un.
        if mco-histor-vida.val-km-hora-gartia <> 0 then do:
            assign d-garantia = mco-histor-vida.val-km-hora-gartia
                   c-ung = bf-mab-model.un.
        end.
        else do:
            if mco-histor-vida.num-dias-gartia <> 0 then do:
                {utp/ut-liter.i Dia}
                assign d-garantia = mco-histor-vida.num-dias-gartia
                       c-ung = return-value.
            end.
        end.
        {utp/ut-liter.i Componente *}
        assign c-tipo       = return-value
               d-uso-real   = dec(mco-histor-vida.val-uso-final) 
               i-ordem      = mco-histor-vida.nr-ord-produ
               cCodSubSist  = mco-compon.cod-sub-sist
               cUN          = bf-mab-model.un
               d-atualiza   = mco-histor-vida.dat-inicial
               d-uso-padrao = mco-histor-vida.val-uso-estimad.
        run piCarrega in this-procedure.
    end.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piComponentesHist wWindow 
PROCEDURE piComponentesHist :
/*------------------------------------------------------------------------------
  Purpose:     piComponentesHist
  Parameters:  <none>
  Notes:       Popula o hist¢rico do Componente
------------------------------------------------------------------------------*/
    DEFINE VARIABLE d-data-garantia AS DATE NO-UNDO.

    for each mco-histor-vida
        where mco-histor-vida.num-seqcial = mco-compon.num-seqcial no-lock:
        assign d-garantia   = 0
               d-uso-real   = 0
               d-uso-padrao = 0.
        if mco-histor-vida.val-km-hora-gartia <> 0 then do:
            assign d-garantia = mco-histor-vida.val-km-hora-gartia.
        end.
        else do:
            if mco-histor-vida.num-dias-gartia <> 0 then do:
                {utp/ut-liter.i Dia}
                assign d-garantia = mco-histor-vida.num-dias-gartia
                       c-ung = return-value.
            end.
        end.
        {utp/ut-liter.i Componente *}
        assign c-tipo       = return-value
               d-uso-real   = dec(mco-histor-vida.val-uso-final) 
               i-ordem      = mco-histor-vida.nr-ord-produ
               d-atualiza   = mco-histor-vida.dat-inicial
               d-uso-padrao = mco-histor-vida.val-uso-estimad.


        /** Busca registro **/
        find first ttDados
            where ttDados.cod-dimensao  = (cCodPai + "#" + c-cod-visao + "#" + string(mco-histor-vida.num-vida)) /*usar a # quando o cadastro do eqpto Ç feito 1,10,2,20 sendo que o correto seria fazer 01,10,02,20 */
            and   ttDados.sequencia     = 31 exclusive-lock no-error.
        /** Se n∆o estiver criado, cria registro **/
        /** Algumas Variaveis j† vem com o valor da procedure piComponentes **/
        if not avail ttDados then do:
            create ttDados.
            assign ttDados.cod-dimensao   = (cCodPai + "#" + c-cod-visao + "#" + string(mco-histor-vida.num-vida)) /** C¢digo encadeado para tree-view **/
                   /*ttDados.cod-oficial    = STRING(mco-histor-vida.dat-inicial,"99/99/9999")                   /** C¢digo Original do registro **/*/
                   ttDados.cod-oficial    = c-cod-visao
                   ttDados.desc-dimensao  = c-des-visao                /** Descriá∆o do c¢digo **/
                   ttDados.sequencia      = 31                        /** Sequància dos dados **/
                   ttDados.p-image        = 31                        /** N£mero da imagem do registro **/
                   ttDados.r-rowid        = rRowid                    /** Rowid do registro para consultas futuras **/
                   ttDados.cod-dimens-pai = cCodPai + "#" + c-cod-visao                   /** C¢digo do pai do registro **/
                   ttDados.tipo           = c-tipo
                   ttDados.uso-real       = d-uso-real
                   ttDados.un             = cUN
                   ttDados.garantia       = d-garantia
                   ttDados.ung            = c-ung
                   ttDados.atualizacao    = d-atualiza
                   ttDados.ordem          = i-ordem
                   ttDados.ep-codigo      = mab-eqpto.ep-codigo
                   ttDados.cod-eqpto      = mab-eqpto.cod-eqpto
                   ttDados.c-cod-sub-sist = cCodSubSist
                   ttDados.val-uso-padr   = d-uso-padrao.

            if ttDados.garantia > 0 then do:
                {utp/ut-liter.i Dia}
                IF ttDados.ung = RETURN-VALUE THEN DO:
                    ASSIGN d-data-garantia = ttDados.atualiza + ttDados.garantia.
                    IF d-data-garantia >= TODAY THEN DO:
                        /** Em garantia = Verde **/
                        assign ttDados.i-estado = 2.
                    END.
                    ELSE DO:
                        /** Vencido = Vermelho **/
                        /* para Usina Colombo, garantias vencidas devem entrar com o mesmo conceito
                        de "Sem Garantia", por isso o estado aqui est† igual a 1 e n∆o igual a 3 */
                        assign ttDados.i-estado = 1.
                    END.
                END.
                ELSE DO:
                    if ttDados.garantia >= ttDados.uso-real then do:
                        /** Em garantia = Verde **/
                        assign ttDados.i-estado = 2.
                    end.
                    else do:
                        /** Vencido = Vermelho **/
                        /* para Usina Colombo, garantias vencidas devem entrar com o mesmo conceito
                           de "Sem Garantia", por isso o estado aqui est† igual a 1 e n∆o igual a 3 */
                        assign ttDados.i-estado = 1.
                    end.
                END.
            end.
            else do:
                assign ttDados.i-estado = 1.
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

/** Guarda vari†veis para controle **/
assign c-eqpto-excel = string(tt-mab-eqpto.ep-codigo:screen-value in frame fPage0) + "-" + tt-mab-eqpto.cod-eqpto:screen-value in frame fPage0
       de-cont-excel = input frame fPage0 fi-contador
       c-km-eqpto    = fi-km-eqpto:screen-value in frame fPage0
       l-primeiro    = yes.

ASSIGN i-cod-image = 30.
FOR FIRST ttSelecao NO-LOCK:
    IF  ttSelecao.lDetalharPordata = YES THEN
        ASSIGN i-cod-image = 4.
END.

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
run colp/col0602f.w (output i-impressao,
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
        if i-col <> brDetalhe:num-columns then
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
    run abp/ab9003.p(input "col0602":U,
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

for each  ttDados
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
       do i-col = 1 to brDetalhe:num-columns in frame fPage0:
           /** Contador para linhas **/
           if fnBrowse(entry(i-col,c-colunas),1) <> "tipo":U then do:
               if i-lin = i-lin2 then do:
                   create tt-dados-ex.
                   assign tt-dados-ex.celula-coluna        = if i-col >= 10 then i-col - 1 else i-col
                          tt-dados-ex.celula-linha         = i-lin
                          tt-dados-ex.celula-cor-interior  = 10
                          tt-dados-ex.celula-fonte-cor     = 19
                          tt-dados-ex.celula-fonte-negrito = yes.
                   assign tt-dados-ex.celula-valor         = fnBrowse(entry(i-col,c-colunas),1).
               end.
               /** Impress∆o da primeira linha de dados **/
               create tt-dados-ex.
               assign tt-dados-ex.celula-coluna = if i-col >= 10 then i-col - 1 else i-col
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
           end.

       end. /** Colunas **/
       if ttDados.p-image = i-cod-image then
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
if l-primeiro then do:
    assign l-primeiro = no.
    if avail bfttDados2 and bfttDados2.p-image <> i-cod-image then do:
        /** Cria os dados na temp-table do excel **/
        create tt-dados-ex.
        assign tt-dados-ex.celula-coluna        = 1
               tt-dados-ex.celula-linha         = i-lin2
               tt-dados-ex.celula-cor-interior  = 27
               tt-dados-ex.celula-fonte-cor     = 1
               tt-dados-ex.celula-fonte-negrito = yes
               tt-dados-ex.celula-formato       = "@":U
               tt-dados-ex.celula-valor         = "Equipamento: " + c-eqpto-excel + " - " + fi-nom-eqpto:screen-value in frame fPage0 + " - " + "Cont: " + string(de-cont-excel) + " - " + "Real: " + c-km-eqpto
               i-lin2                           = i-lin2 + 1.
        /** Cria os dados na temp-table do excel **/
        {colp/col0602.i2 bfttDados2.p-image}
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
end.
else do:
    if avail bfttDados2 and bfttDados2.p-image <> i-cod-image then do:
        /** Cria os dados na temp-table do excel **/
       {colp/col0602.i2 bfttDados2.p-image}
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
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piFormataSegundoParaHora wWindow 
PROCEDURE piFormataSegundoParaHora :
/*------------------------------------------------------------------------------
  Purpose:     piFormataSegundoParaHora
  Parameters:  entrada pSegundo = Segundos
               sa°da   pHora    = Horas
  Notes:       Informar os segundos (time) para que seja retornado a hora
------------------------------------------------------------------------------*/
define input  parameter pSegundo  as integer   format ">>,>>>,>>9" no-undo.
define output parameter pHora     as character format "99:99:99"   no-undo.

assign pHora = replace(string(pSegundo,"hh:mm:ss":U),":","").

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
    run colp/col0602d.w (input table ttDados,
                         input iSequencia,
                         input cDimensao,
                         input iVisao,
                         INPUT ttSelecao.lDetalharPorData).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piNarrativaOM wWindow 
PROCEDURE piNarrativaOM :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
if avail ttDados then do:
    /*--- Seta cursor do mouse para espera ---*/
    assign {&window-name}:sensitive = no.
    /*--- Executa programa de consulta de OM ---*/
    RUN mvp/mv0607e.w (INPUT ttDados.ordem).
    assign {&window-name}:sensitive = yes.
END.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piPneus wWindow 
PROCEDURE piPneus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{utp/ut-liter.i Pneus}
assign c-tipo = return-value.
/** Busca os pneus do equipamento **/
for each  mpn-pneu
    where mpn-pneu.ep-codigo-atual = mab-eqpto.ep-codigo
    and   mpn-pneu.cod-eqpto-atual = mab-eqpto.cod-eqpto no-lock:
    for first mpn-histor-vida
        where mpn-histor-vida.cod-pneu = mpn-pneu.cod-pneu
        and   mpn-histor-vida.num-vida = mpn-pneu.num-vida-atual no-lock:
    end.
    if avail mpn-histor-vida then do:
        for first mpn-model FIELDS (un)
            WHERE mpn-model.cod-model-pneu = mpn-pneu.cod-model-pneu no-lock:
        end.
        if not avail mpn-model then next.
        for first mpn-desen
            where mpn-desen.cod-desen = mpn-histor-vida.cod-desen no-lock:
        end.
        if avail mpn-desen then do:
            assign c-des-visao = mpn-desen.des-desen.
        end.
        else do:
            for first mpn-desen
                where mpn-desen.cod-desen = mpn-model.cod-desen no-lock:
            end.
            if avail mpn-desen then
                assign c-des-visao = mpn-desen.des-desen.
            else
                assign c-des-visao = "".
        end.
        {utp/ut-liter.i Pneu}
        assign d-garantia = mpn-histor-vida.val-km-hora-gartia
               c-ung = mpn-model.un
               c-cod-visao = mpn-pneu.cod-pneu
               d-uso-real  = mpn-histor-vida.val-uso-final
               d-garantia  = mpn-histor-vida.val-km-hora-gartia
               c-ung  = mpn-model.un
               i-ordem = 0
               c-tipo = return-value
               cCodSubSist = "":U
               cUN = mpn-model.un
               d-atualiza = mpn-histor-vida.dat-inicial
               d-uso-padrao = 0.

        IF  mpn-histor-vida.num-livre-1 <> 0 AND d-garantia = 0 THEN DO:
            {utp/ut-liter.i Dia}
            assign d-garantia = dec(mpn-histor-vida.num-livre-1)
                   c-ung = return-value.
        END.

        run piCarrega in this-procedure.
    end.
end.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piPneusHistorico wWindow 
PROCEDURE piPneusHistorico :
/*------------------------------------------------------------------------------
  Purpose:     piPneusHistorico
  Parameters:  <none>
  Notes:       Popula o hist¢rico do Pneu
------------------------------------------------------------------------------*/
    DEFINE VARIABLE d-data-garantia AS DATE NO-UNDO.
    
    FOR EACH  mpn-histor-vida
        WHERE mpn-histor-vida.cod-pneu = mpn-pneu.cod-pneu no-lock:
    
        for first mpn-desen
            where mpn-desen.cod-desen = mpn-histor-vida.cod-desen no-lock:
        end.
        if avail mpn-desen then do:
            assign c-des-visao = mpn-desen.des-desen.
        end.
        else do:
            for first mpn-desen
                where mpn-desen.cod-desen = mpn-model.cod-desen no-lock:
            end.
            if avail mpn-desen then
                assign c-des-visao = mpn-desen.des-desen.
            else
                assign c-des-visao = "".
        end.
        {utp/ut-liter.i Pneu}
        assign d-garantia = mpn-histor-vida.val-km-hora-gartia
               d-uso-real = mpn-histor-vida.val-uso-final
               d-garantia = mpn-histor-vida.val-km-hora-gartia
               c-tipo = return-value
               d-atualiza = mpn-histor-vida.dat-inicial.
    
        IF  mpn-histor-vida.num-livre-1 <> 0 AND d-garantia = 0 THEN DO:
            {utp/ut-liter.i Dia}
            assign d-garantia = dec(mpn-histor-vida.num-livre-1)
                   c-ung = return-value.
        END.
    
        /** Busca registro **/
        find first ttDados
            where ttDados.cod-dimensao  = (cCodPai + "#" + c-cod-visao + "#" + string(mpn-histor-vida.num-vida)) /*usar a # quando o cadastro do eqpto Ç feito 1,10,2,20 sendo que o correto seria fazer 01,10,02,20 */
            and   ttDados.sequencia     = 31 exclusive-lock no-error.
        /** Se n∆o estiver criado, cria registro **/
        /** Algumas Variaveis j† vem com o valor da procedure piPneus **/
        if not avail ttDados then do:
            create ttDados.
            assign ttDados.cod-dimensao   = (cCodPai + "#" + c-cod-visao + "#" + string(mpn-histor-vida.num-vida)) /** C¢digo encadeado para tree-view **/
                   ttDados.cod-oficial    = STRING(mpn-histor-vida.dat-inicial,"99/99/9999")                   /** C¢digo Original do registro **/
                   ttDados.desc-dimensao  = c-des-visao                /** Descriá∆o do c¢digo **/
                   ttDados.sequencia      = 31                        /** Sequància dos dados **/
                   ttDados.p-image        = 31                        /** N£mero da imagem do registro **/
                   ttDados.r-rowid        = rRowid                    /** Rowid do registro para consultas futuras **/
                   ttDados.cod-dimens-pai = cCodPai + "#" + c-cod-visao                   /** C¢digo do pai do registro **/
                   ttDados.tipo           = c-tipo
                   ttDados.uso-real       = d-uso-real
                   ttDados.un             = cUN
                   ttDados.garantia       = d-garantia
                   ttDados.ung            = c-ung
                   ttDados.atualizacao    = d-atualiza
                   ttDados.ordem          = i-ordem
                   ttDados.ep-codigo      = mab-eqpto.ep-codigo
                   ttDados.cod-eqpto      = mab-eqpto.cod-eqpto
                   ttDados.c-cod-sub-sist = cCodSubSist
                   ttDados.val-uso-padr   = d-uso-padrao.

            if ttDados.garantia > 0 then do:
                {utp/ut-liter.i Dia}
                IF ttDados.ung = RETURN-VALUE THEN DO:
                    ASSIGN d-data-garantia = ttDados.atualiza + ttDados.garantia.
                    IF d-data-garantia >= TODAY THEN DO:
                        /** Em garantia = Verde **/
                        assign ttDados.i-estado = 2.
                    END.
                    ELSE DO:
                        /** Vencido = Vermelho **/
                        /* para Usina Colombo, garantias vencidas devem entrar com o mesmo conceito
                        de "Sem Garantia", por isso o estado aqui est† igual a 1 e n∆o igual a 3 */
                        assign ttDados.i-estado = 1.
                    END.
                END.
                ELSE DO:
                    if ttDados.garantia >= ttDados.uso-real then do:
                        /** Em garantia = Verde **/
                        assign ttDados.i-estado = 2.
                    end.
                    else do:
                        /** Vencido = Vermelho **/
                        /* para Usina Colombo, garantias vencidas devem entrar com o mesmo conceito
                           de "Sem Garantia", por isso o estado aqui est† igual a 1 e n∆o igual a 3 */
                        assign ttDados.i-estado = 1.
                    end.
                END.
            end.
            else do:
                assign ttDados.i-estado = 1.
            end.
        end.
    END.
    
    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piSubSistemas wWindow 
PROCEDURE piSubSistemas :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
define variable pData            as date      format "99/99/9999" no-undo.
define variable pHora            as character format "999999"     no-undo.
define variable d-km-atual       as decimal                       no-undo.
define variable d-km-dt          as decimal                       no-undo.
define variable deInvertAtualiza as decimal                       no-undo.
define variable s-garantia       as decimal                       no-undo.

for each  mab-relac-eqpto-sub-sist
    where mab-relac-eqpto-sub-sist.ep-codigo = mab-eqpto.ep-codigo
    and   mab-relac-eqpto-sub-sist.cod-eqpto = mab-eqpto.cod-eqpto no-lock:

    for first mab-sub-sist
        where mab-sub-sist.cod-sub-sist = mab-relac-eqpto-sub-sist.cod-sub-sist no-lock:
    end.
    if not avail mab-sub-sist then next.
    /** Parametro para mostrar componentes nao instalados como sub-sistema **/
    if mab-sub-sist.idi-compon = 2 and not ttSelecao.lMostraCompon then next.

    {utp/ut-liter.i Componente *}
    if can-find (first ttDados
                 where ttDados.tipo           = return-value
                 and   ttDados.c-cod-sub-sist = mab-relac-eqpto-sub-sist.cod-sub-sist
                 and   ttDados.p-image        = 30 no-lock) then next.
    {utp/ut-liter.i Sub-Sistema *}
    assign c-cod-visao = mab-relac-eqpto-sub-sist.cod-sub-sist
           cCodSubSist = mab-relac-eqpto-sub-sist.cod-sub-sist
           c-des-visao = mab-sub-sist.des-sub-sist
           cUN        = mab-model.un
           d-garantia = 0
           d-atualiza = mab-relac-eqpto-sub-sist.dat-ult-troca
           i-ordem    = 0
           c-ung = mab-model.un.
    if mab-relac-eqpto-sub-sist.val-km-hora-gartia <> 0 then do:
        assign d-garantia = mab-relac-eqpto-sub-sist.val-km-hora-gartia.
    end.
    else do:
        if mab-relac-eqpto-sub-sist.num-dias-gartia <> 0 then do:
            {utp/ut-liter.i Dia}
            assign d-garantia = mab-relac-eqpto-sub-sist.num-dias-gartia
                   c-ung = return-value.
        end.
    end.
    for first mmv-param
        where mmv-param.ep-codigo = mab-eqpto.ep-codigo no-lock:
    end.
    for last  mmv-tar-ord-manut use-index mmvtrrdm-02
        where mmv-tar-ord-manut.ep-codigo    = mab-eqpto.ep-codigo
        and   mmv-tar-ord-manut.cod-eqpto    = mab-eqpto.cod-eqpto
        and   mmv-tar-ord-manut.cod-evento   = mmv-param.cod-event-troca
        and   mmv-tar-ord-manut.cod-sub-sist = mab-relac-eqpto-sub-sist.cod-sub-sist
        AND   mmv-tar-ord-manut.val-dat-invrtda-term > 0 no-lock:
    end.
    if avail mmv-tar-ord-manut then do:
        if mmv-tar-ord-manut.val-km-hora-gartia <> 0 and 
           mmv-tar-ord-manut.val-km-hora-gartia <> ? then do:
            assign d-garantia = mmv-tar-ord-manut.val-km-hora-gartia.
        end.
        else do:
            if mmv-tar-ord-manut.num-dias-gartia <> 0 and 
               mmv-tar-ord-manut.num-dias-gartia <> ? then do:
                {utp/ut-liter.i Dia}
                assign d-garantia = mmv-tar-ord-manut.num-dias-gartia
                       c-ung = RETURN-VALUE
                       s-garantia = dec(today - d-atualiza + 1).
            END.
        end.
        for first mmv-ord-manut
            where mmv-ord-manut.nr-ord-produ = mmv-tar-ord-manut.nr-ord-produ no-lock:
        end.
        if avail mmv-ord-manut then do:
            if mmv-tar-ord-manut.val-dat-invrtda-term <> 0 then do:
                run converteInvertidaParaNormal (input  mmv-tar-ord-manut.val-dat-invrtda-term,
                                                 output pData,
                                                 output pHora).
            END.
            if pData <> ? then do:
                assign d-atualiza = pData.
            END.
            ELSE DO:
                if mmv-ord-manut.dat-term <> ? then do:
                    assign d-atualiza = mmv-ord-manut.dat-term.
                END.
            end.
        end.
        assign i-ordem = mmv-tar-ord-manut.nr-ord-produ.
    end.

    /** Busca £ltima quilometragem do equipamento **/
    run piCalculaKM in this-procedure (input  mab-eqpto.ep-codigo,
                                       input  mab-eqpto.cod-eqpto,
                                       input  deInvertidaAtual,
                                       input  1,
                                       output d-km-atual).

    run converteParaHoraInvertida (input  string(d-atualiza,'99/99/9999'),
                                   input  "000000",
                                   output deInvertAtualiza).

    /** Busca £ltima quilometragem do equipamento **/
    run piCalculaKM in this-procedure (input  mab-eqpto.ep-codigo,
                                       input  mab-eqpto.cod-eqpto,
                                       input  deInvertAtualiza,
                                       input  1,
                                       output d-km-dt).

    IF mab-eqpto.val-km-inicial = d-km-dt THEN DO:
        ASSIGN d-km-dt = mab-relac-eqpto-sub-sist.val-km-inicial.
    END.

    {utp/ut-liter.i Sub-Sistema}
    assign cCodSubSist = mab-relac-eqpto-sub-sist.cod-sub-sist
           d-uso-real  = d-km-atual - d-km-dt
           c-tipo      = return-value
           d-uso-padrao = 0.

    run piCarrega in this-procedure.
end.

return "OK":U.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piSubSistemasHistorico wWindow 
PROCEDURE piSubSistemasHistorico :
/*------------------------------------------------------------------------------
  Purpose:     piSubSistemasHistorico
  Parameters:  <none>
  Notes:       Popula o hist¢rico do SubSistemas
------------------------------------------------------------------------------*/
define variable pData            as date      format "99/99/9999" no-undo.
define variable pHora            as character format "999999"     no-undo.
DEFINE VARIABLE s-garantia       AS DECIMAL                       NO-UNDO.
DEFINE VARIABLE d-data-garantia  AS DATE                          NO-UNDO.
DEFINE VARIABLE deInvertAtualiza AS DECIMAL                       NO-UNDO.
DEFINE VARIABLE d-km-atual       AS DECIMAL                       NO-UNDO.
DEFINE VARIABLE d-km-dt          AS DECIMAL                       NO-UNDO.

assign deInvertAtualiza = deInvertidaAtual.

for EACH  mmv-tar-ord-manut use-index mmvtrrdm-02
    where mmv-tar-ord-manut.ep-codigo    = mab-eqpto.ep-codigo
    and   mmv-tar-ord-manut.cod-eqpto    = mab-eqpto.cod-eqpto
    and   mmv-tar-ord-manut.cod-evento   = mmv-param.cod-event-troca
    and   mmv-tar-ord-manut.cod-sub-sist = mab-relac-eqpto-sub-sist.cod-sub-sist
    AND   mmv-tar-ord-manut.estado       = 4 no-lock
    by    mmv-tar-ord-manut.val-dat-invrtda-term desc:
    if mmv-tar-ord-manut.val-km-hora-gartia <> 0 and 
       mmv-tar-ord-manut.val-km-hora-gartia <> ? then do:
        assign d-garantia = mmv-tar-ord-manut.val-km-hora-gartia
               c-ung = mab-model.un.
    END.
    else do:
        if mmv-tar-ord-manut.num-dias-gartia <> 0 and 
           mmv-tar-ord-manut.num-dias-gartia <> ? then do:
            {utp/ut-liter.i Dia}
            assign d-garantia = mmv-tar-ord-manut.num-dias-gartia
                   c-ung      = RETURN-VALUE
                   s-garantia = dec(today - d-atualiza + 1).
        END.
    END.
    for first mmv-ord-manut
        where mmv-ord-manut.nr-ord-produ = mmv-tar-ord-manut.nr-ord-produ no-lock:
        if mmv-ord-manut.dat-term <> ? then do:
            assign d-atualiza = mmv-ord-manut.dat-term.
        END.
    END.
    IF NOT AVAIL mmv-ord-manut THEN DO:
        if mmv-tar-ord-manut.val-dat-invrtda-term <> 0 then do:
            run converteInvertidaParaNormal (input  mmv-tar-ord-manut.val-dat-invrtda-term,
                                             output pData,
                                             output pHora).
        END.
        if pData <> ? then do:
            assign d-atualiza = pData.
        END.
    END.
    
    ASSIGN i-ordem = mmv-tar-ord-manut.nr-ord-produ.
    
    /** Busca £ltima quilometragem do equipamento **/
    run piCalculaKM in this-procedure (input  mab-eqpto.ep-codigo,
                                       input  mab-eqpto.cod-eqpto,
                                       input  deInvertAtualiza,
                                       input  1,
                                       output d-km-atual).
    
    run converteParaHoraInvertida (input  string(d-atualiza,'99/99/9999'),
                                   input  "000000",
                                   output deInvertAtualiza).
    
    /** Busca £ltima quilometragem do equipamento **/
    run piCalculaKM in this-procedure (input  mab-eqpto.ep-codigo,
                                       input  mab-eqpto.cod-eqpto,
                                       input  deInvertAtualiza,
                                       input  1,
                                       output d-km-dt).
    
    IF  mab-eqpto.val-km-inicial = d-km-dt THEN DO:
        ASSIGN d-km-dt = mab-relac-eqpto-sub-sist.val-km-inicial.
    END.
    
    ASSIGN d-uso-real    = dec(d-km-atual - d-km-dt)
           d-uso-padrao  = 0.
    
    /** Busca registro **/
    find first ttDados
        where ttDados.cod-dimensao  = (cCodPai + "#" + c-cod-visao + "#" + string(mmv-tar-ord-manut.nr-ord-prod) + "#" + STRING(mmv-tar-ord-manut.num-seq)) /*usar a # quando o cadastro do eqpto Ç feito 1,10,2,20 sendo que o correto seria fazer 01,10,02,20 */
        and   ttDados.sequencia     = 31 exclusive-lock no-error.
    /** Se n∆o estiver criado, cria registro **/
    /** Algumas Variaveis j† vem com o valor da procedure piSubSistemas **/
    if not avail ttDados then do:
        create ttDados.
        assign ttDados.cod-dimensao   = (cCodPai + "#" + c-cod-visao + "#" + string(mmv-tar-ord-manut.nr-ord-prod) + "#" + STRING(mmv-tar-ord-manut.num-seq)) /** C¢digo encadeado para tree-view **/
               ttDados.desc-dimensao  = c-des-visao                /** Descriá∆o do c¢digo **/
               ttDados.sequencia      = 31                        /** Sequància dos dados **/
               ttDados.p-image        = 31                        /** N£mero da imagem do registro **/
               ttDados.r-rowid        = rRowid                    /** Rowid do registro para consultas futuras **/
               ttDados.cod-dimens-pai = cCodPai + "#" + c-cod-visao                   /** C¢digo do pai do registro **/
               ttDados.tipo           = c-tipo
               ttDados.uso-real       = d-uso-real
               ttDados.un             = cUN
               ttDados.garantia       = d-garantia
               ttDados.ung            = c-ung
               ttDados.atualizacao    = d-atualiza
               ttDados.ordem          = i-ordem
               ttDados.ep-codigo      = mab-eqpto.ep-codigo
               ttDados.cod-eqpto      = mab-eqpto.cod-eqpto
               ttDados.c-cod-sub-sist = cCodSubSist
               ttDados.val-uso-padr   = d-uso-padrao
               ttDados.cod-oficial = c-cod-visao.
    
        if ttDados.garantia > 0 then do:
            {utp/ut-liter.i Dia}
            IF ttDados.ung = RETURN-VALUE THEN DO:
                ASSIGN d-data-garantia = ttDados.atualiza + ttDados.garantia.
                IF d-data-garantia >= TODAY THEN DO:
                    /** Em garantia = Verde **/
                    assign ttDados.i-estado = 2.
                END.
                ELSE DO:
                    /** Vencido = Vermelho **/
                    /* para Usina Colombo, garantias vencidas devem entrar com o mesmo conceito
                    de "Sem Garantia", por isso o estado aqui est† igual a 1 e n∆o igual a 3 */
                    assign ttDados.i-estado = 1.
                END.
            END.
            ELSE DO:
                if ttDados.garantia >= ttDados.uso-real then do:
                    /** Em garantia = Verde **/
                    assign ttDados.i-estado = 2.
                END.
                else do:
                    /** Vencido = Vermelho **/
                    /* para Usina Colombo, garantias vencidas devem entrar com o mesmo conceito
                    de "Sem Garantia", por isso o estado aqui est† igual a 1 e n∆o igual a 3 */
                    assign ttDados.i-estado = 1.
                END.
            END.
        END.
        else do:
            assign ttDados.i-estado = 1.
        END.
    END.
END.
for FIRST mmv-tar-ord-manut use-index mmvtrrdm-02
    where mmv-tar-ord-manut.ep-codigo    = mab-eqpto.ep-codigo
    and   mmv-tar-ord-manut.cod-eqpto    = mab-eqpto.cod-eqpto
    and   mmv-tar-ord-manut.cod-evento   = mmv-param.cod-event-troca
    and   mmv-tar-ord-manut.cod-sub-sist = mab-relac-eqpto-sub-sist.cod-sub-sist
    AND   mmv-tar-ord-manut.estado       = 4 no-lock:

    assign d-garantia = 0
           d-atualiza = mab-relac-eqpto-sub-sist.dat-ult-troca
           i-ordem    = 0.
    if mab-relac-eqpto-sub-sist.val-km-hora-gartia <> 0 then do:
        assign d-garantia = mab-relac-eqpto-sub-sist.val-km-hora-gartia
               c-ung = mab-model.un.
    end.
    else do:
        if mab-relac-eqpto-sub-sist.num-dias-gartia <> 0 then do:
            {utp/ut-liter.i Dia}
            assign d-garantia = mab-relac-eqpto-sub-sist.num-dias-gartia
                   c-ung = return-value
                   s-garantia = dec(today - d-atualiza + 1).
        end.
    end.

    /** Busca £ltima quilometragem do equipamento **/
    run piCalculaKM in this-procedure (input  mab-eqpto.ep-codigo,
                                       input  mab-eqpto.cod-eqpto,
                                       input  deInvertAtualiza,
                                       input  1,
                                       output d-km-atual).

    run converteParaHoraInvertida (input  string(d-atualiza,'99/99/9999'),
                                   input  "000000",
                                   output deInvertAtualiza).

    /** Busca £ltima quilometragem do equipamento **/
    run piCalculaKM in this-procedure (input  mab-eqpto.ep-codigo,
                                       input  mab-eqpto.cod-eqpto,
                                       input  deInvertAtualiza,
                                       input  1,
                                       output d-km-dt).

    IF mab-eqpto.val-km-inicial = d-km-dt THEN DO:
        ASSIGN d-km-dt = mab-relac-eqpto-sub-sist.val-km-inicial.
    END.

    assign d-uso-real  = d-km-atual - d-km-dt
           d-uso-padrao = 0.
    /** Busca registro **/
    find first ttDados
        where ttDados.cod-dimensao  = (cCodPai + "#" + c-cod-visao + "#0#0") /*usar a # quando o cadastro do eqpto Ç feito 1,10,2,20 sendo que o correto seria fazer 01,10,02,20 */
        and   ttDados.sequencia     = 31 exclusive-lock no-error.
    /** Se n∆o estiver criado, cria registro **/
    /** Algumas Variaveis j† vem com o valor da procedure piSubSistemas **/
    if not avail ttDados then do:
        create ttDados.
        assign ttDados.cod-dimensao   = (cCodPai + "#" + c-cod-visao + "#0#0") /** C¢digo encadeado para tree-view **/
               ttDados.desc-dimensao  = c-des-visao                /** Descriá∆o do c¢digo **/
               ttDados.sequencia      = 31                        /** Sequància dos dados **/
               ttDados.p-image        = 31                        /** N£mero da imagem do registro **/
               ttDados.r-rowid        = rRowid                    /** Rowid do registro para consultas futuras **/
               ttDados.cod-dimens-pai = cCodPai + "#" + c-cod-visao                   /** C¢digo do pai do registro **/
               ttDados.tipo           = c-tipo
               ttDados.uso-real       = d-uso-real
               ttDados.un             = cUN
               ttDados.garantia       = d-garantia
               ttDados.ung            = c-ung
               ttDados.atualizacao    = d-atualiza
               ttDados.ordem          = i-ordem
               ttDados.ep-codigo      = mab-eqpto.ep-codigo
               ttDados.cod-eqpto      = mab-eqpto.cod-eqpto
               ttDados.c-cod-sub-sist = cCodSubSist
               ttDados.val-uso-padr   = d-uso-padrao
               ttDados.cod-oficial = c-cod-visao.
    
        if ttDados.garantia > 0 then do:
            {utp/ut-liter.i Dia}
            IF ttDados.ung = RETURN-VALUE THEN DO:
                ASSIGN d-data-garantia = ttDados.atualiza + ttDados.garantia.
                IF d-data-garantia >= TODAY THEN DO:
                    /** Em garantia = Verde **/
                    assign ttDados.i-estado = 2.
                END.
                ELSE DO:
                    /** Vencido = Vermelho **/
                    /* para Usina Colombo, garantias vencidas devem entrar com o mesmo conceito
                    de "Sem Garantia", por isso o estado aqui est† igual a 1 e n∆o igual a 3 */
                    assign ttDados.i-estado = 1.
                END.
            END.
            ELSE DO:
                if ttDados.garantia >= ttDados.uso-real then do:
                    /** Em garantia = Verde **/
                    assign ttDados.i-estado = 2.
                END.
                else do:
                    /** Vencido = Vermelho **/
                    /* para Usina Colombo, garantias vencidas devem entrar com o mesmo conceito
                    de "Sem Garantia", por isso o estado aqui est† igual a 1 e n∆o igual a 3 */
                    assign ttDados.i-estado = 1.
                END.
            END.
        END.
        else do:
            assign ttDados.i-estado = 1.
        END.
    END.
END.
    
RETURN "OK":U.

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
run colp/col0602g.w (input-output cCaminho,
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
    assign cCaminho = replace(cCaminho, "/":U, "/":U).
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
DEFINE VARIABLE dt-atualiz    as character format "x(10)" NO-UNDO.
define variable c-des-eqpto-txt as character format "x(100)" no-undo.

assign cPai = "".

for each  ttDados use-index id
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

        if ttDados.atualizacao <> ? then
            assign dt-atualiz = string(ttDados.atualizacao,"99/99/9999") .
        else
            assign dt-atualiz = "":U.

        do with frame fPage0:
            assign c-des-eqpto-txt = string(tt-mab-eqpto.ep-codigo:screen-value) + "-" + tt-mab-eqpto.cod-eqpto:screen-value + " - " + fi-nom-eqpto:screen-value + " - " + string(INPUT FRAME fPage0 fi-contador) + " - " + string(fi-km-eqpto:screen-value).
        end.

        assign c-tam = "x(" + string(length(trim(trim(c-des-eqpto-txt)                       + cDelimitador +
                                                 trim(cDescPai)                              +
                                                 string(ttDados.uso-real,">>,>>>,>>9.9")     + cDelimitador +
                                                 string(ttDados.val-uso-padr,">>,>>>,>>9.9") + cDelimitador +
                                                 ttDados.un                                  + cDelimitador +
                                                 string(ttDados.garantia,">,>>>,>>9.9")      + cDelimitador +
                                                 ttDados.ung                                 + cDelimitador +
                                                 string(ttDados.atualizacao,"99/99/9999")    + cDelimitador +
                                                 string(ttDados.ordem,">>>,>>>,>>9")         + cDelimitador +
                                                 ttDados.tipo                                + cDelimitador +
                                                 trim(fnEstado(ttDados.i-estado)))))         + ")".


        /* Exporta dados para TXT */
        put trim(trim(c-des-eqpto-txt)                       + cDelimitador +
                 trim(cDescPai)                              +
                 string(ttDados.uso-real,">>,>>>,>>9.9")     + cDelimitador +
                 string(ttDados.val-uso-padr,">>,>>>,>>9.9") + cDelimitador +
                 ttDados.un                                  + cDelimitador +
                 string(ttDados.garantia,">,>>>,>>9.9")      + cDelimitador +
                 ttDados.ung                                 + cDelimitador +
                 string(ttDados.atualizacao,"99/99/9999")    + cDelimitador +
                 string(ttDados.ordem,">>>,>>>,>>9")         + cDelimitador +
                 ttDados.tipo                                + cDelimitador +
                 trim(fnEstado(ttDados.i-estado))) format c-tam at 01.
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
    {colp/col0602.i2 bfttDados2.p-image}
    assign cTituloTxt = cTituloTxt + TRIM(RETURN-VALUE) + cDelimitador.
end.
if avail bfttDados2 and bfttDados2.p-image <> 30 and bfttDados2.p-image <> 31 then do:
    assign cDescPai  = trim(trim(cDescPai) + trim(bfttDados2.cod-oficial) + "-" + TRIM(bfttDados2.desc-dimensao) + cDelimitador).
    /** Se for o pen£ltimo n°vel, imprime cabeáalho se parametrizado **/
    if bfttDados2.sequencia = iVisao and lCabecalho then do:
        assign lCabecalho = no.
        assign c-tam = "x(" + string(length(fnBrowse("equipamento":U,1) + cDelimitador +   
                                     trim(cTituloTxt)                +                     
                                     brDetalhe:TITLE IN FRAME fPage0 + cDelimitador +      
                                     fnBrowse("uso-real":U,1)        + cDelimitador +      
                                     fnBrowse("val-uso-padr":U,1)    + cDelimitador +      
                                     fnBrowse("un":U,1)              + cDelimitador +      
                                     fnBrowse("garantia":U,1)        + cDelimitador +      
                                     fnBrowse("ung":U,1)             + cDelimitador +      
                                     fnBrowse("atualizacao":U,1)     + cDelimitador +      
                                     fnBrowse("ordem":U,1)           + cDelimitador +      
                                     fnBrowse("tipo":U,1)            + cDelimitador +
                                     fnBrowse("c-estado":U,1)))      + ")".

        put trim(fnBrowse("equipamento":U,1)     + cDelimitador +
                 trim(cTituloTxt)                + 
                 brDetalhe:TITLE IN FRAME fPage0 + cDelimitador +
                 fnBrowse("uso-real":U,1)        + cDelimitador +
                 fnBrowse("val-uso-padr":U,1)    + cDelimitador +
                 fnBrowse("un":U,1)              + cDelimitador +
                 fnBrowse("garantia":U,1)        + cDelimitador +
                 fnBrowse("ung":U,1)             + cDelimitador +
                 fnBrowse("atualizacao":U,1)     + cDelimitador +
                 fnBrowse("ordem":U,1)           + cDelimitador +
                 fnBrowse("tipo":U,1)            + cDelimitador +
                 fnBrowse("c-estado":U,1)) format c-tam at 01.
    end.
end.

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
        when "uso-real":U then do:
            {utp/ut-liter.i "Uso Real"}
        end.
        WHEN "val-uso-padr":U THEN DO:
            {utp/ut-liter.i "Uso Padr∆o"}
        END.
        when "un":U then do:
            {utp/ut-liter.i "UN"}
        end.
        when "garantia":U then do:
            {utp/ut-liter.i "Garantia"}
        end.
        when "ung":U then do:
            {utp/ut-liter.i "UN"}
        end.
        when "atualizacao":U then do:
            {utp/ut-liter.i "Atualizaá∆o"}
        end.
        when "ordem":U then do:
            {utp/ut-liter.i "Ordem"}
        end.
        when "tipo":U then do:
            {utp/ut-liter.i "Tipo"}
        end.
        when "c-estado":U then do:
            {utp/ut-liter.i "Estado"}
        end.
        when "equipamento":U then do:
            {utp/ut-liter.i "Equipamento"}
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
        WHEN "val-uso-padr" THEN DO:
            ASSIGN cRetorno = string(ttDados.val-uso-padr,">>,>>>,>>9.9").
        END.
        when "un":U then do:
            assign cRetorno = ttDados.un.
        end.
        when "ung":U then do:
            assign cRetorno = ttDados.ung.
        end.
        when "garantia":U then do:
            assign cRetorno = string(ttDados.garantia,">,>>>,>>9.9").
        end.
        when "atualizacao":U then do:
            assign cRetorno = string(ttDados.atualizacao,"99/99/9999").
        end.
        when "ordem":U then do:
            assign cRetorno = string(ttDados.ordem,">>>,>>>,>>9").
        end.
        when "tipo":U then do:
            assign cRetorno = ttDados.tipo.
        end.
        when "c-estado":U then do:
            assign cRetorno = fnEstado(ttDados.i-estado).
        end.
        when "equipamento":U then do:
            assign cRetorno = ttDados.c-des-eqpto.
        end.
    end case.
end.

  RETURN cRetorno.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnEstado wWindow 
FUNCTION fnEstado RETURNS CHARACTER
  ( iEstado as integer ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
define variable cRetorno as character no-undo.

case iEstado:
    when 1 then do:
        {utp/ut-liter.i "Sem Garantia"}
    end.
    when 2 then do:
        {utp/ut-liter.i "Em Garantia"}
    end.
    when 3 then do:
        {utp/ut-liter.i "Garantia Vencida"}
    end.
end case.

assign cRetorno = return-value.

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
    {colp/col0602.i2 pImage}

    assign cRetorno = trim(return-value).

  RETURN cRetorno.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


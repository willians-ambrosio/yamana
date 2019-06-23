&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME wReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wReport 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESIM0665 2.00.00.016}  /*** 010016 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i esim0665 MIM}
&ENDIF

/********************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

CREATE WIDGET-POOL.

{include/i_dbvers.i} /*Include de Pr‚-Processadores*/

&IF '{&mgdis_version}' < '2.06f' &THEN
    /*Tecnica para implementar valida‡Æo produto Estendido*/
    {cdp/cdapi558.i &ProdutoEstendido=20 &ModuloEstendido=MIM}
    {cdp/cdapi558.i1}
&endif

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESIM0665
&GLOBAL-DEFINE Version        2.00.00.016

&GLOBAL-DEFINE Folder         YES
&GLOBAL-DEFINE InitialPage    1
&GLOBAL-DEFINE FolderLabels   Sele‡Æo,Classifica‡Æo,Parƒmetro,ImpressÆo

&GLOBAL-DEFINE PGLAY          NO
&GLOBAL-DEFINE PGSEL          YES
&GLOBAL-DEFINE PGCLA          YES
&GLOBAL-DEFINE PGPAR          YES
&GLOBAL-DEFINE PGDIG          NO
&GLOBAL-DEFINE PGIMP          YES
&GLOBAL-DEFINE PGLOG          NO

&GLOBAL-DEFINE page0Widgets   btOk ~
                              btCancel ~
                              btHelp2
&GLOBAL-DEFINE page2Widgets   
&GLOBAL-DEFINE page3Widgets   rsClassif
&GLOBAL-DEFINE page4Widgets   rsCustoItem ~
                              rsDespImpos
&GLOBAL-DEFINE page6Widgets   rsDestiny ~
                              btConfigImpr ~
                              btFile ~
                              rsExecution

&GLOBAL-DEFINE page6Text      text-destino text-modo


&GLOBAL-DEFINE page2Fields    cIniEstabel ~
                              cEndEstabel ~
                              cIniEmb ~
                              cEndEmb ~
                              cIniItem ~
                              cEndItem ~
                              iIniGrEst ~
                              iEndGrEst ~
                              cIniFam ~
                              cEndFam ~
                              iIniFornExp ~
                              iEndFornExp ~
                              cIniProc ~
                              cEndProc ~
                              iIniItin ~
                              iEndItin ~
                              daIniDtNac ~
                              daEndDtNac
&GLOBAL-DEFINE page3Fields    
&GLOBAL-DEFINE page4Fields    lDtNac ~
                              lOrcReal ~
                              lDespImp ~
                              lItens ~
                              lDesembParc ~
                              lNfCompl ~
                              iFiMoeda ~
                              daDtConv
&GLOBAL-DEFINE page6Fields    cFile

/* Parameters Definitions ---                                           */

{imp/esim0665.i}                                  

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-arq-layout       as char    no-undo.      
def var c-arq-temp         as char    no-undo.

def stream s-imp.
def var h-boun005 as handle no-undo.

/* Variÿveis para execu»’o de zooms */
def new global shared var adm-broker-hdl as handle no-undo.
def new global shared var gr-simula-item as rowid  no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rtToolBar btOK btCancel btHelp2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wReport AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp2 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "Executar" 
     SIZE 10 BY 1.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE cEndEmb AS CHARACTER FORMAT "X(12)":U INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13.86 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE cEndEstabel LIKE estabelec.cod-estabel
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE cEndFam AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 9.86 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE cEndItem AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17.86 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE cEndProc AS CHARACTER FORMAT "X(12)":U INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13.86 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE cIniEmb AS CHARACTER FORMAT "X(12)":U 
     LABEL "Embarque" 
     VIEW-AS FILL-IN 
     SIZE 13.86 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE cIniEstabel LIKE estabelec.cod-estabel
     LABEL "Estabelecimento":R7 
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE cIniFam AS CHARACTER FORMAT "X(8)":U 
     LABEL "Fam¡lia" 
     VIEW-AS FILL-IN 
     SIZE 9.86 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE cIniItem AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 17.86 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE cIniProc AS CHARACTER FORMAT "X(12)":U 
     LABEL "Processo" 
     VIEW-AS FILL-IN 
     SIZE 13.86 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE daEndDtNac AS DATE FORMAT "99/99/9999":U INITIAL 01/05/15 
     VIEW-AS FILL-IN 
     SIZE 9.86 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE daIniDtNac AS DATE FORMAT "99/99/9999":U INITIAL 01/05/15 
     LABEL "Data Nacionaliza‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 9.86 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE iEndFornExp AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 9.86 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE iEndGrEst AS INTEGER FORMAT ">9":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 3.86 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE iEndItin AS INTEGER FORMAT ">>>>9":U INITIAL 99999 
     VIEW-AS FILL-IN 
     SIZE 5.86 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE iIniFornExp AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Fornecedor(Exportador)" 
     VIEW-AS FILL-IN 
     SIZE 9.86 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE iIniGrEst AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Grupo Estoque" 
     VIEW-AS FILL-IN 
     SIZE 3.86 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE iIniItin AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Itiner rio" 
     VIEW-AS FILL-IN 
     SIZE 5.86 BY .88
     FONT 1 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-13
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-15
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-16
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-17
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-18
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-19
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-20
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-21
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-22
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-23
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-24
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-25
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-26
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rsClassif AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Por Embarque/Detalhado", 1,
"Por Embarque/Resumido", 2,
"Por Fornecedor(Exportador)/Resumido", 3,
"Por Item/Resumido", 4
     SIZE 31 BY 4
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34 BY 5.

DEFINE VARIABLE cFiDescMoeda AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88 NO-UNDO.

DEFINE VARIABLE daDtConv AS DATE FORMAT "99/99/9999":U 
     LABEL "Data ConversÆo" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE iFiMoeda AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Moeda" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE rsCustoItem AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Unit rio", 1,
"Total", 2
     SIZE 10 BY 1.67 NO-UNDO.

DEFINE VARIABLE rsDespImpos AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Despesa", 1,
"Imposto", 2
     SIZE 15.29 BY 2.13 NO-UNDO.

DEFINE RECTANGLE RECT-37
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53 BY 2.75.

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30.43 BY 3.13.

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30.43 BY 2.42.

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30.43 BY 3.13.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30.43 BY 2.42.

DEFINE VARIABLE lDesembParc AS LOGICAL INITIAL no 
     LABEL "Considera Desembarque Parcial" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY 1.08
     FONT 1 NO-UNDO.

DEFINE VARIABLE lDespImp AS LOGICAL INITIAL no 
     LABEL "Detalha Despesas/Impostos" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .96
     FONT 1 NO-UNDO.

DEFINE VARIABLE lDtNac AS LOGICAL INITIAL no 
     LABEL "Data de Nacionaliza‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1.08
     FONT 1 NO-UNDO.

DEFINE VARIABLE lItens AS LOGICAL INITIAL no 
     LABEL "Detalha Itens" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .92
     FONT 1 NO-UNDO.

DEFINE VARIABLE lNFCompl AS LOGICAL INITIAL no 
     LABEL "Considera NF Complementar" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1.08
     FONT 1 NO-UNDO.

DEFINE VARIABLE lOrcReal AS LOGICAL INITIAL no 
     LABEL "Or‡ado X Realizado" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .96
     FONT 1 NO-UNDO.

DEFINE BUTTON btConfigImpr 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON btFile 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE cFile AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.14 BY .63
     FONT 1 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execu‡Æo" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63
     FONT 1 NO-UNDO.

DEFINE VARIABLE rsDestiny AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3
     SIZE 44 BY 1.08
     FONT 1 NO-UNDO.

DEFINE VARIABLE rsExecution AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
"Batch", 2
     SIZE 27.72 BY .92
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btOK AT ROW 16.75 COL 2
     btCancel AT ROW 16.75 COL 13
     btHelp2 AT ROW 16.75 COL 80
     rtToolBar AT ROW 16.54 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 16.96
         FONT 1.

DEFINE FRAME fPage2
     cIniEstabel AT ROW 2 COL 18 COLON-ALIGNED HELP
          ""
          LABEL "Estabelecimento":R7
          FONT 1
     cEndEstabel AT ROW 2 COL 53.86 COLON-ALIGNED HELP
          "" NO-LABEL
          FONT 1
     cIniEmb AT ROW 3 COL 18 COLON-ALIGNED
     cEndEmb AT ROW 3 COL 53.86 COLON-ALIGNED NO-LABEL
     cIniItem AT ROW 4 COL 18 COLON-ALIGNED
     cEndItem AT ROW 4 COL 53.86 COLON-ALIGNED NO-LABEL
     iIniGrEst AT ROW 5 COL 18 COLON-ALIGNED
     iEndGrEst AT ROW 5 COL 53.86 COLON-ALIGNED NO-LABEL
     cIniFam AT ROW 6 COL 18 COLON-ALIGNED
     cEndFam AT ROW 6 COL 53.86 COLON-ALIGNED NO-LABEL
     iIniFornExp AT ROW 7 COL 18 COLON-ALIGNED
     iEndFornExp AT ROW 7 COL 53.86 COLON-ALIGNED NO-LABEL
     cIniProc AT ROW 8 COL 18 COLON-ALIGNED
     cEndProc AT ROW 8 COL 53.86 COLON-ALIGNED NO-LABEL
     iIniItin AT ROW 9 COL 18 COLON-ALIGNED
     iEndItin AT ROW 9 COL 53.86 COLON-ALIGNED NO-LABEL
     daIniDtNac AT ROW 10 COL 18 COLON-ALIGNED
     daEndDtNac AT ROW 10 COL 53.86 COLON-ALIGNED NO-LABEL
     IMAGE-1 AT ROW 2 COL 39
     IMAGE-11 AT ROW 10 COL 39
     IMAGE-12 AT ROW 10 COL 53
     IMAGE-13 AT ROW 9 COL 39
     IMAGE-14 AT ROW 9 COL 53
     IMAGE-15 AT ROW 8 COL 39
     IMAGE-16 AT ROW 8 COL 53
     IMAGE-17 AT ROW 7 COL 39
     IMAGE-18 AT ROW 7 COL 53
     IMAGE-19 AT ROW 6 COL 39
     IMAGE-2 AT ROW 2 COL 53
     IMAGE-20 AT ROW 6 COL 53
     IMAGE-21 AT ROW 5 COL 39
     IMAGE-22 AT ROW 5 COL 53
     IMAGE-23 AT ROW 4 COL 39
     IMAGE-24 AT ROW 4 COL 53
     IMAGE-25 AT ROW 3 COL 39
     IMAGE-26 AT ROW 3 COL 53
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7 ROW 2.81
         SIZE 76.86 BY 10.15
         FONT 1.

DEFINE FRAME fPage4
     iFiMoeda AT ROW 2.25 COL 25.86 COLON-ALIGNED HELP
          "Moeda para conversÆo"
     cFiDescMoeda AT ROW 2.25 COL 30.86 COLON-ALIGNED NO-LABEL
     daDtConv AT ROW 3.25 COL 25.86 COLON-ALIGNED HELP
          "Data de cota‡Æo utilizada para conversÆo"
     lDtNac AT ROW 3.25 COL 43.86 HELP
          "Considera Dt Cot. Nacionaliza‡Æo como Dt Cot. ConversÆo Moeda"
     lOrcReal AT ROW 5.13 COL 10.72 HELP
          "Apresenta coluna valores or‡ados"
     lDespImp AT ROW 5.96 COL 10.72 HELP
          "Detalha despesas e impostos"
     lItens AT ROW 6.83 COL 10.72 HELP
          "Detalha itens dos embarques"
     rsCustoItem AT ROW 8.92 COL 10.72 HELP
          "Apresenta custo do item de forma unit ria ou total" NO-LABEL
     rsDespImpos AT ROW 5.58 COL 42.57 HELP
          "Considera Imposto de Importa‡Æo" NO-LABEL
     lDesembParc AT ROW 8.5 COL 42.57 HELP
          "Considera embarques relacionados"
     lNFCompl AT ROW 9.5 COL 42.57 HELP
          "Considera Notas Fiscais Complementares"
     "Custo Item" VIEW-AS TEXT
          SIZE 9.14 BY .75 AT ROW 8.08 COL 10.57
     "Considera II" VIEW-AS TEXT
          SIZE 8.86 BY .83 AT ROW 4.58 COL 43.72
     RECT-37 AT ROW 1.75 COL 13.86
     RECT-38 AT ROW 4.92 COL 8.86
     RECT-39 AT ROW 8.33 COL 8.86
     RECT-40 AT ROW 4.92 COL 41
     RECT-41 AT ROW 8.33 COL 41
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7 ROW 2.81
         SIZE 76.86 BY 10.15
         FONT 1.

DEFINE FRAME fPage3
     rsClassif AT ROW 2.25 COL 5 HELP
          "Classifica‡Æo para emissÆo do relat¢rio" NO-LABEL
     RECT-13 AT ROW 1.75 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7 ROW 2.81
         SIZE 76.86 BY 10.15
         FONT 1.

DEFINE FRAME fPage6
     rsDestiny AT ROW 2.38 COL 3.29 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     btFile AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     btConfigImpr AT ROW 3.58 COL 43.29 HELP
          "Configura‡Æo da impressora"
     cFile AT ROW 3.63 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rsExecution AT ROW 5.75 COL 3 HELP
          "Modo de Execu‡Æo" NO-LABEL
     text-destino AT ROW 1.63 COL 1.86 COLON-ALIGNED NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7 ROW 2.81
         SIZE 76.86 BY 10.15
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wReport ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 16.96
         WIDTH              = 90
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wReport 
/* ************************* Included-Libraries *********************** */

{report/report.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wReport
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME fPage2:FRAME = FRAME fpage0:HANDLE
       FRAME fPage3:FRAME = FRAME fpage0:HANDLE
       FRAME fPage4:FRAME = FRAME fpage0:HANDLE
       FRAME fPage6:FRAME = FRAME fpage0:HANDLE.

/* SETTINGS FOR FRAME fpage0
   NOT-VISIBLE FRAME-NAME                                               */
/* SETTINGS FOR FRAME fPage2
                                                                        */
/* SETTINGS FOR FILL-IN cEndEstabel IN FRAME fPage2
   LIKE = mgadm.estabelec.cod-estabel EXP-LABEL EXP-SIZE                */
/* SETTINGS FOR FILL-IN cIniEstabel IN FRAME fPage2
   LIKE = mgadm.estabelec.cod-estabel EXP-LABEL EXP-SIZE                */
/* SETTINGS FOR FRAME fPage3
                                                                        */
/* SETTINGS FOR FRAME fPage4
   Custom                                                               */
/* SETTINGS FOR FRAME fPage6
                                                                        */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME fPage6     = 
                "Destino".

ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME fPage6     = 
                "Execu‡Æo".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wReport)
THEN wReport:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fpage0
/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage2
/* Query rebuild information for FRAME fPage2
     _Query            is NOT OPENED
*/  /* FRAME fPage2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage6
/* Query rebuild information for FRAME fPage6
     _Query            is NOT OPENED
*/  /* FRAME fPage6 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wReport wReport
ON END-ERROR OF wReport
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wReport wReport
ON WINDOW-CLOSE OF wReport
DO:
  /* This event will close the window and terminate the procedure.  */
  {report/logfin.i}  
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel wReport
ON CHOOSE OF btCancel IN FRAME fpage0 /* Fechar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage6
&Scoped-define SELF-NAME btConfigImpr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btConfigImpr wReport
ON CHOOSE OF btConfigImpr IN FRAME fPage6
DO:
   {report/rpimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFile wReport
ON CHOOSE OF btFile IN FRAME fPage6
DO:
    {report/rparq.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btHelp2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp2 wReport
ON CHOOSE OF btHelp2 IN FRAME fpage0 /* Ajuda */
DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOK wReport
ON CHOOSE OF btOK IN FRAME fpage0 /* Executar */
DO:
   do  on error undo, return no-apply:
       run piExecute.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage4
&Scoped-define SELF-NAME iFiMoeda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iFiMoeda wReport
ON LEAVE OF iFiMoeda IN FRAME fPage4 /* Moeda */
DO:
  {method/ReferenceFields.i
      &HandleDBOLeave="h-boun005"
      &KeyValue1="INT(iFiMoeda:SCREEN-VALUE IN FRAME fPage4)"
      &FieldName1="descricao"
      &FieldScreen1="cFiDescMoeda"
      &Frame1="fPage4"}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iFiMoeda wReport
ON MOUSE-SELECT-DBLCLICK OF iFiMoeda IN FRAME fPage4 /* Moeda */
OR F5 OF iFiMoeda IN FRAME fPage4 DO:
    {include/zoomvar.i &prog-zoom=adzoom/z01ad178.w
                       &campo=iFiMoeda
                       &campozoom=mo-codigo
                       &campo2=cFiDescMoeda
                       &campozoom2=descricao
                       &frame="fPage4"}                       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lDtNac
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lDtNac wReport
ON VALUE-CHANGED OF lDtNac IN FRAME fPage4 /* Data de Nacionaliza‡Æo */
DO:

  if lDtNac:checked in frame fPage4 then
     assign daDtConv:sensitive in frame fPage4 = no.
  else
     assign daDtConv:sensitive in frame fPage4 = yes.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lItens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lItens wReport
ON VALUE-CHANGED OF lItens IN FRAME fPage4 /* Detalha Itens */
DO:

  if lItens:checked in frame fPage4 then
      assign rsCustoItem:sensitive in frame fPage4 = yes.
  else
      assign rsCustoItem:screen-value in frame fPage4 = "1"
             rsCustoItem:sensitive    in frame fPage4 = no.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage3
&Scoped-define SELF-NAME rsClassif
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsClassif wReport
ON VALUE-CHANGED OF rsClassif IN FRAME fPage3
DO:

    if rsClassif:screen-value in frame fPage3 = "1" then
        assign lOrcReal:sensitive in frame fPage4 = yes.
    else
        assign lOrcReal:checked      in frame fPage4 = no 
               lOrcReal:sensitive    in frame fPage4 = no.

    if rsClassif:screen-value in frame fPage3 = "4" then
        assign lItens:checked      in frame fPage4 = no
               lItens:sensitive    in frame fPage4 = no.
    else
        assign lItens:sensitive in frame fPage4 = yes.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage6
&Scoped-define SELF-NAME rsDestiny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsDestiny wReport
ON VALUE-CHANGED OF rsDestiny IN FRAME fPage6
DO:
do  with frame fPage6:
    case self:screen-value:
        when "1" then do:
            assign cFile:sensitive       = no
                   cFile:visible         = yes
                   btFile:visible        = no
                   btConfigImpr:visible  = yes.
        end.
        when "2" then do:
            assign cFile:sensitive       = yes
                   cFile:visible         = yes
                   btFile:visible        = yes
                   btConfigImpr:visible  = no.
        end.
        when "3" then do:
            assign cFile:visible         = no
                   cFile:sensitive       = no
                   btFile:visible        = no
                   btConfigImpr:visible  = no.
        end.
    end case.
end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsExecution
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsExecution wReport
ON VALUE-CHANGED OF rsExecution IN FRAME fPage6
DO:
   {report/rprse.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wReport 


/*--- L¢gica para inicializa‡Æo do programam ---*/
{report/mainblock.i}

&IF "{&mguni_version}" >= "2.071" &THEN
    ASSIGN cEndEstabel:SCREEN-VALUE IN FRAME fPage2 = "ZZZZZ".
&ELSE
    ASSIGN cEndEstabel:SCREEN-VALUE IN FRAME fPage2 = "ZZZ".
&ENDIF

/*--- Verifica se o DBO j  est  inicializado ---*/
    IF NOT VALID-HANDLE(h-boun005) THEN DO:
        {btb/btb008za.i1 unbo/boun005na.p YES}
        {btb/btb008za.i2 unbo/boun005na.p '' h-boun005} 
    END.

    RUN openQueryStatic IN h-boun005 (INPUT "Main":U) NO-ERROR.

    iFiMoeda:LOAD-MOUSE-POINTER("image/lupa.cur":U) IN FRAME fPage4.

ASSIGN 
  rsClassif:SENSITIVE IN FRAME fPage3 = NO.
ASSIGN
  lOrcReal     = YES
  lDespImp     = YES
  lItens       = YES
  rsCustoItem  = 1
  rsDespImpos  = 1
  lDesembParc  = YES
  lNFCompl     = YES.
DISP
  lOrcReal     
  lDespImp     
  lItens       
  rsCustoItem  
  rsDespImpos  
  lDesembParc  
  lNFCompl     
  WITH FRAME fPage4.
ASSIGN
  lOrcReal     :SENSITIVE IN FRAME fPage4 = NO 
  lDespImp     :SENSITIVE IN FRAME fPage4 = NO 
  lItens       :SENSITIVE IN FRAME fPage4 = NO 
  rsCustoItem  :SENSITIVE IN FRAME fPage4 = NO 
  rsDespImpos  :SENSITIVE IN FRAME fPage4 = NO 
  lDesembParc  :SENSITIVE IN FRAME fPage4 = NO 
  lNFCompl     :SENSITIVE IN FRAME fPage4 = NO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wReport 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   assign daDtConv:screen-value in frame fPage4 = string(today).

   APPLY "VALUE-CHANGED" to rsClassif IN FRAME fPage3.
   APPLY "VALUE-CHANGED" to lItens    IN FRAME fPage4.
   APPLY "VALUE-CHANGED" to lDtNac    IN FRAME fPage4.

    &IF "{&mguni_version}" >= "2.071" &THEN
        ASSIGN cEndEstabel:SCREEN-VALUE IN FRAME fPage2 = "ZZZZZ".
    &ELSE
        ASSIGN cEndEstabel:SCREEN-VALUE IN FRAME fPage2 = "ZZZ".
    &ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piExecute wReport 
PROCEDURE piExecute :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*** Relatorio ***/
do on error undo, return error on stop  undo, return error:
    {report/rpexa.i}

    if input frame fPage6 rsDestiny = 2 and
       input frame fPage6 rsExecution = 1 then do:
        run utp/ut-vlarq.p (input input frame fPage6 cFile).

        if return-value = "NOK":U then do:
            run utp/ut-msgs.p (input "show", input 73, input "").
            apply "ENTRY":U to cFile in frame fPage6.
            return error.
        end.
    end.


    /* Coloque aqui as valida‡äes das outras p ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p gina com 
       problemas e colocar o focus no campo com problemas */

    /* Valia‡Æo Dados P gina Sele‡Æo */
    if  input frame fPage2 cIniEstabel > input frame fPage2 cEndEstabel  then do:
        run utp/ut-msgs.p(input "show",
                          input 4733,
                          input "").
        apply "ENTRY":U to cIniEstabel in frame fPage2.
        return error.
    end.

    if  input frame fPage2 cIniEmb > input frame fPage2 cEndEmb  then do:
        run utp/ut-msgs.p(input "show",
                          input 4733,
                          input "").
        apply "ENTRY":U to cIniEmb in frame fPage2.
        return error.
    end.

    if  input frame fPage2 cIniItem > input frame fPage2 cEndItem  then do:
        run utp/ut-msgs.p(input "show",
                          input 4733,
                          input "").
        apply "ENTRY":U to cIniItem in frame fPage2.
        return error.
    end.

    if  input frame fPage2 iIniGrEst > input frame fPage2 iEndGrEst  then do:
        run utp/ut-msgs.p(input "show",
                          input 4733,
                          input "").
        apply "ENTRY":U to iIniGrEst in frame fPage2.
        return error.
    end.

    if  input frame fPage2 cIniFam > input frame fPage2 cEndFam  then do:
        run utp/ut-msgs.p(input "show",
                      input 4733,
                      input "").
        apply "ENTRY":U to cIniFam in frame fPage2.
        return error.
    end.

    if  input frame fPage2 iIniFornExp > input frame fPage2 iEndFornExp  then do:
        run utp/ut-msgs.p(input "show",
                      input 4733,
                      input "").
        apply "ENTRY":U to iIniFornExp in frame fPage2.
        return error.
    end.

    if  input frame fPage2 cIniProc > input frame fPage2 cEndProc  then do:
        run utp/ut-msgs.p(input "show",
                      input 4733,
                      input "").
        apply "ENTRY":U to cIniProc in frame fPage2.
        return error.
    end.

    if  input frame fPage2 iIniItin > input frame fPage2 iEndItin  then do:
        run utp/ut-msgs.p(input "show",
                      input 4733,
                      input "").
        apply "ENTRY":U to iIniItin in frame fPage2.
        return error.
    end.

    if  input frame fPage2 daIniDtNac > input frame fPage2 daEndDtNac  then do:
        run utp/ut-msgs.p(input "show",
                      input 4733,
                      input "").
        apply "ENTRY":U to daIniDtNac in frame fPage2.
        return error.
    end.                             

    /* Valida Moeda */
    RUN GotoKey IN h-boun005 (input input frame fPage4 iFiMoeda).
    if return-value <> "OK" then do:
       RUN utp/ut-msgs.p (input "SHOW",
                          input 56,
                          input "Moeda").
        apply "Entry":U to iFiMoeda in frame fPage4.
        RETURN error. 
    end.


    /* Aqui sÆo gravados os campos da temp-table que ser  passada como parƒmetro
       para o programa RP.P */

    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame fPage6 rsDestiny
           tt-param.data-exec       = today
           tt-param.hora-exec       = time
           tt-param.classifica      = input frame fPage3 rsClassif
           tt-param.desc-classifica = entry((tt-param.classifica - 1) * 2 + 1, 
                                            rsClassif:radio-buttons in frame fPage3).

    if tt-param.destino = 1 
    then 
        assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame fPage6 cFile.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.

    /* Coloque aqui a l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */

    assign tt-param.c-cod-estabel-ini       = input frame fPage2  cIniEstabel   
           tt-param.c-cod-estabel-fim       = input frame fPage2  cEndEstabel 
           tt-param.c-embarque-ini          = input frame fPage2  cIniEmb    
           tt-param.c-embarque-fim          = input frame fPage2  cEndEmb    
           tt-param.c-it-codigo-ini         = input frame fPage2  cIniItem   
           tt-param.c-it-codigo-fim         = input frame fPage2  cEndItem   
           tt-param.i-ge-codigo-ini         = input frame fPage2  iIniGrEst  
           tt-param.i-ge-codigo-fim         = input frame fPage2  iEndGrEst  
           tt-param.c-fm-codigo-ini         = input frame fPage2  cIniFam    
           tt-param.c-fm-codigo-fim         = input frame fPage2  cEndFam    
           tt-param.i-cod-emitente-ini      = input frame fPage2  iIniFornExp 
           tt-param.i-cod-emitente-fim      = input frame fPage2  iEndFornExp 
           tt-param.c-nr-proc-imp-ini       = input frame fPage2  cIniProc   
           tt-param.c-nr-proc-imp-fim       = input frame fPage2  cEndProc   
           tt-param.i-cod-itiner-ini        = input frame fPage2  iIniItin   
           tt-param.i-cod-itiner-fim        = input frame fPage2  iEndItin   
           tt-param.da-dt-nac-ini           = input frame fPage2  daIniDtNac 
           tt-param.da-dt-nac-fim           = input frame fPage2  daEndDtNac  
           tt-param.moeda                   = input frame fPage4  iFiMoeda   
           tt-param.da-dt-conv              = input frame fPage4  daDtConv 
           tt-param.l-dt-nacional           = input frame fPage4  lDtNac   
           tt-param.l-orc-real              = input frame fPage4  lOrcReal 
           tt-param.l-desp-imposto          = input frame fPage4  lDespImp 
           tt-param.l-itens                 = input frame fPage4  lItens   
           tt-param.l-desemb-parc           = input frame fPage4  lDesembParc
           tt-param.l-nf-compl              = input frame fPage4  lNfCompl 
           tt-param.i-custo-item            = input frame fPage4  rsCustoItem 
           tt-param.i-desp-imposto          = input frame fPage4  rsDespImpos. 

    if tt-param.i-custo-item = 1 then
        assign tt-param.c-custo-item = "Unit rio".
    else
        assign tt-param.c-custo-item = "Total".

    if tt-param.i-desp-imposto = 1 then
        assign tt-param.c-desp-imposto = "Despesa".
    else
        assign tt-param.c-desp-imposto = "Imposto".

    /* Executar do programa RP.P que ir  criar o relat¢rio */
    {report/rpexb.i}

    SESSION:SET-WAIT-STATE("GENERAL":U).

    {report/rprun.i imp/esim0665rp.p}

    {report/rpexc.i}

    SESSION:SET-WAIT-STATE("":U).

    {report/rptrm.i}
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


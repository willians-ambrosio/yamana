&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wReport 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESMV0502 2.06.00.000}

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESMV0502
&GLOBAL-DEFINE Version        2.06.00.000
&GLOBAL-DEFINE VersionLayout  

&GLOBAL-DEFINE Folder         YES
&GLOBAL-DEFINE InitialPage    1
&GLOBAL-DEFINE FolderLabels   Sele��o,Classifica��o,Par�metro,Impress�o

&GLOBAL-DEFINE PGLAY          NO
&GLOBAL-DEFINE PGSEL          YES
&GLOBAL-DEFINE PGCLA          YES
&GLOBAL-DEFINE PGPAR          YES
&GLOBAL-DEFINE PGDIG          NO
&GLOBAL-DEFINE PGIMP          YES
&GLOBAL-DEFINE PGLOG          NO

&GLOBAL-DEFINE RTF            NO

&GLOBAL-DEFINE page0Widgets   btOk ~
                              btCancel ~
                              btHelp2
&GLOBAL-DEFINE page1Widgets   
&GLOBAL-DEFINE page2Widgets   i-ord-ini i-ord-fim c-oficina-ini c-oficina-fim d-data-ini d-data-fim i-empresa-ini i-empresa-fim c-equipamento-ini c-equipamento-fim c-componente-ini c-componente-fim
&GLOBAL-DEFINE page3Widgets   rsClassif
&GLOBAL-DEFINE page4Widgets   lNormal lAguardandoMaterial lAguardandoMOB lServExt lAguardandoEquipamento lAguardadoProgramacao lAguardandoRecurso lOrdemEquipamento  lOrdemComponente lOrdemCCusto 
&GLOBAL-DEFINE page5Widgets   
&GLOBAL-DEFINE page6Widgets   rsDestiny ~
                              btConfigImpr ~
                              btFile ~
                              rsExecution ~
                              l-habilitaRtf ~
                              btModelRtf
&GLOBAL-DEFINE page7Widgets   
&GLOBAL-DEFINE page8Widgets   

&GLOBAL-DEFINE page0Text      
&GLOBAL-DEFINE page1Text      
&GLOBAL-DEFINE page2Text      
&GLOBAL-DEFINE page3Text      
&GLOBAL-DEFINE page4Text      lNormal lAguardandoMaterial lAguardandoMOB lServExt lAguardandoEquipamento lAguardadoProgramacao lAguardandoRecurso lOrdemEquipamento lOrdemComponente lOrdemCCusto
&GLOBAL-DEFINE page5Text      
&GLOBAL-DEFINE page6Text      text-destino text-modo text-rtf text-ModelRtf
&GLOBAL-DEFINE page7Text      
&GLOBAL-DEFINE page8Text   

&GLOBAL-DEFINE page1Fields    
&GLOBAL-DEFINE page2Fields    i-ord-ini i-ord-fim c-oficina-ini c-oficina-fim d-data-ini d-data-fim i-empresa-ini i-empresa-fim c-equipamento-ini c-equipamento-fim c-componente-ini c-componente-fim
&GLOBAL-DEFINE page3Fields    
&GLOBAL-DEFINE page4Fields    
&GLOBAL-DEFINE page5Fields    
&GLOBAL-DEFINE page6Fields    cFile cModelRTF
&GLOBAL-DEFINE page7Fields    
&GLOBAL-DEFINE page8Fields    

/* Parameters Definitions ---                                           */

define temp-table tt-param no-undo
    FIELD i-ord-ini                 LIKE mmv-ord-manut.nr-ord-produ
    FIELD i-ord-fim                 LIKE mmv-ord-manut.nr-ord-produ
    FIELD c-oficina-ini             LIKE mmv-ord-manut.cod-ofici
    FIELD c-oficina-fim             LIKE mmv-ord-manut.cod-ofici
    FIELD d-data-fim                LIKE mmv-ord-manut.dat-abert
    FIELD d-data-ini                LIKE mmv-ord-manut.dat-abert
    FIELD i-empresa-fim             LIKE mmv-ord-manut.ep-codigo
    FIELD i-empresa-ini             LIKE mmv-ord-manut.ep-codigo
    FIELD c-equipamento-ini         LIKE mmv-ord-manut.cod-eqpto
    FIELD c-equipamento-fim         LIKE mmv-ord-manut.cod-eqpto
    FIELD c-componente-ini          LIKE mmv-ord-manut.cod-compon
    FIELD c-componente-fim          LIKE mmv-ord-manut.cod-compon
    FIELD lNormal                   AS LOGICAL 
    FIELD lAguardandoMaterial       AS LOGICAL
    FIELD lAguardandoMOB            AS LOGICAL
    FIELD lServExt                  AS LOGICAL
    FIELD lAguardandoEquipamento    AS LOGICAL
    FIELD lAguardadoProgramacao     AS LOGICAL 
    FIELD lAguardandoRecurso        AS LOGICAL
    FIELD lOrdemEquipamento         AS LOGICAL
    FIELD lOrdemComponente          AS LOGICAL
    FIELD lOrdemCCusto              AS LOGICAL
    FIELD destino                   AS INTEGER
    FIELD arquivo                   AS CHAR FORMAT "x(35)":U
    FIELD usuario                   AS CHAR FORMAT "x(12)":U
    FIELD data-exec                 AS DATE
    FIELD hora-exec                 AS INTEGER
    FIELD classifica                AS INTEGER
    FIELD desc-classifica           AS CHAR FORMAT "x(40)":U
    FIELD modelo                    AS CHAR FORMAT "x(35)":U
    /*Alterado 15/02/2005 - tech1007 - Criado campo l�gico para verificar se o RTF foi habilitado*/
    field l-habilitaRtf    as LOG.
    /*Fim alteracao 15/02/2005*/

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9":U
    field exemplo          as character format "x(30)":U
    index id ordem.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-rtf              as char    no-undo.
def var c-arq-layout       as char    no-undo.      
def var c-arq-temp         as char    no-undo.
DEF VAR c-modelo-default   AS CHAR    NO-UNDO.

def stream s-imp.

/*15/02/2005 - tech1007 - Variavel definida para tratar se o programa est� rodando no WebEnabler*/
DEFINE SHARED VARIABLE hWenController AS HANDLE NO-UNDO.

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
     LABEL "&Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp2 
     LABEL "&Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "&Executar" 
     SIZE 10 BY 1.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE c-componente-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE c-componente-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Componente" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE c-equipamento-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE c-equipamento-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Equipamento" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE c-oficina-fim AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE c-oficina-ini AS CHARACTER FORMAT "X(8)":U 
     LABEL "Oficina" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE d-data-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE d-data-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Data Abertura" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE i-empresa-fim AS INTEGER FORMAT "->>9":U INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE i-empresa-ini AS INTEGER FORMAT "->>9":U INITIAL 0 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 4.86 BY .88 NO-UNDO.

DEFINE VARIABLE i-ord-fim AS INTEGER FORMAT "->>>,>>>,>>9":U INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE i-ord-ini AS INTEGER FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "Ordem" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE VARIABLE rsClassif AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Ordem", 1,
"Status", 2
     SIZE 17.86 BY 2
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22 BY 4.25.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY 8.25.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY 4.5.

DEFINE VARIABLE lAguardadoProgramacao AS LOGICAL INITIAL yes 
     LABEL "Aguardando Programa��o" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .83 NO-UNDO.

DEFINE VARIABLE lAguardandoEquipamento AS LOGICAL INITIAL yes 
     LABEL "Aguardando Equipamento" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .83 NO-UNDO.

DEFINE VARIABLE lAguardandoMaterial AS LOGICAL INITIAL yes 
     LABEL "Aguardando Material" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .83 NO-UNDO.

DEFINE VARIABLE lAguardandoMOB AS LOGICAL INITIAL yes 
     LABEL "Aguardando M�o-de-Obra" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .83 NO-UNDO.

DEFINE VARIABLE lAguardandoRecurso AS LOGICAL INITIAL yes 
     LABEL "Aguardando Recurso" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .83 NO-UNDO.

DEFINE VARIABLE lNormal AS LOGICAL INITIAL yes 
     LABEL "Normal" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .96
     FONT 1 NO-UNDO.

DEFINE VARIABLE lOrdemCCusto AS LOGICAL INITIAL yes 
     LABEL "Centro Custo" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE lOrdemComponente AS LOGICAL INITIAL yes 
     LABEL "Componente" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE lOrdemEquipamento AS LOGICAL INITIAL yes 
     LABEL "Equipamento" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE lServExt AS LOGICAL INITIAL yes 
     LABEL "Aguardando Servi�o Externo" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .83 NO-UNDO.

DEFINE BUTTON btConfigImpr 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON btFile 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON btModelRtf 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE cFile AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cModelRTF AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.14 BY .63
     FONT 1 NO-UNDO.

DEFINE VARIABLE text-ModelRtf AS CHARACTER FORMAT "X(256)":U INITIAL "Modelo:" 
      VIEW-AS TEXT 
     SIZE 10 BY .67 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execu��o" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63
     FONT 1 NO-UNDO.

DEFINE VARIABLE text-rtf AS CHARACTER FORMAT "X(256)":U INITIAL "Rich Text Format(RTF)" 
      VIEW-AS TEXT 
     SIZE 16 BY .67 NO-UNDO.

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
     SIZE 27.86 BY .92
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.14 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.14 BY 1.71.

DEFINE RECTANGLE rect-rtf
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.14 BY 3.21.

DEFINE VARIABLE l-habilitaRtf AS LOGICAL INITIAL no 
     LABEL "RTF" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY 1.08
     FONT 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btOK AT ROW 16.75 COL 2
     btCancel AT ROW 16.75 COL 13
     btHelp2 AT ROW 16.75 COL 80
     rtToolBar AT ROW 16.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90.86 BY 17.17
         FONT 1.

DEFINE FRAME fPage2
     i-ord-ini AT ROW 1.5 COL 24.71
     i-ord-fim AT ROW 1.5 COL 49 NO-LABEL
     c-oficina-ini AT ROW 2.5 COL 28 COLON-ALIGNED
     c-oficina-fim AT ROW 2.5 COL 47 COLON-ALIGNED NO-LABEL
     d-data-ini AT ROW 3.5 COL 28 COLON-ALIGNED
     d-data-fim AT ROW 3.5 COL 47 COLON-ALIGNED NO-LABEL
     i-empresa-ini AT ROW 4.5 COL 33 COLON-ALIGNED
     i-empresa-fim AT ROW 4.5 COL 47 COLON-ALIGNED NO-LABEL
     c-equipamento-ini AT ROW 5.46 COL 19.72 COLON-ALIGNED
     c-equipamento-fim AT ROW 5.5 COL 47 COLON-ALIGNED NO-LABEL
     c-componente-ini AT ROW 6.5 COL 19.72 COLON-ALIGNED
     c-componente-fim AT ROW 6.5 COL 47 COLON-ALIGNED NO-LABEL
     IMAGE-1 AT ROW 1.5 COL 39.86
     IMAGE-2 AT ROW 1.5 COL 46
     IMAGE-3 AT ROW 2.5 COL 39.86
     IMAGE-4 AT ROW 2.5 COL 46
     IMAGE-5 AT ROW 3.5 COL 39.86
     IMAGE-6 AT ROW 3.5 COL 46
     IMAGE-7 AT ROW 4.5 COL 39.86
     IMAGE-8 AT ROW 4.5 COL 46
     IMAGE-9 AT ROW 5.5 COL 39.86
     IMAGE-10 AT ROW 5.5 COL 46
     IMAGE-11 AT ROW 6.5 COL 39.86
     IMAGE-12 AT ROW 6.5 COL 46
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.81
         SIZE 84.43 BY 10.15
         FONT 1.

DEFINE FRAME fPage6
     rsDestiny AT ROW 2.38 COL 3.14 HELP
          "Destino de Impress�o do Relat�rio" NO-LABEL
     cFile AT ROW 3.63 COL 3.14 HELP
          "Nome do arquivo de destino do relat�rio" NO-LABEL
     btFile AT ROW 3.5 COL 43 HELP
          "Escolha do nome do arquivo"
     btConfigImpr AT ROW 3.5 COL 43 HELP
          "Configura��o da impressora"
     l-habilitaRtf AT ROW 5.58 COL 3.14
     cModelRTF AT ROW 7.29 COL 3 HELP
          "Nome do arquivo de modelo" NO-LABEL
     btModelRtf AT ROW 7.29 COL 43 HELP
          "Escolha o arquivo de modelo"
     rsExecution AT ROW 9.5 COL 2.86 HELP
          "Modo de Execu��o" NO-LABEL
     text-destino AT ROW 1.63 COL 1.86 COLON-ALIGNED NO-LABEL
     text-rtf AT ROW 5 COL 2 COLON-ALIGNED NO-LABEL
     text-ModelRtf AT ROW 6.54 COL 2 COLON-ALIGNED NO-LABEL
     text-modo AT ROW 8.75 COL 1.14 COLON-ALIGNED NO-LABEL
     rect-rtf AT ROW 5.29 COL 2
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 9 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.81
         SIZE 84.43 BY 10.15
         FONT 1.

DEFINE FRAME fPage4
     lNormal AT ROW 2.25 COL 5
     lOrdemEquipamento AT ROW 2.25 COL 56
     lAguardandoMaterial AT ROW 3.25 COL 5
     lOrdemComponente AT ROW 3.25 COL 56
     lAguardandoMOB AT ROW 4.25 COL 5
     lOrdemCCusto AT ROW 4.25 COL 56
     lServExt AT ROW 5.25 COL 5
     lAguardandoEquipamento AT ROW 6.25 COL 5
     lAguardadoProgramacao AT ROW 7.25 COL 5
     lAguardandoRecurso AT ROW 8.25 COL 5
     "Status" VIEW-AS TEXT
          SIZE 6 BY .54 AT ROW 1.29 COL 5
     "Tipo Ordem Manuten��o" VIEW-AS TEXT
          SIZE 18 BY .54 AT ROW 1.25 COL 54
     RECT-14 AT ROW 1.5 COL 3
     RECT-15 AT ROW 1.5 COL 53
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.81
         SIZE 84.43 BY 10.15
         FONT 1.

DEFINE FRAME fPage3
     rsClassif AT ROW 3.13 COL 7.29 HELP
          "Classifica��o para emiss�o do relat�rio" NO-LABEL
     "Classificar Por:" VIEW-AS TEXT
          SIZE 11 BY .54 AT ROW 1.75 COL 6
     RECT-13 AT ROW 2 COL 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.81
         SIZE 84.43 BY 10.15
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
  CREATE WINDOW wReport ASSIGN
         HIDDEN             = YES
         TITLE              = "Relat�rio d Listagem O.M Status"
         HEIGHT             = 17.54
         WIDTH              = 90.57
         MAX-HEIGHT         = 38.63
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 38.63
         VIRTUAL-WIDTH      = 182.86
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
/* SETTINGS FOR FILL-IN i-ord-fim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN i-ord-ini IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME fPage3
                                                                        */
/* SETTINGS FOR FRAME fPage4
                                                                        */
/* SETTINGS FOR FRAME fPage6
   Custom                                                               */
ASSIGN 
       btModelRtf:HIDDEN IN FRAME fPage6           = TRUE.

ASSIGN 
       cModelRTF:HIDDEN IN FRAME fPage6           = TRUE.

ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME fPage6     = 
                "Destino".

ASSIGN 
       text-ModelRtf:HIDDEN IN FRAME fPage6           = TRUE
       text-ModelRtf:PRIVATE-DATA IN FRAME fPage6     = 
                "Modelo:".

ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME fPage6     = 
                "Execu��o".

ASSIGN 
       text-rtf:HIDDEN IN FRAME fPage6           = TRUE
       text-rtf:PRIVATE-DATA IN FRAME fPage6     = 
                "Rich Text Format(RTF)".

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
ON END-ERROR OF wReport /* Relat�rio d Listagem O.M Status */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wReport wReport
ON WINDOW-CLOSE OF wReport /* Relat�rio d Listagem O.M Status */
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


&Scoped-define FRAME-NAME fPage6
&Scoped-define SELF-NAME btModelRtf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btModelRtf wReport
ON CHOOSE OF btModelRtf IN FRAME fPage6
DO:
    def var cFile as char no-undo.
    def var l-ok  as logical no-undo.

    assign cModelRTF = replace(input frame {&frame-name} cModelRTF, "/", "~\").
    SYSTEM-DIALOG GET-FILE cFile
       FILTERS "*.rtf" "*.rtf",
               "*.*" "*.*"
       DEFAULT-EXTENSION "rtf"
       INITIAL-DIR "modelos" 
       MUST-EXIST
       USE-FILENAME
       UPDATE l-ok.
    if  l-ok = yes then
        assign cModelRTF:screen-value in frame {&frame-name}  = replace(cFile, "~\", "/"). 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
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


&Scoped-define FRAME-NAME fPage6
&Scoped-define SELF-NAME l-habilitaRtf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-habilitaRtf wReport
ON VALUE-CHANGED OF l-habilitaRtf IN FRAME fPage6 /* RTF */
DO:
    &IF "{&RTF}":U = "YES":U &THEN
    RUN pi-habilitaRtf.  
    &endif
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsDestiny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsDestiny wReport
ON VALUE-CHANGED OF rsDestiny IN FRAME fPage6
DO:
do  with frame fPage6:
    case self:screen-value:
        when "1":U then do:
            assign cFile:sensitive       = no
                   cFile:visible         = yes
                   btFile:visible        = no
                   btConfigImpr:visible  = yes
                   /*Alterado 15/02/2005 - tech1007 - Alterado para suportar adequadamente com a 
                     funcionalidade de RTF*/
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = NO
                   l-habilitaRtf:SCREEN-VALUE IN FRAME fPage6 = "No"
                   l-habilitaRtf = NO
                   &endif
                   .
                   /*Fim alteracao 15/02/2005*/
        end.
        when "2":U then do:
            assign cFile:sensitive       = yes
                   cFile:visible         = yes
                   btFile:visible        = yes
                   btConfigImpr:visible  = no
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = YES
                   &endif
                   .
        end.
        when "3":U then do:
            assign cFile:visible         = no
                   cFile:sensitive       = no
                   btFile:visible        = no
                   btConfigImpr:visible  = no
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = YES
                   &endif
                   .
            /*Alterado 15/02/2005 - tech1007 - Teste para funcionar corretamente no WebEnabler*/
            &IF "{&RTF}":U = "YES":U &THEN
            IF VALID-HANDLE(hWenController) THEN DO:
                ASSIGN l-habilitaRtf:sensitive  = NO
                       l-habilitaRtf:SCREEN-VALUE IN FRAME fPage6 = "No"
                       l-habilitaRtf = NO.
            END.
            &endif
            /*Fim alteracao 15/02/2005*/
        END.
        /*Alterado 15/02/2005 - tech1007 - Condi��o removida pois RTF n�o � mais um destino
        when "4":U then do:
            assign cFile:sensitive       = no
                   cFile:visible         = yes
                   btFile:visible        = no
                   btConfigImpr:visible  = yes
                   text-ModelRtf:VISIBLE   = YES
                   rect-rtf:VISIBLE       = YES
                   blModelRtf:VISIBLE       = yes.
        end.
        Fim alteracao 15/02/2005*/
    end case.
end.
&IF "{&RTF}":U = "YES":U &THEN
RUN pi-habilitaRtf.  
&endif
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


/*:T--- L�gica para inicializa��o do programam ---*/
{report/mainblock.i}

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
/*Alterado 17/02/2005 - tech1007 - Foi criado essa procedure para que seja realizado a inicializa��o
  correta dos componentes do RTF quando executado em ambiente local e no WebEnabler.*/
&IF "{&RTF}":U = "YES":U &THEN
IF VALID-HANDLE(hWenController) THEN DO:
    ASSIGN l-habilitaRtf:sensitive IN FRAME fPage6 = NO
           l-habilitaRtf:SCREEN-VALUE IN FRAME fPage6 = "No"
           l-habilitaRtf = NO.
           
END.
RUN pi-habilitaRtf.
&endif
/*Fim alteracao 17/02/2005*/
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

define var r-tt-digita as rowid no-undo.

&IF DEFINED(PGIMP) <> 0 AND "{&PGIMP}":U = "YES":U &THEN
/*:T** Relatorio ***/
do on error undo, return error on stop  undo, return error:
    {report/rpexa.i}

    /*15/02/2005 - tech1007 - Teste alterado pois RTF n�o � mais op��o de Destino*/
    if input frame fPage6 rsDestiny = 2 and
       input frame fPage6 rsExecution = 1 then do:
        run utp/ut-vlarq.p (input input frame fPage6 cFile).
        
        if return-value = "NOK":U then do:
            run utp/ut-msgs.p (input "show":U, input 73, input "":U).
            apply "ENTRY":U to cFile in frame fPage6.
            return error.
        end.
    end.
    
    /*16/02/2005 - tech1007 - Teste alterado para validar o modelo informado quando for RTF*/
    &IF "{&RTF}":U = "YES":U &THEN
    IF ( input frame fPage6 cModelRTF = "" AND
         input frame fPage6 l-habilitaRtf = YES ) OR
       ( SEARCH(INPUT FRAME fPage6 cModelRTF) = ? AND
         input frame fPage6 rsExecution = 1 AND
         input frame fPage6 l-habilitaRtf = YES )
         THEN DO:
        run utp/ut-msgs.p (input "show":U, input 73, input "":U).
        /*30/12/2004 - tech1007 - Evento removido pois causa problemas no WebEnabler*/
        /*apply "CHOOSE":U to blModelRtf in frame fPage6.*/
        return error.
    END.
    &endif
    
    /*:T Coloque aqui as valida��es da p�gina de Digita��o, lembrando que elas devem
       apresentar uma mensagem de erro cadastrada, posicionar nesta p�gina e colocar
       o focus no campo com problemas */
    /*browse brDigita:SET-REPOSITIONED-ROW (browse brDigita:DOWN, "ALWAYS":U).*/
    
    /*
    for each tt-digita no-lock:
        assign r-tt-digita = rowid(tt-digita).
        
        /*:T Valida��o de duplicidade de registro na temp-table tt-digita */
        find first b-tt-digita 
            where b-tt-digita.ordem = tt-digita.ordem 
              and rowid(b-tt-digita) <> rowid(tt-digita) 
            no-lock no-error.
        if  avail b-tt-digita then do:
            reposition brDigita to rowid rowid(b-tt-digita).
            
            run utp/ut-msgs.p (input "SHOW":U, input 108, input "":U).
            apply "ENTRY":U to tt-digita.ordem in browse brDigita.
            
            return error.
        end.
        
        /*:T As demais valida��es devem ser feitas aqui */
        if  tt-digita.ordem <= 0 then do:
            assign browse brDigita:CURRENT-COLUMN = tt-digita.ordem:HANDLE in browse brDigita.
            
            reposition brDigita to rowid r-tt-digita.
           
            run utp/ut-msgs.p (input "SHOW":U, input 99999, input "":U).
            apply "ENTRY":U to tt-digita.ordem in browse brDigita.
            
            return error.
        end.
        
    end. */
    
    
    /*:T Coloque aqui as valida��es das outras p�ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p�gina com 
       problemas e colocar o focus no campo com problemas */
    
    
    
    /*:T Aqui s�o gravados os campos da temp-table que ser� passada como par�metro
       para o programa RP.P */
    
    create tt-param.
    assign tt-param.i-ord-ini                   = INPUT FRAME fpage2 i-ord-ini        
           tt-param.i-ord-fim                   = INPUT FRAME fpage2 i-ord-fim        
           tt-param.c-oficina-ini               = INPUT FRAME fpage2 c-oficina-ini    
           tt-param.c-oficina-fim               = INPUT FRAME fpage2 c-oficina-fim    
           tt-param.d-data-fim                  = INPUT FRAME fpage2 d-data-fim       
           tt-param.d-data-ini                  = INPUT FRAME fpage2 d-data-ini       
           tt-param.i-empresa-fim               = INPUT FRAME fpage2 i-empresa-fim    
           tt-param.i-empresa-ini               = INPUT FRAME fpage2 i-empresa-ini    
           tt-param.c-equipamento-fim           = INPUT FRAME fpage2 c-equipamento-fim      
           tt-param.c-equipamento-ini           = INPUT FRAME fpage2 c-equipamento-ini      
           tt-param.c-componente-ini            = INPUT FRAME fpage2 c-componente-ini 
           tt-param.c-componente-fim            = INPUT FRAME fpage2 c-componente-fim 
           tt-param.lNormal                     = INPUT FRAME fpage4 lNormal
           tt-param.lAguardandoMaterial         = INPUT FRAME fpage4 lAguardandoMaterial
           tt-param.lAguardandoMOB              = INPUT FRAME fpage4 lAguardandoMOB
           tt-param.lServExt                    = INPUT FRAME fpage4 lServExt
           tt-param.lAguardandoEquipamento      = INPUT FRAME fpage4 lAguardandoEquipamento
           tt-param.lAguardadoProgramacao       = INPUT FRAME fpage4 lAguardadoProgramacao       
           tt-param.lAguardandoRecurso          = INPUT FRAME fpage4 lAguardandoRecurso
           tt-param.lOrdemEquipamento           = INPUT FRAME fpage4 lOrdemEquipamento
           tt-param.lOrdemComponente            = INPUT FRAME fpage4 lOrdemComponente
           tt-param.lOrdemCCusto                = INPUT FRAME fpage4 lOrdemCCusto
           tt-param.usuario                     = c-seg-usuario
           tt-param.destino                     = input frame fPage6 rsDestiny
           tt-param.data-exec                   = today
           tt-param.hora-exec                   = time
           tt-param.classifica                  = input frame fPage3 rsClassif
           tt-param.desc-classifica = entry((tt-param.classifica - 1) * 2 + 1, 
                                            rsClassif:radio-buttons in frame fPage3)
           &IF "{&RTF}":U = "YES":U &THEN
           tt-param.modelo                      = INPUT FRAME fPage6 cModelRTF
           tt-param.l-habilitaRtf               = INPUT FRAME fPage6 l-habilitaRtf
           &endif
           .
    IF tt-param.classifica = 1 THEN 
        ASSIGN tt-param.desc-classifica = "Ordem":T.
    ELSE
        ASSIGN tt-param.desc-classifica = "Status":T.
        
    if tt-param.destino = 1 
    then 
        assign tt-param.arquivo = "":U.
    else if  tt-param.destino = 2 
        then assign tt-param.arquivo = input frame fPage6 cFile.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    
    /*:T Coloque aqui a l�gica de grava��o dos demais campos que devem ser passados
       como par�metros para o programa RP.P, atrav�s da temp-table tt-param */
    
    
    
    /*:T Executar do programa RP.P que ir� criar o relat�rio */
    {report/rpexb.i}
    
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    {report/rprun.i mvp/esmv0502rp.p}
    
    {report/rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
    {report/rptrm.i}
end.
&ELSE

/*:T** Importacao/Exportacao ***/
do  on error undo, return error
    on stop  undo, return error:     

    {report/rpexa.i}

    if  input frame fPage7 rsDestiny = 2 and
        input frame fPage7 rsExecution = 1 then do:
        run utp/ut-vlarq.p (input input frame fPage7 cDestinyFile).
        if  return-value = "NOK":U then do:
            run utp/ut-msgs.p (input "SHOW":U,
                               input 73,
                               input "":U).
            apply "ENTRY":U to cDestinyFile in frame fPage7.                   
            return error.
        end.
    end.
    
    assign file-info:file-name = input frame fPage4 cInputFile.
    if  file-info:pathname = ? and
        input frame fPage7 rsExecution = 1 then do:
        run utp/ut-msgs.p (input "SHOW":U,
                           input 326,
                           input cInputFile).                               
        apply "ENTRY":U to cInputFile in frame fPage4.                
        return error.
    end. 
            
    /*:T Coloque aqui as valida��es das outras p�ginas, lembrando que elas
       devem apresentar uma mensagem de erro cadastrada, posicionar na p�gina 
       com problemas e colocar o focus no campo com problemas             */    
         
    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame fPage7 rsDestiny
           tt-param.todos           = input frame fPage7 rsAll
           tt-param.arq-entrada     = input frame fPage4 cInputFile
           tt-param.data-exec       = today
           tt-param.hora-exec       = time.

    if  tt-param.destino = 1 then
        assign tt-param.arq-destino = "":U.
    else
    if  tt-param.destino = 2 THEN
        assign tt-param.arq-destino = input frame fPage7 cDestinyFile.
    else
        assign tt-param.arq-destino = session:temp-directory + c-programa-mg97 + ".tmp":U.

    /*:T Coloque aqui a l�gica de grava��o dos par�mtros e sele��o na temp-table
       tt-param */ 

    {report/imexb.i}

    if  session:set-wait-state("GENERAL":U) then.

    {report/rprun.i mvp/esmv0502rp.p}

    {report/imexc.i}

    if  session:set-wait-state("":U) then.
    
    {report/imtrm.i tt-param.arq-destino tt-param.destino}
    
end.
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          mgadm            PROGRESS
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
********************************************************************************/
{include/i-prgvrs.i ESRI0404 2.00.00.017 } /*** 010017 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ri0404 MRI}
&ENDIF

{rip/ri9999.i2}
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
&GLOBAL-DEFINE Program        ESRI0404
&GLOBAL-DEFINE Version        2.00.00.017
&GLOBAL-DEFINE VersionLayout  

&GLOBAL-DEFINE Folder         YES
&GLOBAL-DEFINE InitialPage    1
&GLOBAL-DEFINE FolderLabels   Sele‡Æo,Parƒmetros,ImpressÆo

&GLOBAL-DEFINE PGLAY          NO
&GLOBAL-DEFINE PGSEL          YES
&GLOBAL-DEFINE PGCLA          NO
&GLOBAL-DEFINE PGPAR          YES
&GLOBAL-DEFINE PGDIG          NO
&GLOBAL-DEFINE PGIMP          YES
&GLOBAL-DEFINE PGLOG          NO

&GLOBAL-DEFINE page0Widgets   btOk ~
                              btCancel ~
                              btHelp2
&GLOBAL-DEFINE page1Widgets   
&GLOBAL-DEFINE page2Widgets   
&GLOBAL-DEFINE page3Widgets   
&GLOBAL-DEFINE page4Widgets   
&GLOBAL-DEFINE page5Widgets   
&GLOBAL-DEFINE page6Widgets   rsDestiny ~
                              btConfigImpr ~
                              btFile ~
                              rsExecution
&GLOBAL-DEFINE page7Widgets   
&GLOBAL-DEFINE page8Widgets   
&GLOBAL-DEFINE page0Text      
&GLOBAL-DEFINE page1Text      
&GLOBAL-DEFINE page2Text      
&GLOBAL-DEFINE page3Text      
&GLOBAL-DEFINE page4Text      
&GLOBAL-DEFINE page5Text      
&GLOBAL-DEFINE page6Text      text-destino text-modo
&GLOBAL-DEFINE page7Text      
&GLOBAL-DEFINE page8Text   

&GLOBAL-DEFINE page1Fields    
&GLOBAL-DEFINE page2Fields    rs-tipo-sel ~
                              c-cod-estabel-ini ~
                              c-cod-estabel-fim ~
                              i-cod-grupo-ini ~
                              c-nr-patrimonio-ini c-nr-patrimonio-fim ~
                              i-bem-ini           i-bem-fim ~
                              c-serie-ini         c-serie-fim ~
                              c-nr-doc-fis-ini    c-nr-doc-fis-fim ~
                              c-nat-oper-ini      c-nat-oper-fim ~
                              c-cod-emitente-ini  c-cod-emitente-fim ~
                              c-nr-ord-produ-ini  c-nr-ord-produ-fim ~
                              c-dt-docto-ini      c-dt-docto-fim
&GLOBAL-DEFINE page3Fields    
&GLOBAL-DEFINE page4Fields    c-data-corte l-gera-planilha rs-tip-relat
&GLOBAL-DEFINE page5Fields    
&GLOBAL-DEFINE page6Fields    
&GLOBAL-DEFINE page7Fields    
&GLOBAL-DEFINE page8Fields    

/* Parameters Definitions ---                                           */
DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino           AS INTEGER
    FIELD arquivo           AS CHAR    FORMAT "x(35)"
    FIELD usuario           AS CHAR    FORMAT "x(12)"
    FIELD data-exec         AS DATE
    FIELD hora-exec         AS INTEGER
    FIELD tipo-sel          AS INT
&IF "{&mguni_version}" >= "2.071" &THEN
    FIELD cod-estabel-ini    AS CHAR    format "x(05)"
    FIELD cod-estabel-fim    AS CHAR    format "x(05)"
&ELSE
    FIELD cod-estabel-ini    AS CHAR    format "x(03)"
    FIELD cod-estabel-fim    AS CHAR    format "x(03)"
&ENDIF
    FIELD i-tip-relat       AS INTEGER
    FIELD cod-grupo-ini     AS INTEGER FORMAT ">>>>>9"
    FIELD bem-ini           LIKE ri-bem.id-bem
    FIELD bem-fim           LIKE ri-bem.id-bem
    FIELD serie-ini         AS CHAR    FORMAT "x(03)"
    FIELD serie-fim         AS CHAR    FORMAT "x(03)"
    FIELD nr-patrimonio-ini LIKE ri-bem.nr-patrimonio
    FIELD nr-patrimonio-fim LIKE ri-bem.nr-patrimonio
    FIELD nr-doc-fis-ini    AS CHAR    FORMAT "x(16)"
    FIELD nr-doc-fis-fim    AS CHAR    FORMAT "x(16)"
    FIELD nat-oper-ini      AS CHAR    FORMAT "x(06)"
    FIELD nat-oper-fim      AS CHAR    FORMAT "x(06)"
    FIELD cod-emitente-ini  AS INT     FORMAT ">>>>>>>>9"
    FIELD cod-emitente-fim  AS INT     FORMAT ">>>>>>>>9"
    FIELD nr-ord-produ-ini  AS INT     FORMAT ">>>,>>>,>>9"
    FIELD nr-ord-produ-fim  AS INT     FORMAT ">>>,>>>,>>9"
    FIELD dt-docto-ini      AS DATE    FORMAT "99/99/9999"
    FIELD dt-docto-fim      AS DATE    FORMAT "99/99/9999"
    FIELD dt-corte          AS DATE    FORMAT "99/99/9999"
    FIELD l-gera-planilha   AS LOG  INIT NO
    FIELD c-arq-planilha    AS CHAR INIT ''.

/* Transfer Definitions */

def var raw-param          as raw     no-undo.

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-arq-layout       as char    no-undo.      
def var c-arq-temp         as char    no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.

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

DEFINE VARIABLE c-cod-emitente-fim AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-emitente-ini AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Emitente" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-estabel-fim AS CHARACTER FORMAT "X(03)":U 
     VIEW-AS FILL-IN 
     SIZE 5.86 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-estabel-ini AS CHARACTER FORMAT "X(03)":U 
     LABEL "Estabel" 
     VIEW-AS FILL-IN 
     SIZE 5.86 BY .88 NO-UNDO.

DEFINE VARIABLE c-dt-docto-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE c-dt-docto-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Data Docto" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE c-nat-oper-fim AS CHARACTER FORMAT "X(6)":U INITIAL "ZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE c-nat-oper-ini AS CHARACTER FORMAT "X(06)":U 
     LABEL "Nat Opera‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE c-nr-doc-fis-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88 NO-UNDO.

DEFINE VARIABLE c-nr-doc-fis-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Nr. Documento" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88 NO-UNDO.

DEFINE VARIABLE c-nr-ord-produ-fim AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE c-nr-ord-produ-ini AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Ord Produ‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE c-nr-patrimonio-fim AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE c-nr-patrimonio-ini AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Nr Patrim“nio" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE c-serie-fim AS CHARACTER FORMAT "X(05)":U INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE c-serie-ini AS CHARACTER FORMAT "X(03)":U 
     LABEL "S‚rie" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE i-bem-fim AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE i-bem-ini AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Bem" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE i-cod-grupo-ini AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "C¢digo do Grupo" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

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

DEFINE IMAGE IMAGE-15
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-16
     FILENAME "image\im-fir":U
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

DEFINE VARIABLE rs-tipo-sel AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Bem", 1,
"Documento", 2 /*,
"Nr Patrim“nio", 3,
"Ord Produ‡Æo", 4*/
     SIZE 31.57 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 1.21.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 10.46.

DEFINE BUTTON bt-arquivo-planilha 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo-planilha AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arquivo Planilha" 
     VIEW-AS FILL-IN 
     SIZE 48.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-data-corte AS DATE FORMAT "99/99/9999":U 
     LABEL "Data de Corte" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE rs-tip-relat AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Resumido", 1,
"Detalhado", 2
     SIZE 12 BY 3 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 2.75.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 23 BY 4.25.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 2.13.

DEFINE VARIABLE l-gera-planilha AS LOGICAL INITIAL no 
     LABEL "Gera planilha?" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.72 BY .83 NO-UNDO.

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
         SIZE 90 BY 17
         FONT 1.

DEFINE FRAME fPage4
     c-data-corte AT ROW 2.5 COL 13.72 COLON-ALIGNED
     l-gera-planilha AT ROW 4.33 COL 3.43
     bt-arquivo-planilha AT ROW 5.17 COL 68 HELP
          "Escolha do nome do arquivo"
     c-arquivo-planilha AT ROW 5.25 COL 17.57 COLON-ALIGNED
     rs-tip-relat AT ROW 8 COL 6 NO-LABEL WIDGET-ID 6
     "Tipo De Relat¢rio" VIEW-AS TEXT
          SIZE 13 BY .54 AT ROW 7.08 COL 3 WIDGET-ID 4
     "Lista Fichas at‚ a Data de Contabiliza‡Æo" VIEW-AS TEXT
          SIZE 29.72 BY .54 AT ROW 1.54 COL 5.29
     RECT-17 AT ROW 1.83 COL 2
     RECT-10 AT ROW 4.04 COL 2
     RECT-11 AT ROW 7.25 COL 2 WIDGET-ID 8
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7 ROW 2.79
         SIZE 76.86 BY 12.46
         FONT 1.

DEFINE FRAME fPage2
     rs-tipo-sel AT ROW 1.46 COL 9.43 NO-LABEL
     c-cod-estabel-ini AT ROW 2.75 COL 20.14 COLON-ALIGNED WIDGET-ID 6
     c-cod-estabel-fim AT ROW 2.75 COL 50 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     i-cod-grupo-ini AT ROW 3.79 COL 20 COLON-ALIGNED
     i-bem-ini AT ROW 4.79 COL 20 COLON-ALIGNED
     i-bem-fim AT ROW 4.79 COL 50 COLON-ALIGNED NO-LABEL
     c-nr-patrimonio-ini AT ROW 5.79 COL 20 COLON-ALIGNED
     c-nr-patrimonio-fim AT ROW 5.79 COL 50 COLON-ALIGNED NO-LABEL
     c-serie-ini AT ROW 6.79 COL 20 COLON-ALIGNED
     c-serie-fim AT ROW 6.79 COL 50 COLON-ALIGNED NO-LABEL
     c-nr-doc-fis-ini AT ROW 7.79 COL 20 COLON-ALIGNED
     c-nr-doc-fis-fim AT ROW 7.79 COL 50 COLON-ALIGNED NO-LABEL
     c-nat-oper-ini AT ROW 8.79 COL 20 COLON-ALIGNED
     c-nat-oper-fim AT ROW 8.79 COL 50 COLON-ALIGNED NO-LABEL
     c-cod-emitente-ini AT ROW 9.79 COL 20 COLON-ALIGNED
     c-cod-emitente-fim AT ROW 9.79 COL 50 COLON-ALIGNED NO-LABEL
     c-dt-docto-ini AT ROW 10.79 COL 20 COLON-ALIGNED
     c-dt-docto-fim AT ROW 10.79 COL 50 COLON-ALIGNED NO-LABEL
     c-nr-ord-produ-ini AT ROW 11.79 COL 20 COLON-ALIGNED
     c-nr-ord-produ-fim AT ROW 11.79 COL 50 COLON-ALIGNED NO-LABEL
     IMAGE-1 AT ROW 6.79 COL 44
     IMAGE-10 AT ROW 9.79 COL 48.72
     IMAGE-11 AT ROW 4.79 COL 44
     IMAGE-12 AT ROW 4.79 COL 48.72
     IMAGE-15 AT ROW 5.79 COL 48.72
     IMAGE-16 AT ROW 5.79 COL 44
     IMAGE-17 AT ROW 11.79 COL 44
     IMAGE-18 AT ROW 11.79 COL 48.72
     IMAGE-2 AT ROW 6.79 COL 48.72
     IMAGE-3 AT ROW 10.79 COL 44
     IMAGE-4 AT ROW 10.79 COL 48.72
     IMAGE-5 AT ROW 7.79 COL 44
     IMAGE-6 AT ROW 7.79 COL 48.72
     IMAGE-7 AT ROW 8.79 COL 44
     IMAGE-8 AT ROW 8.79 COL 48.72
     IMAGE-9 AT ROW 9.79 COL 44
     RECT-14 AT ROW 1.25 COL 2
     RECT-15 AT ROW 2.54 COL 2
     IMAGE-19 AT ROW 2.75 COL 44 WIDGET-ID 10
     IMAGE-20 AT ROW 2.75 COL 48.72 WIDGET-ID 12
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7 ROW 2.79
         SIZE 76.86 BY 12.46
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
         AT COL 7 ROW 2.79
         SIZE 76.86 BY 12.46
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
         HEIGHT             = 17
         WIDTH              = 90
         MAX-HEIGHT         = 30.21
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 30.21
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
       FRAME fPage4:FRAME = FRAME fpage0:HANDLE
       FRAME fPage6:FRAME = FRAME fpage0:HANDLE.

/* SETTINGS FOR FRAME fpage0
   NOT-VISIBLE FRAME-NAME                                               */
/* SETTINGS FOR FRAME fPage2
                                                                        */
/* SETTINGS FOR FILL-IN c-cod-emitente-fim IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-cod-emitente-ini IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-dt-docto-fim IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-dt-docto-ini IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nat-oper-fim IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nat-oper-ini IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nr-doc-fis-fim IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nr-doc-fis-ini IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nr-ord-produ-fim IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nr-ord-produ-ini IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nr-patrimonio-fim IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nr-patrimonio-ini IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-serie-fim IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-serie-ini IN FRAME fPage2
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME fPage4
                                                                        */
/* SETTINGS FOR BUTTON bt-arquivo-planilha IN FRAME fPage4
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-arquivo-planilha IN FRAME fPage4
   NO-ENABLE                                                            */
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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage4
/* Query rebuild information for FRAME fPage4
     _Query            is NOT OPENED
*/  /* FRAME fPage4 */
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


&Scoped-define FRAME-NAME fPage4
&Scoped-define SELF-NAME bt-arquivo-planilha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo-planilha wReport
ON CHOOSE OF bt-arquivo-planilha IN FRAME fPage4
DO:
    def var c-arq-conv  as char no-undo.
    assign c-arq-conv = replace(input frame fPage4 c-arquivo-planilha, "/", CHR(92)).

    SYSTEM-DIALOG GET-FILE c-arq-conv
       FILTERS "*.csv" "*.csv",
               "*.*" "*.*"
       ASK-OVERWRITE 
       DEFAULT-EXTENSION "csv"
       INITIAL-DIR "spool" 
       SAVE-AS
       USE-FILENAME
       UPDATE l-ok.

    if  l-ok = yes then do:
        assign c-arquivo-planilha = replace(c-arq-conv, CHR(92), "/"). 
        display c-arquivo-planilha with frame fPage4.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
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


&Scoped-define FRAME-NAME fPage2
&Scoped-define SELF-NAME c-cod-estabel-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cod-estabel-fim wReport
ON LEAVE OF c-cod-estabel-fim IN FRAME fPage2
DO:
  FIND FIRST ri-estabelecimento NO-LOCK
        WHERE ri-estabelecimento.cod-estabel >= c-cod-estabel-ini:SCREEN-VALUE IN FRAME fPage2 
        AND   ri-estabelecimento.cod-estabel <= c-cod-estabel-fim:SCREEN-VALUE IN FRAME fPage2 NO-ERROR.

    IF AVAIL ri-estabelecimento THEN
        ASSIGN c-data-corte:SCREEN-VALUE IN FRAME fpage4 = STRING(ri-estabelecimento.dat-ult-contab).
    ELSE
        ASSIGN c-data-corte:SCREEN-VALUE IN FRAME fpage4 = STRING(TODAY).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-cod-estabel-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cod-estabel-ini wReport
ON LEAVE OF c-cod-estabel-ini IN FRAME fPage2 /* Estabel */
DO:
    FIND FIRST ri-estabelecimento NO-LOCK
        WHERE ri-estabelecimento.cod-estabel >= c-cod-estabel-ini:SCREEN-VALUE IN FRAME fPage2 
        AND   ri-estabelecimento.cod-estabel <= c-cod-estabel-fim:SCREEN-VALUE IN FRAME fPage2 NO-ERROR.

    IF AVAIL ri-estabelecimento THEN
        ASSIGN c-data-corte:SCREEN-VALUE IN FRAME fpage4 = STRING(ri-estabelecimento.dat-ult-contab). 
    ELSE
        ASSIGN c-data-corte:SCREEN-VALUE IN FRAME fpage4 = STRING(TODAY).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-cod-grupo-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-cod-grupo-ini wReport
ON F5 OF i-cod-grupo-ini IN FRAME fPage2 /* C¢digo do Grupo */
DO:
    {method/zoomfields.i &ProgramZoom="dizoom/z01di460.w"
                         &FieldZoom1="cod-grupo"
                         &FieldScreen1="i-cod-grupo-ini"
                         &Frame1="fPage2"
                         &EnableImplant="NO"}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-cod-grupo-ini wReport
ON MOUSE-SELECT-DBLCLICK OF i-cod-grupo-ini IN FRAME fPage2 /* C¢digo do Grupo */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage4
&Scoped-define SELF-NAME l-gera-planilha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-gera-planilha wReport
ON VALUE-CHANGED OF l-gera-planilha IN FRAME fPage4 /* Gera planilha? */
DO:
    IF l-gera-planilha:SCREEN-VALUE IN FRAME fPage4 = 'yes' THEN DO:
        ASSIGN c-arquivo-planilha:SCREEN-VALUE IN FRAME fPage4 = STRING(SESSION:TEMP-DIRECTORY + 'ri0404.csv').
        ASSIGN c-arquivo-planilha:SENSITIVE     = YES
               bt-arquivo-planilha:SENSITIVE    = YES.
    END.
    ELSE DO:
        ASSIGN  c-arquivo-planilha:SCREEN-VALUE IN FRAME fPage4 = ''.
        ASSIGN c-arquivo-planilha:SENSITIVE     = NO
               bt-arquivo-planilha:SENSITIVE    = NO.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage2
&Scoped-define SELF-NAME rs-tipo-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-tipo-sel wReport
ON VALUE-CHANGED OF rs-tipo-sel IN FRAME fPage2
DO:
    RUN pi-habilitar-campos.

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

/*IF c-cod-estabel:LOAD-MOUSE-POINTER ("Image~\lupa.cur") IN FRAME fPage2 THEN.*/
IF i-cod-grupo-ini:LOAD-MOUSE-POINTER ("Image~\lupa.cur") IN FRAME fPage2 THEN.

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
DEFINE VARIABLE h-bodi460   AS HANDLE     NO-UNDO.
DEFINE VARIABLE i-cod-grupo AS INTEGER    NO-UNDO.

    ASSIGN INPUT FRAME fPage2
           i-bem-ini
           i-bem-fim
           c-nr-patrimonio-ini
           c-nr-patrimonio-fim
           c-serie-ini
           c-serie-fim
           c-nr-doc-fis-ini
           c-nr-doc-fis-fim
           c-nat-oper-ini
           c-nat-oper-fim
           c-cod-emitente-ini
           c-cod-emitente-fim
           c-dt-docto-ini
           c-dt-docto-fim
           c-nr-ord-produ-ini
           c-nr-ord-produ-fim.


    IF NOT VALID-HANDLE(h-bodi460) THEN
        RUN dibo/bodi460.p PERSISTENT SET h-bodi460.

    RUN openQueryStatic IN h-bodi460 (INPUT "Main":U).
    RUN getFirst        IN h-bodi460.
    RUN getIntField     IN h-bodi460 (INPUT  "cod-grupo",
                                      OUTPUT i-cod-grupo).

    ASSIGN i-cod-grupo-ini:SCREEN-VALUE IN FRAME fpage2 = STRING(i-cod-grupo).

    IF VALID-HANDLE(h-bodi460) THEN DO:
        RUN destroy IN h-bodi460.
        ASSIGN h-bodi460 = ?.
    END.

    ASSIGN c-cod-estabel-fim:SCREEN-VALUE IN FRAME fPage2 = "ZZZ".

    APPLY "VALUE-CHANGED" TO rs-tipo-sel IN FRAME fPage2.

    APPLY "LEAVE" TO c-cod-estabel-ini IN FRAME fPage2.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-habilitar-campos wReport 
PROCEDURE pi-habilitar-campos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF rs-tipo-sel:SCREEN-VALUE IN FRAME fPage2  = "1" THEN
        ASSIGN i-bem-ini          :SENSITIVE IN FRAME fPage2 = YES
               i-bem-fim          :SENSITIVE IN FRAME fPage2 = YES
               c-nr-patrimonio-ini:SENSITIVE IN FRAME fPage2 = NO
               c-nr-patrimonio-fim:SENSITIVE IN FRAME fPage2 = NO
               c-serie-ini        :SENSITIVE IN FRAME fPage2 = NO
               c-serie-fim        :SENSITIVE IN FRAME fPage2 = NO
               c-nr-doc-fis-ini   :SENSITIVE IN FRAME fPage2 = NO
               c-nr-doc-fis-fim   :SENSITIVE IN FRAME fPage2 = NO
               c-nat-oper-ini     :SENSITIVE IN FRAME fPage2 = NO
               c-nat-oper-fim     :SENSITIVE IN FRAME fPage2 = NO
               c-cod-emitente-ini :SENSITIVE IN FRAME fPage2 = NO
               c-cod-emitente-fim :SENSITIVE IN FRAME fPage2 = NO
               c-dt-docto-ini     :SENSITIVE IN FRAME fPage2 = NO
               c-dt-docto-fim     :SENSITIVE IN FRAME fPage2 = NO
               c-nr-ord-produ-ini :SENSITIVE IN FRAME fPage2 = NO
               c-nr-ord-produ-fim :SENSITIVE IN FRAME fPage2 = NO
               c-data-corte       :SENSITIVE IN FRAME fPage4 = YES.
    ELSE
        IF rs-tipo-sel:SCREEN-VALUE IN FRAME fPage2  = "2" THEN
            ASSIGN i-bem-ini          :SENSITIVE IN FRAME fPage2 = NO
                   i-bem-fim          :SENSITIVE IN FRAME fPage2 = NO
                   c-nr-patrimonio-ini:SENSITIVE IN FRAME fPage2 = NO
                   c-nr-patrimonio-fim:SENSITIVE IN FRAME fPage2 = NO
                   c-serie-ini        :SENSITIVE IN FRAME fPage2 = YES
                   c-serie-fim        :SENSITIVE IN FRAME fPage2 = YES
                   c-nr-doc-fis-ini   :SENSITIVE IN FRAME fPage2 = YES
                   c-nr-doc-fis-fim   :SENSITIVE IN FRAME fPage2 = YES
                   c-nat-oper-ini     :SENSITIVE IN FRAME fPage2 = YES
                   c-nat-oper-fim     :SENSITIVE IN FRAME fPage2 = YES
                   c-cod-emitente-ini :SENSITIVE IN FRAME fPage2 = YES
                   c-cod-emitente-fim :SENSITIVE IN FRAME fPage2 = YES
                   c-dt-docto-ini     :SENSITIVE IN FRAME fPage2 = YES
                   c-dt-docto-fim     :SENSITIVE IN FRAME fPage2 = YES
                   c-nr-ord-produ-ini :SENSITIVE IN FRAME fPage2 = NO
                   c-nr-ord-produ-fim :SENSITIVE IN FRAME fPage2 = NO
                   c-data-corte       :SENSITIVE IN FRAME fPage4 = YES.
        ELSE
            IF rs-tipo-sel:SCREEN-VALUE IN FRAME fPage2  = "3" THEN
                ASSIGN i-bem-ini          :SENSITIVE IN FRAME fPage2 = NO
                       i-bem-fim          :SENSITIVE IN FRAME fPage2 = NO
                       c-nr-patrimonio-ini:SENSITIVE IN FRAME fPage2 = YES
                       c-nr-patrimonio-fim:SENSITIVE IN FRAME fPage2 = YES
                       c-serie-ini        :SENSITIVE IN FRAME fPage2 = NO
                       c-serie-fim        :SENSITIVE IN FRAME fPage2 = NO
                       c-nr-doc-fis-ini   :SENSITIVE IN FRAME fPage2 = NO
                       c-nr-doc-fis-fim   :SENSITIVE IN FRAME fPage2 = NO
                       c-nat-oper-ini     :SENSITIVE IN FRAME fPage2 = NO
                       c-nat-oper-fim     :SENSITIVE IN FRAME fPage2 = NO
                       c-cod-emitente-ini :SENSITIVE IN FRAME fPage2 = NO
                       c-cod-emitente-fim :SENSITIVE IN FRAME fPage2 = NO
                       c-dt-docto-ini     :SENSITIVE IN FRAME fPage2 = NO
                       c-dt-docto-fim     :SENSITIVE IN FRAME fPage2 = NO
                       c-nr-ord-produ-ini :SENSITIVE IN FRAME fPage2 = NO
                       c-nr-ord-produ-fim :SENSITIVE IN FRAME fPage2 = NO
                       c-data-corte       :SENSITIVE IN FRAME fPage4 = YES.
            ELSE
              IF rs-tipo-sel:SCREEN-VALUE IN FRAME fPage2  = "4" THEN
                  ASSIGN i-bem-ini          :SENSITIVE IN FRAME fPage2 = NO
                         i-bem-fim          :SENSITIVE IN FRAME fPage2 = NO
                         c-nr-patrimonio-ini:SENSITIVE IN FRAME fPage2 = NO
                         c-nr-patrimonio-fim:SENSITIVE IN FRAME fPage2 = NO
                         c-serie-ini        :SENSITIVE IN FRAME fPage2 = NO
                         c-serie-fim        :SENSITIVE IN FRAME fPage2 = NO
                         c-nr-doc-fis-ini   :SENSITIVE IN FRAME fPage2 = NO
                         c-nr-doc-fis-fim   :SENSITIVE IN FRAME fPage2 = NO
                         c-nat-oper-ini     :SENSITIVE IN FRAME fPage2 = NO
                         c-nat-oper-fim     :SENSITIVE IN FRAME fPage2 = NO
                         c-cod-emitente-ini :SENSITIVE IN FRAME fPage2 = NO
                         c-cod-emitente-fim :SENSITIVE IN FRAME fPage2 = NO
                         c-dt-docto-ini     :SENSITIVE IN FRAME fPage2 = NO
                         c-dt-docto-fim     :SENSITIVE IN FRAME fPage2 = NO
                         c-nr-ord-produ-ini :SENSITIVE IN FRAME fPage2 = YES
                         c-nr-ord-produ-fim :SENSITIVE IN FRAME fPage2 = YES
                         c-data-corte       :SENSITIVE IN FRAME fPage4 = YES.
      IF rs-tipo-sel:SCREEN-VALUE IN FRAME fPage2  = "1" THEN
           rs-tip-relat:DISABLE("Resumido") IN FRAME fPage4.
      ELSE rs-tip-relat:ENABLE("Resumido") IN FRAME fPage4.

    DISPLAY i-bem-ini
            i-bem-fim
            c-nr-patrimonio-ini
            c-nr-patrimonio-fim
            c-serie-ini
            c-serie-fim
            c-nr-doc-fis-ini
            c-nr-doc-fis-fim
            c-nat-oper-ini
            c-nat-oper-fim
            c-cod-emitente-ini
            c-cod-emitente-fim
            c-dt-docto-ini
            c-dt-docto-fim
            c-nr-ord-produ-ini
            c-nr-ord-produ-fim
            WITH FRAME fPage2.

    RETURN "OK":U.
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
    /*IF NOT CAN-FIND(FIRST ri-estabelecimento NO-LOCK 
                    WHERE ri-estabelecimento.cod-estabel = INPUT FRAME fpage2 c-cod-estabel) THEN DO:

        RUN utp/ut-msgs.p (INPUT "show":U,
                           INPUT 2,
                           INPUT "Estabelecimento").
        APPLY 'ENTRY':U TO c-cod-estabel IN FRAME fpage2.
        RETURN NO-APPLY.
    END.*/

    IF NOT CAN-FIND(FIRST ri-grupos NO-LOCK WHERE
                          ri-grupos.cod-grupo = INPUT FRAME fpage2 i-cod-grupo-ini)THEN DO:

        RUN utp/ut-msgs.p (INPUT "show":U,
                           INPUT 2,
                           INPUT "Grupo").
        APPLY 'ENTRY':U TO i-cod-grupo-ini IN FRAME fpage2.
        RETURN NO-APPLY.
    END.

    /*DescomentarFOR EACH ri-estabelecimento NO-LOCK 
        WHERE ri-estabelecimento.cod-estabel >= INPUT FRAME fpage2 c-cod-estabel-ini
        AND   ri-estabelecimento.cod-estabel >= INPUT FRAME fpage2 c-cod-estabel-fim:            
        IF NOT CAN-FIND(FIRST ri-estab-grupos NO-LOCK 
                        WHERE ri-estab-grupos.cod-grupo   = INPUT FRAME fpage2 i-cod-grupo-ini
                          AND ri-estab-grupos.cod-estabel = ri-estabelecimento.cod-estabe )THEN DO:
    
            RUN utp/ut-msgs.p (INPUT "show":U,
                               INPUT 2,
                               INPUT "Grupo x Estabelecimento").
            APPLY 'ENTRY':U TO i-cod-grupo-ini IN FRAME fpage2.
            RETURN NO-APPLY.
        END.
    END.*/
    IF REPLACE(c-data-corte:SCREEN-VALUE IN FRAME fPage4, "/","") = ""
    OR (c-data-corte:SCREEN-VALUE IN FRAME fPage4) = "?" THEN DO:

        run utp/ut-msgs.p (input "show",
                           input  17006,
                           input "Data de Corte em branco." + "~~" +
                                 "O campo Data de Corte esta em branco e deve ser informado.").
        APPLY "ENTRY" TO c-data-corte IN FRAME fPage4.
        RETURN NO-APPLY.
    END.

    FOR EACH  ri-estabelecimento NO-LOCK
        WHERE ri-estabelecimento.cod-estabel >= c-cod-estabel-ini:SCREEN-VALUE IN FRAME fPage2 
        AND   ri-estabelecimento.cod-estabel <= c-cod-estabel-fim:SCREEN-VALUE IN FRAME fPage2:

        IF DATE(c-data-corte:SCREEN-VALUE IN FRAME fPage4) > ri-estabelecimento.dat-ult-contab THEN DO:
    
            run utp/ut-msgs.p (input "show",
                               input  17006,
                               input "Data de Corte maior que a data da £ltima contabiliza‡Æo." + "~~" +
                                     "O campo Data de Corte esta maior que a data da £ltima contabiliza‡Æo do estabelecimento " + ri-estabelecimento.cod-estabel + ". A Data de Corte deve ser alterada.").
            APPLY "ENTRY" TO c-data-corte IN FRAME fPage4.
            RETURN NO-APPLY.
        END.
    END.

    create tt-param.
    assign tt-param.usuario           = c-seg-usuario
           tt-param.destino           = INPUT FRAME fPage6 rsDestiny
           tt-param.data-exec         = TODAY
           tt-param.hora-exec         = TIME
           tt-param.tipo-sel          = INPUT FRAME fpage2 rs-tipo-sel
           tt-param.cod-estabel-ini   = INPUT FRAME fpage2 c-cod-estabel-ini
           tt-param.cod-estabel-fim   = INPUT FRAME fpage2 c-cod-estabel-fim
           tt-param.i-tip-relat       = INPUT FRAME fPage4 rs-tip-relat
           tt-param.cod-grupo-ini     = INPUT FRAME fpage2 i-cod-grupo-ini
           tt-param.bem-ini           = INPUT FRAME fpage2 i-bem-ini
           tt-param.bem-fim           = INPUT FRAME fpage2 i-bem-fim
           tt-param.nr-patrimonio-ini = INPUT FRAME fpage2 c-nr-patrimonio-ini
           tt-param.nr-patrimonio-fim = INPUT FRAME fpage2 c-nr-patrimonio-fim
           tt-param.serie-ini         = INPUT FRAME fpage2 c-serie-ini
           tt-param.serie-fim         = INPUT FRAME fpage2 c-serie-fim
           tt-param.nr-doc-fis-ini    = INPUT FRAME fpage2 c-nr-doc-fis-ini
           tt-param.nr-doc-fis-fim    = INPUT FRAME fpage2 c-nr-doc-fis-fim
           tt-param.nat-oper-ini      = INPUT FRAME fpage2 c-nat-oper-ini
           tt-param.nat-oper-fim      = INPUT FRAME fpage2 c-nat-oper-fim
           tt-param.cod-emitente-ini  = INPUT FRAME fpage2 c-cod-emitente-ini
           tt-param.cod-emitente-fim  = INPUT FRAME fpage2 c-cod-emitente-fim
           tt-param.dt-docto-ini      = INPUT FRAME fpage2 c-dt-docto-ini
           tt-param.dt-docto-fim      = INPUT FRAME fpage2 c-dt-docto-fim
           tt-param.nr-ord-produ-ini  = INPUT FRAME fpage2 c-nr-ord-produ-ini
           tt-param.nr-ord-produ-fim  = INPUT FRAME fpage2 c-nr-ord-produ-fim
           tt-param.dt-corte          = INPUT FRAME fpage4 c-data-corte
           tt-param.l-gera-planilha   = INPUT FRAME fpage4 l-gera-planilha
           tt-param.c-arq-planilha    = INPUT FRAME fpage4 c-arquivo-planilha.



    if tt-param.destino = 1 
    then 
        assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame fPage6 cFile.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.

    {report/rpexb.i}

    SESSION:SET-WAIT-STATE("GENERAL":U).

    {report/rprun.i esp/esri0404rp.p}

    {report/rpexc.i}

    SESSION:SET-WAIT-STATE("":U).

    {report/rptrm.i}
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


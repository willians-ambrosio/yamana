&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wReport 
/*:T*******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESCC0396 2.00.00.000 } /*** 010000 ***/

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ESCC0396 MCC}
&ENDIF

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESCC0396
&GLOBAL-DEFINE Version        2.00.00.000

&GLOBAL-DEFINE Folder         YES
&GLOBAL-DEFINE InitialPage    1
&GLOBAL-DEFINE FolderLabels   Seleá∆o,ParÉmetros,Impress∆o

&GLOBAL-DEFINE PGLAY          NO
&GLOBAL-DEFINE PGSEL          YES
&GLOBAL-DEFINE PGCLA          NO
&GLOBAL-DEFINE PGPAR          YES
&GLOBAL-DEFINE PGDIG          NO
&GLOBAL-DEFINE PGIMP          YES
&GLOBAL-DEFINE PGLOG          NO

&GLOBAL-DEFINE RTF            YES

&GLOBAL-DEFINE page0Widgets   btOk ~
                              btCancel ~
                              btHelp2
&GLOBAL-DEFINE page2Widgets   iPedidoIni iPedidoFim daDataIni daDataFim iEmitIni iEmitFim ~
                              cEstabIni cEstabFim cRespIni cRespFim ~
                              iOrdIni iOrdFim iParcIni iParcFim daDataEntrIni daDataEntrFim

&GLOBAL-DEFINE page4Widgets   tg-zera-saldo tg-atualiza-situacao tg-atualiza-parcela-lote tg-ngera-pendencia tg-simula ~
                              tg-atualiza-parcela-lote-min fi-intervalo-validacao-ini fi-intervalo-validacao-fim

&GLOBAL-DEFINE page6Widgets   rsDestiny ~
                              btConfigImpr ~
                              btFile ~
                              rsExecution ~
                              l-habilitaRtf ~
                              btModelRtf

&GLOBAL-DEFINE page4Text      
&GLOBAL-DEFINE page6Text      text-destino text-modo text-rtf text-ModelRtf

&GLOBAL-DEFINE page2Fields    
&GLOBAL-DEFINE page4Fields    
&GLOBAL-DEFINE page6Fields    cFile cModelRTF

/* Parameters Definitions ---                                           */
{esp/ccp/escc0396.i} /* tt-param */
{include/i_dbtype.i}
/* Transfer Definitions */
def var raw-param        as raw no-undo.

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-rtf              as char    no-undo.
def var c-arq-layout       as char    no-undo.      
def var c-arq-temp         as char    no-undo.
DEF VAR c-modelo-default   AS CHAR    NO-UNDO.

def stream s-imp.

/*15/02/2005 - tech1007 - Variavel definida para tratar se o programa est† rodando no WebEnabler*/
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

DEFINE VARIABLE cEstabFIm AS CHARACTER FORMAT "x(5)" INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE cEstabIni AS CHARACTER FORMAT "x(5)" 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE cRespFim AS CHARACTER FORMAT "X(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 16.14 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE cRespIni AS CHARACTER FORMAT "X(12)" 
     LABEL "Respons†vel" 
     VIEW-AS FILL-IN 
     SIZE 16.14 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE daDataEntrFim AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE daDataEntrIni AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Data Entrega" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE daDataFim AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE daDataIni AS DATE FORMAT "99/99/9999" 
     LABEL "Data" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE iEmitFim AS INTEGER FORMAT ">>>>>>>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE iEmitIni AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Fornecedor" 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE iOrdFim AS INTEGER FORMAT "zzzzz9,99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE iOrdIni AS INTEGER FORMAT "zzzzz9,99" INITIAL 0 
     LABEL "Ordem" 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE iParcFim AS INTEGER FORMAT ">>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE iParcIni AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Parcela" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE iPedidoFim AS INTEGER FORMAT ">>>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE iPedidoIni AS INTEGER FORMAT ">>>>>,>>9" INITIAL 0 
     LABEL "Pedido" 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88
     FONT 1 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-63
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-64
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-65
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-66
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-67
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-68
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-69
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-70
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-71
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-72
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-75
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-76
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-77
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-78
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-68
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82 BY 5.29.

DEFINE RECTANGLE RECT-69
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82 BY 3.88.

DEFINE VARIABLE fi-intervalo-validacao-fim AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 9.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-intervalo-validacao-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Intervalo de validaá∆o para datas das parcelas" 
     VIEW-AS FILL-IN 
     SIZE 9.43 BY .88 NO-UNDO.

DEFINE VARIABLE text-aprovacao AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 8.86 BY .67 NO-UNDO.

DEFINE IMAGE IMAGE-79
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-80
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-70
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82 BY 5.46.

DEFINE RECTANGLE RECT-71
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82 BY 1.83.

DEFINE RECTANGLE RECT-72
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82 BY 1.25.

DEFINE VARIABLE tg-atualiza-parcela-lote AS LOGICAL INITIAL no 
     LABEL "Atualiza parcela com quantidade diferente do lote m£ltiplo" 
     VIEW-AS TOGGLE-BOX
     SIZE 60 BY .83 NO-UNDO.

DEFINE VARIABLE tg-atualiza-parcela-lote-min AS LOGICAL INITIAL yes 
     LABEL "Atualiza parcela com quantidade menor que o lote m°nimo" 
     VIEW-AS TOGGLE-BOX
     SIZE 60 BY .83 NO-UNDO.

DEFINE VARIABLE tg-atualiza-situacao AS LOGICAL INITIAL yes 
     LABEL "Atualiza situaá∆o para recebida" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .83 NO-UNDO.

DEFINE VARIABLE tg-ngera-pendencia AS LOGICAL INITIAL yes 
     LABEL "N∆o gera pendància de aprovaá∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .83 NO-UNDO.

DEFINE VARIABLE tg-simula AS LOGICAL INITIAL yes 
     LABEL "Simular" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .83 NO-UNDO.

DEFINE VARIABLE tg-zera-saldo AS LOGICAL INITIAL yes 
     LABEL "Zera saldo" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .83 NO-UNDO.

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

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execuá∆o" 
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
         SIZE 90 BY 17
         FONT 1.

DEFINE FRAME fPage4
     tg-zera-saldo AT ROW 1.67 COL 3
     tg-atualiza-situacao AT ROW 2.5 COL 3
     tg-atualiza-parcela-lote-min AT ROW 3.5 COL 3
     tg-atualiza-parcela-lote AT ROW 4.5 COL 3
     fi-intervalo-validacao-ini AT ROW 5.5 COL 33.57 COLON-ALIGNED
     fi-intervalo-validacao-fim AT ROW 5.5 COL 51.14 COLON-ALIGNED NO-LABEL
     tg-ngera-pendencia AT ROW 8.04 COL 3
     tg-simula AT ROW 9.75 COL 3
     text-aprovacao AT ROW 7.25 COL 1 COLON-ALIGNED NO-LABEL
     RECT-70 AT ROW 1.29 COL 2
     RECT-71 AT ROW 7.46 COL 2
     RECT-72 AT ROW 9.54 COL 2
     IMAGE-79 AT ROW 5.5 COL 45.29
     IMAGE-80 AT ROW 5.5 COL 50.14
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.79
         SIZE 84.43 BY 10.15
         FONT 1.

DEFINE FRAME fPage2
     iPedidoIni AT ROW 1.5 COL 17.43 HELP
          "N£mero do Pedido de Compra"
     iPedidoFim AT ROW 1.5 COL 48 HELP
          "N£mero do Pedido de Compra" NO-LABEL
     daDataIni AT ROW 2.5 COL 18.86
     daDataFim AT ROW 2.5 COL 48 NO-LABEL
     iEmitIni AT ROW 3.5 COL 14.43
     iEmitFim AT ROW 3.5 COL 48 NO-LABEL
     cEstabIni AT ROW 4.5 COL 11 HELP
          "Estabelecimento do Pedido"
     cEstabFIm AT ROW 4.5 COL 48 HELP
          "Estabelecimento do Pedido" NO-LABEL
     cRespIni AT ROW 5.5 COL 13.28 HELP
          "Respons†vel"
     cRespFim AT ROW 5.5 COL 48 HELP
          "Respons†vel" NO-LABEL
     iOrdIni AT ROW 7 COL 17.71
     iOrdFim AT ROW 7 COL 48 NO-LABEL
     iParcIni AT ROW 8 COL 17 HELP
          "Parcela da Ordem de Compra"
     iParcFim AT ROW 8 COL 48 HELP
          "Parcela da Ordem de Compra" NO-LABEL
     daDataEntrIni AT ROW 9 COL 13.14
     daDataEntrFim AT ROW 9 COL 48 NO-LABEL
     IMAGE-1 AT ROW 1.5 COL 40
     IMAGE-2 AT ROW 1.5 COL 45
     IMAGE-63 AT ROW 2.5 COL 40
     IMAGE-64 AT ROW 2.5 COL 45
     IMAGE-65 AT ROW 3.5 COL 40
     IMAGE-66 AT ROW 3.5 COL 45
     IMAGE-67 AT ROW 4.5 COL 40
     IMAGE-68 AT ROW 4.5 COL 45
     IMAGE-69 AT ROW 5.5 COL 40
     IMAGE-70 AT ROW 5.5 COL 45
     IMAGE-71 AT ROW 7 COL 40
     IMAGE-72 AT ROW 7 COL 45
     IMAGE-75 AT ROW 8 COL 40
     IMAGE-76 AT ROW 8 COL 45
     IMAGE-77 AT ROW 9 COL 40
     IMAGE-78 AT ROW 9 COL 45
     RECT-68 AT ROW 1.25 COL 2
     RECT-69 AT ROW 6.67 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.79
         SIZE 84.43 BY 10.21
         FONT 1.

DEFINE FRAME fPage6
     rsDestiny AT ROW 2.38 COL 3.14 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     cFile AT ROW 3.63 COL 3.14 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     btFile AT ROW 3.5 COL 43 HELP
          "Escolha do nome do arquivo"
     btConfigImpr AT ROW 3.5 COL 43 HELP
          "Configuraá∆o da impressora"
     l-habilitaRtf AT ROW 5.58 COL 3.14
     cModelRTF AT ROW 7.29 COL 3 HELP
          "Nome do arquivo de modelo" NO-LABEL
     btModelRtf AT ROW 7.29 COL 43 HELP
          "Escolha o arquivo de modelo"
     rsExecution AT ROW 9.5 COL 2.86 HELP
          "Modo de Execuá∆o" NO-LABEL
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
         TITLE              = ""
         HEIGHT             = 17
         WIDTH              = 90
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 114.14
         VIRTUAL-HEIGHT     = 22
         VIRTUAL-WIDTH      = 114.14
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
/* SETTINGS FOR FILL-IN cEstabFIm IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN cEstabIni IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN cRespFim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN cRespIni IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN daDataEntrFim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN daDataEntrIni IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN daDataFim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN daDataIni IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN iEmitFim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN iEmitIni IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN iOrdFim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN iOrdIni IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN iParcFim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN iParcIni IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN iPedidoFim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN iPedidoIni IN FRAME fPage2
   ALIGN-L                                                              */
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
                "Execuá∆o".

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
        /*Alterado 15/02/2005 - tech1007 - Condiá∆o removida pois RTF n∆o Ç mais um destino
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


&Scoped-define FRAME-NAME fPage4
&Scoped-define SELF-NAME tg-ngera-pendencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-ngera-pendencia wReport
ON VALUE-CHANGED OF tg-ngera-pendencia IN FRAME fPage4 /* N∆o gera pendància de aprovaá∆o */
DO:
  IF NOT tg-ngera-pendencia:CHECKED THEN
     run utp/ut-msgs.p (input "show",
                             input 28311,
                             input "Atená∆o!~~ Cuidado ser† gerado pendencia de aprovaá∆o. Deseja realmente gerar a pendància de aprovaá∆o?"  ).
    if return-value = "no" then
       ASSIGN tg-ngera-pendencia:CHECKED = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wReport 


/*:T--- L¢gica para inicializaá∆o do programam ---*/
{report/mainblock.i}
{utp/ut-liter.i "Pedido"}
ASSIGN iPedidoIni:LABEL IN FRAME fPage2 = RETURN-VALUE.
{utp/ut-liter.i "Data"}
ASSIGN daDataIni:LABEL IN FRAME fPage2 = RETURN-VALUE.
{utp/ut-liter.i "Fornecedor"}
ASSIGN iEmitIni:LABEL IN FRAME fPage2 = RETURN-VALUE.
{utp/ut-liter.i "Estabelecimento"}
ASSIGN cEstabIni:LABEL IN FRAME fPage2 = RETURN-VALUE.
{utp/ut-liter.i "Respons†vel"}
ASSIGN cRespIni:LABEL IN FRAME fPage2 = RETURN-VALUE.
{utp/ut-liter.i "Ordem"}
ASSIGN iOrdIni:LABEL IN FRAME fPage2 = RETURN-VALUE.
{utp/ut-liter.i "Parcela"}
ASSIGN iParcIni:LABEL IN FRAME fPage2 = RETURN-VALUE.
{utp/ut-liter.i "Data Entrega"}
ASSIGN daDataEntrIni:LABEL IN FRAME fPage2 = RETURN-VALUE.

{utp/ut-liter.i "Zera saldo"}
ASSIGN tg-zera-saldo:LABEL IN FRAME fPage4 = RETURN-VALUE.
{utp/ut-liter.i "Atualiza situaá∆o para recebida"}
ASSIGN tg-atualiza-situacao:LABEL IN FRAME fPage4 = RETURN-VALUE.
{utp/ut-liter.i "Atualiza parcela com quantidade menor que o lote m°nimo"}
ASSIGN tg-atualiza-parcela-lote-min:LABEL IN FRAME fPage4 = RETURN-VALUE.
{utp/ut-liter.i "Atualiza parcela com quantidade diferente do lote m£ltiplo"}
ASSIGN tg-atualiza-parcela-lote:LABEL IN FRAME fPage4 = RETURN-VALUE.
{utp/ut-liter.i "Intervalo de validaá∆o para datas das parcelas"}
ASSIGN fi-intervalo-validacao-ini:LABEL IN FRAME fPage4 = RETURN-VALUE.

{utp/ut-liter.i "Aprovaá∆o"}
ASSIGN text-aprovacao:SCREEN-VALUE IN FRAME fPage4 = RETURN-VALUE.
{utp/ut-liter.i "N∆o gera pendància de aprovaá∆o"}
ASSIGN tg-ngera-pendencia:LABEL IN FRAME fPage4 = RETURN-VALUE.
{utp/ut-liter.i "Simular"}
ASSIGN tg-simula:LABEL IN FRAME fPage4 = RETURN-VALUE.

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
    /*Alterado 17/02/2005 - tech1007 - Foi criado essa procedure para que seja realizado a inicializaá∆o
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
    
    ASSIGN daDataIni = &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 01/01/001  &ENDIF
           fi-intervalo-validacao-ini = TODAY
           fi-intervalo-validacao-fim = 12/31/9999.
    DISPLAY daDataIni WITH FRAME fPage2. 
    DISPLAY fi-intervalo-validacao-ini fi-intervalo-validacao-fim WITH FRAME fPage4.
    
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

define var r-tt-digita as rowid no-undo.

/*:T** Relatorio ***/
do on error undo, return error on stop  undo, return error:
    {report/rpexa.i}

    /*15/02/2005 - tech1007 - Teste alterado pois RTF n∆o Ç mais opá∆o de Destino*/
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
    
    /*:T Coloque aqui as validaá‰es da p†gina de Digitaá∆o, lembrando que elas devem
       apresentar uma mensagem de erro cadastrada, posicionar nesta p†gina e colocar
       o focus no campo com problemas */
    /*browse brDigita:SET-REPOSITIONED-ROW (browse brDigita:DOWN, "ALWAYS":U).*/
    
    
    /*:T Coloque aqui as validaá‰es das outras p†ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p†gina com 
       problemas e colocar o focus no campo com problemas */
    
    
    
    /*:T Aqui s∆o gravados os campos da temp-table que ser† passada como parÉmetro
       para o programa RP.P */
    
    create tt-param.
    assign tt-param.usuario          = c-seg-usuario
           tt-param.destino          = input frame fPage6 rsDestiny
           tt-param.data-exec        = today
           tt-param.hora-exec        = time
           tt-param.execution        = INPUT FRAME fPage6 rsExecution
           &IF "{&RTF}":U = "YES":U &THEN
           tt-param.modelo           = INPUT FRAME fPage6 cModelRTF
           tt-param.l-habilitaRtf    = INPUT FRAME fPage6 l-habilitaRtf
           &endif
           /*****************/         
           tt-param.iPedidoIni           = INPUT FRAME fPage2 iPedidoIni
           tt-param.iPedidoFim           = INPUT FRAME fPage2 iPedidoFim
           tt-param.daDataIni            = INPUT FRAME fPage2 daDataIni 
           tt-param.daDataFim            = INPUT FRAME fPage2 daDataFim 
           tt-param.iEmitenteIni         = INPUT FRAME fPage2 iEmitIni    
           tt-param.iEmitenteFim         = INPUT FRAME fPage2 iEmitFim    
           tt-param.cEstabelecIni        = INPUT FRAME fPage2 cEstabIni   
           tt-param.cEstabelecFim        = INPUT FRAME fPage2 cEstabFim   
           tt-param.cResponsavelIni      = INPUT FRAME fPage2 cRespIni 
           tt-param.cResponsavelFim      = INPUT FRAME fPage2 cRespFim 
           tt-param.iOrdemIni            = INPUT FRAME fPage2 iOrdIni       
           tt-param.iOrdemFim            = INPUT FRAME fPage2 iOrdFim       
           tt-param.iParcelaIni          = INPUT FRAME fPage2 iParcIni     
           tt-param.iParcelaFim          = INPUT FRAME fPage2 iParcFim     
           tt-param.daDataEntregaIni     = INPUT FRAME fPage2 daDataEntrIni
           tt-param.daDataEntregaFim     = INPUT FRAME fPage2 daDataEntrFim
           tt-param.lZeraSaldo           = INPUT FRAME fPage4 tg-zera-saldo
           tt-param.lAtualizaSituacao    = INPUT FRAME fPage4 tg-atualiza-situacao
           tt-param.lAtualizaParcLote    = INPUT FRAME fPage4 tg-atualiza-parcela-lote
           tt-param.lAtualizaParcLoteMin = INPUT FRAME fPage4 tg-atualiza-parcela-lote-min
           tt-param.lNGeraPend           = INPUT FRAME fPage4 tg-ngera-pendencia
           tt-param.lSimula              = INPUT FRAME fPage4 tg-simula
           tt-param.daIntervaloIni       = INPUT FRAME fPage4 fi-intervalo-validacao-ini
           tt-param.daIntervaloFim       = INPUT FRAME fPage4 fi-intervalo-validacao-fim
    .

    if tt-param.destino = 1 
    then 
        assign tt-param.arquivo = "":U.
    else if  tt-param.destino = 2 
        then assign tt-param.arquivo = input frame fPage6 cFile.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    
    /*:T Coloque aqui a l¢gica de gravaá∆o dos demais campos que devem ser passados
       como parÉmetros para o programa RP.P, atravÇs da temp-table tt-param */
    
    
    
    /*:T Executar do programa RP.P que ir† criar o relat¢rio */
    {report/rpexb.i}
    
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    {report/rprun.i esp/ccp/escc0396rp.p}
    
    {report/rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
    {report/rptrm.i}
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


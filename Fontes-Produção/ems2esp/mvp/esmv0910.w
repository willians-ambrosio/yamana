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
{include/i-prgvrs.i ESMV0910 2.06.00.000}

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        ESMV0910
&GLOBAL-DEFINE Version        2.06.00.000

&GLOBAL-DEFINE Folder         YES
&GLOBAL-DEFINE InitialPage    1
&GLOBAL-DEFINE FolderLabels   Seleá∆o,ParÉmetros,Impress∆o

&GLOBAL-DEFINE PGLAY          NO
&GLOBAL-DEFINE PGSEL          YES
&GLOBAL-DEFINE PGCLA          NO
&GLOBAL-DEFINE PGPAR          yes
&GLOBAL-DEFINE PGDIG          no
&GLOBAL-DEFINE PGIMP          YES
&GLOBAL-DEFINE PGLOG          NO

&GLOBAL-DEFINE page0Widgets   btOk ~
                              btCancel ~
                              btHelp2
&GLOBAL-DEFINE page1Widgets   
&GLOBAL-DEFINE page2Widgets   fi-periodo-ini fi-periodo-fim fi-ordem-ini fi-ordem-fim ~
                              fi-tecnico-ini fi-tecnico-fim fi-emp-ini fi-emp-fim fi-eqpto-ini ~
                              fi-eqpto-fim fi-req-ini fi-req-fim
&GLOBAL-DEFINE page3Widgets   
&GLOBAL-DEFINE page4Widgets   tgFrotas tgMI tgAberta tgFechada tgPendente tgCom tgAprov tgNaoAprov ~
                              tgReq tgSolic fi-text fi-text2 fi-text3 fi-text4
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
&GLOBAL-DEFINE page2Fields    
&GLOBAL-DEFINE page3Fields    
&GLOBAL-DEFINE page4Fields    
&GLOBAL-DEFINE page5Fields    
&GLOBAL-DEFINE page6Fields    cFile
&GLOBAL-DEFINE page7Fields    
&GLOBAL-DEFINE page8Fields    

/* Parameters Definitions ---                                           */
define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)":U
    field usuario          as char format "x(12)":U
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)":U
    field modelo           as char format "x(35)":U
    field periodo-ini      as date format "99/99/9999":U
    field periodo-fim      as date format "99/99/9999":U
    field ordem-ini        like ord-manut.nr-ord-produ
    field ordem-fim        like ord-manut.nr-ord-produ
    field tecnico-ini      like tecn-mi.cd-tecnico
    field tecnico-fim      like tecn-mi.cd-tecnico
    field emp-ini          as integer format ">>9" 
    field emp-fim          as integer format ">>9" 
    field eqpto-ini        like ord-manut.cd-equipto
    field eqpto-fim        like ord-manut.cd-equipto
    field req-ini          like req-ord-produc.nr-requisicao
    field req-fim          like req-ord-produc.nr-requisicao
    field lFrotas          as logical
    field lMI              as logical
    field lAberta          as logical
    field lFechada         as logical
    field lPendente        as logical
    field lCom             as logical
    field lAprov           as logical
    field lNaoAprov        as logical
    field lReq             as logical
    field lSolic           as logical.

def var raw-param          as raw     no-undo.
def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-arq-layout       as char    no-undo.      
def var c-arq-temp         as char    no-undo.
DEF VAR c-modelo-default   AS CHAR    NO-UNDO.
def var c-lixo             as char format "x(16)".

def stream s-imp.

/*15/02/2005 - tech1007 - Variavel definida para tratar se o programa est† rodando no WebEnabler*/
DEFINE SHARED VARIABLE hWenController AS HANDLE NO-UNDO.

def temp-table tt-digita no-undo
    field linha         as integer
    field conteudo      as character format "x(80)"
    field i-ep-codigo   like mab-eqpto.ep-codigo
    field c-cd-equipto  like mab-eqpto.cod-eqpto
    field c-descricao   like mab-model.des-model
    field l-sucesso     as log initial yes.

define buffer b-tt-digita for tt-digita.

def temp-table tt-raw-digita
   field raw-digita      as raw.

def stream s-imp.

DEFINE NEW GLOBAL SHARED VARIABLE adm-broker-hdl AS HANDLE NO-UNDO.

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

DEFINE VARIABLE fi-emp-fim AS INTEGER FORMAT ">>>":U INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-emp-ini AS INTEGER FORMAT ">>>":U INITIAL 0 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-eqpto-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 18.29 BY .88 NO-UNDO.

DEFINE VARIABLE fi-eqpto-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Equipamento" 
     VIEW-AS FILL-IN 
     SIZE 18.29 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ordem-fim AS INTEGER FORMAT ">>>,>>>,>>>":U INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ordem-ini AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Ordem" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-periodo-fim AS DATE FORMAT "99/99/9999":U INITIAL 01/01/1800 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-periodo-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/1800 
     LABEL "Periodo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-req-fim AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-req-ini AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Requisiá∆o" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tecnico-fim AS CHARACTER FORMAT "X(6)":U INITIAL "999999" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tecnico-ini AS CHARACTER FORMAT "X(6)":U 
     LABEL "Tecnico" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
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
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-21
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-22
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-23
     FILENAME "image\im-las":U
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

DEFINE VARIABLE fi-text AS CHARACTER FORMAT "X(256)":U INITIAL "Tipo Ordem" 
      VIEW-AS TEXT 
     SIZE 9 BY .67 NO-UNDO.

DEFINE VARIABLE fi-text2 AS CHARACTER FORMAT "X(256)":U INITIAL "Situaá∆o Requisiá∆o / Solicitaá∆o" 
      VIEW-AS TEXT 
     SIZE 26 BY .67 NO-UNDO.

DEFINE VARIABLE fi-text3 AS CHARACTER FORMAT "X(256)":U INITIAL "Estado Requisiá∆o / Solicitaá∆o" 
      VIEW-AS TEXT 
     SIZE 22.14 BY .67 NO-UNDO.

DEFINE VARIABLE fi-text4 AS CHARACTER FORMAT "X(256)":U INITIAL "Tipo Requisiá∆o" 
      VIEW-AS TEXT 
     SIZE 12 BY .67 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77.43 BY 2.25.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38.57 BY 5.5.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37.86 BY 2.79.

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 2.46.

DEFINE VARIABLE tgAberta AS LOGICAL INITIAL yes 
     LABEL "Aberta" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tgAprov AS LOGICAL INITIAL yes 
     LABEL "Aprovada" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tgCom AS LOGICAL INITIAL yes 
     LABEL "Com Ordem" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tgFechada AS LOGICAL INITIAL yes 
     LABEL "Fechada" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tgFrotas AS LOGICAL INITIAL yes 
     LABEL "Manuteá∆o Frota" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.43 BY .83 NO-UNDO.

DEFINE VARIABLE tgMI AS LOGICAL INITIAL yes 
     LABEL "Manutená∆o Industrial" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.43 BY .83 NO-UNDO.

DEFINE VARIABLE tgNaoAprov AS LOGICAL INITIAL yes 
     LABEL "N∆o Aprovada" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .83 NO-UNDO.

DEFINE VARIABLE tgPendente AS LOGICAL INITIAL yes 
     LABEL "Pendente Compl" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .83 NO-UNDO.

DEFINE VARIABLE tgReq AS LOGICAL INITIAL yes 
     LABEL "Requisiá∆o Estoque" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .83 NO-UNDO.

DEFINE VARIABLE tgSolic AS LOGICAL INITIAL yes 
     LABEL "Solicitaá∆o Compras" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .83 NO-UNDO.

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

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execuá∆o" 
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
     SIZE 27.86 BY .92
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.14 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.14 BY 1.71.


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

DEFINE FRAME fPage2
     fi-periodo-ini AT ROW 2.5 COL 28.86 COLON-ALIGNED
     fi-periodo-fim AT ROW 2.5 COL 46.86 COLON-ALIGNED NO-LABEL
     fi-ordem-ini AT ROW 3.5 COL 23.86 COLON-ALIGNED
     fi-ordem-fim AT ROW 3.5 COL 47 COLON-ALIGNED NO-LABEL
     fi-tecnico-ini AT ROW 4.5 COL 30 COLON-ALIGNED
     fi-tecnico-fim AT ROW 4.5 COL 47 COLON-ALIGNED NO-LABEL
     fi-emp-ini AT ROW 5.5 COL 33.86 COLON-ALIGNED
     fi-emp-fim AT ROW 5.5 COL 47 COLON-ALIGNED NO-LABEL
     fi-eqpto-ini AT ROW 6.5 COL 20.43 COLON-ALIGNED
     fi-eqpto-fim AT ROW 6.5 COL 47 COLON-ALIGNED NO-LABEL
     fi-req-ini AT ROW 7.5 COL 30 COLON-ALIGNED
     fi-req-fim AT ROW 7.5 COL 47 COLON-ALIGNED NO-LABEL
     IMAGE-1 AT ROW 2.5 COL 40.86
     IMAGE-2 AT ROW 2.5 COL 45.86
     IMAGE-17 AT ROW 3.5 COL 40.86
     IMAGE-18 AT ROW 3.5 COL 45.86
     IMAGE-19 AT ROW 4.5 COL 40.86
     IMAGE-20 AT ROW 5.5 COL 40.86
     IMAGE-21 AT ROW 6.5 COL 40.86
     IMAGE-22 AT ROW 4.5 COL 45.86
     IMAGE-23 AT ROW 5.5 COL 45.86
     IMAGE-24 AT ROW 6.5 COL 45.86
     IMAGE-25 AT ROW 7.5 COL 40.86
     IMAGE-26 AT ROW 7.5 COL 45.86
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.81
         SIZE 84.43 BY 10.15
         FONT 1.

DEFINE FRAME fPage4
     tgFrotas AT ROW 2.75 COL 9
     tgMI AT ROW 2.75 COL 27
     tgAberta AT ROW 5.5 COL 9
     tgAprov AT ROW 5.5 COL 48
     tgFechada AT ROW 6.5 COL 9
     tgNaoAprov AT ROW 6.5 COL 48
     tgPendente AT ROW 7.5 COL 9
     tgReq AT ROW 8.17 COL 48
     tgCom AT ROW 8.5 COL 9
     tgSolic AT ROW 9.17 COL 48
     fi-text AT ROW 1.75 COL 6 NO-LABEL
     fi-text2 AT ROW 4.46 COL 4 COLON-ALIGNED NO-LABEL
     fi-text3 AT ROW 4.46 COL 43.86 COLON-ALIGNED NO-LABEL
     fi-text4 AT ROW 7.5 COL 43.57 COLON-ALIGNED NO-LABEL
     RECT-10 AT ROW 2 COL 4.43
     RECT-11 AT ROW 4.71 COL 4.43
     RECT-12 AT ROW 4.71 COL 44
     RECT-39 AT ROW 7.75 COL 44
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.81
         SIZE 84.43 BY 10.15
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
     rsExecution AT ROW 5.96 COL 2.86 HELP
          "Modo de Execuá∆o" NO-LABEL
     text-destino AT ROW 1.63 COL 1.86 COLON-ALIGNED NO-LABEL
     text-modo AT ROW 5.21 COL 1.14 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.46 COL 2
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
/* SETTINGS FOR FRAME fPage4
                                                                        */
/* SETTINGS FOR FILL-IN fi-text IN FRAME fPage4
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME fPage6
   Custom                                                               */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME fPage6     = 
                "Destino".

ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME fPage6     = 
                "Execuá∆o".

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
&Scoped-define SELF-NAME fi-tecnico-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tecnico-fim wReport
ON LEAVE OF fi-tecnico-fim IN FRAME fPage2
DO:
    /*{cdp/cd9998.i}*/
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


&Scoped-define FRAME-NAME fpage0
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wReport 


/*:T--- L¢gica para inicializaá∆o do programam ---*/
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
assign fi-tecnico-ini = "000000"
       fi-periodo-fim = today.

display fi-tecnico-ini
        fi-periodo-fim with frame fPage2.

{utp/ut-liter.i "Tipo_Ordem"}
assign fi-text = return-value.
{utp/ut-liter.i "Situaá∆o_Requisiá∆o_/_Solicitaá∆o"}
assign fi-text2 = return-value.
{utp/ut-liter.i "Estado_Requisiá∆o_/_Solicitaá∆o"}
assign fi-text3 = return-value.
{utp/ut-liter.i "Tipo_Requisiá∆o"}
assign fi-text4 = return-value.

{utp/ut-liter.i "Manutená∆o_Frota"}
ASSIGN tgFrotas:LABEL IN FRAME fPage4 = RETURN-VALUE.
{utp/ut-liter.i "Manutená∆o_Industrial"}
ASSIGN tgMI:LABEL IN FRAME fPage4 = RETURN-VALUE.
{utp/ut-liter.i "Aberta"}
ASSIGN tgAberta:LABEL IN FRAME fPage4 = RETURN-VALUE.
{utp/ut-liter.i "Fechada"}
ASSIGN tgFechada:LABEL IN FRAME fPage4 = RETURN-VALUE.
{utp/ut-liter.i "Pendente_Compl"}
ASSIGN tgPendente:LABEL IN FRAME fPage4 = RETURN-VALUE.
{utp/ut-liter.i "Com_Ordem"}
ASSIGN tgCom:LABEL IN FRAME fPage4 = RETURN-VALUE.
{utp/ut-liter.i "Aprovada"}
ASSIGN tgAprov:LABEL IN FRAME fPage4 = RETURN-VALUE.
{utp/ut-liter.i "N∆o_Aprovada"}
ASSIGN tgNaoAprov:LABEL IN FRAME fPage4 = RETURN-VALUE.
{utp/ut-liter.i "Requisiá∆o_Estoque"}
ASSIGN tgReq:LABEL IN FRAME fPage4 = RETURN-VALUE.
{utp/ut-liter.i "Solicitaá∆o_Compras"}
ASSIGN tgSolic:LABEL IN FRAME fPage4 = RETURN-VALUE.

{utp/ut-liter.i "Periodo"}
ASSIGN fi-periodo-ini:LABEL IN FRAME fPage2 = RETURN-VALUE.
{utp/ut-liter.i "Ordem"}
ASSIGN fi-ordem-ini:LABEL IN FRAME fPage2 = RETURN-VALUE.
{utp/ut-liter.i "TÇcnico"}
ASSIGN fi-tecnico-ini:LABEL IN FRAME fPage2 = RETURN-VALUE.
{utp/ut-liter.i "Empresa"}
ASSIGN fi-emp-ini:LABEL IN FRAME fPage2 = RETURN-VALUE.
{utp/ut-liter.i "Equipamento"}
ASSIGN fi-eqpto-ini:LABEL IN FRAME fPage2 = RETURN-VALUE.
{utp/ut-liter.i "Requisiá∆o"}
ASSIGN fi-req-ini:LABEL IN FRAME fPage2 = RETURN-VALUE.

{utp/ut-liter.i "Destino"}
assign text-destino = return-value.
{utp/ut-liter.i "Execuá∆o"}
assign text-modo = return-value.

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
/*:T** Relatorio ***/
do on error undo, return error on stop  undo, return error:
    {report/rpexa.i}

    /*15/02/2005 - tech1007 - Teste alterado pois RTF n∆o Ç mais opá∆o de Destino*/
    if input frame fPage6 rsDestiny   = 2 and
       input frame fPage6 rsExecution = 1 then do:
        run utp/ut-vlarq.p (input input frame fPage6 cFile).
        
        if return-value = "NOK":U then do:
            run utp/ut-msgs.p (input "show":U, input 73, input "":U).
            apply "ENTRY":U to cFile in frame fPage6.
            return error.
        end.
    end.
    
    /*:T Coloque aqui as validaá‰es das outras p†ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p†gina com 
       problemas e colocar o focus no campo com problemas */
    
    /*:T Aqui s∆o gravados os campos da temp-table que ser† passada como parÉmetro
       para o programa RP.P */
    
    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame fPage6 rsDestiny
           tt-param.data-exec       = today
           tt-param.hora-exec       = TIME
           tt-param.periodo-ini     = date(fi-periodo-ini:screen-value in frame fPage2)
           tt-param.periodo-fim     = date(fi-periodo-fim:screen-value in frame fPage2)
           tt-param.ordem-ini       = int(fi-ordem-ini:screen-value in frame fPage2)
           tt-param.ordem-fim       = int(fi-ordem-fim:screen-value in frame fPage2)
           tt-param.tecnico-ini     = fi-tecnico-ini:screen-value in frame fPage2
           tt-param.tecnico-fim     = fi-tecnico-fim:screen-value in frame fPage2
           tt-param.emp-ini         = int(fi-emp-ini:screen-value in frame fPage2)
           tt-param.emp-fim         = int(fi-emp-fim:screen-value in frame fPage2)    
           tt-param.eqpto-ini       = fi-eqpto-ini:screen-value in frame fPage2  
           tt-param.eqpto-fim       = fi-eqpto-fim:screen-value in frame fPage2
           tt-param.req-ini         = int(fi-req-ini:screen-value in frame fPage2)
           tt-param.req-fim         = int(fi-req-fim:screen-value in frame fPage2)
           tt-param.lFrotas         = input frame fPage4 tgFrotas
           tt-param.lMI             = input frame fPage4 tgMI
           tt-param.lAberta         = input frame fPage4 tgAberta
           tt-param.lFechada        = input frame fPage4 tgFechada
           tt-param.lPendente       = input frame fPage4 tgPendente
           tt-param.lCom            = input frame fPage4 tgCom
           tt-param.lAprov          = input frame fPage4 tgAprov
           tt-param.lNaoAprov       = input frame fPage4 tgNaoAprov
           tt-param.lReq            = input frame fPage4 tgReq   
           tt-param.lSolic          = input frame fPage4 tgSolic.

    if tt-param.destino = 1 then 
        assign tt-param.arquivo = "":U.
    else if  tt-param.destino = 2 then
        assign tt-param.arquivo = input frame fPage6 cFile.
    else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.

    /*:T Coloque aqui a l¢gica de gravaá∆o dos demais campos que devem ser passados
       como parÉmetros para o programa RP.P, atravÇs da temp-table tt-param */
    
    /*:T Executar do programa RP.P que ir† criar o relat¢rio */
    {report/rpexb.i}
    
    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    {report/rprun.i mvp/esmv0910rp.p}
    
    {report/rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
/*     {report/rptrm.i} */
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


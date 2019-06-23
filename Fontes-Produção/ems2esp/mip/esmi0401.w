&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
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
{include/i-prgvrs.i esmi0401 2.06.00.000}  /*** 010005 ***/
/********************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{cdp/cdcfgmnt.i}
CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program       esmi0401
&GLOBAL-DEFINE Version        2.06.00.000
&GLOBAL-DEFINE VersionLayout  

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
&GLOBAL-DEFINE page1Widgets                                 
&GLOBAL-DEFINE page2Widgets  fi-ordem-ini ~ fi-ordem-fim ~ fi-empresa-ini ~ fi-empresa-fim ~ fi-equipamento-ini ~ fi-equipamento-fim ~ fi-tipo-manut-ini ~ fi-tipo-manut-fim ~ fi-planejador-ini ~ fi-planejador-fim ~ fi-prev-termino-ini ~ fi-prev-termino-fim ~ fi-termino-ini ~ fi-termino-fim ~ fi-ini-cedo-ini ~ fi-ini-cedo-fim ~ fi-ini-tarde-ini ~ fi-ini-tarde-fim ~ fi-dt-manut-ini ~ fi-dt-manut-fim


&GLOBAL-DEFINE page3Widgets   rsClassif
&GLOBAL-DEFINE page4Widgets   rsEstado 
                              
                              
&GLOBAL-DEFINE page5Widgets   
&GLOBAL-DEFINE page6Widgets   rsDestiny ~
                              btConfigImpr ~
                              btFile ~
                              rsExecution ~
                              
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
&GLOBAL-DEFINE page2Fields  fi-ordem-ini ~ fi-ordem-fim ~ fi-empresa-ini ~ fi-empresa-fim ~ fi-equipamento-ini ~ fi-equipamento-fim ~ fi-ini-cedo-ini ~ fi-ini-cedo-fim ~ fi-ini-tarde-ini ~ fi-ini-tarde-fim ~ fi-tipo-manut-ini ~ fi-tipo-manut-fim ~ fi-planejador-ini ~ fi-planejador-fim ~ fi-dt-manut-ini ~ fi-dt-manut-fim ~ fi-prev-termino-ini ~ fi-prev-termino-fim ~ fi-termino-ini ~ fi-termino-fim  

&GLOBAL-DEFINE page3Fields    
&GLOBAL-DEFINE page4Fields    
&GLOBAL-DEFINE page5Fields    
&GLOBAL-DEFINE page6Fields    cFile
&GLOBAL-DEFINE page7Fields    
&GLOBAL-DEFINE page8Fields    

/* Parameters Definitions ---                                           */
      DEF temp-table tt-param
        field destino              as integer
        field arquivo              as char
        field usuario              as char
        field data-exec            as date
        field hora-exec            as integer
        field parametro            as logical
        FIELD formato              as integer
        field v_num_tip_aces_usuar as integer
        field i-ep-codigo          as integer
        field classifica           as integer
        field estado               as integer
        field tipo                 as integer
        field desc-classifica      as char format "x(40)"
        field rs-considera         as integer
        field desc-considera       as char format "x(40)"
        field l-motorizados        as logical  
        field l-n-motorizados      as logical
        field l-ativos             as logical
        field l-Inativos           as logical 
        field l-proprios           as logical  
        field l-terceiros          as logical
        field l-combust            as logical
        field l-caract             as logical
        field l-comptes            as logical
        field l-hist               as logical  
        field l-eventos            as logical
        field l-planos             as logical
        field l-config             as logical 
        field l-param              as logical
        field l-Insere             as logical
        field i-ordem-ini          like ord-manut.nr-ord-produ
        field i-ordem-fim          like ord-manut.nr-ord-produ
        field i-empresa-ini        like ord-manut.ep-codigo   
        field i-empresa-fim        like ord-manut.ep-codigo   
        field c-equipamento-ini    like ord-manut.cd-equipto  
        field c-equipamento-fim    like ord-manut.cd-equipto  
        field dt-manut-ini         like ord-manut.dt-manut    
        field dt-manut-fim         like ord-manut.dt-manut    
        field dt-ini-cedo-ini      like ord-manut.dt-ini-cedo 
        field dt-ini-cedo-fim      like ord-manut.dt-ini-cedo 
        field i-tipo-manut-ini     like ord-manut.cd-tipo     
        field i-tipo-manut-fim     like ord-manut.cd-tipo     
        FIELD i-planejador-ini     LIKE ord-manut.cd-planejado
        FIELD i-planejador-fim     LIKE ord-manut.cd-planejado
        field dt-ini-tarde-ini     like ord-manut.dt-ini-tarde
        field dt-ini-tarde-fim     like ord-manut.dt-ini-tarde
        field dt-prev-termino-ini  like ord-manut.dt-prev     
        field dt-prev-termino-fim  like ord-manut.dt-prev     
        field dt-termino-ini       like ord-manut.dt-fecham   
        field dt-termino-fim       like ord-manut.dt-fecham.  
       
    
        

def temp-table tt-digita no-undo
    field linha      as integer
    field conteudo   as character format "x(80)"
    index editor-id is primary unique linha.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-arq-layout       as char    no-undo.      
def var c-arq-temp         as char    no-undo.

def stream s-imp.

{abp/ab9000.i} /** Procedures de conversÆo de tempo:
                   piFormataHoraParaSegundo
                   piFormataSegundoParaHora **/

DEFINE VARIABLE cHora       AS CHARACTER format "00:00:00" NO-UNDO.
DEFINE VARIABLE cHora2      AS CHARACTER format "23:59:59" NO-UNDO.

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

DEFINE VARIABLE fi-dt-manut-fim AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88.

DEFINE VARIABLE fi-dt-manut-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Data de Manuten‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-empresa-fim AS INTEGER FORMAT ">>9" INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 6.43 BY .88.

DEFINE VARIABLE fi-empresa-ini AS INTEGER FORMAT "->>9" INITIAL 0 
     LABEL "Empresa":R14 
     VIEW-AS FILL-IN 
     SIZE 6.43 BY .88.

DEFINE VARIABLE fi-equipamento-fim AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88.

DEFINE VARIABLE fi-equipamento-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Equipamento":R21 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88.

DEFINE VARIABLE fi-ini-cedo-fim AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88.

DEFINE VARIABLE fi-ini-cedo-ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Inicio Cedo":R18 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88.

DEFINE VARIABLE fi-ini-tarde-fim AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88.

DEFINE VARIABLE fi-ini-tarde-ini AS DATE FORMAT "99/99/99" 
     LABEL "Inicio Tarde":R22 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88.

DEFINE VARIABLE fi-ordem-fim AS INTEGER FORMAT "->>>,>>>,>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88.

DEFINE VARIABLE fi-ordem-ini AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Ordem":R9 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88.

DEFINE VARIABLE fi-planejador-fim AS CHARACTER FORMAT "X(12)":U INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 15.43 BY .88.

DEFINE VARIABLE fi-planejador-ini AS CHARACTER FORMAT "X(12)":U INITIAL "0" 
     LABEL "Planejador" 
     VIEW-AS FILL-IN 
     SIZE 15.43 BY .88.

DEFINE VARIABLE fi-prev-termino-fim AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88.

DEFINE VARIABLE fi-prev-termino-ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Prev T‚rmino":R15 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88.

DEFINE VARIABLE fi-termino-fim AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88.

DEFINE VARIABLE fi-termino-ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "T‚rmino":R10 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88.

DEFINE VARIABLE fi-tipo-manut-fim AS INTEGER FORMAT ">>,>>9" INITIAL 99999 
     VIEW-AS FILL-IN 
     SIZE 8.43 BY .88.

DEFINE VARIABLE fi-tipo-manut-ini AS INTEGER FORMAT ">>,>>9":U INITIAL 0 
     LABEL "Tipo Manuten‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 15.43 BY .88.

DEFINE IMAGE IMAGE-10
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-13
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-15
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-16
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-22
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-23
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-26
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-27
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-28
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-29
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-33
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-34
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE VARIABLE rsClassif AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Por Ordem", 1,
"Por Empresa / Equipamento", 2,
"Data Manuten‡Æo", 3
     SIZE 27.86 BY 3.79
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 31 BY 5.13.

DEFINE VARIABLE rsEstado AS INTEGER INITIAL 9 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "NÆo Iniciada", 1,
"Liberada", 2,
"Alocada", 3,
"Separada", 4,
"Requisitada", 5,
"Iniciada", 6,
"Finalizada", 7,
"Terminada", 8,
"Todos", 9
     SIZE 15 BY 8 NO-UNDO.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY 9.75.

DEFINE BUTTON btConfigImpr 
     IMAGE-UP FILE "image/im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON btFile 
     IMAGE-UP FILE "image/im-sea":U
     IMAGE-INSENSITIVE FILE "image/ii-sea":U
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

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

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
     fi-ordem-ini AT ROW 1.83 COL 18.57 COLON-ALIGNED HELP
          "C¢digo da empresa do equipamento"
     fi-ordem-fim AT ROW 1.83 COL 43 COLON-ALIGNED HELP
          "C¢digo da empresa do equipamento" NO-LABEL
     fi-empresa-ini AT ROW 2.83 COL 18.57 COLON-ALIGNED HELP
          "C¢digo do Equipamento"
     fi-empresa-fim AT ROW 2.83 COL 43 COLON-ALIGNED HELP
          "C¢digo do Equipamento" NO-LABEL
     fi-equipamento-ini AT ROW 3.83 COL 18.57 COLON-ALIGNED HELP
          "C¢digo Grupo Equipamento"
     fi-equipamento-fim AT ROW 3.83 COL 43 COLON-ALIGNED HELP
          "C¢digo Grupo Equipamento" NO-LABEL
     fi-dt-manut-ini AT ROW 4.83 COL 18.57 COLON-ALIGNED
     fi-dt-manut-fim AT ROW 4.83 COL 43 COLON-ALIGNED HELP
          "C¢digo do Modelo" NO-LABEL
     fi-ini-cedo-ini AT ROW 5.83 COL 18.57 COLON-ALIGNED HELP
          "C¢digo do estabelecimento."
     fi-ini-cedo-fim AT ROW 5.83 COL 43 COLON-ALIGNED HELP
          "C¢digo do estabelecimento." NO-LABEL
     fi-ini-tarde-ini AT ROW 6.83 COL 18.57 COLON-ALIGNED HELP
          "C¢digo Estrutura Mecƒnica"
     fi-ini-tarde-fim AT ROW 6.83 COL 43 COLON-ALIGNED HELP
          "C¢digo Estrutura Mecƒnica" NO-LABEL
     fi-tipo-manut-ini AT ROW 7.88 COL 18.57 COLON-ALIGNED HELP
          "C½digo do proprietÿrio do equipamento"
     fi-tipo-manut-fim AT ROW 7.83 COL 43 COLON-ALIGNED HELP
          "Localiza‡Æo funcional (TAG)" NO-LABEL
     fi-planejador-ini AT ROW 8.83 COL 18.57 COLON-ALIGNED HELP
          "C½digo do proprietÿrio do equipamento"
     fi-planejador-fim AT ROW 8.83 COL 43 COLON-ALIGNED HELP
          "C½digo do proprietÿrio do equipamento" NO-LABEL
     fi-prev-termino-ini AT ROW 9.83 COL 18.57 COLON-ALIGNED HELP
          "C¢digo Centro Custo"
     fi-prev-termino-fim AT ROW 9.83 COL 43 COLON-ALIGNED HELP
          "C¢digo Centro Custo" NO-LABEL
     fi-termino-ini AT ROW 10.83 COL 18.57 COLON-ALIGNED HELP
          "C¢digo da Situacao do Equipamento"
     fi-termino-fim AT ROW 10.83 COL 43 COLON-ALIGNED HELP
          "C¢digo da Situacao do Equipamento" NO-LABEL
     IMAGE-10 AT ROW 4.83 COL 41.14
     IMAGE-11 AT ROW 9.88 COL 36.72
     IMAGE-12 AT ROW 9.83 COL 41.14
     IMAGE-13 AT ROW 6.83 COL 36.72
     IMAGE-14 AT ROW 6.83 COL 41.14
     IMAGE-15 AT ROW 8.83 COL 36.72
     IMAGE-16 AT ROW 8.83 COL 41.14
     IMAGE-22 AT ROW 10.88 COL 36.72
     IMAGE-23 AT ROW 10.83 COL 41.14
     IMAGE-26 AT ROW 1.83 COL 36.72
     IMAGE-27 AT ROW 2.83 COL 36.72
     IMAGE-28 AT ROW 2.83 COL 41.14
     IMAGE-29 AT ROW 1.83 COL 41.14
     IMAGE-3 AT ROW 5.83 COL 36.72
     IMAGE-33 AT ROW 7.83 COL 36.72
     IMAGE-34 AT ROW 7.83 COL 41.14
     IMAGE-4 AT ROW 5.83 COL 41.14
     IMAGE-7 AT ROW 3.83 COL 36.72
     IMAGE-8 AT ROW 3.83 COL 41.14
     IMAGE-9 AT ROW 4.83 COL 36.72
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7 ROW 2.79
         SIZE 76.86 BY 12.96
         FONT 1.

DEFINE FRAME fPage4
     rsEstado AT ROW 2.75 COL 6.72 NO-LABEL
     "Estado" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 1.75 COL 6.86
     RECT-28 AT ROW 2 COL 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7 ROW 2.79
         SIZE 76.86 BY 12.96
         FONT 1.

DEFINE FRAME fPage6
     rsDestiny AT ROW 2.38 COL 3.29 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     btConfigImpr AT ROW 3.58 COL 43.29 HELP
          "Configura‡Æo da impressora"
     btFile AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     cFile AT ROW 3.63 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rsExecution AT ROW 5.75 COL 3 HELP
          "Modo de Execu‡Æo" NO-LABEL
     text-destino AT ROW 1.63 COL 1.86 COLON-ALIGNED NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     RECT-10 AT ROW 5.29 COL 2.14
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7 ROW 2.79
         SIZE 76.86 BY 11.71
         FONT 1.

DEFINE FRAME fPage3
     rsClassif AT ROW 3.21 COL 4.86 HELP
          "Classifica‡Æo para emissÆo do relat¢rio" NO-LABEL
     "Classificar por" VIEW-AS TEXT
          SIZE 11 BY .88 AT ROW 2 COL 4.57
     RECT-13 AT ROW 2.38 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7 ROW 2.79
         SIZE 76.86 BY 12.71
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
         TITLE              = "Listagem de Equipamentos"
         HEIGHT             = 16.96
         WIDTH              = 90
         MAX-HEIGHT         = 29
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 29
         VIRTUAL-WIDTH      = 146.29
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
   Custom                                                               */
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
ON END-ERROR OF wReport /* Listagem de Equipamentos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wReport wReport
ON WINDOW-CLOSE OF wReport /* Listagem de Equipamentos */
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piExecute wReport 
PROCEDURE piExecute :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

define var r-tt-digita as rowid no-undo.

&IF DEFINED(PGIMP) <> 0 AND "{&PGIMP}":U = "YES":U &THEN
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
    
    
    
    /* Aqui sÆo gravados os campos da temp-table que ser  passada como parƒmetro
       para o programa RP.P */
    
    create tt-param.
    assign tt-param.usuario               = c-seg-usuario
           tt-param.destino               = input frame fPage6 rsDestiny
           tt-param.data-exec             = today
           tt-param.hora-exec             = time
           tt-param.classifica            = input frame fPage3 rsClassif
           tt-param.estado                = INPUT FRAME fpage4 rsEstado
           /*tt-param.tipo                  = INPUT FRAME fpage4 rsTipo*/
          
           tt-param.i-ordem-ini           = input frame fpage2  fi-ordem-ini       
           tt-param.i-ordem-fim           = input frame fpage2  fi-ordem-fim       
           tt-param.i-empresa-ini         = input frame fpage2  fi-empresa-ini       
           tt-param.i-empresa-fim         = input frame fpage2  fi-empresa-fim       
           tt-param.c-equipamento-ini     = input frame fpage2  fi-equipamento-ini   
           tt-param.c-equipamento-fim     = input frame fpage2  fi-equipamento-fim   
           tt-param.dt-manut-ini          = input frame fpage2  fi-dt-manut-ini      
           tt-param.dt-manut-fim          = input frame fpage2  fi-dt-manut-fim       
           tt-param.dt-ini-cedo-ini       = input frame fpage2  fi-ini-cedo-ini     
           tt-param.dt-ini-cedo-fim       = input frame fpage2  fi-ini-cedo-fim     
           tt-param.i-tipo-manut-ini      = input frame fpage2  fi-tipo-manut-ini           
           tt-param.i-tipo-manut-fim      = input frame fpage2  fi-tipo-manut-fim 
           tt-param.i-planejador-ini      = INPUT FRAME fpage2  fi-planejador-ini
           tt-param.i-planejador-fim      = INPUT FRAME fpage2  fi-planejador-fim
           tt-param.dt-ini-tarde-ini      = input frame fpage2  fi-ini-tarde-ini  
           tt-param.dt-ini-tarde-fim      = input frame fpage2  fi-ini-tarde-fim  
           tt-param.dt-prev-termino-ini   = input frame fpage2  fi-prev-termino-ini       
           tt-param.dt-prev-termino-fim   = input frame fpage2  fi-prev-termino-fim       
           tt-param.dt-termino-ini        = input frame fpage2  fi-termino-ini   
           tt-param.dt-termino-fim        = input frame fpage2  fi-termino-fim.
   
    if tt-param.destino = 1 
    then 
        assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame fPage6 cFile.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    
    /* Coloque aqui a l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */
    
    /* Executar do programa RP.P que ir  criar o relat¢rio */
    {report/rpexb.i}


    SESSION:SET-WAIT-STATE("GENERAL":U).
    
    {report/rprun.i mip/esmi0401rp.p}
    
    {report/rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
    {report/rptrm.i}
end.
&ELSE
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


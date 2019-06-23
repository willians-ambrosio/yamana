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
{include/i-prgvrs.i ESMV0617 2.06.00.000}  /*** 010005 ***/
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
&GLOBAL-DEFINE Program        ESMV0617
&GLOBAL-DEFINE Version        2.06.00.000
&GLOBAL-DEFINE VersionLayout  

&GLOBAL-DEFINE Folder         YES
&GLOBAL-DEFINE InitialPage    1
&GLOBAL-DEFINE FolderLabels   Sele‡Æo,Parƒmetro,ImpressÆo

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
&GLOBAL-DEFINE page4Widgets  tgNaoIniciada ~
                             tgIniciada ~
                             tgEquipamento ~
                             tgComponente ~
                             tgNaoIniciada-2 ~
                             tgIniciada-2 ~
                             rsTipo ~
                             tgSave
&GLOBAL-DEFINE page5Widgets   
&GLOBAL-DEFINE page6Widgets  rsDestiny ~
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
&GLOBAL-DEFINE page6Text    text-destino text-modo  
&GLOBAL-DEFINE page7Text      
&GLOBAL-DEFINE page8Text   

&GLOBAL-DEFINE page1Fields    
&GLOBAL-DEFINE page2Fields  fi-ordem-ini fi-ordem-fim ~
                            fi-empresa-ini fi-empresa-fim ~
                            fi-equipamento-ini fi-equipamento-fim ~
                            fi-especialidade-ini fi-especialidade-fim ~
                            fi-tipo-manut-ini fi-tipo-manut-fim ~
                            fi-planejador-ini fi-planejador-fim ~
                            fi-oficina-ini fi-oficina-fim ~
                            fi-dat-evento-ini ~ fi-dat-evento-fim ~
                            fi-dat-tendencia-ini ~ fi-dat-tendencia-fim
&GLOBAL-DEFINE page3Fields    
&GLOBAL-DEFINE page4Fields  fi-dat-base  
&GLOBAL-DEFINE page5Fields    
&GLOBAL-DEFINE page6Fields  cFile
&GLOBAL-DEFINE page7Fields    
&GLOBAL-DEFINE page8Fields    

/* Parameters Definitions ---                                           */
define temp-table tt-param
        field destino              as integer
        field arquivo              as char
        field usuario              as char
        field data-exec            as date
        field hora-exec            as integer
        field classifica           as integer
        field desc-classifica      as char format "x(40)"
        field desc-considera       as char format "x(40)"
        field i-ordem-ini          as integer
        field i-ordem-fim          as integer
        field i-empresa-ini        as char
        field i-empresa-fim        as char
        field c-equipamento-ini    as character format "x(16)":U 
        field c-equipamento-fim    as character format "x(16)":U     
        FIELD c-especialidade-ini  as character format "x(12)":U 
        FIELD c-especialidade-fim  as character format "x(12)":U 
        field i-tipo-manut-ini     as integer
        field i-tipo-manut-fim     as integer
        FIELD i-planejador-ini     as character format "x(12)":U
        FIELD i-planejador-fim     as character format "x(12)":U
        field c-oficina-ini        as character format "x(8)":U
        field c-oficina-fim        as character format "x(8)":U
        field dat-evento-ini       as date      format "99/99/9999":U
        field dat-evento-fim       as date      format "99/99/9999":U
        field dat-base             as date      format "99/99/9999":U
        field dat-tendencia-ini    as date      format "99/99/9999":U
        field dat-tendencia-fim    as date      format "99/99/9999":U
        FIELD l-naoIniciada        AS LOGICAL
        FIELD l-Terminada          AS LOGICAL
        FIELD l-iniciada           AS LOGICAL
        FIELD l-equipamento        AS LOGICAL
        FIELD l-componente         AS LOGICAL
        FIELD l-naoIniciada2       AS LOGICAL
        FIELD l-iniciada2          AS logical
        field l-save               as logical
        field i-tipo               as integer.
    
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

DEF VAR hDBOMmvTarOrdManut      AS HANDLE NO-UNDO.
DEF VAR hDBOMmvOrdManut         AS HANDLE NO-UNDO.
DEF VAR hDBOMmvEspecialidFunc   AS HANDLE NO-UNDO.
DEF VAR hDBOMmvTecTarefaOm      AS HANDLE NO-UNDO.
DEF VAR hDBOMmvTarPlanoItem     AS HANDLE NO-UNDO.

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

DEFINE VARIABLE fi-dat-evento-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dat-evento-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Data Evento" 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dat-tendencia-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dat-tendencia-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Per¡odo An lise Tendˆncia" 
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

DEFINE VARIABLE fi-especialidade-fim AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88.

DEFINE VARIABLE fi-especialidade-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Especialidade":R21 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88.

DEFINE VARIABLE fi-oficina-fim AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88.

DEFINE VARIABLE fi-oficina-ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Oficina":R22 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88.

DEFINE VARIABLE fi-ordem-fim AS INTEGER FORMAT "->>>,>>>,>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88.

DEFINE VARIABLE fi-ordem-ini AS INTEGER FORMAT "->>>,>>>,>>9" INITIAL 0 
     LABEL "Ordem":R9 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88.

DEFINE VARIABLE fi-planejador-fim AS CHARACTER FORMAT "X(12)":U INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 15.43 BY .88.

DEFINE VARIABLE fi-planejador-ini AS CHARACTER FORMAT "X(12)":U 
     LABEL "Planejador" 
     VIEW-AS FILL-IN 
     SIZE 15.43 BY .88.

DEFINE VARIABLE fi-tipo-manut-fim AS INTEGER FORMAT ">>,>>9" INITIAL 99999 
     VIEW-AS FILL-IN 
     SIZE 8.43 BY .88.

DEFINE VARIABLE fi-tipo-manut-ini AS INTEGER FORMAT ">>,>>9":U INITIAL 0 
     LABEL "Tipo Manuten‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 15.43 BY .88.

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

DEFINE IMAGE IMAGE-33
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-34
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-57
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-58
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-59
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-60
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-61
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-62
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image/im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image/im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE fi-dat-base AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Data Base" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE rsTipo AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Backlog Online", 1,
"An lise de Tendˆncia", 2
     SIZE 40 BY 1.17 NO-UNDO.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16.57 BY 3.08.

DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16.29 BY 3.

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY 3.

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 58 BY 2.

DEFINE VARIABLE tgComponente AS LOGICAL INITIAL yes 
     LABEL "Componente" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tgEquipamento AS LOGICAL INITIAL yes 
     LABEL "Equipamento" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tgIniciada AS LOGICAL INITIAL yes 
     LABEL "Iniciada" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE tgIniciada-2 AS LOGICAL INITIAL yes 
     LABEL "Iniciada" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE tgNaoIniciada AS LOGICAL INITIAL yes 
     LABEL "NÆo Iniciada" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tgNaoIniciada-2 AS LOGICAL INITIAL yes 
     LABEL "NÆo Iniciada" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tgSave AS LOGICAL INITIAL no 
     LABEL "Salva Registro Tabela" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .83 NO-UNDO.

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
     fi-ordem-ini AT ROW 1.46 COL 18.57 COLON-ALIGNED HELP
          "C¢digo da empresa do equipamento"
     fi-ordem-fim AT ROW 1.46 COL 43 COLON-ALIGNED HELP
          "C¢digo da empresa do equipamento" NO-LABEL
     fi-empresa-ini AT ROW 2.46 COL 18.57 COLON-ALIGNED HELP
          "C¢digo do Equipamento"
     fi-empresa-fim AT ROW 2.46 COL 43 COLON-ALIGNED HELP
          "C¢digo do Equipamento" NO-LABEL
     fi-equipamento-ini AT ROW 3.46 COL 18.57 COLON-ALIGNED HELP
          "C¢digo Grupo Equipamento"
     fi-equipamento-fim AT ROW 3.46 COL 43 COLON-ALIGNED HELP
          "C¢digo Grupo Equipamento" NO-LABEL
     fi-especialidade-ini AT ROW 4.46 COL 18.57 COLON-ALIGNED HELP
          "C¢digo Grupo Equipamento"
     fi-especialidade-fim AT ROW 4.46 COL 43 COLON-ALIGNED HELP
          "C¢digo Grupo Equipamento" NO-LABEL
     fi-tipo-manut-fim AT ROW 5.58 COL 43 COLON-ALIGNED HELP
          "Localiza‡Æo funcional (TAG)" NO-LABEL
     fi-tipo-manut-ini AT ROW 5.63 COL 18.57 COLON-ALIGNED HELP
          "C½digo do proprietÿrio do equipamento"
     fi-planejador-ini AT ROW 6.58 COL 18.57 COLON-ALIGNED HELP
          "Codigo do proprietario do equipamento"
     fi-planejador-fim AT ROW 6.58 COL 43 COLON-ALIGNED HELP
          "C½digo do proprietÿrio do equipamento" NO-LABEL
     fi-oficina-ini AT ROW 7.58 COL 18.57 COLON-ALIGNED HELP
          "C¢digo Estrutura Mecƒnica"
     fi-oficina-fim AT ROW 7.58 COL 43 COLON-ALIGNED HELP
          "C¢digo Estrutura Mecƒnica" NO-LABEL
     fi-dat-evento-ini AT ROW 8.58 COL 18.57 COLON-ALIGNED
     fi-dat-evento-fim AT ROW 8.58 COL 43 COLON-ALIGNED NO-LABEL
     fi-dat-tendencia-ini AT ROW 9.58 COL 18.57 COLON-ALIGNED
     fi-dat-tendencia-fim AT ROW 9.58 COL 43 COLON-ALIGNED NO-LABEL
     IMAGE-13 AT ROW 5.58 COL 36.72
     IMAGE-14 AT ROW 5.58 COL 41.14
     IMAGE-15 AT ROW 7.58 COL 36.72
     IMAGE-16 AT ROW 7.58 COL 41.14
     IMAGE-26 AT ROW 1.46 COL 36.72
     IMAGE-27 AT ROW 2.46 COL 36.72
     IMAGE-28 AT ROW 2.46 COL 41.14
     IMAGE-29 AT ROW 1.46 COL 41.14
     IMAGE-33 AT ROW 6.58 COL 36.72
     IMAGE-34 AT ROW 6.58 COL 41.14
     IMAGE-7 AT ROW 3.46 COL 36.72
     IMAGE-8 AT ROW 3.46 COL 41.14
     IMAGE-57 AT ROW 4.46 COL 36.72
     IMAGE-58 AT ROW 4.46 COL 41.14
     IMAGE-59 AT ROW 8.58 COL 37
     IMAGE-60 AT ROW 8.58 COL 41.14
     IMAGE-61 AT ROW 9.58 COL 37
     IMAGE-62 AT ROW 9.58 COL 41.14
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7 ROW 2.79
         SIZE 76.86 BY 10.71
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
         SIZE 76.86 BY 10.71
         FONT 1.

DEFINE FRAME fPage4
     fi-dat-base AT ROW 1.5 COL 12 COLON-ALIGNED
     tgNaoIniciada AT ROW 3.83 COL 4.57
     tgEquipamento AT ROW 3.83 COL 24.57
     tgNaoIniciada-2 AT ROW 3.83 COL 43.43
     tgIniciada AT ROW 4.83 COL 4.57
     tgComponente AT ROW 4.83 COL 24.57
     tgIniciada-2 AT ROW 4.83 COL 43.43
     rsTipo AT ROW 7.71 COL 7 NO-LABEL
     tgSave AT ROW 9.75 COL 3
     "Tipo OM" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 3.33 COL 25.29
     "Estado OM" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 3.25 COL 4.86
     "Tipo Tarefa" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 3.33 COL 43.86
     RECT-28 AT ROW 3.5 COL 3
     RECT-30 AT ROW 3.58 COL 23.29
     RECT-31 AT ROW 3.58 COL 41.86
     RECT-32 AT ROW 7.25 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7 ROW 2.79
         SIZE 76.86 BY 10.75
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
         TITLE              = "Backlog de Frotas"
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
       FRAME fPage4:FRAME = FRAME fpage0:HANDLE
       FRAME fPage6:FRAME = FRAME fpage0:HANDLE.

/* SETTINGS FOR FRAME fpage0
   NOT-VISIBLE FRAME-NAME                                               */
/* SETTINGS FOR FRAME fPage2
                                                                        */
/* SETTINGS FOR FRAME fPage4
                                                                        */
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
ON END-ERROR OF wReport /* Backlog de Frotas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wReport wReport
ON WINDOW-CLOSE OF wReport /* Backlog de Frotas */
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


&Scoped-define FRAME-NAME fPage4
&Scoped-define SELF-NAME rsTipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsTipo wReport
ON VALUE-CHANGED OF rsTipo IN FRAME fPage4
DO:
    if input frame fPage4 rsTipo = 2 then
        assign fi-dat-base:sensitive          in frame fPage4 = no
               tgNaoIniciada:sensitive        in frame fPage4 = no
               tgIniciada:sensitive           in frame fPage4 = no
               tgEquipamento:sensitive        in frame fPage4 = no
               tgComponente:sensitive         in frame fPage4 = no
               tgNaoIniciada-2:sensitive      in frame fPage4 = no
               tgIniciada-2:sensitive         in frame fPage4 = no
               tgSave:sensitive               in frame fPage4 = no
               fi-dat-tendencia-ini:sensitive in frame fPage2 = yes
               fi-dat-tendencia-fim:sensitive in frame fPage2 = yes
               fi-ordem-ini:sensitive         in frame fPage2 = no
               fi-ordem-fim:sensitive         in frame fPage2 = no
               fi-oficina-ini:sensitive       in frame fPage2 = no
               fi-oficina-fim:sensitive       in frame fPage2 = no
               fi-empresa-ini:sensitive       in frame fPage2 = no
               fi-empresa-fim:sensitive       in frame fPage2 = no
               fi-equipamento-ini:sensitive   in frame fPage2 = no
               fi-equipamento-fim:sensitive   in frame fPage2 = no
               fi-especialidade-ini:sensitive in frame fPage2 = no
               fi-especialidade-fim:sensitive in frame fPage2 = no
               fi-tipo-manut-ini:sensitive    in frame fPage2 = no
               fi-tipo-manut-fim:sensitive    in frame fPage2 = no
               fi-planejador-ini:sensitive    in frame fPage2 = no
               fi-planejador-fim:sensitive    in frame fPage2 = no
               fi-dat-evento-ini:sensitive    in frame fPage2 = no
               fi-dat-evento-fim:sensitive    in frame fPage2 = no.
    else
        assign fi-dat-base:sensitive          in frame fPage4 = yes
               tgNaoIniciada:sensitive        in frame fPage4 = yes
               tgIniciada:sensitive           in frame fPage4 = yes
               tgEquipamento:sensitive        in frame fPage4 = yes
               tgComponente:sensitive         in frame fPage4 = yes
               tgNaoIniciada-2:sensitive      in frame fPage4 = yes
               tgIniciada-2:sensitive         in frame fPage4 = yes
               tgSave:sensitive               in frame fPage4 = yes
               fi-dat-tendencia-ini:sensitive in frame fPage2 = no 
               fi-dat-tendencia-fim:sensitive in frame fPage2 = no
               fi-ordem-ini:sensitive         in frame fPage2 = yes
               fi-ordem-fim:sensitive         in frame fPage2 = yes
               fi-oficina-ini:sensitive       in frame fPage2 = yes
               fi-oficina-fim:sensitive       in frame fPage2 = yes
               fi-empresa-ini:sensitive       in frame fPage2 = yes
               fi-empresa-fim:sensitive       in frame fPage2 = yes
               fi-equipamento-ini:sensitive   in frame fPage2 = yes
               fi-equipamento-fim:sensitive   in frame fPage2 = yes
               fi-especialidade-ini:sensitive in frame fPage2 = yes
               fi-especialidade-fim:sensitive in frame fPage2 = yes
               fi-tipo-manut-ini:sensitive    in frame fPage2 = yes
               fi-tipo-manut-fim:sensitive    in frame fPage2 = yes
               fi-planejador-ini:sensitive    in frame fPage2 = yes
               fi-planejador-fim:sensitive    in frame fPage2 = yes
               fi-dat-evento-ini:sensitive    in frame fPage2 = yes
               fi-dat-evento-fim:sensitive    in frame fPage2 = yes.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterDestroyInterface wReport 
PROCEDURE afterDestroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

if valid-handle(hDBOMmvTarOrdManut) then
     DELETE PROCEDURE  hDBOMmvTarOrdManut. 

if valid-handle(hDBOMmvOrdManut) then
     DELETE PROCEDURE  hDBOMmvOrdManut. 

if valid-handle(hDBOMmvEspecialidFunc) then
     DELETE PROCEDURE  hDBOMmvEspecialidFunc. 

if valid-handle(hDBOMmvTecTarefaOm) then
     DELETE PROCEDURE  hDBOMmvTecTarefaOm. 

if valid-handle(hDBOMmvTarPlanoItem) then
     DELETE PROCEDURE  hDBOMmvTarPlanoItem. 

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wReport 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /** Labels da P gina Sele‡Æo **/
    {utp/ut-liter.i "Ordem"}
    assign fi-ordem-ini:label in frame fPage2 = trim(return-value).
    {utp/ut-liter.i "Oficina"}
    assign fi-oficina-ini:label in frame fPage2 = trim(return-value).
    {utp/ut-liter.i "Empresa"}
    assign fi-empresa-ini:label in frame fPage2 = trim(return-value).
    {utp/ut-liter.i "Equipamento"}
    assign fi-equipamento-ini:label in frame fPage2 = trim(return-value).
    {utp/ut-liter.i "Especialidade"}
    assign fi-especialidade-ini:label in frame fPage2 = trim(return-value).
    {utp/ut-liter.i "Tipo Manuten‡Æo"}
    assign fi-tipo-manut-ini:label in frame fPage2 = trim(return-value).
    {utp/ut-liter.i "Planejador"}
    assign fi-planejador-ini:label in frame fPage2 = trim(return-value).
    {utp/ut-liter.i "Data Evento"}
    assign fi-dat-evento-ini:label in frame fPage2 = trim(return-value).

    /** Labels da P gina Parƒmetros **/
    {utp/ut-liter.i "NÆo_Iniciada"}
    assign tgNaoIniciada:label in frame fPage4 = trim(return-value).
    {utp/ut-liter.i "Iniciada"}
    assign tgIniciada:label in frame fPage4 = trim(return-value).
    {utp/ut-liter.i "Equipamento"}
    assign tgEquipamento:label in frame fPage4 = trim(return-value).
    {utp/ut-liter.i "Componente"}
    assign tgComponente:label in frame fPage4 = trim(return-value).
    {utp/ut-liter.i "NÆo Iniciada"}
    assign tgNaoIniciada-2:label in frame fPage4 = trim(return-value).
    {utp/ut-liter.i "Iniciada"}
    assign tgIniciada-2:label in frame fPage4 = trim(return-value).
    {utp/ut-liter.i "Salva_Registro_Tabela"}
    assign tgSave:label in frame fPage4 = trim(return-value).

    assign fi-dat-base:screen-value in frame fPage4 = string(today,"99/99/9999").

    if input frame fPage4 rsTipo = 1 then
        assign fi-dat-tendencia-ini:sensitive in frame fPage2 = no
               fi-dat-tendencia-fim:sensitive in frame fPage2 = no.

    return "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beforeInitializeInterface wReport 
PROCEDURE beforeInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /*:T--- Verifica se o DBO j  est  inicializado ---*/
    /* Abertura da DBO mmv-tar-ord-manut*/
    IF NOT VALID-HANDLE(hDBOMmvOrdManut) OR
       hDBOMmvOrdManut:TYPE <> "PROCEDURE":U OR
       hDBOMmvOrdManut:FILE-NAME <> "frbo/bofr072.p":U THEN DO:
        RUN frbo/bofr072.p PERSISTENT SET hDBOMmvOrdManut.
    END.
    RUN openQueryStatic IN hDBOMmvOrdManut (INPUT "Main":U) NO-ERROR. 

    /* Abertura da DBO mmv-tar-ord-manut*/
    IF NOT VALID-HANDLE(hDBOMmvTarOrdManut) OR
       hDBOMmvTarOrdManut:TYPE <> "PROCEDURE":U OR
       hDBOMmvTarOrdManut:FILE-NAME <> "frbo/bofr073.p":U THEN DO:
        RUN frbo/bofr073.p PERSISTENT SET hDBOMmvTarOrdManut.
    END.
    RUN openQueryStatic IN hDBOMmvTarOrdManut (INPUT "Main":U) NO-ERROR. 

    /* Abertura da DBO mmv-especialid-func*/
    IF NOT VALID-HANDLE(hDBOMmvEspecialidFunc) OR
       hDBOMmvEspecialidFunc:TYPE <> "PROCEDURE":U OR
       hDBOMmvEspecialidFunc:FILE-NAME <> "yambo/ydm004.p":U THEN DO:
        RUN yambo/ydm004.p PERSISTENT SET hDBOMmvEspecialidFunc.
    END.
    RUN openQueryStatic IN hDBOMmvEspecialidFunc (INPUT "Main":U) NO-ERROR. 

    /* Abertura da DBO mmv-tecnico-tarefa-om*/
    IF NOT VALID-HANDLE(hDBOMmvTecTarefaOm) OR
       hDBOMmvTecTarefaOm:TYPE <> "PROCEDURE":U OR
       hDBOMmvTecTarefaOm:FILE-NAME <> "yambo/ydm002.p":U THEN DO:
        RUN yambo/ydm002.p PERSISTENT SET hDBOMmvTecTarefaOm.
    END.
    RUN openQueryStatic IN hDBOMmvTecTarefaOm (INPUT "Main":U) NO-ERROR. 
  
    /* Abertura da DBO mmv-tar-ord-manut*/
    IF NOT VALID-HANDLE(hDBOMmvTarPlanoItem) OR
       hDBOMmvTarPlanoItem:TYPE <> "PROCEDURE":U OR
       hDBOMmvTarPlanoItem:FILE-NAME <> "frbo/bofr041.p":U THEN DO:
        RUN frbo/bofr041.p PERSISTENT SET hDBOMmvTarPlanoItem.
    END.
    RUN openQueryStatic IN hDBOMmvTarPlanoItem (INPUT "Main":U) NO-ERROR. 

    return "OK":U.
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

    create tt-param.
    assign tt-param.usuario               = c-seg-usuario
           tt-param.destino               = input frame fPage6 rsDestiny
           tt-param.data-exec             = today
           tt-param.hora-exec             = time
           tt-param.i-ordem-ini           = input frame fpage2  fi-ordem-ini       
           tt-param.i-ordem-fim           = input frame fpage2  fi-ordem-fim       
           tt-param.i-empresa-ini         = input frame fpage2  fi-empresa-ini       
           tt-param.i-empresa-fim         = input frame fpage2  fi-empresa-fim       
           tt-param.c-equipamento-ini     = input frame fpage2  fi-equipamento-ini   
           tt-param.c-equipamento-fim     = input frame fpage2  fi-equipamento-fim
           tt-param.c-especialidade-ini   = INPUT FRAME fPage2  fi-especialidade-ini 
           tt-param.c-especialidade-fim   = INPUT FRAME fPage2  fi-especialidade-fim  
           tt-param.i-tipo-manut-ini      = input frame fpage2  fi-tipo-manut-ini           
           tt-param.i-tipo-manut-fim      = input frame fpage2  fi-tipo-manut-fim 
           tt-param.i-planejador-ini      = INPUT FRAME fpage2  fi-planejador-ini
           tt-param.i-planejador-fim      = INPUT FRAME fpage2  fi-planejador-fim
           tt-param.c-oficina-ini         = input frame fpage2  fi-oficina-ini  
           tt-param.c-oficina-fim         = input frame fpage2  fi-oficina-fim  
           tt-param.dat-evento-ini        = input frame fpage2  fi-dat-evento-ini   
           tt-param.dat-evento-fim        = input frame fpage2  fi-dat-evento-fim
           tt-param.dat-base              = input frame fpage4  fi-dat-base
           tt-param.dat-tendencia-ini     = input frame fpage2  fi-dat-tendencia-ini
           tt-param.dat-tendencia-fim     = input frame fpage2  fi-dat-tendencia-fim
           tt-param.l-naoIniciada         = INPUT FRAME fPage4  tgNaoIniciada  
           tt-param.l-iniciada            = input frame fPage4  tgIniciada
           tt-param.l-equipamento         = input frame fPage4  tgEquipamento
           tt-param.l-componente          = INPUT FRAME fPage4  tgComponente
           tt-param.l-naoIniciada2        = INPUT FRAME fPage4  tgNaoIniciada-2    
           tt-param.l-iniciada2           = INPUT FRAME fPage4  tgIniciada-2
           tt-param.l-save                = INPUT FRAME fPage4  tgSave
           tt-param.i-tipo                = INPUT FRAME fPage4  rsTipo.

    if tt-param.destino = 1 
    then
        assign tt-param.arquivo = "":U.
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame fPage6 cFile.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.

    /* Coloque aqui a l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */

    /* Executar do programa RP.P que ir  criar o relat¢rio */
    {report/rpexb.i}

    SESSION:SET-WAIT-STATE("GENERAL":U).

    {report/rprun.i mvp\esmv0617rp.p}

    SESSION:SET-WAIT-STATE("":U).
end.

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


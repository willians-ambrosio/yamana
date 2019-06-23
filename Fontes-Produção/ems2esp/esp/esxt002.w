&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i esxt002 2.06.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat¢rio                            */
/* Obs: Retirar o valor do preprocessador para as p†ginas que n∆o existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA 
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG f-pg-dig
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    FIELD diretorio AS CHAR
    FIELD c-ini-empresa AS CHAR
    FIELD c-ini-estabelec AS CHAR
    FIELD c-fim-estabelec AS CHAR
    FIELD c-ini-periodo AS DATE 
    FIELD c-fim-periodo AS DATE 
    FIELD c-ini-item AS CHAR
    FIELD c-fim-item AS CHAR
    FIELD itemmaster AS LOG
    FIELD entrega AS LOG
    FIELD movto-ent-sai AS LOG
    FIELD prazo-compra AS LOG
    FIELD ordem AS LOG
    FIELD requisicao AS LOG
    FIELD donotexportitems AS LOG
    FIELD formato AS INT INITIAL 2
    FIELD cb-tp-item-ini AS CHAR
    FIELD cb-tp-item-fim AS CHAR
    FIELD c-ini-familia AS CHAR
    FIELD c-fim-familia AS CHAR
    FIELD l-aca         AS log
    field L-DIV         as log
    field L-NU2         as log
    field L-IPL         as log
    field L-NFE         as log
    field L-RCS         as log
    field L-RRQ         as log
    field l-act         as log
    field L-DRM         as log
    field L-NU3         as log
    field L-MOB         as log
    field L-NFS         as log
    field L-RDD         as log
    field L-STR         as log
    field L-NU1         as log
    field L-EAC         as log
    field L-NU4         as log
    field L-NC          as log
    field L-NFT         as log
    field L-REQ         as log
    field L-TRA         as log
    field L-DD          as log
    field L-EGF         as log
    field L-ICM         as log
    field L-NF          as log
    field L-NUS         as log
    field L-RFS         as log
    field L-ZZZ         as log
    field L-DEV         as log
    field L-BEM         as log
    field L-INV         as log
    field L-NFD         as log
    field L-REF         as log
    field L-RM          as log
    field L-SOB         as log.

    

define temp-table tt-digita no-undo
    field cod-depos        LIKE deposito.cod-depos
    field nome             LIKE deposito.nome
    field selec            AS LOG 
    index id cod-depos.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define BROWSE-NAME br-digita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-digita

/* Definitions for BROWSE br-digita                                     */
&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.cod-depos tt-digita.nome tt-digita.selec   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita   
&Scoped-define SELF-NAME br-digita
&Scoped-define QUERY-STRING-br-digita FOR EACH tt-digita
&Scoped-define OPEN-QUERY-br-digita OPEN QUERY br-digita FOR EACH tt-digita.
&Scoped-define TABLES-IN-QUERY-br-digita tt-digita
&Scoped-define FIRST-TABLE-IN-QUERY-br-digita tt-digita


/* Definitions for FRAME f-pg-dig                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-pg-dig ~
    ~{&OPEN-QUERY-br-digita}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-digita IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 ~
IMAGE-9 IMAGE-10 bt-inserir bt-alterar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-alterar 
     LABEL "Desmarcar Todos" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-inserir 
     LABEL "Marcar Todos" 
     SIZE 15 BY 1.

DEFINE IMAGE IMAGE-10
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

DEFINE BUTTON bt-arquivo 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-config-impr 
     IMAGE-UP FILE "image\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execuá∆o" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3
     SIZE 44 BY 1.08 NO-UNDO.

DEFINE VARIABLE rs-execucao AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
"Batch", 2
     SIZE 27.86 BY .92 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.14 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.14 BY 1.71.

DEFINE VARIABLE c-diretorio AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\temp" 
     LABEL "Diretorio" 
     VIEW-AS FILL-IN 
     SIZE 49 BY .88 NO-UNDO.

DEFINE VARIABLE rs-formato AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Portuguàs(Brasil)", 1,
"Inglàs(americano)", 2
     SIZE 36 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48 BY 1.5.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 4.54.

DEFINE VARIABLE l-aca AS LOGICAL INITIAL no 
     LABEL "ACA" 
     VIEW-AS TOGGLE-BOX
     SIZE 7 BY .83 NO-UNDO.

DEFINE VARIABLE l-act AS LOGICAL INITIAL no 
     LABEL "ACT" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE L-BEM AS LOGICAL INITIAL no 
     LABEL "BEM" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE L-DD AS LOGICAL INITIAL no 
     LABEL "DD" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY .83 NO-UNDO.

DEFINE VARIABLE L-DEV AS LOGICAL INITIAL no 
     LABEL "DEV" 
     VIEW-AS TOGGLE-BOX
     SIZE 7 BY .83 NO-UNDO.

DEFINE VARIABLE L-DIV AS LOGICAL INITIAL no 
     LABEL "DIV" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE l-donotexportitems AS LOGICAL INITIAL yes 
     LABEL "Do Not Export Items" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .88 NO-UNDO.

DEFINE VARIABLE L-DRM AS LOGICAL INITIAL no 
     LABEL "DRM" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE L-EAC AS LOGICAL INITIAL no 
     LABEL "EAC" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE L-EGF AS LOGICAL INITIAL no 
     LABEL "EGF" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE l-entregas AS LOGICAL INITIAL yes 
     LABEL "Issues Table" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE L-ICM AS LOGICAL INITIAL no 
     LABEL "ICM" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE L-INV AS LOGICAL INITIAL no 
     LABEL "INV" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE L-IPL AS LOGICAL INITIAL no 
     LABEL "IPL" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE l-item-master AS LOGICAL INITIAL yes 
     LABEL "ItemMaster Table" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .88 NO-UNDO.

DEFINE VARIABLE L-MOB AS LOGICAL INITIAL no 
     LABEL "MOB" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE l-movto-ent-sai AS LOGICAL INITIAL yes 
     LABEL "Last Dates In/Out - Movto Ent/Sai" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY .88 NO-UNDO.

DEFINE VARIABLE L-NC AS LOGICAL INITIAL no 
     LABEL "NC" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE L-NF AS LOGICAL INITIAL no 
     LABEL "NF" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE L-NFD AS LOGICAL INITIAL no 
     LABEL "NFD" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE L-NFE AS LOGICAL INITIAL no 
     LABEL "NFE" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE L-NFS AS LOGICAL INITIAL no 
     LABEL "NFS" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE L-NFT AS LOGICAL INITIAL no 
     LABEL "NFT" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE L-NU1 AS LOGICAL INITIAL no 
     LABEL "NU1" 
     VIEW-AS TOGGLE-BOX
     SIZE 7 BY .83 NO-UNDO.

DEFINE VARIABLE L-NU2 AS LOGICAL INITIAL no 
     LABEL "NU2" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE L-NU3 AS LOGICAL INITIAL no 
     LABEL "NU3" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE L-NU4 AS LOGICAL INITIAL no 
     LABEL "NU4" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE L-NUS AS LOGICAL INITIAL no 
     LABEL "NUS" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE l-ordem AS LOGICAL INITIAL yes 
     LABEL "Item Purchases - Ordens Compra" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .88 NO-UNDO.

DEFINE VARIABLE l-prazo-compra AS LOGICAL INITIAL yes 
     LABEL "Purchases Req. - Prazo de Compra" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .88 NO-UNDO.

DEFINE VARIABLE L-RCS AS LOGICAL INITIAL no 
     LABEL "RCS" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE L-RDD AS LOGICAL INITIAL no 
     LABEL "RDD" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE L-REF AS LOGICAL INITIAL no 
     LABEL "REF" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE L-REQ AS LOGICAL INITIAL no 
     LABEL "REQ" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE l-requisicao AS LOGICAL INITIAL yes 
     LABEL "Item Purchases - Requisiá‰es" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .88 NO-UNDO.

DEFINE VARIABLE L-RFS AS LOGICAL INITIAL no 
     LABEL "RFS" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE L-RM AS LOGICAL INITIAL no 
     LABEL "RM" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE L-RRQ AS LOGICAL INITIAL no 
     LABEL "RRQ" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE L-SOB AS LOGICAL INITIAL no 
     LABEL "SOB" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE L-STR AS LOGICAL INITIAL no 
     LABEL "STR" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE L-TRA AS LOGICAL INITIAL no 
     LABEL "TRA" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE L-ZZZ AS LOGICAL INITIAL no 
     LABEL "ZZZ" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE c-fim-estabelec AS CHARACTER FORMAT "X(5)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 7.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-fim-familia AS CHARACTER FORMAT "X(08)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE c-fim-item AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE c-fim-item-3 AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE c-fim-periodo AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE c-ini-empresa AS CHARACTER FORMAT "x(3)":U INITIAL "" 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 7.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-ini-estabelec AS CHARACTER FORMAT "X(5)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 7.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-ini-familia AS CHARACTER FORMAT "X(08)":U 
     LABEL "Familia" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE c-ini-item AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE c-ini-item-3 AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE c-ini-periodo AS DATE FORMAT "99/99/9999":U INITIAL 01/01/09 
     LABEL "Periodo" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-13
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-15
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-16
     FILENAME "image\im-fir":U
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

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1.

DEFINE IMAGE im-pg-dig
     FILENAME "image\im-fldup":U
     SIZE 15.86 BY 1.21.

DEFINE IMAGE im-pg-imp
     FILENAME "image\im-fldup":U
     SIZE 15.86 BY 1.21.

DEFINE IMAGE im-pg-par
     FILENAME "image\im-fldup":U
     SIZE 15.86 BY 1.21.

DEFINE IMAGE im-pg-sel
     FILENAME "image\im-fldup":U
     SIZE 15.86 BY 1.21.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 79 BY 1.42
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 0    
     SIZE 78.86 BY .13
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 11.38
     FGCOLOR 0 .

DEFINE RECTANGLE rt-folder-left
     EDGE-PIXELS 0    
     SIZE .43 BY 11.21
     BGCOLOR 15 .

DEFINE RECTANGLE rt-folder-right
     EDGE-PIXELS 0    
     SIZE .43 BY 11.21
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder-top
     EDGE-PIXELS 0    
     SIZE 78.86 BY .13
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-digita FOR 
      tt-digita SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-digita w-relat _FREEFORM
  QUERY br-digita DISPLAY
      tt-digita.cod-depos
      tt-digita.nome
      tt-digita.selec FORMAT "SIM/Nao"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 76.57 BY 9
         BGCOLOR 15  ROW-HEIGHT-CHARS .6.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.38 COL 3.14 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     bt-arquivo AT ROW 3.58 COL 43.14 HELP
          "Escolha do nome do arquivo"
     bt-config-impr AT ROW 3.58 COL 43.14 HELP
          "Configuraá∆o da impressora"
     c-arquivo AT ROW 3.63 COL 3.14 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 5.75 COL 3 HELP
          "Modo de Execuá∆o" NO-LABEL
     text-destino AT ROW 1.63 COL 3.86 NO-LABEL
     text-modo AT ROW 5 COL 1.14 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10 WIDGET-ID 100.

DEFINE FRAME f-pg-dig
     br-digita AT ROW 1 COL 1
     bt-inserir AT ROW 10 COL 1
     bt-alterar AT ROW 10 COL 16
     IMAGE-5 AT ROW 5 COL 32 WIDGET-ID 6
     IMAGE-6 AT ROW 5 COL 46.86 WIDGET-ID 8
     IMAGE-7 AT ROW 5 COL 32 WIDGET-ID 14
     IMAGE-8 AT ROW 5 COL 46.86 WIDGET-ID 16
     IMAGE-9 AT ROW 5 COL 32 WIDGET-ID 22
     IMAGE-10 AT ROW 5 COL 46.86 WIDGET-ID 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.31
         SIZE 76.86 BY 10.15 WIDGET-ID 100.

DEFINE FRAME f-relat
     bt-executar AT ROW 14.5 COL 3 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 14.5 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.5 COL 70 HELP
          "Ajuda"
     im-pg-sel AT ROW 1.5 COL 2.14
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-dig AT ROW 1.5 COL 33.57
     im-pg-imp AT ROW 1.5 COL 49.14
     rt-folder AT ROW 2.5 COL 2
     rt-folder-top AT ROW 2.5 COL 2.14
     rt-folder-left AT ROW 2.5 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-6 AT ROW 13.75 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar WIDGET-ID 100.

DEFINE FRAME f-pg-sel
     c-ini-empresa AT ROW 3 COL 15.57 COLON-ALIGNED WIDGET-ID 10
     c-ini-estabelec AT ROW 4 COL 15.57 COLON-ALIGNED WIDGET-ID 2
     c-fim-estabelec AT ROW 4 COL 37.72 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     c-ini-periodo AT ROW 5 COL 15.57 COLON-ALIGNED
     c-fim-periodo AT ROW 5 COL 37.57 COLON-ALIGNED NO-LABEL
     c-ini-item AT ROW 6 COL 15.57 COLON-ALIGNED WIDGET-ID 12
     c-ini-item-3 AT ROW 6 COL 15.57 COLON-ALIGNED WIDGET-ID 30
     c-fim-item AT ROW 6 COL 37.72 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     c-fim-item-3 AT ROW 6 COL 37.72 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     c-ini-familia AT ROW 7 COL 15.57 COLON-ALIGNED WIDGET-ID 22
     c-fim-familia AT ROW 7 COL 37.72 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     IMAGE-1 AT ROW 5.04 COL 32.86
     IMAGE-2 AT ROW 5.04 COL 36.29
     IMAGE-3 AT ROW 4.04 COL 32.86 WIDGET-ID 6
     IMAGE-4 AT ROW 4.04 COL 36.43 WIDGET-ID 8
     IMAGE-11 AT ROW 6 COL 36.43 WIDGET-ID 16
     IMAGE-12 AT ROW 6 COL 32.86 WIDGET-ID 18
     IMAGE-13 AT ROW 7 COL 36.43 WIDGET-ID 24
     IMAGE-14 AT ROW 7 COL 32.86 WIDGET-ID 26
     IMAGE-15 AT ROW 6 COL 36.43 WIDGET-ID 32
     IMAGE-16 AT ROW 6 COL 32.86 WIDGET-ID 34
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62
         FONT 1 WIDGET-ID 100.

DEFINE FRAME f-pg-par
     l-item-master AT ROW 1 COL 2
     l-ordem AT ROW 1 COL 37 WIDGET-ID 4
     l-requisicao AT ROW 1.75 COL 2 WIDGET-ID 6
     l-prazo-compra AT ROW 1.75 COL 37 WIDGET-ID 102
     l-entregas AT ROW 2.5 COL 2 WIDGET-ID 2
     l-donotexportitems AT ROW 2.5 COL 37 WIDGET-ID 8
     l-movto-ent-sai AT ROW 3.25 COL 37 WIDGET-ID 100
     rs-formato AT ROW 4.75 COL 4 NO-LABEL WIDGET-ID 16
     c-diretorio AT ROW 5.88 COL 9 COLON-ALIGNED WIDGET-ID 14
     l-aca AT ROW 7.5 COL 10 WIDGET-ID 24
     L-DIV AT ROW 7.5 COL 19 WIDGET-ID 54
     L-NU2 AT ROW 7.5 COL 28 WIDGET-ID 50
     L-IPL AT ROW 7.5 COL 37 WIDGET-ID 28
     L-NFE AT ROW 7.5 COL 46 WIDGET-ID 66
     L-RCS AT ROW 7.5 COL 56 WIDGET-ID 92
     L-RRQ AT ROW 7.5 COL 65 WIDGET-ID 82
     l-act AT ROW 8.25 COL 10 WIDGET-ID 46
     L-DRM AT ROW 8.25 COL 19 WIDGET-ID 44
     L-NU3 AT ROW 8.25 COL 28 WIDGET-ID 40
     L-MOB AT ROW 8.25 COL 37 WIDGET-ID 74
     L-NFS AT ROW 8.25 COL 46 WIDGET-ID 64
     L-RDD AT ROW 8.25 COL 56 WIDGET-ID 90
     L-STR AT ROW 8.25 COL 65 WIDGET-ID 80
     L-NU1 AT ROW 9 COL 10 WIDGET-ID 36
     L-EAC AT ROW 9 COL 19 WIDGET-ID 52
     L-NU4 AT ROW 9 COL 28 WIDGET-ID 30
     L-NC AT ROW 9 COL 37 WIDGET-ID 72
     L-NFT AT ROW 9 COL 46 WIDGET-ID 62
     L-REQ AT ROW 9 COL 56 WIDGET-ID 88
     L-TRA AT ROW 9 COL 65 WIDGET-ID 78
     L-DD AT ROW 9.75 COL 10 WIDGET-ID 26
     L-EGF AT ROW 9.75 COL 19 WIDGET-ID 42
     L-ICM AT ROW 9.75 COL 28 WIDGET-ID 48
     L-NF AT ROW 9.75 COL 37 WIDGET-ID 70
     L-NUS AT ROW 9.75 COL 46 WIDGET-ID 60
     L-RFS AT ROW 9.75 COL 56 WIDGET-ID 86
     L-ZZZ AT ROW 9.75 COL 65 WIDGET-ID 76
     L-DEV AT ROW 10.5 COL 10 WIDGET-ID 34
     L-BEM AT ROW 10.5 COL 19 WIDGET-ID 32
     L-INV AT ROW 10.5 COL 28 WIDGET-ID 38
     L-NFD AT ROW 10.5 COL 37 WIDGET-ID 68
     L-REF AT ROW 10.5 COL 46 WIDGET-ID 58
     L-RM AT ROW 10.5 COL 56 WIDGET-ID 84
     L-SOB AT ROW 10.5 COL 65 WIDGET-ID 94
     " Transaá∆o:" VIEW-AS TEXT
          SIZE 13 BY .75 AT ROW 6.75 COL 3 WIDGET-ID 98
     "Formato Data/Valor" VIEW-AS TEXT
          SIZE 20 BY .67 AT ROW 4.25 COL 2 WIDGET-ID 22
     RECT-10 AT ROW 4.25 COL 2 WIDGET-ID 20
     RECT-13 AT ROW 7 COL 2 WIDGET-ID 96
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.79
         SIZE 75 BY 10.96 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-relat
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-relat ASSIGN
         HIDDEN             = YES
         TITLE              = "<Title>"
         HEIGHT             = 15
         WIDTH              = 81.14
         MAX-HEIGHT         = 22.33
         MAX-WIDTH          = 114.14
         VIRTUAL-HEIGHT     = 22.33
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-relat 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-relat.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-relat
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-pg-dig
   FRAME-NAME                                                           */
/* BROWSE-TAB br-digita 1 f-pg-dig */
/* SETTINGS FOR FRAME f-pg-imp
                                                                        */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execuá∆o".

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FRAME f-relat
                                                                        */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-left IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-right IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-top IN FRAME f-relat
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-relat)
THEN w-relat:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-digita
/* Query rebuild information for BROWSE br-digita
     _START_FREEFORM
OPEN QUERY br-digita FOR EACH tt-digita.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-digita */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-imp
/* Query rebuild information for FRAME f-pg-imp
     _Query            is NOT OPENED
*/  /* FRAME f-pg-imp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-sel
/* Query rebuild information for FRAME f-pg-sel
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON END-ERROR OF w-relat /* <Title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* <Title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-digita
&Scoped-define SELF-NAME br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON DEL OF br-digita IN FRAME f-pg-dig
DO:
/*   apply 'choose' to bt-retirar in frame f-pg-dig.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON END-ERROR OF br-digita IN FRAME f-pg-dig
ANYWHERE 
DO:
    /*
    if  br-digita:new-row in frame f-pg-dig then do:
        if  avail tt-digita then
            delete tt-digita.
        if  br-digita:delete-current-row() in frame f-pg-dig then. 
    end.                                                               
    else do:
        get current br-digita.
        display tt-digita.cod-depos
                tt-digita.nome
                tt-digita.selec with browse br-digita. 
    end.
    return no-apply.
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ENTER OF br-digita IN FRAME f-pg-dig
ANYWHERE
DO:
  apply 'tab' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON INS OF br-digita IN FRAME f-pg-dig
DO:
  /* apply 'choose' to bt-inserir in frame f-pg-dig.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON MOUSE-SELECT-DBLCLICK OF br-digita IN FRAME f-pg-dig
DO:
  IF AVAIL tt-digita THEN DO:
      ASSIGN tt-digita.selec = NOT tt-digita.selec.
      br-digita:REFRESH().
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON OFF-END OF br-digita IN FRAME f-pg-dig
DO:
    /*
   apply 'entry' to bt-inserir in frame f-pg-dig.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON OFF-HOME OF br-digita IN FRAME f-pg-dig
DO:
  /*apply 'entry' to bt-recuperar in frame f-pg-dig.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ROW-ENTRY OF br-digita IN FRAME f-pg-dig
DO:
   /* trigger para inicializar campos da temp table de digitaá∆o */
    /*
    if  br-digita:new-row in frame f-pg-dig then do:
       assign tt-digita.exemplo:screen-value in browse br-digita = string(today, "99/99/9999").
   end.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ROW-LEAVE OF br-digita IN FRAME f-pg-dig
DO:
    /* ê aqui que a gravaá∆o da linha da temp-table Ç efetivada.
       PorÇm as validaá‰es dos registros devem ser feitas na procedure pi-executar,
       no local indicado pelo coment†rio */
  /*  
    if br-digita:NEW-ROW in frame f-pg-dig then 
    do transaction on error undo, return no-apply:
        create tt-digita.
        assign input browse br-digita tt-digita.ordem
               input browse br-digita tt-digita.exemplo.
    end.
    else do transaction on error undo, return no-apply:
        assign input browse br-digita tt-digita.ordem
               input browse br-digita tt-digita.exemplo.
    end.
    
    br-digita:CREATE-RESULT-LIST-ENTRY() in frame f-pg-dig.
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-relat
ON CHOOSE OF bt-ajuda IN FRAME f-relat /* Ajuda */
DO:
   {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-alterar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-alterar w-relat
ON CHOOSE OF bt-alterar IN FRAME f-pg-dig /* Desmarcar Todos */
DO:
    FOR EACH tt-digita:
        ASSIGN tt-digita.selec = NO.
    END.
    br-digita:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo w-relat
ON CHOOSE OF bt-arquivo IN FRAME f-pg-imp
DO:
    {include/i-rparq.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-relat
ON CHOOSE OF bt-cancelar IN FRAME f-relat /* Fechar */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-config-impr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-config-impr w-relat
ON CHOOSE OF bt-config-impr IN FRAME f-pg-imp
DO:
   {include/i-rpimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar w-relat
ON CHOOSE OF bt-executar IN FRAME f-relat /* Executar */
DO:
   do  on error undo, return no-apply:
       run pi-executar.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-inserir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inserir w-relat
ON CHOOSE OF bt-inserir IN FRAME f-pg-dig /* Marcar Todos */
DO:
    FOR EACH tt-digita:
        ASSIGN tt-digita.selec = YES.
    END.
    br-digita:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME im-pg-dig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-dig w-relat
ON MOUSE-SELECT-CLICK OF im-pg-dig IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-imp w-relat
ON MOUSE-SELECT-CLICK OF im-pg-imp IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-par
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-par w-relat
ON MOUSE-SELECT-CLICK OF im-pg-par IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-sel w-relat
ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino w-relat
ON VALUE-CHANGED OF rs-destino IN FRAME f-pg-imp
DO:
do  with frame f-pg-imp:
    case self:screen-value:
        when "1" then do:
            assign c-arquivo:sensitive    = no
                   bt-arquivo:visible     = no
                   bt-config-impr:visible = yes.
        end.
        when "2" then do:
            assign c-arquivo:sensitive     = yes
                   bt-arquivo:visible      = yes
                   bt-config-impr:visible  = no.
        end.
        when "3" then do:
            assign c-arquivo:sensitive     = no
                   bt-arquivo:visible      = no
                   bt-config-impr:visible  = no.
        end.
    end case.
end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-execucao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-execucao w-relat
ON VALUE-CHANGED OF rs-execucao IN FRAME f-pg-imp
DO:
   {include/i-rprse.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-dig
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */
ASSIGN c-ini-empresa = i-ep-codigo-usuario.

FOR EACH deposito:
    CREATE tt-digita.
    BUFFER-COPY deposito TO tt-digita.
END.
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "esxt002" "2.06.00.000"}

/* inicializaá‰es do template de relat¢rio */
{include/i-rpini.i}

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

{include/i-rplbl.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN enable_UI.
  
    {include/i-rpmbl.i}
  
    IF  NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-relat  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-relat  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-relat  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-relat)
  THEN DELETE WIDGET w-relat.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-relat  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE im-pg-sel im-pg-par im-pg-dig im-pg-imp bt-executar bt-cancelar 
         bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY l-item-master l-ordem l-requisicao l-prazo-compra l-entregas 
          l-donotexportitems l-movto-ent-sai rs-formato c-diretorio l-aca L-DIV 
          L-NU2 L-IPL L-NFE L-RCS L-RRQ l-act L-DRM L-NU3 L-MOB L-NFS L-RDD 
          L-STR L-NU1 L-EAC L-NU4 L-NC L-NFT L-REQ L-TRA L-DD L-EGF L-ICM L-NF 
          L-NUS L-RFS L-ZZZ L-DEV L-BEM L-INV L-NFD L-REF L-RM L-SOB 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE RECT-10 RECT-13 l-item-master l-ordem l-requisicao l-prazo-compra 
         l-entregas l-donotexportitems l-movto-ent-sai rs-formato c-diretorio 
         l-aca L-DIV L-NU2 L-IPL L-NFE L-RCS L-RRQ l-act L-DRM L-NU3 L-MOB 
         L-NFS L-RDD L-STR L-NU1 L-EAC L-NU4 L-NC L-NFT L-REQ L-TRA L-DD L-EGF 
         L-ICM L-NF L-NUS L-RFS L-ZZZ L-DEV L-BEM L-INV L-NFD L-REF L-RM L-SOB 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY c-ini-empresa c-ini-estabelec c-fim-estabelec c-ini-periodo 
          c-fim-periodo c-ini-item c-ini-item-3 c-fim-item c-fim-item-3 
          c-ini-familia c-fim-familia 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-11 IMAGE-12 IMAGE-13 IMAGE-14 
         IMAGE-15 IMAGE-16 c-ini-empresa c-ini-estabelec c-fim-estabelec 
         c-ini-periodo c-fim-periodo c-ini-item c-ini-item-3 c-fim-item 
         c-fim-item-3 c-ini-familia c-fim-familia 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 rs-destino bt-arquivo bt-config-impr c-arquivo 
         rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  ENABLE br-digita IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 IMAGE-9 IMAGE-10 bt-inserir 
         bt-alterar 
      WITH FRAME f-pg-dig IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-dig}
  VIEW w-relat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-relat 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar w-relat 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var r-tt-digita as rowid no-undo.

do on error undo, return error on stop  undo, return error:
    {include/i-rpexa.i}
    
    if input frame f-pg-imp rs-destino = 2 and
       input frame f-pg-imp rs-execucao = 1 then do:
        run utp/ut-vlarq.p (input input frame f-pg-imp c-arquivo).
        
        if return-value = "NOK":U then do:
            run utp/ut-msgs.p (input "show", input 73, input "").
            
            apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
            apply "ENTRY":U to c-arquivo in frame f-pg-imp.
            return error.
        end.
    end.
    
    /* Coloque aqui as validaá‰es da p†gina de Digitaá∆o, lembrando que elas devem
       apresentar uma mensagem de erro cadastrada, posicionar nesta p†gina e colocar
       o focus no campo com problemas */
    /*browse br-digita:SET-REPOSITIONED-ROW (browse br-digita:DOWN, "ALWAYS":U).*/
    
    /* Coloque aqui as validaá‰es das outras p†ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p†gina com 
       problemas e colocar o focus no campo com problemas */
    IF NOT CAN-FIND(FIRST tt-digita WHERE tt-digita.selec) THEN DO:
        /*run utp/ut-msgs.p (input "show", input 73, input "").*/
        MESSAGE "Favor selecionar pelo menos 1 deposito..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        apply "MOUSE-SELECT-CLICK":U to im-pg-dig in frame f-relat.
        apply "ENTRY":U to br-digita in frame f-pg-dig.
        return error.
    END.
    
    
    /* Aqui s∆o gravados os campos da temp-table que ser† passada como parÉmetro
       para o programa RP.P */
    
    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame f-pg-imp rs-destino
           tt-param.data-exec       = today
           tt-param.hora-exec       = time
        /*   tt-param.classifica      = input frame f-pg-cla rs-classif
           tt-param.desc-classifica = entry((tt-param.classifica - 1) * 2 + 1, 
                                            rs-classif:radio-buttons in frame f-pg-cla) */.
    
    ASSIGN tt-param.diretorio        = input frame f-pg-par c-diretorio
           tt-param.c-ini-empresa    = input frame f-pg-sel c-ini-empresa
           tt-param.c-ini-estabelec  = input frame f-pg-sel c-ini-estabelec
           tt-param.c-fim-estabelec  = input frame f-pg-sel c-fim-estabelec
           tt-param.c-ini-periodo    = input frame f-pg-sel c-ini-periodo
           tt-param.c-fim-periodo    = input frame f-pg-sel c-fim-periodo
           tt-param.c-ini-item       = input frame f-pg-sel c-ini-item
           tt-param.c-fim-item       = input frame f-pg-sel c-fim-item
           tt-param.itemmaster       = input frame f-pg-par l-item-master
           tt-param.entrega          = input frame f-pg-par l-entregas
           tt-param.movto-ent-sai    = INPUT FRAME f-pg-par l-movto-ent-sai
           tt-param.prazo-compra     = INPUT FRAME f-pg-par l-prazo-compra
           tt-param.ordem            = input frame f-pg-par l-ordem
           tt-param.requisicao       = input frame f-pg-par l-requisicao
           tt-param.formato          = input frame f-pg-par rs-formato
           tt-param.donotexportitems = input frame f-pg-par l-donotexportitems
/*            tt-param.cb-tp-item-ini  = input frame f-pg-sel  cb-tp-item-ini */
/*            tt-param.cb-tp-item-fim  = input frame f-pg-sel  cb-tp-item-fim */
           tt-param.c-ini-familia    = input frame f-pg-sel  c-ini-familia 
           tt-param.c-fim-familia    = input frame f-pg-sel  c-fim-familia
           tt-param.l-aca            = input frame f-pg-par  l-aca  
           tt-param.L-DIV            = input frame f-pg-par  L-DIV  
           tt-param.L-NU2            = input frame f-pg-par  L-NU2  
           tt-param.L-IPL            = input frame f-pg-par  L-IPL  
           tt-param.L-NFE            = input frame f-pg-par  L-NFE  
           tt-param.L-RCS            = input frame f-pg-par  L-RCS  
           tt-param.L-RRQ            = input frame f-pg-par  L-RRQ  
           tt-param.l-act            = input frame f-pg-par  l-act  
           tt-param.L-DRM            = input frame f-pg-par  L-DRM  
           tt-param.L-NU3            = input frame f-pg-par  L-NU3  
           tt-param.L-MOB            = input frame f-pg-par  L-MOB  
           tt-param.L-NFS            = input frame f-pg-par  L-NFS  
           tt-param.L-RDD            = input frame f-pg-par  L-RDD  
           tt-param.L-STR            = input frame f-pg-par  L-STR  
           tt-param.L-NU1            = input frame f-pg-par  L-NU1  
           tt-param.L-EAC            = input frame f-pg-par  L-EAC  
           tt-param.L-NU4            = input frame f-pg-par  L-NU4  
           tt-param.L-NC             = input frame f-pg-par  L-NC   
           tt-param.L-NFT            = input frame f-pg-par  L-NFT  
           tt-param.L-REQ            = input frame f-pg-par  L-REQ  
           tt-param.L-TRA            = input frame f-pg-par  L-TRA  
           tt-param.L-DD             = input frame f-pg-par  L-DD   
           tt-param.L-EGF            = input frame f-pg-par  L-EGF  
           tt-param.L-ICM            = input frame f-pg-par  L-ICM  
           tt-param.L-NF             = input frame f-pg-par  L-NF   
           tt-param.L-NUS            = input frame f-pg-par  L-NUS  
           tt-param.L-RFS            = input frame f-pg-par  L-RFS  
           tt-param.L-ZZZ            = input frame f-pg-par  L-ZZZ  
           tt-param.L-DEV            = input frame f-pg-par  L-DEV  
           tt-param.L-BEM            = input frame f-pg-par  L-BEM  
           tt-param.L-INV            = input frame f-pg-par  L-INV  
           tt-param.L-NFD            = input frame f-pg-par  L-NFD  
           tt-param.L-REF            = input frame f-pg-par  L-REF  
           tt-param.L-RM             = input frame f-pg-par  L-RM   
           tt-param.L-SOB            = input frame f-pg-par  L-SOB. 


    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".
    
    /* Coloque aqui a l¢gica de gravaá∆o dos demais campos que devem ser passados
       como parÉmetros para o programa RP.P, atravÇs da temp-table tt-param */
    
    FOR EACH tt-digita:
        CREATE tt-raw-digita.
        raw-transfer tt-digita to tt-raw-digita.raw-digita NO-ERROR.
    END.
    
    /* Executar do programa RP.P que ir† criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).

    {include/i-rprun.i esp/esxt002rp.p}
    
    {include/i-rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
    /*{include/i-rptrm.i}*/
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina w-relat 
PROCEDURE pi-troca-pagina :
/*------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P†gina (folder)   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{include/i-rptrp.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-relat  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-digita"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-relat 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  
  run pi-trata-state (p-issuer-hdl, p-state).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


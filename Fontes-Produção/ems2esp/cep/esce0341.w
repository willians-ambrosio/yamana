&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgadm            PROGRESS
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
{include/i-prgvrs.i ESCE0341 2.00.00.013}  /*** 010013 ***/

def buffer param_seg_estab for ems2cadme.param_seg_estab.

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ce0341 MCE}
&ENDIF

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
{cdp/cdcfgmat.i}
{cdp/cd0019.i "MCE"} /*Seguranáa por Estabelecimento*/

/* Temporary Table Definitions ---                                      */

def temp-table tt-param
    field destino        as integer
    field arquivo        as char
    field usuario        as char
    field data-exec      as date
    field hora-exec      as integer
    field classifica     as integer
    field ge-ini         as integer
    field ge-fim         as integer
    field familia-ini    as char
    field familia-fim    as char
    field item-ini       as char
    field item-fim       as char
    field estabelec-ini  as char
    field estabelec-fim  as char
    field data-ini       as date
    field data-fim       as date
    field pto-enc        as logical
    field periodico      as logical
    field l-multip       like estabelec.usa-mensal
    field l-agrupa       like estabelec.usa-mensal
    field l-split        like estabelec.usa-mensal
    field c-classe       as char format "x(40)"
    field c-destino      as char
    field i-icms         as int 
    field c-requisitante like ordem-compra.requisitante.


def temp-table tt-digita
    field cod-estabel   like necessidade-oc.cod-estabel
    field it-codigo     like necessidade-oc.it-codigo
    field data-entrega  like necessidade-oc.data-entrega
    field data-geracao  like necessidade-oc.data-geracao
    field estoque-dispo like necessidade-oc.estoque-dispo
    field qt-orig       like necessidade-oc.qt-orig
    field qt-ordem      like necessidade-oc.qt-ordem
    field qt-pendente   like necessidade-oc.qt-pendente
    field c-geracao     as char 
    field rw-nec        as rowid
    field marca         as char format "x(01)" label ""
    index codigo marca
                 cod-estabel
                 it-codigo
                 data-geracao.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.

/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.

def var l-nova-selecao     as logical init yes.
def var r-row-digita       as rowid   no-undo.
def var l-inicio           as logical no-undo.
def var i-cont             as int     no-undo.
def var l-gerou-pela-digit as logical no-undo.

DEFINE VARIABLE c-lista AS CHARACTER NO-UNDO.

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
&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.marca tt-digita.it-codigo tt-digita.cod-estabel tt-digita.c-geracao tt-digita.data-geracao tt-digita.data-entrega tt-digita.qt-ordem tt-digita.qt-orig tt-digita.qt-pendente tt-digita.estoque-dispo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-digita.qt-ordem ~
tt-digita.data-entrega   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-digita tt-digita
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-digita tt-digita
&Scoped-define SELF-NAME br-digita
&Scoped-define QUERY-STRING-br-digita FOR EACH tt-digita
&Scoped-define OPEN-QUERY-br-digita OPEN QUERY br-digita FOR EACH tt-digita.
&Scoped-define TABLES-IN-QUERY-br-digita tt-digita
&Scoped-define FIRST-TABLE-IN-QUERY-br-digita tt-digita


/* Definitions for FRAME f-pg-dig                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-pg-dig ~
    ~{&OPEN-QUERY-br-digita}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-digita bt-marca bt-todos bt-nenhum 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-marca 
     LABEL "&Selecionar" 
     SIZE 15.14 BY 1.

DEFINE BUTTON bt-nenhum 
     LABEL "&Nenhuma" 
     SIZE 15.14 BY 1.

DEFINE BUTTON bt-todos 
     LABEL "&Todas" 
     SIZE 15.14 BY 1.

DEFINE BUTTON bt-arquivo 
     IMAGE-UP FILE "image~\im-sea":U
     IMAGE-INSENSITIVE FILE "image~\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-config-impr 
     IMAGE-UP FILE "image~\im-cfprt":U
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
     SIZE 27.72 BY .92 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE cb-icms AS CHARACTER FORMAT "X(256)":U 
     LABEL "ICMS" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 18.86 BY 1 NO-UNDO.

DEFINE VARIABLE c-ordem AS CHARACTER FORMAT "X(256)":U INITIAL "Geraá∆o das Ordens" 
      VIEW-AS TEXT 
     SIZE 21.14 BY .67 NO-UNDO.

DEFINE VARIABLE fi-requisitante AS CHARACTER FORMAT "X(12)":U 
     LABEL "Requisitante" 
     VIEW-AS FILL-IN 
     SIZE 18.72 BY .88 NO-UNDO.

DEFINE VARIABLE l-nec AS CHARACTER FORMAT "X(256)":U INITIAL "Tipo de Ressuprimento das Necessidades" 
      VIEW-AS TEXT 
     SIZE 41 BY .67 NO-UNDO.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 76.72 BY 10.67.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 64.14 BY 4.29.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 64 BY 4.29.

DEFINE VARIABLE l-multip AS LOGICAL INITIAL no 
     LABEL "Gerar Ordens M£ltiplas" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.14 BY .83 NO-UNDO.

DEFINE VARIABLE l-periodico AS LOGICAL INITIAL no 
     LABEL "Peri¢dico" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.14 BY .83 NO-UNDO.

DEFINE VARIABLE l-pto-enc AS LOGICAL INITIAL no 
     LABEL "Ponto Encomenda" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.14 BY .83 NO-UNDO.

DEFINE VARIABLE l-split AS LOGICAL INITIAL no 
     LABEL "Fazer Divis∆o das Ordens" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.14 BY .83 NO-UNDO.

DEFINE VARIABLE tg-considera AS LOGICAL INITIAL no 
     LABEL "Considera (Xtivity)" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .83 NO-UNDO.

DEFINE VARIABLE c-estabelec-fim LIKE estabelec.cod-estabel
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-estabelec-ini LIKE estabelec.cod-estabel
     LABEL "Estab":R7 
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-familia-fim AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-familia-ini AS CHARACTER FORMAT "X(8)":U 
     LABEL "Fam°lia" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-item-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .88 NO-UNDO.

DEFINE VARIABLE c-item-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .88 NO-UNDO.

DEFINE VARIABLE da-data-fim AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE da-data-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Geraá∆o" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE i-ge-fim AS INTEGER FORMAT "99":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE i-ge-ini AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Grupo Estoque" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-10
     FILENAME "image~\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image~\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image~\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-13
     FILENAME "image~\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image~\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image~\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image~\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image~\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image~\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image~\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 77.29 BY 10.63.

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
     FILENAME "image~\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-imp
     FILENAME "image~\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-par
     FILENAME "image~\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-sel
     FILENAME "image~\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 79 BY 1.42
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 76.14 BY 10.5.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 0  
     SIZE 78.72 BY .13
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
     SIZE .43 BY 11.17
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder-top
     EDGE-PIXELS 0  
     SIZE 78.72 BY .13
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
      tt-digita.marca
      tt-digita.it-codigo     
      tt-digita.cod-estabel 
      tt-digita.c-geracao  
      tt-digita.data-geracao
      tt-digita.data-entrega
      tt-digita.qt-ordem
      tt-digita.qt-orig             
      tt-digita.qt-pendente   
      tt-digita.estoque-dispo
ENABLE
      tt-digita.qt-ordem
      tt-digita.data-entrega
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 76.57 BY 9.38
         BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     RECT-1 AT ROW 14.29 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     im-pg-dig AT ROW 1.5 COL 33.57
     im-pg-imp AT ROW 1.5 COL 49.29
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-sel AT ROW 1.5 COL 2.14
     RECT-13 AT ROW 3 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.38 COL 3.29 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     bt-config-impr AT ROW 3.58 COL 43.29 HELP
          "Configuraá∆o da impressora"
     bt-arquivo AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     c-arquivo AT ROW 3.63 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 5.75 COL 3 HELP
          "Modo de Execuá∆o" NO-LABEL
     text-destino AT ROW 1.63 COL 3.86 NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.88
         SIZE 77.57 BY 10.88.

DEFINE FRAME f-pg-par
     tg-considera AT ROW 3 COL 46 WIDGET-ID 2
     l-pto-enc AT ROW 3 COL 12.86
     l-periodico AT ROW 4.17 COL 12.86
     l-multip AT ROW 7.92 COL 12.86
     l-split AT ROW 9.08 COL 12.86
     cb-icms AT ROW 7.92 COL 43.14
     fi-requisitante AT ROW 9 COL 47.14 COLON-ALIGNED
     l-nec AT ROW 1.25 COL 8 COLON-ALIGNED NO-LABEL
     c-ordem AT ROW 6.25 COL 7.86 COLON-ALIGNED NO-LABEL
     RECT-16 AT ROW 1 COL 1
     RECT-17 AT ROW 1.5 COL 8.86
     RECT-19 AT ROW 6.54 COL 8.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 77 BY 10.67.

DEFINE FRAME f-pg-dig
     br-digita AT ROW 1 COL 1
     bt-marca AT ROW 10.46 COL 1.29
     bt-todos AT ROW 10.46 COL 16.43
     bt-nenhum AT ROW 10.46 COL 31.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.96
         SIZE 77.43 BY 10.75.

DEFINE FRAME f-pg-sel
     c-estabelec-ini AT ROW 3.46 COL 21.14 COLON-ALIGNED
          LABEL "Estab":R7
     c-estabelec-fim AT ROW 3.46 COL 51.72 COLON-ALIGNED NO-LABEL
     i-ge-ini AT ROW 4.46 COL 21.14 COLON-ALIGNED
     i-ge-fim AT ROW 4.46 COL 51.72 COLON-ALIGNED NO-LABEL
     c-familia-ini AT ROW 5.46 COL 21.14 COLON-ALIGNED
     c-familia-fim AT ROW 5.46 COL 51.72 COLON-ALIGNED NO-LABEL
     c-item-ini AT ROW 6.46 COL 21.14 COLON-ALIGNED
     c-item-fim AT ROW 6.46 COL 51.72 COLON-ALIGNED NO-LABEL
     da-data-ini AT ROW 7.46 COL 21.14 COLON-ALIGNED
     da-data-fim AT ROW 7.46 COL 51.72 COLON-ALIGNED NO-LABEL
     IMAGE-10 AT ROW 4.46 COL 51
     IMAGE-11 AT ROW 3.46 COL 51
     IMAGE-12 AT ROW 6.46 COL 51
     IMAGE-13 AT ROW 7.54 COL 51
     IMAGE-14 AT ROW 7.54 COL 44.86
     IMAGE-3 AT ROW 6.46 COL 44.86
     IMAGE-4 AT ROW 5.46 COL 44.86
     IMAGE-6 AT ROW 4.46 COL 44.86
     IMAGE-7 AT ROW 3.46 COL 44.86
     IMAGE-9 AT ROW 5.46 COL 51
     RECT-15 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.96
         SIZE 77.29 BY 10.63.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-relat
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-relat ASSIGN
         HIDDEN             = YES
         TITLE              = "Ressuprimento dos Estoques"
         HEIGHT             = 15
         WIDTH              = 81.14
         MAX-HEIGHT         = 22.33
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.33
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
   Custom                                                               */
/* SETTINGS FOR FILL-IN c-ordem IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cb-icms IN FRAME f-pg-par
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN l-nec IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FILL-IN c-estabelec-fim IN FRAME f-pg-sel
   LIKE = mgadm.estabelec.cod-estabel EXP-LABEL EXP-SIZE                */
/* SETTINGS FOR FILL-IN c-estabelec-ini IN FRAME f-pg-sel
   LIKE = mgadm.estabelec.cod-estabel EXP-LABEL EXP-SIZE                */
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
OPEN QUERY br-digita FOR EACH tt-digita
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
ON END-ERROR OF w-relat /* Ressuprimento dos Estoques */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Ressuprimento dos Estoques */
DO:
  /* This event will close the window and terminate the procedure.  */
  {cdp/cd0019.i1 "MCC"} /*Seguranáa por Estabelecimento*/
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-digita
&Scoped-define SELF-NAME br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON END-ERROR OF br-digita IN FRAME f-pg-dig
ANYWHERE 
DO:
    if  br-digita:new-row in frame f-pg-dig then do:
        if  avail tt-digita then
            delete tt-digita.
        if  br-digita:delete-current-row() in frame f-pg-dig then. 
    end.                                                               
    else do:
        get current br-digita.
        display tt-digita.it-codigo     
                tt-digita.cod-estabel   
                tt-digita.data-geracao
                tt-digita.data-entrega
                tt-digita.estoque-dispo 
                tt-digita.qt-ordem  
                tt-digita.qt-orig     
                tt-digita.qt-pendente   
                tt-digita.marca with browse br-digita.
    end.
    return no-apply.
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
ON MOUSE-SELECT-DBLCLICK OF br-digita IN FRAME f-pg-dig
DO:
  if avail tt-digita then do:
      if  tt-digita.marca = "*":R then
          assign tt-digita.marca = " ".
      else
          assign tt-digita.marca = "*":R.
      disp tt-digita.marca with browse br-digita.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON OFF-END OF br-digita IN FRAME f-pg-dig
DO:
  /* apply 'entry' to bt-inserir in frame f-pg-dig.*/
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

   if avail tt-digita and tt-digita.marca = " " then do:
       bell. 
       return no-apply.
   end.

   /* trigger para inicializar campos da temp table de digitaá∆o */
   /*if  br-digita:new-row in frame f-pg-dig then do:
       assign tt-digita.exemplo:screen-value in browse br-digita = string(today, "99/99/9999").
   end.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ROW-LEAVE OF br-digita IN FRAME f-pg-dig
DO:
    if br-digita:new-row then br-digita:CREATE-RESULT-LIST-ENTRY() in frame f-pg-dig.
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
   {cdp/cd0019.i1 "MCC"} /*Seguranáa por Estabelecimento*/
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
    find first tt-digita where tt-digita.marca = "*" no-error.
    if avail tt-digita then /* Verifica alteraá‰es somente qdo houve intená∆o de gerar somente as ordens de compra da Digitacao */
        assign l-gerou-pela-digit = yes.
    else do:
        run utp/ut-msgs.p (input "show", input 29122, input "").
        if return-value = "no" then do:
            apply "mouse-select-click" to im-pg-sel.
            return no-apply.
        end.

        for each tt-digita:  /* Nenhum registro foi selecionado na digitacao entao nenhum sera passado ao ce0341rp */
            delete tt-digita.
        end.
    end.

    do  on error undo, return no-apply:
        run pi-executar.
    end.

    if l-gerou-pela-digit then /* Gera novas tt-digita somente se houve geracao de ordem pela Digitacao */
        run pi-carrega-browse (yes).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-marca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-marca w-relat
ON CHOOSE OF bt-marca IN FRAME f-pg-dig /* Selecionar */
DO:
    if available tt-digita then
    apply "mouse-select-dblclick" to browse br-digita.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-nenhum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nenhum w-relat
ON CHOOSE OF bt-nenhum IN FRAME f-pg-dig /* Nenhuma */
DO:
    for each tt-digita:
        assign tt-digita.marca = " ".
    end.
    open query br-digita for each tt-digita WHERE tt-digita.marca = " ".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos w-relat
ON CHOOSE OF bt-todos IN FRAME f-pg-dig /* Todas */
DO:
    run pi-carrega-browse (yes).
    for each tt-digita:
        assign tt-digita.marca = "*":R.
    end.
    open query br-digita for each tt-digita.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME fi-requisitante
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-requisitante w-relat
ON F5 OF fi-requisitante IN FRAME f-pg-par /* Requisitante */
DO:
    ASSIGN l-implanta = NO.
    {include/zoomvar.i &prog-zoom=inzoom/z01in386.w
                       &campo=fi-requisitante
                       &campozoom=nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-requisitante w-relat
ON MOUSE-SELECT-DBLCLICK OF fi-requisitante IN FRAME f-pg-par /* Requisitante */
DO:
    APPLY "F5" TO fi-requisitante IN FRAME f-pg-par.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME im-pg-dig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-dig w-relat
ON MOUSE-SELECT-CLICK OF im-pg-dig IN FRAME f-relat
DO:
    if  l-nova-selecao = yes then do:
        run pi-carrega-browse (yes).
        assign l-nova-selecao = no.
    end.
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
    assign l-nova-selecao = yes.
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-sel w-relat
ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-relat
DO:
    assign l-nova-selecao = yes.
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME l-multip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-multip w-relat
ON VALUE-CHANGED OF l-multip IN FRAME f-pg-par /* Gerar Ordens M£ltiplas */
DO:

if l-multip:checked in frame {&frame-name} = yes then do:
    disable l-split with frame {&frame-name}.
    assign l-split = no.
end.
else do:
    enable l-split with frame {&frame-name}.
end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME l-split
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-split w-relat
ON VALUE-CHANGED OF l-split IN FRAME f-pg-par /* Fazer Divis∆o das Ordens */
DO:

if l-split:checked in frame {&frame-name} = yes then do:
    disable l-multip with frame {&frame-name}.
    assign l-multip = no.
end.
else do:
    &IF '{&bf_mat_versao_ems}' < '2.08' &THEN
    enable l-multip with frame {&frame-name}.
    &ENDIF
end.

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


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME tg-considera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-considera w-relat
ON VALUE-CHANGED OF tg-considera IN FRAME f-pg-par /* Considera (Xtivity) */
DO:
  run pi-carrega-browse (yes).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-dig
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESCE0341" "2.00.00.013"}

/* inicializaá‰es do template de relat¢rio */
{include/i-rpini.i}

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   {cdp/cd0019.i1 "MCC"} /*Seguranáa por Estabelecimento*/
   RUN disable_UI.
END.
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
    {utp/ut-field.i mgind necessidade-oc tp-geracao 1}
    assign tt-digita.c-geracao:label in browse br-digita = return-value
           da-data-ini:screen-value in frame f-pg-sel    = string(today - 30)
           da-data-fim:screen-value in frame f-pg-sel    = string(today).

    ASSIGN c-lista = {ininc/i01in082.i 03}.

    &IF "{&FNC_MULTI_IDIOMA}" = "Yes" &THEN
        RUN utp/ut-lstit.p (INPUT-OUTPUT c-lista).
        ASSIGN cb-icms:LIST-ITEM-PAIRS IN FRAME f-pg-par = c-lista.
    &ELSE
        ASSIGN cb-icms:LIST-ITEMS      IN FRAME f-pg-par = c-lista.
    &ENDIF
    
    /* Consumo, Industrializaá∆o - inicial Ç industrializaá∆o */
    ASSIGN cb-icms:SCREEN-VALUE IN FRAME f-pg-par = {ininc/i01in082.i 04 2}.

    /*Lupa do zoom*/
    IF  fi-requisitante:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME f-pg-par THEN.

    &IF "{&bf_mat_versao_ems}" < "2.04"  &THEN
         ASSIGN cb-icms:VISIBLE         IN FRAME f-pg-par = NO
                fi-requisitante:VISIBLE IN FRAME f-pg-par = NO.
    &ENDIF
    
    IF i-pais-impto-usuario <> 1 THEN
        ASSIGN cb-icms:VISIBLE IN FRAME f-pg-par = NO.

    &IF "{&mguni_version}" >= "2.071" &THEN
    ASSIGN c-estabelec-fim:SCREEN-VALUE IN FRAME f-pg-sel = "ZZZZZ".
    &ELSE
    ASSIGN c-estabelec-fim:SCREEN-VALUE IN FRAME f-pg-sel = "ZZZ".
    &ENDIF

    IF  NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.


ON 'LEAVE' OF tt-digita.qt-ordem DO:
    IF  AVAIL tt-digita AND tt-digita.marca = "*" THEN DO:

        find CURRENT tt-digita exclusive-lock.

        for first necessidade-oc fields (qt-pendente) where
            rowid(necessidade-oc) = tt-digita.rw-nec no-lock: end.

        IF  NOT AVAIL necessidade-oc THEN RETURN NO-APPLY.

        if dec(tt-digita.qt-ordem:screen-value in browse br-digita) > necessidade-oc.qt-pendente then do:
            
            run utp/ut-msgs.p (input "show",
                               input 16235,
                               input " ").
            /*apply "entry" to tt-digita.qt-ordem in browse br-digita.                   
            return no-apply.            */
        end.

        assign tt-digita.qt-ordem     = dec(tt-digita.qt-ordem:screen-value in browse br-digita).

        disp tt-digita.qt-ordem
             with browse br-digita.

        find CURRENT tt-digita NO-LOCK.
    end.        
END.
ON 'LEAVE' OF tt-digita.data-entrega DO:
    IF  AVAIL tt-digita AND tt-digita.marca = "*" THEN DO:

        find CURRENT tt-digita exclusive-lock.

        for first necessidade-oc fields (qt-pendente) where
            rowid(necessidade-oc) = tt-digita.rw-nec no-lock: end.

        IF  NOT AVAIL necessidade-oc THEN RETURN NO-APPLY.

        assign tt-digita.data-entrega = date(tt-digita.data-entrega:screen-value in browse br-digita).
        find CURRENT tt-digita NO-LOCK.
    end.        
END.
ON 'LEAVE' OF tt-digita.qt-pendente DO:

    IF  AVAIL tt-digita AND tt-digita.marca = "*" THEN DO:

        find CURRENT tt-digita exclusive-lock.

        for first necessidade-oc fields (qt-pendente) where
            rowid(necessidade-oc) = tt-digita.rw-nec no-lock: end.

        IF  NOT AVAIL necessidade-oc THEN RETURN NO-APPLY.

        if dec(tt-digita.qt-ordem:screen-value in browse br-digita) > necessidade-oc.qt-pendente then do:
            
            run utp/ut-msgs.p (input "show",
                               input 16235,
                               input " ").
            /*apply "entry" to tt-digita.qt-ordem in browse br-digita.                   
            return no-apply.            */
        end.

        assign tt-digita.qt-pendente  = if (necessidade-oc.qt-pendente - tt-digita.qt-ordem) > 0 then
                                            (necessidade-oc.qt-pendente - tt-digita.qt-ordem)
                                        else 0.

        disp tt-digita.qt-pendente 
             with browse br-digita.

        find CURRENT tt-digita NO-LOCK.
    end.  
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
  ENABLE im-pg-dig im-pg-imp im-pg-par im-pg-sel RECT-13 bt-executar 
         bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 rs-destino bt-config-impr bt-arquivo c-arquivo 
         rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  ENABLE br-digita bt-marca bt-todos bt-nenhum 
      WITH FRAME f-pg-dig IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-dig}
  DISPLAY c-estabelec-ini c-estabelec-fim i-ge-ini i-ge-fim c-familia-ini 
          c-familia-fim c-item-ini c-item-fim da-data-ini da-data-fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-10 IMAGE-11 IMAGE-12 IMAGE-13 IMAGE-14 IMAGE-3 IMAGE-4 IMAGE-6 
         IMAGE-7 IMAGE-9 RECT-15 c-estabelec-ini c-estabelec-fim i-ge-ini 
         i-ge-fim c-familia-ini c-familia-fim c-item-ini c-item-fim da-data-ini 
         da-data-fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY tg-considera l-pto-enc l-periodico l-multip l-split cb-icms fi-requisitante l-nec 
          c-ordem 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE tg-considera l-pto-enc l-periodico l-multip l-split cb-icms fi-requisitante RECT-16 
         RECT-17 RECT-19 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
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
   {cdp/cd0019.i1 "MCC"} /*Seguranáa por Estabelecimento*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.

   RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-browse w-relat 
PROCEDURE pi-carrega-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def input parameter p-cria-novos as logical.
    assign l-inicio = p-cria-novos.

    if tg-considera:screen-value in frame f-pg-par = string(yes) then do:
        if l-inicio then
            for each tt-digita:
            delete tt-digita.
        end.
        if can-find(first esp-necessidade-oc) then
            for each estabelec fields (cod-estabel) where
                     estabelec.cod-estabel >= input frame f-pg-sel c-estabelec-ini and
                     estabelec.cod-estabel <= input frame f-pg-sel c-estabelec-fim no-lock:
            for each esp-necessidade-oc where
                     esp-necessidade-oc.cod-estabel   = estabelec.cod-estabel            and
                     esp-necessidade-oc.it-codigo    >= input frame f-pg-sel c-item-ini  and
                     esp-necessidade-oc.it-codigo    <= input frame f-pg-sel c-item-fim  and
                     esp-necessidade-oc.data-geracao >= input frame f-pg-sel da-data-ini and
                     esp-necessidade-oc.data-geracao <= input frame f-pg-sel da-data-fim and 
                     esp-necessidade-oc.qt-pendente   > 0 no-lock: 
                for first item fields (it-codigo fm-codigo ge-codigo) where
                    item.it-codigo = esp-necessidade-oc.it-codigo no-lock: end.
                    if item.ge-codigo > input frame f-pg-sel i-ge-fim or
                        item.ge-codigo < input frame f-pg-sel i-ge-ini then
                        next.
                    if item.fm-codigo > input frame f-pg-sel c-familia-fim or
                        item.fm-codigo < input frame f-pg-sel c-familia-ini then
                        next.
                    if not (item.ge-codigo >= input frame f-pg-sel i-ge-ini and
                            item.ge-codigo <= input frame f-pg-sel i-ge-fim) then
                        next.
                    if (esp-necessidade-oc.tp-geracao = 1 and l-pto-enc:checked in frame f-pg-par) or
                       (esp-necessidade-oc.tp-geracao = 2 and l-periodico:checked in frame f-pg-par) or
                       (esp-necessidade-oc.tp-geracao = 3 and l-periodico:checked in frame f-pg-par) then do:

                        create tt-digita.
                        assign tt-digita.cod-estabel   = esp-necessidade-oc.cod-estabel
                               tt-digita.it-codigo     = esp-necessidade-oc.it-codigo
                               tt-digita.data-entrega  = esp-necessidade-oc.data-entrega
                               tt-digita.data-geracao  = esp-necessidade-oc.data-geracao
                               tt-digita.estoque-dispo = esp-necessidade-oc.estoque-dispo
                               tt-digita.it-codigo     = esp-necessidade-oc.it-codigo
                               tt-digita.qt-ordem      = esp-necessidade-oc.qt-ordem
                               tt-digita.qt-orig       = esp-necessidade-oc.qt-orig
                               tt-digita.qt-pendente   = esp-necessidade-oc.qt-pendente
                               tt-digita.c-geracao     = {ininc/i01in658.i 04 esp-necessidade-oc.tp-geracao}
                               tt-digita.rw-nec        = rowid(esp-necessidade-oc)
                               tt-digita.marca         = "*".

                    end.
            end.
        end.
        assign l-inicio = no.
        open query br-digita for each tt-digita.
    end.
    else do:
        if l-inicio then
            for each tt-digita:
                delete tt-digita.
            end.
        if can-find(first necessidade-oc) then
        for each estabelec fields (cod-estabel) where
            estabelec.cod-estabel >= input frame f-pg-sel c-estabelec-ini and
            estabelec.cod-estabel <= input frame f-pg-sel c-estabelec-fim no-lock:
            for each necessidade-oc where
                necessidade-oc.cod-estabel   = estabelec.cod-estabel            and
                necessidade-oc.it-codigo    >= input frame f-pg-sel c-item-ini  and
                necessidade-oc.it-codigo    <= input frame f-pg-sel c-item-fim  and
                necessidade-oc.data-geracao >= input frame f-pg-sel da-data-ini and
                necessidade-oc.data-geracao <= input frame f-pg-sel da-data-fim and 
                necessidade-oc.qt-pendente   > 0 no-lock: 
                for first item fields (it-codigo fm-codigo ge-codigo) where
                     item.it-codigo = necessidade-oc.it-codigo no-lock: end.
                if item.ge-codigo > input frame f-pg-sel i-ge-fim or
                   item.ge-codigo < input frame f-pg-sel i-ge-ini then
                    next.
                if item.fm-codigo > input frame f-pg-sel c-familia-fim or
                   item.fm-codigo < input frame f-pg-sel c-familia-ini then
                    next.
                if not (item.ge-codigo >= input frame f-pg-sel i-ge-ini and
                        item.ge-codigo <= input frame f-pg-sel i-ge-fim) then
                    next.
                if (necessidade-oc.tp-geracao = 1 and l-pto-enc:checked in frame f-pg-par) or
                   (necessidade-oc.tp-geracao = 2 and l-periodico:checked in frame f-pg-par) or
                   (necessidade-oc.tp-geracao = 3 and l-periodico:checked in frame f-pg-par) then do:
                    create tt-digita.
                    assign tt-digita.cod-estabel   = necessidade-oc.cod-estabel
                           tt-digita.it-codigo     = necessidade-oc.it-codigo
                           tt-digita.data-entrega  = necessidade-oc.data-entrega
                           tt-digita.data-geracao  = necessidade-oc.data-geracao
                           tt-digita.estoque-dispo = necessidade-oc.estoque-dispo
                           tt-digita.it-codigo     = necessidade-oc.it-codigo
                           tt-digita.qt-ordem      = necessidade-oc.qt-ordem
                           tt-digita.qt-orig       = necessidade-oc.qt-orig
                           tt-digita.qt-pendente   = necessidade-oc.qt-pendente
                           tt-digita.c-geracao     = {ininc/i01in658.i 04 necessidade-oc.tp-geracao}
                           tt-digita.rw-nec        = rowid(necessidade-oc)
                           tt-digita.marca         = "*".
                end.
            end.
        end.
        assign l-inicio = no.
        open query br-digita for each tt-digita.
    end.
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
DEF VAR r-tt-digita AS ROWID NO-UNDO.
DEF VAR c-icms      AS CHAR  NO-UNDO.

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
    browse br-digita:SET-REPOSITIONED-ROW (browse br-digita:DOWN, "ALWAYS":U).
    for each tt-digita no-lock:
        assign r-tt-digita = rowid(tt-digita).
        if tt-digita.data-entrega < today then do:
            run utp/ut-msgs.p (input "show",
                               input 8313,
                               input " ").
            assign browse br-digita:CURRENT-COLUMN = tt-digita.data-entrega:HANDLE in browse br-digita.
            apply "MOUSE-SELECT-CLICK":U to im-pg-dig in frame f-relat.
            reposition br-digita to rowid r-tt-digita.
            apply "entry" to tt-digita.data-entrega in browse br-digita.                   
            return error.            
        end.

    end.

    /* Coloque aqui as validaá‰es das outras p†ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p†gina com 
       problemas e colocar o focus no campo com problemas */

    IF  INPUT FRAME f-pg-par fi-requisitante > " " THEN DO:

        FIND FIRST usuar-mater
             WHERE usuar-mater.cod-usuario = INPUT FRAME f-pg-par fi-requisitante
             NO-LOCK NO-ERROR.
        IF  NOT AVAIL usuar-mater THEN DO:

            /*usu†rio de materiais n∆o cadastrado*/
            {utp/ut-table.i mgind usuar-mater 1}
            RUN utp/ut-msgs.p (INPUT "show":U,
                               INPUT 56,
                               INPUT RETURN-VALUE).

            APPLY "MOUSE-SELECT-CLICK":U TO im-pg-par       IN FRAME f-relat.
            APPLY "ENTRY":U              TO fi-requisitante IN FRAME f-pg-par.
            RETURN "ADM-ERROR":U.

        END.
        ELSE IF NOT usuar-mater.usuar-solic THEN DO:

            /*solicitaá∆o n∆o pode ser efetuada*/
            RUN utp/ut-msgs.p (INPUT "show":U,
                               INPUT 1083,
                               INPUT "").
            APPLY "MOUSE-SELECT-CLICK":U TO im-pg-par       IN FRAME f-relat.
            APPLY "ENTRY":U              TO fi-requisitante IN FRAME f-pg-par.
            RETURN "ADM-ERROR":U.

        END.
    END.

    /* Aqui s∆o gravados os campos da temp-table que ser† passada como parÉmetro
       para o programa RP.P */

    ASSIGN c-icms = INPUT FRAME f-pg-par cb-icms.

    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame f-pg-imp rs-destino
           tt-param.data-exec       = today
           tt-param.hora-exec       = time
           tt-param.ge-ini          = input frame f-pg-sel i-ge-ini
           tt-param.ge-fim          = input frame f-pg-sel i-ge-fim 
           tt-param.familia-ini     = input frame f-pg-sel c-familia-ini
           tt-param.familia-fim     = input frame f-pg-sel c-familia-fim
           tt-param.item-ini        = input frame f-pg-sel c-item-ini
           tt-param.item-fim        = input frame f-pg-sel c-item-fim
           tt-param.estabelec-ini   = input frame f-pg-sel c-estabelec-ini
           tt-param.estabelec-fim   = input frame f-pg-sel c-estabelec-fim
           tt-param.data-ini        = input frame f-pg-sel da-data-ini
           tt-param.data-fim        = input frame f-pg-sel da-data-fim
           tt-param.pto-enc         = input frame f-pg-par l-pto-enc
           tt-param.periodico       = input frame f-pg-par l-periodico
           tt-param.l-multip        = input frame f-pg-par l-multip
           tt-param.l-split         = input frame f-pg-par l-split
           tt-param.i-icms          = {ininc/i01in082.i 06 c-icms}
           tt-param.c-requisitante  = INPUT FRAME f-pg-par fi-requisitante.

    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".

    /* Coloque aqui a l¢gica de gravaá∆o dos demais campos que devem ser passados
       como parÉmetros para o programa RP.P, atravÇs da temp-table tt-param */



    /* Executar do programa RP.P que ir† criar o relat¢rio */
    {include/i-rpexb.i}

    SESSION:SET-WAIT-STATE("general":U).

    if tg-considera:screen-value in frame f-pg-par = "yes" then do:
        run pi-rprun.
    end.
    else do:
        {include/i-rprun.i cep/ce0341rp.p}
    end.

    {include/i-rpexc.i}

    SESSION:SET-WAIT-STATE("":U).

    {include/i-rptrm.i}
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-rprun w-relat 
PROCEDURE pi-rprun :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {include/i-rprun.i cep/esce0341rp.p}

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

&IF '{&bf_mat_versao_ems}' >= '2.08' &THEN
    ASSIGN l-multip:VISIBLE IN FRAME f-pg-par = NO
           l-split:ROW IN FRAME f-pg-par      = 8.08.
&ENDIF
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


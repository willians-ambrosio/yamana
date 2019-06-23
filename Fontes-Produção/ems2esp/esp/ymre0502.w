&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
/*:T*******************************************************************************
** Descria‡Æo: Relat¢rio espec¡fico
** Autor.....: Sergio Luiz Neto da Silveira (DSC Praxis)
*******************************************************************************/

/*
{esinclude/i-buffer.i}
*/

{include/i-prgvrs.i YMRE0502 12.1.17.001}

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i YMRE0502 MRE}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/*:T Preprocessadores do Template de Relat½rio                            */
/*:T Obs: Retirar o valor do preprocessador para as pÿginas que n’o existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA 
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG f-pg-dig
&GLOBAL-DEFINE PGIMP f-pg-imp

&GLOBAL-DEFINE RTF   NO
  
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
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG

    field c-estabel-ini    like doc-fiscal.cod-estabel
    field c-estabel-fim    like doc-fiscal.cod-estabel
    field c-serie-ini      like doc-fiscal.serie
    field c-serie-fim      like doc-fiscal.serie
    field i-cod-emi-ini    like doc-fiscal.cod-emitente
    field i-cod-emi-fim    like doc-fiscal.cod-emitente
    field c-doc-ini        like doc-fiscal.nr-doc-fis
    field c-doc-fim        like doc-fiscal.nr-doc-fis
    field da-dt-ini        like doc-fiscal.dt-docto 
    field da-dt-fim        like doc-fiscal.dt-docto
    field da-emi-ini       like doc-fiscal.dt-emis-doc  
    field da-emi-fim       like doc-fiscal.dt-emis-doc
    field c-pais-ini       like doc-fiscal.pais
    field c-pais-fim       like doc-fiscal.pais
    field c-estado-ini     like doc-fiscal.estado
    field c-estado-fim     like doc-fiscal.estado  
    field c-especie-ini    like doc-fiscal.esp-docto
    field c-especie-fim    like doc-fiscal.esp-docto
    field c-natoper-ini    like doc-fiscal.nat-operacao
    field c-natoper-fim    like doc-fiscal.nat-operacao
    field c-cfop-ini       as char format "9.999xxxxx"
    field c-cfop-fim       as char format "9.999xxxxx"
    FIELD conteudo         AS INTEGER
    .

define temp-table tt-digita no-undo
    field esp-docto       as character format "x(03)"
    index id esp-docto.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-rtf              as char    no-undo.
def var c-modelo-default   as char    no-undo.

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
&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.esp-docto   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-digita.esp-docto   
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
&Scoped-Define ENABLED-OBJECTS br-digita bt-inserir bt-recuperar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-alterar 
     LABEL "Alterar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-inserir 
     LABEL "Inserir" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-recuperar 
     LABEL "Recuperar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-retirar 
     LABEL "Retirar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-salvar 
     LABEL "Salvar" 
     SIZE 15 BY 1.

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

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execu‡Æo" 
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
     SIZE 46.29 BY 2.79.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE rs-conteudo AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Todos Recebimentos", 1,
"Somente naturezas placebo(Tempor ria)", 2
     SIZE 32 BY 3 NO-UNDO.

DEFINE VARIABLE c-cfop-fim AS CHARACTER FORMAT "9.999xxxxx":U INITIAL "9999zzzzz" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-cfop-ini AS CHARACTER FORMAT "x.xxxxxxxx":U 
     LABEL "CFOP" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-doc-fim AS CHARACTER FORMAT "x(16)" INITIAL "zzzzzzzzzzzzzzzz" 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .88.

DEFINE VARIABLE c-doc-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Numero Docto":R20 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .88.

DEFINE VARIABLE c-especie-fim AS CHARACTER FORMAT "x(3)" INITIAL "zzz" 
     VIEW-AS FILL-IN 
     SIZE 8.14 BY .88.

DEFINE VARIABLE c-especie-ini AS CHARACTER FORMAT "x(3)" 
     LABEL "Esp‚cie Doc":R14 
     VIEW-AS FILL-IN 
     SIZE 8.14 BY .88.

DEFINE VARIABLE c-estabel-fim AS CHARACTER FORMAT "X(3)" INITIAL "zzz" 
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88.

DEFINE VARIABLE c-estabel-ini AS CHARACTER FORMAT "X(3)" 
     LABEL "Estabelecimento":R18 
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88.

DEFINE VARIABLE c-estado-fim AS CHARACTER FORMAT "x(04)" INITIAL "zzzz" 
     VIEW-AS FILL-IN 
     SIZE 8.14 BY .88.

DEFINE VARIABLE c-estado-ini AS CHARACTER FORMAT "x(04)" 
     LABEL "UF":R2 
     VIEW-AS FILL-IN 
     SIZE 8.14 BY .88.

DEFINE VARIABLE c-natoper-fim AS CHARACTER FORMAT "9.99-XXX" INITIAL "999zzz" 
     VIEW-AS FILL-IN 
     SIZE 10.29 BY .88.

DEFINE VARIABLE c-natoper-ini AS CHARACTER FORMAT "9.99-XXX" INITIAL "000" 
     LABEL "Nat Operacao":R21 
     VIEW-AS FILL-IN 
     SIZE 10.29 BY .88.

DEFINE VARIABLE c-pais-fim AS CHARACTER FORMAT "X(20)" INITIAL "zzzzzzzzzzzzzzzzzzzz" 
     VIEW-AS FILL-IN 
     SIZE 21.14 BY .88.

DEFINE VARIABLE c-pais-ini AS CHARACTER FORMAT "X(20)" 
     LABEL "Pais":R5 
     VIEW-AS FILL-IN 
     SIZE 21.14 BY .88.

DEFINE VARIABLE c-serie-fim AS CHARACTER FORMAT "x(5)" INITIAL "zzzzz" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88.

DEFINE VARIABLE c-serie-ini AS CHARACTER FORMAT "x(5)" 
     LABEL "S‚rie":R7 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88.

DEFINE VARIABLE da-dt-fim AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88.

DEFINE VARIABLE da-dt-ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/1900 
     LABEL "Dt Entr Docto":R15 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88.

DEFINE VARIABLE da-emi-fim AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88.

DEFINE VARIABLE da-emi-ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/1900 
     LABEL "Dt Emis Docto":R15 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88.

DEFINE VARIABLE i-cod-emi-fim AS INTEGER FORMAT ">>>>>>>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88.

DEFINE VARIABLE i-cod-emi-ini AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Cod Emitente":R17 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88.

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
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-imp
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-par
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-sel
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 79 BY 1.42
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 0    
     SIZE 78.72 BY .13
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 77.29 BY 13.5
     FGCOLOR 0 .

DEFINE RECTANGLE rt-folder-left
     EDGE-PIXELS 0    
     SIZE .43 BY 13
     BGCOLOR 15 .

DEFINE RECTANGLE rt-folder-right
     EDGE-PIXELS 0    
     SIZE .43 BY 13
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
      tt-digita.esp-docto COLUMN-LABEL "Esp‚cie"
ENABLE
tt-digita.esp-docto
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 76.57 BY 9
         BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
     bt-executar AT ROW 17 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 17 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 17 COL 70 HELP
          "Ajuda"
     RECT-1 AT ROW 16.75 COL 2
     RECT-6 AT ROW 16.46 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.75 COL 2.72
     im-pg-dig AT ROW 1.5 COL 33.57
     im-pg-imp AT ROW 1.5 COL 49.29
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-sel AT ROW 1.5 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 17.17
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-dig
     br-digita AT ROW 1 COL 1
     bt-inserir AT ROW 10.08 COL 1.14
     bt-alterar AT ROW 10.08 COL 16.14
     bt-retirar AT ROW 10.08 COL 31.14
     bt-salvar AT ROW 10.08 COL 46.14
     bt-recuperar AT ROW 10.08 COL 61.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.71
         SIZE 76.86 BY 11.42.

DEFINE FRAME f-pg-sel
     c-estabel-ini AT ROW 1.25 COL 19 COLON-ALIGNED HELP
          "C¢digo do estabelecimento" WIDGET-ID 132
     c-estabel-fim AT ROW 1.25 COL 49 COLON-ALIGNED HELP
          "C¢digo do estabelecimento" NO-LABEL WIDGET-ID 130
     c-serie-ini AT ROW 2.25 COL 19 COLON-ALIGNED HELP
          "S‚rie do documento fiscal" WIDGET-ID 76
     c-serie-fim AT ROW 2.25 COL 49 COLON-ALIGNED HELP
          "S‚rie do documento fiscal" NO-LABEL WIDGET-ID 74
     i-cod-emi-ini AT ROW 3.25 COL 19 COLON-ALIGNED HELP
          "C¢digo do emitente (cliente ou fornecedor)" WIDGET-ID 88
     i-cod-emi-fim AT ROW 3.25 COL 49 COLON-ALIGNED HELP
          "C¢digo do emitente (cliente ou fornecedor)" NO-LABEL WIDGET-ID 86
     c-doc-ini AT ROW 4.25 COL 19 COLON-ALIGNED HELP
          "N£mero do documento fiscal" WIDGET-ID 56
     c-doc-fim AT ROW 4.25 COL 49 COLON-ALIGNED HELP
          "N£mero do documento fiscal" NO-LABEL WIDGET-ID 54
     da-dt-ini AT ROW 5.25 COL 19 COLON-ALIGNED HELP
          "Data de emissÆo/entrada do documento" WIDGET-ID 80
     da-dt-fim AT ROW 5.25 COL 49 COLON-ALIGNED HELP
          "Data de emissÆo/entrada do documento" NO-LABEL WIDGET-ID 78
     da-emi-ini AT ROW 6.25 COL 19 COLON-ALIGNED HELP
          "Data de emissÆo/entrada do documento" WIDGET-ID 84
     da-emi-fim AT ROW 6.25 COL 49 COLON-ALIGNED HELP
          "Data de emissÆo/entrada do documento" NO-LABEL WIDGET-ID 82
     c-pais-ini AT ROW 7.25 COL 19 COLON-ALIGNED HELP
          "Pa¡s" WIDGET-ID 72
     c-pais-fim AT ROW 7.25 COL 49 COLON-ALIGNED HELP
          "Pa¡s" NO-LABEL WIDGET-ID 70
     c-estado-ini AT ROW 8.25 COL 19 COLON-ALIGNED HELP
          "Unidade da federa‡Æo" WIDGET-ID 64
     c-estado-fim AT ROW 8.25 COL 49 COLON-ALIGNED HELP
          "Unidade da federa‡Æo" NO-LABEL WIDGET-ID 62
     c-especie-ini AT ROW 9.25 COL 19 COLON-ALIGNED HELP
          "Esp‚cie do documento" WIDGET-ID 60
     c-especie-fim AT ROW 9.25 COL 49 COLON-ALIGNED HELP
          "Esp‚cie do documento" NO-LABEL WIDGET-ID 58
     c-natoper-ini AT ROW 10.25 COL 19 COLON-ALIGNED HELP
          "Natureza de opera‡Æo" WIDGET-ID 68
     c-natoper-fim AT ROW 10.25 COL 49 COLON-ALIGNED HELP
          "Natureza de opera‡Æo" NO-LABEL WIDGET-ID 66
     c-cfop-ini AT ROW 11.25 COL 19 COLON-ALIGNED WIDGET-ID 52
     c-cfop-fim AT ROW 11.25 COL 49 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     IMAGE-9 AT ROW 5.25 COL 43 WIDGET-ID 128
     IMAGE-7 AT ROW 4.25 COL 43 WIDGET-ID 124
     IMAGE-8 AT ROW 4.25 COL 48.43 WIDGET-ID 126
     IMAGE-6 AT ROW 3.25 COL 48.43 WIDGET-ID 122
     IMAGE-5 AT ROW 3.25 COL 43 WIDGET-ID 120
     IMAGE-4 AT ROW 2.25 COL 48.43 WIDGET-ID 118
     IMAGE-3 AT ROW 2.25 COL 43 WIDGET-ID 116
     IMAGE-14 AT ROW 7.25 COL 48.43 WIDGET-ID 98
     IMAGE-13 AT ROW 7.25 COL 43 WIDGET-ID 96
     IMAGE-16 AT ROW 8.25 COL 48.43 WIDGET-ID 102
     IMAGE-10 AT ROW 5.25 COL 48.43 WIDGET-ID 90
     IMAGE-11 AT ROW 6.25 COL 43 WIDGET-ID 92
     IMAGE-12 AT ROW 6.25 COL 48.43 WIDGET-ID 94
     IMAGE-17 AT ROW 9.25 COL 43 WIDGET-ID 104
     IMAGE-22 AT ROW 10.29 COL 48.43 WIDGET-ID 114
     IMAGE-21 AT ROW 10.29 COL 43 WIDGET-ID 112
     IMAGE-18 AT ROW 9.25 COL 48.43 WIDGET-ID 106
     IMAGE-19 AT ROW 11.25 COL 43 WIDGET-ID 108
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.86 ROW 2.79
         SIZE 77 BY 11.63
         FONT 1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME f-pg-sel
     IMAGE-20 AT ROW 11.25 COL 48.43 WIDGET-ID 110
     IMAGE-15 AT ROW 8.25 COL 43 WIDGET-ID 100
     IMAGE-1 AT ROW 1.25 COL 43 WIDGET-ID 134
     IMAGE-2 AT ROW 1.25 COL 48.43 WIDGET-ID 136
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.86 ROW 2.79
         SIZE 77 BY 11.63
         FONT 1.

DEFINE FRAME f-pg-par
     rs-conteudo AT ROW 2.5 COL 7 NO-LABEL WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.79
         SIZE 75 BY 12.17
         FONT 1.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 1.63 COL 3.29 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     bt-arquivo AT ROW 2.71 COL 43.29 HELP
          "Escolha do nome do arquivo"
     bt-config-impr AT ROW 2.71 COL 43.29 HELP
          "Configura‡Æo da impressora"
     c-arquivo AT ROW 2.75 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 5.25 COL 2.86 HELP
          "Modo de Execu‡Æo" NO-LABEL
     text-destino AT ROW 1.04 COL 3.86 NO-LABEL
     text-modo AT ROW 4.5 COL 3.14 NO-LABEL
     RECT-7 AT ROW 1.33 COL 2.14
     RECT-9 AT ROW 4.71 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS THREE-D 
         AT COL 3.29 ROW 2.79
         SIZE 75.57 BY 10.67.


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
         HEIGHT             = 17.17
         WIDTH              = 80
         MAX-HEIGHT         = 22.33
         MAX-WIDTH          = 120.57
         VIRTUAL-HEIGHT     = 22.33
         VIRTUAL-WIDTH      = 120.57
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
/* SETTINGS FOR BUTTON bt-alterar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-retirar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-salvar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-imp
   UNDERLINE                                                            */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execu‡Æo".

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR IMAGE IMAGE-1 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-10 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-11 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-12 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-13 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-14 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-15 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-16 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-17 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-18 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-19 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-2 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-20 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-21 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-22 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-3 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-4 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-5 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-6 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-7 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-8 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-9 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
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
   apply 'choose':U to bt-retirar in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
        display tt-digita.esp-docto with browse br-digita. 
    end.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ENTER OF br-digita IN FRAME f-pg-dig
ANYWHERE
DO:
  apply 'tab':U to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON INS OF br-digita IN FRAME f-pg-dig
DO:
   apply 'choose':U to bt-inserir in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON OFF-END OF br-digita IN FRAME f-pg-dig
DO:
   apply 'entry':U to bt-inserir in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON OFF-HOME OF br-digita IN FRAME f-pg-dig
DO:
  apply 'entry':U to bt-recuperar in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ROW-ENTRY OF br-digita IN FRAME f-pg-dig
DO:
   /*:T trigger para inicializar campos da temp table de digita»’o */
/*    if  br-digita:new-row in frame f-pg-dig then do:                                             */
/*        assign tt-digita.exemplo:screen-value in browse br-digita = string(today, "99/99/9999"). */
/*    end.                                                                                         */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ROW-LEAVE OF br-digita IN FRAME f-pg-dig
DO:
    /*:T È aqui que a grava»’o da linha da temp-table ² efetivada.
       Por²m as valida»„es dos registros devem ser feitas na procedure pi-executar,
       no local indicado pelo comentÿrio */
    
    if br-digita:NEW-ROW in frame f-pg-dig then 
    do transaction on error undo, return no-apply:
        create tt-digita.
        assign input browse br-digita tt-digita.esp-docto.
    
        br-digita:CREATE-RESULT-LIST-ENTRY() in frame f-pg-dig.
    end.
    else do transaction on error undo, return no-apply:
        assign input browse br-digita tt-digita.esp-docto.
    end.
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
ON CHOOSE OF bt-alterar IN FRAME f-pg-dig /* Alterar */
DO:
   apply 'entry':U to tt-digita.esp-docto in browse br-digita. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo w-relat
ON CHOOSE OF bt-arquivo IN FRAME f-pg-imp
DO:
/*     {include/i-rparq.i} */

/*****************************************************************
**
** I-RPARQ - Choose of bt-Arquivo no template de relat«rio
**
*****************************************************************/

    def var c-arq-conv  as char no-undo.

    /* tech1139 - FO 1223.694  - 02/11/2005 */
    assign c-arq-conv = replace(input frame f-pg-imp c-arquivo, "/", CHR(92)).
    /* tech1139 - FO 1223.694  - 02/11/2005 */

    
/*tech14178 modificado para apresentar dialog com extensÊo PDF quando o mesmo estiver sendo usado */
&IF "{&PDF}" = "YES" &THEN /*tech868*/
    
    IF NOT usePDF() THEN

&ENDIF
    
        SYSTEM-DIALOG GET-FILE c-arq-conv
           FILTERS "*.csv" "*.csv",
                   "*.*" "*.*"
           ASK-OVERWRITE 
           DEFAULT-EXTENSION "csv"
           INITIAL-DIR "spool" 
           SAVE-AS
           USE-FILENAME
           UPDATE l-ok.

&IF "{&PDF}" = "YES" &THEN /*tech868*/
   ELSE
       SYSTEM-DIALOG GET-FILE c-arq-conv
          FILTERS "*.pdf" "*.pdf",
                  "*.*" "*.*"
          ASK-OVERWRITE 
          DEFAULT-EXTENSION "pdf"
          INITIAL-DIR "spool" 
          SAVE-AS
          USE-FILENAME
          UPDATE l-ok.

&endif


    if  l-ok = yes then do:
        /* tech1139 - FO 1223.694  - 02/11/2005 */
        assign c-arquivo = replace(c-arq-conv, CHR(92), "/"). 
        /* tech1139 - FO 1223.694  - 02/11/2005 */
        display c-arquivo with frame f-pg-imp.
    end.

/* i-rparq */

    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-relat
ON CHOOSE OF bt-cancelar IN FRAME f-relat /* Fechar */
DO:
   apply "close":U to this-procedure.
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
ON CHOOSE OF bt-inserir IN FRAME f-pg-dig /* Inserir */
DO:
    assign bt-alterar:SENSITIVE in frame f-pg-dig = yes
           bt-retirar:SENSITIVE in frame f-pg-dig = yes
           bt-salvar:SENSITIVE in frame f-pg-dig  = yes.
    
    if num-results("br-digita":U) > 0 then
        br-digita:INSERT-ROW("after":U) in frame f-pg-dig.
    else do transaction:
        create tt-digita.
        
        open query br-digita for each tt-digita.
        
        apply "entry":U to tt-digita.esp-docto in browse br-digita. 
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-recuperar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-recuperar w-relat
ON CHOOSE OF bt-recuperar IN FRAME f-pg-dig /* Recuperar */
DO:
    {include/i-rprcd.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-retirar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-retirar w-relat
ON CHOOSE OF bt-retirar IN FRAME f-pg-dig /* Retirar */
DO:
    if  br-digita:num-selected-rows > 0 then do on error undo, return no-apply:
        get current br-digita.
        delete tt-digita.
        if  br-digita:delete-current-row() in frame f-pg-dig then.
    end.
    
    if num-results("br-digita":U) = 0 then
        assign bt-alterar:SENSITIVE in frame f-pg-dig = no
               bt-retirar:SENSITIVE in frame f-pg-dig = no
               bt-salvar:SENSITIVE in frame f-pg-dig  = no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-salvar w-relat
ON CHOOSE OF bt-salvar IN FRAME f-pg-dig /* Salvar */
DO:
   {include/i-rpsvd.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME c-cfop-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cfop-fim w-relat
ON LEAVE OF c-cfop-fim IN FRAME f-pg-sel
DO:
  {cdp/cd9998.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-cfop-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-cfop-ini w-relat
ON LEAVE OF c-cfop-ini IN FRAME f-pg-sel /* CFOP */
DO:
  {cdp/cd9998.i}
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
  ASSIGN c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = "c:\temp\teste.txt".
/*Alterado 15/02/2005 - tech1007 - Evento alterado para correto funcionamento dos novos widgets
  utilizados para a funcionalidade de RTF*/
do  with frame f-pg-imp:
    case self:screen-value:
        when "1" then do:
            assign c-arquivo:sensitive    = no
                   bt-arquivo:visible     = no
                   bt-config-impr:visible = YES
                   /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para
                     verificar se o RTF estÿ ativo*/
/*                    &IF "{&RTF}":U = "YES":U &THEN                      */
/*                    l-habilitaRtf:sensitive  = NO                       */
/*                    l-habilitaRtf:SCREEN-VALUE IN FRAME f-pg-imp = "No" */
/*                    l-habilitaRtf = NO                                  */
/*                    &endif                                              */
                   /*Fim alteracao 17/02/2005*/
                   .
        end.
        when "2" then do:
            assign c-arquivo:sensitive     = yes
                   bt-arquivo:visible      = yes
                   bt-config-impr:visible  = NO
                   /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para
                     verificar se o RTF estÿ ativo*/
/*                    &IF "{&RTF}":U = "YES":U &THEN */
/*                    l-habilitaRtf:sensitive  = YES */
/*                    &endif                         */
                   /*Fim alteracao 17/02/2005*/
                   .
        end.
        when "3" then do:
            assign c-arquivo:sensitive     = no
                   bt-arquivo:visible      = no
                   bt-config-impr:visible  = no
                   /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para
                     verificar se o RTF estÿ ativo*/
/*                    &IF "{&RTF}":U = "YES":U &THEN */
/*                    l-habilitaRtf:sensitive  = YES */
/*                    &endif                         */
                   /*Fim alteracao 17/02/2005*/
                   .
            /*Alterado 15/02/2005 - tech1007 - Teste para funcionar corretamente no WebEnabler*/
/*             &IF "{&RTF}":U = "YES":U &THEN                                 */
/*             IF VALID-HANDLE(hWenController) THEN DO:                       */
/*                 ASSIGN l-habilitaRtf:sensitive  = NO                       */
/*                        l-habilitaRtf:SCREEN-VALUE IN FRAME f-pg-imp = "No" */
/*                        l-habilitaRtf = NO.                                 */
/*             END.                                                           */
/*             &endif                                                         */
            /*Fim alteracao 15/02/2005*/
        end.
    end case.
end.
/* &IF "{&RTF}":U = "YES":U &THEN  */
/* RUN pi-habilitaRtf.             */
/* &endif                          */
/*Fim alteracao 15/02/2005*/
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

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "YMRE0502" "12.1.17.001"}

/*:T inicializa»„es do template de relat½rio */
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
  
    ASSIGN da-dt-ini:SCREEN-VALUE IN FRAME f-pg-sel = STRING(TODAY)
           da-dt-fim:SCREEN-VALUE IN FRAME f-pg-sel = STRING(TODAY).
           
    ASSIGN rs-destino:screen-value in frame f-pg-imp = "2".   
    
    apply "value-changed" to rs-destino in frame f-pg-imp.
    
    {include/i-rpmbl.i}
    
    assign c-arquivo:screen-value in frame f-pg-imp = session:TEMP-DIRECTORY + "ymre0502_" + REPLACE(STRING(TODAY,'99/99/9999'),'/','') + REPLACE(STRING(TIME,'hh:mm:ss'),':','') + ".csv".    
    
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
  ENABLE im-pg-dig im-pg-imp im-pg-par im-pg-sel bt-executar bt-cancelar 
         bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  ENABLE br-digita bt-inserir bt-recuperar 
      WITH FRAME f-pg-dig IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-dig}
  DISPLAY c-estabel-ini c-estabel-fim c-serie-ini c-serie-fim i-cod-emi-ini 
          i-cod-emi-fim c-doc-ini c-doc-fim da-dt-ini da-dt-fim da-emi-ini 
          da-emi-fim c-pais-ini c-pais-fim c-estado-ini c-estado-fim 
          c-especie-ini c-especie-fim c-natoper-ini c-natoper-fim c-cfop-ini 
          c-cfop-fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE c-estabel-ini c-estabel-fim c-serie-ini c-serie-fim i-cod-emi-ini 
         i-cod-emi-fim c-doc-ini c-doc-fim da-dt-ini da-dt-fim da-emi-ini 
         da-emi-fim c-pais-ini c-pais-fim c-estado-ini c-estado-fim 
         c-especie-ini c-especie-fim c-natoper-ini c-natoper-fim c-cfop-ini 
         c-cfop-fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-conteudo 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE rs-conteudo 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 rs-destino bt-arquivo bt-config-impr c-arquivo 
         rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
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
    /*14/02/2005 - tech1007 - Alterada condicao para n’o considerar mai o RTF como destino*/
    if input frame f-pg-imp rs-destino = 2 and
       input frame f-pg-imp rs-execucao = 1 then do:
        run utp/ut-vlarq.p (input input frame f-pg-imp c-arquivo).
        
        if return-value = "NOK":U then do:
            run utp/ut-msgs.p (input "show":U, input 73, input "").
            
            apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
            apply "ENTRY":U to c-arquivo in frame f-pg-imp.
            return error.
        end.
    end.

    for each tt-digita no-lock:
        assign r-tt-digita = rowid(tt-digita).
        
        /*:T Valida»’o de duplicidade de registro na temp-table tt-digita */
        find first b-tt-digita where b-tt-digita.esp-docto = tt-digita.esp-docto and 
                                     rowid(b-tt-digita) <> rowid(tt-digita) no-lock no-error.
        if avail b-tt-digita then do:
            apply "MOUSE-SELECT-CLICK":U to im-pg-dig in frame f-relat.
            reposition br-digita to rowid rowid(b-tt-digita).
            
            run utp/ut-msgs.p (input "show":U, input 108, input "").
            apply "ENTRY":U to tt-digita.esp-docto in browse br-digita.
            
            return error.
        end.
        
        /*:T As demais valida»„es devem ser feitas aqui */
        if tt-digita.esp-docto = "" then do:
            assign browse br-digita:CURRENT-COLUMN = tt-digita.esp-docto:HANDLE in browse br-digita.
            
            apply "MOUSE-SELECT-CLICK":U to im-pg-dig in frame f-relat.
            reposition br-digita to rowid r-tt-digita.
            
            run utp/ut-msgs.p (input "show":U, input 99999, input "").
            apply "ENTRY":U to tt-digita.esp-docto in browse br-digita.
            
            return error.
        end.
        
    end.
    
    
    /*:T Coloque aqui as valida»„es das outras pÿginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na pÿgina com 
       problemas e colocar o focus no campo com problemas */
    
    
    
    /*:T Aqui s’o gravados os campos da temp-table que serÿ passada como par³metro
       para o programa RP.P */
    
    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame f-pg-imp rs-destino
           tt-param.data-exec       = today
           tt-param.hora-exec       = time.

    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.

    /*:T Coloque aqui a/l½gica de grava»’o dos demais campos que devem ser passados
       como par³metros para o programa RP.P, atrav²s da temp-table tt-param */
    
    ASSIGN tt-param.c-estabel-ini = input frame f-pg-sel c-estabel-ini
           tt-param.c-estabel-fim = input frame f-pg-sel c-estabel-fim
           tt-param.c-serie-ini   = input frame f-pg-sel c-serie-ini  
           tt-param.c-serie-fim   = input frame f-pg-sel c-serie-fim  
           tt-param.i-cod-emi-ini = input frame f-pg-sel i-cod-emi-ini
           tt-param.i-cod-emi-fim = input frame f-pg-sel i-cod-emi-fim
           tt-param.c-doc-ini     = input frame f-pg-sel c-doc-ini    
           tt-param.c-doc-fim     = input frame f-pg-sel c-doc-fim    
           tt-param.da-dt-ini     = input frame f-pg-sel da-dt-ini    
           tt-param.da-dt-fim     = input frame f-pg-sel da-dt-fim    
           tt-param.da-emi-ini    = input frame f-pg-sel da-emi-ini   
           tt-param.da-emi-fim    = input frame f-pg-sel da-emi-fim   
           tt-param.c-pais-ini    = INPUT FRAME f-pg-sel c-pais-ini   
           tt-param.c-pais-fim    = INPUT FRAME f-pg-sel c-pais-fim
           tt-param.c-estado-ini  = INPUT FRAME f-pg-sel c-estado-ini 
           tt-param.c-estado-fim  = INPUT FRAME f-pg-sel c-estado-fim 
           tt-param.c-especie-ini = INPUT FRAME f-pg-sel c-especie-ini
           tt-param.c-especie-fim = INPUT FRAME f-pg-sel c-especie-fim
           tt-param.c-natoper-ini = INPUT FRAME f-pg-sel c-natoper-ini
           tt-param.c-natoper-fim = INPUT FRAME f-pg-sel c-natoper-fim
           tt-param.c-cfop-ini    = INPUT FRAME f-pg-sel c-cfop-ini   
           tt-param.c-cfop-fim    = INPUT FRAME f-pg-sel c-cfop-fim   
           tt-param.conteudo      = INPUT FRAME f-pg-par rs-conteudo
           .
    
    /*:T Executar do programa RP.P que irÿ criar o relat½rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i esp\ymre0502rp.p}
    
    {include/i-rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).

    IF RETURN-VALUE <> "OK" THEN
        os-command silent value(RETURN-VALUE) NO-WAIT NO-ERROR.

    /*ELSE
        {include/i-rptrm.i}*/
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina w-relat 
PROCEDURE pi-troca-pagina :
/*:T------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de Pÿgina (folder)   
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


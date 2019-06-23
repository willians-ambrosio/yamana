&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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
{include/i-prgvrs.i ESAP0749 2.06.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{cdp/cdcfgfin.i}

/* Preprocessadores do Template de Relat½rio                            */
/* Obs: Retirar o valor do preprocessador para as pÿginas que n’o existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA f-pg-cla
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG 
&GLOBAL-DEFINE PGIMP f-pg-imp

/* Parameters Definitions ---                                           */
def new global shared var h-rsocial             as Handle                           no-undo.
def new global shared var l-achou-prog          as Logical                          no-undo.
def                   var rw-log-exec           as rowid                                no-undo.
def new Global shared var c-seg-usuario         as Character        format "x(12)"  no-undo.
DEFINE                var i-template            as integer no-undo.
def new global shared var i-ep-codigo-usuario   like ems2cadme.empresa.ep-codigo          no-undo.
def new global shared var v_cdn_empres_usuar    like ems2cadme.empresa.ep-codigo          no-undo.
def new global shared var v_cod_usuar_corren    like usuar_mestre.cod_usuario   no-undo.
def new global shared var v_nom_razao_social    like ems2cadme.empresa.razao-social       no-undo.


/* Temporary Table Definitions ---                                      */

define temp-table tt-param no-undo
    field destino               as integer                                  
    field arquivo               as char format "x(35)"
    field usuario               as char format "x(12)"
    field data-exec             as date
    field hora-exec             as integer
    field classifica            as integer
    field desc-classifica       as char format "x(40)"
    field cod-estabel-ini       as CHARACTER FORMAT "X(3)"                
    field cod-estabel-fim       as CHARACTER FORMAT "X(3)"                
    field cod-esp-ini           as CHARACTER FORMAT "!!"                  
    field cod-esp-fim           as CHARACTER FORMAT "!!"                  
    field cod-emit-ini          as INTEGER   FORMAT ">>>>>>>>9"           
    field cod-emit-fim          as INTEGER   FORMAT ">>>>>>>>9"           
    field dt-trans-ini          as DATE      FORMAT "99/99/9999"          
    field dt-trans-fim          as DATE      FORMAT "99/99/9999"          
    field dt-venci-ini          as DATE      FORMAT "99/99/9999"          
    field dt-venci-fim          as DATE      FORMAT "99/99/9999"          
    field l-tit-subs            as log                                    
    field l-nota-db-cr          as log                                    
    field c-origem              as char                                   
    field ep-codigo             as char 
    field imp-par               as logical                                
    FIELD referencia-ini        LIKE mov-ap.referencia                    
    FIELD referencia-fim        LIKE mov-ap.referencia
    FIELD dt-maq-ini            AS DATE FORMAT "99/99/9999"
    FIELD dt-maq-fim            AS DATE FORMAT "99/99/9999"
    FIELD tg-excel              AS LOG
    FIELD c-arquivo-csv         AS CHAR FORMAT "x(50)".                   


/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita no-undo
   field raw-digita      as raw.

DEF VAR cArquivo AS char.
/* Local Variable Definitions ---                                       */

def var l-ok               as log     no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-origem-tmp       as char    no-undo.
def var i-cont             as int     no-undo.
DEF VAR l-selec-ref        AS logical NO-UNDO.

DEF VAR cArqConv                    AS CHAR NO-UNDO.



/*** Verificar se a funîÒo especial estÙë habilitada ***/
{include/getdefinedfunction.i}
assign l-selec-ref = &if defined(BF_FIN_SELEC_REF) &then YES &else GetDefinedFunction("SPP-SELEC-REF") &endif.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-cla

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-10 rs-classif 
&Scoped-Define DISPLAYED-OBJECTS rs-classif 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE text-cla AS CHARACTER FORMAT "X(256)":U INITIAL "Classifica‡Æo" 
      VIEW-AS TEXT 
     SIZE 20.86 BY .63 NO-UNDO.

DEFINE VARIABLE rs-classif AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Por Dt Transa‡Æo/Fornec", 1,
"Por Fornec/Dt Transa‡Æo", 2
     SIZE 32.29 BY 2.29 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37.29 BY 2.92.

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

DEFINE VARIABLE text-imp-par AS CHARACTER FORMAT "X(256)":U INITIAL "Parƒmetros de ImpressÆo" 
      VIEW-AS TEXT 
     SIZE 26.29 BY .63 NO-UNDO.

DEFINE VARIABLE text-imp-par-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Par³metros de Impress’o" 
      VIEW-AS TEXT 
     SIZE 26.29 BY .63 NO-UNDO.

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

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.43 BY 1.42.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE imp-par AS LOGICAL INITIAL yes 
     LABEL "Imprime P gina de Parƒmetros" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .83 NO-UNDO.

DEFINE BUTTON bt-arquivo-csv 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-origem AS CHARACTER FORMAT "X(256)":U INITIAL "Todos" 
     LABEL "Origem" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "APB","ACR","EEC","EXP","PED","REC","HR","Todos" 
     DROP-DOWN-LIST
     SIZE 22.57 BY 1 NO-UNDO.

DEFINE VARIABLE c-arquivo-csv AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 37.86 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 73 BY 2.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 73 BY 1.38.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 73.43 BY 2.

DEFINE VARIABLE l-nota-db-cr AS LOGICAL INITIAL no 
     LABEL "Considerar Notas Debito/Credito" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .83 NO-UNDO.

DEFINE VARIABLE l-tit-subs AS LOGICAL INITIAL no 
     LABEL "Considerar Titulos Substituidos" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .83 NO-UNDO.

DEFINE VARIABLE tg-excel AS LOGICAL INITIAL no 
     LABEL "Imprime Arquivo Excel (.CSV)" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .83 NO-UNDO.

DEFINE VARIABLE c-cod-esp-fim AS CHARACTER FORMAT "!!":U INITIAL "ZZ" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-esp-ini AS CHARACTER FORMAT "!!":U 
     LABEL "Esp‚cie" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-estabel-fim AS CHARACTER FORMAT "X(3)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-estabel-ini AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelec" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE c-refer-fim AS CHARACTER FORMAT "X(10)":U INITIAL "ZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE c-refer-ini AS CHARACTER FORMAT "X(10)":U 
     LABEL "Referˆncia" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE dt-maq-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE dt-maq-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Dt Maq" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE dt-trans-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE dt-trans-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Dt Transa‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE dt-venci-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE dt-venci-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Dt Vencimento" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE i-cod-emit-fim AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE i-cod-emit-ini AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Fornec" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-las":U
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

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
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

DEFINE IMAGE im-pg-cla
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


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     RECT-1 AT ROW 14.29 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder AT ROW 2.5 COL 2
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-left AT ROW 2.54 COL 2.14
     im-pg-cla AT ROW 1.5 COL 17.86
     im-pg-imp AT ROW 1.5 COL 49.29
     im-pg-par AT ROW 1.5 COL 33.57
     im-pg-sel AT ROW 1.5 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81.14 BY 15.08
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-cla
     rs-classif AT ROW 2.33 COL 2.14 HELP
          "Classifica‡Æo para emissÆo do relat¢rio" NO-LABEL
     text-cla AT ROW 1.63 COL 3.57 NO-LABEL
     RECT-10 AT ROW 1.88 COL 1.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.86 BY 10.31.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.38 COL 3.29 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     bt-config-impr AT ROW 3.58 COL 43.29 HELP
          "Configura»’o da impressora"
     bt-arquivo AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     c-arquivo AT ROW 3.63 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 5.75 COL 3 HELP
          "Modo de Execu‡Æo" NO-LABEL
     imp-par AT ROW 8.17 COL 3.43
     text-destino AT ROW 1.63 COL 3.86 NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     text-imp-par-2 AT ROW 7.42 COL 1.57 COLON-ALIGNED NO-LABEL
     text-imp-par AT ROW 7.42 COL 1.57 COLON-ALIGNED NO-LABEL
     RECT-12 AT ROW 7.75 COL 2.14
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10.

DEFINE FRAME f-pg-sel
     c-cod-estabel-ini AT ROW 2.17 COL 21.29 COLON-ALIGNED
     c-cod-estabel-fim AT ROW 2.17 COL 45 COLON-ALIGNED NO-LABEL
     c-cod-esp-ini AT ROW 3.17 COL 21.29 COLON-ALIGNED
     c-cod-esp-fim AT ROW 3.17 COL 45 COLON-ALIGNED NO-LABEL
     i-cod-emit-ini AT ROW 4.17 COL 21.29 COLON-ALIGNED
     i-cod-emit-fim AT ROW 4.17 COL 45 COLON-ALIGNED NO-LABEL
     dt-trans-ini AT ROW 5.17 COL 21.29 COLON-ALIGNED
     dt-trans-fim AT ROW 5.17 COL 45 COLON-ALIGNED NO-LABEL
     dt-venci-ini AT ROW 6.17 COL 21.29 COLON-ALIGNED
     dt-venci-fim AT ROW 6.17 COL 45 COLON-ALIGNED NO-LABEL
     c-refer-ini AT ROW 7.17 COL 21.29 COLON-ALIGNED
     c-refer-fim AT ROW 7.17 COL 45 COLON-ALIGNED NO-LABEL
     dt-maq-ini AT ROW 8.08 COL 21.29 COLON-ALIGNED WIDGET-ID 12
     dt-maq-fim AT ROW 8.08 COL 45 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     IMAGE-1 AT ROW 2.17 COL 35
     IMAGE-10 AT ROW 3.21 COL 43.43
     IMAGE-11 AT ROW 4.21 COL 43.43
     IMAGE-12 AT ROW 5.21 COL 43.43
     IMAGE-13 AT ROW 6.17 COL 35
     IMAGE-14 AT ROW 6.17 COL 43.43
     IMAGE-15 AT ROW 7.17 COL 35
     IMAGE-16 AT ROW 7.17 COL 43.43
     IMAGE-2 AT ROW 2.17 COL 43.43
     IMAGE-5 AT ROW 3.17 COL 35
     IMAGE-6 AT ROW 4.17 COL 35
     IMAGE-7 AT ROW 5.17 COL 35
     IMAGE-17 AT ROW 7.17 COL 35 WIDGET-ID 6
     IMAGE-18 AT ROW 7.17 COL 43.43 WIDGET-ID 8
     IMAGE-19 AT ROW 8.08 COL 35 WIDGET-ID 14
     IMAGE-20 AT ROW 8.08 COL 43.43 WIDGET-ID 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62.

DEFINE FRAME f-pg-par
     l-tit-subs AT ROW 1.38 COL 3.29
     l-nota-db-cr AT ROW 2.21 COL 3.43
     c-origem AT ROW 3.67 COL 4.57
     tg-excel AT ROW 6 COL 4 WIDGET-ID 8
     bt-arquivo-csv AT ROW 6 COL 70.86 HELP
          "Escolha do nome do arquivo" WIDGET-ID 2
     c-arquivo-csv AT ROW 6.04 COL 32.57 HELP
          "Nome do arquivo CSV" NO-LABEL WIDGET-ID 4
     "Destino Excel" VIEW-AS TEXT
          SIZE 12.86 BY .67 AT ROW 5.13 COL 3.14 WIDGET-ID 10
     RECT-11 AT ROW 1.25 COL 2.29
     RECT-13 AT ROW 3.46 COL 2.29
     RECT-14 AT ROW 5.5 COL 2 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 75 BY 10.


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
         TITLE              = "T¡tulos Impl. Contas a Pagar"
         HEIGHT             = 15.38
         WIDTH              = 81.57
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
/* {utp/ut-glob.i}  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-relat
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-pg-cla
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN text-cla IN FRAME f-pg-cla
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-cla:PRIVATE-DATA IN FRAME f-pg-cla     = 
                "Classifica‡Æo".

/* SETTINGS FOR FRAME f-pg-imp
   Custom                                                               */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-imp-par IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-imp-par:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Parƒmetros de ImpressÆo".

/* SETTINGS FOR FILL-IN text-imp-par-2 IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-imp-par-2:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Par³metros de Impress’o".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execu‡Æo".

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR BUTTON bt-arquivo-csv IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR c-arquivo-csv IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX c-origem IN FRAME f-pg-par
   ALIGN-L                                                              */
/* SETTINGS FOR TOGGLE-BOX l-nota-db-cr IN FRAME f-pg-par
   NO-DISPLAY                                                           */
/* SETTINGS FOR TOGGLE-BOX l-tit-subs IN FRAME f-pg-par
   NO-DISPLAY                                                           */
/* SETTINGS FOR RECTANGLE RECT-11 IN FRAME f-pg-par
   NO-ENABLE                                                            */
ASSIGN 
       RECT-11:HIDDEN IN FRAME f-pg-par           = TRUE.

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
ON END-ERROR OF w-relat /* T¡tulos Impl. Contas a Pagar */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* T¡tulos Impl. Contas a Pagar */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME bt-arquivo-csv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo-csv w-relat
ON CHOOSE OF bt-arquivo-csv IN FRAME f-pg-par
DO:
    RUN pi-SalvarFile (INPUT-OUTPUT cArquivo, 
                  INPUT SESSION:TEMP-DIRECTORY).
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


&Scoped-define SELF-NAME im-pg-cla
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-cla w-relat
ON MOUSE-SELECT-CLICK OF im-pg-cla IN FRAME f-relat
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


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME tg-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-excel w-relat
ON VALUE-CHANGED OF tg-excel IN FRAME f-pg-par /* Imprime Arquivo Excel (.CSV) */
DO:

    
  
  IF tg-excel:screen-value in frame f-pg-par = "Yes" THEN DO:
  
      ENABLE c-arquivo-csv bt-arquivo-csv WITH FRAME F-PG-PAR.
      c-arquivo-csv:screen-value in frame f-pg-par = "C:\temp\esap0749.csv".

  END.
  ELSE   DO:
     DISABLE c-arquivo-csv bt-arquivo-csv WITH FRAME F-PG-PAR.
      c-arquivo-csv:screen-value in frame f-pg-par = "".

  END.


      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-cla
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

assign c-origem = "Contas a Pagar".

{utp/ut9000.i "ESAP0749" "2.06.00.003"}

/* inicializa»„es do template de relat½rio */
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

    /*assign c-origem-tmp = "".
    do i-cont = 1 to num-entries(c-origem:list-items in frame f-pg-par):
      run utp/ut-liter.p (input entry(i-cont,c-origem:list-items in frame f-pg-par), input "*", input "R").
      assign c-origem-tmp = c-origem-tmp + return-value + ",".
    end.
    assign c-origem-tmp = substring(c-origem-tmp,1,length(c-origem-tmp) - 1)
           c-origem:list-items in frame f-pg-par = c-origem-tmp.
    assign c-origem:screen-value in frame f-pg-par = entry(1,c-origem:list-items in frame f-pg-par).*/


    RUN enable_UI.

    ASSIGN c-origem:SCREEN-VALUE IN FRAME f-pg-par = 'Todos'.

    {include/i-rpmbl.i}

    IF  NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

ASSIGN
    dt-trans-ini:SCREEN-VALUE IN FRAME f-pg-sel = STRING(DATE(TODAY))
    dt-venci-ini:SCREEN-VALUE IN FRAME f-pg-sel = STRING(DATE(TODAY))
    dt-trans-fim:SCREEN-VALUE IN FRAME f-pg-sel = STRING(DATE(TODAY))
    dt-venci-fim:SCREEN-VALUE IN FRAME f-pg-sel = STRING(DATE(TODAY))
    .

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
  ENABLE im-pg-cla im-pg-imp im-pg-par im-pg-sel bt-executar bt-cancelar 
         bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY c-cod-estabel-ini c-cod-estabel-fim c-cod-esp-ini c-cod-esp-fim 
          i-cod-emit-ini i-cod-emit-fim dt-trans-ini dt-trans-fim dt-venci-ini 
          dt-venci-fim c-refer-ini c-refer-fim dt-maq-ini dt-maq-fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-1 IMAGE-10 IMAGE-11 IMAGE-12 IMAGE-13 IMAGE-14 IMAGE-15 IMAGE-16 
         IMAGE-2 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-17 IMAGE-18 IMAGE-19 IMAGE-20 
         c-cod-estabel-ini c-cod-estabel-fim c-cod-esp-ini c-cod-esp-fim 
         i-cod-emit-ini i-cod-emit-fim dt-trans-ini dt-trans-fim dt-venci-ini 
         dt-venci-fim c-refer-ini c-refer-fim dt-maq-ini dt-maq-fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  ENABLE RECT-10 rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-cla}
  DISPLAY rs-destino c-arquivo rs-execucao imp-par 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE rs-destino bt-config-impr bt-arquivo c-arquivo rs-execucao imp-par 
         RECT-12 RECT-7 RECT-9 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY c-origem tg-excel c-arquivo-csv 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE RECT-13 RECT-14 l-tit-subs l-nota-db-cr c-origem tg-excel 
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
   APPLY "CLOSE":U TO THIS-PROCEDURE.

   RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-relat 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  assign dt-trans-ini:screen-value in frame f-pg-sel = "today"
         dt-trans-fim:screen-value in frame f-pg-sel = "today"
         dt-venci-ini:screen-value in frame f-pg-sel = "today"
         dt-venci-fim:screen-value in frame f-pg-sel = "today"
         dt-maq-ini:screen-value in frame f-pg-sel = "today"
         dt-maq-fim:screen-value in frame f-pg-sel = "today".


  /* Code placed here will execute AFTER standard behavior.    */
/*   &if "{&mgadm_version}" < "2.06b" &then                       */
/*     IF l-selec-ref = NO THEN DO:                               */
/*        assign c-referencia-ini:VISIBLE in frame f-pg-sel = NO  */
/*               IMAGE-15:VISIBLE in frame f-pg-sel = NO          */
/*               IMAGE-16:VISIBLE in frame f-pg-sel = NO          */
/*               c-refer-fim:VISIBLE in frame f-pg-sel = no.      */
/*     END.                                                       */
/*   &endif                                                       */

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

    /* Coloque aqui as validacoees das outras paginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na pÿgina com 
       problemas e colocar o focus no campo com problemas */

    if  input frame f-pg-sel c-cod-estabel-fim <
        input frame f-pg-sel c-cod-estabel-ini then do:

        RUN utp/ut-msgs.p(INPUT "show",
                          INPUT 17006,
                          INPUT "Campo final menor que campo inicial." + "~~" + "Codigo Estabelecimento Final menor que codigo Estabelecimento inicial.").
        apply 'entry' to c-cod-estabel-ini in frame f-pg-sel.
        return error.
    end.

    if  input frame f-pg-sel c-cod-esp-fim <
        input frame f-pg-sel c-cod-esp-ini then do:

        RUN utp/ut-msgs.p(INPUT "show",
                          INPUT 17006,
                          INPUT "Campo final menor que campo inicial." + "~~" + "Codigo especie final menor que codigo especie inicial.").
        apply 'entry' to c-cod-esp-ini in frame f-pg-sel.
        return error.
    end.

    if  input frame f-pg-sel i-cod-emit-fim <
        input frame f-pg-sel i-cod-emit-ini then do:
        RUN utp/ut-msgs.p(INPUT "show",
                          INPUT 17006,
                          INPUT "Campo final menor que campo inicial." + "~~" + "Codigo fornecedor final menor que codigo fornecedor inicial.").
        apply 'entry' to i-cod-emit-ini in frame f-pg-sel.
        return error.
    end.

    if  input frame f-pg-sel dt-trans-fim <
        input frame f-pg-sel dt-trans-ini then do:
        RUN utp/ut-msgs.p(INPUT "show",
                          INPUT 17006,
                          INPUT "Campo final menor que campo inicial." + "~~" + "Data transacao final menor que data transacao inicial.").
        apply 'entry' to dt-trans-ini in frame f-pg-sel.
        return error.
    end.

    if  input frame f-pg-sel dt-venci-fim <
        input frame f-pg-sel dt-venci-ini then do:
        RUN utp/ut-msgs.p(INPUT "show",
                          INPUT 17006,
                          INPUT "Campo final menor que campo inicial." + "~~" + "Data transacao final menor que data transacao inicial.").
        apply 'entry' to dt-trans-ini in frame f-pg-sel.
        return error.
    end.

    if  input frame f-pg-sel c-refer-fim <
        input frame f-pg-sel c-refer-ini then do:
        RUN utp/ut-msgs.p(INPUT "show",
                          INPUT 17006,
                          INPUT "Campo final menor que campo inicial." + "~~" + "Referencia final menor que referencia inicial.").
        apply 'entry' to c-refer-ini in frame f-pg-sel.
        return error.
    end.    

    /* Aqui s’o gravados os campos da temp-table que serÿ passada como par³metro
       para o programa RP.P */

    create tt-param.
    assign tt-param.usuario          = c-seg-usuario
           tt-param.destino          = input frame f-pg-imp rs-destino
           tt-param.arquivo          = input frame f-pg-imp c-arquivo
           tt-param.data-exec        = today
           tt-param.hora-exec        = time
           tt-param.classifica       = input frame f-pg-cla rs-classif
           tt-param.desc-classifica  = entry((tt-param.classifica - 1) * 2 + 1, 
                                             rs-classif:radio-buttons in frame f-pg-cla)   
           tt-param.cod-estabel-ini  = input frame f-pg-sel c-cod-estabel-ini  
           tt-param.cod-estabel-fim  = input frame f-pg-sel c-cod-estabel-fim   
           tt-param.cod-esp-ini      = input frame f-pg-sel c-cod-esp-ini 
           tt-param.cod-esp-fim      = input frame f-pg-sel c-cod-esp-fim  
           tt-param.cod-emit-ini     = input frame f-pg-sel i-cod-emit-ini
           tt-param.cod-emit-fim     = input frame f-pg-sel i-cod-emit-fim
           tt-param.dt-trans-ini     = input frame f-pg-sel dt-trans-ini
           tt-param.dt-trans-fim     = input frame f-pg-sel dt-trans-fim
           tt-param.dt-venci-ini     = input frame f-pg-sel dt-venci-ini
           tt-param.dt-venci-fim     = input frame f-pg-sel dt-venci-fim
           tt-param.l-tit-subs       = input frame f-pg-par l-tit-subs
           tt-param.l-nota-db-cr     = input frame f-pg-par l-nota-db-cr
           tt-param.c-origem         = input frame f-pg-par c-origem
           tt-param.ep-codigo        = v_cdn_empres_usuar
           tt-param.imp-par          = input frame f-pg-imp imp-par
           tt-param.referencia-ini   = input frame f-pg-sel c-refer-ini
           tt-param.referencia-fim   = input frame f-pg-sel c-refer-fim
           tt-param.dt-maq-ini       = input frame f-pg-sel dt-maq-ini
           tt-param.dt-maq-fim       = input frame f-pg-sel dt-maq-fim
           tt-param.tg-excel         = input frame f-pg-par tg-excel
           tt-param.c-arquivo-csv    = input frame f-pg-par c-arquivo-csv.


    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".

    /* Coloque aqui a l½gica de grava»’o dos demais campos que devem ser passados
       como par³metros para o programa RP.P, atrav²s da temp-table tt-param */



    /* Executar do programa RP.P que irÿ criar o relat½rio */
    {include/i-rpexb.i}

    SESSION:SET-WAIT-STATE("general":U).

    {include/i-rprun.i esp/esap0749rp.p}

    {include/i-rpexc.i}

    SESSION:SET-WAIT-STATE("":U).

    {include/i-rptrm.i}
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-SalvarFile w-relat 
PROCEDURE pi-SalvarFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT-OUTPUT PARAMETER cNomeArq AS CHAR NO-UNDO. 
    DEF INPUT PARAMETER cDiretorio      AS CHAR NO-UNDO. 
     
    DEF VAR lok                         AS LOG. 

    assign cArqConv = replace(cNomeArq, "/", "\"). 
    SYSTEM-DIALOG GET-FILE cArqConv 
        FILTERS 
                
                "*.csv" "*.csv"
               
        ASK-OVERWRITE 
       DEFAULT-EXTENSION "csv" 
       INITIAL-DIR cDiretorio 
       save-as 
       USE-FILENAME 
       UPDATE lOK. 
    if  lOK = yes then do: 

        


              
            c-arquivo-csv:SCREEN-VALUE IN FRAME f-pg-par = cArqConv.
        
    end. 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina w-relat 
PROCEDURE pi-troca-pagina :
/*------------------------------------------------------------------------------
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-relat, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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


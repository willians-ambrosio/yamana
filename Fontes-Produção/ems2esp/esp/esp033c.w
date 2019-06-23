&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESP033C 12.1.13.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/*:T Preprocessadores do Template de Relat¢rio                            */
/*:T Obs: Retirar o valor do preprocessador para as p†ginas que n∆o existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA 
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG 
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

define temp-table tt-param
    field destino              as integer
    field arquivo              as char
    field usuario              as char
    field data-exec            as date
    field hora-exec            as integer
    field parametro            as logical
    field formato              as integer
    field i-ferra-abre         as integer
    field v_num_tip_aces_usuar as integer
    field ep-codigo            as char
    field v-cod-tipo-grafico-dv203 as character
    field c-cod-estabel-ini     like item.cod-estabel
    field c-cod-estabel-fim     like item.cod-estabel
    field c-fm-codigo-ini       like item.fm-codigo
    field c-fm-codigo-fim       like item.fm-codigo
    field c-it-codigo-ini       like item.it-codigo
    field c-it-codigo-fim       like item.it-codigo
    field c-cod-localiz-ini     like item.cod-localiz
    field c-cod-localiz-fim     like item.cod-localiz
    field c-demanda-ini         like item.demanda 
    field c-demanda-fim         like item.demanda 
    field c-classif-abc-ini     like item.classif-abc
    field c-classif-abc-fim     like item.classif-abc
    field c-criticidade-ini     like item.criticidade
    field c-criticidade-fim     like item.criticidade
    FIELD c-fm-cod-com-ini      LIKE item.fm-cod-com 
    FIELD c-fm-cod-com-fim      LIKE item.fm-cod-com 
    FIELD dt-implantacao-ini    like item.data-implant
    FIELD dt-implantacao-fim    like item.data-implant
    FIELD rd-tipo-descricao     AS INT
    FIELD c-status-item         AS CHAR.

def temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

def buffer b-tt-digita for tt-digita.

/* Transfer Definitions */
def var raw-param as raw no-undo.

def temp-table tt-raw-digita
    field raw-digita as raw.
                    
/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-impressora-old     as char    no-undo. 
def var c-arquivo-old        as char    no-undo. 
def var c-destino-old        as char    no-undo. 

def var v-cod-pg-mouse-selec as char    no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-imp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-9 RECT-10 rs-destino c-arquivo ~
bt-config-impr bt-arquivo rs-execucao tb-parametro rs-formato rs-ferra-abre ~
text-parametro 
&Scoped-Define DISPLAYED-OBJECTS rs-destino c-arquivo rs-execucao ~
tb-parametro rs-formato rs-ferra-abre text-parametro 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
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

DEFINE VARIABLE text-parametro AS CHARACTER FORMAT "X(256)" INITIAL "Abrir Com" 
      VIEW-AS TEXT 
     SIZE 24.72 BY .63 NO-UNDO.

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

DEFINE VARIABLE rs-ferra-abre AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Microsoft Excel", 1,
"Microsoft Internet Explorer", 2
     SIZE 38.57 BY .92 NO-UNDO.

DEFINE VARIABLE rs-formato AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "80 colunas", 1,
"132 colunas", 2
     SIZE 32 BY .92 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 3.5.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE tb-parametro AS LOGICAL INITIAL no 
     LABEL "Imprimir P†gina de ParÉmetros" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .83 NO-UNDO.

DEFINE VARIABLE v-cod-tipo-grafico-dv203 AS CHARACTER FORMAT "x(30)" 
     LABEL "Tipo Gr†fico" 
     VIEW-AS COMBO-BOX 
     LIST-ITEMS "<<Nenhum>>","Colunas Agrupadas","Colunas Agrupadas 3D","Colunas 3D","Barras  Agrupadas","Barras  Agrupadas 3D","Linhas","Linhas  Com Marcadores","Linhas  3D","Pizza","Pizza   Explodida","Pizza   3D","Pizza   Explodida 3D","Colunas Cilindricas Agrupadas","Barras  Cilindricas Agrupadas","Colunas Cilindricas 3D","Colunas Cìnicas Agrupadas","Barras  Cìnicas Agrupadas","Colunas Cìnicas 3D","Colunas Piramidais Agrupadas","Barras  Piramidais Agrupadas","Colunas Piramidais 3D" 
     DROP-DOWN-LIST
     SIZE 31 BY 1
     FONT 1.

DEFINE VARIABLE rd-tipo-descricao AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Listar Descriá∆o", 1,
"Listar Narrativa Completa", 2
     SIZE 27 BY 2 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 4.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 4.

DEFINE RECTANGLE ret-par-fill
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 1.5.

DEFINE VARIABLE tg-ativo AS LOGICAL INITIAL yes 
     LABEL "Ativo" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-obs-ord-aut AS LOGICAL INITIAL no 
     LABEL "Obsol. Ord. Aut." 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE tg-obs-tds-ordens AS LOGICAL INITIAL no 
     LABEL "Obsol. Todas Ordens" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .83 NO-UNDO.

DEFINE VARIABLE tg-obs-total AS LOGICAL INITIAL no 
     LABEL "Obsol. Total" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE c-classif-abc-fim AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE c-classif-abc-ini AS CHARACTER FORMAT "X(256)":U 
     LABEL "Classif.ABC" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE c-criticidade-fim AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE c-criticidade-ini AS CHARACTER FORMAT "X(256)":U 
     LABEL "Criticidade" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE c-demanda-fim AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE c-demanda-ini AS CHARACTER FORMAT "X(256)":U 
     LABEL "Demanda" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE c-cod-estabel-fim AS CHARACTER FORMAT "X(3)":U INITIAL "ZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-estabel-ini AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-localiz-fim AS CHARACTER FORMAT "X(10)":U INITIAL "ZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-localiz-ini AS CHARACTER FORMAT "X(10)":U 
     LABEL "Localizaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE c-fm-cod-com-fim AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE c-fm-cod-com-ini AS CHARACTER FORMAT "X(8)":U 
     LABEL "Familia Comercial" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE c-fm-codigo-fim AS CHARACTER FORMAT "X(15)":U INITIAL "ZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE c-fm-codigo-ini AS CHARACTER FORMAT "X(15)":U 
     LABEL "Fam°lia" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE c-it-codigo-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE c-it-codigo-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE dt-implantacao-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE dt-implantacao-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Implantaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

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

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-33
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-34
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-35
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-36
     FILENAME "image\im-las":U
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

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.38 COL 3.29 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     c-arquivo AT ROW 3.54 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     bt-config-impr AT ROW 3.58 COL 43.29 HELP
          "Configuraá∆o da impressora"
     bt-arquivo AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     rs-execucao AT ROW 5.75 COL 3 HELP
          "Modo de Execuá∆o" NO-LABEL
     tb-parametro AT ROW 7.92 COL 3.14
     rs-formato AT ROW 8.79 COL 3 HELP
          "Formato de Impress∆o" NO-LABEL
     rs-ferra-abre AT ROW 9.71 COL 3 NO-LABEL
     text-destino AT ROW 1.63 COL 3.86 NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     text-parametro AT ROW 7.17 COL 1.29 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
     RECT-10 AT ROW 7.46 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10.

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     im-pg-sel AT ROW 1.5 COL 2.14
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-imp AT ROW 1.5 COL 33.57
     rt-folder AT ROW 2.5 COL 2
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-6 AT ROW 13.75 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar WIDGET-ID 100.

DEFINE FRAME f-pg-sel
     c-cod-estabel-ini AT ROW 1.25 COL 17.72 COLON-ALIGNED WIDGET-ID 4
     c-cod-estabel-fim AT ROW 1.25 COL 45.14 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     c-fm-codigo-ini AT ROW 2.25 COL 17.72 COLON-ALIGNED
     c-fm-codigo-fim AT ROW 2.25 COL 45.14 COLON-ALIGNED NO-LABEL
     c-it-codigo-ini AT ROW 3.33 COL 17.72 COLON-ALIGNED WIDGET-ID 12
     c-it-codigo-fim AT ROW 3.33 COL 45.14 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     c-cod-localiz-ini AT ROW 4.42 COL 17.72 COLON-ALIGNED WIDGET-ID 20
     c-cod-localiz-fim AT ROW 4.42 COL 45.14 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     c-demanda-ini AT ROW 5.46 COL 17.72 COLON-ALIGNED WIDGET-ID 26
     c-demanda-fim AT ROW 5.46 COL 45.29 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     c-classif-abc-ini AT ROW 6.67 COL 17.72 COLON-ALIGNED WIDGET-ID 38
     c-classif-abc-fim AT ROW 6.67 COL 45.29 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     c-criticidade-ini AT ROW 7.88 COL 17.72 COLON-ALIGNED WIDGET-ID 46
     c-criticidade-fim AT ROW 7.88 COL 45.29 COLON-ALIGNED NO-LABEL WIDGET-ID 44
     c-fm-cod-com-ini AT ROW 9.13 COL 17.72 COLON-ALIGNED WIDGET-ID 52
     c-fm-cod-com-fim AT ROW 9.13 COL 45.29 COLON-ALIGNED NO-LABEL WIDGET-ID 58
     dt-implantacao-ini AT ROW 10.13 COL 17.72 COLON-ALIGNED WIDGET-ID 60
     dt-implantacao-fim AT ROW 10.13 COL 45.29 COLON-ALIGNED NO-LABEL WIDGET-ID 66
     IMAGE-1 AT ROW 2.25 COL 37
     IMAGE-2 AT ROW 2.25 COL 43.57
     IMAGE-3 AT ROW 1.25 COL 37 WIDGET-ID 6
     IMAGE-4 AT ROW 1.25 COL 43.57 WIDGET-ID 8
     IMAGE-5 AT ROW 3.33 COL 37 WIDGET-ID 14
     IMAGE-6 AT ROW 3.33 COL 43.57 WIDGET-ID 16
     IMAGE-7 AT ROW 4.42 COL 37 WIDGET-ID 22
     IMAGE-8 AT ROW 4.42 COL 43.57 WIDGET-ID 24
     IMAGE-9 AT ROW 5.54 COL 37 WIDGET-ID 30
     IMAGE-10 AT ROW 5.54 COL 43.57 WIDGET-ID 32
     IMAGE-11 AT ROW 6.75 COL 37 WIDGET-ID 42
     IMAGE-12 AT ROW 6.75 COL 43.57 WIDGET-ID 40
     IMAGE-13 AT ROW 7.96 COL 37 WIDGET-ID 48
     IMAGE-14 AT ROW 7.96 COL 43.57 WIDGET-ID 50
     IMAGE-33 AT ROW 9.13 COL 37 WIDGET-ID 54
     IMAGE-34 AT ROW 9.13 COL 43.57 WIDGET-ID 56
     IMAGE-35 AT ROW 10.13 COL 37 WIDGET-ID 62
     IMAGE-36 AT ROW 10.13 COL 43.57 WIDGET-ID 64
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62 WIDGET-ID 100.

DEFINE FRAME f-pg-par
     v-cod-tipo-grafico-dv203 AT ROW 1.33 COL 14 COLON-ALIGNED
     tg-ativo AT ROW 3.33 COL 48.86 WIDGET-ID 6
     rd-tipo-descricao AT ROW 4.13 COL 4 NO-LABEL WIDGET-ID 2
     tg-obs-ord-aut AT ROW 4.33 COL 48.86 WIDGET-ID 10
     tg-obs-tds-ordens AT ROW 5.33 COL 48.86 WIDGET-ID 12
     tg-obs-total AT ROW 6.33 COL 48.86 WIDGET-ID 14
     "Status do Item" VIEW-AS TEXT
          SIZE 14 BY .67 AT ROW 2.75 COL 42 WIDGET-ID 20
     "Descriá∆o do Item" VIEW-AS TEXT
          SIZE 19 BY .67 AT ROW 2.75 COL 2.29 WIDGET-ID 22
     ret-par-fill AT ROW 1 COL 2
     RECT-11 AT ROW 3.25 COL 2 WIDGET-ID 16
     RECT-12 AT ROW 3.25 COL 41.86 WIDGET-ID 18
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
         TITLE              = "<Title>"
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
/* SETTINGS FOR FRAME f-pg-imp
   FRAME-NAME                                                           */
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

       def var v-ch-Excel as com-handle no-undo.

       IF input frame f-pg-imp rs-execucao = 1  and
          NUM-ENTRIES(INPUT FRAME f-pg-imp c-arquivo,":") < 2 and
          input frame f-pg-imp rs-destino = 2
       THEN DO:
           MESSAGE "Deve ser informado o caminho completo para o destino do arquivo." VIEW-AS ALERT-BOX ERROR.
           RETURN NO-APPLY.
       END.

       case input frame f-pg-par v-cod-tipo-grafico-dv203:lookup(input frame f-pg-par v-cod-tipo-grafico-dv203):
           when(1)  then assign v-cod-tipo-grafico-dv203 = string(0).
           when(2)  then assign v-cod-tipo-grafico-dv203 = string(51).
           when(3)  then assign v-cod-tipo-grafico-dv203 = string(54).
           when(4)  then assign v-cod-tipo-grafico-dv203 = string(-4100).
           when(5)  then assign v-cod-tipo-grafico-dv203 = string(57).
           when(6)  then assign v-cod-tipo-grafico-dv203 = string(60).
           when(7)  then assign v-cod-tipo-grafico-dv203 = string(4).
           when(8)  then assign v-cod-tipo-grafico-dv203 = string(65).
           when(9)  then assign v-cod-tipo-grafico-dv203 = string(-4101).
           when(10) then assign v-cod-tipo-grafico-dv203 = string(5).
           when(11) then assign v-cod-tipo-grafico-dv203 = string(69).
           when(12) then assign v-cod-tipo-grafico-dv203 = string(-4102).
           when(13) then assign v-cod-tipo-grafico-dv203 = string(70).
           when(14) then assign v-cod-tipo-grafico-dv203 = string(92).
           when(15) then assign v-cod-tipo-grafico-dv203 = string(95).
           when(16) then assign v-cod-tipo-grafico-dv203 = string(98).
           when(17) then assign v-cod-tipo-grafico-dv203 = string(99).
           when(18) then assign v-cod-tipo-grafico-dv203 = string(102).
           when(19) then assign v-cod-tipo-grafico-dv203 = string(105).
           when(20) then assign v-cod-tipo-grafico-dv203 = string(106).
           when(21) then assign v-cod-tipo-grafico-dv203 = string(109).
           when(22) then assign v-cod-tipo-grafico-dv203 = string(112).
       end case.

       IF  NUM-ENTRIES(INPUT FRAME f-pg-imp c-arquivo,".") <>  2 and
           input frame f-pg-imp rs-destino = 2
       THEN DO:
           MESSAGE "N∆o foi informada a extens∆o do arquivo destino." VIEW-AS ALERT-BOX ERROR.
           RETURN NO-APPLY.
       END.

       IF input frame f-pg-imp rs-destino = 2 and
          ENTRY(2,INPUT FRAME f-pg-imp c-arquivo,".") <> "csv"
       THEN DO:
           ASSIGN c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = ENTRY(1,INPUT FRAME f-pg-imp c-arquivo,".") + ".csv".
           MESSAGE "A extens∆o do arquivo destino deve ser [CSV]. Portanto, foi alterada automaticamente"VIEW-AS ALERT-BOX INFORMATION.
       END.

       if input frame f-pg-imp rs-execucao <> 2  /* Batch */
       then do:
           CREATE "Excel.Application" v-ch-Excel NO-ERROR.
           v-ch-excel:DisplayAlerts = false no-error.
           if index(v-ch-Excel:Version, "8.") <> 0
           then do:
               if input frame f-pg-imp rs-ferra-abre = 2  /* Internet Explorer */
               then do:
                   v-ch-Excel:quit().
                   release object v-ch-excel.
                   assign v-ch-excel = ?.
                   message "Para utilizar o Microsoft Internet Explorer," skip "Ç necess†rio possuir o Microsoft Office 2000."VIEW-AS ALERT-BOX ERROR.
                   return no-apply.
               end.
               if input frame f-pg-imp rs-ferra-abre  = 1 and  /* Excel */
                  integer(v-cod-tipo-grafico-dv203)  <> 0
               then do:
                   v-ch-Excel:quit().
                   release object v-ch-excel.
                   assign v-ch-excel = ?.
                   message "O Microsoft Office utilizado Ç inferior ao 2000." skip "O gr†fico n∆o ser† gerado na planilha."VIEW-AS ALERT-BOX INFORMATION.
               end.
               if v-ch-excel <> ?
               then do:
                   v-ch-Excel:quit().
                   release object v-ch-excel.
                   assign v-ch-excel = ?.
               end.
           end.
       end.

       if v-ch-excel <> ?
       then do:
           v-ch-Excel:quit().
           release object v-ch-excel.
           assign v-ch-excel = ?.
       end.

       run pi-executar.
   end.
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
    ON VALUE-CHANGED OF rs-destino IN FRAME f-pg-imp
    DO:
       do  with frame f-pg-imp:
           case self:screen-value:
              when "1" then do:
                  if c-destino-old = "2" then assign c-arquivo-old = c-arquivo:screen-value.
                  assign c-arquivo:sensitive    = no
                         c-destino-old          = "1"
                         c-arquivo:visible      = yes
                         c-arquivo:screen-value  = c-impressora-old
                         bt-arquivo:visible     = no
                         bt-config-impr:visible = yes.
                end.

                when "2" then do:
                   if c-destino-old = "1" then assign c-impressora-old = c-arquivo:screen-value.
                   if  input frame f-pg-imp rs-execucao = 2 /* Batch */ 
                       THEN assign c-arquivo-old = 'ESP033C' + "." + 'CSV'.
                       ELSE assign c-arquivo-old = session:temp-directory + 'ESP033C' + "." + 'CSV'.
                   assign c-arquivo:sensitive     = yes
                          c-destino-old           = "2"
                          c-arquivo:visible       = yes
                          c-arquivo:screen-value  = c-arquivo-old
                          bt-arquivo:visible      = yes
                          bt-config-impr:visible  = no.
                end.

                when "3" then do:
                   if c-destino-old = "2" then assign c-arquivo-old = c-arquivo:screen-value.
                   if c-destino-old = "1" then assign c-impressora-old = c-arquivo:screen-value.
                   assign c-arquivo:sensitive     = no
                          c-destino-old           = "3"
                          c-arquivo:visible       = no
                          bt-arquivo:visible      = no
                          bt-config-impr:visible  = no.
                end.
           end case.
       end.
    END.
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESP033C" "12.1.13.000"}

/*:T inicializaá‰es do template de relat¢rio */
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

    assign v-cod-tipo-grafico-dv203 = "<<Nenhum>>".

    RUN enable_UI.
  
    ASSIGN c-demanda-ini    :list-items        in frame f-pg-sel = {ininc/i02in122.i 03}.
    ASSIGN c-demanda-fim    :list-items        in frame f-pg-sel = {ininc/i02in122.i 03}.
    ASSIGN c-classif-abc-ini:list-items        in frame f-pg-sel = {ininc/i03in172.i 03}.
    ASSIGN c-classif-abc-fim:list-items        in frame f-pg-sel = {ininc/i03in172.i 03}.
    ASSIGN c-criticidade-ini:list-items        in frame f-pg-sel = {ininc/i06in095.i 03}.
    ASSIGN c-criticidade-fim:list-items        in frame f-pg-sel = {ininc/i06in095.i 03}.

    ASSIGN c-demanda-ini    :SCREEN-VALUE      in frame f-pg-sel = ENTRY(1,c-demanda-ini    :LIST-ITEMS IN FRAME f-pg-sel).
    ASSIGN c-demanda-fim    :SCREEN-VALUE      in frame f-pg-sel = ENTRY(  c-demanda-fim    :NUM-ITEMS  IN FRAME f-pg-sel ,c-demanda-fim    :LIST-ITEMS IN FRAME f-pg-sel).
    ASSIGN c-classif-abc-ini:SCREEN-VALUE      in frame f-pg-sel = ENTRY(1,c-classif-abc-ini:LIST-ITEMS IN FRAME f-pg-sel).
    ASSIGN c-classif-abc-fim:SCREEN-VALUE      in frame f-pg-sel = ENTRY(  c-classif-abc-FIM:NUM-ITEMS  IN FRAME f-pg-sel ,c-classif-abc-fim:LIST-ITEMS IN FRAME f-pg-sel).
    ASSIGN c-criticidade-ini:SCREEN-VALUE      in frame f-pg-sel = ENTRY(1,c-criticidade-ini:LIST-ITEMS IN FRAME f-pg-sel).
    ASSIGN c-criticidade-fim:SCREEN-VALUE      in frame f-pg-sel = ENTRY(  c-criticidade-fim:NUM-ITEMS  IN FRAME f-pg-sel ,c-criticidade-fim:LIST-ITEMS IN FRAME f-pg-sel).

    {include/i-rpmbl.i}
  
    apply "value-changed" to rs-destino in frame f-pg-imp.
    assign rs-destino:screen-value in frame f-pg-imp = "3".
    assign text-parametro:visible in frame f-pg-imp = yes
           rect-10:visible        in frame f-pg-imp = yes
           tb-parametro:visible   in frame f-pg-imp = yes
           rs-formato:visible     in frame f-pg-imp = yes.
        ASSIGN text-destino:screen-value   IN FRAME f-pg-imp = "Destino".
        ASSIGN text-modo:screen-value      IN FRAME f-pg-imp = "Execuªío".
        ASSIGN text-parametro:screen-value IN FRAME f-pg-imp = "Abrir Com".
    
        assign rs-destino:radio-buttons in frame f-pg-imp = {varinc/var00002.i 07}
               rs-destino:screen-value  in frame f-pg-imp = "3":U.
        assign rs-formato:radio-buttons in frame f-pg-imp = {varinc/var00176.i 07}
               rs-formato:screen-value  in frame f-pg-imp = "2":U.
    
               rs-formato:screen-value  in frame f-pg-imp = "2":U.
    
    
        assign v-cod-pg-mouse-selec = "im-pg-sel".
    
        apply "value-changed" to rs-destino in frame f-pg-imp.
    
        if v-cod-pg-mouse-selec = "im-pg-sel"
        then
            apply "mouse-select-click" to im-pg-sel in frame f-relat.
    
        if v-cod-pg-mouse-selec = "im-pg-par"
        then
            apply "mouse-select-click" to im-pg-par in frame f-relat.
    
        if v-cod-pg-mouse-selec = "im-pg-imp"
        then
            apply "mouse-select-click" to im-pg-imp in frame f-relat.
    
         apply "entry" to frame f-Relat.
    
        rs-formato:disable(entry(1,rs-formato:radio-buttons in frame f-pg-imp)) in frame f-pg-imp.
    
        if  im-pg-sel:sensitive in frame f-relat = no then do:
                 run pi-muda-cor-label-folder(input "Seleá∆o").
    
        end.
    
        if  im-pg-par:sensitive in frame f-relat = no then do:
            run pi-muda-cor-label-folder(input "Par≥metros").
    
        end.

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
  ENABLE im-pg-sel im-pg-par im-pg-imp bt-executar bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY c-cod-estabel-ini c-cod-estabel-fim c-fm-codigo-ini c-fm-codigo-fim 
          c-it-codigo-ini c-it-codigo-fim c-cod-localiz-ini c-cod-localiz-fim 
          c-demanda-ini c-demanda-fim c-classif-abc-ini c-classif-abc-fim 
          c-criticidade-ini c-criticidade-fim c-fm-cod-com-ini c-fm-cod-com-fim 
          dt-implantacao-ini dt-implantacao-fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 
         IMAGE-9 IMAGE-10 IMAGE-11 IMAGE-12 IMAGE-13 IMAGE-14 IMAGE-33 IMAGE-34 
         IMAGE-35 IMAGE-36 c-cod-estabel-ini c-cod-estabel-fim c-fm-codigo-ini 
         c-fm-codigo-fim c-it-codigo-ini c-it-codigo-fim c-cod-localiz-ini 
         c-cod-localiz-fim c-demanda-ini c-demanda-fim c-classif-abc-ini 
         c-classif-abc-fim c-criticidade-ini c-criticidade-fim c-fm-cod-com-ini 
         c-fm-cod-com-fim dt-implantacao-ini dt-implantacao-fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino c-arquivo rs-execucao tb-parametro rs-formato rs-ferra-abre 
          text-parametro 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 RECT-10 rs-destino c-arquivo bt-config-impr bt-arquivo 
         rs-execucao tb-parametro rs-formato rs-ferra-abre text-parametro 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY v-cod-tipo-grafico-dv203 tg-ativo rd-tipo-descricao tg-obs-ord-aut 
          tg-obs-tds-ordens tg-obs-total 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE ret-par-fill RECT-11 RECT-12 v-cod-tipo-grafico-dv203 tg-ativo 
         rd-tipo-descricao tg-obs-ord-aut tg-obs-tds-ordens tg-obs-total 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar w-relat 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var r-tt-digita as rowid no-undo.
DEFINE VARIABLE status-item AS CHARACTER   NO-UNDO.
do on error undo, return error on stop  undo, return error:
    {include/i-rpexa.i}
    
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
    
    /*:T Coloque aqui as validaá‰es da p†gina de Digitaá∆o, lembrando que elas devem
       apresentar uma mensagem de erro cadastrada, posicionar nesta p†gina e colocar
       o focus no campo com problemas */
    /*browse br-digita:SET-REPOSITIONED-ROW (browse br-digita:DOWN, "ALWAYS":U).*/
    
    /*
    for each tt-digita no-lock:
        assign r-tt-digita = rowid(tt-digita).
        
        /*:T Validaá∆o de duplicidade de registro na temp-table tt-digita */
        find first b-tt-digita where b-tt-digita.ordem = tt-digita.ordem and 
                                     rowid(b-tt-digita) <> rowid(tt-digita) no-lock no-error.
        if avail b-tt-digita then do:
            apply "MOUSE-SELECT-CLICK":U to im-pg-dig in frame f-relat.
            reposition br-digita to rowid rowid(b-tt-digita).
            
            run utp/ut-msgs.p (input "show":U, input 108, input "").
            apply "ENTRY":U to tt-digita.ordem in browse br-digita.
            
            return error.
        end.
        
        /*:T As demais validaá‰es devem ser feitas aqui */
        if tt-digita.ordem <= 0 then do:
            assign browse br-digita:CURRENT-COLUMN = tt-digita.ordem:HANDLE in browse br-digita.
            
            apply "MOUSE-SELECT-CLICK":U to im-pg-dig in frame f-relat.
            reposition br-digita to rowid r-tt-digita.
            
            run utp/ut-msgs.p (input "show":U, input 99999, input "").
            apply "ENTRY":U to tt-digita.ordem in browse br-digita.
            
            return error.
        end.
        
    end.
      */
    
    /*:T Coloque aqui as validaá‰es das outras p†ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p†gina com 
       problemas e colocar o focus no campo com problemas */
    
    
    
    /*:T Aqui s∆o gravados os campos da temp-table que ser† passada como parÉmetro
       para o programa RP.P */
    
   if  v_cdn_empres_usuar <> ?
   then
       assign i-ep-codigo-usuario = v_cdn_empres_usuar.

   create tt-param.
   assign tt-param.usuario                  = c-seg-usuario
          tt-param.destino                  = input frame f-pg-imp rs-destino
          tt-param.data-exec                = today
          tt-param.hora-exec                = time
          tt-param.parametro                = if input frame f-pg-imp tb-parametro = "yes" then yes else no
          tt-param.formato                  = if input frame f-pg-imp rs-formato   = "1" then 1 else 2
          tt-param.v_num_tip_aces_usuar     = v_num_tip_aces_usuar
          tt-param.ep-codigo                = i-ep-codigo-usuario
          tt-param.i-ferra-abre             = input frame f-pg-imp rs-ferra-abre
          tt-param.v-cod-tipo-grafico-dv203 = v-cod-tipo-grafico-dv203
          tt-param.c-fm-codigo-ini          = input frame f-pg-sel c-fm-codigo-ini
          tt-param.c-fm-codigo-fim          = input frame f-pg-sel c-fm-codigo-fim
          tt-param.c-cod-estabel-ini        = input frame f-pg-sel c-cod-estabel-ini
          tt-param.c-cod-estabel-fim        = input frame f-pg-sel c-cod-estabel-fim
          tt-param.c-it-codigo-ini          = input frame f-pg-sel c-it-codigo-ini
          tt-param.c-it-codigo-fim          = input frame f-pg-sel c-it-codigo-fim
          tt-param.c-cod-localiz-ini        = input frame f-pg-sel c-cod-localiz-ini
          tt-param.c-cod-localiz-fim        = input frame f-pg-sel c-cod-localiz-fim
          tt-param.c-demanda-ini            = INT(ENTRY(LOOKUP(INPUT FRAME f-pg-sel c-demanda-ini    ,REPLACE({ininc/i02in122.i 07}," ","")) + 1,{ininc/i02in122.i 07}))
          tt-param.c-demanda-fim            = INT(ENTRY(LOOKUP(INPUT FRAME f-pg-sel c-demanda-fim    ,REPLACE({ininc/i02in122.i 07}," ","")) + 1,{ininc/i02in122.i 07}))
          tt-param.c-classif-abc-ini        = INT(ENTRY(LOOKUP(INPUT FRAME f-pg-sel c-classif-abc-ini,REPLACE({ininc/i03in172.i 07}," ","")) + 1,{ininc/i03in172.i 07}))
          tt-param.c-classif-abc-fim        = INT(ENTRY(LOOKUP(INPUT FRAME f-pg-sel c-classif-abc-fim,REPLACE({ininc/i03in172.i 07}," ","")) + 1,{ininc/i03in172.i 07}))
          tt-param.c-criticidade-ini        = INT(ENTRY(LOOKUP(INPUT FRAME f-pg-sel c-criticidade-ini,REPLACE({ininc/i06in095.i 07}," ","")) + 1,{ininc/i06in095.i 07}))
          tt-param.c-criticidade-fim        = INT(ENTRY(LOOKUP(INPUT FRAME f-pg-sel c-criticidade-fim,REPLACE({ininc/i06in095.i 07}," ","")) + 1,{ininc/i06in095.i 07}))
          tt-param.c-fm-cod-com-ini         = input frame f-pg-sel c-fm-cod-com-ini  
          tt-param.c-fm-cod-com-fim         = input frame f-pg-sel c-fm-cod-com-fim  
          tt-param.dt-implantacao-ini       = input frame f-pg-sel dt-implantacao-ini
          tt-param.dt-implantacao-fim       = input frame f-pg-sel dt-implantacao-fim
          tt-param.rd-tipo-descricao        = INPUT FRAME f-pg-par rd-tipo-descricao.

          IF tg-ativo:CHECKED THEN
              ASSIGN status-item            = "1,".
          IF tg-obs-ord-aut:CHECKED THEN
              ASSIGN status-item            = status-item + "2,".
          IF tg-obs-tds-ordens:CHECKED THEN
              ASSIGN status-item            = status-item + "3,".
          IF tg-obs-total:CHECKED THEN
              ASSIGN status-item            = status-item + "4".

          ASSIGN tt-param.c-status-item = status-item.
                
    if  tt-param.destino = 2 then
        assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
    else do:
        find first usuar_mestre no-lock
             where usuar_mestre.cod_usuario = c-seg-usuario no-error.
        if  avail usuar_mestre and
            usuar_mestre.nom_dir_spool <> "" then
            assign tt-param.arquivo = usuar_mestre.nom_dir_spool + "\" + c-programa-mg97 + ".CSV":U.
        else
            assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".CSV":U.

    end.

    /*
    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
      */
    /*:T Coloque aqui a l¢gica de gravaá∆o dos demais campos que devem ser passados
       como parÉmetros para o programa RP.P, atravÇs da temp-table tt-param */
    
    /*:T Executar do programa RP.P que ir† criar o relat¢rio */
    {include/i-rpexb.i}
    SESSION:SET-WAIT-STATE("general":U).
    {include/i-rprun.i esp/esp033crp.p}
    
/*    {include/i-rpexc.i}*/
    SESSION:SET-WAIT-STATE("":U).
/*    {include/i-rptrm.i}*/
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina w-relat 
PROCEDURE pi-troca-pagina :
/*:T------------------------------------------------------------------------------
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


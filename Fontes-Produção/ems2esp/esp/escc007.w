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
{include/i-prgvrs.i ESCC007 2.06.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/*:T Preprocessadores do Template de Relat�rio                            */
/*:T Obs: Retirar o valor do preprocessador para as p�ginas que n�o existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA 
&GLOBAL-DEFINE PGPAR 
&GLOBAL-DEFINE PGDIG 
&GLOBAL-DEFINE PGIMP f-pg-imp

&GLOBAL-DEFINE RTF   NO
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

define temp-table tt-param no-undo
    field destino               as integer
    field arquivo               as char format "x(35)"
    field usuario               as char format "x(12)"
    field data-exec             as date
    field hora-exec             as integer
    FIELD c-cod-estabel-ini     LIKE estabelec.cod-estabel
    FIELD c-cod-estabel-fim     LIKE estabelec.cod-estabel
    FIELD d-data-emis-ini       LIKE ordem-compra.data-emissao
    FIELD d-data-emis-fim       LIKE ordem-compra.data-emissao
    FIELD c-it-codigo-ini       LIKE ITEM.it-codigo
    FIELD c-it-codigo-fim       LIKE ITEM.it-codigo
    FIELD c-ge-codigo-ini       LIKE ITEM.ge-codigo
    FIELD c-ge-codigo-fim       LIKE ITEM.ge-codigo
    FIELD i-cod-emitente-ini    LIKE emitente.cod-emitente
    FIELD i-cod-emitente-fim    LIKE emitente.cod-emitente
    .


/* Transfer Definitions */

define temp-table tt-digita
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id is primary unique
        ordem.
 
define buffer b-tt-digita for tt-digita.
 
/* Transfer Definitions */
 
def var raw-param        as raw no-undo.
 
def temp-table tt-raw-digita
   field raw-digita      as raw.
 
/* Local Variable Definitions ---                                       */
 
def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.

/*15/02/2005 - tech1007 - Variavel definida para tratar se o programa est� rodando no WebEnabler*/
DEFINE SHARED VARIABLE hWenController AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-imp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-9 bt-config-impr 
&Scoped-Define DISPLAYED-OBJECTS rs-destino c-arquivo rs-execucao 

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

DEFINE BUTTON bt-modelo-rtf 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U NO-FOCUS
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE c-modelo-rtf AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE text-modelo-rtf AS CHARACTER FORMAT "X(256)":U INITIAL "Modelo:" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execu��o" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE text-rtf AS CHARACTER FORMAT "X(256)":U INITIAL "Rich Text Format(RTF)" 
      VIEW-AS TEXT 
     SIZE 20.86 BY .63 NO-UNDO.

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

DEFINE RECTANGLE rect-rtf
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 3.54.

DEFINE VARIABLE l-habilitaRtf AS LOGICAL INITIAL no 
     LABEL "RTF" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY 1.08 NO-UNDO.

DEFINE VARIABLE c-cod-estabel-fim AS CHARACTER FORMAT "X(3)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE c-cod-estabel-ini AS CHARACTER FORMAT "X(3)" 
     LABEL "Estabelecimento":R7 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE c-ge-codigo-fim AS INTEGER FORMAT ">9":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE c-ge-codigo-ini AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Grupo Estoque":R16 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE c-it-codigo-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE c-it-codigo-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Item":R5 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .88 NO-UNDO.

DEFINE VARIABLE d-data-emis-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE d-data-emis-ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Data Emiss�o":R15 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE i-cod-emitente-fim AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE i-cod-emitente-ini AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Fornecedor":R8 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-31
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-32
     FILENAME "image\im-las":U
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

DEFINE IMAGE IMAGE-37
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-38
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

DEFINE IMAGE im-pg-imp
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
          "Dispara a execu��o do relat�rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     RECT-1 AT ROW 14.29 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     im-pg-imp AT ROW 1.5 COL 17.72
     im-pg-sel AT ROW 1.5 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar WIDGET-ID 100.

DEFINE FRAME f-pg-imp
     bt-modelo-rtf AT ROW 6.63 COL 43 HELP
          "Escolha do nome do arquivo" WIDGET-ID 2 NO-TAB-STOP 
     rs-destino AT ROW 1.63 COL 3.29 HELP
          "Destino de Impress�o do Relat�rio" NO-LABEL
     bt-config-impr AT ROW 2.71 COL 43.29 HELP
          "Configura��o da impressora"
     bt-arquivo AT ROW 2.71 COL 43.29 HELP
          "Escolha do nome do arquivo"
     c-arquivo AT ROW 2.75 COL 3.29 HELP
          "Nome do arquivo de destino do relat�rio" NO-LABEL
     l-habilitaRtf AT ROW 4.83 COL 3.29 WIDGET-ID 6
     rs-execucao AT ROW 5.08 COL 2.86 HELP
          "Modo de Execu��o" NO-LABEL
     c-modelo-rtf AT ROW 6.63 COL 3 HELP
          "Nome do arquivo de modelo do relat�rio" NO-LABEL WIDGET-ID 4
     text-destino AT ROW 1.04 COL 3.86 NO-LABEL
     text-rtf AT ROW 4.17 COL 1.14 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     text-modo AT ROW 4.33 COL 1.14 COLON-ALIGNED NO-LABEL
     text-modelo-rtf AT ROW 5.96 COL 1.14 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     "Obs.: O relat�rio ser� gerado em Excel." VIEW-AS TEXT
          SIZE 45 BY .67 AT ROW 9.04 COL 2.72 WIDGET-ID 14
          FGCOLOR 12 
     RECT-7 AT ROW 1.33 COL 2.14
     RECT-9 AT ROW 4.54 COL 2
     rect-rtf AT ROW 4.46 COL 2 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10.5 WIDGET-ID 100.

DEFINE FRAME f-pg-sel
     c-cod-estabel-ini AT ROW 2.17 COL 24.72 COLON-ALIGNED HELP
          "C�digo do estabelecimento"
     c-cod-estabel-fim AT ROW 2.17 COL 48.14 COLON-ALIGNED NO-LABEL
     d-data-emis-ini AT ROW 3.17 COL 18.72 COLON-ALIGNED WIDGET-ID 4
     d-data-emis-fim AT ROW 3.17 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     c-it-codigo-ini AT ROW 4.17 COL 11.57 COLON-ALIGNED HELP
          "C�digo do Item" WIDGET-ID 12
     c-it-codigo-fim AT ROW 4.17 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     c-ge-codigo-ini AT ROW 5.17 COL 25.72 COLON-ALIGNED HELP
          "Grupo de estoque a que pertence o item" WIDGET-ID 20
     c-ge-codigo-fim AT ROW 5.17 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     i-cod-emitente-ini AT ROW 6.17 COL 18.72 COLON-ALIGNED WIDGET-ID 28
     i-cod-emitente-fim AT ROW 6.21 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     IMAGE-1 AT ROW 2.17 COL 31.86
     IMAGE-2 AT ROW 2.17 COL 46
     IMAGE-31 AT ROW 3.17 COL 31.86 WIDGET-ID 6
     IMAGE-32 AT ROW 3.17 COL 46 WIDGET-ID 8
     IMAGE-33 AT ROW 4.17 COL 31.86 WIDGET-ID 14
     IMAGE-34 AT ROW 4.17 COL 46 WIDGET-ID 16
     IMAGE-35 AT ROW 5.17 COL 31.86 WIDGET-ID 22
     IMAGE-36 AT ROW 5.17 COL 46 WIDGET-ID 24
     IMAGE-37 AT ROW 6.17 COL 31.86 WIDGET-ID 30
     IMAGE-38 AT ROW 6.17 COL 46 WIDGET-ID 32
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62 WIDGET-ID 100.


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
         TITLE              = "Listagem Ordens x Itens"
         HEIGHT             = 15
         WIDTH              = 81
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
ASSIGN 
       FRAME f-pg-imp:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-arquivo IN FRAME f-pg-imp
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-modelo-rtf IN FRAME f-pg-imp
   NO-ENABLE                                                            */
ASSIGN 
       bt-modelo-rtf:HIDDEN IN FRAME f-pg-imp           = TRUE.

/* SETTINGS FOR EDITOR c-arquivo IN FRAME f-pg-imp
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR c-modelo-rtf IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       c-modelo-rtf:HIDDEN IN FRAME f-pg-imp           = TRUE.

/* SETTINGS FOR TOGGLE-BOX l-habilitaRtf IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       l-habilitaRtf:HIDDEN IN FRAME f-pg-imp           = TRUE.

/* SETTINGS FOR RECTANGLE rect-rtf IN FRAME f-pg-imp
   NO-ENABLE                                                            */
ASSIGN 
       rect-rtf:HIDDEN IN FRAME f-pg-imp           = TRUE.

/* SETTINGS FOR RADIO-SET rs-destino IN FRAME f-pg-imp
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rs-execucao IN FRAME f-pg-imp
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-modelo-rtf IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modelo-rtf:HIDDEN IN FRAME f-pg-imp           = TRUE
       text-modelo-rtf:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Modelo:".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execu��o".

/* SETTINGS FOR FILL-IN text-rtf IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-rtf:HIDDEN IN FRAME f-pg-imp           = TRUE
       text-rtf:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Rich Text Format(RTF)".

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
ON END-ERROR OF w-relat /* Listagem Ordens x Itens */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Listagem Ordens x Itens */
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
       run pi-executar.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-modelo-rtf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modelo-rtf w-relat
ON CHOOSE OF bt-modelo-rtf IN FRAME f-pg-imp
DO:
/*     def var c-arq-conv  as char no-undo.                                                          */
/*     def var l-ok as logical no-undo.                                                              */
/*                                                                                                   */
/*     assign c-modelo-rtf = replace(input frame {&frame-name} c-modelo-rtf, "/", "\").              */
/*     SYSTEM-DIALOG GET-FILE c-arq-conv                                                             */
/*        FILTERS "*.rtf" "*.rtf",                                                                   */
/*                "*.*" "*.*"                                                                        */
/*        DEFAULT-EXTENSION "rtf"                                                                    */
/*        INITIAL-DIR "modelos"                                                                      */
/*        MUST-EXIST                                                                                 */
/*        USE-FILENAME                                                                               */
/*        UPDATE l-ok.                                                                               */
/*     if  l-ok = yes then                                                                           */
/*         assign c-modelo-rtf:screen-value in frame {&frame-name}  = replace(c-arq-conv, "\", "/"). */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME im-pg-imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-imp w-relat
ON MOUSE-SELECT-CLICK OF im-pg-imp IN FRAME f-relat
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
&Scoped-define SELF-NAME l-habilitaRtf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-habilitaRtf w-relat
ON VALUE-CHANGED OF l-habilitaRtf IN FRAME f-pg-imp /* RTF */
DO:
/*     &IF "{&RTF}":U = "YES":U &THEN */
/*     RUN pi-habilitaRtf.            */
/*     &endif                         */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino w-relat
ON VALUE-CHANGED OF rs-destino IN FRAME f-pg-imp
DO:
/*Alterado 15/02/2005 - tech1007 - Evento alterado para correto funcionamento dos novos widgets
  utilizados para a funcionalidade de RTF*/
do  with frame f-pg-imp:
    case self:screen-value:
        when "1" then do:
            assign c-arquivo:sensitive    = no
                   bt-arquivo:visible     = no
                   bt-config-impr:visible = YES
                   /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para
                     verificar se o RTF est� ativo*/
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
                     verificar se o RTF est� ativo*/
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
                     verificar se o RTF est� ativo*/
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

/* &IF "{&RTF}":U = "YES":U &THEN */
/* RUN pi-habilitaRtf.            */
/* &endif                         */
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESCC007" "2.06.00.000"}

/*:T inicializa��es do template de relat�rio */
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
  ENABLE im-pg-imp im-pg-sel bt-executar bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY c-cod-estabel-ini c-cod-estabel-fim d-data-emis-ini d-data-emis-fim 
          c-it-codigo-ini c-it-codigo-fim c-ge-codigo-ini c-ge-codigo-fim 
          i-cod-emitente-ini i-cod-emitente-fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-1 IMAGE-2 IMAGE-31 IMAGE-32 IMAGE-33 IMAGE-34 IMAGE-35 IMAGE-36 
         IMAGE-37 IMAGE-38 c-cod-estabel-ini c-cod-estabel-fim d-data-emis-ini 
         d-data-emis-fim c-it-codigo-ini c-it-codigo-fim c-ge-codigo-ini 
         c-ge-codigo-fim i-cod-emitente-ini i-cod-emitente-fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 bt-config-impr 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  VIEW FRAME f-pg-imp IN WINDOW w-relat.
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
    /*14/02/2005 - tech1007 - Alterada condicao para n�o considerar mai o RTF como destino*/
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


    /*:T Coloque aqui as valida��es da p�gina de Digita��o, lembrando que elas devem
       apresentar uma mensagem de erro cadastrada, posicionar nesta p�gina e colocar
       o focus no campo com problemas */
    /*browse br-digita:SET-REPOSITIONED-ROW (browse br-digita:DOWN, "ALWAYS":U).*/
    
    /*:T Coloque aqui as valida��es das outras p�ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p�gina com 
       problemas e colocar o focus no campo com problemas */

    IF INPUT FRAME f-pg-sel c-cod-estabel-ini > 
        INPUT FRAME f-pg-sel c-cod-estabel-fim THEN
    DO:
        RUN utp/ut-msgs.p ('show',
                           17006,
                           'Estabelecimento Invalido~~Estabelecimento Final menor que Estabelecimento Inicial.').
        APPLY 'MOUSE-SELECT-CLICK':U  TO im-pg-sel IN FRAME f-relat.
        APPLY 'ENTRY':U TO c-cod-estabel-fim IN FRAME f-pg-sel.
        RETURN ERROR.
    END.    
    IF INPUT FRAME f-pg-sel d-data-emis-ini > 
        INPUT FRAME f-pg-sel d-data-emis-fim THEN
    DO:
        RUN utp/ut-msgs.p ('show',
                           17006,
                           'Data Invalida~~Data Final menor que Data Inicial.').
        APPLY 'MOUSE-SELECT-CLICK':U  TO im-pg-sel IN FRAME f-relat.
        APPLY 'ENTRY':U TO d-data-emis-fim IN FRAME f-pg-sel.
        RETURN ERROR.
    END.    
    IF INPUT FRAME f-pg-sel c-it-codigo-ini > 
        INPUT FRAME f-pg-sel c-it-codigo-fim THEN
    DO:
        RUN utp/ut-msgs.p ('show',
                           17006,
                           'Item Invalido~~Item Final menor que Item Inicial.').
        APPLY 'MOUSE-SELECT-CLICK':U  TO im-pg-sel IN FRAME f-relat.
        APPLY 'ENTRY':U TO c-it-codigo-fim IN FRAME f-pg-sel.
        RETURN ERROR.
    END.    
    IF INPUT FRAME f-pg-sel c-ge-codigo-ini > 
        INPUT FRAME f-pg-sel c-ge-codigo-fim THEN
    DO:
        RUN utp/ut-msgs.p ('show',
                           17006,
                           'Grupo de Estoque Invalido~~Grupo Estoque Final menor que Grupo Estoque Inicial.').
        APPLY 'MOUSE-SELECT-CLICK':U  TO im-pg-sel IN FRAME f-relat.
        APPLY 'ENTRY':U TO c-ge-codigo-fim IN FRAME f-pg-sel.
        RETURN ERROR.
    END.    
    IF INPUT FRAME f-pg-sel i-cod-emitente-ini > 
        INPUT FRAME f-pg-sel i-cod-emitente-fim THEN
    DO:
        RUN utp/ut-msgs.p ('show',
                           17006,
                           'Fornecedor Invalido~~Fornecedor Final menor que Fornecedor Inicial.').
        APPLY 'MOUSE-SELECT-CLICK':U  TO im-pg-sel IN FRAME f-relat.
        APPLY 'ENTRY':U TO i-cod-emitente-fim IN FRAME f-pg-sel.
        RETURN ERROR.
    END.    
    
    
    /*:T Aqui s�o gravados os campos da temp-table que ser� passada como par�metro
       para o programa RP.P */
    
    create tt-param.
    assign tt-param.usuario             = c-seg-usuario
           tt-param.destino             = input frame f-pg-imp rs-destino
           tt-param.data-exec           = today
           tt-param.hora-exec           = time
           tt-param.c-cod-estabel-ini   = input frame f-pg-sel c-cod-estabel-ini 
           tt-param.c-cod-estabel-fim   = input frame f-pg-sel c-cod-estabel-fim 
           tt-param.d-data-emis-ini     = input frame f-pg-sel d-data-emis-ini   
           tt-param.d-data-emis-fim     = input frame f-pg-sel d-data-emis-fim   
           tt-param.c-it-codigo-ini     = input frame f-pg-sel c-it-codigo-ini   
           tt-param.c-it-codigo-fim     = input frame f-pg-sel c-it-codigo-fim   
           tt-param.c-ge-codigo-ini     = input frame f-pg-sel c-ge-codigo-ini   
           tt-param.c-ge-codigo-fim     = input frame f-pg-sel c-ge-codigo-fim   
           tt-param.i-cod-emitente-ini  = input frame f-pg-sel i-cod-emitente-ini
           tt-param.i-cod-emitente-fim  = input frame f-pg-sel i-cod-emitente-fim
           .
    
    /*Alterado 14/02/2005 - tech1007 - Alterado o teste para verificar se a op��o de RTF est� selecionada*/
    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.
    /*Fim alteracao 14/02/2005*/

    /*:T Coloque aqui a/l�gica de grava��o dos demais campos que devem ser passados
       como par�metros para o programa RP.P, atrav�s da temp-table tt-param */

   
    
    /*:T Executar do programa RP.P que ir� criar o relat�rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i esp/escc007rp.p}
    
    {include/i-rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
/*     {include/i-rptrm.i} */
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina w-relat 
PROCEDURE pi-troca-pagina :
/*:T------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P�gina (folder)   
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


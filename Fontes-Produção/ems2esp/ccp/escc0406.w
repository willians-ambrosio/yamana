&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESCC0406 2.00.00.028 } /*** 010028 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i ESCC0406 MCC}
&ENDIF

/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat¢rio                            */
/* Obs: Retirar o valor do preprocessador para as p ginas que nÆo existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA f-pg-cla
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG
&GLOBAL-DEFINE PGIMP f-pg-imp

/* Include Com as Vari veis Globais */
{utp/ut-glob.i}
/*Include com a defini‡Æo dos pr‚-processadores*/
{cdp/cdcfgmat.i}

/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */
/********Include de defini‡Æo da tt-param***********/
{ccp/ESCC0406.i5 new}

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-cla

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES pedido-compr estabelec item-fornec

/* Definitions for FRAME f-pg-sel                                       */
&Scoped-define QUERY-STRING-f-pg-sel FOR EACH mgind.pedido-compr SHARE-LOCK, ~
      EACH mgadm.estabelec WHERE TRUE /* Join to mgind.pedido-compr incomplete */ SHARE-LOCK, ~
      EACH mgind.item-fornec WHERE TRUE /* Join to mgind.pedido-compr incomplete */ SHARE-LOCK
&Scoped-define OPEN-QUERY-f-pg-sel OPEN QUERY f-pg-sel FOR EACH mgind.pedido-compr SHARE-LOCK, ~
      EACH mgadm.estabelec WHERE TRUE /* Join to mgind.pedido-compr incomplete */ SHARE-LOCK, ~
      EACH mgind.item-fornec WHERE TRUE /* Join to mgind.pedido-compr incomplete */ SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-f-pg-sel pedido-compr estabelec item-fornec
&Scoped-define FIRST-TABLE-IN-QUERY-f-pg-sel pedido-compr
&Scoped-define SECOND-TABLE-IN-QUERY-f-pg-sel estabelec
&Scoped-define THIRD-TABLE-IN-QUERY-f-pg-sel item-fornec


/* Standard List Definitions                                            */
&Scoped-Define DISPLAYED-OBJECTS i-classifica 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE i-classifica AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Por Pedido", 1,
"Por Estabelecimento", 2,
"Por Fornecedor", 3,
"Por Comprador", 4
     SIZE 30 BY 4 NO-UNDO.

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

DEFINE VARIABLE text-opcao AS CHARACTER FORMAT "X(23)":U INITIAL "Parƒmetros de ImpressÆo" 
      VIEW-AS TEXT 
     SIZE 25.14 BY .67 NO-UNDO.

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
     SIZE 46.29 BY 1.71.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE tg-param AS LOGICAL INITIAL no 
     LABEL "&Imprimir parƒmetros" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.29 BY .83 NO-UNDO.

DEFINE VARIABLE c-moeda AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 18.29 BY .88 NO-UNDO.

DEFINE VARIABLE i-moeda AS INTEGER FORMAT ">9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE text1 AS CHARACTER FORMAT "X(6)":U INITIAL "Moeda" 
      VIEW-AS TEXT 
     SIZE 9 BY .38 NO-UNDO.

DEFINE VARIABLE tx-importacao AS CHARACTER FORMAT "X(15)":U INITIAL "Importa‡Æo" 
      VIEW-AS TEXT 
     SIZE 11.29 BY .63 NO-UNDO.

DEFINE VARIABLE rs-despesas-pag AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Pagas ao Fornecedor do Material", 1,
"Todas", 2
     SIZE 44.86 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72.72 BY 1.5.

DEFINE RECTANGLE rt-importacao
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72.57 BY 2.5.

DEFINE VARIABLE l-alter AS LOGICAL INITIAL no 
     LABEL "Apenas Pedidos com Ordens Alteradas" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .71 NO-UNDO.

DEFINE VARIABLE l-emergencial AS LOGICAL INITIAL no 
     LABEL "Somente Pedidos Emergenciais" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .71 NO-UNDO.

DEFINE VARIABLE l-end-entrega AS LOGICAL INITIAL no 
     LABEL "Endere‡o Entrega" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .71 NO-UNDO.

DEFINE VARIABLE l-extra-fornecedor AS LOGICAL INITIAL yes 
     LABEL "Pedidos Extra Fornecedor" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .71 NO-UNDO.

DEFINE VARIABLE l-moeda AS LOGICAL INITIAL no 
     LABEL "Utilizar outra Moeda" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .71 NO-UNDO.

DEFINE VARIABLE l-narrativa AS LOGICAL INITIAL yes 
     LABEL "Narrativa" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .71 NO-UNDO.

DEFINE VARIABLE l-receb AS LOGICAL INITIAL yes 
     LABEL "Imprimir Pedidos Recebidos" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .71 NO-UNDO.

DEFINE VARIABLE tg-despesas AS LOGICAL INITIAL no 
     LABEL "Despesas Importa‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 23.86 BY .88 NO-UNDO.

DEFINE VARIABLE tg-despesas-inc AS LOGICAL INITIAL no 
     LABEL "Adiciona Despesas Pre‡o Material" 
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY .88 NO-UNDO.

DEFINE VARIABLE c-comp-f AS CHARACTER FORMAT "X(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88.

DEFINE VARIABLE c-comp-i AS CHARACTER FORMAT "X(12)" 
     LABEL "Comprador":R14 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88.

DEFINE VARIABLE c-estabel-f LIKE estabelec.cod-estabel
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE c-estabel-i LIKE estabelec.cod-estabel
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE da-fimper AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE da-iniper AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Data" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE i-fornec-f AS INTEGER FORMAT ">>>>>>>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88.

DEFINE VARIABLE i-fornec-i AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Fornecedor":R12 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88.

DEFINE VARIABLE i-pedido-f AS INTEGER FORMAT ">>>>>,>>9" INITIAL 99999999 
     VIEW-AS FILL-IN 
     SIZE 11.57 BY .88.

DEFINE VARIABLE i-pedido-i AS INTEGER FORMAT ">>>>>,>>9" INITIAL 0 
     LABEL "Pedido":R8 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-25
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-26
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-27
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-28
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-29
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-30
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-31
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-32
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
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

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY f-pg-sel FOR 
      pedido-compr, 
      estabelec, 
      item-fornec SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Cancelar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     im-pg-par AT ROW 1.5 COL 33.57
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     im-pg-sel AT ROW 1.5 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     im-pg-imp AT ROW 1.5 COL 49.29
     rt-folder AT ROW 2.5 COL 2
     im-pg-cla AT ROW 1.5 COL 17.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-par
     l-receb AT ROW 1 COL 2.86
     l-alter AT ROW 1.92 COL 2.86
     l-emergencial AT ROW 2.79 COL 2.86
     l-moeda AT ROW 3.71 COL 2.86
     l-narrativa AT ROW 4.58 COL 2.86
     l-end-entrega AT ROW 5.5 COL 2.86
     l-extra-fornecedor AT ROW 6.33 COL 2.86
     i-moeda AT ROW 7.58 COL 3.57 COLON-ALIGNED NO-LABEL
     c-moeda AT ROW 7.58 COL 9.57 COLON-ALIGNED NO-LABEL
     tg-despesas AT ROW 9.5 COL 4.72
     rs-despesas-pag AT ROW 9.5 COL 29 NO-LABEL
     tg-despesas-inc AT ROW 10.5 COL 29
     text1 AT ROW 7.08 COL 3.72 COLON-ALIGNED NO-LABEL
     tx-importacao AT ROW 8.75 COL 3.57 COLON-ALIGNED NO-LABEL
     RECT-11 AT ROW 7.25 COL 2.86
     rt-importacao AT ROW 9 COL 2.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76 BY 10.58.

DEFINE FRAME f-pg-sel
     i-pedido-i AT ROW 1.5 COL 24.14 COLON-ALIGNED
     i-pedido-f AT ROW 1.5 COL 48.14 COLON-ALIGNED NO-LABEL
     da-iniper AT ROW 2.5 COL 24 COLON-ALIGNED
     da-fimper AT ROW 2.5 COL 48 COLON-ALIGNED NO-LABEL
     c-estabel-i AT ROW 3.5 COL 24 COLON-ALIGNED HELP
          "C¢digo do estabelecimento"
          LABEL "Estabelecimento"
     c-estabel-f AT ROW 3.5 COL 48 COLON-ALIGNED HELP
          "C¢digo do estabelecimento" NO-LABEL
     i-fornec-i AT ROW 4.5 COL 24.14 COLON-ALIGNED
     i-fornec-f AT ROW 4.5 COL 48.14 COLON-ALIGNED NO-LABEL
     c-comp-i AT ROW 5.5 COL 24 COLON-ALIGNED
     c-comp-f AT ROW 5.5 COL 48 COLON-ALIGNED NO-LABEL
     IMAGE-27 AT ROW 3.5 COL 43
     IMAGE-10 AT ROW 1.5 COL 43
     IMAGE-9 AT ROW 1.5 COL 47
     IMAGE-32 AT ROW 5.5 COL 47
     IMAGE-31 AT ROW 5.5 COL 43
     IMAGE-30 AT ROW 4.5 COL 47
     IMAGE-29 AT ROW 4.5 COL 43
     IMAGE-28 AT ROW 3.5 COL 47
     IMAGE-25 AT ROW 2.5 COL 43
     IMAGE-26 AT ROW 2.5 COL 47
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76 BY 10.

DEFINE FRAME f-pg-cla
     i-classifica AT ROW 1.5 COL 3 HELP
          "Classifica‡Æo para emissÆo do relat¢rio" NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76 BY 10.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.38 COL 3.29 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     bt-arquivo AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     bt-config-impr AT ROW 3.58 COL 43.29 HELP
          "Configura‡Æo da impressora"
     c-arquivo AT ROW 3.63 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 5.75 COL 3 HELP
          "Modo de Execu‡Æo" NO-LABEL
     tg-param AT ROW 7.92 COL 4.72
     text-destino AT ROW 1.63 COL 3.86 NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     text-opcao AT ROW 7.13 COL 1.43 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
     RECT-12 AT ROW 7.46 COL 2.29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76 BY 10.54.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Rela‡Æo de Pedidos Emitidos"
         HEIGHT             = 15
         WIDTH              = 81.14
         MAX-HEIGHT         = 28.08
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.08
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-relat.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-pg-cla
   FRAME-NAME                                                           */
/* SETTINGS FOR RADIO-SET i-classifica IN FRAME f-pg-cla
   NO-ENABLE                                                            */
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
                "Execu‡Æo".

/* SETTINGS FOR TOGGLE-BOX tg-param IN FRAME f-pg-imp
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR FILL-IN c-moeda IN FRAME f-pg-par
   NO-ENABLE                                                            */
ASSIGN 
       rs-despesas-pag:HIDDEN IN FRAME f-pg-par           = TRUE.

ASSIGN 
       rt-importacao:HIDDEN IN FRAME f-pg-par           = TRUE.

/* SETTINGS FOR FILL-IN text1 IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text1:PRIVATE-DATA IN FRAME f-pg-par     = 
                "Moeda".

ASSIGN 
       tg-despesas:HIDDEN IN FRAME f-pg-par           = TRUE.

ASSIGN 
       tg-despesas-inc:HIDDEN IN FRAME f-pg-par           = TRUE.

/* SETTINGS FOR FILL-IN tx-importacao IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tx-importacao:HIDDEN IN FRAME f-pg-par           = TRUE
       tx-importacao:PRIVATE-DATA IN FRAME f-pg-par     = 
                "Importa‡Æo".

/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FILL-IN c-estabel-f IN FRAME f-pg-sel
   LIKE = mgadm.estabelec.cod-estabel EXP-LABEL EXP-SIZE                */
/* SETTINGS FOR FILL-IN c-estabel-i IN FRAME f-pg-sel
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

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
     _TblList          = "mgind.pedido-compr,mgadm.estabelec WHERE mgind.pedido-compr ...,mgind.item-fornec WHERE mgind.pedido-compr ..."
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Rela‡Æo de Pedidos Emitidos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Rela‡Æo de Pedidos Emitidos */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda C-Win
ON CHOOSE OF bt-ajuda IN FRAME f-relat /* Ajuda */
DO:
   {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo C-Win
ON CHOOSE OF bt-arquivo IN FRAME f-pg-imp
DO:
    {include/i-rparq.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar C-Win
ON CHOOSE OF bt-cancelar IN FRAME f-relat /* Cancelar */
DO:
   apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-config-impr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-config-impr C-Win
ON CHOOSE OF bt-config-impr IN FRAME f-pg-imp
DO:
   {include/i-rpimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar C-Win
ON CHOOSE OF bt-executar IN FRAME f-relat /* Executar */
DO:
   do  on error undo, return no-apply:
       run pi-executar.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME i-moeda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-moeda C-Win
ON F5 OF i-moeda IN FRAME f-pg-par
DO:
    {include/zoomvar.i &prog-zoom="adzoom/z01ad178.w"
                       &campo=i-moeda
                       &campozoom=mo-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-moeda C-Win
ON LEAVE OF i-moeda IN FRAME f-pg-par
DO:
    {include/leave.i &tabela=moeda
                     &atributo-ref=descricao
                     &variavel-ref=c-moeda
                     &where="moeda.mo-codigo = input frame f-pg-par i-moeda"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-moeda C-Win
ON MOUSE-SELECT-DBLCLICK OF i-moeda IN FRAME f-pg-par
DO:
    apply 'F5' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME im-pg-cla
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-cla C-Win
ON MOUSE-SELECT-CLICK OF im-pg-cla IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-imp C-Win
ON MOUSE-SELECT-CLICK OF im-pg-imp IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-par
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-par C-Win
ON MOUSE-SELECT-CLICK OF im-pg-par IN FRAME f-relat
DO:
    run pi-troca-pagina.
    apply "value-changed" to l-moeda in frame f-pg-par.

    FOR FIRST moeda where moeda.mo-codigo = input frame f-pg-par i-moeda NO-LOCK: END.
    assign c-moeda = moeda.descricao.
    disp c-moeda with frame f-pg-par.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-sel C-Win
ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME l-moeda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-moeda C-Win
ON VALUE-CHANGED OF l-moeda IN FRAME f-pg-par /* Utilizar outra Moeda */
DO:
    assign i-moeda:sensitive = l-moeda:checked.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME rs-destino
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-destino C-Win
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-execucao C-Win
ON VALUE-CHANGED OF rs-execucao IN FRAME f-pg-imp
DO:
   {include/i-rprse.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME tg-despesas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-despesas C-Win
ON VALUE-CHANGED OF tg-despesas IN FRAME f-pg-par /* Despesas Importa‡Æo */
DO:
    if (tg-despesas:checked in frame f-pg-par) then do:
        enable rs-despesas-pag
               tg-despesas-inc
               with frame f-pg-par.
    end.
    else
        disable rs-despesas-pag
                tg-despesas-inc
                with frame f-pg-par.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-cla
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESCC0406" "2.00.00.026"}

/* inicializa‡äes do template de relat¢rio */
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

    if  i-moeda:load-mouse-pointer ("image~\lupa.cur") in frame f-pg-par then.    

    find first param-global no-lock no-error.
    if avail param-global and param-global.modulo-07 then
        assign tg-despesas:sensitive in frame f-pg-par     = yes
               rs-despesas-pag:sensitive in frame f-pg-par = no
               tg-despesas-inc:sensitive in frame f-pg-par = no
               tx-importacao:hidden in frame f-pg-par      = no
               rt-importacao:hidden in frame f-pg-par      = no
               tg-despesas:hidden in frame f-pg-par        = no
               rs-despesas-pag:hidden in frame f-pg-par    = no
               tg-despesas-inc:hidden in frame f-pg-par    = no.
    else
        assign tx-importacao:hidden in frame f-pg-par      = yes
               rt-importacao:hidden in frame f-pg-par      = yes
               tg-despesas:hidden in frame f-pg-par        = yes
               rs-despesas-pag:hidden in frame f-pg-par    = yes
               tg-despesas-inc:hidden in frame f-pg-par    = yes.
    
    &IF "{&mguni_version}" >= "2.071" &THEN
        ASSIGN c-estabel-f:SCREEN-VALUE IN FRAME f-pg-sel = "ZZZZZ".
    &ELSE
        ASSIGN c-estabel-f:SCREEN-VALUE IN FRAME f-pg-sel = "ZZZ".
    
    &ENDIF

    IF  NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

find first param-global no-lock no-error.
if avail param-global and param-global.modulo-07 then
    assign tg-despesas:sensitive in frame f-pg-par     = yes
           rs-despesas-pag:sensitive in frame f-pg-par = no
           tg-despesas-inc:sensitive in frame f-pg-par = no
           tx-importacao:hidden in frame f-pg-par      = no
           rt-importacao:hidden in frame f-pg-par      = no
           tg-despesas:hidden in frame f-pg-par        = no
           rs-despesas-pag:hidden in frame f-pg-par    = no
           tg-despesas-inc:hidden in frame f-pg-par    = no.
else
    assign tx-importacao:hidden in frame f-pg-par      = yes
           rt-importacao:hidden in frame f-pg-par      = yes
           tg-despesas:hidden in frame f-pg-par        = yes
           rs-despesas-pag:hidden in frame f-pg-par    = yes
           tg-despesas-inc:hidden in frame f-pg-par    = yes.

&IF "{&mguni_version}" >= "2.071" &THEN
    ASSIGN c-estabel-f:SCREEN-VALUE IN FRAME f-pg-sel = "ZZZZZ".
&ELSE
    ASSIGN c-estabel-f:SCREEN-VALUE IN FRAME f-pg-sel = "ZZZ".

&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects C-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available C-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  ENABLE im-pg-par im-pg-sel im-pg-imp im-pg-cla bt-executar bt-cancelar 
         bt-ajuda 
      WITH FRAME f-relat IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY i-classifica 
      WITH FRAME f-pg-cla IN WINDOW C-Win.
  VIEW FRAME f-pg-cla IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-cla}
  DISPLAY rs-destino c-arquivo rs-execucao tg-param text-opcao 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  ENABLE RECT-7 RECT-9 RECT-12 rs-destino bt-arquivo bt-config-impr c-arquivo 
         rs-execucao text-opcao 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY l-receb l-alter l-emergencial l-moeda l-narrativa l-end-entrega 
          l-extra-fornecedor i-moeda c-moeda tg-despesas rs-despesas-pag 
          tg-despesas-inc 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  ENABLE RECT-11 rt-importacao l-receb l-alter l-emergencial l-moeda 
         l-narrativa l-end-entrega l-extra-fornecedor i-moeda tg-despesas 
         rs-despesas-pag tg-despesas-inc 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY i-pedido-i i-pedido-f da-iniper da-fimper c-estabel-i c-estabel-f 
          i-fornec-i i-fornec-f c-comp-i c-comp-f 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE IMAGE-27 IMAGE-10 IMAGE-9 IMAGE-32 IMAGE-31 IMAGE-30 IMAGE-29 IMAGE-28 
         IMAGE-25 IMAGE-26 i-pedido-i i-pedido-f da-iniper da-fimper 
         c-estabel-i c-estabel-f i-fornec-i i-fornec-f c-comp-i c-comp-f 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit C-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar C-Win 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

do  on error undo, return error
    on stop  undo, return error:     

    {include/i-rpexa.i}

    if  input frame f-pg-imp rs-destino = 2 then do:
        run utp/ut-vlarq.p (input input frame f-pg-imp c-arquivo).
        if  return-value = "nok" then do:
            run utp/ut-msgs.p (input "show", input 73, input "").
            apply 'mouse-select-click' to im-pg-imp in frame f-relat.
            apply 'entry' to c-arquivo in frame f-pg-imp.
            return error.
        end.
    end.

    /* Coloque aqui as valida‡äes das outras p ginas, lembrando que elas
       devem apresentar uma mensagem de erro cadastrada, posicionar na p gina 
       com problemas e colocar o focus no campo com problemas             */    

    if  i-moeda:sensitive in frame f-pg-par = yes then do:
        find moeda
            where moeda.mo-codigo = input frame f-pg-par i-moeda
            no-lock no-error.
        if  not avail moeda then do:
            {utp/ut-table.i mgadm moeda 2}
            /* Inicio -- Projeto Internacional */
            {utp/ut-liter.i "Moeda" *}
            run utp/ut-msgs.p (input "show", input 2, input RETURN-VALUE).
            apply "entry" to i-moeda in frame f-pg-par.
            return 'adm-error'.
        end.
    end.

    create tt-param.
    assign tt-param.usuario       = c-seg-usuario
           tt-param.destino       = input frame f-pg-imp rs-destino
           tt-param.data-exec     = today
           tt-param.hora-exec     = time
           tt-param.i-classifica  = input frame f-pg-cla i-classifica
           tt-param.i-pedido-i    = input frame f-pg-sel i-pedido-i
           tt-param.i-pedido-f    = input frame f-pg-sel i-pedido-f
           tt-param.da-iniper     = input frame f-pg-sel da-iniper
           tt-param.da-fimper     = input frame f-pg-sel da-fimper
           tt-param.c-estabel-i   = input frame f-pg-sel c-estabel-i
           tt-param.c-estabel-f   = input frame f-pg-sel c-estabel-f
           tt-param.i-fornec-i    = input frame f-pg-sel i-fornec-i
           tt-param.i-fornec-f    = input frame f-pg-sel i-fornec-f
           tt-param.c-comp-i      = input frame f-pg-sel c-comp-i
           tt-param.c-comp-f      = input frame f-pg-sel c-comp-f
           tt-param.l-receb       = input frame f-pg-par l-receb
           tt-param.l-alter       = input frame f-pg-par l-alter
           tt-param.l-emergencial = input frame f-pg-par l-emergencial
           tt-param.l-moeda       = input frame f-pg-par l-moeda
           tt-param.l-narrativa   = input frame f-pg-par l-narrativa
           tt-param.l-end-entrega = input frame f-pg-par l-end-entrega
           tt-param.l-extra-fornecedor  = input frame f-pg-par l-extra-fornecedor
           tt-param.i-moeda       = input frame f-pg-par i-moeda
           tt-param.c-moeda       = input frame f-pg-par c-moeda
           tt-param.c-classe      = entry((tt-param.i-classifica - 1) * 2 + 1,
                                          i-classifica:radio-buttons in frame f-pg-cla)
           tt-param.c-destino       = entry((tt-param.destino - 1) * 2 + 1,
                                            rs-destino:radio-buttons in frame f-pg-imp)
           tt-param.l-despesas      = input frame f-pg-par tg-despesas
           tt-param.i-despesas-pag  = input frame f-pg-par rs-despesas-pag
           tt-param.l-despesas-inc  = input frame f-pg-par tg-despesas-inc
           tt-param.l-imprime-param = input frame f-pg-imp tg-param.

    if  tt-param.destino = 1 then
        assign tt-param.arquivo = "".
    else
    if  tt-param.destino = 2 then 
        assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
    else
        assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".

    /* Coloque aqui a l¢gica de grava‡Æo dos parƒmtros e sele‡Æo na temp-table
       tt-param */ 

    {include/i-rpexb.i}

    if  session:set-wait-state("general") then.    

    {include/i-rprun.i ccp/ESCC0406rp.p}

    {include/i-rpexc.i}

    if  session:set-wait-state("") then.

    /*{include/i-rptrm.i}*/

end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina C-Win 
PROCEDURE pi-troca-pagina :
/*------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P gina (folder)   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{include/i-rptrp.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records C-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "pedido-compr"}
  {src/adm/template/snd-list.i "estabelec"}
  {src/adm/template/snd-list.i "item-fornec"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed C-Win 
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


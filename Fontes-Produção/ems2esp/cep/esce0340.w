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
def buffer localizacao for ems2cademp.localizacao.

{include/i-prgvrs.i ESCE0340 2.00.00.000}  /*** 010007 ***/

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

{cep/ce0340.i} /*Definiá∆o da tt-param*/

define buffer b-necessidade-oc for necessidade-oc.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.

define temp-table tt-digita
    field cod-estabel like estabelec.cod-estabel
    field cod-depos   like deposito.cod-depos
    field descricao   like deposito.nome 
    field cons-saldo  like deposito.cons-saldo
    field lista         as logical FORMAT "*/ " LABEL "X"
    index id is primary cod-estabel.

define buffer b-tt-digita for tt-digita.

/* Local Variable Definitions ---                                       */

def var l-ok             as logical                    no-undo.
def var c-arq-digita     as char                       no-undo.
def var c-terminal       as char                       no-undo.
def var c-tit-depos      as char                       no-undo.
def var i-per-corrente   as integer                    no-undo.
def var i-ano-corrente   as integer                    no-undo.
def var da-iniper-fech   like param-estoq.ult-fech-dia no-undo.
def var da-fimper-fech   like param-estoq.ult-fech-dia no-undo.
def var h-acomp          as handle                     no-undo.
def var c-desc-sldaloc   as char                       no-undo.
def var c-desc-contacont as char                       no-undo.
def var c-desc-consprev  as char                       no-undo.
def var c-desc-elimina   as char                       no-undo.

DEF VAR l-todos AS LOGICAL INIT YES NO-UNDO.
DEF VAR c-lb-todos AS CHAR FORMAT "X(6)" NO-UNDO.
DEF VAR da-iniper-x        AS DATE FORMAT 99/99/9999      NO-UNDO.
DEF VAR da-fimper-x        AS DATE FORMAT 99/99/9999      NO-UNDO.

{utp/ut-liter.i Todos}
ASSIGN c-lb-todos = TRIM(RETURN-VALUE).

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
&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.lista tt-digita.cod-estabel tt-digita.cod-depos tt-digita.descricao tt-digita.cons-saldo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-digita.cod-estabel ~
tt-digita.cod-depos   
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
&Scoped-Define ENABLED-OBJECTS br-digita 

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
     SIZE 11 BY 1.

DEFINE BUTTON bt-carrega 
     LABEL "Carrega" 
     SIZE 9 BY 1.

DEFINE BUTTON bt-inserir 
     LABEL "Inserir" 
     SIZE 11 BY 1.

DEFINE BUTTON bt-recuperar 
     LABEL "Recuperar" 
     SIZE 11 BY 1.

DEFINE BUTTON bt-retirar 
     LABEL "Retirar" 
     SIZE 11 BY 1.

DEFINE BUTTON bt-salvar 
     LABEL "Salvar" 
     SIZE 11 BY 1.

DEFINE BUTTON bt-todos 
     LABEL "Todos" 
     SIZE 9 BY 1.

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
     SIZE 27.72 BY .92 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Considera Itens" 
      VIEW-AS TEXT 
     SIZE 15.14 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Elimina Necessidades" 
      VIEW-AS TEXT 
     SIZE 21.72 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Saldo Estoque" 
      VIEW-AS TEXT 
     SIZE 14 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-4 AS CHARACTER FORMAT "X(256)":U INITIAL "Ordens de Compra" 
      VIEW-AS TEXT 
     SIZE 18 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-5 AS CHARACTER FORMAT "X(256)":U INITIAL "Consumo Previsto Zero" 
      VIEW-AS TEXT 
     SIZE 23 BY .67 NO-UNDO.

DEFINE VARIABLE i-cons-prev AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N∆o Gerar", 1,
"Gerar", 2
     SIZE 34 BY .75 NO-UNDO.

DEFINE VARIABLE i-conta-cont AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Sem Conta Cont†bil", 1,
"Todas", 2
     SIZE 34 BY .75 NO-UNDO.

DEFINE VARIABLE i-elimina AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Todas", 1,
"Somente Selecionadas", 2
     SIZE 25.86 BY 1.67 NO-UNDO.

DEFINE VARIABLE i-sld-aloc AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Considera Alocaá‰es", 1,
"Exceto as Alocaá‰es", 2
     SIZE 53 BY .75 NO-UNDO.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 2.5.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 36.86 BY 2.5.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 7.21.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 73 BY 2.25.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 73 BY 1.5.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 36 BY 1.5.

DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 36 BY 1.5.

DEFINE VARIABLE l-depositos AS LOGICAL INITIAL no 
     LABEL "Informa dep¢sitos" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.57 BY .75 NO-UNDO.

DEFINE VARIABLE l-periodico AS LOGICAL INITIAL no 
     LABEL "Ressuprimento Peri¢dico" 
     VIEW-AS TOGGLE-BOX
     SIZE 27.86 BY .83 NO-UNDO.

DEFINE VARIABLE l-pto-enc AS LOGICAL INITIAL no 
     LABEL "Ressuprimento Ponto Encomenda" 
     VIEW-AS TOGGLE-BOX
     SIZE 34.57 BY .83 NO-UNDO.

DEFINE VARIABLE l-req-est AS LOGICAL INITIAL no 
     LABEL "Requisiá‰es de Estoque" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .83 NO-UNDO.

DEFINE VARIABLE l-sol-comp AS LOGICAL INITIAL no 
     LABEL "Solicitaá‰es de Compra" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .83 NO-UNDO.

DEFINE VARIABLE tg-considera AS LOGICAL INITIAL no 
     LABEL "Considera (XTIVITY)" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .83 NO-UNDO.

DEFINE VARIABLE c-depos-fim AS CHARACTER FORMAT "X(3)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-depos-ini AS CHARACTER FORMAT "X(3)":U 
     LABEL "Dep¢sito" 
     VIEW-AS FILL-IN 
     SIZE 5.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-estabelec-fim AS CHARACTER FORMAT "X(3)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE c-estabelec-ini AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

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

DEFINE VARIABLE i-ge-fim AS INTEGER FORMAT "99":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE i-ge-ini AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Grupo Estoque" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

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

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 76.43 BY 10.54.

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
      tt-digita.lista
      tt-digita.cod-estabel
      tt-digita.cod-depos
      tt-digita.descricao 
      tt-digita.cons-saldo
ENABLE
      tt-digita.cod-estabel
      tt-digita.cod-depos
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 73.72 BY 9
         BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder AT ROW 2.5 COL 2
     RECT-1 AT ROW 14.29 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     im-pg-dig AT ROW 1.5 COL 33.72
     im-pg-imp AT ROW 1.46 COL 49
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-sel AT ROW 1.5 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-sel
     c-estabelec-ini AT ROW 4.08 COL 17.14 COLON-ALIGNED
     c-estabelec-fim AT ROW 4.08 COL 47.72 COLON-ALIGNED NO-LABEL
     i-ge-ini AT ROW 5.08 COL 17.14 COLON-ALIGNED
     i-ge-fim AT ROW 5.08 COL 47.72 COLON-ALIGNED NO-LABEL
     c-familia-ini AT ROW 6.08 COL 17.14 COLON-ALIGNED
     c-familia-fim AT ROW 6.08 COL 47.72 COLON-ALIGNED NO-LABEL
     c-item-ini AT ROW 7.08 COL 17.14 COLON-ALIGNED
     c-item-fim AT ROW 7.08 COL 47.72 COLON-ALIGNED NO-LABEL
     c-depos-ini AT ROW 8.08 COL 17.14 COLON-ALIGNED
     c-depos-fim AT ROW 8.17 COL 47.72 COLON-ALIGNED NO-LABEL
     IMAGE-10 AT ROW 5.08 COL 47
     IMAGE-11 AT ROW 4.08 COL 47
     IMAGE-12 AT ROW 8.13 COL 47
     IMAGE-13 AT ROW 8.13 COL 40.86
     IMAGE-2 AT ROW 7.08 COL 47
     IMAGE-3 AT ROW 7.08 COL 40.86
     IMAGE-4 AT ROW 6.08 COL 40.86
     IMAGE-6 AT ROW 5.08 COL 40.86
     IMAGE-7 AT ROW 4.08 COL 40.86
     IMAGE-9 AT ROW 6.08 COL 47
     RECT-10 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.38 COL 3.29 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     bt-arquivo AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     bt-config-impr AT ROW 3.58 COL 43.29 HELP
          "Configuraá∆o da impressora"
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
         AT COL 3 ROW 3
         SIZE 73.72 BY 10.

DEFINE FRAME f-pg-dig
     br-digita AT ROW 1 COL 1
     bt-inserir AT ROW 10 COL 1
     bt-alterar AT ROW 10 COL 12
     bt-retirar AT ROW 10 COL 23
     bt-salvar AT ROW 10 COL 34
     bt-recuperar AT ROW 10 COL 45
     bt-carrega AT ROW 10 COL 57
     bt-todos AT ROW 10 COL 66
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.31
         SIZE 76.86 BY 10.15.

DEFINE FRAME f-pg-par
     tg-considera AT ROW 6 COL 41 WIDGET-ID 2
     l-pto-enc AT ROW 2.08 COL 3.57
     l-periodico AT ROW 3.08 COL 3.57
     i-elimina AT ROW 2.08 COL 43 NO-LABEL
     l-sol-comp AT ROW 5 COL 4
     l-req-est AT ROW 6 COL 4
     l-depositos AT ROW 5 COL 41
     i-sld-aloc AT ROW 8 COL 4 NO-LABEL
     i-conta-cont AT ROW 10 COL 4 NO-LABEL
     i-cons-prev AT ROW 10 COL 41 NO-LABEL
     FILL-IN-1 AT ROW 1.17 COL 2.86 COLON-ALIGNED NO-LABEL
     FILL-IN-2 AT ROW 1.17 COL 41.57 COLON-ALIGNED NO-LABEL
     FILL-IN-3 AT ROW 7.25 COL 3 COLON-ALIGNED NO-LABEL
     FILL-IN-4 AT ROW 9.25 COL 3 COLON-ALIGNED NO-LABEL
     FILL-IN-5 AT ROW 9.25 COL 40 COLON-ALIGNED NO-LABEL
     RECT-12 AT ROW 1.5 COL 2
     RECT-13 AT ROW 1.5 COL 40
     RECT-26 AT ROW 4.29 COL 2
     RECT-27 AT ROW 4.75 COL 3
     RECT-28 AT ROW 7.5 COL 3
     RECT-29 AT ROW 9.5 COL 3
     RECT-30 AT ROW 9.5 COL 40
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.96
         SIZE 76.72 BY 10.5.


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
         TITLE              = "Ressuprimento Estoque"
         HEIGHT             = 15
         WIDTH              = 81.29
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
/* SETTINGS FOR BUTTON bt-carrega IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-inserir IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-recuperar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-retirar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-salvar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-todos IN FRAME f-pg-dig
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
                "Execuá∆o".

/* SETTINGS FOR FRAME f-pg-par
   Custom                                                               */
/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-2 IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-3 IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-4 IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-5 IN FRAME f-pg-par
   NO-ENABLE                                                            */
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
ON END-ERROR OF w-relat /* Ressuprimento Estoque */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Ressuprimento Estoque */
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
   apply 'choose' to bt-retirar in frame f-pg-dig.
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
        display tt-digita.cod-estabel
                tt-digita.cod-depos with browse br-digita. 
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
ON INS OF br-digita IN FRAME f-pg-dig
DO:
   apply 'choose' to bt-inserir in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON MOUSE-SELECT-DBLCLICK OF br-digita IN FRAME f-pg-dig
DO:
  
if avail tt-digita then do:
    if  tt-digita.lista THEN
        assign tt-digita.lista = NO.
    else
        assign tt-digita.lista = YES.
    disp tt-digita.lista with browse br-digita.
end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON OFF-END OF br-digita IN FRAME f-pg-dig
DO:
   apply 'entry' to bt-inserir in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON OFF-HOME OF br-digita IN FRAME f-pg-dig
DO:
  apply 'entry' to bt-recuperar in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ROW-ENTRY OF br-digita IN FRAME f-pg-dig
DO:
    /* trigger para inicializar campos da temp table de digitaá∆o */
    if  br-digita:new-row in frame f-pg-dig then do:
        assign tt-digita.cod-estabel:screen-value  in browse br-digita = ""
               tt-digita.cod-depos:screen-value    in browse br-digita = "".
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ROW-LEAVE OF br-digita IN FRAME f-pg-dig
DO:
    /* ê aqui que a gravaá∆o da linha da temp-table Ç efetivada.
       PorÇm as validaá‰es dos registros devem ser feitas na procedure pi-executar,
       no local indicado pelo coment†rio */
    
    if br-digita:NEW-ROW in frame f-pg-dig then 
    do transaction on error undo, return no-apply:

        /* Teste da chave duplicada na criaá∆o de registros */
        if  can-find(b-tt-digita
               where b-tt-digita.cod-estabel = input browse br-digita tt-digita.cod-estabel
               and   b-tt-digita.cod-depos   = input browse br-digita tt-digita.cod-depos) then do:
           run utp/ut-msgs.p ("show", 108, "").
           return no-apply.
        end.
        
        /* outras validaá‰es */
        IF input browse br-digita tt-digita.cod-estabel <> "" THEN DO:
            if  not can-find(estabelec
               where estabelec.cod-estabel = input browse br-digita tt-digita.cod-estabel) then do:
               run utp/ut-msgs.p ("show", 13, input browse br-digita tt-digita.cod-estabel).
               return no-apply.
            end.
        END.
        ELSE
            RETURN NO-APPLY.
        IF input browse br-digita tt-digita.cod-depos <> "" THEN DO:
            FIND deposito
               where deposito.cod-depos = input browse br-digita tt-digita.cod-depos NO-LOCK NO-ERROR.
            IF NOT AVAIL deposito then do:
               run utp/ut-msgs.p ("show", 530, "").
               return no-apply.
            end.
        END.
        ELSE
            RETURN NO-APPLY.

        create tt-digita.
        assign input browse br-digita tt-digita.cod-estabel
               input browse br-digita tt-digita.cod-depos
               tt-digita.descricao   = deposito.nome
               tt-digita.cons-saldo  = deposito.cons-saldo
               tt-digita.lista       = YES.  
        DISPLAY tt-digita.descricao
                tt-digita.cons-saldo
                tt-digita.lista WITH BROWSE br-digita.
        br-digita:CREATE-RESULT-LIST-ENTRY() in frame f-pg-dig.
    end.
    else do transaction on error undo, return no-apply:
        
        /* Teste da chave duplicada na alteraá∆o de registros */
        if  can-find(b-tt-digita
            where b-tt-digita.cod-estabel = tt-digita.cod-estabel:screen-value in browse br-digita
            and   b-tt-digita.cod-depos   = tt-digita.cod-depos:screen-value   in browse br-digita
            and   recid(b-tt-digita) <> recid(tt-digita)) then do:
            run utp/ut-msgs.p ("show", 108, input "").
            return no-apply.
        end.

        /* outras validaá‰es */
        IF input browse br-digita tt-digita.cod-estabel <> "" THEN DO:
            if  not can-find(estabelec
               where estabelec.cod-estabel = input browse br-digita tt-digita.cod-estabel) then do:
               run utp/ut-msgs.p ("show", 13, input browse br-digita tt-digita.cod-estabel).
               return no-apply.
            end.
        END.
        ELSE
            RETURN NO-APPLY.
        IF input browse br-digita tt-digita.cod-depos <> "" THEN DO:
            FIND deposito
               where deposito.cod-depos = input browse br-digita tt-digita.cod-depos NO-LOCK NO-ERROR.
            IF NOT AVAIL deposito then do:
               run utp/ut-msgs.p ("show", 530, "").
               return no-apply.
            end.
        END.
        ELSE
            RETURN NO-APPLY.

        assign input browse br-digita tt-digita.cod-estabel
               input browse br-digita tt-digita.cod-depos
               tt-digita.descricao   = deposito.nome
               tt-digita.cons-saldo  = deposito.cons-saldo
               tt-digita.lista       = YES.  
        DISPLAY tt-digita.descricao
                tt-digita.cons-saldo
                tt-digita.lista WITH BROWSE br-digita.
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
   if  avail tt-digita then
       apply 'entry' to tt-digita.cod-estabel in browse br-digita. 
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


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-carrega
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-carrega w-relat
ON CHOOSE OF bt-carrega IN FRAME f-pg-dig /* Carrega */
DO:
    
    RUN pi-carrega-depos.

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
    
    if  num-results('br-digita') > 0 then do:
        if  br-digita:insert-row('after') in frame f-pg-dig then.
    end.
    else do transaction:
        create tt-digita.
        open query br-digita
             for each tt-digita.
        apply 'entry' to tt-digita.cod-estabel in browse br-digita. 
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
    
    if num-results("br-digita") = 0 then
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


&Scoped-define SELF-NAME bt-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos w-relat
ON CHOOSE OF bt-todos IN FRAME f-pg-dig /* Todos */
DO:


for each tt-digita:
    assign tt-digita.lista = IF l-todos THEN YES ELSE NO.
end.

{utp/ut-liter.i Nenhum}
ASSIGN l-todos = (l-todos = NO)
       bt-todos:LABEL IN FRAME f-pg-dig = IF l-todos THEN c-lb-todos ELSE TRIM(RETURN-VALUE).
open query br-digita for each tt-digita.

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


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME l-depositos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-depositos w-relat
ON VALUE-CHANGED OF l-depositos IN FRAME f-pg-par /* Informa dep¢sitos */
DO:
    
    DO WITH FRAME f-pg-dig:    
    assign br-digita:sensitive     = (if l-depositos:checked then yes else no)
           bt-inserir:sensitive    = (if l-depositos:checked then yes else no)
           bt-recuperar:sensitive  = (if l-depositos:checked then yes else no)
           bt-alterar:sensitive    = (if l-depositos:checked then yes else no)
           bt-retirar:sensitive    = (if l-depositos:checked then yes else no)
           bt-salvar:sensitive     = (if l-depositos:checked then yes else no)
           bt-todos:sensitive     = (if l-depositos:checked then yes else no)
           bt-carrega:sensitive     = (if l-depositos:checked then yes else no).
    END.
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

on  "F5", mouse-select-dblclick of tt-digita.cod-estabel in browse br-digita do:
    {include/zoomvar.i &prog-zoom="adzoom/z01ad107.w"
                       &campo=tt-digita.cod-estabel
                       &campozoom=cod-estabel
                       &browse=br-digita}
end.
on  "F5", mouse-select-dblclick of tt-digita.cod-depos in browse br-digita do:
    {include/zoomvar.i &prog-zoom="inzoom/z01in084.w"
                     &campo=tt-digita.cod-depos
                     &campozoom=cod-depos
                     &browse=br-digita}
end.

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESCE0340" "2.00.00.007"}

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
    
    /*find first param-global no-lock no-error.*/
    find first param-estoq no-lock no-error.
    if not avail param-estoq then do:
       run utp/ut-msgs.p (input "show",
                          input 1059,
                          input "").
       apply "close" to this-procedure.
    end.
    else                      
        run cdp/cdapi005.p (input  param-estoq.ult-per-fech,
                            output da-iniper-x,
                            output da-fimper-x,
                            output i-per-corrente,
                            output i-ano-corrente,
                            output da-iniper-fech,
                            output da-fimper-fech).
    /*assign da-data-ini:screen-value in frame f-pg-sel = string(da-iniper-x)
           da-data-fim:screen-value in frame f-pg-sel = string(da-fimper-x)
           bt-inserir:sensitive in frame f-pg-dig = no.*/
    
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
  ENABLE im-pg-dig im-pg-imp im-pg-par im-pg-sel bt-executar bt-cancelar 
         bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY c-estabelec-ini c-estabelec-fim i-ge-ini i-ge-fim c-familia-ini 
          c-familia-fim c-item-ini c-item-fim c-depos-ini c-depos-fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-10 IMAGE-11 IMAGE-12 IMAGE-13 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-6 
         IMAGE-7 IMAGE-9 RECT-10 c-estabelec-ini c-estabelec-fim i-ge-ini 
         i-ge-fim c-familia-ini c-familia-fim c-item-ini c-item-fim c-depos-ini 
         c-depos-fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY tg-considera l-pto-enc l-periodico i-elimina l-sol-comp l-req-est 
          l-depositos i-sld-aloc i-conta-cont i-cons-prev FILL-IN-1 FILL-IN-2 
          FILL-IN-3 FILL-IN-4 FILL-IN-5 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE tg-considera l-pto-enc l-periodico i-elimina l-sol-comp l-req-est 
         l-depositos i-sld-aloc i-conta-cont i-cons-prev RECT-12 RECT-13 
         RECT-26 RECT-27 RECT-28 RECT-29 RECT-30 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 rs-destino bt-arquivo bt-config-impr c-arquivo 
         rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  ENABLE br-digita 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-depos w-relat 
PROCEDURE pi-carrega-depos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH tt-digita:
    DELETE tt-digita.
END.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Carregando_Dep¢sitos * L}
run pi-inicializar in h-acomp (input return-value).
FOR EACH localizacao field (cod-depos cod-estabel) 
    WHERE localizacao.cod-estabel >= INPUT FRAME f-pg-sel c-estabelec-ini  
    AND   localizacao.cod-estabel <= INPUT FRAME f-pg-sel c-estabelec-fim 
    AND   localizacao.cod-depos   >= INPUT FRAME f-pg-sel c-depos-ini  
    AND   localizacao.cod-depos   <= INPUT FRAME f-pg-sel c-depos-fim  
    NO-LOCK:
    IF  NOT CAN-FIND (FIRST tt-digita WHERE 
                            tt-digita.cod-depos   = localizacao.cod-depos AND
                            tt-digita.cod-estabel = localizacao.cod-estabel) 
    THEN DO:
        {utp/ut-table.i mgind deposito 1}
        run pi-acompanhar in h-acomp (input trim(return-value) + ": " + localizacao.cod-depos).
        IF  NOT AVAIL deposito THEN
            FOR FIRST deposito FIELD (cod-depos nome cons-saldo) WHERE
                deposito.cod-depos = localizacao.cod-depos NO-LOCK: END.
        ELSE
        IF  AVAIL deposito AND deposito.cod-depos <> localizacao.cod-depos THEN
            FOR FIRST deposito FIELD (cod-depos nome cons-saldo) WHERE
                deposito.cod-depos = localizacao.cod-depos NO-LOCK: END.
        IF  AVAIL deposito THEN DO:
            create tt-digita.
            assign tt-digita.cod-depos   = deposito.cod-depos
                   tt-digita.cod-estabel = localizacao.cod-estabel
                   tt-digita.descricao   = deposito.nome
                   tt-digita.cons-saldo  = deposito.cons-saldo
                   tt-digita.lista       = deposito.cons-saldo.                    
        END.
    END.
END.
open query br-digita for each tt-digita.

run pi-finalizar in h-acomp.

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
    
    {utp/ut-liter.i Dep¢sito * r} 
    assign c-tit-depos = trim(return-value).    
    if input frame f-pg-par l-depositos then do:
       find first tt-digita no-error.
       if not avail tt-digita then do:
          run utp/ut-msgs.p ("show", 688, c-tit-depos).
          return no-apply.
       end.
    end.   

    assign c-desc-sldaloc   = entry((input frame f-pg-par i-sld-aloc - 1) * 2 + 1,
                              i-sld-aloc:radio-buttons in frame f-pg-par)
           c-desc-contacont = entry((input frame f-pg-par i-conta-cont - 1) * 2 + 1,
                              i-conta-cont:radio-buttons in frame f-pg-par)
           c-desc-consprev  = entry((input frame f-pg-par i-cons-prev - 1) * 2 + 1,
                              i-cons-prev:radio-buttons in frame f-pg-par)
           c-desc-elimina   = entry((input frame f-pg-par i-elimina - 1) * 2 + 1,
                              i-elimina:radio-buttons in frame f-pg-par).
        
    /* Aqui s∆o gravados os campos da temp-table que ser† passada como parÉmetro
       para o programa RP.P */
    
    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame f-pg-imp rs-destino
           tt-param.data-exec       = today
           tt-param.hora-exec       = time
           tt-param.estabelec-ini   = input frame f-pg-sel c-estabelec-ini
           tt-param.estabelec-fim   = input frame f-pg-sel c-estabelec-fim
           tt-param.ge-ini          = input frame f-pg-sel i-ge-ini
           tt-param.ge-fim          = input frame f-pg-sel i-ge-fim
           tt-param.familia-ini     = input frame f-pg-sel c-familia-ini
           tt-param.familia-fim     = input frame f-pg-sel c-familia-fim
           tt-param.item-ini        = input frame f-pg-sel c-item-ini
           tt-param.item-fim        = input frame f-pg-sel c-item-fim
           tt-param.depos-ini       = input frame f-pg-sel c-depos-ini
           tt-param.depos-fim       = input frame f-pg-sel c-depos-fim
           tt-param.periodico       = input frame f-pg-par l-periodico
           tt-param.pto-enc         = input frame f-pg-par l-pto-enc
           tt-param.elimina         = input frame f-pg-par i-elimina
           tt-param.l-depositos     = input frame f-pg-par l-depositos
           tt-param.l-solcomp       = input frame f-pg-par l-sol-comp
           tt-param.l-reqest        = input frame f-pg-par l-req-est
           tt-param.i-sldaloc       = input frame f-pg-par i-sld-aloc
           tt-param.i-contacont     = input frame f-pg-par i-conta-cont
           tt-param.i-consprev      = input frame f-pg-par i-cons-prev
           tt-param.desc-sldaloc    = c-desc-sldaloc
           tt-param.desc-contacont  = c-desc-contacont
           tt-param.desc-consprev   = c-desc-consprev
           tt-param.desc-elimina    = c-desc-elimina.

    if  tt-param.destino = 1 
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
        {include/i-rprun.i cep/esce0340rp.p}
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

    {include/i-rprun.i cep/esce0340rp.p}

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


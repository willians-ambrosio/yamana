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
{include/i-prgvrs.i ESRE0508 12.1.13.000}  /*** 010025 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i esre0508 MRE}
&ENDIF

{cdp/cdcfgmat.i} /*  ddefini‡Æoe pr‚-processadores distribui‡Æo */

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
&GLOBAL-DEFINE PGDIG f-pg-dig
&GLOBAL-DEFINE PGIMP f-pg-imp

/* Parameters Definitions ---                                           */
{esp/esre0508.i4}
define buffer b-tt-digita for tt-digita.

/*Ativa Seguran‡a por Estabelecimento*/
{cdp/cd0019.i "mre"}

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita no-undo
   field raw-digita      as raw.

/* Local Variable Definitions ---                                       */
def var hprogramzoom as handle no-undo.

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var l-mostra           as logical no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-cla
&Scoped-define BROWSE-NAME br-digita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-digita saldo-terc

/* Definitions for BROWSE br-digita                                     */
&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.nat-operacao tt-digita.denominacao tt-digita.tipo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-digita.nat-operacao   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-digita tt-digita
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-digita tt-digita
&Scoped-define SELF-NAME br-digita
&Scoped-define QUERY-STRING-br-digita FOR EACH tt-digita WHERE tt-digita.tipo-digita = 1
&Scoped-define OPEN-QUERY-br-digita OPEN QUERY br-digita FOR EACH tt-digita WHERE tt-digita.tipo-digita = 1.
&Scoped-define TABLES-IN-QUERY-br-digita tt-digita
&Scoped-define FIRST-TABLE-IN-QUERY-br-digita tt-digita


/* Definitions for BROWSE br-digita2                                    */
&Scoped-define FIELDS-IN-QUERY-br-digita2 tt-digita.cod-estabel tt-digita.nome   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita2 tt-digita.cod-estabel   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-digita2 tt-digita
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-digita2 tt-digita
&Scoped-define SELF-NAME br-digita2
&Scoped-define QUERY-STRING-br-digita2 FOR EACH tt-digita WHERE tt-digita.tipo-digita = 2
&Scoped-define OPEN-QUERY-br-digita2 OPEN QUERY br-digita2 FOR EACH tt-digita WHERE tt-digita.tipo-digita = 2.
&Scoped-define TABLES-IN-QUERY-br-digita2 tt-digita
&Scoped-define FIRST-TABLE-IN-QUERY-br-digita2 tt-digita


/* Definitions for FRAME f-pg-dig                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-pg-dig ~
    ~{&OPEN-QUERY-br-digita}~
    ~{&OPEN-QUERY-br-digita2}

/* Definitions for FRAME f-pg-par                                       */
&Scoped-define QUERY-STRING-f-pg-par FOR EACH saldo-terc NO-LOCK
&Scoped-define OPEN-QUERY-f-pg-par OPEN QUERY f-pg-par FOR EACH saldo-terc NO-LOCK.
&Scoped-define TABLES-IN-QUERY-f-pg-par saldo-terc
&Scoped-define FIRST-TABLE-IN-QUERY-f-pg-par saldo-terc


/* Definitions for FRAME f-pg-sel                                       */
&Scoped-define QUERY-STRING-f-pg-sel FOR EACH saldo-terc NO-LOCK
&Scoped-define OPEN-QUERY-f-pg-sel OPEN QUERY f-pg-sel FOR EACH saldo-terc NO-LOCK.
&Scoped-define TABLES-IN-QUERY-f-pg-sel saldo-terc
&Scoped-define FIRST-TABLE-IN-QUERY-f-pg-sel saldo-terc


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-classif 
&Scoped-Define DISPLAYED-OBJECTS rs-classif 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE rs-classif AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Por Fornecedor", 1,
"Por Item", 2
     SIZE 25 BY 2 NO-UNDO.

DEFINE BUTTON bt-alterar 
     LABEL "Alterar" 
     SIZE 15 BY .92.

DEFINE BUTTON bt-alterar2 
     LABEL "Alterar" 
     SIZE 15 BY .92.

DEFINE BUTTON bt-inserir 
     LABEL "Inserir" 
     SIZE 15 BY .92.

DEFINE BUTTON bt-inserir2 
     LABEL "Inserir" 
     SIZE 15 BY .92.

DEFINE BUTTON bt-recuperar 
     LABEL "Recuperar" 
     SIZE 15 BY .92.

DEFINE BUTTON bt-recuperar2 
     LABEL "Recuperar" 
     SIZE 15 BY .92.

DEFINE BUTTON bt-retirar 
     LABEL "Retirar" 
     SIZE 15 BY .92.

DEFINE BUTTON bt-retirar2 
     LABEL "Retirar" 
     SIZE 15 BY .92.

DEFINE BUTTON bt-salvar 
     LABEL "Salvar" 
     SIZE 15 BY .92.

DEFINE BUTTON bt-salvar2 
     LABEL "Salvar" 
     SIZE 15 BY .92.

DEFINE VARIABLE c-tx-cod-estabel AS CHARACTER FORMAT "X(256)":U INITIAL "Estabelecimento" 
      VIEW-AS TEXT 
     SIZE 14 BY .67 NO-UNDO.

DEFINE VARIABLE c-tx-nat-operacao AS CHARACTER FORMAT "X(256)":U INITIAL "Natureza Opera‡Æo" 
      VIEW-AS TEXT 
     SIZE 14 BY .67 NO-UNDO.

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

DEFINE VARIABLE text-impressao AS CHARACTER FORMAT "X(256)":U INITIAL "Parƒmetros de ImpressÆo" 
      VIEW-AS TEXT 
     SIZE 25 BY .63 NO-UNDO.

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

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.25.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE l-param-impr AS LOGICAL INITIAL yes 
     LABEL "&Imprimir parƒmetros" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .83 NO-UNDO.

DEFINE VARIABLE text-emissao AS CHARACTER FORMAT "X(256)":U INITIAL "EmissÆo Relat¢rio" 
      VIEW-AS TEXT 
     SIZE 19 BY .67 NO-UNDO.

DEFINE VARIABLE text-preco AS CHARACTER FORMAT "X(256)":U INITIAL "Pre‡o" 
      VIEW-AS TEXT 
     SIZE 8.14 BY .67 NO-UNDO.

DEFINE VARIABLE text-tipo AS CHARACTER FORMAT "X(256)":U INITIAL "Tipo Movimento" 
      VIEW-AS TEXT 
     SIZE 15.72 BY .67 NO-UNDO.

DEFINE VARIABLE i-tp-emis-relat AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Detalhado", 1,
"Resumido", 2
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE l-preco-medio AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "M‚dio", yes,
"Informado", no
     SIZE 25 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35.14 BY 9.17.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 2.75.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 2.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 2.

DEFINE VARIABLE l-drawback AS LOGICAL INITIAL yes 
     LABEL "Drawback" 
     VIEW-AS TOGGLE-BOX
     SIZE 23.14 BY .83 NO-UNDO.

DEFINE VARIABLE l-ent-ben AS LOGICAL INITIAL yes 
     LABEL "Entrada Beneficiamento" 
     VIEW-AS TOGGLE-BOX
     SIZE 26.14 BY .83 NO-UNDO.

DEFINE VARIABLE l-ent-cons AS LOGICAL INITIAL yes 
     LABEL "Entrada Consigna‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 23.14 BY .83 NO-UNDO.

DEFINE VARIABLE l-ent-fut AS LOGICAL INITIAL yes 
     LABEL "Entrega Futura" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.86 BY .83 NO-UNDO.

DEFINE VARIABLE l-lista-narra AS LOGICAL INITIAL no 
     LABEL "Lista Narrativa" 
     VIEW-AS TOGGLE-BOX
     SIZE 29.14 BY .83 NO-UNDO.

DEFINE VARIABLE l-sai-ben AS LOGICAL INITIAL yes 
     LABEL "Sa¡da Beneficiamento" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .83 NO-UNDO.

DEFINE VARIABLE l-sai-cons AS LOGICAL INITIAL yes 
     LABEL "Sa¡da Consigna‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 22.29 BY .83 NO-UNDO.

DEFINE VARIABLE l-saldo-zerado AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 29.14 BY .83 NO-UNDO.

DEFINE VARIABLE l-transf AS LOGICAL INITIAL yes 
     LABEL "Transferˆncia" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.29 BY .83 NO-UNDO.

DEFINE VARIABLE c-fim-estabel LIKE estabelec.cod-estabel INITIAL "ZZZZ"
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88 NO-UNDO.

DEFINE VARIABLE i-fim-ge-codigo AS INTEGER FORMAT "99" INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88.

DEFINE VARIABLE c-fim-it-codigo AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88.

DEFINE VARIABLE c-fim-nat-operacao AS CHARACTER FORMAT "9.99-xxx" INITIAL "999ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88.

DEFINE VARIABLE c-fim-nro-docto AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88.

DEFINE VARIABLE c-fim-serie-docto AS CHARACTER FORMAT "x(5)" INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88.

DEFINE VARIABLE c-ini-estabel LIKE estabelec.cod-estabel
     LABEL "Estab":R7 
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88 NO-UNDO.

DEFINE VARIABLE i-ini-ge-codigo AS INTEGER FORMAT "99" INITIAL 0 
     LABEL "Grupo Estoque":R11 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88.

DEFINE VARIABLE c-ini-it-codigo AS CHARACTER FORMAT "x(16)" 
     LABEL "Item":R5 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88.

DEFINE VARIABLE c-ini-nat-operacao AS CHARACTER FORMAT "9.99-xxx" INITIAL "000AAA" 
     LABEL "Nat Opera‡Æo":R15 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88.

DEFINE VARIABLE c-ini-nro-docto AS CHARACTER FORMAT "x(16)" 
     LABEL "Documento":R11 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88.

DEFINE VARIABLE c-ini-serie-docto AS CHARACTER FORMAT "x(5)" 
     LABEL "S‚rie":R7 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88.

DEFINE VARIABLE c-nr-ato-conce AS CHARACTER FORMAT "x(20)" 
     LABEL "Nr. Ato Concessorio":R11 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88.

DEFINE VARIABLE c-nr-ato-conce-fim AS CHARACTER FORMAT "x(20)" INITIAL "ZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88.

DEFINE VARIABLE da-fim-per AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88.

DEFINE VARIABLE da-ini-per AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Data":R15 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88.

DEFINE VARIABLE i-fim-cod-emitente AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88.

DEFINE VARIABLE i-ini-cod-emitente AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     LABEL "Emitente":R10 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-fir":U
     SIZE 3 BY 1.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image\im-las":U
     SIZE 3 BY 1.

DEFINE IMAGE IMAGE-13
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-17
     FILENAME "image\im-fir":U
     SIZE 3 BY 1.

DEFINE IMAGE IMAGE-18
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-19
     FILENAME "image\im-fir":U
     SIZE 3 BY 1.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-20
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-21
     FILENAME "image\im-fir":U
     SIZE 3 BY 1.

DEFINE IMAGE IMAGE-22
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY 1.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-fir":U
     SIZE 3 BY 1.

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-fir":U
     SIZE 3 BY 1.

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

DEFINE QUERY br-digita2 FOR 
      tt-digita SCROLLING.

DEFINE QUERY f-pg-par FOR 
      saldo-terc SCROLLING.

DEFINE QUERY f-pg-sel FOR 
      saldo-terc SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-digita C-Win _FREEFORM
  QUERY br-digita DISPLAY
      tt-digita.nat-operacao
      tt-digita.denominacao
      tt-digita.tipo
ENABLE
tt-digita.nat-operacao
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 75 BY 8.71
         BGCOLOR 15 .

DEFINE BROWSE br-digita2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-digita2 C-Win _FREEFORM
  QUERY br-digita2 DISPLAY
      tt-digita.cod-estabel
      tt-digita.nome
ENABLE
tt-digita.cod-estabel
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 75 BY 3.5
         BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Cancelar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder AT ROW 2.5 COL 2
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-left AT ROW 2.54 COL 2.14
     RECT-6 AT ROW 13.75 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     im-pg-cla AT ROW 1.5 COL 17.86
     im-pg-dig AT ROW 1.5 COL 49.29
     im-pg-imp AT ROW 1.5 COL 65
     im-pg-par AT ROW 1.5 COL 33.57
     im-pg-sel AT ROW 1.5 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-cla
     rs-classif AT ROW 1.5 COL 3 HELP
          "Classifica‡Æo para emissÆo do relat¢rio" NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76 BY 10.5.

DEFINE FRAME f-pg-dig
     br-digita AT ROW 1.83 COL 1
     bt-inserir AT ROW 10.58 COL 1
     bt-alterar AT ROW 10.58 COL 16
     bt-retirar AT ROW 10.58 COL 31
     bt-salvar AT ROW 10.58 COL 46
     bt-recuperar AT ROW 10.58 COL 61
     br-digita2 AT ROW 7.08 COL 1
     bt-inserir2 AT ROW 10.58 COL 1
     bt-alterar2 AT ROW 10.58 COL 16
     bt-retirar2 AT ROW 10.58 COL 31
     bt-salvar2 AT ROW 10.58 COL 46
     bt-recuperar2 AT ROW 10.58 COL 61
     c-tx-nat-operacao AT ROW 1.17 COL 2 HELP
          "Natureza Opera‡Æo" NO-LABEL
     c-tx-cod-estabel AT ROW 6.42 COL 2 HELP
          "Estabelecimento" NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76 BY 10.5.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.38 COL 3.29 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     bt-config-impr AT ROW 3.58 COL 43.29 HELP
          "Configura‡Æo da impressora"
     bt-arquivo AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     c-arquivo AT ROW 3.63 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 5.75 COL 3 HELP
          "Modo de Execu‡Æo" NO-LABEL
     l-param-impr AT ROW 8.25 COL 5
     text-destino AT ROW 1.63 COL 3.86 NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     text-impressao AT ROW 7.25 COL 2 COLON-ALIGNED NO-LABEL
     RECT-14 AT ROW 7.5 COL 2
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76 BY 10.5.

DEFINE FRAME f-pg-par
     l-preco-medio AT ROW 2.38 COL 43.86 NO-LABEL
     l-sai-ben AT ROW 2.5 COL 6.14
     l-ent-ben AT ROW 3.63 COL 6.14
     l-saldo-zerado AT ROW 4.67 COL 44.29
     l-transf AT ROW 4.75 COL 6.14
     l-lista-narra AT ROW 5.67 COL 44.29
     l-sai-cons AT ROW 5.88 COL 6.14
     l-ent-cons AT ROW 7 COL 6.14
     i-tp-emis-relat AT ROW 8 COL 44 NO-LABEL
     l-drawback AT ROW 8.13 COL 6.14
     l-ent-fut AT ROW 9.13 COL 6.14 WIDGET-ID 2
     text-preco AT ROW 1.5 COL 41 COLON-ALIGNED NO-LABEL
     text-tipo AT ROW 1.63 COL 2.57 COLON-ALIGNED NO-LABEL
     text-emissao AT ROW 7.25 COL 41 COLON-ALIGNED NO-LABEL
     RECT-10 AT ROW 1.83 COL 3.57
     RECT-11 AT ROW 4.25 COL 41.29
     RECT-12 AT ROW 1.83 COL 41.29
     RECT-13 AT ROW 7.5 COL 41
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76 BY 10.5.

DEFINE FRAME f-pg-sel
     c-ini-nro-docto AT ROW 1.5 COL 20 COLON-ALIGNED
     c-fim-nro-docto AT ROW 1.5 COL 47 COLON-ALIGNED NO-LABEL
     c-ini-serie-docto AT ROW 2.5 COL 20 COLON-ALIGNED
     c-fim-serie-docto AT ROW 2.5 COL 47 COLON-ALIGNED NO-LABEL
     c-ini-nat-operacao AT ROW 3.5 COL 20 COLON-ALIGNED HELP
          "Natureza de Operacao"
     c-fim-nat-operacao AT ROW 3.5 COL 47 COLON-ALIGNED HELP
          "Natureza de Operacao" NO-LABEL
     c-ini-it-codigo AT ROW 4.5 COL 20 COLON-ALIGNED
     c-fim-it-codigo AT ROW 4.5 COL 47 COLON-ALIGNED NO-LABEL
     i-ini-cod-emitente AT ROW 5.5 COL 20 COLON-ALIGNED
     i-fim-cod-emitente AT ROW 5.5 COL 47 COLON-ALIGNED NO-LABEL
     da-ini-per AT ROW 6.5 COL 20 COLON-ALIGNED
     da-fim-per AT ROW 6.5 COL 47 COLON-ALIGNED NO-LABEL
     c-nr-ato-conce AT ROW 7.5 COL 20 COLON-ALIGNED
     c-nr-ato-conce-fim AT ROW 7.5 COL 47 COLON-ALIGNED NO-LABEL
     c-ini-estabel AT ROW 8.5 COL 20 COLON-ALIGNED HELP
          ""
          LABEL "Estab":R7
     c-fim-estabel AT ROW 8.5 COL 47 COLON-ALIGNED HELP
          "" NO-LABEL
     i-ini-ge-codigo AT ROW 9.5 COL 20 COLON-ALIGNED WIDGET-ID 98
     i-fim-ge-codigo AT ROW 9.5 COL 47 COLON-ALIGNED NO-LABEL WIDGET-ID 96
     IMAGE-1 AT ROW 1.5 COL 42
     IMAGE-10 AT ROW 6.5 COL 42
     IMAGE-11 AT ROW 3.5 COL 46
     IMAGE-12 AT ROW 4.5 COL 46
     IMAGE-13 AT ROW 5.5 COL 46
     IMAGE-14 AT ROW 6.5 COL 46
     IMAGE-17 AT ROW 7.5 COL 42
     IMAGE-18 AT ROW 7.5 COL 46
     IMAGE-19 AT ROW 8.5 COL 42
     IMAGE-2 AT ROW 1.5 COL 46
     IMAGE-20 AT ROW 8.5 COL 46
     IMAGE-3 AT ROW 2.5 COL 42
     IMAGE-4 AT ROW 2.5 COL 46
     IMAGE-7 AT ROW 3.5 COL 42
     IMAGE-8 AT ROW 4.5 COL 42
     IMAGE-9 AT ROW 5.5 COL 42
     IMAGE-21 AT ROW 9.5 COL 42 WIDGET-ID 100
     IMAGE-22 AT ROW 9.5 COL 46 WIDGET-ID 102
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-relat
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Relat¢rio Saldos em Poder de Terceiros"
         HEIGHT             = 15.17
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-relat.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-pg-cla
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME f-pg-dig
   Custom                                                               */
/* BROWSE-TAB br-digita 1 f-pg-dig */
/* BROWSE-TAB br-digita2 bt-recuperar f-pg-dig */
ASSIGN 
       br-digita2:HIDDEN  IN FRAME f-pg-dig                = TRUE.

/* SETTINGS FOR BUTTON bt-alterar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-alterar2 IN FRAME f-pg-dig
   NO-ENABLE                                                            */
ASSIGN 
       bt-alterar2:HIDDEN IN FRAME f-pg-dig           = TRUE.

ASSIGN 
       bt-inserir2:HIDDEN IN FRAME f-pg-dig           = TRUE.

ASSIGN 
       bt-recuperar2:HIDDEN IN FRAME f-pg-dig           = TRUE.

/* SETTINGS FOR BUTTON bt-retirar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-retirar2 IN FRAME f-pg-dig
   NO-ENABLE                                                            */
ASSIGN 
       bt-retirar2:HIDDEN IN FRAME f-pg-dig           = TRUE.

/* SETTINGS FOR BUTTON bt-salvar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-salvar2 IN FRAME f-pg-dig
   NO-ENABLE                                                            */
ASSIGN 
       bt-salvar2:HIDDEN IN FRAME f-pg-dig           = TRUE.

/* SETTINGS FOR FILL-IN c-tx-cod-estabel IN FRAME f-pg-dig
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN c-tx-nat-operacao IN FRAME f-pg-dig
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FRAME f-pg-imp
                                                                        */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-impressao IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-impressao:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Parƒmetros de ImpressÆo".

/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Execu‡Æo".

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
ASSIGN 
       l-saldo-zerado:PRIVATE-DATA IN FRAME f-pg-par     = 
                "Lista Saldo Zerado".

/* SETTINGS FOR FILL-IN text-emissao IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-emissao:PRIVATE-DATA IN FRAME f-pg-par     = 
                "EmissÆo Relat¢rio".

/* SETTINGS FOR FILL-IN text-preco IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-preco:PRIVATE-DATA IN FRAME f-pg-par     = 
                "Pre‡o".

/* SETTINGS FOR FILL-IN text-tipo IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-tipo:PRIVATE-DATA IN FRAME f-pg-par     = 
                "Tipo Movimento".

/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FILL-IN c-fim-estabel IN FRAME f-pg-sel
   LIKE = mgadm.estabelec.cod-estabel EXP-LABEL EXP-SIZE                */
ASSIGN 
       c-fim-estabel:HIDDEN IN FRAME f-pg-sel           = TRUE.

/* SETTINGS FOR FILL-IN c-ini-estabel IN FRAME f-pg-sel
   LIKE = mgadm.estabelec.cod-estabel EXP-LABEL EXP-SIZE                */
ASSIGN 
       c-ini-estabel:HIDDEN IN FRAME f-pg-sel           = TRUE.

ASSIGN 
       c-nr-ato-conce:HIDDEN IN FRAME f-pg-sel           = TRUE.

ASSIGN 
       c-nr-ato-conce-fim:HIDDEN IN FRAME f-pg-sel           = TRUE.

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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-digita
/* Query rebuild information for BROWSE br-digita
     _START_FREEFORM
OPEN QUERY br-digita FOR EACH tt-digita WHERE tt-digita.tipo-digita = 1.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-digita */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-digita2
/* Query rebuild information for BROWSE br-digita2
     _START_FREEFORM
OPEN QUERY br-digita2 FOR EACH tt-digita WHERE tt-digita.tipo-digita = 2.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-digita2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-imp
/* Query rebuild information for FRAME f-pg-imp
     _Query            is NOT OPENED
*/  /* FRAME f-pg-imp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-par
/* Query rebuild information for FRAME f-pg-par
     _TblList          = "saldo-terc"
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* FRAME f-pg-par */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-sel
/* Query rebuild information for FRAME f-pg-sel
     _TblList          = "saldo-terc"
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Relat¢rio Saldos em Poder de Terceiros */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Relat¢rio Saldos em Poder de Terceiros */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-digita
&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON DEL OF br-digita IN FRAME f-pg-dig
DO:
   apply 'choose' to bt-retirar in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
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
        display tt-digita.nat-operacao
                tt-digita.denominacao
                tt-digita.tipo
                with browse br-digita. 
    end.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON ENTER OF br-digita IN FRAME f-pg-dig
ANYWHERE
DO:
  apply 'tab' to self.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON INS OF br-digita IN FRAME f-pg-dig
DO:
   apply 'choose' to bt-inserir in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON OFF-END OF br-digita IN FRAME f-pg-dig
DO:
   apply 'entry' to bt-inserir in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON OFF-HOME OF br-digita IN FRAME f-pg-dig
DO:
  apply 'entry' to bt-recuperar in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON ROW-ENTRY OF br-digita IN FRAME f-pg-dig
DO:
   /* trigger para inicializar campos da temp table de digita‡Æo */
   if  br-digita:new-row in frame f-pg-dig then do:
      assign tt-digita.nat-operacao:screen-value in browse br-digita = ""
             tt-digita.denominacao:screen-value  in browse br-digita = ""
             tt-digita.tipo:screen-value         in browse br-digita = ""
             l-mostra = yes.
  end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON ROW-LEAVE OF br-digita IN FRAME f-pg-dig
DO:
   RUN pi-br-digita.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON VALUE-CHANGED OF br-digita IN FRAME f-pg-dig
DO:
    ASSIGN br-digita:SCROLLBAR-VERTICAL IN FRAME f-pg-dig = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-digita2
&Scoped-define SELF-NAME br-digita2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita2 C-Win
ON DEL OF br-digita2 IN FRAME f-pg-dig
DO:
    APPLY 'choose' TO bt-retirar2 IN FRAME f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita2 C-Win
ON END-ERROR OF br-digita2 IN FRAME f-pg-dig
ANYWHERE 
DO:
    RUN pi-end-error-dig2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita2 C-Win
ON ENTER OF br-digita2 IN FRAME f-pg-dig
ANYWHERE
DO:
    APPLY 'tab' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita2 C-Win
ON INS OF br-digita2 IN FRAME f-pg-dig
DO:
    APPLY 'choose' TO bt-inserir2 IN FRAME f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita2 C-Win
ON OFF-END OF br-digita2 IN FRAME f-pg-dig
DO:
    APPLY 'entry' TO bt-inserir2 IN FRAME f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita2 C-Win
ON OFF-HOME OF br-digita2 IN FRAME f-pg-dig
DO:
    APPLY 'entry' TO bt-recuperar2 IN FRAME f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita2 C-Win
ON ROW-ENTRY OF br-digita2 IN FRAME f-pg-dig
DO:
    RUN pi-row-entry-dig2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita2 C-Win
ON ROW-LEAVE OF br-digita2 IN FRAME f-pg-dig
DO:
    RUN pi-row-leave-dig2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita2 C-Win
ON VALUE-CHANGED OF br-digita2 IN FRAME f-pg-dig
DO:
    ASSIGN br-digita2:SCROLLBAR-VERTICAL IN FRAME f-pg-dig = YES.
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


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-alterar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-alterar C-Win
ON CHOOSE OF bt-alterar IN FRAME f-pg-dig /* Alterar */
DO:
   apply 'entry' to tt-digita.nat-operacao in browse br-digita. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-alterar2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-alterar2 C-Win
ON CHOOSE OF bt-alterar2 IN FRAME f-pg-dig /* Alterar */
DO:
    APPLY 'entry' TO tt-digita.cod-estabel IN BROWSE br-digita2.
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


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-inserir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inserir C-Win
ON CHOOSE OF bt-inserir IN FRAME f-pg-dig /* Inserir */
DO:
    RUN pi-bt-inserir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-inserir2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inserir2 C-Win
ON CHOOSE OF bt-inserir2 IN FRAME f-pg-dig /* Inserir */
DO:
    RUN pi-bt-inserir2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-recuperar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-recuperar C-Win
ON CHOOSE OF bt-recuperar IN FRAME f-pg-dig /* Recuperar */
DO:
    RUN pi-bt-recuperar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-recuperar2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-recuperar2 C-Win
ON CHOOSE OF bt-recuperar2 IN FRAME f-pg-dig /* Recuperar */
DO:
    RUN pi-bt-recuperar2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-retirar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-retirar C-Win
ON CHOOSE OF bt-retirar IN FRAME f-pg-dig /* Retirar */
DO:
    APPLY 'entry' TO br-digita IN FRAME f-pg-dig.
    RUN pi-bt-retirar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-retirar2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-retirar2 C-Win
ON CHOOSE OF bt-retirar2 IN FRAME f-pg-dig /* Retirar */
DO:
    APPLY 'entry' TO br-digita2 IN FRAME f-pg-dig.
    RUN pi-bt-retirar2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-salvar C-Win
ON CHOOSE OF bt-salvar IN FRAME f-pg-dig /* Salvar */
DO:
    RUN pi-bt-salvar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-salvar2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-salvar2 C-Win
ON CHOOSE OF bt-salvar2 IN FRAME f-pg-dig /* Salvar */
DO:
    RUN pi-bt-salvar2.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME c-nr-ato-conce-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nr-ato-conce-fim C-Win
ON F5 OF c-nr-ato-conce-fim IN FRAME f-pg-sel
DO:
      {method/zoomfields.i &ProgramZoom="cxzoom/z01cx312.w"
                         &FieldZoom1="nr-ato-concessorio"
                         &FieldScreen1="c-nr-ato-conce"
                         &frame1="{&frame-name}"}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nr-ato-conce-fim C-Win
ON MOUSE-SELECT-DBLCLICK OF c-nr-ato-conce-fim IN FRAME f-pg-sel
DO:
    apply "f5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME i-tp-emis-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-tp-emis-relat C-Win
ON VALUE-CHANGED OF i-tp-emis-relat IN FRAME f-pg-par
DO:
    IF i-tp-emis-relat:SCREEN-VALUE = "2" THEN
        ASSIGN l-lista-narra:SENSITIVE = NO.
    ELSE 
        ASSIGN l-lista-narra:SENSITIVE = YES.
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


&Scoped-define SELF-NAME im-pg-dig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-dig C-Win
ON MOUSE-SELECT-CLICK OF im-pg-dig IN FRAME f-relat
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
&Scoped-define SELF-NAME l-drawback
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-drawback C-Win
ON VALUE-CHANGED OF l-drawback IN FRAME f-pg-par /* Drawback */
DO:

   if l-drawback:screen-value in frame f-pg-par = "no" then do:
      assign c-nr-ato-conce:sensitive in frame f-pg-sel = no.
      assign c-nr-ato-conce-fim:sensitive in frame f-pg-sel = no.
   end.
   else do:
      assign c-nr-ato-conce:sensitive in frame f-pg-sel = yes.
      assign c-nr-ato-conce-fim:sensitive in frame f-pg-sel = yes.
   end.

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


&Scoped-define FRAME-NAME f-pg-cla
&Scoped-define BROWSE-NAME br-digita
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
/************************************************************ 
 *  Pr‚-Processador para - 2.03
 ************************************************************/      

 on F5 of tt-digita.nat-operacao in browse br-digita 
 or MOUSE-SELECT-DBLCLICK    of tt-digita.nat-operacao in browse br-digita do:

    {include/zoomvar.i &prog-zoom=inzoom/z01in245.w
                       &campo1=tt-digita.nat-operacao
                       &campo2=tt-digita.denominacao
                       &campo3=tt-digita.tipo
                       &campozoom=nat-operacao
                       &browse=br-digita}
 end.                      
 on  "entry" of tt-digita.nat-operacao in browse br-digita
     assign l-mostra = yes.

 ON  F5 OF tt-digita.cod-estabel IN BROWSE br-digita2
 OR  MOUSE-SELECT-DBLCLICK OF tt-digita.cod-estabel IN BROWSE br-digita2 DO:

     {include/zoomvar.i &prog-zoom=adzoom/z01ad107.w
                        &campo=tt-digita.cod-estabel
                        &campozoom=cod-estabel
                        &campo1=tt-digita.nome
                        &campozoom1=nome
                        &browse=br-digita2}
 END.


/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.


{utp/ut9000.i "ESRE0508" "12.1.13.000"}

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

    {utp/ut-field.i mgind natur-oper nat-operacao 4}
    assign c-ini-nat-operacao:format in frame f-pg-sel = trim(return-value)
           c-fim-nat-operacao:format in frame f-pg-sel = trim(return-value).

    RUN enable_UI.
/********************* consistˆncias   */  

/* {cep/ce9998.i}*/

    assign da-ini-per:screen-value in frame f-pg-sel = string(today)
           da-fim-per:screen-value in frame f-pg-sel = string(today)
           l-mostra = yes.

 {utp/ut-liter.i Lista_Saldos_Zerados * }
 assign l-saldo-zerado:label in frame f-pg-par = return-value.

 /*{utp/ut-liter.i Lista_Valor_ao_Pre‡o * }
 assign l-preco-medio:label in frame f-pg-par = return-value. 
 */
 {include/i-rpmbl.i}


/************************************************************ 
 *  Pr‚-Processador para - 2.03
 ************************************************************/      
  &if defined (bf_mat_versao_ems)  &then
    &if {&bf_mat_versao_ems} >= 2.03 &then 
      do:
        assign c-nr-ato-conce:hidden        in frame f-pg-sel = no
               c-nr-ato-conce-fim:hidden    in frame f-pg-sel = no
               image-17:hidden              in frame f-pg-sel = no
               image-18:hidden              in frame f-pg-sel = no
               l-drawback:hidden            in frame f-pg-par = no. 
      end.
    &endif
    &if {&bf_mat_versao_ems} >= 2.062 &then 
        assign l-lista-narra:hidden         in frame f-pg-par = no.
    &ELSE
        assign l-lista-narra:hidden         in frame f-pg-par = YES.
    &endif
  &else do:
       assign c-nr-ato-conce:hidden     in frame f-pg-sel = yes   
              c-nr-ato-conce-fim:hidden in frame f-pg-sel = yes
              image-17:hidden           in frame f-pg-sel = yes
              image-18:hidden           in frame f-pg-sel = yes
              l-drawback:hidden         in frame f-pg-par = yes.      
   end.   
  &endif

    /*************************************************************
    **              BROWSE DIGITA ESTABELECIMENTO               **
    *************************************************************/
    &IF '{&bf_mat_versao_ems}' >= '2.04' &THEN

        ASSIGN br-digita2       :HIDDEN IN FRAME f-pg-dig = NO
               bt-inserir2      :HIDDEN IN FRAME f-pg-dig = NO
               bt-alterar2      :HIDDEN IN FRAME f-pg-dig = NO
               bt-retirar2      :HIDDEN IN FRAME f-pg-dig = NO
               bt-salvar2       :HIDDEN IN FRAME f-pg-dig = NO
               bt-recuperar2    :HIDDEN IN FRAME f-pg-dig = NO
               c-tx-cod-estabel :HIDDEN IN FRAME f-pg-dig = NO
               c-tx-nat-operacao:HIDDEN IN FRAME f-pg-dig = NO

               br-digita        :HEIGHT IN FRAME f-pg-dig = 3.5
               br-digita        :COL    IN FRAME f-pg-dig = 1
               br-digita        :ROW    IN FRAME f-pg-dig = 1.83

               bt-inserir       :ROW    IN FRAME f-pg-dig = 5.33
               bt-alterar       :ROW    IN FRAME f-pg-dig = 5.33
               bt-retirar       :ROW    IN FRAME f-pg-dig = 5.33
               bt-salvar        :ROW    IN FRAME f-pg-dig = 5.33
               bt-recuperar     :ROW    IN FRAME f-pg-dig = 5.33.
    &ELSE

        ASSIGN br-digita2       :HIDDEN IN FRAME f-pg-dig = YES
               bt-inserir2      :HIDDEN IN FRAME f-pg-dig = YES
               bt-alterar2      :HIDDEN IN FRAME f-pg-dig = YES
               bt-retirar2      :HIDDEN IN FRAME f-pg-dig = YES
               bt-salvar2       :HIDDEN IN FRAME f-pg-dig = YES
               bt-recuperar2    :HIDDEN IN FRAME f-pg-dig = YES
               c-tx-cod-estabel :HIDDEN IN FRAME f-pg-dig = YES
               c-tx-nat-operacao:HIDDEN IN FRAME f-pg-dig = YES.
    &ENDIF


   IF  NOT THIS-PROCEDURE:PERSISTENT THEN
       WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

&IF "{&mguni_version}" >= "2.071" &THEN
           c-fim-estabel:screen-value in frame f-pg-sel = "ZZZZZ".
&ELSE
           c-fim-estabel:screen-value in frame f-pg-sel = "ZZZ".
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
  ENABLE im-pg-cla im-pg-dig im-pg-imp im-pg-par im-pg-sel bt-executar 
         bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY c-ini-nro-docto c-fim-nro-docto c-ini-serie-docto c-fim-serie-docto 
          c-ini-nat-operacao c-fim-nat-operacao c-ini-it-codigo c-fim-it-codigo 
          i-ini-cod-emitente i-fim-cod-emitente da-ini-per da-fim-per 
          c-nr-ato-conce c-nr-ato-conce-fim c-ini-estabel c-fim-estabel 
          i-ini-ge-codigo i-fim-ge-codigo 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE IMAGE-1 IMAGE-10 IMAGE-11 IMAGE-12 IMAGE-13 IMAGE-14 IMAGE-17 IMAGE-18 
         IMAGE-19 IMAGE-2 IMAGE-20 IMAGE-3 IMAGE-4 IMAGE-7 IMAGE-8 IMAGE-9 
         IMAGE-21 IMAGE-22 c-ini-nro-docto c-fim-nro-docto c-ini-serie-docto 
         c-fim-serie-docto c-ini-nat-operacao c-fim-nat-operacao 
         c-ini-it-codigo c-fim-it-codigo i-ini-cod-emitente i-fim-cod-emitente 
         da-ini-per da-fim-per c-nr-ato-conce c-nr-ato-conce-fim c-ini-estabel 
         c-fim-estabel i-ini-ge-codigo i-fim-ge-codigo 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-classif 
      WITH FRAME f-pg-cla IN WINDOW C-Win.
  ENABLE rs-classif 
      WITH FRAME f-pg-cla IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-cla}
  DISPLAY c-tx-nat-operacao c-tx-cod-estabel 
      WITH FRAME f-pg-dig IN WINDOW C-Win.
  ENABLE br-digita bt-inserir bt-recuperar br-digita2 bt-inserir2 bt-recuperar2 
      WITH FRAME f-pg-dig IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-dig}
  DISPLAY rs-destino c-arquivo rs-execucao l-param-impr 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  ENABLE RECT-14 RECT-7 RECT-9 rs-destino bt-config-impr bt-arquivo c-arquivo 
         rs-execucao l-param-impr 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}

  {&OPEN-QUERY-f-pg-par}
  GET FIRST f-pg-par.
  DISPLAY l-preco-medio l-sai-ben l-ent-ben l-saldo-zerado l-transf 
          l-lista-narra l-sai-cons l-ent-cons i-tp-emis-relat l-drawback 
          l-ent-fut 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  ENABLE RECT-10 RECT-11 RECT-12 RECT-13 l-preco-medio l-sai-ben l-ent-ben 
         l-saldo-zerado l-transf l-lista-narra l-sai-cons l-ent-cons 
         i-tp-emis-relat l-drawback l-ent-fut 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
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
   /*Desativa Seguran‡a por Estabelecimento*/
   {cdp/cd0019.i1 "mre"}

   APPLY "CLOSE":U TO THIS-PROCEDURE.

   RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize C-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
/************************************************************ 
 *  Pr‚-Processador para - 2.03
 ************************************************************/      
 &if defined (bf_mat_versao_ems)  &then
   &if {&bf_mat_versao_ems} < 2.03 &then do:

      assign  c-nr-ato-conce:hidden     in frame f-pg-sel = yes 
              c-nr-ato-conce-fim:hidden in frame f-pg-sel = yes 
              c-ini-estabel             in frame f-pg-sel = yes
              c-fim-estabel             in frame f-pg-sel = yes
              l-drawback:hidden         in frame f-pg-par = yes
              image-17:hidden           in frame f-pg-sel = yes
              image-18:hidden           in frame f-pg-sel = yes
              image-19:hidden           in frame f-pg-sel = yes
              image-20:hidden           in frame f-pg-sel = yes.

      

   end.   
   &endif
 &else 
      assign  c-nr-ato-conce:hidden     in frame f-pg-sel = yes 
              c-nr-ato-conce-fim:hidden in frame f-pg-sel = yes 
              c-ini-estabel:hidden      in frame f-pg-sel = yes
              c-fim-estabel:hidden      in frame f-pg-sel = yes
              l-drawback:hidden         in frame f-pg-par = yes
              image-17:hidden           in frame f-pg-sel = yes
              image-18:hidden           in frame f-pg-sel = yes
              image-19:hidden           in frame f-pg-sel = yes
              image-20:hidden           in frame f-pg-sel = yes.

 &endif   
 

  /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-br-digita C-Win 
PROCEDURE pi-br-digita :
DEF VAR vi-seq AS INTEGER NO-UNDO.

    /*  aqui que a grava‡Æo da linha da temp-table ‚ efetivada. EntÆo
       neste gatilho devem ser implementadas as valida‡äes necess rias */
    if  l-mostra = yes then do:
    assign l-mostra = no.
    if  br-digita:new-row in frame f-pg-dig then do transaction on error undo, return no-apply:
        /* Teste da chave duplicada na cria‡Æo de registros */

        if substr(input browse br-digita tt-digita.nat-operacao,1,3) = " " then do: 
           run utp/ut-msgs.p (input "show", input 5220, input "").
           return no-apply.
        end.

        if  can-find(b-tt-digita 
            where b-tt-digita.nat-operacao = input browse br-digita tt-digita.nat-operacao) then do:
            run utp/ut-msgs.p (input "show", input 108, input "").
            return no-apply.         
        end.                   

        /* outras valida‡äes */
        find natur-oper
            where natur-oper.nat-operacao = input browse br-digita tt-digita.nat-operacao
            no-lock no-error.
        if  not avail natur-oper then do:
            run utp/ut-msgs.p (input "show", input 2050, input "").
            return no-apply.
        end.
        else do:

            FOR LAST b-tt-digita:
                ASSIGN vi-seq = b-tt-digita.i-seq.
            END.

            create tt-digita.
            assign tt-digita.i-seq       = vi-seq + 1
                   tt-digita.tipo-digita = 1
                   tt-digita.denominacao:screen-value in browse br-digita = string(natur-oper.denominacao)
                   tt-digita.tipo:screen-value in browse br-digita        = string(natur-oper.tipo)
                   input browse br-digita tt-digita.nat-operacao
                   input browse br-digita tt-digita.denominacao
                   input browse br-digita tt-digita.tipo NO-ERROR.
        end.
    end.
    else do transaction:
        /* Teste da chave duplicada na altera‡Æo de registros */
        if  can-find(b-tt-digita 
            where b-tt-digita.nat-operacao = tt-digita.nat-operacao:screen-value in browse br-digita
            and   rowid(b-tt-digita) <> rowid(tt-digita)) then do:
            run utp/ut-msgs.p (input "show", input 108, input "").
            return no-apply.         
        end.

        if substr(input browse br-digita tt-digita.nat-operacao,1,3) = " " then do: 
           run utp/ut-msgs.p (input "show", input 5220, input "").
           return no-apply.
        end.

       /* outras valida‡äes */
       find natur-oper
           where natur-oper.nat-operacao = input browse br-digita tt-digita.nat-operacao
           no-lock no-error.
       if  avail natur-oper then do:
           assign tt-digita.denominacao:screen-value in browse br-digita = string(natur-oper.denominacao)
                  tt-digita.tipo:screen-value in browse br-digita        = string(natur-oper.tipo)
                  input browse br-digita tt-digita.denominacao
                  input browse br-digita tt-digita.tipo NO-ERROR. 
       end. 
       else do:
           run utp/ut-msgs.p (input "show", input 2050, input "").
           return.
       end.
   end.
   if br-digita:new-row then br-digita:create-result-list-entry() in frame f-pg-dig.   

   end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-bt-inserir C-Win 
PROCEDURE pi-bt-inserir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF VAR vi-seq AS INT NO-UNDO.

    if  num-results('br-digita') > 0 then do:
        if  br-digita:insert-row('after') in frame f-pg-dig then.
    end.
    else do transaction:

        FOR LAST b-tt-digita:
            ASSIGN vi-seq = b-tt-digita.i-seq.
        END.

        create tt-digita.
        ASSIGN tt-digita.i-seq       = vi-seq + 1
               tt-digita.tipo-digita = 1.
        open query br-digita
             for each tt-digita WHERE tt-digita.tipo-digita = 1.
        apply 'entry' to tt-digita.nat-operacao in browse br-digita. 
        enable bt-alterar 
               bt-retirar 
               bt-salvar with frame f-pg-dig.
    end.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-bt-inserir2 C-Win 
PROCEDURE pi-bt-inserir2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF VAR vi-seq AS INT NO-UNDO.

    IF  NUM-RESULTS('br-digita2') > 0 THEN DO:
        IF  br-digita2:INSERT-ROW('after') IN FRAME f-pg-dig THEN.
    END.
    ELSE DO TRANSACTION:

        FOR LAST b-tt-digita:
            ASSIGN vi-seq = b-tt-digita.i-seq.
        END.

        CREATE tt-digita.
        ASSIGN tt-digita.i-seq       = vi-seq + 1
               tt-digita.tipo-digita = 2.

        OPEN QUERY br-digita2 FOR EACH tt-digita WHERE tt-digita.tipo-digita = 2.
        APPLY 'entry' TO tt-digita.cod-estabel IN BROWSE br-digita2.
        ENABLE bt-alterar2
               bt-retirar2
               bt-salvar2 WITH FRAME f-pg-dig.
    END.
    RETURN NO-APPLY.

/*     RETURN "OK":U. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-bt-recuperar C-Win 
PROCEDURE pi-bt-recuperar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    SYSTEM-DIALOG GET-FILE c-arq-digita
    FILTERS "*.dig" "*.dig",
            "*.*" "*.*"
    DEFAULT-EXTENSION "*.dig"
    MUST-EXIST
    USE-FILENAME
    UPDATE l-ok.

    IF  l-ok THEN DO:

        FOR EACH tt-digita
           WHERE tt-digita.tipo-digita = 1:
            DELETE tt-digita.
        END.

        INPUT FROM VALUE(c-arq-digita) NO-ECHO.
        REPEAT:
            CREATE tt-digita.
            IMPORT tt-digita NO-ERROR.

            IF  tt-digita.tipo-digita = 2 THEN
                DELETE tt-digita.
        END.
        INPUT CLOSE.

        DELETE tt-digita.

        OPEN QUERY br-digita FOR EACH tt-digita WHERE tt-digita.tipo-digita = 1.

        IF  NUM-RESULTS("br-digita":U) > 0 THEN
            ASSIGN bt-alterar:SENSITIVE IN FRAME f-pg-dig = YES
                   bt-retirar:SENSITIVE IN FRAME f-pg-dig = YES
                   bt-salvar:SENSITIVE  IN FRAME f-pg-dig = YES.
        ELSE
            ASSIGN bt-alterar:SENSITIVE IN FRAME f-pg-dig = NO
                   bt-retirar:SENSITIVE IN FRAME f-pg-dig = NO
                   bt-salvar:SENSITIVE  IN FRAME f-pg-dig = NO.
    END.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-bt-recuperar2 C-Win 
PROCEDURE pi-bt-recuperar2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    SYSTEM-DIALOG GET-FILE c-arq-digita
    FILTERS "*.dig" "*.dig",
            "*.*" "*.*"
    DEFAULT-EXTENSION "*.dig"
    MUST-EXIST
    USE-FILENAME
    UPDATE l-ok.

    IF  l-ok THEN DO:

        FOR EACH tt-digita
           WHERE tt-digita.tipo-digita = 2:
            DELETE tt-digita.
        END.

        INPUT FROM VALUE(c-arq-digita) NO-ECHO.
        REPEAT:
            CREATE tt-digita.
            IMPORT tt-digita NO-ERROR.

            IF  tt-digita.tipo-digita = 1 THEN
                DELETE tt-digita.
        END.
        INPUT CLOSE.

        DELETE tt-digita.

        OPEN QUERY br-digita2 FOR EACH tt-digita WHERE tt-digita.tipo-digita = 2.

        IF  NUM-RESULTS("br-digita2":U) > 0 THEN
            ASSIGN bt-alterar2:SENSITIVE IN FRAME f-pg-dig = YES
                   bt-retirar2:SENSITIVE IN FRAME f-pg-dig = YES
                   bt-salvar2:SENSITIVE  IN FRAME f-pg-dig = YES.
        ELSE
            ASSIGN bt-alterar2:SENSITIVE IN FRAME f-pg-dig = NO
                   bt-retirar2:SENSITIVE IN FRAME f-pg-dig = NO
                   bt-salvar2:SENSITIVE  IN FRAME f-pg-dig = NO.
    END.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-bt-retirar C-Win 
PROCEDURE pi-bt-retirar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    if  br-digita:NUM-SELECTED-ROWS IN FRAME f-pg-dig > 0 then do on error undo, return no-apply:
        get current br-digita.
        delete tt-digita.
        if  br-digita:delete-current-row() in frame f-pg-dig then.
    end.

    if  num-results('br-digita') = 0 then disable bt-alterar 
                                                  bt-retirar 
                                                  bt-salvar with frame f-pg-dig.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-bt-retirar2 C-Win 
PROCEDURE pi-bt-retirar2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF  br-digita2:NUM-SELECTED-ROWS IN FRAME f-pg-dig > 0 THEN DO ON ERROR UNDO, RETURN NO-APPLY:
        GET CURRENT br-digita2.
        DELETE tt-digita.
        IF  br-digita2:DELETE-CURRENT-ROW() IN FRAME f-pg-dig THEN.
    END.
    IF  NUM-RESULTS('br-digita2') = 0 THEN
        DISABLE bt-alterar2
                bt-retirar2
                bt-salvar2 WITH FRAME f-pg-dig.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-bt-salvar C-Win 
PROCEDURE pi-bt-salvar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VAR r-tt-digita AS ROWID NO-UNDO.

    SYSTEM-DIALOG GET-FILE c-arq-digita
        FILTERS "*.dig" "*.dig",
                "*.*" "*.*"
        ASK-OVERWRITE
        DEFAULT-EXTENSION "*.dig"
        SAVE-AS
        CREATE-TEST-FILE
        USE-FILENAME
        UPDATE l-ok.

    IF  AVAIL tt-digita AND tt-digita.tipo-digita = 1 THEN ASSIGN r-tt-digita = ROWID(tt-digita).

    IF  l-ok THEN DO:
        OUTPUT TO VALUE(c-arq-digita).
        FOR EACH tt-digita
           WHERE tt-digita.tipo-digita = 1:
            EXPORT tt-digita.
        END.
        OUTPUT CLOSE.

        REPOSITION br-digita TO ROWID(r-tt-digita) NO-ERROR.
    END.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-bt-salvar2 C-Win 
PROCEDURE pi-bt-salvar2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VAR r-tt-digita AS ROWID NO-UNDO.

    SYSTEM-DIALOG GET-FILE c-arq-digita
        FILTERS "*.dig" "*.dig",
                "*.*" "*.*"
        ASK-OVERWRITE
        DEFAULT-EXTENSION "*.dig"
        SAVE-AS
        CREATE-TEST-FILE
        USE-FILENAME
        UPDATE l-ok.

    IF  AVAIL tt-digita AND tt-digita.tipo-digita = 2 THEN ASSIGN r-tt-digita = ROWID(tt-digita).

    IF  l-ok THEN DO:
        OUTPUT TO VALUE(c-arq-digita).
        FOR EACH tt-digita
           WHERE tt-digita.tipo-digita = 2:
            EXPORT tt-digita.
        END.
        OUTPUT CLOSE.

        REPOSITION br-digita2 TO ROWID(r-tt-digita) NO-ERROR.
    END.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-end-error-dig2 C-Win 
PROCEDURE pi-end-error-dig2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF  br-digita2:NEW-ROW IN FRAME f-pg-dig THEN DO:
        IF  AVAIL tt-digita THEN
            DELETE tt-digita.
        IF  br-digita2:DELETE-CURRENT-ROW() IN FRAME f-pg-dig THEN.
    END.
    ELSE DO:
        GET CURRENT br-digita2.
        DISPLAY tt-digita.cod-estabel
                tt-digita.nome
                WITH BROWSE br-digita2.
    END.
    RETURN NO-APPLY. 

/*     RETURN "OK":U. */
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
            run utp/ut-msgs.p (input "show",
                               input 73,
                               input "").
            apply 'mouse-select-click' to im-pg-imp in frame f-relat.
            apply 'entry' to c-arquivo in frame f-pg-imp.                   
            return error.
        end.
    end.

    /* Coloque aqui as valida‡äes das outras p ginas, lembrando que elas
       devem apresentar uma mensagem de erro cadastrada, posicionar na p gina 
       com problemas e colocar o focus no campo com problemas             */    

    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame f-pg-imp rs-destino
           tt-param.data-exec       = today
           tt-param.hora-exec       = time
           tt-param.classifica      = input frame f-pg-cla rs-classif
           tt-param.desc-classifica = entry((tt-param.classifica - 1) * 2 + 1, 
                                            rs-classif:radio-buttons in frame f-pg-cla)
           tt-param.c-ini-nro-docto    = input frame f-pg-sel c-ini-nro-docto
           tt-param.c-fim-nro-docto    = input frame f-pg-sel c-fim-nro-docto
           tt-param.c-ini-serie-docto  = input frame f-pg-sel c-ini-serie-docto  
           tt-param.c-fim-serie-docto  = input frame f-pg-sel c-fim-serie-docto  
           tt-param.c-ini-nat-operacao = input frame f-pg-sel c-ini-nat-operacao
           tt-param.c-fim-nat-operacao = input frame f-pg-sel c-fim-nat-operacao 
           tt-param.c-ini-it-codigo    = input frame f-pg-sel c-ini-it-codigo    
           tt-param.c-fim-it-codigo    = input frame f-pg-sel c-fim-it-codigo
           tt-param.i-ini-ge-codigo    = input frame f-pg-sel i-ini-ge-codigo    
           tt-param.i-fim-ge-codigo    = input frame f-pg-sel i-fim-ge-codigo
           tt-param.i-ini-cod-emitente = input frame f-pg-sel i-ini-cod-emitente 
           tt-param.i-fim-cod-emitente = input frame f-pg-sel i-fim-cod-emitente 
           tt-param.da-ini-per         = input frame f-pg-sel da-ini-per         
           tt-param.da-fim-per         = input frame f-pg-sel da-fim-per         
           tt-param.l-sai-ben          = input frame f-pg-par l-sai-ben
           tt-param.l-ent-ben          = input frame f-pg-par l-ent-ben
           tt-param.l-transf           = input frame f-pg-par l-transf
           tt-param.l-sai-cons         = input frame f-pg-par l-sai-cons
           tt-param.l-ent-cons         = input frame f-pg-par l-ent-cons
           tt-param.l-ent-fut          = input frame f-pg-par l-ent-fut
           tt-param.l-preco-medio      = input frame f-pg-par l-preco-medio        
           tt-param.l-saldo-zerado     = input frame f-pg-par l-saldo-zerado
           tt-param.tp-emis-relat      = INPUT FRAME f-pg-par i-tp-emis-relat
           tt-param.param-impr         = INPUT FRAME f-pg-imp l-param-impr.
          /************************************************************ 
           *  Pr‚-Processador para - 2.03
           ************************************************************/      
           &if defined (bf_mat_versao_ems)  &then
             &if {&bf_mat_versao_ems} >= 2.03 &then do:

                assign tt-param.nr-ato-conce       = input frame f-pg-sel c-nr-ato-conce
                       tt-param.nr-ato-conce-fim   = input frame f-pg-sel c-nr-ato-conce-fim 
                       tt-param.c-ini-estabel      = input frame f-pg-sel c-ini-estabel
                       tt-param.c-fim-estabel      = input frame f-pg-sel c-fim-estabel
                       tt-param.l-drawback         = input frame f-pg-par l-drawback.
             end.   
             &endif
             &IF "{&bf_mat_versao_ems}":U >= "2.062":U &THEN
                ASSIGN tt-param.l-lista-narra      = INPUT FRAME f-pg-par l-lista-narra.
             &ENDIF
           &endif 
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

    {include/i-rprun.i esp/esre0508rp.p}
    {include/i-rpexc.i}
    if  session:set-wait-state("") then.
/*     {include/i-rptrm.i} */
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-row-entry-dig2 C-Win 
PROCEDURE pi-row-entry-dig2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    IF  br-digita2:NEW-ROW IN FRAME f-pg-dig THEN DO:
        ASSIGN tt-digita.cod-estabel:SCREEN-VALUE IN BROWSE br-digita2 = ""
               tt-digita.nome:SCREEN-VALUE        IN BROWSE br-digita2 = "".
    END.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-row-leave-dig2 C-Win 
PROCEDURE pi-row-leave-dig2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF VAR vi-seq AS INT NO-UNDO.

    IF  br-digita2:NEW-ROW IN FRAME f-pg-dig THEN DO TRANSACTION ON ERROR UNDO, RETURN NO-APPLY:

        FIND estabelec NO-LOCK WHERE
             estabelec.cod-estabel = INPUT BROWSE br-digita2 tt-digita.cod-estabel NO-ERROR.

        IF  AVAIL estabelec THEN
            ASSIGN tt-digita.nome:SCREEN-VALUE IN BROWSE br-digita2 = estabelec.nome.

        FOR LAST b-tt-digita:
            ASSIGN vi-seq = b-tt-digita.i-seq.
        END.

        CREATE tt-digita.
        ASSIGN tt-digita.i-seq       = vi-seq + 1
               tt-digita.tipo-digita = 2
               INPUT BROWSE br-digita2 tt-digita.cod-estabel
               INPUT BROWSE br-digita2 tt-digita.nome NO-ERROR.
    END.
    ELSE DO TRANSACTION:

        FIND estabelec NO-LOCK WHERE
             estabelec.cod-estabel = INPUT BROWSE br-digita2 tt-digita.cod-estabel NO-ERROR.

        IF  AVAIL estabelec THEN
            ASSIGN tt-digita.nome:SCREEN-VALUE IN BROWSE br-digita2 = estabelec.nome
                   INPUT BROWSE br-digita2 tt-digita.nome NO-ERROR.
    END.

    IF  br-digita2:NEW-ROW THEN
        br-digita2:CREATE-RESULT-LIST-ENTRY() IN FRAME f-pg-dig.

    RETURN "OK":U.
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
  {src/adm/template/snd-list.i "tt-digita"}
  {src/adm/template/snd-list.i "saldo-terc"}

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


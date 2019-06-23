&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems2cademp       PROGRESS
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
{include/i-prgvrs.i escc0407 2.06.00.000}  /*** 010014 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i escc0407 MCC}
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
{cdp/cdcfgmat.i}


/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

define temp-table tt-param
    field destino              as integer
    field arquivo              as char
    field usuario              as char
    field data-exec            as date
    field hora-exec            as integer
    field i-classifica         as integer
    field i-nome-abrev         as integer
    field l-salto-pagina       as logical
    field c-estabel-i          as char
    field c-estabel-f          as char
    field c-item-i             as char
    field c-item-f             as char
    field i-gr-fornec-i        as integer
    field i-gr-fornec-f        as integer
    field i-fornec-i           as integer
    field i-fornec-f           as integer
    field c-comprado-i         as char
    field c-comprado-f         as char
    field i-nr-processo-i      as integer
    field i-nr-processo-f      as integer
    field i-pedido-i           as integer
    field i-pedido-f           as integer
    FIELD i-contrato-i         AS INTEGER
    FIELD i-contrato-f         AS INTEGER
    FIELD l-ordem              AS LOGICAL
    FIELD l-programacao        AS LOGICAL
    FIELD l-medicao            AS LOGICAL
    field da-iniper            as date format 99/99/9999
    field da-fimper            as date format 99/99/9999
    field l-lista-narra-item   as logical
    field l-lista-narra-pedido as logical
    field l-lista-narra-ordem  as logical
    field l-lista-texto-livre  as logical
    field c-tipo-texto         as char
    field c-descri-moeda       as char
    field i-tipo-moeda         as integer
    field l-perm-texto         as logical
    field c-param              as char
    field c-classe             as char
    field c-destino            as char
    field l-ped-emitido        as logical
    field l-ped-aprovado       as logical
    field l-despesas-imp       as logical  /* Importa‡Æo */
    field i-despesas-pag       as integer  /* Importa‡Æo */
    field l-despesas-inc       as logical. /* Importa‡Æo */ 

define temp-table tt-digita
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id is primary unique ordem.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.

/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.

def var l-perm-texto       as logical no-undo.
def var c-lb-texto         as char    no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-cla

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES estabelec item emitente ordem-compra ~
contrato-for

/* Definitions for FRAME f-pg-sel                                       */
&Scoped-define QUERY-STRING-f-pg-sel FOR EACH mgadm.estabelec SHARE-LOCK, ~
      EACH mgind.item OF mgadm.estabelec SHARE-LOCK, ~
      EACH mgadm.emitente OF mgadm.estabelec SHARE-LOCK, ~
      EACH mgind.ordem-compra OF mgadm.estabelec SHARE-LOCK, ~
      EACH contrato-for OF mgadm.estabelec SHARE-LOCK
&Scoped-define OPEN-QUERY-f-pg-sel OPEN QUERY f-pg-sel FOR EACH mgadm.estabelec SHARE-LOCK, ~
      EACH mgind.item OF mgadm.estabelec SHARE-LOCK, ~
      EACH mgadm.emitente OF mgadm.estabelec SHARE-LOCK, ~
      EACH mgind.ordem-compra OF mgadm.estabelec SHARE-LOCK, ~
      EACH contrato-for OF mgadm.estabelec SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-f-pg-sel estabelec item emitente ~
ordem-compra contrato-for
&Scoped-define FIRST-TABLE-IN-QUERY-f-pg-sel estabelec
&Scoped-define SECOND-TABLE-IN-QUERY-f-pg-sel item
&Scoped-define THIRD-TABLE-IN-QUERY-f-pg-sel emitente
&Scoped-define FOURTH-TABLE-IN-QUERY-f-pg-sel ordem-compra
&Scoped-define FIFTH-TABLE-IN-QUERY-f-pg-sel contrato-for


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-10 i-nome-abrev l-salto-pagina 
&Scoped-Define DISPLAYED-OBJECTS i-classifica i-nome-abrev l-salto-pagina 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE text5 AS CHARACTER FORMAT "X(11)":U INITIAL "Fornecedor" 
      VIEW-AS TEXT 
     SIZE 13 BY .67 NO-UNDO.

DEFINE VARIABLE i-classifica AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Por Comprador / Data", 1,
"Por Data de Entrega", 2,
"Por Estabelecimento", 3,
"Por Fornecedor", 4,
"Por Item", 5,
"Por Pedido", 6,
"Por Processo de Compra", 7
     SIZE 31 BY 8 NO-UNDO.

DEFINE VARIABLE i-nome-abrev AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Por C¢digo", 1,
"Por Nome Abreviado", 2
     SIZE 26 BY 2 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 39 BY 4.88.

DEFINE VARIABLE l-salto-pagina AS LOGICAL INITIAL no 
     LABEL "Saltar P gina a cada Fornecedor" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY 1.08 NO-UNDO.

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
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE c-descri-moeda AS CHARACTER FORMAT "X(12)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-lb-imp AS CHARACTER FORMAT "X(256)":U INITIAL "Importa‡Æo" 
      VIEW-AS TEXT 
     SIZE 11.29 BY .79 NO-UNDO.

DEFINE VARIABLE c-tipo-texto AS CHARACTER FORMAT "X(4)":U 
     LABEL "Tipo de Texto" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE i-tipo-moeda AS INTEGER FORMAT ">9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE text-3 AS CHARACTER FORMAT "X(25)":U INITIAL " Tipo de Controle" 
      VIEW-AS TEXT 
     SIZE 12.86 BY .79 NO-UNDO.

DEFINE VARIABLE text1 AS CHARACTER FORMAT "X(1)":U INITIAL "-" 
      VIEW-AS TEXT 
     SIZE 1 BY .63 NO-UNDO.

DEFINE VARIABLE text2 AS CHARACTER FORMAT "X(16)":U INITIAL " Op‡Æo de Moeda" 
      VIEW-AS TEXT 
     SIZE 17 BY .79 NO-UNDO.

DEFINE VARIABLE text3 AS CHARACTER FORMAT "X(6)":U INITIAL " Texto" 
      VIEW-AS TEXT 
     SIZE 6 BY .79 NO-UNDO.

DEFINE VARIABLE rs-despesas-pag AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Pagas ao Fornecedor do Material", 1,
"Todas", 2
     SIZE 43.43 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-imp
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71.86 BY 2.5.

DEFINE RECTANGLE rt-moeda
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34.72 BY 1.63.

DEFINE RECTANGLE rt-moeda-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34.72 BY 2.88.

DEFINE RECTANGLE rt-texto
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 32 BY 2.5.

DEFINE VARIABLE l-lista-narra-item AS LOGICAL INITIAL no 
     LABEL "Narrativa do Item" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .88 NO-UNDO.

DEFINE VARIABLE l-lista-narra-ordem AS LOGICAL INITIAL no 
     LABEL "Narrativa da Ordem" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .88 NO-UNDO.

DEFINE VARIABLE l-lista-narra-pedido AS LOGICAL INITIAL no 
     LABEL "Narrativa do Pedido" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .88 NO-UNDO.

DEFINE VARIABLE l-lista-texto-livre AS LOGICAL INITIAL no 
     LABEL "Texto Livre do Item" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .88 NO-UNDO.

DEFINE VARIABLE l-medicao AS LOGICAL INITIAL yes 
     LABEL "Medi‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE l-ordem AS LOGICAL INITIAL yes 
     LABEL "Ordem" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE l-ped-aprovado AS LOGICAL INITIAL no 
     LABEL "Somente Pedidos Aprovados" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .88 NO-UNDO.

DEFINE VARIABLE l-ped-emitido AS LOGICAL INITIAL no 
     LABEL "Somente Pedidos Impressos" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .88 NO-UNDO.

DEFINE VARIABLE l-programacao AS LOGICAL INITIAL yes 
     LABEL "Programa‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE tg-despesas-imp AS LOGICAL INITIAL no 
     LABEL "Despesas Importa‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 23.43 BY .83 NO-UNDO.

DEFINE VARIABLE tg-despesas-inc AS LOGICAL INITIAL no 
     LABEL "Adiciona Despesas Pre‡o Material" 
     VIEW-AS TOGGLE-BOX
     SIZE 37.57 BY .83 NO-UNDO.

DEFINE VARIABLE c-comprado-f AS CHARACTER FORMAT "X(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88.

DEFINE VARIABLE c-comprado-i AS CHARACTER FORMAT "X(12)" 
     LABEL "Comprador":R11 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88.

DEFINE VARIABLE c-estabel-f LIKE estabelec.cod-estabel
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE c-estabel-i LIKE estabelec.cod-estabel
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE c-item-f AS CHARACTER FORMAT "X(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .88.

DEFINE VARIABLE c-item-i AS CHARACTER FORMAT "X(16)" 
     LABEL "Item":R5 
     VIEW-AS FILL-IN 
     SIZE 22 BY .88.

DEFINE VARIABLE da-fimper AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE da-iniper AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Data Entrega" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE i-contrato-f AS INTEGER FORMAT ">>>>>>>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88.

DEFINE VARIABLE i-contrato-i AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Nr Contrato" 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88.

DEFINE VARIABLE i-fornec-f AS INTEGER FORMAT ">>>>>>>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88.

DEFINE VARIABLE i-fornec-i AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Fornecedor":R12 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88.

DEFINE VARIABLE i-gr-fornec-f AS INTEGER FORMAT ">9" INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88.

DEFINE VARIABLE i-gr-fornec-i AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Grupo Fornecedor":R20 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88.

DEFINE VARIABLE i-nr-processo-f AS INTEGER FORMAT ">>>,>>9" INITIAL 999999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88.

DEFINE VARIABLE i-nr-processo-i AS INTEGER FORMAT ">>>,>>9" INITIAL 0 
     LABEL "Processo Compra":R22 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88.

DEFINE VARIABLE i-pedido-f AS INTEGER FORMAT ">>>>>,>>9" INITIAL 99999999 
     VIEW-AS FILL-IN 
     SIZE 11.57 BY .88.

DEFINE VARIABLE i-pedido-i AS INTEGER FORMAT ">>>>>,>>9" INITIAL 0 
     LABEL "Pedido":R8 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .88.

DEFINE IMAGE IMAGE-10
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

DEFINE IMAGE IMAGE-19
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-20
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-21
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-22
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-23
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-24
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-25
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-26
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
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
      estabelec, 
      item, 
      emitente, 
      ordem-compra, 
      contrato-for SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Cancelar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     im-pg-sel AT ROW 1.5 COL 2.14
     im-pg-cla AT ROW 1.5 COL 17.86
     im-pg-par AT ROW 1.5 COL 33.57
     im-pg-imp AT ROW 1.5 COL 49.29
     rt-folder AT ROW 2.5 COL 2
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-6 AT ROW 13.75 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

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
     text-destino AT ROW 1.63 COL 3.86 NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76 BY 10.

DEFINE FRAME f-pg-sel
     c-estabel-i AT ROW 1.79 COL 23 COLON-ALIGNED HELP
          ""
     c-estabel-f AT ROW 1.79 COL 52 COLON-ALIGNED HELP
          "" NO-LABEL
     c-item-i AT ROW 2.79 COL 23 COLON-ALIGNED
     c-item-f AT ROW 2.79 COL 52 COLON-ALIGNED NO-LABEL
     i-gr-fornec-i AT ROW 3.79 COL 23 COLON-ALIGNED
     i-gr-fornec-f AT ROW 3.79 COL 52 COLON-ALIGNED NO-LABEL
     i-fornec-i AT ROW 4.79 COL 23 COLON-ALIGNED
     i-fornec-f AT ROW 4.79 COL 52 COLON-ALIGNED NO-LABEL
     c-comprado-i AT ROW 5.79 COL 23 COLON-ALIGNED
     c-comprado-f AT ROW 5.79 COL 52 COLON-ALIGNED NO-LABEL
     i-nr-processo-i AT ROW 6.79 COL 23 COLON-ALIGNED
     i-nr-processo-f AT ROW 6.79 COL 52 COLON-ALIGNED NO-LABEL
     i-pedido-i AT ROW 7.79 COL 23 COLON-ALIGNED
     i-pedido-f AT ROW 7.79 COL 52 COLON-ALIGNED NO-LABEL
     da-iniper AT ROW 8.79 COL 23 COLON-ALIGNED
     da-fimper AT ROW 8.79 COL 52 COLON-ALIGNED NO-LABEL
     i-contrato-i AT ROW 9.79 COL 23 COLON-ALIGNED HELP
          "Permite informar o numero do contrato de fornecimento" WIDGET-ID 2
     i-contrato-f AT ROW 9.79 COL 52 COLON-ALIGNED HELP
          "Permite informar o numero do contrato de fornecimento" NO-LABEL WIDGET-ID 4
     IMAGE-10 AT ROW 1.71 COL 47
     IMAGE-9 AT ROW 1.71 COL 51
     IMAGE-5 AT ROW 2.71 COL 47
     IMAGE-2 AT ROW 2.71 COL 51
     IMAGE-12 AT ROW 3.71 COL 47
     IMAGE-11 AT ROW 3.71 COL 51
     IMAGE-14 AT ROW 4.71 COL 47
     IMAGE-13 AT ROW 4.71 COL 51
     IMAGE-16 AT ROW 5.71 COL 47
     IMAGE-15 AT ROW 5.71 COL 51
     IMAGE-20 AT ROW 6.71 COL 47
     IMAGE-19 AT ROW 6.71 COL 51
     IMAGE-22 AT ROW 7.71 COL 47
     IMAGE-21 AT ROW 7.71 COL 51
     IMAGE-24 AT ROW 8.71 COL 47
     IMAGE-23 AT ROW 8.71 COL 51
     IMAGE-25 AT ROW 9.79 COL 47 WIDGET-ID 8
     IMAGE-26 AT ROW 9.79 COL 51 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76 BY 10.

DEFINE FRAME f-pg-par
     l-lista-narra-item AT ROW 1.04 COL 3.14
     l-ped-emitido AT ROW 1.04 COL 38.14
     l-lista-narra-pedido AT ROW 2.04 COL 3.14
     l-ped-aprovado AT ROW 2.04 COL 38.14
     l-lista-narra-ordem AT ROW 3.04 COL 3.14
     i-tipo-moeda AT ROW 3.79 COL 38.14 COLON-ALIGNED NO-LABEL
     c-descri-moeda AT ROW 3.79 COL 45.14 COLON-ALIGNED NO-LABEL
     l-lista-texto-livre AT ROW 5.04 COL 6.14
     l-medicao AT ROW 5.75 COL 40 WIDGET-ID 6
     c-tipo-texto AT ROW 6.04 COL 17.29 COLON-ALIGNED
     l-ordem AT ROW 6.54 COL 40 WIDGET-ID 8
     l-programacao AT ROW 7.33 COL 40 WIDGET-ID 10
     tg-despesas-imp AT ROW 8.67 COL 6.14
     rs-despesas-pag AT ROW 8.67 COL 30.86 NO-LABEL
     tg-despesas-inc AT ROW 9.67 COL 31
     text2 AT ROW 3.04 COL 37.14 COLON-ALIGNED NO-LABEL
     text1 AT ROW 4.21 COL 43.29 COLON-ALIGNED NO-LABEL
     text3 AT ROW 4.29 COL 2.14 COLON-ALIGNED NO-LABEL
     text-3 AT ROW 5.04 COL 37.14 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     c-lb-imp AT ROW 7.96 COL 3.14 COLON-ALIGNED NO-LABEL
     rt-texto AT ROW 4.63 COL 3.14
     rt-moeda AT ROW 3.38 COL 38.14
     rt-imp AT ROW 8.42 COL 3.57
     rt-moeda-2 AT ROW 5.38 COL 38.14 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76 BY 10.

DEFINE FRAME f-pg-cla
     i-classifica AT ROW 1.5 COL 3 HELP
          "Classifica‡Æo para emissÆo do relat¢rio" NO-LABEL
     i-nome-abrev AT ROW 2.33 COL 38 NO-LABEL
     l-salto-pagina AT ROW 4.75 COL 38
     text5 AT ROW 1.25 COL 35 COLON-ALIGNED NO-LABEL
     RECT-10 AT ROW 1.54 COL 36
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76 BY 10.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Rela‡Æo de Entregas Previstas"
         HEIGHT             = 15.04
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
/* SETTINGS FOR FILL-IN text5 IN FRAME f-pg-cla
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text5:PRIVATE-DATA IN FRAME f-pg-cla     = 
                "Fornecedor".

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

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR FILL-IN c-descri-moeda IN FRAME f-pg-par
   NO-ENABLE                                                            */
ASSIGN 
       c-lb-imp:HIDDEN IN FRAME f-pg-par           = TRUE.

ASSIGN 
       rs-despesas-pag:HIDDEN IN FRAME f-pg-par           = TRUE.

ASSIGN 
       rt-imp:HIDDEN IN FRAME f-pg-par           = TRUE.

/* SETTINGS FOR FILL-IN text-3 IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-3:PRIVATE-DATA IN FRAME f-pg-par     = 
                "Tipo de Controle".

/* SETTINGS FOR FILL-IN text1 IN FRAME f-pg-par
   NO-ENABLE                                                            */
ASSIGN 
       text1:PRIVATE-DATA IN FRAME f-pg-par     = 
                "-".

/* SETTINGS FOR FILL-IN text2 IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text2:PRIVATE-DATA IN FRAME f-pg-par     = 
                "Op‡Æo de Moeda".

/* SETTINGS FOR FILL-IN text3 IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text3:PRIVATE-DATA IN FRAME f-pg-par     = 
                "Texto".

ASSIGN 
       tg-despesas-imp:HIDDEN IN FRAME f-pg-par           = TRUE.

ASSIGN 
       tg-despesas-inc:HIDDEN IN FRAME f-pg-par           = TRUE.

/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FILL-IN c-estabel-f IN FRAME f-pg-sel
   LIKE = mgadm.estabelec.cod-estabel EXP-SIZE                          */
/* SETTINGS FOR FILL-IN c-estabel-i IN FRAME f-pg-sel
   LIKE = mgadm.estabelec.cod-estabel EXP-SIZE                          */
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
     _TblList          = "mgadm.estabelec,mgind.item OF mgadm.estabelec,mgadm.emitente OF mgadm.estabelec,mgind.ordem-compra OF mgadm.estabelec,ems2cademp.contrato-for OF mgadm.estabelec"
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Rela‡Æo de Entregas Previstas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Rela‡Æo de Entregas Previstas */
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
&Scoped-define SELF-NAME c-tipo-texto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-tipo-texto C-Win
ON F5 OF c-tipo-texto IN FRAME f-pg-par /* Tipo de Texto */
DO:
    {include/zoomvar.i &prog-zoom="inzoom/z01in437.w"
                       &campo=c-tipo-texto
                       &campozoom=tipo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-tipo-texto C-Win
ON MOUSE-SELECT-DBLCLICK OF c-tipo-texto IN FRAME f-pg-par /* Tipo de Texto */
DO:
    apply 'F5' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-cla
&Scoped-define SELF-NAME i-classifica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-classifica C-Win
ON VALUE-CHANGED OF i-classifica IN FRAME f-pg-cla
DO:
    assign i-nome-abrev:sensitive   = (if  input frame f-pg-cla i-classifica = 4 then yes else no)
           l-salto-pagina:sensitive = (if  input frame f-pg-cla i-classifica = 4 then yes else no).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME i-tipo-moeda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-tipo-moeda C-Win
ON F5 OF i-tipo-moeda IN FRAME f-pg-par
DO:
    {include/zoomvar.i &prog-zoom="adzoom/z01ad178.w"
                       &campo=i-tipo-moeda
                       &campozoom=mo-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-tipo-moeda C-Win
ON LEAVE OF i-tipo-moeda IN FRAME f-pg-par
DO:
    {include/leave.i &tabela=moeda
                     &atributo-ref=descricao
                     &variavel-ref=c-descri-moeda
                     &where="moeda.mo-codigo = input frame f-pg-par i-tipo-moeda"}
    assign c-descri-moeda.
    disp c-descri-moeda with frame f-pg-par.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-tipo-moeda C-Win
ON MOUSE-SELECT-DBLCLICK OF i-tipo-moeda IN FRAME f-pg-par
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
    apply "value-changed" to i-classifica in frame f-pg-cla.
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
    apply "value-changed" to l-lista-texto-livre in frame f-pg-par.
    apply "leave"         to i-tipo-moeda        in frame f-pg-par.
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
&Scoped-define SELF-NAME l-lista-texto-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-lista-texto-livre C-Win
ON VALUE-CHANGED OF l-lista-texto-livre IN FRAME f-pg-par /* Texto Livre do Item */
DO:
    assign c-tipo-texto:sensitive = l-lista-texto-livre:checked.
    if  l-lista-texto-livre:checked in frame f-pg-par = no then
        assign c-tipo-texto:screen-value in frame f-pg-par = "".

    if  i-tipo-moeda:load-mouse-pointer ("image/lupa.cur") in frame f-pg-par then.
    if  c-tipo-texto:load-mouse-pointer ("image/lupa.cur") in frame f-pg-par then.
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
&Scoped-define SELF-NAME tg-despesas-imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-despesas-imp C-Win
ON VALUE-CHANGED OF tg-despesas-imp IN FRAME f-pg-par /* Despesas Importa‡Æo */
DO:
    if tg-despesas-imp:checked in frame f-pg-par then
        assign rs-despesas-pag:sensitive in frame f-pg-par = yes
               tg-despesas-inc:sensitive in frame f-pg-par = yes.
    else
        assign rs-despesas-pag:sensitive in frame f-pg-par = no
               tg-despesas-inc:sensitive in frame f-pg-par = no.
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

{utp/ut9000.i "escc0407" "2.06.00.000"}

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

    find first param-global NO-LOCK.

    if  i-tipo-moeda:load-mouse-pointer ("image/lupa.cur") in frame f-pg-par then.
    if  c-tipo-texto:load-mouse-pointer ("image/lupa.cur") in frame f-pg-par then.

    IF  NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* Integracao Modulo Importacao */
find first param-global no-lock no-error.
if avail param-global and param-global.modulo-07 then
    assign c-lb-imp:hidden in frame f-pg-par        = no
           rt-imp:hidden in frame f-pg-par          = no
           tg-despesas-imp:hidden in frame f-pg-par = no
           rs-despesas-pag:hidden in frame f-pg-par = no
           tg-despesas-inc:hidden in frame f-pg-par = no
           rs-despesas-pag:sensitive in frame f-pg-par  = no
           tg-despesas-inc:sensitive in frame f-pg-par = no.
else
    assign c-lb-imp:hidden in frame f-pg-par        = yes
           rt-imp:hidden in frame f-pg-par          = yes
           tg-despesas-imp:hidden in frame f-pg-par = yes
           rs-despesas-pag:hidden in frame f-pg-par = yes
           tg-despesas-inc:hidden in frame f-pg-par = yes.

&IF defined (bf_mat_aprova_eletronica) &THEN   
find first param-aprov no-lock no-error.
if avail param-aprov then do:
    if param-aprov.aprova-pedido 
       or param-aprov.aprova-emerg THEN 
       assign l-ped-aprovado:sensitive in frame f-pg-par = yes.
    else 
       assign l-ped-aprovado:sensitive in frame f-pg-par = no.
end.
&else
find first param-compra no-lock no-error.
if avail param-compra then do:
    if substring(param-compra.char-2, 7, 3) = "sim":U 
        or substring(param-compra.char-1,101,1) = "Y":U then
        assign l-ped-aprovado:sensitive in frame f-pg-par = yes.
    else 
        assign l-ped-aprovado:sensitive in frame f-pg-par = no.
end.
&endif 

{utp/ut-liter.i Estabelecimento * R}
ASSIGN c-estabel-i:LABEL IN FRAME f-pg-sel = TRIM(RETURN-VALUE).    

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
  ENABLE im-pg-sel im-pg-cla im-pg-par im-pg-imp bt-executar bt-cancelar 
         bt-ajuda 
      WITH FRAME f-relat IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY i-classifica i-nome-abrev l-salto-pagina 
      WITH FRAME f-pg-cla IN WINDOW C-Win.
  ENABLE RECT-10 i-nome-abrev l-salto-pagina 
      WITH FRAME f-pg-cla IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-cla}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  ENABLE RECT-7 RECT-9 rs-destino bt-arquivo bt-config-impr c-arquivo 
         rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY l-lista-narra-item l-ped-emitido l-lista-narra-pedido l-ped-aprovado 
          l-lista-narra-ordem i-tipo-moeda c-descri-moeda l-lista-texto-livre 
          l-medicao c-tipo-texto l-ordem l-programacao tg-despesas-imp 
          rs-despesas-pag tg-despesas-inc text1 c-lb-imp 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  ENABLE l-lista-narra-item l-ped-emitido rt-texto rt-moeda rt-imp rt-moeda-2 
         l-lista-narra-pedido l-ped-aprovado l-lista-narra-ordem i-tipo-moeda 
         l-lista-texto-livre l-medicao c-tipo-texto l-ordem l-programacao 
         tg-despesas-imp rs-despesas-pag tg-despesas-inc c-lb-imp 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY c-estabel-i c-estabel-f c-item-i c-item-f i-gr-fornec-i i-gr-fornec-f 
          i-fornec-i i-fornec-f c-comprado-i c-comprado-f i-nr-processo-i 
          i-nr-processo-f i-pedido-i i-pedido-f da-iniper da-fimper i-contrato-i 
          i-contrato-f 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE IMAGE-10 IMAGE-9 IMAGE-5 IMAGE-2 IMAGE-12 IMAGE-11 IMAGE-14 IMAGE-13 
         IMAGE-16 IMAGE-15 IMAGE-20 IMAGE-19 IMAGE-22 IMAGE-21 IMAGE-24 
         IMAGE-23 IMAGE-25 IMAGE-26 c-estabel-i c-estabel-f c-item-i c-item-f 
         i-gr-fornec-i i-gr-fornec-f i-fornec-i i-fornec-f c-comprado-i 
         c-comprado-f i-nr-processo-i i-nr-processo-f i-pedido-i i-pedido-f 
         da-iniper da-fimper i-contrato-i i-contrato-f 
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
            apply 'entry'              to c-arquivo in frame f-pg-imp.
            return error.
        end.
    end.

    /* Coloque aqui as valida‡äes das outras p ginas, lembrando que elas
       devem apresentar uma mensagem de erro cadastrada, posicionar na p gina 
       com problemas e colocar o focus no campo com problemas             */    

    create tt-param.

    /* Verifica PermissÆo do Usu rio */
    if  input frame f-pg-par l-lista-texto-livre = yes then do:
        find tipo-texto
            where tipo-texto.tipo = input frame f-pg-par c-tipo-texto
            no-lock no-error.
        if  avail tipo-texto then do:
            run utp/ut-vlusr.p (input tipo-texto.permis-leit,
                                input l-perm-texto).
            assign tt-param.l-perm-texto = if return-value = 'no' then no else yes.
            if  return-value = 'no' then do:
                run utp/ut-msgs.p (input "show", input 2157, input tipo-texto.tipo).
                apply 'mouse-select-click' to im-pg-par    in frame f-relat.
                apply 'entry'              to c-tipo-texto in frame f-pg-par.
                return error.
            end.
        end.
        else do:
            {utp/ut-liter.i Tipo_de_Texto * r}
            assign c-lb-texto = trim(return-value).
            run utp/ut-msgs.p (input "show", input 56, input c-lb-texto).
            apply 'mouse-select-click' to im-pg-par    in frame f-relat.
            apply 'entry'              to c-tipo-texto in frame f-pg-par.
            return error.
        end.
    end.

    if  input frame f-pg-par i-tipo-moeda <> 0 then do:
        find moeda
            where moeda.mo-codigo = input frame f-pg-par i-tipo-moeda
            no-lock no-error.
        if  not avail moeda then do:
            run utp/ut-msgs.p (input "show", input 56, input "Moeda").
            apply 'mouse-select-click' to im-pg-par    in frame f-relat.
            apply 'entry'              to i-tipo-moeda in frame f-pg-par.
            return error.
        end.
    end.

    assign tt-param.usuario              = c-seg-usuario
           tt-param.destino              = input frame f-pg-imp rs-destino
           tt-param.data-exec            = today
           tt-param.hora-exec            = time
           tt-param.i-classifica         = input frame f-pg-cla i-classifica
           tt-param.i-nome-abrev         = input frame f-pg-cla i-nome-abrev
           tt-param.l-salto-pagina       = input frame f-pg-cla l-salto-pagina
           tt-param.c-estabel-i          = input frame f-pg-sel c-estabel-i
           tt-param.c-estabel-f          = input frame f-pg-sel c-estabel-f
           tt-param.c-item-i             = input frame f-pg-sel c-item-i
           tt-param.c-item-f             = input frame f-pg-sel c-item-f
           tt-param.i-gr-fornec-i        = input frame f-pg-sel i-gr-fornec-i
           tt-param.i-gr-fornec-f        = input frame f-pg-sel i-gr-fornec-f
           tt-param.i-fornec-i           = input frame f-pg-sel i-fornec-i
           tt-param.i-fornec-f           = input frame f-pg-sel i-fornec-f
           tt-param.c-comprado-i         = input frame f-pg-sel c-comprado-i
           tt-param.c-comprado-f         = input frame f-pg-sel c-comprado-f
           tt-param.i-nr-processo-i      = input frame f-pg-sel i-nr-processo-i
           tt-param.i-nr-processo-f      = input frame f-pg-sel i-nr-processo-f
           tt-param.i-pedido-i           = input frame f-pg-sel i-pedido-i
           tt-param.i-pedido-f           = input frame f-pg-sel i-pedido-f

           tt-param.i-contrato-i         = input frame f-pg-sel i-contrato-i
           tt-param.i-contrato-f         = input frame f-pg-sel i-contrato-f

           tt-param.da-iniper            = input frame f-pg-sel da-iniper
           tt-param.da-fimper            = input frame f-pg-sel da-fimper
           tt-param.l-lista-narra-item   = input frame f-pg-par l-lista-narra-item
           tt-param.l-lista-narra-pedido = input frame f-pg-par l-lista-narra-pedido
           tt-param.l-lista-narra-ordem  = input frame f-pg-par l-lista-narra-ordem
           tt-param.l-lista-texto-livre  = input frame f-pg-par l-lista-texto-livre
           tt-param.c-tipo-texto         = input frame f-pg-par c-tipo-texto
           tt-param.c-descri-moeda       = input frame f-pg-par c-descri-moeda
           tt-param.i-tipo-moeda         = input frame f-pg-par i-tipo-moeda
           tt-param.l-ped-emitido        = input frame f-pg-par l-ped-emitido
           tt-param.l-ped-aprovado       = input frame f-pg-par l-ped-aprovado
           tt-param.l-medicao            = INPUT FRAME f-pg-par l-medicao
           tt-param.l-ordem              = INPUT FRAME f-pg-par l-ordem
           tt-param.l-programacao        = INPUT FRAME f-pg-par l-programacao
           tt-param.c-param              = entry((tt-param.i-nome-abrev - 1) * 2 + 1,
                                                 i-nome-abrev:radio-buttons in frame f-pg-cla)
           tt-param.c-classe             = entry((tt-param.i-classifica - 1) * 2 + 1,
                                                  i-classifica:radio-buttons in frame f-pg-cla)
           tt-param.c-destino            = entry((tt-param.destino - 1) * 2 + 1,
                                                  rs-destino:radio-buttons in frame f-pg-imp)
           tt-param.l-despesas-imp       = input frame f-pg-par tg-despesas-imp  /* Importa‡Æo */
           tt-param.i-despesas-pag       = input frame f-pg-par rs-despesas-pag  /* Importa‡Æo */
           tt-param.l-despesas-inc       = input frame f-pg-par tg-despesas-inc. /* Importa‡Æo */                                                  .

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

    {include/i-rprun.i esp/escc0407rp.p}

    {include/i-rpexc.i}

    if  session:set-wait-state("") then.

/*     {include/i-rptrm.i} */

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
  {src/adm/template/snd-list.i "estabelec"}
  {src/adm/template/snd-list.i "item"}
  {src/adm/template/snd-list.i "emitente"}
  {src/adm/template/snd-list.i "ordem-compra"}
  {src/adm/template/snd-list.i "contrato-for"}

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


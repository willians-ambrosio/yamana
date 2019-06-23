&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME wReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wReport 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i yg0006 12.1.17.000}  /*** 010013 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i yg0006 MCE}
&ENDIF

/*:T*******************************************************************************
** Copyright DATASUL S.A. (1999)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

CREATE WIDGET-POOL.

/* Preprocessors Definitions ---                                      */
&GLOBAL-DEFINE Program        yg0006
&GLOBAL-DEFINE Version        12.1.17.000
/*&GLOBAL-DEFINE VersionLayout  2.00.00.001*/

&GLOBAL-DEFINE Folder         YES
&GLOBAL-DEFINE InitialPage    1
&GLOBAL-DEFINE FolderLabels   Seleá∆o,ParÉmetros,Classificaá∆o,Digitaá∆o,Impress∆o

&GLOBAL-DEFINE PGLAY          NO
&GLOBAL-DEFINE PGSEL          YES
&GLOBAL-DEFINE PGCLA          YES
&GLOBAL-DEFINE PGPAR          YES
&GLOBAL-DEFINE PGDIG          YES
&GLOBAL-DEFINE PGIMP          YES
&GLOBAL-DEFINE PGLOG          NO

&GLOBAL-DEFINE RTF            YES

&GLOBAL-DEFINE page0Widgets   btOk ~
                              btCancel ~
                              btHelp2
&GLOBAL-DEFINE page2Widgets   
&GLOBAL-DEFINE page3Widgets   rs-tipo-custo cb-moeda  tg-ativo tg-obsol-ord-aut ~
                              tg-Obsol-todas-ordens tg-obsol-total
&GLOBAL-DEFINE page4Widgets   rsClassif
&GLOBAL-DEFINE page5Widgets   brDigita ~
                              btAdd ~
                              btUpdate ~
                              btDelete ~
                              btSave 
&GLOBAL-DEFINE page6Widgets   rsDestiny ~
                              btConfigImpr ~
                              btFile ~
                              rsExecution ~
                              l-habilitaRtf ~
                              btModelRtf

&GLOBAL-DEFINE page0Text      
&GLOBAL-DEFINE page2Text      
&GLOBAL-DEFINE page3Text      
&GLOBAL-DEFINE page4Text      
&GLOBAL-DEFINE page5Text      
&GLOBAL-DEFINE page6Text      text-destino text-modo text-rtf text-ModelRtf

&GLOBAL-DEFINE page2Fields    c-item-ini c-item-fim c-cod-estabel-ini c-cod-estabel-fim ~
                              dt-data-trans-ini dt-data-trans-fim c-cod-refer-ini c-cod-refer-fim ~
                              c-cod-depos-ini c-cod-depos-fim c-lote-ini c-lote-fim ~
                              c-cod-localiz-ini c-cod-localiz-fim /*cb-especie-ini cb-especie-fim*/ ~
                              c-serie-docto-ini c-serie-docto-fim c-nro-docto-ini c-nro-docto-fim c-unid-neg-ini c-unid-neg-fim
&GLOBAL-DEFINE page3Fields    
&GLOBAL-DEFINE page4Fields    
&GLOBAL-DEFINE page5Fields    
&GLOBAL-DEFINE page6Fields    cFile cModelRTF

/* Parameters Definitions ---                                           */
{esp/yg0006.i}  /* Definiá‰es da tt-param */
{cdp/cd0019.i "MCE"} /*Seguranáa por Estabelecimento*/

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita NO-UNDO
   field raw-digita      as raw.

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-rtf              as char    no-undo.
def var c-arq-layout       as char    no-undo.      
def var c-arq-temp         as char    no-undo.
DEF VAR c-modelo-default   AS CHAR    NO-UNDO.

DEF VAR p-moeda            AS CHAR    NO-UNDO.
DEF VAR i-cod-moeda        AS INTEGER NO-UNDO.
DEF VAR c-descricao-moeda  AS CHAR    NO-UNDO.
DEF VAR c-lista            AS CHAR    NO-UNDO.
DEF VAR c-lista-aux        AS CHAR    NO-UNDO.
DEF VAR i-x                AS INTEGER NO-UNDO.
DEF VAR c-desc-estabel     AS CHAR    NO-UNDO.
DEFINE VARIABLE h-boun005  AS HANDLE  NO-UNDO.
DEFINE VARIABLE h-boad107  AS HANDLE  NO-UNDO.
def var ch-excel           as com-handle no-undo.

/* handle do programa de procedures */
DEFINE NEW GLOBAL SHARED VARIABLE adm-broker-hdl AS HANDLE NO-UNDO.
/*DEFINE                   VARIABLE wh-pesquisa    AS HANDLE NO-UNDO.*/


def stream s-imp.

/*15/02/2005 - tech1007 - Variavel definida para tratar se o programa est† rodando no WebEnabler*/
DEFINE SHARED VARIABLE hWenController AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fpage0
&Scoped-define BROWSE-NAME brDigita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-digita

/* Definitions for BROWSE brDigita                                      */
&Scoped-define FIELDS-IN-QUERY-brDigita tt-digita.selecionado tt-digita.esp-docto tt-digita.descricao   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brDigita tt-digita.esp-docto   
&Scoped-define ENABLED-TABLES-IN-QUERY-brDigita tt-digita
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brDigita tt-digita
&Scoped-define SELF-NAME brDigita
&Scoped-define QUERY-STRING-brDigita FOR EACH tt-digita
&Scoped-define OPEN-QUERY-brDigita OPEN QUERY brDigita FOR EACH tt-digita.
&Scoped-define TABLES-IN-QUERY-brDigita tt-digita
&Scoped-define FIRST-TABLE-IN-QUERY-brDigita tt-digita


/* Definitions for FRAME fPage5                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fPage5 ~
    ~{&OPEN-QUERY-brDigita}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rtToolBar btOK btCancel btHelp2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wReport AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON btHelp2 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON btOK 
     LABEL "Executar" 
     SIZE 10 BY 1.

DEFINE RECTANGLE rtToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE cb-especie-fim AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cb-especie-ini AS CHARACTER FORMAT "X(8)":U 
     LABEL "EspÇcie" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE c-cod-depos-fim AS CHARACTER FORMAT "x(3)" INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE c-cod-depos-ini AS CHARACTER FORMAT "x(3)" 
     LABEL "Dep¢sito":R10 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88
     FONT 1 NO-UNDO.

&IF "{&mguni_version}" >= "2.071" &THEN
DEFINE VARIABLE c-cod-estabel-fim AS CHARACTER format "x(05)" INITIAL "ZZZZZ" 
&ELSE
DEFINE VARIABLE c-cod-estabel-fim AS CHARACTER FORMAT "X(3)" INITIAL "ZZZ" 
&ENDIF
     VIEW-AS FILL-IN 
     SIZE 5 BY .88
     FONT 1 NO-UNDO.

&IF "{&mguni_version}" >= "2.071" &THEN
DEFINE VARIABLE c-cod-estabel-ini AS CHARACTER format "x(05)" 
&ELSE
DEFINE VARIABLE c-cod-estabel-ini AS CHARACTER FORMAT "X(3)" 
&ENDIF
     LABEL "Estabelecimento":R15 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE c-cod-localiz-fim AS CHARACTER FORMAT "x(10)" INITIAL "ZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE c-cod-localiz-ini AS CHARACTER FORMAT "x(10)" 
     LABEL "Localizaá∆o":R14 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE c-cod-refer-fim AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE c-cod-refer-ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Referància":R12 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE c-item-fim AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE c-item-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Item":R5 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE c-lote-fim AS CHARACTER FORMAT "x(10)" INITIAL "ZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE c-lote-ini AS CHARACTER FORMAT "x(10)" 
     LABEL "Lote":R12 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE c-nro-docto-fim AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE c-nro-docto-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Documento":R11 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE c-serie-docto-fim AS CHARACTER FORMAT "x(5)" INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE c-serie-docto-ini AS CHARACTER FORMAT "x(5)" 
     LABEL "SÇrie" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE c-unid-neg-fim AS CHARACTER FORMAT "X(3)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE c-unid-neg-ini AS CHARACTER FORMAT "X(3)":U 
     LABEL "Unidade de Neg¢cio" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE dt-data-trans-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE dt-data-trans-ini AS DATE FORMAT "99/99/9999":U INITIAL &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 01/01/001  &ENDIF 
     LABEL "Data Transaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     FONT 1 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image~\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-10
     FILENAME "image~\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image~\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image~\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-13
     FILENAME "image~\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image~\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-15
     FILENAME "image~\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-16
     FILENAME "image~\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-17
     FILENAME "image~\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-18
     FILENAME "image~\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-19
     FILENAME "image~\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image~\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-20
     FILENAME "image~\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-29
     FILENAME "image~\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image~\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-30
     FILENAME "image~\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image~\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image~\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image~\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image~\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image~\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image~\im-fir":U
     SIZE 3 BY .88.

DEFINE VARIABLE cb-moeda AS CHARACTER FORMAT "X(256)":U 
     LABEL "Moeda" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE rs-tipo-custo AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Mensal", 1,
"On Line", 2,
"Padr∆o", 3
     SIZE 23 BY 3 NO-UNDO.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27 BY 4.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27 BY 4.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27 BY 4.5.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27 BY 4.5.

DEFINE VARIABLE tg-ativo AS LOGICAL INITIAL yes 
     LABEL "Ativo" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-obsol-ord-aut AS LOGICAL INITIAL no 
     LABEL "Obsoleto Ordens Autom†ticas" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .83 NO-UNDO.

DEFINE VARIABLE tg-Obsol-todas-ordens AS LOGICAL INITIAL no 
     LABEL "Obsoleto Todas as Ordens" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .83 NO-UNDO.

DEFINE VARIABLE tg-obsol-total AS LOGICAL INITIAL no 
     LABEL "Totalmente Obsoleto" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .83 NO-UNDO.

DEFINE VARIABLE rsClassif AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Item / Estabelecimento / Data", 1,
"Item / Data", 2,
"Item / Estab. / Dep¢sito / Localizaá∆o / Lote / Ref. / Data", 3
     SIZE 44.86 BY 4
     FONT 1 NO-UNDO.

DEFINE BUTTON btAdd 
     LABEL "Marca" 
     SIZE 15 BY 1
     FONT 1.

DEFINE BUTTON btDelete 
     LABEL "Marca Todos" 
     SIZE 15 BY 1
     FONT 1.

DEFINE BUTTON btSave 
     LABEL "Desmarca Todos" 
     SIZE 15 BY 1
     FONT 1.

DEFINE BUTTON btUpdate 
     LABEL "Desmarca" 
     SIZE 15 BY 1
     FONT 1.

DEFINE BUTTON btConfigImpr 
     IMAGE-UP FILE "image~\im-cfprt":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON btFile 
     IMAGE-UP FILE "image~\im-sea":U
     IMAGE-INSENSITIVE FILE "image~\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON btModelRtf 
     IMAGE-UP FILE "image~\im-sea":U
     IMAGE-INSENSITIVE FILE "image~\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE cFile AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cModelRTF AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.14 BY .63
     FONT 1 NO-UNDO.

DEFINE VARIABLE text-ModelRtf AS CHARACTER FORMAT "X(256)":U INITIAL "Modelo:" 
      VIEW-AS TEXT 
     SIZE 10 BY .67 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execuá∆o" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63
     FONT 1 NO-UNDO.

DEFINE VARIABLE text-rtf AS CHARACTER FORMAT "X(256)":U INITIAL "Rich Text Format(RTF)" 
      VIEW-AS TEXT 
     SIZE 16 BY .67 NO-UNDO.

DEFINE VARIABLE rsDestiny AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3,
"Excel", 4
     SIZE 44 BY 1.08
     FONT 1 NO-UNDO.

DEFINE VARIABLE rsExecution AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
"Batch", 2
     SIZE 27.86 BY .92
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.14 BY 1.25.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 45.86 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.14 BY 1.71.

DEFINE RECTANGLE rect-rtf
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.14 BY 3.21.

DEFINE VARIABLE l-habilitaRtf AS LOGICAL INITIAL no 
     LABEL "RTF" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY 1.08
     FONT 1 NO-UNDO.

DEFINE VARIABLE l-imp-param AS LOGICAL INITIAL yes 
     LABEL "Imprime P†gina ParÉmetros" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .83 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brDigita FOR 
      tt-digita SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brDigita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brDigita wReport _FREEFORM
  QUERY brDigita DISPLAY
    tt-digita.selecionado  label "*"
      tt-digita.esp-docto  label "EspÇcie"
tt-digita.descricao                label "Descriá∆o"
/* ENABLE              */
/* tt-digita.esp-docto */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 82 BY 8.75
         BGCOLOR 15 FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fpage0
     btOK AT ROW 16.75 COL 2
     btCancel AT ROW 16.75 COL 13
     btHelp2 AT ROW 16.75 COL 80
     rtToolBar AT ROW 16.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17.04
         FONT 1.

DEFINE FRAME fPage4
     rsClassif AT ROW 1.29 COL 2.14 HELP
          "Classificaá∆o para emiss∆o do relat¢rio" NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.81
         SIZE 84.43 BY 10.15
         FONT 1.

DEFINE FRAME fPage5
     brDigita AT ROW 1.25 COL 1
     btAdd AT ROW 10 COL 1
     btUpdate AT ROW 10 COL 16
     btDelete AT ROW 10 COL 31
     btSave AT ROW 10 COL 46

    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.81
         SIZE 84.43 BY 10.15
         FONT 1.

DEFINE FRAME fPage6
     rsDestiny AT ROW 2 COL 3.14 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     btFile AT ROW 3.13 COL 43 HELP
          "Escolha do nome do arquivo"
     cFile AT ROW 3.25 COL 3.14 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     btConfigImpr AT ROW 3.5 COL 43 HELP
          "Configuraá∆o da impressora"
     l-habilitaRtf AT ROW 5.21 COL 3.14
     cModelRTF AT ROW 6.92 COL 3 HELP
          "Nome do arquivo de modelo" NO-LABEL
     btModelRtf AT ROW 6.92 COL 43 HELP
          "Escolha o arquivo de modelo"
     rsExecution AT ROW 9.13 COL 2.86 HELP
          "Modo de Execuá∆o" NO-LABEL
     l-imp-param AT ROW 10.75 COL 3
     text-destino AT ROW 1.25 COL 1.86 COLON-ALIGNED NO-LABEL
     text-rtf AT ROW 4.63 COL 2 COLON-ALIGNED NO-LABEL
     text-ModelRtf AT ROW 6.17 COL 2 COLON-ALIGNED NO-LABEL
     text-modo AT ROW 8.38 COL 1.14 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.54 COL 2.14
     RECT-9 AT ROW 8.63 COL 2
     rect-rtf AT ROW 4.92 COL 2
     RECT-18 AT ROW 10.5 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.79
         SIZE 84.43 BY 10.96
         FONT 1.

DEFINE FRAME fPage3
     rs-tipo-custo AT ROW 2 COL 3 NO-LABEL
     cb-moeda AT ROW 2 COL 44.86 COLON-ALIGNED
     tg-ativo AT ROW 6.67 COL 4
     tg-obsol-ord-aut AT ROW 7.67 COL 4
     tg-Obsol-todas-ordens AT ROW 8.67 COL 4
     tg-obsol-total AT ROW 9.67 COL 4
     "C¢digo Obsoleto" VIEW-AS TEXT
          SIZE 12 BY .54 AT ROW 5.96 COL 3.29
     "Tipo de Custo" VIEW-AS TEXT
          SIZE 11.72 BY .54 AT ROW 1.25 COL 3.29
     RECT-13 AT ROW 1.5 COL 2
     RECT-14 AT ROW 1.5 COL 38
     RECT-15 AT ROW 6.25 COL 2
     RECT-16 AT ROW 6.25 COL 38
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.81
         SIZE 84.43 BY 10.15
         FONT 1.

DEFINE FRAME fPage2
     c-item-ini AT ROW 1.5 COL 16.86 HELP
          "C¢digo do Item"
     c-item-fim AT ROW 1.5 COL 54.14 HELP
          "C¢digo do Item" NO-LABEL
     c-cod-estabel-ini AT ROW 2.5 COL 9 HELP
          "C¢digo do estabelecimento"
     c-cod-estabel-fim AT ROW 2.5 COL 54.14 HELP
          "C¢digo do estabelecimento" NO-LABEL
     dt-data-trans-ini AT ROW 3.5 COL 9.14 HELP
          "Data Transaá∆o"
     dt-data-trans-fim AT ROW 3.5 COL 54.14 HELP
          "Data Transaá∆o" NO-LABEL
     c-cod-refer-ini AT ROW 4.5 COL 11.86 HELP
          "Referància"
     c-cod-refer-fim AT ROW 4.5 COL 54.14 HELP
          "Referància" NO-LABEL
     c-cod-depos-ini AT ROW 5.5 COL 13.28 HELP
          "Dep¢sito"
     c-cod-depos-fim AT ROW 5.5 COL 54.14 HELP
          "Dep¢sito" NO-LABEL
     c-lote-ini AT ROW 6.5 COL 13.71 HELP
          "Lote"
     c-lote-fim AT ROW 6.5 COL 54.14 HELP
          "Lote" NO-LABEL
     c-cod-localiz-ini AT ROW 7.5 COL 10.71 HELP
          "Localizaá∆o"
     c-cod-localiz-fim AT ROW 7.5 COL 54.14 HELP
          "Localizaá∆o" NO-LABEL
/*      cb-especie-ini AT ROW 8.5 COL 14.71 HELP */
/*           "EspÇcie"                           */
/*      cb-especie-fim AT ROW 8.5 COL 54.14 HELP */
/*           "EspÇcie" NO-LABEL                  */
     c-serie-docto-ini AT ROW 8.5 COL 16.71 HELP
          "SÇrie"
     c-serie-docto-fim AT ROW 8.5 COL 54.14 HELP
          "SÇrie" NO-LABEL
     c-nro-docto-ini AT ROW 9.5 COL 11.43 HELP
          "N£mero do Documento"
     c-nro-docto-fim AT ROW 9.5 COL 54.14 HELP
          "N£mero do Documento" NO-LABEL
     c-unid-neg-ini AT ROW 10.5 COL 19 COLON-ALIGNED 
     c-unid-neg-fim AT ROW 10.5 COL 52.14 COLON-ALIGNED NO-LABEL 
     IMAGE-1 AT ROW 1.5 COL 39.57
     IMAGE-10 AT ROW 5.5 COL 51.14
     IMAGE-11 AT ROW 6.5 COL 51.14
     IMAGE-12 AT ROW 6.5 COL 39.57
     IMAGE-13 AT ROW 7.5 COL 51.14
     IMAGE-14 AT ROW 7.5 COL 39.57
     IMAGE-15 AT ROW 8.5 COL 51.14
     IMAGE-16 AT ROW 8.5 COL 39.57
     IMAGE-17 AT ROW 9.5 COL 51.14
     IMAGE-18 AT ROW 9.5 COL 39.57
     IMAGE-19 AT ROW 10.5 COL 51.14 
     IMAGE-2 AT ROW 1.5 COL 51.14
     IMAGE-20 AT ROW 10.5 COL 39.57
     IMAGE-3 AT ROW 2.5 COL 39.57
     IMAGE-4 AT ROW 2.5 COL 51.14
     IMAGE-5 AT ROW 3.5 COL 39.57
     IMAGE-6 AT ROW 3.5 COL 51.14
     IMAGE-7 AT ROW 4.5 COL 39.57
     IMAGE-8 AT ROW 4.5 COL 51.14
     IMAGE-9 AT ROW 5.5 COL 39.57
/*      IMAGE-29 AT ROW 11.5 COL 51.14 */
/*      IMAGE-30 AT ROW 11.5 COL 39.57 */
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.57 ROW 2.79
         SIZE 84.43 BY 11.71
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wReport ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 17.04
         WIDTH              = 90.43
         MAX-HEIGHT         = 28.83
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.83
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wReport 
/* ************************* Included-Libraries *********************** */

{report~/report.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wReport
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME fPage2:FRAME = FRAME fpage0:HANDLE
       FRAME fPage3:FRAME = FRAME fpage0:HANDLE
       FRAME fPage4:FRAME = FRAME fpage0:HANDLE
       FRAME fPage5:FRAME = FRAME fpage0:HANDLE
       FRAME fPage6:FRAME = FRAME fpage0:HANDLE.

/* SETTINGS FOR FRAME fpage0
   NOT-VISIBLE FRAME-NAME                                               */
/* SETTINGS FOR FRAME fPage2
                                                                        */
/* SETTINGS FOR FILL-IN c-cod-depos-fim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN c-cod-depos-ini IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN c-cod-estabel-fim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN c-cod-estabel-ini IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN c-cod-localiz-fim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN c-cod-localiz-ini IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN c-cod-refer-fim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN c-cod-refer-ini IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN c-item-fim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN c-item-ini IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN c-lote-fim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN c-lote-ini IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN c-nro-docto-fim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN c-nro-docto-ini IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN c-serie-docto-fim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN c-serie-docto-ini IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX cb-especie-fim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX cb-especie-ini IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN dt-data-trans-fim IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN dt-data-trans-ini IN FRAME fPage2
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME fPage3
                                                                        */
/* SETTINGS FOR FRAME fPage4
                                                                        */
/* SETTINGS FOR FRAME fPage5
                                                                        */
/* BROWSE-TAB brDigita 1 fPage5 */
/* SETTINGS FOR BUTTON btDelete IN FRAME fPage5
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btSave IN FRAME fPage5
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btUpdate IN FRAME fPage5
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME fPage6
                                                                        */
ASSIGN 
       btModelRtf:HIDDEN IN FRAME fPage6           = TRUE.

ASSIGN 
       cModelRTF:HIDDEN IN FRAME fPage6           = TRUE.

ASSIGN 
       l-imp-param:HIDDEN IN FRAME fPage6           = TRUE.

ASSIGN 
       RECT-18:HIDDEN IN FRAME fPage6           = TRUE.

ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME fPage6     = 
                "Destino".

ASSIGN 
       text-ModelRtf:HIDDEN IN FRAME fPage6           = TRUE
       text-ModelRtf:PRIVATE-DATA IN FRAME fPage6     = 
                "Modelo:".

ASSIGN 
       text-modo:PRIVATE-DATA IN FRAME fPage6     = 
                "Execuá∆o".

ASSIGN 
       text-rtf:HIDDEN IN FRAME fPage6           = TRUE
       text-rtf:PRIVATE-DATA IN FRAME fPage6     = 
                "Rich Text Format(RTF)".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wReport)
THEN wReport:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brDigita
/* Query rebuild information for BROWSE brDigita
     _START_FREEFORM
OPEN QUERY brDigita FOR EACH tt-digita.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brDigita */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fpage0
/* Query rebuild information for FRAME fpage0
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fpage0 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage2
/* Query rebuild information for FRAME fPage2
     _Query            is NOT OPENED
*/  /* FRAME fPage2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fPage6
/* Query rebuild information for FRAME fPage6
     _Query            is NOT OPENED
*/  /* FRAME fPage6 */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wReport wReport
ON END-ERROR OF wReport
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wReport wReport
ON WINDOW-CLOSE OF wReport
DO:
  /* This event will close the window and terminate the procedure.  */
  {report/logfin.i}  
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brDigita
&Scoped-define FRAME-NAME fPage5
&Scoped-define SELF-NAME brDigita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brDigita wReport
ON DEL OF brDigita IN FRAME fPage5
DO:
  FOR EACH tt-digita:
     ASSIGN tt-digita.selecionado = "*".
  END.

  {&OPEN-QUERY-brDigita}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brDigita wReport
ON END-ERROR OF brDigita IN FRAME fPage5
ANYWHERE 
DO:
    if  brDigita:new-row in frame fPage5 then do:
        if  avail tt-digita then
            delete tt-digita.
        if  brDigita:delete-current-row() in frame fPage5 then. 
    end.                                                               
    else do:
        get current brDigita.
        display tt-digita.selecionado
                tt-digita.esp-docto
                tt-digita.descricao with browse brDigita. 
    end.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brDigita wReport
ON ENTER OF brDigita IN FRAME fPage5
ANYWHERE
DO:
  apply 'tab' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brDigita wReport
ON MOUSE-SELECT-DBLCLICK OF brDigita IN FRAME fPage5
ANYWHERE
DO:
  IF AVAILABLE(tt-digita) THEN DO:
     IF tt-digita.selecionado = "*" THEN 
        ASSIGN tt-digita.selecionado = "".
     ELSE 
        ASSIGN tt-digita.selecionado = "*".
  END.

  {&OPEN-QUERY-brDigita}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brDigita wReport
ON INS OF brDigita IN FRAME fPage5
DO:
  ASSIGN tt-digita.selecionado = "*".
  
  {&OPEN-QUERY-brDigita}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brDigita wReport
ON OFF-END OF brDigita IN FRAME fPage5
DO:
   apply 'entry' to btAdd in frame fPage5.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brDigita wReport
ON OFF-HOME OF brDigita IN FRAME fPage5
DO:
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brDigita wReport
ON ROW-ENTRY OF brDigita IN FRAME fPage5
DO:
    /*
   /*:T trigger para inicializar campos da temp table de digitaá∆o */
   if  brDigita:new-row in frame fPage5 then do:
       assign tt-digita.descricao:screen-value in browse brDigita = string(today, "99/99/9999":U).
   end.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brDigita wReport
ON ROW-LEAVE OF brDigita IN FRAME fPage5
DO:
    /*:T ê aqui que a gravaá∆o da linha da temp-table Ç efetivada.
       PorÇm as validaá‰es dos registros devem ser feitas na procedure pi-executar,
       no local indicado pelo coment†rio */

    IF  brDigita:NUM-SELECTED-ROWS IN FRAME fPage5 > 0 THEN DO:
        RUN goToKey IN h-boad107 (INPUT (INPUT BROWSE brDigita tt-digita.esp-docto)).

        IF  RETURN-VALUE = "OK" THEN
            RUN getCharField IN h-boad107 (INPUT "nome":U, OUTPUT c-desc-estabel).

        ASSIGN tt-digita.descricao:SCREEN-VALUE IN BROWSE brDigita = c-desc-estabel.
    END.

    IF  brDigita:NEW-ROW IN FRAME fPage5 THEN DO:
        DO  TRANSACTION ON ERROR UNDO, RETURN NO-APPLY:
            CREATE tt-digita.
            ASSIGN INPUT BROWSE brDigita tt-digita.selecionado
                   INPUT BROWSE brDigita tt-digita.esp-docto
                   INPUT BROWSE brDigita tt-digita.descricao.
            brDigita:CREATE-RESULT-LIST-ENTRY() IN FRAME fPage5.
        END.
    END.
    ELSE DO:
        DO  TRANSACTION ON ERROR UNDO, RETURN NO-APPLY:
            IF  AVAIL tt-digita THEN
                ASSIGN INPUT BROWSE brDigita tt-digita.selecionado
                       INPUT BROWSE brDigita tt-digita.esp-docto
                       INPUT BROWSE brDigita tt-digita.descricao.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAdd wReport
ON CHOOSE OF btAdd IN FRAME fPage5 /* Inserir */
DO:
  ASSIGN tt-digita.selecionado = "*".
  
  {&OPEN-QUERY-brDigita}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel wReport
ON CHOOSE OF btCancel IN FRAME fpage0 /* Fechar */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage6
&Scoped-define SELF-NAME btConfigImpr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btConfigImpr wReport
ON CHOOSE OF btConfigImpr IN FRAME fPage6
DO:
   {report/rpimp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage5
&Scoped-define SELF-NAME btDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDelete wReport
ON CHOOSE OF btDelete IN FRAME fPage5 /* Retirar */
DO:
  FOR EACH tt-digita:
     ASSIGN tt-digita.selecionado = "*".
  END.

  {&OPEN-QUERY-brDigita}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage6
&Scoped-define SELF-NAME btFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFile wReport
ON CHOOSE OF btFile IN FRAME fPage6
DO:
    {report/rparq.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btHelp2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btHelp2 wReport
ON CHOOSE OF btHelp2 IN FRAME fpage0 /* Ajuda */
DO:
    {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage6
&Scoped-define SELF-NAME btModelRtf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btModelRtf wReport
ON CHOOSE OF btModelRtf IN FRAME fPage6
DO:
    def var cFile as char no-undo.
    def var l-ok  as logical no-undo.

    assign cModelRTF = replace(input frame {&frame-name} cModelRTF, "/", "~\").
    SYSTEM-DIALOG GET-FILE cFile
       FILTERS "*.rtf" "*.rtf",
               "*.*" "*.*"
       DEFAULT-EXTENSION "rtf"
       INITIAL-DIR "modelos" 
       MUST-EXIST
       USE-FILENAME
       UPDATE l-ok.
    if  l-ok = yes then
        assign cModelRTF:screen-value in frame {&frame-name}  = replace(cFile, "~\", "/"). 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&Scoped-define SELF-NAME btOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOK wReport
ON CHOOSE OF btOK IN FRAME fpage0 /* Executar */
DO:
   do  on error undo, return no-apply:
       run piExecute.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSave wReport
ON CHOOSE OF btSave IN FRAME fPage5 /* Salvar */
DO:
  FOR EACH tt-digita:
     ASSIGN tt-digita.selecionado = "".
  END.

  {&OPEN-QUERY-brDigita}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdate wReport
ON CHOOSE OF btUpdate IN FRAME fPage5 /* Alterar */
DO:
  ASSIGN tt-digita.selecionado = "".
  
  {&OPEN-QUERY-brDigita}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage3
&Scoped-define SELF-NAME cb-moeda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-moeda wReport
ON VALUE-CHANGED OF cb-moeda IN FRAME fPage3 /* Moeda */
DO:
   ASSIGN c-descricao-moeda = TRIM(STRING(cb-moeda:SCREEN-VALUE)).
   ASSIGN i-cod-moeda = INT(SUBSTRING(c-descricao-moeda,1,1)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fPage6
&Scoped-define SELF-NAME l-habilitaRtf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-habilitaRtf wReport
ON VALUE-CHANGED OF l-habilitaRtf IN FRAME fPage6 /* RTF */
DO:
    &IF "{&RTF}":U = "YES":U &THEN
    RUN pi-habilitaRtf.  
    &endif
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsDestiny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsDestiny wReport
ON VALUE-CHANGED OF rsDestiny IN FRAME fPage6
DO:
do  with frame fPage6:
    case self:screen-value:
        when "1":U then do:
            assign cFile:sensitive       = no
                   cFile:visible         = yes
                   btFile:visible        = no
                   btConfigImpr:visible  = yes
                   /*Alterado 15/02/2005 - tech1007 - Alterado para suportar adequadamente com a 
                     funcionalidade de RTF*/
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = NO
                   l-habilitaRtf:SCREEN-VALUE IN FRAME fPage6 = "No"
                   l-habilitaRtf = NO
                   &endif
                   .
                   /*Fim alteracao 15/02/2005*/
        end.
        when "2":U then do:
            assign cFile:sensitive       = yes
                   cFile:visible         = yes
                   btFile:visible        = yes
                   btConfigImpr:visible  = no
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = YES
                   &endif
                   .
        end.
        when "3":U then do:
            assign cFile:visible         = no
                   cFile:sensitive       = no
                   btFile:visible        = no
                   btConfigImpr:visible  = no
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = YES
                   &endif
                   .
            /*Alterado 15/02/2005 - tech1007 - Teste para funcionar corretamente no WebEnabler*/
            &IF "{&RTF}":U = "YES":U &THEN
            IF VALID-HANDLE(hWenController) THEN DO:
                ASSIGN l-habilitaRtf:sensitive  = NO
                       l-habilitaRtf:SCREEN-VALUE IN FRAME fPage6 = "No"
                       l-habilitaRtf = NO.
            END.
            &endif
            /*Fim alteracao 15/02/2005*/
        END.
        when "4":U then do:
            assign cFile:visible         = no
                   cFile:sensitive       = no
                   btFile:visible        = no
                   btConfigImpr:visible  = no
                   &IF "{&RTF}":U = "YES":U &THEN
                       l-habilitaRtf:sensitive  = NO
                       l-habilitaRtf:SCREEN-VALUE IN FRAME fPage6 = "No"
                       l-habilitaRtf = NO
                   &endif
                   .
        END.
        /*Alterado 15/02/2005 - tech1007 - Condiá∆o removida pois RTF n∆o Ç mais um destino
        when "4":U then do:
            assign cFile:sensitive       = no
                   cFile:visible         = yes
                   btFile:visible        = no
                   btConfigImpr:visible  = yes
                   text-ModelRtf:VISIBLE   = YES
                   rect-rtf:VISIBLE       = YES
                   btModelRtf:VISIBLE       = yes.
        end.
        Fim alteracao 15/02/2005*/
    end case.
end.
&IF "{&RTF}":U = "YES":U &THEN
RUN pi-habilitaRtf.  
&endif
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsExecution
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsExecution wReport
ON VALUE-CHANGED OF rsExecution IN FRAME fPage6
DO:
   {report/rprse.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fpage0
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wReport 


/*:T--- L¢gica para inicializaá∆o do programam ---*/
{report/mainblock.i}

if tt-digita.esp-docto:load-mouse-pointer ("image/lupa.cur") in BROWSE brDigita then.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    on f5 of tt-digita.esp-docto in browse brDigita
    or mouse-select-dblclick of tt-digita.esp-docto in browse brDigita do:
        {include/zoomvar.i &prog-zoom=adzoom/z01ad107.w
                           &campo=tt-digita.esp-docto
                           &campozoom=cod-estabel
                           &campo2=tt-digita.descricao
                           &campozoom2=nome
                           &browse=brDigita}
    END.

    IF  NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.  
/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterDestroyInterface wReport 
PROCEDURE afterDestroyInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF VALID-HANDLE(h-boun005) THEN DO:
        DELETE PROCEDURE h-boun005.
        ASSIGN h-boun005 = ?.
    END.

    IF VALID-HANDLE(h-boad107) THEN DO:
        RUN destroy IN h-boad107.
        DELETE OBJECT h-boad107 NO-ERROR.
    END.

    {cdp/cd0019.i1 "MCC"} /*Seguranáa por Estabelecimento*/
    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterInitializeInterface wReport 
PROCEDURE afterInitializeInterface :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*Alterado 17/02/2005 - tech1007 - Foi criado essa procedure para que seja realizado a inicializaá∆o
  correta dos componentes do RTF quando executado em ambiente local e no WebEnabler.*/

    &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
        DEFINE VARIABLE c-rs-tipo-custo AS CHARACTER  NO-UNDO.
        {utp/ut-liter.i Mensal}
        ASSIGN c-rs-tipo-custo = RETURN-VALUE + ",1,".
        {utp/ut-liter.i On-line}
        ASSIGN c-rs-tipo-custo = c-rs-tipo-custo + RETURN-VALUE + ",2".
        ASSIGN rs-tipo-custo:RADIO-BUTTONS IN FRAME fPage3 = c-rs-tipo-custo.

        ASSIGN rect-18:HIDDEN        IN FRAME fPage6 = NO
               l-imp-param:HIDDEN    IN FRAME fPage6 = NO
               l-imp-param:SENSITIVE IN FRAME fPage6 = YES
               l-imp-param:CHECKED   IN FRAME fPage6 = YES.

        rsClassif:ADD-LAST("Item/Unidade de Neg¢cio",4) IN FRAME fPage4.

    &ELSE
        ASSIGN rect-18:HIDDEN        IN FRAME fPage6 = YES
               l-imp-param:HIDDEN    IN FRAME fPage6 = YES
               c-unid-neg-ini:HIDDEN IN FRAME fPage2 = YES
               c-unid-neg-fim:HIDDEN IN FRAME fPage2 = YES
               image-30:HIDDEN       IN FRAME fPage2 = YES
               image-29:HIDDEN       IN FRAME fPage2 = YES.
    &ENDIF

&IF "{&RTF}":U = "YES":U &THEN
IF VALID-HANDLE(hWenController) THEN DO:
    ASSIGN l-habilitaRtf:sensitive IN FRAME fPage6 = NO
           l-habilitaRtf:SCREEN-VALUE IN FRAME fPage6 = "No"
           l-habilitaRtf = NO.

END.
RUN pi-habilitaRtf.
&endif
/*Fim alteracao 17/02/2005*/

RUN initializeDBOs.

&IF DEFINED(bf_mat_custeio_item) &then
&IF "{&bf_mat_versao_ems}" < "2.062" &THEN
    If rs-tipo-custo:disable(entry(5, rs-tipo-custo:radio-buttons)) in frame fPage3 then.
&ENDIF
&ENDIF

/** Busca Moeda **/

RUN RetornaMoedas IN h-boun005 (OUTPUT p-moeda).
assign cb-moeda:list-items IN FRAME fPage3  = p-moeda
       cb-moeda:SCREEN-VALUE IN FRAME fPage3 = cb-moeda:list-items.

ASSIGN c-descricao-moeda = TRIM(STRING(cb-moeda:SCREEN-VALUE IN FRAME fPage3)).
ASSIGN i-cod-moeda = INT(SUBSTRING(c-descricao-moeda,1,1)).

ASSIGN dt-data-trans-ini:SCREEN-VALUE IN FRAME fPage2 = STRING(TODAY - 30).
ASSIGN dt-data-trans-fim:SCREEN-VALUE IN FRAME fPage2 = STRING(TODAY).

ASSIGN c-lista     = {ininc/i03in218.i 03}
       c-lista-aux = "".
DO  i-x = 1 TO NUM-ENTRIES(c-lista):
    ASSIGN c-lista-aux = c-lista-aux + STRING(i-x,"99") + " - " + ENTRY(i-x,c-lista) + ",".
END.

/* ASSIGN cb-especie-ini:LIST-ITEMS IN FRAME fPage2 = c-lista-aux                                */
/*        cb-especie-fim:LIST-ITEMS IN FRAME fPage2 = c-lista-aux                                */
/*        cb-especie-ini:SCREEN-VALUE IN FRAME fPage2 = entry(1,c-lista-aux)                     */
/*        cb-especie-fim:SCREEN-VALUE IN FRAME fPage2 = entry(num-entries(c-lista),c-lista-aux). */
rs-tipo-custo:SENSITIVE IN FRAME fPage3 = NO.
cb-moeda:SENSITIVE IN FRAME fPage3 = NO.
rsclassif:SENSITIVE IN FRAME fPage4 = NO.

DEF VAR i-cont AS INT.

DO i-cont = 1 TO 38:
   CREATE tt-digita.
   ASSIGN tt-digita.esp-docto = i-cont
          tt-digita.descricao = ENTRY(i-cont,{ininc/i03in218.i 03})
          tt-digita.selecionado = "*".
END.

{&OPEN-QUERY-brDigita}



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeDBOs wReport 
PROCEDURE initializeDBOs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF  NOT VALID-HANDLE(h-boun005) OR
      h-boun005:TYPE <> "PROCEDURE":U OR
      h-boun005:FILE-NAME <> "unbo/boun005.p":U THEN DO:
       {btb/btb008za.i1 unbo/boun005.p YES}
       {btb/btb008za.i2 unbo/boun005.p '' h-boun005} 
  END.

   /*--- Verifica se o DBO j† est† inicializado ---*/
   IF NOT VALID-HANDLE(h-boad107) OR
      h-boad107:TYPE <> "PROCEDURE":U OR
      h-boad107:FILE-NAME <> "adbo/boad107na.p":U THEN DO:
      {btb/btb008za.i1 adbo/boad107na.p YES}
      {btb/btb008za.i2 adbo/boad107na.p '' h-boad107} 
   END.

   RUN openQueryStatic IN h-boad107 (INPUT "MAIN":U) NO-ERROR.    

  RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piExecute wReport 
PROCEDURE piExecute :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

define var r-tt-digita as rowid no-undo.

&IF DEFINED(PGIMP) <> 0 AND "{&PGIMP}":U = "YES":U &THEN
/*:T** Relatorio ***/
do on error undo, return error on stop  undo, return error:
    {report/rpexa.i}

    /*15/02/2005 - tech1007 - Teste alterado pois RTF n∆o Ç mais opá∆o de Destino*/
    if input frame fPage6 rsDestiny = 2 and
       input frame fPage6 rsExecution = 1 then do:
        run utp/ut-vlarq.p (input input frame fPage6 cFile).

        if return-value = "NOK":U then do:
            run utp/ut-msgs.p (input "show":U, input 73, input "":U).
            apply "ENTRY":U to cFile in frame fPage6.
            return error.
        end.
    end.

    /*16/02/2005 - tech1007 - Teste alterado para validar o modelo informado quando for RTF*/
    &IF "{&RTF}":U = "YES":U &THEN
    IF ( input frame fPage6 cModelRTF = "" AND
         input frame fPage6 l-habilitaRtf = YES ) OR
       ( SEARCH(INPUT FRAME fPage6 cModelRTF) = ? AND
         input frame fPage6 rsExecution = 1 AND
         input frame fPage6 l-habilitaRtf = YES )
         THEN DO:
        run utp/ut-msgs.p (input "show":U, input 73, input "":U).
        /*30/12/2004 - tech1007 - Evento removido pois causa problemas no WebEnabler*/
        /*apply "CHOOSE":U to btModelRtf in frame fPage6.*/
        return error.
    END.
    &endif

    /*:T Coloque aqui as validaá‰es das outras p†ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p†gina com 
       problemas e colocar o focus no campo com problemas */
    /*browse brDigita:SET-REPOSITIONED-ROW (browse brDigita:DOWN, "ALWAYS":U).*/

    for each tt-digita no-lock:
        assign r-tt-digita = rowid(tt-digita).

        /*:T Validaá∆o de duplicidade de registro na temp-table tt-digita */
        find first b-tt-digita 
            where b-tt-digita.esp-docto = tt-digita.esp-docto 
              and rowid(b-tt-digita) <> rowid(tt-digita) 
            no-lock no-error.
        if  avail b-tt-digita then do:
            reposition brDigita to rowid rowid(b-tt-digita).

            run utp/ut-msgs.p (input "SHOW":U, input 108, input "":U).
            apply "ENTRY":U to tt-digita.esp-docto in browse brDigita.

            return error.
        end.

        /*
        /*:T As demais validaá‰es devem ser feitas aqui */
        if  tt-digita.ordem <= 0 then do:
            assign browse brDigita:CURRENT-COLUMN = tt-digita.ordem:HANDLE in browse brDigita.

            reposition brDigita to rowid r-tt-digita.

            run utp/ut-msgs.p (input "SHOW":U, input 99999, input "":U).
            apply "ENTRY":U to tt-digita.ordem in browse brDigita.

            return error.
        end.
        */

    end.

    /*:T Aqui s∆o gravados os campos da temp-table que ser† passada como parÉmetro
       para o programa RP.P */

    EMPTY TEMP-TABLE tt-param.

    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame fPage6 rsDestiny
           tt-param.data-exec       = today
           tt-param.hora-exec       = time
           tt-param.classifica      = input frame fPage4 rsClassif
           tt-param.desc-classifica = entry((tt-param.classifica - 1) * 2 + 1, 
                                            rsClassif:radio-buttons in frame fPage4)
           &IF "{&RTF}":U = "YES":U &THEN
           tt-param.modelo          = INPUT FRAME fPage6 cModelRTF
           tt-param.l-habilitaRtf    = INPUT FRAME fPage6 l-habilitaRtf
           &endif

           tt-param.item-ini        = INPUT FRAME fPage2 c-item-ini
           tt-param.item-fim        = INPUT FRAME fPage2 c-item-fim
           tt-param.cod-estabel-ini = INPUT FRAME fPage2 c-cod-estabel-ini
           tt-param.cod-estabel-fim = INPUT FRAME fPage2 c-cod-estabel-fim
           tt-param.dt-trans-ini    = INPUT FRAME fPage2 dt-data-trans-ini
           tt-param.dt-trans-fim    = INPUT FRAME fPage2 dt-data-trans-fim
           tt-param.cod-refer-ini   = INPUT FRAME fPage2 c-cod-refer-ini
           tt-param.cod-refer-fim   = INPUT FRAME fPage2 c-cod-refer-fim
           tt-param.cod-depos-ini   = INPUT FRAME fPage2 c-cod-depos-ini
           tt-param.cod-depos-fim   = INPUT FRAME fPage2 c-cod-depos-fim
           tt-param.lote-ini        = INPUT FRAME fPage2 c-lote-ini
           tt-param.lote-fim        = INPUT FRAME fPage2 c-lote-fim
           tt-param.cod-localiz-ini = INPUT FRAME fPage2 c-cod-localiz-ini
           tt-param.cod-localiz-fim = INPUT FRAME fPage2 c-cod-localiz-fim
/*            INPUT FRAME fPage2 cb-especie-ini */
/*            INPUT FRAME fPage2 cb-especie-fim */
           tt-param.especie-ini      = 0 /*LOOKUP(cb-especie-ini,c-lista-aux)*/ 
           tt-param.especie-fim      = 999 /*LOOKUP(cb-especie-fim,c-lista-aux)*/ 
           tt-param.desc-especie-ini = "" /*INPUT FRAME fPage2 cb-especie-ini */
           tt-param.desc-especie-fim = "zzzzzzzzzzzzzzzzzzzzzzzzzzz" /*INPUT FRAME fPage2 cb-especie-fim */
           tt-param.serie-docto-ini = INPUT FRAME fPage2 c-serie-docto-ini
           tt-param.serie-docto-fim = INPUT FRAME fPage2 c-serie-docto-fim
           tt-param.nro-docto-ini   = INPUT FRAME fPage2 c-nro-docto-ini
           tt-param.nro-docto-fim   = INPUT FRAME fPage2 c-nro-docto-fim
           tt-param.moeda              = i-cod-moeda
           tt-param.desc-moeda         = c-descricao-moeda
           tt-param.tipo-custo         = INPUT FRAME fPage3 rs-tipo-custo
           tt-param.desc-tipo-custo    = ENTRY((tt-param.tipo-custo - 1) * 2 + 1,
                                              rs-tipo-custo:radio-buttons in frame fPage3)
           tt-param.ativo              = IF tg-ativo:CHECKED IN frame fPage3 = YES THEN YES ELSE NO
           tt-param.obsol-ord-aut      = IF tg-obsol-ord-aut:CHECKED IN frame fPage3 = YES THEN YES ELSE NO
           tt-param.obsol-todas-ordens = IF tg-obsol-todas-ordens:CHECKED IN frame fPage3 = YES THEN YES ELSE NO
           tt-param.obsol-total        = IF tg-obsol-total:CHECKED IN frame fPage3 = YES THEN YES ELSE NO
           &IF "{&bf_mat_versao_ems}":U >= "2.062":U  &THEN
               tt-param.c-unid-neg-ini = INPUT FRAME fPage2 c-unid-neg-ini
               tt-param.c-unid-neg-fim = INPUT FRAME fPage2 c-unid-neg-fim
           &ENDIF
           .

    if tt-param.destino = 1 
    then 
        assign tt-param.arquivo = "":U.
    else if  tt-param.destino = 2 
        then assign tt-param.arquivo = input frame fPage6 cFile.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp":U.

    /*:T Coloque aqui a l¢gica de gravaá∆o dos demais campos que devem ser passados
       como parÉmetros para o programa RP.P, atravÇs da temp-table tt-param */

    &if "{&BF_MAT_VERSAO_EMS}" >= "2.062" &then
        ASSIGN tt-param.l-imp-param = INPUT FRAME fPage6 l-imp-param.
    &ENDIF

    if  input frame fPage6 rsDestiny = 4 then do: /* Excel */
        ASSIGN tt-param.destino = 4.

        if  search(replace(tt-param.arquivo, ".tmp", ".xls")) <> ? THEN
            os-delete value(replace(tt-param.arquivo, ".tmp", ".xls")).        
        
        if  search(replace(tt-param.arquivo, ".tmp", ".xlsx")) <> ? THEN
            os-delete value(replace(tt-param.arquivo, ".tmp", ".xlsx")).

        /* Se j† existir um arquivo excel criado, n∆o deixar† criar outro */
        assign file-info:file-name = replace(tt-param.arquivo, ".tmp", ".xls").
        if  file-info:file-type <> ? then do:
            run utp/ut-msgs.p (input "show",
                               input 18583,
                               input "").
            return error.
        end.

        /* Se j† existir um arquivo excel criado, n∆o deixar† criar outro */
        assign file-info:file-name = replace(tt-param.arquivo, ".tmp", ".xlsx").
        if  file-info:file-type <> ? then do:
            run utp/ut-msgs.p (input "show",
                               input 18583,
                               input "").
            return error.
        end.
     end.


    /*:T Executar do programa RP.P que ir† criar o relat¢rio */
    {report/rpexb.i}

    SESSION:SET-WAIT-STATE("GENERAL":U).

    {report/rprun.i esp/yg0006rp.p}

    {report/rpexc.i}


    SESSION:SET-WAIT-STATE("":U).

    /* {report/rptrm.i} */

end.
&ENDIF


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
{include/i-prgvrs.i escd0001 2.06.00.000}

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

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    FIELD dt-trans-ini     LIKE movto-estoq.dt-trans
    FIELD dt-trans-fim     LIKE movto-estoq.dt-trans
    FIELD ct-codigo-ini    LIKE movto-estoq.ct-codigo
    FIELD ct-codigo-fim    LIKE movto-estoq.ct-codigo
    FIELD sc-codigo-ini    LIKE movto-estoq.sc-codigo
    FIELD sc-codigo-fim    LIKE movto-estoq.sc-codigo
    FIELD cd-equipto-ini   LIKE ord-manut.cd-equipto
    FIELD cd-equipto-fim   LIKE ord-manut.cd-equipto
    FIELD it-codigo-ini    LIKE movto-estoq.it-codigo
    FIELD it-codigo-fim    LIKE movto-estoq.it-codigo
    FIELD i-impressao      AS INTEGER
    FIELD tipo-trans       AS CHAR
    FIELD i-sql            AS INTEGER
    FIELD dir-sql          AS CHAR
    FIELD dir-excel        AS CHAR.

define temp-table tt-digita no-undo
    FIELD nat-operacao     LIKE nota-fiscal.nat-operacao
    INDEX id nat-operacao.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
DEF VAR str-trans          AS CHAR    NO-UNDO.
DEF VAR err-status         AS INT     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define BROWSE-NAME br-digita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-digita

/* Definitions for BROWSE br-digita                                     */
&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.nat-operacao   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-digita.nat-operacao   
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
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-9 RECT-8 bt-arquivo rs-execucao ~
bt-arquivo-sql c-arquivos-sql bt-arquivo-excel c-arquivos-excel 
&Scoped-Define DISPLAYED-OBJECTS rs-destino c-arquivo rs-execucao ~
c-arquivos-sql c-arquivos-excel 

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

DEFINE BUTTON bt-arquivo-excel 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-arquivo-sql 
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

DEFINE VARIABLE c-arquivos-excel AS CHARACTER FORMAT "X(256)":U INITIAL "~\~\ydminteg01~\temp~\rpw" 
     LABEL "Arquivos Excel" 
     VIEW-AS FILL-IN 
     SIZE 52 BY .88 NO-UNDO.

DEFINE VARIABLE c-arquivos-sql AS CHARACTER FORMAT "X(256)":U INITIAL "~\~\ydminteg01~\temp~\rpw" 
     LABEL "Arquivos SQL" 
     VIEW-AS FILL-IN 
     SIZE 52 BY .88 NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE text-destino-2 AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execuá∆o" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3
     SIZE 44 BY 1.08 NO-UNDO.

DEFINE VARIABLE rs-execucao AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
"Batch", 2
     SIZE 27.72 BY .92 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 6.75.

DEFINE VARIABLE tg-trans-1 AS LOGICAL INITIAL yes 
     LABEL "ACA" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-10 AS LOGICAL INITIAL yes 
     LABEL "BEM" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-11 AS LOGICAL INITIAL yes 
     LABEL "NU2" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-12 AS LOGICAL INITIAL yes 
     LABEL "NU3" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-13 AS LOGICAL INITIAL yes 
     LABEL "NU4" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-14 AS LOGICAL INITIAL yes 
     LABEL "ICM" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-15 AS LOGICAL INITIAL yes 
     LABEL "INV" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-16 AS LOGICAL INITIAL yes 
     LABEL "IPL" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-17 AS LOGICAL INITIAL yes 
     LABEL "MOB" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-18 AS LOGICAL INITIAL yes 
     LABEL "NC" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-19 AS LOGICAL INITIAL yes 
     LABEL "NF" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-2 AS LOGICAL INITIAL yes 
     LABEL "ACT" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-20 AS LOGICAL INITIAL yes 
     LABEL "NFD" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-21 AS LOGICAL INITIAL yes 
     LABEL "NFE" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-22 AS LOGICAL INITIAL yes 
     LABEL "NFS" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-23 AS LOGICAL INITIAL yes 
     LABEL "NFT" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-24 AS LOGICAL INITIAL yes 
     LABEL "NUS" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-25 AS LOGICAL INITIAL yes 
     LABEL "REF" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-26 AS LOGICAL INITIAL yes 
     LABEL "RCS" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-27 AS LOGICAL INITIAL yes 
     LABEL "RDD" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-28 AS LOGICAL INITIAL yes 
     LABEL "REQ" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-29 AS LOGICAL INITIAL yes 
     LABEL "RFS" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-3 AS LOGICAL INITIAL yes 
     LABEL "NU1" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-30 AS LOGICAL INITIAL yes 
     LABEL "RM" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-31 AS LOGICAL INITIAL yes 
     LABEL "RRQ" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-32 AS LOGICAL INITIAL yes 
     LABEL "STR" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-33 AS LOGICAL INITIAL yes 
     LABEL "TRA" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-34 AS LOGICAL INITIAL yes 
     LABEL "ZZZ" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-35 AS LOGICAL INITIAL yes 
     LABEL "SOB" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-4 AS LOGICAL INITIAL yes 
     LABEL "DD" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-5 AS LOGICAL INITIAL yes 
     LABEL "DEV" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-6 AS LOGICAL INITIAL yes 
     LABEL "DIV" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-7 AS LOGICAL INITIAL yes 
     LABEL "DRM" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-8 AS LOGICAL INITIAL yes 
     LABEL "EAC" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trans-9 AS LOGICAL INITIAL yes 
     LABEL "EGF" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE c-cd-equipto-fim AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE c-cd-equipto-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Equipamento":R14 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE c-ct-codigo-fim AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE c-ct-codigo-ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Conta":R7 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE c-it-codigo-fim AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE c-it-codigo-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Item":R5 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE c-sc-codigo-fim AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE c-sc-codigo-ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Ccusto":R11 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE dt-dt-trans-fim AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE dt-dt-trans-ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Data Transaá∆o":R17 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-13
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-15
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-16
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-17
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-18
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rds-impressao AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Analitico", 1,
"SintÇtico", 2
     SIZE 26.14 BY .79 NO-UNDO.

DEFINE VARIABLE rds-sql AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Expediá∆o", 1,
"Outros", 2
     SIZE 25 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 76.86 BY 1.75.

DEFINE VARIABLE tb-ontem AS LOGICAL INITIAL no 
     LABEL "Ontem" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

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
      tt-digita.nat-operacao
ENABLE
tt-digita.nat-operacao
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 76.57 BY 9
         BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-pg-dig
     br-digita AT ROW 1 COL 1
     bt-inserir AT ROW 10 COL 1
     bt-alterar AT ROW 10 COL 16
     bt-retirar AT ROW 10 COL 31
     bt-salvar AT ROW 10 COL 46
     bt-recuperar AT ROW 10 COL 61
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.31
         SIZE 76.86 BY 10.15 WIDGET-ID 100.

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execuá∆o do relat¢rio" WIDGET-ID 2
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     im-pg-sel AT ROW 1.5 COL 2.14
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-dig AT ROW 1.5 COL 33.57
     im-pg-imp AT ROW 1.5 COL 49.29
     rt-folder AT ROW 2.5 COL 2
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-6 AT ROW 13.75 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15 WIDGET-ID 100.

DEFINE FRAME f-pg-par
     tg-trans-1 AT ROW 3.83 COL 16.43 WIDGET-ID 16
     tg-trans-8 AT ROW 3.83 COL 26.43 WIDGET-ID 32
     tg-trans-15 AT ROW 3.83 COL 36.43 WIDGET-ID 46
     tg-trans-22 AT ROW 3.83 COL 46.43 WIDGET-ID 60
     tg-trans-29 AT ROW 3.83 COL 56.43 WIDGET-ID 74
     tg-trans-2 AT ROW 4.58 COL 16.43 WIDGET-ID 20
     tg-trans-9 AT ROW 4.58 COL 26.43 WIDGET-ID 34
     tg-trans-16 AT ROW 4.58 COL 36.43 WIDGET-ID 48
     tg-trans-23 AT ROW 4.58 COL 46.43 WIDGET-ID 62
     tg-trans-30 AT ROW 4.58 COL 56.43 WIDGET-ID 76
     tg-trans-3 AT ROW 5.33 COL 16.43 WIDGET-ID 22
     tg-trans-10 AT ROW 5.33 COL 26.43 WIDGET-ID 36
     tg-trans-17 AT ROW 5.33 COL 36.43 WIDGET-ID 50
     tg-trans-24 AT ROW 5.33 COL 46.43 WIDGET-ID 64
     tg-trans-31 AT ROW 5.33 COL 56.43 WIDGET-ID 78
     tg-trans-4 AT ROW 6.08 COL 16.43 WIDGET-ID 24
     tg-trans-11 AT ROW 6.08 COL 26.43 WIDGET-ID 38
     tg-trans-18 AT ROW 6.08 COL 36.43 WIDGET-ID 52
     tg-trans-25 AT ROW 6.08 COL 46.43 WIDGET-ID 66
     tg-trans-32 AT ROW 6.08 COL 56.43 WIDGET-ID 80
     tg-trans-5 AT ROW 6.83 COL 16.43 WIDGET-ID 26
     tg-trans-12 AT ROW 6.83 COL 26.43 WIDGET-ID 40
     tg-trans-19 AT ROW 6.83 COL 36.43 WIDGET-ID 54
     tg-trans-26 AT ROW 6.83 COL 46.43 WIDGET-ID 68
     tg-trans-33 AT ROW 6.83 COL 56.43 WIDGET-ID 82
     tg-trans-6 AT ROW 7.58 COL 16.43 WIDGET-ID 28
     tg-trans-13 AT ROW 7.58 COL 26.43 WIDGET-ID 42
     tg-trans-20 AT ROW 7.58 COL 36.43 WIDGET-ID 56
     tg-trans-27 AT ROW 7.58 COL 46.43 WIDGET-ID 70
     tg-trans-34 AT ROW 7.58 COL 56.43 WIDGET-ID 84
     tg-trans-7 AT ROW 8.33 COL 16.43 WIDGET-ID 30
     tg-trans-14 AT ROW 8.33 COL 26.43 WIDGET-ID 44
     tg-trans-21 AT ROW 8.33 COL 36.43 WIDGET-ID 58
     tg-trans-28 AT ROW 8.33 COL 46.43 WIDGET-ID 72
     tg-trans-35 AT ROW 8.33 COL 56.43 WIDGET-ID 86
     "  Transaá∆o:" VIEW-AS TEXT
          SIZE 9.43 BY .67 AT ROW 2.75 COL 11.57 WIDGET-ID 6
     RECT-10 AT ROW 3.08 COL 10 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 75 BY 10 WIDGET-ID 300.

DEFINE FRAME f-pg-sel
     rds-sql AT ROW 1.5 COL 33.29 NO-LABEL WIDGET-ID 48
     dt-dt-trans-ini AT ROW 3.54 COL 16.43 COLON-ALIGNED
     dt-dt-trans-fim AT ROW 3.54 COL 40.72 COLON-ALIGNED NO-LABEL WIDGET-ID 42
     tb-ontem AT ROW 3.54 COL 58.72 WIDGET-ID 56
     c-ct-codigo-ini AT ROW 4.83 COL 16.29 COLON-ALIGNED WIDGET-ID 10
     c-ct-codigo-fim AT ROW 4.83 COL 40.72 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     c-sc-codigo-ini AT ROW 6.13 COL 16.29 COLON-ALIGNED HELP
          "Ccusto da Conta Cont†bil" WIDGET-ID 12
     c-sc-codigo-fim AT ROW 6.13 COL 40.72 COLON-ALIGNED HELP
          "Ccusto da Conta Cont†bil" NO-LABEL WIDGET-ID 24
     c-cd-equipto-ini AT ROW 7.33 COL 16.14 COLON-ALIGNED WIDGET-ID 14
     c-cd-equipto-fim AT ROW 7.38 COL 40.72 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     c-it-codigo-ini AT ROW 8.58 COL 16 COLON-ALIGNED WIDGET-ID 16
     c-it-codigo-fim AT ROW 8.58 COL 40.72 COLON-ALIGNED NO-LABEL
     rds-impressao AT ROW 10.38 COL 29.29 NO-LABEL WIDGET-ID 44
     "Importaá∆o SQL:" VIEW-AS TEXT
          SIZE 16 BY .67 AT ROW 1.63 COL 17 WIDGET-ID 52
     IMAGE-1 AT ROW 3.5 COL 33.29
     IMAGE-2 AT ROW 3.54 COL 39.86
     IMAGE-11 AT ROW 4.83 COL 33.43 WIDGET-ID 26
     IMAGE-12 AT ROW 6.17 COL 33.43 WIDGET-ID 28
     IMAGE-13 AT ROW 7.33 COL 33.43 WIDGET-ID 30
     IMAGE-14 AT ROW 8.58 COL 33.43 WIDGET-ID 32
     IMAGE-15 AT ROW 4.83 COL 39.86 WIDGET-ID 34
     IMAGE-16 AT ROW 6.25 COL 39.86 WIDGET-ID 36
     IMAGE-17 AT ROW 7.46 COL 39.86 WIDGET-ID 38
     IMAGE-18 AT ROW 8.63 COL 39.86 WIDGET-ID 40
     RECT-11 AT ROW 1 COL 1 WIDGET-ID 54
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.92
         SIZE 76.86 BY 10.58 WIDGET-ID 200.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 1.96 COL 3.29 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     bt-config-impr AT ROW 3.17 COL 43.29 HELP
          "Configuraá∆o da impressora"
     c-arquivo AT ROW 3.21 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     bt-arquivo AT ROW 3.21 COL 43.29 HELP
          "Escolha do nome do arquivo"
     rs-execucao AT ROW 5.54 COL 3 HELP
          "Modo de Execuá∆o" NO-LABEL
     bt-arquivo-sql AT ROW 7.92 COL 68 HELP
          "Escolha do nome do arquivo" WIDGET-ID 4
     c-arquivos-sql AT ROW 8 COL 2.28 WIDGET-ID 6
     bt-arquivo-excel AT ROW 9.21 COL 68 HELP
          "Escolha do nome do arquivo" WIDGET-ID 10
     c-arquivos-excel AT ROW 9.25 COL 1 WIDGET-ID 8
     text-destino AT ROW 1.21 COL 3.86 NO-LABEL
     text-modo AT ROW 4.79 COL 1.29 COLON-ALIGNED NO-LABEL
     text-destino-2 AT ROW 7.21 COL 3.86 NO-LABEL WIDGET-ID 14
     RECT-7 AT ROW 1.5 COL 2.14
     RECT-9 AT ROW 5.08 COL 2.14
     RECT-8 AT ROW 7.5 COL 2 WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10 WIDGET-ID 400.


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
         TITLE              = "Relatorio"
         HEIGHT             = 15
         WIDTH              = 81.14
         MAX-HEIGHT         = 28.92
         MAX-WIDTH          = 194.29
         VIRTUAL-HEIGHT     = 28.92
         VIRTUAL-WIDTH      = 194.29
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
/* REPARENT FRAME */
ASSIGN FRAME f-pg-imp:FRAME = FRAME f-relat:HANDLE
       FRAME f-pg-par:FRAME = FRAME f-relat:HANDLE
       FRAME f-pg-sel:FRAME = FRAME f-relat:HANDLE.

/* SETTINGS FOR FRAME f-pg-dig
                                                                        */
/* BROWSE-TAB br-digita 1 f-pg-dig */
/* SETTINGS FOR BUTTON bt-alterar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-retirar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-salvar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-imp
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON bt-config-impr IN FRAME f-pg-imp
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR c-arquivo IN FRAME f-pg-imp
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-arquivos-excel IN FRAME f-pg-imp
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN c-arquivos-sql IN FRAME f-pg-imp
   ALIGN-L                                                              */
/* SETTINGS FOR RADIO-SET rs-destino IN FRAME f-pg-imp
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

/* SETTINGS FOR FILL-IN text-destino-2 IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino-2:PRIVATE-DATA IN FRAME f-pg-imp     = 
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
ON END-ERROR OF w-relat /* Relatorio */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Relatorio */
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
        display tt-digita.nat-operacao
                 with browse br-digita. 
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
   /*if  br-digita:new-row in frame f-pg-dig then do:
       assign tt-digita.exemplo:screen-value in browse br-digita = "".
   end.*/
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
        create tt-digita.
        assign input browse br-digita tt-digita.nat-operacao.
    end.
    else do transaction on error undo, return no-apply:
        assign input browse br-digita tt-digita.nat-operacao.
    end.
    
    /*br-digita:CREATE-RESULT-LIST-ENTRY() in frame f-pg-dig. */
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
   apply 'entry' to tt-digita.nat-operacao in browse br-digita. 
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


&Scoped-define SELF-NAME bt-arquivo-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo-excel w-relat
ON CHOOSE OF bt-arquivo-excel IN FRAME f-pg-imp
DO:
    
    SYSTEM-DIALOG GET-DIR c-arquivos-excel
        INITIAL-DIR "\\ydminteg01\temp\rpw"
        RETURN-TO-START-DIR
        TITLE "Destino Arquivos Excel".
 
    DISPLAY c-arquivos-excel
       WITH FRAME f-pg-imp.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-arquivo-sql
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo-sql w-relat
ON CHOOSE OF bt-arquivo-sql IN FRAME f-pg-imp
DO:
    
    SYSTEM-DIALOG GET-DIR c-arquivos-sql
        INITIAL-DIR "\\ydminteg01\temp\rpw"
        RETURN-TO-START-DIR
        TITLE "Destino Arquivos SQL".
 
    DISPLAY c-arquivos-sql
       WITH FRAME f-pg-imp.

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
       IF INPUT FRAME f-pg-sel rds-impressao = 1 THEN
       DO:
           RUN pi-movimento.

           IF str-trans = "" OR
              str-trans = ? THEN
           DO:
               MESSAGE "Nenhum tipo de Transacao selecionado." SKIP
                       "Favor selecionar ao menos um tipo de Transacao."
                   VIEW-AS ALERT-BOX ERROR BUTTONS OK.

               APPLY "MOUSE-SELECT-CLICK":U TO im-pg-par IN FRAME f-relat.
           
               RETURN "no-apply".
           END.
       END.

       IF INPUT FRAME f-pg-sel rds-sql = 1 THEN
       DO:
           FIND FIRST tt-digita NO-LOCK NO-ERROR.
           IF NOT AVAILABLE tt-digita THEN
           DO:
               MESSAGE "Nenhum parÉmetro de Natureza de Operaá∆o informado." SKIP
                       "Favor informar algum valor."
                   VIEW-AS ALERT-BOX ERROR BUTTONS OK.

               APPLY "MOUSE-SELECT-CLICK":U TO im-pg-dig IN FRAME f-relat.
           
               RETURN "no-apply".
       END.
           
       END.

       RUN pi-executar.
           
       RETURN "NO-ERROR".
       
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
    
    if num-results("br-digita") > 0 then
        br-digita:INSERT-ROW("after") in frame f-pg-dig.
    else do transaction:
        create tt-digita.
        
        open query br-digita for each tt-digita.
        
        apply "entry" to tt-digita.nat-operacao in browse br-digita. 
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


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME c-arquivos-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-arquivos-excel w-relat
ON LEAVE OF c-arquivos-excel IN FRAME f-pg-imp /* Arquivos Excel */
DO:
  /*ASSIGN arquivo = c-arquivo:SCREEN-VALUE.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-arquivos-sql
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-arquivos-sql w-relat
ON LEAVE OF c-arquivos-sql IN FRAME f-pg-imp /* Arquivos SQL */
DO:
  /*ASSIGN arquivo = c-arquivo:SCREEN-VALUE.*/
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


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME rds-impressao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rds-impressao w-relat
ON VALUE-CHANGED OF rds-impressao IN FRAME f-pg-sel
DO:
/*   FIND tt-param NO-LOCK NO-ERROR.    */
/*   IF i-impressao = 1 THEN            */
/*       RUN pi-imprime.                */
/*   ELSE                               */
/*       MESSAGE "n∆o da para imprimir" */
/*           VIEW-AS ALERT-BOX.         */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rds-sql
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rds-sql w-relat
ON VALUE-CHANGED OF rds-sql IN FRAME f-pg-sel
DO:
    IF rds-sql:SCREEN-VALUE = "1" THEN
    DO:
        ASSIGN c-ct-codigo-ini:SENSITIVE  = NO
               c-ct-codigo-fim:SENSITIVE  = NO
               c-sc-codigo-ini:SENSITIVE  = NO
               c-sc-codigo-fim:SENSITIVE  = NO
               c-cd-equipto-ini:SENSITIVE = NO
               c-cd-equipto-fim:SENSITIVE = NO
               c-it-codigo-ini:SENSITIVE  = NO
               c-it-codigo-fim:SENSITIVE  = NO
               rds-impressao:SENSITIVE    = NO.

        ASSIGN c-arquivos-excel:SENSITIVE IN FRAME f-pg-imp = NO
               bt-arquivo-excel:SENSITIVE IN FRAME f-pg-imp = NO.
    END.

    IF rds-sql:SCREEN-VALUE = "2" THEN
    DO:
        ASSIGN c-ct-codigo-ini:SENSITIVE  = YES
               c-ct-codigo-fim:SENSITIVE  = YES
               c-sc-codigo-ini:SENSITIVE  = YES
               c-sc-codigo-fim:SENSITIVE  = YES
               c-cd-equipto-ini:SENSITIVE = YES
               c-cd-equipto-fim:SENSITIVE = YES
               c-it-codigo-ini:SENSITIVE  = YES
               c-it-codigo-fim:SENSITIVE  = YES
               rds-impressao:SENSITIVE    = YES.

        ASSIGN c-arquivos-excel:SENSITIVE IN FRAME f-pg-imp = YES
               bt-arquivo-excel:SENSITIVE IN FRAME f-pg-imp = YES.
        
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
   /*{include/i-rprse.i}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME tb-ontem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb-ontem w-relat
ON VALUE-CHANGED OF tb-ontem IN FRAME f-pg-sel /* Ontem */
DO:
    IF tb-ontem:SCREEN-VALUE = STRING(YES) THEN
    DO:
        ASSIGN dt-dt-trans-ini:SENSITIVE  = NO
               dt-dt-trans-fim:SENSITIVE  = NO.
    END.

    IF tb-ontem:SCREEN-VALUE = STRING(NO) THEN
    DO:
        ASSIGN dt-dt-trans-ini:SENSITIVE  = YES
               dt-dt-trans-fim:SENSITIVE  = YES.
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "escd0001" "2.06.00.000"}

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
  ENABLE im-pg-sel im-pg-par im-pg-dig im-pg-imp bt-executar bt-cancelar 
         bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY rds-sql dt-dt-trans-ini dt-dt-trans-fim tb-ontem c-ct-codigo-ini 
          c-ct-codigo-fim c-sc-codigo-ini c-sc-codigo-fim c-cd-equipto-ini 
          c-cd-equipto-fim c-it-codigo-ini c-it-codigo-fim rds-impressao 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-1 IMAGE-2 IMAGE-11 IMAGE-12 IMAGE-13 IMAGE-14 IMAGE-15 IMAGE-16 
         IMAGE-17 IMAGE-18 RECT-11 rds-sql dt-dt-trans-ini dt-dt-trans-fim 
         tb-ontem c-ct-codigo-ini c-ct-codigo-fim c-sc-codigo-ini 
         c-sc-codigo-fim c-cd-equipto-ini c-cd-equipto-fim c-it-codigo-ini 
         c-it-codigo-fim rds-impressao 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino c-arquivo rs-execucao c-arquivos-sql c-arquivos-excel 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 RECT-8 bt-arquivo rs-execucao bt-arquivo-sql 
         c-arquivos-sql bt-arquivo-excel c-arquivos-excel 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY tg-trans-1 tg-trans-8 tg-trans-15 tg-trans-22 tg-trans-29 tg-trans-2 
          tg-trans-9 tg-trans-16 tg-trans-23 tg-trans-30 tg-trans-3 tg-trans-10 
          tg-trans-17 tg-trans-24 tg-trans-31 tg-trans-4 tg-trans-11 tg-trans-18 
          tg-trans-25 tg-trans-32 tg-trans-5 tg-trans-12 tg-trans-19 tg-trans-26 
          tg-trans-33 tg-trans-6 tg-trans-13 tg-trans-20 tg-trans-27 tg-trans-34 
          tg-trans-7 tg-trans-14 tg-trans-21 tg-trans-28 tg-trans-35 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE RECT-10 tg-trans-1 tg-trans-8 tg-trans-15 tg-trans-22 tg-trans-29 
         tg-trans-2 tg-trans-9 tg-trans-16 tg-trans-23 tg-trans-30 tg-trans-3 
         tg-trans-10 tg-trans-17 tg-trans-24 tg-trans-31 tg-trans-4 tg-trans-11 
         tg-trans-18 tg-trans-25 tg-trans-32 tg-trans-5 tg-trans-12 tg-trans-19 
         tg-trans-26 tg-trans-33 tg-trans-6 tg-trans-13 tg-trans-20 tg-trans-27 
         tg-trans-34 tg-trans-7 tg-trans-14 tg-trans-21 tg-trans-28 tg-trans-35 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  ENABLE br-digita bt-inserir bt-recuperar 
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
    
    for each tt-digita no-lock:
        assign r-tt-digita = rowid(tt-digita).

        /* Validaá∆o de duplicidade de registro na temp-table tt-digita */
        /*find first b-tt-digita where b-tt-digita.nat-operacao = tt-digita.nat-operacao and
                                     rowid(b-tt-digita) <> rowid(tt-digita) no-lock no-error.
        if avail b-tt-digita then do:
            apply "MOUSE-SELECT-CLICK":U to im-pg-dig in frame f-relat.
            /*reposition br-digita to rowid rowid(b-tt-digita).*/

            run utp/ut-msgs.p (input "show", input 108, input "").
            apply "ENTRY":U to tt-digita.nat-operacao in browse br-digita.

            return error.
        end.*/

    /*As demais validaá‰es devem ser feitas aqui*/
        /*if tt-digita.ordem <= 0 then do:
            assign browse br-digita:CURRENT-COLUMN = tt-digita.ordem:HANDLE in browse br-digita.

            apply "MOUSE-SELECT-CLICK":U to im-pg-dig in frame f-relat.
            reposition br-digita to rowid r-tt-digita.

            run utp/ut-msgs.p (input "show", input 99999, input "").
            apply "ENTRY":U to tt-digita.ordem in browse br-digita.

            return error.
        end.*/

    end.

    /* Coloque aqui as validaá‰es das outras p†ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p†gina com 
       problemas e colocar o focus no campo com problemas */
    
    IF INPUT FRAME f-pg-sel dt-dt-trans-ini > INPUT FRAME f-pg-sel dt-dt-trans-fim THEN  
    DO:
        MESSAGE "Intervalo de datas invalido, verifique!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "MOUSE-SELECT-CLICK":U TO im-pg-sel in frame f-relat.
        APPLY "entry" TO dt-dt-trans-ini IN FRAME f-pg-sel.
        RETURN ERROR.
    END.

    IF INPUT FRAME f-pg-sel c-ct-codigo-ini > INPUT FRAME f-pg-sel c-ct-codigo-fim THEN
    DO:
        MESSAGE "Intervalo entre contas invalido, verifique!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "mouse-select-click":U TO im-pg-sel IN FRAME f-relat.
        APPLY "entry" TO c-ct-codigo-ini IN FRAME f-pg-sel.
        RETURN ERROR.
    END.

    IF INPUT FRAME f-pg-sel c-sc-codigo-ini > INPUT FRAME f-pg-sel c-sc-codigo-fim THEN
    DO:
        MESSAGE "Intervalo entre Ccustos inv†lido, verifique!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "mouse-select-click":U TO im-pg-sel IN FRAME f-relat.
        APPLY "entry" TO c-sc-codigo-ini IN FRAME f-pg-sel.
        RETURN ERROR.
    END.

    IF INPUT FRAME f-pg-sel c-cd-equipto-ini > INPUT FRAME f-pg-sel c-cd-equipto-fim THEN
    DO:
        MESSAGE "Intervalo entre Equipamentos invalido, verifique!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "mouse-select-click":U TO im-pg-sel IN FRAME f-relat.
        APPLY "entry" TO c-cd-equipto-ini IN FRAME f-pg-sel.
        RETURN ERROR.
    END.

    IF INPUT FRAME f-pg-sel c-it-codigo-ini > INPUT FRAME f-pg-sel c-it-codigo-ini THEN 
    DO:
        MESSAGE "Intervalo entre itens inv†lido, verifique!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "mouse-select-click":U TO im-pg-sel IN FRAME f-relat.
        APPLY "entry" TO c-it-codigo-ini IN FRAME f-pg-sel.
        RETURN ERROR.
    END.

    
    OS-RENAME VALUE (c-arquivos-sql:SCREEN-VALUE) VALUE (c-arquivos-sql:SCREEN-VALUE).
    ASSIGN err-status = OS-ERROR.
    IF err-status = 2 THEN
    DO:
        MESSAGE "Diret¢rio n∆o encontrado. Favor selecionar um diret¢rio v†lido."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "mouse-select-click":U TO im-pg-imp IN FRAME f-relat.
        APPLY "entry" TO c-arquivos-sql IN FRAME f-pg-imp.
        RETURN ERROR.
    END.

    IF rds-sql:SCREEN-VALUE = "2" THEN
    DO:
        OS-RENAME VALUE (c-arquivos-excel:SCREEN-VALUE) VALUE (c-arquivos-excel:SCREEN-VALUE).
        ASSIGN err-status = OS-ERROR.
        IF err-status = 2 THEN
        DO:
            MESSAGE "Diret¢rio n∆o encontrado. Favor selecionar um diret¢rio v†lido."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "mouse-select-click":U TO im-pg-imp IN FRAME f-relat.
            APPLY "entry" TO c-arquivos-excel IN FRAME f-pg-imp.
            RETURN ERROR.
        END.
    END.
    

    
    /* Aqui s∆o gravados os campos da temp-table que ser† passada como parÉmetro
       para o programa RP.P */
    
    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame f-pg-imp rs-destino
           tt-param.data-exec       = today
           tt-param.hora-exec       = time.
/*            tt-param.classifica      = input frame f-pg-cla rs-classif                    */
/*            tt-param.desc-classifica = entry((tt-param.classifica - 1) * 2 + 1,           */
/*                                             rs-classif:radio-buttons in frame f-pg-cla). */
    
    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".
    
    /* Coloque aqui a l¢gica de gravaá∆o dos demais campos que devem ser passados
       como parÉmetros para o programa RP.P, atravÇs da temp-table tt-param */
    
    ASSIGN
        tt-param.dt-trans-ini   = INPUT FRAME f-pg-sel dt-dt-trans-ini    
        tt-param.dt-trans-fim   = INPUT FRAME f-pg-sel dt-dt-trans-fim  
        tt-param.ct-codigo-ini  = INPUT FRAME f-pg-sel c-ct-codigo-ini   
        tt-param.ct-codigo-fim  = INPUT FRAME f-pg-sel c-ct-codigo-fim   
        tt-param.sc-codigo-ini  = INPUT FRAME f-pg-sel c-sc-codigo-ini   
        tt-param.sc-codigo-fim  = INPUT FRAME f-pg-sel c-sc-codigo-fim
        tt-param.cd-equipto-ini = INPUT FRAME f-pg-sel c-cd-equipto-ini
        tt-param.cd-equipto-fim = INPUT FRAME f-pg-sel c-cd-equipto-fim
        tt-param.it-codigo-ini  = INPUT FRAME f-pg-sel c-it-codigo-ini  
        tt-param.it-codigo-fim  = INPUT FRAME f-pg-sel c-it-codigo-fim
        tt-param.i-impressao    = INPUT FRAME f-pg-sel rds-impressao
        tt-param.i-sql          = INPUT FRAME f-pg-sel rds-sql
        tt-param.tipo-trans     = str-trans
        tt-param.dir-sql        = INPUT FRAME f-pg-imp c-arquivos-sql
        tt-param.dir-excel      = INPUT FRAME f-pg-imp c-arquivos-excel.

    IF tb-ontem:SCREEN-VALUE = STRING(YES) THEN
    DO:
        ASSIGN tt-param.dt-trans-ini  = TODAY - 1
               tt-param.dt-trans-fim  = TODAY - 1.
    END.

    /* Executar do programa RP.P que ir† criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i esp/escd0001rp.p}
    
    {include/i-rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
/*     {include/i-rptrm.i} */
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-movimento w-relat 
PROCEDURE pi-movimento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN str-trans = "".

IF INPUT FRAME f-pg-par tg-trans-1 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "ACA,".
END.
IF INPUT FRAME f-pg-par tg-trans-2 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "ACT,".
END.
IF INPUT FRAME f-pg-par tg-trans-3 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "NU1,".
END.
IF INPUT FRAME f-pg-par tg-trans-4 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "DD,".
END.
IF INPUT FRAME f-pg-par tg-trans-5 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "DEV,".
END.
IF INPUT FRAME f-pg-par tg-trans-6 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "DIV,".
END.
IF INPUT FRAME f-pg-par tg-trans-7 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "DRM,".
END.
IF INPUT FRAME f-pg-par tg-trans-8 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "EAC,".
END.
IF INPUT FRAME f-pg-par tg-trans-9 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "EGF,".
END.
IF INPUT FRAME f-pg-par tg-trans-10 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "BEM,".
END.
IF INPUT FRAME f-pg-par tg-trans-11 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "NU2,".
END.
IF INPUT FRAME f-pg-par tg-trans-12 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "NU3,".
END.
IF INPUT FRAME f-pg-par tg-trans-13 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "NU4,".
END.
IF INPUT FRAME f-pg-par tg-trans-14 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "ICM,".
END.
IF INPUT FRAME f-pg-par tg-trans-15 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "INV,".
END.
IF INPUT FRAME f-pg-par tg-trans-16 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "IPL,".
END.
IF INPUT FRAME f-pg-par tg-trans-17 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "MOB,".
END.
IF INPUT FRAME f-pg-par tg-trans-18 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "NC,".
END.
IF INPUT FRAME f-pg-par tg-trans-19 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "NF,".
END.
IF INPUT FRAME f-pg-par tg-trans-20 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "NFD,".
END.
IF INPUT FRAME f-pg-par tg-trans-21 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "NFE,".
END.
IF INPUT FRAME f-pg-par tg-trans-22 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "NFS,".
END.
IF INPUT FRAME f-pg-par tg-trans-23 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "NFT,".
END.
IF INPUT FRAME f-pg-par tg-trans-24 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "NUS,".
END.
IF INPUT FRAME f-pg-par tg-trans-25 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "REF,".
END.
IF INPUT FRAME f-pg-par tg-trans-26 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "RCS,".
END.
IF INPUT FRAME f-pg-par tg-trans-27 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "RDD,".
END.
IF INPUT FRAME f-pg-par tg-trans-28 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "REQ,".
END.
IF INPUT FRAME f-pg-par tg-trans-29 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "RFS,".
END.
IF INPUT FRAME f-pg-par tg-trans-30 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "RM,".
END.
IF INPUT FRAME f-pg-par tg-trans-31 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "RRQ,".
END.
IF INPUT FRAME f-pg-par tg-trans-32 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "STR,".
END.
IF INPUT FRAME f-pg-par tg-trans-33 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "TRA,".
END.
IF INPUT FRAME f-pg-par tg-trans-34 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "ZZZ,".
END.
IF INPUT FRAME f-pg-par tg-trans-35 = YES THEN
DO:
    ASSIGN str-trans = str-trans + "SOB,".
END.

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


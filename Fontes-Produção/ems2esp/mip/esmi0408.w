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
{include/i-prgvrs.i MI0408 2.06.00.022}  /*** 010022 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i mi0408 MMI}
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
{cdp/cdcfgmnt.i} /* Versäes EMS MNT */
{cdp/cd9911.i}  /* Varifica Unidade Neg¢cio */

/* Preprocessadores do Template de Relat¢rio                            */
/* Obs: Retirar o valor do preprocessador para as p ginas que nÆo existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA f-pg-cla
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG
&GLOBAL-DEFINE PGIMP f-pg-imp

/* Include Com as Vari veis Globais */
/* {utp/ut-glob.i} */

/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

define temp-table tt-param
    field destino               as integer
    field arquivo               as char    format "x(40)"
    field usuario               as char
    field data-exec             as date
    field hora-exec             as integer
    field classifica            as integer
    field c-cc-ini              as char
    field c-cc-fim              as char
    field i-ordem-ini           as int    format 999999999
    field i-ordem-fim           as int    format 999999999
    field c-equipto-ini         as char   format "x(16)"
    field c-equipto-fim         as char   format "x(16)"
    field c-tag-ini             as char   format "x(16)"
    field c-tag-fim             as char   format "x(16)"
    field c-planeja-ini         as char   format "x(08)"
    field c-planeja-fim         as char   format "x(08)"
    field c-equipe-ini          as char   format "x(08)"
    field c-equipe-fim          as char   format "x(08)"
    field da-data-ini           as date
    field da-data-fim           as date
    field i-prioridade-ini      as int
    field i-prioridade-fim      as int   
    field l-susp                as log
    field l-nao-inic            as log
    field l-inic                as log
    field l-term                as log
    field l-final               as log
    field l-liber               as log
    field l-aloc                as log
    field l-requi               as log
    field l-separ               as log
    field l-narrativa           as log
    field l-quebra-pag          as log
    field l-item                as log
    field l-ferramenta          as log
    field l-equip-protecao      as log
    field l-ficha-metodo        as log
    field i-cod-tipo            as INT  /* Valor Default: 999*/
    field desc-classifica       as char format "x(40)"
&IF "{&mguni_version}" >= "2.071" &THEN
    field c-estabel-ini         as char format "x(05)"
&ELSE
    field c-estabel-ini         as char format "x(03)"
&ENDIF
&IF "{&mguni_version}" >= "2.071" &THEN
    field c-estabel-fim         as char format "x(05)"
&ELSE
    field c-estabel-fim         as char format "x(03)"
&ENDIF
    field c-familia-ini         as char format "x(08)" 
    field c-familia-fim         as char format "x(08)"
    field i-tp-manut-ini        as integer
    field i-tp-manut-fim        as integer
    FIELD c-cod-unid-negoc-ini  AS CHAR FORMAT "x(03)"
    FIELD c-cod-unid-negoc-fim  AS CHAR FORMAT "x(03)".

define temp-table tt-digita
    field ordem                 as integer   format ">>>>9"
    field exemplo               as character format "x(30)"
    index id is primary unique
        ordem.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.

/* Local Variable Definitions ---                                       */

def new global shared var rw-periodo as rowid no-undo.
def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.

def var rw-per-parada      as rowid   no-undo.
def var l-confirma         as logical no-undo.

DEFINE VARIABLE c-cod-unid-negoc-ini AS CHARACTER INITIAL "":U  NO-UNDO.
DEFINE VARIABLE c-cod-unid-negoc-fim AS CHARACTER INITIAL "ZZZ":U  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-cla

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-36 rs-classif 
&Scoped-Define DISPLAYED-OBJECTS rs-classif FILL-IN-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Classificar por" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .67 NO-UNDO.

DEFINE VARIABLE rs-classif AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Centro de Custo", 1,
"Equipamento", 2,
"Descri‡Æo", 3,
"Planejador", 4,
"Tag", 5,
"Equipe", 6
     SIZE 24.29 BY 6.79 NO-UNDO.

DEFINE RECTANGLE RECT-36
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33.72 BY 8.04.

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

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Considerar" 
      VIEW-AS TEXT 
     SIZE 7.86 BY .67 NO-UNDO.

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71 BY 3.63.

DEFINE VARIABLE l-aloc AS LOGICAL INITIAL yes 
     LABEL "Ordens Alocadas" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.43 BY .63 NO-UNDO.

DEFINE VARIABLE l-equip-protecao AS LOGICAL INITIAL yes 
     LABEL "Equipamento de Prote‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .63 NO-UNDO.

DEFINE VARIABLE l-ferramenta AS LOGICAL INITIAL yes 
     LABEL "Ferramenta" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .63 NO-UNDO.

DEFINE VARIABLE l-ficha-metodo AS LOGICAL INITIAL yes 
     LABEL "Ficha M‚todo" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .63 NO-UNDO.

DEFINE VARIABLE l-final AS LOGICAL INITIAL yes 
     LABEL "Ordens Finalizadas" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.72 BY .63 NO-UNDO.

DEFINE VARIABLE l-inic AS LOGICAL INITIAL yes 
     LABEL "Ordens Iniciadas" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.14 BY .63 NO-UNDO.

DEFINE VARIABLE l-item AS LOGICAL INITIAL yes 
     LABEL "Item" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY .63 NO-UNDO.

DEFINE VARIABLE l-liber AS LOGICAL INITIAL yes 
     LABEL "Ordens Liberadas" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.43 BY .63 NO-UNDO.

DEFINE VARIABLE l-nao-inic AS LOGICAL INITIAL yes 
     LABEL "Ordens nÆo Iniciadas" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.57 BY .63 NO-UNDO.

DEFINE VARIABLE l-quebra-pag AS LOGICAL INITIAL yes 
     LABEL "Insere quebra de P gina" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.72 BY .63 NO-UNDO.

DEFINE VARIABLE l-requi AS LOGICAL INITIAL yes 
     LABEL "Ordens Requisitadas" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .63 NO-UNDO.

DEFINE VARIABLE l-separ AS LOGICAL INITIAL yes 
     LABEL "Ordens Separadas" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.29 BY .63 NO-UNDO.

DEFINE VARIABLE l-susp AS LOGICAL INITIAL yes 
     LABEL "Ordens Suspensas" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.29 BY .63 NO-UNDO.

DEFINE VARIABLE l-term AS LOGICAL INITIAL yes 
     LABEL "Ordens Terminadas" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.86 BY .63 NO-UNDO.

DEFINE BUTTON BtSelecao 
     IMAGE-UP FILE "image/im-expan.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-expan.bmp":U
     LABEL "Button 1" 
     SIZE 10 BY 1.13.

DEFINE VARIABLE c-cc-fim AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE c-cc-ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Centro Custo":R15 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE c-equipe-fim AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE c-equipe-ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Equipe":R8 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE c-equipto-fim AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE c-equipto-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Equipamento":R14 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE c-estabel-fim LIKE estabelec.cod-estabel
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE c-estabel-ini LIKE estabelec.cod-estabel
     LABEL "Estabelecimento":R15 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE c-familia-fim AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE c-familia-ini AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fam¡lia" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE c-planeja-fim AS CHARACTER FORMAT "x(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-planeja-ini AS CHARACTER FORMAT "x(12)" 
     LABEL "Planejador":R12 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-tag-fim AS CHARACTER FORMAT "x(16)" INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE c-tag-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Tag":R4 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE da-data-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE da-data-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Data" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tp-manut-fim AS INTEGER FORMAT ">>,>>9":U INITIAL 99999 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tp-manut-ini AS INTEGER FORMAT ">>,>>9":U INITIAL 0 
     LABEL "Tipo Manuten‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE i-ordem-fim AS DECIMAL FORMAT "999,999,999" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE i-ordem-ini AS DECIMAL FORMAT "999,999,999" INITIAL 0 
     LABEL "Ordem Manuten‡Æo":R20 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE i-prioridade-fim AS INTEGER FORMAT ">>9" INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE i-prioridade-ini AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Prioridade":R18 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-13
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-15
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-16
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-35
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-36
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-37
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-38
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-39
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-40
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-42
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-43
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-fir":U
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
     SIZE 78 BY 11.38
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
         AT COL 3 ROW 2.83
         SIZE 77.43 BY 10.92.

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     RECT-1 AT ROW 14.29 COL 2
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     im-pg-cla AT ROW 1.5 COL 17.72
     im-pg-imp AT ROW 1.5 COL 48.86
     im-pg-par AT ROW 1.5 COL 33.29
     im-pg-sel AT ROW 1.5 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-cla
     rs-classif AT ROW 2.08 COL 4 HELP
          "Classifica‡Æo para emissÆo do relat¢rio" NO-LABEL
     FILL-IN-1 AT ROW 1.08 COL 2 COLON-ALIGNED NO-LABEL
     RECT-36 AT ROW 1.29 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.71
         SIZE 77.43 BY 11.04
         FONT 1.

DEFINE FRAME f-pg-sel
     c-cc-ini AT ROW 1.08 COL 21.43 COLON-ALIGNED HELP
          "Centro de Custo do Equipamento"
     c-cc-fim AT ROW 1.08 COL 46.86 COLON-ALIGNED HELP
          "Centro de Custo do Equipamento" NO-LABEL
     i-ordem-ini AT ROW 2 COL 21.43 COLON-ALIGNED HELP
          "N£mero da Ordem de Manuten‡Æo"
     i-ordem-fim AT ROW 2 COL 46.86 COLON-ALIGNED HELP
          "N£mero da Ordem de Manuten‡Æo" NO-LABEL
     c-equipto-ini AT ROW 2.92 COL 21.43 COLON-ALIGNED HELP
          "C¢digo do Equipamento"
     c-equipto-fim AT ROW 2.92 COL 46.86 COLON-ALIGNED HELP
          "C¢digo do Equipamento" NO-LABEL
     c-tag-ini AT ROW 3.88 COL 21.43 COLON-ALIGNED HELP
          "Localizacao Funcional (Tag)"
     c-tag-fim AT ROW 3.88 COL 46.86 COLON-ALIGNED HELP
          "Localizacao Funcional (Tag)" NO-LABEL
     fi-tp-manut-ini AT ROW 4.88 COL 21.43 COLON-ALIGNED
     fi-tp-manut-fim AT ROW 4.88 COL 46.86 COLON-ALIGNED NO-LABEL
     c-planeja-ini AT ROW 5.88 COL 21.43 COLON-ALIGNED HELP
          "C¢digo do Planejador"
     c-planeja-fim AT ROW 5.88 COL 46.86 COLON-ALIGNED HELP
          "C¢digo do Planejador" NO-LABEL
     c-equipe-ini AT ROW 6.88 COL 21.43 COLON-ALIGNED HELP
          "C¢digo da Equipe de Manuten‡Æo"
     c-equipe-fim AT ROW 6.88 COL 46.86 COLON-ALIGNED HELP
          "C¢digo da Equipe de Manuten‡Æo" NO-LABEL
     da-data-ini AT ROW 7.88 COL 21.43 COLON-ALIGNED
     da-data-fim AT ROW 7.88 COL 46.86 COLON-ALIGNED NO-LABEL
     c-estabel-ini AT ROW 8.88 COL 21.43 COLON-ALIGNED HELP
          ""
          LABEL "Estabelecimento":R15
     c-estabel-fim AT ROW 8.88 COL 47 COLON-ALIGNED HELP
          "" NO-LABEL
     i-prioridade-ini AT ROW 9.88 COL 21.43 COLON-ALIGNED HELP
          "Prioridade da Ordem de Manuten‡Æo"
     i-prioridade-fim AT ROW 9.88 COL 46.86 COLON-ALIGNED HELP
          "Prioridade da Ordem de Manuten‡Æo" NO-LABEL
     BtSelecao AT ROW 10.63 COL 67.72
     c-familia-ini AT ROW 10.88 COL 21.43 COLON-ALIGNED
     c-familia-fim AT ROW 10.88 COL 46.86 COLON-ALIGNED NO-LABEL
     IMAGE-43 AT ROW 10.88 COL 45.86
     IMAGE-42 AT ROW 10.88 COL 41.86
     IMAGE-40 AT ROW 10 COL 45.86
     IMAGE-11 AT ROW 3 COL 45.86
     IMAGE-6 AT ROW 4 COL 41.86
     IMAGE-13 AT ROW 4 COL 45.86
     IMAGE-38 AT ROW 5 COL 41.86
     IMAGE-37 AT ROW 5 COL 45.86
     IMAGE-7 AT ROW 6 COL 41.86
     IMAGE-14 AT ROW 6 COL 45.86
     IMAGE-8 AT ROW 7 COL 41.86
     IMAGE-3 AT ROW 2 COL 41.86
     IMAGE-2 AT ROW 1 COL 45.86
     IMAGE-1 AT ROW 1 COL 41.86
     IMAGE-10 AT ROW 2 COL 45.86
     IMAGE-16 AT ROW 8 COL 45.86
     IMAGE-15 AT ROW 7 COL 45.86
     IMAGE-4 AT ROW 3 COL 41.86
     IMAGE-9 AT ROW 8 COL 41.86
     IMAGE-39 AT ROW 10 COL 41.86
     IMAGE-36 AT ROW 9 COL 45.86
     IMAGE-35 AT ROW 9 COL 41.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.83
         SIZE 77.29 BY 10.88
         FONT 1.

DEFINE FRAME f-pg-par
     l-susp AT ROW 1.71 COL 4.29
     l-liber AT ROW 1.71 COL 30
     l-nao-inic AT ROW 2.33 COL 4.29
     l-aloc AT ROW 2.33 COL 30
     l-inic AT ROW 2.96 COL 4.29
     l-separ AT ROW 2.96 COL 30
     l-term AT ROW 3.54 COL 4.29
     l-requi AT ROW 3.54 COL 30
     l-final AT ROW 4.17 COL 4.29
     l-quebra-pag AT ROW 5.75 COL 4.29
     l-item AT ROW 5.75 COL 30
     l-ferramenta AT ROW 6.5 COL 30
     l-equip-protecao AT ROW 7.25 COL 30
     l-ficha-metodo AT ROW 8 COL 30
     FILL-IN-2 AT ROW 1 COL 1.43 COLON-ALIGNED NO-LABEL
     RECT-39 AT ROW 1.33 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.75
         SIZE 77.43 BY 10.96
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Ordem de Manuten‡Æo em Aberto"
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
/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME f-pg-cla
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

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FILL-IN c-estabel-fim IN FRAME f-pg-sel
   LIKE = mgadm.estabelec.cod-estabel EXP-LABEL EXP-SIZE                */
/* SETTINGS FOR FILL-IN c-estabel-ini IN FRAME f-pg-sel
   LIKE = mgadm.estabelec.cod-estabel EXP-LABEL EXP-SIZE                */
/* SETTINGS FOR IMAGE IMAGE-1 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-10 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-11 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-13 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-14 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-15 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-16 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-2 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-3 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-35 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-36 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-37 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-38 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-39 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-4 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-40 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-42 IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE IMAGE-43 IN FRAME f-pg-sel
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
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Ordem de Manuten‡Æo em Aberto */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Ordem de Manuten‡Æo em Aberto */
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
ON CHOOSE OF bt-cancelar IN FRAME f-relat /* Fechar */
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
    RUN pi-executar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME BtSelecao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtSelecao C-Win
ON CHOOSE OF BtSelecao IN FRAME f-pg-sel /* Button 1 */
DO:

RUN mip/mi0408b.w(INPUT-OUTPUT c-cod-unid-negoc-ini,
                    INPUT-OUTPUT c-cod-unid-negoc-fim).
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
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "MI0408" "2.06.00.022"}

/* inicializa‡äes do template de relat¢rio */
{include/i-rpini.i}

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

{include/i-rplbl.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

ASSIGN c-estabel-fim = &IF "{&mguni_version}" >= "2.071" &THEN "ZZZZZ":U &ELSE "ZZZ":U &ENDIF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:


    RUN enable_UI.

    ASSIGN btSelecao:VISIBLE IN FRAME f-pg-sel = l-usa-unid-negoc.

    {include/i-rpmbl.i}
     {mip/mi9999.i}
    IF  NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

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
  ENABLE im-pg-cla im-pg-imp im-pg-par im-pg-sel bt-executar bt-cancelar 
         bt-ajuda 
      WITH FRAME f-relat IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY rs-classif FILL-IN-1 
      WITH FRAME f-pg-cla IN WINDOW C-Win.
  ENABLE RECT-36 rs-classif 
      WITH FRAME f-pg-cla IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-cla}
  DISPLAY l-susp l-liber l-nao-inic l-aloc l-inic l-separ l-term l-requi l-final 
          l-quebra-pag l-item l-ferramenta l-equip-protecao l-ficha-metodo 
          FILL-IN-2 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  ENABLE RECT-39 l-susp l-liber l-nao-inic l-aloc l-inic l-separ l-term l-requi 
         l-final l-quebra-pag l-item l-ferramenta l-equip-protecao 
         l-ficha-metodo FILL-IN-2 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  ENABLE RECT-7 RECT-9 rs-destino bt-arquivo bt-config-impr c-arquivo 
         rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY c-cc-ini c-cc-fim i-ordem-ini i-ordem-fim c-equipto-ini c-equipto-fim 
          c-tag-ini c-tag-fim fi-tp-manut-ini fi-tp-manut-fim c-planeja-ini 
          c-planeja-fim c-equipe-ini c-equipe-fim da-data-ini da-data-fim 
          c-estabel-ini c-estabel-fim i-prioridade-ini i-prioridade-fim 
          c-familia-ini c-familia-fim 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE c-cc-ini c-cc-fim i-ordem-ini i-ordem-fim c-equipto-ini c-equipto-fim 
         c-tag-ini c-tag-fim fi-tp-manut-ini fi-tp-manut-fim c-planeja-ini 
         c-planeja-fim c-equipe-ini c-equipe-fim da-data-ini da-data-fim 
         c-estabel-ini c-estabel-fim i-prioridade-ini i-prioridade-fim 
         BtSelecao c-familia-ini c-familia-fim 
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

DEFINE VARIABLE rp-program AS CHARACTER  NO-UNDO.

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

    if input frame f-pg-par l-nao-inic:checked = no and
       input frame f-pg-par l-inic:checked     = no and
       input frame f-pg-par l-term:checked     = no and
       input frame f-pg-par l-final:checked    = no and
       input frame f-pg-par l-liber:checked    = no and
       input frame f-pg-par l-aloc:checked     = no and
       input frame f-pg-par l-separ:checked    = no and
       input frame f-pg-par l-requi:checked    = no AND 
       input frame f-pg-par l-susp:checked     = NO then do:

       run utp/ut-msgs.p (input "show",
                                input 27243,
                                input "").
       return no-apply.

    end.     
    create tt-param.
    assign tt-param.usuario    = c-seg-usuario
           tt-param.destino    = input frame f-pg-imp rs-destino
           tt-param.data-exec  = today
           tt-param.hora-exec  = time
           tt-param.classifica = input frame f-pg-cla rs-classif.

    if  tt-param.destino = 1 then
        assign tt-param.arquivo = "".
    else
    if  tt-param.destino = 2 then 
        assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
    else
        assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".

    /* Coloque aqui a l¢gica de grava‡Æo dos parƒmtros e sele‡Æo na temp-table
       tt-param */ 

       assign tt-param.c-cc-ini      = input frame f-pg-sel c-cc-ini
              tt-param.c-cc-fim      = input frame f-pg-sel c-cc-fim
              tt-param.i-ordem-ini   = input frame f-pg-sel i-ordem-ini
              tt-param.i-ordem-fim   = input frame f-pg-sel i-ordem-fim
              tt-param.c-equipto-ini = input frame f-pg-sel c-equipto-ini
              tt-param.c-equipto-fim = input frame f-pg-sel c-equipto-fim
              tt-param.c-tag-ini     = input frame f-pg-sel c-tag-ini
              tt-param.c-tag-fim     = input frame f-pg-sel c-tag-fim
              tt-param.c-planeja-ini = input frame f-pg-sel c-planeja-ini
              tt-param.c-planeja-fim = input frame f-pg-sel c-planeja-fim
              tt-param.c-equipe-ini  = input frame f-pg-sel c-equipe-ini
              tt-param.c-equipe-fim  = input frame f-pg-sel c-equipe-fim
              tt-param.da-data-ini   = input frame f-pg-sel da-data-ini
              tt-param.da-data-fim   = input frame f-pg-sel da-data-fim
              tt-param.i-prioridade-ini = input frame f-pg-sel i-prioridade-ini
              tt-param.i-prioridade-fim = input frame f-pg-sel i-prioridade-fim              
              tt-param.l-susp        = input frame f-pg-par l-susp
              tt-param.l-nao-inic    = input frame f-pg-par l-nao-inic
              tt-param.l-inic        = input frame f-pg-par l-inic
              tt-param.l-term        = input frame f-pg-par l-term
              tt-param.l-final       = input frame f-pg-par l-final
              tt-param.l-liber       = input frame f-pg-par l-liber
              tt-param.l-aloc        = input frame f-pg-par l-aloc
              tt-param.l-requi       = input frame f-pg-par l-requi
              tt-param.l-separ       = input frame f-pg-par l-separ
              tt-param.l-quebra-pag  = input frame f-pg-par l-quebra-pag
              tt-param.l-item           = input frame f-pg-par l-item
              tt-param.l-ferramenta     = input frame f-pg-par l-ferramenta
              tt-param.l-equip-protecao = input frame f-pg-par l-equip-protecao
              tt-param.l-ficha-metodo   = input frame f-pg-par l-ficha-metodo
              tt-param.i-cod-tipo    = 999
              tt-param.c-estabel-ini = input frame f-pg-sel c-estabel-ini
              tt-param.c-estabel-fim = input frame f-pg-sel c-estabel-fim
              tt-param.c-familia-ini = input frame f-pg-sel c-familia-ini
              tt-param.c-familia-fim = input frame f-pg-sel c-familia-fim
              tt-param.i-tp-manut-ini = input frame f-pg-sel fi-tp-manut-ini
              tt-param.i-tp-manut-fim = input frame f-pg-sel fi-tp-manut-fim
              tt-param.c-cod-unid-negoc-ini = c-cod-unid-negoc-ini
              tt-param.c-cod-unid-negoc-fim = c-cod-unid-negoc-fim.

       assign  tt-param.desc-classifica = 
                entry((tt-param.classifica - 1) * 2 + 1, rs-classif:radio-buttons in frame f-pg-cla).

/******************** Chama UPC da YAMANA *********************************/ 

       /*IF (c-nom-prog-upc-mg97 <> ""  AND
           c-nom-prog-upc-mg97 <> ? ) THEN DO:

           RUN VALUE(c-nom-prog-upc-mg97) (INPUT "YAMANA":U,
                                           INPUT "",
                                           INPUT THIS-PROCEDURE,
                                           INPUT FRAME f-relat:HANDLE,  
                                           INPUT "",
                                           INPUT ?).

           IF RETURN-VALUE = "ESMI0408":U THEN DO:*/
               ASSIGN rp-program = "mip/esmi0408rp.p":U.
           /*END.
           ELSE DO:
               ASSIGN rp-program = "mip/mi0408rp.p":U.
           END.
       END.
       ELSE DO:
           ASSIGN rp-program = "mip/mi0408rp.p":U.
       END.*/

/********************************************************************/

    {include/i-rpexb.i}
    session:set-wait-state("general":U).

    {include/i-rprun.i VALUE(rp-program)}

    {include/i-rpexc.i}

    session:set-wait-state("":U).

    {include/i-rptrm.i}

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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this Window, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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


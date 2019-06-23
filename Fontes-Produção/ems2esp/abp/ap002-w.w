&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems5             PROGRESS
          ems5_esp         PROGRESS
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i AP002-W 2.06.00.005}
    
{cdp/cdcfgmat.i}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
/* Obs: Retirar o valor do preprocessador para as p ginas que nÆo existirem  */
/* Local Variable Definitions ---                                       */
DEF TEMP-TABLE tt_file
    FIELD arquivo AS CHAR.

DEFINE VARIABLE c-new-file AS CHARACTER  NO-UNDO.
DEFINE VARIABLE hProgramZoom AS HANDLE   NO-UNDO.

def new global shared var v_rec_cta_ctbl_integr as RECID format ">>>>>>9":U initial ? no-undo.
DEF NEW GLOBAL SHARED VAR  v_rec_ccusto AS RECID format ">>>>>>9":U initial ? no-undo.


define temp-table tt-param no-undo
    field destino          as integer
    field c-destino        as char 
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as INTEGER
    FIELD tipo             AS INT.

{apb\ap001-rp1.i NEW}

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */
def var l-ok as logical no-undo. 


DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE rs-execucao AS INTEGER     NO-UNDO INITIAL 3.


DEF FRAME f-pg-imp.
DEF FRAME f-relat.

FORM c-arquivo rs-execucao WITH FRAME f-pg-imp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-relat
&Scoped-define BROWSE-NAME BROWSE-11

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-titulo log_concur_erros tt-rateio

/* Definitions for BROWSE BROWSE-11                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-11 tt-titulo.log_reprocess tt-titulo.cod_empresa tt-titulo.cod_estab tt-titulo.cod_esp tt-titulo.cdn_fornec tt-titulo.titulo tt-titulo.datahora   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-11 tt-titulo.log_reprocess   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-11 tt-titulo
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-11 tt-titulo
&Scoped-define SELF-NAME BROWSE-11
&Scoped-define QUERY-STRING-BROWSE-11 FOR EACH tt-titulo
&Scoped-define OPEN-QUERY-BROWSE-11 OPEN QUERY {&SELF-NAME} FOR EACH tt-titulo.
&Scoped-define TABLES-IN-QUERY-BROWSE-11 tt-titulo
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-11 tt-titulo


/* Definitions for BROWSE BROWSE-12                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-12 log_concur_erros.linha ~
(if log_concur_erros.tipo = 1 then "ERRO" else "SUCESSO") 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-12 
&Scoped-define QUERY-STRING-BROWSE-12 FOR EACH log_concur_erros NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-12 OPEN QUERY BROWSE-12 FOR EACH log_concur_erros NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-12 log_concur_erros
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-12 log_concur_erros


/* Definitions for BROWSE BROWSE-6                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-6 tt-rateio.seq tt-rateio.ccontabil tt-rateio.ccusto tt-rateio.valor tt-rateio.lancto   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-6 tt-rateio.ccontabil ~
tt-rateio.ccusto   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-6 tt-rateio
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-6 tt-rateio
&Scoped-define SELF-NAME BROWSE-6
&Scoped-define QUERY-STRING-BROWSE-6 FOR EACH tt-rateio NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-6 OPEN QUERY {&SELF-NAME} FOR EACH tt-rateio NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-6 tt-rateio
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-6 tt-rateio


/* Definitions for FRAME f-relat                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-6 BROWSE-12 BROWSE-11 bt-processar ~
bt-buscar bt-refresh fi_cod_fornec_ini fi_cod_fornec_fim fi-dt-atualiza-ini ~
fi-dt-atualiza-fim fi-cod-estabel-ini fi-cod-estabel-fim rd-tipo ed_log ~
bt-deletar rt-button RECT-1 RECT-2 IMAGE-7 IMAGE-8 IMAGE-9 IMAGE-10 ~
IMAGE-11 IMAGE-12 RECT-4 RECT-5 
&Scoped-Define DISPLAYED-OBJECTS d-valor c-esp c-forn c-par c-ser c-tit ~
c_est c_nom_fornec fi_cod_fornec_ini fi_cod_fornec_fim fi-dt-atualiza-ini ~
fi-dt-atualiza-fim fi-cod-estabel-ini fi-cod-estabel-fim rd-tipo ed_log 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-livre AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-programa 
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-livre MENUBAR
       SUB-MENU  mi-programa    LABEL "&Nome-do-Programa"
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-buscar 
     IMAGE-UP FILE "image/search.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Buscar" 
     SIZE 4.14 BY 1.25 TOOLTIP "Search data".

DEFINE BUTTON bt-deletar 
     IMAGE-UP FILE "image/delete.png":U
     IMAGE-DOWN FILE "image/delete.png":U
     IMAGE-INSENSITIVE FILE "image/delete.png":U
     LABEL "Deleta Log" 
     SIZE 4.14 BY 1.25 TOOLTIP "Elimina log".

DEFINE BUTTON bt-processar 
     IMAGE-UP FILE "image/process_icon.jpg":U
     IMAGE-DOWN FILE "image/process_ins.png":U
     IMAGE-INSENSITIVE FILE "image/process_ins.png":U
     LABEL "Buscar" 
     SIZE 4.14 BY 1.25 TOOLTIP "Process data".

DEFINE BUTTON bt-refresh 
     IMAGE-UP FILE "image/refresh.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Buscar" 
     SIZE 4.14 BY 1.25 TOOLTIP "Refresh Browse".

DEFINE VARIABLE ed_log AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 64 BY 6.5 NO-UNDO.

DEFINE VARIABLE c-esp AS CHARACTER FORMAT "X(03)":U 
     LABEL "Esp‚cie" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE c-forn AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Fornecedor" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE c-par AS CHARACTER FORMAT "X(3)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE c-ser AS CHARACTER FORMAT "X(03)":U 
     LABEL "Serie" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE c-tit AS CHARACTER FORMAT "X(16)":U 
     LABEL "T¡tulo" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE c_est AS CHARACTER FORMAT "X(05)":U 
     LABEL "Estab" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE c_nom_fornec AS CHARACTER FORMAT "X(45)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY .88 NO-UNDO.

DEFINE VARIABLE d-valor AS CHARACTER FORMAT "X(256)":U 
     LABEL "Valor" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel-fim AS CHARACTER FORMAT "X(3)" INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel-ini AS CHARACTER FORMAT "X(3)" 
     LABEL "Estab":R7 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-atualiza-fim AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-atualiza-ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Data Log":R17 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi_cod_fornec_fim AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi_cod_fornec_ini AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Cod. Fornecedor" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
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

DEFINE VARIABLE rd-tipo AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Com Erro", 1,
"Sucesso", 2
     SIZE 16 BY 2.5 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 116 BY 3.46.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 116 BY 7.25.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 45 BY 8.5.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69 BY 8.5.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 117 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-11 FOR 
      tt-titulo SCROLLING.

DEFINE QUERY BROWSE-12 FOR 
      log_concur_erros SCROLLING.

DEFINE QUERY BROWSE-6 FOR 
      tt-rateio SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-11 w-livre _FREEFORM
  QUERY BROWSE-11 NO-LOCK DISPLAY
      tt-titulo.log_reprocess   COLUMN-LABEL "Reproc"     FORMAT "Sim/NÆo":U VIEW-AS TOGGLE-BOX
      tt-titulo.cod_empresa     COLUMN-LABEL "Emp"        FORMAT "x(3)":U WIDTH 5
      tt-titulo.cod_estab       column-label "Est"        FORMAT "x(05)":U
      tt-titulo.cod_esp         column-label "Esp"        FORMAT "x(4)":U
      tt-titulo.cdn_fornec      column-label "Fornecedor" FORMAT ">>>,>>>,>>9":U
      tt-titulo.titulo          column-label "Titulo"     FORMAT "x(16)":U
      tt-titulo.datahora        column-label "Data Log"   FORMAT "99/99/9999 HH:MM:SS.SSS":U
  ENABLE
      tt-titulo.log_reprocess
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS MULTIPLE SIZE 43 BY 6.75 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-12 w-livre _STRUCTURED
  QUERY BROWSE-12 NO-LOCK DISPLAY
      log_concur_erros.linha COLUMN-LABEL "Seq" FORMAT ">>>9":U
      (if log_concur_erros.tipo = 1 then "ERRO" else "SUCESSO") COLUMN-LABEL "Tipo" FORMAT "x(07)":U
            WIDTH 10.29
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 18.57 BY 6.75 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-6 w-livre _FREEFORM
  QUERY BROWSE-6 NO-LOCK DISPLAY
      tt-rateio.seq     COLUMN-LABEL "Seq." FORMAT ">>9":U
      tt-rateio.ccontabil   FORMAT "x(16)":U
      tt-rateio.ccusto  FORMAT "x(8)":U
      tt-rateio.valor   FORMAT "->>>,>>>,>>>,>>9.99":U
      tt-rateio.lancto COLUMN-LABEL "DB/CR" FORMAT "x(04)":U
            WIDTH 4.57
  ENABLE
      tt-rateio.ccontabil
      tt-rateio.ccusto
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 49 BY 6.75 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
     BROWSE-6 AT ROW 6.54 COL 47.57 WIDGET-ID 300
     BROWSE-12 AT ROW 6.5 COL 98 WIDGET-ID 500
     BROWSE-11 AT ROW 6.5 COL 3 WIDGET-ID 400
     d-valor AT ROW 19.25 COL 13 COLON-ALIGNED WIDGET-ID 114
     bt-processar AT ROW 1.13 COL 2.29 WIDGET-ID 72
     bt-buscar AT ROW 1.13 COL 91 WIDGET-ID 26
     bt-refresh AT ROW 1.13 COL 86 WIDGET-ID 70
     c-esp AT ROW 15.25 COL 13 COLON-ALIGNED WIDGET-ID 100
     c-forn AT ROW 17.25 COL 13 COLON-ALIGNED WIDGET-ID 102
     c-par AT ROW 16.21 COL 33.29 COLON-ALIGNED NO-LABEL WIDGET-ID 98
     c-ser AT ROW 14.25 COL 28 COLON-ALIGNED WIDGET-ID 94
     c-tit AT ROW 16.25 COL 13 COLON-ALIGNED WIDGET-ID 96
     c_est AT ROW 14.25 COL 13 COLON-ALIGNED WIDGET-ID 92
     c_nom_fornec AT ROW 18.29 COL 13 COLON-ALIGNED NO-LABEL WIDGET-ID 104
     fi_cod_fornec_ini AT ROW 2.83 COL 22 COLON-ALIGNED WIDGET-ID 62
     fi_cod_fornec_fim AT ROW 2.83 COL 45.29 COLON-ALIGNED NO-LABEL WIDGET-ID 64
     fi-dt-atualiza-ini AT ROW 3.83 COL 23 COLON-ALIGNED WIDGET-ID 56
     fi-dt-atualiza-fim AT ROW 3.83 COL 45.29 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     fi-cod-estabel-ini AT ROW 4.83 COL 29 COLON-ALIGNED HELP
          "C¢digo do estabelecimento" WIDGET-ID 40
     fi-cod-estabel-fim AT ROW 4.83 COL 45.29 COLON-ALIGNED HELP
          "C¢digo do estabelecimento" NO-LABEL WIDGET-ID 46
     rd-tipo AT ROW 3 COL 71 NO-LABEL WIDGET-ID 66
     ed_log AT ROW 15 COL 51 NO-LABEL WIDGET-ID 74
     bt-deletar AT ROW 1.13 COL 78 WIDGET-ID 112
     "Descri‡Æo do log:" VIEW-AS TEXT
          SIZE 17 BY .67 AT ROW 14.29 COL 51 WIDGET-ID 76
     "/" VIEW-AS TEXT
          SIZE 2 BY .67 AT ROW 16.25 COL 33.29 WIDGET-ID 116
     rt-button AT ROW 1 COL 1
     RECT-1 AT ROW 2.58 COL 1.57 WIDGET-ID 2
     RECT-2 AT ROW 6.25 COL 1.57 WIDGET-ID 4
     IMAGE-7 AT ROW 2.83 COL 39 WIDGET-ID 22
     IMAGE-8 AT ROW 2.83 COL 43 WIDGET-ID 24
     IMAGE-9 AT ROW 3.83 COL 39 WIDGET-ID 32
     IMAGE-10 AT ROW 3.83 COL 43 WIDGET-ID 34
     IMAGE-11 AT ROW 4.83 COL 39 WIDGET-ID 44
     IMAGE-12 AT ROW 4.83 COL 43 WIDGET-ID 42
     RECT-4 AT ROW 13.75 COL 1.57 WIDGET-ID 108
     RECT-5 AT ROW 13.75 COL 48.43 WIDGET-ID 110
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 117.72 BY 21.33 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-livre
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "Documentos por Usu rio"
         HEIGHT             = 21.33
         WIDTH              = 117.72
         MAX-HEIGHT         = 21.33
         MAX-WIDTH          = 117.72
         VIRTUAL-HEIGHT     = 21.33
         VIRTUAL-WIDTH      = 117.72
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-livre:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-livre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-livre.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-livre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-relat
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BROWSE-6 1 f-relat */
/* BROWSE-TAB BROWSE-12 BROWSE-6 f-relat */
/* BROWSE-TAB BROWSE-11 BROWSE-12 f-relat */
/* SETTINGS FOR FILL-IN c-esp IN FRAME f-relat
   NO-ENABLE                                                            */
ASSIGN 
       c-esp:READ-ONLY IN FRAME f-relat        = TRUE.

/* SETTINGS FOR FILL-IN c-forn IN FRAME f-relat
   NO-ENABLE                                                            */
ASSIGN 
       c-forn:READ-ONLY IN FRAME f-relat        = TRUE.

/* SETTINGS FOR FILL-IN c-par IN FRAME f-relat
   NO-ENABLE                                                            */
ASSIGN 
       c-par:READ-ONLY IN FRAME f-relat        = TRUE.

/* SETTINGS FOR FILL-IN c-ser IN FRAME f-relat
   NO-ENABLE                                                            */
ASSIGN 
       c-ser:READ-ONLY IN FRAME f-relat        = TRUE.

/* SETTINGS FOR FILL-IN c-tit IN FRAME f-relat
   NO-ENABLE                                                            */
ASSIGN 
       c-tit:READ-ONLY IN FRAME f-relat        = TRUE.

/* SETTINGS FOR FILL-IN c_est IN FRAME f-relat
   NO-ENABLE                                                            */
ASSIGN 
       c_est:READ-ONLY IN FRAME f-relat        = TRUE.

/* SETTINGS FOR FILL-IN c_nom_fornec IN FRAME f-relat
   NO-ENABLE                                                            */
ASSIGN 
       c_nom_fornec:READ-ONLY IN FRAME f-relat        = TRUE.

/* SETTINGS FOR FILL-IN d-valor IN FRAME f-relat
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-11
/* Query rebuild information for BROWSE BROWSE-11
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-titulo
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "ems5_esp.log_concur_titulo.tipo = rd-tipo
 AND ems5_esp.log_concur_titulo.cdn_fornec >= fi_cod_fornec_ini    
 AND ems5_esp.log_concur_titulo.cdn_fornec <= fi_cod_fornec_fim 
 AND ems5_esp.log_concur_titulo.cod_estab >= fi-cod-estabel-ini   
 AND ems5_esp.log_concur_titulo.cod_estab <= fi-cod-estabel-fim   
 AND date(ems5_esp.log_concur_titulo.datahora) >= fi-dt-atualiza-ini   
 AND date(ems5_esp.log_concur_titulo.datahora) <= fi-dt-atualiza-fim"
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-11 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-12
/* Query rebuild information for BROWSE BROWSE-12
     _TblList          = "ems5_esp.log_concur_erros"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > ems5_esp.log_concur_erros.linha
"log_concur_erros.linha" "Seq" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"(if log_concur_erros.tipo = 1 then ""ERRO"" else ""SUCESSO"")" "Tipo" "x(07)" ? ? ? ? ? ? ? no ? no no "10.29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-12 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-6
/* Query rebuild information for BROWSE BROWSE-6
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-rateio NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-6 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Documentos por Usu rio */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Documentos por Usu rio */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-11
&Scoped-define SELF-NAME BROWSE-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-11 w-livre
ON MOUSE-SELECT-CLICK OF BROWSE-11 IN FRAME f-relat
DO:
  APPLY "value-changed" TO browse-11.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-11 w-livre
ON VALUE-CHANGED OF BROWSE-11 IN FRAME f-relat
DO:
   OPEN QUERY browse-12 FOR EACH LOG_concur_erro 
        NO-LOCK WHERE 
          log_concur_erros.id = tt-titulo.id.
                             
    OPEN QUERY browse-6 FOR EACH tt-rateio NO-LOCK
        WHERE tt-rateio.id = tt-titulo.id.

    APPLY "value-changed" TO browse-12.
    
    FIND fornecedor NO-LOCK 
        WHERE fornecedor.cod_empresa = tt-titulo.cod_empresa
        AND fornecedor.cdn_fornec = tt-titulo.cdn_fornec NO-ERROR.

    
    IF AVAIL tt-titulo THEN
    ASSIGN c_est        :screen-value in frame {&frame-name} = tt-titulo.cod_estab
           c-esp        :screen-value in frame {&frame-name} = tt-titulo.cod_esp
           c-forn       :screen-value in frame {&frame-name} = STRING(tt-titulo.cdn_fornec)
           c-par        :screen-value in frame {&frame-name} = "1"
           c-ser        :screen-value in frame {&frame-name} = "1"
           c-tit        :screen-value in frame {&frame-name} = (IF AVAIL tit_ap THEN tit_ap.cod_tit_ap ELSE tt-titulo.titulo)
           c_nom_fornec :screen-value in frame {&frame-name} = fornecedor.nom_pessoa WHEN AVAIL fornecedor.

    ELSE ASSIGN c_est        :screen-value in frame {&frame-name} = "" 
                c-esp        :screen-value in frame {&frame-name} = ""
                c-forn       :screen-value in frame {&frame-name} = ""
                c-par        :screen-value in frame {&frame-name} = ""
                c-ser        :screen-value in frame {&frame-name} = ""
                c-tit        :screen-value in frame {&frame-name} = ""
                c_nom_fornec :screen-value in frame {&frame-name} = ""
                ed_log       :screen-value in frame {&frame-name} = "".
               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-12
&Scoped-define SELF-NAME BROWSE-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-12 w-livre
ON VALUE-CHANGED OF BROWSE-12 IN FRAME f-relat
DO:
    IF AVAIL LOG_concur_erros THEN
        ASSIGN ed_log:SCREEN-VALUE IN FRAME {&FRAME-NAME} = log_concur_erros.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-buscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-buscar w-livre
ON CHOOSE OF bt-buscar IN FRAME f-relat /* Buscar */
DO:
  ASSIGN INPUT FRAME {&FRAME-NAME} fi_cod_fornec_fim 
         INPUT FRAME {&FRAME-NAME} fi_cod_fornec_ini 
         INPUT FRAME {&FRAME-NAME} fi-cod-estabel-fim 
         INPUT FRAME {&FRAME-NAME} fi-cod-estabel-ini 
         INPUT FRAME {&FRAME-NAME} fi-dt-atualiza-fim 
         INPUT FRAME {&FRAME-NAME} fi-dt-atualiza-ini
         INPUT FRAME {&FRAME-NAME} rd-tipo.

  ASSIGN ed_log:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

  RUN pi_busca_dados.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-deletar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-deletar w-livre
ON CHOOSE OF bt-deletar IN FRAME f-relat /* Deleta Log */
DO:
    ASSIGN INPUT FRAME {&FRAME-NAME} fi_cod_fornec_fim 
         INPUT FRAME {&FRAME-NAME} fi_cod_fornec_ini 
         INPUT FRAME {&FRAME-NAME} fi-cod-estabel-fim 
         INPUT FRAME {&FRAME-NAME} fi-cod-estabel-ini 
         INPUT FRAME {&FRAME-NAME} fi-dt-atualiza-fim 
         INPUT FRAME {&FRAME-NAME} fi-dt-atualiza-ini
         INPUT FRAME {&FRAME-NAME} rd-tipo.


     FOR EACH LOG_concur_titulo 
          WHERE  log_concur_titulo.cdn_fornec >= fi_cod_fornec_ini
          AND    log_concur_titulo.cdn_fornec <= fi_cod_fornec_fim 
          AND    log_concur_titulo.cod_estab  >= fi-cod-estabel-ini 
          AND    log_concur_titulo.cod_estab  <= fi-cod-estabel-fim
          AND    DATE(log_concur_titulo.datahora) >= fi-dt-atualiza-ini
          AND    DATE(log_concur_titulo.datahora) <= fi-dt-atualiza-fim
          AND    LOG_concur_titulo.tipo = rd-tipo:


         FOR EACH LOG_concur_rateio OF LOG_concur_titulo:

             DELETE LOG_concur_rateio.
         END.

         FOR EACH LOG_concur_erro OF LOG_concur_titulo:

             DELETE LOG_concur_erro.
         END.

         DELETE LOG_concur_titulo.
                                 
     END.

     APPLY "choose" TO bt-buscar.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-processar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-processar w-livre
ON CHOOSE OF bt-processar IN FRAME f-relat /* Buscar */
DO:
  
   IF NOT CAN-FIND(FIRST b-tt-titulo WHERE
                         b-tt-titulo.log_reprocess)
    THEN DO:

        MESSAGE "NÆo existe nenhum log selecionado para reprocessamento!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
   END.

   RUN pi-reprocessa.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-livre
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-livre
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-livre
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-programa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-programa w-livre
ON MENU-DROP OF MENU mi-programa /* Nome-do-Programa */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-livre
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-livre
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-tipo w-livre
ON VALUE-CHANGED OF rd-tipo IN FRAME f-relat
DO:
  ASSIGN bt-processar:SENSITIVE IN FRAME {&FRAME-NAME} = (IF SELF:SCREEN-VALUE = "1" THEN YES ELSE NO).

  APPLY "choose" TO bt-buscar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-11
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */
ON 'leave':U OF tt-rateio.ccontabil IN BROWSE browse-6
DO:
    GET CURRENT browse-11.
    GET CURRENT browse-6.

    ASSIGN tt-rateio.ccontabil = tt-rateio.ccontabil:SCREEN-VALUE IN BROWSE browse-6.

    RUN pi-valida-conta.
    IF RETURN-VALUE <> "OK" THEN
        MESSAGE RETURN-VALUE
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
END.

ON 'leave':U OF tt-rateio.ccusto IN BROWSE browse-6
DO:
    GET CURRENT browse-11.
    GET CURRENT browse-6.

    ASSIGN tt-rateio.ccusto = tt-rateio.ccusto:SCREEN-VALUE IN BROWSE browse-6.

    RUN pi-valida-conta.
    IF RETURN-VALUE <> "OK" THEN
        MESSAGE RETURN-VALUE
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

ON 'mouse-select-dblclick':U OF tt-rateio.ccontabil
DO:          
    run prgint/utb/utb033ka.p.
    if  v_rec_cta_ctbl_integr <> ?
    then do:

         find cta_ctbl_integr where recid(cta_ctbl_integr) = v_rec_cta_ctbl_integr no-lock no-error.
         if available cta_ctbl_integr 
             then ASSIGN tt-rateio.ccontabil:SCREEN-VALUE IN BROWSE browse-6 = cta_ctbl_integr.cod_cta_ctbl.

    END.     
END.

ON 'mouse-select-dblclick':U OF tt-rateio.ccusto
DO:          
    run prgint/utb/utb066ka.p.
    if  v_rec_ccusto <> ?
    then do:

         find ccusto NO-LOCK 
             where recid(ccusto) = v_rec_ccusto no-error.
         if available ccusto 
             then ASSIGN tt-rateio.ccusto:SCREEN-VALUE IN BROWSE browse-6 = ccusto.cod_ccusto.

    END.     
END.



PROCEDURE WinExec EXTERNAL "kernel32.dll":U:
    DEFINE INPUT  PARAMETER prg_name                          AS CHARACTER.
    DEFINE INPUT  PARAMETER prg_style                         AS SHORT.
END PROCEDURE.



/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-livre  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-relat:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.13 , 101.43 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             c-tit:HANDLE IN FRAME f-relat , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-livre  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-livre  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
  THEN DELETE WIDGET w-livre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-livre  _DEFAULT-ENABLE
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
  DISPLAY d-valor c-esp c-forn c-par c-ser c-tit c_est c_nom_fornec 
          fi_cod_fornec_ini fi_cod_fornec_fim fi-dt-atualiza-ini 
          fi-dt-atualiza-fim fi-cod-estabel-ini fi-cod-estabel-fim rd-tipo 
          ed_log 
      WITH FRAME f-relat IN WINDOW w-livre.
  ENABLE BROWSE-6 BROWSE-12 BROWSE-11 bt-processar bt-buscar bt-refresh 
         fi_cod_fornec_ini fi_cod_fornec_fim fi-dt-atualiza-ini 
         fi-dt-atualiza-fim fi-cod-estabel-ini fi-cod-estabel-fim rd-tipo 
         ed_log bt-deletar rt-button RECT-1 RECT-2 IMAGE-7 IMAGE-8 IMAGE-9 
         IMAGE-10 IMAGE-11 IMAGE-12 RECT-4 RECT-5 
      WITH FRAME f-relat IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-livre 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-livre 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  run pi-before-initialize.

  {include/win-size.i}

  {utp/ut9000.i "AP002-W" "2.06.00.005"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  fi-dt-atualiza-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING (TODAY,"99/99/9999").
  fi-dt-atualiza-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING (TODAY,"99/99/9999").

  APPLY "choose" TO bt-buscar.

  /* Code placed here will execute AFTER standard behavior.    */
  tt-rateio.ccusto:LOAD-MOUSE-POINTER("GLOVE") IN BROWSE browse-6.
  tt-rateio.ccontabil:LOAD-MOUSE-POINTER("GLOVE") IN BROWSE browse-6.

  run pi-after-initialize.

 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-reprocessa w-livre 
PROCEDURE pi-reprocessa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    create tt-param.
    assign tt-param.usuario    = c-seg-usuario
           tt-param.destino    = 3
           tt-param.arquivo    = SESSION:TEMP-DIRECTORY + "\log_reprocessa.txt"
           tt-param.c-destino  = "Terminal"
           tt-param.data-exec  = today
           tt-param.hora-exec  = time
           tt-param.tipo       = 2.
    
    /* Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}

    SESSION:SET-WAIT-STATE("general":U).

    {include/i-rprun.i apb/ap002-rp.p}

    {include/i-rpexc.i}

    SESSION:SET-WAIT-STATE("":U).

    {include/i-rptrm.i}

    APPLY "choose" TO bt-buscar IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida-conta w-livre 
PROCEDURE pi-valida-conta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   

    FIND estabelecimento NO-LOCK 
        WHERE estabelecimento.cod_estab = tt-titulo.cod_estab NO-ERROR.

    FIND cta_ctbl NO-LOCK WHERE
         cta_ctbl.cod_plano_cta_ctbl = "contsoc" AND
         cta_ctbl.cod_cta_ctbl = tt-rateio.ccontabil NO-ERROR.
    IF NOT AVAIL cta_ctbl 
        THEN RETURN "Conta Cont bil nÆo cadastrada!".
        
    FOR EACH criter_distrib_cta_ctbl NO-LOCK 
        WHERE criter_distrib_cta_ctbl.cod_plano_cta_ctbl = cta_ctbl.cod_plano_cta_ctbl
        AND   criter_distrib_cta_ctbl.cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl
        AND   criter_distrib_cta_ctbl.cod_estab          = tt-titulo.cod_estab: 

        IF criter_distrib_cta_ctbl.ind_criter_distrib_ccusto = "Nao utiliza" THEN NEXT.

        IF (criter_distrib_cta_ctbl.dat_inic_valid > TODAY OR 
            criter_distrib_cta_ctbl.dat_fim_valid < TODAY) THEN NEXT.

        IF criter_distrib_cta_ctbl.ind_criter_distrib_ccusto = "Utiliza Todos" THEN DO:

            FIND ccusto NO-LOCK 
                WHERE ccusto.cod_empresa = estabelecimento.cod_empresa 
                AND   ccusto.cod_plano_ccusto = "PLCUNI"
                AND   ccusto.cod_ccusto = tt-rateio.ccusto NO-ERROR.
            IF NOT AVAIL ccusto 
                THEN RETURN "Centro de custo " + tt-rateio.ccusto + " nÆo cadastrado.".
            ELSE DO:

                /* Verifica se nÆo tem restri‡Æo de estabel */
                FIND restric_ccusto NO-LOCK 
                    WHERE restric_ccusto.cod_empresa      = estabelecimento.cod_empresa
                    AND   restric_ccusto.cod_plano_ccusto = ccusto.cod_plano_ccusto 
                    AND   restric_ccusto.cod_ccusto       = ccusto.cod_ccusto 
                    AND   restric_ccusto.cod_estab        = tt-titulo.cod_estab NO-ERROR.
                IF AVAIL restric_ccusto THEN
                    RETURN "Centro de custo " + tt-rateio.ccusto + " possui restri‡Æo para o estabelecimento " + tt-titulo.cod_estab + ".".

            END.
        END.
        FIND mapa_distrib_ccusto NO-LOCK 
            WHERE mapa_distrib_ccusto.cod_estab = tt-titulo.cod_estab 
            AND    mapa_distrib_ccusto.cod_mapa_distrib_ccusto = criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto NO-ERROR.
        IF NOT AVAIL mapa_distrib_ccusto THEN NEXT.

        FIND ITEM_lista_ccusto NO-LOCK 
            WHERE ITEM_lista_ccusto.cod_estab             = tt-titulo.cod_estab 
            AND ITEM_lista_ccusto.cod_mapa_distrib_ccusto = mapa_distrib_ccusto.cod_mapa_distrib_ccusto
            AND ITEM_lista_ccusto.cod_empresa             = estabelecimento.cod_empresa
            AND ITEM_lista_ccusto.cod_plano_ccusto        = mapa_distrib_ccusto.cod_plano_ccusto
            AND ITEM_lista_ccusto.cod_ccusto              = tt-rateio.ccusto NO-ERROR.
        IF NOT AVAIL ITEM_lista_ccusto 
            THEN RETURN "Centro de custo nÆo cadastrado na mapa de distribui‡Æo." + " Est: " + tt-titulo.cod_estab + " Mapa: " + mapa_distrib_ccusto.cod_mapa_distrib_ccusto +
                                                                                                                              " Emp: " + estabelecimento.cod_empresa + " Plano CCusto: " + mapa_distrib_ccusto.cod_plano_ccusto + 
                                                                                                                              " CCusto: " + tt-rateio.ccusto + " Conta: " + tt-rateio.ccontabil.
        ELSE DO:
        
            IF mapa_distrib_ccusto.dat_inic_valid > TODAY OR 
               mapa_distrib_ccusto.dat_fim_valid < TODAY  THEN
                RETURN  "Data de validade do mapa de distribui‡Æo inv lida." + "Mapa: " + mapa_distrib_ccusto.cod_mapa_distrib_ccusto.

        END.
    END.

    RETURN 'ok'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_busca_dados w-livre 
PROCEDURE pi_busca_dados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Reimporta o arquivo de carga e atualiza dados na tela a partir das sele‡äes de 
tela */

  ASSIGN INPUT FRAME {&FRAME-NAME} fi_cod_fornec_fim 
                                   fi_cod_fornec_ini 
                                   fi-cod-estabel-fim 
                                   fi-cod-estabel-ini 
                                   fi-dt-atualiza-fim 
                                   fi-dt-atualiza-ini.

  EMPTY TEMP-TABLE tt-titulo.
  EMPTY TEMP-TABLE tt-rateio.

  FOR EACH log_concur_titulo NO-LOCK 
    WHERE log_concur_titulo.cdn_fornec     >= fi_cod_fornec_ini           
      AND log_concur_titulo.cdn_fornec     <= fi_cod_fornec_fim           
      AND log_concur_titulo.cod_estab      >= fi-cod-estabel-ini          
      AND log_concur_titulo.cod_estab      <= fi-cod-estabel-fim          
      AND DATE(log_concur_titulo.datahora) >= fi-dt-atualiza-ini      
      AND DATE(log_concur_titulo.datahora) <= fi-dt-atualiza-fim      
      AND LOG_concur_titulo.tipo = rd-tipo:

      FIND usuar_financ_estab_apb NO-LOCK 
          WHERE usuar_financ_estab_apb.cod_usuario = v_cod_usuar_corren 
          AND usuar_financ_estab_apb.cod_estab = log_concur_titulo.cod_estab NO-ERROR.
      IF NOT AVAIL log_concur_titulo THEN NEXT.

      CREATE tt-titulo.
      ASSIGN tt-titulo.cdn_fornec  = log_concur_titulo.cdn_fornec 
             tt-titulo.cod_empresa = log_concur_titulo.cod_empresa 
             tt-titulo.cod_esp     = log_concur_titulo.cod_esp 
             tt-titulo.cod_estab   = log_concur_titulo.cod_estab 
             tt-titulo.datahora    = log_concur_titulo.datahora 
             tt-titulo.id          = log_concur_titulo.id 
             tt-titulo.titulo      = log_concur_titulo.titulo
             tt-titulo.arquivo     = log_concur_titulo.arquivo.


      FOR EACH LOG_concur_rateio OF LOG_concur_titulo NO-LOCK:

          CREATE tt-rateio.
          ASSIGN tt-rateio.sequencia       = log_concur_rateio.seq 
                 tt-rateio.id              = log_concur_rateio.id
                 tt-rateio.ccontabil       = log_concur_rateio.conta
                 tt-rateio.ccusto          = log_concur_rateio.ccusto
                 tt-rateio.valor           = log_concur_rateio.valor
                 tt-rateio.lancto          = log_concur_rateio.lancto.
      END.
  END.

  OPEN QUERY browse-11 FOR EACH tt-titulo.

  APPLY "value-changed" TO browse-11.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_upload w-livre 
PROCEDURE pi_upload :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER c-arquivo AS CHARACTER   NO-UNDO.


CREATE tt_file.
ASSIGN tt_file.arquivo = c-arquivo.


   INPUT FROM VALUE(c-arquivo).
    /* O cabe‡alho do arquivo ‚ ignorado pois cont‚m os totais para todos os pa¡ses e nÆo apenas Brasil */
    REPEAT:
    
        IMPORT UNFORMATTED c-dados.

         IF TRIM(ENTRY(1,c-dados,"|")) = "EXTRACT" 
             THEN ASSIGN c-batch = TRIM(ENTRY(5,c-dados,"|")).
    
        /* Importa os dados de detalhes */
        IF TRIM(ENTRY(1,c-dados,"|")) = "DETAIL" THEN DO:
    
            /* Esta valida‡Æo ‚ necess ria pois o arquivo vindo do Concur traz todos os pa¡ses */
            IF TRIM(ENTRY(23,c-dados,"|")) <> "BRAZIL" THEN NEXT.
       
            CREATE tt-dados.
            ASSIGN tt-dados.BATCH_id    = TRIM(ENTRY(2,c-dados,"|"))
                   tt-dados.sequence    = TRIM(ENTRY(4,c-dados,"|"))
                   tt-dados.cod_estab   = TRIM(ENTRY(193,c-dados,"|"))
                   tt-dados.cod_refer   = TRIM(ENTRY(20,c-dados,"|"))
                   tt-dados.dt_trans    = TRIM(ENTRY(64,c-dados,"|"))
                   tt-dados.valor_total = DECIMAL(REPLACE(TRIM(ENTRY(32,c-dados,"|")),".",","))
                   tt-dados.pessoa      = TRIM(ENTRY(5,c-dados,"|"))
                   tt-dados.titulo      = TRIM(ENTRY(20,c-dados,"|"))
                   tt-dados.parcela     = "1" /* Parcela ser  fixa 1 */
                   tt-dados.dt_emissao  = TODAY
                   tt-dados.dt_vencto   = TODAY + 6 - WEEKDAY(TODAY) /* Data da gera‡Æo do arquivo + dias necess rios para ser sexta-feira*/
                   tt-dados.ccontabil   = TRIM(ENTRY(167,c-dados,"|"))
                   tt-dados.ccusto      = TRIM(ENTRY(203,c-dados,"|"))
                   tt-dados.unid_neg    = TRIM(ENTRY(193,c-dados,"|"))
                   tt-dados.vl_rateio   = DECIMAL(REPLACE(TRIM(ENTRY(169,c-dados,"|")),".",","))
                   tt-dados.lancto      = TRIM(ENTRY(168,c-dados,"|"))
                   tt-dados.pais        = TRIM(ENTRY(23,c-dados,"|")).

            /* Identifica a esp‚cie do documento */
            IF tt-dados.lancto = "DR" THEN DO:     /* Qdo lan‡amento a D‚bito ‚ Reembolso, qdo a cr‚dito ‚ Antecipa‡Æo */

                 FIND FIRST es_parametros NO-LOCK 
                        WHERE es_parametros.cod_prog_dtsul = "ap001-w"
                        AND   es_parametros.cod_referencia = "especie_reembolso"
                        AND   es_parametros.dat_valid_ini <= TODAY 
                        AND   es_parametros.dat_valid_fim >= TODAY NO-ERROR.
                 IF NOT AVAIL es_parametros THEN
                     RUN pi_gera_log (INPUT INT(TRIM(tt-dados.pessoa)),
                                      INPUT "ERRO",
                                      INPUT tt-dados.cod_estab,
                                      INPUT tt-dados.sequence,
                                      INPUT "Esp‚cie nÆo encontrada! Verifique o cadastro ESCD0007 com a referˆncia 'especie_reembolso' para o programa AP001-W",
                                      INPUT tt-dados.titulo).
                 ELSE ASSIGN tt-dados.especie = es_parametros.cod_parametro.

            END.
            IF tt-dados.lancto = "CR" THEN DO:     /* Qdo lan‡amento a D‚bito ‚ Reembolso, qdo a cr‚dito ‚ Antecipa‡Æo */

                 FIND FIRST es_parametros NO-LOCK 
                        WHERE es_parametros.cod_prog_dtsul = "ap001-w"
                        AND   es_parametros.cod_referencia = "especie_antecipacao"
                        AND   es_parametros.dat_valid_ini <= TODAY 
                        AND   es_parametros.dat_valid_fim >= TODAY NO-ERROR.
                 IF NOT AVAIL es_parametros THEN
                     RUN pi_gera_log (INPUT INT(TRIM(tt-dados.pessoa)),
                                      INPUT "ERRO",
                                      INPUT tt-dados.cod_estab,
                                      INPUT tt-dados.sequence,
                                      INPUT "Esp‚cie nÆo encontrada! Verifique o cadastro ESCD0007 com a referˆncia 'especie_reembolso' para o programa AP001-W",
                                      INPUT tt-dados.titulo).
                 ELSE ASSIGN tt-dados.especie = es_parametros.cod_parametro.

            END.


        END.
    END.
    
    INPUT CLOSE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-livre  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-rateio"}
  {src/adm/template/snd-list.i "log_concur_erros"}
  {src/adm/template/snd-list.i "tt-titulo"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-livre 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


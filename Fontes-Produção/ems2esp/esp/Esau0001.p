&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i XX9999 9.99.99.999}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> <m¢dulo>}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF VAR c-opcao AS CHAR NO-UNDO.
DEF TEMP-TABLE tt_base_dados LIKE base_dados
  FIELD selec AS LOG FORMAT "s/".
DEF TEMP-TABLE tt_tabela_monitor LIKE tabela_monitor
  FIELD selec AS LOG FORMAT "s/".
DEF TEMP-TABLE tt_usuar_mestre LIKE usuar_mestre
  FIELD selec AS LOG FORMAT "s/".
DEF TEMP-TABLE tt_usu_bco_tab
  FIELD cod_base_dados LIKE tt_tabela_monitor.cod_base_dados
  FIELD cod_tabela     LIKE tt_tabela_monitor.cod_tabela
  FIELD cod_usuario    LIKE tt_usuar_mestre.cod_usuario.
DEF TEMP-TABLE tt_tabela_vrf_monitor LIKE tabela_vrf_monitor.
DEF VAR c-ender-arq AS CHAR NO-UNDO.
DEF VAR c-titulo AS CHAR NO-UNDO.
def var h-acomp         as handle     no-undo.
def var i-linha         as int        no-undo.
DEF STREAM s-excel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br_tabela

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt_tabela_monitor tt_usuar_mestre

/* Definitions for BROWSE br_tabela                                     */
&Scoped-define FIELDS-IN-QUERY-br_tabela tt_tabela_monitor.selec tt_tabela_monitor.cod_base_dados tt_tabela_monitor.cod_tabela tt_tabela_monitor.log_create tt_tabela_monitor.log_delete tt_tabela_monitor.log_write tt_tabela_monitor.cod_evento   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_tabela   
&Scoped-define SELF-NAME br_tabela
&Scoped-define QUERY-STRING-br_tabela FOR EACH tt_tabela_monitor
&Scoped-define OPEN-QUERY-br_tabela OPEN QUERY br_tabela FOR EACH tt_tabela_monitor .
&Scoped-define TABLES-IN-QUERY-br_tabela tt_tabela_monitor
&Scoped-define FIRST-TABLE-IN-QUERY-br_tabela tt_tabela_monitor


/* Definitions for BROWSE br_usuario                                    */
&Scoped-define FIELDS-IN-QUERY-br_usuario tt_usuar_mestre.Selec tt_usuar_mestre.cod_usuario tt_usuar_mestre.nom_usuario   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_usuario   
&Scoped-define SELF-NAME br_usuario
&Scoped-define QUERY-STRING-br_usuario FOR EACH tt_usuar_mestre
&Scoped-define OPEN-QUERY-br_usuario OPEN QUERY br_usuario FOR EACH tt_usuar_mestre .
&Scoped-define TABLES-IN-QUERY-br_usuario tt_usuar_mestre
&Scoped-define FIRST-TABLE-IN-QUERY-br_usuario tt_usuar_mestre


/* Definitions for FRAME f-tabela                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-tabela ~
    ~{&OPEN-QUERY-br_tabela}

/* Definitions for FRAME f-usuario                                      */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-usuario ~
    ~{&OPEN-QUERY-br_usuario}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button im-pg-tab im-pg-par im-pg-usu ~
im-pg-sel RECT-11 bt-executar 

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
DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 15 BY .88.

DEFINE IMAGE im-pg-par
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-sel
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-tab
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE IMAGE im-pg-usu
     FILENAME "image\im-fldup":U
     SIZE 15.72 BY 1.21.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85.14 BY 10.88.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder-top
     EDGE-PIXELS 0    
     SIZE 78.72 BY .13
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41.72 BY 2.17.

DEFINE VARIABLE l-create AS LOGICAL INITIAL yes 
     LABEL "Create" 
     VIEW-AS TOGGLE-BOX
     SIZE 9.72 BY .83 NO-UNDO.

DEFINE VARIABLE l-delete AS LOGICAL INITIAL yes 
     LABEL "Delete" 
     VIEW-AS TOGGLE-BOX
     SIZE 9.14 BY .83 NO-UNDO.

DEFINE VARIABLE l-write AS LOGICAL INITIAL yes 
     LABEL "Write" 
     VIEW-AS TOGGLE-BOX
     SIZE 8.57 BY .83 NO-UNDO.

DEFINE VARIABLE c-atr-fim AS CHARACTER FORMAT "X(32)":U INITIAL "ZZZZZZZZZZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE c-atr-ini AS CHARACTER FORMAT "X(32)":U 
     LABEL "Atributo" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE c-base-fim AS CHARACTER FORMAT "X(15)":U INITIAL "ZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE c-base-ini AS CHARACTER FORMAT "X(15)":U 
     LABEL "Base de Dados" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE c-dat-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE c-dat-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Data" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE c-prog-fim AS CHARACTER FORMAT "X(15)":U INITIAL "ZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE c-prog-ini AS CHARACTER FORMAT "X(15)":U 
     LABEL "Programa" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-13
     FILENAME "image\im-fir":U
     SIZE 2.86 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image\im-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 2.86 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 2.86 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-fir":U
     SIZE 2.86 BY .88.

DEFINE BUTTON bt-desmarca-2 
     LABEL "Desmarca" 
     SIZE 15 BY .88.

DEFINE BUTTON bt-marca-2 
     LABEL "Marca" 
     SIZE 15 BY .88.

DEFINE BUTTON bt-nenhum-2 
     LABEL "Nenhum" 
     SIZE 15 BY .88.

DEFINE BUTTON bt-todos-2 
     LABEL "Todos" 
     SIZE 15 BY .88.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 80 BY 1.

DEFINE BUTTON bt-desmarca 
     LABEL "Desmarca" 
     SIZE 15 BY .88.

DEFINE BUTTON bt-marca 
     LABEL "Marca" 
     SIZE 15 BY .88.

DEFINE BUTTON bt-nenhum 
     LABEL "Nenhum" 
     SIZE 15 BY .88.

DEFINE BUTTON bt-todos 
     LABEL "Todos" 
     SIZE 15 BY .88.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 80 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_tabela FOR 
      tt_tabela_monitor SCROLLING.

DEFINE QUERY br_usuario FOR 
      tt_usuar_mestre SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_tabela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_tabela w-livre _FREEFORM
  QUERY br_tabela DISPLAY
      tt_tabela_monitor.selec      
 tt_tabela_monitor.cod_base_dados
 tt_tabela_monitor.cod_tabela
 tt_tabela_monitor.log_create
 tt_tabela_monitor.log_delete 
 tt_tabela_monitor.log_write
 tt_tabela_monitor.cod_evento
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 80 BY 7.58
         FONT 1 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.

DEFINE BROWSE br_usuario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_usuario w-livre _FREEFORM
  QUERY br_usuario DISPLAY
      tt_usuar_mestre.Selec
 tt_usuar_mestre.cod_usuario
 tt_usuar_mestre.nom_usuario
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 80 BY 7.58
         FONT 1 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-executar AT ROW 1.33 COL 2 WIDGET-ID 26
     "Parametros" VIEW-AS TEXT
          SIZE 10.57 BY .67 AT ROW 3.13 COL 53.72 WIDGET-ID 24
          FGCOLOR 0 
     "Usu rios" VIEW-AS TEXT
          SIZE 8.57 BY .67 AT ROW 3.13 COL 38.29 WIDGET-ID 22
          FGCOLOR 0 
     "Tabelas" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 3.13 COL 22.43 WIDGET-ID 20
          FGCOLOR 0 
     "Sele‡Æo" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 3.13 COL 7.14 WIDGET-ID 18
          FGCOLOR 0 
     rt-button AT ROW 1 COL 1
     rt-folder-top AT ROW 3.79 COL 3.57 WIDGET-ID 12
     im-pg-tab AT ROW 2.75 COL 19.29 WIDGET-ID 2
     im-pg-par AT ROW 2.75 COL 50.72 WIDGET-ID 4
     im-pg-usu AT ROW 2.75 COL 35 WIDGET-ID 8
     im-pg-sel AT ROW 2.75 COL 3.57 WIDGET-ID 10
     RECT-11 AT ROW 3.88 COL 3.57 WIDGET-ID 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE 
         AT COL 1 ROW 1
         SIZE 90 BY 14.04
         BGCOLOR 8 FGCOLOR 8 FONT 1 WIDGET-ID 100.

DEFINE FRAME f-parametro
     l-create AT ROW 3.17 COL 7.86 WIDGET-ID 2
     l-write AT ROW 3.17 COL 21.57 WIDGET-ID 6
     l-delete AT ROW 3.17 COL 35.29 WIDGET-ID 4
     "Eventos:" VIEW-AS TEXT
          SIZE 6.72 BY .67 AT ROW 2.17 COL 7.29 WIDGET-ID 10
     RECT-10 AT ROW 2.5 COL 5.57 WIDGET-ID 8
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE 
         AT COL 4 ROW 4.25
         SIZE 84 BY 10.25
         BGCOLOR 11 FONT 1 WIDGET-ID 500.

DEFINE FRAME f-selecao
     c-dat-ini AT ROW 2 COL 16.57 COLON-ALIGNED WIDGET-ID 4
     c-dat-fim AT ROW 2 COL 46 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     c-base-ini AT ROW 3 COL 16.57 COLON-ALIGNED WIDGET-ID 12
     c-base-fim AT ROW 3 COL 46 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     c-atr-ini AT ROW 4.04 COL 16.57 COLON-ALIGNED WIDGET-ID 28
     c-atr-fim AT ROW 4.04 COL 46 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     c-prog-ini AT ROW 5.08 COL 16.57 COLON-ALIGNED WIDGET-ID 36
     c-prog-fim AT ROW 5.08 COL 46 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     IMAGE-9 AT ROW 2 COL 36.43 WIDGET-ID 8
     IMAGE-10 AT ROW 1.88 COL 44.29 WIDGET-ID 6
     IMAGE-5 AT ROW 3 COL 36.43 WIDGET-ID 14
     IMAGE-6 AT ROW 3 COL 44.29 WIDGET-ID 16
     IMAGE-7 AT ROW 4.04 COL 36.43 WIDGET-ID 30
     IMAGE-8 AT ROW 4.04 COL 44.29 WIDGET-ID 32
     IMAGE-13 AT ROW 5.08 COL 36.43 WIDGET-ID 38
     IMAGE-14 AT ROW 5.08 COL 44.29 WIDGET-ID 40
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4 ROW 4.25
         SIZE 84 BY 10.25
         BGCOLOR 11 FONT 1 WIDGET-ID 200.

DEFINE FRAME f-tabela
     br_tabela AT ROW 1.75 COL 3 WIDGET-ID 700
     bt-marca-2 AT ROW 9.67 COL 4.29 WIDGET-ID 2
     bt-desmarca-2 AT ROW 9.67 COL 19.43 WIDGET-ID 4
     bt-todos-2 AT ROW 9.67 COL 34.57 WIDGET-ID 6
     bt-nenhum-2 AT ROW 9.67 COL 49.72 WIDGET-ID 8
     RECT-13 AT ROW 9.63 COL 3 WIDGET-ID 10
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE 
         AT COL 4 ROW 4.25
         SIZE 84 BY 10.25
         BGCOLOR 11 FONT 1 WIDGET-ID 300.

DEFINE FRAME f-usuario
     br_usuario AT ROW 1.75 COL 3 WIDGET-ID 600
     bt-marca AT ROW 9.67 COL 4.29 WIDGET-ID 2
     bt-desmarca AT ROW 9.67 COL 19.43 WIDGET-ID 4
     bt-todos AT ROW 9.67 COL 34.57 WIDGET-ID 6
     bt-nenhum AT ROW 9.67 COL 49.72 WIDGET-ID 8
     RECT-12 AT ROW 9.63 COL 3 WIDGET-ID 10
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE 
         AT COL 4 ROW 4.25
         SIZE 84 BY 10.25
         BGCOLOR 11 FONT 1 WIDGET-ID 400.


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
         TITLE              = "Template Livre <Insira complemento>"
         HEIGHT             = 14.13
         WIDTH              = 90
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 90
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
/* REPARENT FRAME */
ASSIGN FRAME f-parametro:FRAME = FRAME f-cad:HANDLE
       FRAME f-selecao:FRAME = FRAME f-cad:HANDLE
       FRAME f-tabela:FRAME = FRAME f-cad:HANDLE
       FRAME f-usuario:FRAME = FRAME f-cad:HANDLE.

/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* SETTINGS FOR RECTANGLE rt-folder-top IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-parametro
                                                                        */
/* SETTINGS FOR FRAME f-selecao
                                                                        */
/* SETTINGS FOR FRAME f-tabela
                                                                        */
/* BROWSE-TAB br_tabela RECT-13 f-tabela */
/* SETTINGS FOR FRAME f-usuario
                                                                        */
/* BROWSE-TAB br_usuario RECT-12 f-usuario */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_tabela
/* Query rebuild information for BROWSE br_tabela
     _START_FREEFORM
OPEN QUERY br_tabela FOR EACH tt_tabela_monitor .
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br_tabela */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_usuario
/* Query rebuild information for BROWSE br_usuario
     _START_FREEFORM
OPEN QUERY br_usuario FOR EACH tt_usuar_mestre .
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br_usuario */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Template Livre <Insira complemento> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Template Livre <Insira complemento> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-usuario
&Scoped-define SELF-NAME bt-desmarca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desmarca w-livre
ON CHOOSE OF bt-desmarca IN FRAME f-usuario /* Desmarca */
DO:
  IF AVAIL tt_usuar_mestre THEN
    ASSIGN tt_usuar_mestre.selec = NO.
  OPEN QUERY br_usuario FOR EACH tt_usuar_mestre.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-tabela
&Scoped-define SELF-NAME bt-desmarca-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desmarca-2 w-livre
ON CHOOSE OF bt-desmarca-2 IN FRAME f-tabela /* Desmarca */
DO:
  IF AVAIL tt_tabela_monitor THEN
    ASSIGN tt_tabela_monitor.selec = NO.
  OPEN QUERY br_tabela FOR EACH tt_tabela_monitor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-cad
&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar w-livre
ON CHOOSE OF bt-executar IN FRAME f-cad /* Executar */
DO:
  RUN pi-executar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-usuario
&Scoped-define SELF-NAME bt-marca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-marca w-livre
ON CHOOSE OF bt-marca IN FRAME f-usuario /* Marca */
DO:
  IF AVAIL tt_usuar_mestre THEN
    ASSIGN tt_usuar_mestre.selec = YES.
  OPEN QUERY br_usuario FOR EACH tt_usuar_mestre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-tabela
&Scoped-define SELF-NAME bt-marca-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-marca-2 w-livre
ON CHOOSE OF bt-marca-2 IN FRAME f-tabela /* Marca */
DO:
  IF AVAIL tt_tabela_monitor THEN
    ASSIGN tt_tabela_monitor.selec = YES.
  OPEN QUERY br_tabela FOR EACH tt_tabela_monitor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-usuario
&Scoped-define SELF-NAME bt-nenhum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nenhum w-livre
ON CHOOSE OF bt-nenhum IN FRAME f-usuario /* Nenhum */
DO:
  FOR EACH tt_usuar_mestre:
    ASSIGN tt_usuar_mestre.selec = NO.
  END. /* FOR EACH usuar_mestre */
  OPEN QUERY br_usuario FOR EACH tt_usuar_mestre.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-tabela
&Scoped-define SELF-NAME bt-nenhum-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nenhum-2 w-livre
ON CHOOSE OF bt-nenhum-2 IN FRAME f-tabela /* Nenhum */
DO:
  FOR EACH tt_tabela_monitor:
    ASSIGN tt_tabela_monitor.selec = NO.
  END. /* FOR EACH tt_tabela_monitor */
  OPEN QUERY br_tabela FOR EACH tt_tabela_monitor.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-usuario
&Scoped-define SELF-NAME bt-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos w-livre
ON CHOOSE OF bt-todos IN FRAME f-usuario /* Todos */
DO:
  FOR EACH tt_usuar_mestre:
    ASSIGN tt_usuar_mestre.selec = YES.
  END. /* FOR EACH usuar_mestre */
  OPEN QUERY br_usuario FOR EACH tt_usuar_mestre.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-tabela
&Scoped-define SELF-NAME bt-todos-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos-2 w-livre
ON CHOOSE OF bt-todos-2 IN FRAME f-tabela /* Todos */
DO:
  FOR EACH tt_tabela_monitor:
    ASSIGN tt_tabela_monitor.selec = YES.
  END. /* FOR EACH tt_tabela_monitor */
  OPEN QUERY br_tabela FOR EACH tt_tabela_monitor.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-cad
&Scoped-define SELF-NAME im-pg-par
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-par w-livre
ON MOUSE-SELECT-CLICK OF im-pg-par IN FRAME f-cad
DO:
   ASSIGN 
      c-opcao = "Tab".
   im-pg-sel:LOAD-IMAGE("image/im-fldup").
   im-pg-tab:LOAD-IMAGE("image/im-fldup").
   im-pg-usu:LOAD-IMAGE("image/im-fldup").
   im-pg-par:LOAD-IMAGE("image/im-flddn").
   ASSIGN 
     im-pg-sel:ROW IN FRAME f-cad = 2.75
     im-pg-tab:ROW IN FRAME f-cad = 2.75
     im-pg-usu:ROW IN FRAME f-cad = 2.75
     im-pg-par:ROW IN FRAME f-cad = 2.89.
    HIDE FRAME f-selecao NO-PAUSE.
    HIDE FRAME f-tabela  NO-PAUSE.
    HIDE FRAME f-usuario   NO-PAUSE.
    VIEW FRAME f-parametro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-sel w-livre
ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-cad
DO:
   ASSIGN 
      c-opcao = "Sel".
   im-pg-sel:LOAD-IMAGE("image/im-flddn").
   im-pg-tab:LOAD-IMAGE("image/im-fldup").
   im-pg-usu:LOAD-IMAGE("image/im-fldup").
   im-pg-par:LOAD-IMAGE("image/im-fldup").
   ASSIGN 
     im-pg-sel:ROW IN FRAME f-cad = 2.89
     im-pg-tab:ROW IN FRAME f-cad = 2.75
     im-pg-usu:ROW IN FRAME f-cad = 2.75
     im-pg-par:ROW IN FRAME f-cad = 2.75.
   VIEW FRAME f-selecao.
   HIDE FRAME f-tabela  NO-PAUSE.
   HIDE FRAME f-usuario  NO-PAUSE.
   HIDE FRAME f-parametro  NO-PAUSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-tab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-tab w-livre
ON MOUSE-SELECT-CLICK OF im-pg-tab IN FRAME f-cad
DO:
   ASSIGN 
      c-opcao = "Tab".
   im-pg-sel:LOAD-IMAGE("image/im-fldup").
   im-pg-tab:LOAD-IMAGE("image/im-flddn").
   im-pg-usu:LOAD-IMAGE("image/im-fldup").
   im-pg-par:LOAD-IMAGE("image/im-fldup").
   ASSIGN 
     im-pg-sel:ROW IN FRAME f-cad = 2.75
     im-pg-tab:ROW IN FRAME f-cad = 2.89
     im-pg-usu:ROW IN FRAME f-cad = 2.75
     im-pg-par:ROW IN FRAME f-cad = 2.75.
   HIDE FRAME f-selecao NO-PAUSE.
   VIEW FRAME f-tabela.
   HIDE FRAME f-usuario  NO-PAUSE.
   HIDE FRAME f-parametro  NO-PAUSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-usu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-usu w-livre
ON MOUSE-SELECT-CLICK OF im-pg-usu IN FRAME f-cad
DO:
    ASSIGN 
       c-opcao = "Usu".
    im-pg-sel:LOAD-IMAGE("image/im-fldup").
    im-pg-tab:LOAD-IMAGE("image/im-fldup").
    im-pg-usu:LOAD-IMAGE("image/im-flddn").
    im-pg-par:LOAD-IMAGE("image/im-fldup").
    ASSIGN 
      im-pg-sel:ROW IN FRAME f-cad = 2.75
      im-pg-tab:ROW IN FRAME f-cad = 2.75
      im-pg-usu:ROW IN FRAME f-cad = 2.89
      im-pg-par:ROW IN FRAME f-cad = 2.75.
    HIDE FRAME f-selecao NO-PAUSE.
    HIDE FRAME f-tabela  NO-PAUSE.
    VIEW FRAME f-usuario.
    HIDE FRAME f-parametro  NO-PAUSE.
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


&Scoped-define BROWSE-NAME br_tabela
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

RUN pi-restaura-tt.
/*
FOR EACH usuar_mestre
  NO-LOCK:
    CREATE tt_usuar_mestre.
    BUFFER-COPY usuar_mestre TO tt_usuar_mestre.
    ASSIGN tt_usuar_mestre.selec = YES.
  END. /* FOR EACH usuar_mestre */
OPEN QUERY br_usuario FOR EACH tt_usuar_mestre.

FOR EACH tabela_monitor
  NO-LOCK:
    CREATE tt_tabela_monitor.
    BUFFER-COPY tabela_monitor TO tt_tabela_monitor.
    ASSIGN tt_tabela_monitor.selec = YES.
  END. /* FOR EACH usuar_mestre */
OPEN QUERY br_tabela FOR EACH tt_tabela_monitor .
*/
APPLY "MOUSE-SELECT-CLICK" TO im-pg-sel IN FRAME f-cad.

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
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.17 , 74.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-executar:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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
  ENABLE rt-button im-pg-tab im-pg-par im-pg-usu im-pg-sel RECT-11 bt-executar 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  DISPLAY l-create l-write l-delete 
      WITH FRAME f-parametro IN WINDOW w-livre.
  ENABLE RECT-10 l-create l-write l-delete 
      WITH FRAME f-parametro IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-parametro}
  DISPLAY c-dat-ini c-dat-fim c-base-ini c-base-fim c-atr-ini c-atr-fim 
          c-prog-ini c-prog-fim 
      WITH FRAME f-selecao IN WINDOW w-livre.
  ENABLE IMAGE-9 IMAGE-10 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 IMAGE-13 IMAGE-14 
         c-dat-ini c-dat-fim c-base-ini c-base-fim c-atr-ini c-atr-fim 
         c-prog-ini c-prog-fim 
      WITH FRAME f-selecao IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-selecao}
  ENABLE RECT-13 br_tabela bt-marca-2 bt-desmarca-2 bt-todos-2 bt-nenhum-2 
      WITH FRAME f-tabela IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-tabela}
  ENABLE RECT-12 br_usuario bt-marca bt-desmarca bt-todos bt-nenhum 
      WITH FRAME f-usuario IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-usuario}
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

  {utp/ut9000.i "XX9999" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar w-livre 
PROCEDURE pi-executar :
/*pi-executar*/
ASSIGN c-ender-arq = SESSION:TEMP-DIRECTORY + STRING(TIME) + STRING(TODAY,"99999999") + ".csv".
FOR EACH tt_usu_bco_tab: DELETE tt_usu_bco_tab. END.
run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp(input "Aguarde ...Verificando.").
assign i-linha = 0.
assign i-linha = i-linha + 1.
run pi-acompanhar in h-acomp(input "Linha : " + string(i-linha)).
/*-----------------------------------------------------------*/
FOR EACH tt_usuar_mestre
  NO-LOCK
  WHERE tt_usuar_mestre.selec = YES,
/*   EACH tt_base_dados               */
/*   NO-LOCK                          */
/*   WHERE tt_base_dados.selec = YES, */
  EACH tt_tabela_monitor
  NO-LOCK
  WHERE  /* tt_tabela_monitor.cod_base_dados = tt_base_dados.cod_base_dados
  AND    */ tt_tabela_monitor.selec          = YES
  AND    tt_tabela_monitor.cod_base_dados    >= c-base-ini
  AND    tt_tabela_monitor.cod_base_dados    <= c-base-fim:
    /*------------------------------------------------------------*/
    FIND tt_usu_bco_tab
      NO-LOCK
      WHERE tt_usu_bco_tab.cod_base_dados = tt_tabela_monitor.cod_base_dados
      AND   tt_usu_bco_tab.cod_tabela     = tt_tabela_monitor.cod_tabela
      AND   tt_usu_bco_tab.cod_usuario    = tt_usuar_mestre.cod_usuario
      NO-ERROR.
    IF NOT AVAIL tt_usu_bco_tab THEN
      DO:
        CREATE tt_usu_bco_tab.
        ASSIGN
          tt_usu_bco_tab.cod_base_dados = tt_tabela_monitor.cod_base_dados
          tt_usu_bco_tab.cod_tabela     = tt_tabela_monitor.cod_tabela
          tt_usu_bco_tab.cod_usuario    = tt_usuar_mestre.cod_usuario.
      END. /* IF NOT AVAIL tt_usu_bco_tab THEN */
    /*------------------------------------------------------------*/
  END. /* FOR EACH tt_usuar_mestre */
assign i-linha = i-linha + 1.
run pi-acompanhar in h-acomp(input "Linha : " + string(i-linha)).
assign i-linha = i-linha + 1.
run pi-acompanhar in h-acomp(input "Linha : " + string(i-linha)).
OUTPUT STREAM s-excel TO VALUE(c-ender-arq) NO-CONVERT. 
ASSIGN
  c-titulo = "Base de dados;Tabela;Atributo;Programa;Programa;Data Atualiza‡Æo;Hora Atualiza‡Æo;Evento;".
PUT STREAM s-excel 
  c-titulo FORMAT "X(600)"
  SKIP.
assign i-linha = i-linha + 1.
run pi-acompanhar in h-acomp(input "Linha : " + string(i-linha)).
FOR FIRST tt_usu_bco_tab
  NO-LOCK:
    assign i-linha = i-linha + 1.                                          
    run pi-acompanhar in h-acomp(input "*Linha : " + string(i-linha)).  
    FOR EACH tabela_vrf_monitor 
      NO-LOCK
      USE-INDEX tbrstdmn-04
      WHERE tabela_vrf_monitor.dat_atualiz    >= INPUT FRAME f-selecao c-dat-ini     
      AND   tabela_vrf_monitor.dat_atualiz    <= INPUT FRAME f-selecao c-dat-fim
      AND   tabela_vrf_monitor.cod_base_dados  = tt_usu_bco_tab.cod_base_dados
      AND   tabela_vrf_monitor.cod_tabela      = tt_usu_bco_tab.cod_tabela 
      AND   tabela_vrf_monitor.cod_usuario     = tt_usu_bco_tab.cod_usuario:
      
        assign i-linha = i-linha + 1.
        run pi-acompanhar in h-acomp(input "*Linha* : " + string(i-linha)).
        
        FOR EACH  atrib_vrf_monitor OF tabela_vrf_monitor
          WHERE atrib_vrf_monitor.cod_atributo >= INPUT FRAME f-selecao c-atr-ini
          AND   atrib_vrf_monitor.cod_atributo <= INPUT FRAME f-selecao c-atr-fim:
            /*-------------------------------------------------------------*/
            assign i-linha = i-linha + 1.
            run pi-acompanhar in h-acomp(input "Linha : " + string(i-linha)).
            /*-------------------------------------------------------------*/
            IF (tabela_vrf_monitor.cod_evento = 'C' AND INPUT FRAME f-parametro l-create = NO)
            OR (tabela_vrf_monitor.cod_evento = 'W' AND INPUT FRAME f-parametro l-write  = NO)
            OR (tabela_vrf_monitor.cod_evento = 'D' AND INPUT FRAME f-parametro l-delete = NO)
            THEN NEXT.
            /*-------------------------------------------------------------*/
            IF NOT (tabela_vrf_monitor.des_prog_atualiz[2]    >= INPUT FRAME f-selecao c-prog-ini
              AND tabela_vrf_monitor.des_prog_atualiz[2]      <= INPUT FRAME f-selecao c-prog-fim) THEN
              NEXT.
            /*-------------------------------------------------------------*/
            RUN pi-imprime.
            /*-------------------------------------------------------------*/
          END. /* FOR EACH  atrib_vrf_monitor OF tabela_vrf_monitor */
          
      END. /* FOR EACH tabela_vrf_monitor */
  END. /* FOR EACH tt_tabela_vrf_monitor */
OUTPUT STREAM s-excel CLOSE.
run pi-finalizar in h-acomp.
DOS SILENT START excel.exe VALUE(c-ender-arq). 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime w-livre 
PROCEDURE pi-imprime :
/*pi-imprime*/
PUT STREAM s-excel 
  tabela_vrf_monitor.cod_base_dados        ";"  
  tabela_vrf_monitor.cod_tabela            ";"
  atrib_vrf_monitor.cod_atributo           ";"
  tabela_vrf_monitor.des_prog_atualiz[2]   ";"
  tabela_vrf_monitor.des_prog_atualiz[1]   ";"
  tabela_vrf_monitor.dat_atualiz           ";"
  tabela_vrf_monitor.hra_atualiz           ";"
  tabela_vrf_monitor.cod_evento            ";"
  SKIP.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-restaura-tt w-livre 
PROCEDURE pi-restaura-tt :
/*pi-restaura-tt*/
  FOR EACH usuar_mestre
    NO-LOCK:
      CREATE tt_usuar_mestre.
      BUFFER-COPY usuar_mestre TO tt_usuar_mestre.
      ASSIGN tt_usuar_mestre.selec = YES.
    END. /* FOR EACH usuar_mestre */
  OPEN QUERY br_usuario FOR EACH tt_usuar_mestre.
  
  FOR EACH tabela_monitor
    NO-LOCK:
      CREATE tt_tabela_monitor.
      BUFFER-COPY tabela_monitor TO tt_tabela_monitor.
      ASSIGN tt_tabela_monitor.selec = YES.
    END. /* FOR EACH usuar_mestre */
  OPEN QUERY br_tabela FOR EACH tt_tabela_monitor .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina w-livre 
PROCEDURE pi-troca-pagina :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
  {src/adm/template/snd-list.i "tt_usuar_mestre"}
  {src/adm/template/snd-list.i "tt_tabela_monitor"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-livre 
PROCEDURE state-changed :
/*:T -----------------------------------------------------------
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


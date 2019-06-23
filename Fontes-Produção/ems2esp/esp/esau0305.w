&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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
{include/i-prgvrs.i ESAU0305 0.12.00.000}  /*** 010005 ***/


&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i esau0305 MAU}
&ENDIF

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
        field c-user-ini       like tabela_vrf_monitor.cod_usuario
        field c-user-fim       like tabela_vrf_monitor.cod_usuario
        field c-base-ini       like tabela_vrf_monitor.cod_base_dados
        field c-base-fim       like tabela_vrf_monitor.cod_base_dados
        field c-tab-ini        like tabela_vrf_monitor.cod_tabela
        field c-tab-fim        like tabela_vrf_monitor.cod_tabela
        field c-atr-ini        like atrib_vrf_monitor.cod_atributo
        field c-atr-fim        like atrib_vrf_monitor.cod_atributo
        field c-dat-ini        like tabela_vrf_monitor.dat_atualiz
        field c-dat-fim        like tabela_vrf_monitor.dat_atualiz
        field l-val-alt        as log
        field c-prog-ini       as char
        field c-prog-fim       as char
        field l-seq-prog       as log
        field l-create         as log
        field l-write          as log
        field l-delete         as log
        field gera-excel       as LOGICAL INITIAL NO
        FIELD arq-csv          AS CHARACTER FORMAT "x(256)"
        Field l-imp-param      As Log
        field envia-email      as log
        FIELD rs-tipo-execucao AS INTEGER.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita NO-UNDO
   field raw-digita      as raw.

/* Local Variable Definitions ---                                       */

def var l-ok               as LOGICAL                no-undo.
def var c-arq-digita       as char                   no-undo.
def var c-terminal         as char                   no-undo.
Define Variable c-arq-csv-old           As CHARACTER NO-UNDO.
Define Variable c-arq-csv-old-batch     As CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-cla

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-classif 
&Scoped-Define DISPLAYED-OBJECTS rs-classif 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE rs-classif AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Por Usu rio", 1
     SIZE 22.29 BY 1.08 NO-UNDO.

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

DEFINE VARIABLE tg-imp-param AS LOGICAL INITIAL no 
     LABEL "Imprime pagina de parƒmetros?" 
     VIEW-AS TOGGLE-BOX
     SIZE 44 BY .83 NO-UNDO.

DEFINE VARIABLE c-arq-csv AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arquivo CSV" 
     VIEW-AS FILL-IN 
     SIZE 48 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41.72 BY 2.17.

DEFINE VARIABLE l-arq-excel AS LOGICAL INITIAL no 
     LABEL "Gera Arquivo CSV" 
     VIEW-AS TOGGLE-BOX
     SIZE 37.86 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-create AS LOGICAL INITIAL yes 
     LABEL "Create" 
     VIEW-AS TOGGLE-BOX
     SIZE 9.72 BY .83 NO-UNDO.

DEFINE VARIABLE l-delete AS LOGICAL INITIAL yes 
     LABEL "Delete" 
     VIEW-AS TOGGLE-BOX
     SIZE 9.14 BY .83 NO-UNDO.

DEFINE VARIABLE l-envia-email AS LOGICAL INITIAL no 
     LABEL "Envia Email" 
     VIEW-AS TOGGLE-BOX
     SIZE 37.86 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-write AS LOGICAL INITIAL yes 
     LABEL "Write" 
     VIEW-AS TOGGLE-BOX
     SIZE 8.57 BY .83 NO-UNDO.

DEFINE VARIABLE c-atr-fim AS CHARACTER FORMAT "X(32)":U INITIAL "ZZZZZZZZZZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .88 NO-UNDO.

DEFINE VARIABLE c-atr-ini AS CHARACTER FORMAT "X(32)":U 
     LABEL "Atributo" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .88 NO-UNDO.

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

DEFINE VARIABLE c-prog-fim AS CHARACTER FORMAT "X(50)":U INITIAL "ZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE c-prog-ini AS CHARACTER FORMAT "X(50)":U 
     LABEL "Programa" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE c-tab-fim AS CHARACTER FORMAT "X(32)":U INITIAL "ZZZZZZZZZZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .88 NO-UNDO.

DEFINE VARIABLE c-tab-ini AS CHARACTER FORMAT "X(32)":U 
     LABEL "Tabela" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .88 NO-UNDO.

DEFINE VARIABLE c-user-fim AS CHARACTER FORMAT "X(50)":U INITIAL "ZZZZZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88 NO-UNDO.

DEFINE VARIABLE c-user-ini AS CHARACTER FORMAT "X(50)":U 
     LABEL "Usu rio" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-fir":U
     SIZE 2.86 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image\im-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-13
     FILENAME "image\im-fir":U
     SIZE 2.86 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image\im-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 2.86 BY .88.

DEFINE IMAGE IMAGE-4
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
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     RECT-1 AT ROW 14.29 COL 2
     rt-folder AT ROW 2.5 COL 2
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-6 AT ROW 13.75 COL 2.14
     im-pg-cla AT ROW 1.5 COL 17.86
     im-pg-imp AT ROW 1.5 COL 49.29
     im-pg-par AT ROW 1.5 COL 33.57
     im-pg-sel AT ROW 1.5 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.75 COL 19.14 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     bt-config-impr AT ROW 3.96 COL 59.14 HELP
          "Configura‡Æo da impressora"
     bt-arquivo AT ROW 3.96 COL 59.14 HELP
          "Escolha do nome do arquivo"
     c-arquivo AT ROW 4 COL 19.14 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 6.13 COL 18.86 HELP
          "Modo de Execu‡Æo" NO-LABEL
     tg-imp-param AT ROW 7.88 COL 18.86
     text-destino AT ROW 2 COL 19.72 NO-LABEL
     text-modo AT ROW 5.38 COL 17.14 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 2.29 COL 18
     RECT-9 AT ROW 5.67 COL 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10.

DEFINE FRAME f-pg-sel
     c-base-ini AT ROW 2.33 COL 14.43 COLON-ALIGNED
     c-base-fim AT ROW 2.33 COL 51.14 COLON-ALIGNED NO-LABEL
     c-tab-ini AT ROW 3.33 COL 14.43 COLON-ALIGNED
     c-tab-fim AT ROW 3.33 COL 51.14 COLON-ALIGNED NO-LABEL
     c-atr-ini AT ROW 4.33 COL 14.43 COLON-ALIGNED
     c-atr-fim AT ROW 4.33 COL 51.14 COLON-ALIGNED NO-LABEL
     c-user-ini AT ROW 5.33 COL 14.43 COLON-ALIGNED
     c-user-fim AT ROW 5.33 COL 51.14 COLON-ALIGNED NO-LABEL
     c-dat-ini AT ROW 6.33 COL 14.43 COLON-ALIGNED
     c-dat-fim AT ROW 6.33 COL 51.14 COLON-ALIGNED NO-LABEL
     c-prog-ini AT ROW 7.33 COL 14.43 COLON-ALIGNED
     c-prog-fim AT ROW 7.33 COL 51.14 COLON-ALIGNED NO-LABEL
     IMAGE-10 AT ROW 6.33 COL 49.43
     IMAGE-11 AT ROW 5.25 COL 41.57
     IMAGE-12 AT ROW 5.33 COL 49.43
     IMAGE-13 AT ROW 7.25 COL 41.57
     IMAGE-14 AT ROW 7.33 COL 49.43
     IMAGE-3 AT ROW 3.25 COL 41.57
     IMAGE-4 AT ROW 3.33 COL 49.43
     IMAGE-5 AT ROW 2.25 COL 41.57
     IMAGE-6 AT ROW 2.33 COL 49.43
     IMAGE-7 AT ROW 4.25 COL 41.57
     IMAGE-8 AT ROW 4.33 COL 49.43
     IMAGE-9 AT ROW 6.25 COL 41.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62.

DEFINE FRAME f-pg-par
     l-create AT ROW 3.17 COL 7.86
     l-write AT ROW 3.17 COL 21.57
     l-delete AT ROW 3.17 COL 35.29
     l-arq-excel AT ROW 5.5 COL 6
     c-arq-csv AT ROW 6.75 COL 14 COLON-ALIGNED
     l-envia-email AT ROW 8.5 COL 6 WIDGET-ID 2
     "Eventos" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 2.17 COL 7.29
     RECT-10 AT ROW 2.5 COL 5.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 77.29 BY 10.75.

DEFINE FRAME f-pg-cla
     rs-classif AT ROW 2.17 COL 6.14 HELP
          "Classifica‡Æo para emissÆo do relat¢rio" NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.86 BY 10.31.


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
         TITLE              = ""
         HEIGHT             = 15
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
/* SETTINGS FOR FRAME f-pg-cla
   FRAME-NAME                                                           */
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
/* SETTINGS FOR FILL-IN c-arq-csv IN FRAME f-pg-par
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
ON END-ERROR OF w-relat
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat
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
    /*** LEON
    run pi-get-directory('', output v_nom_path, output v_log_cancdo).
    if  v_log_cancdo = no then
        assign c-arquivo:screen-value in frame {&FRAME-NAME} = v_nom_path.
    ***/

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


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME c-arq-csv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-arq-csv w-relat
ON F5 OF c-arq-csv IN FRAME f-pg-par /* Arquivo CSV */
DO:
    DEF VAR v_log_cancdo AS LOGICAL INITIAL NO.

    RUN pi-get-directory('', OUTPUT c-arq-csv, OUTPUT v_log_cancdo).

    IF  v_log_cancdo = NO THEN ASSIGN c-arq-csv:SCREEN-VALUE = Replace(Trim(c-arq-csv) + "~\au0305.csv","~\","/").
    Assign  c-arq-csv-old = c-arq-csv:SCREEN-VALUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-arq-csv w-relat
ON MOUSE-SELECT-DBLCLICK OF c-arq-csv IN FRAME f-pg-par /* Arquivo CSV */
DO:
    apply 'F5':U to C-ARQ-CSV.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME im-pg-cla
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-cla w-relat
ON MOUSE-SELECT-CLICK OF im-pg-cla IN FRAME f-relat
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
&Scoped-define SELF-NAME l-arq-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-arq-excel w-relat
ON VALUE-CHANGED OF l-arq-excel IN FRAME f-pg-par /* Gera Arquivo CSV */
DO:
    ASSIGN  c-arq-csv:SENSITIVE     IN FRAME f-pg-par = l-arq-excel:CHECKED
            c-arq-csv:SCREEN-VALUE  IN FRAME f-pg-par = Replace((   IF  l-arq-excel:CHECKED
                                                            THEN SESSION:TEMP-DIRECTORY + "au0305.csv"
                                                            ELSE ""),"~\","/")
            c-arq-csv-old = c-arq-csv:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME l-envia-email
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-envia-email w-relat
ON VALUE-CHANGED OF l-envia-email IN FRAME f-pg-par /* Envia Email */
DO:
    ASSIGN  c-arq-csv:SENSITIVE     IN FRAME f-pg-par = l-arq-excel:CHECKED
            c-arq-csv:SCREEN-VALUE  IN FRAME f-pg-par = Replace((   IF  l-arq-excel:CHECKED
                                                            THEN SESSION:TEMP-DIRECTORY + "au0305.csv"
                                                            ELSE ""),"~\","/")
            c-arq-csv-old = c-arq-csv:SCREEN-VALUE.
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
    /****************************************************************
    **  {include/i-rprse.i}
    ** I-RPRSE.I - Gatilho "Value-Changed" de rs-execucao 
    **
    *****************************************************************/

    ASSIGN  rs-execucao.

    IF rs-execucao = 2 THEN DO:
        IF rs-destino:SCREEN-VALUE IN FRAME f-pg-imp <> "1":U
        Then Do:
            ASSIGN rs-destino:SCREEN-VALUE IN FRAME f-pg-imp = "2":U
                   c-arquivo       = IF c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = "":U
                                     THEN IF c-arquivo = "" 
                                          THEN c-arq-old
                                          ELSE c-arquivo
                                     ELSE c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp
                   c-arq-old       = c-arquivo
                   c-arq-old-batch = SUBSTRING(c-arquivo, R-INDEX(c-arquivo, "/":U) + 1)
                   c-arquivo:screen-value = c-arq-old-batch.

            ASSIGN  c-arq-csv       =   If c-arq-csv:Screen-value In Frame f-pg-par = "":U
                                        Then If c-arq-csv = ""
                                             Then c-arq-csv-old
                                             Else c-arq-csv
                                        Else c-arq-csv:Screen-value In Frame f-pg-par
                    c-arq-csv-old-batch = SUBSTRING(c-arq-csv, R-INDEX(c-arq-csv, "/":U) + 1)
                    c-arq-csv:Screen-value In Frame f-pg-par = c-arq-csv-old-batch.

        End.

        APPLY "VALUE-CHANGED":U TO rs-destino IN FRAME f-pg-imp.

        IF c-arq-old-batch NE "" THEN /* Por Thiago Garcia ref. FO 901.132 */
            ASSIGN c-arquivo.
        IF c-arq-csv-old-batch NE "" THEN /* Por Thiago Garcia ref. FO 901.132 */
            ASSIGN c-arq-csv.

        rs-destino:DISABLE(c-terminal) IN FRAME f-pg-imp.
    END.
    ELSE DO:
        IF rs-destino:SCREEN-VALUE IN FRAME f-pg-imp <> "1":U
        Then Do:
            ASSIGN  c-arquivo       = IF c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = "":U
                                     THEN c-arquivo
                                     ELSE c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp
                    c-arq-old-batch = c-arquivo.

            ASSIGN c-arq-csv:screen-value In Frame f-pg-par = c-arq-csv-old.
        End.

        APPLY "VALUE-CHANGED":U TO rs-destino IN FRAME f-pg-imp.

        rs-destino:ENABLE(c-terminal) IN FRAME f-pg-imp.

        ASSIGN c-arquivo.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-cla
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESAU0305" "0.12.00.000"}

/* inicializa‡äes do template de relat¢rio */
{include/i-rpini.i}

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

{include/i-rplbl.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

PROCEDURE SHBrowseForFolder EXTERNAL "shell32.dll":
    DEF INPUT  PARAM lpbi                              AS LONG.
    DEF RETURN PARAM lpItemIDList                      AS LONG.
END PROCEDURE. /* SHBrowseForFolder */

/******************************** Busca Path ********************************/
PROCEDURE SHGetPathFromIDList EXTERNAL "shell32.dll":
    DEF INPUT  PARAM v_cdn_lista                       AS LONG.
    DEF OUTPUT PARAM pszPath                           AS CHARACTER.
END PROCEDURE. /* SHGetPathFromIDList */



/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    IF  c-arq-csv:LOAD-MOUSE-POINTER ("Image~\lupa.cur") IN FRAME f-pg-par THEN.

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
  ENABLE im-pg-cla im-pg-imp im-pg-par im-pg-sel bt-executar bt-cancelar 
         bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY c-base-ini c-base-fim c-tab-ini c-tab-fim c-atr-ini c-atr-fim 
          c-user-ini c-user-fim c-dat-ini c-dat-fim c-prog-ini c-prog-fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-10 IMAGE-11 IMAGE-12 IMAGE-13 IMAGE-14 IMAGE-3 IMAGE-4 IMAGE-5 
         IMAGE-6 IMAGE-7 IMAGE-8 IMAGE-9 c-base-ini c-base-fim c-tab-ini 
         c-tab-fim c-atr-ini c-atr-fim c-user-ini c-user-fim c-dat-ini 
         c-dat-fim c-prog-ini c-prog-fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  ENABLE rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-cla}
  DISPLAY rs-destino c-arquivo rs-execucao tg-imp-param 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 rs-destino bt-config-impr bt-arquivo c-arquivo 
         rs-execucao tg-imp-param 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY l-create l-write l-delete l-arq-excel c-arq-csv l-envia-email 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE RECT-10 l-create l-write l-delete l-arq-excel l-envia-email 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
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

        if input frame f-pg-par l-arq-excel:checked then do:
            run utp/ut-vlarq.p (input input frame f-pg-par c-arq-csv).
            if return-value = "NOK":U then do:
                run utp/ut-msgs.p (input "show", input 73, input "").

                apply "MOUSE-SELECT-CLICK":U to im-pg-par in frame f-relat.
                apply "ENTRY":U to c-arq-csv in frame f-pg-par.
                return error.
            end.
        end.
    end.
    if input frame f-pg-imp rs-execucao = 2 then do: /*Batch*/
        if index(input frame f-pg-par c-arq-csv,":") <> 0
        then do:
            run utp/ut-msgs.p (input 'show', input 4142, input "~"" + input frame f-pg-par c-arq-csv + "~"").
            apply "MOUSE-SELECT-CLICK":U to im-pg-par in frame f-relat.
            apply "ENTRY":U to c-arq-csv in frame f-pg-par.
            return  error.
        end /* if */.
    end /* if */.    

    /* Coloque aqui as valida‡äes das outras p ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p gina com 
       problemas e colocar o focus no campo com problemas */

    /* Aqui sÆo gravados os campos da temp-table que ser  passada como parƒmetro
       para o programa RP.P */

    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame f-pg-imp rs-destino
           tt-param.data-exec       = today
           tt-param.hora-exec       = time
           tt-param.l-imp-param     = input frame f-pg-imp tg-imp-param
           tt-param.classifica      = input frame f-pg-cla rs-classif
           tt-param.desc-classifica = entry((tt-param.classifica - 1) * 2 + 1, 
                                            rs-classif:radio-buttons in frame f-pg-cla).

    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".

    /* Coloque aqui a l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */

    assign  tt-param.c-user-ini  = c-user-ini:screen-value   in frame f-pg-sel
            tt-param.c-user-fim  = c-user-fim:screen-value   in frame f-pg-sel
            tt-param.c-base-ini  = c-base-ini:screen-value   in frame f-pg-sel
            tt-param.c-base-fim  = c-base-fim:screen-value   in frame f-pg-sel
            tt-param.c-tab-ini   = c-tab-ini:screen-value    in frame f-pg-sel
            tt-param.c-tab-fim   = c-tab-fim:screen-value    in frame f-pg-sel
            tt-param.c-atr-ini   = c-atr-ini:screen-value    in frame f-pg-sel
            tt-param.c-atr-fim   = c-atr-fim:screen-value    in frame f-pg-sel
            tt-param.c-dat-ini   = input frame f-pg-sel c-dat-ini
            tt-param.c-dat-fim   = input frame f-pg-sel c-dat-fim
            tt-param.c-prog-ini  = c-prog-ini:screen-value   in frame f-pg-sel
            tt-param.c-prog-fim  = c-prog-fim:screen-value   in frame f-pg-sel
            tt-param.l-create    = if l-create:checked   in frame f-pg-par then yes else no
            tt-param.l-write     = if l-write:checked    in frame f-pg-par then yes else no
            tt-param.l-delete    = if l-delete:checked   in frame f-pg-par then yes else No
            tt-param.gera-excel  = l-arq-excel:CHECKED       IN FRAME f-pg-par
            tt-param.arq-csv     = Input Frame f-pg-par c-arq-csv
            tt-param.envia-email = input frame f-pg-par l-envia-email 
            tt-param.rs-tipo-execucao = INPUT FRAME f-pg-imp rs-execucao.

    /* Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}

    SESSION:SET-WAIT-STATE("general":U).

    {include/i-rprun.i esp/esau0305rp.p}

    {include/i-rpexc.i}

    SESSION:SET-WAIT-STATE("":U).

    {include/i-rptrm.i}
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-get-directory w-relat 
PROCEDURE pi-get-directory :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /************************ Parameter Definition Begin ************************/

    def Input param p_des_titulo
        as character
        format "x(40)"
        no-undo.
    def output param p_nom_path
        as character
        format "x(50)"
        no-undo.
    def output param p_log_cancdo
        as logical
        format "Sim/NÆo"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_cdn_lista_item
        as Integer
        format ">>>,>>9"
        no-undo.
    def var v_mmp_browse
        as MemPtr
        no-undo.
    def var v_mmp_mostra_nom
        as MemPtr
        no-undo.
    def var v_mmp_title_pointer
        as MemPtr
        no-undo.


    /************************** Variable Definition End *************************/

    set-size(v_mmp_browse)        = 32.
    set-size(v_mmp_mostra_nom)    = 260.
    set-size(v_mmp_title_pointer) = length(p_des_titulo) + 1.

    put-string(v_mmp_title_pointer,1) = p_des_titulo.

    put-long(v_mmp_browse, 1) = 0.
    put-long(v_mmp_browse, 5) = 0.
    put-long(v_mmp_browse, 9) = get-pointer-value(v_mmp_mostra_nom).
    put-long(v_mmp_browse,13) = get-pointer-value(v_mmp_title_pointer).
    put-long(v_mmp_browse,17) = 1.
    put-long(v_mmp_browse,21) = 0.
    put-long(v_mmp_browse,25) = 0.
    put-long(v_mmp_browse,29) = 0.

    run SHBrowseForFolder( input  get-pointer-value(v_mmp_browse), 
                           output v_cdn_lista_item).

    /* parse the result: */
    if v_cdn_lista_item = 0 then do:
       p_log_cancdo   = yes.
       p_nom_path = "".
    end.
    else do:
       assign p_log_cancdo = No
              p_nom_path = fill(" ", 260).
       run SHGetPathFromIDList(v_cdn_lista_item, output p_nom_path).
       assign p_nom_path = trim(p_nom_path).
    end.   

    /* free memory: */
    set-size(v_mmp_browse) = 0.
    set-size(v_mmp_mostra_nom) = 0.
    set-size(v_mmp_title_pointer) = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina w-relat 
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


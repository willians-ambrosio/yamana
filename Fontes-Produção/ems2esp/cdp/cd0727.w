&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems2cadme        PROGRESS
          ems2movme        PROGRESS
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
def buffer empresa for ems2cadme.empresa.

{include/i-prgvrs.i CD0727 2.00.00.005}  /*** 010005 ***/


&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i cd0727 MCD}
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
&GLOBAL-DEFINE PGCLA
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

/* Defini‡Æo Temp-table tt-param */
{cdp/cd0727.i}
{include/i_dbvers.i}
define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.

def new global shared var i-ep-codigo-usuario  like ems2cadme.empresa.ep-codigo no-undo.
def new Global shared var l-implanta           as logical    init no.
def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.
def new global shared var h-rsocial AS HANDLE  no-undo.
def new global shared var l-achou-prog          as Logical                          no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-imp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-9 RECT-84 rs-destino bt-arquivo ~
bt-config-impr c-arquivo rs-execucao l-parametros 
&Scoped-Define DISPLAYED-OBJECTS rs-destino c-arquivo rs-execucao ~
l-parametros 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
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

DEFINE RECTANGLE RECT-84
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE l-parametros AS LOGICAL INITIAL yes 
     LABEL "ImpressÆo p gina parƒmetros" 
     VIEW-AS TOGGLE-BOX
     SIZE 33.72 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "M¢dulo" 
      VIEW-AS TEXT 
     SIZE 8.72 BY .67 NO-UNDO.

DEFINE VARIABLE i-modulo AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Contas a Pagar", 1,
"Contas a Receber", 2,
"Ambos", 3
     SIZE 26.29 BY 2.13 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 29.43 BY 2.63.

DEFINE VARIABLE c-esp-fim AS CHARACTER FORMAT "X(02)":U INITIAL "ZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE c-esp-ini AS CHARACTER FORMAT "X(02)":U 
     LABEL "Esp‚cie" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE c-estab-fim LIKE estabelec.cod-estabel
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-estab-ini LIKE estabelec.cod-estabel
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88 NO-UNDO.

DEFINE VARIABLE c-nr-docto-fim AS CHARACTER FORMAT "x(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-nr-docto-ini AS CHARACTER FORMAT "x(16)":U 
     LABEL "Documento" 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-serie-fim AS CHARACTER FORMAT "X(05)":U INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 7.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-serie-ini AS CHARACTER FORMAT "X(5)":U 
     LABEL "S‚rie" 
     VIEW-AS FILL-IN 
     SIZE 7.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-usuario-fim AS CHARACTER FORMAT "X(12)":U INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-usuario-ini AS CHARACTER FORMAT "x(12)":U 
     LABEL "Usu rio" 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .88 NO-UNDO.

DEFINE VARIABLE da-data-fim AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88 NO-UNDO.

DEFINE VARIABLE da-data-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Elimina‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88 NO-UNDO.

DEFINE VARIABLE i-emitente-fim AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE i-emitente-ini AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     LABEL "Emitente" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE i-empresa-fim LIKE empresa.ep-codigo
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE i-empresa-ini LIKE empresa.ep-codigo
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .79.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-27
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-34
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-35
     FILENAME "image\im-fir":U
     SIZE 3 BY .79.

DEFINE IMAGE IMAGE-36
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-37
     FILENAME "image\im-fir":U
     SIZE 3 BY .79.

DEFINE IMAGE IMAGE-38
     FILENAME "image\im-las":U
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

DEFINE IMAGE IMAGE-41
     FILENAME "image\im-fir":U
     SIZE 3 BY .79.

DEFINE IMAGE IMAGE-42
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
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
     l-parametros AT ROW 7.96 COL 3.57
     text-destino AT ROW 1.63 COL 3.86 NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     "Parƒmetros de ImpressÆo" VIEW-AS TEXT
          SIZE 24.57 BY .67 AT ROW 7.13 COL 4.14
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
     RECT-84 AT ROW 7.46 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10.

DEFINE FRAME f-pg-par
     i-modulo AT ROW 1.83 COL 20.14 NO-LABEL
     FILL-IN-1 AT ROW 1.21 COL 18.72 COLON-ALIGNED NO-LABEL
     RECT-10 AT ROW 1.46 COL 19.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 75 BY 10.

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     im-pg-sel AT ROW 1.5 COL 2.14
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-imp AT ROW 1.5 COL 33.57
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

DEFINE FRAME f-pg-sel
     i-empresa-ini AT ROW 1.33 COL 22 COLON-ALIGNED HELP
          ""
     i-empresa-fim AT ROW 1.33 COL 47 COLON-ALIGNED HELP
          "" NO-LABEL
     c-estab-ini AT ROW 2.33 COL 22 COLON-ALIGNED HELP
          ""
     c-estab-fim AT ROW 2.33 COL 47 COLON-ALIGNED HELP
          "" NO-LABEL
     c-esp-ini AT ROW 3.33 COL 22 COLON-ALIGNED
     c-esp-fim AT ROW 3.33 COL 47 COLON-ALIGNED NO-LABEL
     c-serie-ini AT ROW 4.33 COL 22 COLON-ALIGNED
     c-serie-fim AT ROW 4.33 COL 47 COLON-ALIGNED NO-LABEL
     c-nr-docto-ini AT ROW 5.33 COL 22 COLON-ALIGNED
     c-nr-docto-fim AT ROW 5.33 COL 47 COLON-ALIGNED NO-LABEL
     i-emitente-ini AT ROW 6.33 COL 22 COLON-ALIGNED
     i-emitente-fim AT ROW 6.33 COL 47 COLON-ALIGNED NO-LABEL
     da-data-ini AT ROW 7.33 COL 22 COLON-ALIGNED
     da-data-fim AT ROW 7.33 COL 47 COLON-ALIGNED NO-LABEL
     c-usuario-ini AT ROW 8.33 COL 22 COLON-ALIGNED
     c-usuario-fim AT ROW 8.33 COL 47 COLON-ALIGNED NO-LABEL
     IMAGE-34 AT ROW 1.33 COL 37.57
     IMAGE-27 AT ROW 1.33 COL 46
     IMAGE-1 AT ROW 2.33 COL 37.57
     IMAGE-2 AT ROW 2.33 COL 46
     IMAGE-3 AT ROW 3.33 COL 37.57
     IMAGE-7 AT ROW 3.33 COL 46
     IMAGE-35 AT ROW 4.33 COL 37.57
     IMAGE-36 AT ROW 4.33 COL 46
     IMAGE-37 AT ROW 5.33 COL 37.57
     IMAGE-38 AT ROW 5.33 COL 46
     IMAGE-4 AT ROW 6.33 COL 37.57
     IMAGE-8 AT ROW 6.33 COL 46
     IMAGE-39 AT ROW 7.33 COL 37.57
     IMAGE-40 AT ROW 7.33 COL 46
     IMAGE-41 AT ROW 8.33 COL 37.57
     IMAGE-42 AT ROW 8.33 COL 46
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62.


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
  CREATE WINDOW w-relat ASSIGN
         HIDDEN             = YES
         TITLE              = "<Title>"
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
/* SETTINGS FOR FRAME f-pg-imp
   FRAME-NAME                                                           */
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
/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME f-pg-par
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-1:PRIVATE-DATA IN FRAME f-pg-par     = 
                "M¢dulo".

/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FILL-IN c-estab-fim IN FRAME f-pg-sel
   LIKE = ems2movme.estabelec.cod-estabel EXP-SIZE                      */
/* SETTINGS FOR FILL-IN c-estab-ini IN FRAME f-pg-sel
   LIKE = ems2movme.estabelec.cod-estabel EXP-SIZE                      */
/* SETTINGS FOR FILL-IN i-empresa-fim IN FRAME f-pg-sel
   LIKE = ems2cadme.empresa.ep-codigo EXP-SIZE                          */
/* SETTINGS FOR FILL-IN i-empresa-ini IN FRAME f-pg-sel
   LIKE = ems2cadme.empresa.ep-codigo EXP-SIZE                          */
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
ON END-ERROR OF w-relat /* <Title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* <Title> */
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


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME c-esp-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-esp-fim w-relat
ON MOUSE-SELECT-DBLCLICK OF c-esp-fim IN FRAME f-pg-sel
DO:
  apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-esp-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-esp-ini w-relat
ON MOUSE-SELECT-DBLCLICK OF c-esp-ini IN FRAME f-pg-sel /* Esp‚cie */
DO:
  apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-estab-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-estab-fim w-relat
ON MOUSE-SELECT-DBLCLICK OF c-estab-fim IN FRAME f-pg-sel
DO:
  apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-estab-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-estab-ini w-relat
ON MOUSE-SELECT-DBLCLICK OF c-estab-ini IN FRAME f-pg-sel /* Estab */
DO:
  apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-nr-docto-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nr-docto-fim w-relat
ON MOUSE-SELECT-DBLCLICK OF c-nr-docto-fim IN FRAME f-pg-sel
DO:
  apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-nr-docto-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-nr-docto-ini w-relat
ON MOUSE-SELECT-DBLCLICK OF c-nr-docto-ini IN FRAME f-pg-sel /* Documento */
DO:
  apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-serie-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-serie-fim w-relat
ON MOUSE-SELECT-DBLCLICK OF c-serie-fim IN FRAME f-pg-sel
DO:
  apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-serie-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-serie-ini w-relat
ON MOUSE-SELECT-DBLCLICK OF c-serie-ini IN FRAME f-pg-sel /* S‚rie */
DO:
  apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-usuario-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-usuario-fim w-relat
ON MOUSE-SELECT-DBLCLICK OF c-usuario-fim IN FRAME f-pg-sel
DO:
  apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-usuario-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-usuario-ini w-relat
ON MOUSE-SELECT-DBLCLICK OF c-usuario-ini IN FRAME f-pg-sel /* Usu rio */
DO:
  apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME da-data-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL da-data-fim w-relat
ON MOUSE-SELECT-DBLCLICK OF da-data-fim IN FRAME f-pg-sel
DO:
  apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME da-data-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL da-data-ini w-relat
ON MOUSE-SELECT-DBLCLICK OF da-data-ini IN FRAME f-pg-sel /* Data Elimina‡Æo */
DO:
  apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-emitente-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-emitente-fim w-relat
ON MOUSE-SELECT-DBLCLICK OF i-emitente-fim IN FRAME f-pg-sel
DO:
  apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-emitente-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-emitente-ini w-relat
ON MOUSE-SELECT-DBLCLICK OF i-emitente-ini IN FRAME f-pg-sel /* Emitente */
DO:
  apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-empresa-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-empresa-fim w-relat
ON F5 OF i-empresa-fim IN FRAME f-pg-sel
DO:
    assign l-implanta = no.
    {include/zoomvar.i &prog-zoom=unzoom/z01un004.w
                       &campo=i-empresa-fim
                       &campozoom=ep-codigo}    
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-empresa-fim w-relat
ON MOUSE-SELECT-DBLCLICK OF i-empresa-fim IN FRAME f-pg-sel
DO:
  apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-empresa-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-empresa-ini w-relat
ON F5 OF i-empresa-ini IN FRAME f-pg-sel /* Empresa */
DO:
    assign l-implanta = no.
    {include/zoomvar.i &prog-zoom=unzoom/z01un004.w
                       &campo=i-empresa-ini
                       &campozoom=ep-codigo}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-empresa-ini w-relat
ON MOUSE-SELECT-DBLCLICK OF i-empresa-ini IN FRAME f-pg-sel /* Empresa */
DO:
  apply "F5" to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "CD0727" "2.00.00.005"}

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
  
    assign da-data-ini:screen-value in frame f-pg-sel = string(today, "99/99/9999")
           da-data-fim:screen-value in frame f-pg-sel = string(today, "99/99/9999").
           
    &if "{&ems2movme_version}" < "2.01" &then
              assign  i-emitente-ini:screen-value in frame f-pg-sel = string(0)
                      i-emitente-fim:screen-value in frame f-pg-sel = string(999999).
       
          &else
             assign  i-emitente-ini:screen-value in frame f-pg-sel = string(0)
                     i-emitente-fim:screen-value in frame f-pg-sel = string(999999999).
        &endif

    &IF "{&ems2cadme_version}" >= "2.071" &THEN
    ASSIGN i-empresa-fim:SCREEN-VALUE IN FRAME f-pg-sel = "ZZZZZ"
           c-estab-fim:SCREEN-VALUE IN FRAME f-pg-sel = "ZZZZZ".
    &ELSE
    ASSIGN i-empresa-fim:SCREEN-VALUE IN FRAME f-pg-sel = "ZZZ"
           c-estab-fim:SCREEN-VALUE IN FRAME f-pg-sel = "ZZZ".
    &ENDIF
    
    
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
  ENABLE im-pg-sel im-pg-par im-pg-imp bt-executar bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY i-empresa-ini i-empresa-fim c-estab-ini c-estab-fim c-esp-ini 
          c-esp-fim c-serie-ini c-serie-fim c-nr-docto-ini c-nr-docto-fim 
          i-emitente-ini i-emitente-fim da-data-ini da-data-fim c-usuario-ini 
          c-usuario-fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-34 IMAGE-27 IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-7 IMAGE-35 IMAGE-36 
         IMAGE-37 IMAGE-38 IMAGE-4 IMAGE-8 IMAGE-39 IMAGE-40 IMAGE-41 IMAGE-42 
         i-empresa-ini i-empresa-fim c-estab-ini c-estab-fim c-esp-ini 
         c-esp-fim c-serie-ini c-serie-fim c-nr-docto-ini c-nr-docto-fim 
         i-emitente-ini i-emitente-fim da-data-ini da-data-fim c-usuario-ini 
         c-usuario-fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino c-arquivo rs-execucao l-parametros 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 RECT-84 rs-destino bt-arquivo bt-config-impr c-arquivo 
         rs-execucao l-parametros 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY i-modulo FILL-IN-1 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE RECT-10 i-modulo 
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
    end.
       
    /* Coloque aqui as valida‡äes das outras p ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p gina com 
       problemas e colocar o focus no campo com problemas */

    if  input frame f-pg-sel i-empresa-fim <
        input frame f-pg-sel i-empresa-ini then do:
        
        {utp/ut-field.i ems2cadme empresa ep-codigo 1}
        run utp/ut-msgs.p (input "show",
                           input 142,
                           input trim(return-value) + "~~" +
                                 trim(return-value)).
        apply "ENTRY" to i-empresa-fim in frame f-pg-sel.
        return "NOK".
    end.   

    if  input frame f-pg-sel c-estab-fim <
        input frame f-pg-sel c-estab-ini then do:
        
        {utp/ut-field.i ems2movme estabelec cod-estabel 1}
        run utp/ut-msgs.p (input "show",
                           input 142,
                           input trim(return-value) + "~~" +
                                 trim(return-value)).
        apply "ENTRY" to c-estab-fim in frame f-pg-sel.
        return "NOK".
    end.   

    if  input frame f-pg-sel c-esp-fim <
        input frame f-pg-sel c-esp-ini then do:
        
        {utp/ut-field.i ems2movme espec-ap cod-esp 1}
        run utp/ut-msgs.p (input "show",
                           input 142,
                           input trim(return-value) + "~~" +
                                 trim(return-value)).
        apply "ENTRY" to c-esp-fim in frame f-pg-sel.
        return "NOK".
    end.   

    if  input frame f-pg-sel c-serie-fim <
        input frame f-pg-sel c-serie-ini then do:
        
        {utp/ut-field.i ems2movme titulo serie 1}
        run utp/ut-msgs.p (input "show",
                           input 142,
                           input trim(return-value) + "~~" +
                                 trim(return-value)).
        apply "ENTRY" to c-serie-fim in frame f-pg-sel.
        return "NOK".
    end.   

    if  input frame f-pg-sel c-nr-docto-fim <
        input frame f-pg-sel c-nr-docto-ini then do:
        
        {utp/ut-field.i ems2movme titulo nr-docto 1}
        run utp/ut-msgs.p (input "show",
                           input 142,
                           input trim(return-value) + "~~" +
                                 trim(return-value)).
        apply "ENTRY" to c-nr-docto-fim in frame f-pg-sel.
        return "NOK".
    end.   

    if  input frame f-pg-sel i-emitente-fim <
        input frame f-pg-sel i-emitente-ini then do:
        
        {utp/ut-field.i ems2movme tit-ap cod-emitente 1}
        run utp/ut-msgs.p (input "show",
                           input 142,
                           input trim(return-value) + "~~" +
                                 trim(return-value)).
        apply "ENTRY" to i-emitente-fim in frame f-pg-sel.
        return "NOK".
    end.   

    if  input frame f-pg-sel da-data-fim <
        input frame f-pg-sel da-data-ini then do:
        
        {utp/ut-field.i ems2movme hist-tit-eliminados data-elim-canc 1}
        run utp/ut-msgs.p (input "show",
                           input 142,
                           input trim(return-value) + "~~" +
                                 trim(return-value)).
        apply "ENTRY" to da-data-fim in frame f-pg-sel.
        return "NOK".
    end.   
        
    if  input frame f-pg-sel c-usuario-fim <
        input frame f-pg-sel c-usuario-ini then do:
        
        {utp/ut-field.i ems2movme hist-tit-eliminados usuario-elim-canc 1}
        run utp/ut-msgs.p (input "show",
                           input 142,
                           input trim(return-value) + "~~" +
                                 trim(return-value)).
        apply "ENTRY" to c-usuario-fim in frame f-pg-sel.
        return "NOK".
    end.   
    
    /* Aqui sÆo gravados os campos da temp-table que ser  passada como parƒmetro
       para o programa RP.P */
    
    create tt-param.
    assign tt-param.usuario             = c-seg-usuario
           tt-param.destino             = input frame f-pg-imp rs-destino
           tt-param.data-exec           = today
           tt-param.hora-exec           = time
/*            tt-param.i-empresa           = STRING (i-ep-codigo-usuario) */
           tt-param.i-modulo            = input frame f-pg-par i-modulo
           tt-param.c-modulo            = entry((tt-param.i-modulo - 1) * 2 + 1,
                                            i-modulo:radio-buttons in frame f-pg-par)
           tt-param.i-empresa-ini       = input frame f-pg-sel i-empresa-ini
           tt-param.i-empresa-fim       = input frame f-pg-sel i-empresa-fim
           tt-param.c-estab-ini         = input frame f-pg-sel c-estab-ini
           tt-param.c-estab-fim         = input frame f-pg-sel c-estab-fim
           tt-param.c-esp-ini           = input frame f-pg-sel c-esp-ini
           tt-param.c-esp-fim           = input frame f-pg-sel c-esp-fim
           tt-param.c-serie-ini         = input frame f-pg-sel c-serie-ini
           tt-param.c-serie-fim         = input frame f-pg-sel c-serie-fim
           tt-param.c-nr-docto-ini      = input frame f-pg-sel c-nr-docto-ini
           tt-param.c-nr-docto-fim      = input frame f-pg-sel c-nr-docto-fim
           tt-param.i-emitente-ini      = input frame f-pg-sel i-emitente-ini
           tt-param.i-emitente-fim      = input frame f-pg-sel i-emitente-fim
           tt-param.da-data-ini         = input frame f-pg-sel da-data-ini
           tt-param.da-data-fim         = input frame f-pg-sel da-data-fim
           tt-param.c-usuario-ini       = input frame f-pg-sel c-usuario-ini
           tt-param.c-usuario-fim       = input frame f-pg-sel c-usuario-fim
           tt-param.log-pag-parametros  = input frame f-pg-imp l-parametros.
    
    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".
    
    /* Coloque aqui a l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */
    
    
    
    /* Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i cdp/cd0727rp.p}
    
    {include/i-rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
    {include/i-rptrm.i}
end.
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
     Tables specified for this Window, and there are no
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


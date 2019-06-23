&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
/*:T*******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i APYA510 12.1.17.000}

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

/*:T Preprocessadores do Template de Relat¢rio                            */
/*:T Obs: Retirar o valor do preprocessador para as p ginas que nÆo existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA 
&GLOBAL-DEFINE PGPAR 
&GLOBAL-DEFINE PGDIG 
&GLOBAL-DEFINE PGIMP f-pg-imp

&GLOBAL-DEFINE RTF   NO
  
/* Parameters Definitions ---                                           */
DEF BUFFER cotacao FOR ems2cadme.cotacao.

/* Temporary Table Definitions ---                                      */

define temp-table tt-param no-undo
    field destino                   as integer
    field arquivo                   as char format "x(35)"
    field usuario                   as char format "x(12)"
    field data-exec                 as date
    field hora-exec                 as integer
    FIELD cod_emp_ini               AS CHAR
    FIELD cod_emp_fim               AS CHAR
    FIELD cod_tip_produt_financ_ini as character
    FIELD cod_tip_produt_financ_fim as character
    FIELD cod_produt_financ_ini     as character
    FIELD cod_produt_financ_fim     as character
    FIELD cod_admdra_apf_ini        as character
    FIELD cod_admdra_apf_fim        as character
    FIELD cod_operac_financ_ini     as character
    FIELD cod_operac_financ_fim     as character
    FIELD data_movimento_ini        AS DATE
    FIELD data_movimento_fim        AS DATE
    FIELD cod_banco_ini             AS CHAR
    FIELD cod_banco_fim             AS CHAR.


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
def var c-rtf              as char    no-undo.
def var c-modelo-default   as char    no-undo.

/*15/02/2005 - tech1007 - Variavel definida para tratar se o programa est  rodando no WebEnabler*/
DEFINE SHARED VARIABLE hWenController AS HANDLE NO-UNDO.


DEFINE NEW GLOBAL SHARED VARIABLE v_rec_plano_cta_ctbl AS RECID NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE v_rec_indic_econ     AS RECID NO-UNDO.

DEFINE VARIABLE dt-aux AS DATE        NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-imp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-9 rs-destino bt-config-impr ~
bt-arquivo c-arquivo rs-execucao 
&Scoped-Define DISPLAYED-OBJECTS rs-destino c-arquivo rs-execucao 

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

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 3 
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
     SIZE 46.29 BY 2.79.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE cod_admdra_apf_fim AS CHARACTER FORMAT "x(5)" INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88.

DEFINE VARIABLE cod_admdra_apf_ini AS CHARACTER FORMAT "x(5)" 
     LABEL "Administradora" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88.

DEFINE VARIABLE cod_banco_fim AS CHARACTER FORMAT "x(5)" INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88.

DEFINE VARIABLE cod_banco_ini AS CHARACTER FORMAT "x(5)" 
     LABEL "Banco" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88.

DEFINE VARIABLE cod_emp_fim AS CHARACTER FORMAT "x(5)" INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88.

DEFINE VARIABLE cod_emp_ini AS CHARACTER FORMAT "x(5)" 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88.

DEFINE VARIABLE cod_operac_financ_fim AS CHARACTER FORMAT "x(10)" INITIAL "ZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 14.14 BY .88.

DEFINE VARIABLE cod_operac_financ_ini AS CHARACTER FORMAT "x(10)" 
     LABEL "Opera‡Æo Financeira" 
     VIEW-AS FILL-IN 
     SIZE 14.14 BY .88.

DEFINE VARIABLE cod_produt_financ_fim AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12.14 BY .88.

DEFINE VARIABLE cod_produt_financ_ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Produto Financeiro" 
     VIEW-AS FILL-IN 
     SIZE 12.14 BY .88.

DEFINE VARIABLE cod_tip_produt_financ_fim AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12.14 BY .88.

DEFINE VARIABLE cod_tip_produt_financ_ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Tipo Prod Financeiro" 
     VIEW-AS FILL-IN 
     SIZE 12.14 BY .88.

DEFINE VARIABLE data_movimento_fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/2999 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE data_movimento_ini AS DATE FORMAT "99/99/9999":U INITIAL 12/31/17 
     LABEL "Data Movimenta‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-13
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
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
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     im-pg-imp AT ROW 1.5 COL 17.86
     im-pg-sel AT ROW 1.5 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar WIDGET-ID 100.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 1.63 COL 3.29 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     bt-config-impr AT ROW 2.71 COL 43.29 HELP
          "Configura‡Æo da impressora"
     bt-arquivo AT ROW 2.71 COL 43.29 HELP
          "Escolha do nome do arquivo"
     c-arquivo AT ROW 2.75 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 6 COL 3.86 HELP
          "Modo de Execu‡Æo" NO-LABEL
     text-destino AT ROW 1.04 COL 3.86 NO-LABEL
     text-modo AT ROW 5.25 COL 2.14 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.33 COL 2.14
     RECT-9 AT ROW 5.46 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 77.43 BY 10.5 WIDGET-ID 100.

DEFINE FRAME f-pg-sel
     cod_emp_ini AT ROW 2.75 COL 35.14 RIGHT-ALIGNED HELP
          "C¢digo Administradora" WIDGET-ID 50
     cod_emp_fim AT ROW 2.75 COL 44.72 COLON-ALIGNED HELP
          "C¢digo Administradora" NO-LABEL WIDGET-ID 48
     cod_banco_ini AT ROW 3.75 COL 35.14 RIGHT-ALIGNED HELP
          "C¢digo Administradora" WIDGET-ID 42
     cod_banco_fim AT ROW 3.75 COL 44.72 COLON-ALIGNED HELP
          "C¢digo Administradora" NO-LABEL WIDGET-ID 40
     cod_tip_produt_financ_ini AT ROW 4.75 COL 35.14 RIGHT-ALIGNED HELP
          "C¢digo Tipo Produto Financeiro" WIDGET-ID 4
     cod_tip_produt_financ_fim AT ROW 4.75 COL 44.72 COLON-ALIGNED HELP
          "C¢digo Tipo Produto Financeiro" NO-LABEL WIDGET-ID 34
     cod_produt_financ_ini AT ROW 5.75 COL 35.14 RIGHT-ALIGNED HELP
          "C¢digo do Produto Financeiro" WIDGET-ID 2
     cod_produt_financ_fim AT ROW 5.75 COL 44.72 COLON-ALIGNED HELP
          "C¢digo do Produto Financeiro" NO-LABEL WIDGET-ID 32
     cod_admdra_apf_ini AT ROW 6.75 COL 35.14 RIGHT-ALIGNED HELP
          "C¢digo Administradora" WIDGET-ID 6
     cod_admdra_apf_fim AT ROW 6.75 COL 44.72 COLON-ALIGNED HELP
          "C¢digo Administradora" NO-LABEL WIDGET-ID 28
     cod_operac_financ_ini AT ROW 7.75 COL 35.14 RIGHT-ALIGNED HELP
          "C¢digo da Opera‡Æo Financeira" WIDGET-ID 20
     cod_operac_financ_fim AT ROW 7.75 COL 44.72 COLON-ALIGNED HELP
          "C¢digo da Opera‡Æo Financeira" NO-LABEL WIDGET-ID 30
     data_movimento_ini AT ROW 8.75 COL 35.14 RIGHT-ALIGNED WIDGET-ID 22
     data_movimento_fim AT ROW 8.75 COL 44.72 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     "Este relatorio ser  executado somente a partir de 31/12/2017." VIEW-AS TEXT
          SIZE 57.86 BY .67 AT ROW 10.5 COL 13 WIDGET-ID 38
          FGCOLOR 9 
     IMAGE-1 AT ROW 4.75 COL 37.29
     IMAGE-2 AT ROW 4.75 COL 42.43
     IMAGE-3 AT ROW 5.75 COL 37.29 WIDGET-ID 8
     IMAGE-4 AT ROW 5.75 COL 42.43 WIDGET-ID 10
     IMAGE-5 AT ROW 6.75 COL 37.29 WIDGET-ID 12
     IMAGE-6 AT ROW 6.75 COL 42.43 WIDGET-ID 14
     IMAGE-7 AT ROW 7.75 COL 37.29 WIDGET-ID 16
     IMAGE-8 AT ROW 7.75 COL 42.43 WIDGET-ID 18
     IMAGE-9 AT ROW 8.75 COL 37.29 WIDGET-ID 24
     IMAGE-10 AT ROW 8.75 COL 42.43 WIDGET-ID 26
     IMAGE-11 AT ROW 3.75 COL 37.29 WIDGET-ID 44
     IMAGE-12 AT ROW 3.75 COL 42.43 WIDGET-ID 46
     IMAGE-13 AT ROW 2.75 COL 37.29 WIDGET-ID 52
     IMAGE-14 AT ROW 2.75 COL 42.43 WIDGET-ID 54
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62 WIDGET-ID 100.


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
         TITLE              = "Relat¢rio Intercompany"
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

/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FILL-IN cod_admdra_apf_ini IN FRAME f-pg-sel
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN cod_banco_ini IN FRAME f-pg-sel
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN cod_emp_ini IN FRAME f-pg-sel
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN cod_operac_financ_ini IN FRAME f-pg-sel
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN cod_produt_financ_ini IN FRAME f-pg-sel
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN cod_tip_produt_financ_ini IN FRAME f-pg-sel
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN data_movimento_ini IN FRAME f-pg-sel
   ALIGN-R                                                              */
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
ON END-ERROR OF w-relat /* Relat¢rio Intercompany */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Relat¢rio Intercompany */
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
   apply "close":U to this-procedure.
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


&Scoped-define SELF-NAME im-pg-imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-imp w-relat
ON MOUSE-SELECT-CLICK OF im-pg-imp IN FRAME f-relat
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
/*Alterado 15/02/2005 - tech1007 - Evento alterado para correto funcionamento dos novos widgets
  utilizados para a funcionalidade de RTF*/
do  with frame f-pg-imp:
/*     case self:screen-value:                                                                      */
/*         when "1" then do:                                                                        */
/*             assign c-arquivo:sensitive    = no                                                   */
/*                    bt-arquivo:visible     = no                                                   */
/*                    bt-config-impr:visible = YES                                                  */
/*                    /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para     */
/*                      verificar se o RTF est  ativo*/                                             */
/*                    &IF "{&RTF}":U = "YES":U &THEN                                                */
/*                    l-habilitaRtf:sensitive  = NO                                                 */
/*                    l-habilitaRtf:SCREEN-VALUE IN FRAME f-pg-imp = "No"                           */
/*                    l-habilitaRtf = NO                                                            */
/*                    &endif                                                                        */
/*                    /*Fim alteracao 17/02/2005*/                                                  */
/*                    .                                                                             */
/*         end.                                                                                     */
/*         when "2" then do:                                                                        */
/*             assign c-arquivo:sensitive     = yes                                                 */
/*                    bt-arquivo:visible      = yes                                                 */
/*                    bt-config-impr:visible  = NO                                                  */
/*                    /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para     */
/*                      verificar se o RTF est  ativo*/                                             */
/*                    &IF "{&RTF}":U = "YES":U &THEN                                                */
/*                    l-habilitaRtf:sensitive  = YES                                                */
/*                    &endif                                                                        */
/*                    /*Fim alteracao 17/02/2005*/                                                  */
/*                    .                                                                             */
/*         end.                                                                                     */
/*         when "3" then do:                                                                        */
/*             assign c-arquivo:sensitive     = no                                                  */
/*                    bt-arquivo:visible      = no                                                  */
/*                    bt-config-impr:visible  = no                                                  */
/*                    /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para     */
/*                      verificar se o RTF est  ativo*/                                             */
/*                    &IF "{&RTF}":U = "YES":U &THEN                                                */
/*                    l-habilitaRtf:sensitive  = YES                                                */
/*                    &endif                                                                        */
/*                    /*Fim alteracao 17/02/2005*/                                                  */
/*                    .                                                                             */
/*             /*Alterado 15/02/2005 - tech1007 - Teste para funcionar corretamente no WebEnabler*/ */
/*             &IF "{&RTF}":U = "YES":U &THEN                                                       */
/*             IF VALID-HANDLE(hWenController) THEN DO:                                             */
/*                 ASSIGN l-habilitaRtf:sensitive  = NO                                             */
/*                        l-habilitaRtf:SCREEN-VALUE IN FRAME f-pg-imp = "No"                       */
/*                        l-habilitaRtf = NO.                                                       */
/*             END.                                                                                 */
/*             &endif                                                                               */
/*             /*Fim alteracao 15/02/2005*/                                                         */
/*         end.                                                                                     */
/*     end case.                                                                                    */
end.
&IF "{&RTF}":U = "YES":U &THEN
RUN pi-habilitaRtf.
&endif
/*Fim alteracao 15/02/2005*/
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

{utp/ut9000.i "APYA510" "12.1.17.000"}

/*:T inicializa‡äes do template de relat¢rio */
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
   
    ASSIGN data_movimento_fim = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1.
    ASSIGN data_movimento_ini:SCREEN-VALUE IN FRAME f-pg-sel = STRING(DATE(MONTH(data_movimento_fim),1,YEAR(data_movimento_fim)) - 1)
           data_movimento_fim:SCREEN-VALUE IN FRAME f-pg-sel = STRING(data_movimento_fim).
  
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
  ENABLE im-pg-imp im-pg-sel bt-executar bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY cod_emp_ini cod_emp_fim cod_banco_ini cod_banco_fim 
          cod_tip_produt_financ_ini cod_tip_produt_financ_fim 
          cod_produt_financ_ini cod_produt_financ_fim cod_admdra_apf_ini 
          cod_admdra_apf_fim cod_operac_financ_ini cod_operac_financ_fim 
          data_movimento_ini data_movimento_fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 
         IMAGE-9 IMAGE-10 IMAGE-11 IMAGE-12 IMAGE-13 IMAGE-14 cod_emp_ini 
         cod_emp_fim cod_banco_ini cod_banco_fim cod_tip_produt_financ_ini 
         cod_tip_produt_financ_fim cod_produt_financ_ini cod_produt_financ_fim 
         cod_admdra_apf_ini cod_admdra_apf_fim cod_operac_financ_ini 
         cod_operac_financ_fim data_movimento_ini data_movimento_fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 rs-destino bt-config-impr bt-arquivo c-arquivo 
         rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
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
    /*14/02/2005 - tech1007 - Alterada condicao para nÆo considerar mai o RTF como destino*/
    if input frame f-pg-imp rs-destino = 2 and
       input frame f-pg-imp rs-execucao = 1 then do:
        run utp/ut-vlarq.p (input input frame f-pg-imp c-arquivo).
        
        if return-value = "NOK":U then do:
            run utp/ut-msgs.p (input "show":U, input 73, input "").
            
            apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
            apply "ENTRY":U to c-arquivo in frame f-pg-imp.
            return error.
        end.
    end.
    
    IF INPUT FRAME f-pg-sel cod_tip_produt_financ_ini > INPUT FRAME f-pg-sel cod_tip_produt_financ_fim THEN DO:
       APPLY "MOUSE-SELECT-CLICK":U to im-pg-sel IN FRAME f-relat.  
       {utp/ut-field.i ems5 produt_financ cod_tip_produt_financ 1}
       RUN utp/ut-msgs.p (INPUT "show":U, INPUT 142, input RETURN-VALUE + "~~" + RETURN-VALUE).      
       APPLY "ENTRY":U to cod_tip_produt_financ_ini IN FRAME f-pg-sel.       
       return error.                                                 
    END.
    
    
    IF INPUT FRAME f-pg-sel cod_produt_financ_ini > INPUT FRAME f-pg-sel cod_produt_financ_fim THEN DO:
       APPLY "MOUSE-SELECT-CLICK":U to im-pg-sel IN FRAME f-relat.  
       {utp/ut-field.i ems5 produt_financ cod_produt_financ 1}
       RUN utp/ut-msgs.p (INPUT "show":U, INPUT 142, input RETURN-VALUE + "~~" + RETURN-VALUE).      
       APPLY "ENTRY":U to cod_produt_financ_ini IN FRAME f-pg-sel.       
       return error.                                                 
    END.

    IF INPUT FRAME f-pg-sel cod_admdra_apf_ini > INPUT FRAME f-pg-sel cod_admdra_apf_fim THEN DO:
       APPLY "MOUSE-SELECT-CLICK":U to im-pg-sel IN FRAME f-relat.  
       {utp/ut-field.i mgesp admdra_apf cod_admdra_apf 1}
       RUN utp/ut-msgs.p (INPUT "show":U, INPUT 142, input RETURN-VALUE + "~~" + RETURN-VALUE).      
       APPLY "ENTRY":U to cod_admdra_apf_ini IN FRAME f-pg-sel.              
       return error.                                                 
    END.

    IF INPUT FRAME f-pg-sel cod_operac_financ_ini > INPUT FRAME f-pg-sel cod_operac_financ_fim THEN DO:
       APPLY "MOUSE-SELECT-CLICK":U to im-pg-sel IN FRAME f-relat.  
       {utp/ut-field.i ems5 operac_financ cod_operac_financ 1}
       RUN utp/ut-msgs.p (INPUT "show":U, INPUT 142, input RETURN-VALUE + "~~" + RETURN-VALUE).      
       APPLY "ENTRY":U to cod_operac_financ_ini IN FRAME f-pg-sel.       
       return error.                                                 
    END.

    IF INPUT FRAME f-pg-sel data_movimento_ini > INPUT FRAME f-pg-sel data_movimento_fim THEN DO:
       APPLY "MOUSE-SELECT-CLICK":U to im-pg-sel IN FRAME f-relat.  
       
       RUN utp/ut-msgs.p (INPUT "show":U, INPUT 142, input "Data Movimento" + "~~" + "Data Movimento").      
       APPLY "ENTRY":U to data_movimento_ini IN FRAME f-pg-sel.       
       return error.                                                 
    END.

    IF INPUT FRAME f-pg-sel data_movimento_fim > TODAY THEN DO: 

        MESSAGE "Data final nÆo poder  ser superior a data atual!" 
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY "ENTRY":U TO data_movimento_fim IN FRAME f-pg-sel.       
       RETURN ERROR.
    END.

    ASSIGN data_movimento_ini = INPUT FRAME f-pg-sel data_movimento_ini 
           data_movimento_fim = INPUT FRAME f-pg-sel data_movimento_fim.

    FIND cotacao NO-LOCK WHERE
         cotacao.mo-codigo = 1 AND 
         cotacao.ano-periodo = STRING(YEAR(data_movimento_ini),"9999") + STRING(MONTH(data_movimento_ini),"99") NO-ERROR.
    IF NOT AVAIL cotacao THEN DO:

        MESSAGE "NÆo encontrada cota‡Æo para o per¡odo: " + STRING(MONTH(data_movimento_ini),"99") + "/" + STRING(YEAR(data_movimento_ini),"9999") SKIP
                "Verifique o cadastro de cota‡äes"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.                                                       

    END.
    ELSE DO:

        IF cotacao.cotacao[DAY(data_movimento_ini)] = 0 THEN DO:

            MESSAGE "NÆo encontrada cota‡Æo para a Data: " data_movimento_ini SKIP
                "Verifique o cadastro de cota‡äes"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RETURN NO-APPLY. 

        END.
    END.

    FIND cotacao NO-LOCK WHERE
         cotacao.mo-codigo = 1 AND 
         cotacao.ano-periodo = STRING(YEAR(data_movimento_fim),"9999") + STRING(MONTH(data_movimento_fim),"99") NO-ERROR.
    IF NOT AVAIL cotacao THEN DO:

        MESSAGE "NÆo encontrada cota‡Æo para o per¡odo: " + STRING(MONTH(data_movimento_fim),"99") + "/" + STRING(YEAR(data_movimento_fim),"9999") SKIP
                "Verifique o cadastro de cota‡äes"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.
    ELSE DO:

        IF cotacao.cotacao[DAY(data_movimento_fim)] = 0 THEN DO:

            MESSAGE "NÆo encontrada cota‡Æo para a Data: " data_movimento_fim SKIP
                    "Verifique o cadastro de cota‡äes"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RETURN NO-APPLY. 
        END.
    END.

    /* Valida‡Æo da cota‡Æo m‚dia */


    /* Valida‡Æo da data inicial e final - Obrigatoriamente devem ser o £ltimo dia do mˆs */
    ASSIGN dt-aux = DATE(MONTH(data_movimento_fim),28,YEAR(data_movimento_fim)) + 4.

    IF DATE(MONTH(dt-aux),1,YEAR(dt-aux)) - 1 <> data_movimento_fim
        THEN DO:

        MESSAGE "Data FINAL inv lida! " SKIP
                "A data FINAL deve ser necessariamente o £ltimo dia DO mˆs!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.

    ASSIGN dt-aux = DATE(MONTH(data_movimento_ini),28,YEAR(data_movimento_ini)) + 4.

    IF DATE(MONTH(dt-aux),1,YEAR(dt-aux)) - 1 <> data_movimento_ini
        THEN DO:

        MESSAGE "Data INICIAL inv lida! " SKIP
                "A data INICIAL deve ser necessariamente o £ltimo dia DO mˆs!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.


    CREATE tt-param.
    ASSIGN tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame f-pg-imp rs-destino
           tt-param.data-exec       = today
           tt-param.hora-exec       = time.
    
    /*Alterado 14/02/2005 - tech1007 - Alterado o teste para verificar se a op‡Æo de RTF est  selecionada*/
    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2
         then assign tt-param.arquivo = INPUT FRAME f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:TEMP-DIRECTORY + c-programa-mg97 + ".tmp":U.
    /*Fim alteracao 14/02/2005*/

    /*:T Coloque aqui a/l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */
    ASSIGN  tt-param.cod_tip_produt_financ_ini  = INPUT FRAME f-pg-sel cod_tip_produt_financ_ini
            tt-param.cod_tip_produt_financ_fim  = INPUT FRAME f-pg-sel cod_tip_produt_financ_fim
            tt-param.cod_produt_financ_ini      = INPUT FRAME f-pg-sel cod_produt_financ_ini    
            tt-param.cod_produt_financ_fim      = INPUT FRAME f-pg-sel cod_produt_financ_fim    
            tt-param.cod_admdra_apf_ini         = INPUT FRAME f-pg-sel cod_admdra_apf_ini       
            tt-param.cod_admdra_apf_fim         = INPUT FRAME f-pg-sel cod_admdra_apf_fim       
            tt-param.cod_operac_financ_ini      = INPUT FRAME f-pg-sel cod_operac_financ_ini    
            tt-param.cod_operac_financ_fim      = INPUT FRAME f-pg-sel cod_operac_financ_fim    
            tt-param.data_movimento_ini         = INPUT FRAME f-pg-sel data_movimento_ini       
            tt-param.data_movimento_fim         = INPUT FRAME f-pg-sel data_movimento_fim
            tt-param.cod_banco_ini              = INPUT FRAME f-pg-sel cod_banco_ini
            tt-param.cod_banco_fim              = INPUT FRAME f-pg-sel cod_banco_fim
            tt-param.cod_emp_ini                = INPUT FRAME f-pg-sel cod_emp_ini
            tt-param.cod_emp_fim                = INPUT FRAME f-pg-sel cod_emp_fim.

   /*:T Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i prgfin/apl/apya510rp.p}
    
    {include/i-rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
/*     {include/i-rptrm.i} */
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina w-relat 
PROCEDURE pi-troca-pagina :
/*:T------------------------------------------------------------------------------
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


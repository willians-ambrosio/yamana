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
{include/i-prgvrs.i APY510 2.06.00.001}  /*** 010006 ***/
{utp\ut-glob.i}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Preprocessadores do Template de Relat�rio                            */
/* Obs: Retirar o valor do preprocessador para as p�ginas que n�o existirem  */
&GLOBAL-DEFINE PGIMP f-pg-imp
&GLOBAL-DEFINE PGSEL f-pg-sel


/* def new Global shared var l-implanta            as Logical          init no.                      */
/* def new global shared var l-rpc                 as Logical                          no-undo.      */
/* def new global shared var l-achou-prog          as Logical                          no-undo.      */
/* def new Global shared var c-seg-usuario         as Character        format "x(12)"  no-undo.      */
/* def new global shared var c-arquivo-log         as Character        format "x(60)"  no-undo.      */
/* def new global shared var i-num-ped-exec-rpw    as Integer                          no-undo.      */
/* def new global shared var i-pais-impto-usuario  as Integer          format ">>9"    no-undo.      */
/* Def New Global Shared Var v_idi_dtsul_instan    As Integer                          No-undo.      */
/* def new global shared var r-registro-atual      as Rowid                            no-undo.      */
/* def new global shared var h-rsocial             as Handle                           no-undo.      */
/* def var                   rw-log-exec           as rowid                                no-undo.  */
/* define var                i-template            as integer no-undo.                               */


define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    FIELD cod_tip_produt_financ_ini as character
    FIELD cod_tip_produt_financ_fim as character
    FIELD cod_produt_financ_ini     as character
    FIELD cod_produt_financ_fim     as character
    FIELD cod_admdra_apf_ini        as character
    FIELD cod_admdra_apf_fim        as character
    FIELD cod_operac_financ_ini     as character
    FIELD cod_operac_financ_fim     as character
    FIELD data_movimento_ini        AS DATE
    FIELD data_movimento_fim        AS DATE.


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


DEFINE NEW GLOBAL SHARED VARIABLE v_rec_plano_cta_ctbl AS RECID NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE v_rec_indic_econ     AS RECID NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-cla

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

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execu��o" 
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

DEFINE BUTTON BUTTON-3 
     IMAGE-UP FILE "prgfin/image/search.png":U
     LABEL "Button 3" 
     SIZE 4 BY 1.

DEFINE BUTTON BUTTON-4 
     IMAGE-UP FILE "prgfin/image/search.png":U
     LABEL "Button 4" 
     SIZE 4 BY 1.

DEFINE VARIABLE cod_admdra_apf_fim AS CHARACTER FORMAT "x(5)" INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88.

DEFINE VARIABLE cod_admdra_apf_ini AS CHARACTER FORMAT "x(5)" 
     LABEL "Administradora" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88.

DEFINE VARIABLE cod_operac_financ_fim AS CHARACTER FORMAT "x(10)" INITIAL "ZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 14.14 BY .88.

DEFINE VARIABLE cod_operac_financ_ini AS CHARACTER FORMAT "x(10)" 
     LABEL "Opera��o Financeira" 
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

DEFINE VARIABLE c_moeda AS CHARACTER FORMAT "X(256)":U 
     LABEL "Moeda Corrente" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c_plano_conta AS CHARACTER FORMAT "X(256)":U 
     LABEL "Plano de Contas" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE data_movimento_fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/2999 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE data_movimento_ini AS DATE FORMAT "99/99/9999":U INITIAL 12/31/17 
     LABEL "Data Movimenta��o" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

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

DEFINE IMAGE IMAGE-15
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-16
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-17
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-18
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-19
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-20
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

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "prgfin/image/search.png":U
     LABEL "Button 1" 
     SIZE 4 BY 1.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "prgfin/image/search.png":U
     LABEL "Button 2" 
     SIZE 4 BY 1.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-10
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


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-pg-cla
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.86 BY 10.31.

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu��o do relat�rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     im-pg-imp AT ROW 1.5 COL 17.72
     rt-folder AT ROW 2.5 COL 2
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-6 AT ROW 13.75 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     im-pg-sel AT ROW 1.5 COL 2 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME FRAME-A
     cod_tip_produt_financ_ini AT ROW 1.67 COL 34.43 RIGHT-ALIGNED HELP
          "C�digo Tipo Produto Financeiro" WIDGET-ID 4
          LABEL "Tipo Prod Financeiro" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     cod_tip_produt_financ_fim AT ROW 1.67 COL 44 COLON-ALIGNED HELP
          "C�digo Tipo Produto Financeiro" NO-LABEL WIDGET-ID 34 FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     cod_produt_financ_ini AT ROW 2.67 COL 34.43 RIGHT-ALIGNED HELP
          "C�digo do Produto Financeiro" WIDGET-ID 2
          LABEL "Produto Financeiro" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     cod_produt_financ_fim AT ROW 2.67 COL 44 COLON-ALIGNED HELP
          "C�digo do Produto Financeiro" NO-LABEL WIDGET-ID 32 FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     cod_admdra_apf_ini AT ROW 3.67 COL 34.43 RIGHT-ALIGNED HELP
          "C�digo Administradora" WIDGET-ID 6
          LABEL "Administradora" FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
     cod_admdra_apf_fim AT ROW 3.67 COL 44 COLON-ALIGNED HELP
          "C�digo Administradora" NO-LABEL WIDGET-ID 28 FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
     cod_operac_financ_ini AT ROW 4.67 COL 34.43 RIGHT-ALIGNED HELP
          "C�digo da Opera��o Financeira" WIDGET-ID 20
          LABEL "Opera��o Financeira" FORMAT "x(10)"
          VIEW-AS FILL-IN 
          SIZE 14.14 BY .88
     cod_operac_financ_fim AT ROW 4.67 COL 44 COLON-ALIGNED HELP
          "C�digo da Opera��o Financeira" NO-LABEL WIDGET-ID 30 FORMAT "x(10)"
          VIEW-AS FILL-IN 
          SIZE 14.14 BY .88
     data_movimento_ini AT ROW 5.67 COL 34.43 RIGHT-ALIGNED WIDGET-ID 22
          LABEL "Data Movimenta��o" FORMAT "99/99/9999":U
          VIEW-AS FILL-IN 
          SIZE 14 BY .88
     data_movimento_fim AT ROW 5.67 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 36 FORMAT "99/99/9999":U
          VIEW-AS FILL-IN 
          SIZE 14 BY .88
     BUTTON-1 AT ROW 6.58 COL 36 WIDGET-ID 44
     c_plano_conta AT ROW 6.67 COL 19.57 COLON-ALIGNED WIDGET-ID 40
          LABEL "Plano de Contas" FORMAT "X(256)":U
          VIEW-AS FILL-IN 
          SIZE 14 BY .88
     BUTTON-2 AT ROW 7.63 COL 36 WIDGET-ID 46
     c_moeda AT ROW 7.67 COL 19.57 COLON-ALIGNED WIDGET-ID 42
          LABEL "Moeda Corrente" FORMAT "X(256)":U
          VIEW-AS FILL-IN 
          SIZE 14 BY .88
     "Este relatorio ser� executado somente a partir de 31/12/2017." VIEW-AS TEXT
          SIZE 57.86 BY .67 AT ROW 9.25 COL 11 WIDGET-ID 38
          FGCOLOR 9 
     IMAGE-1 AT ROW 1.67 COL 36.57
     IMAGE-2 AT ROW 1.67 COL 41.72
     IMAGE-3 AT ROW 2.67 COL 36.57 WIDGET-ID 8
     IMAGE-4 AT ROW 2.67 COL 41.72 WIDGET-ID 10
     IMAGE-5 AT ROW 3.67 COL 36.57 WIDGET-ID 12
     IMAGE-6 AT ROW 3.67 COL 41.72 WIDGET-ID 14
     IMAGE-7 AT ROW 4.67 COL 36.57 WIDGET-ID 16
     IMAGE-8 AT ROW 4.67 COL 41.72 WIDGET-ID 18
     IMAGE-9 AT ROW 5.67 COL 36.57 WIDGET-ID 24
     IMAGE-10 AT ROW 5.67 COL 41.72 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62 WIDGET-ID 100.

DEFINE FRAME f-pg-par
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 75 BY 10.5.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.38 COL 3.29 HELP
          "Destino de Impress�o do Relat�rio" NO-LABEL
     bt-arquivo AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     bt-config-impr AT ROW 3.58 COL 43.29 HELP
          "Configura��o da impressora"
     c-arquivo AT ROW 3.63 COL 3.29 HELP
          "Nome do arquivo de destino do relat�rio" NO-LABEL
     rs-execucao AT ROW 5.75 COL 3 HELP
          "Modo de Execu��o" NO-LABEL
     text-destino AT ROW 1.63 COL 3.86 NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10.

DEFINE FRAME f-pg-sel
     cod_tip_produt_financ_ini AT ROW 1.67 COL 34.43 RIGHT-ALIGNED HELP
          "C�digo Tipo Produto Financeiro" WIDGET-ID 4
     cod_tip_produt_financ_fim AT ROW 1.67 COL 44 COLON-ALIGNED HELP
          "C�digo Tipo Produto Financeiro" NO-LABEL WIDGET-ID 34
     cod_produt_financ_ini AT ROW 2.67 COL 34.43 RIGHT-ALIGNED HELP
          "C�digo do Produto Financeiro" WIDGET-ID 2
     cod_produt_financ_fim AT ROW 2.67 COL 44 COLON-ALIGNED HELP
          "C�digo do Produto Financeiro" NO-LABEL WIDGET-ID 32
     cod_admdra_apf_ini AT ROW 3.67 COL 34.43 RIGHT-ALIGNED HELP
          "C�digo Administradora" WIDGET-ID 6
     cod_admdra_apf_fim AT ROW 3.67 COL 44 COLON-ALIGNED HELP
          "C�digo Administradora" NO-LABEL WIDGET-ID 28
     cod_operac_financ_ini AT ROW 4.67 COL 34.43 RIGHT-ALIGNED HELP
          "C�digo da Opera��o Financeira" WIDGET-ID 20
     cod_operac_financ_fim AT ROW 4.67 COL 44 COLON-ALIGNED HELP
          "C�digo da Opera��o Financeira" NO-LABEL WIDGET-ID 30
     data_movimento_ini AT ROW 5.67 COL 34.43 RIGHT-ALIGNED WIDGET-ID 22
     data_movimento_fim AT ROW 5.67 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     BUTTON-3 AT ROW 6.58 COL 36 WIDGET-ID 44
     c_plano_conta AT ROW 6.67 COL 19.57 COLON-ALIGNED WIDGET-ID 40
     BUTTON-4 AT ROW 7.63 COL 36 WIDGET-ID 46
     c_moeda AT ROW 7.67 COL 19.57 COLON-ALIGNED WIDGET-ID 42
     "Este relatorio ser� executado somente a partir de 31/12/2017." VIEW-AS TEXT
          SIZE 57.86 BY .67 AT ROW 9.25 COL 11 WIDGET-ID 38
          FGCOLOR 9 
     IMAGE-11 AT ROW 1.67 COL 36.57 WIDGET-ID 48
     IMAGE-12 AT ROW 1.67 COL 41.72 WIDGET-ID 50
     IMAGE-13 AT ROW 2.67 COL 36.57 WIDGET-ID 8
     IMAGE-14 AT ROW 2.67 COL 41.72 WIDGET-ID 10
     IMAGE-15 AT ROW 3.67 COL 36.57 WIDGET-ID 12
     IMAGE-16 AT ROW 3.67 COL 41.72 WIDGET-ID 14
     IMAGE-17 AT ROW 4.67 COL 36.57 WIDGET-ID 16
     IMAGE-18 AT ROW 4.67 COL 41.72 WIDGET-ID 18
     IMAGE-19 AT ROW 5.67 COL 36.57 WIDGET-ID 24
     IMAGE-20 AT ROW 5.67 COL 41.72 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62.


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
         TITLE              = "Relat�rio de Custo Excedido"
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
/* {utp/ut-glob.i} */
/*                             */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-relat
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-A:FRAME = FRAME f-relat:HANDLE.

/* SETTINGS FOR FRAME f-pg-cla
   NOT-VISIBLE FRAME-NAME                                               */
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
                "Execu��o".

/* SETTINGS FOR FRAME f-pg-par
   NOT-VISIBLE                                                          */
/* SETTINGS FOR FRAME f-pg-sel
   NOT-VISIBLE                                                          */
/* SETTINGS FOR FILL-IN cod_admdra_apf_ini IN FRAME f-pg-sel
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
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* SETTINGS FOR FILL-IN cod_admdra_apf_ini IN FRAME FRAME-A
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN cod_operac_financ_ini IN FRAME FRAME-A
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN cod_produt_financ_ini IN FRAME FRAME-A
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN cod_tip_produt_financ_ini IN FRAME FRAME-A
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN data_movimento_ini IN FRAME FRAME-A
   ALIGN-R                                                              */
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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON END-ERROR OF w-relat /* Relat�rio de Custo Excedido */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Relat�rio de Custo Excedido */
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

    /*
    IF rs-destino:SCREEN-VALUE IN FRAME f-pg-imp = "2" AND
       SUBSTRING (c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp, LENGTH (c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp) - 3,LENGTH (c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp)) <> ".xls" THEN DO:
        MESSAGE "Informe um arquivo Excel .xls"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        UNDO, RETURN.
    END.
    */

   do  on error undo, return no-apply:
       run pi-executar.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-A
&Scoped-define SELF-NAME c_moeda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c_moeda w-relat
ON F5 OF c_moeda IN FRAME FRAME-A /* Moeda Corrente */
DO:
  
    RUN prgint/utb/utb013ka.p.

    IF v_rec_indic_econ <> ? THEN DO:

       FIND indic_econ NO-LOCK 
             WHERE RECID(indic_econ) = v_rec_indic_econ NO-ERROR.

       ASSIGN c_moeda:SCREEN-VALUE IN FRAME {&FRAME-NAME} = indic_econ.cod_indic_econ WHEN AVAIL indic_econ.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c_moeda w-relat
ON MOUSE-SELECT-DBLCLICK OF c_moeda IN FRAME FRAME-A /* Moeda Corrente */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME c_moeda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c_moeda w-relat
ON F5 OF c_moeda IN FRAME f-pg-sel /* Moeda Corrente */
DO:
  
    RUN prgint/utb/utb013ka.p.

    IF v_rec_indic_econ <> ? THEN DO:

       FIND indic_econ NO-LOCK 
             WHERE RECID(indic_econ) = v_rec_indic_econ NO-ERROR.

       ASSIGN c_moeda:SCREEN-VALUE IN FRAME {&FRAME-NAME} = indic_econ.cod_indic_econ WHEN AVAIL indic_econ.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c_moeda w-relat
ON MOUSE-SELECT-DBLCLICK OF c_moeda IN FRAME f-pg-sel /* Moeda Corrente */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c_plano_conta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c_plano_conta w-relat
ON F5 OF c_plano_conta IN FRAME f-pg-sel /* Plano de Contas */
DO:
  
    RUN prgint/utb/utb080ka.p.

    IF v_rec_plano_cta_ctbl <> ? THEN DO:

       FIND plano_cta_ctbl NO-LOCK 
             WHERE RECID(plano_cta_ctbl) = v_rec_plano_cta_ctbl NO-ERROR.

       ASSIGN c_plano_conta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = plano_cta_ctbl.cod_plano_cta_ctbl WHEN AVAIL plano_cta_ctbl.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c_plano_conta w-relat
ON MOUSE-SELECT-DBLCLICK OF c_plano_conta IN FRAME f-pg-sel /* Plano de Contas */
DO:
  APPLY "f5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-A
&Scoped-define SELF-NAME c_plano_conta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c_plano_conta w-relat
ON F5 OF c_plano_conta IN FRAME FRAME-A /* Plano de Contas */
DO:
  
    RUN prgint/utb/utb080ka.p.

    IF v_rec_plano_cta_ctbl <> ? THEN DO:

       FIND plano_cta_ctbl NO-LOCK 
             WHERE RECID(plano_cta_ctbl) = v_rec_plano_cta_ctbl NO-ERROR.

       ASSIGN c_plano_conta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = plano_cta_ctbl.cod_plano_cta_ctbl WHEN AVAIL plano_cta_ctbl.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c_plano_conta w-relat
ON MOUSE-SELECT-DBLCLICK OF c_plano_conta IN FRAME FRAME-A /* Plano de Contas */
DO:
  APPLY "f5" TO SELF.
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


&Scoped-define FRAME-NAME f-pg-cla
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "APYA510" "5.06.00.001"}

/* inicializa��es do template de relat�rio */
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

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
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
  DISPLAY cod_tip_produt_financ_ini cod_tip_produt_financ_fim 
          cod_produt_financ_ini cod_produt_financ_fim cod_admdra_apf_ini 
          cod_admdra_apf_fim cod_operac_financ_ini cod_operac_financ_fim 
          data_movimento_ini data_movimento_fim c_plano_conta c_moeda 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-11 IMAGE-12 IMAGE-13 IMAGE-14 IMAGE-15 IMAGE-16 IMAGE-17 
         IMAGE-18 IMAGE-19 IMAGE-20 cod_tip_produt_financ_ini 
         cod_tip_produt_financ_fim cod_produt_financ_ini cod_produt_financ_fim 
         cod_admdra_apf_ini cod_admdra_apf_fim cod_operac_financ_ini 
         cod_operac_financ_fim data_movimento_ini data_movimento_fim BUTTON-3 
         c_plano_conta BUTTON-4 c_moeda 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY cod_tip_produt_financ_ini cod_tip_produt_financ_fim 
          cod_produt_financ_ini cod_produt_financ_fim cod_admdra_apf_ini 
          cod_admdra_apf_fim cod_operac_financ_ini cod_operac_financ_fim 
          data_movimento_ini data_movimento_fim c_plano_conta c_moeda 
      WITH FRAME FRAME-A IN WINDOW w-relat.
  ENABLE IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 
         IMAGE-9 IMAGE-10 cod_tip_produt_financ_ini cod_tip_produt_financ_fim 
         cod_produt_financ_ini cod_produt_financ_fim cod_admdra_apf_ini 
         cod_admdra_apf_fim cod_operac_financ_ini cod_operac_financ_fim 
         data_movimento_ini data_movimento_fim BUTTON-1 c_plano_conta BUTTON-2 
         c_moeda 
      WITH FRAME FRAME-A IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  {&OPEN-BROWSERS-IN-QUERY-f-pg-cla}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 rs-destino bt-arquivo bt-config-impr c-arquivo 
         rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-relat 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .


   VIEW FRAME f-pg-sel.
  /* Code placed here will execute AFTER standard behavior.    */

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
    /*14/02/2005 - tech1007 - Alterada condicao para n�o considerar mai o RTF como destino*/
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
     IF INPUT FRAME f-pg-sel data_movimento_ini < 12/31/2017 THEN DO:
       APPLY "MOUSE-SELECT-CLICK":U to im-pg-sel IN FRAME f-relat.  
       
       RUN utp/ut-msgs.p (INPUT "show":U, INPUT 142, input "Data Movimento Inicial" + "~~" + "Data Movimento").      
       APPLY "ENTRY":U to data_movimento_ini IN FRAME f-pg-sel.       
       return error.                                                 
    END.
    /*:T Aqui s�o gravados os campos da temp-table que ser� passada como par�metro
       para o programa RP.P */
    
   
    /*:T Coloque aqui a/l�gica de grava��o dos demais campos que devem ser passados
       como par�metros para o programa RP.P, atrav�s da temp-table tt-param */
    CREATE tt-param.
    ASSIGN tt-param.usuario   = c-seg-usuario
           tt-param.destino   = input frame f-pg-imp rs-destino
           tt-param.data-exec = today
           tt-param.hora-exec = TIME
            tt-param.cod_tip_produt_financ_ini  = INPUT FRAME f-pg-sel cod_tip_produt_financ_ini
            tt-param.cod_tip_produt_financ_fim  = INPUT FRAME f-pg-sel cod_tip_produt_financ_fim
            tt-param.cod_produt_financ_ini      = INPUT FRAME f-pg-sel cod_produt_financ_ini    
            tt-param.cod_produt_financ_fim      = INPUT FRAME f-pg-sel cod_produt_financ_fim    
            tt-param.cod_admdra_apf_ini         = INPUT FRAME f-pg-sel cod_admdra_apf_ini       
            tt-param.cod_admdra_apf_fim         = INPUT FRAME f-pg-sel cod_admdra_apf_fim       
            tt-param.cod_operac_financ_ini      = INPUT FRAME f-pg-sel cod_operac_financ_ini    
            tt-param.cod_operac_financ_fim      = INPUT FRAME f-pg-sel cod_operac_financ_fim    
            tt-param.data_movimento_ini         = INPUT FRAME f-pg-sel data_movimento_ini       
            tt-param.data_movimento_fim         = INPUT FRAME f-pg-sel data_movimento_fim.  

    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".txt".
                                                                                         
    
     /* Executar do programa RP.P que ir� criar o relat�rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i prgfin/apl/apya510rp.p}
    
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
  Purpose: Gerencia a Troca de P�gina (folder)   
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


&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
/*------------------------------------------------------------------------

    Programa: ESUT0004
    
    Objetivo: Consolida‡Æo Crit‚rios Dist CCusto (Interface)
    
    Autor...: Eugˆnio A. Marietti [EMA] - DSC
    
    Data....: Nov/2010
    
    Versao..: 5.05.00.000
    
  ----------------------------------------------------------------------*/
  
{include/i_bfems5.i}  
  
{include/i-prgvrs.i ESUT0004 5.06.00.000}

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/*:T Preprocessadores do Template de Relat¢rio                            */
/*:T Obs: Retirar o valor do preprocessador para as p ginas que nÆo existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA 
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG 
&GLOBAL-DEFINE PGIMP f-pg-imp

&GLOBAL-DEFINE RTF   NO
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

define temp-table tt-param no-undo
    field destino                   as integer
    field arquivo                   as char format "x(35)"
    field usuario                   as char format "x(12)"
    field data-exec                 as date
    field hora-exec                 as integer
    Field cod_ccusto                Like es_item_distrib_ccusto_dia.cod_ccusto                Extent 2
    Field cod_empresa               Like es_item_distrib_ccusto_dia.cod_empresa               Extent 2
    Field cod_estab                 Like es_item_distrib_ccusto_dia.cod_estab                 Extent 2
    Field cod_mapa_distrib_ccusto   Like es_item_distrib_ccusto_dia.cod_mapa_distrib_ccusto   Extent 2
    Field cod_plano_ccusto          Like es_item_distrib_ccusto_dia.cod_plano_ccusto          Extent 2
    Field cod_unid_negoc            Like es_item_distrib_ccusto_dia.cod_unid_negoc            Extent 2
    Field dat_criter_distrib_ccusto Like es_item_distrib_ccusto_dia.dat_criter_distrib_ccusto Extent 2
    Field consolidar                As Log
    Field gera_arq                  As Log
    Field dir_saida                 As Char                 
    Field rpw                       As Log Init No.

Def Temp-table tt-digita No-undo
    Field cod_usuario      Like usuar_mestre.cod_usuario
    Field cod_e_mail_local Like usuar_mestre.cod_e_mail_local
    Index pk_tt_digita     Is Primary Unique
          cod_usuario.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */

def new global shared var v_rec_usuar_mestre
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.

def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-rtf              as char    no-undo.
def var c-modelo-default   as char    no-undo.

/*15/02/2005 - tech1007 - Variavel definida para tratar se o programa est  rodando no WebEnabler*/
DEFINE SHARED VARIABLE hWenController AS HANDLE NO-UNDO.

DEF VAR h-acomp            AS   HANDLE       NO-UNDO.
DEF VAR rw-rowid           AS   ROWID        NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define BROWSE-NAME br_email

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-digita

/* Definitions for BROWSE br_email                                      */
&Scoped-define FIELDS-IN-QUERY-br_email tt-digita.cod_usuario tt-digita.cod_e_mail_local   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_email   
&Scoped-define SELF-NAME br_email
&Scoped-define QUERY-STRING-br_email FOR EACH tt-digita
&Scoped-define OPEN-QUERY-br_email OPEN QUERY {&SELF-NAME} FOR EACH tt-digita.
&Scoped-define TABLES-IN-QUERY-br_email tt-digita
&Scoped-define FIRST-TABLE-IN-QUERY-br_email tt-digita


/* Definitions for FRAME f-pg-par                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-pg-par ~
    ~{&OPEN-QUERY-br_email}

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
     SIZE 46.29 BY 2.79.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE BUTTON bt-dir-saida 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt_add_email 
     LABEL "Adicionar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt_del_email 
     LABEL "Remover" 
     SIZE 10 BY 1.

DEFINE VARIABLE c-dir-saida AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 54 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE rs_acao AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Apenas Verificar", 1,
"Consolidar", 2
     SIZE 70 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 6.21.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 1.5.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 1.63.

DEFINE VARIABLE tg-envia-email AS LOGICAL INITIAL no 
     LABEL "Enviar Email com os Crit‚rios Ainda nÆo Informados" 
     VIEW-AS TOGGLE-BOX
     SIZE 37.14 BY .83 NO-UNDO.

DEFINE VARIABLE tg-gera-arq AS LOGICAL INITIAL no 
     LABEL "Gerar Arquivo com as Informa‡äes Di rias" 
     VIEW-AS TOGGLE-BOX
     SIZE 35.14 BY .83 NO-UNDO.

DEFINE VARIABLE fi_cod_ccusto_fim AS CHARACTER FORMAT "x(11)" INITIAL "ZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .79.

DEFINE VARIABLE fi_cod_ccusto_ini AS CHARACTER FORMAT "x(11)" 
     LABEL "Centro Custo" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .79.

DEFINE VARIABLE fi_cod_empresa_fim AS CHARACTER FORMAT "x(3)" INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .79.

DEFINE VARIABLE fi_cod_empresa_ini AS CHARACTER FORMAT "x(3)" 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .79.

DEFINE VARIABLE fi_cod_estab_fim AS CHARACTER FORMAT "x(3)" INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .79.

DEFINE VARIABLE fi_cod_estab_ini AS CHARACTER FORMAT "x(3)" 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .79.

DEFINE VARIABLE fi_cod_mapa_distrib_ccusto_fim AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .79.

DEFINE VARIABLE fi_cod_mapa_distrib_ccusto_ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Mapa Dist CCusto" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .79.

DEFINE VARIABLE fi_cod_plano_ccusto_fim AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .79.

DEFINE VARIABLE fi_cod_plano_ccusto_ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Plano Centros Custo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .79.

DEFINE VARIABLE fi_cod_unid_negoc_fim AS CHARACTER FORMAT "x(3)" INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .79.

DEFINE VARIABLE fi_cod_unid_negoc_ini AS CHARACTER FORMAT "x(3)" 
     LABEL "Unid Neg¢cio" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .79.

DEFINE VARIABLE fi_dat_criter_distrib_ccusto_fim AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .79.

DEFINE VARIABLE fi_dat_criter_distrib_ccusto_ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Data" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .79.

DEFINE IMAGE IMAGE-19
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-20
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-27
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-28
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-37
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-38
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-39
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-40
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-41
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-42
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-43
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-44
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

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_email FOR 
      tt-digita SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_email
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_email w-relat _FREEFORM
  QUERY br_email DISPLAY
      tt-digita.cod_usuario
    tt-digita.cod_e_mail_local
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 71.43 BY 4.33
         FONT 1 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 1.63 COL 3.29 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     bt-config-impr AT ROW 2.71 COL 43.29 HELP
          "Configura‡Æo da impressora"
     bt-arquivo AT ROW 2.71 COL 43.29 HELP
          "Escolha do nome do arquivo"
     c-arquivo AT ROW 2.75 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 5.08 COL 2.86 HELP
          "Modo de Execu‡Æo" NO-LABEL
     text-destino AT ROW 1.04 COL 3.86 NO-LABEL
     text-modo AT ROW 4.33 COL 1.14 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 1.33 COL 2.14
     RECT-9 AT ROW 4.54 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10.5.

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     RECT-1 AT ROW 14.29 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     im-pg-imp AT ROW 1.5 COL 33.43
     im-pg-par AT ROW 1.5 COL 17.72
     im-pg-sel AT ROW 1.5 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-par
     rs_acao AT ROW 1.25 COL 4.14 NO-LABEL WIDGET-ID 20
     tg-envia-email AT ROW 2.67 COL 4 WIDGET-ID 16
     br_email AT ROW 3.63 COL 3.86 WIDGET-ID 200
     bt_add_email AT ROW 8.08 COL 4 WIDGET-ID 26
     bt_del_email AT ROW 8.08 COL 14.43 WIDGET-ID 28
     tg-gera-arq AT ROW 9.54 COL 3.86 WIDGET-ID 10
     bt-dir-saida AT ROW 10.42 COL 71.86 HELP
          "Escolha do nome do arquivo" WIDGET-ID 4
     c-dir-saida AT ROW 10.46 COL 17 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL WIDGET-ID 6
     "Diret¢rio de Sa¡da:" VIEW-AS TEXT
          SIZE 13 BY .54 AT ROW 10.63 COL 4 WIDGET-ID 12
     RECT-8 AT ROW 10 COL 2 WIDGET-ID 8
     RECT-10 AT ROW 3.08 COL 2 WIDGET-ID 14
     RECT-12 AT ROW 1 COL 2 WIDGET-ID 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.92
         SIZE 76.86 BY 10.79
         FONT 1 WIDGET-ID 100.

DEFINE FRAME f-pg-sel
     fi_cod_empresa_ini AT ROW 2.5 COL 17.86 COLON-ALIGNED HELP
          "C¢digo Empresa" WIDGET-ID 158
     fi_cod_empresa_fim AT ROW 2.5 COL 49.57 COLON-ALIGNED HELP
          "C¢digo Empresa" NO-LABEL WIDGET-ID 172
     fi_cod_estab_ini AT ROW 3.5 COL 17.86 COLON-ALIGNED HELP
          "C¢digo Estabelecimento" WIDGET-ID 160
     fi_cod_estab_fim AT ROW 3.5 COL 49.57 COLON-ALIGNED HELP
          "C¢digo Estabelecimento" NO-LABEL WIDGET-ID 174
     fi_cod_mapa_distrib_ccusto_ini AT ROW 4.5 COL 17.86 COLON-ALIGNED HELP
          "C¢digo Mapa Distribui‡Æo Centro Custo" WIDGET-ID 162
     fi_cod_mapa_distrib_ccusto_fim AT ROW 4.5 COL 49.57 COLON-ALIGNED HELP
          "C¢digo Mapa Distribui‡Æo Centro Custo" NO-LABEL WIDGET-ID 176
     fi_cod_unid_negoc_ini AT ROW 5.5 COL 17.86 COLON-ALIGNED HELP
          "C¢digo Unidade Neg¢cio" WIDGET-ID 166
     fi_cod_unid_negoc_fim AT ROW 5.5 COL 49.57 COLON-ALIGNED HELP
          "C¢digo Unidade Neg¢cio" NO-LABEL WIDGET-ID 180
     fi_cod_plano_ccusto_ini AT ROW 6.5 COL 17.86 COLON-ALIGNED HELP
          "C¢digo Plano Centros Custo" WIDGET-ID 164
     fi_cod_plano_ccusto_fim AT ROW 6.5 COL 49.57 COLON-ALIGNED HELP
          "C¢digo Plano Centros Custo" NO-LABEL WIDGET-ID 178
     fi_cod_ccusto_ini AT ROW 7.5 COL 17.86 COLON-ALIGNED HELP
          "C¢digo Centro Custo" WIDGET-ID 156
     fi_cod_ccusto_fim AT ROW 7.5 COL 49.57 COLON-ALIGNED HELP
          "C¢digo Centro Custo" NO-LABEL WIDGET-ID 170
     fi_dat_criter_distrib_ccusto_ini AT ROW 8.5 COL 17.86 COLON-ALIGNED HELP
          "Data Crit‚rio Distribui‡Æo Centro Custo" WIDGET-ID 168
     fi_dat_criter_distrib_ccusto_fim AT ROW 8.5 COL 49.57 COLON-ALIGNED HELP
          "Data Crit‚rio Distribui‡Æo Centro Custo" NO-LABEL WIDGET-ID 182
     IMAGE-3 AT ROW 2.5 COL 36.57 WIDGET-ID 10
     IMAGE-4 AT ROW 2.5 COL 45.14 WIDGET-ID 12
     IMAGE-27 AT ROW 5.5 COL 36.57 WIDGET-ID 104
     IMAGE-28 AT ROW 5.5 COL 45.14 WIDGET-ID 108
     IMAGE-19 AT ROW 3.5 COL 36.57 WIDGET-ID 106
     IMAGE-20 AT ROW 3.5 COL 45.14 WIDGET-ID 110
     IMAGE-37 AT ROW 4.5 COL 36.57 WIDGET-ID 144
     IMAGE-38 AT ROW 4.5 COL 45.14 WIDGET-ID 146
     IMAGE-39 AT ROW 8.5 COL 36.57 WIDGET-ID 188
     IMAGE-40 AT ROW 8.5 COL 45.14 WIDGET-ID 190
     IMAGE-41 AT ROW 6.5 COL 36.57 WIDGET-ID 184
     IMAGE-42 AT ROW 6.5 COL 45.14 WIDGET-ID 186
     IMAGE-43 AT ROW 7.5 COL 36.57 WIDGET-ID 192
     IMAGE-44 AT ROW 7.5 COL 45.14 WIDGET-ID 194
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.83
         SIZE 76.86 BY 10.88
         FONT 1.


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
         TITLE              = "Consolida‡Æo Crit‚rios Dist CCusto"
         HEIGHT             = 15
         WIDTH              = 81.14
         MAX-HEIGHT         = 30.21
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 30.21
         VIRTUAL-WIDTH      = 182.86
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
/* BROWSE-TAB br_email tg-envia-email f-pg-par */
/* SETTINGS FOR BUTTON bt-dir-saida IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt_add_email IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt_del_email IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR c-dir-saida IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FILL-IN fi_cod_empresa_fim IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_cod_empresa_ini IN FRAME f-pg-sel
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-relat)
THEN w-relat:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_email
/* Query rebuild information for BROWSE br_email
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-digita.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br_email */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-imp
/* Query rebuild information for FRAME f-pg-imp
     _Query            is NOT OPENED
*/  /* FRAME f-pg-imp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-par
/* Query rebuild information for FRAME f-pg-par
     _Query            is NOT OPENED
*/  /* FRAME f-pg-par */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-sel
/* Query rebuild information for FRAME f-pg-sel
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON END-ERROR OF w-relat /* Consolida‡Æo Crit‚rios Dist CCusto */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Consolida‡Æo Crit‚rios Dist CCusto */
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


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME bt-dir-saida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dir-saida w-relat
ON CHOOSE OF bt-dir-saida IN FRAME f-pg-par
DO:

    Def Var c-dir  As Char No-undo.
    Def Var l-canc As Log  No-undo.

    Run utp/ut-dir.p (Input "Escolha um diret¢rio",
                      Output c-dir,
                      Output l-canc).

    If Not l-canc Then 
        Assign c-dir-saida:Screen-value In Frame f-pg-par = c-dir.

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
&Scoped-define SELF-NAME bt_add_email
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_add_email w-relat
ON CHOOSE OF bt_add_email IN FRAME f-pg-par /* Adicionar */
DO:

    run prgtec/sec/sec000ka.p /*prg_sea_usuar_mestre*/.

    if  v_rec_usuar_mestre <> ? then do:

        find usuar_mestre where recid(usuar_mestre) = v_rec_usuar_mestre no-lock no-error.

        If Not Can-find(First tt-digita 
                        Where tt-digita.cod_usuario = usuar_mestre.cod_usuario) Then Do:

            Create tt-digita.

            Assign tt-digita.cod_usuario      = usuar_mestre.cod_usuario
                   tt-digita.cod_e_mail_local = usuar_mestre.cod_e_mail_local.

            {&OPEN-QUERY-{&BROWSE-NAME}}

        End.
    
    end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_del_email
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_del_email w-relat
ON CHOOSE OF bt_del_email IN FRAME f-pg-par /* Remover */
DO:
  
    Def Var i_current_row As Int No-undo.

    Assign i_current_row = Current-result-row('br_email').

    If Avail tt-digita Then Do:

        Delete tt-digita.

    End.

    {&OPEN-QUERY-{&BROWSE-NAME}}

    br_email:Query:Reposition-to-row(i_current_row) In Frame f-pg-par.

    If Avail tt-digita Then
        br_email:Select-focused-row() In Frame f-pg-par.

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
/*Alterado 15/02/2005 - tech1007 - Evento alterado para correto funcionamento dos novos widgets
  utilizados para a funcionalidade de RTF*/
do  with frame f-pg-imp:
    case self:screen-value:
        when "1" then do:
            assign c-arquivo:sensitive    = no
                   bt-arquivo:visible     = no
                   bt-config-impr:visible = YES
                   /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para
                     verificar se o RTF est  ativo*/
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = NO
                   l-habilitaRtf:SCREEN-VALUE IN FRAME f-pg-imp = "No"
                   l-habilitaRtf = NO
                   &endif
                   /*Fim alteracao 17/02/2005*/
                   .
        end.
        when "2" then do:
            assign c-arquivo:sensitive     = yes
                   bt-arquivo:visible      = yes
                   bt-config-impr:visible  = NO
                   /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para
                     verificar se o RTF est  ativo*/
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = YES
                   &endif
                   /*Fim alteracao 17/02/2005*/
                   .
        end.
        when "3" then do:
            assign c-arquivo:sensitive     = no
                   bt-arquivo:visible      = no
                   bt-config-impr:visible  = no
                   /*Alterado 17/02/2005 - tech1007 - Realizado teste de preprocessador para
                     verificar se o RTF est  ativo*/
                   &IF "{&RTF}":U = "YES":U &THEN
                   l-habilitaRtf:sensitive  = YES
                   &endif
                   /*Fim alteracao 17/02/2005*/
                   .
            /*Alterado 15/02/2005 - tech1007 - Teste para funcionar corretamente no WebEnabler*/
            &IF "{&RTF}":U = "YES":U &THEN
            IF VALID-HANDLE(hWenController) THEN DO:
                ASSIGN l-habilitaRtf:sensitive  = NO
                       l-habilitaRtf:SCREEN-VALUE IN FRAME f-pg-imp = "No"
                       l-habilitaRtf = NO.
            END.
            &endif
            /*Fim alteracao 15/02/2005*/
        end.
    end case.
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


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME tg-envia-email
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-envia-email w-relat
ON VALUE-CHANGED OF tg-envia-email IN FRAME f-pg-par /* Enviar Email com os Crit‚rios Ainda nÆo Informados */
DO:
  
    Assign bt_add_email:Sensitive = tg-envia-email:Checked
           bt_del_email:Sensitive = tg-envia-email:Checked.

    If Not tg-envia-email:Checked Then Do:

        Empty Temp-table tt-digita.

        {&OPEN-QUERY-{&BROWSE-NAME}}

    End.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-gera-arq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-gera-arq w-relat
ON VALUE-CHANGED OF tg-gera-arq IN FRAME f-pg-par /* Gerar Arquivo com as Informa‡äes Di rias */
DO:
  
    Assign c-dir-saida :Sensitive = tg-gera-arq:Checked
           bt-dir-saida:Sensitive = tg-gera-arq:Checked. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define BROWSE-NAME br_email
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.


/*{utp/ut9000.i "ESUT0004" "5.05.00.000"}*/

Assign c-programa-mg97 = 'ESUT0004'.

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

    Assign fi_dat_criter_distrib_ccusto_ini = Date(Month(Today), 1, Year(Today))
           fi_dat_criter_distrib_ccusto_fim = Today.   

    Assign fi_cod_empresa_ini = v_cod_empres_usuar
           fi_cod_empresa_fim = v_cod_empres_usuar. 

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
  ENABLE im-pg-imp im-pg-par im-pg-sel bt-executar bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY fi_cod_empresa_ini fi_cod_empresa_fim fi_cod_estab_ini 
          fi_cod_estab_fim fi_cod_mapa_distrib_ccusto_ini 
          fi_cod_mapa_distrib_ccusto_fim fi_cod_unid_negoc_ini 
          fi_cod_unid_negoc_fim fi_cod_plano_ccusto_ini fi_cod_plano_ccusto_fim 
          fi_cod_ccusto_ini fi_cod_ccusto_fim fi_dat_criter_distrib_ccusto_ini 
          fi_dat_criter_distrib_ccusto_fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-3 IMAGE-4 IMAGE-27 IMAGE-28 IMAGE-19 IMAGE-20 IMAGE-37 IMAGE-38 
         IMAGE-39 IMAGE-40 IMAGE-41 IMAGE-42 IMAGE-43 IMAGE-44 fi_cod_estab_ini 
         fi_cod_estab_fim fi_cod_mapa_distrib_ccusto_ini 
         fi_cod_mapa_distrib_ccusto_fim fi_cod_unid_negoc_ini 
         fi_cod_unid_negoc_fim fi_cod_plano_ccusto_ini fi_cod_plano_ccusto_fim 
         fi_cod_ccusto_ini fi_cod_ccusto_fim fi_dat_criter_distrib_ccusto_ini 
         fi_dat_criter_distrib_ccusto_fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs_acao tg-envia-email tg-gera-arq c-dir-saida 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE RECT-8 RECT-10 RECT-12 rs_acao tg-envia-email br_email tg-gera-arq 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
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

Def Var i-cont    As Int No-undo.
Def Var i-lst-sit As Int No-undo Extent 5. 

do on error undo, return error on stop  undo, return error:

    Empty Temp-table tt-param.
    Empty Temp-table tt-raw-digita.

    {include/i-rpexa.i}

    /*14/02/2005 - tech1007 - Alterada condicao para nÆo considerar mais o RTF como destino*/
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

    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame f-pg-imp rs-destino
           tt-param.data-exec       = today
           tt-param.hora-exec       = TIME.
    
    /*Alterado 14/02/2005 - tech1007 - Alterado o teste para verificar se a op‡Æo de RTF est  selecionada*/
    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".txt":U.
    /*Fim alteracao 14/02/2005*/

    /*:T Coloque aqui a/l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */

    Assign tt-param.cod_ccusto               [1] = Input Frame f-pg-sel fi_cod_ccusto_ini               
           tt-param.cod_empresa              [1] = Input Frame f-pg-sel fi_cod_empresa_ini              
           tt-param.cod_estab                [1] = Input Frame f-pg-sel fi_cod_estab_ini                
           tt-param.cod_mapa_distrib_ccusto  [1] = Input Frame f-pg-sel fi_cod_mapa_distrib_ccusto_ini  
           tt-param.cod_plano_ccusto         [1] = Input Frame f-pg-sel fi_cod_plano_ccusto_ini         
           tt-param.cod_unid_negoc           [1] = Input Frame f-pg-sel fi_cod_unid_negoc_ini           
           tt-param.dat_criter_distrib_ccusto[1] = Input Frame f-pg-sel fi_dat_criter_distrib_ccusto_ini.

    Assign tt-param.cod_ccusto               [2] = Input Frame f-pg-sel fi_cod_ccusto_fim               
           tt-param.cod_empresa              [2] = Input Frame f-pg-sel fi_cod_empresa_fim              
           tt-param.cod_estab                [2] = Input Frame f-pg-sel fi_cod_estab_fim                
           tt-param.cod_mapa_distrib_ccusto  [2] = Input Frame f-pg-sel fi_cod_mapa_distrib_ccusto_fim  
           tt-param.cod_plano_ccusto         [2] = Input Frame f-pg-sel fi_cod_plano_ccusto_fim         
           tt-param.cod_unid_negoc           [2] = Input Frame f-pg-sel fi_cod_unid_negoc_fim           
           tt-param.dat_criter_distrib_ccusto[2] = Input Frame f-pg-sel fi_dat_criter_distrib_ccusto_fim.

    Assign tt-param.consolidar      = (Input Frame f-pg-par rs_acao = 2).

    Assign tt-param.gera_arq        = Input Frame f-pg-par tg-gera-arq
           tt-param.dir_saida       = Input Frame f-pg-par c-dir-saida.

    If R-index(tt-param.dir_saida, '\') <> Length(tt-param.dir_saida) Then
        Assign tt-param.dir_saida = tt-param.dir_saida + '\'. 

    For Each tt-digita :

        Create tt-raw-digita.

        Raw-transfer tt-digita To tt-raw-digita.raw-digita.

    End.

    If Input Frame f-pg-imp rs-execucao = 2 Then Do:

        Run utp/ut-msgs.p (Input "show":U, 
                           Input 15825, 
                           Input "Execu‡Æo Batch~~" +
                                 "Quando executado em modo Batch o programa nÆo poder  gerar o arquivo com as Informa‡äes Di rias "    +
                                 "e a faixa de datas ser  calculada pelo programa, considerando desde o primeiro dia do mˆs corrente " + 
                                 "at‚ o dia em que o programa estiver sendo executado.").

        Assign tt-param.rpw       = Yes
               tt-param.gera_arq  = No.

    End.
    
    /*:T Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    Run pi_cria_dwb_rpt.

    /*
    {include/i-rprun.i esp/esut0004rp.p}
    */

    {include/i-rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
    {include/i-rptrm.i}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_cria_dwb_rpt w-relat 
PROCEDURE pi_cria_dwb_rpt :
Def Var i_num_dwb_order As Int No-undo. 
    Def Var v_num_ped_exec  As Int No-undo.
    Def Var i_num_param     As Int No-undo.

    Find First dwb_rpt_param Exclusive-lock
         Where dwb_rpt_param.cod_dwb_program = "esut0004rp"         
           And dwb_rpt_param.cod_dwb_user    = v_cod_usuar_corren No-error.

    If Not Avail dwb_rpt_param Then Do:

        Create dwb_rpt_param.

        Assign dwb_rpt_param.cod_dwb_program = "esut0004rp"
               dwb_rpt_param.cod_dwb_user    = v_cod_usuar_corren.

    End.
    
    Assign dwb_rpt_param.cod_dwb_parameters       = ''    /* Opcoes de Classificacao */                       
           dwb_rpt_param.cod_dwb_file             = tt-param.arquivo                                       
           dwb_rpt_param.cod_dwb_output           = Entry(tt-param.destino, "Impressora,Arquivo,Terminal") 
           dwb_rpt_param.cod_dwb_order            = ''    /* Opcoes de Classificacao */                       
           dwb_rpt_param.log_dwb_print_parameters = No    /* Imprime Parƒmetros */                            
           dwb_rpt_param.qtd_dwb_line             = 0     /* Qtde Linhas por Pagina */                        
           dwb_rpt_param.cod_livre_2              = ''                                                     
           dwb_rpt_param.nom_dwb_print_file       = ''                                                     
           dwb_rpt_param.cod_livre_1              = ''
           dwb_rpt_param.ind_dwb_run_mode         = String(tt-param.rpw, "Batch,On-Line").

    Assign dwb_rpt_param.nom_dwb_printer          = ''
           dwb_rpt_param.cod_dwb_print_layout     = ''
           dwb_rpt_param.nom_dwb_print_file       = ''.

    If tt-param.destino = 1 And Num-entries(tt-param.arquivo, ':') = 2 Then Do:

        Assign dwb_rpt_param.nom_dwb_printer          = Entry(1, tt-param.arquivo, ':')                        
               dwb_rpt_param.cod_dwb_print_layout     = Entry(2, tt-param.arquivo, ':')
               dwb_rpt_param.nom_dwb_print_file       = tt-param.arquivo.

    End.

    For Each dwb_rpt_select Exclusive-lock
       Where dwb_rpt_select.cod_dwb_program = dwb_rpt_param.cod_dwb_program
         And dwb_rpt_select.cod_dwb_user    = dwb_rpt_param.cod_dwb_user :

        Delete dwb_rpt_select.

    End.

    Assign i_num_dwb_order = 0.

    Do i_num_param = 1 To 10 :

        Assign i_num_dwb_order = i_num_dwb_order + 10.

        Find First dwb_rpt_select Exclusive-lock
             Where dwb_rpt_select.cod_dwb_program = dwb_rpt_param.cod_dwb_program
               And dwb_rpt_select.cod_dwb_user    = dwb_rpt_param.cod_dwb_user
               And dwb_rpt_select.num_dwb_order   = i_num_dwb_order No-error.

        If Not Avail dwb_rpt_select Then Do:

            Create dwb_rpt_select.

            Assign dwb_rpt_select.cod_dwb_program = dwb_rpt_param.cod_dwb_program
                   dwb_rpt_select.cod_dwb_user    = dwb_rpt_param.cod_dwb_user
                   dwb_rpt_select.num_dwb_order   = i_num_dwb_order.

        End.

        Case i_num_param :

            When 1 Then Do:

                Assign dwb_rpt_select.cod_dwb_field   = 'cod_ccusto'
                       dwb_rpt_select.cod_dwb_initial = tt-param.cod_ccusto[1]
                       dwb_rpt_select.cod_dwb_final   = tt-param.cod_ccusto[2]
                       dwb_rpt_select.log_dwb_rule    = Yes /* Regra/Excecao */.

            End.

            When 2 Then Do:
    
                Assign dwb_rpt_select.cod_dwb_field   = 'cod_empresa'
                       dwb_rpt_select.cod_dwb_initial = tt-param.cod_empresa[1]
                       dwb_rpt_select.cod_dwb_final   = tt-param.cod_empresa[2]
                       dwb_rpt_select.log_dwb_rule    = Yes /* Regra/Excecao */.
    
            End.

            When 3 Then Do:
    
                Assign dwb_rpt_select.cod_dwb_field   = 'cod_estab'
                       dwb_rpt_select.cod_dwb_initial = tt-param.cod_estab[1]
                       dwb_rpt_select.cod_dwb_final   = tt-param.cod_estab[2]
                       dwb_rpt_select.log_dwb_rule    = Yes /* Regra/Excecao */.
    
            End.

            When 4 Then Do:
    
                Assign dwb_rpt_select.cod_dwb_field   = 'cod_mapa_distrib_ccusto'
                       dwb_rpt_select.cod_dwb_initial = tt-param.cod_mapa_distrib_ccusto[1]
                       dwb_rpt_select.cod_dwb_final   = tt-param.cod_mapa_distrib_ccusto[2]
                       dwb_rpt_select.log_dwb_rule    = Yes /* Regra/Excecao */.
    
            End.

            When 5 Then Do:
    
                Assign dwb_rpt_select.cod_dwb_field   = 'cod_plano_ccusto'
                       dwb_rpt_select.cod_dwb_initial = tt-param.cod_plano_ccusto[1]
                       dwb_rpt_select.cod_dwb_final   = tt-param.cod_plano_ccusto[2]
                       dwb_rpt_select.log_dwb_rule    = Yes /* Regra/Excecao */.
    
            End.
    
            When 6 Then Do:
    
                Assign dwb_rpt_select.cod_dwb_field   = 'cod_unid_negoc'
                       dwb_rpt_select.cod_dwb_initial = tt-param.cod_unid_negoc[1]
                       dwb_rpt_select.cod_dwb_final   = tt-param.cod_unid_negoc[2]
                       dwb_rpt_select.log_dwb_rule    = Yes /* Regra/Excecao */.
    
            End.
    
            When 7 Then Do:
    
                Assign dwb_rpt_select.cod_dwb_field   = 'dat_criter_distrib_ccusto'
                       dwb_rpt_select.cod_dwb_initial = String(tt-param.dat_criter_distrib_ccusto[1])
                       dwb_rpt_select.cod_dwb_final   = String(tt-param.dat_criter_distrib_ccusto[2])
                       dwb_rpt_select.log_dwb_rule    = Yes /* Regra/Excecao */.
    
            End.

            When 8 Then Do:
    
                Assign dwb_rpt_select.cod_dwb_field   = 'consolidar'
                       dwb_rpt_select.cod_dwb_initial = String(tt-param.consolidar)
                       dwb_rpt_select.cod_dwb_final   = String(tt-param.consolidar)
                       dwb_rpt_select.log_dwb_rule    = Yes /* Regra/Excecao */.
    
            End.

            When 9 Then Do:
    
                Assign dwb_rpt_select.cod_dwb_field   = 'gera_arq'
                       dwb_rpt_select.cod_dwb_initial = String(tt-param.gera_arq)
                       dwb_rpt_select.cod_dwb_final   = String(tt-param.gera_arq)
                       dwb_rpt_select.log_dwb_rule    = Yes /* Regra/Excecao */.
    
            End.

            When 10 Then Do:
    
                Assign dwb_rpt_select.cod_dwb_field   = 'dir_saida'
                       dwb_rpt_select.cod_dwb_initial = tt-param.dir_saida
                       dwb_rpt_select.cod_dwb_final   = tt-param.dir_saida
                       dwb_rpt_select.log_dwb_rule    = Yes /* Regra/Excecao */.
    
            End.

        End Case.

    End.

    For Each tt-digita No-lock :
    
        Assign i_num_dwb_order = i_num_dwb_order + 10.

        Find First dwb_rpt_select Exclusive-lock
             Where dwb_rpt_select.cod_dwb_program = dwb_rpt_param.cod_dwb_program
               And dwb_rpt_select.cod_dwb_user    = dwb_rpt_param.cod_dwb_user
               And dwb_rpt_select.num_dwb_order   = i_num_dwb_order No-error.

        If Not Avail dwb_rpt_select Then Do:

            Create dwb_rpt_select.

            Assign dwb_rpt_select.cod_dwb_program = dwb_rpt_param.cod_dwb_program
                   dwb_rpt_select.cod_dwb_user    = dwb_rpt_param.cod_dwb_user
                   dwb_rpt_select.num_dwb_order   = i_num_dwb_order.

        End.
    
        Assign dwb_rpt_select.cod_dwb_field   = 'cod_e_mail_local'
               dwb_rpt_select.cod_dwb_initial = tt-digita.cod_e_mail_local
               dwb_rpt_select.cod_dwb_final   = tt-digita.cod_e_mail_local
               dwb_rpt_select.log_dwb_rule    = Yes /* Regra/Excecao */.
    
    End.

    If tt-param.rpw Then Do:

        run prgtec/btb/btb911za.p (Input "esut0004rp",
                                   Input "5.06.00.000",
                                   Input 40,
                                   Input recid(dwb_rpt_param),
                                   output v_num_ped_exec) /*prg_fnc_criac_ped_exec*/.

        if  v_num_ped_exec <> 0 then                     
          run utp/ut-msgs.p (input "show":U, input 4169, input string(v_num_ped_exec)).                      

    End.
    Else Do:

        Run esp/esut0004rp.p.

    End.

End Procedure.

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


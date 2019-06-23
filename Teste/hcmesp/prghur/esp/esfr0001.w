&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          dthrpmg          PROGRESS
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
{include/buffers_RH.i}

{include/i-prgvrs.i esfr0001 2.08.00.000}

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
&GLOBAL-DEFINE PGDIG f-pg-dig
&GLOBAL-DEFINE PGIMP f-pg-imp
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

/*{prghur/fpp/fp4140tt.i}*/

DEFINE temp-table tt-param
    field v_cdn_empres_usuar      like param_empres_rh.cdn_empresa
    field v_cod_unid_lotac_ini    like unid_lotac.cod_unid_lotac
    field v_cod_unid_lotac_fim    like unid_lotac.cod_unid_lotac
    field i-es-ini                like rh_estab.cdn_estab
    field i-es-fim                like rh_estab.cdn_estab
    field i-fc-ini                like funcionario.cdn_funcionario
    field i-fc-fim                like funcionario.cdn_funcionario
    field v_dat_valid             as date format "99/99/9999"
    field v_log_expande_estrut    as log
    field v_num_salta_pg          as integer
    field v_num_quebra            as integer
    field v_num_faixa             as integer
    field destino                 as integer
    field arquivo                 as char format "x(35)"
    field usuario                 as char format "x(12)"
    field data-exec               as date format "99/99/9999"
    field hora-exec               as integer
    field classifica              as integer
    field desc-classifica         as char format "x(40)"
    field cc-codigo-ini           like func_ptoelet.cod_rh_ccusto
    field cc-codigo-fim           like func_ptoelet.cod_rh_ccusto
    field v_log_mensal            as logical
    field v_log_horista           as logical
    field v_log_semanal           as logical
    field v_log_quinzenal         as logical
    field v_log_tarefa            as logical
    field v_log_diarista          as logical
    field v_log_usa_ponto         as logical
    field v_log_ass_resp          as logical
    field v_num_tip_aces_usuar    as integer format "9"
    FIELD v_dt_admissao           AS DATE
    FIELD c-mes                   AS INTE
    FIELD c-ano                   AS INTE
    FIELD tb-parametro            AS LOGICAL
    FIELD v_dt_pt_fim             AS DATE 
    FIELD v_dt_pt_ini             AS DATE 
    FIELD l-digita                AS LOGICAL
    .

define temp-table tt-digita no-undo
       field v_cdn_empres_usuar like empresa.ep-codigo INIT "1"
       field v_cdn_estab        LIKE funcionario.cdn_estab     
       field v_cdn_funcionario  LIKE funcionario.cdn_funcionario     
       FIELD v_num_dv_func      LIKE funcionario.num_digito_verfdor_func    
       FIELD v_nom_pessoa       LIKE funcionario.nom_pessoa_fisic
       field cdn_plano_lotac    LIKE plano_lotac.cdn_plano_lotac
       field v_cod_unid_lotac   LIKE unid_lotac.cod_unid_lotac
       FIELD v_cs_codigo        LIKE funcionario.cdn_categ_sal
    index id is primary 
          v_cdn_empres_usuar
          v_cdn_estab
          v_cdn_funcionario
          v_num_dv_func.

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.

{prghur/fpp/fp9200.i7}
{prghur/fpp/fp9200.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-pg-cla
&Scoped-define BROWSE-NAME br-digita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-digita

/* Definitions for BROWSE br-digita                                     */
&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.v_cdn_estab tt-digita.v_cdn_funcionario tt-digita.v_num_dv_func tt-digita.v_nom_pessoa   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-digita.v_cdn_estab   tt-digita.v_cdn_funcionario   
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
          "Por Estabelecimento / Matr¡cula", 1,
"Por Estabelecimento / Nome", 2,
"Por Estabelecimento / C.Custo / Data Programa‡Æo / Matr¡cula", 3,
"Por Estabelecimento / C.Custo / Data Programa‡Æo / Nome", 4,
"Por Estabelecimento / C.Custo / Matr¡cula", 5,
"Por Estabelecimento / C.Custo / Nome", 6,
"Por C.Custo / Matr¡cula", 7,
"Por C.Custo / Nome", 8,
"Por Estabelecimento / Unidade de Lota‡Æo / Matr¡cula", 9,
"Por Estabelecimento / Unidade de Lota‡Æo / Nome", 10
     SIZE 65 BY 9.25 NO-UNDO.

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

DEFINE VARIABLE c-ano AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-mes AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Mˆs/Ano de Referˆncia" 
     VIEW-AS FILL-IN 
     SIZE 4.29 BY .88 NO-UNDO.

DEFINE VARIABLE v_dat_refer AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Refer Hist Lota‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 9.43 BY .88 NO-UNDO.

DEFINE VARIABLE v_dt_admitido AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Admitidos a Partir de" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE v_num_opcao_quebra AS INTEGER FORMAT "z9":U INITIAL 0 
     LABEL "Quebra" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE v_num_opcao_salta AS INTEGER FORMAT "z9":U INITIAL 0 
     LABEL "Salta P gina" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE l-digita AS LOGICAL INITIAL no 
     LABEL "Digitar Funcion rios" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .83 NO-UNDO.

DEFINE VARIABLE v_log_expande_estrut AS LOGICAL INITIAL yes 
     LABEL "Expande Estrutura" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.57 BY .83 NO-UNDO.

DEFINE VARIABLE cc-codigo-fim AS CHARACTER FORMAT "x(08)" INITIAL "99999999" 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88.

DEFINE VARIABLE cc-codigo-ini AS CHARACTER FORMAT "x(08)" 
     LABEL "Centro Custo":R15 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88.

DEFINE VARIABLE v_cdn_estab_fim AS CHARACTER FORMAT "x(5)":U INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_estab_ini AS CHARACTER FORMAT "x(5)":U INITIAL "" 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE v_cod_unid_lotac_fim AS CHARACTER FORMAT "x(11)":U INITIAL "ZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE v_cod_unid_lotac_ini AS CHARACTER FORMAT "x(11)":U 
     LABEL "Unidade Lota‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE v_dt_pt_fim AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE v_dt_pt_ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Data F‚rias" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE v_num_opcao_faixa AS INTEGER FORMAT "z9":U INITIAL 0 
     LABEL "N¡vel Unidade Lota‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-46
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-47
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-54
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-55
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-56
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-86
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-87
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 38.86 BY 4.25.

DEFINE VARIABLE v_log_diarista AS LOGICAL INITIAL yes 
     LABEL "Diarista" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .71 NO-UNDO.

DEFINE VARIABLE v_log_horista AS LOGICAL INITIAL yes 
     LABEL "Horista" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.86 BY .71 NO-UNDO.

DEFINE VARIABLE v_log_mensal AS LOGICAL INITIAL yes 
     LABEL "Mensal" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.29 BY .71 NO-UNDO.

DEFINE VARIABLE v_log_quinzenal AS LOGICAL INITIAL yes 
     LABEL "Quinzenal" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.86 BY .71 NO-UNDO.

DEFINE VARIABLE v_log_semanal AS LOGICAL INITIAL yes 
     LABEL "Semanal" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .71 NO-UNDO.

DEFINE VARIABLE v_log_tarefa AS LOGICAL INITIAL yes 
     LABEL "Tarefa" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .71 NO-UNDO.

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
      tt-digita.v_cdn_estab
        tt-digita.v_cdn_funcionario
        tt-digita.v_num_dv_func
        tt-digita.v_nom_pessoa
ENABLE
    tt-digita.v_cdn_estab
    tt-digita.v_cdn_funcionario
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 76.57 BY 9
         BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder AT ROW 2.5 COL 2
     im-pg-cla AT ROW 1.5 COL 17.86
     im-pg-dig AT ROW 1.5 COL 49.29
     im-pg-imp AT ROW 1.5 COL 65
     im-pg-par AT ROW 1.5 COL 33.57
     im-pg-sel AT ROW 1.5 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

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
         AT COL 3 ROW 3
         SIZE 73.72 BY 10.

DEFINE FRAME f-pg-cla
     rs-classif AT ROW 1.5 COL 8 HELP
          "Classifica‡Æo para emissÆo do relat¢rio" NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.86 BY 10.31.

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
         SIZE 76.86 BY 10.15.

DEFINE FRAME f-pg-sel
     v_cdn_estab_ini AT ROW 1 COL 26.72 COLON-ALIGNED
     v_cdn_estab_fim AT ROW 1 COL 52 COLON-ALIGNED NO-LABEL
     v_num_opcao_faixa AT ROW 2 COL 26.72 COLON-ALIGNED
     v_cod_unid_lotac_ini AT ROW 3 COL 26.72 COLON-ALIGNED
     v_cod_unid_lotac_fim AT ROW 3 COL 52 COLON-ALIGNED NO-LABEL
     cc-codigo-ini AT ROW 4 COL 26.72 COLON-ALIGNED
     cc-codigo-fim AT ROW 4 COL 52 COLON-ALIGNED NO-LABEL
     v_dt_pt_ini AT ROW 5 COL 26.72 COLON-ALIGNED
     v_dt_pt_fim AT ROW 5 COL 52 COLON-ALIGNED NO-LABEL
     v_log_mensal AT ROW 8 COL 26 HELP
          "Mensal"
     v_log_quinzenal AT ROW 8 COL 41 HELP
          "Quinzenal"
     v_log_horista AT ROW 9 COL 26 HELP
          "Horista"
     v_log_tarefa AT ROW 9 COL 41 HELP
          "Tarefa"
     v_log_semanal AT ROW 10 COL 26 HELP
          "Semanal"
     v_log_diarista AT ROW 10 COL 41 HELP
          "Diarista"
     IMAGE-1 AT ROW 1 COL 43
     IMAGE-2 AT ROW 1 COL 50
     IMAGE-46 AT ROW 2 COL 43
     IMAGE-47 AT ROW 4 COL 43
     IMAGE-54 AT ROW 4 COL 50.14
     IMAGE-55 AT ROW 3 COL 43
     IMAGE-56 AT ROW 3 COL 50
     IMAGE-86 AT ROW 5 COL 43
     IMAGE-87 AT ROW 5 COL 50
     RECT-19 AT ROW 7 COL 20
     " Categoria Salarial" VIEW-AS TEXT
          SIZE 20 BY .67 AT ROW 6.75 COL 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62.

DEFINE FRAME f-pg-par
     v_dat_refer AT ROW 2 COL 25 COLON-ALIGNED HELP
          "Data de Referˆncia para Lota‡Æo"
     v_log_expande_estrut AT ROW 3 COL 27 HELP
          "Expande Estrutura"
     v_num_opcao_salta AT ROW 4 COL 25 COLON-ALIGNED HELP
          "Salta P gina"
     v_num_opcao_quebra AT ROW 5 COL 25 COLON-ALIGNED HELP
          "Quebra"
     c-mes AT ROW 6 COL 25 COLON-ALIGNED HELP
          "Mˆs de Referˆncia"
     c-ano AT ROW 6 COL 29.57 COLON-ALIGNED HELP
          "Ano de Referˆncia" NO-LABEL
     v_dt_admitido AT ROW 7 COL 25 COLON-ALIGNED
     l-digita AT ROW 8.75 COL 27
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 75 BY 10.


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
         TITLE              = "Gera Programa‡Æo de F‚rias"
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
                                                                        */
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
/* SETTINGS FOR FILL-IN c-ano IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-mes IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v_dt_admitido IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX v_log_expande_estrut IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v_num_opcao_quebra IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v_num_opcao_salta IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FILL-IN v_dt_pt_fim IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v_dt_pt_ini IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v_num_opcao_faixa IN FRAME f-pg-sel
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
ON END-ERROR OF w-relat /* Gera Programa‡Æo de F‚rias */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Gera Programa‡Æo de F‚rias */
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
        display tt-digita.v_cdn_estab 
                tt-digita.v_cdn_funcionario     
                tt-digita.v_num_dv_func
                tt-digita.v_nom_pessoa
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
   /* trigger para inicializar campos da temp table de digita‡Æo */
/*   if  br-digita:new-row in frame f-pg-dig then do:
       assign tt-digita.nome:screen-value in browse br-digita = string(today, "99/99/9999").
   end.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita w-relat
ON ROW-LEAVE OF br-digita IN FRAME f-pg-dig
/*DO:
 *    if  br-digita:new-row in frame f-pg-dig then do transaction on error undo, return no-apply:
 *        create tt-digita.
 *        assign tt-digita.v_cdn_empres_usuar = v_cdn_empres_usuar
 *               input browse br-digita tt-digita.v_cdn_estab
 *               input browse br-digita tt-digita.v_cdn_funcionario
 *               tt-digita.v_nom_pessoa     = tt-digita.v_nom_pessoa:screen-value in browse br-digita
 *               tt-digita.cdn_plano_lotac  = v_cdn_plano_lotac
 *               tt-digita.v_num_dv_func = tt-digita.v_num_dv_func:screen-value in browse br-digita.
 *    end.        
 *    else do transaction:
 *        assign tt-digita.v_cdn_empres_usuar     = v_cdn_empres_usuar
 *               tt-digita.v_nom_pessoa     = tt-digita.v_nom_pessoa:screen-value in browse br-digita
 *               tt-digita.cdn_plano_lotac  = v_cdn_plano_lotac
 *               tt-digita.v_num_dv_func = func_unid_lotac_plano.cod_unid_lotac.
 *    end.
 *  
 *    if  br-digita:create-result-list-entry() in frame f-pg-dig then.       
 *    
 * END.*/

DO:
    /*  aqui que a grava‡Æo da linha da temp-table ² efetivada.
       Por‚m as valida‡äes dos registros devem ser feitas na procedure pi-executar,
       no local indicado pelo coment rio */
    
    if br-digita:NEW-ROW in frame f-pg-dig then 
    do transaction on error undo, return no-apply:
        create tt-digita.
        assign tt-digita.v_cdn_empres_usuar = v_cdn_empres_usuar
               input browse br-digita tt-digita.v_cdn_estab
               input browse br-digita tt-digita.v_cdn_funcionario
               input browse br-digita tt-digita.v_nom_pessoa
               input browse br-digita tt-digita.v_num_dv_func.
        br-digita:CREATE-RESULT-LIST-ENTRY() in frame f-pg-dig.
    end.
    else do transaction on error undo, return no-apply:
        assign tt-digita.v_cdn_empres_usuar = v_cdn_empres_usuar
               input browse br-digita tt-digita.v_cdn_estab
               input browse br-digita tt-digita.v_cdn_funcionario
               input browse br-digita tt-digita.v_nom_pessoa
               input browse br-digita tt-digita.v_num_dv_func.
    end.
        
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
  apply 'entry' to tt-digita.v_cdn_estab in browse br-digita. 
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


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-inserir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inserir w-relat
ON CHOOSE OF bt-inserir IN FRAME f-pg-dig /* Inserir */
DO:
    enable bt-retirar bt-salvar bt-alterar with frame f-pg-dig.
           
    if  num-results('br-digita') > 0 then do:
        if  br-digita:insert-row('after') in frame f-pg-dig then.
    end.
    else do transaction:
        create tt-digita.
        open query br-digita for each tt-digita.
        apply 'entry' to tt-digita.v_cdn_estab in browse br-digita. 
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


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME im-pg-cla
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-cla w-relat
ON MOUSE-SELECT-CLICK OF im-pg-cla IN FRAME f-relat
DO:
    run pi-troca-pagina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  if input frame f-pg-cla rs-classif = 09 or 
     input frame f-pg-cla rs-classif = 10 then do:
     enable v_log_expande_estrut
            v_num_opcao_salta 
            v_num_opcao_quebra with frame f-pg-par.
  end.
  else do:
  /*   assign v_log_expande_estrut:checked in frame f-pg-par = no.
            v_num_opcao_salta:screen-value in frame f-pg-par = "0" 
            v_num_opcao_quebra:screen-value in frame f-pg-par = "0".*/
     disable v_log_expande_estrut
             v_num_opcao_salta 
             v_num_opcao_quebra with frame f-pg-par.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME im-pg-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-sel w-relat
ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-relat
DO:
  run pi-troca-pagina.
  if input frame f-pg-cla rs-classif = 09 or 
     input frame f-pg-cla rs-classif = 10 then do:
     enable v_num_opcao_faixa with frame f-pg-sel.
  end.
  else do:
     /*assign v_num_opcao_faixa:screen-value in frame f-pg-sel = "0".*/
     disable v_num_opcao_faixa with frame f-pg-sel.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME l-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-digita w-relat
ON VALUE-CHANGED OF l-digita IN FRAME f-pg-par /* Digitar Funcion rios */
DO:
    run pi-desabilita-folder.
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


on F5, MOUSE-SELECT-DBLCLICK OF tt-digita.v_cdn_estab in browse br-digita do:
   run pi-zoom-estab. 
end.

on LEAVE OF tt-digita.v_cdn_estab in browse br-digita do:
  assign v_cdn_estab_par = input browse br-digita tt-digita.v_cdn_estab.
  if v_cdn_estab_par = "" then
     assign v_cdn_estab_par = tt-digita.v_cdn_estab:screen-value in browse br-digita.
end.

on F5, MOUSE-SELECT-DBLCLICK OF tt-digita.v_cdn_funcionario in browse br-digita do:
if tt-digita.v_cdn_estab:screen-value in browse br-digita <> "0" then do:
    run pi-funcionario.
end.
else do: 
    run pi-terceiro.
end.
end.

on LEAVE OF tt-digita.v_cdn_funcionario in browse br-digita do:
  run pi-leave-func.
end.

find first param_empres_rh where
           param_empres_rh.cdn_empresa = "1" /*v_cdn_empres_usuar*/ no-lock no-error.
if avail param_empres_rh then 
   assign c-mes = param_empres_rh.num_mes_refer_calc_efetd 
          c-ano = param_empres_rh.num_ano_refer_calc_efetd.

ASSIGN v_dat_refer = TODAY
       v_dt_pt_fim = TODAY
       v_dt_pt_ini = TODAY.

ASSIGN v_dat_refer = DATE(month(v_dat_refer),01,YEAR(v_dat_refer))
       v_dat_refer = v_dat_refer + 32
       v_dat_refer = DATE(MONTH(v_dat_refer),01,YEAR(v_dat_refer)) - 1.

ASSIGN /*v_log_expande_estrut = YES*/
       v_num_opcao_faixa    = 1
       v_num_opcao_quebra   = 1
       v_num_opcao_salta    = 1
       .

/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "esfr0001" "2.06.00.001"}

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
  
    If v_num_tip_aces_usuar = 1 then do:
       {prghur/fpp/fp9200.i2}
    end.

/*    {prghur/fpp/fp9200.i9}*/
    
    find first param_empres_rh where
               param_empres_rh.cdn_empresa = v_cdn_empres_usuar no-lock no-error.
    if avail param_empres_rh then 
       assign c-mes:screen-value in frame f-pg-par = string(param_empres_rh.num_mes_refer_calc_efetd)
              c-ano:screen-value in frame f-pg-par = string(param_empres_rh.num_ano_refer_calc_efetd).
    
    assign im-pg-dig:sensitive in frame f-relat = no
           wh-label-dig:fgcolor                 = 7.

/*    if i-cod-termo:load-mouse-pointer("image/lupa.cur") in frame f-pg-par then.*/
    {include/i-rpmbl.i im-pg-cla}

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
  ENABLE bt-executar bt-cancelar bt-ajuda im-pg-cla im-pg-dig im-pg-imp 
         im-pg-par im-pg-sel 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY v_cdn_estab_ini v_cdn_estab_fim v_num_opcao_faixa v_cod_unid_lotac_ini 
          v_cod_unid_lotac_fim cc-codigo-ini cc-codigo-fim v_dt_pt_ini 
          v_dt_pt_fim v_log_mensal v_log_quinzenal v_log_horista v_log_tarefa 
          v_log_semanal v_log_diarista 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE v_cdn_estab_ini v_cdn_estab_fim v_cod_unid_lotac_ini 
         v_cod_unid_lotac_fim cc-codigo-ini cc-codigo-fim v_log_mensal 
         v_log_quinzenal v_log_horista v_log_tarefa v_log_semanal 
         v_log_diarista IMAGE-1 IMAGE-2 IMAGE-46 IMAGE-47 IMAGE-54 IMAGE-55 
         IMAGE-56 IMAGE-86 IMAGE-87 RECT-19 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  ENABLE rs-classif 
      WITH FRAME f-pg-cla IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-cla}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE rs-destino bt-arquivo bt-config-impr c-arquivo rs-execucao RECT-7 
         RECT-9 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY v_dat_refer v_log_expande_estrut v_num_opcao_salta v_num_opcao_quebra 
          c-mes c-ano v_dt_admitido l-digita 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE v_dat_refer l-digita 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-desabilita-folder w-relat 
PROCEDURE pi-desabilita-folder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
if input frame f-pg-par l-digita = NO then do:
     assign im-pg-dig:sensitive in frame f-relat = no
            wh-label-dig:fgcolor                 = 7
            im-pg-sel:sensitive in frame f-relat = yes 
            wh-label-sel:fgcolor                 = ?.
end.
else do:
     assign im-pg-dig:sensitive in frame f-relat = yes
            wh-label-dig:fgcolor                 = ?
            im-pg-sel:sensitive in frame f-relat = no
            wh-label-sel:fgcolor                 = 7.
     apply "mouse-select-click" to im-pg-dig in frame f-relat.
end.   

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
def var r_rowid as rowid no-undo.

do on error undo, return error
   on stop  undo, return error:     

  {include/i-rpexa.i}
    
  If input frame f-pg-par l-digita = YES then do:
    for each tt-digita:
        assign r_rowid = rowid(tt-digita).
             
        find first rh_estab no-lock 
             where rh_estab.cdn_empresa = v_cdn_empres_usuar 
               AND rh_estab.cdn_estab   = tt-digita.v_cdn_estab no-error.
        if not avail rh_estab then do:
           {utp/ut-table.i dthrpyc rh_estab 1}
           run utp/ut-msgs.p (input "show", input 56, input return-value).
           reposition br-digita to rowid r_rowid.
           apply 'MOUSE-SELECT-CLICK' to im-pg-dig in frame f-relat.
           apply 'entry' to tt-digita.v_cdn_estab in browse br-digita.
           return error.
        end.
             
        find first funcionario no-lock 
             where funcionario.cdn_empresa = v_cdn_empres_usuar 
               AND funcionario.cdn_estab = tt-digita.v_cdn_estab 
               AND funcionario.cdn_funcionario = tt-digita.v_cdn_funcionario no-error.
        If not avail funcionario then do:
           {utp/ut-table.i dthrpyc funcionario 1}
           run utp/ut-msgs.p (input "show", input 56, input return-value).
           reposition br-digita to rowid r_rowid.
           apply 'MOUSE-SELECT-CLICK' to im-pg-dig in frame f-relat.
           apply 'entry' to tt-digita.v_cdn_funcionario in browse br-digita.
           return error.
        end.
        ELSE ASSIGN tt-digita.v_cs_codigo     = funcionario.cdn_categ_sal
                    tt-digita.cdn_plano_lotac = funcionario.cdn_plano_lotac.
             
        find first b-tt-digita 
             WHERE b-tt-digita.v_cdn_estab = tt-digita.v_cdn_estab 
               AND b-tt-digita.v_cdn_funcionario = tt-digita.v_cdn_funcionario 
               AND rowid(b-tt-digita) <> rowid(tt-digita) no-lock no-error.
        if avail b-tt-digita then do:
           run utp/ut-msgs.p (input "show", input 108, input "").
           reposition br-digita to rowid r_rowid.
           apply 'MOUSE-SELECT-CLICK' to im-pg-dig in frame f-relat.
           apply 'ENTRY' to tt-digita.v_cdn_funcionario in browse br-digita.                                 
           return error.         
        end.

        find first func_unid_lotac_plano no-lock 
             where func_unid_lotac_plano.cdn_empresa          = v_cdn_empres_usuar 
               and func_unid_lotac_plano.cdn_estab            = v_cdn_estab_par 
               and func_unid_lotac_plano.cdn_funcionario      = tt-digita.v_cdn_funcionario 
               and func_unid_lotac_plano.dat_inic_lotac_func <= input frame f-pg-par v_dat_refer 
               and func_unid_lotac_plano.dat_fim_lotac_func  >= input frame f-pg-par v_dat_refer no-error.

        if avail func_unid_lotac_plano then do:
           find funcionario 
                where funcionario.cdn_empresa     = func_unid_lotac_plano.cdn_empresa 
                  and funcionario.cdn_estab       = func_unid_lotac_plano.cdn_estab 
                  and funcionario.cdn_funcionario = func_unid_lotac_plano.cdn_funcionario no-error.
            If avail funcionario 
            then assign v_cdn_plano_lotac = func_unid_lotac_plano.cdn_plano_lotac
                        tt-digita.v_cod_unid_lotac = func_unid_lotac_plano.cod_unid_lotac.
            ELSE assign v_cdn_plano_lotac = 0
                        tt-digita.v_cod_unid_lotac = "".
        end.
        ELSE ASSIGN v_cdn_plano_lotac = 0
                    tt-digita.v_cod_unid_lotac = "".
     end.
  end.

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
       
  If input frame f-pg-par l-digita = NO then do:

    if input frame f-pg-sel v_cdn_estab_fim < input frame f-pg-sel v_cdn_estab_ini THEN DO:
       run utp/ut-msgs.p (input "show", input 1404, input "").
       apply "MOUSE-SELECT-CLICK":U to im-pg-sel in frame f-relat.
       apply "ENTRY":U to v_cdn_estab_ini in frame f-pg-sel.
       return error.
    end.
    
    if input frame f-pg-sel v_cod_unid_lotac_fim < input frame f-pg-sel v_cod_unid_lotac_ini then do:
       message "Unidade de Lota‡Æo Final Menor que Inicial" 
           view-as alert-box ERROR TITLE "Atencao !".
       apply "mouse-select-click" to im-pg-sel in frame f-relat.
       apply 'entry' to v_cod_unid_lotac_fim in frame f-pg-sel.  
       return error.
    end.    

    if input frame f-pg-sel cc-codigo-fim < input frame f-pg-sel cc-codigo-ini THEN DO:
       run utp/ut-msgs.p (input "show", input 18469, input "").
       apply "MOUSE-SELECT-CLICK":U to im-pg-sel in frame f-relat.
       apply "ENTRY":U to cc-codigo-ini in frame f-pg-sel.
       return error.
    end.

    if input frame f-pg-sel v_dt_pt_fim < input frame f-pg-sel v_dt_pt_ini THEN DO:
       run utp/ut-msgs.p (input "show", input 198, input "").
       apply "MOUSE-SELECT-CLICK":U to im-pg-sel in frame f-relat.
       apply "ENTRY":U to v_dt_pt_ini in frame f-pg-sel.
       return error.
    end.
  END.

  IF input frame f-pg-par c-mes >= 13 OR
     input frame f-pg-par c-mes <=  0 THEN DO:
     run utp/ut-msgs.p (input "show", input 18335, input "").
     apply "MOUSE-SELECT-CLICK":U to im-pg-par in frame f-relat.
     apply "ENTRY":U to c-mes in frame f-pg-par.
     return error.
  end.
    
  IF input frame f-pg-par c-ano <= 0 THEN DO:
     run utp/ut-msgs.p (input "show", input 15204, input "").
     apply "MOUSE-SELECT-CLICK":U to im-pg-par in frame f-relat.
     apply "ENTRY":U to c-ano in frame f-pg-par.
     return error.
  end.

  create tt-param.
  assign tt-param.usuario              = c-seg-usuario
         tt-param.destino              = input frame f-pg-imp rs-destino
         tt-param.data-exec            = today
         tt-param.hora-exec            = time
         tt-param.classifica           = input frame f-pg-cla rs-classif
         tt-param.desc-classifica      = entry((tt-param.classifica - 1) * 2 + 1, 
                                         rs-classif:radio-buttons in frame f-pg-cla).
      if tt-param.destino = 1 
      then assign tt-param.arquivo = "".
      else if  tt-param.destino = 2 
           then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
           else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".

   ASSIGN tt-param.c-ano                = INPUT FRAME f-pg-par c-ano
          tt-param.c-mes                = INPUT FRAME f-pg-par c-mes
          tt-param.i-es-fim             = INPUT FRAME f-pg-sel v_cdn_estab_fim
          tt-param.i-es-ini             = INPUT FRAME f-pg-sel v_cdn_estab_ini
          tt-param.v_cod_unid_lotac_fim = INPUT FRAME f-pg-sel v_cod_unid_lotac_fim
          tt-param.v_cod_unid_lotac_ini = INPUT FRAME f-pg-sel v_cod_unid_lotac_ini
          tt-param.v_dt_pt_fim          = INPUT FRAME f-pg-sel v_dt_pt_fim
          tt-param.v_dt_pt_ini          = INPUT FRAME f-pg-sel v_dt_pt_ini
          tt-param.v_dt_admissao        = INPUT FRAME f-pg-par v_dt_admitido
          tt-param.v_log_mensal         = input frame f-pg-sel v_log_mensal
          tt-param.v_log_horista        = input frame f-pg-sel v_log_horista
          tt-param.v_log_semanal        = input frame f-pg-sel v_log_semanal
          tt-param.v_log_quinzenal      = input frame f-pg-sel v_log_quinzenal
          tt-param.v_log_tarefa         = input frame f-pg-sel v_log_tarefa
          tt-param.v_log_diarista       = input frame f-pg-sel v_log_diarista
          tt-param.cc-codigo-ini        = input frame f-pg-sel cc-codigo-ini
          tt-param.cc-codigo-fim        = input frame f-pg-sel cc-codigo-fim           
          tt-param.l-digita             = input frame f-pg-par l-digita
          tt-param.v_num_tip_aces_usuar = v_num_tip_aces_usuar
          tt-param.v_cdn_empres_usuar   = v_cdn_empres_usuar
          tt-param.i-fc-ini             = 0
          tt-param.i-fc-fim             = 999999999
          .
    

IF tt-param.classifica = 09 OR tt-param.classifica = 10 
THEN ASSIGN tt-param.v_dat_valid          = INPUT FRAME f-pg-par v_dat_refer
            tt-param.v_log_expande_estrut = INPUT FRAME f-pg-par v_log_expande_estrut
            tt-param.v_num_faixa          = input frame f-pg-sel v_num_opcao_faixa
            tt-param.v_num_quebra         = input frame f-pg-par v_num_opcao_quebra
            tt-param.v_num_salta_pg       = input frame f-pg-par v_num_opcao_salta
            .

ELSE ASSIGN tt-param.v_dat_valid          = ?
            tt-param.v_log_expande_estrut = NO
            tt-param.v_num_faixa          = 0
            tt-param.v_num_quebra         = 0
            tt-param.v_num_salta_pg       = 0
            .

    {include/i-rpexb.i}

    if  session:set-wait-state("general":U) then.

    {include/i-rprun.i esp/esfr0001rp.p}
    {include/i-rpexc.i}

    if  session:set-wait-state("":U) then.
    
    {include/i-rptrm.i}
end.
                                      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-funcionario w-relat 
PROCEDURE pi-funcionario :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{include/zoomvar.i &prog-zoom=object/sopy/zoom/z03py085.w
                   &campo=tt-digita.v_cdn_funcionario
                   &campozoom=cdn_funcionario
                   &campo2=tt-digita.v_nom_pessoa
                   &campozoom2=nom_pessoa_fisic
                   &campo3=tt-digita.v_num_dv_func
                   &campozoom3=num_digito_verfdor_func
                   &browse=br-digita
                   &parametros="run pi-seta-inicial in wh-pesquisa 
                   (1,integer(tt-digita.v_cdn_estab:screen-value in browse br-digita)").}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-leave-func w-relat 
PROCEDURE pi-leave-func :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   if tt-digita.v_cdn_estab:screen-value in browse br-digita <> "0" then do:
       find funcionario 
           where funcionario.cdn_empresa     = v_cdn_empres_usuar
             and funcionario.cdn_estab       = tt-digita.v_cdn_estab:screen-value in browse br-digita 
             and funcionario.cdn_funcionario = tt-digita.v_cdn_funcionario:input-value in browse br-digita 
                 NO-LOCK NO-ERROR.
  
      If avail funcionario 
      then assign tt-digita.v_nom_pessoa:screen-value in browse br-digita = funcionario.nom_pessoa_fisic
                  tt-digita.v_num_dv_func:screen-value in browse br-digita = string(funcionario.num_digito_verfdor_func).
      ELSE assign tt-digita.v_nom_pessoa:screen-value in browse br-digita = ""
                  tt-digita.v_num_dv_func:screen-value in browse br-digita = "".
   end.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-terceiro w-relat 
PROCEDURE pi-terceiro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{include/zoomvar.i &prog-zoom=object/sotm/zoom/z01tm027.w
                   &campo=tt-digita.v_cdn_funcionario
                   &campo2=tt-digita.v_nom_pessoa
                   &campozoom=cdn_terc_ptoelet
                   &campozoom2=nom_pessoa_fisic
                   &browse=br-digita}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-zoom-estab w-relat 
PROCEDURE pi-zoom-estab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 if v_num_tip_aces_usuar = 1 then do:
     {include/zoomvar.i &prog-zoom=object/sopy/zoom/z02py298.w
                        &campo=tt-digita.v_cdn_estab
                        &campozoom=cdn_estab
                        &browse=br-digita}
  end.
  else do:
     {include/zoomvar.i &prog-zoom=object/sopy/zoom/z03py298.w
                        &campo=tt-digita.v_cdn_estab
                        &campozoom=cdn_estab
                        &browse=br-digita
                        &ExcludeVar=yes}
  end.
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


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
{include/i-prgvrs.i YMAC0001 1.02.00.037 } /*** 010037 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i YMAC0001 MFP}
&ENDIF

/*------------------------------------------------------------------------

  File: 

  Descritpion: 

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

/* Preprocessadores do Template de Relat¢rio                            */
/* Obs: Retirar o valor do preprocessador para as p ginas que nÆo existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG f-pg-dig
&GLOBAL-DEFINE PGIMP f-pg-imp

/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */
{include/i_dbvers.i}
{prghur/esp/ymac0001tt.i}

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.

/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var i-dia              as int     no-undo.
def var v_des_arq          as char    no-undo.
def var h-acomp            as handle  no-undo.
def var r-tt-digita        as rowid   no-undo.
def var v_nom_arquivo      as char    no-undo.
def var v-msg-estab        as char    no-undo.
def var v-msg-func         as char    no-undo.

{prghur/fpp/fp9200.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define BROWSE-NAME br-digita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-digita

/* Definitions for BROWSE br-digita                                     */
&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.es-codigo tt-digita.c-nome-es tt-digita.fc-codigo tt-digita.c-nome-fc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-digita.es-codigo tt-digita.fc-codigo   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-digita tt-digita
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-digita tt-digita
&Scoped-define SELF-NAME br-digita
&Scoped-define QUERY-STRING-br-digita FOR EACH tt-digita
&Scoped-define OPEN-QUERY-br-digita OPEN QUERY br-digita FOR EACH tt-digita.
&Scoped-define TABLES-IN-QUERY-br-digita tt-digita
&Scoped-define FIRST-TABLE-IN-QUERY-br-digita tt-digita


/* Definitions for BROWSE br-digita-centr                               */
&Scoped-define FIELDS-IN-QUERY-br-digita-centr tt-digita.es-codigo tt-digita.c-nome-es tt-digita.fc-centr tt-digita.c-nome-fc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita-centr tt-digita.es-codigo tt-digita.fc-centr   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-digita-centr tt-digita
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-digita-centr tt-digita
&Scoped-define SELF-NAME br-digita-centr
&Scoped-define QUERY-STRING-br-digita-centr FOR EACH tt-digita
&Scoped-define OPEN-QUERY-br-digita-centr OPEN QUERY br-digita-centr FOR EACH tt-digita.
&Scoped-define TABLES-IN-QUERY-br-digita-centr tt-digita
&Scoped-define FIRST-TABLE-IN-QUERY-br-digita-centr tt-digita


/* Definitions for FRAME f-pg-dig                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-pg-dig ~
    ~{&OPEN-QUERY-br-digita}~
    ~{&OPEN-QUERY-br-digita-centr}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-digita br-digita-centr bt-inserir ~
bt-inserir-2 bt-recuperar-2 bt-recuperar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-alterar 
     LABEL "Alterar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-alterar-2 
     LABEL "Alterar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-inserir 
     LABEL "Inserir" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-inserir-2 
     LABEL "Inserir" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-recuperar 
     LABEL "Recuperar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-recuperar-2 
     LABEL "Recuperar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-retirar 
     LABEL "Retirar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-retirar-2  NO-CONVERT-3D-COLORS
     LABEL "Retirar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-salvar 
     LABEL "Salvar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-salvar-2 
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

DEFINE RECTANGLE RECT-100
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE tb-parametro AS LOGICAL INITIAL no 
     LABEL "Imprimir P ginas de Parƒmetros" 
     VIEW-AS TOGGLE-BOX
     SIZE 33.72 BY .88 NO-UNDO.

DEFINE BUTTON bt-arquivo-entrada 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo-entrada AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE dt-credito AS DATE FORMAT "99/99/9999":U 
     LABEL "Cr‚dito Pensionista" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN-10 AS CHARACTER FORMAT "X(256)":U INITIAL "Op‡Æo" 
      VIEW-AS TEXT 
     SIZE 6 BY .67 NO-UNDO.

DEFINE VARIABLE text-entrada AS CHARACTER FORMAT "X(256)":U INITIAL "Arquivo PensÆo Aliment¡cia" 
      VIEW-AS TEXT 
     SIZE 19.86 BY .63 NO-UNDO.

DEFINE VARIABLE v_cdn_agencia AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Agˆncia Empresa" 
     VIEW-AS FILL-IN 
     SIZE 6.57 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_banco AS INTEGER FORMAT "zz9":U INITIAL 0 
     LABEL "Banco" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_estab_centr LIKE rh_estab.cdn_estab
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE v_cod_docto AS CHARACTER FORMAT "X(20)":U 
     LABEL "Documento" 
     VIEW-AS FILL-IN 
     SIZE 21.57 BY .88 NO-UNDO.

DEFINE VARIABLE v_cod_dv AS CHARACTER FORMAT "X(1)":U 
     VIEW-AS FILL-IN 
     SIZE 2.57 BY .88 NO-UNDO.

DEFINE VARIABLE v_cod_dv_agencia AS CHARACTER FORMAT "X(1)":U 
     VIEW-AS FILL-IN 
     SIZE 2.57 BY .88 NO-UNDO.

DEFINE VARIABLE v_des_texto AS CHARACTER FORMAT "X(256)":U INITIAL "Forma Lan‡amento" 
      VIEW-AS TEXT 
     SIZE 13.72 BY .58 NO-UNDO.

DEFINE VARIABLE v_num_conta AS DECIMAL FORMAT "999999999999":U INITIAL 0 
     LABEL "Conta Corrente Empresa" 
     VIEW-AS FILL-IN 
     SIZE 13.57 BY .88 NO-UNDO.

DEFINE VARIABLE v_num_convenio AS CHARACTER FORMAT "X(20)":U 
     LABEL "Convˆnio" 
     VIEW-AS FILL-IN 
     SIZE 22 BY .88 NO-UNDO.

DEFINE VARIABLE i-ind-selec AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Sele‡Æo", 1,
"Digita‡Æo", 2,
"Digita‡Æo + Sele‡Æo", 3
     SIZE 19.86 BY 2.25 NO-UNDO.

DEFINE RECTANGLE RECT-102
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25.86 BY 2.75.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25.86 BY 4.21.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.5.

DEFINE VARIABLE v_log_cartao AS LOGICAL INITIAL no 
     LABEL "CartÆo Sal rio" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.43 BY .71 NO-UNDO.

DEFINE VARIABLE v_log_cheque AS LOGICAL INITIAL no 
     LABEL "Cheque Pagto/Admin" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.14 BY .71 NO-UNDO.

DEFINE VARIABLE v_log_conta_poup AS LOGICAL INITIAL no 
     LABEL "Cr‚dito Conta Poupan‡a" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.43 BY .83 NO-UNDO.

DEFINE VARIABLE v_log_cred_conta AS LOGICAL INITIAL yes 
     LABEL "Cr‚dito Conta Corrente" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.43 BY .71 NO-UNDO.

DEFINE VARIABLE v_log_doc AS LOGICAL INITIAL no 
     LABEL "Documento Cr‚dito" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.86 BY .71 NO-UNDO.

DEFINE VARIABLE v_log_envia_reg_b AS LOGICAL INITIAL yes 
     LABEL "Enviar Registro B (Conferˆncia CPF)" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .88 NO-UNDO.

DEFINE VARIABLE d-dt-fim AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88.

DEFINE VARIABLE d-dt-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Pagto F‚rias/Rescis" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88.

DEFINE VARIABLE d-dt-sal AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Outros Pagtos" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE i-bc-fim AS INTEGER FORMAT "zz9" INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 4.86 BY .88.

DEFINE VARIABLE i-bc-ini AS INTEGER FORMAT "zz9" INITIAL 0 
     LABEL "Banco":R7 
     VIEW-AS FILL-IN 
     SIZE 4.86 BY .88.

DEFINE VARIABLE i-es-fim LIKE rh_estab.cdn_estab
     VIEW-AS FILL-IN 
     SIZE 6.57 BY .88 NO-UNDO.

DEFINE VARIABLE i-es-ini LIKE rh_estab.cdn_estab
     VIEW-AS FILL-IN 
     SIZE 6.57 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_funcionario_fim AS INTEGER FORMAT "zzzzzzz9" INITIAL 99999999 
     VIEW-AS FILL-IN 
     SIZE 10.29 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_funcionario_ini AS INTEGER FORMAT "zzzzzzz9" INITIAL 0 
     LABEL "Matr¡cula":R11 
     VIEW-AS FILL-IN 
     SIZE 10.29 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-21
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-22
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
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

DEFINE RECTANGLE RECT-101
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50.72 BY 4.38.

DEFINE VARIABLE v_log_13_sal AS LOGICAL INITIAL no 
     LABEL "13 Sal rio" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.57 BY .88 NO-UNDO.

DEFINE VARIABLE v_log_adto_13_sal AS LOGICAL INITIAL no 
     LABEL "Adiantamento 13 Sal rio" 
     VIEW-AS TOGGLE-BOX
     SIZE 26.14 BY .88 NO-UNDO.

DEFINE VARIABLE v_log_adto_normal AS LOGICAL INITIAL no 
     LABEL "Adiantamento Normal" 
     VIEW-AS TOGGLE-BOX
     SIZE 26.14 BY .88 NO-UNDO.

DEFINE VARIABLE v_log_ferias AS LOGICAL INITIAL no 
     LABEL "F‚rias" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .88 NO-UNDO.

DEFINE VARIABLE v_log_normal AS LOGICAL INITIAL no 
     LABEL "Normal" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .88 NO-UNDO.

DEFINE VARIABLE v_log_plr AS LOGICAL INITIAL no 
     LABEL "PLR" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.72 BY .88 NO-UNDO.

DEFINE VARIABLE v_log_rescisao AS LOGICAL INITIAL no 
     LABEL "RescisÆo" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .88 NO-UNDO.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-lay 
     LABEL "Layout" 
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
     SIZE 79 BY 11.42
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

DEFINE QUERY br-digita-centr FOR 
      tt-digita SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-digita C-Win _FREEFORM
  QUERY br-digita DISPLAY
      tt-digita.es-codigo
tt-digita.c-nome-es label "RazÆo Social" format "x(28)"
tt-digita.fc-codigo
tt-digita.c-nome-fc label "Nome"
ENABLE
tt-digita.es-codigo
tt-digita.fc-codigo
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 76.57 BY 9
         BGCOLOR 15 .

DEFINE BROWSE br-digita-centr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-digita-centr C-Win _FREEFORM
  QUERY br-digita-centr DISPLAY
      tt-digita.es-codigo
tt-digita.c-nome-es label "RazÆo Social" format "x(28)"
tt-digita.fc-centr  label "Matric"
tt-digita.c-nome-fc label "Nome"
enable
tt-digita.es-codigo
tt-digita.fc-centr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 76.57 BY 9
         BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 2.5 COL 19 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     c-arquivo AT ROW 3.75 COL 19 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     bt-config-impr AT ROW 3.71 COL 59 HELP
          "Configura‡Æo da impressora"
     bt-arquivo AT ROW 3.71 COL 59 HELP
          "Escolha do nome do arquivo"
     rs-execucao AT ROW 5.88 COL 18.72 HELP
          "Modo de Execu‡Æo" NO-LABEL
     tb-parametro AT ROW 8.29 COL 28.57
     text-destino AT ROW 1.75 COL 19.57 NO-LABEL
     text-modo AT ROW 5.13 COL 17 COLON-ALIGNED NO-LABEL
     "Parƒmetros de ImpressÆo" VIEW-AS TEXT
          SIZE 25.57 BY .67 AT ROW 7.54 COL 19
     RECT-100 AT ROW 7.75 COL 17.86
     RECT-7 AT ROW 2.04 COL 17.86
     RECT-9 AT ROW 5.42 COL 17.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.14 BY 9.96.

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Cancelar"
     bt-lay AT ROW 14.54 COL 25
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-1 AT ROW 14.29 COL 2
     rt-folder AT ROW 2.5 COL 2
     im-pg-dig AT ROW 1.5 COL 33.57
     rt-folder-left AT ROW 2.54 COL 2.14
     im-pg-imp AT ROW 1.5 COL 49.14
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-sel AT ROW 1.5 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-dig
     br-digita AT ROW 1 COL 1
     br-digita-centr AT ROW 1 COL 1
     bt-inserir AT ROW 10 COL 1
     bt-inserir-2 AT ROW 10 COL 1
     bt-alterar AT ROW 10 COL 16
     bt-alterar-2 AT ROW 10 COL 16
     bt-retirar AT ROW 10 COL 31
     bt-retirar-2 AT ROW 10 COL 31
     bt-salvar AT ROW 10 COL 46
     bt-salvar-2 AT ROW 10 COL 46
     bt-recuperar-2 AT ROW 10 COL 61
     bt-recuperar AT ROW 10 COL 61
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.31
         SIZE 76.86 BY 10.15.

DEFINE FRAME f-pg-sel
     i-es-ini AT ROW 1.75 COL 23.72 COLON-ALIGNED HELP
          ""
     i-es-fim AT ROW 1.75 COL 47.57 COLON-ALIGNED HELP
          "" NO-LABEL
     v_cdn_funcionario_ini AT ROW 2.75 COL 23.72 COLON-ALIGNED HELP
          "N£mero da matr¡cula do funcion rio"
     v_cdn_funcionario_fim AT ROW 2.75 COL 47.57 COLON-ALIGNED HELP
          "N£mero da matr¡cula do funcion rio" NO-LABEL
     i-bc-ini AT ROW 3.75 COL 23.72 COLON-ALIGNED
     i-bc-fim AT ROW 3.75 COL 47.57 COLON-ALIGNED NO-LABEL
     d-dt-ini AT ROW 4.75 COL 23.72 COLON-ALIGNED
     d-dt-fim AT ROW 4.75 COL 47.57 COLON-ALIGNED NO-LABEL
     d-dt-sal AT ROW 5.75 COL 23.72 COLON-ALIGNED
     v_log_adto_normal AT ROW 7.58 COL 25.57
     v_log_rescisao AT ROW 7.58 COL 54.57
     v_log_ferias AT ROW 8.58 COL 25.57
     v_log_normal AT ROW 8.58 COL 54.57
     v_log_adto_13_sal AT ROW 9.58 COL 25.57
     v_log_plr AT ROW 9.54 COL 54.57
     v_log_13_sal AT ROW 10.58 COL 25.57
     "Tipo C lculo" VIEW-AS TEXT
          SIZE 9.43 BY .67 AT ROW 6.92 COL 25.57
     IMAGE-10 AT ROW 4.75 COL 46.43
     IMAGE-21 AT ROW 2.75 COL 38.43
     IMAGE-22 AT ROW 2.75 COL 46.43
     IMAGE-3 AT ROW 3.75 COL 38.43
     IMAGE-5 AT ROW 3.75 COL 46.43
     IMAGE-7 AT ROW 1.75 COL 38.43
     IMAGE-8 AT ROW 1.75 COL 46.43
     IMAGE-9 AT ROW 4.75 COL 38.43
     RECT-101 AT ROW 7.25 COL 21.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.83
         SIZE 76.86 BY 10.75
         FONT 1.

DEFINE FRAME f-pg-par
     c-arquivo-entrada AT ROW 1.71 COL 18.72 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     bt-arquivo-entrada AT ROW 1.71 COL 57.86 HELP
          "Escolha do nome do arquivo"
     v_cdn_banco AT ROW 3.17 COL 19 COLON-ALIGNED
     v_cdn_agencia AT ROW 4.17 COL 19 COLON-ALIGNED
     v_cod_dv_agencia AT ROW 4.17 COL 25.72 COLON-ALIGNED NO-LABEL
     v_num_conta AT ROW 5.17 COL 19 COLON-ALIGNED
     v_cod_dv AT ROW 5.17 COL 32.72 COLON-ALIGNED NO-LABEL
     v_cod_docto AT ROW 6.17 COL 19 COLON-ALIGNED
     dt-credito AT ROW 7.17 COL 19 COLON-ALIGNED
     v_cdn_estab_centr AT ROW 8.17 COL 19 COLON-ALIGNED HELP
          ""
     v_num_convenio AT ROW 9.17 COL 19 COLON-ALIGNED HELP
          "N£mero Contrato do Convenio entre Empresa e Banco HSBC"
     i-ind-selec AT ROW 3.5 COL 52.14 NO-LABEL
     v_log_cred_conta AT ROW 6.67 COL 52.29
     v_log_doc AT ROW 7.38 COL 52.29
     v_log_cheque AT ROW 8.08 COL 52.29
     v_log_cartao AT ROW 8.79 COL 52.29
     v_log_conta_poup AT ROW 9.5 COL 52.29
     text-entrada AT ROW 1.04 COL 18.43 NO-LABEL
     v_log_envia_reg_b AT ROW 10.54 COL 49.86
     FILL-IN-10 AT ROW 2.88 COL 50.29 COLON-ALIGNED NO-LABEL
     v_des_texto AT ROW 6.04 COL 50.29 COLON-ALIGNED NO-LABEL
     RECT-102 AT ROW 3.25 COL 50
     RECT-15 AT ROW 6.33 COL 49.86
     RECT-8 AT ROW 1.38 COL 16.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 77 BY 10.54
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-relat
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "L¡quido Magn‚tico PensÆo - Bradesco"
         HEIGHT             = 15.04
         WIDTH              = 81.14
         MAX-HEIGHT         = 30.08
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 30.08
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{include/i_cdrel_hr.i}
{src/adm/method/containr.i}
{include/w-relat.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME f-pg-dig:FRAME = FRAME f-relat:HANDLE.

/* SETTINGS FOR FRAME f-pg-dig
   FRAME-NAME                                                           */
/* BROWSE-TAB br-digita 1 f-pg-dig */
/* BROWSE-TAB br-digita-centr br-digita f-pg-dig */
/* SETTINGS FOR BUTTON bt-alterar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-alterar-2 IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-retirar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-retirar-2 IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-salvar IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-salvar-2 IN FRAME f-pg-dig
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-imp
   Custom                                                               */
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
   Custom                                                               */
ASSIGN 
       FILL-IN-10:PRIVATE-DATA IN FRAME f-pg-par     = 
                "Op‡Æo".

/* SETTINGS FOR FILL-IN text-entrada IN FRAME f-pg-par
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-entrada:PRIVATE-DATA IN FRAME f-pg-par     = 
                "Arquivo PensÆo Aliment¡cia".

/* SETTINGS FOR FILL-IN v_cdn_estab_centr IN FRAME f-pg-par
   LIKE = dthrpyc.rh_estab.cdn_estab                                    */
/* SETTINGS FOR FILL-IN v_des_texto IN FRAME f-pg-par
   NO-ENABLE                                                            */
ASSIGN 
       v_des_texto:PRIVATE-DATA IN FRAME f-pg-par     = 
                "Forma Lan‡amento".

/* SETTINGS FOR TOGGLE-BOX v_log_envia_reg_b IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-sel
   Custom                                                               */
/* SETTINGS FOR FILL-IN i-es-fim IN FRAME f-pg-sel
   LIKE = dthrpyc.rh_estab.cdn_estab EXP-SIZE                           */
/* SETTINGS FOR FILL-IN i-es-ini IN FRAME f-pg-sel
   LIKE = dthrpyc.rh_estab.cdn_estab EXP-SIZE                           */
/* SETTINGS FOR FRAME f-relat
                                                                        */
/* SETTINGS FOR IMAGE im-pg-dig IN FRAME f-relat
   NO-ENABLE                                                            */
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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-digita
/* Query rebuild information for BROWSE br-digita
     _START_FREEFORM
OPEN QUERY br-digita FOR EACH tt-digita.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-digita */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-digita-centr
/* Query rebuild information for BROWSE br-digita-centr
     _START_FREEFORM
OPEN QUERY br-digita-centr FOR EACH tt-digita.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-digita-centr */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-imp
/* Query rebuild information for FRAME f-pg-imp
     _Query            is NOT OPENED
*/  /* FRAME f-pg-imp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-sel
/* Query rebuild information for FRAME f-pg-sel
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* L¡quido Magn‚tico PensÆo - Bradesco */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* L¡quido Magn‚tico PensÆo - Bradesco */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-digita
&Scoped-define SELF-NAME br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON DEL OF br-digita IN FRAME f-pg-dig
DO:
   apply 'choose' to bt-retirar in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
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
        display tt-digita.es-codigo
                c-nome-es
                tt-digita.fc-codigo
                c-nome-fc with browse br-digita. 
    end.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON ENTER OF br-digita IN FRAME f-pg-dig
ANYWHERE
DO:
  /*apply 'tab' to self.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON INS OF br-digita IN FRAME f-pg-dig
DO:
   apply 'choose' to bt-inserir in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON OFF-END OF br-digita IN FRAME f-pg-dig
DO:
   apply 'entry' to bt-inserir in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON OFF-HOME OF br-digita IN FRAME f-pg-dig
DO:
  apply 'entry' to bt-recuperar in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON ROW-ENTRY OF br-digita IN FRAME f-pg-dig
DO:
   /* trigger para inicializar campos da temp table de digita‡Æo */
   if  br-digita:new-row in frame f-pg-dig then do:
       assign tt-digita.es-codigo:screen-value in browse br-digita = "0".
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON ROW-LEAVE OF br-digita IN FRAME f-pg-dig
do:
   if br-digita:NEW-ROW in frame f-pg-dig then 
      do transaction on error undo, return no-apply:
          create tt-digita.
          assign input browse br-digita tt-digita.es-codigo
                 input browse br-digita tt-digita.fc-codigo
                 tt-digita.v_cdn_empres_usuar    = v_cdn_empres_usuar.       
          find first funcionario no-lock where
                     funcionario.cdn_empresa     = tt-digita.v_cdn_empres_usuar and
                     funcionario.cdn_estab       = tt-digita.es-codigo          and
                     funcionario.cdn_funcionario = tt-digita.fc-codigo no-error.
          if avail funcionario then
             assign tt-digita.es-codigo          = funcionario.cdn_estab
                    tt-digita.fc-codigo          = funcionario.cdn_funcionario
                    tt-digita.cs-codigo          = funcionario.cdn_categ_sal 
                    tt-digita.orig-contr         = funcionario.idi_orig_contratac_func 
                    tt-digita.cdn-prest          = funcionario.cdn_prestdor_serv
                    tt-digita.c-nome-fc          = input browse br-digita tt-digita.c-nome-fc
                    tt-digita.c-nome-es          = input browse br-digita tt-digita.c-nome-es.
          else
             assign tt-digita.cs-codigo               = 0
                    tt-digita.orig-contr              = 0
                    tt-digita.cdn-prest               = 0
                    tt-digita.fc-codigo               = {prghur/dop/eng002.i}.

      end.
      else if avail tt-digita then do transaction on error undo, return no-apply:
          assign tt-digita.es-codigo = input browse br-digita tt-digita.es-codigo
                 tt-digita.FC-CODIGO = input browse br-digita tt-digita.fc-codigo
                 tt-digita.v_cdn_empres_usuar    = v_cdn_empres_usuar.
          find first funcionario no-lock where
                     funcionario.cdn_empresa       = tt-digita.v_cdn_empres_usuar and
                     funcionario.cdn_estab         = tt-digita.es-codigo          and
                     funcionario.cdn_funcionario   = tt-digita.fc-codigo no-error.
          if avail funcionario then
             assign tt-digita.es-codigo          = funcionario.cdn_estab       
                    tt-digita.fc-codigo          = funcionario.cdn_funcionario 
                    tt-digita.cs-codigo          = funcionario.cdn_categ_sal 
                    tt-digita.orig-contr         = funcionario.idi_orig_contratac_func 
                    tt-digita.cdn-prest          = funcionario.cdn_prestdor_serv
                    tt-digita.c-nome-fc          = input browse br-digita tt-digita.c-nome-fc
                    tt-digita.c-nome-es          = input browse br-digita tt-digita.c-nome-es.
          else
             assign tt-digita.cs-codigo               = 0
                    tt-digita.orig-contr              = 0
                    tt-digita.cdn-prest               = 0
                    tt-digita.fc-codigo               = {prghur/dop/eng002.i}.
       end.

   if br-digita:NEW-ROW in frame {&frame-name} then     br-digita:CREATE-RESULT-LIST-ENTRY() in frame f-pg-dig.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-digita-centr
&Scoped-define SELF-NAME br-digita-centr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita-centr C-Win
ON DEL OF br-digita-centr IN FRAME f-pg-dig
DO:
   apply 'choose' to bt-retirar in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita-centr C-Win
ON END-ERROR OF br-digita-centr IN FRAME f-pg-dig
ANYWHERE 
DO:
    if  br-digita-centr:new-row in frame f-pg-dig then do:
        if  avail tt-digita then
            delete tt-digita.
        if  br-digita-centr:delete-current-row() in frame f-pg-dig then. 
    end.                                                               
    else do:
        get current br-digita-centr.
        display tt-digita.es-codigo
                c-nome-es
                tt-digita.fc-centr
                c-nome-fc with browse br-digita-centr. 
    end.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita-centr C-Win
ON ENTER OF br-digita-centr IN FRAME f-pg-dig
ANYWHERE
DO:
  /*apply 'tab' to self.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita-centr C-Win
ON INS OF br-digita-centr IN FRAME f-pg-dig
DO:
   apply 'choose' to bt-inserir-2 in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita-centr C-Win
ON OFF-END OF br-digita-centr IN FRAME f-pg-dig
DO:
   apply 'entry' to bt-inserir-2 in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita-centr C-Win
ON OFF-HOME OF br-digita-centr IN FRAME f-pg-dig
DO:
  apply 'entry' to bt-recuperar-2 in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita-centr C-Win
ON ROW-ENTRY OF br-digita-centr IN FRAME f-pg-dig
DO:
   /* trigger para inicializar campos da temp table de digita‡Æo */
   if  br-digita-centr:new-row in frame f-pg-dig then do:
        assign tt-digita.es-codigo:screen-value in browse br-digita = "0".
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita-centr C-Win
ON ROW-LEAVE OF br-digita-centr IN FRAME f-pg-dig
DO:
   if br-digita-centr:NEW-ROW in frame f-pg-dig then 
      do transaction on error undo, return no-apply:
          create tt-digita.
          assign input browse br-digita-centr tt-digita.es-codigo
                 input browse br-digita-centr tt-digita.fc-centr
                 tt-digita.v_cdn_empres_usuar = v_cdn_empres_usuar.
          find first funcionario no-lock where
                     funcionario.cdn_empresa          = tt-digita.v_cdn_empres_usuar and
                     funcionario.cdn_estab            = tt-digita.es-codigo          and
                     funcionario.cdn_func_centrdor    = tt-digita.fc-centr           and
                     funcionario.cdn_tip_contrat_func = 0 no-error.
          if avail funcionario then
             assign tt-digita.cs-codigo               = funcionario.cdn_categ_sal 
                    tt-digita.orig-contr              = funcionario.idi_orig_contratac_func 
                    tt-digita.cdn-prest               = funcionario.cdn_prestdor_serv  
                    tt-digita.fc-codigo               = {prghur/dop/eng004.i &VAR="(funcionario.cdn_func_centrdor * 100)"}.
          else
             assign tt-digita.cs-codigo               = 0
                    tt-digita.orig-contr              = 0
                    tt-digita.cdn-prest               = 0
                    tt-digita.fc-codigo               = {prghur/dop/eng002.i}.
      end.
      else if avail tt-digita then do transaction on error undo, return no-apply:
          assign input browse br-digita-centr tt-digita.es-codigo
                 input browse br-digita-centr tt-digita.fc-centr
                 tt-digita.v_cdn_empres_usuar = v_cdn_empres_usuar.
          find first funcionario no-lock where
                     funcionario.cdn_empresa       = tt-digita.v_cdn_empres_usuar and
                     funcionario.cdn_estab         = tt-digita.es-codigo          and
                     funcionario.cdn_func_centrdor = tt-digita.fc-centr           and
                     funcionario.cdn_tip_contrat_func = 0 no-error.
          if avail funcionario then
             assign tt-digita.cs-codigo               = funcionario.cdn_categ_sal 
                    tt-digita.orig-contr              = funcionario.idi_orig_contratac_func 
                    tt-digita.cdn-prest               = funcionario.cdn_prestdor_serv  
                    tt-digita.fc-codigo               = {prghur/dop/eng004.i &VAR="(funcionario.cdn_func_centrdor * 100)"}.
          else
             assign tt-digita.cs-codigo               = 0
                    tt-digita.orig-contr              = 0
                    tt-digita.cdn-prest               = 0
                    tt-digita.fc-codigo               = {prghur/dop/eng002.i}.
      end.

  if br-digita-centr:NEW-ROW in frame {&frame-name} then    
      br-digita-centr:CREATE-RESULT-LIST-ENTRY() in frame f-pg-dig.
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


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-alterar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-alterar C-Win
ON CHOOSE OF bt-alterar IN FRAME f-pg-dig /* Alterar */
DO:
   apply 'entry' to tt-digita.es-codigo in browse br-digita. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-alterar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-alterar-2 C-Win
ON CHOOSE OF bt-alterar-2 IN FRAME f-pg-dig /* Alterar */
DO:
   apply 'entry' to tt-digita.es-codigo in browse br-digita-centr. 
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


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME bt-arquivo-entrada
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo-entrada C-Win
ON CHOOSE OF bt-arquivo-entrada IN FRAME f-pg-par
DO:
    {include/i-imarq.i c-arquivo-entrada f-pg-par}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar C-Win
ON CHOOSE OF bt-cancelar IN FRAME f-relat /* Cancelar */
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
   do  on error undo, return no-apply:
       run pi-executar.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-inserir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inserir C-Win
ON CHOOSE OF bt-inserir IN FRAME f-pg-dig /* Inserir */
DO:

    assign bt-alterar:SENSITIVE in frame f-pg-dig = yes
           bt-retirar:SENSITIVE in frame f-pg-dig = yes
           bt-salvar:SENSITIVE in frame f-pg-dig  = yes.
if num-results("br-digita":U) > 0 then
        br-digita:INSERT-ROW("after":U) in frame f-pg-dig.
    else do transaction:

    create tt-digita.
    open query br-digita
       for each tt-digita.

      apply 'entry' to tt-digita.es-codigo in browse br-digita. 
       end.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-inserir-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inserir-2 C-Win
ON CHOOSE OF bt-inserir-2 IN FRAME f-pg-dig /* Inserir */
DO:
    assign bt-alterar-2:SENSITIVE in frame f-pg-dig = yes
           bt-retirar-2:SENSITIVE in frame f-pg-dig = yes
           bt-salvar-2:SENSITIVE in frame f-pg-dig  = yes.

    create tt-digita.
    OPEN QUERY br-digita-centr FOR EACH tt-digita.

    apply 'entry' to tt-digita.es-codigo in browse br-digita-centr. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-lay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-lay C-Win
ON CHOOSE OF bt-lay IN FRAME f-relat /* Layout */
DO:
  run prghur/fpp/fp5831a.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-recuperar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-recuperar C-Win
ON CHOOSE OF bt-recuperar IN FRAME f-pg-dig /* Recuperar */
DO:
   {include/i-rprcd.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-recuperar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-recuperar-2 C-Win
ON CHOOSE OF bt-recuperar-2 IN FRAME f-pg-dig /* Recuperar */
DO:
   {include/i-rprcd.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-retirar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-retirar C-Win
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


&Scoped-define SELF-NAME bt-retirar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-retirar-2 C-Win
ON CHOOSE OF bt-retirar-2 IN FRAME f-pg-dig /* Retirar */
DO:
    if  br-digita-centr:num-selected-rows > 0 then do on error undo, return no-apply:
        get current br-digita-centr.
        delete tt-digita.
        if  br-digita-centr:delete-current-row() in frame f-pg-dig then.
    end.

    if num-results("br-digita") = 0 then
        assign bt-alterar-2:SENSITIVE in frame f-pg-dig = no
               bt-retirar-2:SENSITIVE in frame f-pg-dig = no
               bt-salvar-2:SENSITIVE in frame f-pg-dig  = no.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-salvar C-Win
ON CHOOSE OF bt-salvar IN FRAME f-pg-dig /* Salvar */
DO:
   {include/i-rpsvd.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-salvar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-salvar-2 C-Win
ON CHOOSE OF bt-salvar-2 IN FRAME f-pg-dig /* Salvar */
DO:
   {include/i-rpsvd.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME i-ind-selec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-ind-selec C-Win
ON VALUE-CHANGED OF i-ind-selec IN FRAME f-pg-par
DO:
    run pi-elimina-tt.
    run pi-desabilita-folder.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME im-pg-dig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-dig C-Win
ON MOUSE-SELECT-CLICK OF im-pg-dig IN FRAME f-relat
DO:
  if input frame f-pg-par i-ind-selec = 3 then do:
     run pi-elimina-tt.
     run pi-carrega-tt.
  end.
  RUN pi-verifica-pg.
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


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME v_cdn_banco
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_banco C-Win
ON F5 OF v_cdn_banco IN FRAME f-pg-par /* Banco */
DO:
   assign l-implanta = yes.
   {include/zoomvar.i &prog-zoom="object/sopy/zoom/z01py019.w"
                      &campo=v_cdn_banco
                      &campozoom=cdn_banco
                      &frame=f-pg-par}    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_banco C-Win
ON MOUSE-SELECT-DBLCLICK OF v_cdn_banco IN FRAME f-pg-par /* Banco */
DO:
  apply 'F5' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v_cdn_estab_centr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_estab_centr C-Win
ON F5 OF v_cdn_estab_centr IN FRAME f-pg-par /* Estabelecimento */
DO:
  assign l-implanta = yes.
   {include/zoomvar.i &prog-zoom="object/sopy/zoom/z01py060.w"
                      &campo=v_cdn_estab_centr
                      &campozoom=cdn_estab
                      &frame=f-pg-par}    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_estab_centr C-Win
ON MOUSE-SELECT-DBLCLICK OF v_cdn_estab_centr IN FRAME f-pg-par /* Estabelecimento */
DO:
  apply 'F5' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v_log_cheque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_log_cheque C-Win
ON VALUE-CHANGED OF v_log_cheque IN FRAME f-pg-par /* Cheque Pagto/Admin */
DO:
/*  if not input frame f-pg-par v_log_cheque and
 *      not input frame f-pg-par v_log_doc then 
 *      assign rs_cpfpis:sensitive in frame f-pg-par = no.
 *   else
 *      assign rs_cpfpis:sensitive in frame f-pg-par = yes.*/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v_log_doc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_log_doc C-Win
ON VALUE-CHANGED OF v_log_doc IN FRAME f-pg-par /* Documento Cr‚dito */
DO:
/*  if not input frame f-pg-par v_log_cheque and
 *      not input frame f-pg-par v_log_doc then 
 *      assign rs_cpfpis:sensitive in frame f-pg-par = no.
 *   else
 *      assign rs_cpfpis:sensitive in frame f-pg-par = yes.*/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define BROWSE-NAME br-digita
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


on leave of tt-digita.es-codigo in browse br-digita do:
   find rh_estab where 
        rh_estab.cdn_empresa = v_cdn_empres_usuar and
        rh_estab.cdn_estab = &if "{&cd_rel_hr}" >= "2.11" &then tt-digita.es-codigo:screen-value in browse br-digita &else integer(tt-digita.es-codigo:screen-value in browse br-digita) &endif
        no-lock no-error.
   if avail rh_estab then 
      assign tt-digita.c-nome-es:screen-value in browse br-digita = rh_estab.nom_pessoa_jurid
             /*tt-digita.c-nome-es = rh_estab.nom_pessoa_jurid*/.
   else
      assign tt-digita.c-nome-es:screen-value in browse br-digita = "".
end.

on leave of tt-digita.es-codigo in browse br-digita-centr do:
   
   find rh_estab where 
        rh_estab.cdn_empresa = v_cdn_empres_usuar and
        rh_estab.cdn_estab = &if "{&cd_rel_hr}" >= "2.11" &then tt-digita.es-codigo:screen-value in browse br-digita-centr &else integer(tt-digita.es-codigo:screen-value in browse br-digita-centr) &endif

        no-lock no-error.
   if avail rh_estab then 
      assign tt-digita.c-nome-es:screen-value in browse br-digita-centr = rh_estab.nom_pessoa_jurid
             /*tt-digita.c-nome-es = rh_estab.nom_pessoa_jurid*/.
   else
      assign tt-digita.c-nome-es:screen-value in browse br-digita-centr = "".
end.

on leave of tt-digita.fc-codigo in browse br-digita do:
   /*ASSIGN tt-digita.fc-codigo = INT(tt-digita.fc-codigo:screen-value in browse br-digita).*/
   find funcionario where 
        funcionario.cdn_empresa = v_cdn_empres_usuar and
        funcionario.cdn_estab = &if "{&cd_rel_hr}" >= "2.11" &then tt-digita.es-codigo:screen-value in browse br-digita &else integer(tt-digita.es-codigo:screen-value in browse br-digita) &endif
 and
        funcionario.cdn_funcionario = int(tt-digita.fc-codigo:screen-value in browse br-digita) 
        no-lock no-error.
   if avail funcionario then 
      assign tt-digita.c-nome-fc:screen-value in browse br-digita = funcionario.nom_pessoa_fisic.
   else
      assign tt-digita.c-nome-fc:screen-value in browse br-digita = "".
                

end.

on leave of tt-digita.fc-centr in browse br-digita-centr do:
   /*ASSIGN tt-digita.fc-centr = INT(tt-digita.fc-centr:screen-value in browse br-digita-centr).*/
   find funcionario where 
        funcionario.cdn_empresa = v_cdn_empres_usuar and
        funcionario.cdn_estab = &if "{&cd_rel_hr}" >= "2.11" &then tt-digita.es-codigo:screen-value in browse br-digita-centr &else integer(tt-digita.es-codigo:screen-value in browse br-digita-centr) &endif
 and
        funcionario.cdn_func_centrdor = int(tt-digita.fc-centr:screen-value in browse br-digita-centr) and
        funcionario.cdn_tip_contrat_func = 0
        no-lock no-error.
   if avail funcionario and avail tt-digita then 
      assign tt-digita.c-nome-fc:screen-value in browse br-digita-centr = funcionario.nom_pessoa_fisic.
   else
      assign tt-digita.c-nome-fc:screen-value in browse br-digita-centr = "".
                 

end.





/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* inicializa‡äes do template de relat¢rio */
{include/i-rpini.i}

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

{include/i-rplbl.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

assign i-es-ini = {prghur/dop/eng012.i}
       i-es-fim = {prghur/dop/eng007.i}.

 assign im-pg-sel:sensitive in frame f-relat = yes
           im-pg-dig:sensitive in frame f-relat = no
           wh-label-dig:fgcolor                 = 7.



on F5, mouse-select-dblclick of tt-digita.es-codigo in browse br-digita-centr do:
   {include/zoomvar.i &prog-zoom="object/sopy/zoom/z01py060.w"
                      &campo=tt-digita.es-codigo
                      &campozoom=cdn_estab
                      &browse=br-digita-centr}
end.

on F5, mouse-select-dblclick of tt-digita.es-codigo in browse br-digita do:
   {include/zoomvar.i &prog-zoom="object/sopy/zoom/z01py060.w"
                      &campo=tt-digita.es-codigo
                      &campozoom=cdn_estab
                      &browse=br-digita}
end.

on F5, mouse-select-dblclick of tt-digita.fc-codigo in browse br-digita do:
   {include/zoomvar.i &prog-zoom="object/sopy/zoom/z02py085.w"
                      &campo=tt-digita.fc-codigo
                      &campozoom=cdn_funcionario
                      &browse=br-digita}
end.

on F5, mouse-select-dblclick of tt-digita.fc-centr in browse br-digita-centr do:
   {include/zoomvar.i &prog-zoom="object/sopy/zoom/z02py085.w"
                      &campo=tt-digita.fc-centr
                      &campozoom=cdn_func_centrdor
                      &browse=br-digita-centr}
end.




/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN enable_UI.

    find param_empres_rh where param_empres_rh.cdn_empresa = v_cdn_empres_usuar no-lock no-error.
    if avail param_empres_rh then do:

      assign d-dt-ini:screen-value in frame f-pg-sel = string(date(param_empres_rh.num_mes_refer_calc_efetd,01,param_empres_rh.num_ano_refer_calc_efetd)).
      run prghur/fpp/fpapi002.p (input param_empres_rh.num_mes_refer_calc_efetd, input param_empres_rh.num_ano_refer_calc_efetd, input-output d-dt-fim).
      assign d-dt-fim:screen-value in frame f-pg-sel = string(d-dt-fim).
      run prghur/fpp/fpapi002.p (input param_empres_rh.num_mes_refer_calc_efetd, input param_empres_rh.num_ano_refer_calc_efetd, input-output d-dt-sal). 
      assign d-dt-sal:screen-value in frame f-pg-sel = string(d-dt-sal).

      assign dt-credito:screen-value in frame f-pg-par  = if (param_empres_rh.num_mes_refer_calc_efetd + 1) > 12
                          then string(date(01,01,(param_empres_rh.num_ano_refer_calc_efetd + 1)) - 1)
                          else string(date((param_empres_rh.num_mes_refer_calc_efetd + 1),01,param_empres_rh.num_ano_refer_calc_efetd) - 1).


      v_des_arq = "lpe" + string(param_empres_rh.num_mes_refer_calc_efetd,"99") +
                                  string(param_empres_rh.num_ano_refer_calc_efetd,"9999") + ".lst".
      {prghur/fpp/fp9200.i16 c-arquivo-entrada f-pg-par v_des_arq}                             

       FIND FIRST rh_cta_corren NO-LOCK WHERE
                  rh_cta_corren.cdn_empresa = param_empres_rh.cdn_empresa AND 
                  rh_cta_corren.cdn_banco   = 237 NO-ERROR.
       IF AVAIL rh_cta_corren THEN
           ASSIGN v_cdn_agencia:SCREEN-VALUE IN FRAME f-pg-par = STRING(rh_cta_corren.cdn_agenc_bcia)
                  v_cdn_banco:SCREEN-VALUE IN FRAME f-pg-par = STRING(rh_cta_corren.cdn_banco) 
                  v_cdn_estab_centr:SCREEN-VALUE IN FRAME f-pg-par = rh_cta_corren.cdn_estab 
                  v_num_convenio:SCREEN-VALUE IN FRAME f-pg-par =  SUBSTR(rh_cta_corren.cod_livre_1,37,10)
                  v_num_conta:SCREEN-VALUE IN FRAME f-pg-par = STRING(rh_cta_corren.cdn_cta_corren) 
                  v_cod_dv:SCREEN-VALUE IN FRAME f-pg-par = STRING(rh_cta_corren.cod_digito_agenc_bcia)
                  v_cod_dv_agencia:SCREEN-VALUE IN FRAME f-pg-par = STRING(rh_cta_corren.cod_digito_cta_corren).
    end. 

    {include/i-rpmbl.i}

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
  ENABLE im-pg-imp im-pg-par im-pg-sel bt-executar bt-cancelar bt-lay bt-ajuda 
      WITH FRAME f-relat IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY i-es-ini i-es-fim v_cdn_funcionario_ini v_cdn_funcionario_fim i-bc-ini 
          i-bc-fim d-dt-ini d-dt-fim d-dt-sal v_log_adto_normal v_log_rescisao 
          v_log_ferias v_log_normal v_log_adto_13_sal v_log_plr v_log_13_sal 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE i-es-ini i-es-fim v_cdn_funcionario_ini v_cdn_funcionario_fim i-bc-ini 
         i-bc-fim d-dt-ini d-dt-fim d-dt-sal v_log_adto_normal v_log_rescisao 
         v_log_ferias v_log_normal v_log_adto_13_sal v_log_plr v_log_13_sal 
         IMAGE-10 IMAGE-21 IMAGE-22 IMAGE-3 IMAGE-5 IMAGE-7 IMAGE-8 IMAGE-9 
         RECT-101 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino c-arquivo rs-execucao tb-parametro 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  ENABLE rs-destino c-arquivo bt-config-impr bt-arquivo rs-execucao 
         tb-parametro RECT-100 RECT-7 RECT-9 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY c-arquivo-entrada v_cdn_banco v_cdn_agencia v_cod_dv_agencia 
          v_num_conta v_cod_dv v_cod_docto dt-credito v_cdn_estab_centr 
          v_num_convenio i-ind-selec v_log_cred_conta v_log_doc v_log_cheque 
          v_log_cartao v_log_conta_poup v_log_envia_reg_b FILL-IN-10 v_des_texto 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  ENABLE c-arquivo-entrada bt-arquivo-entrada v_cdn_banco v_cdn_agencia 
         v_cod_dv_agencia v_num_conta v_cod_dv v_cod_docto dt-credito 
         v_cdn_estab_centr v_num_convenio i-ind-selec v_log_cred_conta 
         v_log_doc v_log_cheque v_log_cartao v_log_conta_poup FILL-IN-10 
         RECT-102 RECT-15 RECT-8 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  ENABLE br-digita br-digita-centr bt-inserir bt-inserir-2 bt-recuperar-2 
         bt-recuperar 
      WITH FRAME f-pg-dig IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-dig}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize C-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  
{utp/ut9000.i "YMAC0001" "1.02.00.018"}
  /*{include/win-size.i}*/
  
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  
  find first rh_bco_cta_corren_empres no-lock where 
             rh_bco_cta_corren_empres.cdn_empresa    = v_cdn_empres_usuar and
             rh_bco_cta_corren_empres.cdn_banco      = 237                  and
             rh_bco_cta_corren_empres.idi_tip_inform = 1 no-error.
  if avail rh_bco_cta_corren then do:
     find first rh_bco no-lock where
                rh_bco.cdn_banco = rh_bco_cta_corren_empres.cdn_banco no-error.
     if avail rh_bco then
        assign v_cdn_agencia:screen-value in frame f-pg-par    = string(rh_bco_cta_corren_empres.cdn_agenc_bcia)
               v_cod_dv_agencia:screen-value in frame f-pg-par = rh_bco_cta_corren_empres.cod_digito_verfdor_agenc
               v_num_conta:screen-value in frame f-pg-par      = rh_bco_cta_corren_empres.cod_cta_corren
               v_cod_dv:screen-value in frame f-pg-par         = rh_bco_cta_corren_empres.cod_digito_verfdor_cta_co
               v_cdn_banco:screen-value in frame f-pg-par      = string(rh_bco_cta_corren_empres.cdn_banco)
               v_num_convenio:screen-value in frame f-pg-par   = rh_bco_cta_corren_empres.cod_conven_empres_bco
               v_nom_arquivo                                   = "BBR" + string(rh_bco_cta_corren_empres.cdn_empresa) + ".lst".
              {prghur/fpp/fp9200.i16 c-arquivo-entrada f-pg-par v_nom_arquivo} 


  end.
  else
     assign v_cdn_agencia:screen-value in frame f-pg-par    = "0000"
            v_cod_dv_agencia:screen-value in frame f-pg-par = ""
            v_cdn_banco:screen-value in frame f-pg-par      = ""
            v_num_conta:screen-value in frame f-pg-par      = "000000000000"
            v_cod_dv:screen-value in frame f-pg-par         = ""
            v_num_convenio:screen-value in frame f-pg-par   = "".
           


   find param_folha_educnal no-lock where
        param_folha_educnal.cdn_empresa = v_cdn_empres_usuar no-error.
   if avail param_folha_educnal then
      assign v_log_folha_educnal = yes.

   if v_log_folha_educnal then do:
      assign br-digita-centr:visible in frame {&FRAME-NAME} = yes
             br-digita:visible in frame {&FRAME-NAME}       = no
             bt-inserir:visible in frame {&FRAME-NAME}      = no
             bt-alterar:visible in frame {&FRAME-NAME}      = no
             bt-retirar:visible in frame {&FRAME-NAME}      = no
             bt-salvar:visible in frame {&FRAME-NAME}       = no
             bt-recuperar:visible in frame {&FRAME-NAME}    = no.
      close query br-digita.
     {&OPEN-QUERY-br-digita-centr}
   end.
   else do:
      assign br-digita:visible in frame {&FRAME-NAME} = yes
             br-digita-centr:visible in frame {&FRAME-NAME} = no
             bt-inserir-2:visible in frame {&FRAME-NAME}    = no
             bt-alterar-2:visible in frame {&FRAME-NAME}    = no
             bt-retirar-2:visible in frame {&FRAME-NAME}    = no
             bt-salvar-2:visible in frame {&FRAME-NAME}     = no
             bt-recuperar-2:visible in frame {&FRAME-NAME}  = no.
      close query br-digita-centr.
     {&OPEN-QUERY-br-digita}
   end.

   apply "mouse-select-click" to im-pg-par in frame f-relat.

  /* assign v_cod_unid_lotac_ini_ant = v_cod_unid_lotac_ini:screen-value in frame f-pg-sel
          v_cod_unid_lotac_fim_ant = v_cod_unid_lotac_fim:screen-value in frame f-pg-sel.*/
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-tt C-Win 
PROCEDURE pi-carrega-tt :
/* Carrega a tt-digita conforme a sele‡Æo do usu rio */
{utp/ut-liter.i Estab: mfp}
assign v-msg-estab = return-value.
{utp/ut-liter.i Funcion rio: mfp}
assign v-msg-func = return-value.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Carregando_Lista_C lculo mfp}
run pi-inicializar in h-acomp (input return-value).    

if not v_log_folha_educnal then do:
   for each funcionario no-lock  where
            funcionario.cdn_empresa        =  v_cdn_empres_usuar                        and
            funcionario.cdn_estab         >= input frame f-pg-sel i-es-ini              and
            funcionario.cdn_estab         <= input frame f-pg-sel i-es-fim              and
            funcionario.cdn_funcionario   >= input frame f-pg-sel v_cdn_funcionario_ini and
            funcionario.cdn_funcionario   <= input frame f-pg-sel v_cdn_funcionario_fim and
            funcionario.idi_forma_pagto   =   1                                         and
            funcionario.cdn_bco_liq      >=  input frame f-pg-sel i-bc-ini              and
            funcionario.cdn_bco_liq      <=  input frame f-pg-sel i-bc-fim,
       each histor_pagto_palim no-lock
      where histor_pagto_palim.cdn_empresa           = funcionario.cdn_empresa
        and histor_pagto_palim.cdn_estab             = funcionario.cdn_estab  
        AND ((input frame f-pg-sel v_log_normal      = yes 
        and histor_pagto_palim.dat_palim_normal     >= input frame f-pg-sel d-dt-ini
        and histor_pagto_palim.dat_palim_normal     <= input frame f-pg-sel d-dt-fim)
         OR  (input frame f-pg-sel v_log_plr         = yes 
        and   histor_pagto_palim.dat_palim_ppr       = input frame f-pg-sel d-dt-sal) 
         or ((input frame f-pg-sel v_log_normal      = yes 
         or   input frame f-pg-sel v_log_adto_normal = yes) 
        and  (histor_pagto_palim.dat_palim_normal    = input frame f-pg-sel d-dt-sal   
         or   histor_pagto_palim.dat_palim_ppr       = input frame f-pg-sel d-dt-sal)) 
         or ((input frame f-pg-sel v_log_13_sal      = yes 
         or   input frame f-pg-sel v_log_adto_13_sal = yes)
        and   histor_pagto_palim.dat_palim_13        = input frame f-pg-sel d-dt-sal)):

      run pi-acompanhar in h-acomp (input v-msg-estab + " " + string(funcionario.cdn_estab) + " " + v-msg-func + " " + string(funcionario.cdn_funcionario)).
   
      find first tt-digita exclusive-lock where
                 tt-digita.v_cdn_empres_usuar = funcionario.cdn_empresa     and
                 tt-digita.es-codigo          = funcionario.cdn_estab       and                
                 tt-digita.fc-codigo          = funcionario.cdn_funcionario no-error.
      if avail tt-digita then
         next.

     
      find first rh_estab of funcionario no-lock no-error.
      if avail rh_estab then do:
         
         create tt-digita.
         assign tt-digita.v_cdn_empres_usuar      = funcionario.cdn_empresa
                tt-digita.es-codigo               = funcionario.cdn_estab                       
                tt-digita.c-nome-es               = rh_estab.nom_pessoa_jurid
                tt-digita.fc-codigo               = funcionario.cdn_funcionario                        
                tt-digita.c-nome-fc               = funcionario.nom_pessoa_fisic
                tt-digita.cs-codigo               = funcionario.cdn_categ_sal 
                tt-digita.orig-contr              = funcionario.idi_orig_contratac_func 
                tt-digita.cdn-prest               = funcionario.cdn_prestdor_serv.  
      end.
   end.
  
   {&open-query-br-digita}
      


   if num-results("br-digita") > 0 then
      assign bt-alterar:SENSITIVE in frame f-pg-dig = yes
             bt-retirar:SENSITIVE in frame f-pg-dig = yes
             bt-salvar:SENSITIVE in frame f-pg-dig  = yes.
   else
      assign bt-alterar:SENSITIVE in frame f-pg-dig = no
             bt-retirar:SENSITIVE in frame f-pg-dig = no
             bt-salvar:SENSITIVE in frame f-pg-dig  = no.
end.

if v_log_folha_educnal then do:
   for each funcionario no-lock where
            funcionario.cdn_sit_calc_func     = 0                                           and
            funcionario.cdn_empresa           = v_cdn_empres_usuar                          and
            funcionario.cdn_estab             >= input frame f-pg-sel i-es-ini              and
            funcionario.cdn_estab             <= input frame f-pg-sel i-es-fim              and
            funcionario.cdn_funcionario       >= input frame f-pg-sel v_cdn_funcionario_ini and       
            funcionario.cdn_funcionario       <= input frame f-pg-sel v_cdn_funcionario_fim and       
            funcionario.idi_forma_pagto       = 1                                           and
            funcionario.cdn_bco_liq           >= input frame f-pg-sel i-bc-ini              and
            funcionario.cdn_bco_liq           <= input frame f-pg-sel i-bc-fim              and   
            funcionario.cdn_tip_contrat_func  = 0,
       each histor_pagto_palim no-lock
      where histor_pagto_palim.cdn_empresa           = funcionario.cdn_empresa
        and histor_pagto_palim.cdn_estab             = funcionario.cdn_estab  
        AND ((input frame f-pg-sel v_log_normal      = yes 
        and histor_pagto_palim.dat_palim_normal     >= input frame f-pg-sel d-dt-ini
        and histor_pagto_palim.dat_palim_normal     <= input frame f-pg-sel d-dt-fim)
         OR  (input frame f-pg-sel v_log_plr         = yes 
        and   histor_pagto_palim.dat_palim_ppr       = input frame f-pg-sel d-dt-sal) 
         or ((input frame f-pg-sel v_log_normal      = yes 
         or   input frame f-pg-sel v_log_adto_normal = yes) 
        and  (histor_pagto_palim.dat_palim_normal    = input frame f-pg-sel d-dt-sal   
         or   histor_pagto_palim.dat_palim_ppr       = input frame f-pg-sel d-dt-sal)) 
         or ((input frame f-pg-sel v_log_13_sal      = yes 
         or   input frame f-pg-sel v_log_adto_13_sal = yes)
        and   histor_pagto_palim.dat_palim_13        = input frame f-pg-sel d-dt-sal)):

      run pi-acompanhar in h-acomp (input v-msg-estab + string(funcionario.cdn_estab) + " " + v-msg-func + string(funcionario.cdn_funcionario)).

      find first tt-digita no-lock where
                 tt-digita.v_cdn_empres_usuar = funcionario.cdn_empresa       and
                 tt-digita.es-codigo          = funcionario.cdn_estab         and  
                 tt-digita.fc-centr           = funcionario.cdn_func_centrdor and
                 tt-digita.fc-codigo          = funcionario.cdn_funcionario   no-error.
      if avail tt-digita then
         next.

      find first rh_estab of funcionario no-lock no-error.
      if avail rh_estab then do:
         create tt-digita.
         assign tt-digita.v_cdn_empres_usuar      = funcionario.cdn_empresa
                tt-digita.es-codigo               = funcionario.cdn_estab                       
                tt-digita.c-nome-es               = rh_estab.nom_pessoa_jurid
                tt-digita.fc-centr                = funcionario.cdn_func_centrdor
                tt-digita.fc-codigo               = funcionario.cdn_funcionario
                tt-digita.c-nome-fc               = funcionario.nom_pessoa_fisic
                tt-digita.cs-codigo               = funcionario.cdn_categ_sal 
                tt-digita.orig-contr              = funcionario.idi_orig_contratac_func 
                tt-digita.cdn-prest               = funcionario.cdn_prestdor_serv.  
      end.
   end.
   {&open-query-br-digita-centr}
   if num-results("br-digita-centr") > 0 then
      assign bt-alterar-2:SENSITIVE in frame f-pg-dig = yes
             bt-retirar-2:SENSITIVE in frame f-pg-dig = yes
             bt-salvar-2:SENSITIVE in frame f-pg-dig  = yes.
   else
      assign bt-alterar-2:SENSITIVE in frame f-pg-dig = no
             bt-retirar-2:SENSITIVE in frame f-pg-dig = no
             bt-salvar-2:SENSITIVE in frame f-pg-dig  = no.
end.


run pi-finalizar in h-acomp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-desabilita-folder C-Win 
PROCEDURE pi-desabilita-folder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
if input frame f-pg-par i-ind-selec = 1 then do:
   assign im-pg-dig:sensitive in frame f-relat = no
          wh-label-dig:fgcolor                 = 7
          v_cdn_funcionario_ini:sensitive in frame f-pg-sel = yes
          v_cdn_funcionario_fim:sensitive in frame f-pg-sel = yes
          i-es-ini:sensitive in frame f-pg-sel = yes
          i-es-fim:sensitive in frame f-pg-sel = yes.

   apply "mouse-select-click" to im-pg-sel in frame f-relat.
   run pi-first-child (input frame {&PGSEL}:handle).
end.
else if input frame f-pg-par i-ind-selec = 2 then do:
   assign im-pg-dig:sensitive in frame f-relat = yes
          wh-label-dig:fgcolor                 = ?
          v_cdn_funcionario_ini:sensitive in frame f-pg-sel = no
          v_cdn_funcionario_fim:sensitive in frame f-pg-sel = no
          i-es-ini:sensitive in frame f-pg-sel = no
          i-es-fim:sensitive in frame f-pg-sel = no.
         
   apply "mouse-select-click" to im-pg-dig in frame f-relat.
end.
else do:
   assign im-pg-dig:sensitive in frame f-relat = yes
          wh-label-dig:fgcolor                 = ?
          v_cdn_funcionario_ini:sensitive in frame f-pg-sel = yes
          v_cdn_funcionario_fim:sensitive in frame f-pg-sel = yes
          i-es-ini:sensitive in frame f-pg-sel = yes
          i-es-fim:sensitive in frame f-pg-sel = yes.
        
   apply "mouse-select-click" to im-pg-sel in frame f-relat.
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-elimina-tt C-Win 
PROCEDURE pi-elimina-tt :
/* Elimina registros tt-digita devido troca de opcao de selecao */
{utp/ut-liter.i Estab: mfp}
assign v-msg-estab = return-value.
{utp/ut-liter.i Funcion rio: mfp}
assign v-msg-func = return-value.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Eliminando_Lista_C lculo mfp}
run pi-inicializar in h-acomp (input return-value).    

find first tt-digita no-lock no-error.
if avail tt-digita then do:
   for each tt-digita exclusive-lock:
      run pi-acompanhar in h-acomp (input v-msg-estab + " " + string(tt-digita.es-codigo) + "  " + v-msg-func + " " + string(tt-digita.fc-codigo)).
      delete tt-digita.
   end.
   if not v_log_folha_educnal then do:
      {&open-query-br-digita}
      if num-results("br-digita") > 0 then
         assign bt-alterar:SENSITIVE in frame f-pg-dig = yes
                bt-retirar:SENSITIVE in frame f-pg-dig = yes
                bt-salvar:SENSITIVE in frame f-pg-dig  = yes.
      else
         assign bt-alterar:SENSITIVE in frame f-pg-dig = no
                bt-retirar:SENSITIVE in frame f-pg-dig = no
                bt-salvar:SENSITIVE in frame f-pg-dig  = no.
   end.
   else do:
      {&open-query-br-digita-centr}
      if num-results("br-digita-centr") > 0 then
         assign bt-alterar-2:SENSITIVE in frame f-pg-dig = yes
                bt-retirar-2:SENSITIVE in frame f-pg-dig = yes
                bt-salvar-2:SENSITIVE in frame f-pg-dig  = yes.
      else
         assign bt-alterar-2:SENSITIVE in frame f-pg-dig = no
                bt-retirar-2:SENSITIVE in frame f-pg-dig = no
                bt-salvar-2:SENSITIVE in frame f-pg-dig  = no.
   end.
end.
run pi-finalizar in h-acomp.

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

    {prghur/fpp/fp9200.i17 c-arquivo-entrada f-pg-par im-pg-par f-relat}          

   /***** Cr‚dito ********************************************/
    if input frame f-pg-par dt-credito = ? then do:
        {utp/ut-liter.i Cr‚dito_Pesionista MFP C}
        run utp/ut-msgs.p (input "show",
                           input 54,
                           input return-value).                               
        apply 'mouse-select-click' to im-pg-par in frame f-relat.
        apply 'entry' to dt-credito in frame f-pg-par.                
        return error.
    end.
    assign input frame f-pg-par dt-credito.

    assign i-dia = weekday(dt-credito).
    if i-dia = 7 then do:
       {utp/ut-liter.i S bado * C}
       run utp/ut-msgs.p (input "show",
                          input 3141,
                          input return-value).                               
       apply 'mouse-select-click' to im-pg-par in frame f-relat.
       apply 'entry' to dt-credito in frame f-pg-par.                
       return error.
    end.
    if i-dia = 1 then do:
       {utp/ut-liter.i Domingo * C}
       run utp/ut-msgs.p (input "show",
                          input 3141,
                          input return-value).                               
       apply 'mouse-select-click' to im-pg-par in frame f-relat.
       apply 'entry' to dt-credito in frame f-pg-par.                
       return error.
    end.
    /*find rh_bco no-lock where
         rh_bco.cdn_banco = input frame f-pg-par v_cdn_banco no-error.
    if not avail rh_bco then do:
       {utp/ut-table.i dthrpyc rh_bco 1}
        run utp/ut-msgs.p (input "show", input 56, input trim(return-value)).
        apply 'entry' to v_cdn_banco in frame f-pg-par.
        return error.
    end.

    find rh_agenc_bcia no-lock where
         rh_agenc_bcia.cdn_banco      = input frame f-pg-par v_cdn_banco and
         rh_agenc_bcia.cdn_agenc_bcia = input frame f-pg-par v_cdn_agencia no-error.
    if not avail rh_agenc_bcia then do:
       {utp/ut-table.i dthrpyc rh_agenc_bcia 1}
       run utp/ut-msgs.p (input "show", input 56, input trim(return-value)).
       apply 'entry' to v_cdn_agencia in frame f-pg-par.
       return error.
    end.         */

    if input frame f-pg-par v_num_conta < 1 then do:
       {utp/ut-liter.i Conta_da_Empresa MFP C}
        run utp/ut-msgs.p (input "show",
                           input 54,
                           input return-value).
        apply 'mouse-select-click' to im-pg-par in frame f-relat.
        apply 'entry' to v_num_conta in frame f-pg-par.
        return error.
    end.

    if input frame f-pg-par v_cdn_agencia < 1 then do:
       {utp/ut-liter.i Agˆncia_Empresa MFP C}
        run utp/ut-msgs.p (input "show",
                           input 54,
                           input return-value).
        apply 'mouse-select-click' to im-pg-par in frame f-relat.
        apply 'entry' to v_cdn_agencia in frame f-pg-par.
        return error.
    end.


    create tt-param.
    assign tt-param.usuario            = v_cod_usuar_corren
           tt-param.destino            = input frame f-pg-imp rs-destino
           tt-param.data-exec          = today
           tt-param.hora-exec          = time
           tt-param.tb-parametro       = input frame f-pg-imp tb-parametro
           tt-param.v_cdn_empres_usuar = v_cdn_empres_usuar
           tt-param.v_num_tip_aces_usuar = v_num_tip_aces_usuar
           tt-param.v_log_adto_normal  = input frame f-pg-sel v_log_adto_normal
           tt-param.v_log_ferias       = input frame f-pg-sel v_log_ferias
           tt-param.v_log_adto_13_sal  = input frame f-pg-sel v_log_adto_13_sal
           tt-param.v_log_rescisao     = input frame f-pg-sel v_log_rescisao
           tt-param.v_log_13_sal       = input frame f-pg-sel v_log_13_sal
           tt-param.v_log_normal       = input frame f-pg-sel v_log_normal
           tt-param.v_log_plr          = input frame f-pg-sel v_log_plr
           tt-param.i-es-ini           = input frame f-pg-sel i-es-ini
           tt-param.i-es-fim           = input frame f-pg-sel i-es-fim
           tt-param.i-bco-liq-ini      = input frame f-pg-sel i-bc-ini
           tt-param.i-bco-liq-fim      = input frame f-pg-sel i-bc-fim
           tt-param.d-dt-ini           = input frame f-pg-sel d-dt-ini
           tt-param.d-dt-fim           = input frame f-pg-sel d-dt-fim
           tt-param.d-dt-sal           = input frame f-pg-sel d-dt-sal
           tt-param.v_cdn_funcionario_ini = input frame f-pg-sel v_cdn_funcionario_ini
           tt-param.v_cdn_funcionario_fim = input frame f-pg-sel v_cdn_funcionario_fim
           tt-param.i-ind-selec         = input fram  f-pg-par i-ind-selec.

    if  tt-param.destino = 1 then           
        assign tt-param.arquivo = "".
    else
    if  tt-param.destino = 2 then 
        assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
    else
        assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".

           /* valida‡Æo tt-digita */

     IF INPUT FRAME f-pg-par i-ind-selec = 2 or 
        INPUT FRAME f-pg-par i-ind-selec = 3 THEN DO:
      FOR EACH tt-digita NO-LOCK:
          ASSIGN r-tt-digita = ROWID(tt-digita).

          FIND FIRST rh_estab NO-LOCK
               WHERE rh_estab.cdn_empresa = v_cdn_empres_usuar
                 AND rh_estab.cdn_estab   = tt-digita.es-codigo NO-ERROR.
          IF NOT AVAIL rh_estab THEN DO:
             {utp/ut-table.i dthrpyc rh_estab 1}
             RUN utp/ut-msgs.p (INPUT "show", INPUT 2, INPUT RETURN-VALUE).
             APPLY  "mouse-select-click" TO im-pg-dig IN FRAME f-relat.
             REPOSITION br-digita TO ROWID r-tt-digita.
             APPLY 'entry' TO tt-digita.es-codigo IN BROWSE br-digita.
             RETURN ERROR.
          END.

          FIND FIRST funcionario NO-LOCK
               WHERE funcionario.cdn_empresa     = v_cdn_empres_usuar
                 AND funcionario.cdn_estab       = tt-digita.es-codigo
                 AND funcionario.cdn_funcionario = tt-digita.fc-codigo NO-ERROR.
          IF NOT AVAIL funcionario THEN DO:
             {utp/ut-table.i dthrpyc funcionario 1}
             RUN utp/ut-msgs.p (INPUT "show", INPUT 2, INPUT RETURN-VALUE).
             APPLY  "mouse-select-click" TO im-pg-dig IN FRAME f-relat.
             REPOSITION br-digita TO ROWID r-tt-digita.
             APPLY 'entry' TO tt-digita.fc-codigo IN BROWSE br-digita.
             RETURN ERROR.
          END.

          FIND FIRST b-tt-digita NO-LOCK
               WHERE b-tt-digita.es-codigo = tt-digita.es-codigo
                 AND b-tt-digita.fc-codigo = tt-digita.fc-codigo
                 AND ROWID(b-tt-digita) <> r-tt-digita NO-ERROR.
          IF AVAIL b-tt-digita THEN DO:
             APPLY "MOUSE-SELECT-CLICK":U TO im-pg-dig IN FRAME f-relat.
             REPOSITION br-digita TO ROWID ROWID(b-tt-digita).
             RUN utp/ut-msgs.p (INPUT "show":U, INPUT 108, INPUT "").
             APPLY "ENTRY":U TO tt-digita.fc-codigo IN BROWSE br-digita.
             RETURN ERROR.
          END.
      END.
   END.


   if input frame f-pg-par v_cdn_estab_centr > &if "{&cd_rel_hr}" >= "2.11" &then " " &else 0 &endif
 then do:
       find rh_estab no-lock where
            rh_estab.cdn_empresa = v_cdn_empres_usuar and
            rh_estab.cdn_estab   = input frame f-pg-par v_cdn_estab_centr no-error.
       if not avail rh_estab then do:
          {utp/ut-table.i dthrpyc rh_estab 1}
          run utp/ut-msgs.p (input "show", input 56, input return-value).
          apply 'mouse-select-click' to im-pg-par in frame f-relat.
          apply 'entry' to v_cdn_estab_centr in frame f-pg-par.                   
          return error.
       end.

       IF v_num_tip_aces_usuar = 1 then do:
          if not can-find(permis_estab_usuar where 
                          permis_estab_usuar.cod_usuario = v_cod_usuar_corren and
                          permis_estab_usuar.cdn_empresa = v_cdn_empres_usuar and
                          permis_estab_usuar.cdn_estab   = input frame f-pg-par v_cdn_estab_centr) then do:
             run utp/ut-msgs.p (input "show",
                                input 16501,
                                input "").
             apply 'mouse-select-click' to im-pg-par in frame f-relat.
             apply 'entry' to v_cdn_estab_centr in frame f-pg-par. 
             return error.
          end.    
      end.
    end.



    /* Coloque aqui a l¢gica de grava‡Æo dos parƒmtros e sele‡Æo na temp-table
       tt-param */ 

    assign tt-param.c-arq-lqd          = input frame f-pg-par c-arquivo-entrada
           tt-param.cdn_agencia        = input frame f-pg-par v_cdn_agencia
           tt-param.cod_dv_agencia     = input frame f-pg-par v_cod_dv_agencia
           tt-param.num_conta          = input frame f-pg-par v_num_conta
           tt-param.cod_dv_conta       = input frame f-pg-par v_cod_dv
           tt-param.cod_docto          = input frame f-pg-par v_cod_docto
           tt-param.dat_lancto         = input frame f-pg-par dt-credito
           tt-param.cdn_estab_centr    = input frame f-pg-par v_cdn_estab_centr 
           tt-param.num_convenio       = input frame f-pg-par v_num_convenio
           tt-param.log_cred_conta     = input frame f-pg-par v_log_cred_conta
           tt-param.log_doc            = input frame f-pg-par v_log_doc
           tt-param.cdn_banco          = input frame f-pg-par v_cdn_banco
           tt-param.log_cheque         = input frame f-pg-par v_log_cheque
           tt-param.log_cartao         = input frame f-pg-par v_log_cartao
           tt-param.log_conta_poup     = input frame f-pg-par v_log_conta_poup
           tt-param.v_log_envia_reg_b  = input frame f-pg-par v_log_envia_reg_b
           tt-param.i-ind-selec        = input frame f-pg-par i-ind-selec
           tt-param.cdn_funcionario_ini = input frame f-pg-sel v_cdn_funcionario_ini
           tt-param.cdn_funcionario_fim = input frame f-pg-sel v_cdn_funcionario_fim
           tt-param.num_cpfpis         = 1 .

    {include/i-rpexb.i}

    if  session:set-wait-state("general") then.

    {include/i-rprun.i prghur/esp/ymac0001rp.p}

    {include/i-rpexc.i}

    if  session:set-wait-state("") then.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-verifica-pg C-Win 
PROCEDURE pi-verifica-pg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-digita"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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


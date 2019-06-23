&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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
{include/i-prgvrs.i YMAC0003 1.02.00.003 } /*** 010003 ***/
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

/* Preprocessadores do Template de Relat¢rio                            */
/* Obs: Retirar o valor do preprocessador para as p†ginas que n∆o existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA f-pg-cla
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG 
&GLOBAL-DEFINE PGIMP f-pg-imp

/* Include Com as Vari†veis Globais */
{utp/ut-glob.i}
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */

{prghur/esp/ymac0003tt.i}

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.
                    
/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var i-dia              as integer no-undo.
def var v_des_arq          as char    no-undo.

{prghur/fpp/fp9200.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
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
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE rs-classif AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Por Estabelecimento/Matr°cula", 1,
"Por Estabelecimento/Nome", 2,
"Por Estabelecimento/Conta Corrente", 3
     SIZE 49.14 BY 3.79 NO-UNDO.

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

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execuá∆o" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE text-parametro AS CHARACTER FORMAT "X(256)":U INITIAL "ParÉmetros de Impress∆o" 
      VIEW-AS TEXT 
     SIZE 27 BY .63 NO-UNDO.

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

DEFINE RECTANGLE RECT-98
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE VARIABLE tb-parametro AS LOGICAL INITIAL no 
     LABEL "Imprimir P†gina de ParÉmetros" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .83 NO-UNDO.

DEFINE BUTTON bt-arquivo-3 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arq-lqd AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_agencia AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Agància Empresa" 
     VIEW-AS FILL-IN 
     SIZE 6.57 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_banco AS INTEGER FORMAT "zz9":U INITIAL 0 
     LABEL "Banco Empresa" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_estab_centr AS INTEGER FORMAT "zz9":U INITIAL 0 
     LABEL "Estabelecimento Central" 
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

DEFINE VARIABLE v_dat_lancto AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Lanáamento" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE v_des_texto AS CHARACTER FORMAT "X(256)":U INITIAL "Forma Lanáamento" 
      VIEW-AS TEXT 
     SIZE 14.72 BY .67 NO-UNDO.

DEFINE VARIABLE v_num_conta AS DECIMAL FORMAT "999999999999":U INITIAL 0 
     LABEL "Conta Corrente Empresa" 
     VIEW-AS FILL-IN 
     SIZE 13.57 BY .88 NO-UNDO.

DEFINE VARIABLE v_num_convenio AS INTEGER FORMAT "999999":U INITIAL 0 
     LABEL "Convànio" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE v_num_seq AS INTEGER FORMAT "999,999":U INITIAL 0 
     LABEL "Nß Sequencial" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26.14 BY 4.13.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.83.

DEFINE RECTANGLE ret_cpfpis-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26.14 BY 1.46.

DEFINE VARIABLE v_log_cartao AS LOGICAL INITIAL no 
     LABEL "Cart∆o Sal†rio" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.43 BY .71 NO-UNDO.

DEFINE VARIABLE v_log_cheque AS LOGICAL INITIAL no 
     LABEL "Cheque Pagto/Admin" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.14 BY .71 NO-UNDO.

DEFINE VARIABLE v_log_conta_poup AS LOGICAL INITIAL no 
     LABEL "CrÇdito Conta Poupanáa" 
     VIEW-AS TOGGLE-BOX
     SIZE 22.57 BY .71 NO-UNDO.

DEFINE VARIABLE v_log_cred_conta AS LOGICAL INITIAL yes 
     LABEL "CrÇdito Conta Corrente" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.43 BY .71 NO-UNDO.

DEFINE VARIABLE v_log_doc AS LOGICAL INITIAL no 
     LABEL "Documento CrÇdito" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.86 BY .71 NO-UNDO.

DEFINE VARIABLE v_log_gera_seg_b AS LOGICAL INITIAL yes 
     LABEL "Gera Segmento B" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .71 NO-UNDO.

DEFINE VARIABLE da-pgto-fin AS DATE FORMAT "99/99/9999" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE da-pgto-ini AS DATE FORMAT "99/99/9999" 
     LABEL "Data Pagamento":R18 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE i-ban-liq-fin AS INTEGER FORMAT "zz9" INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE i-ban-liq-ini AS INTEGER FORMAT "zz9" INITIAL 1 
     LABEL "Banco":R7 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE i-es-fim AS INTEGER FORMAT "zz9" INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE i-es-ini AS INTEGER FORMAT "zz9" INITIAL 1 
     LABEL "Estabelecimento":R18 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE v_matr_fim AS INTEGER FORMAT ">>,>>>,>>9":U INITIAL 99999999 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE v_matr_ini AS INTEGER FORMAT ">>,>>>,>>9":U INITIAL 0 
     LABEL "Matr°cula" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43.43 BY 3.83.

DEFINE VARIABLE l-diarista AS LOGICAL INITIAL yes 
     LABEL "Diarista" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE l-horista AS LOGICAL INITIAL yes 
     LABEL "Horista" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE l-mensal AS LOGICAL INITIAL yes 
     LABEL "Mensal" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE l-quinzenal AS LOGICAL INITIAL yes 
     LABEL "Quinzenal" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.14 BY .83 NO-UNDO.

DEFINE VARIABLE l-semanal AS LOGICAL INITIAL yes 
     LABEL "Semanal" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE l-tarefa AS LOGICAL INITIAL yes 
     LABEL "Tarefa" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

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

DEFINE FRAME f-pg-cla
     rs-classif AT ROW 4.17 COL 16.14 HELP
          "Classificaá∆o para emiss∆o do relat¢rio" NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.86 BY 10.31.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 3.54 COL 18.14 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     bt-arquivo AT ROW 4.71 COL 58.14 HELP
          "Escolha do nome do arquivo"
     bt-config-impr AT ROW 4.71 COL 58.14 HELP
          "Configuraá∆o da impressora"
     c-arquivo AT ROW 4.75 COL 18.14 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 6.92 COL 17.86 HELP
          "Modo de Execuá∆o" NO-LABEL
     tb-parametro AT ROW 9.21 COL 29
     text-destino AT ROW 2.75 COL 18.72 NO-LABEL
     text-modo AT ROW 6.13 COL 16.14 COLON-ALIGNED NO-LABEL
     text-parametro AT ROW 8.42 COL 19 NO-LABEL
     RECT-7 AT ROW 3.08 COL 17
     RECT-9 AT ROW 6.46 COL 17
     RECT-98 AT ROW 8.71 COL 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 75.43 BY 10.29.

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Cancelar"
     bt-lay AT ROW 14.54 COL 25
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     rt-folder-right AT ROW 2.67 COL 80.43
     rt-folder-left AT ROW 2.54 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     RECT-6 AT ROW 13.75 COL 2.14
     im-pg-cla AT ROW 1.5 COL 33.57
     im-pg-imp AT ROW 1.5 COL 49.29
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-sel AT ROW 1.5 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-par
     c-arq-lqd AT ROW 2.46 COL 1.57 COLON-ALIGNED HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     bt-arquivo-3 AT ROW 2.46 COL 43.43 HELP
          "Escolha do nome do arquivo"
     v_dat_lancto AT ROW 4.42 COL 24.29 COLON-ALIGNED
     v_cdn_banco AT ROW 5.42 COL 24.29 COLON-ALIGNED
     v_cdn_agencia AT ROW 6.42 COL 24.29 COLON-ALIGNED
     v_cod_dv_agencia AT ROW 6.42 COL 31 COLON-ALIGNED NO-LABEL
     v_num_conta AT ROW 7.42 COL 24.29 COLON-ALIGNED
     v_cod_dv AT ROW 7.42 COL 38 COLON-ALIGNED NO-LABEL
     v_cdn_estab_centr AT ROW 8.42 COL 24.29 COLON-ALIGNED
     v_num_convenio AT ROW 9.42 COL 24.29 COLON-ALIGNED HELP
          "Contrato do Convenio entre Empresa e o  Banco"
     v_cod_docto AT ROW 10.42 COL 24.29 COLON-ALIGNED
     v_log_cred_conta AT ROW 3.79 COL 51.86
     v_log_doc AT ROW 4.5 COL 51.86
     v_log_cheque AT ROW 5.21 COL 51.86
     v_log_cartao AT ROW 5.96 COL 51.86
     v_des_texto AT ROW 3.08 COL 50.86 COLON-ALIGNED NO-LABEL
     v_log_conta_poup AT ROW 6.71 COL 51.86
     v_log_gera_seg_b AT ROW 8.5 COL 51.72
     v_num_seq AT ROW 10 COL 62 COLON-ALIGNED
     "Arquivo L°quidos" VIEW-AS TEXT
          SIZE 17.14 BY .67 AT ROW 1.5 COL 4.57
     "Segmento B" VIEW-AS TEXT
          SIZE 16.43 BY .71 AT ROW 7.75 COL 52.86
     RECT-15 AT ROW 3.5 COL 49.43
     RECT-21 AT ROW 1.88 COL 2
     ret_cpfpis-2 AT ROW 8.04 COL 49.43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.29 ROW 2.83
         SIZE 76.57 BY 10.67.

DEFINE FRAME f-pg-sel
     i-es-ini AT ROW 1.75 COL 23 COLON-ALIGNED HELP
          "Informe C¢digo do Estabelecimento Inicial"
     i-es-fim AT ROW 1.75 COL 47.86 COLON-ALIGNED HELP
          "Informe C¢digo do Estabelecimento Final" NO-LABEL
     i-ban-liq-ini AT ROW 2.75 COL 23 COLON-ALIGNED HELP
          "Informe C¢digo do Banco Inicial"
     i-ban-liq-fin AT ROW 2.75 COL 47.86 COLON-ALIGNED HELP
          "Informe C¢digo do Banco Final" NO-LABEL
     da-pgto-ini AT ROW 3.75 COL 23 COLON-ALIGNED HELP
          "Informe Data de Pagamento Inicial"
     da-pgto-fin AT ROW 3.75 COL 61.86 RIGHT-ALIGNED HELP
          "Informe Data de Pagamento Final" NO-LABEL
     v_matr_ini AT ROW 4.75 COL 22 COLON-ALIGNED WIDGET-ID 2
     v_matr_fim AT ROW 4.75 COL 48 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     l-mensal AT ROW 6.92 COL 21
     l-horista AT ROW 7.92 COL 21
     l-semanal AT ROW 8.92 COL 21
     l-quinzenal AT ROW 6.92 COL 45.86
     l-tarefa AT ROW 7.92 COL 45.86
     l-diarista AT ROW 8.92 COL 45.86
     "Categoria Salarial" VIEW-AS TEXT
          SIZE 17.72 BY .67 AT ROW 5.92 COL 22.14
     IMAGE-1 AT ROW 1.75 COL 37.86
     IMAGE-2 AT ROW 1.75 COL 47.29
     IMAGE-5 AT ROW 3.75 COL 37.86
     IMAGE-6 AT ROW 2.75 COL 37.86
     IMAGE-7 AT ROW 2.75 COL 47.29
     IMAGE-9 AT ROW 3.75 COL 47.29
     RECT-14 AT ROW 6.25 COL 18.72
     IMAGE-8 AT ROW 4.75 COL 38 WIDGET-ID 6
     IMAGE-10 AT ROW 4.75 COL 47 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 15.08
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-relat.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME f-pg-par:FRAME = FRAME f-relat:HANDLE.

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
                "Execuá∆o".

/* SETTINGS FOR FILL-IN text-parametro IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-parametro:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "ParÉmetros de Impress∆o".

/* SETTINGS FOR FRAME f-pg-par
   Custom                                                               */
/* SETTINGS FOR FILL-IN v_des_texto IN FRAME f-pg-par
   NO-ENABLE                                                            */
ASSIGN 
       v_des_texto:PRIVATE-DATA IN FRAME f-pg-par     = 
                "Forma Lanáamento".

/* SETTINGS FOR TOGGLE-BOX v_log_gera_seg_b IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-sel
   Custom                                                               */
/* SETTINGS FOR FILL-IN da-pgto-fin IN FRAME f-pg-sel
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
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
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


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME bt-arquivo-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo-3 C-Win
ON CHOOSE OF bt-arquivo-3 IN FRAME f-pg-par
DO:
 {include/i-imarq.i c-arq-lqd f-pg-par} 
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


&Scoped-define SELF-NAME bt-lay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-lay C-Win
ON CHOOSE OF bt-lay IN FRAME f-relat /* Layout */
DO:
  run prghur/esp/ymfr0002a.w .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME v_cdn_banco
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_banco C-Win
ON F5 OF v_cdn_banco IN FRAME f-pg-par /* Banco Empresa */
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
ON MOUSE-SELECT-DBLCLICK OF v_cdn_banco IN FRAME f-pg-par /* Banco Empresa */
DO:
  apply 'F5' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v_cdn_estab_centr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_cdn_estab_centr C-Win
ON F5 OF v_cdn_estab_centr IN FRAME f-pg-par /* Estabelecimento Central */
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
ON MOUSE-SELECT-DBLCLICK OF v_cdn_estab_centr IN FRAME f-pg-par /* Estabelecimento Central */
DO:
  apply 'F5' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-cla
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
v_cdn_banco:load-mouse-pointer ("image/lupa.cur").
v_cdn_estab_centr:load-mouse-pointer ("image/lupa.cur").

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.



{utp/ut9000.i "YMAC0003" "1.02.00.000"}

/* inicializaá‰es do template de relat¢rio */
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
  
   {prghur/fpp/fp9200.i2}
      
    find param_empres_rh where param_empres_rh.cdn_empresa = v_cdn_empres_usuar no-lock no-error.
    if avail param_empres_rh then do:
       assign da-pgto-ini:screen-value in frame f-pg-sel = string(date(param_empres_rh.num_mes_refer_calc_efetd,01,param_empres_rh.num_ano_refer_calc_efetd))
              da-pgto-fin:screen-value in frame f-pg-sel = if (param_empres_rh.num_mes_refer_calc_efetd + 1) > 12 
                                                           then string(date(01,01,(param_empres_rh.num_ano_refer_calc_efetd + 1)) - 1)
                                                           else string(date((param_empres_rh.num_mes_refer_calc_efetd + 1),01,param_empres_rh.num_ano_refer_calc_efetd) - 1)
              v_dat_lancto:screen-value in frame f-pg-par = 
              if (param_empres_rh.num_mes_refer_calc_efetd + 1) > 12 then
                 string(date(01,01,(param_empres_rh.num_ano_refer_calc_efetd + 1)) - 1)
              else 
                 string(date((param_empres_rh.num_mes_refer_calc_efetd + 1),01,param_empres_rh.num_ano_refer_calc_efetd) - 1).
       if (param_empres_rh.num_mes_refer_calc_efetd + 1) > 12 then
           string(date(01,01,(param_empres_rh.num_ano_refer_calc_efetd + 1)) - 1).
       else 
           string(date((param_empres_rh.num_mes_refer_calc_efetd + 1),01,param_empres_rh.num_ano_refer_calc_efetd) - 1).
       v_des_arq = "liq" + string(param_empres_rh.num_mes_refer_calc_efetd,"99") +
                           string(param_empres_rh.num_ano_refer_calc_efetd,"9999") + ".lst".
       {prghur/fpp/fp9200.i16 c-arq-lqd f-pg-par v_des_arq}
    end.  

        /* Dados de cadastro de conta corrente */ 
       FIND FIRST rh_cta_corren NO-LOCK WHERE
                  rh_cta_corren.cdn_empresa = v_cdn_empres_usuar NO-ERROR.
       IF AVAIL rh_cta_corren THEN
           ASSIGN v_cdn_agencia    :SCREEN-VALUE IN FRAME f-pg-par = STRING(rh_cta_corren.cdn_agenc_bcia)
                  v_cdn_banco      :SCREEN-VALUE IN FRAME f-pg-par = STRING(rh_cta_corren.cdn_banco) 
                  v_cdn_estab_centr:SCREEN-VALUE IN FRAME f-pg-par = rh_cta_corren.cdn_estab 
                  v_num_convenio   :SCREEN-VALUE IN FRAME f-pg-par = SUBSTR(rh_cta_corren.cod_livre_1,37,10)
                  v_num_conta      :SCREEN-VALUE IN FRAME f-pg-par = STRING(rh_cta_corren.cdn_cta_corren) 
                  v_cod_dv_agencia :SCREEN-VALUE IN FRAME f-pg-par = STRING(rh_cta_corren.cod_digito_agenc_bcia)
                  v_cod_dv         :SCREEN-VALUE IN FRAME f-pg-par = STRING(rh_cta_corren.cod_digito_cta_corren).
        
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
  ENABLE im-pg-cla im-pg-imp im-pg-par im-pg-sel bt-executar bt-cancelar bt-lay 
         bt-ajuda 
      WITH FRAME f-relat IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY c-arq-lqd v_dat_lancto v_cdn_banco v_cdn_agencia v_cod_dv_agencia 
          v_num_conta v_cod_dv v_cdn_estab_centr v_num_convenio v_cod_docto 
          v_log_cred_conta v_log_doc v_log_cheque v_log_cartao v_des_texto 
          v_log_conta_poup v_log_gera_seg_b v_num_seq 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  ENABLE c-arq-lqd bt-arquivo-3 v_dat_lancto v_cdn_banco v_cdn_agencia 
         v_cod_dv_agencia v_num_conta v_cod_dv v_cdn_estab_centr v_num_convenio 
         v_cod_docto v_log_cred_conta v_log_doc v_log_cheque v_log_cartao 
         v_log_conta_poup v_num_seq RECT-15 RECT-21 ret_cpfpis-2 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY i-es-ini i-es-fim i-ban-liq-ini i-ban-liq-fin da-pgto-ini da-pgto-fin 
          v_matr_ini v_matr_fim l-mensal l-horista l-semanal l-quinzenal 
          l-tarefa l-diarista 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE i-es-ini i-es-fim i-ban-liq-ini i-ban-liq-fin da-pgto-ini da-pgto-fin 
         v_matr_ini v_matr_fim l-mensal l-horista l-semanal l-quinzenal 
         l-tarefa l-diarista IMAGE-1 IMAGE-2 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-9 
         RECT-14 IMAGE-8 IMAGE-10 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-classif 
      WITH FRAME f-pg-cla IN WINDOW C-Win.
  ENABLE rs-classif 
      WITH FRAME f-pg-cla IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-cla}
  DISPLAY rs-destino c-arquivo rs-execucao tb-parametro 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  ENABLE RECT-7 RECT-9 RECT-98 rs-destino bt-arquivo bt-config-impr c-arquivo 
         rs-execucao tb-parametro 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
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

do  on error undo, return error
    on stop  undo, return error:     

    {include/i-rpexa.i}

    if  input frame f-pg-par c-arq-lqd = "" then do:
        run utp/ut-msgs.p (input "show",
                           input 73,
                           input "").                               
        apply 'mouse-select-click' to im-pg-par in frame f-relat.
        apply 'entry' to c-arq-lqd in frame f-pg-par.                
        return error.
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

    {prghur/fpp/fp9200.i17 c-arq-lqd f-pg-par im-pg-par f-relat}
    
    if input frame f-pg-par v_dat_lancto = ? then do:
       {utp/ut-liter.i Data_do_DÇbito MFP C}
        run utp/ut-msgs.p (input "show",
                           input 54,
                           input return-value).                               
        apply 'mouse-select-click' to im-pg-par in frame f-relat.
        apply 'entry' to v_dat_lancto in frame f-pg-par.                
        return error.
    end.
    assign input frame f-pg-par v_dat_lancto.
    assign i-dia = weekday(v_dat_lancto).
    if i-dia = 7 then do:
       {utp/ut-liter.i S†bado * C}
       run utp/ut-msgs.p (input "show",
                          input 3141,
                          input return-value).                               
       apply 'mouse-select-click' to im-pg-par in frame f-relat.
       apply 'entry' to v_dat_lancto in frame f-pg-par.                
       return error.
    end.
    if i-dia = 1 then do:
       {utp/ut-liter.i Domingo * C}
       run utp/ut-msgs.p (input "show",
                          input 3141,
                          input return-value).                               
       apply 'mouse-select-click' to im-pg-par in frame f-relat.
       apply 'entry' to v_dat_lancto in frame f-pg-par.                
       return error.
    end.

    find rh_bco no-lock where
         rh_bco.cdn_banco = input frame f-pg-par v_cdn_banco no-error.
    if not avail rh_bco then do:
       {utp/ut-table.i dthrpyc rh_bco 1}
        run utp/ut-msgs.p (input "show", input 56, input trim(return-value)).
        apply 'mouse-select-click' to im-pg-par in frame f-relat.
        apply 'entry' to v_cdn_banco in frame f-pg-par.
        return error.
    end.
    
    find rh_agenc_bcia no-lock where
         rh_agenc_bcia.cdn_banco      = input frame f-pg-par v_cdn_banco and
         rh_agenc_bcia.cdn_agenc_bcia = input frame f-pg-par v_cdn_agencia no-error.
    if not avail rh_agenc_bcia then do:
       {utp/ut-table.i dthrpyc rh_agenc_bcia 1}
       run utp/ut-msgs.p (input "show", input 56, input trim(return-value)).
       apply 'mouse-select-click' to im-pg-par in frame f-relat.
       apply 'entry' to v_cdn_agencia in frame f-pg-par.
       return error.
    end.  

    if input frame f-pg-par v_num_conta < 1 then do:
       {utp/ut-liter.i Conta_da_Empresa MFP C}
        run utp/ut-msgs.p (input "show",
                           input 54,
                           input return-value).
        apply 'mouse-select-click' to im-pg-par in frame f-relat.
        apply 'entry' to v_num_conta in frame f-pg-par.
        return error.
    end.

                 
    create tt-param.
    assign tt-param.usuario              = v_cod_usuar_corren
           tt-param.destino              = input frame f-pg-imp rs-destino
           tt-param.data-exec            = today
           tt-param.hora-exec            = time
           tt-param.v_cdn_empres_usuar   = v_cdn_empres_usuar
           tt-param.v_num_tip_aces_usuar = v_num_tip_aces_usuar
           tt-param.classifica           = input frame f-pg-cla rs-classif
           tt-param.desc-classifica      = entry((tt-param.classifica - 1) * 2 + 1, 
                                                  rs-classif:radio-buttons in frame f-pg-cla).
    if  tt-param.destino = 1 then
        assign tt-param.arquivo = "".
    else
    if  tt-param.destino = 2 then 
        assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
    else
        assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".

    /* Coloque aqui a l¢gica de gravaá∆o dos parÉmtros e seleá∆o na temp-table
       tt-param */ 
   
   
   assign  tt-param.c-arq-lqd       = input frame f-pg-par c-arq-lqd
           tt-param.i-es-ini        = input frame f-pg-sel i-es-ini 
           tt-param.i-es-fim        = input frame f-pg-sel i-es-fim
           tt-param.i-ban-liq-ini   = input frame f-pg-sel i-ban-liq-ini
           tt-param.i-ban-liq-fin   = input frame f-pg-sel i-ban-liq-fin
           tt-param.i-fc-ini        = INPUT FRAME f-pg-sel v_matr_ini
           tt-param.i-fc-fim        = INPUT FRAME f-pg-sel v_matr_fim  
           tt-param.da-pgto-ini     = input frame f-pg-sel da-pgto-ini
           tt-param.da-pgto-fin     = input frame f-pg-sel da-pgto-fin
           tt-param.l-mensal        = input frame f-pg-sel l-mensal
           tt-param.l-horista       = input frame f-pg-sel l-horista
           tt-param.l-semanal       = input frame f-pg-sel l-semanal
           tt-param.l-quinzenal     = input frame f-pg-sel l-quinzenal
           tt-param.l-tarefa        = input frame f-pg-sel l-tarefa
           tt-param.l-diarista      = input frame f-pg-sel l-diarista
           tt-param.cdn_estab_centr = input frame f-pg-par v_cdn_estab_centr 
           tt-param.cdn_banco       = input frame f-pg-par v_cdn_banco
           tt-param.cdn_agencia     = input frame f-pg-par v_cdn_agencia
           tt-param.cod_dv_agencia  = input frame f-pg-par v_cod_dv_agencia
           tt-param.num_conta       = input frame f-pg-par v_num_conta
           tt-param.cod_dv_conta    = input frame f-pg-par v_cod_dv
           tt-param.num_convenio    = input frame f-pg-par v_num_convenio
           tt-param.cod_docto       = input frame f-pg-par v_cod_docto
           tt-param.dat_lancto      = input frame f-pg-par v_dat_lancto
           tt-param.log_cred_conta  = input frame f-pg-par v_log_cred_conta
           tt-param.log_doc         = input frame f-pg-par v_log_doc
           tt-param.log_cheque      = input frame f-pg-par v_log_cheque
           tt-param.log_cartao      = input frame f-pg-par v_log_cartao
           tt-param.log_conta_poup  = input frame f-pg-par v_log_conta_poup
           tt-param.parametro       = input frame f-pg-imp tb-parametro
           tt-param.LOG_gera_seg_b  = INPUT FRAME f-pg-par v_log_gera_seg_b
           tt-param.num_seq         = input frame f-pg-par v_num_seq.

   MESSAGE "Confirma os dados do banco?" SKIP
             "Banco:" tt-param.cdn_banco SKIP
             "Agància: " tt-param.cdn_agencia "-" tt-param.cod_dv_agencia  SKIP
             "Conta Corrente:" tt-param.num_conta  "-"   tt-param.cod_dv_conta SKIP
             "Convànio:" tt-param.num_convenio
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-ok AS LOG.

    IF NOT l-ok THEN DO:
        apply 'mouse-select-click' to im-pg-par in frame f-relat.
        apply 'entry' to v_cdn_banco in frame f-pg-par.                
        return error.
    END.       
    
    {include/i-rpexb.i}

    if  session:set-wait-state("general") then.

    {include/i-rprun.i prghur/esp/ymac0003rp.p}

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
  Purpose: Gerencia a Troca de P†gina (folder)   
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


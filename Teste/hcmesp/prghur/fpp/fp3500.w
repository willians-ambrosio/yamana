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
{include/i-prgvrs.i FP3500 12.01.19.000 } /*** 010458 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i fp3500 MFP}
&ENDIF

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
/* Obs: Retirar o valor do preprocessador para as p ginas que nÆo existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA f-pg-cla
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG 
&GLOBAL-DEFINE PGIMP f-pg-imp

&GLOBAL-DEFINE btmais yes
&GLOBAL-DEFINE segur  yes

/* Include Com as Vari veis Globais */

/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */
&if "{&segur}" = "yes" &then
{prghur/fpp/fp9200.i}
&endif
&global-define param_run_terc 1
{prghur/fpp/fp3500tt.i}

define buffer b-tt-digita for tt-digita.

/* Transfer Definitions */

 {include/i-func-dtype.i}
def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.

/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var v_num_aux_classif  as integer no-undo.
def var v-clas             as integer no-undo.
def var wh-label         as widget-handle no-undo.
/* Begins Rev05 */
DEF VAR l-email            AS LOG    NO-UNDO.

DEF NEW GLOBAL SHARED VAR g-texto-email        AS CHAR                NO-UNDO.
DEF NEW GLOBAL SHARED VAR g-arquivo-imagem     AS CHAR FORMAT "x(12)" NO-UNDO.
/* end rev05 */
{prghur/fpp/fp9200.i7}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-cla

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cb-classif 
&Scoped-Define DISPLAYED-OBJECTS cb-classif 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cb-classif AS CHARACTER FORMAT "X(256)":U INITIAL "Por Estabelecimento/Matr¡cula" 
     VIEW-AS COMBO-BOX INNER-LINES 18
     DROP-DOWN-LIST
     SIZE 47 BY 1 NO-UNDO.

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

DEFINE BUTTON bt-mais 
     LABEL "Mais" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE v_dat_refer AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Referˆncia" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE v_des_arquivo_imagem AS CHARACTER FORMAT "X(200)":U 
     LABEL "Arquivo Logomarca (PDF)" 
     VIEW-AS FILL-IN 
     SIZE 45 BY .88 NO-UNDO.

DEFINE VARIABLE v_num_opcao_quebra AS INTEGER FORMAT "z9":U INITIAL 0 
     LABEL "Quebra" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE v_num_opcao_salta AS INTEGER FORMAT "z9":U INITIAL 0 
     LABEL "Salta P gina" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE v_envia_email AS LOGICAL INITIAL no 
     LABEL "Enviar Email" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .83 NO-UNDO.

DEFINE VARIABLE v_log_expande_estrut AS LOGICAL INITIAL no 
     LABEL "Expande Estrutura" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.86 BY .83 NO-UNDO.

DEFINE VARIABLE i-ag-codigo-1 AS INTEGER FORMAT "zzz9":U INITIAL 0 
     LABEL "Agˆncia" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE i-ag-codigo-2 AS INTEGER FORMAT "zzz9":U INITIAL 9999 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE i-bc-codigo-1 AS INTEGER FORMAT "zz9":U INITIAL 0 
     LABEL "Banco" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE i-bc-codigo-2 AS INTEGER FORMAT "zz9":U INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE i-cc-codigo-1 AS CHARACTER FORMAT "x(20)" 
     LABEL "Centro Custo":R15 
     VIEW-AS FILL-IN 
     SIZE 21.14 BY .88 NO-UNDO.

DEFINE VARIABLE i-cc-codigo-2 AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 21.14 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_estab_fim LIKE rh_estab.cdn_estab
     VIEW-AS FILL-IN 
     SIZE 6.57 BY 1 NO-UNDO.

DEFINE VARIABLE v_cdn_estab_ini LIKE rh_estab.cdn_estab
     VIEW-AS FILL-IN 
     SIZE 6.57 BY 1 NO-UNDO.

DEFINE VARIABLE v_cdn_funcionario_fim LIKE funcionario.cdn_funcionario
     VIEW-AS FILL-IN 
     SIZE 10.29 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_funcionario_ini LIKE funcionario.cdn_funcionario
     VIEW-AS FILL-IN 
     SIZE 10.29 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_local_pagto_fim AS DECIMAL FORMAT ">>>>>>>>>>9" INITIAL 99999999999 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88.

DEFINE VARIABLE v_cdn_local_pagto_ini AS DECIMAL FORMAT ">>>>>>>>>>9" INITIAL 0 
     LABEL "Local Pgto":R12 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88.

DEFINE VARIABLE v_cod_unid_lotac_fim AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE v_cod_unid_lotac_ini AS CHARACTER FORMAT "x(20)" 
     LABEL "Unidade Lota‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE v_nom_func_fim AS CHARACTER FORMAT "X(40)":U INITIAL "Zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz" 
     VIEW-AS FILL-IN 
     SIZE 25 BY .88 NO-UNDO.

DEFINE VARIABLE v_nom_func_ini AS CHARACTER FORMAT "X(40)":U 
     LABEL "Nome" 
     VIEW-AS FILL-IN 
     SIZE 25 BY .88 NO-UNDO.

DEFINE VARIABLE v_num_opcao_faixa AS INTEGER FORMAT "z9":U INITIAL 0 
     LABEL "N¡vel Unid Lotac" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-30
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-35
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-36
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-37
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-38
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-42
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-43
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-44
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-45
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-46
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-47
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-48
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-49
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-50
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-51
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-52
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-terceiros 
     LABEL "&Terceiros" 
     SIZE 10 BY 1 TOOLTIP "Informa‡äes Complementares de Terceiros".

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
          "Cancelar"
     bt-terceiros AT ROW 14.54 COL 25
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder-top AT ROW 2.54 COL 2.14
     RECT-6 AT ROW 13.75 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     rt-folder-right AT ROW 2.67 COL 80.43
     im-pg-cla AT ROW 1.5 COL 2.14
     im-pg-imp AT ROW 1.5 COL 49.29
     im-pg-par AT ROW 1.5 COL 17.86
     im-pg-sel AT ROW 1.5 COL 33.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81.29 BY 15.08
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-cla
     cb-classif AT ROW 2.08 COL 15.72 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.75
         SIZE 76.86 BY 10.88.

DEFINE FRAME f-pg-sel
     v_cdn_estab_ini AT ROW 2 COL 18 COLON-ALIGNED HELP
          ""
     v_cdn_estab_fim AT ROW 2 COL 50.29 COLON-ALIGNED HELP
          "" NO-LABEL
     v_cdn_funcionario_ini AT ROW 3 COL 18 COLON-ALIGNED HELP
          ""
     v_cdn_funcionario_fim AT ROW 3 COL 50.29 COLON-ALIGNED HELP
          "" NO-LABEL
     v_nom_func_ini AT ROW 4 COL 18 COLON-ALIGNED
     v_nom_func_fim AT ROW 4 COL 50.29 COLON-ALIGNED NO-LABEL
     v_num_opcao_faixa AT ROW 5 COL 18 COLON-ALIGNED
     v_cod_unid_lotac_ini AT ROW 6 COL 18 COLON-ALIGNED
     v_cod_unid_lotac_fim AT ROW 6 COL 50.29 COLON-ALIGNED NO-LABEL
     i-cc-codigo-1 AT ROW 7 COL 18 COLON-ALIGNED
     i-cc-codigo-2 AT ROW 7 COL 50.29 COLON-ALIGNED NO-LABEL
     i-bc-codigo-1 AT ROW 8 COL 18 COLON-ALIGNED
     i-bc-codigo-2 AT ROW 8 COL 50.29 COLON-ALIGNED NO-LABEL
     i-ag-codigo-1 AT ROW 9 COL 18 COLON-ALIGNED
     i-ag-codigo-2 AT ROW 9 COL 50.29 COLON-ALIGNED NO-LABEL
     v_cdn_local_pagto_ini AT ROW 10 COL 18 COLON-ALIGNED HELP
          "Local Pagamento Inicial"
     v_cdn_local_pagto_fim AT ROW 10 COL 50.29 COLON-ALIGNED HELP
          "Local Pagamento Final" NO-LABEL
     IMAGE-1 AT ROW 8 COL 44.86
     IMAGE-30 AT ROW 9 COL 44.86
     IMAGE-35 AT ROW 7 COL 44.86
     IMAGE-36 AT ROW 9 COL 49.86
     IMAGE-37 AT ROW 7 COL 49.86
     IMAGE-38 AT ROW 8 COL 49.86
     IMAGE-42 AT ROW 6 COL 44.86
     IMAGE-43 AT ROW 6 COL 49.86
     IMAGE-44 AT ROW 3 COL 44.86
     IMAGE-45 AT ROW 3 COL 49.86
     IMAGE-46 AT ROW 2 COL 44.86
     IMAGE-47 AT ROW 2 COL 49.86
     IMAGE-48 AT ROW 4 COL 44.86
     IMAGE-49 AT ROW 4 COL 49.86
     IMAGE-50 AT ROW 5 COL 44.86
     IMAGE-51 AT ROW 10 COL 44.86
     IMAGE-52 AT ROW 10 COL 49.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.92.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 4 COL 17 HELP
          "Destino de ImpressÆo do Relat¢rio" NO-LABEL
     bt-config-impr AT ROW 5.17 COL 57.14 HELP
          "Configura‡Æo da impressora"
     bt-arquivo AT ROW 5.17 COL 57.14 HELP
          "Escolha do Nome do arquivo"
     c-arquivo AT ROW 5.21 COL 17 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 7.38 COL 17 HELP
          "Modo de Execu‡Æo" NO-LABEL
     text-destino AT ROW 3.25 COL 16 COLON-ALIGNED NO-LABEL
     text-modo AT ROW 6.58 COL 16 COLON-ALIGNED NO-LABEL
     RECT-7 AT ROW 3.54 COL 16
     RECT-9 AT ROW 6.92 COL 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10.

DEFINE FRAME f-pg-par
     v_dat_refer AT ROW 2.67 COL 24.43 COLON-ALIGNED
     v_log_expande_estrut AT ROW 3.67 COL 26.43
     v_num_opcao_salta AT ROW 4.67 COL 24.43 COLON-ALIGNED
     v_num_opcao_quebra AT ROW 5.67 COL 24.43 COLON-ALIGNED
     v_envia_email AT ROW 6.75 COL 26.57
     v_des_arquivo_imagem AT ROW 7.71 COL 24.43 COLON-ALIGNED
     bt-mais AT ROW 9.75 COL 57 HELP
          "Mais Parƒmetros"
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 76.29 BY 10.46.


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
         HEIGHT             = 15.17
         WIDTH              = 81.43
         MAX-HEIGHT         = 27.96
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 27.96
         VIRTUAL-WIDTH      = 146.29
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
{utp/ut-glob.i}
{prghur/fpp/fp9300.i4}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-pg-cla
   FRAME-NAME                                                           */
/* SETTINGS FOR COMBO-BOX cb-classif IN FRAME f-pg-cla
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME f-pg-imp
                                                                        */
/* SETTINGS FOR FILL-IN text-modo IN FRAME f-pg-imp
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR TOGGLE-BOX v_log_expande_estrut IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v_num_opcao_quebra IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v_num_opcao_salta IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FILL-IN v_cdn_estab_fim IN FRAME f-pg-sel
   LIKE = dthrpyc.rh_estab.cdn_estab EXP-SIZE                           */
/* SETTINGS FOR FILL-IN v_cdn_estab_ini IN FRAME f-pg-sel
   LIKE = dthrpyc.rh_estab.cdn_estab EXP-SIZE                           */
/* SETTINGS FOR FILL-IN v_cdn_funcionario_fim IN FRAME f-pg-sel
   LIKE = dthrpyc.funcionario.cdn_funcionario EXP-SIZE                  */
/* SETTINGS FOR FILL-IN v_cdn_funcionario_ini IN FRAME f-pg-sel
   LIKE = dthrpyc.funcionario.cdn_funcionario EXP-SIZE                  */
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
    IF AVAIL tt-param AND                                                   
             tt-param.destino = 3 and search(tt-param.arquivo-pdf) <> ? then
      os-delete value(tt-param.arquivo-pdf).                                
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


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar C-Win
ON CHOOSE OF bt-cancelar IN FRAME f-relat /* Cancelar */
DO:
   apply "WINDOW-CLOSE" to c-win.
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


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME bt-mais
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-mais C-Win
ON CHOOSE OF bt-mais IN FRAME f-pg-par /* Mais */
DO:
  c-win:sensitive = no.
  run prghur/fpp/fp3500a.w (input-output table tt-param,
                            input v_cdn_estab_par,
                            input frame f-pg-par v_dat_refer).
  c-win:sensitive = yes.
  apply "entry" to bt-executar in frame f-relat.

  FOR FIRST tt-param no-lock:
      IF tt-param.i-tipo-formula = 5 or
         tt-param.i-tipo-formula = 6 THEN DO: /*PDF*/
          rs-destino:DISABLE("Impressora") in FRAME f-pg-imp.
          assign c-arquivo = replace(c-arquivo,".LST":U, ".PDF":U)
                 c-arq-old = replace(c-arq-old,".LST":U, ".PDF":U)
                 c-arq-old-batch = replace(c-arq-old-batch,".LST":U, ".PDF":U)
                 c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = c-arquivo
                 v_des_arquivo_imagem:SENSITIVE IN FRAME f-pg-par = YES
                 v_envia_email:CHECKED IN FRAME f-pg-par = NO
                 v_envia_email:SENSITIVE IN FRAME f-pg-par = NO.
      END.
      ELSE DO:
          rs-destino:ENABLE("Impressora") in FRAME f-pg-imp.
          assign c-arquivo = replace(c-arquivo,".PDF":U, ".LST":U)
                 c-arq-old = replace(c-arq-old,".PDF":U, ".LST":U)
                 c-arq-old-batch = replace(c-arq-old-batch,".PDF":U, ".LST":U)
                 c-arquivo:SCREEN-VALUE IN FRAME f-pg-imp = c-arquivo
                 v_des_arquivo_imagem:SENSITIVE IN FRAME f-pg-par = NO
                 v_envia_email:SENSITIVE IN FRAME f-pg-par = YES.
      END.
      APPLY "VALUE-CHANGED" TO v_envia_email IN FRAME f-pg-par.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-terceiros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-terceiros C-Win
ON CHOOSE OF bt-terceiros IN FRAME f-relat /* Terceiros */
DO:
  {prghur/fpp/fp9240.i12}
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
&Scoped-define SELF-NAME v_envia_email
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_envia_email C-Win
ON VALUE-CHANGED OF v_envia_email IN FRAME f-pg-par /* Enviar Email */
DO:
  IF INPUT FRAME f-pg-par v_envia_email THEN DO:
      {utp/ut-liter.i Log * C}
      create text wh-label
      assign frame        = frame f-relat:handle
             format       = "x(09)"
             font         = 1
             screen-value = trim(return-value)
             width        = 8
             row          = 1.8
             col          = im-pg-imp:col in frame f-relat + 1.86
             visible      = yes
      triggers:
          on  mouse-select-click
              apply "mouse-select-click":U to im-pg-imp in frame f-relat.
      end triggers.
   END.
   ELSE DO:
       {utp/ut-liter.i ImpressÆo * C}
      create text wh-label
      assign frame        = frame f-relat:handle
             format       = "x(09)"
             font         = 1
             screen-value = trim(return-value)
             width        = 8
             row          = 1.8
             col          = im-pg-imp:col in frame f-relat + 1.86
             visible      = yes
      triggers:
          on  mouse-select-click
              apply "mouse-select-click":U to im-pg-imp in frame f-relat.
      end triggers.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-cla
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "FP3500" "12.01.19.050"}


/* inicializa‡äes do Template de Relat¢rio */
{include/i-rpini.i}

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

  {include/i-rplbl.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.


IF SEARCH(session:temp-directory + "arq_conf_fp3500.tmp") <> ? THEN DO:
    INPUT FROM value(session:temp-directory + "arq_conf_fp3500.tmp").
    IMPORT v_des_arquivo_imagem.
    INPUT CLOSE.
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */

/*{prghur/fpp/fp9200.i19 3 4 0 0}*/

ON MOUSE-SELECT-CLICK OF im-pg-par IN FRAME f-relat
DO:
  run pi-troca-pagina.
  &if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
      if not(input frame f-pg-cla cb-classif = entry(06,cb-classif:list-item-pairs) or 
             input frame f-pg-cla cb-classif = entry(08,cb-classif:list-item-pairs) or
             input frame f-pg-cla cb-classif = entry(10,cb-classif:list-item-pairs) or
             input frame f-pg-cla cb-classif = entry(05,cb-classif:list-item-pairs) or
             input frame f-pg-cla cb-classif = entry(04,cb-classif:list-item-pairs) or
             input frame f-pg-cla cb-classif = entry(03,cb-classif:list-item-pairs) or
             input frame f-pg-cla cb-classif = entry(12,cb-classif:list-item-pairs)) then
         assign v_num_opcao_salta:screen-value in frame f-pg-par  = "1" 
                v_num_opcao_quebra:screen-value in frame f-pg-par = "1"
                v_log_expande_estrut:sensitive in frame f-pg-par  = no
                v_num_opcao_salta:sensitive in frame f-pg-par     = no 
                v_num_opcao_quebra:sensitive in frame f-pg-par    = no.
      else 
         assign v_log_expande_estrut:sensitive in frame f-pg-par = yes
                v_num_opcao_salta:sensitive in frame f-pg-par    = yes 
                v_num_opcao_quebra:sensitive in frame f-pg-par   = yes.
  &else
      if not(input frame f-pg-cla cb-classif = entry(06,cb-classif:list-items) or 
             input frame f-pg-cla cb-classif = entry(08,cb-classif:list-items) or
             input frame f-pg-cla cb-classif = entry(10,cb-classif:list-items) or
             input frame f-pg-cla cb-classif = entry(05,cb-classif:list-items) or
             input frame f-pg-cla cb-classif = entry(04,cb-classif:list-items) or
             input frame f-pg-cla cb-classif = entry(03,cb-classif:list-items) or
             input frame f-pg-cla cb-classif = entry(12,cb-classif:list-items)) then
         assign v_num_opcao_salta:screen-value in frame f-pg-par  = "1" 
                v_num_opcao_quebra:screen-value in frame f-pg-par = "1"
                v_log_expande_estrut:sensitive in frame f-pg-par  = no
                v_num_opcao_salta:sensitive in frame f-pg-par     = no 
                v_num_opcao_quebra:sensitive in frame f-pg-par    = no.
      else 
         assign v_log_expande_estrut:sensitive in frame f-pg-par = yes
                v_num_opcao_salta:sensitive in frame f-pg-par    = yes 
                v_num_opcao_quebra:sensitive in frame f-pg-par   = yes.  
  &endif
  
END.

ON MOUSE-SELECT-CLICK OF im-pg-sel IN FRAME f-relat
DO:
  run pi-troca-pagina.
  &if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
      if not(input frame f-pg-cla cb-classif = entry(06,cb-classif:list-item-pairs) or 
             input frame f-pg-cla cb-classif = entry(08,cb-classif:list-item-pairs) or
             input frame f-pg-cla cb-classif = entry(10,cb-classif:list-item-pairs) or
             input frame f-pg-cla cb-classif = entry(12,cb-classif:list-item-pairs)) then
         assign v_num_opcao_faixa:screen-value in frame f-pg-sel = "1"
                v_num_opcao_faixa:sensitive in frame f-pg-sel    = no.
      else 
         assign v_num_opcao_faixa:sensitive in frame f-pg-sel = yes.
  &else
      if not(input frame f-pg-cla cb-classif = entry(05,cb-classif:list-items) or 
             input frame f-pg-cla cb-classif = entry(06,cb-classif:list-items) or 
             input frame f-pg-cla cb-classif = entry(08,cb-classif:list-items) or
             input frame f-pg-cla cb-classif = entry(10,cb-classif:list-items) or
             input frame f-pg-cla cb-classif = entry(12,cb-classif:list-items)) then
         assign v_num_opcao_faixa:screen-value in frame f-pg-sel = "1"
                v_num_opcao_faixa:sensitive in frame f-pg-sel    = no.
      else 
         assign v_num_opcao_faixa:sensitive in frame f-pg-sel = yes.  
  &endif

END.


MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    &if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
        DEFINE VARIABLE cAuxTraducao001 AS CHARACTER NO-UNDO.
        ASSIGN cAuxTraducao001 = {varinc/var10184.i 03}.
        RUN utp/ut-lstit.p (INPUT-OUTPUT cAuxTraducao001).
        ASSIGN  cb-classif:list-item-pairs in frame f-pg-cla = cAuxTraducao001 .
    &else
        ASSIGN cb-classif:list-items in frame f-pg-cla = {varinc/var10184.i 03}.
    &endif

    ASSIGN v_cdn_funcionario_ini = {prghur/dop/eng002.i}
           v_cdn_funcionario_fim = {prghur/dop/eng001.i}
           v_cdn_estab_ini = {prghur/dop/eng012.i}
           v_cdn_estab_fim = {prghur/dop/eng007.i}
           i-cc-codigo-2 = {prghur/dop/eng011.i}.

    RUN enable_UI.

    &if "{&segur}" = "yes" &then
    if v_num_tip_aces_usuar = 1 then do:
       {prghur/fpp/fp9200.i2}
    end.
    &endif
    {prghur/fpp/fp9200.i9}

    find param_folha_educnal no-lock where
         param_folha_educnal.cdn_empresa = v_cdn_empres_usuar no-error.
    if avail param_folha_educnal then
       assign v_cdn_funcionario_fim:screen-value in frame f-pg-sel = "999999"
              v_cdn_funcionario_ini:format in frame f-pg-sel = "zzzzz9"
              v_cdn_funcionario_fim:format in frame f-pg-sel = "zzzzz9".

    {prghur/fpp/fp9240.i9}
    {include/i-rpmbl.i im-pg-cla}

    ASSIGN v_des_arquivo_imagem:SENSITIVE IN FRAME f-pg-par = NO.

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
  ENABLE im-pg-cla im-pg-imp im-pg-par im-pg-sel bt-executar bt-cancelar 
         bt-terceiros bt-ajuda 
      WITH FRAME f-relat IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY cb-classif 
      WITH FRAME f-pg-cla IN WINDOW C-Win.
  ENABLE cb-classif 
      WITH FRAME f-pg-cla IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-cla}
  DISPLAY v_cdn_estab_ini v_cdn_estab_fim v_cdn_funcionario_ini 
          v_cdn_funcionario_fim v_nom_func_ini v_nom_func_fim v_num_opcao_faixa 
          v_cod_unid_lotac_ini v_cod_unid_lotac_fim i-cc-codigo-1 i-cc-codigo-2 
          i-bc-codigo-1 i-bc-codigo-2 i-ag-codigo-1 i-ag-codigo-2 
          v_cdn_local_pagto_ini v_cdn_local_pagto_fim 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE IMAGE-1 IMAGE-30 IMAGE-35 IMAGE-36 IMAGE-37 IMAGE-38 IMAGE-42 IMAGE-43 
         IMAGE-44 IMAGE-45 IMAGE-46 IMAGE-47 IMAGE-48 IMAGE-49 IMAGE-50 
         IMAGE-51 IMAGE-52 v_cdn_estab_ini v_cdn_estab_fim 
         v_cdn_funcionario_ini v_cdn_funcionario_fim v_nom_func_ini 
         v_nom_func_fim v_cod_unid_lotac_ini v_cod_unid_lotac_fim i-cc-codigo-1 
         i-cc-codigo-2 i-bc-codigo-1 i-bc-codigo-2 i-ag-codigo-1 i-ag-codigo-2 
         v_cdn_local_pagto_ini v_cdn_local_pagto_fim 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino c-arquivo rs-execucao text-destino text-modo 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  ENABLE RECT-7 RECT-9 rs-destino bt-config-impr bt-arquivo c-arquivo 
         rs-execucao text-destino 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY v_dat_refer v_log_expande_estrut v_num_opcao_salta v_num_opcao_quebra 
          v_envia_email v_des_arquivo_imagem 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  ENABLE v_dat_refer v_envia_email v_des_arquivo_imagem bt-mais 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
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

    if  input frame f-pg-imp rs-destino = 1 then do:
        run utp/ut-msgs.p (input "show",
                           input 3458,
                           input "").
        if return-value <> "yes" then do:
           apply 'entry' to bt-executar in frame f-relat.                   
           return error.
        end.
    end.    

  /*{prghur/fpp/fp9200.i3 4 3 4 0 0}*/

  &if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
      if (input frame f-pg-cla cb-classif = entry(06,cb-classif:list-item-pairs) or
          input frame f-pg-cla cb-classif = entry(08,cb-classif:list-item-pairs) or
          input frame f-pg-cla cb-classif = entry(10,cb-classif:list-item-pairs) or
          input frame f-pg-cla cb-classif = entry(12,cb-classif:list-item-pairs)) then do:
           if input frame f-pg-par v_num_opcao_salta < 1 then do:
              {utp/ut-liter.i Salta_Pÿgina MFP R}
              run utp/ut-msgs.p (input "show", input 36, input trim(return-value)).
              apply 'mouse-select-click' to im-pg-par in frame f-relat.
              apply "entry" to v_num_opcao_salta in frame f-pg-par.
              return error.
           end.
           if input frame f-pg-par v_num_opcao_quebra < 1 then do:
              {utp/ut-liter.i Quebra MFP R}
              run utp/ut-msgs.p (input "show", input 36, input trim(return-value)).
              apply 'mouse-select-click' to im-pg-par in frame f-relat.
              apply "entry" to v_num_opcao_quebra in frame f-pg-par.
              return error.
           end.
           if input frame f-pg-sel v_num_opcao_faixa < 1 then do:
              {utp/ut-liter.i N­vel_Unidade_de_Lota»’o MFP R}
              run utp/ut-msgs.p (input "show", input 36, input trim(return-value)).
              apply 'mouse-select-click' to im-pg-sel in frame f-relat.
              apply "entry" to v_num_opcao_faixa in frame f-pg-sel.
              return error.
           end.
    
           if input frame f-pg-par v_num_opcao_quebra < input frame f-pg-sel v_num_opcao_faixa then do:
              {utp/ut-liter.i de_Faixa MFP R}
              run utp/ut-msgs.p (input "show", input 7915, input trim(return-value)).
              apply 'mouse-select-click' to im-pg-par in frame f-relat.
              apply "entry" to v_num_opcao_quebra in frame f-pg-par.
              return error.
           end.
           if input frame f-pg-par v_num_opcao_quebra < input frame f-pg-par v_num_opcao_salta then do:
              {utp/ut-liter.i Salta_Pÿgina MFP R}
              run utp/ut-msgs.p (input "show", input 7915, input trim(return-value)).
              apply 'mouse-select-click' to im-pg-par in frame f-relat.
              apply "entry" to v_num_opcao_quebra in frame f-pg-par.
              return error.
           end.
       end.
   &else
      if (input frame f-pg-cla cb-classif = entry(06,cb-classif:list-items) or
          input frame f-pg-cla cb-classif = entry(08,cb-classif:list-items) or
          input frame f-pg-cla cb-classif = entry(10,cb-classif:list-items) or
          input frame f-pg-cla cb-classif = entry(12,cb-classif:list-items)) then do:
           if input frame f-pg-par v_num_opcao_salta < 1 then do:
              {utp/ut-liter.i Salta_Pÿgina MFP R}
              run utp/ut-msgs.p (input "show", input 36, input trim(return-value)).
              apply 'mouse-select-click' to im-pg-par in frame f-relat.
              apply "entry" to v_num_opcao_salta in frame f-pg-par.
              return error.
           end.
           if input frame f-pg-par v_num_opcao_quebra < 1 then do:
              {utp/ut-liter.i Quebra MFP R}
              run utp/ut-msgs.p (input "show", input 36, input trim(return-value)).
              apply 'mouse-select-click' to im-pg-par in frame f-relat.
              apply "entry" to v_num_opcao_quebra in frame f-pg-par.
              return error.
           end.
           if input frame f-pg-sel v_num_opcao_faixa < 1 then do:
              {utp/ut-liter.i N­vel_Unidade_de_Lota»’o MFP R}
              run utp/ut-msgs.p (input "show", input 36, input trim(return-value)).
              apply 'mouse-select-click' to im-pg-sel in frame f-relat.
              apply "entry" to v_num_opcao_faixa in frame f-pg-sel.
              return error.
           end.
    
           if input frame f-pg-par v_num_opcao_quebra < input frame f-pg-sel v_num_opcao_faixa then do:
              {utp/ut-liter.i de_Faixa MFP R}
              run utp/ut-msgs.p (input "show", input 7915, input trim(return-value)).
              apply 'mouse-select-click' to im-pg-par in frame f-relat.
              apply "entry" to v_num_opcao_quebra in frame f-pg-par.
              return error.
           end.
           if input frame f-pg-par v_num_opcao_quebra < input frame f-pg-par v_num_opcao_salta then do:
              {utp/ut-liter.i Salta_Pÿgina MFP R}
              run utp/ut-msgs.p (input "show", input 7915, input trim(return-value)).
              apply 'mouse-select-click' to im-pg-par in frame f-relat.
              apply "entry" to v_num_opcao_quebra in frame f-pg-par.
              return error.
           end.
       end.   
   &endif

   &IF "{&BTMAIS}" = "yes" &THEN
       FIND FIRST tt-param NO-ERROR.   
       IF NOT AVAIL tt-param then do:
          {utp/ut-liter.i Mais *}
          run utp/ut-msgs.p (input "show", input 7927, input return-value).
          apply 'mouse-select-click' to im-pg-par in frame f-relat.
          apply "entry" to bt-mais in frame f-pg-par.
          return error.
       END.
   &ENDIF
   &IF "{&BTMAIS}" = "no" &THEN
       FIND FIRST tt-param NO-ERROR.   
       IF NOT AVAIL tt-param then 
          create tt-param.
   &ENDIF

    assign tt-param.usuario               = v_cod_usuar_corren
           tt-param.destino               = input frame f-pg-imp rs-destino
           tt-param.data-exec             = today
           tt-param.hora-exec             = time
           tt-param.v_cdn_empres_usuar    = v_cdn_empres_usuar
           tt-param.v_cdn_empresa_evento  = v_cdn_empresa_evento
           tt-param.v_cod_grp_usuar       = v_cod_grp_usuar_lst
           tt-param.v_num_opcao           = 0
           tt-param.v_des_opcao           = ""
           tt-param.v_dat_valid           = input frame f-pg-par v_dat_refer
           tt-param.i-mes-ref             = month(input frame f-pg-par v_dat_refer)
           tt-param.i-ano-ref             = year(input frame f-pg-par v_dat_refer)
           tt-param.v_log_expande_estrut  = input frame f-pg-par v_log_expande_estrut
           tt-param.i-es-ini              = input frame f-pg-sel v_cdn_estab_ini
           tt-param.i-es-fim              = input frame f-pg-sel v_cdn_estab_fim
           &if "{&emit_func}" = "no" &THEN
           tt-param.i-fc-ini              = 0
           tt-param.i-fc-fim              = 99999999
           &else
           tt-param.i-fc-ini              = input frame f-pg-sel v_cdn_funcionario_ini
           tt-param.i-fc-fim              = input frame f-pg-sel v_cdn_funcionario_fim
           &endif
           tt-param.v_cod_unid_lotac_ini  = input frame f-pg-sel v_cod_unid_lotac_ini
           tt-param.v_cod_unid_lotac_fim  = input frame f-pg-sel v_cod_unid_lotac_fim
           tt-param.v_num_salta_pg        = input frame f-pg-par v_num_opcao_salta
           tt-param.v_num_quebra          = input frame f-pg-par v_num_opcao_quebra
           tt-param.v_num_faixa           = input frame f-pg-sel v_num_opcao_faixa
           tt-param.v_log_enviar_email    = INPUT FRAME f-pg-par v_envia_email
           tt-param.l-parametro           = NO
           tt-param.arquivo-imagem        = input frame f-pg-par v_des_arquivo_imagem.

        &if "{&segur}" = "yes" &then
           tt-param.v_num_tip_aces_usuar  = v_num_tip_aces_usuar.
        &else
           tt-param.v_num_tip_aces_usuar  = 0.
        &endif

    assign input frame f-pg-cla cb-classif
           tt-param.classifica           = {varinc/var10184.i 06 cb-classif}    
    .
    &if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
        DEFINE VARIABLE cAuxTraducao002 AS CHARACTER NO-UNDO.
        ASSIGN cAuxTraducao002 = {varinc/var10184.i 04 tt-param.classifica}.
        run utp/ut-liter.p (INPUT REPLACE(TRIM(cAuxTraducao002)," ","_"),
                            INPUT "",
                            INPUT "").
        ASSIGN tt-param.desc-classifica      = RETURN-VALUE.
    &else
        ASSIGN tt-param.desc-classifica      = {varinc/var10184.i 04 tt-param.classifica}.
    &endif

    if tt-param.destino = 1 then
        assign tt-param.arquivo = "".
    else
    if tt-param.destino = 2 then DO:
        assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
    END.
    ELSE DO:
        assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".
    END.

    IF tt-param.i-tipo-formula = 5 or
       tt-param.i-tipo-formula = 6 THEN DO: /* PDF */
        if tt-param.destino = 2 then DO:
            assign tt-param.arquivo     = replace(input frame f-pg-imp c-arquivo,".pdf",".tmp") /* session:temp-directory + c-programa-mg97 + ".tmp" */ 
                   tt-param.arquivo-pdf = input frame f-pg-imp c-arquivo.
            if input frame f-pg-imp rs-execucao = 2 then
                assign tt-param.arquivo     = c-programa-mg97 + ".tmp"
                       tt-param.arquivo-pdf = input frame f-pg-imp c-arquivo.
            
        END.
        ELSE IF tt-param.destino = 3 THEN DO:
            assign tt-param.arquivo-pdf = session:temp-directory + c-programa-mg97 + ".pdf".
        END.
        IF SEARCH(tt-param.arquivo-imagem) <> ? THEN DO:
            OUTPUT TO VALUE(session:temp-directory + "arq_conf_fp3500.tmp").
            EXPORT tt-param.arquivo-imagem SKIP(1).
            OUTPUT CLOSE.
        END.
    END.

  /* Coloque aqui a l¢gica de grava‡Æo dos parƒmtros e sele‡Æo na temp-table
     tt-param */ 

   assign  tt-param.i-cc-codigo-1           = input frame f-pg-sel i-cc-codigo-1
           tt-param.i-cc-codigo-2           = input frame f-pg-sel i-cc-codigo-2
           tt-param.i-bc-codigo-1           = input frame f-pg-sel i-bc-codigo-1
           tt-param.i-bc-codigo-2           = input frame f-pg-sel i-bc-codigo-2
           tt-param.i-ag-codigo-1           = input frame f-pg-sel i-ag-codigo-1
           tt-param.i-ag-codigo-2           = input frame f-pg-sel i-ag-codigo-2
           tt-param.v_nom_func_ini          = input frame f-pg-sel v_nom_func_ini
           tt-param.v_nom_func_fim          = input frame f-pg-sel v_nom_func_fim
           tt-param.cdn_local_pagto_ini     = input frame f-pg-sel v_cdn_local_pagto_ini
           tt-param.cdn_local_pagto_fim     = input frame f-pg-sel v_cdn_local_pagto_fim
           tt-param.v_niv_unid_lotac        = INPUT FRAME f-pg-sel v_num_opcao_faixa
           tt-param.v_cod_unid_lotac_ini    = INPUT FRAME f-pg-sel v_cod_unid_lotac_ini
           tt-param.v_cod_unid_lotac_fim    = INPUT FRAME f-pg-sel v_cod_unid_lotac_fim
           tt-param.v_log_origem            = YES
           tt-param.tipo-execucao           = input frame f-pg-imp rs-execucao.

   RUN pi-grava-msg.
        
    MESSAGE
          "Deseja Enviar PDF por E-mail?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-email.
    
    IF l-email = YES THEN
       RUN prghur\fpp\fp3500r1b.p.

    ASSIGN tt-param.l-email-pdf    = l-email
           tt-param.texto-email    = g-texto-email
           tt-param.arquivo-imagem = g-arquivo-imagem.     

    {prghur/fpp/fp9240.i10}
    {include/i-rpexb.i}

    if  session:set-wait-state("general") then.

    {include/i-rprun.i prghur/fpp/fp3500rp.p}

    {include/i-rpexc.i}

    if  session:set-wait-state("") then.

    IF tt-param.i-tipo-formula <> 5 and
       tt-param.i-tipo-formula <> 6 THEN DO:
        {include/i-rptrm.i}
        {prghur/fpp/fp9240.i10}
    END.

end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava-msg C-Win 
PROCEDURE pi-grava-msg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN g-texto-email     =  "Prezado(a),"                                                                                                                            + CHR(10) +  CHR(10) +
                            "Segue envelope de Pagamento ref. ao mˆs " + string(month(tt-param.v_dat_valid),"99") + "/" + string(YEAR(tt-param.v_dat_valid),"9999")  + CHR(10) +  CHR(10) +
                            "Favor informar o CPF para abrir o arquivo em PDF."                                                                                      + CHR(10) +  CHR(10) +
                            "Recursos Humanos" + CHR(10) +  CHR(10) +
                            "Realize o ¢bvio, pense no improv vel e conquiste o imposs¡vel."
       g-arquivo-imagem  = "".

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


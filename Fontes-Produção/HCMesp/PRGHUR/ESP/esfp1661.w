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
{include/i-prgvrs.i ESFP1661 2.06.00.000}  /*** 010259 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i esfp1661 MFP}
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
/* Obs: Retirar o valor do preprocessador para as p†ginas que n∆o existirem  */

&GLOBAL-DEFINE PGSEL f-pg-sel
&GLOBAL-DEFINE PGCLA f-pg-cla
&GLOBAL-DEFINE PGPAR f-pg-par
&GLOBAL-DEFINE PGDIG f-pg-dig
&GLOBAL-DEFINE PGIMP f-pg-imp

&GLOBAL-DEFINE RTF YES

&GLOBAL-DEFINE btmais no
&GLOBAL-DEFINE segur  yes
{include/i-func-dtype.i} 

&if "{&segur}" = "yes" &then
{prghur/fpp/fp9200.i}
&endif
&global-define param_run_terc 1
{prghur/esp/esfp1661tt.i new shared}

/*define buffer b-tt-digita for tt-digita.*/

/* Transfer Definitions */

def var raw-param        as raw no-undo.

def temp-table tt-raw-digita
   field raw-digita      as raw.

/* Local Variable Definitions ---                                       */

def var l-ok               as logical no-undo.
def var c-arq-digita       as char    no-undo.
def var c-terminal         as char    no-undo.
def var c-modelo-default   as char    no-undo.
def var c-valida-arquivo   as char    no-undo.
def var v_tip_contr        as char format "x(13)" no-undo.
def var v_tip_contrat_ini  as char format "x(24)" no-undo.

{prghur/fpp/fp9200.i7}
{prghur/fpp/fp9300.i5}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-cla
&Scoped-define BROWSE-NAME br-digita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-digita

/* Definitions for BROWSE br-digita                                     */
&Scoped-define FIELDS-IN-QUERY-br-digita tt-digita.cdn_motiv_liber_sal tt-digita.des_motiv_liber_sal   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-digita.cdn_motiv_liber_sal   
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
&Scoped-Define ENABLED-OBJECTS RECT-103 rs-classif rs-tip-imp-motiv 
&Scoped-Define DISPLAYED-OBJECTS rs-classif rs-tip-imp-motiv 

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
          "Por Centro de Custo/Estabelecimento/Matr°cula", 1,
"Por Centro de Custo/Estabelecimento/Nome", 2,
"Por Estabelecimento/Matr°cula", 3,
"Por Estabelecimento/Nome", 4,
"Por Estabelecimento/Unidade Lotaá∆o/Matr°cula", 5,
"Por Estabelecimento/Unidade Lotaá∆o/Nome", 6,
"Por Estabelecimento/Turno/Matr°cula", 7,
"Por Estabelecimento/Turno/Nome", 8
     SIZE 57.86 BY 7.5 NO-UNDO.

DEFINE VARIABLE rs-tip-imp-motiv AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Por Seleá∆o", 1,
"Por Digitaá∆o", 2
     SIZE 31.72 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-103
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34 BY 1.5.

DEFINE BUTTON bt-inserir 
     LABEL "Inserir" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-retirar 
     LABEL "Retirar" 
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

DEFINE BUTTON bt-modelo-rtf 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE c-modelo-rtf AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE text-destino AS CHARACTER FORMAT "X(256)":U INITIAL " Destino" 
      VIEW-AS TEXT 
     SIZE 8.57 BY .63 NO-UNDO.

DEFINE VARIABLE text-modelo-rtf AS CHARACTER FORMAT "X(30)":U INITIAL "Modelo:" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE text-modo AS CHARACTER FORMAT "X(256)":U INITIAL "Execuá∆o" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .63 NO-UNDO.

DEFINE VARIABLE text-parametro AS CHARACTER FORMAT "X(256)":U INITIAL "ParÉmetros de Impress∆o" 
      VIEW-AS TEXT 
     SIZE 27 BY .63 NO-UNDO.

DEFINE VARIABLE text-rtf AS CHARACTER FORMAT "X(256)":U INITIAL "Rich Text Format(RTF)" 
      VIEW-AS TEXT 
     SIZE 20.86 BY .63 NO-UNDO.

DEFINE VARIABLE rs-destino AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impressora", 1,
"Arquivo", 2,
"Terminal", 3
     SIZE 44 BY .67 NO-UNDO.

DEFINE VARIABLE rs-execucao AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
"Batch", 2
     SIZE 27.72 BY .67 NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.46.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.33.

DEFINE RECTANGLE RECT-98
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.42.

DEFINE RECTANGLE RECT-RTF
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.92.

DEFINE VARIABLE l-habilitaRtf AS LOGICAL INITIAL no 
     LABEL "RTF":U 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY .71 NO-UNDO.

DEFINE VARIABLE tb-parametro AS LOGICAL INITIAL no 
     LABEL "Imprimir P†gina de ParÉmetros" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .83 NO-UNDO.

DEFINE VARIABLE v_dat_refer AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Referància" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE v_num_casas_dec AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "N£mero Casas Decimais" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE v_num_opcao_quebra AS INTEGER FORMAT "z9":U INITIAL 0 
     LABEL "Quebra" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE v_num_opcao_salta AS INTEGER FORMAT "z9":U INITIAL 0 
     LABEL "Salta P†gina" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE rs-imp-ult-mov-tip AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Cargo", 1,
"Sal†rio", 2,
"Ambos", 3
     SIZE 32.29 BY 1 NO-UNDO.

DEFINE VARIABLE v_idi_tip_cargo_funcao AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Cargo", 1,
"Funá∆o", 2,
"Ambos", 3
     SIZE 30 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-104
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 36 BY 1.5.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 32 BY 1.5.

DEFINE VARIABLE l-imp-desl AS LOGICAL INITIAL no 
     LABEL "Imprime Desligados" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.86 BY .92 NO-UNDO.

DEFINE VARIABLE v_log_expande_estrut AS LOGICAL INITIAL no 
     LABEL "Expande Estrutura" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.86 BY .83 NO-UNDO.

DEFINE VARIABLE v_log_imp_ult_movto AS LOGICAL INITIAL no 
     LABEL "Imprime Èltima Movimentaá∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 23.43 BY .83 NO-UNDO.

DEFINE VARIABLE c-cc-fim AS CHARACTER FORMAT "x(20)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE c-cc-ini AS CHARACTER FORMAT "x(20)" 
     LABEL "Centro Custo":R15 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE c-nm-fim AS CHARACTER FORMAT "x(10)" INITIAL "ZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE c-nm-ini AS CHARACTER FORMAT "x(10)" INITIAL "AAAAAAAAAA" 
     LABEL "Nome Abreviado":R17 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE d-dt-fim AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12.86 BY .88 NO-UNDO.

DEFINE VARIABLE d-dt-ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/1800 
     LABEL "Data de Corte":R17 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE i-mt-fim AS INTEGER FORMAT "zz9" INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 4.29 BY .88 NO-UNDO.

DEFINE VARIABLE i-mt-ini AS INTEGER FORMAT "999" INITIAL 0 
     LABEL "Motivo":R8 
     VIEW-AS FILL-IN 
     SIZE 4.43 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_cargo_fim AS INTEGER FORMAT "zzzz9" INITIAL 99999 
     VIEW-AS FILL-IN 
     SIZE 6.57 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_cargo_ini AS INTEGER FORMAT "zzzz9" INITIAL 0 
     LABEL "Cargo B†sico" 
     VIEW-AS FILL-IN 
     SIZE 6.57 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_estab_fim AS CHARACTER FORMAT "X(5)":U INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 7.29 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_estab_ini AS CHARACTER FORMAT "X(5)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 7.29 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_funcionario_fim LIKE funcionario.cdn_funcionario
     VIEW-AS FILL-IN 
     SIZE 10.29 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_funcionario_ini LIKE funcionario.cdn_funcionario
     LABEL "Funcion†rio":R11 
     VIEW-AS FILL-IN 
     SIZE 10.29 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_func_centrdor_fim AS INTEGER FORMAT "zzzzz9" INITIAL 999999 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_func_centrdor_ini AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Funcion†rio" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_tip_contrat_fim AS INTEGER FORMAT ">9":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 3.14 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_tip_contrat_ini AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Tipo Contrato" 
     VIEW-AS FILL-IN 
     SIZE 3.14 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_turno_fim AS INTEGER FORMAT "9999":U INITIAL 9999 
     VIEW-AS FILL-IN 
     SIZE 5.57 BY .88 NO-UNDO.

DEFINE VARIABLE v_cdn_turno_ini AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Turno" 
     VIEW-AS FILL-IN 
     SIZE 5.57 BY .88 NO-UNDO.

DEFINE VARIABLE v_cod_unid_lotac_fim AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE v_cod_unid_lotac_ini AS CHARACTER FORMAT "x(20)" 
     LABEL "Unidade Lotaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE v_num_opcao_faixa AS INTEGER FORMAT "z9":U INITIAL 0 
     LABEL "N°vel Unidade Lotaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-31
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-32
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-33
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-34
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

DEFINE IMAGE IMAGE-50
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-51
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-52
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-53
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

DEFINE IMAGE IMAGE-58
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-59
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-60
     FILENAME "image\im-fir":U
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
     SIZE 10 BY 1 TOOLTIP "InformaÁıes Complementares de Terceiros".

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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-digita C-Win _FREEFORM
  QUERY br-digita DISPLAY
      tt-digita.cdn_motiv_liber_sal
tt-digita.des_motiv_liber_sal
ENABLE
tt-digita.cdn_motiv_liber_sal
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 74 BY 9
         BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Cancelar"
     bt-terceiros AT ROW 14.54 COL 25
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-6 AT ROW 13.75 COL 2.14
     rt-folder AT ROW 2.5 COL 2
     RECT-1 AT ROW 14.29 COL 2
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-top AT ROW 2.54 COL 2.14
     im-pg-cla AT ROW 1.5 COL 2
     im-pg-dig AT ROW 1.5 COL 49.14
     im-pg-imp AT ROW 1.5 COL 64.86
     im-pg-par AT ROW 1.5 COL 17.72
     im-pg-sel AT ROW 1.5 COL 33.43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar.

DEFINE FRAME f-pg-imp
     rs-destino AT ROW 1.88 COL 17 HELP
          "Destino de Impress∆o do Relat¢rio" NO-LABEL
     bt-arquivo AT ROW 2.58 COL 57.14 HELP
          "Escolha do nome do arquivo"
     bt-config-impr AT ROW 2.58 COL 57.14 HELP
          "Configuraá∆o da impressora"
     c-arquivo AT ROW 2.67 COL 17 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     l-habilitaRtf AT ROW 4.58 COL 17.43 HELP
          "Impress∆o Formato RTF"
     bt-modelo-rtf AT ROW 5.88 COL 57.14 HELP
          "Escolha do Nome do Arquivo"
     c-modelo-rtf AT ROW 5.92 COL 17 HELP
          "Nome do Arquivo de Modelo do Relat¢rio" NO-LABEL
     rs-execucao AT ROW 7.88 COL 17 HELP
          "Modo de Execuá∆o" NO-LABEL
     tb-parametro AT ROW 9.58 COL 17.43
     text-destino AT ROW 1.13 COL 17.14 NO-LABEL
     text-rtf AT ROW 3.88 COL 15.29 COLON-ALIGNED NO-LABEL
     text-modelo-rtf AT ROW 5.29 COL 15.43 COLON-ALIGNED NO-LABEL
     text-modo AT ROW 7.13 COL 15.14 COLON-ALIGNED NO-LABEL
     text-parametro AT ROW 8.79 COL 17.43 NO-LABEL
     RECT-7 AT ROW 1.42 COL 16
     RECT-9 AT ROW 7.42 COL 16
     RECT-98 AT ROW 9.08 COL 16
     RECT-RTF AT ROW 4.21 COL 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.57 ROW 2.67
         SIZE 76.72 BY 10.54.

DEFINE FRAME f-pg-dig
     br-digita AT ROW 1.5 COL 2.57
     bt-inserir AT ROW 10.54 COL 2.57
     bt-retirar AT ROW 10.54 COL 17.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.75
         SIZE 76.86 BY 10.71.

DEFINE FRAME f-pg-cla
     rs-classif AT ROW 1.75 COL 9 HELP
          "Classificaá∆o para emiss∆o do relat¢rio" NO-LABEL
     rs-tip-imp-motiv AT ROW 10.33 COL 10.14 NO-LABEL
     "Classificaá∆o Motivo" VIEW-AS TEXT
          SIZE 14.29 BY .67 AT ROW 9.5 COL 9.72
     RECT-103 AT ROW 9.88 COL 9
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.57 ROW 2.71
         SIZE 77.72 BY 10.96.

DEFINE FRAME f-pg-par
     v_dat_refer AT ROW 2.63 COL 28.72 COLON-ALIGNED
     v_log_expande_estrut AT ROW 3.63 COL 30.72
     v_num_opcao_salta AT ROW 4.63 COL 28.72 COLON-ALIGNED
     v_num_opcao_quebra AT ROW 5.63 COL 28.72 COLON-ALIGNED
     l-imp-desl AT ROW 6.63 COL 30.72
     v_num_casas_dec AT ROW 7.63 COL 28.72 COLON-ALIGNED
     v_log_imp_ult_movto AT ROW 9 COL 5.57
     rs-imp-ult-mov-tip AT ROW 9.75 COL 6 NO-LABEL
     v_idi_tip_cargo_funcao AT ROW 9.67 COL 44 HELP
          "Seleá∆o ou Digitaá∆o" NO-LABEL
     "Tipo" VIEW-AS TEXT
          SIZE 7.14 BY .63 AT ROW 9.04 COL 44
     RECT-104 AT ROW 9.42 COL 4
     RECT-12 AT ROW 9.42 COL 43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.57 ROW 2.71
         SIZE 77.72 BY 10.96.

DEFINE FRAME f-pg-sel
     v_cdn_estab_ini AT ROW 1.13 COL 25.43 COLON-ALIGNED
     v_cdn_estab_fim AT ROW 1.13 COL 49.72 COLON-ALIGNED NO-LABEL
     v_cdn_func_centrdor_ini AT ROW 2.13 COL 25.43 COLON-ALIGNED
     v_cdn_funcionario_ini AT ROW 2.13 COL 25.43 COLON-ALIGNED HELP
          ""
          LABEL "Funcion†rio":R11
     v_cdn_func_centrdor_fim AT ROW 2.13 COL 49.43 COLON-ALIGNED NO-LABEL
     v_cdn_funcionario_fim AT ROW 2.13 COL 49.72 COLON-ALIGNED HELP
          "" NO-LABEL
     v_num_opcao_faixa AT ROW 3.13 COL 25.43 COLON-ALIGNED
     v_cod_unid_lotac_ini AT ROW 4.13 COL 25.43 COLON-ALIGNED
     v_cod_unid_lotac_fim AT ROW 4.13 COL 49.72 COLON-ALIGNED NO-LABEL
     c-cc-ini AT ROW 5.13 COL 25.43 COLON-ALIGNED HELP
          "C¢digo do centro de custo inicial"
     c-cc-fim AT ROW 5.13 COL 49.72 COLON-ALIGNED HELP
          "C¢digo do centro de custo final" NO-LABEL
     v_cdn_cargo_ini AT ROW 6.13 COL 25.43 COLON-ALIGNED
     v_cdn_cargo_fim AT ROW 6.13 COL 49.72 COLON-ALIGNED NO-LABEL
     v_cdn_turno_ini AT ROW 7.08 COL 25.43 COLON-ALIGNED
     v_cdn_turno_fim AT ROW 7.08 COL 49.72 COLON-ALIGNED NO-LABEL
     c-nm-ini AT ROW 8.08 COL 25.43 COLON-ALIGNED HELP
          "Nome abreviado inicial (exclusivo para classificaá∆o)"
     c-nm-fim AT ROW 8.08 COL 49.72 COLON-ALIGNED HELP
          "Nome abreviado final (exclusivo para classificaá∆o)" NO-LABEL
     i-mt-ini AT ROW 9.08 COL 25.43 COLON-ALIGNED HELP
          "C¢digo do motivo de alteraá∆o salarial inicial"
     i-mt-fim AT ROW 9.08 COL 49.72 COLON-ALIGNED HELP
          "C¢digo do motivo de alteraá∆o salarial final" NO-LABEL
     d-dt-ini AT ROW 10.08 COL 25.43 COLON-ALIGNED HELP
          "Data de corte inicial"
     d-dt-fim AT ROW 10.08 COL 49.72 COLON-ALIGNED HELP
          "Data de corte final" NO-LABEL
     v_cdn_tip_contrat_ini AT ROW 11 COL 25.43 COLON-ALIGNED
     v_cdn_tip_contrat_fim AT ROW 11 COL 49.72 COLON-ALIGNED NO-LABEL
     IMAGE-1 AT ROW 1.13 COL 40.57
     IMAGE-2 AT ROW 1.13 COL 48.86
     IMAGE-31 AT ROW 9.08 COL 40.57
     IMAGE-32 AT ROW 7.08 COL 48.86
     IMAGE-33 AT ROW 10.08 COL 40.57
     IMAGE-34 AT ROW 10.08 COL 48.86
     IMAGE-42 AT ROW 4.13 COL 40.57
     IMAGE-43 AT ROW 4.13 COL 48.86
     IMAGE-44 AT ROW 2.13 COL 40.57
     IMAGE-45 AT ROW 2.13 COL 48.86
     IMAGE-46 AT ROW 5.13 COL 40.57
     IMAGE-50 AT ROW 9.08 COL 48.86
     IMAGE-51 AT ROW 8.08 COL 40.57
     IMAGE-52 AT ROW 8.08 COL 48.86
     IMAGE-53 AT ROW 11.08 COL 40.57
     IMAGE-54 AT ROW 11.08 COL 48.86
     IMAGE-55 AT ROW 6.13 COL 40.57
     IMAGE-56 AT ROW 2.13 COL 48.86
     IMAGE-58 AT ROW 5.13 COL 48.86
     IMAGE-59 AT ROW 6.13 COL 48.86
     IMAGE-60 AT ROW 7.08 COL 40.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.57 ROW 2.71
         SIZE 77.86 BY 11.04.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 15.04
         WIDTH              = 81.86
         MAX-HEIGHT         = 27.38
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 27.38
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
/* SETTINGS FOR FRAME f-pg-dig
                                                                        */
/* BROWSE-TAB br-digita 1 f-pg-dig */
/* SETTINGS FOR FRAME f-pg-imp
                                                                        */
/* SETTINGS FOR FILL-IN text-destino IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       text-destino:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Destino".

ASSIGN 
       text-modelo-rtf:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Modelo:".

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

ASSIGN 
       text-rtf:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Rich Text Format(RTF)".

/* SETTINGS FOR FRAME f-pg-par
   Custom                                                               */
/* SETTINGS FOR TOGGLE-BOX v_log_expande_estrut IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v_num_opcao_quebra IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v_num_opcao_salta IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FILL-IN v_cdn_funcionario_fim IN FRAME f-pg-sel
   LIKE = dthrpyc.funcionario.cdn_funcionario EXP-SIZE                  */
/* SETTINGS FOR FILL-IN v_cdn_funcionario_ini IN FRAME f-pg-sel
   LIKE = dthrpyc.funcionario.cdn_funcionario EXP-LABEL EXP-SIZE        */
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


&Scoped-define BROWSE-NAME br-digita
&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON DEL OF br-digita IN FRAME f-pg-dig
DO:
   apply 'choose':U to bt-retirar in frame f-pg-dig.
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
        display tt-digita.cdn_motiv_liber_sal
                tt-digita.des_motiv_liber_sal with browse br-digita. 
    end.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON ENTER OF br-digita IN FRAME f-pg-dig
ANYWHERE
DO:
  if can-find(tt-digita where
              tt-digita.cdn_motiv_liber_sal = input browse br-digita tt-digita.cdn_motiv_liber_sal) then do:
      run utp/ut-msgs.p (input "show":U,
                         input 30406, 
                         input "campo" + "~~" + "esse c¢digo").

      apply 'ENTRY' to tt-digita.cdn_motiv_liber_sal.
      return no-apply.
  end.
  apply 'tab':U to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON INS OF br-digita IN FRAME f-pg-dig
DO:
   apply 'choose':U to bt-inserir in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON OFF-END OF br-digita IN FRAME f-pg-dig
DO:
   apply 'entry':U to bt-inserir in frame f-pg-dig.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON OFF-HOME OF br-digita IN FRAME f-pg-dig
DO:
/*   apply 'entry':U to bt-recuperar in frame f-pg-dig. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON ROW-ENTRY OF br-digita IN FRAME f-pg-dig
DO:
   /*:T trigger para inicializar campos da temp table de digitaá∆o */
    /*
   if  br-digita:new-row in frame f-pg-dig then do:
       assign tt-digita.cdn_motiv_liber_sal:screen-value in browse br-digita = string(0).
   end.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita C-Win
ON ROW-LEAVE OF br-digita IN FRAME f-pg-dig
DO:
    /*:T ê aqui que a gravaá∆o da linha da temp-table Ç efetivada.
       PorÇm as validaá‰es dos registros devem ser feitas na procedure pi-executar,
       no local indicado pelo coment†rio */
    
    if br-digita:new-row in frame f-pg-dig then
    do transaction on error undo, return no-apply:
        if not can-find(tt-digita where
                        tt-digita.cdn_motiv_liber_sal = input browse br-digita tt-digita.cdn_motiv_liber_sal) then do:
            create tt-digita.
            assign input browse br-digita tt-digita.cdn_motiv_liber_sal
                   input browse br-digita tt-digita.des_motiv_liber_sal.

            br-digita:create-result-list-entry() in frame f-pg-dig.
        end.
    end.
    else do transaction on error undo, return no-apply:
        assign input browse br-digita tt-digita.cdn_motiv_liber_sal
               input browse br-digita tt-digita.des_motiv_liber_sal.
    end.
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
    assign bt-retirar:SENSITIVE in frame f-pg-dig = yes.
    
    if num-results("br-digita":U) > 0 then
        br-digita:INSERT-ROW("after":U) in frame f-pg-dig.
    else do transaction:
        create tt-digita.
        
        open query br-digita for each tt-digita.
        
        apply "entry":U to tt-digita.cdn_motiv_liber_sal in browse br-digita. 
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&Scoped-define SELF-NAME bt-modelo-rtf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modelo-rtf C-Win
ON CHOOSE OF bt-modelo-rtf IN FRAME f-pg-imp
DO:
    def var c-arq-conv  as char no-undo.
    def var l-ok as logical no-undo.

    assign c-modelo-rtf = replace(input frame f-pg-imp c-modelo-rtf, "/", "~\").
    SYSTEM-DIALOG GET-FILE c-arq-conv
       FILTERS "*.rtf" "*.rtf",
               "*.*" "*.*"
       DEFAULT-EXTENSION "rtf"
       INITIAL-DIR "modelos" 
       MUST-EXIST
       USE-FILENAME
       UPDATE l-ok.
    if  l-ok = yes then
        assign c-modelo-rtf:screen-value in frame f-pg-imp  = replace(c-arq-conv, "~\", "/"). 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-dig
&Scoped-define SELF-NAME bt-retirar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-retirar C-Win
ON CHOOSE OF bt-retirar IN FRAME f-pg-dig /* Retirar */
DO:
    if  br-digita:num-selected-rows > 0 then do on error undo, return no-apply:
        get current br-digita.
        delete tt-digita.
        if  br-digita:delete-current-row() in frame f-pg-dig then.
    end.
        /*
    if num-results("br-digita":U) = 0 then
        assign bt-retirar:sensitive in frame f-pg-dig = no.
        */
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


&Scoped-define SELF-NAME im-pg-dig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL im-pg-dig C-Win
ON MOUSE-SELECT-CLICK OF im-pg-dig IN FRAME f-relat
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
&Scoped-define SELF-NAME l-habilitaRtf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-habilitaRtf C-Win
ON VALUE-CHANGED OF l-habilitaRtf IN FRAME f-pg-imp /* RTF */
DO:
        RUN pi-habilitaRtf.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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


&Scoped-define FRAME-NAME f-pg-cla
&Scoped-define SELF-NAME rs-tip-imp-motiv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-tip-imp-motiv C-Win
ON VALUE-CHANGED OF rs-tip-imp-motiv IN FRAME f-pg-cla
DO:
  if self:input-value = 1 then do:
      run pi-habilita-dig (input yes).
      assign im-pg-dig:sensitive in frame f-relat = no
             i-mt-ini:sensitive in frame f-pg-sel = yes
             i-mt-fim:sensitive in frame f-pg-sel = yes.
  end.
  else do:
      run pi-habilita-dig (input no).
      assign i-mt-ini:sensitive in frame f-pg-sel = no
             i-mt-fim:sensitive in frame f-pg-sel = no
             im-pg-dig:sensitive in frame f-relat = yes.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME v_log_imp_ult_movto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v_log_imp_ult_movto C-Win
ON VALUE-CHANGED OF v_log_imp_ult_movto IN FRAME f-pg-par /* Imprime Èltima Movimentaá∆o */
DO:
  if self:checked then
      assign rs-imp-ult-mov-tip:sensitive in frame {&frame-name} = true.
  else
      assign rs-imp-ult-mov-tip:sensitive in frame {&frame-name} = false.
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

{utp/ut9000.i "ESFP1661" "2.06.00.000"}

/* inicializaá‰es do template de relat¢rio */
{include/i-rpini.i}

find param_folha_educnal no-lock where
     param_folha_educnal.cdn_empresa = v_cdn_empres_usuar no-error.
assign v_log_folha_educnal = if avail param_folha_educnal then yes else no.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

{include/i-rplbl.i}

assign c-list-folders = "".

&IF "{&PGPAR}" <> "" &THEN 
    assign c-list-folders = c-list-folders + "im-pg-par,":U.
&ENDIF
&IF "{&PGCLA}" <> "" &THEN 
    assign c-list-folders = c-list-folders + "im-pg-cla,":U.
&ENDIF
&IF "{&PGSEL}" <> "" &THEN 
    assign c-list-folders = c-list-folders + "im-pg-sel,":U.
&ENDIF
&IF "{&PGDIG}" <> "" &THEN 
    assign c-list-folders = c-list-folders + "im-pg-dig,":U.
&ENDIF
&IF "{&PGIMP}" <> "" &THEN
    assign c-list-folders = c-list-folders + "im-pg-imp":U.
&ENDIF

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */

{prghur/fpp/fp9200.i19 5 6 0 0}

MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:


    ASSIGN c-modelo-default = "doc-rtf/ModeloRTF":U + TRIM(c-programa-mg97) + ".rtf":U.

    IF  SEARCH(c-modelo-default) = ? THEN
        ASSIGN c-modelo-default = SEARCH("doc-rtf/ModeloRTF0000.rtf":U).
    ELSE
        ASSIGN c-modelo-default = SEARCH(c-modelo-default).

    RUN enable_UI.

    ASSIGN v_cdn_funcionario_ini:SCREEN-VALUE IN FRAME f-pg-sel = STRING({prghur/dop/eng002.i})
           v_cdn_funcionario_fim:SCREEN-VALUE IN FRAME f-pg-sel = STRING({prghur/dop/eng001.i}).
    
    &if "{&func-dtype}" = "INT" &then 
        if v_log_folha_educnal = YES then
           assign v_cdn_func_centrdor_ini:visible in frame f-pg-sel = yes
                  v_cdn_func_centrdor_fim:visible in frame f-pg-sel = yes
                  v_cdn_funcionario_ini:visible in frame f-pg-sel   = no
                  v_cdn_funcionario_fim:visible in frame f-pg-sel   = no
                  v_cdn_tip_contrat_ini:visible in frame f-pg-sel   = yes
                  v_cdn_tip_contrat_fim:visible in frame f-pg-sel   = yes
                  image-53:visible in frame f-pg-sel                = yes 
                  image-54:visible in frame f-pg-sel                = yes.
    &EndIF

    if v_log_folha_educnal = NO then
       assign v_cdn_func_centrdor_ini:visible in frame f-pg-sel = no
              v_cdn_func_centrdor_fim:visible in frame f-pg-sel = no
              v_cdn_funcionario_ini:visible in frame f-pg-sel   = yes
              v_cdn_funcionario_fim:visible in frame f-pg-sel   = yes
              v_cdn_tip_contrat_ini:visible in frame f-pg-sel   = no
              v_cdn_tip_contrat_fim:visible in frame f-pg-sel   = no
              image-53:visible in frame f-pg-sel                = no 
              image-54:visible in frame f-pg-sel                = no.

    if v_cdn_func_usuar <> {prghur/dop/eng002.i} then
        run pi_emis_qsq.              

    FIND FIRST PARAM_empres_rh NO-LOCK WHERE
               PARAM_empres_rh.cdn_empresa = v_cdn_empres_usuar.
    assign v_num_casas_dec:help in frame F-PG-PAR         = "Informe 2, 3 ou 4"
           v_num_casas_dec:SCREEN-VALUE IN FRAME f-pg-par = IF AVAIL PARAM_empres_rh 
                                                            THEN string(PARAM_empres_rh.qti_dec_salario_horist)
                                                            ELSE string(0).

    &if "{&segur}" = "yes" &then
    if v_num_tip_aces_usuar = 1 then do:
       {prghur/fpp/fp9200.i2}
    end.
    &endif
    {prghur/fpp/fp9200.i9}

    {prghur/fpp/fp9240.i9}
    
    on leave of tt-digita.cdn_motiv_liber_sal in browse br-digita do:
        if br-digita:new-row in frame f-pg-dig then do:
            if can-find(tt-digita where
                        tt-digita.cdn_motiv_liber_sal = input browse br-digita tt-digita.cdn_motiv_liber_sal) then do:
                run utp/ut-msgs.p (input "show":U,
                                   input 30406,
                                   input "campo" + "~~" + "esse c¢digo").
    
                apply 'ENTRY' to tt-digita.cdn_motiv_liber_sal.
                return no-apply.
            end.
            else do:
                find motiv_liber_sal no-lock where
                     motiv_liber_sal.cdn_motiv_liber_sal  = integer(tt-digita.cdn_motiv_liber_sal:screen-value in browse br-digita) no-error.
                if avail motiv_liber_sal then
                    assign tt-digita.des_motiv_liber_sal:screen-value in browse br-digita = motiv_liber_sal.des_motiv_liber_sal.
                else
                    assign tt-digita.des_motiv_liber_sal:screen-value in browse br-digita = "".
            end.
        end.
        else do:
            find motiv_liber_sal no-lock where
                 motiv_liber_sal.cdn_motiv_liber_sal  = integer(tt-digita.cdn_motiv_liber_sal:screen-value in browse br-digita) no-error.
            if avail motiv_liber_sal then
                assign tt-digita.des_motiv_liber_sal:screen-value in browse br-digita = motiv_liber_sal.des_motiv_liber_sal.
            else
                assign tt-digita.des_motiv_liber_sal:screen-value in browse br-digita = "".
        end.
    end.
    tt-digita.cdn_motiv_liber_sal:load-mouse-pointer ("image/lupa.cur") in BROWSE br-digita .
    on F5 of tt-digita.cdn_motiv_liber_sal in browse br-digita
    or mouse-select-dblclick of tt-digita.cdn_motiv_liber_sal in browse br-digita do:
        {include/zoomvar.i &prog-zoom=object/sopy/zoom/z01py123.w
                          &campo=tt-digita.cdn_motiv_liber_sal
                          &campozoom=cdn_motiv_liber_sal
                          &campo2=tt-digita.des_motiv_liber_sal
                          &campozoom2=des_motiv_liber_sal
                          &browse=br-digita
                          &parametros="run pi_recebe_motiv in wh-pesquisa(input 1)".}
    end. 
    
    {include/i-rpmbl.i im-pg-cla}

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
  ENABLE im-pg-cla im-pg-dig im-pg-imp im-pg-par im-pg-sel bt-executar 
         bt-cancelar bt-terceiros bt-ajuda 
      WITH FRAME f-relat IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY rs-destino c-arquivo l-habilitaRtf c-modelo-rtf rs-execucao 
          tb-parametro text-rtf text-modelo-rtf 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  ENABLE RECT-7 RECT-9 RECT-98 RECT-RTF rs-destino bt-arquivo bt-config-impr 
         c-arquivo l-habilitaRtf bt-modelo-rtf c-modelo-rtf rs-execucao 
         tb-parametro text-rtf text-modelo-rtf 
      WITH FRAME f-pg-imp IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY rs-classif rs-tip-imp-motiv 
      WITH FRAME f-pg-cla IN WINDOW C-Win.
  ENABLE RECT-103 rs-classif rs-tip-imp-motiv 
      WITH FRAME f-pg-cla IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-cla}
  DISPLAY v_dat_refer v_log_expande_estrut v_num_opcao_salta v_num_opcao_quebra 
          l-imp-desl v_num_casas_dec v_log_imp_ult_movto rs-imp-ult-mov-tip 
          v_idi_tip_cargo_funcao 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  ENABLE v_dat_refer l-imp-desl v_num_casas_dec v_log_imp_ult_movto 
         rs-imp-ult-mov-tip v_idi_tip_cargo_funcao RECT-104 RECT-12 
      WITH FRAME f-pg-par IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY v_cdn_estab_ini v_cdn_estab_fim v_cdn_func_centrdor_ini 
          v_cdn_funcionario_ini v_cdn_func_centrdor_fim v_cdn_funcionario_fim 
          v_num_opcao_faixa v_cod_unid_lotac_ini v_cod_unid_lotac_fim c-cc-ini 
          c-cc-fim v_cdn_cargo_ini v_cdn_cargo_fim v_cdn_turno_ini 
          v_cdn_turno_fim c-nm-ini c-nm-fim i-mt-ini i-mt-fim d-dt-ini d-dt-fim 
          v_cdn_tip_contrat_ini v_cdn_tip_contrat_fim 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE IMAGE-1 IMAGE-2 IMAGE-31 IMAGE-32 IMAGE-33 IMAGE-34 IMAGE-42 IMAGE-43 
         IMAGE-44 IMAGE-45 IMAGE-46 IMAGE-50 IMAGE-51 IMAGE-52 IMAGE-53 
         IMAGE-54 IMAGE-55 IMAGE-56 IMAGE-58 IMAGE-59 IMAGE-60 v_cdn_estab_ini 
         v_cdn_estab_fim v_cdn_func_centrdor_ini v_cdn_funcionario_ini 
         v_cdn_func_centrdor_fim v_cdn_funcionario_fim v_cod_unid_lotac_ini 
         v_cod_unid_lotac_fim c-cc-ini c-cc-fim v_cdn_cargo_ini v_cdn_cargo_fim 
         v_cdn_turno_ini v_cdn_turno_fim c-nm-ini c-nm-fim i-mt-ini i-mt-fim 
         d-dt-ini d-dt-fim v_cdn_tip_contrat_ini v_cdn_tip_contrat_fim 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  ENABLE br-digita bt-inserir bt-retirar 
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
  
  for each motiv_liber_sal no-lock:
      create tt-digita.
      assign tt-digita.cdn_motiv_liber_sal = motiv_liber_sal.cdn_motiv_liber_sal
             tt-digita.des_motiv_liber_sal = motiv_liber_sal.des_motiv_liber_sal.
  end.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  apply 'VALUE-CHANGED' to v_log_imp_ult_movto in frame f-pg-par.
  apply 'VALUE-CHANGED' to rs-tip-imp-motiv in frame f-pg-cla.
  
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

    IF  input frame f-pg-imp l-habilitaRtf and
        (INPUT FRAME f-pg-imp c-modelo-rtf = "":U or
         (SEARCH(INPUT FRAME f-pg-imp c-modelo-rtf) = ? AND
         input frame f-pg-imp rs-execucao = 1)) THEN DO:
        {utp/ut-liter.i Modelo *}
        run utp/ut-msgs.p (input "show":U, input 27066, input trim(return-value)).
        apply "MOUSE-SELECT-CLICK":U to im-pg-imp in frame f-relat.
        apply "entry":U to c-modelo-rtf in frame f-pg-imp.
        return error.
    END.

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

    if  input frame f-pg-par v_num_casas_dec < 2 OR
        input frame f-pg-par v_num_casas_dec > 4 then do:
        run utp/ut-msgs.p (input "show",
                           input 28074,
                           input "").
        apply 'mouse-select-click' to im-pg-par in frame f-relat.
        apply 'entry' to v_num_casas_dec in frame f-pg-par.
        return error.

    end.

    /* Coloque aqui as validaá‰es das outras p†ginas, lembrando que elas
       devem apresentar uma mensagem de erro cadastrada, posicionar na p†gina 
       com problemas e colocar o focus no campo com problemas             */    

    {prghur/fpp/fp9200.i3 6 5 6 0 0}

    assign tt-param.v_cdn_empres_usuar    = v_cdn_empres_usuar
           tt-param.c-cc-ini              = input frame f-pg-sel c-cc-ini
           tt-param.c-cc-fim              = input frame f-pg-sel c-cc-fim 
           tt-param.c-nm-ini              = input frame f-pg-sel c-nm-ini
           tt-param.c-nm-fim              = input frame f-pg-sel c-nm-fim 
           tt-param.i-mt-ini              = input frame f-pg-sel i-mt-ini
           tt-param.i-mt-fim              = input frame f-pg-sel i-mt-fim 
           tt-param.d-dt-ini              = input frame f-pg-sel d-dt-ini
           tt-param.d-dt-fim              = input frame f-pg-sel d-dt-fim 
           tt-param.cdn_cargo_ini         = input frame f-pg-sel v_cdn_cargo_ini
           tt-param.cdn_cargo_fim         = input frame f-pg-sel v_cdn_cargo_fim 
           tt-param.cdn_func_centrdor_ini = input frame f-pg-sel v_cdn_func_centrdor_ini
           tt-param.cdn_func_centrdor_fim = input frame f-pg-sel v_cdn_func_centrdor_fim
           tt-param.cdn_tip_contrat_ini   = input frame f-pg-sel v_cdn_tip_contrat_ini
           tt-param.cdn_tip_contrat_fim   = input frame f-pg-sel v_cdn_tip_contrat_fim
           tt-param.l-imp-desl            = input frame f-pg-par l-imp-desl
           tt-param.parametro             = input frame f-pg-imp tb-parametro
           tt-param.idi_tip_cla_motiv     = input frame f-pg-cla rs-tip-imp-motiv
           tt-param.idi_imp_ult_mov_tip   = input frame f-pg-par rs-imp-ult-mov-tip    
           tt-param.v_log_imp_ult_movto   = input frame f-pg-par v_log_imp_ult_movto
           tt-param.num_casas_dec         = INPUT FRAME f-pg-par v_num_casas_dec
           tt-param.idi_tip_cargo_funcao  = input frame  f-pg-par v_idi_tip_cargo_funcao
           tt-param.des_tip_cargo_funcao  = entry((tt-param.idi_tip_cargo_funcao * 2 - 1), 
                                            v_idi_tip_cargo_funcao:radio-buttons in frame f-pg-par).
    assign
           tt-param.cdn_turno_ini         = input frame f-pg-sel v_cdn_turno_ini
           tt-param.cdn_turno_fim         = input frame f-pg-sel v_cdn_turno_fim.

    
    {prghur/fpp/fp9240.i10}
    {include/i-rpexb.i}

    if  session:set-wait-state("general") then.

    {include/i-rprun.i prghur/esp/esfp1661rp.p}

    {include/i-rpexc.i}

    if  session:set-wait-state("") then.

    /*{include/i-rptrm.i}*/
    {prghur/fpp/fp9240.i10}

end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-habilita-dig C-Win 
PROCEDURE pi-habilita-dig :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input param v_input_dig as log no-undo.

if v_input_dig = yes then
    wh-label-dig:fgcolor = 7.
else 
    wh-label-dig:fgcolor = ?.

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
{&OPEN-QUERY-br-digita}
                   
{include/i-rptrp.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_emis_qsq C-Win 
PROCEDURE pi_emis_qsq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
find funcionario no-lock where
     funcionario.cdn_empresa = v_cdn_empres_usuar and
     funcionario.cdn_estab   = v_cdn_estab_usuar_rh and
     funcionario.cdn_funcionario = v_cdn_func_usuar.

assign  v_cdn_func_centrdor_fim:sensitive in frame f-pg-sel = no
        v_cdn_func_centrdor_ini:sensitive in frame f-pg-sel = no 
        rs-classif:sensitive in frame f-pg-cla = no
        v_cdn_estab_fim:sensitive in frame f-pg-sel = no 
        v_cdn_estab_ini:sensitive in frame f-pg-sel = no 
        v_cdn_funcionario_fim:sensitive in frame f-pg-sel = no 
        v_cdn_funcionario_ini:sensitive in frame f-pg-sel = no 
        v_num_opcao_faixa:sensitive in frame f-pg-sel = no 
        v_cod_unid_lotac_fim:sensitive in frame f-pg-sel = no 
        v_cod_unid_lotac_ini:sensitive in frame f-pg-sel = no 
        c-cc-fim:sensitive in frame f-pg-sel = no 
        c-cc-ini:sensitive in frame f-pg-sel = no 
        c-nm-fim:sensitive in frame f-pg-sel = no 
        c-nm-ini:sensitive in frame f-pg-sel = no
        v_cdn_cargo_fim:sensitive in frame f-pg-sel = no
        v_cdn_cargo_ini:sensitive in frame f-pg-sel = no
        v_cdn_tip_contrat_fim:sensitive in frame f-pg-sel = no 
        v_cdn_tip_contrat_ini:sensitive in frame f-pg-sel = no 
        v_cdn_turno_ini:sensitive in frame f-pg-sel = no
        v_cdn_turno_fim:sensitive in frame f-pg-sel = no.

assign  v_cdn_estab_fim:screen-value in frame f-pg-sel = string(funcionario.cdn_estab) 
        v_cdn_estab_ini:screen-value in frame f-pg-sel = string(funcionario.cdn_estab)
        v_cdn_funcionario_fim:screen-value in frame f-pg-sel = string(funcionario.cdn_funcionario) 
        v_cdn_funcionario_ini:screen-value in frame f-pg-sel = string(funcionario.cdn_funcionario).
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


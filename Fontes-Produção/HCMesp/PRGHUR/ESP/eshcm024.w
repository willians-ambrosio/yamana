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
{include/buffers_RH.i}

{include/i-prgvrs.i ESHCM024 2.09.00.000}

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

define temp-table tt-param no-undo
    field destino               as integer
    field arquivo               as char format "x(35)"
    field usuario               as char format "x(12)"
    field data-exec             as date
    field hora-exec             as integer
    field cdn_empresa_ini       as char
    field cdn_empresa_fim       as char
    field cdn_estab_ini         as char
    field cdn_estab_fim         as char
    field cdn_funcionario_ini   as integer
    field cdn_funcionario_fim   as integer
    field dat_admis_func_ini    as date
    field dat_admis_func_fim    as date
    field dat_desligto_func_ini as date
    field dat_desligto_func_fim as date
    field cod_unid_lotac_ini    as char
    field cod_unid_lotac_fim    as char
    field cod_rh_ccusto_ini     as char
    field cod_rh_ccusto_fim     as char
    field periodo_ini           as char
    field periodo_fim           as char
    field tipo_relat            as int
    field arquivo-csv           as char.

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-imp

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-9 RECT-10 bt-config-impr ~
bt-arquivo c-arquivo rs-execucao rs_tipo_relat 
&Scoped-Define DISPLAYED-OBJECTS rs-destino c-arquivo rs-execucao ~
rs_tipo_relat 

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

DEFINE VARIABLE text-modo-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Tipo Relat¢rio" 
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

DEFINE VARIABLE rs_tipo_relat AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Anal¡tico", 1,
"Sint‚tico", 2
     SIZE 30.14 BY .92 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE BUTTON bt-arquivo-csv 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE c-arquivo-csv AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 256
     SIZE 40 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.92.

DEFINE VARIABLE tg-csv AS LOGICAL INITIAL no 
     LABEL "Exporta para CSV?" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.29 BY .83 NO-UNDO.

DEFINE VARIABLE fi_cdn_empresa_fim AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi_cdn_empresa_ini AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi_cdn_estab_fim AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi_cdn_estab_ini AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi_cdn_funcionario_fim AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi_cdn_funcionario_ini AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Matr¡cula" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi_cod_rh_ccusto_fim AS CHARACTER FORMAT "X(08)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi_cod_rh_ccusto_ini AS CHARACTER FORMAT "X(08)":U 
     LABEL "Centro Custo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi_cod_unid_lotac_fim AS CHARACTER FORMAT "X(11)":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi_cod_unid_lotac_ini AS CHARACTER FORMAT "X(11)":U 
     LABEL "Unidade Lota‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi_dat_admis_func_fim AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi_dat_admis_func_ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Data AdmissÆo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi_dat_desligto_func_fim AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi_dat_desligto_func_ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Desligamento" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi_periodo_fim AS CHARACTER FORMAT "99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi_periodo_ini AS CHARACTER FORMAT "99/9999":U 
     LABEL "Per¡odo" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

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

DEFINE IMAGE IMAGE-15
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-16
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
     bt-config-impr AT ROW 3.58 COL 43.29 HELP
          "Configura‡Æo da impressora"
     bt-arquivo AT ROW 3.58 COL 43.29 HELP
          "Escolha do nome do arquivo"
     c-arquivo AT ROW 3.63 COL 3.29 HELP
          "Nome do arquivo de destino do relat¢rio" NO-LABEL
     rs-execucao AT ROW 5.75 COL 3 HELP
          "Modo de Execu‡Æo" NO-LABEL
     rs_tipo_relat AT ROW 8 COL 2.86 HELP
          "Modo de Execu‡Æo" NO-LABEL WIDGET-ID 4
     text-destino AT ROW 1.63 COL 3.86 NO-LABEL
     text-modo AT ROW 5 COL 1.29 COLON-ALIGNED NO-LABEL
     text-modo-2 AT ROW 7.25 COL 1.14 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     RECT-7 AT ROW 1.92 COL 2.14
     RECT-9 AT ROW 5.29 COL 2.14
     RECT-10 AT ROW 7.54 COL 2 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 73.72 BY 10
         FONT 1 WIDGET-ID 100.

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     im-pg-sel AT ROW 1.5 COL 2.14
     im-pg-imp AT ROW 1.5 COL 33.57
     rt-folder AT ROW 2.5 COL 2
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-6 AT ROW 13.75 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
     im-pg-par AT ROW 1.5 COL 17.86 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar WIDGET-ID 100.

DEFINE FRAME f-pg-par
     tg-csv AT ROW 1.25 COL 2.72 WIDGET-ID 10
     bt-arquivo-csv AT ROW 2.04 COL 43 HELP
          "Escolha do nome do arquivo" WIDGET-ID 2
     c-arquivo-csv AT ROW 2.08 COL 3 HELP
          "Nome do arquivo de Exporta‡Æo 'Pessoal'" NO-LABEL WIDGET-ID 4
     RECT-8 AT ROW 1.58 COL 2 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 75 BY 10
         FONT 1 WIDGET-ID 200.

DEFINE FRAME f-pg-sel
     fi_cdn_empresa_ini AT ROW 1.25 COL 17 COLON-ALIGNED WIDGET-ID 30
     fi_cdn_empresa_fim AT ROW 1.25 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     fi_cdn_estab_ini AT ROW 2.25 COL 17 COLON-ALIGNED WIDGET-ID 38
     fi_cdn_estab_fim AT ROW 2.25 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     fi_cdn_funcionario_ini AT ROW 3.25 COL 17 COLON-ALIGNED WIDGET-ID 46
     fi_cdn_funcionario_fim AT ROW 3.25 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 44
     fi_dat_admis_func_ini AT ROW 4.25 COL 17 COLON-ALIGNED WIDGET-ID 54
     fi_dat_admis_func_fim AT ROW 4.25 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     fi_dat_desligto_func_ini AT ROW 5.25 COL 17 COLON-ALIGNED WIDGET-ID 62
     fi_dat_desligto_func_fim AT ROW 5.25 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 60
     fi_cod_unid_lotac_ini AT ROW 6.25 COL 17 COLON-ALIGNED WIDGET-ID 70
     fi_cod_unid_lotac_fim AT ROW 6.25 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 68
     fi_cod_rh_ccusto_ini AT ROW 7.25 COL 17 COLON-ALIGNED WIDGET-ID 78
     fi_cod_rh_ccusto_fim AT ROW 7.25 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     fi_periodo_ini AT ROW 8.25 COL 17 COLON-ALIGNED WIDGET-ID 86
     fi_periodo_fim AT ROW 8.25 COL 48.14 COLON-ALIGNED NO-LABEL WIDGET-ID 84
     IMAGE-1 AT ROW 1.25 COL 31.86 WIDGET-ID 32
     IMAGE-2 AT ROW 1.25 COL 47.29 WIDGET-ID 34
     IMAGE-3 AT ROW 2.25 COL 31.86 WIDGET-ID 40
     IMAGE-4 AT ROW 2.25 COL 47.29 WIDGET-ID 42
     IMAGE-5 AT ROW 3.25 COL 31.86 WIDGET-ID 48
     IMAGE-6 AT ROW 3.25 COL 47.29 WIDGET-ID 50
     IMAGE-7 AT ROW 4.25 COL 31.86 WIDGET-ID 56
     IMAGE-8 AT ROW 4.25 COL 47.29 WIDGET-ID 58
     IMAGE-9 AT ROW 5.25 COL 31.86 WIDGET-ID 64
     IMAGE-10 AT ROW 5.25 COL 47.29 WIDGET-ID 66
     IMAGE-11 AT ROW 6.25 COL 31.86 WIDGET-ID 74
     IMAGE-12 AT ROW 6.25 COL 47.29 WIDGET-ID 72
     IMAGE-13 AT ROW 7.25 COL 31.86 WIDGET-ID 80
     IMAGE-14 AT ROW 7.25 COL 47.29 WIDGET-ID 82
     IMAGE-15 AT ROW 8.25 COL 31.86 WIDGET-ID 90
     IMAGE-16 AT ROW 8.25 COL 47.29 WIDGET-ID 88
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3
         SIZE 75 BY 10.67
         FONT 1 WIDGET-ID 100.

DEFINE FRAME FRAME-A
     "Lembre-se de que qualquer novo evento de hora extra dever  ser inserido neste evento Sint‚tico.":100 VIEW-AS TEXT
          SIZE 72.72 BY .54 AT ROW 1.63 COL 1 WIDGET-ID 4
     "Este Relat¢rio ‚ gerado a partir do FP0180-Eventos Sint‚ticos, utilizando o evento 1000-HORAS EXTRAS.":100 VIEW-AS TEXT
          SIZE 72.72 BY .54 AT ROW 1 COL 1 WIDGET-ID 6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.57 ROW 9.25
         SIZE 73 BY 2.25
         FONT 1
         TITLE "Aten‡Æo" WIDGET-ID 300.


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
/* REPARENT FRAME */
ASSIGN FRAME f-pg-par:FRAME = FRAME f-relat:HANDLE
       FRAME FRAME-A:FRAME = FRAME f-pg-sel:HANDLE.

/* SETTINGS FOR FRAME f-pg-imp
   FRAME-NAME                                                           */
/* SETTINGS FOR RADIO-SET rs-destino IN FRAME f-pg-imp
   NO-ENABLE                                                            */
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

/* SETTINGS FOR FILL-IN text-modo-2 IN FRAME f-pg-imp
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       text-modo-2:PRIVATE-DATA IN FRAME f-pg-imp     = 
                "Tipo Relat¢rio".

/* SETTINGS FOR FRAME f-pg-par
                                                                        */
/* SETTINGS FOR BUTTON bt-arquivo-csv IN FRAME f-pg-par
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR c-arquivo-csv IN FRAME f-pg-par
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
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
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


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME bt-arquivo-csv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo-csv w-relat
ON CHOOSE OF bt-arquivo-csv IN FRAME f-pg-par
DO:
    {include/i-imarq.i c-arquivo-csv f-pg-par "'*.csv' '*.CSV'"}
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
&Scoped-define SELF-NAME c-arquivo-csv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-arquivo-csv w-relat
ON LEAVE OF c-arquivo-csv IN FRAME f-pg-par
DO:
    assign c-arquivo-csv:screen-value in frame f-pg-par = replace(c-arquivo-csv:screen-value in frame f-pg-par,"\","/").
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


&Scoped-define SELF-NAME rs_tipo_relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs_tipo_relat w-relat
ON VALUE-CHANGED OF rs_tipo_relat IN FRAME f-pg-imp
DO:
   {include/i-rprse.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME tg-csv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-csv w-relat
ON VALUE-CHANGED OF tg-csv IN FRAME f-pg-par /* Exporta para CSV? */
DO:
    assign c-arquivo-csv :sensitive      in frame {&frame-name} = tg-csv:checked
           bt-arquivo-csv:sensitive      in frame {&frame-name} = tg-csv:checked
           c-arquivo-csv :screen-value   in frame {&frame-name} = if tg-csv:checked then session:temp-directory + "eshcm024.csv" else ""
           c-arquivo-csv :screen-value   in frame {&frame-name} = replace(c-arquivo-csv :screen-value   in frame {&frame-name},"\","/").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-imp
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

{utp/ut9000.i "ESHCM024" "2.09.00.000"}

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

    assign fi_cdn_empresa_ini       = 0
           fi_cdn_empresa_fim       = 999
           fi_cdn_estab_ini         = 0
           fi_cdn_estab_fim         = 999
           fi_cdn_funcionario_ini   = 0
           fi_cdn_funcionario_fim   = 99999999
           fi_dat_admis_func_ini    = 01.01.0001
           fi_dat_admis_func_fim    = today
           fi_dat_desligto_func_ini = 01.01.0001
           fi_dat_desligto_func_fim = today
           fi_cod_unid_lotac_ini    = '00000000000'
           fi_cod_unid_lotac_fim    = '99999999999'
           fi_cod_rh_ccusto_ini     = '00000000'
           fi_cod_rh_ccusto_fim     = '99999999'
           fi_periodo_ini           = string(month(today),'99') + string(year(today),'9999')
           fi_periodo_fim           = string(month(today),'99') + string(year(today),'9999').

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
  ENABLE im-pg-sel im-pg-imp im-pg-par bt-executar bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY rs-destino c-arquivo rs-execucao rs_tipo_relat 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 RECT-10 bt-config-impr bt-arquivo c-arquivo rs-execucao 
         rs_tipo_relat 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-imp}
  DISPLAY tg-csv c-arquivo-csv 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE RECT-8 tg-csv 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY fi_cdn_empresa_ini fi_cdn_empresa_fim fi_cdn_estab_ini 
          fi_cdn_estab_fim fi_cdn_funcionario_ini fi_cdn_funcionario_fim 
          fi_dat_admis_func_ini fi_dat_admis_func_fim fi_dat_desligto_func_ini 
          fi_dat_desligto_func_fim fi_cod_unid_lotac_ini fi_cod_unid_lotac_fim 
          fi_cod_rh_ccusto_ini fi_cod_rh_ccusto_fim fi_periodo_ini 
          fi_periodo_fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 
         IMAGE-9 IMAGE-10 IMAGE-11 IMAGE-12 IMAGE-13 IMAGE-14 IMAGE-15 IMAGE-16 
         fi_cdn_empresa_ini fi_cdn_empresa_fim fi_cdn_estab_ini 
         fi_cdn_estab_fim fi_cdn_funcionario_ini fi_cdn_funcionario_fim 
         fi_dat_admis_func_ini fi_dat_admis_func_fim fi_dat_desligto_func_ini 
         fi_dat_desligto_func_fim fi_cod_unid_lotac_ini fi_cod_unid_lotac_fim 
         fi_cod_rh_ccusto_ini fi_cod_rh_ccusto_fim fi_periodo_ini 
         fi_periodo_fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  VIEW FRAME FRAME-A IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
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
    
    /* Coloque aqui as valida‡äes da p gina de Digita‡Æo, lembrando que elas devem
       apresentar uma mensagem de erro cadastrada, posicionar nesta p gina e colocar
       o focus no campo com problemas */
    /*browse br-digita:SET-REPOSITIONED-ROW (browse br-digita:DOWN, "ALWAYS":U).*/
        
    
    /* Coloque aqui as valida‡äes das outras p ginas, lembrando que elas devem 
       apresentar uma mensagem de erro cadastrada, posicionar na p gina com 
       problemas e colocar o focus no campo com problemas */
    
    if tg-csv:checked in frame f-pg-par then do:
        if c-arquivo-csv:input-value in frame f-pg-par = '' then do:
            run utp/ut-msgs.p (input 'show':U,
                               input 17006,
                               input 'Erro~~Arquivo de Exporta‡Æo CSV em Branco.').
            apply 'mouse-select-click' to im-pg-par in frame f-relat.
            apply 'entry' to c-arquivo-csv in frame f-pg-par.
            return error.
        end.

        run utp/ut-vlarq.p (input input frame f-pg-par c-arquivo-csv).
        
        if return-value = "NOK":U then do:
            run utp/ut-msgs.p (input "show", input 73, input "").
            
            apply 'mouse-select-click' to im-pg-par in frame f-relat.
            apply 'entry' to c-arquivo-csv in frame f-pg-par.
            return error.
        end.
    end.

    if fi_cdn_empresa_ini:input-value in frame f-pg-sel >
       fi_cdn_empresa_fim:input-value in frame f-pg-sel then do:
        run utp/ut-msgs.p (input 'show':U,
                           input 17006,
                           input 'Erro~~Empresa Inicial maior que Empresa Final.').
        apply 'mouse-select-click' to im-pg-sel in frame f-relat.
        apply 'entry' to fi_cdn_empresa_ini in frame f-pg-sel.
        return error.
    end.

    if fi_cdn_estab_ini:input-value in frame f-pg-sel >
       fi_cdn_estab_fim:input-value in frame f-pg-sel then do:
        run utp/ut-msgs.p (input 'show':U,
                           input 17006,
                           input 'Erro~~Estabelecimento Inicial maior que Estabelecimento Final.').
        apply 'mouse-select-click' to im-pg-sel in frame f-relat.
        apply 'entry' to fi_cdn_estab_ini in frame f-pg-sel.
        return error.
    end.

    if fi_cdn_funcionario_ini:input-value in frame f-pg-sel >
       fi_cdn_funcionario_fim:input-value in frame f-pg-sel then do:
        run utp/ut-msgs.p (input 'show':U,
                           input 17006,
                           input 'Erro~~Funcion rio Inicial maior que Funcion rio Final.').
        apply 'mouse-select-click' to im-pg-sel in frame f-relat.
        apply 'entry' to fi_cdn_funcionario_ini in frame f-pg-sel.
        return error.
    end.

    if fi_dat_admis_func_ini:input-value in frame f-pg-sel >
       fi_dat_admis_func_fim:input-value in frame f-pg-sel then do:
        run utp/ut-msgs.p (input 'show':U,
                           input 17006,
                           input 'Erro~~AdmissÆo Inicial maior que AdmissÆo Final.').
        apply 'mouse-select-click' to im-pg-sel in frame f-relat.
        apply 'entry' to fi_dat_admis_func_ini in frame f-pg-sel.
        return error.
    end.

    if fi_dat_desligto_func_ini:input-value in frame f-pg-sel >
       fi_dat_desligto_func_fim:input-value in frame f-pg-sel then do:
        run utp/ut-msgs.p (input 'show':U,
                           input 17006,
                           input 'Erro~~Desligamento Inicial maior que Desligamento Final.').
        apply 'mouse-select-click' to im-pg-sel in frame f-relat.
        apply 'entry' to fi_dat_desligto_func_ini in frame f-pg-sel.
        return error.
    end.

    if fi_cod_unid_lotac_ini:input-value in frame f-pg-sel >
       fi_cod_unid_lotac_fim:input-value in frame f-pg-sel then do:
        run utp/ut-msgs.p (input 'show':U,
                           input 17006,
                           input 'Erro~~Lota‡Æo Inicial maior que Lota‡Æo Final.').
        apply 'mouse-select-click' to im-pg-sel in frame f-relat.
        apply 'entry' to fi_cod_unid_lotac_ini in frame f-pg-sel.
        return error.
    end.

    if fi_cod_rh_ccusto_ini:input-value in frame f-pg-sel >
       fi_cod_rh_ccusto_fim:input-value in frame f-pg-sel then do:
        run utp/ut-msgs.p (input 'show':U,
                           input 17006,
                           input 'Erro~~Centro Custo Inicial maior que Centro Custo Final.').
        apply 'mouse-select-click' to im-pg-sel in frame f-relat.
        apply 'entry' to fi_cod_rh_ccusto_ini in frame f-pg-sel.
        return error.
    end.


    if substring(fi_periodo_ini:input-value in frame f-pg-sel,3,4) = substring(fi_periodo_fim:input-value in frame f-pg-sel,3,4) then do:
        if substring(fi_periodo_ini:input-value in frame f-pg-sel,1,2) >
           substring(fi_periodo_fim:input-value in frame f-pg-sel,1,2) then do:
            run utp/ut-msgs.p (input 'show':U,
                               input 17006,
                               input 'Erro~~Per¡odo Inicial maior que Per¡odo Final.').
            apply 'mouse-select-click' to im-pg-sel in frame f-relat.
            apply 'entry' to fi_periodo_ini in frame f-pg-sel.
            return error.
        end.
    end.
    else do:
        if substring(fi_periodo_ini:input-value in frame f-pg-sel,3,4) > substring(fi_periodo_fim:input-value in frame f-pg-sel,3,4) then do:
            run utp/ut-msgs.p (input 'show':U,
                               input 17006,
                               input 'Erro~~Per¡odo Inicial maior que Per¡odo Final.').
            apply 'mouse-select-click' to im-pg-sel in frame f-relat.
            apply 'entry' to fi_periodo_ini in frame f-pg-sel.
            return error.
        end.
    end.


    /* Aqui sÆo gravados os campos da temp-table que ser  passada como parƒmetro
       para o programa RP.P */
    
    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = input frame f-pg-imp rs-destino
           tt-param.data-exec       = today
           tt-param.hora-exec       = time.
    
    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".
    
    /* Coloque aqui a l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */
    
    assign tt-param.cdn_empresa_ini       = fi_cdn_empresa_ini      :input-value in frame f-pg-sel  
           tt-param.cdn_empresa_fim       = fi_cdn_empresa_fim      :input-value in frame f-pg-sel  
           tt-param.cdn_estab_ini         = fi_cdn_estab_ini        :input-value in frame f-pg-sel  
           tt-param.cdn_estab_fim         = fi_cdn_estab_fim        :input-value in frame f-pg-sel  
           tt-param.cdn_funcionario_ini   = fi_cdn_funcionario_ini  :input-value in frame f-pg-sel  
           tt-param.cdn_funcionario_fim   = fi_cdn_funcionario_fim  :input-value in frame f-pg-sel  
           tt-param.dat_admis_func_ini    = fi_dat_admis_func_ini   :input-value in frame f-pg-sel  
           tt-param.dat_admis_func_fim    = fi_dat_admis_func_fim   :input-value in frame f-pg-sel  
           tt-param.dat_desligto_func_ini = fi_dat_desligto_func_ini:input-value in frame f-pg-sel  
           tt-param.dat_desligto_func_fim = fi_dat_desligto_func_fim:input-value in frame f-pg-sel  
           tt-param.cod_unid_lotac_ini    = fi_cod_unid_lotac_ini   :input-value in frame f-pg-sel  
           tt-param.cod_unid_lotac_fim    = fi_cod_unid_lotac_fim   :input-value in frame f-pg-sel  
           tt-param.cod_rh_ccusto_ini     = fi_cod_rh_ccusto_ini    :input-value in frame f-pg-sel  
           tt-param.cod_rh_ccusto_fim     = fi_cod_rh_ccusto_fim    :input-value in frame f-pg-sel  
           tt-param.periodo_ini           = fi_periodo_ini          :input-value in frame f-pg-sel  
           tt-param.periodo_fim           = fi_periodo_fim          :input-value in frame f-pg-sel
           tt-param.tipo_relat            = rs_tipo_relat           :input-value in frame f-pg-imp
           tt-param.arquivo-csv           = c-arquivo-csv           :input-value in frame f-pg-par.

    /* Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}
    
    SESSION:SET-WAIT-STATE("general":U).
    
    {include/i-rprun.i prghur/esp/eshcm024rp.p}
    
    {include/i-rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
    if tg-csv:checked then do:
        {include/i-rptrm.i}
    end.

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


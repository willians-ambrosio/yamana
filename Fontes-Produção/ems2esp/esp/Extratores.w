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
{include/i-prgvrs.i extrat-connect 2.06.00.000}

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
    field classifica            as integer
    field desc-classifica       as char format "x(40)"
    FIELD tipo-periodo          AS INTEGER
    FIELD dias-extrat           AS INTEGER
    FIELD data-inicial          AS DATE
    FIELD data-final            AS DATE
    FIELD dir-extrat            AS CHAR
    FIELD l-centroCusto         AS LOGICAL 
    FIELD l-banco                AS LOGICAL 
    FIELD l-conta               AS LOGICAL
/*     FIELD l-contaContabil       AS LOGICAL */
    FIELD l-contrato            AS LOGICAL
/*     FIELD l-cotacao             AS LOGICAL */
    FIELD l-departamento        AS LOGICAL
    FIELD l-departamentoItem    AS LOGICAL
    FIELD l-deposito            AS LOGICAL
    FIELD l-sub-div-ordem       AS LOGICAL
    FIELD l-empresa             AS LOGICAL
/*     FIELD l-estab               AS LOGICAL */
    FIELD l-familiaItem         AS LOGICAL
    FIELD l-importacao          AS LOGICAL
    FIELD l-impostosimportacao  AS LOGICAL
    FIELD l-item                AS LOGICAL
    FIELD l-itemDeposito        AS LOGICAL
    FIELD l-moeda               AS LOGICAL
    FIELD l-movtoEstoq          AS LOGICAL
    FIELD l-naturOper           AS LOGICAL
    FIELD l-ordemCompra         AS LOGICAL
    FIELD l-ordem-import        AS LOGICAL
    FIELD l-ordemInvest         AS LOGICAL
    FIELD l-pedidoCompra        AS LOGICAL
    FIELD l-prazoCompra         AS LOGICAL
    FIELD l-recebimento         AS LOGICAL
    FIELD l-Ccustos             AS LOGICAL
    FIELD l-tipoTransacao       AS LOGICAL
    FIELD l-unidade             AS LOGICAL
    FIELD l-usuarioMaterial     AS LOGICAL
    FIELD l-sl-it-per           AS LOGICAL.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

define buffer b-tt-digita for tt-digita.

DEFINE VARIABLE c-caminho AS CHAR NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS RECT-7 RECT-9 rs-destino bt-arquivo ~
bt-config-impr c-arquivo rs-execucao 
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
     SIZE 46.29 BY 2.92.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 1.71.

DEFINE BUTTON desmarcarTds 
     LABEL "Desmarcar Todas" 
     SIZE 13 BY 1.

DEFINE BUTTON marcarTds 
     LABEL "Marcar Todas" 
     SIZE 13 BY 1.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22.72 BY 6.42.

DEFINE VARIABLE l-banco AS LOGICAL INITIAL yes 
     LABEL "Banco" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.29 BY .83 NO-UNDO.

DEFINE VARIABLE l-CentroCusto AS LOGICAL INITIAL yes 
     LABEL "Centro de Custo" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-conta AS LOGICAL INITIAL yes 
     LABEL "Conta" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-contrato AS LOGICAL INITIAL yes 
     LABEL "Contrato" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-departamento AS LOGICAL INITIAL yes 
     LABEL "Departamento" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-departamentoItem AS LOGICAL INITIAL yes 
     LABEL "Departamento X Item" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-deposito AS LOGICAL INITIAL yes 
     LABEL "Dep¢sito" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-empresa AS LOGICAL INITIAL yes 
     LABEL "Empresa" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.29 BY .83 NO-UNDO.

DEFINE VARIABLE l-familiaItem AS LOGICAL INITIAL yes 
     LABEL "Fam¡lia Item" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-importacao AS LOGICAL INITIAL yes 
     LABEL "Importacao" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-impostosImportacao AS LOGICAL INITIAL yes 
     LABEL "Impostos Importa‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-item AS LOGICAL INITIAL yes 
     LABEL "Item" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-itemDeposito AS LOGICAL INITIAL yes 
     LABEL "Item x Dep¢sito" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-moeda AS LOGICAL INITIAL yes 
     LABEL "Moeda" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-movtoEstoq AS LOGICAL INITIAL yes 
     LABEL "Movto. Estoque" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-naturOper AS LOGICAL INITIAL yes 
     LABEL "Natureza de Opera‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-ordem-import AS LOGICAL INITIAL yes 
     LABEL "Ordem Embarque" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-ordemCompra AS LOGICAL INITIAL yes 
     LABEL "Ordem Compra" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-ordemInvest AS LOGICAL INITIAL yes 
     LABEL "Ordem Investimento" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.72 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-pedidoCompra AS LOGICAL INITIAL yes 
     LABEL "Pedido de Compra" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-prazoCompra AS LOGICAL INITIAL yes 
     LABEL "Prazo de Compra" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-recebimento AS LOGICAL INITIAL yes 
     LABEL "Recebimento" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-sl-it-per AS LOGICAL INITIAL yes 
     LABEL "Saldo Item do Per¡odo" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-sub-div-ordem AS LOGICAL INITIAL yes 
     LABEL "SubDibOrdem" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.72 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-Ccustos AS LOGICAL INITIAL yes 
     LABEL "Ccusto" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-tipoTransacao AS LOGICAL INITIAL yes 
     LABEL "Tipo Transa‡Æo" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-unidade AS LOGICAL INITIAL yes 
     LABEL "Unidade" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY 1.08 NO-UNDO.

DEFINE VARIABLE l-usuarioMaterial AS LOGICAL INITIAL yes 
     LABEL "Usu rio Material" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY 1.08 NO-UNDO.

DEFINE VARIABLE d-dt-final AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88 NO-UNDO.

DEFINE VARIABLE d-dt-inicial AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .88 NO-UNDO.

DEFINE VARIABLE i-dias-extrat AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 30 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rdb-tipo-periodo AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Numero dias", 1,
"Intervalo de Datas", 2
     SIZE 21 BY 2.75 NO-UNDO.

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

DEFINE FRAME f-relat
     bt-executar AT ROW 14.54 COL 3 HELP
          "Dispara a execu‡Æo do relat¢rio"
     bt-cancelar AT ROW 14.54 COL 14 HELP
          "Fechar"
     bt-ajuda AT ROW 14.54 COL 70 HELP
          "Ajuda"
     im-pg-sel AT ROW 1.75 COL 2
     im-pg-par AT ROW 1.5 COL 17.57
     im-pg-imp AT ROW 1.5 COL 33.14
     rt-folder AT ROW 2.5 COL 2
     rt-folder-top AT ROW 2.54 COL 2.14
     rt-folder-left AT ROW 2.54 COL 2.14
     rt-folder-right AT ROW 2.67 COL 80.43
     RECT-6 AT ROW 13.75 COL 2.14
     RECT-1 AT ROW 14.29 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81 BY 15
         DEFAULT-BUTTON bt-executar WIDGET-ID 100.

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
         SIZE 73.72 BY 10 WIDGET-ID 100.

DEFINE FRAME f-pg-sel
     rdb-tipo-periodo AT ROW 2 COL 2 NO-LABEL WIDGET-ID 2
     i-dias-extrat AT ROW 2.25 COL 22 COLON-ALIGNED NO-LABEL
     d-dt-inicial AT ROW 3.5 COL 22 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     d-dt-final AT ROW 3.5 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     IMAGE-7 AT ROW 3.5 COL 36.43 WIDGET-ID 10
     IMAGE-8 AT ROW 3.5 COL 43 WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.85
         SIZE 76.86 BY 10.62 WIDGET-ID 100.

DEFINE FRAME f-pg-par
     l-CentroCusto AT ROW 1 COL 25 WIDGET-ID 4
     l-ordem-import AT ROW 1 COL 53.14 WIDGET-ID 6
     l-conta AT ROW 2 COL 2.29 WIDGET-ID 14
     l-familiaItem AT ROW 2 COL 25 WIDGET-ID 26
     l-departamentoItem AT ROW 2 COL 53.14 WIDGET-ID 20
     l-moeda AT ROW 2.83 COL 2.29 WIDGET-ID 34
     l-importacao AT ROW 3 COL 25 WIDGET-ID 10
     l-pedidoCompra AT ROW 3 COL 53.14 WIDGET-ID 46
     l-tipoTransacao AT ROW 3.88 COL 2.29 WIDGET-ID 54
     l-impostosImportacao AT ROW 4 COL 25 WIDGET-ID 74
     l-prazoCompra AT ROW 4 COL 53.14 WIDGET-ID 48
     l-Ccustos AT ROW 4.88 COL 2.43 WIDGET-ID 52
     l-item AT ROW 5 COL 25 WIDGET-ID 30
     l-recebimento AT ROW 5 COL 53.14 WIDGET-ID 50
     l-banco AT ROW 6 COL 2.57 WIDGET-ID 84
     l-itemDeposito AT ROW 6 COL 25 WIDGET-ID 32
     l-departamento AT ROW 6 COL 53.14 WIDGET-ID 18
     l-empresa AT ROW 7 COL 2.57 WIDGET-ID 88
     l-deposito AT ROW 7 COL 25 WIDGET-ID 22
     l-contrato AT ROW 7 COL 53.14 WIDGET-ID 8
     l-movtoEstoq AT ROW 8 COL 25 WIDGET-ID 36
     l-unidade AT ROW 8 COL 53.14 WIDGET-ID 56
     l-ordemInvest AT ROW 8.92 COL 2 WIDGET-ID 44
     l-naturOper AT ROW 9 COL 25 WIDGET-ID 38
     l-usuarioMaterial AT ROW 9 COL 53.14 WIDGET-ID 58
     l-sub-div-ordem AT ROW 9.92 COL 2 WIDGET-ID 82
     l-ordemCompra AT ROW 10 COL 25 WIDGET-ID 40
     l-sl-it-per AT ROW 10 COL 53.14 WIDGET-ID 76
     marcarTds AT ROW 11.08 COL 47 WIDGET-ID 68
     desmarcarTds AT ROW 11.08 COL 61 WIDGET-ID 70
     "Ext. Multi" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 1.17 COL 1.86 WIDGET-ID 80
     RECT-10 AT ROW 1.83 COL 1.29 WIDGET-ID 78
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.57 ROW 2.67
         SIZE 77.43 BY 11.08 WIDGET-ID 100.


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
         HEIGHT             = 15.29
         WIDTH              = 82.29
         MAX-HEIGHT         = 30.25
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 30.25
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
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
/* SETTINGS FOR FILL-IN d-dt-final IN FRAME f-pg-sel
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-dt-inicial IN FRAME f-pg-sel
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
&Scoped-define SELF-NAME d-dt-final
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL d-dt-final w-relat
ON LEAVE OF d-dt-final IN FRAME f-pg-sel
DO:
  ASSIGN d-dt-final.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME d-dt-inicial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL d-dt-inicial w-relat
ON LEAVE OF d-dt-inicial IN FRAME f-pg-sel
DO:

    ASSIGN d-dt-inicial.
/*    IF  day(d-dt-inicial) = 1 THEN DO:
        ASSIGN d-dt-final = DATE(MONTH(d-dt-inicial + 45),1,YEAR(d-dt-inicial + 45)) - 1.
        DISPLAY d-dt-final WITH FRAME f-pg-sel.
    END.
*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME desmarcarTds
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL desmarcarTds w-relat
ON CHOOSE OF desmarcarTds IN FRAME f-pg-par /* Desmarcar Todas */
DO:
   RUN pi-marca-desmarca (INPUT NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME i-dias-extrat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-dias-extrat w-relat
ON LEAVE OF i-dias-extrat IN FRAME f-pg-sel
DO:
  ASSIGN i-dias-extrat.
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


&Scoped-define FRAME-NAME f-pg-par
&Scoped-define SELF-NAME marcarTds
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL marcarTds w-relat
ON CHOOSE OF marcarTds IN FRAME f-pg-par /* Marcar Todas */
DO:
  RUN pi-marca-desmarca (INPUT YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME rdb-tipo-periodo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rdb-tipo-periodo w-relat
ON VALUE-CHANGED OF rdb-tipo-periodo IN FRAME f-pg-sel
DO:
    
    IF rdb-tipo-periodo:INPUT-VALUE = 1 THEN
        ASSIGN 
            i-dias-extrat:SENSITIVE IN FRAME f-pg-sel = YES
            d-dt-inicial:SENSITIVE IN FRAME f-pg-sel = NO
            d-dt-final:SENSITIVE IN FRAME f-pg-sel = NO.
    ELSE
        ASSIGN 
            i-dias-extrat:SENSITIVE IN FRAME f-pg-sel = NO
            d-dt-inicial:SENSITIVE IN FRAME f-pg-sel = YES
            d-dt-final:SENSITIVE IN FRAME f-pg-sel = YES.
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

{utp/ut9000.i "Extrator" "2.06.00.000"}

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

    ASSIGN d-dt-final   = TODAY 
           d-dt-inicial = DATE(MONTH(d-dt-final),1,YEAR(d-dt-final)).
    
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
  ENABLE im-pg-sel im-pg-par im-pg-imp bt-executar bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY l-CentroCusto l-ordem-import l-conta l-familiaItem l-departamentoItem 
          l-moeda l-importacao l-pedidoCompra l-tipoTransacao 
          l-impostosImportacao l-prazoCompra l-Ccustos l-item 
          l-recebimento l-banco l-itemDeposito l-departamento l-empresa 
          l-deposito l-contrato l-movtoEstoq l-unidade l-ordemInvest l-naturOper 
          l-usuarioMaterial l-sub-div-ordem l-ordemCompra l-sl-it-per 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  ENABLE l-CentroCusto l-ordem-import RECT-10 l-conta l-familiaItem 
         l-departamentoItem l-moeda l-importacao l-pedidoCompra l-tipoTransacao 
         l-impostosImportacao l-prazoCompra l-Ccustos l-item 
         l-recebimento l-banco l-itemDeposito l-departamento l-empresa 
         l-deposito l-contrato l-movtoEstoq l-unidade l-ordemInvest l-naturOper 
         l-usuarioMaterial l-sub-div-ordem l-ordemCompra l-sl-it-per marcarTds 
         desmarcarTds 
      WITH FRAME f-pg-par IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-par}
  DISPLAY rdb-tipo-periodo i-dias-extrat d-dt-inicial d-dt-final 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-7 IMAGE-8 rdb-tipo-periodo i-dias-extrat 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  DISPLAY rs-destino c-arquivo rs-execucao 
      WITH FRAME f-pg-imp IN WINDOW w-relat.
  ENABLE RECT-7 RECT-9 rs-destino bt-arquivo bt-config-impr c-arquivo 
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
    
    
    
    /* Aqui sÆo gravados os campos da temp-table que ser  passada como parƒmetro
       para o programa RP.P */

    EMPTY TEMP-TABLE tt-param.
    
    create tt-param.
    assign tt-param.usuario         = c-seg-usuario
           tt-param.destino         = INPUT FRAME f-pg-imp rs-destino
           tt-param.data-exec       = TODAY 
           tt-param.hora-exec       = TIME.

    if tt-param.destino = 1 
    then assign tt-param.arquivo = "".
    else if  tt-param.destino = 2 
         then assign tt-param.arquivo = input frame f-pg-imp c-arquivo.
         else assign tt-param.arquivo = session:temp-directory + c-programa-mg97 + ".tmp".
    
    /* Coloque aqui a l¢gica de grava‡Æo dos demais campos que devem ser passados
       como parƒmetros para o programa RP.P, atrav‚s da temp-table tt-param */

    DO  WITH FRAME f-pg-sel:

        ASSIGN tt-param.dias-extrat  = i-dias-extrat
               /*tt-param.tipo-periodo = rdb-tipo-periodo*/
               tt-param.data-inicial = d-dt-inicial
               tt-param.data-final   = d-dt-final.

    END.
    
    IF rdb-tipo-periodo:INPUT-VALUE = 1 THEN DO: /* Modifica‡Æo feita por Daniel 12/09/11 */
            tt-param.tipo-periodo = 1.           /* for‡ando a modifica‡Æo do campo */
    END.                                         /* tt-param.tipo-periodo                  */ 
    ELSE DO:
            tt-param.tipo-periodo = 2.
    END.

    DO  WITH FRAME f-pg-par:

        ASSIGN tt-param.l-centroCusto        = l-CentroCusto:CHECKED
               tt-param.l-conta              = l-conta:CHECKED           
               tt-param.l-banco              = l-banco:CHECKED           
/*                tt-param.l-contaContabil      = l-contaContabil:CHECKED */
               tt-param.l-contrato           = l-contrato:CHECKED
/*                tt-param.l-cotacao            = l-cotacao:CHECKED       */
               tt-param.l-departamento       = l-departamento:CHECKED    
               tt-param.l-departamentoItem   = l-departamentoItem:CHECKED
               tt-param.l-deposito           = l-deposito:CHECKED
               tt-param.l-sub-div-ordem      = l-sub-div-ordem:CHECKED
               tt-param.l-empresa            = l-empresa:CHECKED        
/*                tt-param.l-estab              = l-estab:CHECKED          */
               tt-param.l-familiaItem        = l-familiaItem:CHECKED     
               tt-param.l-importacao         = l-importacao:CHECKED      
               tt-param.l-impostosImportacao = l-impostosImportacao:CHECKED
               tt-param.l-item               = l-item:CHECKED 
               tt-param.l-itemDeposito       = l-itemDeposito:CHECKED 
               tt-param.l-moeda              = l-moeda:CHECKED           
               tt-param.l-movtoEstoq         = l-movtoEstoq:CHECKED      
               tt-param.l-naturOper          = l-naturOper:CHECKED       
               tt-param.l-ordemCompra        = l-ordemCompra:CHECKED     
               tt-param.l-ordem-import       = l-ordem-import:CHECKED    
               tt-param.l-ordemInvest        = l-ordemInvest:CHECKED     
               tt-param.l-pedidoCompra       = l-pedidoCompra:CHECKED    
               tt-param.l-prazoCompra        = l-prazoCompra:CHECKED     
               tt-param.l-recebimento        = l-recebimento:CHECKED     
               tt-param.l-Ccustos            = l-Ccustos:CHECKED       
               tt-param.l-tipoTransacao      = l-tipoTransacao:CHECKED   
               tt-param.l-unidade            = l-unidade:CHECKED         
               tt-param.l-usuarioMaterial    = l-usuarioMaterial:CHECKED
               tt-param.l-sl-it-per          = l-sl-it-per:CHECKED.

    END.
    
    /* Executar do programa RP.P que ir  criar o relat¢rio */
    {include/i-rpexb.i}
    
        

    SESSION:SET-WAIT-STATE("general":U).

    {include/i-rprun.i esp/ExtratoresRP.p}
    
    {include/i-rpexc.i}
    
    SESSION:SET-WAIT-STATE("":U).
    
    {include/i-rptrm.i}
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-marca-desmarca w-relat 
PROCEDURE pi-marca-desmarca :
/*-----------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER l-logico AS LOGICAL.

    ASSIGN 
        l-centroCusto:CHECKED IN FRAME f-pg-par       = l-logico
        l-conta:CHECKED IN FRAME f-pg-par             = l-logico
        l-banco:CHECKED IN FRAME f-pg-par             = l-logico
/*         l-contaContabil:CHECKED IN FRAME f-pg-par     = l-logico */
        l-contrato:CHECKED IN FRAME f-pg-par          = l-logico
/*         l-cotacao:CHECKED IN FRAME f-pg-par           = l-logico */
        l-departamento:CHECKED IN FRAME f-pg-par      = l-logico
        l-departamentoItem:CHECKED IN FRAME f-pg-par  = l-logico
        l-deposito:CHECKED IN FRAME f-pg-par          = l-logico
        l-sub-div-ordem:CHECKED IN FRAME f-pg-par     = l-logico
        l-empresa:CHECKED IN FRAME f-pg-par           = l-logico
        /* l-estab:CHECKED  IN FRAME f-pg-par            = l-logico   */
        l-familiaItem:CHECKED IN FRAME f-pg-par       = l-logico
        l-importacao:CHECKED IN FRAME f-pg-par        = l-logico
        l-impostosImportacao:CHECKED IN FRAME f-pg-par = l-logico
        l-item:CHECKED IN FRAME f-pg-par              = l-logico
        l-itemDeposito:CHECKED IN FRAME f-pg-par      = l-logico
        l-moeda:CHECKED IN FRAME f-pg-par             = l-logico
        l-movtoEstoq:CHECKED IN FRAME f-pg-par        = l-logico
        l-naturOper:CHECKED IN FRAME f-pg-par         = l-logico
        l-ordemCompra:CHECKED IN FRAME f-pg-par       = l-logico
        l-ordem-import:CHECKED IN FRAME f-pg-par      = l-logico
        l-ordemInvest:CHECKED IN FRAME f-pg-par       = l-logico
        l-pedidoCompra:CHECKED IN FRAME f-pg-par      = l-logico
        l-prazoCompra:CHECKED IN FRAME f-pg-par       = l-logico
        l-recebimento:CHECKED IN FRAME f-pg-par       = l-logico
        l-Ccustos:CHECKED IN FRAME f-pg-par           = l-logico
        l-tipoTransacao:CHECKED IN FRAME f-pg-par     = l-logico
        l-unidade:CHECKED IN FRAME f-pg-par           = l-logico 
        l-usuarioMaterial:CHECKED IN FRAME f-pg-par   = l-logico
        l-sl-it-per:CHECKED IN FRAME f-pg-par         = l-logico

        .

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


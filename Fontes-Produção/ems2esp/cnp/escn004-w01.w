&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems2cademp       PROGRESS
          ems2cadme        PROGRESS
          ems2movemp       PROGRESS
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/****************************************************************************************** 
**         Programa: escn0401.p
**        Autor: Daniela Campos
**       Fornecedor: DKP
**         Data: 06/11/2018
** Change/Chamado: 
**      Objetivo: Controle de retená‰es e garantias de contratos 
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor                   Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: 
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
{include/i-prgvrs.i XX9999 9.99.99.999}

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
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

/* Parameters Definitions ---                                           */
DEFINE VARIABLE c-status AS CHARACTER   NO-UNDO INITIAL "Vigente,Termo Encerramento Solicitado,Retená∆o Liberada,Processo Judicial". /* 1-Vigente, 2-Termo Enc, 3-Retená∆o Liberada, 4-Processo Jud */
/* Local Variable Definitions ---                                       */
DEF TEMP-TABLE tt-contrato LIKE ext-contrato-for 
     FIELD cod-emitente LIKE contrato-for.cod-emitente 
     FIELD nome-emit    LIKE emitente.nome-emit 
     FIELD valor-contr  LIKE contrato-for.dec-2
     FIELD tip-gar       AS CHAR.

DEFINE VARIABLE c_param AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-gar AS CHARACTER   NO-UNDO INITIAL "Cauá∆o,Seguro,Carta Fianáa".
DEFINE VARIABLE l-proc AS LOGICAL     NO-UNDO.

DEFINE VARIABLE hShowMsg AS HANDLE      NO-UNDO.

DEF TEMP-TABLE RowErrors
    FIELD ErrorSequence AS Integer              
    FIELD ErrorNumber   AS Integer              
    FIELD ErrorDescription  AS Character        
    FIELD ErrorParameters   AS Character        
    FIELD ErrorType             AS CHARACTER
    FIELD ErrorHelp         AS CHAR 
    FIELD ErrorSubType      AS CHAR
.
/* Tempor†rias para a alteraá∆o de t°tulos */
{cnp\escn004.i}

DEF BUFFER b-tt-retencao FOR tt-retencao.
DEF BUFFER bf-ext-contrato-for FOR ext-contrato-for.
DEF BUFFER bf-medicao-contrat FOR medicao-contrat.

{utp/utapi019.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME BROWSE-16

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ext-contrato-for contrato-for emitente ~
tt-retencao medicao-contrat es-medicao-contrat

/* Definitions for BROWSE BROWSE-16                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-16 ext-contrato-for.nr-contrato ~
contrato-for.cod-emitente emitente.nome-emit contrato-for.dec-2 ~
ENTRY (ext-contrato-for.ind_tip_gar,c-gar) ext-contrato-for.val_garantia ~
ext-contrato-for.perc_1 ext-contrato-for.perc_2 ext-contrato-for.dt_liber ~
ext-contrato-for.usuar_liber ext-contrato-for.dt_termo_enc ~
ext-contrato-for.dt_pg_ret ENTRY (ext-contrato-for.ind-status,c-status) 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-16 
&Scoped-define QUERY-STRING-BROWSE-16 FOR EACH ext-contrato-for ~
      WHERE ext-contrato-for.ind_tip_gar > 0 ~
 AND ext-contrato-for.pendente NO-LOCK, ~
      EACH contrato-for OF ext-contrato-for NO-LOCK, ~
      EACH emitente OF contrato-for NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-16 OPEN QUERY BROWSE-16 FOR EACH ext-contrato-for ~
      WHERE ext-contrato-for.ind_tip_gar > 0 ~
 AND ext-contrato-for.pendente NO-LOCK, ~
      EACH contrato-for OF ext-contrato-for NO-LOCK, ~
      EACH emitente OF contrato-for NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-16 ext-contrato-for contrato-for ~
emitente
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-16 ext-contrato-for
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-16 contrato-for
&Scoped-define THIRD-TABLE-IN-QUERY-BROWSE-16 emitente


/* Definitions for BROWSE BROWSE-17                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-17 tt-retencao.cod_estab tt-retencao.cod_tit_ap tt-retencao.cod_ser_docto tt-retencao.cod_espec_docto tt-retencao.cod_parcela tt-retencao.val_origin_tit_ap tt-retencao.dat_emis_docto tt-retencao.dat_vencto_tit_ap   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-17   
&Scoped-define SELF-NAME BROWSE-17
&Scoped-define QUERY-STRING-BROWSE-17 FOR EACH tt-retencao
&Scoped-define OPEN-QUERY-BROWSE-17 OPEN QUERY {&SELF-NAME} FOR EACH tt-retencao.
&Scoped-define TABLES-IN-QUERY-BROWSE-17 tt-retencao
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-17 tt-retencao


/* Definitions for BROWSE BROWSE-19                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-19 medicao-contrat.num-seq-item ~
medicao-contrat.num-seq-medicao medicao-contrat.it-codigo ~
medicao-contrat.dat-medicao medicao-contrat.perc-medicao ~
medicao-contrat.qtd-medicao medicao-contrat.sld-rec-medicao ~
medicao-contrat.sld-val-medicao medicao-contrat.val-medicao ~
es-medicao-contrat.vl-glosa-desc medicao-contrat.responsavel 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-19 
&Scoped-define QUERY-STRING-BROWSE-19 FOR EACH medicao-contrat NO-LOCK, ~
      EACH es-medicao-contrat OF medicao-contrat OUTER-JOIN NO-LOCK ~
    BY medicao-contrat.dat-medicao DESCENDING INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-19 OPEN QUERY BROWSE-19 FOR EACH medicao-contrat NO-LOCK, ~
      EACH es-medicao-contrat OF medicao-contrat OUTER-JOIN NO-LOCK ~
    BY medicao-contrat.dat-medicao DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-19 medicao-contrat es-medicao-contrat
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-19 medicao-contrat
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-19 es-medicao-contrat


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-BROWSE-17}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button IMAGE-1 IMAGE-2 IMAGE-11 IMAGE-12 ~
IMAGE-13 IMAGE-14 IMAGE-15 IMAGE-16 RECT-27 RECT-28 RECT-29 RECT-30 RECT-31 ~
RECT-32 i-nr-ctr-ini i-nr-ctr-fim rd-lib i-forn-ini i-forn-fim dt-lib-ini ~
dt-lib-fim dt-termo-ini dt-termo-fim cb_tp_gar BROWSE-16 BROWSE-19 ~
BROWSE-17 bt-liber bt-refresh 
&Scoped-Define DISPLAYED-OBJECTS i-nr-ctr-ini i-nr-ctr-fim rd-lib ~
i-forn-ini i-forn-fim dt-lib-ini dt-lib-fim dt-termo-ini dt-termo-fim ~
cb_tp_gar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 bt-ap dt-lib-ini dt-lib-fim dt-termo-ini dt-termo-fim 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-livre AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-programa 
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-livre MENUBAR
       SUB-MENU  mi-programa    LABEL "&Nome-do-Programa"
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ap 
     IMAGE-UP FILE "image/im-chck1.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-chck1.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Buscar" 
     SIZE 4.14 BY 1.25 TOOLTIP "Atualiza Contas a Pagar".

DEFINE BUTTON bt-liber 
     IMAGE-UP FILE "image/emailpng.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Buscar" 
     SIZE 4.14 BY 1.25 TOOLTIP "Dispara e-mail solicitaá∆o Termo Encerramento".

DEFINE BUTTON bt-refresh 
     IMAGE-UP FILE "image/refresh.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Buscar" 
     SIZE 4.14 BY 1.25 TOOLTIP "Busca dados".

DEFINE VARIABLE cb_tp_gar AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Garantia" 
     VIEW-AS COMBO-BOX INNER-LINES 4
     LIST-ITEM-PAIRS "Todas","0",
                     "Cauá∆o","1",
                     "Seguro","2",
                     "Carta Fianáa","3"
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE dt-lib-fim AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE dt-lib-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt. Liberaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE dt-termo-fim AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE dt-termo-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt. Termo En." 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE i-forn-fim AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 99999 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE i-forn-ini AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Fornecedor" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE i-nr-ctr-fim AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 99999 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE i-nr-ctr-ini AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "Contrato" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
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

DEFINE VARIABLE rd-lib AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Liberado p/ Pagamento", 1,
"N∆o liberado Pagamento", 2
     SIZE 40 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 3.25.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47.43 BY 3.25.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 107.57 BY 5.25.

DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 107.57 BY 4.46.

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18.43 BY 1.58.

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 107.43 BY 4.75.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 18.43 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-16 FOR 
      ext-contrato-for, 
      contrato-for, 
      emitente SCROLLING.

DEFINE QUERY BROWSE-17 FOR 
      tt-retencao SCROLLING.

DEFINE QUERY BROWSE-19 FOR 
      medicao-contrat, 
      es-medicao-contrat SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-16 w-livre _STRUCTURED
  QUERY BROWSE-16 NO-LOCK DISPLAY
      ext-contrato-for.nr-contrato COLUMN-LABEL "Contrato" FORMAT ">>>>>>>>9":U
      contrato-for.cod-emitente FORMAT ">>>>>>>>9":U
      emitente.nome-emit FORMAT "x(30)":U
      contrato-for.dec-2 COLUMN-LABEL "Valor Contrato" FORMAT ">>>,>>>,>>>,>>9.99":U
      ENTRY (ext-contrato-for.ind_tip_gar,c-gar) COLUMN-LABEL "Garantia" FORMAT "x(15)":U
            WIDTH 15
      ext-contrato-for.val_garantia COLUMN-LABEL "Valor Garantia" FORMAT ">>>,>>>,>>9.99":U
      ext-contrato-for.perc_1 COLUMN-LABEL "% Ret. 1 Fat" FORMAT ">>9.99":U
      ext-contrato-for.perc_2 FORMAT ">>9.99":U
      ext-contrato-for.dt_liber COLUMN-LABEL "Data Lib." FORMAT "99/99/9999":U
      ext-contrato-for.usuar_liber COLUMN-LABEL "Usu liberaá∆o" FORMAT "x(12)":U
      ext-contrato-for.dt_termo_enc COLUMN-LABEL "Dt Termo encer." FORMAT "99/99/9999":U
      ext-contrato-for.dt_pg_ret COLUMN-LABEL "Dt Pagto Ret" FORMAT "99/99/9999":U
      ENTRY (ext-contrato-for.ind-status,c-status) COLUMN-LABEL "Status" FORMAT "x(20)":U
            WIDTH 20
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 105 BY 4.29
         FONT 1 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-17 w-livre _FREEFORM
  QUERY BROWSE-17 DISPLAY
      tt-retencao.cod_estab         COLUMN-LABEL "Est"
         tt-retencao.cod_tit_ap        column-label "T°tulo" 
         tt-retencao.cod_ser_docto     COLUMN-LABEL "Serie" 
         tt-retencao.cod_espec_docto   column-label "Esp" 
         tt-retencao.cod_parcela       column-label "Parcela" 
         tt-retencao.val_origin_tit_ap   column-label "Valor" 
         tt-retencao.dat_emis_docto    column-label "Data Emiss∆o" 
         tt-retencao.dat_vencto_tit_ap column-label "Data Vencimento"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 105 BY 4
         FONT 1 ROW-HEIGHT-CHARS .67 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-19 w-livre _STRUCTURED
  QUERY BROWSE-19 NO-LOCK DISPLAY
      medicao-contrat.num-seq-item FORMAT ">>,>>9":U
      medicao-contrat.num-seq-medicao FORMAT ">,>>9":U
      medicao-contrat.it-codigo FORMAT "x(16)":U
      medicao-contrat.dat-medicao COLUMN-LABEL "Data Mediá∆o" FORMAT "99/99/9999":U
      medicao-contrat.perc-medicao FORMAT ">>9.99":U
      medicao-contrat.qtd-medicao FORMAT ">>>>>,>>>,>>9.99":U
      medicao-contrat.sld-rec-medicao FORMAT "->>>,>>>,>>9.9999":U
      medicao-contrat.sld-val-medicao FORMAT "->>>,>>>,>>9.9999":U
      medicao-contrat.val-medicao FORMAT ">>>>>,>>>,>>9.99":U
      es-medicao-contrat.vl-glosa-desc FORMAT ">>>,>>>,>>9.9999":U
      medicao-contrat.responsavel FORMAT "x(12)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 105 BY 3.63
         FONT 1 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-ap AT ROW 2.92 COL 104 WIDGET-ID 90
     i-nr-ctr-ini AT ROW 1.25 COL 8.29 COLON-ALIGNED WIDGET-ID 94
     i-nr-ctr-fim AT ROW 1.25 COL 26.86 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     rd-lib AT ROW 1.25 COL 46 NO-LABEL WIDGET-ID 20
     i-forn-ini AT ROW 2.25 COL 8.43 COLON-ALIGNED WIDGET-ID 6
     i-forn-fim AT ROW 2.25 COL 26.86 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     dt-lib-ini AT ROW 2.25 COL 52 COLON-ALIGNED WIDGET-ID 14
     dt-lib-fim AT ROW 2.25 COL 72.14 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     dt-termo-ini AT ROW 3.25 COL 52 COLON-ALIGNED WIDGET-ID 10
     dt-termo-fim AT ROW 3.25 COL 72.14 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     cb_tp_gar AT ROW 3.29 COL 8.43 COLON-ALIGNED WIDGET-ID 74
     BROWSE-16 AT ROW 5.42 COL 3.14 HELP
          "Duplo click para alteraá∆o da garantia" WIDGET-ID 200
     BROWSE-19 AT ROW 10.63 COL 3.14 WIDGET-ID 400
     BROWSE-17 AT ROW 15.5 COL 3 WIDGET-ID 300
     bt-liber AT ROW 2.96 COL 97.72 WIDGET-ID 92
     bt-refresh AT ROW 2.92 COL 92 WIDGET-ID 70
     "Retená‰es" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 14.75 COL 3 WIDGET-ID 88
     "Contrato & Garantias" VIEW-AS TEXT
          SIZE 13 BY .54 AT ROW 4.54 COL 3.72 WIDGET-ID 78
     "Mediá‰es" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 10.04 COL 3.29 WIDGET-ID 82
     rt-button AT ROW 1.08 COL 91
     IMAGE-1 AT ROW 1.25 COL 21.72 WIDGET-ID 24
     IMAGE-2 AT ROW 1.25 COL 25.57 WIDGET-ID 26
     IMAGE-11 AT ROW 2.25 COL 21.43 WIDGET-ID 28
     IMAGE-12 AT ROW 2.25 COL 25.57 WIDGET-ID 30
     IMAGE-13 AT ROW 2.25 COL 67.43 WIDGET-ID 32
     IMAGE-14 AT ROW 2.25 COL 71 WIDGET-ID 38
     IMAGE-15 AT ROW 3.25 COL 67.43 WIDGET-ID 34
     IMAGE-16 AT ROW 3.25 COL 71 WIDGET-ID 36
     RECT-27 AT ROW 1.08 COL 1.57 WIDGET-ID 40
     RECT-28 AT ROW 1.08 COL 43 WIDGET-ID 42
     RECT-29 AT ROW 4.71 COL 1.43 WIDGET-ID 76
     RECT-30 AT ROW 10.29 COL 1.43 WIDGET-ID 80
     RECT-31 AT ROW 2.75 COL 91 WIDGET-ID 84
     RECT-32 AT ROW 15 COL 1.43 WIDGET-ID 86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 108.43 BY 18.88
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-livre
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "Template Livre <Insira complemento>"
         HEIGHT             = 18.88
         WIDTH              = 108.43
         MAX-HEIGHT         = 18.88
         MAX-WIDTH          = 108.43
         VIRTUAL-HEIGHT     = 18.88
         VIRTUAL-WIDTH      = 108.43
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-livre:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-livre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-livre.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-livre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB BROWSE-16 cb_tp_gar f-cad */
/* BROWSE-TAB BROWSE-19 BROWSE-16 f-cad */
/* BROWSE-TAB BROWSE-17 BROWSE-19 f-cad */
ASSIGN 
       BROWSE-16:NUM-LOCKED-COLUMNS IN FRAME f-cad     = 2.

/* SETTINGS FOR BUTTON bt-ap IN FRAME f-cad
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN dt-lib-fim IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR FILL-IN dt-lib-ini IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR FILL-IN dt-termo-fim IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR FILL-IN dt-termo-ini IN FRAME f-cad
   1                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-16
/* Query rebuild information for BROWSE BROWSE-16
     _TblList          = "mgesp.ext-contrato-for,ems2cademp.contrato-for OF mgesp.ext-contrato-for,ems2cadme.emitente OF ems2cademp.contrato-for"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "mgesp.ext-contrato-for.ind_tip_gar > 0
 AND mgesp.ext-contrato-for.pendente"
     _FldNameList[1]   > mgesp.ext-contrato-for.nr-contrato
"mgesp.ext-contrato-for.nr-contrato" "Contrato" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = ems2cademp.contrato-for.cod-emitente
     _FldNameList[3]   > ems2cadme.emitente.nome-emit
"ems2cadme.emitente.nome-emit" ? "x(30)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ems2cademp.contrato-for.dec-2
"ems2cademp.contrato-for.dec-2" "Valor Contrato" ">>>,>>>,>>>,>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"ENTRY (ext-contrato-for.ind_tip_gar,c-gar)" "Garantia" "x(15)" ? ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > mgesp.ext-contrato-for.val_garantia
"mgesp.ext-contrato-for.val_garantia" "Valor Garantia" ">>>,>>>,>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > mgesp.ext-contrato-for.perc_1
"mgesp.ext-contrato-for.perc_1" "% Ret. 1 Fat" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = mgesp.ext-contrato-for.perc_2
     _FldNameList[9]   > mgesp.ext-contrato-for.dt_liber
"mgesp.ext-contrato-for.dt_liber" "Data Lib." ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > mgesp.ext-contrato-for.usuar_liber
"mgesp.ext-contrato-for.usuar_liber" "Usu liberaá∆o" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > mgesp.ext-contrato-for.dt_termo_enc
"mgesp.ext-contrato-for.dt_termo_enc" "Dt Termo encer." ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > mgesp.ext-contrato-for.dt_pg_ret
"mgesp.ext-contrato-for.dt_pg_ret" "Dt Pagto Ret" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"ENTRY (ext-contrato-for.ind-status,c-status)" "Status" "x(20)" ? ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-16 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-17
/* Query rebuild information for BROWSE BROWSE-17
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-retencao.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-17 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-19
/* Query rebuild information for BROWSE BROWSE-19
     _TblList          = "ems2movemp.medicao-contrat,mgesp.es-medicao-contrat OF ems2movemp.medicao-contrat"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ", OUTER"
     _OrdList          = "ems2movemp.medicao-contrat.dat-medicao|no"
     _FldNameList[1]   = ems2movemp.medicao-contrat.num-seq-item
     _FldNameList[2]   = ems2movemp.medicao-contrat.num-seq-medicao
     _FldNameList[3]   = ems2movemp.medicao-contrat.it-codigo
     _FldNameList[4]   > ems2movemp.medicao-contrat.dat-medicao
"medicao-contrat.dat-medicao" "Data Mediá∆o" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = ems2movemp.medicao-contrat.perc-medicao
     _FldNameList[6]   = ems2movemp.medicao-contrat.qtd-medicao
     _FldNameList[7]   = ems2movemp.medicao-contrat.sld-rec-medicao
     _FldNameList[8]   = ems2movemp.medicao-contrat.sld-val-medicao
     _FldNameList[9]   = ems2movemp.medicao-contrat.val-medicao
     _FldNameList[10]   = mgesp.es-medicao-contrat.vl-glosa-desc
     _FldNameList[11]   = ems2movemp.medicao-contrat.responsavel
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-19 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Template Livre <Insira complemento> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Template Livre <Insira complemento> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-16
&Scoped-define SELF-NAME BROWSE-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-16 w-livre
ON MOUSE-SELECT-DBLCLICK OF BROWSE-16 IN FRAME f-cad
DO:
    IF NOT AVAIL ext-contrato-for  THEN RETURN.

    ASSIGN l-proc = NO.
    IF ext-contrato-for.pendente THEN DO:

        IF CAN-FIND(FIRST bf-medicao-contrat OF contrato-for) THEN DO:

            MESSAGE "Contrato j† possui a primeira mediá∆o e n∆o poder† ser alterada a garantia!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RUN cnp\escn004-w03.w (INPUT ROWID(ext-contrato-for), INPUT contrato-for.nr-contrato, INPUT NO, OUTPUT l-proc). /* N∆o altera campos */ 
            
        END.
        ELSE RUN cnp\escn004-w03.w (INPUT ROWID(ext-contrato-for), INPUT contrato-for.nr-contrato, INPUT YES, OUTPUT l-proc) /* altera campos */.
        
    END.
    ELSE DO:

        MESSAGE "Termo de encerramento j† foi assinado!" skip
                "Contrato n∆o est† mais pendente e n∆o poder† ser alterada a garantia." 
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN.
    END.

    IF l-proc THEN 
        browse-16:REFRESH() IN FRAME {&FRAME-NAME}.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-16 w-livre
ON VALUE-CHANGED OF BROWSE-16 IN FRAME f-cad
DO:

   OPEN QUERY browse-19 FOR EACH medicao-contrat NO-LOCK WHERE
                                 medicao-contrat.nr-contrato = ext-contrato-for.nr-contrato,
                            EACH es-medicao-contrat OF medicao-contrat NO-LOCK OUTER-JOIN.
   
   APPLY "VALUE-CHANGED" TO browse-19.

   IF NOT CAN-FIND(FIRST tt-retencao WHERE
                         tt-retencao.nr-contrato = ext-contrato-for.nr-contrato) 
       THEN RUN pi-dados.

   OPEN QUERY browse-17 FOR EACH tt-retencao 
                            WHERE tt-retencao.nr-contrato = ext-contrato-for.nr-contrato.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ap
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ap w-livre
ON CHOOSE OF bt-ap IN FRAME f-cad /* Buscar */
DO:  
    
    ASSIGN l-proc = NO.

    IF NOT CAN-FIND(FIRST b-tt-retencao WHERE b-tt-retencao.nr-contrato = ext-contrato-for.nr-contrato) THEN  DO:

        MESSAGE "N∆o h† nenhum t°tulo para atualizaá∆o!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN.
    END.

    IF ext-contrato-for.dt_liber = ? THEN DO:

        MESSAGE "Contrato ainda n∆o liberado para pagamento!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN.
    END.

    RUN cnp\escn004-w02.w (INPUT TABLE tt-retencao,
                         OUTPUT l-proc).

  IF NOT l-proc  THEN RETURN.

  RUN pi-atu-tit_ap.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-liber
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-liber w-livre
ON CHOOSE OF bt-liber IN FRAME f-cad /* Buscar */
DO:  
    IF NOT AVAIL ext-contrato-for THEN RETURN.

    ASSIGN l-proc = NO.
    IF ext-contrato-for.pendente THEN DO:

        IF NOT CAN-FIND(FIRST bf-medicao-contrat OF contrato-for) THEN DO:

            MESSAGE "Contrato n∆o possui a primeira mediá∆o!" SKIP
                    "N∆o ser† poss°vel solicitar o termo de encerramento."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.
        ELSE DO: 
            MESSAGE "Confirma o envio de e-mail de solicitaá∆o do" SKIP
                    "Termo de Encerramento para o contrato " ext-contrato-for.nr-contrato 
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-conf AS LOG.

            IF l-conf THEN
                RUN pi-termogar. /* altera campos */.
        END.
        
    END.
    ELSE DO:

        MESSAGE "Termo de encerramento j† foi assinado!" skip
                "Contrato n∆o est† mais pendente." 
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN.
    END.


    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-refresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-refresh w-livre
ON CHOOSE OF bt-refresh IN FRAME f-cad /* Buscar */
DO:  
    ASSIGN INPUT FRAME {&FRAME-NAME} cb_tp_gar 
           INPUT FRAME {&FRAME-NAME} dt-lib-fim 
           INPUT FRAME {&FRAME-NAME} dt-lib-ini 
           INPUT FRAME {&FRAME-NAME} dt-termo-fim 
           INPUT FRAME {&FRAME-NAME} dt-termo-ini 
           INPUT FRAME {&FRAME-NAME} i-forn-fim 
           INPUT FRAME {&FRAME-NAME} i-forn-ini 
           INPUT FRAME {&FRAME-NAME} i-nr-ctr-fim 
           INPUT FRAME {&FRAME-NAME} i-nr-ctr-ini.


    IF INPUT FRAME {&FRAME-NAME} rd-lib = 2 /* N∆o Liberado para pagamento */ 
    THEN DO:

        IF INT(cb_tp_gar:SCREEN-VALUE IN FRAME {&FRAME-NAME}) = 0 THEN
             OPEN QUERY browse-16 
                 FOR EACH mgesp.ext-contrato-for
                          WHERE ext-contrato-for.nr-contrato >= i-nr-ctr-ini
                            AND ext-contrato-for.nr-contrato <= i-nr-ctr-fim
                            AND ext-contrato-for.ind_tip_gar > 0 
                            AND ext-contrato-for.pendente
                            AND ext-contrato-for.dt_liber    = ? NO-LOCK,
                          EACH contrato-for OF mgesp.ext-contrato-for NO-LOCK 
                             WHERE contrato-for.cod-emitente >= i-forn-ini 
                               AND contrato-for.cod-emitente <= i-forn-fim,
                          EACH emitente OF ems2cademp.contrato-for NO-LOCK.
        ELSE OPEN QUERY browse-16
                 FOR EACH mgesp.ext-contrato-for
                          WHERE ext-contrato-for.nr-contrato >= i-nr-ctr-ini
                            AND ext-contrato-for.nr-contrato <= i-nr-ctr-fim
                            AND ext-contrato-for.ind_tip_gar = INT(cb_tp_gar)
                            AND ext-contrato-for.pendente
                            AND ext-contrato-for.dt_liber    = ? NO-LOCK,
                          EACH contrato-for OF mgesp.ext-contrato-for NO-LOCK 
                             WHERE contrato-for.cod-emitente >= i-forn-ini 
                               AND contrato-for.cod-emitente <= i-forn-fim,
                          EACH emitente OF ems2cademp.contrato-for NO-LOCK.
     END.
     ELSE DO:
        
        IF INT(cb_tp_gar:SCREEN-VALUE IN FRAME {&FRAME-NAME}) = 0 THEN
             OPEN QUERY browse-16 
                 FOR EACH mgesp.ext-contrato-for
                          WHERE ext-contrato-for.nr-contrato  >= i-nr-ctr-ini
                            AND ext-contrato-for.nr-contrato  <= i-nr-ctr-fim
                            AND ext-contrato-for.ind_tip_gar  > 0 
                            AND ext-contrato-for.pendente
                            AND ext-contrato-for.dt_liber     >= dt-lib-ini    
                            AND ext-contrato-for.dt_liber     <= dt-lib-fim
                            AND ext-contrato-for.dt_termo_enc >= dt-termo-ini 
                            AND ext-contrato-for.dt_termo_enc <= dt-termo-fim NO-LOCK,
                          EACH contrato-for OF mgesp.ext-contrato-for NO-LOCK 
                             WHERE contrato-for.cod-emitente  >= i-forn-ini 
                               AND contrato-for.cod-emitente  <= i-forn-fim,
                          EACH emitente OF ems2cademp.contrato-for NO-LOCK.
        ELSE OPEN QUERY browse-16 
                 FOR EACH mgesp.ext-contrato-for
                          WHERE ext-contrato-for.nr-contrato  >= i-nr-ctr-ini
                            AND ext-contrato-for.nr-contrato  <= i-nr-ctr-fim
                            AND ext-contrato-for.ind_tip_gar  = INT(cb_tp_gar) 
                            AND ext-contrato-for.pendente
                            AND ext-contrato-for.dt_liber     >= dt-lib-ini    
                            AND ext-contrato-for.dt_liber     <= dt-lib-fim
                            AND ext-contrato-for.dt_termo_enc >= dt-termo-ini 
                            AND ext-contrato-for.dt_termo_enc <= dt-termo-fim NO-LOCK,
                          EACH contrato-for OF mgesp.ext-contrato-for NO-LOCK 
                             WHERE contrato-for.cod-emitente >= i-forn-ini 
                               AND contrato-for.cod-emitente <= i-forn-fim,
                          EACH emitente OF ems2cademp.contrato-for NO-LOCK.
     END.

     APPLY "VALUE-CHANGED" TO browse-16.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-livre
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-livre
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-livre
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-programa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-programa w-livre
ON MENU-DROP OF MENU mi-programa /* Nome-do-Programa */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-livre
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-livre
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-lib
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-lib w-livre
ON VALUE-CHANGED OF rd-lib IN FRAME f-cad
DO:
  IF INPUT FRAME {&frame-name} rd-lib = 1
       THEN DO:
      ENABLE {&list-1} WITH FRAME {&FRAME-NAME}.
      DISABLE bt-liber WITH FRAME {&FRAME-NAME}.
      ASSIGN dt-lib-fim = date(month(today),28,year(today)) + 4
             dt-lib-fim:SCREEN-VALUE   IN FRAME {&FRAME-NAME} = STRING(DATE(MONTH(dt-lib-fim),1,YEAR(dt-lib-fim)))
             dt-lib-ini:SCREEN-VALUE   IN FRAME {&FRAME-NAME} = string(date(month(today),1,year(today)))
             dt-termo-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(DATE(MONTH(dt-lib-fim),1,YEAR(dt-lib-fim)))
             dt-termo-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(date(month(today),1,year(today))).

  END.
  ELSE DO:
      DISABLE {&list-1} WITH FRAME {&FRAME-NAME}.
      ENABLE bt-liber WITH FRAME {&FRAME-NAME}.

       ASSIGN dt-lib-fim:SCREEN-VALUE   IN FRAME {&FRAME-NAME} = ""
              dt-lib-ini:SCREEN-VALUE   IN FRAME {&FRAME-NAME} = ""
              dt-termo-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
              dt-termo-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.
  APPLY "choose" TO bt-refresh.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */
/* ON 'LEAVE':U OF tt-retencao.perc_corr IN BROWSE browse-17                                                                  */
/* DO:                                                                                                                        */
/*     ASSIGN tt-retencao.vlr_corr = tt-retencao.val_origin_tit_ap * (1 + INPUT BROWSE browse-17 tt-retencao.perc_corr / 100) */
/*            tt-retencao.vlr_corr:SCREEN-VALUE IN BROWSE browse-17 = STRING(tt-retencao.vlr_corr).                           */
/*                                                                                                                            */
/*                                                                                                                            */
/* END.                                                                                                                       */

ON 'leave':U OF ext-contrato-for.dt_pg_ret
DO:
    
    /* 1-Vigente, 2-Termo Enc Solicitado, 3-Retená∆o Liberada, 4-Processo Jud, 5-Termo Encerramento liberado */
    IF ext-contrato-for.ind-status <> 5 OR ext-contrato-for.dt_liber = ?  THEN DO:

        MESSAGE "Situaá∆o do contrato n∆o permite atualizaá∆o da data de pagamento!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN.
    END.

    FOR EACH b-tt-retencao WHERE
             b-tt-retencao.nr-contrato = ext-contrato-for.nr-contrato:
        ASSIGN b-tt-retencao.dat_vencto_tit_ap = ext-contrato-for.dt_pg_ret.
    END.


    browse-17:REFRESH() IN FRAME {&FRAME-NAME}.

END.


/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-livre  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.17 , 92.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-livre  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-livre  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
  THEN DELETE WIDGET w-livre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-livre  _DEFAULT-ENABLE
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
  DISPLAY i-nr-ctr-ini i-nr-ctr-fim rd-lib i-forn-ini i-forn-fim dt-lib-ini 
          dt-lib-fim dt-termo-ini dt-termo-fim cb_tp_gar 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button IMAGE-1 IMAGE-2 IMAGE-11 IMAGE-12 IMAGE-13 IMAGE-14 IMAGE-15 
         IMAGE-16 RECT-27 RECT-28 RECT-29 RECT-30 RECT-31 RECT-32 i-nr-ctr-ini 
         i-nr-ctr-fim rd-lib i-forn-ini i-forn-fim dt-lib-ini dt-lib-fim 
         dt-termo-ini dt-termo-fim cb_tp_gar BROWSE-16 BROWSE-19 BROWSE-17 
         bt-liber bt-refresh 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-livre 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-livre 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  run pi-before-initialize.

  {include/win-size.i}

      
  {utp/ut9000.i "ESCN004-W01" "12.01.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY "VALUE-CHANGED" TO rd-lib IN FRAME {&FRAME-NAME}.


  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atu-tit_ap w-livre 
PROCEDURE pi-atu-tit_ap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Atualiza os t°tulos do contas a pagar - data de vencimento e valor */
DEFINE VARIABLE v_hdl_aux AS HANDLE      NO-UNDO.
DEFINE VARIABLE i-seq AS INTEGER     NO-UNDO.

EMPTY TEMP-TABLE tt_tit_ap_alteracao_base_aux_3.
EMPTY TEMP-TABLE RowErrors.

FOR EACH tt-retencao WHERE
         tt-retencao.atualiza:

    FIND tit_ap NO-LOCK WHERE
         tit_ap.cod_estab = tt-retencao.cod_estab 
        AND tit_ap.num_id_tit_ap = tt-retencao.num_id_tit_ap NO-ERROR.
    IF NOT AVAIL tit_ap THEN NEXT.

    /* Cria tempor†ria para alteraá∆o de t°tulos */
    CREATE tt_tit_ap_alteracao_base_aux_3.
    ASSIGN tt_tit_ap_alteracao_base_aux_3.ttv_cod_usuar_corren              = v_cod_usuar_corren
            tt_tit_ap_alteracao_base_aux_3.tta_cod_empresa                   = v_cod_empres_usuar
            tt_tit_ap_alteracao_base_aux_3.tta_cod_estab                     = tit_ap.cod_estab
            tt_tit_ap_alteracao_base_aux_3.tta_num_id_tit_ap                 = tit_ap.num_id_tit_ap
            tt_tit_ap_alteracao_base_aux_3.ttv_rec_tit_ap                    = RECID(tit_ap)
            tt_tit_ap_alteracao_base_aux_3.tta_dat_vencto_tit_ap             = tt-retencao.dat_vencto_tit_ap
            tt_tit_ap_alteracao_base_aux_3.tta_log_pagto_bloqdo              = NO 
            ttv_log_gera_ocor_alter_valores                                  = NO
            tt_tit_ap_alteracao_base_aux_3.ttv_ind_motiv_alter_val_tit_ap    = "ALTERACAO"
            tt_tit_ap_alteracao_base_aux_3.tta_des_histor_padr               = "T°tulo liberado para pagamento ap¢s validaá∆o da CÇlula de contratos".

END.

run prgfin/apb/apb767zf.py persistent set v_hdl_aux.

run pi_main_code_api_integr_ap_alter_tit_ap_6 
            in v_hdl_aux (Input 1,
                          Input "APB",  /* M¢dulo */
                          Input ""      /*p_cod_matriz_trad_org_ext*/ ,
                          input NO,     /*p_log_estorn_comis*/
                          input-output table tt_tit_ap_alteracao_base_aux_3,
                          input-output table tt_tit_ap_alteracao_rateio,
                          input-output table tt_params_generic_api,
                          output table tt_log_erros_tit_ap_alteracao).

FOR EACH tt_log_erros_tit_ap_alteracao WHERE
         tt_log_erros_tit_ap_alteracao.ttv_num_mensagem <> 6542:
      
    ASSIGN i-seq = i-seq + 1.
    CREATE RowErrors.
    ASSIGN RowErrors.ErrorSequence     = i-seq
           RowErrors.ErrorNumber       = tt_log_erros_tit_ap_alteracao.ttv_num_mensagem 
           RowErrors.ErrorDescription  = tt_log_erros_tit_ap_alteracao.ttv_des_msg_erro
           RowErrors.ErrorParameters   = tt_log_erros_tit_ap_alteracao.ttv_des_msg_ajuda_1 
           RowErrors.ErrorType         = "ERROR"
           RowErrors.ErrorHelp         =  "T°tulo: " + tt_log_erros_tit_ap_alteracao.tta_cod_tit_ap + 
                                          " Fornec: " + STRING(tt_log_erros_tit_ap_alteracao.tta_cdn_fornecedor) + 
                                          " Espec: " + tt_log_erros_tit_ap_alteracao.tta_cod_estab + 
                                          " /P: " + tt_log_erros_tit_ap_alteracao.tta_cod_parcela 
           RowErrors.ErrorSubType      = "QUESTION".

END.
IF CAN-FIND(FIRST RowErrors) THEN DO:
            {method/showmessage.i1}
            {method/ShowMessage.i2 &Modal="YES"}
END.
ELSE DO:

    FOR EACH tt_log_erros_tit_ap_alteracao WHERE
             tt_log_erros_tit_ap_alteracao.ttv_num_mensagem = 6542:

        FIND b-tt-retencao WHERE
             b-tt-retencao.cod_estab     = tt_log_erros_tit_ap_alteracao.tta_cod_estab AND 
             b-tt-retencao.num_id_tit_ap = tt_log_erros_tit_ap_alteracao.tta_num_id_tit_ap NO-ERROR.
        IF AVAIL b-tt-retencao
             THEN DO TRANS: 
            
            ASSIGN b-tt-retencao.ap-atual = YES.

            FIND bf-ext-contrato-for WHERE
                 bf-ext-contrato-for.nr-contrato = b-tt-retencao.nr-contrato NO-ERROR.
            IF AVAIL bf-ext-contrato-for
                 THEN ASSIGN bf-ext-contrato-for.pendente = NO.

            RELEASE bf-ext-contrato-for NO-ERROR.
        END.
    END.

    /* Envia e-mail */
    RUN pi-sendmail.

    /* Muda a situaá∆o do contrato de pendente para n∆o pendente */
    MESSAGE "Atualizaá∆o conclu°da com sucesso!"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    APPLY "CHOOSE" TO bt-refresh IN FRAME {&FRAME-NAME}.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-dados w-livre 
PROCEDURE pi-dados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 FIND es_parametros NO-LOCK 
          WHERE es_parametros.cod_prog_dtsul = "re1001" 
          AND   es_parametros.cod_referencia = "especie_retencao" 
          AND   es_parametros.dat_valid_ini <= TODAY NO-ERROR.
 ASSIGN c_param = es_parametros.cod_parametro WHEN AVAIL es_parametros.

FOR EACH rat-ordem NO-LOCK OF medicao-contrat:

      FIND item-doc-est OF rat-ordem NO-LOCK NO-ERROR.
      FIND docum-est NO-LOCK OF item-doc-est NO-ERROR.

      /* Busca o t°tulo no contas a pagar */ 
      FOR EACH tit_ap NO-LOCK 
          WHERE tit_ap.cod_estab       = docum-est.cod-estab
            AND tit_ap.cdn_fornecedor  = rat-ordem.cod-emitente
            AND tit_ap.cod_espec_docto = c_param
            AND tit_ap.cod_ser_docto   = rat-ordem.serie-docto 
            AND tit_ap.cod_tit_ap      = rat-ordem.nro-docto
            AND tit_ap.log_pagto_bloqdo: 
    
          IF YEAR(tit_ap.dat_vencto_tit_ap) <> 2500 THEN NEXT.

          IF CAN-FIND(FIRST tt-retencao WHERE
                          tt-retencao.cod_estab         = tit_ap.cod_estab
                      AND tt-retencao.num_id_tit_ap     = tit_ap.num_id_tit_ap) THEN NEXT.
 
          CREATE tt-retencao.
          BUFFER-COPY tit_ap TO tt-retencao
          ASSIGN tt-retencao.dat_vencto_tit_ap = (IF AVAIL ext-contrato-for AND ext-contrato-for.dt_pg_ret <> ? THEN ext-contrato-for.dt_pg_ret ELSE tit_ap.dat_vencto_tit_ap)
                 tt-retencao.nr-contrato       = ext-contrato-for.nr-contrato
                 tt-retencao.num_id_tit_ap     = tit_ap.num_id_tit_ap
                 tt-retencao.row-medicao       = ROWID(medicao-contrat).
      END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-sendmail w-livre 
PROCEDURE pi-sendmail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE c-arq-anexo AS CHARACTER   NO-UNDO.
    FIND FIRST param_email NO-LOCK NO-ERROR.

    EMPTY TEMP-TABLE tt-envio2.

    FOR EACH es_param_emp NO-LOCK 
        WHERE es_param_emp.cod_empresa    = v_cod_empres_usuar
        AND   es_param_emp.cod_prog_dtsul = "CN0201"
        AND   es_param_emp.cod_referencia BEGINS "email_fin":
         
         /* Verifica a existencia da planilha em excel gerada pela procedure pi-report-excel*/
         /* Ser† enviado pelo anexo para o usuario */
             
         EMPTY TEMP-TABLE tt-envio2.
         EMPTY TEMP-TABLE tt-mensagem.

         CREATE tt-envio2.
         ASSIGN tt-envio2.versao-integracao = 1
                tt-envio2.exchange          = param_email.log_servid_exchange
                tt-envio2.servidor          = param_email.cod_servid_e_mail
                tt-envio2.porta             = param_email.num_porta
                tt-envio2.destino           = es_param_emp.cod_parametro
                tt-envio2.assunto           = "Comunicado de liberaá∆o de retená∆o de contrato"
                tt-envio2.remetente         = "contratos@yamana.com"
                tt-envio2.copia             = ""
                tt-envio2.mensagem          = "Prezados(a/as)," +  CHR(13) + CHR(13) +
                                              "Segue abaixo os t°tulos de retená∆o que foram liberados para pagamento:" + CHR(13)
                tt-envio2.importancia       = 1
                tt-envio2.log-enviada       = NO 
                tt-envio2.log-lida          = NO 
                tt-envio2.acomp             = NO.

         CREATE tt-mensagem.
         ASSIGN tt-mensagem.seq-mensagem = 1
                tt-mensagem.mensagem     = "Prezados(a/as)," +  CHR(13) + CHR(13) +                                               
                                           "Segue abaixo o(s) t°tulo(s) de retená∆o que foram liberados para pagamento:" + CHR(13).  

         FOR EACH tt-retencao WHERE
                  tt-retencao.atualiza AND 
                  tt-retencao.ap-atual:

             CREATE tt-mensagem.
             ASSIGN tt-mensagem.seq-mensagem = 1
                    tt-mensagem.mensagem     = "Fornecedor: " + STRING(tt-retencao.cdn_fornec) 
                                                + " T°tulo: " + tt-retencao.cod_tit_ap 
                                                + " Ser: " + tt-retencao.cod_ser_docto 
                                                + " /P:" + tt-retencao.cod_parcela 
                                                + " Est.:" + tt-retencao.cod_estab.
         END.
             
         CREATE tt-mensagem.
         ASSIGN tt-mensagem.seq-mensagem = 1
                tt-mensagem.mensagem     = CHR(13) + CHR(13) + "Atenciosamente," + CHR(13) + CHR(13) + "CÇlula de Contratos Yamana".
                                                           
                                           
         RUN utp/utapi019.p PERSISTENT SET h-utapi019.

         RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                       INPUT  TABLE tt-mensagem,
                                       OUTPUT TABLE tt-erros).
         FOR EACH tt-erros:

             MESSAGE  "Falha ao enviar e-mail" SKIP
                      "Cod. Erro: " tt-erros.cod-erro  SKIP
                      tt-erros.desc-erro
                      tt-erros.desc-arq 
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
         END.
    
         DELETE PROCEDURE h-utapi019.     
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-termogar w-livre 
PROCEDURE pi-termogar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND FIRST param_email NO-LOCK NO-ERROR.

    EMPTY TEMP-TABLE tt-envio2.
    EMPTY TEMP-TABLE tt-mensagem.
    EMPTY TEMP-TABLE tt-erros.

    FOR EACH es_param_emp NO-LOCK 
        WHERE es_param_emp.cod_empresa    = v_cod_empres_usuar
        AND   es_param_emp.cod_prog_dtsul = "CN0201"
        AND   es_param_emp.cod_referencia BEGINS "email_sup":
         
         /* Verifica a existencia da planilha em excel gerada pela procedure pi-report-excel*/
         /* Ser† enviado pelo anexo para o usuario */
             
         EMPTY TEMP-TABLE tt-envio2.
         EMPTY TEMP-TABLE tt-mensagem.

         CREATE tt-envio2.
         ASSIGN tt-envio2.versao-integracao = 1
                tt-envio2.exchange          = param_email.log_servid_exchange
                tt-envio2.servidor          = param_email.cod_servid_e_mail
                tt-envio2.porta             = param_email.num_porta
                tt-envio2.destino           = es_param_emp.cod_parametro
                tt-envio2.assunto           = "Solicitaá∆o de termo de encerramento"
                tt-envio2.remetente         = "contratos@yamana.com"
                tt-envio2.copia             = ""
                tt-envio2.mensagem          = "Prezados(a/as)," +  CHR(13) + CHR(13) +
                                              "Por favor, solicitar o termo de encerramento para o contrato " + string(ext-contrato-for.nr-contrato) + "."
                tt-envio2.importancia       = 1
                tt-envio2.log-enviada       = NO 
                tt-envio2.log-lida          = NO 
                tt-envio2.acomp             = NO.

         CREATE tt-mensagem.
         ASSIGN tt-mensagem.seq-mensagem = 1
                tt-mensagem.mensagem     = "Prezados(a/as)," +  CHR(13) + CHR(13) +                                                                         
                                           "Por favor solicitar o termo de encerramento para o contrato " + string(ext-contrato-for.nr-contrato) + "." + 
                                            CHR(13) + CHR(13) + "Atenciosamente," + CHR(13) + CHR(13) + "CÇlula de Contratos Yamana". 
         
                                                           
                                           
         RUN utp/utapi019.p PERSISTENT SET h-utapi019.

         RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                        INPUT  TABLE tt-mensagem,
                                        OUTPUT TABLE tt-erros).

         IF NOT CAN-FIND(FIRST tt-erros) THEN DO:

             /* Atualiza o status do contrato para solicitado o termo de encerramento */ 
             FIND bf-ext-contrato-for WHERE
                  ROWID(bf-ext-contrato-for) = ROWID(ext-contrato-for) NO-ERROR.
             IF AVAIL bf-ext-contrato-for
                  THEN ASSIGN bf-ext-contrato-for.ind-status = 2. /* Solicitado Termo de encerramento */

             /* 1-Vigente, 2-Termo Enc, 3-Retená∆o Liberada, 4-Processo Jud */

             RELEASE bf-ext-contrato-for.

             browse-16:REFRESH() IN FRAME {&FRAME-NAME}.

         END.
         ELSE DO:
         
             FOR EACH tt-erros:
    
                 MESSAGE  "Falha ao enviar e-mail" SKIP
                          "Cod. Erro: " tt-erros.cod-erro  SKIP
                          tt-erros.desc-erro
                          tt-erros.desc-arq 
                     VIEW-AS ALERT-BOX INFO BUTTONS OK.
             END.
         END.
    
         DELETE PROCEDURE h-utapi019.     
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-livre  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "medicao-contrat"}
  {src/adm/template/snd-list.i "es-medicao-contrat"}
  {src/adm/template/snd-list.i "tt-retencao"}
  {src/adm/template/snd-list.i "ext-contrato-for"}
  {src/adm/template/snd-list.i "contrato-for"}
  {src/adm/template/snd-list.i "emitente"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-livre 
PROCEDURE state-changed :
/*:T -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


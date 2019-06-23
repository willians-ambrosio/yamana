&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems2cademp       PROGRESS
*/
&Scoped-define WINDOW-NAME w-window


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-matriz NO-UNDO LIKE matriz-rat-contr
       FIELD r-rowid AS ROWID.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa     for ems2cadme.empresa.

{include/i-prgvrs.i CN0201F 2.00.00.016 } /*** 010016 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i cn0201f MCN}
&ENDIF

/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 

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

/* Parameters Definitions ---                                           */

{include/i-frm055.i}
/* Local Variable Definitions ---                                       */

&Scoped-define table-parent contrato-for

{cdp/cd0666.i} /* defini‡Æo da temp-table de erros */   

/* insira a seguir a defini‡Æo da temp-table */

/*
def temp-table tt-matriz no-undo
    field nr-contrato    like contrato-for.nr-contrato
    field conta-contabil like conta-contab.conta-contabil             
    field perc-rateio    like matriz-rat-contr.perc-rateio.
*/
DEF VAR c-formato-conta  AS CHAR NO-UNDO INIT "x(20)".
DEF VAR c-formato-ccusto AS CHAR NO-UNDO INIT "x(20)".

DEF VAR de-percent LIKE matriz-rat-contr.perc-rateio NO-UNDO.
DEF VAR de-elimina LIKE matriz-rat-contr.perc-rateio NO-UNDO.
DEF VAR l-grava    AS LOGICAL                        NO-UNDO.
DEF VAR l-elimina  AS LOGICAL                        NO-UNDO.
DEF VAR l-invest   AS LOGICAL                        NO-UNDO.
DEF VAR i-est-ini1 AS INT                            NO-UNDO.
DEF VAR i-est-fim1 AS INT                            NO-UNDO.
DEF VAR i-est-ini2 AS INT                            NO-UNDO.
DEF VAR i-est-fim2 AS INT                            NO-UNDO.
DEF VAR c-conta    AS CHAR                           NO-UNDO.
&IF "{&mguni_version}" >= "2.071" &THEN
    DEF VAR iempresa   LIKE empresa.ep-codigo            NO-UNDO.
&ELSE
    DEF VAR iempresa   AS INT                            NO-UNDO.
&ENDIF
/** Usado no filtro da conta cd9017.w 
**/
{cdp/cd9017.i}

DEF VAR c-lista-tipo AS CHARACTER                                         NO-UNDO.
DEF VAR c-tipo       AS CHARACTER FORMAT "x(10)"                          NO-UNDO.
DEF VAR c-conta-ini  AS CHAR  FORMAT "x(20)" INIT ""                      NO-UNDO.
DEF VAR c-conta-fim  AS CHAR  FORMAT "x(20)" INIT "ZZZZZZZZZZZZZZZZZZZZ"  NO-UNDO.
DEF VAR l-cancela    AS LOGICAL              INIT YES                     NO-UNDO.
DEF VAR de-perc-ant  AS DECIMAL FORMAT "->>9.99"                          NO-UNDO.
DEF VAR wh-program   AS HANDLE                                            NO-UNDO.
DEF VAR i-empresa    LIKE param-global.empresa-prin                       NO-UNDO.
DEF VAR c-item       AS CHAR                                              NO-UNDO.
DEF VAR hProgramZoom AS HANDLE                                            NO-UNDO.
DEF VAR h-cdapi024   AS HANDLE                                            NO-UNDO.
DEF VAR h-boiv029    AS HANDLE                                            NO-UNDO.

{cdp/cdcfgdis.i}
{cdp/cdcfgmat.i}
{cdp/cd9731.i1} /*l-integra-cn-in-medicao*/

&if "{&bf_mat_versao_ems}":U >= "2.04":U &then
    /* Integracao Contratos - declaracao temp-tables */
    {cnp/cnapi020.i}
&endif

def temp-table tt-erro-aux1 no-undo
    field i-sequen as int             
    field cd-erro  as int
    field mensagem as char format "x(255)"
    field c-param  as char.

&if '{&bf_mat_versao_ems}' >= '2.062' &THEN
    {cdp/cd9590.i}    /*Include verifica‡Æo Unidade Neg¢cio*/
    DEFINE VARIABLE l-unid-neg AS LOGICAL INITIAL NO NO-UNDO.

    def temp-table tt-matriz-aux no-undo 
        field cod-unid-neg as char
        field perc-rateio  as dec.

    def temp-table tt-erro-aux no-undo
        field i-sequen as int
        field cd-erro  as int
        field mensagem as char format "x(255)".

    def buffer btt-matriz for tt-matriz.
&endif

def temp-table tt_cta_integr no-undo
    field ttv_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field ttv_cod_cta_ctbl                 as character format "x(20)" label "Conta Contÿbil" column-label "Conta Contÿbil"
    field ttv_des_titulo                   as character format "x(40)"
    field ttv_num_tip_cta_ctbl             as integer format ">9" label "Tipo Conta" column-label "Tipo Conta"
    field ttv_num_sit_cta_ctbl             as integer format ">9" label "Situa»’o Conta" column-label "Situa»’o Cta"
    field ttv_ind_finalid_ctbl_cta         as character format "X(40)" label "Finalidade Contÿbil"
    index tt_id                           
          ttv_cod_plano_cta_ctbl           ascending
          ttv_cod_cta_ctbl                 ascending
    .

def temp-table tt_cta_integr_aux no-undo
    field ttv_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field ttv_cod_cta_ctbl                 as character format "x(20)" label "Conta Contÿbil" column-label "Conta Contÿbil"
    field ttv_des_titulo                   as character format "x(40)"
    field ttv_num_tip_cta_ctbl             as integer format ">9" label "Tipo Conta" column-label "Tipo Conta"
    field ttv_num_sit_cta_ctbl             as integer format ">9" label "Situa»’o Conta" column-label "Situa»’o Cta"
    field ttv_ind_finalid_ctbl_cta         as character format "X(40)" label "Finalidade Contÿbil"
    index tt_id                           
          ttv_cod_plano_cta_ctbl           ascending
          ttv_cod_cta_ctbl                 ascending
    .

def temp-table tt_cta_integr_aux1 no-undo
    field ttv_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field ttv_cod_cta_ctbl                 as character format "x(20)" label "Conta Contÿbil" column-label "Conta Contÿbil"
    field ttv_des_titulo                   as character format "x(40)"
    field ttv_num_tip_cta_ctbl             as integer format ">9" label "Tipo Conta" column-label "Tipo Conta"
    field ttv_num_sit_cta_ctbl             as integer format ">9" label "Situa»’o Conta" column-label "Situa»’o Cta"
    field ttv_ind_finalid_ctbl_cta         as character format "X(40)" label "Finalidade Contÿbil"
    index tt_id                           
          ttv_cod_plano_cta_ctbl           ascending
          ttv_cod_cta_ctbl                 ascending
    .

def temp-table tt_ccusto_cta_integr no-undo
    field ttv_cod_empresa                  as character format "x(3)" label "Empresa" column-label "Empresa"
    field ttv_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field ttv_cod_cta_ctbl                 as character format "x(20)" label "Conta Contÿbil" column-label "Conta Contÿbil"
    field ttv_cod_plano_ccusto             as character format "x(8)" label "Plano CCusto" column-label "Plano CCusto"
    field ttv_cod_ccusto                   as Character format "x(11)" label "Centro Custo" column-label "Centro Custo"
    field ttv_des_ccusto                   as character format "x(40)" label "Des Ccusto" column-label "Des Ccusto"
    index tt_id                           
          ttv_cod_empresa                  ascending
          ttv_cod_plano_cta_ctbl           ascending
          ttv_cod_cta_ctbl                 ascending
          ttv_cod_plano_ccusto             ascending
          ttv_cod_ccusto                   ascending
    .

def temp-table tt_log_erro no-undo
    field ttv_num_cod_erro                 as integer format ">>>>,>>9" label "Nœmero" column-label "Nœmero"
    field ttv_des_msg_ajuda                as character format "x(40)" label "Mensagem Ajuda" column-label "Mensagem Ajuda"
    field ttv_des_msg_erro                 as character format "x(60)" label "Mensagem Erro" column-label "Inconsist¼ncia"
    .

DEF TEMP-TABLE tt-conta  NO-UNDO
    FIELD ct-codigo AS CHAR
    FIELD ct-desc   AS CHAR
    FIELD tipo      AS INT
    FIELD situacao  AS INT
    FIELD finalid   AS INT
    FIELD c-modulo  AS CHAR   
    .

DEF TEMP-TABLE tt-conta-cc-custo NO-UNDO
    FIELD ct-codigo AS CHAR
    FIELD ct-desc   AS CHAR
    FIELD sc-codigo AS CHAR
    FIELD sc-desc   AS CHAR    
    .

def var h_api_ccusto       as handle no-undo.
def var h_api_cta_ctbl     as handle no-undo.
DEF VAR i-lista-finalid    AS INT    NO-UNDO.
DEF VAR c-ct-codigo-browse AS CHAR   NO-UNDO.
DEF VAR c-sc-codigo-browse AS CHAR   NO-UNDO.
DEF VAR c-sc-codigo-matriz AS CHAR   NO-UNDO.
DEF VAR c-ct-codigo-matriz AS CHAR   NO-UNDO.

DEF BUFFER b-tt-matriz FOR tt-matriz.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-form2
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-formation
&Scoped-define BROWSE-NAME br-conta-cc-custo

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-conta-cc-custo tt-conta contrato-for ~
tt-matriz

/* Definitions for BROWSE br-conta-cc-custo                             */
&Scoped-define FIELDS-IN-QUERY-br-conta-cc-custo STRING(tt-conta-cc-custo.sc-codigo, c-formato-ccusto) tt-conta-cc-custo.sc-desc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-conta-cc-custo   
&Scoped-define SELF-NAME br-conta-cc-custo
&Scoped-define QUERY-STRING-br-conta-cc-custo FOR EACH tt-conta-cc-custo
&Scoped-define OPEN-QUERY-br-conta-cc-custo OPEN QUERY {&SELF-NAME} FOR EACH tt-conta-cc-custo.
&Scoped-define TABLES-IN-QUERY-br-conta-cc-custo tt-conta-cc-custo
&Scoped-define FIRST-TABLE-IN-QUERY-br-conta-cc-custo tt-conta-cc-custo


/* Definitions for BROWSE br-source-browse                              */
&Scoped-define FIELDS-IN-QUERY-br-source-browse STRING(tt-conta.ct-codigo, c-formato-conta) tt-conta.ct-desc tt-conta.tipo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-source-browse   
&Scoped-define SELF-NAME br-source-browse
&Scoped-define QUERY-STRING-br-source-browse FOR EACH tt-conta
&Scoped-define OPEN-QUERY-br-source-browse OPEN QUERY {&SELF-NAME} FOR EACH tt-conta.
&Scoped-define TABLES-IN-QUERY-br-source-browse tt-conta
&Scoped-define FIRST-TABLE-IN-QUERY-br-source-browse tt-conta


/* Definitions for BROWSE br-target-browse                              */
&Scoped-define FIELDS-IN-QUERY-br-target-browse STRING(tt-matriz.ct-codigo, c-formato-conta) STRING(tt-matriz.sc-codigo, c-formato-ccusto) &if '{&bf_mat_versao_ems}' >= '2.062' &then tt-matriz.cod-unid-negoc &endif tt-matriz.perc-rateio   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-target-browse &if '{&bf_mat_versao_ems}' >= '2.062' &then ~
 tt-matriz.cod-unid-negoc ~
&endif ~
tt-matriz.perc-rateio   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-target-browse '2 tt-matriz
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-target-browse '2
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-br-target-browse tt-matriz
&Scoped-define SELF-NAME br-target-browse
&Scoped-define QUERY-STRING-br-target-browse FOR EACH contrato-for, ~
               EACH tt-matriz        where tt-matriz.nr-contrato = contrato-for.nr-contrato
&Scoped-define OPEN-QUERY-br-target-browse OPEN QUERY {&SELF-NAME}     FOR EACH contrato-for, ~
               EACH tt-matriz        where tt-matriz.nr-contrato = contrato-for.nr-contrato.
&Scoped-define TABLES-IN-QUERY-br-target-browse contrato-for tt-matriz
&Scoped-define FIRST-TABLE-IN-QUERY-br-target-browse contrato-for
&Scoped-define SECOND-TABLE-IN-QUERY-br-target-browse tt-matriz


/* Definitions for FRAME f-formation                                    */
&Scoped-define FIELDS-IN-QUERY-f-formation contrato-for.nr-contrato ~
contrato-for.des-contrat 
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-formation ~
    ~{&OPEN-QUERY-br-conta-cc-custo}~
    ~{&OPEN-QUERY-br-source-browse}~
    ~{&OPEN-QUERY-br-target-browse}
&Scoped-define QUERY-STRING-f-formation FOR EACH contrato-for NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-f-formation OPEN QUERY f-formation FOR EACH contrato-for NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-f-formation contrato-for
&Scoped-define FIRST-TABLE-IN-QUERY-f-formation contrato-for


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 rt-key-parent rt-source-browse ~
rt-source-browse-2 RECT-7 fi-i-num-ord-inv br-source-browse ~
br-conta-cc-custo bt-filtro bt-faixa bt-add bt-del bt-invest ~
br-target-browse bt-confirma bt-ok bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-FIELDS contrato-for.nr-contrato ~
contrato-for.des-contrat 
&Scoped-define DISPLAYED-TABLES contrato-for
&Scoped-define FIRST-DISPLAYED-TABLE contrato-for
&Scoped-Define DISPLAYED-OBJECTS fi-i-num-ord-inv de-perc-total 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD is-create-allowed w-window 
FUNCTION is-create-allowed RETURNS LOGICAL
  ( v-row-tt as rowid)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD is-delete-allowed w-window 
FUNCTION is-delete-allowed RETURNS LOGICAL
  ( v-row-target as rowid)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-window AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-add 
     IMAGE-UP FILE "image\im-dw":U
     IMAGE-INSENSITIVE FILE "image\ii-dw":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-confirma 
     LABEL "C&onfirma" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "image\im-up":U
     IMAGE-INSENSITIVE FILE "image\ii-up":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-faixa 
     IMAGE-UP FILE "image\im-ran":U
     IMAGE-INSENSITIVE FILE "image\ii-ran":U
     LABEL "Fai&xa" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-filtro 
     IMAGE-UP FILE "image\im-fil":U
     IMAGE-INSENSITIVE FILE "image\ii-fil":U
     LABEL "&Filtro" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-invest 
     LABEL "&Invest" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&Ok" 
     SIZE 10 BY 1.

DEFINE VARIABLE de-perc-total AS DECIMAL FORMAT "->>9.99":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE fi-i-num-ord-inv AS INTEGER FORMAT ">>>>>,>>9":U INITIAL 0 
     LABEL "Num Ordem Inv" 
     VIEW-AS FILL-IN 
     SIZE 12.14 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 90 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 8.71.

DEFINE RECTANGLE rt-key-parent
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.14 BY 2.21.

DEFINE RECTANGLE rt-source-browse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44.14 BY 7.79.

DEFINE RECTANGLE rt-source-browse-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 7.79.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-conta-cc-custo FOR 
      tt-conta-cc-custo SCROLLING.

DEFINE QUERY br-source-browse FOR 
      tt-conta SCROLLING.

DEFINE QUERY br-target-browse FOR 
      contrato-for, 
      tt-matriz SCROLLING.

DEFINE QUERY f-formation FOR 
      contrato-for SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-conta-cc-custo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-conta-cc-custo w-window _FREEFORM
  QUERY br-conta-cc-custo DISPLAY
      STRING(tt-conta-cc-custo.sc-codigo, c-formato-ccusto) COLUMN-LABEL "Centro Custo" FORMAT "x(20)" WIDTH 20
tt-conta-cc-custo.sc-desc   COLUMN-LABEL "Titulo" FORMAT "x(40)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 40.86 BY 7.29.

DEFINE BROWSE br-source-browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-source-browse w-window _FREEFORM
  QUERY br-source-browse DISPLAY
      STRING(tt-conta.ct-codigo, c-formato-conta) COLUMN-LABEL "Conta" FORMAT "x(20)" WIDTH 20
            tt-conta.ct-desc   COLUMN-LABEL "T¡tulo" FORMAT "x(30)" 
            tt-conta.tipo      COLUMN-LABEL "Tipo"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 38 BY 7.29 ROW-HEIGHT-CHARS .63.

DEFINE BROWSE br-target-browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-target-browse w-window _FREEFORM
  QUERY br-target-browse DISPLAY
      STRING(tt-matriz.ct-codigo, c-formato-conta) COLUMN-LABEL "Conta" FORMAT "x(20)" WIDTH 20
      STRING(tt-matriz.sc-codigo, c-formato-ccusto) COLUMN-LABEL "Centro Custo" FORMAT "x(20)" WIDTH 20
      &if '{&bf_mat_versao_ems}' >= '2.062' &then
       tt-matriz.cod-unid-negoc COLUMN-LABEL "UNeg" WIDTH 04.0
      &endif
      tt-matriz.perc-rateio    COLUMN-LABEL "Rateio %" WIDTH 12

enable &if '{&bf_mat_versao_ems}' >= '2.062' &then
        tt-matriz.cod-unid-negoc
       &endif
       tt-matriz.perc-rateio
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 86 BY 7 ROW-HEIGHT-CHARS .63.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-formation
     contrato-for.nr-contrato AT ROW 1.25 COL 19.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.14 BY .88
     contrato-for.des-contrat AT ROW 1.25 COL 37.57 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 33.14 BY .88
     fi-i-num-ord-inv AT ROW 2.25 COL 19.86 COLON-ALIGNED
     br-source-browse AT ROW 3.71 COL 7
     br-conta-cc-custo AT ROW 3.71 COL 48.14 WIDGET-ID 100
     bt-filtro AT ROW 6 COL 2.57
     bt-faixa AT ROW 7 COL 2.57
     bt-add AT ROW 11.33 COL 42.29
     bt-del AT ROW 11.33 COL 46.57
     bt-invest AT ROW 11.92 COL 61
     br-target-browse AT ROW 12.75 COL 3
     de-perc-total AT ROW 19.88 COL 48 COLON-ALIGNED
     bt-confirma AT ROW 19.92 COL 33.57
     bt-ok AT ROW 21.42 COL 2.14
     bt-cancelar AT ROW 21.42 COL 13.14
     bt-ajuda AT ROW 21.42 COL 80.29
     RECT-1 AT ROW 21.21 COL 1
     rt-key-parent AT ROW 1.13 COL 1.86
     rt-source-browse AT ROW 3.46 COL 1.86
     rt-source-browse-2 AT ROW 3.46 COL 47
     RECT-7 AT ROW 12.42 COL 2 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 21.92.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-form2
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-matriz T "?" NO-UNDO mgind matriz-rat-contr
      ADDITIONAL-FIELDS:
          FIELD r-rowid AS ROWID
      END-FIELDS.
   END-TABLES.
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "Matriz de Rateio"
         HEIGHT             = 21.92
         WIDTH              = 89.86
         MAX-HEIGHT         = 28.83
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.83
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-window 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-window.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-formation
   FRAME-NAME                                                           */
/* BROWSE-TAB br-source-browse fi-i-num-ord-inv f-formation */
/* BROWSE-TAB br-conta-cc-custo br-source-browse f-formation */
/* BROWSE-TAB br-target-browse bt-invest f-formation */
/* SETTINGS FOR FILL-IN de-perc-total IN FRAME f-formation
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN contrato-for.des-contrat IN FRAME f-formation
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN contrato-for.nr-contrato IN FRAME f-formation
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-conta-cc-custo
/* Query rebuild information for BROWSE br-conta-cc-custo
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-conta-cc-custo.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-conta-cc-custo */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-source-browse
/* Query rebuild information for BROWSE br-source-browse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-conta.
     _END_FREEFORM
     _OrdList          = "mgadm.conta-contab.conta-contabil|yes"
     _Where[1]         = "conta-contab.conta-contabil >= c-conta-ini and
conta-contab.conta-contabil <= c-conta-fim and
conta-contab.ep-codigo       = i-empresa and 
((l-despesa and conta-contab.tipo = 1)         or 
 (l-receita and conta-contab.tipo = 2)         or 
 (l-venda   and conta-contab.tipo = 3)         or 
 (l-passivo and conta-contab.tipo = 4)         or 
 (l-ativo   and conta-contab.tipo = 5)         or 
 (l-titulo  and conta-contab.tipo = 6))        and 
((l-ativada    and conta-contab.estado = 1)    or 
 (l-desativada and conta-contab.estado = 2)    or 
((l-sistema    and conta-contab.estado = 3)    and
((l-contab     and conta-contab.contab <> 1)   or 
 (l-contas-pagar and conta-contab.contas-pagar <> 1) or 
 (l-contas-receb and conta-contab.contas-receb <> 1) or 
 (l-estoque      and conta-contab.estoque <> 10)     or 
 (l-faturamento  and conta-contab.faturamento <> 1)  or 
 (l-folha-pagto  and conta-contab.folha-pagto <> 1)  or 
 (l-patrimonio   and conta-contab.patrimonio <> 1)   or 
 (l-investimento and conta-contab.investimento <> 1) or 
 (l-obrig-fisc   and conta-contab.obrig-fisc <> 1)   or 
 (l-caixa-bancos and conta-contab.caixa-bancos <> 1))))"
     _Query            is OPENED
*/  /* BROWSE br-source-browse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-target-browse
/* Query rebuild information for BROWSE br-target-browse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME}
    FOR EACH contrato-for,
        EACH tt-matriz
       where tt-matriz.nr-contrato = contrato-for.nr-contrato.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-target-browse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-formation
/* Query rebuild information for FRAME f-formation
     _TblList          = "ems2cademp.contrato-for"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* FRAME f-formation */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* Matriz de Rateio */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* Matriz de Rateio */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-conta-cc-custo
&Scoped-define SELF-NAME br-conta-cc-custo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-conta-cc-custo w-window
ON ROW-DISPLAY OF br-conta-cc-custo IN FRAME f-formation
DO:
    IF c-sc-codigo-browse = '' THEN
        ASSIGN c-sc-codigo-browse = tt-conta-cc-custo.sc-codigo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-conta-cc-custo w-window
ON VALUE-CHANGED OF br-conta-cc-custo IN FRAME f-formation
DO:
     ASSIGN c-sc-codigo-browse = tt-conta-cc-custo.sc-codigo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-source-browse
&Scoped-define SELF-NAME br-source-browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-source-browse w-window
ON ROW-DISPLAY OF br-source-browse IN FRAME f-formation
DO:
    IF c-ct-codigo-browse = '' THEN DO:
        
        EMPTY TEMP-TABLE tt_cta_integr_aux1.
        
        ASSIGN c-ct-codigo-browse = tt-conta.ct-codigo.

        find FIRST tt_cta_integr_aux
             WHERE tt_cta_integr_aux.ttv_cod_cta_ctbl = tt-conta.ct-codigo no-error.
        if avail tt_cta_integr_aux THEN DO:

            CREATE tt_cta_integr_aux1.
            BUFFER-COPY tt_cta_integr_aux TO tt_cta_integr_aux1.
           
            RUN pi-busca-ccusto.
            {&OPEN-QUERY-br-conta-cc-custo}
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-source-browse w-window
ON VALUE-CHANGED OF br-source-browse IN FRAME f-formation
DO:
  /*{include/i-frm020.i}*/

    ASSIGN c-ct-codigo-browse = tt-conta.ct-codigo.

    EMPTY TEMP-TABLE tt_cta_integr_aux1.

    find FIRST tt_cta_integr_aux
         WHERE tt_cta_integr_aux.ttv_cod_cta_ctbl = tt-conta.ct-codigo no-error.
    if avail tt_cta_integr_aux THEN DO:

        CREATE tt_cta_integr_aux1.
        BUFFER-COPY tt_cta_integr_aux TO tt_cta_integr_aux1.

        RUN pi-busca-ccusto.
        {&OPEN-QUERY-br-conta-cc-custo}
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-target-browse
&Scoped-define SELF-NAME br-target-browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-target-browse w-window
ON ROW-DISPLAY OF br-target-browse IN FRAME f-formation
DO:
    IF c-ct-codigo-matriz = '' THEN
        ASSIGN c-ct-codigo-matriz = tt-matriz.ct-codigo
               c-sc-codigo-matriz = tt-matriz.sc-codigo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-target-browse w-window
ON ROW-ENTRY OF br-target-browse IN FRAME f-formation
DO:
  assign de-perc-ant = dec(tt-matriz.perc-rateio:screen-value in browse {&browse-name}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-target-browse w-window
ON ROW-LEAVE OF br-target-browse IN FRAME f-formation
DO:
  assign de-perc-total = input frame {&frame-name} de-perc-total - de-perc-ant +
                         dec(tt-matriz.perc-rateio:screen-value in browse {&browse-name}).
  disp de-perc-total with frame {&frame-name}.                       
  assign de-perc-ant = dec(tt-matriz.perc-rateio:screen-value in browse {&browse-name}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-target-browse w-window
ON VALUE-CHANGED OF br-target-browse IN FRAME f-formation
DO:
  {include/i-frm010.i}
  assign de-perc-ant = dec(tt-matriz.perc-rateio:screen-value in browse {&browse-name}).

  ASSIGN c-ct-codigo-matriz = tt-matriz.ct-codigo
         c-sc-codigo-matriz = tt-matriz.sc-codigo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add w-window
ON CHOOSE OF bt-add IN FRAME f-formation
DO:
    DEF VAR i-cont  AS INT NO-UNDO.
    DEF VAR l-achou AS LOG NO-UNDO.
    
    FIND FIRST tt-conta-cc-custo NO-ERROR.
    IF AVAIL tt-conta-cc-custo THEN DO:
        IF (br-conta-cc-custo:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} <> 0) THEN DO:
                
            DO i-cont = 1 TO br-conta-cc-custo:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}:
              
                ASSIGN l-achou = NO
                       l-achou = br-conta-cc-custo:FETCH-SELECTED-ROW(i-cont) IN FRAME {&FRAME-NAME}.
                IF l-achou THEN DO:
                    ASSIGN c-sc-codigo-browse = tt-conta-cc-custo.sc-codigo.
                    RUN pi-ins.
            
                    IF  RETURN-VALUE = 'adm-error' THEN
                        APPLY "TAB" TO bt-add.
                END.
            END.
        END.
    END.
    ELSE DO:
        ASSIGN c-sc-codigo-browse = "".
        RUN pi-ins-sem-ccusto.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-window
ON CHOOSE OF bt-ajuda IN FRAME f-formation /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-window
ON CHOOSE OF bt-cancelar IN FRAME f-formation /* Cancelar */
DO:

    IF  l-integra-cn-in-medicao THEN DO:
        IF  NOT AVAIL contrato-for THEN
            FIND FIRST contrato-for
                 WHERE ROWID(contrato-for) = v-row-parent NO-LOCK NO-ERROR.

        RUN inp/inapi405.p (INPUT contrato-for.nr-contrato).
    END.

    apply "close" to this-procedure.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma w-window
ON CHOOSE OF bt-confirma IN FRAME f-formation /* Confirma */
DO:
  assign l-grava = yes.

  for each tt-matriz where tt-matriz.nr-contrato = contrato-for.nr-contrato:  
      if tt-matriz.perc-rateio = 0 then do:
         run utp/ut-msgs.p (input "show",
                           input 8462,
                           input "").
         assign l-grava = no.
         return 'adm-error'.
      end.
      
      &if '{&bf_mat_versao_ems}' >= '2.062' &THEN
       IF  l-unidade-negocio AND l-mat-unid-negoc THEN DO:
           IF  NOT CAN-FIND(FIRST unid-negoc NO-LOCK
               WHERE unid-negoc.cod-unid-negoc = tt-matriz.cod-unid-negoc) THEN DO:
               {utp/ut-field.i mgind unid-negoc cod-unid-negoc 1}
               RUN utp/ut-msgs.p (INPUT "show",
                                  INPUT 2,
                                  INPUT RETURN-VALUE).
               ASSIGN l-grava = NO.
               RETURN 'adm-error'.
           END. 
       END.
      &endif
  end.

  assign de-percent = 0.
  for each tt-matriz where tt-matriz.nr-contrato = contrato-for.nr-contrato:  
      assign de-percent = de-percent + tt-matriz.perc-rateio.
  end.

  if de-percent > 0 and de-percent < 100 then do:
     run utp/ut-msgs.p (input "show",
                        input 8450,
                        input return-value).
     if return-value = "no" then do:
        assign l-grava = no.
        return 'adm-error'.
     end.
     if return-value = "yes" then do:
        assign l-grava = no.
        return 'adm-error'.
     end.
  end.  
  else do:
     assign l-grava = yes.
  end.

  if de-percent > 100 then do:
     run utp/ut-msgs.p (input "show",
                        input 8451,
                        input "").
     assign l-grava = no.
     return 'adm-error'.
  end.
  else do:
     assign l-grava = yes.
  end.

  if de-percent = 0 then do:
     assign l-elimina = yes.
  end.
  else do:
     assign l-elimina = no.
  end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del w-window
ON CHOOSE OF bt-del IN FRAME f-formation
DO:

    DEF VAR i-cont  AS INT NO-UNDO.
    DEF VAR l-achou AS LOG NO-UNDO.
    
    IF (br-target-browse:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} <> 0) THEN DO:
            
        DO i-cont = 1 TO br-target-browse:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}:
          
            ASSIGN l-achou = NO
                   l-achou = br-target-browse:FETCH-SELECTED-ROW(i-cont) IN FRAME {&FRAME-NAME}.

            IF l-achou THEN DO:
                FIND FIRST b-tt-matriz
                     WHERE b-tt-matriz.ct-codigo = tt-matriz.ct-codigo
                       AND b-tt-matriz.sc-codigo = tt-matriz.sc-codigo NO-ERROR.
              IF AVAIL b-tt-matriz THEN DO:
                  IF AVAIL tt-matriz THEN DO:
                      ASSIGN de-perc-total = de-perc-total - tt-matriz.perc-rateio.
                      DISP de-perc-total WITH FRAME {&FRAME-NAME}.
                  END.
                  DELETE b-tt-matriz.
              END.
            END.
        END.
    END.

    {&OPEN-QUERY-br-target-browse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-faixa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-faixa w-window
ON CHOOSE OF bt-faixa IN FRAME f-formation /* Faixa */
DO:
    assign c-conta-ini   = ""
           c-conta-fim   = "ZZZZZZZZZZZZZZZZZZZZ".

    run cnp/cn0201c6.w(input-output c-conta-ini, 
                       input-output c-conta-fim,
                       output l-cancela).
    if  not l-cancela THEN DO:
        RUN pi-cria-tt-conta.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-filtro w-window
ON CHOOSE OF bt-filtro IN FRAME f-formation /* Filtro */
DO:
    run cdp/cd9017.w.
    
    RUN pi-cria-tt-conta.
    
    /*{&OPEN-QUERY-br-source-browse}

    {&OPEN-QUERY-br-conta-cc-custo}*/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-invest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-invest w-window
ON CHOOSE OF bt-invest IN FRAME f-formation /* Invest */
DO:
   APPLY "choose":u to bt-confirma IN FRAME {&FRAME-NAME}.
   IF  RETURN-VALUE = "adm-error":U THEN
       RETURN NO-APPLY.

   RUN pi-commit.
   RUN dispatch IN wh-browse ('open-query':U).

   {&WINDOW-NAME}:SENSITIVE = NO.
   RUN cnp/cn0201f1.w PERSISTENT SET wh-program (INPUT ROWID(contrato-for),
                                                 INPUT THIS-PROCEDURE,
                                                 INPUT TABLE tt-matriz).
   RUN dispatch IN wh-program ('initialize':U).
   WAIT-FOR CLOSE OF wh-program.
   {&WINDOW-NAME}:SENSITIVE = YES.
   APPLY 'entry' TO {&WINDOW-NAME}.

   {&OPEN-QUERY-br-target-browse}
   ASSIGN l-invest = YES.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME f-formation /* Ok */
DO:

  APPLY "choose":U TO bt-confirma IN FRAME {&FRAME-NAME}.
  IF  RETURN-VALUE = "adm-error":U THEN
      RETURN NO-APPLY.

  RUN pi-commit.

  IF RETURN-VALUE = "NOK":U THEN
      RETURN NO-APPLY.

  RUN dispatch IN wh-browse ('open-query':U).

  FOR EACH  tt-matriz
      WHERE tt-matriz.nr-contrato = contrato-for.nr-contrato: 
      DELETE tt-matriz.
  END.

  apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-i-num-ord-inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-i-num-ord-inv w-window
ON F5 OF fi-i-num-ord-inv IN FRAME f-formation /* Num Ordem Inv */
DO:
    find estabelec where 
         estabelec.cod-estabel = contrato-for.cod-estabel no-lock no-error.
    if avail estabelec then
        assign i-empresa = estabelec.ep-codigo.
    else
        assign i-empresa = ?. 

    {include/zoomvar.i &prog-zoom=ivzoom/z05iv043.w
                   &campo=fi-i-num-ord-inv
                   &campozoom=num-ord-magnus
                   &parametros="run pi-seta-inicial in wh-pesquisa (input i-empresa)."
                   &frame="f-formation"}
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-i-num-ord-inv w-window
ON MOUSE-SELECT-DBLCLICK OF fi-i-num-ord-inv IN FRAME f-formation /* Num Ordem Inv */
DO:
    APPLY 'F5' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-conta-cc-custo
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */


{include/i-frm040.i}
/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}
fi-i-num-ord-inv:LOAD-MOUSE-POINTER("image/lupa.cur":U) IN FRAME {&FRAME-NAME}.
{utp/ut-field.i mgadm conta-contab tipo 1}.

&if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
    DEFINE VARIABLE cAuxTraducao001 AS CHARACTER NO-UNDO.
    ASSIGN cAuxTraducao001 = {adinc/i02ad047.i 03}.
    RUN utp/ut-list.p (INPUT-OUTPUT cAuxTraducao001).
    ASSIGN  c-lista-tipo = cAuxTraducao001.
&else
    ASSIGN c-lista-tipo = {adinc/i02ad047.i 03}.
&endif

{utp/ut-liter.i Total * R}
  assign de-perc-total:label in frame {&frame-name} = return-value.

&if '{&bf_mat_versao_ems}' >= '2.062' &then
    {utp/ut-liter.i F5_para_zoom * R}
    assign c-item = return-value.
    
    on F5, mouse-select-dblclick
       of tt-matriz.cod-unid-negoc in browse br-target-browse
    do:
        IF  AVAIL tt-matriz THEN DO:
            {method/zoomfields.i &ProgramZoom  = "inzoom/z01in745.w"
                                 &FieldZoom1   = "cod-unid-negoc"
                                 &FieldScreen1 = "tt-matriz.cod-unid-negoc"
                                 &browse1      = br-target-browse}
        END.
    end.

    on entry of tt-matriz.cod-unid-negoc in browse br-target-browse
    do:
       status input c-item.
    end.

    on LEAVE of tt-matriz.cod-unid-negoc in browse br-target-browse
    do:
       IF  INPUT BROWSE br-target-browse tt-matriz.cod-unid-negoc <> ""
       AND VALID-HANDLE(h-cdapi024) then do:
           EMPTY temp-table tt-erro-aux.
           
           /*UN X Estabelecimento*/
           run ValidaUnidadeNegocioEstabel in h-cdapi024 (input contrato-for.cod-estabel,
                                                          input TODAY,
                                                          input INPUT BROWSE br-target-browse tt-matriz.cod-unid-negoc, 
                                                          output table tt-erro-aux).

           /*UN X Usu rio*/
           run ValidaUnidadeNegocioUsuario in h-cdapi024 (input contrato-for.cod-comprado,
                                                          input INPUT BROWSE br-target-browse tt-matriz.cod-unid-negoc,
                                                          output table tt-erro-aux append).

           /*Validacao Restricao Unidade Negocio*/

            run pi_valida_conta_contabil in h_api_cta_ctbl (input  String(i-empresa),                 /* EMPRESA EMS2 */
                                                            input  contrato-for.cod-estabel,  /* ESTABELECIMENTO EMS2 */
                                                            input  input browse br-target-browse tt-matriz.cod-unid-negoc,   /* UNIDADE NEGàCIO */
                                                            input  "",                        /* PLANO CONTAS */ 
                                                            input  tt-matriz.ct-codigo,    /* CONTA */
                                                            input  "",                        /* PLANO CCUSTO */ 
                                                            input  tt-matriz.sc-codigo,    /* CCUSTO */
                                                            input  today, /* DATA TRANSACAO */
                                                            output table tt_log_erro).        /* ERROS */
                        
            IF CAN-FIND(tt_log_erro) OR RETURN-VALUE = "NOK" THEN DO:    
               RUN pi_transfere_tt_log_erro.
        
               RUN cdp/cd0666.w (INPUT TABLE tt-erro).
        
               RETURN NO-APPLY.                             
            END.    
       end.

       IF  CAN-FIND(FIRST tt-erro-aux) THEN DO:
           FOR EACH tt-erro-aux:
               RUN utp/ut-msgs.p (INPUT "show",
                                  INPUT 15825,
                                  INPUT tt-erro-aux.mensagem
                                        + '~~' +
                                        tt-erro-aux.mensagem).
           END.
           RETURN NO-APPLY.
       END.
       
       IF  CAN-FIND(FIRST btt-matriz
                    WHERE RECID(btt-matriz)        <> RECID(tt-matriz)
                      AND btt-matriz.ct-codigo = tt-matriz.ct-codigo
                      AND btt-matriz.sc-codigo = tt-matriz.sc-codigo
                      AND btt-matriz.cod-unid-negoc = INPUT BROWSE br-target-browse tt-matriz.cod-unid-negoc) THEN DO:
                        assign tt-matriz.cod-unid-negoc:screen-value in BROWSE br-target-browse = "":U.
           {utp/ut-liter.i Conta_Cont bil_e_Unidade_de_Neg¢cio * R}
           RUN utp/ut-msgs.p (INPUT "show",
                              INPUT 8,
                              INPUT RETURN-VALUE).
           RETURN NO-APPLY.
       END.
    END.
&endif

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-window  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-window  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-window  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
  THEN DELETE WIDGET w-window.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-window  _DEFAULT-ENABLE
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
  DISPLAY fi-i-num-ord-inv de-perc-total 
      WITH FRAME f-formation IN WINDOW w-window.
  IF AVAILABLE contrato-for THEN 
    DISPLAY contrato-for.nr-contrato contrato-for.des-contrat 
      WITH FRAME f-formation IN WINDOW w-window.
  ENABLE RECT-1 rt-key-parent rt-source-browse rt-source-browse-2 RECT-7 
         fi-i-num-ord-inv br-source-browse br-conta-cc-custo bt-filtro bt-faixa 
         bt-add bt-del bt-invest br-target-browse bt-confirma bt-ok bt-cancelar 
         bt-ajuda 
      WITH FRAME f-formation IN WINDOW w-window.
  {&OPEN-BROWSERS-IN-QUERY-f-formation}
  VIEW w-window.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-window 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  &if "{&bf_mat_versao_ems}" >= "2.062" &then
   IF  VALID-HANDLE(h-cdapi024)  THEN RUN pi-finalizar IN h-cdapi024.
   ASSIGN h-cdapi024  = ?.
  &endif
  
  delete object h_api_ccusto.
  delete object h_api_cta_ctbl.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable w-window 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  &if '{&bf_mat_versao_ems}' >= '2.062' &THEN
      ASSIGN tt-matriz.cod-unid-negoc:VISIBLE IN BROWSE br-target-browse = l-unidade-negocio AND l-mat-unid-negoc.
  &endif
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-window 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-window 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  {utp/ut9000.i "CN0201F" "2.00.00.034"}

  /* Dispatch standard ADM method.                             */
  run pi-show-master-record.

  run prgint/utb/utb742za.py persistent set h_api_ccusto.
  run prgint/utb/utb743za.py persistent set h_api_cta_ctbl.

  

  &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
      IF  l-unidade-negocio THEN DO:
          IF  NOT VALID-HANDLE(h-cdapi024) THEN
              RUN cdp/cdapi024.p persistent set h-cdapi024.
      END.

      IF NOT VALID-HANDLE (h-boiv029) THEN DO:
          RUN ivbo/boiv029.p PERSISTENT SET h-boiv029.
          RUN openQueryStatic IN h-boiv029 (INPUT "Main":U).
      END.
  &ENDIF

  find contrato-for where rowid(contrato-for) = v-row-parent no-lock no-error.
  for each matriz-rat-contr where matriz-rat-contr.nr-contrato = contrato-for.nr-contrato no-lock:  
      create tt-matriz.
      assign tt-matriz.nr-contrato    = matriz-rat-contr.nr-contrato
             tt-matriz.ct-codigo      = matriz-rat-contr.ct-codigo
             tt-matriz.sc-codigo      = matriz-rat-contr.sc-codigo
             tt-matriz.perc-rateio    = matriz-rat-contr.perc-rateio
             de-perc-total            = de-perc-total
                                      + tt-matriz.perc-rateio.
      &if '{&bf_mat_versao_ems}' >= '2.062' &THEN
          ASSIGN tt-matriz.cod-unid-negoc = matriz-rat-contr.cod-unid-negoc.
      &endif
  end.

  find first param-global no-lock no-error.
  assign i-empresa = param-global.empresa-prin.

  /*Formato de Conta e Centro de Custo - utilizado para apresentar os dados formatados em tela*/
  RUN pi-busca-formatos IN THIS-PROCEDURE.

  find first pedido-compr where
             pedido-compr.nr-contrato = contrato-for.nr-contrato no-lock no-error.
  if avail pedido-compr then do:

     find first estabelec where
                estabelec.cod-estabel = pedido-compr.cod-estabel no-lock no-error.
     if avail estabelec then
        assign i-empresa = estabelec.ep-codigo.
  end.                              
  

  if valid-handle(THIS-PROCEDURE) then
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
   /* Code placed here will execute AFTER standard behavior.    */

  assign c-conta = ""
         i-est-ini1 = 0
         i-est-fim1 = 5
         i-est-ini2 = 6
         i-est-fim2 = 9.

  find param-global no-lock no-error.
  if avail param-global then do:
     if param-global.modulo-ce then 
        assign i-est-ini1 = 1
               i-est-fim1 = 3
               i-est-ini2 = 5
               i-est-fim2 = 6.
  end.  

  ASSIGN bt-invest:VISIBLE IN FRAME {&FRAME-NAME} = IF  l-integra-cn-in-medicao THEN YES ELSE NO.

  &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
      ASSIGN bt-invest:HIDDEN IN FRAME f-formation = YES.

      FIND contrato-for WHERE ROWID(contrato-for) = v-row-parent NO-LOCK NO-ERROR.
      IF AVAIL contrato-for THEN DO:      
          IF param-global.modulo-in THEN DO:      
              ASSIGN fi-i-num-ord-inv:HIDDEN IN FRAME f-formation = NO.
    
              &IF '{&bf_mat_versao_ems}' >= '2.07' &THEN
                  ASSIGN fi-i-num-ord-inv:SCREEN-VALUE IN FRAME f-formation = STRING(contrato-for.num-ord-inv).
              &ELSE
                  ASSIGN fi-i-num-ord-inv:SCREEN-VALUE IN FRAME f-formation = SUBSTRING(contrato-for.char-1,1,9).
              &ENDIF
          END.
          ELSE
              ASSIGN fi-i-num-ord-inv:HIDDEN IN FRAME f-formation = YES.
      END.

  &ELSE
  
      ASSIGN fi-i-num-ord-inv:HIDDEN IN FRAME f-formation = YES.

  &ENDIF

  
  RUN pi-cria-tt-conta IN THIS-PROCEDURE.

  /*Ponto UPC YAMANA*/
  IF  c-nom-prog-upc-mg97 <> "" THEN DO:
      RUN VALUE(c-nom-prog-upc-mg97) (INPUT "after-local-initialize", 
                                      INPUT "after-local-initialize",
                                      INPUT THIS-PROCEDURE,
                                      INPUT ?,
                                      INPUT "",
                                      INPUT ?).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-temp-table w-window 
PROCEDURE pi-atualiza-temp-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /*Esta procedure ‚ executada pelo cn0201f1*/
    DEFINE INPUT PARAMETER TABLE FOR tt-matriz.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-busca-ccusto w-window 
PROCEDURE pi-busca-ccusto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    EMPTY TEMP-TABLE tt_ccusto_cta_integr.
    EMPTY TEMP-TABLE tt-conta-cc-custo.

    run pi_busca_ccustos_x_cta_ctbl in h_api_ccusto (input  STRING(i-empresa),          /* EMPRESA EMS2 */
                                                     input  "",                         /* ESTABELECIMENTO EMS2 */
                                                     input  YES,                        /* TODOS OS ESTABELECIMENTOS? */
                                                     input  "",                         /* CODIGO DO PLANO CCUSTO */
                                                     input  "",                         /* UNIDADE DE NEGOCIO */
                                                     input  today,                      /* DATA DE TRANSACAO */
                                                     input  table tt_cta_integr_aux1,   /* CONTAS CONTæBEIS */
                                                     output table tt_ccusto_cta_integr, /* CCUSTOS UTILIZADOS PELAS CONTAS */
                                                     output table tt_log_erro).         /* ERROS */ 

    for each tt_ccusto_cta_integr:
        CREATE tt-conta-cc-custo.
        ASSIGN tt-conta-cc-custo.sc-codigo = tt_ccusto_cta_integr.ttv_cod_ccusto
               tt-conta-cc-custo.sc-desc   = tt_ccusto_cta_integr.ttv_des_ccusto.
    end.

    RETURN "ok".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-busca-contas w-window 
PROCEDURE pi-busca-contas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF INPUT PARAM c-modulo AS CHAR NO-UNDO.

    DEF VAR i-tipo-finalid AS INT NO-UNDO.
    DEF VAR icont          AS INT NO-UNDO.

    ASSIGN i-lista-finalid = 0.

    IF l-estoque THEN
        ASSIGN i-tipo-finalid = 10. /* 10 - NÆo gera lan‡aamentos */
    ELSE
        ASSIGN i-tipo-finalid = 1. /* 1 - NÆo gera lan‡aamentos */

    run pi_busca_ctas_integr in h_api_cta_ctbl (input  STRING(i-empresa),   /* EMPRESA EMS2 */
                                                input  c-modulo,            /* MODULO */
                                                input  "",                  /* PLANO CONTAS */ 
                                                input  "(nenhum)",          /* FINALIDADES */
                                                input  today,               /* DATA DE TRANSACAO */  
                                                output table tt_cta_integr, /* CONTAS */
                                                output table tt_log_erro).  /* ERROS */

    if l-despesa and
       l-receita and
       l-venda   and
       l-passivo and
       l-ativo   and
       l-titulo  and
       l-estoque        and
       l-investimento   THEN DO:

        FOR EACH tt_cta_integr
           WHERE tt_cta_integr.ttv_cod_cta_ctbl >= c-conta-ini
             AND tt_cta_integr.ttv_cod_cta_ctbl <= c-conta-fim:
    
            /* Salvar todas as contas */
            CREATE tt_cta_integr_aux.
            BUFFER-COPY tt_cta_integr TO tt_cta_integr_aux.

            IF c-modulo = "CEP" THEN /* Modulo CEP */
                assign i-lista-finalid = {adinc/i05ad049.i 6 tt_cta_integr.ttv_ind_finalid_ctbl_cta}.
            ELSE DO: /* Modulo INP */
                run pi_valida_cta_ctbl_integr in h_api_cta_ctbl (input  STRING(i-empresa),  /* EMPRESA EMS2 */
                                                                 input  c-modulo,           /* MODULO */
                                                                 input  "",                 /* PLANO CONTAS */ 
                                                                 input  tt_cta_integr.ttv_cod_cta_ctbl, /* CONTA */
                                                                 input  "Gera lan‡amentos", /* FINALIDADES */
                                                                 input  today,              /* DATA DE TRANSACAO */  
                                                                 output table tt_log_erro). /* ERROS */
                IF RETURN-VALUE = "OK" THEN
                    ASSIGN i-lista-finalid = 2. /* 2 - Gera lan‡amentos */
                ELSE
                    ASSIGN i-lista-finalid = 1. /* 1 - NÆo gera lan‡aamentos */
            END.
    
            CREATE tt-conta.
            ASSIGN tt-conta.ct-codigo = tt_cta_integr.ttv_cod_cta_ctbl
                   tt-conta.ct-desc   = tt_cta_integr.ttv_des_titulo
                   tt-conta.tipo      = tt_cta_integr.ttv_num_tip_cta_ctbl
                   tt-conta.situacao  = tt_cta_integr.ttv_num_sit_cta_ctbl
                   tt-conta.finalid   = i-lista-finalid
                   tt-conta.c-modulo  = c-modulo.
            
        END.
    END.
    ELSE DO:

        FOR EACH tt_cta_integr
           WHERE tt_cta_integr.ttv_cod_cta_ctbl >= c-conta-ini
             AND tt_cta_integr.ttv_cod_cta_ctbl <= c-conta-fim:
    
            /* Salvar todas as contas */
            CREATE tt_cta_integr_aux.
            BUFFER-COPY tt_cta_integr TO tt_cta_integr_aux.
    
            IF c-modulo = "CEP" THEN /* Modulo CEP */
                assign i-lista-finalid = {adinc/i05ad049.i 6 tt_cta_integr.ttv_ind_finalid_ctbl_cta}.
            ELSE DO: /* Modulo INP */
                run pi_valida_cta_ctbl_integr in h_api_cta_ctbl (input  STRING(i-empresa),  /* EMPRESA EMS2 */
                                                                 input  c-modulo,           /* MODULO */
                                                                 input  "GERAL",            /* PLANO CONTAS */ 
                                                                 input  tt_cta_integr.ttv_cod_cta_ctbl, /* CONTA */
                                                                 input  "Gera lan‡amentos", /* FINALIDADES */
                                                                 input  today,              /* DATA DE TRANSACAO */  
                                                                 output table tt_log_erro). /* ERROS */
                IF RETURN-VALUE = "OK" THEN
                    ASSIGN i-lista-finalid = 2. /* 2 - Gera lan‡amentos */
                ELSE
                    ASSIGN i-lista-finalid = 1. /* 1 - NÆo gera lan‡aamentos */
            END.
    
            IF ( (l-despesa and tt_cta_integr.ttv_num_tip_cta_ctbl = 1) or
                 (l-receita and tt_cta_integr.ttv_num_tip_cta_ctbl = 2) or
                 (l-venda   and tt_cta_integr.ttv_num_tip_cta_ctbl = 3) or
                 (l-passivo and tt_cta_integr.ttv_num_tip_cta_ctbl = 4) or
                 (l-ativo   and tt_cta_integr.ttv_num_tip_cta_ctbl = 5) or
                 (l-titulo  and tt_cta_integr.ttv_num_tip_cta_ctbl = 6)
               )
                
               and 
               (i-lista-finalid <> i-tipo-finalid) /* Se for estoque tem q ser <> 10, investimento <> 1, demais valoares est  correto */
               THEN DO:
    
                CREATE tt-conta.
                ASSIGN tt-conta.ct-codigo = tt_cta_integr.ttv_cod_cta_ctbl
                       tt-conta.ct-desc   = tt_cta_integr.ttv_des_titulo
                       tt-conta.tipo      = tt_cta_integr.ttv_num_tip_cta_ctbl
                       tt-conta.situacao  = tt_cta_integr.ttv_num_sit_cta_ctbl
                       tt-conta.finalid   = i-lista-finalid
                       tt-conta.c-modulo  = c-modulo.
            END.
        END.
    END.

    RETURN "ok".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-busca-formatos w-window 
PROCEDURE pi-busca-formatos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Retorna formato da conta contabil */
  run pi_retorna_formato_cta_ctbl in h_api_cta_ctbl (input  STRING(i-empresa),   /* EMPRESA EMS2 */
                                                     input  "",                  /* PLANO CONTAS */
                                                     input  TODAY,               /* DATA DE TRANSACAO */
                                                     output c-formato-conta,     /* FORMATO CONTA */
                                                     output table tt_log_erro).  /* ERROS */
  /* Retorna formato do centro de custo */
  run pi_retorna_formato_ccusto in h_api_ccusto (input  STRING(i-empresa),   /* EMPRESA EMS2 */
                                                 input  "",                  /* PLANO CCUSTO */
                                                 input  TODAY,               /* DATA DE TRANSACAO */
                                                 output c-formato-ccusto,    /* FORMATO CCUSTO */
                                                 output table tt_log_erro).  /* ERROS */  

  IF c-formato-conta = "" THEN
      ASSIGN c-formato-conta = "x(20)".
  
  IF c-formato-ccusto = "" THEN
      ASSIGN c-formato-ccusto = "x(20)".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-commit w-window 
PROCEDURE pi-commit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DO TRANSACTION:
        
        &if "{&bf_mat_versao_ems}":U >= "2.04":U &then
            /** Integracao Contratos - limpa registros temp-table integra‡Æo **/
            {cnp/cnapi020.i4}
        &endif
    
        find first param-global no-lock no-error.
        if l-grava = yes then do: 
    
           if l-elimina = yes then do:
              find first contrato-for where rowid(contrato-for) = v-row-parent no-lock no-error.
              for each matriz-rat-contr where matriz-rat-contr.nr-contrato = contrato-for.nr-contrato exclusive-lock:
                  &if "{&bf_mat_versao_ems}":U >= "2.04":U &then  
                      /* Integracao Contratos - criar registro para elimina‡Æo */
                      {cnp/cnapi020.i1 2 3 CN0201F " " matriz-rat-contr matriz-rat-contr}
                  &endif
                  delete matriz-rat-contr.
              end.
           end.
    
           for each matriz-rat-contr where matriz-rat-contr.nr-contrato = contrato-for.nr-contrato exclusive-lock:
               &if "{&bf_mat_versao_ems}":U >= "2.04":U &then  
                   /* Integracao Contratos - criar registro para elimina‡Æo */
                   {cnp/cnapi020.i1 2 3 CN0201F " " matriz-rat-contr matriz-rat-contr}
               &endif
               delete matriz-rat-contr.
           end.
    
           for each tt-matriz:
               create matriz-rat-contr.
               assign matriz-rat-contr.nr-contrato    = tt-matriz.nr-contrato
                      matriz-rat-contr.ct-codigo      = tt-matriz.ct-codigo.
                      matriz-rat-contr.sc-codigo      = tt-matriz.sc-codigo.
                      matriz-rat-contr.perc-rateio    = tt-matriz.perc-rateio.
               &if '{&bf_mat_versao_ems}' >= '2.062' &THEN
                     ASSIGN matriz-rat-contr.cod-unid-negoc = tt-matriz.cod-unid-negoc.
               &endif
               &if "{&bf_mat_versao_ems}":U >= "2.04":U &then  
                   /* Integracao Contratos - criar registro para inclusao */
                   {cnp/cnapi020.i1 2 1 CN0201F " " matriz-rat-contr matriz-rat-contr}
               &endif
           end.
    
           &if "{&bf_mat_versao_ems}":U >= "2.04":U &then  
               /* Integracao Contratos - executa API integra‡Æo e valida erros */  
               {cnp/cnapi020.i2 ADM-ERROR yes}
           &endif
    
        end.
    
        IF  l-integra-cn-in-medicao THEN DO:
            IF  NOT AVAIL contrato-for THEN
                FIND contrato-for WHERE ROWID(contrato-for) = v-row-parent NO-LOCK NO-ERROR.
    
            RUN inp/inapi405.p (INPUT contrato-for.nr-contrato).
        END.    
        
        &IF '{&bf_mat_versao_ems}' >= '2.062' &THEN  
            EMPTY TEMP-TABLE tt-erro-aux.
            EMPTY TEMP-TABLE tt-matriz-aux.
            IF INPUT FRAME f-formation fi-i-num-ord-inv > 0 
               AND param-global.modulo-in
               AND   l-mat-unid-negoc THEN DO:
            
                FOR EACH matriz-rat-contr NO-LOCK
                   WHERE matriz-rat-contr.nr-contrato = contrato-for.nr-contrato:
              
                    FIND FIRST tt-matriz-aux NO-LOCK
                         WHERE tt-matriz-aux.cod-unid-neg = matriz-rat-contr.cod-unid-neg NO-ERROR.
                    IF NOT AVAIL tt-matriz-aux THEN DO:
                        CREATE tt-matriz-aux.
                        ASSIGN tt-matriz-aux.cod-unid-neg = matriz-rat-contr.cod-unid-neg.
                    END.
                    ASSIGN tt-matriz-aux.perc-rateio  = tt-matriz-aux.perc-rateio + matriz-rat-contr.perc-rateio.
                END.
                assign iEmpresa =  (param-global.empresa-prin).
                IF NOT AVAIL estabelec 
                   OR (AVAIL estabelec 
                         AND estabelec.cod-estabel <> contrato-for.cod-estabel) THEN
                    FIND FIRST estabelec WHERE 
                               estabelec.cod-estabel = contrato-for.cod-estabel NO-LOCK NO-ERROR.
/*                 &IF "{&mguni_version}" >= "2.071" &THEN                */
/*                 if avail estabelec AND estabelec.ep-codigo <> "" then  */
/*                 &ELSE                                                  */
/*                 if avail estabelec AND estabelec.ep-codigo <> 0 then   */
/*                 &ENDIF                                                 */
                    ASSIGN iempresa =  (estabelec.ep-codigo).
        
                RUN recebeEmpresa IN h-boiv029 (INPUT iempresa).
                RUN ValidateMatrizUnidNegoc IN h-boiv029 (INPUT TABLE tt-matriz-aux, 
                                                          INPUT INPUT FRAME f-formation fi-i-num-ord-inv, 
                                                          OUTPUT TABLE tt-erro-aux).

                IF NOT l-unidade-negocio AND 
                   NOT CAN-FIND (FIRST tt-matriz-aux NO-LOCK) AND 
                   NOT CAN-FIND (FIRST tt-erro-aux NO-LOCK)   THEN DO:
                    RUN utp/ut-msgs.p ("msg":U, 17006, "Deve haver matriz de rateio cadastrada quando informado ordem de investimento").
                    CREATE tt-erro-aux.
                    ASSIGN tt-erro-aux.i-sequen = 1
                           tt-erro-aux.cd-erro  = 17006
                           tt-erro-aux.mensagem = RETURN-VALUE.
                END.
            END.
            IF CAN-FIND (FIRST tt-erro-aux) THEN DO:
                RUN cdp/cd0666.w (INPUT TABLE tt-erro-aux).          
                UNDO, RETURN "NOK".
            END.
            ELSE DO:      
                FIND CURRENT contrato-for EXCLUSIVE-LOCK NO-ERROR.
                                
                &IF '{&bf_mat_versao_ems}' >= '2.07' &THEN
                    ASSIGN contrato-for.num-ord-inv = INPUT FRAME f-formation fi-i-num-ord-inv.
                &ELSE
                    ASSIGN OVERLAY(contrato-for.char-1,1,9) = STRING(INPUT FRAME f-formation fi-i-num-ord-inv).
                &ENDIF
            
                FIND CURRENT contrato-for NO-LOCK NO-ERROR.
            END.
        &ENDIF  
    
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-tt-conta w-window 
PROCEDURE pi-cria-tt-conta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    EMPTY TEMP-TABLE tt-conta.
    EMPTY TEMP-TABLE tt-conta-cc-custo.

    ASSIGN c-ct-codigo-browse = "".

    {&OPEN-QUERY-br-source-browse}
    {&OPEN-QUERY-br-conta-cc-custo}

    IF l-investimento THEN
        RUN pi-busca-contas (INPUT "INP").
    
    IF l-estoque THEN
        RUN pi-busca-contas (INPUT "CEP").

    {&OPEN-QUERY-br-source-browse}
    {&OPEN-QUERY-br-conta-cc-custo}

    RETURN "ok".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-delete-from-target w-window 
PROCEDURE pi-delete-from-target :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   define input parameter v-row-target-browse as rowid no-undo.

  /* Jamais remova a definição do include a seguir de dentro da lógica do programa */
  {include/i-frm180.i}

   /*
   find tt-tabela-destino where rowid( tt-tabela-destino ) = v-row-target-browse exclusive-lock no-error.
   if available tt-tabela-destino then
      delete tt-tabela-destino.     
   */  

/* find tt-matriz where rowid(tt-matriz) = v-row-target-browse exclusive-lock no-error.  */
   if available tt-matriz then do:
      assign de-perc-total = de-perc-total - tt-matriz.perc-rateio.
      disp de-perc-total with frame {&frame-name}.
      delete tt-matriz.     
   end.


  /* Jamais remova a definição do include a seguir de dentro da lógica do programa */
  {include/i-frm185.i}


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ins w-window 
PROCEDURE pi-ins :
/*------------------------------------------------------------------------------
  Purpose   : Incluir no browse destiono o registro selecionado no browse origem     
  Parameters: 
------------------------------------------------------------------------------*/
  /* Jamais remova a definição do include a seguir de dentro da lógica do programa */

  {include/i-frm157.i}
  DEF VAR c-cod-estabel LIKE estabelec.cod-estabel.
  DEF VAR i-num-ord-inv AS INTEGER FORMAT ">>>>>,>>9":U INITIAL 0.
  
  EMPTY TEMP-TABLE tt-erro-aux1.
  
  IF NOT AVAIL contrato-for THEN
    find contrato-for where rowid(contrato-for) = v-row-parent no-lock no-error.
  ASSIGN c-cod-estabel = contrato-for.cod-estabel.
         i-num-ord-inv = &IF '{&bf_mat_versao_ems}' >= '2.062' &THEN 
                              INPUT FRAME {&FRAME-NAME} fi-i-num-ord-inv 
                         &ELSE
                              0
                         &ENDIF.
  
  &IF DEFINED(bf_mat_conta_contab_inp) &THEN
      &IF '{&bf_mat_versao_ems}' < '2.062' &THEN
      IF  l-integra-cn-in-medicao THEN DO:
      &ENDIF
          {inp/inapi060.i c-cod-estabel c-ct-codigo-browse c-sc-codigo-browse i-num-ord-inv NO tt-erro-aux1}
          
          IF  RETURN-VALUE = "NOK":U THEN DO:
              IF CAN-FIND (FIRST tt-erro-aux1) THEN DO:
                  RUN cdp/cd0669.w (INPUT TABLE tt-erro-aux1).
                  RETURN 'adm-error'.  
              END.
          END.
      &IF '{&bf_mat_versao_ems}' < '2.062' &THEN
      END.
      &ENDIF
  &ENDIF

  /* Caso o include a seguir nao atenda suas necessidades, apague-o  e crie sua propria
  logica usando-o como modelo. */   

  &if '{&bf_mat_versao_ems}' < '2.062' &THEN
      for each tt-matriz:
          if tt-matriz.ct-codigo = c-ct-codigo-browse AND
             tt-matriz.sc-codigo = c-sc-codigo-browse then do:
             run utp/ut-msgs.p (input "show",
                                input 8452,
                                input "").
             return 'adm-error'.
          end.
      end.
  &else
      for each tt-matriz:
          if tt-matriz.ct-codigo = c-ct-codigo-browse AND
             tt-matriz.sc-codigo = c-sc-codigo-browse AND
             tt-matriz.cod-unid-negoc = "" then do:
             run utp/ut-msgs.p (input "show",
                                input 8452,
                                input "").
             return 'adm-error'.
          end.
      end.
  &endif

  FIND FIRST tt-conta
       WHERE tt-conta.ct-codigo = c-ct-codigo-browse NO-ERROR.

  IF (tt-conta.tipo <> 1 AND tt-conta.tipo <> 4 AND 
      tt-conta.tipo <> 5 AND tt-conta.tipo <> 2) OR tt-conta.situacao <> 3 THEN DO:
      RUN utp/ut-msgs.p (INPUT "show",
                         INPUT 3297,
                         INPUT "").
      RETURN 'adm-error'.
  END.

  find first param-global no-lock no-error.
  IF  param-global.modulo-ce THEN DO:
      IF  tt-conta.finalid  = 0 OR  
         (tt-conta.finalid <> 1 AND tt-conta.finalid <> 2 AND
          tt-conta.finalid <> 3 AND tt-conta.finalid <> 5 AND tt-conta.finalid <> 6) THEN DO:
          RUN utp/ut-msgs.p (INPUT "show",
                             INPUT 3298,
                             INPUT "").
          RETURN 'adm-error'.
      END.
  END.

  CREATE tt-matriz.
  ASSIGN tt-matriz.ct-codigo   = c-ct-codigo-browse
                 tt-matriz.sc-codigo   = c-sc-codigo-browse
                 tt-matriz.nr-contrato = contrato-for.nr-contrato
                 tt-matriz.perc-rateio = 0.
 

  {&OPEN-QUERY-br-target-browse}


  /*{include/i-frm160.i}*/

  /* Jamais remova a definição do include a seguir de dentro da lógica do programa */
  {include/i-frm165.i}   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ins-sem-ccusto w-window 
PROCEDURE pi-ins-sem-ccusto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {include/i-frm157.i}
  DEF VAR c-cod-estabel LIKE estabelec.cod-estabel.
  DEF VAR i-num-ord-inv AS INTEGER FORMAT ">>>>>,>>9":U INITIAL 0.
  
  EMPTY TEMP-TABLE tt-erro-aux1.
  
  IF NOT AVAIL contrato-for THEN
    find contrato-for where rowid(contrato-for) = v-row-parent no-lock no-error.
  ASSIGN c-cod-estabel = contrato-for.cod-estabel.
         i-num-ord-inv = &IF '{&bf_mat_versao_ems}' >= '2.062' &THEN 
                              INPUT FRAME {&FRAME-NAME} fi-i-num-ord-inv 
                         &ELSE
                              0
                         &ENDIF.
  
  &IF DEFINED(bf_mat_conta_contab_inp) &THEN
      &IF '{&bf_mat_versao_ems}' < '2.062' &THEN
      IF  l-integra-cn-in-medicao THEN DO:
      &ENDIF
          {inp/inapi060.i c-cod-estabel c-ct-codigo-browse c-sc-codigo-browse i-num-ord-inv NO tt-erro-aux1}
          
          IF  RETURN-VALUE = "NOK":U THEN DO:
              IF CAN-FIND (FIRST tt-erro-aux1) THEN DO:
                  RUN cdp/cd0669.w (INPUT TABLE tt-erro-aux1).
                  RETURN 'adm-error'.  
              END.
          END.
      &IF '{&bf_mat_versao_ems}' < '2.062' &THEN
      END.
      &ENDIF
  &ENDIF

  /* Caso o include a seguir nao atenda suas necessidades, apague-o  e crie sua propria
  logica usando-o como modelo. */   

  &if '{&bf_mat_versao_ems}' < '2.062' &THEN
      for each tt-matriz:
          if tt-matriz.ct-codigo = c-ct-codigo-browse then do:
             run utp/ut-msgs.p (input "show",
                                input 8452,
                                input "").
             return 'adm-error'.
          end.
      end.
  &else
      for each tt-matriz:
          if tt-matriz.ct-codigo = c-ct-codigo-browse AND
             tt-matriz.cod-unid-negoc = "" then do:
             run utp/ut-msgs.p (input "show",
                                input 8452,
                                input "").
             return 'adm-error'.
          end.
      end.
  &endif

  FIND FIRST tt-conta
       WHERE tt-conta.ct-codigo = c-ct-codigo-browse NO-ERROR.

  IF (tt-conta.tipo <> 1 AND tt-conta.tipo <> 4 AND 
      tt-conta.tipo <> 5 AND tt-conta.tipo <> 2) OR tt-conta.situacao <> 3 THEN DO:
      RUN utp/ut-msgs.p (INPUT "show",
                         INPUT 3297,
                         INPUT "").
      RETURN 'adm-error'.
  END.

  find first param-global no-lock no-error.
  IF  param-global.modulo-ce THEN DO:
      IF  tt-conta.finalid  = 0 OR  
         (tt-conta.finalid <> 1 AND tt-conta.finalid <> 2 AND
          tt-conta.finalid <> 3 AND tt-conta.finalid <> 5 AND tt-conta.finalid <> 6) THEN DO:
          RUN utp/ut-msgs.p (INPUT "show",
                             INPUT 3298,
                             INPUT "").
          RETURN 'adm-error'.
      END.
  END.

        CREATE tt-matriz.
    ASSIGN tt-matriz.ct-codigo   = c-ct-codigo-browse
           tt-matriz.nr-contrato = contrato-for.nr-contrato
           tt-matriz.perc-rateio = 0.

  {&OPEN-QUERY-br-target-browse}


  /*{include/i-frm160.i}*/

  /* Jamais remova a definição do include a seguir de dentro da lógica do programa */
  {include/i-frm165.i}   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-show-master-record w-window 
PROCEDURE pi-show-master-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Jamais remova a definição do include a seguir de dentro da lógica do programa */

  {include/i-frm145.i}

  /* Lógica padrão para apresentação do registro da tabela pai caso esta lógica atenda 
     as suas necessidades chame o include a seguir no corpo da procedure (retire-o de
     dentro do comentário). Em caso contrário crie sua própria lógica usando a 
     do include como modelo.
  */   

  {include/i-frm150.i}  

  {include/i-frm155.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_transfere_tt_log_erro w-window 
PROCEDURE pi_transfere_tt_log_erro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE i-count AS INTEGER INITIAL 0 NO-UNDO.

    EMPTY TEMP-TABLE tt-erro NO-ERROR.

    FOR EACH tt_log_erro:

        ASSIGN i-count = i-count + 1.

        CREATE tt-erro.
        ASSIGN tt-erro.mensagem    = tt_log_erro.ttv_des_msg_erro
               tt-erro.cd-erro     = tt_log_erro.ttv_num_cod_erro
               tt-erro.i-sequen    = i-count.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-window  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "contrato-for"}
  {src/adm/template/snd-list.i "tt-matriz"}
  {src/adm/template/snd-list.i "tt-conta"}
  {src/adm/template/snd-list.i "tt-conta-cc-custo"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-window 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  if p-state = "apply-entry":u then
     apply "entry":u to bt-ok in frame {&frame-name}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION is-create-allowed w-window 
FUNCTION is-create-allowed RETURNS LOGICAL
  ( v-row-tt as rowid) : /* rowid do registro selecionado no origem */
/*------------------------------------------------------------------------------
  Purpose:  Insira aqui a l¢gica que deve verificar se o registro corrente pode ou
            nÆo ser incluido no browse de destino
    Notes:  
------------------------------------------------------------------------------*/

  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION is-delete-allowed w-window 
FUNCTION is-delete-allowed RETURNS LOGICAL
  ( v-row-target as rowid) : /* rowid do registro selecionado no destino */
/*------------------------------------------------------------------------------
  Purpose:  Insira aqui a l¢gica que deve verificar se o registro corrente pode ou
            nÆo ser eliminado do browse de destino
    Notes:  
------------------------------------------------------------------------------*/

  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


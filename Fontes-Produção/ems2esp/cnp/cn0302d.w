&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa         for ems2cadme.empresa.

{include/i-prgvrs.i CN0302D 2.00.00.025 } /*** 010025 ***/


&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i cn0302d MCN}
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
DEF INPUT PARAMETER p-nr-contrato     LIKE matriz-rat-med.nr-contrat      NO-UNDO.
DEF INPUT PARAMETER p-num-seq-item    LIKE matriz-rat-med.num-seq-item    NO-UNDO.
DEF INPUT PARAMETER p-numero-ordem    LIKE matriz-rat-med.numero-ordem    NO-UNDO.
DEF INPUT PARAMETER p-num-seq-event   LIKE matriz-rat-med.num-seq-event   NO-UNDO.
DEF INPUT PARAMETER p-num-seq-medicao LIKE matriz-rat-med.num-seq-medicao NO-UNDO.
DEF INPUT PARAMETER wh-browse         AS   HANDLE                         NO-UNDO.
/* Local Variable Definitions ---                                       */

DEF VAR c-formato-conta  AS CHAR NO-UNDO INIT "x(20)".
DEF VAR c-formato-ccusto AS CHAR NO-UNDO INIT "x(20)".


DEF TEMP-TABLE tt-matriz NO-UNDO LIKE matriz-rat-med.

def temp-table tt_cta_integr no-undo
    field ttv_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field ttv_cod_cta_ctbl                 as character format "x(20)" label "Conta Contˇbil" column-label "Conta Contˇbil"
    field ttv_des_titulo                   as character format "x(40)"
    field ttv_num_tip_cta_ctbl             as integer format ">9" label "Tipo Conta" column-label "Tipo Conta"
    field ttv_num_sit_cta_ctbl             as integer format ">9" label "Situaªío Conta" column-label "Situaªío Cta"
    field ttv_ind_finalid_ctbl_cta         as character format "X(40)" label "Finalidade Contˇbil"
    index tt_id                           
          ttv_cod_plano_cta_ctbl           ascending
          ttv_cod_cta_ctbl                 ascending
    .

def temp-table tt_cta_integr_aux no-undo
    field ttv_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field ttv_cod_cta_ctbl                 as character format "x(20)" label "Conta Contˇbil" column-label "Conta Contˇbil"
    field ttv_des_titulo                   as character format "x(40)"
    field ttv_num_tip_cta_ctbl             as integer format ">9" label "Tipo Conta" column-label "Tipo Conta"
    field ttv_num_sit_cta_ctbl             as integer format ">9" label "Situaªío Conta" column-label "Situaªío Cta"
    field ttv_ind_finalid_ctbl_cta         as character format "X(40)" label "Finalidade Contˇbil"
    index tt_id                           
          ttv_cod_plano_cta_ctbl           ascending
          ttv_cod_cta_ctbl                 ascending
    .

def temp-table tt_cta_integr_aux1 no-undo
    field ttv_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field ttv_cod_cta_ctbl                 as character format "x(20)" label "Conta Contˇbil" column-label "Conta Contˇbil"
    field ttv_des_titulo                   as character format "x(40)"
    field ttv_num_tip_cta_ctbl             as integer format ">9" label "Tipo Conta" column-label "Tipo Conta"
    field ttv_num_sit_cta_ctbl             as integer format ">9" label "Situaªío Conta" column-label "Situaªío Cta"
    field ttv_ind_finalid_ctbl_cta         as character format "X(40)" label "Finalidade Contˇbil"
    index tt_id                           
          ttv_cod_plano_cta_ctbl           ascending
          ttv_cod_cta_ctbl                 ascending
    .

def temp-table tt_ccusto_cta_integr no-undo
    field ttv_cod_empresa                  as character format "x(3)" label "Empresa" column-label "Empresa"
    field ttv_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field ttv_cod_cta_ctbl                 as character format "x(20)" label "Conta Contˇbil" column-label "Conta Contˇbil"
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
    field ttv_num_cod_erro                 as integer format ">>>>,>>9" label "Número" column-label "Número"
    field ttv_des_msg_ajuda                as character format "x(40)" label "Mensagem Ajuda" column-label "Mensagem Ajuda"
    field ttv_des_msg_erro                 as character format "x(60)" label "Mensagem Erro" column-label "Inconsistºncia"
    .

DEF TEMP-TABLE tt-conta  NO-UNDO
    FIELD ct-codigo  AS CHAR
    FIELD ct-desc    AS CHAR
    FIELD tipo       AS INT
    FIELD situacao   AS INT
    FIELD finalid    AS INT
    FIELD finalidade AS CHAR
    FIELD c-modulo   AS CHAR
    index tt_id
          ct-codigo  ascending
          c-modulo   ascending
    .

DEF TEMP-TABLE tt-conta-cc-custo NO-UNDO
    FIELD ct-codigo AS CHAR
    FIELD ct-desc   AS CHAR
    FIELD sc-codigo AS CHAR
    FIELD sc-desc   AS CHAR
    index tt_id
          ct-codigo  ascending
          sc-codigo  ascending
    .

def var h_api_ccusto       as handle no-undo.
def var h_api_cta_ctbl     as handle no-undo.
DEF VAR i-lista-finalid    AS INT    NO-UNDO.
DEF VAR c-ct-codigo-browse AS CHAR   NO-UNDO.
DEF VAR c-sc-codigo-browse AS CHAR   NO-UNDO.
DEF VAR c-sc-codigo-matriz AS CHAR   NO-UNDO.
DEF VAR c-ct-codigo-matriz AS CHAR   NO-UNDO.

DEF VAR de-percent        LIKE matriz-rat-med.perc-rateio   NO-UNDO.
DEF VAR de-elimina        LIKE matriz-rat-med.perc-rateio   NO-UNDO.
DEF VAR i-seq-comp        AS   INTEGER init 0               NO-UNDO.
DEF VAR l-grava           AS   LOGICAL                      NO-UNDO.
DEF VAR l-inclui          AS   LOGICAL                      NO-UNDO.
DEF VAR l-eliminado       AS   LOGICAL                      NO-UNDO.
DEF VAR hshowmsg          AS   HANDLE                       NO-UNDO.
DEF VAR h_inapi404        AS   HANDLE                       NO-UNDO.
def var l-altera-inv      as   logical                      no-undo.
/*
&IF "{&mguni_version}" >= "2.071" &THEN
    DEF VAR iempresa          LIKE empresa.ep-codigo            NO-UNDO.
&ELSE
*/
    DEF VAR iempresa          AS   INTEGER                      NO-UNDO.
/*&ENDIF*/

{cdp/cdcfgmat.i} /* pre-processadores                  */
{cdp/cd9017.i}   /* definicoes das variaveis do filtro */
{cdp/cd9731.i1}  /* l-integra-cn-in-medicao            */
{cdp/cd9731.i2}  /* l-matriz-medicao - param contratos */
{cdp/cd0666.i}   /* tt-erro - temp table de erro       */
{cdp/cd9731.i8}  /* l-spp-integra-cn-mi-med */
{inp/inapi402.i}

{method/dbotterr.i}

DEF VAR c-conta-ini AS CHAR    FORMAT "x(17)" INIT ""                  NO-UNDO.
DEF VAR c-conta-fim AS CHAR    FORMAT "x(17)" INIT "ZZZZZZZZZZZZZZZZZ" NO-UNDO.
DEF VAR l-cancela   AS LOGICAL                INIT YES                 NO-UNDO.
DEF VAR de-perc-ant AS DECIMAL FORMAT "->>9.99"                        NO-UNDO.

DEF VAR i-empresa   LIKE param-global.empresa-prin                     NO-UNDO.
DEF VAR h-acomp       AS HANDLE.
DEF VAR c-item        AS CHAR                                          NO-UNDO.
DEF VAR hProgramZoom  AS HANDLE                                        NO-UNDO.
DEF VAR h-cdapi024    AS HANDLE                                        NO-UNDO.
DEF VAR h-boiv029     AS HANDLE                                        NO-UNDO.

def temp-table tt-erro-aux1 no-undo
    field i-sequen as int             
    field cd-erro  as int
    field mensagem as char format "x(255)"
    field c-param  as char.

{cdp/cdcfgdis.i}

/* Integracao Contratos - declaracao temp-tables */
{cnp/cnapi020.i3}

&if '{&bf_mat_versao_ems}' >= '2.062' &THEN
    {cdp/cd9590.i}    /*Include verificaá∆o Unidade Neg¢cio*/
    DEFINE VARIABLE l-unid-neg AS LOGICAL INITIAL NO NO-UNDO.

    def temp-table tt-erro-aux no-undo
        field i-sequen as int
        field cd-erro  as int
        field mensagem as char format "x(255)".
    
    DEF TEMP-TABLE tt-matriz-inv NO-UNDO 
        FIELD cod-unid-neg AS CHAR
        FIELD perc-rateio  AS DEC.

    DEF BUFFER btt-matriz FOR tt-matriz.
&endif

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
&Scoped-define INTERNAL-TABLES tt-conta-cc-custo tt-matriz tt-conta ~
medicao-contrat estabelec item

/* Definitions for BROWSE br-conta-cc-custo                             */
&Scoped-define FIELDS-IN-QUERY-br-conta-cc-custo STRING(tt-conta-cc-custo.sc-codigo, c-formato-ccusto) tt-conta-cc-custo.sc-desc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-conta-cc-custo   
&Scoped-define SELF-NAME br-conta-cc-custo
&Scoped-define QUERY-STRING-br-conta-cc-custo FOR EACH tt-conta-cc-custo
&Scoped-define OPEN-QUERY-br-conta-cc-custo OPEN QUERY {&SELF-NAME} FOR EACH tt-conta-cc-custo.
&Scoped-define TABLES-IN-QUERY-br-conta-cc-custo tt-conta-cc-custo
&Scoped-define FIRST-TABLE-IN-QUERY-br-conta-cc-custo tt-conta-cc-custo


/* Definitions for BROWSE br-target-browse                              */
&Scoped-define FIELDS-IN-QUERY-br-target-browse STRING(tt-matriz.ct-codigo, c-formato-conta) STRING(tt-matriz.sc-codigo, c-formato-ccusto) &if '{&bf_mat_versao_ems}' >= '2.062' &THEN tt-matriz.cod-unid-negoc &endif tt-matriz.int-1 tt-matriz.perc-rateio   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-target-browse tt-matriz.perc-rateio ~
&if '{&bf_mat_versao_ems}' >= '2.062' &THEN ~
 tt-matriz.cod-unid-negoc ~
&endif ~
tt-matriz.int-1   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-target-browse tt-matriz '2
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-target-browse tt-matriz
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-br-target-browse '2
&Scoped-define SELF-NAME br-target-browse
&Scoped-define QUERY-STRING-br-target-browse FOR EACH tt-matriz where tt-matriz.nr-contrato  = p-nr-contrato and                                                  tt-matriz.num-seq-item = p-num-seq-item
&Scoped-define OPEN-QUERY-br-target-browse OPEN QUERY {&SELF-NAME} FOR EACH tt-matriz where tt-matriz.nr-contrato  = p-nr-contrato and                                                  tt-matriz.num-seq-item = p-num-seq-item.
&Scoped-define TABLES-IN-QUERY-br-target-browse tt-matriz
&Scoped-define FIRST-TABLE-IN-QUERY-br-target-browse tt-matriz


/* Definitions for BROWSE br-tt-conta                                   */
&Scoped-define FIELDS-IN-QUERY-br-tt-conta STRING(tt-conta.ct-codigo, c-formato-conta) tt-conta.ct-desc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-tt-conta   
&Scoped-define SELF-NAME br-tt-conta
&Scoped-define QUERY-STRING-br-tt-conta FOR EACH tt-conta
&Scoped-define OPEN-QUERY-br-tt-conta OPEN QUERY {&SELF-NAME} FOR EACH tt-conta.
&Scoped-define TABLES-IN-QUERY-br-tt-conta tt-conta
&Scoped-define FIRST-TABLE-IN-QUERY-br-tt-conta tt-conta


/* Definitions for FRAME f-formation                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-formation ~
    ~{&OPEN-QUERY-br-conta-cc-custo}~
    ~{&OPEN-QUERY-br-target-browse}~
    ~{&OPEN-QUERY-br-tt-conta}
&Scoped-define QUERY-STRING-f-formation FOR EACH movind.medicao-contrat NO-LOCK, ~
      EACH movind.estabelec OF movind.medicao-contrat NO-LOCK, ~
      EACH movind.item OF movind.estabelec NO-LOCK
&Scoped-define OPEN-QUERY-f-formation OPEN QUERY f-formation FOR EACH movind.medicao-contrat NO-LOCK, ~
      EACH movind.estabelec OF movind.medicao-contrat NO-LOCK, ~
      EACH movind.item OF movind.estabelec NO-LOCK.
&Scoped-define TABLES-IN-QUERY-f-formation medicao-contrat estabelec item
&Scoped-define FIRST-TABLE-IN-QUERY-f-formation medicao-contrat
&Scoped-define SECOND-TABLE-IN-QUERY-f-formation estabelec
&Scoped-define THIRD-TABLE-IN-QUERY-f-formation item


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 rt-key-parent rt-source-browse ~
rt-source-browse-2 rt-conta-cc-custo fi-i-num-ord-inv br-conta-cc-custo ~
br-tt-conta bt-Filtro bt-Faixa bt-add bt-del br-target-browse bt-invest ~
bt-ok bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-nr-contrato c-desc-contrat ~
fi-cod-estabel Fi-nome fi-numero-ordem fi-num-seq-item fi-it-codigo ~
fi-desc-item fi-num-seq-medicao fi-i-num-ord-inv fi-ord-manut de-perc-total 

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

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "image\im-up":U
     IMAGE-INSENSITIVE FILE "image\ii-up":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-Faixa 
     IMAGE-UP FILE "image\im-ran":U
     IMAGE-INSENSITIVE FILE "image\ii-ran":U
     LABEL "Fai&xa" 
     SIZE 4 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-Filtro 
     IMAGE-UP FILE "image\im-fil":U
     IMAGE-INSENSITIVE FILE "image\ii-fil":U
     LABEL "&Filtro" 
     SIZE 4 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-invest 
     LABEL "&Invest" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&Ok" 
     SIZE 10 BY 1.

DEFINE VARIABLE c-desc-contrat AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .88 NO-UNDO.

DEFINE VARIABLE de-perc-total AS DECIMAL FORMAT "->>9.99":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel LIKE estabelec.cod-estabel
     LABEL "Estabelecimento":R7 
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "x(60)" 
     VIEW-AS FILL-IN 
     SIZE 40.14 BY .88.

DEFINE VARIABLE fi-i-num-ord-inv AS INTEGER FORMAT ">>>>>,>>9":U INITIAL 0 
     LABEL "Nr Ordem Inv" 
     VIEW-AS FILL-IN 
     SIZE 12.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .88.

DEFINE VARIABLE Fi-nome AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 41 BY .88.

DEFINE VARIABLE fi-nr-contrato AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Nr Contrato":R14 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-num-seq-event AS INTEGER FORMAT ">,>>9" INITIAL 0 
     LABEL "Seq Evento":R11 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-num-seq-item AS INTEGER FORMAT ">,>>9" INITIAL 0 
     LABEL "Seq Item":R17 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-num-seq-medicao AS INTEGER FORMAT ">,>>9" INITIAL 0 
     LABEL "Seq Mediá∆o":R14 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-numero-ordem AS INTEGER FORMAT "zzzzz9,99" INITIAL 0 
     LABEL "Ordem" 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ord-manut AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Ord Manut" 
     VIEW-AS FILL-IN 
     SIZE 13.14 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 92 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE rt-conta-cc-custo
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91.43 BY 6.

DEFINE RECTANGLE rt-key-parent
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91.72 BY 4.29.

DEFINE RECTANGLE rt-source-browse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44.43 BY 6.5.

DEFINE RECTANGLE rt-source-browse-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.43 BY 6.5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-conta-cc-custo FOR 
      tt-conta-cc-custo SCROLLING.

DEFINE QUERY br-target-browse FOR 
      tt-matriz SCROLLING.

DEFINE QUERY br-tt-conta FOR 
      tt-conta SCROLLING.

DEFINE QUERY f-formation FOR 
      medicao-contrat, 
      estabelec, 
      item SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-conta-cc-custo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-conta-cc-custo w-window _FREEFORM
  QUERY br-conta-cc-custo DISPLAY
      STRING(tt-conta-cc-custo.sc-codigo, c-formato-ccusto) COLUMN-LABEL "Centro Custo"  FORMAT "x(20)" WIDTH 21
tt-conta-cc-custo.sc-desc   COLUMN-LABEL "Titulo" FORMAT "x(40)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 44.57 BY 6.04.

DEFINE BROWSE br-target-browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-target-browse w-window _FREEFORM
  QUERY br-target-browse DISPLAY
      STRING(tt-matriz.ct-codigo, c-formato-conta)  WIDTH 21 COLUMN-LABEL "Conta" FORMAT "x(20)"
      STRING(tt-matriz.sc-codigo, c-formato-ccusto) WIDTH 21 COLUMN-LABEL "Centro Custo" FORMAT "x(20)"
      &if '{&bf_mat_versao_ems}' >= '2.062' &THEN
       tt-matriz.cod-unid-negoc COLUMN-LABEL "UNeg" WIDTH 04.0
      &endif
      tt-matriz.int-1          COLUMN-LABEL "Ordem Man" FORMAT ">>>,>>>,>>9" WIDTH 11
      tt-matriz.perc-rateio    COLUMN-LABEL "Rateio %" WIDTH 7.0
enable tt-matriz.perc-rateio
       &if '{&bf_mat_versao_ems}' >= '2.062' &THEN
        tt-matriz.cod-unid-negoc
       &endif
       tt-matriz.int-1
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 89.72 BY 5.5.

DEFINE BROWSE br-tt-conta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-tt-conta w-window _FREEFORM
  QUERY br-tt-conta DISPLAY
      STRING(tt-conta.ct-codigo, c-formato-conta) COLUMN-LABEL "Conta" FORMAT "x(20)" WIDTH 21
tt-conta.ct-desc   COLUMN-LABEL "T°tulo" FORMAT "x(30)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 38 BY 6.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-formation
     fi-nr-contrato AT ROW 1.25 COL 14.57 COLON-ALIGNED HELP
          "Permite informar o n£mero do contrato"
     c-desc-contrat AT ROW 1.25 COL 26.43 COLON-ALIGNED NO-LABEL
     fi-cod-estabel AT ROW 2.25 COL 14.57 COLON-ALIGNED HELP
          ""
          LABEL "Estabelecimento":R7
     Fi-nome AT ROW 2.25 COL 22.14 COLON-ALIGNED HELP
          "Nome Estabelecimento" NO-LABEL
     fi-numero-ordem AT ROW 2.25 COL 78.86 COLON-ALIGNED HELP
          "N£mero da ordem de compra"
     fi-num-seq-item AT ROW 3.25 COL 14.57 COLON-ALIGNED HELP
          "SeqÅància do item no contrato."
     fi-it-codigo AT ROW 3.25 COL 21.86 COLON-ALIGNED NO-LABEL
     fi-desc-item AT ROW 3.25 COL 39.43 COLON-ALIGNED NO-LABEL
     fi-num-seq-medicao AT ROW 4.25 COL 14.57 COLON-ALIGNED HELP
          "Sequencia da mediá∆o"
     fi-i-num-ord-inv AT ROW 4.25 COL 35.14 COLON-ALIGNED
     fi-num-seq-event AT ROW 4.25 COL 59.29 COLON-ALIGNED HELP
          "Sequància do evento."
     fi-ord-manut AT ROW 4.25 COL 77.29 COLON-ALIGNED
     br-conta-cc-custo AT ROW 5.71 COL 47.43 WIDGET-ID 100
     br-tt-conta AT ROW 5.75 COL 7 WIDGET-ID 200
     bt-Filtro AT ROW 7.25 COL 2.29
     bt-Faixa AT ROW 8.5 COL 2.29
     bt-add AT ROW 12.04 COL 42 WIDGET-ID 2
     bt-del AT ROW 12.04 COL 46.57 WIDGET-ID 4
     br-target-browse AT ROW 13.33 COL 2.29
     bt-invest AT ROW 14.83 COL 51.86
     bt-ok AT ROW 19.38 COL 2
     bt-cancelar AT ROW 19.38 COL 13
     de-perc-total AT ROW 19.38 COL 68 COLON-ALIGNED
     bt-ajuda AT ROW 19.38 COL 81.86
     RECT-1 AT ROW 19.17 COL 1
     rt-key-parent AT ROW 1.08 COL 1.43
     rt-source-browse AT ROW 5.5 COL 1.57
     rt-source-browse-2 AT ROW 5.5 COL 46.57
     rt-conta-cc-custo AT ROW 13.08 COL 1.57 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 92.72 BY 19.71.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-form2
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Other Settings: PERSISTENT-ONLY COMPILE
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
         TITLE              = "Matriz de Mediá∆o"
         HEIGHT             = 19.71
         WIDTH              = 92.72
         MAX-HEIGHT         = 21.13
         MAX-WIDTH          = 92.72
         VIRTUAL-HEIGHT     = 21.13
         VIRTUAL-WIDTH      = 92.72
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
/* BROWSE-TAB br-conta-cc-custo fi-ord-manut f-formation */
/* BROWSE-TAB br-tt-conta br-conta-cc-custo f-formation */
/* BROWSE-TAB br-target-browse bt-del f-formation */
/* SETTINGS FOR FILL-IN c-desc-contrat IN FRAME f-formation
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN de-perc-total IN FRAME f-formation
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-estabel IN FRAME f-formation
   NO-ENABLE LIKE = mgadm.estabelec.cod-estabel EXP-LABEL EXP-SIZE      */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME f-formation
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-it-codigo IN FRAME f-formation
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Fi-nome IN FRAME f-formation
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nr-contrato IN FRAME f-formation
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-num-seq-event IN FRAME f-formation
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi-num-seq-event:HIDDEN IN FRAME f-formation           = TRUE.

/* SETTINGS FOR FILL-IN fi-num-seq-item IN FRAME f-formation
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-num-seq-medicao IN FRAME f-formation
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-numero-ordem IN FRAME f-formation
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-ord-manut IN FRAME f-formation
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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-target-browse
/* Query rebuild information for BROWSE br-target-browse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-matriz where tt-matriz.nr-contrato  = p-nr-contrato and
                                                 tt-matriz.num-seq-item = p-num-seq-item.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-target-browse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-tt-conta
/* Query rebuild information for BROWSE br-tt-conta
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-conta.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-tt-conta */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-formation
/* Query rebuild information for FRAME f-formation
     _TblList          = "movind.medicao-contrat,movind.estabelec OF movind.medicao-contrat,movind.item OF movind.estabelec"
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-formation */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* Matriz de Mediá∆o */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* Matriz de Mediá∆o */
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


&Scoped-define BROWSE-NAME br-target-browse
&Scoped-define SELF-NAME br-target-browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-target-browse w-window
ON ROW-DISPLAY OF br-target-browse IN FRAME f-formation
DO:
    IF c-sc-codigo-matriz = '' THEN
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
  assign de-perc-total = (input frame {&frame-name} de-perc-total - de-perc-ant) +
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


&Scoped-define BROWSE-NAME br-tt-conta
&Scoped-define SELF-NAME br-tt-conta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-tt-conta w-window
ON ROW-DISPLAY OF br-tt-conta IN FRAME f-formation
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-tt-conta w-window
ON VALUE-CHANGED OF br-tt-conta IN FRAME f-formation
DO:
    ASSIGN c-ct-codigo-browse = tt-conta.ct-codigo.

    EMPTY TEMP-TABLE tt_cta_integr_aux1.
    EMPTY TEMP-TABLE tt-conta-cc-custo.

    {&OPEN-QUERY-br-conta-cc-custo}

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
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del w-window
ON CHOOSE OF bt-del IN FRAME f-formation
DO:
   run pi-del.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-Faixa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-Faixa w-window
ON CHOOSE OF bt-Faixa IN FRAME f-formation /* Faixa */
DO:
    run cnp/cn0201c6.w (input-output c-conta-ini,
                        input-output c-conta-fim,
                        output       l-cancela).
    
    if not l-cancela THEN DO:
        RUN pi-cria-tt-conta.
        {&OPEN-QUERY-br-tt-conta}
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-Filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-Filtro w-window
ON CHOOSE OF bt-Filtro IN FRAME f-formation /* Filtro */
DO:
    run cdp/cd9017.w.

    RUN pi-cria-tt-conta.

    /*{&OPEN-QUERY-br-tt-conta}

    {&OPEN-QUERY-br-conta-cc-custo}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-invest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-invest w-window
ON CHOOSE OF bt-invest IN FRAME f-formation /* Invest */
DO:

    /*IF  p-num-seq-event <> 0 THEN DO: /*q seja evento*/
        /*item do contrato Ç controlado por evento*/
        RUN utp/ut-msgs.p (INPUT "show",
                           INPUT 28370,
                           INPUT "").
        RETURN "adm-error":U.
    END.*/

    find item-contrat where
         item-contrat.nr-contrato = p-nr-contrato and
         item-contrat.num-seq-item = p-num-seq-item no-lock no-error.
    if (avail item-contrat and item-contrat.ind-tipo-control <> 1) then do:
         RUN utp/ut-msgs.p (INPUT "show",
                           INPUT 17006,
                           INPUT "Matriz de Rateio de Investimento n∆o pode ser informada para contratos por ordem").
        RETURN "adm-error":U.
    end. 
    if p-num-seq-event <> 0 and
       p-num-seq-medicao = 0 then do:
       RUN utp/ut-msgs.p (INPUT "show",
                          INPUT 17006,
                          INPUT "Matriz de Rateio de Investimento deve ser informada na mediá∆o do evento").
        RETURN "adm-error":U.
    end.


    IF  p-num-seq-medicao <> 0 THEN DO:
        w-window:SENSITIVE = NO.

        RUN pi-inclui-medicao IN h_inapi404 (INPUT-OUTPUT TABLE tt-matriz,
                                             INPUT ROWID(medicao-contrat)).

        w-window:SENSITIVE = YES.
    END.
    APPLY 'entry' TO w-window.

    OPEN QUERY br-target-browse FOR EACH tt-matriz where tt-matriz.nr-contrato  = p-nr-contrato and                                                  tt-matriz.num-seq-item = p-num-seq-item.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME f-formation /* Ok */
DO:

  RUN pi-commit.

  IF  RETURN-VALUE <> "ADM-ERROR":U THEN
      APPLY "close" TO THIS-PROCEDURE.
  ELSE
      APPLY "TAB" TO bt-add.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-i-num-ord-inv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-i-num-ord-inv w-window
ON F5 OF fi-i-num-ord-inv IN FRAME f-formation /* Nr Ordem Inv */
DO:
    {include/zoomvar.i &prog-zoom="ivzoom/z05iv043.w"
                       &campo=fi-i-num-ord-inv
                       &campozoom=num-ord-magnus
                       &frame="f-formation"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-i-num-ord-inv w-window
ON MOUSE-SELECT-DBLCLICK OF fi-i-num-ord-inv IN FRAME f-formation /* Nr Ordem Inv */
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

{utp/ut-liter.i Total * R}
ASSIGN de-perc-total:LABEL IN FRAME {&FRAME-NAME} = RETURN-VALUE.

fi-i-num-ord-inv:LOAD-MOUSE-POINTER("image/lupa.cur":U) IN FRAME {&FRAME-NAME}.

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
       IF  NOT CAN-FIND(FIRST unid-negoc NO-LOCK
                        WHERE unid-negoc.cod-unid-negoc = 
                              INPUT BROWSE br-target-browse tt-matriz.cod-unid-negoc) THEN DO:
           {utp/ut-field.i mgind unid-negoc cod-unid-negoc 1}
           RUN utp/ut-msgs.p (INPUT "show",
                              INPUT 2,
                              INPUT RETURN-VALUE).
           RETURN NO-APPLY.
       END.
       
       if  valid-handle(h-cdapi024) then do:
           empty temp-table tt-erro-aux.
           
           /*UN X Estabelecimento*/
           run ValidaUnidadeNegocioEstabel in h-cdapi024 (input IF AVAIL ordem-compra THEN  ordem-compra.cod-estabel ELSE contrato-for.cod-estabel,
                                                          input TODAY,
                                                          input INPUT BROWSE br-target-browse tt-matriz.cod-unid-negoc, 
                                                          output table tt-erro-aux).
           
           /*UN X Usu†rio*/
           run ValidaUnidadeNegocioUsuario in h-cdapi024 (input contrato-for.cod-comprado,
                                                          input INPUT BROWSE br-target-browse tt-matriz.cod-unid-negoc,
                                                          output table tt-erro-aux append).

           /*Validacao Restricao Unidade Negocio*/
           if not avail param-global then
               find first param-global no-lock no-error.

           &if "{&bf_mat_versao_ems}" <= "2.072" &then
               assign l-unid-neg = if substring(param-global.char-2,31,1) = "2" then yes else no.
           &else
               assign l-unid-neg = param-global.log-validac-ems5.
           &endif
           
           if  l-unid-neg and tt-matriz.ct-codigo <> "" then do:
               run ValidaRestricoesUnidadeNegocio in h-cdapi024 (input  IF AVAIL ordem-compra THEN  ordem-compra.cod-estabel ELSE contrato-for.cod-estabel,
                                                                 input  tt-matriz.ct-codigo,
                                                                 input  tt-matriz.sc-codigo,
                                                                 input  input browse br-target-browse tt-matriz.cod-unid-negoc,
                                                                 input  today,
                                                                 input  no,
                                                                 output table tt-erro-aux append).
           end.
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
           {utp/ut-liter.i Conta_Cont†bil_e_Unidade_de_Neg¢cio * R}
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
  DISPLAY fi-nr-contrato c-desc-contrat fi-cod-estabel Fi-nome fi-numero-ordem 
          fi-num-seq-item fi-it-codigo fi-desc-item fi-num-seq-medicao 
          fi-i-num-ord-inv fi-ord-manut de-perc-total 
      WITH FRAME f-formation IN WINDOW w-window.
  ENABLE RECT-1 rt-key-parent rt-source-browse rt-source-browse-2 
         rt-conta-cc-custo fi-i-num-ord-inv br-conta-cc-custo br-tt-conta 
         bt-Filtro bt-Faixa bt-add bt-del br-target-browse bt-invest bt-ok 
         bt-cancelar bt-ajuda 
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  &if '{&bf_mat_versao_ems}' >= '2.062' &THEN
       ASSIGN tt-matriz.cod-unid-negoc:VISIBLE IN BROWSE br-target-browse = l-unidade-negocio AND l-mat-unid-negoc.
  &endif
  
  
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

   IF  l-integra-cn-in-medicao AND
       VALID-HANDLE(h_inapi404) THEN
       DELETE PROCEDURE h_inapi404.

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

{utp/ut9000.i "CN0302D" "2.00.00.025"}

  /* Dispatch standard ADM method.                             */
  RUN pi-show-master-record.

  run prgint/utb/utb742za.py persistent set h_api_ccusto.
  run prgint/utb/utb743za.py persistent set h_api_cta_ctbl.

  &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
     IF  l-unidade-negocio AND l-mat-unid-negoc THEN DO:
         IF NOT VALID-HANDLE(h-cdapi024) THEN 
             RUN cdp/cdapi024.p persistent set h-cdapi024.
     END.
            
     IF NOT VALID-HANDLE (h-boiv029) THEN DO:
         RUN ivbo/boiv029.p PERSISTENT SET h-boiv029.
         RUN openQueryStatic IN h-boiv029 (INPUT "Main":U).
     END.
  &ENDIF
  
  /* Localiza Tabelas */
  FIND FIRST param-global                                                             NO-LOCK NO-ERROR.
  FIND FIRST contrato-for  WHERE contrato-for.nr-contrato  = p-nr-contrato            NO-LOCK NO-ERROR.
  FIND FIRST ordem-compra  WHERE ordem-compra.numero-ordem = p-numero-ordem           NO-LOCK NO-ERROR.
  FIND FIRST estabelec     WHERE estabelec.cod-estabel     = ordem-compra.cod-estabel NO-LOCK NO-ERROR.
  FIND       item          WHERE item.it-codigo            = ordem-compra.it-codigo   NO-LOCK NO-ERROR.
  FIND FIRST medicao-contrat
       WHERE medicao-contrat.nr-contrato     = p-nr-contrato
         AND medicao-contrat.num-seq-item    = p-num-seq-item
         AND medicao-contrat.numero-ordem    = p-numero-ordem
         AND medicao-contrat.num-seq-event   = p-num-seq-event
         AND medicao-contrat.num-seq-medicao = p-num-seq-medicao NO-LOCK NO-ERROR.

  FIND FIRST pedido-compr
       WHERE pedido-compr.nr-contrato = contrato-for.nr-contrato NO-LOCK NO-ERROR.
  IF  AVAIL pedido-compr THEN DO:
      FIND FIRST estabelec
           WHERE estabelec.cod-estabel = pedido-compr.cod-estabel NO-LOCK NO-ERROR.
     IF  AVAIL estabelec THEN
         assign i-empresa = estabelec.ep-codigo.
  END.

  IF i-empresa = '0' THEN
      FIND FIRST estabelec
           WHERE estabelec.cod-estabel = contrato-for.cod-estabel NO-LOCK NO-ERROR.
     IF  AVAIL estabelec THEN
         assign i-empresa = estabelec.ep-codigo.

  /*Formato de Conta e Centro de Custo - utilizado para apresentar os dados formatados em tela*/
  RUN pi-busca-formatos IN THIS-PROCEDURE.

  IF  l-integra-cn-in-medicao THEN DO:

      IF  p-num-seq-medicao <> 0 /*AND      /* q seja medicao */
          p-num-seq-event    = 0 */ THEN DO: /* n seja evento  */ 
          RUN inp/inapi404.p PERSISTENT SET h_inapi404.
          RUN pi-cria-tt-mat-rat-med-inv IN h_inapi404 (INPUT p-nr-contrato,
                                                        INPUT p-num-seq-item,
                                                        INPUT p-num-seq-event,
                                                        INPUT p-num-seq-medicao).
      END.
  END.

  FOR EACH  matriz-rat-med
      WHERE matriz-rat-med.nr-contrato     = p-nr-contrato
        AND matriz-rat-med.num-seq-item    = p-num-seq-item
        and matriz-rat-med.numero-ordem    = p-numero-ordem
        AND matriz-rat-med.num-seq-event   = p-num-seq-event
        AND matriz-rat-med.num-seq-medicao = p-num-seq-medicao NO-LOCK:

      CREATE tt-matriz.
      ASSIGN tt-matriz.nr-contrato     = matriz-rat-med.nr-contrato
             tt-matriz.num-seq-item    = matriz-rat-med.num-seq-item
             tt-matriz.numero-ordem    = matriz-rat-med.numero-ordem
             tt-matriz.num-seq-event   = matriz-rat-med.num-seq-event
             tt-matriz.num-seq-medicao = matriz-rat-med.num-seq-medicao
             tt-matriz.ct-codigo       = matriz-rat-med.ct-codigo
             tt-matriz.sc-codigo       = matriz-rat-med.sc-codigo
             tt-matriz.perc-rateio     = matriz-rat-med.perc-rateio
             de-perc-total             = tt-matriz.perc-rateio + de-perc-total
             tt-matriz.int-1           = &IF "{&mguni_version}" >= "2.08" &THEN
                                         matriz-rat-med.nr-ord-produ
                                         &ELSE
                                         matriz-rat-med.int-1
                                         &ENDIF .
      &if '{&bf_mat_versao_ems}' >= '2.062' &THEN
          ASSIGN tt-matriz.cod-unid-negoc = matriz-rat-med.cod-unid-negoc.
      &endif
  END.

  IF  VALID-HANDLE(THIS-PROCEDURE) THEN
      RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  /* Code placed here will execute AFTER standard behavior.    */

  IF  AVAIL contrato-for THEN
      DISP contrato-for.des-contrat @ c-desc-contrat WITH FRAME {&FRAME-NAME}.

  IF  AVAIL estabelec THEN
      DISP estabelec.nome @ fi-nome WITH FRAME {&FRAME-NAME}.

  IF  AVAIL ITEM THEN DO:

      DISP ITEM.desc-item @ fi-desc-item
           ITEM.it-codigo @ fi-it-codigo WITH FRAME {&frame-name}.

      IF  ITEM.it-codigo  = "" AND
          ITEM.tipo-contr = 4 THEN DO: /*debito direto*/

          FIND item-contrat WHERE
               item-contrat.nr-contrato  = p-nr-contrato  AND
               item-contrat.num-seq-item = p-num-seq-item NO-LOCK NO-ERROR.
          IF  AVAIL item-contrat THEN
              DISP item-contrat.narrat-item @ fi-desc-item WITH FRAME {&FRAME-NAME}.
      END.
  END.

  ASSIGN bt-invest:VISIBLE = IF  l-integra-cn-in-medicao THEN YES ELSE NO.

  DISP ordem-compra.cod-estabel @ fi-cod-estabel
       p-nr-contrato            @ fi-nr-contrato
       p-num-seq-item           @ fi-num-seq-item
       p-num-seq-event          @ fi-num-seq-event
       p-num-seq-medicao        @ fi-num-seq-medicao
       p-numero-ordem           @ fi-numero-ordem WITH FRAME {&FRAME-NAME}.


  {utp/ut-liter.i Ord_Manut *}
  ASSIGN fi-ord-manut:LABEL IN FRAME {&FRAME-NAME} = RETURN-VALUE.
  &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
       IF param-global.modulo-mi = yes then
          ASSIGN fi-ord-manut:VISIBLE IN FRAME {&FRAME-NAME} = YES.
       ELSE
          ASSIGN fi-ord-manut:VISIBLE IN FRAME {&FRAME-NAME} = NO.
  &ELSE
       ASSIGN fi-ord-manut:VISIBLE IN FRAME {&FRAME-NAME} = NO.
  &ENDIF
  
  /*APPLY "entry" TO br-source-browse IN FRAME {&FRAME-NAME}.*/

  IF NOT AVAIL item-contrat THEN
      FOR  FIRST item-contrat WHERE
                 item-contrat.nr-contrato  = p-nr-contrato  AND
                 item-contrat.num-seq-item = p-num-seq-item NO-LOCK: END.

  IF NOT l-spp-integra-cn-mi-med OR 
     ( AVAIL item-contrat AND item-contrat.ind-tipo-control <> 1 ) THEN
     ASSIGN tt-matriz.int-1:VISIBLE        IN BROWSE br-target-browse = NO
            /*
            tt-matriz.ct-codigo:WIDTH IN BROWSE br-target-browse = 20
            tt-matriz.sc-codigo:WIDTH IN BROWSE br-target-browse = 20
            */
            tt-matriz.perc-rateio:WIDTH    IN BROWSE br-target-browse = 16.

  &IF '{&bf_mat_versao_ems}' >= '2.062' &THEN          
     IF  item-contrat.ind-tipo-control = 2 THEN DO:
         ASSIGN fi-num-seq-medicao:VISIBLE IN FRAME {&FRAME-NAME} = NO.
         ASSIGN fi-num-seq-event:VISIBLE IN FRAME {&FRAME-NAME} = item-contrat.log-control-event.
     END.

     ASSIGN fi-numero-ordem:LABEL IN FRAME {&FRAME-NAME} = "Ordem compra"
            bt-invest:HIDDEN IN FRAME {&FRAME-NAME}      = YES.
    
     IF param-global.modulo-in THEN
         ASSIGN fi-i-num-ord-inv:HIDDEN IN FRAME {&FRAME-NAME} = NO.
     ELSE
         ASSIGN fi-i-num-ord-inv:HIDDEN IN FRAME {&FRAME-NAME} = YES.

     IF p-num-seq-medicao = 0 AND
        p-num-seq-event  <> 0 THEN DO:
                       
        FIND FIRST evento-ped EXCLUSIVE-LOCK 
             WHERE evento-ped.numero-ordem = p-numero-ordem
               AND evento-ped.seq-evento   = p-num-seq-event NO-ERROR.
        IF AVAIL evento-ped THEN DO:
            &IF '{&bf_mat_versao_ems}' >= '2.07' &THEN
                ASSIGN fi-i-num-ord-inv:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(evento-ped.num-ord-inv). 
            &ELSE
                ASSIGN fi-i-num-ord-inv:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTRING(evento-ped.char-1,1,9).
            &ENDIF
            
            &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
              &IF "{&bf_mat_versao_ems}" >= "2.08" &THEN
                   ASSIGN fi-ord-manut = evento-ped.nr-ord-produ.
              &ELSE
                   ASSIGN fi-ord-manut = evento-ped.int-1.
              &ENDIF
              IF param-global.modulo-mi THEN
                 DISP fi-ord-manut WITH FRAME {&FRAME-NAME}.
            &ENDIF
        END.      
     END.
     ELSE DO:
        &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
          &IF "{&bf_mat_versao_ems}" >= "2.08" &THEN
               ASSIGN fi-ord-manut = IF AVAIL medicao-contrat THEN medicao-contrat.nr-ord-produ ELSE 0.
          &ELSE
               ASSIGN fi-ord-manut = IF AVAIL medicao-contrat THEN medicao-contrat.int-1        ELSE 0.
          &ENDIF
          IF param-global.modulo-mi THEN
             DISP fi-ord-manut WITH FRAME {&FRAME-NAME}.
        &ENDIF
          
        IF  p-num-seq-medicao <> 0 
            AND  AVAIL medicao-contrat THEN DO:            
            FIND CURRENT medicao-contrat EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN fi-i-num-ord-inv:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(medicao-contrat.num-ord-inv).
        END.
        ELSE DO:
            IF p-num-seq-medicao = 0 THEN DO:                
                FIND FIRST ordem-compra EXCLUSIVE-LOCK
                     WHERE ordem-compra.numero-ordem = p-numero-ordem NO-ERROR.
                IF AVAIL ordem-compra THEN 
                    ASSIGN fi-i-num-ord-inv:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ordem-compra.num-ord-inv).
            END.
        END.
     END.   

     IF INPUT FRAME {&FRAME-NAME} fi-i-num-ord-inv = 0 THEN DO:        
         IF p-num-seq-medicao = 0 THEN DO:
             &IF '{&bf_mat_versao_ems}' >= '2.07' &THEN            
                 IF AVAIL item-contrat AND
                          item-contrat.num-ord-inv > 0 THEN
                     ASSIGN fi-i-num-ord-inv:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(item-contrat.num-ord-inv).
             &ELSE
                 IF AVAIL item-contrat AND
                          INT(SUBSTRING(item-contrat.char-1,1,9)) > 0 THEN
                     ASSIGN fi-i-num-ord-inv:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTRING(item-contrat.char-1,1,9).
             &ENDIF 
         END.
         ELSE DO:
             &IF '{&bf_mat_versao_ems}' >= '2.07' &THEN            
                 IF AVAIL contrato-for AND
                          contrato-for.num-ord-inv > 0 THEN
                     ASSIGN fi-i-num-ord-inv:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(contrato-for.num-ord-inv).
             &ELSE
                 IF AVAIL contrato-for AND
                          INT(SUBSTRING(contrato-for.char-1,1,9)) > 0 THEN
                     ASSIGN fi-i-num-ord-inv:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTRING(contrato-for.char-1,1,9).
             &ENDIF
         END.
     END.

  &ELSE  
     ASSIGN fi-i-num-ord-inv:HIDDEN IN FRAME {&FRAME-NAME}   = YES. 
  &ENDIF

  &IF '{&bf_mat_versao_ems}' >= '2.062' &THEN 
      ASSIGN tt-matriz.int-1:VISIBLE        IN BROWSE br-target-browse = NO.

      IF fi-ord-manut:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "0":U AND  
         fi-ord-manut:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "":U  THEN DO:
         ASSIGN bt-add:SENSITIVE IN FRAME {&FRAME-NAME} = NO 
                bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

         ASSIGN tt-matriz.perc-rateio   :READ-ONLY IN BROWSE br-target-browse = YES
                tt-matriz.cod-unid-negoc:READ-ONLY IN BROWSE br-target-browse = YES.
      END.
  &ENDIF

    RUN pi-cria-tt-conta.

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
                                                     input  INPUT FRAME {&FRAME-NAME} fi-cod-estabel ,                         /* ESTABELECIMENTO EMS2 */
                                                     input  NO,                        /* TODOS OS ESTABELECIMENTOS? */
                                                     input  "",                         /* CODIGO DO PLANO CCUSTO */
                                                     input  "",                         /* UNIDADE DE NEGOCIO */
                                                     input  today,                      /* DATA DE TRANSACAO */
                                                     input  table tt_cta_integr_aux1,   /* CONTAS CONTÊBEIS */
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

    EMPTY TEMP-TABLE tt_cta_integr.
    EMPTY TEMP-TABLE tt_log_erro.

    ASSIGN i-lista-finalid = 0.

    IF l-estoque THEN
        ASSIGN i-tipo-finalid = 10. /* 10 - N∆o gera lanáaamentos */
    ELSE
        ASSIGN i-tipo-finalid = 1. /* 1 - N∆o gera lanáaamentos */

    run pi_busca_ctas_integr in h_api_cta_ctbl (input  STRING(i-empresa),   /* EMPRESA EMS2 */
                                                input  c-modulo,            /* MODULO */
                                                input  "",                  /* PLANO CONTAS */ 
                                                input  "(nenhum)",          /* FINALIDADES */
                                                input  today,               /* DATA DE TRANSACAO */  
                                                output table tt_cta_integr, /* CONTAS */
                                                output table tt_log_erro).  /* ERROS */

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
                                                             input  "Gera lanáamentos", /* FINALIDADES */
                                                             input  today,              /* DATA DE TRANSACAO */  
                                                             output table tt_log_erro). /* ERROS */
            IF RETURN-VALUE = "OK" THEN
                ASSIGN i-lista-finalid = 2. /* 2 - Gera lanáamentos */
            ELSE
                ASSIGN i-lista-finalid = 1. /* 1 - N∆o gera lanáaamentos */
        END.

        IF ( (l-despesa and tt_cta_integr.ttv_num_tip_cta_ctbl = 1) or
             (l-receita and tt_cta_integr.ttv_num_tip_cta_ctbl = 2) or
             (l-venda   and tt_cta_integr.ttv_num_tip_cta_ctbl = 3) or
             (l-passivo and tt_cta_integr.ttv_num_tip_cta_ctbl = 4) or
             (l-ativo   and tt_cta_integr.ttv_num_tip_cta_ctbl = 5) or
             (l-titulo  and tt_cta_integr.ttv_num_tip_cta_ctbl = 6)
           )
           and (i-lista-finalid <> i-tipo-finalid) /* Se for estoque tem q ser <> 10, investimento <> 1, demais valoares est† correto */
           THEN DO:

            CREATE tt-conta.
            ASSIGN tt-conta.ct-codigo  = tt_cta_integr.ttv_cod_cta_ctbl
                   tt-conta.ct-desc    = tt_cta_integr.ttv_des_titulo
                   tt-conta.tipo       = tt_cta_integr.ttv_num_tip_cta_ctbl
                   tt-conta.situacao   = tt_cta_integr.ttv_num_sit_cta_ctbl
                   tt-conta.finalidade = tt_cta_integr.ttv_ind_finalid_ctbl_cta
                   tt-conta.finalid    = i-lista-finalid
                   tt-conta.c-modulo   = c-modulo.
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

DEF VAR i-seq-comp AS INT NO-UNDO.                                                                            
                                                                            
IF  NOT AVAIL param-global THEN
    FIND FIRST param-global NO-LOCK NO-ERROR.

RUN pi-valida-ttmatriz IN THIS-PROCEDURE.

IF  RETURN-VALUE = 'adm-error' THEN
    RETURN 'adm-error'.

assign l-altera-inv = no.

DO TRANSACTION:

    &IF '{&bf_mat_versao_ems}' >= '2.062' &THEN
        IF p-num-seq-medicao = 0 AND
           p-num-seq-event  <> 0 THEN DO:
            FIND FIRST evento-ped EXCLUSIVE-LOCK 
                 WHERE evento-ped.numero-ordem = p-numero-ordem
                   AND evento-ped.seq-evento   = p-num-seq-event NO-ERROR.
            IF AVAIL evento-ped THEN
                &IF '{&bf_mat_versao_ems}' >= '2.07' &THEN
                    ASSIGN evento-ped.num-ord-inv = INPUT FRAME {&FRAME-NAME} fi-i-num-ord-inv.
                &ELSE
                    ASSIGN OVERLAY(evento-ped.char-1,1,9) = STRING(INPUT FRAME {&FRAME-NAME} fi-i-num-ord-inv).
                &ENDIF
        END.
        ELSE DO:
            IF  p-num-seq-medicao <> 0 
            AND AVAIL medicao-contrat THEN DO:            
/*
                RUN pi-retorna-seq-comp IN THIS-PROCEDURE (OUTPUT i-seq-comp).

                FIND CURRENT medicao-contrat EXCLUSIVE-LOCK NO-ERROR.
                ASSIGN &IF '{&bf_mat_versao_ems}' >= '2.07' &THEN 
                            medicao-contrat.seq-comp = i-seq-comp
                       &ELSE
                            OVERLAY(medicao-contrat.char-1,1,3) = STRING(i-seq-comp)
                       &ENDIF
                       .
*/
                if medicao-contrat.num-ord-inv <> input frame {&FRAME-NAME} fi-i-num-ord-inv then do:
                    assign l-altera-inv = yes.

                    if  medicao-contrat.num-ord-inv <> 0 
                    and l-integra-cn-in-medicao then do:
                        RUN inp/inapi403.p (INPUT 2,  /* exclusao */
                                            INPUT ROWID(medicao-contrat),
                                            INPUT-OUTPUT TABLE rowerrors).     
                    end.
                end.
                ASSIGN medicao-contrat.num-ord-inv = INPUT FRAME {&FRAME-NAME} fi-i-num-ord-inv.
            END.
            ELSE DO:
                IF p-num-seq-medicao = 0 THEN DO:            
                    FIND FIRST ordem-compra EXCLUSIVE-LOCK
                         WHERE ordem-compra.numero-ordem = p-numero-ordem NO-ERROR.
                    IF AVAIL ordem-compra THEN 
                        ASSIGN ordem-compra.num-ord-inv = INPUT FRAME {&FRAME-NAME} fi-i-num-ord-inv.
                END.
            END.
        END.
    &ENDIF

    IF  l-grava = YES THEN DO: 

       /* Integracao Contratos - limpa registros temp-table */
       {cnp/cnapi020.i4}

        IF  NOT l-lib-sem-matriz THEN DO:
            IF  AVAIL medicao-contrat AND
                medicao-contrat.ind-sit-medicao = 2 THEN DO: /*Liberada*/
                IF  NOT CAN-FIND(FIRST tt-matriz
                                 WHERE tt-matriz.nr-contrato     = p-nr-contrato
                                   AND tt-matriz.num-seq-item    = p-num-seq-item
                                   AND tt-matriz.numero-ordem    = p-numero-ordem
                                   AND tt-matriz.num-seq-event   = p-num-seq-event
                                   AND tt-matriz.num-seq-medicao = p-num-seq-medicao) THEN DO:

                    /*Deve possuir pelo menos uma matriz de rateio*/
                    RUN utp/ut-msgs.p (INPUT "show",
                                       INPUT 28419,
                                       INPUT "").
                    RETURN "adm-error":U.
                END.
            END.
        END.

        /* elimina toda matriz-rat-med baseado na medicao-contrat */
        FOR EACH matriz-rat-med EXCLUSIVE-LOCK
           WHERE matriz-rat-med.nr-contrato     = p-nr-contrato
             AND matriz-rat-med.num-seq-item    = p-num-seq-item
             AND matriz-rat-med.numero-ordem    = p-numero-ordem
             AND matriz-rat-med.num-seq-event   = p-num-seq-event
             AND matriz-rat-med.num-seq-medicao = p-num-seq-medicao:

            ASSIGN l-inclui = YES.

            /****MULTIPLANTA****/
            IF  NOT l-eliminado THEN DO:

                /*Eliminacao*/
                IF  NOT CAN-FIND(FIRST tt-matriz) THEN DO:
                    RUN pi-trata-multiplanta (INPUT ROWID(matriz-rat-med),
                                              INPUT 3).
                END.
                /*Modificacao*/
                ELSE IF NOT CAN-FIND(FIRST tt-matriz
                                     WHERE tt-matriz.nr-contrato     = matriz-rat-med.nr-contrato
                                       AND tt-matriz.num-seq-item    = matriz-rat-med.num-seq-item
                                       AND tt-matriz.numero-ordem    = matriz-rat-med.numero-ordem
                                       AND tt-matriz.num-seq-event   = matriz-rat-med.num-seq-event
                                       AND tt-matriz.num-seq-medicao = matriz-rat-med.num-seq-medicao
                                       AND tt-matriz.ct-codigo       = matriz-rat-med.ct-codigo
                                       AND tt-matriz.sc-codigo       = matriz-rat-med.sc-codigo
                                       AND tt-matriz.perc-rateio     = matriz-rat-med.perc-rateio
                                       AND (IF l-spp-integra-cn-mi-med AND tt-matriz.int-1:VISIBLE IN BROWSE br-target-browse THEN
                                               tt-matriz.int-1 = &IF "{&mguni_version}" >= "2.08" &THEN
                                                                 matriz-rat-med.nr-ord-produ
                                                                 &ELSE
                                                                 matriz-rat-med.int-1
                                                                 &ENDIF
                                            ELSE YES) ) THEN DO:
                    ASSIGN l-inclui = NO. /*houve modificacao*/

                END.
            END.
            /****MULTIPLANTA****/

            /* Integracao Contratos - criar registro para eliminaá∆o */
            {cnp/cnapi020.i1 2 3 CN0302D " " matriz-rat-med matriz-rat-med}

            DELETE matriz-rat-med.
        END.

        /* recria matriz-rat-med baseado na tt-matriz */
        FOR EACH tt-matriz NO-LOCK:
            CREATE matriz-rat-med.
            ASSIGN matriz-rat-med.nr-contrato     = tt-matriz.nr-contrato
                   matriz-rat-med.num-seq-item    = tt-matriz.num-seq-item
                   matriz-rat-med.numero-ordem    = tt-matriz.numero-ordem   
                   matriz-rat-med.num-seq-event   = tt-matriz.num-seq-event  
                   matriz-rat-med.num-seq-medicao = tt-matriz.num-seq-medicao
                   matriz-rat-med.ct-codigo       = tt-matriz.ct-codigo
                   matriz-rat-med.sc-codigo       = tt-matriz.sc-codigo
                   matriz-rat-med.perc-rateio     = tt-matriz.perc-rateio
                   &IF "{&mguni_version}" >= "2.08" &THEN
                   matriz-rat-med.nr-ord-produ
                   &ELSE
                   matriz-rat-med.int-1
                   &ENDIF
                   = tt-matriz.int-1.
            &if '{&bf_mat_versao_ems}' >= '2.062' &THEN
                 ASSIGN matriz-rat-med.cod-unid-negoc = tt-matriz.cod-unid-negoc.
            &endif

            /* Integracao Contratos - criar registro para inclusao */
            {cnp/cnapi020.i1 2 1 CN0302D " " matriz-rat-med matriz-rat-med}
        END.

        /****MULTIPLANTA****/
        FIND FIRST matriz-rat-med
             WHERE matriz-rat-med.nr-contrato     = p-nr-contrato
               AND matriz-rat-med.num-seq-item    = p-num-seq-item
               AND matriz-rat-med.numero-ordem    = p-numero-ordem
               AND matriz-rat-med.num-seq-event   = p-num-seq-event
               AND matriz-rat-med.num-seq-medicao = p-num-seq-medicao NO-LOCK NO-ERROR.
        IF  AVAIL matriz-rat-med THEN DO:

            IF  NOT l-inclui THEN DO:
                RUN pi-trata-multiplanta (INPUT ROWID(matriz-rat-med),
                                          INPUT 1).
            END.
        END.
        ASSIGN l-eliminado = NO.
        /****MULTIPLANTA****/


        /* -elimina toda mat-rat-med-inv baseado na medicao-contrat,
           -recria  toda mat-rat-med-inv baseado na temp-table alterada
           -executa inapi403 eliminando tabelas relacionadas */

        IF  l-integra-cn-in-medicao THEN DO:
            &IF '{&bf_mat_versao_ems}' >= '2.062' &THEN
                IF  p-num-seq-medicao <> 0 
                AND INPUT FRAME {&FRAME-NAME} fi-i-num-ord-inv > 0 
                /*and l-altera-inv*/ THEN DO:         
                    /*elaine*/
                    RUN inp/inapi403.p (INPUT 2,  /* exclusao */
                                        INPUT ROWID(medicao-contrat),
                                        INPUT-OUTPUT TABLE rowerrors).     

                    &IF '{&bf_mat_versao_ems}' >= '2.07' &THEN 
                        IF medicao-contrat.seq-comp = 0 THEN DO:
                    &ELSE
                        IF SUBSTRING(medicao-contrat.char-1,1,3) = "" OR
                           SUBSTRING(medicao-contrat.char-1,1,3) = "0"  THEN DO:
                    &ENDIF
                        RUN pi-retorna-seq-comp IN THIS-PROCEDURE (OUTPUT i-seq-comp).
                        FIND CURRENT medicao-contrat EXCLUSIVE-LOCK NO-ERROR.
                        ASSIGN &IF '{&bf_mat_versao_ems}' >= '2.07' &THEN 
                                    medicao-contrat.seq-comp = i-seq-comp
                               &ELSE
                                    OVERLAY(medicao-contrat.char-1,1,3) = STRING(i-seq-comp)
                               &ENDIF
                               .
                    END.

                    RUN inp/inapi403.p (INPUT 1,  /* inclusao */
                                        INPUT ROWID(medicao-contrat),
                                        INPUT-OUTPUT TABLE rowerrors).     
                END.
            &ELSE            
                RUN pi-atualiza-mat-rat-med-inv IN h_inapi404 (INPUT ROWID(medicao-contrat),
                                                               INPUT-OUTPUT TABLE rowErrors).
            &ENDIF                            
        END.

        IF  CAN-FIND(FIRST RowErrors) THEN DO:
            {method/showmessage.i1}
            {method/showmessage.i2}
            UNDO, RETURN "ADM-ERROR".
        END.

       /* Integracao Contratos - executa API integraá∆o e valida erros */  
       {cnp/cnapi020.i2 ADM-ERROR yes}
       
       &IF '{&bf_mat_versao_ems}' >= '2.062' &THEN
        IF  l-unidade-negocio = YES  
        AND CAN-FIND(FIRST matriz-rat-med 
                     WHERE matriz-rat-med.numero-ordem = p-numero-ordem) THEN DO:
            FIND FIRST ordem-compra  WHERE ordem-compra.numero-ordem = p-numero-ordem EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN /*ordem-compra.conta-contabil = ""*/
                   ordem-compra.sc-codigo      = ""
                   ordem-compra.ct-codigo      = ""
                   ordem-compra.cod-unid-negoc = "".
        END.
       &ENDIF
    END.
END.

RUN dispatch IN wh-browse ('open-query':U).

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

    {&OPEN-QUERY-br-tt-conta}
    {&OPEN-QUERY-br-conta-cc-custo}

    ASSIGN c-ct-codigo-browse = ''
           c-sc-codigo-browse = ''.

    IF l-investimento THEN
        RUN pi-busca-contas (INPUT "INP").
    
    IF l-estoque THEN
        RUN pi-busca-contas (INPUT "CEP").

    {&OPEN-QUERY-br-tt-conta}
    {&OPEN-QUERY-br-conta-cc-custo}

    RETURN "ok".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-del w-window 
PROCEDURE pi-del :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Jamais remova a definiÁ„o do include a seguir de dentro da lÛgica do programa */
  {include/i-frm167.i}

   /* Caso o include a seguir nao atenda suas necessidades, apague-o  e crie sua propria
  logica usando-o como modelo. 
  */   
   /*{include/i-frm170.i}*/

  /*FIND FIRST tt-matriz
      WHERE ROWID(tt-matriz) = rowid( {&FIRST-TABLE-IN-QUERY-br-target-browse}) NO-ERROR.
  IF AVAIL tt-matriz THEN DO:
      
      ASSIGN de-perc-total = de-perc-total - tt-matriz.perc-rateio.
      
      DISP de-perc-total WITH FRAME {&FRAME-NAME}.
      
      DELETE tt-matriz.
  END.

  {&OPEN-QUERY-br-target-browse}*/


    DEF VAR i-cont  AS INT NO-UNDO.
    DEF VAR l-achou AS LOG NO-UNDO.
    
    IF (br-target-browse:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} <> 0) THEN DO:
            
        DO i-cont = 1 TO br-target-browse:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}:
          
            ASSIGN l-achou = NO
                   l-achou = br-target-browse:FETCH-SELECTED-ROW(i-cont) IN FRAME {&FRAME-NAME}.

            IF l-achou THEN DO:
                /*FIND FIRST b-tt-matriz
                     WHERE b-tt-matriz.ct-codigo = tt-matriz.ct-codigo
                       AND b-tt-matriz.sc-codigo = tt-matriz.sc-codigo NO-ERROR.
              IF AVAIL b-tt-matriz THEN DO:*/
                  IF AVAIL tt-matriz THEN DO:
                      ASSIGN de-perc-total = de-perc-total - tt-matriz.perc-rateio.
                      DISP de-perc-total WITH FRAME {&FRAME-NAME}.
                      DELETE tt-matriz.
                  END.
                  
              /*END.*/
            END.
        END.
    END.

    {&OPEN-QUERY-br-target-browse}




  /* Jamais remova a definiÁ„o do include a seguir de dentro da lÛgica do programa */
  {include/i-frm175.i}

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
   DEFINE INPUT PARAMETER v-row-target-browse AS ROWID NO-UNDO.

  /* Jamais remova a definiÁ„o do include a seguir de dentro da lÛgica do programa */
  {include/i-frm180.i}

  /* find tt-matriz where rowid(tt-matriz) = v-row-target-browse exclusive-lock no-error.  */
   IF  AVAIL tt-matriz THEN DO:
       ASSIGN de-perc-total = de-perc-total
                            - tt-matriz.perc-rateio.
       DISP de-perc-total WITH FRAME {&FRAME-NAME}.
       DELETE tt-matriz.
   END.

  /* Jamais remova a definiÁ„o do include a seguir de dentro da lÛgica do programa */
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
  /* Jamais remova a definiÁ„o do include a seguir de dentro da lÛgica do programa */
  
  {include/i-frm157.i}

  DEF VAR c-cod-estabel LIKE estabelec.cod-estabel.
  DEF VAR i-num-ord-inv AS INTEGER FORMAT ">>>>>,>>9":U INITIAL 0.

  EMPTY TEMP-TABLE tt-erro-aux1.

  ASSIGN c-cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel.
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

  
  IF  CAN-FIND (tt-matriz WHERE tt-matriz.ct-codigo = c-ct-codigo-browse
                            AND tt-matriz.sc-codigo = c-sc-codigo-browse
                &if '{&bf_mat_versao_ems}' >= '2.062' &THEN
                 and tt-matriz.cod-unid-negoc = ""
                &endif ) THEN DO:
      RUN utp/ut-msgs.p (INPUT "show",
                         INPUT 8497,
                         INPUT "").
      RETURN 'adm-error'.
  END.

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

      IF  c-modulo = "CEP" THEN DO:
          IF  tt-conta.finalid  = 0 OR  
             (tt-conta.finalid <> 1 AND tt-conta.finalid <> 2 AND
              tt-conta.finalid <> 3 AND tt-conta.finalid <> 5 AND tt-conta.finalid <> 6) THEN DO:
              RUN utp/ut-msgs.p (INPUT "show",
                                 INPUT 3298,
                                 INPUT "").
              RETURN 'adm-error'.
          END.
      END.
  END.

  CREATE tt-matriz.
  ASSIGN tt-matriz.nr-contrato     = p-nr-contrato
                 tt-matriz.num-seq-item    = p-num-seq-item
                 tt-matriz.numero-ordem    = p-numero-ordem
                 tt-matriz.num-seq-event   = p-num-seq-event
                 tt-matriz.num-seq-medicao = p-num-seq-medicao
                 tt-matriz.ct-codigo       = c-ct-codigo-browse
                 tt-matriz.sc-codigo       = c-sc-codigo-browse
                 tt-matriz.perc-rateio     = 0
                 tt-matriz.int-1           = 0.

  {&OPEN-QUERY-br-target-browse}

  /* Jamais remova a definiÁ„o do include a seguir de dentro da lÛgica do programa */
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

  ASSIGN c-cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel.
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

  
  IF  CAN-FIND (tt-matriz WHERE tt-matriz.ct-codigo = c-ct-codigo-browse
                &if '{&bf_mat_versao_ems}' >= '2.062' &THEN
                 and tt-matriz.cod-unid-negoc = ""
                &endif ) THEN DO:
      RUN utp/ut-msgs.p (INPUT "show",
                         INPUT 8497,
                         INPUT "").
      RETURN 'adm-error'.
  END.

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

      IF  c-modulo = "CEP" THEN DO:
          IF  tt-conta.finalid  = 0 OR  
             (tt-conta.finalid <> 1 AND tt-conta.finalid <> 2 AND
              tt-conta.finalid <> 3 AND tt-conta.finalid <> 5 AND tt-conta.finalid <> 6) THEN DO:
              RUN utp/ut-msgs.p (INPUT "show",
                                 INPUT 3298,
                                 INPUT "").
              RETURN 'adm-error'.
          END.
      END.
  END.
  
  CREATE tt-matriz.
  ASSIGN tt-matriz.nr-contrato     = p-nr-contrato
                 tt-matriz.num-seq-item    = p-num-seq-item
                 tt-matriz.numero-ordem    = p-numero-ordem
                 tt-matriz.num-seq-event   = p-num-seq-event
                 tt-matriz.num-seq-medicao = p-num-seq-medicao
                 tt-matriz.ct-codigo       = c-ct-codigo-browse
                 tt-matriz.sc-codigo       = c-sc-codigo-browse
                 tt-matriz.perc-rateio     = 0
                 tt-matriz.int-1           = 0.

  {&OPEN-QUERY-br-target-browse}

  /* Jamais remova a definiÁ„o do include a seguir de dentro da lÛgica do programa */
  {include/i-frm165.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-retorna-seq-comp w-window 
PROCEDURE pi-retorna-seq-comp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF OUTPUT PARAM pi-seq-comp AS INT NO-UNDO.                      
                      
    DEF VAR i-cont AS INT NO-UNDO.    
    DEF VAR iempresa LIKE estabelec.ep-codigo.

    FIND ordem-compra NO-LOCK WHERE
         ordem-compra.numero-ordem = medicao-contrat.numero-ordem NO-ERROR.

    /*elaine*/
    ASSIGN iempresa = i-ep-codigo-usuario.
    IF AVAIL ordem-compra THEN DO:
        IF ordem-compra.num-ord-inv <> 0 THEN RETURN "OK":U.
        FOR FIRST estabelec FIELDS (cod-estabel ep-codigo)
            WHERE estabelec.cod-estabel = ordem-compra.cod-estabel NO-LOCK:
            IF estabelec.ep-codigo <> "0" THEN
                ASSIGN iempresa = estabelec.ep-codigo. 
        END.
    END.

    FIND LAST plano-aprov 
        WHERE plano-aprov.ep-codigo      = iempresa
          AND plano-aprov.num-ord-magnus = medicao-contrat.num-ord-inv
          AND plano-aprov.it-codigo      = ordem-compra.it-codigo NO-LOCK NO-ERROR.              

    IF AVAIL plano-aprov THEN        
        ASSIGN pi-seq-comp = plano-aprov.seq-planej + 1.        
    ELSE 
        ASSIGN pi-seq-comp = 1.
/*
    FIND LAST ord-ped USE-INDEX emp-ped
        WHERE ord-ped.ep-codigo    = i-ep-codigo-usuario
          AND ord-ped.num-pedido   = ordem-compra.num-pedido
          AND ord-ped.seq-pedido   = 0
          AND ord-ped.cod-estabel  = ordem-compra.cod-estabel
          AND ord-ped.cod-area     = 0
          AND ord-ped.num-ord-comp = ordem-compra.numero-ordem NO-LOCK NO-ERROR.

    IF AVAIL ord-ped THEN DO:
        IF ord-ped.seq-comp >= 999 THEN DO:
            DO  i-cont = 1 TO 999:
                FIND LAST ord-ped USE-INDEX emp-ped
                    WHERE ord-ped.ep-codigo    = i-ep-codigo-usuario
                      AND ord-ped.num-pedido   = ordem-compra.num-pedido
                      AND ord-ped.seq-pedido   = 0
                      AND ord-ped.cod-estabel  = ordem-compra.cod-estabel
                      AND ord-ped.cod-area     = 0
                      AND ord-ped.num-ord-comp = ordem-compra.numero-ordem 
                      AND ord-ped.seq-comp     = i-cont NO-LOCK NO-ERROR.
                IF  NOT AVAIL ord-ped THEN
                    LEAVE.
            END.
        END.

        IF  i-cont >= 999 THEN DO:                           
            CREATE RowErrors.
            ASSIGN RowErrors.ErrorNumber      = 17006
                   RowErrors.ErrorDescription = "Estouro da Sequencia de mediá∆o."
                   RowErrors.ErrorType        = "EMS":U
                   RowErrors.ErrorSubType     = "ERROR".

            RETURN "NOK":U.
        END.
        ELSE IF i-cont > 0 THEN
             ASSIGN pi-seq-comp = i-cont.
        ELSE ASSIGN pi-seq-comp = ord-ped.seq-comp + 1.
        
    END.
*/

    RETURN "OK":U.

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
  /* Jamais remova a definiÁ„o do include a seguir de dentro da lÛgica do programa */

  {include/i-frm145.i}

  /* LÛgica padr„o para apresentaÁ„o do registro da tabela pai caso esta lÛgica atenda 
     as suas necessidades chame o include a seguir no corpo da procedure (retire-o de
     dentro do coment·rio). Em caso contr·rio crie sua prÛpria lÛgica usando a 
     do include como modelo.
  {include/i-frm150.i} 
  */

  {include/i-frm155.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-trata-multiplanta w-window 
PROCEDURE pi-trata-multiplanta :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-rowid  AS ROWID   NO-UNDO.
  DEFINE INPUT PARAMETER p-estado AS INTEGER NO-UNDO.

  IF  (p-num-seq-event = 0 AND p-num-seq-medicao = 0) OR       /* matriz da ordem  */
      (p-num-seq-event > 0 AND p-num-seq-medicao = 0) THEN DO: /* matriz do evento */

      IF  param-global.modulo-mp              AND
          ordem-compra.cod-estab-gestor <> "" THEN DO:

          ASSIGN l-eliminado = YES.
          RUN ccp/ccapi200.p (INPUT IF p-num-seq-event = 0
                                       THEN "matriz-rat-med-ordem":U
                                       ELSE "matriz-rat-med":U,
                              INPUT p-rowid,
                              INPUT p-estado,
                              INPUT-OUTPUT TABLE tt-erro).
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida-ttmatriz w-window 
PROCEDURE pi-valida-ttmatriz :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Esta l¢gica constava na trigger do bot∆o confirma */

  ASSIGN l-grava    = YES
         de-percent = 0.
  &IF '{&bf_mat_versao_ems}' >= '2.062' &THEN
      EMPTY TEMP-TABLE tt-matriz-inv.
  &ENDIF

  FOR EACH  tt-matriz
      WHERE tt-matriz.nr-contrato     = p-nr-contrato
        AND tt-matriz.num-seq-item    = p-num-seq-item
        AND tt-matriz.numero-ordem    = p-numero-ordem
        AND tt-matriz.num-seq-event   = p-num-seq-event
        AND tt-matriz.num-seq-medicao = p-num-seq-medicao:

      &IF '{&bf_mat_versao_ems}' >= '2.062' &THEN
          IF  l-unidade-negocio 
          AND l-mat-unid-negoc  THEN DO:
              IF trim(tt-matriz.cod-unid-neg) = ''  THEN DO:
                  RUN utp/ut-msgs.p (INPUT "show",
                                     INPUT 2,
                                     INPUT "Unidade Neg¢cio").
                  ASSIGN l-grava = NO.
                  RETURN 'adm-error':U.
              END.
          END.

          IF INPUT FRAME {&FRAME-NAME} fi-i-num-ord-inv > 0 THEN DO:                                                         
              FIND FIRST tt-matriz-inv NO-LOCK
                   WHERE tt-matriz-inv.cod-unid-neg = tt-matriz.cod-unid-neg NO-ERROR.
              IF NOT AVAIL tt-matriz-inv THEN DO:
                  CREATE tt-matriz-inv.
                  ASSIGN tt-matriz-inv.cod-unid-neg = tt-matriz.cod-unid-neg.
              END.
              ASSIGN tt-matriz-inv.perc-rateio = tt-matriz-inv.perc-rateio + tt-matriz.perc-rateio.
          END.
      &ENDIF  

      IF  tt-matriz.perc-rateio = 0 THEN DO:
          RUN utp/ut-msgs.p (INPUT "show",
                             INPUT 8462,
                             INPUT "").
          ASSIGN l-grava = NO.
          RETURN 'adm-error'.
      END.
      ELSE
          ASSIGN de-percent = de-percent
                            + tt-matriz.perc-rateio.

      IF l-spp-integra-cn-mi-med AND
         tt-matriz.int-1:VISIBLE IN BROWSE br-target-browse  THEN DO:

          FOR FIRST ord-prod WHERE 
                    ord-prod.nr-ord-prod = tt-matriz.int-1 NO-LOCK: END.

          IF NOT AVAIL ord-prod THEN DO:
             IF tt-matriz.int-1 <> 0 THEN DO:
                /* Ordem de Produá∆o n∆o cadastrada ! */
                RUN utp/ut-msgs.p(INPUT "show":U, INPUT 1879, INPUT "").
                ASSIGN l-grava = NO.
                RETURN 'adm-error'.
             END.
          END.
          ELSE /* Se OM n∆o est† sendo modificada/inclu°da, n∆o consite situaá∆o */
               IF NOT CAN-FIND (FIRST matriz-rat-med WHERE 
                                      matriz-rat-med.nr-contrato     = tt-matriz.nr-contrato
                                  AND matriz-rat-med.num-seq-item    = tt-matriz.num-seq-item
                                  AND matriz-rat-med.numero-ordem    = tt-matriz.numero-ordem   
                                  AND matriz-rat-med.num-seq-event   = tt-matriz.num-seq-event  
                                  AND matriz-rat-med.num-seq-medicao = tt-matriz.num-seq-medicao
                                  AND matriz-rat-med.ct-codigo       = tt-matriz.ct-codigo
                                  AND matriz-rat-med.sc-codigo       = tt-matriz.sc-codigo
                                  AND &IF "{&mguni_version}" >= "2.08" &THEN
                                      matriz-rat-med.nr-ord-produ
                                      &ELSE
                                      matriz-rat-med.int-1
                                      &ENDIF
                                             = tt-matriz.int-1) AND
                  ord-prod.estado = 8 THEN DO:
                     /* Ordem de Produá∆o finalizada ! */
                     RUN utp/ut-msgs.p(INPUT "show":U, INPUT 1882, INPUT "").
                     ASSIGN l-grava = NO.
                     RETURN 'adm-error'.
               END.
      END.
  END.

  &IF '{&bf_mat_versao_ems}' >= '2.062' &THEN
      EMPTY TEMP-TABLE tt-erro.
      IF INPUT FRAME {&FRAME-NAME} fi-i-num-ord-inv > 0 
          AND  l-mat-unid-negoc THEN DO:
          assign iEmpresa = INT (param-global.empresa-prin).
          IF NOT AVAIL estabelec 
          OR (AVAIL estabelec 
                AND estabelec.cod-estabel <> contrato-for.cod-estabel) THEN
              FIND FIRST estabelec WHERE 
                         estabelec.cod-estabel = contrato-for.cod-estabel NO-LOCK NO-ERROR.
/*           &IF "{&mguni_version}" >= "2.071" &THEN                */
/*           if avail estabelec AND estabelec.ep-codigo <> "" then  */
/*           &ELSE                                                  */
/*           if avail estabelec AND estabelec.ep-codigo <> 0 then   */
/*           &ENDIF                                                 */
             ASSIGN iempresa = INT (estabelec.ep-codigo).

          RUN recebeEmpresa IN h-boiv029 (INPUT iempresa).
          RUN validateMatrizUnidNegoc IN h-boiv029 (INPUT TABLE tt-matriz-inv, 
                                                    INPUT INPUT FRAME {&FRAME-NAME} fi-i-num-ord-inv, 
                                                    OUTPUT TABLE tt-erro).
          IF NOT l-unidade-negocio AND 
             NOT CAN-FIND (FIRST tt-matriz-inv NO-LOCK) AND 
             NOT CAN-FIND (FIRST tt-erro NO-LOCK)   THEN DO:
              RUN utp/ut-msgs.p ("msg":U, 17006, "Deve haver matriz de rateio cadastrada quando informado ordem de investimento").
              CREATE tt-erro.
              ASSIGN tt-erro.i-sequen = 1
                     tt-erro.cd-erro  = 17006
                     tt-erro.mensagem = RETURN-VALUE.
          END.

          IF CAN-FIND (FIRST tt-erro) THEN DO:
              RUN cdp/cd0666.w (INPUT TABLE tt-erro).
              RETURN 'adm-error'.  
          END.
      END.
  &ENDIF

  IF  de-percent > 0 AND de-percent < 100 THEN DO:
      RUN utp/ut-msgs.p(INPUT "show":U,
                        INPUT 17167,
                        INPUT "Percentual total" + "~~" + "100%").
      ASSIGN l-grava = NO.
      RETURN 'adm-error'.
  END.
  ELSE
      ASSIGN l-grava = YES.

  IF  de-percent > 100 THEN DO:
      RUN utp/ut-msgs.p (INPUT "show",
                         INPUT 8451,
                         INPUT "").
      ASSIGN l-grava = NO.
      RETURN 'adm-error'.
  END.
  ELSE
      ASSIGN l-grava = YES.

  RETURN "OK":U.

/* Fim da l¢gica do bot∆o confirma */

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
  {src/adm/template/snd-list.i "medicao-contrat"}
  {src/adm/template/snd-list.i "estabelec"}
  {src/adm/template/snd-list.i "item"}
  {src/adm/template/snd-list.i "tt-conta"}
  {src/adm/template/snd-list.i "tt-matriz"}
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
            n∆o ser incluido no browse de destino
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
            n∆o ser eliminado do browse de destino
    Notes:  
------------------------------------------------------------------------------*/

  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems2cadme        PROGRESS
          ems2movme        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer moeda for ems2cadme.moeda.

{include/i-prgvrs.i B08AD183 2.00.00.018}  /*** 010018 ***/

&IF "{&EMSFND_VERSION}" >= "1.00"
&THEN
{include/i-license-manager.i B08AD183 MUT}
&ENDIF


/*------------------------------------------------------------------------

  File:  

  Description: from BROWSER.W - Basic SmartBrowser Object Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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
&Scop adm-attribute-dlg support/browserd.w

/* {utp/ut-glob.i} */

/* Miniflexibiliza‡Æo */  
  {include/i_dbvers.i}
  {cdp/cdcfgfin.i}

def var c-modalidade         as char format "x(15)"       no-undo.
def var c-lista-modalidade   as char format "x(15)"       no-undo.
def var c-mo-codigo          as char format "x(03)"       no-undo.
def var c-cod-esp-inicial    as char init "AA"            no-undo.
def var c-cod-esp-final      as char init "ZZ"            no-undo.
def var d-dt-trans-inicial   as date init today           no-undo.
def var d-dt-trans-final     as date init today           no-undo.
def var da-data-sel          as date init today           no-undo.
def var i-dias-atraso        as integer init 0            no-undo.
def var l-atualiza           as logical                   no-undo.
def var de-cotacao           like titulo.cotacao-dia      no-undo.
def var de-vl-abatimen       like mov-tit.vl-abatimen-me  no-undo.
def var de-vl-antecip        like mov-tit.vl-antecip-me   no-undo.
def var de-vl-baixa          like mov-tit.vl-baixa-me     no-undo.
def var de-vl-desconto       like mov-tit.vl-desconto-me  no-undo.
def var de-vl-desp-banc      like mov-tit.vl-desp-banc-me no-undo.
def var de-vl-juros-rec      like mov-tit.vl-juros-rec-me no-undo.
def var de-vl-var-monet      like mov-tit.vl-var-monet    no-undo.

def buffer bf-emitente for emitente.
DEF BUFFER b-mov-tit   FOR mov-tit.
DEFINE BUFFER b-titulo FOR titulo.
def var v-log-sl-ab-venc as logical no-undo.
DEF VAR l-imprime-cli    AS LOGICAL NO-UNDO.

DEF VAR l-venct-matriz AS LOGICAL INITIAL NO NO-UNDO.
DEF VAR cName          AS CHAR NO-UNDO.
DEF VAR lDesc          AS LOGICAL NO-UNDO.
DEF VAR c-ordemcampo   AS CHAR    NO-UNDO.

def new global shared var i-ep-codigo-usuario  like ems2cadme.empresa.ep-codigo no-undo.
def new Global shared var l-implanta           as logical    init no.
def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.

DEFINE temp-table tt-documento no-undo like titulo
       field row-documento  as rowid
       field c-modalidade   like titulo.modalidade
       field c-mo-codigo    as char format "x(03)"
       field de-saldo       like titulo.vl-saldo
       field de-saldo-me    like titulo.vl-saldo-me
       field i-dias         as int init 0
       field de-juros       as dec 
       field de-total-juros as dec.

define temp-table tt-estat no-undo

       field destino              as integer
       field arquivo              as char format "x(35)"
       field usuario              as char format "x(12)"
       field data-exec            as date
       field hora-exec            as integer
       field tipo-relat           as INTEGER
       FIELD cod-emitente         LIKE titulo.cod-emitente
       FIELD nome-abrev           LIKE titulo.nome-abrev
       FIELD fi-periodo           as char   FORMAT "99/9999"
       field da-data-conver       as date   FORMAT "99/99/9999"
       FIELD dt-maior-tit         AS DATE   FORMAT "99/99/9999"
       FIELD c-sigla-4            AS CHAR   FORMAT "X(5)"
       FIELD vl-maior-tit         AS DEC    FORMAT ">>>,>>>,>>>,>>9,9"
       FIELD dt-ult-tit           AS DATE   FORMAT "99/99/9999"
       FIELD c-sigla-5            AS CHAR   FORMAT "X(5)"
       FIELD vl-ult-tit           AS DEC    FORMAT ">>>,>>>,>>>,>>9,9"
       FIELD c-sigla-1            AS CHAR   FORMAT "X(5)"
       FIELD fi-vendas            AS DEC    FORMAT ">>>,>>>,>>>,>>9,9"
       FIELD c-sigla-2            AS CHAR   FORMAT "X(5)"
       FIELD fi-vendas-acumuladas AS DEC    FORMAT ">>>,>>>,>>>,>>9,9"
       FIELD c-sigla-3            AS CHAR   FORMAT "X(5)"
       FIELD fi-saldo-aberto      AS DEC    FORMAT ">>>,>>>,>>>,>>9,9"
       FIELD c-sigla-6            AS CHAR   FORMAT "X(5)"
       FIELD fi-saldo-aberto-venc AS DEC    FORMAT ">>>,>>>,>>>,>>9,9"
       field fi-label-periodo     as char   format "x(15)"
       field fi-periodo-1         as char   format "x(15)"
       field fi-periodo-2         as char   format "x(15)"
       field fi-periodo-3         as char   format "x(15)"
       field fi-periodo-4         as char   format "x(15)"
       field fi-periodo-5         as char   format "x(15)"
       field fi-periodo-6         as char   format "x(15)"
       field fi-periodo-7         as char   format "x(15)"
       field fi-periodo-8         as char   format "x(15)"
       field fi-periodo-9         as char   format "x(15)"
       field fi-periodo-10        as char   format "x(15)"
       field fi-periodo-11        as char   format "x(15)"
       field fi-periodo-12        as char   format "x(15)"
       FIELD fi-label-media       AS CHAR   FORMAT "x(15)"
       FIELD fi-label-atm         AS CHAR   FORMAT "x(15)"
       field fi-atm-1             as INT    format "->>9"
       field fi-atm-2             as INT    format "->>9"
       field fi-atm-3             as INT    format "->>9"
       field fi-atm-4             as INT    format "->>9"
       field fi-atm-5             as INT    format "->>9"
       field fi-atm-6             as INT    FORMAT "->>9"
       field fi-atm-7             as INT    format "->>9"
       field fi-atm-8             as INT    format "->>9"
       field fi-atm-9             as INT    format "->>9"
       field fi-atm-10            as INT    format "->>9"
       field fi-atm-11            as INT    format "->>9"
       field fi-atm-12            as INT    format "->>9"
       field fi-atm-media         as INT    format "->>9"
       FIELD fi-label-pmr         AS CHAR   FORMAT "x(15)"
       field fi-pmr-1             as INT    format "->>9"
       field fi-pmr-2             as INT    format "->>9"
       field fi-pmr-3             as INT    format "->>9"
       field fi-pmr-4             as INT    format "->>9"
       field fi-pmr-5             as INT    format "->>9"
       field fi-pmr-6             as INT    FORMAT "->>9"
       field fi-pmr-7             as INT    format "->>9"
       field fi-pmr-8             as INT    format "->>9"
       field fi-pmr-9             as INT    format "->>9"
       field fi-pmr-10            as INT    format "->>9"
       field fi-pmr-11            as INT    format "->>9"
       field fi-pmr-12            as INT    format "->>9"
       field fi-pmr-media         as INT    format "->>9"
       FIELD fi-label-vendas      AS CHAR   FORMAT "x(15)"
       field fi-vendas-1          as dec    format ">>>,>>>,>>>,>>9,9"
       field fi-vendas-2          as DEC    format ">>>,>>>,>>>,>>9,9"
       field fi-vendas-3          as DEC    format ">>>,>>>,>>>,>>9,9"
       field fi-vendas-4          as DEC    format ">>>,>>>,>>>,>>9,9"
       field fi-vendas-5          as DEC    format ">>>,>>>,>>>,>>9,9"
       field fi-vendas-6          as DEC    format ">>>,>>>,>>>,>>9,9"
       field fi-vendas-7          as DEC    format ">>>,>>>,>>>,>>9,9"
       field fi-vendas-8          as DEC    format ">>>,>>>,>>>,>>9,9"
       field fi-vendas-9          as DEC    format ">>>,>>>,>>>,>>9,9"
       field fi-vendas-10         as DEC    format ">>>,>>>,>>>,>>9,9"
       field fi-vendas-11         as DEC    format ">>>,>>>,>>>,>>9,9"
       field fi-vendas-12         as DEC    format ">>>,>>>,>>>,>>9,9"
       field fi-vendas-media      as DEC    format ">>>,>>>,>>>,>>9,9".

define temp-table tt-liq no-undo

       FIELD c-barra         AS CHAR FORMAT "x(01)" INIT "/"
       FIELD c-barra2        LIKE c-barra           INIT "/"
       FIELD cod-emitente    LIKE titulo.cod-emitente
       FIELD nome-abrev      LIKE titulo.nome-abrev
       FIELD ep-codigo       LIKE mov-tit.ep-codigo
       FIELD cod-estabel     LIKE mov-tit.cod-estabel
       FIELD cod-esp         LIKE mov-tit.cod-esp
       FIELD serie           LIKE mov-tit.serie
       FIELD nr-docto        LIKE mov-tit.nr-docto
       FIELD parcela         LIKE mov-tit.parcela 
       FIELD vl-baixa        LIKE mov-tit.vl-baixa
       FIELD dt-credito      LIKE mov-tit.dt-credito
       FIELD dt-baixa        LIKE mov-tit.dt-baixa 
       FIELD cod-portador    LIKE mov-tit.cod-portador
       FIELD de-total-baixa  LIKE mov-tit.vl-baixa
       FIELD c-modalidade    AS CHAR
       FIELD vl-baixa-me     LIKE mov-tit.vl-baixa-me
       FIELD mo-codigo       LIKE mov-tit.mo-codigo
       FIELD cotacao-dia     LIKE mov-tit.cotacao-dia
       FIELD vl-desconto     LIKE mov-tit.vl-desconto
       FIELD vl-desconto-me  LIKE mov-tit.vl-desconto-me  
       FIELD vl-juros-rec    LIKE mov-tit.vl-juros-rec
       FIELD vl-juros-rec-me LIKE mov-tit.vl-juros-rec-me
       FIELD vl-desp-banc    LIKE mov-tit.vl-desp-banc 
       FIELD vl-desp-banc-me LIKE mov-tit.vl-desp-banc-me
       FIELD vl-abatimen     LIKE mov-tit.vl-abatimen
       FIELD vl-abatimen-me  LIKE mov-tit.vl-abatimen-me
       FIELD dt-vencimen     LIKE mov-tit.dt-vencimen
       FIELD esp-antecip     LIKE mov-tit.esp-antecip
       FIELD serie-antecip   LIKE mov-tit.serie-antecip
       FIELD doc-antecip     LIKE mov-tit.doc-antecip
       FIELD parc-antecip    LIKE mov-tit.parc-antecip 
       FIELD vl-antecip      LIKE mov-tit.vl-antecip
       FIELD vl-antecip-me   LIKE mov-tit.vl-antecip-me
       FIELD referencia      LIKE mov-tit.referencia 
       FIELD i-dias-atraso   AS INT.

define temp-table tt-hist no-undo
       FIELD cod-emitente LIKE titulo.cod-emitente
       FIELD nome-abrev   LIKE titulo.nome-abrev
       FIELD dt-his-emit  LIKE his-emit.dt-his-emit
       FIELD horario      LIKE his-emit.horario
       FIELD historico    LIKE his-emit.historico.

define temp-table tt-doc no-undo

       FIELD c-barra                      AS CHAR FORMAT "x(01)" INIT "/"
       FIELD c-barra2                     AS CHAR FORMAT "x(01)" INIT "/"
       FIELD cod-estabel                  LIKE titulo.cod-estabel
       FIELD cod-esp                      LIKE titulo.cod-esp
       FIELD serie                        LIKE titulo.serie
       FIELD nr-docto                     LIKE titulo.nr-docto
       FIELD parcela                      LIKE titulo.parcela
       FIELD cod-portador                 LIKE mov-tit.cod-portador
       FIELD c-modalidade                 AS CHAR
       FIELD vl-saldo                     LIKE titulo.vl-saldo
       FIELD vl-original                  LIKE titulo.vl-original
       field de-juros                     as dec
       FIELD de-total-juros               AS DEC
       FIELD dt-emissao                   LIKE titulo.dt-emissao
       FIELD dt-vencimen                  LIKE titulo.dt-vencimen
       FIELD cod-emitente                 LIKE titulo.cod-emitente
       FIELD nome-abrev                   LIKE titulo.nome-abrev
       FIELD i-dias                       AS int
       FIELD de-total-saldo-aberto        AS DEC
       FIELD de-saldo-anterior-do-cliente AS DEC.

def VAR r-emitente as rowid no-undo.
def VAR fi-total-titulos  as dec no-undo.
def VAR fi-total-matriz   as dec no-undo.
def VAR fi-saldo-anterior as dec no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES emitente
&Scoped-define FIRST-EXTERNAL-TABLE emitente


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR emitente.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mov-tit

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table mov-tit.cod-estabel mov-tit.cod-esp ~
mov-tit.serie mov-tit.nr-docto mov-tit.parcela mov-tit.vl-baixa ~
mov-tit.dt-credito mov-tit.dt-baixa mov-tit.cod-portador ~
entry(mov-tit.modalidade, c-lista-modalidade) @ c-modalidade ~
mov-tit.vl-baixa-me mov-tit.mo-codigo mov-tit.cotacao-dia ~
mov-tit.vl-desconto mov-tit.vl-desconto-me mov-tit.vl-juros-rec ~
mov-tit.vl-juros-rec-me mov-tit.vl-desp-banc mov-tit.vl-desp-banc-me ~
mov-tit.vl-abatimen mov-tit.vl-abatimen-me mov-tit.dt-vencimen ~
mov-tit.esp-antecip mov-tit.serie-antecip mov-tit.doc-antecip ~
mov-tit.parc-antecip mov-tit.vl-antecip mov-tit.vl-antecip-me ~
mov-tit.referencia fn-dias-atraso(i-dias-atraso) @ i-dias-atraso 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table 
&Scoped-define QUERY-STRING-br-table FOR EACH mov-tit WHERE mov-tit.cod-emitente = emitente.cod-emitente ~
      AND mov-tit.ep-codigo = i-ep-codigo-usuario ~
AND l-atualiza = yes ~
AND ( mov-tit.transacao = 2 or mov-tit.transacao = 3) ~
AND mov-tit.cod-esp  >= c-cod-esp-inicial ~
AND mov-tit.cod-esp  <= c-cod-esp-final ~
AND mov-tit.dt-trans >= d-dt-trans-inicial ~
AND mov-tit.dt-trans <= d-dt-trans-final ~
AND mov-tit.tipo     <> 7 NO-LOCK ~
    BY mov-tit.cod-estabel ~
       BY mov-tit.cod-esp ~
        BY mov-tit.serie ~
         BY mov-tit.nr-docto ~
          BY mov-tit.parcela
&Scoped-define OPEN-QUERY-br-table OPEN QUERY br-table FOR EACH mov-tit WHERE mov-tit.cod-emitente = emitente.cod-emitente ~
      AND mov-tit.ep-codigo = i-ep-codigo-usuario ~
AND l-atualiza = yes ~
AND ( mov-tit.transacao = 2 or mov-tit.transacao = 3) ~
AND mov-tit.cod-esp  >= c-cod-esp-inicial ~
AND mov-tit.cod-esp  <= c-cod-esp-final ~
AND mov-tit.dt-trans >= d-dt-trans-inicial ~
AND mov-tit.dt-trans <= d-dt-trans-final ~
AND mov-tit.tipo     <> 7 NO-LOCK ~
    BY mov-tit.cod-estabel ~
       BY mov-tit.cod-esp ~
        BY mov-tit.serie ~
         BY mov-tit.nr-docto ~
          BY mov-tit.parcela.
&Scoped-define TABLES-IN-QUERY-br-table mov-tit
&Scoped-define FIRST-TABLE-IN-QUERY-br-table mov-tit


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BT-IMP tg-atualiza bt-filtro br-table RECT-1 ~
RECT-11 
&Scoped-Define DISPLAYED-OBJECTS c-sigla-1 tg-atualiza fi-total-baixa 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS
><EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
************************
* Initialize Filter Attributes */
RUN set-attribute-list IN THIS-PROCEDURE ('
  Filter-Value=':U).
/************************
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-dias-atraso B-table-Win 
FUNCTION fn-dias-atraso RETURNS INTEGER
  ( INPUT i-dias-atraso AS INTEGER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-filtro 
     LABEL "&Filtro" 
     SIZE 15.72 BY 1.

DEFINE BUTTON BT-IMP 
     LABEL "Imprimir" 
     SIZE 15.72 BY 1.

DEFINE VARIABLE c-sigla-1 AS CHARACTER FORMAT "X(5)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-total-baixa AS DECIMAL FORMAT "->>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67.57 BY 2.67.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 21.29 BY 2.67.

DEFINE VARIABLE tg-atualiza AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.14 BY .88.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      mov-tit SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _STRUCTURED
  QUERY br-table NO-LOCK DISPLAY
      mov-tit.cod-estabel FORMAT "x(4)":U
      mov-tit.cod-esp FORMAT "!!":U
      mov-tit.serie FORMAT "x(5)":U
      mov-tit.nr-docto FORMAT "x(16)":U
      mov-tit.parcela FORMAT "x(2)":U
      mov-tit.vl-baixa FORMAT "->>>,>>>,>>>,>>9.99":U
      mov-tit.dt-credito FORMAT "99/99/9999":U
      mov-tit.dt-baixa COLUMN-LABEL "Dt Baixa" FORMAT "99/99/9999":U
      mov-tit.cod-portador FORMAT ">>>>>>9":U
      entry(mov-tit.modalidade, c-lista-modalidade) @ c-modalidade
      mov-tit.vl-baixa-me FORMAT "->>>,>>>,>>>,>>9.99":U
      mov-tit.mo-codigo FORMAT ">9":U
      mov-tit.cotacao-dia FORMAT ">>>,>9.99999999":U
      mov-tit.vl-desconto FORMAT ">>>>>>>,>>9.99":U
      mov-tit.vl-desconto-me FORMAT ">>>>>>>,>>9.99":U
      mov-tit.vl-juros-rec FORMAT ">>>>>>>,>>9.99":U
      mov-tit.vl-juros-rec-me FORMAT ">>>>>>>,>>9.99":U
      mov-tit.vl-desp-banc FORMAT ">>>,>>>,>>>,>>9.99":U
      mov-tit.vl-desp-banc-me FORMAT ">>>,>>>,>>>,>>9.99":U
      mov-tit.vl-abatimen FORMAT ">>>>>>>,>>9.99":U
      mov-tit.vl-abatimen-me FORMAT ">>>>>>>,>>9.99":U
      mov-tit.dt-vencimen FORMAT "99/99/9999":U
      mov-tit.esp-antecip FORMAT "!!":U
      mov-tit.serie-antecip FORMAT "x(5)":U
      mov-tit.doc-antecip FORMAT "x(16)":U
      mov-tit.parc-antecip FORMAT "x(2)":U
      mov-tit.vl-antecip FORMAT ">>>>>>>,>>9.99":U
      mov-tit.vl-antecip-me FORMAT ">>>>>>>,>>9.99":U
      mov-tit.referencia FORMAT "x(10)":U
      fn-dias-atraso(i-dias-atraso) @ i-dias-atraso FORMAT "->>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 88.86 BY 8.08.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BT-IMP AT ROW 10 COL 31
     c-sigla-1 AT ROW 9.5 COL 64 COLON-ALIGNED
     tg-atualiza AT ROW 9.38 COL 4.29
     bt-filtro AT ROW 10.54 COL 3.86
     fi-total-baixa AT ROW 9.5 COL 69 COLON-ALIGNED NO-LABEL
     br-table AT ROW 1 COL 1.14
     RECT-1 AT ROW 9.13 COL 22.43
     RECT-11 AT ROW 9.13 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: ems2cadme.emitente
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
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
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 10.79
         WIDTH              = 89.29.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{include/c-browse.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
/* BROWSE-TAB br-table fi-total-baixa F-Main */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:HEIGHT           = 10.79
       FRAME F-Main:WIDTH            = 89.29.

/* SETTINGS FOR FILL-IN c-sigla-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-total-baixa IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _TblList          = "ems2movme.mov-tit WHERE ems2cadme.emitente <external> ..."
     _Options          = "NO-LOCK KEY-PHRASE"
     _OrdList          = "ems2movme.mov-tit.cod-estabel|yes,ems2movme.mov-tit.cod-esp|yes,ems2movme.mov-tit.serie|yes,ems2movme.mov-tit.nr-docto|yes,ems2movme.mov-tit.parcela|yes"
     _JoinCode[1]      = "ems2movme.mov-tit.cod-emitente = ems2cadme.emitente.cod-emitente"
     _Where[1]         = "mov-tit.ep-codigo = i-ep-codigo-usuario
AND l-atualiza = yes
AND ( mov-tit.transacao = 2 or mov-tit.transacao = 3)
AND mov-tit.cod-esp  >= c-cod-esp-inicial
AND mov-tit.cod-esp  <= c-cod-esp-final
AND mov-tit.dt-trans >= d-dt-trans-inicial
AND mov-tit.dt-trans <= d-dt-trans-final
AND mov-tit.tipo     <> 7"
     _FldNameList[1]   > ems2movme.mov-tit.cod-estabel
"mov-tit.cod-estabel" ? "x(4)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = ems2movme.mov-tit.cod-esp
     _FldNameList[3]   = ems2movme.mov-tit.serie
     _FldNameList[4]   = ems2movme.mov-tit.nr-docto
     _FldNameList[5]   = ems2movme.mov-tit.parcela
     _FldNameList[6]   = ems2movme.mov-tit.vl-baixa
     _FldNameList[7]   = ems2movme.mov-tit.dt-credito
     _FldNameList[8]   > ems2movme.mov-tit.dt-baixa
"mov-tit.dt-baixa" "Dt Baixa" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ems2movme.mov-tit.cod-portador
"mov-tit.cod-portador" ? ">>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"entry(mov-tit.modalidade, c-lista-modalidade) @ c-modalidade" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   = ems2movme.mov-tit.vl-baixa-me
     _FldNameList[12]   = ems2movme.mov-tit.mo-codigo
     _FldNameList[13]   = ems2movme.mov-tit.cotacao-dia
     _FldNameList[14]   = ems2movme.mov-tit.vl-desconto
     _FldNameList[15]   = ems2movme.mov-tit.vl-desconto-me
     _FldNameList[16]   = ems2movme.mov-tit.vl-juros-rec
     _FldNameList[17]   = ems2movme.mov-tit.vl-juros-rec-me
     _FldNameList[18]   = ems2movme.mov-tit.vl-desp-banc
     _FldNameList[19]   = ems2movme.mov-tit.vl-desp-banc-me
     _FldNameList[20]   = ems2movme.mov-tit.vl-abatimen
     _FldNameList[21]   = ems2movme.mov-tit.vl-abatimen-me
     _FldNameList[22]   = ems2movme.mov-tit.dt-vencimen
     _FldNameList[23]   = ems2movme.mov-tit.esp-antecip
     _FldNameList[24]   = ems2movme.mov-tit.serie-antecip
     _FldNameList[25]   = ems2movme.mov-tit.doc-antecip
     _FldNameList[26]   = ems2movme.mov-tit.parc-antecip
     _FldNameList[27]   = ems2movme.mov-tit.vl-antecip
     _FldNameList[28]   = ems2movme.mov-tit.vl-antecip-me
     _FldNameList[29]   = ems2movme.mov-tit.referencia
     _FldNameList[30]   > "_<CALC>"
"fn-dias-atraso(i-dias-atraso) @ i-dias-atraso" ? "->>>9" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-table
&Scoped-define SELF-NAME br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON MOUSE-SELECT-DBLCLICK OF br-table IN FRAME F-Main
DO:
    RUN New-State('DblClick':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON ROW-ENTRY OF br-table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON ROW-LEAVE OF br-table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON START-SEARCH OF br-table IN FRAME F-Main
DO:
    IF l-venct-matriz THEN DO:
        &IF int(entry(1,proversion,".")) >= 9 &THEN
            DEF VAR hTemp1 AS WIDGET-HANDLE NO-UNDO.
            DEF VAR hTemp2 AS WIDGET-HANDLE NO-UNDO.
            DEFINE VARIABLE c-where AS CHAR      NO-UNDO.
            DEFINE VARIABLE cField  AS CHARACTER NO-UNDO.
            DEFINE VARIABLE hHandle AS HANDLE    NO-UNDO.
            ASSIGN hTemp1 = br-table:CURRENT-COLUMN
                 hTemp2 = br-table:QUERY.

            IF cName = hTemp1:NAME THEN
              ASSIGN lDesc = NOT lDesc.
            ELSE
              ASSIGN lDesc = YES.

            IF hTemp1:NAME = "c-modalidade" THEN
                ASSIGN c-ordemcampo = "modalidade".
            ELSE 
                 IF hTemp1:NAME = "i-dias-atraso" THEN
                      ASSIGN c-ordemcampo = "dt-baixa - dt-vencimen" .
                 ELSE
                    ASSIGN c-ordemcampo = hTemp1:NAME.
                
            ASSIGN hHandle = {&browse-name}:CURRENT-COLUMN
                   cField = hHandle:NAME
                   c-where = " mov-tit.cod-emitente    = " + STRING(emitente.cod-emitente) + 
                             " and  mov-tit.ep-codigo  = " + STRING(i-ep-codigo-usuario) +
                             " and  (mov-tit.transacao = " + STRING(2) +
                             " or mov-tit.transacao    = " + STRING(3) + ")" +
                             " and  mov-tit.cod-esp  >= '" + STRING(c-cod-esp-inicial) + "'" +
                             " and  mov-tit.cod-esp  <= '" + STRING(c-cod-esp-final) + "'" +
                             " and  mov-tit.dt-trans  >= " + STRING(d-dt-trans-inicial) +
                             " and  mov-tit.dt-trans  <= " + STRING(d-dt-trans-final) +
                             " and  mov-tit.tipo      <> " + STRING(7). 

            IF lDesc THEN DO:
              hTemp2:QUERY-PREPARE("FOR EACH mov-tit no-lock WHERE " + c-where +
                                   " OUTER-JOIN by " + c-ordemcampo).
            END.
            ELSE DO:
              hTemp2:QUERY-PREPARE("FOR EACH mov-tit no-lock WHERE " + c-where +
                                   " OUTER-JOIN by " + c-ordemcampo + " desc").
            END.
            ASSIGN cName = hTemp1:NAME.
            hTemp2:QUERY-OPEN().        
        &ENDIF
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON VALUE-CHANGED OF br-table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-filtro B-table-Win
ON CHOOSE OF bt-filtro IN FRAME F-Main /* Filtro */
DO:

    assign l-atualiza = input frame {&FRAME-NAME} tg-atualiza.

    run crp/cr0716b.w (input-output c-cod-esp-inicial,
                       input-output c-cod-esp-final,
                       input-output d-dt-trans-inicial,
                       input-output d-dt-trans-final,
                       input-output l-atualiza).

    assign tg-atualiza:screen-value in frame {&FRAME-NAME} = string(l-atualiza).

    apply 'value-changed':U to tg-atualiza in frame {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BT-IMP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BT-IMP B-table-Win
ON CHOOSE OF BT-IMP IN FRAME F-Main /* Imprimir */
DO:
    DEF VAR l-cont AS INTEGER INITIAL 0 NO-UNDO. 
    FOR EACH tt-liq EXCLUSIVE-LOCK:
        DELETE tt-liq.
    END.
    FOR EACH b-mov-tit WHERE b-mov-tit.cod-emitente = emitente.cod-emitente
      AND b-mov-tit.ep-codigo = i-ep-codigo-usuario
      AND l-atualiza = yes
      AND ( b-mov-tit.transacao = 2 or b-mov-tit.transacao = 3)
      AND b-mov-tit.cod-esp  >= c-cod-esp-inicial
      AND b-mov-tit.cod-esp  <= c-cod-esp-final
      AND b-mov-tit.dt-trans >= d-dt-trans-inicial
      AND b-mov-tit.dt-trans <= d-dt-trans-final
      AND b-mov-tit.tipo     <> 7 NO-LOCK
        BY b-mov-tit.cod-estabel
            BY b-mov-tit.cod-esp
                BY b-mov-tit.serie
                    BY b-mov-tit.nr-docto
                        BY b-mov-tit.parcela:

        ASSIGN l-cont = l-cont + 1.

        CREATE tt-liq.                
        ASSIGN tt-liq.ep-codigo       = b-mov-tit.ep-codigo
               tt-liq.cod-emitente    = emitente.cod-emitente
               tt-liq.nome-abrev      = emitente.nome-abrev
               tt-liq.cod-estabel     = b-mov-tit.cod-estabel 
               tt-liq.mo-codigo       = b-mov-tit.mo-codigo 
               tt-liq.cod-esp         = b-mov-tit.cod-esp
               tt-liq.serie           = b-mov-tit.serie
               tt-liq.nr-docto        = b-mov-tit.nr-docto
               tt-liq.parcela         = b-mov-tit.parcela
               tt-liq.cod-portador    = b-mov-tit.cod-portador
               tt-liq.c-modalidade    = c-modalidade
               tt-liq.mo-codigo       = b-mov-tit.mo-codigo
               tt-liq.cotacao-dia     = b-mov-tit.cotacao-dia
               tt-liq.vl-baixa        = b-mov-tit.vl-baixa
               tt-liq.vl-baixa-me     = b-mov-tit.vl-baixa-me
               tt-liq.dt-vencimen     = b-mov-tit.dt-vencimen
               tt-liq.dt-credito      = b-mov-tit.dt-credito 
               tt-liq.dt-baixa        = b-mov-tit.dt-baixa
               tt-liq.esp-antecip     = b-mov-tit.esp-antecip
               tt-liq.serie-antecip   = b-mov-tit.serie-antecip
               tt-liq.doc-antecip     = b-mov-tit.doc-antecip
               tt-liq.parc-antecip    = b-mov-tit.parc-antecip
               tt-liq.vl-antecip      = b-mov-tit.vl-antecip
               tt-liq.vl-antecip-me   = b-mov-tit.vl-antecip-me
               tt-liq.vl-desconto     = b-mov-tit.vl-desconto
               tt-liq.vl-desconto-me  = b-mov-tit.vl-desconto-me
               tt-liq.vl-juros-rec    = b-mov-tit.vl-juros-rec 
               tt-liq.vl-juros-rec-me = b-mov-tit.vl-juros-rec-me
               tt-liq.vl-desp-banc-   = b-mov-tit.vl-desp-banc
               tt-liq.vl-desp-banc-me = b-mov-tit.vl-desp-banc-me
               tt-liq.vl-abatimen     = b-mov-tit.vl-abatimen 
               tt-liq.vl-abatimen-me  = b-mov-tit.vl-abatimen-me
               tt-liq.i-dias-atraso   = b-mov-tit.dt-baixa - b-mov-tit.dt-vencimen.
    END.
    RUN crp/cr0706d.w (INPUT TABLE tt-estat,
                       INPUT TABLE tt-liq,
                       INPUT TABLE tt-hist,
                       INPUT TABLE tt-doc,
                       INPUT TABLE tt-documento,        
                       INPUT r-emitente, 
                       INPUT fi-total-titulos,  
                       INPUT fi-total-matriz,   
                       INPUT fi-saldo-anterior,
                       INPUT 2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-atualiza
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-atualiza B-table-Win
ON VALUE-CHANGED OF tg-atualiza IN FRAME F-Main
DO:
      if  input frame {&FRAME-NAME} tg-atualiza = yes then do:
          assign l-atualiza = yes.
      end.    
      else do: 
          assign l-atualiza = no.
      end.

      if  input frame {&FRAME-NAME} tg-atualiza = yes then do:
          RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .
          run pi-totaliza.
      end.
/*      else do: 
 *           assign fi-total-baixa:screen-value in frame {&FRAME-NAME} = string(0).
 *       end.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

{utp/ut-liter.i Modalidade * R}.
assign c-modalidade:label in browse {&browse-name} = return-value.

{utp/ut-field.i ems2cadme mov-tit mo-codigo 1}.
assign mov-tit.mo-codigo:label in browse {&browse-name} = return-value.

{utp/ut-liter.i Vl_Liquida‡Æo * R}.
assign mov-tit.vl-baixa:label in browse {&browse-name} = return-value.

{utp/ut-liter.i Vl_Liquida‡Æo_ME * R}.
assign mov-tit.vl-baixa-me:label in browse {&browse-name} = return-value.

{utp/ut-field.i ems2cadme mov-tit vl-antecip 1}
assign mov-tit.vl-antecip:label in browse br-table = return-value.

{utp/ut-liter.i Vl_Antecip_ME}
assign mov-tit.vl-antecip-me:label in browse br-table = return-value.

{utp/ut-liter.i Vl_Abatimento * R}.
assign mov-tit.vl-abatimen:label in browse {&browse-name} = return-value.

{utp/ut-liter.i Vl_Abatimento_ME * R}.
assign mov-tit.vl-abatimen-me:label in browse {&browse-name} = return-value.

{utp/ut-liter.i Vl_Desconto * R}.
assign mov-tit.vl-desconto:label in browse {&browse-name} = return-value.

{utp/ut-liter.i Vl_Desconto_ME * R}.
assign mov-tit.vl-desconto-me:label in browse {&browse-name} = return-value.

{utp/ut-liter.i Vl_Desp_Banc * R}.
assign mov-tit.vl-desp-banc:label in browse {&browse-name} = return-value.

{utp/ut-liter.i Vl_Desp_Banc_ME * R}.
assign mov-tit.vl-desp-banc-me:label in browse {&browse-name} = return-value.

{utp/ut-liter.i Vl_Juros * R}.
assign mov-tit.vl-juros-rec:label in browse {&browse-name} = return-value.

{utp/ut-liter.i Vl_Juros_ME * R}.
assign mov-tit.vl-juros-rec-me:label in browse {&browse-name} = return-value.

{utp/ut-field.i ems2cadme mov-tit dt-vencimen 1}.
assign mov-tit.dt-vencimen:label in browse {&browse-name} = return-value.

{utp/ut-liter.i Atraso * R}
assign i-dias-atraso:label in browse br-table = return-value.

{utp/ut-liter.i S‚rie_Antecip * R}
assign mov-tit.serie-antecip:label in browse br-table = return-value.

{utp/ut-liter.i Total_Liquida‡äes * L}
assign c-sigla-1:label in frame {&FRAME-NAME} = return-value.

{utp/ut-liter.i Atualiza * R}
assign tg-atualiza:label in frame {&FRAME-NAME} = return-value.

{utp/ut-field.i ems2cadme mov-tit vl-desconto 1}
assign mov-tit.vl-desconto:label in browse br-table = return-value.

&if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
    DEFINE VARIABLE cAuxTraducao001 AS CHARACTER NO-UNDO.
    ASSIGN cAuxTraducao001 = {adinc/i03ad209.i 03}.
    RUN utp/ut-list.p (INPUT-OUTPUT cAuxTraducao001).
    ASSIGN  c-lista-modalidade = cAuxTraducao001.
&else
    ASSIGN c-lista-modalidade = {adinc/i03ad209.i 03}.
&endif

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

ASSIGN l-venct-matriz = NO.
&IF DEFINED(BF_FIN_VENCT_MATRIZ) &THEN
    ASSIGN l-venct-matriz = YES.
&else
    IF CAN-FIND(funcao
                WHERE funcao.cd-funcao = 'spp-venct-matriz' 
                AND   funcao.ativo = yes) THEN
        ASSIGN l-venct-matriz = YES.
&ENDIF
    
ASSIGN l-imprime-cli = NO.
&IF DEFINED(bf_fin_imprime_cli) &THEN
    ASSIGN l-imprime-cli = YES.
&else
    IF CAN-FIND(funcao WHERE funcao.cd-funcao = 'spp-imprime-cli' AND funcao.ativo = YES) THEN
        ASSIGN l-imprime-cli = YES.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "emitente"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "emitente"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields B-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  find first moeda no-lock
       where moeda.mo-codigo = 0 no-error.

  if  avail moeda then do:
      assign c-sigla-1 = moeda.sigla.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  IF l-imprime-cli = YES  THEN
      ASSIGN bt-imp:VISIBLE IN FRAME {&FRAME-NAME} = YES.
  ELSE 
      ASSIGN bt-imp:VISIBLE IN FRAME {&FRAME-NAME} = NO.

      find first param-cr no-lock
       where param-cr.ep-codigo = i-ep-codigo-usuario no-error.

  find first moeda no-lock
       where moeda.mo-codigo = 0 no-error.

  if  avail moeda then do:
      assign c-mo-codigo = moeda.sigla.
  end.

  IF l-venct-matriz THEN DO:
    &IF int(entry(1,proversion,".")) >= 9 &THEN
        ASSIGN br-table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.
    &ENDIF
  END.

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  assign tg-atualiza:screen-value    in frame {&FRAME-NAME} = string(l-atualiza).

  if  input frame {&FRAME-NAME} tg-atualiza = yes then
      run pi-totaliza.
  else
      assign fi-total-baixa:screen-value in frame {&FRAME-NAME} = string(0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view B-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  apply 'value-changed':U to {&browse-name} in frame {&frame-name}.

END PROCEDURE.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartBrowser, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-add-browse B-table-Win 
PROCEDURE pi-add-browse :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/

  def input parameter rw-row-1 as rowid no-undo.
  def input parameter rw-row-2 as rowid no-undo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-totaliza B-table-Win 
PROCEDURE pi-totaliza :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* calculo do total do browse */
  assign fi-total-baixa = 0.
  assign br-table:REFRESHABLE IN FRAME {&frame-name}= no.
  GET FIRST br-table.

  DO WHILE AVAIL mov-tit: /* tabela da query */
     if mov-tit.tipo <> 7 then do:
         assign fi-total-baixa = fi-total-baixa + mov-tit.vl-baixa.
     end.
     GET NEXT br-table.
  END.

  ASSIGN br-table:REFRESHABLE IN FRAME {&frame-name} = yes.
  /* Fim calculo do total do Browse */

  display fi-total-baixa with frame {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-dias-atraso B-table-Win 
FUNCTION fn-dias-atraso RETURNS INTEGER
  ( INPUT i-dias-atraso AS INTEGER ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF AVAIL mov-tit THEN
      ASSIGN i-dias-atraso =   mov-tit.dt-baixa - mov-tit.dt-vencimen.

  RETURN i-dias-atraso.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
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
def buffer cotacao for ems2cadme.cotacao.
def buffer moeda   for ems2cadme.moeda.

{include/i-prgvrs.i B55AD264 2.00.00.036}  /*** 010036 ***/

&IF "{&EMSFND_VERSION}" >= "1.00"
&THEN
{include/i-license-manager.i B55AD264 MUT}
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

/* {utp/ut-glob.i}  */

/* Definicao TTs e Variaveis - Validacao Decimais - Chile */
{cdp/cd1234.i}

/* Miniflexibiliza‡Æo */  
  {include/i_dbvers.i}
  {cdp/cdcfgfin.i}

/* Vari veis */
&if "{&mgadm_version}" >= "2.02" &then
  {crp/cr0501d.i4}
&endif
  

/* Local Variable Definitions ---                                       */
def var c-saldo              as char                                no-undo.
def var c-saldo-me           as char                                no-undo.
def var c-lista-modalidade   as char format "x(15)"                 no-undo.
def var c-cod-esp-inicial    like titulo.cod-esp init "AA"          no-undo.
def var c-cod-esp-final      like titulo.cod-esp init "ZZ"          no-undo.
def var c-modalidade         as char format "x(20)"                 no-undo.
def var c-mo-codigo          as char format "x(03)"                 no-undo.
def var c-totaliza           as char                                no-undo.
def var de-cotacao           like titulo.cotacao-dia                no-undo.
def var de-juros-calc        like titulo.vl-saldo                   no-undo.
def var de-juros             as dec                                 no-undo.
def var de-saldo-tit         as dec                                 no-undo.
def var de-saldo-tit-me      as dec                                 no-undo.
def var de-calc-vl-saldo     like titulo.vl-saldo                   no-undo.
def var de-calc-vl-baixa     like mov-tit.vl-baixa                  no-undo.
def var de-calc-vl-antecip   like mov-tit.vl-antecip                no-undo.
DEF VAR l-imprime-cli        AS LOGICAL                             NO-UNDO.

def var da-data-sel          as date init today                     no-undo.
def var da-dat               as date                                no-undo.

def var r-registro           as rowid                               no-undo.
def var rg-titulo            as rowid                               no-undo.

def var d-dt-emissao-inicial like titulo.dt-emissao  init "01/01/1996"  no-undo.
def var d-dt-emissao-final   like titulo.dt-emissao  init "12/31/9999"  no-undo.
def var d-dt-vencto-inicial  like titulo.dt-vencimen INIT TODAY         no-undo.
def var d-dt-vencto-final    like titulo.dt-vencimen INIT TODAY         no-undo.

def var i-moeda              like titulo.mo-codigo   init 0         no-undo.
def var i-cod-rep-inicial    like titulo.cod-rep                    no-undo.
def var i-cod-rep-final      like titulo.cod-rep     init 99999     no-undo.
def var i-cod-por-inicial    like titulo.cod-por                    no-undo.
def var i-cod-por-final      like titulo.cod-por     init 99999     no-undo.
def var da-data-base         as date format "99/99/9999" init today no-undo.
DEF VAR da-data-atraso       AS DATE FORMAT "99/99/9999" init today NO-UNDO.
DEF VAR da-data-antiga       AS DATE FORMAT "99/99/9999"            NO-UNDO.

def var i-op-juros           as int     init 1                      no-undo.
def var i-titulo             as int     init 2                      no-undo.
def var i-valor              as int     init 2                      no-undo.
def var i-contador           as int                                 no-undo.
def var i-atraso             as int     format 99999                no-undo.
def var i-dias-vencimento    as int     format ">>9" init 0         no-undo.

def var l-juros              as logical init no                     no-undo.
def var l-vencidos           as logical init no                     no-undo.
def var l-cancela            as logical init no                     no-undo.
def var l-a-vencer           as logical init no                     no-undo.
def var l-atualiza           as logical init no                     no-undo.
def var l-ok                 as logical                             no-undo.

def var h-acomp              as handle                              no-undo.

def var de-total-mat         as dec                                 no-undo.
def var de-saldo-anterior    like titulo.vl-saldo                   no-undo.
def var de-total-matriz      like titulo.vl-saldo                   no-undo.
def var de-indice            like cotacao.cota-mensal initial 1.
def var de-total-orig        like titulo.vl-original                no-undo.     

def VAR r-emitente as rowid no-undo.
def buffer b-emitente for emitente.

DEF VAR l-venct-matriz AS LOGICAL INITIAL NO NO-UNDO.
DEF VAR cName          AS CHAR NO-UNDO.
DEF VAR lDesc          AS LOGICAL NO-UNDO.
DEF VAR c-ordemcampo   AS CHAR    NO-UNDO.
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

def temp-table tt-documento no-undo like titulo
     field row-documento  as rowid
     field c-modalidade   like titulo.modalidade
     field c-mo-codigo    as char format "x(03)"
     field de-saldo       like titulo.vl-saldo
     field de-saldo-me    like titulo.vl-saldo-me
     field i-dias         as int init 0
     field de-juros       as dec 
     field de-total-juros as dec.

define temp-table tt-hist no-undo
     FIELD cod-emitente LIKE titulo.cod-emitente
     FIELD nome-abrev   LIKE titulo.nome-abrev
     FIELD dt-his-emit  LIKE his-emit.dt-his-emit
     FIELD horario      LIKE his-emit.horario
     FIELD c-historico  AS CHAR.



def new global shared var gr-titulo     as rowid   no-undo.
def var dilo as char.

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
&Scoped-define INTERNAL-TABLES tt-documento

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table tt-documento.cod-estabel tt-documento.cod-esp tt-documento.serie tt-documento.nr-docto tt-documento.parcela tt-documento.cod-port entry(tt-documento.modalidade, c-lista-modalidade) @ c-modalidade tt-documento.mo-codigo tt-documento.de-saldo tt-documento.de-saldo-me tt-documento.de-juros tt-documento.de-total-juros tt-documento.vl-original tt-documento.vl-original-me tt-documento.dt-emissao tt-documento.dt-vencimen tt-documento.i-dias &if "{&mgadm_version}" >= "2.04" &then tt-documento.nr-proc-exp &endif   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table   
&Scoped-define SELF-NAME br-table
&Scoped-define OPEN-QUERY-br-table run pi-cria-tt-documento.  OPEN QUERY {&SELF-NAME} FOR EACH tt-documento by tt-documento.cod-estabel                                               by tt-documento.cod-esp                                               by tt-documento.serie                                               by tt-documento.nr-docto                                               by tt-documento.parcela.
&Scoped-define TABLES-IN-QUERY-br-table tt-documento
&Scoped-define FIRST-TABLE-IN-QUERY-br-table tt-documento


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-table RECT-11 RECT-3 bt-atualiza ~
bt-filtro bt-detalhar BUTTON-1 
&Scoped-Define DISPLAYED-OBJECTS c-sigla-3 fi-total-titulos c-sigla-4 ~
fi-total-original c-sigla-1 fi-total-matriz c-sigla-2 fi-saldo-anterior 

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


/* ***********************  Control Definitions  ********************** */


/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-bt-atualiza 
       MENU-ITEM m_Atualiza_Saldo_AnteriorAtua LABEL "Atualiza Saldo Anterior~\Atual".


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-atualiza 
     IMAGE-UP FILE "image/im-chck1.bmp":U
     LABEL "Atualiza Saldo Atual~\Anterior" 
     SIZE 7 BY 1.13.

DEFINE BUTTON bt-detalhar 
     LABEL "&Detalhar" 
     SIZE 12.43 BY 1.

DEFINE BUTTON bt-filtro 
     LABEL "&Filtro" 
     SIZE 12.43 BY 1.

DEFINE BUTTON BUTTON-1 
     LABEL "&Imprimir" 
     SIZE 12.43 BY 1.

DEFINE VARIABLE c-sigla-1 AS CHARACTER FORMAT "X(5)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-sigla-2 AS CHARACTER FORMAT "X(5)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-sigla-3 AS CHARACTER FORMAT "X(5)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-sigla-4 AS CHARACTER FORMAT "X(256)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-saldo-anterior AS DECIMAL FORMAT "->>>>>,>>>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-total-matriz AS DECIMAL FORMAT "->>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-total-original AS DECIMAL FORMAT "->>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-total-titulos AS DECIMAL FORMAT "->>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.86 BY 4.25.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40.29 BY 4.25.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      tt-documento SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _FREEFORM
  QUERY br-table NO-LOCK DISPLAY
      tt-documento.cod-estabel    format "x(4)"
      tt-documento.cod-esp
      tt-documento.serie          format "x(6)"
      tt-documento.nr-docto
      tt-documento.parcela        format "x(3)"
      tt-documento.cod-port       format ">>>>>9"
      entry(tt-documento.modalidade, c-lista-modalidade) @ c-modalidade
      tt-documento.mo-codigo  
      tt-documento.de-saldo       format "->>>,>>>,>>9.99"
      tt-documento.de-saldo-me    format "->>>,>>>,>>9.99"
      tt-documento.de-juros       format "->>>,>>9.99"
      tt-documento.de-total-juros format "->>>,>>>,>>9.99"
      tt-documento.vl-original    format "->>>,>>>,>>9.99"
      tt-documento.vl-original-me format "->>>,>>>,>>9.99"
      tt-documento.dt-emissao
      tt-documento.dt-vencimen
      tt-documento.i-dias         format "->>>>>9"
      &if "{&mgadm_version}" >= "2.04" &then                       
         tt-documento.nr-proc-exp
      &endif
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 87.43 BY 6.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-table AT ROW 1 COL 1
     c-sigla-3 AT ROW 7.96 COL 63 COLON-ALIGNED
     fi-total-titulos AT ROW 7.96 COL 68 COLON-ALIGNED NO-LABEL
     bt-atualiza AT ROW 8.5 COL 4 HELP
          "Atualiza Saldo Atual~\Anterior"
     c-sigla-4 AT ROW 8.96 COL 63 COLON-ALIGNED
     fi-total-original AT ROW 8.96 COL 68 COLON-ALIGNED NO-LABEL
     c-sigla-1 AT ROW 9.96 COL 63 COLON-ALIGNED
     fi-total-matriz AT ROW 9.96 COL 68 COLON-ALIGNED NO-LABEL
     bt-filtro AT ROW 10.25 COL 4
     bt-detalhar AT ROW 10.25 COL 18
     BUTTON-1 AT ROW 10.25 COL 32
     c-sigla-2 AT ROW 10.96 COL 63 COLON-ALIGNED
     fi-saldo-anterior AT ROW 10.96 COL 68 COLON-ALIGNED NO-LABEL
     RECT-11 AT ROW 7.79 COL 1
     RECT-3 AT ROW 7.79 COL 48.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: mgadm.emitente
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
         HEIGHT             = 11.04
         WIDTH              = 87.72.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br-table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       bt-atualiza:POPUP-MENU IN FRAME F-Main       = MENU POPUP-MENU-bt-atualiza:HANDLE.

/* SETTINGS FOR FILL-IN c-sigla-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-sigla-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-sigla-3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-sigla-4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-saldo-anterior IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-total-matriz IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-total-original IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-total-titulos IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _START_FREEFORM
run pi-cria-tt-documento.

OPEN QUERY {&SELF-NAME} FOR EACH tt-documento by tt-documento.cod-estabel
                                              by tt-documento.cod-esp
                                              by tt-documento.serie
                                              by tt-documento.nr-docto
                                              by tt-documento.parcela.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE"
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
    
    apply 'choose' to bt-detalhar.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON START-SEARCH OF br-table IN FRAME F-Main
DO:
    IF l-venct-matriz THEN DO:
        &IF INTEGER(ENTRY(1,PROVERSION,'.')) >= 9 &THEN
            DEF VAR hTemp1 AS WIDGET-HANDLE NO-UNDO.
            DEF VAR hTemp2 AS WIDGET-HANDLE NO-UNDO.
            ASSIGN hTemp1 = br-table:CURRENT-COLUMN
                 hTemp2 = br-table:QUERY.

            IF cName = hTemp1:NAME THEN
              ASSIGN lDesc = NOT lDesc.
            ELSE
              ASSIGN lDesc = YES.

            IF hTemp1:NAME = "c-modalidade" THEN
                ASSIGN c-ordemcampo = "modalidade".
            ELSE 
                ASSIGN c-ordemcampo = hTemp1:NAME.

            IF lDesc THEN DO:
              hTemp2:QUERY-PREPARE("For each tt-documento " +
                                   " OUTER-JOIN by " + c-ordemcampo).
            END.
            ELSE DO:
              hTemp2:QUERY-PREPARE("For each tt-documento " +
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


&Scoped-define SELF-NAME bt-atualiza
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-atualiza B-table-Win
ON CHOOSE OF bt-atualiza IN FRAME F-Main /* Atualiza Saldo Atual\Anterior */
DO:
  RUN pi-totaliza.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-detalhar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-detalhar B-table-Win
ON CHOOSE OF bt-detalhar IN FRAME F-Main /* Detalhar */
DO:
   if  avail tt-documento then do:
       assign gr-titulo = tt-documento.row-documento.
       run crp/cr0709.w.
   end.      
   else do:
       run utp/ut-msgs.p (input "show",
                          input 1880,
                          input "").            
   end.
   assign gr-titulo = rowid(titulo).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-filtro B-table-Win
ON CHOOSE OF bt-filtro IN FRAME F-Main /* Filtro */
DO:
           
  run crp/cr0706aa.w (input-output i-moeda,
                     input-output da-data-sel,
                     input-output c-cod-esp-inicial,
                     input-output c-cod-esp-final,
                     input-output d-dt-emissao-inicial,
                     input-output d-dt-emissao-final,
                     input-output d-dt-vencto-inicial,
                     input-output d-dt-vencto-final,
                     input-output i-cod-rep-inicial,
                     input-output i-cod-rep-final,
                     input-output i-cod-por-inicial,
                     input-output i-cod-por-final,
                     input-output i-titulo,
                     input-output i-valor,
                     input-output l-vencidos,
                     input-output l-a-vencer,
                     input-output i-dias-vencimento,
                     input-output l-juros,
                     input-output i-op-juros,
                     input-output de-juros,
                     input-output l-cancela,
                     input-output da-data-base,
                     INPUT-OUTPUT da-data-atraso). 

&if "{&mgadm_version}" >= "2.02" &then
  {crp/cr0501d.i3 "da-data-base"
                  "de-perc-juro"
                  "i-car-juro"
                  "de-valor-min"
                  "l-gera-ad"
                  "de-perc-multa"
                  "i-car-multa"
                  "i-tp-juros"
                  "i-mo-vl-min"}
&endif

  if  l-vencidos then
      assign da-dat = da-data-atraso - i-dias-vencimento.
                
  if  l-a-vencer then
      assign da-dat = da-data-atraso + i-dias-vencimento.               
  
  if l-cancela = no then do:
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ).
  end.                  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 B-table-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Imprimir */
DO:
  run crp/cr0706d.w (INPUT TABLE tt-estat,
                     INPUT TABLE tt-liq,
                     INPUT TABLE tt-hist,
                     INPUT TABLE tt-doc,
                     INPUT TABLE tt-documento,
                     INPUT r-registro, 
                     INPUT fi-total-titulos,  
                     INPUT fi-total-matriz,   
                     INPUT fi-saldo-anterior,
                     INPUT 5).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */ 
    /*{utp/ut-liter.i Data_Conversäes_Moeda_Estrangeira * R}
     * assign da-data:label in frame {&FRAME-NAME} = trim(return-value).*/
    
    {utp/ut-field.i mgadm titulo Modalidade 1}.
    assign c-modalidade:label in browse {&browse-name} = return-value.
    
    {utp/ut-field.i mgadm titulo mo-codigo 1}.
    assign tt-documento.mo-codigo:label in browse {&browse-name} = return-value.
    
    {utp/ut-liter.i Dias * R}.
    assign tt-documento.i-dias:label in browse {&browse-name} = return-value.
    
    {utp/ut-liter.i Valor_Saldo_ME_ * R}.
    assign tt-documento.de-saldo-me:label in browse {&browse-name} = return-value.

    {utp/ut-liter.i Vl_Original_ME * R}.
    assign tt-documento.vl-original-me:label in browse {&browse-name} = return-value.

    {utp/ut-field.i mgadm titulo dt-emissao 1}.
    assign tt-documento.dt-emissao:label in browse {&browse-name} = return-value.
    
    {utp/ut-field.i mgadm titulo dt-vencimen 1}.
    assign tt-documento.dt-vencimen:label in browse {&browse-name} = return-value.
    
    {utp/ut-liter.i Total_Saldo_Aberto * L}.
    assign c-sigla-1:label in frame {&frame-name} = return-value.
    
    {utp/ut-liter.i Saldo_Anterior_Cliente * L}.
    assign c-sigla-2:label in frame {&frame-name} = return-value.
    
    {utp/ut-liter.i Total_Saldo_Tela * L}.
    assign c-sigla-3:label in frame {&frame-name} = return-value.
    
    {utp/ut-liter.i Total_Original_Tela * L}.
    assign c-sigla-4:label in frame {&frame-name} = return-value.

    {utp/ut-liter.i Valor_Saldo_ * L}.
    assign tt-documento.de-saldo:label in browse br-table = return-value.
     
    {utp/ut-liter.i Valor_Juros * L}.
    assign tt-documento.de-juros:label in browse {&browse-name} = return-value.
      
    {utp/ut-liter.i Total_Saldo * L}.
    assign tt-documento.de-total-juros:label in browse {&browse-name} = return-value.
    
    assign c-lista-modalidade = {adinc/i03ad209.i 03}.

    ASSIGN l-venct-matriz = NO.
    &IF DEFINED(BF_FIN_VENCT_MATRIZ) &THEN
        ASSIGN l-venct-matriz = YES.
    &else
        IF CAN-FIND(funcao
                    WHERE funcao.cd-funcao = 'spp-venct-matriz' 
                    AND   funcao.ativo = yes) THEN
            ASSIGN l-venct-matriz = YES.
    &ENDIF
    find first param-cr no-lock
         where param-cr.ep-codigo = i-ep-codigo-usuario no-error.
         
    &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
        RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
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
    def buffer moeda   for ems2cadme.moeda.

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  find first moeda no-lock
       where moeda.mo-codigo = i-moeda no-error.
  
  if  avail moeda then do:
      assign c-sigla-1:screen-value in frame {&FRAME-NAME} = moeda.sigla
             c-sigla-2:screen-value in frame {&FRAME-NAME} = moeda.sigla
             c-sigla-3:screen-value in frame {&FRAME-NAME} = moeda.sigla
             c-sigla-4:screen-value in frame {&FRAME-NAME} = moeda.sigla.
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


  /* Code placed here will execute PRIOR to standard behavior. */

  find first param-cr    
       where param-cr.ep-codigo = i-ep-codigo-usuario no-lock no-error.

  IF l-venct-matriz THEN DO:
    &IF INTEGER(ENTRY(1,PROVERSION,'.')) >= 9 &THEN
        ASSIGN br-table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.
    &ENDIF
  END.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calcula-valor B-table-Win 
PROCEDURE pi-calcula-valor :
/*------------------------------------------------------------------------------
   Purpose:     
   Parameters:  <none>
   Notes:       
------------------------------------------------------------------------------*/
      
    assign de-juros-calc   = 0
           i-atraso        = 0.
   
    assign de-calc-vl-saldo   = titulo.vl-saldo.

    assign de-saldo-tit    = de-calc-vl-saldo
           de-saldo-tit-me = titulo.vl-saldo-me.

    if  titulo.tipo = 1 then do:
        if  l-juros = yes then do:
            if  titulo.dt-vencimen < da-data-base then do:
                assign i-atraso = da-data-base - titulo.dt-vencimen.
                                     
                if  i-op-juros = 1 then
                    assign de-juros-calc     = fn_ajust_dec((de-calc-vl-saldo
                                             * (i-atraso * titulo.perc-juros) / 100),0).
                                             
                if  i-op-juros = 2 and avail param-cr then 
                    &if "{&mgadm_version}" < "2.02" &then
                        if  i-op-juros = 2 then 
                            assign de-juros-calc = fn_ajust_dec((de-calc-vl-saldo
                                                 * (i-atraso * (param-cr.perc-juros / 30)) / 100),0).
                    &endif                         
                    &if "{&mgadm_version}" >= "2.02" &then                       
                    if  i-op-juros = 2 then 
                        assign de-juros-calc = fn_ajust_dec((de-calc-vl-saldo
                                             * (i-atraso * (de-perc-juro / 30)) / 100),0).
                    &endif                         
                    
                if i-op-juros = 3 then
                   assign de-juros-calc = fn_ajust_dec((de-calc-vl-saldo
                                        * (i-atraso * (de-juros / 30)) / 100),0).
            end.
        end.                                   
    end.
             
    /*--- Saldo dos t¡tulos ---*/
    /*{crp/cr0706.i2}  Deve mostrar o saldo com a data de hoje*/
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-tt-documento B-table-Win 
PROCEDURE pi-cria-tt-documento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

for each tt-documento:     
    delete tt-documento.
end.     

assign r-registro        = rowid(emitente)
       fi-total-titulos  = 0
       fi-total-original = 0
       de-juros-calc     = 0
       i-atraso          = 0
       de-cotacao        = 1
       de-saldo-tit      = 0.

assign fi-total-matriz   = 0
       fi-saldo-anterior = 0.

run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input "Totalizando Documentos do Cliente").

FOR EACH titulo use-index chave-cli
    WHERE titulo.ep-codigo     = i-ep-codigo-usuario
    AND   titulo.cod-emitente  = emitente.cod-emitente
    AND   titulo.dt-vencimen  >= d-dt-vencto-inicial
    AND   titulo.dt-vencimen  <= d-dt-vencto-final
    AND   titulo.cod-esp      >= c-cod-esp-inicial
    AND   titulo.cod-esp      <= c-cod-esp-final
    AND   titulo.dt-emissao   >= d-dt-emissao-inicial
    AND   titulo.dt-emissao   <= d-dt-emissao-final
    AND   titulo.cod-rep      >= i-cod-rep-inicial
    AND   titulo.cod-rep      <= i-cod-rep-final
    and   titulo.cod-por      >= i-cod-por-inicial
    and   titulo.cod-por      <= i-cod-por-final
    /*AND   titulo.tipo         <> 7*/
    NO-LOCK:

    assign de-juros-calc   = 0
           i-atraso        = 0
           de-saldo-tit    = 0
           de-saldo-tit-me = 0.
    
    IF (titulo.dt-vencimen <= (da-data-atraso - i-dias-vencimento) AND
        l-vencidos) OR (titulo.dt-vencimen >= (da-data-atraso - i-dias-vencimento) AND
        l-a-vencer) OR (l-vencidos = FALSE AND l-a-vencer = FALSE) THEN
    DO:        
    
       FIND tt-documento 
           WHERE tt-documento.ep-codigo = titulo.ep-codigo
           AND   tt-documento.cod-est   = titulo.cod-est 
           AND   tt-documento.cod-esp   = titulo.cod-esp
           AND   tt-documento.serie     = titulo.serie
           AND   tt-documento.nr-docto  = titulo.nr-docto
           AND   tt-documento.parcela   = titulo.parcela no-error.

       if  not avail tt-documento then do:
           run pi-calcula-valor.  
        
           if  (de-saldo-tit <> 0 and i-titulo = 2)
           or  i-titulo = 1 then do:            
               ASSIGN da-data-antiga = da-data-base
                      da-data-base   = da-data-atraso.            
               {crp/cr0706.i6}       
               ASSIGN da-data-base = da-data-antiga.
          end.
       end.    
    END. 
END.

run pi-finalizar in h-acomp.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-saldo-anterior B-table-Win 
PROCEDURE pi-saldo-anterior :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /*----- Busca  Cota‡Æo ----*/
   {crp/cr0706.i4 "i-moeda"
                  "de-cotacao"
                  "da-data-sel"}            
   
   /*---- Calculo valores ----*/          
   
   {crp/cr0706.i3}

   /*---- Saldo  Anterior ----*/
                
   {crp/cr0706.i "d-dt-emissao-inicial"}
   
   assign fi-saldo-anterior = de-saldo-anterior. 
   
   /*assign fi-saldo-anterior:screen-value in frame {&frame-name} = string(de-saldo-anterior).*/
             
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

assign r-registro        = rowid(emitente)
       /*fi-total-titulos  = 0*/
       de-juros-calc     = 0
       i-atraso          = 0
       de-cotacao        = 1
       de-saldo-tit      = 0
       fi-total-matriz   = 0
       de-total-matriz   = 0
       de-saldo-anterior = 0
       fi-saldo-anterior = 0.

find first b-emitente 
    where rowid(b-emitente) = r-registro no-lock no-error.
    
if  avail emitente then do:
    
    /*Calcula saldo Atual e anterior*/
    run crp/cr0706f.p (input b-emitente.cod-emitente,
                       input c-cod-esp-inicial,
                       input c-cod-esp-final,
                       input i-cod-rep-inicial,
                       input i-cod-rep-final,
                       input i-cod-por-inicial,
                       input i-cod-por-final,
                       input d-dt-emissao-inicial,
                       input d-dt-emissao-final,
                       input da-data-base,
                       input l-juros,
                       input de-juros,
                       input i-op-juros,
                       input no,   /*l-web*/
                       input yes,  /*l-cliente*/
                       input-output de-saldo-anterior,
                       input-output de-total-matriz,
                       input-output de-juros-calc).

end.

assign fi-total-matriz   = de-total-matriz
       fi-saldo-anterior = de-saldo-anterior.                                

/*RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ). */
RUN local-display-fields IN THIS-PROCEDURE.

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


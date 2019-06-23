&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgadm            PROGRESS
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
{include/i-prgvrs.i B05AD134 2.00.00.004}  /*** 010004 ***/

&IF "{&EMSFND_VERSION}" >= "1.00"
&THEN
{include/i-license-manager.i B05AD134 MUT}
&ENDIF



/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def var c-historico   as char format "x(100)".
def var dt-inicial    as date no-undo initial &IF "{&ems_dbtype}":U = "MSS":U &THEN "01/01/1800":U &ELSE "01/01/0001":U &ENDIF.
def var dt-final      as date no-undo initial "12/31/9999".
DEF VAR l-imprime-cli AS LOGICAL NO-UNDO.

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


define temp-table tt-hist no-undo
       FIELD cod-emitente LIKE titulo.cod-emitente
       FIELD nome-abrev   LIKE titulo.nome-abrev
       FIELD dt-his-emit  LIKE his-emit.dt-his-emit
       FIELD horario      LIKE his-emit.horario
       FIELD c-historico  AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES emitente
&Scoped-define FIRST-EXTERNAL-TABLE emitente


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR emitente.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES his-emit

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table his-emit.dt-his-emit ~
his-emit.horario his-emit.historico @ c-historico 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH his-emit OF emitente WHERE ~{&KEY-PHRASE} ~
      AND dt-his-emit >= dt-inicial and ~
dt-his-emit <= dt-final   NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br_table his-emit
&Scoped-define FIRST-TABLE-IN-QUERY-br_table his-emit


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table bt-detalhar bt-filtro BT-IMP 

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
cod-emitente||y|mgadm.his-emit.cod-emitente
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "cod-emitente"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
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
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fc-historico B-table-Win 
FUNCTION fc-historico RETURNS CHARACTER
  ( c-hist-orig as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-detalhar 
     LABEL "&Detalhar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-filtro 
     LABEL "&Filtro" 
     SIZE 12 BY 1.

DEFINE BUTTON BT-IMP 
     LABEL "Imprimir" 
     SIZE 12 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      his-emit
    FIELDS(his-emit.dt-his-emit
      his-emit.horario
      his-emit.historico) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      his-emit.dt-his-emit FORMAT "99/99/9999":U
      his-emit.horario FORMAT "99:99":U
      his-emit.historico @ c-historico
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 87 BY 9.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 2
     bt-detalhar AT ROW 10.75 COL 2
     bt-filtro AT ROW 10.75 COL 13
     BT-IMP AT ROW 10.75 COL 31
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
         HEIGHT             = 10.96
         WIDTH              = 89.
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
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       bt-filtro:AUTO-RESIZE IN FRAME F-Main      = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "mgadm.his-emit OF mgadm.emitente"
     _Options          = "NO-LOCK INDEXED-REPOSITION KEY-PHRASE"
     _TblOptList       = "USED"
     _Where[1]         = "dt-his-emit >= dt-inicial and
dt-his-emit <= dt-final  "
     _FldNameList[1]   = mgadm.his-emit.dt-his-emit
     _FldNameList[2]   = mgadm.his-emit.horario
     _FldNameList[3]   > "_<CALC>"
"his-emit.historico @ c-historico" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON MOUSE-SELECT-DBLCLICK OF br_table IN FRAME F-Main
DO:
    apply "choose" to bt-detalhar in frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-detalhar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-detalhar B-table-Win
ON CHOOSE OF bt-detalhar IN FRAME F-Main /* Detalhar */
DO:
  if avail his-emit then 
     run crp/cr0706b.w(input rowid(his-emit)).


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-filtro B-table-Win
ON CHOOSE OF bt-filtro IN FRAME F-Main /* Filtro */
DO:

  run crp/cr0706a.w(input-output dt-inicial,
                    input-output dt-final).

 {&OPEN-QUERY-{&BROWSE-NAME}}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BT-IMP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BT-IMP B-table-Win
ON CHOOSE OF BT-IMP IN FRAME F-Main /* Imprimir */
DO:
    DEF VAR l-cont AS INTEGER INITIAL 0 NO-UNDO.

    FOR EACH tt-hist EXCLUSIVE-LOCK:
        DELETE tt-hist.
    END.

    FOR EACH his-emit OF emitente WHERE {&KEY-PHRASE}
      AND his-emit.dt-his-emit >= dt-inicial and
      his-emit.dt-his-emit <= dt-final   NO-LOCK. /*INDEXED-REPOSITION*/

        ASSIGN l-cont = l-cont + 1.

        CREATE tt-hist.                
        ASSIGN tt-hist.cod-emitente = emitente.cod-emitente
               tt-hist.nome-abrev   = emitente.nome-abrev
               tt-hist.dt-his-emit  = his-emit.dt-his-emit 
               tt-hist.horario      = his-emit.horario 
               tt-hist.c-historico  = his-emit.historico.
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
                       INPUT 3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

  /* Code placed here will execute PRIOR to standard behavior. */
  {utp/ut-field.i mgadm his-emit historico 2}
   assign c-historico:label in browse {&browse-name} = return-value.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "cod-emitente" "his-emit" "cod-emitente"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "emitente"}
  {src/adm/template/snd-list.i "his-emit"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fc-historico B-table-Win 
FUNCTION fc-historico RETURNS CHARACTER
  ( c-hist-orig as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

assign c-historico = c-hist-orig.

  RETURN c-historico.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


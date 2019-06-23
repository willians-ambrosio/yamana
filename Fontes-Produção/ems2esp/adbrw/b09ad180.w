&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
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
{include/i-prgvrs.i B09AD180 2.00.00.021}  /*** 010021 ***/

&IF "{&EMSFND_VERSION}" >= "1.00"
&THEN
{include/i-license-manager.i B09AD180 MUT}
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

def new global shared var i-ep-codigo-usuario  like ems2cadme.empresa.ep-codigo no-undo.
def new Global shared var l-implanta           as logical    init no.
def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.

def new global shared var c-aberto        as logical init no.
def new global shared var c-matriz        as logical init no.
def new global shared var l-saldo-aberto  as logical init no.
def new global shared var c-cod-est       like mov-ap.cod-est NO-UNDO.
def new global shared var c-ini-esp       like mov-ap.cod-esp init "AA".
def new global shared var c-fim-esp       like mov-ap.cod-esp init "ZZ".
def new global shared var dt-ini          like mov-ap.dt-transacao init &IF "{&ems_dbtype}":U = "MSS":U &THEN "01/01/1800":U &ELSE "01/01/0001":U &ENDIF.
def new global shared var dt-fim          like mov-ap.dt-transacao init today.
def new global shared var i-cod-moeda     as integer init 0.
def new global shared var l-estabel       as logical.
def new global shared var gr-mov-ap       as rowid no-undo.
def new global shared var i-port-ini      as integer no-undo.
def new global shared var i-port-fim      as integer no-undo.

def var wh-browse as handle no-undo.
def var de-dias-atraso as integer format -999.

def new global shared var v-row-parent    as rowid no-undo.

def var v-row-table   as rowid   no-undo.

def temp-table tt-documento no-undo like titulo
    field row-documento  as rowid
    field c-modalidade   like titulo.modalidade
    field c-mo-codigo    as char format "x(03)"
    field de-saldo       like titulo.vl-saldo
    field de-saldo-me    like titulo.vl-saldo-me
    field i-dias         as int init 0
    field de-juros       as dec 
    field de-total-juros as dec.

DEFINE TEMP-TABLE tt-doc NO-UNDO
       FIELD c-barra                      AS CHAR FORMAT "x(01)" INIT "/"
       FIELD c-barra2                     AS CHAR FORMAT "x(01)" INIT "/"
       FIELD cod-estabel                  LIKE titulo.cod-estabel
       FIELD cod-esp                      LIKE titulo.cod-esp
       FIELD serie                        LIKE titulo.serie
       FIELD nr-docto                     LIKE titulo.nr-docto
       FIELD parcela                      LIKE titulo.parcela
       FIELD portador                     LIKE tit-ap.portador
       FIELD modalidade                   LIKE tit-ap.modalidade
       FIELD vl-saldo                     LIKE titulo.vl-saldo
       FIELD vl-original                  LIKE titulo.vl-original
       FIELD dt-emissao                   LIKE tit-ap.dt-emissao
       FIELD dt-vencimen                  LIKE tit-ap.dt-vencimen
       FIELD cod-emitente                 LIKE titulo.cod-emitente
       FIELD nome-abrev                   LIKE titulo.nome-abrev
       FIELD dias-atraso                  LIKE tit-ap.dias-atraso.

DEFINE TEMP-TABLE tt-pag NO-UNDO
       FIELD c-barra        AS CHAR FORMAT "x(01)" INIT "/"
       FIELD c-barra2       AS CHAR FORMAT "x(01)" INIT "/"
       FIELD cod-emitente   LIKE emitente.cod-emitente
       FIELD nome-abrev     LIKE mov-ap.nome-abrev
       FIELD ep-codigo      LIKE mov-ap.ep-codigo
       FIELD cod-estabel    LIKE mov-ap.cod-estabel
       FIELD cod-esp        LIKE mov-ap.cod-esp
       FIELD nr-docto       LIKE mov-ap.nr-docto
       FIELD parcela        LIKE mov-ap.parcela
       FIELD portador       LIKE mov-ap.portador
       FIELD modalidade     LIKE mov-ap.modalidade 
       FIELD dt-vencimen    LIKE mov-ap.dt-vencimen
       FIELD dt-transacao   LIKE mov-ap.dt-transacao
       FIELD valor-baixa    LIKE mov-ap.valor-mov
       FIELD de-dias-atraso AS int.

def VAR r-emitente       as rowid no-undo.
def VAR fi-total-titulos as dec no-undo.
def VAR de-tot-aberto    as dec no-undo.
def VAR de-tot-anterior  as dec no-undo.

DEFINE BUFFER b-mov-ap FOR mov-ap.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE BrowserCadastro2
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES emitente
&Scoped-define FIRST-EXTERNAL-TABLE emitente


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR emitente.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mov-ap

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table mov-ap.dt-transacao mov-ap.nr-docto mov-ap.cod-estabel mov-ap.cod-esp mov-ap.serie mov-ap.parcela mov-ap.dt-vencimen mov-ap.portador mov-ap.valor-mov (mov-ap.dt-transacao - mov-ap.dt-vencimen) @ de-dias-atraso   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table   
&Scoped-define SELF-NAME br-table
&Scoped-define QUERY-STRING-br-table FOR EACH mov-ap     WHERE mov-ap.ep-codigo    = i-ep-codigo-usuario     and   mov-ap.cod-fornec   = emitente.cod-emit     and   mov-ap.transacao    = 2     and   mov-ap.cod-estabel  = c-cod-est     and  (mov-ap.cod-esp >=  c-ini-esp     and   mov-ap.cod-esp <=  c-fim-esp)     and  (mov-ap.dt-transacao >= dt-ini     and   mov-ap.dt-transacao <= dt-fim)     and  (mov-ap.portador     >= i-port-ini     and   mov-ap.portador     <= i-port-fim) NO-LOCK     BY mov-ap.dt-transacao      BY mov-ap.nr-docto       BY mov-ap.parcela
&Scoped-define OPEN-QUERY-br-table OPEN QUERY {&SELF-NAME} FOR EACH mov-ap     WHERE mov-ap.ep-codigo    = i-ep-codigo-usuario     and   mov-ap.cod-fornec   = emitente.cod-emit     and   mov-ap.transacao    = 2     and   mov-ap.cod-estabel  = c-cod-est     and  (mov-ap.cod-esp >=  c-ini-esp     and   mov-ap.cod-esp <=  c-fim-esp)     and  (mov-ap.dt-transacao >= dt-ini     and   mov-ap.dt-transacao <= dt-fim)     and  (mov-ap.portador     >= i-port-ini     and   mov-ap.portador     <= i-port-fim) NO-LOCK     BY mov-ap.dt-transacao      BY mov-ap.nr-docto       BY mov-ap.parcela.
&Scoped-define TABLES-IN-QUERY-br-table mov-ap
&Scoped-define FIRST-TABLE-IN-QUERY-br-table mov-ap


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-table bt-detalhe bt-filtro BT-IMP 
&Scoped-Define DISPLAYED-OBJECTS vl-tot-baixa 

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


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-detalhe 
     LABEL "&Detalhar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-filtro 
     LABEL "&Filtro" 
     SIZE 10 BY 1.

DEFINE BUTTON BT-IMP 
     LABEL "Imprimir" 
     SIZE 10 BY 1.

DEFINE VARIABLE vl-tot-baixa AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total Baixa" 
     VIEW-AS FILL-IN 
     SIZE 18.29 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      mov-ap SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _FREEFORM
  QUERY br-table NO-LOCK DISPLAY
      mov-ap.dt-transacao COLUMN-LABEL "Dt Trans"
      mov-ap.nr-docto
      mov-ap.cod-estabel
      mov-ap.cod-esp
      mov-ap.serie
      mov-ap.parcela
      mov-ap.dt-vencimen
      mov-ap.portador
      mov-ap.valor-mov
      (mov-ap.dt-transacao - mov-ap.dt-vencimen) @ de-dias-atraso
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 83.43 BY 6.92.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-table AT ROW 1 COL 2.14
     bt-detalhe AT ROW 8.08 COL 1.57
     bt-filtro AT ROW 8.08 COL 12
     BT-IMP AT ROW 8.08 COL 22.43
     vl-tot-baixa AT ROW 8.17 COL 64.72 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: BrowserCadastro2
   External Tables: mgadm.emitente
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 8.21
         WIDTH              = 85.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

/* {utp/ut-glob.i} */
{src/adm/method/browser.i}
{include/c-brows2.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit L-To-R                                       */
/* BROWSE-TAB br-table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN vl-tot-baixa IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH mov-ap
    WHERE mov-ap.ep-codigo    = i-ep-codigo-usuario
    and   mov-ap.cod-fornec   = emitente.cod-emit
    and   mov-ap.transacao    = 2
    and   mov-ap.cod-estabel  = c-cod-est
    and  (mov-ap.cod-esp >=  c-ini-esp
    and   mov-ap.cod-esp <=  c-fim-esp)
    and  (mov-ap.dt-transacao >= dt-ini
    and   mov-ap.dt-transacao <= dt-fim)
    and  (mov-ap.portador     >= i-port-ini
    and   mov-ap.portador     <= i-port-fim)
NO-LOCK
    BY mov-ap.dt-transacao
     BY mov-ap.nr-docto
      BY mov-ap.parcela.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE"
     _TblOptList       = "USED"
     _OrdList          = "movadm.mov-ap.dt-transacao|yes,movadm.mov-ap.nr-docto|yes,movadm.mov-ap.parcela|yes"
     _Where[1]         = "mov-ap.ep-codigo    = i-ep-codigo-usuario and
mov-ap.transacao    = 2         and
mov-ap.cod-estabel  = c-cod-est and
(mov-ap.cod-esp >=  c-ini-esp   and
 mov-ap.cod-esp <=  c-fim-esp) and
(mov-ap.dt-transacao >= dt-ini and 
 mov-ap.dt-transacao <= dt-fim) and
(mov-ap.portador     >= i-port-ini and 
 mov-ap.portador     <= i-port-fim)
"
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
  if avail mov-ap then
     apply 'choose' to bt-detalhe.
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
ON VALUE-CHANGED OF br-table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  /* run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))). */

  assign gr-mov-ap = rowid(mov-ap).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-detalhe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-detalhe B-table-Win
ON CHOOSE OF bt-detalhe IN FRAME F-Main /* Detalhar */
DO:

 APPLY 'value-changed' TO br-table IN FRAME {&FRAME-NAME}.

  run app/ap0804e.w .


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-filtro B-table-Win
ON CHOOSE OF bt-filtro IN FRAME F-Main /* Filtro */
DO:

    run app/ap0804d.w (input-output c-aberto,
                       input-output c-matriz,
                       input-output l-saldo-aberto,
                       input-output c-cod-est,
                       input-output c-ini-esp ,
                       input-output c-fim-esp,
                       input-output dt-ini,
                       input-output dt-fim,
                       input-output i-cod-moeda,
                       input-output i-port-ini,
                       input-output i-port-fim).

   run dispatch in this-procedure ('open-query':U).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BT-IMP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BT-IMP B-table-Win
ON CHOOSE OF BT-IMP IN FRAME F-Main /* Imprimir */
DO:
   FOR EACH tt-pag EXCLUSIVE-LOCK:
       DELETE tt-pag.
   END.
   FOR EACH b-mov-ap 
       WHERE b-mov-ap.ep-codigo    = i-ep-codigo-usuario
       and   b-mov-ap.cod-fornec   = emitente.cod-emit
       and   b-mov-ap.transacao    = 2
       and   b-mov-ap.cod-estabel  = c-cod-est
       and  (b-mov-ap.cod-esp >=  c-ini-esp
       and   b-mov-ap.cod-esp <=  c-fim-esp)
       and  (b-mov-ap.dt-transacao >= dt-ini
       and   b-mov-ap.dt-transacao <= dt-fim)
       and  (b-mov-ap.portador     >= i-port-ini
       and   b-mov-ap.portador     <= i-port-fim)
       NO-LOCK
        BY b-mov-ap.dt-transacao
        BY b-mov-ap.nr-docto
        BY b-mov-ap.parcela.

        CREATE tt-pag.                
        ASSIGN tt-pag.cod-emitente   = emitente.cod-emitente
               tt-pag.nome-abrev     = b-mov-ap.nome-abrev
               tt-pag.ep-codigo      = b-mov-ap.ep-codigo
               tt-pag.cod-estabel    = b-mov-ap.cod-estabel
               tt-pag.cod-esp        = b-mov-ap.cod-esp
               tt-pag.nr-docto       = b-mov-ap.nr-docto
               tt-pag.parcela        = b-mov-ap.parcela
               tt-pag.portador       = b-mov-ap.portador
               tt-pag.modalidade     = b-mov-ap.modalidade
               tt-pag.dt-vencimen    = b-mov-ap.dt-vencimen
               tt-pag.dt-transacao   = b-mov-ap.dt-transacao
               tt-pag.valor-baixa    = b-Mov-AP.valor-mov
               tt-pag.de-dias-atraso = (b-mov-ap.dt-transacao - b-mov-ap.dt-vencimen).
   END.
   RUN app/ap0804n.w (INPUT TABLE tt-doc,
                      INPUT TABLE tt-pag,
                      INPUT TABLE tt-documento,
                      INPUT r-emitente,
                      INPUT fi-total-titulos,
                      INPUT de-tot-aberto,
                      INPUT de-tot-anterior,
                      INPUT 2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{utp/ut-liter.i Dias_Atraso}

assign de-dias-atraso:label in browse br-table = return-value.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
    Purpose:     Override standard ADM method
    Notes:       
------------------------------------------------------------------------------*/

    ASSIGN bt-imp:VISIBLE IN FRAME {&FRAME-NAME} = YES.

      assign c-ini-esp       = "AA"
           c-fim-esp       = "ZZ"
           dt-ini          = today
           dt-fim          = today
           i-port-ini      = 0
           i-port-fim      = 99999.

    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    {utp/ut-liter.i Total_Baixa}
    vl-tot-baixa:label in frame {&frame-name} = trim(return-value).

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

  assign br-table:refreshable in frame {&frame-name} = no.

  assign vl-tot-baixa = 0.

  get first br-table.

  do while avail mov-ap :
     assign vl-tot-baixa = vl-tot-baixa + mov-ap.valor-mov.
     get next br-table.
  end.

  assign br-table:refreshable in frame {&frame-name} = yes.
  if avail mov-ap then 
     if br-table:fetch-selected-row(1) in frame {&frame-name} then.

  disp vl-tot-baixa with frame {&frame-name}.
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
       when "no-record-available":U then do:
          assign bt-detalhe:sensitive in frame {&frame-name} = no.
       end.
       when "no-external-record-available":U then do:
          assign bt-detalhe:sensitive in frame {&frame-name} = no.
       end.
       when "record-available":U then do:
          assign bt-detalhe:sensitive in frame {&frame-name} = yes.
       end.
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


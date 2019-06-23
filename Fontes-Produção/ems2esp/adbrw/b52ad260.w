&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
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
def buffer empresa for ems2cadme.empresa.

{include/i-prgvrs.i B52AD260 2.00.00.005}  /*** 010005 ***/


&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i b52ad260 MUT}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/browserd.w

&IF "{&mguni_version}" >= "2.07A" &THEN
def new global shared var i-emp-selecao as CHAR no-undo.
&ELSE
def new global shared var i-emp-selecao as CHAR no-undo.
&ENDIF

define variable c-lista-valor as character init '':U no-undo.

/*------------- FILTRO ---------------*/
def var c-nome-ini      as char no-undo.
def var c-nome-fim      as char no-undo.
def var c-esp-ini       as char no-undo.
def var c-esp-fim       as char no-undo.
def var i-portador-ini  as inte no-undo.
def var i-portador-fim  as inte no-undo.
def var i-bordero-ini   as inte no-undo.
def var i-bordero-fim   as inte no-undo.
def var dt-emissao-ini  as date no-undo.
def var dt-emissao-fim  as date no-undo.

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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tit-ap

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table tit-ap.ep-codigo tit-ap.cod-estabel ~
tit-ap.cod-esp tit-ap.serie tit-ap.cod-fornec tit-ap.nome-abrev ~
tit-ap.nr-docto tit-ap.parcela tit-ap.dt-emissao tit-ap.dt-vencimen ~
tit-ap.valor-saldo tit-ap.vl-original 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table 
&Scoped-define QUERY-STRING-br-table FOR EACH tit-ap WHERE ~{&KEY-PHRASE} ~
      AND     tit-ap.ep-codigo    = i-emp-selecao ~
and tit-ap.cod-estabel >= c-est-ini ~
and tit-ap.cod-estabel <= c-est-fim ~
and tit-ap.cod-fornec  >= i-forn-ini ~
and tit-ap.cod-fornec  <= i-forn-fim ~
and tit-ap.nr-docto    >= c-docto-ini ~
and tit-ap.nr-docto    <= c-docto-fim ~
and tit-ap.dt-vencimen >= dt-vencimen-ini ~
and tit-ap.dt-vencimen <= dt-vencimen-fim ~
and tit-ap.nome-abrev  >= c-nome-ini ~
and tit-ap.nome-abrev  <= c-nome-fim ~
and tit-ap.cod-esp     >= c-esp-ini ~
and tit-ap.cod-esp     <= c-esp-fim ~
and tit-ap.portador    >= i-portador-ini ~
and tit-ap.portador    <= i-portador-fim ~
and tit-ap.nr-bordero  >= i-bordero-ini ~
and tit-ap.nr-bordero  <= i-bordero-fim ~
and tit-ap.dt-emissao  >= dt-emissao-ini ~
and tit-ap.dt-emissao  <= dt-emissao-fim NO-LOCK
&Scoped-define OPEN-QUERY-br-table OPEN QUERY br-table FOR EACH tit-ap WHERE ~{&KEY-PHRASE} ~
      AND     tit-ap.ep-codigo    = i-emp-selecao ~
and tit-ap.cod-estabel >= c-est-ini ~
and tit-ap.cod-estabel <= c-est-fim ~
and tit-ap.cod-fornec  >= i-forn-ini ~
and tit-ap.cod-fornec  <= i-forn-fim ~
and tit-ap.nr-docto    >= c-docto-ini ~
and tit-ap.nr-docto    <= c-docto-fim ~
and tit-ap.dt-vencimen >= dt-vencimen-ini ~
and tit-ap.dt-vencimen <= dt-vencimen-fim ~
and tit-ap.nome-abrev  >= c-nome-ini ~
and tit-ap.nome-abrev  <= c-nome-fim ~
and tit-ap.cod-esp     >= c-esp-ini ~
and tit-ap.cod-esp     <= c-esp-fim ~
and tit-ap.portador    >= i-portador-ini ~
and tit-ap.portador    <= i-portador-fim ~
and tit-ap.nr-bordero  >= i-bordero-ini ~
and tit-ap.nr-bordero  <= i-bordero-fim ~
and tit-ap.dt-emissao  >= dt-emissao-ini ~
and tit-ap.dt-emissao  <= dt-emissao-fim NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-table tit-ap
&Scoped-define FIRST-TABLE-IN-QUERY-br-table tit-ap


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS c-est-ini c-est-fim i-forn-ini i-forn-fim ~
c-docto-ini c-docto-fim dt-vencimen-ini dt-vencimen-fim bt-confirma ~
br-table IMAGE-1 IMAGE-2 IMAGE-21 IMAGE-22 IMAGE-23 IMAGE-24 IMAGE-25 ~
IMAGE-26 
&Scoped-Define DISPLAYED-OBJECTS c-est-ini c-est-fim i-forn-ini i-forn-fim ~
c-docto-ini c-docto-fim dt-vencimen-ini dt-vencimen-fim 

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
<FOREIGN-KEYS></FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = ':U).

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
DEFINE BUTTON bt-confirma 
     IMAGE-UP FILE "image\im-sav":U
     LABEL "Button 1" 
     SIZE 5.14 BY 1.

DEFINE VARIABLE c-docto-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE c-docto-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE c-est-fim LIKE estabelec.cod-estabel
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE c-est-ini LIKE estabelec.cod-estabel
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE dt-vencimen-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE dt-vencimen-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE i-forn-fim AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 999999 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE i-forn-ini AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-2
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-21
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-22
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-23
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-24
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-25
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-26
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      tit-ap SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _STRUCTURED
  QUERY br-table NO-LOCK DISPLAY
      tit-ap.ep-codigo FORMAT ">>>9":U
      tit-ap.cod-estabel FORMAT "x(5)":U
      tit-ap.cod-esp FORMAT "!!!":U
      tit-ap.serie FORMAT "x(6)":U
      tit-ap.cod-fornec FORMAT ">>>>>>>>9":U
      tit-ap.nome-abrev FORMAT "X(15)":U
      tit-ap.nr-docto FORMAT "x(16)":U
      tit-ap.parcela FORMAT "x(2)":U
      tit-ap.dt-emissao FORMAT "99/99/9999":U
      tit-ap.dt-vencimen FORMAT "99/99/9999":U
      tit-ap.valor-saldo FORMAT "->>,>>>,>>>,>>9.99":U
      tit-ap.vl-original FORMAT "->>>>>>>,>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 86.86 BY 7.96.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     c-est-ini AT ROW 1 COL 17 COLON-ALIGNED HELP
          ""
     c-est-fim AT ROW 1 COL 42 COLON-ALIGNED HELP
          "" NO-LABEL
     i-forn-ini AT ROW 2 COL 17 COLON-ALIGNED
     i-forn-fim AT ROW 2 COL 42 COLON-ALIGNED NO-LABEL
     c-docto-ini AT ROW 3 COL 17 COLON-ALIGNED
     c-docto-fim AT ROW 3 COL 42 COLON-ALIGNED NO-LABEL
     dt-vencimen-ini AT ROW 4 COL 17 COLON-ALIGNED
     dt-vencimen-fim AT ROW 4 COL 42 COLON-ALIGNED NO-LABEL
     bt-confirma AT ROW 1.25 COL 82.43
     br-table AT ROW 5.08 COL 1
     IMAGE-1 AT ROW 1 COL 37
     IMAGE-2 AT ROW 1 COL 41
     IMAGE-21 AT ROW 2 COL 37
     IMAGE-22 AT ROW 2 COL 41
     IMAGE-23 AT ROW 3 COL 37
     IMAGE-24 AT ROW 3 COL 41
     IMAGE-25 AT ROW 4 COL 37
     IMAGE-26 AT ROW 4 COL 41
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
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
         HEIGHT             = 12.08
         WIDTH              = 87.29.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{include/c-brwzoo.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
/* BROWSE-TAB br-table bt-confirma F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN c-est-fim IN FRAME F-Main
   LIKE = mgadm.estabelec.cod-estabel EXP-SIZE                          */
/* SETTINGS FOR FILL-IN c-est-ini IN FRAME F-Main
   LIKE = mgadm.estabelec.cod-estabel EXP-SIZE                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _TblList          = "ems2movme.tit-ap"
     _Options          = "NO-LOCK KEY-PHRASE"
     _Where[1]         = "    tit-ap.ep-codigo    = i-emp-selecao
and tit-ap.cod-estabel >= c-est-ini
and tit-ap.cod-estabel <= c-est-fim
and tit-ap.cod-fornec  >= i-forn-ini
and tit-ap.cod-fornec  <= i-forn-fim
and tit-ap.nr-docto    >= c-docto-ini
and tit-ap.nr-docto    <= c-docto-fim
and tit-ap.dt-vencimen >= dt-vencimen-ini
and tit-ap.dt-vencimen <= dt-vencimen-fim
and tit-ap.nome-abrev  >= c-nome-ini
and tit-ap.nome-abrev  <= c-nome-fim
and tit-ap.cod-esp     >= c-esp-ini
and tit-ap.cod-esp     <= c-esp-fim
and tit-ap.portador    >= i-portador-ini
and tit-ap.portador    <= i-portador-fim
and tit-ap.nr-bordero  >= i-bordero-ini
and tit-ap.nr-bordero  <= i-bordero-fim
and tit-ap.dt-emissao  >= dt-emissao-ini
and tit-ap.dt-emissao  <= dt-emissao-fim"
     _FldNameList[1]   > ems2movme.tit-ap.ep-codigo
"tit-ap.ep-codigo" ? ">>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = ems2movme.tit-ap.cod-estabel
     _FldNameList[3]   > ems2movme.tit-ap.cod-esp
"tit-ap.cod-esp" ? "!!!" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ems2movme.tit-ap.serie
"tit-ap.serie" ? "x(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = ems2movme.tit-ap.cod-fornec
     _FldNameList[6]   > ems2movme.tit-ap.nome-abrev
"tit-ap.nome-abrev" ? "X(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   = ems2movme.tit-ap.nr-docto
     _FldNameList[8]   = ems2movme.tit-ap.parcela
     _FldNameList[9]   = ems2movme.tit-ap.dt-emissao
     _FldNameList[10]   = ems2movme.tit-ap.dt-vencimen
     _FldNameList[11]   > ems2movme.tit-ap.valor-saldo
"tit-ap.valor-saldo" ? "->>,>>>,>>>,>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   = ems2movme.tit-ap.vl-original
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

  run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))).
  run seta-valor.
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
  run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))).
  run new-state('Value-Changed|':U + string(this-procedure)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma B-table-Win
ON CHOOSE OF bt-confirma IN FRAME F-Main /* Button 1 */
DO:

    if  input frame {&FRAME-NAME} c-est-ini >
        input frame {&FRAME-NAME} c-est-fim then do:
        run utp/ut-msgs.p (input "show",
                           input 252,
                           input " ").
        apply 'entry' to c-est-ini in frame {&FRAME-NAME}.
        return no-apply.
    end.

    if  input frame {&FRAME-NAME} i-forn-ini >
        input frame {&FRAME-NAME} i-forn-fim then do:
        run utp/ut-msgs.p (input "show",
                           input 252,
                           input " ").
        apply 'entry' to i-forn-ini in frame {&FRAME-NAME}.
        return no-apply.
    end.

    if  input frame {&FRAME-NAME} c-docto-ini >
        input frame {&FRAME-NAME} c-docto-fim then do:
        run utp/ut-msgs.p (input "show",
                           input 252,
                           input " ").
        apply 'entry' to c-docto-ini in frame {&FRAME-NAME}.
        return no-apply.
    end.

    if  input frame {&FRAME-NAME} dt-vencimen-ini >
        input frame {&FRAME-NAME} dt-vencimen-fim then do:
        run utp/ut-msgs.p (input "show",
                           input 252,
                           input " ").
        apply 'entry' to dt-vencimen-ini in frame {&FRAME-NAME}.
        return no-apply.
    end.

    assign input frame {&FRAME-NAME} c-docto-ini
           input frame {&FRAME-NAME} c-docto-fim
           input frame {&FRAME-NAME} c-est-ini
           input frame {&FRAME-NAME} c-est-fim
           input frame {&FRAME-NAME} dt-vencimen-ini
           input frame {&FRAME-NAME} dt-vencimen-fim
           input frame {&FRAME-NAME} i-forn-ini
           input frame {&FRAME-NAME} i-forn-fim.

    RUN dispatch IN THIS-PROCEDURE ('open-query':U).
    apply 'value-changed' to {&browse-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

{utp/ut-liter.i Fornecedor}
assign i-forn-ini:label in frame {&FRAME-NAME} = trim(return-value).

{utp/ut-liter.i Estabelecimento}
assign c-est-ini:label in frame {&FRAME-NAME} = trim(return-value).

{utp/ut-liter.i Vencimento}
assign dt-vencimen-ini:label in frame {&FRAME-NAME} = trim(return-value).

{utp/ut-liter.i Documento}
assign c-docto-ini:label in frame {&FRAME-NAME} = trim(return-value).

{utp/ut-liter.i Data_Vencimento}
assign tit-ap.dt-vencimen:label in browse {&BROWSE-NAME} = trim(return-value).

{utp/ut-liter.i Data_Emiss∆o}
assign tit-ap.dt-emissao:label in browse {&BROWSE-NAME} = trim(return-value).

assign i-forn-fim = int(fill("9",length(i-forn-fim:format in frame {&FRAME-NAME}))).

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
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
  DEF VAR Filter-Value AS CHAR NO-UNDO.

  /* Copy 'Filter-Attributes' into local variables. */
  RUN get-attribute ('Filter-Value':U).
  Filter-Value = RETURN-VALUE.

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

    &IF "{&mguni_version}" >= "2.07A" &THEN
    ASSIGN c-est-fim = "ZZZZZ".
    &ELSE
    ASSIGN c-est-fim = "ZZZ".
    &ENDIF

    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    apply 'entry' to c-est-ini in frame {&FRAME-NAME}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-filtro B-table-Win 
PROCEDURE pi-filtro :
/*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
------------------------------------------------------------------------------*/

    def input param p-nome-ini      as char no-undo.
    def input param p-nome-fim      as char no-undo.
    def input param p-esp-ini       as char no-undo.
    def input param p-esp-fim       as char no-undo.
    def input param p-portador-ini  as inte no-undo.
    def input param p-portador-fim  as inte no-undo.
    def input param p-bordero-ini   as inte no-undo.
    def input param p-bordero-fim   as inte no-undo.
    def input param p-emissao-ini   as date no-undo.
    def input param p-emissao-fim   as date no-undo.
    def input param p-query         as logi no-undo.

    assign c-nome-ini      = p-nome-ini
           c-nome-fim      = p-nome-fim
           c-esp-ini       = p-esp-ini
           c-esp-fim       = p-esp-fim
           i-portador-ini  = p-portador-ini
           i-portador-fim  = p-portador-fim
           i-bordero-ini   = p-bordero-ini
           i-bordero-fim   = p-bordero-fim
           dt-emissao-ini  = p-emissao-ini
           dt-emissao-fim  = p-emissao-fim.

    if  p-query = yes then        
        RUN dispatch IN THIS-PROCEDURE ('open-query':U).

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

  /* There are no foreign keys supplied by this SmartObject. */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "RetornaValorCampo" B-table-Win _INLINE
/* Actions: ? ? ? ? support/brwrtval.p */
/* Procedure desativada */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


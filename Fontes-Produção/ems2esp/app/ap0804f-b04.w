&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          movadm           PROGRESS
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
{include/i-prgvrs.i AP0804F-B04 2.00.00.005}  /*** 010005 ***/


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

/*Tratamento para independˆncia de objetos*/
&glob ORIGINALNAME 'adbrw~\b32ad260.w'

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def buffer b-tit-ap for tit-ap.
DEF BUFFER b-mov-ap FOR mov-ap.

def new global shared var rw-tit      as rowid no-undo.

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
&Scoped-define EXTERNAL-TABLES tit-ap
&Scoped-define FIRST-EXTERNAL-TABLE tit-ap


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR tit-ap.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mov-ap b-mov-ap espec-ap b-tit-ap

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table b-tit-ap.cod-estabel b-tit-ap.nr-docto b-tit-ap.parcela b-tit-ap.cod-esp b-tit-ap.serie b-tit-ap.cod-fornec b-tit-ap.vl-original b-tit-ap.valor-saldo b-tit-ap.valor-ir b-tit-ap.cod-retencao b-tit-ap.cod-impto-ret   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table   
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH mov-ap OF tit-ap NO-LOCK                             WHERE (mov-ap.transacao       = 1 or    (mov-ap.transacao  = 6 and   mov-ap.lancamento = 2)) , ~
                                   EACH b-mov-ap NO-LOCK                                 WHERE b-mov-ap.ep-codigo    =  mov-ap.ep-codigo                                 and   b-mov-ap.cod-estabel  =  mov-ap.cod-estabel                                 and   b-mov-ap.esp-ant      =  mov-ap.cod-esp                                 and   b-mov-ap.serie-ant    =  mov-ap.serie                                 and   b-mov-ap.docto-ant    =  mov-ap.nr-docto                                 and   b-mov-ap.parc-ant     =  mov-ap.parcela                                 and   b-mov-ap.fornec-ant   =  mov-ap.cod-fornec                                 AND   (b-mov-ap.transacao       = 1 or    (b-mov-ap.transacao  = 6 and    b-mov-ap.lancamento = 2)), ~
                                       EACH espec-ap NO-LOCK                                      WHERE espec-ap.cod-esp = b-mov-ap.cod-esp                                        AND ((espec-ap.tipo = 8)                                        OR   (espec-ap.tipo = 1 AND espec-ap.log-1 = YES)), ~
                                               EACH b-tit-ap                                              WHERE  b-tit-ap.ep-codigo   = b-mov-ap.ep-codigo                                                and  b-tit-ap.cod-estabel = b-mov-ap.cod-estabel                                                and  b-tit-ap.cod-fornec  = b-mov-ap.cod-fornec                                                and  b-tit-ap.cod-esp     = b-mov-ap.cod-esp                                                and  b-tit-ap.serie       = b-mov-ap.serie                                                and  b-tit-ap.nr-docto    = b-mov-ap.nr-docto                                                and  b-tit-ap.parcela     = b-mov-ap.parcela                                                and (b-tit-ap.tipo        = 8                                                or   b-tit-ap.tipo        = 1) no-lock
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH mov-ap OF tit-ap NO-LOCK     where (mov-ap.transacao       = 1 or    (mov-ap.transacao  = 6 and    mov-ap.lancamento = 2)), ~
                                   EACH b-mov-ap NO-LOCK                                 WHERE b-mov-ap.ep-codigo    =  mov-ap.ep-codigo                                 and   b-mov-ap.cod-estabel  =  mov-ap.cod-estabel                                 and   b-mov-ap.esp-ant      =  mov-ap.cod-esp                                 and   b-mov-ap.serie-ant    =  mov-ap.serie                                 and   b-mov-ap.docto-ant    =  mov-ap.nr-docto                                 and   b-mov-ap.parc-ant     =  mov-ap.parcela                                 and   b-mov-ap.fornec-ant   =  mov-ap.cod-fornec                                 AND  (b-mov-ap.transacao       = 1 or    (b-mov-ap.transacao  = 6 and    b-mov-ap.lancamento = 2)), ~
                                       EACH espec-ap NO-LOCK                                      WHERE espec-ap.cod-esp = b-mov-ap.cod-esp                                        AND ((espec-ap.tipo = 8)                                        OR   (espec-ap.tipo = 1 AND espec-ap.log-1 = YES)), ~
                                               EACH b-tit-ap                                              WHERE  b-tit-ap.ep-codigo   = b-mov-ap.ep-codigo                                                and  b-tit-ap.cod-estabel = b-mov-ap.cod-estabel                                                and  b-tit-ap.cod-fornec  = b-mov-ap.cod-fornec                                                and  b-tit-ap.cod-esp     = b-mov-ap.cod-esp                                                and  b-tit-ap.serie       = b-mov-ap.serie                                                and  b-tit-ap.nr-docto    = b-mov-ap.nr-docto                                                and  b-tit-ap.parcela     = b-mov-ap.parcela                                                and (b-tit-ap.tipo        = 8                                                or   b-tit-ap.tipo        = 1) no-lock.
&Scoped-define TABLES-IN-QUERY-br_table mov-ap b-mov-ap espec-ap b-tit-ap
&Scoped-define FIRST-TABLE-IN-QUERY-br_table mov-ap
&Scoped-define SECOND-TABLE-IN-QUERY-br_table b-mov-ap
&Scoped-define THIRD-TABLE-IN-QUERY-br_table espec-ap
&Scoped-define FOURTH-TABLE-IN-QUERY-br_table b-tit-ap


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-det bt-impto br_table RECT-13 

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
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-det 
     IMAGE-UP FILE "image~\im-det.bmp":U NO-FOCUS
     LABEL "Detalhe" 
     SIZE 4.14 BY 1.25
     FONT 4.

DEFINE BUTTON bt-impto 
     IMAGE-UP FILE "image/im-impt.bmp":U NO-FOCUS
     LABEL "Impostos" 
     SIZE 4.14 BY 1.25 TOOLTIP "Verificar impostos pendentes vinculados ao t¡tulo"
     FONT 4.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 5.86 BY 5.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      mov-ap, 
      b-mov-ap, 
      espec-ap, 
      b-tit-ap SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      b-tit-ap.cod-estabel format "x(04)"
b-tit-ap.nr-docto
b-tit-ap.parcela
b-tit-ap.cod-esp format "x(04)"
b-tit-ap.serie  format "x(06)"
b-tit-ap.cod-fornec format ">>>>>>>>>>9"
b-tit-ap.vl-original
b-tit-ap.valor-saldo
b-tit-ap.valor-ir
b-tit-ap.cod-retencao
b-tit-ap.cod-impto-ret format ">>>9"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 80 BY 5.29
         TITLE "".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     bt-det AT ROW 1.25 COL 82 HELP
          "Detalhar Imposto Selecionado"
     bt-impto AT ROW 2.75 COL 82 HELP
          "Verificar impostos pendentes vinculados ao t¡tulo"
     br_table AT ROW 1 COL 1
     RECT-13 AT ROW 1 COL 81.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: movadm.tit-ap
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY
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
         HEIGHT             = 5.38
         WIDTH              = 86.
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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH mov-ap OF tit-ap NO-LOCK
                            WHERE mov-ap.transacao = 1,
                            EACH b-mov-ap NO-LOCK
                                WHERE b-mov-ap.ep-codigo    =  mov-ap.ep-codigo
                                and   b-mov-ap.cod-estabel  =  mov-ap.cod-estabel
                                and   b-mov-ap.esp-ant      =  mov-ap.cod-esp
                                and   b-mov-ap.serie-ant    =  mov-ap.serie
                                and   b-mov-ap.docto-ant    =  mov-ap.nr-docto
                                and   b-mov-ap.parc-ant     =  mov-ap.parcela
                                and   b-mov-ap.fornec-ant   =  mov-ap.cod-fornec
                                AND   b-mov-ap.transacao   = 1,
                                EACH espec-ap NO-LOCK
                                     WHERE espec-ap.cod-esp = b-mov-ap.cod-esp
                                       AND ((espec-ap.tipo = 8)
                                       OR   (espec-ap.tipo = 1 AND espec-ap.log-1 = YES)),
                                        EACH b-tit-ap
                                             WHERE  b-tit-ap.ep-codigo   = b-mov-ap.ep-codigo
                                               and  b-tit-ap.cod-estabel = b-mov-ap.cod-estabel
                                               and  b-tit-ap.cod-fornec  = b-mov-ap.cod-fornec
                                               and  b-tit-ap.cod-esp     = b-mov-ap.cod-esp
                                               and  b-tit-ap.serie       = b-mov-ap.serie
                                               and  b-tit-ap.nr-docto    = b-mov-ap.nr-docto
                                               and  b-tit-ap.parcela     = b-mov-ap.parcela
                                               and (b-tit-ap.tipo        = 8
                                               or   b-tit-ap.tipo        = 1) no-lock.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
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


&Scoped-define SELF-NAME bt-det
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det B-table-Win
ON CHOOSE OF bt-det IN FRAME F-Main /* Detalhe */
DO:

    IF  AVAIL b-tit-ap THEN DO:
        ASSIGN rw-tit = ROWID(b-tit-ap).
        RUN app/ap0804f.w.
    END.
    ELSE DO:
        run utp/ut-msgs.p (input "show",
                           input 17197,
                           input "um registro").
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-impto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-impto B-table-Win
ON CHOOSE OF bt-impto IN FRAME F-Main /* Impostos */
DO:
   
    IF  AVAIL tit-ap THEN
        RUN app/ap0804ff.w (INPUT ROWID(tit-ap)).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

{utp/ut-liter.i Rend_Tribut vel}
ASSIGN b-tit-ap.valor-ir:LABEL IN BROWSE br_table       = TRIM(RETURN-VALUE).

{utp/ut-liter.i Impostos_J _Retidos_T¡tulo}
ASSIGN br_table:TITLE IN FRAME {&FRAME-NAME} = TRIM(RETURN-VALUE).

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
  {src/adm/template/row-list.i "tit-ap"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "tit-ap"}

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
  {src/adm/template/snd-list.i "tit-ap"}
  {src/adm/template/snd-list.i "mov-ap"}
  {src/adm/template/snd-list.i "b-mov-ap"}
  {src/adm/template/snd-list.i "espec-ap"}
  {src/adm/template/snd-list.i "b-tit-ap"}

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


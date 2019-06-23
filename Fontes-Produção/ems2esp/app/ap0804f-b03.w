&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
{include/i-prgvrs.i AP0804F-B03 2.00.00.004}  /*** 010004 ***/
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

/*Tratamento para independˆncia de objetos*/
&glob ORIGINALNAME 'adbrw\b04ad180.w'

def var c-transacao     as char no-undo.
def var c-lista-trans   as char no-undo.
def var l-modulo-fc     as log  no-undo.
def var de-efeito-maxi  like mov-ap.efeito-maxi no-undo.
def var de-ganho-perda  like mov-ap.ganho-perda no-undo.

/*def var de-vl-antecip   like mov-ap.valor-mov no-undo.*/

def buffer b-mov-ap for mov-ap.
def buffer b-mov-ap-aux for mov-ap.

def temp-table tt-mov-ap no-undo like mov-ap
    field c-transacao    as char format "x(5)"
    field c-lancamento   like mov-ap.lancamento
    field gr-tt-doc      as rowid.

def temp-table tt-espec-ap no-undo like espec-ap.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES tit-ap
&Scoped-define FIRST-EXTERNAL-TABLE tit-ap


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR tit-ap.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-mov-ap

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table entry(tt-mov-ap.transacao, c-lista-trans) @ c-transacao tt-mov-ap.dt-trans tt-mov-ap.dt-vencimen if tt-mov-ap.transacao = 8 then 0 else fn-valor-mov() @ tt-mov-ap.vl-mov-fasb if tt-mov-ap.transacao <> 8 then 0 else fn-efeito-maxi() @ de-efeito-maxi if tt-mov-ap.transacao <> 8 then 0 else fn-ganho-perda() @ de-ganho-perda   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table   
&Scoped-define FIELD-PAIRS-IN-QUERY-br-table
&Scoped-define SELF-NAME br-table
&Scoped-define OPEN-QUERY-br-table if l-modulo-fc = yes then    run pi-criar-tt-mov-ap.  OPEN QUERY {&SELF-NAME}    FOR EACH tt-mov-ap WHERE tt-mov-ap.transacao  <> 9                        AND   l-modulo-fc = yes                        NO-LOCK                        BY tt-mov-ap.dt-today                        BY tt-mov-ap.c-time.
&Scoped-define TABLES-IN-QUERY-br-table tt-mov-ap
&Scoped-define FIRST-TABLE-IN-QUERY-br-table tt-mov-ap


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-table 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-efeito-maxi B-table-Win 
FUNCTION fn-efeito-maxi RETURNS DECIMAL
    ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-ganho-perda B-table-Win 
FUNCTION fn-ganho-perda RETURNS DECIMAL
    ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-valor-mov B-table-Win 
FUNCTION fn-valor-mov RETURNS DECIMAL
    ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      tt-mov-ap SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _FREEFORM
  QUERY br-table NO-LOCK DISPLAY
      entry(tt-mov-ap.transacao,  c-lista-trans) @ c-transacao format "X(03)"
      tt-mov-ap.dt-trans
      tt-mov-ap.dt-vencimen
      if tt-mov-ap.transacao  = 8 then 0 else fn-valor-mov()   @ tt-mov-ap.vl-mov-fasb
      if tt-mov-ap.transacao <> 8 then 0 else fn-efeito-maxi() @ de-efeito-maxi
      if tt-mov-ap.transacao <> 8 then 0 else fn-ganho-perda() @ de-ganho-perda
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 87.43 BY 6.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-table AT ROW 1 COL 1
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
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 6.83
         WIDTH              = 87.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br-table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _START_FREEFORM
if l-modulo-fc = yes then
   run pi-criar-tt-mov-ap.

OPEN QUERY {&SELF-NAME}
   FOR EACH tt-mov-ap WHERE tt-mov-ap.transacao  <> 9
                       AND   l-modulo-fc = yes
                       NO-LOCK
                       BY tt-mov-ap.dt-today
                       BY tt-mov-ap.c-time.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "l-modulo-fc = yes
and mov-ap.transacao <> 9"
     _Query            is NOT OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{include/c-browse.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
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
ON VALUE-CHANGED OF br-table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))).
  
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

{utp/ut-field.i mgadm mov-ap transacao  1}
assign c-transacao:label in browse br-table = return-value.

assign c-lista-trans = {adinc/i01ad165.i 03}.   

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win _ADM-ROW-AVAILABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win _DEFAULT-DISABLE
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
    
    assign l-modulo-fc = no.
    
    find first param-global no-lock no-error.
    if  avail param-global then do:
        if  param-global.modulo-fc = yes then
            assign l-modulo-fc = yes.
        else
            assign l-modulo-fc = no.
    end.
    
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view B-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calcula-valores B-table-Win 
PROCEDURE pi-calcula-valores :
/*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
------------------------------------------------------------------------------*/
         
    assign de-efeito-maxi = 0
           de-ganho-perda = 0.
           
    if  avail tit-ap 
    and avail tt-mov-ap then do:
        if  tit-ap.tipo = 2 and tit-ap.valor-saldo < tit-ap.vl-original then do:
            if  tt-mov-ap.transacao <> 8 and tt-mov-ap.transacao <> 9 then do:
                if  tt-mov-ap.transacao = 3 or (tt-mov-ap.transacao = 5 and tt-mov-ap.ind-tp-alter = 2) then do:
                    find first b-mov-ap
                         where b-mov-ap.ep-codigo    = tt-mov-ap.ep-codigo
                         and   b-mov-ap.cod-estabel  = tt-mov-ap.cod-estabel
                         and   b-mov-ap.cod-fornec   = tt-mov-ap.cod-fornec
                         and   b-mov-ap.cod-esp      = tt-mov-ap.cod-esp
                         and   b-mov-ap.serie        = tt-mov-ap.serie
                         and   b-mov-ap.nr-docto     = tt-mov-ap.nr-docto
                         and   b-mov-ap.parcela      = tt-mov-ap.parcela
                         and   b-mov-ap.dt-transacao = tt-mov-ap.dt-transacao
                         and   b-mov-ap.transacao    = 8
                         no-lock no-error.
                                
                    if  avail b-mov-ap then do:     
                        assign de-efeito-maxi = b-mov-ap.valor-mov
                               de-ganho-perda = b-mov-ap.valor-juros.
                    end.
                end.
            end.
            else do:
                if  tt-mov-ap.transacao = 8 then do:
                    assign de-efeito-maxi = tt-mov-ap.valor-mov
                           de-ganho-perda = tt-mov-ap.valor-juros.
                end.
            end.
        end.
        else do:
            if  tt-mov-ap.transacao <> 8 and tt-mov-ap.transacao <> 9 then do:
                if  tt-mov-ap.transacao = 2 or (tt-mov-ap.transacao = 5 and tt-mov-ap.ind-tp-alter = 2) then do:
                    find first b-mov-ap
                         where b-mov-ap.ep-codigo    = tt-mov-ap.ep-codigo
                         and   b-mov-ap.cod-estabel  = tt-mov-ap.cod-estabel
                         and   b-mov-ap.cod-fornec   = tt-mov-ap.cod-fornec
                         and   b-mov-ap.cod-esp      = tt-mov-ap.cod-esp
                         and   b-mov-ap.serie        = tt-mov-ap.serie
                         and   b-mov-ap.nr-docto     = tt-mov-ap.nr-docto
                         and   b-mov-ap.parcela      = tt-mov-ap.parcela
                         and   b-mov-ap.dt-transacao = tt-mov-ap.dt-transacao
                         and   b-mov-ap.transacao    = 8
                         no-lock no-error.
                    if  avail b-mov-ap then do:    
                        assign de-efeito-maxi = b-mov-ap.valor-mov
                               de-ganho-perda = b-mov-ap.valor-juros.
                    end.
                end.
            end.             
            else do:
                if  tt-mov-ap.transacao = 8 then do:
                    assign de-efeito-maxi = tt-mov-ap.valor-mov
                           de-ganho-perda = tt-mov-ap.valor-juros.
                end.
            end.
        end. 
    end. 
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-criar-tt-mov-ap B-table-Win 
PROCEDURE pi-criar-tt-mov-ap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    for each tt-mov-ap exclusive-lock:
        delete tt-mov-ap.
    end.   

    for each  mov-ap
        where mov-ap.ep-codigo    = tit-ap.ep-codigo 
        and   mov-ap.cod-estabel  = tit-ap.cod-estabel
        and   mov-ap.cod-esp      = tit-ap.cod-esp
        and   mov-ap.cod-fornec   = tit-ap.cod-fornec
        and   mov-ap.serie        = tit-ap.serie
        and   mov-ap.nr-docto     = tit-ap.nr-docto
        and   mov-ap.parcela      = tit-ap.parcela
        no-lock:

        find first tt-espec-ap where tt-espec-ap.cod-esp = mov-ap.cod-esp no-lock no-error.
        if  not avail tt-espec-ap then do:
            find espec-ap where espec-ap.cod-esp = mov-ap.cod-esp no-lock no-error.       
            if avail espec-ap then do:
               create tt-espec-ap.
               buffer-copy espec-ap to tt-espec-ap.
            end.   
        end.
                
        if  avail tt-espec-ap and tt-espec-ap.tipo = 2 then do:
                                                    
            for each  b-mov-ap-aux use-index ch-antecip
                where b-mov-ap-aux.ep-codigo     = mov-ap.ep-codigo 
                and   b-mov-ap-aux.cod-estabel   = mov-ap.cod-estabel
                and   b-mov-ap-aux.esp-ant       = mov-ap.cod-esp
                and   b-mov-ap-aux.serie-ant     = mov-ap.serie
                and   b-mov-ap-aux.docto-ant     = mov-ap.nr-docto
                and   b-mov-ap-aux.parc-ant      = mov-ap.parcela
                no-lock:
                        
                find first tt-mov-ap 
                     where tt-mov-ap.num-id-mov-ap  = b-mov-ap-aux.num-id-mov-ap no-error.
                        
                if  not avail tt-mov-ap then do:
                    create tt-mov-ap.
                    assign tt-mov-ap.transacao         = b-mov-ap-aux.transacao
                           tt-mov-ap.c-transacao       = entry(b-mov-ap-aux.transacao, c-lista-trans)
                           tt-mov-ap.lancamento        = b-mov-ap-aux.lancamento
                           tt-mov-ap.dt-trans          = b-mov-ap-aux.dt-trans
                           tt-mov-ap.dt-vencimen       = b-mov-ap-aux.dt-vencimen
                           tt-mov-ap.valor-juros       = b-mov-ap-aux.valor-juros
                           tt-mov-ap.valor-juros-me    = b-mov-ap-aux.valor-juros-me
                           tt-mov-ap.vl-original       = b-mov-ap-aux.vl-original
                           tt-mov-ap.vl-mov-fasb       = b-mov-ap-aux.vl-mov-fasb
                           tt-mov-ap.valor-mov         = b-mov-ap-aux.valor-mov                  
                           tt-mov-ap.vl-ant-fasb       = b-mov-ap-aux.vl-ant-fasb
                           tt-mov-ap.vl-antecip        = b-mov-ap-aux.vl-antecip
                           tt-mov-ap.vl-antecip-me     = b-mov-ap-aux.vl-antecip-me                           
                           tt-mov-ap.ep-codigo         = b-mov-ap-aux.ep-codigo
                           tt-mov-ap.serie             = b-mov-ap-aux.serie
                           tt-mov-ap.cod-esp           = b-mov-ap-aux.cod-esp
                           tt-mov-ap.cod-estabel       = b-mov-ap-aux.cod-estabel
                           tt-mov-ap.cod-fornec        = b-mov-ap-aux.cod-fornec
                           tt-mov-ap.nr-docto          = b-mov-ap-aux.nr-docto
                           tt-mov-ap.parcela           = b-mov-ap-aux.parcela
                           tt-mov-ap.serie-ant         = b-mov-ap-aux.serie
                           tt-mov-ap.esp-ant           = b-mov-ap-aux.cod-esp
                           tt-mov-ap.docto-ant         = b-mov-ap-aux.nr-docto
                           tt-mov-ap.parc-ant          = b-mov-ap-aux.parcela
                           tt-mov-ap.num-id-mov-ap     = b-mov-ap-aux.num-id-mov-ap
                           tt-mov-ap.flag-contab       = b-mov-ap-aux.flag-contab
                           tt-mov-ap.tipo              = b-mov-ap-aux.tipo
                           tt-mov-ap.gr-tt-doc         = rowid(b-mov-ap-aux)
                           tt-mov-ap.ind-tp-alter      = b-mov-ap-aux.ind-tp-alter
                           tt-mov-ap.dt-today          = b-mov-ap-aux.dt-today
                           tt-mov-ap.c-time            = b-mov-ap-aux.c-time.
                end.      
            end.
        end.
        find first tt-mov-ap where tt-mov-ap.num-id-mov-ap  = mov-ap.num-id-mov-ap no-error.
        if  not avail tt-mov-ap then do:           
            create tt-mov-ap.
            assign tt-mov-ap.transacao         = mov-ap.transacao
                   tt-mov-ap.c-transacao       = entry(mov-ap.transacao, c-lista-trans)
                   tt-mov-ap.lancamento        = mov-ap.lancamento
                   tt-mov-ap.dt-transacao      = mov-ap.dt-transacao
                   tt-mov-ap.dt-vencimen       = mov-ap.dt-vencimen
                   tt-mov-ap.valor-juros       = mov-ap.valor-juros
                   tt-mov-ap.valor-juros-me    = mov-ap.valor-juros-me
                   tt-mov-ap.vl-original       = mov-ap.vl-original
                   tt-mov-ap.vl-mov-fasb       = mov-ap.vl-mov-fasb
                   tt-mov-ap.valor-mov         = mov-ap.valor-mov                  
                   tt-mov-ap.vl-ant-fasb       = mov-ap.vl-ant-fasb
                   tt-mov-ap.vl-antecip        = mov-ap.vl-antecip
                   tt-mov-ap.vl-antecip-me     = mov-ap.vl-antecip-me
                   tt-mov-ap.ep-codigo         = mov-ap.ep-codigo
                   tt-mov-ap.serie             = mov-ap.serie
                   tt-mov-ap.cod-esp           = mov-ap.cod-esp
                   tt-mov-ap.cod-estabel       = mov-ap.cod-estabel                   
                   tt-mov-ap.cod-fornec        = mov-ap.cod-fornec                   
                   tt-mov-ap.nr-docto          = mov-ap.nr-docto
                   tt-mov-ap.parcela           = mov-ap.parcela
                   tt-mov-ap.serie-ant         = mov-ap.serie-ant
                   tt-mov-ap.esp-ant           = mov-ap.esp-ant
                   tt-mov-ap.docto-ant         = mov-ap.docto-ant
                   tt-mov-ap.parc-ant          = mov-ap.parc-ant
                   tt-mov-ap.flag-contab       = mov-ap.flag-contab
                   tt-mov-ap.tipo              = mov-ap.tipo
                   tt-mov-ap.num-id-mov-ap     = mov-ap.num-id-mov-ap
                   tt-mov-ap.gr-tt-doc         = rowid(mov-ap)
                   tt-mov-ap.ind-tp-alter      = mov-ap.ind-tp-alter
                   tt-mov-ap.dt-today          = mov-ap.dt-today
                   tt-mov-ap.c-time            = mov-ap.c-time.
                
            /* A logica abaixo foi implementada pela localizacao
               argentina. Conceito de titulos em garantia, que
               ainda nao existe no produto padrao */
            if mov-ap.modalidade = 3 /* Caucao */ and
               mov-ap.referencia begins "GAR" then 
               assign tt-mov-ap.c-transacao = "GAR".

        end.       
    end. 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-efeito-maxi B-table-Win 
FUNCTION fn-efeito-maxi RETURNS DECIMAL
    () :
/*------------------------------------------------------------------------------
    Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    
    run pi-calcula-valores.  
        
    RETURN de-efeito-maxi.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-ganho-perda B-table-Win 
FUNCTION fn-ganho-perda RETURNS DECIMAL
    () :
/*------------------------------------------------------------------------------
    Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    
    run pi-calcula-valores.  
        
    RETURN de-ganho-perda.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-valor-mov B-table-Win 
FUNCTION fn-valor-mov RETURNS DECIMAL
    () :
/*------------------------------------------------------------------------------
    Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    return tt-mov-ap.vl-mov-fasb.
        
    RETURN de-ganho-perda.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



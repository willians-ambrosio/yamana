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
def buffer empresa for ems2cadme.empresa.
def buffer moeda   for ems2cadme.moeda.

{include/i-prgvrs.i B01AD228 2.00.00.019}  /*** 010019 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i b01ad228 MUT}
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
 
/* Definicao TTs e Variaveis - Validacao Decimais - Chile */
{cdp/cd1234.i} 

/* miniflexibilizaá∆o Rac */
  {cdp/cdcfgfin.i}
   
/* Parameters Definitions ---                                           */
 
/* Local Variable Definitions ---                                       */
def var de-perc       as de format ">>9.99" no-undo.
def var de-vl-emissao like titulo.vl-original no-undo.
def var v-row-table   as rowid.
 
def new global shared var v-row-parent as rowid no-undo.

&IF "{&mguni_version}" >= "2.07A" &THEN
def new global shared var i-emp-selecao LIKE empresa.ep-codigo no-undo.
&ELSE
def new global shared var i-emp-selecao as integer no-undo.
&ENDIF

&IF DEFINED(BF_FIN_RAC) &THEN
   def var de-abati          like rep-tit.perc-abatimento.
   def var de-juros          like rep-tit.perc-juros.
   def var de-multa          like rep-tit.perc-multa.
   def var de-descon         like rep-tit.perc-desconto.
   def var de-ava            like rep-tit.perc-ava.
   def var c-tipo            as char format "x(15)"  no-undo.
&ENDIF

def var l-calcula as logical no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES titulo
&Scoped-define FIRST-EXTERNAL-TABLE titulo


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR titulo.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rep-tit repres

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table rep-tit.cod-rep &IF DEFINED(BF_FIN_RAC) &THEN rep-tit.cod-classificador &ENDIF repres.nome rep-tit.comissao rep-tit.comis-emis &IF DEFINED(BF_FIN_RAC) &THEN rep-tit.perc-abatimento @ de-abati rep-tit.perc-juros @ de-juros rep-tit.perc-multa @ de-multa rep-tit.perc-desconto @ de-descon rep-tit.perc-ava @ de-ava rep-tit.comis-propor &ENDIF   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table   
&Scoped-define SELF-NAME br-table
&Scoped-define QUERY-STRING-br-table FOR EACH rep-tit WHERE rep-tit.ep-codigo = titulo.ep-codigo  AND rep-tit.cod-estabel = titulo.cod-estabel  AND rep-tit.cod-esp = titulo.cod-esp  AND rep-tit.nr-docto = titulo.nr-docto  AND rep-tit.parcela = titulo.parcela  AND rep-tit.serie   = titulo.serie OUTER-JOIN NO-LOCK, ~
             EACH repres OF rep-tit NO-LOCK     ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br-table OPEN QUERY {&SELF-NAME} FOR EACH rep-tit WHERE rep-tit.ep-codigo = titulo.ep-codigo  AND rep-tit.cod-estabel = titulo.cod-estabel  AND rep-tit.cod-esp = titulo.cod-esp  AND rep-tit.nr-docto = titulo.nr-docto  AND rep-tit.parcela = titulo.parcela  AND rep-tit.serie   = titulo.serie OUTER-JOIN NO-LOCK, ~
             EACH repres OF rep-tit NO-LOCK     ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br-table rep-tit repres
&Scoped-define FIRST-TABLE-IN-QUERY-br-table rep-tit
&Scoped-define SECOND-TABLE-IN-QUERY-br-table repres


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 br-table 
&Scoped-Define DISPLAYED-FIELDS titulo.cod-rep 
&Scoped-define DISPLAYED-TABLES titulo
&Scoped-define FIRST-DISPLAYED-TABLE titulo
&Scoped-Define DISPLAYED-OBJECTS c-descricao tg-desativado c-sigla-1 ~
de-vl-liquido c-sigla-2 de-vl-comissao fi-pedido-rep c-sigla-3 ~
de-vl-vinculado fi-pedido-cli 

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
cod-rep|y|y|movadm.rep-tit.cod-rep
cod-emitente||y|movadm.rep-tit.cod-emitente
cod-esp||y|movadm.rep-tit.cod-esp
cod-estabel||y|movadm.rep-tit.cod-estabel
ep-codigo||y|movadm.rep-tit.ep-codigo
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "cod-rep",
     Keys-Supplied = "cod-rep,cod-emitente,cod-esp,cod-estabel,ep-codigo"':U).
 
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
DEFINE VARIABLE c-descricao AS CHARACTER FORMAT "X(45)":U 
     VIEW-AS FILL-IN 
     SIZE 53.29 BY .88 NO-UNDO.

DEFINE VARIABLE c-sigla-1 AS CHARACTER FORMAT "X(5)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE c-sigla-2 AS CHARACTER FORMAT "X(5)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE c-sigla-3 AS CHARACTER FORMAT "X(5)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE de-vl-comissao AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE de-vl-liquido AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE de-vl-vinculado AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE fi-pedido-cli AS CHARACTER FORMAT "X(12)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 21.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-pedido-rep AS CHARACTER FORMAT "x(12)" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 21.57 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 83 BY 3.42.

DEFINE VARIABLE tg-desativado AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      rep-tit, 
      repres SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _FREEFORM
  QUERY br-table NO-LOCK DISPLAY
      rep-tit.cod-rep
      &IF DEFINED(BF_FIN_RAC) &THEN
          rep-tit.cod-classificador
      &ENDIF
      repres.nome FORMAT "x(57)"
      rep-tit.comissao   FORMAT ">>9.9999"
      rep-tit.comis-emis FORMAT ">>9.99"
      &IF DEFINED(BF_FIN_RAC) &THEN
          rep-tit.perc-abatimento @ de-abati  column-label "% Abatimento" 
          rep-tit.perc-juros      @ de-juros  column-label "% Juros"      
          rep-tit.perc-multa      @ de-multa  column-label "% Multa"      
          rep-tit.perc-desconto   @ de-descon column-label "% Desconto"   
          rep-tit.perc-ava        @ de-ava    column-label "% Ava"        
          rep-tit.comis-propor    column-label "Comiss∆o Propor"
      &ENDIF
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 83 BY 5.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     titulo.cod-rep AT ROW 1.17 COL 20.29 COLON-ALIGNED
          LABEL "Representante":R16
          VIEW-AS FILL-IN 
          SIZE 7.72 BY .88
     c-descricao AT ROW 1.17 COL 28.29 COLON-ALIGNED NO-LABEL
     br-table AT ROW 2.17 COL 83 RIGHT-ALIGNED
     tg-desativado AT ROW 7.75 COL 2
     c-sigla-1 AT ROW 7.75 COL 25.29 COLON-ALIGNED
     de-vl-liquido AT ROW 7.75 COL 29.72 COLON-ALIGNED NO-LABEL
     c-sigla-2 AT ROW 7.75 COL 60.29 COLON-ALIGNED
     de-vl-comissao AT ROW 7.75 COL 64.72 COLON-ALIGNED NO-LABEL
     fi-pedido-rep AT ROW 8.75 COL 25.29 COLON-ALIGNED
     c-sigla-3 AT ROW 8.75 COL 60.29 COLON-ALIGNED
     de-vl-vinculado AT ROW 8.75 COL 64.72 COLON-ALIGNED NO-LABEL
     fi-pedido-cli AT ROW 9.75 COL 25.29 COLON-ALIGNED
     RECT-1 AT ROW 7.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: movadm.titulo
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
         HEIGHT             = 9.96
         WIDTH              = 83.
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
/* BROWSE-TAB br-table c-descricao F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE br-table IN FRAME F-Main
   ALIGN-R                                                              */
/* SETTINGS FOR FILL-IN c-descricao IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-sigla-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-sigla-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-sigla-3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN titulo.cod-rep IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN de-vl-comissao IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN de-vl-liquido IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN de-vl-vinculado IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-pedido-cli IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-pedido-rep IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-desativado IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH rep-tit WHERE rep-tit.ep-codigo = titulo.ep-codigo
 AND rep-tit.cod-estabel = titulo.cod-estabel
 AND rep-tit.cod-esp = titulo.cod-esp
 AND rep-tit.nr-docto = titulo.nr-docto
 AND rep-tit.parcela = titulo.parcela
 AND rep-tit.serie   = titulo.serie OUTER-JOIN NO-LOCK,
      EACH repres OF rep-tit NO-LOCK
    ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "OUTER,,"
     _JoinCode[1]      = "movadm.rep-tit.ep-codigo = movadm.titulo.ep-codigo
 AND movadm.rep-tit.cod-estabel = movadm.titulo.cod-estabel
 AND movadm.rep-tit.cod-esp = movadm.titulo.cod-esp
 AND movadm.rep-tit.nr-docto = movadm.titulo.nr-docto
 AND movadm.rep-tit.parcela = movadm.titulo.parcela
 AND movadm.rep-tit.serie   = movadm.titulo.serie"
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
ON VALUE-CHANGED OF br-table IN FRAME F-Main
DO:
    def buffer moeda   for ems2cadme.moeda.
    
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))).
  
  assign de-vl-liquido   = 0
         de-vl-comissao  = 0
         de-vl-vinculado = 0
         c-descricao     = ""
         c-sigla-1       = ""
         c-sigla-2       = ""
         c-sigla-3       = ""
         fi-pedido-rep   = ""
         fi-pedido-cli   = "".
         
  if  avail titulo and avail rep-tit then do:
 
      FIND param-cr NO-LOCK
          WHERE param-cr.ep-codigo = titulo.ep-codigo
          NO-ERROR.
 
      if  avail param-cr then do:
          run calculo-vl-comissao-vl-vinculado.
      end.
      
      find moeda no-lock
           where moeda.mo-codigo = titulo.mo-codigo no-error.
 
      if  avail moeda then do:
          assign c-sigla-1:screen-value in frame {&FRAME-NAME} = moeda.sigla
                 c-sigla-2:screen-value in frame {&FRAME-NAME} = moeda.sigla
                 c-sigla-3:screen-value in frame {&FRAME-NAME} = moeda.sigla.
      end.
  end.
  if avail titulo then 
     assign fi-pedido-rep = titulo.pedido-rep
            fi-pedido-cli = titulo.nr-pedcli.

  display de-vl-liquido
          de-vl-comissao
          de-vl-vinculado
          fi-pedido-rep
          fi-pedido-cli
          with frame {&frame-name}.

    &if defined(BF_FIN_RAC) &then
        find first param-global no-lock no-error.
        if param-global.modulo-rac = yes then do:
            if avail rep-tit then do:
                assign tg-desativado:screen-value in frame {&FRAME-NAME} = string(rep-tit.desativado).
            end.
        end.
    &endif
    
    /*EPC*/
    ASSIGN THIS-PROCEDURE:PRIVATE-DATA = THIS-PROCEDURE:file-name.
    
    if  valid-handle(widget-handle(c-container)) then do:
        
         /* DPC */
        run pi-retorna-dpc in widget-handle(c-container).
        if  return-value <> ""
        and return-value <> ? then do:                  
            assign c-nom-prog-dpc-mg97 = return-value.
            if  c-nom-prog-dpc-mg97 <> "" then do:
                run value(c-nom-prog-dpc-mg97) (input "AFTER-VALOR-COMIS":U,
                                                input "BROWSER":U,
                                                input this-procedure,
                                                input frame F-Main:handle,
                                                input "rep-tit",
                                                input (if  avail rep-tit then rowid(rep-tit) else ?)).
                
            end.
        end.
         /* APPC */
        run pi-retorna-appc in widget-handle(c-container).
        if  return-value <> ""
        and return-value <> ? then do:                  
            assign c-nom-prog-appc-mg97 = return-value.
            if  c-nom-prog-appc-mg97 <> "" then do:
                run value(c-nom-prog-appc-mg97) (input "AFTER-VALOR-COMIS":U,
                                                input "BROWSER":U,
                                                input this-procedure,
                                                input frame F-Main:handle,
                                                input "rep-tit",
                                                input (if  avail rep-tit then rowid(rep-tit) else ?)).
                
            end.
        end.
        
         /* UPC */
        run pi-retorna-upc in widget-handle(c-container).
        if  return-value <> ""
        and return-value <> ? then do:                  
            assign c-nom-prog-upc-mg97 = return-value.
            if  c-nom-prog-upc-mg97 <> "" then do:
                run value(c-nom-prog-upc-mg97) (input "AFTER-VALOR-COMIS":U,
                                                input "BROWSER":U,
                                                input this-procedure,
                                                input frame F-Main:handle,
                                                input "rep-tit",
                                                input (if  avail rep-tit then rowid(rep-tit) else ?)).
                
            end.
        end.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
 
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
  DEF VAR key-value AS CHAR NO-UNDO.

  DEF VAR Filter-Value AS CHAR NO-UNDO.

  /* Copy 'Filter-Attributes' into local variables. */
  RUN get-attribute ('Filter-Value':U).
  Filter-Value = RETURN-VALUE.
  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'cod-rep':U THEN DO:
       &Scope KEY-PHRASE rep-tit.cod-rep eq INTEGER(key-value)
       {&OPEN-QUERY-{&BROWSE-NAME}}
    END. /* cod-rep */
    OTHERWISE DO:
       &Scope KEY-PHRASE TRUE
       {&OPEN-QUERY-{&BROWSE-NAME}}
    END. /* OTHERWISE...*/
  END CASE.

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
  {src/adm/template/row-list.i "titulo"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "titulo"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calculo-vl-comissao-vl-vinculado B-table-Win 
PROCEDURE calculo-vl-comissao-vl-vinculado :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    assign de-vl-liquido   = 0
           de-vl-vinculado = 0
           de-vl-emissao   = 0
           de-vl-comissao  = 0
           l-calcula       = yes.
  
    &IF DEFINED(BF_FIN_RAC) &THEN     /*--  Para versÑes superiores a 2.03 --*/

        find first param-global no-lock no-error.
        if param-global.modulo-rac = yes then do:
            if rep-tit.desativado = yes then do:
                assign l-calcula = no.
            end.
        end.

        if avail rep-tit then do:
            assign de-vl-liquido = rep-tit.vl-base-calc-comis.
        end.
    &ELSE
        if  param-cr.flag-comis = 1 then do:
            if  titulo.mo-codigo = 0 then do:
                assign de-vl-liquido = titulo.vl-liquido.
            end.
            else do: 
                assign de-vl-liquido = titulo.vl-liquido-me.
            end.
        end.
        else do:
            if  titulo.mo-codigo = 0 then do:
                assign de-vl-liquido = titulo.vl-original.
            end.
            else do:
                assign de-vl-liquido = titulo.vl-original-me.
            end. 
        end.
    &ENDIF 

    if l-calcula = yes then do:
        assign de-vl-comissao = fn_ajust_dec(((de-vl-liquido * rep-tit.comissao) / 100),titulo.mo-codigo).

        if  param-cr.comis-propor = no then do:
            if  titulo.vl-saldo <= 0 then do:
                assign de-vl-vinculado = 0.
            end.
            else do:
                assign de-vl-vinculado = fn_ajust_dec((de-vl-comissao - ((de-vl-comissao * rep-tit.comis-emis)
                                                       / 100)),titulo.mo-codigo).
            end.
        end.
        else do:
            if  titulo.mo-codigo = 0 then do:
                assign de-perc = titulo.vl-saldo / titulo.vl-original.
            end.
            else do:
                assign de-perc = titulo.vl-saldo-me / titulo.vl-original-me.
            end.
            
            assign de-vl-emissao   = if rep-tit.comis-emis = 0 then 0
                                    else fn_ajust_dec(((de-vl-comissao * rep-tit.comis-emis) / 100),titulo.mo-codigo)
                   de-vl-vinculado = fn_ajust_dec(((de-vl-comissao - de-vl-emissao) * de-perc),titulo.mo-codigo).
        end.
        if  de-vl-vinculado < 0 then do:
            assign de-vl-vinculado = 0.
        end.
    end.

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
  
  assign de-vl-liquido   = 0
         de-vl-comissao  = 0
         de-vl-vinculado = 0
         c-descricao     = ""
         c-sigla-1       = ""
         c-sigla-2       = ""
         c-sigla-3       = ""
         fi-pedido-rep   = ""
         fi-pedido-cli   = "".
  
  if  avail titulo then do:
      disp titulo.cod-rep with frame {&frame-name}.
      find first repres 
           where repres.cod-rep = titulo.cod-rep no-lock no-error.
      if  avail repres then
          display repres.nome @ c-descricao with frame {&FRAME-NAME}.
          
      find moeda no-lock
           where moeda.mo-codigo = titulo.mo-codigo no-error.
 
      if  avail moeda then do:
          assign c-sigla-1:screen-value in frame {&FRAME-NAME} = moeda.sigla
                 c-sigla-2:screen-value in frame {&FRAME-NAME} = moeda.sigla
                 c-sigla-3:screen-value in frame {&FRAME-NAME} = moeda.sigla.
      end.
  end.
    &if defined(BF_FIN_RAC) &then
        if param-global.modulo-rac = yes then do:
            if avail rep-tit then do:
                assign tg-desativado:screen-value in frame {&FRAME-NAME} = string(rep-tit.desativado).
            end.
        end.
    &endif
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
    {utp/ut-liter.i B†se_C†lculo * l}
    assign c-sigla-1:label in frame {&FRAME-NAME} = return-value.
   
    {utp/ut-liter.i Pedido_Repres * l}
    assign fi-pedido-rep:label in frame {&FRAME-NAME} = return-value.  
   
    {utp/ut-liter.i Pedido_Cliente * l}
    assign fi-pedido-cli:label in frame {&FRAME-NAME} = return-value.  
   
    {utp/ut-liter.i Valor_Comiss∆o * l}
    assign c-sigla-2:label in frame {&FRAME-NAME} = return-value.
   
    {utp/ut-liter.i Valor_Vinculado * l}
    assign c-sigla-3:label in frame {&FRAME-NAME} = return-value.
    
    {utp/ut-liter.i Representante_Principal * l}
    assign titulo.cod-rep:label in frame {&FRAME-NAME} = return-value.

    &if defined(BF_FIN_RAC) &then
        {utp/ut-liter.i Desativado * l}
        assign tg-desativado:label  in frame {&FRAME-NAME} = return-value.
    &else
        assign tg-desativado:hidden in frame {&FRAME-NAME} = yes.
    &endif

    find first param-global no-lock no-error.

    FIND param-cr NO-LOCK
        WHERE param-cr.ep-codigo = STRING (i-emp-selecao)
        NO-ERROR.
 
     RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query-cases B-table-Win 
PROCEDURE local-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query-cases':U ) .
  
  assign de-vl-liquido   = 0
         de-vl-comissao  = 0
         de-vl-vinculado = 0
         c-descricao     = ""
         c-sigla-1       = ""
         c-sigla-2       = ""
         c-sigla-3       = ""
         fi-pedido-rep   = ""
         fi-pedido-cli   = "".
          
  if  avail titulo and avail rep-tit then do:
      if  avail param-cr then do:
          run calculo-vl-comissao-vl-vinculado.
      end.
  end.
  if avail titulo then 
     assign fi-pedido-rep = titulo.pedido-rep
            fi-pedido-cli = titulo.nr-pedcli.

  display de-vl-liquido
          de-vl-comissao
          de-vl-vinculado
          fi-pedido-rep
          fi-pedido-cli with frame {&frame-name}.
 
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
  {src/adm/template/sndkycas.i "cod-rep" "rep-tit" "cod-rep"}
  {src/adm/template/sndkycas.i "cod-emitente" "rep-tit" "cod-emitente"}
  {src/adm/template/sndkycas.i "cod-esp" "rep-tit" "cod-esp"}
  {src/adm/template/sndkycas.i "cod-estabel" "rep-tit" "cod-estabel"}
  {src/adm/template/sndkycas.i "ep-codigo" "rep-tit" "ep-codigo"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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


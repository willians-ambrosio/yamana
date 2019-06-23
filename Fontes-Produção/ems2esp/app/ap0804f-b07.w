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
{include/i-prgvrs.i AP0804F-B07 2.00.00.002}  /*** 010002 ***/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/browserd.w

/* Parameters Definitions ---                                           */
 
/* Local Variable Definitions ---                                       */

def temp-table tt-documento
    field cod-estabel  like tit-ap.cod-estabel
    field cod-fornec   like tit-ap.cod-fornec
    field nome-abrev   like tit-ap.nome-abrev
    field cod-esp      like tit-ap.cod-esp
    field nr-docto     like tit-ap.nr-docto
    field parcela      like tit-ap.parcela
    field serie        like tit-ap.serie
    field vl-original  like tit-ap.vl-original
    field dt-emissao   like tit-ap.dt-emissao
    field c-tipo       as char.


/* Variaveis usadas internamente pelo estilo, favor nao elimina-las     */

/* v ri veis de uso globla */
def new global shared var v-row-parent as rowid no-undo.

/* vari veis de uso local */
def var v-row-table  as rowid.

/* fim das variaveis utilizadas no estilo */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-nd-nc

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES tit-ap
&Scoped-define FIRST-EXTERNAL-TABLE tit-ap


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR tit-ap.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-documento

/* Definitions for BROWSE br-nd-nc                                      */
&Scoped-define FIELDS-IN-QUERY-br-nd-nc tt-documento.cod-estabel tt-documento.cod-fornec tt-documento.nome-abrev tt-documento.cod-esp tt-documento.nr-docto tt-documento.parcela tt-documento.serie tt-documento.vl-original tt-documento.dt-emissao tt-documento.c-tipo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-nd-nc   
&Scoped-define FIELD-PAIRS-IN-QUERY-br-nd-nc
&Scoped-define SELF-NAME br-nd-nc
&Scoped-define OPEN-QUERY-br-nd-nc RUN pi-criar-tt-documento.  OPEN QUERY {&SELF-NAME} FOR EACH tt-documento Indexed-Reposition.
&Scoped-define TABLES-IN-QUERY-br-nd-nc tt-documento
&Scoped-define FIRST-TABLE-IN-QUERY-br-nd-nc tt-documento


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-nd-nc}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-nd-nc 

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
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-nd-nc FOR 
      tt-documento SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-nd-nc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-nd-nc B-table-Win _FREEFORM
  QUERY br-nd-nc NO-LOCK DISPLAY
      tt-documento.cod-estabel
tt-documento.cod-fornec format ">>>>>>>>>>9"
tt-documento.nome-abrev
tt-documento.cod-esp
tt-documento.nr-docto
tt-documento.parcela
tt-documento.serie
tt-documento.vl-original
tt-documento.dt-emissao
tt-documento.c-tipo format "x(21)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 86.43 BY 9.42.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-nd-nc AT ROW 1 COL 1
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
         HEIGHT             = 9.42
         WIDTH              = 86.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit L-To-R                                       */
/* BROWSE-TAB br-nd-nc 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-nd-nc
/* Query rebuild information for BROWSE br-nd-nc
     _START_FREEFORM
RUN pi-criar-tt-documento.

OPEN QUERY {&SELF-NAME} FOR EACH tt-documento Indexed-Reposition.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-nd-nc */
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

&Scoped-define BROWSE-NAME br-nd-nc
&Scoped-define SELF-NAME br-nd-nc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-nd-nc B-table-Win
ON MOUSE-SELECT-DBLCLICK OF br-nd-nc IN FRAME F-Main
DO:
    RUN New-State("DblClick, SELF":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-nd-nc B-table-Win
ON ROW-ENTRY OF br-nd-nc IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-nd-nc B-table-Win
ON ROW-LEAVE OF br-nd-nc IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-nd-nc B-table-Win
ON VALUE-CHANGED OF br-nd-nc IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  /* run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{utp/ut-liter.i Data_EmissÆo}
assign tt-documento.dt-emissao:label in browse {&browse-name} = trim(return-value).

{utp/ut-liter.i Tipo}
assign tt-documento.c-tipo:label in browse {&browse-name} = return-value.

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-criar-tt-documento B-table-Win 
PROCEDURE pi-criar-tt-documento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def buffer b-tit-ap for tit-ap.
  
  for each tt-documento:
      delete tt-documento.
  end.
  
/************
Na Consulta de uma Nota de D‚bito ou Cr‚dito, 
mostrar os t¡tulos que foram vinculadas as mesmas.
************/

if tit-ap.tipo = 4 or           /* Nota de D‚bito */
   tit-ap.tipo = 5 then do:     /* Nota de Credito  */
   
   /* Encontrar os t¡tulos que as mesmas foram vinculadas */
   
     for each mov-ap where
            mov-ap.ep-codigo   = tit-ap.ep-codigo
       and  mov-ap.cod-estabel = tit-ap.cod-estabel
       and  mov-ap.cod-fornec  = tit-ap.cod-fornec
       and  mov-ap.esp-ant     = tit-ap.cod-esp
       and  mov-ap.docto-ant   = tit-ap.nr-docto
       and  mov-ap.parc-ant    = tit-ap.parcela
       and  mov-ap.serie-ant   = tit-ap.serie no-lock: 
       
     create tt-documento.
     assign tt-documento.cod-estabel    = mov-ap.cod-estabel
            tt-documento.cod-fornec     = mov-ap.cod-fornec
            tt-documento.cod-esp        = mov-ap.cod-esp
            tt-documento.serie          = mov-ap.serie
            tt-documento.nr-docto       = mov-ap.nr-docto
            tt-documento.parcela        = mov-ap.parcela
            tt-documento.nome-abrev     = mov-ap.nome-abrev
            tt-documento.dt-emissao     = mov-ap.dt-emissao
            tt-documento.vl-original    = mov-ap.vl-original
            tt-documento.c-tipo         = {adinc/i01ad103.i 04 mov-ap.tipo}.
   end.
end.       
       
/**********
 Na Consulta de um T¡tulo mostrar as Notas de D‚bito e Cr‚dito 
**********/


if tit-ap.tipo = 1
or tit-ap.tipo = 2 then do:
   for each mov-ap where
            mov-ap.ep-codigo   = tit-ap.ep-codigo
       and  mov-ap.cod-estabel = tit-ap.cod-estabel
       and  mov-ap.cod-fornec  = tit-ap.cod-fornec
       and  mov-ap.cod-esp     = tit-ap.cod-esp
       and  mov-ap.nr-docto    = tit-ap.nr-docto
       and  mov-ap.parcela     = tit-ap.parcela
       and  mov-ap.serie       = tit-ap.serie no-lock : /* no-error.*/
    
    find b-tit-ap where
         b-tit-ap.ep-codigo    = mov-ap.ep-codigo
     and b-tit-ap.cod-estabel  = mov-ap.cod-estabel
     and b-tit-ap.cod-fornec   = mov-ap.cod-fornec
     and b-tit-ap.cod-esp      = mov-ap.esp-ant
     and b-tit-ap.nr-docto     = mov-ap.docto-ant
     and b-tit-ap.parcela      = mov-ap.parc-ant
     and b-tit-ap.serie        = mov-ap.serie-ant
     and (b-tit-ap.tipo = 4 or b-tit-ap.tipo = 5) no-lock no-error.
     
     if avail b-tit-ap then do:
       create tt-documento.
       assign tt-documento.cod-estabel    = b-tit-ap.cod-estabel
              tt-documento.cod-fornec     = b-tit-ap.cod-fornec
              tt-documento.cod-esp        = b-tit-ap.cod-esp
              tt-documento.serie          = b-tit-ap.serie
              tt-documento.nr-docto       = b-tit-ap.nr-docto
              tt-documento.parcela        = b-tit-ap.parcela
              tt-documento.nome-abrev     = b-tit-ap.nome-abrev
              tt-documento.dt-emissao     = b-tit-ap.dt-emissao
              tt-documento.vl-original    = b-tit-ap.vl-original
              tt-documento.c-tipo         = {adinc/i01ad103.i 04 b-tit-ap.tipo}.
     end.
    end.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win _ADM-SEND-RECORDS
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
  {src/adm/template/snd-list.i "tt-documento"}

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
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



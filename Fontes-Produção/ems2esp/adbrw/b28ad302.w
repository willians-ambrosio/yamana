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
def buffer cheque for ems2movme.cheque.
def buffer moeda  for ems2cadme.moeda.

{include/i-prgvrs.i B28AD302 2.00.00.011}  /*** 010011 ***/

&IF "{&EMSFND_VERSION}" >= "1.00"
&THEN
{include/i-license-manager.i B28AD302 MUT}
&ENDIF



/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
/* {utp/ut-glob.i}  */

/* MINFLEXIBILIZA€ÇO */
{cdp/cdcfgfin.i}
{include/i_dbvers.i}
{crp/cr0570.i}
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def var l-pendente         as log  no-undo init yes.
def var l-depositado       as log  no-undo init yes.
def var l-caucao           as log  no-undo init yes.
def var l-compensado       as log  no-undo init yes.
def var l-substituido      as log  no-undo init yes.
def var l-devolvido        as log  no-undo init yes.
def var l-descontado       as log  no-undo init yes.
DEF VAR l-cancelado        AS LOG  NO-UNDO INIT YES.

def var i-tp-situacao      as int                    no-undo.
def var dt-emissao-inicial as date    init today     no-undo.
def var dt-emissao-final   as date    init today     no-undo.
def var c-mo-codigo        as char    format "x(03)" no-undo.
def var c-lista            as char    format "x(10)" column-label "Situa‡Æo do Cheque" no-undo.
def var c-lista-sit        as char                   no-undo.
def buffer b-emitente        for emitente.

def new global shared var gr-cheq as rowid no-undo.
def new shared var g-cliente-abrev as char no-undo.

def temp-table tt-documento no-undo 
    field i-cod-banco       like cheque.cod-banco
    field de-nr-cheque      like cheque.nr-cheque
    field c-agencia         like cheque.agencia
    field c-conta-corren    like cheque.conta-corren
    field de-vl-cheque      like cheque.vl-cheque
    field data-emissao      like cheque.dt-emissao
    field data-vencimento   like cheque.dt-vencimento
    field c-situacao        as char format "x(12)" column-label "Situa‡Æo do Cheque"
    field c-nome            like cheque.nom-emit-cheque
    field gr-cheque         as rowid.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES emitente
&Scoped-define FIRST-EXTERNAL-TABLE emitente


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR emitente.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-documento

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table tt-documento.de-nr-cheque tt-documento.i-cod-banco tt-documento.c-agencia tt-documento.c-conta-corren tt-documento.de-vl-cheque tt-documento.data-emissao tt-documento.c-situacao tt-documento.data-vencimento tt-documento.c-nome   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table   
&Scoped-define SELF-NAME br_table
&Scoped-define OPEN-QUERY-br_table &if "{&mguni_version}" >= "2.02" &then     run pi-cria-tt-documento. &endif.  OPEN QUERY {&SELF-NAME} FOR EACH tt-documento  BY tt-documento.de-nr-cheque.  apply 'value-changed' to br_table in frame {&FRAME-NAME}.
&Scoped-define TABLES-IN-QUERY-br_table tt-documento
&Scoped-define FIRST-TABLE-IN-QUERY-br_table tt-documento


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table RECT-12 RECT-13 bt-filtro 
&Scoped-Define DISPLAYED-OBJECTS c-sigla tot-cheques 

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
DEFINE BUTTON bt-detalhes AUTO-END-KEY 
     LABEL "&Detalhar" 
     SIZE 15 BY 1.

DEFINE BUTTON bt-filtro 
     LABEL "Filtro" 
     SIZE 15 BY 1.

DEFINE VARIABLE c-sigla AS CHARACTER FORMAT "X(3)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 5.86 BY .88 NO-UNDO.

DEFINE VARIABLE tot-cheques AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 21.14 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 1.67.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50.14 BY 1.67.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      tt-documento SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      tt-documento.de-nr-cheque
      tt-documento.i-cod-banco
      tt-documento.c-agencia
      tt-documento.c-conta-corren
      tt-documento.de-vl-cheque
      tt-documento.data-emissao
      tt-documento.c-situacao
      tt-documento.data-vencimento
      tt-documento.c-nome
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 87.43 BY 8.92.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
     bt-filtro AT ROW 10.25 COL 4.29
     c-sigla AT ROW 10.25 COL 57.57 COLON-ALIGNED
     tot-cheques AT ROW 10.25 COL 64.14 COLON-ALIGNED NO-LABEL
     bt-detalhes AT ROW 10.29 COL 20.72
     RECT-12 AT ROW 10.04 COL 1
     RECT-13 AT ROW 10.04 COL 38.29
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
         HEIGHT             = 10.79
         WIDTH              = 87.43.
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
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-detalhes IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-sigla IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN tot-cheques IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
&if "{&mguni_version}" >= "2.02" &then
    run pi-cria-tt-documento.
&endif.

OPEN QUERY {&SELF-NAME} FOR EACH tt-documento  BY tt-documento.de-nr-cheque.

apply 'value-changed' to br_table in frame {&FRAME-NAME}.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE"
     _TblOptList       = ","
     _OrdList          = "movadm.cheque.nr-cheque|yes"
     _JoinCode[1]      = "movadm.cheque.cod-emitente = mgadm.emitente.cod-emitente"
     _Where[1]         = "movadm.cheque.dt-emissao >= dt-emissao-inicial
AND movadm.cheque.dt-emissao <= dt-emissao-final
AND (movadm.cheque.situacao-cheque  = 1 and l-pendente   = yes)
OR  (movadm.cheque.situacao-cheque  = 2 and l-depositado = yes)
OR  ( movadm.cheque.situacao-cheque = 5 and l-devolvido  = yes)"
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

  apply "choose" to bt-detalhes.  
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



    if avail tt-documento then
       assign bt-detalhes:sensitive in frame {&FRAME-NAME} = yes.
    else
       assign bt-detalhes:sensitive in frame {&FRAME-NAME} = no.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-detalhes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-detalhes B-table-Win
ON CHOOSE OF bt-detalhes IN FRAME F-Main /* Detalhar */
DO:  
  if  avail tt-documento then do:
      assign gr-cheq = tt-documento.gr-cheque. 

      &if  defined(BF_FIN_CHEQUES_CRP) &then  
          run crp/cr0770a.w.
      &else
          run cbp/cb0701a.w.          
      &endif
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-filtro B-table-Win
ON CHOOSE OF bt-filtro IN FRAME F-Main /* Filtro */
DO:

  run crp/cr0706e.w   (input-output l-pendente,
                       input-output l-depositado,
                       input-output l-caucao,
                       input-output l-compensado,
                       input-output l-substituido,
                       input-output l-devolvido,
                       input-output l-descontado,
                       INPUT-OUTPUT l-cancelado,
                       input-output dt-emissao-inicial,
                       input-output dt-emissao-final).



  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */


{utp/ut-liter.i Total_Cheques MCR L}
assign c-sigla:label in frame {&FRAME-NAME} = return-value.

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

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .


  find first moeda no-lock
       where moeda.mo-codigo = 0 no-error.

  if  avail moeda then do:
      assign c-sigla = moeda.sigla.
  end.

 disp c-sigla with frame {&FRAME-NAME}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  find first moeda no-lock
       where moeda.mo-codigo = 0 no-error.

  if  avail moeda then
      assign c-sigla:screen-value in frame {&FRAME-NAME} = moeda.sigla.

  c-lista-sit = {adinc/i02ad302.i 03}.   

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

  run pi-totaliza. 



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

    assign tot-cheques = 0.

    for each b-emitente  no-lock where 
             b-emitente.cod-emitente  = emitente.cod-emitente:

        for each  cheque no-lock 
            where cheque.cod-emitente     = b-emitente.cod-emitente
            AND   cheque.ep-codigo-mvto   = i-ep-codigo-usuario
            and   cheque.dt-emissao      >= dt-emissao-inicial
            and   cheque.dt-emissao      <= dt-emissao-final
            and ((cheque.situacao-cheque  = 1 and l-pendente    = yes)
            or   (cheque.situacao-cheque  = 2 and l-depositado  = yes) 
            or   (cheque.situacao-cheque  = 3 and l-caucao      = yes) 
            or   (cheque.situacao-cheque  = 8 and l-compensado  = yes) 
            or   (cheque.situacao-cheque  = 4 and l-substituido = yes) 
            or   (cheque.situacao-cheque  = 5 and l-devolvido   = yes) 
            or   (cheque.situacao-cheque  = 7 and l-descontado  = yes)
            OR   (cheque.situacao-cheque  = 6 AND l-cancelado   = YES 
                  AND l-func-bx-ch-recib   = YES
                  AND i-pais-impto-usuario = 1))
            and   cheque.origem           = 6:

            create tt-documento. 
            assign tt-documento.i-cod-banco       = cheque.cod-banco
                   tt-documento.c-agencia         = cheque.agencia
                   tt-documento.c-conta-corren    = cheque.conta-corren
                   tt-documento.de-nr-cheque      = cheque.nr-cheque
                   tt-documento.de-vl-cheque      = cheque.vl-cheque
                   tt-documento.data-emissao      = cheque.dt-emissao
                   tt-documento.data-vencimento   = cheque.dt-vencimento
                   tt-documento.c-situacao        = {adinc/i02ad302.i 04 cheque.situacao-cheque}
                   tt-documento.c-nome            = cheque.nom-emit-cheque
                   tt-documento.gr-cheque         = rowid(cheque).

            assign tot-cheques = tot-cheques + cheque.vl-cheque.

        end.
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-totaliza B-table-Win 
PROCEDURE pi-totaliza :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:    Calculo do total do browse   
------------------------------------------------------------------------------*/

    /*assign tot-cheques = 0.
 *     assign br_table:REFRESHABLE IN FRAME {&frame-name}= no.
 *     GET FIRST br_table.
 *   
 *     DO WHILE AVAIL tt-documento: /* tabela da query */
 *         assign tot-cheques = tot-cheques + tt-documento.de-vl-cheque.
 *        GET NEXT br_table.
 *     END.
 *     
 *     ASSIGN br_table:REFRESHABLE IN FRAME {&frame-name} = yes.
 *     /* Fim calculo do total do Browse */*/

    display tot-cheques with frame {&FRAME-NAME}.

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
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


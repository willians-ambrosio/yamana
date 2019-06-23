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
{include/i-prgvrs.i B02AD183 2.00.00.015}  /*** 010015 ***/

&IF "{&EMSFND_VERSION}" >= "1.00"
&THEN
{include/i-license-manager.i B02AD183 MUT}
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

def buffer b-mov-tit for mov-tit.
def buffer b-mov-tit-aux for mov-tit.

def temp-table tt-mov-tit like mov-tit
    field c-transacao     as   char format "x(5)"
    field c-lancamento    like mov-tit.lancamento
    field gr-tt-doc       as   rowid.


def var c-transacao        as char    no-undo.
def var c-lista-transacao  as char    no-undo.
def var de-valor-mov       as decimal format "->>>,>>>,>>9.99" no-undo.
def var de-vl-original     as decimal format "->>>,>>>,>>9.99" no-undo.
def var de-vl-juros-rec    as decimal format "->>>,>>>,>>9.99" no-undo.
def var de-vl-antecip      as decimal format "->>>,>>>,>>9.99" no-undo.
def var l-modulo-fc        as logical no-undo.
def var l-habil-cta-contab as logical no-undo. 

def new global shared var v-row-parent as rowid no-undo.
def new global shared var gr-mov-tit   as rowid no-undo.

def var v-row-table  as rowid.

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
&Scoped-define EXTERNAL-TABLES titulo
&Scoped-define FIRST-EXTERNAL-TABLE titulo


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR titulo.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-mov-tit

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table entry(tt-mov-tit.transacao, c-lista-transacao) @ c-transacao tt-mov-tit.dt-trans tt-mov-tit.dt-vencimen if transacao = 15 then 0 else fn-valor-mov() @ de-valor-mov if transacao <> 15 then 0 else fn-vl-original() @ de-vl-original if transacao <> 15 then 0 else fn-vl-juros-rec() @ de-vl-juros-rec   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table   
&Scoped-define FIELD-PAIRS-IN-QUERY-br-table
&Scoped-define SELF-NAME br-table
&Scoped-define OPEN-QUERY-br-table run pi-criar-tt-mov-tit.  OPEN QUERY {&SELF-NAME}    FOR EACH tt-mov-tit WHERE tt-mov-tit.transacao  <> 16                        AND   l-modulo-fc = yes                        NO-LOCK                        BY tt-mov-tit.dt-today                        BY tt-mov-tit.c-time.
&Scoped-define TABLES-IN-QUERY-br-table tt-mov-tit
&Scoped-define FIRST-TABLE-IN-QUERY-br-table tt-mov-tit


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-table bt-contas-contabeis 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-valor-ant B-table-Win 
FUNCTION fn-valor-ant RETURNS DECIMAL
    ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-valor-mov B-table-Win 
FUNCTION fn-valor-mov RETURNS DECIMAL
    ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-vl-juros-rec B-table-Win 
FUNCTION fn-vl-juros-rec RETURNS DECIMAL
    ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-vl-original B-table-Win 
FUNCTION fn-vl-original RETURNS DECIMAL
    ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-contas-contabeis 
     LABEL "Contas Cont beis" 
     SIZE 20 BY 1.17.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      tt-mov-tit SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _FREEFORM
  QUERY br-table NO-LOCK DISPLAY
      entry(tt-mov-tit.transacao, c-lista-transacao) @ c-transacao FORMAT "x(3)"
      tt-mov-tit.dt-trans
      tt-mov-tit.dt-vencimen
      if transacao  = 15 then 0 else fn-valor-mov() @ de-valor-mov 
      if transacao <> 15 then 0 else fn-vl-original() @ de-vl-original
      if transacao <> 15 then 0 else fn-vl-juros-rec()  @ de-vl-juros-rec
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 82 BY 8.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-table AT ROW 1 COL 1
     bt-contas-contabeis AT ROW 9.58 COL 1
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
         HEIGHT             = 9.83
         WIDTH              = 82.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit L-To-R                                       */
/* BROWSE-TAB br-table 1 F-Main */
ASSIGN        FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _START_FREEFORM
run pi-criar-tt-mov-tit.

OPEN QUERY {&SELF-NAME}
   FOR EACH tt-mov-tit WHERE tt-mov-tit.transacao  <> 16
                       AND   l-modulo-fc = yes
                       NO-LOCK
                       BY tt-mov-tit.dt-today
                       BY tt-mov-tit.c-time.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE"
     _OrdList          = "movadm.mov-tit.dt-today|yes,movadm.mov-tit.c-time|yes"
     _JoinCode[1]      = "    movadm.mov-tit.ep-codigo   = movadm.titulo.ep-codigo
AND movadm.mov-tit.cod-estabel = movadm.titulo.cod-estabel
AND movadm.mov-tit.cod-esp     = movadm.titulo.cod-esp
AND movadm.mov-tit.serie       = movadm.titulo.serie
AND movadm.mov-tit.nr-docto    = movadm.titulo.nr-docto
AND movadm.mov-tit.parcela     = movadm.titulo.parcela
AND movadm.mov-tit.transacao  <> 16 
AND l-modulo-fc = yes
"
     _Query            is OPENED
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
  /* run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))). */
  assign bt-contas-contabeis:sensitive in frame {&FRAME-NAME} = yes.
  if avail tt-mov-tit then do:
    if tt-mov-tit.transacao =  7 or  /* Abatimento */
       tt-mov-tit.transacao =  8 or  /* Cancelamento */
       tt-mov-tit.transacao =  9 or  /* Pedido de Devolu‡Æo */
       tt-mov-tit.transacao = 10 or  /* Pedido de Baixa */
       tt-mov-tit.transacao = 11 or  /* Protesto */
       tt-mov-tit.transacao = 12 or  /* Susta‡Æo de Protesto */
       tt-mov-tit.tipo      =  3 or  /* previsÆo */
       tt-mov-tit.tipo      =  4 or  /* Nota de D‚bito */
       tt-mov-tit.tipo      =  5 or  /* Nota de Cr‚dito */
      (tt-mov-tit.tipo      =  6 and
       tt-mov-tit.transacao <> 2) or /* Docto Juros com transacao <> de baixa */
      (tt-mov-tit.flag-contab = no and l-habil-cta-contab = no)  /* esp‚cie nÆo contabiliza */
       then do:
       assign bt-contas-contabeis:sensitive in frame {&FRAME-NAME} = no.
    end.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-contas-contabeis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-contas-contabeis B-table-Win
ON CHOOSE OF bt-contas-contabeis IN FRAME F-Main /* Contas Cont beis */
DO:
  if  avail tt-mov-tit then do:
      assign gr-mov-tit = tt-mov-tit.gr-tt-doc.
      run crp/cr0709a.w.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

{utp/ut-liter.i Trans * R}
assign c-transacao:label in browse {&browse-name} = trim(return-value).
&if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
    DEFINE VARIABLE cAuxTraducao001 AS CHARACTER NO-UNDO.
    ASSIGN cAuxTraducao001 = {adinc/i02ad166.i 03}.
    RUN utp/ut-list.p (INPUT-OUTPUT cAuxTraducao001).
    ASSIGN  c-lista-transacao = cAuxTraducao001.
&else
    ASSIGN c-lista-transacao = {adinc/i02ad166.i 03}.
&endif

{utp/ut-liter.i Data_Transa‡Æo * R}
assign tt-mov-tit.dt-trans:label in browse {&browse-name} = trim(return-value).

{utp/ut-liter.i Data_Vencimento * R}
assign tt-mov-tit.dt-vencimen:label in browse {&browse-name} = trim(return-value).

{utp/ut-liter.i Valor_Movto * R}
assign de-valor-mov:label in browse {&browse-name} = trim(return-value).

{utp/ut-liter.i Efeito_M ximo * R}
assign de-vl-original:label in browse {&browse-name} = trim(return-value).

{utp/ut-liter.i Ganho_&_Perda * R}
assign de-vl-juros-rec:label in browse {&browse-name} = trim(return-value).

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-val-saldos-moeda-fasb B-table-Win 
PROCEDURE calc-val-saldos-moeda-fasb :
/*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
------------------------------------------------------------------------------*/

    assign de-valor-mov    = 0
           de-vl-original  = 0
           de-vl-juros-rec = 0.

    if  avail mov-tit then do:

        if  mov-tit.transacao = 14 or 
            mov-tit.transacao = 4  then 
            assign de-valor-mov = mov-tit.vl-orig-fasb.
        else 
           if  mov-tit.transacao = 2  or 
               mov-tit.transacao = 21 or
               mov-tit.transacao = 22 or
               mov-tit.transacao = 23 or
               mov-tit.transacao = 3  or 
               mov-tit.transacao = 13 then 
               assign de-valor-mov = mov-tit.vl-bai-fasb. 

        if  mov-tit.transacao = 2 then do:
            find first b-mov-tit
                 where b-mov-tit.ep-codigo   = mov-tit.ep-codigo
                 and   b-mov-tit.cod-estabel = mov-tit.cod-estabel
                 and   b-mov-tit.cod-esp     = mov-tit.cod-esp
                 and   b-mov-tit.serie       = mov-tit.serie
                 and   b-mov-tit.nr-docto    = mov-tit.nr-docto
                 and   b-mov-tit.parcela     = mov-tit.parcela
                 and   b-mov-tit.dt-trans    = mov-tit.dt-trans
                 and   b-mov-tit.transacao   = 15 no-lock no-error.
            if  avail b-mov-tit then do:
                if  titulo.mo-codigo = 0 then do:
                    assign de-vl-original  = b-mov-tit.vl-original
                           de-vl-juros-rec = b-mov-tit.vl-juros-rec.
                end.
                else do:
                    assign de-vl-original  = b-mov-tit.vl-original-me
                           de-vl-juros-rec = b-mov-tit.vl-juros-rec-me.
                end. 
            end.
        end.   
    end.

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
      if  param-global.modulo-fc = yes then do:
          assign l-modulo-fc = yes.
      end.
  end.

  find first funcao
       where funcao.cd-funcao = "spp-habilita-cta-contab":U no-lock no-error.
  if avail funcao then
      assign l-habil-cta-contab = yes.
  else
      assign l-habil-cta-contab = no.

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-criar-tt-mov-tit B-table-Win 
PROCEDURE pi-criar-tt-mov-tit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    for each tt-mov-tit:
        delete tt-mov-tit.
    end.   

    find esp-doc where esp-doc.cod-esp = titulo.cod-esp no-lock no-error.       

    for each  mov-tit
        where mov-tit.ep-codigo    = titulo.ep-codigo 
        and   mov-tit.cod-estabel  = titulo.cod-estabel
        and   mov-tit.cod-esp      = titulo.cod-esp
        and   mov-tit.serie        = titulo.serie
        and   mov-tit.nr-docto     = titulo.nr-docto
        and   mov-tit.parcela      = titulo.parcela
        no-lock:

        if  avail esp-doc and esp-doc.tipo = 2 then do:

            for each  b-mov-tit-aux use-index ch-antecip
                where b-mov-tit-aux.ep-codigo     = mov-tit.ep-codigo 
                and   b-mov-tit-aux.cod-estabel   = mov-tit.cod-estabel
                and   b-mov-tit-aux.esp-antecip   = mov-tit.cod-esp
                and   b-mov-tit-aux.serie-antecip = mov-tit.serie
                and   b-mov-tit-aux.doc-antecip   = mov-tit.nr-docto
                and   b-mov-tit-aux.parc-antecip  = mov-tit.parcela
                no-lock:

                find first tt-mov-tit 
                     where tt-mov-tit.num-id-mov-tit  = b-mov-tit-aux.num-id-mov-tit no-error.

                if  not avail tt-mov-tit then do:
                    create tt-mov-tit.
                    assign tt-mov-tit.transacao         = b-mov-tit-aux.transacao
                           tt-mov-tit.c-transacao       = entry(b-mov-tit-aux.transacao, c-lista-transacao)
                           tt-mov-tit.lancamento        = b-mov-tit-aux.lancamento
                           tt-mov-tit.dt-trans          = b-mov-tit-aux.dt-trans
                           tt-mov-tit.dt-vencimen       = b-mov-tit-aux.dt-vencimen
                           tt-mov-tit.vl-juros-rec      = b-mov-tit-aux.vl-juros-rec
                           tt-mov-tit.vl-juros-rec-me   = b-mov-tit-aux.vl-juros-rec-me
                           tt-mov-tit.vl-original       = b-mov-tit-aux.vl-original
                           tt-mov-tit.vl-orig-fasb      = b-mov-tit-aux.vl-orig-fasb
                           tt-mov-tit.vl-bai-fasb       = b-mov-tit-aux.vl-bai-fasb
                           tt-mov-tit.vl-ant-fasb       = b-mov-tit-aux.vl-ant-fasb
                           tt-mov-tit.vl-antecip        = b-mov-tit-aux.vl-antecip
                           tt-mov-tit.vl-antecip-me     = b-mov-tit-aux.vl-antecip-me                           
                           tt-mov-tit.ep-codigo         = b-mov-tit-aux.ep-codigo
                           tt-mov-tit.serie             = b-mov-tit-aux.serie
                           tt-mov-tit.cod-esp           = b-mov-tit-aux.cod-esp
                           tt-mov-tit.cod-estabel       = b-mov-tit-aux.cod-estabel
                           tt-mov-tit.nr-docto          = b-mov-tit-aux.nr-docto
                           tt-mov-tit.parcela           = b-mov-tit-aux.parcela
                           tt-mov-tit.serie-antecip     = b-mov-tit-aux.serie
                           tt-mov-tit.esp-antecip       = b-mov-tit-aux.cod-esp
                           tt-mov-tit.doc-antecip       = b-mov-tit-aux.nr-docto
                           tt-mov-tit.parc-antecip      = b-mov-tit-aux.parcela
                           tt-mov-tit.num-id-mov-tit    = b-mov-tit-aux.num-id-mov-tit
                           tt-mov-tit.flag-contab       = b-mov-tit-aux.flag-contab
                           tt-mov-tit.tipo              = b-mov-tit-aux.tipo
                           tt-mov-tit.gr-tt-doc         = rowid(b-mov-tit-aux)
                           tt-mov-tit.dt-today          = b-mov-tit-aux.dt-today
                           tt-mov-tit.c-time            = b-mov-tit-aux.c-time
                           tt-mov-tit.vl-liquido        = b-mov-tit-aux.vl-liquido.
                end.      
            end.
        end.
        find first tt-mov-tit where tt-mov-tit.num-id-mov-tit  = mov-tit.num-id-mov-tit no-error.
        if  not avail tt-mov-tit then do:           
            create tt-mov-tit.
            assign tt-mov-tit.transacao         = mov-tit.transacao
                   tt-mov-tit.c-transacao       = entry(mov-tit.transacao, c-lista-transacao)
                   tt-mov-tit.lancamento        = mov-tit.lancamento
                   tt-mov-tit.dt-trans          = mov-tit.dt-trans
                   tt-mov-tit.dt-vencimen       = mov-tit.dt-vencimen
                   tt-mov-tit.vl-juros-rec      = mov-tit.vl-juros-rec
                   tt-mov-tit.vl-juros-rec-me   = mov-tit.vl-juros-rec-me
                   tt-mov-tit.vl-original       = mov-tit.vl-original
                   tt-mov-tit.vl-orig-fasb      = mov-tit.vl-orig-fasb
                   tt-mov-tit.vl-bai-fasb       = mov-tit.vl-bai-fasb
                   tt-mov-tit.vl-ant-fasb       = mov-tit.vl-ant-fasb
                   tt-mov-tit.vl-antecip        = mov-tit.vl-antecip
                   tt-mov-tit.vl-antecip-me     = mov-tit.vl-antecip-me
                   tt-mov-tit.ep-codigo         = mov-tit.ep-codigo
                   tt-mov-tit.serie             = mov-tit.serie
                   tt-mov-tit.cod-esp           = mov-tit.cod-esp
                   tt-mov-tit.cod-estabel       = mov-tit.cod-estabel                   
                   tt-mov-tit.nr-docto          = mov-tit.nr-docto
                   tt-mov-tit.parcela           = mov-tit.parcela
                   tt-mov-tit.serie-antecip     = mov-tit.serie-antecip
                   tt-mov-tit.esp-antecip       = mov-tit.esp-antecip
                   tt-mov-tit.doc-antecip       = mov-tit.doc-antecip
                   tt-mov-tit.parc-antecip      = mov-tit.parc-antecip
                   tt-mov-tit.flag-contab       = mov-tit.flag-contab
                   tt-mov-tit.tipo              = mov-tit.tipo
                   tt-mov-tit.num-id-mov-tit    = mov-tit.num-id-mov-tit
                   tt-mov-tit.gr-tt-doc         = rowid(mov-tit)
                   tt-mov-tit.dt-today          = mov-tit.dt-today
                   tt-mov-tit.c-time            = mov-tit.c-time
                   tt-mov-tit.vl-liquido        = mov-tit.vl-liquido.

            /* A logica abaixo foi implementada pela localizacao
               argentina. Conceito de titulos em garantia, que
               ainda nao existe no produto padrao */
            if mov-tit.modalidade = 3 /* Caucao */ and
               mov-tit.referencia begins "GAR" then 
               assign tt-mov-tit.c-transacao = "GAR".

        end.       
    end. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-fasb B-table-Win 
PROCEDURE pi-fasb :
/*------------------------------------------------------------------------------
    Purpose:     
    Parameters:  <none>
    Notes:       
------------------------------------------------------------------------------*/

    assign de-valor-mov    = 0
           de-vl-original  = 0
           de-vl-juros-rec = 0.

    if  avail tt-mov-tit and avail titulo then do:

        if  tt-mov-tit.transacao = 14 or tt-mov-tit.transacao = 4 then do:
            assign de-valor-mov = tt-mov-tit.vl-orig-fasb.
        end.
        else do:
            if  tt-mov-tit.transacao = 2
            or  tt-mov-tit.transacao = 21
            or  tt-mov-tit.transacao = 22
            or  tt-mov-tit.transacao = 23
            or  tt-mov-tit.transacao = 3
            or  tt-mov-tit.transacao = 13 then do:
                assign de-valor-mov  = tt-mov-tit.vl-bai-fasb
                       de-vl-antecip = tt-mov-tit.vl-ant-fasb.
            end.
        end.

        if  tt-mov-tit.transacao = 2 then do:
            find first b-mov-tit
                 where b-mov-tit.ep-codigo   = tt-mov-tit.ep-codigo
                 and   b-mov-tit.cod-estabel = tt-mov-tit.cod-estabel
                 and   b-mov-tit.cod-esp     = tt-mov-tit.cod-esp
                 and   b-mov-tit.serie       = tt-mov-tit.serie
                 and   b-mov-tit.nr-docto    = tt-mov-tit.nr-docto
                 and   b-mov-tit.parcela     = tt-mov-tit.parcela
                 and   b-mov-tit.dt-trans    = tt-mov-tit.dt-trans
                 and   b-mov-tit.transacao   = 15 
                 no-lock no-error.

            if  avail b-mov-tit then do:
                assign de-vl-original  = b-mov-tit.vl-original
                       de-vl-juros-rec = b-mov-tit.vl-juros-rec.
            end.
            else do:
                assign de-vl-original  = 0
                       de-vl-juros-rec = 0.
            end.
        end.
        else do:
            if tt-mov-tit.transacao = 15 then 
               assign de-vl-original  = tt-mov-tit.vl-original
                      de-vl-juros-rec = tt-mov-tit.vl-juros-rec.
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
  apply 'value-changed' to br-table in frame {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-valor-ant B-table-Win 
FUNCTION fn-valor-ant RETURNS DECIMAL
    () :
/*------------------------------------------------------------------------------
    Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    run pi-fasb.

    RETURN de-vl-antecip.

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

    run pi-fasb.

    RETURN de-valor-mov.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-vl-juros-rec B-table-Win 
FUNCTION fn-vl-juros-rec RETURNS DECIMAL
    () :
/*------------------------------------------------------------------------------
    Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    run pi-fasb.

    RETURN de-vl-juros-rec.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-vl-original B-table-Win 
FUNCTION fn-vl-original RETURNS DECIMAL
    () :
/*------------------------------------------------------------------------------
    Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    run pi-fasb.

    RETURN de-vl-original.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



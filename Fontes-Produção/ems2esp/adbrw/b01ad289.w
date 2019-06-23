&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          mgdis            PROGRESS
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
{include/i-prgvrs.i B01AD289 2.00.00.014}  /*** 010014 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i b01ad289 MUT}
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
 
 
/* Local Variable Definitions ---                                       */
define var c-formato                as character no-undo format "x(17)".
define var c-conta-tax              as character no-undo format "x(17)".
define var c-conta-retencao         as character no-undo format "x(17)".
define var c-conta-percepcao        as character no-undo format "x(17)".
define var c-conta-debito           as character no-undo format "x(17)".
 
define var c-tipo                   as character no-undo.
define var c-lista-tipo             as character no-undo.
 
define var c-lanc                   as character no-undo.
define var c-lista-lanc             as character no-undo.
 
define var c-origem                 as character no-undo.
define var c-lista-origem           as character no-undo.
 
define var c-tipo-calculo           as character no-undo.
define var c-lista-tipo-calculo     as character no-undo.

define buffer b-titulo for titulo.

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
&Scoped-define INTERNAL-TABLES impto-tit-cr tipo-tax

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table impto-tit-cr.cod-imposto ~
tipo-tax.descricao impto-tit-cr.mo-codigo impto-tit-cr.cotacao-dia ~
entry(impto-tit-cr.lancamento, c-lista-lanc) @ c-lanc ~
impto-tit-cr.vl-base-me impto-tit-cr.vl-imposto-me ~
impto-tit-cr.perc-imposto ~
string(impto-tit-cr.conta-imposto, c-formato) @ c-conta-tax ~
string(tipo-tax.char-1, c-formato) @ c-conta-debito ~
impto-tit-cr.vl-percepcao-me impto-tit-cr.perc-percepcao ~
string(impto-tit-cr.conta-percepcao, c-formato) @ c-conta-percepcao ~
impto-tit-cr.vl-retencao-me impto-tit-cr.perc-retencao ~
string(impto-tit-cr.conta-retencao, c-formato) @ c-conta-retencao ~
impto-tit-cr.contabilizou impto-tit-cr.dt-emissao impto-tit-cr.dt-transacao ~
entry(impto-tit-cr.tipo, c-lista-tipo) @ c-tipo ~
entry(impto-tit-cr.ind-tip-calculo, c-lista-tipo-calculo) @ c-tipo-calculo ~
entry(impto-tit-cr.origem-impto, c-lista-origem) @ c-origem ~
impto-tit-cr.usuario impto-tit-cr.dt-today impto-tit-cr.c-time ~
impto-tit-cr.obs 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table 
&Scoped-define FIELD-PAIRS-IN-QUERY-br-table
&Scoped-define OPEN-QUERY-br-table OPEN QUERY br-table FOR EACH impto-tit-cr WHERE impto-tit-cr.ep-codigo = titulo.ep-codigo ~
  AND impto-tit-cr.cod-estabel = titulo.cod-estabel ~
  AND impto-tit-cr.cod-esp = titulo.cod-esp ~
  AND impto-tit-cr.serie = titulo.serie ~
  AND impto-tit-cr.nr-docto = titulo.nr-docto ~
  AND impto-tit-cr.parcela = titulo.parcela NO-LOCK, ~
      EACH tipo-tax WHERE tipo-tax.cod-tax = impto-tit-cr.cod-imposto ~
      AND tipo-tax.cod-tax = impto-tit-cr.cod-imposto NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-table impto-tit-cr tipo-tax
&Scoped-define FIRST-TABLE-IN-QUERY-br-table impto-tit-cr


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-table bt-retencoes bt-contas 

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
DEFINE BUTTON bt-contas 
     LABEL "" 
     SIZE 18 BY 1.

DEFINE BUTTON bt-retencoes 
     LABEL "" 
     SIZE 18 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      impto-tit-cr, 
      tipo-tax SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _STRUCTURED
  QUERY br-table NO-LOCK DISPLAY
      impto-tit-cr.cod-imposto
      tipo-tax.descricao FORMAT "x(35)"
      impto-tit-cr.mo-codigo
      impto-tit-cr.cotacao-dia
      entry(impto-tit-cr.lancamento, c-lista-lanc) @ c-lanc FORMAT "x(03)"
      impto-tit-cr.vl-base-me
      impto-tit-cr.vl-imposto-me
      impto-tit-cr.perc-imposto
      string(impto-tit-cr.conta-imposto, c-formato) @ c-conta-tax
      string(tipo-tax.char-1, c-formato) @ c-conta-debito
      impto-tit-cr.vl-percepcao-me
      impto-tit-cr.perc-percepcao
      string(impto-tit-cr.conta-percepcao, c-formato) @ c-conta-percepcao
      impto-tit-cr.vl-retencao-me
      impto-tit-cr.perc-retencao
      string(impto-tit-cr.conta-retencao, c-formato) @ c-conta-retencao
      impto-tit-cr.contabilizou
      impto-tit-cr.dt-emissao
      impto-tit-cr.dt-transacao
      entry(impto-tit-cr.tipo, c-lista-tipo) @ c-tipo FORMAT "x(30)"
      entry(impto-tit-cr.ind-tip-calculo, c-lista-tipo-calculo) @ c-tipo-calculo FORMAT "x(25)"
      entry(impto-tit-cr.origem-impto, c-lista-origem) @ c-origem FORMAT "x(32)"
      impto-tit-cr.usuario
      impto-tit-cr.dt-today
      impto-tit-cr.c-time
      impto-tit-cr.obs
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 81 BY 8.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-table AT ROW 1 COL 1
     bt-retencoes AT ROW 9.63 COL 1
     bt-contas AT ROW 9.63 COL 19.43
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
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 9.88
         WIDTH              = 81.
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

ASSIGN 
       br-table:NUM-LOCKED-COLUMNS IN FRAME F-Main = 1.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _TblList          = "movadm.impto-tit-cr WHERE movadm.titulo ...,mgdis.tipo-tax WHERE movadm.impto-tit-cr ..."
     _Options          = "NO-LOCK KEY-PHRASE"
     _TblOptList       = ","
     _JoinCode[1]      = "movadm.impto-tit-cr.ep-codigo = movadm.titulo.ep-codigo
  AND movadm.impto-tit-cr.cod-estabel = movadm.titulo.cod-estabel
  AND movadm.impto-tit-cr.cod-esp = movadm.titulo.cod-esp
  AND movadm.impto-tit-cr.serie = movadm.titulo.serie
  AND movadm.impto-tit-cr.nr-docto = movadm.titulo.nr-docto
  AND movadm.impto-tit-cr.parcela = movadm.titulo.parcela"
     _JoinCode[2]      = "mgdis.tipo-tax.cod-tax = movadm.impto-tit-cr.cod-imposto"
     _Where[2]         = "mgdis.tipo-tax.cod-tax = impto-tit-cr.cod-imposto"
     _FldNameList[1]   = movadm.impto-tit-cr.cod-imposto
     _FldNameList[2]   > mgdis.tipo-tax.descricao
"tipo-tax.descricao" ? "x(35)" "character" ? ? ? ? ? ? no ?
     _FldNameList[3]   = movadm.impto-tit-cr.mo-codigo
     _FldNameList[4]   = movadm.impto-tit-cr.cotacao-dia
     _FldNameList[5]   > "_<CALC>"
"entry(impto-tit-cr.lancamento, c-lista-lanc) @ c-lanc" ? "x(03)" ? ? ? ? ? ? ? no ?
     _FldNameList[6]   = movadm.impto-tit-cr.vl-base-me
     _FldNameList[7]   = movadm.impto-tit-cr.vl-imposto-me
     _FldNameList[8]   = movadm.impto-tit-cr.perc-imposto
     _FldNameList[9]   > "_<CALC>"
"string(impto-tit-cr.conta-imposto, c-formato) @ c-conta-tax" ? ? ? ? ? ? ? ? ? no ?
     _FldNameList[10]   > "_<CALC>"
"string(tipo-tax.char-1, c-formato) @ c-conta-debito" ? ? ? ? ? ? ? ? ? no ?
     _FldNameList[11]   = movadm.impto-tit-cr.vl-percepcao-me
     _FldNameList[12]   = movadm.impto-tit-cr.perc-percepcao
     _FldNameList[13]   > "_<CALC>"
"string(impto-tit-cr.conta-percepcao, c-formato) @ c-conta-percepcao" ? ? ? ? ? ? ? ? ? no ?
     _FldNameList[14]   = movadm.impto-tit-cr.vl-retencao-me
     _FldNameList[15]   = movadm.impto-tit-cr.perc-retencao
     _FldNameList[16]   > "_<CALC>"
"string(impto-tit-cr.conta-retencao, c-formato) @ c-conta-retencao" ? ? ? ? ? ? ? ? ? no ?
     _FldNameList[17]   = movadm.impto-tit-cr.contabilizou
     _FldNameList[18]   = movadm.impto-tit-cr.dt-emissao
     _FldNameList[19]   = movadm.impto-tit-cr.dt-transacao
     _FldNameList[20]   > "_<CALC>"
"entry(impto-tit-cr.tipo, c-lista-tipo) @ c-tipo" ? "x(30)" ? ? ? ? ? ? ? no ?
     _FldNameList[21]   > "_<CALC>"
"entry(impto-tit-cr.ind-tip-calculo, c-lista-tipo-calculo) @ c-tipo-calculo" ? "x(25)" ? ? ? ? ? ? ? no ?
     _FldNameList[22]   > "_<CALC>"
"entry(impto-tit-cr.origem-impto, c-lista-origem) @ c-origem" ? "x(32)" ? ? ? ? ? ? ? no ?
     _FldNameList[23]   = movadm.impto-tit-cr.usuario
     _FldNameList[24]   = movadm.impto-tit-cr.dt-today
     _FldNameList[25]   = movadm.impto-tit-cr.c-time
     _FldNameList[26]   = movadm.impto-tit-cr.obs
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

if avail impto-tit-cr then do:
   if  impto-tit-cr.origem-impto = 13 then
       assign bt-contas:sensitive in frame {&FRAME-NAME} = no.
   else
       assign bt-contas:sensitive in frame {&FRAME-NAME} = yes.
end.
else do:
  find first impto-tit-cr of titulo no-lock no-error.  
  if  not avail impto-tit-cr then
      assign bt-contas:sensitive in frame {&FRAME-NAME} = no.
  else do: 
      if  impto-tit-cr.origem-impto = 13 then
          assign bt-contas:sensitive in frame {&FRAME-NAME} = no.
      else
          assign bt-contas:sensitive in frame {&FRAME-NAME} = yes.
  end.
end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-contas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-contas B-table-Win
ON CHOOSE OF bt-contas IN FRAME F-Main
DO:
 
     if  avail impto-tit-cr then do:
         run crp/cr0709e.w (input rowid(impto-tit-cr)).
     end.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-retencoes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-retencoes B-table-Win
ON CHOOSE OF bt-retencoes IN FRAME F-Main
DO:
    if  avail titulo then do:
        run crp/cr0709b.w (Input rowid(titulo)).
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
 
find first param-global no-lock no-error.
assign c-formato = param-global.formato-conta-contabil.
 
{utp/ut-liter.i Conta_Cr‚dito_Imposto}
assign c-conta-tax:label in browse br-table = return-value.
 
{utp/ut-field.i mgadm impto-tit-cr obs 1}
assign impto-tit-cr.obs:label in browse br-table = return-value.
 
{utp/ut-liter.i Conta_D‚bito_Imposto}
assign c-conta-debito:label in browse br-table = return-value.
 
{utp/ut-field.i mgadm impto-tit-cr conta-retencao 1}
assign c-conta-retencao:label in browse br-table = return-value.
 
{utp/ut-field.i mgadm impto-tit-cr conta-percepcao 1}
assign c-conta-percepcao:label in browse br-table = return-value.
 
{utp/ut-field.i mgadm impto-tit-ap origem-impto 1}
assign c-origem:label in browse br-table = trim(return-value).
 
{utp/ut-liter.i Tipo_C lculo * R}
assign c-tipo-calculo:label in browse br-table = trim(return-value).
 
{utp/ut-liter.i LC * C}
assign c-lanc:label in browse br-table = trim(return-value).
 
assign c-lista-lanc         = {adinc/i01ad059.i 03}
       c-lista-tipo         = {diinc/i01di217.i 03}
       c-lista-origem       = {adinc/i01ad290.i 03}
       c-lista-tipo-calculo = {diinc/i05di217.i 03}.
 
{utp/ut-field.i mgadm impto-tit-cr tipo 1}
assign c-tipo:label in browse br-table = return-value.
 
{utp/ut-liter.i Reten‡äes * C}
assign bt-retencoes:label in frame {&FRAME-NAME} = trim(return-value).
 
{utp/ut-liter.i Contas_Cont beis * C}
assign bt-contas:label in frame {&FRAME-NAME} = trim(return-value).
 
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
 
  /* Code placed here will execute PRIOR to standard behavior. */
 
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
 
  /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  assign bt-retencoes:sensitive in frame {&FRAME-NAME} = no.
  if avail titulo then 
     for each mov-tit where 
              mov-tit.ep-codigo     = titulo.ep-codigo   and
              mov-tit.cod-estabel   = titulo.cod-estabel and
              mov-tit.esp-antecip   = titulo.cod-esp     and
              mov-tit.serie-antecip = titulo.serie       and
              mov-tit.doc-antecip   = titulo.nr-docto    and
              mov-tit.parc-antecip  = titulo.parcela no-lock:

        find b-titulo of mov-tit where
             b-titulo.tipo = 8 no-lock no-error.  
  
        if avail b-titulo then do:
           assign bt-retencoes:sensitive in frame {&FRAME-NAME} = yes.
           leave.
        end.   
     end. 
     
  find impto-tit-cr of titulo no-lock no-error.
     
  if  not avail impto-tit-cr then
      assign bt-contas:sensitive in frame {&FRAME-NAME} = no.
  else do: 
      if  impto-tit-cr.origem-impto = 13 then
          assign bt-contas:sensitive in frame {&FRAME-NAME} = no.
      else
          assign bt-contas:sensitive in frame {&FRAME-NAME} = yes.
  end.
     
  apply 'value-changed':U to {&browse-name} in frame {&frame-name}.  
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-add-browse B-table-Win 
PROCEDURE pi-add-browse :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  def input parameter rw-row-1 as rowid no-undo.
  def input parameter rw-row-2 as rowid no-undo.
 
/*
 
  find <tabela-1> where rowid(<tabela-1>) = rw-row-1 no-lock no-error.
  find <tabela-2> where rowid(<tabela-2>) = rw-row-2 no-lock no-error.
  find <tabela-formacao> where <tabela-formacao>.<campo-1> = <tabela-1>.<campo-1> and
                               <tabela-formacao>.<campo-2> = <tabela-2>.<campo-2> no-error.
  if not avail <tabela-formacao> then do:  
    create <tabela-formacao>.
    assign <tabela-formacao>.<campo-1> = <tabela-1>.<campo-1>
           <tabela-formacao>.<campo-2> = <tabela-2>.<campo-2>.
  end.
 
*/
 
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



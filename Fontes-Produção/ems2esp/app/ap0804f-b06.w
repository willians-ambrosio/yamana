&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgdis            PROGRESS
          movadm           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i AP0804F-B06 2.00.00.002}  /*** 010002 ***/
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 


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
&glob ORIGINALNAME 'adbrw\b03ad293.w'

def buffer b-tit-ap for tit-ap.

def new global shared var gr-impto-tit-ap as rowid no-undo.

def buffer b-impto-tit-ap for impto-tit-ap.

def var c-origem             as character format "x(25)"          no-undo.
def var c-formato            as character                         no-undo.
def var c-conta-imposto      like impto-tit-ap.conta-imposto      no-undo.
def var c-conta-retencao     like impto-tit-ap.conta-retencao     no-undo.
def var c-conta-percepcao    like impto-tit-ap.conta-percepcao    no-undo.
def var c-conta-iva-liberado like impto-tit-ap.conta-iva-liberado no-undo.
def var c-lancamento         as char format "x(4)"                no-undo.
def var c-lista-lancamento   as char                              no-undo.
def var de-tot-imposto       as dec format ">,>>>,>>9.99"         no-undo.

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
&Scoped-define INTERNAL-TABLES impto-tit-ap tipo-tax

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table impto-tit-ap.cod-imposto ~
entry(impto-tit-ap.lancamento, c-lista-lancamento) @ c-lancamento ~
tipo-tax.descricao impto-tit-ap.mo-codigo impto-tit-ap.cotacao-dia ~
impto-tit-ap.vl-base-me impto-tit-ap.vl-imposto-me ~
impto-tit-ap.perc-imposto ~
string(impto-tit-ap.conta-imposto, c-formato) @ c-conta-imposto ~
impto-tit-ap.vl-retencao-me impto-tit-ap.perc-retencao ~
string(impto-tit-ap.conta-retencao, c-formato) @ c-conta-retencao ~
impto-tit-ap.vl-percepcao-me impto-tit-ap.perc-percepcao ~
string(impto-tit-ap.conta-percepcao, c-formato) @ c-conta-percepcao ~
impto-tit-ap.vl-iva-liberado-me impto-tit-ap.perc-iva-liberado ~
string(impto-tit-ap.conta-iva-liberado, c-formato) @ c-conta-iva-liberado ~
impto-tit-ap.contabilizou fn-origem() @ c-origem impto-tit-ap.obs 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH impto-tit-ap OF tit-ap WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH tipo-tax WHERE tipo-tax.cod-tax = impto-tit-ap.cod-imposto NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br_table impto-tit-ap tipo-tax
&Scoped-define FIRST-TABLE-IN-QUERY-br_table impto-tit-ap
&Scoped-define SECOND-TABLE-IN-QUERY-br_table tipo-tax


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table bt-contas 
&Scoped-Define DISPLAYED-OBJECTS de-total-imposto 

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
cod-fornec||y|movadm.impto-tit-ap.cod-fornec
cod-esp||y|movadm.impto-tit-ap.cod-esp
cod-estabel||y|movadm.impto-tit-ap.cod-estabel
hp-codigo||y|movadm.impto-tit-ap.hp-codigo
num-id-mov-ap||y|movadm.impto-tit-ap.num-id-mov-ap
ep-codigo||y|movadm.impto-tit-ap.ep-codigo
tp-codigo||y|movadm.impto-tit-ap.tp-codigo
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "cod-fornec,cod-esp,cod-estabel,hp-codigo,num-id-mov-ap,ep-codigo,tp-codigo"':U).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-origem B-table-Win 
FUNCTION fn-origem RETURNS CHARACTER
   () FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-contas 
     LABEL "" 
     SIZE 18 BY 1.

DEFINE VARIABLE de-total-imposto AS DECIMAL FORMAT "->>,>>>,>>9.99" INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      impto-tit-ap, 
      tipo-tax SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      impto-tit-ap.cod-imposto FORMAT ">>>9":U
      entry(impto-tit-ap.lancamento, c-lista-lancamento) @ c-lancamento
      tipo-tax.descricao FORMAT "x(30)":U
      impto-tit-ap.mo-codigo FORMAT ">9":U
      impto-tit-ap.cotacao-dia FORMAT ">>>,>9.99999999":U
      impto-tit-ap.vl-base-me FORMAT ">>>,>>>,>>>,>>9.99":U
      impto-tit-ap.vl-imposto-me FORMAT ">>>,>>>,>>>,>>9.99":U
      impto-tit-ap.perc-imposto FORMAT ">>>9.99":U
      string(impto-tit-ap.conta-imposto, c-formato) @ c-conta-imposto
      impto-tit-ap.vl-retencao-me FORMAT ">>>,>>>,>>>,>>9.99":U
      impto-tit-ap.perc-retencao FORMAT ">>>9.99":U
      string(impto-tit-ap.conta-retencao, c-formato) @ c-conta-retencao
      impto-tit-ap.vl-percepcao-me FORMAT ">>>,>>>,>>>,>>9.99":U
      impto-tit-ap.perc-percepcao FORMAT ">>>9.99":U
      string(impto-tit-ap.conta-percepcao, c-formato) @ c-conta-percepcao
      impto-tit-ap.vl-iva-liberado-me FORMAT ">>>,>>>,>>>,>>9.99":U
      impto-tit-ap.perc-iva-liberado FORMAT ">>>9.99":U
      string(impto-tit-ap.conta-iva-liberado, c-formato) @ c-conta-iva-liberado
      impto-tit-ap.contabilizou FORMAT "Sim/NÆo":U
      fn-origem() @ c-origem
      impto-tit-ap.obs FORMAT "x(2000)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 87 BY 8.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
     bt-contas AT ROW 9.88 COL 1.29
     de-total-imposto AT ROW 9.96 COL 70.14 COLON-ALIGNED
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
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 10
         WIDTH              = 87.29.
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
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 1.

/* SETTINGS FOR FILL-IN de-total-imposto IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "movadm.impto-tit-ap OF movadm.tit-ap,mgdis.tipo-tax WHERE movadm.impto-tit-ap ..."
     _Options          = "NO-LOCK KEY-PHRASE"
     _TblOptList       = ","
     _JoinCode[2]      = "mgdis.tipo-tax.cod-tax = movadm.impto-tit-ap.cod-imposto"
     _FldNameList[1]   = movadm.impto-tit-ap.cod-imposto
     _FldNameList[2]   > "_<CALC>"
"entry(impto-tit-ap.lancamento, c-lista-lancamento) @ c-lancamento" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   = mgdis.tipo-tax.descricao
     _FldNameList[4]   = movadm.impto-tit-ap.mo-codigo
     _FldNameList[5]   = movadm.impto-tit-ap.cotacao-dia
     _FldNameList[6]   = movadm.impto-tit-ap.vl-base-me
     _FldNameList[7]   = movadm.impto-tit-ap.vl-imposto-me
     _FldNameList[8]   = movadm.impto-tit-ap.perc-imposto
     _FldNameList[9]   > "_<CALC>"
"string(impto-tit-ap.conta-imposto, c-formato) @ c-conta-imposto" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[10]   = movadm.impto-tit-ap.vl-retencao-me
     _FldNameList[11]   = movadm.impto-tit-ap.perc-retencao
     _FldNameList[12]   > "_<CALC>"
"string(impto-tit-ap.conta-retencao, c-formato) @ c-conta-retencao" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[13]   = movadm.impto-tit-ap.vl-percepcao-me
     _FldNameList[14]   = movadm.impto-tit-ap.perc-percepcao
     _FldNameList[15]   > "_<CALC>"
"string(impto-tit-ap.conta-percepcao, c-formato) @ c-conta-percepcao" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[16]   = movadm.impto-tit-ap.vl-iva-liberado-me
     _FldNameList[17]   = movadm.impto-tit-ap.perc-iva-liberado
     _FldNameList[18]   > "_<CALC>"
"string(impto-tit-ap.conta-iva-liberado, c-formato) @ c-conta-iva-liberado" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[19]   = movadm.impto-tit-ap.contabilizou
     _FldNameList[20]   > "_<CALC>"
"fn-origem() @ c-origem" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[21]   = movadm.impto-tit-ap.obs
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
    RUN New-State('DblClick':U).

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

    {src/adm/template/brschnge.i}
    run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))).

    assign bt-contas:sensitive in frame {&FRAME-NAME} = no.

    if  avail impto-tit-ap then do:

        if  impto-tit-ap.origem-impto <> 13 then
            assign bt-contas:sensitive in frame {&FRAME-NAME} = yes.
        ELSE
            /** Alterado em fun‡Æo do Pa¡s ser M‚xico **/   
            IF  i-pais-impto-usuario = 4 THEN
                assign bt-contas:sensitive in frame {&FRAME-NAME} = no.

        assign de-tot-imposto = 0.

        for each b-impto-tit-ap 
           where b-impto-tit-ap.ep-codigo   = tit-ap.ep-codigo
           and   b-impto-tit-ap.cod-estabel = tit-ap.cod-estabel
           and   b-impto-tit-ap.cod-esp     = tit-ap.cod-esp
           and   b-impto-tit-ap.serie       = tit-ap.serie
           and   b-impto-tit-ap.nr-docto    = tit-ap.nr-docto
           and   b-impto-tit-ap.parcela     = tit-ap.parcela
           and   b-impto-tit-ap.cod-imposto = input browse {&browse-name} impto-tit-ap.cod-imposto:

           if  b-impto-tit-ap.lancamento = 2 then
               assign de-tot-imposto  = de-tot-imposto - b-impto-tit-ap.vl-imposto.
           else 
               assign de-tot-imposto = de-tot-imposto + b-impto-tit-ap.vl-imposto.

        end.

        assign de-total-imposto:screen-value in frame {&frame-name} = string(de-tot-imposto).

    end.
    else 
         assign de-total-imposto:screen-value in frame {&frame-name} = string(0).


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-contas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-contas B-table-Win
ON CHOOSE OF bt-contas IN FRAME F-Main
DO:

     if  avail impto-tit-ap then do:
         run app/ap0804fb.w (input rowid(impto-tit-ap)).
     end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

find first param-global no-lock no-error.
assign c-formato = param-global.formato-conta-contabil.

{utp/ut-liter.i Conta_Imposto}
assign c-conta-imposto:label in browse {&browse-name} = return-value.

{utp/ut-field.i mgadm tit-ap Origem 1}
assign c-origem:label in browse {&browse-name} = return-value.

{utp/ut-liter.i Total}
assign de-total-imposto:label in frame {&frame-name} = trim(return-value).

{utp/ut-field.i mgadm impto-tit-ap lancamento 2}
assign c-lancamento:label in browse {&browse-name} = return-value.

{utp/ut-liter.i Contas_Cont beis * C}
assign bt-contas:label in frame {&FRAME-NAME} = trim(return-value).

assign c-lista-lancamento = {adinc/i01ad059.i 03}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  apply 'value-changed' to br_table in frame {&FRAME-NAME}.

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
  {src/adm/template/sndkycas.i "cod-fornec" "impto-tit-ap" "cod-fornec"}
  {src/adm/template/sndkycas.i "cod-esp" "impto-tit-ap" "cod-esp"}
  {src/adm/template/sndkycas.i "cod-estabel" "impto-tit-ap" "cod-estabel"}
  {src/adm/template/sndkycas.i "hp-codigo" "impto-tit-ap" "hp-codigo"}
  {src/adm/template/sndkycas.i "num-id-mov-ap" "impto-tit-ap" "num-id-mov-ap"}
  {src/adm/template/sndkycas.i "ep-codigo" "impto-tit-ap" "ep-codigo"}
  {src/adm/template/sndkycas.i "tp-codigo" "impto-tit-ap" "tp-codigo"}

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
  {src/adm/template/snd-list.i "tit-ap"}
  {src/adm/template/snd-list.i "impto-tit-ap"}
  {src/adm/template/snd-list.i "tipo-tax"}

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

    apply 'value-changed' to br_table in frame {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-origem B-table-Win 
FUNCTION fn-origem RETURNS CHARACTER
   ():
/*------------------------------------------------------------------------------
   Purpose:  
   Notes:  
------------------------------------------------------------------------------*/

   assign c-origem = "".

   if  impto-tit-ap.origem-impto <> 0 then
       assign c-origem = {adinc/i01ad290.i 04 impto-tit-ap.origem-impto}.

   RETURN c-origem.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


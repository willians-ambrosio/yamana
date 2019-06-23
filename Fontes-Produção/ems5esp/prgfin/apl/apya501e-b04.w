&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems5             PROGRESS
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i B99XX999 9.99.99.999}

/* Chamada a include do gerenciador de licen�as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m�dulo>:  Informar qual o m�dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m�dulo dever� ser MUT                    */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> MUT}
&ENDIF

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
define variable c-lista-valor as character init '':U no-undo.

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
&Scoped-define INTERNAL-TABLES es_operac_financ operac_financ

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table es_operac_financ.cod_banco ~
es_operac_financ.cod_produt_financ es_operac_financ.cod_contrat_apf ~
es_operac_financ.cod_operac_financ operac_financ.val_operac_financ 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table 
&Scoped-define QUERY-STRING-br-table FOR EACH es_operac_financ ~
      WHERE es_operac_financ.cod_banco >= cod_banco_ini ~
 AND es_operac_financ.cod_banco <= cod_banco_fim ~
 AND es_operac_financ.cod_produt_financ >= cod_produt_financ_ini ~
 AND es_operac_financ.cod_produt_financ <= cod_produt_financ_fim ~
 AND es_operac_financ.cod_operac_financ >= cod_operac_financ_ini ~
 AND es_operac_financ.cod_operac_financ <= cod_operac_financ_fim ~
 AND es_operac_financ.cod_contrat_apf >= cod_contrat_apf_ini ~
 AND es_operac_financ.cod_contrat_apf <= cod_contrat_apf_fim NO-LOCK, ~
      EACH operac_financ OF es_operac_financ NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-table OPEN QUERY br-table FOR EACH es_operac_financ ~
      WHERE es_operac_financ.cod_banco >= cod_banco_ini ~
 AND es_operac_financ.cod_banco <= cod_banco_fim ~
 AND es_operac_financ.cod_produt_financ >= cod_produt_financ_ini ~
 AND es_operac_financ.cod_produt_financ <= cod_produt_financ_fim ~
 AND es_operac_financ.cod_operac_financ >= cod_operac_financ_ini ~
 AND es_operac_financ.cod_operac_financ <= cod_operac_financ_fim ~
 AND es_operac_financ.cod_contrat_apf >= cod_contrat_apf_ini ~
 AND es_operac_financ.cod_contrat_apf <= cod_contrat_apf_fim NO-LOCK, ~
      EACH operac_financ OF es_operac_financ NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-table es_operac_financ operac_financ
&Scoped-define FIRST-TABLE-IN-QUERY-br-table es_operac_financ
&Scoped-define SECOND-TABLE-IN-QUERY-br-table operac_financ


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cod_banco_ini cod_banco_fim bt-confirma ~
IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 ~
cod_produt_financ_ini cod_produt_financ_fim cod_operac_financ_ini ~
cod_operac_financ_fim cod_contrat_apf_ini cod_contrat_apf_fim br-table 
&Scoped-Define DISPLAYED-OBJECTS cod_banco_ini cod_banco_fim ~
cod_produt_financ_ini cod_produt_financ_fim cod_operac_financ_ini ~
cod_operac_financ_fim cod_contrat_apf_ini cod_contrat_apf_fim 

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
cod_produt_financ||y|mgesp.es_operac_financ.cod_produt_financ
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "cod_produt_financ"':U).

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

DEFINE VARIABLE cod_banco_fim AS CHARACTER FORMAT "x(8)" INITIAL "zzzzzzzz" 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .88 NO-UNDO.

DEFINE VARIABLE cod_banco_ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Banco" 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .88 NO-UNDO.

DEFINE VARIABLE cod_contrat_apf_fim AS CHARACTER FORMAT "x(10)" INITIAL "zzzzzzzzzz" 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .88 NO-UNDO.

DEFINE VARIABLE cod_contrat_apf_ini AS CHARACTER FORMAT "x(10)" 
     LABEL "Contrato" 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .88 NO-UNDO.

DEFINE VARIABLE cod_operac_financ_fim AS CHARACTER FORMAT "x(10)" INITIAL "zzzzzzzzzz" 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .88 NO-UNDO.

DEFINE VARIABLE cod_operac_financ_ini AS CHARACTER FORMAT "x(10)" 
     LABEL "Opera��o Financeira" 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .88 NO-UNDO.

DEFINE VARIABLE cod_produt_financ_fim AS CHARACTER FORMAT "x(8)" INITIAL "zzzzzzzz" 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .88 NO-UNDO.

DEFINE VARIABLE cod_produt_financ_ini AS CHARACTER FORMAT "x(8)" 
     LABEL "Produto Financeiro" 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-2
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-3
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-4
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-5
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-6
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-7
     FILENAME "image\ii-fir":U
     SIZE 2.86 BY 1.

DEFINE IMAGE IMAGE-8
     FILENAME "image\ii-las":U
     SIZE 2.86 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      es_operac_financ, 
      operac_financ SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _STRUCTURED
  QUERY br-table NO-LOCK DISPLAY
      es_operac_financ.cod_banco FORMAT "x(8)":U
      es_operac_financ.cod_produt_financ FORMAT "x(8)":U
      es_operac_financ.cod_contrat_apf FORMAT "x(10)":U
      es_operac_financ.cod_operac_financ FORMAT "x(10)":U
      operac_financ.val_operac_financ FORMAT ">>>,>>>,>>9.9999999999":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 80 BY 7.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     cod_banco_ini AT ROW 1 COL 20 COLON-ALIGNED HELP
          "C�digo Banco"
     cod_banco_fim AT ROW 1 COL 43 COLON-ALIGNED HELP
          "C�digo Banco" NO-LABEL WIDGET-ID 26
     bt-confirma AT ROW 1 COL 76
     cod_produt_financ_ini AT ROW 2 COL 20 COLON-ALIGNED HELP
          "C�digo do Produto Financeiro" WIDGET-ID 4
     cod_produt_financ_fim AT ROW 2 COL 43 COLON-ALIGNED HELP
          "C�digo do Produto Financeiro" NO-LABEL WIDGET-ID 28
     cod_operac_financ_ini AT ROW 3 COL 20 COLON-ALIGNED HELP
          "C�digo da Opera��o Financeira" WIDGET-ID 12
     cod_operac_financ_fim AT ROW 3 COL 43 COLON-ALIGNED HELP
          "C�digo da Opera��o Financeira" NO-LABEL WIDGET-ID 30
     cod_contrat_apf_ini AT ROW 4 COL 20 COLON-ALIGNED HELP
          "C�digo Contrato" WIDGET-ID 20
     cod_contrat_apf_fim AT ROW 4 COL 43 COLON-ALIGNED HELP
          "C�digo Contrato" NO-LABEL WIDGET-ID 32
     br-table AT ROW 5.25 COL 1
     IMAGE-1 AT ROW 1 COL 32.72
     IMAGE-2 AT ROW 1 COL 41
     IMAGE-3 AT ROW 2 COL 32.72 WIDGET-ID 6
     IMAGE-4 AT ROW 2 COL 41 WIDGET-ID 8
     IMAGE-5 AT ROW 3 COL 32.72 WIDGET-ID 14
     IMAGE-6 AT ROW 3 COL 41 WIDGET-ID 16
     IMAGE-7 AT ROW 4 COL 32.72 WIDGET-ID 22
     IMAGE-8 AT ROW 4 COL 41 WIDGET-ID 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0  WIDGET-ID 100.


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
         HEIGHT             = 11.88
         WIDTH              = 80.29.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit L-To-R                            */
/* BROWSE-TAB br-table cod_contrat_apf_fim F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _TblList          = "mgesp.es_operac_financ,ems5.operac_financ OF mgesp.es_operac_financ"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ","
     _Where[1]         = "mgesp.es_operac_financ.cod_banco >= cod_banco_ini
 AND mgesp.es_operac_financ.cod_banco <= cod_banco_fim
 AND mgesp.es_operac_financ.cod_produt_financ >= cod_produt_financ_ini
 AND mgesp.es_operac_financ.cod_produt_financ <= cod_produt_financ_fim
 AND mgesp.es_operac_financ.cod_operac_financ >= cod_operac_financ_ini
 AND mgesp.es_operac_financ.cod_operac_financ <= cod_operac_financ_fim
 AND mgesp.es_operac_financ.cod_contrat_apf >= cod_contrat_apf_ini
 AND mgesp.es_operac_financ.cod_contrat_apf <= cod_contrat_apf_fim"
     _FldNameList[1]   = mgesp.es_operac_financ.cod_banco
     _FldNameList[2]   = mgesp.es_operac_financ.cod_produt_financ
     _FldNameList[3]   = mgesp.es_operac_financ.cod_contrat_apf
     _FldNameList[4]   = mgesp.es_operac_financ.cod_operac_financ
     _FldNameList[5]   = ems5.operac_financ.val_operac_financ
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
  assign input frame {&frame-name} cod_banco_ini cod_banco_fim cod_produt_financ_ini cod_produt_financ_fim cod_operac_financ_ini cod_operac_financ_fim cod_contrat_apf_ini cod_contrat_apf_fim.
  RUN dispatch IN THIS-PROCEDURE ('open-query':U).
  apply 'value-changed':U to {&browse-name}.
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
  {src/adm/template/sndkycas.i "cod_produt_financ" "es_operac_financ" "cod_produt_financ"}

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
  {src/adm/template/snd-list.i "es_operac_financ"}
  {src/adm/template/snd-list.i "operac_financ"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "RetornaValorCampo" B-table-Win _INLINE
/* Actions: ? ? ? ? support/brwrtval.p */
procedure pi-retorna-valor:

end.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/********************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i YMSA0005-B01 1.00.00.001}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m¢dulo dever  ser MUT                    */

/* &IF "{&EMSFND_VERSION}" >= "1.00" &THEN          */
/*     {include/i-license-manager.i <programa> MUT} */
/* &ENDIF                                           */

/* Create an unnamed pool to store all the widgets created by this procedure.
   This is a good default which assures that this procedure's triggers and 
   internal procedures will execute in this procedure's storage, and that 
   proper cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE wh-pesquisa AS WIDGET-HANDLE      NO-UNDO.

DEFINE TEMP-TABLE tt-func NO-UNDO
  FIELD line as int /*:T Este campo ‚ obrigat¢rio */
  FIELD cdn_usuario     LIKE usuar_control_aces.cdn_usuar_control_aces
  FIELD cdn_empresa     LIKE funcionario.cdn_empresa
  FIELD cdn_estab       LIKE funcionario.cdn_estab
  FIELD cdn_funcionario LIKE funcionario.cdn_funcionario
  FIELD nom_funcionario LIKE funcionario.nom_pessoa_fisic.

def var c-cod-lista-obj as char no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE BrowseDigitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-digita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-func

/* Definitions for BROWSE br-digita                                     */
&Scoped-define FIELDS-IN-QUERY-br-digita tt-func.cdn_empresa tt-func.cdn_estab tt-func.cdn_funcionario tt-func.cdn_usuario tt-func.nom_funcionario   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-func.cdn_empresa ~
tt-func.cdn_estab ~
tt-func.cdn_funcionario   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-digita tt-func
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-digita tt-func
&Scoped-define SELF-NAME br-digita
&Scoped-define QUERY-STRING-br-digita FOR EACH tt-func NO-LOCK
&Scoped-define OPEN-QUERY-br-digita OPEN QUERY {&self-name} FOR EACH tt-func NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-digita tt-func
&Scoped-define FIRST-TABLE-IN-QUERY-br-digita tt-func


/* Definitions for FRAME F-Main                                         */

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-name
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
&BROWSE-name
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
DEFINE BUTTON bt-eliminar 
     LABEL "&Eliminar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-incluir 
     LABEL "&Incluir" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-modificar 
     LABEL "&Modificar" 
     SIZE 10 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-digita FOR 
      tt-func SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-digita B-table-Win _FREEFORM
  QUERY br-digita NO-LOCK DISPLAY
      tt-func.cdn_empresa
      tt-func.cdn_estab         COLUMN-LABEL "Estab."
      tt-func.cdn_funcionario   COLUMN-LABEL "Matr¡cula"
      tt-func.cdn_usuario
      tt-func.nom_funcionario
   ENABLE
      tt-func.cdn_empresa
      tt-func.cdn_estab
      tt-func.cdn_funcionario
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 66 BY 6.5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-digita AT ROW 1 COL 1
     bt-incluir AT ROW 7.54 COL 1.29
     bt-modificar AT ROW 7.54 COL 11.57
     bt-eliminar AT ROW 7.54 COL 21.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: BrowseDigitacao
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 7.58
         WIDTH              = 66.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{include/c-brows5.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br-digita 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE br-digita IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-eliminar IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-incluir IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-modificar IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-digita
/* Query rebuild information for BROWSE br-digita
     _START_FREEFORM
OPEN QUERY {&self-name} FOR EACH tt-func NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* BROWSE br-digita */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-digita
&Scoped-define SELF-NAME br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita B-table-Win
ON DEL OF br-digita IN FRAME F-Main
DO:
   run pi-elimina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita B-table-Win
ON INS OF br-digita IN FRAME F-Main
DO:
   run pi-save-record.
   run insere-registro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita B-table-Win
ON OFF-END OF br-digita IN FRAME F-Main
DO:
   run pi-off-end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita B-table-Win
ON ROW-ENTRY OF br-digita IN FRAME F-Main
DO:
   /* This code displays initial values for newly added or copied rows.
   {src/adm/template/brsentry.i}
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita B-table-Win
ON ROW-LEAVE OF br-digita IN FRAME F-Main
DO:
   /* Do not disable this code or no updates will take place except
      by pressing the Save button on an Update SmartPanel.
   {src/adm/template/brsleave.i}
   */

   IF br-digita:NEW-ROW IN FRAME {&FRAME-NAME} THEN DO TRANSACTION ON ERROR UNDO, RETURN NO-APPLY:
      CREATE tt-func.
      ASSIGN INPUT BROWSE br-digita tt-func.cdn_usuario    
             INPUT BROWSE br-digita tt-func.cdn_empresa    
             INPUT BROWSE br-digita tt-func.cdn_estab      
             INPUT BROWSE br-digita tt-func.cdn_funcionario
             INPUT BROWSE br-digita tt-func.nom_funcionario.

      br-digita:CREATE-RESULT-LIST-ENTRY() IN FRAME {&FRAME-NAME}.
   END.
   ELSE DO TRANSACTION ON ERROR UNDO, RETURN NO-APPLY:

       if avail tt-func then do:
          ASSIGN INPUT BROWSE br-digita tt-func.cdn_usuario    
                 INPUT BROWSE br-digita tt-func.cdn_empresa    
                 INPUT BROWSE br-digita tt-func.cdn_estab      
                 INPUT BROWSE br-digita tt-func.cdn_funcionario
                 INPUT BROWSE br-digita tt-func.nom_funcionario.
       end.

   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita B-table-Win
ON VALUE-CHANGED OF br-digita IN FRAME F-Main
DO:
   /* This ADM trigger code must be preserved in order to notify other
      objects when the browser's current row changes. */
   {src/adm/template/brschnge.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-eliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-eliminar B-table-Win
ON CHOOSE OF bt-eliminar IN FRAME F-Main /* Eliminar */
DO:
   IF br-digita:NUM-SELECTED-ROWS > 0 THEN DO ON ERROR UNDO, RETURN NO-APPLY:
      GET CURRENT br-digita.
      DELETE tt-func.
      IF br-digita:DELETE-CURRENT-ROW() IN FRAME {&FRAME-NAME} THEN.
    END.

    IF NUM-RESULTS("br-digita":U) = 0 THEN
       ASSIGN bt-modificar:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              bt-eliminar:SENSITIVE IN FRAME {&FRAME-NAME}  = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-incluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-incluir B-table-Win
ON CHOOSE OF bt-incluir IN FRAME F-Main /* Incluir */
DO:
   IF NUM-RESULTS("br-digita":U) > 0 THEN
      br-digita:INSERT-ROW("after":U) IN FRAME {&FRAME-NAME}.
   ELSE DO TRANSACTION:
      CREATE tt-func.
      OPEN QUERY br-digita FOR EACH tt-func.
      APPLY "ENTRY":U TO tt-func.cdn_empresa IN BROWSE br-digita. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modificar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modificar B-table-Win
ON CHOOSE OF bt-modificar IN FRAME F-Main /* Modificar */
DO:
   RUN pi-entry.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

ON F5, MOUSE-SELECT-DBLCLICK OF tt-func.cdn_funcionario IN BROWSE br-digita DO:
   {include/zoomvar.i &prog-zoom=object/sopy/zoom/z03py085.w
                      &campo=tt-func.cdn_funcionario
                      &campozoom=cdn_funcionario
                      &campo2=tt-func.nom_funcionario
                      &campozoom2=nom_pessoa_fisic
                      &browse=br-digita
                      &parametros="run pi-seta-inicial in wh-pesquisa (tt-func.cdn_empresa:SCREEN-VALUE IN BROWSE br-digita,
                                   INTEGER(tt-func.cdn_estab:SCREEN-VALUE IN BROWSE br-digita))."}.
END.

ON LEAVE OF tt-func.cdn_funcionario IN BROWSE br-digita DO:
    if avail tt-func then do:
        IF INPUT BROWSE br-digita tt-func.cdn_estab <> "" THEN DO:
           FIND FIRST funcionario NO-LOCK
                WHERE funcionario.cdn_empresa     = INPUT BROWSE br-digita tt-func.cdn_empresa
                  AND funcionario.cdn_estab       = INPUT BROWSE br-digita tt-func.cdn_estab
                  AND funcionario.cdn_funcionario = INPUT BROWSE br-digita tt-func.cdn_funcionario NO-ERROR.
           IF AVAIL funcionario THEN
              ASSIGN tt-func.nom_funcionario:SCREEN-VALUE IN BROWSE br-digita = funcionario.nom_pessoa_fisic.
           ELSE
              ASSIGN tt-func.nom_funcionario:SCREEN-VALUE IN BROWSE br-digita = "".
        END.
    end.
END.

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
  /* a chamada para a pi abaixo deve ser mantida */
  
  run pi-foco-atributo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-assign B-table-Win 
PROCEDURE pi-assign :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER TABLE FOR tt-func.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-registro B-table-Win 
PROCEDURE pi-cria-registro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*:T Aqui devem ser lidos os registros da temp-table, criados os registros na tabela f¡sica
  e atualizados os valores na tabela f¡sica, pois as valida‡äes sÆo feitas na pi-salva-rel.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-entry B-table-Win 
PROCEDURE pi-entry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*:T Esta procedure da um "ENTRY" no primeiro campo edit vel do browse */
    APPLY "ENTRY":U TO tt-func.cdn_empresa IN BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-rowid B-table-Win 
PROCEDURE pi-rowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p_row_func_revista AS ROWID  NO-UNDO.

DEFINE VARIABLE v_num_aux AS INTEGER     NO-UNDO.

EMPTY TEMP-TABLE tt-func NO-ERROR.
FIND FIRST func_revista NO-LOCK
     WHERE ROWID(func_revista) = p_row_func_revista NO-ERROR.
IF AVAIL func_revista THEN
DO v_num_aux = 1 TO func_revista.qti_func_cadastro:
   FIND FIRST funcionario NO-LOCK
        WHERE funcionario.cdn_empresa     = func_revista.cdn_empr_func[v_num_aux]
          AND funcionario.cdn_estab       = func_revista.cdn_estab_func[v_num_aux]
          AND funcionario.cdn_funcionario = func_revista.cdn_matr_func[v_num_aux] NO-ERROR.

   FIND FIRST usuar_control_aces NO-LOCK
        WHERE usuar_control_aces.cdn_empresa     = func_revista.cdn_empr_func[v_num_aux]
          AND usuar_control_aces.cdn_estab       = func_revista.cdn_estab_func[v_num_aux]
          AND usuar_control_aces.cdn_funcionario = func_revista.cdn_matr_func[v_num_aux] NO-ERROR.

   IF AVAIL funcionario THEN DO:
      CREATE tt-func.
      ASSIGN tt-func.line            = v_num_aux
             tt-func.cdn_usuario     = IF AVAIL usuar_control_aces THEN usuar_control_aces.cdn_usuar_control_aces ELSE 0
             tt-func.cdn_empresa     = funcionario.cdn_empresa
             tt-func.cdn_estab       = funcionario.cdn_estab
             tt-func.cdn_funcionario = funcionario.cdn_funcionario
             tt-func.nom_funcionario = funcionario.nom_pessoa_fisic.
   END.
END.
{&OPEN-QUERY-br-digita}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-salva-rel B-table-Win 
PROCEDURE pi-salva-rel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*:T A pi abaixo buscar  os valores de tela das vari veis instanciadas na viewer/frame, 
   necess rios para grava‡Æo ou valida‡Æo dos registros da temp-table.*/

   run pi-busca-valor(func_revista.dat_referencia,func_revista.qti_funcionarios).
   assign c-cod-lista-obj = return-value. /*:T lista dos valores em tela na viewer.*/

/*:T Aqui devem ser atualizados os valores retornados da viewer na temp-table e 
   tamb‚m feitas as valida‡äes necess rias, como, valida‡äes de valores informados e
   valida‡äes de banco de dados. Se ocorrer erro retornar "NOK".*/

   
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-sensitive B-table-Win 
PROCEDURE pi-sensitive :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p_log_sensitive  AS LOGICAL  NO-UNDO.

IF p_log_sensitive THEN DO:
   ASSIGN br-digita:SENSITIVE IN FRAME {&FRAME-NAME}  = YES
          bt-incluir:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

   IF CAN-FIND(FIRST tt-func) THEN
      ASSIGN bt-modificar:SENSITIVE IN FRAME {&FRAME-NAME} = YES
             bt-eliminar:SENSITIVE IN FRAME {&FRAME-NAME}  = YES.
END.
ELSE
   ASSIGN br-digita:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
          bt-incluir:SENSITIVE IN FRAME {&FRAME-NAME}   = NO
          bt-modificar:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          bt-eliminar:SENSITIVE IN FRAME {&FRAME-NAME}  = NO.

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
  {src/adm/template/snd-list.i "tt-func"}

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


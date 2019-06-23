&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          hcm              PROGRESS
          hresp            PROGRESS
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
{include/i-prgvrs.i UPC-FP1350-B01 12.1.11.000}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m¢dulo dever  ser MUT                    */

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
DEFINE VARIABLE rw-HistGestor AS ROWID       NO-UNDO.
/*:T Variaveis usadas internamente pelo estilo, favor nao elimina-las     */

/*:T v ri veis de uso globla */
/* v ri veis de uso globla */
DEF NEW GLOBAL SHARED VAR p-row-upc-fp1350      AS ROWID NO-UNDO.

def  var v-row-parent    as rowid no-undo.

/* vari veis de uso local */
def var v-row-table                      as rowid                    no-undo.
def var v_prg_parametro                  as handle                   no-undo.

/*:T fim das variaveis utilizadas no estilo */
DEFINE BUFFER bf-histGestor FOR es_HistGestor.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE BrowserCadastro2
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-Gestor

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES funcionario
&Scoped-define FIRST-EXTERNAL-TABLE funcionario


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR funcionario.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES es_HistGestor es_Gestor

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-Gestor                                     */
&Scoped-define FIELDS-IN-QUERY-br-Gestor es_HistGestor.cdn_gestor ~
es_Gestor.nom_gestor es_Gestor.niv_hier_funcnal es_HistGestor.da-inicio ~
es_HistGestor.da-final 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-Gestor 
&Scoped-define QUERY-STRING-br-Gestor FOR EACH es_HistGestor WHERE es_HistGestor.cdn_empresa = funcionario.cdn_empresa ~
  AND es_HistGestor.cdn_estab = funcionario.cdn_estab ~
  AND es_HistGestor.cdn_funcionario = funcionario.cdn_funcionario NO-LOCK, ~
      EACH es_Gestor OF es_HistGestor NO-LOCK ~
    BY es_HistGestor.cdn_estab ~
       BY es_HistGestor.cdn_funcionario ~
        BY es_HistGestor.da-inicio DESCENDING INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-Gestor OPEN QUERY br-Gestor FOR EACH es_HistGestor WHERE es_HistGestor.cdn_empresa = funcionario.cdn_empresa ~
  AND es_HistGestor.cdn_estab = funcionario.cdn_estab ~
  AND es_HistGestor.cdn_funcionario = funcionario.cdn_funcionario NO-LOCK, ~
      EACH es_Gestor OF es_HistGestor NO-LOCK ~
    BY es_HistGestor.cdn_estab ~
       BY es_HistGestor.cdn_funcionario ~
        BY es_HistGestor.da-inicio DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-Gestor es_HistGestor es_Gestor
&Scoped-define FIRST-TABLE-IN-QUERY-br-Gestor es_HistGestor
&Scoped-define SECOND-TABLE-IN-QUERY-br-Gestor es_Gestor


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-Gestor bt-incluir bt-modificar ~
bt-eliminar 

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
cdn_gestor||y|hresp.es_HistGestor.cdn_gestor
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "cdn_gestor"':U).

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
DEFINE QUERY br-Gestor FOR 
      es_HistGestor, 
      es_Gestor SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-Gestor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-Gestor B-table-Win _STRUCTURED
  QUERY br-Gestor NO-LOCK DISPLAY
      es_HistGestor.cdn_gestor FORMAT "ZZZZZZZ9":U
      es_Gestor.nom_gestor FORMAT "x(40)":U
      es_Gestor.niv_hier_funcnal FORMAT "ZZ9":U
      es_HistGestor.da-inicio FORMAT "99/99/9999":U
      es_HistGestor.da-final COLUMN-LABEL "Fim Gestor" FORMAT "99/99/9999":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 78 BY 7.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-Gestor AT ROW 1 COL 1
     bt-incluir AT ROW 8.75 COL 1
     bt-modificar AT ROW 8.75 COL 11
     bt-eliminar AT ROW 8.75 COL 21
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: BrowserCadastro2
   External Tables: hcm.funcionario
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 8.88
         WIDTH              = 78.86.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{utp/ut-glob.i}
{src/adm/method/browser.i}
{include/c-brows3.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit L-To-R                            */
/* BROWSE-TAB br-Gestor 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-Gestor
/* Query rebuild information for BROWSE br-Gestor
     _TblList          = "hresp.es_HistGestor WHERE hcm.funcionario <external> ...,hresp.es_Gestor OF hresp.es_HistGestor"
     _Options          = "NO-LOCK INDEXED-REPOSITION KEY-PHRASE"
     _TblOptList       = ","
     _OrdList          = "hresp.es_HistGestor.cdn_estab|yes,hresp.es_HistGestor.cdn_funcionario|yes,hresp.es_HistGestor.da-inicio|no"
     _JoinCode[1]      = "hresp.es_HistGestor.cdn_empresa = hcm.funcionario.cdn_empresa
  AND hresp.es_HistGestor.cdn_estab = hcm.funcionario.cdn_estab
  AND hresp.es_HistGestor.cdn_funcionario = hcm.funcionario.cdn_funcionario"
     _FldNameList[1]   = hresp.es_HistGestor.cdn_gestor
     _FldNameList[2]   = hresp.es_Gestor.nom_gestor
     _FldNameList[3]   = hresp.es_Gestor.niv_hier_funcnal
     _FldNameList[4]   = hresp.es_HistGestor.da-inicio
     _FldNameList[5]   > hresp.es_HistGestor.da-final
"es_HistGestor.da-final" "Fim Gestor" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br-Gestor */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-Gestor
&Scoped-define SELF-NAME br-Gestor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-Gestor B-table-Win
ON MOUSE-SELECT-DBLCLICK OF br-Gestor IN FRAME F-Main
DO:
    RUN New-State("DblClick, SELF":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-Gestor B-table-Win
ON ROW-ENTRY OF br-Gestor IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-Gestor B-table-Win
ON ROW-LEAVE OF br-Gestor IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-Gestor B-table-Win
ON VALUE-CHANGED OF br-Gestor IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  /* run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-eliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-eliminar B-table-Win
ON CHOOSE OF bt-eliminar IN FRAME F-Main /* Eliminar */
DO:

    FIND funcionario WHERE ROWID(funcionario) = p-row-upc-fp1350 NO-LOCK NO-ERROR.

    find param_empres_rh no-lock where
         param_empres_rh.cdn_empresa = v_cdn_empres_usuar no-error.

    if avail param_empres_rh then 
       if funcionario.cdn_sit_calc_func = 9 or
         (ES_HISTGESTOR.DA-INICIO < date(param_empres_rh.num_mes_refer_calc_efetd,01,param_empres_rh.num_ano_refer_calc_efetd)) then do:
          run utp/ut-msgs.p (input "show", 
                             input 17006, 
                             input "Elimina‡Æo nÆo Permitida !!!~~Funcionario DESLIGADO ou data de Referencia maior que data de inicio do Gestor.").
          return NO-APPLY.
       end.        

    ASSIGN rw-HistGestor = ROWID(es_HistGestor).

    run utp/ut-msgs.p (input "show":U, input 550, input "").

    IF RETURN-VALUE = "YES" THEN DO:

        FIND LAST bf-HistGestor USE-INDEX idx_GestorIni
            WHERE bf-HistGestor.cdn_empresa     = es_histGestor.cdn_empresa
              AND bf-HistGestor.cdn_estab       = es_histGestor.cdn_estab
              AND bf-HistGestor.cdn_funcionario = es_histgestor.cdn_funcionario
              AND bf-HistGestor.da-final        = (es_HistGestor.da-inicio - 1) NO-ERROR.
    
        IF AVAIL bf-HistGestor THEN
           ASSIGN bf-HistGestor.da-final = 12/31/9999.

        FIND bf-HistGestor
            WHERE ROWID(bf-HistGestor) = rw-HistGestor EXCLUSIVE-LOCK.
        IF AVAIL bf-HistGestor THEN DELETE bf-HistGestor.
        RELEASE bf-HistGestor.


    END.
/*     RUN pi-eliminar.                                                                   */

    {&OPEN-QUERY-Br-Gestor}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-incluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-incluir B-table-Win
ON CHOOSE OF bt-incluir IN FRAME F-Main /* Incluir */
DO:
/*     RUN pi-Incmod ('incluir':U).  */

    FIND funcionario
        WHERE ROWID(funcionario) = p-row-upc-fp1350 NO-LOCK NO-ERROR.

    find param_empres_rh no-lock where
         param_empres_rh.cdn_empresa = v_cdn_empres_usuar no-error.
    if avail param_empres_rh then 
       if funcionario.cdn_sit_calc_func = 9 or
         (funcionario.dat_desligto_func < date(param_empres_rh.num_mes_refer_calc_efetd,01,param_empres_rh.num_ano_refer_calc_efetd)) then do:
          run utp/ut-msgs.p (input "show", 
                             input 17006, 
                             input "Funcionario Desligado.~~Somente Funcionarios Ativos podem ter Gestores relacionados.").
          return NO-APPLY.
       end.                      

       find last es_HistGestor of funcionario no-lock no-error.
       assign v-row-parent = rowid(funcionario)
              v-row-table  = rowid(es_histGestor).

       RUN prghur\UPC\upc-fp1350-u01.w persistent  set v_prg_parametro (this-procedure, v-row-parent,  v-row-table).

       if valid-handle(v_prg_parametro) then do:
          RUN dispatch IN v_prg_parametro ('initialize':U).

          if valid-handle(v_prg_parametro) then do:
             run pi_reposiciona in v_prg_parametro .
             RUN pi_entry_atrib IN v_prg_parametro.
          end.
       end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modificar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modificar B-table-Win
ON CHOOSE OF bt-modificar IN FRAME F-Main /* Modificar */
DO:
  RUN pi-Incmod ('modificar':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

ASSIGN bt-modificar:SENSITIVE = NO.
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "funcionario"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "funcionario"}

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
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FIND funcionario WHERE ROWID(funcionario) = p-row-upc-fp1350 NO-LOCK NO-ERROR.

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

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
  {src/adm/template/sndkycas.i "cdn_gestor" "es_HistGestor" "cdn_gestor"}

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
  {src/adm/template/snd-list.i "funcionario"}
  {src/adm/template/snd-list.i "es_HistGestor"}
  {src/adm/template/snd-list.i "es_Gestor"}

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
  ASSIGN bt-modificar:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


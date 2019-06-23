&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems5_esp         PROGRESS
          mgesp            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i escd0008-V01 2.06.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */
def new global shared var v_rec_prog_dtsul as recid no-undo.
def new global shared var v_rec_empresa    as recid no-undo.
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES es_param_emp
&Scoped-define FIRST-EXTERNAL-TABLE es_param_emp


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es_param_emp.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es_param_emp.dat_valid_fim ~
es_param_emp.cod_empresa es_param_emp.cod_parametro 
&Scoped-define ENABLED-TABLES es_param_emp
&Scoped-define FIRST-ENABLED-TABLE es_param_emp
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS es_param_emp.cod_prog_dtsul ~
es_param_emp.cod_referencia es_param_emp.dat_valid_ini ~
es_param_emp.dat_valid_fim es_param_emp.cod_empresa ~
es_param_emp.cod_parametro es_param_emp.cod_usuar_ult_atualiz ~
es_param_emp.dat_ult_atualiz es_param_emp.hra_ult_atualiz 
&Scoped-define DISPLAYED-TABLES es_param_emp
&Scoped-define FIRST-DISPLAYED-TABLE es_param_emp
&Scoped-Define DISPLAYED-OBJECTS c-des_prog_dtsul c-des_empresa 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS es_param_emp.cod_prog_dtsul ~
es_param_emp.cod_referencia es_param_emp.dat_valid_ini ~
es_param_emp.cod_empresa 
&Scoped-define ADM-MODIFY-FIELDS es_param_emp.cod_prog_dtsul ~
es_param_emp.cod_empresa 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS> 
</FOREIGN-KEYS>
<EXECUTING-CODE>
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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE c-des_empresa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41.57 BY .88 NO-UNDO.

DEFINE VARIABLE c-des_prog_dtsul AS CHARACTER FORMAT "x(40)" 
     LABEL "Descri‡Æo":R11 
     VIEW-AS FILL-IN 
     SIZE 41.57 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 5.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 2.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es_param_emp.cod_prog_dtsul AT ROW 1.17 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 51.14 BY .88
     c-des_prog_dtsul AT ROW 2.17 COL 21 COLON-ALIGNED HELP
          "Descri‡Æo do Programa"
     es_param_emp.cod_referencia AT ROW 3.17 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 60 BY .88
     es_param_emp.dat_valid_ini AT ROW 4.17 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     es_param_emp.dat_valid_fim AT ROW 4.17 COL 55 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     es_param_emp.cod_empresa AT ROW 5.13 COL 21 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 5 BY .88
     c-des_empresa AT ROW 5.13 COL 26.43 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     es_param_emp.cod_parametro AT ROW 6.79 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 60 BY .88
     es_param_emp.cod_usuar_ult_atualiz AT ROW 7.75 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.14 BY .88
     es_param_emp.dat_ult_atualiz AT ROW 7.75 COL 39 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     es_param_emp.hra_ult_atualiz AT ROW 7.75 COL 60 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.29 BY .88
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 6.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgesp.es_param_emp
   Allow: Basic,DB-Fields
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 8.08
         WIDTH              = 88.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}
{include/i_dbtype.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN c-des_empresa IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-des_prog_dtsul IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN es_param_emp.cod_empresa IN FRAME f-main
   1 3                                                                  */
/* SETTINGS FOR FILL-IN es_param_emp.cod_prog_dtsul IN FRAME f-main
   NO-ENABLE 1 3                                                        */
/* SETTINGS FOR FILL-IN es_param_emp.cod_referencia IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN es_param_emp.cod_usuar_ult_atualiz IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN es_param_emp.dat_ult_atualiz IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN es_param_emp.dat_valid_ini IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN es_param_emp.hra_ult_atualiz IN FRAME f-main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME es_param_emp.cod_empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_param_emp.cod_empresa V-table-Win
ON F5 OF es_param_emp.cod_empresa IN FRAME f-main /* Empresa */
DO:
  assign v_rec_empresa = ?.
    run prgint/utb/utb069ka.p.

    if  v_rec_empresa <> ? then
        for first ems5.empresa no-lock
            where recid(ems5.empresa) = v_rec_empresa:
            assign es_param_emp.cod_empresa:screen-value in frame {&frame-name}  = ems5.empresa.cod_empresa
                   c-des_empresa            :screen-value in frame {&frame-name} = ems5.empresa.nom_razao_social.
            return no-apply.
        end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_param_emp.cod_empresa V-table-Win
ON LEAVE OF es_param_emp.cod_empresa IN FRAME f-main /* Empresa */
DO:
 {include/leave.i &tabela=ems5.empresa
                &atributo-ref=nom_razao_social
                &variavel-ref=c-des_empresa
                &where="ems5.empresa.cod_empresa = input frame {&frame-name} es_param_emp.cod_empresa"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_param_emp.cod_empresa V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es_param_emp.cod_empresa IN FRAME f-main /* Empresa */
DO:
  apply "F5":U to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es_param_emp.cod_prog_dtsul
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_param_emp.cod_prog_dtsul V-table-Win
ON F5 OF es_param_emp.cod_prog_dtsul IN FRAME f-main /* Programa */
DO:
    assign v_rec_prog_dtsul = ?.
    run men/men012ka.p.

    if  v_rec_prog_dtsul <> ? then
        for first prog_dtsul no-lock
            where recid(prog_dtsul) = v_rec_prog_dtsul:
            assign es_param_emp.cod_prog_dtsul:screen-value in frame {&frame-name} = prog_dtsul.cod_prog_dtsul
                   c-des_prog_dtsul            :screen-value in frame {&frame-name} = prog_dtsul.des_prog_dtsul.
            return no-apply.
        end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_param_emp.cod_prog_dtsul V-table-Win
ON LEAVE OF es_param_emp.cod_prog_dtsul IN FRAME f-main /* Programa */
DO:
    {include/leave.i &tabela=prog_dtsul
                     &atributo-ref=des_prog_dtsul
                     &variavel-ref=c-des_prog_dtsul
                     &where="prog_dtsul.cod_prog_dtsul = input frame {&frame-name} es_param_emp.cod_prog_dtsul"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_param_emp.cod_prog_dtsul V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es_param_emp.cod_prog_dtsul IN FRAME f-main /* Programa */
DO:
    apply "F5":U to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
es_param_emp.cod_prog_dtsul:load-mouse-pointer("image/lupa.cur").
es_param_emp.cod_empresa:load-mouse-pointer("image/lupa.cur").

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).
  &ENDIF

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "es_param_emp"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es_param_emp"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME f-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    if  not frame {&frame-name}:validate() then
        return 'ADM-ERROR':U.
    
    /*:T Ponha na pi-validate todas as valida‡äes */
    /*:T NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na pi-validate. */
    run pi-validate.
    if  return-value = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */
    assign es_param_emp.cod_usuar_ult_atualiz = c-seg-usuario
           es_param_emp.dat_ult_atualiz       = today
           es_param_emp.hra_ult_atualiz       = replace(string(time,"HH:MM:SS"),":","").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    apply "LEAVE":U to es_param_emp.cod_prog_dtsul in frame {&frame-name}.
    apply "LEAVE":U to es_param_emp.cod_empresa    in frame {&frame-name}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    if adm-new-record = yes then
        enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    ELSE 
        DISABLE {&ADM-MODIFY-FIELDS} with frame {&frame-name}. 
    &endif

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-parent V-table-Win 
PROCEDURE pi-atualiza-parent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define input parameter v-row-parent-externo as rowid no-undo.
    
    assign v-row-parent = v-row-parent-externo.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-validate V-table-Win 
PROCEDURE pi-validate :
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: NÆo fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida‡äes, pois neste ponto do programa o registro 
  ainda nÆo foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Valida‡Æo de dicion rio */

    do with frame {&frame-name}:
        if  adm-new-record then do:
            if  es_param_emp.cod_prog_dtsul:input-value = "" then do:
                {include/i-vldprg.i}
                apply "ENTRY":U to es_param_emp.cod_prog_dtsul.
                run utp/ut-msgs.p(input "show":U, input 3553, input "Programa").
                return 'ADM-ERROR':U.
            end.

            find first prog_dtsul no-lock
                 where prog_dtsul.cod_prog_dtsul = es_param_emp.cod_prog_dtsul:input-value no-error.
            if  not avail prog_dtsul then do:
                {include/i-vldprg.i}
                apply "ENTRY":U to es_param_emp.cod_prog_dtsul.
                run utp/ut-msgs.p(input "show":U, input 2, input "Programa").
                return 'ADM-ERROR':U.
            end.

            if  es_param_emp.cod_referencia:input-value = "" then do:
                {include/i-vldprg.i}
                apply "ENTRY":U to es_param_emp.cod_referencia.
                run utp/ut-msgs.p(input "show":U, input 3553, input "Referˆncia").
                return 'ADM-ERROR':U.
            end.

            if  es_param_emp.dat_valid_ini:input-value = ? then do:
                {include/i-vldprg.i}
                apply "ENTRY":U to es_param_emp.dat_valid_ini.
                run utp/ut-msgs.p(input "show":U, input 3553, input "Data Validade Inicial").
                return 'ADM-ERROR':U.
            end.
        end.
        if  es_param_emp.dat_valid_fim:input-value = ? then do:
            {include/i-vldprg.i}
            apply "ENTRY":U to es_param_emp.dat_valid_fim.
            run utp/ut-msgs.p(input "show":U, input 3553, input "Data Validade Final").
            return 'ADM-ERROR':U.
        end.

        if  es_param_emp.dat_valid_ini:input-value > es_param_emp.dat_valid_fim:input-value then do:
            {include/i-vldprg.i}
            apply "ENTRY":U to es_param_emp.dat_valid_fim.
            run utp/ut-msgs.p(input "show":U, input 1168, input "Data Validade").
            return 'ADM-ERROR':U.
        end.

        if  es_param_emp.cod_empresa:input-value = ""  then do:
            {include/i-vldprg.i}
            apply "ENTRY":U to es_param_emp.cod_empresa.
            run utp/ut-msgs.p(input "show":U, input 3553, input "Empresa").
            return 'ADM-ERROR':U.
        end.

        IF NOT CAN-FIND(FIRST ems5.empresa WHERE cod_empresa = es_param_emp.cod_empresa:input-value)  then do:
            {include/i-vldprg.i}
            apply "ENTRY":U to es_param_emp.cod_empresa.
            run utp/ut-msgs.p(input "show":U, input 2, input "Empresa").
            return 'ADM-ERROR':U.
        end.

        IF CAN-FIND(FIRST es_param_emp WHERE cod_empresa    = es_param_emp.cod_empresa:INPUT-VALUE 
                                         AND cod_referencia = es_param_emp.cod_referencia:input-value  
                                         AND cod_prog_dtsul = es_param_emp.cod_prog_dtsul:input-value 
                                         AND dat_valid_ini  = es_param_emp.dat_valid_ini:input-value )  then do:
            {include/i-vldprg.i}
            apply "ENTRY":U to es_param_emp.cod_prog_dtsul.
            run utp/ut-msgs.p(input "show":U, input 17006, input "Parƒmetro Espec¡fico j  existe~~J  existe um Parƒmetro Espec¡fico com os campos Programa, referˆncia, Empresa e Data de validade.").
            return 'ADM-ERROR':U.
        end.

    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "es_param_emp"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
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
      {src/adm/template/vstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


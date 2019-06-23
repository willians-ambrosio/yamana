&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          hresp            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESTIP0003v 2.12.00.004}

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
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */

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
&Scoped-define EXTERNAL-TABLES es_import_ppr
&Scoped-define FIRST-EXTERNAL-TABLE es_import_ppr


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es_import_ppr.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es_import_ppr.cdn_empresa ~
es_import_ppr.cdn_estab es_import_ppr.cdn_funcionario ~
es_import_ppr.cdn_cargo_basic es_import_ppr.sequencia ~
es_import_ppr.log_livre_1 es_import_ppr.num_anotip ~
es_import_ppr.val_percent 
&Scoped-define ENABLED-TABLES es_import_ppr
&Scoped-define FIRST-ENABLED-TABLE es_import_ppr
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS es_import_ppr.cdn_empresa ~
es_import_ppr.cdn_estab es_import_ppr.cdn_funcionario ~
es_import_ppr.cdn_cargo_basic es_import_ppr.sequencia ~
es_import_ppr.log_livre_1 es_import_ppr.num_anotip ~
es_import_ppr.val_percent 
&Scoped-define DISPLAYED-TABLES es_import_ppr
&Scoped-define FIRST-DISPLAYED-TABLE es_import_ppr
&Scoped-Define DISPLAYED-OBJECTS v_nom_pessoa_jurid fi-nom-func ~
fi-nom-cargo 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */

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
DEFINE VARIABLE fi-nom-cargo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nom-func AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39.29 BY .88 NO-UNDO.

DEFINE VARIABLE v_nom_empresa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42.43 BY .88 NO-UNDO.

DEFINE VARIABLE v_nom_pessoa_jurid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40.57 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 5.75.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 2.83.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es_import_ppr.cdn_empresa AT ROW 1.46 COL 29 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     v_nom_empresa AT ROW 1.5 COL 36.57 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     v_nom_pessoa_jurid AT ROW 2.42 COL 38.43 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     es_import_ppr.cdn_estab AT ROW 2.46 COL 29 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
     es_import_ppr.cdn_funcionario AT ROW 3.5 COL 29 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 10.29 BY .88
     fi-nom-func AT ROW 3.5 COL 39.72 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     es_import_ppr.cdn_cargo_basic AT ROW 4.54 COL 29 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     fi-nom-cargo AT ROW 4.54 COL 37.14 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     es_import_ppr.sequencia AT ROW 5.58 COL 29 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .88
     es_import_ppr.log_livre_1 AT ROW 5.67 COL 40.57 HELP
          "Atributo livre para uso interno" WIDGET-ID 24
          LABEL "Planilha"
          VIEW-AS TOGGLE-BOX
          SIZE 11.57 BY .83
     es_import_ppr.num_anotip AT ROW 7.25 COL 29 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 5.72 BY 1
     es_import_ppr.val_percent AT ROW 8.46 COL 29.14 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 6.92 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: hresp.es_import_ppr
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
         HEIGHT             = 8.79
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

/* SETTINGS FOR FILL-IN fi-nom-cargo IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nom-func IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX es_import_ppr.log_livre_1 IN FRAME f-main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN v_nom_empresa IN FRAME f-main
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN v_nom_pessoa_jurid IN FRAME f-main
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

&Scoped-define SELF-NAME es_import_ppr.cdn_cargo_basic
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_import_ppr.cdn_cargo_basic V-table-Win
ON LEAVE OF es_import_ppr.cdn_cargo_basic IN FRAME f-main /* Cargo B sico */
DO:
  
    FIND FIRST cargo_basic WHERE cargo_basic.cdn_cargo_basic = INT(es_import_ppr.cdn_cargo_basic) NO-LOCK NO-ERROR.
    IF AVAIL cargo_basic THEN
        ASSIGN fi-nom-cargo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(cargo_basic.des_cargo_basic).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es_import_ppr.cdn_empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_import_ppr.cdn_empresa V-table-Win
ON F5 OF es_import_ppr.cdn_empresa IN FRAME f-main /* Empresa */
DO:
    {include/zoomvar.i &prog-zoom=object/sopy/zoom/z02py298.w
                       &campo=es_import_ppr.cdn_empresa
                       &campozoom=cdn_empresa
                       &campo2=es_import_ppr.cdn_estab
                       &campozoom2=cdn_estab
                       &campo3=v_nom_empresa
                       &campozoom3=nom_pessoa_jurid}
                       
    APPLY 'leave' TO es_import_ppr.cdn_empresa IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_import_ppr.cdn_empresa V-table-Win
ON LEAVE OF es_import_ppr.cdn_empresa IN FRAME f-main /* Empresa */
DO:

    FIND FIRST mgcad.empresa
        WHERE empresa.ep-codigo = INPUT FRAME {&FRAME-NAME} es_import_ppr.cdn_empresa NO-LOCK NO-ERROR.
    IF AVAIL empresa THEN
        ASSIGN v_nom_empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = empresa.razao-social.
    ELSE
        ASSIGN v_nom_empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es_import_ppr.cdn_estab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_import_ppr.cdn_estab V-table-Win
ON LEAVE OF es_import_ppr.cdn_estab IN FRAME f-main /* Estabelecimento */
DO:
    find first rh_estab no-lock where
               string(rh_estab.cdn_empresa)     = input frame {&FRAME-NAME} es_import_ppr.cdn_empresa and
               string(rh_estab.cdn_estab)       = input frame {&FRAME-NAME} es_import_ppr.cdn_estab   no-error.
    if avail rh_estab then
        assign v_nom_pessoa_jurid:screen-value in frame {&FRAME-NAME} = rh_estab.nom_pessoa_jurid.
    else
        assign v_nom_pessoa_jurid:screen-value in frame {&FRAME-NAME} = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es_import_ppr.cdn_funcionario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_import_ppr.cdn_funcionario V-table-Win
ON LEAVE OF es_import_ppr.cdn_funcionario IN FRAME f-main /* Matr¡culo */
DO:
  
    FIND FIRST funcionario WHERE 
               funcionario.cdn_empresa     = INPUT FRAME {&FRAME-NAME} es_import_ppr.cdn_empresa AND
               funcionario.cdn_estab       = INPUT FRAME {&FRAME-NAME} es_import_ppr.cdn_estab AND
               funcionario.cdn_funcionario = INPUT FRAME {&FRAME-NAME} es_import_ppr.cdn_funcionario NO-LOCK NO-ERROR.
    IF AVAIL funcionario THEN
        ASSIGN fi-nom-func:SCREEN-VALUE IN FRAME {&FRAME-NAME} = funcionario.nom_pessoa_fisic.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

    es_import_ppr.cdn_empresa:load-mouse-pointer('image/lupa.cur') in frame {&FRAME-NAME}.
    es_import_ppr.cdn_estab:load-mouse-pointer('image/lupa.cur')   in frame {&FRAME-NAME}.
    es_import_ppr.cdn_funcionario:LOAD-MOUSE-POINTER('image/lupa.cur') IN FRAME {&FRAME-NAME}.
    es_import_ppr.cdn_cargo_basic:LOAD-MOUSE-POINTER('image/lupa.cur') IN FRAME {&FRAME-NAME}.

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
  {src/adm/template/row-list.i "es_import_ppr"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es_import_ppr"}

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
    {include/i-valid.i}
    
    /*:T Ponha na pi-validate todas as valida‡äes */
    /*:T NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

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
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  apply 'LEAVE':U to es_import_ppr.cdn_empresa in frame {&FRAME-NAME}.
  apply 'LEAVE':U to es_import_ppr.cdn_estab in frame {&FRAME-NAME}.
  apply 'LEAVE':U to es_import_ppr.cdn_funcionario in frame {&FRAME-NAME}.
  apply 'LEAVE':U to es_import_ppr.cdn_cargo_basic in frame {&FRAME-NAME}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: NÆo fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida‡äes, pois neste ponto do programa o registro 
  ainda nÆo foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Valida‡Æo de dicion rio */
    
/*:T    Segue um exemplo de valida‡Æo de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */
      
      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */

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
  {src/adm/template/snd-list.i "es_import_ppr"}

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


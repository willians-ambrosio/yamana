&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          hcm              PROGRESS
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
{include/i-prgvrs.i ESTIP00012v 2.12.00.004}

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
def var v_cdn_empresa  like param_empres_rh.cdn_empresa no-undo.

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
&Scoped-define EXTERNAL-TABLES es_controle_pag
&Scoped-define FIRST-EXTERNAL-TABLE es_controle_pag


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es_controle_pag.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es_controle_pag.cdn_empresa ~
es_controle_pag.cdn_estab es_controle_pag.cdn_niv_hier_funcnal ~
es_controle_pag.num_anotip es_controle_pag.num_qtdsal 
&Scoped-define ENABLED-TABLES es_controle_pag
&Scoped-define FIRST-ENABLED-TABLE es_controle_pag
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS es_controle_pag.cdn_empresa ~
es_controle_pag.cdn_estab es_controle_pag.cdn_niv_hier_funcnal ~
es_controle_pag.num_anotip es_controle_pag.num_qtdsal 
&Scoped-define DISPLAYED-TABLES es_controle_pag
&Scoped-define FIRST-DISPLAYED-TABLE es_controle_pag
&Scoped-Define DISPLAYED-OBJECTS c-nome-estab c-desc-hier 

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
DEFINE VARIABLE c-desc-hier AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY .88 NO-UNDO.

DEFINE VARIABLE c-nome-empresa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33.43 BY .88 NO-UNDO.

DEFINE VARIABLE c-nome-estab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 4.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 4.04.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es_controle_pag.cdn_empresa AT ROW 1.63 COL 29.14 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     c-nome-empresa AT ROW 1.63 COL 36.72 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     es_controle_pag.cdn_estab AT ROW 2.63 COL 29.14 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
     c-nome-estab AT ROW 2.63 COL 38.72 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     es_controle_pag.cdn_niv_hier_funcnal AT ROW 3.63 COL 29.14 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .88
     c-desc-hier AT ROW 3.63 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     es_controle_pag.num_anotip AT ROW 6.25 COL 29 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .88
     es_controle_pag.num_qtdsal AT ROW 7.29 COL 29 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 7 BY .88
     rt-key AT ROW 1.04 COL 1
     rt-mold AT ROW 5.21 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: hcmesp.es_controle_pag
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
         HEIGHT             = 8.33
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

/* SETTINGS FOR FILL-IN c-desc-hier IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-nome-empresa IN FRAME f-main
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN c-nome-estab IN FRAME f-main
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

&Scoped-define SELF-NAME es_controle_pag.cdn_empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_controle_pag.cdn_empresa V-table-Win
ON F5 OF es_controle_pag.cdn_empresa IN FRAME f-main /* Empresa */
DO:
  APPLY "MOUSE-SELECT-DBL-CLICK":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_controle_pag.cdn_empresa V-table-Win
ON LEAVE OF es_controle_pag.cdn_empresa IN FRAME f-main /* Empresa */
DO:
   RUN pi-empresa.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_controle_pag.cdn_empresa V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es_controle_pag.cdn_empresa IN FRAME f-main /* Empresa */
DO:
    {include/zoomvar.i &prog-zoom=object/sopy/zoom/z02py298.w
                       &campo=es_controle_pag.cdn_empresa
                       &campozoom=cdn_empresa
                       &campo2=es_controle_pag.cdn_estab
                       &campozoom2=cdn_estab
                       &campo3=c-nome-empresa
                       &campozoom3=nom_pessoa_jurid}
                       
    APPLY 'leave' TO es_controle_pag.cdn_empresa IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es_controle_pag.cdn_estab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_controle_pag.cdn_estab V-table-Win
ON LEAVE OF es_controle_pag.cdn_estab IN FRAME f-main /* Estabelecimento */
DO:
   RUN pi-estab.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_controle_pag.cdn_estab V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es_controle_pag.cdn_estab IN FRAME f-main /* Estabelecimento */
DO:
  
    {include/zoomvar.i &prog-zoom=object/sopy/zoom/z02py298.w
                       &campo=es_controle_pag.cdn_estab
                       &campozoom=cdn_estab}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es_controle_pag.cdn_niv_hier_funcnal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_controle_pag.cdn_niv_hier_funcnal V-table-Win
ON F5 OF es_controle_pag.cdn_niv_hier_funcnal IN FRAME f-main /* N¡vel Hier rquico */
DO:
  APPLY "MOUSE-SELECT-DBL-CLICK":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_controle_pag.cdn_niv_hier_funcnal V-table-Win
ON LEAVE OF es_controle_pag.cdn_niv_hier_funcnal IN FRAME f-main /* N¡vel Hier rquico */
DO:
    FIND FIRST niv_hier_funcnal WHERE niv_hier_funcnal.cdn_niv_hier = int(es_controle_pag.cdn_niv_hier:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
    IF NOT AVAIL niv_hier_funcnal THEN DO:
        RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N¡vel Hier rquico Invalido." + "~~" + "Favor verificar.").
        RETURN NO-APPLY.
    END.
    ELSE DO:
        ASSIGN c-desc-hier:SCREEN-VALUE IN FRAME {&FRAME-NAME} = niv_hier_funcnal.des_niv_hier_funcnal.
    END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_controle_pag.cdn_niv_hier_funcnal V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es_controle_pag.cdn_niv_hier_funcnal IN FRAME f-main /* N¡vel Hier rquico */
DO:
    DEFINE VARIABLE l-implanta AS LOGICAL     NO-UNDO.
    assign l-implanta = yes.
      {include/zoomvar.i &prog-zoom=object/sopy/zoom/z01py090.w
                         &campo=es_controle_pag.cdn_niv_hier_funcnal
                         &campozoom=cdn_niv_hier_funcnal
                         &campo2=c-desc-hier
                         &campozoom2=des_niv_hier_funcnal} 
                         


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
es_controle_pag.cdn_empresa:LOAD-MOUSE-POINTER("image/lupa.cur").
es_controle_pag.cdn_estab:LOAD-MOUSE-POINTER("image/lupa.cur").
es_controle_pag.cdn_niv_hier_funcnal:LOAD-MOUSE-POINTER("image/lupa.cur").

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
  {src/adm/template/row-list.i "es_controle_pag"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es_controle_pag"}

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
    RUN pi-validate.
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.

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
  ASSIGN  c-nome-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
          c-desc-hier:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = "".


  RUN pi-estab.
  RUN pi-empresa.

  FIND FIRST niv_hier_funcnal
       WHERE niv_hier_funcnal.cdn_niv_hier = int(es_controle_pag.cdn_niv_hier:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
  IF AVAIL niv_hier_funcnal THEN DO:
      ASSIGN c-desc-hier:SCREEN-VALUE IN FRAME {&FRAME-NAME} = niv_hier_funcnal.des_niv_hier_funcnal.
  END.





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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-empresa V-table-Win 
PROCEDURE pi-empresa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF INPUT FRAME {&FRAME-NAME} es_controle_pag.cdn_empresa <> "" THEN DO:
    FIND FIRST hcm.empresa
        WHERE empresa.ep-codigo = INPUT FRAME {&FRAME-NAME} es_controle_pag.cdn_empresa NO-LOCK NO-ERROR.
    IF AVAIL empresa THEN
        ASSIGN c-nome-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = empresa.razao-social.
    ELSE
        ASSIGN c-nome-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-estab V-table-Win 
PROCEDURE pi-estab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF INPUT FRAME {&FRAME-NAME} es_controle_pag.cdn_estab <> "" THEN DO:
    FIND FIRST rh_estab
        WHERE rh_estab.cdn_estab = INPUT FRAME {&FRAME-NAME} es_controle_pag.cdn_estab NO-LOCK NO-ERROR.
    IF AVAIL rh_estab THEN
        ASSIGN c-nome-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = rh_estab.nom_pessoa_jurid.
    ELSE
        ASSIGN c-nome-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''.
END.
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
    {include/i-vldprg.i} 
/*:T    Segue um exemplo de valida‡Æo de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */
      
      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */
          /*
IF int(es_controle_pag.cdn_niv_hier:SCREEN-VALUE IN FRAME {&FRAME-NAME}) > 0 THEN DO:
    FIND FIRST niv_hier_funcnal WHERE niv_hier_funcnal.cdn_niv_hier = int(es_controle_pag.cdn_niv_hier:SCREEN-VALUE IN FRAME {&FRAME-NAME}) NO-LOCK NO-ERROR.
    IF NOT AVAIL niv_hier_funcnal THEN
        RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N¡vel Hier rquico Invalido." + "~~" + "Favor verificar.").
        RETURN "adm-error".
END.        */

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
  {src/adm/template/snd-list.i "es_controle_pag"}

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


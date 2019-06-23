&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
                    PROGRESS
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
{include/i-prgvrs.i YMVWR003 1.02.00.007 } /*** 010007 ***/

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m¢dulo dever† ser MUT                    */

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
&Scoped-define EXTERNAL-TABLES tip_metricas_plr
&Scoped-define FIRST-EXTERNAL-TABLE tip_metricas_plr


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR tip_metricas_plr.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tip_metricas_plr.cdn_empresa ~
tip_metricas_plr.cdn_estab tip_metricas_plr.log_corporativo 
&Scoped-define ENABLED-TABLES tip_metricas_plr
&Scoped-define FIRST-ENABLED-TABLE tip_metricas_plr
&Scoped-Define ENABLED-OBJECTS rt-key 
&Scoped-Define DISPLAYED-FIELDS tip_metricas_plr.cdn_empresa ~
tip_metricas_plr.cdn_estab tip_metricas_plr.log_corporativo 
&Scoped-define DISPLAYED-TABLES tip_metricas_plr
&Scoped-define FIRST-DISPLAYED-TABLE tip_metricas_plr
&Scoped-Define DISPLAYED-OBJECTS fi-desc-empresa fi-desc-estab 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS tip_metricas_plr.cdn_empresa ~
tip_metricas_plr.cdn_estab tip_metricas_plr.log_corporativo 

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
DEFINE VARIABLE fi-desc-empresa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-estab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 3.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     tip_metricas_plr.cdn_empresa AT ROW 1.17 COL 28.29 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     tip_metricas_plr.cdn_estab AT ROW 2.17 COL 28.29 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
     tip_metricas_plr.log_corporativo AT ROW 3.17 COL 28.29 WIDGET-ID 6
          VIEW-AS TOGGLE-BOX
          SIZE 22 BY .83
     fi-desc-empresa AT ROW 1.17 COL 36 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fi-desc-estab AT ROW 2.17 COL 38 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     rt-key AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: tip_metricas_plr
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
         HEIGHT             = 3.25
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit L-To-R,COLUMNS                    */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN tip_metricas_plr.cdn_empresa IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN tip_metricas_plr.cdn_estab IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-desc-empresa IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-estab IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tip_metricas_plr.log_corporativo IN FRAME f-main
   1                                                                    */
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

&Scoped-define SELF-NAME tip_metricas_plr.cdn_empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tip_metricas_plr.cdn_empresa V-table-Win
ON F5 OF tip_metricas_plr.cdn_empresa IN FRAME f-main /* Empresa */
DO:
    {include/zoomvar.i &prog-zoom=object/sopy/zoom/z04py197.w
                       &campo=tip_metricas_plr.cdn_empresa
                       &campozoom=ep-codigo
                       &campo2=fi-desc-empresa
                       &campozoom2=razao-social}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tip_metricas_plr.cdn_empresa V-table-Win
ON LEAVE OF tip_metricas_plr.cdn_empresa IN FRAME f-main /* Empresa */
DO:
  {include/leave.i &tabela=empresa
                   &atributo-ref=razao-social
                   &variavel-ref=fi-desc-empresa
                   &where="empresa.ep-codigo = input frame {&frame-name} tip_metricas_plr.cdn_empresa"}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tip_metricas_plr.cdn_empresa V-table-Win
ON MOUSE-SELECT-DBLCLICK OF tip_metricas_plr.cdn_empresa IN FRAME f-main /* Empresa */
DO:
  APPLY 'F5':U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tip_metricas_plr.cdn_estab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tip_metricas_plr.cdn_estab V-table-Win
ON F5 OF tip_metricas_plr.cdn_estab IN FRAME f-main /* Estabelecimento */
DO:
  {include/zoomvar.i &prog-zoom="object/sopy/zoom/z02py298.w"
                     &campo=tip_metricas_plr.cdn_estab
                     &campo2=fi-desc-estab
                     &campozoom=cdn_estab
                     &campozoom2=nom_pessoa_jurid
                     &parametros="run pi-seta-inicial in wh-pesquisa (input frame {&frame-name} tip_metricas_plr.cdn_empresa)".}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tip_metricas_plr.cdn_estab V-table-Win
ON LEAVE OF tip_metricas_plr.cdn_estab IN FRAME f-main /* Estabelecimento */
DO:
  {include/leave.i &tabela=rh_estab
                   &atributo-ref=nom_pessoa_jurid
                   &variavel-ref=fi-desc-estab
                   &where="rh_estab.cdn_estab = input frame {&frame-name} tip_metricas_plr.cdn_estab 
                           AND rh_estab.cdn_empresa = input frame {&frame-name} tip_metricas_plr.cdn_empresa "}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tip_metricas_plr.cdn_estab V-table-Win
ON MOUSE-SELECT-DBLCLICK OF tip_metricas_plr.cdn_estab IN FRAME f-main /* Estabelecimento */
DO:
  APPLY "F5":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  

  tip_metricas_plr.cdn_empresa:load-mouse-pointer("image/lupa.cur").
  tip_metricas_plr.cdn_estab:load-mouse-pointer("image/lupa.cur").
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
  {src/adm/template/row-list.i "tip_metricas_plr"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "tip_metricas_plr"}

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

    RUN pi-validate.
    IF RETURN-VALUE = "ADM-ERROR":U THEN
        RETURN "ADM-ERROR":U.
    
    /*:T Ponha na pi-validate todas as validaá‰es */
    /*:T N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
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
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  APPLY "leave" TO tip_metricas_plr.cdn_empresa IN FRAME {&FRAME-NAME}.
  APPLY "leave" TO tip_metricas_plr.cdn_estab IN FRAME {&FRAME-NAME}. 

  /* Code placed here will execute AFTER standard behavior.    */

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
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
    IF NOT CAN-FIND (FIRST empresa
                     WHERE empresa.ep-codigo = tip_metricas_plr.cdn_empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN DO:
        run utp/ut-msgs.p (input "show":U, input 2, INPUT  "Empresa").
        APPLY "ENTRY":U TO tip_metricas_plr.cdn_empresa IN FRAME {&FRAME-NAME}.
        RETURN "ADM-ERROR":U.
    END.

    IF NOT CAN-FIND (FIRST rh_estab
                     WHERE rh_estab.cdn_estab = tip_metricas_plr.cdn_estab:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN DO:
        run utp/ut-msgs.p (input "show":U, input 2, INPUT  "Estabelecimento").
        APPLY "ENTRY":U TO tip_metricas_plr.cdn_estab IN FRAME {&FRAME-NAME}.
        RETURN "ADM-ERROR":U.
    END.

    IF NOT CAN-FIND (FIRST rh_estab
                     WHERE rh_estab.cdn_estab = tip_metricas_plr.cdn_estab:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                       AND rh_estab.cdn_empresa = tip_metricas_plr.cdn_empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN DO:
        run utp/ut-msgs.p (input "show":U, input 17006, INPUT  "Estabelecimento n∆o vinculado Ö empresa"  
                           + "~~" + "O estabelecimento " + tip_metricas_plr.cdn_estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
                           + " n∆o possui vinculo com a empresa " + tip_metricas_plr.cdn_empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
        APPLY "ENTRY":U TO tip_metricas_plr.cdn_estab IN FRAME {&FRAME-NAME}.
        RETURN "ADM-ERROR":U.
    END.

    IF CAN-FIND (FIRST tip_metricas_plr
                 WHERE tip_metricas_plr.cdn_empresa = tip_metricas_plr.cdn_empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
                   AND tip_metricas_plr.cdn_estab = tip_metricas_plr.cdn_estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
                   AND tip_metricas_plr.LOG_corporativo = tip_metricas_plr.log_corporativo:CHECKED IN FRAME {&FRAME-NAME})  THEN DO:
        run utp/ut-msgs.p (input "show":U, input 1, INPUT  "MÇtricas PLR").
        APPLY "ENTRY":U TO tip_metricas_plr.cdn_empresa IN FRAME {&FRAME-NAME}.
        RETURN "ADM-ERROR":U.
    END.

    RETURN "OK".

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
  {src/adm/template/snd-list.i "tip_metricas_plr"}

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


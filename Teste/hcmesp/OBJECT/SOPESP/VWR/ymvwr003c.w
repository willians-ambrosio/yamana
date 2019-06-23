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
{include/i-prgvrs.i YMVWR003C 1.02.00.006 } /*** 010006 ***/

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m¢dulo dever  ser MUT                    */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ymvwr003c mfp}
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
DEF VAR l-update AS LOGICAL INITIAL NO NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES tip_targets
&Scoped-define FIRST-EXTERNAL-TABLE tip_targets


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR tip_targets.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tip_targets.result tip_targets.dt_inicio ~
tip_targets.ind_diretor tip_targets.ind_gerente tip_targets.ind_coordenador ~
tip_targets.ind_superv_sr tip_targets.ind_demais 
&Scoped-define ENABLED-TABLES tip_targets
&Scoped-define FIRST-ENABLED-TABLE tip_targets
&Scoped-Define ENABLED-OBJECTS rt-mold RECT-3 
&Scoped-Define DISPLAYED-FIELDS tip_targets.result tip_targets.dt_inicio ~
tip_targets.dt_termino tip_targets.ind_diretor tip_targets.ind_gerente ~
tip_targets.ind_coordenador tip_targets.ind_superv_sr ~
tip_targets.ind_demais 
&Scoped-define DISPLAYED-TABLES tip_targets
&Scoped-define FIRST-DISPLAYED-TABLE tip_targets


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
DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62.43 BY 2.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 6.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     tip_targets.result AT ROW 1.21 COL 16.86 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     tip_targets.dt_inicio AT ROW 2.21 COL 16.86 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     tip_targets.dt_termino AT ROW 3.46 COL 16.86 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     tip_targets.ind_diretor AT ROW 4.46 COL 16.86 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     tip_targets.ind_gerente AT ROW 5.46 COL 16.86 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     tip_targets.ind_coordenador AT ROW 6.46 COL 16.86 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     tip_targets.ind_superv_sr AT ROW 7.46 COL 16.86 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     tip_targets.ind_demais AT ROW 8.46 COL 16.86 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     rt-mold AT ROW 3.38 COL 1.29
     RECT-3 AT ROW 1 COL 1 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: tip_targets
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
         HEIGHT             = 8.5
         WIDTH              = 62.43.
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

/* SETTINGS FOR FILL-IN tip_targets.dt_termino IN FRAME f-main
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

&Scoped-define SELF-NAME tip_targets.dt_inicio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tip_targets.dt_inicio V-table-Win
ON LEAVE OF tip_targets.dt_inicio IN FRAME f-main /* Data In¡cio */
DO:
  ASSIGN tip_targets.dt_termino:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "31/12/9999".
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
  {src/adm/template/row-list.i "tip_targets"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "tip_targets"}

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
    DEFINE VARIABLE horas AS INTEGER     NO-UNDO.
    DEFINE VARIABLE minutos AS INTEGER     NO-UNDO.
    DEFINE VARIABLE segundos AS INTEGER     NO-UNDO.
    DEFINE VARIABLE horario AS INTEGER     NO-UNDO.
    DEFINE VARIABLE horaInclusao AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE v_result AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v_dt_inicio AS DATE    NO-UNDO.          

    /* Code placed here will execute PRIOR to standard behavior. */
    RUN pi-validate(INPUT "").
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Ponha na pi-validate todas as valida‡äes */
    /*:T NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

    ASSIGN horario = TIME.
    ASSIGN segundos = horario MOD 60.
    ASSIGN horario = (horario - segundos) / 60.
    ASSIGN minutos = horario MOD 60.           
    ASSIGN horas = (horario - minutos) / 60.
    
    ASSIGN horaInclusao = IF horas < 10 THEN "0" + STRING(horas) ELSE STRING(horas).
    ASSIGN horaInclusao = horaInclusao + ":" + IF minutos < 10 THEN "0" + STRING(minutos) ELSE STRING(minutos).
    ASSIGN horaInclusao = horaInclusao + ":" + IF segundos < 10 THEN "0" + STRING(segundos) ELSE STRING(segundos).

    FIND FIRST tip_metricas_plr
         WHERE ROWID(tip_metricas_plr) = v-row-parent NO-LOCK NO-ERROR.
    IF AVAIL tip_metricas_plr THEN DO:
        ASSIGN tip_targets.cdn_empresa = tip_metricas_plr.cdn_empresa
               tip_targets.cdn_estab = tip_metricas_plr.cdn_estab
               tip_targets.cod_usuario = v_cod_usuar_corren
               tip_targets.dt_inclusao = TODAY
               tip_targets.hr_inclusao = horaInclusao
               tip_targets.LOG_corporativo = tip_metricas_plr.LOG_corporativo
               v_dt_inicio = tip_targets.dt_inicio
               v_result = tip_targets.RESULT.
        
        IF l-update = NO THEN DO:
            ASSIGN tip_targets.dt_termino = 12/31/9999.
        END.
        ELSE DO:
            FOR EACH  tip_est_targets_pag
                WHERE tip_est_targets_pag.cdn_empresa = tip_targets.cdn_empresa  
                  and tip_est_targets_pag.cdn_estab = tip_targets.cdn_estab  
                  and tip_est_targets_pag.LOG_corporativo = tip_targets.LOG_corporativo
                  and tip_est_targets_pag.result = tip_targets.result 
                  and tip_est_targets_pag.dt_inicio >= tip_targets.dt_inicio
                  and tip_est_targets_pag.dt_inicio <= tip_targets.dt_termino EXCLUSIVE-LOCK:
                ASSIGN tip_est_targets_pag.ind_diretor = tip_targets.ind_diretor
                       tip_est_targets_pag.ind_gerente = tip_targets.ind_gerente
                       tip_est_targets_pag.ind_coordenador = tip_targets.ind_coordenador
                       tip_est_targets_pag.ind_superv_sr = tip_targets.ind_superv_sr
                       tip_est_targets_pag.ind_demais = tip_targets.ind_demais.
            END.
            FOR EACH  tip_est_targets_prov
                WHERE tip_est_targets_prov.cdn_empresa = tip_targets.cdn_empresa  
                  and tip_est_targets_prov.cdn_estab = tip_targets.cdn_estab  
                  and tip_est_targets_prov.LOG_corporativo = tip_targets.LOG_corporativo
                  and tip_est_targets_prov.result = tip_targets.result 
                  and tip_est_targets_prov.dt_inicio >= tip_targets.dt_inicio
                  and tip_est_targets_prov.dt_inicio <= tip_targets.dt_termino EXCLUSIVE-LOCK:
                ASSIGN tip_est_targets_prov.ind_diretor = tip_targets.ind_diretor          
                       tip_est_targets_prov.ind_gerente = tip_targets.ind_gerente          
                       tip_est_targets_prov.ind_coordenador = tip_targets.ind_coordenador  
                       tip_est_targets_prov.ind_superv_sr = tip_targets.ind_superv_sr      
                       tip_est_targets_prov.ind_demais = tip_targets.ind_demais.           
            END.
            FOR EACH  tiph_est_targets_pag
                WHERE tiph_est_targets_pag.cdn_empresa = tip_targets.cdn_empresa  
                  and tiph_est_targets_pag.cdn_estab = tip_targets.cdn_estab  
                  and tiph_est_targets_pag.LOG_corporativo = tip_targets.LOG_corporativo
                  and tiph_est_targets_pag.result = tip_targets.result 
                  and tiph_est_targets_pag.dt_inicio >= tip_targets.dt_inicio
                  and tiph_est_targets_pag.dt_inicio <= tip_targets.dt_termino EXCLUSIVE-LOCK:
                ASSIGN tiph_est_targets_pag.ind_diretor = tip_targets.ind_diretor          
                       tiph_est_targets_pag.ind_gerente = tip_targets.ind_gerente          
                       tiph_est_targets_pag.ind_coordenador = tip_targets.ind_coordenador  
                       tiph_est_targets_pag.ind_superv_sr = tip_targets.ind_superv_sr      
                       tiph_est_targets_pag.ind_demais = tip_targets.ind_demais.           
            END. 
            FOR EACH  tiph_est_targets_prov
                WHERE tiph_est_targets_prov.cdn_empresa = tip_targets.cdn_empresa  
                  and tiph_est_targets_prov.cdn_estab = tip_targets.cdn_estab  
                  and tiph_est_targets_prov.LOG_corporativo = tip_targets.LOG_corporativo
                  and tiph_est_targets_prov.result = tip_targets.result 
                  and tiph_est_targets_prov.dt_inicio >= tip_targets.dt_inicio
                  and tiph_est_targets_prov.dt_inicio <= tip_targets.dt_termino EXCLUSIVE-LOCK:
                ASSIGN tiph_est_targets_prov.ind_diretor = tip_targets.ind_diretor          
                       tiph_est_targets_prov.ind_gerente = tip_targets.ind_gerente          
                       tiph_est_targets_prov.ind_coordenador = tip_targets.ind_coordenador  
                       tiph_est_targets_prov.ind_superv_sr = tip_targets.ind_superv_sr      
                       tiph_est_targets_prov.ind_demais = tip_targets.ind_demais.           
            END.
        END.

        FOR LAST tip_targets
            WHERE tip_targets.cdn_empresa = tip_metricas_plr.cdn_empresa
              AND tip_targets.cdn_estab = tip_metricas_plr.cdn_estab
              AND tip_targets.LOG_corporativo = tip_metricas_plr.LOG_corporativo
              AND tip_targets.RESULT = v_result
              AND tip_targets.dt_inicio <> v_dt_inicio
              AND tip_targets.dt_inicio < v_dt_inicio
               BY tip_targets.dt_inicio :
            ASSIGN tip_targets.dt_termino = v_dt_inicio - 1.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN pi-validate(INPUT "create").
  if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

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
    DEFINE INPUT PARAMETER state AS CHARACTER NO-UNDO.

    IF state = "create" THEN DO:
        FIND FIRST tip_metricas_plr
             WHERE ROWID(tip_metricas_plr) = v-row-parent NO-LOCK NO-ERROR.
        IF AVAIL tip_metricas_plr THEN DO:
            /*verifica registro duplicado*/
            IF CAN-FIND(FIRST tip_targets
                        WHERE tip_targets.cdn_estab = tip_metricas_plr.cdn_estab
                        AND tip_targets.cdn_empresa = tip_metricas_plr.cdn_empresa
                        AND tip_targets.LOG_corporativo = tip_metricas_plr.LOG_corporativo
                        AND tip_targets.RESULT = DEC(tip_targets.RESULT:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                        AND tip_targets.dt_inicio = date(tip_targets.dt_inicio:SCREEN-VALUE IN FRAME {&FRAME-NAME})) THEN DO:
                run utp/ut-msgs.p (input "show":U, input 1, INPUT  "Tip Targets").
                APPLY "ENTRY":U TO tip_targets.RESULT IN FRAME {&FRAME-NAME}.
                RETURN "ADM-ERROR":U.
            END.
            /*verifica registros posteriores*/
            IF CAN-FIND (FIRST tip_targets
                         WHERE tip_targets.cdn_empresa = tip_metricas_plr.cdn_empresa
                           AND tip_targets.cdn_estab = tip_metricas_plr.cdn_estab
                           AND tip_targets.LOG_corporativo = tip_metricas_plr.LOG_corporativo
                           AND tip_targets.RESULT = DEC(tip_targets.RESULT:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                           AND tip_targets.dt_inicio >= date(tip_targets.dt_inicio:SCREEN-VALUE IN FRAME {&FRAME-NAME}) )  THEN DO:
                RUN utp/ut-msgs.p(INPUT "show":U, INPUT 17006, INPUT "Data de in¡cio inv lida" + "~~" 
                                  + "J  existe um registro Tip Targets com data de in¡cio posterior a " 
                                  + String(tip_targets.dt_inicio:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
                                  + ". Para adicionar registros retroativos ‚ preciso excluir os posteriores.").
                APPLY "ENTRY":U TO tip_targets.dt_inicio IN FRAME {&FRAME-NAME}.
                RETURN "ADM-ERROR":U.
            END.
        END.
    END.

    IF tip_targets.dt_inicio:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO:
        RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "Data de in¡cio inv lida" + "~~" + "Favor informar uma data de in¡cio.").
        APPLY "ENTRY":U TO tip_targets.dt_inicio IN FRAME {&FRAME-NAME}.
        RETURN "ADM-ERROR":U.
    END.
    
    IF day(date(tip_targets.dt_inicio:SCREEN-VALUE IN FRAME {&FRAME-NAME})) <> 1 THEN DO:
        run utp/ut-msgs.p (input "show":U, input 17006, INPUT "Data de in¡cio inv lida" + "~~" + "O dia tem que ser igual a 1.").
        APPLY "ENTRY":U TO tip_targets.dt_inicio IN FRAME {&FRAME-NAME}.
        RETURN "ADM-ERROR":U.
    END.

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
  {src/adm/template/snd-list.i "tip_targets"}

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

  IF p-state = 'UPDATE-BEGIN':U THEN DO:
      ASSIGN l-update = YES
             tip_targets.RESULT:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tip_targets.dt_inicio:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

  END.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


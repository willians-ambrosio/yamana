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
{include/i-prgvrs.i YMVWR003A 1.02.10.000 } /*** 01010004 ***/

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
{utp/ut-glob.i}
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
&Scoped-define EXTERNAL-TABLES tip_indiv_factor
&Scoped-define FIRST-EXTERNAL-TABLE tip_indiv_factor


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR tip_indiv_factor.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS tip_indiv_factor.cdn_indiv_factor ~
tip_indiv_factor.desc_indiv_factor tip_indiv_factor.dt_inicio ~
tip_indiv_factor.indice 
&Scoped-define ENABLED-TABLES tip_indiv_factor
&Scoped-define FIRST-ENABLED-TABLE tip_indiv_factor
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS tip_indiv_factor.cdn_indiv_factor ~
tip_indiv_factor.desc_indiv_factor tip_indiv_factor.dt_inicio ~
tip_indiv_factor.indice tip_indiv_factor.dt_termino 
&Scoped-define DISPLAYED-TABLES tip_indiv_factor
&Scoped-define FIRST-DISPLAYED-TABLE tip_indiv_factor


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
cod_usuario||y|espec.tip_indiv_factor.cod_usuario
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "cod_usuario"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71 BY 2.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71 BY 2.58.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     tip_indiv_factor.cdn_indiv_factor AT ROW 1.17 COL 16 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
     tip_indiv_factor.desc_indiv_factor AT ROW 1.17 COL 26 COLON-ALIGNED NO-LABEL WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 31.14 BY .88
     tip_indiv_factor.dt_inicio AT ROW 2.17 COL 16 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     tip_indiv_factor.indice AT ROW 3.71 COL 16 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     tip_indiv_factor.dt_termino AT ROW 4.71 COL 16 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 3.33 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.tip_indiv_factor
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY
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
         HEIGHT             = 5.21
         WIDTH              = 71.29.
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

/* SETTINGS FOR FILL-IN tip_indiv_factor.dt_termino IN FRAME f-main
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

&Scoped-define SELF-NAME tip_indiv_factor.dt_inicio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tip_indiv_factor.dt_inicio V-table-Win
ON LEAVE OF tip_indiv_factor.dt_inicio IN FRAME f-main /* Data In¡cio */
DO:
  ASSIGN tip_indiv_factor.dt_termino:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "31/12/9999".
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
  {src/adm/template/row-list.i "tip_indiv_factor"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "tip_indiv_factor"}

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
    DEFINE VARIABLE v_cdn_indiv_factor AS INTEGER NO-UNDO.
    DEFINE VARIABLE v_dt_inicio AS DATE    NO-UNDO.

    RUN pi-validate(INPUT "assign").
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.

    /* Code placed here will execute PRIOR to standard behavior. */
    /*{include/i-valid.i}*/
    
    /*:T Ponha na pi-validate todas as valida‡äes */
    /*:T NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  

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
        ASSIGN tip_indiv_factor.cdn_empresa = tip_metricas_plr.cdn_empresa
               tip_indiv_factor.cdn_estab = tip_metricas_plr.cdn_estab
               tip_indiv_factor.LOG_corporativo = tip_metricas_plr.LOG_corporativo
               tip_indiv_factor.cod_usuario = v_cod_usuar_corren
               tip_indiv_factor.dt_inclusao = TODAY    
               tip_indiv_factor.hr_inclusao = horaInclusao
               v_dt_inicio = tip_indiv_factor.dt_inicio
               v_cdn_indiv_factor = tip_indiv_factor.cdn_indiv_factor.
        
        IF l-update = NO THEN DO:
            ASSIGN tip_indiv_factor.dt_termino = 12/31/9999.
        END.
        ELSE DO:
            FOR EACH  tip_func_indiv_factor_pag
                WHERE tip_func_indiv_factor_pag.cdn_empresa = tip_indiv_factor.cdn_empresa  
                  and tip_func_indiv_factor_pag.cdn_estab = tip_indiv_factor.cdn_estab  
                  and tip_func_indiv_factor_pag.LOG_corporativo = tip_indiv_factor.LOG_corporativo
                  and tip_func_indiv_factor_pag.cdn_indiv_factor = tip_indiv_factor.cdn_indiv_factor 
                  and tip_func_indiv_factor_pag.dt_inicio >= tip_indiv_factor.dt_inicio
                  and tip_func_indiv_factor_pag.dt_inicio <= tip_indiv_factor.dt_termino EXCLUSIVE-LOCK:
                ASSIGN tip_func_indiv_factor_pag.DESC_indiv_factor = tip_indiv_factor.DESC_indiv_factor
                       tip_func_indiv_factor_pag.indice = tip_indiv_factor.indice.
            END.
            FOR EACH  tip_func_indiv_factor_prov
                WHERE tip_func_indiv_factor_prov.cdn_empresa = tip_indiv_factor.cdn_empresa  
                  and tip_func_indiv_factor_prov.cdn_estab = tip_indiv_factor.cdn_estab  
                  and tip_func_indiv_factor_prov.LOG_corporativo = tip_indiv_factor.LOG_corporativo
                  and tip_func_indiv_factor_prov.cdn_indiv_factor = tip_indiv_factor.cdn_indiv_factor 
                  and tip_func_indiv_factor_prov.dt_inicio >= tip_indiv_factor.dt_inicio
                  and tip_func_indiv_factor_prov.dt_inicio <= tip_indiv_factor.dt_termino EXCLUSIVE-LOCK:
                ASSIGN tip_func_indiv_factor_prov.DESC_indiv_factor = tip_indiv_factor.DESC_indiv_factor
                       tip_func_indiv_factor_prov.indice = tip_indiv_factor.indice.
            END.
            FOR EACH  tiph_func_indiv_factor_pag
                WHERE tiph_func_indiv_factor_pag.cdn_empresa = tip_indiv_factor.cdn_empresa  
                  and tiph_func_indiv_factor_pag.cdn_estab = tip_indiv_factor.cdn_estab  
                  and tiph_func_indiv_factor_pag.LOG_corporativo = tip_indiv_factor.LOG_corporativo
                  and tiph_func_indiv_factor_pag.cdn_indiv_factor = tip_indiv_factor.cdn_indiv_factor 
                  and tiph_func_indiv_factor_pag.dt_inicio >= tip_indiv_factor.dt_inicio
                  and tiph_func_indiv_factor_pag.dt_inicio <= tip_indiv_factor.dt_termino EXCLUSIVE-LOCK:
                ASSIGN tiph_func_indiv_factor_pag.DESC_indiv_factor = tip_indiv_factor.DESC_indiv_factor
                       tiph_func_indiv_factor_pag.indice = tip_indiv_factor.indice.
            END.
            FOR EACH  tiph_func_indiv_factor_prov
                WHERE tiph_func_indiv_factor_prov.cdn_empresa = tip_indiv_factor.cdn_empresa  
                  and tiph_func_indiv_factor_prov.cdn_estab = tip_indiv_factor.cdn_estab  
                  and tiph_func_indiv_factor_prov.LOG_corporativo = tip_indiv_factor.LOG_corporativo
                  and tiph_func_indiv_factor_prov.cdn_indiv_factor = tip_indiv_factor.cdn_indiv_factor 
                  and tiph_func_indiv_factor_prov.dt_inicio >= tip_indiv_factor.dt_inicio
                  and tiph_func_indiv_factor_prov.dt_inicio <= tip_indiv_factor.dt_termino EXCLUSIVE-LOCK:
                ASSIGN tiph_func_indiv_factor_prov.DESC_indiv_factor = tip_indiv_factor.DESC_indiv_factor
                       tiph_func_indiv_factor_prov.indice = tip_indiv_factor.indice.
            END.
        END.

        FOR LAST tip_indiv_factor
            WHERE tip_indiv_factor.cdn_empresa = tip_metricas_plr.cdn_empresa
              AND tip_indiv_factor.cdn_estab = tip_metricas_plr.cdn_estab
              AND tip_indiv_factor.LOG_corporativo = tip_metricas_plr.LOG_corporativo
              AND tip_indiv_factor.cdn_indiv_factor = v_cdn_indiv_factor         
              AND tip_indiv_factor.dt_inicio <> v_dt_inicio
              AND tip_indiv_factor.dt_inicio < v_dt_inicio
               BY tip_indiv_factor.dt_inicio :
            
            ASSIGN tip_indiv_factor.dt_termino = v_dt_inicio - 1.
        END.
    END.

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN pi-validate(INPUT "create").                             
if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.

RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ).

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
    
    IF tip_indiv_factor.cdn_indiv_factor:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0" THEN DO:
        run utp/ut-msgs.p (input "show":U, input 17006, INPUT "C¢digo inv lido" + "~~" + "Favor informar um c¢digo.").
        APPLY "ENTRY" TO tip_indiv_factor.cdn_indiv_factor IN FRAME {&FRAME-NAME}.
        RETURN "ADM-ERROR":U.
    END.

    IF tip_indiv_factor.desc_indiv_factor:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO:
        run utp/ut-msgs.p (input "show":U, input 17006, INPUT "Descri‡Æo inv lida" + "~~" + "Favor informar uma descri‡Æo.").
        APPLY "ENTRY" TO tip_indiv_factor.desc_indiv_factor IN FRAME {&FRAME-NAME}.
        RETURN "ADM-ERROR":U.
    END.
    
    IF state = "create" THEN DO:
        FIND FIRST tip_metricas_plr
             WHERE ROWID(tip_metricas_plr) = v-row-parent NO-LOCK NO-ERROR.
        IF AVAIL tip_metricas_plr THEN DO:
            IF CAN-FIND(FIRST tip_indiv_factor
                        WHERE tip_indiv_factor.cdn_indiv_factor = int(tip_indiv_factor.cdn_indiv_factor:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
                        AND tip_indiv_factor.cdn_empresa = tip_metricas_plr.cdn_empresa
                        AND tip_indiv_factor.cdn_estab = tip_metricas_plr.cdn_estab
                        AND tip_indiv_factor.LOG_corporativo = tip_metricas_plr.LOG_corporativo
                        AND tip_indiv_factor.dt_inicio = date(tip_indiv_factor.dt_inicio:SCREEN-VALUE IN FRAME {&FRAME-NAME})) THEN DO:
                run utp/ut-msgs.p (input "show":U, input 1, INPUT  "Individual Factor").
                APPLY "ENTRY" TO tip_indiv_factor.cdn_indiv_factor IN FRAME {&FRAME-NAME}.
                RETURN "ADM-ERROR":U.
            END.

            IF CAN-FIND (FIRST tip_indiv_factor
                         WHERE tip_indiv_factor.cdn_indiv_factor = int(tip_indiv_factor.cdn_indiv_factor:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
                           AND tip_indiv_factor.cdn_empresa = tip_metricas_plr.cdn_empresa
                           AND tip_indiv_factor.cdn_estab = tip_metricas_plr.cdn_estab
                           AND tip_indiv_factor.LOG_corporativo = tip_metricas_plr.LOG_corporativo
                           AND tip_indiv_factor.dt_inicio >= date(tip_indiv_factor.dt_inicio:SCREEN-VALUE IN FRAME {&FRAME-NAME}) )  THEN DO:
                RUN utp/ut-msgs.p(INPUT "show":U, INPUT 17006, INPUT "Data de in¡cio inv lida" + "~~" 
                                  + "J  existe um registro Individual Factor com data de in¡cio posterior a " 
                                  + String(tip_indiv_factor.dt_inicio:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
                                  + ". Para adicionar registros retroativos ‚ preciso excluir os posteriores.").
                APPLY "ENTRY" TO tip_indiv_factor.dt_inicio IN FRAME {&FRAME-NAME}.
                RETURN "ADM-ERROR":U.
            END.
        END.
    END.

    IF tip_indiv_factor.dt_inicio:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO:
        run utp/ut-msgs.p (input "show":U, input 17006, INPUT "Data de in¡cio inv lida" + "~~" + "Favor informar uma data de in¡cio.").
        APPLY "ENTRY" TO tip_indiv_factor.dt_inicio IN FRAME {&FRAME-NAME}.
        RETURN "ADM-ERROR":U.
    END.

    IF day(date(tip_indiv_factor.dt_inicio:SCREEN-VALUE IN FRAME {&FRAME-NAME})) <> 1 THEN DO:
        run utp/ut-msgs.p (input "show":U, input 17006, INPUT "Data de in¡cio inv lida" + "~~" + "O dia tem que ser igual a 1.").
        APPLY "ENTRY" TO tip_indiv_factor.dt_inicio IN FRAME {&FRAME-NAME}.
        RETURN "ADM-ERROR":U.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "cod_usuario" "tip_indiv_factor" "cod_usuario"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "tip_indiv_factor"}

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

  IF p-state = "UPDATE-BEGIN" THEN DO:
      ASSIGN tip_indiv_factor.cdn_indiv_factor:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tip_indiv_factor.dt_inicio:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             l-update = YES.
  END.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


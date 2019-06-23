&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          dthresp          PROGRESS
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
{include/buffers_RH.i}

{include/i-prgvrs.i YMVWR002 1.00.00.000}

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
def var v-row-parent    as rowid                     no-undo.
DEF VAR v_cod_grp_usuar LIKE grp_usuar.cod_grp_usuar NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES grp_usuar_esp
&Scoped-define FIRST-EXTERNAL-TABLE grp_usuar_esp


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR grp_usuar_esp.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS grp_usuar_esp.cdn_empresa 
&Scoped-define ENABLED-TABLES grp_usuar_esp
&Scoped-define FIRST-ENABLED-TABLE grp_usuar_esp
&Scoped-Define ENABLED-OBJECTS rt-key 
&Scoped-Define DISPLAYED-FIELDS grp_usuar_esp.cdn_empresa ~
grp_usuar_esp.cod_grp_usuar 
&Scoped-define DISPLAYED-TABLES grp_usuar_esp
&Scoped-define FIRST-DISPLAYED-TABLE grp_usuar_esp
&Scoped-Define DISPLAYED-OBJECTS v_des_empresa v_des_grp_usuar 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS grp_usuar_esp.cod_grp_usuar 

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
DEFINE VARIABLE v_des_empresa AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 51.57 BY .88 NO-UNDO.

DEFINE VARIABLE v_des_grp_usuar AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 51.57 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 2.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     grp_usuar_esp.cdn_empresa AT ROW 1.33 COL 21 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     v_des_empresa AT ROW 1.33 COL 28.43 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     grp_usuar_esp.cod_grp_usuar AT ROW 2.33 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     v_des_grp_usuar AT ROW 2.33 COL 28.43 COLON-ALIGNED NO-LABEL
     rt-key AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: grp_usuar_esp
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
         HEIGHT             = 2.58
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

/* SETTINGS FOR FILL-IN grp_usuar_esp.cod_grp_usuar IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN v_des_empresa IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v_des_grp_usuar IN FRAME f-main
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

&Scoped-define SELF-NAME grp_usuar_esp.cdn_empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL grp_usuar_esp.cdn_empresa V-table-Win
ON F5 OF grp_usuar_esp.cdn_empresa IN FRAME f-main /* Empresa */
DO:
    {include/zoomvar.i &prog-zoom="object\sopy\zoom\z02py197.w"
                     &campo=grp_usuar_esp.cdn_empresa
                     &campo2=v_des_empresa
                     &campozoom=ep-codigo
                     &campozoom2=razao-social}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL grp_usuar_esp.cdn_empresa V-table-Win
ON LEAVE OF grp_usuar_esp.cdn_empresa IN FRAME f-main /* Empresa */
DO:
    {include/leave.i &tabela=empresa
                   &atributo-ref=razao-social
                   &variavel-ref=v_des_empresa
                   &where="empresa.ep-codigo = input frame {&frame-name} grp_usuar_esp.cdn_empresa"}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL grp_usuar_esp.cdn_empresa V-table-Win
ON MOUSE-SELECT-DBLCLICK OF grp_usuar_esp.cdn_empresa IN FRAME f-main /* Empresa */
DO:
    apply 'F5' to self.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME grp_usuar_esp.cod_grp_usuar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL grp_usuar_esp.cod_grp_usuar V-table-Win
ON F5 OF grp_usuar_esp.cod_grp_usuar IN FRAME f-main /* Grupo Usu rios */
DO:
  assign l-implanta = yes.
  {include/zoomvar.i &prog-zoom="object/sopy/zoom/z01py261.w"
                     &campo=grp_usuar_esp.cod_grp_usuar
                     &campo2=v_des_grp_usuar
                     &campozoom=cod_grp_usuar
                     &campozoom2=des_grp_usuar}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL grp_usuar_esp.cod_grp_usuar V-table-Win
ON LEAVE OF grp_usuar_esp.cod_grp_usuar IN FRAME f-main /* Grupo Usu rios */
DO:
  {include/leave.i &tabela=grp_usuar
                   &atributo-ref=des_grp_usuar
                   &variavel-ref=v_des_grp_usuar
                   &where="grp_usuar.cod_grp_usuar = input frame {&frame-name} grp_usuar_esp.cod_grp_usuar"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL grp_usuar_esp.cod_grp_usuar V-table-Win
ON MOUSE-SELECT-DBLCLICK OF grp_usuar_esp.cod_grp_usuar IN FRAME f-main /* Grupo Usu rios */
DO:
  apply 'F5' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF 
          
  grp_usuar_esp.cdn_empresa:load-mouse-pointer ("image/lupa.cur") in frame {&frame-name}.
  grp_usuar_esp.cod_grp_usuar:load-mouse-pointer ("image/lupa.cur") in frame {&frame-name}.
  
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
  {src/adm/template/row-list.i "grp_usuar_esp"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "grp_usuar_esp"}

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
/*     {include/i-valid.i} */
    if not frame {&frame-name}:validate() then
                return 'ADM-ERROR':U.

    /* Verificar se ao incluir um novo registro, o mesmo j  existe. Caso exista, deverÿ mostrar a seguinte mensagem 
    1 - J  existe ocorrˆncia & informada) */
    if adm-new-record then do:
        find first grp_usuar_esp NO-LOCK WHERE
                   grp_usuar_esp.cdn_empresa   = input frame {&frame-name} grp_usuar_esp.cdn_empresa   AND
                   grp_usuar_esp.cod_grp_usuar = input frame {&frame-name} grp_usuar_esp.cod_grp_usuar no-error.
        If avail grp_usuar_esp then do:
            {utp/ut-table.i mguni grp_usuar 1}
            run utp/ut-msgs.p (input "show":U,
                               input 1, 
                               input return-value).
            apply 'entry' to grp_usuar_esp.cod_grp_usuar in frame {&frame-name}.
            return 'adm-error':U. 
        END.
    end.

    /* Caso nÆo seja informado nenhuma empresa, emitir a seguinte mensagem (806 - Empresa nÆo cadastrada) */
    find empresa no-lock where
         empresa.ep-codigo = input frame {&frame-name} grp_usuar_esp.cdn_empresa no-error.
    if not avail empresa then do:
      run utp/ut-msgs.p (input "show":U, input 806, input "").
      apply 'entry' to grp_usuar_esp.cdn_empresa in frame {&frame-name}.
      return 'ADM-ERROR':U.
    end.     

    /* Caso nÆo seja informado nenhum grupo de usu rios, emitir a seguinte mensagem (17897 - Grupo de Usu rios deve ser informado) */
    IF input frame {&frame-name} grp_usuar_esp.cod_grp_usuar = "" then do:
        run utp/ut-msgs.p (input "show":U,
                           input 17897, 
                           input return-value).
        return 'ADM-ERROR':U.
    END.

    /* Somente deverÿ salvar na tabela os grupos de usu rios que existirem na tabela grp_usuar (19368 - Grupo de Usu rios & Inexistente). */
    IF not can-find(first grp_usuar WHERE
                          grp_usuar.cod_grp_usuar = input frame {&frame-name} grp_usuar_esp.cod_grp_usuar) then do:
        run utp/ut-msgs.p (input "show":U,
                           input 19368, 
                           input input frame {&frame-name} grp_usuar_esp.cod_grp_usuar).
        return 'ADM-ERROR':U.
    END.

    /*ASSIGN v_cod_grp_usuar = INPUT FRAME {&FRAME-NAME} grp_usuar_esp.cod_grp_usuar.*/

    /*:T Ponha na pi-validate todas as valida‡äes */
    /*:T NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.

    /*:T Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

    /*ASSIGN grp_usuar_esp.cod_grp_usuar = v_cod_grp_usuar.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  FIND CURRENT grp_usuar_esp NO-LOCK NO-ERROR.

  FOR EACH sit_afast_nao_autoriz EXCLUSIVE-LOCK WHERE
           sit_afast_nao_autoriz.cdn_empresa   = grp_usuar_esp.cdn_empresa AND
           sit_afast_nao_autoriz.cod_grp_usuar = grp_usuar_esp.cod_grp_usuar:

      DELETE sit_afast_nao_autoriz.
  END.

  FOR EACH ocor_nao_autoriz EXCLUSIVE-LOCK WHERE
           ocor_nao_autoriz.cdn_empresa   = grp_usuar_esp.cdn_empresa AND
           ocor_nao_autoriz.cod_grp_usuar = grp_usuar_esp.cod_grp_usuar:

      DELETE ocor_nao_autoriz.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

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

  IF AVAIL grp_usuar_esp THEN DO:
    APPLY "leave":U TO grp_usuar_esp.cdn_empresa IN FRAME {&FRAME-NAME}.
    APPLY "leave":U TO grp_usuar_esp.cod_grp_usuar IN FRAME {&FRAME-NAME}.
  END.

  /*if available grp_usuar_esp then do:
      FIND FIRST grp_usuar no-lock where
                 grp_usuar.cod_grp_usuar = grp_usuar_esp.cod_grp_usuar no-error.
      IF AVAIL grp_usuar THEN
          assign grp_usuar_esp.cod_grp_usuar:SCREEN-VALUE IN FRAME {&FRAME-NAME} = grp_usuar.cod_grp_usuar
                 v_des_grp_usuar:SCREEN-VALUE IN FRAME {&FRAME-NAME}             = grp_usuar.des_grp_usuar.
  end.
  ELSE 
      ASSIGN grp_usuar_esp.cod_grp_usuar:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = ""
             v_des_grp_usuar:SCREEN-VALUE IN FRAME {&FRAME-NAME}                 = "".*/
  

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
/*     if adm-new-record = yes then */
        enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
    IF NOT adm-new-record THEN
        ASSIGN grp_usuar_esp.cdn_empresa:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

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
  {src/adm/template/snd-list.i "grp_usuar_esp"}

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


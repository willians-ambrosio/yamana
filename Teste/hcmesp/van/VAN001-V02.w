&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems5esp          PROGRESS
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
/****************************************************************************************** 
**             Programa: VAN001-V01
**            Autor: Felipe Vieira
**       Fornecedor: DKP
**             Data: 30/08/2018
**   Change/Chamado: XXXXXXX
**         Objetivo: Campos da tela de Cadastro simples da tabela es_param_ws_van (Parametro webserver)
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** 
** Data         Autor                   Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
** 
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
*****************************************************************************************/
{include/i-prgvrs.i VAN001-V01 1.00.00.000}
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

DEFINE VARIABLE c-dir-erro    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-dir-enviado AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-dir-rec     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-dir-pend    AS CHARACTER   NO-UNDO.

DEFINE VARIABLE h-van001-b05 AS HANDLE      NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES es_param_ws_van
&Scoped-define FIRST-EXTERNAL-TABLE es_param_ws_van


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es_param_ws_van.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es_param_ws_van.cdn_empresa ~
es_param_ws_van.tipo es_param_ws_van.usu es_param_ws_van.log_ativo ~
es_param_ws_van.pwd es_param_ws_van.url 
&Scoped-define ENABLED-TABLES es_param_ws_van
&Scoped-define FIRST-ENABLED-TABLE es_param_ws_van
&Scoped-Define ENABLED-OBJECTS rt-mold RECT-3 RECT-6 
&Scoped-Define DISPLAYED-FIELDS es_param_ws_van.cdn_empresa ~
es_param_ws_van.tipo es_param_ws_van.nomeserv es_param_ws_van.usu ~
es_param_ws_van.log_ativo es_param_ws_van.pwd es_param_ws_van.url 
&Scoped-define DISPLAYED-TABLES es_param_ws_van
&Scoped-define FIRST-DISPLAYED-TABLE es_param_ws_van


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS es_param_ws_van.cdn_empresa ~
es_param_ws_van.nomeserv 
&Scoped-define ADM-ASSIGN-FIELDS es_param_ws_van.cdn_empresa ~
es_param_ws_van.nomeserv es_param_ws_van.usu es_param_ws_van.pwd ~
es_param_ws_van.url 
&Scoped-define ADM-MODIFY-FIELDS es_param_ws_van.usu b-visualizar-senha ~
es_param_ws_van.pwd es_param_ws_van.url 

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
    'Keys-Accepted = "cdn_empresa",
     Keys-Supplied = "cdn_empresa"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON b-visualizar-senha 
     LABEL "Mostrar" 
     SIZE 6 BY .88
     FONT 1.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94.14 BY 3.5.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94.14 BY 4.38.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 8.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es_param_ws_van.cdn_empresa AT ROW 1.5 COL 28 COLON-ALIGNED WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     es_param_ws_van.tipo AT ROW 2.42 COL 30 NO-LABEL WIDGET-ID 40
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Envia Arquivos", yes,
"Recebe Arquivos", no
          SIZE 36 BY 1
     es_param_ws_van.nomeserv AT ROW 3.54 COL 28 COLON-ALIGNED WIDGET-ID 8
          LABEL "Serviáo"
          VIEW-AS FILL-IN 
          SIZE 32 BY .88
     es_param_ws_van.usu AT ROW 5.25 COL 13.86 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 21 BY .88
     es_param_ws_van.log_ativo AT ROW 5.25 COL 65 WIDGET-ID 38
          VIEW-AS TOGGLE-BOX
          SIZE 8 BY 1
     b-visualizar-senha AT ROW 6.25 COL 67 WIDGET-ID 34
     es_param_ws_van.pwd AT ROW 6.29 COL 13.86 COLON-ALIGNED WIDGET-ID 20 PASSWORD-FIELD 
          VIEW-AS FILL-IN 
          SIZE 51 BY .88
     es_param_ws_van.url AT ROW 7.29 COL 15.72 NO-LABEL WIDGET-ID 16
          VIEW-AS EDITOR MAX-CHARS 300 SCROLLBAR-VERTICAL
          SIZE 78.29 BY 1.83
     "Tipo:" VIEW-AS TEXT
          SIZE 5 BY .67 AT ROW 2.58 COL 24.57 WIDGET-ID 44
     "URL:" VIEW-AS TEXT
          SIZE 4.72 BY .88 AT ROW 7.29 COL 10.72 WIDGET-ID 18
     rt-mold AT ROW 1 COL 1
     RECT-3 AT ROW 1.25 COL 2 WIDGET-ID 12
     RECT-6 AT ROW 5 COL 1.86 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ems5esp.es_param_ws_van
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
         WIDTH              = 96.86.
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

/* SETTINGS FOR BUTTON b-visualizar-senha IN FRAME f-main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN es_param_ws_van.cdn_empresa IN FRAME f-main
   1 2                                                                  */
/* SETTINGS FOR FILL-IN es_param_ws_van.nomeserv IN FRAME f-main
   NO-ENABLE 1 2 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN es_param_ws_van.pwd IN FRAME f-main
   2 3                                                                  */
/* SETTINGS FOR EDITOR es_param_ws_van.url IN FRAME f-main
   2 3                                                                  */
/* SETTINGS FOR FILL-IN es_param_ws_van.usu IN FRAME f-main
   2 3                                                                  */
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

&Scoped-define SELF-NAME b-visualizar-senha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-visualizar-senha V-table-Win
ON CHOOSE OF b-visualizar-senha IN FRAME f-main /* Mostrar */
DO:
   es_param_ws_van.pwd:PASSWORD-FIELD = NOT es_param_ws_van.pwd:PASSWORD-FIELD. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es_param_ws_van.cdn_empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_param_ws_van.cdn_empresa V-table-Win
ON F5 OF es_param_ws_van.cdn_empresa IN FRAME f-main /* Empresa */
DO:
   {include/zoomvar.i &prog-zoom="object/sopy/zoom/z01py197.w"
                     &campo=es_param_ws_van.cdn_empresa
                     &campozoom=cdn_empresa}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_param_ws_van.cdn_empresa V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es_param_ws_van.cdn_empresa IN FRAME f-main /* Empresa */
DO:
  APPLY "f5" TO SELF.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  {src/adm/template/row-list.i "es_param_ws_van"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es_param_ws_van"}

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
    
    /*:T Ponha na pi-validate todas as validaá‰es */
    /*:T N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */

    RUN PI-validate.
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN es_param_ws_van.token = STRING(HEX-ENCODE(MD5-DIGEST(es_param_ws_van.pwd))).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF CAN-FIND(FIRST es_param_dir_van
                WHERE es_param_dir_van.cdn_empresa   = es_param_ws.cdn_empresa
                  AND es_param_dir_van.tipo          = es_param_ws.tipo
                  AND es_param_dir_van.nomeserv      = es_param_ws_van.nomeserv) THEN DO:

       RUN utp/ut-msgs.p (INPUT "show",
                          INPUT 17006,
                          INPUT "Registro possui relacionamento!~~Favor verificar os relacionamentos.").
       RETURN "NOK".     

    END.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .


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
    
     run get-link-handle in adm-broker-hdl (THIS-PROCEDURE , "hab-target":u, output h-van001-b05). 
     RUN Bloquear IN h-van001-b05 (INPUT 2).
    

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
        enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    if adm-new-record = yes THEN DO:
        enable {&ADM-CREATE-FIELDS} with frame {&frame-name}.
        es_param_ws_van.cdn_empresa:LOAD-MOUSE-POINTER("image/lupa.cur").
        run get-link-handle in adm-broker-hdl (THIS-PROCEDURE , "hab-target":u, output h-van001-b05). 
        RUN Bloquear IN h-van001-b05 (INPUT 1).
    END.
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
    {include/i-vldfrm.i} /*:T Validaá∆o de dicion†rio */
    
/*:T    Segue um exemplo de validaá∆o de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */

 IF   (INPUT FRAME {&FRAME-NAME} es_param_ws_van.nomeserv         = "" 
  OR  INPUT FRAME {&FRAME-NAME} es_param_ws_van.pwd              = "" 
  OR  INPUT FRAME {&FRAME-NAME} es_param_ws_van.usu              = "" 
  OR  INPUT FRAME {&FRAME-NAME} es_param_ws_van.URL              = "")
 THEN DO:     
     /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
      {include/i-vldprg.i}
      run utp/ut-msgs.p (input "show":U, input 17006, "Todos os campos devem ser preenchidos!").
      return 'ADM-ERROR':U.
 END.
 
 IF adm-new-record THEN DO:

     IF NOT CAN-FIND(FIRST param_empres_rh  WHERE
                           param_empres_rh.cdn_empresa = INPUT FRAME {&FRAME-NAME} es_param_ws_van.cdn_empresa) THEN DO:

         {include/i-vldprg.i}
          run utp/ut-msgs.p (input "show":U, input 17006, "Empresa n∆o cadastrada!").
          return 'ADM-ERROR':U.
     END.

     IF CAN-FIND(es_param_ws_van WHERE
                      es_param_ws_van.cdn_empresa   = INPUT FRAME {&FRAME-NAME} es_param_ws_van.cdn_empresa AND 
                      es_param_ws_van.tipo          = INPUT FRAME {&FRAME-NAME} es_param_ws_van.tipo AND
                      es_param_ws_van.nomeserv      = INPUT FRAME {&FRAME-NAME} es_param_ws_van.nomeserv)
             
     THEN DO:
          /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
          {include/i-vldprg.i}
          run utp/ut-msgs.p (input "show":U, input 17006, "Registro j† existe!~~J† existe um registro!").
          return 'ADM-ERROR':U.
     END.
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

  /* There are no foreign keys supplied by this SmartObject. */

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
  {src/adm/template/snd-list.i "es_param_ws_van"}

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


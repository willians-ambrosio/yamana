&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgesp            PROGRESS
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
{include/i-prgvrs.i V99XX999 9.99.99.999}

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
&Scoped-define EXTERNAL-TABLES es-beneficio-ncm
&Scoped-define FIRST-EXTERNAL-TABLE es-beneficio-ncm


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es-beneficio-ncm.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es-beneficio-ncm.cod-beneficio ~
es-beneficio-ncm.class-fisc-ini es-beneficio-ncm.class-fisc-fim 
&Scoped-define ENABLED-TABLES es-beneficio-ncm
&Scoped-define FIRST-ENABLED-TABLE es-beneficio-ncm
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold FILL-IN-7 f-ncm-ini f-ncm-fim 
&Scoped-Define DISPLAYED-FIELDS es-beneficio-ncm.cod-beneficio ~
es-beneficio-ncm.class-fisc-ini es-beneficio-ncm.class-fisc-fim 
&Scoped-define DISPLAYED-TABLES es-beneficio-ncm
&Scoped-define FIRST-DISPLAYED-TABLE es-beneficio-ncm
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-7 f-ncm-ini f-ncm-fim 

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
cod-beneficio||y|es-beneficio-ncm.cod-beneficio
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "cod-beneficio"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE f-ncm-fim AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35.86 BY .88 NO-UNDO.

DEFINE VARIABLE f-ncm-ini AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35.86 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42.86 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 66 BY 1.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 66 BY 4.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es-beneficio-ncm.cod-beneficio AT ROW 1.17 COL 14.29 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .88
     FILL-IN-7 AT ROW 1.17 COL 20.14 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     es-beneficio-ncm.class-fisc-ini AT ROW 2.5 COL 14 COLON-ALIGNED WIDGET-ID 4
          LABEL "NCM Inicial"
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     f-ncm-ini AT ROW 2.5 COL 27.14 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     es-beneficio-ncm.class-fisc-fim AT ROW 3.75 COL 14 COLON-ALIGNED WIDGET-ID 2
          LABEL "NCM Final"
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     f-ncm-fim AT ROW 3.75 COL 27.14 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: es-beneficio-ncm
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
         HEIGHT             = 4.5
         WIDTH              = 67.72.
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

/* SETTINGS FOR FILL-IN es-beneficio-ncm.class-fisc-fim IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN es-beneficio-ncm.class-fisc-ini IN FRAME f-main
   EXP-LABEL                                                            */
ASSIGN 
       es-beneficio-ncm.cod-beneficio:READ-ONLY IN FRAME f-main        = TRUE.

ASSIGN 
       f-ncm-fim:READ-ONLY IN FRAME f-main        = TRUE.

ASSIGN 
       f-ncm-ini:READ-ONLY IN FRAME f-main        = TRUE.

ASSIGN 
       FILL-IN-7:READ-ONLY IN FRAME f-main        = TRUE.

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

&Scoped-define SELF-NAME es-beneficio-ncm.class-fisc-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-beneficio-ncm.class-fisc-fim V-table-Win
ON LEAVE OF es-beneficio-ncm.class-fisc-fim IN FRAME f-main /* NCM Final */
DO:
  FIND classif-fisc  WHERE classif-fisc.class-fiscal = INPUT FRAME f-main  es-beneficio-ncm.class-fisc-fim  NO-LOCK NO-ERROR.
   IF AVAIL classif-fisc  THEN f-ncm-fim:SCREEN-VALUE = classif-fisc.descricao.
   ELSE DO:
      MESSAGE "Classificaá∆o n∆o cadastrada!!!"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

      RETURN NO-APPLY.


   END.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-beneficio-ncm.class-fisc-fim V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-beneficio-ncm.class-fisc-fim IN FRAME f-main /* NCM Final */
DO:
   {include/zoomvar.i &prog-zoom=inzoom\z01in046.r
                        &campo=es-beneficio-ncm.class-fisc-fim
                        &campozoom=class-fiscal 
                        &campo2=f-ncm-fim
                        &campozoom2=descricao 
                        }
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-beneficio-ncm.class-fisc-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-beneficio-ncm.class-fisc-ini V-table-Win
ON LEAVE OF es-beneficio-ncm.class-fisc-ini IN FRAME f-main /* NCM Inicial */
DO:
  FIND classif-fisc  WHERE classif-fisc.class-fiscal = INPUT FRAME f-main  es-beneficio-ncm.class-fisc-ini  NO-LOCK NO-ERROR.
   IF AVAIL classif-fisc  THEN f-ncm-ini:SCREEN-VALUE = classif-fisc.descricao.
   ELSE DO:
      MESSAGE "Classificaá∆o n∆o cadastrada!!!"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

      RETURN NO-APPLY.


   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-beneficio-ncm.class-fisc-ini V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-beneficio-ncm.class-fisc-ini IN FRAME f-main /* NCM Inicial */
DO:
  {include/zoomvar.i &prog-zoom=inzoom\z01in046.r
                        &campo=es-beneficio-ncm.class-fisc-ini
                        &campozoom=class-fiscal 
                        &campo2=f-ncm-ini
                        &campozoom2=descricao 
                        }

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
   es-beneficio-ncm.class-fisc-ini:LOAD-MOUSE-POINTER('image/lupa.cur').
   es-beneficio-ncm.class-fisc-fim:LOAD-MOUSE-POINTER('image/lupa.cur').

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
  {src/adm/template/row-list.i "es-beneficio-ncm"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es-beneficio-ncm"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/




 RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .


find es-beneficio  where rowid(es-beneficio) = v-row-parent no-lock no-error.                                        
   if available es-beneficio  then do:                                                                                
       assign  es-beneficio-ncm.cod-beneficio:SCREEN-VALUE IN FRAME f-main = STRING(es-beneficio.cod-beneficio).       
               FILL-IN-7:SCREEN-VALUE = es-beneficio.desc-beneficio.                                                 
   end.     


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
   FIND classif-fisc  WHERE classif-fisc.class-fiscal = INPUT FRAME f-main  es-beneficio-ncm.class-fisc-ini  NO-LOCK NO-ERROR.
   IF NOT AVAIL classif-fisc  THEN  
   DO:
       run utp/ut-msgs.p (input "show",
                        input 17006,
                        input "Classificaá∆o n∆o cadastrada!!!" ).
       return 'ADM-ERROR':U.
   END.
   FIND classif-fisc  WHERE classif-fisc.class-fiscal = INPUT FRAME f-main  es-beneficio-ncm.class-fisc-fim  NO-LOCK NO-ERROR.
   IF NOT AVAIL classif-fisc  THEN  
   DO:
        run utp/ut-msgs.p (input "show",                                  
                         input 17006,                                     
                         input "Classificaá∆o n∆o cadastrada!!!" ).

    
    return 'ADM-ERROR':U.
   END.


   IF es-beneficio-ncm.class-fisc-fim:SCREEN-VALUE < es-beneficio-ncm.class-fisc-ini:SCREEN-VALUE THEN DO:
           run utp/ut-msgs.p (input "show",                                 
                            input 17006,                                    
                            input "NCM FINAL MENOR QUE O INICIAL!!!" ). 
       return 'ADM-ERROR':U.
   END.

   FIND FIRST es-beneficio-cfa WHERE es-beneficio-cfa.cod-beneficio = INPUT FRAME f-main es-beneficio-ncm.cod-beneficio NO-LOCK NO-ERROR.
   IF AVAIL es-beneficio-cfa THEN DO:
    
       run utp/ut-msgs.p (input "show",                                 
                            input 17006,                                    
                            input "BENEFICIO JA CADASTRADO POR CFA" ). 
       return 'ADM-ERROR':U.
     
END.

   






    /* Code placed here will execute PRIOR to standard behavior. */
    {include/i-valid.i}
    
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
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

find es-beneficio  where rowid(es-beneficio) = v-row-parent no-lock no-error.                                        
   if available es-beneficio  then do:                                                                                
       assign  es-beneficio-ncm.cod-beneficio:SCREEN-VALUE IN FRAME f-main = STRING(es-beneficio.cod-beneficio).       
              FILL-IN-7:SCREEN-VALUE = es-beneficio.desc-beneficio.                                                 
   end.

   FIND classif-fisc  WHERE classif-fisc.class-fiscal = INPUT FRAME f-main  es-beneficio-ncm.class-fisc-ini  NO-LOCK NO-ERROR.
   IF AVAIL classif-fisc  THEN f-ncm-ini:SCREEN-VALUE = classif-fisc.descricao.

   FIND classif-fisc  WHERE classif-fisc.class-fiscal = INPUT FRAME f-main  es-beneficio-ncm.class-fisc-fim  NO-LOCK NO-ERROR.
   IF AVAIL classif-fisc  THEN f-ncm-fim:SCREEN-VALUE = classif-fisc.descricao.






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
    {include/i-vldfrm.i} /*:T Validaá∆o de dicion†rio */
    
/*:T    Segue um exemplo de validaá∆o de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */
      
      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */

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
  {src/adm/template/sndkycas.i "cod-beneficio" "es-beneficio-ncm" "cod-beneficio"}

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
  {src/adm/template/snd-list.i "es-beneficio-ncm"}

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


&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems5_esp         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{esp/esapb009-bf.i}
{include/i-prgvrs.i V99XX999 9.99.99.999}

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

DEFINE NEW GLOBAL SHARED VAR v_rec_fornecedor AS RECID NO-UNDO.

/* Begins: REV001  */
DEFINE VARIABLE cCodCodeAtual    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCodCodeAnterior AS CHARACTER   NO-UNDO.
/* end: REV001 */

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
&Scoped-define EXTERNAL-TABLES es-cat-code-fornec
&Scoped-define FIRST-EXTERNAL-TABLE es-cat-code-fornec


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es-cat-code-fornec.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es-cat-code-fornec.cat-code ~
es-cat-code-fornec.cons-relatorio 
&Scoped-define ENABLED-TABLES es-cat-code-fornec
&Scoped-define FIRST-ENABLED-TABLE es-cat-code-fornec
&Scoped-Define ENABLED-OBJECTS rt-mold rt-mold-2 
&Scoped-Define DISPLAYED-FIELDS es-cat-code-fornec.cdn_fornecedor ~
es-cat-code-fornec.cat-code es-cat-code-fornec.cons-relatorio 
&Scoped-define DISPLAYED-TABLES es-cat-code-fornec
&Scoped-define FIRST-DISPLAYED-TABLE es-cat-code-fornec
&Scoped-Define DISPLAYED-OBJECTS c-desc-forn c-desc-cat-code 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS es-cat-code-fornec.cdn_fornecedor 
&Scoped-define ADM-ASSIGN-FIELDS es-cat-code-fornec.cat-code 
&Scoped-define ADM-MODIFY-FIELDS es-cat-code-fornec.cat-code 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
cat-code||y|ESTMA.es-cat-code-fornec.cat-code
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "cat-code"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE c-desc-cat-code AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47.86 BY .88 NO-UNDO.

DEFINE VARIABLE c-desc-forn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 51 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 2.

DEFINE RECTANGLE rt-mold-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 2.63.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es-cat-code-fornec.cdn_fornecedor AT ROW 1.79 COL 20.86 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 13.72 BY .88
     c-desc-forn AT ROW 1.79 COL 35 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     es-cat-code-fornec.cat-code AT ROW 3.46 COL 20.86 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 17.14 BY .88
     c-desc-cat-code AT ROW 3.46 COL 38.43 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     es-cat-code-fornec.cons-relatorio AT ROW 4.58 COL 22.72 WIDGET-ID 16
          VIEW-AS TOGGLE-BOX
          SIZE 22.72 BY .83
     rt-mold AT ROW 1 COL 1
     rt-mold-2 AT ROW 3.04 COL 1 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ESTMA.es-cat-code-fornec
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
         HEIGHT             = 4.96
         WIDTH              = 88.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}

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

/* SETTINGS FOR FILL-IN c-desc-cat-code IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-desc-forn IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN es-cat-code-fornec.cat-code IN FRAME f-main
   2 3                                                                  */
/* SETTINGS FOR FILL-IN es-cat-code-fornec.cdn_fornecedor IN FRAME f-main
   NO-ENABLE 1                                                          */
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

&Scoped-define SELF-NAME es-cat-code-fornec.cat-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-cat-code-fornec.cat-code V-table-Win
ON F5 OF es-cat-code-fornec.cat-code IN FRAME f-main /* CAT CODE */
DO:
  {include/zoomvar.i &prog-zoom = esp/esapb006-z01.w
                     &campo     = es-cat-code-fornec.cat-code
                     &campozoom = es-cat-code.cat-code}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-cat-code-fornec.cat-code V-table-Win
ON LEAVE OF es-cat-code-fornec.cat-code IN FRAME f-main /* CAT CODE */
DO:
  RUN pi-leave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-cat-code-fornec.cat-code V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-cat-code-fornec.cat-code IN FRAME f-main /* CAT CODE */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-cat-code-fornec.cdn_fornecedor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-cat-code-fornec.cdn_fornecedor V-table-Win
ON F5 OF es-cat-code-fornec.cdn_fornecedor IN FRAME f-main /* Fornecedor */
DO:
  

    RUN prgint/utb/utb031nb.r.

  

    FIND FIRST fornecedor NO-LOCK
        WHERE RECID(fornecedor) = v_rec_fornecedor NO-ERROR.
    IF AVAIL fornecedor THEN DO:
        ASSIGN es-cat-code-fornec.cdn_fornecedor:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(fornecedor.cdn_fornecedor).
    END.

    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-cat-code-fornec.cdn_fornecedor V-table-Win
ON LEAVE OF es-cat-code-fornec.cdn_fornecedor IN FRAME f-main /* Fornecedor */
DO:
  RUN pi-leave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-cat-code-fornec.cdn_fornecedor V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-cat-code-fornec.cdn_fornecedor IN FRAME f-main /* Fornecedor */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  es-cat-code-fornec.cdn_fornecedor:load-mouse-pointer ("image\lupa.cur") in frame {&frame-name}.  
  es-cat-code-fornec.cat-code      :load-mouse-pointer ("image\lupa.cur") in frame {&frame-name}.  

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
  {src/adm/template/row-list.i "es-cat-code-fornec"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es-cat-code-fornec"}

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
    
    /* Ponha na pi-validate todas as valida��es */
    /* N�o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    RUN pi-validate.
    
    if RETURN-VALUE <> 'ADM-ERROR':U then  DO:
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    END.
    /* Todos os assign�s n�o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */   


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
    FIND FIRST es-cat-code-fornec WHERE
               es-cat-code-fornec.cdn_fornecedor = INPUT FRAME {&FRAME-NAME} es-cat-code-fornec.cdn_fornecedor  AND
               es-cat-code-fornec.cat-code       = INPUT FRAME {&FRAME-NAME} es-cat-code-fornec.cat-code        NO-LOCK NO-ERROR. 
    IF AVAIL es-cat-code-fornec THEN 
    DO:
       RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, 
                          INPUT "Cat Code ja Cadastrado." + "~~" +
                                "Registro j� existente para a chave fornecedor x cat code.").
          APPLY "ENTRY":U TO es-cat-code-fornec.cat-code IN FRAME {&FRAME-NAME}. 
          RETURN "ADM-ERROR":U.
    END.

    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .


RUN pi-leave.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-leave V-table-Win 
PROCEDURE pi-leave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST fornecedor NO-LOCK
    WHERE fornecedor.cdn_fornecedor = INPUT FRAME {&FRAME-NAME} es-cat-code-fornec.cdn_fornecedor NO-ERROR.
IF AVAIL fornecedor THEN
    ASSIGN c-desc-forn:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fornecedor.nom_pessoa . 
ELSE
    ASSIGN c-desc-forn:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "". 

FIND FIRST es-cat-code NO-LOCK
    WHERE es-cat-code.cat-code = INPUT FRAME {&FRAME-NAME} es-cat-code-fornec.cat-code NO-ERROR.
IF AVAIL es-cat-code THEN
   ASSIGN c-desc-cat-code:SCREEN-VALUE IN FRAME {&FRAME-NAME} = es-cat-code.desc-cat-code . 
ELSE
   ASSIGN c-desc-cat-code:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "". 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: N�o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida��es, pois neste ponto do programa o registro 
  ainda n�o foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /* Valida��o de dicion�rio */
    
/*/*    Segue um exemplo de valida��o de programa */
 *     find tabela where tabela.campo1 = c-variavel and
 *                       tabela.campo2 > i-variavel no-lock no-error.
 *     
 *     /* Este include deve ser colocado sempre antes do ut-msgs.p */
 *     {include/i-vldprg.i}
 *     run utp/ut-msgs.p (input "show":U, input 7, input return-value).
 *     return 'ADM-ERROR':U.*/
             
     FIND FIRST fornecedor WHERE
                fornecedor.cdn_fornecedor       = INPUT FRAME {&FRAME-NAME} es-cat-code-fornec.cdn_fornecedor  NO-LOCK NO-ERROR. 
     IF NOT AVAIL fornecedor THEN 
     DO:
        RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, 
                           INPUT "Cat Code nao Cadastrado." + "~~" +
                                 "Verificar Cadastro de Cat Code ESAPB006.").
        APPLY "ENTRY":U TO es-cat-code-fornec.cdn_fornecedor IN FRAME {&FRAME-NAME}. 
        RETURN "ADM-ERROR":U.
     END.

     FIND FIRST es-cat-code WHERE
                es-cat-code.cat-code       = INPUT FRAME {&FRAME-NAME} es-cat-code-fornec.cat-code  NO-LOCK NO-ERROR. 
     IF NOT AVAIL es-cat-code THEN 
     DO:
        RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, 
                           INPUT "Cat Code nao Cadastrado." + "~~" +
                                 "Verificar Cadastro de Cat Code ESAPB006.").
        APPLY "ENTRY":U TO es-cat-code-fornec.cat-code IN FRAME {&FRAME-NAME}. 
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
  {src/adm/template/sndkycas.i "cat-code" "es-cat-code-fornec" "cat-code"}

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
  {src/adm/template/snd-list.i "es-cat-code-fornec"}

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


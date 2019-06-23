&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems5_esp         PROGRESS
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
&Scoped-define EXTERNAL-TABLES es-cfa-emp
&Scoped-define FIRST-EXTERNAL-TABLE es-cfa-emp


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es-cfa-emp.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es-cfa-emp.classe es-cfa-emp.ep-codigo ~
es-cfa-emp.cod-estabel es-cfa-emp.credita-icms es-cfa-emp.credita-piscofins 
&Scoped-define ENABLED-TABLES es-cfa-emp
&Scoped-define FIRST-ENABLED-TABLE es-cfa-emp
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS es-cfa-emp.classe es-cfa-emp.ep-codigo ~
es-cfa-emp.cod-estabel es-cfa-emp.credita-icms es-cfa-emp.credita-piscofins 
&Scoped-define DISPLAYED-TABLES es-cfa-emp
&Scoped-define FIRST-DISPLAYED-TABLE es-cfa-emp
&Scoped-Define DISPLAYED-OBJECTS f-desc-cfa FILL-IN-1 f-desc-estab 

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
classe||y|es-cfa-emp.classe
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "classe"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE f-desc-cfa AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY .88 NO-UNDO.

DEFINE VARIABLE f-desc-estab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42.14 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72 BY 1.5.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72 BY 5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es-cfa-emp.classe AT ROW 1.25 COL 13 COLON-ALIGNED WIDGET-ID 2
          LABEL "Classe"
          VIEW-AS FILL-IN NATIVE 
          SIZE 8 BY .88
     f-desc-cfa AT ROW 1.25 COL 21 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     es-cfa-emp.ep-codigo AT ROW 2.67 COL 13 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN NATIVE 
          SIZE 7.14 BY .88
     FILL-IN-1 AT ROW 2.67 COL 20.86 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     es-cfa-emp.cod-estabel AT ROW 3.75 COL 13 COLON-ALIGNED WIDGET-ID 32
          LABEL "Estabelec"
          VIEW-AS FILL-IN NATIVE 
          SIZE 7.14 BY .88
     f-desc-estab AT ROW 3.75 COL 21 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     es-cfa-emp.credita-icms AT ROW 5 COL 25 NO-LABEL WIDGET-ID 12
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Sim", yes,
"N∆o", no
          SIZE 18 BY 1
     es-cfa-emp.credita-piscofins AT ROW 6.13 COL 25.14 NO-LABEL WIDGET-ID 18
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Sim", yes,
"N∆o", no
          SIZE 17.86 BY 1
     "Credita ICMS" VIEW-AS TEXT
          SIZE 12 BY .67 AT ROW 5.13 COL 12 WIDGET-ID 16
     "Creidta PIS/COFINS" VIEW-AS TEXT
          SIZE 18 BY .67 AT ROW 6.13 COL 6 WIDGET-ID 22
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 2.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: es-cfa-emp
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
         HEIGHT             = 6.67
         WIDTH              = 72.
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

/* SETTINGS FOR FILL-IN es-cfa-emp.classe IN FRAME f-main
   EXP-LABEL                                                            */
ASSIGN 
       es-cfa-emp.classe:READ-ONLY IN FRAME f-main        = TRUE.

/* SETTINGS FOR FILL-IN es-cfa-emp.cod-estabel IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN f-desc-cfa IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN f-desc-estab IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME f-main
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

&Scoped-define SELF-NAME es-cfa-emp.cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-cfa-emp.cod-estabel V-table-Win
ON LEAVE OF es-cfa-emp.cod-estabel IN FRAME f-main /* Estabelec */
DO:
   FIND estabelec WHERE estabelec.cod-estabel = es-cfa-emp.cod-estabel:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF AVAIL estabelec THEN DO:
       f-desc-estab:SCREEN-VALUE = estabelec.nome.
   END.
   ELSE DO:
       IF es-cfa-emp.cod-estabel:SCREEN-VALUE = "" THEN RETURN.
       MESSAGE "Estabelecimento n∆o cadastrado!!!"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
       RETURN NO-APPLY.
   END.



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-cfa-emp.cod-estabel V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-cfa-emp.cod-estabel IN FRAME f-main /* Estabelec */
DO:
    {include/zoomvar.i &prog-zoom=adzoom\z01ad107.r
                         &campo=es-cfa-emp.cod-estabel
                         &campozoom=cod-estabel
                          }

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-cfa-emp.ep-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-cfa-emp.ep-codigo V-table-Win
ON LEAVE OF es-cfa-emp.ep-codigo IN FRAME f-main /* Empresa */
DO:
  
    FIND ems2cadme.empresa WHERE ems2cadme.empresa.ep-codigo = es-cfa-emp.ep-codigo:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAIL ems2cadme.empresa THEN DO:
        ASSIGN FILL-IN-1:SCREEN-VALUE = ems2cadme.empresa.razao-social.
    END.
    ELSE DO:

     IF es-cfa-emp.ep-codigo:SCREEN-VALUE = "" THEN RETURN.
     MESSAGE "Empresa n∆o cadastrada!!!"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN NO-APPLY.


    END.

    APPLY "leave" TO es-cfa-emp.ep-codigo IN FRAME f-main.



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-cfa-emp.ep-codigo V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-cfa-emp.ep-codigo IN FRAME f-main /* Empresa */
DO:
  
    {include/zoomvar.i &prog-zoom=unzoom\z01un004.r
                       &campo=es-cfa-emp.ep-codigo
                       &campozoom=ep-codigo }


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF 
          
    es-cfa-emp.ep-codigo:LOAD-MOUSE-POINTER('image/lupa.cur').
    es-cfa-emp.cod-estabel:LOAD-MOUSE-POINTER('image/lupa.cur').
  
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
  {src/adm/template/row-list.i "es-cfa-emp"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es-cfa-emp"}

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

find es-cfa  where rowid(es-cfa) = v-row-parent no-lock no-error.
    if available es-cfa  then do:
        assign es-cfa-emp.classe:SCREEN-VALUE IN FRAME f-main = STRING(es-cfa.classe).
               f-desc-cfa:SCREEN-VALUE = es-cfa.descricao. 
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
    IF es-cfa-emp.cod-estabel:SCREEN-VALUE IN FRAME f-main = "" THEN DO:
        run utp/ut-msgs.p (input "show",
                        input 17006,
                        input "Estabelecimento n∆o poder estar em branco!!!" ).
         return 'ADM-ERROR':U.
    END.


    IF es-cfa-emp.ep-codigo:SCREEN-VALUE IN FRAME f-main = "" THEN DO:
       run utp/ut-msgs.p (input "show",
                        input 17006,
                        input "Empresa n∆o poder estar em branco!!!" ).
         return 'ADM-ERROR':U.
    END.







    
   

    /* Code placed here will execute PRIOR to standard behavior. */
    {include/i-valid.i}


    RUN Pi-validate.
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

 






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

 APPLY "leave"  TO es-cfa-emp.ep-codigo IN FRAME f-main.
 APPLY "leave"  TO es-cfa-emp.cod-estabel IN FRAME f-main.
 FIND es-cfa WHERE es-cfa.classe = es-cfa-emp.classe:SCREEN-VALUE IN FRAME f-main   NO-LOCK NO-ERROR.
 IF AVAIL  es-cfa THEN DO:
     f-desc-cfa:SCREEN-VALUE = es-cfa.descricao.
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


   IF ADM-NEW-RECORD THEN DO:


    FIND es-cfa-emp WHERE es-cfa-emp.ep-codigo         = INPUT FRAME f-main es-cfa-emp.ep-codigo
                    AND   es-cfa-emp.classe            = INPUT FRAME f-main es-cfa-emp.classe 
                    AND   es-cfa-emp.cod-estabel = INPUT FRAME f-main es-cfa-emp.cod-estabel NO-LOCK NO-ERROR.

    IF AVAIL es-cfa-emp THEN DO:
        run utp/ut-msgs.p (input "show",
                        input 17006,
                        input "Relacionamento CFA X EMPRESA X ESTABELECIMENTO Jµ EXISTE!!!" ).
         return 'ADM-ERROR':U. 
    END.

   END.






    
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
  {src/adm/template/sndkycas.i "classe" "es-cfa-emp" "classe"}

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
  {src/adm/template/snd-list.i "es-cfa-emp"}

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


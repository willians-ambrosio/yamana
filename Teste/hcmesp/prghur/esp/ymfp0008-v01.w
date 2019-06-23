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
{include/i-prgvrs.i YMFP0008 12.1.11.000}

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
DEFINE NEW GLOBAL SHARED VAR ym-empresa LIKE rh_estab.cdn_empresa  NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR ym-estab   LIKE rh_estab.cdn_estab    NO-UNDO.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.
DEFINE VARIABLE i-cod-gestor AS INTEGER     NO-UNDO.

DEFINE BUFFER bf_es_gestor FOR es_gestor.

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
&Scoped-define EXTERNAL-TABLES es_Gestor
&Scoped-define FIRST-EXTERNAL-TABLE es_Gestor


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es_Gestor.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es_Gestor.niv_hier_funcnal 
&Scoped-define ENABLED-TABLES es_Gestor
&Scoped-define FIRST-ENABLED-TABLE es_Gestor
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS es_Gestor.cdn_gestor es_Gestor.origem ~
es_Gestor.cdn_empresa es_Gestor.cdn_estab es_Gestor.cdn_funcionario ~
es_Gestor.nom_gestor es_Gestor.niv_hier_funcnal 
&Scoped-define DISPLAYED-TABLES es_Gestor
&Scoped-define FIRST-DISPLAYED-TABLE es_Gestor
&Scoped-Define DISPLAYED-OBJECTS fi_des_niv_hier_funcnal 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS es_Gestor.cdn_gestor es_Gestor.origem ~
es_Gestor.cdn_empresa es_Gestor.cdn_estab es_Gestor.cdn_funcionario ~
es_Gestor.nom_gestor 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS></FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = ':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE fi_des_niv_hier_funcnal AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 1.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 5.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es_Gestor.cdn_gestor AT ROW 1.17 COL 18 COLON-ALIGNED WIDGET-ID 8 FORMAT "99999999"
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
          FONT 0
     es_Gestor.origem AT ROW 1.17 COL 41 NO-LABEL WIDGET-ID 14
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Interno", 1,
"Externo", 2
          SIZE 40 BY .88
     es_Gestor.cdn_empresa AT ROW 2.67 COL 18 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .88
     es_Gestor.cdn_estab AT ROW 3.67 COL 18 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .88
     es_Gestor.cdn_funcionario AT ROW 4.67 COL 18 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 10.29 BY .88
     es_Gestor.nom_gestor AT ROW 5.67 COL 18 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 41.14 BY .88
     es_Gestor.niv_hier_funcnal AT ROW 6.67 COL 18 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .88
     fi_des_niv_hier_funcnal AT ROW 6.67 COL 22.86 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 2.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: hresp.es_Gestor
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
         HEIGHT             = 7.33
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

/* SETTINGS FOR FILL-IN es_Gestor.cdn_empresa IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN es_Gestor.cdn_estab IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN es_Gestor.cdn_funcionario IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN es_Gestor.cdn_gestor IN FRAME f-main
   NO-ENABLE 1 EXP-FORMAT                                               */
ASSIGN 
       es_Gestor.cdn_gestor:READ-ONLY IN FRAME f-main        = TRUE.

/* SETTINGS FOR FILL-IN fi_des_niv_hier_funcnal IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN es_Gestor.nom_gestor IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR RADIO-SET es_Gestor.origem IN FRAME f-main
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

&Scoped-define SELF-NAME es_Gestor.cdn_empresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_Gestor.cdn_empresa V-table-Win
ON F5 OF es_Gestor.cdn_empresa IN FRAME f-main /* Empresa */
DO: 
    {include/zoomvar.i &prog-zoom="adzoom\z01ad101.w"
                     &campo=es_gestor.cdn_empresa
                     &campozoom=ep-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_Gestor.cdn_empresa V-table-Win
ON LEAVE OF es_Gestor.cdn_empresa IN FRAME f-main /* Empresa */
DO:
    ASSIGN ym-empresa = INPUT FRAME {&FRAME-NAME} es_gestor.cdn_empresa.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_Gestor.cdn_empresa V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es_Gestor.cdn_empresa IN FRAME f-main /* Empresa */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es_Gestor.cdn_estab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_Gestor.cdn_estab V-table-Win
ON F5 OF es_Gestor.cdn_estab IN FRAME f-main /* Estabelecimento */
DO:
    ASSIGN ym-empresa = INPUT FRAME {&FRAME-NAME} es_gestor.cdn_empresa.

    {include/zoomvar.i &prog-zoom="prghur\esp\ymfp0008-z02.w"
                     &campo=es_gestor.cdn_estab
                     &campozoom=cdn_estab}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_Gestor.cdn_estab V-table-Win
ON LEAVE OF es_Gestor.cdn_estab IN FRAME f-main /* Estabelecimento */
DO:
    ASSIGN ym-empresa = INPUT FRAME {&FRAME-NAME} es_gestor.cdn_empresa
           ym-estab   = INPUT FRAME {&FRAME-NAME} es_gestor.cdn_estab.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_Gestor.cdn_estab V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es_Gestor.cdn_estab IN FRAME f-main /* Estabelecimento */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es_Gestor.cdn_funcionario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_Gestor.cdn_funcionario V-table-Win
ON F5 OF es_Gestor.cdn_funcionario IN FRAME f-main /* Matricula */
DO:  
    ASSIGN ym-empresa = INPUT FRAME {&FRAME-NAME} es_gestor.cdn_empresa
           ym-estab   = INPUT FRAME {&FRAME-NAME} es_gestor.cdn_estab.

    {include/zoomvar.i &prog-zoom="prghur\esp\ymfp0008-z03.w"
                     &campo=es_gestor.cdn_funcionario
                     &campozoom=cdn_funcionario}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_Gestor.cdn_funcionario V-table-Win
ON LEAVE OF es_Gestor.cdn_funcionario IN FRAME f-main /* Matricula */
DO:
    {include/leave.i &tabela=funcionario
                 &atributo-ref=nom_pessoa_fisic
                 &variavel-ref=nom_gestor
                 &where="funcionario.cdn_empresa     = input frame {&frame-name} es_gestor.cdn_empresa
                     AND funcionario.cdn_estab       = input frame {&frame-name} es_gestor.cdn_estab
                     AND funcionario.cdn_funcionario = input frame {&frame-name} es_gestor.cdn_funcionario "}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_Gestor.cdn_funcionario V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es_Gestor.cdn_funcionario IN FRAME f-main /* Matricula */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es_Gestor.niv_hier_funcnal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_Gestor.niv_hier_funcnal V-table-Win
ON F5 OF es_Gestor.niv_hier_funcnal IN FRAME f-main /* Nivel Hieraquico */
DO:
  {include/zoomvar.i &prog-zoom="object\sopy\zoom\z01py090.w"
                   &campo=es_gestor.niv_hier_funcnal
                   &campozoom=cdn_niv_hier_funcnal}
                                    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_Gestor.niv_hier_funcnal V-table-Win
ON LEAVE OF es_Gestor.niv_hier_funcnal IN FRAME f-main /* Nivel Hieraquico */
DO:  
  {include/leave.i &tabela=niv_hier_funcnal
                 &atributo-ref=des_niv_hier_funcnal
                 &variavel-ref=fi_des_niv_hier_funcnal
                 &where="niv_hier_funcnal.cdn_niv_hier_funcnal = input frame {&frame-name} es_gestor.niv_hier_funcnal"}
                 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_Gestor.niv_hier_funcnal V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es_Gestor.niv_hier_funcnal IN FRAME f-main /* Nivel Hieraquico */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es_Gestor.origem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_Gestor.origem V-table-Win
ON LEAVE OF es_Gestor.origem IN FRAME f-main /* Origem */
DO:
    IF INPUT FRAME {&FRAME-NAME} es_gestor.origem = 1 THEN

        ASSIGN es_gestor.cdn_gestor:SENSITIVE = NO
               es_gestor.nom_gestor:SENSITIVE = NO
               es_gestor.cdn_empresa:SENSITIVE = YES
               es_gestor.cdn_estab:SENSITIVE = YES
               es_gestor.cdn_funcionario:SENSITIVE = YES.
    ELSE DO:
        ASSIGN es_gestor.cdn_gestor:SENSITIVE = NO
               es_gestor.cdn_empresa:SENSITIVE = NO
               es_gestor.cdn_estab:SENSITIVE = NO
               es_gestor.cdn_funcionario:SENSITIVE = NO
               es_gestor.nom_gestor:SENSITIVE = YES
               es_gestor.cdn_funcionario:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0"
               es_gestor.nom_gestor:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_Gestor.origem V-table-Win
ON VALUE-CHANGED OF es_Gestor.origem IN FRAME f-main /* Origem */
DO:
  IF INPUT FRAME {&FRAME-NAME} es_gestor.origem = 1 THEN

      ASSIGN es_gestor.cdn_gestor:SENSITIVE = NO
             es_gestor.nom_gestor:SENSITIVE = NO
             es_gestor.cdn_empresa:SENSITIVE = YES
             es_gestor.cdn_estab:SENSITIVE = YES
             es_gestor.cdn_funcionario:SENSITIVE = YES.
  ELSE DO:
      ASSIGN es_gestor.cdn_gestor:SENSITIVE = NO
             es_gestor.cdn_empresa:SENSITIVE = NO
             es_gestor.cdn_estab:SENSITIVE = NO
             es_gestor.cdn_funcionario:SENSITIVE = NO
             es_gestor.nom_gestor:SENSITIVE = YES.

      RUN pi-calcula-externo.

  END.
             
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
es_gestor.niv_hier_funcnal:load-mouse-pointer ("image/lupa.cur") in frame {&frame-name}.
es_gestor.cdn_funcionario:load-mouse-pointer ("image/lupa.cur") in frame {&frame-name}.
es_gestor.cdn_estab:load-mouse-pointer ("image/lupa.cur") in frame {&frame-name}.
es_gestor.cdn_empresa:load-mouse-pointer ("image/lupa.cur") in frame {&frame-name}.


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
  {src/adm/template/row-list.i "es_Gestor"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es_Gestor"}

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
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /*:T Ponha na pi-validate todas as validaá‰es */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ).

    /* Dispatch standard ADM method.                             */
    disable es_gestor.cdn_gestor with frame {&frame-name}.

    ASSIGN i-cod-gestor  = 1.
    
    REPEAT:
        FIND FIRST bf_es_gestor WHERE 
                   bf_es_gestor.cdn_gestor = i-cod-gestor NO-LOCK NO-ERROR.
        IF NOT AVAIL bf_es_gestor THEN LEAVE.
    
        ASSIGN i-cod-gestor = i-cod-gestor + 1.
    END.
    
    ASSIGN es_gestor.cdn_gestor:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(i-cod-gestor).


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
/*     {include/i-valid.i}  */
    if  not frame {&frame-name}:validate() then
      return 'ADM-ERROR':U.    
    
    /*:T Ponha na pi-validate todas as validaá‰es */
    /*:T N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */

    RUN dispatch IN THIS-PROCEDURE ( INPUT 'pi-validate':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record V-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

    disable es_gestor.cdn_gestor with frame {&frame-name}.

    ASSIGN i-cod-gestor  = 1.
    
    REPEAT:
        FIND FIRST bf_es_gestor WHERE 
                   bf_es_gestor.cdn_gestor = i-cod-gestor NO-LOCK NO-ERROR.
        IF NOT AVAIL bf_es_gestor THEN LEAVE.
    
        ASSIGN i-cod-gestor = i-cod-gestor + 1.
    END.
    
    ASSIGN es_gestor.cdn_gestor:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(i-cod-gestor).


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
FIND CURRENT es_Gestor NO-ERROR.

FIND es_HistGestor 
    WHERE es_HistGestor.cdn_gestor = es_gestor.cdn_gestor NO-LOCK NO-ERROR.
IF AVAIL es_HistGestor THEN  DO:
   run utp/ut-msgs.p (input "show":U, 
                      input 17006, 
                      input "Eliminaá∆o n∆o Permitida !!!~~Gestor possui Historicos, n∆o pode ser eliminado !!!").
   RETURN NO-APPLY.
END.

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
if   available es_gestor then do:
     find niv_hier_funcnal where niv_hier_funcnal.cdn_niv_hier_funcnal = es_gestor.niv_hier_funcnal no-lock no-error.
     assign fi_des_niv_hier_funcnal = if avail niv_hier_funcnal then niv_hier_funcnal.des_niv_hier_funcnal else "":U.
end.
                
RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

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
    
    disable es_gestor.cdn_gestor with frame {&frame-name}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calcula-externo V-table-Win 
PROCEDURE pi-calcula-externo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* FIND LAST es_gestor WHERE es_gestor.cdn_gestor >= 99900000 NO-LOCK NO-ERROR.                       */
/* ASSIGN i-cod-gestor = IF AVAIL es_gestor THEN (es_gestor.cdn_gestor + 1) ELSE 99900000.            */
/*                                                                                                    */
/* ASSIGN es_gestor.cdn_gestor:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(i-cod-gestor,"99999999"). */

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

     IF INPUT FRAME {&FRAME-NAME} es_gestor.origem = 1 /*Interno*/ THEN DO:

         FIND mgcad.empresa
             WHERE empresa.ep-codigo = INPUT FRAME {&frame-name} es_gestor.cdn_empresa NO-LOCK NO-ERROR.
         IF NOT AVAIL empresa THEN DO:
            {include/i-vldprg.i}
            run utp/ut-msgs.p (input "show":U, input 2, input "Empresa").
            return 'ADM-ERROR':U.
         END.

         FIND rh_estab 
             WHERE rh_estab.cdn_empresa = INPUT FRAME {&frame-name} es_gestor.cdn_empresa
               AND rh_estab.cdn_estab   = INPUT FRAME {&frame-name} es_gestor.cdn_estab NO-LOCK NO-ERROR.
         IF NOT AVAIL rh_estab THEN DO:
            {include/i-vldprg.i}
            run utp/ut-msgs.p (input "show":U, input 2, input "Estabelecimento").
            return 'ADM-ERROR':U.
         END.

         FIND funcionario
             WHERE funcionario.cdn_empresa      = INPUT FRAME {&frame-name} es_gestor.cdn_empresa
               AND funcionario.cdn_estab        = INPUT FRAME {&frame-name} es_gestor.cdn_estab
               AND funcionario.cdn_funcionario  = INPUT FRAME {&frame-name} es_gestor.cdn_funcionario
             NO-LOCK NO-ERROR.
         IF NOT AVAIL funcionario THEN DO:
            {include/i-vldprg.i}
            run utp/ut-msgs.p (input "show":U, input 2, input "Funcionario").
            return 'ADM-ERROR':U.
         END.

         if adm-new-record = YES THEN DO:
             FIND bf_es_gestor
                 WHERE bf_es_gestor.cdn_empresa     = INPUT FRAME {&FRAME-NAME} es_gestor.cdn_empresa
                   AND bf_es_gestor.cdn_estab       = INPUT FRAME {&FRAME-NAME} es_gestor.cdn_estab
                   AND bf_es_gestor.cdn_funcionario = INPUT FRAME {&FRAME-NAME} es_gestor.cdn_funcionario
                 NO-LOCK NO-ERROR.
             IF AVAIL bf_es_gestor THEN DO:
                 {include/i-vldprg.i}
                 run utp/ut-msgs.p (input "show":U, input 17006, 
                                    input "Inclus∆o Invalida !!!~~Gestor Ja Cadastrado com essa Matricula para esta Empresa/Estabelecimento.").
                 return 'ADM-ERROR':U.
             END.
         END.

         find param_empres_rh no-lock where
              param_empres_rh.cdn_empresa = INPUT FRAME {&frame-name} es_gestor.cdn_empresa no-error.
         if avail param_empres_rh then 
            if funcionario.cdn_sit_calc_func = 9 or
              (funcionario.dat_desligto_func < date(param_empres_rh.num_mes_refer_calc_efetd,01,param_empres_rh.num_ano_refer_calc_efetd)) then do:
               run utp/ut-msgs.p (input "show", 
                                  input 17006, 
                                  input "Funcionario Desligado.~~Somente Funcionarios Ativos podem ser cadastrados como Gestores.").
               APPLY "Entry" TO es_gestor.cdn_funcionario. 
               return 'ADM-ERROR':U.
            end.                      
     END.
     ELSE DO:
         if adm-new-record = YES THEN DO:
             FIND FIRST bf_es_gestor
                 WHERE bf_es_gestor.cdn_empresa     = INPUT FRAME {&FRAME-NAME} es_gestor.cdn_empresa
                   AND bf_es_gestor.cdn_estab       = INPUT FRAME {&FRAME-NAME} es_gestor.cdn_estab
                   AND bf_es_gestor.cdn_funcionario = INPUT FRAME {&FRAME-NAME} es_gestor.cdn_funcionario
                   AND bf_es_gestor.nom_gestor      = INPUT FRAME {&FRAME-NAME} es_gestor.nom_gestor
                 NO-LOCK NO-ERROR.
             IF AVAIL bf_es_gestor THEN DO:
                 {include/i-vldprg.i}
                 run utp/ut-msgs.p (input "show":U, input 17006, 
                                    input "Inclus∆o Invalida !!!~~Gestor EXTERNO j† cadastrado com este Nome Gestor.").
                 return 'ADM-ERROR':U.
             END.
         END.
     END.

     FIND niv_hier_funcnal
         WHERE niv_hier_funcnal.cdn_niv_hier_funcnal = INPUT FRAME {&frame-name} es_Gestor.niv_hier_funcnal
         NO-LOCK NO-ERROR.
     IF NOT AVAIL niv_hier_funcnal THEN DO:
        {include/i-vldprg.i}
        run utp/ut-msgs.p (input "show":U, input 2, input "Nivel de Hierarquia").
        return 'ADM-ERROR':U.
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
  {src/adm/template/snd-list.i "es_Gestor"}

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


&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems5             PROGRESS
          ems5_esp         PROGRESS
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
{include/i-prgvrs.i ESAPB013 12.01.23.000}

/* Chamada a include do gerenciador de licen�as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m�dulo>:  Informar qual o m�dulo a qual o programa pertence.                  */
/*                                                                                */
/* OBS: Para os smartobjects o parametro m�dulo dever� ser MUT                    */

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

DEFINE BUFFER empresa FOR ems5.empresa.

/* global variable definitions */
DEFINE NEW GLOBAL SHARED VAR v_cod_usuar_corren       LIKE usuar_mestre.cod_usuario no-undo.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.

def new global shared var v_rec_empresa
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_plano_cta_ctbl
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_cta_ctbl
    as recid
    format ">>>>>>9":U
    no-undo.

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
&Scoped-define EXTERNAL-TABLES es-cat-code-cta-ex
&Scoped-define FIRST-EXTERNAL-TABLE es-cat-code-cta-ex


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR es-cat-code-cta-ex.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es-cat-code-cta-ex.cod_plano_cta_ctbl 
&Scoped-define ENABLED-TABLES es-cat-code-cta-ex
&Scoped-define FIRST-ENABLED-TABLE es-cat-code-cta-ex
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS es-cat-code-cta-ex.cod_plano_cta_ctbl ~
es-cat-code-cta-ex.cod_cta_ctbl es-cat-code-cta-ex.cod_usuario ~
es-cat-code-cta-ex.dt-trans es-cat-code-cta-ex.hr-trans 
&Scoped-define DISPLAYED-TABLES es-cat-code-cta-ex
&Scoped-define FIRST-DISPLAYED-TABLE es-cat-code-cta-ex
&Scoped-Define DISPLAYED-OBJECTS fi-nom-plano-cta fi-nom-conta ~
fi-nom-usuario 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS es-cat-code-cta-ex.cod_plano_cta_ctbl ~
es-cat-code-cta-ex.cod_cta_ctbl 
&Scoped-define ADM-ASSIGN-FIELDS es-cat-code-cta-ex.cod_usuario ~
es-cat-code-cta-ex.dt-trans es-cat-code-cta-ex.hr-trans 

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
DEFINE VARIABLE fi-nom-conta AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 44.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nom-plano-cta AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 53.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nom-usuario AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 2.5.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.57 BY 3.46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     es-cat-code-cta-ex.cod_plano_cta_ctbl AT ROW 1.29 COL 18 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     fi-nom-plano-cta AT ROW 1.29 COL 30.29 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     es-cat-code-cta-ex.cod_cta_ctbl AT ROW 2.21 COL 18 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 21.14 BY .88
     fi-nom-conta AT ROW 2.21 COL 39.29 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     es-cat-code-cta-ex.cod_usuario AT ROW 3.88 COL 18 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 16.14 BY .88
     fi-nom-usuario AT ROW 3.88 COL 34.29 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     es-cat-code-cta-ex.dt-trans AT ROW 4.88 COL 18 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     es-cat-code-cta-ex.hr-trans AT ROW 5.88 COL 18 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 3.58 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ems5_esp.es-cat-code-cta-ex
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
         HEIGHT             = 6.25
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

/* SETTINGS FOR FILL-IN es-cat-code-cta-ex.cod_cta_ctbl IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN es-cat-code-cta-ex.cod_plano_cta_ctbl IN FRAME f-main
   1                                                                    */
/* SETTINGS FOR FILL-IN es-cat-code-cta-ex.cod_usuario IN FRAME f-main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN es-cat-code-cta-ex.dt-trans IN FRAME f-main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi-nom-conta IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nom-plano-cta IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nom-usuario IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN es-cat-code-cta-ex.hr-trans IN FRAME f-main
   NO-ENABLE 2                                                          */
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

&Scoped-define SELF-NAME es-cat-code-cta-ex.cod_cta_ctbl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-cat-code-cta-ex.cod_cta_ctbl V-table-Win
ON F5 OF es-cat-code-cta-ex.cod_cta_ctbl IN FRAME f-main /* Conta Ctbl */
DO:
    ASSIGN v_rec_cta_ctbl  = ?.

    run prgint/utb/utb080nc.p /*prg_see_cta_ctbl_plano*/.
    if  v_rec_cta_ctbl <> ? THEN
    DO:
       FIND FIRST cta_ctbl WHERE
            RECID(cta_ctbl) = v_rec_cta_ctbl NO-LOCK NO-ERROR.
       IF AVAIL cta_ctbl THEN
         ASSIGN es-cat-code-cta-ex.cod_cta_ctbl:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cta_ctbl.cod_cta_ctbl
                fi-nom-conta                   :SCREEN-VALUE IN FRAME {&FRAME-NAME} = cta_ctbl.des_tit_ctbl.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-cat-code-cta-ex.cod_cta_ctbl V-table-Win
ON LEAVE OF es-cat-code-cta-ex.cod_cta_ctbl IN FRAME f-main /* Conta Ctbl */
DO:
    FIND FIRST cta_ctbl NO-LOCK
        WHERE cta_ctbl.cod_cta_ctbl = INPUT FRAME {&FRAME-NAME} es-cat-code-cta-ex.cod_cta_ctbl NO-ERROR.
    IF AVAIL cta_ctbl THEN
        ASSIGN fi-nom-conta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cta_ctbl.des_tit_ctbl.
    ELSE
        ASSIGN fi-nom-conta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-cat-code-cta-ex.cod_cta_ctbl V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-cat-code-cta-ex.cod_cta_ctbl IN FRAME f-main /* Conta Ctbl */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-cat-code-cta-ex.cod_plano_cta_ctbl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-cat-code-cta-ex.cod_plano_cta_ctbl V-table-Win
ON F5 OF es-cat-code-cta-ex.cod_plano_cta_ctbl IN FRAME f-main /* Plano de Contas */
DO:
    ASSIGN v_rec_plano_cta_ctbl = ?.

    run prgint/utb/utb080ka.p /*prg_sea_plano_cta_ctbl*/.
        
    if  v_rec_plano_cta_ctbl <> ? THEN
    DO:
        FIND FIRST plano_cta_ctbl WHERE 
                   RECID(plano_cta_ctbl) = v_rec_plano_cta_ctbl NO-LOCK NO-ERROR.
        IF AVAIL plano_cta_ctbl THEN
            ASSIGN es-cat-code-cta-ex.cod_plano_cta_ctbl:SCREEN-VALUE IN FRAME {&FRAME-NAME} = plano_cta_ctbl.cod_plano_cta_ctbl 
                   fi-nom-plano-cta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = plano_cta_ctbl.des_tit_ctbl.
        ELSE
            ASSIGN fi-nom-plano-cta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "". 
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-cat-code-cta-ex.cod_plano_cta_ctbl V-table-Win
ON LEAVE OF es-cat-code-cta-ex.cod_plano_cta_ctbl IN FRAME f-main /* Plano de Contas */
DO:
    FIND FIRST plano_cta_ctbl WHERE 
               plano_cta_ctbl.cod_plano_cta_ctbl = INPUT FRAME {&FRAME-NAME} es-cat-code-cta-ex.cod_plano_cta_ctbl NO-LOCK NO-ERROR.
    IF AVAIL plano_cta_ctbl THEN
        ASSIGN fi-nom-plano-cta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = plano_cta_ctbl.des_tit_ctbl.
    ELSE
        ASSIGN fi-nom-plano-cta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "". 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-cat-code-cta-ex.cod_plano_cta_ctbl V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-cat-code-cta-ex.cod_plano_cta_ctbl IN FRAME f-main /* Plano de Contas */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es-cat-code-cta-ex.cod_usuario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-cat-code-cta-ex.cod_usuario V-table-Win
ON F5 OF es-cat-code-cta-ex.cod_usuario IN FRAME f-main /* Usu�rio */
DO:
    {include/zoomvar.i &prog-zoom=fnzoom\z01fn017.r
                       &campo=es-cat-code-cta-ex.cod_usuario
                       &campozoom=cod_usuario}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-cat-code-cta-ex.cod_usuario V-table-Win
ON LEAVE OF es-cat-code-cta-ex.cod_usuario IN FRAME f-main /* Usu�rio */
DO:
   FIND FIRST usuar_mestre WHERE 
              usuar_mestre.cod_usuario = es-cat-code-cta-ex.cod_usuario:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
   IF AVAIL usuar_mestre THEN
      ASSIGN fi-nom-usuario:SCREEN-VALUE IN FRAME {&FRAME-NAME} = usuar_mestre.nom_usuario.
   ELSE
      ASSIGN fi-nom-usuario:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es-cat-code-cta-ex.cod_usuario V-table-Win
ON MOUSE-SELECT-DBLCLICK OF es-cat-code-cta-ex.cod_usuario IN FRAME f-main /* Usu�rio */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  es-cat-code-cta-ex.cod_plano_cta_ctbl:Load-mouse-pointer ("image/lupa.cur":U) In Frame {&FRAME-NAME}.
  es-cat-code-cta-ex.cod_cta_ctbl      :Load-mouse-pointer ("image/lupa.cur":U) In Frame {&FRAME-NAME}.
  es-cat-code-cta-ex.cod_usuario       :Load-mouse-pointer ("image/lupa.cur":U) In Frame {&FRAME-NAME}.

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
  {src/adm/template/row-list.i "es-cat-code-cta-ex"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "es-cat-code-cta-ex"}

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
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN es-cat-code-cta-ex.dt-trans    :screen-value in frame {&frame-name} = STRING(TODAY)
         es-cat-code-cta-ex.hr-trans    :screen-value in frame {&frame-name} = STRING(TIME,"hh:mm:ss")
         es-cat-code-cta-ex.cod_usuario :screen-value in frame {&frame-name} = v_cod_usuar_corren.

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
    
    /*:T Ponha na pi-validate todas as valida��es */
    /*:T N�o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assign�s n�o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record V-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN es-cat-code-cta-ex.dt-trans    :screen-value in frame {&frame-name} = STRING(TODAY)
         es-cat-code-cta-ex.hr-trans    :screen-value in frame {&frame-name} = STRING(TIME,"hh:mm:ss")
         es-cat-code-cta-ex.cod_usuario :screen-value in frame {&frame-name} = v_cod_usuar_corren.

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

  /* Code placed here will execute AFTER standard behavior.    */

  APPLY "LEAVE" TO es-cat-code-cta-ex.cod_plano_cta_ctbl  IN FRAME {&FRAME-NAME}.
  APPLY "LEAVE" TO es-cat-code-cta-ex.cod_cta_ctbl        IN FRAME {&FRAME-NAME}.  
  APPLY "LEAVE" TO es-cat-code-cta-ex.cod_usuario         IN FRAME {&FRAME-NAME}.  

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

    if adm-new-record = NO then
       DISABLE es-cat-code-cta-ex.cod_plano_cta_ctbl with frame {&frame-name}.

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
  Notes: N�o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida��es, pois neste ponto do programa o registro 
  ainda n�o foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Valida��o de dicion�rio */
    
/*:T    Segue um exemplo de valida��o de programa */
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
  {src/adm/template/snd-list.i "es-cat-code-cta-ex"}

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


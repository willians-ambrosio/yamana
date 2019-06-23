&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          hresp            PROGRESS
*/
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i XX9999 9.99.99.999}

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> <m¢dulo>}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEF NEW GLOBAL SHARED VAR p-row-upc-fp1350      AS ROWID NO-UNDO.

/* Parameters Definitions ---                                           */

/* Parameters Definitions ---                                           */

{include/i_dbvers.i}

def input parameter v_prh_program      as handle             no-undo.
def input parameter v_row_parent       as rowid              no-undo.
def input parameter v_row_table        as rowid              no-undo.
/* Local Variable Definitions ---                                       */

DEFINE BUFFER b_es_HistGestor FOR es_histGestor.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE JanelaDetalhe
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS es_HistGestor.cdn_gestor ~
es_HistGestor.da-inicio es_HistGestor.da-final 
&Scoped-define ENABLED-TABLES es_HistGestor
&Scoped-define FIRST-ENABLED-TABLE es_HistGestor
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 bt-ok bt-cancelar ~
bt-ajuda 
&Scoped-Define DISPLAYED-FIELDS es_HistGestor.cdn_estab ~
es_HistGestor.cdn_funcionario es_HistGestor.cdn_gestor ~
es_Gestor.niv_hier_funcnal es_HistGestor.da-inicio es_HistGestor.da-final 
&Scoped-define DISPLAYED-TABLES es_HistGestor es_Gestor
&Scoped-define FIRST-DISPLAYED-TABLE es_HistGestor
&Scoped-define SECOND-DISPLAYED-TABLE es_Gestor
&Scoped-Define DISPLAYED-OBJECTS fi-nom_funcionario fi_nom_gestor ~
fi_des_niv_hier_funcnal 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-window AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE fi-nom_funcionario AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .88 NO-UNDO.

DEFINE VARIABLE fi_des_niv_hier_funcnal AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .88 NO-UNDO.

DEFINE VARIABLE fi_nom_gestor AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 77.86 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 2.75.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 3.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     es_HistGestor.cdn_estab AT ROW 1.25 COL 18 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
     es_HistGestor.cdn_funcionario AT ROW 2.25 COL 18 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 10.29 BY .88
     fi-nom_funcionario AT ROW 2.25 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     es_HistGestor.cdn_gestor AT ROW 4 COL 18 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 8 BY .88
     fi_nom_gestor AT ROW 4 COL 26 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     es_Gestor.niv_hier_funcnal AT ROW 5 COL 18 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .88
     fi_des_niv_hier_funcnal AT ROW 5 COL 23 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     es_HistGestor.da-inicio AT ROW 6 COL 18 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     es_HistGestor.da-final AT ROW 6 COL 54 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     bt-ok AT ROW 8.21 COL 3
     bt-cancelar AT ROW 8.21 COL 14
     bt-ajuda AT ROW 8.21 COL 69
     RECT-1 AT ROW 8 COL 2
     RECT-2 AT ROW 1 COL 1 WIDGET-ID 16
     RECT-3 AT ROW 3.75 COL 1 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 12.58 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: JanelaDetalhe
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert Custom SmartWindow title>"
         HEIGHT             = 8.63
         WIDTH              = 79.29
         MAX-HEIGHT         = 21.13
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.13
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-window 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-window.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN es_HistGestor.cdn_estab IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN es_HistGestor.cdn_funcionario IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nom_funcionario IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_des_niv_hier_funcnal IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_nom_gestor IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN es_Gestor.niv_hier_funcnal IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* <insert Custom SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* <insert Custom SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-window
ON CHOOSE OF bt-ajuda IN FRAME F-Main /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-window
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
  /*apply "close":U to this-procedure.*/

    run pi-valida-historico IN THIS-PROCEDURE.

    if return-value = "adm-error" then
       return no-apply.
    RUN dispatch IN v_prh_program ('open-query':U).
    apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME es_HistGestor.cdn_gestor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_HistGestor.cdn_gestor w-window
ON F5 OF es_HistGestor.cdn_gestor IN FRAME F-Main /* Cod.Gestor */
DO:
  
    {include/zoomvar.i &prog-zoom="prghur\esp\ymfp0008-z01.w"
                     &campo=es_HistGestor.cdn_gestor
                     &campozoom=cdn_gestor}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_HistGestor.cdn_gestor w-window
ON LEAVE OF es_HistGestor.cdn_gestor IN FRAME F-Main /* Cod.Gestor */
DO:
  
    {include/leave.i &tabela=es_gestor
                 &atributo-ref=nom_gestor
                 &variavel-ref=fi_nom_gestor
                 &where="es_gestor.cdn_gestor = input frame {&frame-name} es_HistGestor.cdn_gestor"}
                 
    FIND CURRENT es_gestor NO-LOCK NO-ERROR.
    IF AVAIL es_gestor THEN
        DISPLAY es_gestor.niv_hier_funcnal WITH FRAME {&FRAME-NAME}.


    {include/leave.i &tabela=niv_hier_funcnal
                   &atributo-ref=des_niv_hier_funcnal
                   &variavel-ref=fi_des_niv_hier_funcnal
                   &where="niv_hier_funcnal.cdn_niv_hier_funcnal = input frame {&frame-name} es_gestor.niv_hier_funcnal"}


    ASSIGN es_HistGestor.da-inicio:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY)
           es_HistGestor.da-final:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(DATE(12/31/9999)).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL es_HistGestor.cdn_gestor w-window
ON MOUSE-SELECT-DBLCLICK OF es_HistGestor.cdn_gestor IN FRAME F-Main /* Cod.Gestor */
DO:
  APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */
es_HistGestor.cdn_gestor:load-mouse-pointer ("image/lupa.cur") in frame {&frame-name}.
/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-window  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-window  _ADM-ROW-AVAILABLE
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-window  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
  THEN DELETE WIDGET w-window.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-window  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fi-nom_funcionario fi_nom_gestor fi_des_niv_hier_funcnal 
      WITH FRAME F-Main IN WINDOW w-window.
  IF AVAILABLE es_Gestor THEN 
    DISPLAY es_Gestor.niv_hier_funcnal 
      WITH FRAME F-Main IN WINDOW w-window.
  IF AVAILABLE es_HistGestor THEN 
    DISPLAY es_HistGestor.cdn_estab es_HistGestor.cdn_funcionario 
          es_HistGestor.cdn_gestor es_HistGestor.da-inicio 
          es_HistGestor.da-final 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-1 RECT-2 RECT-3 es_HistGestor.cdn_gestor es_HistGestor.da-inicio 
         es_HistGestor.da-final bt-ok bt-cancelar bt-ajuda 
      WITH FRAME F-Main IN WINDOW w-window.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-window.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-window 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-window 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-window 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}
  
  {utp/ut9000.i "UPC-FP1350-U01" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  FIND funcionario WHERE ROWID(funcionario) = p-row-upc-fp1350 NO-LOCK NO-ERROR.
  ASSIGN es_histgestor.cdn_estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = funcionario.cdn_estab
         es_histgestor.cdn_funcionario:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(funcionario.cdn_funcionario)
         fi-nom_funcionario:SCREEN-VALUE IN FRAME {&FRAME-NAME} = funcionario.nom_pessoa_fisic.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-historico w-window 
PROCEDURE pi-cria-historico :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


   CREATE es_histGestor.
   ASSIGN es_histGestor.cdn_empresa     = v_cdn_empres_usuar
          es_histGestor.cdn_estab       = INPUT FRAME {&FRAME-NAME} es_histGestor.cdn_estab 
          es_histGestor.cdn_funcionario = INPUT FRAME {&FRAME-NAME} es_histGestor.cdn_funcionario
          es_histGestor.cdn_gestor      = INPUT FRAME {&FRAME-NAME} es_histGestor.cdn_gestor
          es_histGestor.da-inicio       = INPUT FRAME {&FRAME-NAME} es_histGestor.da-inicio
          es_histGestor.da-final        = INPUT FRAME {&FRAME-NAME} es_histGestor.da-final.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava-historico w-window 
PROCEDURE pi-grava-historico :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND LAST b_es_HistGestor USE-INDEX idx_GestorIni
    WHERE b_es_HistGestor.cdn_empresa     = v_cdn_empres_usuar
      AND b_es_HistGestor.cdn_estab       = INPUT FRAME {&FRAME-NAME} es_HistGestor.cdn_estab 
      AND b_es_HistGestor.cdn_funcionario = INPUT FRAME {&FRAME-NAME} es_HistGestor.cdn_funcionario
/*       AND b_es_HistGestor.cdn_gestor      = INPUT FRAME {&FRAME-NAME} es_HistGestor.cdn_gestor  */
     NO-ERROR.

IF NOT AVAIL b_es_HistGestor THEN DO:
    RUN pi-cria-historico.
END. 
ELSE DO:
    ASSIGN b_es_HistGestor.da-final = INPUT FRAME {&FRAME-NAME} es_HistGestor.da-inicio - 1.
    RUN pi-cria-historico.
END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida-historico w-window 
PROCEDURE pi-valida-historico :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>                                                                    
  Notes:       
------------------------------------------------------------------------------*/
     FIND es_gestor
         WHERE es_gestor.cdn_gestor = INPUT FRAME {&frame-name} es_histGestor.cdn_gestor
         NO-LOCK NO-ERROR.
     IF NOT AVAIL es_gestor THEN DO:        
         run utp/ut-msgs.p (input "show":U,INPUT 2, input "Gestor !!!").
         return 'ADM-ERROR':U.
     END.

     FIND niv_hier_funcnal
         WHERE niv_hier_funcnal.cdn_niv_hier_funcnal = INPUT FRAME {&frame-name} es_Gestor.niv_hier_funcnal
         NO-LOCK NO-ERROR.
     IF NOT AVAIL niv_hier_funcnal THEN DO:
        run utp/ut-msgs.p (input "show":U, input 2, input "Nivel de Hierarquia").
        return 'ADM-ERROR':U.
     END.

     IF INPUT FRAME {&FRAME-NAME} es_histGestor.da-inicio > INPUT FRAME {&FRAME-NAME} es_histGestor.da-final
     THEN DO:
         run utp/ut-msgs.p (input "show":U, input 17006, 
                            input "Data Inicial Invalida !!!~~Data de Inicio deve ser maio que Final").
         return 'ADM-ERROR':U.
     END.

     FIND FIRST es_histGestor
         WHERE es_histGestor.cdn_empresa     = v_cdn_empres_usuar
           AND es_histGestor.cdn_estab       = INPUT FRAME {&FRAME-NAME} es_histGestor.cdn_estab 
           AND es_histGestor.cdn_funcionario = INPUT FRAME {&FRAME-NAME} es_histGestor.cdn_funcionario
           AND es_histGestor.cdn_gestor      = INPUT FRAME {&FRAME-NAME} es_histGestor.cdn_gestor
           AND es_histGestor.da-inicio       >= INPUT FRAME {&FRAME-NAME} es_histGestor.da-inicio
         NO-LOCK NO-ERROR.

     IF AVAIL es_histGestor THEN DO:
         IF es_histGestor.da-inicio = INPUT FRAME {&FRAME-NAME} es_histGestor.da-inicio THEN
             run utp/ut-msgs.p (input "show":U, input 17006, 
                                input "Data de Inicio Invalida !!!~~Gestor ja relacionado ao Colaborador com esta data de inicio.").
         ELSE 
             run utp/ut-msgs.p (input "show":U, input 17006, 
                                input "Data de Inicio Invalida !!!~~Data de Inicio deve ser maior que o ultimo historico informado.").

         APPLY "Entry" TO es_histGestor.da-inicio IN FRAME {&FRAME-NAME}.
         return 'ADM-ERROR':U.

     END.

     FIND FIRST es_histGestor
         WHERE es_histGestor.cdn_empresa     = v_cdn_empres_usuar
           AND es_histGestor.cdn_estab       = INPUT FRAME {&FRAME-NAME} es_histGestor.cdn_estab 
           AND es_histGestor.cdn_funcionario = INPUT FRAME {&FRAME-NAME} es_histGestor.cdn_funcionario
           AND es_histGestor.da-inicio      >= INPUT FRAME {&FRAME-NAME} es_histGestor.da-inicio
         NO-LOCK NO-ERROR.

     IF AVAIL es_histGestor THEN DO:
         run utp/ut-msgs.p (input "show":U, input 17006, 
                            input "Data Final Invalida !!!~~Ja existe Gestor cadastrado no periodo que abrange a data informada.").
         return 'ADM-ERROR':U.
     END.

     FIND FIRST es_histGestor
         WHERE es_histGestor.cdn_empresa     = v_cdn_empres_usuar
           AND es_histGestor.cdn_estab       = INPUT FRAME {&FRAME-NAME} es_histGestor.cdn_estab 
           AND es_histGestor.cdn_funcionario = INPUT FRAME {&FRAME-NAME} es_histGestor.cdn_funcionario
           AND es_histGestor.da-inicio       = INPUT FRAME {&FRAME-NAME} es_histGestor.da-inicio
         NO-LOCK NO-ERROR.

     IF AVAIL es_histGestor THEN DO:
         run utp/ut-msgs.p (input "show":U, input 17006, 
                            input "Data de Inicio Invalida !!!~~Ja existe Gestor cadastrado com esta data de Inicio.").
         return 'ADM-ERROR':U.
     END.

     FIND funcionario
         WHERE funcionario.cdn_empresa      = es_HistGestor.cdn_empresa
           AND funcionario.cdn_estab        = es_HistGestor.cdn_estab
           AND funcionario.cdn_funcionario  = es_HistGestor.cdn_funcionario
         NO-LOCK NO-ERROR.
     IF AVAIL funcionario AND funcionario.dat_admis_func > INPUT FRAME {&FRAME-NAME} es_histGestor.da-inicio THEN DO:
         run utp/ut-msgs.p (input "show":U, input 17006,
                            input "Data de Inicio Invalida !!!~~Data de Inicio deve ser Maior que a de Admiss∆o do Funcionario.").
        return 'ADM-ERROR':U.
     END.

     RUN pi-grava-historico.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_entry_atrib w-window 
PROCEDURE pi_entry_atrib :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable v_han_object as handle no-undo.

assign v_han_object = frame {&frame-name}:first-child /* Busca o field-group        */
       v_han_object = v_han_object:first-tab-item.    /* Primeiro item da tabulaªío */

do while valid-handle(v_han_object):
    if  v_han_object:sensitive then do:
        apply 'entry' to v_han_object.
        leave.
    end.
    assign v_han_object = v_han_object:next-tab-item.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_reposiciona w-window 
PROCEDURE pi_reposiciona :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  find b_es_HistGestor where rowid(b_es_HistGestor) = v_row_table no-lock no-wait no-error.

  if available b_es_HistGestor then do:
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-window  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this JanelaDetalhe, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-window 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


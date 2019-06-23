&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems5             PROGRESS
          ems5_esp         PROGRESS
*/
&Scoped-define WINDOW-NAME w-cadsimples
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-cadsimples 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i YG0008 12.1.17.001}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */


/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

def var p-table as rowid.

DEFINE NEW GLOBAL SHARED VARIABLE gr-categ_ptoelet AS ROWID NO-UNDO.

DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES concur_param

/* Definitions for FRAME f-cad                                          */
&Scoped-define FIELDS-IN-QUERY-f-cad concur_param.dir_sucesso ~
concur_param.dir_pendente concur_param.dir_erro 
&Scoped-define ENABLED-FIELDS-IN-QUERY-f-cad concur_param.dir_sucesso ~
concur_param.dir_pendente concur_param.dir_erro 
&Scoped-define ENABLED-TABLES-IN-QUERY-f-cad concur_param
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-f-cad concur_param
&Scoped-define QUERY-STRING-f-cad FOR EACH concur_param SHARE-LOCK
&Scoped-define OPEN-QUERY-f-cad OPEN QUERY f-cad FOR EACH concur_param SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-f-cad concur_param
&Scoped-define FIRST-TABLE-IN-QUERY-f-cad concur_param


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS concur_param.dir_sucesso ~
concur_param.dir_pendente concur_param.dir_erro 
&Scoped-define ENABLED-TABLES concur_param
&Scoped-define FIRST-ENABLED-TABLE concur_param
&Scoped-Define ENABLED-OBJECTS rt-button RECT-1 BUTTON-6 BUTTON-4 BUTTON-2 ~
BUTTON-5 
&Scoped-Define DISPLAYED-FIELDS concur_param.dir_sucesso ~
concur_param.dir_pendente concur_param.dir_erro 
&Scoped-define DISPLAYED-TABLES concur_param
&Scoped-define FIRST-DISPLAYED-TABLE concur_param


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-cadsimples AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-cadastro MENUBAR
       MENU-ITEM mi-sair        LABEL "Sair"          
       SUB-MENU  mi-ajuda       LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "image/file.png":U
     LABEL "Button 2" 
     SIZE 4.57 BY 1.25.

DEFINE BUTTON BUTTON-4 
     IMAGE-UP FILE "image/file.png":U
     LABEL "Button 4" 
     SIZE 4.57 BY 1.25.

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "image/file.png":U
     LABEL "Button 5" 
     SIZE 4.57 BY 1.25.

DEFINE BUTTON BUTTON-6 
     IMAGE-UP FILE "image/save-button.png":U
     LABEL "Salvar" 
     SIZE 12 BY 1.08 TOOLTIP "Salvar".

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 6.13.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 92 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY f-cad FOR 
      concur_param SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     BUTTON-6 AT ROW 1.21 COL 40 WIDGET-ID 20
     concur_param.dir_sucesso AT ROW 3 COL 4.28 WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 57 BY .88
     BUTTON-4 AT ROW 3 COL 85 WIDGET-ID 16
     concur_param.dir_pendente AT ROW 5 COL 2.43 WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 57 BY .88
     BUTTON-2 AT ROW 5 COL 85 WIDGET-ID 12
     concur_param.dir_erro AT ROW 7 COL 4 WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 57 BY .88
     BUTTON-5 AT ROW 7 COL 85 WIDGET-ID 18
     rt-button AT ROW 1 COL 1
     RECT-1 AT ROW 2.63 COL 1 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 92.86 BY 8 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-cadsimples ASSIGN
         HIDDEN             = YES
         TITLE              = "Manuten‡Æo Parametros Concur"
         HEIGHT             = 8
         WIDTH              = 92.29
         MAX-HEIGHT         = 22.25
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.25
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-cadastro:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-cadsimples 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-cadsimples
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* SETTINGS FOR FILL-IN concur_param.dir_erro IN FRAME f-cad
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN concur_param.dir_pendente IN FRAME f-cad
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN concur_param.dir_sucesso IN FRAME f-cad
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadsimples)
THEN w-cadsimples:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-cad
/* Query rebuild information for FRAME f-cad
     _TblList          = "ems5_esp.concur_param"
     _Query            is OPENED
*/  /* FRAME f-cad */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-cadsimples
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadsimples w-cadsimples
ON END-ERROR OF w-cadsimples /* Manuten‡Æo Parametros Concur */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadsimples w-cadsimples
ON WINDOW-CLOSE OF w-cadsimples /* Manuten‡Æo Parametros Concur */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 w-cadsimples
ON CHOOSE OF BUTTON-2 IN FRAME f-cad /* Button 2 */
DO:
   SYSTEM-DIALOG GET-DIR c-arquivo
         INITIAL-DIR SESSION:TEMP-DIRECTORY
         RETURN-TO-START-DIR 
     TITLE "Diret¢rio para c¢pia arquivos lidos com sucesso".


    concur_param.dir_pendente:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = c-arquivo.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 w-cadsimples
ON CHOOSE OF BUTTON-4 IN FRAME f-cad /* Button 4 */
DO:
  
    SYSTEM-DIALOG GET-DIR c-arquivo
         INITIAL-DIR SESSION:TEMP-DIRECTORY
         RETURN-TO-START-DIR 
     TITLE "Diret¢rio para c¢pia arquivos lidos com sucesso".

     ASSIGN concur_param.dir_sucesso:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = c-arquivo.

            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 w-cadsimples
ON CHOOSE OF BUTTON-5 IN FRAME f-cad /* Button 5 */
DO:
   SYSTEM-DIALOG GET-DIR c-arquivo
         INITIAL-DIR SESSION:TEMP-DIRECTORY
         RETURN-TO-START-DIR 
     TITLE "Diret¢rio para c¢pia arquivos lidos com sucesso".


    ASSIGN concur_param.dir_erro:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-arquivo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 w-cadsimples
ON CHOOSE OF BUTTON-6 IN FRAME f-cad /* Salvar */
DO:
  FIND FIRST concur_param NO-ERROR.
  IF NOT AVAIL concur_param
       THEN CREATE concur_param.
  
  ASSIGN concur_param.dir_erro     = concur_param.dir_erro:SCREEN-VALUE IN FRAME {&FRAME-NAME}
         concur_param.dir_pendente = concur_param.dir_pendente:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
         concur_param.dir_sucesso  = concur_param.dir_sucesso:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-cadsimples
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-cadsimples
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-cadsimples
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-cadsimples 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */

{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-cadsimples  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.17 , 76.43 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             BUTTON-6:HANDLE IN FRAME f-cad , 'BEFORE':U ).
    END. /* Page 0 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-cadsimples  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadsimples)
  THEN DELETE WIDGET w-cadsimples.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-cadsimples  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-f-cad}
  GET FIRST f-cad.
  IF AVAILABLE concur_param THEN 
    DISPLAY concur_param.dir_sucesso concur_param.dir_pendente 
          concur_param.dir_erro 
      WITH FRAME f-cad IN WINDOW w-cadsimples.
  ENABLE rt-button RECT-1 BUTTON-6 concur_param.dir_sucesso BUTTON-4 
         concur_param.dir_pendente BUTTON-2 concur_param.dir_erro BUTTON-5 
      WITH FRAME f-cad IN WINDOW w-cadsimples.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-cadsimples.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-cadsimples 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-cadsimples 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-cadsimples 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}

  {utp/ut9000.i "AP003-W" "12.1.17.001"}

  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  FIND FIRST concur_param NO-LOCK NO-ERROR.
  IF AVAIL concur_param
       THEN DISP concur_param.dir_erro 
                 concur_param.dir_pendente 
                 concur_param.dir_sucesso
      WITH FRAME {&FRAME-NAME}.

  /* Code placed here will execute AFTER standard behavior.    */
  RUN pi-after-initialize.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PI-AFTER-INITIALIZE w-cadsimples 
PROCEDURE PI-AFTER-INITIALIZE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PI-DISABLE-MENU w-cadsimples 
PROCEDURE PI-DISABLE-MENU :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i YMUT0002A 2.06.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE NEW GLOBAL SHARED VARIABLE cod-emitente-ini LIKE es-item-doc-est-natoper.cod-emitente NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cod-emitente-fim LIKE es-item-doc-est-natoper.cod-emitente NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE l-cliente          AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-fornecedor       AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-ambos            AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-pessoa-fisica    AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-pessoa-juridica  AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-estrangeiro      AS LOGICAL NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-trading          AS LOGICAL NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 IMAGE-1 IMAGE-2 RECT-5 RECT-6 ~
cod-emitente-ini cod-emitente-fim tg-cliente tg-pessoa-fisica tg-fornecedor ~
tg-pessoa-juridica tg-ambos tg-estrangeiro tg-trading bt-ok bt-cancelar ~
bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS cod-emitente-ini cod-emitente-fim ~
tg-cliente tg-pessoa-fisica tg-fornecedor tg-pessoa-juridica tg-ambos ~
tg-estrangeiro tg-trading 

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

DEFINE VARIABLE cod-emitente-fim AS INTEGER FORMAT ">>>>>>>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88.

DEFINE VARIABLE cod-emitente-ini AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Emitente" 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 77.86 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77.86 BY 1.17
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 4.71
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38.57 BY 4.71
     BGCOLOR 7 .

DEFINE VARIABLE tg-ambos AS LOGICAL INITIAL no 
     LABEL "Ambos" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-cliente AS LOGICAL INITIAL no 
     LABEL "Cliente" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-estrangeiro AS LOGICAL INITIAL no 
     LABEL "Estrangeiro" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.29 BY .83 NO-UNDO.

DEFINE VARIABLE tg-fornecedor AS LOGICAL INITIAL no 
     LABEL "Fornecedor" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .83 NO-UNDO.

DEFINE VARIABLE tg-pessoa-fisica AS LOGICAL INITIAL no 
     LABEL "Pessoa Fisica" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.29 BY .83 NO-UNDO.

DEFINE VARIABLE tg-pessoa-juridica AS LOGICAL INITIAL no 
     LABEL "Pessoa Juridica" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.29 BY .83 NO-UNDO.

DEFINE VARIABLE tg-trading AS LOGICAL INITIAL no 
     LABEL "Trading" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.29 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     cod-emitente-ini AT ROW 1.17 COL 21 COLON-ALIGNED HELP
          "C¢digo do Emitente" WIDGET-ID 2
     cod-emitente-fim AT ROW 1.17 COL 45.43 COLON-ALIGNED HELP
          "C¢digo do Emitente" NO-LABEL WIDGET-ID 12
     tg-cliente AT ROW 3.17 COL 7 WIDGET-ID 54
     tg-pessoa-fisica AT ROW 3.17 COL 47.43 WIDGET-ID 64
     tg-fornecedor AT ROW 4.17 COL 7 WIDGET-ID 58
     tg-pessoa-juridica AT ROW 4.17 COL 47.43 WIDGET-ID 66
     tg-ambos AT ROW 5.17 COL 7 WIDGET-ID 60
     tg-estrangeiro AT ROW 5.17 COL 47.43 WIDGET-ID 68
     tg-trading AT ROW 6.17 COL 47.43 WIDGET-ID 70
     bt-ok AT ROW 8.25 COL 3
     bt-cancelar AT ROW 8.25 COL 14
     bt-ajuda AT ROW 8.25 COL 69
     "Natureza" VIEW-AS TEXT
          SIZE 9.14 BY .75 AT ROW 2.33 COL 42.29 WIDGET-ID 74
     "Identifica‡Æo" VIEW-AS TEXT
          SIZE 12.29 BY .75 AT ROW 2.33 COL 3.14 WIDGET-ID 72
     RECT-1 AT ROW 8.04 COL 2
     RECT-2 AT ROW 1.08 COL 2 WIDGET-ID 10
     IMAGE-1 AT ROW 1.17 COL 36 WIDGET-ID 20
     IMAGE-2 AT ROW 1.17 COL 42.29 WIDGET-ID 22
     RECT-5 AT ROW 2.54 COL 2 WIDGET-ID 52
     RECT-6 AT ROW 2.54 COL 41 WIDGET-ID 62
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
         TITLE              = "Filtro"
         HEIGHT             = 8.58
         WIDTH              = 80
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* Filtro */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* Filtro */
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
  IF INPUT FRAME {&FRAME-NAME} cod-emitente-ini > INPUT FRAME {&FRAME-NAME} cod-emitente-fim THEN DO:
     RUN utp/ut-msgs.p (INPUT 'show',
                        INPUT 142,
                        INPUT "Emitente~~Emitente").
     APPLY "ENTRY" TO cod-emitente-ini IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.





   ASSIGN cod-emitente-ini  = INPUT FRAME {&FRAME-NAME} cod-emitente-ini
          cod-emitente-fim  = INPUT FRAME {&FRAME-NAME} cod-emitente-fim
          l-cliente         = INPUT FRAME {&FRAME-NAME} tg-cliente        
          l-fornecedor      = INPUT FRAME {&FRAME-NAME} tg-fornecedor     
          l-ambos           = INPUT FRAME {&FRAME-NAME} tg-ambos          
          l-pessoa-fisica   = INPUT FRAME {&FRAME-NAME} tg-pessoa-fisica  
          l-pessoa-juridica = INPUT FRAME {&FRAME-NAME} tg-pessoa-juridica
          l-estrangeiro     = INPUT FRAME {&FRAME-NAME} tg-estrangeiro    
          l-trading         = INPUT FRAME {&FRAME-NAME} tg-trading.       

    
    apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */

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
  DISPLAY cod-emitente-ini cod-emitente-fim tg-cliente tg-pessoa-fisica 
          tg-fornecedor tg-pessoa-juridica tg-ambos tg-estrangeiro tg-trading 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-1 RECT-2 IMAGE-1 IMAGE-2 RECT-5 RECT-6 cod-emitente-ini 
         cod-emitente-fim tg-cliente tg-pessoa-fisica tg-fornecedor 
         tg-pessoa-juridica tg-ambos tg-estrangeiro tg-trading bt-ok 
         bt-cancelar bt-ajuda 
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
  
  {utp/ut9000.i "YMUT0002A" "2.06.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN cod-emitente-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(cod-emitente-ini)
         cod-emitente-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(cod-emitente-fim).


  ASSIGN tg-cliente        :CHECKED  IN FRAME {&FRAME-NAME} =  l-cliente          
         tg-fornecedor     :CHECKED  IN FRAME {&FRAME-NAME} =  l-fornecedor       
         tg-ambos          :CHECKED  IN FRAME {&FRAME-NAME} =  l-ambos            
         tg-pessoa-fisica  :CHECKED  IN FRAME {&FRAME-NAME} =  l-pessoa-fisica    
         tg-pessoa-juridica:CHECKED  IN FRAME {&FRAME-NAME} =  l-pessoa-juridica  
         tg-estrangeiro    :CHECKED  IN FRAME {&FRAME-NAME} =  l-estrangeiro      
         tg-trading        :CHECKED  IN FRAME {&FRAME-NAME} =  l-trading          .

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


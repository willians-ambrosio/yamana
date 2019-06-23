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
{include/i-prgvrs.i YMUT0001A 2.06.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE NEW GLOBAL SHARED VARIABLE ep-codigo-ini    LIKE es-item-doc-est-natoper.ep-codigo    NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE ep-codigo-fim    LIKE es-item-doc-est-natoper.ep-codigo    NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE serie-docto-ini  LIKE es-item-doc-est-natoper.serie-docto  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE serie-docto-fim  LIKE es-item-doc-est-natoper.serie-docto  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE nro-docto-ini    LIKE es-item-doc-est-natoper.nro-docto    NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE nro-docto-fim    LIKE es-item-doc-est-natoper.nro-docto    NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cod-emitente-ini LIKE es-item-doc-est-natoper.cod-emitente NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE cod-emitente-fim LIKE es-item-doc-est-natoper.cod-emitente NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE dt-emissao-ini   LIKE docum-est.dt-emissao                 NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE dt-emissao-fim   LIKE docum-est.dt-emissao                 NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE dt-trans-ini     LIKE docum-est.dt-trans                   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE dt-trans-fim     LIKE docum-est.dt-trans                   NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 IMAGE-1 IMAGE-2 IMAGE-3 ~
IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 IMAGE-9 IMAGE-10 IMAGE-11 IMAGE-12 ~
serie-docto-ini serie-docto-fim nro-docto-ini nro-docto-fim ~
cod-emitente-ini cod-emitente-fim dt-emissao-ini dt-emissao-fim ~
dt-trans-ini dt-trans-fim bt-ok bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS ep-codigo-ini ep-codigo-fim ~
serie-docto-ini serie-docto-fim nro-docto-ini nro-docto-fim ~
cod-emitente-ini cod-emitente-fim dt-emissao-ini dt-emissao-fim ~
dt-trans-ini dt-trans-fim 

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

DEFINE VARIABLE dt-emissao-fim AS DATE FORMAT "99/99/9999" INITIAL 12/31/2999 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88.

DEFINE VARIABLE dt-emissao-ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/1900 
     LABEL "Data EmissÆo" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88.

DEFINE VARIABLE dt-trans-fim AS DATE FORMAT "99/99/9999" INITIAL 12/31/2999 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88.

DEFINE VARIABLE dt-trans-ini AS DATE FORMAT "99/99/9999" INITIAL 01/01/1900 
     LABEL "Data Transa‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88.

DEFINE VARIABLE ep-codigo-fim AS CHARACTER FORMAT "x(3)" INITIAL "zzz" 
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88.

DEFINE VARIABLE ep-codigo-ini AS CHARACTER FORMAT "x(3)" 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 7.14 BY .88.

DEFINE VARIABLE nro-docto-fim AS CHARACTER FORMAT "x(16)" INITIAL "zzzzzzzzzzzzzzzz" 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .88.

DEFINE VARIABLE nro-docto-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Documento" 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY .88.

DEFINE VARIABLE serie-docto-fim AS CHARACTER FORMAT "x(5)" INITIAL "zzzzz" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88.

DEFINE VARIABLE serie-docto-ini AS CHARACTER FORMAT "x(5)" 
     LABEL "S‚rie" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 77.86 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77.86 BY 6.17
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     ep-codigo-ini AT ROW 1.17 COL 21 COLON-ALIGNED HELP
          "C¢digo da Empresa" WIDGET-ID 4
     ep-codigo-fim AT ROW 1.17 COL 49.43 COLON-ALIGNED HELP
          "C¢digo da Empresa" NO-LABEL WIDGET-ID 14
     serie-docto-ini AT ROW 2.17 COL 21 COLON-ALIGNED HELP
          "S‚rie do documento" WIDGET-ID 8
     serie-docto-fim AT ROW 2.17 COL 49.43 COLON-ALIGNED HELP
          "S‚rie do documento" NO-LABEL WIDGET-ID 18
     nro-docto-ini AT ROW 3.17 COL 21 COLON-ALIGNED HELP
          "N£mero do Documento" WIDGET-ID 6
     nro-docto-fim AT ROW 3.17 COL 49.43 COLON-ALIGNED HELP
          "N£mero do Documento" NO-LABEL WIDGET-ID 16
     cod-emitente-ini AT ROW 4.17 COL 21 COLON-ALIGNED HELP
          "C¢digo do Emitente" WIDGET-ID 2
     cod-emitente-fim AT ROW 4.17 COL 49.43 COLON-ALIGNED HELP
          "C¢digo do Emitente" NO-LABEL WIDGET-ID 12
     dt-emissao-ini AT ROW 5.17 COL 21 COLON-ALIGNED WIDGET-ID 36
     dt-emissao-fim AT ROW 5.17 COL 49.43 COLON-ALIGNED NO-LABEL WIDGET-ID 48
     dt-trans-ini AT ROW 6.17 COL 21 COLON-ALIGNED WIDGET-ID 38
     dt-trans-fim AT ROW 6.17 COL 49.43 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     bt-ok AT ROW 7.67 COL 3
     bt-cancelar AT ROW 7.67 COL 14
     bt-ajuda AT ROW 7.67 COL 69
     RECT-1 AT ROW 7.46 COL 2
     RECT-2 AT ROW 1.08 COL 2 WIDGET-ID 10
     IMAGE-1 AT ROW 1.17 COL 41 WIDGET-ID 20
     IMAGE-2 AT ROW 1.17 COL 47.29 WIDGET-ID 22
     IMAGE-3 AT ROW 2.17 COL 41 WIDGET-ID 24
     IMAGE-4 AT ROW 2.17 COL 47.29 WIDGET-ID 26
     IMAGE-5 AT ROW 3.17 COL 41 WIDGET-ID 28
     IMAGE-6 AT ROW 3.17 COL 47.29 WIDGET-ID 30
     IMAGE-7 AT ROW 4.17 COL 41 WIDGET-ID 32
     IMAGE-8 AT ROW 4.17 COL 47.29 WIDGET-ID 34
     IMAGE-9 AT ROW 5.17 COL 41 WIDGET-ID 40
     IMAGE-10 AT ROW 5.17 COL 47.29 WIDGET-ID 42
     IMAGE-11 AT ROW 6.17 COL 41 WIDGET-ID 46
     IMAGE-12 AT ROW 6.17 COL 47.29 WIDGET-ID 44
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
         HEIGHT             = 8.04
         WIDTH              = 80.14
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
/* SETTINGS FOR FILL-IN ep-codigo-fim IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ep-codigo-ini IN FRAME F-Main
   NO-ENABLE                                                            */
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
  IF INPUT FRAME {&FRAME-NAME} ep-codigo-ini > INPUT FRAME {&FRAME-NAME} ep-codigo-fim THEN DO:
     RUN utp/ut-msgs.p (INPUT 'show',
                        INPUT 142,
                        INPUT "Empresa~~Empresa").
     APPLY "ENTRY" TO ep-codigo-ini IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.

  IF INPUT FRAME {&FRAME-NAME} serie-docto-ini > INPUT FRAME {&FRAME-NAME} serie-docto-fim THEN DO:
     RUN utp/ut-msgs.p (INPUT 'show',
                        INPUT 142,
                        INPUT "S‚rie~~S‚rie").
     APPLY "ENTRY" TO serie-docto-ini IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.

  IF INPUT FRAME {&FRAME-NAME} nro-docto-ini > INPUT FRAME {&FRAME-NAME} nro-docto-fim THEN DO:
     RUN utp/ut-msgs.p (INPUT 'show',
                        INPUT 142,
                        INPUT "Documento~~Documento").
     APPLY "ENTRY" TO nro-docto-ini IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.

  IF INPUT FRAME {&FRAME-NAME} cod-emitente-ini > INPUT FRAME {&FRAME-NAME} cod-emitente-fim THEN DO:
     RUN utp/ut-msgs.p (INPUT 'show',
                        INPUT 142,
                        INPUT "Emitente~~Emitente").
     APPLY "ENTRY" TO cod-emitente-ini IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.

  IF INPUT FRAME {&FRAME-NAME} dt-emissao-ini > INPUT FRAME {&FRAME-NAME} dt-emissao-fim THEN DO:
     RUN utp/ut-msgs.p (INPUT 'show',
                        INPUT 142,
                        INPUT "Data EmissÆo~~Data EmissÆo").
     APPLY "ENTRY" TO dt-emissao-ini IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.

  IF INPUT FRAME {&FRAME-NAME} dt-trans-ini > INPUT FRAME {&FRAME-NAME} dt-trans-fim THEN DO:
     RUN utp/ut-msgs.p (INPUT 'show',
                        INPUT 142,
                        INPUT "Data Transa‡Æo~~Data Transa‡Æo").
     APPLY "ENTRY" TO dt-trans-ini IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.


   ASSIGN ep-codigo-ini    = INPUT FRAME {&FRAME-NAME} ep-codigo-ini   
          ep-codigo-fim    = INPUT FRAME {&FRAME-NAME} ep-codigo-fim   
          serie-docto-ini  = INPUT FRAME {&FRAME-NAME} serie-docto-ini 
          serie-docto-fim  = INPUT FRAME {&FRAME-NAME} serie-docto-fim 
          nro-docto-ini    = INPUT FRAME {&FRAME-NAME} nro-docto-ini   
          nro-docto-fim    = INPUT FRAME {&FRAME-NAME} nro-docto-fim   
          cod-emitente-ini = INPUT FRAME {&FRAME-NAME} cod-emitente-ini
          cod-emitente-fim = INPUT FRAME {&FRAME-NAME} cod-emitente-fim
          dt-emissao-ini   = INPUT FRAME {&FRAME-NAME} dt-emissao-ini 
          dt-emissao-fim   = INPUT FRAME {&FRAME-NAME} dt-emissao-fim 
          dt-trans-ini     = INPUT FRAME {&FRAME-NAME} dt-trans-ini   
          dt-trans-fim     = INPUT FRAME {&FRAME-NAME} dt-trans-fim.  
    
    
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
  DISPLAY ep-codigo-ini ep-codigo-fim serie-docto-ini serie-docto-fim 
          nro-docto-ini nro-docto-fim cod-emitente-ini cod-emitente-fim 
          dt-emissao-ini dt-emissao-fim dt-trans-ini dt-trans-fim 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-1 RECT-2 IMAGE-1 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 
         IMAGE-8 IMAGE-9 IMAGE-10 IMAGE-11 IMAGE-12 serie-docto-ini 
         serie-docto-fim nro-docto-ini nro-docto-fim cod-emitente-ini 
         cod-emitente-fim dt-emissao-ini dt-emissao-fim dt-trans-ini 
         dt-trans-fim bt-ok bt-cancelar bt-ajuda 
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
  
  {utp/ut9000.i "YMUT0001A" "2.06.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN ep-codigo-ini   :SCREEN-VALUE IN FRAME {&FRAME-NAME} = ep-codigo-ini   
         ep-codigo-fim   :SCREEN-VALUE IN FRAME {&FRAME-NAME} = ep-codigo-fim   
         serie-docto-ini :SCREEN-VALUE IN FRAME {&FRAME-NAME} = serie-docto-ini 
         serie-docto-fim :SCREEN-VALUE IN FRAME {&FRAME-NAME} = serie-docto-fim 
         nro-docto-ini   :SCREEN-VALUE IN FRAME {&FRAME-NAME} = nro-docto-ini   
         nro-docto-fim   :SCREEN-VALUE IN FRAME {&FRAME-NAME} = nro-docto-fim   
         cod-emitente-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(cod-emitente-ini)
         cod-emitente-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(cod-emitente-fim)
         dt-emissao-ini  :SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(dt-emissao-ini) 
         dt-emissao-fim  :SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(dt-emissao-fim) 
         dt-trans-ini    :SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(dt-trans-ini)   
         dt-trans-fim    :SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(dt-trans-fim).


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


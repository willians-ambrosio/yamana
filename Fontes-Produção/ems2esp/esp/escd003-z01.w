&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          mgadm            PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/*{src/adm2/widgetprto.i}*/

/*{include\variaveis.i} */

DEFINE BUFFER bf-es-depto FOR es-depto.

DEFINE NEW GLOBAL SHARED VARIABLE gsvr-transacao AS ROWID.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME browser-pesquisa

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES es-depto

/* Definitions for BROWSE browser-pesquisa                              */
&Scoped-define FIELDS-IN-QUERY-browser-pesquisa es-depto.codigo es-depto.descricao   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browser-pesquisa   
&Scoped-define SELF-NAME browser-pesquisa
&Scoped-define QUERY-STRING-browser-pesquisa FOR EACH es-depto NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-browser-pesquisa OPEN QUERY {&SELF-NAME} FOR EACH es-depto NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-browser-pesquisa es-depto
&Scoped-define FIRST-TABLE-IN-QUERY-browser-pesquisa es-depto


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-browser-pesquisa}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-27 IMAGE-1 RECT-28 RECT-34 btn-carga ~
i-codigo-ini i-codigo-fim browser-pesquisa btn-ok btn-cancelar 
&Scoped-Define DISPLAYED-OBJECTS i-codigo-ini i-codigo-fim 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancelar AUTO-END-KEY 
     IMAGE-UP FILE "img/im-cancel.bmp":U
     LABEL "Cancelar" 
     SIZE 10 BY 1.13 TOOLTIP "Cancelar".

DEFINE BUTTON btn-carga 
     IMAGE-UP FILE "img/im-sav.bmp":U
     LABEL "Ok" 
     SIZE 10 BY 1.13 TOOLTIP "Confirmar".

DEFINE BUTTON btn-ok 
     IMAGE-UP FILE "img/im-check.bmp":U
     LABEL "Ok" 
     SIZE 10 BY 1.13 TOOLTIP "Confirmar".

DEFINE VARIABLE i-codigo-fim AS INTEGER FORMAT ">>>>9":U INITIAL 99999 
     VIEW-AS FILL-IN 
     SIZE 7 BY .92 NO-UNDO.

DEFINE VARIABLE i-codigo-ini AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Cod. Departamento" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .92 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "img/im-faixa.bmp":U
     SIZE 6.43 BY .96.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 1.71.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 9.33.

DEFINE RECTANGLE RECT-34
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 1.71.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browser-pesquisa FOR 
      es-depto SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browser-pesquisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browser-pesquisa wWin _FREEFORM
  QUERY browser-pesquisa NO-LOCK DISPLAY
      es-depto.codigo
es-depto.descricao
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 86 BY 8.58
         TITLE "Pesquisa Departamento" EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     btn-carga AT ROW 1.46 COL 79.14
     i-codigo-ini AT ROW 1.63 COL 29.72 COLON-ALIGNED
     i-codigo-fim AT ROW 1.63 COL 43.72 COLON-ALIGNED NO-LABEL
     browser-pesquisa AT ROW 3.58 COL 3
     btn-ok AT ROW 12.83 COL 30
     btn-cancelar AT ROW 12.88 COL 53.14
     RECT-27 AT ROW 1.29 COL 2
     IMAGE-1 AT ROW 1.63 COL 39.29
     RECT-28 AT ROW 3.13 COL 2
     RECT-34 AT ROW 12.54 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.2 ROW 1
         SIZE 89.8 BY 15.57.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Pesquisa"
         HEIGHT             = 13.67
         WIDTH              = 90
         MAX-HEIGHT         = 35.71
         MAX-WIDTH          = 256
         VIRTUAL-HEIGHT     = 35.71
         VIRTUAL-WIDTH      = 256
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* BROWSE-TAB browser-pesquisa i-codigo-fim fMain */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browser-pesquisa
/* Query rebuild information for BROWSE browser-pesquisa
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH es-depto NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE browser-pesquisa */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Pesquisa */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Pesquisa */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browser-pesquisa
&Scoped-define SELF-NAME browser-pesquisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browser-pesquisa wWin
ON MOUSE-SELECT-DBLCLICK OF browser-pesquisa IN FRAME fMain /* Pesquisa Departamento */
DO:
    APPLY "choose" TO btn-ok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancelar wWin
ON CHOOSE OF btn-cancelar IN FRAME fMain /* Cancelar */
DO:
    ASSIGN gsvr-transacao = ?.
    APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-carga
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-carga wWin
ON CHOOSE OF btn-carga IN FRAME fMain /* Ok */
DO:
  CLOSE QUERY browser-pesquisa.
  OPEN QUERY browser-pesquisa 
    FOR EACH es-depto NO-LOCK
      WHERE es-depto.codigo >= INT(i-codigo-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME})
      AND   es-depto.codigo <= INT(i-codigo-fim:SCREEN-VALUE IN FRAME {&FRAME-NAME}) .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-ok wWin
ON CHOOSE OF btn-ok IN FRAME fMain /* Ok */
DO:

  FIND FIRST es-depto NO-LOCK 
    WHERE es-depto.codigo = INT(es-depto.codigo:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) NO-ERROR.
  IF AVAIL es-depto THEN DO:
      MESSAGE "1"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

    ASSIGN gsvr-transacao = ROWID(es-depto). 
    APPLY "GO" TO FRAME {&FRAME-NAME}.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
  END.
  ELSE DO:

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY i-codigo-ini i-codigo-fim 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-27 IMAGE-1 RECT-28 RECT-34 btn-carga i-codigo-ini i-codigo-fim 
         browser-pesquisa btn-ok btn-cancelar 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


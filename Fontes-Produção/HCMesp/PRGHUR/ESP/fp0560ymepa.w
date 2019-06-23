&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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
{utp/ut-glob.i}

DEF SHARED VAR v_cdn_estab_fp0560ymep    AS CHARACTER NO-UNDO.
def shared var v_prog_chamador_fp0560ymep  as CHARACTER no-undo.
def var v_data_corrente as DATE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-margin-plr-corp bt-hist-margin-corp ~
fi-result-corp bt-hist-result-corp fi-margin-corp-prov ~
bt-hist-margin-corp-prov fi-result-corp-prov bt-hist-result-corp-prov ~
fi-margin-plr-nao-corp bt-hist-margin-nao-corp fi-result-nao-corp ~
bt-hist-result-nao-corp fi-margin-nao-corp-prov ~
bt-hist-margin-nao-corp-prov fi-result-nao-corp-prov ~
bt-hist-result-nao-corp-prov bt-ok bt-cancelar rt-button RECT-20 RECT-21 ~
RECT-22 RECT-23 RECT-24 RECT-25 
&Scoped-Define DISPLAYED-OBJECTS fi-margin-plr-corp fi-result-corp ~
fi-margin-corp-prov fi-result-corp-prov fi-margin-plr-nao-corp ~
fi-result-nao-corp fi-margin-nao-corp-prov fi-result-nao-corp-prov 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn_busca_hora W-Win 
FUNCTION fn_busca_hora RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancelar 
     LABEL "&Cancelar" 
     SIZE 13 BY 1.

DEFINE BUTTON bt-hist-margin-corp 
     LABEL "Hist¢rico" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-hist-margin-corp-prov 
     LABEL "Hist¢rico" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-hist-margin-nao-corp 
     LABEL "Hist¢rico" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-hist-margin-nao-corp-prov 
     LABEL "Hist¢rico" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-hist-result-corp 
     LABEL "Hist¢rico" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-hist-result-corp-prov 
     LABEL "Hist¢rico" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-hist-result-nao-corp 
     LABEL "Hist¢rico" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-hist-result-nao-corp-prov 
     LABEL "Hist¢rico" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-ok 
     LABEL "&OK" 
     SIZE 13 BY 1.

DEFINE VARIABLE fi-margin-corp-prov AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "Contribution Margin" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-margin-nao-corp-prov AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "Contribution Margin" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-margin-plr-corp AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "Contribution Margin" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-margin-plr-nao-corp AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "Contribution Margin" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-result-corp AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "Result" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-result-corp-prov AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "Result" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-result-nao-corp AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "Result" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-result-nao-corp-prov AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "Result" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 2.75.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 2.75.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 4.5.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 2.75.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 2.75.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 4.5.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 78 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-margin-plr-corp AT ROW 3.29 COL 19 COLON-ALIGNED WIDGET-ID 8
     bt-hist-margin-corp AT ROW 3.25 COL 29.43 WIDGET-ID 12
     fi-result-corp AT ROW 4.29 COL 19 COLON-ALIGNED WIDGET-ID 10
     bt-hist-result-corp AT ROW 4.25 COL 29.43 WIDGET-ID 14
     fi-margin-corp-prov AT ROW 3.29 COL 57.57 COLON-ALIGNED WIDGET-ID 20
     bt-hist-margin-corp-prov AT ROW 3.25 COL 68 WIDGET-ID 16
     fi-result-corp-prov AT ROW 4.29 COL 57.57 COLON-ALIGNED WIDGET-ID 22
     bt-hist-result-corp-prov AT ROW 4.25 COL 68 WIDGET-ID 18
     fi-margin-plr-nao-corp AT ROW 8.38 COL 19 COLON-ALIGNED WIDGET-ID 46
     bt-hist-margin-nao-corp AT ROW 8.33 COL 29.43 WIDGET-ID 36
     fi-result-nao-corp AT ROW 9.38 COL 19 COLON-ALIGNED WIDGET-ID 48
     bt-hist-result-nao-corp AT ROW 9.33 COL 29.43 WIDGET-ID 40
     fi-margin-nao-corp-prov AT ROW 8.38 COL 57.57 COLON-ALIGNED WIDGET-ID 44
     bt-hist-margin-nao-corp-prov AT ROW 8.33 COL 68 WIDGET-ID 38
     fi-result-nao-corp-prov AT ROW 9.38 COL 57.57 COLON-ALIGNED WIDGET-ID 50
     bt-hist-result-nao-corp-prov AT ROW 9.33 COL 68 WIDGET-ID 42
     bt-ok AT ROW 11.79 COL 3.43 WIDGET-ID 4
     bt-cancelar AT ROW 11.79 COL 17.43 WIDGET-ID 6
     "PLR" VIEW-AS TEXT
          SIZE 3 BY .67 AT ROW 7.58 COL 5 WIDGET-ID 62
     "Provis∆o PLR" VIEW-AS TEXT
          SIZE 11 BY .67 AT ROW 7.58 COL 42.72 WIDGET-ID 60
     "N∆o Corporativo" VIEW-AS TEXT
          SIZE 12 BY .67 AT ROW 6.46 COL 3.14 WIDGET-ID 58
          FONT 0
     "Corporativo" VIEW-AS TEXT
          SIZE 8.57 BY .67 AT ROW 1.38 COL 3.14 WIDGET-ID 34
          FONT 0
     "Provis∆o PLR" VIEW-AS TEXT
          SIZE 11 BY .67 AT ROW 2.5 COL 42.72 WIDGET-ID 32
     "PLR" VIEW-AS TEXT
          SIZE 3 BY .67 AT ROW 2.5 COL 5 WIDGET-ID 30
     rt-button AT ROW 11.58 COL 2.14 WIDGET-ID 2
     RECT-20 AT ROW 2.92 COL 3.43 WIDGET-ID 24
     RECT-21 AT ROW 2.92 COL 41.86 WIDGET-ID 26
     RECT-22 AT ROW 1.75 COL 2.14 WIDGET-ID 28
     RECT-23 AT ROW 8 COL 3.43 WIDGET-ID 52
     RECT-24 AT ROW 8 COL 41.86 WIDGET-ID 54
     RECT-25 AT ROW 6.83 COL 2.14 WIDGET-ID 56
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80.14 BY 12.25 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Indicadores PLR"
         HEIGHT             = 12.25
         WIDTH              = 80.14
         MAX-HEIGHT         = 39.04
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 39.04
         VIRTUAL-WIDTH      = 182.86
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Custom                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Indicadores PLR */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF VALID-HANDLE(h-pai) THEN DO:
      ASSIGN h-pai:SENSITIVE = YES.
  END.
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Indicadores PLR */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  IF VALID-HANDLE(h-pai) THEN DO:
      ASSIGN h-pai:SENSITIVE = YES.
  END.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar W-Win
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
    IF VALID-HANDLE(h-pai) THEN DO:
        ASSIGN h-pai:SENSITIVE = YES.
    END.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-hist-margin-corp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-hist-margin-corp W-Win
ON CHOOSE OF bt-hist-margin-corp IN FRAME F-Main /* Hist¢rico */
DO:
  RUN prghur/esp/fp0560ymepb1.w(INPUT YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-hist-margin-corp-prov
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-hist-margin-corp-prov W-Win
ON CHOOSE OF bt-hist-margin-corp-prov IN FRAME F-Main /* Hist¢rico */
DO:
  RUN prghur/esp/fp0560ymepb2.w(INPUT YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-hist-margin-nao-corp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-hist-margin-nao-corp W-Win
ON CHOOSE OF bt-hist-margin-nao-corp IN FRAME F-Main /* Hist¢rico */
DO:
  RUN prghur/esp/fp0560ymepb1.w(INPUT NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-hist-margin-nao-corp-prov
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-hist-margin-nao-corp-prov W-Win
ON CHOOSE OF bt-hist-margin-nao-corp-prov IN FRAME F-Main /* Hist¢rico */
DO:
  RUN prghur/esp/fp0560ymepb2.w(INPUT NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-hist-result-corp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-hist-result-corp W-Win
ON CHOOSE OF bt-hist-result-corp IN FRAME F-Main /* Hist¢rico */
DO:
  RUN prghur/esp/fp0560ymepb3.w(INPUT YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-hist-result-corp-prov
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-hist-result-corp-prov W-Win
ON CHOOSE OF bt-hist-result-corp-prov IN FRAME F-Main /* Hist¢rico */
DO:
  RUN prghur/esp/fp0560ymepb4.w(INPUT YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-hist-result-nao-corp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-hist-result-nao-corp W-Win
ON CHOOSE OF bt-hist-result-nao-corp IN FRAME F-Main /* Hist¢rico */
DO:
  RUN prghur/esp/fp0560ymepb3.w(INPUT NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-hist-result-nao-corp-prov
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-hist-result-nao-corp-prov W-Win
ON CHOOSE OF bt-hist-result-nao-corp-prov IN FRAME F-Main /* Hist¢rico */
DO:
  RUN prghur/esp/fp0560ymepb4.w(INPUT NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok W-Win
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
  DO ON ERROR UNDO, LEAVE:
      RUN pi-salvar.
      IF RETURN-VALUE = "OK":U THEN DO:
          IF VALID-HANDLE(h-pai) THEN DO:
              ASSIGN h-pai:SENSITIVE = YES.
          END.
          APPLY "Close":U TO THIS-PROCEDURE.
      END.                                  
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY fi-margin-plr-corp fi-result-corp fi-margin-corp-prov 
          fi-result-corp-prov fi-margin-plr-nao-corp fi-result-nao-corp 
          fi-margin-nao-corp-prov fi-result-nao-corp-prov 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE fi-margin-plr-corp bt-hist-margin-corp fi-result-corp 
         bt-hist-result-corp fi-margin-corp-prov bt-hist-margin-corp-prov 
         fi-result-corp-prov bt-hist-result-corp-prov fi-margin-plr-nao-corp 
         bt-hist-margin-nao-corp fi-result-nao-corp bt-hist-result-nao-corp 
         fi-margin-nao-corp-prov bt-hist-margin-nao-corp-prov 
         fi-result-nao-corp-prov bt-hist-result-nao-corp-prov bt-ok bt-cancelar 
         rt-button RECT-20 RECT-21 RECT-22 RECT-23 RECT-24 RECT-25 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable W-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  ASSIGN h-pai = SOURCE-PROCEDURE
         h-pai = h-pai:CURRENT-WINDOW.

  IF NOT VALID-HANDLE(h-pai) THEN DO:
      ASSIGN h-pai = SESSION:FIRST-CHILD
             h-pai = h-pai:NEXT-SIBLING.
  END.

  IF VALID-HANDLE(h-pai) THEN DO:
      ASSIGN h-pai:SENSITIVE = NO.
  END.

  /* Code placed here will execute AFTER standard behavior.    */

  FIND FIRST rh_estab
       WHERE rh_estab.cdn_estab = v_cdn_estab_fp0560ymep NO-LOCK NO-ERROR.

  IF v_prog_chamador_fp0560ymep = 'prghur/fpp/fp0560.w':U THEN DO:
      ASSIGN fi-margin-plr-corp:SENSITIVE IN FRAME {&FRAME-NAME}  = YES
             fi-margin-plr-nao-corp:SENSITIVE IN FRAME {&FRAME-NAME}  = YES
             fi-margin-corp-prov:SENSITIVE IN FRAME {&FRAME-NAME}  = YES
             fi-margin-nao-corp-prov:SENSITIVE IN FRAME {&FRAME-NAME}  = YES
             fi-result-corp:SENSITIVE IN FRAME {&FRAME-NAME}  = YES
             fi-result-nao-corp:SENSITIVE IN FRAME {&FRAME-NAME}  = YES
             fi-result-corp-prov:SENSITIVE IN FRAME {&FRAME-NAME}  = YES
             fi-result-nao-corp-prov:SENSITIVE IN FRAME {&FRAME-NAME}  = YES.
                
  END.
  ELSE DO:
      ASSIGN fi-margin-plr-corp:SENSITIVE IN FRAME {&FRAME-NAME}  = NO
             fi-margin-plr-nao-corp:SENSITIVE IN FRAME {&FRAME-NAME}  = NO
             fi-margin-corp-prov:SENSITIVE IN FRAME {&FRAME-NAME}  = NO
             fi-margin-nao-corp-prov:SENSITIVE IN FRAME {&FRAME-NAME}  = NO
             fi-result-corp:SENSITIVE IN FRAME {&FRAME-NAME}  = NO
             fi-result-nao-corp:SENSITIVE IN FRAME {&FRAME-NAME}  = NO
             fi-result-corp-prov:SENSITIVE IN FRAME {&FRAME-NAME}  = NO
             fi-result-nao-corp-prov:SENSITIVE IN FRAME {&FRAME-NAME}  = NO.
  END.

  FIND FIRST tip_est_contrib_margin_pag
       WHERE tip_est_contrib_margin_pag.cdn_empresa = rh_estab.cdn_empresa
         AND tip_est_contrib_margin_pag.cdn_estab = v_cdn_estab_fp0560ymep
         AND tip_est_contrib_margin_pag.LOG_corporativo = YES
         AND tip_est_contrib_margin_pag.dt_termino = 12/31/9999
             NO-LOCK NO-ERROR.
  IF AVAIL tip_est_contrib_margin_pag THEN DO: /*atualiza o registro da func_indiv_factor*/
      ASSIGN fi-margin-plr-corp:SCREEN-VALUE = String(tip_est_contrib_margin_pag.perc_margem).
  END.                                                                     
  ELSE DO:
      ASSIGN fi-margin-plr-corp:SCREEN-VALUE = "".
  END.

  FIND FIRST tip_est_contrib_margin_pag
       WHERE tip_est_contrib_margin_pag.cdn_empresa = rh_estab.cdn_empresa
         AND tip_est_contrib_margin_pag.cdn_estab = v_cdn_estab_fp0560ymep
         AND tip_est_contrib_margin_pag.LOG_corporativo = NO
         AND tip_est_contrib_margin_pag.dt_termino = 12/31/9999
             NO-LOCK NO-ERROR.
  IF AVAIL tip_est_contrib_margin_pag THEN DO: /*atualiza o registro da func_indiv_factor*/
      ASSIGN fi-margin-plr-nao-corp:SCREEN-VALUE = String(tip_est_contrib_margin_pag.perc_margem).
  END.                                                                     
  ELSE DO:
      ASSIGN fi-margin-plr-nao-corp:SCREEN-VALUE = "".
  END.

  FIND FIRST tip_est_contrib_margin_prov
       WHERE tip_est_contrib_margin_prov.cdn_empresa = rh_estab.cdn_empresa
         AND tip_est_contrib_margin_prov.cdn_estab = v_cdn_estab_fp0560ymep
         AND tip_est_contrib_margin_prov.LOG_corporativo = YES
         AND tip_est_contrib_margin_prov.dt_termino = 12/31/9999
             NO-LOCK NO-ERROR.
  IF AVAIL tip_est_contrib_margin_prov THEN DO: /*atualiza o registro da func_indiv_factor*/
      ASSIGN fi-margin-corp-prov:SCREEN-VALUE = String(tip_est_contrib_margin_prov.perc_margem).
  END.                                                                     
  ELSE DO:
      ASSIGN fi-margin-corp-prov:SCREEN-VALUE = "".
  END.

  FIND FIRST tip_est_contrib_margin_prov
       WHERE tip_est_contrib_margin_prov.cdn_empresa = rh_estab.cdn_empresa
         AND tip_est_contrib_margin_prov.cdn_estab = v_cdn_estab_fp0560ymep
         AND tip_est_contrib_margin_prov.LOG_corporativo = NO
         AND tip_est_contrib_margin_prov.dt_termino = 12/31/9999
             NO-LOCK NO-ERROR.
  IF AVAIL tip_est_contrib_margin_prov THEN DO: /*atualiza o registro da func_indiv_factor*/
      ASSIGN fi-margin-nao-corp-prov:SCREEN-VALUE = String(tip_est_contrib_margin_prov.perc_margem).
  END.                                                                     
  ELSE DO:
      ASSIGN fi-margin-nao-corp-prov:SCREEN-VALUE = "".
  END.

  FIND FIRST tip_est_targets_pag
       WHERE tip_est_targets_pag.cdn_empresa = rh_estab.cdn_empresa
         AND tip_est_targets_pag.cdn_estab = v_cdn_estab_fp0560ymep
         AND tip_est_targets_pag.LOG_corporativo = YES
         AND tip_est_targets_pag.dt_termino = 12/31/9999
             NO-LOCK NO-ERROR.
  IF AVAIL tip_est_targets_pag THEN DO: /*atualiza o registro da func_indiv_factor*/
      ASSIGN fi-result-corp:SCREEN-VALUE = String(tip_est_targets_pag.RESULT).
  END.                                                                     
  ELSE DO:
      ASSIGN fi-result-corp:SCREEN-VALUE = "".
  END.

  FIND FIRST tip_est_targets_pag
       WHERE tip_est_targets_pag.cdn_empresa = rh_estab.cdn_empresa
         AND tip_est_targets_pag.cdn_estab = v_cdn_estab_fp0560ymep
         AND tip_est_targets_pag.LOG_corporativo = NO
         AND tip_est_targets_pag.dt_termino = 12/31/9999
             NO-LOCK NO-ERROR.
  IF AVAIL tip_est_targets_pag THEN DO: /*atualiza o registro da func_indiv_factor*/
      ASSIGN fi-result-nao-corp:SCREEN-VALUE = String(tip_est_targets_pag.RESULT).
  END.                                                                     
  ELSE DO:
      ASSIGN fi-result-nao-corp:SCREEN-VALUE = "".
  END.

  FIND FIRST tip_est_targets_prov
       WHERE tip_est_targets_prov.cdn_empresa = rh_estab.cdn_empresa
         AND tip_est_targets_prov.cdn_estab = v_cdn_estab_fp0560ymep
         AND tip_est_targets_prov.LOG_corporativo = YES
         AND tip_est_targets_prov.dt_termino = 12/31/9999
             NO-LOCK NO-ERROR.
  IF AVAIL tip_est_targets_prov THEN DO: /*atualiza o registro da func_indiv_factor*/
      ASSIGN fi-result-corp-prov:SCREEN-VALUE = String(tip_est_targets_prov.RESULT).
  END.                                                                     
  ELSE DO:
      ASSIGN fi-result-corp-prov:SCREEN-VALUE = "".
  END.

  FIND FIRST tip_est_targets_prov
       WHERE tip_est_targets_prov.cdn_empresa = rh_estab.cdn_empresa
         AND tip_est_targets_prov.cdn_estab = v_cdn_estab_fp0560ymep
         AND tip_est_targets_prov.LOG_corporativo = NO
         AND tip_est_targets_prov.dt_termino = 12/31/9999
             NO-LOCK NO-ERROR.
  IF AVAIL tip_est_targets_prov THEN DO: /*atualiza o registro da func_indiv_factor*/
      ASSIGN fi-result-nao-corp-prov:SCREEN-VALUE = String(tip_est_targets_prov.RESULT).
  END.                                                                     
  ELSE DO:
      ASSIGN fi-result-nao-corp-prov:SCREEN-VALUE = "".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-est-contrib-margin-pag W-Win 
PROCEDURE pi-cria-est-contrib-margin-pag :
/*------------------------------------------------------------------------------
  Purpose:     pi-cria-est-contrib-margin-pag
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE l_existe_contrib_margin AS LOGICAL INITIAL NO NO-UNDO.

    /*cria a margem corporativa*/
    
    FIND FIRST tip_est_contrib_margin_pag
         WHERE tip_est_contrib_margin_pag.cdn_empresa = rh_estab.cdn_empresa
           AND tip_est_contrib_margin_pag.cdn_estab = v_cdn_estab_fp0560ymep
           AND tip_est_contrib_margin_pag.LOG_corporativo = YES
           AND tip_est_contrib_margin_pag.dt_inicio = v_data_corrente
                   EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL tip_est_contrib_margin_pag THEN DO: /*atualiza o registro da tip_est_contrib_margin_pag*/
        FOR EACH  tip_contrib_margin
            WHERE tip_contrib_margin.cdn_empresa      = rh_estab.cdn_empresa 
              AND tip_contrib_margin.cdn_estab = v_cdn_estab_fp0560ymep
              AND tip_contrib_margin.LOG_corporativo = YES
              AND tip_contrib_margin.perc_ini <= dec(fi-margin-plr-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME})
              AND tip_contrib_margin.perc_fim >= dec(fi-margin-plr-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME})
              AND tip_contrib_margin.dt_inicio       <= v_data_corrente BREAK BY tip_contrib_margin.dt_inicio : 
            IF LAST(tip_contrib_margin.dt_inicio) THEN DO:
                IF tip_est_contrib_margin_pag.perc_margem <> dec(fi-margin-plr-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN DO:
                    RUN pi-cria-hist-est-contrib-margin-pag(INPUT YES, INPUT dec(fi-margin-plr-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME})).

                    ASSIGN tip_est_contrib_margin_pag.perc_margem = dec(fi-margin-plr-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                               tip_est_contrib_margin_pag.cod_usuario = v_cod_usuar_corren
                               tip_est_contrib_margin_pag.dt_inclusao = TODAY
                               tip_est_contrib_margin_pag.hr_inclusao = fn_busca_hora().
                END.
                ASSIGN l_existe_contrib_margin = YES.
            END.
        END.
        /*N∆o existe nenhum tip_contrib_margin anterior a data do mes corrente, sendo assim n∆o salva*/
        IF l_existe_contrib_margin = NO THEN DO:
            RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar as informaá‰es de PLR" 
                                               + "~~" + "N∆o existe registro Contribution Margin com o c¢digo " + fi-margin-plr-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME}  + " anterior a " + String(v_data_corrente)).
            ASSIGN fi-margin-plr-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(tip_est_contrib_margin_pag.perc_margem).
            RETURN ERROR.
        END.
    END.
    ELSE DO: /*cria o registro da tip_est_contrib_margin_pag*/
        FOR EACH  tip_contrib_margin
            WHERE tip_contrib_margin.perc_ini <= dec(fi-margin-plr-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME})
              AND tip_contrib_margin.perc_fim >= dec(fi-margin-plr-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME})
              AND tip_contrib_margin.cdn_empresa      = rh_estab.cdn_empresa 
              AND tip_contrib_margin.cdn_estab = v_cdn_estab_fp0560ymep
              AND tip_contrib_margin.LOG_corporativo = YES
              AND tip_contrib_margin.dt_inicio       <= v_data_corrente BREAK BY tip_contrib_margin.dt_inicio : 
            IF LAST(tip_contrib_margin.dt_inicio) THEN DO:

                RUN pi-cria-hist-est-contrib-margin-pag(INPUT YES, INPUT dec(fi-margin-plr-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME})).

                CREATE tip_est_contrib_margin_pag.
                ASSIGN tip_est_contrib_margin_pag.cdn_empresa = rh_estab.cdn_empresa
                       tip_est_contrib_margin_pag.cdn_estab = v_cdn_estab_fp0560ymep
                       tip_est_contrib_margin_pag.LOG_corporativo = YES
                       tip_est_contrib_margin_pag.perc_margem = dec(fi-margin-plr-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                       tip_est_contrib_margin_pag.perc_ini = tip_contrib_margin.perc_ini
                       tip_est_contrib_margin_pag.perc_fim = tip_contrib_margin.perc_fim
                       tip_est_contrib_margin_pag.cod_usuario = v_cod_usuar_corren
                       tip_est_contrib_margin_pag.dt_inicio = v_data_corrente
                       tip_est_contrib_margin_pag.dt_termino = 12/31/9999
                       tip_est_contrib_margin_pag.dt_inclusao = TODAY
                       tip_est_contrib_margin_pag.hr_inclusao = fn_busca_hora()
                       l_existe_contrib_margin = YES.

                FOR LAST tip_est_contrib_margin_pag EXCLUSIVE-LOCK
                    WHERE tip_est_contrib_margin_pag.cdn_empresa = rh_estab.cdn_empresa
                      and tip_est_contrib_margin_pag.cdn_estab = v_cdn_estab_fp0560ymep
                      and tip_est_contrib_margin_pag.LOG_corporativo = YES
                      AND tip_est_contrib_margin_pag.dt_inicio <> v_data_corrente
                      AND tip_est_contrib_margin_pag.dt_inicio < v_data_corrente
                       BY tip_est_contrib_margin_pag.dt_inicio :
                    
                    ASSIGN tip_est_contrib_margin_pag.dt_termino = v_data_corrente - 1.
                END.
            END.
        END.
        IF l_existe_contrib_margin = NO THEN DO:
            RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar as informaá‰es de PLR" 
                                               + "~~" + "N∆o existe registro Contribution Margin com o c¢digo " + fi-margin-plr-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME} + " anterior a " + String(v_data_corrente)).
            ASSIGN fi-margin-plr-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
            RETURN ERROR.
        END.
    END. 

/*cria a margem N«O corporativa*/
    FIND FIRST tip_est_contrib_margin_pag
         WHERE tip_est_contrib_margin_pag.cdn_empresa = rh_estab.cdn_empresa
           AND tip_est_contrib_margin_pag.cdn_estab = v_cdn_estab_fp0560ymep
           AND tip_est_contrib_margin_pag.LOG_corporativo = NO
           AND tip_est_contrib_margin_pag.dt_inicio = v_data_corrente
                   EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL tip_est_contrib_margin_pag THEN DO: /*atualiza o registro da tip_est_contrib_margin_pag*/
    
        FOR EACH  tip_contrib_margin
            WHERE tip_contrib_margin.cdn_empresa      = rh_estab.cdn_empresa 
              AND tip_contrib_margin.cdn_estab = v_cdn_estab_fp0560ymep
              AND tip_contrib_margin.LOG_corporativo = NO
              AND tip_contrib_margin.perc_ini <= dec(fi-margin-plr-nao-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME})
              AND tip_contrib_margin.perc_fim >= dec(fi-margin-plr-nao-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME})
              AND tip_contrib_margin.dt_inicio       <= v_data_corrente BREAK BY tip_contrib_margin.dt_inicio : 
            IF LAST(tip_contrib_margin.dt_inicio) THEN DO:
            
                IF(tip_est_contrib_margin_pag.perc_margem <> dec(fi-margin-plr-nao-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME})) THEN DO:
                    RUN pi-cria-hist-est-contrib-margin-pag(INPUT NO, dec(fi-margin-plr-nao-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME})).

                    ASSIGN tip_est_contrib_margin_pag.perc_margem = dec(fi-margin-plr-nao-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                               tip_est_contrib_margin_pag.cod_usuario = v_cod_usuar_corren
                               tip_est_contrib_margin_pag.dt_inclusao = TODAY
                               tip_est_contrib_margin_pag.hr_inclusao = fn_busca_hora().
                END.                                                                
                ASSIGN l_existe_contrib_margin = YES.
            END.
        END.
        /*N∆o existe nenhum tip_contrib_margin anterior a data do mes corrente, sendo assim n∆o salva*/
        IF l_existe_contrib_margin = NO THEN DO:
            RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar as informaá‰es de PLR" 
                                               + "~~" + "N∆o existe registro Contribution Margin com o c¢digo " + fi-margin-plr-nao-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME}  + " anterior a " + String(v_data_corrente)).
            ASSIGN fi-margin-plr-nao-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(tip_est_contrib_margin_pag.perc_margem).
            RETURN ERROR.
        END.
    END.
    ELSE DO: /*cria o registro da tip_est_contrib_margin_pag*/
        FOR EACH  tip_contrib_margin
            WHERE tip_contrib_margin.perc_ini <= dec(fi-margin-plr-nao-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME})
              AND tip_contrib_margin.perc_fim >= dec(fi-margin-plr-nao-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME})
              AND tip_contrib_margin.cdn_empresa      = rh_estab.cdn_empresa 
              AND tip_contrib_margin.cdn_estab = v_cdn_estab_fp0560ymep
              AND tip_contrib_margin.LOG_corporativo = NO
              AND tip_contrib_margin.dt_inicio       <= v_data_corrente BREAK BY tip_contrib_margin.dt_inicio : 
            IF LAST(tip_contrib_margin.dt_inicio) THEN DO:

                RUN pi-cria-hist-est-contrib-margin-pag(INPUT NO, dec(fi-margin-plr-nao-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME})).

                CREATE tip_est_contrib_margin_pag.
                ASSIGN tip_est_contrib_margin_pag.cdn_empresa = rh_estab.cdn_empresa
                           tip_est_contrib_margin_pag.cdn_estab = v_cdn_estab_fp0560ymep
                           tip_est_contrib_margin_pag.LOG_corporativo = NO
                           tip_est_contrib_margin_pag.perc_margem = dec(fi-margin-plr-nao-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                           tip_est_contrib_margin_pag.perc_ini = tip_contrib_margin.perc_ini
                           tip_est_contrib_margin_pag.perc_fim = tip_contrib_margin.perc_fim
                           tip_est_contrib_margin_pag.cod_usuario = v_cod_usuar_corren
                           tip_est_contrib_margin_pag.dt_inicio = v_data_corrente
                           tip_est_contrib_margin_pag.dt_termino = 12/31/9999
                           tip_est_contrib_margin_pag.dt_inclusao = TODAY
                           tip_est_contrib_margin_pag.hr_inclusao = fn_busca_hora()
                           l_existe_contrib_margin = YES.

                FOR LAST tip_est_contrib_margin_pag EXCLUSIVE-LOCK
                    WHERE tip_est_contrib_margin_pag.cdn_empresa = rh_estab.cdn_empresa
                      and tip_est_contrib_margin_pag.cdn_estab = v_cdn_estab_fp0560ymep
                      and tip_est_contrib_margin_pag.LOG_corporativo = NO
                      AND tip_est_contrib_margin_pag.dt_inicio <> v_data_corrente
                      AND tip_est_contrib_margin_pag.dt_inicio < v_data_corrente
                       BY tip_est_contrib_margin_pag.dt_inicio :
                    
                    ASSIGN tip_est_contrib_margin_pag.dt_termino = v_data_corrente - 1.
                END.
            END.
        END.
        IF l_existe_contrib_margin = NO THEN DO:
            RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar as informaá‰es de PLR" 
                                               + "~~" + "N∆o existe registro Contribution Margin com o c¢digo " + fi-margin-plr-nao-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME} + " anterior a " + String(v_data_corrente)).
            ASSIGN fi-margin-plr-nao-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
            RETURN ERROR.
        END.    
    END.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-est-contrib-margin-prov W-Win 
PROCEDURE pi-cria-est-contrib-margin-prov :
/*------------------------------------------------------------------------------
  Purpose:     pi-cria-est-contrib-margin-prov
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE l_existe_indiv_factor AS LOGICAL INITIAL NO NO-UNDO.
    
    FIND FIRST tip_est_contrib_margin_prov
         WHERE tip_est_contrib_margin_prov.cdn_empresa = rh_estab.cdn_empresa
           AND tip_est_contrib_margin_prov.cdn_estab = v_cdn_estab_fp0560ymep
           AND tip_est_contrib_margin_prov.LOG_corporativo = YES
           AND tip_est_contrib_margin_prov.dt_inicio = v_data_corrente
                   EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL tip_est_contrib_margin_prov THEN DO: /*atualiza o registro da func_indiv_factor*/
    
        FOR EACH  tip_contrib_margin
            WHERE tip_contrib_margin.cdn_empresa      = rh_estab.cdn_empresa 
              AND tip_contrib_margin.cdn_estab = v_cdn_estab_fp0560ymep
              AND tip_contrib_margin.LOG_corporativo = YES
              AND tip_contrib_margin.perc_ini <= dec(fi-margin-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME})
              AND tip_contrib_margin.perc_fim >= dec(fi-margin-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME})
              AND tip_contrib_margin.dt_inicio       <= v_data_corrente BREAK BY tip_contrib_margin.dt_inicio : 
            IF LAST(tip_contrib_margin.dt_inicio) THEN DO:
            
                IF tip_est_contrib_margin_prov.perc_margem <> dec(fi-margin-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN DO:
                    RUN pi-cria-hist-est-contrib-margin-prov(INPUT YES, INPUT dec(fi-margin-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME})).

                    ASSIGN tip_est_contrib_margin_prov.perc_margem =  dec(fi-margin-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                               tip_est_contrib_margin_prov.cod_usuario = v_cod_usuar_corren
                               tip_est_contrib_margin_prov.dt_inclusao = TODAY
                               tip_est_contrib_margin_prov.hr_inclusao = fn_busca_hora().
                END.
                ASSIGN l_existe_indiv_factor = YES.
            END.
        END.
        /*N∆o existe nenhum tip_contrib_margin anterior a data do mes corrente, sendo assim n∆o salva*/
        IF l_existe_indiv_factor = NO THEN DO:
            RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar as informaá‰es de PLR" 
                                               + "~~" + "N∆o existe registro Individual Factor com o c¢digo " + fi-margin-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME}  + " anterior a " + String(v_data_corrente)).
            ASSIGN fi-margin-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(tip_est_contrib_margin_prov.perc_margem).
            RETURN ERROR.
        END.
    END.
    ELSE DO: /*cria o registro da func_indiv_factor*/
        FOR EACH  tip_contrib_margin
            WHERE tip_contrib_margin.perc_ini <= dec(fi-margin-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME})
              AND tip_contrib_margin.perc_fim >= dec(fi-margin-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME})
              AND tip_contrib_margin.cdn_empresa      = rh_estab.cdn_empresa 
              AND tip_contrib_margin.cdn_estab = v_cdn_estab_fp0560ymep
              AND tip_contrib_margin.LOG_corporativo = YES
              AND tip_contrib_margin.dt_inicio       <= v_data_corrente BREAK BY tip_contrib_margin.dt_inicio : 
            IF LAST(tip_contrib_margin.dt_inicio) THEN DO:

                RUN pi-cria-hist-est-contrib-margin-prov(INPUT YES, INPUT dec(fi-margin-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME})).

                CREATE tip_est_contrib_margin_prov.
                ASSIGN tip_est_contrib_margin_prov.cdn_empresa = rh_estab.cdn_empresa
                           tip_est_contrib_margin_prov.cdn_estab = v_cdn_estab_fp0560ymep
                           tip_est_contrib_margin_prov.LOG_corporativo = YES
                           tip_est_contrib_margin_prov.perc_margem = dec(fi-margin-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                           tip_est_contrib_margin_prov.perc_ini = tip_contrib_margin.perc_ini
                           tip_est_contrib_margin_prov.perc_fim = tip_contrib_margin.perc_fim
                           tip_est_contrib_margin_prov.cod_usuario = v_cod_usuar_corren
                           tip_est_contrib_margin_prov.dt_inicio = v_data_corrente
                           tip_est_contrib_margin_prov.dt_termino = 12/31/9999
                           tip_est_contrib_margin_prov.dt_inclusao = TODAY
                           tip_est_contrib_margin_prov.hr_inclusao = fn_busca_hora()
                           l_existe_indiv_factor = YES.

                FOR LAST tip_est_contrib_margin_prov EXCLUSIVE-LOCK
                    WHERE tip_est_contrib_margin_prov.cdn_empresa = rh_estab.cdn_empresa
                      and tip_est_contrib_margin_prov.cdn_estab = v_cdn_estab_fp0560ymep
                      and tip_est_contrib_margin_prov.LOG_corporativo = YES
                      AND tip_est_contrib_margin_prov.dt_inicio <> v_data_corrente
                      AND tip_est_contrib_margin_prov.dt_inicio < v_data_corrente
                       BY tip_est_contrib_margin_prov.dt_inicio :
                    
                    ASSIGN tip_est_contrib_margin_prov.dt_termino = v_data_corrente - 1.
                END.
            END.
        END.
        IF l_existe_indiv_factor = NO THEN DO:
            RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar as informaá‰es de PLR" 
                                               + "~~" + "N∆o existe registro Individual Factor com o c¢digo " + fi-margin-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME} + " anterior a " + String(v_data_corrente)).
            ASSIGN fi-margin-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
            RETURN ERROR.
        END.
    END. 

    FIND FIRST tip_est_contrib_margin_prov
         WHERE tip_est_contrib_margin_prov.cdn_empresa = rh_estab.cdn_empresa
           AND tip_est_contrib_margin_prov.cdn_estab = v_cdn_estab_fp0560ymep
           AND tip_est_contrib_margin_prov.LOG_corporativo = NO
           AND tip_est_contrib_margin_prov.dt_inicio = v_data_corrente
                   EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL tip_est_contrib_margin_prov THEN DO: /*atualiza o registro da func_indiv_factor*/
    
        FOR EACH  tip_contrib_margin
            WHERE tip_contrib_margin.cdn_empresa      = rh_estab.cdn_empresa 
              AND tip_contrib_margin.cdn_estab = v_cdn_estab_fp0560ymep
              AND tip_contrib_margin.LOG_corporativo = NO
              AND tip_contrib_margin.perc_ini <= dec(fi-margin-nao-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME})
              AND tip_contrib_margin.perc_fim >= dec(fi-margin-nao-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME})
              AND tip_contrib_margin.dt_inicio       <= v_data_corrente BREAK BY tip_contrib_margin.dt_inicio : 
            IF LAST(tip_contrib_margin.dt_inicio) THEN DO:
            
                IF tip_est_contrib_margin_prov.perc_margem <> dec(fi-margin-nao-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN DO:
                    RUN pi-cria-hist-est-contrib-margin-prov(INPUT NO, INPUT dec(fi-margin-nao-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME})).

                    ASSIGN tip_est_contrib_margin_prov.perc_margem = dec(fi-margin-nao-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                               tip_est_contrib_margin_prov.cod_usuario = v_cod_usuar_corren
                               tip_est_contrib_margin_prov.dt_inclusao = TODAY
                               tip_est_contrib_margin_prov.hr_inclusao = fn_busca_hora().
                END.
                ASSIGN l_existe_indiv_factor = YES.
            END.
        END.
        /*N∆o existe nenhum tip_contrib_margin anterior a data do mes corrente, sendo assim n∆o salva*/
        IF l_existe_indiv_factor = NO THEN DO:
            RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar as informaá‰es de PLR" 
                                               + "~~" + "N∆o existe registro Individual Factor com o c¢digo " + fi-margin-nao-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME}  + " anterior a " + String(v_data_corrente)).
            ASSIGN fi-margin-nao-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(tip_est_contrib_margin_prov.perc_margem).
            RETURN ERROR.
        END.
    END.
    ELSE DO: /*cria o registro da func_indiv_factor*/
        FOR EACH  tip_contrib_margin
            WHERE tip_contrib_margin.perc_ini <= dec(fi-margin-nao-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME})
              AND tip_contrib_margin.perc_fim >= dec(fi-margin-nao-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME})
              AND tip_contrib_margin.cdn_empresa      = rh_estab.cdn_empresa 
              AND tip_contrib_margin.cdn_estab = v_cdn_estab_fp0560ymep
              AND tip_contrib_margin.LOG_corporativo = NO
              AND tip_contrib_margin.dt_inicio       <= v_data_corrente BREAK BY tip_contrib_margin.dt_inicio : 
            IF LAST(tip_contrib_margin.dt_inicio) THEN DO:

                RUN pi-cria-hist-est-contrib-margin-prov(INPUT NO, INPUT dec(fi-margin-nao-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME})).

                CREATE tip_est_contrib_margin_prov.
                ASSIGN tip_est_contrib_margin_prov.cdn_empresa = rh_estab.cdn_empresa
                           tip_est_contrib_margin_prov.cdn_estab = v_cdn_estab_fp0560ymep
                           tip_est_contrib_margin_prov.LOG_corporativo = NO
                           tip_est_contrib_margin_prov.perc_margem = dec(fi-margin-nao-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                           tip_est_contrib_margin_prov.perc_ini = tip_contrib_margin.perc_ini
                           tip_est_contrib_margin_prov.perc_fim = tip_contrib_margin.perc_fim
                           tip_est_contrib_margin_prov.cod_usuario = v_cod_usuar_corren
                           tip_est_contrib_margin_prov.dt_inicio = v_data_corrente
                           tip_est_contrib_margin_prov.dt_termino = 12/31/9999
                           tip_est_contrib_margin_prov.dt_inclusao = TODAY
                           tip_est_contrib_margin_prov.hr_inclusao = fn_busca_hora()
                           l_existe_indiv_factor = YES.

                FOR LAST tip_est_contrib_margin_prov EXCLUSIVE-LOCK
                    WHERE tip_est_contrib_margin_prov.cdn_empresa = rh_estab.cdn_empresa
                      and tip_est_contrib_margin_prov.cdn_estab = v_cdn_estab_fp0560ymep
                      and tip_est_contrib_margin_prov.LOG_corporativo = NO
                      AND tip_est_contrib_margin_prov.dt_inicio <> v_data_corrente
                      AND tip_est_contrib_margin_prov.dt_inicio < v_data_corrente
                       BY tip_est_contrib_margin_prov.dt_inicio :
                    
                    ASSIGN tip_est_contrib_margin_prov.dt_termino = v_data_corrente - 1.
                END.
            END.
        END.
        IF l_existe_indiv_factor = NO THEN DO:
            RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar as informaá‰es de PLR" 
                                               + "~~" + "N∆o existe registro Individual Factor com o c¢digo " + fi-margin-nao-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME} + " anterior a " + String(v_data_corrente)).
            ASSIGN fi-margin-nao-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
            RETURN ERROR.
        END.
    END.     

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-est-targets-pag W-Win 
PROCEDURE pi-cria-est-targets-pag :
/*------------------------------------------------------------------------------
  Purpose:     pi-cria-est-contrib-margin-pag
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE l_existe_indiv_factor AS LOGICAL INITIAL NO NO-UNDO.

    FIND FIRST tip_est_targets_pag
         WHERE tip_est_targets_pag.cdn_empresa = rh_estab.cdn_empresa
           AND tip_est_targets_pag.cdn_estab = v_cdn_estab_fp0560ymep
           AND tip_est_targets_pag.LOG_corporativo = YES
           AND tip_est_targets_pag.dt_inicio = v_data_corrente
                   EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL tip_est_targets_pag THEN DO: /*atualiza o registro da func_indiv_factor*/
    
        FOR EACH  tip_targets
            WHERE tip_targets.cdn_empresa      = rh_estab.cdn_empresa 
              AND tip_targets.cdn_estab        = v_cdn_estab_fp0560ymep
              AND tip_targets.LOG_corporativo  = YES
              AND tip_targets.RESULT = dec(fi-result-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
              AND tip_targets.dt_inicio       <= v_data_corrente BREAK BY tip_targets.dt_inicio : 
            IF LAST(tip_targets.dt_inicio) THEN DO:
            
                IF tip_est_targets_pag.RESULT <> tip_targets.RESULT THEN DO:
                    RUN pi-cria-hist-est-targets-pag(INPUT YES).

                    ASSIGN tip_est_targets_pag.RESULT = tip_targets.RESULT
                               tip_est_targets_pag.cod_usuario = v_cod_usuar_corren
                               tip_est_targets_pag.dt_inclusao = TODAY
                               tip_est_targets_pag.hr_inclusao = fn_busca_hora().
                END.                                                         
                ASSIGN l_existe_indiv_factor = YES.
            END.
        END.
        /*N∆o existe nenhum tip_targets anterior a data do mes corrente, sendo assim n∆o salva*/
        IF l_existe_indiv_factor = NO THEN DO:
            RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar as informaá‰es de PLR" 
                                               + "~~" + "N∆o existe registro Contribution Margin com o c¢digo " + fi-result-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME}  + " anterior a " + String(v_data_corrente)).
            ASSIGN fi-result-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(tip_est_targets_pag.RESULT).
            RETURN ERROR.
        END.
    END.
    ELSE DO: /*cria o registro da func_indiv_factor*/
        FOR EACH  tip_targets
            WHERE tip_targets.RESULT = DEC(fi-result-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME} ) 
              AND tip_targets.cdn_empresa      = rh_estab.cdn_empresa 
              AND tip_targets.cdn_estab = v_cdn_estab_fp0560ymep
              AND tip_targets.LOG_corporativo = YES
              AND tip_targets.dt_inicio       <= v_data_corrente BREAK BY tip_targets.dt_inicio : 
            IF LAST(tip_targets.dt_inicio) THEN DO:

                RUN pi-cria-hist-est-targets-pag(INPUT YES).

                CREATE tip_est_targets_pag.
                ASSIGN tip_est_targets_pag.cdn_empresa = rh_estab.cdn_empresa
                       tip_est_targets_pag.cdn_estab = v_cdn_estab_fp0560ymep
                       tip_est_targets_pag.RESULT = tip_targets.RESULT
                       tip_est_targets_pag.LOG_corporativo = YES
                       tip_est_targets_pag.RESULT = tip_targets.RESULT
                       tip_est_targets_pag.ind_diretor = tip_targets.ind_diretor
                       tip_est_targets_pag.ind_diretor = tip_targets.ind_gerente
                       tip_est_targets_pag.ind_diretor = tip_targets.ind_coordenador
                       tip_est_targets_pag.ind_diretor = tip_targets.ind_superv_sr
                       tip_est_targets_pag.ind_demais  = tip_targets.ind_demais
                       tip_est_targets_pag.cod_usuario = v_cod_usuar_corren
                       tip_est_targets_pag.dt_inicio = v_data_corrente
                       tip_est_targets_pag.dt_termino = 12/31/9999
                       tip_est_targets_pag.dt_inclusao = TODAY
                       tip_est_targets_pag.hr_inclusao = fn_busca_hora()
                       l_existe_indiv_factor = YES.

                FOR LAST tip_est_targets_pag EXCLUSIVE-LOCK
                    WHERE tip_est_targets_pag.cdn_empresa = rh_estab.cdn_empresa
                      and tip_est_targets_pag.cdn_estab = v_cdn_estab_fp0560ymep
                      and tip_est_targets_pag.LOG_corporativo = YES
                      AND tip_est_targets_pag.dt_inicio <> v_data_corrente
                      AND tip_est_targets_pag.dt_inicio < v_data_corrente
                       BY tip_est_targets_pag.dt_inicio :
                    
                    ASSIGN tip_est_targets_pag.dt_termino = v_data_corrente - 1.
                END.
            END.
        END.
        IF l_existe_indiv_factor = NO THEN DO:
            RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar as informaá‰es de PLR" 
                                               + "~~" + "N∆o existe registro Contribution Margin com o c¢digo " + fi-result-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME} + " anterior a " + String(v_data_corrente)).
            ASSIGN fi-result-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
            RETURN ERROR.
        END.
    END.

    FIND FIRST tip_est_targets_pag
         WHERE tip_est_targets_pag.cdn_empresa = rh_estab.cdn_empresa
           AND tip_est_targets_pag.cdn_estab = v_cdn_estab_fp0560ymep
           AND tip_est_targets_pag.LOG_corporativo = NO
           AND tip_est_targets_pag.dt_inicio = v_data_corrente
                   EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL tip_est_targets_pag THEN DO: /*atualiza o registro da func_indiv_factor*/
    
        FOR EACH  tip_targets
            WHERE tip_targets.cdn_empresa      = rh_estab.cdn_empresa 
              AND tip_targets.cdn_estab        = v_cdn_estab_fp0560ymep
              AND tip_targets.LOG_corporativo  = NO
              AND tip_targets.RESULT = dec(fi-result-nao-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
              AND tip_targets.dt_inicio       <= v_data_corrente BREAK BY tip_targets.dt_inicio : 
            IF LAST(tip_targets.dt_inicio) THEN DO:
            
                IF tip_est_targets_pag.RESULT <> tip_targets.RESULT THEN DO:
                    RUN pi-cria-hist-est-targets-pag(INPUT NO).

                    ASSIGN tip_est_targets_pag.RESULT = tip_targets.RESULT
                               tip_est_targets_pag.cod_usuario = v_cod_usuar_corren
                               tip_est_targets_pag.dt_inclusao = TODAY
                               tip_est_targets_pag.hr_inclusao = fn_busca_hora().
                END.
                ASSIGN l_existe_indiv_factor = YES.
            END.
        END.
        /*N∆o existe nenhum tip_targets anterior a data do mes corrente, sendo assim n∆o salva*/
        IF l_existe_indiv_factor = NO THEN DO:
            RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar as informaá‰es de PLR" 
                                               + "~~" + "N∆o existe registro Contribution Margin com o c¢digo " + fi-result-nao-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME}  + " anterior a " + String(v_data_corrente)).
            ASSIGN fi-result-nao-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(tip_est_targets_pag.RESULT).
            RETURN ERROR.
        END.
    END.
    ELSE DO: /*cria o registro da func_indiv_factor*/
        FOR EACH  tip_targets
            WHERE tip_targets.RESULT = DEC(fi-result-nao-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME} ) 
              AND tip_targets.cdn_empresa      = rh_estab.cdn_empresa 
              AND tip_targets.cdn_estab = v_cdn_estab_fp0560ymep
              AND tip_targets.LOG_corporativo = NO
              AND tip_targets.dt_inicio       <= v_data_corrente BREAK BY tip_targets.dt_inicio : 
            IF LAST(tip_targets.dt_inicio) THEN DO:

                RUN pi-cria-hist-est-targets-pag(INPUT NO).

                CREATE tip_est_targets_pag.
                ASSIGN tip_est_targets_pag.cdn_empresa = rh_estab.cdn_empresa
                           tip_est_targets_pag.cdn_estab = v_cdn_estab_fp0560ymep
                           tip_est_targets_pag.RESULT = tip_targets.RESULT
                           tip_est_targets_pag.LOG_corporativo = NO
                           tip_est_targets_pag.RESULT = tip_targets.RESULT
                           tip_est_targets_pag.ind_diretor = tip_targets.ind_diretor
                           tip_est_targets_pag.ind_diretor = tip_targets.ind_gerente
                           tip_est_targets_pag.ind_diretor = tip_targets.ind_coordenador
                           tip_est_targets_pag.ind_diretor = tip_targets.ind_superv_sr
                           tip_est_targets_pag.ind_demais  = tip_targets.ind_demais
                           tip_est_targets_pag.cod_usuario = v_cod_usuar_corren
                           tip_est_targets_pag.dt_inicio = v_data_corrente
                           tip_est_targets_pag.dt_termino = 12/31/9999
                           tip_est_targets_pag.dt_inclusao = TODAY
                           tip_est_targets_pag.hr_inclusao = fn_busca_hora()
                           l_existe_indiv_factor = YES.

                FOR LAST tip_est_targets_pag EXCLUSIVE-LOCK
                    WHERE tip_est_targets_pag.cdn_empresa = rh_estab.cdn_empresa
                      and tip_est_targets_pag.cdn_estab = v_cdn_estab_fp0560ymep
                      and tip_est_targets_pag.LOG_corporativo = NO
                      AND tip_est_targets_pag.dt_inicio <> v_data_corrente
                      AND tip_est_targets_pag.dt_inicio < v_data_corrente
                       BY tip_est_targets_pag.dt_inicio :
                    
                    ASSIGN tip_est_targets_pag.dt_termino = v_data_corrente - 1.
                END.
            END.
        END.
        IF l_existe_indiv_factor = NO THEN DO:
            RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar as informaá‰es de PLR" 
                                               + "~~" + "N∆o existe registro Contribution Margin com o c¢digo " + fi-result-nao-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME} + " anterior a " + String(v_data_corrente)).
            ASSIGN fi-result-nao-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
            RETURN ERROR.
        END.
    END.
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-est-targets-prov W-Win 
PROCEDURE pi-cria-est-targets-prov :
/*------------------------------------------------------------------------------
  Purpose:     pi-cria-est-contrib-margin-prov
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE l_existe_indiv_factor AS LOGICAL INITIAL NO NO-UNDO.

        FIND FIRST tip_est_targets_prov
             WHERE tip_est_targets_prov.cdn_empresa = rh_estab.cdn_empresa
               AND tip_est_targets_prov.cdn_estab = v_cdn_estab_fp0560ymep
               AND tip_est_targets_prov.LOG_corporativo = YES
               AND tip_est_targets_prov.dt_inicio = v_data_corrente
                       EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL tip_est_targets_prov THEN DO: /*atualiza o registro da func_indiv_factor*/
        
            FOR EACH  tip_targets
                WHERE tip_targets.cdn_empresa      = rh_estab.cdn_empresa 
                  AND tip_targets.cdn_estab        = v_cdn_estab_fp0560ymep
                  AND tip_targets.LOG_corporativo  = YES
                  AND tip_targets.RESULT = dec(fi-result-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
                  AND tip_targets.dt_inicio       <= v_data_corrente BREAK BY tip_targets.dt_inicio : 
                IF LAST(tip_targets.dt_inicio) THEN DO:
                
                    IF tip_est_targets_prov.RESULT <> tip_targets.RESULT THEN DO:
                        RUN pi-cria-hist-est-targets-prov(INPUT YES).

                        ASSIGN tip_est_targets_prov.RESULT = tip_targets.RESULT
                                   tip_est_targets_prov.cod_usuario = v_cod_usuar_corren
                                   tip_est_targets_prov.dt_inclusao = TODAY
                                   tip_est_targets_prov.hr_inclusao = fn_busca_hora().
                    END.
                    ASSIGN l_existe_indiv_factor = YES.
                END.
            END.
            /*N∆o existe nenhum tip_targets anterior a data do mes corrente, sendo assim n∆o salva*/
            IF l_existe_indiv_factor = NO THEN DO:
                RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar as informaá‰es de PLR" 
                                                   + "~~" + "N∆o existe registro Contribution Margin com o c¢digo " + fi-result-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME}  + " anterior a " + String(v_data_corrente)).
                ASSIGN fi-result-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(tip_est_targets_prov.RESULT).
                RETURN ERROR.
            END.
        END.
        ELSE DO: /*cria o registro da func_indiv_factor*/
            FOR EACH  tip_targets
                WHERE tip_targets.RESULT = DEC(fi-result-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME} ) 
                  AND tip_targets.cdn_empresa      = rh_estab.cdn_empresa 
                  AND tip_targets.cdn_estab = v_cdn_estab_fp0560ymep
                  AND tip_targets.LOG_corporativo = YES
                  AND tip_targets.dt_inicio       <= v_data_corrente BREAK BY tip_targets.dt_inicio : 
                IF LAST(tip_targets.dt_inicio) THEN DO:

                    RUN pi-cria-hist-est-targets-prov(INPUT YES).

                    CREATE tip_est_targets_prov.
                    ASSIGN tip_est_targets_prov.cdn_empresa = rh_estab.cdn_empresa
                               tip_est_targets_prov.cdn_estab = v_cdn_estab_fp0560ymep
                               tip_est_targets_prov.RESULT = tip_targets.RESULT
                               tip_est_targets_prov.LOG_corporativo = YES
                               tip_est_targets_prov.RESULT = tip_targets.RESULT
                               tip_est_targets_prov.ind_diretor = tip_targets.ind_diretor
                               tip_est_targets_prov.ind_diretor = tip_targets.ind_gerente
                               tip_est_targets_prov.ind_diretor = tip_targets.ind_coordenador
                               tip_est_targets_prov.ind_diretor = tip_targets.ind_superv_sr
                               tip_est_targets_prov.ind_demais  = tip_targets.ind_demais
                               tip_est_targets_prov.cod_usuario = v_cod_usuar_corren
                               tip_est_targets_prov.dt_inicio = v_data_corrente
                               tip_est_targets_prov.dt_termino = 12/31/9999
                               tip_est_targets_prov.dt_inclusao = TODAY
                               tip_est_targets_prov.hr_inclusao = fn_busca_hora()
                               l_existe_indiv_factor = YES.

                    FOR LAST tip_est_targets_prov EXCLUSIVE-LOCK
                        WHERE tip_est_targets_prov.cdn_empresa = rh_estab.cdn_empresa
                          and tip_est_targets_prov.cdn_estab = v_cdn_estab_fp0560ymep
                          and tip_est_targets_prov.LOG_corporativo = YES
                          AND tip_est_targets_prov.dt_inicio <> v_data_corrente
                          AND tip_est_targets_prov.dt_inicio < v_data_corrente
                           BY tip_est_targets_prov.dt_inicio :
                        
                        ASSIGN tip_est_targets_prov.dt_termino = v_data_corrente - 1.
                    END.
                END.
            END.
            IF l_existe_indiv_factor = NO THEN DO:
                RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar as informaá‰es de PLR" 
                                                   + "~~" + "N∆o existe registro Contribution Margin com o c¢digo " + fi-result-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME} + " anterior a " + String(v_data_corrente)).
                ASSIGN fi-result-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
                RETURN ERROR.
            END.
        END.

        FIND FIRST tip_est_targets_prov
             WHERE tip_est_targets_prov.cdn_empresa = rh_estab.cdn_empresa
               AND tip_est_targets_prov.cdn_estab = v_cdn_estab_fp0560ymep
               AND tip_est_targets_prov.LOG_corporativo = NO
               AND tip_est_targets_prov.dt_inicio = v_data_corrente
                       EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL tip_est_targets_prov THEN DO: /*atualiza o registro da func_indiv_factor*/
        
            FOR EACH  tip_targets
                WHERE tip_targets.cdn_empresa      = rh_estab.cdn_empresa 
                  AND tip_targets.cdn_estab        = v_cdn_estab_fp0560ymep
                  AND tip_targets.LOG_corporativo  = NO
                  AND tip_targets.RESULT = dec(fi-result-nao-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                  AND tip_targets.dt_inicio       <= v_data_corrente BREAK BY tip_targets.dt_inicio : 
                IF LAST(tip_targets.dt_inicio) THEN DO:
                
                    IF tip_est_targets_prov.RESULT <> tip_targets.RESULT THEN DO:
                        RUN pi-cria-hist-est-targets-prov(INPUT NO).

                        ASSIGN tip_est_targets_prov.RESULT = tip_targets.RESULT
                                   tip_est_targets_prov.cod_usuario = v_cod_usuar_corren
                                   tip_est_targets_prov.dt_inclusao = TODAY
                                   tip_est_targets_prov.hr_inclusao = fn_busca_hora().
                    END.
                    ASSIGN l_existe_indiv_factor = YES.
                END.
            END.
            /*N∆o existe nenhum tip_targets anterior a data do mes corrente, sendo assim n∆o salva*/
            IF l_existe_indiv_factor = NO THEN DO:
                RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar as informaá‰es de PLR" 
                                                   + "~~" + "N∆o existe registro Contribution Margin com o c¢digo " + fi-result-nao-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME}  + " anterior a " + String(v_data_corrente)).
                ASSIGN fi-result-nao-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(tip_est_targets_prov.RESULT).
                RETURN ERROR.
            END.
        END.
        ELSE DO: /*cria o registro da func_indiv_factor*/
            FOR EACH  tip_targets
                WHERE tip_targets.RESULT = DEC(fi-result-nao-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME} ) 
                  AND tip_targets.cdn_empresa      = rh_estab.cdn_empresa 
                  AND tip_targets.cdn_estab = v_cdn_estab_fp0560ymep
                  AND tip_targets.LOG_corporativo = NO
                  AND tip_targets.dt_inicio       <= v_data_corrente BREAK BY tip_targets.dt_inicio : 
                IF LAST(tip_targets.dt_inicio) THEN DO:

                    RUN pi-cria-hist-est-targets-prov(INPUT NO).

                    CREATE tip_est_targets_prov.
                    ASSIGN tip_est_targets_prov.cdn_empresa = rh_estab.cdn_empresa
                               tip_est_targets_prov.cdn_estab = v_cdn_estab_fp0560ymep
                               tip_est_targets_prov.RESULT = tip_targets.RESULT
                               tip_est_targets_prov.LOG_corporativo = NO
                               tip_est_targets_prov.RESULT = tip_targets.RESULT
                               tip_est_targets_prov.ind_diretor = tip_targets.ind_diretor
                               tip_est_targets_prov.ind_diretor = tip_targets.ind_gerente
                               tip_est_targets_prov.ind_diretor = tip_targets.ind_coordenador
                               tip_est_targets_prov.ind_diretor = tip_targets.ind_superv_sr
                               tip_est_targets_prov.ind_demais  = tip_targets.ind_demais
                               tip_est_targets_prov.cod_usuario = v_cod_usuar_corren
                               tip_est_targets_prov.dt_inicio = v_data_corrente
                               tip_est_targets_prov.dt_termino = 12/31/9999
                               tip_est_targets_prov.dt_inclusao = TODAY
                               tip_est_targets_prov.hr_inclusao = fn_busca_hora()
                               l_existe_indiv_factor = YES.

                    FOR LAST tip_est_targets_prov EXCLUSIVE-LOCK
                        WHERE tip_est_targets_prov.cdn_empresa = rh_estab.cdn_empresa
                          and tip_est_targets_prov.cdn_estab = v_cdn_estab_fp0560ymep
                          and tip_est_targets_prov.LOG_corporativo = NO
                          AND tip_est_targets_prov.dt_inicio <> v_data_corrente
                          AND tip_est_targets_prov.dt_inicio < v_data_corrente
                           BY tip_est_targets_prov.dt_inicio :
                        
                        ASSIGN tip_est_targets_prov.dt_termino = v_data_corrente - 1.
                    END.
                END.
            END.
            IF l_existe_indiv_factor = NO THEN DO:
                RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar as informaá‰es de PLR" 
                                                   + "~~" + "N∆o existe registro Contribution Margin com o c¢digo " + fi-result-nao-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME} + " anterior a " + String(v_data_corrente)).
                ASSIGN fi-result-nao-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
                RETURN ERROR.
            END.
        END.
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-hist-est-contrib-margin-pag W-Win 
PROCEDURE pi-cria-hist-est-contrib-margin-pag :
/*------------------------------------------------------------------------------
  Purpose:     pi-cria-hist-est-contrib-margin-pag
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p_log_corporativo AS LOG NO-UNDO.
    DEFINE INPUT PARAMETER p_margem AS DECIMAL NO-UNDO.

    DEFINE VARIABLE v_hr_inclusao AS CHARACTER   NO-UNDO.

    CREATE tiph_est_contrib_margin_pag.
    ASSIGN tiph_est_contrib_margin_pag.cdn_empresa = rh_estab.cdn_empresa
           tiph_est_contrib_margin_pag.cdn_estab = v_cdn_estab_fp0560ymep
           tiph_est_contrib_margin_pag.LOG_corporativo = p_log_corporativo
           tiph_est_contrib_margin_pag.perc_margem = p_margem
           tiph_est_contrib_margin_pag.perc_ini = tip_contrib_margin.perc_ini
           tiph_est_contrib_margin_pag.perc_fim = tip_contrib_margin.perc_fim
           tiph_est_contrib_margin_pag.cod_usuario = v_cod_usuar_corren
           tiph_est_contrib_margin_pag.dt_inicio = v_data_corrente
           tiph_est_contrib_margin_pag.dt_termino = 12/31/9999
           tiph_est_contrib_margin_pag.dt_inclusao = TODAY
           tiph_est_contrib_margin_pag.hr_inclusao = fn_busca_hora()
           v_hr_inclusao = tiph_est_contrib_margin_pag.hr_inclusao.

    FOR LAST  tiph_est_contrib_margin_pag EXCLUSIVE-LOCK
        WHERE tiph_est_contrib_margin_pag.cdn_empresa = rh_estab.cdn_empresa
          AND tiph_est_contrib_margin_pag.cdn_estab = v_cdn_estab_fp0560ymep
          AND tiph_est_contrib_margin_pag.LOG_corporativo = p_log_corporativo
          AND tiph_est_contrib_margin_pag.hr_inclusao <> v_hr_inclusao 
           BY tiph_est_contrib_margin_pag.dt_inclusao
           BY tiph_est_contrib_margin_pag.hr_inclusao:
       
        IF tiph_est_contrib_margin_pag.dt_inicio = v_data_corrente THEN
            ASSIGN tiph_est_contrib_margin_pag.dt_termino       = v_data_corrente.
        ELSE 
            ASSIGN tiph_est_contrib_margin_pag.dt_termino       = v_data_corrente - 1.
    END.
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-hist-est-contrib-margin-prov W-Win 
PROCEDURE pi-cria-hist-est-contrib-margin-prov :
/*------------------------------------------------------------------------------
  Purpose:     pi-cria-hist-est-contrib-margin-prov
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p_log_corporativo AS LOG NO-UNDO.
    DEFINE INPUT PARAMETER p_margem AS DECIMAL NO-UNDO.

    DEFINE VARIABLE v_hr_inclusao AS CHARACTER   NO-UNDO.

    CREATE tiph_est_contrib_margin_prov.
    ASSIGN tiph_est_contrib_margin_prov.cdn_empresa = rh_estab.cdn_empresa
           tiph_est_contrib_margin_prov.cdn_estab = v_cdn_estab_fp0560ymep
           tiph_est_contrib_margin_prov.LOG_corporativo = p_log_corporativo
           tiph_est_contrib_margin_prov.perc_margem = p_margem
           tiph_est_contrib_margin_prov.perc_ini = tip_contrib_margin.perc_ini
           tiph_est_contrib_margin_prov.perc_fim = tip_contrib_margin.perc_fim
           tiph_est_contrib_margin_prov.cod_usuario = v_cod_usuar_corren
           tiph_est_contrib_margin_prov.dt_inicio = v_data_corrente
           tiph_est_contrib_margin_prov.dt_termino = 12/31/9999
           tiph_est_contrib_margin_prov.dt_inclusao = TODAY
           tiph_est_contrib_margin_prov.hr_inclusao = fn_busca_hora()
           v_hr_inclusao = tiph_est_contrib_margin_prov.hr_inclusao.

    FOR LAST  tiph_est_contrib_margin_prov EXCLUSIVE-LOCK
        WHERE tiph_est_contrib_margin_prov.cdn_empresa = rh_estab.cdn_empresa
          AND tiph_est_contrib_margin_prov.cdn_estab = v_cdn_estab_fp0560ymep
          AND tiph_est_contrib_margin_prov.LOG_corporativo = p_log_corporativo
          AND tiph_est_contrib_margin_prov.hr_inclusao <> v_hr_inclusao 
           BY tiph_est_contrib_margin_prov.dt_inclusao
           BY tiph_est_contrib_margin_prov.hr_inclusao:
       
        IF tiph_est_contrib_margin_prov.dt_inicio = v_data_corrente THEN
            ASSIGN tiph_est_contrib_margin_prov.dt_termino       = v_data_corrente.
        ELSE 
            ASSIGN tiph_est_contrib_margin_prov.dt_termino       = v_data_corrente - 1.
    END.
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-hist-est-targets-pag W-Win 
PROCEDURE pi-cria-hist-est-targets-pag :
/*------------------------------------------------------------------------------
  Purpose:     pi-cria-hist-est-targets-pag
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p_log_corporativo AS LOG NO-UNDO.

    DEFINE VARIABLE v_hr_inclusao AS CHARACTER   NO-UNDO.

    CREATE tiph_est_targets_pag.
    ASSIGN tiph_est_targets_pag.cdn_empresa = rh_estab.cdn_empresa
           tiph_est_targets_pag.cdn_estab = v_cdn_estab_fp0560ymep
           tiph_est_targets_pag.LOG_corporativo = p_log_corporativo
           tiph_est_targets_pag.RESULT = tip_targets.RESULT
           tiph_est_targets_pag.ind_diretor = tip_targets.ind_diretor
           tiph_est_targets_pag.ind_gerente = tip_targets.ind_gerente
           tiph_est_targets_pag.ind_coordenador = tip_targets.ind_coordenador
           tiph_est_targets_pag.ind_superv_sr = tip_targets.ind_superv_sr
           tiph_est_targets_pag.ind_demais  = tip_targets.ind_demais
           tiph_est_targets_pag.cod_usuario = v_cod_usuar_corren
           tiph_est_targets_pag.dt_inicio = v_data_corrente
           tiph_est_targets_pag.dt_termino = 12/31/9999
           tiph_est_targets_pag.dt_inclusao = TODAY
           tiph_est_targets_pag.hr_inclusao = fn_busca_hora()
           v_hr_inclusao = tiph_est_targets_pag.hr_inclusao.

    FOR LAST  tiph_est_targets_pag EXCLUSIVE-LOCK
        WHERE tiph_est_targets_pag.cdn_empresa = rh_estab.cdn_empresa
          AND tiph_est_targets_pag.cdn_estab = v_cdn_estab_fp0560ymep
          AND tiph_est_targets_pag.LOG_corporativo = p_log_corporativo
          AND tiph_est_targets_pag.hr_inclusao <> v_hr_inclusao 
           BY tiph_est_targets_pag.dt_inclusao
           BY tiph_est_targets_pag.hr_inclusao:
       
        IF tiph_est_targets_pag.dt_inicio = v_data_corrente THEN
            ASSIGN tiph_est_targets_pag.dt_termino       = v_data_corrente.
        ELSE 
            ASSIGN tiph_est_targets_pag.dt_termino       = v_data_corrente - 1.
    END.
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-hist-est-targets-prov W-Win 
PROCEDURE pi-cria-hist-est-targets-prov :
/*------------------------------------------------------------------------------
  Purpose:     pi-cria-hist-est-targets-prov
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p_log_corporativo AS LOG NO-UNDO.

    DEFINE VARIABLE v_hr_inclusao AS CHARACTER   NO-UNDO.

    CREATE tiph_est_targets_prov.
    ASSIGN tiph_est_targets_prov.cdn_empresa = rh_estab.cdn_empresa
           tiph_est_targets_prov.cdn_estab = v_cdn_estab_fp0560ymep
           tiph_est_targets_prov.RESULT = tip_targets.RESULT
           tiph_est_targets_prov.LOG_corporativo = p_log_corporativo
           tiph_est_targets_prov.ind_diretor = tip_targets.ind_diretor
           tiph_est_targets_prov.ind_gerente = tip_targets.ind_gerente
           tiph_est_targets_prov.ind_coordenador = tip_targets.ind_coordenador
           tiph_est_targets_prov.ind_superv_sr = tip_targets.ind_superv_sr
           tiph_est_targets_prov.ind_demais  = tip_targets.ind_demais
           tiph_est_targets_prov.cod_usuario = v_cod_usuar_corren
           tiph_est_targets_prov.dt_inicio = v_data_corrente
           tiph_est_targets_prov.dt_termino = 12/31/9999
           tiph_est_targets_prov.dt_inclusao = TODAY
           tiph_est_targets_prov.hr_inclusao = fn_busca_hora()
           v_hr_inclusao = tiph_est_targets_prov.hr_inclusao.

    FOR LAST  tiph_est_targets_prov EXCLUSIVE-LOCK
        WHERE tiph_est_targets_prov.cdn_empresa = rh_estab.cdn_empresa
          AND tiph_est_targets_prov.cdn_estab = v_cdn_estab_fp0560ymep
          AND tiph_est_targets_prov.LOG_corporativo = p_log_corporativo
          AND tiph_est_targets_prov.hr_inclusao <> v_hr_inclusao 
           BY tiph_est_targets_prov.dt_inclusao
           BY tiph_est_targets_prov.hr_inclusao:
       
        IF tiph_est_targets_prov.dt_inicio = v_data_corrente THEN
            ASSIGN tiph_est_targets_prov.dt_termino       = v_data_corrente.
        ELSE 
            ASSIGN tiph_est_targets_prov.dt_termino       = v_data_corrente - 1.
    END.
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-salvar W-Win 
PROCEDURE pi-salvar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN pi-validate.

IF RETURN-VALUE = "OK":U THEN DO:
    RUN pi-cria-est-contrib-margin-pag.
    RUN pi-cria-est-contrib-margin-prov.
    RUN pi-cria-est-targets-pag.
    RUN pi-cria-est-targets-prov.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-validate W-Win 
PROCEDURE pi-validate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/******************************VALIDAÄÂES GERAIS*******************************/
    FIND FIRST rh_estab
         WHERE rh_estab.cdn_estab = v_cdn_estab_fp0560ymep NO-LOCK NO-ERROR.
    IF AVAIL rh_estab THEN DO:
        FOR FIRST param_empres_rh
            WHERE PARAM_empres_rh.cdn_empresa = rh_estab.cdn_empresa NO-LOCK:
            ASSIGN v_data_corrente = DATE("01/" + STRING(PARAM_empres_rh.num_mes_refer_calc_efetd) + "/" + STRING(PARAM_empres_rh.num_ano_refer_calc_efetd)).
            IF CAN-FIND (FIRST habilit_calc_fp
                         WHERE habilit_calc_fp.cdn_empresa              = rh_estab.cdn_empresa
                           AND habilit_calc_fp.cdn_estab                = rh_estab.cdn_estab
                           AND habilit_calc_fp.num_ano_refer_fp_calcula = PARAM_empres_rh.num_ano_refer_calc_efetd
                           AND habilit_calc_fp.num_mes_refer_fp         = PARAM_empres_rh.num_mes_refer_calc_efetd
                           AND habilit_calc_fp.idi_tip_fp               = 1
                           AND habilit_calc_fp.qti_parc_habilit_calc_fp = 9
                           AND habilit_calc_fp.idi_sit_calc_fp <> 1) THEN DO: /*idi_sit_calc_fp <> habilitado*/
                RUN utp/ut-msgs.p(INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar os indicadores" + "~~" 
                                  + "N∆o Ç poss°vel salvar os Indicadores de PLR porque a folha j† est† calculada para o màs " 
                                  + STRING(PARAM_empres_rh.num_mes_refer_calc_efetd) + "/" + STRING(PARAM_empres_rh.num_ano_refer_calc_efetd) + ".").
                RETURN "NOK":U.
            END.
        END.
    END.

/******************************************************************************/
/****************************VALIDAÄÂES CORPORATIVO****************************/
/******************************************************************************/
    /**************CONTRIBUTION MARGIN PLR**************/
        IF NOT CAN-FIND (FIRST tip_contrib_margin
                         WHERE tip_contrib_margin.cdn_empresa = rh_estab.cdn_empresa
                           AND tip_contrib_margin.cdn_estab = rh_estab.cdn_estab
                           AND tip_contrib_margin.LOG_corporativo = YES
                           AND tip_contrib_margin.dt_inicio <= v_data_corrente
                           AND tip_contrib_margin.perc_ini <= dec(fi-margin-plr-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                           AND tip_contrib_margin.perc_fim >= dec(fi-margin-plr-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME})) THEN DO:
            RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar os indicadores - Corporativo - PLR - Contribution Margin" 
                                   + "~~" + "N∆o existe registro Contribution Margin Corporativo com a margem " + String(fi-margin-plr-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
                                   + "% e data anterior a " + String(v_data_corrente, "99/99/9999") + ".").
            APPLY "ENTRY":U TO fi-margin-plr-corp IN FRAME {&FRAME-NAME}.
            RETURN "NOK":U.
        END.                                                 
    /***************************************************/                                                                               
    
    /********************RESULT PLR*********************/
        IF NOT CAN-FIND (FIRST tip_targets
                         WHERE tip_targets.cdn_empresa = rh_estab.cdn_empresa
                           AND tip_targets.cdn_estab = rh_estab.cdn_estab
                           AND tip_targets.LOG_corporativo = YES
                           AND tip_targets.dt_inicio <= v_data_corrente
                           AND tip_targets.RESULT = int(fi-result-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME})) THEN DO:
            RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar os indicadores - Corporativo - PLR - Result" 
                                   + "~~" + "N∆o existe registro Result (Tip Targets) Corporativo com o valor " + String(fi-result-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
                                   + " e data anterior a " + String(v_data_corrente, "99/99/9999") + ".").
            APPLY "ENTRY":U TO fi-result-corp IN FRAME {&FRAME-NAME}.
            RETURN "NOK":U.
        END.
    /***************************************************/
    
    /***********CONTRIBUTION MARGIN PROV PLR************/
        IF NOT CAN-FIND (FIRST tip_contrib_margin
                         WHERE tip_contrib_margin.cdn_empresa = rh_estab.cdn_empresa
                           AND tip_contrib_margin.cdn_estab = rh_estab.cdn_estab
                           AND tip_contrib_margin.LOG_corporativo = YES
                           AND tip_contrib_margin.dt_inicio <= v_data_corrente
                           AND tip_contrib_margin.perc_ini <= dec(fi-margin-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                           AND tip_contrib_margin.perc_fim >= dec(fi-margin-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME})) THEN DO:
            RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar os indicadores - Corporativo - Provis∆o PLR - Contribution Margin" 
                                   + "~~" + "N∆o existe registro Contribution Margin Provis∆o Corporativo com a margem " + String(fi-margin-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
                                   + "% e data anterior a " + String(v_data_corrente, "99/99/9999") + ".").
            APPLY "ENTRY":U TO fi-margin-corp-prov IN FRAME {&FRAME-NAME}.
            RETURN "NOK":U.
        END.
    /***************************************************/
    
    /*****************RESULT PROV PLR*******************/
        IF NOT CAN-FIND (FIRST tip_targets
                         WHERE tip_targets.cdn_empresa = rh_estab.cdn_empresa
                           AND tip_targets.cdn_estab = rh_estab.cdn_estab
                           AND tip_targets.LOG_corporativo = YES
                           AND tip_targets.dt_inicio <= v_data_corrente
                           AND tip_targets.RESULT = int(fi-result-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME})) THEN DO:
            RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar os indicadores - Corporativo - Provis∆o PLR - Result" 
                                   + "~~" + "N∆o existe registro Result (Tip Targets) Provis∆o Corporativo com o valor " + String(fi-result-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
                                   + " e data anterior a " + String(v_data_corrente, "99/99/9999") + ".").
            APPLY "ENTRY":U TO fi-result-corp-prov IN FRAME {&FRAME-NAME}.
            RETURN "NOK":U.
        END.
    /***************************************************/

/******************************************************************************/
/**************************VALIDAÄÂES N«O CORPORATIVO**************************/
/******************************************************************************/

    /**************CONTRIBUTION MARGIN PLR**************/
        IF NOT CAN-FIND (FIRST tip_contrib_margin
                         WHERE tip_contrib_margin.cdn_empresa = rh_estab.cdn_empresa
                           AND tip_contrib_margin.cdn_estab = rh_estab.cdn_estab
                           AND tip_contrib_margin.LOG_corporativo = NO
                           AND tip_contrib_margin.dt_inicio <= v_data_corrente
                           AND tip_contrib_margin.perc_ini <= dec(fi-margin-plr-nao-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                           AND tip_contrib_margin.perc_fim >= dec(fi-margin-plr-nao-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME})) THEN DO:
            RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar os indicadores - N∆o Corporativo - PLR - Contribution Margin" 
                                   + "~~" + "N∆o existe registro Contribution Margin N«O Corporativo com a margem " + String(fi-margin-plr-nao-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
                                   + "% e data anterior a " + String(v_data_corrente, "99/99/9999") + ".").
            APPLY "ENTRY":U TO fi-margin-plr-nao-corp IN FRAME {&FRAME-NAME}.
            RETURN "NOK":U.
        END.
    /***************************************************/                                                                               
    
    /********************RESULT PLR*********************/
        IF NOT CAN-FIND (FIRST tip_targets
                         WHERE tip_targets.cdn_empresa = rh_estab.cdn_empresa
                           AND tip_targets.cdn_estab = rh_estab.cdn_estab
                           AND tip_targets.LOG_corporativo = NO
                           AND tip_targets.dt_inicio <= v_data_corrente
                           AND tip_targets.RESULT = int(fi-result-nao-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME})) THEN DO:
            RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar os indicadores - N∆o Corporativo - PLR - Result" 
                                   + "~~" + "N∆o existe registro Result (Tip Targets) N«O Corporativo com o valor " + String(fi-result-nao-corp:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
                                   + " e data anterior a " + String(v_data_corrente, "99/99/9999") + ".").
            APPLY "ENTRY":U TO fi-result-nao-corp IN FRAME {&FRAME-NAME}.
            RETURN "NOK":U.
        END.
    /***************************************************/
    
    /***********CONTRIBUTION MARGIN PROV PLR************/
        IF NOT CAN-FIND (FIRST tip_contrib_margin
                         WHERE tip_contrib_margin.cdn_empresa = rh_estab.cdn_empresa
                           AND tip_contrib_margin.cdn_estab = rh_estab.cdn_estab
                           AND tip_contrib_margin.LOG_corporativo = NO
                           AND tip_contrib_margin.dt_inicio <= v_data_corrente
                           AND tip_contrib_margin.perc_ini <= dec(fi-margin-nao-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                           AND tip_contrib_margin.perc_fim >= dec(fi-margin-nao-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME})) THEN DO:
            RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar os indicadores - N∆o Corporativo - Provis∆o PLR - Contribution Margin" 
                                   + "~~" + "N∆o existe registro Contribution Margin Provis∆o N«O Corporativo com a margem " + String(fi-margin-nao-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
                                   + "% e data anterior a " + String(v_data_corrente, "99/99/9999") + ".").
            APPLY "ENTRY":U TO fi-margin-nao-corp-prov IN FRAME {&FRAME-NAME}.
            RETURN "NOK":U.
        END.
    /***************************************************/
    
    /*****************RESULT PROV PLR*******************/
        IF NOT CAN-FIND (FIRST tip_targets
                         WHERE tip_targets.cdn_empresa = rh_estab.cdn_empresa
                           AND tip_targets.cdn_estab = rh_estab.cdn_estab
                           AND tip_targets.LOG_corporativo = NO
                           AND tip_targets.dt_inicio <= v_data_corrente
                           AND tip_targets.RESULT = int(fi-result-nao-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME})) THEN DO:
            RUN utp/ut-msgs.p (INPUT "show", INPUT 17006, INPUT "N∆o foi poss°vel salvar os indicadores - N∆o Corporativo - Provis∆o PLR - Result" 
                                   + "~~" + "N∆o existe registro Result (Tip Targets) Provis∆o N«O Corporativo com o valor " + String(fi-result-nao-corp-prov:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
                                   + " e data anterior a " + String(v_data_corrente, "99/99/9999") + ".").
            APPLY "ENTRY":U TO fi-result-nao-corp-prov IN FRAME {&FRAME-NAME}.
            RETURN "NOK":U.
        END.
    /***************************************************/

/******************************************************************************/

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn_busca_hora W-Win 
FUNCTION fn_busca_hora RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE horas AS INTEGER     NO-UNDO.
    DEFINE VARIABLE minutos AS INTEGER     NO-UNDO.
    DEFINE VARIABLE segundos AS INTEGER     NO-UNDO.
    DEFINE VARIABLE horario AS INTEGER     NO-UNDO.
    DEFINE VARIABLE horaInclusao AS CHARACTER   NO-UNDO.

    ASSIGN horario = TIME.
    ASSIGN segundos = horario MOD 60.
    ASSIGN horario = (horario - segundos) / 60.
    ASSIGN minutos = horario MOD 60.           
    ASSIGN horas = (horario - minutos) / 60.

    ASSIGN horaInclusao = IF horas < 10 THEN "0" + STRING(horas) ELSE STRING(horas).
    ASSIGN horaInclusao = horaInclusao + ":" + IF minutos < 10 THEN "0" + STRING(minutos) ELSE STRING(minutos).
    ASSIGN horaInclusao = horaInclusao + ":" + IF segundos < 10 THEN "0" + STRING(segundos) ELSE STRING(segundos).
    
    RETURN horaInclusao.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


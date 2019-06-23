&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESFP1593A 12.1.13.000}  /*** 010104 ***/

&IF "{&EMSFND_VERSION}" >= "1.00"
&THEN
{include/i-license-manager.i ESFP1593A MFP}
&ENDIF

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

&GLOBAL-DEFINE safrista YES

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{prghur/fpp/ESFP1593tt.i}

def input-output parameter table for tt-param.
def input        parameter v_cdn_estab_ini like rh_estab.cdn_estab no-undo.

define var c-progr    as character                                     no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



&Scoped-define PROCEDURE-TYPE SmartWindow

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-19 RECT-10 RECT-11 v_log_mensal l-func ~
v_log_horista l-estag v_log_semanal l-aposent v_log_quinzenal l-empregador ~
v_log_tarefa l-temp v_log_diarista l-tempoparc l-aprendiz v-data-aposent ~
RECT-1 bt-ok bt-cancelar bt-ajuda v-label-tipo-func 
&Scoped-Define DISPLAYED-OBJECTS v_log_mensal l-func v_log_horista l-estag ~
v_log_semanal l-aposent v_log_quinzenal l-empregador v_log_tarefa l-temp ~
v_log_diarista l-tempoparc l-aprendiz v-data-aposent v-label-tipo-func 

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

DEFINE BUTTON bt-ok 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE v-data-aposent AS DATE FORMAT "99/99/9999":U INITIAL 01/01/00 
     LABEL "Data Aposentadoria" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE v-label-tipo-func AS CHARACTER FORMAT "X(256)":U INITIAL "Tipo Funcion rio" 
      VIEW-AS TEXT 
     SIZE 17.86 BY .67 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 74.29 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 19 BY 7.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 22.86 BY 7.88.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 74.29 BY 12.08.

DEFINE VARIABLE l-aposent AS LOGICAL INITIAL yes 
     LABEL "Aposentado" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .83 NO-UNDO.

DEFINE VARIABLE l-aprendiz AS LOGICAL INITIAL no 
     LABEL "Menor Aprendiz" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.57 BY .83 NO-UNDO.

DEFINE VARIABLE l-empregador AS LOGICAL INITIAL yes 
     LABEL "Empregador" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.43 BY .83 NO-UNDO.

DEFINE VARIABLE l-estag AS LOGICAL INITIAL yes 
     LABEL "Estagi rio" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.57 BY .83 NO-UNDO.

DEFINE VARIABLE l-func AS LOGICAL INITIAL yes 
     LABEL "Funcion rio" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.72 BY .83 NO-UNDO.

DEFINE VARIABLE l-temp AS LOGICAL INITIAL yes 
     LABEL "Prazo Determinado" 
     VIEW-AS TOGGLE-BOX
     SIZE 21.14 BY .83 NO-UNDO.

DEFINE VARIABLE l-tempoparc AS LOGICAL INITIAL yes 
     LABEL "Tempo Parcial" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.43 BY .83 NO-UNDO.

DEFINE VARIABLE v_log_diarista AS LOGICAL INITIAL yes 
     LABEL "Diarista" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE v_log_horista AS LOGICAL INITIAL yes 
     LABEL "Horista" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE v_log_mensal AS LOGICAL INITIAL yes 
     LABEL "Mensal" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE v_log_quinzenal AS LOGICAL INITIAL yes 
     LABEL "Quinzenal" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .83 NO-UNDO.

DEFINE VARIABLE v_log_semanal AS LOGICAL INITIAL yes 
     LABEL "Semanal" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE v_log_tarefa AS LOGICAL INITIAL yes 
     LABEL "Tarefa" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     v_log_mensal AT ROW 3.42 COL 13.72
     l-func AT ROW 3.42 COL 45
     v_log_horista AT ROW 4.42 COL 13.72
     l-estag AT ROW 4.42 COL 45
     v_log_semanal AT ROW 5.42 COL 13.72
     l-aposent AT ROW 5.42 COL 45
     v_log_quinzenal AT ROW 6.42 COL 13.72
     l-empregador AT ROW 6.42 COL 45
     v_log_tarefa AT ROW 7.42 COL 13.72
     l-temp AT ROW 7.42 COL 45
     v_log_diarista AT ROW 8.42 COL 13.72
     l-tempoparc AT ROW 8.42 COL 45
     l-aprendiz AT ROW 9.46 COL 45.43
     v-data-aposent AT ROW 11.04 COL 42.29 COLON-ALIGNED
     bt-ok AT ROW 13.67 COL 3
     bt-cancelar AT ROW 13.67 COL 14
     bt-ajuda AT ROW 13.67 COL 65.29
     v-label-tipo-func AT ROW 2.42 COL 44.43 COLON-ALIGNED NO-LABEL
     RECT-19 AT ROW 1 COL 1.86
     "Categoria Salarial" VIEW-AS TEXT
          SIZE 17 BY .67 AT ROW 2.42 COL 11.72
     RECT-10 AT ROW 2.67 COL 10.72
     RECT-11 AT ROW 2.67 COL 44
     RECT-1 AT ROW 13.42 COL 1.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 14.17.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert Custom SmartWindow title>"
         HEIGHT             = 14.13
         WIDTH              = 75.72
         MAX-HEIGHT         = 27.96
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 27.96
         VIRTUAL-WIDTH      = 146.29
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


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-window 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-window.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
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
  apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
    for each tt-param:
       delete tt-param.
    end.
    create tt-param.
    assign tt-param.v-data-aposent    = input frame {&frame-name} v-data-aposent
           tt-param.log_mensal        = input frame {&frame-name} v_log_mensal
           tt-param.log_horista       = input frame {&frame-name} v_log_horista
           tt-param.log_semanal       = input frame {&frame-name} v_log_semanal
           tt-param.log_quinzenal     = input frame {&frame-name} v_log_quinzenal
           tt-param.log_tarefa        = input frame {&frame-name} v_log_tarefa
           tt-param.log_diarista      = input frame {&frame-name} v_log_diarista
           tt-param.l-func            = input frame {&frame-name} l-func
           tt-param.l-estag           = input frame {&frame-name} l-estag
           tt-param.l-aposent         = input frame {&frame-name} l-aposent
           tt-param.l-empreg          = input frame {&frame-name} l-empregador
           tt-param.l-prazo-determ    = input frame {&frame-name} l-temp
           tt-param.l-tempo-parcial   = input frame {&frame-name} l-tempoparc
           tt-param.l-aprendiz        = input frame {&frame-name} l-aprendiz.
    apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME l-aposent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL l-aposent w-window
ON VALUE-CHANGED OF l-aposent IN FRAME F-Main /* Aposentado */
DO:
  if  input frame {&frame-name} l-aposent = yes then
      assign v-data-aposent:sensitive in frame {&frame-name} = yes.
  else
      assign v-data-aposent:sensitive in frame {&frame-name} = no.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-window _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-window _ADM-ROW-AVAILABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-window _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-window _DEFAULT-ENABLE
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
  DISPLAY v_log_mensal l-func v_log_horista l-estag v_log_semanal l-aposent 
          v_log_quinzenal l-empregador v_log_tarefa l-temp v_log_diarista 
          l-tempoparc l-aprendiz v-data-aposent v-label-tipo-func 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-19 RECT-10 RECT-11 v_log_mensal l-func v_log_horista l-estag 
         v_log_semanal l-aposent v_log_quinzenal l-empregador v_log_tarefa 
         l-temp v_log_diarista l-tempoparc l-aprendiz v-data-aposent RECT-1 
         bt-ok bt-cancelar bt-ajuda v-label-tipo-func 
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

{utp/ut9000.i "ESFP1593A" "12.1.13.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
find first tt-param no-lock no-error.
if avail tt-param then do:
   assign v_log_mensal:checked            in frame {&FRAME-NAME} = tt-param.log_mensal
          v_log_horista:checked           in frame {&FRAME-NAME} = tt-param.log_horista
          v_log_semanal:checked           in frame {&FRAME-NAME} = tt-param.log_semanal
          v_log_quinzenal:checked         in frame {&FRAME-NAME} = tt-param.log_quinzenal
          v_log_tarefa:checked            in frame {&FRAME-NAME} = tt-param.log_tarefa
          v_log_diarista:checked          in frame {&FRAME-NAME} = tt-param.log_diarista
          l-func:checked                  in frame {&frame-name} = tt-param.l-func
          l-estag:checked                 in frame {&frame-name} = tt-param.l-estag
          l-aposent:checked               in frame {&frame-name} = tt-param.l-aposent
          l-aprendiz:checked              in frame {&frame-name} = tt-param.l-aprendiz
          l-empregador:checked            in frame {&frame-name} = tt-param.l-empreg.

   assign l-temp:checked              in frame {&frame-name} = tt-param.l-prazo-determ
          l-tempoparc:checked         in frame {&frame-name} = tt-param.l-tempo-parcial 
          v-data-aposent:screen-value in frame {&frame-name} = string(tt-param.v-data-aposent).
end.

apply "value-changed" to l-aposent in frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-window _ADM-SEND-RECORDS
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



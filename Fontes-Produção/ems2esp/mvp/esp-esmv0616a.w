&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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
{include/i-prgvrs.i esp-esmv0616a 0.12.00.000}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{mvp\esp-esmv0616.i1}

define input-output parameter table for ttSelecao.

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
&Scoped-Define ENABLED-OBJECTS RECT-1 IMAGE-01 IMAGE-2 IMAGE-5 IMAGE-6 ~
IMAGE-7 IMAGE-8 IMAGE-11 IMAGE-12 IMAGE-17 IMAGE-18 IMAGE-19 IMAGE-20 ~
IMAGE-1 IMAGE-21 RECT-3 fi-periodo-ini fi-periodo-fim fi-empresa-ini ~
fi-empresa-fim fi-equipto-ini fi-equipto-fim fi-estab-ini fi-estab-fim ~
fi-modelo-ini fi-modelo-fim fi-tag-ini fi-tag-fim fi-cc-ini fi-cc-fim bt-ok ~
bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-periodo-ini fi-periodo-fim ~
fi-empresa-ini fi-empresa-fim fi-equipto-ini fi-equipto-fim fi-estab-ini ~
fi-estab-fim fi-modelo-ini fi-modelo-fim fi-tag-ini fi-tag-fim fi-cc-ini ~
fi-cc-fim 

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

DEFINE VARIABLE fi-cc-fim AS CHARACTER FORMAT "X(08)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cc-ini AS CHARACTER FORMAT "X(08)":U 
     LABEL "Per¡odo" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-empresa-fim AS CHARACTER FORMAT "X(03)":U INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-empresa-ini AS CHARACTER FORMAT "X(03)":U 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-equipto-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-equipto-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Equipamento" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-estab-fim AS CHARACTER FORMAT "x(3)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-estab-ini AS CHARACTER FORMAT "x(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-modelo-fim AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-modelo-ini AS CHARACTER FORMAT "X(8)":U 
     LABEL "Modelo" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-periodo-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-periodo-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/50 
     LABEL "Per¡odo" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tag-fim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tag-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Tag" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-01
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-17
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-18
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-19
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-20
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-21
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

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 77.86 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 76.86 BY 8.13.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-periodo-ini AT ROW 2.33 COL 22.29 COLON-ALIGNED WIDGET-ID 2
     fi-periodo-fim AT ROW 2.33 COL 53.72 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fi-empresa-ini AT ROW 3.38 COL 22.29 COLON-ALIGNED WIDGET-ID 20
     fi-empresa-fim AT ROW 3.38 COL 53.72 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     fi-equipto-ini AT ROW 4.38 COL 22.29 COLON-ALIGNED WIDGET-ID 28
     fi-equipto-fim AT ROW 4.38 COL 53.72 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     fi-estab-ini AT ROW 5.42 COL 22.29 COLON-ALIGNED WIDGET-ID 88
     fi-estab-fim AT ROW 5.42 COL 53.72 COLON-ALIGNED NO-LABEL WIDGET-ID 86
     fi-modelo-ini AT ROW 6.46 COL 22.29 COLON-ALIGNED WIDGET-ID 44
     fi-modelo-fim AT ROW 6.46 COL 53.72 COLON-ALIGNED NO-LABEL WIDGET-ID 42
     fi-tag-ini AT ROW 7.46 COL 22.29 COLON-ALIGNED WIDGET-ID 66
     fi-tag-fim AT ROW 7.46 COL 53.72 COLON-ALIGNED NO-LABEL WIDGET-ID 68
     fi-cc-ini AT ROW 8.5 COL 22.29 COLON-ALIGNED WIDGET-ID 76
     fi-cc-fim AT ROW 8.5 COL 53.72 COLON-ALIGNED NO-LABEL WIDGET-ID 74
     bt-ok AT ROW 10.29 COL 3
     bt-cancelar AT ROW 10.29 COL 14
     bt-ajuda AT ROW 10.29 COL 69
     "Filtro:" VIEW-AS TEXT
          SIZE 5 BY .54 AT ROW 1.33 COL 12.72 WIDGET-ID 82
     RECT-1 AT ROW 10.08 COL 2
     IMAGE-01 AT ROW 2.33 COL 41.43 WIDGET-ID 6
     IMAGE-2 AT ROW 2.33 COL 52.14 WIDGET-ID 8
     IMAGE-5 AT ROW 3.38 COL 41.43 WIDGET-ID 22
     IMAGE-6 AT ROW 3.38 COL 52.14 WIDGET-ID 24
     IMAGE-7 AT ROW 4.38 COL 41.43 WIDGET-ID 30
     IMAGE-8 AT ROW 4.38 COL 52.14 WIDGET-ID 32
     IMAGE-11 AT ROW 6.46 COL 41.43 WIDGET-ID 46
     IMAGE-12 AT ROW 6.46 COL 52.14 WIDGET-ID 48
     IMAGE-17 AT ROW 7.46 COL 41.43 WIDGET-ID 70
     IMAGE-18 AT ROW 7.46 COL 52.14 WIDGET-ID 72
     IMAGE-19 AT ROW 8.5 COL 41.43 WIDGET-ID 78
     IMAGE-20 AT ROW 8.5 COL 52.14 WIDGET-ID 80
     IMAGE-1 AT ROW 5.42 COL 41.43 WIDGET-ID 90
     IMAGE-21 AT ROW 5.42 COL 52.14 WIDGET-ID 92
     RECT-3 AT ROW 1.63 COL 2.14 WIDGET-ID 94
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 10.75
         FONT 1 WIDGET-ID 100.


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
         HEIGHT             = 10.96
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
  RUN piGrava.
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
  DISPLAY fi-periodo-ini fi-periodo-fim fi-empresa-ini fi-empresa-fim 
          fi-equipto-ini fi-equipto-fim fi-estab-ini fi-estab-fim fi-modelo-ini 
          fi-modelo-fim fi-tag-ini fi-tag-fim fi-cc-ini fi-cc-fim 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-1 IMAGE-01 IMAGE-2 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 IMAGE-11 
         IMAGE-12 IMAGE-17 IMAGE-18 IMAGE-19 IMAGE-20 IMAGE-1 IMAGE-21 RECT-3 
         fi-periodo-ini fi-periodo-fim fi-empresa-ini fi-empresa-fim 
         fi-equipto-ini fi-equipto-fim fi-estab-ini fi-estab-fim fi-modelo-ini 
         fi-modelo-fim fi-tag-ini fi-tag-fim fi-cc-ini fi-cc-fim bt-ok 
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
  
  {utp/ut9000.i "esp-esmv0616a" "0.12.00.000"}
 
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  do with frame F-Main:
      find first ttSelecao no-lock no-error.
      if avail ttSelecao then do:
          assign fi-empresa-ini:screen-value    = string(ttSelecao.empresa-ini)
                 fi-empresa-fim:screen-value    = string(ttSelecao.empresa-fim)
                 fi-equipto-ini:screen-value    = ttSelecao.equipto-ini    
                 fi-equipto-fim:screen-value    = ttSelecao.equipto-fim    
/*                  fi-grupo-ini:screen-value      = ttSelecao.grupo-ini */
/*                  fi-grupo-fim:screen-value      = ttSelecao.grupo-fim */
                 fi-modelo-ini:screen-value     = ttSelecao.modelo-ini     
                 fi-modelo-fim:screen-value     = ttSelecao.modelo-fim     
                 fi-estab-ini:screen-value      = ttSelecao.estab-ini      
                 fi-estab-fim:screen-value      = ttSelecao.estab-fim      
/*                  fi-estrut-ini:screen-value     = ttSelecao.estrut-ini */
/*                  fi-estrut-fim:screen-value     = ttSelecao.estrut-fim */
/*                  fi-ano-fabric-ini:screen-value = string(ttSelecao.ano-fabric-ini) */
/*                  fi-ano-fabric-fim:screen-value = string(ttSelecao.ano-fabric-fim) */
                 fi-periodo-ini:screen-value    = string(ttSelecao.periodo-ini)  
                 fi-periodo-fim:screen-value    = string(ttSelecao.periodo-fim)
/*                  fi-dt-trans-ini:SCREEN-VALUE   = STRING(ttSelecao.dt-trans-ini) */
/*                  fi-dt-trans-fim:SCREEN-VALUE   = STRING(dt-trans-fim)           */
                 fi-tag-ini:screen-value in frame F-Main = ttSelecao.tag-ini
                 fi-tag-fim:screen-value in frame F-Main = ttSelecao.tag-fim
                 fi-cc-ini:screen-value in frame F-Main  = ttSelecao.cc-ini
                 fi-cc-fim:screen-value in frame F-Main  = ttSelecao.cc-fim.
      end.
  end.
  
  RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piGrava w-window 
PROCEDURE piGrava :
/*piGrava*/
  find first ttSelecao exclusive-lock no-error.
  if avail ttSelecao then do:
    assign ttSelecao.empresa-ini        =  fi-empresa-ini:screen-value in frame F-Main
           ttSelecao.empresa-fim        =  fi-empresa-fim:screen-value in frame F-Main
           ttSelecao.equipto-ini        =  fi-equipto-ini:screen-value in frame F-Main    
           ttSelecao.equipto-fim        =  fi-equipto-fim:screen-value in frame F-Main    
/*            ttSelecao.grupo-ini          =  fi-grupo-ini:screen-value in frame F-Main */
/*            ttSelecao.grupo-fim          =  fi-grupo-fim:screen-value in frame F-Main */
           ttSelecao.modelo-ini         =  fi-modelo-ini:screen-value in frame F-Main     
           ttSelecao.modelo-fim         =  fi-modelo-fim:screen-value in frame F-Main     
           ttSelecao.estab-ini          =  fi-estab-ini:screen-value in frame F-Main      
           ttSelecao.estab-fim          =  fi-estab-fim:screen-value in frame F-Main      
/*            ttSelecao.estrut-ini         =  fi-estrut-ini:screen-value in frame F-Main */
/*            ttSelecao.estrut-fim         =  fi-estrut-fim:screen-value in frame F-Main */
           ttSelecao.tag-ini            =  fi-tag-ini:screen-value in frame F-Main
           ttSelecao.tag-fim            =  fi-tag-fim:screen-value in frame F-Main
           ttSelecao.cc-ini             =  fi-cc-ini:screen-value in frame F-Main
           ttSelecao.cc-fim             =  fi-cc-fim:screen-value in frame F-Main
           ttSelecao.periodo-ini        =  date(fi-periodo-ini:screen-value in frame F-Main) 
           ttSelecao.periodo-fim        =  date(fi-periodo-fim:screen-value in frame F-Main)
/*            ttSelecao.dt-trans-ini       =  DATE(fi-dt-trans-ini:SCREEN-VALUE IN FRAME F-Main) */
/*            ttSelecao.dt-trans-fim       =  DATE(fi-dt-trans-fim:SCREEN-VALUE IN FRAME F-Main) */
/*            ttSelecao.ano-fabric-ini     =  int(fi-ano-fabric-ini:screen-value in frame F-Main) */
/*            ttSelecao.ano-fabric-fim     =  int(fi-ano-fabric-fim:screen-value in frame F-Main) */
           . 
  end.
  
  return "OK":U.
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


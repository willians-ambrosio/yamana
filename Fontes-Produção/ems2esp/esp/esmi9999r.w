&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-digita 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESMI9999R 12.00.00.001}

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> <m¢dulo>}
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE TEMP-TABLE tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field nr-ord-ini       as INTEGER
    field nr-ord-fim       as INTEGER
    FIELD equipamento-ini  AS CHARACTER
    FIELD equipamento-fim  AS CHARACTER
    FIELD dt-abertura-ini  AS DATE
    FIELD dt-abertura-fim  AS DATE
    FIELD dt-exec-ini      AS DATE
    FIELD dt-exec-fim      AS DATE
    FIELD status-ini       AS CHARACTER
    FIELD status-fim       AS CHARACTER
    FIELD tag-ini          AS CHARACTER
    FIELD tag-fim          AS CHARACTER
    /* Begins REV001 - inclus∆o equipe responsavel */
    FIELD cd-equip-res-ini AS CHARACTER
    FIELD cd-equip-res-fin AS CHARACTER.
    /* end REV001 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Digitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 ~
IMAGE-7 IMAGE-8 IMAGE-9 IMAGE-10 IMAGE-11 IMAGE-12 IMAGE-13 IMAGE-14 ~
RECT-17 IMAGE-15 IMAGE-16 fiNrOrdIni fiNrOrdFim fiEquipIni fiEquipFim ~
fiDtAberturaIni fiDtAberturaFim fiDtExecIni fiDtExecFim fiStatusIni ~
fiStatusFim fiTagIni fiTagFim fiEqResIni fiEqResFin bt-ajuda bt-ok ~
bt-cancelar 
&Scoped-Define DISPLAYED-OBJECTS fiNrOrdIni fiNrOrdFim fiEquipIni ~
fiEquipFim fiDtAberturaIni fiDtAberturaFim fiDtExecIni fiDtExecFim ~
fiStatusIni fiStatusFim fiTagIni fiTagFim fiEqResIni fiEqResFin 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-digita AS WIDGET-HANDLE NO-UNDO.

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

DEFINE VARIABLE fiDtAberturaFim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 9.72 BY .88 NO-UNDO.

DEFINE VARIABLE fiDtAberturaIni AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Data Abertura" 
     VIEW-AS FILL-IN 
     SIZE 9.72 BY .88 NO-UNDO.

DEFINE VARIABLE fiDtExecFim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 9.72 BY .88 NO-UNDO.

DEFINE VARIABLE fiDtExecIni AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Data Execuá∆o" 
     VIEW-AS FILL-IN 
     SIZE 9.72 BY .88 NO-UNDO.

DEFINE VARIABLE fiEqResFin AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12.14 BY .88 NO-UNDO.

DEFINE VARIABLE fiEqResIni AS CHARACTER FORMAT "x(8)" 
     LABEL "Equipe" 
     VIEW-AS FILL-IN 
     SIZE 12.14 BY .88 NO-UNDO.

DEFINE VARIABLE fiEquipFim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 16.29 BY .88 NO-UNDO.

DEFINE VARIABLE fiEquipIni AS CHARACTER FORMAT "X(16)":U 
     LABEL "Equipamento" 
     VIEW-AS FILL-IN 
     SIZE 15.72 BY .88 NO-UNDO.

DEFINE VARIABLE fiNrOrdFim AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE fiNrOrdIni AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Nr Ordem" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE fiStatusFim AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 9.72 BY .88 NO-UNDO.

DEFINE VARIABLE fiStatusIni AS CHARACTER FORMAT "X(8)":U 
     LABEL "Status" 
     VIEW-AS FILL-IN 
     SIZE 9.72 BY .88 NO-UNDO.

DEFINE VARIABLE fiTagFim AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 16.29 BY .88 NO-UNDO.

DEFINE VARIABLE fiTagIni AS CHARACTER FORMAT "X(16)":U 
     LABEL "TAG" 
     VIEW-AS FILL-IN 
     SIZE 15.72 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-11
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-12
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-13
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-15
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-16
     FILENAME "image\im-fir":U
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
     SIZE 62.72 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62.57 BY 7.88.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fiNrOrdIni AT ROW 1.79 COL 13.43 COLON-ALIGNED WIDGET-ID 44
     fiNrOrdFim AT ROW 1.79 COL 39 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     fiEquipIni AT ROW 2.79 COL 10.29 COLON-ALIGNED WIDGET-ID 4
     fiEquipFim AT ROW 2.79 COL 39 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     fiDtAberturaIni AT ROW 3.79 COL 16.29 COLON-ALIGNED WIDGET-ID 12
     fiDtAberturaFim AT ROW 3.79 COL 39 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     fiDtExecIni AT ROW 4.79 COL 16.29 COLON-ALIGNED WIDGET-ID 22
     fiDtExecFim AT ROW 4.79 COL 39 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     fiStatusIni AT ROW 5.79 COL 16.29 COLON-ALIGNED WIDGET-ID 28
     fiStatusFim AT ROW 5.79 COL 39 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     fiTagIni AT ROW 6.79 COL 10.29 COLON-ALIGNED WIDGET-ID 38
     fiTagFim AT ROW 6.79 COL 39 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     fiEqResIni AT ROW 7.75 COL 13.86 COLON-ALIGNED WIDGET-ID 54
     fiEqResFin AT ROW 7.75 COL 39 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     bt-ajuda AT ROW 9.29 COL 52.86
     bt-ok AT ROW 9.33 COL 2.14
     bt-cancelar AT ROW 9.33 COL 13.14
     RECT-1 AT ROW 9.13 COL 1.29
     IMAGE-3 AT ROW 1.88 COL 28.72 WIDGET-ID 46
     IMAGE-4 AT ROW 1.88 COL 37.72 WIDGET-ID 48
     IMAGE-5 AT ROW 2.88 COL 28.72 WIDGET-ID 6
     IMAGE-6 AT ROW 2.88 COL 37.72 WIDGET-ID 8
     IMAGE-7 AT ROW 3.92 COL 28.72 WIDGET-ID 14
     IMAGE-8 AT ROW 3.88 COL 37.72 WIDGET-ID 16
     IMAGE-9 AT ROW 4.92 COL 28.72 WIDGET-ID 24
     IMAGE-10 AT ROW 4.88 COL 37.72 WIDGET-ID 26
     IMAGE-11 AT ROW 5.88 COL 28.72 WIDGET-ID 30
     IMAGE-12 AT ROW 5.88 COL 37.72 WIDGET-ID 32
     IMAGE-13 AT ROW 6.88 COL 28.72 WIDGET-ID 40
     IMAGE-14 AT ROW 6.88 COL 37.72 WIDGET-ID 42
     RECT-17 AT ROW 1.13 COL 1.43 WIDGET-ID 50
     IMAGE-15 AT ROW 7.79 COL 37.72 WIDGET-ID 56
     IMAGE-16 AT ROW 7.79 COL 28.72 WIDGET-ID 58
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 12.58
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Digitacao
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-digita ASSIGN
         HIDDEN             = YES
         TITLE              = "Template de Digitaá∆o?"
         HEIGHT             = 9.63
         WIDTH              = 63.29
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-digita 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-digit.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-digita
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Template de Digitaá∆o? */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Template de Digitaá∆o? */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-digita
ON CHOOSE OF bt-ajuda IN FRAME F-Main /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-digita
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-digita
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
    EMPTY TEMP-TABLE tt-param.
    create tt-param.
    assign tt-param.usuario          = c-seg-usuario
           tt-param.data-exec        = today
           tt-param.hora-exec        = time
           tt-param.nr-ord-ini       = input frame F-Main fiNrOrdIni 
           tt-param.nr-ord-fim       = input frame F-Main fiNrOrdFim
           tt-param.equipamento-ini  = fiEquipIni:INPUT-VALUE IN FRAME F-Main
           tt-param.equipamento-fim  = fiEquipFim:INPUT-VALUE IN FRAME F-Main
           tt-param.dt-abertura-ini  = fiDtAberturaIni:INPUT-VALUE IN FRAME F-Main
           tt-param.dt-abertura-fim  = fiDtAberturaFim:INPUT-VALUE IN FRAME F-Main
           tt-param.dt-exec-ini      = fiDtExecIni:INPUT-VALUE IN FRAME F-Main
           tt-param.dt-exec-fim      = fiDtExecFim:INPUT-VALUE IN FRAME F-Main
           tt-param.status-ini       = fiStatusIni:INPUT-VALUE IN FRAME F-Main
           tt-param.status-fim       = fiStatusFim:INPUT-VALUE IN FRAME F-Main
           tt-param.tag-ini          = fiTagIni:INPUT-VALUE IN FRAME F-Main
           tt-param.tag-fim          = fiTagFim:INPUT-VALUE IN FRAME F-Main
           /* Begins REV001 - inclus∆o equipe responsavel */
           tt-param.cd-equip-res-ini = fiEqResIni:INPUT-VALUE IN FRAME F-Main 
           tt-param.cd-equip-res-fin = fiEqResFin:INPUT-VALUE IN FRAME F-Main. 
           /* End REV001 */

  RUN esp/esmi9999rrp.p( INPUT TABLE tt-param ).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-digita 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-digita  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-digita  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-digita  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
  THEN DELETE WIDGET w-digita.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-digita  _DEFAULT-ENABLE
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
  DISPLAY fiNrOrdIni fiNrOrdFim fiEquipIni fiEquipFim fiDtAberturaIni 
          fiDtAberturaFim fiDtExecIni fiDtExecFim fiStatusIni fiStatusFim 
          fiTagIni fiTagFim fiEqResIni fiEqResFin 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE RECT-1 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-7 IMAGE-8 IMAGE-9 
         IMAGE-10 IMAGE-11 IMAGE-12 IMAGE-13 IMAGE-14 RECT-17 IMAGE-15 IMAGE-16 
         fiNrOrdIni fiNrOrdFim fiEquipIni fiEquipFim fiDtAberturaIni 
         fiDtAberturaFim fiDtExecIni fiDtExecFim fiStatusIni fiStatusFim 
         fiTagIni fiTagFim fiEqResIni fiEqResFin bt-ajuda bt-ok bt-cancelar 
      WITH FRAME F-Main IN WINDOW w-digita.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-digita.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-digita 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-digita 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-digita 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  {utp/ut9000.i "ESMI9999R" "12.00.00.001"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  {include/i-inifld.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-digita  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this Digitacao, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-digita 
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


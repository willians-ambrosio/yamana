&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
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

Def Temp-table tt_user No-undo
    Field cod_usuario Like usuar_mestre.cod_usuario
    Field nom_usuario Like usuar_mestre.nom_usuario
    Index pk_tt_user  Is Primary Unique
          cod_usuario.

def new global shared var v_cod_dwb_program
    as character
    format "x(32)":U
    label "Programa"
    column-label "Programa"
    no-undo.

def new global shared var v_cod_usuar_corren
    as character
    format "x(12)":U
    label "Usu rio Corrente"
    column-label "Usu rio Corrente"
    no-undo.

Def New Global Shared Var v_cod_param As Int No-undo.

def new global shared var v_rec_usuar_mestre
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

Def Var l_alterou As Log No-undo.

{src/adm2/widgetprto.i}

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
&Scoped-define BROWSE-NAME br_user

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt_user

/* Definitions for BROWSE br_user                                       */
&Scoped-define FIELDS-IN-QUERY-br_user tt_user.cod_usuario tt_user.nom_usuario   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_user   
&Scoped-define SELF-NAME br_user
&Scoped-define QUERY-STRING-br_user FOR EACH tt_user
&Scoped-define OPEN-QUERY-br_user OPEN QUERY {&SELF-NAME} FOR EACH tt_user.
&Scoped-define TABLES-IN-QUERY-br_user tt_user
&Scoped-define FIRST-TABLE-IN-QUERY-br_user tt_user


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-br_user}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 RECT-4 fi_des_param ~
br_user bt_add_user bt_del_user bt_salvar bt_excluir bt_sair 
&Scoped-Define DISPLAYED-OBJECTS fi_cod_dwb_program fi_cod_param ~
fi_des_param 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt_add_user 
     LABEL "Adicionar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt_del_user 
     LABEL "Remover" 
     SIZE 10 BY 1.

DEFINE BUTTON bt_excluir 
     LABEL "Excluir" 
     SIZE 10 BY 1.

DEFINE BUTTON bt_sair 
     LABEL "Sair" 
     SIZE 10 BY 1.

DEFINE BUTTON bt_salvar 
     LABEL "Salvar" 
     SIZE 10 BY 1.

DEFINE VARIABLE fi_cod_dwb_program AS CHARACTER FORMAT "x(32)" 
     LABEL "Programa" 
     VIEW-AS FILL-IN 
     SIZE 34 BY .88 NO-UNDO.

DEFINE VARIABLE fi_cod_param AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "C¢d. Param" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .79 NO-UNDO.

DEFINE VARIABLE fi_des_param AS CHARACTER FORMAT "X(32)":U 
     LABEL "Descri‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 34 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY 2.75.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 49 BY 1.29
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY 1.5.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY 10.38.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_user FOR 
      tt_user SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_user
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_user wWin _FREEFORM
  QUERY br_user DISPLAY
      tt_user.cod_usuario
    tt_user.nom_usuario
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 41 BY 8.54
         FONT 1 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fi_cod_dwb_program AT ROW 1.67 COL 11 COLON-ALIGNED HELP
          "Programa" WIDGET-ID 2
     fi_cod_param AT ROW 2.75 COL 11 COLON-ALIGNED WIDGET-ID 24
     fi_des_param AT ROW 4.46 COL 11 COLON-ALIGNED WIDGET-ID 20
     br_user AT ROW 6.17 COL 6 WIDGET-ID 200
     bt_add_user AT ROW 14.88 COL 6 WIDGET-ID 28
     bt_del_user AT ROW 14.88 COL 16.43 WIDGET-ID 30
     bt_salvar AT ROW 16.58 COL 2.57 WIDGET-ID 10
     bt_excluir AT ROW 16.58 COL 12.86 WIDGET-ID 12
     bt_sair AT ROW 16.58 COL 40.57 WIDGET-ID 14
     RECT-1 AT ROW 1.25 COL 2 WIDGET-ID 6
     RECT-2 AT ROW 16.42 COL 2 WIDGET-ID 8
     RECT-3 AT ROW 4.17 COL 2 WIDGET-ID 16
     RECT-4 AT ROW 5.83 COL 2 WIDGET-ID 32
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 50.86 BY 16.96
         FONT 1 WIDGET-ID 100.


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
         TITLE              = "Manuten‡Æo de Parametriza‡Æo M£ltipla"
         HEIGHT             = 16.96
         WIDTH              = 50.86
         MAX-HEIGHT         = 30
         MAX-WIDTH          = 161.43
         VIRTUAL-HEIGHT     = 30
         VIRTUAL-WIDTH      = 161.43
         MAX-BUTTON         = no
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
/* BROWSE-TAB br_user fi_des_param fMain */
/* SETTINGS FOR FILL-IN fi_cod_dwb_program IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_cod_param IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       fi_des_param:HIDDEN IN FRAME fMain           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_user
/* Query rebuild information for BROWSE br_user
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt_user.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br_user */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Manuten‡Æo de Parametriza‡Æo M£ltipla */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Manuten‡Æo de Parametriza‡Æo M£ltipla */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_add_user
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_add_user wWin
ON CHOOSE OF bt_add_user IN FRAME fMain /* Adicionar */
DO:

    Assign l_alterou = Yes.

    run prgtec/sec/sec000ka.p /*prg_sea_usuar_mestre*/.

    if  v_rec_usuar_mestre <> ? then do:

        find usuar_mestre where recid(usuar_mestre) = v_rec_usuar_mestre no-lock no-error.

        If Not Can-find(First tt_user 
                        Where tt_user.cod_usuario = usuar_mestre.cod_usuario) Then Do:

            Create tt_user.

            Assign tt_user.cod_usuario = usuar_mestre.cod_usuario
                   tt_user.nom_usuario = usuar_mestre.nom_usuario.

            {&OPEN-QUERY-{&BROWSE-NAME}}

        End.
    
    end.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_del_user
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_del_user wWin
ON CHOOSE OF bt_del_user IN FRAME fMain /* Remover */
DO:

    Assign l_alterou = Yes.

    Def Var i_current_row As Int No-undo.

    Assign i_current_row = Current-result-row('br_user').

    If Avail tt_user Then Do:

        Delete tt_user.

    End.

    {&OPEN-QUERY-{&BROWSE-NAME}}

    br_user:Query:Reposition-to-row(i_current_row) In Frame fMain.

    If Avail tt_user Then
        br_user:Select-focused-row() In Frame fMain.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_excluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_excluir wWin
ON CHOOSE OF bt_excluir IN FRAME fMain /* Excluir */
DO:

    MESSAGE 'Esta opera‡Æo afetar  todos os usu rios relacionados a este Programa\Parƒmetro.' Skip
            'Deseja continuar?'
        VIEW-AS ALERT-BOX INFO BUTTONS Yes-no Update l_excluir As Log.

    If l_excluir Then Do:

        Run pi_excluir.

        Apply 'Close' To This-procedure.

        Return No-apply.

    End.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_sair wWin
ON CHOOSE OF bt_sair IN FRAME fMain /* Sair */
DO:

    If l_alterou Then Do:

        MESSAGE 'Deseja salvar as altera‡äes realizadas?'
            VIEW-AS ALERT-BOX INFO BUTTONS Yes-no Update l_salvar As Log.

        If l_salvar Then Do:

            Apply 'Choose' To bt_salvar.
            Return No-apply.

        End.

    End.

    If Not l_alterou Or Not l_salvar Then Do:

        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.

    End.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_salvar wWin
ON CHOOSE OF bt_salvar IN FRAME fMain /* Salvar */
DO:

    Run pi_salvar.

    Apply 'Close' To This-procedure.

    Return No-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_des_param
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_des_param wWin
ON LEAVE OF fi_des_param IN FRAME fMain /* Descri‡Æo */
DO:
  
    Assign l_alterou = (fi_des_param:Screen-value <> fi_des_param).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br_user
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

Run pi_iniciar.

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
  DISPLAY fi_cod_dwb_program fi_cod_param fi_des_param 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-1 RECT-2 RECT-3 RECT-4 fi_des_param br_user bt_add_user 
         bt_del_user bt_salvar bt_excluir bt_sair 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_excluir wWin 
PROCEDURE pi_excluir :
/*----------------------------------------------------------------------------*/

    Find First es_dwb_set_list_mult Exclusive-lock
         Where es_dwb_set_list_mult.cod_dwb_program = fi_cod_dwb_program
           And es_dwb_set_list_mult.cod_param       = fi_cod_param No-error.

    If Avail es_dwb_set_list_mult Then Do:

        Delete es_dwb_set_list_mult.

    End.

    For Each es_dwb_set_list_mult_user Exclusive-lock
       Where es_dwb_set_list_mult_user.cod_dwb_program = fi_cod_dwb_program
         And es_dwb_set_list_mult_user.cod_param       = fi_cod_param :

        Delete es_dwb_set_list_mult_user.

    End.

    For Each es_dwb_set_list Exclusive-lock
       Where es_dwb_set_list.cod_dwb_program = fi_cod_dwb_program
         And es_dwb_set_list.cod_param       = fi_cod_param :

        Delete es_dwb_set_list.

    End.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_iniciar wWin 
PROCEDURE pi_iniciar :
/*----------------------------------------------------------------------------*/

    Assign fi_cod_dwb_program = v_cod_dwb_program 
           fi_cod_param       = v_cod_param.

    Find First es_dwb_set_list_mult No-lock        
         Where es_dwb_set_list_mult.cod_dwb_program = fi_cod_dwb_program
           And es_dwb_set_list_mult.cod_param       = fi_cod_param No-error.

    If Avail es_dwb_set_list_mult Then
        Assign fi_des_param = es_dwb_set_list_mult.des_param.

    Empty Temp-table tt_user.

    For Each es_dwb_set_list_mult_user No-lock
       Where es_dwb_set_list_mult_user.cod_dwb_program = fi_cod_dwb_program
         And es_dwb_set_list_mult_user.cod_param       = fi_cod_param :

        Find First usuar_mestre No-lock
             Where usuar_mestre.cod_usuario = es_dwb_set_list_mult_user.cod_dwb_user No-error.

        Create tt_user.

        Assign tt_user.cod_usuario = es_dwb_set_list_mult_user.cod_dwb_user
               tt_user.nom_usuario = If Avail usuar_mestre 
                                     Then usuar_mestre.nom_usuario
                                     Else ''.

    End.

    {&OPEN-QUERY-{&BROWSE-NAME}}

    Assign l_alterou = No.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_salvar wWin 
PROCEDURE pi_salvar :
/*----------------------------------------------------------------------------*/

    Find First es_dwb_set_list_mult Exclusive-lock
         Where es_dwb_set_list_mult.cod_dwb_program = fi_cod_dwb_program
           And es_dwb_set_list_mult.cod_param       = fi_cod_param No-error.

    If Avail es_dwb_set_list_mult Then Do:

        Assign es_dwb_set_list_mult.des_param       = Input Frame fMain fi_des_param
               es_dwb_set_list_mult.dat_alteracao   = Today
               es_dwb_set_list_mult.hra_alteracao   = String(Time, 'hh:mm:ss').

    End.

    For Each es_dwb_set_list_mult_user Exclusive-lock
       Where es_dwb_set_list_mult.cod_dwb_program = fi_cod_dwb_program
         And es_dwb_set_list_mult.cod_param       = fi_cod_param :

        If Not Can-find(First tt_user
                        Where tt_user.cod_usuario = es_dwb_set_list_mult_user.cod_dwb_user) Then Do:

            Delete es_dwb_set_list_mult_user.

        End.

    End.

    For Each tt_user :

        If Not Can-find(First es_dwb_set_list_mult_user 
                        Where es_dwb_set_list_mult_user.cod_dwb_program = fi_cod_dwb_program
                          And es_dwb_set_list_mult_user.cod_param       = fi_cod_param 
                          And es_dwb_set_list_mult_user.cod_dwb_user    = tt_user.cod_usuario) Then Do:

            Create es_dwb_set_list_mult_user.
            Assign es_dwb_set_list_mult_user.cod_dwb_program = fi_cod_dwb_program 
                   es_dwb_set_list_mult_user.cod_param       = fi_cod_param       
                   es_dwb_set_list_mult_user.cod_dwb_user    = tt_user.cod_usuario.

        End.

    End.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


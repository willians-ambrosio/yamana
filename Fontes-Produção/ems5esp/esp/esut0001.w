&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
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

Def Temp-table tt_dwb_set_list           No-undo Like dwb_set_list.
Def Temp-table tt_dwb_set_list_param     No-undo Like dwb_set_list_param.
Def Temp-table tt_dwb_set_list_param_aux No-undo Like dwb_set_list_param_aux.

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

Def Var c_cod_program     As Char No-undo.
Def Var c_lst_des_program As Char No-undo Init 'tar_apuracao_variac_econ'.
Def Var c_lst_cod_program As Char No-undo Init 'prgfin/fgl/fgl707aa.p'.

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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 bt_carregar bt_alterar ~
bt_sair 
&Scoped-Define DISPLAYED-OBJECTS fi_cod_dwb_program fi_cod_dwb_user 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt_alterar 
     LABEL "Alterar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt_carregar 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE BUTTON bt_sair 
     LABEL "&Sair" 
     SIZE 10 BY 1.

DEFINE VARIABLE cb_param AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Parƒmetro" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "",1
     DROP-DOWN-LIST
     SIZE 34.29 BY 1 NO-UNDO.

DEFINE VARIABLE fi_cod_dwb_program AS CHARACTER FORMAT "x(32)" 
     LABEL "Programa" 
     VIEW-AS FILL-IN 
     SIZE 34 BY .88 NO-UNDO.

DEFINE VARIABLE fi_cod_dwb_user AS CHARACTER FORMAT "x(21)" 
     LABEL "Usu rio" 
     VIEW-AS FILL-IN 
     SIZE 23 BY .88 NO-UNDO.

DEFINE VARIABLE fi_param AS CHARACTER FORMAT "X(32)":U 
     LABEL "Parƒmetro" 
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
     SIZE 49 BY 1.88.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     fi_cod_dwb_program AT ROW 1.67 COL 10.72 COLON-ALIGNED HELP
          "Programa" WIDGET-ID 2
     fi_cod_dwb_user AT ROW 2.67 COL 10.72 COLON-ALIGNED HELP
          "Usu rio" WIDGET-ID 4
     cb_param AT ROW 4.67 COL 10.72 COLON-ALIGNED WIDGET-ID 18
     fi_param AT ROW 4.67 COL 10.72 COLON-ALIGNED WIDGET-ID 20
     bt_carregar AT ROW 6.42 COL 2.57 WIDGET-ID 10
     bt_alterar AT ROW 6.42 COL 12.86 WIDGET-ID 22
     bt_sair AT ROW 6.42 COL 40.57 WIDGET-ID 14
     RECT-1 AT ROW 1.25 COL 2 WIDGET-ID 6
     RECT-2 AT ROW 6.25 COL 2 WIDGET-ID 8
     RECT-3 AT ROW 4.17 COL 2 WIDGET-ID 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 50.86 BY 6.75
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Parametriza‡Æo M£ltipla"
         HEIGHT             = 6.75
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
/* SETTINGS FOR COMBO-BOX cb_param IN FRAME fMain
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN fi_cod_dwb_program IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_cod_dwb_user IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_param IN FRAME fMain
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi_param:HIDDEN IN FRAME fMain           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Parametriza‡Æo M£ltipla */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Parametriza‡Æo M£ltipla */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_alterar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_alterar wWin
ON CHOOSE OF bt_alterar IN FRAME fMain /* Alterar */
DO:

    If cb_param:Screen-value = '' Then Do:

        MESSAGE 'Selecione um parƒmetro!'
            VIEW-AS ALERT-BOX Error BUTTONS OK.

        Apply 'Entry' To cb_param. 
        Return No-apply.

    End.
  
    Run esp/esut0003.w.

    Run pi_iniciar.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_carregar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_carregar wWin
ON CHOOSE OF bt_carregar IN FRAME fMain /* OK */
DO:

    If bt_carregar:Label = 'OK' Then Do:

        Run pi_carregar.

    End.
    Else Do:

        Run pi_salvar.

    End.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_sair wWin
ON CHOOSE OF bt_sair IN FRAME fMain /* Sair */
DO:
  
    Apply 'Close' To This-procedure.

    Return No-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb_param
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb_param wWin
ON VALUE-CHANGED OF cb_param IN FRAME fMain /* Parƒmetro */
DO:
  
    Assign v_cod_param = Input Frame fMain cb_param.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Run pi_iniciar. */

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
  DISPLAY fi_cod_dwb_program fi_cod_dwb_user 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-1 RECT-2 RECT-3 bt_carregar bt_alterar bt_sair 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  Run pi_iniciar.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_carregar wWin 
PROCEDURE pi_carregar :
/*----------------------------------------------------------------------------*/

    Def Var l_alterou As Log No-undo.

    If cb_param:Screen-value In Frame fMain <> ? Then Do:

        Run pi_limpar. 
        
        For Each es_dwb_set_list
           Where es_dwb_set_list.cod_dwb_program = Input Frame fMain fi_cod_dwb_program
             And es_dwb_set_list.cod_param       = Input Frame fMain cb_param :
    
            Case es_dwb_set_list.nom_tabela :
    
                When 'dwb_set_list' Then Do:
    
                    Empty Temp-table tt_dwb_set_list.
    
                    Create tt_dwb_set_list.
    
                    Raw-transfer es_dwb_set_list.raw_tabela 
                              To tt_dwb_set_list.
    
                    Find dwb_set_list Exclusive-lock
                         Where dwb_set_list.cod_dwb_program = Input Frame fMain fi_cod_dwb_program
                           And dwb_set_list.cod_dwb_user    = Input Frame fMain fi_cod_dwb_user  
                           And dwb_set_list.num_dwb_order   = es_dwb_set_list.num_dwb_order No-error.
            
                    If Not Avail dwb_set_list Then Do:
            
                        Create dwb_set_list.
            
                        Assign dwb_set_list.cod_dwb_program = Input Frame fMain fi_cod_dwb_program
                               dwb_set_list.cod_dwb_user    = Input Frame fMain fi_cod_dwb_user
                               dwb_set_list.num_dwb_order   = es_dwb_set_list.num_dwb_order.   
            
                    End.
    
                    Buffer-copy tt_dwb_set_list
                         Except cod_dwb_program 
                                cod_dwb_user
                                num_dwb_order
                             To dwb_set_list.
    
                End.
                When 'dwb_set_list_param' Then Do:
    
                    Empty Temp-table tt_dwb_set_list_param.
    
                    Create tt_dwb_set_list_param.
    
                    Raw-transfer es_dwb_set_list.raw_tabela 
                              To tt_dwb_set_list_param.
    
                    Find dwb_set_list_param Exclusive-lock
                         Where dwb_set_list_param.cod_dwb_program = Input Frame fMain fi_cod_dwb_program
                           And dwb_set_list_param.cod_dwb_user    = Input Frame fMain fi_cod_dwb_user    No-error.
            
                    If Not Avail dwb_set_list_param Then Do:
            
                        Create dwb_set_list_param.
            
                        Assign dwb_set_list_param.cod_dwb_program = Input Frame fMain fi_cod_dwb_program
                               dwb_set_list_param.cod_dwb_user    = Input Frame fMain fi_cod_dwb_user.   
            
                    End.
    
                    Buffer-copy tt_dwb_set_list_param
                         Except cod_dwb_program 
                                cod_dwb_user
                             To dwb_set_list_param.
    
                End.
                When 'dwb_set_list_param_aux' Then Do:
    
                    Empty Temp-table tt_dwb_set_list_param_aux.
    
                    Create tt_dwb_set_list_param_aux.
    
                    Raw-transfer es_dwb_set_list.raw_tabela 
                              To tt_dwb_set_list_param_aux.
    
                    Find dwb_set_list_param_aux Exclusive-lock
                         Where dwb_set_list_param_aux.cod_dwb_program = Input Frame fMain fi_cod_dwb_program
                           And dwb_set_list_param_aux.cod_dwb_user    = Input Frame fMain fi_cod_dwb_user  
                           And dwb_set_list_param_aux.num_dwb_order   = es_dwb_set_list.num_dwb_order No-error.
            
                    If Not Avail dwb_set_list_param_aux Then Do:
            
                        Create dwb_set_list_param_aux.
            
                        Assign dwb_set_list_param_aux.cod_dwb_program = Input Frame fMain fi_cod_dwb_program
                               dwb_set_list_param_aux.cod_dwb_user    = Input Frame fMain fi_cod_dwb_user
                               dwb_set_list_param_aux.num_dwb_order   = es_dwb_set_list.num_dwb_order.   
            
                    End.
    
                    Buffer-copy tt_dwb_set_list_param_aux
                         Except cod_dwb_program 
                                cod_dwb_user
                                num_dwb_order
                             To dwb_set_list_param_aux.
    
                End.
    
            End.
    
        End.
    
    End.

    Assign wWin:Visible = No.
  
    Run Value(c_cod_program).

    If cb_param:Screen-value <> ? Then Do:

        Run pi_comparar(Output l_alterou).

    End.

    If cb_param:Screen-value = ? Or l_alterou Then Do:

        MESSAGE 'Deseja salvar os parƒmetros utilizados?'
            VIEW-AS ALERT-BOX INFO BUTTONS Yes-no Update l_salvar As Log.

        If l_salvar Then Do With Frame fMain:

            Assign wWin:Visible          = Yes
                   cb_param:Visible      = No 
                   fi_param:Visible      = Yes
                   fi_param:Sensitive    = Yes
                   fi_param:Screen-value = ''
                   bt_alterar:Sensitive  = No
                   bt_carregar:Label     = 'Salvar' No-error.

        End.
        Else Do:

            Apply 'Close' To This-procedure.
            Return No-apply.

        End.

    End.
    Else Do:

        Apply 'Close' To This-procedure.
        Return No-apply.

    End.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_comparar wWin 
PROCEDURE pi_comparar :
/*----------------------------------------------------------------------------*/

    Def Output Param p_alterou As Log No-undo Init No.

    Def Var c_comparacao As Char No-undo.

    For Each dwb_set_list No-lock
       Where dwb_set_list.cod_dwb_program = Input Frame fMain fi_cod_dwb_program
         And dwb_set_list.cod_dwb_user    = Input Frame fMain fi_cod_dwb_user :

        Empty Temp-table tt_dwb_set_list.

        Assign c_comparacao = ''.

        Find First es_dwb_set_list No-lock                                                      
             Where es_dwb_set_list.cod_dwb_program = Input Frame fMain fi_cod_dwb_program
               And es_dwb_set_list.cod_param       = Input Frame fMain cb_param 
               And es_dwb_set_list.nom_tabela      = 'dwb_set_list'
               And es_dwb_set_list.num_dwb_order   = dwb_set_list.num_dwb_order No-error.

        If Avail es_dwb_set_list Then Do:

            Create tt_dwb_set_list.

            Raw-transfer es_dwb_set_list.raw_tabela 
                      To tt_dwb_set_list.

            Buffer-compare tt_dwb_set_list
                    Except cod_dwb_program 
                           cod_dwb_user
                           num_dwb_order
                        To dwb_set_list 
                      Save Result In c_comparacao.

            If Not p_alterou Then
                Assign p_alterou = (c_comparacao <> '').

        End.
        Else Do:

            Assign p_alterou = Yes.

        End.

    End.

    Find dwb_set_list_param No-lock
         Where dwb_set_list_param.cod_dwb_program = Input Frame fMain fi_cod_dwb_program
           And dwb_set_list_param.cod_dwb_user    = Input Frame fMain fi_cod_dwb_user No-error.                         

    If Avail dwb_set_list_param Then Do:

        Empty Temp-table tt_dwb_set_list_param.

        Assign c_comparacao = ''.

        Find First es_dwb_set_list No-lock                                                      
             Where es_dwb_set_list.cod_dwb_program = Input Frame fMain fi_cod_dwb_program
               And es_dwb_set_list.cod_param       = Input Frame fMain cb_param                       
               And es_dwb_set_list.nom_tabela      = 'dwb_set_list_param' No-error.

        If Avail es_dwb_set_list Then Do:

            Create tt_dwb_set_list_param.

            Raw-transfer es_dwb_set_list.raw_tabela 
                      To tt_dwb_set_list_param.

            Buffer-compare tt_dwb_set_list_param
                    Except cod_dwb_program 
                           cod_dwb_user
                        To dwb_set_list_param 
                      Save Result In c_comparacao.

            If Not p_alterou Then
                Assign p_alterou = (c_comparacao <> '').

        End.
        Else Do:

            Assign p_alterou = Yes.

        End.


    End.

    For Each dwb_set_list_param_aux No-lock
       Where dwb_set_list_param_aux.cod_dwb_program = Input Frame fMain fi_cod_dwb_program
         And dwb_set_list_param_aux.cod_dwb_user    = Input Frame fMain fi_cod_dwb_user :

        Empty Temp-table tt_dwb_set_list_param_aux.

        Assign c_comparacao = ''.

        Find First es_dwb_set_list No-lock                                                      
             Where es_dwb_set_list.cod_dwb_program = Input Frame fMain fi_cod_dwb_program
               And es_dwb_set_list.cod_param       = Input Frame fMain cb_param 
               And es_dwb_set_list.nom_tabela      = 'dwb_set_list_param_aux'
               And es_dwb_set_list.num_dwb_order   = dwb_set_list_param_aux.num_dwb_order No-error.

        If Avail es_dwb_set_list Then Do:

            Create tt_dwb_set_list_param_aux.

            Raw-transfer es_dwb_set_list.raw_tabela 
                      To tt_dwb_set_list_param_aux.

            Buffer-compare tt_dwb_set_list_param_aux
                    Except cod_dwb_program 
                           cod_dwb_user
                           num_dwb_order
                        To dwb_set_list_param_aux 
                      Save Result In c_comparacao.

            If Not p_alterou Then
                Assign p_alterou = (c_comparacao <> '').

        End.
        Else Do:

            Assign p_alterou = Yes.

        End.

    End.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_iniciar wWin 
PROCEDURE pi_iniciar :
/*----------------------------------------------------------------------------*/

    Def Var c_lst_params As Char No-undo.

    If fi_cod_dwb_program = '' Or fi_cod_dwb_user = '' Then
        Assign fi_cod_dwb_program = v_cod_dwb_program 
               fi_cod_dwb_user    = v_cod_usuar_corren.

    If Lookup(v_cod_dwb_program, c_lst_des_program) > 0 Then 
        Assign c_cod_program = Entry(Lookup(v_cod_dwb_program, c_lst_des_program), c_lst_cod_program) No-error.   

    If c_cod_program = '' Then Do:

        MESSAGE 'Este programa nÆo pode ser executado diretamente! Deve ser chamado a partir do ESUT0002.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        Apply 'Close' To This-procedure.
        Return No-apply.

    End.

    Assign cb_param:List-item-pairs In Frame fMain = cb_param:List-item-pairs.    

    For Each es_dwb_set_list_mult_user No-lock
       Where es_dwb_set_list_mult_user.cod_dwb_program = fi_cod_dwb_program
         And es_dwb_set_list_mult_user.cod_dwb_user    = fi_cod_dwb_user :

        Find First es_dwb_set_list_mult No-lock
             Where es_dwb_set_list_mult.cod_dwb_program = fi_cod_dwb_program
               And es_dwb_set_list_mult.cod_param       = es_dwb_set_list_mult_user.cod_param No-error.

        If Not Avail es_dwb_set_list_mult Then Next.

        If c_lst_params <> '' Then Assign c_lst_params = c_lst_params + ','.

        Assign c_lst_params = c_lst_params + es_dwb_set_list_mult.des_param + ',' +
                                             String(es_dwb_set_list_mult_user.cod_param). 

    End.

    Disp fi_cod_dwb_program
         fi_cod_dwb_user   
         With Frame fMain In Window wWin.

    If c_lst_params <> '' Then Do:

        Assign cb_param:List-item-pairs In Frame fMain = c_lst_params.

        Assign cb_param:Sensitive = Yes.

    End.
    Else Do:

        Apply 'Choose' To bt_carregar.

    End.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_limpar wWin 
PROCEDURE pi_limpar :
/*----------------------------------------------------------------------------*/

    For Each dwb_set_list Exclusive-lock
       Where dwb_set_list.cod_dwb_program = Input Frame fMain fi_cod_dwb_program
         And dwb_set_list.cod_dwb_user    = Input Frame fMain fi_cod_dwb_user :

        Delete dwb_set_list.

    End.

    For Each dwb_set_list_param_aux Exclusive-lock
       Where dwb_set_list_param_aux.cod_dwb_program = Input Frame fMain fi_cod_dwb_program
         And dwb_set_list_param_aux.cod_dwb_user    = Input Frame fMain fi_cod_dwb_user :

        Delete dwb_set_list_param_aux.

    End.

    Find dwb_set_list_param Exclusive-lock
         Where dwb_set_list_param.cod_dwb_program = Input Frame fMain fi_cod_dwb_program
           And dwb_set_list_param.cod_dwb_user    = Input Frame fMain fi_cod_dwb_user No-error.                         

    If Avail dwb_set_list_param Then Do:

        Delete dwb_set_list_param.

    End.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_salvar wWin 
PROCEDURE pi_salvar :
/*----------------------------------------------------------------------------*/

    Def Var i_cod_param As Int No-undo.

    If fi_param:Screen-value In Frame fMain = '' Then Do:

        MESSAGE 'A Descri‡Æo do Parƒmetro deve ser informada!'
            VIEW-AS ALERT-BOX Error BUTTONS OK.

        Apply 'Entry' To fi_param.
        Return No-apply.

    End.

    Find Last es_dwb_set_list_mult 
        Where es_dwb_set_list_mult.cod_dwb_program = Input Frame fMain fi_cod_dwb_program No-error.

    If Avail es_dwb_set_list_mult Then 
        Assign i_cod_param = es_dwb_set_list_mult.cod_param + 1.
    Else 
        Assign i_cod_param = 1.

    Assign v_cod_param = i_cod_param.

    Create es_dwb_set_list_mult.
    Assign es_dwb_set_list_mult.cod_dwb_program = Input Frame fMain fi_cod_dwb_program
           es_dwb_set_list_mult.cod_param       = i_cod_param
           es_dwb_set_list_mult.des_param       = fi_param:Screen-value In Frame fMain
           es_dwb_set_list_mult.dat_alteracao   = Today
           es_dwb_set_list_mult.hra_alteracao   = String(Time, 'hh:mm:ss').

    Create es_dwb_set_list_mult_user.
    Assign es_dwb_set_list_mult_user.cod_dwb_program = Input Frame fMain fi_cod_dwb_program
           es_dwb_set_list_mult_user.cod_param       = i_cod_param
           es_dwb_set_list_mult_user.cod_dwb_user    = Input Frame fMain fi_cod_dwb_user.

    For Each dwb_set_list No-lock
       Where dwb_set_list.cod_dwb_program = Input Frame fMain fi_cod_dwb_program
         And dwb_set_list.cod_dwb_user    = Input Frame fMain fi_cod_dwb_user :

        Create es_dwb_set_list.
            
        Assign es_dwb_set_list.cod_dwb_program = Input Frame fMain fi_cod_dwb_program
               es_dwb_set_list.cod_param       = i_cod_param                         
               es_dwb_set_list.nom_tabela      = 'dwb_set_list'
               es_dwb_set_list.num_dwb_order   = dwb_set_list.num_dwb_order.

        Raw-transfer dwb_set_list
                  To es_dwb_set_list.raw_tabela.

    End.

    Find dwb_set_list_param No-lock
         Where dwb_set_list_param.cod_dwb_program = Input Frame fMain fi_cod_dwb_program
           And dwb_set_list_param.cod_dwb_user    = Input Frame fMain fi_cod_dwb_user No-error.                         

    If Avail dwb_set_list_param Then Do:

        Create es_dwb_set_list.
            
        Assign es_dwb_set_list.cod_dwb_program = Input Frame fMain fi_cod_dwb_program
               es_dwb_set_list.cod_param       = i_cod_param                         
               es_dwb_set_list.nom_tabela      = 'dwb_set_list_param'.

        Raw-transfer dwb_set_list_param
                  To es_dwb_set_list.raw_tabela.

    End.

    For Each dwb_set_list_param_aux No-lock
       Where dwb_set_list_param_aux.cod_dwb_program = Input Frame fMain fi_cod_dwb_program
         And dwb_set_list_param_aux.cod_dwb_user    = Input Frame fMain fi_cod_dwb_user :

        Create es_dwb_set_list.
            
        Assign es_dwb_set_list.cod_dwb_program = Input Frame fMain fi_cod_dwb_program
               es_dwb_set_list.cod_param       = i_cod_param                         
               es_dwb_set_list.nom_tabela      = 'dwb_set_list_param_aux'
               es_dwb_set_list.num_dwb_order   = dwb_set_list_param_aux.num_dwb_order.

        Raw-transfer dwb_set_list_param_aux
                  To es_dwb_set_list.raw_tabela.

    End.

    Assign cb_param:Visible     = Yes 
           fi_param:Visible     = No
           fi_param:Sensitive   = No
           bt_alterar:Sensitive = Yes
           bt_carregar:Label    = 'OK' No-error.

    Run pi_iniciar.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


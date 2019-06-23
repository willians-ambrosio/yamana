&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

/*****************************************************************************
** Programa..............: esjd001
** Descricao.............: Carga planilha JDE
** Versao................: 1.00.00.000
** Procedimento..........: 
** Nome Externo..........: 
** Criado por............: Bruno Bertulli (DSC)
** Criado em.............: 03/06/2014
*****************************************************************************/

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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
def new global shared var v_cod_usuar_corren like usuar_mestre.cod_usuario no-undo.

def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.

def var v_cod_dwb_file
    as character
    format "x(40)":U
    label "Arquivo"
    column-label "Arquivo"
    no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f_main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-busca-arq rt_rgf rt_cxcf RECT-12 RECT-13 ~
rs-saida fi-arq-jde bt-busca-arq-2 fi-arquivo bt_executar bt_fechar ~
bt_cancelar Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS rs-saida fi-arq-jde fi-arquivo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-busca-arq 
     IMAGE-UP FILE "image/im-sea1.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-sea1.bmp":U NO-FOCUS
     LABEL "Busca Arquivo" 
     SIZE 4 BY 1.13 TOOLTIP "Busca Arquivo".

DEFINE BUTTON bt-busca-arq-2 
     IMAGE-UP FILE "image/im-sea1.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-sea1.bmp":U NO-FOCUS
     LABEL "Busca Arquivo" 
     SIZE 4 BY 1.13 TOOLTIP "Busca Arquivo".

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt_cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt_executar 
     IMAGE-UP FILE "image/im-run.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Executar" 
     SIZE 4 BY 1.13 TOOLTIP "Executar".

DEFINE BUTTON bt_fechar AUTO-GO 
     LABEL "Fechar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-arq-jde AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arquivo JDE" 
     VIEW-AS FILL-IN 
     SIZE 59 BY .88 NO-UNDO.

DEFINE VARIABLE fi-arquivo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arquivo Saida" 
     VIEW-AS FILL-IN 
     SIZE 59 BY .88 NO-UNDO.

DEFINE VARIABLE rs-saida AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "On-Line", 1,
"Batch", 2
     SIZE 27 BY 1 TOOLTIP "Saida" NO-UNDO.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 80 BY 1.75.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 80 BY 1.71.

DEFINE RECTANGLE rt_cxcf
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80 BY 1.42.

DEFINE RECTANGLE rt_rgf
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80 BY 1.5
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f_main
     bt-busca-arq AT ROW 3.38 COL 73.29
     rs-saida AT ROW 1.25 COL 30 NO-LABEL
     fi-arq-jde AT ROW 3.42 COL 12 COLON-ALIGNED
     bt-busca-arq-2 AT ROW 13.79 COL 74
     fi-arquivo AT ROW 14 COL 12.72 COLON-ALIGNED
     bt_executar AT ROW 1.17 COL 1.86
     bt_fechar AT ROW 15.71 COL 3
     bt_cancelar AT ROW 15.71 COL 13.86
     Btn_Help AT ROW 15.71 COL 70.43
     rt_rgf AT ROW 1 COL 1
     rt_cxcf AT ROW 15.5 COL 1
     RECT-12 AT ROW 3.08 COL 1
     RECT-13 AT ROW 13.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16
         FONT 1
         CANCEL-BUTTON bt_cancelar.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = ".: Importa planilha JDE - ESJD001 :."
         HEIGHT             = 16
         WIDTH              = 80
         MAX-HEIGHT         = 42.42
         MAX-WIDTH          = 274.29
         VIRTUAL-HEIGHT     = 42.42
         VIRTUAL-WIDTH      = 274.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f_main
   FRAME-NAME                                                           */
ASSIGN 
       bt-busca-arq:PRIVATE-DATA IN FRAME f_main     = 
                "Erase".

ASSIGN 
       bt-busca-arq-2:PRIVATE-DATA IN FRAME f_main     = 
                "Erase".

ASSIGN 
       bt_executar:PRIVATE-DATA IN FRAME f_main     = 
                "Erase".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* .: Importa planilha JDE - ESJD001 :. */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* .: Importa planilha JDE - ESJD001 :. */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-busca-arq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-busca-arq C-Win
ON CHOOSE OF bt-busca-arq IN FRAME f_main /* Busca Arquivo */
DO:
    system-dialog GET-FILE v_cod_dwb_file
        title "Importar" /*l_importar*/ 
        filters '*.txt,*csv'  '*.txt,*csv'
        save-as
        create-test-file.
    assign fi-arq-jde:screen-value in frame {&FRAME-NAME} = v_cod_dwb_file.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-busca-arq-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-busca-arq-2 C-Win
ON CHOOSE OF bt-busca-arq-2 IN FRAME f_main /* Busca Arquivo */
DO:
    DEFINE VARIABLE l-ok AS LOGICAL     NO-UNDO.

    assign input frame {&FRAME-NAME} fi-arquivo.
    SYSTEM-DIALOG GET-FILE fi-arquivo
       FILTERS "*.txt" "*.txt",
               "*.*" "*.*"
       ASK-OVERWRITE
       DEFAULT-EXTENSION "*.txt"
       INITIAL-DIR "spool"
       SAVE-AS
       USE-FILENAME
       UPDATE l-ok.
    if  l-ok = yes then
        display fi-arquivo with frame {&FRAME-NAME}.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME f_main /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_cancelar C-Win
ON CHOOSE OF bt_cancelar IN FRAME f_main /* Cancelar */
DO:
  apply "CLOSE" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_executar C-Win
ON CHOOSE OF bt_executar IN FRAME f_main /* Executar */
DO:
    do on error undo, return no-apply:
        run pi_executar.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_fechar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_fechar C-Win
ON CHOOSE OF bt_fechar IN FRAME f_main /* Fechar */
DO:
    apply "CLOSE" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    {include/i_dbinst.i}
    {include/i_dbtype.i}
    {include/i_fcldef.i}

    {include/i_fclwin.i c-win}
    {include/i_fclfrm.i f_main }

  RUN enable_UI.
  
  run pi_inicio.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY rs-saida fi-arq-jde fi-arquivo 
      WITH FRAME f_main IN WINDOW C-Win.
  ENABLE bt-busca-arq rt_rgf rt_cxcf RECT-12 RECT-13 rs-saida fi-arq-jde 
         bt-busca-arq-2 fi-arquivo bt_executar bt_fechar bt_cancelar Btn_Help 
      WITH FRAME f_main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f_main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_executar C-Win 
PROCEDURE pi_executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
Def Var v_num_ped_exec  As Int No-undo.

FIND FIRST dwb_rpt_param EXCLUSIVE-LOCK
     WHERE dwb_rpt_param.cod_dwb_program = "esjd001rp"
     AND   dwb_rpt_param.cod_dwb_user    = v_cod_usuar_corren No-error.
IF NOT AVAIL dwb_rpt_param THEN DO:
    CREATE dwb_rpt_param.
    ASSIGN dwb_rpt_param.cod_dwb_program = "esjd001rp"
           dwb_rpt_param.cod_dwb_user    = v_cod_usuar_corren.
END.

ASSIGN dwb_rpt_param.cod_dwb_parameters = INPUT FRAME {&FRAME-NAME} fi-arq-jde
       dwb_rpt_param.cod_dwb_parameters = dwb_rpt_param.cod_dwb_parameters + ";" + INPUT FRAME {&FRAME-NAME} fi-arquivo
       dwb_rpt_param.ind_dwb_run_mode   = ENTRY(INTEGER (INPUT FRAME {&FRAME-NAME} rs-saida), "On-Line,Batch").

IF INTEGER (INPUT FRAME {&FRAME-NAME} rs-saida) = 2 THEN DO: /* Batch */
    run prgtec/btb/btb911za.p (Input "esjd001rp",
                               Input "5.06.00.001",
                               Input 40,
                               Input recid(dwb_rpt_param),
                               output v_num_ped_exec) /*prg_fnc_criac_ped_exec*/.

    if  v_num_ped_exec <> 0 then                     
      run utp/ut-msgs.p (input "show":U, input 4169, input string(v_num_ped_exec)).                      
END.
ELSE DO: /* On-Line */
    RUN esp/esjd001rp.p.
END.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_inicio C-Win 
PROCEDURE pi_inicio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
cadastrar no programa (esp/esfnd001.w)

Programa  : "ESJD001"
Parƒmetro : "RPW Ativo"
Valor     : Sim
*/
    do with frame {&frame-name}:
        find first es-param-prog no-lock
             where es-param-prog.cod_prog_dtsul = "ESJD001"
               and es-param-prog.cod-param      = "RPW Ativo"
               and es-param-prog.txt-valor      = "Sim" no-error.
        if  avail es-param-prog then
            assign rs-saida:sensitive = yes
                   rs-saida:hidden    = no.
        else
            assign rs-saida:sensitive = no
                   rs-saida:hidden    = yes.

        assign fi-arq-jde:screen-value = session:temp-directory + "esjd001-carga.csv".
               fi-arquivo:screen-value = session:temp-directory + "esjd001-log-" + string(time) + ".txt".
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


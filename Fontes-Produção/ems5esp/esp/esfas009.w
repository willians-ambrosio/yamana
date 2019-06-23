&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

/*****************************************************************************
** Programa..............: 
** Descricao.............: 
** Versao................: 1.00.00.000
** Procedimento..........: 
** Nome Externo..........: 
** Criado por............: Bruno Bertulli (DSC)
** Criado em.............: 
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

Def Buffer dwb_set_list For dwb_set_list.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.

def var c-param-mais   as char format "x(2000)":U                  no-undo.
def var i-sequencial   as int                                      no-undo init 200.

assign c-param-mais = "" + chr(10) + "" + chr(10) + "" + chr(10) + "" + chr(10) + "". /* se diferente de venda */

def temp-table tt-bem-pat no-undo
    FIELD cod_empresa       LIKE bem_pat.cod_empresa
    field cod_cta_pat       like bem_pat.cod_cta_pat     init "0"
    field num_bem_pat       like bem_pat.num_bem_pat     init 0
    field num_seq_bem_pat   like bem_pat.num_seq_bem_pat init 0
    field de-perc-corrente  AS   DECIMAL
    field de-perc-dolar     AS   DECIMAL
    index codigo as primary unique 
      cod_empresa
      cod_cta_pat 
      num_bem_pat 
      num_seq_bem_pat.
    
def new global shared var v_cod_usuar_corren      As Char no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f_main

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES item_lancto_ctbl

/* Definitions for FRAME f_main                                         */
&Scoped-define QUERY-STRING-f_main FOR EACH item_lancto_ctbl SHARE-LOCK
&Scoped-define OPEN-QUERY-f_main OPEN QUERY f_main FOR EACH item_lancto_ctbl SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-f_main item_lancto_ctbl
&Scoped-define FIRST-TABLE-IN-QUERY-f_main item_lancto_ctbl


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-busca-arq rt_rgf rt_cxcf RECT-12 ~
fi-arquivo bt_fechar bt_cancelar Btn_Help bt_executar 
&Scoped-Define DISPLAYED-OBJECTS fi-arquivo 

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

DEFINE VARIABLE fi-arquivo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arquivo" 
     VIEW-AS FILL-IN 
     SIZE 59 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 80 BY 1.71.

DEFINE RECTANGLE rt_cxcf
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80 BY 1.42.

DEFINE RECTANGLE rt_rgf
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80 BY 1.5
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY f_main FOR 
      item_lancto_ctbl SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f_main
     bt-busca-arq AT ROW 6.46 COL 69.57 WIDGET-ID 154
     fi-arquivo AT ROW 6.63 COL 8 COLON-ALIGNED WIDGET-ID 104
     bt_fechar AT ROW 15.71 COL 3 WIDGET-ID 26
     bt_cancelar AT ROW 15.71 COL 13.86 WIDGET-ID 36
     Btn_Help AT ROW 15.71 COL 70.43 WIDGET-ID 24
     bt_executar AT ROW 1.17 COL 1.86 WIDGET-ID 76
     rt_rgf AT ROW 1 COL 1 WIDGET-ID 4
     rt_cxcf AT ROW 15.5 COL 1 WIDGET-ID 2
     RECT-12 AT ROW 6.13 COL 1 WIDGET-ID 106
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16
         FONT 1
         CANCEL-BUTTON bt_cancelar WIDGET-ID 100.


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
         TITLE              = ".: Altera Percentual Deprecia‡Æo - ESFAS009 :."
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
       bt_executar:PRIVATE-DATA IN FRAME f_main     = 
                "Erase".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f_main
/* Query rebuild information for FRAME f_main
     _TblList          = "item_lancto_ctbl"
     _Query            is OPENED
*/  /* FRAME f_main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* .: Altera Percentual Deprecia‡Æo - ESFAS009 :. */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* .: Altera Percentual Deprecia‡Æo - ESFAS009 :. */
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
    DEFINE VARIABLE v_cod_dwb_file AS CHARACTER   NO-UNDO.

    system-dialog GET-FILE v_cod_dwb_file
        title "Importar" /*l_importar*/ 
        filters '*.csv'  '*.csv'
        save-as
        create-test-file.
    assign fi-arquivo:screen-value in frame {&FRAME-NAME} = v_cod_dwb_file.
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
    run pi_executar.
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

  {&OPEN-QUERY-f_main}
  GET FIRST f_main.
  DISPLAY fi-arquivo 
      WITH FRAME f_main IN WINDOW C-Win.
  ENABLE bt-busca-arq rt_rgf rt_cxcf RECT-12 fi-arquivo bt_fechar bt_cancelar 
         Btn_Help bt_executar 
      WITH FRAME f_main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f_main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-abre-arq C-Win 
PROCEDURE pi-abre-arq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def Input param p_cod_dwb_file
        as character
        format "x(40)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_cod_key_value
        as character
        format "x(8)":U
        no-undo.


    /************************** Variable Definition End *************************/

    get-key-value section 'EMS' key 'Show-Report-Program' value v_cod_key_value.
    if  v_cod_key_value = ""
    or   v_cod_key_value = ?
    then do:
        assign v_cod_key_value = 'notepad.exe'.
        put-key-value section 'EMS' key 'Show-Report-Program' value v_cod_key_value no-error.
    end /* if */.

    run winexec (input v_cod_key_value + chr(32) + p_cod_dwb_file, input 1).

    END PROCEDURE.

    PROCEDURE WinExec EXTERNAL 'kernel32.dll':
      DEF INPUT  PARAM prg_name                          AS CHARACTER.
      DEF INPUT  PARAM prg_style                         AS SHORT.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-arquivo C-Win 
PROCEDURE pi-carrega-arquivo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    def var c-linha as char no-undo.

    empty temp-table tt-bem-pat.
    
    input from value(INPUT FRAME {&FRAME-NAME} fi-arquivo) no-convert.

    repeat:

        import unformat c-linha.

        if c-linha begins "conta" then next.
        if c-linha begins "empresa" then next.

        find first tt-bem-pat 
            WHERE tt-bem-pat.cod_empresa     = entry(1,c-linha,";")
            AND   tt-bem-pat.cod_cta_pat     = entry(2,c-linha,";")
            AND   tt-bem-pat.num_bem_pat     = integer (entry(3,c-linha,";"))
            AND   tt-bem-pat.num_seq_bem_pat = integer (entry(4,c-linha,";")) no-error.
        if not avail tt-bem-pat then do:
            create tt-bem-pat.
            assign tt-bem-pat.cod_empresa      = entry(1,c-linha,";")          
                   tt-bem-pat.cod_cta_pat      = entry(2,c-linha,";")          
                   tt-bem-pat.num_bem_pat      = integer (entry(3,c-linha,";"))
                   tt-bem-pat.num_seq_bem_pat  = integer (entry(4,c-linha,";"))
                   tt-bem-pat.de-perc-corrente = DECIMAL (entry(5,c-linha,";"))
                   tt-bem-pat.de-perc-dolar    = DECIMAL (entry(6,c-linha,";")).
        end.

        RUN pi-acompanhar IN h-acomp (INPUT tt-bem-pat.cod_empresa + " - " + tt-bem-pat.cod_cta_pat + " - " + STRING (tt-bem-pat.num_bem_pat) + " - " + STRING (tt-bem-pat.num_seq_bem_pat)).

    end.

    input close.

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

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp (input "Inicializando").

run pi-carrega-arquivo.

for each tt-bem-pat,
    first bem_pat no-lock where bem_pat.cod_empresa     = tt-bem-pat.cod_empresa
                            AND bem_pat.cod_cta_pat     = tt-bem-pat.cod_cta_pat
                            and bem_pat.num_bem_pat     = tt-bem-pat.num_bem_pat
                            and bem_pat.num_seq_bem_pat = tt-bem-pat.num_seq_bem_pat:

    RUN pi-acompanhar IN h-acomp (INPUT bem_pat.cod_empresa + " - " + bem_pat.cod_cta_pat + " - " + STRING (bem_pat.num_bem_pat) + " - " + STRING (bem_pat.num_seq_bem_pat)).

    for each param_calc_bem_pat
        where param_calc_bem_pat.num_id_bem_pat = bem_pat.num_id_bem_pat
        and   param_calc_bem_pat.cod_tip_calc  <> "CM"
        and   param_calc_bem_pat.cod_tip_calc  <> "":

        IF param_calc_bem_pat.cod_finalid_econ = "Dolar" THEN DO:
            IF tt-bem-pat.de-perc-dolar = 0 THEN DO:
                FOR FIRST param_calc_cta NO-LOCK
                    WHERE param_calc_cta.cod_empresa      = bem_pat.cod_empresa
                    AND   param_calc_cta.cod_cta_pat      = bem_pat.cod_cta_pat
                    AND   param_calc_cta.cod_cenar_ctbl   = param_calc_bem_pat.cod_cenar_ctbl
                    AND   param_calc_cta.cod_finalid_econ = param_calc_bem_pat.cod_finalid_econ
                    AND   param_calc_cta.cod_tip_calc     = param_calc_bem_pat.cod_tip_calc:

                    assign param_calc_bem_pat.val_perc_anual_dpr = param_calc_cta.val_perc_anual_dpr.
                END.
            END.
            ELSE DO:
                assign param_calc_bem_pat.val_perc_anual_dpr = tt-bem-pat.de-perc-dolar.
            END.
        END.
        ELSE DO:
            IF tt-bem-pat.de-perc-corrente = 0 THEN DO:
                FOR FIRST param_calc_cta NO-LOCK
                    WHERE param_calc_cta.cod_empresa      = bem_pat.cod_empresa
                    AND   param_calc_cta.cod_cta_pat      = bem_pat.cod_cta_pat
                    AND   param_calc_cta.cod_cenar_ctbl   = param_calc_bem_pat.cod_cenar_ctbl
                    AND   param_calc_cta.cod_finalid_econ = param_calc_bem_pat.cod_finalid_econ
                    AND   param_calc_cta.cod_tip_calc     = param_calc_bem_pat.cod_tip_calc:

                    assign param_calc_bem_pat.val_perc_anual_dpr = param_calc_cta.val_perc_anual_dpr.
                END.
            END.
            ELSE DO:
                assign param_calc_bem_pat.val_perc_anual_dpr = tt-bem-pat.de-perc-corrente.
            END.
        END.

        ASSIGN param_calc_bem_pat.qtd_anos_vida_util = 100 / param_calc_bem_pat.val_perc_anual_dpr.
    end /* for each para_calc_bem_pat */.

end.

run pi-finalizar in h-acomp. 

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


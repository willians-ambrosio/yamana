&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

/*****************************************************************************
** Programa..............: esjd400
** Descricao.............: Valida‡Æo EMS5 X JDE
** Versao................: 1.00.00.000
** Procedimento..........: 
** Nome Externo..........: 
** Criado por............: Hilton Borba
** Criado em.............: 14/09/2014
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
DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.

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

def temp-table tt-controle
    field conta  as char
    field ccusto as char
    index idx1  conta ccusto.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f_main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-busca-arq rt_rgf rt_cxcf RECT-12 ~
fi-arquivo bt_fechar bt_cancelar bt_executar Btn_Help 
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
     LABEL "Arquivo Saida" 
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


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f_main
     bt-busca-arq AT ROW 13.79 COL 74.14
     fi-arquivo AT ROW 14 COL 12.86 COLON-ALIGNED
     bt_fechar AT ROW 15.71 COL 3
     bt_cancelar AT ROW 15.71 COL 13.86
     bt_executar AT ROW 1.17 COL 1.86
     Btn_Help AT ROW 15.71 COL 70.43
     rt_rgf AT ROW 1 COL 1
     rt_cxcf AT ROW 15.5 COL 1
     RECT-12 AT ROW 13.5 COL 1
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
         TITLE              = ".: Valida‡Æo EMS5 X JDE - ESJD400 :."
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* .: Valida‡Æo EMS5 X JDE - ESJD400 :. */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* .: Valida‡Æo EMS5 X JDE - ESJD400 :. */
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
    DEFINE VARIABLE l-ok AS LOGICAL     NO-UNDO.

    assign input frame {&FRAME-NAME} fi-arquivo.
    SYSTEM-DIALOG GET-FILE fi-arquivo
       FILTERS "*.csv" "*.csv",
               "*.*" "*.*"
       ASK-OVERWRITE
       DEFAULT-EXTENSION "*.csv"
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
  DISPLAY fi-arquivo 
      WITH FRAME f_main IN WINDOW C-Win.
  ENABLE bt-busca-arq rt_rgf rt_cxcf RECT-12 fi-arquivo bt_fechar bt_cancelar 
         bt_executar Btn_Help 
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
    def var ch-Excel as component-handle no-undo.
    def var c-ccusto as char no-undo.

    run utp/ut-acomp.p persistent set h-acomp.

    run pi-inicializar in h-acomp (input "Valida EMS5 X JDE...").

    empty temp-table tt-controle.

    output to value(fi-arquivo:screen-value in FRAME {&FRAME-NAME}) no-echo no-convert.

    PUT UNFORMATTED "LOG DE ERRO EMS5 X JDE" SKIP(2).

    PUT UNFORMATTED "Emp"
                    ";Estab"
                    ";Conta Pat"
                    ";Bem"
                    ";Seq"
                    ";Cenario"
                    ";Finalidade"
                    ";Finalid Ctbl"
                    ";Data Ini"
                    ";Data Fim"
                    ";Plano Contas"
                    ";Conta"
                    ";Ccusto" SKIP.

    for each bem_pat no-lock
       where bem_pat.val_perc_bxa <> 100:
        run pi-acompanhar in h-acomp(input "Emp: "      + bem_pat.cod_empresa + 
                                           " / Conta: " + bem_pat.cod_cta_pat + 
                                           " / Bem: "   + string(bem_pat.num_bem_pat) + 
                                           " / Seq: "   + string(bem_pat.num_seq_bem_pat)).

        for each param_ctbz_cta_pat no-lock
           where param_ctbz_cta_pat.cod_empresa      = bem_pat.cod_empresa
             and param_ctbz_cta_pat.cod_cta_pat      = bem_pat.cod_cta_pat
             and param_ctbz_cta_pat.cod_finalid_econ = "Corrente":
            if  param_ctbz_cta_pat.cod_cta_ctbl_cr <> "" and
                not param_ctbz_cta_pat.cod_cta_ctbl_cr begins "172" and
                param_ctbz_cta_pat.cod_cta_ctbl_cr < "80000000" then do:
                assign c-ccusto = "".
                find last criter_distrib_cta_ctbl no-lock
                    where criter_distrib_cta_ctbl.cod_empresa        = bem_pat.cod_empresa
                      and criter_distrib_cta_ctbl.cod_plano_cta_ctbl = "CONTSOC"
                      and criter_distrib_cta_ctbl.cod_cta_ctbl       = param_ctbz_cta_pat.cod_cta_ctbl_cr
                      and criter_distrib_cta_ctbl.cod_estab          = bem_pat.cod_estab
                      and criter_distrib_cta_ctbl.dat_inic_valid    <= today
                      and criter_distrib_cta_ctbl.dat_fim_valid      > today no-error.
                if  avail criter_distrib_cta_ctbl and
                    criter_distrib_cta_ctbl.ind_criter_distrib_ccusto <> "NÆo Utiliza" then
                    assign c-ccusto = bem_pat.cod_ccusto_respons.

                if  param_ctbz_cta_pat.cod_cta_ctbl_cr begins "1" or       /* Ativo */
                    param_ctbz_cta_pat.cod_cta_ctbl_cr begins "2" then do: /* Passivo */
                    find first es-cross-reference-jde no-lock
                         where es-cross-reference-jde.cod_cta_ctbl = param_ctbz_cta_pat.cod_cta_ctbl_cr
                           and es-cross-reference-jde.log-erro     = 0 no-error.
                    if  not avail es-cross-reference-jde then do:
                        put unformatted
                            ";" bem_pat.cod_empresa
                            ";" bem_pat.cod_estab
                            ";" bem_pat.cod_cta_pat
                            ";" bem_pat.num_bem_pat
                            ";" bem_pat.num_seq_bem_pat
                            ";" param_ctbz_cta_pat.cod_cenar_ctbl 
                            ";" param_ctbz_cta_pat.cod_finalid_econ 
                            ";" param_ctbz_cta_pat.ind_finalid_ctbl 
                            ";" param_ctbz_cta_pat.dat_inic_valid 
                            ";" param_ctbz_cta_pat.dat_fim_valid 
                            ";" param_ctbz_cta_pat.cod_plano_cta_ctbl 
                            ";" param_ctbz_cta_pat.cod_cta_ctbl_cr
                            ";" skip.
                    end.
                end.
                else do:
                    find first es-cross-reference-jde no-lock
                         where es-cross-reference-jde.cod_cta_ctbl = param_ctbz_cta_pat.cod_cta_ctbl_cr
                           and es-cross-reference-jde.cod_ccusto   = c-ccusto
                           and es-cross-reference-jde.log-erro     = 0 no-error.
                    if  not avail es-cross-reference-jde then do:
                        put unformatted
                            ";" bem_pat.cod_empresa
                            ";" bem_pat.cod_estab
                            ";" bem_pat.cod_cta_pat
                            ";" bem_pat.num_bem_pat
                            ";" bem_pat.num_seq_bem_pat
                            ";" param_ctbz_cta_pat.cod_cenar_ctbl 
                            ";" param_ctbz_cta_pat.cod_finalid_econ 
                            ";" param_ctbz_cta_pat.ind_finalid_ctbl 
                            ";" param_ctbz_cta_pat.dat_inic_valid 
                            ";" param_ctbz_cta_pat.dat_fim_valid 
                            ";" param_ctbz_cta_pat.cod_plano_cta_ctbl 
                            ";" param_ctbz_cta_pat.cod_cta_ctbl_cr
                            ";" bem_pat.cod_ccusto_respons skip.
                    end.
                end.
            end.

            if  param_ctbz_cta_pat.cod_cta_ctbl_db <> "" and
                not param_ctbz_cta_pat.cod_cta_ctbl_db begins "172" and
                param_ctbz_cta_pat.cod_cta_ctbl_db < "80000000" then do:
                assign c-ccusto = "".
                find last criter_distrib_cta_ctbl no-lock
                    where criter_distrib_cta_ctbl.cod_empresa        = bem_pat.cod_empresa
                      and criter_distrib_cta_ctbl.cod_plano_cta_ctbl = "CONTSOC"
                      and criter_distrib_cta_ctbl.cod_cta_ctbl       = param_ctbz_cta_pat.cod_cta_ctbl_db
                      and criter_distrib_cta_ctbl.cod_estab          = bem_pat.cod_estab
                      and criter_distrib_cta_ctbl.dat_inic_valid    <= today
                      and criter_distrib_cta_ctbl.dat_fim_valid      > today no-error.
                if  avail criter_distrib_cta_ctbl and
                    criter_distrib_cta_ctbl.ind_criter_distrib_ccusto <> "NÆo Utiliza" then
                    assign c-ccusto = bem_pat.cod_ccusto_respons.

                if  param_ctbz_cta_pat.cod_cta_ctbl_db begins "1" or       /* Ativo */
                    param_ctbz_cta_pat.cod_cta_ctbl_db begins "2" then do: /* Passivo */
                    find first es-cross-reference-jde no-lock
                         where es-cross-reference-jde.cod_cta_ctbl = param_ctbz_cta_pat.cod_cta_ctbl_db
                           and es-cross-reference-jde.log-erro     = 0 no-error.
                    if  not avail es-cross-reference-jde then do:
                        put unformatted
                            ";" bem_pat.cod_empresa
                            ";" bem_pat.cod_estab
                            ";" bem_pat.cod_cta_pat
                            ";" bem_pat.num_bem_pat
                            ";" bem_pat.num_seq_bem_pat
                            ";" param_ctbz_cta_pat.cod_cenar_ctbl 
                            ";" param_ctbz_cta_pat.cod_finalid_econ 
                            ";" param_ctbz_cta_pat.ind_finalid_ctbl 
                            ";" param_ctbz_cta_pat.dat_inic_valid 
                            ";" param_ctbz_cta_pat.dat_fim_valid 
                            ";" param_ctbz_cta_pat.cod_plano_cta_ctbl 
                            ";" param_ctbz_cta_pat.cod_cta_ctbl_db
                            ";" skip.
                    end.
                end.
                else do:
                    find first es-cross-reference-jde no-lock
                         where es-cross-reference-jde.cod_cta_ctbl = param_ctbz_cta_pat.cod_cta_ctbl_db
                           and es-cross-reference-jde.cod_ccusto   = c-ccusto
                           and es-cross-reference-jde.log-erro     = 0 no-error.
                    if  not avail es-cross-reference-jde then do:
                        put unformatted
                            ";" bem_pat.cod_empresa
                            ";" bem_pat.cod_estab
                            ";" bem_pat.cod_cta_pat
                            ";" bem_pat.num_bem_pat
                            ";" bem_pat.num_seq_bem_pat
                            ";" param_ctbz_cta_pat.cod_cenar_ctbl 
                            ";" param_ctbz_cta_pat.cod_finalid_econ 
                            ";" param_ctbz_cta_pat.ind_finalid_ctbl 
                            ";" param_ctbz_cta_pat.dat_inic_valid 
                            ";" param_ctbz_cta_pat.dat_fim_valid 
                            ";" param_ctbz_cta_pat.cod_plano_cta_ctbl 
                            ";" param_ctbz_cta_pat.cod_cta_ctbl_db
                            ";" bem_pat.cod_ccusto_respons skip.
                    end.
                end.
            end.

            if  param_ctbz_cta_pat.cod_cta_ctbl_alter <> "" and
                not param_ctbz_cta_pat.cod_cta_ctbl_alter begins "172" and
                param_ctbz_cta_pat.cod_cta_ctbl_alter < "80000000" then do:
                assign c-ccusto = "".
                find last criter_distrib_cta_ctbl no-lock
                    where criter_distrib_cta_ctbl.cod_empresa        = bem_pat.cod_empresa
                      and criter_distrib_cta_ctbl.cod_plano_cta_ctbl = "CONTSOC"
                      and criter_distrib_cta_ctbl.cod_cta_ctbl       = param_ctbz_cta_pat.cod_cta_ctbl_alter
                      and criter_distrib_cta_ctbl.cod_estab          = bem_pat.cod_estab
                      and criter_distrib_cta_ctbl.dat_inic_valid    <= today
                      and criter_distrib_cta_ctbl.dat_fim_valid      > today no-error.
                if  avail criter_distrib_cta_ctbl and
                    criter_distrib_cta_ctbl.ind_criter_distrib_ccusto <> "NÆo Utiliza" then
                    assign c-ccusto = bem_pat.cod_ccusto_respons.

                if  param_ctbz_cta_pat.cod_cta_ctbl_alter begins "1" or       /* Ativo */
                    param_ctbz_cta_pat.cod_cta_ctbl_alter begins "2" then do: /* Passivo */
                    find first es-cross-reference-jde no-lock
                         where es-cross-reference-jde.cod_cta_ctbl = param_ctbz_cta_pat.cod_cta_ctbl_alter
                           and es-cross-reference-jde.log-erro     = 0 no-error.
                    if  not avail es-cross-reference-jde then do:
                        put unformatted
                            ";" bem_pat.cod_empresa
                            ";" bem_pat.cod_estab
                            ";" bem_pat.cod_cta_pat
                            ";" bem_pat.num_bem_pat
                            ";" bem_pat.num_seq_bem_pat
                            ";" param_ctbz_cta_pat.cod_cenar_ctbl 
                            ";" param_ctbz_cta_pat.cod_finalid_econ 
                            ";" param_ctbz_cta_pat.ind_finalid_ctbl 
                            ";" param_ctbz_cta_pat.dat_inic_valid 
                            ";" param_ctbz_cta_pat.dat_fim_valid 
                            ";" param_ctbz_cta_pat.cod_plano_cta_ctbl 
                            ";" param_ctbz_cta_pat.cod_cta_ctbl_alter
                            ";" skip.
                    end.
                end.
                else do:
                    find first es-cross-reference-jde no-lock
                         where es-cross-reference-jde.cod_cta_ctbl = param_ctbz_cta_pat.cod_cta_ctbl_alter
                           and es-cross-reference-jde.cod_ccusto   = c-ccusto
                           and es-cross-reference-jde.log-erro     = 0 no-error.
                    if  not avail es-cross-reference-jde then do:
                        put unformatted
                            ";" bem_pat.cod_empresa
                            ";" bem_pat.cod_estab
                            ";" bem_pat.cod_cta_pat
                            ";" bem_pat.num_bem_pat
                            ";" bem_pat.num_seq_bem_pat
                            ";" param_ctbz_cta_pat.cod_cenar_ctbl 
                            ";" param_ctbz_cta_pat.cod_finalid_econ 
                            ";" param_ctbz_cta_pat.ind_finalid_ctbl 
                            ";" param_ctbz_cta_pat.dat_inic_valid 
                            ";" param_ctbz_cta_pat.dat_fim_valid 
                            ";" param_ctbz_cta_pat.cod_plano_cta_ctbl 
                            ";" param_ctbz_cta_pat.cod_cta_ctbl_alter
                            ";" bem_pat.cod_ccusto_respons skip.
                    end.
                end.
            end.
        end.
    end.
    OUTPUT CLOSE.

    run pi-finalizar in h-acomp.

    Create "Excel.Application" ch-Excel no-error.

    ch-Excel:workbooks:open(fi-arquivo:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
    ch-Excel:visible = YES.
    ch-Excel:application:DisplayAlerts = NO.
    ch-Excel:sheets:item(1).

    release object ch-Excel.
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

    assign fi-arquivo:screen-value in frame {&frame-name} = SESSION:TEMP-DIRECTORY + "esjd400.csv".
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


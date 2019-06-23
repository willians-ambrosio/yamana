
&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

/*****************************************************************************
** Programa..............: esfas350ab
** Descricao.............: Relat¢rio de Bens no Excel
** Versao................: 1.00.00.000
** Procedimento..........: esfas350ab
** Nome Externo..........: esp/esfas350ab.w
** Criado por............: Bruno Bertulli (DSC)
** Criado em.............: 15/02/2013
*****************************************************************************
** Data Altera‡Æo: 22/05/2018
** Auor..........: Willians Ambrosio - Grupo DKP
** Descri‡Æo.....: InclusÆo da coluna valor dpr U$
** RevisÆo.......: 02
*****************************************************************************
** Data Altera‡Æo: 25/06/2018
** Auor..........: Willians Ambrosio - Grupo DKP
** Descri‡Æo.....: InclusÆo das colunas documento e serie (recebimento)
** RevisÆo.......: 03
*****************************************************************************/

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
{include/i-prgvrs.i ESFAS350AB 12.01.19.003}
/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
define new global shared variable v_cod_recid_dwb as recid no-undo.

def new global shared var v_cod_dwb_user
    as character
    format "x(21)":U
    label "Usuÿrio"
    column-label "Usuÿrio"
    no-undo.
def new global shared var v_rec_cenar_ctbl
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.

/* ------ Vari veis para execu‡Æo em Batch ------- */
def var v_cod_program as character no-undo initial "esfas350ab" .
def var v_cod_externo as character no-undo initial "esp\esfas350ab.w" .

def new shared var v_cod_dwb_program as character format "x(32)":U label "Programa" column-label "Programa" no-undo. 
def new global shared var v_cod_usuar_corren as character format "x(12)":U label "Usu rio Corrente" column-label "Usu rio Corrente" no-undo.
def new shared var v_cod_release     as character format "x(12)":U no-undo.
def new global shared var v_rec_table as recid no-undo.

def var v_num_ped_exec as integer format ">>>>9":U label "Pedido" column-label "Pedido" no-undo.

def temp-table tt_tip_calc_sit_movto no-undo
    field tta_cod_tip_calc                 as character format "x(7)" label "Tipo Cÿlculo" column-label "Tipo Cÿlculo"
    field tta_ind_tip_calc                 as character format "X(20)" initial "Deprecia‡Æo" label "Tipo" column-label "Tipo"
    .

def buffer b_sdo_bem_pat
    for sdo_bem_pat.

def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.
def var v_val_pct_dpr
    as decimal
    format ">>>>9.99":U
    decimals 2
    initial 0
    label "% Deprecia‡Æo"
    column-label "% Deprecia‡Æo"
    no-undo.
def var v_val_pct_dpr_incevda
    as decimal
    format ">>>>9.99":U
    decimals 2
    initial 0
    label "% Deprec Incentivada"
    column-label "% Deprec Incentivada"
    no-undo.
def var v_val_original
    as decimal
    format "->>>>>,>>>,>>9.99":U
    decimals 4
    initial 0
    label "Valor Original"
    column-label "Valor Original"
    no-undo.
def var v_val_origin_corrig
    as decimal
    format "->>>>>,>>>,>>9.99":U
    decimals 4
    label "Val Origin Corrigido"
    column-label "Val Origin Corrigido"
    no-undo.
def var v_val_dpr
    as decimal
    format "->>>,>>>,>>>,>>9.99":U
    decimals 2
    initial 0
    label "Valor Depreciado"
    column-label "Valor Depreciado"
    no-undo.
def var v_dat_movto_bem_pat
    as date
    format "99/99/9999":U
    initial 12/31/9999
    label "Data Movimento"
    column-label "Data Movimento"
    no-undo.
def var v_dat_sdo_bem_pat
    as date
    format "99/99/9999":U
    label "Data Saldo Bem"
    column-label "Data Saldo Bem"
    no-undo.

def var v_val_original_dolar
    as decimal
    format "->>>>>,>>>,>>9.99":U
    decimals 4
    initial 0
    label "Valor Original"
    column-label "Valor Original"
    no-undo.
def var v_val_origin_corrig_dolar
    as decimal
    format "->>>>>,>>>,>>9.99":U
    decimals 4
    label "Val Origin Corrigido"
    column-label "Val Origin Corrigido"
    no-undo.
def var v_val_dpr_dolar
    as decimal
    format "->>>,>>>,>>>,>>9.99":U
    decimals 2
    initial 0
    label "Valor Depreciado"
    column-label "Valor Depreciado"
    no-undo.


DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.

DEFINE VARIABLE chExcelApplication AS COM-HANDLE    NO-UNDO.
DEFINE VARIABLE chWorkBook         AS COM-HANDLE    NO-UNDO.
DEFINE VARIABLE chWorkSheet        AS COM-HANDLE    NO-UNDO.

DEFINE VARIABLE i-linha AS INTEGER     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f_main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-busca-cenario rt_rgf rt_cxcf RECT-8 ~
rt_key fi-cod_cta_pat-ini fi-cod_cta_pat-fim fi-data-ini fi-data-fim ~
fi-cod_cenar_ctbl fi_arquivo bt_fechar bt_cancelar Btn_Help bt_executar 
&Scoped-Define DISPLAYED-OBJECTS fi-cod_cta_pat-ini fi-cod_cta_pat-fim ~
fi-data-ini fi-data-fim fi-cod_cenar_ctbl fi_arquivo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-busca-cenario 
     IMAGE-UP FILE "image/im-zoo.bmp":U NO-FOCUS
     LABEL "Busca Cen rio" 
     SIZE 4 BY .88 TOOLTIP "Busca Cen rio".

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

DEFINE VARIABLE fi-cod_cenar_ctbl AS CHARACTER FORMAT "X(08)":U 
     LABEL "Cen rio" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .79 NO-UNDO.

DEFINE VARIABLE fi-cod_cta_pat-fim AS CHARACTER FORMAT "X(18)":U INITIAL "ZZZZZZZZZZZZZZZZZZ" 
     LABEL "At‚" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .79 NO-UNDO.

DEFINE VARIABLE fi-cod_cta_pat-ini AS CHARACTER FORMAT "X(18)":U 
     LABEL "Conta Patrimonial" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .79 NO-UNDO.

DEFINE VARIABLE fi-data-fim AS DATE FORMAT "99/99/9999":U 
     LABEL "At‚" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .79 NO-UNDO.

DEFINE VARIABLE fi-data-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Per¡odo" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .79 NO-UNDO.

DEFINE VARIABLE fi_arquivo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 2.75.

DEFINE RECTANGLE rt_cxcf
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80 BY 1.42.

DEFINE RECTANGLE rt_key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 9.

DEFINE RECTANGLE rt_rgf
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80 BY 1.5
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f_main
     bt-busca-cenario AT ROW 5.08 COL 42.14 WIDGET-ID 118
     fi-cod_cta_pat-ini AT ROW 3.38 COL 26 COLON-ALIGNED WIDGET-ID 136
     fi-cod_cta_pat-fim AT ROW 3.38 COL 48 COLON-ALIGNED WIDGET-ID 138
     fi-data-ini AT ROW 4.25 COL 26 COLON-ALIGNED WIDGET-ID 132
     fi-data-fim AT ROW 4.25 COL 48 COLON-ALIGNED WIDGET-ID 134
     fi-cod_cenar_ctbl AT ROW 5.13 COL 26 COLON-ALIGNED WIDGET-ID 120
     fi_arquivo AT ROW 13.25 COL 3 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     bt_fechar AT ROW 15.71 COL 3 WIDGET-ID 26
     bt_cancelar AT ROW 15.71 COL 13.86 WIDGET-ID 36
     Btn_Help AT ROW 15.71 COL 70.43 WIDGET-ID 24
     bt_executar AT ROW 1.17 COL 1.86 WIDGET-ID 76
     " Destino" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 11.75 COL 4 WIDGET-ID 12
     rt_rgf AT ROW 1 COL 1 WIDGET-ID 4
     rt_cxcf AT ROW 15.5 COL 1 WIDGET-ID 2
     RECT-8 AT ROW 12 COL 2 WIDGET-ID 6
     rt_key AT ROW 2.75 COL 2 WIDGET-ID 82
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82 BY 16
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
         TITLE              = ".: Movimentos de Bens Sint‚tico :. ESFAS350AB 12.01.19.003"
         HEIGHT             = 16
         WIDTH              = 81.43
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
       bt_executar:PRIVATE-DATA IN FRAME f_main     = 
                "Erase".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* .: Movimentos de Bens Sint‚tico :. ESFAS350AB 12.01.19.002 */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* .: Movimentos de Bens Sint‚tico :. ESFAS350AB 12.01.19.002 */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-busca-cenario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-busca-cenario C-Win
ON CHOOSE OF bt-busca-cenario IN FRAME f_main /* Busca Cen rio */
DO:
    /* fn_generic_zoom_variable */
    if  search("prgint/utb/utb076ka.r") = ? and search("prgint/utb/utb076ka.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa executÿvel n’o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb076ka.p".
        else do:
            message "Programa executÿvel n’o foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb076ka.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run prgint/utb/utb076ka.p /*prg_sea_cenar_ctbl*/.
    if  v_rec_cenar_ctbl <> ?
    then do:
        find cenar_ctbl where recid(cenar_ctbl) = v_rec_cenar_ctbl no-lock no-error.
        assign fi-cod_cenar_ctbl:screen-value in frame {&frame-name} =
               string(cenar_ctbl.cod_cenar_ctbl).

        apply "entry" to fi-cod_cenar_ctbl in frame {&frame-name}.
    end /* if */.
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
    run pi_salva.
    apply "CLOSE" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod_cenar_ctbl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod_cenar_ctbl C-Win
ON F5 OF fi-cod_cenar_ctbl IN FRAME f_main /* Cen rio */
DO:
    APPLY "CHOOSE" TO bt-busca-cenario.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod_cenar_ctbl C-Win
ON LEAVE OF fi-cod_cenar_ctbl IN FRAME f_main /* Cen rio */
DO:
    find first cenar_ctbl no-lock
         where cenar_ctbl.cod_cenar_ctbl = INPUT FRAME {&FRAME-NAME} fi-cod_cenar_ctbl no-error.
    if  not avail cenar_ctbl then do:
        /* Cenÿrio Contÿbil incorreto ! */
        run pi_messages (input "show",
                         input 601,
                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_601*/.
        return no-apply.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_arquivo C-Win
ON LEAVE OF fi_arquivo IN FRAME f_main
DO:

    if index (fi_arquivo:screen-value in frame {&frame-name},'XLS') = 0 then do:
        assign fi_arquivo:screen-value in frame {&frame-name} = session:temp-directory + "esfas350ab.xls".
    end.
    
  
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
  DISPLAY fi-cod_cta_pat-ini fi-cod_cta_pat-fim fi-data-ini fi-data-fim 
          fi-cod_cenar_ctbl fi_arquivo 
      WITH FRAME f_main IN WINDOW C-Win.
  ENABLE bt-busca-cenario rt_rgf rt_cxcf RECT-8 rt_key fi-cod_cta_pat-ini 
         fi-cod_cta_pat-fim fi-data-ini fi-data-fim fi-cod_cenar_ctbl 
         fi_arquivo bt_fechar bt_cancelar Btn_Help bt_executar 
      WITH FRAME f_main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f_main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_depreciacao C-Win 
PROCEDURE pi_depreciacao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

        assign v_val_pct_dpr = 0
               v_val_pct_dpr_incevda = 0.
        param_block:
        for  each param_calc_bem_pat no-lock
            where param_calc_bem_pat.num_id_bem_pat   = bem_pat.num_id_bem_pat 
              and param_calc_bem_pat.cod_cenar_ctbl   = INPUT FRAME {&FRAME-NAME} fi-cod_cenar_ctbl
              and param_calc_bem_pat.cod_finalid_econ = 'Corrente'
              and param_calc_bem_pat.cod_tip_calc    <> " " :
                find tt_tip_calc_sit_movto no-lock where tt_tip_calc_sit_movto.tta_cod_tip_calc =
                     param_calc_bem_pat.cod_tip_calc no-error.
                if not avail tt_tip_calc_sit_movto then do:
                   find tip_calc no-lock where tip_calc.cod_tip_calc =
                        param_calc_bem_pat.cod_tip_calc no-error.
                   if avail tip_calc then do:
                      create tt_tip_calc_sit_movto.
                      update tt_tip_calc_sit_movto.tta_cod_tip_calc = tip_calc.cod_tip_calc
                             tt_tip_calc_sit_movto.tta_ind_tip_calc = tip_calc.ind_tip_calc.
                   end.
                end.
                if  avail tt_tip_calc_sit_movto and tt_tip_calc_sit_movto.tta_ind_tip_calc = "Deprecia‡Æo" /*l_depreciacao*/  or
                           tt_tip_calc_sit_movto.tta_ind_tip_calc = "Amortiza‡Æo" /*l_amortizacao*/ 
                then do:
                     assign v_val_pct_dpr = param_calc_bem_pat.val_perc_anual_dpr
                            v_val_pct_dpr_incevda = param_calc_bem_pat.val_perc_anual_dpr_incevda.
                end /* if */.
        end /* for param_block */.

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

run pi-inicializar in h-acomp (input "Processando...").

do on error undo, return error on stop  undo, return error:

    run pi_salva.

    RUN pi-acompanhar IN h-acomp (INPUT "Inicializando o MS Excel... ").

    /* ---> Cria objeto excel <--- */
    CREATE "Excel.Application" chExcelApplication.

    /* ---> Adiciona o modelo do documento <--- */
    chWorkbook  = chExcelApplication:Workbooks:ADD().
    chWorkSheet = chExcelApplication:Sheets:ITEM(1).

    ASSIGN i-linha = 1.

    /* Cabe‡alho */
    ASSIGN chExcelApplication:Range("A"  + STRING (i-linha)):VALUE = "Conta"
           chExcelApplication:Range("B"  + STRING (i-linha)):VALUE = "Bem"
           chExcelApplication:Range("C"  + STRING (i-linha)):VALUE = "Seq"
           chExcelApplication:Range("D"  + STRING (i-linha)):VALUE = "Seq Incorp"
           chExcelApplication:Range("E"  + STRING (i-linha)):VALUE = "Descri‡Æo"
           chExcelApplication:Range("F"  + STRING (i-linha)):VALUE = "Data Movto"
           chExcelApplication:Range("G"  + STRING (i-linha)):VALUE = "Estab"
           chExcelApplication:Range("H"  + STRING (i-linha)):VALUE = "Plano CCusto"
           chExcelApplication:Range("I"  + STRING (i-linha)):VALUE = "CCusto"
           chExcelApplication:Range("J"  + STRING (i-linha)):VALUE = "Tipo Transa‡Æo"
           chExcelApplication:Range("K"  + STRING (i-linha)):VALUE = "Origem"
           chExcelApplication:Range("L"  + STRING (i-linha)):VALUE = "Data Aquisi‡Æo"
           chExcelApplication:Range("M"  + STRING (i-linha)):VALUE = "Grupo Calc."
           chExcelApplication:Range("N"  + STRING (i-linha)):VALUE = "Fornec"
           chExcelApplication:Range("O"  + STRING (i-linha)):VALUE = "% Deprec. Anual"
           chExcelApplication:Range("P"  + STRING (i-linha)):VALUE = "% Deprec. Inc."
           chExcelApplication:Range("Q"  + STRING (i-linha)):VALUE = "% Baixado"
           chExcelApplication:Range("R"  + STRING (i-linha)):VALUE = "Valor Original R$"
           chExcelApplication:Range("S"  + STRING (i-linha)):VALUE = "Valor Corrigido R$"
           chExcelApplication:Range("T"  + STRING (i-linha)):VALUE = "Valor Deprecia‡Æo"
           chExcelApplication:Range("U"  + STRING (i-linha)):VALUE = "Quantidade"
           chExcelApplication:Range("V"  + STRING (i-linha)):VALUE = "Valor Original U$"
           chExcelApplication:Range("W"  + STRING (i-linha)):VALUE = "Valor Corrigido U$"
           /* Begins : REV 02 */         
           chExcelApplication:Range("X"  + STRING (i-linha)):VALUE = "Valor Deprecia‡Æo U$"
           /* End : REV 02    */         
           chExcelApplication:Range("Y"  + STRING (i-linha)):VALUE = "O.I."
           /* Begins: Rev 03  */
           chExcelApplication:Range("Z"  + STRING (i-linha)):VALUE = "Documento"
           chExcelApplication:Range("AA" + STRING (i-linha)):VALUE = "Serie"
           /* End: Rev 03     */ 
           .

    ASSIGN i-linha = i-linha + 1.

    RUN pi-acompanhar IN h-acomp (INPUT "Processando Bens ").

    for EACH bem_pat no-lock
        where bem_pat.cod_empresa  = v_cod_empres_usuar
        and   bem_pat.cod_cta_pat >= INPUT FRAME {&FRAME-NAME} fi-cod_cta_pat-ini
        and   bem_pat.cod_cta_pat <= INPUT FRAME {&FRAME-NAME} fi-cod_cta_pat-fim
        :

        RUN pi-acompanhar IN h-acomp (INPUT "Conta: " + bem_pat.cod_cta_pat + " - Bem: " + STRING (bem_pat.num_bem_pat)).
    
        for each movto_bem_pat no-lock
            where movto_bem_pat.num_id_bem_pat = bem_pat.num_id_bem_pat
            and   movto_bem_pat.log_estorn_movto_bem_pat = NO
            and   movto_bem_pat.dat_movto_bem_pat >= INPUT FRAME {&FRAME-NAME} fi-data-ini
            and   movto_bem_pat.dat_movto_bem_pat <= INPUT FRAME {&FRAME-NAME} fi-data-fim
            :
            
            RUN pi-acompanhar IN h-acomp (INPUT "Movto: " + string (movto_bem_pat.dat_movto_bem_pat,"99/99/9999")).
    
            if  movto_bem_pat.ind_trans_calc_bem_pat = "Implanta‡Æo" /*l_implantacao*/ 
            then do:
                if  movto_bem_pat.ind_orig_calc_bem_pat <> "Aquisi‡Æo" /*l_aquisicao*/  and 
                    movto_bem_pat.ind_orig_calc_bem_pat <> "Adi‡Æo" /*l_adicao*/  and 
                    movto_bem_pat.ind_orig_calc_bem_pat <> "Altera‡Æo" /*l_alteracao*/  and 
                    movto_bem_pat.ind_orig_calc_bem_pat <> "Benfeitoria" /*l_benfeitoria*/  and 
                    movto_bem_pat.ind_orig_calc_bem_pat <> "Reavalia‡Æo" /*l_reavaliacao*/  and 
                    movto_bem_pat.ind_orig_calc_bem_pat <> "Difer Cambial" /*l_diferenca_cambial*/  and 
                    movto_bem_pat.ind_orig_calc_bem_pat <> "Rateio" /*l_rateio*/  and 
                    movto_bem_pat.ind_orig_calc_bem_pat <> "Imobilizado" /*l_imobilizado*/  and 
                    movto_bem_pat.ind_orig_calc_bem_pat <> "Desmembramento" /*l_desmembramento*/  and 
                    movto_bem_pat.ind_orig_calc_bem_pat <> "Reclassifica‡Æo" /*l_reclassificacao*/  and
                    movto_bem_pat.ind_orig_calc_bem_pat <> "Reativa‡Æo" /*l_reativacao*/  and 
                    movto_bem_pat.ind_orig_calc_bem_pat <> "UniÆo" /*l_uniao*/  and 
                    movto_bem_pat.ind_orig_calc_bem_pat <> "Leasing" /*l_leasing*/ 
                THEN NEXT.
            end /* if */.
    
            if  movto_bem_pat.ind_trans_calc_bem_pat = "Baixa" /*l_baixa*/ 
            then do:
                IF  movto_bem_pat.ind_orig_calc_bem_pat <> "Inutiliza‡Æo" /*l_inutilizacao*/  and
                    movto_bem_pat.ind_orig_calc_bem_pat <> "Quebra" /*l_quebra*/  and
                    movto_bem_pat.ind_orig_calc_bem_pat <> "Devolu‡Æo" /*l_devolucao*/  and
                    movto_bem_pat.ind_orig_calc_bem_pat <> "ExaustÆo" /*l_exaustao*/  and
                    movto_bem_pat.ind_orig_calc_bem_pat <> "Venda" /*l_venda*/  and
                    movto_bem_pat.ind_orig_calc_bem_pat <> "Desmembramento" /*l_desmembramento*/  and
                    movto_bem_pat.ind_orig_calc_bem_pat <> "Reclassifica‡Æo" /*l_reclassificacao*/  and
                    movto_bem_pat.ind_orig_calc_bem_pat <> "Transferˆncia" /*l_transferencia*/  and
                    movto_bem_pat.ind_orig_calc_bem_pat <> "UniÆo" /*l_uniao*/  and
                    movto_bem_pat.ind_orig_calc_bem_pat <> "Rateio" /*l_rateio*/ 
                then NEXT.
            end /* if */.
    
            if  movto_bem_pat.ind_trans_calc_bem_pat = "Implanta‡Æo" /*l_implantacao*/ 
            then do:
                RUN pi_movto_implantacao.
                RUN pi_movto_impl_dolar.
            end /* if */.
            else do:
                if  movto_bem_pat.ind_trans_calc_bem_pat = "Baixa" /*l_baixa*/ 
                then do:
                    RUN pi_movto_baixa.
                    RUN pi_movto_baixa_dolar.
                end /* if */.
                else if movto_bem_pat.ind_orig_calc_bem_pat = "Imparidade" /*l_imparidade*/  then do:
        
                    assign v_dat_sdo_bem_pat   = movto_bem_pat.dat_movto_bem_pat
                           v_val_original      = movto_bem_pat.val_origin_movto_bem_pat
                           v_val_origin_corrig = movto_bem_pat.val_origin_movto_bem_pat.
                end.
            end /* else */.
        
            RUN pi_depreciacao.
    
            RUN pi_gera_excel.
        END.
    END.

    chExcelApplication:application:DisplayAlerts = false.
    chExcelApplication:ActiveWorkbook:Saveas(INPUT FRAME {&FRAME-NAME} fi_arquivo,,,,,,).
    chExcelApplication:VISIBLE = TRUE.

    RELEASE object chExcelApplication.      
    RELEASE object chWorkbook.
    RELEASE object chWorksheet.

end.

run pi-finalizar in h-acomp. 

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_gera_excel C-Win 
PROCEDURE pi_gera_excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN chExcelApplication:Range("A" + STRING (i-linha)):VALUE = bem_pat.cod_cta_pat
       chExcelApplication:Range("B" + STRING (i-linha)):VALUE = bem_pat.num_bem_pat
       chExcelApplication:Range("C" + STRING (i-linha)):VALUE = bem_pat.num_seq_bem_pat
       chExcelApplication:Range("D" + STRING (i-linha)):VALUE = movto_bem_pat.num_seq_incorp_bem_pat
       chExcelApplication:Range("E" + STRING (i-linha)):VALUE = bem_pat.des_bem_pat
       chExcelApplication:Range("F" + STRING (i-linha)):VALUE = movto_bem_pat.dat_movto_bem_pat
       chExcelApplication:Range("G" + STRING (i-linha)):VALUE = bem_pat.cod_estab
       chExcelApplication:Range("H" + STRING (i-linha)):VALUE = bem_pat.cod_plano_ccusto
       chExcelApplication:Range("I" + STRING (i-linha)):VALUE = bem_pat.cod_ccusto_respons
       chExcelApplication:Range("J" + STRING (i-linha)):VALUE = movto_bem_pat.ind_trans_calc_bem_pat
       chExcelApplication:Range("K" + STRING (i-linha)):VALUE = movto_bem_pat.ind_orig_calc_bem_pat
       chExcelApplication:Range("L" + STRING (i-linha)):VALUE = bem_pat.dat_aquis_bem_pat
       chExcelApplication:Range("M" + STRING (i-linha)):VALUE = bem_pat.cod_grp_calc
       chExcelApplication:Range("N" + STRING (i-linha)):VALUE = bem_pat.cdn_fornecedor
       chExcelApplication:Range("O" + STRING (i-linha)):VALUE = v_val_pct_dpr
       chExcelApplication:Range("P" + STRING (i-linha)):VALUE = v_val_pct_dpr_incevda
       chExcelApplication:Range("Q" + STRING (i-linha)):VALUE = movto_bem_pat.val_perc_movto_bem_pat
       chExcelApplication:Range("R" + STRING (i-linha)):VALUE = v_val_original
       chExcelApplication:Range("S" + STRING (i-linha)):VALUE = v_val_origin_corrig
       chExcelApplication:Range("T" + STRING (i-linha)):VALUE = v_val_dpr
       chExcelApplication:Range("U" + STRING (i-linha)):VALUE = movto_bem_pat.qtd_movto_bem_pat
       chExcelApplication:Range("V" + STRING (i-linha)):VALUE = v_val_original_dolar
       chExcelApplication:Range("W" + STRING (i-linha)):VALUE = v_val_origin_corrig_dolar
       /* Begins : REV 02 */
       chExcelApplication:Range("X" + STRING (i-linha)):VALUE = v_val_dpr_dolar 
       /* End : REV 02 */ 
       chExcelApplication:Range("Y" + STRING (i-linha)):VALUE = bem_pat.cod_licenc_uso.
       .

/* Begins: Rev03 */
FIND FIRST bem_pat_item_docto_entr OF bem_pat NO-LOCK NO-ERROR.
IF AVAIL bem_pat_item_docto_entr THEN
DO:  
   FIND FIRST item_docto_entr WHERE 
              item_docto_entr.cod_estab           = bem_pat_item_docto_entr.cod_estab           AND 
              item_docto_entr.cod_empresa         = bem_pat_item_docto_entr.cod_empresa         AND
              item_docto_entr.cdn_fornecedor      = bem_pat_item_docto_entr.cdn_fornecedor      AND 
              item_docto_entr.cod_docto_entr      = bem_pat_item_docto_entr.cod_docto_entr      AND
              item_docto_entr.cod_ser_nota        = bem_pat_item_docto_entr.cod_ser_nota        AND 
              item_docto_entr.num_item_docto_entr = bem_pat_item_docto_entr.num_item_docto_entr NO-LOCK NO-ERROR.
   IF AVAIL item_docto_entr THEN
   DO:
      FIND FIRST docto_entr OF ITEM_docto_entr NO-LOCK NO-ERROR.
      IF AVAIL docto_entr THEN
         ASSIGN chExcelApplication:Range("Z"  + STRING (i-linha)):VALUE = docto_entr.cod_docto_entr      
                chExcelApplication:Range("AA" + STRING (i-linha)):VALUE = docto_entr.cod_ser_nota.
   END.
END.
/* End: Rev03 */

ASSIGN i-linha = i-linha + 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_inicio C-Win 
PROCEDURE pi_inicio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
assign fi_arquivo:screen-value in frame {&frame-name} = session:temp-directory + "esfas350ab.xls".

assign v_cod_dwb_program = "esfas350ab".

find first dwb_rpt_param no-lock where dwb_rpt_param.cod_dwb_user    = v_cod_usuar_corren 
                                   and dwb_rpt_param.cod_dwb_program = v_cod_dwb_program no-error.
if not avail dwb_rpt_param then do:

    create dwb_rpt_param.
    assign dwb_rpt_param.cod_dwb_user    = v_cod_usuar_corren
           dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
           dwb_rpt_param.cod_dwb_param   = ""                                        + chr(10) + /* 01 */
                                           "ZZZZZZZZZZZZZZZZZZ"                      + chr(10) + /* 02 */  
                                           STRING (TODAY,"99/99/9999")               + chr(10) + /* 03 */  
                                           STRING (TODAY,"99/99/9999")               + chr(10) + /* 04 */  
                                           ""                                        + chr(10) + /* 05 */  
                                           session:temp-directory + "esfas350ab.xls" + chr(10)   /* 06 */      
                                           .

END.

assign v_cod_recid_dwb = recid(dwb_rpt_param).

/* ------ inicia periodos  na tela -------- */
assign fi-cod_cta_pat-ini:screen-value in frame {&frame-name} = entry(1, dwb_rpt_param.cod_dwb_param,chr(10))
       fi-cod_cta_pat-fim:screen-value in frame {&frame-name} = entry(2, dwb_rpt_param.cod_dwb_param,chr(10))
       fi-data-ini       :screen-value in frame {&frame-name} = entry(3, dwb_rpt_param.cod_dwb_param,chr(10))
       fi-data-fim       :screen-value in frame {&frame-name} = entry(4, dwb_rpt_param.cod_dwb_param,chr(10))
       fi-cod_cenar_ctbl :screen-value in frame {&frame-name} = entry(5, dwb_rpt_param.cod_dwb_param,chr(10))
       fi_arquivo        :screen-value in frame {&frame-name} = entry(6, dwb_rpt_param.cod_dwb_param,chr(10))
       .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_messages C-Win 
PROCEDURE pi_messages :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input param c_action    as char    no-undo. 
    def input param i_msg       as integer no-undo. 
    def input param c_param     as char    no-undo. 

    def var c_prg_msg           as char    no-undo. 

    assign c_prg_msg = "messages/" 
                     + string(trunc(i_msg / 1000,0),"99") 
                     + "/msg" 
                     + string(i_msg, "99999"). 

    if search(c_prg_msg + ".r") = ? and search(c_prg_msg + ".p") = ? then do: 
        message "Mensagem nr. " i_msg "!!!" skip 
                "Programa Mensagem" c_prg_msg "n’o encontrado." 
                view-as alert-box error. 
        return error. 
    end. 

    run value(c_prg_msg + ".p") (input c_action, input c_param). 
    return return-value.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_movto_baixa C-Win 
PROCEDURE pi_movto_baixa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

                assign v_val_dpr           = 0
                       v_val_original      = 0
                       v_val_origin_corrig = 0.
                if  movto_bem_pat.ind_orig_calc_bem_pat <> "Transferˆncia" /*l_transferencia*/ 
                then do:
                    for each reg_calc_bem_pat no-lock
                        where reg_calc_bem_pat.num_id_bem_pat       = movto_bem_pat.num_id_bem_pat
                        and reg_calc_bem_pat.num_seq_incorp_bem_pat = movto_bem_pat.num_seq_incorp_bem_pat
                        and reg_calc_bem_pat.cod_cenar_ctbl         = INPUT FRAME {&FRAME-NAME} fi-cod_cenar_ctbl
                        and reg_calc_bem_pat.cod_finalid_econ       = 'Corrente'
                        and reg_calc_bem_pat.dat_calc_pat           = movto_bem_pat.dat_movto_bem_pat
                        and reg_calc_bem_pat.ind_trans_calc_bem_pat = movto_bem_pat.ind_trans_calc_bem_pat
                        and reg_calc_bem_pat.ind_orig_calc_bem_pat  = movto_bem_pat.ind_orig_calc_bem_pat /* cl_regcalc_ttmovto of reg_calc_bem_pat*/
                        break by reg_calc_bem_pat.num_id_bem_pat.
    
                             /* Begin_Include: i_p30_rpt_bem_pat_sint_movto */
                             if  reg_calc_bem_pat.ind_tip_calc = "Deprecia‡Æo" /*l_depreciacao*/ or
                                  reg_calc_bem_pat.ind_tip_calc = "Amortiza‡Æo" /*l_amortizacao*/ 
                             then do:
                                  assign v_val_dpr              = reg_calc_bem_pat.val_dpr_val_origin + reg_calc_bem_pat.val_dpr_cm + reg_calc_bem_pat.val_cm_dpr.
                             end /* if */.
    
                             /* FUT1082 - 08/07/2002
                             Altera‡Æo feita para nÆo lan»ar no relat½rio o valor original do Bem com 0. */
                             if  reg_calc_bem_pat.ind_tip_calc = "" or
                                 movto_bem_pat.ind_orig_calc_bem_pat = "Altera‡Æo" /*l_alteracao*/  or
                                 movto_bem_pat.ind_orig_calc_bem_pat = "Transferˆncia" /*l_transferencia*/  then do:
                                 assign v_dat_sdo_bem_pat           = reg_calc_bem_pat.dat_calc
                                        v_val_original              = reg_calc_bem_pat.val_original.
                                 if movto_bem_pat.ind_orig_calc_bem_pat = "Transferˆncia" /*l_transferencia*/  then
                                     assign v_val_origin_corrig = reg_calc_bem_pat.val_origin_corrig.
                             end.
    
                             if movto_bem_pat.ind_orig_calc_bem_pat = "Reclassifica‡Æo" /*l_reclassificacao*/  then do:
                                /* Se ² reclassifica‡Æo pega o valor corrigido da sdo_bem_pat*/
                                find first sdo_bem_pat   
                                    where sdo_bem_pat.num_id_bem_pat         = reg_calc_bem_pat.num_id_bem_pat
                                    and   sdo_bem_pat.num_seq_incorp_bem_pat = reg_calc_bem_pat.num_seq_incorp_bem_pat
                                    and   sdo_bem_pat.cod_cenar_ctbl         = reg_calc_bem_pat.cod_cenar_ctbl
                                    and   sdo_bem_pat.cod_finalid_econ       = reg_calc_bem_pat.cod_finalid_econ
                                    and   sdo_bem_pat.dat_sdo_bem_pat        <= reg_calc_bem_pat.dat_calc_pat
                                    no-lock no-error.
                                if avail sdo_bem_pat then
                                   assign v_val_origin_corrig = sdo_bem_pat.val_origin_corrig.
                             end.      
                             else do:
                                if movto_bem_pat.ind_orig_calc_bem_pat = "Altera‡Æo" /*l_alteracao*/  then
                                    assign v_val_origin_corrig = reg_calc_bem_pat.val_original + reg_calc_bem_pat.val_cm. /* Quando altera‡Æo pega da reg_calc_bem_pat pois nÆo tem sdo_bem_pat */
                                else do:
                                    if movto_bem_pat.ind_orig_calc_bem_pat <> "Transferˆncia" /*l_transferencia*/  then do:
                                        find first sdo_bem_pat   
                                            where sdo_bem_pat.num_id_bem_pat         = reg_calc_bem_pat.num_id_bem_pat
                                            and   sdo_bem_pat.num_seq_incorp_bem_pat = reg_calc_bem_pat.num_seq_incorp_bem_pat
                                            and   sdo_bem_pat.cod_cenar_ctbl         = reg_calc_bem_pat.cod_cenar_ctbl
                                            and   sdo_bem_pat.cod_finalid_econ       = reg_calc_bem_pat.cod_finalid_econ
                                            and   sdo_bem_pat.dat_sdo_bem_pat        = reg_calc_bem_pat.dat_calc_pat
                                            no-lock no-error.
                                        if avail sdo_bem_pat then do:
                                            find first b_sdo_bem_pat   
                                                where b_sdo_bem_pat.num_id_bem_pat         = reg_calc_bem_pat.num_id_bem_pat
                                                and   b_sdo_bem_pat.num_seq_incorp_bem_pat = reg_calc_bem_pat.num_seq_incorp_bem_pat
                                                and   b_sdo_bem_pat.cod_cenar_ctbl         = reg_calc_bem_pat.cod_cenar_ctbl
                                                and   b_sdo_bem_pat.cod_finalid_econ       = reg_calc_bem_pat.cod_finalid_econ
                                                and   b_sdo_bem_pat.dat_sdo_bem_pat        > reg_calc_bem_pat.dat_calc_pat
                                                no-lock no-error.               
                                            if avail b_sdo_bem_pat then  
                                               assign v_val_origin_corrig = sdo_bem_pat.val_origin_corrig - b_sdo_bem_pat.val_origin_corrig.
                                        end.
                                        else do:
                                            find last sdo_bem_pat   
                                                where sdo_bem_pat.num_id_bem_pat         = reg_calc_bem_pat.num_id_bem_pat
                                                and   sdo_bem_pat.num_seq_incorp_bem_pat = reg_calc_bem_pat.num_seq_incorp_bem_pat
                                                and   sdo_bem_pat.cod_cenar_ctbl         = reg_calc_bem_pat.cod_cenar_ctbl
                                                and   sdo_bem_pat.cod_finalid_econ       = reg_calc_bem_pat.cod_finalid_econ
                                                and   sdo_bem_pat.dat_sdo_bem_pat       <= reg_calc_bem_pat.dat_calc_pat
                                                and   sdo_bem_pat.num_seq_movto_bem_pat < 10000000
                                                no-lock no-error.
                                            if avail sdo_bem_pat then do:
                                                find first b_sdo_bem_pat   
                                                   where b_sdo_bem_pat.num_id_bem_pat         = reg_calc_bem_pat.num_id_bem_pat
                                                   and   b_sdo_bem_pat.num_seq_incorp_bem_pat = reg_calc_bem_pat.num_seq_incorp_bem_pat
                                                   and   b_sdo_bem_pat.cod_cenar_ctbl         = reg_calc_bem_pat.cod_cenar_ctbl
                                                   and   b_sdo_bem_pat.cod_finalid_econ       = reg_calc_bem_pat.cod_finalid_econ
                                                   and   b_sdo_bem_pat.dat_sdo_bem_pat        > reg_calc_bem_pat.dat_calc_pat
                                                   no-lock no-error.               
                                               if avail b_sdo_bem_pat then  
                                                   assign v_val_origin_corrig = sdo_bem_pat.val_origin_corrig - b_sdo_bem_pat.val_origin_corrig.
                                            end.
                                        end.
                                    end.
                                end.   
                             end.
    
                    end /* for Reg_calc_bem_pat */.
                end /* if */.
                else do:
                    assign v_dat_movto_bem_pat = ?.
                    /* caso o bem esteja 100% dpr nÆo irÿ gerar mais reg_calc, sendo assim ² utilizada esse 
                    l½gica para pegar o œltimo reg_calc_com valor */
                    if not can-find(first Reg_calc_bem_pat
                        where reg_calc_bem_pat.num_id_bem_pat         = movto_bem_pat.num_id_bem_pat
                        and   reg_calc_bem_pat.num_seq_incorp_bem_pat = movto_bem_pat.num_seq_incorp_bem_pat
                        and   reg_calc_bem_pat.cod_cenar_ctbl         = INPUT FRAME {&FRAME-NAME} fi-cod_cenar_ctbl
                        and   reg_calc_bem_pat.cod_finalid_econ       = 'Corrente'
                        and   reg_calc_bem_pat.dat_calc_pat           = movto_bem_pat.dat_movto_bem_pat - 1) then do:                        
                        find first reg_calc_bem_pat no-lock
                            where reg_calc_bem_pat.num_id_bem_pat         = movto_bem_pat.num_id_bem_pat
                            and   reg_calc_bem_pat.num_seq_incorp_bem_pat = movto_bem_pat.num_seq_incorp_bem_pat
                            and   reg_calc_bem_pat.cod_cenar_ctbl         = INPUT FRAME {&FRAME-NAME} fi-cod_cenar_ctbl
                            and   reg_calc_bem_pat.cod_finalid_econ       = 'Corrente'
                            and   reg_calc_bem_pat.cod_tip_calc           <> ""
                            and   reg_calc_bem_pat.dat_calc_pat           <= movto_bem_pat.dat_movto_bem_pat - 1 no-error.
                        if avail reg_calc_bem_pat then do:
                            assign v_dat_sdo_bem_pat             = reg_calc_bem_pat.dat_calc
                                   v_val_original                = reg_calc_bem_pat.val_original
                                   v_dat_movto_bem_pat           = reg_calc_bem_pat.dat_calc_pat.
                        end.
                        else do:
                            /* Alterado para pegar o ultimo reg_calc com valor, menor ou igual a data de movto_bem_pat
                               independente do tipo de calculo. */
                            find last reg_calc_bem_pat no-lock
                                where reg_calc_bem_pat.num_id_bem_pat         = movto_bem_pat.num_id_bem_pat
                                and   reg_calc_bem_pat.num_seq_incorp_bem_pat = movto_bem_pat.num_seq_incorp_bem_pat
                                and   reg_calc_bem_pat.cod_cenar_ctbl         = INPUT FRAME {&FRAME-NAME} fi-cod_cenar_ctbl
                                and   reg_calc_bem_pat.cod_finalid_econ       = 'Corrente'
                                and   reg_calc_bem_pat.dat_calc_pat          <= movto_bem_pat.dat_movto_bem_pat no-error.
                            if avail reg_calc_bem_pat then
                                assign v_dat_movto_bem_pat = reg_calc_bem_pat.dat_calc_pat.
                        end.
                    end.
                    if  v_dat_movto_bem_pat = ? then
                        assign v_dat_movto_bem_pat = movto_bem_pat.dat_movto_bem_pat - 1.
                    Reg_calc_bem_pat:
                    for each reg_calc_bem_pat no-lock
                        where reg_calc_bem_pat.num_id_bem_pat         = movto_bem_pat.num_id_bem_pat
                        and   reg_calc_bem_pat.num_seq_incorp_bem_pat = movto_bem_pat.num_seq_incorp_bem_pat
                        and   reg_calc_bem_pat.cod_cenar_ctbl         = INPUT FRAME {&FRAME-NAME} fi-cod_cenar_ctbl
                        and   reg_calc_bem_pat.cod_finalid_econ       = 'Corrente'
                        and   reg_calc_bem_pat.dat_calc_pat           = v_dat_movto_bem_pat
                        break by reg_calc_bem_pat.num_id_bem_pat: 
    
                        /* Begin_Include: i_p30_rpt_bem_pat_sint_movto */
                        if  reg_calc_bem_pat.ind_tip_calc = "Deprecia‡Æo" /*l_depreciacao*/ or
                             reg_calc_bem_pat.ind_tip_calc = "Amortiza‡Æo" /*l_amortizacao*/ 
                        then do:
                             assign v_val_dpr              = reg_calc_bem_pat.val_dpr_val_origin + reg_calc_bem_pat.val_dpr_cm + reg_calc_bem_pat.val_cm_dpr.
                        end /* if */.
    
                        /* FUT1082 - 08/07/2002
                        Altera‡Æo feita para nÆo lan»ar no relat½rio o valor original do Bem com 0. */
                        if  reg_calc_bem_pat.ind_tip_calc = "" or
                            movto_bem_pat.ind_orig_calc_bem_pat = "Altera‡Æo" /*l_alteracao*/  or
                            movto_bem_pat.ind_orig_calc_bem_pat = "Transferˆncia" /*l_transferencia*/  then do:
                            assign v_dat_sdo_bem_pat           = reg_calc_bem_pat.dat_calc
                                   v_val_original              = reg_calc_bem_pat.val_original.
                            if movto_bem_pat.ind_orig_calc_bem_pat = "Transferˆncia" /*l_transferencia*/  then
                                assign v_val_origin_corrig = reg_calc_bem_pat.val_origin_corrig.
                        end.
    
                        if movto_bem_pat.ind_orig_calc_bem_pat = "Reclassifica‡Æo" /*l_reclassificacao*/  then do:
                           /* Se ² reclassifica‡Æo pega o valor corrigido da sdo_bem_pat*/
                           find first sdo_bem_pat   
                               where sdo_bem_pat.num_id_bem_pat         = reg_calc_bem_pat.num_id_bem_pat
                               and   sdo_bem_pat.num_seq_incorp_bem_pat = reg_calc_bem_pat.num_seq_incorp_bem_pat
                               and   sdo_bem_pat.cod_cenar_ctbl         = reg_calc_bem_pat.cod_cenar_ctbl
                               and   sdo_bem_pat.cod_finalid_econ       = reg_calc_bem_pat.cod_finalid_econ
                               and   sdo_bem_pat.dat_sdo_bem_pat        = reg_calc_bem_pat.dat_calc_pat
                               no-lock no-error.
                           if avail sdo_bem_pat then
                              assign v_val_origin_corrig = sdo_bem_pat.val_origin_corrig.
                        end.      
                        else do:
                           if movto_bem_pat.ind_orig_calc_bem_pat = "Altera‡Æo" /*l_alteracao*/  then
                               assign v_val_origin_corrig = reg_calc_bem_pat.val_original + reg_calc_bem_pat.val_cm. /* Quando altera‡Æo pega da reg_calc_bem_pat pois nÆo tem sdo_bem_pat */
                           else do:
                               if movto_bem_pat.ind_orig_calc_bem_pat <> "Transferˆncia" /*l_transferencia*/  then do:
                                   find first sdo_bem_pat   
                                       where sdo_bem_pat.num_id_bem_pat         = reg_calc_bem_pat.num_id_bem_pat
                                       and   sdo_bem_pat.num_seq_incorp_bem_pat = reg_calc_bem_pat.num_seq_incorp_bem_pat
                                       and   sdo_bem_pat.cod_cenar_ctbl         = reg_calc_bem_pat.cod_cenar_ctbl
                                       and   sdo_bem_pat.cod_finalid_econ       = reg_calc_bem_pat.cod_finalid_econ
                                       and   sdo_bem_pat.dat_sdo_bem_pat        = reg_calc_bem_pat.dat_calc_pat
                                       no-lock no-error.
                                   if avail sdo_bem_pat then do:
                                       find first b_sdo_bem_pat   
                                           where b_sdo_bem_pat.num_id_bem_pat         = reg_calc_bem_pat.num_id_bem_pat
                                           and   b_sdo_bem_pat.num_seq_incorp_bem_pat = reg_calc_bem_pat.num_seq_incorp_bem_pat
                                           and   b_sdo_bem_pat.cod_cenar_ctbl         = reg_calc_bem_pat.cod_cenar_ctbl
                                           and   b_sdo_bem_pat.cod_finalid_econ       = reg_calc_bem_pat.cod_finalid_econ
                                           and   b_sdo_bem_pat.dat_sdo_bem_pat        > reg_calc_bem_pat.dat_calc_pat
                                           no-lock no-error.               
                                       if avail b_sdo_bem_pat then  
                                          assign v_val_origin_corrig = sdo_bem_pat.val_origin_corrig - b_sdo_bem_pat.val_origin_corrig.
                                   end.
                                   else do:
                                       find last sdo_bem_pat   
                                           where sdo_bem_pat.num_id_bem_pat         = reg_calc_bem_pat.num_id_bem_pat
                                           and   sdo_bem_pat.num_seq_incorp_bem_pat = reg_calc_bem_pat.num_seq_incorp_bem_pat
                                           and   sdo_bem_pat.cod_cenar_ctbl         = reg_calc_bem_pat.cod_cenar_ctbl
                                           and   sdo_bem_pat.cod_finalid_econ       = reg_calc_bem_pat.cod_finalid_econ
                                           and   sdo_bem_pat.dat_sdo_bem_pat       <= reg_calc_bem_pat.dat_calc_pat
                                           and   sdo_bem_pat.num_seq_movto_bem_pat < 10000000
                                           no-lock no-error.
                                       if avail sdo_bem_pat then do:
                                           find first b_sdo_bem_pat   
                                              where b_sdo_bem_pat.num_id_bem_pat         = reg_calc_bem_pat.num_id_bem_pat
                                              and   b_sdo_bem_pat.num_seq_incorp_bem_pat = reg_calc_bem_pat.num_seq_incorp_bem_pat
                                              and   b_sdo_bem_pat.cod_cenar_ctbl         = reg_calc_bem_pat.cod_cenar_ctbl
                                              and   b_sdo_bem_pat.cod_finalid_econ       = reg_calc_bem_pat.cod_finalid_econ
                                              and   b_sdo_bem_pat.dat_sdo_bem_pat        > reg_calc_bem_pat.dat_calc_pat
                                              no-lock no-error.               
                                          if avail b_sdo_bem_pat then  
                                              assign v_val_origin_corrig = sdo_bem_pat.val_origin_corrig - b_sdo_bem_pat.val_origin_corrig.
                                       end.
                                   end.
                               end.
                           end.   
                        end.
    
                    end /* for Reg_calc_bem_pat */.
                end /* else */.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_movto_baixa_dolar C-Win 
PROCEDURE pi_movto_baixa_dolar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

                assign v_val_dpr_dolar           = 0
                       v_val_original_dolar      = 0
                       v_val_origin_corrig_dolar = 0.
                if  movto_bem_pat.ind_orig_calc_bem_pat <> "Transferˆncia" /*l_transferencia*/ 
                then do:
                    for each reg_calc_bem_pat no-lock
                        where reg_calc_bem_pat.num_id_bem_pat       = movto_bem_pat.num_id_bem_pat
                        and reg_calc_bem_pat.num_seq_incorp_bem_pat = movto_bem_pat.num_seq_incorp_bem_pat
                        and reg_calc_bem_pat.cod_cenar_ctbl         = INPUT FRAME {&FRAME-NAME} fi-cod_cenar_ctbl
                        and reg_calc_bem_pat.cod_finalid_econ       = 'Dolar'
                        and reg_calc_bem_pat.dat_calc_pat           = movto_bem_pat.dat_movto_bem_pat
                        and reg_calc_bem_pat.ind_trans_calc_bem_pat = movto_bem_pat.ind_trans_calc_bem_pat
                        and reg_calc_bem_pat.ind_orig_calc_bem_pat  = movto_bem_pat.ind_orig_calc_bem_pat /* cl_regcalc_ttmovto of reg_calc_bem_pat*/
                        break by reg_calc_bem_pat.num_id_bem_pat.
    
                             /* Begin_Include: i_p30_rpt_bem_pat_sint_movto */
                             if  reg_calc_bem_pat.ind_tip_calc = "Deprecia‡Æo" /*l_depreciacao*/ or
                                  reg_calc_bem_pat.ind_tip_calc = "Amortiza‡Æo" /*l_amortizacao*/ 
                             then do:
                                  assign v_val_dpr_dolar              = reg_calc_bem_pat.val_dpr_val_origin + reg_calc_bem_pat.val_dpr_cm + reg_calc_bem_pat.val_cm_dpr.
                             end /* if */.
    
                             /* FUT1082 - 08/07/2002
                             Altera‡Æo feita para nÆo lan»ar no relat½rio o valor original do Bem com 0. */
                             if  reg_calc_bem_pat.ind_tip_calc = "" or
                                 movto_bem_pat.ind_orig_calc_bem_pat = "Altera‡Æo" /*l_alteracao*/  or
                                 movto_bem_pat.ind_orig_calc_bem_pat = "Transferˆncia" /*l_transferencia*/  then do:
                                 assign v_dat_sdo_bem_pat           = reg_calc_bem_pat.dat_calc
                                        v_val_original_dolar              = reg_calc_bem_pat.val_original.
                                 if movto_bem_pat.ind_orig_calc_bem_pat = "Transferˆncia" /*l_transferencia*/  then
                                     assign v_val_origin_corrig_dolar = reg_calc_bem_pat.val_origin_corrig.
                             end.
    
                             if movto_bem_pat.ind_orig_calc_bem_pat = "Reclassifica‡Æo" /*l_reclassificacao*/  then do:
                                /* Se ² reclassifica‡Æo pega o valor corrigido da sdo_bem_pat*/
                                find first sdo_bem_pat   
                                    where sdo_bem_pat.num_id_bem_pat         = reg_calc_bem_pat.num_id_bem_pat
                                    and   sdo_bem_pat.num_seq_incorp_bem_pat = reg_calc_bem_pat.num_seq_incorp_bem_pat
                                    and   sdo_bem_pat.cod_cenar_ctbl         = reg_calc_bem_pat.cod_cenar_ctbl
                                    and   sdo_bem_pat.cod_finalid_econ       = reg_calc_bem_pat.cod_finalid_econ
                                    and   sdo_bem_pat.dat_sdo_bem_pat        <= reg_calc_bem_pat.dat_calc_pat
                                    no-lock no-error.
                                if avail sdo_bem_pat then
                                   assign v_val_origin_corrig_dolar = sdo_bem_pat.val_origin_corrig.
                             end.      
                             else do:
                                if movto_bem_pat.ind_orig_calc_bem_pat = "Altera‡Æo" /*l_alteracao*/  then
                                    assign v_val_origin_corrig_dolar = reg_calc_bem_pat.val_original + reg_calc_bem_pat.val_cm. /* Quando altera‡Æo pega da reg_calc_bem_pat pois nÆo tem sdo_bem_pat */
                                else do:
                                    if movto_bem_pat.ind_orig_calc_bem_pat <> "Transferˆncia" /*l_transferencia*/  then do:
                                        find first sdo_bem_pat   
                                            where sdo_bem_pat.num_id_bem_pat         = reg_calc_bem_pat.num_id_bem_pat
                                            and   sdo_bem_pat.num_seq_incorp_bem_pat = reg_calc_bem_pat.num_seq_incorp_bem_pat
                                            and   sdo_bem_pat.cod_cenar_ctbl         = reg_calc_bem_pat.cod_cenar_ctbl
                                            and   sdo_bem_pat.cod_finalid_econ       = reg_calc_bem_pat.cod_finalid_econ
                                            and   sdo_bem_pat.dat_sdo_bem_pat        = reg_calc_bem_pat.dat_calc_pat
                                            no-lock no-error.
                                        if avail sdo_bem_pat then do:
                                            find first b_sdo_bem_pat   
                                                where b_sdo_bem_pat.num_id_bem_pat         = reg_calc_bem_pat.num_id_bem_pat
                                                and   b_sdo_bem_pat.num_seq_incorp_bem_pat = reg_calc_bem_pat.num_seq_incorp_bem_pat
                                                and   b_sdo_bem_pat.cod_cenar_ctbl         = reg_calc_bem_pat.cod_cenar_ctbl
                                                and   b_sdo_bem_pat.cod_finalid_econ       = reg_calc_bem_pat.cod_finalid_econ
                                                and   b_sdo_bem_pat.dat_sdo_bem_pat        > reg_calc_bem_pat.dat_calc_pat
                                                no-lock no-error.               
                                            if avail b_sdo_bem_pat then  
                                               assign v_val_origin_corrig_dolar = sdo_bem_pat.val_origin_corrig - b_sdo_bem_pat.val_origin_corrig.
                                        end.
                                        else do:
                                            find last sdo_bem_pat   
                                                where sdo_bem_pat.num_id_bem_pat         = reg_calc_bem_pat.num_id_bem_pat
                                                and   sdo_bem_pat.num_seq_incorp_bem_pat = reg_calc_bem_pat.num_seq_incorp_bem_pat
                                                and   sdo_bem_pat.cod_cenar_ctbl         = reg_calc_bem_pat.cod_cenar_ctbl
                                                and   sdo_bem_pat.cod_finalid_econ       = reg_calc_bem_pat.cod_finalid_econ
                                                and   sdo_bem_pat.dat_sdo_bem_pat       <= reg_calc_bem_pat.dat_calc_pat
                                                and   sdo_bem_pat.num_seq_movto_bem_pat < 10000000
                                                no-lock no-error.
                                            if avail sdo_bem_pat then do:
                                                find first b_sdo_bem_pat   
                                                   where b_sdo_bem_pat.num_id_bem_pat         = reg_calc_bem_pat.num_id_bem_pat
                                                   and   b_sdo_bem_pat.num_seq_incorp_bem_pat = reg_calc_bem_pat.num_seq_incorp_bem_pat
                                                   and   b_sdo_bem_pat.cod_cenar_ctbl         = reg_calc_bem_pat.cod_cenar_ctbl
                                                   and   b_sdo_bem_pat.cod_finalid_econ       = reg_calc_bem_pat.cod_finalid_econ
                                                   and   b_sdo_bem_pat.dat_sdo_bem_pat        > reg_calc_bem_pat.dat_calc_pat
                                                   no-lock no-error.               
                                               if avail b_sdo_bem_pat then  
                                                   assign v_val_origin_corrig_dolar = sdo_bem_pat.val_origin_corrig - b_sdo_bem_pat.val_origin_corrig.
                                            end.
                                        end.
                                    end.
                                end.   
                             end.
    
                    end /* for Reg_calc_bem_pat */.
                end /* if */.
                else do:
                    assign v_dat_movto_bem_pat = ?.
                    /* caso o bem esteja 100% dpr nÆo irÿ gerar mais reg_calc, sendo assim ² utilizada esse 
                    l½gica para pegar o œltimo reg_calc_com valor */
                    if not can-find(first Reg_calc_bem_pat
                        where reg_calc_bem_pat.num_id_bem_pat         = movto_bem_pat.num_id_bem_pat
                        and   reg_calc_bem_pat.num_seq_incorp_bem_pat = movto_bem_pat.num_seq_incorp_bem_pat
                        and   reg_calc_bem_pat.cod_cenar_ctbl         = INPUT FRAME {&FRAME-NAME} fi-cod_cenar_ctbl
                        and   reg_calc_bem_pat.cod_finalid_econ       = 'Dolar'
                        and   reg_calc_bem_pat.dat_calc_pat           = movto_bem_pat.dat_movto_bem_pat - 1) then do:                        
                        find first reg_calc_bem_pat no-lock
                            where reg_calc_bem_pat.num_id_bem_pat         = movto_bem_pat.num_id_bem_pat
                            and   reg_calc_bem_pat.num_seq_incorp_bem_pat = movto_bem_pat.num_seq_incorp_bem_pat
                            and   reg_calc_bem_pat.cod_cenar_ctbl         = INPUT FRAME {&FRAME-NAME} fi-cod_cenar_ctbl
                            and   reg_calc_bem_pat.cod_finalid_econ       = 'Dolar'
                            and   reg_calc_bem_pat.cod_tip_calc           <> ""
                            and   reg_calc_bem_pat.dat_calc_pat           <= movto_bem_pat.dat_movto_bem_pat - 1 no-error.
                        if avail reg_calc_bem_pat then do:
                            assign v_dat_sdo_bem_pat             = reg_calc_bem_pat.dat_calc
                                   v_val_original_dolar                = reg_calc_bem_pat.val_original
                                   v_dat_movto_bem_pat           = reg_calc_bem_pat.dat_calc_pat.
                        end.
                        else do:
                            /* Alterado para pegar o ultimo reg_calc com valor, menor ou igual a data de movto_bem_pat
                               independente do tipo de calculo. */
                            find last reg_calc_bem_pat no-lock
                                where reg_calc_bem_pat.num_id_bem_pat         = movto_bem_pat.num_id_bem_pat
                                and   reg_calc_bem_pat.num_seq_incorp_bem_pat = movto_bem_pat.num_seq_incorp_bem_pat
                                and   reg_calc_bem_pat.cod_cenar_ctbl         = INPUT FRAME {&FRAME-NAME} fi-cod_cenar_ctbl
                                and   reg_calc_bem_pat.cod_finalid_econ       = 'Dolar'
                                and   reg_calc_bem_pat.dat_calc_pat          <= movto_bem_pat.dat_movto_bem_pat no-error.
                            if avail reg_calc_bem_pat then
                                assign v_dat_movto_bem_pat = reg_calc_bem_pat.dat_calc_pat.
                        end.
                    end.
                    if  v_dat_movto_bem_pat = ? then
                        assign v_dat_movto_bem_pat = movto_bem_pat.dat_movto_bem_pat - 1.
                    Reg_calc_bem_pat:
                    for each reg_calc_bem_pat no-lock
                        where reg_calc_bem_pat.num_id_bem_pat         = movto_bem_pat.num_id_bem_pat
                        and   reg_calc_bem_pat.num_seq_incorp_bem_pat = movto_bem_pat.num_seq_incorp_bem_pat
                        and   reg_calc_bem_pat.cod_cenar_ctbl         = INPUT FRAME {&FRAME-NAME} fi-cod_cenar_ctbl
                        and   reg_calc_bem_pat.cod_finalid_econ       = 'Dolar'
                        and   reg_calc_bem_pat.dat_calc_pat           = v_dat_movto_bem_pat
                        break by reg_calc_bem_pat.num_id_bem_pat: 
    
                        /* Begin_Include: i_p30_rpt_bem_pat_sint_movto */
                        if  reg_calc_bem_pat.ind_tip_calc = "Deprecia‡Æo" /*l_depreciacao*/ or
                             reg_calc_bem_pat.ind_tip_calc = "Amortiza‡Æo" /*l_amortizacao*/ 
                        then do:
                             assign v_val_dpr_dolar              = reg_calc_bem_pat.val_dpr_val_origin + reg_calc_bem_pat.val_dpr_cm + reg_calc_bem_pat.val_cm_dpr.
                        end /* if */.
    
                        /* FUT1082 - 08/07/2002
                        Altera‡Æo feita para nÆo lan»ar no relat½rio o valor original do Bem com 0. */
                        if  reg_calc_bem_pat.ind_tip_calc = "" or
                            movto_bem_pat.ind_orig_calc_bem_pat = "Altera‡Æo" /*l_alteracao*/  or
                            movto_bem_pat.ind_orig_calc_bem_pat = "Transferˆncia" /*l_transferencia*/  then do:
                            assign v_dat_sdo_bem_pat           = reg_calc_bem_pat.dat_calc
                                   v_val_original_dolar              = reg_calc_bem_pat.val_original.
                            if movto_bem_pat.ind_orig_calc_bem_pat = "Transferˆncia" /*l_transferencia*/  then
                                assign v_val_origin_corrig_dolar = reg_calc_bem_pat.val_origin_corrig.
                        end.
    
                        if movto_bem_pat.ind_orig_calc_bem_pat = "Reclassifica‡Æo" /*l_reclassificacao*/  then do:
                           /* Se ² reclassifica‡Æo pega o valor corrigido da sdo_bem_pat*/
                           find first sdo_bem_pat   
                               where sdo_bem_pat.num_id_bem_pat         = reg_calc_bem_pat.num_id_bem_pat
                               and   sdo_bem_pat.num_seq_incorp_bem_pat = reg_calc_bem_pat.num_seq_incorp_bem_pat
                               and   sdo_bem_pat.cod_cenar_ctbl         = reg_calc_bem_pat.cod_cenar_ctbl
                               and   sdo_bem_pat.cod_finalid_econ       = reg_calc_bem_pat.cod_finalid_econ
                               and   sdo_bem_pat.dat_sdo_bem_pat        = reg_calc_bem_pat.dat_calc_pat
                               no-lock no-error.
                           if avail sdo_bem_pat then
                              assign v_val_origin_corrig_dolar = sdo_bem_pat.val_origin_corrig.
                        end.      
                        else do:
                           if movto_bem_pat.ind_orig_calc_bem_pat = "Altera‡Æo" /*l_alteracao*/  then
                               assign v_val_origin_corrig_dolar = reg_calc_bem_pat.val_original + reg_calc_bem_pat.val_cm. /* Quando altera‡Æo pega da reg_calc_bem_pat pois nÆo tem sdo_bem_pat */
                           else do:
                               if movto_bem_pat.ind_orig_calc_bem_pat <> "Transferˆncia" /*l_transferencia*/  then do:
                                   find first sdo_bem_pat   
                                       where sdo_bem_pat.num_id_bem_pat         = reg_calc_bem_pat.num_id_bem_pat
                                       and   sdo_bem_pat.num_seq_incorp_bem_pat = reg_calc_bem_pat.num_seq_incorp_bem_pat
                                       and   sdo_bem_pat.cod_cenar_ctbl         = reg_calc_bem_pat.cod_cenar_ctbl
                                       and   sdo_bem_pat.cod_finalid_econ       = reg_calc_bem_pat.cod_finalid_econ
                                       and   sdo_bem_pat.dat_sdo_bem_pat        = reg_calc_bem_pat.dat_calc_pat
                                       no-lock no-error.
                                   if avail sdo_bem_pat then do:
                                       find first b_sdo_bem_pat   
                                           where b_sdo_bem_pat.num_id_bem_pat         = reg_calc_bem_pat.num_id_bem_pat
                                           and   b_sdo_bem_pat.num_seq_incorp_bem_pat = reg_calc_bem_pat.num_seq_incorp_bem_pat
                                           and   b_sdo_bem_pat.cod_cenar_ctbl         = reg_calc_bem_pat.cod_cenar_ctbl
                                           and   b_sdo_bem_pat.cod_finalid_econ       = reg_calc_bem_pat.cod_finalid_econ
                                           and   b_sdo_bem_pat.dat_sdo_bem_pat        > reg_calc_bem_pat.dat_calc_pat
                                           no-lock no-error.               
                                       if avail b_sdo_bem_pat then  
                                          assign v_val_origin_corrig_dolar = sdo_bem_pat.val_origin_corrig - b_sdo_bem_pat.val_origin_corrig.
                                   end.
                                   else do:
                                       find last sdo_bem_pat   
                                           where sdo_bem_pat.num_id_bem_pat         = reg_calc_bem_pat.num_id_bem_pat
                                           and   sdo_bem_pat.num_seq_incorp_bem_pat = reg_calc_bem_pat.num_seq_incorp_bem_pat
                                           and   sdo_bem_pat.cod_cenar_ctbl         = reg_calc_bem_pat.cod_cenar_ctbl
                                           and   sdo_bem_pat.cod_finalid_econ       = reg_calc_bem_pat.cod_finalid_econ
                                           and   sdo_bem_pat.dat_sdo_bem_pat       <= reg_calc_bem_pat.dat_calc_pat
                                           and   sdo_bem_pat.num_seq_movto_bem_pat < 10000000
                                           no-lock no-error.
                                       if avail sdo_bem_pat then do:
                                           find first b_sdo_bem_pat   
                                              where b_sdo_bem_pat.num_id_bem_pat         = reg_calc_bem_pat.num_id_bem_pat
                                              and   b_sdo_bem_pat.num_seq_incorp_bem_pat = reg_calc_bem_pat.num_seq_incorp_bem_pat
                                              and   b_sdo_bem_pat.cod_cenar_ctbl         = reg_calc_bem_pat.cod_cenar_ctbl
                                              and   b_sdo_bem_pat.cod_finalid_econ       = reg_calc_bem_pat.cod_finalid_econ
                                              and   b_sdo_bem_pat.dat_sdo_bem_pat        > reg_calc_bem_pat.dat_calc_pat
                                              no-lock no-error.               
                                          if avail b_sdo_bem_pat then  
                                              assign v_val_origin_corrig_dolar = sdo_bem_pat.val_origin_corrig - b_sdo_bem_pat.val_origin_corrig.
                                       end.
                                   end.
                               end.
                           end.   
                        end.
    
                    end /* for Reg_calc_bem_pat */.
                end /* else */.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_movto_implantacao C-Win 
PROCEDURE pi_movto_implantacao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


           if (movto_bem_pat.ind_orig_calc_bem_pat = "Reclassifica‡Æo" /*l_reclassificacao*/  or
               movto_bem_pat.ind_orig_calc_bem_pat = "Altera‡Æo" /*l_alteracao*/ ) then do:
              find first reg_calc_bem_pat no-lock
                 where reg_calc_bem_pat.num_id_bem_pat = movto_bem_pat.num_id_bem_pat
                   and reg_calc_bem_pat.num_seq_incorp_bem_pat = movto_bem_pat.num_seq_incorp_bem_pat
                   and reg_calc_bem_pat.cod_cenar_ctbl = INPUT FRAME {&FRAME-NAME} fi-cod_cenar_ctbl
                   and reg_calc_bem_pat.cod_finalid_econ = 'Corrente'
                   and reg_calc_bem_pat.dat_calc_pat = movto_bem_pat.dat_movto_bem_pat
                   and reg_calc_bem_pat.cod_tip_calc = ""
                   and reg_calc_bem_pat.ind_trans_calc_bem_pat = movto_bem_pat.ind_trans_calc_bem_pat
                    /* cl_b_reg_calc_bem of reg_calc_bem_pat*/ no-error.  
           end.            
           else       
              find first reg_calc_bem_pat no-lock
                   where reg_calc_bem_pat.num_id_bem_pat = movto_bem_pat.num_id_bem_pat
                     and reg_calc_bem_pat.num_seq_incorp_bem_pat = movto_bem_pat.num_seq_incorp_bem_pat
                     and reg_calc_bem_pat.cod_cenar_ctbl = INPUT FRAME {&FRAME-NAME} fi-cod_cenar_ctbl
                     and reg_calc_bem_pat.cod_finalid_econ = 'Corrente'
                     and reg_calc_bem_pat.dat_calc_pat = movto_bem_pat.dat_movto_bem_pat
                     and reg_calc_bem_pat.ind_trans_calc_bem_pat = movto_bem_pat.ind_trans_calc_bem_pat
                     and reg_calc_bem_pat.ind_orig_calc_bem_pat = movto_bem_pat.ind_orig_calc_bem_pat /*cl_b_regcalc_ttrptmovto of reg_calc_bem_pat*/ no-error.
    
           if  not avail reg_calc_bem_pat
           then do:
               if bem_pat.cod_contrat_leas <> "" then do:
                  find first reg_calc_bem_pat no-lock
                       where reg_calc_bem_pat.num_id_bem_pat = movto_bem_pat.num_id_bem_pat
                       and reg_calc_bem_pat.num_seq_incorp_bem_pat = movto_bem_pat.num_seq_incorp_bem_pat
                       and reg_calc_bem_pat.cod_cenar_ctbl = INPUT FRAME {&FRAME-NAME} fi-cod_cenar_ctbl
                       and reg_calc_bem_pat.cod_finalid_econ = 'Corrente'
                       and reg_calc_bem_pat.dat_calc_pat = movto_bem_pat.dat_movto_bem_pat
                       and reg_calc_bem_pat.ind_trans_calc_bem_pat = movto_bem_pat.ind_trans_calc_bem_pat
                       and reg_calc_bem_pat.ind_orig_calc_bem_pat = "Aquisi‡Æo" /*l_aquisicao*/  no-error.
               end.
               if  not avail reg_calc_bem_pat
               then do:
                find first reg_calc_bem_pat no-lock
                    where reg_calc_bem_pat.num_id_bem_pat          = movto_bem_pat.num_id_bem_pat
                    and   reg_calc_bem_pat.num_seq_incorp_bem_pat  = movto_bem_pat.num_seq_incorp_bem_pat
                    and   reg_calc_bem_pat.cod_cenar_ctbl          = INPUT FRAME {&FRAME-NAME} fi-cod_cenar_ctbl
                    and   reg_calc_bem_pat.cod_finalid_econ        = 'Corrente'
                    and   reg_calc_bem_pat.dat_calc_pat            = bem_pat.dat_calc_pat
                    and   reg_calc_bem_pat.ind_trans_calc_bem_pat  = movto_bem_pat.ind_trans_calc_bem_pat
                    and   ((reg_calc_bem_pat.ind_orig_calc_bem_pat = movto_bem_pat.ind_orig_calc_bem_pat 
                    and   movto_bem_pat.ind_orig_calc_bem_pat = "Reclassifica‡Æo" /*l_reclassificacao*/ 
                    and   reg_calc_bem_pat.cod_tip_calc            = " ") 
                    or    (reg_calc_bem_pat.ind_orig_calc_bem_pat = movto_bem_pat.ind_orig_calc_bem_pat 
                    and   movto_bem_pat.ind_orig_calc_bem_pat <> "Reclassifica‡Æo" /*l_reclassificacao*/ ))
                    no-error.
               end.
               if  not avail reg_calc_bem_pat
               then do:
                  find first reg_calc_bem_pat no-lock
                     where reg_calc_bem_pat.num_id_bem_pat = movto_bem_pat.num_id_bem_pat
                       and reg_calc_bem_pat.num_seq_incorp_bem_pat = movto_bem_pat.num_seq_incorp_bem_pat
                       and reg_calc_bem_pat.cod_cenar_ctbl = INPUT FRAME {&FRAME-NAME} fi-cod_cenar_ctbl
                       and reg_calc_bem_pat.cod_finalid_econ = 'Corrente'
                       and reg_calc_bem_pat.dat_calc_pat >= movto_bem_pat.dat_movto_bem_pat
                       and reg_calc_bem_pat.cod_tip_calc = ""
                       and reg_calc_bem_pat.ind_trans_calc_bem_pat = movto_bem_pat.ind_trans_calc_bem_pat
                       and reg_calc_bem_pat.ind_orig_calc_bem_pat = "Aquisi‡Æo" /*l_aquisicao*/  no-error.
               end.
            end /* if */.
            if  avail reg_calc_bem_pat
            then do:
    
                ASSIGN v_val_original = reg_calc_bem_pat.val_original.
    
                if movto_bem_pat.ind_orig_calc_bem_pat = "Reclassifica‡Æo" /*l_reclassificacao*/ 
                or movto_bem_pat.ind_orig_calc_bem_pat = "Desmembramento" /*l_desmembramento*/ 
                or movto_bem_pat.ind_orig_calc_bem_pat = "Imobilizado" /*l_imobilizado*/  then do:
                    /* Quando reclassifica‡Æo pega do sdo_bem_pat pois na reg_calc_bem_pat nÆo terÿ o valor corrigido */
                    find first sdo_bem_pat   
                        where sdo_bem_pat.num_id_bem_pat         = reg_calc_bem_pat.num_id_bem_pat
                        and   sdo_bem_pat.num_seq_incorp_bem_pat = reg_calc_bem_pat.num_seq_incorp_bem_pat
                        and   sdo_bem_pat.cod_cenar_ctbl         = reg_calc_bem_pat.cod_cenar_ctbl
                        and   sdo_bem_pat.cod_finalid_econ       = reg_calc_bem_pat.cod_finalid_econ
                        and   sdo_bem_pat.dat_sdo_bem_pat        = reg_calc_bem_pat.dat_calc_pat
                        no-lock no-error.
                    if avail sdo_bem_pat then
                       assign v_val_origin_corrig = sdo_bem_pat.val_origin_corrig.   
                end.       
                else do:
                    assign v_val_origin_corrig = reg_calc_bem_pat.val_origin_corrig. /* Quando altera‡Æo pega do reg_calc_bem_pat pois nÆo tem sdo_bem_pat */
                end.
    
                if  reg_calc_bem_pat.ind_orig_calc_bem_pat = "Reclassifica‡Æo" /*l_reclassificacao*/  or
                      reg_calc_bem_pat.ind_orig_calc_bem_pat = "Desmembramento" /*l_desmembramento*/  or
                      reg_calc_bem_pat.ind_orig_calc_bem_pat = "UniÆo" /*l_uniao*/  or 
                      reg_calc_bem_pat.ind_orig_calc_bem_pat = "Altera‡Æo" /*l_alteracao*/  or
                      reg_calc_bem_pat.ind_orig_calc_bem_pat = "Imobilizado" /*l_imobilizado*/ 
                then do:
                    reg_block:
                    for each reg_calc_bem_pat no-lock
                        where reg_calc_bem_pat.num_id_bem_pat         = movto_bem_pat.num_id_bem_pat
                        and   reg_calc_bem_pat.num_seq_incorp_bem_pat = movto_bem_pat.num_seq_incorp_bem_pat
                        and   reg_calc_bem_pat.cod_cenar_ctbl         = INPUT FRAME {&FRAME-NAME} fi-cod_cenar_ctbl
                        and   reg_calc_bem_pat.cod_finalid_econ       = 'Corrente'
                        and   reg_calc_bem_pat.dat_calc_pat           = movto_bem_pat.dat_movto_bem_pat
                        /* and   reg_calc_bem_pat.ind_trans_calc_bem_pat = movto_bem_pat.ind_trans_calc_bem_pat*/
                        and   reg_calc_bem_pat.ind_orig_calc_bem_pat  = movto_bem_pat.ind_orig_calc_bem_pat:
                        if  reg_calc_bem_pat.ind_tip_calc = "Deprecia‡Æo" /*l_depreciacao*/  or
                             reg_calc_bem_pat.ind_tip_calc = "Amortiza‡Æo" /*l_amortizacao*/ 
                        then do:
                            assign v_val_dpr    = reg_calc_bem_pat.val_dpr_val_origin +
                                                  reg_calc_bem_pat.val_dpr_cm         +
                                                  reg_calc_bem_pat.val_cm_dpr.
                        end /* if */.
                    end /* for reg_block */.
                end /* if */.
                /* End_Include: i_p20_rpt_bem_pat_sint_movto */
                .
            end /* if */.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_movto_impl_dolar C-Win 
PROCEDURE pi_movto_impl_dolar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

           if (movto_bem_pat.ind_orig_calc_bem_pat = "Reclassifica‡Æo" /*l_reclassificacao*/  or
               movto_bem_pat.ind_orig_calc_bem_pat = "Altera‡Æo" /*l_alteracao*/ ) then do:
              find first reg_calc_bem_pat no-lock
                 where reg_calc_bem_pat.num_id_bem_pat = movto_bem_pat.num_id_bem_pat
                   and reg_calc_bem_pat.num_seq_incorp_bem_pat = movto_bem_pat.num_seq_incorp_bem_pat
                   and reg_calc_bem_pat.cod_cenar_ctbl = INPUT FRAME {&FRAME-NAME} fi-cod_cenar_ctbl
                   and reg_calc_bem_pat.cod_finalid_econ = 'Dolar'
                   and reg_calc_bem_pat.dat_calc_pat = movto_bem_pat.dat_movto_bem_pat
                   and reg_calc_bem_pat.cod_tip_calc = ""
                   and reg_calc_bem_pat.ind_trans_calc_bem_pat = movto_bem_pat.ind_trans_calc_bem_pat
                    /* cl_b_reg_calc_bem of reg_calc_bem_pat*/ no-error.  
           end.            
           else       
              find first reg_calc_bem_pat no-lock
                   where reg_calc_bem_pat.num_id_bem_pat = movto_bem_pat.num_id_bem_pat
                     and reg_calc_bem_pat.num_seq_incorp_bem_pat = movto_bem_pat.num_seq_incorp_bem_pat
                     and reg_calc_bem_pat.cod_cenar_ctbl = INPUT FRAME {&FRAME-NAME} fi-cod_cenar_ctbl
                     and reg_calc_bem_pat.cod_finalid_econ = 'Dolar'
                     and reg_calc_bem_pat.dat_calc_pat = movto_bem_pat.dat_movto_bem_pat
                     and reg_calc_bem_pat.ind_trans_calc_bem_pat = movto_bem_pat.ind_trans_calc_bem_pat
                     and reg_calc_bem_pat.ind_orig_calc_bem_pat = movto_bem_pat.ind_orig_calc_bem_pat /*cl_b_regcalc_ttrptmovto of reg_calc_bem_pat*/ no-error.
    
           if  not avail reg_calc_bem_pat
           then do:
               if bem_pat.cod_contrat_leas <> "" then do:
                  find first reg_calc_bem_pat no-lock
                       where reg_calc_bem_pat.num_id_bem_pat = movto_bem_pat.num_id_bem_pat
                       and reg_calc_bem_pat.num_seq_incorp_bem_pat = movto_bem_pat.num_seq_incorp_bem_pat
                       and reg_calc_bem_pat.cod_cenar_ctbl = INPUT FRAME {&FRAME-NAME} fi-cod_cenar_ctbl
                       and reg_calc_bem_pat.cod_finalid_econ = 'Dolar'
                       and reg_calc_bem_pat.dat_calc_pat = movto_bem_pat.dat_movto_bem_pat
                       and reg_calc_bem_pat.ind_trans_calc_bem_pat = movto_bem_pat.ind_trans_calc_bem_pat
                       and reg_calc_bem_pat.ind_orig_calc_bem_pat = "Aquisi‡Æo" /*l_aquisicao*/  no-error.
               end.
               if  not avail reg_calc_bem_pat
               then do:
                find first reg_calc_bem_pat no-lock
                    where reg_calc_bem_pat.num_id_bem_pat          = movto_bem_pat.num_id_bem_pat
                    and   reg_calc_bem_pat.num_seq_incorp_bem_pat  = movto_bem_pat.num_seq_incorp_bem_pat
                    and   reg_calc_bem_pat.cod_cenar_ctbl          = INPUT FRAME {&FRAME-NAME} fi-cod_cenar_ctbl
                    and   reg_calc_bem_pat.cod_finalid_econ        = 'Dolar'
                    and   reg_calc_bem_pat.dat_calc_pat            = bem_pat.dat_calc_pat
                    and   reg_calc_bem_pat.ind_trans_calc_bem_pat  = movto_bem_pat.ind_trans_calc_bem_pat
                    and   ((reg_calc_bem_pat.ind_orig_calc_bem_pat = movto_bem_pat.ind_orig_calc_bem_pat 
                    and   movto_bem_pat.ind_orig_calc_bem_pat = "Reclassifica‡Æo" /*l_reclassificacao*/ 
                    and   reg_calc_bem_pat.cod_tip_calc            = " ") 
                    or    (reg_calc_bem_pat.ind_orig_calc_bem_pat = movto_bem_pat.ind_orig_calc_bem_pat 
                    and   movto_bem_pat.ind_orig_calc_bem_pat <> "Reclassifica‡Æo" /*l_reclassificacao*/ ))
                    no-error.
               end.
               if  not avail reg_calc_bem_pat
               then do:
                  find first reg_calc_bem_pat no-lock
                     where reg_calc_bem_pat.num_id_bem_pat = movto_bem_pat.num_id_bem_pat
                       and reg_calc_bem_pat.num_seq_incorp_bem_pat = movto_bem_pat.num_seq_incorp_bem_pat
                       and reg_calc_bem_pat.cod_cenar_ctbl = INPUT FRAME {&FRAME-NAME} fi-cod_cenar_ctbl
                       and reg_calc_bem_pat.cod_finalid_econ = 'Dolar'
                       and reg_calc_bem_pat.dat_calc_pat >= movto_bem_pat.dat_movto_bem_pat
                       and reg_calc_bem_pat.cod_tip_calc = ""
                       and reg_calc_bem_pat.ind_trans_calc_bem_pat = movto_bem_pat.ind_trans_calc_bem_pat
                       and reg_calc_bem_pat.ind_orig_calc_bem_pat = "Aquisi‡Æo" /*l_aquisicao*/  no-error.
               end.
            end /* if */.
            if  avail reg_calc_bem_pat
            then do:
    
                ASSIGN v_val_original_dolar = reg_calc_bem_pat.val_original.
    
                if movto_bem_pat.ind_orig_calc_bem_pat = "Reclassifica‡Æo" /*l_reclassificacao*/ 
                or movto_bem_pat.ind_orig_calc_bem_pat = "Desmembramento" /*l_desmembramento*/ 
                or movto_bem_pat.ind_orig_calc_bem_pat = "Imobilizado" /*l_imobilizado*/  then do:
                    /* Quando reclassifica‡Æo pega do sdo_bem_pat pois na reg_calc_bem_pat nÆo terÿ o valor corrigido */
                    find first sdo_bem_pat   
                        where sdo_bem_pat.num_id_bem_pat         = reg_calc_bem_pat.num_id_bem_pat
                        and   sdo_bem_pat.num_seq_incorp_bem_pat = reg_calc_bem_pat.num_seq_incorp_bem_pat
                        and   sdo_bem_pat.cod_cenar_ctbl         = reg_calc_bem_pat.cod_cenar_ctbl
                        and   sdo_bem_pat.cod_finalid_econ       = reg_calc_bem_pat.cod_finalid_econ
                        and   sdo_bem_pat.dat_sdo_bem_pat        = reg_calc_bem_pat.dat_calc_pat
                        no-lock no-error.
                    if avail sdo_bem_pat then
                       assign v_val_origin_corrig_dolar = sdo_bem_pat.val_origin_corrig.   
                end.       
                else do:
                    assign v_val_origin_corrig_dolar = reg_calc_bem_pat.val_origin_corrig. /* Quando altera‡Æo pega do reg_calc_bem_pat pois nÆo tem sdo_bem_pat */
                end.
    
                if  reg_calc_bem_pat.ind_orig_calc_bem_pat = "Reclassifica‡Æo" /*l_reclassificacao*/  or
                      reg_calc_bem_pat.ind_orig_calc_bem_pat = "Desmembramento" /*l_desmembramento*/  or
                      reg_calc_bem_pat.ind_orig_calc_bem_pat = "UniÆo" /*l_uniao*/  or 
                      reg_calc_bem_pat.ind_orig_calc_bem_pat = "Altera‡Æo" /*l_alteracao*/  or
                      reg_calc_bem_pat.ind_orig_calc_bem_pat = "Imobilizado" /*l_imobilizado*/ 
                then do:
                    reg_block:
                    for each reg_calc_bem_pat no-lock
                        where reg_calc_bem_pat.num_id_bem_pat         = movto_bem_pat.num_id_bem_pat
                        and   reg_calc_bem_pat.num_seq_incorp_bem_pat = movto_bem_pat.num_seq_incorp_bem_pat
                        and   reg_calc_bem_pat.cod_cenar_ctbl         = INPUT FRAME {&FRAME-NAME} fi-cod_cenar_ctbl
                        and   reg_calc_bem_pat.cod_finalid_econ       = 'Dolar'
                        and   reg_calc_bem_pat.dat_calc_pat           = movto_bem_pat.dat_movto_bem_pat
                        /* and   reg_calc_bem_pat.ind_trans_calc_bem_pat = movto_bem_pat.ind_trans_calc_bem_pat*/
                        and   reg_calc_bem_pat.ind_orig_calc_bem_pat  = movto_bem_pat.ind_orig_calc_bem_pat:
                        if  reg_calc_bem_pat.ind_tip_calc = "Deprecia‡Æo" /*l_depreciacao*/  or
                             reg_calc_bem_pat.ind_tip_calc = "Amortiza‡Æo" /*l_amortizacao*/ 
                        then do:
                            assign v_val_dpr_dolar    = reg_calc_bem_pat.val_dpr_val_origin +
                                                  reg_calc_bem_pat.val_dpr_cm         +
                                                  reg_calc_bem_pat.val_cm_dpr.
                        end /* if */.
                    end /* for reg_block */.
                end /* if */.
                /* End_Include: i_p20_rpt_bem_pat_sint_movto */
                .
            end /* if */.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi_salva C-Win 
PROCEDURE pi_salva :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

find current dwb_rpt_param exclusive-lock no-error.
if not avail dwb_rpt_param then 
    find first dwb_rpt_param exclusive-lock where recid(dwb_rpt_param) = v_cod_recid_dwb no-error.

if avail dwb_rpt_param then do:

    if fi_arquivo:screen-value in frame {&frame-name} = "" then 
        assign fi_arquivo:screen-value in frame {&frame-name} = session:temp-directory + "esfas350ab.xls".
        
    ASSIGN dwb_rpt_param.cod_dwb_param = fi-cod_cta_pat-ini:screen-value in frame {&frame-name} + chr(10) + /* 01 */
                                         fi-cod_cta_pat-fim:screen-value in frame {&frame-name} + chr(10) + /* 02 */  
                                         fi-data-ini       :screen-value in frame {&frame-name} + chr(10) + /* 03 */  
                                         fi-data-fim       :screen-value in frame {&frame-name} + chr(10) + /* 04 */  
                                         fi-cod_cenar_ctbl :screen-value in frame {&frame-name} + chr(10) + /* 05 */  
                                         fi_arquivo        :screen-value in frame {&frame-name} + chr(10)   /* 06 */      
                                         .
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


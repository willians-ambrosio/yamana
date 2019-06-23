&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

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
def temp-table tt-detalhe no-undo
    field num_id_bem_pat         like bem_pat.num_id_bem_pat
    field cod_cta_pat            like bem_pat.cod_cta_pat
    field num_bem_pat            like bem_pat.num_bem_pat
    field num_seq_bem_pat        like bem_pat.num_seq_bem_pat
    field num_seq_incorp_bem_pat like incorp_bem_pat.num_seq_incorp_bem_pat
    field des_bem_pat            like bem_pat.des_bem_pat
    field val_original           like bem_pat.val_original
    field val_salvage_value      like bem_pat_salvage_value.val_salvage_value
    field val_dpr_mes            like reg_calc_bem_pat.val_dpr_val_origin
    field val_dpr_acum           like reg_calc_bem_pat.val_dpr_val_origin
    index ix-1 as primary cod_cta_pat
    index ix-2 as unique cod_cta_pat num_bem_pat num_seq_bem_pat num_seq_incorp_bem_pat.

def temp-table tt-resumo no-undo
    field cod_cta_pat            like bem_pat.cod_cta_pat
    field val_original           like bem_pat.val_original
    field val_salvage_value      like bem_pat_salvage_value.val_salvage_value
    field val_dpr_mes            like reg_calc_bem_pat.val_dpr_val_origin
    field val_dpr_acum           like reg_calc_bem_pat.val_dpr_val_origin
    index ix-1 as primary cod_cta_pat.

def new global shared var v_cod_empres_usuar as char format "x(3)"
    label "Empresa" column-label "Empresa" no-undo.

def new global shared var v_rec_finalid_econ as recid no-undo.
def new global shared var v_rec_cenar_ctbl   as recid no-undo.

/* Variaveis para Gerar em Excel */
def var excelappl   as com-handle no-undo.
def var chWorkbook  as com-handle no-undo.
def var chWorksheet as com-handle no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BtnOK BtnCancel RECT-20 RECT-21 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Sair" 
     SIZE 13 BY 1.13
     BGCOLOR 8 FONT 1.

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "Imprimir" 
     SIZE 13 BY 1.13
     BGCOLOR 8 FONT 1.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 72 BY 11.75.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 72 BY 1.75.

DEFINE BUTTON bt_cenario 
     IMAGE-UP FILE "adeicon/props.bmp":U
     LABEL "BotÆo Zoom Cen rio" 
     SIZE 3 BY 1 TOOLTIP "Zoom".

DEFINE BUTTON bt_finalidade 
     IMAGE-UP FILE "adeicon/props.bmp":U
     LABEL "BotÆo Zoom Finalidade Econ“mica" 
     SIZE 3 BY 1 TOOLTIP "Zoom".

DEFINE VARIABLE fi-bem-fim AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-bem-ini AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Bem Patrimonial" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cc-fim AS CHARACTER FORMAT "X(08)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cc-ini AS CHARACTER FORMAT "X(08)":U 
     LABEL "Centro de Custo" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cenario AS CHARACTER FORMAT "X(8)":U 
     LABEL "Cen rio" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ct-pat-fim AS CHARACTER FORMAT "X(18)":U INITIAL "ZZZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ct-pat-ini AS CHARACTER FORMAT "X(18)":U 
     LABEL "Conta Patrimonial" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-fim AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Data de Aquisi‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-finalidade AS CHARACTER FORMAT "X(8)":U 
     LABEL "Finalidade Econ“mica" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fl-texto AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 44 BY .88 NO-UNDO.

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
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-17
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-18
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-19
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-20
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-tipo AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Resumido", 1,
"Detalhado", 2
     SIZE 43 BY .75 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 52 BY 1.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BtnOK AT ROW 13 COL 25
     BtnCancel AT ROW 13 COL 43
     RECT-20 AT ROW 1 COL 1
     RECT-21 AT ROW 12.75 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 72.43 BY 13.58
         DEFAULT-BUTTON BtnOK CANCEL-BUTTON BtnCancel.

DEFINE FRAME f-pg-sel
     fi-ct-pat-ini AT ROW 1.25 COL 13 COLON-ALIGNED
     fi-ct-pat-fim AT ROW 1.25 COL 45 COLON-ALIGNED NO-LABEL
     fi-dt-ini AT ROW 2.25 COL 13 COLON-ALIGNED
     fi-dt-fim AT ROW 2.25 COL 45 COLON-ALIGNED NO-LABEL
     fi-cc-ini AT ROW 3.25 COL 13 COLON-ALIGNED
     fi-cc-fim AT ROW 3.25 COL 45 COLON-ALIGNED NO-LABEL
     fi-bem-ini AT ROW 4.25 COL 13 COLON-ALIGNED
     fi-bem-fim AT ROW 4.25 COL 45 COLON-ALIGNED NO-LABEL
     fi-cenario AT ROW 5.5 COL 30 COLON-ALIGNED
     bt_cenario AT ROW 5.5 COL 46
     fi-finalidade AT ROW 6.5 COL 30 COLON-ALIGNED
     bt_finalidade AT ROW 6.5 COL 46
     rs-tipo AT ROW 8 COL 15 NO-LABEL
     fl-texto AT ROW 10 COL 12 COLON-ALIGNED NO-LABEL
     IMAGE-13 AT ROW 1.25 COL 34
     IMAGE-14 AT ROW 1.25 COL 41
     IMAGE-15 AT ROW 2.25 COL 41
     IMAGE-16 AT ROW 3.25 COL 41
     IMAGE-17 AT ROW 2.25 COL 34
     IMAGE-18 AT ROW 3.25 COL 34
     IMAGE-19 AT ROW 4.25 COL 34
     IMAGE-20 AT ROW 4.25 COL 41
     RECT-22 AT ROW 7.75 COL 11
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 1.25
         SIZE 69 BY 10.25
         FONT 1.


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
         TITLE              = "Relat¢rio de Savage Value - esbem02.003"
         HEIGHT             = 13.58
         WIDTH              = 72.72
         MAX-HEIGHT         = 31.29
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 31.29
         VIRTUAL-WIDTH      = 182.86
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
/* REPARENT FRAME */
ASSIGN FRAME f-pg-sel:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR FRAME f-pg-sel
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-sel
/* Query rebuild information for FRAME f-pg-sel
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <Relat¢rio de Savage Value - esbem02.003> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Relat¢rio de Savage Value - esbem02.003> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel C-Win
ON CHOOSE OF BtnCancel IN FRAME DEFAULT-FRAME /* Sair */
DO:
   apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK C-Win
ON CHOOSE OF BtnOK IN FRAME DEFAULT-FRAME /* Imprimir */
DO:
    find first cenar_ctbl no-lock
         where cenar_ctbl.cod_cenar_ctbl = input frame f-pg-sel fi-cenario no-error.
    if  not avail cenar_ctbl then do:
        run utp/ut-msgs.p("show",17006,"Cen rio Cont bil Inexistente ~~ Verifique...").
        apply 'entry' to input fi-cenario.
        return no-apply.
    end.

    find first finalid_econ no-lock
         where finalid_econ.cod_finalid_econ = input frame f-pg-sel fi-finalidade no-error.
    if  not avail finalid_econ then do:
        run utp/ut-msgs.p("show",17006,"Indicador Econ“mico Inexistente ~~ Verifique...").
        apply 'entry' to input fi-finalidade.
        return no-apply.
    end.

    run pi-executa.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME bt_cenario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_cenario C-Win
ON CHOOSE OF bt_cenario IN FRAME f-pg-sel /* bt_cenario */
DO:
    find first cenar_ctbl no-lock
         where cenar_ctbl.cod_cenar_ctbl = "Prim rio" no-error.
    if  avail cenar_ctbl then
        assign v_rec_cenar_ctbl = recid(cenar_ctbl).

    run prgint/utb/utb076ka.p.

    if  v_rec_cenar_ctbl <> ? then do:
        find first cenar_ctbl no-lock
             where recid(cenar_ctbl) = v_rec_cenar_ctbl no-error.
        assign fi-cenario:screen-value in frame f-pg-sel = cenar_ctbl.cod_cenar_ctbl.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_finalidade
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_finalidade C-Win
ON CHOOSE OF bt_finalidade IN FRAME f-pg-sel /* bt_finalidade */
DO:
    find first finalid_econ no-lock
         where finalid_econ.cod_finalid_econ = "Prim rio" no-error.
    if  avail finalid_econ then
        assign v_rec_finalid_econ = recid(finalid_econ).

    run prgint/utb/utb077ka.p.

    if  v_rec_finalid_econ <> ? then do:
        find first finalid_econ no-lock
             where recid(finalid_econ) = v_rec_finalid_econ no-error.
        assign fi-finalidade:screen-value in frame f-pg-sel = finalid_econ.cod_finalid_econ.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cenario C-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cenario IN FRAME f-pg-sel /* Cen rio */
DO:
    apply "CHOOSE" to bt_cenario in frame f-pg-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-finalidade C-Win
ON MOUSE-SELECT-DBLCLICK OF fi-finalidade IN FRAME f-pg-sel /* Moeda */
DO:
    apply "CHOOSE" to bt_finalidade in frame f-pg-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

assign fi-dt-ini = date(month(today),1,year(today))
       fi-dt-fim = today.

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

  RUN enable_UI.

  apply 'entry' to fi-ct-pat-ini in frame f-pg-sel.

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
  ENABLE BtnOK BtnCancel RECT-20 RECT-21 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY fi-ct-pat-ini fi-ct-pat-fim fi-dt-ini fi-dt-fim fi-cc-ini fi-cc-fim 
          fi-bem-ini fi-bem-fim fi-cenario fi-finalidade rs-tipo fl-texto 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  ENABLE fi-ct-pat-ini fi-ct-pat-fim fi-dt-ini fi-dt-fim fi-cc-ini fi-cc-fim 
         fi-bem-ini fi-bem-fim fi-cenario bt_cenario fi-finalidade 
         bt_finalidade rs-tipo fl-texto IMAGE-13 IMAGE-14 IMAGE-15 IMAGE-16 IMAGE-17 
         IMAGE-18 IMAGE-19 IMAGE-20 RECT-22 
      WITH FRAME f-pg-sel IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executa C-Win 
PROCEDURE pi-executa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    for each tt-detalhe: delete tt-detalhe. end.
    for each tt-resumo : delete tt-resumo . end.

    for each bem_pat no-lock
       where bem_pat.cod_empresa        = v_cod_empres_usuar
         and bem_pat.cod_cta_pat       >= input frame f-pg-sel fi-ct-pat-ini
         and bem_pat.cod_cta_pat       <= input frame f-pg-sel fi-ct-pat-fim
         and bem_pat.num_bem_pat       >= input frame f-pg-sel fi-bem-ini
         and bem_pat.num_bem_pat       <= input frame f-pg-sel fi-bem-fim
         and bem_pat.dat_aquis_bem_pat >= input frame f-pg-sel fi-dt-ini
         and bem_pat.dat_aquis_bem_pat <= input frame f-pg-sel fi-dt-fim
         and bem_pat.cod_plano_ccusto  >= input frame f-pg-sel fi-cc-ini
         and bem_pat.cod_plano_ccusto  <= input frame f-pg-sel fi-cc-fim:

        assign fl-texto:screen-value = "Conta " + string(bem_pat.cod_cta_pat) +
                                       " / Bem " + string(bem_pat.num_bem_pat) +
                                       " / " + string(bem_pat.num_seq_bem_pat).

        create tt-detalhe no-error.
        assign tt-detalhe.num_id_bem_pat  = bem_pat.num_id_bem_pat
               tt-detalhe.cod_cta_pat     = bem_pat.cod_cta_pat
               tt-detalhe.num_bem_pat     = bem_pat.num_bem_pat
               tt-detalhe.num_seq_bem_pat = bem_pat.num_seq_bem_pat
               tt-detalhe.des_bem_pat     = bem_pat.des_bem_pat.

        run pi-valores(input 0).

        for each incorp_bem_pat of bem_pat no-lock:
            assign fl-texto:screen-value = "Conta " + string(bem_pat.cod_cta_pat) +
                                           " / Bem " + string(bem_pat.num_bem_pat) +
                                           " / " + string(bem_pat.num_seq_bem_pat) +
                                           " / " + string(incorp_bem_pat.num_seq_incorp_bem_pat).

            create tt-detalhe no-error.
            assign tt-detalhe.num_id_bem_pat         = bem_pat.num_id_bem_pat
                   tt-detalhe.cod_cta_pat            = bem_pat.cod_cta_pat
                   tt-detalhe.num_bem_pat            = bem_pat.num_bem_pat
                   tt-detalhe.num_seq_bem_pat        = bem_pat.num_seq_bem_pat
                   tt-detalhe.des_bem_pat            = bem_pat.des_bem_pat
                   tt-detalhe.num_seq_incorp_bem_pat = incorp_bem_pat.num_seq_incorp_bem_pat.

            run pi-valores(input incorp_bem_pat.num_seq_incorp_bem_pat).
        end.
    end.

    for each tt-detalhe:
        find first tt-resumo
             where tt-resumo.cod_cta_pat = tt-detalhe.cod_cta_pat no-error.
        if  not avail tt-resumo then do:
            create tt-resumo no-error.
            assign tt-resumo.cod_cta_pat = tt-detalhe.cod_cta_pat.
        end.
        assign tt-resumo.val_original      = tt-resumo.val_original      + tt-detalhe.val_original
               tt-resumo.val_salvage_value = tt-resumo.val_salvage_value + tt-detalhe.val_salvage_value
               tt-resumo.val_dpr_mes       = tt-resumo.val_dpr_mes       + tt-detalhe.val_dpr_mes
               tt-resumo.val_dpr_acum      = tt-resumo.val_dpr_acum      + tt-detalhe.val_dpr_acum.
    end.

    find first tt-detalhe no-error.
    if  avail tt-detalhe then do:
        /************************************************************/
        /*          Abre planilha Excel                             */
        create "excel.application":u excelappl connect no-error.
        if  error-status:error then
            create "excel.application":u excelappl.

        excelappl:visible = no.
        excelappl:displayalerts = no.
        /************************************************************/

        run pi-imprime.

        excelappl:DisplayAlerts = yes.
        excelappl:visible = yes.

        /* release com-handles */
        if  valid-handle(excelappl)   then release object excelappl.
        if  valid-handle(chWorkbook)  then release object chWorkbook.
        if  valid-handle(chWorkSheet) then release object chWorkSheet.

        assign fl-texto:screen-value = "Fim de Processamento...".
    end.
    else
        assign fl-texto:screen-value = "NÆo existe dados para sele‡Æo...".

    return "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valores C-Win 
PROCEDURE pi-valores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def input param p_incorp like incorp_bem_pat.num_seq_incorp_bem_pat no-undo.

    def var dt-aux as date no-undo.

    if  month(input frame f-pg-sel fi-dt-fim) = 12 then
        assign dt-aux = date(01, 01, year(input frame f-pg-sel fi-dt-fim) + 1).
    else
        assign dt-aux = date(01, month(input frame f-pg-sel fi-dt-fim) + 1, year(input frame f-pg-sel fi-dt-fim)).

    find first bem_pat_salvage_value no-lock
         where bem_pat_salvage_value.num_id_bem_pat         = bem_pat.num_id_bem_pat
           and bem_pat_salvage_value.num_seq_incorp_bem_pat = p_incorp
           and bem_pat_salvage_value.cod_cenar_ctbl         = input frame f-pg-sel fi-cenario
           and bem_pat_salvage_value.cod_indic_econ         = input frame f-pg-sel fi-finalidade no-error.
    if  avail bem_pat_salvage_value then
        assign tt-detalhe.val_salvage_value = bem_pat_salvage_value.val_salvage_value.

    find first param_calc_bem_pat of bem_pat no-lock
         where(param_calc_bem_pat.cod_tip_calc     = "DP"
            or param_calc_bem_pat.cod_tip_calc     = "AM")
           and param_calc_bem_pat.cod_cenar_ctbl   = input frame f-pg-sel fi-cenario
           and param_calc_bem_pat.cod_finalid_econ = input frame f-pg-sel fi-finalidade no-error.

    for each reg_calc_bem_pat no-lock
       where reg_calc_bem_pat.num_id_bem_pat         = bem_pat.num_id_bem_pat
         and reg_calc_bem_pat.num_seq_incorp_bem_pat = p_incorp
         and reg_calc_bem_pat.cod_tip_calc           = param_calc_bem_pat.cod_tip_calc
         and reg_calc_bem_pat.cod_cenar_ctbl         = input frame f-pg-sel fi-cenario
         and reg_calc_bem_pat.cod_finalid_econ       = input frame f-pg-sel fi-finalidade
         and reg_calc_bem_pat.dat_calc_pat           = dt-aux /* input frame f-pg-sel fi-dt-fim */
/*          and reg_calc_bem_pat.num_seq_reg_calc_bem_pat */
         and reg_calc_bem_pat.cod_ccusto_respons     = bem_pat.cod_ccusto_respons:

        assign tt-detalhe.val_dpr_mes  = reg_calc_bem_pat.val_dpr_val_origin
/*                tt-detalhe.val_dpr_acum = tt-detalhe.val_dpr_acum + reg_calc_bem_pat.val_dpr_val_origin */
            .
    end.

    for each sdo_bem_pat no-lock
       where sdo_bem_pat.num_id_bem_pat         = bem_pat.num_id_bem_pat
         and sdo_bem_pat.num_seq_incorp_bem_pat = p_incorp
         and sdo_bem_pat.cod_cenar_ctbl         = input frame f-pg-sel fi-cenario
         and sdo_bem_pat.cod_finalid_econ       = input frame f-pg-sel fi-finalidade
         and sdo_bem_pat.dat_sdo_bem_pat       <= dt-aux:
        assign tt-detalhe.val_dpr_acum = sdo_bem_pat.val_dpr_val_origin.
    end.

    find first val_origin_bem_pat no-lock
         where val_origin_bem_pat.num_id_bem_pat         = bem_pat.num_id_bem_pat
           and val_origin_bem_pat.num_seq_incorp_bem_pat = p_incorp
           and val_origin_bem_pat.cod_cenar_ctbl         = input frame f-pg-sel fi-cenario
           and val_origin_bem_pat.cod_finalid_econ       = input frame f-pg-sel fi-finalidade no-error.
    if  avail val_origin_bem_pat then
        assign tt-detalhe.val_original = val_origin_bem_pat.val_original.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime C-Win 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def var tot-val-orig     like bem_pat.val_original                    no-undo.
    def var tot-val-salvage  like bem_pat_salvage_value.val_salvage_value no-undo.
    def var tot-val-dpr-mes  like bem_pat.val_original                    no-undo.
    def var tot-val-dpr-acum like bem_pat.val_original                    no-undo.
    def var ger-val-orig     like bem_pat.val_original                    no-undo.
    def var ger-val-salvage  like bem_pat_salvage_value.val_salvage_value no-undo.
    def var ger-val-dpr-mes  like bem_pat.val_original                    no-undo.
    def var ger-val-dpr-acum like bem_pat.val_original                    no-undo.

    def var cRange   as char no-undo.
    def var i-linha  as int  no-undo.
    def var i-folder as int  no-undo.

    assign fl-texto:screen-value in frame f-pg-sel = "Gerando Excel....".

    find first emsuni.empresa no-lock
         where emsuni.empresa.cod_empresa = v_cod_empres_usuar no-error.

    assign i-linha  = 5
           i-folder = 1.

    if  input frame f-pg-sel rs-tipo = 1 then do: /* resumido */
        assign ger-val-orig     = 0
               ger-val-salvage  = 0
               ger-val-dpr-acum = 0.

        if  i-linha = 5 then do:
            if  i-folder = 1 then
                chworkbook  = excelappl:workbooks:add.
            else do:
                chWorkSheet = excelappl:workbooks:item(1):worksheets("Plan" + string(i-folder)).
                excelappl:workbooks:item(1):worksheets:add(,chWorkSheet).
            end.

            excelappl:worksheets:item(i-folder):select. /* seleciona o folder novo */

            /*************** Cabe‡alho ***********************************/
            excelappl:range("A4"):value = "Conta Patrimonial".
            excelappl:range("B4"):value = "C¢digo do Bem".
            excelappl:range("C4"):value = "Seq Bem".
            excelappl:range("D4"):value = "Seq Incorp".
            excelappl:range("E4"):value = "Descri‡Æo do Bem".
            excelappl:range("F4"):value = "Valor Aquis Bem".
            excelappl:range("G4"):value = "Salvage Value".
            excelappl:range("H4"):value = "Deprec Mes".
            excelappl:range("I4"):value = "Deprec Acumul".

            excelappl:range("A1:I1"):font:size = 12.
            excelappl:range("A1:I4"):font:bold = true.
            excelappl:range("A2:I2"):font:size = 11.
            excelappl:range("A:I"):EntireColumn:AutoFit.

            excelappl:range("A1"):value = if  avail empresa then emsuni.empresa.nom_razao_social else ''.
            excelappl:range("F1"):value = "Periodo: " + string(input frame f-pg-sel fi-dt-ini,"99/99/9999") +
                                            "    a    " + string(input frame f-pg-sel fi-dt-fim,"99/99/9999").
            excelappl:range("A2"):value = "Savage Value".
            excelappl:range("F2"):value = "Finalidade: " + string(input frame f-pg-sel fi-finalidade) +
                                            "       Cen rio: " + string(input frame f-pg-sel fi-cenario).

            assign excelappl:range("E:I"):NumberFormat="###.###.##0,00".
        end.

        for each tt-resumo:
            /* imprime totais */
            assign excelappl:range("A" + string(i-linha)):value = tt-resumo.cod_cta_pat
                   excelappl:range("F" + string(i-linha)):value = tt-resumo.val_original
                   excelappl:range("G" + string(i-linha)):value = tt-resumo.val_salvage_value
                   excelappl:range("H" + string(i-linha)):value = tt-resumo.val_dpr_mes
                   excelappl:range("I" + string(i-linha)):value = tt-resumo.val_dpr_acum.

            assign ger-val-orig     = ger-val-orig     + tt-resumo.val_original
                   ger-val-salvage  = ger-val-salvage  + tt-resumo.val_salvage_value
                   ger-val-dpr-mes  = ger-val-dpr-mes  + tt-resumo.val_dpr_mes
                   ger-val-dpr-acum = ger-val-dpr-acum + tt-resumo.val_dpr_acum.

            assign i-linha = i-linha + 1.
        end.

        assign i-linha = i-linha + 1.

        /* imprime totais */
        assign excelappl:range("A" + string(i-linha)):value = "Total Geral: "
               excelappl:range("F" + string(i-linha)):value = ger-val-orig
               excelappl:range("G" + string(i-linha)):value = ger-val-salvage
               excelappl:range("H" + string(i-linha)):value = ger-val-dpr-mes
               excelappl:range("I" + string(i-linha)):value = ger-val-dpr-acum.
    end.

    if  input frame f-pg-sel rs-tipo = 2 then do: /* detalhado */
        assign ger-val-orig     = 0
               ger-val-salvage  = 0
               ger-val-dpr-acum = 0.

        for each tt-detalhe
            break by tt-detalhe.cod_cta_pat:

            if  i-linha = 5 then do:
                if  i-folder = 1 then
                    chworkbook  = excelappl:workbooks:add.
                else do:
                    chWorkSheet = excelappl:workbooks:item(1):worksheets("Plan" + string(i-folder)).
                    excelappl:workbooks:item(1):worksheets:add(,chWorkSheet).
                end.

                excelappl:worksheets:item(i-folder):select. /* seleciona o folder novo */

                /*************** Cabe‡alho ***********************************/
                excelappl:range("A4"):value = "Conta Patrimonial".
                excelappl:range("B4"):value = "C¢digo do Bem".
                excelappl:range("C4"):value = "Seq Bem".
                excelappl:range("D4"):value = "Seq Incorp".
                excelappl:range("E4"):value = "Descri‡Æo do Bem".
                excelappl:range("F4"):value = "Valor Aquis Bem".
                excelappl:range("G4"):value = "Salvage Value".
                excelappl:range("H4"):value = "Deprec Mes".
                excelappl:range("I4"):value = "Deprec Acumul".

                excelappl:range("A1:I1"):font:size = 12.
                excelappl:range("A1:I4"):font:bold = true.
                excelappl:range("A2:I2"):font:size = 11.
                excelappl:range("A:I"):EntireColumn:AutoFit.

                excelappl:range("A1"):value = if  avail empresa then emsuni.empresa.nom_razao_social else ''.
                excelappl:range("F1"):value = "Periodo: " + string(input frame f-pg-sel fi-dt-ini,"99/99/9999") +
                                                "    a    " + string(input frame f-pg-sel fi-dt-fim,"99/99/9999").
                excelappl:range("A2"):value = "Savage Value".
                excelappl:range("F2"):value = "Finalidade: " + string(input frame f-pg-sel fi-finalidade) +
                                                "       Cen rio: " + string(input frame f-pg-sel fi-cenario).

                assign excelappl:range("E:I"):NumberFormat="###.###.##0,00".
            end.

            assign excelappl:range("A" + string(i-linha)):value = tt-detalhe.cod_cta_pat
                   excelappl:range("B" + string(i-linha)):value = tt-detalhe.num_bem_pat
                   excelappl:range("C" + string(i-linha)):value = tt-detalhe.num_seq_bem_pat
                   excelappl:range("D" + string(i-linha)):value = tt-detalhe.num_seq_incorp_bem_pat
                   excelappl:range("E" + string(i-linha)):value = tt-detalhe.des_bem_pat
                   excelappl:range("F" + string(i-linha)):value = tt-detalhe.val_original
                   excelappl:range("G" + string(i-linha)):value = tt-detalhe.val_salvage_value
                   excelappl:range("H" + string(i-linha)):value = tt-detalhe.val_dpr_mes
                   excelappl:range("I" + string(i-linha)):value = tt-detalhe.val_dpr_acum.

            if  last-of(tt-detalhe.cod_cta_pat) then do:
                assign i-linha = i-linha + 2.

                for each tt-resumo
                   where tt-resumo.cod_cta_pat = tt-detalhe.cod_cta_pat:
                    /* imprime totais */
                    assign excelappl:range("A" + string(i-linha)):value = "Total " + tt-resumo.cod_cta_pat + ": "
                           excelappl:range("F" + string(i-linha)):value = tt-resumo.val_original
                           excelappl:range("G" + string(i-linha)):value = tt-resumo.val_salvage_value
                           excelappl:range("H" + string(i-linha)):value = tt-resumo.val_dpr_mes
                           excelappl:range("I" + string(i-linha)):value = tt-resumo.val_dpr_acum.

                    assign ger-val-orig     = ger-val-orig     + tt-resumo.val_original
                           ger-val-salvage  = ger-val-salvage  + tt-resumo.val_salvage_value
                           ger-val-dpr-mes  = ger-val-dpr-mes  + tt-resumo.val_dpr_mes
                           ger-val-dpr-acum = ger-val-dpr-acum + tt-resumo.val_dpr_acum.
                end.

                if  i-linha > 55000 then
                    assign i-linha  = 5
                           i-folder = 1.
                else
                    assign i-linha = i-linha + 2.
            end.

            assign i-linha = i-linha + 1.
        end.

        assign i-linha = i-linha + 1.

        /* imprime totais */
        assign excelappl:range("A" + string(i-linha)):value = "Total Geral: "
               excelappl:range("F" + string(i-linha)):value = ger-val-orig
               excelappl:range("G" + string(i-linha)):value = ger-val-salvage
               excelappl:range("H" + string(i-linha)):value = ger-val-dpr-mes
               excelappl:range("I" + string(i-linha)):value = ger-val-dpr-acum.
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/*****************************************************************************
** Programa..............: mod_es_cons_estab_emp
** Descricao.............: Modifica Pai De Para Estab X Empresa
** Versao................:  1.00.00.000
** Procedimento..........: man_es_cons_estab_emp
** Nome Externo..........: esp/esfgl071ea.p
** Criado por............: Hilton Borba
** Criado em.............: 04/10/2011
*****************************************************************************/
def var c-versao-prg as char initial " 1.00.00.000":U no-undo.

/* buffer para tabelas iguais no ems2 e ems5 - ini */
def buffer empresa             for emsuni.empresa.
/* buffer para tabelas iguais no ems2 e ems5 - fim */

{include/i_dbinst.i}
{include/i_dbtype.i}
{include/i_fcldef.i}

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i mod_es_cons_estab_emp FGL}
&ENDIF

/******************************* Private-Data *******************************/
assign this-procedure:private-data = "HLP=15":U.
/*************************************  *************************************/

&if "{&emsuni_dbinst}" <> "yes" &then
run pi_messages (input "show", input 5884, input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9", "EMSUNI")) /*msg_5884*/.
&elseif "{&emsuni_version}" < "1.00" &then
run pi_messages (input "show", input 5009,
                 input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9",
                                    "MOD_ES_CONS_ESTAB_EMP","~~EMSUNI", "~~{~&emsuni_version}", "~~1.00")) /*msg_5009*/.
&else

/************************** Buffer Definition Begin *************************/

def buffer b_es_cons_estab_emp
    for es_cons_estab_emp.
def buffer b_es_cons_estab_emp_mod
    for es_cons_estab_emp.

/*************************** Buffer Definition End **************************/

/************************* Variable Definition Begin ************************/

def new global shared var v_cod_dwb_user
    as character
    format "x(21)":U
    label "Usu†rio"
    column-label "Usu†rio"
    no-undo.
def new global shared var v_cod_usuar_corren
    as character
    format "x(12)":U
    label "Usu†rio Corrente"
    column-label "Usu†rio Corrente"
    no-undo.
def new global shared var v_rec_es_cons_estab_emp
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_plano_ccusto
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_plano_cta_ctbl
    as recid
    format ">>>>>>9":U
    no-undo.
def var v_log_answer
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    no-undo.
def var v_log_repeat
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    no-undo.
def var v_log_save
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    no-undo.
def var v_log_save_ok
    as logical
    format "Sim/N∆o"
    initial no
    no-undo.
def var v_nom_title_aux
    as character
    format "x(60)":U
    no-undo.
def var v_rec_log
    as recid
    format ">>>>>>9":U
    no-undo.
def var v_rec_table
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.
def var v_wgh_focus
    as widget-handle
    format ">>>>>>9":U
    no-undo.

/************************** Variable Definition End *************************/

/*************************** Menu Definition Begin **************************/

.

def menu      m_help                menubar
    menu-item mi_conteudo           label "&Conte£do"
    menu-item mi_sobre              label "&Sobre".

/**************************** Menu Definition End ***************************/

/************************ Rectangle Definition Begin ************************/

def rectangle rt_key
    size 1 by 1
    edge-pixels 2.
def rectangle rt_mold
    size 1 by 1
    edge-pixels 2.
def rectangle rt_cxcf
    size 1 by 1
    fgcolor 1 edge-pixels 2.

/************************* Rectangle Definition End *************************/

/************************** Button Definition Begin *************************/

def button bt_can
    label "Cancela"
    tooltip "Cancela"
    size 1 by 1
    auto-endkey.
def button bt_hel2
    label "Ajuda"
    tooltip "Ajuda"
    size 1 by 1.
def button bt_nex2
    label ">"
    tooltip "Pr¢xima Ocorrància da Tabela"
    image-up file "image/im-nex2"
    image-insensitive file "image/ii-nex2"
    size 1 by 1
    auto-go.
def button bt_ok
    label "OK"
    tooltip "OK"
    size 1 by 1
    auto-go.
def button bt_pre2
    label "<"
    tooltip "Ocorrància Anterior da Tabela"
    image-up file "image/im-pre2"
    image-insensitive file "image/ii-pre2"
    size 1 by 1
    auto-go.
def button bt_sav
    label "Salva"
    tooltip "Salva"
    size 1 by 1
    auto-go.
def button bt_sea2
    label "Psq"
    tooltip "Pesquisa"
    image-up file "image/im-sea2"
    image-insensitive file "image/ii-sea2"
    size 1 by 1
    auto-go.
/****************************** Function Button *****************************/
def button bt_zoo_080
    label "Zoom"
    tooltip "Zoom"
    image-up file "image/im-zoo"
    image-insensitive file "image/ii-zoo"
    size 4 by .88.
def button bt_zoo_083
    label "Zoom"
    tooltip "Zoom"
    image-up file "image/im-zoo"
    image-insensitive file "image/ii-zoo"
    size 4 by .88.

/*************************** Button Definition End **************************/

/************************** Frame Definition Begin **************************/

def frame f_mop_03_es_cons_estab_emp
    rt_mold
         at row 04.54 col 02.00
    rt_cxcf
         at row 07.05 col 02.00 bgcolor 7 
    rt_key
         at row 01.21 col 02.00
    es_cons_estab_emp.cod_empresa
         at row 01.38 col 21.00 colon-aligned
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    empresa.nom_razao_social
         at row 01.38 col 32.00 no-label
         view-as fill-in
         size-chars 41.14 by .88
         fgcolor ? bgcolor 15 font 2
    es_cons_estab_emp.cod_cenar_ctbl
         at row 02.38 col 21.00 colon-aligned
         view-as fill-in
         size-chars 12.14 by .88
         fgcolor ? bgcolor 15 font 2
    es_cons_estab_emp.dat_inic_valid
         at row 03.38 col 21.00 colon-aligned
         view-as fill-in
         size-chars 12.14 by .88
         fgcolor ? bgcolor 15 font 2
    es_cons_estab_emp.dat_fim_valid
         at row 03.38 col 50.00 colon-aligned
         view-as fill-in
         size-chars 12.14 by .88
         fgcolor ? bgcolor 15 font 2
    es_cons_estab_emp.cod_plano_cta_ctbl
         at row 04.71 col 21.00 colon-aligned
         view-as fill-in
         size-chars 16.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_zoo_080
         at row 04.71 col 39.50
    es_cons_estab_emp.cod_plano_ccusto
         at row 05.71 col 21.00 colon-aligned
         view-as fill-in
         size-chars 16.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_zoo_083
         at row 05.71 col 39.50
    bt_ok
         at row 07.26 col 03.00 font ?
         help "OK"
    bt_sav
         at row 07.26 col 14.00 font ?
         help "Salva"
    bt_can
         at row 07.26 col 25.00 font ?
         help "Cancela"
    bt_pre2
         at row 07.26 col 38.00 font ?
         help "Ocorrància Anterior da Tabela"
    bt_sea2
         at row 07.26 col 43.00 font ?
         help "Pesquisa"
    bt_nex2
         at row 07.26 col 48.00 font ?
         help "Pr¢xima Ocorrància da Tabela"
    bt_hel2
         at row 07.26 col 70.86 font ?
         help "Ajuda"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 83.29 by 08.88 default-button bt_sav
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Modifica De Para Estab X Empresa".
    /* adjust size of objects in this frame */
    assign bt_can:width-chars   in frame f_mop_03_es_cons_estab_emp = 10.00
           bt_can:height-chars  in frame f_mop_03_es_cons_estab_emp = 01.00
           bt_hel2:width-chars  in frame f_mop_03_es_cons_estab_emp = 10.00
           bt_hel2:height-chars in frame f_mop_03_es_cons_estab_emp = 01.00
           bt_nex2:width-chars  in frame f_mop_03_es_cons_estab_emp = 04.00
           bt_nex2:height-chars in frame f_mop_03_es_cons_estab_emp = 01.00
           bt_ok:width-chars    in frame f_mop_03_es_cons_estab_emp = 10.00
           bt_ok:height-chars   in frame f_mop_03_es_cons_estab_emp = 01.00
           bt_pre2:width-chars  in frame f_mop_03_es_cons_estab_emp = 04.00
           bt_pre2:height-chars in frame f_mop_03_es_cons_estab_emp = 01.00
           bt_sav:width-chars   in frame f_mop_03_es_cons_estab_emp = 10.00
           bt_sav:height-chars  in frame f_mop_03_es_cons_estab_emp = 01.00
           bt_sea2:width-chars  in frame f_mop_03_es_cons_estab_emp = 04.00
           bt_sea2:height-chars in frame f_mop_03_es_cons_estab_emp = 01.00
           rt_cxcf:width-chars  in frame f_mop_03_es_cons_estab_emp = 79.86
           rt_cxcf:height-chars in frame f_mop_03_es_cons_estab_emp = 01.42
           rt_key:width-chars   in frame f_mop_03_es_cons_estab_emp = 79.86
           rt_key:height-chars  in frame f_mop_03_es_cons_estab_emp = 03.21
           rt_mold:width-chars  in frame f_mop_03_es_cons_estab_emp = 79.86
           rt_mold:height-chars in frame f_mop_03_es_cons_estab_emp = 02.25.
    /* set private-data for the help system */
    assign bt_ok:private-data                                in frame f_mop_03_es_cons_estab_emp = "HLP=000010721":U
           bt_sav:private-data                               in frame f_mop_03_es_cons_estab_emp = "HLP=000011048":U
           bt_can:private-data                               in frame f_mop_03_es_cons_estab_emp = "HLP=000011050":U
           bt_pre2:private-data                              in frame f_mop_03_es_cons_estab_emp = "HLP=000010824":U
           bt_sea2:private-data                              in frame f_mop_03_es_cons_estab_emp = "HLP=000010831":U
           bt_nex2:private-data                              in frame f_mop_03_es_cons_estab_emp = "HLP=000010823":U
           bt_hel2:private-data                              in frame f_mop_03_es_cons_estab_emp = "HLP=000011326":U
           es_cons_estab_emp.cod_empresa:private-data        in frame f_mop_03_es_cons_estab_emp = "HLP=000005341":U
           empresa.nom_razao_social:private-data             in frame f_mop_03_es_cons_estab_emp = "HLP=000009760":U
           es_cons_estab_emp.cod_cenar_ctbl:private-data     in frame f_mop_03_es_cons_estab_emp = "HLP=000004548":U
           es_cons_estab_emp.dat_inic_valid:private-data     in frame f_mop_03_es_cons_estab_emp = "HLP=000009978":U
           es_cons_estab_emp.dat_fim_valid:private-data      in frame f_mop_03_es_cons_estab_emp = "HLP=000022223":U
           es_cons_estab_emp.cod_plano_cta_ctbl:private-data in frame f_mop_03_es_cons_estab_emp = "HLP=000025116":U
           bt_zoo_080:private-data                           in frame f_mop_03_es_cons_estab_emp = "HLP=000009431":U
           es_cons_estab_emp.cod_plano_ccusto:private-data   in frame f_mop_03_es_cons_estab_emp = "HLP=000009923":U
           bt_zoo_083:private-data                           in frame f_mop_03_es_cons_estab_emp = "HLP=000009431":U
           frame f_mop_03_es_cons_estab_emp:private-data                                     = "HLP=000006532":U.
    /* enable function buttons */
    assign bt_zoo_080:sensitive in frame f_mop_03_es_cons_estab_emp = yes
           bt_zoo_083:sensitive in frame f_mop_03_es_cons_estab_emp = yes.
    /* move buttons to top */
    bt_zoo_080:move-to-top().
    bt_zoo_083:move-to-top().

{include/i_fclfrm.i f_mop_03_es_cons_estab_emp }
/*************************** Frame Definition End ***************************/

/*********************** User Interface Trigger Begin ***********************/

ON CHOOSE OF bt_can IN FRAME f_mop_03_es_cons_estab_emp
DO:

    apply "end-error" to self.
END. /* ON CHOOSE OF bt_can IN FRAME f_mop_03_es_cons_estab_emp */

ON CHOOSE OF bt_hel2 IN FRAME f_mop_03_es_cons_estab_emp
DO:

    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.

    /* End_Include: i_context_help_frame */

END. /* ON CHOOSE OF bt_hel2 IN FRAME f_mop_03_es_cons_estab_emp */

ON CHOOSE OF bt_nex2 IN FRAME f_mop_03_es_cons_estab_emp
DO:

    find next b_es_cons_estab_emp_mod no-lock no-error.
    if  not avail b_es_cons_estab_emp_mod
    then do:
        find last b_es_cons_estab_emp_mod no-lock no-error.
        if  avail b_es_cons_estab_emp_mod
        then do:
            message "Èltima ocorrància da tabela." /*l_last*/ 
                   view-as alert-box warning buttons ok.
            assign v_rec_table = recid(b_es_cons_estab_emp_mod).
        end.
        else do:
            clear frame f_mop_03_es_cons_estab_emp no-pause.
            message "N∆o existem ocorràncias na tabela." /*l_no_record*/ 
                   view-as alert-box warning buttons ok.
            assign v_rec_table = ?.
        end.
    end.
    else do:
        assign v_rec_table = recid(b_es_cons_estab_emp_mod).
    end.
    assign v_log_repeat = yes.

END. /* ON CHOOSE OF bt_nex2 IN FRAME f_mop_03_es_cons_estab_emp */

ON CHOOSE OF bt_ok IN FRAME f_mop_03_es_cons_estab_emp
DO:

    assign v_log_repeat   = no
           v_log_save     = yes
           v_rec_es_cons_estab_emp = v_rec_table.
END. /* ON CHOOSE OF bt_ok IN FRAME f_mop_03_es_cons_estab_emp */

ON CHOOSE OF bt_pre2 IN FRAME f_mop_03_es_cons_estab_emp
DO:

    find prev b_es_cons_estab_emp_mod no-lock no-error.
    if  not avail b_es_cons_estab_emp_mod
    then do:
        find first b_es_cons_estab_emp_mod no-lock no-error.
        if  avail b_es_cons_estab_emp_mod
        then do:
            message "Primeira ocorrància da tabela." /*l_first*/ 
                   view-as alert-box warning buttons ok.
            assign v_rec_table = recid(b_es_cons_estab_emp_mod).
        end.
        else do:
            clear frame f_mop_03_es_cons_estab_emp no-pause.
            message "N∆o existem ocorràncias na tabela." /*l_no_record*/ 
                   view-as alert-box warning buttons ok.
            assign v_rec_table = ?.
        end.
    end.
    else do:
        assign v_rec_table = recid(b_es_cons_estab_emp_mod).
    end.
    assign v_log_repeat = yes.

END. /* ON CHOOSE OF bt_pre2 IN FRAME f_mop_03_es_cons_estab_emp */

ON CHOOSE OF bt_sav IN FRAME f_mop_03_es_cons_estab_emp
DO:

    assign v_log_repeat = yes
           v_log_save   = yes.
END. /* ON CHOOSE OF bt_sav IN FRAME f_mop_03_es_cons_estab_emp */

ON CHOOSE OF bt_sea2 IN FRAME f_mop_03_es_cons_estab_emp
DO:

    assign v_rec_es_cons_estab_emp = v_rec_table.
    if  search("esp/esfgl071ka.r") = ? and search("esp/esfgl071ka.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut†vel n∆o foi encontrado: esp/esfgl071ka.p".
        else do:
            message "Programa execut†vel n∆o foi encontrado: esp/esfgl071ka.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run esp/esfgl071ka.p /*prg_sea_es_cons_estab_emp*/.

    if  v_rec_es_cons_estab_emp <> ? then do:
        assign v_rec_table = v_rec_es_cons_estab_emp.
    end.
    assign v_log_save   = no
           v_log_repeat = yes.
END. /* ON CHOOSE OF bt_sea2 IN FRAME f_mop_03_es_cons_estab_emp */

/************************ User Interface Trigger End ************************/

/************************** Function Trigger Begin **************************/

ON  CHOOSE OF bt_zoo_080 IN FRAME f_mop_03_es_cons_estab_emp
OR F5 OF es_cons_estab_emp.cod_plano_cta_ctbl IN FRAME f_mop_03_es_cons_estab_emp DO:

    /* fn_generic_zoom */
    if  search("prgint/utb/utb080ka.r") = ? and search("prgint/utb/utb080ka.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut†vel n∆o foi encontrado: prgint/utb/utb080ka.p".
        else do:
            message "Programa execut†vel n∆o foi encontrado: prgint/utb/utb080ka.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run prgint/utb/utb080ka.p /*prg_sea_cenar_ctbl*/.
    if  v_rec_plano_cta_ctbl <> ?
    then do:
        find plano_cta_ctbl where recid(plano_cta_ctbl) = v_rec_plano_cta_ctbl no-lock no-error.
        assign es_cons_estab_emp.cod_plano_cta_ctbl:screen-value in frame f_mop_03_es_cons_estab_emp =
               string(plano_cta_ctbl.cod_plano_cta_ctbl).

    end.
    apply "entry" to es_cons_estab_emp.cod_plano_cta_ctbl in frame f_mop_03_es_cons_estab_emp.
end. /* ON  CHOOSE OF bt_zoo_080 IN FRAME f_mop_03_es_cons_estab_emp */

ON  CHOOSE OF bt_zoo_083 IN FRAME f_mop_03_es_cons_estab_emp
OR F5 OF es_cons_estab_emp.cod_plano_ccusto IN FRAME f_mop_03_es_cons_estab_emp DO:

    /* fn_generic_zoom */
    if  search("prgint/utb/utb083ka.r") = ? and search("prgint/utb/utb083ka.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut†vel n∆o foi encontrado: prgint/utb/utb083ka.p".
        else do:
            message "Programa execut†vel n∆o foi encontrado: prgint/utb/utb083ka.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run prgint/utb/utb083ka.p /*prg_sea_cenar_ctbl*/.
    if  v_rec_plano_ccusto <> ?
    then do:
        find plano_ccusto where recid(plano_ccusto) = v_rec_plano_ccusto no-lock no-error.
        assign es_cons_estab_emp.cod_plano_ccusto:screen-value in frame f_mop_03_es_cons_estab_emp =
               string(plano_ccusto.cod_plano_ccusto).

    end.
    apply "entry" to es_cons_estab_emp.cod_plano_ccusto in frame f_mop_03_es_cons_estab_emp.
end. /* ON  CHOOSE OF bt_zoo_083 IN FRAME f_mop_03_es_cons_estab_emp */

/*************************** Function Trigger End ***************************/

/**************************** Frame Trigger Begin ***************************/

ON ALT-CURSOR-LEFT OF FRAME f_mop_03_es_cons_estab_emp
DO:

    if  bt_pre2:sensitive in frame f_mop_03_es_cons_estab_emp
    then do:
        apply "choose" to bt_pre2 in frame f_mop_03_es_cons_estab_emp.
    end.

END. /* ON ALT-CURSOR-LEFT OF FRAME f_mop_03_es_cons_estab_emp */

ON ALT-CURSOR-RIGHT OF FRAME f_mop_03_es_cons_estab_emp
DO:

    if  bt_nex2:sensitive in frame f_mop_03_es_cons_estab_emp
    then do:
        apply "choose" to bt_nex2 in frame f_mop_03_es_cons_estab_emp.
    end.

END. /* ON ALT-CURSOR-RIGHT OF FRAME f_mop_03_es_cons_estab_emp */

ON ALT-Z OF FRAME f_mop_03_es_cons_estab_emp
DO:

    if  bt_sea2:sensitive in frame f_mop_03_es_cons_estab_emp
    then do:
        apply "choose" to bt_sea2 in frame f_mop_03_es_cons_estab_emp.
    end.

END. /* ON ALT-Z OF FRAME f_mop_03_es_cons_estab_emp */

ON ENDKEY OF FRAME f_mop_03_es_cons_estab_emp
DO:

END. /* ON ENDKEY OF FRAME f_mop_03_es_cons_estab_emp */

ON END-ERROR OF FRAME f_mop_03_es_cons_estab_emp
DO:

    assign v_rec_es_cons_estab_emp = ?.
END. /* ON END-ERROR OF FRAME f_mop_03_es_cons_estab_emp */

ON HELP OF FRAME f_mop_03_es_cons_estab_emp ANYWHERE
DO:

    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */

END. /* ON HELP OF FRAME f_mop_03_es_cons_estab_emp */

ON RIGHT-MOUSE-DOWN OF FRAME f_mop_03_es_cons_estab_emp ANYWHERE
DO:

    /************************* Variable Definition Begin ************************/

    def var v_wgh_frame
        as widget-handle
        format ">>>>>>9":U
        no-undo.

    /************************** Variable Definition End *************************/

    /* Begin_Include: i_right_mouse_down_dialog_box */
    if  (self:type <> "DIALOG-BOX")
    and (self:type <> "FRAME")
    and (self:type <> "text")
    and (self:type <> "IMAGE")
    and (self:type <> "RECTANGLE")
    then do:
        assign v_wgh_frame = self:parent.

        if  self:type        = "fill-in"
        and v_wgh_frame:type = "Browse" then
            return no-apply.

        if  valid-handle(self:popup-menu) = yes then
            return no-apply.

        assign v_wgh_frame = self:frame.

        if  (v_wgh_frame:type <> "DIALOG-BOX") and (v_wgh_frame:frame <> ?)
        then do:
               assign v_wgh_frame     = v_wgh_frame:frame.
        end.
        assign v_nom_title_aux    = v_wgh_frame:title
               v_wgh_frame:title  = self:help.
    end.
    /* End_Include: i_right_mouse_down_dialog_box */

END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_mop_03_es_cons_estab_emp */

ON RIGHT-MOUSE-UP OF FRAME f_mop_03_es_cons_estab_emp ANYWHERE
DO:

    /************************* Variable Definition Begin ************************/

    def var v_wgh_frame
        as widget-handle
        format ">>>>>>9":U
        no-undo.

    /************************** Variable Definition End *************************/

    /* Begin_Include: i_right_mouse_up_dialog_box */
    if  (self:type <> "DIALOG-BOX")
    and (self:type <> "FRAME")
    and (self:type <> "text")
    and (self:type <> "IMAGE")
    and (self:type <> "RECTANGLE")
    then do:
        assign v_wgh_frame = self:parent.

        if  self:type        = "fill-in"
        and v_wgh_frame:type = "Browse" then
            return no-apply.

        if  valid-handle(self:popup-menu) = yes then
            return no-apply.

        assign v_wgh_frame        = self:frame.
        if  (v_wgh_frame:type <> "DIALOG-BOX") and (v_wgh_frame:frame <> ?)
        then do:
               assign v_wgh_frame     = v_wgh_frame:frame.
        end.
        assign v_wgh_frame:title  = v_nom_title_aux.
    end.

    /* End_Include: i_right_mouse_up_dialog_box */

END. /* ON RIGHT-MOUSE-UP OF FRAME f_mop_03_es_cons_estab_emp */

ON WINDOW-CLOSE OF FRAME f_mop_03_es_cons_estab_emp
DO:

    apply "end-error" to self.
END. /* ON WINDOW-CLOSE OF FRAME f_mop_03_es_cons_estab_emp */

/***************************** Frame Trigger End ****************************/

/**************************** Menu Trigger Begin ****************************/

ON CHOOSE OF MENU-ITEM mi_conteudo IN MENU m_help
DO:

        apply "choose" to bt_hel2 in frame f_mop_03_es_cons_estab_emp.

END. /* ON CHOOSE OF MENU-ITEM mi_conteudo IN MENU m_help */

ON CHOOSE OF MENU-ITEM mi_sobre IN MENU m_help
DO:

    /************************* Variable Definition Begin ************************/

    def var v_cod_release
        as character
        format "x(12)":U
        no-undo.
    def var v_nom_prog
        as character
        format "x(08)":U
        no-undo.
    def var v_nom_prog_ext
        as character
        format "x(08)":U
        label "Nome Externo"
        no-undo.

    /************************** Variable Definition End *************************/

        assign v_nom_prog     = substring(frame f_mop_03_es_cons_estab_emp:title, 1, max(1, length(frame f_mop_03_es_cons_estab_emp:title) - 10)).
        if  v_nom_prog = ? then
            assign v_nom_prog = "".

        assign v_nom_prog     = v_nom_prog
                              + chr(10)
                              + "mod_es_cons_estab_emp":U.

    assign v_nom_prog_ext = "esp/esfgl071ea.p":U
           v_cod_release  = trim(" 1.00.00.000":U).

    {include/sobre5.i}
END. /* ON CHOOSE OF MENU-ITEM mi_sobre IN MENU m_help */

/***************************** Menu Trigger End *****************************/

/****************************** Main Code Begin *****************************/

/* Begin_Include: i_version_extract */
def new global shared var v_cod_arq
    as char
    format "x(60)":U
    no-undo.
def new global shared var v_cod_tip_prog
    as character
    format "x(08)":U
    no-undo.

def stream s-arq.

if  v_cod_arq <> '' and v_cod_arq <> ? then
    run pi_version_extract ('mod_es_cons_estab_emp':U, 'esp/esfgl071ea.p':U, '1.00.00.000':U, 'pro':U).
/* End_Include: i_version_extract */

if  search("prgtec/btb/btb906za.r") = ? and search("prgtec/btb/btb906za.py") = ? then do:
    if  v_cod_dwb_user begins 'es_' then
        return "Programa execut†vel n∆o foi encontrado: prgtec/btb/btb906za.py".
    else do:
        message "Programa execut†vel n∆o foi encontrado: prgtec/btb/btb906za.py"
               view-as alert-box error buttons ok.
        stop.
    end.
end.
else
    run prgtec/btb/btb906za.py /*prg_fnc_verify_controls*/.

/* Begin_Include: i_verify_security */
if  search("prgtec/men/men901za.r") = ? and search("prgtec/men/men901za.py") = ? then do:
    if  v_cod_dwb_user begins 'es_' then
        return "Programa execut†vel n∆o foi encontrado: prgtec/men/men901za.py".
    else do:
        message "Programa execut†vel n∆o foi encontrado: prgtec/men/men901za.py"
               view-as alert-box error buttons ok.
        return.
    end.
end.
else
    run prgtec/men/men901za.py (Input 'mod_es_cons_estab_emp') /*prg_fnc_verify_security*/.
if  return-value = "2014"
then do:
    /* Programa a ser executado n∆o Ç um programa v†lido Datasul ! */
    run pi_messages (input "show", input 2014,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'mod_es_cons_estab_emp')) /*msg_2014*/.
    return.
end.
if  return-value = "2012"
then do:
    /* Usu†rio sem permiss∆o para acessar o programa. */
    run pi_messages (input "show", input 2012,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'mod_es_cons_estab_emp')) /*msg_2014*/.
    return.
end.
/* End_Include: i_verify_security */

/* Begin_Include: i_log_exec_prog_dtsul_ini */
assign v_rec_log = ?.

if can-find(prog_dtsul
       where prog_dtsul.cod_prog_dtsul = 'mod_es_cons_estab_emp' 
         and prog_dtsul.log_gera_log_exec = yes) then do transaction:
    create log_exec_prog_dtsul.
    assign log_exec_prog_dtsul.cod_prog_dtsul           = 'mod_es_cons_estab_emp'
           log_exec_prog_dtsul.cod_usuario              = v_cod_usuar_corren
           log_exec_prog_dtsul.dat_inic_exec_prog_dtsul = today
           log_exec_prog_dtsul.hra_inic_exec_prog_dtsul = replace(string(time,"hh:mm:ss"),":":U,"":U).
    assign v_rec_log = recid(log_exec_prog_dtsul).
    release log_exec_prog_dtsul no-error.
end.

/* End_Include: i_log_exec_prog_dtsul_ini */

/* ix_p00_mod_es_cons_estab_emp */

/* Begin_Include: i_std_dialog_box */
/* tratamento do titulo e vers∆o */
assign frame f_mop_03_es_cons_estab_emp:title = frame f_mop_03_es_cons_estab_emp:title
                            + chr(32)
                            + chr(40)
                            + trim(" 1.00.00.000":U)
                            + chr(41).
/* menu pop-up de ajuda e sobre */
assign menu m_help:popup-only = yes
       bt_hel2:popup-menu in frame f_mop_03_es_cons_estab_emp = menu m_help:handle.

/* End_Include: i_std_dialog_box */

{include/title5.i f_mop_03_es_cons_estab_emp FRAME}

pause 0 before-hide.
view frame f_mop_03_es_cons_estab_emp.

assign v_log_repeat = yes
       v_rec_table  = v_rec_es_cons_estab_emp.

main_block:
repeat while v_log_repeat:
    /* ix_p10_mod_es_cons_estab_emp */
    assign v_log_repeat  = no
           v_log_save_ok = no.

    find es_cons_estab_emp where recid(es_cons_estab_emp) = v_rec_table exclusive-lock no-error.

    /* Begin_Include: i_ref_es_cons_estab_emp */
    find empresa no-lock
         where empresa.cod_empresa = es_cons_estab_emp.cod_empresa no-error.
    /* End_Include: i_ref_es_cons_estab_emp */

    /* ix_p15_mod_es_cons_estab_emp */
    if  not retry
    then do:
        display es_cons_estab_emp.cod_empresa
                es_cons_estab_emp.cod_cenar_ctbl
                es_cons_estab_emp.dat_inic_valid
                es_cons_estab_emp.dat_fim_valid
                es_cons_estab_emp.cod_plano_cta_ctbl
                es_cons_estab_emp.cod_plano_ccusto
                with frame f_mop_03_es_cons_estab_emp.
        display empresa.nom_razao_social when avail empresa
                "" when not avail empresa @ empresa.nom_razao_social
                with frame f_mop_03_es_cons_estab_emp.
    end.
    enable es_cons_estab_emp.cod_empresa
           es_cons_estab_emp.cod_cenar_ctbl
           es_cons_estab_emp.dat_inic_valid
           es_cons_estab_emp.dat_fim_valid
           es_cons_estab_emp.cod_plano_cta_ctbl
           es_cons_estab_emp.cod_plano_ccusto
           bt_ok
           bt_sav
           bt_can
           bt_pre2
           bt_sea2
           bt_nex2
           bt_hel2
           with frame f_mop_03_es_cons_estab_emp.

    /* Begin_Include: ix_p20_mod_es_cons_estab_emp */
    disable es_cons_estab_emp.cod_empresa
            es_cons_estab_emp.cod_cenar_ctbl
            es_cons_estab_emp.dat_inic_valid
            with frame f_mop_03_es_cons_estab_emp.
    /* End_Include: ix_p20_mod_es_cons_estab_emp */

    wait_block:
    repeat on endkey undo main_block, leave main_block while v_log_save_ok = no:
        assign v_log_save  = no
               v_rec_table = recid(es_cons_estab_emp).
        find b_es_cons_estab_emp_mod where recid(b_es_cons_estab_emp_mod) = v_rec_table no-lock no-error.
        if  valid-handle(v_wgh_focus) then
            wait-for go of frame f_mop_03_es_cons_estab_emp focus v_wgh_focus.
        else
            wait-for go of frame f_mop_03_es_cons_estab_emp.

        if  v_log_save = no
        then do:
            if (es_cons_estab_emp.dat_fim_valid     :visible in frame f_mop_03_es_cons_estab_emp and 
                es_cons_estab_emp.dat_fim_valid     :sensitive in frame f_mop_03_es_cons_estab_emp and
                input frame f_mop_03_es_cons_estab_emp es_cons_estab_emp.dat_fim_valid      <> es_cons_estab_emp.dat_fim_valid     ) or
               (es_cons_estab_emp.cod_plano_cta_ctbl:visible in frame f_mop_03_es_cons_estab_emp and
                es_cons_estab_emp.cod_plano_cta_ctbl:sensitive in frame f_mop_03_es_cons_estab_emp and
                input frame f_mop_03_es_cons_estab_emp es_cons_estab_emp.cod_plano_cta_ctbl <> es_cons_estab_emp.cod_plano_cta_ctbl) or
               (es_cons_estab_emp.cod_plano_ccusto  :visible in frame f_mop_03_es_cons_estab_emp and
                es_cons_estab_emp.cod_plano_ccusto  :sensitive in frame f_mop_03_es_cons_estab_emp and
                input frame f_mop_03_es_cons_estab_emp es_cons_estab_emp.cod_plano_ccusto   <> es_cons_estab_emp.cod_plano_ccusto  )
                then do:
                message substitute("&1 sofreu alteraá‰es. Deseja salv†-las ?" /*l_mod_save*/ , "Estabelecimento")
                       view-as alert-box question buttons yes-no-cancel title substitute("&1", c-versao-prg) update v_log_answer.
                assign v_log_save = v_log_answer.
            end.
            /* ix_p21_mod_es_cons_estab_emp */
        end.
        if  v_log_save = yes
        then do:
            save_block:
            do on error undo save_block, leave save_block:

                /* Begin_Include: ix_p25_mod_es_cons_estab_emp */
                if  input frame f_mop_03_es_cons_estab_emp es_cons_estab_emp.dat_inic_valid >=
                    input frame f_mop_03_es_cons_estab_emp es_cons_estab_emp.dat_fim_valid
                then do:
                    /* Validade Inv†lida ! */
                    run pi_messages (input "show", input 610,
                                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_610*/.
                    assign v_wgh_focus = es_cons_estab_emp.dat_inic_valid:handle in frame f_mop_03_es_cons_estab_emp.
                    undo save_block, leave save_block.
                end.

                if  es_cons_estab_emp.cod_plano_cta_ctbl:screen-value in frame f_mop_03_es_cons_estab_emp = ""
                then do:
                    /* &1 deve ser informado(a) ! */
                    run pi_messages (input "show", input 106,
                                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9", "Plano de Conta Cont†bil")) /*msg_106*/.
                    assign v_wgh_focus = es_cons_estab_emp.cod_plano_cta_ctbl:handle in frame f_mop_03_es_cons_estab_emp.
                    undo save_block, leave save_block.
                end.

                if  es_cons_estab_emp.cod_plano_ccusto:screen-value in frame f_mop_03_es_cons_estab_emp = ""
                then do:
                    /* &1 deve ser informado(a) ! */
                    run pi_messages (input "show", input 106,
                                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9", "Plano de Centro de Custo")) /*msg_106*/.
                    assign v_wgh_focus = es_cons_estab_emp.cod_plano_ccusto:handle in frame f_mop_03_es_cons_estab_emp.
                    undo save_block, leave save_block.
                end.

                /* End_Include: ix_p25_mod_es_cons_estab_emp */

                run pi_save_fields /*pi_save_fields*/.

                assign v_log_save_ok = yes.
            end /* do save_block */.
        end.
        if  v_log_save = no then
            assign v_log_save_ok = yes.

    end /* repeat wait_block */.
end /* repeat main_block */.

/* ix_p40_mod_es_cons_estab_emp */

hide frame f_mop_03_es_cons_estab_emp.

/* Begin_Include: i_log_exec_prog_dtsul_fim */
if v_rec_log <> ? then do transaction:
    find log_exec_prog_dtsul where recid(log_exec_prog_dtsul) = v_rec_log exclusive-lock no-error.
    if  avail log_exec_prog_dtsul
    then do:
        assign log_exec_prog_dtsul.dat_fim_exec_prog_dtsul = today
               log_exec_prog_dtsul.hra_fim_exec_prog_dtsul = replace(string(time,"hh:mm:ss"),":":U,"":U).
    end.
    release log_exec_prog_dtsul.
end.
/* End_Include: i_log_exec_prog_dtsul_fim */

/******************************* Main Code End ******************************/

/************************* Internal Procedure Begin *************************/

/*****************************************************************************
** Procedure Interna.....: pi_save_fields
** Descricao.............: pi_save_fields
*****************************************************************************/
PROCEDURE pi_save_fields:

    assign_block:
    do on error undo assign_block, return error:
        assign input frame f_mop_03_es_cons_estab_emp es_cons_estab_emp.dat_fim_valid
               input frame f_mop_03_es_cons_estab_emp es_cons_estab_emp.cod_plano_cta_ctbl
               input frame f_mop_03_es_cons_estab_emp es_cons_estab_emp.cod_plano_ccusto.
        assign v_wgh_focus = ?.
    end /* do assign_block */.

    /* Foráar a criaá∆o do registro com bases Oracle */
    if recid(es_cons_estab_emp) <> ? then.
END PROCEDURE. /* pi_save_fields */
/*****************************************************************************
** Procedure Interna.....: pi_version_extract
** Descricao.............: pi_version_extract
*****************************************************************************/
PROCEDURE pi_version_extract:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_program
        as character
        format "x(08)":U
        no-undo.
    def Input param p_cod_program_ext
        as character
        format "x(08)":U
        no-undo.
    def Input param p_cod_version
        as character
        format "x(08)":U
        no-undo.
    def Input param p_cod_program_type
        as character
        format "x(08)":U
        no-undo.

    /************************* Parameter Definition End *************************/

    if  can-do(v_cod_tip_prog, p_cod_program_type)
    then do:
        if p_cod_program_type = 'dic' then
           assign p_cod_program_ext = replace(p_cod_program_ext, 'database/', '').

        output stream s-arq to value(v_cod_arq) no-echo no-convert append.

        put stream s-arq unformatted
            p_cod_program            at 1
            p_cod_program_ext        at 43
            p_cod_version            at 69
            today                    at 84 format "99/99/99"
            string(time, 'HH:MM:SS') at 94 skip.

        output stream s-arq close.
    end.

END PROCEDURE. /* pi_version_extract */

/************************** Internal Procedure End **************************/

&endif

/*************************************  *************************************/
/*****************************************************************************
**  Procedure Interna: pi_messages
**  Descricao........: Mostra Mensagem com Ajuda
*****************************************************************************/
PROCEDURE pi_messages:

    def input param c_action    as char    no-undo.
    def input param i_msg       as integer no-undo.
    def input param c_param     as char    no-undo.

    def var c_prg_msg           as char    no-undo.

    assign c_prg_msg = "messages/":U
                     + string(trunc(i_msg / 1000,0),"99":U)
                     + "/msg":U
                     + string(i_msg, "99999":U).

    if search(c_prg_msg + ".r":U) = ? and search(c_prg_msg + ".p":U) = ? then do:
        message "Mensagem nr. " i_msg "!!!":U skip
                "Programa Mensagem" c_prg_msg "n∆o encontrado."
                view-as alert-box error.
        return error.
    end.

    run value(c_prg_msg + ".p":U) (input c_action, input c_param).
    return return-value.
END PROCEDURE.  /* pi_messages */
/************************  End of mod_es_cons_estab_emp ***********************/

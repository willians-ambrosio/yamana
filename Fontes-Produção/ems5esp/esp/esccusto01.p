def var c-versao-prg as char init "5.0.00.000":U no-undo.

{include/i_dbinst.i}
{include/i_dbtype.i}

/******************************* Private-Data *******************************/
assign this-procedure:private-data = "HLP=15":U.
/*************************************  *************************************/

&if "{&emsuni_dbinst}" <> "yes" &then
run pi_messages(input "show", input 5884,
                 input substitute("&1~&2~&3~&4~&5~&6~&7~&8~&9", "EMSUNI")).
&else

/********************* Temporary Table Definition Begin *********************/

def temp-table tt_mapa_distrib_ccusto no-undo
    field cod_mapa_distrib_ccusto like mapa_distrib_ccusto.cod_mapa_distrib_ccusto
    field des_mapa_distrib_ccusto like mapa_distrib_ccusto.des_mapa_distrib_ccusto
    field l_replica               as log
    index ix-1 as primary unique cod_mapa_distrib_ccusto.

/********************** Temporary Table Definition End **********************/

/************************** Buffer Definition Begin *************************/

def buffer b-ccusto for ems5.ccusto.

/*************************** Buffer Definition End **************************/

/************************* Variable Definition Begin ************************/

def var v_cod_mapa_distrib_ccusto_fim
    as Character
    format "x(08)":U
    initial "ZZZZZZZZ"
    label "Mapa Final"
    column-label "Mapa"
    no-undo.
def var v_cod_mapa_distrib_ccusto_ini
    as Character
    format "x(08)":U
    label "Mapa Inicial"
    column-label "Mapa"
    no-undo.
def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.
def new global shared var v_cod_usuar_corren
    as character
    format "x(12)":U
    label "Usu†rio Corrente"
    column-label "Usu†rio Corrente"
    no-undo.
def var v_log_method
    as logical
    format "Sim/N∆o"
    initial yes
    no-undo.
def var v_log_repeat
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    no-undo.
def var v_nom_title_aux
    as character
    format "x(60)":U
    no-undo.
def var v_num_row_a
    as integer
    format ">>>,>>9":U
    no-undo.
def new global shared var v_rec_plano_ccusto
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.
def new global shared var v_rec_ccusto
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.
def var v_rec_table
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.
def new global shared var v_rec_ccusto
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.

/************************** Variable Definition End *************************/

/*************************** Menu Definition Begin **************************/

def menu      m_help      menubar
    menu-item mi_conteudo label "&Conte£do"
    menu-item mi_sobre    label "&Sobre".

/**************************** Menu Definition End ***************************/

/************************** Query Definition Begin **************************/

def query qr_frm_mapa_de   for tt_mapa_distrib_ccusto scrolling.
def query qr_frm_mapa_para for tt_mapa_distrib_ccusto scrolling.

/*************************** Query Definition End ***************************/

/************************** Browse Definition Begin *************************/

def browse br_frm_mapa_de query qr_frm_mapa_de display 
    tt_mapa_distrib_ccusto.cod_mapa_distrib_ccusto
    width-chars 8.00
        column-label "Mapa"
    tt_mapa_distrib_ccusto.des_mapa_distrib_ccusto
    width-chars 40.00
        column-label "T°tulo"
    with separators multiple 
         size 39.00 by 10.38
         font 4
         bgcolor 15
         title "Mapas Dispon°veis".

def browse br_frm_mapa_para query qr_frm_mapa_para display 
    tt_mapa_distrib_ccusto.cod_mapa_distrib_ccusto
    width-chars 8.00
        column-label "Mapa"
    tt_mapa_distrib_ccusto.des_mapa_distrib_ccusto
    width-chars 40.00
        column-label "T°tulo"
    with separators multiple 
         size 39.00 by 10.38
         font 4
         bgcolor 15
         title "Mapa para Replicar".

/*************************** Browse Definition End **************************/

/************************ Rectangle Definition Begin ************************/

def rectangle rt_cxcf size 1 by 1 edge-pixels 2 fgcolor 1.
def rectangle rt_key  size 1 by 1 edge-pixels 2.
def rectangle rt_mold size 1 by 1 edge-pixels 2.

/************************* Rectangle Definition End *************************/

/************************** Button Definition Begin *************************/

def button bt_can
    label "Cancela"
    tooltip "Cancela"
    size 1 by 1
    auto-endkey.
def button bt_del_frm
    label "<"
    tooltip "Retira Linha"
    image-up file "image/im-pre1"
    image-insensitive file "image/ii-pre1"
    size 1 by 1.
def button bt_faixa
    label "Faixa de Dados"
    tooltip "Faixa de Dados"
    image-up file "image/im-ran"
    image-insensitive file "image/ii-ran"
    size 1 by 1.
def button bt_del_todos
    label "Retira Todos"
    tooltip "Retira Todos"
    image file "image/im-rew.bmp"
    size 1 by 1.
def button bt_hel2
    label "Ajuda"
    tooltip "Ajuda"
    size 1 by 1.
def button bt_ins_frm
    label ">"
    tooltip "Insere Linha"
    image-up file "image/im-nex1"
    image-insensitive file "image/ii-nex1"
    size 1 by 1.
def button bt_ins_todos
    label "Seleciona Todos"
    tooltip "Seleciona Todos"
    image file "image/im-ff.bmp"
    size 1 by 1.
def button bt_ok
    label "OK"
    tooltip "OK"
    size 1 by 1
    auto-go.
def button bt_sav
    label "Salva"
    tooltip "Salva"
    size 1 by 1
    auto-go.
/****************************** Function Button *****************************/
def button bt_zoo_59187
    label "Zoom"
    tooltip "Zoom"
    image-up file "image/im-zoo"
    image-insensitive file "image/ii-zoo"
    size 4 by .88.

def button bt_zoo_59188
    label "Zoom"
    tooltip "Zoom"
    image-up file "image/im-zoo"
    image-insensitive file "image/ii-zoo"
    size 4 by .88.


/*************************** Button Definition End **************************/

/************************** Frame Definition Begin **************************/

def frame f_dlg_01_faixa_mapa
    rt_mold
         at row 01.21 col 02.00
    v_cod_mapa_distrib_ccusto_ini
         at row 01.79 col 23.00 colon-aligned label "Mapa Inicial"
         help "C¢digo Mapa"
         view-as fill-in
         size-chars 12.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_mapa_distrib_ccusto_fim
         at row 02.79 col 23.00 colon-aligned label "Mapa Final"
         help "C¢digo Mapa"
         view-as fill-in
         size-chars 12.14 by .88
         fgcolor ? bgcolor 15 font 2
    rt_cxcf
         at row 04.63 col 02.00 bgcolor 7 
    bt_ok
         at row 04.83 col 03.00 font ?
         help "OK"
    bt_can
         at row 04.83 col 14.00 font ?
         help "Cancela"
    bt_hel2
         at row 04.83 col 43.29 font ?
         help "Ajuda"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 55.72 by 06.46 default-button bt_ok
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Seleciona Faixa Mapa".
    /* adjust size of objects in this frame */
    assign bt_can :width-chars  in frame f_dlg_01_faixa_mapa = 10.00
           bt_can :height-chars in frame f_dlg_01_faixa_mapa = 01.00
           bt_hel2:width-chars  in frame f_dlg_01_faixa_mapa = 10.00
           bt_hel2:height-chars in frame f_dlg_01_faixa_mapa = 01.00
           bt_ok  :width-chars  in frame f_dlg_01_faixa_mapa = 10.00
           bt_ok  :height-chars in frame f_dlg_01_faixa_mapa = 01.00
           rt_cxcf:width-chars  in frame f_dlg_01_faixa_mapa = 52.29
           rt_cxcf:height-chars in frame f_dlg_01_faixa_mapa = 01.42
           rt_mold:width-chars  in frame f_dlg_01_faixa_mapa = 52.29
           rt_mold:height-chars in frame f_dlg_01_faixa_mapa = 03.04.

def frame f_frm_02_mapa_para
    rt_key
         at row 01.21 col 02.00
    plano_ccusto.cod_plano_ccusto
         at row 01.38 col 21.00 colon-aligned label "Plano de Custo"
         view-as fill-in
         size-chars 9.14 by .88
         fgcolor ? bgcolor 15 font 2
    plano_ccusto.des_tit_ctbl
         at row 01.38 col 36.70 no-label
         view-as fill-in
         size-chars 41.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_zoo_59187
         at row 01.38 col 32.50
    ems5.ccusto.cod_ccusto
         at row 02.38 col 21.00 colon-aligned label "Centro de Custo"
         view-as fill-in
         size-chars 9.50 by .88
         fgcolor ? bgcolor 15 font 2
    bt_zoo_59188
         at row 02.38 col 32.50
    ems5.ccusto.des_tit_ctbl
         at row 02.38 col 36.70 no-label
         view-as fill-in
         size-chars 41.14 by .88
         fgcolor ? bgcolor 15 font 2

    rt_mold
         at row 03.54 col 02.00
    br_frm_mapa_de
         at row 04.38 col 03.29
         help "Mapas Dispon°veis"
    br_frm_mapa_para
         at row 04.38 col 48.43
         help "Mapa para Replicar"
    bt_ins_frm
         at row 04.71 col 43.29 font ?
         help "Insere Linha"
    bt_ins_todos
         at row 06.71 col 43.29 font ?
         help "Seleciona Todos"
    bt_del_todos
         at row 08.71 col 43.29 font ?
         help "Retira Todos"
    bt_del_frm
         at row 10.71 col 43.29 font ?
         help "Retira Linha"
    bt_faixa
         at row 12.71 col 43.29 font ?
         help "Faixa de Dados"

    rt_cxcf
         at row 15.17 col 02.00 bgcolor 7 
    bt_ok
         at row 15.38 col 03.00 font ?
         help "OK"
    bt_sav
         at row 15.38 col 14.00 font ?
         help "Salva"
    bt_can
         at row 15.38 col 25.00 font ?
         help "Cancela"
    bt_hel2
         at row 15.38 col 77.57 font ?
         help "Ajuda"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 90.00 by 17.00 default-button bt_ok
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Formaá∆o Itens Autom†tico Centros Custo".
    /* adjust size of objects in this frame */
    assign bt_can      :width-chars  in frame f_frm_02_mapa_para = 10.00
           bt_can      :height-chars in frame f_frm_02_mapa_para = 01.00
           bt_del_frm  :width-chars  in frame f_frm_02_mapa_para = 04.00
           bt_del_frm  :height-chars in frame f_frm_02_mapa_para = 01.13
           bt_faixa    :width-chars  in frame f_frm_02_mapa_para = 04.00
           bt_faixa    :height-chars in frame f_frm_02_mapa_para = 01.13
           bt_del_todos:width-chars  in frame f_frm_02_mapa_para = 04.00
           bt_del_todos:height-chars in frame f_frm_02_mapa_para = 01.13
           bt_hel2     :width-chars  in frame f_frm_02_mapa_para = 10.00
           bt_hel2     :height-chars in frame f_frm_02_mapa_para = 01.00
           bt_ins_frm  :width-chars  in frame f_frm_02_mapa_para = 04.00
           bt_ins_frm  :height-chars in frame f_frm_02_mapa_para = 01.13
           bt_ins_todos:width-chars  in frame f_frm_02_mapa_para = 04.00
           bt_ins_todos:height-chars in frame f_frm_02_mapa_para = 01.13
           bt_ok       :width-chars  in frame f_frm_02_mapa_para = 10.00
           bt_ok       :height-chars in frame f_frm_02_mapa_para = 01.00
           bt_sav      :width-chars  in frame f_frm_02_mapa_para = 10.00
           bt_sav      :height-chars in frame f_frm_02_mapa_para = 01.00
           rt_cxcf     :width-chars  in frame f_frm_02_mapa_para = 86.57
           rt_cxcf     :height-chars in frame f_frm_02_mapa_para = 01.42
           rt_key      :width-chars  in frame f_frm_02_mapa_para = 86.57
           rt_key      :height-chars in frame f_frm_02_mapa_para = 02.29
           rt_mold     :width-chars  in frame f_frm_02_mapa_para = 86.57
           rt_mold     :height-chars in frame f_frm_02_mapa_para = 11.46.
    /* enable function buttons */
    assign bt_zoo_59187:sensitive in frame f_frm_02_mapa_para = yes
           bt_zoo_59188:sensitive in frame f_frm_02_mapa_para = yes.

/*************************** Frame Definition End ***************************/

/*********************** User Interface Trigger Begin ***********************/

ON CHOOSE OF bt_hel2 IN FRAME f_dlg_01_faixa_mapa
DO:
    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py(input self:frame,
                               input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help_frame */
END.

ON CTRL-CURSOR-RIGHT OF br_frm_mapa_de IN FRAME f_frm_02_mapa_para
DO:
    if  bt_ins_frm:sensitive in frame f_frm_02_mapa_para then
        apply "choose" to bt_ins_frm in frame f_frm_02_mapa_para.
END.

ON CHOOSE OF bt_del_frm IN FRAME f_frm_02_mapa_para
DO:
    retire:
    do  v_num_row_a = 1 to browse br_frm_mapa_para:num-selected-rows:
        assign v_log_method = browse br_frm_mapa_para:fetch-selected-row(v_num_row_a).
        assign v_rec_table  = recid(tt_mapa_distrib_ccusto).
        find first tt_mapa_distrib_ccusto
             where recid(tt_mapa_distrib_ccusto) = v_rec_table no-error.
        if  avail tt_mapa_distrib_ccusto then
            assign tt_mapa_distrib_ccusto.l_replica = no.
    end.
    run pi_open_frm_mapa_para.
END.

ON CHOOSE OF bt_del_todos IN FRAME f_frm_02_mapa_para
DO:
    session:set-wait-state('general').
    for each tt_mapa_distrib_ccusto no-lock
       where tt_mapa_distrib_ccusto.cod_mapa_distrib_ccusto >= v_cod_mapa_distrib_ccusto_ini
         and tt_mapa_distrib_ccusto.cod_mapa_distrib_ccusto <= v_cod_mapa_distrib_ccusto_fim:
        assign tt_mapa_distrib_ccusto.l_replica = no.
    end.
    session:set-wait-state('').
    run pi_open_frm_mapa_para.
END.

ON CHOOSE OF bt_faixa IN FRAME f_frm_02_mapa_para
DO:
    view frame f_dlg_01_faixa_mapa.

    disp bt_can
         bt_hel2
         bt_ok
         v_cod_mapa_distrib_ccusto_fim
         v_cod_mapa_distrib_ccusto_ini
         with frame f_dlg_01_faixa_mapa.

    enable all with frame f_dlg_01_faixa_mapa.

    ccusto_block:
    do on error  undo ccusto_block, leave ccusto_block
       on endkey undo ccusto_block, leave ccusto_block:

        disp v_cod_mapa_distrib_ccusto_fim v_cod_mapa_distrib_ccusto_ini with frame f_dlg_01_faixa_mapa.

        wait-for go of frame f_dlg_01_faixa_mapa.
        assign v_cod_mapa_distrib_ccusto_fim = input frame f_dlg_01_faixa_mapa v_cod_mapa_distrib_ccusto_fim
               v_cod_mapa_distrib_ccusto_ini = input frame f_dlg_01_faixa_mapa v_cod_mapa_distrib_ccusto_ini. 
        run pi_open_frm_mapa_para.
    end.

    hide frame f_dlg_01_faixa_mapa.
END.

ON CHOOSE OF bt_hel2 IN FRAME f_frm_02_mapa_para
DO:
    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py(input self:frame,
                               input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help_frame */
END.

ON CHOOSE OF bt_ins_frm IN FRAME f_frm_02_mapa_para
DO:
    insere:
    do v_num_row_a = 1 to browse br_frm_mapa_de:num-selected-rows:
        assign v_log_method = browse br_frm_mapa_de:fetch-selected-row(v_num_row_a).
        assign v_rec_table  = recid(tt_mapa_distrib_ccusto).

        find first tt_mapa_distrib_ccusto
             where recid(tt_mapa_distrib_ccusto) = v_rec_table no-error.
        if  avail tt_mapa_distrib_ccusto then
            assign tt_mapa_distrib_ccusto.l_replica = yes.

/*         assign v_log_method = browse br_frm_mapa_de:deselect-selected-row(v_num_row_a). */
    end.
    run pi_open_frm_mapa_para.
END.

ON CHOOSE OF bt_ins_todos IN FRAME f_frm_02_mapa_para
DO:
    session:set-wait-state('general').
    for each tt_mapa_distrib_ccusto no-lock
       where tt_mapa_distrib_ccusto.cod_mapa_distrib_ccusto >= v_cod_mapa_distrib_ccusto_ini
         and tt_mapa_distrib_ccusto.cod_mapa_distrib_ccusto <= v_cod_mapa_distrib_ccusto_fim:
        assign tt_mapa_distrib_ccusto.l_replica = yes.
    end.
    session:set-wait-state('').
    run pi_open_frm_mapa_para.
END.

ON CHOOSE OF bt_ok IN FRAME f_frm_02_mapa_para
DO:
    assign v_log_repeat = no.
    run pi_salva_alteracoes.
END.

ON CHOOSE OF bt_sav IN FRAME f_frm_02_mapa_para
DO:
    assign v_log_repeat = yes.
    run pi_salva_alteracoes.
END.

ON LEAVE OF ems5.ccusto.cod_ccusto IN FRAME f_frm_02_mapa_para
DO:
    find first ems5.ccusto no-lock
         where ems5.ccusto.cod_ccusto = input frame f_frm_02_mapa_para ems5.ccusto.cod_ccusto no-error.
    disp ems5.ccusto.des_tit_ctbl when avail ems5.ccusto
         "" when not avail ems5.ccusto @ ems5.ccusto.des_tit_ctbl
         with frame f_frm_02_mapa_para.

    if  avail ems5.ccusto then do:
        assign bt_ins_frm:sensitive in frame f_frm_02_mapa_para = yes.
        run pi_open_frm_mapa_para.
    end.
    else
        assign bt_ins_frm:sensitive in frame f_frm_02_mapa_para = no.
END.

/************************ User Interface Trigger End ************************/

/************************** Function Trigger Begin **************************/

ON  CHOOSE OF bt_zoo_59187 IN FRAME f_frm_02_mapa_para
OR F5 OF plano_ccusto.cod_plano_ccusto IN FRAME f_frm_02_mapa_para DO:
    /* fn_generic_zoom */
    if  search("prgint/utb/utb083ka.r") = ? and search("prgint/utb/utb083ka.p") = ? then do:
        message "Programa execut†vel n∆o foi encontrado: prgint/utb/utb083ka.p"
               view-as alert-box error buttons ok.
        return.
    end.
    else
        run prgint/utb/utb083ka.p.
    if  v_rec_plano_ccusto <> ? then do:
        find first plano_ccusto no-lock
             where recid(plano_ccusto) = v_rec_plano_ccusto no-error.
        assign plano_ccusto.cod_plano_ccusto:screen-value in frame f_frm_02_mapa_para = string(plano_ccusto.cod_plano_ccusto).

        disp plano_ccusto.des_tit_ctbl with frame f_frm_02_mapa_para.
    end.
    apply "entry" to plano_ccusto.cod_plano_ccusto in frame f_frm_02_mapa_para.
end.

ON  CHOOSE OF bt_zoo_59188 IN FRAME f_frm_02_mapa_para
OR F5 OF ems5.ccusto.cod_ccusto IN FRAME f_frm_02_mapa_para DO:
    if  v_rec_plano_ccusto = ? then do:
        find first plano_ccusto no-lock
             where plano_ccusto.cod_empresa      = v_cod_empres_usuar
               and plano_ccusto.cod_plano_ccusto = input frame f_frm_02_mapa_para plano_ccusto.cod_plano_ccusto
               and plano_ccusto.dat_fim_valid   >= today no-error.
        if  avail plano_ccusto then
            assign v_rec_plano_ccusto = recid(plano_ccusto).
    end.

    /* fn_generic_zoom */
    if  search("prgint/utb/utb066ka.r") = ? and search("prgint/utb/utb066ka.p") = ? then do:
        message "Programa execut†vel n∆o foi encontrado: prgint/utb/utb066ka.p"
               view-as alert-box error buttons ok.
        return.
    end.
    else
        run prgint/utb/utb066ka.p.
    if  v_rec_ccusto <> ? then do:
        find first plano_ccusto no-lock
             where recid(plano_ccusto) = v_rec_plano_ccusto no-error.
        assign plano_ccusto.cod_plano_ccusto:screen-value in frame f_frm_02_mapa_para = string(plano_ccusto.cod_plano_ccusto).

        find first ems5.ccusto no-lock
             where recid(ems5.ccusto) = v_rec_ccusto no-error.
        assign ems5.ccusto.cod_ccusto:screen-value in frame f_frm_02_mapa_para = string(ems5.ccusto.cod_ccusto).

        disp ems5.ccusto.des_tit_ctbl with frame f_frm_02_mapa_para.
    end.
    apply "entry" to ems5.ccusto.cod_ccusto in frame f_frm_02_mapa_para.
end.


/*************************** Function Trigger End ***************************/

/**************************** Frame Trigger Begin ***************************/

ON HELP OF FRAME f_dlg_01_faixa_mapa ANYWHERE
DO:
    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py(input self:handle,
                               input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */
END.

ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_01_faixa_mapa ANYWHERE
DO:
    def var v_wgh_frame as widget-handle format ">>>>>>9":U no-undo.

    /* Begin_Include: i_right_mouse_down_dialog_box */
    if  (self:type <> "DIALOG-BOX") and
        (self:type <> "FRAME"     ) and
        (self:type <> "text"      ) and
        (self:type <> "IMAGE"     ) and
        (self:type <> "RECTANGLE" ) then do:
        assign v_wgh_frame = self:parent.

        if  self:type = "fill-in" and v_wgh_frame:type = "Browse" then
            return no-apply.

        if  valid-handle(self:popup-menu) then
            return no-apply.

        assign v_wgh_frame = self:frame.
        if  (v_wgh_frame:type <> "DIALOG-BOX") and (v_wgh_frame:frame <> ?) then
            assign v_wgh_frame = v_wgh_frame:frame.

        assign v_nom_title_aux   = v_wgh_frame:title
               v_wgh_frame:title = self:help.
    end.
    /* End_Include: i_right_mouse_down_dialog_box */
END.

ON RIGHT-MOUSE-UP OF FRAME f_dlg_01_faixa_mapa ANYWHERE
DO:
    def var v_wgh_frame as widget-handle format ">>>>>>9":U no-undo.

    /* Begin_Include: i_right_mouse_up_dialog_box */
    if  (self:type <> "DIALOG-BOX") and
        (self:type <> "FRAME"     ) and
        (self:type <> "text"      ) and
        (self:type <> "IMAGE"     ) and
        (self:type <> "RECTANGLE" ) then do:
        assign v_wgh_frame = self:parent.

        if  self:type = "fill-in" and v_wgh_frame:type = "Browse" then
            return no-apply.

        if  valid-handle(self:popup-menu) then
            return no-apply.

        assign v_wgh_frame = self:frame.
        if  (v_wgh_frame:type <> "DIALOG-BOX") and (v_wgh_frame:frame <> ?) then
            assign v_wgh_frame = v_wgh_frame:frame.

        assign v_wgh_frame:title = v_nom_title_aux.
    end.
    /* End_Include: i_right_mouse_up_dialog_box */
END.

ON WINDOW-CLOSE OF FRAME f_dlg_01_faixa_mapa
DO:
    apply "end-error" to self.
END.

ON HELP OF FRAME f_frm_02_mapa_para ANYWHERE
DO:
    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py(input self:handle,
                               input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */
END.

ON RIGHT-MOUSE-DOWN OF FRAME f_frm_02_mapa_para ANYWHERE
DO:
    def var v_wgh_frame as widget-handle format ">>>>>>9":U no-undo.

    /* Begin_Include: i_right_mouse_down_dialog_box */
    if  (self:type <> "DIALOG-BOX") and
        (self:type <> "FRAME"     ) and
        (self:type <> "text"      ) and
        (self:type <> "IMAGE"     ) and
        (self:type <> "RECTANGLE" ) then do:
        assign v_wgh_frame = self:parent.

        if  self:type = "fill-in" and v_wgh_frame:type = "Browse" then
            return no-apply.

        if  valid-handle(self:popup-menu) then
            return no-apply.

        assign v_wgh_frame = self:frame.
        if  (v_wgh_frame:type <> "DIALOG-BOX") and (v_wgh_frame:frame <> ?) then
            assign v_wgh_frame = v_wgh_frame:frame.

        assign v_nom_title_aux   = v_wgh_frame:title
               v_wgh_frame:title = self:help.
    end.
    /* End_Include: i_right_mouse_down_dialog_box */
END.

ON RIGHT-MOUSE-UP OF FRAME f_frm_02_mapa_para ANYWHERE
DO:
    def var v_wgh_frame as widget-handle format ">>>>>>9":U no-undo.

    /* Begin_Include: i_right_mouse_up_dialog_box */
    if  (self:type <> "DIALOG-BOX") and
        (self:type <> "FRAME"     ) and
        (self:type <> "text"      ) and
        (self:type <> "IMAGE"     ) and
        (self:type <> "RECTANGLE" ) then do:
        assign v_wgh_frame = self:parent.

        if  self:type = "fill-in" and v_wgh_frame:type = "Browse" then
            return no-apply.

        if  valid-handle(self:popup-menu) then
            return no-apply.

        assign v_wgh_frame = self:frame.
        if  (v_wgh_frame:type <> "DIALOG-BOX") and (v_wgh_frame:frame <> ?) then
            assign v_wgh_frame = v_wgh_frame:frame.

        assign v_wgh_frame:title = v_nom_title_aux.
    end.
    /* End_Include: i_right_mouse_up_dialog_box */
END.

ON WINDOW-CLOSE OF FRAME f_frm_02_mapa_para
DO:
    apply "end-error" to self.
END.

/***************************** Frame Trigger End ****************************/

/**************************** Menu Trigger Begin ****************************/

ON CHOOSE OF MENU-ITEM mi_conteudo IN MENU m_help
DO:
    apply "choose" to bt_hel2 in frame f_frm_02_mapa_para.
END.

ON CHOOSE OF MENU-ITEM mi_sobre IN MENU m_help
DO:
    def var v_cod_release  as char format "x(12)":U no-undo.
    def var v_nom_prog     as char format "x(8)":U  no-undo.
    def var v_nom_prog_ext as char format "x(8)":U  label "Nome Externo" no-undo.

    assign v_nom_prog = substr(frame f_frm_02_mapa_para:title, 1, max(1, length(frame f_frm_02_mapa_para:title) - 10)).

    if  v_nom_prog = ? then
        assign v_nom_prog = "".

    assign v_nom_prog     = v_nom_prog + chr(10) + "frm_mapa_para":U.

    assign v_nom_prog_ext = "prgint/utb/utb028ma.p":U
           v_cod_release  = trim("5.06.00.000":U).

    run prgtec/btb/btb901zb.p(input v_nom_prog,
                              input v_nom_prog_ext,
                              input v_cod_release) /*prg_fnc_about*/.
END.

/***************************** Menu Trigger End *****************************/

/****************************** Main Code Begin *****************************/

/* Begin_Include: i_version_extract */
def new global shared var v_cod_arq
    as char  
    format 'x(60)'
    no-undo.
def new global shared var v_cod_tip_prog
    as character
    format 'x(8)'
    no-undo.

def stream s-arq.

if  v_cod_arq <> '' and v_cod_arq <> ? then
    run pi_version_extract('frm_mapa_para':U, 'prgint/utb/utb028ma.p':U, '1.00.00.008':U, 'pro':U).
/* End_Include: i_version_extract */

if  search("prgtec/btb/btb906za.r") = ? and search("prgtec/btb/btb906za.py") = ? then do:
    message "Programa execut†vel n∆o foi encontrado: prgtec/btb/btb906za.py"
           view-as alert-box error buttons ok.
    stop.
end.
else
    run prgtec/btb/btb906za.py /*prg_fnc_verify_controls*/.

/* Begin_Include: i_verify_security */
if  search("prgtec/men/men901za.r") = ? and search("prgtec/men/men901za.py") = ? then do:
    message "Programa execut†vel n∆o foi encontrado: prgtec/men/men901za.py"
           view-as alert-box error buttons ok.
    return.
end.
else
    run prgtec/men/men901za.py (input 'frm_mapa_para') /*prg_fnc_verify_security*/.
if  return-value = "2014" then do:
    /* Programa a ser executado n∆o Ç um programa v†lido Datasul ! */
    run pi_messages(input "show", input 2014,
                    input substitute("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                     'frm_mapa_para')).
    return.
end.
if  return-value = "2012" then do:
    /* Usu†rio sem permiss∆o para acessar o programa. */
    run pi_messages(input "show", input 2012,
                    input substitute("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                     'frm_mapa_para')).
    return.
end.
/* End_Include: i_verify_security */

/* ix_p00_frm_mapa_para */

/* redefiniá‰es do frame */

/* Begin_Include: i_std_dialog_box */
/* tratamento do titulo e vers∆o */
assign frame f_frm_02_mapa_para:title = frame f_frm_02_mapa_para:title + chr(32) + chr(40) +
                                        trim("5.06.00.000":U) + chr(41).
/* menu pop-up de ajuda e sobre */
assign menu m_help:popup-only = yes
       bt_hel2:popup-menu in frame f_frm_02_mapa_para = menu m_help:handle.

/* End_Include: i_std_dialog_box */

pause 0 before-hide.
view frame f_frm_02_mapa_para.

assign v_rec_table  = ?
       v_log_repeat = yes.

find first plano_ccusto no-lock
     where plano_ccusto.cod_empresa    = v_cod_empres_usuar
       and plano_ccusto.dat_fim_valid >= today no-error.
if  not avail plano_ccusto then do:
    message "N∆o existem ocorràncias na tabela."
           view-as alert-box warning buttons ok.
    return.
end.
else
    assign plano_ccusto.cod_plano_ccusto:screen-value in frame f_frm_02_mapa_para = plano_ccusto.cod_plano_ccusto
           plano_ccusto.des_tit_ctbl    :screen-value in frame f_frm_02_mapa_para = plano_ccusto.des_tit_ctbl
           v_rec_plano_ccusto = recid(plano_ccusto).

for each tt_mapa_distrib_ccusto: delete tt_mapa_distrib_ccusto. end.

for each mapa_distrib_ccusto no-lock
   where mapa_distrib_ccusto.cod_empresa                 = v_cod_empres_usuar
     and mapa_distrib_ccusto.ind_tip_mapa_distrib_ccusto = 'lista'
     and mapa_distrib_ccusto.dat_fim_valid              >= today:

    find first tt_mapa_distrib_ccusto
         where tt_mapa_distrib_ccusto.cod_mapa_distrib_ccusto = mapa_distrib_ccusto.cod_mapa_distrib_ccusto no-error.
    if  avail tt_mapa_distrib_ccusto then next.

    create tt_mapa_distrib_ccusto no-error.
    assign tt_mapa_distrib_ccusto.cod_mapa_distrib_ccusto = mapa_distrib_ccusto.cod_mapa_distrib_ccusto
           tt_mapa_distrib_ccusto.des_mapa_distrib_ccusto = mapa_distrib_ccusto.des_mapa_distrib_ccusto.
end.

main_block:
repeat while v_log_repeat
    on endkey undo main_block, leave main_block
    on error undo main_block, leave main_block trans:
    /* ix_p10_frm_mapa_para */
    enable plano_ccusto.cod_plano_ccusto
           ems5.ccusto.cod_ccusto
           bt_zoo_59187
           bt_zoo_59188
           br_frm_mapa_de
           bt_ins_frm
           bt_del_todos
           bt_ins_todos
           bt_del_frm
           bt_faixa
           br_frm_mapa_para
           bt_ok
           bt_sav
           bt_can
           bt_hel2
           with frame f_frm_02_mapa_para.

    assign br_frm_mapa_de:sensitive in frame f_frm_02_mapa_para = yes
           br_frm_mapa_de:visible   in frame f_frm_02_mapa_para = yes
           bt_ins_frm    :sensitive in frame f_frm_02_mapa_para = yes.

    run pi_open_frm_mapa_para.

    /* ix_p20_frm_mapa_para */
    wait-for go of frame f_frm_02_mapa_para.

    /* ix_p30_frm_mapa_para */
end.

hide frame f_frm_02_mapa_para.


/******************************* Main Code End ******************************/

/************************* Internal procedure Begin *************************/

procedure pi_open_frm_mapa_para:
    open query qr_frm_mapa_de
      for each tt_mapa_distrib_ccusto no-lock
         where tt_mapa_distrib_ccusto.cod_mapa_distrib_ccusto >= v_cod_mapa_distrib_ccusto_ini 
           and tt_mapa_distrib_ccusto.cod_mapa_distrib_ccusto <= v_cod_mapa_distrib_ccusto_fim
           and not tt_mapa_distrib_ccusto.l_replica
            by tt_mapa_distrib_ccusto.cod_mapa_distrib_ccusto.

    open query qr_frm_mapa_para
      for each tt_mapa_distrib_ccusto no-lock
         where tt_mapa_distrib_ccusto.cod_mapa_distrib_ccusto >= v_cod_mapa_distrib_ccusto_ini 
           and tt_mapa_distrib_ccusto.cod_mapa_distrib_ccusto <= v_cod_mapa_distrib_ccusto_fim
           and tt_mapa_distrib_ccusto.l_replica
            by tt_mapa_distrib_ccusto.cod_mapa_distrib_ccusto.
end procedure.

procedure pi_salva_alteracoes:
    for each ems5.ccusto no-lock
       where ems5.ccusto.cod_empresa      = v_cod_empres_usuar
         and ems5.ccusto.cod_plano_ccusto = input frame f_frm_02_mapa_para plano_ccusto.cod_plano_ccusto
         and ems5.ccusto.cod_ccusto       = input frame f_frm_02_mapa_para ems5.ccusto.cod_ccusto:
        for each ems5.empresa no-lock
           where ems5.empresa.cod_empresa <> v_cod_empres_usuar:
            find first b-ccusto exclusive-lock
                 where b-ccusto.cod_empresa      = ems5.empresa.cod_empresa
                   and b-ccusto.cod_plano_ccusto = ems5.ccusto.cod_plano_ccusto
                   and b-ccusto.cod_ccusto       = ems5.ccusto.cod_ccusto no-error.
            if  not avail b-ccusto then do:
                create b-ccusto no-error.
                assign b-ccusto.cod_empresa      = ems5.empresa.cod_empresa
                       b-ccusto.cod_plano_ccusto = ems5.ccusto.cod_plano_ccusto
                       b-ccusto.cod_ccusto       = ems5.ccusto.cod_ccusto.
            end.

            assign b-ccusto.des_tit_ctbl           = ems5.ccusto.des_tit_ctbl
                   b-ccusto.dat_inic_valid         = ems5.ccusto.dat_inic_valid
                   b-ccusto.dat_fim_valid          = ems5.ccusto.dat_fim_valid
                   b-ccusto.cod_usuar_respons      = ems5.ccusto.cod_usuar_respons
                   b-ccusto.log_permit_lancto_ctbl = ems5.ccusto.log_permit_lancto_ctbl
                   b-ccusto.des_anot_tab           = ems5.ccusto.des_anot_tab
                   b-ccusto.cod_livre_1            = ems5.ccusto.cod_livre_1
                   b-ccusto.log_livre_1            = ems5.ccusto.log_livre_1
                   b-ccusto.num_livre_1            = ems5.ccusto.num_livre_1
                   b-ccusto.val_livre_1            = ems5.ccusto.val_livre_1
                   b-ccusto.dat_livre_1            = ems5.ccusto.dat_livre_1.
        end.
    end.

    for each tt_mapa_distrib_ccusto no-lock
       where tt_mapa_distrib_ccusto.l_replica,
        each mapa_distrib_ccusto no-lock
       where mapa_distrib_ccusto.cod_mapa_distrib_ccusto     = tt_mapa_distrib_ccusto.cod_mapa_distrib_ccusto
         and mapa_distrib_ccusto.ind_tip_mapa_distrib_ccusto = 'lista'
         and mapa_distrib_ccusto.dat_fim_valid              >= today:

        find first item_lista_ccusto no-lock
             where item_lista_ccusto.cod_estab               = mapa_distrib_ccusto.cod_estab
               and item_lista_ccusto.cod_mapa_distrib_ccusto = mapa_distrib_ccusto.cod_mapa_distrib_ccusto
               and item_lista_ccusto.cod_empresa             = mapa_distrib_ccusto.cod_empresa
               and item_lista_ccusto.cod_plano_ccusto        = input frame f_frm_02_mapa_para plano_ccusto.cod_plano_ccusto
               and item_lista_ccusto.cod_ccusto              = input frame f_frm_02_mapa_para ems5.ccusto.cod_ccusto no-error.
        if  not avail item_lista_ccusto then do:
            create item_lista_ccusto no-error.
            assign item_lista_ccusto.cod_estab               = mapa_distrib_ccusto.cod_estab
                   item_lista_ccusto.cod_mapa_distrib_ccusto = mapa_distrib_ccusto.cod_mapa_distrib_ccusto
                   item_lista_ccusto.cod_empresa             = mapa_distrib_ccusto.cod_empresa
                   item_lista_ccusto.cod_plano_ccusto        = input frame f_frm_02_mapa_para plano_ccusto.cod_plano_ccusto
                   item_lista_ccusto.cod_ccusto              = input frame f_frm_02_mapa_para ems5.ccusto.cod_ccusto
                   item_lista_ccusto.dat_livre_1             = today.
        end.
    end.

    message 'Fim do processamento' view-as alert-box.
end procedure.

procedure pi_version_extract:
    def input param p_cod_program      as char format "x(8)" no-undo.
    def input param p_cod_program_ext  as char format "x(8)" no-undo.
    def input param p_cod_version      as char format "x(8)" no-undo.
    def input param p_cod_program_type as char format "x(8)" no-undo.

    if  can-do(v_cod_tip_prog, p_cod_program_type) then do:
        if  p_cod_program_type = 'dic' then 
            assign p_cod_program_ext = replace(p_cod_program_ext, 'database/', '').

        output stream s-arq to value(v_cod_arq) append.
        put stream s-arq unformatted
            p_cod_program            at 01
            p_cod_program_ext        at 43 
            p_cod_version            at 69 
            today                    at 84 
            string(time, 'HH:MM:SS') at 94 skip.
        output stream s-arq close.
    end.
end procedure.

/************************** Internal procedure End **************************/
&endif

/*************************************  *************************************/
/*****************************************************************************
**  procedure Interna: pi_messages
**  Descricao........: Mostra Mensagem com Ajuda
*****************************************************************************/
procedure pi_messages:
    def input param c_action as char no-undo.
    def input param i_msg    as int  no-undo.
    def input param c_param  as char no-undo.

    def var c_prg_msg        as char no-undo.

    assign c_prg_msg = "messages/":U + string(trunc(i_msg / 1000,0),"99":U) +
                       "/msg":U + string(i_msg, "99999":U).

    if  search(c_prg_msg + ".r":U) = ? and
        search(c_prg_msg + ".p":U) = ? then do:
        message "Mensagem nr. " i_msg "!!!":U skip
                "Programa Mensagem" c_prg_msg "n∆o encontrado."
            view-as alert-box error.
        return error.
    end.

    run value(c_prg_msg + ".p":U) (input c_action, input c_param).
    return return-value.
end procedure.
/**********************  End of frm_mapa_para *********************/

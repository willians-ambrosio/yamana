/*****************************************************************************
** Programa..............: frm_es_cons_cta_ctbl
** Descricao.............: Formaá∆o(Filho) De Para Cta Ctbl X Empresa
** Versao................:  1.00.00.000
** Procedimento..........: man_es_cons_estab_emp
** Nome Externo..........: esp/esfgl071mb.p
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
{include/i-license-manager.i frm_es_cons_cta_ctbl FGL}
&ENDIF

/******************************* Private-Data *******************************/
assign this-procedure:private-data = "HLP=15":U.
/*************************************  *************************************/

&if "{&emsuni_dbinst}" <> "yes" &then
run pi_messages (input "show", input 5884, input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9", "EMSUNI")) /*msg_5884*/.
&elseif "{&emsuni_version}" < "1.00" &then
run pi_messages (input "show", input 5009,
                 input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9",
                                    "FRM_ES_CONS_CTA_CTBL","~~EMSUNI", "~~{~&emsuni_version}", "~~1.00")) /*msg_5009*/.
&else

/************************** Buffer Definition Begin *************************/
def temp-table tt_cta_ctbl_de   no-undo like cta_ctbl.
def temp-table tt_cta_ctbl_para no-undo like cta_ctbl.

def buffer b_es_cons_cta_ctbl_delete
    for es_cons_cta_ctbl.
def buffer b_es_cons_cta_ctbl_mod
    for es_cons_cta_ctbl.
def buffer b_cta_ctbl_para
    for cta_ctbl.

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
def var v_log_exist
    as logical
    format "Sim/N∆o"
    initial yes
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
def var v_rec_log
    as recid
    format ">>>>>>9":U
    no-undo.
def var v_rec_table
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.
def var v_lista_plano
    as character
    no-undo.
def var v_cod_plano_cta_ctbl
    as character
    format "x(30)"
    view-as combo-box
    list-items ""
    inner-lines 5
    bgcolor 15 font 2
    label "Plano de Contas"
    column-label "Plano de Contas"
    no-undo.


/************************** Variable Definition End *************************/

/*************************** Menu Definition Begin **************************/

.

def menu      m_help                menubar
    menu-item mi_conteudo           label "&Conte£do"
    menu-item mi_sobre              label "&Sobre".

/**************************** Menu Definition End ***************************/

/************************** Query Definition Begin **************************/

def query qr_frm_es_cons_cta_ctbl
    for es_cons_cta_ctbl
    scrolling.
def query qr_frm_cta_ctbl_de
    for tt_cta_ctbl_de
    scrolling.
def query qr_frm_cta_ctbl_para
    for tt_Cta_ctbl_para
    scrolling.

/*************************** Query Definition End ***************************/

/************************** Browse Definition Begin *************************/

def browse br_frm_es_cons_cta_ctbl query qr_frm_es_cons_cta_ctbl display 
    es_cons_cta_ctbl.cod_empresa
    width-chars 05.00
        column-label "Emp"
    es_cons_cta_ctbl.cod_plano_cta_ctbl
    width-chars 10.00
    es_cons_cta_ctbl.cod_cta_ctbl
    width-chars 12.00
        column-label "Cta Ctbl De"
    es_cons_cta_ctbl.cod_plano_cta_ctbl_para
    width-chars 10.00
    es_cons_cta_ctbl.cod_cta_ctbl_para
    width-chars 12.00
        column-label "Cta Ctbl Para"
    with separators multiple 
         size 39.00 by 11.00
         font 1
         bgcolor 15
         title "De Para de Cta Ctbl".

def browse br_frm_cta_ctbl_de query qr_frm_cta_ctbl_de display 
    tt_cta_ctbl_de.cod_plano_cta_ctbl
    width-chars 10.00
    tt_cta_ctbl_de.cod_cta_ctbl
    width-chars 12.00
    tt_cta_ctbl_de.des_tit_ctbl
    width-chars 40.00
    with separators multiple 
         size 39.00 by 05.50
         font 1
         bgcolor 15
         title "Cta Ctbl De".

def browse br_frm_cta_ctbl_para query qr_frm_cta_ctbl_para display 
    tt_cta_ctbl_para.cod_plano_cta_ctbl
    width-chars 10.00
    tt_cta_ctbl_para.cod_cta_ctbl
    width-chars 12.00
    tt_cta_ctbl_para.des_tit_ctbl
    width-chars 40.00
    with separators single 
         size 39.00 by 05.50
         font 1
         bgcolor 15
         title "Cta Ctbl Para".

/*************************** Browse Definition End **************************/

/************************ Rectangle Definition Begin ************************/

def rectangle rt_cxcf
    size 1 by 1
    fgcolor 1 edge-pixels 2.
def rectangle rt_key
    size 1 by 1
    edge-pixels 2.
def rectangle rt_mold
    size 1 by 1
    edge-pixels 2.

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

/*************************** Button Definition End **************************/

/************************** Frame Definition Begin **************************/

def frame f_frm_02_es_cons_cta_ctbl
    rt_mold
         at row 04.54 col 02.00
    rt_cxcf
         at row 16.56 col 02.00 bgcolor 7 
    rt_key
         at row 01.21 col 02.00
    es_cons_estab_emp.cod_empresa
         at row 01.38 col 25.00 colon-aligned
         view-as fill-in
         size-chars 5.00 by .88
         fgcolor ? bgcolor 15 font 2
    es_cons_estab_emp.cod_cenar_ctbl
         at row 02.38 col 25.00 colon-aligned
         view-as fill-in
         size-chars 08.00 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_plano_cta_ctbl
         at row 02.38 col 50.00 colon-aligned 
         /*label "Plano de Contas"
         view-as combo-box
         list-item-pairs "",""
         inner-lines 5*/
         fgcolor ? bgcolor 15 font 2         
    es_cons_estab_emp.dat_inic_valid
         at row 03.38 col 25.00 colon-aligned
         view-as fill-in
         size-chars 12.00 by .88
         fgcolor ? bgcolor 15 font 2
    es_cons_estab_emp.dat_fim_valid
         at row 03.38 col 55.00 colon-aligned
         view-as fill-in
         size-chars 12.00 by .88
         fgcolor ? bgcolor 15 font 2
    br_frm_cta_ctbl_de
         at row 04.71 col 03.00
         help "Cta Ctbl De"
    br_frm_cta_ctbl_para
         at row 10.21 col 03.00
         help "Cta Ctbl Para"
    br_frm_es_cons_cta_ctbl
         at row 04.71 col 48.00
         help "De Para de Cta Ctbl"
    bt_ins_frm
         at row 07.50 col 43.00 font ?
         help "Insere Linha"
    bt_del_frm
         at row 10.50 col 43.00 font ?
         help "Retira Linha"
    bt_ok
         at row 16.75 col 03.00 font ?
         help "OK"
    bt_sav
         at row 16.75 col 14.00 font ?
         help "Salva"
    bt_can
         at row 16.75 col 25.00 font ?
         help "Cancela"
    bt_hel2
         at row 16.75 col 77.43 font ?
         help "Ajuda"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 89.86 by 18.38 default-button bt_ok
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Formaá∆o De Para de Cta Ctbl".
    /* adjust size of objects in this frame */
    assign bt_can:width-chars      in frame f_frm_02_es_cons_cta_ctbl = 10.00
           bt_can:height-chars     in frame f_frm_02_es_cons_cta_ctbl = 01.00
           bt_del_frm:width-chars  in frame f_frm_02_es_cons_cta_ctbl = 04.00
           bt_del_frm:height-chars in frame f_frm_02_es_cons_cta_ctbl = 01.13
           bt_hel2:width-chars     in frame f_frm_02_es_cons_cta_ctbl = 10.00
           bt_hel2:height-chars    in frame f_frm_02_es_cons_cta_ctbl = 01.00
           bt_ins_frm:width-chars  in frame f_frm_02_es_cons_cta_ctbl = 04.00
           bt_ins_frm:height-chars in frame f_frm_02_es_cons_cta_ctbl = 01.13
           bt_ok:width-chars       in frame f_frm_02_es_cons_cta_ctbl = 10.00
           bt_ok:height-chars      in frame f_frm_02_es_cons_cta_ctbl = 01.00
           bt_sav:width-chars      in frame f_frm_02_es_cons_cta_ctbl = 10.00
           bt_sav:height-chars     in frame f_frm_02_es_cons_cta_ctbl = 01.00
           rt_cxcf:width-chars     in frame f_frm_02_es_cons_cta_ctbl = 86.42
           rt_cxcf:height-chars    in frame f_frm_02_es_cons_cta_ctbl = 01.42
           rt_key:width-chars      in frame f_frm_02_es_cons_cta_ctbl = 86.43
           rt_key:height-chars     in frame f_frm_02_es_cons_cta_ctbl = 03.21
           rt_mold:width-chars     in frame f_frm_02_es_cons_cta_ctbl = 86.42
           rt_mold:height-chars    in frame f_frm_02_es_cons_cta_ctbl = 11.42.
&if '{&emsbas_version}' >= '5.06' &then
if OPSYS = 'WIN32':U then do:
assign br_frm_es_cons_cta_ctbl:ALLOW-COLUMN-SEARCHING in frame f_frm_02_es_cons_cta_ctbl = no
       br_frm_es_cons_cta_ctbl:COLUMN-MOVABLE         in frame f_frm_02_es_cons_cta_ctbl = no.
end.
&endif
&if '{&emsbas_version}' >= '5.06' &then
if OPSYS = 'WIN32':U then do:
assign br_frm_cta_ctbl_de  :ALLOW-COLUMN-SEARCHING in frame f_frm_02_es_cons_cta_ctbl = no
       br_frm_cta_ctbl_de  :COLUMN-MOVABLE         in frame f_frm_02_es_cons_cta_ctbl = no
       br_frm_cta_ctbl_para:ALLOW-COLUMN-SEARCHING in frame f_frm_02_es_cons_cta_ctbl = no
       br_frm_cta_ctbl_para:COLUMN-MOVABLE         in frame f_frm_02_es_cons_cta_ctbl = no.
end.
&endif
    /* set private-data for the help system */
    assign es_cons_estab_emp.cod_empresa:private-data    in frame f_frm_02_es_cons_cta_ctbl = "HLP=000004548":U
           es_cons_estab_emp.cod_cenar_ctbl:private-data in frame f_frm_02_es_cons_cta_ctbl = "HLP=000025116":U
           es_cons_estab_emp.dat_inic_valid:private-data in frame f_frm_02_es_cons_cta_ctbl = "HLP=000000000":U
           es_cons_estab_emp.dat_fim_valid:private-data  in frame f_frm_02_es_cons_cta_ctbl = "HLP=000000000":U
           v_cod_plano_cta_ctbl:private-data             in frame f_frm_02_es_cons_cta_ctbl = "HLP=000000000":U
           br_frm_cta_ctbl_de:private-data               in frame f_frm_02_es_cons_cta_ctbl = "HLP=000006597":U
           br_frm_cta_ctbl_para:private-data             in frame f_frm_02_es_cons_cta_ctbl = "HLP=000006597":U
           br_frm_es_cons_cta_ctbl:private-data          in frame f_frm_02_es_cons_cta_ctbl = "HLP=000006597":U
           bt_ins_frm:private-data                       in frame f_frm_02_es_cons_cta_ctbl = "HLP=000009353":U
           bt_del_frm:private-data                       in frame f_frm_02_es_cons_cta_ctbl = "HLP=000009347":U
           bt_ok:private-data                            in frame f_frm_02_es_cons_cta_ctbl = "HLP=000010721":U
           bt_sav:private-data                           in frame f_frm_02_es_cons_cta_ctbl = "HLP=000011048":U
           bt_can:private-data                           in frame f_frm_02_es_cons_cta_ctbl = "HLP=000011050":U
           bt_hel2:private-data                          in frame f_frm_02_es_cons_cta_ctbl = "HLP=000011326":U
           frame f_frm_02_es_cons_cta_ctbl:private-data                                     = "HLP=000006597":U.

{include/i_fclfrm.i f_frm_02_es_cons_cta_ctbl }
/*************************** Frame Definition End ***************************/

/*********************** User Interface Trigger Begin ***********************/

ON VALUE-CHANGED OF v_cod_plano_cta_ctbl IN FRAME f_frm_02_es_cons_cta_ctbl
DO:
    run pi_open_frm_es_cons_cta_ctbl.

END. /* ON VALUE-CHANGED OF v_cod_plano_cta_ctbl IN FRAME f_frm_02_es_cons_cta_ctbl */

ON CTRL-CURSOR-LEFT OF br_frm_es_cons_cta_ctbl IN FRAME f_frm_02_es_cons_cta_ctbl
DO:

    if  bt_del_frm:sensitive in frame f_frm_02_es_cons_cta_ctbl = yes
    then do:
        apply "choose" to bt_del_frm in frame f_frm_02_es_cons_cta_ctbl.
    end.

END. /* ON CTRL-CURSOR-LEFT OF br_frm_es_cons_cta_ctbl IN FRAME f_frm_02_es_cons_cta_ctbl */

ON VALUE-CHANGED OF br_frm_es_cons_cta_ctbl IN FRAME f_frm_02_es_cons_cta_ctbl
DO:

    if  avail es_cons_cta_ctbl
    then do:
        assign v_rec_table = recid(es_cons_cta_ctbl).
        find b_es_cons_cta_ctbl_mod where recid(b_es_cons_cta_ctbl_mod) = v_rec_table exclusive-lock no-error.
    end.

END. /* ON VALUE-CHANGED OF br_frm_es_cons_cta_ctbl IN FRAME f_frm_02_es_cons_cta_ctbl */

ON CTRL-CURSOR-RIGHT OF br_frm_cta_ctbl_de IN FRAME f_frm_02_es_cons_cta_ctbl
DO:

    if  bt_ins_frm:sensitive in frame f_frm_02_es_cons_cta_ctbl = yes
    then do:
        apply "choose" to bt_ins_frm in frame f_frm_02_es_cons_cta_ctbl.
    end.
END. /* ON CTRL-CURSOR-RIGHT OF br_frm_cta_ctbl IN FRAME f_frm_02_es_cons_cta_ctbl */

ON CTRL-CURSOR-RIGHT OF br_frm_cta_ctbl_para IN FRAME f_frm_02_es_cons_cta_ctbl
DO:

    if  bt_ins_frm:sensitive in frame f_frm_02_es_cons_cta_ctbl = yes
    then do:
        apply "choose" to bt_ins_frm in frame f_frm_02_es_cons_cta_ctbl.
    end.
END. /* ON CTRL-CURSOR-RIGHT OF br_frm_cta_ctbl IN FRAME f_frm_02_es_cons_cta_ctbl */

ON CHOOSE OF bt_del_frm IN FRAME f_frm_02_es_cons_cta_ctbl
DO:

    retire:
    do v_num_row_a = 1 to browse br_frm_es_cons_cta_ctbl:num-selected-rows:
        assign v_log_method = browse br_frm_es_cons_cta_ctbl:fetch-selected-row(v_num_row_a).

        assign v_rec_table  = recid(es_cons_cta_ctbl).
        find es_cons_cta_ctbl where recid(es_cons_cta_ctbl) = v_rec_table exclusive-lock no-error.
        delete es_cons_cta_ctbl.
    end /* do retire */.
    assign v_log_exist = yes.
    run pi_open_frm_es_cons_cta_ctbl /*pi_open_frm_es_cons_cta_ctbl*/.
END. /* ON CHOOSE OF bt_del_frm IN FRAME f_frm_02_es_cons_cta_ctbl */

ON CHOOSE OF bt_hel2 IN FRAME f_frm_02_es_cons_cta_ctbl
DO:

    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.

    /* End_Include: i_context_help_frame */

END. /* ON CHOOSE OF bt_hel2 IN FRAME f_frm_02_es_cons_cta_ctbl */

ON CHOOSE OF bt_ins_frm IN FRAME f_frm_02_es_cons_cta_ctbl
DO:

    table_src_a:
    do v_num_row_a = 1 to browse br_frm_cta_ctbl_de:num-selected-rows:
        assign v_log_method = browse br_frm_cta_ctbl_para:fetch-selected-row(1).
        assign v_log_method = browse br_frm_cta_ctbl_de:fetch-selected-row(v_num_row_a).

        find first es_cons_cta_ctbl no-lock where es_cons_cta_ctbl.num_id_estab_emp   = es_cons_estab_emp.num_id_estab_emp
               and es_cons_cta_ctbl.cod_empresa        = es_cons_estab_emp.cod_empresa
               and es_cons_cta_ctbl.cod_plano_cta_ctbl = input frame f_frm_02_es_cons_cta_ctbl v_cod_plano_cta_ctbl
               and es_cons_cta_ctbl.cod_cta_ctbl       = tt_cta_ctbl_de.cod_cta_ctbl no-error.
        if  not avail es_cons_cta_ctbl
        then do:
            create es_cons_cta_ctbl.
            assign es_cons_cta_ctbl.num_id_estab_emp        = es_cons_estab_emp.num_id_estab_emp
                   es_cons_cta_ctbl.cod_empresa             = es_cons_estab_emp.cod_empresa
                   es_cons_cta_ctbl.cod_plano_cta_ctbl      = input frame f_frm_02_es_cons_cta_ctbl v_cod_plano_cta_ctbl
                   es_cons_cta_ctbl.cod_cta_ctbl            = tt_cta_ctbl_de.cod_cta_ctbl
                   es_cons_cta_ctbl.cod_plano_cta_ctbl_para = es_cons_estab_emp.cod_plano_cta_ctbl
                   es_cons_cta_ctbl.cod_cta_ctbl_para       = tt_cta_ctbl_para.cod_cta_ctbl.

            assign v_log_method = browse br_frm_cta_ctbl_de:deselect-selected-row(v_num_row_a)
                   v_num_row_a  = v_num_row_a - 1.
        end.
    end /* do table_src_a */.
    assign v_log_exist = yes.
    run pi_open_frm_es_cons_cta_ctbl /*pi_open_frm_es_cons_cta_ctbl*/.

END. /* ON CHOOSE OF bt_ins_frm IN FRAME f_frm_02_es_cons_cta_ctbl */

ON CHOOSE OF bt_ok IN FRAME f_frm_02_es_cons_cta_ctbl
DO:

    assign v_log_repeat = no.

END. /* ON CHOOSE OF bt_ok IN FRAME f_frm_02_es_cons_cta_ctbl */

ON CHOOSE OF bt_sav IN FRAME f_frm_02_es_cons_cta_ctbl
DO:

    assign v_log_repeat = yes.

END. /* ON CHOOSE OF bt_sav IN FRAME f_frm_02_es_cons_cta_ctbl */

/************************ User Interface Trigger End ************************/

/**************************** Frame Trigger Begin ***************************/

ON ENDKEY OF FRAME f_frm_02_es_cons_cta_ctbl
DO:

END. /* ON ENDKEY OF FRAME f_frm_02_es_cons_cta_ctbl */

ON HELP OF FRAME f_frm_02_es_cons_cta_ctbl ANYWHERE
DO:
    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */
END. /* ON HELP OF FRAME f_frm_02_es_cons_cta_ctbl */

ON RIGHT-MOUSE-DOWN OF FRAME f_frm_02_es_cons_cta_ctbl ANYWHERE
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

        if  valid-handle(self:popup-menu) then
            return no-apply.

        assign v_wgh_frame = self:frame.

        if  (v_wgh_frame:type <> "DIALOG-BOX" ) and (v_wgh_frame:frame <> ?) then
               assign v_wgh_frame = v_wgh_frame:frame.

        assign v_nom_title_aux    = v_wgh_frame:title
               v_wgh_frame:title  = self:help.
    end.
    /* End_Include: i_right_mouse_down_dialog_box */
END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_frm_02_es_cons_cta_ctbl */

ON RIGHT-MOUSE-UP OF FRAME f_frm_02_es_cons_cta_ctbl ANYWHERE
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

        if  valid-handle(self:popup-menu) then
            return no-apply.

        assign v_wgh_frame = self:frame.
        if  (v_wgh_frame:type <> "DIALOG-BOX" ) and (v_wgh_frame:frame <> ?) then
               assign v_wgh_frame     = v_wgh_frame:frame.

        assign v_wgh_frame:title  = v_nom_title_aux.
    end.

    /* End_Include: i_right_mouse_up_dialog_box */
END. /* ON RIGHT-MOUSE-UP OF FRAME f_frm_02_es_cons_cta_ctbl */

ON WINDOW-CLOSE OF FRAME f_frm_02_es_cons_cta_ctbl
DO:
    apply "end-error" to self.
END. /* ON WINDOW-CLOSE OF FRAME f_frm_02_es_cons_cta_ctbl */

/***************************** Frame Trigger End ****************************/

/**************************** Menu Trigger Begin ****************************/

ON CHOOSE OF MENU-ITEM mi_conteudo IN MENU m_help
DO:
    apply "choose" to bt_hel2 in frame f_frm_02_es_cons_cta_ctbl.
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
        format "x(8)":U
        no-undo.
    def var v_nom_prog_ext
        as character
        format "x(8)":U
        label "Nome Externo"
        no-undo.

    /************************** Variable Definition End *************************/

    assign v_nom_prog     = substr(frame f_frm_02_es_cons_cta_ctbl:title, 1, max(1, length(frame f_frm_02_es_cons_cta_ctbl:title) - 10)).
    if  v_nom_prog = ? then
        assign v_nom_prog = "".

    assign v_nom_prog = v_nom_prog + chr(10) + "frm_es_cons_cta_ctbl":U.

    assign v_nom_prog_ext = "esp/esfgl071mb.p":U
           v_cod_release  = trim(" 1.00.00.000":U).

    {include/sobre5.i}
END. /* ON CHOOSE OF MENU-ITEM mi_sobre IN MENU m_help */

/***************************** Menu Trigger End *****************************/

/****************************** Main Code Begin *****************************/

/* Begin_Include: i_version_extract */
/* {include/i-ctrlrp5.i frm_es_cons_cta_ctbl} */

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
    run pi_version_extract ('frm_es_cons_cta_ctbl':U, 'esp/esfgl071mb.p':U, '1.00.00.000':U, 'pro':U).

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
    run prgtec/men/men901za.py (Input 'frm_es_cons_cta_ctbl') /*prg_fnc_verify_security*/.
if  return-value = "2014"
then do:
    /* Programa a ser executado n∆o Ç um programa v†lido Datasul ! */
    run pi_messages (input "show", input 2014,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'frm_es_cons_cta_ctbl')) /*msg_2014*/.
    return.
end.
if  return-value = "2012"
then do:
    /* Usu†rio sem permiss∆o para acessar o programa. */
    run pi_messages (input "show", input 2012,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'frm_es_cons_cta_ctbl')) /*msg_2012*/.
    return.
end.
/* End_Include: i_verify_security */

/* Begin_Include: i_log_exec_prog_dtsul_ini */
assign v_rec_log = ?.

if can-find(prog_dtsul
       where prog_dtsul.cod_prog_dtsul = 'frm_es_cons_cta_ctbl' 
         and prog_dtsul.log_gera_log_exec = yes) then do transaction:
    create log_exec_prog_dtsul.
    assign log_exec_prog_dtsul.cod_prog_dtsul           = 'frm_es_cons_cta_ctbl'
           log_exec_prog_dtsul.cod_usuario              = v_cod_usuar_corren
           log_exec_prog_dtsul.dat_inic_exec_prog_dtsul = today
           log_exec_prog_dtsul.hra_inic_exec_prog_dtsul = replace(string(time,"hh:mm:ss"),":":U,"":U).
    assign v_rec_log = recid(log_exec_prog_dtsul).
    release log_exec_prog_dtsul no-error.
end.

/* End_Include: i_log_exec_prog_dtsul_ini */

/* ix_p00_frm_es_cons_cta_ctbl */

/* redefiniá‰es do frame */

/* Begin_Include: i_std_dialog_box */
/* tratamento do titulo e vers∆o */
assign frame f_frm_02_es_cons_cta_ctbl:title = frame f_frm_02_es_cons_cta_ctbl:title
                            + chr(32)
                            + chr(40)
                            + trim(" 1.00.00.000":U)
                            + chr(41).
/* menu pop-up de ajuda e sobre */
assign menu m_help:popup-only = yes
       bt_hel2:popup-menu in frame f_frm_02_es_cons_cta_ctbl = menu m_help:handle.

/* End_Include: i_std_dialog_box */

{include/title5.i f_frm_02_es_cons_cta_ctbl FRAME}

pause 0 before-hide.
view frame f_frm_02_es_cons_cta_ctbl.

assign v_log_exist  = no
       v_log_repeat = yes.

find es_cons_estab_emp where recid(es_cons_estab_emp) = v_rec_es_cons_estab_emp no-lock no-error.
if  not avail es_cons_estab_emp then
    find first es_cons_estab_emp no-lock no-error.
if  not avail es_cons_estab_emp then do:
    message "N∆o existem ocorràncias na tabela." /*l_no_record*/ 
           view-as alert-box warning buttons ok.
    return.
end.
else
    assign v_rec_es_cons_estab_emp = recid(es_cons_estab_emp).

for each plano_cta_ctbl no-lock
   where plano_cta_ctbl.dat_inic_valid <= today
     and plano_cta_ctbl.dat_fim_valid  >= today:
    assign v_lista_plano = v_lista_plano + (if  v_lista_plano <> "" then "," else "") +
                           plano_cta_ctbl.cod_plano_cta_ctbl + " - " + plano_cta_ctbl.des_tit_ctbl +
                           "," + plano_cta_ctbl.cod_plano_cta_ctbl.
end.
assign v_cod_plano_cta_ctbl:list-item-pairs in frame f_frm_02_es_cons_cta_ctbl = v_lista_plano.

/* ix_p05_frm_es_cons_cta_ctbl */

main_block:
repeat while v_log_repeat = yes on endkey undo main_block, leave main_block
                                on error undo main_block, leave main_block transaction:
    /* ix_p10_frm_es_cons_cta_ctbl */
    enable br_frm_es_cons_cta_ctbl
           br_frm_cta_ctbl_de
           br_frm_cta_ctbl_para
           v_cod_plano_cta_ctbl
           bt_ok
           bt_sav
           bt_can
           bt_hel2
           bt_ins_frm
           bt_del_frm
           with frame f_frm_02_es_cons_cta_ctbl.

    display es_cons_estab_emp.cod_empresa
            es_cons_estab_emp.cod_cenar_ctbl
            es_cons_estab_emp.dat_inic_valid
            es_cons_estab_emp.dat_fim_valid
            v_cod_plano_cta_ctbl
            with frame f_frm_02_es_cons_cta_ctbl.

    /* ix_p15_frm_es_cons_cta_ctbl */

    run pi_open_frm_es_cons_cta_ctbl /*pi_open_frm_es_cons_cta_ctbl*/.

    /* ix_p20_frm_es_cons_cta_ctbl */
    wait-for go of frame f_frm_02_es_cons_cta_ctbl.

    /* ix_p30_frm_es_cons_cta_ctbl */

    assign v_log_exist = yes.
end /* repeat main_block */.

hide frame f_frm_02_es_cons_cta_ctbl.

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
** Procedure Interna.....: pi_atualiza_tt
** Descricao.............: pi_atualiza_tt
*****************************************************************************/
PROCEDURE pi_atualiza_tt:
    empty temp-table tt_cta_ctbl_de.
    empty temp-table tt_cta_ctbl_para.

    for each cta_ctbl no-lock
       where cta_ctbl.cod_plano_cta_ctbl = input frame f_frm_02_es_cons_cta_ctbl v_cod_plano_cta_ctbl
         AND cta_ctbl.ind_espec_cta_ctbl = 'Anal°tica' :
        find first es_cons_cta_ctbl no-lock
             where es_cons_cta_ctbl.num_id_estab_emp   = es_cons_estab_emp.num_id_estab_emp
               and es_cons_cta_ctbl.cod_empresa        = es_cons_estab_emp.cod_empresa
               and es_cons_cta_ctbl.cod_plano_cta_ctbl = input frame f_frm_02_es_cons_cta_ctbl v_cod_plano_cta_ctbl
               and es_cons_cta_ctbl.cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl no-error.
        if  not avail es_cons_cta_ctbl then do:
            create tt_cta_ctbl_de.
            buffer-copy cta_ctbl to tt_cta_ctbl_de.
        end.
    end.

    FOR EACH cta_ctbl NO-LOCK
        WHERE cta_ctbl.cod_plano_cta_ctbl      = es_cons_estab_emp.cod_plano_cta_ctbl
          AND cta_ctbl.ind_espec_cta_ctbl      = 'Anal°tica':
        create tt_cta_ctbl_para.
        buffer-copy cta_ctbl to tt_cta_ctbl_para.        
    END.

END PROCEDURE. /* pi_atualiza_tt */
/*****************************************************************************
** Procedure Interna.....: pi_open_frm_es_cons_cta_ctbl
** Descricao.............: pi_open_frm_es_cons_cta_ctbl
*****************************************************************************/
PROCEDURE pi_open_frm_es_cons_cta_ctbl:
    run pi_atualiza_tt.

   open query qr_frm_cta_ctbl_de for
         each tt_cta_ctbl_de no-lock.

    if  v_log_exist = no then do:
       open query qr_frm_cta_ctbl_para for
             each tt_Cta_ctbl_para no-lock.
    end.

    open query qr_frm_es_cons_cta_ctbl for
          each es_cons_cta_ctbl exclusive-lock
         where es_cons_cta_ctbl.num_id_estab_emp = es_cons_estab_emp.num_id_estab_emp.

    apply "value-changed" to br_frm_es_cons_cta_ctbl in frame f_frm_02_es_cons_cta_ctbl.
END PROCEDURE. /* pi_open_frm_es_cons_cta_ctbl */
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
/***********************  End of frm_es_cons_cta_ctbl ***********************/

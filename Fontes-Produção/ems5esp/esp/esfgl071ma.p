/*****************************************************************************
** Programa..............: frm_es_cons_estab
** Descricao.............: Formaá∆o(Filho) De Para Estab X Empresa
** Versao................:  1.00.00.000
** Procedimento..........: man_es_cons_estab_emp
** Nome Externo..........: esp/esfgl071ma.p
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
{include/i-license-manager.i frm_es_cons_estab FGL}
&ENDIF

/******************************* Private-Data *******************************/
assign this-procedure:private-data = "HLP=15":U.
/*************************************  *************************************/

&if "{&emsuni_dbinst}" <> "yes" &then
run pi_messages (input "show", input 5884, input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9", "EMSUNI")) /*msg_5884*/.
&elseif "{&emsuni_version}" < "1.00" &then
run pi_messages (input "show", input 5009,
                 input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9",
                                    "FRM_ES_CONS_ESTAB","~~EMSUNI", "~~{~&emsuni_version}", "~~1.00")) /*msg_5009*/.
&else

/************************** Buffer Definition Begin *************************/
def temp-table tt_estab_de no-undo like estabelecimento.

def buffer b_es_cons_estab_delete
    for es_cons_estab.
def buffer b_es_cons_estab_mod
    for es_cons_estab.
def buffer b_estab_para
    for estabelecimento.

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

/************************** Variable Definition End *************************/

/*************************** Menu Definition Begin **************************/

.

def menu      m_help                menubar
    menu-item mi_conteudo           label "&Conte£do"
    menu-item mi_sobre              label "&Sobre".

/**************************** Menu Definition End ***************************/

/************************** Query Definition Begin **************************/

def query qr_frm_es_cons_estab
    for es_cons_estab
    scrolling.
def query qr_frm_estabelecimento_de
    for tt_estab_de
    scrolling.
def query qr_frm_estabelecimento_para
    for b_estab_para
    scrolling.

/*************************** Query Definition End ***************************/

/************************** Browse Definition Begin *************************/

def browse br_frm_es_cons_estab query qr_frm_es_cons_estab display 
    es_cons_estab.cod_empresa
    width-chars 05.00
        column-label "Emp"
    es_cons_estab.cod_estab
    width-chars 10.00
        column-label "Estab De"
    es_cons_estab.cod_estab_para
    width-chars 10.00
        column-label "Estab Para"
    with separators multiple 
         size 39.00 by 11.00
         font 1
         bgcolor 15
         title "De Para de Estab".

def browse br_frm_estabelecimento_de query qr_frm_estabelecimento_de display 
    tt_estab_de.cod_empresa
    width-chars 05.00
        column-label "Emp"
    tt_estab_de.cod_estab
    width-chars 05.29
    tt_estab_de.nom_pessoa
    width-chars 40.00
    with separators multiple 
         size 39.00 by 05.50
         font 1
         bgcolor 15
         title "Estab De".

def browse br_frm_estabelecimento_para query qr_frm_estabelecimento_para display 
    b_estab_para.cod_empresa
    width-chars 05.00
        column-label "Emp"
    b_estab_para.cod_estab
    width-chars 05.29
    b_estab_para.nom_pessoa
    width-chars 40.00
    with separators single 
         size 39.00 by 05.50
         font 1
         bgcolor 15
         title "Estab Para".

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

def frame f_frm_02_es_cons_estab
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
    br_frm_estabelecimento_de
         at row 04.71 col 03.00
         help "Estab De"
    br_frm_estabelecimento_para
         at row 10.21 col 03.00
         help "Estab Para"
    br_frm_es_cons_estab
         at row 04.71 col 48.00
         help "De Para de Estab"
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
         title "Formaá∆o De Para de Estab".
    /* adjust size of objects in this frame */
    assign bt_can:width-chars      in frame f_frm_02_es_cons_estab = 10.00
           bt_can:height-chars     in frame f_frm_02_es_cons_estab = 01.00
           bt_del_frm:width-chars  in frame f_frm_02_es_cons_estab = 04.00
           bt_del_frm:height-chars in frame f_frm_02_es_cons_estab = 01.13
           bt_hel2:width-chars     in frame f_frm_02_es_cons_estab = 10.00
           bt_hel2:height-chars    in frame f_frm_02_es_cons_estab = 01.00
           bt_ins_frm:width-chars  in frame f_frm_02_es_cons_estab = 04.00
           bt_ins_frm:height-chars in frame f_frm_02_es_cons_estab = 01.13
           bt_ok:width-chars       in frame f_frm_02_es_cons_estab = 10.00
           bt_ok:height-chars      in frame f_frm_02_es_cons_estab = 01.00
           bt_sav:width-chars      in frame f_frm_02_es_cons_estab = 10.00
           bt_sav:height-chars     in frame f_frm_02_es_cons_estab = 01.00
           rt_cxcf:width-chars     in frame f_frm_02_es_cons_estab = 86.42
           rt_cxcf:height-chars    in frame f_frm_02_es_cons_estab = 01.42
           rt_key:width-chars      in frame f_frm_02_es_cons_estab = 86.43
           rt_key:height-chars     in frame f_frm_02_es_cons_estab = 03.21
           rt_mold:width-chars     in frame f_frm_02_es_cons_estab = 86.42
           rt_mold:height-chars    in frame f_frm_02_es_cons_estab = 11.42.
&if '{&emsbas_version}' >= '5.06' &then
if OPSYS = 'WIN32':U then do:
assign br_frm_es_cons_estab:ALLOW-COLUMN-SEARCHING in frame f_frm_02_es_cons_estab = no
       br_frm_es_cons_estab:COLUMN-MOVABLE         in frame f_frm_02_es_cons_estab = no.
end.
&endif
&if '{&emsbas_version}' >= '5.06' &then
if OPSYS = 'WIN32':U then do:
assign br_frm_estabelecimento_de  :ALLOW-COLUMN-SEARCHING in frame f_frm_02_es_cons_estab = no
       br_frm_estabelecimento_de  :COLUMN-MOVABLE         in frame f_frm_02_es_cons_estab = no
       br_frm_estabelecimento_para:ALLOW-COLUMN-SEARCHING in frame f_frm_02_es_cons_estab = no
       br_frm_estabelecimento_para:COLUMN-MOVABLE         in frame f_frm_02_es_cons_estab = no.
end.
&endif
    /* set private-data for the help system */
    assign es_cons_estab_emp.cod_empresa:private-data    in frame f_frm_02_es_cons_estab = "HLP=000004548":U
           es_cons_estab_emp.cod_cenar_ctbl:private-data in frame f_frm_02_es_cons_estab = "HLP=000025116":U
           es_cons_estab_emp.dat_inic_valid:private-data in frame f_frm_02_es_cons_estab = "HLP=000000000":U
           es_cons_estab_emp.dat_fim_valid:private-data  in frame f_frm_02_es_cons_estab = "HLP=000000000":U
           br_frm_estabelecimento_de:private-data        in frame f_frm_02_es_cons_estab = "HLP=000006597":U
           br_frm_estabelecimento_para:private-data      in frame f_frm_02_es_cons_estab = "HLP=000006597":U
           br_frm_es_cons_estab:private-data             in frame f_frm_02_es_cons_estab = "HLP=000006597":U
           bt_ins_frm:private-data                       in frame f_frm_02_es_cons_estab = "HLP=000009353":U
           bt_del_frm:private-data                       in frame f_frm_02_es_cons_estab = "HLP=000009347":U
           bt_ok:private-data                            in frame f_frm_02_es_cons_estab = "HLP=000010721":U
           bt_sav:private-data                           in frame f_frm_02_es_cons_estab = "HLP=000011048":U
           bt_can:private-data                           in frame f_frm_02_es_cons_estab = "HLP=000011050":U
           bt_hel2:private-data                          in frame f_frm_02_es_cons_estab = "HLP=000011326":U
           frame f_frm_02_es_cons_estab:private-data                                     = "HLP=000006597":U.

{include/i_fclfrm.i f_frm_02_es_cons_estab }
/*************************** Frame Definition End ***************************/

/*********************** User Interface Trigger Begin ***********************/

ON CTRL-CURSOR-LEFT OF br_frm_es_cons_estab IN FRAME f_frm_02_es_cons_estab
DO:

    if  bt_del_frm:sensitive in frame f_frm_02_es_cons_estab = yes
    then do:
        apply "choose" to bt_del_frm in frame f_frm_02_es_cons_estab.
    end.

END. /* ON CTRL-CURSOR-LEFT OF br_frm_es_cons_estab IN FRAME f_frm_02_es_cons_estab */

ON VALUE-CHANGED OF br_frm_es_cons_estab IN FRAME f_frm_02_es_cons_estab
DO:

    if  avail es_cons_estab
    then do:
        assign v_rec_table = recid(es_cons_estab).
        find b_es_cons_estab_mod where recid(b_es_cons_estab_mod) = v_rec_table exclusive-lock no-error.
    end.

END. /* ON VALUE-CHANGED OF br_frm_es_cons_estab IN FRAME f_frm_02_es_cons_estab */

ON CTRL-CURSOR-RIGHT OF br_frm_estabelecimento_de IN FRAME f_frm_02_es_cons_estab
DO:

    if  bt_ins_frm:sensitive in frame f_frm_02_es_cons_estab = yes
    then do:
        apply "choose" to bt_ins_frm in frame f_frm_02_es_cons_estab.
    end.
END. /* ON CTRL-CURSOR-RIGHT OF br_frm_estabelecimento IN FRAME f_frm_02_es_cons_estab */

ON CTRL-CURSOR-RIGHT OF br_frm_estabelecimento_para IN FRAME f_frm_02_es_cons_estab
DO:

    if  bt_ins_frm:sensitive in frame f_frm_02_es_cons_estab = yes
    then do:
        apply "choose" to bt_ins_frm in frame f_frm_02_es_cons_estab.
    end.
END. /* ON CTRL-CURSOR-RIGHT OF br_frm_estabelecimento IN FRAME f_frm_02_es_cons_estab */

ON CHOOSE OF bt_del_frm IN FRAME f_frm_02_es_cons_estab
DO:

    retire:
    do v_num_row_a = 1 to browse br_frm_es_cons_estab:num-selected-rows:
        assign v_log_method = browse br_frm_es_cons_estab:fetch-selected-row(v_num_row_a).

        assign v_rec_table  = recid(es_cons_estab).
        find es_cons_estab where recid(es_cons_estab) = v_rec_table exclusive-lock no-error.
        delete es_cons_estab.
    end /* do retire */.
    assign v_log_exist = yes.
    run pi_open_frm_es_cons_estab /*pi_open_frm_es_cons_estab*/.
END. /* ON CHOOSE OF bt_del_frm IN FRAME f_frm_02_es_cons_estab */

ON CHOOSE OF bt_hel2 IN FRAME f_frm_02_es_cons_estab
DO:

    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.

    /* End_Include: i_context_help_frame */

END. /* ON CHOOSE OF bt_hel2 IN FRAME f_frm_02_es_cons_estab */

ON CHOOSE OF bt_ins_frm IN FRAME f_frm_02_es_cons_estab
DO:

    table_src_a:
    do v_num_row_a = 1 to browse br_frm_estabelecimento_de:num-selected-rows:
        assign v_log_method = browse br_frm_estabelecimento_para:fetch-selected-row(1).
        assign v_log_method = browse br_frm_estabelecimento_de:fetch-selected-row(v_num_row_a).

        find first es_cons_estab no-lock
             where es_cons_estab.num_id_estab_emp = es_cons_estab_emp.num_id_estab_emp
               and es_cons_estab.cod_empresa      = es_cons_estab_emp.cod_empresa
               and es_cons_estab.cod_estab        = tt_estab_de.cod_estab no-error.
        if  not avail es_cons_estab
        then do:
            create es_cons_estab.
            assign es_cons_estab.num_id_estab_emp = es_cons_estab_emp.num_id_estab_emp
                   es_cons_estab.cod_empresa      = es_cons_estab_emp.cod_empresa
                   es_cons_estab.cod_estab        = tt_estab_de.cod_estab
                   es_cons_estab.cod_estab_para   = b_estab_para.cod_estab.

            assign v_log_method = browse br_frm_estabelecimento_de:deselect-selected-row(v_num_row_a)
                   v_num_row_a  = v_num_row_a - 1.
        end.
    end /* do table_src_a */.
    assign v_log_exist = yes.
    run pi_open_frm_es_cons_estab /*pi_open_frm_es_cons_estab*/.

END. /* ON CHOOSE OF bt_ins_frm IN FRAME f_frm_02_es_cons_estab */

ON CHOOSE OF bt_ok IN FRAME f_frm_02_es_cons_estab
DO:

    assign v_log_repeat = no.

END. /* ON CHOOSE OF bt_ok IN FRAME f_frm_02_es_cons_estab */

ON CHOOSE OF bt_sav IN FRAME f_frm_02_es_cons_estab
DO:

    assign v_log_repeat = yes.

END. /* ON CHOOSE OF bt_sav IN FRAME f_frm_02_es_cons_estab */

/************************ User Interface Trigger End ************************/

/**************************** Frame Trigger Begin ***************************/

ON ENDKEY OF FRAME f_frm_02_es_cons_estab
DO:

END. /* ON ENDKEY OF FRAME f_frm_02_es_cons_estab */

ON HELP OF FRAME f_frm_02_es_cons_estab ANYWHERE
DO:
    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */
END. /* ON HELP OF FRAME f_frm_02_es_cons_estab */

ON RIGHT-MOUSE-DOWN OF FRAME f_frm_02_es_cons_estab ANYWHERE
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
END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_frm_02_es_cons_estab */

ON RIGHT-MOUSE-UP OF FRAME f_frm_02_es_cons_estab ANYWHERE
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
               assign v_wgh_frame = v_wgh_frame:frame.

        assign v_wgh_frame:title = v_nom_title_aux.
    end.

    /* End_Include: i_right_mouse_up_dialog_box */
END. /* ON RIGHT-MOUSE-UP OF FRAME f_frm_02_es_cons_estab */

ON WINDOW-CLOSE OF FRAME f_frm_02_es_cons_estab
DO:
    apply "end-error" to self.
END. /* ON WINDOW-CLOSE OF FRAME f_frm_02_es_cons_estab */

/***************************** Frame Trigger End ****************************/

/**************************** Menu Trigger Begin ****************************/

ON CHOOSE OF MENU-ITEM mi_conteudo IN MENU m_help
DO:
    apply "choose" to bt_hel2 in frame f_frm_02_es_cons_estab.
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

    assign v_nom_prog     = substr(frame f_frm_02_es_cons_estab:title, 1, max(1, length(frame f_frm_02_es_cons_estab:title) - 10)).
    if  v_nom_prog = ? then
        assign v_nom_prog = "".

    assign v_nom_prog = v_nom_prog + chr(10) + "frm_es_cons_estab":U.

    assign v_nom_prog_ext = "esp/esfgl071ma.p":U
           v_cod_release  = trim(" 1.00.00.000":U).

    {include/sobre5.i}
END. /* ON CHOOSE OF MENU-ITEM mi_sobre IN MENU m_help */

/***************************** Menu Trigger End *****************************/

/****************************** Main Code Begin *****************************/

/* Begin_Include: i_version_extract */
/* {include/i-ctrlrp5.i frm_es_cons_estab} */

def new global shared var v_cod_arq
    as char
    format 'x(60)'
    no-undo.
def new global shared var v_cod_tip_prog
    as character
    format 'x(08)':U
    no-undo.

def stream s-arq.

if  v_cod_arq <> '' and v_cod_arq <> ? then
    run pi_version_extract ('frm_es_cons_estab':U, 'esp/esfgl071ma.p':U, '1.00.00.000':U, 'pro':U).

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
    run prgtec/men/men901za.py (Input 'frm_es_cons_estab') /*prg_fnc_verify_security*/.
if  return-value = "2014"
then do:
    /* Programa a ser executado n∆o Ç um programa v†lido Datasul ! */
    run pi_messages (input "show", input 2014,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'frm_es_cons_estab')) /*msg_2014*/.
    return.
end.
if  return-value = "2012"
then do:
    /* Usu†rio sem permiss∆o para acessar o programa. */
    run pi_messages (input "show", input 2012,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'frm_es_cons_estab')) /*msg_2012*/.
    return.
end.
/* End_Include: i_verify_security */

/* Begin_Include: i_log_exec_prog_dtsul_ini */
assign v_rec_log = ?.

if can-find(prog_dtsul
       where prog_dtsul.cod_prog_dtsul = 'frm_es_cons_estab' 
         and prog_dtsul.log_gera_log_exec = yes) then do transaction:
    create log_exec_prog_dtsul.
    assign log_exec_prog_dtsul.cod_prog_dtsul           = 'frm_es_cons_estab'
           log_exec_prog_dtsul.cod_usuario              = v_cod_usuar_corren
           log_exec_prog_dtsul.dat_inic_exec_prog_dtsul = today
           log_exec_prog_dtsul.hra_inic_exec_prog_dtsul = replace(string(time,"hh:mm:ss"),":":U,"":U).
    assign v_rec_log = recid(log_exec_prog_dtsul).
    release log_exec_prog_dtsul no-error.
end.

/* End_Include: i_log_exec_prog_dtsul_ini */

/* ix_p00_frm_es_cons_estab */

/* redefiniá‰es do frame */

/* Begin_Include: i_std_dialog_box */
/* tratamento do titulo e vers∆o */
assign frame f_frm_02_es_cons_estab:title = frame f_frm_02_es_cons_estab:title
                            + chr(32)
                            + chr(40)
                            + trim(" 1.00.00.000":U)
                            + chr(41).
/* menu pop-up de ajuda e sobre */
assign menu m_help:popup-only = yes
       bt_hel2:popup-menu in frame f_frm_02_es_cons_estab = menu m_help:handle.

/* End_Include: i_std_dialog_box */

{include/title5.i f_frm_02_es_cons_estab FRAME}

pause 0 before-hide.
view frame f_frm_02_es_cons_estab.

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

/* ix_p05_frm_es_cons_estab */

main_block:
repeat while v_log_repeat = yes on endkey undo main_block, leave main_block
                                on error undo main_block, leave main_block transaction:
    /* ix_p10_frm_es_cons_estab */
    enable br_frm_es_cons_estab
           br_frm_estabelecimento_de
           br_frm_estabelecimento_para
           bt_ok
           bt_sav
           bt_can
           bt_hel2
           bt_ins_frm
           bt_del_frm
           with frame f_frm_02_es_cons_estab.

    display es_cons_estab_emp.cod_empresa
            es_cons_estab_emp.cod_cenar_ctbl
            es_cons_estab_emp.dat_inic_valid
            es_cons_estab_emp.dat_fim_valid
            with frame f_frm_02_es_cons_estab.

    /* ix_p15_frm_es_cons_estab */

    run pi_open_frm_es_cons_estab /*pi_open_frm_es_cons_estab*/.

    /* ix_p20_frm_es_cons_estab */
    wait-for go of frame f_frm_02_es_cons_estab.

    /* ix_p30_frm_es_cons_estab */

    assign v_log_exist = yes.
end /* repeat main_block */.

hide frame f_frm_02_es_cons_estab.

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
    empty temp-table tt_estab_de.

    for each estabelecimento no-lock
       where estabelecimento.cod_empresa <> es_cons_estab_emp.cod_empresa:
        find first es_cons_estab no-lock
             where es_cons_estab.num_id_estab_emp = es_cons_estab_emp.num_id_estab_emp
               and es_cons_estab.cod_empresa      = es_cons_estab_emp.cod_empresa
               and es_cons_estab.cod_estab        = estabelecimento.cod_estab no-error.
        if  not avail es_cons_estab then do:
            create tt_estab_de.
            buffer-copy estabelecimento to tt_estab_de.
        end.
    end.
END PROCEDURE. /* pi_atualiza_tt */
/*****************************************************************************
** Procedure Interna.....: pi_open_frm_es_cons_estab
** Descricao.............: pi_open_frm_es_cons_estab
*****************************************************************************/
PROCEDURE pi_open_frm_es_cons_estab:
    run pi_atualiza_tt.

   open query qr_frm_estabelecimento_de for
         each tt_estab_de no-lock.

    if  v_log_exist = no then do:
       open query qr_frm_estabelecimento_para for
             each b_estab_para no-lock
            where b_estab_para.cod_empresa = es_cons_estab_emp.cod_empresa.
    end.

    open query qr_frm_es_cons_estab for
          each es_cons_estab exclusive-lock
         where es_cons_estab.num_id_estab_emp = es_cons_estab_emp.num_id_estab_emp.

    apply "value-changed" to br_frm_es_cons_estab in frame f_frm_02_es_cons_estab.
END PROCEDURE. /* pi_open_frm_es_cons_estab */
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
/***********************  End of frm_es_cons_estab ***********************/

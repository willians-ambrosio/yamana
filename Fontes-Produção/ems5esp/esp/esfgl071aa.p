/*****************************************************************************
** Programa..............: bas_es_cons_estab_emp
** Descricao.............: Base Pai De Para Estab X Empresa
** Versao................:  1.00.00.000
** Procedimento..........: man_es_cons_estab_emp
** Nome Externo..........: esp/esfgl071aa.p
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
{include/i-license-manager.i bas_es_cons_estab_emp FGL}
&ENDIF

/******************************* Private-Data *******************************/
assign this-procedure:private-data = "HLP=15":U.
/*************************************  *************************************/

&if "{&emsuni_dbinst}" <> "yes" &then
run pi_messages (input "show", input 5884, input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9", "EMSUNI")) /*msg_5884*/.
&elseif "{&emsuni_version}" < "1.00" &then
run pi_messages (input "show", input 5009,
                 input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9",
                                    "BAS_ES_CONS_ESTAB_EMP","~~EMSUNI", "~~{~&emsuni_version}", "~~1.00")) /*msg_5009*/.
&else

/************************** Window Definition Begin *************************/

def var wh_w_program
    as widget-handle
    no-undo.

IF session:window-system <> "TTY" THEN
DO:
    create window wh_w_program
    assign
         row                  = 01.00
         col                  = 01.00
         height-chars         = 01.00
         width-chars          = 01.00
         min-width-chars      = 01.00
         min-height-chars     = 01.00
         max-width-chars      = 01.00
         max-height-chars     = 01.00
         virtual-width-chars  = 300.00
         virtual-height-chars = 200.00
         title                = "Program"
&if '{&emsbas_version}' >= '5.06' &then
         resize               = no
&else
         resize               = yes
&endif
         scroll-bars          = no
         status-area          = yes
         status-area-font     = ?
         message-area         = no
         message-area-font    = ?
         fgcolor              = ?
         bgcolor              = ?.
END.

{include/i_fclwin.i wh_w_program }
/*************************** Window Definition End **************************/

/************************** Buffer Definition Begin *************************/

def buffer b_es_cons_estab_emp_enter
    for es_cons_estab_emp.

/*************************** Buffer Definition End **************************/

/************************* Variable Definition Begin ************************/

def new global shared var v_cod_modul_dtsul_corren
    as character
    format "x(03)":U
    label "M¢dulo Corrente"
    column-label "M¢dulo Corrente"
    no-undo.
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

&if '{&emsbas_version}' >= '5.06' &then
def temp-table tt_maximizacao no-undo
    field hdl-widget             as widget-handle
    field tipo-widget            as character 
    field row-original           as decimal
    field col-original           as decimal
    field width-original         as decimal
    field height-original        as decimal
    field log-posiciona-row      as logical
    field log-posiciona-col      as logical
    field log-calcula-width      as logical
    field log-calcula-height     as logical
    field log-button-right       as logical
    field frame-width-original   as decimal
    field frame-height-original  as decimal
    field window-width-original  as decimal
    field window-height-original as decimal.
&endif
/************************** Variable Definition End *************************/

/*************************** Menu Definition Begin **************************/

def sub-menu  mi_table
    menu-item mi_fir             label "Primeiro"  accelerator "ALT-HOME"
    menu-item mi_pre             label "Anterior"  accelerator "ALT-CURSOR-LEFT"
    menu-item mi_nex             label "Pr¢ximo"   accelerator "ALT-CURSOR-RIGHT"
    menu-item mi_las             label "Èltimo"    accelerator "ALT-END"
    RULE
    menu-item mi_add             label "Inclui"    accelerator "ALT-INS"
    menu-item mi_mod             label "Modifica"  accelerator "ALT-ENTER"
    menu-item mi_era             label "Elimina"   accelerator "ALT-DEL"
    RULE
    menu-item mi_sea             label "Pesquisa"  accelerator "ALT-Z"
    RULE
    menu-item mi_exi             label "Sa°da".

def sub-menu  mi_hel
    menu-item mi_contents        label "Conte£do"
    RULE
    menu-item mi_sobre           label "Sobre".

def menu      m_03_des_anot_tab  menubar
    sub-menu  mi_table           label "Tabela"
    sub-menu  mi_hel             label "Ajuda".

/**************************** Menu Definition End ***************************/

/************************ Rectangle Definition Begin ************************/

def rectangle rt_key
    size 1 by 1
    edge-pixels 2.
def rectangle rt_mold
    size 1 by 1
    edge-pixels 2.
def rectangle rt_rgf
    size 1 by 1
    edge-pixels 2.

/************************* Rectangle Definition End *************************/

/************************** Button Definition Begin *************************/

def button bt_add1
    label "Inc"
    tooltip "Inclui"
    image-up file "image/im-add"
    image-insensitive file "image/ii-add"
    size 1 by 1.
def button bt_era1
    label "Eli"
    tooltip "Elimina"
    image-up file "image/im-era1"
    image-insensitive file "image/ii-era1"
    size 1 by 1.
def button bt_exi
    label "Sa°da"
    tooltip "Sa°da"
    image-up file "image/im-exi"
    image-insensitive file "image/ii-exi"
    size 1 by 1.
def button bt_fir
    label "<<"
    tooltip "Primeira Ocorrància da Tabela"
    image-up file "image/im-fir"
    image-insensitive file "image/ii-fir"
    size 1 by 1.
def button bt_hel1
    label " ?"
    tooltip "Ajuda"
    image-up file "image/im-hel"
    image-insensitive file "image/ii-hel"
    size 1 by 1.
def button bt_las
    label ">>"
    tooltip "Èltima Ocorrància da Tabela"
    image-up file "image/im-las"
    image-insensitive file "image/ii-las"
    size 1 by 1.
def button bt_mod1
    label "Mod"
    tooltip "Modifica"
    image-up file "image/im-mod"
    image-insensitive file "image/ii-mod"
    size 1 by 1.
def button bt_mov1
    label "Mov"
    tooltip "Movimentos"
    image-up file "image/im-mov"
    image-insensitive file "image/ii-mov.bmp"
    size 1 by 1.
def button bt_mov2
    label "Mov"
    tooltip "Movimentos"
    image-up file "image/im-mov"
    image-insensitive file "image/ii-mov.bmp"
    size 1 by 1.
def button bt_mov3
    label "Mov"
    tooltip "Movimentos"
    image-up file "image/im-mov"
    image-insensitive file "image/ii-mov.bmp"
    size 1 by 1.
def button bt_nex1
    label ">"
    tooltip "Pr¢xima Ocorrància da Tabela"
    image-up file "image/im-nex1"
    image-insensitive file "image/ii-nex1"
    size 1 by 1.
def button bt_pre1
    label "<"
    tooltip "Ocorrància Anterior da Tabela"
    image-up file "image/im-pre1"
    image-insensitive file "image/ii-pre1"
    size 1 by 1.
def button bt_sea1
    label "Psq"
    tooltip "Pesquisa"
    image-up file "image/im-sea1"
    image-insensitive file "image/ii-sea1"
    size 1 by 1.
/****************************** Function Button *****************************/
def button bt_ent_47009
    label "Loc"
    tooltip "Entra"
    image-up file "image/im-enter"
    image-insensitive file "image/ii-enter"
    size 4 by .88.

/*************************** Button Definition End **************************/

/************************** Frame Definition Begin **************************/

def frame f_bap_03_es_cons_estab_emp
    rt_mold
         at row 05.85 col 02.00
    rt_rgf
         at row 01.00 col 01.00 bgcolor 7 
    rt_key
         at row 02.50 col 02.00
    bt_fir
         at row 01.08 col 01.14 font ?
         help "Primeira Ocorrància da Tabela"
    bt_pre1
         at row 01.08 col 05.14 font ?
         help "Ocorrància Anterior da Tabela"
    bt_nex1
         at row 01.08 col 09.14 font ?
         help "Pr¢xima Ocorrància da Tabela"
    bt_las
         at row 01.08 col 13.14 font ?
         help "Èltima Ocorrància da Tabela"
    bt_add1
         at row 01.08 col 18.14 font ?
         help "Inclui"
    bt_mod1
         at row 01.08 col 22.14 font ?
         help "Modifica"
    bt_era1
         at row 01.08 col 26.14 font ?
         help "Elimina"
    bt_sea1
         at row 01.08 col 40.14 font ?
         help "Pesquisa"
    bt_exi
         at row 01.08 col 70.86 font ?
         help "Sa°da"
    bt_hel1
         at row 01.08 col 74.86 font ?
         help "Ajuda"
    es_cons_estab_emp.cod_empresa
         at row 02.67 col 21.00 colon-aligned
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_mov1
         at row 02.67 col 27.50 font ?
         help "Movimentos"
    empresa.nom_razao_social
         at row 02.67 col 32.00 no-label
         view-as fill-in
         size-chars 41.14 by .88
         fgcolor ? bgcolor 15 font 2
    es_cons_estab_emp.cod_cenar_ctbl
         at row 03.67 col 21.00 colon-aligned
         view-as fill-in
         size-chars 12.14 by .88
         fgcolor ? bgcolor 15 font 2
    es_cons_estab_emp.dat_inic_valid
         at row 04.67 col 21.00 colon-aligned
         view-as fill-in
         size-chars 12.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_ent_47009
         at row 04.67 col 35.14
    es_cons_estab_emp.dat_fim_valid
         at row 04.67 col 50.00 colon-aligned
         view-as fill-in
         size-chars 12.14 by .88
         fgcolor ? bgcolor 15 font 2
    es_cons_estab_emp.cod_plano_cta_ctbl
         at row 06.00 col 21.00 colon-aligned
         view-as fill-in
         size-chars 16.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_mov2
         at row 06.00 col 39.50 font ?
         help "Movimentos"
    es_cons_estab_emp.cod_plano_ccusto
         at row 07.00 col 21.00 colon-aligned
         view-as fill-in
         size-chars 16.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_mov3
         at row 07.00 col 39.50 font ?
         help "Movimentos"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 78.29 by 8.20
         at row 01.00 col 07.57
         font 1 fgcolor ? bgcolor 8
         title "Manutená∆o De Para Estab X Empresa".
    /* adjust size of objects in this frame */
    assign bt_add1:width-chars  in frame f_bap_03_es_cons_estab_emp = 04.00
           bt_add1:height-chars in frame f_bap_03_es_cons_estab_emp = 01.13
           bt_era1:width-chars  in frame f_bap_03_es_cons_estab_emp = 04.00
           bt_era1:height-chars in frame f_bap_03_es_cons_estab_emp = 01.13
           bt_exi:width-chars   in frame f_bap_03_es_cons_estab_emp = 04.00
           bt_exi:height-chars  in frame f_bap_03_es_cons_estab_emp = 01.13
           bt_fir:width-chars   in frame f_bap_03_es_cons_estab_emp = 04.00
           bt_fir:height-chars  in frame f_bap_03_es_cons_estab_emp = 01.13
           bt_hel1:width-chars  in frame f_bap_03_es_cons_estab_emp = 04.00
           bt_hel1:height-chars in frame f_bap_03_es_cons_estab_emp = 01.13
           bt_las:width-chars   in frame f_bap_03_es_cons_estab_emp = 04.00
           bt_las:height-chars  in frame f_bap_03_es_cons_estab_emp = 01.13
           bt_mod1:width-chars  in frame f_bap_03_es_cons_estab_emp = 04.00
           bt_mod1:height-chars in frame f_bap_03_es_cons_estab_emp = 01.13
           bt_mov1:width-chars  in frame f_bap_03_es_cons_estab_emp = 04.00
           bt_mov1:height-chars in frame f_bap_03_es_cons_estab_emp = 01.00
           bt_mov2:width-chars  in frame f_bap_03_es_cons_estab_emp = 04.00
           bt_mov2:height-chars in frame f_bap_03_es_cons_estab_emp = 01.00
           bt_mov3:width-chars  in frame f_bap_03_es_cons_estab_emp = 04.00
           bt_mov3:height-chars in frame f_bap_03_es_cons_estab_emp = 01.00
           bt_nex1:width-chars  in frame f_bap_03_es_cons_estab_emp = 04.00
           bt_nex1:height-chars in frame f_bap_03_es_cons_estab_emp = 01.13
           bt_pre1:width-chars  in frame f_bap_03_es_cons_estab_emp = 04.00
           bt_pre1:height-chars in frame f_bap_03_es_cons_estab_emp = 01.13
           bt_sea1:width-chars  in frame f_bap_03_es_cons_estab_emp = 04.00
           bt_sea1:height-chars in frame f_bap_03_es_cons_estab_emp = 01.13
           rt_key:width-chars   in frame f_bap_03_es_cons_estab_emp = 76.00
           rt_key:height-chars  in frame f_bap_03_es_cons_estab_emp = 03.25
           rt_mold:width-chars  in frame f_bap_03_es_cons_estab_emp = 76.00
           rt_mold:height-chars in frame f_bap_03_es_cons_estab_emp = 02.25
           rt_rgf:width-chars   in frame f_bap_03_es_cons_estab_emp = 78.00
           rt_rgf:height-chars  in frame f_bap_03_es_cons_estab_emp = 01.29.
    /* set private-data for the help system */
    assign bt_fir:private-data                               in frame f_bap_03_es_cons_estab_emp = "HLP=000004657":U
           bt_pre1:private-data                              in frame f_bap_03_es_cons_estab_emp = "HLP=000010790":U
           bt_nex1:private-data                              in frame f_bap_03_es_cons_estab_emp = "HLP=000010787":U
           bt_las:private-data                               in frame f_bap_03_es_cons_estab_emp = "HLP=000004658":U
           bt_add1:private-data                              in frame f_bap_03_es_cons_estab_emp = "HLP=000010793":U
           bt_mod1:private-data                              in frame f_bap_03_es_cons_estab_emp = "HLP=000010797":U
           bt_era1:private-data                              in frame f_bap_03_es_cons_estab_emp = "HLP=000011152":U
           bt_mov1:private-data                              in frame f_bap_03_es_cons_estab_emp = "HLP=000004657":U
           bt_mov2:private-data                              in frame f_bap_03_es_cons_estab_emp = "HLP=000004657":U
           bt_mov3:private-data                              in frame f_bap_03_es_cons_estab_emp = "HLP=000004657":U
           bt_sea1:private-data                              in frame f_bap_03_es_cons_estab_emp = "HLP=000010810":U
           bt_exi:private-data                               in frame f_bap_03_es_cons_estab_emp = "HLP=000004665":U
           bt_hel1:private-data                              in frame f_bap_03_es_cons_estab_emp = "HLP=000004666":U
           bt_ent_47009:private-data                         in frame f_bap_03_es_cons_estab_emp = "HLP=000009422":U
           es_cons_estab_emp.cod_empresa       :private-data in frame f_bap_03_es_cons_estab_emp = "HLP=000005341":U
           empresa.nom_razao_social            :private-data in frame f_bap_03_es_cons_estab_emp = "HLP=000009760":U
           es_cons_estab_emp.cod_cenar_ctbl    :private-data in frame f_bap_03_es_cons_estab_emp = "HLP=000004548":U
           es_cons_estab_emp.dat_inic_valid    :private-data in frame f_bap_03_es_cons_estab_emp = "HLP=000009978":U
           es_cons_estab_emp.dat_fim_valid     :private-data in frame f_bap_03_es_cons_estab_emp = "HLP=000022223":U
           es_cons_estab_emp.cod_plano_cta_ctbl:private-data in frame f_bap_03_es_cons_estab_emp = "HLP=000025116":U
           es_cons_estab_emp.cod_plano_ccusto  :private-data in frame f_bap_03_es_cons_estab_emp = "HLP=000009923":U
           frame f_bap_03_es_cons_estab_emp:private-data                                         = "HLP=000004735":U.
    /* enable function buttons */
    assign bt_ent_47009:sensitive in frame f_bap_03_es_cons_estab_emp = yes.

{include/i_fclfrm.i f_bap_03_es_cons_estab_emp }
/*************************** Frame Definition End ***************************/
&if '{&emsbas_version}' >= '5.06' &then
ON WINDOW-MAXIMIZED OF wh_w_program
DO:
    def var v_whd_widget as widget-handle no-undo.
    assign frame f_bap_03_es_cons_estab_emp:width-chars  = wh_w_program:width-chars
           frame f_bap_03_es_cons_estab_emp:height-chars = wh_w_program:height-chars no-error.

    for each tt_maximizacao:
        assign v_whd_widget = tt_maximizacao.hdl-widget.

        if tt_maximizacao.log-posiciona-row = yes then do:
            assign v_whd_widget:row = wh_w_program:height - (tt_maximizacao.window-height-original - tt_maximizacao.row-original).
        end.
        if tt_maximizacao.log-calcula-width = yes then do:
            assign v_whd_widget:width = wh_w_program:width - ( tt_maximizacao.window-width-original - tt_maximizacao.width-original ).
        end.
        if tt_maximizacao.log-calcula-height = yes then do:
            assign v_whd_widget:height = wh_w_program:height - ( tt_maximizacao.window-height-original - tt_maximizacao.height-original ).
        end.
        if tt_maximizacao.log-posiciona-col = yes then do:
            assign v_whd_widget:col = wh_w_program:width - (tt_maximizacao.window-width-original - tt_maximizacao.col-original).
        end.
        if tt_maximizacao.tipo-widget = 'button'
        and tt_maximizacao.log-button-right = yes then do:
            assign v_whd_widget:col = wh_w_program:width - (tt_maximizacao.window-width-original - tt_maximizacao.col-original).
        end.
    end.
end. /* ON WINDOW-MAXIMIZED OF wh_w_program */
&endif
&if '{&emsbas_version}' >= '5.06' &then
ON WINDOW-RESTORED OF wh_w_program
DO:
    def var v_whd_widget as widget-handle no-undo.

    for each tt_maximizacao:
        assign v_whd_widget = tt_maximizacao.hdl-widget.

        if can-query(v_whd_widget,'row') then
            assign v_whd_widget:row    = tt_maximizacao.row-original    no-error.

        if can-query(v_whd_widget,'col') then
            assign v_whd_widget:col    = tt_maximizacao.col-original    no-error.

        if can-query(v_whd_widget,'width') then
            assign v_whd_widget:width  = tt_maximizacao.width-original  no-error.

        if can-query(v_whd_widget,'height') then
            assign v_whd_widget:height = tt_maximizacao.height-original no-error.
    end.
end. /* ON WINDOW-RESTORED OF wh_w_program */
&endif

/*********************** User Interface Trigger Begin ***********************/

ON CHOOSE OF bt_add1 IN FRAME f_bap_03_es_cons_estab_emp
DO:

    assign v_rec_es_cons_estab_emp = v_rec_table.
    if  search("esp/esfgl071ca.r") = ? and search("esp/esfgl071ca.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut†vel n∆o foi encontrado: esp/esfgl071ca.p".
        else do:
            message "Programa execut†vel n∆o foi encontrado: esp/esfgl071ca.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run esp/esfgl071ca.p /*prg_add_es_cons_estab_emp*/.

    if  v_rec_es_cons_estab_emp <> ? then do:
        assign v_rec_table = v_rec_es_cons_estab_emp.
        run pi_disp_fields /*pi_disp_fields*/.
    end.
    else
        assign v_rec_es_cons_estab_emp = v_rec_table.
END. /* ON CHOOSE OF bt_add1 IN FRAME f_bap_03_es_cons_estab_emp */

ON CHOOSE OF bt_era1 IN FRAME f_bap_03_es_cons_estab_emp
DO:

    assign v_rec_es_cons_estab_emp = v_rec_table.
    if  avail es_cons_estab_emp then do:
        if  search("esp/esfgl071ga.r") = ? and search("esp/esfgl071ga.p") = ? then do:
            if  v_cod_dwb_user begins 'es_' then
                return "Programa execut†vel n∆o foi encontrado: esp/esfgl071ga.p".
            else do:
                message "Programa execut†vel n∆o foi encontrado: esp/esfgl071ga.p"
                       view-as alert-box error buttons ok.
                return.
            end.
        end.
        else
            run esp/esfgl071ga.p /*prg_era_es_cons_estab_emp*/.

        assign v_rec_table = v_rec_es_cons_estab_emp.
        if  v_rec_table <> ? then do:
            run pi_disp_fields /*pi_disp_fields*/.
        end.
        else
            apply "choose" to bt_nex1 in frame f_bap_03_es_cons_estab_emp.
    end.
END. /* ON CHOOSE OF bt_era1 IN FRAME f_bap_03_es_cons_estab_emp */

ON CHOOSE OF bt_exi IN FRAME f_bap_03_es_cons_estab_emp
DO:

    run pi_close_program /*pi_close_program*/.

END. /* ON CHOOSE OF bt_exi IN FRAME f_bap_03_es_cons_estab_emp */

ON CHOOSE OF bt_fir IN FRAME f_bap_03_es_cons_estab_emp
DO:

    find first es_cons_estab_emp no-lock no-error.
    if  avail es_cons_estab_emp then do:
        assign v_rec_table = recid(es_cons_estab_emp).
        run pi_disp_fields /*pi_disp_fields*/.
    end.
    else do:
        assign empresa.nom_razao_social            :screen-value in frame f_bap_03_es_cons_estab_emp = ""
               es_cons_estab_emp.cod_empresa       :screen-value in frame f_bap_03_es_cons_estab_emp = ""
               es_cons_estab_emp.cod_cenar_ctbl    :screen-value in frame f_bap_03_es_cons_estab_emp = ""
               es_cons_estab_emp.dat_inic_valid    :screen-value in frame f_bap_03_es_cons_estab_emp = ""
               es_cons_estab_emp.dat_fim_valid     :screen-value in frame f_bap_03_es_cons_estab_emp = ""
               es_cons_estab_emp.cod_plano_ccusto  :screen-value in frame f_bap_03_es_cons_estab_emp = ""
               es_cons_estab_emp.cod_plano_cta_ctbl:screen-value in frame f_bap_03_es_cons_estab_emp = "".
        message "N∆o existem ocorràncias na tabela." 
               view-as alert-box warning buttons ok.
        assign v_rec_table = ?.
    end.
END. /* ON CHOOSE OF bt_fir IN FRAME f_bap_03_es_cons_estab_emp */

ON CHOOSE OF bt_hel1 IN FRAME f_bap_03_es_cons_estab_emp
DO:

    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help_frame */

END. /* ON CHOOSE OF bt_hel1 IN FRAME f_bap_03_es_cons_estab_emp */

ON CHOOSE OF bt_las IN FRAME f_bap_03_es_cons_estab_emp
DO:

    find last es_cons_estab_emp no-lock no-error.
    if  avail es_cons_estab_emp then do:
        assign v_rec_table = recid(es_cons_estab_emp).
        run pi_disp_fields /*pi_disp_fields*/.
    end.
    else do:
        assign es_cons_estab_emp.cod_empresa       :screen-value in frame f_bap_03_es_cons_estab_emp = ""
               empresa.nom_razao_social            :screen-value in frame f_bap_03_es_cons_estab_emp = ""
               es_cons_estab_emp.cod_cenar_ctbl    :screen-value in frame f_bap_03_es_cons_estab_emp = ""
               es_cons_estab_emp.dat_inic_valid    :screen-value in frame f_bap_03_es_cons_estab_emp = ""
               es_cons_estab_emp.dat_fim_valid     :screen-value in frame f_bap_03_es_cons_estab_emp = ""
               es_cons_estab_emp.cod_plano_ccusto  :screen-value in frame f_bap_03_es_cons_estab_emp = ""
               es_cons_estab_emp.cod_plano_cta_ctbl:screen-value in frame f_bap_03_es_cons_estab_emp = "".
        message "N∆o existem ocorràncias na tabela." 
               view-as alert-box warning buttons ok.
        assign v_rec_table = ?.
    end.
END. /* ON CHOOSE OF bt_las IN FRAME f_bap_03_es_cons_estab_emp */

ON CHOOSE OF bt_mod1 IN FRAME f_bap_03_es_cons_estab_emp
DO:

    assign v_rec_es_cons_estab_emp = v_rec_table.
    if  avail es_cons_estab_emp then do:
        if  search("esp/esfgl071ea.r") = ? and search("esp/esfgl071ea.p") = ? then do:
            if  v_cod_dwb_user begins 'es_' then
                return "Programa execut†vel n∆o foi encontrado: esp/esfgl071ea.p".
            else do:
                message "Programa execut†vel n∆o foi encontrado: esp/esfgl071ea.p"
                       view-as alert-box error buttons ok.
                return.
            end.
        end.
        else
            run esp/esfgl071ea.p /*prg_mod_es_cons_estab_emp*/.
    end.

    if  v_rec_es_cons_estab_emp <> ? then
        assign v_rec_table = v_rec_es_cons_estab_emp.
    else
        assign v_rec_es_cons_estab_emp = v_rec_table.

    run pi_disp_fields /*pi_disp_fields*/.
END. /* ON CHOOSE OF bt_mod1 IN FRAME f_bap_03_es_cons_estab_emp */

ON CHOOSE OF bt_mov1 IN FRAME f_bap_03_es_cons_estab_emp
DO:

    assign v_rec_es_cons_estab_emp = v_rec_table.
    if  avail es_cons_estab_emp then do:
        if  search("esp/esfgl071ma.r") = ? and search("esp/esfgl071ma.p") = ? then do:
            if  v_cod_dwb_user begins 'es_' then
                return "Programa execut†vel n∆o foi encontrado: esp/esfgl071ma.p".
            else do:
                message "Programa execut†vel n∆o foi encontrado: esp/esfgl071ma.p"
                       view-as alert-box error buttons ok.
                return.
            end.
        end.
        else
            run esp/esfgl071ma.p /*prg_frm_estab_unid_negoc*/.

        if  v_rec_es_cons_estab_emp <> ? then do:
            assign v_rec_table = v_rec_es_cons_estab_emp.
            run pi_disp_fields /*pi_disp_fields*/.
        end.
        else
            assign v_rec_es_cons_estab_emp = v_rec_table.
    end.
END. /* ON CHOOSE OF bt_mov1 IN FRAME f_bap_03_es_cons_estab_emp */

ON CHOOSE OF bt_mov2 IN FRAME f_bap_03_es_cons_estab_emp
DO:

    assign v_rec_es_cons_estab_emp = v_rec_table.
    if  avail es_cons_estab_emp then do:
        if  search("esp/esfgl071mb.r") = ? and search("esp/esfgl071mb.p") = ? then do:
            if  v_cod_dwb_user begins 'es_' then
                return "Programa execut†vel n∆o foi encontrado: esp/esfgl071mb.p".
            else do:
                message "Programa execut†vel n∆o foi encontrado: esp/esfgl071mb.p"
                       view-as alert-box error buttons ok.
                return.
            end.
        end.
        else
            run esp/esfgl071mb.p /*prg_frm_estab_unid_negoc*/.

        if  v_rec_es_cons_estab_emp <> ? then do:
            assign v_rec_table = v_rec_es_cons_estab_emp.
            run pi_disp_fields /*pi_disp_fields*/.
        end.
        else
            assign v_rec_es_cons_estab_emp = v_rec_table.
    end.
END. /* ON CHOOSE OF bt_mov2 IN FRAME f_bap_03_es_cons_estab_emp */

ON CHOOSE OF bt_mov3 IN FRAME f_bap_03_es_cons_estab_emp
DO:

    assign v_rec_es_cons_estab_emp = v_rec_table.
    if  avail es_cons_estab_emp then do:
        if  search("esp/esfgl071mc.r") = ? and search("esp/esfgl071mc.p") = ? then do:
            if  v_cod_dwb_user begins 'es_' then
                return "Programa execut†vel n∆o foi encontrado: esp/esfgl071mc.p".
            else do:
                message "Programa execut†vel n∆o foi encontrado: esp/esfgl071mc.p"
                       view-as alert-box error buttons ok.
                return.
            end.
        end.
        else
            run esp/esfgl071mc.p /*prg_frm_estab_unid_negoc*/.

        if  v_rec_es_cons_estab_emp <> ? then do:
            assign v_rec_table = v_rec_es_cons_estab_emp.
            run pi_disp_fields /*pi_disp_fields*/.
        end.
        else
            assign v_rec_es_cons_estab_emp = v_rec_table.
    end.
END. /* ON CHOOSE OF bt_mov3 IN FRAME f_bap_03_es_cons_estab_emp */

ON CHOOSE OF bt_nex1 IN FRAME f_bap_03_es_cons_estab_emp
DO:

    find next es_cons_estab_emp no-lock no-error.
    if  avail es_cons_estab_emp then do:
        assign v_rec_table = recid(es_cons_estab_emp).
        run pi_disp_fields /*pi_disp_fields*/.
    end.
    else do:
        apply "choose" to bt_las in frame f_bap_03_es_cons_estab_emp.
        if  avail es_cons_estab_emp then do:
            message "Èltima ocorrància da tabela." 
                   view-as alert-box warning buttons ok.
        end.
    end.
END. /* ON CHOOSE OF bt_nex1 IN FRAME f_bap_03_es_cons_estab_emp */

ON CHOOSE OF bt_pre1 IN FRAME f_bap_03_es_cons_estab_emp
DO:
    find prev es_cons_estab_emp no-lock no-error.
    if  avail es_cons_estab_emp then do:
        assign v_rec_table = recid(es_cons_estab_emp).
        run pi_disp_fields /*pi_disp_fields*/.
    end.
    else do:
        apply "choose" to bt_fir in frame f_bap_03_es_cons_estab_emp.
        if  avail es_cons_estab_emp then do:
            message "Primeira ocorrància da tabela." /*l_first*/ 
                   view-as alert-box warning buttons ok.
        end.
    end.
END. /* ON CHOOSE OF bt_pre1 IN FRAME f_bap_03_es_cons_estab_emp */

ON CHOOSE OF bt_sea1 IN FRAME f_bap_03_es_cons_estab_emp
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
        run pi_disp_fields /*pi_disp_fields*/.
    end.
    else
        assign v_rec_es_cons_estab_emp = v_rec_table.
END. /* ON CHOOSE OF bt_sea1 IN FRAME f_bap_03_es_cons_estab_emp */

/************************ User Interface Trigger End ************************/

/************************** Function Trigger Begin **************************/

ON  CHOOSE OF bt_ent_47009 IN FRAME f_bap_03_es_cons_estab_emp
OR ENTER OF es_cons_estab_emp.dat_inic_valid IN FRAME f_bap_03_es_cons_estab_emp DO:

    find b_es_cons_estab_emp_enter no-lock
         where b_es_cons_estab_emp_enter.cod_empresa    = input frame f_bap_03_es_cons_estab_emp es_cons_estab_emp.cod_empresa
           and b_es_cons_estab_emp_enter.cod_cenar_ctbl = input frame f_bap_03_es_cons_estab_emp es_cons_estab_emp.cod_cenar_ctbl
           and b_es_cons_estab_emp_enter.dat_inic_valid = input frame f_bap_03_es_cons_estab_emp es_cons_estab_emp.dat_inic_valid no-error.
    if  avail b_es_cons_estab_emp_enter then do:
        assign v_rec_table = recid(b_es_cons_estab_emp_enter).
        run pi_disp_fields /*pi_disp_fields*/.
    end.
    else do:
        message substitute("&1 inexistente." ,"De Para Estab X Empresa")
               view-as alert-box warning buttons ok.
        find es_cons_estab_emp where recid(es_cons_estab_emp) = v_rec_table no-lock no-error.
    end.
end. /* ON  CHOOSE OF bt_ent_47009 IN FRAME f_bap_03_es_cons_estab_emp */

/*************************** Function Trigger End ***************************/

/**************************** Frame Trigger Begin ***************************/

ON ENDKEY OF FRAME f_bap_03_es_cons_estab_emp
DO:

END. /* ON ENDKEY OF FRAME f_bap_03_es_cons_estab_emp */

ON END-ERROR OF FRAME f_bap_03_es_cons_estab_emp
DO:

    run pi_close_program /*pi_close_program*/.
END. /* ON END-ERROR OF FRAME f_bap_03_es_cons_estab_emp */

ON HELP OF FRAME f_bap_03_es_cons_estab_emp ANYWHERE
DO:

    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */

END. /* ON HELP OF FRAME f_bap_03_es_cons_estab_emp */

ON RIGHT-MOUSE-DOWN OF FRAME f_bap_03_es_cons_estab_emp ANYWHERE
DO:

    /************************* Variable Definition Begin ************************/

    def var v_wgh_frame
        as widget-handle
        format ">>>>>>9":U
        no-undo.

    /************************** Variable Definition End *************************/

    /* Begin_Include: i_right_mouse_down_window */
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

        assign v_nom_title_aux       = current-window:title
               current-window:title  = self:help.
    end.
    /* End_Include: i_right_mouse_down_window */

END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_bap_03_es_cons_estab_emp */

ON RIGHT-MOUSE-UP OF FRAME f_bap_03_es_cons_estab_emp ANYWHERE
DO:

    /************************* Variable Definition Begin ************************/

    def var v_wgh_frame
        as widget-handle
        format ">>>>>>9":U
        no-undo.

    /************************** Variable Definition End *************************/

    /* Begin_Include: i_right_mouse_up_window */
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

        assign current-window:title  = v_nom_title_aux.
    end.

    /* End_Include: i_right_mouse_up_window */

END. /* ON RIGHT-MOUSE-UP OF FRAME f_bap_03_es_cons_estab_emp */

/***************************** Frame Trigger End ****************************/

/*************************** Window Trigger Begin ***************************/

IF session:window-system <> "TTY" THEN
DO:

    ON ENTRY OF wh_w_program
    DO:
    &if '{&emsbas_version}' >= '5.06' &then
        def var v_whd_field_group   as widget-handle no-undo.
        def var v_whd_widget        as widget-handle no-undo.
        def buffer b_tt_maximizacao for tt_maximizacao.
        find first tt_maximizacao no-error.
        if not avail tt_maximizacao then do:
            assign v_whd_field_group = frame f_bap_03_es_cons_estab_emp:first-child.
            repeat while valid-handle(v_whd_field_group):
                assign v_whd_widget = v_whd_field_group:first-child.
                repeat while valid-handle(v_whd_widget):
                    create tt_maximizacao.
                    if can-query(v_whd_widget,'handle') then
                        assign tt_maximizacao.hdl-widget            = v_whd_widget:handle no-error.
                    if can-query(v_whd_widget,'type') then
                        assign tt_maximizacao.tipo-widget           = v_whd_widget:type no-error.
                    if can-query(v_whd_widget,'row') then
                        assign tt_maximizacao.row-original          = v_whd_widget:row no-error.
                    if can-query(v_whd_widget,'col') then
                        assign tt_maximizacao.col-original          = v_whd_widget:col no-error.
                    if can-query(v_whd_widget,'width') then
                        assign tt_maximizacao.width-original        = v_whd_widget:width no-error.
                    if can-query(v_whd_widget,'height') then
                        assign tt_maximizacao.height-original       = v_whd_widget:height no-error.
                    assign tt_maximizacao.frame-width-original   = frame f_bap_03_es_cons_estab_emp:width.
                    assign tt_maximizacao.frame-height-original  = frame f_bap_03_es_cons_estab_emp:height.
                    assign tt_maximizacao.window-width-original  = wh_w_program:width.
                    assign tt_maximizacao.window-height-original = wh_w_program:height.
                    assign tt_maximizacao.log-posiciona-row  = no.
                    assign tt_maximizacao.log-posiciona-col  = no.
                    assign tt_maximizacao.log-calcula-width  = no.
                    assign tt_maximizacao.log-calcula-height = no.
                    assign tt_maximizacao.log-button-right   = no.
                    if can-query(v_whd_widget,'flat-button') then do:
                        if v_whd_widget:flat-button = yes then do:
                            assign tt_maximizacao.log-posiciona-col  = no.
                            if v_whd_widget:name = 'bt_exi' or
                               v_whd_widget:name = 'bt_hel1' then do:
                                assign tt_maximizacao.log-button-right   = yes.
                            end.
                        end.
                    end.
                    if can-query(v_whd_widget,'type') then do:
                        if v_whd_widget:type = 'browse' then
                            assign tt_maximizacao.log-calcula-height = yes.
                    end.
                    assign v_whd_widget = v_whd_widget:next-sibling.
                end.
                assign v_whd_field_group = v_whd_field_group:next-sibling.
            end.
        end.
        for each tt_maximizacao
           where tt_maximizacao.tipo-widget = 'browse'
              by tt_maximizacao.row-original:
            find first b_tt_maximizacao
                 where b_tt_maximizacao.tipo-widget = 'browse'
                   and b_tt_maximizacao.hdl-widget = tt_maximizacao.hdl-widget
                no-error.
            if avail b_tt_maximizacao then do:
                leave.
            end.
        end.
        if avail b_tt_maximizacao then do:
            for each tt_maximizacao
                where tt_maximizacao.row-original >=  b_tt_maximizacao.row-original +
                                                      b_tt_maximizacao.height-original - 1:
                assign tt_maximizacao.log-calcula-height = no.
                assign tt_maximizacao.log-posiciona-row  = yes.
                assign tt_maximizacao.log-posiciona-col  = no.
            end.
        end.
        for each b_tt_maximizacao
            where b_tt_maximizacao.tipo-widget = 'browse':
            assign b_tt_maximizacao.log-calcula-width = yes.
            for each tt_maximizacao
                where tt_maximizacao.row-original + tt_maximizacao.height-original >=
                      b_tt_maximizacao.row-original + b_tt_maximizacao.height-original
                  and tt_maximizacao.row-original < b_tt_maximizacao.row-original + b_tt_maximizacao.height-original
                  and tt_maximizacao.tipo-widget = 'rectangle'
                  and b_tt_maximizacao.log-calcula-height = yes:
                assign tt_maximizacao.log-calcula-height = yes.
            end.
            for each tt_maximizacao
               where tt_maximizacao.tipo-widget <> 'browse'
                 and not (    tt_maximizacao.row-original >= b_tt_maximizacao.row-original
                          and tt_maximizacao.row-original + tt_maximizacao.height-original < b_tt_maximizacao.row-original + b_tt_maximizacao.height-original
                          and tt_maximizacao.col-original >= b_tt_maximizacao.col-original
                          and tt_maximizacao.col-original + tt_maximizacao.width-original < b_tt_maximizacao.col-original + b_tt_maximizacao.width-original )
                 and ((    tt_maximizacao.row-original >= b_tt_maximizacao.row-original
                       and tt_maximizacao.row-original < b_tt_maximizacao.row-original + b_tt_maximizacao.height-original - 0.5 )
                  or (     tt_maximizacao.row-original < b_tt_maximizacao.row-original
                       and tt_maximizacao.row-original + tt_maximizacao.height-original > b_tt_maximizacao.row-original )):
                assign tt_maximizacao.log-posiciona-col = yes.
            end.
        end.
        for each tt_maximizacao
           where tt_maximizacao.tipo-widget = 'rectangle':
            if tt_maximizacao.frame-width-original - tt_maximizacao.width-original < 4 then do:
                assign tt_maximizacao.log-posiciona-col  = no.
                assign tt_maximizacao.log-calcula-width  = yes.
            end.
        end.
        assign wh_w_program:max-width-chars = 300
               wh_w_program:max-height-chars = 300.

    &endif

        if  valid-handle (wh_w_program) then do:
            assign current-window = wh_w_program:handle.
        end.
    END. /* ON ENTRY OF wh_w_program */

    ON WINDOW-CLOSE OF wh_w_program
    DO:

        apply "choose" to bt_exi in frame f_bap_03_es_cons_estab_emp.

    END. /* ON WINDOW-CLOSE OF wh_w_program */
END.

/**************************** Window Trigger End ****************************/

/**************************** Menu Trigger Begin ****************************/

ON CHOOSE OF MENU-ITEM mi_fir IN MENU m_03_des_anot_tab
DO:

    apply "choose" to bt_fir in frame f_bap_03_es_cons_estab_emp.

END. /* ON CHOOSE OF MENU-ITEM mi_fir IN MENU m_03_des_anot_tab */

ON CHOOSE OF MENU-ITEM mi_pre IN MENU m_03_des_anot_tab
DO:

    apply "choose" to bt_pre1 in frame f_bap_03_es_cons_estab_emp.

END. /* ON CHOOSE OF MENU-ITEM mi_pre IN MENU m_03_des_anot_tab */

ON CHOOSE OF MENU-ITEM mi_nex IN MENU m_03_des_anot_tab
DO:

    apply "choose" to bt_nex1 in frame f_bap_03_es_cons_estab_emp.

END. /* ON CHOOSE OF MENU-ITEM mi_nex IN MENU m_03_des_anot_tab */

ON CHOOSE OF MENU-ITEM mi_las IN MENU m_03_des_anot_tab
DO:

    apply "choose" to bt_las in frame f_bap_03_es_cons_estab_emp.

END. /* ON CHOOSE OF MENU-ITEM mi_las IN MENU m_03_des_anot_tab */

ON CHOOSE OF MENU-ITEM mi_add IN MENU m_03_des_anot_tab
DO:

    apply "choose" to bt_add1 in frame f_bap_03_es_cons_estab_emp.

END. /* ON CHOOSE OF MENU-ITEM mi_add IN MENU m_03_des_anot_tab */

ON CHOOSE OF MENU-ITEM mi_mod IN MENU m_03_des_anot_tab
DO:

    apply "choose" to bt_mod1 in frame f_bap_03_es_cons_estab_emp.

END. /* ON CHOOSE OF MENU-ITEM mi_mod IN MENU m_03_des_anot_tab */

ON CHOOSE OF MENU-ITEM mi_era IN MENU m_03_des_anot_tab
DO:

    apply "choose" to bt_era1 in frame f_bap_03_es_cons_estab_emp.

END. /* ON CHOOSE OF MENU-ITEM mi_era IN MENU m_03_des_anot_tab */

ON CHOOSE OF MENU-ITEM mi_sea IN MENU m_03_des_anot_tab
DO:

    apply "choose" to bt_sea1 in frame f_bap_03_es_cons_estab_emp.

END. /* ON CHOOSE OF MENU-ITEM mi_sea IN MENU m_03_des_anot_tab */

ON CHOOSE OF MENU-ITEM mi_exi IN MENU m_03_des_anot_tab
DO:

    apply "choose" to bt_exi in frame f_bap_03_es_cons_estab_emp.

END. /* ON CHOOSE OF MENU-ITEM mi_exi IN MENU m_03_des_anot_tab */

ON CHOOSE OF MENU-ITEM mi_contents IN MENU m_03_des_anot_tab
DO:

    apply "choose" to bt_hel1 in frame f_bap_03_es_cons_estab_emp.

END. /* ON CHOOSE OF MENU-ITEM mi_contents IN MENU m_03_des_anot_tab */

ON CHOOSE OF MENU-ITEM mi_sobre IN MENU m_03_des_anot_tab
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

    /* Begin_Include: i_about_call */
    assign v_nom_prog     = substring(current-window:title, 1, max(1, length(current-window:title) - 10))
                          + chr(10)
                          + "bas_es_cons_estab_emp":U
           v_nom_prog_ext = "esp/esfgl071aa.p":U
           v_cod_release  = trim(" 1.00.00.000":U).

    {include/sobre5.i}
    /* End_Include: i_about_call */

END. /* ON CHOOSE OF MENU-ITEM mi_sobre IN MENU m_03_des_anot_tab */

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
    run pi_version_extract ('bas_es_cons_estab_emp':U, 'esp/esfgl071aa.p':U, '1.00.00.000':U, 'pro':U).
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
    run prgtec/men/men901za.py (Input 'bas_es_cons_estab_emp') /*prg_fnc_verify_security*/.
if  return-value = "2014"
then do:
    /* Programa a ser executado n∆o Ç um programa v†lido Datasul ! */
    run pi_messages (input "show", input 2014,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9", 'bas_es_cons_estab_emp')) /*msg_2014*/.
    return.
end.
if  return-value = "2012"
then do:
    /* Usu†rio sem permiss∆o para acessar o programa. */
    run pi_messages (input "show", input 2012,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9", 'bas_es_cons_estab_emp')) /*msg_2012*/.
    return.
end.
/* End_Include: i_verify_security */

/* Begin_Include: i_log_exec_prog_dtsul_ini */
assign v_rec_log = ?.

if can-find(prog_dtsul
       where prog_dtsul.cod_prog_dtsul = 'bas_es_cons_estab_emp' 
         and prog_dtsul.log_gera_log_exec = yes) then do transaction:
    create log_exec_prog_dtsul.
    assign log_exec_prog_dtsul.cod_prog_dtsul           = 'bas_es_cons_estab_emp'
           log_exec_prog_dtsul.cod_usuario              = v_cod_usuar_corren
           log_exec_prog_dtsul.dat_inic_exec_prog_dtsul = today
           log_exec_prog_dtsul.hra_inic_exec_prog_dtsul = replace(string(time,"hh:mm:ss"),":":U,"":U).
    assign v_rec_log = recid(log_exec_prog_dtsul).
    release log_exec_prog_dtsul no-error.
end.

/* End_Include: i_log_exec_prog_dtsul_ini */

/* ix_p00_bas_es_cons_estab_emp */
/* redefiniá‰es do menu */
assign sub-menu mi_table:label in menu m_03_des_anot_tab = "Arquivo".

/* redefiniá‰es de window, frame e menu */

/* Begin_Include: i_std_window */
/* tratamento do t°tulo, menu, vers∆o e dimens‰es */
assign wh_w_program:title         = frame f_bap_03_es_cons_estab_emp:title
                                  + chr(32)
                                  + chr(40)
                                  + trim(" 1.00.00.000":U)
                                  + chr(41)
       frame f_bap_03_es_cons_estab_emp:title       = ?
       wh_w_program:width-chars   = frame f_bap_03_es_cons_estab_emp:width-chars
       wh_w_program:height-chars  = frame f_bap_03_es_cons_estab_emp:height-chars - 0.85
       frame f_bap_03_es_cons_estab_emp:row         = 1
       frame f_bap_03_es_cons_estab_emp:col         = 1
       wh_w_program:menubar       = menu m_03_des_anot_tab:handle
       wh_w_program:col           = max((session:width-chars - wh_w_program:width-chars) / 2, 1)
       wh_w_program:row           = max((session:height-chars - wh_w_program:height-chars) / 2, 1)
       current-window             = wh_w_program.

find first modul_dtsul no-lock
     where modul_dtsul.cod_modul_dtsul = v_cod_modul_dtsul_corren no-error.
if  avail modul_dtsul then do:
    if  wh_w_program:load-icon(modul_dtsul.img_icone) then do:
        /* Utiliza como °cone sempre o °cone do m¢dulo corrente */
    end.
end.

/* End_Include: i_std_window */

{include/title5.i wh_w_program}

run pi_frame_settings(Input frame f_bap_03_es_cons_estab_emp:handle) /*pi_frame_settings*/.

/* redefiniá‰es bt_mov1 / bt_mov2 / bt_mov3 */
assign bt_mov1:help in frame f_bap_03_es_cons_estab_emp = "De Para Estabelecimentos"
       bt_mov2:help in frame f_bap_03_es_cons_estab_emp = "De Para Conta Cont†bil"
       bt_mov3:help in frame f_bap_03_es_cons_estab_emp = "De Para Centro de Custo".

pause 0 before-hide.
/* ix_p05_bas_es_cons_estab_emp */

view frame f_bap_03_es_cons_estab_emp.

enable bt_fir
       bt_pre1
       bt_nex1
       bt_las
       bt_add1
       bt_mod1
       bt_era1
       bt_mov1
       bt_mov2
       bt_mov3
       bt_sea1
       bt_exi
       bt_hel1
       es_cons_estab_emp.cod_empresa
       es_cons_estab_emp.cod_cenar_ctbl
       es_cons_estab_emp.dat_inic_valid
       with frame f_bap_03_es_cons_estab_emp.

if  v_rec_es_cons_estab_emp <> ? then do:
    find es_cons_estab_emp no-lock
         where recid(es_cons_estab_emp) = v_rec_es_cons_estab_emp no-error.
    if  not avail es_cons_estab_emp then
        apply "choose" to bt_fir in frame f_bap_03_es_cons_estab_emp.
    else do:
        assign v_rec_table = v_rec_es_cons_estab_emp.
        run pi_disp_fields /*pi_disp_fields*/.
    end.
end.
else
    apply "choose" to bt_fir in frame f_bap_03_es_cons_estab_emp.

main_block:
do on endkey undo main_block, leave main_block on error undo main_block, leave main_block:
    /* ix_p10_bas_es_cons_estab_emp */

    /* Begin_Include: ix_p15_bas_es_cons_estab_emp */
    if  search("prgint/utb/utb807aa.r") = ? and search("prgint/utb/utb807aa.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut†vel n∆o foi encontrado: prgint/utb/utb807aa.p".
        else do:
            message "Programa execut†vel n∆o foi encontrado: prgint/utb/utb807aa.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run prgint/utb/utb807aa.p /*prg_spp_criter_distrib_cta_ctbl*/.
    /* End_Include: ix_p15_bas_es_cons_estab_emp */

    if  not this-procedure:persistent then
        wait-for choose of bt_exi in frame f_bap_03_es_cons_estab_emp.
end /* do main_block */.

/******************************* Main Code End ******************************/

/************************* Internal Procedure Begin *************************/

/*****************************************************************************
** Procedure Interna.....: pi_disp_fields
** Descricao.............: pi_disp_fields
*****************************************************************************/
PROCEDURE pi_disp_fields:
    find es_cons_estab_emp no-lock
         where recid(es_cons_estab_emp) = v_rec_table no-error.

    if  avail es_cons_estab_emp then do:
        /* Begin_Include: i_ref_es_cons_estab_emp */
        find empresa no-lock
             where empresa.cod_empresa = es_cons_estab_emp.cod_empresa no-error.
        /* End_Include: i_ref_es_cons_estab_emp */

        /* ix_i05_bas_es_cons_estab_emp */
        disp es_cons_estab_emp.cod_empresa
             es_cons_estab_emp.cod_cenar_ctbl
             es_cons_estab_emp.dat_inic_valid
             es_cons_estab_emp.dat_fim_valid
             es_cons_estab_emp.cod_plano_cta_ctbl
             es_cons_estab_emp.cod_plano_ccusto
             with frame f_bap_03_es_cons_estab_emp.
        disp empresa.nom_razao_social when avail empresa
             "" when not avail empresa @ empresa.nom_razao_social
             with frame f_bap_03_es_cons_estab_emp.
        /* ix_i10_bas_es_cons_estab_emp */
    end.
END PROCEDURE. /* pi_disp_fields */
/*****************************************************************************
** Procedure Interna.....: pi_close_program
** Descricao.............: pi_close_program
*****************************************************************************/
PROCEDURE pi_close_program:
    assign v_rec_es_cons_estab_emp = if  avail es_cons_estab_emp then recid(es_cons_estab_emp) else ?.

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

    delete widget wh_w_program.
    if  this-procedure:persistent then
        delete procedure this-procedure.
END PROCEDURE. /* pi_close_program */
/*****************************************************************************
** Procedure Interna.....: pi_frame_settings
** Descricao.............: pi_frame_settings
*****************************************************************************/
PROCEDURE pi_frame_settings:

    /************************ Parameter Definition Begin ************************/

    def Input param p_wgh_frame
        as widget-handle
        format ">>>>>>9":U
        no-undo.

    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_wgh_child                      as widget-handle   no-undo.
    def var v_wgh_group                      as widget-handle   no-undo.

    /************************** Variable Definition End *************************/

    assign v_wgh_group = p_wgh_frame:first-child.
    block_group:
    do while v_wgh_group <> ?:

        assign v_wgh_child = v_wgh_group:first-child.

        block_child:
        do while v_wgh_child <> ?:
            if  v_wgh_child:type = "editor" /*l_editor*/ 
            then do:
                assign v_wgh_child:read-only = yes
                       v_wgh_child:sensitive = yes.
            end.
            assign v_wgh_child = v_wgh_child:next-sibling.
        end /* do block_child */.

        assign v_wgh_group = v_wgh_group:next-sibling.
    end /* do block_group */.

END PROCEDURE. /* pi_frame_settings */
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
/************************  End of bas_es_cons_estab_emp ***********************/

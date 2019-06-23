/*****************************************************************************
** Programa..............: sea_es_cons_estab_emp
** Descricao.............: Pesquisa Pai De Para Estab X Empresa
** Versao................:  1.00.00.000
** Procedimento..........: man_es_cons_estab_emp
** Nome Externo..........: esp/esfgl071ka.p
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
{include/i-license-manager.i sea_es_cons_estab_emp FGL}
&ENDIF

/******************************* Private-Data *******************************/
assign this-procedure:private-data = "HLP=15":U.
/*************************************  *************************************/

&if "{&emsuni_dbinst}" <> "yes" &then
run pi_messages (input "show", input 5884, input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9", "EMSUNI")) /*msg_5884*/.
&elseif "{&emsuni_version}" < "1.00" &then
run pi_messages (input "show", input 5009,
                 input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9",
                                    "SEA_ES_CONS_ESTAB_EMP","~~EMSUNI", "~~{~&emsuni_version}", "~~1.00")) /*msg_5009*/.
&else

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
def var v_cod_empresa_fim
    as character
    format "x(3)":U
    initial "ZZZ"
    label "atÇ"
    column-label "Final"
    no-undo.
def var v_cod_empresa_ini
    as character
    format "x(3)":U
    label "Inicial"
    column-label "Inicial"
    no-undo.
def var v_dat_inic_valid_fim
    as date
    format "99/99/9999":U
    initial 12/31/9999
    label "atÇ"
    column-label "Data Final"
    no-undo.
def var v_dat_inic_valid_ini
    as date
    format "99/99/9999":U
    initial 01/01/1800
    label "Data"
    column-label "Data Inicial"
    no-undo.
def var v_cod_cenar_ctbl_fim
    as character
    format "x(08)":U
    initial "ZZZZZZZZ"
    label "Final"
    column-label "Final"
    no-undo.
def var v_cod_cenar_ctbl_ini
    as character
    format "x(08)":U
    label "Cen†rio Inicial"
    column-label "Inicial"
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

/************************** Variable Definition End *************************/

/*************************** Menu Definition Begin **************************/

.

def menu      m_help                menubar
    menu-item mi_conteudo           label "&Conte£do"
    menu-item mi_sobre              label "&Sobre".

/**************************** Menu Definition End ***************************/

/************************** Query Definition Begin **************************/

def query qr_sea_es_cons_estab_emp
    for es_cons_estab_emp
    scrolling.

/*************************** Query Definition End ***************************/

/************************** Browse Definition Begin *************************/

def browse br_sea_es_cons_estab_emp query qr_sea_es_cons_estab_emp display 
    es_cons_estab_emp.cod_empresa
    width-chars 05.86
    es_cons_estab_emp.cod_cenar_ctbl
    width-chars 12.50
    es_cons_estab_emp.dat_inic_valid
    width-chars 12.00
    es_cons_estab_emp.dat_fim_valid
    width-chars 12.00
    es_cons_estab_emp.cod_plano_cta_ctbl
    width-chars 12.00
    es_cons_estab_emp.cod_plano_ccusto
    width-chars 14.00
    with no-box separators single 
         size 83.00 by 07.00
         font 1
         bgcolor 15.

/*************************** Browse Definition End **************************/

/************************ Rectangle Definition Begin ************************/

def rectangle rt_001
    size 1 by 1
    edge-pixels 2.
def rectangle rt_cxcf
    size 1 by 1
    fgcolor 1 edge-pixels 2.
def rectangle rt_cxcl
    size 1 by 1
    edge-pixels 2.
def rectangle rt_mold
    size 1 by 1
    edge-pixels 2.

/************************* Rectangle Definition End *************************/

/************************** Button Definition Begin *************************/

def button bt_add2
    label "Inclui"
    tooltip "Inclui"
    size 1 by 1.
def button bt_can
    label "Cancela"
    tooltip "Cancela"
    size 1 by 1
    auto-endkey.
def button bt_hel2
    label "Ajuda"
    tooltip "Ajuda"
    size 1 by 1.
def button bt_mod2
    label "Modifica"
    tooltip "Modifica"
    size 1 by 1.
def button bt_ok
    label "OK"
    tooltip "OK"
    size 1 by 1
    auto-go.
def button bt_ran
    label "Faixa"
    tooltip "Faixa"
    size 1 by 1.
/****************************** Function Button *****************************/

/*************************** Button Definition End **************************/

/************************ Radio-Set Definition Begin ************************/

def var rs_sea_es_cons_estab_emp
    as character
    initial "Por Data Inicio"
    view-as radio-set Horizontal
    radio-buttons "Por Data Inicio", "Por Data Inicio","Por Cen†rio", "Por Cen†rio","Por Empresa", "Por Empresa"
    bgcolor 15 
    no-undo.

/************************* Radio-Set Definition End *************************/

/************************** Frame Definition Begin **************************/

def frame f_fil_01_es_cons_estab_emp
    rt_001
         at row 01.21 col 02.00
    rt_cxcf
         at row 04.18 col 02.00 bgcolor 7 
    bt_ok
         at row 04.38 col 03.00 font ?
         help "OK"
    bt_can
         at row 04.38 col 14.00 font ?
         help "Cancela"
    bt_hel2
         at row 04.38 col 25.00 font ?
         help "Ajuda"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 37.43 by 06.00 default-button bt_ok
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Filtro Estabelecimento".
    /* adjust size of objects in this frame */
    assign bt_can:width-chars   in frame f_fil_01_es_cons_estab_emp = 10.00
           bt_can:height-chars  in frame f_fil_01_es_cons_estab_emp = 01.00
           bt_hel2:width-chars  in frame f_fil_01_es_cons_estab_emp = 10.00
           bt_hel2:height-chars in frame f_fil_01_es_cons_estab_emp = 01.00
           bt_ok:width-chars    in frame f_fil_01_es_cons_estab_emp = 10.00
           bt_ok:height-chars   in frame f_fil_01_es_cons_estab_emp = 01.00
           rt_001:width-chars   in frame f_fil_01_es_cons_estab_emp = 34.00
           rt_001:height-chars  in frame f_fil_01_es_cons_estab_emp = 02.46
           rt_cxcf:width-chars  in frame f_fil_01_es_cons_estab_emp = 33.99
           rt_cxcf:height-chars in frame f_fil_01_es_cons_estab_emp = 01.42.
    /* set private-data for the help system */
    assign bt_ok:private-data   in frame f_fil_01_es_cons_estab_emp = "HLP=000010721":U
           bt_can:private-data  in frame f_fil_01_es_cons_estab_emp = "HLP=000011050":U
           bt_hel2:private-data in frame f_fil_01_es_cons_estab_emp = "HLP=000011326":U
           frame f_fil_01_es_cons_estab_emp:private-data            = "HLP=000006535":U.

def frame f_ran_01_es_cons_estab_emp_data
    rt_mold
         at row 01.21 col 02.00
    rt_cxcf
         at row 03.79 col 02.00 bgcolor 7 
    v_dat_inic_valid_ini
         at row 01.38 col 20.00 colon-aligned label "Data Inicio"
         help "Data Inicial"
         view-as fill-in
         size-chars 12.00 by .88
         fgcolor ? bgcolor 15 font 2
    v_dat_inic_valid_fim
         at row 02.38 col 20.00 colon-aligned label "atÇ"
         help "Data Final"
         view-as fill-in
         size-chars 12.00 by .88
         fgcolor ? bgcolor 15 font 2
    bt_ok
         at row 04.00 col 03.00 font ?
         help "OK"
    bt_can
         at row 04.00 col 14.00 font ?
         help "Cancela"
    bt_hel2
         at row 04.00 col 37.57 font ?
         help "Ajuda"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 50.00 by 05.63 default-button bt_ok
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Faixa - Data".
    /* adjust size of objects in this frame */
    assign bt_can:width-chars   in frame f_ran_01_es_cons_estab_emp_data = 10.00
           bt_can:height-chars  in frame f_ran_01_es_cons_estab_emp_data = 01.00
           bt_hel2:width-chars  in frame f_ran_01_es_cons_estab_emp_data = 10.00
           bt_hel2:height-chars in frame f_ran_01_es_cons_estab_emp_data = 01.00
           bt_ok:width-chars    in frame f_ran_01_es_cons_estab_emp_data = 10.00
           bt_ok:height-chars   in frame f_ran_01_es_cons_estab_emp_data = 01.00
           rt_cxcf:width-chars  in frame f_ran_01_es_cons_estab_emp_data = 46.57
           rt_cxcf:height-chars in frame f_ran_01_es_cons_estab_emp_data = 01.42
           rt_mold:width-chars  in frame f_ran_01_es_cons_estab_emp_data = 46.57
           rt_mold:height-chars in frame f_ran_01_es_cons_estab_emp_data = 02.21.
    /* set private-data for the help system */
    assign v_dat_inic_valid_ini:private-data in frame f_ran_01_es_cons_estab_emp_data = "HLP=000016633":U
           v_dat_inic_valid_fim:private-data in frame f_ran_01_es_cons_estab_emp_data = "HLP=000016634":U
           bt_ok:private-data                in frame f_ran_01_es_cons_estab_emp_data = "HLP=000010721":U
           bt_can:private-data               in frame f_ran_01_es_cons_estab_emp_data = "HLP=000011050":U
           bt_hel2:private-data              in frame f_ran_01_es_cons_estab_emp_data = "HLP=000011326":U
           frame f_ran_01_es_cons_estab_emp_data:private-data                         = "HLP=000006535":U.

def frame f_ran_01_es_cons_estab_emp_empresa
    rt_mold
         at row 01.21 col 02.00
    rt_cxcf
         at row 03.79 col 02.00 bgcolor 7 
    v_cod_empresa_ini
         at row 01.38 col 20.00 colon-aligned label "Empresa"
         help "C¢digo Empresa Inicial"
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_empresa_fim
         at row 02.38 col 20.00 colon-aligned label "Final"
         help "C¢digo Empresa Final"
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_ok
         at row 04.00 col 03.00 font ?
         help "OK"
    bt_can
         at row 04.00 col 14.00 font ?
         help "Cancela"
    bt_hel2
         at row 04.00 col 37.57 font ?
         help "Ajuda"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 50.00 by 05.63 default-button bt_ok
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Faixa - Empresa".
    /* adjust size of objects in this frame */
    assign bt_can:width-chars   in frame f_ran_01_es_cons_estab_emp_empresa = 10.00
           bt_can:height-chars  in frame f_ran_01_es_cons_estab_emp_empresa = 01.00
           bt_hel2:width-chars  in frame f_ran_01_es_cons_estab_emp_empresa = 10.00
           bt_hel2:height-chars in frame f_ran_01_es_cons_estab_emp_empresa = 01.00
           bt_ok:width-chars    in frame f_ran_01_es_cons_estab_emp_empresa = 10.00
           bt_ok:height-chars   in frame f_ran_01_es_cons_estab_emp_empresa = 01.00
           rt_cxcf:width-chars  in frame f_ran_01_es_cons_estab_emp_empresa = 46.57
           rt_cxcf:height-chars in frame f_ran_01_es_cons_estab_emp_empresa = 01.42
           rt_mold:width-chars  in frame f_ran_01_es_cons_estab_emp_empresa = 46.57
           rt_mold:height-chars in frame f_ran_01_es_cons_estab_emp_empresa = 02.21.
    /* set private-data for the help system */
    assign v_cod_empresa_ini:private-data in frame f_ran_01_es_cons_estab_emp_empresa = "HLP=000021878":U
           v_cod_empresa_fim:private-data in frame f_ran_01_es_cons_estab_emp_empresa = "HLP=000021880":U
           bt_ok:private-data             in frame f_ran_01_es_cons_estab_emp_empresa = "HLP=000010721":U
           bt_can:private-data            in frame f_ran_01_es_cons_estab_emp_empresa = "HLP=000011050":U
           bt_hel2:private-data           in frame f_ran_01_es_cons_estab_emp_empresa = "HLP=000011326":U
           frame f_ran_01_es_cons_estab_emp_empresa:private-data                      = "HLP=000006535":U.

def frame f_ran_01_es_cons_estab_emp_cenario
    rt_mold
         at row 01.21 col 02.00
    rt_cxcf
         at row 03.79 col 02.00 bgcolor 7 
    v_cod_cenar_ctbl_ini
         at row 01.38 col 20.00 colon-aligned label "Cen†rio"
         help "Cen†rio Inicial"
         view-as fill-in
         size-chars 10.00 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_cenar_ctbl_fim
         at row 02.38 col 20.00 colon-aligned label "Final"
         help "Cen†rio Final"
         view-as fill-in
         size-chars 10.00 by .88
         fgcolor ? bgcolor 15 font 2
    bt_ok
         at row 04.00 col 03.00 font ?
         help "OK"
    bt_can
         at row 04.00 col 14.00 font ?
         help "Cancela"
    bt_hel2
         at row 04.00 col 45.57 font ?
         help "Ajuda"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 58.00 by 05.63 default-button bt_ok
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Faixa - Cen†rio".
    /* adjust size of objects in this frame */
    assign bt_can:width-chars   in frame f_ran_01_es_cons_estab_emp_cenario = 10.00
           bt_can:height-chars  in frame f_ran_01_es_cons_estab_emp_cenario = 01.00
           bt_hel2:width-chars  in frame f_ran_01_es_cons_estab_emp_cenario = 10.00
           bt_hel2:height-chars in frame f_ran_01_es_cons_estab_emp_cenario = 01.00
           bt_ok:width-chars    in frame f_ran_01_es_cons_estab_emp_cenario = 10.00
           bt_ok:height-chars   in frame f_ran_01_es_cons_estab_emp_cenario = 01.00
           rt_cxcf:width-chars  in frame f_ran_01_es_cons_estab_emp_cenario = 54.57
           rt_cxcf:height-chars in frame f_ran_01_es_cons_estab_emp_cenario = 01.42
           rt_mold:width-chars  in frame f_ran_01_es_cons_estab_emp_cenario = 54.57
           rt_mold:height-chars in frame f_ran_01_es_cons_estab_emp_cenario = 02.21.
    /* set private-data for the help system */
    assign v_cod_cenar_ctbl_ini:private-data in frame f_ran_01_es_cons_estab_emp_cenario = "HLP=000021976":U
           v_cod_cenar_ctbl_fim:private-data in frame f_ran_01_es_cons_estab_emp_cenario = "HLP=000021978":U
           bt_ok:private-data                in frame f_ran_01_es_cons_estab_emp_cenario = "HLP=000010721":U
           bt_can:private-data               in frame f_ran_01_es_cons_estab_emp_cenario = "HLP=000011050":U
           bt_hel2:private-data              in frame f_ran_01_es_cons_estab_emp_cenario = "HLP=000011326":U
           frame f_ran_01_es_cons_estab_emp_cenario:private-data                         = "HLP=000006535":U.

def frame f_sea_03_es_cons_estab_emp
    rt_cxcf
         at row 11.25 col 02.00 bgcolor 7 
    rt_cxcl
         at row 01.00 col 01.00 bgcolor 15 
    rs_sea_es_cons_estab_emp
         at row 01.21 col 03.00
         help "" no-label
    br_sea_es_cons_estab_emp
         at row 02.25 col 01.00
    bt_add2
         at row 09.75 col 02.00 font ?
         help "Inclui"
    bt_mod2
         at row 09.75 col 14.00 font ?
         help "Modifica"
    bt_ran
         at row 09.75 col 50.00 font ?
         help "Faixa"
    bt_ok
         at row 11.46 col 03.00 font ?
         help "OK"
    bt_can
         at row 11.46 col 14.00 font ?
         help "Cancela"
    bt_hel2
         at row 11.46 col 73.14 font ?
         help "Ajuda"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 85.57 by 13.08 default-button bt_ok
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Pesquisa De Para Estab x Emp".
    /* adjust size of objects in this frame */
    assign bt_add2:width-chars  in frame f_sea_03_es_cons_estab_emp = 10.00
           bt_add2:height-chars in frame f_sea_03_es_cons_estab_emp = 01.00
           bt_can:width-chars   in frame f_sea_03_es_cons_estab_emp = 10.00
           bt_can:height-chars  in frame f_sea_03_es_cons_estab_emp = 01.00
           bt_hel2:width-chars  in frame f_sea_03_es_cons_estab_emp = 10.00
           bt_hel2:height-chars in frame f_sea_03_es_cons_estab_emp = 01.00
           bt_mod2:width-chars  in frame f_sea_03_es_cons_estab_emp = 10.00
           bt_mod2:height-chars in frame f_sea_03_es_cons_estab_emp = 01.00
           bt_ok:width-chars    in frame f_sea_03_es_cons_estab_emp = 10.00
           bt_ok:height-chars   in frame f_sea_03_es_cons_estab_emp = 01.00
           bt_ran:width-chars   in frame f_sea_03_es_cons_estab_emp = 10.00
           bt_ran:height-chars  in frame f_sea_03_es_cons_estab_emp = 01.00
           rt_cxcf:width-chars  in frame f_sea_03_es_cons_estab_emp = 82.14
           rt_cxcf:height-chars in frame f_sea_03_es_cons_estab_emp = 01.42
           rt_cxcl:width-chars  in frame f_sea_03_es_cons_estab_emp = 84.14
           rt_cxcl:height-chars in frame f_sea_03_es_cons_estab_emp = 01.25.
&if '{&emsbas_version}' >= '5.06' &then
if OPSYS = 'WIN32':U then do:
assign br_sea_es_cons_estab_emp:ALLOW-COLUMN-SEARCHING in frame f_sea_03_es_cons_estab_emp = no
       br_sea_es_cons_estab_emp:COLUMN-MOVABLE         in frame f_sea_03_es_cons_estab_emp = no.
end.
&endif
    /* set private-data for the help system */
    assign rs_sea_es_cons_estab_emp:private-data in frame f_sea_03_es_cons_estab_emp = "HLP=000006535":U
           br_sea_es_cons_estab_emp:private-data in frame f_sea_03_es_cons_estab_emp = "HLP=000006535":U
           bt_add2:private-data                  in frame f_sea_03_es_cons_estab_emp = "HLP=000010825":U
           bt_mod2:private-data                  in frame f_sea_03_es_cons_estab_emp = "HLP=000010827":U
           bt_ran:private-data                   in frame f_sea_03_es_cons_estab_emp = "HLP=000008967":U
           bt_ok:private-data                    in frame f_sea_03_es_cons_estab_emp = "HLP=000010721":U
           bt_can:private-data                   in frame f_sea_03_es_cons_estab_emp = "HLP=000011050":U
           bt_hel2:private-data                  in frame f_sea_03_es_cons_estab_emp = "HLP=000011326":U
           frame f_sea_03_es_cons_estab_emp:private-data                             = "HLP=000006535":U.

{include/i_fclfrm.i f_fil_01_es_cons_estab_emp f_ran_01_es_cons_estab_emp_data f_ran_01_es_cons_estab_emp_empresa f_ran_01_es_cons_estab_emp_cenario f_sea_03_es_cons_estab_emp }
/*************************** Frame Definition End ***************************/

/*********************** User Interface Trigger Begin ***********************/

ON CHOOSE OF bt_hel2 IN FRAME f_fil_01_es_cons_estab_emp
DO:

    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.

    /* End_Include: i_context_help_frame */

END. /* ON CHOOSE OF bt_hel2 IN FRAME f_fil_01_es_cons_estab_emp */

ON CHOOSE OF bt_hel2 IN FRAME f_ran_01_es_cons_estab_emp_data
DO:

    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.

    /* End_Include: i_context_help_frame */

END. /* ON CHOOSE OF bt_hel2 IN FRAME f_ran_01_es_cons_estab_emp_data */

ON CHOOSE OF bt_hel2 IN FRAME f_ran_01_es_cons_estab_emp_empresa
DO:

    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.

    /* End_Include: i_context_help_frame */

END. /* ON CHOOSE OF bt_hel2 IN FRAME f_ran_01_es_cons_estab_emp_empresa */

ON CHOOSE OF bt_hel2 IN FRAME f_ran_01_es_cons_estab_emp_cenario
DO:

    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.

    /* End_Include: i_context_help_frame */

END. /* ON CHOOSE OF bt_hel2 IN FRAME f_ran_01_es_cons_estab_emp_cenario */

ON INS OF br_sea_es_cons_estab_emp IN FRAME f_sea_03_es_cons_estab_emp
DO:

    if  bt_add2:sensitive in frame f_sea_03_es_cons_estab_emp
    then do:
        apply "choose" to bt_add2 in frame f_sea_03_es_cons_estab_emp.
    end.

END. /* ON INS OF br_sea_es_cons_estab_emp IN FRAME f_sea_03_es_cons_estab_emp */

ON VALUE-CHANGED OF br_sea_es_cons_estab_emp IN FRAME f_sea_03_es_cons_estab_emp
DO:

    assign v_rec_table = recid(es_cons_estab_emp).
    find es_cons_estab_emp where recid(es_cons_estab_emp) = v_rec_table no-lock no-error.
END. /* ON VALUE-CHANGED OF br_sea_es_cons_estab_emp IN FRAME f_sea_03_es_cons_estab_emp */

ON CHOOSE OF bt_add2 IN FRAME f_sea_03_es_cons_estab_emp
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
    assign v_rec_table = v_rec_es_cons_estab_emp.
    run pi_open_sea_es_cons_estab_emp /*pi_open_sea_es_cons_estab_emp*/.
    reposition qr_sea_es_cons_estab_emp to recid v_rec_table no-error.

END. /* ON CHOOSE OF bt_add2 IN FRAME f_sea_03_es_cons_estab_emp */

ON CHOOSE OF bt_can IN FRAME f_sea_03_es_cons_estab_emp
DO:

    apply "end-error" to self.
END. /* ON CHOOSE OF bt_can IN FRAME f_sea_03_es_cons_estab_emp */

ON CHOOSE OF bt_hel2 IN FRAME f_sea_03_es_cons_estab_emp
DO:

    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.

    /* End_Include: i_context_help_frame */

END. /* ON CHOOSE OF bt_hel2 IN FRAME f_sea_03_es_cons_estab_emp */

ON CHOOSE OF bt_mod2 IN FRAME f_sea_03_es_cons_estab_emp
DO:

    if  avail es_cons_estab_emp
    then do:
        assign v_rec_es_cons_estab_emp = recid(es_cons_estab_emp)
               v_rec_table    = recid(es_cons_estab_emp).
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
        if  v_rec_es_cons_estab_emp <> ?
        then do:
            assign v_rec_table = v_rec_es_cons_estab_emp.
        end.
        run pi_open_sea_es_cons_estab_emp /*pi_open_sea_es_cons_estab_emp*/.
        reposition qr_sea_es_cons_estab_emp to recid v_rec_table no-error.
    end.
END. /* ON CHOOSE OF bt_mod2 IN FRAME f_sea_03_es_cons_estab_emp */

ON CHOOSE OF bt_ok IN FRAME f_sea_03_es_cons_estab_emp
DO:

    if  avail es_cons_estab_emp then
        assign v_rec_es_cons_estab_emp = recid(es_cons_estab_emp).

END. /* ON CHOOSE OF bt_ok IN FRAME f_sea_03_es_cons_estab_emp */

ON CHOOSE OF bt_ran IN FRAME f_sea_03_es_cons_estab_emp
DO:

    /* case_block: */
    case input frame f_sea_03_es_cons_estab_emp rs_sea_es_cons_estab_emp:
        when "Por Data Inicio" then
            dat_inic_valid_block:
            do:
                /* Begin_Include: i_sea_range */
                view frame f_ran_01_es_cons_estab_emp_data.

                range_block:
                do on error undo range_block, retry range_block:
                    update v_dat_inic_valid_ini
                           v_dat_inic_valid_fim
                           bt_ok
                           bt_can
                           bt_hel2
                           with frame f_ran_01_es_cons_estab_emp_data.
                    run pi_open_sea_es_cons_estab_emp /*pi_open_sea_es_cons_estab_emp*/.
                end /* do range_block */.

                hide frame f_ran_01_es_cons_estab_emp_data.

                /* End_Include: i_sea_range */

            end /* do dat_inic_valid_block */.
        when "Por Cen†rio" then
            nom_estab_block:
            do:
                /* Begin_Include: i_sea_range */
                view frame f_ran_01_es_cons_estab_emp_cenario.

                range_block:
                do on error undo range_block, retry range_block:
                    update v_cod_cenar_ctbl_ini
                           v_cod_cenar_ctbl_fim
                           bt_ok
                           bt_can
                           bt_hel2
                           with frame f_ran_01_es_cons_estab_emp_cenario.
                    run pi_open_sea_es_cons_estab_emp /*pi_open_sea_es_cons_estab_emp*/.
                end /* do range_block */.

                hide frame f_ran_01_es_cons_estab_emp_cenario.

                /* End_Include: i_sea_range */

            end /* do nom_estab_block */.
        when "Por Empresa" then
            empresa_block:
            do:
                /* Begin_Include: i_sea_range */
                view frame f_ran_01_es_cons_estab_emp_empresa.

                range_block:
                do on error undo range_block, retry range_block:
                    update v_cod_empresa_ini
                           v_cod_empresa_fim
                           bt_ok
                           bt_can
                           bt_hel2
                           with frame f_ran_01_es_cons_estab_emp_empresa.
                    run pi_open_sea_es_cons_estab_emp /*pi_open_sea_es_cons_estab_emp*/.
                end /* do range_block */.

                hide frame f_ran_01_es_cons_estab_emp_empresa.

                /* End_Include: i_sea_range */

            end /* do empresa_block */.
    end /* case case_block */.
END. /* ON CHOOSE OF bt_ran IN FRAME f_sea_03_es_cons_estab_emp */

ON VALUE-CHANGED OF rs_sea_es_cons_estab_emp IN FRAME f_sea_03_es_cons_estab_emp
DO:

    run pi_open_sea_es_cons_estab_emp /*pi_open_sea_es_cons_estab_emp*/.
END. /* ON VALUE-CHANGED OF rs_sea_es_cons_estab_emp IN FRAME f_sea_03_es_cons_estab_emp */

/************************ User Interface Trigger End ************************/

/**************************** Frame Trigger Begin ***************************/

ON HELP OF FRAME f_fil_01_es_cons_estab_emp ANYWHERE
DO:

    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */
END. /* ON HELP OF FRAME f_fil_01_es_cons_estab_emp */

ON RIGHT-MOUSE-DOWN OF FRAME f_fil_01_es_cons_estab_emp ANYWHERE
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
END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_fil_01_es_cons_estab_emp */

ON RIGHT-MOUSE-UP OF FRAME f_fil_01_es_cons_estab_emp ANYWHERE
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

END. /* ON RIGHT-MOUSE-UP OF FRAME f_fil_01_es_cons_estab_emp */

ON WINDOW-CLOSE OF FRAME f_fil_01_es_cons_estab_emp
DO:

    apply "end-error" to self.
END. /* ON WINDOW-CLOSE OF FRAME f_fil_01_es_cons_estab_emp */

ON HELP OF FRAME f_ran_01_es_cons_estab_emp_data ANYWHERE
DO:

    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */

END. /* ON HELP OF FRAME f_ran_01_es_cons_estab_emp_data */

ON RIGHT-MOUSE-DOWN OF FRAME f_ran_01_es_cons_estab_emp_data ANYWHERE
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
               assign v_wgh_frame = v_wgh_frame:frame.
        end.
        assign v_nom_title_aux   = v_wgh_frame:title
               v_wgh_frame:title = self:help.
    end.
    /* End_Include: i_right_mouse_down_dialog_box */

END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_ran_01_es_cons_estab_emp_data */

ON RIGHT-MOUSE-UP OF FRAME f_ran_01_es_cons_estab_emp_data ANYWHERE
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

        assign v_wgh_frame = self:frame.
        if  (v_wgh_frame:type <> "DIALOG-BOX") and (v_wgh_frame:frame <> ?)
        then do:
               assign v_wgh_frame = v_wgh_frame:frame.
        end.
        assign v_wgh_frame:title = v_nom_title_aux.
    end.

    /* End_Include: i_right_mouse_up_dialog_box */

END. /* ON RIGHT-MOUSE-UP OF FRAME f_ran_01_es_cons_estab_emp_data */

ON WINDOW-CLOSE OF FRAME f_ran_01_es_cons_estab_emp_data
DO:

    apply "end-error" to self.
END. /* ON WINDOW-CLOSE OF FRAME f_ran_01_es_cons_estab_emp_data */

ON HELP OF FRAME f_ran_01_es_cons_estab_emp_empresa ANYWHERE
DO:

    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */

END. /* ON HELP OF FRAME f_ran_01_es_cons_estab_emp_empresa */

ON RIGHT-MOUSE-DOWN OF FRAME f_ran_01_es_cons_estab_emp_empresa ANYWHERE
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
               assign v_wgh_frame = v_wgh_frame:frame.
        end.
        assign v_nom_title_aux   = v_wgh_frame:title
               v_wgh_frame:title = self:help.
    end.
    /* End_Include: i_right_mouse_down_dialog_box */

END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_ran_01_es_cons_estab_emp_empresa */

ON RIGHT-MOUSE-UP OF FRAME f_ran_01_es_cons_estab_emp_empresa ANYWHERE
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

        assign v_wgh_frame = self:frame.
        if  (v_wgh_frame:type <> "DIALOG-BOX") and (v_wgh_frame:frame <> ?)
        then do:
               assign v_wgh_frame = v_wgh_frame:frame.
        end.
        assign v_wgh_frame:title = v_nom_title_aux.
    end.

    /* End_Include: i_right_mouse_up_dialog_box */

END. /* ON RIGHT-MOUSE-UP OF FRAME f_ran_01_es_cons_estab_emp_empresa */

ON WINDOW-CLOSE OF FRAME f_ran_01_es_cons_estab_emp_empresa
DO:

    apply "end-error" to self.
END. /* ON WINDOW-CLOSE OF FRAME f_ran_01_es_cons_estab_emp_empresa */

ON HELP OF FRAME f_ran_01_es_cons_estab_emp_cenario ANYWHERE
DO:

    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */

END. /* ON HELP OF FRAME f_ran_01_es_cons_estab_emp_cenario */

ON RIGHT-MOUSE-DOWN OF FRAME f_ran_01_es_cons_estab_emp_cenario ANYWHERE
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
               assign v_wgh_frame = v_wgh_frame:frame.
        end.
        assign v_nom_title_aux   = v_wgh_frame:title
               v_wgh_frame:title = self:help.
    end.
    /* End_Include: i_right_mouse_down_dialog_box */

END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_ran_01_es_cons_estab_emp_cenario */

ON RIGHT-MOUSE-UP OF FRAME f_ran_01_es_cons_estab_emp_cenario ANYWHERE
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

        assign v_wgh_frame = self:frame.
        if  (v_wgh_frame:type <> "DIALOG-BOX") and (v_wgh_frame:frame <> ?)
        then do:
            assign v_wgh_frame = v_wgh_frame:frame.
        end.
        assign v_wgh_frame:title = v_nom_title_aux.
    end.

    /* End_Include: i_right_mouse_up_dialog_box */

END. /* ON RIGHT-MOUSE-UP OF FRAME f_ran_01_es_cons_estab_emp_cenario */

ON WINDOW-CLOSE OF FRAME f_ran_01_es_cons_estab_emp_cenario
DO:

    apply "end-error" to self.
END. /* ON WINDOW-CLOSE OF FRAME f_ran_01_es_cons_estab_emp_cenario */

ON ENDKEY OF FRAME f_sea_03_es_cons_estab_emp
DO:

END. /* ON ENDKEY OF FRAME f_sea_03_es_cons_estab_emp */

ON END-ERROR OF FRAME f_sea_03_es_cons_estab_emp
DO:

    assign v_rec_es_cons_estab_emp = ?.
END. /* ON END-ERROR OF FRAME f_sea_03_es_cons_estab_emp */

ON ENTRY OF FRAME f_sea_03_es_cons_estab_emp
DO:

    apply "value-changed" to rs_sea_es_cons_estab_emp in frame f_sea_03_es_cons_estab_emp.
END. /* ON ENTRY OF FRAME f_sea_03_es_cons_estab_emp */

ON HELP OF FRAME f_sea_03_es_cons_estab_emp ANYWHERE
DO:

    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */

END. /* ON HELP OF FRAME f_sea_03_es_cons_estab_emp */

ON RIGHT-MOUSE-DOWN OF FRAME f_sea_03_es_cons_estab_emp ANYWHERE
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
               assign v_wgh_frame = v_wgh_frame:frame.
        end.
        assign v_nom_title_aux   = v_wgh_frame:title
               v_wgh_frame:title = self:help.
    end.
    /* End_Include: i_right_mouse_down_dialog_box */

END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_sea_03_es_cons_estab_emp */

ON RIGHT-MOUSE-UP OF FRAME f_sea_03_es_cons_estab_emp ANYWHERE
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

        assign v_wgh_frame = self:frame.
        if  (v_wgh_frame:type <> "DIALOG-BOX") and (v_wgh_frame:frame <> ?)
        then do:
            assign v_wgh_frame = v_wgh_frame:frame.
        end.
        assign v_wgh_frame:title = v_nom_title_aux.
    end.

    /* End_Include: i_right_mouse_up_dialog_box */

END. /* ON RIGHT-MOUSE-UP OF FRAME f_sea_03_es_cons_estab_emp */

ON WINDOW-CLOSE OF FRAME f_sea_03_es_cons_estab_emp
DO:
    apply "end-error" to self.
END. /* ON WINDOW-CLOSE OF FRAME f_sea_03_es_cons_estab_emp */

/***************************** Frame Trigger End ****************************/

/**************************** Menu Trigger Begin ****************************/

ON CHOOSE OF MENU-ITEM mi_conteudo IN MENU m_help
DO:
    apply "choose" to bt_hel2 in frame f_sea_03_es_cons_estab_emp.
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

    assign v_nom_prog     = substr(frame f_sea_03_es_cons_estab_emp:title, 1, max(1, length(frame f_sea_03_es_cons_estab_emp:title) - 10)).
    if  v_nom_prog = ? then
        assign v_nom_prog = "".

    assign v_nom_prog = v_nom_prog + chr(10) + "sea_es_cons_estab_emp":U.

    assign v_nom_prog_ext = "esp/esfgl071ka.p":U
           v_cod_release  = trim(" 1.00.00.000":U).

    {include/sobre5.i}
END. /* ON CHOOSE OF MENU-ITEM mi_sobre IN MENU m_help */

/***************************** Menu Trigger End *****************************/

/****************************** Main Code Begin *****************************/

/* Begin_Include: i_version_extract */
/* {include/i-ctrlrp5.i sea_es_cons_estab_emp} */

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
    run pi_version_extract ('sea_es_cons_estab_emp':U, 'esp/esfgl071ka.p':U, '1.00.00.000':U, 'pro':U).
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
    run prgtec/men/men901za.py (Input 'sea_es_cons_estab_emp') /*prg_fnc_verify_security*/.
if  return-value = "2014"
then do:
    /* Programa a ser executado n∆o Ç um programa v†lido Datasul ! */
    run pi_messages (input "show", input 2014,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'sea_es_cons_estab_emp')) /*msg_2014*/.
    return.
end.
if  return-value = "2012"
then do:
    /* Usu†rio sem permiss∆o para acessar o programa. */
    run pi_messages (input "show", input 2012,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'sea_es_cons_estab_emp')) /*msg_2012*/.
    return.
end.
/* End_Include: i_verify_security */

/* Begin_Include: i_log_exec_prog_dtsul_ini */
assign v_rec_log = ?.

if can-find(prog_dtsul
       where prog_dtsul.cod_prog_dtsul = 'sea_es_cons_estab_emp' 
         and prog_dtsul.log_gera_log_exec = yes) then do transaction:
    create log_exec_prog_dtsul.
    assign log_exec_prog_dtsul.cod_prog_dtsul           = 'sea_es_cons_estab_emp'
           log_exec_prog_dtsul.cod_usuario              = v_cod_usuar_corren
           log_exec_prog_dtsul.dat_inic_exec_prog_dtsul = today
           log_exec_prog_dtsul.hra_inic_exec_prog_dtsul = replace(string(time,"hh:mm:ss"),":":U,"":U).
    assign v_rec_log = recid(log_exec_prog_dtsul).
    release log_exec_prog_dtsul no-error.
end.

/* End_Include: i_log_exec_prog_dtsul_ini */

/* Begin_Include: ix_p00_sea_es_cons_estab_emp */

/* End_Include: i_atuliz_var_empres_usuar */

/* Begin_Include: i_std_dialog_box */
/* tratamento do titulo e vers∆o */
assign frame f_sea_03_es_cons_estab_emp:title = frame f_sea_03_es_cons_estab_emp:title
                            + chr(32)
                            + chr(40)
                            + trim(" 1.00.00.000":U)
                            + chr(41).
/* menu pop-up de ajuda e sobre */
assign menu m_help:popup-only = yes
       bt_hel2:popup-menu in frame f_sea_03_es_cons_estab_emp = menu m_help:handle.

/* End_Include: i_std_dialog_box */

{include/title5.i f_sea_03_es_cons_estab_emp FRAME}

assign br_sea_es_cons_estab_emp:num-locked-columns in frame f_sea_03_es_cons_estab_emp = 1.

/* ix_p05_sea_es_cons_estab_emp */

pause 0 before-hide.
view frame f_sea_03_es_cons_estab_emp.

assign v_rec_table = v_rec_es_cons_estab_emp.

main_block:
do on endkey undo main_block, leave main_block on error undo main_block, leave main_block.
    enable rs_sea_es_cons_estab_emp
           br_sea_es_cons_estab_emp
           bt_add2
           bt_mod2
           bt_ran
           bt_ok
           bt_can
           bt_hel2
           with frame f_sea_03_es_cons_estab_emp.

    if  index(program-name(2), "esp/esfgl071ca.p") <> 0
    or  index(program-name(2), "esp/esfgl071ea.p") <> 0
    or  index(program-name(2), "esp/esfgl071ia.p") <> 0
    or  index(program-name(2), "esp/esfgl071ga.p") <> 0
    then do:
         assign bt_add2:sensitive in frame f_sea_03_es_cons_estab_emp = no
                bt_mod2:sensitive in frame f_sea_03_es_cons_estab_emp = no.
    end.

    /* Begin_Include: ix_p20_sea_es_cons_estab_emp */

    /* End_Include: i_disable_buttons_zoom */

    wait-for go of frame f_sea_03_es_cons_estab_emp
          or default-action of br_sea_es_cons_estab_emp
          or mouse-select-dblclick of br_sea_es_cons_estab_emp focus browse br_sea_es_cons_estab_emp.
    if  avail es_cons_estab_emp
    then do:
        assign v_rec_es_cons_estab_emp = recid(es_cons_estab_emp).
        /* ix_p30_sea_es_cons_estab_emp */
    end.
    else do:
        assign v_rec_es_cons_estab_emp = ?.
        /* ix_p35_sea_es_cons_estab_emp */
    end.
end /* do main_block */.

hide frame f_sea_03_es_cons_estab_emp.

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
** Procedure Interna.....: pi_open_sea_es_cons_estab_emp
** Descricao.............: pi_open_sea_es_cons_estab_emp
*****************************************************************************/
PROCEDURE pi_open_sea_es_cons_estab_emp:
    /* case_block: */
    case input frame f_sea_03_es_cons_estab_emp rs_sea_es_cons_estab_emp:
        when "Por Data Inicio" then
            open query qr_sea_es_cons_estab_emp for
                  each es_cons_estab_emp no-lock
                 where es_cons_estab_emp.dat_inic_valid >= v_dat_inic_valid_ini
                   and es_cons_estab_emp.dat_inic_valid <= v_dat_inic_valid_fim
                    by es_cons_estab_emp.dat_inic_valid.
        when "Por Cen†rio" then
            open query qr_sea_es_cons_estab_emp for
                  each es_cons_estab_emp no-lock
                 where es_cons_estab_emp.cod_cenar_ctbl >= v_cod_cenar_ctbl_ini
                   and es_cons_estab_emp.cod_cenar_ctbl <= v_cod_cenar_ctbl_fim
                    by es_cons_estab_emp.cod_cenar_ctbl.
        when "Por Empresa" then
            open query qr_sea_es_cons_estab_emp for
                  each es_cons_estab_emp no-lock
                 where es_cons_estab_emp.cod_empresa >= v_cod_empresa_ini
                   and es_cons_estab_emp.cod_empresa <= v_cod_empresa_fim                  
                    by es_cons_estab_emp.cod_empresa.            
    end /* case case_block */.

END PROCEDURE. /* pi_open_sea_es_cons_estab_emp */
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
/************************  End of sea_es_cons_estab_emp ***********************/

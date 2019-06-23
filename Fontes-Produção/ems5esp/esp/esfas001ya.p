def var c-versao-prg as char initial " 5.06.00.001":U no-undo.

{include/i_dbinst.i}
{include/i_dbtype.i}
{include/i_fcldef.i}

/******************************* Private-Data *******************************/
assign this-procedure:private-data = "HLP=2":U.
/*************************************  *************************************/

&if "{&emsfin_dbinst}" <> "yes" &then
run pi_messages (input "show",
                 input 5884,
                 input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9", 
                                    "EMSFIN")) /*msg_5884*/.
&elseif "{&emsfin_version}" < "5.06" &then
run pi_messages (input "show",
                 input 5009,
                 input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9", 
                                    "rpt_razao_grp_cta_espec","~~EMSFIN", "~~{~&emsfin_version}", "~~5.06")) /*msg_5009*/.
&else

/************************** Buffer Definition Begin *************************/

&if "{&emsbas_version}" >= "1.00" &then
def buffer b_ped_exec_style
    for ped_exec.
&endif
&if "{&emsfin_version}" >= "1.00" &then
def buffer bf_sdo_bem_pat
    for sdo_bem_pat.
&endif
&if "{&emsbas_version}" >= "1.00" &then
def buffer b_servid_exec_style
    for servid_exec.
&endif

/*************************** Buffer Definition End **************************/

/************************** Stream Definition Begin *************************/

def new shared stream s_1.

/*************************** Stream Definition End **************************/

/************************* Variable Definition Begin ************************/

def new global shared var v5_cod_usuar_corren
    as character
    format 'x(12)'
    label 'Usu†rio Corrente'
    column-label 'Usu†rio Corrente'
    no-undo.

def new global shared var v5_cod_usuar_corren_criptog
    as character
    format 'x(16)'
    no-undo.

def new global shared var v_cod_aplicat_dtsul_corren
    as character
    format "x(3)":U
    no-undo.
def new global shared var v_cod_ccusto_corren
    as character
    format "x(11)":U
    label "Centro Custo"
    column-label "Centro Custo"
    no-undo.
def var v_cod_cenar_ctbl
    as character
    format "x(8)":U
    label "Cen†rio Cont†bil"
    column-label "Cen†rio Cont†bil"
    no-undo.
def var v_cod_cta_ctbl
    as character
    format "x(20)":U
    label "Conta Cont†bil"
    column-label "Conta Cont†bil"
    no-undo.
def new shared var v_cod_dwb_file
    as character
    format "x(40)":U
    label "Arquivo"
    column-label "Arquivo"
    no-undo.
def var v_cod_dwb_file_temp
    as character
    format "x(12)":U
    no-undo.
def var v_cod_dwb_parameters
    as character
    format "x(8)":U
    no-undo.
def var v_cod_dwb_print_layout
    as character
    format "x(8)":U
    no-undo.
def var v_cod_dwb_proced
    as character
    format "x(8)":U
    no-undo.
def new shared var v_cod_dwb_program
    as character
    format "x(32)":U
    label "Programa"
    column-label "Programa"
    no-undo.
def new global shared var v_cod_dwb_user
    as character
    format "x(21)":U
    label "Usu†rio"
    column-label "Usu†rio"
    no-undo.
def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.
def new global shared var v_cod_estab_usuar
    as character
    format "x(3)":U
    label "Estabelecimento"
    column-label "Estab"
    no-undo.
def var v_cod_finalid_econ
    as character
    format "x(10)":U
    label "Finalidade Econìmica"
    column-label "Finalidade Econìmica"
    no-undo.
def new global shared var v_cod_funcao_negoc_empres
    as character
    format "x(50)":U
    no-undo.
def new global shared var v_cod_grp_usuar_lst
    as character
    format "x(3)":U
    label "Grupo Usu†rios"
    column-label "Grupo"
    no-undo.
def new global shared var v_cod_idiom_usuar
    as character
    format "x(8)":U
    label "Idioma"
    column-label "Idioma"
    no-undo.
def new global shared var v_cod_modul_dtsul_corren
    as character
    format "x(3)":U
    label "M¢dulo Corrente"
    column-label "M¢dulo Corrente"
    no-undo.
def new global shared var v_cod_modul_dtsul_empres
    as character
    format "x(100)":U
    no-undo.
def new global shared var v_cod_pais_empres_usuar
    as character
    format "x(3)":U
    label "Pa°s Empresa Usu†rio"
    column-label "Pa°s"
    no-undo.
def new global shared var v_cod_plano_ccusto_corren
    as character
    format "x(8)":U
    label "Plano CCusto"
    column-label "Plano CCusto"
    no-undo.
def var v_cod_plano_cta_ctbl
    as character
    format "x(8)":U
    label "Plano Contas"
    column-label "Plano Contas"
    no-undo.
def new shared var v_cod_release
    as character
    format "x(12)":U
    no-undo.
def new global shared var v_cod_unid_negoc_usuar
    as character
    format "x(3)":U
    view-as combo-box
    list-items ""
    inner-lines 5
    bgcolor 15 font 2
    label "Unidade Neg¢cio"
    column-label "Unid Neg¢cio"
    no-undo.
def new global shared var v_cod_usuar_corren
    as character
    format "x(12)":U
    label "Usu†rio Corrente"
    column-label "Usu†rio Corrente"
    no-undo.
def new global shared var v_cod_usuar_corren_criptog
    as character
    format "x(16)":U
    no-undo.
def new shared var v_dat_execution
    as date
    format "99/99/9999":U
    no-undo.
def new shared var v_dat_execution_end
    as date
    format "99/99/9999":U
    no-undo.
def var v_cod_empresa_ini
    as character
    format "x(3)"
    label "Empresa Inicial"
    no-undo.
def var v_cod_empresa_fim
    as character
    format "x(3)"
    label "Empresa Final"
    initial "ZZZ"
    no-undo.
def var v_cod_estab_ini
    as character
    format "x(3)"
    label "Estabelecimento Inicial"
    no-undo.
def var v_cod_estab_fim
    as character
    format "x(3)"
    label "Estabelecimento Final"
    initial "ZZZ"
    no-undo.
def var v_dat_inic
    as date
    format "99/99/9999":U
    initial today
    label "Per°odo Inicial"
    column-label "Per°odo Inicial"
    no-undo.
def var v_dat_fim
    as date
    format "99/99/9999":U
    initial 12/31/9999
    label "atÇ"
    column-label "Final"
    no-undo.
def new shared var v_hra_execution
    as Character
    format "99:99":U
    no-undo.
def new shared var v_hra_execution_end
    as Character
    format "99:99:99":U
    label "Tempo Exec"
    no-undo.
def var v_ind_dwb_run_mode
    as character
    format "X(07)":U
    initial "On-Line" /*l_online*/
    view-as radio-set Horizontal
    radio-buttons "On-Line", "On-Line","Batch", "Batch"
     /*l_online*/ /*l_online*/ /*l_batch*/ /*l_batch*/
    bgcolor 8 
    label "Run Mode"
    column-label "Run Mode"
    no-undo.
def new global shared var v_log_execution
    as logical
    format "Sim/N∆o"
    initial yes
    no-undo.
def var v_log_method
    as logical
    format "Sim/N∆o"
    initial yes
    no-undo.
def var v_log_print
    as logical
    format "Sim/N∆o"
    initial no
    no-undo.
def var v_log_print_par
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    no-undo.
def var v_nom_dwb_printer
    as character
    format "x(30)":U
    no-undo.
def var v_nom_dwb_print_file
    as character
    format "x(100)":U
    label "Arquivo Impress∆o"
    column-label "Arq Impr"
    no-undo.
def new shared var v_nom_enterprise
    as character
    format "x(40)":U
    no-undo.
def var v_nom_integer
    as character
    format "x(30)":U
    no-undo.

def new shared var v_nom_prog_ext
    as character
    format "x(8)":U
    label "Nome Externo"
    no-undo.
def new shared var v_nom_report_title
    as character
    format "x(40)":U
    no-undo.
def var v_nom_title_aux
    as character
    format "x(60)":U
    no-undo.
def var v_num_count
    as integer
    format ">>>>,>>9":U
    no-undo.
def new shared var v_num_entry
    as integer
    format ">>>>,>>9":U
    label "Ordem"
    column-label "Ordem"
    no-undo.
def new shared var v_num_page_number
    as integer
    format ">>>>>9":U
    label "P†gina"
    column-label "P†gina"
    no-undo.
def var v_num_ped_exec
    as integer
    format ">>>>9":U
    label "Pedido"
    column-label "Pedido"
    no-undo.
def new global shared var v_num_ped_exec_corren
    as integer
    format ">>>>9":U
    no-undo.
def var v_qtd_bottom
    as decimal
    format ">>9":U
    decimals 0
    no-undo.
def var v_qtd_column
    as decimal
    format ">>9":U
    decimals 0
    label "Colunas"
    column-label "Colunas"
    no-undo.
def var v_qtd_line
    as decimal
    format ">>9":U
    decimals 0
    label "Linhas"
    column-label "Linhas"
    no-undo.
def var v_qtd_line_ant
    as decimal
    format "->>>>,>>9.9999":U
    decimals 4
    no-undo.
def new global shared var v_rec_cenar_ctbl
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.
def new global shared var v_rec_cta_ctbl
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_finalid_econ
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_grp_cta_pat_usuar
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.
def var v_rec_log
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_plano_cta_ctbl
    as recid
    format ">>>>>>9":U
    no-undo.

def var v_val_g_dpr_amort_tot     as decimal no-undo.
def var v_val_g_dpr_amort_ini     as decimal no-undo.
def var v_val_g_dpr_amort_per     as decimal no-undo.
def var v_val_g_adic              as decimal no-undo.
def var v_val_g_bxa_imob_total    as decimal no-undo.
def var v_val_g_bxa_imob_vendas   as decimal no-undo.
def var v_val_g_bxa_imob_outros   as decimal no-undo.
def var v_val_g_cm                as decimal no-undo.
def var v_val_g_dpr_amort_bxa     as decimal no-undo.
def var v_val_g_dpr_transf        as decimal no-undo.
def var v_val_g_transf            as decimal no-undo.
def var v_val_g_sdo_imob_cta      as decimal no-undo.
def var v_val_g_sdo_inicial_cta   as decimal no-undo.
def var v_val_g_salvage_value_cta as decimal no-undo.

def var v_val_amort_inic          as decimal no-undo.
def var v_val_dpr_amort_tot       as decimal no-undo.
def var v_val_dpr_amort_ini       as decimal no-undo.
def var v_val_dpr_amort_per       as decimal no-undo.
def var v_val_dpr_inic            as decimal no-undo.
                                  
def var v_val_juros               as decimal no-undo.
def var v_val_adic                as decimal no-undo.
def var v_val_bxa_dpr_transf      as decimal no-undo.
def var v_val_bxa_imob_total      as decimal no-undo.
def var v_val_bxa_imob_outros     as decimal no-undo.
def var v_val_bxa_imob_vendas     as decimal no-undo.
def var v_val_bxa_transf          as decimal no-undo.
def var v_val_cm                  as decimal no-undo.
def var v_val_dpr_amort_bxa       as decimal no-undo.
def var v_val_dpr_transf          as decimal no-undo.
def var v_val_impl_dpr_transf     as decimal no-undo.
def var v_val_impl_transf         as decimal no-undo.
def var v_val_transf              as decimal no-undo.
def var v_val_sdo_imob_cta        as decimal no-undo.
def var v_val_sdo_inicial_cta     as decimal no-undo.
def var v_val_salvage_value_cta   as decimal no-undo.

def var v_val_tot_dpr_amort_tot     as decimal no-undo.
def var v_val_tot_dpr_amort_ini     as decimal no-undo.
def var v_val_tot_dpr_amort_per     as decimal no-undo.
def var v_val_tot_adic              as decimal no-undo.
def var v_val_tot_bxa_imob_total    as decimal no-undo.
def var v_val_tot_bxa_imob_vendas   as decimal no-undo.
def var v_val_tot_bxa_imob_outros   as decimal no-undo.
def var v_val_tot_cm                as decimal no-undo.
def var v_val_tot_dpr_amort_bxa     as decimal no-undo.
def var v_val_tot_dpr_transf        as decimal no-undo.
def var v_val_tot_transf            as decimal no-undo.
def var v_val_tot_sdo_imob_cta      as decimal no-undo.
def var v_val_tot_sdo_inicial_cta   as decimal no-undo.
def var v_val_tot_salvage_value_cta as decimal no-undo.

def var v_val_emp_dpr_amort_tot     as decimal no-undo.
def var v_val_emp_dpr_amort_ini     as decimal no-undo.
def var v_val_emp_dpr_amort_per     as decimal no-undo.
def var v_val_emp_adic              as decimal no-undo.
def var v_val_emp_bxa_imob_total    as decimal no-undo.
def var v_val_emp_bxa_imob_vendas   as decimal no-undo.
def var v_val_emp_bxa_imob_outros   as decimal no-undo.
def var v_val_emp_cm                as decimal no-undo.
def var v_val_emp_dpr_amort_bxa     as decimal no-undo.
def var v_val_emp_dpr_transf        as decimal no-undo.
def var v_val_emp_transf            as decimal no-undo.
def var v_val_emp_sdo_imob_cta      as decimal no-undo.
def var v_val_emp_sdo_inicial_cta   as decimal no-undo.
def var v_val_emp_salvage_value_cta as decimal no-undo.

def var v_val_est_dpr_amort_tot     as decimal no-undo.
def var v_val_est_dpr_amort_ini     as decimal no-undo.
def var v_val_est_dpr_amort_per     as decimal no-undo.
def var v_val_est_adic              as decimal no-undo.
def var v_val_est_bxa_imob_total    as decimal no-undo.
def var v_val_est_bxa_imob_vendas   as decimal no-undo.
def var v_val_est_bxa_imob_outros   as decimal no-undo.
def var v_val_est_cm                as decimal no-undo.
def var v_val_est_dpr_amort_bxa     as decimal no-undo.
def var v_val_est_dpr_transf        as decimal no-undo.
def var v_val_est_transf            as decimal no-undo.
def var v_val_est_sdo_imob_cta      as decimal no-undo.
def var v_val_est_sdo_inicial_cta   as decimal no-undo.
def var v_val_est_salvage_value_cta as decimal no-undo.

def var v_wgh_focus as widget-handle no-undo.
/************************** Variable Definition End *************************/

/*************************** Menu Definition Begin **************************/
def menu      m_help                menubar
    menu-item mi_conteudo           label "&Conte£do"
    menu-item mi_sobre              label "&Sobre".



/**************************** Menu Definition End ***************************/

/************************ Rectangle Definition Begin ************************/

def rectangle rt_001
    size 1 by 1
    edge-pixels 2.
def rectangle rt_002
    size 1 by 1
    edge-pixels 2.
def rectangle rt_cxcf
    size 1 by 1
    fgcolor 1 edge-pixels 2.
def rectangle rt_dimensions
    size 1 by 1
    edge-pixels 2.
def rectangle rt_mold
    size 1 by 1
    edge-pixels 2.
def rectangle rt_order
    size 1 by 1
    edge-pixels 2.
def rectangle rt_parameters_label
    size 1 by 1
    edge-pixels 2.
def rectangle rt_run
    size 1 by 1
    edge-pixels 2.
def rectangle rt_select
    size 1 by 1
    edge-pixels 2.
def rectangle rt_target
    size 1 by 1
    edge-pixels 2.


/************************* Rectangle Definition End *************************/

/************************** Button Definition Begin *************************/

def button bt_can
    label "Cancela"
    tooltip "Cancela"
    size 1 by 1
    auto-endkey.
def button bt_close
    label "&Fecha"
    tooltip "Fecha"
    size 1 by 1
    auto-go.
def button bt_get_file
    label "Pesquisa Arquivo"
    tooltip "Pesquisa Arquivo"
&if "{&window-system}" <> "TTY" &then
    image-up file "image/im-sea1"
    image-insensitive file "image/ii-sea1"
&endif
    size 1 by 1.
def button bt_grupo
    label "Grupos"
    tooltip "Grupos de Contas Patrimoniais"
    size 1 by 1.
def button bt_hel2
    label "Ajuda"
    tooltip "Ajuda"
    size 1 by 1.
def button bt_print
    label "&Imprime"
    tooltip "Imprime"
    size 1 by 1
    auto-go.
def button bt_set_printer
    label "Define Impressora e Layout"
    tooltip "Define Impressora e Layout de Impress∆o"
&if "{&window-system}" <> "TTY" &then
    image-up file "image/im-setpr.bmp"
    image-insensitive file "image/ii-setpr"
&endif
    size 1 by 1.
/****************************** Function Button *****************************/
def button bt_zoom_cenar
    label "Zoom"
    tooltip "Zoom"
&if "{&window-system}" <> "TTY" &then
    image-up file "image/im-zoo"
    image-insensitive file "image/ii-zoo"
&endif
    size 4 by .88.
def button bt_zoom_finalid
    label "Zoom"
    tooltip "Zoom"
&if "{&window-system}" <> "TTY" &then
    image-up file "image/im-zoo"
    image-insensitive file "image/ii-zoo"
&endif
    size 4 by .88.


/*************************** Button Definition End **************************/

/************************** Editor Definition Begin *************************/

def var ed_1x40
    as character
    view-as editor no-word-wrap
    size 40 by 1
    bgcolor 15 font 2
    no-undo.


/*************************** Editor Definition End **************************/

/************************ Radio-Set Definition Begin ************************/

def var rs_cod_dwb_output
    as character
    initial "Terminal"
    view-as radio-set Horizontal
    radio-buttons "Terminal", "Terminal","Arquivo", "Arquivo","Impressora", "Impressora", "Excel", "Excel"
     /*l_terminal*/ /*l_terminal*/ /*l_file*/ /*l_file*/ /*l_printer*/ /*l_printer*/
    bgcolor 8 
    no-undo.
def var rs_ind_run_mode
    as character
    initial "On-Line"
    view-as radio-set Horizontal
    radio-buttons "On-Line", "On-Line","Batch", "Batch"
     /*l_online*/ /*l_online*/ /*l_batch*/ /*l_batch*/
    bgcolor 8 
    no-undo.


/************************* Radio-Set Definition End *************************/

/************************** Report Definition Begin *************************/

def new shared var v_rpt_s_1_lines as integer initial 66.
def new shared var v_rpt_s_1_columns as integer initial 255.
def new shared var v_rpt_s_1_bottom as integer initial 65.
def new shared var v_rpt_s_1_page as integer.
def new shared var v_rpt_s_1_name as character initial "Relat¢rio Movimentaá∆o Permanente".
def frame f_rpt_s_1_header_period header
    "------------------------------------------------------------" at 1
    "------------------------------------------------------------" at 61
    "------------------------------------------------------------" at 121
    "----------------------------------------------------------" at 181
    "--" at 239
    "P†gina: " at 242
    (page-number (s_1) + v_rpt_s_1_page) to 255 format ">>>>>9" skip
    v_nom_enterprise at 1 format "x(40)"
    v_nom_report_title at 216 format "x(40)" skip
    "Per°odo: " at 1
    v_dat_inic       at 10 format "99/99/9999"
    "A" at 21
    v_dat_fim        at 23 format "99/99/9999"
    "------------------------------------------------------------" at 34
    "------------------------------------------------------------" at 94
    "------------------------------------------------------------" at 154
    "--------------------" at 214
    "---" at 234
    v_dat_execution at 238 format "99/99/9999"
    "-" at 249
    v_hra_execution at 251 format "99:99" skip (1)
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_header_unique header
    "------------------------------------------------------------" at 1
    "------------------------------------------------------------" at 61
    "------------------------------------------------------------" at 121
    "----------------------------------------------------------" at 181
    "--" at 239
    "P†gina: " at 242
    (page-number (s_1) + v_rpt_s_1_page) to 255 format ">>>>>9" skip
    v_nom_enterprise at 1 format "x(40)"
    fill(" ", 40 - length(trim(v_nom_report_title))) + trim(v_nom_report_title) to 255 format "x(40)" skip
    "------------------------------------------------------------" at 1
    "------------------------------------------------------------" at 61
    "------------------------------------------------------------" at 121
    "--------------------------------------------------" at 181
    "------" at 231
    v_dat_execution at 238 format "99/99/9999"
    "-" at 249
    v_hra_execution at 251 format "99:99" skip (1)
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_footer_last_page header
    skip (1)
    "Èltima p†gina" at 1
    "---------------------------------------------------------" at 15
    "------------------------------------------------------------" at 72
    "------------------------------------------------------------" at 132
    "----------------------------------------" at 192
    v_nom_prog_ext at 233 format "x(8)"
    "-" at 242
    v_cod_release at 244 format "x(12)" skip
    with no-box no-labels width 255 page-bottom stream-io.
def frame f_rpt_s_1_footer_normal header
    skip (1)
    "---------------------------------------------------------" at 1
    "------------------------------------------------------------" at 58
    "------------------------------------------------------------" at 118
    "--------------------------------------------------" at 178
    "----" at 228
    v_nom_prog_ext at 233 format "x(8)"
    "-" at 242
    v_cod_release at 244 format "x(12)" skip
    with no-box no-labels width 255 page-bottom stream-io.

def frame f_rpt_s_1_footer_param_page header
    skip (1)
    "P†gina ParÉmetros" at 1
    "----------------------------------------------------" at 23
    "------------------------------------------------------------" at 76
    "------------------------------------------------------------" at 136
    "--------------------------------" at 196
    "----" at 228
    v_nom_prog_ext at 233 format "x(8)"
    "-" at 242
    v_cod_release at 244 format "x(12)" skip
    with no-box no-labels width 255 page-bottom stream-io.

/*************************** Report Definition End **************************/

/************************** Frame Definition Begin **************************/
def frame f_main
    rt_parameters_label
         at row 08.00 col 02.00
    " ParÉmetros " view-as text
         at row 07.70 col 04.00 bgcolor 8 
    rt_select
         at row 01.50 col 42.00
    " Seleá∆o " view-as text
         at row 01.20 col 44.00
    rt_order
         at row 01.50 col 02.00
    " Classificaá∆o " view-as text
         at row 01.20 col 04.00 bgcolor 8 
    rt_target
         at row 08.00 col 42.00
    " Destino " view-as text
         at row 07.70 col 44.00 bgcolor 8 
    rt_run
         at row 11.50 col 42.00
    " Execuá∆o " view-as text
         at row 11.20 col 44.00
    rt_dimensions
         at row 11.50 col 68.00
    " Dimens‰es " view-as text
         at row 11.20 col 70.00
    rt_cxcf
         at row 15.00 col 02.00 bgcolor 7 
    rs_cod_dwb_output
         at row 08.50 col 44.00
         help "" no-label
    ed_1x40
         at row 09.50 col 44.00
         help "" no-label
    bt_set_printer
         at row 09.50 col 83.00 font ?
         help "Define Impressora e Layout de Impress∆o"
    bt_get_file
         at row 09.50 col 83.00 font ?
         help "Pesquisa Arquivo"
    v_cod_empresa_ini
         at row  2 col 50 colon-aligned label "Empresa"
         help "Empresa Inicial"
         view-as fill-in
         size-chars 5 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_empresa_fim
         at row  2 col 65 colon-aligned label "atÇ"
         help "Empresa Final"
         view-as fill-in
         size-chars 5 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_estab_ini
         at row  3 col 50 colon-aligned label "Estabel."
         help "Estabelecimento Inicial"
         view-as fill-in
         size-chars 5 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_estab_fim
         at row  3 col 65 colon-aligned label "atÇ"
         help "Estabelecimento Final"
         view-as fill-in
         size-chars 5 by .88
         fgcolor ? bgcolor 15 font 2
    v_dat_inic
         at row 4 col 50.00 colon-aligned label "Per°odo"
         help "Data Movimento Bem Patrimonial"
         view-as fill-in
         size-chars 11.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_dat_fim
         at row 4 col 65.00 colon-aligned label "atÇ"
         help "Data Movimento Bem Patrimonial"
         view-as fill-in
         size-chars 11.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_cenar_ctbl
         at row 9.21 col 13.00 colon-aligned label "Cen†rio"
         help "Cen†rio Cont†bil"
         view-as fill-in
         size-chars 9.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_zoom_cenar
         at row 9.21 col 24.14
    v_cod_finalid_econ
         at row 10.21 col 13.00 colon-aligned label "Finalidade"
         help "Finalidade Econìmica"
         view-as fill-in
         size-chars 11.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_zoom_finalid
         at row 10.21 col 26.14
    bt_grupo
         at row 11.21 col 15 font ?
         help "Grupos de Contas Patrimoniais"
    rs_ind_run_mode
         at row 12.21 col 44.00
         help "" no-label
    v_qtd_line
         at row 12.21 col 79.00 colon-aligned
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_log_print_par
         at row 13.21 col 44.00 label "Imprime ParÉmetros"
         view-as toggle-box
    v_qtd_column
         at row 13.21 col 79.00 colon-aligned
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_close
         at row 15.25 col 03.00 font ?
         help "Fecha"
    bt_print
         at row 15.25 col 14.00 font ?
         help "Imprime"
    bt_can
         at row 15.25 col 25.00 font ?
         help "Cancela"
    bt_hel2
         at row 15.25 col 77.57 font ?
         help "Ajuda"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 90.00 by 17.00
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Raz∆o por Grupo de Contas - Espec°fico".

    /* adjust size of objects in this frame */
    assign bt_can:width-chars               in frame f_main = 10.00
           bt_can:height-chars              in frame f_main = 01.00
           bt_close:width-chars             in frame f_main = 10.00
           bt_close:height-chars            in frame f_main = 01.00
           bt_get_file:width-chars          in frame f_main = 04.00
           bt_get_file:height-chars         in frame f_main = 01.08
           bt_hel2:width-chars              in frame f_main = 10.00
           bt_hel2:height-chars             in frame f_main = 01.00
           bt_print:width-chars             in frame f_main = 10.00
           bt_print:height-chars            in frame f_main = 01.00
           bt_grupo:width-chars             in frame f_main = 10.00
           bt_grupo:height-chars            in frame f_main = 01.00
           bt_set_printer:width-chars       in frame f_main = 04.00
           bt_set_printer:height-chars      in frame f_main = 01.08
           ed_1x40:width-chars              in frame f_main = 38.00
           ed_1x40:height-chars             in frame f_main = 01.00
           rt_cxcf:width-chars              in frame f_main = 86.57
           rt_cxcf:height-chars             in frame f_main = 01.42
           rt_dimensions:width-chars        in frame f_main = 20.57
           rt_dimensions:height-chars       in frame f_main = 03.00
           rt_order:width-chars             in frame f_main = 39.00
           rt_order:height-chars            in frame f_main = 06.00
           rt_parameters_label:width-chars  in frame f_main = 39.00
           rt_parameters_label:height-chars in frame f_main = 06.50
           rt_run:width-chars               in frame f_main = 25.00
           rt_run:height-chars              in frame f_main = 03.00
           rt_select:width-chars            in frame f_main = 46.57
           rt_select:height-chars           in frame f_main = 06.00
           rt_target:width-chars            in frame f_main = 46.57
           rt_target:height-chars           in frame f_main = 03.00.

assign bt_zoom_cenar:sensitive in frame f_main = yes
       bt_zoom_finalid:sensitive in frame f_main = yes.

{include/i_fclfrm.i f_main }
/*************************** Frame Definition End ***************************/

/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.06':U &then
def var v_prog_filtro_pdf as handle no-undo.

function getCodTipoRelat returns character in v_prog_filtro_pdf.

run prgtec/btb/btb920aa.py persistent set v_prog_filtro_pdf.

run pi_define_objetos in v_prog_filtro_pdf (frame f_main:handle,
                       rs_cod_dwb_output:handle in frame f_main,
                       bt_get_file:row in frame f_main,
                       bt_get_file:col in frame f_main).

&endif
/* tech38629 - Fim da alteraá∆o */


/*********************** User Interface Trigger Begin ***********************/
ON CHOOSE OF bt_get_file IN FRAME f_main
DO:

    system-dialog get-file v_cod_dwb_file
        title "Imprimir" /*l_imprimir*/ 
        filters '*.rpt' '*.rpt',
                "*.*"   "*.*"
        save-as
        create-test-file
        ask-overwrite.
        assign dwb_rpt_param.cod_dwb_file             = v_cod_dwb_file
               ed_1x40:screen-value in frame f_main = v_cod_dwb_file.

END. /* ON CHOOSE OF bt_get_file IN FRAME f_main */

ON CHOOSE OF bt_grupo IN FRAME f_main DO:
    /***************************************************
     * Chama o programa padr∆o para seleá∆o dos grupos *
     ***************************************************/
    def var v_cod_usuar_aux as character no-undo.

    do on error undo, retry:
        assign v_cod_usuar_aux             = v_cod_usuar_corren
               v_cod_dwb_user              = "20100079"  
               v_cod_usuar_corren          = "20100079"
               v5_cod_usuar_corren         = "20100079"
               v_cod_usuar_corren_criptog  = encode("20100079")
               v5_cod_usuar_corren_criptog = encode("20100079").

        find dwb_rpt_param no-lock
             where dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
             and   dwb_rpt_param.cod_dwb_user    = v_cod_dwb_user no-error.
        if not avail dwb_rpt_param then do:
            create dwb_rpt_param.
            assign dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
                   dwb_rpt_param.cod_dwb_user    = v_cod_dwb_user.
        end.

        run prgfin/fas/fas360za.p.

        assign v_cod_usuar_corren          = v_cod_usuar_aux
               v_cod_dwb_user              = v_cod_usuar_aux
               v5_cod_usuar_corren         = v_cod_usuar_aux
               v_cod_usuar_corren_criptog  = encode(v_cod_usuar_aux)
               v5_cod_usuar_corren_criptog = encode(v_cod_usuar_aux).
    end.
END.

ON CHOOSE OF bt_hel2 IN FRAME f_main
DO:
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle).

END. /* ON CHOOSE OF bt_hel2 IN FRAME f_main */

ON CHOOSE OF bt_print IN FRAME f_main
DO:

do:
/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.06':U &then
    run pi_restricoes in v_prog_filtro_pdf (input rs_cod_dwb_output:screen-value in frame f_main).
    if return-value = 'nok' then 
        return no-apply.
&endif
/* tech38629 - Fim da alteraá∆o */
    assign v_log_print = yes.
end.

END. /* ON CHOOSE OF bt_print IN FRAME f_main */

ON CHOOSE OF bt_set_printer IN FRAME f_main
DO:

    assign v_nom_dwb_printer      = ""
           v_cod_dwb_print_layout = "".

    &if '{&emsbas_version}' <= '1.00' &then
    if  search("prgtec/btb/btb036nb.r") = ? and search("prgtec/btb/btb036nb.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgtec/btb/btb036nb.p".
        else do:
            message "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgtec/btb/btb036nb.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run prgtec/btb/btb036nb.p (output v_nom_dwb_printer,
                               output v_cod_dwb_print_layout) /*prg_see_layout_impres_imprsor*/.
    &else
    if  search("prgtec/btb/btb036zb.r") = ? and search("prgtec/btb/btb036zb.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgtec/btb/btb036zb.p".
        else do:
            message "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgtec/btb/btb036zb.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run prgtec/btb/btb036zb.p (input-output v_nom_dwb_printer,
                               input-output v_cod_dwb_print_layout,
                               input-output v_nom_dwb_print_file) /*prg_fnc_layout_impres_imprsor*/.
    &endif

    if  v_nom_dwb_printer <> ""
    and  v_cod_dwb_print_layout <> ""
    then do:
        assign dwb_rpt_param.nom_dwb_printer      = v_nom_dwb_printer
               dwb_rpt_param.cod_dwb_print_layout = v_cod_dwb_print_layout
    &if '{&emsbas_version}' > '1.00' &then           
    &if '{&emsbas_version}' >= '5.03' &then           
               dwb_rpt_param.nom_dwb_print_file        = v_nom_dwb_print_file
    &else
               dwb_rpt_param.cod_livre_1               = v_nom_dwb_print_file
    &endif
    &endif
               ed_1x40:screen-value in frame f_main = v_nom_dwb_printer
                                                       + ":"
                                                       + v_cod_dwb_print_layout
    &if '{&emsbas_version}' > '1.00' &then
                                                       + (if v_nom_dwb_print_file <> "" then ":" + v_nom_dwb_print_file
                                                          else "")
    &endif
    .
        find layout_impres no-lock
             where layout_impres.nom_impressora = dwb_rpt_param.nom_dwb_printer
               and layout_impres.cod_layout_impres = dwb_rpt_param.cod_dwb_print_layout /*cl_get_layout of layout_impres*/ no-error.
        assign v_qtd_line               = layout_impres.num_lin_pag.
        display v_qtd_line
                with frame f_main.
    end /* if */.

END. /* ON CHOOSE OF bt_set_printer IN FRAME f_main */

ON LEAVE OF ed_1x40 IN FRAME f_main
DO:

    /************************* Variable Definition Begin ************************/

    def var v_cod_filename_final             as character       no-undo. /*local*/
    def var v_cod_filename_initial           as character       no-undo. /*local*/


    /************************** Variable Definition End *************************/

    block:
    do with frame f_main:
        if  rs_cod_dwb_output:screen-value = "Arquivo" /*l_file*/ 
        then do:
            if  rs_ind_run_mode:screen-value <> "Batch" /*l_batch*/ 
            then do:
                if  ed_1x40:screen-value <> ""
                then do:
                    assign ed_1x40:screen-value   = replace(ed_1x40:screen-value, '~\', '/')
                           v_cod_filename_initial = entry(num-entries(ed_1x40:screen-value, '/'), ed_1x40:screen-value, '/')
                           v_cod_filename_final   = substring(ed_1x40:screen-value, 1,
                                                              length(ed_1x40:screen-value) - length(v_cod_filename_initial) - 1)
                           file-info:file-name    = v_cod_filename_final.
                    if  file-info:file-type = ?
                    then do:
                         /* O diret¢rio &1 n∆o existe ! */
                         run pi_messages (input "show",
                                          input 4354,
                                          input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                             v_cod_filename_final)) /*msg_4354*/.
                         return no-apply.
                    end /* if */.
                end /* if */.
            end /* if */.

            find first dwb_rpt_param exclusive-lock where
                 dwb_rpt_param.cod_dwb_user    = v_cod_usuar_corren and
                 dwb_rpt_param.cod_dwb_program = v_cod_dwb_program  no-error.

            assign dwb_rpt_param.cod_dwb_file = ed_1x40:screen-value.
        end /* if */.
    end /* do block */.

END. /* ON LEAVE OF ed_1x40 IN FRAME f_main */

ON VALUE-CHANGED OF rs_cod_dwb_output IN FRAME f_main DO:
    initout:
    do with frame f_main:
        /* block: */
        case self:screen-value:
            when "Terminal" /*l_terminal*/ then ter:
             do:
                if  rs_cod_dwb_output <> "Impressora" /*l_printer*/ 
                then do:
                    assign v_qtd_line_ant = input frame f_main v_qtd_line.
                end /* if */.
                if  v_qtd_line_ant > 0
                then do:
                    assign v_qtd_line = v_qtd_line_ant.
                end /* if */.
                else do:
                    assign v_qtd_line = (if  dwb_rpt_param.qtd_dwb_line > 0 then dwb_rpt_param.qtd_dwb_line
                                        else v_rpt_s_1_lines).
                end /* else */.
                display v_qtd_line
                        with frame f_main.
                assign ed_1x40:screen-value   = ""
                       ed_1x40:sensitive      = no
                       bt_get_file:visible    = no
                       bt_set_printer:visible = no.
            end /* do ter */.
            when "Arquivo" /*l_file*/ then fil:
             do:
                if  rs_cod_dwb_output <> "Impressora" /*l_printer*/ 
                then do:
                    assign v_qtd_line_ant = input frame f_main v_qtd_line.
                end /* if */.
                if  v_qtd_line_ant > 0
                then do:
                    assign v_qtd_line = v_qtd_line_ant.
                end /* if */.
                else do:
                    assign v_qtd_line = (if  dwb_rpt_param.qtd_dwb_line > 0 then dwb_rpt_param.qtd_dwb_line
                                        else v_rpt_s_1_lines).
                end /* else */.
                display v_qtd_line
                        with frame f_main.
                assign ed_1x40:screen-value   = ""
                       ed_1x40:sensitive      = yes
                       bt_set_printer:visible = no
                       bt_get_file:visible    = yes.

                /* define arquivo default */
                find usuar_mestre no-lock
                     where usuar_mestre.cod_usuario = v_cod_dwb_user use-index srmstr_id no-error.
    
                do  transaction:                
                    find first dwb_rpt_param
                        where dwb_rpt_param.cod_dwb_user    = v_cod_usuar_corren
                        and   dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
                        exclusive-lock no-error.

                    assign dwb_rpt_param.cod_dwb_file = "".

                    if  rs_ind_run_mode:screen-value in frame f_main <> "Batch" /*l_batch*/ 
                    then do:
                        if  usuar_mestre.nom_dir_spool <> ""
                        then do:
                            assign dwb_rpt_param.cod_dwb_file = usuar_mestre.nom_dir_spool
                                                              + "~/".
                        end /* if */.
                        if  usuar_mestre.nom_subdir_spool <> ""
                        then do:
                            assign dwb_rpt_param.cod_dwb_file = dwb_rpt_param.cod_dwb_file
                                                              + usuar_mestre.nom_subdir_spool
                                                              + "~/".
                        end /* if */.
                    end /* if */.
                    else do:
                        assign dwb_rpt_param.cod_dwb_file = dwb_rpt_param.cod_dwb_file.
                    end /* else */.
                    if  v_cod_dwb_file_temp = ""
                    then do:
                        assign dwb_rpt_param.cod_dwb_file = dwb_rpt_param.cod_dwb_file
                                                          + caps("fas360aa":U)
                                                          + '.rpt'.
                    end /* if */.
                    else do:
                        assign dwb_rpt_param.cod_dwb_file = dwb_rpt_param.cod_dwb_file
                                                          + v_cod_dwb_file_temp.
                    end /* else */.
                    assign ed_1x40:screen-value               = dwb_rpt_param.cod_dwb_file
                           dwb_rpt_param.cod_dwb_print_layout = ""
                           v_qtd_line                         = (if v_qtd_line_ant > 0 then v_qtd_line_ant
                                                                 else v_rpt_s_1_lines)
    &if '{&emsbas_version}' > '1.00' &then
                           v_nom_dwb_print_file               = ""
    &endif
    .
                end.     
            end /* do fil */.
            when "Impressora" /*l_printer*/ then prn:
             do:
                if  rs_cod_dwb_output <> "Impressora" /*l_printer*/  /* and rs_ind_run_mode <> "Batch" /*l_batch*/  */
                then do: 
                    assign v_qtd_line_ant = input frame f_main v_qtd_line.
                end /* if */.

                assign ed_1x40:sensitive        = no
                       bt_get_file:visible      = no
                       bt_set_printer:visible   = yes
                       bt_set_printer:sensitive = yes.

                /* define layout default */
                if  dwb_rpt_param.nom_dwb_printer = ""
                or  dwb_rpt_param.cod_dwb_print_layout = ""
                then do:
                    run pi_set_print_layout_default /*pi_set_print_layout_default*/.
                end /* if */.
                else do:
                    assign ed_1x40:screen-value = dwb_rpt_param.nom_dwb_printer
                                                + ":"
                                                + dwb_rpt_param.cod_dwb_print_layout.
                end /* else */.
                find layout_impres no-lock
                     where layout_impres.nom_impressora = dwb_rpt_param.nom_dwb_printer
                       and layout_impres.cod_layout_impres = dwb_rpt_param.cod_dwb_print_layout /*cl_get_layout of layout_impres*/ no-error.
                if  avail layout_impres
                then do:
                    assign v_qtd_line               = layout_impres.num_lin_pag.
                end /* if */.
                display v_qtd_line
                        with frame f_main.
            end /* do prn */.
            when "Excel" then do:
                assign ed_1x40:screen-value   = "esfas001ya.xls"
                       ed_1x40:sensitive      = no
                       bt_get_file:visible    = no
                       bt_set_printer:visible = no.
            END.
        end /* case block */.

        assign v_cod_dwb_file_temp = replace(dwb_rpt_param.cod_dwb_file, "~\", "~/").
        if  index(v_cod_dwb_file_temp, "~/") <> 0
        then do:
            assign v_cod_dwb_file_temp = substring(v_cod_dwb_file_temp, r-index(v_cod_dwb_file_temp, "~/") + 1).
        end /* if */.
        else do:
            assign v_cod_dwb_file_temp = dwb_rpt_param.cod_dwb_file.
        end /* else */.
    end /* do initout */.

    if  self:screen-value = "Impressora" /*l_printer*/ 
    then do:
        disable v_qtd_line
                with frame f_main.
    end /* if */.
    else do:
        enable v_qtd_line
               with frame f_main.
    end /* else */.
    assign rs_cod_dwb_output.
END. /* ON VALUE-CHANGED OF rs_cod_dwb_output IN FRAME f_main */

ON VALUE-CHANGED OF rs_ind_run_mode IN FRAME f_main
DO:

    do  transaction:
        find first dwb_rpt_param
            where dwb_rpt_param.cod_dwb_user    = v_cod_usuar_corren
            and  dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
            exclusive-lock no-error.
        assign dwb_rpt_param.ind_dwb_run_mode = input frame f_main rs_ind_run_mode.

        if  dwb_rpt_param.ind_dwb_run_mode = "Batch" /*l_batch*/ 
        then do:
            if  rs_cod_dwb_output:disable("Terminal" /*l_terminal*/ ) in frame f_main
            then do:
            end /* if */.
            if  rs_cod_dwb_output:disable("Excel" /*l_Excel*/ ) in frame f_main
            then do:
            end /* if */.
        end /* if */.
        else do:
            if  rs_cod_dwb_output:enable("Terminal" /*l_terminal*/ ) in frame f_main
            then do:
            end /* if */.
            if  rs_cod_dwb_output:enable("Excel" /*l_Excel*/ ) in frame f_main
            then do:
            end /* if */.
        end /* else */.
        if  rs_ind_run_mode = "Batch" /*l_batch*/ 
        then do:
           assign v_qtd_line = v_qtd_line_ant.
           display v_qtd_line
                   with frame f_main.
        end /* if */.
        assign rs_ind_run_mode.
        apply "value-changed" to rs_cod_dwb_output in frame f_main.
    end.    

END. /* ON VALUE-CHANGED OF rs_ind_run_mode IN FRAME f_main */


/************************ User Interface Trigger End ************************/

/************************** Function Trigger Begin **************************/


ON  CHOOSE OF bt_zoom_cenar IN FRAME f_main
OR F5 OF v_cod_cenar_ctbl IN FRAME f_main DO:

    /* fn_generic_zoom_variable */
    if  search("prgint/utb/utb076ka.r") = ? and search("prgint/utb/utb076ka.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb076ka.p".
        else do:
            message "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb076ka.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run prgint/utb/utb076ka.p /*prg_sea_cenar_ctbl*/.
    if  v_rec_cenar_ctbl <> ?
    then do:
        find cenar_ctbl where recid(cenar_ctbl) = v_rec_cenar_ctbl no-lock no-error.
        assign v_cod_cenar_ctbl:screen-value in frame f_main =
               string(cenar_ctbl.cod_cenar_ctbl).

        apply "entry" to v_cod_cenar_ctbl in frame f_main.
    end /* if */.

end. /* ON  CHOOSE OF bt_zoom_cenar IN FRAME f_main */

ON  CHOOSE OF bt_zoom_finalid IN FRAME f_main
OR F5 OF v_cod_finalid_econ IN FRAME f_main DO:

    /* fn_generic_zoom_variable */
    if  search("prgint/utb/utb077ka.r") = ? and search("prgint/utb/utb077ka.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb077ka.p".
        else do:
            message "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb077ka.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run prgint/utb/utb077ka.p /*prg_sea_finalid_econ*/.
    if  v_rec_finalid_econ <> ?
    then do:
        find finalid_econ where recid(finalid_econ) = v_rec_finalid_econ no-lock no-error.
        assign v_cod_finalid_econ:screen-value in frame f_main =
               string(finalid_econ.cod_finalid_econ).

        apply "entry" to v_cod_finalid_econ in frame f_main.
    end /* if */.

end. /* ON  CHOOSE OF bt_zoom_finalid IN FRAME f_main */


/*************************** Function Trigger End ***************************/

/**************************** Frame Trigger Begin ***************************/
ON GO OF FRAME f_main
DO:

    do transaction:
        find first dwb_rpt_param
            where dwb_rpt_param.cod_dwb_user    = v_cod_usuar_corren
            and   dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
            exclusive-lock no-error.
        assign dwb_rpt_param.cod_dwb_output     = rs_cod_dwb_output:screen-value in frame f_main
               dwb_rpt_param.qtd_dwb_line       = input frame f_main v_qtd_line
               dwb_rpt_param.nom_dwb_print_file = v_nom_dwb_print_file.

        if  dwb_rpt_param.cod_dwb_output = "Arquivo" /*l_file*/ 
        then do:
             run pi_filename_validation (Input dwb_rpt_param.cod_dwb_file) /*pi_filename_validation*/.
             if  dwb_rpt_param.ind_dwb_run_mode <> "Batch" /*l_batch*/ 
             then do:
                 if  index  ( dwb_rpt_param.cod_dwb_file ,'~\') <> 0
                 then do:
                      assign file-info:file-name= substring( dwb_rpt_param.cod_dwb_file ,
                                                             1,
                                                             r-index  ( dwb_rpt_param.cod_dwb_file ,'~\') - 1
                                                            ).
                 end /* if */.
                 else do:
                      assign file-info:file-name= substring( dwb_rpt_param.cod_dwb_file ,
                                                             1,
                                                             r-index  ( dwb_rpt_param.cod_dwb_file ,'/') - 1
                                                            ).
                 end /* else */.
                 if  (  file-info:file-type = ? )
                 and    (  index  ( dwb_rpt_param.cod_dwb_file ,'~\') <> 0
                              or
                           index  ( dwb_rpt_param.cod_dwb_file ,'/')  <> 0
                              or
                           index  ( dwb_rpt_param.cod_dwb_file ,':')  <> 0
                         )
                 then do:
                     /* O diret¢rio &1 n∆o existe ! */
                     run pi_messages (input "show",
                                      input 4354,
                                      input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                         file-info:file-name)) /*msg_4354*/.
                     return no-apply.
                  end /* if */.
             end /* if */.
             if  return-value = "NOK" /*l_nok*/ 
             then do:
                 /* Nome do arquivo incorreto ! */
                 run pi_messages (input "show",
                                  input 1064,
                                  input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_1064*/.
                 return no-apply.
             end /* if */.
        end /* if */.
        else do:
            if  dwb_rpt_param.cod_dwb_output = "Impressora" /*l_printer*/ 
            then do:
                if  not avail layout_impres
                then do:
                   /* Layout de impress∆o inexistente ! */
                   run pi_messages (input "show",
                                    input 4366,
                                    input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_4366*/.
                   return no-apply.
                end /* if */.
                if  dwb_rpt_param.nom_dwb_printer = ""
                or   dwb_rpt_param.cod_dwb_print_layout = ""
                then do:
                    /* Impressora destino e layout de impress∆o n∆o definidos ! */
                    run pi_messages (input "show",
                                     input 2052,
                                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_2052*/.
                    return no-apply.
                end /* if */.
            end /* if */.
        end /* else */.
    end.    

END. /* ON GO OF FRAME f_main */

ON HELP OF FRAME f_main ANYWHERE
DO:


    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */

END. /* ON HELP OF FRAME f_main */

ON RIGHT-MOUSE-DOWN OF FRAME f_main ANYWHERE
DO:

    /************************* Variable Definition Begin ************************/

    def var v_wgh_frame
        as widget-handle
        format ">>>>>>9":U
        no-undo.


    /************************** Variable Definition End *************************/


    /* Begin_Include: i_right_mouse_down_dialog_box */
    if  (self:type <> "DIALOG-BOX" /*l_dialog_box*/ )
    and (self:type <> "FRAME" /*l_frame*/      )
    and (self:type <> "text" /*l_text*/       )
    and (self:type <> "IMAGE" /*l_image*/      )
    and (self:type <> "RECTANGLE" /*l_rectangle*/  )
    then do:

        assign v_wgh_frame = self:parent.

        if  self:type        = "fill-in" /*l_fillin*/ 
        and v_wgh_frame:type = "Browse" /*l_browse*/  then
            return no-apply.

        if  valid-handle(self:popup-menu) = yes then
            return no-apply.

        assign v_wgh_frame = self:frame.

        if  (v_wgh_frame:type <> "DIALOG-BOX" /*l_dialog_box*/ ) and (v_wgh_frame:frame <> ?)
        then do:
               assign v_wgh_frame     = v_wgh_frame:frame.
        end /* if */.
        assign v_nom_title_aux    = v_wgh_frame:title
               v_wgh_frame:title  = self:help.
    end /* if */.
    /* End_Include: i_right_mouse_down_dialog_box */

END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_main */

ON RIGHT-MOUSE-UP OF FRAME f_main ANYWHERE
DO:

    /************************* Variable Definition Begin ************************/

    def var v_wgh_frame
        as widget-handle
        format ">>>>>>9":U
        no-undo.


    /************************** Variable Definition End *************************/


    /* Begin_Include: i_right_mouse_up_dialog_box */
    if  (self:type <> "DIALOG-BOX" /*l_dialog_box*/ )
    and (self:type <> "FRAME" /*l_frame*/      )
    and (self:type <> "text" /*l_text*/       )
    and (self:type <> "IMAGE" /*l_image*/      )
    and (self:type <> "RECTANGLE" /*l_rectangle*/  )
    then do:

        assign v_wgh_frame = self:parent.

        if  self:type        = "fill-in" /*l_fillin*/ 
        and v_wgh_frame:type = "Browse" /*l_browse*/  then
            return no-apply.

        if  valid-handle(self:popup-menu) = yes then
            return no-apply.

        assign v_wgh_frame        = self:frame.
        if  (v_wgh_frame:type <> "DIALOG-BOX" /*l_dialog_box*/ ) and (v_wgh_frame:frame <> ?)
        then do:
               assign v_wgh_frame     = v_wgh_frame:frame.
        end /* if */.
        assign v_wgh_frame:title  = v_nom_title_aux.
    end /* if */.

    /* End_Include: i_right_mouse_up_dialog_box */

END. /* ON RIGHT-MOUSE-UP OF FRAME f_main */

ON WINDOW-CLOSE OF FRAME f_main
DO:

    apply "end-error" to self.
END. /* ON WINDOW-CLOSE OF FRAME f_main */


/***************************** Frame Trigger End ****************************/

/**************************** Menu Trigger Begin ****************************/


ON CHOOSE OF MENU-ITEM mi_conteudo IN MENU m_help
DO:


        apply "choose" to bt_hel2 in frame f_main.





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


        assign v_nom_prog     = substring(frame f_main:title, 1, max(1, length(frame f_main:title) - 10)).
        if  v_nom_prog = ? then
            assign v_nom_prog = "".

        assign v_nom_prog     = v_nom_prog
                              + chr(10)
                              + "rpt_razao_grp_cta_espec":U.




    assign v_nom_prog_ext = "prgfin/fas/esfas001ya.p":U
           v_cod_release  = trim(" 5.06.00.000":U).
    run prgtec/btb/btb901zb.p (Input v_nom_prog,
                               Input v_nom_prog_ext,
                               Input v_cod_release) /*prg_fnc_about*/.
END. /* ON CHOOSE OF MENU-ITEM mi_sobre IN MENU m_help */


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

if  v_cod_arq <> '' and v_cod_arq <> ?
then do:
    run pi_version_extract ('rpt_razao_grp_cta_espec':U, 'prgfin/fas/esfas001ya.p':U, '5.06.00.000':U, 'pro':U).
end /* if */.
/* End_Include: i_version_extract */

run pi_return_user (output v_cod_dwb_user) /*pi_return_user*/.

if  search("prgtec/btb/btb906za.r") = ? and search("prgtec/btb/btb906za.py") = ? then do:
    if  v_cod_dwb_user begins 'es_' then
        return "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgtec/btb/btb906za.py".
    else do:
        message "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgtec/btb/btb906za.py"
               view-as alert-box error buttons ok.
        stop.
    end.
end.
else
    run prgtec/btb/btb906za.py /*prg_fnc_verify_controls*/.
if (v_cod_dwb_user = "") then
   assign v_cod_dwb_user = v_cod_usuar_corren.


/* Begin_Include: i_verify_security */
if  search("prgtec/men/men901za.r") = ? and search("prgtec/men/men901za.py") = ? then do:
    if  v_cod_dwb_user begins 'es_' then
        return "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgtec/men/men901za.py".
    else do:
        message "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgtec/men/men901za.py"
               view-as alert-box error buttons ok.
        return.
    end.
end.
else
    run prgtec/men/men901za.py (Input 'rpt_razao_grp_cta_espec') /*prg_fnc_verify_security*/.
if  return-value = "2014"
then do:
    run pi_messages (input "show",
                     input 2014,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'rpt_razao_grp_cta_espec')) /*msg_2014*/.
    return.
end /* if */.
if  return-value = "2012"
then do:
    /* Usu†rio sem permiss∆o para acessar o programa. */
    run pi_messages (input "show",
                     input 2012,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'rpt_razao_grp_cta_espec')) /*msg_2012*/.
    return.
end /* if */.
/* End_Include: i_verify_security */



/* Begin_Include: i_log_exec_prog_dtsul_ini */
assign v_rec_log = ?.

if can-find(prog_dtsul
       where prog_dtsul.cod_prog_dtsul = 'rpt_razao_grp_cta_espec' 
         and prog_dtsul.log_gera_log_exec = yes) then do transaction:
    create log_exec_prog_dtsul.
    assign log_exec_prog_dtsul.cod_prog_dtsul           = 'rpt_razao_grp_cta_espec'
           log_exec_prog_dtsul.cod_usuario              = v_cod_usuar_corren
           log_exec_prog_dtsul.dat_inic_exec_prog_dtsul = today
           log_exec_prog_dtsul.hra_inic_exec_prog_dtsul = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":","").
    assign v_rec_log = recid(log_exec_prog_dtsul).
    release log_exec_prog_dtsul no-error.
end.


/* End_Include: i_log_exec_prog_dtsul_ini */

/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.06':U &then
run pi_permissoes in v_prog_filtro_pdf (input 'rpt_razao_grp_cta_espec':U).
&endif
/* tech38629 - Fim da alteraá∆o */

assign frame f_main:title = frame f_main:title
                            + chr(32)
                            + chr(40)
                            + trim(" 5.06.00.000":U)
                            + chr(41).
/* menu pop-up de ajuda e sobre */
assign menu m_help:popup-only = yes
       bt_hel2:popup-menu in frame f_main = menu m_help:handle.


find ems5.empresa no-lock
     where empresa.cod_empresa = v_cod_empres_usuar /*cl_empres_usuar of empresa*/ no-error.
find first dwb_rpt_param
     where dwb_rpt_param.cod_dwb_program = "rel_razao_grupo_contas_espec":U
       and dwb_rpt_param.cod_dwb_user    = v_cod_dwb_user
       no-lock no-error.
if  avail dwb_rpt_param then do:
    assign v_nom_dwb_print_file = dwb_rpt_param.nom_dwb_print_file.

    if  dwb_rpt_param.qtd_dwb_line <> 0 then
        assign v_qtd_line = dwb_rpt_param.qtd_dwb_line.
    else
        assign v_qtd_line = v_rpt_s_1_lines.
end.

assign v_cod_dwb_proced   = "rel_razao_grupo_contas_espec"
       v_cod_dwb_program  = "rel_razao_grupo_contas_espec"
       v_cod_release      = trim(" 5.06.00.000":U)
       v_ind_dwb_run_mode = "On-Line" /*l_online*/ 
       v_qtd_column       = v_rpt_s_1_columns
       v_qtd_bottom       = v_rpt_s_1_bottom.

if (avail empresa) then
    assign v_nom_enterprise   = empresa.nom_razao_social.
else
    assign v_nom_enterprise   = 'Yamana'.

find first dwb_rpt_param no-lock
     where dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
     and   dwb_rpt_param.cod_dwb_user    = v_cod_dwb_user no-error.
if  avail dwb_rpt_param then do:
    for each dwb_rpt_select no-lock
        where dwb_rpt_select.cod_dwb_program = dwb_rpt_param.cod_dwb_program
        and   dwb_rpt_select.cod_dwb_user    = dwb_rpt_param.cod_dwb_user:

        if not can-find(first grp_cta_pat_usuar no-lock
            where grp_cta_pat_usuar.cod_grp_cta_pat_usuar = dwb_rpt_select.cod_dwb_field) then do:

            create grp_cta_pat_usuar.
            assign grp_cta_pat_usuar.cod_grp_cta_pat_usuar = dwb_rpt_select.cod_dwb_field
                   grp_cta_pat_usuar.des_grp_cta_pat_usuar = dwb_rpt_select.cod_dwb_initial.
        end.

        if  dwb_rpt_select.cod_dwb_final <> "" then do:
            find first compos_cta_pat_usuar exclusive-lock
                where compos_cta_pat_usuar.cod_grp_cta_pat_usuar = dwb_rpt_select.cod_dwb_field
                and   compos_cta_pat_usuar.cod_cta_pat           = dwb_rpt_select.cod_dwb_final no-error.
            if not avail compos_cta_pat_usuar then do:    
                create compos_cta_pat_usuar.
                assign compos_cta_pat_usuar.cod_grp_cta_pat_usuar = dwb_rpt_select.cod_dwb_field
                       compos_cta_pat_usuar.cod_cta_pat           = dwb_rpt_select.cod_dwb_final.
            end.
        end.    
    end.
end.

if  v_cod_dwb_user begins 'es_' then do:
    find first dwb_rpt_param no-lock
         where dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
           and dwb_rpt_param.cod_dwb_user = v_cod_dwb_user no-error.
    if (not avail dwb_rpt_param) then
        return "ParÉmetros para o relat¢rio n∆o encontrado." /*1993*/ + " (" + "1993" + ")" + chr(10) + "N∆o foi poss°vel encontrar os parÉmetros necess†rios para a impress∆o do relat¢rio para o programa e usu†rio corrente." /*1993*/.
    if index( dwb_rpt_param.cod_dwb_file ,'~\') <> 0 then
        assign file-info:file-name = replace(dwb_rpt_param.cod_dwb_file, '~\', '~/').
    else
        assign file-info:file-name = dwb_rpt_param.cod_dwb_file.

    assign file-info:file-name = substring(file-info:file-name, 1,
                                           r-index(file-info:file-name, '~/') - 1).
    if  dwb_rpt_param.cod_dwb_output = "Arquivo" /*l_file*/ 
    then do:
       if file-info:file-type = ? then
          return "Diret¢rio Inexistente:" /*l_directory*/  + dwb_rpt_param.cod_dwb_file.
    end /* if */.

    find ped_exec no-lock
         where ped_exec.num_ped_exec = v_num_ped_exec_corren /*cl_le_ped_exec_global of ped_exec*/ no-error.
    if (ped_exec.cod_release_prog_dtsul <> trim(" 5.06.00.000":U)) then
        return "Vers‰es do programa diferente." /*1994*/ + " (" + "1994" + ")" + chr(10)
                                     + substitute("A vers∆o do programa (&3) que gerou o pedido de execuá∆o batch (&1) Ç diferente da vers∆o do programa que deveria executar o pedido batch (&2)." /*1994*/,ped_exec.cod_release_prog_dtsul,
                                                  trim(" 5.06.00.000":U),
                                                  "prgfin/fas/esfas001ya.p":U).
    assign v_nom_prog_ext     = caps("esfas001ya":U)
           v_dat_execution    = today
           v_hra_execution    = replace(string(time, "hh:mm:ss" /*l_hh:mm:ss*/ ), ":", "")
           v_cod_dwb_file     = dwb_rpt_param.cod_dwb_file
           v_nom_report_title = fill(" ", 40 - length(v_rpt_s_1_name)) + v_rpt_s_1_name
           v_ind_dwb_run_mode = "Batch" /*l_batch*/ .

    if num-entries( dwb_rpt_param.cod_dwb_parameters , chr(10) ) = 6  then do:
        assign v_cod_empresa_ini     = entry(1,entry(1, dwb_rpt_param.cod_dwb_parameters, chr(10)))
               v_cod_empresa_fim     = entry(2,entry(1, dwb_rpt_param.cod_dwb_parameters, chr(10)))
               v_cod_estab_ini       = entry(1,entry(2, dwb_rpt_param.cod_dwb_parameters, chr(10)))
               v_cod_estab_fim       = entry(2,entry(2, dwb_rpt_param.cod_dwb_parameters, chr(10)))
               v_dat_inic            = date(entry(3, dwb_rpt_param.cod_dwb_parameters, chr(10)))
               v_dat_fim             = date(entry(4, dwb_rpt_param.cod_dwb_parameters, chr(10)))
               v_cod_cenar_ctbl      = entry(5, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_cod_finalid_econ    = entry(6, dwb_rpt_param.cod_dwb_parameters, chr(10)).
    end.
    
    /* configura e define destino de impress∆o */
    if (dwb_rpt_param.cod_dwb_output = "Impressora" /*l_printer*/ ) then
        assign v_qtd_line_ant = v_qtd_line.

    IF dwb_rpt_param.cod_dwb_output <> "Excel" THEN DO:
        run pi_output_reports /*pi_output_reports*/.

        if  dwb_rpt_param.log_dwb_print_parameters = yes then do:
            if (page-number (s_1) > 0) then
                page stream s_1.

            hide stream s_1 frame f_rpt_s_1_header_period.
            view stream s_1 frame f_rpt_s_1_header_unique.
            hide stream s_1 frame f_rpt_s_1_footer_last_page.
            hide stream s_1 frame f_rpt_s_1_footer_normal.
            view stream s_1 frame f_rpt_s_1_footer_param_page.
            if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                page stream s_1.
            put stream s_1 unformatted 
                skip (1)
                "Usu†rio: " at 1
                v_cod_usuar_corren at 10 format "x(12)" skip (1).

            if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                page stream s_1.
            put stream s_1 unformatted 
                "  Per°odo: " at 2
                v_dat_inic at 13 format "99/99/9999"
                "atÇ: " at 24
                v_dat_fim at 29 format "99/99/9999" skip
                "Cen†rio: " at 4
                v_cod_cenar_ctbl at 13 format "x(8)" skip
                "Finalidade: " at 1
                v_cod_finalid_econ at 13 format "x(10)" skip.
        end /* if */.

        output stream s_1 close.

        /* tech38629 - Alteraá∆o efetuada via filtro */
        &if '{&emsbas_version}':U >= '5.06':U &then
        run pi_call_convert_object in v_prog_filtro_pdf (input yes,
                                                         input dwb_rpt_param.cod_dwb_output,
                                                         input dwb_rpt_param.nom_dwb_print_file,
                                                         input v_cod_dwb_file,
                                                         input v_nom_report_title).
        &endif
        /* tech38629 - Fim da alteraá∆o */

        &if '{&emsbas_version}':U >= '5.06':U &then
            if ((dwb_rpt_param.cod_dwb_output = 'Impressora' or dwb_rpt_param.cod_dwb_output = 'Impresora' or dwb_rpt_param.cod_dwb_output = 'printer') and getCodTipoRelat() = 'PDF':U) then do:
                if dwb_rpt_param.nom_dwb_print_file = '' then
                    run pi_print_pdf_file in v_prog_filtro_pdf (input yes).
            end.
        &endif
    END.

    return "OK" /*l_ok*/ .

end /* if */.

pause 0 before-hide.
view frame f_main.

super_block:
repeat
    on stop undo super_block, retry super_block:

    if (retry) then
       output stream s_1 close.

    param_block:
    do transaction:

        find dwb_rpt_param exclusive-lock
             where dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
               and dwb_rpt_param.cod_dwb_user    = v_cod_dwb_user no-error.
        if  not available dwb_rpt_param
        then do:
            create dwb_rpt_param.
            assign dwb_rpt_param.cod_dwb_program         = v_cod_dwb_program
                   dwb_rpt_param.cod_dwb_user            = v_cod_dwb_user
                   dwb_rpt_param.cod_dwb_parameters      = v_cod_dwb_parameters
                   dwb_rpt_param.cod_dwb_output          = "Terminal" /*l_terminal*/ 
                   dwb_rpt_param.ind_dwb_run_mode        = "On-Line" /*l_online*/ 
                   dwb_rpt_param.cod_dwb_file            = ""
                   dwb_rpt_param.nom_dwb_printer         = ""
                   dwb_rpt_param.cod_dwb_print_layout    = ""
                   v_cod_dwb_file_temp                   = "".
        end /* if */.
        assign v_qtd_line = (if dwb_rpt_param.qtd_dwb_line <> 0 then dwb_rpt_param.qtd_dwb_line else v_rpt_s_1_lines).
    end /* do param_block */.

    init:
    do with frame f_main:
        assign rs_cod_dwb_output:screen-value   = dwb_rpt_param.cod_dwb_output
               rs_ind_run_mode:screen-value     = dwb_rpt_param.ind_dwb_run_mode.

        if  dwb_rpt_param.cod_dwb_output = "Arquivo" then do:
            assign v_cod_dwb_file_temp = replace(dwb_rpt_param.cod_dwb_file, "~\", "~/").
            if (index(v_cod_dwb_file_temp, "~/") <> 0) then
                assign v_cod_dwb_file_temp = substring(v_cod_dwb_file_temp, r-index(v_cod_dwb_file_temp, "~/") + 1).
            else
                assign v_cod_dwb_file_temp = dwb_rpt_param.cod_dwb_file.
            assign ed_1x40:screen-value = v_cod_dwb_file_temp.
        end.

        if  dwb_rpt_param.cod_dwb_output = "Impressora" then do:
            if (not can-find(imprsor_usuar
                            where imprsor_usuar.nom_impressora = dwb_rpt_param.nom_dwb_printer
                              and imprsor_usuar.cod_usuario = dwb_rpt_param.cod_dwb_user use-index imprsrsr_id)
            or   not can-find(layout_impres
                             where layout_impres.nom_impressora = dwb_rpt_param.nom_dwb_printer
                               and layout_impres.cod_layout_impres = dwb_rpt_param.cod_dwb_print_layout)) then
                run pi_set_print_layout_default.

            assign ed_1x40:screen-value = dwb_rpt_param.nom_dwb_printer
                                        + ":"
                                        + dwb_rpt_param.cod_dwb_print_layout.
        end.
        assign v_log_print_par = dwb_rpt_param.log_dwb_print_parameters.
        display v_log_print_par
                with frame f_main.
    end.

    display v_qtd_column
            v_qtd_line
            with frame f_main.
    
    enable rs_cod_dwb_output
           v_log_print_par
           bt_get_file
           bt_set_printer
           bt_close
           bt_print
           bt_can
           bt_hel2
           v_cod_empresa_ini
           v_cod_empresa_fim
           v_cod_estab_ini
           v_cod_estab_fim
           v_dat_inic
           v_dat_fim
           v_cod_cenar_ctbl
           v_cod_finalid_econ
           bt_grupo
           with frame f_main.

/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.06':U &then
    run pi_posiciona_dwb_rpt_param in v_prog_filtro_pdf (input rowid(dwb_rpt_param)).
    run pi_load_params in v_prog_filtro_pdf.
&endif

    apply "value-changed" to rs_cod_dwb_output in frame f_main.
    
    enable rs_ind_run_mode with frame f_main.
    apply "value-changed" to rs_ind_run_mode in frame f_main.

    if num-entries( dwb_rpt_param.cod_dwb_parameters , chr(10) ) = 6  then do:
        assign v_cod_empresa_ini     = entry(1,entry(1, dwb_rpt_param.cod_dwb_parameters, chr(10)))
               v_cod_empresa_fim     = entry(2,entry(1, dwb_rpt_param.cod_dwb_parameters, chr(10)))
               v_cod_estab_ini       = entry(1,entry(2, dwb_rpt_param.cod_dwb_parameters, chr(10)))
               v_cod_estab_fim       = entry(2,entry(2, dwb_rpt_param.cod_dwb_parameters, chr(10)))
               v_dat_inic            = date(entry(3, dwb_rpt_param.cod_dwb_parameters, chr(10)))
               v_dat_fim             = date(entry(4, dwb_rpt_param.cod_dwb_parameters, chr(10)))
               v_cod_cenar_ctbl      = entry(5, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_cod_finalid_econ    = entry(6, dwb_rpt_param.cod_dwb_parameters, chr(10)).
    end.

    find cenar_ctbl no-lock
        where cenar_ctbl.cod_cenar_ctbl = v_cod_cenar_ctbl no-error.
    
    find finalid_econ no-lock
        where finalid_econ.cod_finalid_econ = v_cod_finalid_econ no-error.
    
    display v_cod_empresa_ini 
            v_cod_empresa_fim 
            v_cod_estab_ini   
            v_cod_estab_fim   
            v_dat_inic
            v_dat_fim
            v_cod_cenar_ctbl
            v_cod_finalid_econ
            with frame f_main.
    
    block1:
    repeat on error undo block1, retry block1:

        main_block:
        repeat on error undo super_block, retry super_block
                        on endkey undo super_block, leave super_block
                        on stop undo super_block, retry super_block
                        with frame f_main:

            if (retry) then
                output stream s_1 close.
            assign v_log_print = no.
            if  valid-handle(v_wgh_focus) then
                wait-for go of frame f_main focus v_wgh_focus.
            else
                wait-for go of frame f_main.

            param_block:
            do transaction:
                assign input frame f_main v_cod_empresa_ini
                       input frame f_main v_cod_empresa_fim
                       input frame f_main v_cod_estab_ini
                       input frame f_main v_cod_estab_fim
                       input frame f_main v_dat_inic
                       input frame f_main v_dat_fim
                       input frame f_main v_cod_cenar_ctbl
                       input frame f_main v_cod_finalid_econ.
                /* End_Include: ix_p15_rpt_razao_grp_cta_espec */

                assign dwb_rpt_param.log_dwb_print_parameters = input frame f_main v_log_print_par
                       dwb_rpt_param.ind_dwb_run_mode         = input frame f_main rs_ind_run_mode
                       input frame f_main v_qtd_line.

                assign dwb_rpt_param.cod_dwb_parameters = v_cod_empresa_ini + "," + v_cod_empresa_fim + chr(10) +
                                                          v_cod_estab_ini + "," + v_cod_estab_fim + chr(10) +
                                                          string(v_dat_inic) + chr(10) + 
                                                          string(v_dat_fim)             + chr(10) +
                                                          v_cod_cenar_ctbl              + chr(10) +
                                                          v_cod_finalid_econ.
                
            end /* do param_block */.

            if  v_log_print = yes then do:
                if  dwb_rpt_param.ind_dwb_run_mode = "Batch" /*l_batch*/ 
                then do:
                   if  dwb_rpt_param.cod_dwb_output = "Arquivo" /*l_file*/ 
                   then do:
                       assign v_cod_dwb_file = replace(dwb_rpt_param.cod_dwb_file, "~\", "~/")
                              v_nom_integer = v_cod_dwb_file.
                       if  index(v_cod_dwb_file, ":") <> 0
                       then do:
                           /* Nome de arquivo com problemas. */
                           run pi_messages (input "show",
                                            input 1979,
                                            input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_1979*/.
                           next main_block.
                       end /* if */.

                       file_1:
                       do
                          while index(v_cod_dwb_file,"~/") <> 0:
                          assign v_cod_dwb_file = substring(v_cod_dwb_file,(index(v_cod_dwb_file,"~/" ) + 1)).
                       end /* do file_1 */.

                       /* valname: */
                       case num-entries(v_cod_dwb_file,"."):
                           when 1 then
                               if  length(v_cod_dwb_file) > 8
                               then do:
                                  /* Nome de arquivo com problemas. */
                                  run pi_messages (input "show",
                                                   input 1979,
                                                   input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_1979*/.
                                  next main_block.
                               end /* if */.
                           when 2 then
                               if  length(entry(1, v_cod_dwb_file, ".")) > 8
                               or length(entry(2, v_cod_dwb_file, ".")) > 3
                               then do:
                                  /* Nome de arquivo com problemas. */
                                  run pi_messages (input "show",
                                                   input 1979,
                                                   input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_1979*/.
                                  next main_block.
                               end /* if */.
                           otherwise other:
                                     do:
                               /* Nome de arquivo com problemas. */
                               run pi_messages (input "show",
                                                input 1979,
                                                input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_1979*/.
                               next main_block.
                           end /* do other */.
                       end /* case valname */.
                   end /* if */.
                   
                   /* tech38629 - Alteraá∆o efetuada via filtro */
                   &if '{&emsbas_version}':U >= '5.06':U &then
                   run pi_filename_batch in v_prog_filtro_pdf.
                   &endif
                   /* tech38629 - Fim da alteraá∆o */

                   assign v_cod_dwb_file = v_nom_integer.
                   if  search("prgtec/btb/btb911za.r") = ? and search("prgtec/btb/btb911za.p") = ? then do:
                       if  v_cod_dwb_user begins 'es_' then
                           return "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgtec/btb/btb911za.p".
                       else do:
                           message "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgtec/btb/btb911za.p"
                                  view-as alert-box error buttons ok.
                           return.
                       end.
                   end.
                   else
                       run prgtec/btb/btb911za.p (Input v_cod_dwb_program,
                                                  Input v_cod_release,
                                                  Input 40,
                                                  Input recid(dwb_rpt_param),
                                                  output v_num_ped_exec) /*prg_fnc_criac_ped_exec*/.
                   if (v_num_ped_exec <> 0) then
                       leave main_block.
                   else
                       next main_block.
                end /* if */.
                else do:

                    assign v_log_method = session:set-wait-state('general')
                           v_nom_report_title = fill(" ", 40 - length(v_rpt_s_1_name)) + v_rpt_s_1_name.

                    if dwb_rpt_param.cod_dwb_output = 'Terminal' then do:
                        assign v_cod_dwb_file   = session:temp-directory + substring ("prgfin/fas/esfas001ya.p", 12, 8) + '.tmp'
                               v_rpt_s_1_bottom = v_qtd_line - (v_rpt_s_1_lines - v_qtd_bottom).
                        output stream s_1 to value(v_cod_dwb_file) paged page-size value(v_qtd_line) convert target 'iso8859-1'.
                    end /* do out_term */.

                    if dwb_rpt_param.cod_dwb_output = 'Impressora' and getCodTipoRelat() <> 'PDF':U and getCodTipoRelat() <> 'RTF':U then do:
                        find imprsor_usuar no-lock
                             where imprsor_usuar.nom_impressora = dwb_rpt_param.nom_dwb_printer
                               and imprsor_usuar.cod_usuario = dwb_rpt_param.cod_dwb_user use-index imprsrsr_id no-error.
                        find impressora no-lock
                             where impressora.nom_impressora = imprsor_usuar.nom_impressora
                              no-error.
                        find tip_imprsor no-lock
                             where tip_imprsor.cod_tip_imprsor = impressora.cod_tip_imprsor
                              no-error.
                        find layout_impres no-lock
                             where layout_impres.nom_impressora = dwb_rpt_param.nom_dwb_printer
                               and layout_impres.cod_layout_impres = dwb_rpt_param.cod_dwb_print_layout /*cl_get_layout of layout_impres*/ no-error.
                        assign v_rpt_s_1_bottom = layout_impres.num_lin_pag - (v_rpt_s_1_lines - v_qtd_bottom).

                        if  v_nom_dwb_print_file <> "" then
                            if  layout_impres.num_lin_pag = 0 then
                                output stream s_1 to value(lc(v_nom_dwb_print_file))
                                       page-size 0 convert target tip_imprsor.cod_pag_carac_conver.
                            else
                                output stream s_1 to value(lc(v_nom_dwb_print_file))
                                       paged page-size value(layout_impres.num_lin_pag) convert target  tip_imprsor.cod_pag_carac_conver.
                        else

                            if  layout_impres.num_lin_pag = 0 then
                                output stream s_1 to value(imprsor_usuar.nom_disposit_so)
                                       page-size 0 convert target tip_imprsor.cod_pag_carac_conver.
                            else
                                output stream s_1 to value(imprsor_usuar.nom_disposit_so)
                                       paged page-size value(layout_impres.num_lin_pag) convert target  tip_imprsor.cod_pag_carac_conver.

                        setting:
                        for each configur_layout_impres no-lock
                           where configur_layout_impres.num_id_layout_impres = layout_impres.num_id_layout_impres

                            by configur_layout_impres.num_ord_funcao_imprsor:
                            find configur_tip_imprsor no-lock
                                 where configur_tip_imprsor.cod_tip_imprsor = layout_impres.cod_tip_imprsor
                                   and configur_tip_imprsor.cod_funcao_imprsor = configur_layout_impres.cod_funcao_imprsor
                                   and configur_tip_imprsor.cod_opc_funcao_imprsor = configur_layout_impres.cod_opc_funcao_imprsor 
                                   use-index cnfgrtpm_id no-error.
                            bloco_1:
                            do
                                v_num_count = 1 to extent(configur_tip_imprsor.num_carac_configur):
                                /* configur_tip_imprsor: */
                                case configur_tip_imprsor.num_carac_configur[v_num_count]:
                                     when 0 then put  stream s_1 control null.
                                     when ? then leave.
                                     otherwise 
                                         /* Convers∆o interna do OUTPUT TARGET */
                                         put stream s_1 control codepage-convert ( chr(configur_tip_imprsor.num_carac_configur[v_num_count]),
                                                                                   session:cpinternal,
                                                                                   tip_imprsor.cod_pag_carac_conver).
                                end /* case configur_tip_imprsor */.
                            end /* do bloco_1 */.   
                        end /* for setting */.
                    end /* do out_print */.

                    if dwb_rpt_param.cod_dwb_output = 'Impressora' and getCodTipoRelat() = 'PDF':U then do:
                        run pi_config_output_print_pdf in v_prog_filtro_pdf (input v_qtd_line, input-output v_cod_dwb_file, input dwb_rpt_param.cod_dwb_user, input no).
                    end.

                    if dwb_rpt_param.cod_dwb_output = 'Arquivo' then do:
                        assign v_cod_dwb_file   = dwb_rpt_param.cod_dwb_file
                               v_rpt_s_1_bottom = v_qtd_line - (v_rpt_s_1_lines - v_qtd_bottom).

                        /* tech38629 - Alteraá∆o efetuada via filtro */
                        &if '{&emsbas_version}':U >= '5.06':U &then
                        run pi_rename_file in v_prog_filtro_pdf (input-output v_cod_dwb_file).
                        &endif
                        /* tech38629 - Fim da alteraá∆o */

                        output stream s_1 to value(v_cod_dwb_file)
                               paged page-size value(v_qtd_line) convert target 'iso8859-1'.
                    end /* do out_file */.

                    assign v_nom_prog_ext  = caps(substring("prgfin/fas/esfas001ya.p",12,8))
                           v_cod_release   = trim(" 5.06.00.000":U)
                           v_dat_execution = today
                           v_hra_execution = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":","").

                    IF dwb_rpt_param.cod_dwb_output <> "Excel" THEN
                        run pi_load_data_to_print.
                    ELSE
                        run pi_load_data_to_excel.

                end /* else */.

                if  dwb_rpt_param.log_dwb_print_parameters then do:
                    if (page-number (s_1) > 0) then
                        page stream s_1.
                    
                    hide stream s_1 frame f_rpt_s_1_header_period.
                    view stream s_1 frame f_rpt_s_1_header_unique.
                    hide stream s_1 frame f_rpt_s_1_footer_last_page.
                    hide stream s_1 frame f_rpt_s_1_footer_normal.
                    view stream s_1 frame f_rpt_s_1_footer_param_page.
                    if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        skip (1)
                        "Usu†rio: " at 1
                        v_cod_usuar_corren at 10 format "x(12)" skip (1).

                    if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "  Empresa: " at 8
                        v_cod_empresa_ini " atÇ " 
                        v_cod_empresa_fim skip
                        "Estabelecimento: " at 2
                        v_cod_estab_ini " atÇ " v_cod_estab_fim skip
                        "  Per°odo: " at 8 v_dat_inic format "99/99/9999" " atÇ: " v_dat_fim skip
                        "Cen†rio: " at 10
                        v_cod_cenar_ctbl format "x(8)" skip
                        "Finalidade: " at 7
                        v_cod_finalid_econ format "x(10)" skip.

                end /* if */.
                output stream s_1 close.
                /* tech38629 - Alteraá∆o efetuada via filtro */
                &if '{&emsbas_version}':U >= '5.06':U &then
                run pi_call_convert_object in v_prog_filtro_pdf (input no,
                                                                 input rs_cod_dwb_output:screen-value in frame f_main,
                                                                 input v_nom_dwb_print_file,
                                                                 input v_cod_dwb_file,
                                                                 input v_nom_report_title).
                &endif
                /* tech38629 - Fim da alteraá∆o */

                &if '{&emsbas_version}':U >= '5.06':U &then
                    if ((dwb_rpt_param.cod_dwb_output = 'Impressora' or dwb_rpt_param.cod_dwb_output = 'Impresora' or dwb_rpt_param.cod_dwb_output = 'printer') and getCodTipoRelat() = 'PDF':U) then do:
                        if v_nom_dwb_print_file = '' then
                            run pi_print_pdf_file in v_prog_filtro_pdf (input no).
                    end.
                &endif
                
                assign v_log_method = session:set-wait-state("").
                if (dwb_rpt_param.cod_dwb_output = "Terminal" /*l_terminal*/ ) then
                /* tech38629 - Alteraá∆o efetuada via filtro */
                &if '{&emsbas_version}':U >= '5.06':U &then
                    if  getCodTipoRelat() = 'PDF':U and OPSYS = 'WIN32':U
                    then do:
                        run pi_open_pdf_file in v_prog_filtro_pdf.
                    end.
                    else if getCodTipoRelat() = 'Texto' then do:
                &endif
                /* tech38629 - Fim da alteraá∆o */
                    run pi_show_report_2 (Input v_cod_dwb_file) /*pi_show_report_2*/.
                /* tech38629 - Alteraá∆o efetuada via filtro */
                &if '{&emsbas_version}':U >= '5.06':U &then
                    end.
                &endif
                /* tech38629 - Fim da alteraá∆o */

                leave main_block.

            end /* if */.
            else do:
                leave super_block.
            end /* else */.

        end /* repeat main_block */.

        if  v_num_ped_exec <> 0 then do:
            /* Criado pedido &1 para execuá∆o batch. */
            run pi_messages (input "show",
                             input 3556,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                v_num_ped_exec)) /*msg_3556*/.
            assign v_num_ped_exec = 0.
        end /* if */.
    end /* repeat block1 */.
end /* repeat super_block */.

hide frame f_main.

/* Begin_Include: i_log_exec_prog_dtsul_fim */
if v_rec_log <> ? then do transaction:
    find log_exec_prog_dtsul where recid(log_exec_prog_dtsul) = v_rec_log exclusive-lock no-error.
    if  avail log_exec_prog_dtsul
    then do:
        assign log_exec_prog_dtsul.dat_fim_exec_prog_dtsul = today
               log_exec_prog_dtsul.hra_fim_exec_prog_dtsul = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":","").
    end /* if */.
    release log_exec_prog_dtsul.
end.

/* End_Include: i_log_exec_prog_dtsul_fim */
        
if  this-procedure:persistent then
    delete procedure this-procedure.
        
/******************************* Main Code End ******************************/

/************************* Internal Procedure Begin *************************/

/*****************************************************************************
** Procedure Interna.....: pi_return_user
** Descricao.............: pi_return_user
** Criado por............: 
** Criado em.............: // 
** Alterado por..........: vladimir
** Alterado em...........: 12/02/1996 10:16:42
*****************************************************************************/
PROCEDURE pi_return_user:

    /************************ Parameter Definition Begin ************************/

    def output param p_nom_user
        as character
        format "x(32)"
        no-undo.


    /************************* Parameter Definition End *************************/

    assign p_nom_user = v_cod_usuar_corren.

    if  v_cod_usuar_corren begins 'es_'
    then do:
       assign v_cod_usuar_corren = entry(2,v_cod_usuar_corren,"_").
    end /* if */.

END PROCEDURE. /* pi_return_user */
/*****************************************************************************
** Procedure Interna.....: pi_filename_validation
** Descricao.............: pi_filename_validation
** Criado por............: 
** Criado em.............: // 
** Alterado por..........: tech35592
** Alterado em...........: 14/02/2006 07:39:05
*****************************************************************************/
PROCEDURE pi_filename_validation:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_filename
        as character
        format "x(40)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_cod_1                          as character       no-undo. /*local*/
    def var v_cod_2                          as character       no-undo. /*local*/
    def var v_num_1                          as integer         no-undo. /*local*/
    def var v_num_2                          as integer         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    if  p_cod_filename = "" or p_cod_filename = "."
    then do:
        return "NOK" /*l_nok*/ .
    end /* if */.

    assign v_cod_1 = replace(p_cod_filename, "~\", "/").

    1_block:
    repeat v_num_1 = 1 to length(v_cod_1):
        if  index('abcdefghijklmnopqrstuvwxyz0123456789-_:/.', substring(v_cod_1, v_num_1, 1)) = 0
        then do:
            return "NOK" /*l_nok*/ .
        end /* if */.
    end /* repeat 1_block */.

    if  num-entries(v_cod_1, ":") > 2
    then do:
        return "NOK" /*l_nok*/ .
    end /* if */.

    if  num-entries(v_cod_1, ":") = 2 and length(entry(1,v_cod_1,":")) > 1
    then do:
        return "NOK" /*l_nok*/ .
    end /* if */.

    if  num-entries(v_cod_1, ".") > 2
    then do:
        return "NOK" /*l_nok*/ .
    end /* if */.

    if  num-entries(v_cod_1, ".") = 2 and length(entry(2,v_cod_1,".")) > 3
    then do:
        return "NOK" /*l_nok*/ .
    end /* if */.

    if  index(entry(num-entries(v_cod_1, "/"),v_cod_1, "/"),".") = 0
    then do:
        return "NOK" /*l_nok*/ .
    end /* if */.
    else do:
        if  entry(1,entry(num-entries(v_cod_1,"/"),v_cod_1,"/"),".") = ""
        or  entry(2,entry(num-entries(v_cod_1,"/"),v_cod_1,"/"),".") = ""
        then do:
           return "NOK" /*l_nok*/ .
        end /* if */.
    end /* else */.

    assign v_num_1 = 1.
    2_block:
    repeat v_num_2 = 1 to length(v_cod_1):
        if  index(":" + "/" + ".", substring(v_cod_1, v_num_2, 1)) > 0
        then do:
            assign v_cod_2 = substring(v_cod_1, v_num_1, v_num_2 - v_num_1)
                   v_num_1 = v_num_2 + 1.
        end /* if */.
    end /* repeat 2_block */.
    assign v_cod_2 = substring(v_cod_1, v_num_1).

    return "OK" /*l_ok*/ .
END PROCEDURE. /* pi_filename_validation */
/*****************************************************************************
** Procedure Interna.....: pi_set_print_layout_default
** Descricao.............: pi_set_print_layout_default
** Criado por............: Gilsinei
** Criado em.............: 04/03/1996 09:22:54
** Alterado por..........: bre19127
** Alterado em...........: 16/09/2002 08:39:04
*****************************************************************************/
PROCEDURE pi_set_print_layout_default:

    dflt:
    do with frame f_main:

        find layout_impres_padr no-lock
             where layout_impres_padr.cod_usuario = v_cod_dwb_user
               and layout_impres_padr.cod_proced = v_cod_dwb_proced
    &if "{&emsbas_version}" >= "5.01" &then
             use-index lytmprsp_id
    &endif
              /*cl_default_procedure_user of layout_impres_padr*/ no-error.
        if  not avail layout_impres_padr
        then do:
            find layout_impres_padr no-lock
                 where layout_impres_padr.cod_usuario = "*"
                   and layout_impres_padr.cod_proced = v_cod_dwb_proced
    &if "{&emsbas_version}" >= "5.01" &then
                 use-index lytmprsp_id
    &endif
                  /*cl_default_procedure of layout_impres_padr*/ no-error.
            if  avail layout_impres_padr
            then do:
                find imprsor_usuar no-lock
                     where imprsor_usuar.nom_impressora = layout_impres_padr.nom_impressora
                       and imprsor_usuar.cod_usuario = v_cod_dwb_user
    &if "{&emsbas_version}" >= "5.01" &then
                     use-index imprsrsr_id
    &endif
                      /*cl_layout_current_user of imprsor_usuar*/ no-error.
            end /* if */.
            if  not avail imprsor_usuar
            then do:
                find layout_impres_padr no-lock
                     where layout_impres_padr.cod_usuario = v_cod_dwb_user
                       and layout_impres_padr.cod_proced = "*"
    &if "{&emsbas_version}" >= "5.01" &then
                     use-index lytmprsp_id
    &endif
                      /*cl_default_user of layout_impres_padr*/ no-error.
            end /* if */.
        end /* if */.
        do transaction:
            find first dwb_rpt_param
                where  dwb_rpt_param.cod_dwb_user = v_cod_usuar_corren
                  and  dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
                exclusive-lock no-error.
            if  avail layout_impres_padr
            then do:
                assign dwb_rpt_param.nom_dwb_printer      = layout_impres_padr.nom_impressora
                       dwb_rpt_param.cod_dwb_print_layout = layout_impres_padr.cod_layout_impres
                       ed_1x40:screen-value = dwb_rpt_param.nom_dwb_printer
                                            + ":"
                                            + dwb_rpt_param.cod_dwb_print_layout.
            end /* if */.
            else do:
                assign dwb_rpt_param.nom_dwb_printer       = ""
                       dwb_rpt_param.cod_dwb_print_layout  = ""
                       ed_1x40:screen-value = "".
            end /* else */.
        end.
    end /* do dflt */.
END PROCEDURE. /* pi_set_print_layout_default */
/*****************************************************************************
** Procedure Interna.....: pi_show_report_2
** Descricao.............: pi_show_report_2
** Criado por............: Gilsinei
** Criado em.............: 07/03/1996 14:42:50
** Alterado por..........: bre19127
** Alterado em...........: 21/05/2002 10:16:34
*****************************************************************************/
PROCEDURE pi_show_report_2:

    /************************ Parameter Definition Begin ************************/

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
END PROCEDURE. /* pi_show_report_2 */

/*****************************************************************************
** Procedure Interna.....: pi_output_reports
** Descricao.............: pi_output_reports
** Criado por............: glauco
** Criado em.............: 21/03/1997 09:26:29
** Alterado por..........: bre18856
** Alterado em...........: 16/03/2001 16:38:44
*****************************************************************************/
PROCEDURE pi_output_reports:

    /* tech38629 - Alteraá∆o efetuada via filtro */
    &if '{&emsbas_version}':U >= '5.06':U &then
        run pi_posiciona_dwb_rpt_param in v_prog_filtro_pdf (input rowid(dwb_rpt_param)).
        run pi_load_params in v_prog_filtro_pdf.
    &endif
    /* tech38629 - Fim da alteraá∆o */

    assign v_log_method       = session:set-wait-state('general')
           v_nom_report_title = fill(" ",40 - length(v_rpt_s_1_name)) + v_rpt_s_1_name
           v_rpt_s_1_bottom   = v_qtd_line - (v_rpt_s_1_lines - v_qtd_bottom).

    if dwb_rpt_param.cod_dwb_output = 'Impressora' and getCodTipoRelat() = 'PDF':U then do:
        run pi_config_output_print_pdf in v_prog_filtro_pdf (input v_qtd_line, input-output v_cod_dwb_file, input v_cod_usuar_corren, input yes).
    end.

    if dwb_rpt_param.cod_dwb_output = 'Arquivo' then
    block1:
    do:
        /* tech38629 - Alteraá∆o efetuada via filtro */
        &if '{&emsbas_version}':U >= '5.06':U &then
        run pi_rename_file in v_prog_filtro_pdf (input-output v_cod_dwb_file).
        &endif
        /* tech38629 - Fim da alteraá∆o */

        output stream s_1 to value(v_cod_dwb_file)
        paged page-size value(v_qtd_line) convert target 'iso8859-1'.
    end /* do block1 */.

    if dwb_rpt_param.cod_dwb_output = 'Impressora' and getCodTipoRelat() <> 'PDF':U and getCodTipoRelat() <> 'RTF':U then
       block2:
       do:
          find imprsor_usuar use-index imprsrsr_id no-lock
              where imprsor_usuar.nom_impressora = dwb_rpt_param.nom_dwb_printer
              and   imprsor_usuar.cod_usuario    = v_cod_usuar_corren no-error.
          find impressora no-lock
               where impressora.nom_impressora = imprsor_usuar.nom_impressora
                no-error.
          find tip_imprsor no-lock
               where tip_imprsor.cod_tip_imprsor = impressora.cod_tip_imprsor
                no-error.
          find layout_impres no-lock
               where layout_impres.nom_impressora = dwb_rpt_param.nom_dwb_printer
                 and layout_impres.cod_layout_impres = dwb_rpt_param.cod_dwb_print_layout /*cl_get_layout of layout_impres*/ no-error.
          find b_ped_exec_style
              where b_ped_exec_style.num_ped_exec = v_num_ped_exec_corren no-lock no-error.
          find servid_exec_imprsor no-lock
               where servid_exec_imprsor.nom_impressora = dwb_rpt_param.nom_dwb_printer
                 and servid_exec_imprsor.cod_servid_exec = b_ped_exec_style.cod_servid_exec no-error.

          find b_servid_exec_style no-lock
               where b_servid_exec_style.cod_servid_exec = b_ped_exec_style.cod_servid_exec
               no-error.

          if  avail layout_impres
          then do:
             assign v_rpt_s_1_bottom = layout_impres.num_lin_pag - (v_rpt_s_1_lines - v_qtd_bottom).
          end /* if */.

          if  available b_servid_exec_style
          and b_servid_exec_style.ind_tip_fila_exec = 'UNIX' then do:
              if dwb_rpt_param.nom_dwb_print_file <> "" then do:
                  if  layout_impres.num_lin_pag = 0
                  then do:
                      output stream s_1 to value(lc(dwb_rpt_param.nom_dwb_print_file))
                             page-size 0 convert target tip_imprsor.cod_pag_carac_conver.
                  end /* if */. 
                  else do:
                      output stream s_1 to value(lc(dwb_rpt_param.nom_dwb_print_file))
                             paged page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver.
                  end /* else */.
              end.
              else do:
                  if  layout_impres.num_lin_pag = 0
                  then do:
                      output stream s_1 through value(servid_exec_imprsor.nom_disposit_so)
                             page-size 0 convert target tip_imprsor.cod_pag_carac_conver.
                  end /* if */. 
                  else do:
                      output stream s_1 through value(servid_exec_imprsor.nom_disposit_so)
                             paged page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver.
                  end /* else */.
              end.
              
          end /* if */.
          else do:
              if dwb_rpt_param.nom_dwb_print_file <> "" then do:
                  if  layout_impres.num_lin_pag = 0
                  then do:
                      output stream s_1 to value(lc(dwb_rpt_param.nom_dwb_print_file))
                             page-size 0 convert target tip_imprsor.cod_pag_carac_conver.                            
                  end /* if */.
                  else do:
                      output stream s_1 to value(lc(dwb_rpt_param.nom_dwb_print_file))
                             paged page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver.                     
                  end /* else */.
              end.
              else do:
                  if  layout_impres.num_lin_pag = 0
                  then do:
                      output stream s_1 to value(servid_exec_imprsor.nom_disposit_so)
                             page-size 0 convert target tip_imprsor.cod_pag_carac_conver.                            
                  end /* if */.
                  else do:
                      output stream s_1 to value(servid_exec_imprsor.nom_disposit_so)
                             paged page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver.                     
                  end /* else */.
              end.
          end /* else */.

          setting:
          for each configur_layout_impres no-lock
              where configur_layout_impres.num_id_layout_impres = layout_impres.num_id_layout_impres

              by configur_layout_impres.num_ord_funcao_imprsor:

              find configur_tip_imprsor no-lock
                   where configur_tip_imprsor.cod_tip_imprsor = layout_impres.cod_tip_imprsor
                     and configur_tip_imprsor.cod_funcao_imprsor = configur_layout_impres.cod_funcao_imprsor
                     and configur_tip_imprsor.cod_opc_funcao_imprsor = configur_layout_impres.cod_opc_funcao_imprsor 
               use-index cnfgrtpm_id no-error.

              bloco_1:
              do
                  v_num_count = 1 to extent(configur_tip_imprsor.num_carac_configur):
                  /* configur_tip_imprsor: */
                  case configur_tip_imprsor.num_carac_configur[v_num_count]:
                      when 0 then put  stream s_1 control null.
                      when ? then leave.
                      otherwise 
                          /* Convers∆o interna do OUTPUT TARGET */
                          put stream s_1 control codepage-convert ( chr(configur_tip_imprsor.num_carac_configur[v_num_count]),
                                                                    session:cpinternal,
                                                                    tip_imprsor.cod_pag_carac_conver).
                  end /* case configur_tip_imprsor */.
              end /* do bloco_1 */.
          end /* for setting */.
    end /* do block2 */.

    run pi_load_data_to_print .
END PROCEDURE. /* pi_output_reports */

PROCEDURE pi_load_data_to_print:
    DEFINE VARIABLE l-impr-cab AS LOGICAL INITIAL NO NO-UNDO.

    if  not v_cod_dwb_user begins 'es_' then do:
        if  v_dat_inic > v_dat_fim then do:
            /* Faixa de datas incorreta ! */
            run pi_messages (input "show",
                             input 2651,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                               v_dat_inic,v_dat_fim)) /*msg_2651*/.
            assign v_wgh_focus = v_dat_inic:handle in frame f_main.
            return error.
        end.

        find cenar_ctbl no-lock
            where cenar_ctbl.cod_cenar_ctbl = input frame f_main v_cod_cenar_ctbl no-error.
        if not avail cenar_ctbl then do:
            /* Cen†rio Cont†bil Inexistente ! */
            run pi_messages (input "show",
                             input 449,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_449*/.
            assign v_wgh_focus = v_cod_cenar_ctbl:handle in frame f_main.
            return error.
        end.

        find finalid_econ no-lock
            where finalid_econ.cod_finalid_econ = input frame f_main v_cod_finalid_econ no-error.
        if not avail finalid_econ then do:
            /* Finalidade Economica &1 Inexistente ! */
            run pi_messages (input "show",
                             input 6385,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                               v_cod_finalid_econ)) /*msg_6385*/.
            assign v_wgh_focus = v_cod_finalid_econ:handle in frame f_main.
            return error.
        end.
    end /* if */.
    
    hide stream s_1 frame f_rpt_s_1_header_unique.
    view stream s_1 frame f_rpt_s_1_header_period.
    hide stream s_1 frame f_rpt_s_1_footer_last_page.
    hide stream s_1 frame f_rpt_s_1_footer_param_page.
    view stream s_1 frame f_rpt_s_1_footer_normal.

    assign v_val_sdo_inicial_cta     = 0 v_val_tot_sdo_inicial_cta = 0 v_val_g_sdo_inicial_cta     = 0
           v_val_adic                = 0 v_val_tot_adic            = 0 v_val_g_adic                = 0
           v_val_cm                  = 0 v_val_tot_cm              = 0 v_val_g_cm                  = 0
           v_val_transf              = 0 v_val_tot_transf          = 0 v_val_g_transf              = 0
           v_val_impl_transf         = 0 v_val_bxa_transf          = 0 v_val_bxa_imob_total        = 0
           v_val_tot_bxa_imob_total  = 0 v_val_g_bxa_imob_total    = 0 v_val_sdo_imob_cta          = 0
           v_val_tot_sdo_imob_cta    = 0 v_val_g_sdo_imob_cta      = 0 v_val_dpr_amort_ini         = 0
           v_val_tot_dpr_amort_ini   = 0 v_val_g_dpr_amort_ini     = 0 v_val_dpr_amort_per         = 0
           v_val_tot_dpr_amort_per   = 0 v_val_g_dpr_amort_per     = 0 v_val_dpr_amort_bxa         = 0
           v_val_tot_dpr_amort_bxa   = 0 v_val_g_dpr_amort_bxa     = 0 v_val_dpr_amort_tot         = 0 
           v_val_tot_dpr_amort_tot   = 0 v_val_g_dpr_amort_tot     = 0 v_val_dpr_transf            = 0
           v_val_bxa_dpr_transf      = 0 v_val_impl_dpr_transf     = 0 v_val_tot_dpr_transf        = 0
           v_val_g_dpr_transf        = 0 v_val_salvage_value_cta   = 0 v_val_tot_salvage_value_cta = 0
           v_val_g_salvage_value_cta = 0.
    ASSIGN v_val_bxa_imob_outros = 0 v_val_bxa_imob_vendas = 0
           v_val_tot_bxa_imob_vendas = 0 v_val_tot_bxa_imob_outros = 0
           v_val_g_bxa_imob_vendas = 0 v_val_g_bxa_imob_outros = 0.

    for each estabelecimento no-lock
       where estabelecimento.cod_empresa >= v_cod_empresa_ini
         and estabelecimento.cod_empresa <= v_cod_empresa_fim
         and estabelecimento.cod_estab   >= v_cod_estab_ini
         and estabelecimento.cod_estab   <= v_cod_estab_fim
       break by estabelecimento.cod_empresa
             by estabelecimento.cod_estab:

        find ems5.empresa no-lock where
             empresa.cod_empresa = estabelecimento.cod_empresa no-error.

        /*
        if first-of(estabelecimento.cod_empresa) then do:
            put stream s_1 unformatted skip (1)
                "Empresa: " estabelecimento.cod_empresa " - " empresa.nom_razao_social .
        end.

        if first-of(estabelecimento.cod_estab) then do:
            put stream s_1 unformatted skip (1)
                "Estabelecimento: " estabelecimento.cod_estab " - " estabelecimento.nom_pessoa .
        end.
        */

        for EACH grp_cta_pat_usuar 
           where grp_cta_pat_usuar.cod_usuario = "20100079" no-lock:

            for EACH compos_cta_pat_usuar no-lock
               where compos_cta_pat_usuar.cod_grp_cta_pat_usuar = grp_cta_pat_usuar.cod_grp_cta_pat_usuar
                 and compos_cta_pat_usuar.cod_usuario           = grp_cta_pat_usuar.cod_usuario 
               break by compos_cta_pat_usuar.cod_grp_cta_pat_usuar
                     by compos_cta_pat_usuar.cod_cta_pat:
    
                if first-of(compos_cta_pat_usuar.cod_grp_cta_pat_usuar) THEN
                    ASSIGN l-impr-cab = YES.

                for each bem_pat no-lock 
                   where bem_pat.cod_empresa  = estabelecimento.cod_empresa
                     and bem_pat.cod_estab    = estabelecimento.cod_estab
                     and bem_pat.cod_cta_pat = compos_cta_pat_usuar.cod_cta_pat
                   break by bem_pat.cod_empresa
                         by bem_pat.cod_estab :
    
                    run pi_acumula_incorp(input 0).
            
                    for each incorp_bem_pat no-lock
                        where incorp_bem_pat.num_id_bem_pat = bem_pat.num_id_bem_pat:
            
                        run pi_acumula_incorp(input incorp_bem_pat.num_seq_incorp_bem_pat).
                    end.
                end.
    
                assign v_val_adic         = v_val_adic        + v_val_cm
                       v_val_transf       = v_val_impl_transf - v_val_bxa_transf
                       v_val_sdo_imob_cta       = v_val_sdo_inicial_cta   + v_val_adic 
                                                + v_val_transf      - v_val_bxa_imob_total
                       v_val_dpr_transf   = v_val_impl_dpr_transf - v_val_bxa_dpr_transf
                       v_val_dpr_amort_tot = v_val_dpr_amort_ini    + v_val_dpr_amort_per 
                                                + v_val_dpr_transf  - v_val_dpr_amort_bxa.

                /* bno
                find first cta_pat no-lock 
                    where cta_pat.cod_cta_pat = compos_cta_pat_usuar.cod_cta_pat
                    and   cta_pat.cod_empresa = v_cod_empres_usuar no-error.
                    */
                find first cta_pat no-lock 
                    where cta_pat.cod_cta_pat = compos_cta_pat_usuar.cod_cta_pat
                    and   cta_pat.cod_empresa = estabelecimento.cod_empresa no-error.

                if avail cta_pat THEN DO:
                    IF v_val_sdo_inicial_cta   <> 0 OR
                       v_val_adic              <> 0 OR
                       v_val_transf            <> 0 OR
                       v_val_bxa_imob_total    <> 0 OR
                       v_val_sdo_imob_cta      <> 0 OR
                       v_val_dpr_amort_ini     <> 0 OR
                       v_val_dpr_amort_per     <> 0 OR
                       v_val_dpr_transf        <> 0 OR
                       v_val_dpr_amort_bxa     <> 0 OR
                       v_val_dpr_amort_tot     <> 0 OR
                       v_val_salvage_value_cta <> 0 THEN DO:

                        IF l-impr-cab = YES THEN DO:
                            if line-counter(s_1) > 1 then do:
                                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                        page stream s_1.
                                put stream s_1 unformatted  skip (1).
                            end.
                            if (line-counter(s_1) + 6) > v_rpt_s_1_bottom then
                                page stream s_1.
                            if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                page stream s_1.
                            /* bno
                            put stream s_1 unformatted 
                                "Grupo:" at 1
                                grp_cta_pat_usuar.des_grp_cta_pat_usuar at 8 format "x(40)" skip.
                            if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                page stream s_1.
                            put stream s_1 unformatted  skip (1).
                            if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                                page stream s_1.
                                fim - bno */
                            put stream s_1 unformatted 
                                "Emp"                   AT 3
                                "EST"                   AT 10
                                "Grupo"                 AT 15
                                "Cod Cta Pat"           at 43
                                "Descriá∆o Cta Pat"     at 63
                                "Saldo Inicial"         to 114
                                "Adiá‰es"               to 132
                                "Transferància"         to 150
                                "Baixas Imobilizado"    to 169
                                "Saldo Imobilizado"     to 187
                                "Depr/Amort Inicial"    to 206
                                "Depr/Amort Per°odo"    to 225
                                "Transferància"         to 244
                                "Baixas Depr/Amort"     to 262
                                "Depr/Amort Acumulada"  to 283 
                                "Salvage Value"         TO 301 skip
                                "---"                               AT 3
                                "---"                               AT 10
                                "--------------------"              AT 15
                                "-------------------"               at 43
                                "--------------------------------"  at 63
                                "-----------------"                 to 114
                                "-----------------"                 to 132
                                "-----------------"                 to 150
                                "------------------"                to 169
                                "-----------------"                 to 187
                                "------------------"                to 206
                                "------------------"                to 225
                                "------------------"                to 244
                                "-----------------"                 to 262
                                "--------------------"              to 283
                                "-----------------"                 TO 301 skip.

                            ASSIGN l-impr-cab = NO.
                        END.

                        if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then do:
                            page stream s_1.
                            if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                page stream s_1.
                            /* bno
                            put stream s_1 unformatted 
                                "Grupo:" at 1
                                grp_cta_pat_usuar.des_grp_cta_pat_usuar at 8 format "x(40)" skip.
                            if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                page stream s_1.
                            put stream s_1 unformatted  skip (1).
                            if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                                page stream s_1.
                                fim - bno */
                            put stream s_1 unformatted 
                                "Emp"                   AT 3
                                "EST"                   AT 10
                                "Grupo"                 AT 15
                                "Cod Cta Pat"           at 43
                                "Descriá∆o Cta Pat"     at 63
                                "Saldo Inicial"         to 114
                                "Adiá‰es"               to 132
                                "Transferància"         to 150
                                "Baixas Imobilizado"    to 169
                                "Saldo Imobilizado"     to 187
                                "Depr/Amort Inicial"    to 206
                                "Depr/Amort Per°odo"    to 225
                                "Transferància"         to 244
                                "Baixas Depr/Amort"     to 262
                                "Depr/Amort Acumulada"  to 283 
                                "Salvage Value"         TO 301 skip
                                "---"                               AT 3
                                "---"                               AT 10
                                "--------------------"              AT 15
                                "-------------------"               at 43
                                "--------------------------------"  at 63
                                "-----------------"                 to 114
                                "-----------------"                 to 132
                                "-----------------"                 to 150
                                "------------------"                to 169
                                "-----------------"                 to 187
                                "------------------"                to 206
                                "------------------"                to 225
                                "------------------"                to 244
                                "-----------------"                 to 262
                                "--------------------"              to 283
                                "-----------------"                 TO 301 skip.
                        end.

                        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                            page stream s_1.

                        put stream s_1 unformatted 
                            estabelecimento.cod_empresa     AT 3
                            estabelecimento.cod_estab       AT 10
                            grp_cta_pat_usuar.cod_grp_cta_pat_usuar AT 15
                            cta_pat.cod_cta_pat             at 43 format "x(18)"
                            cta_pat.des_cta_pat             at 63 format "x(32)"
                            v_val_sdo_inicial_cta           to  114 format "->>>>>,>>>,>>9.99"
                            v_val_adic                      to  132 format "->>>>>,>>>,>>9.99"
                            v_val_transf                    to  150 format "->>>>>,>>>,>>9.99"
                            v_val_bxa_imob_total            to 169 format "->>>>>,>>>,>>9.99"
                            v_val_sdo_imob_cta              to 187 format "->>>>>,>>>,>>9.99"
                            v_val_dpr_amort_ini             to 206 format "->>>>>,>>>,>>9.99"
                            v_val_dpr_amort_per             to 225 format "->>>>>,>>>,>>9.99"
                            v_val_dpr_transf                to 244 format "->>,>>>,>>>,>>9.99"
                            v_val_dpr_amort_bxa             to 262 format ">>>>>,>>>,>>9.99"
                            v_val_dpr_amort_tot             to 283 format "->>>>>,>>>,>>9.99"
                            v_val_salvage_value_cta         TO 301 FORMAT "->>>>>,>>>,>>9.99" SKIP.

                        assign v_val_tot_sdo_inicial_cta   = v_val_tot_sdo_inicial_cta   + v_val_sdo_inicial_cta
                               v_val_tot_adic              = v_val_tot_adic              + v_val_adic
                               v_val_tot_transf            = v_val_tot_transf            + v_val_transf
                               v_val_tot_bxa_imob_total    = v_val_tot_bxa_imob_total    + v_val_bxa_imob_total
                               v_val_tot_bxa_imob_vendas   = v_val_tot_bxa_imob_vendas   + v_val_bxa_imob_vendas
                               v_val_tot_bxa_imob_outros   = v_val_tot_bxa_imob_outros   + v_val_bxa_imob_outros
                               v_val_tot_sdo_imob_cta      = v_val_tot_sdo_imob_cta      + v_val_sdo_imob_cta
                               v_val_tot_dpr_amort_ini     = v_val_tot_dpr_amort_ini     + v_val_dpr_amort_ini
                               v_val_tot_dpr_amort_per     = v_val_tot_dpr_amort_per     + v_val_dpr_amort_per
                               v_val_tot_dpr_amort_bxa     = v_val_tot_dpr_amort_bxa     + v_val_dpr_amort_bxa
                               v_val_tot_dpr_amort_tot     = v_val_tot_dpr_amort_tot     + v_val_dpr_amort_tot
                               v_val_tot_dpr_transf        = v_val_tot_dpr_transf        + v_val_dpr_transf
                               v_val_tot_salvage_value_cta = v_val_tot_salvage_value_cta + v_val_salvage_value_cta.

                        /***************************************Totais por Empresa******************************************/
                        assign v_val_emp_sdo_inicial_cta   = v_val_emp_sdo_inicial_cta   + v_val_sdo_inicial_cta
                               v_val_emp_adic              = v_val_emp_adic              + v_val_adic
                               v_val_emp_transf            = v_val_emp_transf            + v_val_transf
                               v_val_emp_bxa_imob_total    = v_val_emp_bxa_imob_total    + v_val_bxa_imob_total
                               v_val_emp_bxa_imob_vendas   = v_val_emp_bxa_imob_vendas   + v_val_bxa_imob_vendas
                               v_val_emp_bxa_imob_outros   = v_val_emp_bxa_imob_outros   + v_val_bxa_imob_outros
                               v_val_emp_sdo_imob_cta      = v_val_emp_sdo_imob_cta      + v_val_sdo_imob_cta
                               v_val_emp_dpr_amort_ini     = v_val_emp_dpr_amort_ini     + v_val_dpr_amort_ini
                               v_val_emp_dpr_amort_per     = v_val_emp_dpr_amort_per     + v_val_dpr_amort_per
                               v_val_emp_dpr_amort_bxa     = v_val_emp_dpr_amort_bxa     + v_val_dpr_amort_bxa
                               v_val_emp_dpr_amort_tot     = v_val_emp_dpr_amort_tot     + v_val_dpr_amort_tot
                               v_val_emp_dpr_transf        = v_val_emp_dpr_transf        + v_val_dpr_transf
                               v_val_emp_salvage_value_cta = v_val_emp_salvage_value_cta + v_val_salvage_value_cta.
                        /***************************************************************************************************/

                        /************************************** Totais por Estab *******************************************/
                        assign v_val_est_sdo_inicial_cta   = v_val_est_sdo_inicial_cta   + v_val_sdo_inicial_cta
                               v_val_est_adic              = v_val_est_adic              + v_val_adic
                               v_val_est_transf            = v_val_est_transf            + v_val_transf
                               v_val_est_bxa_imob_total    = v_val_est_bxa_imob_total    + v_val_bxa_imob_total
                               v_val_est_bxa_imob_vendas   = v_val_est_bxa_imob_vendas   + v_val_bxa_imob_vendas
                               v_val_est_bxa_imob_outros   = v_val_est_bxa_imob_outros   + v_val_bxa_imob_outros
                               v_val_est_sdo_imob_cta      = v_val_est_sdo_imob_cta      + v_val_sdo_imob_cta
                               v_val_est_dpr_amort_ini     = v_val_est_dpr_amort_ini     + v_val_dpr_amort_ini
                               v_val_est_dpr_amort_per     = v_val_est_dpr_amort_per     + v_val_dpr_amort_per
                               v_val_est_dpr_amort_bxa     = v_val_est_dpr_amort_bxa     + v_val_dpr_amort_bxa
                               v_val_est_dpr_amort_tot     = v_val_est_dpr_amort_tot     + v_val_dpr_amort_tot
                               v_val_est_dpr_transf        = v_val_est_dpr_transf        + v_val_dpr_transf
                               v_val_est_salvage_value_cta = v_val_est_salvage_value_cta + v_val_salvage_value_cta.
                        /***************************************************************************************************/

                        assign v_val_sdo_inicial_cta = 0 v_val_sdo_imob_cta      = 0
                               v_val_adic            = 0 v_val_dpr_amort_ini     = 0
                               v_val_cm              = 0 v_val_dpr_amort_per     = 0
                               v_val_transf          = 0 v_val_dpr_amort_bxa     = 0
                               v_val_bxa_imob_total  = 0 v_val_dpr_amort_tot     = 0
                               v_val_impl_transf     = 0 v_val_bxa_transf        = 0
                               v_val_impl_dpr_transf = 0 v_val_bxa_dpr_transf    = 0
                               v_val_dpr_transf      = 0 v_val_salvage_value_cta = 0.
                        ASSIGN v_val_bxa_imob_outros = 0 v_val_bxa_imob_vendas = 0.
                    END.
                END.
    
                if last-of(compos_cta_pat_usuar.cod_grp_cta_pat_usuar) then do:
                    IF v_val_tot_sdo_inicial_cta   <> 0 OR
                       v_val_tot_adic              <> 0 OR
                       v_val_tot_transf            <> 0 OR
                       v_val_tot_bxa_imob_total    <> 0 OR
                       v_val_tot_sdo_imob_cta      <> 0 OR
                       v_val_tot_dpr_amort_ini     <> 0 OR
                       v_val_tot_dpr_amort_per     <> 0 OR
                       v_val_tot_dpr_transf        <> 0 OR
                       v_val_tot_dpr_amort_bxa     <> 0 OR
                       v_val_tot_dpr_amort_tot     <> 0 OR
                       v_val_tot_salvage_value_cta <> 0 THEN DO:
                        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                            page stream s_1.

                        put stream s_1 unformatted 
                            "-------------------"               at 43
                            "--------------------------------"  at 63
                            "-----------------"                 at 98
                            "-----------------"                 at 116
                            "-----------------"                 at 134
                            "------------------"                at 152
                            "-----------------"                 at 171
                            "------------------"                at 189
                            "------------------"                at 208
                            "------------------"                at 227
                            "-----------------"                 at 246
                            "--------------------"              at 264
                            "-----------------"                 at 285 skip.

                        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted  skip (1).

                        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                            page stream s_1.

                        put stream s_1 unformatted 
                            grp_cta_pat_usuar.des_grp_cta_pat_usuar + " - Total:"                    at   1
                            v_val_tot_sdo_inicial_cta   to 114 format "->>>>>,>>>,>>9.99"
                            v_val_tot_adic              to 132 format "->>>>>,>>>,>>9.99"
                            v_val_tot_transf            to 150 format "->>>>>,>>>,>>9.99"
                            v_val_tot_bxa_imob_total    to 169 format "->>>>>,>>>,>>9.99"
                            v_val_tot_sdo_imob_cta      to 187 format "->>>>>,>>>,>>9.99"
                            v_val_tot_dpr_amort_ini     to 206 format "->>>>>,>>>,>>9.99"
                            v_val_tot_dpr_amort_per     to 225 format "->>>>>,>>>,>>9.99"
                            v_val_tot_dpr_transf        to 244 format "->>>>>,>>>,>>9.99"
                            v_val_tot_dpr_amort_bxa     to 262 format "->>>>>,>>>,>>9.99"
                            v_val_tot_dpr_amort_tot     to 283 format "->>>>>,>>>,>>9.99"
                            v_val_tot_salvage_value_cta TO 301 format "->>>>>,>>>,>>9.99" skip.

                        assign v_val_g_sdo_inicial_cta   = v_val_g_sdo_inicial_cta   + v_val_tot_sdo_inicial_cta
                               v_val_g_adic              = v_val_g_adic              + v_val_tot_adic
                               v_val_g_transf            = v_val_g_transf            + v_val_tot_transf
                               v_val_g_bxa_imob_total    = v_val_g_bxa_imob_total    + v_val_tot_bxa_imob_total
                               v_val_g_bxa_imob_vendas   = v_val_g_bxa_imob_vendas   + v_val_tot_bxa_imob_vendas
                               v_val_g_bxa_imob_outros   = v_val_g_bxa_imob_outros   + v_val_tot_bxa_imob_outros
                               v_val_g_sdo_imob_cta      = v_val_g_sdo_imob_cta      + v_val_tot_sdo_imob_cta
                               v_val_g_dpr_amort_ini     = v_val_g_dpr_amort_ini     + v_val_tot_dpr_amort_ini 
                               v_val_g_dpr_amort_per     = v_val_g_dpr_amort_per     + v_val_tot_dpr_amort_per
                               v_val_g_dpr_amort_bxa     = v_val_g_dpr_amort_bxa     + v_val_tot_dpr_amort_bxa
                               v_val_g_dpr_amort_tot     = v_val_g_dpr_amort_tot     + v_val_tot_dpr_amort_tot
                               v_val_g_dpr_transf        = v_val_g_dpr_transf        + v_val_tot_dpr_transf
                               v_val_g_salvage_value_cta = v_val_g_salvage_value_cta + v_val_tot_salvage_value_cta.

                        assign v_val_tot_sdo_inicial_cta   = 0 v_val_tot_dpr_amort_ini = 0
                               v_val_tot_adic              = 0 v_val_tot_dpr_amort_per = 0
                               v_val_tot_transf            = 0 v_val_tot_dpr_amort_bxa = 0
                               v_val_tot_bxa_imob_total    = 0 v_val_tot_dpr_amort_tot = 0
                               v_val_tot_sdo_imob_cta      = 0 v_val_tot_dpr_transf    = 0
                               v_val_tot_salvage_value_cta = 0.
                        ASSIGN v_val_tot_bxa_imob_vendas = 0 v_val_tot_bxa_imob_outros = 0.
                    END.
                end.
            end.
        end.

        if last-of(estabelecimento.cod_estab) then do:
            IF v_val_est_sdo_inicial_cta   <> 0 OR
               v_val_est_adic              <> 0 OR
               v_val_est_transf            <> 0 OR
               v_val_est_bxa_imob_total    <> 0 OR
               v_val_est_sdo_imob_cta      <> 0 OR
               v_val_est_dpr_amort_ini     <> 0 OR
               v_val_est_dpr_amort_per     <> 0 OR
               v_val_est_dpr_transf        <> 0 OR
               v_val_est_dpr_amort_bxa     <> 0 OR
               v_val_est_dpr_amort_tot     <> 0 OR
               v_val_est_salvage_value_cta <> 0 THEN DO:
                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.

                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.

                put stream s_1 unformatted  skip (1).

                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.

                put stream s_1 unformatted 
                    "Total Estabelecimento:"        at   1
                    v_val_est_sdo_inicial_cta       to 114 format "->>>>>,>>>,>>9.99"
                    v_val_est_adic                  to 132 format "->>>>>,>>>,>>9.99"
                    v_val_est_transf                to 150 format "->>>>>,>>>,>>9.99"
                    v_val_est_bxa_imob_total        to 169 format "->>>>>,>>>,>>9.99"
                    v_val_est_sdo_imob_cta          to 187 format "->>>>>,>>>,>>9.99"
                    v_val_est_dpr_amort_ini         to 206 format "->>>>>,>>>,>>9.99"
                    v_val_est_dpr_amort_per         to 225 format "->>>>>,>>>,>>9.99"
                    v_val_est_dpr_transf            to 244 format "->>>>>,>>>,>>9.99"
                    v_val_est_dpr_amort_bxa         to 262 format "->>>>>,>>>,>>9.99"
                    v_val_est_dpr_amort_tot         to 283 format "->>>>>,>>>,>>9.99"
                    v_val_est_salvage_value_cta     TO 301 format "->>>>>,>>>,>>9.99" skip.

                assign v_val_est_sdo_inicial_cta   = 0 v_val_est_dpr_amort_ini = 0
                       v_val_est_adic              = 0 v_val_est_dpr_amort_per = 0
                       v_val_est_transf            = 0 v_val_est_dpr_amort_bxa = 0
                       v_val_est_bxa_imob_total    = 0 v_val_est_dpr_amort_tot = 0
                       v_val_est_sdo_imob_cta      = 0 v_val_est_dpr_transf    = 0
                       v_val_est_salvage_value_cta = 0.
                ASSIGN v_val_est_bxa_imob_vendas = 0 v_val_est_bxa_imob_outros = 0.
            END.
        end.

        if last-of(estabelecimento.cod_empresa) then do:
            IF v_val_emp_sdo_inicial_cta   <> 0 OR
               v_val_emp_adic              <> 0 OR
               v_val_emp_transf            <> 0 OR
               v_val_emp_bxa_imob_total    <> 0 OR
               v_val_emp_sdo_imob_cta      <> 0 OR
               v_val_emp_dpr_amort_ini     <> 0 OR
               v_val_emp_dpr_amort_per     <> 0 OR
               v_val_emp_dpr_transf        <> 0 OR
               v_val_emp_dpr_amort_bxa     <> 0 OR
               v_val_emp_dpr_amort_tot     <> 0 OR
               v_val_emp_salvage_value_cta <> 0 THEN DO:
                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.

                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted  skip (1).

                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.

                put stream s_1 unformatted 
                    "Total Empresa:"            at 1
                    v_val_emp_sdo_inicial_cta   to 114 format "->>>>>,>>>,>>9.99"
                    v_val_emp_adic              to 132 format "->>>>>,>>>,>>9.99"
                    v_val_emp_transf            to 150 format "->>>>>,>>>,>>9.99"
                    v_val_emp_bxa_imob_total    to 169 format "->>>>>,>>>,>>9.99"
                    v_val_emp_sdo_imob_cta      to 187 format "->>>>>,>>>,>>9.99"
                    v_val_emp_dpr_amort_ini     to 206 format "->>>>>,>>>,>>9.99"
                    v_val_emp_dpr_amort_per     to 225 format "->>>>>,>>>,>>9.99"
                    v_val_emp_dpr_transf        to 244 format "->>>>>,>>>,>>9.99"
                    v_val_emp_dpr_amort_bxa     to 262 format "->>>>>,>>>,>>9.99"
                    v_val_emp_dpr_amort_tot     to 283 format "->>>>>,>>>,>>9.99"
                    v_val_emp_salvage_value_cta TO 301 format "->>>>>,>>>,>>9.99" skip.

                assign v_val_emp_sdo_inicial_cta   = 0 v_val_emp_dpr_amort_ini = 0
                       v_val_emp_adic              = 0 v_val_emp_dpr_amort_per = 0
                       v_val_emp_transf            = 0 v_val_emp_dpr_amort_bxa = 0
                       v_val_emp_bxa_imob_total    = 0 v_val_emp_dpr_amort_tot = 0
                       v_val_emp_sdo_imob_cta      = 0 v_val_emp_dpr_transf    = 0
                       v_val_emp_salvage_value_cta = 0.
                ASSIGN v_val_emp_bxa_imob_vendas = 0 v_val_emp_bxa_imob_outros = 0.

                page stream s_1 .
            END.
        end.
    end.

    if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
        page stream s_1.
    put stream s_1 unformatted  skip (1).
    if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
        page stream s_1.

    put stream s_1 unformatted 
        "Total Geral:"              at 1
        v_val_g_sdo_inicial_cta     to 114 format "->>>>>,>>>,>>9.99"
        v_val_g_adic                to 132 format "->>>>>,>>>,>>9.99"
        v_val_g_transf              to 150 format "->>>>>,>>>,>>9.99"
        v_val_g_bxa_imob_total      to 169 format "->>>>>,>>>,>>9.99"
        v_val_g_sdo_imob_cta        to 187 format "->>>>>,>>>,>>9.99"
        v_val_g_dpr_amort_ini       to 206 format "->>>>>,>>>,>>9.99"
        v_val_g_dpr_amort_per       to 225 format "->>>>>,>>>,>>9.99"
        v_val_g_dpr_transf          to 244 format "->>>>>,>>>,>>9.99"
        v_val_g_dpr_amort_bxa       to 262 format "->>>>>,>>>,>>9.99"
        v_val_g_dpr_amort_tot       to 283 format "->>>>>,>>>,>>9.99"
        v_val_g_salvage_value_cta   TO 301 format "->>>>>,>>>,>>9.99" skip.

    assign v_val_g_sdo_inicial_cta   = 0 v_val_g_dpr_amort_ini = 0
           v_val_g_adic              = 0 v_val_g_dpr_amort_per = 0
           v_val_g_transf            = 0 v_val_g_dpr_amort_bxa = 0
           v_val_g_bxa_imob_total    = 0 v_val_g_dpr_amort_tot = 0
           v_val_g_sdo_imob_cta      = 0 v_val_g_dpr_transf    = 0
           v_val_g_salvage_value_cta = 0.
    ASSIGN v_val_g_bxa_imob_vendas = 0 v_val_g_bxa_imob_outros = 0.

    hide stream s_1 frame f_rpt_s_1_Grp_movtos_Lay_movtos.
    hide stream s_1 frame f_rpt_s_1_footer_normal.
    hide stream s_1 frame f_rpt_s_1_footer_param_page.
    view stream s_1 frame f_rpt_s_1_footer_last_page.

END PROCEDURE.

PROCEDURE pi_load_data_to_excel:

    DEFINE VARIABLE chExcelApplication AS COM-HANDLE    NO-UNDO.
    DEFINE VARIABLE chWorkBook         AS COM-HANDLE    NO-UNDO.
    DEFINE VARIABLE chWorkSheet        AS COM-HANDLE    NO-UNDO.
    DEFINE VARIABLE i-linha            AS INTEGER       NO-UNDO.

    if  not v_cod_dwb_user begins 'es_' then do:
        if  v_dat_inic > v_dat_fim then do:
            /* Faixa de datas incorreta ! */
            run pi_messages (input "show",
                             input 2651,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                               v_dat_inic,v_dat_fim)) /*msg_2651*/.
            assign v_wgh_focus = v_dat_inic:handle in frame f_main.
            return error.
        end.

        find cenar_ctbl no-lock
            where cenar_ctbl.cod_cenar_ctbl = input frame f_main v_cod_cenar_ctbl no-error.
        if not avail cenar_ctbl then do:
            /* Cen†rio Cont†bil Inexistente ! */
            run pi_messages (input "show",
                             input 449,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_449*/.
            assign v_wgh_focus = v_cod_cenar_ctbl:handle in frame f_main.
            return error.
        end.

        find finalid_econ no-lock
            where finalid_econ.cod_finalid_econ = input frame f_main v_cod_finalid_econ no-error.
        if not avail finalid_econ then do:
            /* Finalidade Economica &1 Inexistente ! */
            run pi_messages (input "show",
                             input 6385,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                               v_cod_finalid_econ)) /*msg_6385*/.
            assign v_wgh_focus = v_cod_finalid_econ:handle in frame f_main.
            return error.
        end.
    end /* if */.
    
    /* ---> Cria objeto excel <--- */
    CREATE "Excel.Application" chExcelApplication.
    
    /* ---> Adiciona o modelo do documento <--- */
    chWorkbook  = chExcelApplication:Workbooks:ADD(SEARCH("esp\esfas001ya.xlt")).
    chWorkSheet = chExcelApplication:Sheets:ITEM(1).
    
    ASSIGN i-linha = 1.

    ASSIGN chExcelApplication:Range("A" + STRING (i-linha)):VALUE = "Emp".
    ASSIGN chExcelApplication:Range("B" + STRING (i-linha)):VALUE = "Est".
    ASSIGN chExcelApplication:Range("C" + STRING (i-linha)):VALUE = "Grupo".
    ASSIGN chExcelApplication:Range("D" + STRING (i-linha)):VALUE = "Cod Cta Pat".
    ASSIGN chExcelApplication:Range("E" + STRING (i-linha)):VALUE = "Descriá∆o Cta Pat".
    ASSIGN chExcelApplication:Range("F" + STRING (i-linha)):VALUE = "Saldo Inicial".
    ASSIGN chExcelApplication:Range("G" + STRING (i-linha)):VALUE = "Adiá‰es".      
    ASSIGN chExcelApplication:Range("H" + STRING (i-linha)):VALUE = "Transferància".
    ASSIGN chExcelApplication:Range("I" + STRING (i-linha)):VALUE = "Baixas Imob Vendas".
    ASSIGN chExcelApplication:Range("J" + STRING (i-linha)):VALUE = "Baixas Imob Outros".
    ASSIGN chExcelApplication:Range("K" + STRING (i-linha)):VALUE = "Saldo Imobilizado".
    ASSIGN chExcelApplication:Range("L" + STRING (i-linha)):VALUE = "Depr/Amort Inicial".
    ASSIGN chExcelApplication:Range("M" + STRING (i-linha)):VALUE = "Depr/Amort Per°odo".
    ASSIGN chExcelApplication:Range("N" + STRING (i-linha)):VALUE = "Transferància".
    ASSIGN chExcelApplication:Range("O" + STRING (i-linha)):VALUE = "Baixas Depr/Amort".
    ASSIGN chExcelApplication:Range("P" + STRING (i-linha)):VALUE = "Depr/Amort Acumulada".
    ASSIGN chExcelApplication:Range("Q" + STRING (i-linha)):VALUE = "Salvage Value".

    ASSIGN i-linha = i-linha + 1.
    
    assign v_val_sdo_inicial_cta     = 0 v_val_tot_sdo_inicial_cta = 0 v_val_g_sdo_inicial_cta     = 0
           v_val_adic                = 0 v_val_tot_adic            = 0 v_val_g_adic                = 0
           v_val_cm                  = 0 v_val_tot_cm              = 0 v_val_g_cm                  = 0
           v_val_transf              = 0 v_val_tot_transf          = 0 v_val_g_transf              = 0
           v_val_impl_transf         = 0 v_val_bxa_transf          = 0 v_val_bxa_imob_total        = 0
           v_val_tot_bxa_imob_total  = 0 v_val_g_bxa_imob_total    = 0 v_val_sdo_imob_cta          = 0
           v_val_tot_sdo_imob_cta    = 0 v_val_g_sdo_imob_cta      = 0 v_val_dpr_amort_ini         = 0
           v_val_tot_dpr_amort_ini   = 0 v_val_g_dpr_amort_ini     = 0 v_val_dpr_amort_per         = 0
           v_val_tot_dpr_amort_per   = 0 v_val_g_dpr_amort_per     = 0 v_val_dpr_amort_bxa         = 0
           v_val_tot_dpr_amort_bxa   = 0 v_val_g_dpr_amort_bxa     = 0 v_val_dpr_amort_tot         = 0 
           v_val_tot_dpr_amort_tot   = 0 v_val_g_dpr_amort_tot     = 0 v_val_dpr_transf            = 0
           v_val_bxa_dpr_transf      = 0 v_val_impl_dpr_transf     = 0 v_val_tot_dpr_transf        = 0
           v_val_g_dpr_transf        = 0 v_val_salvage_value_cta   = 0 v_val_tot_salvage_value_cta = 0
           v_val_g_salvage_value_cta = 0.
    ASSIGN v_val_bxa_imob_outros = 0 v_val_bxa_imob_vendas = 0
           v_val_tot_bxa_imob_vendas = 0 v_val_tot_bxa_imob_outros = 0
           v_val_g_bxa_imob_vendas = 0 v_val_g_bxa_imob_outros = 0.

    for each estabelecimento no-lock
       where estabelecimento.cod_empresa >= v_cod_empresa_ini
         and estabelecimento.cod_empresa <= v_cod_empresa_fim
         and estabelecimento.cod_estab   >= v_cod_estab_ini
         and estabelecimento.cod_estab   <= v_cod_estab_fim
       break by estabelecimento.cod_empresa
             by estabelecimento.cod_estab:

        find ems5.empresa no-lock where
             empresa.cod_empresa = estabelecimento.cod_empresa no-error.

        for EACH grp_cta_pat_usuar 
           where grp_cta_pat_usuar.cod_usuario = "20100079" no-lock:

            for EACH compos_cta_pat_usuar no-lock
               where compos_cta_pat_usuar.cod_grp_cta_pat_usuar = grp_cta_pat_usuar.cod_grp_cta_pat_usuar
                 and compos_cta_pat_usuar.cod_usuario           = grp_cta_pat_usuar.cod_usuario 
               break by compos_cta_pat_usuar.cod_grp_cta_pat_usuar
                     by compos_cta_pat_usuar.cod_cta_pat:

                for each bem_pat no-lock 
                   where bem_pat.cod_empresa  = estabelecimento.cod_empresa
                     and bem_pat.cod_estab    = estabelecimento.cod_estab
                     and bem_pat.cod_cta_pat  = compos_cta_pat_usuar.cod_cta_pat
                   break by bem_pat.cod_empresa
                         by bem_pat.cod_estab :
    
                    run pi_acumula_incorp(input 0).
            
                    for each incorp_bem_pat no-lock
                        where incorp_bem_pat.num_id_bem_pat = bem_pat.num_id_bem_pat:
            
                        run pi_acumula_incorp(input incorp_bem_pat.num_seq_incorp_bem_pat).
                    end.
                end.
    
                assign v_val_adic         = v_val_adic        + v_val_cm
                       v_val_transf       = v_val_impl_transf - v_val_bxa_transf
                       v_val_sdo_imob_cta       = v_val_sdo_inicial_cta   + v_val_adic 
                                                + v_val_transf      - v_val_bxa_imob_total
                       v_val_dpr_transf   = v_val_impl_dpr_transf - v_val_bxa_dpr_transf
                       v_val_dpr_amort_tot = v_val_dpr_amort_ini    + v_val_dpr_amort_per 
                                                + v_val_dpr_transf  - v_val_dpr_amort_bxa.
    
                /* bno
                find first cta_pat no-lock 
                    where cta_pat.cod_cta_pat = compos_cta_pat_usuar.cod_cta_pat
                    and   cta_pat.cod_empresa = v_cod_empres_usuar no-error.
                    */
                find first cta_pat no-lock 
                    where cta_pat.cod_cta_pat = compos_cta_pat_usuar.cod_cta_pat
                    and   cta_pat.cod_empresa = estabelecimento.cod_empresa no-error.
                IF AVAILABLE cta_pat THEN DO:
                    IF v_val_sdo_inicial_cta   <> 0 OR
                       v_val_adic              <> 0 OR
                       v_val_transf            <> 0 OR
                       v_val_bxa_imob_total    <> 0 OR
                       v_val_sdo_imob_cta      <> 0 OR
                       v_val_dpr_amort_ini     <> 0 OR
                       v_val_dpr_amort_per     <> 0 OR
                       v_val_dpr_transf        <> 0 OR
                       v_val_dpr_amort_bxa     <> 0 OR
                       v_val_dpr_amort_tot     <> 0 OR
                       v_val_salvage_value_cta <> 0 THEN DO:
                        ASSIGN chExcelApplication:Range("A" + STRING (i-linha)):VALUE = estabelecimento.cod_empresa.
                        ASSIGN chExcelApplication:Range("B" + STRING (i-linha)):VALUE = estabelecimento.cod_estab.
                        ASSIGN chExcelApplication:Range("C" + STRING (i-linha)):VALUE = grp_cta_pat_usuar.cod_grp_cta_pat_usuar.
                        ASSIGN chExcelApplication:Range("D" + STRING (i-linha)):VALUE = cta_pat.cod_cta_pat.
                        ASSIGN chExcelApplication:Range("E" + STRING (i-linha)):VALUE = cta_pat.des_cta_pat.
                        ASSIGN chExcelApplication:Range("F" + STRING (i-linha)):VALUE = v_val_sdo_inicial_cta  .
                        ASSIGN chExcelApplication:Range("G" + STRING (i-linha)):VALUE = v_val_adic             .
                        ASSIGN chExcelApplication:Range("H" + STRING (i-linha)):VALUE = v_val_transf           .
                        ASSIGN chExcelApplication:Range("I" + STRING (i-linha)):VALUE = v_val_bxa_imob_vendas  .
                        ASSIGN chExcelApplication:Range("J" + STRING (i-linha)):VALUE = v_val_bxa_imob_outros  .
                        ASSIGN chExcelApplication:Range("K" + STRING (i-linha)):VALUE = v_val_sdo_imob_cta     .
                        ASSIGN chExcelApplication:Range("L" + STRING (i-linha)):VALUE = v_val_dpr_amort_ini    .
                        ASSIGN chExcelApplication:Range("M" + STRING (i-linha)):VALUE = v_val_dpr_amort_per    .
                        ASSIGN chExcelApplication:Range("N" + STRING (i-linha)):VALUE = v_val_dpr_transf       .
                        ASSIGN chExcelApplication:Range("O" + STRING (i-linha)):VALUE = v_val_dpr_amort_bxa    .
                        ASSIGN chExcelApplication:Range("P" + STRING (i-linha)):VALUE = v_val_dpr_amort_tot    .
                        ASSIGN chExcelApplication:Range("Q" + STRING (i-linha)):VALUE = v_val_salvage_value_cta.

                        ASSIGN i-linha = i-linha + 1.

                        assign v_val_tot_sdo_inicial_cta   = v_val_tot_sdo_inicial_cta   + v_val_sdo_inicial_cta
                               v_val_tot_adic              = v_val_tot_adic              + v_val_adic
                               v_val_tot_transf            = v_val_tot_transf            + v_val_transf
                               v_val_tot_bxa_imob_total    = v_val_tot_bxa_imob_total    + v_val_bxa_imob_total
                               v_val_tot_bxa_imob_vendas   = v_val_tot_bxa_imob_vendas   + v_val_bxa_imob_vendas
                               v_val_tot_bxa_imob_outros   = v_val_tot_bxa_imob_outros   + v_val_bxa_imob_outros
                               v_val_tot_sdo_imob_cta      = v_val_tot_sdo_imob_cta      + v_val_sdo_imob_cta
                               v_val_tot_dpr_amort_ini     = v_val_tot_dpr_amort_ini     + v_val_dpr_amort_ini
                               v_val_tot_dpr_amort_per     = v_val_tot_dpr_amort_per     + v_val_dpr_amort_per
                               v_val_tot_dpr_amort_bxa     = v_val_tot_dpr_amort_bxa     + v_val_dpr_amort_bxa
                               v_val_tot_dpr_amort_tot     = v_val_tot_dpr_amort_tot     + v_val_dpr_amort_tot
                               v_val_tot_dpr_transf        = v_val_tot_dpr_transf        + v_val_dpr_transf
                               v_val_tot_salvage_value_cta = v_val_tot_salvage_value_cta + v_val_salvage_value_cta.

                        /***************************************Totais por Empresa******************************************/
                        assign v_val_emp_sdo_inicial_cta   = v_val_emp_sdo_inicial_cta   + v_val_sdo_inicial_cta
                               v_val_emp_adic              = v_val_emp_adic              + v_val_adic
                               v_val_emp_transf            = v_val_emp_transf            + v_val_transf
                               v_val_emp_bxa_imob_total    = v_val_emp_bxa_imob_total    + v_val_bxa_imob_total
                               v_val_emp_bxa_imob_vendas   = v_val_emp_bxa_imob_vendas   + v_val_bxa_imob_vendas
                               v_val_emp_bxa_imob_outros   = v_val_emp_bxa_imob_outros   + v_val_bxa_imob_outros
                               v_val_emp_sdo_imob_cta      = v_val_emp_sdo_imob_cta      + v_val_sdo_imob_cta
                               v_val_emp_dpr_amort_ini     = v_val_emp_dpr_amort_ini     + v_val_dpr_amort_ini
                               v_val_emp_dpr_amort_per     = v_val_emp_dpr_amort_per     + v_val_dpr_amort_per
                               v_val_emp_dpr_amort_bxa     = v_val_emp_dpr_amort_bxa     + v_val_dpr_amort_bxa
                               v_val_emp_dpr_amort_tot     = v_val_emp_dpr_amort_tot     + v_val_dpr_amort_tot
                               v_val_emp_dpr_transf        = v_val_emp_dpr_transf        + v_val_dpr_transf
                               v_val_emp_salvage_value_cta = v_val_emp_salvage_value_cta + v_val_salvage_value_cta.
                        /***************************************************************************************************/

                        /************************************** Totais por Estab *******************************************/
                        assign v_val_est_sdo_inicial_cta   = v_val_est_sdo_inicial_cta   + v_val_sdo_inicial_cta
                               v_val_est_adic              = v_val_est_adic              + v_val_adic
                               v_val_est_transf            = v_val_est_transf            + v_val_transf
                               v_val_est_bxa_imob_total    = v_val_est_bxa_imob_total    + v_val_bxa_imob_total
                               v_val_est_bxa_imob_vendas   = v_val_est_bxa_imob_vendas   + v_val_bxa_imob_vendas
                               v_val_est_bxa_imob_outros   = v_val_est_bxa_imob_outros   + v_val_bxa_imob_outros
                               v_val_est_sdo_imob_cta      = v_val_est_sdo_imob_cta      + v_val_sdo_imob_cta
                               v_val_est_dpr_amort_ini     = v_val_est_dpr_amort_ini     + v_val_dpr_amort_ini
                               v_val_est_dpr_amort_per     = v_val_est_dpr_amort_per     + v_val_dpr_amort_per
                               v_val_est_dpr_amort_bxa     = v_val_est_dpr_amort_bxa     + v_val_dpr_amort_bxa
                               v_val_est_dpr_amort_tot     = v_val_est_dpr_amort_tot     + v_val_dpr_amort_tot
                               v_val_est_dpr_transf        = v_val_est_dpr_transf        + v_val_dpr_transf
                               v_val_est_salvage_value_cta = v_val_est_salvage_value_cta + v_val_salvage_value_cta.
                        /***************************************************************************************************/

                        assign v_val_sdo_inicial_cta = 0 v_val_sdo_imob_cta      = 0
                               v_val_adic            = 0 v_val_dpr_amort_ini     = 0
                               v_val_cm              = 0 v_val_dpr_amort_per     = 0
                               v_val_transf          = 0 v_val_dpr_amort_bxa     = 0
                               v_val_bxa_imob_total  = 0 v_val_dpr_amort_tot     = 0
                               v_val_impl_transf     = 0 v_val_bxa_transf        = 0
                               v_val_impl_dpr_transf = 0 v_val_bxa_dpr_transf    = 0
                               v_val_dpr_transf      = 0 v_val_salvage_value_cta = 0.
                        ASSIGN v_val_bxa_imob_vendas = 0 v_val_bxa_imob_outros = 0.
                    END.
                END.

    
                if last-of(compos_cta_pat_usuar.cod_grp_cta_pat_usuar) then do:
                    IF v_val_tot_sdo_inicial_cta   <> 0 OR
                       v_val_tot_adic              <> 0 OR
                       v_val_tot_transf            <> 0 OR
                       v_val_tot_bxa_imob_total    <> 0 OR
                       v_val_tot_sdo_imob_cta      <> 0 OR
                       v_val_tot_dpr_amort_ini     <> 0 OR
                       v_val_tot_dpr_amort_per     <> 0 OR
                       v_val_tot_dpr_transf        <> 0 OR
                       v_val_tot_dpr_amort_bxa     <> 0 OR
                       v_val_tot_dpr_amort_tot     <> 0 OR
                       v_val_tot_salvage_value_cta <> 0 THEN DO:
                        ASSIGN i-linha = i-linha + 1.

                        ASSIGN chExcelApplication:Range("A" + STRING (i-linha)):VALUE = "Total por Grupo:".
                        ASSIGN chExcelApplication:Range("B" + STRING (i-linha)):VALUE = compos_cta_pat_usuar.cod_grp_cta_pat_usuar.
                        ASSIGN chExcelApplication:Range("F" + STRING (i-linha)):VALUE = v_val_tot_sdo_inicial_cta  .
                        ASSIGN chExcelApplication:Range("G" + STRING (i-linha)):VALUE = v_val_tot_adic             .
                        ASSIGN chExcelApplication:Range("H" + STRING (i-linha)):VALUE = v_val_tot_transf           .
                        ASSIGN chExcelApplication:Range("I" + STRING (i-linha)):VALUE = v_val_tot_bxa_imob_vendas  .
                        ASSIGN chExcelApplication:Range("J" + STRING (i-linha)):VALUE = v_val_tot_bxa_imob_outros  .
                        ASSIGN chExcelApplication:Range("K" + STRING (i-linha)):VALUE = v_val_tot_sdo_imob_cta     .
                        ASSIGN chExcelApplication:Range("L" + STRING (i-linha)):VALUE = v_val_tot_dpr_amort_ini    .
                        ASSIGN chExcelApplication:Range("M" + STRING (i-linha)):VALUE = v_val_tot_dpr_amort_per    .
                        ASSIGN chExcelApplication:Range("N" + STRING (i-linha)):VALUE = v_val_tot_dpr_transf       .
                        ASSIGN chExcelApplication:Range("O" + STRING (i-linha)):VALUE = v_val_tot_dpr_amort_bxa    .
                        ASSIGN chExcelApplication:Range("P" + STRING (i-linha)):VALUE = v_val_tot_dpr_amort_tot    .
                        ASSIGN chExcelApplication:Range("Q" + STRING (i-linha)):VALUE = v_val_tot_salvage_value_cta.

                        ASSIGN i-linha = i-linha + 2.

                        assign v_val_g_sdo_inicial_cta   = v_val_g_sdo_inicial_cta   + v_val_tot_sdo_inicial_cta
                               v_val_g_adic              = v_val_g_adic              + v_val_tot_adic
                               v_val_g_transf            = v_val_g_transf            + v_val_tot_transf
                               v_val_g_bxa_imob_total    = v_val_g_bxa_imob_total    + v_val_tot_bxa_imob_total
                               v_val_g_bxa_imob_vendas   = v_val_g_bxa_imob_vendas   + v_val_tot_bxa_imob_vendas
                               v_val_g_bxa_imob_outros   = v_val_g_bxa_imob_outros   + v_val_tot_bxa_imob_outros
                               v_val_g_sdo_imob_cta      = v_val_g_sdo_imob_cta      + v_val_tot_sdo_imob_cta
                               v_val_g_dpr_amort_ini     = v_val_g_dpr_amort_ini     + v_val_tot_dpr_amort_ini 
                               v_val_g_dpr_amort_per     = v_val_g_dpr_amort_per     + v_val_tot_dpr_amort_per
                               v_val_g_dpr_amort_bxa     = v_val_g_dpr_amort_bxa     + v_val_tot_dpr_amort_bxa
                               v_val_g_dpr_amort_tot     = v_val_g_dpr_amort_tot     + v_val_tot_dpr_amort_tot
                               v_val_g_dpr_transf        = v_val_g_dpr_transf        + v_val_tot_dpr_transf
                               v_val_g_salvage_value_cta = v_val_g_salvage_value_cta + v_val_tot_salvage_value_cta.

                        assign v_val_tot_sdo_inicial_cta   = 0 v_val_tot_dpr_amort_ini = 0
                               v_val_tot_adic              = 0 v_val_tot_dpr_amort_per = 0
                               v_val_tot_transf            = 0 v_val_tot_dpr_amort_bxa = 0
                               v_val_tot_bxa_imob_total    = 0 v_val_tot_dpr_amort_tot = 0
                               v_val_tot_sdo_imob_cta      = 0 v_val_tot_dpr_transf    = 0
                               v_val_tot_salvage_value_cta = 0.
                        ASSIGN v_val_tot_bxa_imob_vendas = 0 v_val_tot_bxa_imob_outros = 0.
                    END.
                end.
            end.
        end.

        if last-of(estabelecimento.cod_estab) then do:
            IF v_val_est_sdo_inicial_cta   <> 0 OR
               v_val_est_adic              <> 0 OR
               v_val_est_transf            <> 0 OR
               v_val_est_bxa_imob_total    <> 0 OR
               v_val_est_sdo_imob_cta      <> 0 OR
               v_val_est_dpr_amort_ini     <> 0 OR
               v_val_est_dpr_amort_per     <> 0 OR
               v_val_est_dpr_transf        <> 0 OR
               v_val_est_dpr_amort_bxa     <> 0 OR
               v_val_est_dpr_amort_tot     <> 0 OR
               v_val_est_salvage_value_cta <> 0 THEN DO:
                ASSIGN chExcelApplication:Range("A" + STRING (i-linha)):VALUE = "Total Estabelecimento:".
                ASSIGN chExcelApplication:Range("B" + STRING (i-linha)):VALUE = estabelecimento.cod_estab.
                ASSIGN chExcelApplication:Range("F" + STRING (i-linha)):VALUE = v_val_est_sdo_inicial_cta  .
                ASSIGN chExcelApplication:Range("G" + STRING (i-linha)):VALUE = v_val_est_adic             .
                ASSIGN chExcelApplication:Range("H" + STRING (i-linha)):VALUE = v_val_est_transf           .
                ASSIGN chExcelApplication:Range("I" + STRING (i-linha)):VALUE = v_val_est_bxa_imob_vendas  .
                ASSIGN chExcelApplication:Range("J" + STRING (i-linha)):VALUE = v_val_est_bxa_imob_outros  .
                ASSIGN chExcelApplication:Range("K" + STRING (i-linha)):VALUE = v_val_est_sdo_imob_cta     .
                ASSIGN chExcelApplication:Range("L" + STRING (i-linha)):VALUE = v_val_est_dpr_amort_ini    .
                ASSIGN chExcelApplication:Range("M" + STRING (i-linha)):VALUE = v_val_est_dpr_amort_per    .
                ASSIGN chExcelApplication:Range("N" + STRING (i-linha)):VALUE = v_val_est_dpr_transf       .
                ASSIGN chExcelApplication:Range("O" + STRING (i-linha)):VALUE = v_val_est_dpr_amort_bxa    .
                ASSIGN chExcelApplication:Range("P" + STRING (i-linha)):VALUE = v_val_est_dpr_amort_tot    .
                ASSIGN chExcelApplication:Range("Q" + STRING (i-linha)):VALUE = v_val_est_salvage_value_cta.

                ASSIGN i-linha = i-linha + 1.

                assign v_val_est_sdo_inicial_cta   = 0 v_val_est_dpr_amort_ini = 0
                       v_val_est_adic              = 0 v_val_est_dpr_amort_per = 0
                       v_val_est_transf            = 0 v_val_est_dpr_amort_bxa = 0
                       v_val_est_bxa_imob_total    = 0 v_val_est_dpr_amort_tot = 0
                       v_val_est_sdo_imob_cta      = 0 v_val_est_dpr_transf    = 0
                       v_val_est_salvage_value_cta = 0.
                ASSIGN v_val_est_bxa_imob_vendas = 0 v_val_est_bxa_imob_outros = 0.
            END.
        end.

        if last-of(estabelecimento.cod_empresa) then do:
            IF v_val_emp_sdo_inicial_cta   <> 0 OR
               v_val_emp_adic              <> 0 OR
               v_val_emp_transf            <> 0 OR
               v_val_emp_bxa_imob_total    <> 0 OR
               v_val_emp_sdo_imob_cta      <> 0 OR
               v_val_emp_dpr_amort_ini     <> 0 OR
               v_val_emp_dpr_amort_per     <> 0 OR
               v_val_emp_dpr_transf        <> 0 OR
               v_val_emp_dpr_amort_bxa     <> 0 OR
               v_val_emp_dpr_amort_tot     <> 0 OR
               v_val_emp_salvage_value_cta <> 0 THEN DO:
                ASSIGN i-linha = i-linha + 1.

                ASSIGN chExcelApplication:Range("A" + STRING (i-linha)):VALUE = "Total Empresa:".
                ASSIGN chExcelApplication:Range("B" + STRING (i-linha)):VALUE = estabelecimento.cod_empresa.
                ASSIGN chExcelApplication:Range("F" + STRING (i-linha)):VALUE = v_val_emp_sdo_inicial_cta  .
                ASSIGN chExcelApplication:Range("G" + STRING (i-linha)):VALUE = v_val_emp_adic             .
                ASSIGN chExcelApplication:Range("H" + STRING (i-linha)):VALUE = v_val_emp_transf           .
                ASSIGN chExcelApplication:Range("I" + STRING (i-linha)):VALUE = v_val_emp_bxa_imob_vendas  .
                ASSIGN chExcelApplication:Range("J" + STRING (i-linha)):VALUE = v_val_emp_bxa_imob_outros  .
                ASSIGN chExcelApplication:Range("K" + STRING (i-linha)):VALUE = v_val_emp_sdo_imob_cta     .
                ASSIGN chExcelApplication:Range("L" + STRING (i-linha)):VALUE = v_val_emp_dpr_amort_ini    .
                ASSIGN chExcelApplication:Range("M" + STRING (i-linha)):VALUE = v_val_emp_dpr_amort_per    .
                ASSIGN chExcelApplication:Range("N" + STRING (i-linha)):VALUE = v_val_emp_dpr_transf       .
                ASSIGN chExcelApplication:Range("O" + STRING (i-linha)):VALUE = v_val_emp_dpr_amort_bxa    .
                ASSIGN chExcelApplication:Range("P" + STRING (i-linha)):VALUE = v_val_emp_dpr_amort_tot    .
                ASSIGN chExcelApplication:Range("Q" + STRING (i-linha)):VALUE = v_val_emp_salvage_value_cta.

                ASSIGN i-linha = i-linha + 1.

                assign v_val_emp_sdo_inicial_cta   = 0 v_val_emp_dpr_amort_ini = 0
                       v_val_emp_adic              = 0 v_val_emp_dpr_amort_per = 0
                       v_val_emp_transf            = 0 v_val_emp_dpr_amort_bxa = 0
                       v_val_emp_bxa_imob_total    = 0 v_val_emp_dpr_amort_tot = 0
                       v_val_emp_sdo_imob_cta      = 0 v_val_emp_dpr_transf    = 0
                       v_val_emp_salvage_value_cta = 0.
                ASSIGN v_val_emp_bxa_imob_vendas = 0 v_val_emp_bxa_imob_outros = 0.
            END.
        end.
    end.

    ASSIGN i-linha = i-linha + 1.

    ASSIGN chExcelApplication:Range("A" + STRING (i-linha)):VALUE = "Total Geral:".
    ASSIGN chExcelApplication:Range("F" + STRING (i-linha)):VALUE = v_val_g_sdo_inicial_cta  .
    ASSIGN chExcelApplication:Range("G" + STRING (i-linha)):VALUE = v_val_g_adic             .
    ASSIGN chExcelApplication:Range("H" + STRING (i-linha)):VALUE = v_val_g_transf           .
    ASSIGN chExcelApplication:Range("I" + STRING (i-linha)):VALUE = v_val_g_bxa_imob_vendas  .
    ASSIGN chExcelApplication:Range("J" + STRING (i-linha)):VALUE = v_val_g_bxa_imob_outros  .
    ASSIGN chExcelApplication:Range("K" + STRING (i-linha)):VALUE = v_val_g_sdo_imob_cta     .
    ASSIGN chExcelApplication:Range("L" + STRING (i-linha)):VALUE = v_val_g_dpr_amort_ini    .
    ASSIGN chExcelApplication:Range("M" + STRING (i-linha)):VALUE = v_val_g_dpr_amort_per    .
    ASSIGN chExcelApplication:Range("N" + STRING (i-linha)):VALUE = v_val_g_dpr_transf       .
    ASSIGN chExcelApplication:Range("O" + STRING (i-linha)):VALUE = v_val_g_dpr_amort_bxa    .
    ASSIGN chExcelApplication:Range("P" + STRING (i-linha)):VALUE = v_val_g_dpr_amort_tot    .
    ASSIGN chExcelApplication:Range("Q" + STRING (i-linha)):VALUE = v_val_g_salvage_value_cta.

    ASSIGN i-linha = i-linha + 1.

    assign v_val_g_sdo_inicial_cta   = 0 v_val_g_dpr_amort_ini = 0
           v_val_g_adic              = 0 v_val_g_dpr_amort_per = 0
           v_val_g_transf            = 0 v_val_g_dpr_amort_bxa = 0
           v_val_g_bxa_imob_total    = 0 v_val_g_dpr_amort_tot = 0
           v_val_g_sdo_imob_cta      = 0 v_val_g_dpr_transf    = 0
           v_val_g_salvage_value_cta = 0.
    ASSIGN v_val_g_bxa_imob_vendas = 0 v_val_g_bxa_imob_outros = 0.

    chExcelApplication:VISIBLE = TRUE.

    RELEASE object chExcelApplication.      
    RELEASE object chWorkbook.
    RELEASE object chWorksheet.

END PROCEDURE.

procedure pi_acumula_incorp:
    def input param p_seq_incorp as integer no-undo.

    find last reg_calc_bem_pat no-lock
        where reg_calc_bem_pat.num_id_bem_pat         = bem_pat.num_id_bem_pat
          and reg_calc_bem_pat.num_seq_incorp_bem_pat = p_seq_incorp
          and reg_calc_bem_pat.dat_calc_pat           < v_dat_inic no-error.
    if  avail reg_calc_bem_pat and 
        not (reg_calc_bem_pat.ind_trans_calc_bem_pat = "Implantaá∆o" and
             reg_calc_bem_pat.ind_orig_calc_bem_pat  = "Aquisiá∆o") then do:

        find last sdo_bem_pat no-lock
            where sdo_bem_pat.num_id_bem_pat         = bem_pat.num_id_bem_pat
              and sdo_bem_pat.cod_cenar_ctbl         = v_cod_cenar_ctbl
              and sdo_bem_pat.cod_finalid_econ       = v_cod_finalid_econ
              and sdo_bem_pat.dat_sdo_bem_pat       <= v_dat_inic
              and sdo_bem_pat.num_seq_incorp_bem_pat = p_seq_incorp no-error.

        find first bf_sdo_bem_pat no-lock
             where bf_sdo_bem_pat.num_id_bem_pat         = bem_pat.num_id_bem_pat
               and bf_sdo_bem_pat.cod_cenar_ctbl         = v_cod_cenar_ctbl
               and bf_sdo_bem_pat.cod_finalid_econ       = v_cod_finalid_econ
               and bf_sdo_bem_pat.dat_sdo_bem_pat        > 01/01/0001
               and bf_sdo_bem_pat.num_seq_incorp_bem_pat = p_seq_incorp no-error.

        if  avail sdo_bem_pat then do:
            if  (recid(bf_sdo_bem_pat)        <> recid(sdo_bem_pat)) or
                (recid(bf_sdo_bem_pat)         = recid(sdo_bem_pat) and 
                 sdo_bem_pat.dat_sdo_bem_pat <= v_dat_inic) then do:

                assign v_val_sdo_inicial_cta  = v_val_sdo_inicial_cta  + sdo_bem_pat.val_origin_corrig
                       v_val_dpr_amort_ini   = v_val_dpr_amort_ini   + sdo_bem_pat.val_dpr_val_origin 
                                              + sdo_bem_pat.val_dpr_cm + sdo_bem_pat.val_cm_dpr
                       v_val_dpr_amort_ini   = v_val_dpr_amort_ini   + sdo_bem_pat.val_dpr_val_origin_amort 
                                              + sdo_bem_pat.val_dpr_cm_amort + sdo_bem_pat.val_cm_dpr_amort.
            end.
        end.
        else do:
            if avail bf_sdo_bem_pat and reg_calc_bem_pat.ind_orig_calc_bem_pat = "Imobilizado" then
                assign v_val_sdo_inicial_cta = v_val_sdo_inicial_cta + bf_sdo_bem_pat.val_original.
        end.
    end.    
    else do:
        find last sdo_bem_pat no-lock
            where sdo_bem_pat.num_id_bem_pat         = bem_pat.num_id_bem_pat
              and sdo_bem_pat.cod_cenar_ctbl         = v_cod_cenar_ctbl
              and sdo_bem_pat.cod_finalid_econ       = v_cod_finalid_econ
              and sdo_bem_pat.dat_sdo_bem_pat        < v_dat_inic
              and sdo_bem_pat.num_seq_incorp_bem_pat = p_seq_incorp no-error.

        find first bf_sdo_bem_pat no-lock
             where bf_sdo_bem_pat.num_id_bem_pat         = bem_pat.num_id_bem_pat
               and bf_sdo_bem_pat.cod_cenar_ctbl         = v_cod_cenar_ctbl
               and bf_sdo_bem_pat.cod_finalid_econ       = v_cod_finalid_econ
               and bf_sdo_bem_pat.dat_sdo_bem_pat        > 01/01/0001
               and bf_sdo_bem_pat.num_seq_incorp_bem_pat = p_seq_incorp no-error.

        if avail sdo_bem_pat then do:
           if  (recid(bf_sdo_bem_pat)       <> recid(sdo_bem_pat)) or 
               (recid(bf_sdo_bem_pat)        = recid(sdo_bem_pat) and
                sdo_bem_pat.dat_sdo_bem_pat <= v_dat_inic) then do: 

               assign v_val_sdo_inicial_cta = v_val_sdo_inicial_cta  + sdo_bem_pat.val_origin_corrig
                      v_val_dpr_amort_ini  = v_val_dpr_amort_ini   + sdo_bem_pat.val_dpr_val_origin 
                                            + sdo_bem_pat.val_dpr_cm + sdo_bem_pat.val_cm_dpr
                      v_val_dpr_amort_ini  = v_val_dpr_amort_ini   + sdo_bem_pat.val_dpr_val_origin_amort 
                                            + sdo_bem_pat.val_dpr_cm_amort + sdo_bem_pat.val_cm_dpr_amort.
           end.
        end.
    end.

    for each reg_calc_bem_pat no-lock
       where reg_calc_bem_pat.num_id_bem_pat         = bem_pat.num_id_bem_pat
         and reg_calc_bem_pat.num_seq_incorp_bem_pat = p_seq_incorp
         and reg_calc_bem_pat.cod_cenar_ctbl         = v_cod_cenar_ctbl
         and reg_calc_bem_pat.cod_finalid_econ       = v_cod_finalid_econ
         and reg_calc_bem_pat.dat_calc_pat          >= v_dat_inic
         and reg_calc_bem_pat.dat_calc_pat          <= v_dat_fim
       break by reg_calc_bem_pat.num_seq_incorp_bem_pat:

        if  reg_calc_bem_pat.ind_trans_calc_bem_pat = "Depreciaá∆o" then
            assign v_val_dpr_amort_per  = v_val_dpr_amort_per + reg_calc_bem_pat.val_dpr_val_origin 
                                               + reg_calc_bem_pat.val_dpr_cm + reg_calc_bem_pat.val_cm_dpr.
        
        if  reg_calc_bem_pat.ind_trans_calc_bem_pat = "Amortizaá∆o" then
            assign v_val_dpr_amort_per  = v_val_dpr_amort_per + reg_calc_bem_pat.val_dpr_val_origin 
                                               + reg_calc_bem_pat.val_dpr_cm + reg_calc_bem_pat.val_cm_dpr.
        
        if  reg_calc_bem_pat.ind_trans_calc_bem_pat = "Correá∆o Monet†ria" then
            assign v_val_cm = v_val_cm + reg_calc_bem_pat.val_cm.
        
        if reg_calc_bem_pat.ind_trans_calc_bem_pat = "Implantaá∆o" then do:

            if  reg_calc_bem_pat.ind_orig_calc_bem_pat = "Aquisiá∆o"     or
                reg_calc_bem_pat.ind_orig_calc_bem_pat = "Adiá∆o"        or
                reg_calc_bem_pat.ind_orig_calc_bem_pat = "Benfeitoria"   or
                reg_calc_bem_pat.ind_orig_calc_bem_pat = "Difer Cambial" or
                reg_calc_bem_pat.ind_orig_calc_bem_pat = "Imobilizado"   or
                reg_calc_bem_pat.ind_orig_calc_bem_pat = "Migraá∆o"      or
                reg_calc_bem_pat.ind_orig_calc_bem_pat = "Reavaliaá∆o"  then
                assign v_val_adic = v_val_adic + reg_calc_bem_pat.val_original.
            else 
                if  reg_calc_bem_pat.ind_orig_calc_bem_pat = "Alteraá∆o"       or
                    reg_calc_bem_pat.ind_orig_calc_bem_pat = "Desmembramento"  or
                    reg_calc_bem_pat.ind_orig_calc_bem_pat = "Reclassificaá∆o" or
                    reg_calc_bem_pat.ind_orig_calc_bem_pat = "Uni∆o"           then do:
                    
                    if  reg_calc_bem_pat.ind_tip_calc = "" or
                        reg_calc_bem_pat.cod_tip_calc = "" then
                        assign v_val_impl_transf = v_val_impl_transf + reg_calc_bem_pat.val_original.
                    
                    if  reg_calc_bem_pat.ind_tip_calc = "Correá∆o Monet†ria" then 
                        assign v_val_impl_transf = v_val_impl_transf + reg_calc_bem_pat.val_cm.
                    
                    if  reg_calc_bem_pat.ind_tip_calc = "Depreciaá∆o" then
                        assign v_val_impl_dpr_transf = v_val_impl_dpr_transf + (reg_calc_bem_pat.val_dpr_val_origin 
                                                  + reg_calc_bem_pat.val_dpr_cm + reg_calc_bem_pat.val_cm_dpr).
                    
                    if  reg_calc_bem_pat.ind_tip_calc = "Amortizaá∆o" then
                        assign v_val_impl_dpr_transf = v_val_impl_dpr_transf + (reg_calc_bem_pat.val_dpr_val_origin 
                                                  + reg_calc_bem_pat.val_dpr_cm + reg_calc_bem_pat.val_cm_dpr).
                end.
        end.
        else 
            if  reg_calc_bem_pat.ind_trans_calc_bem_pat = "Baixa" then do:
                
                if  reg_calc_bem_pat.ind_orig_calc_bem_pat = "Inutilizaá∆o" or
                    reg_calc_bem_pat.ind_orig_calc_bem_pat = "Devoluá∆o"    or
                    reg_calc_bem_pat.ind_orig_calc_bem_pat = "Exaust∆o"     or
                    reg_calc_bem_pat.ind_orig_calc_bem_pat = "Quebra"       or 
                    reg_calc_bem_pat.ind_orig_calc_bem_pat = "Venda"        then do:

                    if  reg_calc_bem_pat.ind_tip_calc = "" THEN DO:
                        assign v_val_bxa_imob_total = v_val_bxa_imob_total + reg_calc_bem_pat.val_original.

                        IF reg_calc_bem_pat.ind_orig_calc_bem_pat = "Venda" THEN
                            assign v_val_bxa_imob_vendas = v_val_bxa_imob_vendas + reg_calc_bem_pat.val_original.
                        ELSE
                            assign v_val_bxa_imob_outros = v_val_bxa_imob_outros + reg_calc_bem_pat.val_original.
                    END.

                    if  reg_calc_bem_pat.ind_tip_calc = "Amortizaá∆o" then
                        assign v_val_dpr_amort_bxa = v_val_dpr_amort_bxa + reg_calc_bem_pat.val_dpr_val_origin 
                                                         + reg_calc_bem_pat.val_dpr_cm + reg_calc_bem_pat.val_cm_dpr.
                    
                    if  reg_calc_bem_pat.ind_tip_calc = "Correá∆o Monet†ria" THEN DO:
                        assign v_val_bxa_imob_total = v_val_bxa_imob_total + reg_calc_bem_pat.val_cm.

                        IF reg_calc_bem_pat.ind_orig_calc_bem_pat = "Venda" THEN
                            assign v_val_bxa_imob_vendas = v_val_bxa_imob_vendas + reg_calc_bem_pat.val_cm.
                        ELSE
                            assign v_val_bxa_imob_outros = v_val_bxa_imob_outros + reg_calc_bem_pat.val_cm.
                    END.
                    
                    if  reg_calc_bem_pat.ind_tip_calc = "Depreciaá∆o" then
                        assign v_val_dpr_amort_bxa = v_val_dpr_amort_bxa + reg_calc_bem_pat.val_dpr_val_origin 
                                                         + reg_calc_bem_pat.val_dpr_cm + reg_calc_bem_pat.val_cm_dpr.
                    
                end.
    
                if  reg_calc_bem_pat.ind_orig_calc_bem_pat = "Alteraá∆o"       or
                    reg_calc_bem_pat.ind_orig_calc_bem_pat = "Desmembramento"  or
                    reg_calc_bem_pat.ind_orig_calc_bem_pat = "Reclassificaá∆o" or
                    reg_calc_bem_pat.ind_orig_calc_bem_pat = "Uni∆o"           then do:

                    if  reg_calc_bem_pat.ind_tip_calc = "Amortizaá∆o" then
                        assign v_val_bxa_dpr_transf = v_val_bxa_dpr_transf + (reg_calc_bem_pat.val_dpr_val_origin 
                                                      + reg_calc_bem_pat.val_dpr_cm + reg_calc_bem_pat.val_cm_dpr).

                    if  reg_calc_bem_pat.ind_tip_calc = "" then
                        assign v_val_bxa_transf = v_val_bxa_transf + reg_calc_bem_pat.val_original.
                    
                    if  reg_calc_bem_pat.ind_tip_calc = "Correá∆o Monet†ria" then
                        assign v_val_bxa_transf = v_val_bxa_transf + reg_calc_bem_pat.val_cm.
                    
                    if  reg_calc_bem_pat.ind_tip_calc = "Depreciaá∆o" then
                        assign v_val_bxa_dpr_transf = v_val_bxa_dpr_transf + (reg_calc_bem_pat.val_dpr_val_origin 
                                                      + reg_calc_bem_pat.val_dpr_cm + reg_calc_bem_pat.val_cm_dpr).
                    
                end.
            end.
    end.

    FIND FIRST bem_pat_salvage_value WHERE bem_pat_salvage_value.num_id_bem_pat         = bem_pat.num_id_bem_pat AND
                                           bem_pat_salvage_value.num_seq_incorp_bem_pat = p_seq_incorp           AND
                                           bem_pat_salvage_value.cod_cenar_ctbl         = v_cod_cenar_ctbl       AND
                                           bem_pat_salvage_value.cod_indic_econ         = v_cod_finalid_econ     NO-LOCK NO-ERROR.
    IF AVAILABLE bem_pat_salvage_value THEN
        ASSIGN v_val_salvage_value_cta = v_val_salvage_value_cta + bem_pat_salvage_value.val_salvage_value.

END PROCEDURE.

/*****************************************************************************
** Procedure Interna.....: pi_version_extract
** Descricao.............: pi_version_extract
** Criado por............: jaison
** Criado em.............: 31/07/1998 09:33:22
** Alterado por..........: tech14013
** Alterado em...........: 05/01/2005 19:27:44
*****************************************************************************/
PROCEDURE pi_version_extract:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_program
        as character
        format "x(08)"
        no-undo.
    def Input param p_cod_program_ext
        as character
        format "x(8)"
        no-undo.
    def Input param p_cod_version
        as character
        format "x(8)"
        no-undo.
    def Input param p_cod_program_type
        as character
        format "x(8)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_cod_event_dic
        as character
        format "x(20)":U
        label "Evento"
        column-label "Evento"
        no-undo.
    def var v_cod_tabela
        as character
        format "x(28)":U
        label "Tabela"
        column-label "Tabela"
        no-undo.


    /************************** Variable Definition End *************************/

    if  can-do(v_cod_tip_prog, p_cod_program_type)
    then do:
        if p_cod_program_type = 'dic' then 
           assign p_cod_program_ext = replace(p_cod_program_ext, 'database/', '').

        output stream s-arq to value(v_cod_arq) append.

        put stream s-arq unformatted
            p_cod_program            at 1 
            p_cod_program_ext        at 43 
            p_cod_version            at 69 
            today                    at 84 
            string(time, 'HH:MM:SS') at 94 skip.

        if  p_cod_program_type = 'pro' then do:
            &if '{&emsbas_version}' > '1.00' &then
            find prog_dtsul 
                where prog_dtsul.cod_prog_dtsul = p_cod_program 
                no-lock no-error.
            if  avail prog_dtsul
            then do:
                &if '{&emsbas_version}' > '5.00' &then
                    if  prog_dtsul.nom_prog_dpc <> '' then
                        put stream s-arq 'DPC : ' at 5 prog_dtsul.nom_prog_dpc  at 15 skip.
                &endif
                if  prog_dtsul.nom_prog_appc <> '' then
                    put stream s-arq 'APPC: ' at 5 prog_dtsul.nom_prog_appc at 15 skip.
                if  prog_dtsul.nom_prog_upc <> '' then
                    put stream s-arq 'UPC : ' at 5 prog_dtsul.nom_prog_upc  at 15 skip.
            end /* if */.
            &endif
        end.

        if  p_cod_program_type = 'dic' then do:
            &if '{&emsbas_version}' > '1.00' &then
            assign v_cod_event_dic = ENTRY(1,p_cod_program ,'/':U)
                   v_cod_tabela    = ENTRY(2,p_cod_program ,'/':U). /* FO 1100.980 */
            find tab_dic_dtsul 
                where tab_dic_dtsul.cod_tab_dic_dtsul = v_cod_tabela 
                no-lock no-error.
            if  avail tab_dic_dtsul
            then do:
                &if '{&emsbas_version}' > '5.00' &then
                    if  tab_dic_dtsul.nom_prog_dpc_gat_delete <> '' and v_cod_event_dic = 'Delete':U then
                        put stream s-arq 'DPC-DELETE : ' at 5 tab_dic_dtsul.nom_prog_dpc_gat_delete  at 25 skip.
                &endif
                if  tab_dic_dtsul.nom_prog_appc_gat_delete <> '' and v_cod_event_dic = 'Delete':U then
                    put stream s-arq 'APPC-DELETE: ' at 5 tab_dic_dtsul.nom_prog_appc_gat_delete at 25 skip.
                if  tab_dic_dtsul.nom_prog_upc_gat_delete <> '' and v_cod_event_dic = 'Delete':U then
                    put stream s-arq 'UPC-DELETE : ' at 5 tab_dic_dtsul.nom_prog_upc_gat_delete  at 25 skip.
                &if '{&emsbas_version}' > '5.00' &then
                    if  tab_dic_dtsul.nom_prog_dpc_gat_write <> '' and v_cod_event_dic = 'Write':U then
                        put stream s-arq 'DPC-WRITE : ' at 5 tab_dic_dtsul.nom_prog_dpc_gat_write  at 25 skip.
                &endif
                if  tab_dic_dtsul.nom_prog_appc_gat_write <> '' and v_cod_event_dic = 'Write':U then
                    put stream s-arq 'APPC-WRITE: ' at 5 tab_dic_dtsul.nom_prog_appc_gat_write at 25 skip.
                if  tab_dic_dtsul.nom_prog_upc_gat_write <> '' and v_cod_event_dic = 'Write':U  then
                    put stream s-arq 'UPC-WRITE : ' at 5 tab_dic_dtsul.nom_prog_upc_gat_write  at 25 skip.
            end /* if */.
            &endif
        end.

        output stream s-arq close.
    end /* if */.

END PROCEDURE. /* pi_version_extract */

/************************** Internal Procedure End **************************/

/*****************************************************************************
**  Procedure Interna: pi_print_editor
**  Descricao........: Imprime editores nos relat¢rios
*****************************************************************************/
PROCEDURE pi_print_editor:

    def input param p_stream    as char    no-undo.
    def input param p1_editor   as char    no-undo.
    def input param p1_pos      as char    no-undo.
    def input param p2_editor   as char    no-undo.
    def input param p2_pos      as char    no-undo.
    def input param p3_editor   as char    no-undo.
    def input param p3_pos      as char    no-undo.

    def var c_editor as char    extent 5             no-undo.
    def var l_first  as logical extent 5 initial yes no-undo.
    def var c_at     as char    extent 5             no-undo.
    def var i_pos    as integer extent 5             no-undo.
    def var i_len    as integer extent 5             no-undo.

    def var c_aux    as char               no-undo.
    def var i_aux    as integer            no-undo.
    def var c_ret    as char               no-undo.
    def var i_ind    as integer            no-undo.

    assign c_editor [1] = p1_editor
           c_at  [1]    =         substr(p1_pos,1,2)
           i_pos [1]    = integer(substr(p1_pos,3,3))
           i_len [1]    = integer(substr(p1_pos,6,3)) - 4
           c_editor [2] = p2_editor
           c_at  [2]    =         substr(p2_pos,1,2)
           i_pos [2]    = integer(substr(p2_pos,3,3))
           i_len [2]    = integer(substr(p2_pos,6,3)) - 4
           c_editor [3] = p3_editor
           c_at  [3]    =         substr(p3_pos,1,2)
           i_pos [3]    = integer(substr(p3_pos,3,3))
           i_len [3]    = integer(substr(p3_pos,6,3)) - 4
           c_ret        = chr(255) + chr(255).

    do while c_editor [1] <> "" or c_editor [2] <> "" or c_editor [3] <> "":
        do i_ind = 1 to 3:
            if c_editor[i_ind] <> "" then do:
                assign i_aux = index(c_editor[i_ind], chr(10)).
                if i_aux > i_len[i_ind] or (i_aux = 0 and length(c_editor[i_ind]) > i_len[i_ind]) then
                    assign i_aux = r-index(c_editor[i_ind], " ", i_len[i_ind] + 1).
                if i_aux = 0 then
                    assign c_aux = substr(c_editor[i_ind], 1, i_len[i_ind])
                           c_editor[i_ind] = substr(c_editor[i_ind], i_len[i_ind] + 1).
                else
                    assign c_aux = substr(c_editor[i_ind], 1, i_aux - 1)
                           c_editor[i_ind] = substr(c_editor[i_ind], i_aux + 1).
                if i_pos[1] = 0 then
                    assign entry(i_ind, c_ret, chr(255)) = c_aux.
                else
                    if l_first[i_ind] then
                        assign l_first[i_ind] = no.
                    else
                        case p_stream:
                            when "s_1" then
                                if c_at[i_ind] = "at" then
                                    put stream s_1 unformatted c_aux at i_pos[i_ind].
                                else
                                    put stream s_1 unformatted c_aux to i_pos[i_ind].
                        end.
            end.
        end.
        case p_stream:
        when "s_1" then
            put stream s_1 unformatted skip.
        end.
        if i_pos[1] = 0 then
            return c_ret.
    end.
    return c_ret.
END PROCEDURE.  /* pi_print_editor */
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
/********************  End of rpt_razao_grp_cta_espec *******************/


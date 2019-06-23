 /****************************************************************************
** Programa..............: fnc_exporta_lote_ctbl 
** Descricao.............: Envio de Lotes Cont†beis Unificados
** Procedimento..........: fnc_exporta_lote_ctbl
** Nome Externo..........: esp/esfgl072.p 
** Criado por............: Hilton Borba
** Criado em.............: 04/10/2011
*****************************************************************************/
def var c-versao-prg as char initial " 1.00.00.000":U no-undo.

/* buffer para tabelas iguais no ems2 e ems5 - ini */
def buffer empresa                for emsuni.empresa.
/* buffer para tabelas iguais no ems2 e ems5 - fim */

{include/i_dbinst.i}
{include/i_dbtype.i}
{include/i_fcldef.i}

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i esfgl072 FGL}
&ENDIF

/******************************* Private-Data *******************************/
assign this-procedure:private-data = "HLP=2":U.
/*************************************  *************************************/

&if "{&emsfin_dbinst}" <> "yes" &then
run pi_messages (input "show", input 5884,
                 input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9",
                                    "EMSFIN")) /*msg_5884*/.
&elseif "{&emsfin_version}" < "1.00" &then
run pi_messages (input "show", input 5009,
                 input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9",
                                   "ESFGL072","~~EMSFIN", "~~{~&emsfin_version}", "~~1.00")) /*msg_5009*/.
&else

/********************* Temporary Table Definition Begin *********************/

def TEMP-TABLE tt_param NO-UNDO
    FIELD c_emp_orig_ini AS CHARACTER FORMAT "x(03)"
    FIELD c_emp_orig_fim AS CHARACTER FORMAT "x(03)"
    FIELD c_emp_dest_ini AS CHARACTER FORMAT "x(03)"
    FIELD c_emp_dest_fim AS CHARACTER FORMAT "x(03)"
    FIELD c_estab_ini    AS CHARACTER FORMAT "x(03)"
    FIELD c_estab_fim    AS CHARACTER FORMAT "x(03)"
    FIELD i_lote_ini     AS INTEGER  FORMAT ">>>>>>>>9"
    FIELD i_lote_fim     AS INTEGER  FORMAT ">>>>>>>>9"
    FIELD dt_lote_ini    AS DATE FORMAT 99/99/9999
    FIELD dt_lote_fim    AS DATE FORMAT 99/99/9999
    FIELD dt_lanca_ini   AS DATE FORMAT 99/99/9999
    FIELD dt_lanca_fim   AS DATE FORMAT 99/99/9999
    FIELD c_cenario_ini  AS CHARACTER FORMAT "x(09)"
    FIELD c_cenario_fim  AS CHARACTER FORMAT "x(09)"
    FIELD c_modulo_ini   AS CHARACTER FORMAT "x(03)"
    FIELD c_modulo_fim   AS CHARACTER FORMAT "x(03)"
    FIELD class-1        AS CHARACTER FORMAT "x(15)"
    FIELD class-2        AS CHARACTER FORMAT "x(15)"
    FIELD class-3        AS CHARACTER FORMAT "x(15)"
    FIELD rs_tipo_log    AS CHARACTER
    FIELD rs_OUTPUT      AS CHARACTER
    FIELD c_arquivo      AS CHARACTER FORMAT "x(30)".

/********************** Temporary Table Definition End **********************/

/************************** Buffer Definition Begin *************************/

&if "{&emsbas_version}" >= "1.00" &then
def buffer b_ped_exec_style
    for ped_exec.
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

def new global shared var v_rec_estabelecimento
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_empresa
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_cod_cenario
    as char
    format "x(10)"
    no-undo.
def new global shared var v_cod_lote_contabil
    as int
    format ">>>>>>9":U
    no-undo.
def new global shared var v_cod_modulo
    as char
    format "x(03)"
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
def new global shared var v_cod_usuar_corren
    as character
    format "x(12)":U
    label "Usu†rio Corrente"
    column-label "Usu†rio Corrente"
    no-undo.
def new global shared var v_num_ped_exec_corren
    as integer
    format ">>>>9":U
    no-undo.
def new global shared var v_rec_empresa
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_lote_contabil
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_cenar_ctbl
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.
def new global shared var v_rec_modul_dtsul
    as recid
    format ">>>>>>9":U
    no-undo.
def new shared var v_cod_dwb_file
    as character
    format "x(40)":U
    label "Arquivo"
    column-label "Arquivo"
    no-undo.
def new shared var v_cod_dwb_program
    as character
    format "x(32)":U
    label "Programa"
    column-label "Programa"
    no-undo.
def new shared var v_cod_dwb_select
    as character
    format "x(32)":U
    no-undo.
def new shared var v_cod_release
    as character
    format "x(12)":U
    no-undo.
def new shared var v_dat_execution
    as date
    format "99/99/9999":U
    no-undo.
def new shared var v_hra_execution
    as character
    format "99:99":U
    no-undo.

def new shared var v_cod_emp_orig_ini
    as character
    format "x(3)":U
    label "Emp Origem"
    INITIAL ''
    no-undo.

def new shared var v_cod_emp_orig_fim
    as character
    format "x(3)":U
    INITIAL 'ZZZ'
    no-undo.

def new shared var v_cod_emp_dest_ini
    as character
    format "x(3)":U
    label "Emp Origem"
    INITIAL ''
    no-undo.

def new shared var v_cod_emp_dest_fim
    as character
    format "x(3)":U
    INITIAL 'ZZZ'
    no-undo.
/*
def new shared var v_cod_estab_ini
    as character
    format "x(3)":U
    label "Estabelecimento"
    no-undo.
def new shared var v_cod_estab_fim
    as character
    format "x(3)":U
    no-undo.
*/    

def new global shared var v_rec_lote_ctbl
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.

def new shared var v_lote_contabil_ini
    as integer
    format ">>>>>>>>9":U
    label "Lote Cont†bil"
    no-undo.
def new shared var v_lote_contabil_fim
    as integer
    format ">>>>>>>>9":U
    no-undo.
/*
def new shared var v_dat_lote_ini
    as date
    format "99/99/9999":U
    label "Data Lote"
    no-undo.
    */
/*
def new shared var v_dat_lote_fim
    as date
    format "99/99/9999":U
    no-undo.
    */
def new shared var v_dat_lancamento_ini
    as date
    format "99/99/9999":U
    label "Data Lanáamento"
    no-undo.
def new shared var v_dat_lancamento_fim
    as date
    format "99/99/9999":U
    no-undo.
def new shared var v_cenario_ini
    as char
    format "x(09)":U
    label "Cenario"
    no-undo.
def new shared var v_cenario_fim
    as char
    format "x(09)":U
    no-undo.
def new shared var v_modulo_ini
    as char
    format "x(03)":U
    label "M¢dulo"
    no-undo.
def new shared var v_modulo_fim
    as char
    format "x(03)":U
    no-undo.
def new shared var v_nom_enterprise
    as character
    format "x(40)":U
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
def new shared var v_num_entry
    as integer
    format ">>>>,>>9":U
    label "Ordem"
    column-label "Ordem"
    no-undo.
def var v_cod_dwb_field
    as character
    format "x(32)":U
    no-undo.
def var v_cod_dwb_file_old
    as character
    format "x(50)":U
    label "Arquivo Externo"
    column-label "Arquivo Externo"
    no-undo.
def var v_cod_dwb_file_temp
    as character
    format "x(12)":U
    no-undo.
def var v_cod_dwb_order
    as character
    format "x(32)":U
    label "Classificaáao"
    column-label "Classificador"
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
def var v_nom_title_aux
    as character
    format "x(60)":U
    no-undo.
def var v_num_count
    as integer
    format ">>>>,>>9":U
    no-undo.
def var v_num_ped_exec
    as integer
    format ">>>>9":U
    label "Pedido"
    column-label "Pedido"
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
def var v_rec_log
    as recid
    format ">>>>>>9":U
    no-undo.
def var v_wgh_focus
    as widget-handle
    format ">>>>>>9":U
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

/*def var v_log_print_par
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    no-undo.
*/
def var rs_ind_run_mode
    as character
    initial "On-Line"
    view-as radio-set Horizontal
    radio-buttons "On-Line", "On-Line","Batch", "Batch"
     /*l_online*/ /*l_online*/ /*l_batch*/ /*l_batch*/
    bgcolor 8 
    no-undo.

/************************** Variable Definition End *************************/

/*************************** Menu Definition Begin **************************/

def menu      m_help                menubar
    menu-item mi_conteudo           label "&Conte£do"
    menu-item mi_sobre              label "&Sobre".

/**************************** Menu Definition End ***************************/

/************************ Rectangle Definition Begin ************************/

def rectangle rt_cxcf
    size 1 by 1
    fgcolor 1 edge-pixels 2.
def rectangle rt_dimensions
    size 1 by 1
    edge-pixels 2.
def rectangle rt_order
    size 1 by 1
    edge-pixels 2.
def rectangle rt_parameters_label
    size 1 by 1
    edge-pixels 2.
def rectangle rt_select
    size 1 by 1
    edge-pixels 2.
def rectangle rt_target
    size 1 by 1
    edge-pixels 2.
def rectangle rt_run
    size 1 by 1
    edge-pixels 2.

/************************* Rectangle Definition End *************************/

/************************** Button Definition Begin *************************/

def button bt_close
    label "&Fecha"
    tooltip "Fecha"
    size 1 by 1
    auto-go.
def button bt_down
    label "V"
    tooltip "Desce"
    image-up file "image/im-dw"
    image-insensitive file "image/ii-dw"
    size 1 by 1.
def button bt_get_file
    label "Pesquisa Arquivo"
    tooltip "Pesquisa Arquivo"
    image-up file "image/im-sea1"
    image-insensitive file "image/ii-sea1"
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
    image-up file "image/im-setpr.bmp"
    image-insensitive file "image/ii-setpr"
    size 1 by 1.
def button bt_up
    label "AtÇ"
    tooltip "Sobe"
    image-up file "image/im-up"
    image-insensitive file "image/ii-up"
    size 1 by 1.
def button bt_zoom_cod_estab_ini
    label "Psq"
    tooltip "Pesquisa"
    image-up file "image/im-sea2"
    image-insensitive file "image/ii-sea2".
def button bt_zoom_cod_estab_fim
    label "Psq"
    tooltip "Pesquisa"
    image-up file "image/im-sea2"
    image-insensitive file "image/ii-sea2".
def button bt_zoom_cod_emp_orig_ini
    label "Psq"
    tooltip "Pesquisa"
    image-up file "image/im-sea2"
    image-insensitive file "image/ii-sea2".
def button bt_zoom_cod_emp_orig_fim
    label "Psq"
    tooltip "Pesquisa"
    image-up file "image/im-sea2"
    image-insensitive file "image/ii-sea2".

/*def button bt_zoom_cod_emp_dest_ini
    label "Psq"
    tooltip "Pesquisa"
    image-up file "image/im-sea2"
    image-insensitive file "image/ii-sea2".
def button bt_zoom_cod_emp_dest_fim
    label "Psq"
    tooltip "Pesquisa"
    image-up file "image/im-sea2"
    image-insensitive file "image/ii-sea2".*/
/*def button bt_zoom_lote_contabil_ini
    label "Psq"
    tooltip "Pesquisa"
    image-up file "image/im-sea2"
    image-insensitive file "image/ii-sea2".
def button bt_zoom_lote_contabil_fim
    label "Psq"
    tooltip "Pesquisa"
    image-up file "image/im-sea2"
    image-insensitive file "image/ii-sea2".*/
def button bt_zoom_modulo_ini
    label "Psq"
    tooltip "Pesquisa"
    image-up file "image/im-sea2"
    image-insensitive file "image/ii-sea2".
def button bt_zoom_modulo_fim
    label "Psq"
    tooltip "Pesquisa"
    image-up file "image/im-sea2"
    image-insensitive file "image/ii-sea2".
def button bt_zoom_cenario_ini
    label "Psq"
    tooltip "Pesquisa"
    image-up file "image/im-sea2"
    image-insensitive file "image/ii-sea2".
def button bt_zoom_cenario_fim
    label "Psq"
    tooltip "Pesquisa"
    image-up file "image/im-sea2"
    image-insensitive file "image/ii-sea2".

/****************************** Function Button *****************************/

/*************************** Button Definition End **************************/

/************************** Editor Definition Begin *************************/

def var ed_1x40
    as character
    view-as editor no-word-wrap
    size 30 by 1
    bgcolor 15 font 2
    no-undo.

/*def var ed_3x40
    as character
    view-as editor
    size 37 by 3
    bgcolor 15 font 2
    no-undo.*/

/*************************** Editor Definition End **************************/

/********************** Selection List Definition Begin *********************/

def var ls_order
    as character
    view-as selection-list single
    scrollbar-vertical
    list-items ""
    size 30 by 5
    bgcolor 15
    no-undo.

/*********************** Selection List Definition End **********************/

/************************ Radio-Set Definition Begin ************************/
/*
def var rs_tipo_log
    as character
    initial "Detalhado"
    view-as radio-set Horizontal
    radio-buttons "Detalhado", "Detalhado","Resumido", "Resumido"
    bgcolor 8
    no-undo.
*/    



def var rs_cod_dwb_output
    as character
    initial "Terminal"
    view-as radio-set Horizontal
    radio-buttons "Terminal", "Terminal","Arquivo", "Arquivo","Impressora", "Impressora"
    bgcolor 8
    no-undo.

/************************* Radio-Set Definition End *************************/

/************************** Report Definition Begin *************************/

def new shared var v_rpt_s_1_lines as integer initial 66.
def new shared var v_rpt_s_1_columns as integer initial 80.
def new shared var v_rpt_s_1_bottom as integer initial 65.
def new shared var v_rpt_s_1_page as integer.
def new shared var v_rpt_s_1_name as character initial "Relat¢rio Caract Espec Pat".

/*************************** Report Definition End **************************/

/************************** Frame Definition Begin **************************/

def frame f_tela
    " Classificaá∆o " view-as text
         at row 01.20 col 04.00 bgcolor 8
    rt_order
         at row 01.50 col 02.00
    ls_order
         at row 02.00 col 03.00
         help "" no-label
    bt_up
         at row 03.40 col 35.00 font ?
         help "Sobe"
    bt_down
         at row 04.60 col 35.00 font ?
         help "Desce"

    " Seleá∆o " view-as text
         at row 01.20 col 44.00
    rt_select
         at row 01.50 col 42.00

    v_cod_emp_orig_ini
        at row 02.70 col 52.00 colon-aligned label "Emp. Origem"
        view-as fill-in
        size-chars 05.00 by .88
        fgcolor ? bgcolor 15 font 2    
    bt_zoom_cod_emp_orig_ini
        at row 02.70 col 58.30 font ?
         help "Pesquisa"
    v_cod_emp_orig_fim
        at row 02.70 col 70.00 COLON-ALIGNED NO-LABEL
        view-as fill-in
        size-chars 05.00 by .88
        fgcolor ? bgcolor 15 font 2
    bt_zoom_cod_emp_orig_fim
        at row 02.70 col 76.30 font ?
         help "Pesquisa"
    v_cod_emp_dest_ini
        at row 03.70 col 52.00 colon-aligned label "Emp. Destino"
        view-as fill-in
        size-chars 05.00 by .88
        fgcolor ? bgcolor 15 font 2  
    /*
    bt_zoom_cod_emp_dest_ini
        at row 03.70 col 58.30 font ?
         help "Pesquisa"*/

    /*v_cod_emp_dest_fim
        at row 03.70 col 70.00 COLON-ALIGNED NO-LABEL
        view-as fill-in
        size-chars 05.00 by .88
        fgcolor ? bgcolor 15 font 2
    bt_zoom_cod_emp_dest_fim
        at row 03.70 col 76.30 font ?
         help "Pesquisa"*/
    
    /*
    v_cod_estab_ini
        at row 04.70 col 52.00 colon-aligned label "Estab.Destino"
        view-as fill-in
        size-chars 05.00 by .88
        fgcolor ? bgcolor 15 font 2
    bt_zoom_cod_estab_ini
         at row 04.70 col 58.30 font ?
         help "Pesquisa"
    v_cod_estab_fim
        at row 04.70 col 70.00 colon-aligned no-label
        view-as fill-in
        size-chars 05.00 by .88
        fgcolor ? bgcolor 15 font 2
    bt_zoom_cod_estab_fim
         at row 04.70 col 76.30 font ?
         help "Pesquisa"
*/         
    v_lote_contabil_ini
        at row 04.70 col 52.00 colon-aligned label "Lote Cont†bil"
        view-as fill-in
        size-chars 11.14 by .88
        fgcolor ? bgcolor 15 font 2
    /*bt_zoom_lote_contabil_ini
         at row 04.70 col 65.30 font ?
         help "Pesquisa"*/
    v_lote_contabil_fim
        at row 04.70 col 70.00 colon-aligned no-label
        view-as fill-in
        size-chars 11.14 by .88
        fgcolor ? bgcolor 15 font 2
    /*bt_zoom_lote_contabil_fim
         at row 04.70 col 83.30 font ?
         help "Pesquisa"*/
    /*
    v_dat_lote_ini
        at row 05.60 col 52.00 colon-aligned label "Data Lote"
        view-as fill-in
        size-chars 11.14 by .88
        fgcolor ? bgcolor 15 font 2
    v_dat_lote_fim
        at row 05.60 col 70.00 colon-aligned no-label
        view-as fill-in
        size-chars 11.14 by .88
        fgcolor ? bgcolor 15 font 2
        */
    v_dat_lancamento_ini
        at row 06.50 col 52.00 colon-aligned label "Dt Lanáamento"
        view-as fill-in
        size-chars 11.14 by .88
        fgcolor ? bgcolor 15 font 2
    v_dat_lancamento_fim
        at row 06.50 col 70.00 colon-aligned no-label
        view-as fill-in
        size-chars 11.14 by .88
        fgcolor ? bgcolor 15 font 2
    v_cenario_ini
        at row 07.40 col 52.00 colon-aligned label "Cen†rio"
        view-as fill-in
        size-chars 9.14 by .88
        fgcolor ? bgcolor 15 font 2
    bt_zoom_cenario_ini
         at row 07.35 col 63.30 font ?
         help "Pesquisa"
    v_cenario_fim
        at row 07.40 col 70.00 colon-aligned no-label
        view-as fill-in
        size-chars 9.14 by .88
        fgcolor ? bgcolor 15 font 2
    bt_zoom_cenario_fim
         at row 07.35 col 81.30 font ?
         help "Pesquisa"
    v_modulo_ini
        at row 08.30 col 52.00 colon-aligned label "M¢dulo"
        view-as fill-in
        size-chars 5.14 by .88
        fgcolor ? bgcolor 15 font 2
    bt_zoom_modulo_ini
         at row 08.35 col 59.30 font ?
         help "Pesquisa"
    v_modulo_fim
        at row 08.30 col 70.00 colon-aligned no-label
        view-as fill-in
        size-chars 5.14 by .88
        fgcolor ? bgcolor 15 font 2
    bt_zoom_modulo_fim
         at row 08.35 col 77.30 font ?
         help "Pesquisa"
    /*rt_parameters_label
         at row 8.00 col 02.00*/
    /*rs_tipo_log
         at row 9.70 col 3.00
         help "" no-label  */
    /*ed_3x40
         at row 10.70 col 3.00
         help "" NO-LABEL*/
   
    rt_target                 
         at row 11.5 col 2.00 
    " Destino " view-as text
         at row 11.2 col 4.00 bgcolor 8 
    rt_run
         at row 11.50 col 42.00
    " Execuá∆o " view-as text
         at row 11.20 col 44.00
    rt_dimensions
         at row 11.50 col 68.00
    " Dimens‰es " view-as text
         at row 11.20 col 70.00
    rs_cod_dwb_output
         at row 12.21 col 4.00
         help "" no-label
    ed_1x40
         at row 13.0 col 4.00
         help "" no-label
    bt_set_printer
         at row 13.00 col 36.15 font ?
         help "Define Impressora e Layout de Impress∆o"
    bt_get_file
         at row 13.0 col 36.15 font ?
         help "Pesquisa Arquivo"
    rs_ind_run_mode
         at row 12.21 col 44.00
         help "" no-label
    /* v_log_print_par
         at row 13.21 col 44.00 label "Imprime ParÉmetros" 
         view-as toggle-box                                */
    v_qtd_line
         at row 12.21 col 79.00 colon-aligned
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_qtd_column
         at row 13.21 col 79.00 colon-aligned
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    rt_cxcf
         at row 15.00 col 02.00 bgcolor 7
    bt_close
         at row 15.25 col 03.00 font ?
         help "Fecha"
    bt_print
         at row 15.25 col 14.00 font ?
         help "Imprime"
    bt_hel2
         at row 15.25 col 77.56 font ?
         help "Ajuda"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 90.00 by 17.00
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Relat¢rio Envio de Lotes Contabeis Unificados".
    /* adjust size of objects in this frame */
    assign bt_close:width-chars             in frame f_tela = 10.00
           bt_close:height-chars            in frame f_tela = 01.00
           bt_down:width-chars              in frame f_tela = 04.00
           bt_down:height-chars             in frame f_tela = 01.12
           bt_get_file:width-chars          in frame f_tela = 04.00
           bt_get_file:height-chars         in frame f_tela = 01.10
           bt_hel2:width-chars              in frame f_tela = 10.00
           bt_hel2:height-chars             in frame f_tela = 01.00
           bt_print:width-chars             in frame f_tela = 10.00
           bt_print:height-chars            in frame f_tela = 01.00
           bt_set_printer:width-chars       in frame f_tela = 04.00
           bt_set_printer:height-chars      in frame f_tela = 01.10
           bt_up:width-chars                in frame f_tela = 04.00
           bt_up:height-chars               in frame f_tela = 01.12
           ed_1x40:width-chars              in frame f_tela = 32.00
           ed_1x40:height-chars             in frame f_tela = 01.00
           /*ed_3x40:width-chars              in frame f_tela = 37.00*/
           /*ed_3x40:height-chars             in frame f_tela = 03.00*/
           ls_order:width-chars             in frame f_tela = 30.00
           ls_order:height-chars            in frame f_tela = 05.00
           rt_cxcf:width-chars              in frame f_tela = 86.50
           rt_cxcf:height-chars             in frame f_tela = 01.40
           rt_dimensions:width-chars        in frame f_tela = 20.57
           rt_dimensions:height-chars       in frame f_tela = 03.00
           rt_order:width-chars             in frame f_tela = 39.00
           rt_order:height-chars            in frame f_tela = 9.50
           /*rt_parameters_label:width-chars  in frame f_tela = 39.00
           rt_parameters_label:height-chars in frame f_tela = 06.50*/
           rt_run:width-chars               in frame f_tela = 25.00
           rt_run:height-chars              in frame f_tela = 03.00
           rt_select:width-chars            in frame f_tela = 46.50
           rt_select:height-chars           in frame f_tela = 9.50
           rt_target:width-chars            in frame f_tela = 39
           rt_target:height-chars           in frame f_tela = 03.00.

    assign ed_1x40:return-inserted in frame f_tela = yes
           /*ed_3x40:return-inserted in frame f_tela = yes */.

    /* set private-data for the help system */
    assign ls_order:private-data          in frame f_tela = "HLP=000006914":U
           bt_up:private-data             in frame f_tela = "HLP=000009438":U
           bt_down:private-data           in frame f_tela = "HLP=000009436":U
           rs_cod_dwb_output:private-data in frame f_tela = "HLP=000006914":U
           ed_1x40:private-data           in frame f_tela = "HLP=000006914":U
           /*ed_3x40:private-data           in frame f_tela = "HLP=000006914":U*/
           bt_get_file:private-data       in frame f_tela = "HLP=000008782":U
           bt_set_printer:private-data    in frame f_tela = "HLP=000008785":U
           v_qtd_line:private-data        in frame f_tela = "HLP=000024737":U
           v_qtd_column:private-data      in frame f_tela = "HLP=000024669":U
           bt_close:private-data          in frame f_tela = "HLP=000009420":U
           bt_print:private-data          in frame f_tela = "HLP=000010815":U
           bt_hel2:private-data           in frame f_tela = "HLP=000011326":U
           frame f_tela:private-data                      = "HLP=000006914":U.

{include/i_fclfrm.i f_tela }
/*************************** Frame Definition End ***************************/

/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
def var v_prog_filtro_pdf as handle no-undo.

function getCodTipoRelat returns character in v_prog_filtro_pdf.

run prgtec/btb/btb920aa.py persistent set v_prog_filtro_pdf.

run pi_define_objetos in v_prog_filtro_pdf (frame f_tela:handle,
                       rs_cod_dwb_output:handle in frame f_tela,
                       bt_get_file:row in frame f_tela,
                       bt_get_file:col in frame f_tela).

&endif
/* tech38629 - Fim da alteraá∆o */

/*********************** User Interface Trigger Begin ***********************/

ON CHOOSE OF bt_down IN FRAME f_tela
DO:
    /************************* Variable Definition Begin ************************/

    def var v_cod_dwb_field
        as character
        format "x(32)":U
        no-undo.
    def var v_cod_dwb_order
        as character
        format "x(32)":U
        label "Classificaá∆o"
        column-label "Classificador"
        no-undo.
    def var v_num_entry
        as integer
        format ">>>>,>>9":U
        label "Ordem"
        column-label "Ordem"
        no-undo.

    /************************** Variable Definition End *************************/

    assign v_cod_dwb_field = ls_order:screen-value in frame f_tela
           v_cod_dwb_order = ls_order:list-items in frame f_tela
           v_num_entry     = lookup(v_cod_dwb_field, v_cod_dwb_order).

    if  v_num_entry > 0 and v_num_entry < num-entries (v_cod_dwb_order) then do:
        assign entry(v_num_entry, v_cod_dwb_order)     = entry(v_num_entry + 1, v_cod_dwb_order)
               entry(v_num_entry + 1, v_cod_dwb_order) = v_cod_dwb_field
               ls_order:list-items in frame f_tela     = v_cod_dwb_order
               ls_order:screen-value in frame f_tela   = v_cod_dwb_field.
    end.
END. /* ON CHOOSE OF bt_down IN FRAME f_tela */

ON CHOOSE OF bt_get_file IN FRAME f_tela
DO:
    system-dialog get-file v_cod_dwb_file
        title "Imprimir"
        filters '*.rpt' '*.rpt',
                "*.*"  "*.*"
        save-as
        create-test-file
        ask-overwrite.
        assign dwb_rpt_param.cod_dwb_file           = v_cod_dwb_file
               ed_1x40:screen-value in frame f_tela = v_cod_dwb_file.

END. /* ON CHOOSE OF bt_get_file IN FRAME f_tela */

ON CHOOSE OF bt_hel2 IN FRAME f_tela
DO:
    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (INPUT SELF:FRAME,
                                INPUT THIS-PROCEDURE:HANDLE) /*prg_fnc_chamar_help_context*/.

    /* End_Include: i_context_help_frame */
END. /* ON CHOOSE OF bt_hel2 IN FRAME f_tela */

ON CHOOSE OF bt_print IN FRAME f_tela
DO:
    empty temp-table tt_param.

    IF NOT CAN-find(FIRST es_cons_estab_emp WHERE es_cons_estab_emp.cod_empresa = INPUT FRAME f_tela v_cod_emp_dest_ini) THEN DO:
        MESSAGE 'ParÉmetro de De-Para n∆o cadastrado para a empresa destino informada.' VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'ENTRY' TO v_cod_emp_dest_ini.
        RETURN NO-APPLY.
    END. 

    /*
    create tt_param.
    assign /*tt_param.c_estab_ini   = input frame f_tela v_cod_estab_ini
           tt_param.c_estab_fim   = input frame f_tela v_cod_estab_fim*/
           tt_param.c_emp_orig_ini  = input frame f_tela v_cod_emp_orig_ini
           tt_param.c_emp_orig_fim  = input frame f_tela v_cod_emp_orig_fim
           tt_param.c_emp_dest_ini  = input frame f_tela v_cod_emp_dest_ini
           tt_param.c_emp_dest_fim  = input frame f_tela v_cod_emp_dest_fim
           tt_param.i_lote_ini      = input frame f_tela v_lote_contabil_ini
           tt_param.i_lote_fim      = input frame f_tela v_lote_contabil_fim
           tt_param.dt_lote_ini     = input frame f_tela v_dat_lote_ini
           tt_param.dt_lote_fim     = input frame f_tela v_dat_lote_fim
           tt_param.dt_lanca_ini    = input frame f_tela v_dat_lancamento_ini
           tt_param.dt_lanca_fim    = input frame f_tela v_dat_lancamento_fim
           tt_param.c_cenario_ini   = input frame f_tela v_cenario_ini
           tt_param.c_cenario_fim   = input frame f_tela v_cenario_fim
           tt_param.c_modulo_ini    = input frame f_tela v_modulo_ini
           tt_param.c_modulo_fim    = input frame f_tela v_modulo_fim
           tt_param.class-1         = entry(1,ls_order:list-items,",")
           tt_param.class-2         = entry(2,ls_order:list-items,",")
           tt_param.class-3         = entry(3,ls_order:list-items,",")
           tt_param.rs_tipo_log     = ''
           tt_param.rs_output       = input frame f_tela rs_cod_dwb_output
           tt_param.c_arquivo       = input frame f_tela ed_1x40.
           */

    ASSIGN dwb_rpt_param.cod_dwb_parameters = input frame f_tela v_cod_emp_orig_ini             + '|' +
                                              input frame f_tela v_cod_emp_orig_fim             + '|' +
                                              input frame f_tela v_cod_emp_dest_ini             +  '|' +
                                             /* input frame f_tela v_cod_emp_dest_fim             +*/  '|' +
                                              input frame f_tela v_lote_contabil_ini            + '|' +
                                              input frame f_tela v_lote_contabil_fim            + '|' +
                                              /*string(input frame f_tela v_dat_lote_ini) */      '|' +
                                              /*string(input frame f_tela v_dat_lote_fim) */      '|' +
                                              string(input frame f_tela v_dat_lancamento_ini)   + '|' +
                                              string(input frame f_tela v_dat_lancamento_fim )  + '|' +
                                              input frame f_tela v_cenario_ini                  + '|' +
                                              input frame f_tela v_cenario_fim                  + '|' +
                                              input frame f_tela v_modulo_ini                   + '|' +
                                              input frame f_tela v_modulo_fim                   + '|' +
                                              entry(1,ls_order:list-items,",")                  + '|' +              
                                              entry(2,ls_order:list-items,",")                  + '|' +
                                              entry(3,ls_order:list-items,",")                  + '|' +
                                              input frame f_tela rs_cod_dwb_output              + '|' +
                                              input frame f_tela ed_1x40 .
    
    if INPUT FRAME f_tela rs_ind_run_mode = "Batch" then do:
        RUN prgtec/btb/btb911za.p (Input v_cod_dwb_program,
                                   Input v_cod_release,
                                   Input 42,
                                   Input recid(dwb_rpt_param),
                                   output v_num_ped_exec).        
        run pi_messages (input "show",
                         input 3556,
                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                           v_num_ped_exec)).
            assign v_num_ped_exec = 0.
    END.
    ELSE DO:
        RUN esp/esfgl072rp.p.
    END.

    return no-apply.
END. /* ON CHOOSE OF bt_print IN FRAME f_tela */

ON CHOOSE OF bt_close IN FRAME f_tela
DO:

END. /* ON CHOOSE OF bt_close IN FRAME f_tela */

ON CHOOSE OF bt_zoom_cod_emp_orig_ini IN FRAME f_tela
DO:
    RUN prgint/utb/utb069ka.p.

    find first empresa no-lock
         where recid(empresa) = v_rec_empresa no-error.
    if  avail empresa then
        assign v_cod_emp_orig_ini:screen-value in frame f_tela = empresa.cod_empresa.
END. 

ON CHOOSE OF bt_zoom_cod_emp_orig_fim IN FRAME f_tela
DO:
    RUN prgint/utb/utb069ka.p.

    find first empresa no-lock
         where recid(empresa) = v_rec_empresa no-error.
    if  avail empresa then
        assign v_cod_emp_orig_fim:screen-value in frame f_tela = empresa.cod_empresa.
END. 


ON LEAVE OF v_cod_emp_dest_ini IN FRAME f_tela
DO:
    IF NOT CAN-find(FIRST es_cons_estab_emp WHERE es_cons_estab_emp.cod_empresa = INPUT FRAME f_tela v_cod_emp_dest_ini) THEN DO:
        MESSAGE 'ParÉmetro de De-Para n∆o cadastrado para a empresa destino informada.' VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.        

END. 

/*
ON CHOOSE OF bt_zoom_cod_emp_dest_fim IN FRAME f_tela
DO:
    RUN prgint/utb/utb069ka.p.

    find first empresa no-lock
         where recid(empresa) = v_rec_empresa no-error.
    if  avail empresa then
        assign v_cod_emp_dest_fim:screen-value in frame f_tela = empresa.cod_empresa.
END. 
*/
/*
ON CHOOSE OF bt_zoom_cod_estab_ini IN FRAME f_tela
DO:
    run prgint/utb/utb071ka.p /*prg_sea_estabelecimento*/.

    find first estabelecimento no-lock
         where recid(estabelecimento) = v_rec_estabelecimento no-error.
    if  avail estabelecimento then
        assign v_cod_estab_ini:screen-value in frame f_tela = estabelecimento.cod_estab.
END. /* ON CHOOSE OF bt_zoom_cod_estab_ini IN FRAME f_tela */

ON CHOOSE OF bt_zoom_cod_estab_fim IN FRAME f_tela
DO:
    run prgint/utb/utb071ka.p /*prg_sea_estabelecimento*/.

    find first estabelecimento no-lock
         where recid(estabelecimento) = v_rec_estabelecimento no-error.
    if  avail estabelecimento then
        assign v_cod_estab_fim:screen-value in frame f_tela = estabelecimento.cod_estab.
END. /* ON CHOOSE OF bt_zoom_cod_estab_fim IN FRAME f_tela */
*/
ON CHOOSE OF bt_zoom_cenario_ini IN FRAME f_tela
DO:
    run prgint/utb/utb076ka.p.

    FIND FIRST cenar_ctbl WHERE recid(cenar_ctbl) = v_rec_cenar_ctbl NO-LOCK NO-ERROR.
    IF AVAIL cenar_ctbl THEN
        assign v_cenario_ini:screen-value in frame f_tela = cenar_ctbl.cod_cenar_Ctbl.
END. /* ON CHOOSE OF bt_zoom_cenario_ini IN FRAME f_tela */

ON CHOOSE OF bt_zoom_cenario_fim IN FRAME f_tela
DO:
    run prgint/utb/utb076ka.p.

    FIND FIRST cenar_ctbl WHERE recid(cenar_ctbl) = v_rec_cenar_ctbl NO-LOCK NO-ERROR.
    IF AVAIL cenar_ctbl THEN
        assign v_cenario_fim:screen-value in frame f_tela = cenar_ctbl.cod_cenar_ctbl.
END. /* ON CHOOSE OF bt_zoom_cenario_fim IN FRAME f_tela */

ON CHOOSE OF bt_zoom_modulo_ini IN FRAME f_tela
DO:
    run prgtec/men/men004ka.p.

    FIND FIRST modul_dtsul WHERE RECID(modul_dtsul) = v_rec_modul_dtsul NO-LOCK NO-ERROR.
    IF AVAIL MODul_dtsul THEN
        assign v_modulo_ini:screen-value in frame f_tela = modul_dtsul.cod_modul_dts.
END. /* ON CHOOSE OF bt_zoom_modulo_ini IN FRAME f_tela */

ON CHOOSE OF bt_zoom_modulo_fim IN FRAME f_tela
DO:
    run prgtec/men/men004ka.p.
    FIND FIRST modul_dtsul WHERE RECID(modul_dtsul) = v_rec_modul_dtsul NO-LOCK NO-ERROR.
    IF AVAIL MODul_dtsul THEN
        assign v_modulo_fim:screen-value in frame f_tela = modul_dtsul.cod_modul_dts.
END. /* ON CHOOSE OF bt_zoom_modulo_fim IN FRAME f_tela */

/*
ON CHOOSE OF bt_zoom_lote_contabil_ini IN FRAME f_tela
DO:
    run prgfin/fgl/fgl702za.p.

    FIND FIRST lote_ctbl WHERE recid(lote_ctbl) = v_rec_lote_ctbl NO-LOCK NO-ERROR.
    IF AVAIL lote_ctbl THEN
        assign v_lote_contabil_ini:screen-value in frame f_tela = string(lote_ctbl.num_lote_ctbl).
END. /* ON CHOOSE OF bt_zoom_lote_contabil_ini IN FRAME f_tela */

ON CHOOSE OF bt_zoom_lote_contabil_fim IN FRAME f_tela
DO:
    run prgfin/fgl/fgl702za.p.

    FIND FIRST lote_ctbl WHERE recid(lote_ctbl) = v_rec_lote_ctbl NO-LOCK NO-ERROR.
    IF AVAIL lote_ctbl THEN
        assign v_lote_contabil_fim:screen-value in frame f_tela = string(lote_ctbl.num_lote_ctbl).

END. /* ON CHOOSE OF bt_zoom_lote_contabil_fim IN FRAME f_tela */
  */
ON CHOOSE OF bt_set_printer IN FRAME f_tela
DO:
    assign v_nom_dwb_printer      = ""
           v_cod_dwb_print_layout = "".

    &if '{&emsbas_version}' <= '1.00' &then
    if  search("prgtec/btb/btb036nb.r") = ? and search("prgtec/btb/btb036nb.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa executavel n∆o foi encontrado: prgtec/btb/btb036nb.p".
        else do:
            message "Programa executavel n∆o foi encontrado: prgtec/btb/btb036nb.p"
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
            return "Programa executˇvel n∆o foi encontrado: prgtec/btb/btb036zb.p".
        else do:
            message "Programa executˇvel n∆o foi encontrado: prgtec/btb/btb036zb.p"
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
    and  v_cod_dwb_print_layout <> "" then do:
        assign dwb_rpt_param.nom_dwb_printer        = v_nom_dwb_printer
               dwb_rpt_param.cod_dwb_print_layout   = v_cod_dwb_print_layout
    &if '{&emsbas_version}' > '1.00' &then
    &if '{&emsbas_version}' >= '5.03' &then
               dwb_rpt_param.nom_dwb_print_file     = v_nom_dwb_print_file
    &else
               dwb_rpt_param.cod_livre_1            = v_nom_dwb_print_file
    &endif
    &endif
               ed_1x40:screen-value in frame f_tela = v_nom_dwb_printer + ":" +
                                                      v_cod_dwb_print_layout
    &if '{&emsbas_version}' > '1.00' &then
                                                      + (if  v_nom_dwb_print_file <> "" then ":" + v_nom_dwb_print_file
                                                         else "")
    &endif
    .
        find layout_impres no-lock
             where layout_impres.nom_impressora    = dwb_rpt_param.nom_dwb_printer
               and layout_impres.cod_layout_impres = dwb_rpt_param.cod_dwb_print_layout no-error.
        assign v_qtd_line = layout_impres.num_lin_pag.
        display v_qtd_line
                with frame f_tela.
    end.
END. /* ON CHOOSE OF bt_set_printer IN FRAME f_tela */

ON CHOOSE OF bt_up IN FRAME f_tela
DO:
    /************************* Variable Definition Begin ************************/

    def var v_cod_dwb_field
        as character
        format "x(32)":U
        no-undo.
    def var v_cod_dwb_order
        as character
        format "x(32)":U
        label "Classificaá∆o"
        column-label "Classificador"
        no-undo.
    def var v_num_entry
        as integer
        format ">>>>,>>9":U
        label "Ordem"
        column-label "Ordem"
        no-undo.

    /************************** Variable Definition End *************************/

    assign v_cod_dwb_field = ls_order:screen-value in frame f_tela
           v_cod_dwb_order = ls_order:list-items   in frame f_tela
           v_num_entry     = lookup(v_cod_dwb_field, v_cod_dwb_order).

    if  v_num_entry > (1 + 0) then do:
        assign entry(v_num_entry, v_cod_dwb_order) = entry(v_num_entry - 1, v_cod_dwb_order)
               entry(v_num_entry - 1, v_cod_dwb_order) = v_cod_dwb_field
               ls_order:list-items in frame f_tela = v_cod_dwb_order
               ls_order:screen-value in frame f_tela = v_cod_dwb_field.
    end.
END. /* ON CHOOSE OF bt_up IN FRAME f_tela */

ON LEAVE OF ed_1x40 IN FRAME f_tela
DO:
    /************************* Variable Definition Begin ************************/

    def var v_cod_filename_final   as char no-undo.
    def var v_cod_filename_initial as char no-undo.

    /************************** Variable Definition End *************************/
END. /* ON LEAVE OF ed_1x40 IN FRAME f_tela */

ON VALUE-CHANGED OF rs_ind_run_mode IN FRAME f_tela
DO:
    assign dwb_rpt_param.ind_dwb_run_mode = input frame f_tela rs_ind_run_mode.
    if  dwb_rpt_param.ind_dwb_run_mode = "Batch" /*l_batch*/ 
    then do:
        if  rs_cod_dwb_output:disable("Terminal" /*l_terminal*/ ) in frame f_tela
        then do:
        end /* if */.
    end /* if */.
    else do:
        if  rs_cod_dwb_output:enable("Terminal" /*l_terminal*/ ) in frame f_tela
        then do:
        end /* if */.
    end /* else */.
    if  rs_ind_run_mode = "Batch" /*l_batch*/ 
    then do:
        assign v_qtd_line = v_qtd_line_ant.
        display v_qtd_line
                with frame f_tela.
    end /* if */.
    assign rs_ind_run_mode.
    apply "value-changed" to rs_cod_dwb_output in frame f_tela.

END.

ON VALUE-CHANGED OF rs_cod_dwb_output IN FRAME f_tela
DO:
    initout:
    do with frame f_tela:
        /* block: */
        case self:screen-value:
            when "Terminal" then ter:
             do:
                if  rs_cod_dwb_output <> "Impressora" then
                    assign v_qtd_line_ant = input frame f_tela v_qtd_line.

                if  v_qtd_line_ant > 0 then
                    assign v_qtd_line = v_qtd_line_ant.
                else
                    assign v_qtd_line = (if  dwb_rpt_param.qtd_dwb_line > 0 then dwb_rpt_param.qtd_dwb_line
                                        else v_rpt_s_1_lines).

                display v_qtd_line
                        with frame f_tela.

                assign ed_1x40:screen-value   = ""
                       ed_1x40:sensitive      = no
                       bt_get_file:visible    = no
                       bt_set_printer:visible = no.
            end /* do ter */.
            when "Arquivo" then fil:
             do:
                if  rs_cod_dwb_output <> "Impressora" then
                    assign v_qtd_line_ant = input frame f_tela v_qtd_line.

                if  v_qtd_line_ant > 0 then
                    assign v_qtd_line = v_qtd_line_ant.
                else
                    assign v_qtd_line = (if  dwb_rpt_param.qtd_dwb_line > 0 then dwb_rpt_param.qtd_dwb_line
                                        else v_rpt_s_1_lines).

                display v_qtd_line
                        with frame f_tela.

                assign ed_1x40:screen-value       = ""
                       ed_1x40:sensitive          = yes
                       bt_set_printer:visible     = no
                       bt_get_file:visible        = yes.

                if  dwb_rpt_param.cod_dwb_print_layout <> "" then
                    assign v_cod_dwb_file_old = dwb_rpt_param.cod_dwb_print_layout.

                /* define arquivo default */
                find usuar_mestre no-lock
                     where usuar_mestre.cod_usuario = v_cod_dwb_user
    &if "{&emsbas_version}" >= "5.01" &then
                     use-index srmstr_id
    &endif
                      no-error.
                    assign dwb_rpt_param.cod_dwb_file = "".

                assign dwb_rpt_param.cod_dwb_file = dwb_rpt_param.cod_dwb_file + if  v_cod_dwb_file_temp = "" then
                                                                                     caps("esfgl072":U) + '.rpt'
                                                                                 else v_cod_dwb_file_temp.

                assign ed_1x40:screen-value                = dwb_rpt_param.cod_dwb_file
                       dwb_rpt_param.cod_dwb_print_layout  = ""
                       v_qtd_line                          = (if  v_qtd_line_ant > 0 then v_qtd_line_ant
                                                              else v_rpt_s_1_lines)
    &if '{&emsbas_version}' > '1.00' &then
                       v_nom_dwb_print_file                = ""
    &endif
    .
            end.
            when "Impressora" then prn:
             do:
                assign ed_1x40:sensitive        = no
                       bt_get_file:visible      = no
                       bt_set_printer:visible   = yes
                       bt_set_printer:sensitive = yes.

                /* define layout default */
                if   v_cod_dwb_file_old <> "" then
                     assign dwb_rpt_param.cod_dwb_print_layout = v_cod_dwb_file_old.

                if  dwb_rpt_param.nom_dwb_printer = ""
                or  dwb_rpt_param.cod_dwb_print_layout = "" then
                    run pi_set_print_layout_default /*pi_set_print_layout_default*/.
                else
                    assign ed_1x40:screen-value = dwb_rpt_param.nom_dwb_printer + ":" +
                                                  dwb_rpt_param.cod_dwb_print_layout.

                if  dwb_rpt_param.cod_dwb_print_layout <> "" then
                    assign v_cod_dwb_file_old = dwb_rpt_param.cod_dwb_print_layout.

                find layout_impres no-lock
                     where layout_impres.nom_impressora    = dwb_rpt_param.nom_dwb_printer
                       and layout_impres.cod_layout_impres = dwb_rpt_param.cod_dwb_print_layout no-error.
                if  avail layout_impres then
                    assign v_qtd_line = layout_impres.num_lin_pag.

                display v_qtd_line
                        with frame f_tela.
            end /* do prn */.
        end /* case block */.

        assign v_cod_dwb_file_temp = replace(dwb_rpt_param.cod_dwb_file, "~\", "~/").
        if  index(v_cod_dwb_file_temp, "~/") <> 0 then
            assign v_cod_dwb_file_temp = substr(v_cod_dwb_file_temp, r-index(v_cod_dwb_file_temp, "~/") + 1).
        else
            assign v_cod_dwb_file_temp = dwb_rpt_param.cod_dwb_file.
    end /* do initout */.

    if  self:screen-value = "Impressora" then
        disable v_qtd_line
                with frame f_tela.
    else
        enable v_qtd_line
               with frame f_tela.

    assign rs_cod_dwb_output.
END. /* ON VALUE-CHANGED OF rs_cod_dwb_output IN FRAME f_tela */

/************************ User Interface Trigger End ************************/

/**************************** Frame Trigger Begin ***************************/

ON ENDKEY OF FRAME f_tela
DO:

END. /* ON ENDKEY OF FRAME f_tela */

ON GO OF FRAME f_tela
DO:

END. /* ON GO OF FRAME f_tela */

ON HELP OF FRAME f_tela ANYWHERE
DO:
    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */
END. /* ON HELP OF FRAME f_tela */

ON RIGHT-MOUSE-DOWN OF FRAME f_tela ANYWHERE
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
END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_tela */

ON RIGHT-MOUSE-UP OF FRAME f_tela ANYWHERE
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
END. /* ON RIGHT-MOUSE-UP OF FRAME f_tela */

ON WINDOW-CLOSE OF FRAME f_tela
DO:
    apply "end-error" to self.
END. /* ON WINDOW-CLOSE OF FRAME f_tela */

/***************************** Frame Trigger End ****************************/

/**************************** Menu Trigger Begin ****************************/

ON CHOOSE OF MENU-ITEM mi_conteudo IN MENU m_help
DO:
    apply "choose" to bt_hel2 in frame f_tela.
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

    assign v_nom_prog = substr(frame f_tela:title, 1, max(1, length(frame f_tela:title) - 10)).
    if  v_nom_prog = ? then
        assign v_nom_prog = "".

    assign v_nom_prog = v_nom_prog + chr(10) + "esfgl072":U.

    assign v_nom_prog_ext = "esp/esfgl072.p":U
           v_cod_release  = trim(" 1.00.00.000":U).

    {include/sobre5.i}
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

if  v_cod_arq <> '' and v_cod_arq <> ? then
    run pi_version_extract ('esfgl072':U, 'esp/esfgl072.p':U, '1.00.00.000':U, 'pro':U).

/* End_Include: i_version_extract */

run pi_return_user (output v_cod_dwb_user) /*pi_return_user*/.

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
if  v_cod_dwb_user = "" then
    assign v_cod_dwb_user = v_cod_usuar_corren.

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
    run prgtec/men/men901za.py (Input 'esfgl072') /*prg_fnc_verify_security*/.
if  return-value = "2014"
then do:
    /* Programa a ser executado n∆o Ç um programa v†lido Datasul ! */
    run pi_messages (input "show", input 2014,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'esfgl072')) /*msg_2014*/.
    return.
end.
if  return-value = "2012"
then do:
    /* Usuˇrio sem permiss∆o para acessar o programa. */
    run pi_messages (input "show", input 2012,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'esfgl072')) /*msg_2012*/.
    return.
end.
/* End_Include: i_verify_security */

/* Begin_Include: i_log_exec_prog_dtsul_ini */
assign v_rec_log = ?.

if can-find(prog_dtsul
       where prog_dtsul.cod_prog_dtsul = 'esfgl072'
         and prog_dtsul.log_gera_log_exec = yes) then do trans:
    create log_exec_prog_dtsul.
    assign log_exec_prog_dtsul.cod_prog_dtsul           = 'esfgl072'
           log_exec_prog_dtsul.cod_usuario              = v_cod_usuar_corren
           log_exec_prog_dtsul.dat_inic_exec_prog_dtsul = today
           log_exec_prog_dtsul.hra_inic_exec_prog_dtsul = replace(string(time,"hh:mm:ss"),":":U,"":U).
    assign v_rec_log = recid(log_exec_prog_dtsul).
    release log_exec_prog_dtsul no-error.
end.
/* End_Include: i_log_exec_prog_dtsul_ini */

/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
run pi_permissoes in v_prog_filtro_pdf (input 'esfgl072':U).
&endif
/* tech38629 - Fim da alteraá∆o */

/* redefiniá‰es do frame */

/* Begin_Include: i_std_dialog_box */
/* tratamento do titulo e vers∆o */
assign frame f_tela:title = frame f_tela:title
                            + chr(32)
                            + chr(40)
                            + trim(" 1.00.00.000":U)
                            + chr(41).
/* menu pop-up de ajuda e sobre */
assign menu m_help:popup-only = yes
       bt_hel2:popup-menu in frame f_tela = menu m_help:handle.

/* End_Include: i_std_dialog_box */

{include/title5.i f_tela FRAME}

/* inicializa variˇveis */
run pi_initialize_reports /*pi_initialize_reports*/.

pause 0 before-hide.
view frame f_tela.

/*assign ed_3x40:screen-value in frame f_tela = 'Na opá∆o "Resumido", a classificaá∆o ser† restrita apenas por Lote Cont†bil (origem).'
       ed_3x40:sensitive    in frame f_tela = no.*/

super_block:
repeat:
    run pi_configure_dwb_param /*pi_configure_dwb_param*/.
    assign v_qtd_line = dwb_rpt_param.qtd_dwb_line.

    init:
    do with frame f_tela:
        assign rs_cod_dwb_output:screen-value  = dwb_rpt_param.cod_dwb_output.
        if  dwb_rpt_param.cod_dwb_output = "Arquivo" then
            ed_1x40:screen-value = dwb_rpt_param.cod_dwb_file.

        if  dwb_rpt_param.cod_dwb_output = "Impressora" then do:
            if  not can-find(imprsor_usuar
                       where imprsor_usuar.nom_impressora = dwb_rpt_param.nom_dwb_printer
                         and imprsor_usuar.cod_usuario    = dwb_rpt_param.cod_dwb_user
&if "{&emsbas_version}" >= "5.01" &then
                    use-index imprsrsr_id
&endif
                     )
            or   not can-find(layout_impres
                        where layout_impres.nom_impressora    = dwb_rpt_param.nom_dwb_printer
                          and layout_impres.cod_layout_impres = dwb_rpt_param.cod_dwb_print_layout) then do:
                run pi_set_print_layout_default /*pi_set_print_layout_default*/.
            end.
            assign ed_1x40:screen-value = dwb_rpt_param.nom_dwb_printer + ":" +
                                          dwb_rpt_param.cod_dwb_print_layout.
        end.
    end /* do init */.

    display v_qtd_column
            v_qtd_line
            with frame f_tela.

    enable ls_order
           rs_cod_dwb_output
           bt_up
           bt_down
           bt_get_file
           bt_set_printer
           bt_close
           bt_print
           bt_hel2
           rs_ind_run_mode           
           /*rs_tipo_log*/
           /*v_cod_estab_ini
           v_cod_estab_fim*/
           v_cod_emp_orig_ini
           v_cod_emp_orig_fim
           v_cod_emp_dest_ini
           /*v_cod_emp_dest_fim}*/
           v_lote_contabil_ini
           v_lote_contabil_fim
           /*v_dat_lote_ini
           v_dat_lote_fim*/
           v_dat_lancamento_ini
           v_dat_lancamento_fim
           v_cenario_ini
           v_cenario_fim
           v_modulo_ini
           v_modulo_fim
           bt_zoom_cod_emp_orig_ini
           bt_zoom_cod_emp_orig_fim
           /*bt_zoom_cod_emp_dest_ini
           bt_zoom_cod_emp_dest_fim*/
           /*bt_zoom_cod_estab_ini
           bt_zoom_cod_estab_fim*/
           /*bt_zoom_lote_contabil_ini
           bt_zoom_lote_contabil_fim*/
           bt_zoom_cenario_ini
           bt_zoom_cenario_fim
           bt_zoom_modulo_ini
           bt_zoom_modulo_fim
           bt_close
           bt_print
        with frame f_tela.

    assign v_cod_emp_orig_ini  :SCREEN-VALUE IN FRAME f_tela = ''
           v_cod_emp_orig_fim  :SCREEN-VALUE IN FRAME f_tela = 'ZZZ'
           v_cod_emp_dest_ini  :SCREEN-VALUE IN FRAME f_tela = ''
           /*v_cod_emp_dest_fim  :SCREEN-VALUE IN FRAME f_tela = 'ZZZ'*/
           /*v_cod_estab_ini     :SCREEN-VALUE IN FRAME f_tela = ''
           v_cod_estab_fim     :SCREEN-VALUE IN FRAME f_tela = 'ZZZ'*/
           v_lote_contabil_fim :screen-value in frame f_tela = "999999999"
           /*v_dat_lote_ini      :screen-value in frame f_tela = "01/01/1800"
           v_dat_lote_fim      :screen-value in frame f_tela = string(today)*/
           v_dat_lancamento_ini:screen-value in frame f_tela = "01/01/1800"
           v_dat_lancamento_fim:screen-value in frame f_tela = string(today)
           v_cenario_fim       :screen-value in frame f_tela = "ZZZZZZZZZ"
           v_modulo_fim        :screen-value in frame f_tela = "ZZZ".

    if  num-entries(v_cod_dwb_order) < 2 then do:
        disable bt_down
                bt_up
                with frame f_tela.
    end.

    /* tech38629 - Alteraá∆o efetuada via filtro */
    &if '{&emsbas_version}':U >= '5.05':U &then
        run pi_posiciona_dwb_rpt_param in v_prog_filtro_pdf (input rowid(dwb_rpt_param)).
        run pi_load_params in v_prog_filtro_pdf.
    &endif
    /* tech38629 - Fim da alterá∆o */

    apply "value-changed" to rs_cod_dwb_output in frame f_tela.

    block1:
    repeat on error undo block1, retry block1:

        main_block:
        repeat on error undo super_block, leave super_block
                            on endkey undo super_block, leave super_block
                            on stop undo super_block, retry super_block
                            with frame f_tela:

            if  retry THEN output stream s_1 close.

            assign v_log_print = no.

            if valid-handle( v_wgh_focus ) THEN wait-for go of frame f_tela focus v_wgh_focus.
            ELSE wait-for go of frame f_tela.
            assign dwb_rpt_param.cod_dwb_order = ls_order:list-items
                   v_cod_dwb_order             = ls_order:list-items
                   input frame f_tela v_qtd_line.

            if  v_log_print = yes
           then do:
                assign v_log_method       = session:set-wait-state('general')
                       v_nom_report_title = fill(" ", 40 - length(v_rpt_s_1_name)) + v_rpt_s_1_name.
                /* out_def: */
            &if '{&emsbas_version}':U >= '5.05':U &then
            /*                    case dwb_rpt_param.cod_dwb_output:*/
            &else
                            case dwb_rpt_param.cod_dwb_output:
            &endif
            &if '{&emsbas_version}':U >= '5.05':U &then
            /*                        when "Terminal" then out_term:*/
                                if dwb_rpt_param.cod_dwb_output = 'Terminal' then
            &else
                                when "Terminal" then out_term:
            &ENDIF do:
                assign v_cod_dwb_file   = session:temp-directory + "esfgl072":U + '.tmp'
                       v_rpt_s_1_bottom = v_qtd_line - (v_rpt_s_1_lines - v_qtd_bottom).
                
                output stream s_1 to value(v_cod_dwb_file) paged page-size value(v_qtd_line) no-echo convert target 'iso8859-1'.
            END.

&if '{&emsbas_version}':U >= '5.05':U &then
/*                        when "Impressora" then out_print:*/
                    if dwb_rpt_param.cod_dwb_output = 'Impressora' and getCodTipoRelat() <> 'PDF':U and getCodTipoRelat() <> 'RTF':U then
&else
                    when "Impressora" then out_print:
&endif
                     do:
                        find imprsor_usuar no-lock
                             where imprsor_usuar.nom_impressora = dwb_rpt_param.nom_dwb_printer
                               and imprsor_usuar.cod_usuario    = dwb_rpt_param.cod_dwb_user
&if "{&emsbas_version}" >= "5.01" &then
                             use-index imprsrsr_id
&endif
                              no-error.
                        find impressora no-lock
                             where impressora.nom_impressora = imprsor_usuar.nom_impressora
                              no-error.
                        find tip_imprsor no-lock
                             where tip_imprsor.cod_tip_imprsor = impressora.cod_tip_imprsor
                              no-error.
                        find layout_impres no-lock
                             where layout_impres.nom_impressora    = dwb_rpt_param.nom_dwb_printer
                               and layout_impres.cod_layout_impres = dwb_rpt_param.cod_dwb_print_layout no-error.
                        assign v_rpt_s_1_bottom = layout_impres.num_lin_pag - (v_rpt_s_1_lines - v_qtd_bottom).
&if '{&emsbas_version}' > '1.00' &then
                        if  v_nom_dwb_print_file <> "" then
                            if  layout_impres.num_lin_pag = 0 then
                                output stream s_1 to value(lc(v_nom_dwb_print_file))
                                       page-size 0 no-echo convert target tip_imprsor.cod_pag_carac_conver.
                            else
                                output stream s_1 to value(lc(v_nom_dwb_print_file))
                                       paged page-size value(layout_impres.num_lin_pag) no-echo convert target  tip_imprsor.cod_pag_carac_conver.
                        else
&endif
                            if  layout_impres.num_lin_pag = 0 then
                                output stream s_1 to value(imprsor_usuar.nom_disposit_so)
                                       page-size 0 no-echo convert target tip_imprsor.cod_pag_carac_conver.
                            else
                                output stream s_1 to value(imprsor_usuar.nom_disposit_so)
                                       paged page-size value(layout_impres.num_lin_pag) no-echo convert target  tip_imprsor.cod_pag_carac_conver.

                        setting:
                        for each configur_layout_impres no-lock
                           where configur_layout_impres.num_id_layout_impres = layout_impres.num_id_layout_impres
                              by configur_layout_impres.num_ord_funcao_imprsor:

                            find configur_tip_imprsor no-lock
                                 where configur_tip_imprsor.cod_tip_imprsor        = layout_impres.cod_tip_imprsor
                                   and configur_tip_imprsor.cod_funcao_imprsor     = configur_layout_impres.cod_funcao_imprsor
                                   and configur_tip_imprsor.cod_opc_funcao_imprsor = configur_layout_impres.cod_opc_funcao_imprsor
&if "{&emsbas_version}" >= "5.01" &then
                                 use-index cnfgrtpm_id
&endif
                                  no-error.
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

&if '{&emsbas_version}':U >= '5.05':U &then
/*                        when "Arquivo" then out_file:*/
                    if dwb_rpt_param.cod_dwb_output = 'Impressora' and getCodTipoRelat() = 'PDF':U then do:
                        run pi_config_output_print_pdf in v_prog_filtro_pdf (input v_qtd_line, input-output v_cod_dwb_file, input dwb_rpt_param.cod_dwb_user, input no).
                    end.
                    if dwb_rpt_param.cod_dwb_output = 'Arquivo' then
&else
                    when "Arquivo" then out_file:
&endif
                     do:
                        assign v_cod_dwb_file   = dwb_rpt_param.cod_dwb_file
                               v_rpt_s_1_bottom = v_qtd_line - (v_rpt_s_1_lines - v_qtd_bottom).
/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
run pi_rename_file in v_prog_filtro_pdf (input-output v_cod_dwb_file).
&endif
/* tech38629 - Fim da alteraá∆o */

                        output stream s_1 to value(v_cod_dwb_file)
                               paged page-size value(v_qtd_line) no-echo convert target 'iso8859-1'.
                    end /* do out_file */.
&if '{&emsbas_version}':U >= '5.05':U &then
/*                    end /* case out_def */.*/
&else
                end /* case out_def */.
&endif
                assign v_nom_prog_ext = caps(substr("esfgl072.p",12,8))
                       v_dat_execution = today
                       v_hra_execution = replace(string(time,"hh:mm:ss" ),":","").

                if  dwb_rpt_param.log_dwb_print_parameters then
                    run pi_print_parameters /*pi_print_parameters*/.

                output stream s_1 close.

/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
run pi_call_convert_object in v_prog_filtro_pdf (input no,
                                                 input rs_cod_dwb_output:screen-value in frame f_tela,
                                                 input v_nom_dwb_print_file,
                                                 input v_cod_dwb_file,
                                                 input v_nom_report_title).
&endif
/* tech38629 - Fim da alteraá∆o */
&if '{&emsbas_version}':U >= '5.05':U &then
    if (dwb_rpt_param.cod_dwb_output = 'Impressora' and getCodTipoRelat() = 'PDF':U) then do:
        if  v_nom_dwb_print_file = '' then
            run pi_print_pdf_file in v_prog_filtro_pdf (input no).
    end.
&endif
                assign v_log_method = session:set-wait-state("").
                if  dwb_rpt_param.cod_dwb_output = "Terminal" then do:
                /* tech38629 - Alteraá∆o efetuada via filtro */
                &if '{&emsbas_version}':U >= '5.05':U &then
                    if  getCodTipoRelat() = 'PDF':U and OPSYS = 'WIN32':U then do:
                        run pi_open_pdf_file in v_prog_filtro_pdf.
                    end.
                    else if getCodTipoRelat() = 'Texto' then do:
                &endif
                /* tech38629 - Fim da alteraá∆o */
                    run pi_show_report_2 (Input v_cod_dwb_file) /*pi_show_report_2*/.
                /* tech38629 - Alteraá∆o efetuada via filtro */
                &if '{&emsbas_version}':U >= '5.05':U &then
                    end.
                &endif
                /* tech38629 - Fim da alteraá∆o */
                end.
                leave main_block.
            end.
            else
                leave super_block.
        end /* repeat main_block */.

        if v_num_ped_exec <> 0 then do:
            /* Criado pedido &1 para execuá∆o batch. */
            run pi_messages (input "show", input 3556,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                v_num_ped_exec)) /*msg_3556*/.
            assign v_num_ped_exec = 0.
        end.

    end /* repeat block1 */.
end /* repeat super_block */.

hide frame f_tela.

/* Begin_Include: i_log_exec_prog_dtsul_fim */
if  v_rec_log <> ? then do trans:
    find log_exec_prog_dtsul where recid(log_exec_prog_dtsul) = v_rec_log exclusive-lock no-error.
    if  avail log_exec_prog_dtsul then
        assign log_exec_prog_dtsul.dat_fim_exec_prog_dtsul = today
               log_exec_prog_dtsul.hra_fim_exec_prog_dtsul = replace(string(time,"hh:mm:ss" ),":","").

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
*****************************************************************************/
PROCEDURE pi_return_user:
    /************************ Parameter Definition Begin ************************/

    def output param p_nom_user
        as character
        format "x(32)"
        no-undo.

    /************************* Parameter Definition End *************************/

    assign p_nom_user = v_cod_usuar_corren.

    if  v_cod_usuar_corren begins 'es_' then
        assign v_cod_usuar_corren = entry(2,v_cod_usuar_corren,"_").

END PROCEDURE. /* pi_return_user */
/*****************************************************************************
** Procedure Interna.....: pi_filename_validation
** Descricao.............: pi_filename_validation
*****************************************************************************/
PROCEDURE pi_filename_validation:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_filename
        as character
        format "x(40)"
        no-undo.

    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_cod_1 as char no-undo.
    def var v_cod_2 as char no-undo.
    def var v_num_1 as int  no-undo.
    def var v_num_2 as int  no-undo.

    /************************** Variable Definition End *************************/

    if  p_cod_filename = "" or p_cod_filename = "." then
        return "NOK".

    assign v_cod_1 = replace(p_cod_filename, "~\", "/").

    1_block:
    repeat v_num_1 = 1 to length(v_cod_1):
        if  index('abcdefghijklmnopqrstuvwxyz0123456789-_:/.', substr(v_cod_1, v_num_1, 1)) = 0 then
            return "NOK" .
    end /* repeat 1_block */.

    if  num-entries(v_cod_1, ":") > 2 then
        return "NOK" .

    if  num-entries(v_cod_1, ":") = 2 and length(entry(1,v_cod_1,":")) > 1 then
        return "NOK" .

    if  num-entries(v_cod_1, ".") > 2 then
        return "NOK" .

    if  num-entries(v_cod_1, ".") = 2 and length(entry(2,v_cod_1,".")) > 3 then
        return "NOK" .

    if  index(entry(num-entries(v_cod_1, "/"),v_cod_1, "/"),".") = 0 then
        return "NOK" .
    else do:
        if  entry(1,entry(num-entries(v_cod_1,"/"),v_cod_1,"/"),".") = ""
        or  entry(2,entry(num-entries(v_cod_1,"/"),v_cod_1,"/"),".") = "" then
           return "NOK" .
    end.

    assign v_num_1 = 1.
    2_block:
    repeat v_num_2 = 1 to length(v_cod_1):
        if  index(":" + "/" + ".", substr(v_cod_1, v_num_2, 1)) > 0 then
            assign v_cod_2 = substr(v_cod_1, v_num_1, v_num_2 - v_num_1)
                   v_num_1 = v_num_2 + 1.
    end /* repeat 2_block */.
    assign v_cod_2 = substr(v_cod_1, v_num_1).

    return "OK" /*l_ok*/ .
END PROCEDURE. /* pi_filename_validation */
/*****************************************************************************
** Procedure Interna.....: pi_set_print_layout_default
** Descricao.............: pi_set_print_layout_default
*****************************************************************************/
PROCEDURE pi_set_print_layout_default:

    dflt:
    do with frame f_tela:
        find layout_impres_padr no-lock
             where layout_impres_padr.cod_usuario = v_cod_dwb_user
               and layout_impres_padr.cod_proced  = v_cod_dwb_proced
    &if "{&emsbas_version}" >= "5.01" &then
             use-index lytmprsp_id
    &endif
              no-error.
        if  not avail layout_impres_padr then do:
            find layout_impres_padr no-lock
                 where layout_impres_padr.cod_usuario = "*"
                   and layout_impres_padr.cod_proced  = v_cod_dwb_proced
    &if "{&emsbas_version}" >= "5.01" &then
                 use-index lytmprsp_id
    &endif
                  no-error.
            if  avail layout_impres_padr then do:
                find imprsor_usuar no-lock
                     where imprsor_usuar.nom_impressora = layout_impres_padr.nom_impressora
                       and imprsor_usuar.cod_usuario    = v_cod_dwb_user
    &if "{&emsbas_version}" >= "5.01" &then
                     use-index imprsrsr_id
    &endif
                      no-error.
            end.
            if  not avail imprsor_usuar then do:
                find layout_impres_padr no-lock
                     where layout_impres_padr.cod_usuario = v_cod_dwb_user
                       and layout_impres_padr.cod_proced  = "*"
    &if "{&emsbas_version}" >= "5.01" &then
                     use-index lytmprsp_id
    &endif
                      no-error.
            end.
        end.
        do trans:
            find dwb_rpt_param exclusive-lock
                where dwb_rpt_param.cod_dwb_user    = v_cod_usuar_corren
                  and dwb_rpt_param.cod_dwb_program = v_cod_dwb_program no-error.
            if  avail layout_impres_padr then
                assign dwb_rpt_param.nom_dwb_printer      = layout_impres_padr.nom_impressora
                       dwb_rpt_param.cod_dwb_print_layout = layout_impres_padr.cod_layout_impres
                       ed_1x40:screen-value               = dwb_rpt_param.nom_dwb_printer + ":" +
                                                            dwb_rpt_param.cod_dwb_print_layout.
            else
                assign dwb_rpt_param.nom_dwb_printer       = ""
                       dwb_rpt_param.cod_dwb_print_layout  = ""
                       ed_1x40:screen-value = "".
        end.
    end /* do dflt */.
END PROCEDURE. /* pi_set_print_layout_default */
/*****************************************************************************
** Procedure Interna.....: pi_show_report_2
** Descricao.............: pi_show_report_2
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
    or   v_cod_key_value = ? then
        assign v_cod_key_value = 'notepad.exe'.
        put-key-value section 'EMS' key 'Show-Report-Program' value v_cod_key_value no-error.

    run winexec (input v_cod_key_value + chr(32) + p_cod_dwb_file, input 1).

    END PROCEDURE.

    PROCEDURE WinExec EXTERNAL 'kernel32.dll':
      DEF INPUT  PARAM prg_name                          AS CHARACTER.
      DEF INPUT  PARAM prg_style                         AS SHORT.
END PROCEDURE. /* pi_show_report_2 */
/*****************************************************************************
** Procedure Interna.....: pi_print_parameters
** Descricao.............: pi_print_parameters
*****************************************************************************/
PROCEDURE pi_print_parameters:

    if  page-number (s_1) > 0 then
        page stream s_1.

    if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
        page stream s_1.
    put stream s_1 unformatted
        skip (1)
        "Usuˇrio: " at 1
        v_cod_usuar_corren at 10 format "x(12)" skip (1).

    if (line-counter(s_1) + 4) > v_rpt_s_1_bottom then
        page stream s_1.
    put stream s_1 unformatted
        skip (1)
        "Ordem" to 5
        "Classificador" at 7 skip
        "-----" to 5
        "--------------------------------" at 7 skip.
    1_block:
    repeat v_num_entry = 1 to num-entries (v_cod_dwb_order):
        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
            page stream s_1.
        put stream s_1 unformatted
            v_num_entry to 5 format ">>>>9"
            entry(v_num_entry,v_cod_dwb_order) at 7 format "x(32)" skip.
    end /* repeat 1_block */.

    if (line-counter(s_1) + 4) > v_rpt_s_1_bottom then
        page stream s_1.
    put stream s_1 unformatted
        skip (1)
        "Tipo" at 1
        "Conjunto" at 9
        "Inicial" at 42
        "Final" at 61 skip
        "-------" at 1
        "--------------------------------" at 9
        "------------------" at 42
        "------------------" at 61 skip.
END PROCEDURE. /* pi_print_parameters */
/*****************************************************************************
** Procedure Interna.....: pi_initialize_reports
** Descricao.............: pi_initialize_reports
*****************************************************************************/
PROCEDURE pi_initialize_reports:

    /* inicializa variˇveis */
    find empresa no-lock
         where empresa.cod_empresa = v_cod_empres_usuar no-error.
    find dwb_rpt_param no-lock
         where dwb_rpt_param.cod_dwb_program = "esfgl072":U
           and dwb_rpt_param.cod_dwb_user    = v_cod_dwb_user no-error.

    if  avail dwb_rpt_param then do:
    &if '{&emsbas_version}' > '1.00' &then
    &if '{&emsbas_version}' >= '5.03' &then
        assign v_nom_dwb_print_file = dwb_rpt_param.nom_dwb_print_file.
    &else
        assign v_nom_dwb_print_file = dwb_rpt_param.cod_livre_1.
    &endif
    &endif
        assign v_qtd_line = if  dwb_rpt_param.qtd_dwb_line <> 0 then dwb_rpt_param.qtd_dwb_line else v_rpt_s_1_lines.
    end.

    assign v_cod_dwb_proced  = "esfgl072rp":U
           v_cod_dwb_program = "esfgl072rp":U
           v_cod_dwb_order   = "Lote Cont†bil,Data Lote Cont†bil,Data Lanáamento"
           v_cod_release     = trim(" 1.00.00.000":U)
           v_cod_dwb_select  = "Lote Cont†bil,Data Lote Cont†bil,Data Lanáamento"
           v_qtd_column      = v_rpt_s_1_columns
           v_qtd_bottom      = v_rpt_s_1_bottom.
    assign v_nom_enterprise = if  avail empresa then empresa.nom_razao_social else 'DATASUL'.
END PROCEDURE. /* pi_initialize_reports */
/*****************************************************************************
** Procedure Interna.....: pi_configure_dwb_param
** Descricao.............: pi_configure_dwb_param
*****************************************************************************/
PROCEDURE pi_configure_dwb_param:
        find dwb_rpt_param exclusive-lock
             where dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
               and dwb_rpt_param.cod_dwb_user = v_cod_dwb_user no-error.
        if  not available dwb_rpt_param then do:
            create dwb_rpt_param.
            assign dwb_rpt_param.cod_dwb_program       = v_cod_dwb_program
                   dwb_rpt_param.cod_dwb_user          = v_cod_dwb_user
                   dwb_rpt_param.cod_dwb_parameters    = v_cod_dwb_parameters
                   dwb_rpt_param.cod_dwb_output        = "Terminal"
                   dwb_rpt_param.cod_dwb_order         = v_cod_dwb_order
                   dwb_rpt_param.ind_dwb_run_mode      = "On-Line"
                   dwb_rpt_param.cod_dwb_file          = ""
                   dwb_rpt_param.nom_dwb_printer       = ""
                   dwb_rpt_param.cod_dwb_print_layout  = ""
                   v_cod_dwb_file_temp                 = ""
                   ls_order:list-items in frame f_tela = v_cod_dwb_order.
        end.
        else do:
            assign ls_order:list-items in frame f_tela = "!".
            if  ls_order:delete(1) in frame f_tela then do:
                order_1:
                repeat v_num_entry = 1 to num-entries (dwb_rpt_param.cod_dwb_order):
                    assign v_cod_dwb_field = entry (v_num_entry, dwb_rpt_param.cod_dwb_order).
                    if  lookup (v_cod_dwb_field, v_cod_dwb_order) > 0 and
                        ls_order:lookup (v_cod_dwb_field) = 0 then
                        assign v_log_method = ls_order:add-last(v_cod_dwb_field).
                end /* repeat order_1 */.
                order_2:
                repeat v_num_entry = 1 to num-entries (v_cod_dwb_order):
                    assign v_cod_dwb_field = entry (v_num_entry, v_cod_dwb_order).
                    if  ls_order:lookup (v_cod_dwb_field) = 0 then
                       assign v_log_method = ls_order:add-last(v_cod_dwb_field).
                end /* repeat order_2 */.
            end.

            assign v_cod_dwb_file_temp = replace(dwb_rpt_param.cod_dwb_file, "~\", "~/").
            if  index(v_cod_dwb_file_temp, "~/") <> 0 then
                assign v_cod_dwb_file_temp = substr(v_cod_dwb_file_temp, r-index(v_cod_dwb_file_temp, "~/") + 1).
            else
                assign v_cod_dwb_file_temp = dwb_rpt_param.cod_dwb_file.
        end.
END PROCEDURE. /* pi_configure_dwb_param */
/*****************************************************************************
** Procedure Interna.....: pi_output_reports
** Descricao.............: pi_output_reports
*****************************************************************************/
PROCEDURE pi_output_reports:

/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
    run pi_posiciona_dwb_rpt_param in v_prog_filtro_pdf (input rowid(dwb_rpt_param)).
    run pi_load_params in v_prog_filtro_pdf.
&endif
/* tech38629 - Fim da alteraá∆o */

    assign v_log_method       = session:set-wait-state('general')
           v_nom_report_title = fill(" ",40 - length(v_rpt_s_1_name)) + v_rpt_s_1_name
           v_rpt_s_1_bottom   = v_qtd_line - (v_rpt_s_1_lines - v_qtd_bottom).

    /* block: */
&if '{&emsbas_version}':U >= '5.05':U &then
/*    case dwb_rpt_param.cod_dwb_output:*/
&else
    case dwb_rpt_param.cod_dwb_output:
&endif
&if '{&emsbas_version}':U >= '5.05':U &then
            if dwb_rpt_param.cod_dwb_output = 'Impressora' and getCodTipoRelat() = 'PDF':U then do:
                run pi_config_output_print_pdf in v_prog_filtro_pdf (input v_qtd_line, input-output v_cod_dwb_file, input v_cod_usuar_corren, input yes).
            end.
            if dwb_rpt_param.cod_dwb_output = 'Arquivo' then
&else
            when "Arquivo" then
&endif
            block1:
            do:
/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
run pi_rename_file in v_prog_filtro_pdf (input-output v_cod_dwb_file).
&endif
/* tech38629 - Fim da alteraá∆o */

               output stream s_1 to value(v_cod_dwb_file)
               paged page-size value(v_qtd_line) no-echo convert target 'iso8859-1'.
            end /* do block1 */.
&if '{&emsbas_version}':U >= '5.05':U &then
            if dwb_rpt_param.cod_dwb_output = 'Impressora' and getCodTipoRelat() <> 'PDF':U and getCodTipoRelat() <> 'RTF':U then
&else
            when "Impressora" then
&endif
               block2:
               do:
                  find imprsor_usuar use-index imprsrsr_id no-lock
                      where imprsor_usuar.nom_impressora = dwb_rpt_param.nom_dwb_printer
                      and   imprsor_usuar.cod_usuario    = v_cod_usuar_corren no-error.
                  find impressora no-lock
                       where impressora.nom_impressora = imprsor_usuar.nom_impressora no-error.
                  find tip_imprsor no-lock
                       where tip_imprsor.cod_tip_imprsor = impressora.cod_tip_imprsor no-error.
                  find layout_impres no-lock
                       where layout_impres.nom_impressora    = dwb_rpt_param.nom_dwb_printer
                         and layout_impres.cod_layout_impres = dwb_rpt_param.cod_dwb_print_layout no-error.
                  find b_ped_exec_style
                      where b_ped_exec_style.num_ped_exec = v_num_ped_exec_corren no-lock no-error.
                  find servid_exec_imprsor no-lock
                       where servid_exec_imprsor.nom_impressora  = dwb_rpt_param.nom_dwb_printer
                         and servid_exec_imprsor.cod_servid_exec = b_ped_exec_style.cod_servid_exec no-error.

                  find b_servid_exec_style no-lock
                       where b_servid_exec_style.cod_servid_exec = b_ped_exec_style.cod_servid_exec no-error.

                  if  avail layout_impres then
                      assign v_rpt_s_1_bottom = layout_impres.num_lin_pag - (v_rpt_s_1_lines - v_qtd_bottom).

                  if  available b_servid_exec_style
                  and b_servid_exec_style.ind_tip_fila_exec = 'UNIX' then do:
                      &if '{&emsbas_version}' > '1.00' &then
                      &if '{&emsbas_version}' >= '5.03' &then
                          if dwb_rpt_param.nom_dwb_print_file <> "" then do:
                              if  layout_impres.num_lin_pag = 0 then do:
                                  output stream s_1 to value(lc(dwb_rpt_param.nom_dwb_print_file))
                                         page-size 0 no-echo convert target tip_imprsor.cod_pag_carac_conver.
                              end.
                              else do:
                                  output stream s_1 to value(lc(dwb_rpt_param.nom_dwb_print_file))
                                         paged page-size value(layout_impres.num_lin_pag) no-echo convert target tip_imprsor.cod_pag_carac_conver.
                              end.
                          end.
                          else do:
                              if  layout_impres.num_lin_pag = 0 then do:
                                  output stream s_1 through value(servid_exec_imprsor.nom_disposit_so)
                                         page-size 0 no-echo convert target tip_imprsor.cod_pag_carac_conver.
                              end.
                              else do:
                                  output stream s_1 through value(servid_exec_imprsor.nom_disposit_so)
                                         paged page-size value(layout_impres.num_lin_pag) no-echo convert target tip_imprsor.cod_pag_carac_conver.
                              end.
                          end.
                      &else
                          if dwb_rpt_param.cod_livre_1 <> "" then do:
                              if  layout_impres.num_lin_pag = 0 then do:
                                  output stream s_1 to value(lc(dwb_rpt_param.cod_livre_1))
                                         page-size 0 no-echo convert target tip_imprsor.cod_pag_carac_conver.
                              end.
                              else do:
                                  output stream s_1 to value(lc(dwb_rpt_param.cod_livre_1))
                                         paged page-size value(layout_impres.num_lin_pag) no-echo convert target tip_imprsor.cod_pag_carac_conver.
                              end.
                          end.
                          else do:
                              if  layout_impres.num_lin_pag = 0 then do:
                                  output stream s_1 through value(servid_exec_imprsor.nom_disposit_so)
                                         page-size 0 no-echo convert target tip_imprsor.cod_pag_carac_conver.
                              end.
                              else do:
                                  output stream s_1 through value(servid_exec_imprsor.nom_disposit_so)
                                         paged page-size value(layout_impres.num_lin_pag) no-echo convert target tip_imprsor.cod_pag_carac_conver.
                              end.
                          end.
                      &endif
                      &endif
                  end.
                  else do:
                      &if '{&emsbas_version}' > '1.00' &then
                      &if '{&emsbas_version}' >= '5.03' &then
                          if dwb_rpt_param.nom_dwb_print_file <> "" then do:
                              if  layout_impres.num_lin_pag = 0 then do:
                                  output stream s_1 to value(lc(dwb_rpt_param.nom_dwb_print_file))
                                         page-size 0 no-echo convert target tip_imprsor.cod_pag_carac_conver.
                              end.
                              else do:
                                  output stream s_1 to value(lc(dwb_rpt_param.nom_dwb_print_file))
                                         paged page-size value(layout_impres.num_lin_pag) no-echo convert target tip_imprsor.cod_pag_carac_conver.
                              end.
                          end.
                          else do:
                              if  layout_impres.num_lin_pag = 0 then do:
                                  output stream s_1 to value(servid_exec_imprsor.nom_disposit_so)
                                         page-size 0 convert target tip_imprsor.cod_pag_carac_conver.
                              end.
                              else do:
                                  output stream s_1 to value(servid_exec_imprsor.nom_disposit_so)
                                         paged page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver.
                              end.
                          end.
                      &else
                          if dwb_rpt_param.cod_livre_1 <> "" then do:
                              if  layout_impres.num_lin_pag = 0 then do:
                                  output stream s_1 to value(lc(dwb_rpt_param.cod_livre_1))
                                         page-size 0 convert target tip_imprsor.cod_pag_carac_conver.
                              end.
                              else do:
                                  output stream s_1 to value(lc(dwb_rpt_param.cod_livre_1))
                                         paged page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver.
                              end.
                          end.
                          else do:
                              if  layout_impres.num_lin_pag = 0 then do:
                                  output stream s_1 to value(servid_exec_imprsor.nom_disposit_so)
                                         page-size 0 convert target tip_imprsor.cod_pag_carac_conver.
                              end.
                              else do:
                                  output stream s_1 to value(servid_exec_imprsor.nom_disposit_so)
                                         paged page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver.
                              end.
                          end.
                      &endif
                      &endif
                  end.

                  setting:
                  for each configur_layout_impres no-lock
                     where configur_layout_impres.num_id_layout_impres = layout_impres.num_id_layout_impres
                        by configur_layout_impres.num_ord_funcao_imprsor:

                      find configur_tip_imprsor no-lock
                           where configur_tip_imprsor.cod_tip_imprsor = layout_impres.cod_tip_imprsor
                             and configur_tip_imprsor.cod_funcao_imprsor = configur_layout_impres.cod_funcao_imprsor
                             and configur_tip_imprsor.cod_opc_funcao_imprsor = configur_layout_impres.cod_opc_funcao_imprsor
    &if "{&emsbas_version}" >= "5.01" &then
                           use-index cnfgrtpm_id
    &endif
                            no-error.

                      bloco_1:
                      do v_num_count = 1 to extent(configur_tip_imprsor.num_carac_configur):
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
&if '{&emsbas_version}':U >= '5.05':U &then
/*    end /* case block */.*/
&else
    end /* case block */.
&endif
END PROCEDURE. /* pi_output_reports */
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
/***********************  End of esfgl072 ***********************/

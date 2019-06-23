/****************************************************************************
** Programa..............: ESFGL073
** Descricao.............: Concilia‡Æo das bases
** Procedimento..........: esfgl073
** Nome Externo..........: esp/esfgl073.p
** Criado por............: Hilton Borba
** Criado em.............: 04/10/2011
*****************************************************************************/
def var c-versao-prg as char initial " 1.00.00.000":U no-undo.

/* buffer para tabelas iguais no ems2 e ems5 - ini */
def buffer empresa for emsuni.empresa.
/* buffer para tabelas iguais no ems2 e ems5 - fim */

{include/i_dbinst.i}
{include/i_dbtype.i}
{include/i_fcldef.i}

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i esfgl073 FGL}
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
                                   "ESFGL073","~~EMSFIN", "~~{~&emsfin_version}", "~~1.00")) /*msg_5009*/.
&else

/********************* Temporary Table Definition Begin *********************/

def temp-table tt_esfgl073 no-undo like caract_espec_pat
    field ttv_rec_caract_espec_pat         as recid format ">>>>>>9"
    field ttv_cod_dwb_field_rpt            as character extent 13 format "x(32)" label "Conjunto" column-label "Conjunto"
    field tta_des_caract_pat               as character format "x(32)" label "Descri‡Æo Caract Pat" column-label "Descri‡Æo"
    field tta_des_espec_bem                as character format "x(32)" label "Descri‡Æo Esp‚cie" column-label "Descri‡Æo Esp‚cie"
    index tt_esfgl073_id       is primary unique
          ttv_rec_caract_espec_pat         ascending
    index tt_rpt_cod_caract_pat
          cod_caract_pat                   ascending
    index tt_rpt_cod_espec_bem
          cod_espec_bem                    ascending
    .

def temp-table tt_param no-undo
    FIELD c_empresa_orig_ini AS CHAR FORMAT "x(03)"
    FIELD c_empresa_orig_fim AS CHAR FORMAT "x(03)"
    FIELD c_empresa_Dest     AS CHAR FORMAT "X(03)"
    FIELD i_lote_orig_ini   AS INT  FORMAT ">>>>>>>>9"
    FIELD i_lote_orig_fim   AS INT  FORMAT ">>>>>>>>9"
    FIELD i_lote_dest_ini   AS INT  FORMAT ">>>>>>>>9"
    FIELD i_lote_dest_fim   AS INT  FORMAT ">>>>>>>>9"
    FIELD dt_lote_orig_ini  AS DATE FORMAT 99/99/9999
    FIELD dt_lote_orig_fim  AS DATE FORMAT 99/99/9999
    FIELD dt_lote_dest_ini  AS DATE FORMAT 99/99/9999
    FIELD dt_lote_dest_fim  AS DATE FORMAT 99/99/9999
    FIELD dt_lanca_orig_ini AS DATE FORMAT 99/99/9999
    FIELD dt_lanca_orig_fim AS DATE FORMAT 99/99/9999
    FIELD c_cenario_ini     AS CHAR FORMAT "x(09)"
    FIELD c_cenario_fim     AS CHAR FORMAT "x(09)"
    FIELD c_modulo_ini      AS CHAR FORMAT "x(03)"
    FIELD c_modulo_fim      AS CHAR FORMAT "x(03)"
    field valida_origem     as log 
    FIELD class-1           AS CHAR FORMAT "x(15)"
    FIELD class-2           AS CHAR FORMAT "x(15)"
    FIELD class-3           AS CHAR FORMAT "x(15)"
    FIELD tipo_detalhado    AS LOG
    FIELD rs_output         AS CHAR
    FIELD c_arquivo         AS CHAR FORMAT "x(30)".

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
    label "Usu rio"
    column-label "Usu rio"
    no-undo.
def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.

def new global shared var v_cod_aplicat_dtsul_corren
    as character
    format "x(3)":U
    no-undo.
def new global shared var v_cod_funcao_negoc_empres
    as character
    format "x(50)":U
    no-undo.
def new global shared var v_cod_grp_usuar_lst
    as character
    format "x(3)":U
    label "Grupo Usuÿrios"
    column-label "Grupo"
    no-undo.
def new global shared var v_cod_idiom_usuar
    as character
    format "x(8)":U
    label "Idioma"
    column-label "Idioma"
    no-undo.
def new global shared var v_cod_estab_usuar
    as character
    format "x(3)":U
    label "Estabelecimento"
    column-label "Estab"
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
    label "Pa¡s Empresa Usuÿrio"
    column-label "Pa¡s"
    no-undo.
def new global shared var v_cod_plano_ccusto_corren
    as character
    format "x(8)":U
    label "Plano CCusto"
    column-label "Plano CCusto"
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
    label "Usu rio Corrente"
    column-label "Usu rio Corrente"
    no-undo.
def new global shared var v_cod_usuar_corren_criptog
    as character
    format "x(16)":U
    no-undo.
def new global shared var v_log_execution
    as logical
    format "Sim/NÆo"
    initial yes
    no-undo.
def new global shared var v_num_ped_exec_corren
    as integer
    format ">>>>9":U
    no-undo.
def new global shared var v_rec_caract_espec_pat
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_empresa
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_lote_contabil
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_cenario
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_modulo
    as recid
    format ">>>>>>9":U
    no-undo.
def new shared var v_cod_dat_type
    as character
    format "x(8)":U
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
def new shared var v_cod_final
    as character
    format "x(8)":U
    initial ?
    label "Final"
    no-undo.
def new shared var v_cod_format
    as character
    format "x(8)":U
    label "Formato"
    column-label "Formato"
    no-undo.
def new shared var v_cod_initial
    as character
    format "x(8)":U
    initial ?
    label "Inicial"
    no-undo.
def new shared var v_cod_release
    as character
    format "x(12)":U
    no-undo.
def new shared var v_dat_execution
    as date
    format "99/99/9999":U
    no-undo.
def new shared var v_dat_fim_period
    as date
    format "99/99/9999":U
    label "Fim Per¡odo"
    no-undo.
def new shared var v_dat_inic_period
    as date
    format "99/99/9999":U
    label "In¡cio Per¡odo"
    column-label "Per¡odo"
    no-undo.
def new shared var v_hra_execution
    as character
    format "99:99":U
    no-undo.
def new shared var v_hra_execution_end
    as character
    format "99:99:99":U
    label "Tempo Exec"
    no-undo.
def new shared var v_empresa
    as character
    format "x(03)":U
    label "Empresa Origem"
    no-undo.

def new shared var v_empresa_fim
    as character
    INITIAL 'ZZZ'
    format "x(03)":U
    label "Empresa Origem"
    no-undo.

def new shared var v_empresa_dest
    as character
    INITIAL 'ZZZ'
    format "x(03)":U
    label "Empresa Destino"
    no-undo.

DEF NEW SHARED VAR v_lote_contabil_orig_ini
    AS INTEGER
    FORMAT ">>>>>>>>9":U
    LABEL "Lote Cont bil Origem"
    NO-UNDO.
DEF NEW SHARED VAR v_lote_contabil_orig_fim
    AS INTEGER
    FORMAT ">>>>>>>>9":U
    NO-UNDO.
DEF NEW SHARED VAR v_lote_contabil_dest_ini
    as integer
    format ">>>>>>>>9":U
    LABEL "Lote Cont bil Destino"
    no-undo.
DEF NEW SHARED VAR v_lote_contabil_dest_fim
    as integer
    format ">>>>>>>>9":U
    no-undo.
DEF NEW SHARED VAR v_dat_lote_orig_ini
    AS DATE
    FORMAT "99/99/9999":U
    LABEL "Data Lote Origem"
    NO-UNDO.
DEF NEW shared VAR v_dat_lote_orig_fim
    as date
    format "99/99/9999":U
    NO-UNDO.
DEF NEW SHARED VAR v_dat_lote_dest_ini
    AS DATE
    FORMAT "99/99/9999":U
    LABEL "Data Lote Destino"
    no-undo.
DEF NEW shared VAR v_dat_lote_dest_fim
    as date
    format "99/99/9999":U
    no-undo.
def new shared var v_dat_lancamento_ini
    as date
    format "99/99/9999":U
    label "Data Lan‡amento"
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
def new shared var v_valida_origem
    as logical
    format "yes/no"                         
    label "Valida Lote Destino contra Origem"
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
def new shared var v_num_page_number
    as integer
    format ">>>>>9":U
    label "Pÿgina"
    column-label "Pÿgina"
    no-undo.
def new shared var v_rec_dwb_rpt_select
    as recid
    format ">>>>>>9":U
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
    label "Classifica‡ao"
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
def var v_cod_order
    as character
    format "x(40)":U
    no-undo.
def var v_cod_ult_obj_procesdo
    as character
    format "x(32)":U
    no-undo.
def var v_ind_dwb_run_mode
    as character
    format "X(07)":U
    initial "On-Line" /*l_online*/
    view-as radio-set Horizontal
    radio-buttons "On-Line", "On-Line","Batch", "Batch"
    bgcolor 8
    label "Run Mode"
    column-label "Run Mode"
    no-undo.
def var v_log_method
    as logical
    format "Sim/NÆo"
    initial yes
    no-undo.
def var v_log_print
    as logical
    format "Sim/NÆo"
    initial no
    no-undo.
def var v_nom_dwb_printer
    as character
    format "x(30)":U
    no-undo.
def var v_nom_dwb_print_file
    as character
    format "x(100)":U
    label "Arquivo ImpressÆo"
    column-label "Arq Impr"
    no-undo.
def var v_nom_integer
    as character
    format "x(30)":U
    no-undo.
def var v_nom_prog_appc
    as character
    format "x(50)":U
    label "Programa APPC"
    column-label "Programa APPC"
    no-undo.
def var v_nom_prog_dpc
    as character
    format "x(50)":U
    label "Programa Dpc"
    column-label "Programa Dpc"
    no-undo.
def var v_nom_prog_upc
    as character
    format "X(50)":U
    label "Programa UPC"
    column-label "Programa UPC"
    no-undo.
def var v_nom_table_epc
    as character
    format "x(30)":U
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
def var v_rec_table_epc
    as recid
    format ">>>>>>9":U
    no-undo.
def var v_wgh_focus
    as widget-handle
    format ">>>>>>9":U
    no-undo.
def var v_wgh_frame_epc
    as widget-handle
    format ">>>>>>9":U
    no-undo.

/************************** Variable Definition End *************************/

/*************************** Menu Definition Begin **************************/

def menu      m_help                menubar
    menu-item mi_conteudo           label "&Conte£do"
    menu-item mi_sobre              label "&Sobre".

/**************************** Menu Definition End ***************************/

/************************** Query Definition Begin **************************/

def query qr_dwb_rpt_select
    for dwb_rpt_select
    scrolling.

/*************************** Query Definition End ***************************/

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
def rectangle rt_select
    size 1 by 1
    edge-pixels 2.
def rectangle rt_target
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
    tooltip "Define Impressora e Layout de ImpressÆo"
    image-up file "image/im-setpr.bmp"
    image-insensitive file "image/ii-setpr"
    size 1 by 1.
def button bt_up
    label "At‚"
    tooltip "Sobe"
    image-up file "image/im-up"
    image-insensitive file "image/ii-up"
    size 1 by 1.
def button bt_zoom_empresa
    label "Psq"
    tooltip "Pesquisa"
    image-up file "image/im-sea2"
    image-insensitive file "image/ii-sea2".
def button bt_zoom_empresa_fim
    label "Psq"
    tooltip "Pesquisa"
    image-up file "image/im-sea2"
    image-insensitive file "image/ii-sea2".
def button bt_zoom_lote_contabil_orig_ini
    label "Psq"
    tooltip "Pesquisa"
    image-up file "image/im-sea2"
    image-insensitive file "image/ii-sea2".
def button bt_zoom_lote_contabil_orig_fim
    label "Psq"
    tooltip "Pesquisa"
    image-up file "image/im-sea2"
    image-insensitive file "image/ii-sea2".
def button bt_zoom_lote_contabil_dest_ini
    label "Psq"
    tooltip "Pesquisa"
    image-up file "image/im-sea2"
    image-insensitive file "image/ii-sea2".
def button bt_zoom_lote_contabil_dest_fim
    label "Psq"
    tooltip "Pesquisa"
    image-up file "image/im-sea2"
    image-insensitive file "image/ii-sea2".
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
    size 40 by 1
    bgcolor 15 font 2
    no-undo.

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
    " Classifica‡Æo " view-as text
         at row 01.20 col 04.00 bgcolor 8
    rt_order
         at row 01.50 col 02.00
    ls_order
         at row 02.00 col 03.00
         help "" no-label
    bt_up
         at row 03.40 col 29.00 font ?
         help "Sobe"
    bt_down
         at row 04.60 col 29.00 font ?
         help "Desce"

    " Sele‡Æo " view-as text
         at row 01.20 col 43.00
    rt_select
         at row 01.50 col 37.00
    v_empresa
         at row 02.50 col 52.00 colon-aligned label "Empresa Origem"
         view-as fill-in
         size-chars 5.14 by .88
         fgcolor ? bgcolor 15 font 2 
    bt_zoom_empresa
         at row 02.50 col 59.30 font ?
         help "Pesquisa"
    v_empresa_fim
         at row 02.50 col 70.00 colon-aligned  
         view-as fill-in
         size-chars 5.14 by .88
         fgcolor ? bgcolor 15 font 2 NO-LABEL 
    bt_zoom_empresa_fim
         at row 02.50 col 76.30 font ?
         help "Pesquisa"

    v_empresa_dest
         at row 03.50 col 52.00 colon-aligned label "Empresa Destino"
         view-as fill-in
         size-chars 5.14 by .88
         fgcolor ? bgcolor 15 font 2 

    v_lote_contabil_orig_ini
        at row 04.50 col 52.00 colon-aligned label "Lote Cont bil Origem"
        view-as fill-in
        size-chars 11.14 by .88
        fgcolor ? bgcolor 15 font 2
    bt_zoom_lote_contabil_orig_ini
         at row 04.50 col 65.30 font ?
         help "Pesquisa"
    v_lote_contabil_orig_fim
        at row 04.50 col 70.00 colon-aligned no-label
        view-as fill-in
        size-chars 11.14 by .88
        fgcolor ? bgcolor 15 font 2
    bt_zoom_lote_contabil_orig_fim
         at row 04.50 col 83.30 font ?
         help "Pesquisa"
    v_lote_contabil_dest_ini
        at row 05.50 col 52.00 colon-aligned label "Lote Cont bil Destino"
        view-as fill-in
        size-chars 11.14 by .88
        fgcolor ? bgcolor 15 font 2
    bt_zoom_lote_contabil_dest_ini
         at row 05.50 col 65.30 font ?
         help "Pesquisa"
    v_lote_contabil_dest_fim
        at row 05.50 col 70.00 colon-aligned NO-LABEL
        view-as fill-in
        size-chars 11.14 by .88
        fgcolor ? bgcolor 15 font 2
    bt_zoom_lote_contabil_dest_fim
         at row 05.50 col 83.30 font ?
         help "Pesquisa"
    v_dat_lote_orig_ini
        at row 06.50 col 52.00 colon-aligned label "Data Lote Origem"
        view-as fill-in
        size-chars 11.14 by .88
        fgcolor ? bgcolor 15 font 2
    v_dat_lote_orig_fim
        at row 06.50 col 70.00 colon-aligned NO-LABEL
        view-as fill-in
        size-chars 11.14 by .88
        fgcolor ? bgcolor 15 font 2
    v_dat_lote_dest_ini
        at row 07.50 col 52.00 colon-aligned label "Data Lote Destino"
        view-as fill-in
        size-chars 11.14 by .88
        fgcolor ? bgcolor 15 font 2
    v_dat_lote_dest_fim
        at row 07.50 col 70.00 colon-aligned no-label
        view-as fill-in
        size-chars 11.14 by .88
        fgcolor ? bgcolor 15 font 2
    v_dat_lancamento_ini
        at row 08.50 col 52.00 colon-aligned label "Dt Lan‡amento"
        view-as fill-in
        size-chars 11.14 by .88
        fgcolor ? bgcolor 15 font 2
    v_dat_lancamento_fim
        at row 08.50 col 70.00 colon-aligned no-label
        view-as fill-in
        size-chars 11.14 by .88
        fgcolor ? bgcolor 15 font 2
    v_cenario_ini
        at row 09.50 col 52.00 colon-aligned label "Cen rio"
        view-as fill-in
        size-chars 11.14 by .88
        fgcolor ? bgcolor 15 font 2
    bt_zoom_cenario_ini
         at row 09.50 col 65.30 font ?
         help "Pesquisa"
    v_cenario_fim
        at row 09.50 col 70.00 colon-aligned no-label
        view-as fill-in
        size-chars 11.14 by .88
        fgcolor ? bgcolor 15 font 2
    bt_zoom_cenario_fim
         at row 09.50 col 83.30 font ?
         help "Pesquisa"
    v_modulo_ini
        at row 10.50 col 52.00 colon-aligned label "M¢dulo"
        view-as fill-in
        size-chars 5.14 by .88
        fgcolor ? bgcolor 15 font 2
    bt_zoom_modulo_ini
         at row 10.50 col 59.30 font ?
         help "Pesquisa"
    v_modulo_fim
        at row 10.50 col 70.00 colon-aligned no-label
        view-as fill-in
        size-chars 5.14 by .88
        fgcolor ? bgcolor 15 font 2
    v_valida_origem
        at row 12.50 col 42.00 colon-aligned no-label
        view-as toggle-box
        size-chars 38.14 by .88 font 2
    bt_zoom_modulo_fim
         at row 10.50 col 77.30 font ?
         help "Pesquisa"
/*
    v_ind_dwb_run_mode
        AT ROW 12.50 COL 50 */

    " Destino " view-as text
         at row 7.70 col 02.00 bgcolor 8
    rt_target
         at row 8.00 col 02.00
    rs_cod_dwb_output
         at row 8.30 col 03.00
         help "" no-label
    ed_1x40
         at row 9.30 col 2.80
         help "" no-label
    bt_get_file
         at row 09.50 col 31.00 font ?
         help "Pesquisa Arquivo"
    bt_set_printer
         at row 09.50 col 31.00 font ?
         help "Define Impressora e Layout de ImpressÆo"

    " Dimensäes " view-as text
         at row 11.20 col 4.00
    rt_dimensions
         at row 11.50 col 02.00
    v_qtd_line
         at row 12.20 col 9.00 colon-aligned
         view-as fill-in
         fgcolor ? bgcolor 15 font 2
    v_qtd_column
         at row 13.20 col 9.00 colon-aligned
         view-as fill-in
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
           ed_1x40:width-chars              in frame f_tela = 31.00
           ed_1x40:height-chars             in frame f_tela = 01.00
           ls_order:width-chars             in frame f_tela = 25.00
           ls_order:height-chars            in frame f_tela = 05.00
           rt_cxcf:width-chars              in frame f_tela = 86.50
           rt_cxcf:height-chars             in frame f_tela = 01.40
           rt_dimensions:width-chars        in frame f_tela = 33.50
           rt_dimensions:height-chars       in frame f_tela = 03.00
           rt_order:width-chars             in frame f_tela = 33.50
           rt_order:height-chars            in frame f_tela = 06.00
           rt_select:width-chars            in frame f_tela = 51.50
           rt_select:height-chars           in frame f_tela = 13.20
           rt_target:width-chars            in frame f_tela = 33.50
           rt_target:height-chars           in frame f_tela = 03.00.
    /* set return-inserted = yes for editors */
    assign ed_1x40:return-inserted in frame f_tela = yes.
    /* set private-data for the help system */
    assign ls_order:private-data          in frame f_tela = "HLP=000006914":U
           bt_up:private-data             in frame f_tela = "HLP=000009438":U
           bt_down:private-data           in frame f_tela = "HLP=000009436":U
           rs_cod_dwb_output:private-data in frame f_tela = "HLP=000006914":U
           ed_1x40:private-data           in frame f_tela = "HLP=000006914":U
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

/* tech38629 - Altera‡Æo efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
def var v_prog_filtro_pdf as handle no-undo.

function getCodTipoRelat returns character in v_prog_filtro_pdf.

run prgtec/btb/btb920aa.py persistent set v_prog_filtro_pdf.

run pi_define_objetos in v_prog_filtro_pdf (frame f_tela:handle,
                       rs_cod_dwb_output:handle in frame f_tela,
                       bt_get_file:row in frame f_tela,
                       bt_get_file:col in frame f_tela).

&endif
/* tech38629 - Fim da altera‡Æo */

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
        label "Classifica‡Æo"
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

    create tt_param.
    ASSIGN tt_param.c_empresa_orig_ini = INPUT FRAME f_tela v_empresa
           tt_param.c_empresa_orig_fim = INPUT FRAME f_tela v_empresa_fim
           tt_param.c_empresa_Dest     = INPUT FRAME f_tela v_empresa_dest
           tt_param.i_lote_orig_ini   = INPUT FRAME f_tela v_lote_contabil_orig_ini
           tt_param.i_lote_orig_fim   = INPUT FRAME f_tela v_lote_contabil_orig_fim
           tt_param.i_lote_dest_ini   = INPUT FRAME f_tela v_lote_contabil_dest_ini
           tt_param.i_lote_dest_fim   = INPUT FRAME f_tela v_lote_contabil_dest_fim
           tt_param.dt_lote_orig_ini  = INPUT FRAME f_tela v_dat_lote_orig_ini
           tt_param.dt_lote_orig_fim  = INPUT FRAME f_tela v_dat_lote_orig_fim
           tt_param.dt_lote_dest_ini  = INPUT FRAME f_tela v_dat_lote_dest_ini
           tt_param.dt_lote_dest_fim  = INPUT FRAME f_tela v_dat_lote_dest_fim
           tt_param.dt_lanca_orig_ini = INPUT FRAME f_tela v_dat_lancamento_ini
           tt_param.dt_lanca_orig_fim = INPUT FRAME f_tela v_dat_lancamento_fim
           tt_param.c_cenario_ini     = INPUT FRAME f_tela v_cenario_ini
           tt_param.c_cenario_fim     = INPUT FRAME f_tela v_cenario_fim
           tt_param.c_modulo_ini      = INPUT FRAME f_tela v_modulo_ini
           tt_param.c_modulo_fim      = INPUT FRAME f_tela v_modulo_fim
           tT_param.valida_orig       = (if v_valida_origem:checked in frame f_tela  then yes else no)
           tt_param.class-1           = ENTRY(1,ls_order:LIST-ITEMS,",")
           tt_param.class-2           = ENTRY(2,ls_order:LIST-ITEMS,",")
           tt_param.class-3           = ENTRY(3,ls_order:LIST-ITEMS,",")
           tt_param.rs_output         = INPUT FRAME f_tela rs_cod_dwb_output
           tt_param.c_arquivo         = INPUT FRAME f_tela ed_1x40.

    run esp/esfgl073rp.p (input table tt_param).

    return no-apply.
END. /* ON CHOOSE OF bt_print IN FRAME f_tela */

ON CHOOSE OF bt_zoom_empresa IN FRAME f_tela
DO:
    RUN prgint/utb/utb069ka.p.

    find first empresa no-lock
         where recid(empresa) = v_rec_empresa no-error.
    IF  AVAIL empresa THEN DO:
        ASSIGN v_empresa:screen-value in frame f_tela = empresa.cod_empresa.
    END.
END. /* ON CHOOSE OF bt_zoom_empresa IN FRAME f_tela */

ON CHOOSE OF bt_zoom_empresa_fim IN FRAME f_tela
DO:
    RUN prgint/utb/utb069ka.p.

    find first empresa no-lock
         where recid(empresa) = v_rec_empresa no-error.
    IF  AVAIL empresa THEN DO:
        ASSIGN v_empresa_fim:screen-value in frame f_tela = empresa.cod_empresa.
    END.
END. /* ON CHOOSE OF bt_zoom_empresa IN FRAME f_tela */

ON CHOOSE OF bt_zoom_cenario_ini IN FRAME f_tela
DO:
    run prgint/utb/utb076ka.p.

    assign v_cenario_ini:screen-value in frame f_tela = v_cod_cenario.
END. /* ON CHOOSE OF bt_zoom_cenario_ini IN FRAME f_tela */

ON CHOOSE OF bt_zoom_cenario_fim IN FRAME f_tela
DO:
    run prgint/utb/utb076ka.p.

    assign v_cenario_fim:screen-value in frame f_tela = v_cod_cenario.
END. /* ON CHOOSE OF bt_zoom_cenario_fim IN FRAME f_tela */

ON CHOOSE OF bt_zoom_modulo_ini IN FRAME f_tela
DO:
    run prgtec/men/men004ka.p.

    assign v_modulo_ini:screen-value in frame f_tela = string(v_cod_modulo).
END. /* ON CHOOSE OF bt_zoom_modulo_ini IN FRAME f_tela */

ON CHOOSE OF bt_zoom_modulo_fim IN FRAME f_tela
DO:
    run prgtec/men/men004ka.p.

    assign v_modulo_fim:screen-value in frame f_tela = string(v_cod_modulo).
END. /* ON CHOOSE OF bt_zoom_modulo_fim IN FRAME f_tela */

ON CHOOSE OF bt_zoom_lote_contabil_orig_ini IN FRAME f_tela
DO:
    run prgfin/fgl/fgl702za.p.

    ASSIGN v_lote_contabil_orig_ini:SCREEN-VALUE IN FRAME f_tela = string(v_cod_lote_contabil).
END. /* ON CHOOSE OF bt_zoom_lote_contabil_orig_ini IN FRAME f_tela */

ON CHOOSE OF bt_zoom_lote_contabil_orig_fim IN FRAME f_tela
DO:
    run prgfin/fgl/fgl702za.p.

    ASSIGN v_lote_contabil_orig_fim:SCREEN-VALUE IN FRAME f_tela = string(v_cod_lote_contabil).
END. /* ON CHOOSE OF bt_zoom_lote_contabil_orig_fim IN FRAME f_tela */

ON CHOOSE OF bt_zoom_lote_contabil_dest_ini IN FRAME f_tela
DO:
    RUN prgfin/fgl/fgl702za.p.

    ASSIGN v_lote_contabil_dest_ini:SCREEN-VALUE IN FRAME f_tela = string(v_cod_lote_contabil).
END. /* ON CHOOSE OF bt_zoom_lote_contabil_dest_ini IN FRAME f_tela */

ON CHOOSE OF bt_zoom_lote_contabil_dest_fim IN FRAME f_tela
DO:
    RUN prgfin/fgl/fgl702za.p.

    ASSIGN v_lote_contabil_dest_fim:SCREEN-VALUE IN FRAME f_tela = string(v_cod_lote_contabil).
END. /* ON CHOOSE OF bt_zoom_lote_contabil_dest_fim IN FRAME f_tela */

ON CHOOSE OF bt_set_printer IN FRAME f_tela
DO:
    assign v_nom_dwb_printer      = ""
           v_cod_dwb_print_layout = "".

    &if '{&emsbas_version}' <= '1.00' &then
    if  search("prgtec/btb/btb036nb.r") = ? and search("prgtec/btb/btb036nb.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa executavel nÆo foi encontrado: prgtec/btb/btb036nb.p".
        else do:
            message "Programa executavel nÆo foi encontrado: prgtec/btb/btb036nb.p"
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
            return "Programa executÿvel nÆo foi encontrado: prgtec/btb/btb036zb.p".
        else do:
            message "Programa executÿvel nÆo foi encontrado: prgtec/btb/btb036zb.p"
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
        label "Classifica‡Æo"
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

    block:
    do with frame f_tela:
        if  rs_cod_dwb_output:screen-value = "Arquivo" /*l_file*/
        then do:
            assign dwb_rpt_param.cod_dwb_file = ed_1x40:screen-value.
        end /* if */.
    end /* do block */.
END. /* ON LEAVE OF ed_1x40 IN FRAME f_tela */

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
                                                                                     caps("esfgl073":U) + '.rpt'
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

    assign v_nom_prog = v_nom_prog + chr(10) + "esfgl073":U.

    assign v_nom_prog_ext = "esp/esfgl073.p":U
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
    run pi_version_extract ('esfgl073':U, 'esp/esfgl073.p':U, '1.00.00.000':U, 'pro':U).

/* End_Include: i_version_extract */

run pi_return_user (output v_cod_dwb_user) /*pi_return_user*/.

if  search("prgtec/btb/btb906za.r") = ? and search("prgtec/btb/btb906za.py") = ? then do:
    if  v_cod_dwb_user begins 'es_' then
        return "Programa execut vel nÆo foi encontrado: prgtec/btb/btb906za.py".
    else do:
        message "Programa execut vel nÆo foi encontrado: prgtec/btb/btb906za.py"
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
        return "Programa execut vel nÆo foi encontrado: prgtec/men/men901za.py".
    else do:
        message "Programa execut vel nÆo foi encontrado: prgtec/men/men901za.py"
               view-as alert-box error buttons ok.
        return.
    end.
end.
else
    run prgtec/men/men901za.py (Input 'esfgl073') /*prg_fnc_verify_security*/.
if  return-value = "2014"
then do:
    /* Programa a ser executado nÆo ‚ um programa v lido Datasul ! */
    run pi_messages (input "show", input 2014,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'esfgl073')) /*msg_2014*/.
    return.
end.
if  return-value = "2012"
then do:
    /* Usuÿrio sem permissÆo para acessar o programa. */
    run pi_messages (input "show", input 2012,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'esfgl073')) /*msg_2012*/.
    return.
end.
/* End_Include: i_verify_security */

/* Begin_Include: i_log_exec_prog_dtsul_ini */
assign v_rec_log = ?.

if can-find(prog_dtsul
       where prog_dtsul.cod_prog_dtsul = 'esfgl073'
         and prog_dtsul.log_gera_log_exec = yes) then do trans:
    create log_exec_prog_dtsul.
    assign log_exec_prog_dtsul.cod_prog_dtsul           = 'esfgl073'
           log_exec_prog_dtsul.cod_usuario              = v_cod_usuar_corren
           log_exec_prog_dtsul.dat_inic_exec_prog_dtsul = today
           log_exec_prog_dtsul.hra_inic_exec_prog_dtsul = replace(string(time,"hh:mm:ss"),":":U,"":U).
    assign v_rec_log = recid(log_exec_prog_dtsul).
    release log_exec_prog_dtsul no-error.
end.

/* End_Include: i_log_exec_prog_dtsul_ini */

/* tech38629 - Altera‡Æo efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
run pi_permissoes in v_prog_filtro_pdf (input 'esfgl073':U).
&endif
/* tech38629 - Fim da altera‡Æo */

/* redefini‡äes do frame */

/* Begin_Include: i_std_dialog_box */
/* tratamento do titulo e versÆo */
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

/* inicializa variÿveis */
run pi_initialize_reports /*pi_initialize_reports*/.

if  v_cod_dwb_user begins 'es_'
then do:
    find dwb_rpt_param no-lock
         where dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
           and dwb_rpt_param.cod_dwb_user = v_cod_dwb_user /*cl_dwb_rpt_param of dwb_rpt_param*/ no-error.
    if  not avail dwb_rpt_param
    then do:
        return "Parƒmetros para o relat¢rio nÆo encontrado." /*1993*/ + " (" + "1993" + ")" + chr(10) +
               "NÆo foi poss¡vel encontrar os parƒmetros necessÿrios para a impressÆo do relat¢rio para o programa e usuÿrio corrente." /*1993*/.
    end /* if */.

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
    if  ped_exec.cod_release_prog_dtsul <> trim(" 1.00.00.000":U)
    then do:
        return "Versäes do programa diferente." /*1994*/ + " (" + "1994" + ")" + chr(10)
                                     + substitute("A versÆo do programa (&3) que gerou o pedido de execu‡Æo batch (&1) ? diferente da versÆo do programa que deveria executar o pedido batch (&2)." /*1994*/,
                                                  ped_exec.cod_release_prog_dtsul,
                                                  trim(" 1.00.00.000":U),
                                                  "esp/esfgl073.p":U).
    end /* if */.
    assign v_nom_prog_ext     = caps("esfgl073":U)
           v_dat_execution    = today
           v_hra_execution    = replace(string(time, "hh:mm:ss" /*l_hh:mm:ss*/ ), ":", "")
           v_cod_dwb_file     = dwb_rpt_param.cod_dwb_file
           v_nom_report_title = fill(" ", 40 - length(v_rpt_s_1_name)) + v_rpt_s_1_name
           v_ind_dwb_run_mode = "Batch" /*l_batch*/
           v_cod_dwb_order    = dwb_rpt_param.cod_dwb_order.

    /* configura e define destino de impressÆo */
    if  dwb_rpt_param.cod_dwb_output = "Impressora" /*l_printer*/
    then do:
         assign v_qtd_line_ant = v_qtd_line.
    end /* if */.
    run pi_output_reports /*pi_output_reports*/.

    if  dwb_rpt_param.log_dwb_print_parameters = yes
    then do:
        run pi_print_parameters /*pi_print_parameters*/.
    end /* if */.

    output stream s_1 close.

/* tech38629 - Altera‡Æo efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
run pi_call_convert_object in v_prog_filtro_pdf (input yes,
                                                 input dwb_rpt_param.cod_dwb_output,
                                                 input dwb_rpt_param.nom_dwb_print_file,
                                                 input v_cod_dwb_file,
                                                 input v_nom_report_title).
&endif
/* tech38629 - Fim da altera‡Æo */

&if '{&emsbas_version}':U >= '5.05':U &then
    if (dwb_rpt_param.cod_dwb_output = 'Impressora' and getCodTipoRelat() = 'PDF':U) then do:
        if dwb_rpt_param.nom_dwb_print_file = '' then
            run pi_print_pdf_file in v_prog_filtro_pdf (input yes).
    end.
&endif
    return "OK" /*l_ok*/ .

end /* if */.

pause 0 before-hide.
view frame f_tela.

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
           v_empresa
           v_empresa_fim
           v_empresa_dest
           v_lote_contabil_orig_ini
           v_lote_contabil_orig_fim
           v_lote_contabil_dest_ini
           v_lote_contabil_dest_fim
           v_dat_lote_orig_ini
           v_dat_lote_orig_fim
           v_dat_lancamento_ini
           v_dat_lancamento_fim
           v_cenario_ini
           v_cenario_fim
           v_modulo_ini
           v_modulo_fim
           v_valida_origem
           /*v_ind_dwb_run_mode*/
           bt_zoom_empresa
           bt_zoom_lote_contabil_orig_ini
           bt_zoom_lote_contabil_orig_fim
           bt_zoom_lote_contabil_dest_ini
           bt_zoom_lote_contabil_dest_fim
           bt_zoom_cenario_ini
           bt_zoom_cenario_fim
           bt_zoom_modulo_ini
           bt_zoom_modulo_fim
        with frame f_tela.

    bt_zoom_cenario_fim:MOVE-TO-TOP().

    ASSIGN v_empresa_fim:SCREEN-VALUE IN FRAME f_tela = 'ZZZ'
           v_empresa_dest:SCREEN-VALUE IN FRAME f_tela = 'CAN'
           v_lote_contabil_orig_fim:SCREEN-VALUE IN FRAME f_tela = "999999999"
           v_lote_contabil_dest_fim:SCREEN-VALUE IN FRAME f_tela = "999999999"
           v_dat_lote_orig_ini:SCREEN-VALUE IN FRAME f_tela      = "01/01/1800"
           v_dat_lote_orig_fim:SCREEN-VALUE IN FRAME f_tela      = string(today)
           v_dat_lote_dest_ini:SCREEN-VALUE IN FRAME f_tela      = "01/01/1800"
           v_dat_lote_dest_fim:SCREEN-VALUE IN FRAME f_tela      = string(today)
           v_dat_lancamento_ini:SCREEN-VALUE IN FRAME f_tela     = "01/01/1800"
           v_dat_lancamento_fim:SCREEN-VALUE IN FRAME f_tela     = string(today)
           v_cenario_fim:SCREEN-VALUE IN FRAME f_tela            = "ZZZZZZZZZ"
           v_modulo_fim:SCREEN-VALUE IN FRAME f_tela             = "ZZZ"
           v_valida_origem:checked in frame f_tela               = no.

    if  num-entries(v_cod_dwb_order) < 2 then do:
        disable bt_down
                bt_up
                with frame f_tela.
    end.

/* tech38629 - Altera‡Æo efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
    run pi_posiciona_dwb_rpt_param in v_prog_filtro_pdf (input rowid(dwb_rpt_param)).
    run pi_load_params in v_prog_filtro_pdf.
&endif
/* tech38629 - Fim da alter‡Æo */

    apply "value-changed" to rs_cod_dwb_output in frame f_tela.

    run pi_open_dwb_rpt_select /*pi_open_dwb_rpt_select*/.

    block1:
    repeat on error undo block1, retry block1:

        main_block:
        repeat on error undo super_block, leave super_block
                            on endkey undo super_block, leave super_block
                            on stop undo super_block, retry super_block
                            with frame f_tela:

            if  retry then
                output stream s_1 close.

            assign v_log_print = no.
            if  valid-handle( v_wgh_focus ) then
                wait-for go of frame f_tela focus v_wgh_focus.
            else
                wait-for go of frame f_tela.

            assign dwb_rpt_param.cod_dwb_order = ls_order:list-items
                   v_cod_dwb_order             = ls_order:list-items
                   input frame f_tela v_qtd_line.

            if  v_log_print = yes
           then do:

                if  dwb_rpt_param.ind_dwb_run_mode = "Batch" /*l_batch*/
                then do:
                    assign v_cod_dwb_file = replace(dwb_rpt_param.cod_dwb_file, "~\", "~/")
                           v_nom_integer = v_cod_dwb_file.

                   if dwb_rpt_param.cod_dwb_output = "Arquivo" /*l_file*/  then do:
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
                               or   length(entry(2, v_cod_dwb_file, ".")) > 3
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
                    end.

/* tech38629 - Altera‡Æo efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
                    run pi_filename_batch in v_prog_filtro_pdf.
&endif
/* tech38629 - Fim da altera‡Æo */

                    assign v_cod_dwb_file = v_nom_integer.

                    if  search("prgtec/btb/btb911za.r") = ? and search("prgtec/btb/btb911za.p") = ? then do:
                        if  v_cod_dwb_user begins 'es_' then
                            return "Programa executÿvel nÆo foi encontrado:" /*l_programa_nao_encontrado*/  + "prgtec/btb/btb911za.p".
                        else do:
                            message "Programa execut vel nÆo foi encontrado:" /*l_programa_nao_encontrado*/  "prgtec/btb/btb911za.p"
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

                    if  v_num_ped_exec <> 0
                    then do:
                        leave main_block.
                    end /* if */.
                    else do:
                        next main_block.
                    end /* else */.

                end /* if */.
                else do:
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
&endif
                     do:

                        assign v_cod_dwb_file   = session:temp-directory + "esfgl072":U + '.tmp'
                               v_rpt_s_1_bottom = v_qtd_line - (v_rpt_s_1_lines - v_qtd_bottom).
                        output stream s_1 to value(v_cod_dwb_file) paged page-size value(v_qtd_line) convert target 'iso8859-1'.
                    end /* do out_term */.

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
                                       page-size 0 convert target tip_imprsor.cod_pag_carac_conver.
                            else
                                output stream s_1 to value(lc(v_nom_dwb_print_file))
                                       paged page-size value(layout_impres.num_lin_pag) convert target  tip_imprsor.cod_pag_carac_conver.
                        else
&endif
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
                                         /* ConversÆo interna do OUTPUT TARGET */
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
/* tech38629 - Altera‡Æo efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
run pi_rename_file in v_prog_filtro_pdf (input-output v_cod_dwb_file).
&endif
/* tech38629 - Fim da altera‡Æo */

                        output stream s_1 to value(v_cod_dwb_file)
                               paged page-size value(v_qtd_line) convert target 'iso8859-1'.
                    end /* do out_file */.
&if '{&emsbas_version}':U >= '5.05':U &then
/*                    end /* case out_def */.*/
&else
                end /* case out_def */.
&endif
                assign v_nom_prog_ext = caps(substr("esfgl073.p",12,8))
                       v_dat_execution = today
                       v_hra_execution = replace(string(time,"hh:mm:ss" ),":","").

                    run pi_esfgl073.
                end /* else */.

                if  dwb_rpt_param.log_dwb_print_parameters then
                    run pi_print_parameters /*pi_print_parameters*/.

                output stream s_1 close.

/* tech38629 - Altera‡Æo efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
run pi_call_convert_object in v_prog_filtro_pdf (input no,
                                                 input rs_cod_dwb_output:screen-value in frame f_tela,
                                                 input v_nom_dwb_print_file,
                                                 input v_cod_dwb_file,
                                                 input v_nom_report_title).
&endif
/* tech38629 - Fim da altera‡Æo */

&if '{&emsbas_version}':U >= '5.05':U &then
    if (dwb_rpt_param.cod_dwb_output = 'Impressora' and getCodTipoRelat() = 'PDF':U) then do:
        if  v_nom_dwb_print_file = '' then
            run pi_print_pdf_file in v_prog_filtro_pdf (input no).
    end.
&endif
                assign v_log_method = session:set-wait-state("").
                if  dwb_rpt_param.cod_dwb_output = "Terminal" then do:
                /* tech38629 - Altera‡Æo efetuada via filtro */
                &if '{&emsbas_version}':U >= '5.05':U &then
                    if  getCodTipoRelat() = 'PDF':U and OPSYS = 'WIN32':U then do:
                        run pi_open_pdf_file in v_prog_filtro_pdf.
                    end.
                    else if getCodTipoRelat() = 'Texto' then do:
                &endif
                /* tech38629 - Fim da altera‡Æo */
                    run pi_show_report_2 (Input v_cod_dwb_file) /*pi_show_report_2*/.
                /* tech38629 - Altera‡Æo efetuada via filtro */
                &if '{&emsbas_version}':U >= '5.05':U &then
                    end.
                &endif
                /* tech38629 - Fim da altera‡Æo */
                end.
                leave main_block.
            end.
            else
                leave super_block.
        end /* repeat main_block */.

        if  v_num_ped_exec <> 0 then do:
            /* Criado pedido &1 para execu‡Æo batch. */
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
** Procedure Interna.....: pi_open_dwb_rpt_select
** Descricao.............: pi_open_dwb_rpt_select
*****************************************************************************/
PROCEDURE pi_open_dwb_rpt_select:
    open query qr_dwb_rpt_select for
        each dwb_rpt_select no-lock
        where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
          and dwb_rpt_select.cod_dwb_user = v_cod_dwb_user /*cl_dwb_rpt_select of dwb_rpt_select*/
            by dwb_rpt_select.log_dwb_rule descending
            by dwb_rpt_select.num_dwb_order.
END PROCEDURE. /* pi_open_dwb_rpt_select */
/*****************************************************************************
** Procedure Interna.....: pi_isl_dwb_rpt_select
** Descricao.............: pi_isl_dwb_rpt_select
*****************************************************************************/
PROCEDURE pi_isl_dwb_rpt_select:
    /************************* Variable Definition Begin ************************/

    def var v_num_dwb_order
        as integer
        format ">>>>,>>9":U
        no-undo.
    def var v_wgh_fill_in_fim
        as widget-handle
        format ">>>>>>9":U
        no-undo.
    def var v_wgh_fill_in_ini
        as widget-handle
        format ">>>>>>9":U
        no-undo.
    def var v_wgh_label_fim
        as widget-handle
        format ">>>>>>9":U
        no-undo.
    def var v_wgh_label_ini
        as widget-handle
        format ">>>>>>9":U
        no-undo.
    def var v_log_ok                         as logical         no-undo. /*local*/

    /************************** Variable Definition End *************************/

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

    /************************* Rectangle Definition End *************************/

    /************************** Button Definition Begin *************************/

    def button bt_hel2
        label "Ajuda"
        tooltip "Ajuda"
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

    def frame f_dlg_04_dwb_rpt_select
        rt_001
             at row 01.25 col 02.00
        rt_002
             at row 02.75 col 03.00
        " Conjunto " view-as text
             at row 02.45 col 05.00
        rt_cxcf
             at row 08.17 col 02.00 bgcolor 7
        dwb_rpt_select.log_dwb_rule
             at row 01.50 col 03.14 no-label
             view-as radio-set Horizontal
             radio-buttons "Regra", yes,"Exce‡Æo", no
              /*l_rule*/ /*l_yes*/ /*l_exception*/ /*l_no*/
             bgcolor 8
        dwb_rpt_select.cod_dwb_field
             at row 03.21 col 04.29 no-label
             view-as combo-box
             list-items "!"
              /*l_!*/
             inner-lines 5
             bgcolor 15 font 2
        bt_ok
             at row 08.38 col 03.00 font ?
             help "OK"
        bt_sav
             at row 08.38 col 14.00 font ?
             help "Salva"
        bt_hel2
             at row 08.38 col 51.13 font ?
             help "Ajuda"
        with 1 down side-labels no-validate keep-tab-order three-d
             size-char 63.57 by 10.00 default-button bt_sav
             view-as dialog-box
             font 1 fgcolor ? bgcolor 8
             title "Conjunto de Sele‡Æo".
        /* adjust size of objects in this frame */
        assign bt_hel2:width-chars  in frame f_dlg_04_dwb_rpt_select = 10.00
               bt_hel2:height-chars in frame f_dlg_04_dwb_rpt_select = 01.00
               bt_ok:width-chars    in frame f_dlg_04_dwb_rpt_select = 10.00
               bt_ok:height-chars   in frame f_dlg_04_dwb_rpt_select = 01.00
               bt_sav:width-chars   in frame f_dlg_04_dwb_rpt_select = 10.00
               bt_sav:height-chars  in frame f_dlg_04_dwb_rpt_select = 01.00
               rt_001:width-chars   in frame f_dlg_04_dwb_rpt_select = 60.00
               rt_001:height-chars  in frame f_dlg_04_dwb_rpt_select = 06.75
               rt_002:width-chars   in frame f_dlg_04_dwb_rpt_select = 58.00
               rt_002:height-chars  in frame f_dlg_04_dwb_rpt_select = 05.00
               rt_cxcf:width-chars  in frame f_dlg_04_dwb_rpt_select = 60.13
               rt_cxcf:height-chars in frame f_dlg_04_dwb_rpt_select = 01.42.
        /* set private-data for the help system */
        assign dwb_rpt_select.log_dwb_rule:private-data  in frame f_dlg_04_dwb_rpt_select = "HLP=000000000":U
               dwb_rpt_select.cod_dwb_field:private-data in frame f_dlg_04_dwb_rpt_select = "HLP=000000000":U
               bt_ok:private-data                        in frame f_dlg_04_dwb_rpt_select = "HLP=000010721":U
               bt_sav:private-data                       in frame f_dlg_04_dwb_rpt_select = "HLP=000011048":U
               bt_hel2:private-data                      in frame f_dlg_04_dwb_rpt_select = "HLP=000011326":U
               frame f_dlg_04_dwb_rpt_select:private-data                                 = "HLP=000000000".

{include/i_fclfrm.i f_dlg_04_dwb_rpt_select }
    /*************************** Frame Definition End ***************************/

    /*********************** User Interface Trigger Begin ***********************/

    ON CHOOSE OF bt_hel2 IN FRAME f_dlg_04_dwb_rpt_select
    DO:

        /* Begin_Include: i_context_help_frame */
        run prgtec/men/men900za.py (Input self:frame,
                                    Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.

        /* End_Include: i_context_help_frame */

    END. /* ON CHOOSE OF bt_hel2 IN FRAME f_dlg_04_dwb_rpt_select */

    ON CHOOSE OF bt_ok IN FRAME f_dlg_04_dwb_rpt_select
    DO:

        assign v_log_ok = yes.
    END. /* ON CHOOSE OF bt_ok IN FRAME f_dlg_04_dwb_rpt_select */

    ON VALUE-CHANGED OF dwb_rpt_select.cod_dwb_field IN FRAME f_dlg_04_dwb_rpt_select
    DO:

        assign v_cod_dwb_field = self:screen-value in frame f_dlg_04_dwb_rpt_select.

        run pi_isl_esfgl073.

        if  v_wgh_label_ini = ?
        then do:
            create text v_wgh_label_ini
                assign frame        = frame f_dlg_04_dwb_rpt_select:handle
                    screen-value = "Inicial:" /*l_Inicial:*/
                    visible      = no
                    row          = 5
                    col          = 12
                    width        = 7.
{include/i_fcldin.i v_wgh_label_ini }
        end /* if */.

        if  v_wgh_label_fim = ?
        then do:
            create text v_wgh_label_fim
                assign frame        = frame f_dlg_04_dwb_rpt_select:handle
                    screen-value = "  Final:" /*l_bbfinal:*/
                    visible      = no
                    row          = 6
                    col          = 12
                    width        = 7.
{include/i_fcldin.i v_wgh_label_fim }
        end /* if */.

        if  v_wgh_fill_in_ini <> ?
        then do:
            delete widget v_wgh_fill_in_ini.
        end /* if */.

        create fill-in v_wgh_fill_in_ini
            assign frame              = frame f_dlg_04_dwb_rpt_select:handle
                   font               = 2
                   data-type          = v_cod_dat_type
                   format             = v_cod_format
                   side-label-handle  = v_wgh_label_ini:handle
                   row                = 5
                   column             = 19
                   height             = 0.88
                   bgcolor            = 15
                   visible            = yes
                   sensitive          = yes
                   screen-value       = v_cod_initial.
{include/i_fcldin.i v_wgh_fill_in_ini }

        if  v_wgh_fill_in_fim <> ?
        then do:
            delete widget v_wgh_fill_in_fim.
        end /* if */.

        create fill-in v_wgh_fill_in_fim
            assign frame              = frame f_dlg_04_dwb_rpt_select:handle
                   font               = 2
                   data-type          = v_cod_dat_type
                   format             = v_cod_format
                   side-label-handle  = v_wgh_label_fim:handle
                   row                = 6
                   col                = 19
                   height             = 0.88
                   bgcolor            = 15
                   visible            = yes
                   sensitive          = yes
                   screen-value       = v_cod_final.
{include/i_fcldin.i v_wgh_fill_in_fim }

    END. /* ON VALUE-CHANGED OF dwb_rpt_select.cod_dwb_field IN FRAME f_dlg_04_dwb_rpt_select */

    ON VALUE-CHANGED OF dwb_rpt_select.log_dwb_rule IN FRAME f_dlg_04_dwb_rpt_select
    DO:

        /************************** Buffer Definition Begin *************************/

        &if "{&emsbas_version}" >= "1.00" &then
        def buffer b_dwb_rpt_select
            for dwb_rpt_select.
        &endif

        /*************************** Buffer Definition End **************************/

        find last b_dwb_rpt_select
            where b_dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
            and   b_dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
            and   b_dwb_rpt_select.log_dwb_rule    = (dwb_rpt_select.log_dwb_rule:screen-value = 'yes')
            no-lock no-error.
        if  not available b_dwb_rpt_select
        then do:
            assign v_num_dwb_order = (if dwb_rpt_select.log_dwb_rule:screen-value = 'yes' then 10 else 500).
        end /* if */.
        else do:
            assign v_num_dwb_order = b_dwb_rpt_select.num_dwb_order + 10.
        end /* else */.
    END. /* ON VALUE-CHANGED OF dwb_rpt_select.log_dwb_rule IN FRAME f_dlg_04_dwb_rpt_select */

    /************************ User Interface Trigger End ************************/

    /**************************** Frame Trigger Begin ***************************/

    ON GO OF FRAME f_dlg_04_dwb_rpt_select
    DO:

        if  (v_wgh_fill_in_ini:data-type = 'character' and v_wgh_fill_in_ini:screen-value > v_wgh_fill_in_fim:screen-value) or
             (v_wgh_fill_in_ini:data-type = 'date'      and date(v_wgh_fill_in_ini:screen-value) > date(v_wgh_fill_in_fim:screen-value)) or
             (v_wgh_fill_in_ini:data-type = "integer" /*l_integer*/    and integer(v_wgh_fill_in_ini:screen-value) > integer(v_wgh_fill_in_fim:screen-value)) or
             (v_wgh_fill_in_ini:data-type = 'Decimal'   and decimal(v_wgh_fill_in_ini:screen-value) > decimal(v_wgh_fill_in_fim:screen-value))
        then do:
            /* Argumento Inicial maior que o Final ! */
            run pi_messages (input "show",
                             input 1085,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_1085*/.
            return no-apply.
        end /* if */.

    END. /* ON GO OF FRAME f_dlg_04_dwb_rpt_select */

    ON HELP OF FRAME f_dlg_04_dwb_rpt_select ANYWHERE
    DO:

        /* Begin_Include: i_context_help */
        run prgtec/men/men900za.py (Input self:handle,
                                    Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
        /* End_Include: i_context_help */

    END. /* ON HELP OF FRAME f_dlg_04_dwb_rpt_select */

    ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_04_dwb_rpt_select ANYWHERE
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
    END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_04_dwb_rpt_select */

    ON RIGHT-MOUSE-UP OF FRAME f_dlg_04_dwb_rpt_select ANYWHERE
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
    END. /* ON RIGHT-MOUSE-UP OF FRAME f_dlg_04_dwb_rpt_select */

    /***************************** Frame Trigger End ****************************/

    pause 0 before-hide.
    view frame f_dlg_04_dwb_rpt_select.

    assign v_log_ok = no
           frame f_dlg_04_dwb_rpt_select:title = "Inclui" /*l_inclui*/  + " Conjunto de Sele‡Æo" /*l_conjunto_selecao*/ .

    main_block:
    repeat while v_log_ok = no
        on endkey undo main_block, leave main_block
        on error undo main_block, leave main_block:

        find last dwb_rpt_select no-lock
             where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
               and dwb_rpt_select.cod_dwb_user = v_cod_dwb_user /*cl_dwb_rpt_select of dwb_rpt_select*/ no-error.
        if  not available dwb_rpt_select
        then do:
            assign v_num_dwb_order = 10.
        end /* if */.
        else do:
            assign v_num_dwb_order = dwb_rpt_select.num_dwb_order + 10.
        end /* else */.

        create dwb_rpt_select.
        assign dwb_rpt_select.log_dwb_rule = yes.

        assign dwb_rpt_select.cod_dwb_field:list-items in frame f_dlg_04_dwb_rpt_select = v_cod_dwb_select
               dwb_rpt_select.cod_dwb_field:screen-value = entry(1,v_cod_dwb_select).

        display dwb_rpt_select.log_dwb_rule
                with frame f_dlg_04_dwb_rpt_select.
        enable dwb_rpt_select.log_dwb_rule
               dwb_rpt_select.cod_dwb_field
               bt_ok
               bt_sav
               with frame f_dlg_04_dwb_rpt_select.
        apply "value-changed" to dwb_rpt_select.cod_dwb_field in frame f_dlg_04_dwb_rpt_select.
        apply "value-changed" to dwb_rpt_select.log_dwb_rule  in frame f_dlg_04_dwb_rpt_select.

        wait-for go of frame f_dlg_04_dwb_rpt_select.

        assign dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
               dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
               dwb_rpt_select.num_dwb_order   = v_num_dwb_order
               dwb_rpt_select.cod_dwb_initial = v_wgh_fill_in_ini:screen-value
               dwb_rpt_select.cod_dwb_final   = v_wgh_fill_in_fim:screen-value
               dwb_rpt_select.log_dwb_rule
               dwb_rpt_select.cod_dwb_field.

    end /* repeat main_block */.

    hide frame f_dlg_04_dwb_rpt_select.
END PROCEDURE. /* pi_isl_dwb_rpt_select */
/*****************************************************************************
** Procedure Interna.....: pi_edl_dwb_rpt_select
** Descricao.............: pi_edl_dwb_rpt_select
*****************************************************************************/
PROCEDURE pi_edl_dwb_rpt_select:
    /************************ Parameter Definition Begin ************************/

    def Input param p_rec_dwb_rpt_select
        as recid
        format ">>>>>>9"
        no-undo.

    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_num_dwb_order
        as integer
        format ">>>>,>>9":U
        no-undo.
    def var v_wgh_fill_in_fim
        as widget-handle
        format ">>>>>>9":U
        no-undo.
    def var v_wgh_fill_in_ini
        as widget-handle
        format ">>>>>>9":U
        no-undo.
    def var v_wgh_label_fim
        as widget-handle
        format ">>>>>>9":U
        no-undo.
    def var v_wgh_label_ini
        as widget-handle
        format ">>>>>>9":U
        no-undo.
    def var v_log_ok                         as logical         no-undo. /*local*/

    /************************** Variable Definition End *************************/

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

    def frame f_dlg_04_dwb_rpt_select
        rt_001
             at row 01.25 col 02.00
        rt_002
             at row 02.75 col 03.00
        " Conjunto " view-as text
             at row 02.45 col 05.00
        rt_cxcf
             at row 08.17 col 02.00 bgcolor 7
        dwb_rpt_select.log_dwb_rule
             at row 01.50 col 03.14 no-label
             view-as radio-set Horizontal
             radio-buttons "Regra", yes,"Exce‡Æo", no
              /*l_rule*/ /*l_yes*/ /*l_exception*/ /*l_no*/
             bgcolor 8
        dwb_rpt_select.cod_dwb_field
             at row 03.21 col 04.29 no-label
             view-as combo-box
             list-items "!"
              /*l_!*/
             inner-lines 5
             bgcolor 15 font 2
        bt_ok
             at row 08.38 col 03.00 font ?
             help "OK"
        bt_sav
             at row 08.38 col 14.00 font ?
             help "Salva"
        bt_can
             at row 08.38 col 25.00 font ?
             help "Cancela"
        bt_hel2
             at row 08.38 col 51.13 font ?
             help "Ajuda"
        with 1 down side-labels no-validate keep-tab-order three-d
             size-char 63.57 by 10.00 default-button bt_sav
             view-as dialog-box
             font 1 fgcolor ? bgcolor 8
             title "Conjunto de Sele‡Æo".
        /* adjust size of objects in this frame */
        assign bt_can:width-chars   in frame f_dlg_04_dwb_rpt_select = 10.00
               bt_can:height-chars  in frame f_dlg_04_dwb_rpt_select = 01.00
               bt_hel2:width-chars  in frame f_dlg_04_dwb_rpt_select = 10.00
               bt_hel2:height-chars in frame f_dlg_04_dwb_rpt_select = 01.00
               bt_ok:width-chars    in frame f_dlg_04_dwb_rpt_select = 10.00
               bt_ok:height-chars   in frame f_dlg_04_dwb_rpt_select = 01.00
               bt_sav:width-chars   in frame f_dlg_04_dwb_rpt_select = 10.00
               bt_sav:height-chars  in frame f_dlg_04_dwb_rpt_select = 01.00
               rt_001:width-chars   in frame f_dlg_04_dwb_rpt_select = 60.00
               rt_001:height-chars  in frame f_dlg_04_dwb_rpt_select = 06.75
               rt_002:width-chars   in frame f_dlg_04_dwb_rpt_select = 58.00
               rt_002:height-chars  in frame f_dlg_04_dwb_rpt_select = 05.00
               rt_cxcf:width-chars  in frame f_dlg_04_dwb_rpt_select = 60.13
               rt_cxcf:height-chars in frame f_dlg_04_dwb_rpt_select = 01.42.
        /* set private-data for the help system */
        assign dwb_rpt_select.log_dwb_rule:private-data  in frame f_dlg_04_dwb_rpt_select = "HLP=000000000":U
               dwb_rpt_select.cod_dwb_field:private-data in frame f_dlg_04_dwb_rpt_select = "HLP=000000000":U
               bt_ok:private-data                        in frame f_dlg_04_dwb_rpt_select = "HLP=000010721":U
               bt_sav:private-data                       in frame f_dlg_04_dwb_rpt_select = "HLP=000011048":U
               bt_can:private-data                       in frame f_dlg_04_dwb_rpt_select = "HLP=000011050":U
               bt_hel2:private-data                      in frame f_dlg_04_dwb_rpt_select = "HLP=000011326":U
               frame f_dlg_04_dwb_rpt_select:private-data                                 = "HLP=000000000".

{include/i_fclfrm.i f_dlg_04_dwb_rpt_select }
    /*************************** Frame Definition End ***************************/

    /*********************** User Interface Trigger Begin ***********************/

    ON CHOOSE OF bt_hel2 IN FRAME f_dlg_04_dwb_rpt_select
    DO:

        /* Begin_Include: i_context_help_frame */
        run prgtec/men/men900za.py (Input self:frame,
                                    Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.


        /* End_Include: i_context_help_frame */

    END. /* ON CHOOSE OF bt_hel2 IN FRAME f_dlg_04_dwb_rpt_select */

    ON CHOOSE OF bt_ok IN FRAME f_dlg_04_dwb_rpt_select
    DO:

        assign v_log_ok = yes.
    END. /* ON CHOOSE OF bt_ok IN FRAME f_dlg_04_dwb_rpt_select */

    ON VALUE-CHANGED OF dwb_rpt_select.cod_dwb_field IN FRAME f_dlg_04_dwb_rpt_select
    DO:

        assign v_cod_dwb_field = self:screen-value in frame f_dlg_04_dwb_rpt_select.

        run pi_isl_esfgl073.

        if  v_wgh_label_ini = ?
        then do:
            create text v_wgh_label_ini
                assign frame        = frame f_dlg_04_dwb_rpt_select:handle
                    screen-value = "Inicial:" /*l_Inicial:*/
                    visible      = no
                    row          = 5
                    col          = 12
                    width        = 7.
{include/i_fcldin.i v_wgh_label_ini }
        end /* if */.

        if  v_wgh_label_fim = ?
        then do:
            create text v_wgh_label_fim
                assign frame        = frame f_dlg_04_dwb_rpt_select:handle
                    screen-value = "  Final:" /*l_bbfinal:*/
                    visible      = no
                    row          = 6
                    col          = 12
                    width        = 7.
{include/i_fcldin.i v_wgh_label_fim }
        end /* if */.

        if  v_wgh_fill_in_ini <> ?
        then do:
            delete widget v_wgh_fill_in_ini.
        end /* if */.

        create fill-in v_wgh_fill_in_ini
            assign frame              = frame f_dlg_04_dwb_rpt_select:handle
                   font               = 2
                   data-type          = v_cod_dat_type
                   format             = v_cod_format
                   side-label-handle  = v_wgh_label_ini:handle
                   row                = 5
                   column             = 19
                   height             = 0.88
                   bgcolor            = 15
                   visible            = yes
                   sensitive          = yes
                   screen-value       = v_cod_initial.
{include/i_fcldin.i v_wgh_fill_in_ini }

        if  v_wgh_fill_in_fim <> ?
        then do:
            delete widget v_wgh_fill_in_fim.
        end /* if */.

        create fill-in v_wgh_fill_in_fim
            assign frame              = frame f_dlg_04_dwb_rpt_select:handle
                   font               = 2
                   data-type          = v_cod_dat_type
                   format             = v_cod_format
                   side-label-handle  = v_wgh_label_fim:handle
                   row                = 6
                   col                = 19
                   height             = 0.88
                   bgcolor            = 15
                   visible            = yes
                   sensitive          = yes
                   screen-value       = v_cod_final.
{include/i_fcldin.i v_wgh_fill_in_fim }

    END. /* ON VALUE-CHANGED OF dwb_rpt_select.cod_dwb_field IN FRAME f_dlg_04_dwb_rpt_select */

    ON VALUE-CHANGED OF dwb_rpt_select.log_dwb_rule IN FRAME f_dlg_04_dwb_rpt_select
    DO:

        /************************** Buffer Definition Begin *************************/

        &if "{&emsbas_version}" >= "1.00" &then
        def buffer b_dwb_rpt_select
            for dwb_rpt_select.
        &endif

        /*************************** Buffer Definition End **************************/

        find last b_dwb_rpt_select
            where b_dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
            and   b_dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
            and   b_dwb_rpt_select.log_dwb_rule    = (dwb_rpt_select.log_dwb_rule:screen-value = 'yes')
            no-lock no-error.
        if  not available b_dwb_rpt_select
        then do:
            assign v_num_dwb_order = (if dwb_rpt_select.log_dwb_rule:screen-value = 'yes' then 10 else 500).
        end /* if */.
        else do:
            assign v_num_dwb_order = b_dwb_rpt_select.num_dwb_order + 10.
        end /* else */.
    END. /* ON VALUE-CHANGED OF dwb_rpt_select.log_dwb_rule IN FRAME f_dlg_04_dwb_rpt_select */

    /************************ User Interface Trigger End ************************/

    /**************************** Frame Trigger Begin ***************************/

    ON GO OF FRAME f_dlg_04_dwb_rpt_select
    DO:

        if  (v_wgh_fill_in_ini:data-type = 'character' and v_wgh_fill_in_ini:screen-value > v_wgh_fill_in_fim:screen-value) or
             (v_wgh_fill_in_ini:data-type = 'date'      and date(v_wgh_fill_in_ini:screen-value) > date(v_wgh_fill_in_fim:screen-value)) or
             (v_wgh_fill_in_ini:data-type = "integer" /*l_integer*/    and integer(v_wgh_fill_in_ini:screen-value) > integer(v_wgh_fill_in_fim:screen-value)) or
             (v_wgh_fill_in_ini:data-type = 'Decimal'   and decimal(v_wgh_fill_in_ini:screen-value) > decimal(v_wgh_fill_in_fim:screen-value))
        then do:
            /* Argumento Inicial maior que o Final ! */
            run pi_messages (input "show",
                             input 1085,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_1085*/.
            return no-apply.
        end /* if */.

    END. /* ON GO OF FRAME f_dlg_04_dwb_rpt_select */

    ON HELP OF FRAME f_dlg_04_dwb_rpt_select ANYWHERE
    DO:

        /* Begin_Include: i_context_help */
        run prgtec/men/men900za.py (Input self:handle,
                                    Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
        /* End_Include: i_context_help */

    END. /* ON HELP OF FRAME f_dlg_04_dwb_rpt_select */

    ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_04_dwb_rpt_select ANYWHERE
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

    END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_04_dwb_rpt_select */

    ON RIGHT-MOUSE-UP OF FRAME f_dlg_04_dwb_rpt_select ANYWHERE
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

    END. /* ON RIGHT-MOUSE-UP OF FRAME f_dlg_04_dwb_rpt_select */

    /***************************** Frame Trigger End ****************************/

    pause 0 before-hide.
    view frame f_dlg_04_dwb_rpt_select.

    assign v_log_ok = no
           frame f_dlg_04_dwb_rpt_select:title = "Edita" /*l_edita*/  + " Conjunto de Sele‡Æo" /*l_conjunto_selecao*/ .

    main_block:
    repeat while v_log_ok = no
        on endkey undo main_block, leave main_block
        on error undo main_block, leave main_block:

        find dwb_rpt_select where recid(dwb_rpt_select) = p_rec_dwb_rpt_select exclusive-lock.

        assign dwb_rpt_select.cod_dwb_field:list-items in frame f_dlg_04_dwb_rpt_select = v_cod_dwb_select
               v_cod_dwb_field = dwb_rpt_select.cod_dwb_field.

        display dwb_rpt_select.log_dwb_rule
                dwb_rpt_select.cod_dwb_field
                with frame f_dlg_04_dwb_rpt_select.

        run pi_isl_esfgl073.

        create text v_wgh_label_ini
            assign frame        = frame f_dlg_04_dwb_rpt_select:handle
                screen-value = "Inicial:" /*l_Inicial:*/
                visible      = no
                row          = 5
                col          = 12
                width        = 7.
{include/i_fcldin.i v_wgh_label_ini }

        create text v_wgh_label_fim
            assign frame        = frame f_dlg_04_dwb_rpt_select:handle
                screen-value = "  Final:" /*l_bbfinal:*/
                visible      = no
                row          = 6
                col          = 12
                width        = 7.
{include/i_fcldin.i v_wgh_label_fim }

        create fill-in v_wgh_fill_in_ini
            assign frame          = frame f_dlg_04_dwb_rpt_select:handle
               font               = 2
               data-type          = v_cod_dat_type
               format             = v_cod_format
               side-label-handle  = v_wgh_label_ini:handle
               row                = 5
               column             = 19
               height             = 0.88
               bgcolor            = 15
               visible            = yes
               sensitive          = yes
               screen-value       = dwb_rpt_select.cod_dwb_initial.
{include/i_fcldin.i v_wgh_fill_in_ini }

        create fill-in v_wgh_fill_in_fim
            assign frame          = frame f_dlg_04_dwb_rpt_select:handle
               font               = 2
               data-type          = v_cod_dat_type
               format             = v_cod_format
               side-label-handle  = v_wgh_label_fim:handle
               row                = 6
               col                = 19
               height             = 0.88
               bgcolor            = 15
               visible            = yes
               sensitive          = yes
               screen-value       = dwb_rpt_select.cod_dwb_final.
{include/i_fcldin.i v_wgh_fill_in_fim }


        disable dwb_rpt_select.log_dwb_rule
                dwb_rpt_select.cod_dwb_field
                with frame f_dlg_04_dwb_rpt_select.

        enable bt_ok
               bt_can
               with frame f_dlg_04_dwb_rpt_select.

        wait-for go of frame f_dlg_04_dwb_rpt_select.

        assign dwb_rpt_select.cod_dwb_initial = v_wgh_fill_in_ini:screen-value
               dwb_rpt_select.cod_dwb_final   = v_wgh_fill_in_fim:screen-value.

    end /* repeat main_block */.

    hide frame f_dlg_04_dwb_rpt_select.

END PROCEDURE. /* pi_edl_dwb_rpt_select */
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
        "Usuÿrio: " at 1
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
    ler:
    for each dwb_rpt_select no-lock
     where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
       and dwb_rpt_select.cod_dwb_user = v_cod_dwb_user /*cl_dwb_rpt_select of dwb_rpt_select*/:
        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
            page stream s_1.
        put stream s_1 unformatted
            dwb_rpt_select.log_dwb_rule at 1 format "Regra/Exce‡Æo"
            dwb_rpt_select.cod_dwb_field at 9 format "x(32)"
            dwb_rpt_select.cod_dwb_initial at 42 format "x(18)"
            dwb_rpt_select.cod_dwb_final at 61 format "x(18)" skip.
    end /* for ler */.
END PROCEDURE. /* pi_print_parameters */
/*****************************************************************************
** Procedure Interna.....: pi_initialize_reports
** Descricao.............: pi_initialize_reports
*****************************************************************************/
PROCEDURE pi_initialize_reports:

    /* inicializa variÿveis */
    find empresa no-lock
         where empresa.cod_empresa = v_cod_empres_usuar no-error.
    find dwb_rpt_param no-lock
         where dwb_rpt_param.cod_dwb_program = "esfgl073":U
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

    assign v_cod_dwb_proced   = "esfgl073":U
           v_cod_dwb_program  = "esfgl073":U
           v_cod_dwb_order    = "Empresa,Lote Cont bil Origem,Lote Cont bil Destino,Data Lote Origem,Data Lote Destino,Data Lan‡amento"
           v_cod_release      = trim(" 1.00.00.002":U)
           v_cod_dwb_select   = "Empresa,Lote Cont bil Origem,Lote Cont bil Destino,Data Lote Origem,Data Lote Destino,Data Lan‡amento"
           v_ind_dwb_run_mode = "On-Line" /*l_online*/
           v_qtd_column       = v_rpt_s_1_columns
           v_qtd_bottom       = v_rpt_s_1_bottom.
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

/* tech38629 - Altera‡Æo efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
    run pi_posiciona_dwb_rpt_param in v_prog_filtro_pdf (input rowid(dwb_rpt_param)).
    run pi_load_params in v_prog_filtro_pdf.
&endif
/* tech38629 - Fim da altera‡Æo */

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
/* tech38629 - Altera‡Æo efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
run pi_rename_file in v_prog_filtro_pdf (input-output v_cod_dwb_file).
&endif
/* tech38629 - Fim da altera‡Æo */

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
                                  /* ConversÆo interna do OUTPUT TARGET */
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

    run pi_esfgl073.
END PROCEDURE. /* pi_output_reports */

PROCEDURE pi_esfgl073:
    assign  v_cod_order = dwb_rpt_param.cod_dwb_order.

    selecao:
    for
        each dwb_rpt_select no-lock
        where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
          and dwb_rpt_select.cod_dwb_user = v_cod_dwb_user /*cl_dwb_rpt_select of dwb_rpt_select*/:
        if  dwb_rpt_select.cod_dwb_field = "Caracter¡stica"
        then do:
            if  dwb_rpt_select.log_dwb_rule = yes
            then do:
                block:
                for each caract_espec_pat no-lock
                 where caract_espec_pat.cod_caract_pat >= dwb_rpt_select.cod_dwb_initial
                   and caract_espec_pat.cod_caract_pat <= dwb_rpt_select.cod_dwb_final:
                    if  v_ind_dwb_run_mode = "Batch" /*l_batch*/
                    then do:
                        assign v_cod_ult_obj_procesdo = caract_espec_pat.cod_espec_bem + "," + caract_espec_pat.cod_caract_pat.
                        run prgtec/btb/btb908ze.py (Input 1,
                                                    Input v_cod_ult_obj_procesdo) /*prg_api_atualizar_ult_obj*/.
                    end /* if */.
                    run pi_tratar_esfgl073.
                end /* for block */.
            end /* if */.
            else do:
                excessao:
                for each tt_esfgl073
                where tt_esfgl073.cod_caract_pat >= dwb_rpt_select.cod_dwb_initial
                and   tt_esfgl073.cod_caract_pat <= dwb_rpt_select.cod_dwb_final use-index tt_rpt_cod_caract_pat:
                     delete tt_esfgl073.
                end /* for excessao */.
            end /* else */.
        end /* if */.

        if  dwb_rpt_select.cod_dwb_field = "Esp‚cie Bem Patrimonial"
        then do:
            if  dwb_rpt_select.log_dwb_rule = yes
            then do:
                block:
                for each caract_espec_pat no-lock
                 where caract_espec_pat.cod_espec_bem >= dwb_rpt_select.cod_dwb_initial
                   and caract_espec_pat.cod_espec_bem <= dwb_rpt_select.cod_dwb_final:
                    if  v_ind_dwb_run_mode = "Batch" /*l_batch*/
                    then do:
                        assign v_cod_ult_obj_procesdo = caract_espec_pat.cod_espec_bem + "," + caract_espec_pat.cod_caract_pat.
                        run prgtec/btb/btb908ze.py (Input 1,
                                                    Input v_cod_ult_obj_procesdo) /*prg_api_atualizar_ult_obj*/.
                    end /* if */.
                    run pi_tratar_esfgl073.
                end /* for block */.
            end /* if */.
            else do:
                excessao:
                for each tt_esfgl073
                where tt_esfgl073.cod_espec_bem >= dwb_rpt_select.cod_dwb_initial
                and   tt_esfgl073.cod_espec_bem <= dwb_rpt_select.cod_dwb_final use-index tt_rpt_cod_espec_bem:
                     delete tt_esfgl073.
                end /* for excessao */.
            end /* else */.
        end /* if */.
    end /* for selecao */.

    grp_block:
    for
        each tt_esfgl073 no-lock
        break by tt_esfgl073.ttv_cod_dwb_field_rpt[1]
              by tt_esfgl073.ttv_cod_dwb_field_rpt[2]:

        if  v_ind_dwb_run_mode = "Batch" /*l_batch*/
        then do:
            assign v_cod_ult_obj_procesdo = tt_esfgl073.cod_espec_bem + "," +
                                            tt_esfgl073.cod_caract_pat.
            run prgtec/btb/btb908ze.py (Input 1,
                                        Input v_cod_ult_obj_procesdo) /*prg_api_atualizar_ult_obj*/.
        end /* if */.

        if  first-of(tt_esfgl073.ttv_cod_dwb_field_rpt[1])
        then do:
            if  entry(1, dwb_rpt_param.cod_dwb_order) = "Caracter¡stica"
            then do:
                if (line-counter(s_1) + 4) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted
                    skip (2)
                    "Caracter¡stica: " at 1
                    tt_esfgl073.cod_caract_pat at 17 format "x(6)"
                    "-" at 24
                    tt_esfgl073.tta_des_caract_pat at 26 format "x(32)" skip (1).
                if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted
                    "Esp‚cie" at 11
                    "Descri‡Æo Esp‚cie" at 25
                    "Obrig" at 64 skip
                    "-------" at 11
                    "--------------------------------" at 25
                    "-----" at 64 skip.
            end /* if */.
            if  entry(1, dwb_rpt_param.cod_dwb_order) = "Esp‚cie Bem Patrimonial"
            then do:
               if (line-counter(s_1) + 4) > v_rpt_s_1_bottom then
                   page stream s_1.
               put stream s_1 unformatted
                   skip (2)
                   "Esp‚cie Bem Patrimonial: " at 1
                   tt_esfgl073.cod_espec_bem at 26 format "x(6)"
                   "-" at 33
                   tt_esfgl073.tta_des_espec_bem at 35 format "x(32)" skip (1).
               if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                   page stream s_1.
               put stream s_1 unformatted
                   "Caract" at 11
                   "Descri‡Æo" at 24
                   "Obrig" at 63 skip
                   "------" at 11
                   "--------------------------------" at 24
                   "-----" at 63 skip.
            end /* if */.
        end /* if */.

        if  entry(1, dwb_rpt_param.cod_dwb_order) = "Caracter¡stica"
        then do:
            if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                page stream s_1.
            put stream s_1 unformatted
                tt_esfgl073.cod_espec_bem at 11 format "x(6)"
                tt_esfgl073.tta_des_espec_bem at 25 format "x(32)"
                tt_esfgl073.log_obrig_preench at 64 format "Sim/NÆo" skip.
        end /* if */.

        if  entry(1, dwb_rpt_param.cod_dwb_order) = "Esp‚cie Bem Patrimonial"
        then do:
            if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                page stream s_1.
            put stream s_1 unformatted
                tt_esfgl073.cod_caract_pat at 11 format "x(6)"
                tt_esfgl073.tta_des_caract_pat at 24 format "x(32)"
                tt_esfgl073.log_obrig_preench at 63 format "Sim/NÆo" skip.
        end /* if */.

        delete tt_esfgl073.
    end /* for grp_block */.
END PROCEDURE. /* pi_esfgl073 */

PROCEDURE pi_isl_esfgl073:
    /* formato: */
    case v_cod_dwb_field:
        when "Esp‚cie Bem Patrimonial" then
            assign v_cod_dat_type = "character"
                   v_cod_format   = "x(6)":U
                   v_cod_initial  = string("":U, v_cod_format)
                   v_cod_final    = string("ZZZZZZ":U, v_cod_format).
        when "Caracter¡stica" then
            assign v_cod_dat_type = "character"
                   v_cod_format   = "x(6)":U
                   v_cod_initial  = string("":U, v_cod_format)
                   v_cod_final    = string("ZZZZZZ":U, v_cod_format).
    end /* case formato */.
END PROCEDURE. /* pi_isl_esfgl073 */

PROCEDURE pi_tratar_esfgl073:
    find tt_esfgl073 no-lock
         where tt_esfgl073.ttv_rec_caract_espec_pat = recid(caract_espec_pat) /*cl_key_recid of tt_esfgl073*/ no-error.
    if  dwb_rpt_select.log_dwb_rule = yes
    then do:
       if  not avail tt_esfgl073
       then do:
          create tt_esfgl073.
          assign tt_esfgl073.ttv_rec_caract_espec_pat = recid(caract_espec_pat)
                 tt_esfgl073.ttv_cod_dwb_field_rpt[lookup("Caracter¡stica", v_cod_order)] = caract_espec_pat.cod_caract_pat
                 tt_esfgl073.ttv_cod_dwb_field_rpt[lookup("Esp‚cie Bem Patrimonial", v_cod_order)] = caract_espec_pat.cod_espec_bem
                 tt_esfgl073.cod_caract_pat = caract_espec_pat.cod_caract_pat
                 tt_esfgl073.cod_espec_bem  = caract_espec_pat.cod_espec_bem
                 tt_esfgl073.log_obrig_preench = caract_espec_pat.log_obrig_preench.

          /* GRAVANDO A DESCRI‡€O DA CARACTER™STICA PATRIMONIAL NA TEMP-TABLE */
          find caract_pat no-lock
               where caract_pat.cod_caract_pat = tt_esfgl073.cod_caract_pat
                no-error.
          if  avail caract_pat
          then do:
              assign tt_esfgl073.tta_des_caract_pat = caract_pat.des_caract_pat.
          end /* if */.
          else do:
              assign tt_esfgl073.tta_des_caract_pat = " ".
          end /* else */.

          /* GRAVANDO A DESCRI‡€O DA Esp‚cie DE BEM PATRIMONIAL NA TEMP-TABLE */
          find espec_bem no-lock
               where espec_bem.cod_espec_bem = tt_esfgl073.cod_espec_bem
                no-error.
          if  avail espec_bem
          then do:
              assign tt_esfgl073.tta_des_espec_bem = espec_bem.des_espec_bem.
          end /* if */.
          else do:
              assign tt_esfgl073.tta_des_espec_bem = " ".
          end /* else */.
       end /* if */.
    end /* if */.
END PROCEDURE. /* pi_tratar_esfgl073 */
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
                "Programa Mensagem" c_prg_msg "nÆo encontrado."
                view-as alert-box error.
        return error.
    end.

    run value(c_prg_msg + ".p":U) (input c_action, input c_param).
    return return-value.
END PROCEDURE.  /* pi_messages */
/***********************  End of esfgl073 ***********************/


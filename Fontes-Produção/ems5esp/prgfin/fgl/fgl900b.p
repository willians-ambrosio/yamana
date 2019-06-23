/*****************************************************************************
** Copyright KRAFT CONSULTING
** Todos os Direitos Reservados.
** 
** Este fonte e de propriedade exclusiva da KRAFT CONSULTING, sua reproducao
** parcial ou total por qualquer meio, so' podera ser feita mediante
** autorizacao expressa.
**
** Programa..............: fnc_sdo_cta_ctbl_balan
** Descricao.............: FunªÑes Saldo Cta Ctbl Mais ParÉmetros
** Versao................:  1.00.01.016
** Procedimento..........: rel_balanct_ctbl
** Nome Externo..........: prgfin/fgl/fgl900b.p
** Data Geracao..........: 15/03/2011
** Criado por............: Augusto Guimar∆es
*****************************************************************************/
def buffer unid_organ for ems5.unid_organ.

def var c-versao-prg as char initial " 1.00.01.016":U no-undo.

{include/i_dbinst.i}
{include/i_dbtype.i}


/******************************* Private-Data *******************************/
assign this-procedure:private-data = "HLP=3":U.
/*************************************  *************************************/

&if "{&emsfin_dbinst}" <> "yes" &then
run pi_messages (input "show",
                 input 5884,
                 input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9", 
                                    "EMSFIN")) /*msg_5884*/.
&elseif "{&emsfin_version}" < "1.00" &then
run pi_messages (input "show",
                 input 5009,
                 input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9", 
                                    "FNC_SDO_CTA_CTBL_BALAN","~~EMSFIN", "~~{~&emsfin_version}", "~~1.00")) /*msg_5009*/.
&else

/************************** Buffer Definition Begin *************************/

def buffer b_finalid_econ
    for finalid_econ.


/*************************** Buffer Definition End **************************/

/************************* Variable Definition Begin ************************/

def new global shared var v_cod_aplicat_dtsul_corren
    as character
    format "x(3)":U
    no-undo.
def shared var v_cod_arq_planilha
    as character
    format "x(256)":U
    label "Arq Planilha"
    column-label "Arq Planilha"
    no-undo.
/*def shared var v_cod_carac_lim
    as character
    format "x(1)":U
    initial ";"
    label "Caracter Delimitador"
    no-undo.*/
def new global shared var v_cod_ccusto_corren
    as character
    format "x(11)":U
    label "Centro Custo"
    column-label "Centro Custo"
    no-undo.
def shared var v_cod_ccusto_excec
    as character
    format "x(11)":U
    initial "..........."
    label "Parte Exceá∆o"
    column-label "Parte Exceá∆o"
    no-undo.
def shared var v_cod_ccusto_pfixa
    as character
    format "x(11)":U
    label "Parte Fixa CCusto"
    column-label "Parte Fixa CCusto"
    no-undo.
def shared var v_cod_cta_ctbl_excec
    as character
    format "x(20)":U
    initial "...................."
    label "Parte Exceá∆o"
    column-label "Parte Exceá∆o"
    no-undo.
def shared var v_cod_cta_ctbl_pfixa
    as character
    format "x(20)":U
    label "Parte Fixa"
    column-label "Parte Fixa Cta Ctbl"
    no-undo.
def shared var v_cod_dwb_file
    as character
    format "x(40)":U
    label "Arquivo"
    column-label "Arquivo"
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
def shared var v_cod_finalid_econ_apr
    as character
    format "x(10)":U
    label "Finalid Converte"
    column-label "Finalid Conv"
    no-undo.
def shared var v_cod_finalid_econ_bas
    as character
    format "x(10)":U
    label "Finalidade Base"
    column-label "Finalidade Base"
    no-undo.
def var v_cod_format_proj_financ
    as character
    format "x(20)":U
    label "Formato Projeto"
    column-label "Formato Projeto"
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
def shared var v_cod_idioma_apr
    as character
    format "x(8)":U
    label "Idioma Apresentaá∆o"
    column-label "Idioma"
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
def shared var v_cod_plano_ccusto
    as character
    format "x(8)":U
    label "Plano CCusto"
    column-label "Plano CCusto"
    no-undo.
def shared var v_cod_plano_ccusto_ant
    as character
    format "x(8)":U
    label "Plano Centros Custo"
    column-label "Plano Centros Custo"
    no-undo.
def new global shared var v_cod_plano_ccusto_corren
    as character
    format "x(8)":U
    label "Plano CCusto"
    column-label "Plano CCusto"
    no-undo.
def shared var v_cod_plano_cta_ctbl
    as character
    format "x(8)":U
    label "Plano Contas"
    column-label "Plano Contas"
    no-undo.
def shared var v_cod_plano_cta_ctbl_ant
    as character
    format "x(8)":U
    label "Plano Contas"
    column-label "Plano Contas"
    no-undo.
def shared var v_cod_proj_financ
    as character
    format "x(20)":U
    label "Projeto"
    column-label "Projeto"
    no-undo.
def shared var v_cod_proj_financ_ant
    as character
    format "x(20)":U
    no-undo.
def shared var v_cod_proj_financ_excec
    as character
    format "x(20)":U
    initial "...................."
    label "Exceá∆o"
    column-label "Exceá∆o"
    no-undo.
def shared var v_cod_proj_financ_pfixa
    as character
    format "x(20)":U
    label "Parte Fixa"
    no-undo.
def var v_cod_return
    as character
    format "x(40)":U
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
def shared var v_cod_unid_organ
    as character
    format "x(3)":U
    label "Unid Organizacional"
    column-label "Unid Organizacional"
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
def shared var v_dat_cotac_indic_econ
    as date
    format "99/99/9999":U
    initial today
    label "Data Cotaá∆o"
    column-label "Data Cotaá∆o"
    no-undo.
def shared var v_log_balanct_param
    as logical
    format "Sim/N∆o"
    initial yes
    no-undo.
def shared var v_log_ccusto_sum
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    label "Sumaria Ccusto"
    column-label "Sum Ccusto"
    no-undo.
def shared var v_log_consid_apurac_restdo
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    label "Consid Apurac Restdo"
    column-label "Apurac Restdo"
    no-undo.
def shared var v_log_cta_ctbl_analit
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    label "Somente Cta Analit"
    no-undo.
def shared var v_log_cta_ctbl_internac
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    label "Consid Cta Internac"
    no-undo.
def shared var v_log_cta_ctbl_sdo
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    label "Impr Cta Sem Sdo"
    no-undo.
def shared var v_log_estab_sum
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    label "Sumaria Estab"
    no-undo.
/*def shared var v_log_gerac_planilha
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    label "Gera Excel"
    no-undo.*/
def new global shared var v_log_gerencial
    as logical
    format "Sim/N∆o"
    initial no
    no-undo.
def shared var v_log_mostra_sem_aprop_cc
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    label "Mostra sem Aprop CC"
    column-label "Mostra S/Apr CC"
    no-undo.
def shared var v_log_period_ctbl_ant_impr
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    label "Impr Period Anterior"
    no-undo.
def shared var v_log_proj_financ
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    label "Sumaria Projeto"
    no-undo.
def shared var v_log_unid_negoc_sum
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    label "Sumaria Unid Negoc"
    column-label "Sum Un Neg"
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
def shared var v_nom_prog_ext_aux
    as character
    format "x(8)":U
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
def shared var v_num_niv_estrut
    as integer
    format ">>9":U
    label "N°vel Estrutura"
    no-undo.
def new global shared var v_num_ped_exec_corren
    as integer
    format ">>>>9":U
    no-undo.
def shared var v_rec_dwb_rpt_param
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_finalid_econ
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_idioma
    as recid
    format ">>>>>>9":U
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
def var v_cod_arq_planilha_aux           as character       no-undo. /*local*/
/*def var v_cod_carac_lim_aux              as character       no-undo. /*local*/*/
def var v_cod_finalid_econ_aprop_aux     as character       no-undo. /*local*/
def var v_cod_finalid_econ_base_aux      as character       no-undo. /*local*/
def var v_cod_idiom_apres_aux            as character       no-undo. /*local*/
def var v_dat_cotac_indic_econ_aux       as date            no-undo. /*local*/
def var v_log_ccusto_aux                 as logical         no-undo. /*local*/
def var v_log_consid_apurac_aux          as logical         no-undo. /*local*/
def var v_log_cta_ctbl_analit_aux        as logical         no-undo. /*local*/
def var v_log_cta_ctbl_internac_aux      as logical         no-undo. /*local*/
def var v_log_cta_ctbl_sdo_aux           as logical         no-undo. /*local*/
def var v_log_estab_aux                  as logical         no-undo. /*local*/
/*def var v_log_gerac_planilha_aux         as logical         no-undo. /*local*/*/
def var v_log_mostra_sem_aprop_cc_aux    as logical         no-undo. /*local*/
def var v_log_period_ctbl_ant_aux        as logical         no-undo. /*local*/
def var v_log_proj_financ_aux            as logical         no-undo. /*local*/
def var v_log_unid_negoc_aux             as logical         no-undo. /*local*/
def var v_num_niv_estrut_aux             as integer         no-undo. /*local*/


/************************** Variable Definition End *************************/

/*************************** Menu Definition Begin **************************/

.

def menu      m_help                menubar
    menu-item mi_conteudo           label "&Conteﬂdo"
    menu-item mi_sobre              label "&Sobre".



/**************************** Menu Definition End ***************************/

/************************ Rectangle Definition Begin ************************/

def rectangle rt_001
    size 1 by 1
    edge-pixels 2.
def rectangle rt_002
    size 1 by 1
    edge-pixels 2.
def rectangle rt_003
    size 1 by 1
    edge-pixels 2.
def rectangle rt_005
    size 1 by 1
    edge-pixels 2.
def rectangle rt_006
    size 1 by 1
    edge-pixels 2.
def rectangle rt_007
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
/****************************** Function Button *****************************/


def button bt_get_file2_179325
    label "Get"
    tooltip "Encontra Arquivo"
&if "{&window-system}" <> "TTY" &then
    image-up file "image/im-sea2"
    image-insensitive file "image/ii-sea2"
&endif
    size 4 by 1.1.
def button bt_zoo_135382
    label "Zoom"
    tooltip "Zoom"
&if "{&window-system}" <> "TTY" &then
    image-up file "image/im-zoo"
    image-insensitive file "image/ii-zoo"
&endif
    size 4 by .88.
def button bt_zoo_135383
    label "Zoom"
    tooltip "Zoom"
&if "{&window-system}" <> "TTY" &then
    image-up file "image/im-zoo"
    image-insensitive file "image/ii-zoo"
&endif
    size 4 by .88.
def button bt_zoo_177688
    label "Zoom"
    tooltip "Zoom"
&if "{&window-system}" <> "TTY" &then
    image-up file "image/im-zoo"
    image-insensitive file "image/ii-zoo"
&endif
    size 4 by .88.


/*************************** Button Definition End **************************/

/************************** Frame Definition Begin **************************/

def frame f_dlg_03_gerac_planilha
    rt_001
         at row 01.38 col 02.14 bgcolor 8 
    rt_002
         at row 02.50 col 03.29
    rt_cxcf
         at row 05.80 col 02.00 bgcolor 7 
    /*v_log_gerac_planilha
         at row 01.63 col 06.00 label "Gerar Arquivo em Excel"
         view-as toggle-box*/
    v_cod_arq_planilha
         at row 02.92 col 06.00 no-label
         help "Arquivo Sa°da para Planilha CSV"
         view-as fill-in
         size-chars 41.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_get_file2_179325
         at row 02.92 col 47.14
    /*v_cod_carac_lim
         at row 04.13 col 24.00 colon-aligned label "Caracter Delimitador"
         help "Caracter Delimitador de Colunas"
         view-as fill-in
         size-chars 2.14 by .88
         fgcolor ? bgcolor 15 font 2*/
    bt_ok
         at row 06.02 col 03.00 font ?
         help "OK"
    bt_can
         at row 06.02 col 14.00 font ?
         help "Cancela"
    bt_hel2
         at row 06.02 col 42.28 font ?
         help "Ajuda"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 54.72 by 07.63 default-button bt_ok
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Arquivo CSV".
    /* adjust size of objects in this frame */
    assign bt_can:width-chars   in frame f_dlg_03_gerac_planilha = 10.00
           bt_can:height-chars  in frame f_dlg_03_gerac_planilha = 01.00
           bt_hel2:width-chars  in frame f_dlg_03_gerac_planilha = 10.00
           bt_hel2:height-chars in frame f_dlg_03_gerac_planilha = 01.00
           bt_ok:width-chars    in frame f_dlg_03_gerac_planilha = 10.00
           bt_ok:height-chars   in frame f_dlg_03_gerac_planilha = 01.00
           rt_001:width-chars   in frame f_dlg_03_gerac_planilha = 51.14
           rt_001:height-chars  in frame f_dlg_03_gerac_planilha = 04.21
           rt_002:width-chars   in frame f_dlg_03_gerac_planilha = 49.14
           rt_002:height-chars  in frame f_dlg_03_gerac_planilha = 02.75
           rt_cxcf:width-chars  in frame f_dlg_03_gerac_planilha = 51.28
           rt_cxcf:height-chars in frame f_dlg_03_gerac_planilha = 01.40.
    /* set private-data for the help system */
    assign /*v_log_gerac_planilha:private-data in frame f_dlg_03_gerac_planilha = "HLP=000016907":U*/
           bt_get_file2_179325:private-data  in frame f_dlg_03_gerac_planilha = "HLP=000016974":U
           v_cod_arq_planilha:private-data   in frame f_dlg_03_gerac_planilha = "HLP=000016908":U
           /*v_cod_carac_lim:private-data      in frame f_dlg_03_gerac_planilha = "HLP=000016909":U*/
           bt_ok:private-data                in frame f_dlg_03_gerac_planilha = "HLP=000010721":U
           bt_can:private-data               in frame f_dlg_03_gerac_planilha = "HLP=000011050":U
           bt_hel2:private-data              in frame f_dlg_03_gerac_planilha = "HLP=000011326":U
           frame f_dlg_03_gerac_planilha:private-data                         = "HLP=000016974".
    /* enable function buttons */
    assign bt_get_file2_179325:sensitive in frame f_dlg_03_gerac_planilha = yes.

def frame f_dlg_03_sdo_cta_ctbl_balan
    rt_002
         at row 06.25 col 01.57 bgcolor 8 
    rt_001
         at row 01.38 col 01.86
    " Sumaria " view-as text
         at row 01.08 col 03.86 bgcolor 8 
    rt_003
         at row 01.38 col 40.43
    " Converte " view-as text
         at row 01.08 col 42.43 bgcolor 8 
    rt_005
         at row 06.29 col 39.86
    " Conta Ctbl " view-as text
         at row 05.99 col 41.86 bgcolor 8 
    rt_006
         at row 09.38 col 39.86
    " Centro Custo " view-as text
         at row 09.08 col 41.86 bgcolor 8 
    rt_007
         at row 12.50 col 40.14 bgcolor 8 
    rt_cxcf
         at row 15.46 col 02.00 bgcolor 7 
    v_log_estab_sum
         at row 02.00 col 04.00 label "Estabelecimento"
         view-as toggle-box
    v_log_unid_negoc_sum
         at row 03.00 col 04.00 label "Un Negoc"
         view-as toggle-box
    v_log_ccusto_sum
         at row 04.00 col 04.00 label "Centro Custo"
         view-as toggle-box
    v_log_proj_financ
         at row 05.00 col 04.00 label "Projeto"
         view-as toggle-box
    v_cod_finalid_econ_bas
         at row 02.00 col 58.00 colon-aligned label "Finalidade Base"
         help "Finalidade Econìmica Base"
         view-as fill-in
         size-chars 11.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_zoo_135383
         at row 02.00 col 71.14
    v_cod_finalid_econ_apr
         at row 03.00 col 58.00 colon-aligned label "Finalid Converte"
         help "Finalidade Econìmica de Convers∆o"
         view-as fill-in
         size-chars 11.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_zoo_135382
         at row 03.00 col 71.14
    v_dat_cotac_indic_econ
         at row 04.00 col 58.00 colon-aligned label "Data Cotaá∆o"
         view-as fill-in
         size-chars 11.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_log_cta_ctbl_internac
         at row 06.63 col 03.57 label "Considera Conta Internacional"
         view-as toggle-box
    v_log_period_ctbl_ant_impr
         at row 07.63 col 03.57 label "Imprime Per°odo Cont†bil Anterior"
         view-as toggle-box
    v_log_cta_ctbl_sdo
         at row 08.63 col 03.57 label "Imprime Contas Sem Saldo"
         view-as toggle-box
    v_log_cta_ctbl_analit
         at row 09.63 col 03.57 label "Somente Contas Anal°ticas"
         view-as toggle-box
    v_log_consid_apurac_restdo
         at row 10.63 col 03.57 label "Consid Apurac Restdo"
         help "Considera apuraá∆o de resultados"
         view-as toggle-box
    v_log_mostra_sem_aprop_cc
         at row 11.63 col 03.57 label "Mostra Contas sem Apropriaá‰es de C.Custo"
         help "Mostra Contas sem Apropriaá‰es de C.Custo"
         view-as toggle-box
    v_cod_idioma_apr
         at row 12.63 col 16.57 colon-aligned label "Idioma Apres"
         help "Idioma para Apresentaá∆o"
         view-as fill-in
         size-chars 9.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_zoo_177688
         at row 12.63 col 27.71
    v_num_niv_estrut
         at row 13.63 col 16.57 colon-aligned label "N°v Estrut"
         help "N°vel Estrutura Conta Cont†bil"
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_cta_ctbl_pfixa
         at row 06.83 col 53.00 colon-aligned label "Parte Fixa"
         help "Parte Fixa Conta Cont†bil"
         view-as fill-in
         size-chars 21.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_cta_ctbl_excec
         at row 07.83 col 53.00 colon-aligned label "Exceá∆o"
         help "Parte Exceá∆o da Conta Cont†bil"
         view-as fill-in
         size-chars 21.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_ccusto_pfixa
         at row 09.83 col 52.72 colon-aligned label "Parte Fixa"
         help "C¢digo Parte Fixa Centro Custo"
         view-as fill-in
         size-chars 12.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_ccusto_excec
         at row 10.83 col 52.72 colon-aligned label "Exceá∆o"
         help "Parte Exceá∆o do Centro de Custo"
         view-as fill-in
         size-chars 12.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_proj_financ_pfixa
         at row 13.00 col 52.72 colon-aligned label "Parte Fixa"
         help "Parte Fixa"
         view-as fill-in
         size-chars 21.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_proj_financ_excec
         at row 14.00 col 52.72 colon-aligned label "Exceá∆o"
         help "Exceá∆o"
         view-as fill-in
         size-chars 21.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_ok
         at row 15.67 col 03.00 font ?
         help "OK"
    bt_can
         at row 15.67 col 14.00 font ?
         help "Cancela"
    bt_hel2
         at row 15.67 col 67.00 font ?
         help "Ajuda"
    "Projeto"
         at row 12.25 col 42.29 font 1
         view-as text /*l_projeto*/
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 79.43 by 17.29 default-button bt_ok
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "ParÉmetros do Balancete".
    /* adjust size of objects in this frame */
    assign bt_can:width-chars   in frame f_dlg_03_sdo_cta_ctbl_balan = 10.00
           bt_can:height-chars  in frame f_dlg_03_sdo_cta_ctbl_balan = 01.00
           bt_hel2:width-chars  in frame f_dlg_03_sdo_cta_ctbl_balan = 10.00
           bt_hel2:height-chars in frame f_dlg_03_sdo_cta_ctbl_balan = 01.00
           bt_ok:width-chars    in frame f_dlg_03_sdo_cta_ctbl_balan = 10.00
           bt_ok:height-chars   in frame f_dlg_03_sdo_cta_ctbl_balan = 01.00
           rt_001:width-chars   in frame f_dlg_03_sdo_cta_ctbl_balan = 37.43
           rt_001:height-chars  in frame f_dlg_03_sdo_cta_ctbl_balan = 04.63
           rt_002:width-chars   in frame f_dlg_03_sdo_cta_ctbl_balan = 37.43
           rt_002:height-chars  in frame f_dlg_03_sdo_cta_ctbl_balan = 08.88
           rt_003:width-chars   in frame f_dlg_03_sdo_cta_ctbl_balan = 37.72
           rt_003:height-chars  in frame f_dlg_03_sdo_cta_ctbl_balan = 04.00
           rt_005:width-chars   in frame f_dlg_03_sdo_cta_ctbl_balan = 37.72
           rt_005:height-chars  in frame f_dlg_03_sdo_cta_ctbl_balan = 02.75
           rt_006:width-chars   in frame f_dlg_03_sdo_cta_ctbl_balan = 37.72
           rt_006:height-chars  in frame f_dlg_03_sdo_cta_ctbl_balan = 02.71
           rt_007:width-chars   in frame f_dlg_03_sdo_cta_ctbl_balan = 37.43
           rt_007:height-chars  in frame f_dlg_03_sdo_cta_ctbl_balan = 02.63
           rt_cxcf:width-chars  in frame f_dlg_03_sdo_cta_ctbl_balan = 76.00
           rt_cxcf:height-chars in frame f_dlg_03_sdo_cta_ctbl_balan = 01.42.
    /* set private-data for the help system */
    assign v_log_estab_sum:private-data            in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000024437":U
           v_log_unid_negoc_sum:private-data       in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000024440":U
           v_log_ccusto_sum:private-data           in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000024425":U
           v_log_proj_financ:private-data          in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000014325":U
           bt_zoo_135383:private-data              in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000009431":U
           v_cod_finalid_econ_bas:private-data     in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000024418":U
           bt_zoo_135382:private-data              in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000009431":U
           v_cod_finalid_econ_apr:private-data     in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000019258":U
           v_dat_cotac_indic_econ:private-data     in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000012264":U
           v_log_cta_ctbl_internac:private-data    in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000024424":U
           v_log_period_ctbl_ant_impr:private-data in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000024331":U
           v_log_cta_ctbl_sdo:private-data         in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000024333":U
           v_log_cta_ctbl_analit:private-data      in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000024259":U
           v_log_consid_apurac_restdo:private-data in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000023726":U
           v_log_mostra_sem_aprop_cc:private-data  in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000014325":U
           bt_zoo_177688:private-data              in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000009431":U
           v_cod_idioma_apr:private-data           in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000024309":U
           v_num_niv_estrut:private-data           in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000024340":U
           v_cod_cta_ctbl_pfixa:private-data       in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000019706":U
           v_cod_cta_ctbl_excec:private-data       in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000019707":U
           v_cod_ccusto_pfixa:private-data         in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000023796":U
           v_cod_ccusto_excec:private-data         in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000023559":U
           v_cod_proj_financ_pfixa:private-data    in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000014325":U
           v_cod_proj_financ_excec:private-data    in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000014325":U
           bt_ok:private-data                      in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000010721":U
           bt_can:private-data                     in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000011050":U
           bt_hel2:private-data                    in frame f_dlg_03_sdo_cta_ctbl_balan = "HLP=000011326":U
           frame f_dlg_03_sdo_cta_ctbl_balan:private-data                               = "HLP=000014325".
    /* enable function buttons */
    assign bt_zoo_135383:sensitive in frame f_dlg_03_sdo_cta_ctbl_balan = yes
           bt_zoo_135382:sensitive in frame f_dlg_03_sdo_cta_ctbl_balan = yes
           bt_zoo_177688:sensitive in frame f_dlg_03_sdo_cta_ctbl_balan = yes.



/*************************** Frame Definition End ***************************/

/*********************** User Interface Trigger Begin ***********************/


ON CHOOSE OF bt_ok IN FRAME f_dlg_03_gerac_planilha
DO:
    ASSIGN v_cod_arq_planilha = INPUT FRAME f_dlg_03_gerac_planilha v_cod_arq_planilha.

    /*
    ASSIGN FILE-INFO:FILE-NAME = v_cod_arq_planilha:SCREEN-VALUE.

    IF FILE-INFO:FILE-TYPE = ? THEN
    DO:
        /* O diret¢rio &1 n∆o existe ! */
         run pi_messages (input "show",
                          input 4354,
                          input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                             v_cod_arq_planilha:SCREEN-VALUE)) /*msg_4354*/.

         APPLY "entry" TO v_cod_arq_planilha IN FRAME f_dlg_03_gerac_planilha.
         RETURN NO-APPLY.
    END.
    */
END.



ON CHOOSE OF bt_can IN FRAME f_dlg_03_gerac_planilha
DO:

    assign /*v_log_gerac_planilha = /*v_log_gerac_planilha_aux*/ YES*/
           v_cod_arq_planilha   = v_cod_arq_planilha_aux
           /*v_cod_carac_lim      = v_cod_carac_lim_aux*/.
END. /* ON CHOOSE OF bt_can IN FRAME f_dlg_03_gerac_planilha */

ON CHOOSE OF bt_hel2 IN FRAME f_dlg_03_gerac_planilha
DO:


    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.


    /* End_Include: i_context_help_frame */

END. /* ON CHOOSE OF bt_hel2 IN FRAME f_dlg_03_gerac_planilha */

/*ON VALUE-CHANGED OF v_log_gerac_planilha IN FRAME f_dlg_03_gerac_planilha
DO:

    if  input frame f_dlg_03_gerac_planilha v_log_gerac_planilha = yes
    then do:
        if  v_cod_arq_planilha = ""
        then do:
           find usuar_mestre no-lock
             where usuar_mestre.cod_usuario = v_cod_usuar_corren no-error.
           if  avail usuar_mestre
           then do:
              if  usuar_mestre.nom_dir_spool <> ""
              then do:
                 assign v_cod_arq_planilha = usuar_mestre.nom_dir_spool + '~/'.
              end /* if */.
              assign v_cod_arq_planilha = v_cod_arq_planilha
                                        + lc(v_nom_prog_ext_aux).
              if  substr(v_cod_dwb_file,r-index(v_cod_dwb_file,chr(46)) + 1) = 'txt'
              then do:
                  assign v_cod_arq_planilha = v_cod_arq_planilha
                                            + '.prn'.
              end /* if */.
              else do:
                  assign v_cod_arq_planilha = v_cod_arq_planilha
                                            + '.txt'.
              end /* else */.
           end /* if */.
           assign v_cod_carac_lim = ";".
        end /* if */.
        enable v_cod_arq_planilha
               v_cod_carac_lim
               with frame f_dlg_03_gerac_planilha.
    end /* if */.
    else do:
        assign v_cod_arq_planilha = ""
               v_cod_carac_lim    = "".
        disable v_cod_arq_planilha
                v_cod_carac_lim
                with frame f_dlg_03_gerac_planilha.
    end /* else */.
    display v_cod_arq_planilha
            v_cod_carac_lim
            with frame f_dlg_03_gerac_planilha.

END. /* ON VALUE-CHANGED OF v_log_gerac_planilha IN FRAME f_dlg_03_gerac_planilha */*/

ON CHOOSE OF bt_can IN FRAME f_dlg_03_sdo_cta_ctbl_balan
DO:

    assign v_cod_finalid_econ_bas  = v_cod_finalid_econ_base_aux
           v_cod_finalid_econ_apr  = v_cod_finalid_econ_aprop_aux
           v_log_cta_ctbl_internac = v_log_cta_ctbl_internac_aux
           v_log_cta_ctbl_sdo      = v_log_cta_ctbl_sdo_aux
           v_log_cta_ctbl_analit   = v_log_cta_ctbl_analit_aux
           v_log_estab_sum         = v_log_estab_aux
           v_log_ccusto_sum        = v_log_ccusto_aux
           v_log_unid_negoc_sum    = v_log_unid_negoc_aux
           v_log_period_ctbl_ant_impr = v_log_period_ctbl_ant_aux
           v_log_consid_apurac_restdo = v_log_consid_apurac_aux
           v_dat_cotac_indic_econ  = v_dat_cotac_indic_econ_aux
           v_num_niv_estrut        = v_num_niv_estrut_aux
           v_cod_idioma_apr        = v_cod_idiom_apres_aux
           v_log_proj_financ       = v_log_proj_financ_aux.

    apply "endkey" to frame f_dlg_03_sdo_cta_ctbl_balan.
END. /* ON CHOOSE OF bt_can IN FRAME f_dlg_03_sdo_cta_ctbl_balan */

ON CHOOSE OF bt_hel2 IN FRAME f_dlg_03_sdo_cta_ctbl_balan
DO:


    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.


    /* End_Include: i_context_help_frame */

END. /* ON CHOOSE OF bt_hel2 IN FRAME f_dlg_03_sdo_cta_ctbl_balan */

ON LEAVE OF v_cod_finalid_econ_apr IN FRAME f_dlg_03_sdo_cta_ctbl_balan
DO:

    if  input frame f_dlg_03_sdo_cta_ctbl_balan v_cod_finalid_econ_bas = input frame f_dlg_03_sdo_cta_ctbl_balan v_cod_finalid_econ_apr
    then do:
        disable v_dat_cotac_indic_econ
                with frame f_dlg_03_sdo_cta_ctbl_balan.
    end /* if */.
    else do:
        enable v_dat_cotac_indic_econ
               with frame f_dlg_03_sdo_cta_ctbl_balan.
    end /* else */.
END. /* ON LEAVE OF v_cod_finalid_econ_apr IN FRAME f_dlg_03_sdo_cta_ctbl_balan */

ON VALUE-CHANGED OF v_log_ccusto_sum IN FRAME f_dlg_03_sdo_cta_ctbl_balan
DO:

    if  v_log_ccusto_sum:checked in frame f_dlg_03_sdo_cta_ctbl_balan = yes then 
        assign v_log_mostra_sem_aprop_cc:checked in frame f_dlg_03_sdo_cta_ctbl_balan   = no
               v_log_mostra_sem_aprop_cc:sensitive in frame f_dlg_03_sdo_cta_ctbl_balan = no.
    else
        assign v_log_mostra_sem_aprop_cc:sensitive in frame f_dlg_03_sdo_cta_ctbl_balan = yes.

END. /* ON VALUE-CHANGED OF v_log_ccusto_sum IN FRAME f_dlg_03_sdo_cta_ctbl_balan */


/************************ User Interface Trigger End ************************/

/************************** Function Trigger Begin **************************/


ON  CHOOSE OF bt_get_file2_179325 IN FRAME f_dlg_03_gerac_planilha
OR F5 OF v_cod_arq_planilha IN FRAME f_dlg_03_gerac_planilha DO:

    /************************* Variable Definition Begin ************************/

    def var v_cod_get_file                   as character       no-undo. /*local*/
    def var v_log_pressed                    as logical         no-undo. /*local*/


    /************************** Variable Definition End *************************/
/*
    SYSTEM-DIALOG GET-DIR v_cod_get_file
        /*INITIAL-DIR "\\ydminteg01\temp\rpw"*/
        RETURN-TO-START-DIR
        TITLE "Destino Arquivos CSV".
        */
    
    system-dialog get-file v_cod_get_file
        title "Procurando..." /*l_procurando...*/ 
        filters '*.csv' '*.csv'
        must-exist
        update v_log_pressed.

    if  v_log_pressed = yes
    then do:
        assign v_cod_arq_planilha:screen-value in frame f_dlg_03_gerac_planilha = v_cod_get_file.
        apply "entry" to v_cod_arq_planilha in frame f_dlg_03_gerac_planilha.
    end. /*  if */

end. /* ON  CHOOSE OF bt_get_file2_179325 IN FRAME f_dlg_03_gerac_planilha */

ON  CHOOSE OF bt_zoo_135382 IN FRAME f_dlg_03_sdo_cta_ctbl_balan
OR F5 OF v_cod_finalid_econ_apr IN FRAME f_dlg_03_sdo_cta_ctbl_balan DO:

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
        assign v_cod_finalid_econ_apr:screen-value in frame f_dlg_03_sdo_cta_ctbl_balan =
               string(finalid_econ.cod_finalid_econ).

        apply "entry" to v_cod_finalid_econ_apr in frame f_dlg_03_sdo_cta_ctbl_balan.
    end /* if */.

end. /* ON  CHOOSE OF bt_zoo_135382 IN FRAME f_dlg_03_sdo_cta_ctbl_balan */

ON  CHOOSE OF bt_zoo_135383 IN FRAME f_dlg_03_sdo_cta_ctbl_balan
OR F5 OF v_cod_finalid_econ_bas IN FRAME f_dlg_03_sdo_cta_ctbl_balan DO:

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
        assign v_cod_finalid_econ_bas:screen-value in frame f_dlg_03_sdo_cta_ctbl_balan =
               string(finalid_econ.cod_finalid_econ).

        apply "entry" to v_cod_finalid_econ_bas in frame f_dlg_03_sdo_cta_ctbl_balan.
    end /* if */.

end. /* ON  CHOOSE OF bt_zoo_135383 IN FRAME f_dlg_03_sdo_cta_ctbl_balan */

ON  CHOOSE OF bt_zoo_177688 IN FRAME f_dlg_03_sdo_cta_ctbl_balan
OR F5 OF v_cod_idioma_apr IN FRAME f_dlg_03_sdo_cta_ctbl_balan DO:

    /* fn_generic_zoom_variable */
    if  search("prgint/utb/utb014ka.r") = ? and search("prgint/utb/utb014ka.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb014ka.p".
        else do:
            message "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb014ka.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run prgint/utb/utb014ka.p /*prg_sea_idioma*/.
    if  v_rec_idioma <> ?
    then do:
        find idioma where recid(idioma) = v_rec_idioma no-lock no-error.
        assign v_cod_idioma_apr:screen-value in frame f_dlg_03_sdo_cta_ctbl_balan =
               string(idioma.cod_idioma).

        apply "entry" to v_cod_idioma_apr in frame f_dlg_03_sdo_cta_ctbl_balan.
    end /* if */.

end. /* ON  CHOOSE OF bt_zoo_177688 IN FRAME f_dlg_03_sdo_cta_ctbl_balan */


/*************************** Function Trigger End ***************************/

/**************************** Frame Trigger Begin ***************************/


ON HELP OF FRAME f_dlg_03_gerac_planilha ANYWHERE
DO:


    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */

END. /* ON HELP OF FRAME f_dlg_03_gerac_planilha */

ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_03_gerac_planilha ANYWHERE
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

END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_03_gerac_planilha */

ON RIGHT-MOUSE-UP OF FRAME f_dlg_03_gerac_planilha ANYWHERE
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

END. /* ON RIGHT-MOUSE-UP OF FRAME f_dlg_03_gerac_planilha */

ON WINDOW-CLOSE OF FRAME f_dlg_03_gerac_planilha
DO:

    apply "end-error" to self.

END. /* ON WINDOW-CLOSE OF FRAME f_dlg_03_gerac_planilha */

ON HELP OF FRAME f_dlg_03_sdo_cta_ctbl_balan ANYWHERE
DO:


    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */

END. /* ON HELP OF FRAME f_dlg_03_sdo_cta_ctbl_balan */

ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_03_sdo_cta_ctbl_balan ANYWHERE
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

END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_03_sdo_cta_ctbl_balan */

ON RIGHT-MOUSE-UP OF FRAME f_dlg_03_sdo_cta_ctbl_balan ANYWHERE
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

END. /* ON RIGHT-MOUSE-UP OF FRAME f_dlg_03_sdo_cta_ctbl_balan */

ON WINDOW-CLOSE OF FRAME f_dlg_03_sdo_cta_ctbl_balan
DO:

    apply "end-error" to self.

END. /* ON WINDOW-CLOSE OF FRAME f_dlg_03_sdo_cta_ctbl_balan */


/***************************** Frame Trigger End ****************************/

/**************************** Menu Trigger Begin ****************************/


ON CHOOSE OF MENU-ITEM mi_conteudo IN MENU m_help
DO:


        apply "choose" to bt_hel2 in frame f_dlg_03_sdo_cta_ctbl_balan.



        apply "choose" to bt_hel2 in frame f_dlg_03_gerac_planilha.



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


        assign v_nom_prog     = substring(frame f_dlg_03_sdo_cta_ctbl_balan:title, 1, max(1, length(frame f_dlg_03_sdo_cta_ctbl_balan:title) - 10)).
        if  v_nom_prog = ? then
            assign v_nom_prog = "".

        assign v_nom_prog     = v_nom_prog
                              + chr(10)
                              + "fnc_sdo_cta_ctbl_balan":U.



        assign v_nom_prog     = substring(frame f_dlg_03_gerac_planilha:title, 1, max(1, length(frame f_dlg_03_gerac_planilha:title) - 10)).
        if  v_nom_prog = ? then
            assign v_nom_prog = "".

        assign v_nom_prog     = v_nom_prog
                              + chr(10)
                              + "fnc_sdo_cta_ctbl_balan":U.


    assign v_nom_prog_ext = "prgfin/fgl/fgl900b.p":U
           v_cod_release  = trim(" 1.00.01.016":U).
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

/*def stream s-arq.*/

if  v_cod_arq <> '' and v_cod_arq <> ?
then do:
    run pi_version_extract ('fnc_sdo_cta_ctbl_balan', 'prgfin/fgl/fgl900b.p', '1.00.01.016', 'pro').
end /* if */.
/* End_Include: i_version_extract */

/*assign v_cod_carac_lim:width-chars in frame f_dlg_03_gerac_planilha = 3.14 .*/
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
    run prgtec/men/men901za.py (Input 'fnc_sdo_cta_ctbl_balan') /*prg_fnc_verify_security*/.
if  return-value = "2014"
then do:
    /* Programa a ser executado n∆o ˝ um programa v†lido Datasul ! */
    run pi_messages (input "show",
                     input 2014,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'fnc_sdo_cta_ctbl_balan')) /*msg_2014*/.
    return.
end /* if */.
if  return-value = "2012"
then do:
    /* Usu†rio sem permiss∆o para acessar o programa. */
    run pi_messages (input "show",
                     input 2012,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'fnc_sdo_cta_ctbl_balan')) /*msg_2012*/.
    return.
end /* if */.
/* End_Include: i_verify_security */



/* Begin_Include: i_log_exec_prog_dtsul_ini */
assign v_rec_log = ?.

if can-find(prog_dtsul
       where prog_dtsul.cod_prog_dtsul = 'fnc_sdo_cta_ctbl_balan' 
         and prog_dtsul.log_gera_log_exec = yes) then do transaction:
    create log_exec_prog_dtsul.
    assign log_exec_prog_dtsul.cod_prog_dtsul           = 'fnc_sdo_cta_ctbl_balan'
           log_exec_prog_dtsul.cod_usuario              = v_cod_usuar_corren
           log_exec_prog_dtsul.dat_inic_exec_prog_dtsul = today
           log_exec_prog_dtsul.hra_inic_exec_prog_dtsul = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":","").
    assign v_rec_log = recid(log_exec_prog_dtsul).
    release log_exec_prog_dtsul no-error.
end.


/* End_Include: i_log_exec_prog_dtsul_ini */



/* Begin_Include: i_std_dialog_box */
/* tratamento do titulo e vers∆o */
assign frame f_dlg_03_sdo_cta_ctbl_balan:title = frame f_dlg_03_sdo_cta_ctbl_balan:title
                            + chr(32)
                            + chr(40)
                            + trim(" 1.00.01.016":U)
                            + chr(41).
/* menu pop-up de ajuda e sobre */
assign menu m_help:popup-only = yes
       bt_hel2:popup-menu in frame f_dlg_03_sdo_cta_ctbl_balan = menu m_help:handle.


/* End_Include: i_std_dialog_box */


if  v_log_gerencial  /* Se foi chamado do Balancete Gerencial */ then DO:
    assign v_cod_proj_financ_pfixa:visible in frame f_dlg_03_sdo_cta_ctbl_balan = no
           v_cod_proj_financ_excec:visible in frame f_dlg_03_sdo_cta_ctbl_balan = no
           v_log_proj_financ:visible in frame f_dlg_03_sdo_cta_ctbl_balan       = no.
END.
else do:       
    &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
        find last emsuni.param_geral_ems no-lock no-error.
        if  available emsuni.param_geral_ems
        then do:
            assign v_cod_format_proj_financ = emsuni.param_geral_ems.cod_format_proj_financ
                   v_cod_proj_financ_pfixa:format in frame f_dlg_03_sdo_cta_ctbl_balan = emsuni.param_geral_ems.cod_format_proj_financ
                   v_cod_proj_financ_excec:format in frame f_dlg_03_sdo_cta_ctbl_balan = emsuni.param_geral_ems.cod_format_proj_financ.
        end.
    &ELSE
        assign v_cod_proj_financ_pfixa:visible in frame f_dlg_03_sdo_cta_ctbl_balan = no
               v_cod_proj_financ_excec:visible in frame f_dlg_03_sdo_cta_ctbl_balan = no
               v_log_proj_financ:visible in frame f_dlg_03_sdo_cta_ctbl_balan       = no.
    &ENDIF
end.

if  v_log_balanct_param = yes
then do:  /* --- Mais ParÉmetros ---*/
    run pi_sdo_cta_ctbl_balanct_param /*pi_sdo_cta_ctbl_balanct_param*/.
end /* if */.
else do:  /* --- Gera Arquivo Planilha ---*/
    run pi_sdo_cta_ctbl_balanct_planilha /*pi_sdo_cta_ctbl_balanct_planilha*/.
end /* else */.


/* Begin_Include: i_log_exec_prog_dtsul_ini */
assign v_rec_log = ?.

if can-find(prog_dtsul
       where prog_dtsul.cod_prog_dtsul = 'fnc_sdo_cta_ctbl_balan' 
         and prog_dtsul.log_gera_log_exec = yes) then do transaction:
    create log_exec_prog_dtsul.
    assign log_exec_prog_dtsul.cod_prog_dtsul           = 'fnc_sdo_cta_ctbl_balan'
           log_exec_prog_dtsul.cod_usuario              = v_cod_usuar_corren
           log_exec_prog_dtsul.dat_inic_exec_prog_dtsul = today
           log_exec_prog_dtsul.hra_inic_exec_prog_dtsul = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":","").
    assign v_rec_log = recid(log_exec_prog_dtsul).
    release log_exec_prog_dtsul no-error.
end.

/* End_Include: i_log_exec_prog_dtsul_ini */

return.


/******************************* Main Code End ******************************/

/************************* Internal Procedure Begin *************************/

/*****************************************************************************
** Procedure Interna.....: pi_vld_fnc_sdo_cta_ctbl_balan
** Descricao.............: pi_vld_fnc_sdo_cta_ctbl_balan
** Criado por............: Henke
** Criado em.............: 22/07/1996 17:42:11
** Alterado por..........: Augusto Guimar∆es    
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_vld_fnc_sdo_cta_ctbl_balan:

    if  v_log_balanct_param = yes
    then do:
        /* --- Finalidade Econìmica Base ---*/
        find finalid_econ no-lock
             where finalid_econ.cod_finalid_econ = input frame f_dlg_03_sdo_cta_ctbl_balan v_cod_finalid_econ_bas
              no-error.
        if  not avail finalid_econ
        then do:
            assign v_wgh_focus = v_cod_finalid_econ_bas:handle in frame f_dlg_03_sdo_cta_ctbl_balan.
            /* Finalidade Econìmica Inexistente ! */
            run pi_messages (input "show",
                             input 75,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_75*/.
            return error.
        end /* if */.
        run pi_validar_finalid_unid_organ (Input input frame f_dlg_03_sdo_cta_ctbl_balan v_cod_finalid_econ_bas,
                                           Input v_cod_empres_usuar,
                                           Input v_dat_cotac_indic_econ,
                                           output v_cod_return) /*pi_validar_finalid_unid_organ*/.
        if  v_cod_return = "NOK" /*l_nok*/ 
        then do:
            assign v_wgh_focus = v_cod_finalid_econ_bas:handle in frame f_dlg_03_sdo_cta_ctbl_balan.
            /* Finalidade Econìmica corrente n∆o econtrada para U.O ! */
            run pi_messages (input "show",
                             input 1192,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_1192*/.
            return error.
        end /* if */.

        /* --- Finalidade Econìmica de Apresentaá∆o ---*/
        find b_finalid_econ no-lock
             where b_finalid_econ.cod_finalid_econ = input frame f_dlg_03_sdo_cta_ctbl_balan v_cod_finalid_econ_apr
              no-error.
        if  not avail b_finalid_econ
        then do:
            assign v_wgh_focus = v_cod_finalid_econ_apr:handle in frame f_dlg_03_sdo_cta_ctbl_balan.
            /* Finalidade Econìmica Inexistente ! */
            run pi_messages (input "show",
                             input 75,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_75*/.
            return error.
        end /* if */.

        assign input frame f_dlg_03_sdo_cta_ctbl_balan v_cod_finalid_econ_bas
               input frame f_dlg_03_sdo_cta_ctbl_balan v_cod_finalid_econ_apr
               input frame f_dlg_03_sdo_cta_ctbl_balan v_log_cta_ctbl_internac
               input frame f_dlg_03_sdo_cta_ctbl_balan v_log_cta_ctbl_sdo
               input frame f_dlg_03_sdo_cta_ctbl_balan v_log_cta_ctbl_analit
               input frame f_dlg_03_sdo_cta_ctbl_balan v_log_estab_sum
               input frame f_dlg_03_sdo_cta_ctbl_balan v_log_ccusto_sum
               input frame f_dlg_03_sdo_cta_ctbl_balan v_log_unid_negoc_sum
               input frame f_dlg_03_sdo_cta_ctbl_balan v_log_period_ctbl_ant_impr
               input frame f_dlg_03_sdo_cta_ctbl_balan v_log_consid_apurac_restdo
               input frame f_dlg_03_sdo_cta_ctbl_balan v_dat_cotac_indic_econ
               input frame f_dlg_03_sdo_cta_ctbl_balan v_num_niv_estrut
               input frame f_dlg_03_sdo_cta_ctbl_balan v_cod_idioma_apr
               input frame f_dlg_03_sdo_cta_ctbl_balan v_cod_cta_ctbl_pfixa
               input frame f_dlg_03_sdo_cta_ctbl_balan v_cod_cta_ctbl_excec
               input frame f_dlg_03_sdo_cta_ctbl_balan v_cod_ccusto_pfixa
               input frame f_dlg_03_sdo_cta_ctbl_balan v_cod_ccusto_excec
               input frame f_dlg_03_sdo_cta_ctbl_balan v_log_mostra_sem_aprop_cc.

        &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
            if  v_log_gerencial = no /* Se N∆o foi chamado do Balancete Gerencial */ then DO:
                assign input frame f_dlg_03_sdo_cta_ctbl_balan v_log_proj_financ
                       input frame f_dlg_03_sdo_cta_ctbl_balan v_cod_proj_financ_pfixa
                       input frame f_dlg_03_sdo_cta_ctbl_balan v_cod_proj_financ_excec.
            end.
        &ENDIF
    end /* if */.
    else do:
       /* --- Validar Arquivo p/ Planilha ---*/
       assign /*input frame f_dlg_03_gerac_planilha v_log_gerac_planilha*/
              v_cod_arq_planilha = replace(input frame f_dlg_03_gerac_planilha v_cod_arq_planilha, "~\", "~/").
       /*if  v_log_gerac_planilha = yes
       then do:*/
          find dwb_rpt_param
              where recid(dwb_rpt_param) = v_rec_dwb_rpt_param no-lock no-error.
          if  dwb_rpt_param.ind_dwb_run_mode = "Batch" /*l_batch*/ 
          then do:
              assign v_cod_arq_planilha = replace(v_cod_arq_planilha, "~\", "~/").
             /* if  index(v_cod_arq_planilha, ":") <> 0
              then do:
                  /* Nome de arquivo com problemas. */
                  run pi_messages (input "show",
                                   input 1979,
                                   input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_1979*/.
                  return.
              end /* if */.
              
              if  length(entry(2, v_cod_arq_planilha, ".")) > 3
              then do:
                  assign v_wgh_focus = v_cod_arq_planilha:handle in frame f_dlg_03_gerac_planilha.
                  /* Nome de arquivo com problemas. */
                  run pi_messages (input "show",
                                   input 1979,
                                   input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_1979*/.
                  return error.
              end /* if */.
              */
          end /* if */.
          else do:
              if  v_cod_arq_planilha = ""
              then do:
                  /* &1 n∆o pode ser igual a branco ! */
                  run pi_messages (input "show",
                                   input 3703,
                                   input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                      "O Arq Externo Planilha de C†lculo" /*l_arq_ext_plani_cal*/)) /*msg_3703*/.
                  return error.
              end /* if */.
          end /* else */.
       /*end*/ /* if */.
       /* --- Validar Caracter Delimitador ---*/
       /*if  input frame f_dlg_03_gerac_planilha v_cod_carac_lim = "" /*and
           v_log_gerac_planilha = yes*/
       then do:
           assign v_wgh_focus = v_cod_carac_lim:handle in frame f_dlg_03_gerac_planilha.
           /* &1 n∆o pode ser igual a branco ! */
           run pi_messages (input "show",
                            input 3703,
                            input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                               "O Caracter Delimitador" /*l_caracter_delimitador*/)) /*msg_3703*/.
           return error.
       end /* if */.
       assign input frame f_dlg_03_gerac_planilha v_cod_carac_lim.*/
    end /* else */.
END PROCEDURE. /* pi_vld_fnc_sdo_cta_ctbl_balan */
/*****************************************************************************
** Procedure Interna.....: pi_validar_finalid_unid_organ
** Descricao.............: pi_validar_finalid_unid_organ
** Criado por............: 
** Criado em.............: // 
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_validar_finalid_unid_organ:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_finalid_econ
        as character
        format "x(10)"
        no-undo.
    def Input param p_cod_unid_organ
        as character
        format "x(3)"
        no-undo.
    def Input param p_dat_transacao
        as date
        format "99/99/9999"
        no-undo.
    def output param p_cod_return
        as character
        format "x(40)"
        no-undo.


    /************************* Parameter Definition End *************************/

    find first finalid_unid_organ no-lock
         where finalid_unid_organ.cod_unid_organ = p_cod_unid_organ
           and finalid_unid_organ.cod_finalid_econ = p_cod_finalid_econ
           and finalid_unid_organ.dat_inic_valid <= p_dat_transacao
           and finalid_unid_organ.dat_fim_valid > p_dat_transacao /*cl_param_unid_organ of finalid_unid_organ*/ no-error.
    if  not avail finalid_unid_organ
    then do:
        assign p_cod_return = "338".
    end /* if */.
    else do:
        assign p_cod_return = "OK" /*l_ok*/ .
    end /* else */.
END PROCEDURE. /* pi_validar_finalid_unid_organ */
/*****************************************************************************
** Procedure Interna.....: pi_sdo_cta_ctbl_balanct_param
** Descricao.............: pi_sdo_cta_ctbl_balanct_param
** Criado por............: Henke
** Criado em.............: 13/11/1996 08:18:42
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_sdo_cta_ctbl_balanct_param:

    /************************* Variable Definition Begin ************************/

    def var v_hdl_grp
        as Handle
        format ">>>>>>9":U
        no-undo.
    def var v_hdl_tmp
        as Handle
        format ">>>>>>9":U
        no-undo.
    def var v_log_saida
        as logical
        format "Sim/N∆o"
        initial no
        no-undo.


    /************************** Variable Definition End *************************/

    assign v_cod_finalid_econ_base_aux   = v_cod_finalid_econ_bas
           v_cod_finalid_econ_aprop_aux  = v_cod_finalid_econ_apr
           v_log_cta_ctbl_internac_aux   = v_log_cta_ctbl_internac
           v_log_cta_ctbl_sdo_aux        = v_log_cta_ctbl_sdo
           v_log_cta_ctbl_analit_aux     = v_log_cta_ctbl_analit
           v_log_estab_aux               = v_log_estab_sum
           v_log_ccusto_aux              = v_log_ccusto_sum
           v_log_mostra_sem_aprop_cc_aux = v_log_mostra_sem_aprop_cc
           v_log_unid_negoc_aux          = v_log_unid_negoc_sum
           v_log_period_ctbl_ant_aux     = v_log_period_ctbl_ant_impr
           v_log_consid_apurac_aux       = v_log_consid_apurac_restdo
           v_dat_cotac_indic_econ_aux    = v_dat_cotac_indic_econ
           v_num_niv_estrut_aux          = v_num_niv_estrut
           v_cod_idiom_apres_aux         = v_cod_idioma_apr
           v_wgh_focus                   = v_log_ccusto_sum:handle in frame f_dlg_03_sdo_cta_ctbl_balan.

    &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
        assign v_log_proj_financ_aux = v_log_proj_financ.
    &ENDIF

    pause 0 before-hide.
    view frame f_dlg_03_sdo_cta_ctbl_balan.

    /* --- Projeto ---*/
    &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
        if  v_log_gerencial = no /* Se n∆o foi chamado do Balancete Gerencial */ then DO:
            if  v_cod_format_proj_financ <> ""
            then do:
                run pi_formatar_pfixa (Input v_cod_proj_financ_pfixa:handle in frame f_dlg_03_sdo_cta_ctbl_balan,
                                       Input v_cod_format_proj_financ) /*pi_formatar_pfixa*/.
                run pi_formatar_pfixa (Input v_cod_proj_financ_excec:handle in frame f_dlg_03_sdo_cta_ctbl_balan,
                                       Input v_cod_format_proj_financ) /*pi_formatar_pfixa*/.
            end /* if */.
            else
                assign v_cod_proj_financ_pfixa:format = "x(20)":U
                       v_cod_proj_financ_excec:format = "x(20)":U
                       v_cod_proj_financ_pfixa = ""
                       v_cod_proj_financ_excec = "".
        end.
    &ENDIF

    /* --- Conta Cont†bil ---*/
    find plano_cta_ctbl no-lock
         where plano_cta_ctbl.cod_plano_cta_ctbl  = v_cod_plano_cta_ctbl no-error.
    if  avail plano_cta_ctbl
    then do:
        run pi_formatar_pfixa (Input v_cod_cta_ctbl_pfixa:handle in frame f_dlg_03_sdo_cta_ctbl_balan,
                               Input plano_cta_ctbl.cod_format_cta_ctbl) /*pi_formatar_pfixa*/.
        run pi_formatar_pfixa (Input v_cod_cta_ctbl_excec:handle in frame f_dlg_03_sdo_cta_ctbl_balan,
                               Input plano_cta_ctbl.cod_format_cta_ctbl) /*pi_formatar_pfixa*/.
    end /* if */.
    else do:
        assign v_cod_cta_ctbl_pfixa:format = "x(20)":U
               v_cod_cta_ctbl_excec:format = "x(20)":U
               v_cod_cta_ctbl_pfixa = ""
               v_cod_cta_ctbl_excec = "".
    end /* else */.
    /* --- Unid Organ ---*/
    find unid_organ no-lock
        where unid_organ.cod_unid_organ = v_cod_unid_organ no-error.
    if  avail unid_organ
    then do:
        /* --- Centro de Custo ---*/
        find plano_ccusto no-lock
            where plano_ccusto.cod_empresa      = unid_organ.cod_unid_organ
              and plano_ccusto.cod_plano_ccusto = v_cod_plano_ccusto no-error.
        if  avail plano_ccusto
        then do:
            run pi_formatar_pfixa (Input v_cod_ccusto_pfixa:handle in frame f_dlg_03_sdo_cta_ctbl_balan,
                                   Input plano_ccusto.cod_format_ccusto) /*pi_formatar_pfixa*/.
            run pi_formatar_pfixa (Input v_cod_ccusto_excec:handle in frame f_dlg_03_sdo_cta_ctbl_balan,
                                   Input plano_ccusto.cod_format_ccusto) /*pi_formatar_pfixa*/.
        end /* if */.
    end /* if */.
    &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
        if  v_log_gerencial  = no /* Se n∆o foi chamado do Balancete Gerencial */ then DO:
            if  v_cod_proj_financ = v_cod_proj_financ_ant
            then do:
                display v_cod_proj_financ_pfixa
                        v_cod_proj_financ_excec
                        with frame f_dlg_03_sdo_cta_ctbl_balan.
            end /* if */.
        end.
    &ENDIF
    if  v_cod_plano_cta_ctbl = v_cod_plano_cta_ctbl_ant
    then do:
        display v_cod_cta_ctbl_pfixa
                v_cod_cta_ctbl_excec
                with frame f_dlg_03_sdo_cta_ctbl_balan.
    end /* if */.
    if  v_cod_plano_ccusto = v_cod_plano_ccusto_ant
    then do:
        display v_cod_ccusto_pfixa
                v_cod_ccusto_excec
                with frame f_dlg_03_sdo_cta_ctbl_balan.
    end /* if */.
    assign v_cod_plano_cta_ctbl_ant = v_cod_plano_cta_ctbl
           v_cod_plano_ccusto_ant   = v_cod_plano_ccusto.
    &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
        assign v_cod_proj_financ_ant = v_cod_proj_financ.
    &ENDIF

    main_block:
    do on endkey undo main_block, leave main_block on error undo main_block, retry main_block:

        if  v_log_gerencial = no /* Se n∆o foi chamado do Balancete Gerencial */ then DO:
            &IF DEFINED(BF_ADM_FIN_PROJ)  &THEN
                enable all with frame f_dlg_03_sdo_cta_ctbl_balan.
            &ELSE
                assign v_cod_proj_financ_pfixa:visible in frame f_dlg_03_sdo_cta_ctbl_balan = no
                       v_cod_proj_financ_excec:visible in frame f_dlg_03_sdo_cta_ctbl_balan = no
                       v_log_proj_financ:visible in frame f_dlg_03_sdo_cta_ctbl_balan       = no.

                enable all 
                except v_log_proj_financ
                       v_cod_proj_financ_pfixa
                       v_cod_proj_financ_excec
                with frame f_dlg_03_sdo_cta_ctbl_balan.

                assign rt_007:visible in frame f_dlg_03_sdo_cta_ctbl_balan = no.

                assign v_hdl_grp = frame f_dlg_03_sdo_cta_ctbl_balan:first-child.
                do  while v_hdl_grp <> ?:
                    assign v_hdl_tmp = v_hdl_grp:first-child.
                    do  while v_hdl_tmp <> ?:
                        if  v_hdl_tmp:row = 12.25 and
                            v_hdl_tmp:col = 42.29 then do:
                            v_hdl_tmp:screen-value = "".
                            assign v_log_saida = yes.
                            leave.
                        end.
                        assign v_hdl_tmp = v_hdl_tmp:next-sibling.
                    end.
                    if  v_log_saida = yes then
                        leave.
                    assign v_hdl_grp = v_hdl_grp:next-sibling.
                end.
            &ENDIF
         end.
         else do:
            assign v_cod_proj_financ_pfixa:visible in frame f_dlg_03_sdo_cta_ctbl_balan = no
                   v_cod_proj_financ_excec:visible in frame f_dlg_03_sdo_cta_ctbl_balan = no
                   v_log_proj_financ:visible in frame f_dlg_03_sdo_cta_ctbl_balan       = no.

            enable all 
            except v_log_proj_financ
                   v_cod_proj_financ_pfixa
                   v_cod_proj_financ_excec
            with frame f_dlg_03_sdo_cta_ctbl_balan.

            assign rt_007:visible in frame f_dlg_03_sdo_cta_ctbl_balan = no.

            assign v_hdl_grp = frame f_dlg_03_sdo_cta_ctbl_balan:first-child.
            do  while v_hdl_grp <> ?:
                assign v_hdl_tmp = v_hdl_grp:first-child.
                do  while v_hdl_tmp <> ?:
                    if  v_hdl_tmp:row = 12.25 and
                        v_hdl_tmp:col = 42.29 then do:
                        v_hdl_tmp:screen-value = "".
                        assign v_log_saida = yes.
                        leave.
                    end.
                    assign v_hdl_tmp = v_hdl_tmp:next-sibling.
                end.
                if  v_log_saida = yes then
                    leave.
                assign v_hdl_grp = v_hdl_grp:next-sibling.
            end.
         end.

        /* --- Verifica a ordem de classificaá∆o ---*/
        find dwb_rpt_param where recid(dwb_rpt_param) = v_rec_dwb_rpt_param no-lock no-error.
        if  avail dwb_rpt_param
        then do:
            if  entry(1,dwb_rpt_param.cod_dwb_order) = "Estrutura" /*l_estrutura*/ 
            then do:
                enable v_num_niv_estrut
                       with frame f_dlg_03_sdo_cta_ctbl_balan.
                disable v_log_cta_ctbl_analit
                        v_cod_cta_ctbl_pfixa
                        v_cod_cta_ctbl_excec
                        with frame f_dlg_03_sdo_cta_ctbl_balan.
                assign v_log_cta_ctbl_analit = no.
                if  avail plano_cta_ctbl then
                    assign v_cod_cta_ctbl_pfixa = fill("#", length(plano_cta_ctbl.cod_format_cta_ctbl))
                           v_cod_cta_ctbl_excec = fill("#", length(plano_cta_ctbl.cod_format_cta_ctbl)).
            end /* if */.
            else do:
                disable v_num_niv_estrut
                        with frame f_dlg_03_sdo_cta_ctbl_balan.
                assign v_num_niv_estrut = 999.
            end /* else */.
        end /* if */.
        if  avail unid_organ 
        and unid_organ.num_niv_unid_organ <> 998
        or  not avail plano_ccusto
        then do:
            assign v_cod_ccusto_pfixa = fill("#", 11)
                   v_cod_ccusto_excec = fill("#", 11).
            disable v_cod_ccusto_pfixa
                    v_cod_ccusto_excec
                    with frame f_dlg_03_sdo_cta_ctbl_balan.
        end /* if */.
        if  v_cod_finalid_econ_bas = v_cod_finalid_econ_apr
        then do:
            disable v_dat_cotac_indic_econ
                    with frame f_dlg_03_sdo_cta_ctbl_balan.
        end /* if */.
        display v_cod_finalid_econ_apr
                v_cod_finalid_econ_bas
                v_cod_idioma_apr
                v_dat_cotac_indic_econ
                v_log_ccusto_sum
                v_log_consid_apurac_restdo
                v_log_cta_ctbl_analit
                v_log_cta_ctbl_internac
                v_log_cta_ctbl_sdo
                v_log_estab_sum
                v_log_period_ctbl_ant_impr
                v_log_unid_negoc_sum
                v_num_niv_estrut
                v_log_mostra_sem_aprop_cc
                with frame f_dlg_03_sdo_cta_ctbl_balan.
        &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
            if  v_log_gerencial = no /* Se n∆o foi chamado do Balancete Gerencial */ then DO:
                display v_log_proj_financ
                        with frame f_dlg_03_sdo_cta_ctbl_balan.
            end.
        &ENDIF
        apply "value-changed" to v_log_ccusto_sum.

        if  valid-handle(v_wgh_focus) then
            wait-for go of frame f_dlg_03_sdo_cta_ctbl_balan focus v_wgh_focus.
        else
            wait-for go of frame f_dlg_03_sdo_cta_ctbl_balan focus bt_ok.

        run pi_vld_fnc_sdo_cta_ctbl_balan /*pi_vld_fnc_sdo_cta_ctbl_balan*/.
    end /* do main_block */.

    assign v_wgh_focus = ?.
    hide frame f_dlg_03_sdo_cta_ctbl_balan.
END PROCEDURE. /* pi_sdo_cta_ctbl_balanct_param */
/*****************************************************************************
** Procedure Interna.....: pi_sdo_cta_ctbl_balanct_planilha
** Descricao.............: pi_sdo_cta_ctbl_balanct_planilha
** Criado por............: Henke
** Criado em.............: 13/11/1996 08:19:24
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_sdo_cta_ctbl_balanct_planilha:

    assign /*v_log_gerac_planilha_aux = v_log_gerac_planilha*/
           v_cod_arq_planilha_aux   = v_cod_arq_planilha
           /*v_cod_carac_lim_aux      = v_cod_carac_lim*/
           /*v_wgh_focus = v_log_gerac_planilha:handle in frame f_dlg_03_gerac_planilha*/.

    pause 0 before-hide.
    display bt_can
            bt_hel2
            bt_ok
            v_cod_arq_planilha
            /*v_cod_carac_lim*/
            /*v_log_gerac_planilha*/
            with frame f_dlg_03_gerac_planilha.
    enable all with frame f_dlg_03_gerac_planilha.
    /*apply "value-changed" to v_log_gerac_planilha in frame f_dlg_03_gerac_planilha.*/

    planilha_block:
    do on endkey undo planilha_block, leave planilha_block on error undo planilha_block, retry planilha_block.
        if  valid-handle(v_wgh_focus)
        then do:
            wait-for go of frame f_dlg_03_gerac_planilha focus v_wgh_focus.
        end /* if */.
        else do:
            wait-for go of frame f_dlg_03_gerac_planilha focus bt_ok.
        end /* else */.
        run pi_vld_fnc_sdo_cta_ctbl_balan /*pi_vld_fnc_sdo_cta_ctbl_balan*/.
    end /* do planilha_block */.

    assign v_wgh_focus = ?.
    hide frame f_dlg_03_gerac_planilha.
END PROCEDURE. /* pi_sdo_cta_ctbl_balanct_planilha */
/*****************************************************************************
** Procedure Interna.....: pi_formatar_pfixa
** Descricao.............: pi_formatar_pfixa
** Criado por............: vanei
** Criado em.............: 21/12/1995 08:48:21
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_formatar_pfixa:

    /************************ Parameter Definition Begin ************************/

    def Input param p_wgh_focus
        as widget-handle
        format ">>>>>>9"
        no-undo.
    def Input param p_cod_format
        as character
        format "x(8)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_cod_attrib                     as character       no-undo. /*local*/
    def var v_cod_format                     as character       no-undo. /*local*/
    def var v_cod_inicial                    as character       no-undo. /*local*/
    def var v_num_count                      as integer         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    assign v_cod_format   = ""
           v_cod_inicial  = "".

    conta_block:
    do v_num_count = 1 to length(p_cod_format):
        assign v_cod_attrib = substring(p_cod_format, v_num_count, 1).
        if  v_cod_attrib = 'X'
        or  v_cod_attrib = "!"
        or  v_cod_attrib = "9"
        then do:
            assign v_cod_inicial  = v_cod_inicial + "#"
                   v_cod_format   = v_cod_format  + 'X'.
        end /* if */.
        else do:
            if  v_cod_attrib = "-"
            or  v_cod_attrib = "."
            then do:
                assign v_cod_format = v_cod_format + v_cod_attrib.
            end /* if */.
        end /* else */.
    end /* do conta_block */.

    if  valid-handle(p_wgh_focus)
    then do:
        assign p_wgh_focus:format       = v_cod_format
               p_wgh_focus:screen-value = v_cod_inicial.
    end /* if */.

    return v_cod_format.
END PROCEDURE. /* pi_formatar_pfixa */
/*****************************************************************************
** Procedure Interna.....: pi_version_extract
** Descricao.............: pi_version_extract
** Criado por............: jaison
** Criado em.............: 31/07/1998 09:33:22
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
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

    if  can-do(v_cod_tip_prog, p_cod_program_type)
    then do:
        if p_cod_program_type = 'dic' then 
           assign p_cod_program_ext = replace(p_cod_program_ext, 'database/', '').

        /*output stream s-arq to value(v_cod_arq) append.

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
            find tab_dic_dtsul 
                where tab_dic_dtsul.cod_tab_dic_dtsul = p_cod_program 
                no-lock no-error.
            if  avail tab_dic_dtsul
            then do:
                &if '{&emsbas_version}' > '5.00' &then
                    if  tab_dic_dtsul.nom_prog_dpc_gat_delete <> '' then
                        put stream s-arq 'DPC-DELETE : ' at 5 tab_dic_dtsul.nom_prog_dpc_gat_delete  at 25 skip.
                &endif
                if  tab_dic_dtsul.nom_prog_appc_gat_delete <> '' then
                    put stream s-arq 'APPC-DELETE: ' at 5 tab_dic_dtsul.nom_prog_appc_gat_delete at 25 skip.
                if  tab_dic_dtsul.nom_prog_upc_gat_delete <> '' then
                    put stream s-arq 'UPC-DELETE : ' at 5 tab_dic_dtsul.nom_prog_upc_gat_delete  at 25 skip.
                &if '{&emsbas_version}' > '5.00' &then
                    if  tab_dic_dtsul.nom_prog_dpc_gat_write <> '' then
                        put stream s-arq 'DPC-WRITE : ' at 5 tab_dic_dtsul.nom_prog_dpc_gat_write  at 25 skip.
                &endif
                if  tab_dic_dtsul.nom_prog_appc_gat_write <> '' then
                    put stream s-arq 'APPC-WRITE: ' at 5 tab_dic_dtsul.nom_prog_appc_gat_write at 25 skip.
                if  tab_dic_dtsul.nom_prog_upc_gat_write <> '' then
                    put stream s-arq 'UPC-WRITE : ' at 5 tab_dic_dtsul.nom_prog_upc_gat_write  at 25 skip.
            end /* if */.
            &endif
        end.

        output stream s-arq close.*/
    end /* if */.

END PROCEDURE. /* pi_version_extract */


/************************** Internal Procedure End **************************/

/************************* External Procedure Begin *************************/



/************************** External Procedure End **************************/
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
/**********************  End of fnc_sdo_cta_ctbl_balan **********************/



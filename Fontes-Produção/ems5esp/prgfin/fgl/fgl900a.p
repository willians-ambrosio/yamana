/*****************************************************************************
** Copyright KRAFT CONSULTING
** Todos os Direitos Reservados.
** 
** Este fonte e de propriedade exclusiva da KRAFT CONSULTING, sua reproducao
** parcial ou total por qualquer meio, so' podera ser feita mediante
** autorizacao expressa.
**
** Programa..............: rpt_sdo_cta_ctbl_balanct
** Descricao.............: Relat¢rio Balencete Cont†bil
** Versao................:  1.00.01.076
** Procedimento..........: rel_balanct_ctbl
** Nome Externo..........: prgfin/fgl/fgl900a.p
** Data Geracao..........: 15/03/2011
** Criado por............: Augusto Guimar∆es
*****************************************************************************/
def buffer unid_organ           for ems5.unid_organ.
def buffer histor_exec_especial for ems5.histor_exec_especial.

def var c-versao-prg as char initial " 1.00.01.076":U no-undo.

{include/i_dbinst.i}
{include/i_dbtype.i}
{include/i_fcldef.i}


&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i rpt_sdo_cta_ctbl_balanct FGL}
&ENDIF

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
                                    "RPT_SDO_CTA_CTBL_BALANCT","~~EMSFIN", "~~{~&emsfin_version}", "~~1.00")) /*msg_5009*/.
&else

/************************** Buffer Definition Begin *************************/

&if "{&emsbas_version}" >= "1.00" &then
def buffer b_ped_exec_style
    for ped_exec.
&endif
&if "{&emsuni_version}" >= "1.00" &then
def buffer b_period_ctbl
    for period_ctbl.
&endif
&if "{&emsbas_version}" >= "1.00" &then
def buffer b_servid_exec
    for servid_exec.
&endif
&if "{&emsbas_version}" >= "1.00" &then
def buffer b_servid_exec_style
    for servid_exec.
&endif


/*************************** Buffer Definition End **************************/

/************************** Stream Definition Begin *************************/

def new shared stream s_1.

OUTPUT STREAM s_1 TO "CLIPBOARD".


/*************************** Stream Definition End **************************/

/************************* Variable Definition Begin ************************/

def new global shared var v_cod_aplicat_dtsul_corren
    as character
    format "x(3)":U
    no-undo.
def var v_cod_armaz_unid
    as character
    format "x(3)":U
    no-undo.
def new shared var v_cod_arq_planilha
    as character
    format "x(40)":U
    label "Arq Planilha"
    column-label "Arq Planilha"
    no-undo.
def new shared var v_cod_carac_lim
    as character
    format "x(1)":U
    initial ";"
    label "Caracter Delimitador"
    no-undo.
def var v_cod_ccusto
    as Character
    format "x(11)":U
    label "Centro Custo"
    column-label "Centro Custo"
    no-undo.
def new global shared var v_cod_ccusto_corren
    as character
    format "x(11)":U
    label "Centro Custo"
    column-label "Centro Custo"
    no-undo.
def new shared var v_cod_ccusto_excec
    as character
    format "x(11)":U
    initial "..........."
    label "Parte Exceá∆o"
    column-label "Parte Exceá∆o"
    no-undo.
def var v_cod_ccusto_fim_aux
    as character
    format "x(11)":U
    no-undo.
def var v_cod_ccusto_inic_aux
    as character
    format "x(11)":U
    no-undo.
def new shared var v_cod_ccusto_pfixa
    as character
    format "x(11)":U
    label "Parte Fixa CCusto"
    column-label "Parte Fixa CCusto"
    no-undo.
def new shared var v_cod_cenar_ctbl
    as character
    format "x(8)":U
    label "Cen†rio Cont†bil"
    column-label "Cen†rio Cont†bil"
    no-undo.
def new shared var v_cod_cta_ctbl_excec
    as character
    format "x(20)":U
    initial "...................."
    label "Parte Exceá∆o"
    column-label "Parte Exceá∆o"
    no-undo.
def var v_cod_cta_ctbl_pai_final
    as character
    format "x(20)":U
    initial "ZZZZZZZZZZZZZZZZZZZZ"
    label "atÇ"
    column-label "atÇ"
    no-undo.
def var v_cod_cta_ctbl_pai_inicial
    as character
    format "x(20)":U
    label "Conta Cont†bil"
    column-label "Conta Cont†bil"
    no-undo.
def new shared var v_cod_cta_ctbl_pfixa
    as character
    format "x(20)":U
    label "Parte Fixa"
    column-label "Parte Fixa Cta Ctbl"
    no-undo.
def new shared var v_cod_dat_type
    as character
    format "x(8)":U
    no-undo.
def var v_cod_dwb_field
    as character
    format "x(32)":U
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
def var v_cod_dwb_order
    as character
    format "x(32)":U
    label "Classificaá∆o"
    column-label "Classificador"
    no-undo.
def new shared var v_cod_dwb_output
    as character
    format "x(8)":U
    label "Sa°da"
    column-label "Sa°da"
    no-undo.
def var v_cod_dwb_parameters
    as character
    format "x(8)":U
    no-undo.
def new shared var v_cod_dwb_print_layout
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
def new shared var v_cod_dwb_select
    as character
    format "x(32)":U
    no-undo.
def new global shared var v_cod_dwb_user
    as character
    format "x(21)":U
    label "Usu†rio"
    column-label "Usu†rio"
    no-undo.
def new global shared var v_cod_empresa
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.
def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.
def var v_cod_erro
    as character
    format "x(10)":U
    column-label "Cod Erro"
    no-undo.
def new global shared var v_cod_estab_usuar
    as character
    format "x(3)":U
    label "Estabelecimento"
    column-label "Estab"
    no-undo.
def new shared var v_cod_exerc_ctbl
    as character
    format "9999":U
    label "Exerc°cio Cont†bil"
    column-label "Exerc°cio Cont†bil"
    no-undo.
def new shared var v_cod_final
    as character
    format "x(8)":U
    initial ?
    label "Final"
    no-undo.
def new shared var v_cod_finalid_econ_apr
    as character
    format "x(10)":U
    label "Finalid Converte"
    column-label "Finalid Conv"
    no-undo.
def new shared var v_cod_finalid_econ_bas
    as character
    format "x(10)":U
    label "Finalidade Base"
    column-label "Finalidade Base"
    no-undo.
def new shared var v_cod_format
    as character
    format "x(8)":U
    label "Formato"
    column-label "Formato"
    no-undo.
def new shared var v_cod_funcao_imprsor
    as character
    format "x(20)":U
    label "Funá∆o Configur"
    column-label "Funá∆o Configuraá∆o"
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
def new shared var v_cod_idioma_apr
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
def new shared var v_cod_initial
    as character
    format "x(8)":U
    initial ?
    label "Inicial"
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
def new shared var v_cod_opc_funcao_imprsor
    as character
    format "x(30)":U
    view-as combo-box
    list-items ""
    inner-lines 6
    bgcolor 15 font 2
    label "Opá∆o Configuraá∆o"
    no-undo.
def new global shared var v_cod_order
    as character
    format "x(40)":U
    no-undo.
def new global shared var v_cod_pais_empres_usuar
    as character
    format "x(3)":U
    label "Pa°s ems5.empresa Usu†rio"
    column-label "Pa°s"
    no-undo.
def var v_cod_plano_antes
    as character
    format "x(20)":U
    no-undo.
def new shared var v_cod_plano_ccusto
    as character
    format "x(8)":U
    label "Plano CCusto"
    column-label "Plano CCusto"
    no-undo.
def new shared var v_cod_plano_ccusto_ant
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
def new shared var v_cod_plano_cta_ctbl
    as character
    format "x(8)":U
    label "Plano Contas"
    column-label "Plano Contas"
    no-undo.
def new shared var v_cod_plano_cta_ctbl_ant
    as character
    format "x(8)":U
    label "Plano Contas"
    column-label "Plano Contas"
    no-undo.
def new shared var v_cod_plano_cta_ctbl_old
    as character
    format "x(8)":U
    label "Plano antigo"
    column-label "Plano antigo"
    no-undo.
def new shared var v_cod_proj_financ
    as character
    format "x(20)":U
    label "Projeto"
    column-label "Projeto"
    no-undo.
def new shared var v_cod_proj_financ_ant
    as character
    format "x(20)":U
    no-undo.
def new shared var v_cod_proj_financ_excec
    as character
    format "x(20)":U
    initial "...................."
    label "Exceá∆o"
    column-label "Exceá∆o"
    no-undo.
def new shared var v_cod_proj_financ_pfixa
    as character
    format "x(20)":U
    label "Parte Fixa"
    no-undo.
def new shared var v_cod_release
    as character
    format "x(12)":U
    no-undo.
def new shared var v_cod_tempo_exec
    as character
    format "x(8)":U
    label "Tempo Execuá∆o"
    column-label "Tempo"
    no-undo.
def new shared var v_cod_tip_impres
    as character
    format "x(10)":U
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
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
def new shared var v_cod_unid_organ
    as character
    format "x(3)":U
    label "Unid Organizacional"
    column-label "Unid Organizacional"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
def new shared var v_cod_unid_organ
    as Character
    format "x(5)":U
    label "Unid Organizacional"
    column-label "Unid Organizacional"
    no-undo.
&ENDIF
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
def new shared var v_dat_cotac_indic_econ
    as date
    format "99/99/9999":U
    initial today
    label "Data Cotaá∆o"
    column-label "Data Cotaá∆o"
    no-undo.
def new shared var v_dat_execution
    as date
    format "99/99/9999":U
    no-undo.
def new shared var v_dat_execution_end
    as date
    format "99/99/9999":U
    no-undo.
def new shared var v_dat_fim_period
    as date
    format "99/99/9999":U
    label "Fim Per°odo"
    no-undo.
def new global shared var v_dat_fim_period_ctbl
    as date
    format "99/99/9999":U
    initial 12/31/9999
    label "Fim"
    column-label "Fim"
    no-undo.
def new shared var v_dat_inic_period
    as date
    format "99/99/9999":U
    label "In°cio Per°odo"
    column-label "Per°odo"
    no-undo.
def new global shared var v_dat_inic_period_ctbl
    as date
    format "99/99/9999":U
    initial &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 01/01/0001 &ENDIF
    label "In°cio"
    column-label "In°cio"
    no-undo.
def new shared var v_des_ajuda_aux_1
    as character
    format "x(80)":U
    column-label "Ajuda"
    no-undo.
def new shared var v_des_error_aux
    as character
    format "x(60)":U
    column-label "Inconsistància"
    no-undo.
def new shared var v_des_lista_estab
    as character
    format "x(2000)":U
    view-as editor max-chars 2000 scrollbar-vertical
    size 80 by 12
    bgcolor 15 font 2
    label "Estabelecimentos"
    column-label "Estabelecimentos"
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
def var v_log_answer
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    no-undo.
def new shared var v_log_balanct_param
    as logical
    format "Sim/N∆o"
    initial yes
    no-undo.
def var v_log_bloq_fgl
    as logical
    format "Sim/N∆o"
    initial no
    no-undo.
def var v_log_bloq_fgl_desativ
    as logical
    format "Sim/N∆o"
    initial no
    no-undo.
def new shared var v_log_ccusto_sum
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    label "Sumaria Ccusto"
    column-label "Sum Ccusto"
    no-undo.
def new shared var v_log_consid_apurac_restdo
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    label "Consid Apurac Restdo"
    column-label "Apurac Restdo"
    no-undo.
def new shared var v_log_cta_ctbl_analit
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    label "Somente Cta Analit"
    no-undo.
def new shared var v_log_cta_ctbl_internac
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    label "Consid Cta Internac"
    no-undo.
def new shared var v_log_cta_ctbl_sdo
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    label "Impr Cta Sem Sdo"
    no-undo.
def new global shared var v_log_eai_habilit
    as logical
    format "Sim/N∆o"
    initial no
    no-undo.
def var v_log_emis_balanct_por_estrut
    as logical
    format "Sim/N∆o"
    initial no
    no-undo.
def new shared var v_log_estab_sum
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    label "Sumaria Estab"
    no-undo.
def new global shared var v_log_execution
    as logical
    format "Sim/N∆o"
    initial yes
    no-undo.
def new shared var v_log_gerac_planilha
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    label "Gera Excel"
    no-undo.
def new shared var v_log_gera_eai
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    label "Gera XML"
    column-label "Gera XML"
    no-undo.
def var v_log_method
    as logical
    format "Sim/N∆o"
    initial yes
    no-undo.
def new shared var v_log_mostra_sem_aprop_cc
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    label "Mostra sem Aprop CC"
    column-label "Mostra S/Apr CC"
    no-undo.
def new shared var v_log_period_ctbl_ant_impr
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    label "Impr Period Anterior"
    no-undo.
def var v_log_plano_ccusto_uni
    as logical
    format "Sim/N∆o"
    initial no
    no-undo.
def var v_log_plano_cta_ctbl_uni
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
def new shared var v_log_proj_financ
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    label "Sumaria Projeto"
    no-undo.
def new shared var v_log_unid_negoc_sum
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    label "Sumaria Unid Negoc"
    column-label "Sum Un Neg"
    no-undo.
def new shared var v_nom_dwb_printer
    as character
    format "x(30)":U
    no-undo.
def new shared var v_nom_dwb_print_file
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
def new shared var v_nom_prog_ext
    as character
    format "x(8)":U
    label "Nome Externo"
    no-undo.
def new shared var v_nom_prog_ext_aux
    as character
    format "x(8)":U
    no-undo.
def var v_nom_prog_upc
    as character
    format "X(50)":U
    label "Programa UPC"
    column-label "Programa UPC"
    no-undo.
def new shared var v_nom_report_title
    as character
    format "x(40)":U
    no-undo.
def var v_nom_table_epc
    as character
    format "x(30)":U
    no-undo.
def var v_nom_title_aux
    as character
    format "x(60)":U
    no-undo.
def new shared var v_num_cod_erro_aux
    as integer
    format ">>>>,>>9":U
    label "Erro"
    column-label "N£mero"
    no-undo.
def var v_num_cont
    as integer
    format ">,>>9":U
    initial 0
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
def new shared var v_num_id_layout_impres
    as integer
    format ">>>>,>>9":U
    label "N£mero Layout"
    column-label "N£mero Layout"
    no-undo.
def new shared var v_num_niv_estrut
    as integer
    format ">>9":U
    label "N°vel Estrutura"
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
def new shared var v_num_period_ctbl
    as integer
    format "99":U
    initial 01
    label "Per°odo Atual"
    column-label "Period"
    no-undo.
def new shared var v_qtd_bottom
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
def new shared var v_qtd_line_aux
    as decimal
    format "->>>>,>>9.9999":U
    decimals 4
    initial 66
    no-undo.
def new global shared var v_rec_cenar_ctbl
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.
def new shared var v_rec_dwb_rpt_param
    as recid
    format ">>>>>>9":U
    no-undo.
def new shared var v_rec_dwb_rpt_select
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_exerc_ctbl
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.
def new global shared var v_rec_finalid_econ
    as recid
    format ">>>>>>9":U
    no-undo.
def var v_rec_log
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
def new global shared var v_rec_sdo_cta_ctbl
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.
def var v_rec_table_epc
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_unid_organ
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.
def var v_row_plano
    as Rowid
    no-undo.
def new shared var v_val_current_value
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    no-undo.
def new shared var v_val_maximum
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
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

.

def menu      m_help                menubar
    menu-item mi_conteudo           label "&Conte£do"
    menu-item mi_sobre              label "&Sobre".



/**************************** Menu Definition End ***************************/

/************************** Query Definition Begin **************************/

def query qr_dwb_rpt_select_range
    for dwb_rpt_select
    scrolling.


/*************************** Query Definition End ***************************/

/************************** Browse Definition Begin *************************/

def browse br_dwb_rpt_select_range query qr_dwb_rpt_select_range display 
    dwb_rpt_select.cod_dwb_field
    width-chars 32.00
        column-label "Conjunto"
    dwb_rpt_select.cod_dwb_initial
    width-chars 18.00
        column-label "Inicial"
    dwb_rpt_select.cod_dwb_final
    width-chars 18.00
        column-label "Final"
    with no-box separators single 
         size 38.00 by 05.00
         font 1
         bgcolor 15.


/*************************** Browse Definition End **************************/

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
def button bt_imprime
    label "&Imprime"
    tooltip "Imprime"
    size 1 by 1
    auto-go.
def button bt_down
    label "V"
    tooltip "Desce"
&if "{&window-system}" <> "TTY" &then
    image-up file "image/im-dw"
    image-insensitive file "image/ii-dw"
&endif
    size 1 by 1.
def button bt_edl1
    label "Alt"
    tooltip "Edita Linha"
&if "{&window-system}" <> "TTY" &then
    image-up file "image/im-edl"
    image-insensitive file "image/ii-edl"
&endif
    size 1 by 1.
def button bt_get_file
    label "Pesquisa Arquivo"
    tooltip "Pesquisa Arquivo"
&if "{&window-system}" <> "TTY" &then
    image-up file "image/im-sea1"
    image-insensitive file "image/ii-sea1"
&endif
    size 1 by 1.
def button bt_hel2
    label "Ajuda"
    tooltip "Ajuda"
    size 1 by 1.
def button bt_isl1
    label "Ins"
    tooltip "Insere Linha"
&if "{&window-system}" <> "TTY" &then
    image-up file "image/im-inl"
    image-insensitive file "image/ii-inl"
&endif
    size 1 by 1.
def button bt_mais
    label "Mais"
    tooltip ""
    size 1 by 1.
def button bt_planilha
    label "CSV"
    tooltip "Arquivo CSV"
    size 1 by 1.
/*def button bt_print
    label "&Imprime"
    tooltip "Imprime"
    size 1 by 1
    auto-go.*/
def button bt_rml1
    label "Ret"
    tooltip "Retira Linha"
&if "{&window-system}" <> "TTY" &then
    image-up file "image/im-rml"
    image-insensitive file "image/ii-rml"
&endif
    size 1 by 1.
def button bt_set_printer
    label "Define impressora e Layout"
    tooltip "Define impressora e Layout de Impress∆o"
&if "{&window-system}" <> "TTY" &then
    image-up file "image/im-setpr.bmp"
    image-insensitive file "image/ii-setpr"
&endif
    size 1 by 1.
def button bt_up
    label "A"
    tooltip "Sobe"
&if "{&window-system}" <> "TTY" &then
    image-up file "image/im-up"
    image-insensitive file "image/ii-up"
&endif
    size 1 by 1.
/****************************** Function Button *****************************/
def button bt_zoo_177628
    label "Zoom"
    tooltip "Zoom"
&if "{&window-system}" <> "TTY" &then
    image-up file "image/im-zoo"
    image-insensitive file "image/ii-zoo"
&endif
    size 4 by .88.
def button bt_zoo_177629
    label "Zoom"
    tooltip "Zoom"
&if "{&window-system}" <> "TTY" &then
    image-up file "image/im-zoo"
    image-insensitive file "image/ii-zoo"
&endif
    size 4 by .88.
def button bt_zoo_177633
    label "Zoom"
    tooltip "Zoom"
&if "{&window-system}" <> "TTY" &then
    image-up file "image/im-zoo"
    image-insensitive file "image/ii-zoo"
&endif
    size 4 by .88.
def button bt_zoo_188325
    label "Zoom"
    tooltip "Zoom"
&if "{&window-system}" <> "TTY" &then
    image-up file "image/im-zoo"
    image-insensitive file "image/ii-zoo"
&endif
    size 4 by .88.
def button bt_zoo_218012
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
    radio-buttons "Terminal", "Terminal","Arquivo CSV", "Arquivo","Impressora", "Impressora"
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
def new shared var v_rpt_s_1_columns as integer initial 172.
def new shared var v_rpt_s_1_bottom as integer initial 65.
def new shared var v_rpt_s_1_page as integer.
def new shared var v_rpt_s_1_name as character initial "Balancete".
def frame f_rpt_s_1_header_period header
    "-----------------------------------------------------------------------------------------------------------------------------------------------" at 1
    "P†gina:" at 145
    (page-number (s_1) + v_rpt_s_1_page) at 152 format ">>>>>9" skip
    v_nom_enterprise at 1 format "x(40)"
    v_nom_report_title at 117 format "x(40)" skip
    "Per°odo: " at 1
    v_dat_inic_period at 10 format "99/99/9999"
    "A" at 21
    v_dat_fim_period at 23 format "99/99/9999"
    "---------------------------------------------------------------------------------------------------------" at 34
    v_dat_execution at 140 format "99/99/9999" "- "
    v_hra_execution at 153 format "99:99" skip (1)
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_header_unique header
    "-----------------------------------------------------------------------------------------------------------------------------------------------" at 1
    'P†gina:' at 145
    (page-number (s_1) + v_rpt_s_1_page) at 152 format '>>>>>9' skip
    v_nom_enterprise at 1 format 'x(40)'
    v_nom_report_title at 118 format 'x(40)' skip
    '------------------------------------------------------------------------------------------------------------------------------------------' at 1
    v_dat_execution at 140 format '99/99/9999' '- '
    v_hra_execution at 153 format "99:99" skip (1)
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_footer_last_page header
    "Çltima P†gina " at 1
    "-----------------------------------------------------------------------------------------------------------------------" at 15
    v_nom_prog_ext at 135 format "x(08)" "- "
    v_cod_release at 146 format "x(12)" skip
    with no-box no-labels width 157 page-bottom stream-io.
def frame f_rpt_s_1_footer_normal header
    "------------------------------------------------------------------------------------------------------------------------------------" at 1
    "- " at 133
    v_nom_prog_ext at 135 format "x(08)" "- "
    v_cod_release at 146 format "x(12)" skip
    with no-box no-labels width 157 page-bottom stream-io.
def frame f_rpt_s_1_footer_param_page header
    "P†gina ParÉmetros " at 1
    "-------------------------------------------------------------------------------------------------------------------" at 19
    v_nom_prog_ext at 135 format "x(08)" "- "
    v_cod_release at 146 format "x(12)" skip
    with no-box no-labels width 157 page-bottom stream-io.
def frame f_rpt_s_1_ass_ass header
    skip (4)
    "Assinaturas:   " at 1
    skip (4)
    "----------------------------------------" at 1
    skip (4)
    "----------------------------------------" at 1 skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_Avisos_Lay_Avisos header
    "N£mero" to 8
    "Inconsistància" at 10
    "Ajuda" at 71 skip
    "--------" to 8
    "------------------------------------------------------------" at 10
    "--------------------------------------------------------------------------------" at 71 skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_lin_balanct_Lay_period header
    "T°tulo" at 1
    "Saldo Inicial" to 99
    "DÇbito" to 119
    "CrÇdito" to 138
    "Saldo Final" to 156 skip
    "--------------------------------------------------------------------------------" at 1
    "-----------------" to 99
    "------------------" to 119
    "------------------" to 138
    "-----------------" to 156 skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_lin_balanct_Lay_period_ant header
    "T°tulo" at 1
    "Sdo Inicial Ant" to 99
    "Sdo Final Ant" to 118
    "Saldo Inicial" to 137
    "Saldo Final" to 156 skip
    "--------------------------------------------------------------------------------" at 1
    "-----------------" to 99
    "-----------------" to 118
    "-----------------" to 137
    "-----------------" to 156 skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_lin_blnct_di_Lay_Per_ant_dia header
    "T°tulo" at 1
    "Sdo Inicial Ant" to 77
    "Sdo Final Ant" to 95
    "Saldo Inicial" to 113
    "Saldo Final" to 131 skip
    "-------------------------------------------------------" at 1
    "-----------------" to 77
    "---------------" to 95
    "---------------" to 113
    "-----------------" to 131 skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_lin_blnct_di_Lay_per_dia header
    "T°tulo" at 1
    "Saldo Inicial" to 73
    "DÇbito" to 94
    "CrÇdito" to 113
    "Saldo Final" to 131 skip
    "-------------------------------------------------------" at 1
    "-----------------" to 73
    "------------------" to 94
    "------------------" to 113
    "-----------------" to 131 skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_parametros_Lay_parametros header
    skip (1)
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Unid Organizacional: " at 31
    v_cod_unid_organ at 52 format "x(3)" view-as text
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Unid Organizacional: " at 31
    v_cod_unid_organ at 52 format "x(5)" view-as text
&ENDIF skip
    "Plano Contas: " at 38
    v_cod_plano_cta_ctbl at 52 format "x(8)" view-as text skip
    "Plano CCusto: " at 38
    v_cod_plano_ccusto at 52 format "x(8)" view-as text skip
    "Cen†rio Cont†bil: " at 34
    v_cod_cenar_ctbl at 52 format "x(8)" view-as text skip
    "Exerc°cio Cont†bil: " at 32
    v_cod_exerc_ctbl at 52 format "9999" view-as text skip
    "Per°odo Atual: " at 37
    v_num_period_ctbl to 53 format "99" view-as text skip
    "Sumaria Estab: " at 37
    v_log_estab_sum at 52 format "Sim/N∆o" view-as text skip
    "Sumaria Unid Negoc: " at 32
    v_log_unid_negoc_sum at 52 format "Sim/N∆o" view-as text skip
    "Sumaria Ccusto: " at 36
    v_log_ccusto_sum at 52 format "Sim/N∆o" view-as text skip
    "Finalidade Base: " at 35
    v_cod_finalid_econ_bas at 52 format "x(10)" view-as text skip
    "Finalid Converte: " at 34
    v_cod_finalid_econ_apr at 52 format "x(10)" view-as text skip
    "Data Cotaá∆o: " at 38
    v_dat_cotac_indic_econ at 52 format "99/99/9999" view-as text skip
    "Consid Cta Internac: " at 31
    v_log_cta_ctbl_internac at 52 format "Sim/N∆o" view-as text skip
    "Impr Period Anterior: " at 30
    v_log_period_ctbl_ant_impr at 52 format "Sim/N∆o" view-as text skip
    "Impr Cta Sem Sdo: " at 34
    v_log_cta_ctbl_sdo at 52 format "Sim/N∆o" view-as text skip
    "Somente Cta Analit: " at 32
    v_log_cta_ctbl_analit at 52 format "Sim/N∆o" view-as text skip
    "Consid Apurac Restdo: " at 30
    v_log_consid_apurac_restdo at 52 format "Sim/N∆o" view-as text skip
    "Mostra sem Aprop CC: " at 31
    v_log_mostra_sem_aprop_cc at 52 format "Sim/N∆o" view-as text skip
    "N°vel Estrutura: " at 35
    v_num_niv_estrut to 54 format ">>9" view-as text skip
    "Idioma Apresentaá∆o: " at 31
    v_cod_idioma_apr at 52 format "x(8)" view-as text skip
    "Parte Fixa: " at 40
    v_cod_cta_ctbl_pfixa at 52 format "x(20)" view-as text skip
    "Parte Exceá∆o: " at 37
    v_cod_cta_ctbl_excec at 52 format "x(20)" view-as text skip
    "Parte Fixa CCusto: " at 33
    v_cod_ccusto_pfixa at 52 format "x(11)" view-as text skip
    "Parte Exceá∆o: " at 37
    v_cod_ccusto_excec at 52 format "x(11)" view-as text skip
    "Gera Planilha: " at 37
    v_log_gerac_planilha at 52 format "Sim/N∆o" view-as text skip
    "Arq Planilha: " at 38
    v_cod_arq_planilha at 52 format "x(40)" view-as text skip
    "Caracter Delimitador: " at 30
    v_cod_carac_lim at 52 format "x(1)" view-as text skip
    "Tempo Execuá∆o: " at 36
    v_cod_tempo_exec at 52 format "x(8)" view-as text
    skip (1)
    v_cod_erro at 1 format "x(10)" view-as text
    v_des_error_aux at 12 format "x(60)" view-as text
    v_des_ajuda_aux_1 at 73 format "x(80)" view-as text skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_parametros_Lay_param_505 header
    skip (1)
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Unid Organizacional: " at 31
    v_cod_unid_organ at 52 format "x(3)" view-as text
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Unid Organizacional: " at 31
    v_cod_unid_organ at 52 format "x(5)" view-as text
&ENDIF skip
    "Plano Contas: " at 38
    v_cod_plano_cta_ctbl at 52 format "x(8)" view-as text skip
    "Plano CCusto: " at 38
    v_cod_plano_ccusto at 52 format "x(8)" view-as text skip
    "Cen†rio Cont†bil: " at 34
    v_cod_cenar_ctbl at 52 format "x(8)" view-as text skip
    "Exerc°cio Cont†bil: " at 32
    v_cod_exerc_ctbl at 52 format "9999" view-as text skip
    "Per°odo Atual: " at 37
    v_num_period_ctbl to 53 format "99" view-as text skip
    "Sumaria Estab: " at 37
    v_log_estab_sum at 52 format "Sim/N∆o" view-as text skip
    "Sumaria Unid Negoc: " at 32
    v_log_unid_negoc_sum at 52 format "Sim/N∆o" view-as text skip
    "Sumaria Projeto: " at 35
    v_log_proj_financ at 52 format "Sim/N∆o" view-as text skip
    "Sumaria Ccusto: " at 36
    v_log_ccusto_sum at 52 format "Sim/N∆o" view-as text skip
    "Finalidade Base: " at 35
    v_cod_finalid_econ_bas at 52 format "x(10)" view-as text skip
    "Finalid Converte: " at 34
    v_cod_finalid_econ_apr at 52 format "x(10)" view-as text skip
    "Data Cotaá∆o: " at 38
    v_dat_cotac_indic_econ at 52 format "99/99/9999" view-as text skip
    "Consid Cta Internac: " at 31
    v_log_cta_ctbl_internac at 52 format "Sim/N∆o" view-as text skip
    "Impr Period Anterior: " at 30
    v_log_period_ctbl_ant_impr at 52 format "Sim/N∆o" view-as text skip
    "Impr Cta Sem Sdo: " at 34
    v_log_cta_ctbl_sdo at 52 format "Sim/N∆o" view-as text skip
    "Somente Cta Analit: " at 32
    v_log_cta_ctbl_analit at 52 format "Sim/N∆o" view-as text skip
    "Consid Apurac Restdo: " at 30
    v_log_consid_apurac_restdo at 52 format "Sim/N∆o" view-as text skip
    "Mostra sem Aprop CC: " at 31
    v_log_mostra_sem_aprop_cc at 52 format "Sim/N∆o" view-as text skip
    "N°vel Estrutura: " at 35
    v_num_niv_estrut to 54 format ">>9" view-as text skip
    "Idioma Apresentaá∆o: " at 31
    v_cod_idioma_apr at 52 format "x(8)" view-as text skip
    "Parte Fixa: " at 40
    v_cod_cta_ctbl_pfixa at 52 format "x(20)" view-as text skip
    "Parte Exceá∆o: " at 37
    v_cod_cta_ctbl_excec at 52 format "x(20)" view-as text skip
    "Parte Fixa CCusto: " at 33
    v_cod_ccusto_pfixa at 52 format "x(11)" view-as text skip
    "Parte Exceá∆o: " at 37
    v_cod_ccusto_excec at 52 format "x(11)" view-as text skip
    "PFixa Proj: " at 40
    v_cod_proj_financ_pfixa at 52 format "x(20)" view-as text skip
    "Exceá∆o: " at 43
    v_cod_proj_financ_excec at 52 format "x(20)" view-as text skip
    "Gera Planilha: " at 37
    v_log_gerac_planilha at 52 format "Sim/N∆o" view-as text skip
    "Arq Planilha: " at 38
    v_cod_arq_planilha at 52 format "x(40)" view-as text skip
    "Caracter Delimitador: " at 30
    v_cod_carac_lim at 52 format "x(1)" view-as text skip
    "Tempo Execuá∆o: " at 36
    v_cod_tempo_exec at 52 format "x(8)" view-as text
    skip (1)
    v_cod_erro at 1 format "x(10)" view-as text
    v_des_error_aux at 12 format "x(60)" view-as text
    v_des_ajuda_aux_1 at 73 format "x(80)" view-as text skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_parametros_Lay_param_505XML header
    skip (1)
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Unid Organizacional: " at 31
    v_cod_unid_organ at 52 format "x(3)" view-as text
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Unid Organizacional: " at 31
    v_cod_unid_organ at 52 format "x(5)" view-as text
&ENDIF skip
    "Plano Contas: " at 38
    v_cod_plano_cta_ctbl at 52 format "x(8)" view-as text skip
    "Plano CCusto: " at 38
    v_cod_plano_ccusto at 52 format "x(8)" view-as text skip
    "Cen†rio Cont†bil: " at 34
    v_cod_cenar_ctbl at 52 format "x(8)" view-as text skip
    "Exerc°cio Cont†bil: " at 32
    v_cod_exerc_ctbl at 52 format "9999" view-as text skip
    "Per°odo Atual: " at 37
    v_num_period_ctbl to 53 format "99" view-as text skip
    "Sumaria Estab: " at 37
    v_log_estab_sum at 52 format "Sim/N∆o" view-as text skip
    "Sumaria Unid Negoc: " at 32
    v_log_unid_negoc_sum at 52 format "Sim/N∆o" view-as text skip
    "Sumaria Projeto: " at 35
    v_log_proj_financ at 52 format "Sim/N∆o" view-as text skip
    "Sumaria Ccusto: " at 36
    v_log_ccusto_sum at 52 format "Sim/N∆o" view-as text skip
    "Finalidade Base: " at 35
    v_cod_finalid_econ_bas at 52 format "x(10)" view-as text skip
    "Finalid Converte: " at 34
    v_cod_finalid_econ_apr at 52 format "x(10)" view-as text skip
    "Data Cotaá∆o: " at 38
    v_dat_cotac_indic_econ at 52 format "99/99/9999" view-as text skip
    "Consid Cta Internac: " at 31
    v_log_cta_ctbl_internac at 52 format "Sim/N∆o" view-as text skip
    "Impr Period Anterior: " at 30
    v_log_period_ctbl_ant_impr at 52 format "Sim/N∆o" view-as text skip
    "Impr Cta Sem Sdo: " at 34
    v_log_cta_ctbl_sdo at 52 format "Sim/N∆o" view-as text skip
    "Somente Cta Analit: " at 32
    v_log_cta_ctbl_analit at 52 format "Sim/N∆o" view-as text skip
    "Consid Apurac Restdo: " at 30
    v_log_consid_apurac_restdo at 52 format "Sim/N∆o" view-as text skip
    "Mostra sem Aprop CC: " at 31
    v_log_mostra_sem_aprop_cc at 52 format "Sim/N∆o" view-as text skip
    "N°vel Estrutura: " at 35
    v_num_niv_estrut to 54 format ">>9" view-as text skip
    "Idioma Apresentaá∆o: " at 31
    v_cod_idioma_apr at 52 format "x(8)" view-as text skip
    "Parte Fixa: " at 40
    v_cod_cta_ctbl_pfixa at 52 format "x(20)" view-as text skip
    "Parte Exceá∆o: " at 37
    v_cod_cta_ctbl_excec at 52 format "x(20)" view-as text skip
    "Parte Fixa CCusto: " at 33
    v_cod_ccusto_pfixa at 52 format "x(11)" view-as text skip
    "Parte Exceá∆o: " at 37
    v_cod_ccusto_excec at 52 format "x(11)" view-as text skip
    "PFixa Proj: " at 40
    v_cod_proj_financ_pfixa at 52 format "x(20)" view-as text skip
    "Exceá∆o: " at 43
    v_cod_proj_financ_excec at 52 format "x(20)" view-as text skip
    "Gera Planilha: " at 37
    v_log_gerac_planilha at 52 format "Sim/N∆o" view-as text skip
    "Arq Planilha: " at 38
    v_cod_arq_planilha at 52 format "x(40)" view-as text skip
    "Caracter Delimitador: " at 30
    v_cod_carac_lim at 52 format "x(1)" view-as text skip
    "Tempo Execuá∆o: " at 36
    v_cod_tempo_exec at 52 format "x(8)" view-as text skip
    "Gera XML: " at 42
    v_log_gera_eai at 52 format "Sim/N∆o" view-as text skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_parametros_Lay_param_XML header
    skip (1)
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Unid Organizacional: " at 31
    v_cod_unid_organ at 52 format "x(3)" view-as text
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Unid Organizacional: " at 31
    v_cod_unid_organ at 52 format "x(5)" view-as text
&ENDIF skip
    "Plano Contas: " at 38
    v_cod_plano_cta_ctbl at 52 format "x(8)" view-as text skip
    "Plano CCusto: " at 38
    v_cod_plano_ccusto at 52 format "x(8)" view-as text skip
    "Cen†rio Cont†bil: " at 34
    v_cod_cenar_ctbl at 52 format "x(8)" view-as text skip
    "Exerc°cio Cont†bil: " at 32
    v_cod_exerc_ctbl at 52 format "9999" view-as text skip
    "Per°odo Atual: " at 37
    v_num_period_ctbl to 53 format "99" view-as text skip
    "Sumaria Estab: " at 37
    v_log_estab_sum at 52 format "Sim/N∆o" view-as text skip
    "Sumaria Unid Negoc: " at 32
    v_log_unid_negoc_sum at 52 format "Sim/N∆o" view-as text skip
    "Sumaria Ccusto: " at 36
    v_log_ccusto_sum at 52 format "Sim/N∆o" view-as text skip
    "Finalidade Base: " at 35
    v_cod_finalid_econ_bas at 52 format "x(10)" view-as text skip
    "Finalid Converte: " at 34
    v_cod_finalid_econ_apr at 52 format "x(10)" view-as text skip
    "Data Cotaá∆o: " at 38
    v_dat_cotac_indic_econ at 52 format "99/99/9999" view-as text skip
    "Consid Cta Internac: " at 31
    v_log_cta_ctbl_internac at 52 format "Sim/N∆o" view-as text skip
    "Impr Period Anterior: " at 30
    v_log_period_ctbl_ant_impr at 52 format "Sim/N∆o" view-as text skip
    "Impr Cta Sem Sdo: " at 34
    v_log_cta_ctbl_sdo at 52 format "Sim/N∆o" view-as text skip
    "Somente Cta Analit: " at 32
    v_log_cta_ctbl_analit at 52 format "Sim/N∆o" view-as text skip
    "Consid Apurac Restdo: " at 30
    v_log_consid_apurac_restdo at 52 format "Sim/N∆o" view-as text skip
    "Mostra sem Aprop CC: " at 31
    v_log_mostra_sem_aprop_cc at 52 format "Sim/N∆o" view-as text skip
    "N°vel Estrutura: " at 35
    v_num_niv_estrut to 54 format ">>9" view-as text skip
    "Idioma Apresentaá∆o: " at 31
    v_cod_idioma_apr at 52 format "x(8)" view-as text skip
    "Parte Fixa: " at 40
    v_cod_cta_ctbl_pfixa at 52 format "x(20)" view-as text skip
    "Parte Exceá∆o: " at 37
    v_cod_cta_ctbl_excec at 52 format "x(20)" view-as text skip
    "Parte Fixa CCusto: " at 33
    v_cod_ccusto_pfixa at 52 format "x(11)" view-as text skip
    "Parte Exceá∆o: " at 37
    v_cod_ccusto_excec at 52 format "x(11)" view-as text skip
    "Gera Planilha: " at 37
    v_log_gerac_planilha at 52 format "Sim/N∆o" view-as text skip
    "Arq Planilha: " at 38
    v_cod_arq_planilha at 52 format "x(40)" view-as text skip
    "Caracter Delimitador: " at 30
    v_cod_carac_lim at 52 format "x(1)" view-as text skip
    "Tempo Execuá∆o: " at 36
    v_cod_tempo_exec at 52 format "x(8)" view-as text skip
    "Gera XML: " at 42
    v_log_gera_eai at 52 format "Sim/N∆o" view-as text skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_parametros_Lay_segur_estab header
    skip (1)
    entry(1, return-value, chr(255)) at 15 format "x(80)" skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_termo_Lay_termo header
    /* Vari†vel v_des_termo_diario ignorada. N∆o esta definida no programa */ skip (1)
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_tot_balanct_Lay_demai_grp header
    skip (1)
    "Lucros e Perdas" at 1
    /* Vari†vel v_val_tot_sdo_inic_apurac ignorada. N∆o esta definida no programa */
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */
    /* Vari†vel v_val_tot_sdo_fim_apurac ignorada. N∆o esta definida no programa */
    /* Vari†vel v_ind_cr_sdo_fim ignorada. N∆o esta definida no programa */
    /* Vari†vel v_cod_grp_cta_ctbl_nok ignorada. N∆o esta definida no programa */
    /* Vari†vel v_val_tot_sdo_inic_dem ignorada. N∆o esta definida no programa */
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */
    /* Vari†vel v_val_tot_sdo_fim_dem ignorada. N∆o esta definida no programa */
    /* Vari†vel v_ind_cr_sdo_fim ignorada. N∆o esta definida no programa */ skip (1)
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_tot_balanct_Lay_grp_cta header
    /* Atributo tt_grp_cta_ctbl.tta_cod_grp_cta_ctbl ignorado */
    "-" at 59
    /* Atributo tt_grp_cta_ctbl.tta_val_sdo_ctbl_inic ignorado */
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */
    /* Atributo tt_grp_cta_ctbl.tta_val_sdo_ctbl_fim ignorado */
    /* Vari†vel v_ind_cr_sdo_fim ignorada. N∆o esta definida no programa */ skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_tot_balanct_Lay_grp_cta_ant header
    /* Atributo tt_grp_cta_ctbl.tta_cod_grp_cta_ctbl ignorado */
    "-" at 78
    /* Atributo tt_grp_cta_ctbl.ttv_val_sdo_ctbl_inic_ant ignorado */
    /* Vari†vel v_ind_cr_sdo_inic_ant ignorada. N∆o esta definida no programa */
    /* Atributo tt_grp_cta_ctbl.ttv_val_sdo_ctbl_fim_ant ignorado */
    /* Vari†vel v_ind_cr_sdo_fim_ant ignorada. N∆o esta definida no programa */
    /* Atributo tt_grp_cta_ctbl.tta_val_sdo_ctbl_inic ignorado */
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */
    /* Atributo tt_grp_cta_ctbl.tta_val_sdo_ctbl_fim ignorado */
    /* Vari†vel v_ind_cr_sdo_fim ignorada. N∆o esta definida no programa */ skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_tot_balanct_Lay_lucro_perda header
    skip (1)
    "Lucros e Perdas" at 1
    /* Vari†vel v_val_tot_sdo_inic_apurac ignorada. N∆o esta definida no programa */
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */
    /* Vari†vel v_val_tot_sdo_fim_apurac ignorada. N∆o esta definida no programa */
    /* Vari†vel v_ind_cr_sdo_fim ignorada. N∆o esta definida no programa */
    /* Vari†vel v_cod_grp_cta_ctbl_nok ignorada. N∆o esta definida no programa */
    /* Vari†vel v_val_tot_sdo_inic_dem ignorada. N∆o esta definida no programa */
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */
    /* Vari†vel v_val_tot_sdo_fim_dem ignorada. N∆o esta definida no programa */
    /* Vari†vel v_ind_cr_sdo_fim ignorada. N∆o esta definida no programa */ skip (1)
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_tot_balanct_Lay_tot_db header
    "Total de DÇbitos" at 40
    /* Vari†vel v_val_tot_sdo_ctbl_db ignorada. N∆o esta definida no programa */ skip
    "Total de CrÇditos" at 39
    /* Vari†vel v_val_tot_sdo_ctbl_cr ignorada. N∆o esta definida no programa */ skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_tot_blnct_di_Lay_grp_cta_ant header
    /* Atributo tt_grp_cta_ctbl.tta_cod_grp_cta_ctbl ignorado */
    "-" at 59
    /* Atributo tt_grp_cta_ctbl.ttv_val_sdo_ctbl_inic_ant ignorado */
    /* Vari†vel v_ind_cr_sdo_inic_ant ignorada. N∆o esta definida no programa */
    /* Atributo tt_grp_cta_ctbl.ttv_val_sdo_ctbl_fim_ant ignorado */
    /* Vari†vel v_ind_cr_sdo_fim_ant ignorada. N∆o esta definida no programa */
    /* Atributo tt_grp_cta_ctbl.tta_val_sdo_ctbl_inic ignorado */
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */
    /* Atributo tt_grp_cta_ctbl.tta_val_sdo_ctbl_fim ignorado */
    /* Vari†vel v_ind_cr_sdo_fim ignorada. N∆o esta definida no programa */ skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_tot_blnct_di_Lay_Grp_cta_diar header
    /* Atributo tt_grp_cta_ctbl.tta_cod_grp_cta_ctbl ignorado */
    "-" at 59
    /* Atributo tt_grp_cta_ctbl.tta_val_sdo_ctbl_inic ignorado */
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */
    /* Atributo tt_grp_cta_ctbl.tta_val_sdo_ctbl_fim ignorado */
    /* Vari†vel v_ind_cr_sdo_fim ignorada. N∆o esta definida no programa */ skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_tot_blnct_di_Lay_grp_Tot_diar header
    "Total de DÇbitos" at 40
    /* Vari†vel v_val_tot_sdo_ctbl_db ignorada. N∆o esta definida no programa */ skip
    "Total de CrÇditos" at 39
    /* Vari†vel v_val_tot_sdo_ctbl_cr ignorada. N∆o esta definida no programa */ skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_tot_blnct_di_Lay_lucr_perd header
    skip (1)
    "Lucros e Perdas" at 1
    /* Vari†vel v_val_tot_sdo_inic_apurac ignorada. N∆o esta definida no programa */
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */
    /* Vari†vel v_val_tot_sdo_fim_apurac ignorada. N∆o esta definida no programa */
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */
    /* Vari†vel v_cod_grp_cta_ctbl_nok ignorada. N∆o esta definida no programa */
    /* Vari†vel v_val_tot_sdo_inic_dem ignorada. N∆o esta definida no programa */
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */
    /* Vari†vel v_val_tot_sdo_fim_dem ignorada. N∆o esta definida no programa */
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */ skip (1)
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_tot_blnct_di_Lay_lucr_perd_an header
    skip (1)
    "Lucros e Perdas" at 1
    /* Vari†vel v_val_tot_sdo_inic_apurac ignorada. N∆o esta definida no programa */
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */
    /* Vari†vel v_val_tot_sdo_fim_apurac ignorada. N∆o esta definida no programa */
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */
    /* Vari†vel v_cod_grp_cta_ctbl_nok ignorada. N∆o esta definida no programa */
    /* Vari†vel v_val_tot_sdo_inic_dem ignorada. N∆o esta definida no programa */
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */
    /* Vari†vel v_val_tot_sdo_fim_dem ignorada. N∆o esta definida no programa */
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */ skip (1)
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_tot_cta_Lay_tot_cta_1 header
    "Saldo Inicial" to 99
    "DÇbito" to 119
    "CrÇdito" to 138
    "Saldo Final" to 156 skip
    "-----------------" to 99
    "------------------" to 119
    "------------------" to 138
    "-----------------" to 156 skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_tot_cta_Lay_tot_cta_2 header
    "Sdo Inicial Ant" to 99
    "Sdo Final Ant" to 118
    "Saldo Inicial" to 137
    "Saldo Final" to 156 skip
    "-----------------" to 99
    "-----------------" to 118
    "-----------------" to 137
    "-----------------" to 156 skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_tot_cta_dia_Lay_tot_cta_dia1 header
    "Saldo Inicial" to 73
    "DÇbito" to 94
    "CrÇdito" to 113
    "Saldo Final" to 131 skip
    "-----------------" to 73
    "------------------" to 94
    "------------------" to 113
    "-----------------" to 131 skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_tot_cta_dia_Lay_tot_cta_dia2 header
    "Sdo Inicial Ant" to 77
    "Sdo Final Ant" to 95
    "Saldo Inicial" to 113
    "Saldo Final" to 132 skip
    "-----------------" to 77
    "---------------" to 95
    "---------------" to 113
    "-----------------" to 132 skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_tt_erros_xml_Lay_erro_xml header
    "N£m" to 8
    "Erro" at 10 skip
    "--------" to 8
    "------------------------------------------------------------" at 10 skip
    with no-box no-labels width 157 page-top stream-io.


/*************************** Report Definition End **************************/

/************************** Frame Definition Begin **************************/

def frame f_rpt_42_sdo_cta_ctbl_balanct
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
    ls_order
         at row 02.00 col 04.00
         help "" no-label
    bt_up
         at row 03.42 col 35.00 font ?
         help "Sobe"
    bt_down
         at row 04.58 col 35.00 font ?
         help "Desce"
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    unid_organ.cod_unid_organ
         at row 08.42 col 14.00 colon-aligned label "UO"
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_zoo_188325
         at row 08.42 col 20.14
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    unid_organ.cod_unid_organ
         at row 08.42 col 14.00 colon-aligned label "UO"
         view-as fill-in
         size-chars 6.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_zoo_188325
         at row 08.42 col 22.14
&ENDIF
    plano_cta_ctbl.cod_plano_cta_ctbl
         at row 09.42 col 14.00 colon-aligned label "Plano Ctas"
         view-as fill-in
         size-chars 9.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_zoo_177633
         at row 09.42 col 25.14
    cenar_ctbl.cod_cenar_ctbl
         at row 10.42 col 14.00 colon-aligned label "Cen†rio"
         view-as fill-in
         size-chars 9.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_zoo_177628
         at row 10.42 col 25.14
    exerc_ctbl.cod_exerc_ctbl
         at row 11.42 col 14.00 colon-aligned label "Exerc°cio"
         view-as fill-in
         size-chars 5.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_zoo_177629
         at row 11.42 col 21.14
    period_ctbl.num_period_ctbl
         at row 12.42 col 14.00 colon-aligned label "Per°odo"
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    plano_ccusto.cod_plano_ccusto
         at row 13.42 col 14.00 colon-aligned label "Plano CCusto"
         view-as fill-in
         size-chars 9.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_zoo_218012
         at row 13.42 col 25.14
    bt_mais
         at row 08.42 col 30.29 font ?
    bt_planilha
         at row 09.42 col 30.29 font ?
         help "Arquivo CSV"
    br_dwb_rpt_select_range
         at row 02.00 col 44.00
         help "Faixas"
    bt_isl1
         at row 02.79 col 83.00 font ?
         help "Insere Linha"
    bt_edl1
         at row 04.00 col 83.00 font ?
         help "Edita Linha"
    bt_rml1
         at row 05.21 col 83.00 font ?
         help "Retira Linha"
    rs_cod_dwb_output
         at row 08.50 col 44.00
         help "" no-label
    ed_1x40
         at row 09.50 col 44.00
         help "" no-label
    bt_set_printer
         at row 09.50 col 83.00 font ?
         help "Define impressora e Layout de Impress∆o"
    bt_get_file
         at row 09.50 col 83.00 font ?
         help "Pesquisa Arquivo"
    rs_ind_run_mode
         at row 12.21 col 44.00
         help "" no-label
    v_log_print_par
         at row 13.21 col 44.00 label "Imprime ParÉmetros"
         view-as toggle-box
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
    bt_imprime
         at row 15.25 col 03.00 font ?
         help "Fecha"
    /*bt_print
         at row 15.25 col 14.00 font ?
         help "Imprime"*/
    bt_can
         at row 15.25 col 14.00 font ?
         help "Cancela"
    bt_hel2
         at row 15.25 col 77.57 font ?
         help "Ajuda"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 90.00 by 17.00
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Balancete".
    /* adjust size of objects in this frame */
    assign bt_can:width-chars               in frame f_rpt_42_sdo_cta_ctbl_balanct = 10.00
           bt_can:height-chars              in frame f_rpt_42_sdo_cta_ctbl_balanct = 01.00
           bt_imprime:width-chars             in frame f_rpt_42_sdo_cta_ctbl_balanct = 10.00
           bt_imprime:height-chars            in frame f_rpt_42_sdo_cta_ctbl_balanct = 01.00
           bt_down:width-chars              in frame f_rpt_42_sdo_cta_ctbl_balanct = 04.00
           bt_down:height-chars             in frame f_rpt_42_sdo_cta_ctbl_balanct = 01.13
           bt_edl1:width-chars              in frame f_rpt_42_sdo_cta_ctbl_balanct = 04.00
           bt_edl1:height-chars             in frame f_rpt_42_sdo_cta_ctbl_balanct = 01.13
           bt_get_file:width-chars          in frame f_rpt_42_sdo_cta_ctbl_balanct = 04.00
           bt_get_file:height-chars         in frame f_rpt_42_sdo_cta_ctbl_balanct = 01.08
           bt_hel2:width-chars              in frame f_rpt_42_sdo_cta_ctbl_balanct = 10.00
           bt_hel2:height-chars             in frame f_rpt_42_sdo_cta_ctbl_balanct = 01.00
           bt_isl1:width-chars              in frame f_rpt_42_sdo_cta_ctbl_balanct = 04.00
           bt_isl1:height-chars             in frame f_rpt_42_sdo_cta_ctbl_balanct = 01.13
           bt_mais:width-chars              in frame f_rpt_42_sdo_cta_ctbl_balanct = 10.00
           bt_mais:height-chars             in frame f_rpt_42_sdo_cta_ctbl_balanct = 01.00
           bt_planilha:width-chars          in frame f_rpt_42_sdo_cta_ctbl_balanct = 10.00
           bt_planilha:height-chars         in frame f_rpt_42_sdo_cta_ctbl_balanct = 01.00
           /*bt_print:width-chars             in frame f_rpt_42_sdo_cta_ctbl_balanct = 10.00
           bt_print:height-chars            in frame f_rpt_42_sdo_cta_ctbl_balanct = 01.00*/
           bt_rml1:width-chars              in frame f_rpt_42_sdo_cta_ctbl_balanct = 04.00
           bt_rml1:height-chars             in frame f_rpt_42_sdo_cta_ctbl_balanct = 01.13
           bt_set_printer:width-chars       in frame f_rpt_42_sdo_cta_ctbl_balanct = 04.00
           bt_set_printer:height-chars      in frame f_rpt_42_sdo_cta_ctbl_balanct = 01.08
           bt_up:width-chars                in frame f_rpt_42_sdo_cta_ctbl_balanct = 04.00
           bt_up:height-chars               in frame f_rpt_42_sdo_cta_ctbl_balanct = 01.13
           ed_1x40:width-chars              in frame f_rpt_42_sdo_cta_ctbl_balanct = 38.00
           ed_1x40:height-chars             in frame f_rpt_42_sdo_cta_ctbl_balanct = 01.00
           ls_order:width-chars             in frame f_rpt_42_sdo_cta_ctbl_balanct = 30.00
           ls_order:height-chars            in frame f_rpt_42_sdo_cta_ctbl_balanct = 05.00
           rt_cxcf:width-chars              in frame f_rpt_42_sdo_cta_ctbl_balanct = 86.50
           rt_cxcf:height-chars             in frame f_rpt_42_sdo_cta_ctbl_balanct = 01.40
           rt_dimensions:width-chars        in frame f_rpt_42_sdo_cta_ctbl_balanct = 20.57
           rt_dimensions:height-chars       in frame f_rpt_42_sdo_cta_ctbl_balanct = 03.00
           rt_order:width-chars             in frame f_rpt_42_sdo_cta_ctbl_balanct = 39.00
           rt_order:height-chars            in frame f_rpt_42_sdo_cta_ctbl_balanct = 06.00
           rt_parameters_label:width-chars  in frame f_rpt_42_sdo_cta_ctbl_balanct = 39.00
           rt_parameters_label:height-chars in frame f_rpt_42_sdo_cta_ctbl_balanct = 06.50
           rt_run:width-chars               in frame f_rpt_42_sdo_cta_ctbl_balanct = 25.00
           rt_run:height-chars              in frame f_rpt_42_sdo_cta_ctbl_balanct = 03.00
           rt_select:width-chars            in frame f_rpt_42_sdo_cta_ctbl_balanct = 46.57
           rt_select:height-chars           in frame f_rpt_42_sdo_cta_ctbl_balanct = 06.00
           rt_target:width-chars            in frame f_rpt_42_sdo_cta_ctbl_balanct = 46.57
           rt_target:height-chars           in frame f_rpt_42_sdo_cta_ctbl_balanct = 03.00.
&if '{&emsbas_version}' >= '5.06' &then
if OPSYS = 'WIN32':U then do:
assign br_dwb_rpt_select_range:ALLOW-COLUMN-SEARCHING in frame f_rpt_42_sdo_cta_ctbl_balanct = no
       br_dwb_rpt_select_range:COLUMN-MOVABLE in frame f_rpt_42_sdo_cta_ctbl_balanct = no.
end.
&endif
    /* set return-inserted = yes for editors */
    assign ed_1x40:return-inserted in frame f_rpt_42_sdo_cta_ctbl_balanct = yes.
    /* set private-data for the help system */
    assign ls_order:private-data                          in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000016972":U
           bt_up:private-data                             in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000009438":U
           bt_down:private-data                           in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000009436":U
           bt_zoo_188325:private-data                     in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000009431":U
           bt_zoo_188325:private-data                     in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000009431":U
           unid_organ.cod_unid_organ:private-data         in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000009591":U
           bt_zoo_177633:private-data                     in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000009431":U
           plano_cta_ctbl.cod_plano_cta_ctbl:private-data in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000011125":U
           bt_zoo_177628:private-data                     in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000009431":U
           cenar_ctbl.cod_cenar_ctbl:private-data         in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000005310":U
           bt_zoo_177629:private-data                     in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000009431":U
           exerc_ctbl.cod_exerc_ctbl:private-data         in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000007309":U
           period_ctbl.num_period_ctbl:private-data       in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000007364":U
           bt_zoo_218012:private-data                     in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000009431":U
           plano_ccusto.cod_plano_ccusto:private-data     in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000005518":U
           bt_mais:private-data                           in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000008800":U
           bt_planilha:private-data                       in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000021613":U
           br_dwb_rpt_select_range:private-data           in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000016972":U
           bt_isl1:private-data                           in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000008786":U
           bt_edl1:private-data                           in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000008790":U
           bt_rml1:private-data                           in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000008792":U
           rs_cod_dwb_output:private-data                 in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000016972":U
           ed_1x40:private-data                           in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000016972":U
           bt_set_printer:private-data                    in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000008785":U
           bt_get_file:private-data                       in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000008782":U
           rs_ind_run_mode:private-data                   in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000016972":U
           v_log_print_par:private-data                   in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000024662":U
           v_qtd_line:private-data                        in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000024737":U
           v_qtd_column:private-data                      in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000024669":U
           bt_imprime:private-data                          in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000009420":U
           /*bt_print:private-data                          in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000010815":U*/
           bt_can:private-data                            in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000011050":U
           bt_hel2:private-data                           in frame f_rpt_42_sdo_cta_ctbl_balanct = "HLP=000011326":U
           frame f_rpt_42_sdo_cta_ctbl_balanct:private-data                                      = "HLP=000016972".
    /* enable function buttons */
    assign bt_zoo_188325:sensitive in frame f_rpt_42_sdo_cta_ctbl_balanct = yes
           bt_zoo_188325:sensitive in frame f_rpt_42_sdo_cta_ctbl_balanct = yes
           bt_zoo_177633:sensitive in frame f_rpt_42_sdo_cta_ctbl_balanct = yes
           bt_zoo_177628:sensitive in frame f_rpt_42_sdo_cta_ctbl_balanct = yes
           bt_zoo_177629:sensitive in frame f_rpt_42_sdo_cta_ctbl_balanct = yes
           bt_zoo_218012:sensitive in frame f_rpt_42_sdo_cta_ctbl_balanct = yes.
    /* move buttons to top */
    bt_zoo_188325:move-to-top().
    bt_zoo_188325:move-to-top().
    bt_zoo_177633:move-to-top().
    bt_zoo_177628:move-to-top().
    bt_zoo_177629:move-to-top().
    bt_zoo_218012:move-to-top().



{include/i_fclfrm.i f_rpt_42_sdo_cta_ctbl_balanct }
/*************************** Frame Definition End ***************************/

/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
def var v_prog_filtro_pdf as handle no-undo.

function getCodTipoRelat returns character in v_prog_filtro_pdf.

run prgtec/btb/btb920aa.py persistent set v_prog_filtro_pdf.

run pi_define_objetos in v_prog_filtro_pdf (frame f_rpt_42_sdo_cta_ctbl_balanct:handle,
                       rs_cod_dwb_output:handle in frame f_rpt_42_sdo_cta_ctbl_balanct,
                       bt_get_file:row in frame f_rpt_42_sdo_cta_ctbl_balanct,
                       bt_get_file:col in frame f_rpt_42_sdo_cta_ctbl_balanct).

&endif
/* tech38629 - Fim da alteraá∆o */


/*********************** User Interface Trigger Begin ***********************/


ON CHOOSE OF bt_down IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:

    run pi_bt_down_rpt_sdo_cta_ctbl_balanct /*pi_bt_down_rpt_sdo_cta_ctbl_balanct*/.
END. /* ON CHOOSE OF bt_down IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON CHOOSE OF bt_edl1 IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:

    if  br_dwb_rpt_select_range:num-selected-rows in frame f_rpt_42_sdo_cta_ctbl_balanct = 1
    then do:
        assign v_log_method = br_dwb_rpt_select_range:fetch-selected-row(1) in frame f_rpt_42_sdo_cta_ctbl_balanct.
        run pi_seleciona_faixa (Input recid(dwb_rpt_select),
                                Input 'pi_isl_rpt_sdo_cta_ctbl_balanct') /*pi_seleciona_faixa*/.
    end /* if */.
END. /* ON CHOOSE OF bt_edl1 IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON CHOOSE OF bt_get_file IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:

    system-dialog get-file v_cod_dwb_file
        title "Imprimir" /*l_imprimir*/ 
        filters '*.rpt' '*.rpt',
                "*.*"  "*.*"
        save-as
        create-test-file
        ask-overwrite.

    assign dwb_rpt_param.cod_dwb_file             = v_cod_dwb_file
           ed_1x40:screen-value in frame f_rpt_42_sdo_cta_ctbl_balanct = v_cod_dwb_file.

END. /* ON CHOOSE OF bt_get_file IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON CHOOSE OF bt_hel2 IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:


    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.


    /* End_Include: i_context_help_frame */

END. /* ON CHOOSE OF bt_hel2 IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON CHOOSE OF bt_isl1 IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:

    &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
        run pi_isl_dwb_rpt_select_range_balanct /*pi_isl_dwb_rpt_select_range_balanct*/.
    &ELSE
        run pi_isl_dwb_rpt_select_range /*pi_isl_dwb_rpt_select_range*/.
    &ENDIF

END. /* ON CHOOSE OF bt_isl1 IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON CHOOSE OF bt_mais IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:

    run pi_bt_mais_rpt_sdo_cta_ctbl_balanct /*pi_bt_mais_rpt_sdo_cta_ctbl_balanct*/.
END. /* ON CHOOSE OF bt_mais IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON CHOOSE OF bt_planilha IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:
    
    assign v_log_balanct_param = no
           v_cod_dwb_file = ed_1x40:screen-value in frame f_rpt_42_sdo_cta_ctbl_balanct.
    if  search("prgfin/fgl/fgl900b.r") = ? and search("prgfin/fgl/fgl900b.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgfin/fgl/fgl900b.p".
        else do:
            message "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgfin/fgl/fgl900b.p"
                   view-as alert-box error buttons ok.
            stop.
        end.
    end.
    else
    do:
        run prgfin/fgl/fgl900b.p /*prg_fnc_sdo_cta_ctbl_balan*/.
    end.
    
END. /* ON CHOOSE OF bt_planilha IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */

/*ON CHOOSE OF bt_print IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:

    /************************* Variable Definition Begin ************************/

    def var v_log_teste
        as logical
        format "Sim/N∆o"
        initial no
        no-undo.


    /************************** Variable Definition End *************************/

    if v_log_ccusto_sum = yes then do:
       if avail plano_ccusto then do:
          for each dwb_rpt_select
              where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
              and dwb_rpt_select.cod_dwb_user = v_cod_dwb_user
              and dwb_rpt_select.cod_dwb_field = "Centro Custo" /*l_centro_custo*/  exclusive-lock:
              run pi_retornar_min_format_2 (Input plano_ccusto.cod_format_ccusto,
                                            output v_cod_ccusto_inic_aux) /*pi_retornar_min_format_2*/.
              run pi_retornar_max_format (Input plano_ccusto.cod_format_ccusto,
                                          output v_cod_ccusto_fim_aux) /*pi_retornar_max_format*/.
          end.
          run pi_open_dwb_rpt_select_range /*pi_open_dwb_rpt_select_range*/.
       end.
    end.
    assign v_log_print = yes v_val_maximum = 0 v_val_current_value = 0. 
END. /* ON CHOOSE OF bt_print IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */*/

ON CHOOSE OF bt_imprime IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:

    /************************* Variable Definition Begin ************************/

    def var v_log_teste
        as logical
        format "Sim/N∆o"
        initial no
        no-undo.

    /************************** Variable Definition End *************************/

    if v_log_ccusto_sum = yes then do:
       if avail plano_ccusto then do:
          for each dwb_rpt_select
              where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
              and dwb_rpt_select.cod_dwb_user = v_cod_dwb_user
              and dwb_rpt_select.cod_dwb_field = "Centro Custo" /*l_centro_custo*/  exclusive-lock:
              run pi_retornar_min_format_2 (Input plano_ccusto.cod_format_ccusto,
                                            output v_cod_ccusto_inic_aux) /*pi_retornar_min_format_2*/.
              run pi_retornar_max_format (Input plano_ccusto.cod_format_ccusto,
                                          output v_cod_ccusto_fim_aux) /*pi_retornar_max_format*/.
          end.
          run pi_open_dwb_rpt_select_range /*pi_open_dwb_rpt_select_range*/.
       end.
    end.
    assign v_log_print = yes v_val_maximum = 0 v_val_current_value = 0.
END.

ON CHOOSE OF bt_rml1 IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:

    if  br_dwb_rpt_select_range:num-selected-rows = 1 and
        br_dwb_rpt_select_range:fetch-selected-row(1)
    then do:
        assign v_rec_dwb_rpt_select = recid(dwb_rpt_select).
        find dwb_rpt_select exclusive-lock
             where recid(dwb_rpt_select) = v_rec_dwb_rpt_select /*cl_dwb_rpt_select_recid of dwb_rpt_select*/ no-error.
        delete dwb_rpt_select.
        run pi_open_dwb_rpt_select_range /*pi_open_dwb_rpt_select_range*/.
    end /* if */.
END. /* ON CHOOSE OF bt_rml1 IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON CHOOSE OF bt_set_printer IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:

    assign v_nom_dwb_printer      = ""
           v_cod_dwb_print_layout = "".
    &if '{&emsbas_version}' <= '1.00' &then
    if  search("prgtec/btb/btb036nb.r") = ? and search("prgtec/btb/btb036nb.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgtec/btb/btb036nb.p".
        else do:
            message "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgtec/btb/btb036nb.p"
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
            return "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgtec/btb/btb036zb.p".
        else do:
            message "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgtec/btb/btb036zb.p"
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
        assign dwb_rpt_param.nom_dwb_printer           = v_nom_dwb_printer
               dwb_rpt_param.cod_dwb_print_layout      = v_cod_dwb_print_layout
    &if '{&emsbas_version}' > '1.00' &then           
    &if '{&emsbas_version}' >= '5.03' &then           
               dwb_rpt_param.nom_dwb_print_file        = v_nom_dwb_print_file
    &else
               dwb_rpt_param.cod_livre_1               = v_nom_dwb_print_file
    &endif
    &endif
               ed_1x40:screen-value in frame f_rpt_42_sdo_cta_ctbl_balanct = v_nom_dwb_printer
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
                with frame f_rpt_42_sdo_cta_ctbl_balanct.
    end /* if */.

END. /* ON CHOOSE OF bt_set_printer IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON CHOOSE OF bt_up IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:

    assign v_cod_dwb_field = ls_order:screen-value in frame f_rpt_42_sdo_cta_ctbl_balanct
           v_cod_dwb_order = ls_order:list-items in frame f_rpt_42_sdo_cta_ctbl_balanct
           v_num_entry = lookup(v_cod_dwb_field, v_cod_dwb_order).

    if  v_num_entry > (1 + 0)
    then do:
        assign entry(v_num_entry, v_cod_dwb_order) = entry(v_num_entry - 1, v_cod_dwb_order)
               entry(v_num_entry - 1, v_cod_dwb_order) = v_cod_dwb_field
               ls_order:list-items in frame f_rpt_42_sdo_cta_ctbl_balanct = v_cod_dwb_order
               ls_order:screen-value in frame f_rpt_42_sdo_cta_ctbl_balanct = v_cod_dwb_field.
        find dwb_rpt_param where recid(dwb_rpt_param) = v_rec_dwb_rpt_param exclusive-lock no-error.
        if  avail dwb_rpt_param
        then do:
            assign dwb_rpt_param.cod_dwb_order = v_cod_dwb_order.
        end /* if */.
    end /* if */.
END. /* ON CHOOSE OF bt_up IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON LEAVE OF ed_1x40 IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:

    /************************* Variable Definition Begin ************************/

    def var v_cod_filename_final             as character       no-undo. /*local*/
    def var v_cod_filename_initial           as character       no-undo. /*local*/


    /************************** Variable Definition End *************************/

    block:
    do with frame f_rpt_42_sdo_cta_ctbl_balanct:
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
                   /* ASSIGN FILE-INFO:FILE-NAME = v_cod_arq_planilha.
                    if  file-info:file-type = ?
                    then do:
                         /* O diret¢rio &1 N∆o existe ! */
                         run pi_messages (input "show",
                                          input 4354,
                                          input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                             v_cod_filename_final)) /*msg_4354*/.
                         return no-apply.
                    end /* if */. */
                end /* if */.
            end /* if */.
            assign dwb_rpt_param.cod_dwb_file = ed_1x40:screen-value.
        end /* if */.
    end /* do block */.

END. /* ON LEAVE OF ed_1x40 IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON VALUE-CHANGED OF rs_cod_dwb_output IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:

    initout:
    do with frame f_rpt_42_sdo_cta_ctbl_balanct:
        /* block: */
        case self:screen-value:
            when "Terminal" /*l_terminal*/ then ter:
             do:
                if  rs_cod_dwb_output <> "Impressora" /*l_printer*/ 
                then do:
                    assign v_qtd_line_ant = input frame f_rpt_42_sdo_cta_ctbl_balanct v_qtd_line.
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
                        with frame f_rpt_42_sdo_cta_ctbl_balanct.
                assign ed_1x40:screen-value   = ""
                       ed_1x40:sensitive      = no
                       ed_1x40:VISIBLE        = NO
                       bt_get_file:visible    = no
                       bt_set_printer:visible = no
                       bt_planilha:SENSITIVE  = NO.
            end /* do ter */.
            when "Arquivo" /*l_file*/ then fil:
             do:
                if  rs_cod_dwb_output <> "Impressora" /*l_printer*/ 
                then do:
                    assign v_qtd_line_ant = input frame f_rpt_42_sdo_cta_ctbl_balanct v_qtd_line.
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
                        with frame f_rpt_42_sdo_cta_ctbl_balanct.
                assign ed_1x40:sensitive      = yes
                       ed_1x40:VISIBLE        = NO
                       bt_set_printer:visible = no
                       bt_get_file:visible    = NO
                       bt_planilha:SENSITIVE  = YES.

                /* define arquivo default */
                find usuar_mestre no-lock
                     where usuar_mestre.cod_usuario = v_cod_dwb_user
    &if "{&emsbas_version}" >= "5.01" &then
                     use-index srmstr_id
    &endif
                      /*cl_current_user of usuar_mestre*/ no-error.
                assign dwb_rpt_param.cod_dwb_file = "".

                if  rs_ind_run_mode:screen-value in frame f_rpt_42_sdo_cta_ctbl_balanct <> "Batch" /*l_batch*/ 
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
                                                      + caps("fgl900a":U)
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
                       v_nom_dwb_print_file                = ""
    &endif
    .
            end /* do fil */.
            when "Impressora" /*l_printer*/ then prn:
             do:
                if  rs_cod_dwb_output <> "Impressora" /*l_printer*/  and rs_ind_run_mode <> "Batch" /*l_batch*/ 
                then do: 
                    assign v_qtd_line_ant = input frame f_rpt_42_sdo_cta_ctbl_balanct v_qtd_line.
                end /* if */.
                assign ed_1x40:VISIBLE          = YES
                       ed_1x40:sensitive        = no
                       bt_get_file:visible      = no
                       bt_planilha:SENSITIVE    = NO
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
                        with frame f_rpt_42_sdo_cta_ctbl_balanct.
            end /* do prn */.
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
                with frame f_rpt_42_sdo_cta_ctbl_balanct.
    end /* if */.
    else do:
        enable v_qtd_line
               with frame f_rpt_42_sdo_cta_ctbl_balanct.
    end /* else */.
    assign rs_cod_dwb_output.

END. /* ON VALUE-CHANGED OF rs_cod_dwb_output IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON VALUE-CHANGED OF rs_ind_run_mode IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:

    assign dwb_rpt_param.ind_dwb_run_mode = input frame f_rpt_42_sdo_cta_ctbl_balanct rs_ind_run_mode.
    if  dwb_rpt_param.ind_dwb_run_mode = "Batch" /*l_batch*/ 
    then do:
        if  rs_cod_dwb_output:DISABLE("Terminal" /*l_terminal*/ ) in frame f_rpt_42_sdo_cta_ctbl_balanct
        then do:
        end /* if */.
    end /* if */.
    else do:
        if  rs_cod_dwb_output:enable("Terminal" /*l_terminal*/ ) in frame f_rpt_42_sdo_cta_ctbl_balanct
        then do:
            ASSIGN rs_cod_dwb_output:SCREEN-VALUE = "Terminal".
        end /* if */.
    end /* else */.
    if  rs_ind_run_mode = "Batch" /*l_batch*/ 
    then do:
        assign v_qtd_line = v_qtd_line_ant.
        display v_qtd_line
                with frame f_rpt_42_sdo_cta_ctbl_balanct.
    end /* if */.
    assign rs_ind_run_mode.
    apply "value-changed" to rs_cod_dwb_output in frame f_rpt_42_sdo_cta_ctbl_balanct.

END. /* ON VALUE-CHANGED OF rs_ind_run_mode IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON LEAVE OF cenar_ctbl.cod_cenar_ctbl IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:

    apply "leave" to period_ctbl.num_period_ctbl in frame f_rpt_42_sdo_cta_ctbl_balanct.
END. /* ON LEAVE OF cenar_ctbl.cod_cenar_ctbl IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON LEAVE OF exerc_ctbl.cod_exerc_ctbl IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:

    apply "leave" to period_ctbl.num_period_ctbl in frame f_rpt_42_sdo_cta_ctbl_balanct.

END. /* ON LEAVE OF exerc_ctbl.cod_exerc_ctbl IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON LEAVE OF period_ctbl.num_period_ctbl IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:

    find unid_organ no-lock where unid_organ.cod_unid_organ = input frame f_rpt_42_sdo_cta_ctbl_balanct unid_organ.cod_unid_organ no-error.
    if  avail unid_organ
    then do:
        assign v_cod_unid_organ = unid_organ.cod_unid_organ
        v_rec_unid_organ = recid(unid_organ).
        if  unid_organ.num_niv_unid_organ = 998
        then do:
            assign v_cod_cenar_ctbl  = input frame f_rpt_42_sdo_cta_ctbl_balanct cenar_ctbl.cod_cenar_ctbl
            v_cod_exerc_ctbl  = input frame f_rpt_42_sdo_cta_ctbl_balanct exerc_ctbl.cod_exerc_ctbl
            v_num_period_ctbl = input frame f_rpt_42_sdo_cta_ctbl_balanct period_ctbl.num_period_ctbl.
            find period_ctbl no-lock
               where period_ctbl.cod_cenar_ctbl = v_cod_cenar_ctbl
                 and period_ctbl.cod_exerc_ctbl = v_cod_exerc_ctbl
                 and period_ctbl.num_period_ctbl = v_num_period_ctbl no-error.
            if available period_ctbl then assign v_dat_inic_period_ctbl = period_ctbl.dat_inic_period_ctbl.          
              if v_cod_armaz_unid <> input frame f_rpt_42_sdo_cta_ctbl_balanct unid_organ.cod_unid_organ and v_num_cont > 1 then do:
                 run pi_retornar_plano_ccusto_ult (Input v_cod_unid_organ,
                                                   Input v_dat_inic_period_ctbl,
                                                   output v_cod_plano_ccusto,
                                                   output v_log_plano_ccusto_uni) /*pi_retornar_plano_ccusto_ult*/.
              end.                                                   
              else do:
                 assign v_cod_ccusto = v_cod_plano_ccusto.
                 run pi_retornar_plano_ccusto_ult (Input v_cod_unid_organ,
                                                   Input v_dat_inic_period_ctbl,
                                                   output v_cod_plano_ccusto,
                                                   output v_log_plano_ccusto_uni) /*pi_retornar_plano_ccusto_ult*/.
                 if v_cod_ccusto <> ' ' then
                    assign v_cod_plano_ccusto = v_cod_ccusto.
              end.
              assign v_num_cont = v_num_cont + 1.
              assign plano_ccusto.cod_plano_ccusto:screen-value in frame f_rpt_42_sdo_cta_ctbl_balanct = v_cod_plano_ccusto
                     v_cod_empresa     = unid_organ.cod_unid_organ
                     v_cod_plano_antes = v_cod_plano_ccusto.
              enable plano_ccusto.cod_plano_ccusto
                     bt_zoo_218012
                     with frame f_rpt_42_sdo_cta_ctbl_balanct.
        end /* if */.
        else do:
            assign plano_ccusto.cod_plano_ccusto:screen-value in frame f_rpt_42_sdo_cta_ctbl_balanct = ""
            v_cod_empresa = "".
            disable plano_ccusto.cod_plano_ccusto
                    bt_zoo_218012
                    with frame f_rpt_42_sdo_cta_ctbl_balanct.
        end /* else */.
    end /* if */.
END. /* ON LEAVE OF period_ctbl.num_period_ctbl IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON LEAVE OF plano_ccusto.cod_plano_ccusto IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:

    find plano_ccusto no-lock
       where plano_ccusto.cod_plano_ccusto = input frame f_rpt_42_sdo_cta_ctbl_balanct plano_ccusto.cod_plano_ccusto
       and   plano_ccusto.cod_empresa = unid_organ.cod_unid_organ no-error.
    if avail plano_ccusto then
        assign v_rec_plano_ccusto = recid(plano_ccusto)
        v_cod_plano_ccusto = plano_ccusto.cod_plano_ccusto
        v_cod_armaz_unid   = unid_organ.cod_unid_organ.

END. /* ON LEAVE OF plano_ccusto.cod_plano_ccusto IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON LEAVE OF plano_cta_ctbl.cod_plano_cta_ctbl IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:

    find plano_cta_ctbl no-lock
       where plano_cta_ctbl.cod_plano_cta_ctbl = input frame f_rpt_42_sdo_cta_ctbl_balanct plano_cta_ctbl.cod_plano_cta_ctbl no-error.
    if  avail plano_cta_ctbl
    then do:
        assign v_rec_plano_cta_ctbl = recid(plano_cta_ctbl)
        v_cod_plano_cta_ctbl = plano_cta_ctbl.cod_plano_cta_ctbl.
       find current dwb_rpt_select  no-lock no-error. 
          if avail dwb_rpt_select then assign v_row_plano = rowid(dwb_rpt_select).
          for each dwb_rpt_select  
              where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
                and dwb_rpt_select.cod_dwb_user = v_cod_dwb_user exclusive-lock :
               if dwb_rpt_select.cod_dwb_field = "Conta Cont†bil" /*l_conta_contabil*/  then do:            
                    run pi_retorna_conta ( input-output dwb_rpt_select.cod_dwb_initial, input plano_cta_ctbl.cod_format_cta_ctbl).
                    run pi_retorna_conta ( input-output dwb_rpt_select.cod_dwb_final, input plano_cta_ctbl.cod_format_cta_ctbl).
             end. 
         end.                 
         find dwb_rpt_select where rowid(dwb_rpt_select) = v_row_plano no-lock no-error. 
    end /* if */.
END. /* ON LEAVE OF plano_cta_ctbl.cod_plano_cta_ctbl IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON ENTRY OF unid_organ.cod_unid_organ IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:

    apply "leave" /*l_leave*/  to period_ctbl.num_period_ctbl in frame f_rpt_42_sdo_cta_ctbl_balanct.
END. /* ON ENTRY OF unid_organ.cod_unid_organ IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON LEAVE OF unid_organ.cod_unid_organ IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:

    assign v_cod_armaz_unid = unid_organ.cod_unid_organ.
    apply "leave" to period_ctbl.num_period_ctbl in frame f_rpt_42_sdo_cta_ctbl_balanct.
END. /* ON LEAVE OF unid_organ.cod_unid_organ IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */


/************************ User Interface Trigger End ************************/

/************************** Function Trigger Begin **************************/


ON  CHOOSE OF bt_zoo_177628 IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
OR F5 OF cenar_ctbl.cod_cenar_ctbl IN FRAME f_rpt_42_sdo_cta_ctbl_balanct DO:

    /* fn_generic_zoom */
    if  search("prgint/utb/utb076ka.r") = ? and search("prgint/utb/utb076ka.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb076ka.p".
        else do:
            message "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb076ka.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run prgint/utb/utb076ka.p /*prg_sea_cenar_ctbl*/.
    if  v_rec_cenar_ctbl <> ?
    then do:
        find cenar_ctbl where recid(cenar_ctbl) = v_rec_cenar_ctbl no-lock no-error.
        assign cenar_ctbl.cod_cenar_ctbl:screen-value in frame f_rpt_42_sdo_cta_ctbl_balanct =
               string(cenar_ctbl.cod_cenar_ctbl).

    end /* if */.
    apply "entry" to cenar_ctbl.cod_cenar_ctbl in frame f_rpt_42_sdo_cta_ctbl_balanct.
end. /* ON  CHOOSE OF bt_zoo_177628 IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON  CHOOSE OF bt_zoo_177629 IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
OR F5 OF exerc_ctbl.cod_exerc_ctbl IN FRAME f_rpt_42_sdo_cta_ctbl_balanct DO:

    /* fn_generic_zoom */
    if  search("prgint/utb/utb075ka.r") = ? and search("prgint/utb/utb075ka.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb075ka.p".
        else do:
            message "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb075ka.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run prgint/utb/utb075ka.p /*prg_sea_exerc_ctbl*/.
    if  v_rec_exerc_ctbl <> ?
    then do:
        find exerc_ctbl where recid(exerc_ctbl) = v_rec_exerc_ctbl no-lock no-error.
        assign exerc_ctbl.cod_exerc_ctbl:screen-value in frame f_rpt_42_sdo_cta_ctbl_balanct =
               string(exerc_ctbl.cod_exerc_ctbl).

    end /* if */.
    apply "entry" to exerc_ctbl.cod_exerc_ctbl in frame f_rpt_42_sdo_cta_ctbl_balanct.
end. /* ON  CHOOSE OF bt_zoo_177629 IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON  CHOOSE OF bt_zoo_177633 IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
OR F5 OF plano_cta_ctbl.cod_plano_cta_ctbl IN FRAME f_rpt_42_sdo_cta_ctbl_balanct DO:

    /* fn_generic_zoom */
    if  search("prgint/utb/utb080ka.r") = ? and search("prgint/utb/utb080ka.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb080ka.p".
        else do:
            message "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb080ka.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run prgint/utb/utb080ka.p /*prg_sea_plano_cta_ctbl*/.
    if  v_rec_plano_cta_ctbl <> ?
    then do:
        find plano_cta_ctbl where recid(plano_cta_ctbl) = v_rec_plano_cta_ctbl no-lock no-error.
        assign plano_cta_ctbl.cod_plano_cta_ctbl:screen-value in frame f_rpt_42_sdo_cta_ctbl_balanct =
               string(plano_cta_ctbl.cod_plano_cta_ctbl).

    end /* if */.
    apply "entry" to plano_cta_ctbl.cod_plano_cta_ctbl in frame f_rpt_42_sdo_cta_ctbl_balanct.
end. /* ON  CHOOSE OF bt_zoo_177633 IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON  CHOOSE OF bt_zoo_188325 IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
OR F5 OF unid_organ.cod_unid_organ IN FRAME f_rpt_42_sdo_cta_ctbl_balanct DO:

    /* fn_generic_zoom */
    if  search("prgint/utb/utb010ka.r") = ? and search("prgint/utb/utb010ka.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb010ka.p".
        else do:
            message "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb010ka.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run prgint/utb/utb010ka.p /*prg_sea_unid_organ*/.
    if  v_rec_unid_organ <> ?
    then do:
        find unid_organ where recid(unid_organ) = v_rec_unid_organ no-lock no-error.
        assign unid_organ.cod_unid_organ:screen-value in frame f_rpt_42_sdo_cta_ctbl_balanct =
               string(unid_organ.cod_unid_organ).

    end /* if */.
    apply "entry" to unid_organ.cod_unid_organ in frame f_rpt_42_sdo_cta_ctbl_balanct.
end. /* ON  CHOOSE OF bt_zoo_188325 IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON  CHOOSE OF bt_zoo_218012 IN FRAME f_rpt_42_sdo_cta_ctbl_balanct
OR F5 OF plano_ccusto.cod_plano_ccusto IN FRAME f_rpt_42_sdo_cta_ctbl_balanct DO:

    /* fn_generic_zoom */
    if  search("prgint/utb/utb083ka.r") = ? and search("prgint/utb/utb083ka.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb083ka.p".
        else do:
            message "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb083ka.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run prgint/utb/utb083ka.p /*prg_sea_plano_ccusto*/.
    if  v_rec_plano_ccusto <> ?
    then do:
        find plano_ccusto where recid(plano_ccusto) = v_rec_plano_ccusto no-lock no-error.
        assign plano_ccusto.cod_plano_ccusto:screen-value in frame f_rpt_42_sdo_cta_ctbl_balanct =
               string(plano_ccusto.cod_plano_ccusto).

    end /* if */.
    apply "entry" to plano_ccusto.cod_plano_ccusto in frame f_rpt_42_sdo_cta_ctbl_balanct.
end. /* ON  CHOOSE OF bt_zoo_218012 IN FRAME f_rpt_42_sdo_cta_ctbl_balanct */


/*************************** Function Trigger End ***************************/

/**************************** Frame Trigger Begin ***************************/


ON ENDKEY OF FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:


    /* Begin_Include: i_exec_program_epc */
    &if '{&emsbas_version}' > '1.00' &then
    if  v_nom_prog_upc <> '' then
    do:
        assign v_rec_table_epc = recid(sdo_cta_ctbl).    
        run value(v_nom_prog_upc) (input 'CANCEL',
                                   input 'viewer',
                                   input this-procedure,
                                   input v_wgh_frame_epc,
                                   input v_nom_table_epc,
                                   input v_rec_table_epc).
        if  'no' = 'yes'
        and return-value = 'NOK' then
            undo, retry.
    end.

    if  v_nom_prog_appc <> '' then
    do:
        assign v_rec_table_epc = recid(sdo_cta_ctbl).    
        run value(v_nom_prog_appc) (input 'CANCEL',
                                    input 'viewer',
                                    input this-procedure,
                                    input v_wgh_frame_epc,
                                    input v_nom_table_epc,
                                    input v_rec_table_epc).
        if  'no' = 'yes'
        and return-value = 'NOK' then
            undo, retry.
    end.

    &if '{&emsbas_version}' > '5.00' &then
    if  v_nom_prog_dpc <> '' then
    do:
        assign v_rec_table_epc = recid(sdo_cta_ctbl).    
        run value(v_nom_prog_dpc) (input 'CANCEL',
                                    input 'viewer',
                                    input this-procedure,
                                    input v_wgh_frame_epc,
                                    input v_nom_table_epc,
                                    input v_rec_table_epc).
        if  'no' = 'yes'
        and return-value = 'NOK' then
            undo, retry.
    end.
    &endif
    &endif
    /* End_Include: i_exec_program_epc */

END. /* ON ENDKEY OF FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON GO OF FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:

    assign dwb_rpt_param.cod_dwb_output     = rs_cod_dwb_output:screen-value in frame f_rpt_42_sdo_cta_ctbl_balanct
           dwb_rpt_param.qtd_dwb_line       = input frame f_rpt_42_sdo_cta_ctbl_balanct v_qtd_line
    &if '{&emsbas_version}' > '1.00' &then
    &if '{&emsbas_version}' >= '5.03' &then
           dwb_rpt_param.nom_dwb_print_file = v_nom_dwb_print_file
    &else
           dwb_rpt_param.cod_livre_1 = v_nom_dwb_print_file
    &endif
    &endif
    .
    if  dwb_rpt_param.cod_dwb_output = "Arquivo" /*l_file*/ 
    then do:

         ASSIGN ed_1x40:SCREEN-VALUE = v_cod_arq_planilha.
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
             /*
             ASSIGN FILE-INFO:FILE-NAME = v_cod_arq_planilha.
             if  (  file-info:file-type = ? )
             and    (  index  ( dwb_rpt_param.cod_dwb_file ,'~\') <> 0
                          or
                       index  ( dwb_rpt_param.cod_dwb_file ,'/')  <> 0
                          or
                       index  ( dwb_rpt_param.cod_dwb_file ,':')  <> 0
                     )
             then do:
                 /* O diret¢rio &1 N∆o existe ! */
                 run pi_messages (input "show",
                                  input 4354,
                                  input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                     file-info:file-name)) /*msg_4354*/.
                 return no-apply.
              end /* if */.
              */
         end /* if */.
                         /*
         if  return-value = "NOK" /*l_nok*/ 
         then do:
             /* Nome do arquivo incorreto ! */
             run pi_messages (input "show",
                              input 1064,
                              input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_1064*/.
             return no-apply.
         end /* if */.
         */
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
                /* impressora destino e layout de impress∆o N∆o definidos ! */
                run pi_messages (input "show",
                                 input 2052,
                                 input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_2052*/.
                return no-apply.
            end /* if */.
        end /* if */.
    end /* else */.

    find first dwb_rpt_select no-lock
         where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
           and dwb_rpt_select.cod_dwb_user = v_cod_dwb_user
           and dwb_rpt_select.log_dwb_rule = yes /*cl_dwb_rpt_select_rule of dwb_rpt_select*/ no-error.
    if  not avail dwb_rpt_select
    then do:
        /* Incluir ao menos uma regra antes de imprimir. */
        run pi_messages (input "show",
                         input 874,
                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_874*/.
        return no-apply.
    end /* if */.

END. /* ON GO OF FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON HELP OF FRAME f_rpt_42_sdo_cta_ctbl_balanct ANYWHERE
DO:


    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */

END. /* ON HELP OF FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON RIGHT-MOUSE-DOWN OF FRAME f_rpt_42_sdo_cta_ctbl_balanct ANYWHERE
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

END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON RIGHT-MOUSE-UP OF FRAME f_rpt_42_sdo_cta_ctbl_balanct ANYWHERE
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

END. /* ON RIGHT-MOUSE-UP OF FRAME f_rpt_42_sdo_cta_ctbl_balanct */

ON WINDOW-CLOSE OF FRAME f_rpt_42_sdo_cta_ctbl_balanct
DO:

    apply "end-error" to self.
END. /* ON WINDOW-CLOSE OF FRAME f_rpt_42_sdo_cta_ctbl_balanct */


/***************************** Frame Trigger End ****************************/

/**************************** Menu Trigger Begin ****************************/


ON CHOOSE OF MENU-ITEM mi_conteudo IN MENU m_help
DO:


        apply "choose" to bt_hel2 in frame f_rpt_42_sdo_cta_ctbl_balanct.





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


        assign v_nom_prog     = substring(frame f_rpt_42_sdo_cta_ctbl_balanct:title, 1, max(1, length(frame f_rpt_42_sdo_cta_ctbl_balanct:title) - 10)).
        if  v_nom_prog = ? then
            assign v_nom_prog = "".

        assign v_nom_prog     = v_nom_prog
                              + chr(10)
                              + "rpt_sdo_cta_ctbl_balanct":U.




    assign v_nom_prog_ext = "prgfin/fgl/fgl900a.p":U
           v_cod_release  = trim(" 1.00.01.076":U).
/*    run prgtec/btb/btb901zb.p (Input v_nom_prog,
                               Input v_nom_prog_ext,
                               Input v_cod_release) /*prg_fnc_about*/. */
{include/sobre5.i}
END. /* ON CHOOSE OF MENU-ITEM mi_sobre IN MENU m_help */


/***************************** Menu Trigger End *****************************/


/****************************** Main Code Begin *****************************/


/* Begin_Include: i_version_extract */
/* {include/i-ctrlrp5.i rpt_sdo_cta_ctbl_balanct} */


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
    run pi_version_extract ('rpt_sdo_cta_ctbl_balanct':U, 'prgfin/fgl/fgl900a.p':U, '1.00.01.076':U, 'pro':U).
end /* if */.



/* End_Include: i_version_extract */

run pi_return_user (output v_cod_dwb_user) /*pi_return_user*/.

if  search("prgtec/btb/btb906za.r") = ? and search("prgtec/btb/btb906za.py") = ? then do:
    if  v_cod_dwb_user begins 'es_' then
        return "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgtec/btb/btb906za.py".
    else do:
        message "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgtec/btb/btb906za.py"
               view-as alert-box error buttons ok.
        stop.
    end.
end.
else
    run prgtec/btb/btb906za.py /*prg_fnc_verify_controls*/.
if  v_cod_dwb_user = ""
then do:
    assign v_cod_dwb_user = v_cod_usuar_corren.
end /* if */.

/* Begin_Include: i_verify_security */
if  search("prgtec/men/men901za.r") = ? and search("prgtec/men/men901za.py") = ? then do:
    if  v_cod_dwb_user begins 'es_' then
        return "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgtec/men/men901za.py".
    else do:
        message "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgtec/men/men901za.py"
               view-as alert-box error buttons ok.
        return.
    end.
end.
else
    run prgtec/men/men901za.py (Input 'rpt_sdo_cta_ctbl_balanct') /*prg_fnc_verify_security*/.
if  return-value = "2014"
then do:
    /* Programa a ser executado N∆o Ç um programa v†lido Datasul ! */
    run pi_messages (input "show",
                     input 2014,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'rpt_sdo_cta_ctbl_balanct')) /*msg_2014*/.
    return.
end /* if */.
if  return-value = "2012"
then do:
    /* Usu†rio sem permiss∆o para acessar o programa. */
    run pi_messages (input "show",
                     input 2012,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'rpt_sdo_cta_ctbl_balanct')) /*msg_2012*/.
    return.
end /* if */.
/* End_Include: i_verify_security */


/* Begin_Include: i_log_exec_prog_dtsul_ini */
assign v_rec_log = ?.

if can-find(prog_dtsul
       where prog_dtsul.cod_prog_dtsul = 'rpt_sdo_cta_ctbl_balanct' 
         and prog_dtsul.log_gera_log_exec = yes) then do transaction:
    create log_exec_prog_dtsul.
    assign log_exec_prog_dtsul.cod_prog_dtsul           = 'rpt_sdo_cta_ctbl_balanct'
           log_exec_prog_dtsul.cod_usuario              = v_cod_usuar_corren
           log_exec_prog_dtsul.dat_inic_exec_prog_dtsul = today
           log_exec_prog_dtsul.hra_inic_exec_prog_dtsul = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":":U,"":U).
    assign v_rec_log = recid(log_exec_prog_dtsul).
    release log_exec_prog_dtsul no-error.
end.


/* End_Include: i_log_exec_prog_dtsul_ini */

/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
run pi_permissoes in v_prog_filtro_pdf (input 'rpt_sdo_cta_ctbl_balanct':U).
&endif
/* tech38629 - Fim da alteraá∆o */




/* Begin_Include: i_verify_program_epc */
&if '{&emsbas_version}' > '1.00' &then
assign v_rec_table_epc = ?
       v_wgh_frame_epc = ?.

find prog_dtsul
    where prog_dtsul.cod_prog_dtsul = "rpt_sdo_cta_ctbl_balanct":U
    no-lock no-error.
if  avail prog_dtsul then do:
    if  prog_dtsul.nom_prog_upc <> ''
    and prog_dtsul.nom_prog_upc <> ? then
        assign v_nom_prog_upc = prog_dtsul.nom_prog_upc.
    if  prog_dtsul.nom_prog_appc <> ''
    and prog_dtsul.nom_prog_appc <> ? then
        assign v_nom_prog_appc = prog_dtsul.nom_prog_appc.
&if '{&emsbas_version}' > '5.00' &then
    if  prog_dtsul.nom_prog_dpc <> ''
    and prog_dtsul.nom_prog_dpc <> ? then
        assign v_nom_prog_dpc = prog_dtsul.nom_prog_dpc.
&endif
end.


assign v_wgh_frame_epc = frame f_rpt_42_sdo_cta_ctbl_balanct:handle.



assign v_nom_table_epc = 'sdo_cta_ctbl':U
       v_rec_table_epc = recid(sdo_cta_ctbl).

&endif

/* End_Include: i_verify_program_epc */


/* redefiniá¥es do frame */

/* Begin_Include: i_std_dialog_box */
/* tratamento do titulo e vers∆o */
assign frame f_rpt_42_sdo_cta_ctbl_balanct:title = frame f_rpt_42_sdo_cta_ctbl_balanct:title
                            + chr(32)
                            + chr(40)
                            + trim(" 1.00.01.076":U)
                            + chr(41).
/* menu pop-up de ajuda e sobre */
assign menu m_help:popup-only = yes
       bt_hel2:popup-menu in frame f_rpt_42_sdo_cta_ctbl_balanct = menu m_help:handle.


/* End_Include: i_std_dialog_box */
{include/title5.i f_rpt_42_sdo_cta_ctbl_balanct FRAME}


/* inicializa vari†veis */
run pi_initialize_reports /*pi_initialize_reports*/.


/* Begin_Include: ix_p01_rpt_sdo_cta_ctbl_balanct */

/* Begin_Include: i_declara_GetDefinedFunction */
FUNCTION GetDefinedFunction RETURNS LOGICAL (INPUT SPP AS CHARACTER):

    DEF VAR v_log_retorno AS LOGICAL INITIAL NO NO-UNDO.

    IF CAN-FIND (FIRST histor_exec_especial NO-LOCK
         WHERE histor_exec_especial.cod_modul_dtsul = "UFN" /* l_ufn*/ 
           AND histor_exec_especial.cod_prog_dtsul  = SPP) THEN
        ASSIGN v_log_retorno = YES.


    /* Begin_Include: i_funcao_extract */
    if  v_cod_arq <> '' and v_cod_arq <> ?
    then do:

        /*output stream s-arq to value(v_cod_arq) append.

        put stream s-arq unformatted
            SPP      at 1 
            v_log_retorno  at 43 skip.

        output stream s-arq close.*/

    end /* if */.
    /* End_Include: i_funcao_extract */
    .

    RETURN v_log_retorno.
END FUNCTION.
/* End_Include: i_declara_GetDefinedFunction */


assign v_log_emis_balanct_por_estrut = &IF DEFINED (BF_FIN_EMIS_BALANCT_POR_ESTRUT) &THEN YES 
                                       &ELSE GetDefinedFunction('SPP_EMIS_BALANCT_POR_ESTRUT':U) &ENDIF.

assign v_log_bloq_fgl = GetDefinedFunction('SPP_BLOQUEIO_FGL').
assign v_log_bloq_fgl_desativ = GetDefinedFunction('SPP_BLOQUEIO_FGL_DESATIV').

&if '{&emsfin_version}' >= '5.05' &then  
if not v_log_bloq_fgl and not v_log_bloq_fgl_desativ
then do transaction:
  create histor_exec_especial.
  assign histor_exec_especial.cod_modul_dtsul = 'UFN'
         histor_exec_especial.cod_prog_dtsul  = 'SPP_BLOQUEIO_FGL'.
end.
&endif    

if not can-find(first histor_exec_especial
    where  histor_exec_especial.cod_modul_dtsul = "FGL" /*l_fgl*/ 
    and    histor_exec_especial.cod_prog_dtsul  = 'spp_habilit_trig_fin_fgl') then do:
    /* Aviso ! */
    run pi_messages (input "show",
                     input 13950,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_13950*/.
end.
else do:
    if not can-find(first histor_exec_especial
                    where histor_exec_especial.cod_modul_dtsul = "FGL" /*l_fgl*/ 
                    and   histor_exec_especial.cod_prog_dtsul  = 'spp_habilit_trig_periodos') then do:
        /* Aviso ! */
        run pi_messages (input "show",
                         input 14313,
                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_14313*/.
    end.
end.
/* End_Include: i_declara_GetDefinedFunction */


if  v_cod_dwb_user begins 'es_'
then do:
    find dwb_rpt_param no-lock
         where dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
           and dwb_rpt_param.cod_dwb_user = v_cod_dwb_user /*cl_dwb_rpt_param of dwb_rpt_param*/ no-error.
    if  not avail dwb_rpt_param
    then do:
        return "ParÉmetros para o relat¢rio N∆o encontrado." /*1993*/ + " (" + "1993" + ")" + chr(10) + "N∆o foi poss°vel encontrar os ParÉmetros necess†rios para a impress∆o do relat¢rio para o programa e usu†rio corrente." /*1993*/.
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
    if  ped_exec.cod_release_prog_dtsul <> trim(" 1.00.01.076":U)
    then do:
        return "Vers¥es do programa diferente." /*1994*/ + " (" + "1994" + ")" + chr(10)
                                     + substitute("A vers∆o do programa (&3) que gerou o pedido de Execuá∆o batch (&1) Ç diferente da vers∆o do programa que deveria executar o pedido batch (&2)." /*1994*/,ped_exec.cod_release_prog_dtsul,
                                                  trim(" 1.00.01.076":U),
                                                  "prgfin/fgl/fgl900a.p":U).
    end /* if */.
    assign v_nom_prog_ext     = caps("fgl900a":U)
           v_dat_execution    = today
           v_hra_execution    = replace(string(time, "hh:mm:ss" /*l_hh:mm:ss*/ ), ":", "")
           v_cod_dwb_file     = dwb_rpt_param.cod_dwb_file
           v_nom_report_title = fill(" ", 40 - length(v_rpt_s_1_name)) + v_rpt_s_1_name
           v_ind_dwb_run_mode = "Batch" /*l_batch*/ 
           v_cod_dwb_order    = dwb_rpt_param.cod_dwb_order.

    /* Begin_Include: ix_p02_rpt_sdo_cta_ctbl_balanct */
    assign v_cod_plano_cta_ctbl = entry(1,dwb_rpt_param.cod_dwb_parameters,chr(10))
    v_cod_cenar_ctbl = entry(2,dwb_rpt_param.cod_dwb_parameters,chr(10))
    v_cod_exerc_ctbl = entry(3,dwb_rpt_param.cod_dwb_parameters,chr(10))
    v_num_period_ctbl = int(entry(4,dwb_rpt_param.cod_dwb_parameters,chr(10)))
    v_log_estab_sum = (entry(5,dwb_rpt_param.cod_dwb_parameters,chr(10)) = 'yes')
    v_log_unid_negoc_sum = (entry(6,dwb_rpt_param.cod_dwb_parameters,chr(10)) = 'yes')
    v_log_ccusto_sum = (entry(7,dwb_rpt_param.cod_dwb_parameters,chr(10)) = 'yes')
    v_cod_finalid_econ_bas = entry(8,dwb_rpt_param.cod_dwb_parameters,chr(10))
    v_cod_finalid_econ_apr = entry(9,dwb_rpt_param.cod_dwb_parameters,chr(10))
    v_dat_cotac_indic_econ = date(entry(10,dwb_rpt_param.cod_dwb_parameters,chr(10)))
    v_log_cta_ctbl_internac = (entry(11,dwb_rpt_param.cod_dwb_parameters,chr(10)) = 'yes')
    v_log_period_ctbl_ant_impr = (entry(12,dwb_rpt_param.cod_dwb_parameters,chr(10)) = 'yes')
    v_log_cta_ctbl_sdo = (entry(13,dwb_rpt_param.cod_dwb_parameters,chr(10)) = 'yes')
    v_log_cta_ctbl_analit = (entry(14,dwb_rpt_param.cod_dwb_parameters,chr(10)) = 'yes')
    v_log_consid_apurac_restdo = (entry(15, dwb_rpt_param.cod_dwb_parameters,chr(10)) = 'yes')
    v_num_niv_estrut = int(entry(16,dwb_rpt_param.cod_dwb_parameters,chr(10)))
    v_cod_idioma_apr = entry(17,dwb_rpt_param.cod_dwb_parameters,chr(10))
    v_rec_dwb_rpt_param = int(entry(18,dwb_rpt_param.cod_dwb_parameters,chr(10)))
    v_log_gerac_planilha = (entry(19,dwb_rpt_param.cod_dwb_parameters,chr(10)) = 'yes')
    v_cod_arq_planilha = entry(20,dwb_rpt_param.cod_dwb_parameters,chr(10))
    v_cod_carac_lim = entry(21,dwb_rpt_param.cod_dwb_parameters,chr(10))
    v_cod_plano_ccusto = entry(22,dwb_rpt_param.cod_dwb_parameters,chr(10))
    v_cod_unid_organ = entry(23,dwb_rpt_param.cod_dwb_parameters,chr(10))
    v_cod_cta_ctbl_pfixa = entry(24, dwb_rpt_param.cod_dwb_parameters,chr(10))
    v_cod_cta_ctbl_excec = entry(25, dwb_rpt_param.cod_dwb_parameters,chr(10))
    v_cod_ccusto_pfixa = entry(26, dwb_rpt_param.cod_dwb_parameters,chr(10))
    v_cod_ccusto_excec = entry(27, dwb_rpt_param.cod_dwb_parameters,chr(10))
    v_log_mostra_sem_aprop_cc = (entry(28,dwb_rpt_param.cod_dwb_parameters,chr(10)) = 'yes').
    &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
      if num-entries(dwb_rpt_param.cod_dwb_parameters,chr(10)) >= 29 then assign v_cod_proj_financ_pfixa = entry(29, dwb_rpt_param.cod_dwb_parameters,chr(10)).
      if num-entries(dwb_rpt_param.cod_dwb_parameters,chr(10)) >= 30 then assign v_cod_proj_financ_excec = entry(30, dwb_rpt_param.cod_dwb_parameters,chr(10)).
      if num-entries(dwb_rpt_param.cod_dwb_parameters,chr(10)) >= 31 then assign v_log_proj_financ = (entry(31,dwb_rpt_param.cod_dwb_parameters,chr(10)) = "yes" /*l_yes*/ ).
    &ENDIF
    &if '{&emsbas_version}' >= '5.05' &then   
     if v_log_eai_habilit then do:
        if num-entries(dwb_rpt_param.cod_dwb_parameters,chr(10)) = 32 then assign v_log_gera_eai = (entry(32,dwb_rpt_param.cod_dwb_parameters,chr(10)) = "yes" /*l_yes*/ ).
        else do:
          if num-entries(dwb_rpt_param.cod_dwb_parameters,chr(10)) = 29 then assign v_log_gera_eai = (entry(29,dwb_rpt_param.cod_dwb_parameters,chr(10)) = "yes" /*l_yes*/ ).
        end.
    end.   
    &endif
    find first b_servid_exec where b_servid_exec.cod_servid_exec = ped_exec.cod_servid_exec no-lock no-error.
    if avail b_servid_exec then assign v_cod_arq_planilha = b_servid_exec.nom_dir_spool + '/' + v_cod_arq_planilha.
    find plano_cta_ctbl no-lock where plano_cta_ctbl.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl no-error.
    /* End_Include: ix_p02_rpt_sdo_cta_ctbl_balanct */

    /* configura e define destino de impress∆o */
    if  dwb_rpt_param.cod_dwb_output = "Impressora" /*l_printer*/ 
    then do:
         assign v_qtd_line_ant = v_qtd_line.
    end /* if */.
    run pi_output_reports /*pi_output_reports*/.
    if  dwb_rpt_param.log_dwb_print_parameters = yes
    then do:
        run pi_print_parameters /*pi_print_parameters*/.

        /* Begin_Include: ix_p30_rpt_sdo_cta_ctbl_balanct */
        run pi_ix_p30_rpt_sdo_cta_ctbl_balanct /*pi_ix_p30_rpt_sdo_cta_ctbl_balanct*/.
        /* End_Include: ix_p30_rpt_sdo_cta_ctbl_balanct */

    end /* if */.
    /*output stream s_1 close.*/
    /* ix_p50_rpt_sdo_cta_ctbl_balanct */    

/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
run pi_call_convert_object in v_prog_filtro_pdf (input yes,
                                                 input dwb_rpt_param.cod_dwb_output,
                                                 input dwb_rpt_param.nom_dwb_print_file,
                                                 input v_cod_dwb_file,
                                                 input v_nom_report_title).
&endif
/* tech38629 - Fim da alteraá∆o */


&if '{&emsbas_version}':U >= '5.05':U &then
    if ((dwb_rpt_param.cod_dwb_output = 'Impressora':U or dwb_rpt_param.cod_dwb_output = 'Impresora':U or dwb_rpt_param.cod_dwb_output = 'printer':U) and getCodTipoRelat() = 'PDF':U) then do:
        if dwb_rpt_param.nom_dwb_print_file = '' then
            run pi_print_pdf_file in v_prog_filtro_pdf (input yes).
    end.
&endif
    return "OK" /*l_ok*/ .
end /* if */.

pause 0 before-hide.
view frame f_rpt_42_sdo_cta_ctbl_balanct.

/* Begin_Include: i_exec_program_epc */
&if '{&emsbas_version}' > '1.00' &then
if  v_nom_prog_upc <> '' then
do:
    assign v_rec_table_epc = recid(sdo_cta_ctbl).    
    run value(v_nom_prog_upc) (input 'INITIALIZE',
                               input 'viewer',
                               input this-procedure,
                               input v_wgh_frame_epc,
                               input v_nom_table_epc,
                               input v_rec_table_epc).
    if  'no' = 'yes'
    and return-value = 'NOK' then
        undo, retry.
end.

if  v_nom_prog_appc <> '' then
do:
    assign v_rec_table_epc = recid(sdo_cta_ctbl).    
    run value(v_nom_prog_appc) (input 'INITIALIZE',
                                input 'viewer',
                                input this-procedure,
                                input v_wgh_frame_epc,
                                input v_nom_table_epc,
                                input v_rec_table_epc).
    if  'no' = 'yes'
    and return-value = 'NOK' then
        undo, retry.
end.

&if '{&emsbas_version}' > '5.00' &then
if  v_nom_prog_dpc <> '' then
do:
    assign v_rec_table_epc = recid(sdo_cta_ctbl).    
    run value(v_nom_prog_dpc) (input 'INITIALIZE',
                                input 'viewer',
                                input this-procedure,
                                input v_wgh_frame_epc,
                                input v_nom_table_epc,
                                input v_rec_table_epc).
    if  'no' = 'yes'
    and return-value = 'NOK' then
        undo, retry.
end.
&endif
&endif
/* End_Include: i_exec_program_epc */


/* Begin_Include: ix_p05_rpt_sdo_cta_ctbl_balanct */
&IF DEFINED(BF_ADM_FIN_PROJ) &THEN
    assign v_cod_dwb_select = "Conta Cont†bil,Alternativa Conta," + &IF '{&emsfin_version}' >= '5.01' AND '{&emsfin_version}' <= '5.07' &THEN "Estabelecimento" &ELSE "Estabelecimento" &ENDIF + ",Unid Neg¢cio,Centro Custo,Projeto".
&ELSE
    assign v_cod_dwb_select = "Conta Cont†bil,Alternativa Conta," + &IF '{&emsfin_version}' >= '5.01' AND '{&emsfin_version}' <= '5.07' &THEN "Estabelecimento" &ELSE "Estabelecimento" &ENDIF + ",Unid Neg¢cio,Centro Custo".
&ENDIF
/* End_Include: ix_p05_rpt_sdo_cta_ctbl_balanct */

super_block:
repeat:
    run pi_configure_dwb_param /*pi_configure_dwb_param*/.
    assign v_qtd_line = dwb_rpt_param.qtd_dwb_line.
    init:
    do with frame f_rpt_42_sdo_cta_ctbl_balanct:
        assign rs_cod_dwb_output:screen-value  = dwb_rpt_param.cod_dwb_output
               rs_ind_run_mode:screen-value    = dwb_rpt_param.ind_dwb_run_mode.
        if  dwb_rpt_param.cod_dwb_output = "Arquivo" /*l_file*/ 
        then do:
            ed_1x40:screen-value = dwb_rpt_param.cod_dwb_file.
        end /* if */.
        if  dwb_rpt_param.cod_dwb_output = "Impressora" /*l_printer*/ 
        then do:
            if  not can-find(imprsor_usuar
                    where imprsor_usuar.nom_impressora = dwb_rpt_param.nom_dwb_printer
                      and imprsor_usuar.cod_usuario = dwb_rpt_param.cod_dwb_user
&if "{&emsbas_version}" >= "5.01" &then
                    use-index imprsrsr_id
&endif
                     /*cl_get_printer of imprsor_usuar*/)
            or   not can-find(layout_impres
                            where layout_impres.nom_impressora = dwb_rpt_param.nom_dwb_printer
                              and layout_impres.cod_layout_impres = dwb_rpt_param.cod_dwb_print_layout /*cl_get_layout of layout_impres*/)
                            then do:
                run pi_set_print_layout_default /*pi_set_print_layout_default*/.
            end /* if */.
            assign ed_1x40:screen-value = dwb_rpt_param.nom_dwb_printer
                                        + ":"
                                        + dwb_rpt_param.cod_dwb_print_layout.
        end /* if */.
        assign v_log_print_par = dwb_rpt_param.log_dwb_print_parameters.
        display v_log_print_par
                with frame f_rpt_42_sdo_cta_ctbl_balanct.
    end /* do init */.
    display v_qtd_column
            v_qtd_line
            with frame f_rpt_42_sdo_cta_ctbl_balanct.

    /* Begin_Include: i_exec_program_epc */
    &if '{&emsbas_version}' > '1.00' &then
    if  v_nom_prog_upc <> '' then
    do:
        assign v_rec_table_epc = recid(sdo_cta_ctbl).    
        run value(v_nom_prog_upc) (input 'DISPLAY',
                                   input 'viewer',
                                   input this-procedure,
                                   input v_wgh_frame_epc,
                                   input v_nom_table_epc,
                                   input v_rec_table_epc).
        if  'no' = 'yes'
        and return-value = 'NOK' then
            undo, retry.
    end.

    if  v_nom_prog_appc <> '' then
    do:
        assign v_rec_table_epc = recid(sdo_cta_ctbl).    
        run value(v_nom_prog_appc) (input 'DISPLAY',
                                    input 'viewer',
                                    input this-procedure,
                                    input v_wgh_frame_epc,
                                    input v_nom_table_epc,
                                    input v_rec_table_epc).
        if  'no' = 'yes'
        and return-value = 'NOK' then
            undo, retry.
    end.

    &if '{&emsbas_version}' > '5.00' &then
    if  v_nom_prog_dpc <> '' then
    do:
        assign v_rec_table_epc = recid(sdo_cta_ctbl).    
        run value(v_nom_prog_dpc) (input 'DISPLAY',
                                    input 'viewer',
                                    input this-procedure,
                                    input v_wgh_frame_epc,
                                    input v_nom_table_epc,
                                    input v_rec_table_epc).
        if  'no' = 'yes'
        and return-value = 'NOK' then
            undo, retry.
    end.
    &endif
    &endif
    /* End_Include: i_exec_program_epc */

    enable ls_order
           br_dwb_rpt_select_range
           rs_cod_dwb_output
           v_log_print_par
           bt_isl1
           bt_up
           bt_edl1
           bt_down
           bt_rml1
           /*bt_get_file*/
           bt_set_printer
           bt_imprime
           /*bt_print*/
           bt_can
           bt_hel2
           plano_cta_ctbl.cod_plano_cta_ctbl
           cenar_ctbl.cod_cenar_ctbl
           exerc_ctbl.cod_exerc_ctbl
           period_ctbl.num_period_ctbl
           bt_mais
           bt_planilha
           unid_organ.cod_unid_organ
           with frame f_rpt_42_sdo_cta_ctbl_balanct.

    /* Begin_Include: i_exec_program_epc */
    &if '{&emsbas_version}' > '1.00' &then
    if  v_nom_prog_upc <> '' then
    do:
        assign v_rec_table_epc = recid(sdo_cta_ctbl).    
        run value(v_nom_prog_upc) (input 'ENABLE',
                                   input 'viewer',
                                   input this-procedure,
                                   input v_wgh_frame_epc,
                                   input v_nom_table_epc,
                                   input v_rec_table_epc).
        if  'no' = 'yes'
        and return-value = 'NOK' then
            undo, retry.
    end.

    if  v_nom_prog_appc <> '' then
    do:
        assign v_rec_table_epc = recid(sdo_cta_ctbl).    
        run value(v_nom_prog_appc) (input 'ENABLE',
                                    input 'viewer',
                                    input this-procedure,
                                    input v_wgh_frame_epc,
                                    input v_nom_table_epc,
                                    input v_rec_table_epc).
        if  'no' = 'yes'
        and return-value = 'NOK' then
            undo, retry.
    end.

    &if '{&emsbas_version}' > '5.00' &then
    if  v_nom_prog_dpc <> '' then
    do:
        assign v_rec_table_epc = recid(sdo_cta_ctbl).    
        run value(v_nom_prog_dpc) (input 'ENABLE',
                                    input 'viewer',
                                    input this-procedure,
                                    input v_wgh_frame_epc,
                                    input v_nom_table_epc,
                                    input v_rec_table_epc).
        if  'no' = 'yes'
        and return-value = 'NOK' then
            undo, retry.
    end.
    &endif
    &endif
    /* End_Include: i_exec_program_epc */

    if  num-entries(v_cod_dwb_order) < 2
    then do:
        disable bt_down
                bt_up
                with frame f_rpt_42_sdo_cta_ctbl_balanct.
    end /* if */.

/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
    run pi_posiciona_dwb_rpt_param in v_prog_filtro_pdf (input rowid(dwb_rpt_param)).
    run pi_load_params in v_prog_filtro_pdf.
&endif
/* tech38629 - Fim da alteraá∆o */



    apply "value-changed" to rs_cod_dwb_output in frame f_rpt_42_sdo_cta_ctbl_balanct.

    if  yes = yes
    then do:
        enable rs_ind_run_mode
               with frame f_rpt_42_sdo_cta_ctbl_balanct.
        if  dwb_rpt_param.ind_dwb_run_mode = "Batch" /*l_batch*/ 
        then do:
            apply "value-changed" to rs_ind_run_mode in frame f_rpt_42_sdo_cta_ctbl_balanct.
        end /* if */.
    end /* if */.


    /* Begin_Include: ix_p10_rpt_sdo_cta_ctbl_balanct */
    run pi_ix_p10_rpt_sdo_cta_ctbl_balanct /*pi_ix_p10_rpt_sdo_cta_ctbl_balanct*/.
    /* End_Include: ix_p10_rpt_sdo_cta_ctbl_balanct */

    run pi_open_dwb_rpt_select_range /*pi_open_dwb_rpt_select_range*/.
    block1:
    repeat on error undo block1, retry block1:
        main_block:
        repeat on error undo super_block, leave super_block
                            on endkey undo super_block, leave super_block
                            on stop undo super_block, retry super_block
                            with frame f_rpt_42_sdo_cta_ctbl_balanct:
            /*if  retry
            then do:
                output stream s_1 close.
            END.*/
            assign v_log_print = no.
            if  valid-handle(v_wgh_focus)
            then do:
                wait-for go of frame f_rpt_42_sdo_cta_ctbl_balanct focus v_wgh_focus.
            end /* if */.
            else do:
                wait-for go of frame f_rpt_42_sdo_cta_ctbl_balanct.
            end /* else */.

            /* Begin_Include: ix_p15_rpt_sdo_cta_ctbl_balanct */
            if  v_log_print then do:
                find first tab_livre_emsfin no-lock
                    where  tab_livre_emsfin.cod_modul_dtsul = "utb" /*l_utb*/ 
                    and    tab_livre_emsfin.cod_tab_dic_dtsul = 'utb044za|Altera Projeto Padr∆o'
                    and    not tab_livre_emsfin.log_livre_1 no-error.
                if  avail tab_livre_emsfin then do:
                    /* Alteraá∆o do projeto padr∆o N∆o foi conclu°da ! */
                    run pi_messages (input "show",
                                     input 20334,
                                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                       tab_livre_emsfin.cod_compon_1_idx_tab,tab_livre_emsfin.cod_compon_2_idx_tab)) /*msg_20334*/.
                    leave main_block.
                end.
            end.

            assign v_log_gerac_planilha = yes.
            if v_log_gerac_planilha then do:
                if  dwb_rpt_param.ind_dwb_run_mode = "Batch" /*l_batch*/  then do:
                    assign v_cod_arq_planilha = replace(v_cod_arq_planilha, "~\", "~/").
                    if  index(v_cod_arq_planilha, ":") <> 0 then do:
                        /* Arquivo informado para planilha com problema. */
                        run pi_messages (input "show",
                                         input 12814,
                                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_12814*/.
                        leave main_block.
                    end.
                    if  length(entry(2, v_cod_arq_planilha, '.')) > 3 then do:
                        /* Arquivo informado para planilha com problema. */
                        run pi_messages (input "show",
                                         input 12814,
                                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_12814*/.
                        leave main_block.
                    end.
                end.
            end.    
            /* End_Include: ix_p15_rpt_sdo_cta_ctbl_balanct */

            assign dwb_rpt_param.cod_dwb_order            = ls_order:list-items
                   dwb_rpt_param.ind_dwb_run_mode         = input frame f_rpt_42_sdo_cta_ctbl_balanct rs_ind_run_mode
                   v_cod_dwb_order                        = ls_order:list-items
                   dwb_rpt_param.log_dwb_print_parameters = input frame f_rpt_42_sdo_cta_ctbl_balanct v_log_print_par
                   input frame f_rpt_42_sdo_cta_ctbl_balanct v_qtd_line.


            /* Begin_Include: ix_p20_rpt_sdo_cta_ctbl_balanct */
            /*output stream s_1 close.*/
            run pi_vld_rpt_sdo_cta_ctbl_balanct /*pi_vld_rpt_sdo_cta_ctbl_balanct*/.
            if return-value = "NOK" /*l_nok*/  then leave main_block.
            assign v_log_gerac_planilha = yes
                   v_cod_carac_lim      = ";".
            assign v_cod_unid_organ = unid_organ.cod_unid_organ
            v_nom_enterprise = unid_organ.des_unid_organ
            v_cod_plano_cta_ctbl = plano_cta_ctbl.cod_plano_cta_ctbl
            v_cod_cenar_ctbl = cenar_ctbl.cod_cenar_ctbl
            v_cod_exerc_ctbl = exerc_ctbl.cod_exerc_ctbl
            v_num_period_ctbl = period_ctbl.num_period_ctbl
            dwb_rpt_param.cod_dwb_parameters = v_cod_plano_cta_ctbl + chr(10)
            + v_cod_cenar_ctbl + chr(10) + v_cod_exerc_ctbl + chr(10) + string(v_num_period_ctbl) + chr(10)
            + string(v_log_estab_sum) + chr(10) + string(v_log_unid_negoc_sum) + chr(10) + string(v_log_ccusto_sum) + chr(10)
            + v_cod_finalid_econ_bas + chr(10) + v_cod_finalid_econ_apr + chr(10) + string(v_dat_cotac_indic_econ) + chr(10)
            + string(v_log_cta_ctbl_internac) + chr(10) + string(v_log_period_ctbl_ant_impr) + chr(10)
            + string(v_log_cta_ctbl_sdo) + chr(10) + string(v_log_cta_ctbl_analit) + chr(10)
            + string(v_log_consid_apurac_restdo) + chr(10) + string(v_num_niv_estrut) + chr(10)
            + v_cod_idioma_apr + chr(10) + string(v_rec_dwb_rpt_param) + chr(10) + string(v_log_gerac_planilha) + chr(10) 
            + v_cod_arq_planilha + chr(10) + v_cod_carac_lim + chr(10) + v_cod_plano_ccusto + chr(10)
            + v_cod_unid_organ + chr(10) + v_cod_cta_ctbl_pfixa + chr(10) + v_cod_cta_ctbl_excec + chr(10)
            + v_cod_ccusto_pfixa + chr(10) + v_cod_ccusto_excec + chr(10) + string(v_log_mostra_sem_aprop_cc)
            &IF DEFINED(BF_ADM_FIN_PROJ) &THEN 
            + chr(10) + v_cod_proj_financ_pfixa + chr(10) + v_cod_proj_financ_excec + chr(10) + string(v_log_proj_financ)
            + string(v_log_proj_financ)
            &ENDIF.
            &if '{&emsbas_version}' >= '5.05' &then                                    
                if v_log_eai_habilit then do:
                    assign dwb_rpt_param.cod_dwb_parameters = dwb_rpt_param.cod_dwb_parameters + chr(10) + string(v_log_gera_eai).
                end.
            &endif
            /* End_Include: ix_p20_rpt_sdo_cta_ctbl_balanct */

            if  v_log_print = yes
            then do:
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
                               or  length(entry(2, v_cod_dwb_file, ".")) > 3
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
&if '{&emsbas_version}':U >= '5.05':U &then
                    run pi_filename_batch in v_prog_filtro_pdf.
&endif
/* tech38629 - Fim da alteraá∆o */


                    assign v_cod_dwb_file = v_nom_integer.
                    if  search("prgtec/btb/btb911za.r") = ? and search("prgtec/btb/btb911za.p") = ? then do:
                        if  v_cod_dwb_user begins 'es_' then
                            return "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgtec/btb/btb911za.p".
                        else do:
                            message "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgtec/btb/btb911za.p"
                                   view-as alert-box error buttons ok.
                            return.
                        end.
                    end.
                    else
                        run prgtec/btb/btb911za.p (Input v_cod_dwb_program,
                                               Input v_cod_release,
                                               Input 42,
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
                    assign v_log_method = session:set-wait-state('general')
                           v_nom_report_title = fill(" ", 40 - length(v_rpt_s_1_name)) + v_rpt_s_1_name.
                    /* out_def: */
&if '{&emsbas_version}':U >= '5.05':U &then
/*                    case dwb_rpt_param.cod_dwb_output:*/
&else
                    case dwb_rpt_param.cod_dwb_output:
&endif
&if '{&emsbas_version}':U >= '5.05':U &then
/*                        when "Terminal" /*l_terminal*/ then out_term:*/
                        if dwb_rpt_param.cod_dwb_output = 'Terminal' then
&else
                        when "Terminal" /*l_terminal*/ then out_term:
&endif
                         do:
                            assign v_cod_dwb_file   = session:temp-directory + "fgl900a":U + '.tmp'
                                   v_rpt_s_1_bottom = v_qtd_line - (v_rpt_s_1_lines - v_qtd_bottom).
                            /*output stream s_1 to value(v_cod_dwb_file) paged page-size value(v_qtd_line) convert target 'iso8859-1'.*/
                        end /* do out_term */.
&if '{&emsbas_version}':U >= '5.05':U &then
/*                        when "Impressora" /*l_printer*/ then out_print:*/
                        if dwb_rpt_param.cod_dwb_output = 'Impressora' and getCodTipoRelat() <> 'PDF':U and getCodTipoRelat() <> 'RTF':U then
&else
                        when "Impressora" /*l_printer*/ then out_print:
&endif
                         do:
                            find imprsor_usuar no-lock
                                 where imprsor_usuar.nom_impressora = dwb_rpt_param.nom_dwb_printer
                                   and imprsor_usuar.cod_usuario = dwb_rpt_param.cod_dwb_user
&if "{&emsbas_version}" >= "5.01" &then
                                 use-index imprsrsr_id
&endif
                                  /*cl_get_printer of imprsor_usuar*/ no-error.
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
/*&if '{&emsbas_version}' > '1.00' &then
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
                                           paged page-size value(layout_impres.num_lin_pag) convert target  tip_imprsor.cod_pag_carac_conver.*/
                            setting:
                            for
                                each configur_layout_impres no-lock
                                where configur_layout_impres.num_id_layout_impres = layout_impres.num_id_layout_impres

                                by configur_layout_impres.num_ord_funcao_imprsor:
                                find configur_tip_imprsor no-lock
                                     where configur_tip_imprsor.cod_tip_imprsor = layout_impres.cod_tip_imprsor
                                       and configur_tip_imprsor.cod_funcao_imprsor = configur_layout_impres.cod_funcao_imprsor
                                       and configur_tip_imprsor.cod_opc_funcao_imprsor = configur_layout_impres.cod_opc_funcao_imprsor
&if "{&emsbas_version}" >= "5.01" &then
                                     use-index cnfgrtpm_id
&endif
                                      /*cl_get_print_command of configur_tip_imprsor*/ no-error.
                                bloco_1:
                                do
                                     v_num_count = 1 to extent(configur_tip_imprsor.num_carac_configur):
                                     /* configur_tip_imprsor: */
                                     case configur_tip_imprsor.num_carac_configur[v_num_count]:
                                         /*when 0 then put  stream s_1 control null.*/
                                         when ? then leave.
                                         /*otherwise 
                                             /* Convers∆o interna do OUTPUT TARGET */
                                             put stream s_1 control codepage-convert ( chr(configur_tip_imprsor.num_carac_configur[v_num_count]),
                                                                                       session:cpinternal,
                                                                                       tip_imprsor.cod_pag_carac_conver).*/
                                     end /* case configur_tip_imprsor */.
                                end /* do bloco_1 */.   
                            end /* for setting */.
                        end /* do out_print */.
&if '{&emsbas_version}':U >= '5.05':U &then
/*                        when "Arquivo" /*l_file*/ then out_file:*/
                        if dwb_rpt_param.cod_dwb_output = 'Impressora' and getCodTipoRelat() = 'PDF':U then do:
                            run pi_config_output_print_pdf in v_prog_filtro_pdf (input v_qtd_line, input-output v_cod_dwb_file, input dwb_rpt_param.cod_dwb_user, input no).
                        end.
                        if dwb_rpt_param.cod_dwb_output = 'Arquivo' then
&else
                        when "Arquivo" /*l_file*/ then out_file:
&endif
                         do:
                            assign v_cod_dwb_file   = dwb_rpt_param.cod_dwb_file
                                   v_rpt_s_1_bottom = v_qtd_line - (v_rpt_s_1_lines - v_qtd_bottom).

/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
run pi_rename_file in v_prog_filtro_pdf (input-output v_cod_dwb_file).
&endif
/* tech38629 - Fim da alteraá∆o */



                            /*output stream s_1 to value(v_cod_dwb_file)
                                   paged page-size value(v_qtd_line) convert target 'iso8859-1'.*/
                        end /* do out_file */.
&if '{&emsbas_version}':U >= '5.05':U &then
/*                    end /* case out_def */.*/
&else
                    end /* case out_def */.
&endif
                    assign v_nom_prog_ext = caps(substring("prgfin/fgl/fgl900a.p",12,8))
                           v_dat_execution = today
                           v_hra_execution = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":","").
                    run pi_rpt_sdo_cta_ctbl_balanct /*pi_rpt_sdo_cta_ctbl_balanct*/.
                end /* else */.
                if  dwb_rpt_param.log_dwb_print_parameters = yes
                then do:
                    run pi_print_parameters /*pi_print_parameters*/.

                    /* Begin_Include: ix_p30_rpt_sdo_cta_ctbl_balanct */
                    run pi_ix_p30_rpt_sdo_cta_ctbl_balanct /*pi_ix_p30_rpt_sdo_cta_ctbl_balanct*/.
                    /* End_Include: ix_p30_rpt_sdo_cta_ctbl_balanct */

                end /* if */.
                /*output stream s_1 close.*/

                /* Begin_Include: ix_p40_rpt_sdo_cta_ctbl_balanct */
                /* 204112 - gerar relat¢rio p/extens∆o .pdf. */ 
                /* soluÓ“o chamado DTSFND-72 - (FO1795.405) */
                /* tech38629 - AlteraÓ“o efetuada via filtro */
                &if '{&emsbas_version}':U >= '5.05':U &then

                run pi_call_convert_object in v_prog_filtro_pdf (input no,
                                                                 input rs_cod_dwb_output:screen-value in frame f_rpt_42_sdo_cta_ctbl_balanct,
                                                                 input v_nom_dwb_print_file,
                                                                 input v_cod_dwb_file,
                                                                 input v_nom_report_title).
                &endif
                /* tech38629 - Fim da alteraÓ“o */
                /* End_Include: ix_p40_rpt_sdo_cta_ctbl_balanct */


                assign v_log_method = session:set-wait-state("").
                if  dwb_rpt_param.cod_dwb_output = "Terminal" /*l_terminal*/ 
                then do:                    
                /* tech38629 - Alteraá∆o efetuada via filtro */
                &if '{&emsbas_version}':U >= '5.05':U &then
                    if  getCodTipoRelat() = 'PDF':U and OPSYS = 'WIN32':U
                    then do:
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
                end /* if */.
                leave main_block.
            end /* if */.
            else do:
                leave super_block.
            end /* else */.
        end /* repeat main_block */.
        if  v_num_ped_exec <> 0
        then do:
            /* Criado pedido &1 para Execuá∆o batch. */
            run pi_messages (input "show",
                             input 3556,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                v_num_ped_exec)) /*msg_3556*/.
            assign v_num_ped_exec = 0.
        end /* if */.
    end /* repeat block1 */.
end /* repeat super_block */.
/* ix_p50_rpt_sdo_cta_ctbl_balanct */
hide frame f_rpt_42_sdo_cta_ctbl_balanct.

/* Begin_Include: i_log_exec_prog_dtsul_fim */
if v_rec_log <> ? then do transaction:
    find log_exec_prog_dtsul where recid(log_exec_prog_dtsul) = v_rec_log exclusive-lock no-error.
    if  avail log_exec_prog_dtsul
    then do:
        assign log_exec_prog_dtsul.dat_fim_exec_prog_dtsul = today
               log_exec_prog_dtsul.hra_fim_exec_prog_dtsul = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":":U,"":U).
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
** Alterado por..........: Augusto Guimar∆es    
** Alterado em...........: 15/03/2011
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
** Procedure Interna.....: pi_open_dwb_rpt_select_range
** Descricao.............: pi_open_dwb_rpt_select_range
** Criado por............: Henke
** Criado em.............: 05/07/1996 09:13:45
** Alterado por..........: Augusto Guimar∆es    
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_open_dwb_rpt_select_range:

    open query qr_dwb_rpt_select_range for
        each dwb_rpt_select no-lock
        where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
          and dwb_rpt_select.cod_dwb_user = v_cod_dwb_user /*cl_dwb_rpt_select of dwb_rpt_select*/
            by dwb_rpt_select.cod_dwb_field
            by dwb_rpt_select.num_dwb_order.


END PROCEDURE. /* pi_open_dwb_rpt_select_range */
/*****************************************************************************
** Procedure Interna.....: pi_isl_dwb_rpt_select_range
** Descricao.............: pi_isl_dwb_rpt_select_range
** Criado por............: Henke
** Criado em.............: 03/07/1996 14:18:58
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_isl_dwb_rpt_select_range:

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

    def frame f_dlg_04_dwb_rpt_select_range
        rt_001
             at row 01.25 col 02.00
        rt_cxcf
             at row 06.17 col 02.00 bgcolor 7 
        dwb_rpt_select.cod_dwb_field
             at row 01.79 col 04.29 no-label
             view-as combo-box
             list-items "!"
              /*l_!*/
             inner-lines 5
             bgcolor 15 font 2
        bt_ok
             at row 06.38 col 03.00 font ?
             help "OK"
        bt_sav
             at row 06.38 col 14.00 font ?
             help "Salva"
        bt_can
             at row 06.38 col 25.00 font ?
             help "Cancela"
        bt_hel2
             at row 06.38 col 51.29 font ?
             help "Ajuda"
        with 1 down side-labels no-validate keep-tab-order three-d
             size-char 63.72 by 08.00 default-button bt_sav
             view-as dialog-box
             font 1 fgcolor ? bgcolor 8
             title "".
        /* adjust size of objects in this frame */
        assign bt_can:width-chars   in frame f_dlg_04_dwb_rpt_select_range = 10.00
               bt_can:height-chars  in frame f_dlg_04_dwb_rpt_select_range = 01.00
               bt_hel2:width-chars  in frame f_dlg_04_dwb_rpt_select_range = 10.00
               bt_hel2:height-chars in frame f_dlg_04_dwb_rpt_select_range = 01.00
               bt_ok:width-chars    in frame f_dlg_04_dwb_rpt_select_range = 10.00
               bt_ok:height-chars   in frame f_dlg_04_dwb_rpt_select_range = 01.00
               bt_sav:width-chars   in frame f_dlg_04_dwb_rpt_select_range = 10.00
               bt_sav:height-chars  in frame f_dlg_04_dwb_rpt_select_range = 01.00
               rt_001:width-chars   in frame f_dlg_04_dwb_rpt_select_range = 60.00
               rt_001:height-chars  in frame f_dlg_04_dwb_rpt_select_range = 04.75
               rt_cxcf:width-chars  in frame f_dlg_04_dwb_rpt_select_range = 60.29
               rt_cxcf:height-chars in frame f_dlg_04_dwb_rpt_select_range = 01.42.
        /* set private-data for the help system */
        assign dwb_rpt_select.cod_dwb_field:private-data in frame f_dlg_04_dwb_rpt_select_range = "HLP=000000000":U
               bt_ok:private-data                        in frame f_dlg_04_dwb_rpt_select_range = "HLP=000010721":U
               bt_sav:private-data                       in frame f_dlg_04_dwb_rpt_select_range = "HLP=000011048":U
               bt_can:private-data                       in frame f_dlg_04_dwb_rpt_select_range = "HLP=000011050":U
               bt_hel2:private-data                      in frame f_dlg_04_dwb_rpt_select_range = "HLP=000011326":U
               frame f_dlg_04_dwb_rpt_select_range:private-data                                 = "HLP=000000000".



{include/i_fclfrm.i f_dlg_04_dwb_rpt_select_range }
    /*************************** Frame Definition End ***************************/

    /*********************** User Interface Trigger Begin ***********************/


    ON CHOOSE OF bt_hel2 IN FRAME f_dlg_04_dwb_rpt_select_range
    DO:


        /* Begin_Include: i_context_help_frame */
        run prgtec/men/men900za.py (Input self:frame,
                                    Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.


        /* End_Include: i_context_help_frame */

    END. /* ON CHOOSE OF bt_hel2 IN FRAME f_dlg_04_dwb_rpt_select_range */

    ON CHOOSE OF bt_ok IN FRAME f_dlg_04_dwb_rpt_select_range
    DO:

        assign v_log_ok = yes.
    END. /* ON CHOOSE OF bt_ok IN FRAME f_dlg_04_dwb_rpt_select_range */

    ON VALUE-CHANGED OF dwb_rpt_select.cod_dwb_field IN FRAME f_dlg_04_dwb_rpt_select_range
    DO:

        assign v_cod_dwb_field = self:screen-value in frame f_dlg_04_dwb_rpt_select_range.

        run pi_isl_rpt_sdo_cta_ctbl_balanct /*pi_isl_rpt_sdo_cta_ctbl_balanct*/.

        if  v_wgh_label_ini = ?
        then do:
            create text v_wgh_label_ini
                assign frame        = frame f_dlg_04_dwb_rpt_select_range:handle
                    screen-value = "Inicial:" /*l_Inicial:*/ 
                    visible      = no
                    row          = 3.4
                    col          = 12
                    width        = 7.
{include/i_fcldin.i v_wgh_label_ini }
        end /* if */.

        if  v_wgh_label_fim = ?
        then do:
            create text v_wgh_label_fim
                assign frame        = frame f_dlg_04_dwb_rpt_select_range:handle
                    screen-value = "  Final:" /*l_bbfinal:*/ 
                    visible      = no
                    row          = 4.4
                    col          = 12
                    width        = 7.
{include/i_fcldin.i v_wgh_label_fim }
        end /* if */.

        if  v_wgh_fill_in_ini <> ?
        then do:
            delete widget v_wgh_fill_in_ini.
        end /* if */.

        create fill-in v_wgh_fill_in_ini
            assign frame              = frame f_dlg_04_dwb_rpt_select_range:handle
                   font               = 2
                   data-type          = v_cod_dat_type
                   format             = v_cod_format
                   side-label-handle  = v_wgh_label_ini:handle
                   row                = 3.4
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
            assign frame              = frame f_dlg_04_dwb_rpt_select_range:handle
                   font               = 2
                   data-type          = v_cod_dat_type
                   format             = v_cod_format
                   side-label-handle  = v_wgh_label_fim:handle
                   row                = 4.4
                   col                = 19
                   height             = 0.88
                   bgcolor            = 15
                   visible            = yes
                   sensitive          = yes
                   screen-value       = v_cod_final.
{include/i_fcldin.i v_wgh_fill_in_fim }

    END. /* ON VALUE-CHANGED OF dwb_rpt_select.cod_dwb_field IN FRAME f_dlg_04_dwb_rpt_select_range */


    /************************ User Interface Trigger End ************************/

    /**************************** Frame Trigger Begin ***************************/


    ON GO OF FRAME f_dlg_04_dwb_rpt_select_range
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

    END. /* ON GO OF FRAME f_dlg_04_dwb_rpt_select_range */

    ON HELP OF FRAME f_dlg_04_dwb_rpt_select_range ANYWHERE
    DO:


        /* Begin_Include: i_context_help */
        run prgtec/men/men900za.py (Input self:handle,
                                    Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
        /* End_Include: i_context_help */

    END. /* ON HELP OF FRAME f_dlg_04_dwb_rpt_select_range */

    ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_04_dwb_rpt_select_range ANYWHERE
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

    END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_04_dwb_rpt_select_range */

    ON RIGHT-MOUSE-UP OF FRAME f_dlg_04_dwb_rpt_select_range ANYWHERE
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

    END. /* ON RIGHT-MOUSE-UP OF FRAME f_dlg_04_dwb_rpt_select_range */


    /***************************** Frame Trigger End ****************************/

    pause 0 before-hide.
    view frame f_dlg_04_dwb_rpt_select_range.

    assign v_log_ok = no
           frame f_dlg_04_dwb_rpt_select_range:title = "Inclui" /*l_inclui*/  + " Conjunto de Seleá∆o" /*l_conjunto_selecao*/ .

    main_block:
    repeat while v_log_ok = no on endkey undo main_block, leave main_block 
                                            on error undo main_block, retry main_block:

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

        assign dwb_rpt_select.cod_dwb_field:list-items in frame f_dlg_04_dwb_rpt_select_range = v_cod_dwb_select
               dwb_rpt_select.cod_dwb_field:screen-value = entry(1,v_cod_dwb_select).

        enable dwb_rpt_select.cod_dwb_field
               bt_ok
               bt_sav
               bt_can
               with frame f_dlg_04_dwb_rpt_select_range.
        apply "value-changed" to dwb_rpt_select.cod_dwb_field in frame f_dlg_04_dwb_rpt_select_range.

        wait-for go of frame f_dlg_04_dwb_rpt_select_range.

        assign dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
               dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
               dwb_rpt_select.num_dwb_order   = v_num_dwb_order
               dwb_rpt_select.cod_dwb_initial = v_wgh_fill_in_ini:screen-value
               dwb_rpt_select.cod_dwb_final   = v_wgh_fill_in_fim:screen-value
               dwb_rpt_select.cod_dwb_field.

    end /* repeat main_block */.

    hide frame f_dlg_04_dwb_rpt_select_range.

    run pi_open_dwb_rpt_select_range /*pi_open_dwb_rpt_select_range*/.
END PROCEDURE. /* pi_isl_dwb_rpt_select_range */
/*****************************************************************************
** Procedure Interna.....: pi_edl_dwb_rpt_select_range
** Descricao.............: pi_edl_dwb_rpt_select_range
** Criado por............: Henke
** Criado em.............: 03/07/1996 16:17:05
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_edl_dwb_rpt_select_range:

    /************************ Parameter Definition Begin ************************/

    def Input param p_rec_dwb_rpt_select
        as recid
        format ">>>>>>9"
        no-undo.
    def Input param p_des_procedure
        as character
        format "x(50)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_des_cta
        as character
        format "x(40)":U
        no-undo.
    def var v_des_cta_antes
        as character
        format "x(40)":U
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

    def frame f_dlg_04_dwb_rpt_select_range
        rt_001
             at row 01.25 col 02.00
        rt_cxcf
             at row 06.17 col 02.00 bgcolor 7 
        dwb_rpt_select.cod_dwb_field
             at row 01.79 col 04.29 no-label
             view-as combo-box
             list-items "!"
              /*l_!*/
             inner-lines 5
             bgcolor 15 font 2
        bt_ok
             at row 06.38 col 03.00 font ?
             help "OK"
        bt_sav
             at row 06.38 col 14.00 font ?
             help "Salva"
        bt_can
             at row 06.38 col 25.00 font ?
             help "Cancela"
        bt_hel2
             at row 06.38 col 51.29 font ?
             help "Ajuda"
        with 1 down side-labels no-validate keep-tab-order three-d
             size-char 63.72 by 08.00 default-button bt_sav
             view-as dialog-box
             font 1 fgcolor ? bgcolor 8
             title "".
        /* adjust size of objects in this frame */
        assign bt_can:width-chars   in frame f_dlg_04_dwb_rpt_select_range = 10.00
               bt_can:height-chars  in frame f_dlg_04_dwb_rpt_select_range = 01.00
               bt_hel2:width-chars  in frame f_dlg_04_dwb_rpt_select_range = 10.00
               bt_hel2:height-chars in frame f_dlg_04_dwb_rpt_select_range = 01.00
               bt_ok:width-chars    in frame f_dlg_04_dwb_rpt_select_range = 10.00
               bt_ok:height-chars   in frame f_dlg_04_dwb_rpt_select_range = 01.00
               bt_sav:width-chars   in frame f_dlg_04_dwb_rpt_select_range = 10.00
               bt_sav:height-chars  in frame f_dlg_04_dwb_rpt_select_range = 01.00
               rt_001:width-chars   in frame f_dlg_04_dwb_rpt_select_range = 60.00
               rt_001:height-chars  in frame f_dlg_04_dwb_rpt_select_range = 04.75
               rt_cxcf:width-chars  in frame f_dlg_04_dwb_rpt_select_range = 60.29
               rt_cxcf:height-chars in frame f_dlg_04_dwb_rpt_select_range = 01.42.
        /* set private-data for the help system */
        assign dwb_rpt_select.cod_dwb_field:private-data in frame f_dlg_04_dwb_rpt_select_range = "HLP=000000000":U
               bt_ok:private-data                        in frame f_dlg_04_dwb_rpt_select_range = "HLP=000010721":U
               bt_sav:private-data                       in frame f_dlg_04_dwb_rpt_select_range = "HLP=000011048":U
               bt_can:private-data                       in frame f_dlg_04_dwb_rpt_select_range = "HLP=000011050":U
               bt_hel2:private-data                      in frame f_dlg_04_dwb_rpt_select_range = "HLP=000011326":U
               frame f_dlg_04_dwb_rpt_select_range:private-data                                 = "HLP=000000000".



{include/i_fclfrm.i f_dlg_04_dwb_rpt_select_range }
    /*************************** Frame Definition End ***************************/

    /*********************** User Interface Trigger Begin ***********************/


    ON CHOOSE OF bt_hel2 IN FRAME f_dlg_04_dwb_rpt_select_range
    DO:


        /* Begin_Include: i_context_help_frame */
        run prgtec/men/men900za.py (Input self:frame,
                                    Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.


        /* End_Include: i_context_help_frame */

    END. /* ON CHOOSE OF bt_hel2 IN FRAME f_dlg_04_dwb_rpt_select_range */

    ON CHOOSE OF bt_ok IN FRAME f_dlg_04_dwb_rpt_select_range
    DO:

        assign v_log_ok = yes.
    END. /* ON CHOOSE OF bt_ok IN FRAME f_dlg_04_dwb_rpt_select_range */

    ON VALUE-CHANGED OF dwb_rpt_select.cod_dwb_field IN FRAME f_dlg_04_dwb_rpt_select_range
    DO:

        assign v_cod_dwb_field = self:screen-value in frame f_dlg_04_dwb_rpt_select_range.

        run pi_isl_rpt_sdo_cta_ctbl_balanct /*pi_isl_rpt_sdo_cta_ctbl_balanct*/.

        if  v_wgh_label_ini = ?
        then do:
            create text v_wgh_label_ini
                assign frame        = frame f_dlg_04_dwb_rpt_select_range:handle
                    screen-value = "Inicial:" /*l_Inicial:*/ 
                    visible      = no
                    row          = 3.4
                    col          = 12
                    width        = 7.
{include/i_fcldin.i v_wgh_label_ini }
        end /* if */.

        if  v_wgh_label_fim = ?
        then do:
            create text v_wgh_label_fim
                assign frame        = frame f_dlg_04_dwb_rpt_select_range:handle
                    screen-value = "  Final:" /*l_bbfinal:*/ 
                    visible      = no
                    row          = 4.4
                    col          = 12
                    width        = 7.
{include/i_fcldin.i v_wgh_label_fim }
        end /* if */.

        if  v_wgh_fill_in_ini <> ?
        then do:
            delete widget v_wgh_fill_in_ini.
        end /* if */.

        create fill-in v_wgh_fill_in_ini
            assign frame              = frame f_dlg_04_dwb_rpt_select_range:handle
                   font               = 2
                   data-type          = v_cod_dat_type
                   format             = v_cod_format
                   side-label-handle  = v_wgh_label_ini:handle
                   row                = 3.4
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
            assign frame              = frame f_dlg_04_dwb_rpt_select_range:handle
                   font               = 2
                   data-type          = v_cod_dat_type
                   format             = v_cod_format
                   side-label-handle  = v_wgh_label_fim:handle
                   row                = 4.4
                   col                = 19
                   height             = 0.88
                   bgcolor            = 15
                   visible            = yes
                   sensitive          = yes
                   screen-value       = v_cod_final.
{include/i_fcldin.i v_wgh_fill_in_fim }

    END. /* ON VALUE-CHANGED OF dwb_rpt_select.cod_dwb_field IN FRAME f_dlg_04_dwb_rpt_select_range */


    /************************ User Interface Trigger End ************************/

    /**************************** Frame Trigger Begin ***************************/


    ON GO OF FRAME f_dlg_04_dwb_rpt_select_range
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

    END. /* ON GO OF FRAME f_dlg_04_dwb_rpt_select_range */

    ON HELP OF FRAME f_dlg_04_dwb_rpt_select_range ANYWHERE
    DO:


        /* Begin_Include: i_context_help */
        run prgtec/men/men900za.py (Input self:handle,
                                    Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
        /* End_Include: i_context_help */

    END. /* ON HELP OF FRAME f_dlg_04_dwb_rpt_select_range */

    ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_04_dwb_rpt_select_range ANYWHERE
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

    END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_04_dwb_rpt_select_range */

    ON RIGHT-MOUSE-UP OF FRAME f_dlg_04_dwb_rpt_select_range ANYWHERE
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

    END. /* ON RIGHT-MOUSE-UP OF FRAME f_dlg_04_dwb_rpt_select_range */


    /***************************** Frame Trigger End ****************************/

    pause 0 before-hide.
    view frame f_dlg_04_dwb_rpt_select_range.

    assign v_log_ok = no
           frame f_dlg_04_dwb_rpt_select_range:title = "Edita" /*l_edita*/  + " Conjunto de Seleá∆o" /*l_conjunto_selecao*/ .

    main_block:
    repeat while v_log_ok = no on endkey undo main_block, leave main_block on error undo main_block, leave main_block:

        find dwb_rpt_select where recid(dwb_rpt_select) = p_rec_dwb_rpt_select exclusive-lock.

        assign dwb_rpt_select.cod_dwb_field:list-items in frame f_dlg_04_dwb_rpt_select_range = v_cod_dwb_select
               v_cod_dwb_field = dwb_rpt_select.cod_dwb_field.

        display dwb_rpt_select.cod_dwb_field
                with frame f_dlg_04_dwb_rpt_select_range.

        run value(p_des_procedure) /*prg_value(p_des_procedure)*/.

        create text v_wgh_label_ini
            assign frame        = frame f_dlg_04_dwb_rpt_select_range:handle
                screen-value = "Inicial:" /*l_Inicial:*/ 
                visible      = no
                row          = 3.4
                col          = 12
                width        = 7.
{include/i_fcldin.i v_wgh_label_ini }

        create text v_wgh_label_fim
            assign frame        = frame f_dlg_04_dwb_rpt_select_range:handle
                screen-value = "  Final:" /*l_bbfinal:*/ 
                visible      = no
                row          = 4.4
                col          = 12
                width        = 7.
{include/i_fcldin.i v_wgh_label_fim }

        create fill-in v_wgh_fill_in_ini
            assign frame          = frame f_dlg_04_dwb_rpt_select_range:handle
               font               = 2
               data-type          = v_cod_dat_type
               format             = v_cod_format
               side-label-handle  = v_wgh_label_ini:handle
               row                = 3.4
               column             = 19
               height             = 0.88
               bgcolor            = 15
               visible            = yes
               sensitive          = yes
               screen-value       = dwb_rpt_select.cod_dwb_initial.
{include/i_fcldin.i v_wgh_fill_in_ini }

        create fill-in v_wgh_fill_in_fim
            assign frame          = frame f_dlg_04_dwb_rpt_select_range:handle
               font               = 2        
               data-type          = v_cod_dat_type
               format             = v_cod_format
               side-label-handle  = v_wgh_label_fim:handle
               row                = 4.4
               col                = 19
               height             = 0.88
               bgcolor            = 15
               visible            = yes
               sensitive          = yes
               screen-value       = dwb_rpt_select.cod_dwb_final.
{include/i_fcldin.i v_wgh_fill_in_fim }

        disable dwb_rpt_select.cod_dwb_field
                with frame f_dlg_04_dwb_rpt_select_range.

        enable bt_ok
               bt_can
               with frame f_dlg_04_dwb_rpt_select_range.

        wait-for go of frame f_dlg_04_dwb_rpt_select_range.

        assign dwb_rpt_select.cod_dwb_initial = v_wgh_fill_in_ini:screen-value
               dwb_rpt_select.cod_dwb_final   = v_wgh_fill_in_fim:screen-value.

    end /* repeat main_block */.

    hide frame f_dlg_04_dwb_rpt_select_range.

    run pi_open_dwb_rpt_select_range /*pi_open_dwb_rpt_select_range*/.
END PROCEDURE. /* pi_edl_dwb_rpt_select_range */
/*****************************************************************************
** Procedure Interna.....: pi_filename_validation
** Descricao.............: pi_filename_validation
** Criado por............: 
** Criado em.............: // 
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
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
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_set_print_layout_default:

    dflt:
    do with frame f_rpt_42_sdo_cta_ctbl_balanct:

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
            find dwb_rpt_param
                where dwb_rpt_param.cod_dwb_user = v_cod_usuar_corren
                and   dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
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
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
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

    /*run winexec (input v_cod_key_value + chr(32) + p_cod_dwb_file, input 1).*/

    END PROCEDURE.

    PROCEDURE WinExec EXTERNAL 'kernel32.dll':
      DEF INPUT  PARAM prg_name                          AS CHARACTER.
      DEF INPUT  PARAM prg_style                         AS SHORT.




END PROCEDURE. /* pi_show_report_2 */
/*****************************************************************************
** Procedure Interna.....: pi_print_parameters
** Descricao.............: pi_print_parameters
** Criado por............: gilsinei
** Criado em.............: 04/09/1996 10:46:51
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_print_parameters:

    /*if  page-number (s_1) > 0
    then do:
        page stream s_1.
    end.*/

    /*hide stream s_1 frame f_rpt_s_1_footer_last_page.
    hide stream s_1 frame f_rpt_s_1_footer_normal.
    view stream s_1 frame f_rpt_s_1_footer_param_page.*/
    /*if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
        page stream s_1.
    put stream s_1 unformatted 
        skip (1)
        "Usu†rio: " at 1
        v_cod_usuar_corren at 10 format "x(12)" skip (1).*/

    /*if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
        page stream s_1.
    put stream s_1 unformatted 
        "Ordem" to 5
        "Classificador" at 7 skip
        "-----" to 5
        "--------------------------------" at 7 skip.*/
    /*1_block:
    repeat v_num_entry = 1 to num-entries (v_cod_dwb_order):
        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
            page stream s_1.
        put stream s_1 unformatted 
            v_num_entry to 5 format ">>>>9"
            entry(v_num_entry,v_cod_dwb_order) at 7 format "x(32)" skip.
    end /* repeat 1_block */.*/

    /*if (line-counter(s_1) + 4) > v_rpt_s_1_bottom then
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
        "------------------" at 61 skip.*/
    /*ler:
    for each dwb_rpt_select no-lock
     where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
       and dwb_rpt_select.cod_dwb_user = v_cod_dwb_user /*cl_dwb_rpt_select of dwb_rpt_select*/:
        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
            page stream s_1.
        put stream s_1 unformatted 
            dwb_rpt_select.log_dwb_rule at 1 format "Regra/Exceá∆o"
            dwb_rpt_select.cod_dwb_field at 9 format "x(32)"
            dwb_rpt_select.cod_dwb_initial at 42 format "x(18)"
            dwb_rpt_select.cod_dwb_final at 61 format "x(18)" skip.
    end /* for ler */.*/

END PROCEDURE. /* pi_print_parameters */
/*****************************************************************************
** Procedure Interna.....: pi_initialize_reports
** Descricao.............: pi_initialize_reports
** Criado por............: glauco
** Criado em.............: 21/03/1997 09:22:53
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_initialize_reports:

    /* inicializa vari†veis */
    find ems5.empresa no-lock
         where ems5.empresa.cod_empresa = v_cod_empres_usuar /*cl_empres_usuar of ems5.empresa*/ no-error.
    find dwb_rpt_param
         where dwb_rpt_param.cod_dwb_program = "rel_balanct_ctbl":U
           and dwb_rpt_param.cod_dwb_user    = v_cod_dwb_user
           no-lock no-error.

    if  avail dwb_rpt_param then do:
    &if '{&emsbas_version}' > '1.00' &then
    &if '{&emsbas_version}' >= '5.03' &then
        assign v_nom_dwb_print_file = dwb_rpt_param.nom_dwb_print_file.
    &else
        assign v_nom_dwb_print_file = dwb_rpt_param.cod_livre_1.
    &endif
    &endif
        if  dwb_rpt_param.qtd_dwb_line <> 0 then
            assign v_qtd_line = dwb_rpt_param.qtd_dwb_line.
        else
            assign v_qtd_line = v_rpt_s_1_lines.
    end.

    assign v_cod_dwb_proced   = "rel_balanct_ctbl":U
           v_cod_dwb_program  = "rel_balanct_ctbl":U

           v_cod_dwb_order    = "Conta Cont†bil"

           v_cod_release      = trim(" 1.00.01.076":U)

           v_cod_dwb_select   = "Conta Cont†bil,Alternativa Conta," + &IF '{&emsfin_version}' >= '5.01' AND '{&emsfin_version}' <= '5.07' &THEN "Estabelecimento" &ELSE "Estabelecimento" &ENDIF + ",Unid Neg¢cio,Centro Custo"

           v_ind_dwb_run_mode = "On-Line" /*l_online*/ 
           v_qtd_column       = v_rpt_s_1_columns
           v_qtd_bottom       = v_rpt_s_1_bottom.
    if  avail ems5.empresa
    then do:
        assign v_nom_enterprise   = ems5.empresa.nom_razao_social.
    end /* if */.
    else do:
        assign v_nom_enterprise   = 'DATASUL'.
    end /* else */.
END PROCEDURE. /* pi_initialize_reports */
/*****************************************************************************
** Procedure Interna.....: pi_configure_dwb_param
** Descricao.............: pi_configure_dwb_param
** Criado por............: glauco
** Criado em.............: 21/03/1997 10:15:36
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_configure_dwb_param:

        find dwb_rpt_param exclusive-lock
             where dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
               and dwb_rpt_param.cod_dwb_user = v_cod_dwb_user /*cl_dwb_rpt_param of dwb_rpt_param*/ no-error.
        if  not available dwb_rpt_param
        then do:
            create dwb_rpt_param.
            assign dwb_rpt_param.cod_dwb_program         = v_cod_dwb_program
                   dwb_rpt_param.cod_dwb_user            = v_cod_dwb_user
                   dwb_rpt_param.cod_dwb_parameters      = v_cod_dwb_parameters
                   dwb_rpt_param.cod_dwb_output          = "Terminal" /*l_terminal*/ 
                   dwb_rpt_param.cod_dwb_order           = v_cod_dwb_order
                   dwb_rpt_param.ind_dwb_run_mode        = "On-Line" /*l_online*/ 
                   dwb_rpt_param.cod_dwb_file            = ""
                   dwb_rpt_param.nom_dwb_printer         = ""
                   dwb_rpt_param.cod_dwb_print_layout    = ""
                   v_cod_dwb_file_temp                   = ""
                   ls_order:list-items in frame f_rpt_42_sdo_cta_ctbl_balanct = v_cod_dwb_order.
        end /* if */.
        else do:
            assign ls_order:list-items in frame f_rpt_42_sdo_cta_ctbl_balanct = "!".
            if  ls_order:delete(1) in frame f_rpt_42_sdo_cta_ctbl_balanct
            then do:
                order_1:
                repeat v_num_entry = 1 to num-entries (dwb_rpt_param.cod_dwb_order):
                    assign v_cod_dwb_field = entry (v_num_entry, dwb_rpt_param.cod_dwb_order).
                    if  lookup (v_cod_dwb_field, v_cod_dwb_order) > 0 and
                        ls_order:lookup (v_cod_dwb_field) = 0
                    then do:
                        assign v_log_method = ls_order:add-last(v_cod_dwb_field).
                    end /* if */.
                end /* repeat order_1 */.
                order_2:
                repeat v_num_entry = 1 to num-entries (v_cod_dwb_order):
                    assign v_cod_dwb_field = entry (v_num_entry, v_cod_dwb_order).
                    if  ls_order:lookup (v_cod_dwb_field) = 0
                    then do:
                       assign v_log_method = ls_order:add-last(v_cod_dwb_field).
                    end /* if */.
                end /* repeat order_2 */.
            end /* if */.

            assign v_cod_dwb_file_temp = replace(dwb_rpt_param.cod_dwb_file, "~\", "~/").
            if  index(v_cod_dwb_file_temp, "~/") <> 0
            then do:
                assign v_cod_dwb_file_temp = substring(v_cod_dwb_file_temp, r-index(v_cod_dwb_file_temp, "~/") + 1).
            end /* if */.
            else do:
                assign v_cod_dwb_file_temp = dwb_rpt_param.cod_dwb_file.
            end /* else */.
        end /* else */.
END PROCEDURE. /* pi_configure_dwb_param */
/*****************************************************************************
** Procedure Interna.....: pi_output_reports
** Descricao.............: pi_output_reports
** Criado por............: glauco
** Criado em.............: 21/03/1997 09:26:29
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
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
/*            when "Arquivo" /*l_file*/ then*/
            if dwb_rpt_param.cod_dwb_output = 'Impressora' and getCodTipoRelat() = 'PDF':U then do:
                run pi_config_output_print_pdf in v_prog_filtro_pdf (input v_qtd_line, input-output v_cod_dwb_file, input v_cod_usuar_corren, input yes).
            end.
            if dwb_rpt_param.cod_dwb_output = 'Arquivo' then
&else
            when "Arquivo" /*l_file*/ then
&endif
            block1:
            do:

               /* tech38629 - Relatorio PDF e RTF                 */
               /* Renomear arquivo quando extensao for pdf ou rtf */

/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
run pi_rename_file in v_prog_filtro_pdf (input-output v_cod_dwb_file).
&endif
/* tech38629 - Fim da alteraá∆o */



               /*output stream s_1 to value(v_cod_dwb_file)
               paged page-size value(v_qtd_line) convert target 'iso8859-1'.*/
            end /* do block1 */.
&if '{&emsbas_version}':U >= '5.05':U &then
/*            when "Impressora" /*l_printer*/ then*/
            if dwb_rpt_param.cod_dwb_output = 'Impressora' and getCodTipoRelat() <> 'PDF':U and getCodTipoRelat() <> 'RTF':U then
&else
            when "Impressora" /*l_printer*/ then
&endif
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

/*                   if  available b_servid_exec_style                                                                                               */
/*                   and b_servid_exec_style.ind_tip_fila_exec = 'UNIX'                                                                              */
/*                   then do:                                                                                                                        */
/*                       &if '{&emsbas_version}' > '1.00' &then                                                                                      */
/*                       &if '{&emsbas_version}' >= '5.03' &then                                                                                     */
/*                           if dwb_rpt_param.nom_dwb_print_file <> "" then do:                                                               */
/*                               if  layout_impres.num_lin_pag = 0                                                                            */
/*                               then do:                                                                                                            */
/*                                   output stream s_1 to value(lc(dwb_rpt_param.nom_dwb_print_file))                                                */
/*                                          page-size 0 convert target tip_imprsor.cod_pag_carac_conver.                                      */
/*                               end /* if */.                                                                                                       */
/*                               else do:                                                                                                            */
/*                                   output stream s_1 to value(lc(dwb_rpt_param.nom_dwb_print_file))                                                */
/*                                          paged page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver. */
/*                               end /* else */.                                                                                                     */
/*                           end.                                                                                                                    */
/*                           else do:                                                                                                                */
/*                               if  layout_impres.num_lin_pag = 0                                                                            */
/*                               then do:                                                                                                            */
/*                                   output stream s_1 through value(servid_exec_imprsor.nom_disposit_so)                                            */
/*                                          page-size 0 convert target tip_imprsor.cod_pag_carac_conver.                                      */
/*                               end /* if */.                                                                                                       */
/*                               else do:                                                                                                            */
/*                                   output stream s_1 through value(servid_exec_imprsor.nom_disposit_so)                                            */
/*                                          paged page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver. */
/*                               end /* else */.                                                                                                     */
/*                           end.                                                                                                                    */
/*                       &else                                                                                                                       */
/*                           if dwb_rpt_param.cod_livre_1 <> "" then do:                                                                      */
/*                               if  layout_impres.num_lin_pag = 0                                                                            */
/*                               then do:                                                                                                            */
/*                                   output stream s_1 to value(lc(dwb_rpt_param.cod_livre_1))                                                       */
/*                                          page-size 0 convert target tip_imprsor.cod_pag_carac_conver.                                      */
/*                               end /* if */.                                                                                                       */
/*                               else do:                                                                                                            */
/*                                   output stream s_1 to value(lc(dwb_rpt_param.cod_livre_1))                                                       */
/*                                          paged page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver. */
/*                               end /* else */.                                                                                                     */
/*                           end.                                                                                                                    */
/*                           else do:                                                                                                                */
/*                               if  layout_impres.num_lin_pag = 0                                                                            */
/*                               then do:                                                                                                            */
/*                                   output stream s_1 through value(servid_exec_imprsor.nom_disposit_so)                                            */
/*                                          page-size 0 convert target tip_imprsor.cod_pag_carac_conver.                                      */
/*                               end /* if */.                                                                                                       */
/*                               else do:                                                                                                            */
/*                                   output stream s_1 through value(servid_exec_imprsor.nom_disposit_so)                                            */
/*                                          paged page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver. */
/*                               end /* else */.                                                                                                     */
/*                           end.                                                                                                                    */
/*                       &endif                                                                                                                      */
/*                       &endif                                                                                                                      */
/*                   end /* if */.                                                                                                                   */
/*                   else do:                                                                                                                        */
/*                       &if '{&emsbas_version}' > '1.00' &then                                                                                      */
/*                       &if '{&emsbas_version}' >= '5.03' &then                                                                                     */
/*                           if dwb_rpt_param.nom_dwb_print_file <> "" then do:                                                               */
/*                               if  layout_impres.num_lin_pag = 0                                                                            */
/*                               then do:                                                                                                            */
/*                                   output stream s_1 to value(lc(dwb_rpt_param.nom_dwb_print_file))                                                */
/*                                          page-size 0 convert target tip_imprsor.cod_pag_carac_conver.                                      */
/*                               end /* if */.                                                                                                       */
/*                               else do:                                                                                                            */
/*                                   output stream s_1 to value(lc(dwb_rpt_param.nom_dwb_print_file))                                                */
/*                                          paged page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver. */
/*                               end /* else */.                                                                                                     */
/*                           end.                                                                                                                    */
/*                           else do:                                                                                                                */
/*                               if  layout_impres.num_lin_pag = 0                                                                            */
/*                               then do:                                                                                                            */
/*                                   output stream s_1 to value(servid_exec_imprsor.nom_disposit_so)                                                 */
/*                                          page-size 0 convert target tip_imprsor.cod_pag_carac_conver.                                      */
/*                               end /* if */.                                                                                                       */
/*                               else do:                                                                                                            */
/*                                   output stream s_1 to value(servid_exec_imprsor.nom_disposit_so)                                                 */
/*                                          paged page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver. */
/*                               end /* else */.                                                                                                     */
/*                           end.                                                                                                                    */
/*                       &else                                                                                                                       */
/*                           if dwb_rpt_param.cod_livre_1 <> "" then do:                                                                      */
/*                               if  layout_impres.num_lin_pag = 0                                                                            */
/*                               then do:                                                                                                            */
/*                                   output stream s_1 to value(lc(dwb_rpt_param.cod_livre_1))                                                       */
/*                                          page-size 0 convert target tip_imprsor.cod_pag_carac_conver.                                      */
/*                               end /* if */.                                                                                                       */
/*                               else do:                                                                                                            */
/*                                   output stream s_1 to value(lc(dwb_rpt_param.cod_livre_1))                                                       */
/*                                          paged page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver. */
/*                               end /* else */.                                                                                                     */
/*                           end.                                                                                                                    */
/*                           else do:                                                                                                                */
/*                               if  layout_impres.num_lin_pag = 0                                                                            */
/*                               then do:                                                                                                            */
/*                                   output stream s_1 to value(servid_exec_imprsor.nom_disposit_so)                                                 */
/*                                          page-size 0 convert target tip_imprsor.cod_pag_carac_conver.                                      */
/*                               end /* if */.                                                                                                       */
/*                               else do:                                                                                                            */
/*                                   output stream s_1 to value(servid_exec_imprsor.nom_disposit_so)                                                 */
/*                                          paged page-size value(layout_impres.num_lin_pag) convert target tip_imprsor.cod_pag_carac_conver. */
/*                               end /* else */.                                                                                                     */
/*                           end.                                                                                                                    */
/*                       &endif                                                                                                                      */
/*                       &endif                                                                                                                      */
/*                   end /* else */.                                                                                                                 */

                  setting:
                  for
                      each configur_layout_impres no-lock
                      where configur_layout_impres.num_id_layout_impres = layout_impres.num_id_layout_impres

                      by configur_layout_impres.num_ord_funcao_imprsor:

                      find configur_tip_imprsor no-lock
                           where configur_tip_imprsor.cod_tip_imprsor = layout_impres.cod_tip_imprsor
                             and configur_tip_imprsor.cod_funcao_imprsor = configur_layout_impres.cod_funcao_imprsor
                             and configur_tip_imprsor.cod_opc_funcao_imprsor = configur_layout_impres.cod_opc_funcao_imprsor
    &if "{&emsbas_version}" >= "5.01" &then
                           use-index cnfgrtpm_id
    &endif
                            /*cl_get_print_command of configur_tip_imprsor*/ no-error.

                      bloco_1:
                      do
                          v_num_count = 1 to extent(configur_tip_imprsor.num_carac_configur):
                          /* configur_tip_imprsor: */
                          case configur_tip_imprsor.num_carac_configur[v_num_count]:
                              /*when 0 then put  stream s_1 control null.*/
                              when ? then leave.
                              /*otherwise 
                                  /* Convers∆o interna do OUTPUT TARGET */
                                  put stream s_1 control codepage-convert ( chr(configur_tip_imprsor.num_carac_configur[v_num_count]),
                                                                            session:cpinternal,
                                                                            tip_imprsor.cod_pag_carac_conver).*/
                          end /* case configur_tip_imprsor */.
                      end /* do bloco_1 */.
                 end /* for setting */.
            end /* do block2 */.
&if '{&emsbas_version}':U >= '5.05':U &then
/*    end /* case block */.*/
&else
    end /* case block */.
&endif

    run pi_rpt_sdo_cta_ctbl_balanct /*pi_rpt_sdo_cta_ctbl_balanct*/.
END PROCEDURE. /* pi_output_reports */
/*****************************************************************************
** Procedure Interna.....: pi_isl_rpt_sdo_cta_ctbl_balanct
** Descricao.............: pi_isl_rpt_sdo_cta_ctbl_balanct
** Criado por............: 
** Criado em.............: // 
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_isl_rpt_sdo_cta_ctbl_balanct:

    /************************* Variable Definition Begin ************************/

    def var v_cod_ccusto_fim
        as Character
        format "x(11)":U
        initial "ZZZZZZZZZZZ" /*l_ZZZZZZZZZZZ*/
        label "atÇ"
        column-label "Centro Custo"
        no-undo.
    def var v_cod_ccusto_ini
        as Character
        format "x(11)":U
        label "CCusto Inicial"
        column-label "Centro Custo"
        no-undo.
    def var v_cod_cta_ctbl_fim
        as character
        format "x(20)":U
        initial "ZZZZZZZZZZZZZZZZZZZZ"
        label "Conta Final"
        column-label "Final"
        no-undo.
    def var v_cod_cta_ctbl_ini
        as character
        format "x(20)":U
        label "Conta Inicial"
        column-label "Inicial"
        no-undo.
    def var v_cod_format_proj_financ
        as character
        format "x(20)":U
        label "Formato Projeto"
        column-label "Formato Projeto"
        no-undo.
    def var v_cod_proj_financ_000
        as character
        format "x(20)":U
        label "Projeto"
        column-label "Projeto"
        no-undo.
    def var v_cod_proj_financ_999
        as character
        format "x(20)":U
        label "Projeto"
        column-label "Projeto"
        no-undo.
    def var v_num_count_proj
        as integer
        format ">>>>,>>9":U
        no-undo.


    /************************** Variable Definition End *************************/

    /* formato: */
    case v_cod_dwb_field:
        when "Conta Cont†bil" then
        cta_ctbl_block:
        do:
           find plano_cta_ctbl where recid(plano_cta_ctbl) = v_rec_plano_cta_ctbl no-lock no-error.
           if  avail plano_cta_ctbl
           then do:
               run pi_retornar_min_format_2 (Input plano_cta_ctbl.cod_format_cta_ctbl,
                                             output v_cod_cta_ctbl_ini) /*pi_retornar_min_format_2*/.
               run pi_retornar_max_format (Input plano_cta_ctbl.cod_format_cta_ctbl,
                                           output v_cod_cta_ctbl_fim) /*pi_retornar_max_format*/.
               assign v_cod_format = plano_cta_ctbl.cod_format_cta_ctbl.
           end /* if */.
           else do:
               assign v_cod_format = "x(20)":U.
           end /* else */.
           assign v_cod_dat_type = "character"
                  v_cod_initial  = v_cod_cta_ctbl_ini
                  v_cod_final    = v_cod_cta_ctbl_fim.
        end /* do cta_ctbl_block */.
        when "Centro Custo" then
        ccusto_block:
        do:
           find plano_ccusto
               where plano_ccusto.cod_plano_ccusto = v_cod_plano_ccusto
               and   plano_ccusto.cod_empresa      = unid_organ.cod_unid_organ
               no-lock no-error.
           if  avail plano_ccusto
           then do:
               run pi_retornar_min_format_2 (Input plano_ccusto.cod_format_ccusto,
                                             output v_cod_ccusto_ini) /*pi_retornar_min_format_2*/.
               run pi_retornar_max_format (Input plano_ccusto.cod_format_ccusto,
                                           output v_cod_ccusto_fim) /*pi_retornar_max_format*/.
               assign v_cod_format = plano_ccusto.cod_format_ccusto.
           end /* if */.
           else do:
               assign v_cod_format = "x(11)":U.
           end /* else */.
           assign v_cod_dat_type = "Character"
                  v_cod_initial  = v_cod_ccusto_ini
                  v_cod_final    = v_cod_ccusto_fim.
        end /* do ccusto_block */.
        &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
        when "Projeto" then
        projeto_block:
        do:
            find last param_geral_ems no-lock no-error.
            assign v_cod_format_proj_financ = param_geral_ems.cod_format_proj_financ.

            assign v_num_count_proj = 1
                   v_cod_proj_financ_000 = ""
                   v_cod_proj_financ_999 = "".

            do while v_num_count_proj <= length(v_cod_format_proj_financ):
                if  substring(v_cod_format_proj_financ,v_num_count_proj,1) <> "-"
                and substring(v_cod_format_proj_financ,v_num_count_proj,1) <> "."
                then do:
                    if  substring(v_cod_format_proj_financ,v_num_count_proj,1) = "9"
                    then do:
                        assign v_cod_proj_financ_000 = v_cod_proj_financ_000 + "0"
                               v_cod_proj_financ_999 = v_cod_proj_financ_999 + "9".
                    end.
                    else do:
                        if  substring(v_cod_format_proj_financ,v_num_count_proj,1) = "x" /*l_x*/ 
                        then do:
                            assign v_cod_proj_financ_999 = v_cod_proj_financ_999 + "Z" /*l_Z*/ .
                        end.
                    end.
                end.
                assign v_num_count_proj = v_num_count_proj + 1.
            end.
            assign v_cod_dat_type = "character"
                   v_cod_format   = v_cod_format_proj_financ
                   v_cod_initial  = string(v_cod_proj_financ_000, v_cod_format)
                   v_cod_final    = string(v_cod_proj_financ_999, v_cod_format).
        end /* do projeto_block */.
        &ENDIF
        when "Alternativa Conta" then
            assign v_cod_dat_type = "character"
                   v_cod_format   = "x(12)":U
                   v_cod_initial  = string("":U, v_cod_format)
                   v_cod_final    = string("ZZZZZZZZZZZZ":U, v_cod_format).
        when &IF '{&emsfin_version}' >= '5.01' AND '{&emsfin_version}' <= '5.07' &THEN "Estabelecimento" &ELSE "Estabelecimento" &ENDIF then
            assign v_cod_dat_type = &IF '{&emsfin_version}' >= '5.01' AND '{&emsfin_version}' <= '5.07' &THEN "character" &ELSE "Character" &ENDIF
                   v_cod_format   = &IF '{&emsfin_version}' >= '5.01' AND '{&emsfin_version}' <= '5.07' &THEN "x(3)":U &ELSE "x(5)":U &ENDIF
                   v_cod_initial  = string(&IF '{&emsfin_version}' >= '5.01' AND '{&emsfin_version}' <= '5.07' &THEN "":U &ELSE "":U &ENDIF, v_cod_format)
                   v_cod_final    = string(&IF '{&emsfin_version}' >= '5.01' AND '{&emsfin_version}' <= '5.07' &THEN "ZZZ":U &ELSE "ZZZZZ":U &ENDIF, v_cod_format).
        when "Unid Neg¢cio" then
            assign v_cod_dat_type = "character"
                   v_cod_format   = "x(3)":U
                   v_cod_initial  = string("":U, v_cod_format)
                   v_cod_final    = string("ZZZ":U, v_cod_format).
    end /* case formato */.
END PROCEDURE. /* pi_isl_rpt_sdo_cta_ctbl_balanct */
/*****************************************************************************
** Procedure Interna.....: pi_rpt_sdo_cta_ctbl_balanct
** Descricao.............: pi_rpt_sdo_cta_ctbl_balanct
** Criado por............: Jane
** Criado em.............: // 
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_rpt_sdo_cta_ctbl_balanct:

    DEF VAR v_cod_dwb_output AS CHAR NO-UNDO.

    /* **   Posiciona na primeira opá∆o de Classificaá∆o   ***/
    assign ls_order:list-items in frame f_rpt_42_sdo_cta_ctbl_balanct = v_cod_dwb_order
           v_qtd_line_aux      = v_rpt_s_1_bottom
           v_rpt_s_1_name      = "Balancete" /*l_balancete*/ 
           v_nom_report_title  = fill(" ", 40 - length(v_rpt_s_1_name)) + v_rpt_s_1_name.

    /*hide stream s_1 frame f_rpt_s_1_header_unique.
    view stream s_1 frame f_rpt_s_1_header_period.
    hide stream s_1 frame f_rpt_s_1_footer_last_page.
    hide stream s_1 frame f_rpt_s_1_footer_param_page.
    view stream s_1 frame f_rpt_s_1_footer_normal.*/

    if  search("prgfin/fgl/fgl900c.r") = ? and search("prgfin/fgl/fgl900c.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgfin/fgl/fgl900c.p".
        else do:
            message "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgfin/fgl/fgl900c.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
    DO:
        ASSIGN v_cod_dwb_output = rs_cod_dwb_output:SCREEN-VALUE.
        run prgfin/fgl/fgl900c.p (INPUT v_cod_dwb_output) /*prg_fnc_sdo_cta_ctbl_balanct_impr*/.
    END.

        

    for each dwb_rpt_select exclusive-lock
        where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
          and dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
          and dwb_rpt_select.cod_dwb_field   = "Conta Cont†bil" /*l_conta_contabil*/ 
          and dwb_rpt_select.log_dwb_rule    = yes:
        assign dwb_rpt_select.cod_dwb_initial = string(dwb_rpt_select.cod_dwb_initial, plano_cta_ctbl.cod_format_cta_ctbl)
               dwb_rpt_select.cod_dwb_final   = string(dwb_rpt_select.cod_dwb_final  , plano_cta_ctbl.cod_format_cta_ctbl).
    end.

    /*hide stream s_1 frame f_rpt_s_1_footer_normal.
    hide stream s_1 frame f_rpt_s_1_footer_param_page.
    view stream s_1 frame f_rpt_s_1_footer_last_page.*/
    if not v_cod_dwb_user begins 'es_' then do:
       run pi_open_dwb_rpt_select_range.
    end.


END PROCEDURE. /* pi_rpt_sdo_cta_ctbl_balanct */
/*****************************************************************************
** Procedure Interna.....: pi_retornar_plano_cta_ctbl_prim
** Descricao.............: pi_retornar_plano_cta_ctbl_prim
** Criado por............: 
** Criado em.............: // 
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_retornar_plano_cta_ctbl_prim:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_unid_organ
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
        as character
        format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
        as Character
        format "x(5)"
    &ENDIF
        no-undo.
    def Input param p_dat_refer_ent
        as date
        format "99/99/9999"
        no-undo.
    def output param p_cod_plano_cta_ctbl
        as character
        format "x(8)"
        no-undo.
    def output param p_log_plano_cta_ctbl_uni
        as logical
        format "Sim/N∆o"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_cod_return
        as character
        format "x(40)":U
        no-undo.


    /************************** Variable Definition End *************************/

    calcula_block:
    for each plano_cta_unid_organ no-lock
     where plano_cta_unid_organ.cod_unid_organ = p_cod_unid_organ
       and plano_cta_unid_organ.ind_tip_plano_cta_ctbl = "Prim†rio" /*cl_retorna_plano_cta_ctbl_prim of plano_cta_unid_organ*/:
        if  p_dat_refer_ent = ?
        or (plano_cta_unid_organ.dat_inic_valid <= p_dat_refer_ent
        and plano_cta_unid_organ.dat_fim_valid >= p_dat_refer_ent)
        then do:
            assign v_cod_return         = v_cod_return + "," + plano_cta_unid_organ.cod_plano_cta_ctbl
                   p_cod_plano_cta_ctbl = plano_cta_unid_organ.cod_plano_cta_ctbl.
        end /* if */.
    end /* for calcula_block */.

    if  num-entries(v_cod_return) = 2
    then do:
        assign p_log_plano_cta_ctbl_uni = yes.
    end /* if */.
    else do:
        assign p_log_plano_cta_ctbl_uni = no.
    end /* else */.
END PROCEDURE. /* pi_retornar_plano_cta_ctbl_prim */
/*****************************************************************************
** Procedure Interna.....: pi_retornar_cenar_ctbl_fisc
** Descricao.............: pi_retornar_cenar_ctbl_fisc
** Criado por............: 
** Criado em.............: // 
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_retornar_cenar_ctbl_fisc:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_empresa
        as character
        format "x(3)"
        no-undo.
    def Input param p_dat_refer_ent
        as date
        format "99/99/9999"
        no-undo.
    def output param p_cod_cenar_ctbl
        as character
        format "x(8)"
        no-undo.


    /************************* Parameter Definition End *************************/

    find first utiliz_cenar_ctbl no-lock
         where utiliz_cenar_ctbl.cod_empresa = p_cod_empresa
           and utiliz_cenar_ctbl.log_cenar_fisc = yes /*cl_retorna_fisc of utiliz_cenar_ctbl*/ no-error.
    if  avail utiliz_cenar_ctbl
    then do:
        if  p_dat_refer_ent = ? or (utiliz_cenar_ctbl.dat_inic_valid <= p_dat_refer_ent and
                                    utiliz_cenar_ctbl.dat_fim_valid >= p_dat_refer_ent)
        then do:
            assign p_cod_cenar_ctbl = utiliz_cenar_ctbl.cod_cenar_ctbl.
        end /* if */.
        else do:
            assign p_cod_cenar_ctbl = "".
        end /* else */.
    end /* if */.
    else do:
        assign p_cod_cenar_ctbl = "".
    end /* else */.
END PROCEDURE. /* pi_retornar_cenar_ctbl_fisc */
/*****************************************************************************
** Procedure Interna.....: pi_retornar_finalid_econ_corren_estab
** Descricao.............: pi_retornar_finalid_econ_corren_estab
** Criado por............: 
** Criado em.............: // 
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_retornar_finalid_econ_corren_estab:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_estab
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
        as character
        format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
        as Character
        format "x(5)"
    &ENDIF
        no-undo.
    def output param p_cod_finalid_econ
        as character
        format "x(10)"
        no-undo.


    /************************* Parameter Definition End *************************/

    find estabelecimento no-lock
         where estabelecimento.cod_estab = p_cod_estab
         use-index stblcmnt_id no-error.
    if  avail estabelecimento
    then do:
       find ems5.pais no-lock
            where ems5.pais.cod_pais = estabelecimento.cod_pais
             no-error.
       assign p_cod_finalid_econ = ems5.pais.cod_finalid_econ_pais.
    end.
END PROCEDURE. /* pi_retornar_finalid_econ_corren_estab */
/*****************************************************************************
** Procedure Interna.....: pi_retornar_min_format_2
** Descricao.............: pi_retornar_min_format_2
** Criado por............: fut12209
** Criado em.............: 08/10/2004 14:30:00
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_retornar_min_format_2:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_format
        as character
        format "x(8)"
        no-undo.
    def output param p_cod_return
        as character
        format "x(40)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_num_count                      as integer         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    assign v_num_count  = 1
           p_cod_return = "".

    contador:
    do while v_num_count <= length(p_cod_format):
        /* testa_block: */
        case substring(p_cod_format, v_num_count, 1):
            when "!" then
                assign p_cod_return = p_cod_return + keylabel(65).
            when 'X' then
                assign p_cod_return = p_cod_return + "0".
            when "9" then
                assign p_cod_return = p_cod_return + "0".
        end /* case testa_block */.
        assign v_num_count = v_num_count + 1.
    end /* do contador */.
END PROCEDURE. /* pi_retornar_min_format_2 */
/*****************************************************************************
** Procedure Interna.....: pi_retornar_max_format
** Descricao.............: pi_retornar_max_format
** Criado por............: vanei
** Criado em.............: 07/12/1995 11:08:18
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_retornar_max_format:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_format
        as character
        format "x(8)"
        no-undo.
    def output param p_cod_return
        as character
        format "x(40)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_num_count                      as integer         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    assign v_num_count  = 1
           p_cod_return = "".

    contador:
    do while v_num_count <= length(p_cod_format):
        /* testa_block: */
        case substring(p_cod_format, v_num_count, 1):
            when "!" then
                assign p_cod_return = p_cod_return + keylabel(90).
            when 'X' then
                assign p_cod_return = p_cod_return + keylabel(90).
            when "9" then
                assign p_cod_return = p_cod_return + "9".
        end /* case testa_block */.
        assign v_num_count = v_num_count + 1.
    end /* do contador */.
END PROCEDURE. /* pi_retornar_max_format */
/*****************************************************************************
** Procedure Interna.....: pi_retornar_plano_ccusto_ult
** Descricao.............: pi_retornar_plano_ccusto_ult
** Criado por............: Henke
** Criado em.............: // 
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_retornar_plano_ccusto_ult:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_empresa
        as character
        format "x(3)"
        no-undo.
    def Input param p_dat_refer_ent
        as date
        format "99/99/9999"
        no-undo.
    def output param p_cod_plano_ccusto
        as character
        format "x(8)"
        no-undo.
    def output param p_log_plano_ccusto_uni
        as logical
        format "Sim/N∆o"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_cod_return
        as character
        format "x(40)":U
        no-undo.


    /************************** Variable Definition End *************************/

    calcula_block:
    for
        each plano_ccusto no-lock
        where plano_ccusto.cod_empresa = p_cod_empresa /*cl_p_cod_empresa of plano_ccusto*/:
        if  p_dat_refer_ent = ?
        or (plano_ccusto.dat_inic_valid <= p_dat_refer_ent
        and plano_ccusto.dat_fim_valid  >= p_dat_refer_ent)
        then do:
            assign v_cod_return         = v_cod_return + ',' + plano_ccusto.cod_plano_ccusto
                   p_cod_plano_ccusto = plano_ccusto.cod_plano_ccusto.
        end /* if */.
    end /* for calcula_block */.

    if  num-entries(v_cod_return) = 2
    then do:
        assign p_log_plano_ccusto_uni = yes.
    end /* if */.
    else do:
        assign p_log_plano_ccusto_uni = no.
    end /* else */.

END PROCEDURE. /* pi_retornar_plano_ccusto_ult */
/*****************************************************************************
** Procedure Interna.....: pi_vld_rpt_sdo_cta_ctbl_balanct
** Descricao.............: pi_vld_rpt_sdo_cta_ctbl_balanct
** Criado por............: Henke
** Criado em.............: 22/11/1996 08:20:57
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_vld_rpt_sdo_cta_ctbl_balanct:

    /************************** Buffer Definition Begin *************************/

    &if "{&emsbas_version}" >= "1.00" &then
    def buffer b_dwb_rpt_select
        for dwb_rpt_select.
    &endif


    /*************************** Buffer Definition End **************************/

    /************************* Variable Definition Begin ************************/

    def var v_cod_cta_ctbl_fim
        as character
        format "x(20)":U
        initial "ZZZZZZZZZZZZZZZZZZZZ"
        label "Conta Final"
        column-label "Final"
        no-undo.
    def var v_cod_cta_ctbl_ini
        as character
        format "x(20)":U
        label "Conta Inicial"
        column-label "Inicial"
        no-undo.
    def var v_cod_dwb_final
        as character
        format "x(20)":U
        no-undo.
    def var v_cod_dwb_initial
        as character
        format "x(20)":U
        no-undo.
    def var v_num_cont
        as integer
        format ">,>>9":U
        initial 0
        no-undo.
    def var v_num_contag
        as integer
        format ">>>>,>>9":U
        no-undo.
    def var v_num_final                      as integer         no-undo. /*local*/
    def var v_num_inicial                    as integer         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    /* --- Validaá∆o dos ParÉmetros ---*/
    /* --- Unidade Organizacional ---*/
    find unid_organ no-lock
         where unid_organ.cod_unid_organ = input frame f_rpt_42_sdo_cta_ctbl_balanct unid_organ.cod_unid_organ no-error.
    if  not avail unid_organ
    then do:
        assign v_wgh_focus = unid_organ.cod_unid_organ:handle in frame f_rpt_42_sdo_cta_ctbl_balanct.
        /* &1 inexistente ! */
        run pi_messages (input "show",
                         input 1284,
                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                           "Unidade Organizacional","Unidades Organizacionais")) /*msg_1284*/.
        return "NOK" /*l_nok*/ .
    end /* if */.
    /* --- Plano Cta Ctbl ---*/
    find plano_cta_ctbl no-lock
         where plano_cta_ctbl.cod_plano_cta_ctbl = input frame f_rpt_42_sdo_cta_ctbl_balanct plano_cta_ctbl.cod_plano_cta_ctbl
          no-error.
    if  not avail plano_cta_ctbl
    then do:
        assign v_wgh_focus = plano_cta_ctbl.cod_plano_cta_ctbl:handle in frame f_rpt_42_sdo_cta_ctbl_balanct.
        /* &1 inexistente ! */
        run pi_messages (input "show",
                         input 1284,
                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                           "Plano Contas Cont†beis","Planos Contas Cont†beis")) /*msg_1284*/.
        return "NOK" /*l_nok*/ .
    end /* if */.
    else do:
       find first plano_cta_unid_organ no-lock
            where plano_cta_unid_organ.cod_unid_organ     = unid_organ.cod_unid_organ
              and plano_cta_unid_organ.cod_plano_cta_ctbl = plano_cta_ctbl.cod_plano_cta_ctbl
              no-error.
       if  not avail plano_cta_unid_organ
       then do:
           assign v_wgh_focus = plano_cta_ctbl.cod_plano_cta_ctbl:handle in frame f_rpt_42_sdo_cta_ctbl_balanct.
           /* Plano Contas N∆o cadastrado para Unidade Organizacional ! */
           run pi_messages (input "show",
                            input 527,
                            input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_527*/.
           return "NOK" /*l_nok*/ .
       end /* if */.
    end /* else */.
    /* --- Plano Ccusto ---*/
    if  plano_ccusto.cod_plano_ccusto:sensitive in frame f_rpt_42_sdo_cta_ctbl_balanct = yes
    then do: 
        find plano_ccusto no-lock
             where plano_ccusto.cod_empresa      = unid_organ.cod_unid_organ
               and plano_ccusto.cod_plano_ccusto = input frame f_rpt_42_sdo_cta_ctbl_balanct plano_ccusto.cod_plano_ccusto no-error.
        if  not avail plano_ccusto
        then do:
            assign v_wgh_focus = plano_ccusto.cod_plano_ccusto:handle in frame f_rpt_42_sdo_cta_ctbl_balanct.
            /* Plano Centro de Custo inexistente ! */
            run pi_messages (input "show",
                             input 238,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_238*/.
            return "NOK" /*l_nok*/ .
        end /* if */.
    end /* if */.
    /* --- Cen†rio Ctbl ---*/
    find cenar_ctbl no-lock
         where cenar_ctbl.cod_cenar_ctbl = input frame f_rpt_42_sdo_cta_ctbl_balanct cenar_ctbl.cod_cenar_ctbl
          no-error.
    if  not avail cenar_ctbl
    then do:
        assign v_wgh_focus = cenar_ctbl.cod_cenar_ctbl:handle in frame f_rpt_42_sdo_cta_ctbl_balanct.
        /* &1 inexistente ! */
        run pi_messages (input "show",
                         input 1284,
                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                           "Cen†rio Cont†bil","Cen†rios Cont†beis")) /*msg_1284*/.
        return "NOK" /*l_nok*/ .
    end /* if */.
    /* --- Exerc°cio e Per°odo Ctbl ---*/
    find exerc_ctbl no-lock
         where exerc_ctbl.cod_cenar_ctbl = input frame f_rpt_42_sdo_cta_ctbl_balanct cenar_ctbl.cod_cenar_ctbl
           and exerc_ctbl.cod_exerc_ctbl = input frame f_rpt_42_sdo_cta_ctbl_balanct exerc_ctbl.cod_exerc_ctbl no-error.
    if  not avail exerc_ctbl
    then do:
        assign v_wgh_focus = exerc_ctbl.cod_exerc_ctbl:handle in frame f_rpt_42_sdo_cta_ctbl_balanct.
        /* &1 inexistente ! */
        run pi_messages (input "show",
                         input 1284,
                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                           "Exerc°cio Cont†bil","Exerc°cios Cont†beis")) /*msg_1284*/.
        return "NOK" /*l_nok*/ .
    end /* if */.
    find period_ctbl no-lock
         where period_ctbl.cod_cenar_ctbl  = cenar_ctbl.cod_cenar_ctbl
           and period_ctbl.cod_exerc_ctbl  = exerc_ctbl.cod_exerc_ctbl
           and period_ctbl.num_period_ctbl = input frame f_rpt_42_sdo_cta_ctbl_balanct period_ctbl.num_period_ctbl no-error.
    if  not avail period_ctbl
    then do:
        assign v_wgh_focus = period_ctbl.num_period_ctbl:handle in frame f_rpt_42_sdo_cta_ctbl_balanct.
        /* &1 inexistente ! */
        run pi_messages (input "show",
                         input 1284,
                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                           "Per°odo Cont†bil","Exerc°cios Cont†beis")) /*msg_1284*/.
        return "NOK" /*l_nok*/ .
    end /* if */.
    assign v_cod_dwb_file = ed_1x40:screen-value in frame f_rpt_42_sdo_cta_ctbl_balanct
           v_dat_fim_period_ctbl = period_ctbl.dat_fim_period_ctbl.
    /* --- Validar Arquivo p/ Planilha ---*/
    assign v_log_gerac_planilha = yes.
    if  v_log_gerac_planilha = yes
    then do:
        if  v_cod_dwb_file = v_cod_arq_planilha
        then do:
            assign v_wgh_focus = ed_1x40:handle in frame f_rpt_42_sdo_cta_ctbl_balanct.
            /* Arquivo sa°da para Planilha igual ao Relat¢rio Balancete ! */
            /*run pi_messages (input "show",
                             input 3768,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_3768*/.
            return "NOK" /*l_nok*/ .*/
        end /* if */.
    end /* if */.
    /* --- Faixa de Regras ---*/
    /* --- Conta Cont†bil ---*/
    find first dwb_rpt_select no-lock
       where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
         and dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
         and (dwb_rpt_select.cod_dwb_field  = "Conta Cont†bil" 
          or  dwb_rpt_select.cod_dwb_field  = "Alternativa Conta")
         and dwb_rpt_select.log_dwb_rule    = yes no-error.
    if  avail dwb_rpt_select
    then do:
        /* --- Sobreposiá∆o de Faixas ---*/
        if  dwb_rpt_select.cod_dwb_field   = "Conta Cont†bil"
        then do:
           run pi_validar_faixa_dwb_rpt_select (Input "Conta Cont†bil") /*pi_validar_faixa_dwb_rpt_select*/.
           if  return-value = "3683"
           then do:
              assign v_wgh_focus = br_dwb_rpt_select_range:handle in frame f_rpt_42_sdo_cta_ctbl_balanct.
              /* Faixa de &1 irregular ! */
              run pi_messages (input "show",
                               input 3683,
                               input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                  "Conta Cont†bil")) /*msg_3683*/.
              return "NOK" /*l_nok*/ .
           end /* if */.
        end /* if */.   
        else do:
           run pi_validar_faixa_dwb_rpt_select (Input "Alternativa Conta") /*pi_validar_faixa_dwb_rpt_select*/.
           if  return-value = "3683"
           then do:
              assign v_wgh_focus = br_dwb_rpt_select_range:handle in frame f_rpt_42_sdo_cta_ctbl_balanct.
              /* Faixa de &1 irregular ! */
              run pi_messages (input "show",
                               input 3683,
                               input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                  "Alternativa Conta")) /*msg_3683*/.
              return "NOK" /*l_nok*/ .
           end /* if */.
        end /* else */.   
        /* --- Classificado pela Estrutura s¢ Contas do Primeiro N°vel ---*/
        if  entry(1,dwb_rpt_param.cod_dwb_order) = "Estrutura" /*l_estrutura*/ 
        then do:
            select_block:
            for
                each b_dwb_rpt_select exclusive-lock
                   where b_dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
                     and b_dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
                     and (b_dwb_rpt_select.cod_dwb_field  = "Conta Cont†bil"
                      or  b_dwb_rpt_select.cod_dwb_field  = "Alternativa Conta")
                     and b_dwb_rpt_select.log_dwb_rule    = yes:
                if  b_dwb_rpt_select.cod_dwb_field  = "Conta Cont†bil"
                then do:

                    assign v_cod_dwb_final   = ''
                           v_cod_dwb_initial = ''.

                    assign v_num_contag = 1.
                    do while v_num_contag <= length(b_dwb_rpt_select.cod_dwb_final):
                       if  not ((substring(b_dwb_rpt_select.cod_dwb_final,v_num_contag,1) = "-" and
                                 substring(b_dwb_rpt_select.cod_dwb_final,v_num_contag,1) = substring(plano_cta_ctbl.cod_format_cta_ctbl,v_num_contag,1))
                       or
                                (substring(b_dwb_rpt_select.cod_dwb_final,v_num_contag,1) = "." and
                                 substring(b_dwb_rpt_select.cod_dwb_final,v_num_contag,1) = substring(plano_cta_ctbl.cod_format_cta_ctbl,v_num_contag,1)))
                       then do:
                                 assign v_cod_dwb_final = v_cod_dwb_final + substring(b_dwb_rpt_select.cod_dwb_final,v_num_contag,1).
                       end /* if */.
                       assign v_num_contag = v_num_contag + 1.
                    end.
                    assign v_num_contag = 1.
                    do while v_num_contag <= length(b_dwb_rpt_select.cod_dwb_initial):
                        if  not ((substring(b_dwb_rpt_select.cod_dwb_initial,v_num_contag,1) = "-" and
                                  substring(b_dwb_rpt_select.cod_dwb_initial,v_num_contag,1) = substring(plano_cta_ctbl.cod_format_cta_ctbl,v_num_contag,1))
                        or
                                  (substring(b_dwb_rpt_select.cod_dwb_initial,v_num_contag,1) = "." and
                                   substring(b_dwb_rpt_select.cod_dwb_initial,v_num_contag,1) = substring(plano_cta_ctbl.cod_format_cta_ctbl,v_num_contag,1)))
                        then do:
                                   assign v_cod_dwb_initial = v_cod_dwb_initial + substring(b_dwb_rpt_select.cod_dwb_initial,v_num_contag,1).
                        end /* if */.
                        assign v_num_contag = v_num_contag + 1.
                    end.
                    assign b_dwb_rpt_select.cod_dwb_final   = v_cod_dwb_final 
                           b_dwb_rpt_select.cod_dwb_initial = v_cod_dwb_initial.

                    find first estrut_cta_ctbl no-lock
                         where estrut_cta_ctbl.cod_plano_cta_ctbl = plano_cta_ctbl.cod_plano_cta_ctbl

                           and estrut_cta_ctbl.cod_cta_ctbl_filho >= b_dwb_rpt_select.cod_dwb_initial /*cl_estrut_cta_buffer of estrut_cta_ctbl*/ no-error.
                end /* if */.
                else do:
                   find first cta_ctbl no-lock
                        where cta_ctbl.cod_plano_cta_ctbl = plano_cta_ctbl.cod_plano_cta_ctbl

                          and cta_ctbl.cod_altern_cta_ctbl >= b_dwb_rpt_select.cod_dwb_initial /*cl_estrut_cta_b_dwb_parameters of cta_ctbl*/ no-error. 
                   if  avail cta_ctbl
                   then do:
                      find estrut_cta_ctbl no-lock where 
                           estrut_cta_ctbl.cod_plano_cta_ctbl = plano_cta_ctbl.cod_plano_cta_ctbl and 
                           estrut_cta_ctbl.cod_cta_ctbl_filho = cta_ctbl.cod_cta_ctbl no-error.
                   end /* if */.        
                end /* else */.
                if avail estrut_cta_ctbl then
                assign v_cod_cta_ctbl_pai_inicial = estrut_cta_ctbl.cod_cta_ctbl_pai.
                if  v_log_emis_balanct_por_estrut = yes
                then do:
                   if  not avail estrut_cta_ctbl
                   then do:
                       leave select_block.
                   end /* if */.
                end /* if */.
                else do:
                   if  (avail estrut_cta_ctbl and estrut_cta_ctbl.cod_cta_ctbl_pai <> "") or
                       not avail estrut_cta_ctbl
                   then do:
                       leave select_block.
                   end /* if */.
                end /* else */.
                if  b_dwb_rpt_select.cod_dwb_field  = "Conta Cont†bil"
                then do:            
                   find last estrut_cta_ctbl no-lock where 
                             estrut_cta_ctbl.cod_plano_cta_ctbl = plano_cta_ctbl.cod_plano_cta_ctbl and 
                             estrut_cta_ctbl.cod_cta_ctbl_filho <= b_dwb_rpt_select.cod_dwb_final no-error.
                end /* if */.
                else do:     
                   find last cta_ctbl no-lock where  
                             cta_ctbl.cod_plano_cta_ctbl  = plano_cta_ctbl.cod_plano_cta_ctbl and
                             cta_ctbl.cod_altern_cta_ctbl <= b_dwb_rpt_select.cod_dwb_final no-error.
                   if  avail cta_ctbl
                   then do:
                      find estrut_cta_ctbl no-lock where 
                           estrut_cta_ctbl.cod_plano_cta_ctbl = plano_cta_ctbl.cod_plano_cta_ctbl and 
                           estrut_cta_ctbl.cod_cta_ctbl_filho = cta_ctbl.cod_cta_ctbl no-error.
                   end /* if */.        
                end /* else */.
                if avail estrut_cta_ctbl then
                assign v_cod_cta_ctbl_pai_final = estrut_cta_ctbl.cod_cta_ctbl_pai.
                if  v_log_emis_balanct_por_estrut = yes
                then do:
                    if  not avail estrut_cta_ctbl
                    then do:
                        leave select_block.
                    end /* if */.
                end /* if */.    
                else do:
                    if  (avail estrut_cta_ctbl and estrut_cta_ctbl.cod_cta_ctbl_pai <> "") or not avail estrut_cta_ctbl
                    then do:
                        leave select_block.
                    end /* if */.
                end /* else */.
                if  v_log_emis_balanct_por_estrut = yes
                then do:
                    if  b_dwb_rpt_select.cod_dwb_initial <> b_dwb_rpt_select.cod_dwb_final
                    then do:
                      if  v_cod_cta_ctbl_pai_final <> v_cod_cta_ctbl_pai_inicial
                      then do:
                         assign v_wgh_focus = br_dwb_rpt_select_range:handle in frame f_rpt_42_sdo_cta_ctbl_balanct.
                         /* Faixa Cta Ctbl irregular para Classificaá∆o por Estrutura ! */
                         run pi_messages (input "show",
                                          input 14156,
                                          input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_14156*/.
                         return "NOK" /*l_nok*/ . 
                      end /* if */.
                   end /* if */.
                   run pi_valida_conta_pai_recursivo (Input v_cod_cta_ctbl_pai_inicial) /*pi_valida_conta_pai_recursivo*/.
                   if  return-value = "NOK" /*l_nok*/ 
                   then do:
                      return "NOK" /*l_nok*/ .
                   end /* if */.            
                end /* if */.
            end /* for select_block */.
            if  avail b_dwb_rpt_select
            then do:
                if  b_dwb_rpt_select.cod_dwb_field  = "Conta Cont†bil"
                then do:            
                    assign v_wgh_focus = br_dwb_rpt_select_range:handle in frame f_rpt_42_sdo_cta_ctbl_balanct.
                    /* Faixa &1 irregular para Classificaá∆o por Estrutura ! */
                    run pi_messages (input "show",
                                     input 3781,
                                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                        "Conta Cont†bil", "Contas Cont†beis")) /*msg_3781*/.
                    return "NOK" /*l_nok*/ .
                end /* if */.
                else do:
                    assign v_wgh_focus = br_dwb_rpt_select_range:handle in frame f_rpt_42_sdo_cta_ctbl_balanct.
                    /* Faixa &1 irregular para Classificaá∆o por Estrutura ! */
                    run pi_messages (input "show",
                                     input 3781,
                                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                        "Alternativa Conta", "Contas Cont†beis")) /*msg_3781*/.
                    return "NOK" /*l_nok*/ .
                end /* else */.
            end /* if */.     
        end /* if */.
    end /* if */.
    else do:
        assign v_wgh_focus = br_dwb_rpt_select_range:handle in frame f_rpt_42_sdo_cta_ctbl_balanct.
        /* Faixa de &1 inexistente ! */
        run pi_messages (input "show",
                         input 3782,
                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                            "Conta Cont†bil")) /*msg_3782*/.
        return "NOK" /*l_nok*/ .
    end /* else */.
    /* --- Centro Custo ---*/
    run pi_validar_faixa_dwb_rpt_select (Input "Centro Custo") /*pi_validar_faixa_dwb_rpt_select*/.
    if  return-value = "3683"
    then do:
        assign v_wgh_focus = br_dwb_rpt_select_range:handle in frame f_rpt_42_sdo_cta_ctbl_balanct.
        /* Faixa de &1 irregular ! */
        run pi_messages (input "show",
                         input 3683,
                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                            "Centro Custo")) /*msg_3683*/.
        return "NOK" /*l_nok*/ .
    end /* if */.
    /* --- Estabelecimento ---*/
    run pi_validar_faixa_dwb_rpt_select (Input &IF '{&emsfin_version}' >= '5.01' AND '{&emsfin_version}' <= '5.07' &THEN "Estabelecimento" &ELSE "Estabelecimento" &ENDIF) /*pi_validar_faixa_dwb_rpt_select*/.
    if  return-value = "3683"
    then do:
        assign v_wgh_focus = br_dwb_rpt_select_range:handle in frame f_rpt_42_sdo_cta_ctbl_balanct.
        /* Faixa de &1 irregular ! */
        run pi_messages (input "show",
                         input 3683,
                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                            &IF '{&emsfin_version}' >= '5.01' AND '{&emsfin_version}' <= '5.07' &THEN "Estabelecimento" &ELSE "Estabelecimento" &ENDIF)) /*msg_3683*/.
        return "NOK" /*l_nok*/ .
    end /* if */.
    /* --- Unid Negoc ---*/
    run pi_validar_faixa_dwb_rpt_select (Input "Unid Neg¢cio") /*pi_validar_faixa_dwb_rpt_select*/.
    if  return-value = "3683"
    then do:
        assign v_wgh_focus = br_dwb_rpt_select_range:handle in frame f_rpt_42_sdo_cta_ctbl_balanct.
        /* Faixa de &1 irregular ! */
        run pi_messages (input "show",
                         input 3683,
                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                            "Unid Neg¢cio")) /*msg_3683*/.
        return "NOK" /*l_nok*/ .
    end /* if */.
    /* --- Projeto ---*/
    &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
        run pi_validar_faixa_dwb_rpt_select (Input "Projeto") /*pi_validar_faixa_dwb_rpt_select*/.
        if  return-value = "3683"
        then do:
            assign v_wgh_focus = br_dwb_rpt_select_range:handle in frame f_rpt_42_sdo_cta_ctbl_balanct.
            /* Faixa de &1 irregular ! */
            run pi_messages (input "show",
                             input 3683,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                "Projeto")) /*msg_3683*/.
            return "NOK" /*l_nok*/ .
        end /* if */.
    &ENDIF
    /* --- Atualiza valores conforme Classificaá∆o ---*/
    if  entry(1,dwb_rpt_param.cod_dwb_order) = "Estrutura" /*l_estrutura*/ 
    then do:
        assign v_log_cta_ctbl_analit = no.
    end /* if */.
    else do:
        assign v_num_niv_estrut = 999.
    end /* else */.

    if v_log_print = no and entry(1,dwb_rpt_param.cod_dwb_order) = "Estrutura" /*l_estrutura*/ 
    then do:
         for each dwb_rpt_select exclusive-lock
             where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
               and dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
               and dwb_rpt_select.cod_dwb_field   = "Conta Cont†bil" /*l_conta_contabil*/ 
               and dwb_rpt_select.log_dwb_rule    = yes:
             assign dwb_rpt_select.cod_dwb_initial = string(dwb_rpt_select.cod_dwb_initial, plano_cta_ctbl.cod_format_cta_ctbl)
                    dwb_rpt_select.cod_dwb_final   = string(dwb_rpt_select.cod_dwb_final  , plano_cta_ctbl.cod_format_cta_ctbl).
         end.
    end.
END PROCEDURE. /* pi_vld_rpt_sdo_cta_ctbl_balanct */
/*****************************************************************************
** Procedure Interna.....: pi_validar_faixa_dwb_rpt_select
** Descricao.............: pi_validar_faixa_dwb_rpt_select
** Criado por............: Henke
** Criado em.............: 22/11/1996 17:41:04
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_validar_faixa_dwb_rpt_select:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_dwb_field
        as character
        format "x(32)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************** Buffer Definition Begin *************************/

    &if "{&emsbas_version}" >= "1.00" &then
    def buffer b_dwb_rpt_select
        for dwb_rpt_select.
    &endif


    /*************************** Buffer Definition End **************************/

    for each dwb_rpt_select no-lock
       where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
       and   dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
       and   dwb_rpt_select.cod_dwb_field   = p_cod_dwb_field
       and   dwb_rpt_select.log_dwb_rule    = yes:
        for each b_dwb_rpt_select no-lock
           where b_dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
           and   b_dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
           and   b_dwb_rpt_select.cod_dwb_field   = p_cod_dwb_field
           and   b_dwb_rpt_select.log_dwb_rule    = yes
           and   b_dwb_rpt_select.num_dwb_order   <> dwb_rpt_select.num_dwb_order:

            if  b_dwb_rpt_select.cod_dwb_initial < dwb_rpt_select.cod_dwb_final 
            and b_dwb_rpt_select.cod_dwb_final   > dwb_rpt_select.cod_dwb_initial then
                return '3683'.
        end.
    end.
    return "OK" /*l_ok*/  .


END PROCEDURE. /* pi_validar_faixa_dwb_rpt_select */
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

        /*output stream s-arq to value(v_cod_arq) append.

        put stream s-arq unformatted
            p_cod_program            at 1 
            p_cod_program_ext        at 43 
            p_cod_version            at 69 
            today                    at 84 format "99/99/99"
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
        end.*/

        if  p_cod_program_type = 'dic' then do:
            &if '{&emsbas_version}' > '1.00' &then
            assign v_cod_event_dic = ENTRY(1,p_cod_program ,'/':U)
                   v_cod_tabela    = ENTRY(2,p_cod_program ,'/':U). /* FO 1100.980 */
            /*find tab_dic_dtsul 
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
            end /* if */.*/
            &endif
        end.

        /*output stream s-arq close.*/
    end /* if */.

END PROCEDURE. /* pi_version_extract */
/*****************************************************************************
** Procedure Interna.....: pi_retorna_conta
** Descricao.............: pi_retorna_conta
** Criado por............: src12345
** Criado em.............: 19/10/2000 14:25:05
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_retorna_conta:

    /************************ Parameter Definition Begin ************************/

    def input-output param p_des_cta_aux
        as character
        format "x(40)"
        no-undo.
    def Input param p_cod_format_aux_cta
        as character
        format "x(20)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_des_verifica_cta
        as character
        format "x(40)":U
        no-undo.
    def var v_num_contag
        as integer
        format ">>>>,>>9":U
        no-undo.


    /************************** Variable Definition End *************************/

    assign v_num_contag       = 1
           v_des_verifica_cta = "".

    do while v_num_contag <= length(p_des_cta_aux):
           if  (substring(p_des_cta_aux,v_num_contag,1) <> "-" and
                  substring(p_des_cta_aux,v_num_contag,1) <> ".") 
           or 
                ((substring(p_des_cta_aux,v_num_contag,1) = "-" and
                  substring(p_des_cta_aux,v_num_contag,1) <> substring(p_cod_format_aux_cta,v_num_contag,1)) or
                 (substring(p_des_cta_aux,v_num_contag,1) = "." and
                  substring(p_des_cta_aux,v_num_contag,1) <> substring(p_cod_format_aux_cta,v_num_contag,1)))
           then do:
                  assign v_des_verifica_cta = v_des_verifica_cta +  substring(p_des_cta_aux,v_num_contag,1).
           end /* if */.
          assign v_num_contag = v_num_contag + 1.
    end.

    assign p_des_cta_aux = v_des_verifica_cta
           v_num_contag = 1.    

    assign v_des_verifica_cta = "".
           p_des_cta_aux = string(p_des_cta_aux,p_cod_format_aux_cta).

    do while v_num_contag <= length(p_cod_format_aux_cta):
       if substring(p_cod_format_aux_cta,v_num_contag,1) = "9"  then do:
          if substring(p_des_cta_aux,v_num_contag,1) >= "0" and substring(p_des_cta_aux,v_num_contag,1) <= "9" then do:
             assign v_des_verifica_cta = v_des_verifica_cta + substring(p_des_cta_aux,v_num_contag,1).             
          end.
          else do:
             assign v_des_verifica_cta = v_des_verifica_cta +  "0".
          end.          
      end. 
      else do:
          assign v_des_verifica_cta = v_des_verifica_cta + substring(p_des_cta_aux,v_num_contag,1).         
      end.      
      assign v_num_contag = v_num_contag + 1.
    end.    
    assign p_des_cta_aux = v_des_verifica_cta.

END PROCEDURE. /* pi_retorna_conta */
/*****************************************************************************
** Procedure Interna.....: pi_seleciona_faixa
** Descricao.............: pi_seleciona_faixa
** Criado por............: bre18732
** Criado em.............: 26/12/2000 10:18:41
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_seleciona_faixa:

    /************************ Parameter Definition Begin ************************/

    def Input param p_rec_dwb_rpt_select
        as recid
        format ">>>>>>9"
        no-undo.
    def Input param p_des_procedure
        as character
        format "x(50)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_des_cta
        as character
        format "x(40)":U
        no-undo.
    def var v_des_cta_2
        as character
        format "x(40)":U
        no-undo.
    def var v_des_cta_antes
        as character
        format "x(40)":U
        no-undo.
    def var v_des_cta_antes_2
        as character
        format "x(40)":U
        no-undo.
    def var v_des_verifica_cta
        as character
        format "x(40)":U
        no-undo.
    def var v_log_ok
        as logical
        format "Sim/N∆o"
        initial yes
        no-undo.
    def var v_num_contag
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


    /************************** Variable Definition End *************************/

    /************************ Rectangle Definition Begin ************************/

    def rectangle rt_001
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

    def frame f_dlg_04_selecao_faixa
        rt_001
             at row 01.25 col 02.00
        rt_cxcf
             at row 06.17 col 02.00 bgcolor 7 
        dwb_rpt_select.cod_dwb_field
             at row 01.79 col 04.29 no-label
             view-as combo-box
             list-items "!"
              /*l_!*/
             inner-lines 5
             bgcolor 15 font 2
        bt_ok
             at row 06.38 col 03.00 font ?
             help "OK"
        bt_sav
             at row 06.38 col 14.00 font ?
             help "Salva"
        bt_can
             at row 06.38 col 25.00 font ?
             help "Cancela"
        bt_hel2
             at row 06.38 col 51.28 font ?
             help "Ajuda"
        with 1 down side-labels no-validate keep-tab-order three-d
             size-char 63.72 by 08.00 default-button bt_sav
             view-as dialog-box
             font 1 fgcolor ? bgcolor 8
             title "Seleá∆o na Impress∆o".
        /* adjust size of objects in this frame */
        assign bt_can:width-chars   in frame f_dlg_04_selecao_faixa = 10.00
               bt_can:height-chars  in frame f_dlg_04_selecao_faixa = 01.00
               bt_hel2:width-chars  in frame f_dlg_04_selecao_faixa = 10.00
               bt_hel2:height-chars in frame f_dlg_04_selecao_faixa = 01.00
               bt_ok:width-chars    in frame f_dlg_04_selecao_faixa = 10.00
               bt_ok:height-chars   in frame f_dlg_04_selecao_faixa = 01.00
               bt_sav:width-chars   in frame f_dlg_04_selecao_faixa = 10.00
               bt_sav:height-chars  in frame f_dlg_04_selecao_faixa = 01.00
               rt_001:width-chars   in frame f_dlg_04_selecao_faixa = 60.00
               rt_001:height-chars  in frame f_dlg_04_selecao_faixa = 04.75
               rt_cxcf:width-chars  in frame f_dlg_04_selecao_faixa = 60.28
               rt_cxcf:height-chars in frame f_dlg_04_selecao_faixa = 01.42.
        /* set private-data for the help system */
        assign dwb_rpt_select.cod_dwb_field:private-data in frame f_dlg_04_selecao_faixa = "HLP=000000000":U
               bt_ok:private-data                        in frame f_dlg_04_selecao_faixa = "HLP=000010721":U
               bt_sav:private-data                       in frame f_dlg_04_selecao_faixa = "HLP=000011048":U
               bt_can:private-data                       in frame f_dlg_04_selecao_faixa = "HLP=000011050":U
               bt_hel2:private-data                      in frame f_dlg_04_selecao_faixa = "HLP=000011326":U
               frame f_dlg_04_selecao_faixa:private-data                                 = "HLP=000000000".



{include/i_fclfrm.i f_dlg_04_selecao_faixa }
    /*************************** Frame Definition End ***************************/

    /*********************** User Interface Trigger Begin ***********************/


    ON CHOOSE OF bt_hel2 IN FRAME f_dlg_04_selecao_faixa
    DO:


        /* Begin_Include: i_context_help_frame */
        run prgtec/men/men900za.py (Input self:frame,
                                    Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.


        /* End_Include: i_context_help_frame */

    END. /* ON CHOOSE OF bt_hel2 IN FRAME f_dlg_04_selecao_faixa */

    ON CHOOSE OF bt_ok IN FRAME f_dlg_04_selecao_faixa
    DO:

        assign v_log_ok = yes.
    END. /* ON CHOOSE OF bt_ok IN FRAME f_dlg_04_selecao_faixa */

    ON VALUE-CHANGED OF dwb_rpt_select.cod_dwb_field IN FRAME f_dlg_04_selecao_faixa
    DO:

        assign v_cod_dwb_field = self:screen-value in frame f_dlg_04_selecao_faixa.

        run pi_isl_rpt_sdo_cta_ctbl_balanct /*pi_isl_rpt_sdo_cta_ctbl_balanct*/.

        if  v_wgh_label_ini = ?
        then do:
            create text v_wgh_label_ini
                assign frame        = frame f_dlg_04_selecao_faixa:handle
                    screen-value = "Inicial:" /*l_Inicial:*/ 
                    visible      = no
                    row          = 3.4
                    col          = 12
                    width        = 7.
{include/i_fcldin.i v_wgh_label_ini }
        end /* if */.

        if  v_wgh_label_fim = ?
        then do:
            create text v_wgh_label_fim
                assign frame        = frame f_dlg_04_selecao_faixa:handle
                    screen-value = "  Final:" /*l_bbfinal:*/ 
                    visible      = no
                    row          = 4.4
                    col          = 12
                    width        = 7.
{include/i_fcldin.i v_wgh_label_fim }
        end /* if */.

        if  v_wgh_fill_in_ini <> ?
        then do:
            delete widget v_wgh_fill_in_ini.
        end /* if */.

        create fill-in v_wgh_fill_in_ini
            assign frame              = frame f_dlg_04_selecao_faixa:handle
                   font               = 2
                   data-type          = v_cod_dat_type
                   format             = v_cod_format
                   side-label-handle  = v_wgh_label_ini:handle
                   row                = 3.4
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
            assign frame              = frame f_dlg_04_selecao_faixa:handle
                   font               = 2
                   data-type          = v_cod_dat_type
                   format             = v_cod_format
                   side-label-handle  = v_wgh_label_fim:handle
                   row                = 4.4
                   col                = 19
                   height             = 0.88
                   bgcolor            = 15
                   visible            = yes
                   sensitive          = yes
                   screen-value       = v_cod_final.
{include/i_fcldin.i v_wgh_fill_in_fim }

    END. /* ON VALUE-CHANGED OF dwb_rpt_select.cod_dwb_field IN FRAME f_dlg_04_selecao_faixa */


    /************************ User Interface Trigger End ************************/

    /**************************** Frame Trigger Begin ***************************/


    ON GO OF FRAME f_dlg_04_selecao_faixa
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

    END. /* ON GO OF FRAME f_dlg_04_selecao_faixa */

    ON HELP OF FRAME f_dlg_04_selecao_faixa ANYWHERE
    DO:


        /* Begin_Include: i_context_help */
        run prgtec/men/men900za.py (Input self:handle,
                                    Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
        /* End_Include: i_context_help */

    END. /* ON HELP OF FRAME f_dlg_04_selecao_faixa */

    ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_04_selecao_faixa ANYWHERE
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

    END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_04_selecao_faixa */

    ON RIGHT-MOUSE-UP OF FRAME f_dlg_04_selecao_faixa ANYWHERE
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

    END. /* ON RIGHT-MOUSE-UP OF FRAME f_dlg_04_selecao_faixa */


    /***************************** Frame Trigger End ****************************/

    pause 0 before-hide.
    view frame f_dlg_04_selecao_faixa.

    assign v_log_ok = no
           frame f_dlg_04_selecao_faixa:title = "Edita" /*l_edita*/  + " Conjunto de Seleá∆o" /*l_conjunto_selecao*/ .

    main_block:
    repeat while v_log_ok = no on endkey undo main_block, leave main_block on error undo main_block, leave main_block:

        find dwb_rpt_select where recid(dwb_rpt_select) = p_rec_dwb_rpt_select exclusive-lock no-error.

        assign dwb_rpt_select.cod_dwb_field:list-items in frame f_dlg_04_selecao_faixa = v_cod_dwb_select
               v_cod_dwb_field = dwb_rpt_select.cod_dwb_field.

        display dwb_rpt_select.cod_dwb_field
                with frame f_dlg_04_selecao_faixa.

        if v_cod_dwb_field = "Centro Custo" /*l_centro_custo*/ 
        and avail plano_ccusto then do:
           if v_cod_plano_antes <> plano_ccusto.cod_plano_ccusto then do:
              assign v_des_cta     = v_wgh_fill_in_ini:screen-value 
                     v_des_cta_antes = v_wgh_fill_in_ini:screen-value no-error.
              if v_cod_format <> "" then do:
                 run pi_retorna_conta (input-output v_des_cta,
                                       input v_cod_format).
                 if v_des_cta <> v_des_cta_antes then
                    assign dwb_rpt_select.cod_dwb_initial = plano_ccusto.cod_format_ccusto
                           dwb_rpt_select.cod_dwb_final = plano_ccusto.cod_format_ccusto
                           v_cod_plano_antes  = plano_ccusto.cod_plano_ccusto.
              end.
           end.
        end.

        run value(p_des_procedure) /*prg_value(p_des_procedure)*/.

        create text v_wgh_label_ini
            assign frame        = frame f_dlg_04_selecao_faixa:handle
                screen-value = "Inicial:" /*l_Inicial:*/ 
                visible      = no
                row          = 3.4
                col          = 12
                width        = 7.
{include/i_fcldin.i v_wgh_label_ini }

        create text v_wgh_label_fim
            assign frame        = frame f_dlg_04_selecao_faixa:handle
                screen-value = "  Final:" /*l_bbfinal:*/ 
                visible      = no
                row          = 4.4
                col          = 12
                width        = 7.
{include/i_fcldin.i v_wgh_label_fim }

        create fill-in v_wgh_fill_in_ini
            assign frame          = frame f_dlg_04_selecao_faixa:handle
               font               = 2
               data-type          = v_cod_dat_type
               format             = v_cod_format
               side-label-handle  = v_wgh_label_ini:handle
               row                = 3.4
               column             = 19
               height             = 0.88
               bgcolor            = 15
               visible            = yes
               sensitive          = yes
               screen-value       = dwb_rpt_select.cod_dwb_initial
               triggers:
                  on leave 
                  do:
                     if v_cod_dwb_field = "Estabelecimento" /*l_estabelecimento*/  then return.

                     assign v_des_cta     = v_wgh_fill_in_ini:screen-value 
                            v_des_cta_antes = v_wgh_fill_in_ini:screen-value no-error.
                     run pi_retorna_conta (input-output v_des_cta,
                                           input v_cod_format).
                     if v_des_cta <> v_des_cta_antes then do:
                       /* alteraá∆o sob demanda atividade 174520*/
                         if  v_wgh_fill_in_ini:screen-value <> v_cod_format
                         then do:
                            /* Formato &2 Inv†lido ! */
                            run pi_messages (input "show",
                                             input 4488,
                                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                               v_cod_format,' ')) /*msg_4488*/.
                            return no-apply.
                         end /* if */.
                         else do:
                            assign v_wgh_fill_in_ini:format = 'x(20)':U
                                   v_wgh_fill_in_ini:screen-value = v_des_cta
                                   v_wgh_fill_in_ini:screen-value = replace(v_wgh_fill_in_ini:screen-value,".",",")
                                   v_wgh_fill_in_ini:screen-value = replace(v_wgh_fill_in_ini:screen-value,"-",";" )
                                   v_wgh_fill_in_ini:format = v_cod_format
                                   v_wgh_fill_in_ini:screen-value = replace(v_wgh_fill_in_ini:screen-value, "," , ".")
                                   v_wgh_fill_in_ini:screen-value = replace(v_wgh_fill_in_ini:screen-value, ";" , "-").
                         end /* else */.
                     end.
                  end.                                   
                  on entry
                  do:
                     if v_cod_dwb_field = "Estabelecimento" /*l_estabelecimento*/  then return.

                     if v_wgh_fill_in_ini:screen-value = v_des_cta  then do: 
                            assign v_wgh_fill_in_ini:format = 'x(20)':U
                                   v_wgh_fill_in_ini:screen-value = replace(v_wgh_fill_in_ini:screen-value,".",",")

                                   v_wgh_fill_in_ini:screen-value = replace(v_wgh_fill_in_ini:screen-value,"-",";" )

                                   v_wgh_fill_in_ini:format = v_cod_format

                                   v_wgh_fill_in_ini:screen-value = replace(v_wgh_fill_in_ini:screen-value, "," , ".")

                                   v_wgh_fill_in_ini:screen-value = replace(v_wgh_fill_in_ini:screen-value, ";" , "-").
                     end.
                  end. 
               end triggers.
{include/i_fcldin.i v_wgh_fill_in_ini }

        create fill-in v_wgh_fill_in_fim
            assign frame          = frame f_dlg_04_selecao_faixa:handle
               font               = 2        
               data-type          = v_cod_dat_type
               format             = v_cod_format
               side-label-handle  = v_wgh_label_fim:handle
               row                = 4.4
               col                = 19
               height             = 0.88
               bgcolor            = 15
               visible            = yes
               sensitive          = yes
               screen-value       = dwb_rpt_select.cod_dwb_final
               triggers:
                   on leave 
                       do:
                           if v_cod_dwb_field = "Estabelecimento" /*l_estabelecimento*/  then return.

                           assign v_des_cta_2       = v_wgh_fill_in_fim:screen-value 
                                  v_des_cta_antes_2 = v_wgh_fill_in_fim:screen-value no-error.
                           run pi_retorna_conta (input-output v_des_cta_2,
                                                 input v_cod_format).
                           if v_des_cta_2 <> v_des_cta_antes_2 then do:
                                if  v_wgh_fill_in_fim:screen-value <> v_cod_format
                                then do:
                                /* alteraá∆o sob demanda atividade 174520*/
                                    /* Formato &2 Inv†lido ! */
                                    run pi_messages (input "show",
                                                     input 4488,
                                                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                                       v_cod_format,' ')) /*msg_4488*/.
                                    return no-apply.
                                end /* if */.
                                else do:
                                    assign v_wgh_fill_in_fim:format = 'x(20)':U
                                               v_wgh_fill_in_fim:screen-value = v_des_cta_2
                                               v_wgh_fill_in_fim:screen-value = replace(v_wgh_fill_in_fim:screen-value,".",",")

                                               v_wgh_fill_in_fim:screen-value = replace(v_wgh_fill_in_fim:screen-value,"-",";" )

                                               v_wgh_fill_in_fim:format = v_cod_format

                                               v_wgh_fill_in_fim:screen-value = replace(v_wgh_fill_in_fim:screen-value, "," , ".")

                                              v_wgh_fill_in_fim:screen-value = replace(v_wgh_fill_in_fim:screen-value, ";" , "-").
                                end /* else */.
                           end.
                       end.                                   
                   on entry
                      do:
                         if v_cod_dwb_field = "Estabelecimento" /*l_estabelecimento*/  then return.

                         if v_wgh_fill_in_fim:screen-value = v_des_cta_2  then do:
                                assign v_wgh_fill_in_fim:format = 'x(20)':U
                                       v_wgh_fill_in_fim:screen-value = replace(v_wgh_fill_in_fim:screen-value,".",",")

                                       v_wgh_fill_in_fim:screen-value = replace(v_wgh_fill_in_fim:screen-value,"-",";" )

                                       v_wgh_fill_in_fim:format = v_cod_format
                                       v_wgh_fill_in_fim:screen-value = replace(v_wgh_fill_in_fim:screen-value, "," , ".")

                                       v_wgh_fill_in_fim:screen-value = replace(v_wgh_fill_in_fim:screen-value, ";" , "-").
                         end.
                      end. 
               end triggers.
{include/i_fcldin.i v_wgh_fill_in_fim }
        disable dwb_rpt_select.cod_dwb_field
                with frame f_dlg_04_selecao_faixa.

        enable bt_ok
               bt_can
               with frame f_dlg_04_selecao_faixa.

        wait-for go of frame f_dlg_04_selecao_faixa.

        assign dwb_rpt_select.cod_dwb_initial = v_wgh_fill_in_ini:screen-value
               dwb_rpt_select.cod_dwb_final   = v_wgh_fill_in_fim:screen-value.

    end /* repeat main_block */.

    hide frame f_dlg_04_selecao_faixa.

    run pi_open_dwb_rpt_select_range /*pi_open_dwb_rpt_select_range*/.
END PROCEDURE. /* pi_seleciona_faixa */
/*****************************************************************************
** Procedure Interna.....: pi_isl_dwb_rpt_select_range_balanct
** Descricao.............: pi_isl_dwb_rpt_select_range_balanct
** Criado por............: src388
** Criado em.............: 27/03/2001 18:58:42
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_isl_dwb_rpt_select_range_balanct:

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

    def frame f_dlg_04_dwb_rpt_select_range
        rt_001
             at row 01.25 col 02.00
        rt_cxcf
             at row 06.17 col 02.00 bgcolor 7 
        dwb_rpt_select.cod_dwb_field
             at row 01.79 col 04.29 no-label
             view-as combo-box
             list-items "!"
              /*l_!*/
             inner-lines 5
             bgcolor 15 font 2
        bt_ok
             at row 06.38 col 03.00 font ?
             help "OK"
        bt_sav
             at row 06.38 col 14.00 font ?
             help "Salva"
        bt_can
             at row 06.38 col 25.00 font ?
             help "Cancela"
        bt_hel2
             at row 06.38 col 51.29 font ?
             help "Ajuda"
        with 1 down side-labels no-validate keep-tab-order three-d
             size-char 63.72 by 08.00 default-button bt_sav
             view-as dialog-box
             font 1 fgcolor ? bgcolor 8
             title "".
        /* adjust size of objects in this frame */
        assign bt_can:width-chars   in frame f_dlg_04_dwb_rpt_select_range = 10.00
               bt_can:height-chars  in frame f_dlg_04_dwb_rpt_select_range = 01.00
               bt_hel2:width-chars  in frame f_dlg_04_dwb_rpt_select_range = 10.00
               bt_hel2:height-chars in frame f_dlg_04_dwb_rpt_select_range = 01.00
               bt_ok:width-chars    in frame f_dlg_04_dwb_rpt_select_range = 10.00
               bt_ok:height-chars   in frame f_dlg_04_dwb_rpt_select_range = 01.00
               bt_sav:width-chars   in frame f_dlg_04_dwb_rpt_select_range = 10.00
               bt_sav:height-chars  in frame f_dlg_04_dwb_rpt_select_range = 01.00
               rt_001:width-chars   in frame f_dlg_04_dwb_rpt_select_range = 60.00
               rt_001:height-chars  in frame f_dlg_04_dwb_rpt_select_range = 04.75
               rt_cxcf:width-chars  in frame f_dlg_04_dwb_rpt_select_range = 60.29
               rt_cxcf:height-chars in frame f_dlg_04_dwb_rpt_select_range = 01.42.
        /* set private-data for the help system */
        assign dwb_rpt_select.cod_dwb_field:private-data in frame f_dlg_04_dwb_rpt_select_range = "HLP=000000000":U
               bt_ok:private-data                        in frame f_dlg_04_dwb_rpt_select_range = "HLP=000010721":U
               bt_sav:private-data                       in frame f_dlg_04_dwb_rpt_select_range = "HLP=000011048":U
               bt_can:private-data                       in frame f_dlg_04_dwb_rpt_select_range = "HLP=000011050":U
               bt_hel2:private-data                      in frame f_dlg_04_dwb_rpt_select_range = "HLP=000011326":U
               frame f_dlg_04_dwb_rpt_select_range:private-data                                 = "HLP=000000000".



{include/i_fclfrm.i f_dlg_04_dwb_rpt_select_range }
    /*************************** Frame Definition End ***************************/

    /*********************** User Interface Trigger Begin ***********************/


    ON CHOOSE OF bt_hel2 IN FRAME f_dlg_04_dwb_rpt_select_range
    DO:


        /* Begin_Include: i_context_help_frame */
        run prgtec/men/men900za.py (Input self:frame,
                                    Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.


        /* End_Include: i_context_help_frame */

    END. /* ON CHOOSE OF bt_hel2 IN FRAME f_dlg_04_dwb_rpt_select_range */

    ON CHOOSE OF bt_ok IN FRAME f_dlg_04_dwb_rpt_select_range
    DO:

        assign v_log_ok = yes.
    END. /* ON CHOOSE OF bt_ok IN FRAME f_dlg_04_dwb_rpt_select_range */

    ON VALUE-CHANGED OF dwb_rpt_select.cod_dwb_field IN FRAME f_dlg_04_dwb_rpt_select_range
    DO:

        assign v_cod_dwb_field = self:screen-value in frame f_dlg_04_dwb_rpt_select_range.

        run pi_isl_rpt_sdo_cta_ctbl_balanct /*pi_isl_rpt_sdo_cta_ctbl_balanct*/.

        if  v_wgh_label_ini = ?
        then do:
            create text v_wgh_label_ini
                assign frame        = frame f_dlg_04_dwb_rpt_select_range:handle
                    screen-value = "Inicial:" /*l_Inicial:*/ 
                    visible      = no
                    row          = 3.4
                    col          = 12
                    width        = 7.
{include/i_fcldin.i v_wgh_label_ini }
        end /* if */.

        if  v_wgh_label_fim = ?
        then do:
            create text v_wgh_label_fim
                assign frame        = frame f_dlg_04_dwb_rpt_select_range:handle
                    screen-value = "  Final:" /*l_bbfinal:*/ 
                    visible      = no
                    row          = 4.4
                    col          = 12
                    width        = 7.
{include/i_fcldin.i v_wgh_label_fim }
        end /* if */.

        if  v_wgh_fill_in_ini <> ?
        then do:
            delete widget v_wgh_fill_in_ini.
        end /* if */.

        create fill-in v_wgh_fill_in_ini
            assign frame              = frame f_dlg_04_dwb_rpt_select_range:handle
                   font               = 2
                   data-type          = v_cod_dat_type
                   format             = v_cod_format
                   side-label-handle  = v_wgh_label_ini:handle
                   row                = 3.4
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
            assign frame              = frame f_dlg_04_dwb_rpt_select_range:handle
                   font               = 2
                   data-type          = v_cod_dat_type
                   format             = v_cod_format
                   side-label-handle  = v_wgh_label_fim:handle
                   row                = 4.4
                   col                = 19
                   height             = 0.88
                   bgcolor            = 15
                   visible            = yes
                   sensitive          = yes
                   screen-value       = v_cod_final.
{include/i_fcldin.i v_wgh_fill_in_fim }

    END. /* ON VALUE-CHANGED OF dwb_rpt_select.cod_dwb_field IN FRAME f_dlg_04_dwb_rpt_select_range */


    /************************ User Interface Trigger End ************************/

    /**************************** Frame Trigger Begin ***************************/


    ON GO OF FRAME f_dlg_04_dwb_rpt_select_range
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

    END. /* ON GO OF FRAME f_dlg_04_dwb_rpt_select_range */

    ON HELP OF FRAME f_dlg_04_dwb_rpt_select_range ANYWHERE
    DO:


        /* Begin_Include: i_context_help */
        run prgtec/men/men900za.py (Input self:handle,
                                    Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
        /* End_Include: i_context_help */

    END. /* ON HELP OF FRAME f_dlg_04_dwb_rpt_select_range */

    ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_04_dwb_rpt_select_range ANYWHERE
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

    END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_04_dwb_rpt_select_range */

    ON RIGHT-MOUSE-UP OF FRAME f_dlg_04_dwb_rpt_select_range ANYWHERE
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

    END. /* ON RIGHT-MOUSE-UP OF FRAME f_dlg_04_dwb_rpt_select_range */


    /***************************** Frame Trigger End ****************************/

    pause 0 before-hide.
    view frame f_dlg_04_dwb_rpt_select_range.

    assign v_log_ok = no
           frame f_dlg_04_dwb_rpt_select_range:title = "Inclui" /*l_inclui*/  + " Conjunto de Seleá∆o" /*l_conjunto_selecao*/ .

    main_block:
    repeat while v_log_ok = no on endkey undo main_block, leave main_block 
                                            on error undo main_block, retry main_block:

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

        assign dwb_rpt_select.cod_dwb_field:list-items in frame f_dlg_04_dwb_rpt_select_range = v_cod_dwb_select
               dwb_rpt_select.cod_dwb_field:inner-lines in frame f_dlg_04_dwb_rpt_select_range = 6
               dwb_rpt_select.cod_dwb_field:screen-value = entry(1,v_cod_dwb_select).


        enable dwb_rpt_select.cod_dwb_field
               bt_ok
               bt_sav
               bt_can
               with frame f_dlg_04_dwb_rpt_select_range.
        apply "value-changed" to dwb_rpt_select.cod_dwb_field in frame f_dlg_04_dwb_rpt_select_range.

        wait-for go of frame f_dlg_04_dwb_rpt_select_range.

        assign dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
               dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
               dwb_rpt_select.num_dwb_order   = v_num_dwb_order
               dwb_rpt_select.cod_dwb_initial = v_wgh_fill_in_ini:screen-value
               dwb_rpt_select.cod_dwb_final   = v_wgh_fill_in_fim:screen-value
               dwb_rpt_select.cod_dwb_field.

    end /* repeat main_block */.

    hide frame f_dlg_04_dwb_rpt_select_range.

    run pi_open_dwb_rpt_select_range /*pi_open_dwb_rpt_select_range*/.
END PROCEDURE. /* pi_isl_dwb_rpt_select_range_balanct */
/*****************************************************************************
** Procedure Interna.....: pi_bt_down_rpt_sdo_cta_ctbl_balanct
** Descricao.............: pi_bt_down_rpt_sdo_cta_ctbl_balanct
** Criado por............: its0062
** Criado em.............: 20/05/2004 17:37:13
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_bt_down_rpt_sdo_cta_ctbl_balanct:

    assign v_cod_dwb_field = ls_order:screen-value in frame f_rpt_42_sdo_cta_ctbl_balanct
           v_cod_dwb_order = ls_order:list-items in frame f_rpt_42_sdo_cta_ctbl_balanct
           v_num_entry = lookup(v_cod_dwb_field, v_cod_dwb_order).

    if  v_num_entry > 0 and v_num_entry < num-entries (v_cod_dwb_order)
    then do:
        assign entry(v_num_entry, v_cod_dwb_order) = entry(v_num_entry + 1, v_cod_dwb_order)
               entry(v_num_entry + 1, v_cod_dwb_order) = v_cod_dwb_field
               ls_order:list-items in frame f_rpt_42_sdo_cta_ctbl_balanct   = v_cod_dwb_order
               ls_order:screen-value in frame f_rpt_42_sdo_cta_ctbl_balanct = v_cod_dwb_field.
        find dwb_rpt_param where recid(dwb_rpt_param) = v_rec_dwb_rpt_param exclusive-lock no-error.
        if  avail dwb_rpt_param
        then do:
            assign dwb_rpt_param.cod_dwb_order = v_cod_dwb_order.
        end .
    end .

END PROCEDURE. /* pi_bt_down_rpt_sdo_cta_ctbl_balanct */
/*****************************************************************************
** Procedure Interna.....: pi_bt_mais_rpt_sdo_cta_ctbl_balanct
** Descricao.............: pi_bt_mais_rpt_sdo_cta_ctbl_balanct
** Criado por............: its0062
** Criado em.............: 20/05/2004 17:47:37
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_bt_mais_rpt_sdo_cta_ctbl_balanct:

    assign v_log_balanct_param  = yes
           v_cod_unid_organ     = input frame f_rpt_42_sdo_cta_ctbl_balanct unid_organ.cod_unid_organ
           v_cod_plano_ccusto   = input frame f_rpt_42_sdo_cta_ctbl_balanct plano_ccusto.cod_plano_ccusto.
    if  search("prgfin/fgl/fgl900b.r") = ? and search("prgfin/fgl/fgl900b.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgfin/fgl/fgl900b.p".
        else do:
            message "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgfin/fgl/fgl900b.p"
                   view-as alert-box error buttons ok.
            stop.
        end.
    end.
    else
        run prgfin/fgl/fgl900b.p /*prg_fnc_sdo_cta_ctbl_balan*/.
END PROCEDURE. /* pi_bt_mais_rpt_sdo_cta_ctbl_balanct */
/*****************************************************************************
** Procedure Interna.....: pi_ix_p30_rpt_sdo_cta_ctbl_balanct
** Descricao.............: pi_ix_p30_rpt_sdo_cta_ctbl_balanct
** Criado por............: its38216
** Criado em.............: 16/08/2005 09:10:34
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_ix_p30_rpt_sdo_cta_ctbl_balanct:

    if v_num_cod_erro_aux <> 0 then assign v_cod_erro = string(v_num_cod_erro_aux).
    else assign v_cod_erro = "".
    &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
        if not v_log_eai_habilit then do:
            /*if (line-counter(s_1) + 34) > v_rpt_s_1_bottom then
                page stream s_1.
            put stream s_1 unformatted 
                skip (1)
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                "Unid Organizacional: " at 31
                v_cod_unid_organ at 52 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                "Unid Organizacional: " at 31
                v_cod_unid_organ at 52 format "x(5)"
    &ENDIF skip
                "Plano Contas: " at 38
                v_cod_plano_cta_ctbl at 52 format "x(8)" skip
                "Plano CCusto: " at 38
                v_cod_plano_ccusto at 52 format "x(8)" skip
                "Cen†rio Cont†bil: " at 34
                v_cod_cenar_ctbl at 52 format "x(8)" skip
                "Exerc°cio Cont†bil: " at 32
                v_cod_exerc_ctbl at 52 format "9999" skip
                "Per°odo Atual: " at 37
                v_num_period_ctbl to 53 format "99" skip
                "Sumaria Estab: " at 37
                v_log_estab_sum at 52 format "Sim/N∆o" skip
                "Sumaria Unid Negoc: " at 32
                v_log_unid_negoc_sum at 52 format "Sim/N∆o" skip
                "Sumaria Projeto: " at 35
                v_log_proj_financ at 52 format "Sim/N∆o" skip
                "Sumaria Ccusto: " at 36
                v_log_ccusto_sum at 52 format "Sim/N∆o" skip
                "Finalidade Base: " at 35
                v_cod_finalid_econ_bas at 52 format "x(10)" skip
                "Finalid Converte: " at 34
                v_cod_finalid_econ_apr at 52 format "x(10)" skip
                "Data Cotaá∆o: " at 38
                v_dat_cotac_indic_econ at 52 format "99/99/9999" skip
                "Consid Cta Internac: " at 31
                v_log_cta_ctbl_internac at 52 format "Sim/N∆o" skip
                "Impr Period Anterior: " at 30
                v_log_period_ctbl_ant_impr at 52 format "Sim/N∆o" skip
                "Impr Cta Sem Sdo: " at 34
                v_log_cta_ctbl_sdo at 52 format "Sim/N∆o" skip
                "Somente Cta Analit: " at 32
                v_log_cta_ctbl_analit at 52 format "Sim/N∆o" skip
                "Consid Apurac Restdo: " at 30
                v_log_consid_apurac_restdo at 52 format "Sim/N∆o" skip
                "Mostra sem Aprop CC: " at 31
                v_log_mostra_sem_aprop_cc at 52 format "Sim/N∆o" skip
                "N°vel Estrutura: " at 35
                v_num_niv_estrut to 54 format ">>9" skip
                "Idioma Apresentaá∆o: " at 31
                v_cod_idioma_apr at 52 format "x(8)" skip
                "Parte Fixa: " at 40
                v_cod_cta_ctbl_pfixa at 52 format "x(20)" skip
                "Parte Exceá∆o: " at 37
                v_cod_cta_ctbl_excec at 52 format "x(20)" skip
                "Parte Fixa CCusto: " at 33
                v_cod_ccusto_pfixa at 52 format "x(11)" skip
                "Parte Exceá∆o: " at 37
                v_cod_ccusto_excec at 52 format "x(11)" skip
                "PFixa Proj: " at 40
                v_cod_proj_financ_pfixa at 52 format "x(20)" skip
                "Exceá∆o: " at 43
                v_cod_proj_financ_excec at 52 format "x(20)" skip
                "Gera Planilha: " at 37
                v_log_gerac_planilha at 52 format "Sim/N∆o" skip
                "Arq Planilha: " at 38
                v_cod_arq_planilha at 52 format "x(40)" skip
                "Caracter Delimitador: " at 30
                v_cod_carac_lim at 52 format "x(1)" skip
                "Tempo Execuá∆o: " at 36
                v_cod_tempo_exec at 52 format "x(8)"
                skip (1)
                v_cod_erro at 1 format "x(10)"
                v_des_error_aux at 12 format "x(60)"
                v_des_ajuda_aux_1 at 73 format "x(80)" skip.*/
        end.
        else do:
            &if '{&emsbas_version}' >= '5.05' &then
                /*if (line-counter(s_1) + 33) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    skip (1)
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                    "Unid Organizacional: " at 31
                    v_cod_unid_organ at 52 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                    "Unid Organizacional: " at 31
                    v_cod_unid_organ at 52 format "x(5)"
    &ENDIF skip
                    "Plano Contas: " at 38
                    v_cod_plano_cta_ctbl at 52 format "x(8)" skip
                    "Plano CCusto: " at 38
                    v_cod_plano_ccusto at 52 format "x(8)" skip
                    "Cen†rio Cont†bil: " at 34
                    v_cod_cenar_ctbl at 52 format "x(8)" skip
                    "Exerc°cio Cont†bil: " at 32
                    v_cod_exerc_ctbl at 52 format "9999" skip
                    "Per°odo Atual: " at 37
                    v_num_period_ctbl to 53 format "99" skip
                    "Sumaria Estab: " at 37
                    v_log_estab_sum at 52 format "Sim/N∆o" skip
                    "Sumaria Unid Negoc: " at 32
                    v_log_unid_negoc_sum at 52 format "Sim/N∆o" skip
                    "Sumaria Projeto: " at 35
                    v_log_proj_financ at 52 format "Sim/N∆o" skip
                    "Sumaria Ccusto: " at 36
                    v_log_ccusto_sum at 52 format "Sim/N∆o" skip
                    "Finalidade Base: " at 35
                    v_cod_finalid_econ_bas at 52 format "x(10)" skip
                    "Finalid Converte: " at 34
                    v_cod_finalid_econ_apr at 52 format "x(10)" skip
                    "Data Cotaá∆o: " at 38
                    v_dat_cotac_indic_econ at 52 format "99/99/9999" skip
                    "Consid Cta Internac: " at 31
                    v_log_cta_ctbl_internac at 52 format "Sim/N∆o" skip
                    "Impr Period Anterior: " at 30
                    v_log_period_ctbl_ant_impr at 52 format "Sim/N∆o" skip
                    "Impr Cta Sem Sdo: " at 34
                    v_log_cta_ctbl_sdo at 52 format "Sim/N∆o" skip
                    "Somente Cta Analit: " at 32
                    v_log_cta_ctbl_analit at 52 format "Sim/N∆o" skip
                    "Consid Apurac Restdo: " at 30
                    v_log_consid_apurac_restdo at 52 format "Sim/N∆o" skip
                    "Mostra sem Aprop CC: " at 31
                    v_log_mostra_sem_aprop_cc at 52 format "Sim/N∆o" skip
                    "N°vel Estrutura: " at 35
                    v_num_niv_estrut to 54 format ">>9" skip
                    "Idioma Apresentaá∆o: " at 31
                    v_cod_idioma_apr at 52 format "x(8)" skip
                    "Parte Fixa: " at 40
                    v_cod_cta_ctbl_pfixa at 52 format "x(20)" skip
                    "Parte Exceá∆o: " at 37
                    v_cod_cta_ctbl_excec at 52 format "x(20)" skip
                    "Parte Fixa CCusto: " at 33
                    v_cod_ccusto_pfixa at 52 format "x(11)" skip
                    "Parte Exceá∆o: " at 37
                    v_cod_ccusto_excec at 52 format "x(11)" skip
                    "PFixa Proj: " at 40
                    v_cod_proj_financ_pfixa at 52 format "x(20)" skip
                    "Exceá∆o: " at 43
                    v_cod_proj_financ_excec at 52 format "x(20)" skip
                    "Gera Planilha: " at 37
                    v_log_gerac_planilha at 52 format "Sim/N∆o" skip
                    "Arq Planilha: " at 38
                    v_cod_arq_planilha at 52 format "x(40)" skip
                    "Caracter Delimitador: " at 30
                    v_cod_carac_lim at 52 format "x(1)" skip
                    "Tempo Execuá∆o: " at 36
                    v_cod_tempo_exec at 52 format "x(8)" skip
                    "Gera XML: " at 42
                    v_log_gera_eai at 52 format "Sim/N∆o" skip.*/
            &endif
        end.
    &ELSE
        if not v_log_eai_habilit then do:
            /*if (line-counter(s_1) + 31) > v_rpt_s_1_bottom then
                page stream s_1.
            put stream s_1 unformatted 
                skip (1)
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                "Unid Organizacional: " at 31
                v_cod_unid_organ at 52 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                "Unid Organizacional: " at 31
                v_cod_unid_organ at 52 format "x(5)"
    &ENDIF skip
                "Plano Contas: " at 38
                v_cod_plano_cta_ctbl at 52 format "x(8)" skip
                "Plano CCusto: " at 38
                v_cod_plano_ccusto at 52 format "x(8)" skip
                "Cen†rio Cont†bil: " at 34
                v_cod_cenar_ctbl at 52 format "x(8)" skip
                "Exerc°cio Cont†bil: " at 32
                v_cod_exerc_ctbl at 52 format "9999" skip
                "Per°odo Atual: " at 37
                v_num_period_ctbl to 53 format "99" skip
                "Sumaria Estab: " at 37
                v_log_estab_sum at 52 format "Sim/N∆o" skip
                "Sumaria Unid Negoc: " at 32
                v_log_unid_negoc_sum at 52 format "Sim/N∆o" skip
                "Sumaria Ccusto: " at 36
                v_log_ccusto_sum at 52 format "Sim/N∆o" skip
                "Finalidade Base: " at 35
                v_cod_finalid_econ_bas at 52 format "x(10)" skip
                "Finalid Converte: " at 34
                v_cod_finalid_econ_apr at 52 format "x(10)" skip
                "Data Cotaá∆o: " at 38
                v_dat_cotac_indic_econ at 52 format "99/99/9999" skip
                "Consid Cta Internac: " at 31
                v_log_cta_ctbl_internac at 52 format "Sim/N∆o" skip
                "Impr Period Anterior: " at 30
                v_log_period_ctbl_ant_impr at 52 format "Sim/N∆o" skip
                "Impr Cta Sem Sdo: " at 34
                v_log_cta_ctbl_sdo at 52 format "Sim/N∆o" skip
                "Somente Cta Analit: " at 32
                v_log_cta_ctbl_analit at 52 format "Sim/N∆o" skip
                "Consid Apurac Restdo: " at 30
                v_log_consid_apurac_restdo at 52 format "Sim/N∆o" skip
                "Mostra sem Aprop CC: " at 31
                v_log_mostra_sem_aprop_cc at 52 format "Sim/N∆o" skip
                "N°vel Estrutura: " at 35
                v_num_niv_estrut to 54 format ">>9" skip
                "Idioma Apresentaá∆o: " at 31
                v_cod_idioma_apr at 52 format "x(8)" skip
                "Parte Fixa: " at 40
                v_cod_cta_ctbl_pfixa at 52 format "x(20)" skip
                "Parte Exceá∆o: " at 37
                v_cod_cta_ctbl_excec at 52 format "x(20)" skip
                "Parte Fixa CCusto: " at 33
                v_cod_ccusto_pfixa at 52 format "x(11)" skip
                "Parte Exceá∆o: " at 37
                v_cod_ccusto_excec at 52 format "x(11)" skip
                "Gera Planilha: " at 37
                v_log_gerac_planilha at 52 format "Sim/N∆o" skip
                "Arq Planilha: " at 38
                v_cod_arq_planilha at 52 format "x(40)" skip
                "Caracter Delimitador: " at 30
                v_cod_carac_lim at 52 format "x(1)" skip
                "Tempo Execuá∆o: " at 36
                v_cod_tempo_exec at 52 format "x(8)"
                skip (1)
                v_cod_erro at 1 format "x(10)"
                v_des_error_aux at 12 format "x(60)"
                v_des_ajuda_aux_1 at 73 format "x(80)" skip.*/
        end.
        else do:
            &if '{&emsbas_version}' >= '5.05' &then
                /*if (line-counter(s_1) + 30) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    skip (1)
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                    "Unid Organizacional: " at 31
                    v_cod_unid_organ at 52 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                    "Unid Organizacional: " at 31
                    v_cod_unid_organ at 52 format "x(5)"
    &ENDIF skip
                    "Plano Contas: " at 38
                    v_cod_plano_cta_ctbl at 52 format "x(8)" skip
                    "Plano CCusto: " at 38
                    v_cod_plano_ccusto at 52 format "x(8)" skip
                    "Cen†rio Cont†bil: " at 34
                    v_cod_cenar_ctbl at 52 format "x(8)" skip
                    "Exerc°cio Cont†bil: " at 32
                    v_cod_exerc_ctbl at 52 format "9999" skip
                    "Per°odo Atual: " at 37
                    v_num_period_ctbl to 53 format "99" skip
                    "Sumaria Estab: " at 37
                    v_log_estab_sum at 52 format "Sim/N∆o" skip
                    "Sumaria Unid Negoc: " at 32
                    v_log_unid_negoc_sum at 52 format "Sim/N∆o" skip
                    "Sumaria Ccusto: " at 36
                    v_log_ccusto_sum at 52 format "Sim/N∆o" skip
                    "Finalidade Base: " at 35
                    v_cod_finalid_econ_bas at 52 format "x(10)" skip
                    "Finalid Converte: " at 34
                    v_cod_finalid_econ_apr at 52 format "x(10)" skip
                    "Data Cotaá∆o: " at 38
                    v_dat_cotac_indic_econ at 52 format "99/99/9999" skip
                    "Consid Cta Internac: " at 31
                    v_log_cta_ctbl_internac at 52 format "Sim/N∆o" skip
                    "Impr Period Anterior: " at 30
                    v_log_period_ctbl_ant_impr at 52 format "Sim/N∆o" skip
                    "Impr Cta Sem Sdo: " at 34
                    v_log_cta_ctbl_sdo at 52 format "Sim/N∆o" skip
                    "Somente Cta Analit: " at 32
                    v_log_cta_ctbl_analit at 52 format "Sim/N∆o" skip
                    "Consid Apurac Restdo: " at 30
                    v_log_consid_apurac_restdo at 52 format "Sim/N∆o" skip
                    "Mostra sem Aprop CC: " at 31
                    v_log_mostra_sem_aprop_cc at 52 format "Sim/N∆o" skip
                    "N°vel Estrutura: " at 35
                    v_num_niv_estrut to 54 format ">>9" skip
                    "Idioma Apresentaá∆o: " at 31
                    v_cod_idioma_apr at 52 format "x(8)" skip
                    "Parte Fixa: " at 40
                    v_cod_cta_ctbl_pfixa at 52 format "x(20)" skip
                    "Parte Exceá∆o: " at 37
                    v_cod_cta_ctbl_excec at 52 format "x(20)" skip
                    "Parte Fixa CCusto: " at 33
                    v_cod_ccusto_pfixa at 52 format "x(11)" skip
                    "Parte Exceá∆o: " at 37
                    v_cod_ccusto_excec at 52 format "x(11)" skip
                    "Gera Planilha: " at 37
                    v_log_gerac_planilha at 52 format "Sim/N∆o" skip
                    "Arq Planilha: " at 38
                    v_cod_arq_planilha at 52 format "x(40)" skip
                    "Caracter Delimitador: " at 30
                    v_cod_carac_lim at 52 format "x(1)" skip
                    "Tempo Execuá∆o: " at 36
                    v_cod_tempo_exec at 52 format "x(8)" skip
                    "Gera XML: " at 42
                    v_log_gera_eai at 52 format "Sim/N∆o" skip.*/
            &endif
        end.
    &ENDIF
    if  v_des_lista_estab <> "" then do:
        assign v_des_lista_estab = substitute("ATEN¯∞O! O usu†rio &1 N∆o possui acesso aos seguintes estabelecimentos, que foram ent∆o desconsiderados neste relat¢rio: " /*9744*/, v_cod_usuar_corren) + v_des_lista_estab + ".".
        /*put stream s_1 skip(2).*/
        run pi_print_editor ("s_1", v_des_lista_estab, "     080", "", "     ", "", "     ").
        /*put stream s_1 unformatted 
            skip (1)
            entry(1, return-value, chr(255)) at 15 format "x(80)" skip.*/
        run pi_print_editor ("s_1", v_des_lista_estab, "at015080", "", "", "", "").
    end.
END PROCEDURE. /* pi_ix_p30_rpt_sdo_cta_ctbl_balanct */
/*****************************************************************************
** Procedure Interna.....: pi_ix_p10_rpt_sdo_cta_ctbl_balanct
** Descricao.............: pi_ix_p10_rpt_sdo_cta_ctbl_balanct
** Criado por............: its38216
** Criado em.............: 16/08/2005 09:24:34
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_ix_p10_rpt_sdo_cta_ctbl_balanct:

    /* --- Inicializaá¥es ---*/
    if  v_log_print = no
    then do:
        if  num-entries(dwb_rpt_param.cod_dwb_parameters,chr(10)) >= &IF DEFINED(BF_ADM_FIN_PROJ) &THEN 31 &ELSE 28 &ENDIF
        then do:
            assign v_cod_plano_cta_ctbl = entry(1, dwb_rpt_param.cod_dwb_parameters,chr(10))
            v_cod_cenar_ctbl = entry(2, dwb_rpt_param.cod_dwb_parameters,chr(10))
            v_cod_exerc_ctbl = entry(3, dwb_rpt_param.cod_dwb_parameters,chr(10))
            v_num_period_ctbl = int(entry(4, dwb_rpt_param.cod_dwb_parameters,chr(10)))
            v_log_estab_sum = (entry(5, dwb_rpt_param.cod_dwb_parameters,chr(10)) = 'yes')
            v_log_unid_negoc_sum = (entry(6, dwb_rpt_param.cod_dwb_parameters,chr(10)) = 'yes')
            v_log_ccusto_sum = (entry(7, dwb_rpt_param.cod_dwb_parameters,chr(10)) = 'yes')
            v_cod_finalid_econ_bas = entry(8, dwb_rpt_param.cod_dwb_parameters,chr(10))
            v_cod_finalid_econ_apr = entry(9, dwb_rpt_param.cod_dwb_parameters,chr(10))
            v_dat_cotac_indic_econ = date(entry(10, dwb_rpt_param.cod_dwb_parameters,chr(10)))
            v_log_cta_ctbl_internac = (entry(11, dwb_rpt_param.cod_dwb_parameters,chr(10)) = 'yes')
            v_log_period_ctbl_ant_impr = (entry(12, dwb_rpt_param.cod_dwb_parameters,chr(10)) = 'yes')
            v_log_cta_ctbl_sdo = (entry(13, dwb_rpt_param.cod_dwb_parameters,chr(10)) = 'yes')
            v_log_cta_ctbl_analit = (entry(14, dwb_rpt_param.cod_dwb_parameters,chr(10)) = 'yes')
            v_log_consid_apurac_restdo = (entry(15, dwb_rpt_param.cod_dwb_parameters,chr(10)) = 'yes')
            v_num_niv_estrut = int(entry(16, dwb_rpt_param.cod_dwb_parameters,chr(10)))
            v_cod_idioma_apr = entry(17, dwb_rpt_param.cod_dwb_parameters,chr(10))
            v_rec_dwb_rpt_param = int(entry(18, dwb_rpt_param.cod_dwb_parameters,chr(10)))
            v_log_gerac_planilha = (entry(19, dwb_rpt_param.cod_dwb_parameters,chr(10)) = 'yes')
            v_cod_arq_planilha = entry(20, dwb_rpt_param.cod_dwb_parameters,chr(10))
            v_cod_carac_lim = entry(21, dwb_rpt_param.cod_dwb_parameters,chr(10))
            v_cod_plano_ccusto = entry(22, dwb_rpt_param.cod_dwb_parameters,chr(10))
            v_cod_unid_organ = entry(23, dwb_rpt_param.cod_dwb_parameters,chr(10))
            v_cod_cta_ctbl_pfixa = entry(24, dwb_rpt_param.cod_dwb_parameters,chr(10))
            v_cod_cta_ctbl_excec = entry(25, dwb_rpt_param.cod_dwb_parameters,chr(10))
            v_cod_ccusto_pfixa = entry(26, dwb_rpt_param.cod_dwb_parameters,chr(10))
            v_cod_ccusto_excec = entry(27, dwb_rpt_param.cod_dwb_parameters,chr(10))
            v_log_mostra_sem_aprop_cc = (entry(28,dwb_rpt_param.cod_dwb_parameters,chr(10)) = 'yes')
            &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
            v_cod_proj_financ_pfixa = entry(29, dwb_rpt_param.cod_dwb_parameters,chr(10))
            v_cod_proj_financ_excec = entry(30, dwb_rpt_param.cod_dwb_parameters,chr(10))
            v_log_proj_financ = (entry(31,dwb_rpt_param.cod_dwb_parameters,chr(10)) = 'yes')               
            &ENDIF.
            &if '{&emsbas_version}' >= '5.05' &then
              if v_log_eai_habilit then do:
                  if num-entries(dwb_rpt_param.cod_dwb_parameters,chr(10)) = 32 then assign v_log_gera_eai = (entry(32,dwb_rpt_param.cod_dwb_parameters,chr(10)) = "yes" /*l_yes*/ ).
                else do:
                    if num-entries(dwb_rpt_param.cod_dwb_parameters,chr(10)) = 29 then assign v_log_gera_eai = (entry(29,dwb_rpt_param.cod_dwb_parameters,chr(10)) = "yes" /*l_yes*/ ).
                 end.
              end.   
            &endif
        end /* if */.
        else do:
            assign v_log_consid_apurac_restdo = yes
            v_cod_cta_ctbl_pfixa = fill("#", 20)
            v_cod_cta_ctbl_excec = fill("#", 20)
            v_cod_ccusto_pfixa = fill("#", 11)
            v_cod_ccusto_excec = fill("#", 11)
            &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
              v_cod_proj_financ_pfixa = fill("#", 20)
              v_cod_proj_financ_excec = fill("#", 20)
            &ENDIF.
        end /* else */.
        assign ls_order:list-items in frame f_rpt_42_sdo_cta_ctbl_balanct = dwb_rpt_param.cod_dwb_order
        v_rec_dwb_rpt_param = recid(dwb_rpt_param).
        enable bt_down
               bt_up
               with frame f_rpt_42_sdo_cta_ctbl_balanct.
        if  ls_order:lookup("Estrutura" /*l_estrutura*/ ) in frame f_rpt_42_sdo_cta_ctbl_balanct <> 0
        then do:
           assign ls_order:screen-value in frame f_rpt_42_sdo_cta_ctbl_balanct = ls_order:entry(1).
        end /* if */.
        else do:
           if  not ls_order:add-last("Estrutura" /*l_estrutura*/ ) in frame f_rpt_42_sdo_cta_ctbl_balanct
           then do:
              assign ls_order:screen-value in frame f_rpt_42_sdo_cta_ctbl_balanct = ls_order:entry(1).
           end /* if */.
        end /* else */.
        apply "choose" to bt_up in frame f_rpt_42_sdo_cta_ctbl_balanct.
        if  v_cod_plano_cta_ctbl = ""
        then do:
            run pi_retornar_plano_cta_ctbl_prim (Input v_cod_empres_usuar,
                                                 Input ?,
                                                 output v_cod_plano_cta_ctbl,
                                                 output v_log_plano_cta_ctbl_uni) /*pi_retornar_plano_cta_ctbl_prim*/.
        end /* if */.
        find plano_cta_ctbl no-lock
           where plano_cta_ctbl.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl no-error.
        if  avail plano_cta_ctbl
        then do:
            assign v_rec_plano_cta_ctbl = recid(plano_cta_ctbl).
            display plano_cta_ctbl.cod_plano_cta_ctbl
                    with frame f_rpt_42_sdo_cta_ctbl_balanct.
        end /* if */.
        if  v_cod_unid_organ = "" then
            find unid_organ
               where unid_organ.cod_unid_organ = v_cod_empres_usuar no-lock no-error.
        else
            find unid_organ
               where unid_organ.cod_unid_organ = v_cod_unid_organ no-lock no-error.
        if  avail unid_organ
        then do:
            assign v_rec_unid_organ = recid(unid_organ).
            display unid_organ.cod_unid_organ
                    with frame f_rpt_42_sdo_cta_ctbl_balanct.
        end /* if */.
        if  v_cod_cenar_ctbl = ""
        then do:
            run pi_retornar_cenar_ctbl_fisc (Input v_cod_empres_usuar,
                                             Input ?,
                                             output v_cod_cenar_ctbl) /*pi_retornar_cenar_ctbl_fisc*/.
        end /* if */.
        find cenar_ctbl no-lock
              where cenar_ctbl.cod_cenar_ctbl = v_cod_cenar_ctbl no-error.
        display cenar_ctbl.cod_cenar_ctbl when avail cenar_ctbl
                "" when not avail cenar_ctbl @ cenar_ctbl.cod_cenar_ctbl
                with frame f_rpt_42_sdo_cta_ctbl_balanct.
        if  v_cod_exerc_ctbl  <> "" and v_num_period_ctbl <> 0
        then do:
            find exerc_ctbl no-lock
               where exerc_ctbl.cod_cenar_ctbl = v_cod_cenar_ctbl
                 and exerc_ctbl.cod_exerc_ctbl = v_cod_exerc_ctbl no-error.
            find period_ctbl no-lock
               where period_ctbl.cod_cenar_ctbl = v_cod_cenar_ctbl
                 and period_ctbl.cod_exerc_ctbl = v_cod_exerc_ctbl
                 and period_ctbl.num_period_ctbl = v_num_period_ctbl no-error.
        end /* if */.
        else do:
            find first exerc_ctbl no-lock
               where exerc_ctbl.cod_cenar_ctbl = v_cod_cenar_ctbl
                 and exerc_ctbl.cod_exerc_ctbl >= string(year(today)) no-error.
            if  not avail exerc_ctbl or not can-find(first period_ctbl
                    where period_ctbl.cod_cenar_ctbl = exerc_ctbl.cod_cenar_ctbl
                      and period_ctbl.cod_exerc_ctbl = exerc_ctbl.cod_exerc_ctbl
                    )
                    then do:
                find first period_ctbl no-lock
                     where period_ctbl.cod_cenar_ctbl = cenar_ctbl.cod_cenar_ctbl /*cl_cenar_ctbl of period_ctbl*/ no-error.
                find exerc_ctbl no-lock
                     where exerc_ctbl.cod_cenar_ctbl = period_ctbl.cod_cenar_ctbl
                       and exerc_ctbl.cod_exerc_ctbl = period_ctbl.cod_exerc_ctbl
                      no-error.
            end /* if */.
            if  avail exerc_ctbl
            then do:
                find last period_ctbl no-lock
                     where period_ctbl.cod_cenar_ctbl = exerc_ctbl.cod_cenar_ctbl
                       and period_ctbl.cod_exerc_ctbl = exerc_ctbl.cod_exerc_ctbl
                       and period_ctbl.dat_fim_period_ctbl < date(month(today),day(today),int(exerc_ctbl.cod_exerc_ctbl)) no-error.
            end /* if */.
        end /* else */.
        if  avail period_ctbl
        then do:
            display exerc_ctbl.cod_exerc_ctbl
                    period_ctbl.num_period_ctbl
                    with frame f_rpt_42_sdo_cta_ctbl_balanct.
            assign v_cod_exerc_ctbl = period_ctbl.cod_exerc_ctbl
            v_dat_fim_period_ctbl = period_ctbl.dat_fim_period_ctbl.
        end /* if */.
        else do:
            assign exerc_ctbl.cod_exerc_ctbl:screen-value = string(year(today))
            period_ctbl.num_period_ctbl:screen-value = string(month(today))
            v_cod_exerc_ctbl = string(year(today))
            v_dat_fim_period_ctbl = today.
        end /* else */.
        apply "leave" to period_ctbl.num_period_ctbl in frame f_rpt_42_sdo_cta_ctbl_balanct.
        if  v_cod_finalid_econ_bas = ""
        then do:
            run pi_retornar_finalid_econ_corren_estab (Input v_cod_estab_usuar,
                                                       output v_cod_finalid_econ_bas) /*pi_retornar_finalid_econ_corren_estab*/.
            if  v_cod_finalid_econ_bas <> ""
            then do:
                assign v_cod_finalid_econ_apr = v_cod_finalid_econ_bas
                v_dat_cotac_indic_econ = today.
            end /* if */.
        end /* if */.
        if  v_cod_idioma_apr = ""
        then do:
            if  v_num_niv_estrut = 0 then assign v_num_niv_estrut = 999.
            if  v_cod_idioma_apr = ? or  v_cod_idioma_apr = ""
            then do:
                find param_geral_ems no-lock no-error.
                if  avail param_geral_ems then assign v_cod_idioma_apr = param_geral_ems.cod_idiom_princ.
            end /* if */.
        end /* if */.
        assign v_nom_prog_ext_aux = lc("fgl900a":U)
        v_wgh_focus = unid_organ.cod_unid_organ:handle in frame f_rpt_42_sdo_cta_ctbl_balanct
        v_val_maximum = 0
        v_val_current_value = 0
        v_cod_plano_cta_ctbl_ant = v_cod_plano_cta_ctbl
        v_cod_plano_ccusto_ant = v_cod_plano_ccusto.
    end /* if */.
    &if '{&emsfin_version}' >= '5.05' &then
        if  search("prgint/utb/utb402aa.r") = ? and search("prgint/utb/utb402aa.py") = ? then do:
            if  v_cod_dwb_user begins 'es_' then
                return "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb402aa.py".
            else do:
                message "Programa execut†vel N∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb402aa.py"
                       view-as alert-box error buttons ok.
                return.
            end.
        end.
        else
            run prgint/utb/utb402aa.py /*prg_spp_proj_financ_estab*/.
    &endif
END PROCEDURE. /* pi_ix_p10_rpt_sdo_cta_ctbl_balanct */
/*****************************************************************************
** Procedure Interna.....: pi_valida_conta_pai_recursivo
** Descricao.............: pi_valida_conta_pai_recursivo
** Criado por............: its38216
** Criado em.............: 16/09/2005 15:59:32
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_valida_conta_pai_recursivo:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_cta_ctbl_pai_inicial
        as character
        format "x(20)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************** Buffer Definition Begin *************************/

    &if "{&emsbas_version}" >= "1.00" &then
    def buffer b_dwb_rpt_select_pai
        for dwb_rpt_select.
    &endif


    /*************************** Buffer Definition End **************************/

    find first b_dwb_rpt_select_pai no-lock
        where b_dwb_rpt_select_pai.cod_dwb_program  = v_cod_dwb_program
          and b_dwb_rpt_select_pai.cod_dwb_user     = v_cod_dwb_user
          and b_dwb_rpt_select_pai.cod_dwb_field    = "Conta Cont†bil" /*l_conta_contabil*/ 
          and b_dwb_rpt_select_pai.log_dwb_rule     = yes
          and b_dwb_rpt_select_pai.cod_dwb_initial >= p_cod_cta_ctbl_pai_inicial
          and b_dwb_rpt_select_pai.cod_dwb_final   <= p_cod_cta_ctbl_pai_inicial
        no-error.
    if  avail b_dwb_rpt_select_pai
    then do:
        /* Faixa de &1 irregular ! */
        run pi_messages (input "show",
                         input 3683,
                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                           "Conta Cont†bil" /*l_conta_contabil*/)) /*msg_3683*/.
        return "NOK" /*l_nok*/ .
    end /* if */.
    else do:
        find first estrut_cta_ctbl no-lock
            where estrut_cta_ctbl.cod_plano_cta_ctbl = plano_cta_ctbl.cod_plano_cta_ctbl
              and estrut_cta_ctbl.cod_cta_ctbl_filho = p_cod_cta_ctbl_pai_inicial
            no-error.
        if  avail estrut_cta_ctbl and
            estrut_cta_ctbl.cod_cta_ctbl_pai <> ""
        then do:
            run pi_valida_conta_pai_recursivo (Input estrut_cta_ctbl.cod_cta_ctbl_pai) /*pi_valida_conta_pai_recursivo*/.
        end /* if */.    
    end /* else */.
    return "OK" /*l_ok*/ .
END PROCEDURE. /* pi_valida_conta_pai_recursivo */


/************************** Internal Procedure End **************************/

/************************* External Procedure Begin *************************/



/************************** External Procedure End **************************/

/*************************************  *************************************/
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
                    /*else
                        case p_stream:
                            when "s_1" then
                                if c_at[i_ind] = "at" then
                                    put stream s_1 unformatted c_aux at i_pos[i_ind].
                                else
                                    put stream s_1 unformatted c_aux to i_pos[i_ind].
                        end.*/
            end.
        end.
        /*case p_stream:
        when "s_1" then
            put stream s_1 unformatted skip.
        end.*/
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
                "Programa Mensagem" c_prg_msg "N∆o encontrado."
                view-as alert-box error.
        return error.
    end.

    run value(c_prg_msg + ".p":U) (input c_action, input c_param).
    return return-value.
END PROCEDURE.  /* pi_messages */
/*********************  End of rpt_sdo_cta_ctbl_balanct *********************/


OUTPUT STREAM s_1 CLOSE.



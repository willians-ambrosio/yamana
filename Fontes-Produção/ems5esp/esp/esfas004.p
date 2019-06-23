/*****************************************************************************
** Copyright DATASUL S.A. (1994)
** Todos os Direitos Reservados.
** 
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so' podera ser feita mediante
** autorizacao expressa.
**
** Programa..............: esfas004
** Descricao.............: Relat¢rio Situaá∆o Geral Pat Excel
** Versao................:  1.00.01.002
** Procedimento..........: esfas004
** Nome Externo..........: esp/esfas004.p
** Data Geracao..........: 15/08/2014
** Criado por............: Bruno Bertulli (DSC)
** Criado em.............: 
** Alterado por..........: 
** Alterado em...........: 
** Gerado por............: 
*****************************************************************************/

def var chExcelApplication AS COM-HANDLE NO-UNDO.
def var chWorkBook         AS COM-HANDLE NO-UNDO.
def var chWorkSheet        AS COM-HANDLE NO-UNDO.
def var i-linha            AS INT        NO-UNDO.
def var cArquivo           AS CHAR       NO-UNDO.

def var c-versao-prg as char initial " 1.00.01.002":U no-undo.

{include/i_dbinst.i}
{include/i_dbtype.i}
{include/i_fcldef.i}


&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i esfas004 FAS}
&ENDIF

/******************************* Private-Data *******************************/
assign this-procedure:private-data = "HLP=2":U.
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
                                    "ESFAS004","~~EMSFIN", "~~{~&emsfin_version}", "~~1.00")) /*msg_5009*/.
&else

/********************* Temporary Table Definition Begin *********************/

def temp-table tt_cta_pat_sit_geral no-undo
    field tta_cod_empresa                  as character format "x(3)" label "Empresa" column-label "Empresa"
    field tta_cod_cta_pat                  as character format "x(18)" label "Conta Patrimonial" column-label "Conta Patrimonial"
    field tta_cod_grp_calc                 as character format "x(6)" label "Grupo C†lculo" column-label "Grupo C†lculo"
    field tta_log_control_ctbl             as logical format "Sim/N∆o" initial no label "Controle Cont†bil" column-label "Controle Cont†bil"
    .

def temp-table tt_rpt_bem_pat no-undo like bem_pat
    field ttv_rec_bem_pat                  as recid format ">>>>>>9" initial ?
    field ttv_cod_dwb_field_rpt            as character extent 13 format "x(32)" label "Conjunto" column-label "Conjunto"
    field ttv_val_pct_dpr                  as decimal format ">>>>9.99" decimals 2 initial 0 label "% Depreciaá∆o" column-label "% Depreciaá∆o"
    field ttv_val_pct_dpr_incevda          as decimal format ">>>>9.99" decimals 2 initial 0 label "% Deprec Incentivada" column-label "% Deprec Incentivada"
    field tta_dat_calc_pat                 as date format "99/99/9999" initial today label "Data C†lculo" column-label "Data C†lculo"
    field tta_log_det_agro                 as logical format "Sim/N∆o" initial no label "Envia PIMS Detalhado" column-label "PIMS Det"
    index tt_cod_grp_calc                 
          cod_grp_calc                     ascending
    index tt_rpt_bem_pat_id                is primary unique
          ttv_rec_bem_pat                  ascending
    index tt_rpt_bem_pat_seq               is unique
          num_id_bem_pat                   ascending
    index tt_rpt_cod_ccusto_respons       
          cod_empresa                      ascending
          cod_ccusto_respons               ascending
    index tt_rpt_cod_cta_pat              
          cod_empresa                      ascending
          cod_cta_pat                      ascending
    index tt_rpt_cod_espec                
          cod_empresa                      ascending
          cod_espec_bem                    ascending
    index tt_rpt_cod_especif_tec          
          cod_empresa                      ascending
          cod_especif_tec                  ascending
    index tt_rpt_cod_estab                
          cod_empresa                      ascending
          cod_estab                        ascending
    index tt_rpt_cod_licenc_uso           
          cod_empresa                      ascending
          cod_licenc_uso                   ascending
    index tt_rpt_cod_marca                
          cod_empresa                      ascending
          cod_marca                        ascending
    index tt_rpt_cod_modelo               
          cod_empresa                      ascending
          cod_modelo                       ascending
    index tt_rpt_cod_plano_ccusto         
          cod_empresa                      ascending
          cod_plano_ccusto                 ascending
    index tt_rpt_cod_unid_negoc           
          cod_empresa                      ascending
          cod_unid_negoc                   ascending
    index tt_rpt_dat_aquis                
          cod_empresa                      ascending
          dat_aquis_bem_pat                ascending
    index tt_rpt_dat_calc                 
          cod_empresa                      ascending
          dat_calc_pat                     ascending
    index tt_rpt_des_bem_pat              
          des_bem_pat                      ascending
    index tt_rpt_num_bem_pat              
          num_bem_pat                      ascending
          cod_empresa                      ascending
    .



/********************** Temporary Table Definition End **********************/

/************************** Buffer Definition Begin *************************/

def buffer ccusto               for ems5.ccusto.
def buffer empresa              for ems5.empresa.
def buffer histor_exec_especial for ems5.histor_exec_especial.
def buffer unid_negoc           for ems5.unid_negoc.

def buffer b-cta_pat for cta_pat.

&if "{&emsfin_version}" >= "1.00" &then
def buffer b_cta_pat
    for cta_pat.
&endif
&if "{&emsfin_version}" >= "1.00" &then
def buffer b_movto_bem_pat
    for movto_bem_pat.
&endif
&if "{&emsfin_version}" >= "1.00" &then
def buffer b_movto_bem_pat_transf
    for movto_bem_pat.
&endif
&if "{&emsbas_version}" >= "1.00" &then
def buffer b_ped_exec_style
    for ped_exec.
&endif
&if "{&emsfin_version}" >= "1.00" &then
def buffer US_PARAM_calc_bem_pat
    for PARAM_calc_bem_pat.
def buffer US_sdo_bem_pat
    for sdo_bem_pat.
def buffer USb_sdo_bem_pat
    for sdo_bem_pat.
def buffer b_sdo_bem_pat
    for sdo_bem_pat.
&endif
&if "{&emsfin_version}" >= "1.00" &then
def buffer b_sdo_bem_pat_vo
    for sdo_bem_pat.
def buffer USb_sdo_bem_pat_vo
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

def new global shared var v_cod_aplicat_dtsul_corren
    as character
    format "x(3)":U
    no-undo.
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07" &THEN
def var v_cod_ccusto
    as Character
    format "x(11)":U
    label "Centro Custo"
    column-label "Centro Custo"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.07" AND "{&emsfin_version}" < "9.99" &THEN
def var v_cod_ccusto
    as character
    format "x(20)":U
    label "Centro de Custo"
    column-label "CCusto"
    no-undo.
&ENDIF
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
def var v_cod_cta_pat
    as character
    format "x(18)":U
    label "Conta Patrimonial"
    column-label "Conta Patrimonial"
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
    label "Classificaá∆o"
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
def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
def var v_cod_estab
    as character
    format "x(3)":U
    label "Estabelecimento"
    column-label "Estabelecimento"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
def var v_cod_estab
    as Character
    format "x(5)":U
    label "Estabelecimento"
    column-label "Estabelecimento"
    no-undo.
&ENDIF
def new global shared var v_cod_estab_usuar
    as character
    format "x(3)":U
    label "Estabelecimento"
    column-label "Estab"
    no-undo.
def new shared var v_cod_final
    as character
    format "x(8)":U
    initial ?
    label "Final"
    no-undo.
def var v_cod_finalid_econ
    as character
    format "x(10)":U
    label "Finalidade Econìmica"
    column-label "Finalidade Econìmica"
    no-undo.
def new shared var v_cod_format
    as character
    format "x(8)":U
    label "Formato"
    column-label "Formato"
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
def var v_cod_indic_econ
    as character
    format "x(8)":U
    label "Moeda"
    column-label "Moeda"
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
def var v_cod_order
    as character
    format "x(40)":U
    no-undo.
def new global shared var v_cod_pais_empres_usuar
    as character
    format "x(3)":U
    label "Pa°s Empresa Usu†rio"
    column-label "Pa°s"
    no-undo.
def var v_cod_plano_ccusto
    as character
    format "x(8)":U
    label "Plano CCusto"
    column-label "Plano CCusto"
    no-undo.
def new global shared var v_cod_plano_ccusto_corren
    as character
    format "x(8)":U
    label "Plano CCusto"
    column-label "Plano CCusto"
    no-undo.
def new shared var v_cod_release
    as character
    format "x(12)":U
    no-undo.
def var v_cod_ult_obj_procesdo
    as character
    format "x(32)":U
    no-undo.
def var v_cod_unid_negoc
    as character
    format "x(3)":U
    label "Unid Neg¢cio"
    column-label "Un Neg"
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
def var v_dat_aquis_bem_pat
    as date
    format "99/99/9999":U
    label "Data Aquisiá∆o"
    column-label "Data Aquisiá∆o"
    no-undo.
def var v_dat_calc_pat
    as date
    format "99/99/9999":U
    label "Data C†lculo"
    column-label "Data C†lculo"
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
def new shared var v_dat_inic_period
    as date
    format "99/99/9999":U
    label "In°cio Per°odo"
    column-label "Per°odo"
    no-undo.
def var v_des_tot_grp_relat
    as character
    format "x(50)":U
    no-undo.
def var v_des_val_amort_aux
    as character
    format "x(23)":U
    label "Valor Amortizado"
    column-label "Valor Amortizado"
    no-undo.
def var v_des_val_US_amort_aux
    as character
    format "x(23)":U
    label "Valor Amortizado"
    column-label "Valor Amortizado"
    no-undo.
def var v_des_val_dpr_aux
    as character
    format "x(23)":U
    label "Valor Depreciado"
    column-label "Valor Depreciado"
    no-undo.
def var v_des_val_US_dpr_aux
    as character
    format "x(23)":U
    label "Valor Depreciado"
    column-label "Valor Depreciado"
    no-undo.
def var v_des_val_imobdo_orig
    as character
    format "x(23)":U
    label "Valor Imobilizado"
    column-label "Valor Imobilizado"
    no-undo.
def var v_des_val_US_imobdo_orig
    as character
    format "x(23)":U
    label "Valor Imobilizado"
    column-label "Valor Imobilizado"
    no-undo.
def var v_des_val_liq_bem_pat_aux
    as character
    format "x(23)":U
    label "Valor Liquido"
    column-label "Valor Liquido"
    no-undo.
def var v_des_val_tot_amort_aux
    as character
    format "x(23)":U
    label "Total Amortizado"
    column-label "Total Amortizado"
    no-undo.
def var v_des_val_tot_dpr_aux
    as character
    format "x(23)":U
    label "Total Depreciado"
    column-label "Total Depreciado"
    no-undo.
def var v_des_val_tot_geral_amort_aux
    as character
    format "x(23)":U
    label "Geral Amortizado"
    column-label "Geral Amortizado"
    no-undo.
def var v_des_val_tot_geral_dpr_aux
    as character
    format "x(23)":U
    label "Geral Depreciado"
    column-label "Geral Depreciado"
    no-undo.
def var v_des_val_tot_geral_imobdo_aux
    as character
    format "x(23)":U
    label "Geral Imobilizado"
    column-label "Geral Imobilizado"
    no-undo.
def var v_des_val_tot_geral_liq_bem_pat
    as character
    format "x(23)":U
    label "Geral Liquido Bem"
    column-label "Geral Liquido Bem"
    no-undo.
def var v_des_val_tot_imobdo_aux
    as character
    format "x(23)":U
    label "Total Imobilizado"
    column-label "Total Imobilizado"
    no-undo.
def var v_des_val_tot_liq_bem_pat_aux
    as character
    format "x(23)":U
    label "Total Liquido Bem"
    column-label "Total Liquido Bem"
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
    radio-buttons "On-Line", "On-Line", "Batch", "Batch"
     /*l_online*/ /*l_online*/ /*l_batch*/ /*l_batch*/
    bgcolor 8 
    label "Run Mode"
    column-label "Run Mode"
    no-undo.
def var v_log_aux
    as logical
    format "Sim/N∆o"
    initial yes
    no-undo.
def var v_log_bem_imobdo
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    label "Bens n∆o Imobilizado"
    no-undo.
def var v_log_cabec_relat
    as logical
    format "Sim/N∆o"
    initial yes
    no-undo.
def var v_log_consid_bem_bxado
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    label "Considera Bem Bxado"
    no-undo.
def var v_log_cta_zero
    as logical
    format "Sim/N∆o"
    initial no
    no-undo.
def new global shared var v_log_execution
    as logical
    format "Sim/N∆o"
    initial yes
    no-undo.
def var v_log_impr_cabec
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    no-undo.
def var v_log_impr_narrat
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    label "Imprime Narrativa"
    no-undo.
def var v_log_impr_relat
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
def var v_log_quant_bem
    as logical
    format "Sim/N∆o"
    initial no
    no-undo.
def var v_log_resum_cta
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    label "Resumido por Conta"
    no-undo.
def var v_log_rodap_relat
    as logical
    format "Sim/N∆o"
    initial yes
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
def var v_num_bem_pat
    as integer
    format ">>>>>>>>9":U
    initial 1
    label "Bem Patrimonial"
    column-label "Bem Pat"
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
def var v_num_dec
    as integer
    format "9":U
    initial 2
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
def var v_num_seq_incorp_bem_pat
    as integer
    format ">>>>,>>9":U
    initial 0
    label "Seq Incorp Bem"
    column-label "Seq Incorp Bem"
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
def var v_qtd_tot_bem_pat
    as decimal
    format ">>>>>>>>9":U
    decimals 0
    label "Total Quantidade"
    column-label "Total Quantidade"
    no-undo.
def var v_qtd_tot_geral_bem_pat
    as decimal
    format ">>>>>>>>9":U
    decimals 0
    label "Total Quantidade"
    column-label "Total Quantidade"
    no-undo.
def new global shared var v_rec_bem_pat
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.
def new global shared var v_rec_cenar_ctbl
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.
def new shared var v_rec_dwb_rpt_select
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_finalid_econ
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
def var v_val_amort
    as decimal
    format "->>>,>>>,>>>,>>9.99":U
    decimals 2
    initial 0
    label "Valor Amortizaá∆o"
    column-label "Valor Amortizado"
    no-undo.
def var v_val_amort_aux
    as decimal
    format "->>>,>>>,>>>,>>9.999999":U
    decimals 6
    label "Valor Amortizaá∆o"
    column-label "Valor Amortizaá∆o"
    no-undo.
def var v_val_US_amort_aux
    as decimal
    format "->>>,>>>,>>>,>>9.999999":U
    decimals 6
    label "Valor Amortizaá∆o"
    column-label "Valor Amortizaá∆o"
    no-undo.
def var v_val_calc
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    no-undo.
def var v_val_cm
    as decimal
    format "->>>>>,>>>,>>9.99":U
    decimals 4
    label "Correá∆o Monet†ria"
    column-label "Correá∆o Monet†ria"
    no-undo.
def var v_val_dpr
    as decimal
    format "->>>,>>>,>>>,>>9.99":U
    decimals 2
    initial 0
    label "Valor Depreciado"
    column-label "Valor Depreciado"
    no-undo.
def var v_val_dpr_aux
    as decimal
    format "->>>,>>>,>>>,>>9.999999":U
    decimals 6
    label "Valor Depreciado"
    column-label "Valor Depreciado"
    no-undo.
def var v_val_US_dpr_aux
    as decimal
    format "->>>,>>>,>>>,>>9.999999":U
    decimals 6
    label "Valor Depreciado"
    column-label "Valor Depreciado"
    no-undo.
def var v_val_imobdo
    as decimal
    format "->>>,>>>,>>>,>>9.99":U
    decimals 2
    initial 0
    label "Valor Imobilizado"
    column-label "Valor Imobilizado"
    no-undo.
def var v_val_imobdo_orig
    as decimal
    format "->>>,>>>,>>>,>>9.999999":U
    decimals 6
    label "Valor Imobilizado"
    column-label "Valor Imobilizado"
    no-undo.
def var v_val_US_imobdo_orig
    as decimal
    format "->>>,>>>,>>>,>>9.999999":U
    decimals 6
    label "Valor Imobilizado"
    column-label "Valor Imobilizado"
    no-undo.
def var v_val_liq_bem_pat
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Valor L°quido Dpr"
    column-label "Valor L°quido Dpr"
    no-undo.
def var v_val_liq_bem_pat_amort
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Valor L°quido Amort"
    column-label "Valor L°quido Amort"
    no-undo.
def var v_val_liq_bem_pat_aux
    as decimal
    format "->>,>>>,>>>,>>9.999999":U
    decimals 6
    label "Valor L°quido"
    column-label "Valor L°quido"
    no-undo.
def var v_val_original
    as decimal
    format "->>>>>,>>>,>>9.99":U
    decimals 4
    initial 0
    label "Valor Original"
    column-label "Valor Original"
    no-undo.
def var v_val_tot_amort
    as decimal
    format "->>>>,>>>,>>>,>>9.99":U
    decimals 2
    initial 0
    label "Total Amortizaá∆o"
    column-label "Total Amortizado"
    no-undo.
def var v_val_tot_amort_aux
    as decimal
    format "->>>>,>>>,>>>,>>9.999999":U
    decimals 6
    label "Total Amortizado"
    column-label "Total Amortizado"
    no-undo.
def var v_val_tot_cm
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Valor Total CM"
    column-label "Valor Total CM"
    no-undo.
def var v_val_tot_dpr
    as decimal
    format "->>>,>>>,>>>,>>9.99":U
    decimals 2
    initial 0
    label "Total Depreciado"
    column-label "Total Depreciado"
    no-undo.
def var v_val_tot_dpr_aux
    as decimal
    format "->>>,>>>,>>>,>>9.999999":U
    decimals 6
    label "Total Depreciado"
    column-label "Total Depreciado"
    no-undo.
def var v_val_tot_geral_amort
    as decimal
    format "->>>>,>>>,>>>,>>9.99":U
    decimals 2
    initial 0
    label "Total Geral Amortiz"
    column-label "Total Geral Amortiz"
    no-undo.
def var v_val_tot_geral_amort_aux
    as decimal
    format "->>>>,>>>,>>>,>>9.999999":U
    decimals 6
    label "Geral Amortizaá∆o"
    column-label "Geral Amortizaá∆o"
    no-undo.
def var v_val_tot_geral_cm
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Valor Geral CM"
    column-label "Valor Geral CM"
    no-undo.
def var v_val_tot_geral_dpr
    as decimal
    format "->>>>,>>>,>>>,>>9.99":U
    decimals 2
    initial 0
    label "Geral Deprecido"
    column-label "Geral Deprecido"
    no-undo.
def var v_val_tot_geral_dpr_aux
    as decimal
    format "->>>>,>>>,>>>,>>9.999999":U
    decimals 6
    label "Geral Depreciado"
    column-label "Geral Depreciado"
    no-undo.
def var v_val_tot_geral_imobdo
    as decimal
    format "->>>,>>>,>>>,>>9.99":U
    decimals 2
    initial 0
    label "Geral Imobilizado"
    column-label "Geral Imobilizado"
    no-undo.
def var v_val_tot_geral_imobdo_aux
    as decimal
    format "->>>,>>>,>>>,>>9.999999":U
    decimals 6
    label "Geral Imobilizado"
    column-label "Geral Imobilizado"
    no-undo.
def var v_val_tot_geral_liq_bem_amort
    as decimal
    format "->>>>,>>>,>>>,>>9.99":U
    decimals 2
    label "Geral L°quido Bem"
    column-label "Geral L°quido Bem"
    no-undo.
def var v_val_tot_geral_liq_bem_pat
    as decimal
    format "->>>>,>>>,>>>,>>9.99":U
    decimals 2
    label "Geral L°quido Bem"
    column-label "Geral L°quido Bem"
    no-undo.
def var v_val_tot_geral_liq_bem_pat_aux
    as decimal
    format "->>>>,>>>,>>>,>>9.999999":U
    decimals 6
    label "Geral L°quido Bem"
    column-label "Geral L°quido Bem"
    no-undo.
def var v_val_tot_geral_origin
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Total Geral Original"
    column-label "Total Geral Original"
    no-undo.
def var v_val_tot_geral_origin_aux
    as decimal
    format "->>,>>>,>>>,>>9.9999":U
    decimals 4
    label "Total Geral Original"
    no-undo.
def var v_val_tot_imobdo
    as decimal
    format "->>>,>>>,>>>,>>9.99":U
    decimals 2
    initial 0
    label "Total Imobilizado"
    column-label "Total Imobilizado"
    no-undo.
def var v_val_tot_imobdo_aux
    as decimal
    format "->>>,>>>,>>>,>>9.999999":U
    decimals 6
    label "Total Imobilizado"
    column-label "Total Imobilizado"
    no-undo.
def var v_val_tot_liq_bem_pat
    as decimal
    format "->>>,>>>,>>>,>>9.99":U
    decimals 2
    label "Total L°quido Bem"
    column-label "Total L°quido Bem"
    no-undo.
def var v_val_tot_liq_bem_pat_amort
    as decimal
    format "->>>,>>>,>>>,>>9.99":U
    decimals 2
    label "Total L°quido Bem"
    column-label "Total L°quido Bem"
    no-undo.
def var v_val_tot_liq_bem_pat_aux
    as decimal
    format "->>>,>>>,>>>,>>9.999999":U
    decimals 6
    label "Total L°quido Bem"
    column-label "Total L°quido Bem"
    no-undo.
def var v_val_tot_orig
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Total Valor Original"
    column-label "Total Valor Original"
    no-undo.
def var v_wgh_focus
    as widget-handle
    format ">>>>>>9":U
    no-undo.
def var v_wgh_frame_epc
    as widget-handle
    format ">>>>>>9":U
    no-undo.
def var v_num_entries                    as integer         no-undo. /*local*/


/************************** Variable Definition End *************************/

/*************************** Menu Definition Begin **************************/

.

def menu      m_help                menubar
    menu-item mi_conteudo           label "&Conte£do"
    menu-item mi_sobre              label "&Sobre".



/**************************** Menu Definition End ***************************/

/************************** Query Definition Begin **************************/

def query qr_dwb_rpt_select
    for dwb_rpt_select
    scrolling.


/*************************** Query Definition End ***************************/

/************************** Browse Definition Begin *************************/

def browse br_dwb_rpt_select query qr_dwb_rpt_select display 
    if dwb_rpt_select.log_dwb_rule then "Regra" else "Exceá∆o" format "x(8)" column-label "Tipo"
    dwb_rpt_select.cod_dwb_field
    width-chars 32.00
        column-label "Conjunto"
    dwb_rpt_select.cod_dwb_initial
    width-chars 40.00
        column-label "Inicial"
    dwb_rpt_select.cod_dwb_final
    width-chars 40.00
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
def button bt_close
    label "&Fecha"
    tooltip "Fecha"
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
def button bt_print
    label "&Imprime"
    tooltip "Imprime"
    size 1 by 1
    auto-go.
def button bt_rml1
    label "Ret"
    tooltip "Retira Linha"
&if "{&window-system}" <> "TTY" &then
    image-up file "image/im-rml"
    image-insensitive file "image/ii-rml"
&endif
    size 1 by 1.
def button bt_set_printer
    label "Define Impressora e Layout"
    tooltip "Define Impressora e Layout de Impress∆o"
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
def button bt_zoo_87086
    label "Zoom"
    tooltip "Zoom"
&if "{&window-system}" <> "TTY" &then
    image-up file "image/im-zoo"
    image-insensitive file "image/ii-zoo"
&endif
    size 4 by .88.
def button bt_zoo_87087
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
    radio-buttons "Terminal", "Terminal", "Arquivo", "Arquivo", "Impressora", "Impressora", "Excel", "Excel"
     /*l_terminal*/ /*l_terminal*/ /*l_file*/ /*l_file*/ /*l_printer*/ /*l_printer*/
    bgcolor 8 
    no-undo.
def var rs_ind_run_mode
    as character
    initial "On-Line"
    view-as radio-set Horizontal
    radio-buttons "On-Line", "On-Line", "Batch", "Batch"
     /*l_online*/ /*l_online*/ /*l_batch*/ /*l_batch*/
    bgcolor 8 
    no-undo.


/************************* Radio-Set Definition End *************************/

/************************** Report Definition Begin *************************/

def new shared var v_rpt_s_1_lines as integer initial 66.
def new shared var v_rpt_s_1_columns as integer initial 255.
def new shared var v_rpt_s_1_bottom as integer initial 65.
def new shared var v_rpt_s_1_page as integer.
def new shared var v_rpt_s_1_name as character initial "Situaá∆o Geral Patrimìnio".
def frame f_rpt_s_1_header_period header
    "-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" at 1
    "P†gina:" at 243
    (page-number (s_1) + v_rpt_s_1_page) at 250 format ">>>>>9" skip
    v_nom_enterprise at 1 format "x(40)"
    v_nom_report_title at 216 format "x(40)" skip
    "Per°odo " at 1
    "atÇ:" at 9 
    v_dat_fim_period at 15 format "99/99/9999"
    "--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" at 25
    v_dat_execution at 238 format "99/99/9999" "- "
    v_hra_execution at 251 format "99:99" skip (1)
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_header_unique header
    "-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" at 1
    'P†gina:' at 243
    (page-number (s_1) + v_rpt_s_1_page) at 250 format '>>>>>9' skip
    v_nom_enterprise at 1 format 'x(40)'
    v_nom_report_title at 216 format 'x(40)' skip
    '--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------' at 1
    v_dat_execution at 238 format '99/99/9999' '- '
    v_hra_execution at 251 format "99:99" skip (1)
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_footer_last_page header
    "Èltima p†gina " at 1
    "-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" at 15
    v_nom_prog_ext at 233 format "x(08)" "- "
    v_cod_release at 244 format "x(12)" skip
    with no-box no-labels width 255 page-bottom stream-io.
def frame f_rpt_s_1_footer_normal header
    "--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" at 1
    "- " at 231
    v_nom_prog_ext at 233 format "x(08)" "- "
    v_cod_release at 244 format "x(12)" skip
    with no-box no-labels width 255 page-bottom stream-io.
def frame f_rpt_s_1_footer_param_page header
    "P†gina ParÉmetros " at 1
    "---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" at 19
    v_nom_prog_ext at 233 format "x(08)" "- "
    v_cod_release at 244 format "x(12)" skip
    with no-box no-labels width 255 page-bottom stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_bem header
    "Bem Pat" to 9
    "Seq" to 15
    "Conta Pat" at 17
    "Descriá∆o" at 36
    "Data Aquis" at 77
    "Data Calc" at 88
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Est" at 99
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Est" at 99
&ENDIF
    "Un N" at 105
    "Data Saldo" at 110
    "Plano CCusto" at 121
    "CC Respons" at 134
    "Valor Imobilizado" at 148
    "Valor Depreciado" at 172
    "Valor Amortizado" at 197
    "Valor Liquido" at 222
    "Quantidade" to 255 skip
    "---------" to 9
    "-----" to 15
    "------------------" at 17
    "----------------------------------------" at 36
    "----------" at 77
    "----------" at 88
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "---" at 99
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "-----" at 99
&ENDIF
    "----" at 105
    "----------" at 110
    "------------" at 121
    "-----------" at 134
    "-----------------------" at 148
    "-----------------------" at 172
    "-----------------------" at 197
    "-----------------------" at 222
    "----------" to 255 skip
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_bem_narr header
    "Bem Pat" to 9
    "Seq" to 15
    "Conta Pat" at 17
    "Descriá∆o" at 36
    "Data Aquis" at 77
    "Data Calc" at 88
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Est" at 99
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Est" at 99
&ENDIF
    "Un Neg" at 105
    "Data Saldo" at 112
    "Plano CCusto" at 123
    "CC Respons" at 136
    "Valor Imobilizado" at 148
    "Valor Depreciado" at 172
    "Valor Amortizado" at 197
    "Valor Liquido" at 222
    "Quantidade" to 255 skip skip
    "---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" at 1 skip
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_CCusto_Narr header
    "Conta Pat" at 1
    "Bem Pat" to 28
    "Seq" to 34
    "Descriá∆o" at 36
    "Data Aquis" at 77
    "Data Calc" at 88
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Est" at 99
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Est" at 99
&ENDIF
    "Un Neg" at 105
    "Data Saldo" at 112
    "Plano CCusto" at 123
    "Valor Imobilizado" at 148
    "Valor Depreciado" at 172
    "Valor Amortizado" at 197
    "Valor Liquido" at 222
    "Quantidade" to 255 skip skip
    "---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" at 1 skip
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_CCusto_Resp header
    "Conta Pat" at 1
    "Bem Pat" to 28
    "Seq" to 34
    "Descriá∆o" at 36
    "Data Aquis" at 77
    "Data Calc" at 88
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Est" at 99
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Est" at 99
&ENDIF
    "Un Neg" at 105
    "Data Saldo" at 112
    "Plano CCusto" at 123
    "Valor Imobilizado" at 148
    "Valor Depreciado" at 172
    "Valor Amortizado" at 197
    "Valor Liquido" at 222
    "Quantidade" to 255 skip
    "------------------" at 1
    "---------" to 28
    "-----" to 34
    "----------------------------------------" at 36
    "----------" at 77
    "----------" at 88
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "---" at 99
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "-----" at 99
&ENDIF
    "------" at 105
    "----------" at 112
    "------------" at 123
    "-----------------------" at 148
    "-----------------------" at 172
    "-----------------------" at 197
    "-----------------------" at 222
    "----------" to 255 skip
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_conta header
    "Bem Pat" to 9
    "Seq" to 15
    "Descriá∆o" at 17
    "Data Aquis" at 58
    "Data Calc" at 69
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Est" at 80
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Est" at 80
&ENDIF
    "Un Neg" at 86
    "Data Saldo" at 93
    "Plano CCusto" at 104
    "CC Respons" at 117
    "Valor Imobilizado" at 148
    "Valor Depreciado" at 172
    "Valor Amortizado" at 197
    "Valor Liquido" at 222
    "Quantidade" to 255 skip
    "---------" to 9
    "-----" to 15
    "----------------------------------------" at 17
    "----------" at 58
    "----------" at 69
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "---" at 80
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "-----" at 80
&ENDIF
    "------" at 86
    "----------" at 93
    "------------" at 104
    "-----------" at 117
    "-----------------------" at 148
    "-----------------------" at 172
    "-----------------------" at 197
    "-----------------------" at 222
    "----------" to 255 skip
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_conta_narr header
    "Bem Pat" to 9
    "Seq" to 15
    "Descriá∆o" at 17
    "Data Aquis" at 58
    "Data Calc" at 69
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Est" at 80
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Est" at 80
&ENDIF
    "Un Neg" at 86
    "Data Saldo" at 93
    "Plano CCusto" at 104
    "CC Respons" at 117
    "Valor Imobilizado" at 148
    "Valor Depreciado" at 172
    "Valor Amortizado" at 197
    "Valor Liquido" at 222
    "Quantidade" to 255 skip skip
    "---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" at 1 skip
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_conta_zero header
    "Valor Imobilizado" at 148
    "Valor Depreciado" at 172
    "Valor Amortizado" at 197
    "Valor Liquido" at 222
    "Quantidade" to 255 skip
    "-----------------------" at 148
    "-----------------------" at 172
    "-----------------------" at 197
    "-----------------------" at 222
    "----------" to 255 skip
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_data header
    "Data Aquis" at 1
    "Conta Pat" at 12
    "Bem Pat" to 39
    "Seq" to 45
    "Descriá∆o" at 47
    "Data Calc" at 88
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Est" at 99
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Est" at 99
&ENDIF
    "Un Neg" at 105
    "Data Saldo" at 112
    "Plano CCusto" at 123
    "CC Respons" at 136
    "Valor Imobilizado" at 148
    "Valor Depreciado" at 172
    "Valor Amortizado" at 197
    "Valor Liquido" at 222
    "Quantidade" to 255 skip
    "----------" at 1
    "------------------" at 12
    "---------" to 39
    "-----" to 45
    "----------------------------------------" at 47
    "----------" at 88
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "---" at 99
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "-----" at 99
&ENDIF
    "------" at 105
    "----------" at 112
    "------------" at 123
    "-----------" at 136
    "-----------------------" at 148
    "-----------------------" at 172
    "-----------------------" at 197
    "-----------------------" at 222
    "----------" to 255 skip
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_datas_narr header
    "Data Aquis" at 1
    "Conta Pat" at 12
    "Bem Pat" to 39
    "Seq" to 45
    "Descriá∆o" at 47
    "Data Calc" at 88
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Est" at 99
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Est" at 99
&ENDIF
    "Un Neg" at 105
    "Data Saldo" at 112
    "Plano CCusto" at 123
    "CC Respons" at 136
    "Valor Imobilizado" at 148
    "Valor Depreciado" at 172
    "Valor Amortizado" at 197
    "Valor Liquido" at 222
    "Quantidade" to 255 skip skip
    "---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" at 1 skip
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_data_calc header
    "Data Calc" at 1
    "Conta Pat" at 12
    "Bem Pat" to 39
    "Seq" to 45
    "Descriá∆o" at 47
    "Data Aquis" at 88
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Est" at 99
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Est" at 99
&ENDIF
    "Un Neg" at 105
    "Data Saldo" at 112
    "Plano CCusto" at 123
    "CC Respons" at 136
    "Valor Imobilizado" at 148
    "Valor Depreciado" at 172
    "Valor Amortizado" at 197
    "Valor Liquido" at 222
    "Quantidade" to 255 skip
    "----------" at 1
    "------------------" at 12
    "---------" to 39
    "-----" to 45
    "----------------------------------------" at 47
    "----------" at 88
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "---" at 99
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "-----" at 99
&ENDIF
    "------" at 105
    "----------" at 112
    "------------" at 123
    "-----------" at 136
    "-----------------------" at 148
    "-----------------------" at 172
    "-----------------------" at 197
    "-----------------------" at 222
    "----------" to 255 skip
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_data_calc_n header
    "Data Calc" at 1
    "Conta Pat" at 12
    "Bem Pat" to 39
    "Seq" to 45
    "Descriá∆o" at 47
    "Data Aquis" at 88
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Est" at 99
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Est" at 99
&ENDIF
    "Un Neg" at 105
    "Data Saldo" at 112
    "Plano CCusto" at 123
    "CC Respons" at 136
    "Valor Imobilizado" at 148
    "Valor Depreciado" at 172
    "Valor Amortizado" at 197
    "Valor Liquido" at 222
    "Quantidade" to 255 skip skip
    "---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" at 1 skip
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_descr header
    "Descriá∆o" at 1
    "Bem Pat" to 50
    "Seq" to 56
    "Conta Pat" at 58
    "Data Aquis" at 77
    "Data Calc" at 88
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Est" at 99
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Est" at 99
&ENDIF
    "Un Neg" at 105
    "Data Saldo" at 112
    "Plano CCusto" at 123
    "CC Respons" at 136
    "Valor Imobilizado" at 148
    "Valor Depreciado" at 172
    "Valor Amortizado" at 197
    "Valor Liquido" at 222
    "Quantidade" to 255 skip
    "----------------------------------------" at 1
    "---------" to 50
    "-----" to 56
    "------------------" at 58
    "----------" at 77
    "----------" at 88
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "---" at 99
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "-----" at 99
&ENDIF
    "------" at 105
    "----------" at 112
    "------------" at 123
    "-----------" at 136
    "-----------------------" at 148
    "-----------------------" at 172
    "-----------------------" at 197
    "-----------------------" at 222
    "----------" to 255 skip
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_descr_narr header
    "Descriá∆o" at 1
    "Bem Pat" to 50
    "Seq" to 56
    "Conta Pat" at 58
    "Data Aquis" at 77
    "Data Calc" at 88
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Est" at 99
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Est" at 99
&ENDIF
    "Un Neg" at 105
    "Data Saldo" at 112
    "Plano CCusto" at 123
    "CC Respons" at 136
    "Valor Imobilizado" at 148
    "Valor Depreciado" at 172
    "Valor Amortizado" at 197
    "Valor Liquido" at 222
    "Quantidade" to 255 skip skip
    "---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" at 1 skip
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_estab header
    "Conta Pat" at 1
    "Bem Pat" to 28
    "Seq" to 34
    "Descriá∆o" at 36
    "Data Aquis" at 77
    "Data Calc" at 88
    "Un Neg" at 99
    "Data Saldo" at 106
    "Plano CCusto" at 117
    "CC Respons" at 130
    "Valor Imobilizado" at 148
    "Valor Depreciado" at 172
    "Valor Amortizado" at 197
    "Valor Liquido" at 222
    "Quantidade" to 255 skip
    "------------------" at 1
    "---------" to 28
    "-----" to 34
    "----------------------------------------" at 36
    "----------" at 77
    "----------" at 88
    "------" at 99
    "----------" at 106
    "------------" at 117
    "-----------" at 130
    "-----------------------" at 148
    "-----------------------" at 172
    "-----------------------" at 197
    "-----------------------" at 222
    "----------" to 255 skip
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_estab_narr header
    "Conta Pat" at 1
    "Bem Pat" to 28
    "Seq" to 34
    "Descriá∆o" at 36
    "Data Aquis" at 77
    "Data Calc" at 88
    "Un Neg" at 99
    "Data Saldo" at 106
    "Plano CCusto" at 117
    "CC Respons" at 130
    "Valor Imobilizado" at 148
    "Valor Depreciado" at 172
    "Valor Amortizado" at 197
    "Valor Liquido" at 222
    "Quantidade" to 255 skip skip
    "---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" at 1 skip
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_Plano_CCusto header
    "Conta Pat" at 1
    "Bem Pat" to 28
    "Seq" to 34
    "Descriá∆o" at 36
    "Data Aquis" at 77
    "Data Calc" at 88
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Est" at 99
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Est" at 99
&ENDIF
    "Un Neg" at 105
    "Data Saldo" at 112
    "CC Respons" at 123
    "Valor Imobilizado" at 148
    "Valor Depreciado" at 172
    "Valor Amortizado" at 197
    "Valor Liquido" at 222
    "Quantidade" to 255 skip
    "------------------" at 1
    "---------" to 28
    "-----" to 34
    "----------------------------------------" at 36
    "----------" at 77
    "----------" at 88
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "---" at 99
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "-----" at 99
&ENDIF
    "------" at 105
    "----------" at 112
    "-----------" at 123
    "-----------------------" at 148
    "-----------------------" at 172
    "-----------------------" at 197
    "-----------------------" at 222
    "----------" to 255 skip
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_plano_ccus_n header
    "Conta Pat" at 1
    "Bem Pat" to 28
    "Seq" to 34
    "Descriá∆o" at 36
    "Data Aquis" at 77
    "Data Calc" at 88
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Est" at 99
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Est" at 99
&ENDIF
    "Un Neg" at 105
    "Data Saldo" at 112
    "CC Respons" at 123
    "Valor Imobilizado" at 148
    "Valor Depreciado" at 172
    "Valor Amortizado" at 197
    "Valor Liquido" at 222
    "Quantidade" to 255 skip skip
    "---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" at 1 skip
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_resum_cta header
    "Conta Patrimonial" at 3
    "Descriá∆o" at 23
    "Total Imobilizado" at 57
    "Total Depreciado" at 82
    "Total Amortizado" at 107
    "Total Liquido Bem" at 132
    "Total Quant" to 167 skip
    "------------------" at 3
    "--------------------------------" at 23
    "-----------------------" at 57
    "-----------------------" at 82
    "-----------------------" at 107
    "-----------------------" at 132
    "-----------" to 167 skip
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_unid_negoc header
    "Conta Pat" at 1
    "Bem Pat" to 28
    "Seq" to 34
    "Descriá∆o" at 36
    "Data Aquis" at 77
    "Data Calc" at 88
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Est" at 99
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Est" at 99
&ENDIF
    "Data Saldo" at 105
    "Plano CCusto" at 116
    "CC Respons" at 129
    "Valor Imobilizado" at 148
    "Valor Depreciado" at 172
    "Valor Amortizado" at 197
    "Valor Liquido" at 222
    "Quantidade" to 255 skip
    "------------------" at 1
    "---------" to 28
    "-----" to 34
    "----------------------------------------" at 36
    "----------" at 77
    "----------" at 88
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "---" at 99
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "-----" at 99
&ENDIF
    "----------" at 105
    "------------" at 116
    "-----------" at 129
    "-----------------------" at 148
    "-----------------------" at 172
    "-----------------------" at 197
    "-----------------------" at 222
    "----------" to 255 skip
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_unid_neg_nar header
    "Conta Pat" at 1
    "Bem Pat" to 28
    "Seq" to 34
    "Descriá∆o" at 36
    "Data Aquis" at 77
    "Data Calc" at 88
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Est" at 99
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Est" at 99
&ENDIF
    "Data Saldo" at 105
    "Plano CCusto" at 116
    "CC Respons" at 129
    "Valor Imobilizado" at 148
    "Valor Depreciado" at 172
    "Valor Amortizado" at 197
    "Valor Liquido" at 222
    "Quantidade" to 255 skip skip
    "---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" at 1 skip
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_parametros_Lay_parametros header
    skip (1)
    "Cen†rio Cont†bil: " at 94
    v_cod_cenar_ctbl at 112 format "x(8)" view-as text skip
    "Finalidade Econìmica: " at 90
    v_cod_finalid_econ at 112 format "x(10)" view-as text
    /* Vari†vel v_dat_fim_period ignorada. N∆o esta definida no programa */
    skip (1)
    "Considera Bem Bxado: " at 91
    v_log_consid_bem_bxado at 112 format "Sim/N∆o" view-as text
    "Resumido por Conta: " at 122
    v_log_resum_cta at 142 format "Sim/N∆o" view-as text skip
    "Imprime Narrativa: " at 93
    v_log_impr_narrat at 112 format "Sim/N∆o" view-as text skip
    "Bens n∆o Imobilizado: " at 90
    v_log_bem_imobdo at 112 format "Sim/N∆o" view-as text skip
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_quebra_Lay_ccusto_resp header
    /* Atributo ccusto.cod_ccusto ignorado */
    "-" at 27
    /* Atributo ccusto.des_tit_ctbl ignorado */ skip (1)
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_quebra_Lay_conta header
    /* Atributo cta_pat.cod_cta_pat ignorado */
    "-" at 39
    /* Atributo cta_pat.des_cta_pat ignorado */ skip (1)
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_quebra_Lay_estab header
    /* Atributo estabelecimento.cod_estab ignorado */
    "-" at 24
    /* Atributo estabelecimento.nom_pessoa ignorado */ skip (1)
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_quebra_Lay_Plan_CCusto header
    /* Atributo plano_ccusto.cod_plano_ccusto ignorado */
    "-" at 31
    /* Atributo plano_ccusto.des_tit_ctbl ignorado */ skip (1)
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_quebra_Lay_unid_negoc header
    /* Atributo unid_negoc.cod_unid_negoc ignorado */
    "-" at 19
    /* Atributo unid_negoc.des_unid_negoc ignorado */ skip (1)
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_total_geral_Lay_total_geral header
    skip (1)
    "Total Geral" at 133
    ":" at 145
    v_des_val_tot_geral_imobdo_aux at 148 format "x(23)" view-as text
    v_des_val_tot_geral_dpr_aux at 172 format "x(23)" view-as text
    v_des_val_tot_geral_amort_aux at 197 format "x(23)" view-as text
    v_des_val_tot_geral_liq_bem_pat at 222 format "x(23)" view-as text
    v_qtd_tot_geral_bem_pat to 255 format ">>>>>>>>9" view-as text skip
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_total_geral_Lay_tot_res_cta header
    skip (1)
    "Total Geral" at 44
    v_des_val_tot_geral_imobdo_aux at 57 format "x(23)" view-as text
    v_des_val_tot_geral_dpr_aux at 82 format "x(23)" view-as text
    v_des_val_tot_geral_amort_aux at 107 format "x(23)" view-as text
    v_des_val_tot_geral_liq_bem_pat at 132 format "x(23)" view-as text
    v_qtd_tot_geral_bem_pat to 165 format ">>>>>>>>9" view-as text skip
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_total_quebra_Lay_total_geral header
    skip (1)
    "Total Imobilizado" at 148
    "Total Depreciado" at 172
    "Total Amortizado" at 197
    "Total Liquido Bem" at 222 skip
    "-------------------------------------------------------------------------------------------------" at 148 skip
    with no-box no-labels width 255 page-top stream-io.
def frame f_rpt_s_1_Grp_total_quebra_Lay_total_quebra header
    "Total Imobilizado" at 148
    "Total Depreciado" at 172
    "Total Amortizado" at 197
    "Total Liquido Bem" at 222 skip
    "-----------------------" at 148
    "-----------------------" at 172
    "-----------------------" at 197
    "-----------------------" at 222 skip
    with no-box no-labels width 255 page-top stream-io.


/*************************** Report Definition End **************************/

/************************** Frame Definition Begin **************************/

def frame f_rpt_40_bem_pat_sit_geral_pat
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
    br_dwb_rpt_select
         at row 02.00 col 44.00
    bt_isl1
         at row 02.79 col 83.00 font ?
         help "Insere Linha"
    bt_up
         at row 03.42 col 35.00 font ?
         help "Sobe"
    bt_edl1
         at row 04.00 col 83.00 font ?
         help "Edita Linha"
    bt_down
         at row 04.58 col 35.00 font ?
         help "Desce"
    bt_rml1
         at row 05.21 col 83.00 font ?
         help "Retira Linha"
    cenar_ctbl.cod_cenar_ctbl
         at row 08.50 col 13.00 colon-aligned label "Cen†rio"
         view-as fill-in
         size-chars 9.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_zoo_87086
         at row 08.50 col 24.14
    finalid_econ.cod_finalid_econ
         at row 09.50 col 13.00 colon-aligned label "Finalidade"
         view-as fill-in
         size-chars 11.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_zoo_87087
         at row 09.50 col 26.14
    v_dat_fim_period
         at row 10.50 col 13.00 colon-aligned label "atÇ"
         help "Data Fim do Per°odo"
         view-as fill-in
         size-chars 11.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_log_consid_bem_bxado
         at row 11.58 col 03.43 label "Considera bem baixado"
         view-as toggle-box
    v_log_resum_cta
         at row 11.58 col 23.00 label "Resumido por Conta"
         view-as toggle-box
    v_log_impr_narrat
         at row 12.58 col 03.43 label "Imprime Narrativa"
         view-as toggle-box
    v_log_bem_imobdo
         at row 13.58 col 03.43 label "Imprime Bens n∆o Imobilizados"
         view-as toggle-box
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
    rs_ind_run_mode
         at row 12.21 col 44.00
         help "" no-label
    v_log_print_par
         at row 13.21 col 44.00 label "Imprime ParÉmetros"
         view-as toggle-box
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
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 90.00 by 17.00
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Relat¢rio Situaá‰es Gerais Patrimìnio".
    /* adjust size of objects in this frame */
    assign bt_can:width-chars               in frame f_rpt_40_bem_pat_sit_geral_pat = 10.00
           bt_can:height-chars              in frame f_rpt_40_bem_pat_sit_geral_pat = 01.00
           bt_close:width-chars             in frame f_rpt_40_bem_pat_sit_geral_pat = 10.00
           bt_close:height-chars            in frame f_rpt_40_bem_pat_sit_geral_pat = 01.00
           bt_down:width-chars              in frame f_rpt_40_bem_pat_sit_geral_pat = 04.00
           bt_down:height-chars             in frame f_rpt_40_bem_pat_sit_geral_pat = 01.13
           bt_edl1:width-chars              in frame f_rpt_40_bem_pat_sit_geral_pat = 04.00
           bt_edl1:height-chars             in frame f_rpt_40_bem_pat_sit_geral_pat = 01.13
           bt_get_file:width-chars          in frame f_rpt_40_bem_pat_sit_geral_pat = 04.00
           bt_get_file:height-chars         in frame f_rpt_40_bem_pat_sit_geral_pat = 01.08
           bt_hel2:width-chars              in frame f_rpt_40_bem_pat_sit_geral_pat = 10.00
           bt_hel2:height-chars             in frame f_rpt_40_bem_pat_sit_geral_pat = 01.00
           bt_isl1:width-chars              in frame f_rpt_40_bem_pat_sit_geral_pat = 04.00
           bt_isl1:height-chars             in frame f_rpt_40_bem_pat_sit_geral_pat = 01.13
           bt_print:width-chars             in frame f_rpt_40_bem_pat_sit_geral_pat = 10.00
           bt_print:height-chars            in frame f_rpt_40_bem_pat_sit_geral_pat = 01.00
           bt_rml1:width-chars              in frame f_rpt_40_bem_pat_sit_geral_pat = 04.00
           bt_rml1:height-chars             in frame f_rpt_40_bem_pat_sit_geral_pat = 01.13
           bt_set_printer:width-chars       in frame f_rpt_40_bem_pat_sit_geral_pat = 04.00
           bt_set_printer:height-chars      in frame f_rpt_40_bem_pat_sit_geral_pat = 01.08
           bt_up:width-chars                in frame f_rpt_40_bem_pat_sit_geral_pat = 04.00
           bt_up:height-chars               in frame f_rpt_40_bem_pat_sit_geral_pat = 01.13
           ed_1x40:width-chars              in frame f_rpt_40_bem_pat_sit_geral_pat = 38.00
           ed_1x40:height-chars             in frame f_rpt_40_bem_pat_sit_geral_pat = 01.00
           ls_order:width-chars             in frame f_rpt_40_bem_pat_sit_geral_pat = 30.00
           ls_order:height-chars            in frame f_rpt_40_bem_pat_sit_geral_pat = 05.00
           rt_cxcf:width-chars              in frame f_rpt_40_bem_pat_sit_geral_pat = 86.50
           rt_cxcf:height-chars             in frame f_rpt_40_bem_pat_sit_geral_pat = 01.40
           rt_dimensions:width-chars        in frame f_rpt_40_bem_pat_sit_geral_pat = 20.57
           rt_dimensions:height-chars       in frame f_rpt_40_bem_pat_sit_geral_pat = 03.00
           rt_order:width-chars             in frame f_rpt_40_bem_pat_sit_geral_pat = 39.00
           rt_order:height-chars            in frame f_rpt_40_bem_pat_sit_geral_pat = 06.00
           rt_parameters_label:width-chars  in frame f_rpt_40_bem_pat_sit_geral_pat = 39.00
           rt_parameters_label:height-chars in frame f_rpt_40_bem_pat_sit_geral_pat = 06.50
           rt_run:width-chars               in frame f_rpt_40_bem_pat_sit_geral_pat = 25.00
           rt_run:height-chars              in frame f_rpt_40_bem_pat_sit_geral_pat = 03.00
           rt_select:width-chars            in frame f_rpt_40_bem_pat_sit_geral_pat = 46.57
           rt_select:height-chars           in frame f_rpt_40_bem_pat_sit_geral_pat = 06.00
           rt_target:width-chars            in frame f_rpt_40_bem_pat_sit_geral_pat = 46.57
           rt_target:height-chars           in frame f_rpt_40_bem_pat_sit_geral_pat = 03.00.
&if '{&emsbas_version}' >= '5.06' &then
if OPSYS = 'WIN32':U then do:
assign br_dwb_rpt_select:ALLOW-COLUMN-SEARCHING in frame f_rpt_40_bem_pat_sit_geral_pat = no
       br_dwb_rpt_select:COLUMN-MOVABLE in frame f_rpt_40_bem_pat_sit_geral_pat = no.
end.
&endif
    /* set return-inserted = yes for editors */
    assign ed_1x40:return-inserted in frame f_rpt_40_bem_pat_sit_geral_pat = yes.
    /* set private-data for the help system */
    assign ls_order:private-data                      in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000007715":U
           br_dwb_rpt_select:private-data             in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000007715":U
           bt_isl1:private-data                       in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000008786":U
           bt_up:private-data                         in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000009438":U
           bt_edl1:private-data                       in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000008790":U
           bt_down:private-data                       in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000009436":U
           bt_rml1:private-data                       in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000008792":U
           bt_zoo_87086:private-data                  in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000009431":U
           cenar_ctbl.cod_cenar_ctbl:private-data     in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000005310":U
           bt_zoo_87087:private-data                  in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000009431":U
           finalid_econ.cod_finalid_econ:private-data in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000005371":U
           v_dat_fim_period:private-data              in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000013009":U
           v_log_consid_bem_bxado:private-data        in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000023241":U
           v_log_resum_cta:private-data               in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000023343":U
           v_log_impr_narrat:private-data             in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000023344":U
           v_log_bem_imobdo:private-data              in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000023345":U
           rs_cod_dwb_output:private-data             in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000007715":U
           ed_1x40:private-data                       in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000007715":U
           bt_set_printer:private-data                in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000008785":U
           bt_get_file:private-data                   in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000008782":U
           rs_ind_run_mode:private-data               in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000007715":U
           v_log_print_par:private-data               in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000024662":U
           bt_close:private-data                      in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000009420":U
           bt_print:private-data                      in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000010815":U
           bt_can:private-data                        in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000011050":U
           bt_hel2:private-data                       in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000011326":U
           v_qtd_line:private-data                    in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000024737":U
           v_qtd_column:private-data                  in frame f_rpt_40_bem_pat_sit_geral_pat = "HLP=000024669":U
           frame f_rpt_40_bem_pat_sit_geral_pat:private-data                                  = "HLP=000007715".
    /* enable function buttons */
    assign bt_zoo_87086:sensitive in frame f_rpt_40_bem_pat_sit_geral_pat = yes
           bt_zoo_87087:sensitive in frame f_rpt_40_bem_pat_sit_geral_pat = yes.
    /* move buttons to top */
    bt_zoo_87086:move-to-top().
    bt_zoo_87087:move-to-top().



{include/i_fclfrm.i f_rpt_40_bem_pat_sit_geral_pat }
/*************************** Frame Definition End ***************************/

/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
def var v_prog_filtro_pdf as handle no-undo.

function getCodTipoRelat returns character in v_prog_filtro_pdf.

run prgtec/btb/btb920aa.py persistent set v_prog_filtro_pdf.

run pi_define_objetos in v_prog_filtro_pdf (frame f_rpt_40_bem_pat_sit_geral_pat:handle,
                       rs_cod_dwb_output:handle in frame f_rpt_40_bem_pat_sit_geral_pat,
                       bt_get_file:row in frame f_rpt_40_bem_pat_sit_geral_pat,
                       bt_get_file:col in frame f_rpt_40_bem_pat_sit_geral_pat).

&endif
/* tech38629 - Fim da alteraá∆o */


/*********************** User Interface Trigger Begin ***********************/


ON CHOOSE OF bt_down IN FRAME f_rpt_40_bem_pat_sit_geral_pat
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

    assign v_cod_dwb_field = ls_order:screen-value in frame f_rpt_40_bem_pat_sit_geral_pat
           v_cod_dwb_order = ls_order:list-items in frame f_rpt_40_bem_pat_sit_geral_pat
           v_num_entry = lookup(v_cod_dwb_field, v_cod_dwb_order).

    if  v_num_entry > 0 and v_num_entry < num-entries (v_cod_dwb_order)
    then do:
        assign entry(v_num_entry, v_cod_dwb_order) = entry(v_num_entry + 1, v_cod_dwb_order)
               entry(v_num_entry + 1, v_cod_dwb_order) = v_cod_dwb_field
               ls_order:list-items in frame f_rpt_40_bem_pat_sit_geral_pat = v_cod_dwb_order
               ls_order:screen-value in frame f_rpt_40_bem_pat_sit_geral_pat = v_cod_dwb_field.
    end /* if */.
END. /* ON CHOOSE OF bt_down IN FRAME f_rpt_40_bem_pat_sit_geral_pat */

ON CHOOSE OF bt_edl1 IN FRAME f_rpt_40_bem_pat_sit_geral_pat
DO:

    /************************* Variable Definition Begin ************************/

    def var v_log_method
        as logical
        format "Sim/N∆o"
        initial yes
        no-undo.


    /************************** Variable Definition End *************************/

    if  br_dwb_rpt_select:num-selected-rows in frame f_rpt_40_bem_pat_sit_geral_pat = 1
    then do:
        assign v_log_method = br_dwb_rpt_select:fetch-selected-row(1) in frame f_rpt_40_bem_pat_sit_geral_pat.
        run pi_edl_dwb_rpt_select (Input recid(dwb_rpt_select)) /*pi_edl_dwb_rpt_select*/.
        run pi_open_dwb_rpt_select /*pi_open_dwb_rpt_select*/.
    end /* if */.
END. /* ON CHOOSE OF bt_edl1 IN FRAME f_rpt_40_bem_pat_sit_geral_pat */

ON CHOOSE OF bt_get_file IN FRAME f_rpt_40_bem_pat_sit_geral_pat
DO:

    system-dialog get-file v_cod_dwb_file
        title "Imprimir" /*l_imprimir*/ 
        filters '*.rpt' '*.rpt',
                "*.*"  "*.*"
        save-as
        create-test-file
        ask-overwrite.
        assign dwb_rpt_param.cod_dwb_file              = v_cod_dwb_file
               ed_1x40:screen-value in frame f_rpt_40_bem_pat_sit_geral_pat = v_cod_dwb_file.

END. /* ON CHOOSE OF bt_get_file IN FRAME f_rpt_40_bem_pat_sit_geral_pat */

ON CHOOSE OF bt_hel2 IN FRAME f_rpt_40_bem_pat_sit_geral_pat
DO:


    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.


    /* End_Include: i_context_help_frame */

END. /* ON CHOOSE OF bt_hel2 IN FRAME f_rpt_40_bem_pat_sit_geral_pat */

ON CHOOSE OF bt_isl1 IN FRAME f_rpt_40_bem_pat_sit_geral_pat
DO:

    /************************* Variable Definition Begin ************************/

    def var v_cod_dwb_field
        as character
        format "x(32)":U
        no-undo.


    /************************** Variable Definition End *************************/

    run pi_isl_dwb_rpt_select /*pi_isl_dwb_rpt_select*/.
    run pi_open_dwb_rpt_select /*pi_open_dwb_rpt_select*/.

END. /* ON CHOOSE OF bt_isl1 IN FRAME f_rpt_40_bem_pat_sit_geral_pat */

ON CHOOSE OF bt_print IN FRAME f_rpt_40_bem_pat_sit_geral_pat
DO:

    assign v_log_print = yes
           v_num_cont  = 0.
END. /* ON CHOOSE OF bt_print IN FRAME f_rpt_40_bem_pat_sit_geral_pat */

ON CHOOSE OF bt_rml1 IN FRAME f_rpt_40_bem_pat_sit_geral_pat
DO:

    /************************* Variable Definition Begin ************************/

    def var v_rec_dwb_rpt_select
        as recid
        format ">>>>>>9":U
        no-undo.


    /************************** Variable Definition End *************************/

    if  br_dwb_rpt_select:num-selected-rows = 1 and
        br_dwb_rpt_select:fetch-selected-row(1)
    then do:
        assign v_rec_dwb_rpt_select = recid(dwb_rpt_select).
        find dwb_rpt_select exclusive-lock
             where recid(dwb_rpt_select) = v_rec_dwb_rpt_select /*cl_dwb_rpt_select_recid of dwb_rpt_select*/.
        delete dwb_rpt_select.
        run pi_open_dwb_rpt_select /*pi_open_dwb_rpt_select*/.
    end /* if */.

END. /* ON CHOOSE OF bt_rml1 IN FRAME f_rpt_40_bem_pat_sit_geral_pat */

ON CHOOSE OF bt_set_printer IN FRAME f_rpt_40_bem_pat_sit_geral_pat
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
               ed_1x40:screen-value in frame f_rpt_40_bem_pat_sit_geral_pat = v_nom_dwb_printer
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
                with frame f_rpt_40_bem_pat_sit_geral_pat.
    end /* if */.

END. /* ON CHOOSE OF bt_set_printer IN FRAME f_rpt_40_bem_pat_sit_geral_pat */

ON CHOOSE OF bt_up IN FRAME f_rpt_40_bem_pat_sit_geral_pat
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

    assign v_cod_dwb_field = ls_order:screen-value in frame f_rpt_40_bem_pat_sit_geral_pat
           v_cod_dwb_order = ls_order:list-items in frame f_rpt_40_bem_pat_sit_geral_pat
           v_num_entry = lookup(v_cod_dwb_field, v_cod_dwb_order).

    if  v_num_entry > (1 + 0)
    then do:
        assign entry(v_num_entry, v_cod_dwb_order) = entry(v_num_entry - 1, v_cod_dwb_order)
               entry(v_num_entry - 1, v_cod_dwb_order) = v_cod_dwb_field
               ls_order:list-items in frame f_rpt_40_bem_pat_sit_geral_pat = v_cod_dwb_order
               ls_order:screen-value in frame f_rpt_40_bem_pat_sit_geral_pat = v_cod_dwb_field.
    end /* if */.
END. /* ON CHOOSE OF bt_up IN FRAME f_rpt_40_bem_pat_sit_geral_pat */

ON LEAVE OF ed_1x40 IN FRAME f_rpt_40_bem_pat_sit_geral_pat
DO:

    /************************* Variable Definition Begin ************************/

    def var v_cod_filename_final             as character       no-undo. /*local*/
    def var v_cod_filename_initial           as character       no-undo. /*local*/


    /************************** Variable Definition End *************************/

    block:
    do with frame f_rpt_40_bem_pat_sit_geral_pat:
        if  rs_cod_dwb_output:screen-value = "Arquivo" /*l_file*/ 
        then do:
            if  rs_ind_run_mode:screen-value <> "Batch" /*l_batch*/ 
            then do:
                if  ed_1x40:screen-value  <> ""
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
            assign dwb_rpt_param.cod_dwb_file = ed_1x40:screen-value.
        end /* if */.
    end /* do block */.

END. /* ON LEAVE OF ed_1x40 IN FRAME f_rpt_40_bem_pat_sit_geral_pat */

ON VALUE-CHANGED OF rs_cod_dwb_output IN FRAME f_rpt_40_bem_pat_sit_geral_pat
DO:

    initout:
    do with frame f_rpt_40_bem_pat_sit_geral_pat:
        /* block: */
        case self:screen-value:
            when "Terminal" /*l_terminal*/ then ter:
             do:
                if  rs_cod_dwb_output <> "Impressora" /*l_printer*/ 
                then do:
                    assign v_qtd_line_ant = input frame f_rpt_40_bem_pat_sit_geral_pat v_qtd_line.
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
                        with frame f_rpt_40_bem_pat_sit_geral_pat.

                assign ed_1x40:screen-value   = ""
                       ed_1x40:sensitive      = no
                       bt_get_file:visible    = no
                       bt_set_printer:visible = no.
            end /* do ter */.
            when "Arquivo" /*l_file*/ then fil:
             do:
                if  rs_cod_dwb_output <> "Impressora" /*l_printer*/ 
                then do:
                    assign v_qtd_line_ant = input frame f_rpt_40_bem_pat_sit_geral_pat v_qtd_line.
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
                        with frame f_rpt_40_bem_pat_sit_geral_pat.

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
                      /*cl_current_user of usuar_mestre*/ no-error.
                    assign dwb_rpt_param.cod_dwb_file = "".

                if  rs_ind_run_mode:screen-value in frame f_rpt_40_bem_pat_sit_geral_pat <> "Batch" /*l_batch*/ 
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
                                                      + caps("esfas004":U)
                                                      + '.rpt'.
                end /* if */.
                else do:
                    assign dwb_rpt_param.cod_dwb_file = dwb_rpt_param.cod_dwb_file
                                                      + v_cod_dwb_file_temp.
                end /* else */.
                assign ed_1x40:screen-value                = dwb_rpt_param.cod_dwb_file
                       dwb_rpt_param.cod_dwb_print_layout  = ""
                       v_qtd_line                          = (if v_qtd_line_ant > 0 then v_qtd_line_ant
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
                    assign v_qtd_line_ant = input frame f_rpt_40_bem_pat_sit_geral_pat v_qtd_line.
                end /* if */.

                assign ed_1x40:sensitive        = no
                       bt_get_file:visible      = no
                       bt_set_printer:visible   = yes
                       bt_set_printer:sensitive = yes.

                /* define layout default */
                if   v_cod_dwb_file_old <> "" then
                     assign dwb_rpt_param.cod_dwb_print_layout = v_cod_dwb_file_old.

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

                if  dwb_rpt_param.cod_dwb_print_layout <> "" then
                    assign v_cod_dwb_file_old = dwb_rpt_param.cod_dwb_print_layout.

                find layout_impres no-lock
                     where layout_impres.nom_impressora = dwb_rpt_param.nom_dwb_printer
                       and layout_impres.cod_layout_impres = dwb_rpt_param.cod_dwb_print_layout /*cl_get_layout of layout_impres*/ no-error.
                if  avail layout_impres
                then do:
                    assign v_qtd_line               = layout_impres.num_lin_pag.
                end /* if */.
                display v_qtd_line
                        with frame f_rpt_40_bem_pat_sit_geral_pat.

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
                with frame f_rpt_40_bem_pat_sit_geral_pat.
    end /* if */.
    else do:
        enable v_qtd_line
               with frame f_rpt_40_bem_pat_sit_geral_pat.
    end /* else */.

    assign rs_cod_dwb_output.

END. /* ON VALUE-CHANGED OF rs_cod_dwb_output IN FRAME f_rpt_40_bem_pat_sit_geral_pat */

ON VALUE-CHANGED OF rs_ind_run_mode IN FRAME f_rpt_40_bem_pat_sit_geral_pat
DO:

    assign dwb_rpt_param.ind_dwb_run_mode = input frame f_rpt_40_bem_pat_sit_geral_pat rs_ind_run_mode.

    if  dwb_rpt_param.ind_dwb_run_mode = "Batch" /*l_batch*/ 
    then do:
        if  rs_cod_dwb_output:disable("Terminal" /*l_terminal*/ ) in frame f_rpt_40_bem_pat_sit_geral_pat
        then do:
        end /* if */.
    end /* if */.
    else do:
        if  rs_cod_dwb_output:enable("Terminal" /*l_terminal*/ ) in frame f_rpt_40_bem_pat_sit_geral_pat
        then do:
        end /* if */.
    end /* else */.
    if  rs_ind_run_mode = "Batch" /*l_batch*/ 
    then do:
        assign v_qtd_line = v_qtd_line_ant.
        display v_qtd_line
                with frame f_rpt_40_bem_pat_sit_geral_pat.
    end /* if */.
    assign rs_ind_run_mode.
    apply "value-changed" to rs_cod_dwb_output in frame f_rpt_40_bem_pat_sit_geral_pat.
END. /* ON VALUE-CHANGED OF rs_ind_run_mode IN FRAME f_rpt_40_bem_pat_sit_geral_pat */

ON VALUE-CHANGED OF v_log_resum_cta IN FRAME f_rpt_40_bem_pat_sit_geral_pat
DO:

    if  v_log_resum_cta:checked = yes
    then do:
        assign v_cod_dwb_order = "Conta Patrimonial,Bem Patrimonial,Descriá∆o Bem Pat,Data Aquisiá∆o,Data C†lculo," + &IF '{&emsfin_version}' >= '5.01' AND '{&emsfin_version}' <= '5.07' &THEN "Estabelecimento" &ELSE "Estabelecimento" &ENDIF + ",Unid Neg¢cio,Plano Centros Custo,CCusto Responsab"
               ls_order:list-items in frame f_rpt_40_bem_pat_sit_geral_pat = v_cod_dwb_order
               v_log_impr_narrat = no.
        disable bt_down
                bt_up
                ls_order
                v_log_impr_narrat
                with frame f_rpt_40_bem_pat_sit_geral_pat.
    end /* if */.
    else do:
        enable bt_down
               bt_up
               ls_order
               v_log_impr_narrat
               with frame f_rpt_40_bem_pat_sit_geral_pat.
    end /* else */.    




END. /* ON VALUE-CHANGED OF v_log_resum_cta IN FRAME f_rpt_40_bem_pat_sit_geral_pat */


/************************ User Interface Trigger End ************************/

/************************** Function Trigger Begin **************************/


ON  CHOOSE OF bt_zoo_87086 IN FRAME f_rpt_40_bem_pat_sit_geral_pat
OR F5 OF cenar_ctbl.cod_cenar_ctbl IN FRAME f_rpt_40_bem_pat_sit_geral_pat DO:

    /* fn_generic_zoom */
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
        assign cenar_ctbl.cod_cenar_ctbl:screen-value in frame f_rpt_40_bem_pat_sit_geral_pat =
               string(cenar_ctbl.cod_cenar_ctbl).

    end /* if */.
    apply "entry" to cenar_ctbl.cod_cenar_ctbl in frame f_rpt_40_bem_pat_sit_geral_pat.
end. /* ON  CHOOSE OF bt_zoo_87086 IN FRAME f_rpt_40_bem_pat_sit_geral_pat */

ON  CHOOSE OF bt_zoo_87087 IN FRAME f_rpt_40_bem_pat_sit_geral_pat
OR F5 OF finalid_econ.cod_finalid_econ IN FRAME f_rpt_40_bem_pat_sit_geral_pat DO:

    /* fn_generic_zoom */
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
        assign finalid_econ.cod_finalid_econ:screen-value in frame f_rpt_40_bem_pat_sit_geral_pat =
               string(finalid_econ.cod_finalid_econ).

    end /* if */.
    apply "entry" to finalid_econ.cod_finalid_econ in frame f_rpt_40_bem_pat_sit_geral_pat.
end. /* ON  CHOOSE OF bt_zoo_87087 IN FRAME f_rpt_40_bem_pat_sit_geral_pat */


/*************************** Function Trigger End ***************************/

/**************************** Frame Trigger Begin ***************************/


ON ENDKEY OF FRAME f_rpt_40_bem_pat_sit_geral_pat
DO:


    /* Begin_Include: i_exec_program_epc */
    &if '{&emsbas_version}' > '1.00' &then
    if  v_nom_prog_upc <> '' then
    do:
        assign v_rec_table_epc = recid(bem_pat).    
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
        assign v_rec_table_epc = recid(bem_pat).    
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
        assign v_rec_table_epc = recid(bem_pat).    
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

END. /* ON ENDKEY OF FRAME f_rpt_40_bem_pat_sit_geral_pat */

ON GO OF FRAME f_rpt_40_bem_pat_sit_geral_pat
DO:

    assign dwb_rpt_param.cod_dwb_output     = rs_cod_dwb_output:screen-value in frame f_rpt_40_bem_pat_sit_geral_pat
           dwb_rpt_param.qtd_dwb_line       = input frame f_rpt_40_bem_pat_sit_geral_pat v_qtd_line
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

END. /* ON GO OF FRAME f_rpt_40_bem_pat_sit_geral_pat */

ON HELP OF FRAME f_rpt_40_bem_pat_sit_geral_pat ANYWHERE
DO:


    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */

END. /* ON HELP OF FRAME f_rpt_40_bem_pat_sit_geral_pat */

ON RIGHT-MOUSE-DOWN OF FRAME f_rpt_40_bem_pat_sit_geral_pat ANYWHERE
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

END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_rpt_40_bem_pat_sit_geral_pat */

ON RIGHT-MOUSE-UP OF FRAME f_rpt_40_bem_pat_sit_geral_pat ANYWHERE
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

END. /* ON RIGHT-MOUSE-UP OF FRAME f_rpt_40_bem_pat_sit_geral_pat */

ON WINDOW-CLOSE OF FRAME f_rpt_40_bem_pat_sit_geral_pat
DO:

    apply "end-error" to self.
END. /* ON WINDOW-CLOSE OF FRAME f_rpt_40_bem_pat_sit_geral_pat */


/***************************** Frame Trigger End ****************************/

/**************************** Menu Trigger Begin ****************************/


ON CHOOSE OF MENU-ITEM mi_conteudo IN MENU m_help
DO:


        apply "choose" to bt_hel2 in frame f_rpt_40_bem_pat_sit_geral_pat.





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


        assign v_nom_prog     = substring(frame f_rpt_40_bem_pat_sit_geral_pat:title, 1, max(1, length(frame f_rpt_40_bem_pat_sit_geral_pat:title) - 10)).
        if  v_nom_prog = ? then
            assign v_nom_prog = "".

        assign v_nom_prog     = v_nom_prog
                              + chr(10)
                              + "esfas004":U.




    assign v_nom_prog_ext = "esp/esfas004.p":U
           v_cod_release  = trim(" 1.00.01.002":U).
/*    run prgtec/btb/btb901zb.p (Input v_nom_prog,
                               Input v_nom_prog_ext,
                               Input v_cod_release) /*prg_fnc_about*/. */
{include/sobre5.i}
END. /* ON CHOOSE OF MENU-ITEM mi_sobre IN MENU m_help */


/***************************** Menu Trigger End *****************************/


/****************************** Main Code Begin *****************************/


/* Begin_Include: i_version_extract */
/*
{include/i-ctrlrp5.i esfas004}
*/


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
    run pi_version_extract ('esfas004':U, 'esp/esfas004.p':U, '1.00.01.002':U, 'pro':U).
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
if  v_cod_dwb_user = ""
then do:
    assign v_cod_dwb_user = v_cod_usuar_corren.
end /* if */.

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
    run prgtec/men/men901za.py (Input 'esfas004') /*prg_fnc_verify_security*/.
if  return-value = "2014"
then do:
    /* Programa a ser executado n∆o Ç um programa v†lido Datasul ! */
    run pi_messages (input "show",
                     input 2014,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'esfas004')) /*msg_2014*/.
    return.
end /* if */.
if  return-value = "2012"
then do:
    /* Usu†rio sem permiss∆o para acessar o programa. */
    run pi_messages (input "show",
                     input 2012,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'esfas004')) /*msg_2012*/.
    return.
end /* if */.
/* End_Include: i_verify_security */



/* Begin_Include: i_log_exec_prog_dtsul_ini */
assign v_rec_log = ?.

if can-find(prog_dtsul
       where prog_dtsul.cod_prog_dtsul = 'esfas004' 
         and prog_dtsul.log_gera_log_exec = yes) then do transaction:
    create log_exec_prog_dtsul.
    assign log_exec_prog_dtsul.cod_prog_dtsul           = 'esfas004'
           log_exec_prog_dtsul.cod_usuario              = v_cod_usuar_corren
           log_exec_prog_dtsul.dat_inic_exec_prog_dtsul = today
           log_exec_prog_dtsul.hra_inic_exec_prog_dtsul = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":":U,"":U).
    assign v_rec_log = recid(log_exec_prog_dtsul).
    release log_exec_prog_dtsul no-error.
end.


/* End_Include: i_log_exec_prog_dtsul_ini */

/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
run pi_permissoes in v_prog_filtro_pdf (input 'esfas004':U).
&endif
/* tech38629 - Fim da alteraá∆o */




/* Begin_Include: i_verify_program_epc */
&if '{&emsbas_version}' > '1.00' &then
assign v_rec_table_epc = ?
       v_wgh_frame_epc = ?.

find prog_dtsul
    where prog_dtsul.cod_prog_dtsul = "esfas004":U
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


assign v_wgh_frame_epc = frame f_rpt_40_bem_pat_sit_geral_pat:handle.



assign v_nom_table_epc = 'bem_pat':U
       v_rec_table_epc = recid(bem_pat).

&endif

/* End_Include: i_verify_program_epc */


/* redefiniá‰es do frame */

/* Begin_Include: i_std_dialog_box */
/* tratamento do titulo e vers∆o */
assign frame f_rpt_40_bem_pat_sit_geral_pat:title = frame f_rpt_40_bem_pat_sit_geral_pat:title
                            + chr(32)
                            + chr(40)
                            + trim(" 1.00.01.002":U)
                            + chr(41).
/* menu pop-up de ajuda e sobre */
assign menu m_help:popup-only = yes
       bt_hel2:popup-menu in frame f_rpt_40_bem_pat_sit_geral_pat = menu m_help:handle.


/* End_Include: i_std_dialog_box */


/* inicializa vari†veis */
run pi_initialize_reports /*pi_initialize_reports*/.


/* Begin_Include: ix_p01_rpt_bem_pat_sit_geral_pat */
assign v_log_quant_bem = no.
&if  defined(BF_FIN_QT_BEM) &then
        assign v_log_quant_bem = yes.
&else
        find histor_exec_especial
     	  where histor_exec_especial.cod_modul_dtsul = 'UFN'
    and   histor_exec_especial.cod_prog_dtsul  = 'spp_qt_bem':U
          no-lock no-error.
         if  avail histor_exec_especial then
        assign v_log_quant_bem = yes.
&endif
/* End_Include: ix_p01_rpt_bem_pat_sit_geral_pat */


if  v_cod_dwb_user begins 'es_'
then do:
    find dwb_rpt_param no-lock
         where dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
           and dwb_rpt_param.cod_dwb_user = v_cod_dwb_user /*cl_dwb_rpt_param of dwb_rpt_param*/ no-error.
    if  not avail dwb_rpt_param
    then do:
        return "ParÉmetros para o relat¢rio n∆o encontrado." /*1993*/ + " (" + "1993" + ")" + chr(10) + "N∆o foi poss°vel encontrar os parÉmetros necess†rios para a impress∆o do relat¢rio para o programa e usu†rio corrente." /*1993*/.
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
    if  ped_exec.cod_release_prog_dtsul <> trim(" 1.00.01.002":U)
    then do:
        return "Vers‰es do programa diferente." /*1994*/ + " (" + "1994" + ")" + chr(10)
                                     + substitute("A vers∆o do programa (&3) que gerou o pedido de execuá∆o batch (&1) Ç diferente da vers∆o do programa que deveria executar o pedido batch (&2)." /*1994*/,ped_exec.cod_release_prog_dtsul,
                                                  trim(" 1.00.01.002":U),
                                                  "esp/esfas004.p":U).
    end /* if */.
    assign v_nom_prog_ext     = caps("esfas004":U)
           v_dat_execution    = today
           v_hra_execution    = replace(string(time, "hh:mm:ss" /*l_hh:mm:ss*/ ), ":", "")
           v_cod_dwb_file     = dwb_rpt_param.cod_dwb_file
           v_nom_report_title = fill(" ", 40 - length(v_rpt_s_1_name)) + v_rpt_s_1_name
           v_ind_dwb_run_mode = "Batch" /*l_batch*/ 
           v_cod_dwb_order    = dwb_rpt_param.cod_dwb_order.

    /* ix_p02_rpt_bem_pat_sit_geral_pat */

    /* configura e define destino de impress∆o */
    if  dwb_rpt_param.cod_dwb_output = "Impressora" /*l_printer*/ 
    then do:
         assign v_qtd_line_ant = v_qtd_line.
    end /* if */.
    run pi_output_reports /*pi_output_reports*/.

    if  dwb_rpt_param.log_dwb_print_parameters = yes
    then do:
        run pi_print_parameters /*pi_print_parameters*/.

        /* Begin_Include: ix_p30_rpt_bem_pat_sit_geral_pat */
        if (line-counter(s_1) + 7) > v_rpt_s_1_bottom then
            page stream s_1.
        put stream s_1 unformatted 
            skip (1)
            "Cen†rio Cont†bil: " at 94
            v_cod_cenar_ctbl at 112 format "x(8)" skip
            "Finalidade Econìmica: " at 90
            v_cod_finalid_econ at 112 format "x(10)" skip
            "Fim Per°odo: " at 99
            v_dat_fim_period at 112 format "99/99/9999" skip
            "Considera Bem Bxado: " at 91
            v_log_consid_bem_bxado at 112 format "Sim/N∆o"
            "Resumido por Conta: " at 122
            v_log_resum_cta at 142 format "Sim/N∆o" skip
            "Imprime Narrativa: " at 93
            v_log_impr_narrat at 112 format "Sim/N∆o" skip
            "Bens n∆o Imobilizado: " at 90
            v_log_bem_imobdo at 112 format "Sim/N∆o" skip.
        /* End_Include: ix_p30_rpt_bem_pat_sit_geral_pat */

    end /* if */.

    output stream s_1 close.

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
view frame f_rpt_40_bem_pat_sit_geral_pat.

/* Begin_Include: i_exec_program_epc */
&if '{&emsbas_version}' > '1.00' &then
if  v_nom_prog_upc <> '' then
do:
    assign v_rec_table_epc = recid(bem_pat).    
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
    assign v_rec_table_epc = recid(bem_pat).    
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
    assign v_rec_table_epc = recid(bem_pat).    
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


/* ix_p05_rpt_bem_pat_sit_geral_pat */

super_block:
repeat:

    run pi_configure_dwb_param /*pi_configure_dwb_param*/.
    assign v_qtd_line = dwb_rpt_param.qtd_dwb_line.

    init:
    do with frame f_rpt_40_bem_pat_sit_geral_pat:
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
                with frame f_rpt_40_bem_pat_sit_geral_pat.
    end /* do init */.

    display v_qtd_column
            v_qtd_line
            with frame f_rpt_40_bem_pat_sit_geral_pat.

    /* Begin_Include: i_exec_program_epc */
    &if '{&emsbas_version}' > '1.00' &then
    if  v_nom_prog_upc <> '' then
    do:
        assign v_rec_table_epc = recid(bem_pat).    
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
        assign v_rec_table_epc = recid(bem_pat).    
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
        assign v_rec_table_epc = recid(bem_pat).    
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
           br_dwb_rpt_select
           rs_cod_dwb_output
           v_log_print_par
           bt_isl1
           bt_up
           bt_edl1
           bt_down
           bt_rml1
           bt_get_file
           bt_set_printer
           bt_close
           bt_print
           bt_can
           bt_hel2
           v_log_consid_bem_bxado
           v_log_resum_cta
           v_log_impr_narrat
           v_log_bem_imobdo
           with frame f_rpt_40_bem_pat_sit_geral_pat.

    /* Begin_Include: i_exec_program_epc */
    &if '{&emsbas_version}' > '1.00' &then
    if  v_nom_prog_upc <> '' then
    do:
        assign v_rec_table_epc = recid(bem_pat).    
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
        assign v_rec_table_epc = recid(bem_pat).    
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
        assign v_rec_table_epc = recid(bem_pat).    
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
                with frame f_rpt_40_bem_pat_sit_geral_pat.
    end /* if */.

/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
    run pi_posiciona_dwb_rpt_param in v_prog_filtro_pdf (input rowid(dwb_rpt_param)).
    run pi_load_params in v_prog_filtro_pdf.
&endif
/* tech38629 - Fim da alteraá∆o */


    apply "value-changed" to rs_cod_dwb_output in frame f_rpt_40_bem_pat_sit_geral_pat.


    if  yes = yes
    then do:
        enable rs_ind_run_mode
               with frame f_rpt_40_bem_pat_sit_geral_pat.
        if  dwb_rpt_param.ind_dwb_run_mode = "Batch" /*l_batch*/ 
        then do:
            apply "value-changed" to rs_ind_run_mode in frame f_rpt_40_bem_pat_sit_geral_pat.
        end /* if */.
    end /* if */.



    /* Begin_Include: ix_p10_rpt_bem_pat_sit_geral_pat */
    enable cenar_ctbl.cod_cenar_ctbl
           finalid_econ.cod_finalid_econ
           bt_zoo_87086
           bt_zoo_87087
           v_dat_fim_period
           with frame f_rpt_40_bem_pat_sit_geral_pat.

    /* ** PARAMETRO NOVO ***/
    if  num-entries(dwb_rpt_param.cod_dwb_parameters, chr(10)) < 8
    then do:
        blk_entries:
        do v_num_entries = num-entries(dwb_rpt_param.cod_dwb_parameters, chr(10)) to 8:
            assign dwb_rpt_param.cod_dwb_parameters = dwb_rpt_param.cod_dwb_parameters + chr(10) + "".
        end /* do blk_entries */.
    end /* if */.
    if  entry(5, dwb_rpt_param.cod_dwb_parameters, chr(10)) = ""
    then do:
        assign entry(5, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "N∆o" /*l_nao*/ .
    end /* if */.
    if  entry(6, dwb_rpt_param.cod_dwb_parameters, chr(10)) = ""
    then do:
        assign entry(6, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "N∆o" /*l_nao*/ .
    end /* if */.
    if  entry(7, dwb_rpt_param.cod_dwb_parameters, chr(10)) = ""
    then do:
        assign entry(7, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "N∆o" /*l_nao*/ .
    end /* if */.
    if  entry(8, dwb_rpt_param.cod_dwb_parameters, chr(10)) = ""
    then do:
        assign entry(8, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "N∆o" /*l_nao*/ .
    end /* if */.

    if  dwb_rpt_param.cod_dwb_parameters <> " "
    then do:
        assign cenar_ctbl.cod_cenar_ctbl:screen-value in frame f_rpt_40_bem_pat_sit_geral_pat     = entry(1, dwb_rpt_param.cod_dwb_parameters, chr(10))
               finalid_econ.cod_finalid_econ:screen-value in frame f_rpt_40_bem_pat_sit_geral_pat = entry(2, dwb_rpt_param.cod_dwb_parameters, chr(10))
               /* v_dat_inic_period:screen-value in frame @&(frame)             = entry(3, dwb_rpt_param.cod_dwb_parameters, chr(10))*/
               v_dat_fim_period:screen-value in frame f_rpt_40_bem_pat_sit_geral_pat              = entry(4, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_log_consid_bem_bxado:checked in frame f_rpt_40_bem_pat_sit_geral_pat        = entry(5, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "Sim" /*l_sim*/ 
               v_log_resum_cta:checked in frame f_rpt_40_bem_pat_sit_geral_pat               = entry(6, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "Sim" /*l_sim*/ 
               v_log_impr_narrat:checked in frame f_rpt_40_bem_pat_sit_geral_pat             = entry(7, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "Sim" /*l_sim*/ 
               v_log_bem_imobdo:checked in frame f_rpt_40_bem_pat_sit_geral_pat             = entry(8, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "Sim" /*l_sim*/ .
    end /* if */.

    apply "value-changed" to v_log_resum_cta.
    /* End_Include: ix_p10_rpt_bem_pat_sit_geral_pat */


    run pi_open_dwb_rpt_select /*pi_open_dwb_rpt_select*/.

    block1:
    repeat on error undo block1, retry block1:

        main_block:
        repeat on error undo super_block, leave super_block
                            on endkey undo super_block, leave super_block
                            on stop undo super_block, retry super_block
                            with frame f_rpt_40_bem_pat_sit_geral_pat:

            if  retry
            then do:
                output stream s_1 close.
            end /* if */.

            assign v_log_print = no.
            if  valid-handle( v_wgh_focus )
            then do:
                wait-for go of frame f_rpt_40_bem_pat_sit_geral_pat focus v_wgh_focus.
            end /* if */.
            else do:
                wait-for go of frame f_rpt_40_bem_pat_sit_geral_pat.
            end /* else */.


            /* Begin_Include: ix_p15_rpt_bem_pat_sit_geral_pat */
            if  v_log_print = yes
            then do:
                if  input frame f_rpt_40_bem_pat_sit_geral_pat cenar_ctbl.cod_cenar_ctbl = " " or
                    input frame f_rpt_40_bem_pat_sit_geral_pat cenar_ctbl.cod_cenar_ctbl = ?
                then do:
                    /* Cen†rio Cont†bil deve ser informado ! */
                    run pi_messages (input "show",
                                     input 694,
                                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_694*/.
                    apply "entry" to cenar_ctbl.cod_cenar_ctbl in frame f_rpt_40_bem_pat_sit_geral_pat.
                    next main_block.
                end /* if */.

                if  input frame f_rpt_40_bem_pat_sit_geral_pat finalid_econ.cod_finalid_econ = " " or
                    input frame f_rpt_40_bem_pat_sit_geral_pat finalid_econ.cod_finalid_econ = ?
                then do:
                    /* Finalidade Econìmica deve ser Informada ! */
                    run pi_messages (input "show",
                                     input 1075,
                                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_1075*/.
                    apply "entry" to finalid_econ.cod_finalid_econ in frame f_rpt_40_bem_pat_sit_geral_pat.
                    next main_block.
                end /* if */.

                find cenar_ctbl no-lock
                     where cenar_ctbl.cod_cenar_ctbl = input frame f_rpt_40_bem_pat_sit_geral_pat cenar_ctbl.cod_cenar_ctbl /*cl_rpt_40_bem_pat_sit_geral_pat of cenar_ctbl*/ no-error.
                if  not avail cenar_ctbl
                then do:
                    /* Cen†rio Cont†bil incorreto ! */
                    run pi_messages (input "show",
                                     input 601,
                                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_601*/.
                    apply "entry" to cenar_ctbl.cod_cenar_ctbl in frame f_rpt_40_bem_pat_sit_geral_pat.
                    next main_block.
                end /* if */.

                find finalid_econ no-lock
                     where finalid_econ.cod_finalid_econ = input frame f_rpt_40_bem_pat_sit_geral_pat finalid_econ.cod_finalid_econ /*cl_rpt_40_bem_pat_sit_geral_pat of finalid_econ*/ no-error.
                if  not avail finalid_econ
                then do:
                    /* Finalidade Econìmica Inv†lida ! */
                    run pi_messages (input "show",
                                     input 1080,
                                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_1080*/.
                    apply "entry" to finalid_econ.cod_finalid_econ in frame f_rpt_40_bem_pat_sit_geral_pat.
                    next main_block.
                end /* if */.

                if  input frame f_rpt_40_bem_pat_sit_geral_pat v_dat_fim_period = ? or
                    input frame f_rpt_40_bem_pat_sit_geral_pat v_dat_fim_period = " "
                then do:
                    /* In°cio e Fim Per°odo devem ser informados ! */
                    run pi_messages (input "show",
                                     input 1383,
                                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_1383*/.
                    apply "entry" to v_dat_fim_period in frame f_rpt_40_bem_pat_sit_geral_pat.
                    next main_block.
                end /* if */.

                /* @if(input frame @&(frame) v_dat_inic_period > input frame @&(frame) v_dat_fim_period)
                    @cx_message(1371).
                    @apply(entry) v_dat_inic_period in frame @&(frame).
                    @next(main_block).
                @end_if().*/
            end /* if */.
            /* assign dwb_rpt_param.cod_dwb_parameters = input frame @&(frame) cenar_ctbl.cod_cenar_ctbl + chr(10) +
                                                      input frame @&(frame) finalid_econ.cod_finalid_econ + chr(10) +
                                                      string(input frame @&(frame) v_dat_inic_period) + chr(10) +
                                                      string(input frame @&(frame) v_dat_fim_period) + chr(10) +
                                                      input frame @&(frame) v_log_consid_bem_bxado + chr(10) +
                                                      input frame @&(frame) v_log_resum_cta + chr(10) +
                                                      input frame @&(frame) v_log_impr_narrat + chr(10) +
                                                      input frame @&(frame) v_log_bem_imobdo + chr(10).
            */
            /* End_Include: ix_p15_rpt_bem_pat_sit_geral_pat */


            assign dwb_rpt_param.cod_dwb_order            = ls_order:list-items
                   dwb_rpt_param.ind_dwb_run_mode         = input frame f_rpt_40_bem_pat_sit_geral_pat rs_ind_run_mode
                   v_cod_dwb_order                        = ls_order:list-items
                   dwb_rpt_param.log_dwb_print_parameters = input frame f_rpt_40_bem_pat_sit_geral_pat v_log_print_par
                   input frame f_rpt_40_bem_pat_sit_geral_pat v_qtd_line.


            assign dwb_rpt_param.cod_dwb_parameters = input frame f_rpt_40_bem_pat_sit_geral_pat cenar_ctbl.cod_cenar_ctbl + chr(10) + input frame f_rpt_40_bem_pat_sit_geral_pat finalid_econ.cod_finalid_econ + chr(10) + string(input frame f_rpt_40_bem_pat_sit_geral_pat v_dat_fim_period) + chr(10) + string(input frame f_rpt_40_bem_pat_sit_geral_pat v_dat_fim_period) + chr(10) + input frame f_rpt_40_bem_pat_sit_geral_pat v_log_consid_bem_bxado + chr(10) + input frame f_rpt_40_bem_pat_sit_geral_pat v_log_resum_cta + chr(10) +     input frame f_rpt_40_bem_pat_sit_geral_pat v_log_impr_narrat + chr(10) +     input frame f_rpt_40_bem_pat_sit_geral_pat v_log_bem_imobdo + chr(10).


            /* ix_p20_rpt_bem_pat_sit_geral_pat */

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
/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
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
/*                        when "Terminal" /*l_terminal*/ then out_term:*/
                        if dwb_rpt_param.cod_dwb_output = 'Terminal' then
&else
                        when "Terminal" /*l_terminal*/ then out_term:
&endif
                         do:

                            assign v_cod_dwb_file   = session:temp-directory + "esfas004":U + '.tmp'
                                   v_rpt_s_1_bottom = v_qtd_line - (v_rpt_s_1_lines - v_qtd_bottom).
                            output stream s_1 to value(v_cod_dwb_file) paged page-size value(v_qtd_line) convert target 'iso8859-1'.
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



                            output stream s_1 to value(v_cod_dwb_file)
                                   paged page-size value(v_qtd_line) convert target 'iso8859-1'.
                        end /* do out_file */.
&if '{&emsbas_version}':U >= '5.05':U &then
/*                    end /* case out_def */.*/
&else
                    end /* case out_def */.
&endif
                    assign v_nom_prog_ext = caps(substring("esp/esfas004.p",12,8))
                           v_dat_execution = today
                           v_hra_execution = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":","").

                    run pi_rpt_bem_pat_sit_geral_pat /*pi_rpt_bem_pat_sit_geral_pat*/.
                end /* else */.

                if  dwb_rpt_param.log_dwb_print_parameters = yes
                then do:
                    run pi_print_parameters /*pi_print_parameters*/.

                    /* Begin_Include: ix_p30_rpt_bem_pat_sit_geral_pat */
                    if (line-counter(s_1) + 7) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        skip (1)
                        "Cen†rio Cont†bil: " at 94
                        v_cod_cenar_ctbl at 112 format "x(8)" skip
                        "Finalidade Econìmica: " at 90
                        v_cod_finalid_econ at 112 format "x(10)" skip
                        "Fim Per°odo: " at 99
                        v_dat_fim_period at 112 format "99/99/9999" skip.
                    put stream s_1 unformatted 
                        "Considera Bem Bxado: " at 91
                        v_log_consid_bem_bxado at 112 format "Sim/N∆o"
                        "Resumido por Conta: " at 122
                        v_log_resum_cta at 142 format "Sim/N∆o" skip
                        "Imprime Narrativa: " at 93
                        v_log_impr_narrat at 112 format "Sim/N∆o" skip
                        "Bens n∆o Imobilizado: " at 90
                        v_log_bem_imobdo at 112 format "Sim/N∆o" skip.
                    /* End_Include: ix_p30_rpt_bem_pat_sit_geral_pat */

                end /* if */.

                output stream s_1 close.

/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
run pi_call_convert_object in v_prog_filtro_pdf (input no,
                                                 input rs_cod_dwb_output:screen-value in frame f_rpt_40_bem_pat_sit_geral_pat,
                                                 input v_nom_dwb_print_file,
                                                 input v_cod_dwb_file,
                                                 input v_nom_report_title).
&endif
/* tech38629 - Fim da alteraá∆o */


&if '{&emsbas_version}':U >= '5.05':U &then
    if ((dwb_rpt_param.cod_dwb_output = 'Impressora':U or dwb_rpt_param.cod_dwb_output = 'Impresora':U or dwb_rpt_param.cod_dwb_output = 'printer':U) and getCodTipoRelat() = 'PDF':U) then do:
        if v_nom_dwb_print_file = '' then
            run pi_print_pdf_file in v_prog_filtro_pdf (input no).
    end.
&endif
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
            /* Criado pedido &1 para execuá∆o batch. */
            run pi_messages (input "show",
                             input 3556,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                v_num_ped_exec)) /*msg_3556*/.
            assign v_num_ped_exec = 0.
        end /* if */.

    end /* repeat block1 */.
end /* repeat super_block */.

/* ix_p40_rpt_bem_pat_sit_geral_pat */
hide frame f_rpt_40_bem_pat_sit_geral_pat.

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

/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
delete procedure v_prog_filtro_pdf.
&endif
/* tech38629 - Fim da alteraá∆o */






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
** Procedure Interna.....: pi_open_dwb_rpt_select
** Descricao.............: pi_open_dwb_rpt_select
** Criado por............: 
** Criado em.............: // 
** Alterado por..........: 
** Alterado em...........: 25/04/1995 16:13:44
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
** Criado por............: roger
** Criado em.............: // 
** Alterado por..........: izaura
** Alterado em...........: 21/05/1998 15:47:57
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
             radio-buttons "Regra", yes, "Exceá∆o", no
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
             title "Conjunto de Seleá∆o".
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

        run pi_isl_rpt_bem_pat_sit_geral_pat /*pi_isl_rpt_bem_pat_sit_geral_pat*/.

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

    ON RIGHT-MOUSE-UP OF FRAME f_dlg_04_dwb_rpt_select
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


    /***************************** Frame Trigger End ****************************/

    pause 0 before-hide.
    view frame f_dlg_04_dwb_rpt_select.

    assign v_log_ok = no
           frame f_dlg_04_dwb_rpt_select:title = "Inclui" /*l_inclui*/  + " Conjunto de Seleá∆o" /*l_conjunto_selecao*/ .

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
               bt_can
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
** Criado por............: roger
** Criado em.............: // 
** Alterado por..........: izaura
** Alterado em...........: 21/09/1999 10:13:00
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
             radio-buttons "Regra", yes, "Exceá∆o", no
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
             title "Conjunto de Seleá∆o".
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

        run pi_isl_rpt_bem_pat_sit_geral_pat /*pi_isl_rpt_bem_pat_sit_geral_pat*/.

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

    ON RIGHT-MOUSE-UP OF FRAME f_dlg_04_dwb_rpt_select
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


    /***************************** Frame Trigger End ****************************/

    pause 0 before-hide.
    view frame f_dlg_04_dwb_rpt_select.

    assign v_log_ok = no
           frame f_dlg_04_dwb_rpt_select:title = "Edita" /*l_edita*/  + " Conjunto de Seleá∆o" /*l_conjunto_selecao*/ .

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

        run pi_isl_rpt_bem_pat_sit_geral_pat /*pi_isl_rpt_bem_pat_sit_geral_pat*/.

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
    do with frame f_rpt_40_bem_pat_sit_geral_pat:

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
** Procedure Interna.....: pi_print_parameters
** Descricao.............: pi_print_parameters
** Criado por............: gilsinei
** Criado em.............: 04/09/1996 10:46:51
** Alterado por..........: gilsinei
** Alterado em...........: 04/09/1996 10:48:34
*****************************************************************************/
PROCEDURE pi_print_parameters:

    if  page-number (s_1) > 0
    then do:
        page stream s_1.
    end /* if */.

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
            dwb_rpt_select.log_dwb_rule at 1 format "Regra/Exceá∆o"
            dwb_rpt_select.cod_dwb_field at 9 format "x(32)"
            dwb_rpt_select.cod_dwb_initial at 42 format "x(18)"
            dwb_rpt_select.cod_dwb_final at 61 format "x(18)" skip.
    end /* for ler */.

END PROCEDURE. /* pi_print_parameters */
/*****************************************************************************
** Procedure Interna.....: pi_initialize_reports
** Descricao.............: pi_initialize_reports
** Criado por............: glauco
** Criado em.............: 21/03/1997 09:22:53
** Alterado por..........: izaura
** Alterado em...........: 06/04/2000 11:05:33
*****************************************************************************/
PROCEDURE pi_initialize_reports:

    /* inicializa vari†veis */
    find empresa no-lock
         where empresa.cod_empresa = v_cod_empres_usuar /*cl_empres_usuar of empresa*/ no-error.
    find dwb_rpt_param
         where dwb_rpt_param.cod_dwb_program = "esfas004":U
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

    assign v_cod_dwb_proced   = "esfas004":U
           v_cod_dwb_program  = "esfas004":U

           v_cod_dwb_order    = "Conta Patrimonial,Bem Patrimonial,Descriá∆o Bem Pat,Data Aquisiá∆o,Data C†lculo," + &IF '{&emsfin_version}' >= '5.01' AND '{&emsfin_version}' <= '5.07' &THEN "Estabelecimento" &ELSE "Estabelecimento" &ENDIF + ",Unid Neg¢cio,Plano Centros Custo,CCusto Responsab"

           v_cod_release      = trim(" 1.00.01.002":U)

           v_cod_dwb_select   = "Conta Patrimonial,Bem Patrimonial,Descriá∆o Bem Pat,Data Aquisiá∆o,Data C†lculo," + &IF '{&emsfin_version}' >= '5.01' AND '{&emsfin_version}' <= '5.07' &THEN "Estabelecimento" &ELSE "Estabelecimento" &ENDIF + ",Unid Neg¢cio,Plano Centros Custo,CCusto Responsab"

           v_ind_dwb_run_mode = "On-Line" /*l_online*/ 
           v_qtd_column       = v_rpt_s_1_columns
           v_qtd_bottom       = v_rpt_s_1_bottom.
    if  avail empresa
    then do:
        assign v_nom_enterprise   = empresa.nom_razao_social.
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
** Alterado por..........: izaura
** Alterado em...........: 21/09/1999 10:15:13
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
                   ls_order:list-items in frame f_rpt_40_bem_pat_sit_geral_pat = v_cod_dwb_order.
        end /* if */.
        else do:
            assign ls_order:list-items in frame f_rpt_40_bem_pat_sit_geral_pat = "!".
            if  ls_order:delete(1) in frame f_rpt_40_bem_pat_sit_geral_pat
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
** Alterado por..........: tech38629
** Alterado em...........: 06/10/2006 22:40:15
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



               output stream s_1 to value(v_cod_dwb_file)
               paged page-size value(v_qtd_line) convert target 'iso8859-1'.
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

                  if  available b_servid_exec_style
                  and b_servid_exec_style.ind_tip_fila_exec = 'UNIX'
                  then do:
                      &if '{&emsbas_version}' > '1.00' &then           
                      &if '{&emsbas_version}' >= '5.03' &then           
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
                      &else
                          if dwb_rpt_param.cod_livre_1 <> "" then do:
                              if  layout_impres.num_lin_pag = 0
                              then do:
                                  output stream s_1 to value(lc(dwb_rpt_param.cod_livre_1))
                                         page-size 0 convert target tip_imprsor.cod_pag_carac_conver.
                              end /* if */. 
                              else do:
                                  output stream s_1 to value(lc(dwb_rpt_param.cod_livre_1))
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
                      &endif
                      &endif
                  end /* if */.
                  else do:
                      &if '{&emsbas_version}' > '1.00' &then           
                      &if '{&emsbas_version}' >= '5.03' &then           
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
                      &else
                          if dwb_rpt_param.cod_livre_1 <> "" then do:
                              if  layout_impres.num_lin_pag = 0
                              then do:
                                  output stream s_1 to value(lc(dwb_rpt_param.cod_livre_1))
                                         page-size 0 convert target tip_imprsor.cod_pag_carac_conver.                            
                              end /* if */.
                              else do:
                                  output stream s_1 to value(lc(dwb_rpt_param.cod_livre_1))
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
                      &endif
                      &endif
                  end /* else */.

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

    run pi_rpt_bem_pat_sit_geral_pat /*pi_rpt_bem_pat_sit_geral_pat*/.
END PROCEDURE. /* pi_output_reports */
/*****************************************************************************
** Procedure Interna.....: pi_isl_rpt_bem_pat_sit_geral_pat
** Descricao.............: pi_isl_rpt_bem_pat_sit_geral_pat
** Criado por............: Muller
** Criado em.............: 10/11/1995 14:48:24
** Alterado por..........: log352036_2
** Alterado em...........: 21/01/2011 11:52:46
*****************************************************************************/
PROCEDURE pi_isl_rpt_bem_pat_sit_geral_pat:

    /* formato: */
    case v_cod_dwb_field:
        when "Conta Patrimonial" then
            assign v_cod_dat_type = "character"
                   v_cod_format   = "x(18)":U
                   v_cod_initial  = string("":U, v_cod_format)
                   v_cod_final    = string("ZZZZZZZZZZZZZZZZZZ":U, v_cod_format).
        when "Bem Patrimonial" then
            assign v_cod_dat_type = "integer"
                   v_cod_format   = ">>>>>>>>9":U
                   v_cod_initial  = string(0, v_cod_format)
                   v_cod_final    = string(999999999, v_cod_format).
        when "Descriá∆o Bem Pat" then
            assign v_cod_dat_type = "character"
                   v_cod_format   = "x(40)":U
                   v_cod_initial  = string("":U, v_cod_format)
                   v_cod_final    = string("ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ":U, v_cod_format).
        when "Data Aquisiá∆o" then
            assign v_cod_dat_type = "date"
                   v_cod_format   = "99/99/9999":U
                   v_cod_initial  = string(01/01/0001, v_cod_format)
                   v_cod_final    = string(12/31/9999, v_cod_format).
        when "Data C†lculo" then
            assign v_cod_dat_type = "date"
                   v_cod_format   = "99/99/9999":U
                   v_cod_initial  = string(01/01/0001, v_cod_format)
                   v_cod_final    = string(12/31/9999, v_cod_format).
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
        when "Plano Centros Custo" then
            assign v_cod_dat_type = "character"
                   v_cod_format   = "x(8)":U
                   v_cod_initial  = string("":U, v_cod_format)
                   v_cod_final    = string("ZZZZZZZZ":U, v_cod_format).
        when "CCusto Responsab" then
            assign v_cod_dat_type = "Character"
                   v_cod_format   = &IF DEFINED(BF_FIN_AUMENTO_DIGITO_CCUSTO) &THEN 'x(20)':U &ELSE "x(11)":U &ENDIF
                   v_cod_initial  = string("":U, v_cod_format)
                   v_cod_final    = &IF DEFINED(BF_FIN_AUMENTO_DIGITO_CCUSTO) &THEN string('ZZZZZZZZZZZZZZZZZZZZ':U) &ELSE string("ZZZZZZZZZZZ":U) &ENDIF.
    end /* case formato */.
END PROCEDURE. /* pi_isl_rpt_bem_pat_sit_geral_pat */
/*****************************************************************************
** Procedure Interna.....: pi_ler_tt_rpt_bem_pat_sit_geral_pat
** Descricao.............: pi_ler_tt_rpt_bem_pat_sit_geral_pat
** Criado por............: Uno
** Criado em.............: 12/03/1996 08:46:41
** Alterado por..........: src370
** Alterado em...........: 05/05/2001 11:06:43
*****************************************************************************/
PROCEDURE pi_ler_tt_rpt_bem_pat_sit_geral_pat:

    /************************* Variable Definition Begin ************************/

    def var v_val_percentual
        as decimal
        format "->>,>>>,>>>,>>9.99":U
        decimals 2
        label "Percentual"
        no-undo.


    /************************** Variable Definition End *************************/

    selecao:
    for each dwb_rpt_select no-lock
     where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
       and dwb_rpt_select.cod_dwb_user = v_cod_dwb_user /*cl_dwb_rpt_select of dwb_rpt_select*/ use-index dwbrptsl_id:
        if  dwb_rpt_select.log_dwb_rule = yes
        then do:
            run pi_ler_tt_rpt_bem_pat_sit_geral_pat_regra /*pi_ler_tt_rpt_bem_pat_sit_geral_pat_regra*/.
        end /* if */.
        else do:
            run pi_ler_tt_rpt_bem_pat_sit_geral_pat_excec /*pi_ler_tt_rpt_bem_pat_sit_geral_pat_excec*/.
        end /* else */.
    end /* for selecao */.
END PROCEDURE. /* pi_ler_tt_rpt_bem_pat_sit_geral_pat */
/*****************************************************************************
** Procedure Interna.....: pi_tratar_tt_rpt_bem_pat_sit_geral_pat
** Descricao.............: pi_tratar_tt_rpt_bem_pat_sit_geral_pat
** Criado por............: Muller
** Criado em.............: 10/11/1995 14:55:30
** Alterado por..........: corp45760
** Alterado em...........: 06/05/2013 11:04:03
*****************************************************************************/
PROCEDURE pi_tratar_tt_rpt_bem_pat_sit_geral_pat:


       /* Begin_Include: i_exec_program_epc */
       &if '{&emsbas_version}' > '1.00' &then
       if  v_nom_prog_upc <> '' then
       do:
           assign v_rec_table_epc = recid(bem_pat).
           run value(v_nom_prog_upc) (input 'verifica_incorp',
                                      input 'viewer',
                                      input this-procedure,
                                      input v_wgh_frame_epc,
                                      input v_nom_table_epc,
                                      input v_rec_table_epc).
           if  return-value = 'NOK-INCORP' then
               NEXT.
       end.

       if  v_nom_prog_appc <> '' then
       do:
           assign v_rec_table_epc = recid(bem_pat).
           run value(v_nom_prog_appc) (input 'verifica_incorp',
                                       input 'viewer',
                                       input this-procedure,
                                       input v_wgh_frame_epc,
                                       input v_nom_table_epc,
                                       input v_rec_table_epc).
           if  RETURN-VALUE = 'NOK-INCORP' then
               NEXT.
       end.

       &if '{&emsbas_version}' > '5.00' &then
       if  v_nom_prog_dpc <> '' then
       do:
           assign v_rec_table_epc = recid(bem_pat).
           run value(v_nom_prog_dpc) (input 'verifica_incorp',
                                      input 'viewer',
                                      input this-procedure,
                                      INPUT v_wgh_frame_epc,
                                      input v_nom_table_epc,
                                      input v_rec_table_epc).
           if  return-value = 'NOK-INCORP' then
               NEXT.
       end.
       &endif
       &endif
       /* End_Include: i_exec_program_epc */

    assign v_cod_cta_pat = " ".

    find tt_rpt_bem_pat no-lock
         where tt_rpt_bem_pat.ttv_rec_bem_pat = recid(bem_pat) /*cl_key_recid of tt_rpt_bem_pat*/ no-error.
    if  dwb_rpt_select.log_dwb_rule = yes
    then do:
        if  not avail tt_rpt_bem_pat
        then do:
            find first contrat_leas
                where contrat_leas.cod_arrendador        = bem_pat.cod_arrendador
                and   contrat_leas.cod_contrat_leas      = bem_pat.cod_contrat_leas
                and   contrat_leas.dat_term_contrat_leas > v_dat_fim_period
                no-lock no-error.
            if  not avail contrat_leas
            then do: 
                find first movto_bem_pat
                    where movto_bem_pat.num_id_bem_pat  = bem_pat.num_id_bem_pat
                    and   movto_bem_pat.num_seq_incorp_bem_pat = 0
                    and   movto_bem_pat.log_estorn_movto_bem_pat = no
                    and   movto_bem_pat.dat_movto_bem_pat <= v_dat_fim_period
                    no-lock no-error.
                if  avail movto_bem_pat
                then do:

                    find first b_movto_bem_pat no-lock
                    where b_movto_bem_pat.num_id_bem_pat         = bem_pat.num_id_bem_pat
                      and   b_movto_bem_pat.num_seq_incorp_bem_pat = 0
                      and   b_movto_bem_pat.log_estorn_movto_bem_pat = no
                      and   b_movto_bem_pat.dat_movto_bem_pat      > v_dat_fim_period
                      and   b_movto_bem_pat.ind_orig_calc_bem_pat = "Transferància" /*l_transferencia*/ 
                      no-error.
                    if  avail b_movto_bem_pat then do:
                        assign v_cod_plano_ccusto = b_movto_bem_pat.cod_plano_ccusto
                               v_cod_ccusto       = b_movto_bem_pat.cod_ccusto
                               v_cod_unid_negoc   = b_movto_bem_pat.cod_unid_negoc
                               v_cod_estab        = b_movto_bem_pat.cod_estab.                        
                    end.
                    else do:
                        assign v_cod_plano_ccusto = bem_pat.cod_plano_ccusto
                               v_cod_ccusto       = bem_pat.cod_ccusto_respons
                               v_cod_unid_negoc   = bem_pat.cod_unid_negoc
                               v_cod_estab        = bem_pat.cod_estab.
                    end.


                    create tt_rpt_bem_pat.
                    assign tt_rpt_bem_pat.ttv_rec_bem_pat = recid(bem_pat)
                           tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[lookup("Conta Patrimonial", v_cod_order)]       = bem_pat.cod_cta_pat
                           tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[lookup("Bem Patrimonial", v_cod_order)]       = string(bem_pat.num_bem_pat, ">>>>>>>>9":U)
                           tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[lookup("Descriá∆o Bem Pat", v_cod_order)]       = bem_pat.des_bem_pat
                           tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[lookup("Data Aquisiá∆o", v_cod_order)] = string(year(bem_pat.dat_aquis_bem_pat),"9999") +
                                                                                                                             string(month(bem_pat.dat_aquis_bem_pat),"99") +
                                                                                                                             string(day(bem_pat.dat_aquis_bem_pat),"99")
                           tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[lookup("Data C†lculo", v_cod_order)]      = string(year(bem_pat.dat_calc_pat),"9999") +
                                                                                                                             string(month(bem_pat.dat_calc_pat),"99") +
                                                                                                                             string(day(bem_pat.dat_calc_pat),"99")
                           tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[lookup(&IF '{&emsfin_version}' >= '5.01' AND '{&emsfin_version}' <= '5.07' &THEN "Estabelecimento" &ELSE "Estabelecimento" &ENDIF, v_cod_order)]         = v_cod_estab
                           tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[lookup("Plano Centros Custo", v_cod_order)]  = v_cod_plano_ccusto
                           tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[lookup("CCusto Responsab", v_cod_order)]= v_cod_ccusto
                           tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[lookup("Unid Neg¢cio", v_cod_order)]    = v_cod_unid_negoc.
                     assign 
                            tt_rpt_bem_pat.cod_empresa     = bem_pat.cod_empresa
                            tt_rpt_bem_pat.cod_cta_pat     = bem_pat.cod_cta_pat
                            tt_rpt_bem_pat.num_bem_pat     = bem_pat.num_bem_pat
                            tt_rpt_bem_pat.num_seq_bem_pat = bem_pat.num_seq_bem_pat.

                     assign 
    &if '{&emsfin_version}' >= "5.01" &then
                           tt_rpt_bem_pat.des_bem_pat               = trim(bem_pat.des_bem_pat)
    &endif
    &if '{&emsfin_version}' >= "5.01" &then
                           tt_rpt_bem_pat.cod_ident_cop             = bem_pat.cod_ident_cop
    &endif
                           tt_rpt_bem_pat.qtd_cop_gerad             = bem_pat.qtd_cop_gerad
                           tt_rpt_bem_pat.ind_orig_bem              = bem_pat.ind_orig_bem
                           tt_rpt_bem_pat.qtd_bem_pat_represen      = bem_pat.qtd_bem_pat_represen
                           tt_rpt_bem_pat.dat_lim_utiliz            = bem_pat.dat_lim_utiliz
                           tt_rpt_bem_pat.dat_ult_invent            = bem_pat.dat_ult_invent
                           tt_rpt_bem_pat.ind_period_invent         = bem_pat.ind_period_invent
                           tt_rpt_bem_pat.qtd_interv_invent         = bem_pat.qtd_interv_invent
                           tt_rpt_bem_pat.dat_aquis_bem_pat         = bem_pat.dat_aquis_bem_pat
                           tt_rpt_bem_pat.cod_estab                 = bem_pat.cod_estab
    &if '{&emsfin_version}' >= "5.07" &then
                           tt_rpt_bem_pat.cod_estab                 = bem_pat.cod_estab
    &endif
                           tt_rpt_bem_pat.cod_grp_calc              = bem_pat.cod_grp_calc
                           tt_rpt_bem_pat.ind_periodic_invent       = bem_pat.ind_periodic_invent
                           tt_rpt_bem_pat.cb3_ident_visual          = bem_pat.cb3_ident_visual
                           tt_rpt_bem_pat.val_perc_bxa              = bem_pat.val_perc_bxa
                           tt_rpt_bem_pat.cod_espec_bem             = bem_pat.cod_espec_bem
                           tt_rpt_bem_pat.cod_marca                 = bem_pat.cod_marca
                           tt_rpt_bem_pat.cod_modelo                = bem_pat.cod_modelo
                           tt_rpt_bem_pat.cod_licenc_uso            = bem_pat.cod_licenc_uso
                           tt_rpt_bem_pat.cod_especif_tec           = bem_pat.cod_especif_tec
                           tt_rpt_bem_pat.cod_arrendador            = bem_pat.cod_arrendador
                           tt_rpt_bem_pat.cod_contrat_leas          = bem_pat.cod_contrat_leas
                           tt_rpt_bem_pat.cod_estado_fisic_bem_pat  = bem_pat.cod_estado_fisic_bem_pat
                           tt_rpt_bem_pat.cdn_fornecedor            = bem_pat.cdn_fornecedor
                           tt_rpt_bem_pat.cod_localiz               = bem_pat.cod_localiz
                           tt_rpt_bem_pat.cod_usuario               = bem_pat.cod_usuario
                           tt_rpt_bem_pat.cod_plano_ccusto          = bem_pat.cod_plano_ccusto
                           tt_rpt_bem_pat.cod_ccusto_respons        = bem_pat.cod_ccusto_respons
                           tt_rpt_bem_pat.des_anot_tab              = bem_pat.des_anot_tab
                           tt_rpt_bem_pat.cod_unid_negoc            = bem_pat.cod_unid_negoc
                           tt_rpt_bem_pat.cod_imagem                = bem_pat.cod_imagem
                           tt_rpt_bem_pat.dat_calc_pat              = bem_pat.dat_calc_pat
                           tt_rpt_bem_pat.cod_indic_econ            = bem_pat.cod_indic_econ
                           tt_rpt_bem_pat.val_original              = bem_pat.val_original
                           tt_rpt_bem_pat.val_despes_financ         = bem_pat.val_despes_financ
                           tt_rpt_bem_pat.dat_refer                 = bem_pat.dat_refer
                           tt_rpt_bem_pat.log_informa_quant_produz  = bem_pat.log_informa_quant_produz
                           tt_rpt_bem_pat.cod_indic_econ_avaliac    = bem_pat.cod_indic_econ_avaliac
                           tt_rpt_bem_pat.val_avaliac_apol_seguro   = bem_pat.val_avaliac_apol_seguro
                           tt_rpt_bem_pat.dat_avaliac_apol_seguro   = bem_pat.dat_avaliac_apol_seguro
                           tt_rpt_bem_pat.des_narrat_bem_pat        = bem_pat.des_narrat_bem_pat
                           tt_rpt_bem_pat.log_utiliz_alug_inter     = bem_pat.log_utiliz_alug_inter
                           tt_rpt_bem_pat.cod_plano_ccusto_cr       = bem_pat.cod_plano_ccusto_cr
                           tt_rpt_bem_pat.cod_ccusto_cr             = bem_pat.cod_ccusto_cr
                           tt_rpt_bem_pat.cod_unid_negoc_cr         = bem_pat.cod_unid_negoc_cr
                           tt_rpt_bem_pat.num_pessoa_jurid_gartia   = bem_pat.num_pessoa_jurid_gartia
                           tt_rpt_bem_pat.dat_inic_valid_gartia_bem = bem_pat.dat_inic_valid_gartia_bem
                           tt_rpt_bem_pat.dat_fim_valid_gartia_bem  = bem_pat.dat_fim_valid_gartia_bem
                           tt_rpt_bem_pat.des_termo_gartia_bem_pat  = bem_pat.des_termo_gartia_bem_pat
                           tt_rpt_bem_pat.cod_docto_entr            = bem_pat.cod_docto_entr
                           tt_rpt_bem_pat.cod_ser_nota              = bem_pat.cod_ser_nota
                           tt_rpt_bem_pat.num_item_docto_entr       = bem_pat.num_item_docto_entr
                           tt_rpt_bem_pat.cod_img_gartia_bem_pat    = bem_pat.cod_img_gartia_bem_pat
                           tt_rpt_bem_pat.cod_usuar_ult_atualiz     = bem_pat.cod_usuar_ult_atualiz
                           tt_rpt_bem_pat.dat_ult_atualiz           = bem_pat.dat_ult_atualiz
                           tt_rpt_bem_pat.hra_ult_atualiz           = bem_pat.hra_ult_atualiz
                           tt_rpt_bem_pat.ind_sit_bem_pat           = bem_pat.ind_sit_bem_pat
    &if '{&emsfin_version}' >= "5.00" &then
                           tt_rpt_bem_pat.dat_fim_calc_pat          = bem_pat.dat_fim_calc_pat
    &endif
    &if '{&emsfin_version}' >= "5.06" &then
                           tt_rpt_bem_pat.log_bem_imptdo            = bem_pat.log_bem_imptdo
    &endif
    &if '{&emsfin_version}' >= "5.06" &then
                           tt_rpt_bem_pat.log_cr_pis                = bem_pat.log_cr_pis
    &endif
    &if '{&emsfin_version}' >= "5.06" &then
                           tt_rpt_bem_pat.log_cr_cofins             = bem_pat.log_cr_cofins
    &endif
    &if '{&emsfin_version}' >= "5.06" &then
                           tt_rpt_bem_pat.num_parc_pis_cofins       = bem_pat.num_parc_pis_cofins
    &endif
    &if '{&emsfin_version}' >= "5.06" &then
                           tt_rpt_bem_pat.val_cr_pis                = bem_pat.val_cr_pis
    &endif
    &if '{&emsfin_version}' >= "5.06" &then
                           tt_rpt_bem_pat.val_cr_cofins             = bem_pat.val_cr_cofins
    &endif
    &if '{&emsfin_version}' >= "5.06" &then
                           tt_rpt_bem_pat.log_cr_csll               = bem_pat.log_cr_csll
    &endif
    &if '{&emsfin_version}' >= "5.06" &then
                           tt_rpt_bem_pat.num_exerc_cr_csll         = bem_pat.num_exerc_cr_csll
    &endif
    &if '{&emsfin_version}' >= "5.06" &then
                           tt_rpt_bem_pat.log_det_agro              = bem_pat.log_det_agro
    &endif.

                     assign tt_rpt_bem_pat.cod_estab                 = v_cod_estab
                            tt_rpt_bem_pat.cod_plano_ccusto          = v_cod_plano_ccusto
                            tt_rpt_bem_pat.cod_ccusto_respons        = v_cod_ccusto
                            tt_rpt_bem_pat.cod_unid_negoc            = v_cod_unid_negoc
                            tt_rpt_bem_pat.num_id_bem_pat = bem_pat.num_id_bem_pat.
                     if  v_log_quant_bem then do:
                         blk_movto_bem_pat:
                         for each movto_bem_pat no-lock  where 
                                 movto_bem_pat.num_id_bem_pat = bem_pat.num_id_bem_pat and
                    	     movto_bem_pat.num_seq_incorp_bem_pat = 0 and
                                 movto_bem_pat.ind_trans_calc_bem_pat = "Baixa" /*l_baixa*/  and
                                 movto_bem_pat.dat_movto_bem_pat > v_dat_fim_period:
                             assign tt_rpt_bem_pat.qtd_bem_pat_represen      = tt_rpt_bem_pat.qtd_bem_pat_represen +         
                                                                                movto_bem_pat.qtd_movto_bem_pat.
                         end /* for blk_movto_bem_pat */.
                     end.
                end /* if */.
            end /* if */.
            else do: 
                if  v_log_bem_imobdo = yes
                then do:
                    create tt_rpt_bem_pat.
                    assign tt_rpt_bem_pat.ttv_rec_bem_pat = recid(bem_pat)
                           tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[lookup("Conta Patrimonial", v_cod_order)]       = bem_pat.cod_cta_pat
                           tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[lookup("Bem Patrimonial", v_cod_order)]       = string(bem_pat.num_bem_pat, ">>>>>>>>9":U)
                           tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[lookup("Descriá∆o Bem Pat", v_cod_order)]       = bem_pat.des_bem_pat
                           tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[lookup("Data Aquisiá∆o", v_cod_order)] = string(year(bem_pat.dat_aquis_bem_pat),"9999") +
                                                                                                                             string(month(bem_pat.dat_aquis_bem_pat),"99") +
                                                                                                                             string(day(bem_pat.dat_aquis_bem_pat),"99")
                           tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[lookup("Data C†lculo", v_cod_order)]      = string(year(bem_pat.dat_calc_pat),"9999") +
                                                                                                                             string(month(bem_pat.dat_calc_pat),"99") +
                                                                                                                             string(day(bem_pat.dat_calc_pat),"99")
                           tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[lookup(&IF '{&emsfin_version}' >= '5.01' AND '{&emsfin_version}' <= '5.07' &THEN "Estabelecimento" &ELSE "Estabelecimento" &ENDIF, v_cod_order)]         = bem_pat.cod_estab
                           tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[lookup("Plano Centros Custo", v_cod_order)]  = bem_pat.cod_plano_ccusto
                           tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[lookup("CCusto Responsab", v_cod_order)]= bem_pat.cod_ccusto_respons
                           tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[lookup("Unid Neg¢cio", v_cod_order)]    = bem_pat.cod_unid_negoc.
                     assign 
                            tt_rpt_bem_pat.cod_empresa     = bem_pat.cod_empresa
                            tt_rpt_bem_pat.cod_cta_pat     = bem_pat.cod_cta_pat
                            tt_rpt_bem_pat.num_bem_pat     = bem_pat.num_bem_pat
                            tt_rpt_bem_pat.num_seq_bem_pat = bem_pat.num_seq_bem_pat.

                     assign 
    &if '{&emsfin_version}' >= "5.01" &then
                           tt_rpt_bem_pat.des_bem_pat               = trim(bem_pat.des_bem_pat)
    &endif
    &if '{&emsfin_version}' >= "5.01" &then
                           tt_rpt_bem_pat.cod_ident_cop             = bem_pat.cod_ident_cop
    &endif
                           tt_rpt_bem_pat.qtd_cop_gerad             = bem_pat.qtd_cop_gerad
                           tt_rpt_bem_pat.ind_orig_bem              = bem_pat.ind_orig_bem
                           tt_rpt_bem_pat.qtd_bem_pat_represen      = bem_pat.qtd_bem_pat_represen
                           tt_rpt_bem_pat.dat_lim_utiliz            = bem_pat.dat_lim_utiliz
                           tt_rpt_bem_pat.dat_ult_invent            = bem_pat.dat_ult_invent
                           tt_rpt_bem_pat.ind_period_invent         = bem_pat.ind_period_invent
                           tt_rpt_bem_pat.qtd_interv_invent         = bem_pat.qtd_interv_invent
                           tt_rpt_bem_pat.dat_aquis_bem_pat         = bem_pat.dat_aquis_bem_pat
                           tt_rpt_bem_pat.cod_estab                 = bem_pat.cod_estab
    &if '{&emsfin_version}' >= "5.07" &then
                           tt_rpt_bem_pat.cod_estab                 = bem_pat.cod_estab
    &endif
                           tt_rpt_bem_pat.cod_grp_calc              = bem_pat.cod_grp_calc
                           tt_rpt_bem_pat.ind_periodic_invent       = bem_pat.ind_periodic_invent
                           tt_rpt_bem_pat.cb3_ident_visual          = bem_pat.cb3_ident_visual
                           tt_rpt_bem_pat.val_perc_bxa              = bem_pat.val_perc_bxa
                           tt_rpt_bem_pat.cod_espec_bem             = bem_pat.cod_espec_bem
                           tt_rpt_bem_pat.cod_marca                 = bem_pat.cod_marca
                           tt_rpt_bem_pat.cod_modelo                = bem_pat.cod_modelo
                           tt_rpt_bem_pat.cod_licenc_uso            = bem_pat.cod_licenc_uso
                           tt_rpt_bem_pat.cod_especif_tec           = bem_pat.cod_especif_tec
                           tt_rpt_bem_pat.cod_arrendador            = bem_pat.cod_arrendador
                           tt_rpt_bem_pat.cod_contrat_leas          = bem_pat.cod_contrat_leas
                           tt_rpt_bem_pat.cod_estado_fisic_bem_pat  = bem_pat.cod_estado_fisic_bem_pat
                           tt_rpt_bem_pat.cdn_fornecedor            = bem_pat.cdn_fornecedor
                           tt_rpt_bem_pat.cod_localiz               = bem_pat.cod_localiz
                           tt_rpt_bem_pat.cod_usuario               = bem_pat.cod_usuario
                           tt_rpt_bem_pat.cod_plano_ccusto          = bem_pat.cod_plano_ccusto
                           tt_rpt_bem_pat.cod_ccusto_respons        = bem_pat.cod_ccusto_respons
                           tt_rpt_bem_pat.des_anot_tab              = bem_pat.des_anot_tab
                           tt_rpt_bem_pat.cod_unid_negoc            = bem_pat.cod_unid_negoc
                           tt_rpt_bem_pat.cod_imagem                = bem_pat.cod_imagem
                           tt_rpt_bem_pat.dat_calc_pat              = bem_pat.dat_calc_pat
                           tt_rpt_bem_pat.cod_indic_econ            = bem_pat.cod_indic_econ
                           tt_rpt_bem_pat.val_original              = bem_pat.val_original
                           tt_rpt_bem_pat.val_despes_financ         = bem_pat.val_despes_financ
                           tt_rpt_bem_pat.dat_refer                 = bem_pat.dat_refer
                           tt_rpt_bem_pat.log_informa_quant_produz  = bem_pat.log_informa_quant_produz
                           tt_rpt_bem_pat.cod_indic_econ_avaliac    = bem_pat.cod_indic_econ_avaliac
                           tt_rpt_bem_pat.val_avaliac_apol_seguro   = bem_pat.val_avaliac_apol_seguro
                           tt_rpt_bem_pat.dat_avaliac_apol_seguro   = bem_pat.dat_avaliac_apol_seguro
                           tt_rpt_bem_pat.des_narrat_bem_pat        = bem_pat.des_narrat_bem_pat
                           tt_rpt_bem_pat.log_utiliz_alug_inter     = bem_pat.log_utiliz_alug_inter
                           tt_rpt_bem_pat.cod_plano_ccusto_cr       = bem_pat.cod_plano_ccusto_cr
                           tt_rpt_bem_pat.cod_ccusto_cr             = bem_pat.cod_ccusto_cr
                           tt_rpt_bem_pat.cod_unid_negoc_cr         = bem_pat.cod_unid_negoc_cr
                           tt_rpt_bem_pat.num_pessoa_jurid_gartia   = bem_pat.num_pessoa_jurid_gartia
                           tt_rpt_bem_pat.dat_inic_valid_gartia_bem = bem_pat.dat_inic_valid_gartia_bem
                           tt_rpt_bem_pat.dat_fim_valid_gartia_bem  = bem_pat.dat_fim_valid_gartia_bem
                           tt_rpt_bem_pat.des_termo_gartia_bem_pat  = bem_pat.des_termo_gartia_bem_pat
                           tt_rpt_bem_pat.cod_docto_entr            = bem_pat.cod_docto_entr
                           tt_rpt_bem_pat.cod_ser_nota              = bem_pat.cod_ser_nota
                           tt_rpt_bem_pat.num_item_docto_entr       = bem_pat.num_item_docto_entr
                           tt_rpt_bem_pat.cod_img_gartia_bem_pat    = bem_pat.cod_img_gartia_bem_pat
                           tt_rpt_bem_pat.cod_usuar_ult_atualiz     = bem_pat.cod_usuar_ult_atualiz
                           tt_rpt_bem_pat.dat_ult_atualiz           = bem_pat.dat_ult_atualiz
                           tt_rpt_bem_pat.hra_ult_atualiz           = bem_pat.hra_ult_atualiz
                           tt_rpt_bem_pat.ind_sit_bem_pat           = bem_pat.ind_sit_bem_pat
    &if '{&emsfin_version}' >= "5.00" &then
                           tt_rpt_bem_pat.dat_fim_calc_pat          = bem_pat.dat_fim_calc_pat
    &endif
    &if '{&emsfin_version}' >= "5.06" &then
                           tt_rpt_bem_pat.log_bem_imptdo            = bem_pat.log_bem_imptdo
    &endif
    &if '{&emsfin_version}' >= "5.06" &then
                           tt_rpt_bem_pat.log_cr_pis                = bem_pat.log_cr_pis
    &endif
    &if '{&emsfin_version}' >= "5.06" &then
                           tt_rpt_bem_pat.log_cr_cofins             = bem_pat.log_cr_cofins
    &endif
    &if '{&emsfin_version}' >= "5.06" &then
                           tt_rpt_bem_pat.num_parc_pis_cofins       = bem_pat.num_parc_pis_cofins
    &endif
    &if '{&emsfin_version}' >= "5.06" &then
                           tt_rpt_bem_pat.val_cr_pis                = bem_pat.val_cr_pis
    &endif
    &if '{&emsfin_version}' >= "5.06" &then
                           tt_rpt_bem_pat.val_cr_cofins             = bem_pat.val_cr_cofins
    &endif
    &if '{&emsfin_version}' >= "5.06" &then
                           tt_rpt_bem_pat.log_cr_csll               = bem_pat.log_cr_csll
    &endif
    &if '{&emsfin_version}' >= "5.06" &then
                           tt_rpt_bem_pat.num_exerc_cr_csll         = bem_pat.num_exerc_cr_csll
    &endif
    &if '{&emsfin_version}' >= "5.06" &then
                           tt_rpt_bem_pat.log_det_agro              = bem_pat.log_det_agro
    &endif.

                     assign tt_rpt_bem_pat.num_id_bem_pat = bem_pat.num_id_bem_pat.
                     if  v_log_quant_bem then do:
                         blk_movto_bem_pat:
                         for each movto_bem_pat no-lock  where 
                                 movto_bem_pat.num_id_bem_pat = bem_pat.num_id_bem_pat and
                    	     movto_bem_pat.num_seq_incorp_bem_pat = 0 and
                                 movto_bem_pat.ind_trans_calc_bem_pat = "Baixa" /*l_baixa*/  and
                                 movto_bem_pat.dat_movto_bem_pat > v_dat_fim_period:
                             assign tt_rpt_bem_pat.qtd_bem_pat_represen      = tt_rpt_bem_pat.qtd_bem_pat_represen +
                                                                                movto_bem_pat.qtd_movto_bem_pat.
                         end /* for blk_movto_bem_pat */.
                     end.
                 end /* if */.            
            end /* else */.    
        end /* if */.
    end /* if */.
    else do:
        if  avail tt_rpt_bem_pat
        then do:
            delete tt_rpt_bem_pat.
        end /* if */.
    end /* else */.

END PROCEDURE. /* pi_tratar_tt_rpt_bem_pat_sit_geral_pat */
/*****************************************************************************
** Procedure Interna.....: pi_rpt_bem_pat_sit_geral_pat
** Descricao.............: pi_rpt_bem_pat_sit_geral_pat
** Criado por............: Muller
** Criado em.............: 10/11/1995 14:57:36
** Alterado por..........: fut35059
** Alterado em...........: 26/10/2005 15:47:04
*****************************************************************************/
PROCEDURE pi_rpt_bem_pat_sit_geral_pat:

    /************************* Variable Definition Begin ************************/

    def var v_cod_cta_pat
        as character
        format "x(18)":U
        label "Conta Patrimonial"
        column-label "Conta Patrimonial"
        no-undo.
    def var v_dat_aquis_bem_pat
        as date
        format "99/99/9999":U
        label "Data Aquisiá∆o"
        column-label "Data Aquisiá∆o"
        no-undo.
    def var v_num_seq_incorp_bem_pat
        as integer
        format ">>>>,>>9":U
        initial 0
        label "Seq Incorp Bem"
        column-label "Seq Incorp Bem"
        no-undo.
    def var v_num_dec_aux                    as integer         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    if  num-entries(dwb_rpt_param.cod_dwb_parameters, chr(10)) > 7
    then do:
        assign v_cod_order = dwb_rpt_param.cod_dwb_order
               v_cod_cenar_ctbl = entry(1, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_cod_finalid_econ = entry(2, dwb_rpt_param.cod_dwb_parameters, chr(10))            
               v_dat_fim_period = date(entry(4, dwb_rpt_param.cod_dwb_parameters, chr(10)))
               v_log_consid_bem_bxado = (entry(5, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "Sim" /*l_sim*/ )
               v_log_resum_cta = (entry(6, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "Sim" /*l_sim*/ )
               v_log_impr_narrat = (entry(7, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "Sim" /*l_sim*/ )
               v_log_bem_imobdo = (entry(8, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "Sim" /*l_sim*/ ).
    end /* if */.
    for each tt_cta_pat_sit_geral exclusive-lock:
        delete tt_cta_pat_sit_geral.
    end.
    run pi_ler_tt_rpt_bem_pat_sit_geral_pat /*pi_ler_tt_rpt_bem_pat_sit_geral_pat*/.

    IF dwb_rpt_param.cod_dwb_output <> "Excel" THEN DO:
        hide stream s_1 frame f_rpt_s_1_header_unique.
        view stream s_1 frame f_rpt_s_1_header_period.
        hide stream s_1 frame f_rpt_s_1_footer_last_page.
        hide stream s_1 frame f_rpt_s_1_footer_param_page.
        view stream s_1 frame f_rpt_s_1_footer_normal.

        if  entry(1, dwb_rpt_param.cod_dwb_order) = "Bem Patrimonial"
        then do:
            if  v_log_quant_bem then do:
                if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "Bem Pat" to 9
                    "Seq" to 15
                    "Conta Pat" at 17
                    "Descriá∆o" at 36
                    "Data Aquis" at 77
                    "Data Calc" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                    "Est" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                    "Est" at 99
        &ENDIF
                    "Un N" at 105
                    "Data Saldo" at 110
                    "Plano CCusto" at 121
                    "CC Respons" at 134
                    "Valor Imobilizado" at 148
                    "Valor Depreciado" at 172
                    "Valor Amortizado" at 197
                    "Valor Liquido" at 222
                    "Quantidade" to 255 skip
                    "---------" to 9
                    "-----" to 15
                    "------------------" at 17
                    "----------------------------------------" at 36
                    "----------" at 77
                    "----------" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                    "---" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                    "-----" at 99
        &ENDIF
                    "----" at 105
                    "----------" at 110
                    "------------" at 121
                    "-----------" at 134
                    "-----------------------" at 148
                    "-----------------------" at 172
                    "-----------------------" at 197
                    "-----------------------" at 222
                    "----------" to 255 skip.
            end.
            else do:
                if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "Bem Pat" to 9
                    "Seq" to 15
                    "Descriá∆o" at 36
                    "Data Aquis" at 77
                    "Data Calc" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                    "Est" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                    "Est" at 99
        &ENDIF
                    "Un N" at 105
                    "Plano CCusto" at 121
                    "CC Respons" at 134
                    "Valor Imobilizado" at 148
                    "Valor Depreciado" at 172
                    "Valor Amortizado" at 197
                    "Valor Liquido" at 222 skip
                    "---------" to 9
                    "-----" to 15
                    "----------------------------------------" at 36
                    "----------" at 77
                    "----------" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                    "---" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                    "-----" at 99
        &ENDIF
                    "----" at 105
                    "------------" at 121
                    "-----------" at 134
                    "-----------------------" at 148
                    "-----------------------" at 172
                    "-----------------------" at 197
                    "-----------------------" at 222 skip.
            end.
        end /* if */.
        else do:
            if  entry(1, dwb_rpt_param.cod_dwb_order) = "Descriá∆o Bem Pat"
            then do:
                if  v_log_quant_bem then do:
                    if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "Descriá∆o" at 1
                        "Bem Pat" to 50
                        "Seq" to 56
                        "Conta Pat" at 58
                        "Data Aquis" at 77
                        "Data Calc" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                        "Est" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        "Est" at 99
        &ENDIF
                        "Un Neg" at 105
                        "Data Saldo" at 112
                        "Plano CCusto" at 123
                        "CC Respons" at 136
                        "Valor Imobilizado" at 148
                        "Valor Depreciado" at 172
                        "Valor Amortizado" at 197
                        "Valor Liquido" at 222
                        "Quantidade" to 255 skip
                        "----------------------------------------" at 1
                        "---------" to 50
                        "-----" to 56
                        "------------------" at 58
                        "----------" at 77
                        "----------" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                        "---" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        "-----" at 99
        &ENDIF
                        "------" at 105
                        "----------" at 112
                        "------------" at 123
                        "-----------" at 136
                        "-----------------------" at 148
                        "-----------------------" at 172
                        "-----------------------" at 197
                        "-----------------------" at 222
                        "----------" to 255 skip.
                end.
                else do:
                    if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "Descriá∆o" at 1
                        "Bem Pat" to 50
                        "Seq" to 56
                        "Data Aquis" at 77
                        "Data Calc" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                        "Est" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        "Est" at 99
        &ENDIF
                        "Un Neg" at 105
                        "Plano CCusto" at 123
                        "CC Respons" at 136
                        "Valor Imobilizado" at 148
                        "Valor Depreciado" at 172
                        "Valor Amortizado" at 197
                        "Valor Liquido" at 222 skip
                        "----------------------------------------" at 1
                        "---------" to 50
                        "-----" to 56
                        "----------" at 77
                        "----------" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                        "---" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        "-----" at 99
        &ENDIF
                        "------" at 105
                        "------------" at 123
                        "-----------" at 136
                        "-----------------------" at 148
                        "-----------------------" at 172
                        "-----------------------" at 197
                        "-----------------------" at 222 skip.
                end.
            end /* if */.
            else do:
                if  entry(1, dwb_rpt_param.cod_dwb_order) = "Data Aquisiá∆o"
                then do:
                    if  v_log_quant_bem then do:
                        if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            "Data Aquis" at 1
                            "Conta Pat" at 12
                            "Bem Pat" to 39
                            "Seq" to 45
                            "Descriá∆o" at 47
                            "Data Calc" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            "Est" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            "Est" at 99
        &ENDIF
                            "Un Neg" at 105
                            "Data Saldo" at 112
                            "Plano CCusto" at 123
                            "CC Respons" at 136
                            "Valor Imobilizado" at 148
                            "Valor Depreciado" at 172
                            "Valor Amortizado" at 197
                            "Valor Liquido" at 222
                            "Quantidade" to 255 skip
                            "----------" at 1
                            "------------------" at 12
                            "---------" to 39
                            "-----" to 45
                            "----------------------------------------" at 47
                            "----------" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            "---" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            "-----" at 99
        &ENDIF
                            "------" at 105
                            "----------" at 112
                            "------------" at 123
                            "-----------" at 136
                            "-----------------------" at 148
                            "-----------------------" at 172
                            "-----------------------" at 197
                            "-----------------------" at 222
                            "----------" to 255 skip.
                    end.
                    else do:
                        if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            "Data Aquis" at 1
                            "Bem Pat" to 39
                            "Seq" to 45
                            "Descriá∆o" at 47
                            "Data Calc" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            "Est" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            "Est" at 99
        &ENDIF
                            "Un Neg" at 105
                            "Plano CCusto" at 123
                            "CC Respons" at 136
                            "Valor Imobilizado" at 148
                            "Valor Depreciado" at 172
                            "Valor Amortizado" at 197
                            "Valor Liquido" at 222 skip
                            "----------" at 1
                            "---------" to 39
                            "-----" to 45
                            "----------------------------------------" at 47
                            "----------" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            "---" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            "-----" at 99
        &ENDIF
                            "------" at 105
                            "------------" at 123
                            "-----------" at 136
                            "-----------------------" at 148
                            "-----------------------" at 172
                            "-----------------------" at 197
                            "-----------------------" at 222 skip.
                    end.
                end /* if */.
                else do:
                    if  entry(1, dwb_rpt_param.cod_dwb_order) = "Data C†lculo"
                    then do:
                        if  v_log_quant_bem then do:
                            if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                                page stream s_1.
                            put stream s_1 unformatted 
                                "Data Calc" at 1
                                "Conta Pat" at 12
                                "Bem Pat" to 39
                                "Seq" to 45
                                "Descriá∆o" at 47
                                "Data Aquis" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                                "Est" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                                "Est" at 99
        &ENDIF
                                "Un Neg" at 105
                                "Data Saldo" at 112
                                "Plano CCusto" at 123
                                "CC Respons" at 136
                                "Valor Imobilizado" at 148
                                "Valor Depreciado" at 172
                                "Valor Amortizado" at 197
                                "Valor Liquido" at 222
                                "Quantidade" to 255 skip
                                "----------" at 1
                                "------------------" at 12
                                "---------" to 39
                                "-----" to 45
                                "----------------------------------------" at 47
                                "----------" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                                "---" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                                "-----" at 99
        &ENDIF
                                "------" at 105
                                "----------" at 112
                                "------------" at 123
                                "-----------" at 136
                                "-----------------------" at 148
                                "-----------------------" at 172
                                "-----------------------" at 197
                                "-----------------------" at 222
                                "----------" to 255 skip.
                        end.
                        else do:
                            if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                                page stream s_1.
                            put stream s_1 unformatted 
                                "Data Calc" at 1
                                "Bem Pat" to 39
                                "Seq" to 45
                                "Descriá∆o" at 47
                                "Data Aquis" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                                "Est" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                                "Est" at 99
        &ENDIF
                                "Un Neg" at 105
                                "Plano CCusto" at 123
                                "CC Respons" at 136
                                "Valor Imobilizado" at 148
                                "Valor Depreciado" at 172
                                "Valor Amortizado" at 197
                                "Valor Liquido" at 222 skip
                                "----------" at 1
                                "---------" to 39
                                "-----" to 45
                                "----------------------------------------" at 47
                                "----------" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                                "---" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                                "-----" at 99
        &ENDIF
                                "------" at 105
                                "------------" at 123
                                "-----------" at 136
                                "-----------------------" at 148
                                "-----------------------" at 172
                                "-----------------------" at 197
                                "-----------------------" at 222 skip.
                        end.
                    end /* if */.
                end /* else */.
            end /* else */.
        end /* else */.
    END.
    ELSE DO:
        /*
        /* ---> Cria objeto excel <--- */
        CREATE "Excel.Application" chExcelApplication.

        /* ---> Adiciona o modelo do documento <--- */
        chWorkbook  = chExcelApplication:Workbooks:ADD().
        chWorkSheet = chExcelApplication:Sheets:ITEM(1).

        ASSIGN i-linha = 1.
        */

        ASSIGN cArquivo = SESSION:TEMP-DIRECTORY + "ESFAS004-" + STRING(TIME) + ".csv".
        OUTPUT TO VALUE(cArquivo) NO-CONVERT.

        /*
        /* Cabeáalho */
        ASSIGN chExcelApplication:Range("A" + STRING (i-linha)):VALUE = "Conta"
               chExcelApplication:Range("B" + STRING (i-linha)):VALUE = "Descriá∆o Conta"
               chExcelApplication:Range("C" + STRING (i-linha)):VALUE = "Bem"
               chExcelApplication:Range("D" + STRING (i-linha)):VALUE = "Seq"
               chExcelApplication:Range("E" + STRING (i-linha)):VALUE = "Descriá∆o"
               chExcelApplication:Range("F" + STRING (i-linha)):VALUE = "Unid Negoc"
               chExcelApplication:Range("G" + STRING (i-linha)):VALUE = "Estab"
               chExcelApplication:Range("H" + STRING (i-linha)):VALUE = "Plano CCusto"
               chExcelApplication:Range("I" + STRING (i-linha)):VALUE = "CCusto"
               chExcelApplication:Range("J" + STRING (i-linha)):VALUE = "Data Aquisiá∆o"
               chExcelApplication:Range("K" + STRING (i-linha)):VALUE = "Data Inic. Calc"
               chExcelApplication:Range("L" + STRING (i-linha)):VALUE = "Grupo Calc."      
               chExcelApplication:Range("M" + STRING (i-linha)):VALUE = "Fornec"           
               chExcelApplication:Range("N" + STRING (i-linha)):VALUE = "% Depre Acum"     
               chExcelApplication:Range("O" + STRING (i-linha)):VALUE = "Valor Custo"
               chExcelApplication:Range("P" + STRING (i-linha)):VALUE = "Depr. Acumulada"  
               chExcelApplication:Range("Q" + STRING (i-linha)):VALUE = "Amort. Acumulada"  
               chExcelApplication:Range("R" + STRING (i-linha)):VALUE = "Valor Liquido"    
               chExcelApplication:Range("S" + STRING (i-linha)):VALUE = "Valor Residual"   
               chExcelApplication:Range("T" + STRING (i-linha)):VALUE = "Base Calculo Deprec."
               chExcelApplication:Range("U" + STRING (i-linha)):VALUE = "% Anual Depre"
               chExcelApplication:Range("V" + STRING (i-linha)):VALUE = "Data Calculo"
               chExcelApplication:Range("W" + STRING (i-linha)):VALUE = "Plaqueta"
               chExcelApplication:Range("X" + STRING (i-linha)):VALUE = "Placa"
               chExcelApplication:Range("Y" + STRING (i-linha)):VALUE = "Renavam"
               chExcelApplication:Range("Z" + STRING (i-linha)):VALUE = "Chassi"
               chExcelApplication:Range("AA" + STRING (i-linha)):VALUE = "Narrativa"
               .
               */

        /*
        /* Cabeáalho */
        ASSIGN chExcelApplication:Range("A" + STRING (i-linha)):VALUE  = "Empresa                "
               chExcelApplication:Range("B" + STRING (i-linha)):VALUE  = "Conta Patrimonial      "
               chExcelApplication:Range("C" + STRING (i-linha)):VALUE  = "Descriá∆o Conta Pat    "
               chExcelApplication:Range("D" + STRING (i-linha)):VALUE  = "Bem Patrimonial        "
               chExcelApplication:Range("E" + STRING (i-linha)):VALUE  = "Sequància Bem          "
               chExcelApplication:Range("F" + STRING (i-linha)):VALUE  = "Identificaá∆o Bem      "
               chExcelApplication:Range("G" + STRING (i-linha)):VALUE  = "Descriá∆o Bem Pat      "
               chExcelApplication:Range("H" + STRING (i-linha)):VALUE  = "Quantidade             "
               chExcelApplication:Range("I" + STRING (i-linha)):VALUE  = "Data Aquisiá∆o         "
               chExcelApplication:Range("J" + STRING (i-linha)):VALUE  = "Estabel                "
               chExcelApplication:Range("K" + STRING (i-linha)):VALUE  = "Grupo C†lculo          "
               chExcelApplication:Range("L" + STRING (i-linha)):VALUE  = "N£mero Plaqueta        "
               chExcelApplication:Range("M" + STRING (i-linha)):VALUE  = "EspÇcie Bem Patrimonia "
               chExcelApplication:Range("N" + STRING (i-linha)):VALUE  = "Marca                  "
               chExcelApplication:Range("O" + STRING (i-linha)):VALUE  = "Modelo                 "
               chExcelApplication:Range("P" + STRING (i-linha)):VALUE  = "Licenáa Uso            "
               chExcelApplication:Range("Q" + STRING (i-linha)):VALUE  = "Especificaá∆o TÇcnica  "
               chExcelApplication:Range("R" + STRING (i-linha)):VALUE  = "Estado F°sico          "
               chExcelApplication:Range("S" + STRING (i-linha)):VALUE  = "Fornecedor             "
               chExcelApplication:Range("T" + STRING (i-linha)):VALUE  = "Nome Fornec            "
               chExcelApplication:Range("U" + STRING (i-linha)):VALUE  = "Docto Entrada          "
               chExcelApplication:Range("V" + STRING (i-linha)):VALUE  = "SÇrie Nota             "
               chExcelApplication:Range("W" + STRING (i-linha)):VALUE  = "Numero Item            "
               chExcelApplication:Range("X" + STRING (i-linha)):VALUE  = "Localizaá∆o            "
               chExcelApplication:Range("Y" + STRING (i-linha)):VALUE  = "Usu†rio                "
               chExcelApplication:Range("Z" + STRING (i-linha)):VALUE  = "Nome Usu†rio           "
               chExcelApplication:Range("AA" + STRING (i-linha)):VALUE = "Plano Centros Custo    " 
               chExcelApplication:Range("AB" + STRING (i-linha)):VALUE = "CCusto                 " 
               chExcelApplication:Range("AC" + STRING (i-linha)):VALUE = "Unid Neg¢cio           "
               chExcelApplication:Range("AD" + STRING (i-linha)):VALUE = "Desc Unid Negoc        " 
               chExcelApplication:Range("AE" + STRING (i-linha)):VALUE = "Imagem                 " 
               chExcelApplication:Range("AF" + STRING (i-linha)):VALUE = "Calculado AtÇ          " 
               chExcelApplication:Range("AG" + STRING (i-linha)):VALUE = "VL Corrigido R$        " 
               chExcelApplication:Range("AH" + STRING (i-linha)):VALUE = "Deprec. Acum R$        " 
               chExcelApplication:Range("AI" + STRING (i-linha)):VALUE = "Taxa Deprec Anual R$   " 
               chExcelApplication:Range("AJ" + STRING (i-linha)):VALUE = "VL Corrigido US$       " 
               chExcelApplication:Range("AK" + STRING (i-linha)):VALUE = "Deprec. Acum US$       " 
               chExcelApplication:Range("AL" + STRING (i-linha)):VALUE = "Taxa Deprec Anual US$  " 
               chExcelApplication:Range("AM" + STRING (i-linha)):VALUE = "Bem Importado          " 
               chExcelApplication:Range("AN" + STRING (i-linha)):VALUE = "Credita PIS            " 
               chExcelApplication:Range("AO" + STRING (i-linha)):VALUE = "Credita COFINS         " 
               chExcelApplication:Range("AP" + STRING (i-linha)):VALUE = "Nr Parcelas            " 
               chExcelApplication:Range("AQ" + STRING (i-linha)):VALUE = "Valor Cred PIS/PASEP   " 
               chExcelApplication:Range("AR" + STRING (i-linha)):VALUE = "Valor CrÇdito COFINS   " 
               chExcelApplication:Range("AS" + STRING (i-linha)):VALUE = "Credita CSLL           "
               chExcelApplication:Range("AT" + STRING (i-linha)):VALUE = "Exercicios CrÇd CSLL   "
               chExcelApplication:Range("AU" + STRING (i-linha)):VALUE = "Narrativa Bem          "
               .

        ASSIGN i-linha = i-linha + 1.
        */

        /* Cabeáalho */
        PUT UNFORMATTED 
        "Empresa                "
        ";Conta Patrimonial      "
        ";Descriá∆o Conta Pat    "
        ";Bem Patrimonial        "
        ";Sequància Bem          "
        ";Identificaá∆o Bem      "
        ";Descriá∆o Bem Pat      "
        ";Quantidade             "
        ";Data Aquisiá∆o         "
        ";Estabel                "
        ";Grupo C†lculo          "
        ";N£mero Plaqueta        "
        ";EspÇcie Bem Patrimonia "
        ";Marca                  "
        ";Modelo                 "
        ";Licenáa Uso            "
        ";Especificaá∆o TÇcnica  "
        ";Estado F°sico          "
        ";Fornecedor             "
        ";CNPJ                   "
        ";Docto Entrada          "
        ";SÇrie Nota             "
        ";Numero Item            "
        ";Localizaá∆o            "
        ";Usu†rio                "
        ";Nome Usu†rio           "
        ";Plano Centros Custo    " 
        ";CCusto                 " 
        ";Unid Neg¢cio           "
        ";Desc Unid Negoc        " 
        ";Imagem                 " 
        ";Calculado AtÇ          " 
        ";VL Corrigido R$        " 
        ";Deprec. Acum R$        " 
        ";Taxa Deprec Anual R$   " 
        ";VL Corrigido US$       " 
        ";Deprec. Acum US$       " 
        ";Taxa Deprec Anual US$  " 
        ";Bem Importado          " 
        ";Credita PIS            " 
        ";Credita COFINS         " 
        ";Nr Parcelas            " 
        ";Valor Cred PIS/PASEP   " 
        ";Valor CrÇdito COFINS   " 
        ";Credita CSLL           "
        ";Exercicios CrÇd CSLL   "
        ";Narrativa Bem          "
        SKIP.
    END.

    assign v_val_tot_imobdo_aux = 0  v_val_tot_geral_imobdo_aux = 0 v_val_tot_geral_origin_aux = 0
           v_val_tot_geral_cm = 0  v_val_tot_dpr_aux = 0 v_val_tot_geral_dpr_aux = 0
           v_val_tot_liq_bem_pat_aux = 0  v_val_tot_geral_liq_bem_pat_aux = 0 v_val_tot_amort_aux = 0
           v_val_tot_geral_amort_aux = 0  v_val_tot_liq_bem_pat_amort = 0  v_val_tot_geral_liq_bem_amort = 0
           v_cod_cta_pat = ? v_num_bem_pat = ? v_dat_aquis_bem_pat = ? v_dat_calc_pat = ?
           v_log_cabec_relat = no v_log_rodap_relat = no v_log_impr_relat = no  v_qtd_tot_bem_pat = 0
           v_qtd_tot_geral_bem_pat = 0
           v_num_dec = 2.

    /* ******************************/
    if  entry(1,v_cod_order) = "Bem Patrimonial" /*l_bem_patrimonial*/ 
    then do:
        grp_block:
        for each tt_rpt_bem_pat no-lock
            break by tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[1]  by tt_rpt_bem_pat.num_seq_bem_pat
                  by tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[2]
                  by tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[3]  by tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[4]
                  by tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[5]  by tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[6]
                  by tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[7]  by tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[8] :

        /* Begin_Include: i_rpt_bem_pat_sit_geral_pat */
            if first-of(tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[1]) then do:
                assign v_des_val_imobdo_orig = string(0, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec))
                       v_des_val_dpr_aux = string(0, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec))
                       v_des_val_amort_aux = string(0, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec))
                       v_des_val_liq_bem_pat_aux = string(0, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec))
                       v_des_val_tot_imobdo_aux = string(0, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec))
                       v_des_val_tot_dpr_aux = string(0, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec))
                       v_des_val_tot_amort_aux = string(0, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec))
                       v_des_val_tot_liq_bem_pat_aux = string(0, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec))
                       v_des_val_tot_geral_imobdo_aux = string(0, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec))
                       v_des_val_tot_geral_dpr_aux = string(0, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec))
                       v_des_val_tot_geral_amort_aux = string(0, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec))
                       v_des_val_tot_geral_liq_bem_pat = string(0, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec))
                       v_val_tot_imobdo_aux = 0 v_val_tot_dpr_aux = 0 v_val_tot_liq_bem_pat_aux = 0 v_val_tot_amort_aux = 0.
            end.               
            if  v_ind_dwb_run_mode = "Batch" /*l_batch*/ 
            then do:
                assign v_cod_ult_obj_procesdo = tt_rpt_bem_pat.cod_cta_pat + "," + string(tt_rpt_bem_pat.num_bem_pat) + "," +
                                                string(tt_rpt_bem_pat.num_seq_bem_pat).
                run prgtec/btb/btb908ze.py (Input 1,
                                            Input v_cod_ult_obj_procesdo) /*prg_api_atualizar_ult_obj*/.
            end /* if */.
            if  first-of(tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[1])
            then do:
                assign v_log_cabec_relat = yes
                       v_log_rodap_relat = no
                       v_log_impr_relat  = no
                       v_log_aux         = yes.

            end /* if */.
            else assign v_log_aux = no
                        v_log_impr_relat  = yes.
            if  last-of(tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[1])
            then do:
                assign v_log_rodap_relat = yes.
                if  v_val_tot_imobdo_aux = 0 and v_val_tot_dpr_aux = 0 and v_val_tot_liq_bem_pat_aux = 0 and v_val_tot_amort_aux = 0 then
                    assign v_log_impr_relat = no.            
            end /* if */.
            assign v_val_imobdo_orig        = 0
                   v_val_dpr_aux            = 0
                   v_val_liq_bem_pat_aux    = 0
                   v_val_amort_aux          = 0
                   v_val_cm                 = 0.
            find last sdo_bem_pat no-lock
                 where sdo_bem_pat.num_id_bem_pat  = tt_rpt_bem_pat.num_id_bem_pat
                   and sdo_bem_pat.num_seq_incorp_bem_pat = 0
                   and sdo_bem_pat.cod_cenar_ctbl   = v_cod_cenar_ctbl
                   and sdo_bem_pat.cod_finalid_econ = v_cod_finalid_econ
                   and sdo_bem_pat.dat_sdo_bem_pat <= v_dat_fim_period + 1
                   no-error.

            find last b_sdo_bem_pat_vo no-lock
                 where b_sdo_bem_pat_vo.num_id_bem_pat  = tt_rpt_bem_pat.num_id_bem_pat
                   and b_sdo_bem_pat_vo.num_seq_incorp_bem_pat = 0
                   and b_sdo_bem_pat_vo.cod_cenar_ctbl   = v_cod_cenar_ctbl
                   and b_sdo_bem_pat_vo.cod_finalid_econ = v_cod_finalid_econ
                   and b_sdo_bem_pat_vo.dat_sdo_bem_pat <= v_dat_fim_period + 1
                   no-error.

            find first b_sdo_bem_pat no-lock
                 where b_sdo_bem_pat.num_id_bem_pat = tt_rpt_bem_pat.num_id_bem_pat
                   and b_sdo_bem_pat.num_seq_incorp_bem_pat = 0
                   and b_sdo_bem_pat.cod_cenar_ctbl = v_cod_cenar_ctbl
                   and b_sdo_bem_pat.cod_finalid_econ = v_cod_finalid_econ
                   and b_sdo_bem_pat.dat_sdo_bem_pat > &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 01/01/0001 &ENDIF no-error.

            if  not avail sdo_bem_pat or
                (avail sdo_bem_pat and
                tt_rpt_bem_pat.dat_aquis_bem_pat > v_dat_fim_period)
            then do:

                IF dwb_rpt_param.cod_dwb_output <> "Excel" THEN DO:
                    if  v_log_impr_relat = yes and v_log_rodap_relat = yes
                    then do:
                        if  v_log_resum_cta = yes
                        then do:
                            find cta_pat no-lock
                                 where cta_pat.cod_cta_pat = tt_rpt_bem_pat.cod_cta_pat
                                   and cta_pat.cod_empresa = tt_rpt_bem_pat.cod_empresa
                                  no-error.
                            if  v_log_impr_cabec = no
                            then do:
                                if  v_log_quant_bem then do:
                                    if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                                        page stream s_1.
                                    put stream s_1 unformatted 
                                        "Conta Patrimonial" at 3
                                        "Descriá∆o" at 23
                                        "Total Imobilizado" at 57
                                        "Total Depreciado" at 82
                                        "Total Amortizado" at 107
                                        "Total Liquido Bem" at 132
                                        "Total Quant" to 167 skip
                                        "------------------" at 3
                                        "--------------------------------" at 23
                                        "-----------------------" at 57
                                        "-----------------------" at 82
                                        "-----------------------" at 107
                                        "-----------------------" at 132
                                        "-----------" to 167 skip.
                                end.
                                else do:
                                    if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                                        page stream s_1.
                                    put stream s_1 unformatted 
                                        "Conta Patrimonial" at 3
                                        "Descriá∆o" at 23
                                        "Total Imobilizado" at 57
                                        "Total Depreciado" at 82
                                        "Total Amortizado" at 107
                                        "Total Liquido Bem" at 132 skip
                                        "------------------" at 3
                                        "--------------------------------" at 23
                                        "-----------------------" at 57
                                        "-----------------------" at 82
                                        "-----------------------" at 107
                                        "-----------------------" at 132 skip.
                                end.
                                assign v_log_impr_cabec = yes.
                            end /* if */.
                            if  v_log_quant_bem then do:
                                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                    page stream s_1.
                                put stream s_1 unformatted 
                                    tt_rpt_bem_pat.cod_cta_pat at 3 format "x(18)"
                                    cta_pat.des_cta_pat at 23 format "x(32)"
                                    v_des_val_tot_imobdo_aux at 57 format "x(23)"
                                    v_des_val_tot_dpr_aux at 82 format "x(23)"
                                    v_des_val_tot_amort_aux at 107 format "x(23)"
                                    v_des_val_tot_liq_bem_pat_aux at 132 format "x(23)"
                                    v_qtd_tot_bem_pat to 167 format ">>>>>>>>9" skip.
                            end.
                            else do:
                                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                    page stream s_1.
                                put stream s_1 unformatted 
                                    tt_rpt_bem_pat.cod_cta_pat at 3 format "x(18)"
                                    cta_pat.des_cta_pat at 23 format "x(32)"
                                    v_des_val_tot_imobdo_aux at 57 format "x(23)"
                                    v_des_val_tot_dpr_aux at 82 format "x(23)"
                                    v_des_val_tot_amort_aux at 107 format "x(23)"
                                    v_des_val_tot_liq_bem_pat_aux at 132 format "x(23)" skip.
                            end.
                        end /* if */.
                        else do:
                            if  (line-counter(s_1) + 3) > v_rpt_s_1_bottom
                            then do:
                                page stream s_1.
                            end /* if */.
                            if  v_val_tot_imobdo_aux     <> 0 or 
                                v_val_tot_dpr_aux         <> 0 or
                                v_val_tot_liq_bem_pat_aux <> 0 or
                                v_val_tot_amort_aux       <> 0
                            then do:
                                find cta_pat no-lock
                                     where cta_pat.cod_cta_pat = tt_rpt_bem_pat.cod_cta_pat
                                       and cta_pat.cod_empresa = tt_rpt_bem_pat.cod_empresa
                                      no-error.
                                assign v_des_tot_grp_relat = "Total Conta Patrimonial" /*l_total_cta_pat_abrev*/  + " " + cta_pat.cod_cta_pat.
                                if  v_log_quant_bem then do:
                                    if (line-counter(s_1) + 4) > v_rpt_s_1_bottom then
                                        page stream s_1.
                                    put stream s_1 unformatted 
                                        skip (1)
                                        fill(" ", 50 - length(v_des_tot_grp_relat)) + v_des_tot_grp_relat at 94 format "x(50)"
                                        ":" at 145
                                        v_des_val_tot_imobdo_aux at 148 format "x(23)"
                                        v_des_val_tot_dpr_aux at 172 format "x(23)"
                                        v_des_val_tot_amort_aux at 197 format "x(23)"
                                        v_des_val_tot_liq_bem_pat_aux at 222 format "x(23)".
            put stream s_1 unformatted 
                                        v_qtd_tot_bem_pat to 255 format ">>>>>>>>9" skip (2).
                                end.
                                else do:
                                    if (line-counter(s_1) + 4) > v_rpt_s_1_bottom then
                                        page stream s_1.
                                    put stream s_1 unformatted 
                                        skip (1)
                                        fill(" ", 50 - length(v_des_tot_grp_relat)) + v_des_tot_grp_relat at 94 format "x(50)"
                                        ":" at 145
                                        v_des_val_tot_imobdo_aux at 148 format "x(23)"
                                        v_des_val_tot_dpr_aux at 172 format "x(23)"
                                        v_des_val_tot_amort_aux at 197 format "x(23)"
                                        v_des_val_tot_liq_bem_pat_aux at 222 format "x(23)" skip (2).
                                end.
                            end /* if */.
                        end /* else */.
                        assign v_val_tot_imobdo_aux      = 0
                               v_val_tot_dpr_aux         = 0
                               v_val_tot_liq_bem_pat_aux = 0
                               v_log_rodap_relat         = no
                               v_qtd_tot_bem_pat         = 0.
                    end /* if */.
                END.
                if  v_log_bem_imobdo = no
                then do:
                    next grp_block.
                end /* if */.
            end /* if */.
            if  avail b_sdo_bem_pat_vo
            then do:
                    assign  v_val_imobdo_orig =  round(sdo_bem_pat.val_original +  sdo_bem_pat.val_cm, v_num_dec).
            end /* if */.
            if  avail sdo_bem_pat
            then do:
                assign v_val_cm        = round(sdo_bem_pat.val_cm, v_num_dec)
                       v_val_dpr_aux   = round(sdo_bem_pat.val_dpr_val_origin + sdo_bem_pat.val_dpr_cm + sdo_bem_pat.val_cm_dpr, v_num_dec)
                       v_val_amort_aux = round(sdo_bem_pat.val_dpr_val_origin_amort + sdo_bem_pat.val_dpr_cm_amort + sdo_bem_pat.val_cm_dpr_amort, v_num_dec).
            end /* if */.

            incorp_block:
            for each incorp_bem_pat no-lock
                where incorp_bem_pat.num_id_bem_pat = tt_rpt_bem_pat.num_id_bem_pat
                  and incorp_bem_pat.dat_incorp_bem_pat <= v_dat_fim_period
                  and (incorp_bem_pat.cod_cenar_ctbl = v_cod_cenar_ctbl or incorp_bem_pat.cod_cenar_ctbl = ""):
                if  v_ind_dwb_run_mode = "Batch" /*l_batch*/ 
                then do:
                    assign v_cod_ult_obj_procesdo = tt_rpt_bem_pat.cod_cta_pat + "," + string(tt_rpt_bem_pat.num_bem_pat) + "," +
                                                    string(tt_rpt_bem_pat.num_seq_bem_pat) + "," + string(incorp_bem_pat.num_seq_incorp_bem_pat).
                    run prgtec/btb/btb908ze.py (Input 1,
                                                Input v_cod_ult_obj_procesdo) /*prg_api_atualizar_ult_obj*/.
                end /* if */.
                find last sdo_bem_pat no-lock
                     where sdo_bem_pat.num_id_bem_pat = tt_rpt_bem_pat.num_id_bem_pat
                       and sdo_bem_pat.num_seq_incorp_bem_pat = incorp_bem_pat.num_seq_incorp_bem_pat
                       and sdo_bem_pat.cod_cenar_ctbl = v_cod_cenar_ctbl
                       and sdo_bem_pat.cod_finalid_econ = v_cod_finalid_econ
                       and sdo_bem_pat.dat_sdo_bem_pat <= v_dat_fim_period + 1
                       no-error.
                find last b_sdo_bem_pat_vo no-lock
                     where b_sdo_bem_pat_vo.num_id_bem_pat  = tt_rpt_bem_pat.num_id_bem_pat
                       and b_sdo_bem_pat_vo.num_seq_incorp_bem_pat = incorp_bem_pat.num_seq_incorp_bem_pat
                       and b_sdo_bem_pat_vo.cod_cenar_ctbl   = v_cod_cenar_ctbl
                       and b_sdo_bem_pat_vo.cod_finalid_econ = v_cod_finalid_econ
                       and b_sdo_bem_pat_vo.dat_sdo_bem_pat <= v_dat_fim_period + 1
                       no-error.
                find first b_sdo_bem_pat no-lock
                     where b_sdo_bem_pat.num_id_bem_pat = tt_rpt_bem_pat.num_id_bem_pat
                       and b_sdo_bem_pat.num_seq_incorp_bem_pat = incorp_bem_pat.num_seq_incorp_bem_pat
                       and b_sdo_bem_pat.cod_cenar_ctbl = v_cod_cenar_ctbl
                       and b_sdo_bem_pat.cod_finalid_econ = v_cod_finalid_econ
                       and b_sdo_bem_pat.dat_sdo_bem_pat > &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 01/01/0001 &ENDIF no-error.
                if  not avail sdo_bem_pat
                then do:
                    if  v_log_bem_imobdo = no
                    then do:
                        next incorp_block.
                    end /* if */.

                end /* if */.
                if  avail b_sdo_bem_pat_vo
                then do:
                        assign  v_val_imobdo_orig = v_val_imobdo_orig + round(sdo_bem_pat.val_original + sdo_bem_pat.val_cm, v_num_dec).
                end /* if */.
                if  avail sdo_bem_pat
                then do:
                     assign v_val_cm        = v_val_cm        + round(sdo_bem_pat.val_cm, v_num_dec)
                            v_val_dpr_aux   = v_val_dpr_aux   + round(sdo_bem_pat.val_dpr_val_origin + sdo_bem_pat.val_dpr_cm + sdo_bem_pat.val_cm_dpr, v_num_dec)
                            v_val_amort_aux = v_val_amort_aux + round(sdo_bem_pat.val_dpr_val_origin_amort + sdo_bem_pat.val_dpr_cm_amort + sdo_bem_pat.val_cm_dpr_amort, v_num_dec).

                end /* if */.
            end /* for incorp_block */.
            if  v_log_resum_cta = no
            then do:
                assign v_log_impr_relat = yes.
            end /* if */.
            assign v_val_calc                      = v_val_dpr_aux                   + v_val_amort_aux
                   v_val_liq_bem_pat_aux           = v_val_imobdo_orig               - v_val_calc
                   v_val_tot_imobdo_aux            = v_val_tot_imobdo_aux            + v_val_imobdo_orig
                   v_val_tot_geral_imobdo_aux      = v_val_tot_geral_imobdo_aux      + v_val_imobdo_orig
                   v_val_tot_geral_origin_aux      = v_val_tot_geral_origin_aux      + v_val_original
                   v_val_tot_dpr_aux               = v_val_tot_dpr_aux               + v_val_dpr_aux
                   v_val_tot_geral_dpr_aux         = v_val_tot_geral_dpr_aux         + v_val_dpr_aux
                   v_val_tot_liq_bem_pat_aux       = v_val_tot_liq_bem_pat_aux       + v_val_liq_bem_pat_aux
                   v_val_tot_geral_liq_bem_pat_aux = v_val_tot_geral_liq_bem_pat_aux + v_val_liq_bem_pat_aux
                   v_val_tot_amort_aux             = v_val_tot_amort_aux             + v_val_amort_aux
                   v_val_tot_geral_amort_aux       = v_val_tot_geral_amort_aux       + v_val_amort_aux
                   v_qtd_tot_bem_pat               = v_qtd_tot_bem_pat               + tt_rpt_bem_pat.qtd_bem_pat_represen
          	   v_qtd_tot_geral_bem_pat         = v_qtd_tot_geral_bem_pat         + tt_rpt_bem_pat.qtd_bem_pat_represen.
            find last sdo_bem_pat no-lock
                 where sdo_bem_pat.num_id_bem_pat         = tt_rpt_bem_pat.num_id_bem_pat
                   and sdo_bem_pat.num_seq_incorp_bem_pat = 0
                   and sdo_bem_pat.cod_cenar_ctbl         = v_cod_cenar_ctbl
                   and sdo_bem_pat.cod_finalid_econ       = v_cod_finalid_econ
                   and sdo_bem_pat.dat_sdo_bem_pat       <= v_dat_fim_period + 1
                   no-error.


            /* Begin_Include: i_verifica_casas_decimais */
            assign v_num_dec_aux = v_num_dec
                   v_des_val_imobdo_orig           = ""
                   v_des_val_dpr_aux               = ""
                   v_des_val_amort_aux             = ""
                   v_des_val_liq_bem_pat_aux       = ""
                   v_des_val_tot_imobdo_aux        = ""
                   v_des_val_tot_dpr_aux           = ""
                   v_des_val_tot_amort_aux         = ""
                   v_des_val_tot_liq_bem_pat_aux   = ""
                   v_des_val_tot_geral_imobdo_aux  = ""
                   v_des_val_tot_geral_dpr_aux     = ""
                   v_des_val_tot_geral_amort_aux   = ""
                   v_des_val_tot_geral_liq_bem_pat = "".

            if  v_num_dec >= 6 then
                assign v_num_dec_aux = 6.

            if v_num_dec_aux = 0 then assign v_num_dec_aux = 2.

            case v_num_dec_aux:
                 when 2 then 
                        assign v_des_val_imobdo_orig           = string(v_val_imobdo_orig,               "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_dpr_aux               = string(v_val_dpr_aux,                   "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_amort_aux             = string(v_val_amort_aux,                 "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_liq_bem_pat_aux       = string(v_val_liq_bem_pat_aux,           "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_imobdo_aux        = string(v_val_tot_imobdo_aux,            "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_dpr_aux           = string(v_val_tot_dpr_aux,               "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_amort_aux         = string(v_val_tot_amort_aux,             "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_liq_bem_pat_aux   = string(v_val_tot_liq_bem_pat_aux,       "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_imobdo_aux  = string(v_val_tot_geral_imobdo_aux,      "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_dpr_aux     = string(v_val_tot_geral_dpr_aux,         "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_amort_aux   = string(v_val_tot_geral_amort_aux,       "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_liq_bem_pat = string(v_val_tot_geral_liq_bem_pat_aux, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux)).
                 when 3 then 
                        assign v_des_val_imobdo_orig           = string(v_val_imobdo_orig,               "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_dpr_aux               = string(v_val_dpr_aux,                   "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_amort_aux             = string(v_val_amort_aux,                 "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_liq_bem_pat_aux       = string(v_val_liq_bem_pat_aux,           "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_imobdo_aux        = string(v_val_tot_imobdo_aux,            "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_dpr_aux           = string(v_val_tot_dpr_aux,               "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_amort_aux         = string(v_val_tot_amort_aux,             "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_liq_bem_pat_aux   = string(v_val_tot_liq_bem_pat_aux,       "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_imobdo_aux  = string(v_val_tot_geral_imobdo_aux,      "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_dpr_aux     = string(v_val_tot_geral_dpr_aux,         "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_amort_aux   = string(v_val_tot_geral_amort_aux,       "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_liq_bem_pat = string(v_val_tot_geral_liq_bem_pat_aux, "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux)).            
                 when 4 then 
                        assign v_des_val_imobdo_orig           = string(v_val_imobdo_orig,               "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_dpr_aux               = string(v_val_dpr_aux,                   "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_amort_aux             = string(v_val_amort_aux,                 "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_liq_bem_pat_aux       = string(v_val_liq_bem_pat_aux,           "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_imobdo_aux        = string(v_val_tot_imobdo_aux,            "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_dpr_aux           = string(v_val_tot_dpr_aux,               "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_amort_aux         = string(v_val_tot_amort_aux,             "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_liq_bem_pat_aux   = string(v_val_tot_liq_bem_pat_aux,       "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_imobdo_aux  = string(v_val_tot_geral_imobdo_aux,      "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_dpr_aux     = string(v_val_tot_geral_dpr_aux,         "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_amort_aux   = string(v_val_tot_geral_amort_aux,       "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_liq_bem_pat = string(v_val_tot_geral_liq_bem_pat_aux, "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux)).
                 when 5 then 
                        assign v_des_val_imobdo_orig           = string(v_val_imobdo_orig,               "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_dpr_aux               = string(v_val_dpr_aux,                   "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_amort_aux             = string(v_val_amort_aux,                 "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_liq_bem_pat_aux       = string(v_val_liq_bem_pat_aux,           "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_imobdo_aux        = string(v_val_tot_imobdo_aux,            "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_dpr_aux           = string(v_val_tot_dpr_aux,               "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_amort_aux         = string(v_val_tot_amort_aux,             "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_liq_bem_pat_aux   = string(v_val_tot_liq_bem_pat_aux,       "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_imobdo_aux  = string(v_val_tot_geral_imobdo_aux,      "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_dpr_aux     = string(v_val_tot_geral_dpr_aux,         "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_amort_aux   = string(v_val_tot_geral_amort_aux,       "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_liq_bem_pat = string(v_val_tot_geral_liq_bem_pat_aux, "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux)).
                 when 6 then 
                        assign v_des_val_imobdo_orig           = string(v_val_imobdo_orig,               "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_dpr_aux               = string(v_val_dpr_aux,                   "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_amort_aux             = string(v_val_amort_aux,                 "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_liq_bem_pat_aux       = string(v_val_liq_bem_pat_aux,           "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_imobdo_aux        = string(v_val_tot_imobdo_aux,            "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_dpr_aux           = string(v_val_tot_dpr_aux,               "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_amort_aux         = string(v_val_tot_amort_aux,             "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_liq_bem_pat_aux   = string(v_val_tot_liq_bem_pat_aux,       "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_imobdo_aux  = string(v_val_tot_geral_imobdo_aux,      "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_dpr_aux     = string(v_val_tot_geral_dpr_aux,         "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_amort_aux   = string(v_val_tot_geral_amort_aux,       "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_liq_bem_pat = string(v_val_tot_geral_liq_bem_pat_aux, "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux)).
            end case.            
            /* End_Include: i_verifica_casas_decimais */


            run pi_rpt_bem_pat_sit_geral_pat_2 /*pi_rpt_bem_pat_sit_geral_pat_2*/.

            if (v_val_tot_imobdo_aux <> 0 or v_val_tot_dpr_aux <> 0 or v_val_tot_liq_bem_pat_aux <> 0 or v_val_tot_amort_aux <> 0) then do:
                if  v_log_rodap_relat = yes
                then do:
                    IF dwb_rpt_param.cod_dwb_output <> "Excel" THEN DO:
                        if  (line-counter(s_1) + 3) > v_rpt_s_1_bottom
                        then do:
                          page stream s_1.
                        end /* if */.
                        if  v_log_resum_cta = no
                        then do:
                            if  v_val_tot_imobdo_aux     <> 0 or
                                v_val_tot_dpr_aux         <> 0 or
                                v_val_tot_liq_bem_pat_aux <> 0 or
                                v_val_tot_amort_aux       <> 0
                            then do:
                                   if  v_log_quant_bem then do:
                                       if (line-counter(s_1) + 4) > v_rpt_s_1_bottom then
                                           page stream s_1.
                                       put stream s_1 unformatted 
                                           skip (1)
                                           fill(" ", 50 - length(v_des_tot_grp_relat)) + v_des_tot_grp_relat at 94 format "x(50)"
                                           ":" at 145
                                           v_des_val_tot_imobdo_aux at 148 format "x(23)"
                                           v_des_val_tot_dpr_aux at 172 format "x(23)"
                                           v_des_val_tot_amort_aux at 197 format "x(23)"
                                           v_des_val_tot_liq_bem_pat_aux at 222 format "x(23)".
            put stream s_1 unformatted 
                                           v_qtd_tot_bem_pat to 255 format ">>>>>>>>9" skip (2).
                                   end.
                                   else do:
                                       if (line-counter(s_1) + 4) > v_rpt_s_1_bottom then
                                           page stream s_1.
                                       put stream s_1 unformatted 
                                           skip (1)
                                           fill(" ", 50 - length(v_des_tot_grp_relat)) + v_des_tot_grp_relat at 94 format "x(50)"
                                           ":" at 145
                                           v_des_val_tot_imobdo_aux at 148 format "x(23)"
                                           v_des_val_tot_dpr_aux at 172 format "x(23)"
                                           v_des_val_tot_amort_aux at 197 format "x(23)"
                                           v_des_val_tot_liq_bem_pat_aux at 222 format "x(23)" skip (2).                       
                                   end.
                            end /* if */.
                        end /* if */.
                        else do:
                            find cta_pat no-lock
                                 where cta_pat.cod_cta_pat = tt_rpt_bem_pat.cod_cta_pat
                                   and cta_pat.cod_empresa = tt_rpt_bem_pat.cod_empresa
                                  no-error.
                            if  v_log_quant_bem then do:
                                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                    page stream s_1.
                                put stream s_1 unformatted 
                                    tt_rpt_bem_pat.cod_cta_pat at 3 format "x(18)"
                                    cta_pat.des_cta_pat at 23 format "x(32)"
                                    v_des_val_tot_imobdo_aux at 57 format "x(23)"
                                    v_des_val_tot_dpr_aux at 82 format "x(23)"
                                    v_des_val_tot_amort_aux at 107 format "x(23)"
                                    v_des_val_tot_liq_bem_pat_aux at 132 format "x(23)"
                                    v_qtd_tot_bem_pat to 167 format ">>>>>>>>9" skip.
                            end.
                            else do:
                                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                    page stream s_1.
                                put stream s_1 unformatted 
                                    tt_rpt_bem_pat.cod_cta_pat at 3 format "x(18)"
                                    cta_pat.des_cta_pat at 23 format "x(32)"
                                    v_des_val_tot_imobdo_aux at 57 format "x(23)"
                                    v_des_val_tot_dpr_aux at 82 format "x(23)"
                                    v_des_val_tot_amort_aux at 107 format "x(23)"
                                    v_des_val_tot_liq_bem_pat_aux at 132 format "x(23)" skip.
                            end.
                        end /* else */.
                    END.
                    assign v_val_tot_imobdo_aux        = 0
                           v_val_tot_dpr_aux           = 0
                           v_val_tot_liq_bem_pat_aux   = 0
                           v_val_tot_amort_aux         = 0
                           v_val_tot_liq_bem_pat_amort = 0
                           v_log_rodap_relat           = no
                           v_qtd_tot_bem_pat           = 0.
                end /* if */.
            end.
            delete tt_rpt_bem_pat.
        /* End_Include: i_verifica_casas_decimais */

        end /* for grp_block */.
    end /* if */.    
    else do:
        grp_block:
        for each tt_rpt_bem_pat no-lock
            break by tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[1]  by tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[2]
                  by tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[3]  by tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[4]
                  by tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[5]  by tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[6]
                  by tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[7]  by tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[8]
                  by tt_rpt_bem_pat.num_seq_bem_pat:

        /* Begin_Include: i_rpt_bem_pat_sit_geral_pat */
            if first-of(tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[1]) then do:
                assign v_des_val_imobdo_orig = string(0, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec))
                       v_des_val_dpr_aux = string(0, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec))
                       v_des_val_amort_aux = string(0, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec))
                       v_des_val_liq_bem_pat_aux = string(0, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec))
                       v_des_val_tot_imobdo_aux = string(0, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec))
                       v_des_val_tot_dpr_aux = string(0, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec))
                       v_des_val_tot_amort_aux = string(0, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec))
                       v_des_val_tot_liq_bem_pat_aux = string(0, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec))
                       v_des_val_tot_geral_imobdo_aux = string(0, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec))
                       v_des_val_tot_geral_dpr_aux = string(0, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec))
                       v_des_val_tot_geral_amort_aux = string(0, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec))
                       v_des_val_tot_geral_liq_bem_pat = string(0, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec))
                       v_val_tot_imobdo_aux = 0 v_val_tot_dpr_aux = 0 v_val_tot_liq_bem_pat_aux = 0 v_val_tot_amort_aux = 0.
            end.               
            if  v_ind_dwb_run_mode = "Batch" /*l_batch*/ 
            then do:
                assign v_cod_ult_obj_procesdo = tt_rpt_bem_pat.cod_cta_pat + "," + string(tt_rpt_bem_pat.num_bem_pat) + "," +
                                                string(tt_rpt_bem_pat.num_seq_bem_pat).
                run prgtec/btb/btb908ze.py (Input 1,
                                            Input v_cod_ult_obj_procesdo) /*prg_api_atualizar_ult_obj*/.
            end /* if */.
            if  first-of(tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[1])
            then do:
                assign v_log_cabec_relat = yes
                       v_log_rodap_relat = no
                       v_log_impr_relat  = no
                       v_log_aux         = yes.

            end /* if */.
            else assign v_log_aux = no
                        v_log_impr_relat  = yes.
            if  last-of(tt_rpt_bem_pat.ttv_cod_dwb_field_rpt[1])
            then do:
                assign v_log_rodap_relat = yes.
                if  v_val_tot_imobdo_aux = 0 and v_val_tot_dpr_aux = 0 and v_val_tot_liq_bem_pat_aux = 0 and v_val_tot_amort_aux = 0 then
                    assign v_log_impr_relat = no.            
            end /* if */.
            assign v_val_imobdo_orig        = 0
                   v_val_dpr_aux            = 0
                   v_val_liq_bem_pat_aux    = 0
                   v_val_amort_aux          = 0
                   v_val_cm                 = 0
                   v_val_US_imobdo_orig     = 0
                   v_val_US_dpr_aux         = 0
                   v_val_US_amort_aux       = 0
                   .
            find last sdo_bem_pat no-lock
                 where sdo_bem_pat.num_id_bem_pat  = tt_rpt_bem_pat.num_id_bem_pat
                   and sdo_bem_pat.num_seq_incorp_bem_pat = 0
                   and sdo_bem_pat.cod_cenar_ctbl   = v_cod_cenar_ctbl
                   and sdo_bem_pat.cod_finalid_econ = v_cod_finalid_econ
                   and sdo_bem_pat.dat_sdo_bem_pat <= v_dat_fim_period + 1
                   no-error.

            find last b_sdo_bem_pat_vo no-lock
                 where b_sdo_bem_pat_vo.num_id_bem_pat  = tt_rpt_bem_pat.num_id_bem_pat
                   and b_sdo_bem_pat_vo.num_seq_incorp_bem_pat = 0
                   and b_sdo_bem_pat_vo.cod_cenar_ctbl   = v_cod_cenar_ctbl
                   and b_sdo_bem_pat_vo.cod_finalid_econ = v_cod_finalid_econ
                   and b_sdo_bem_pat_vo.dat_sdo_bem_pat <= v_dat_fim_period + 1
                   no-error.

            find first b_sdo_bem_pat no-lock
                 where b_sdo_bem_pat.num_id_bem_pat = tt_rpt_bem_pat.num_id_bem_pat
                   and b_sdo_bem_pat.num_seq_incorp_bem_pat = 0
                   and b_sdo_bem_pat.cod_cenar_ctbl = v_cod_cenar_ctbl
                   and b_sdo_bem_pat.cod_finalid_econ = v_cod_finalid_econ
                   and b_sdo_bem_pat.dat_sdo_bem_pat > &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 01/01/0001 &ENDIF no-error.

            find last US_sdo_bem_pat no-lock
                 where US_sdo_bem_pat.num_id_bem_pat  = tt_rpt_bem_pat.num_id_bem_pat
                   and US_sdo_bem_pat.num_seq_incorp_bem_pat = 0
                   and US_sdo_bem_pat.cod_cenar_ctbl   = v_cod_cenar_ctbl
                   and US_sdo_bem_pat.cod_finalid_econ = "Dolar"
                   and US_sdo_bem_pat.dat_sdo_bem_pat <= v_dat_fim_period + 1 no-error.

            find last USb_sdo_bem_pat_vo no-lock
                 where USb_sdo_bem_pat_vo.num_id_bem_pat  = tt_rpt_bem_pat.num_id_bem_pat
                   and USb_sdo_bem_pat_vo.num_seq_incorp_bem_pat = 0
                   and USb_sdo_bem_pat_vo.cod_cenar_ctbl   = v_cod_cenar_ctbl
                   and USb_sdo_bem_pat_vo.cod_finalid_econ = "Dolar"
                   and USb_sdo_bem_pat_vo.dat_sdo_bem_pat <= v_dat_fim_period + 1 no-error.

            find first USb_sdo_bem_pat no-lock
                 where USb_sdo_bem_pat.num_id_bem_pat = tt_rpt_bem_pat.num_id_bem_pat
                   and USb_sdo_bem_pat.num_seq_incorp_bem_pat = 0
                   and USb_sdo_bem_pat.cod_cenar_ctbl = v_cod_cenar_ctbl
                   and USb_sdo_bem_pat.cod_finalid_econ = "Dolar"
                   and USb_sdo_bem_pat.dat_sdo_bem_pat > &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 01/01/0001 &ENDIF no-error.

            if  not avail sdo_bem_pat or
                (avail sdo_bem_pat and
                tt_rpt_bem_pat.dat_aquis_bem_pat > v_dat_fim_period)
            then do:
                if  v_log_impr_relat = yes and v_log_rodap_relat = yes
                then do:
                    IF dwb_rpt_param.cod_dwb_output <> "Excel" THEN DO:
                        if  v_log_resum_cta = yes
                        then do:
                            find cta_pat no-lock
                                 where cta_pat.cod_cta_pat = tt_rpt_bem_pat.cod_cta_pat
                                   and cta_pat.cod_empresa = tt_rpt_bem_pat.cod_empresa
                                  no-error.
                            if  v_log_impr_cabec = no
                            then do:
                                if  v_log_quant_bem then do:
                                    if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                                        page stream s_1.
                                    put stream s_1 unformatted 
                                        "Conta Patrimonial" at 3
                                        "Descriá∆o" at 23
                                        "Total Imobilizado" at 57
                                        "Total Depreciado" at 82
                                        "Total Amortizado" at 107
                                        "Total Liquido Bem" at 132
                                        "Total Quant" to 167 skip
                                        "------------------" at 3
                                        "--------------------------------" at 23
                                        "-----------------------" at 57
                                        "-----------------------" at 82
                                        "-----------------------" at 107
                                        "-----------------------" at 132
                                        "-----------" to 167 skip.
                                end.
                                else do:
                                    if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                                        page stream s_1.
                                    put stream s_1 unformatted 
                                        "Conta Patrimonial" at 3
                                        "Descriá∆o" at 23
                                        "Total Imobilizado" at 57
                                        "Total Depreciado" at 82
                                        "Total Amortizado" at 107
                                        "Total Liquido Bem" at 132 skip
                                        "------------------" at 3
                                        "--------------------------------" at 23
                                        "-----------------------" at 57
                                        "-----------------------" at 82
                                        "-----------------------" at 107
                                        "-----------------------" at 132 skip.
                                end.
                                assign v_log_impr_cabec = yes.
                            end /* if */.
                            if  v_log_quant_bem then do:
                                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                    page stream s_1.
                                put stream s_1 unformatted 
                                    tt_rpt_bem_pat.cod_cta_pat at 3 format "x(18)"
                                    cta_pat.des_cta_pat at 23 format "x(32)"
                                    v_des_val_tot_imobdo_aux at 57 format "x(23)"
                                    v_des_val_tot_dpr_aux at 82 format "x(23)"
                                    v_des_val_tot_amort_aux at 107 format "x(23)"
                                    v_des_val_tot_liq_bem_pat_aux at 132 format "x(23)"
                                    v_qtd_tot_bem_pat to 167 format ">>>>>>>>9" skip.
                            end.
                            else do:
                                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                    page stream s_1.
                                put stream s_1 unformatted 
                                    tt_rpt_bem_pat.cod_cta_pat at 3 format "x(18)"
                                    cta_pat.des_cta_pat at 23 format "x(32)"
                                    v_des_val_tot_imobdo_aux at 57 format "x(23)"
                                    v_des_val_tot_dpr_aux at 82 format "x(23)"
                                    v_des_val_tot_amort_aux at 107 format "x(23)"
                                    v_des_val_tot_liq_bem_pat_aux at 132 format "x(23)" skip.
                            end.
                        end /* if */.
                        else do:
                            if  (line-counter(s_1) + 3) > v_rpt_s_1_bottom
                            then do:
                                page stream s_1.
                            end /* if */.
                            if  v_val_tot_imobdo_aux     <> 0 or 
                                v_val_tot_dpr_aux         <> 0 or
                                v_val_tot_liq_bem_pat_aux <> 0 or
                                v_val_tot_amort_aux       <> 0
                            then do:
                                find cta_pat no-lock
                                     where cta_pat.cod_cta_pat = tt_rpt_bem_pat.cod_cta_pat
                                       and cta_pat.cod_empresa = tt_rpt_bem_pat.cod_empresa
                                      no-error.
                                assign v_des_tot_grp_relat = "Total Conta Patrimonial" /*l_total_cta_pat_abrev*/  + " " + cta_pat.cod_cta_pat.
                                if  v_log_quant_bem then do:
                                    if (line-counter(s_1) + 4) > v_rpt_s_1_bottom then
                                        page stream s_1.
                                    put stream s_1 unformatted 
                                        skip (1)
                                        fill(" ", 50 - length(v_des_tot_grp_relat)) + v_des_tot_grp_relat at 94 format "x(50)"
                                        ":" at 145
                                        v_des_val_tot_imobdo_aux at 148 format "x(23)"
                                        v_des_val_tot_dpr_aux at 172 format "x(23)"
                                        v_des_val_tot_amort_aux at 197 format "x(23)"
                                        v_des_val_tot_liq_bem_pat_aux at 222 format "x(23)".
            put stream s_1 unformatted 
                                        v_qtd_tot_bem_pat to 255 format ">>>>>>>>9" skip (2).
                                end.
                                else do:
                                    if (line-counter(s_1) + 4) > v_rpt_s_1_bottom then
                                        page stream s_1.
                                    put stream s_1 unformatted 
                                        skip (1)
                                        fill(" ", 50 - length(v_des_tot_grp_relat)) + v_des_tot_grp_relat at 94 format "x(50)"
                                        ":" at 145
                                        v_des_val_tot_imobdo_aux at 148 format "x(23)"
                                        v_des_val_tot_dpr_aux at 172 format "x(23)"
                                        v_des_val_tot_amort_aux at 197 format "x(23)"
                                        v_des_val_tot_liq_bem_pat_aux at 222 format "x(23)" skip (2).
                                end.
                            end /* if */.
                        end /* else */.
                    END.
                    assign v_val_tot_imobdo_aux      = 0
                           v_val_tot_dpr_aux         = 0
                           v_val_tot_liq_bem_pat_aux = 0
                           v_log_rodap_relat         = no
                           v_qtd_tot_bem_pat         = 0.
                end /* if */.
                if  v_log_bem_imobdo = no
                then do:
                    next grp_block.
                end /* if */.
            end /* if */.
            if  avail b_sdo_bem_pat_vo
            then do:
                    assign  v_val_imobdo_orig =  round(sdo_bem_pat.val_original +  sdo_bem_pat.val_cm, v_num_dec).
                    assign  v_val_US_imobdo_orig =  round(US_sdo_bem_pat.val_original +  US_sdo_bem_pat.val_cm, v_num_dec).
            end /* if */.
            if  avail sdo_bem_pat
            then do:
                assign v_val_cm           = round(sdo_bem_pat.val_cm, v_num_dec)
                       v_val_dpr_aux      = round(sdo_bem_pat.val_dpr_val_origin + sdo_bem_pat.val_dpr_cm + sdo_bem_pat.val_cm_dpr, v_num_dec)
                       v_val_US_dpr_aux   = round(US_sdo_bem_pat.val_dpr_val_origin + US_sdo_bem_pat.val_dpr_cm + US_sdo_bem_pat.val_cm_dpr, v_num_dec)
                       v_val_amort_aux    = round(sdo_bem_pat.val_dpr_val_origin_amort + sdo_bem_pat.val_dpr_cm_amort + sdo_bem_pat.val_cm_dpr_amort, v_num_dec).
                       v_val_US_amort_aux = round(US_sdo_bem_pat.val_dpr_val_origin_amort + US_sdo_bem_pat.val_dpr_cm_amort + US_sdo_bem_pat.val_cm_dpr_amort, v_num_dec).
            end /* if */.

            incorp_block:
            for each incorp_bem_pat no-lock
                where incorp_bem_pat.num_id_bem_pat = tt_rpt_bem_pat.num_id_bem_pat
                  and incorp_bem_pat.dat_incorp_bem_pat <= v_dat_fim_period
                  and (incorp_bem_pat.cod_cenar_ctbl = v_cod_cenar_ctbl or incorp_bem_pat.cod_cenar_ctbl = ""):
                if  v_ind_dwb_run_mode = "Batch" /*l_batch*/ 
                then do:
                    assign v_cod_ult_obj_procesdo = tt_rpt_bem_pat.cod_cta_pat + "," + string(tt_rpt_bem_pat.num_bem_pat) + "," +
                                                    string(tt_rpt_bem_pat.num_seq_bem_pat) + "," + string(incorp_bem_pat.num_seq_incorp_bem_pat).
                    run prgtec/btb/btb908ze.py (Input 1,
                                                Input v_cod_ult_obj_procesdo) /*prg_api_atualizar_ult_obj*/.
                end /* if */.
                find last sdo_bem_pat no-lock
                     where sdo_bem_pat.num_id_bem_pat = tt_rpt_bem_pat.num_id_bem_pat
                       and sdo_bem_pat.num_seq_incorp_bem_pat = incorp_bem_pat.num_seq_incorp_bem_pat
                       and sdo_bem_pat.cod_cenar_ctbl = v_cod_cenar_ctbl
                       and sdo_bem_pat.cod_finalid_econ = v_cod_finalid_econ
                       and sdo_bem_pat.dat_sdo_bem_pat <= v_dat_fim_period + 1
                       no-error.
                find last b_sdo_bem_pat_vo no-lock
                     where b_sdo_bem_pat_vo.num_id_bem_pat  = tt_rpt_bem_pat.num_id_bem_pat
                       and b_sdo_bem_pat_vo.num_seq_incorp_bem_pat = incorp_bem_pat.num_seq_incorp_bem_pat
                       and b_sdo_bem_pat_vo.cod_cenar_ctbl   = v_cod_cenar_ctbl
                       and b_sdo_bem_pat_vo.cod_finalid_econ = v_cod_finalid_econ
                       and b_sdo_bem_pat_vo.dat_sdo_bem_pat <= v_dat_fim_period + 1
                       no-error.
                find first b_sdo_bem_pat no-lock
                     where b_sdo_bem_pat.num_id_bem_pat = tt_rpt_bem_pat.num_id_bem_pat
                       and b_sdo_bem_pat.num_seq_incorp_bem_pat = incorp_bem_pat.num_seq_incorp_bem_pat
                       and b_sdo_bem_pat.cod_cenar_ctbl = v_cod_cenar_ctbl
                       and b_sdo_bem_pat.cod_finalid_econ = v_cod_finalid_econ
                       and b_sdo_bem_pat.dat_sdo_bem_pat > &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 01/01/0001 &ENDIF no-error.

                find last US_sdo_bem_pat no-lock
                     where US_sdo_bem_pat.num_id_bem_pat = tt_rpt_bem_pat.num_id_bem_pat
                       and US_sdo_bem_pat.num_seq_incorp_bem_pat = incorp_bem_pat.num_seq_incorp_bem_pat
                       and US_sdo_bem_pat.cod_cenar_ctbl = v_cod_cenar_ctbl
                       and US_sdo_bem_pat.cod_finalid_econ = "Dolar"
                       and US_sdo_bem_pat.dat_sdo_bem_pat <= v_dat_fim_period + 1 no-error.

                find last USb_sdo_bem_pat_vo no-lock
                     where USb_sdo_bem_pat_vo.num_id_bem_pat  = tt_rpt_bem_pat.num_id_bem_pat
                       and USb_sdo_bem_pat_vo.num_seq_incorp_bem_pat = incorp_bem_pat.num_seq_incorp_bem_pat
                       and USb_sdo_bem_pat_vo.cod_cenar_ctbl   = v_cod_cenar_ctbl
                       and USb_sdo_bem_pat_vo.cod_finalid_econ = "Dolar"
                       and USb_sdo_bem_pat_vo.dat_sdo_bem_pat <= v_dat_fim_period + 1 no-error.

                find first USb_sdo_bem_pat no-lock
                     where USb_sdo_bem_pat.num_id_bem_pat = tt_rpt_bem_pat.num_id_bem_pat
                       and USb_sdo_bem_pat.num_seq_incorp_bem_pat = incorp_bem_pat.num_seq_incorp_bem_pat
                       and USb_sdo_bem_pat.cod_cenar_ctbl = v_cod_cenar_ctbl
                       and USb_sdo_bem_pat.cod_finalid_econ = "Dolar"
                       and USb_sdo_bem_pat.dat_sdo_bem_pat > &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 01/01/0001 &ENDIF no-error.

                if  not avail sdo_bem_pat
                then do:
                    if  v_log_bem_imobdo = no
                    then do:
                        next incorp_block.
                    end /* if */.

                end /* if */.
                if  avail b_sdo_bem_pat_vo
                then do:
                        assign  v_val_imobdo_orig = v_val_imobdo_orig + round(sdo_bem_pat.val_original + sdo_bem_pat.val_cm, v_num_dec).
                        assign  v_val_US_imobdo_orig = v_val_US_imobdo_orig + round(US_sdo_bem_pat.val_original + US_sdo_bem_pat.val_cm, v_num_dec).
                end /* if */.
                if  avail sdo_bem_pat
                then do:
                     assign v_val_cm           = v_val_cm           + round(sdo_bem_pat.val_cm, v_num_dec)
                            v_val_dpr_aux      = v_val_dpr_aux      + round(sdo_bem_pat.val_dpr_val_origin + sdo_bem_pat.val_dpr_cm + sdo_bem_pat.val_cm_dpr, v_num_dec)
                            v_val_US_dpr_aux   = v_val_US_dpr_aux   + round(US_sdo_bem_pat.val_dpr_val_origin + US_sdo_bem_pat.val_dpr_cm + US_sdo_bem_pat.val_cm_dpr, v_num_dec)
                            v_val_amort_aux    = v_val_amort_aux    + round(sdo_bem_pat.val_dpr_val_origin_amort + sdo_bem_pat.val_dpr_cm_amort + sdo_bem_pat.val_cm_dpr_amort, v_num_dec).
                            v_val_US_amort_aux = v_val_US_amort_aux + round(US_sdo_bem_pat.val_dpr_val_origin_amort + US_sdo_bem_pat.val_dpr_cm_amort + US_sdo_bem_pat.val_cm_dpr_amort, v_num_dec).

                end /* if */.
            end /* for incorp_block */.
            if  v_log_resum_cta = no
            then do:
                assign v_log_impr_relat = yes.
            end /* if */.
            assign v_val_calc                      = v_val_dpr_aux                   + v_val_amort_aux
                   v_val_liq_bem_pat_aux           = v_val_imobdo_orig               - v_val_calc
                   v_val_tot_imobdo_aux            = v_val_tot_imobdo_aux            + v_val_imobdo_orig
                   v_val_tot_geral_imobdo_aux      = v_val_tot_geral_imobdo_aux      + v_val_imobdo_orig
                   v_val_tot_geral_origin_aux      = v_val_tot_geral_origin_aux      + v_val_original
                   v_val_tot_dpr_aux               = v_val_tot_dpr_aux               + v_val_dpr_aux
                   v_val_tot_geral_dpr_aux         = v_val_tot_geral_dpr_aux         + v_val_dpr_aux
                   v_val_tot_liq_bem_pat_aux       = v_val_tot_liq_bem_pat_aux       + v_val_liq_bem_pat_aux
                   v_val_tot_geral_liq_bem_pat_aux = v_val_tot_geral_liq_bem_pat_aux + v_val_liq_bem_pat_aux
                   v_val_tot_amort_aux             = v_val_tot_amort_aux             + v_val_amort_aux
                   v_val_tot_geral_amort_aux       = v_val_tot_geral_amort_aux       + v_val_amort_aux
                   v_qtd_tot_bem_pat               = v_qtd_tot_bem_pat               + tt_rpt_bem_pat.qtd_bem_pat_represen
          	   v_qtd_tot_geral_bem_pat         = v_qtd_tot_geral_bem_pat         + tt_rpt_bem_pat.qtd_bem_pat_represen.
            find last sdo_bem_pat no-lock
                 where sdo_bem_pat.num_id_bem_pat         = tt_rpt_bem_pat.num_id_bem_pat
                   and sdo_bem_pat.num_seq_incorp_bem_pat = 0
                   and sdo_bem_pat.cod_cenar_ctbl         = v_cod_cenar_ctbl
                   and sdo_bem_pat.cod_finalid_econ       = v_cod_finalid_econ
                   and sdo_bem_pat.dat_sdo_bem_pat       <= v_dat_fim_period + 1
                   no-error.


            /* Begin_Include: i_verifica_casas_decimais */
            assign v_num_dec_aux = v_num_dec
                   v_des_val_imobdo_orig           = ""
                   v_des_val_dpr_aux               = ""
                   v_des_val_amort_aux             = ""
                   v_des_val_liq_bem_pat_aux       = ""
                   v_des_val_tot_imobdo_aux        = ""
                   v_des_val_tot_dpr_aux           = ""
                   v_des_val_tot_amort_aux         = ""
                   v_des_val_tot_liq_bem_pat_aux   = ""
                   v_des_val_tot_geral_imobdo_aux  = ""
                   v_des_val_tot_geral_dpr_aux     = ""
                   v_des_val_tot_geral_amort_aux   = ""
                   v_des_val_tot_geral_liq_bem_pat = "".

            if  v_num_dec >= 6 then
                assign v_num_dec_aux = 6.

            if v_num_dec_aux = 0 then assign v_num_dec_aux = 2.

            case v_num_dec_aux:
                 when 2 then 
                        assign v_des_val_imobdo_orig           = string(v_val_imobdo_orig,               "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_US_imobdo_orig        = string(v_val_US_imobdo_orig,            "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_dpr_aux               = string(v_val_dpr_aux,                   "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_US_dpr_aux            = string(v_val_US_dpr_aux,                "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_amort_aux             = string(v_val_amort_aux,                 "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_US_amort_aux          = string(v_val_US_amort_aux,              "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_liq_bem_pat_aux       = string(v_val_liq_bem_pat_aux,           "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_imobdo_aux        = string(v_val_tot_imobdo_aux,            "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_dpr_aux           = string(v_val_tot_dpr_aux,               "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_amort_aux         = string(v_val_tot_amort_aux,             "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_liq_bem_pat_aux   = string(v_val_tot_liq_bem_pat_aux,       "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_imobdo_aux  = string(v_val_tot_geral_imobdo_aux,      "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_dpr_aux     = string(v_val_tot_geral_dpr_aux,         "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_amort_aux   = string(v_val_tot_geral_amort_aux,       "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_liq_bem_pat = string(v_val_tot_geral_liq_bem_pat_aux, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux)).
                 when 3 then 
                        assign v_des_val_imobdo_orig           = string(v_val_imobdo_orig,               "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_US_imobdo_orig        = string(v_val_US_imobdo_orig,            "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_dpr_aux               = string(v_val_dpr_aux,                   "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_US_dpr_aux            = string(v_val_US_dpr_aux,                "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_amort_aux             = string(v_val_amort_aux,                 "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_US_amort_aux          = string(v_val_US_amort_aux,              "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_liq_bem_pat_aux       = string(v_val_liq_bem_pat_aux,           "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_imobdo_aux        = string(v_val_tot_imobdo_aux,            "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_dpr_aux           = string(v_val_tot_dpr_aux,               "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_amort_aux         = string(v_val_tot_amort_aux,             "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_liq_bem_pat_aux   = string(v_val_tot_liq_bem_pat_aux,       "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_imobdo_aux  = string(v_val_tot_geral_imobdo_aux,      "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_dpr_aux     = string(v_val_tot_geral_dpr_aux,         "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_amort_aux   = string(v_val_tot_geral_amort_aux,       "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_liq_bem_pat = string(v_val_tot_geral_liq_bem_pat_aux, "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux)).            
                 when 4 then 
                        assign v_des_val_imobdo_orig           = string(v_val_imobdo_orig,               "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_US_imobdo_orig        = string(v_val_US_imobdo_orig,            "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_dpr_aux               = string(v_val_dpr_aux,                   "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_US_dpr_aux            = string(v_val_US_dpr_aux,                "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_amort_aux             = string(v_val_amort_aux,                 "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_US_amort_aux          = string(v_val_US_amort_aux,              "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_liq_bem_pat_aux       = string(v_val_liq_bem_pat_aux,           "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_imobdo_aux        = string(v_val_tot_imobdo_aux,            "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_dpr_aux           = string(v_val_tot_dpr_aux,               "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_amort_aux         = string(v_val_tot_amort_aux,             "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_liq_bem_pat_aux   = string(v_val_tot_liq_bem_pat_aux,       "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_imobdo_aux  = string(v_val_tot_geral_imobdo_aux,      "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_dpr_aux     = string(v_val_tot_geral_dpr_aux,         "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_amort_aux   = string(v_val_tot_geral_amort_aux,       "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_liq_bem_pat = string(v_val_tot_geral_liq_bem_pat_aux, "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux)).
                 when 5 then 
                        assign v_des_val_imobdo_orig           = string(v_val_imobdo_orig,               "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_US_imobdo_orig        = string(v_val_US_imobdo_orig,            "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_dpr_aux               = string(v_val_dpr_aux,                   "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_US_dpr_aux            = string(v_val_US_dpr_aux,                "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_amort_aux             = string(v_val_amort_aux,                 "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_US_amort_aux          = string(v_val_US_amort_aux,              "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_liq_bem_pat_aux       = string(v_val_liq_bem_pat_aux,           "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_imobdo_aux        = string(v_val_tot_imobdo_aux,            "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_dpr_aux           = string(v_val_tot_dpr_aux,               "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_amort_aux         = string(v_val_tot_amort_aux,             "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_liq_bem_pat_aux   = string(v_val_tot_liq_bem_pat_aux,       "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_imobdo_aux  = string(v_val_tot_geral_imobdo_aux,      "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_dpr_aux     = string(v_val_tot_geral_dpr_aux,         "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_amort_aux   = string(v_val_tot_geral_amort_aux,       "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_liq_bem_pat = string(v_val_tot_geral_liq_bem_pat_aux, "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux)).
                 when 6 then 
                        assign v_des_val_imobdo_orig           = string(v_val_imobdo_orig,               "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_US_imobdo_orig        = string(v_val_US_imobdo_orig,            "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_dpr_aux               = string(v_val_dpr_aux,                   "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_US_dpr_aux            = string(v_val_US_dpr_aux,                "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_amort_aux             = string(v_val_amort_aux,                 "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_US_amort_aux          = string(v_val_US_amort_aux,              "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_liq_bem_pat_aux       = string(v_val_liq_bem_pat_aux,           "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_imobdo_aux        = string(v_val_tot_imobdo_aux,            "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_dpr_aux           = string(v_val_tot_dpr_aux,               "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_amort_aux         = string(v_val_tot_amort_aux,             "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_liq_bem_pat_aux   = string(v_val_tot_liq_bem_pat_aux,       "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_imobdo_aux  = string(v_val_tot_geral_imobdo_aux,      "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_dpr_aux     = string(v_val_tot_geral_dpr_aux,         "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_amort_aux   = string(v_val_tot_geral_amort_aux,       "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                               v_des_val_tot_geral_liq_bem_pat = string(v_val_tot_geral_liq_bem_pat_aux, "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux)).
            end case.            
            /* End_Include: i_verifica_casas_decimais */


            run pi_rpt_bem_pat_sit_geral_pat_2 /*pi_rpt_bem_pat_sit_geral_pat_2*/.

            if (v_val_tot_imobdo_aux <> 0 or v_val_tot_dpr_aux <> 0 or v_val_tot_liq_bem_pat_aux <> 0 or v_val_tot_amort_aux <> 0) then do:
                if  v_log_rodap_relat = yes
                then do:
                    IF dwb_rpt_param.cod_dwb_output <> "Excel" THEN DO:
                        if  (line-counter(s_1) + 3) > v_rpt_s_1_bottom
                        then do:
                          page stream s_1.
                        end /* if */.
                        if  v_log_resum_cta = no
                        then do:
                            if  v_val_tot_imobdo_aux     <> 0 or
                                v_val_tot_dpr_aux         <> 0 or
                                v_val_tot_liq_bem_pat_aux <> 0 or
                                v_val_tot_amort_aux       <> 0
                            then do:
                                   if  v_log_quant_bem then do:
                                       if (line-counter(s_1) + 4) > v_rpt_s_1_bottom then
                                           page stream s_1.
                                       put stream s_1 unformatted 
                                           skip (1)
                                           fill(" ", 50 - length(v_des_tot_grp_relat)) + v_des_tot_grp_relat at 94 format "x(50)"
                                           ":" at 145
                                           v_des_val_tot_imobdo_aux at 148 format "x(23)"
                                           v_des_val_tot_dpr_aux at 172 format "x(23)"
                                           v_des_val_tot_amort_aux at 197 format "x(23)"
                                           v_des_val_tot_liq_bem_pat_aux at 222 format "x(23)".
            put stream s_1 unformatted 
                                           v_qtd_tot_bem_pat to 255 format ">>>>>>>>9" skip (2).
                                   end.
                                   else do:
                                       if (line-counter(s_1) + 4) > v_rpt_s_1_bottom then
                                           page stream s_1.
                                       put stream s_1 unformatted 
                                           skip (1)
                                           fill(" ", 50 - length(v_des_tot_grp_relat)) + v_des_tot_grp_relat at 94 format "x(50)"
                                           ":" at 145
                                           v_des_val_tot_imobdo_aux at 148 format "x(23)"
                                           v_des_val_tot_dpr_aux at 172 format "x(23)"
                                           v_des_val_tot_amort_aux at 197 format "x(23)"
                                           v_des_val_tot_liq_bem_pat_aux at 222 format "x(23)" skip (2).                       
                                   end.
                            end /* if */.
                        end /* if */.
                        else do:
                            find cta_pat no-lock
                                 where cta_pat.cod_cta_pat = tt_rpt_bem_pat.cod_cta_pat
                                   and cta_pat.cod_empresa = tt_rpt_bem_pat.cod_empresa
                                  no-error.
                            if  v_log_quant_bem then do:
                                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                    page stream s_1.
                                put stream s_1 unformatted 
                                    tt_rpt_bem_pat.cod_cta_pat at 3 format "x(18)"
                                    cta_pat.des_cta_pat at 23 format "x(32)"
                                    v_des_val_tot_imobdo_aux at 57 format "x(23)"
                                    v_des_val_tot_dpr_aux at 82 format "x(23)"
                                    v_des_val_tot_amort_aux at 107 format "x(23)"
                                    v_des_val_tot_liq_bem_pat_aux at 132 format "x(23)"
                                    v_qtd_tot_bem_pat to 167 format ">>>>>>>>9" skip.
                            end.
                            else do:
                                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                    page stream s_1.
                                put stream s_1 unformatted 
                                    tt_rpt_bem_pat.cod_cta_pat at 3 format "x(18)"
                                    cta_pat.des_cta_pat at 23 format "x(32)"
                                    v_des_val_tot_imobdo_aux at 57 format "x(23)"
                                    v_des_val_tot_dpr_aux at 82 format "x(23)"
                                    v_des_val_tot_amort_aux at 107 format "x(23)"
                                    v_des_val_tot_liq_bem_pat_aux at 132 format "x(23)" skip.
                            end.
                        end /* else */.
                    END.

                    assign v_val_tot_imobdo_aux        = 0
                           v_val_tot_dpr_aux           = 0
                           v_val_tot_liq_bem_pat_aux   = 0
                           v_val_tot_amort_aux         = 0
                           v_val_tot_liq_bem_pat_amort = 0
                           v_log_rodap_relat           = no
                           v_qtd_tot_bem_pat           = 0.
                end /* if */.
            end.
            delete tt_rpt_bem_pat.
        /* End_Include: i_verifica_casas_decimais */

        end /* for grp_block */.
    end /* if */.

    IF dwb_rpt_param.cod_dwb_output <> "Excel" THEN DO:
        if  (line-counter(s_1) + 4) > v_rpt_s_1_bottom
        then do:
            page stream s_1.
        end /* if */.
    END.

    if dec(trim(v_des_val_tot_geral_imobdo_aux)) = 0 then do:

        /* Begin_Include: i_atribui_valor_casas */

        case v_num_dec_aux:
             when 2 then 
                    assign v_des_val_imobdo_orig           = string(v_val_imobdo_orig,               "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_dpr_aux               = string(v_val_dpr_aux,                   "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_amort_aux             = string(v_val_amort_aux,                 "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_liq_bem_pat_aux       = string(v_val_liq_bem_pat_aux,           "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_imobdo_aux        = string(v_val_tot_imobdo_aux,            "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_dpr_aux           = string(v_val_tot_dpr_aux,               "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_amort_aux         = string(v_val_tot_amort_aux,             "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_liq_bem_pat_aux   = string(v_val_tot_liq_bem_pat_aux,       "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_geral_imobdo_aux  = string(v_val_tot_geral_imobdo_aux,      "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_geral_dpr_aux     = string(v_val_tot_geral_dpr_aux,         "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_geral_amort_aux   = string(v_val_tot_geral_amort_aux,       "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_geral_liq_bem_pat = string(v_val_tot_geral_liq_bem_pat_aux, "->>>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux)).
             when 3 then 
                    assign v_des_val_imobdo_orig           = string(v_val_imobdo_orig,               "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_dpr_aux               = string(v_val_dpr_aux,                   "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_amort_aux             = string(v_val_amort_aux,                 "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_liq_bem_pat_aux       = string(v_val_liq_bem_pat_aux,           "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_imobdo_aux        = string(v_val_tot_imobdo_aux,            "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_dpr_aux           = string(v_val_tot_dpr_aux,               "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_amort_aux         = string(v_val_tot_amort_aux,             "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_liq_bem_pat_aux   = string(v_val_tot_liq_bem_pat_aux,       "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_geral_imobdo_aux  = string(v_val_tot_geral_imobdo_aux,      "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_geral_dpr_aux     = string(v_val_tot_geral_dpr_aux,         "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_geral_amort_aux   = string(v_val_tot_geral_amort_aux,       "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_geral_liq_bem_pat = string(v_val_tot_geral_liq_bem_pat_aux, "->>,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux)).            
             when 4 then 
                    assign v_des_val_imobdo_orig           = string(v_val_imobdo_orig,               "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_dpr_aux               = string(v_val_dpr_aux,                   "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_amort_aux             = string(v_val_amort_aux,                 "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_liq_bem_pat_aux       = string(v_val_liq_bem_pat_aux,           "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_imobdo_aux        = string(v_val_tot_imobdo_aux,            "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_dpr_aux           = string(v_val_tot_dpr_aux,               "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_amort_aux         = string(v_val_tot_amort_aux,             "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_liq_bem_pat_aux   = string(v_val_tot_liq_bem_pat_aux,       "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_geral_imobdo_aux  = string(v_val_tot_geral_imobdo_aux,      "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_geral_dpr_aux     = string(v_val_tot_geral_dpr_aux,         "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_geral_amort_aux   = string(v_val_tot_geral_amort_aux,       "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_geral_liq_bem_pat = string(v_val_tot_geral_liq_bem_pat_aux, "->,>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux)).
             when 5 then 
                    assign v_des_val_imobdo_orig           = string(v_val_imobdo_orig,               "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_dpr_aux               = string(v_val_dpr_aux,                   "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_amort_aux             = string(v_val_amort_aux,                 "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_liq_bem_pat_aux       = string(v_val_liq_bem_pat_aux,           "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_imobdo_aux        = string(v_val_tot_imobdo_aux,            "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_dpr_aux           = string(v_val_tot_dpr_aux,               "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_amort_aux         = string(v_val_tot_amort_aux,             "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_liq_bem_pat_aux   = string(v_val_tot_liq_bem_pat_aux,       "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_geral_imobdo_aux  = string(v_val_tot_geral_imobdo_aux,      "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_geral_dpr_aux     = string(v_val_tot_geral_dpr_aux,         "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_geral_amort_aux   = string(v_val_tot_geral_amort_aux,       "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_geral_liq_bem_pat = string(v_val_tot_geral_liq_bem_pat_aux, "->>>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux)).
             when 6 then 
                    assign v_des_val_imobdo_orig           = string(v_val_imobdo_orig,               "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_dpr_aux               = string(v_val_dpr_aux,                   "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_amort_aux             = string(v_val_amort_aux,                 "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_liq_bem_pat_aux       = string(v_val_liq_bem_pat_aux,           "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_imobdo_aux        = string(v_val_tot_imobdo_aux,            "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_dpr_aux           = string(v_val_tot_dpr_aux,               "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_amort_aux         = string(v_val_tot_amort_aux,             "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_liq_bem_pat_aux   = string(v_val_tot_liq_bem_pat_aux,       "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_geral_imobdo_aux  = string(v_val_tot_geral_imobdo_aux,      "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_geral_dpr_aux     = string(v_val_tot_geral_dpr_aux,         "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_geral_amort_aux   = string(v_val_tot_geral_amort_aux,       "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux))
                           v_des_val_tot_geral_liq_bem_pat = string(v_val_tot_geral_liq_bem_pat_aux, "->>>,>>>,>>>,>>9." + fill("9", v_num_dec_aux)).
        end case.
        /* End_Include: i_atribui_valor_casas */

    end.

    IF dwb_rpt_param.cod_dwb_output <> "Excel" THEN DO:
        if  v_log_resum_cta = no
        then do:
            if  v_log_quant_bem then do:
                if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    skip (1)
                    "Total Geral" at 133
                    ":" at 145
                    v_des_val_tot_geral_imobdo_aux at 148 format "x(23)"
                    v_des_val_tot_geral_dpr_aux at 172 format "x(23)"
                    v_des_val_tot_geral_amort_aux at 197 format "x(23)"
                    v_des_val_tot_geral_liq_bem_pat at 222 format "x(23)".
        put stream s_1 unformatted 
                    v_qtd_tot_geral_bem_pat to 255 format ">>>>>>>>9" skip.
            end.
            else do:
                if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    skip (1)
                    "Total Geral" at 133
                    ":" at 145
                    v_des_val_tot_geral_imobdo_aux at 148 format "x(23)"
                    v_des_val_tot_geral_dpr_aux at 172 format "x(23)"
                    v_des_val_tot_geral_amort_aux at 197 format "x(23)"
                    v_des_val_tot_geral_liq_bem_pat at 222 format "x(23)" skip.
            end.
        end /* if */.
        else do:
            if  v_log_quant_bem then do:
                if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    skip (1)
                    "Total Geral" at 44
                    v_des_val_tot_geral_imobdo_aux at 57 format "x(23)"
                    v_des_val_tot_geral_dpr_aux at 82 format "x(23)"
                    v_des_val_tot_geral_amort_aux at 107 format "x(23)"
                    v_des_val_tot_geral_liq_bem_pat at 132 format "x(23)"
                    v_qtd_tot_geral_bem_pat to 165 format ">>>>>>>>9" skip.
            end.
            else do:
                if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    skip (1)
                    "Total Geral" at 44
                    v_des_val_tot_geral_imobdo_aux at 57 format "x(23)"
                    v_des_val_tot_geral_dpr_aux at 82 format "x(23)"
                    v_des_val_tot_geral_amort_aux at 107 format "x(23)"
                    v_des_val_tot_geral_liq_bem_pat at 132 format "x(23)" skip.
            end.
        end /* else */.
    END.
    ELSE DO:
        OUTPUT CLOSE.

        /* ---> Cria objeto excel <--- */
        CREATE "Excel.Application" chExcelApplication.

        /* ---> Adiciona o modelo do documento <--- */
        chExcelApplication:VISIBLE = TRUE.
        chExcelApplication:Workbooks:OpenText(cArquivo, , , , , , TRUE ).

        chWorkSheet = chExcelApplication:Sheets:Item(1).

        chExcelApplication:Cells:Select.
        chExcelApplication:Cells:EntireColumn:AutoFit.

        chWorkSheet:Range("A1"):Select(). 

        chExcelApplication:application:DisplayAlerts = false.
        chExcelApplication:ActiveWorkbook:Saveas(session:TEMP-DIRECTORY + "ESFAS004",,,,,,).

        RELEASE object chExcelApplication.      
        /*
        RELEASE object chWorkbook.
        */
        RELEASE object chWorksheet.
    END.

    grp_block:
    for each tt_rpt_bem_pat exclusive-lock:
        delete tt_rpt_bem_pat.
    end /* for grp_block */.

    IF dwb_rpt_param.cod_dwb_output <> "Excel" THEN DO:
        hide stream s_1 frame f_rpt_s_1_footer_normal.
        hide stream s_1 frame f_rpt_s_1_footer_param_page.
        view stream s_1 frame f_rpt_s_1_footer_last_page.
    END.

    assign v_log_cta_zero   = no
           v_log_impr_cabec = no.
END PROCEDURE. /* pi_rpt_bem_pat_sit_geral_pat */
/*****************************************************************************
** Procedure Interna.....: pi_rpt_bem_pat_sit_geral_pat_2
** Descricao.............: pi_rpt_bem_pat_sit_geral_pat_2
** Criado por............: celma
** Criado em.............: 27/11/1997 15:20:24
** Alterado por..........: src531
** Alterado em...........: 23/04/2002 16:12:43
*****************************************************************************/
PROCEDURE pi_rpt_bem_pat_sit_geral_pat_2:

    IF dwb_rpt_param.cod_dwb_output <> "Excel" THEN DO:
        if  v_log_cabec_relat = yes
        then do:
            if  entry(1, dwb_rpt_param.cod_dwb_order) = "Conta Patrimonial"
            then do:
            /* BUSCANDO A DESCRIÄ«O DA CONTA PATRIMONIAL */
                find cta_pat no-lock
                     where cta_pat.cod_cta_pat = tt_rpt_bem_pat.cod_cta_pat
                       and cta_pat.cod_empresa = tt_rpt_bem_pat.cod_empresa
                      no-error.
                if  (line-counter(s_1) + 6) > v_rpt_s_1_bottom
                then do:
                    page stream s_1.
                end /* if */.
                if  v_log_resum_cta = no
                then do:
                    if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "Conta Patrimonial: " at 1
                        cta_pat.cod_cta_pat at 20 format "x(18)"
                        "-" at 39
                        if avail cta_pat then cta_pat.des_cta_pat else "" at 41 format "x(32)" skip (1).
                    if  not avail(sdo_bem_pat)
                    then do: 
                        if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            "Bem Pat" to 9
                            "Seq" to 15
                            "Descriá∆o" at 17
                            "Data Aquis" at 58
                            "Data Calc" at 69
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            "Est" at 80
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            "Est" at 80
        &ENDIF
                            "Un Neg" at 86
                            "Plano CCusto" at 104
                            "CC Respons" at 117 skip
                            "---------" to 9
                            "-----" to 15
                            "----------------------------------------" at 17
                            "----------" at 58
                            "----------" at 69
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            "---" at 80
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            "-----" at 80
        &ENDIF
                            "------" at 86
                            "------------" at 104
                            "-----------" at 117 skip.
                    end /* if */.
                    else do:
                        if  v_log_quant_bem then do:
                            if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                                page stream s_1.
                            put stream s_1 unformatted 
                                "Bem Pat" to 9
                                "Seq" to 15
                                "Descriá∆o" at 17
                                "Data Aquis" at 58
                                "Data Calc" at 69
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                                "Est" at 80
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                                "Est" at 80
        &ENDIF
                                "Un Neg" at 86
                                "Data Saldo" at 93
                                "Plano CCusto" at 104
                                "CC Respons" at 117
                                "Valor Imobilizado" at 148
                                "Valor Depreciado" at 172
                                "Valor Amortizado" at 197
                                "Valor Liquido" at 222
                                "Quantidade" to 255 skip
                                "---------" to 9
                                "-----" to 15
                                "----------------------------------------" at 17
                                "----------" at 58
                                "----------" at 69
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                                "---" at 80
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                                "-----" at 80
        &ENDIF
                                "------" at 86
                                "----------" at 93
                                "------------" at 104
                                "-----------" at 117
                                "-----------------------" at 148
                                "-----------------------" at 172
                                "-----------------------" at 197
                                "-----------------------" at 222
                                "----------" to 255 skip.
                        end.
                        else do:
                            if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                                page stream s_1.
                            put stream s_1 unformatted 
                                "Bem Pat" to 9
                                "Seq" to 15
                                "Descriá∆o" at 17
                                "Data Aquis" at 58
                                "Data Calc" at 69
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                                "Est" at 80
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                                "Est" at 80
        &ENDIF
                                "Un Neg" at 86
                                "Plano CCusto" at 104
                                "CC Respons" at 117
                                "Valor Imobilizado" at 148
                                "Valor Depreciado" at 172
                                "Valor Amortizado" at 197
                                "Valor Liquido" at 222 skip
                                "---------" to 9
                                "-----" to 15
                                "----------------------------------------" at 17
                                "----------" at 58
                                "----------" at 69
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                                "---" at 80
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                                "-----" at 80
        &ENDIF
                                "------" at 86
                                "------------" at 104
                                "-----------" at 117
                                "-----------------------" at 148
                                "-----------------------" at 172
                                "-----------------------" at 197
                                "-----------------------" at 222 skip. /* weniton */
                        end.
                    end /* else */.
                    assign v_des_tot_grp_relat = "Total Conta Patrimonial" /*l_total_cta_pat_abrev*/  + " " + cta_pat.cod_cta_pat.
                end /* if */.
                else do:
                    if  v_num_cont = 0
                    then do:
                        if  v_log_impr_cabec = no
                        then do:
                            if  not avail(sdo_bem_pat)
                            then do: 
                                if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                                    page stream s_1.
                                put stream s_1 unformatted 
                                    "Conta Patrimonial" at 3
                                    "Descriá∆o" at 23 skip
                                    "------------------" at 3
                                    "--------------------------------" at 23 skip.                       
                            end /* if */.
                            else do:
                                if  v_log_quant_bem then do:
                                    if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                                        page stream s_1.
                                    put stream s_1 unformatted 
                                        "Conta Patrimonial" at 3
                                        "Descriá∆o" at 23
                                        "Total Imobilizado" at 57
                                        "Total Depreciado" at 82
                                        "Total Amortizado" at 107
                                        "Total Liquido Bem" at 132
                                        "Total Quant" to 167 skip
                                        "------------------" at 3
                                        "--------------------------------" at 23
                                        "-----------------------" at 57
                                        "-----------------------" at 82
                                        "-----------------------" at 107
                                        "-----------------------" at 132
                                        "-----------" to 167 skip.
                                end.
                                else do:
                                    if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                                        page stream s_1.
                                    put stream s_1 unformatted 
                                        "Conta Patrimonial" at 3
                                        "Descriá∆o" at 23
                                        "Total Imobilizado" at 57
                                        "Total Depreciado" at 82
                                        "Total Amortizado" at 107
                                        "Total Liquido Bem" at 132 skip
                                        "------------------" at 3
                                        "--------------------------------" at 23
                                        "-----------------------" at 57
                                        "-----------------------" at 82
                                        "-----------------------" at 107
                                        "-----------------------" at 132 skip.
                                end.
                            end /* else */.
                            assign v_log_impr_cabec = yes.
                        end /* if */.    
                    end /* if */.
                    assign v_num_cont = 1.
                    /* @cx_rpt_put(rp_bem_pat_sit_geral_pat,grp_detalhe, lay_resum_cta).*/
                end /* else */.
            end /* if */.
            if  entry(1, dwb_rpt_param.cod_dwb_order) = &IF '{&emsfin_version}' >= '5.01' AND '{&emsfin_version}' <= '5.07' &THEN "Estabelecimento" &ELSE "Estabelecimento" &ENDIF
            then do:
            /* BUSCANDO A DESCRIÄ«O DO ESTABELECIMENTO */
                find estabelecimento no-lock
                     where estabelecimento.cod_estab = tt_rpt_bem_pat.cod_estab
                      no-error.
                if  (line-counter(s_1) + 6) > v_rpt_s_1_bottom
                then do:
                    page stream s_1.
                end /* if */.
                if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                    "Estabelecimento: " at 1
                    estabelecimento.cod_estab at 18 format "x(3)"
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                    "Estabelecimento: " at 1
                    estabelecimento.cod_estab at 18 format "x(5)".
        put stream s_1 unformatted 
        &ENDIF
                    "-" at 24
                    if avail estabelecimento then estabelecimento.nom_pessoa else "" at 26 format "x(40)" skip (1).
                if  not avail(sdo_bem_pat)
                then do: 
                     if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                         page stream s_1.
                     put stream s_1 unformatted 
                         "Conta Pat" at 1
                         "Bem Pat" to 28
                         "Seq" to 34
                         "Descriá∆o" at 36
                         "Data Aquis" at 77
                         "Data Calc" at 88
                         "Un Neg" at 99
                         "Plano CCusto" at 117
                         "CC Respons" at 130 skip
                         "------------------" at 1
                         "---------" to 28
                         "-----" to 34
                         "----------------------------------------" at 36
                         "----------" at 77
                         "----------" at 88
                         "------" at 99
                         "------------" at 117
                         "-----------" at 130 skip. 
                end /* if */.
                else do:
                    if  v_log_quant_bem then do:
                        if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            "Conta Pat" at 1
                            "Bem Pat" to 28
                            "Seq" to 34
                            "Descriá∆o" at 36
                            "Data Aquis" at 77
                            "Data Calc" at 88
                            "Un Neg" at 99
                            "Data Saldo" at 106
                            "Plano CCusto" at 117
                            "CC Respons" at 130
                            "Valor Imobilizado" at 148
                            "Valor Depreciado" at 172
                            "Valor Amortizado" at 197
                            "Valor Liquido" at 222
                            "Quantidade" to 255 skip
                            "------------------" at 1
                            "---------" to 28
                            "-----" to 34
                            "----------------------------------------" at 36
                            "----------" at 77
                            "----------" at 88
                            "------" at 99
                            "----------" at 106
                            "------------" at 117
                            "-----------" at 130
                            "-----------------------" at 148
                            "-----------------------" at 172
                            "-----------------------" at 197
                            "-----------------------" at 222
                            "----------" to 255 skip.
                    end.
                    else do:
                        if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            "Conta Pat" at 1
                            "Bem Pat" to 28
                            "Seq" to 34
                            "Descriá∆o" at 36
                            "Data Aquis" at 77
                            "Data Calc" at 88
                            "Un Neg" at 99
                            "Data Saldo" at 106
                            "Plano CCusto" at 117
                            "CC Respons" at 130
                            "Valor Imobilizado" at 148
                            "Valor Depreciado" at 172
                            "Valor Amortizado" at 197
                            "Valor Liquido" at 222 skip
                            "------------------" at 1
                            "---------" to 28
                            "-----" to 34
                            "----------------------------------------" at 36
                            "----------" at 77
                            "----------" at 88
                            "------" at 99
                            "----------" at 106
                            "------------" at 117
                            "-----------" at 130
                            "-----------------------" at 148
                            "-----------------------" at 172
                            "-----------------------" at 197
                            "-----------------------" at 222 skip. 
                    end.
                end /* else */.        
    
                assign v_des_tot_grp_relat = "Total Estabelecimento" /*l_tot_estabelecimento*/  + " " + estabelecimento.cod_estab.
            end /* if */.
            if  entry(1, dwb_rpt_param.cod_dwb_order) = "Unid Neg¢cio"
            then do:
            /* BUSCANDO A DESCRIÄ«O DA UNIDADE DE NEG‡CIO DO BEM PATRIMONIAL */
                find unid_negoc no-lock
                     where unid_negoc.cod_unid_negoc = tt_rpt_bem_pat.cod_unid_negoc
                      no-error.
                if  (line-counter(s_1) + 6) > v_rpt_s_1_bottom
                then do:
                    page stream s_1.
                end /* if */.
                if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "Unid Neg¢cio: " at 1
                    unid_negoc.cod_unid_negoc at 15 format "x(3)"
                    "-" at 19
                    if avail unid_negoc then unid_negoc.des_unid_negoc else "" at 21 format "x(40)" skip (1).
                if  not avail(sdo_bem_pat)
                then do: 
                    if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "Conta Pat" at 1
                        "Bem Pat" to 28
                        "Seq" to 34
                        "Descriá∆o" at 36
                        "Data Aquis" at 77
                        "Data Calc" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                        "Est" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        "Est" at 99
        &ENDIF
                        "Plano CCusto" at 116
                        "CC Respons" at 129 skip
                        "------------------" at 1
                        "---------" to 28
                        "-----" to 34
                        "----------------------------------------" at 36
                        "----------" at 77
                        "----------" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                        "---" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        "-----" at 99
        &ENDIF
                        "------------" at 116
                        "-----------" at 129 skip.
                end /* if */.
                else do:
                     if  v_log_quant_bem then do:
                        if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            "Conta Pat" at 1
                            "Bem Pat" to 28
                            "Seq" to 34
                            "Descriá∆o" at 36
                            "Data Aquis" at 77
                            "Data Calc" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            "Est" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            "Est" at 99
        &ENDIF
                            "Data Saldo" at 105
                            "Plano CCusto" at 116
                            "CC Respons" at 129
                            "Valor Imobilizado" at 148
                            "Valor Depreciado" at 172
                            "Valor Amortizado" at 197
                            "Valor Liquido" at 222
                            "Quantidade" to 255 skip
                            "------------------" at 1
                            "---------" to 28
                            "-----" to 34
                            "----------------------------------------" at 36
                            "----------" at 77
                            "----------" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            "---" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            "-----" at 99
        &ENDIF
                            "----------" at 105
                            "------------" at 116
                            "-----------" at 129
                            "-----------------------" at 148
                            "-----------------------" at 172
                            "-----------------------" at 197
                            "-----------------------" at 222
                            "----------" to 255 skip.
                    end.
                    else do:
                        if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            "Conta Pat" at 1
                            "Bem Pat" to 28
                            "Seq" to 34
                            "Descriá∆o" at 36
                            "Data Aquis" at 77
                            "Data Calc" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            "Est" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            "Est" at 99
        &ENDIF
                            "Data Saldo" at 105
                            "Plano CCusto" at 116
                            "CC Respons" at 129
                            "Valor Imobilizado" at 148
                            "Valor Depreciado" at 172
                            "Valor Amortizado" at 197
                            "Valor Liquido" at 222 skip
                            "------------------" at 1
                            "---------" to 28
                            "-----" to 34
                            "----------------------------------------" at 36
                            "----------" at 77
                            "----------" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            "---" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            "-----" at 99
        &ENDIF
                            "----------" at 105
                            "------------" at 116
                            "-----------" at 129
                            "-----------------------" at 148
                            "-----------------------" at 172
                            "-----------------------" at 197
                            "-----------------------" at 222 skip.     
                    end.
                end /* else */.
                assign v_des_tot_grp_relat = "Total Unid Neg¢cio" /*l_tot_unid_nego*/  + " " + unid_negoc.cod_unid_negoc.
            end /* if */.
            if  entry(1, dwb_rpt_param.cod_dwb_order) = "Plano Centros Custo"
            then do:
            /* BUSCANDO A DESCRIÄ«O DO PLANO DE CENTRO DE CUSTO DO BEM PATRIMONIAL */
                find plano_ccusto no-lock
                     where plano_ccusto.cod_empresa = tt_rpt_bem_pat.cod_empresa
                       and plano_ccusto.cod_plano_ccusto = tt_rpt_bem_pat.cod_plano_ccusto
                      no-error.
                if  (line-counter(s_1) + 6) > v_rpt_s_1_bottom
                then do:
                   page stream s_1.
                end /* if */.
                if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "Plano Centros Custo: " at 1
                    plano_ccusto.cod_plano_ccusto at 22 format "x(8)"
                    "-" at 31
                    plano_ccusto.des_tit_ctbl at 33 format "x(40)" skip (1).
        /* marcos */
    
                if  not avail(sdo_bem_pat)
                then do: 
                     if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                         page stream s_1.
                     put stream s_1 unformatted 
                         "Conta Pat" at 1
                         "Bem Pat" to 28
                         "Seq" to 34
                         "Descriá∆o" at 36
                         "Data Aquis" at 77
                         "Data Calc" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                         "Est" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                         "Est" at 99
        &ENDIF
                         "Un Neg" at 105
                         "CC Respons" at 123 skip
                         "------------------" at 1
                         "---------" to 28
                         "-----" to 34
                         "----------------------------------------" at 36
                         "----------" at 77
                         "----------" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                         "---" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                         "-----" at 99
        &ENDIF
                         "------" at 105
                         "-----------" at 123 skip.      
                end /* if */.
                else do:
                     if  v_log_quant_bem then do:
                         if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                             page stream s_1.
                         put stream s_1 unformatted 
                             "Conta Pat" at 1
                             "Bem Pat" to 28
                             "Seq" to 34
                             "Descriá∆o" at 36
                             "Data Aquis" at 77
                             "Data Calc" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                             "Est" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                             "Est" at 99
        &ENDIF
                             "Un Neg" at 105
                             "Data Saldo" at 112
                             "CC Respons" at 123
                             "Valor Imobilizado" at 148
                             "Valor Depreciado" at 172
                             "Valor Amortizado" at 197
                             "Valor Liquido" at 222
                             "Quantidade" to 255 skip
                             "------------------" at 1
                             "---------" to 28
                             "-----" to 34
                             "----------------------------------------" at 36
                             "----------" at 77
                             "----------" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                             "---" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                             "-----" at 99
        &ENDIF
                             "------" at 105
                             "----------" at 112
                             "-----------" at 123
                             "-----------------------" at 148
                             "-----------------------" at 172
                             "-----------------------" at 197
                             "-----------------------" at 222
                             "----------" to 255 skip. 
                     end.
                     else do:
                        if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            "Conta Pat" at 1
                            "Bem Pat" to 28
                            "Seq" to 34
                            "Descriá∆o" at 36
                            "Data Aquis" at 77
                            "Data Calc" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            "Est" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            "Est" at 99
        &ENDIF
                            "Un Neg" at 105
                            "Data Saldo" at 112
                            "CC Respons" at 123
                            "Valor Imobilizado" at 148
                            "Valor Depreciado" at 172
                            "Valor Amortizado" at 197
                            "Valor Liquido" at 222 skip
                            "------------------" at 1
                            "---------" to 28
                            "-----" to 34
                            "----------------------------------------" at 36
                            "----------" at 77
                            "----------" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            "---" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            "-----" at 99
        &ENDIF
                            "------" at 105
                            "----------" at 112
                            "-----------" at 123
                            "-----------------------" at 148
                            "-----------------------" at 172
                            "-----------------------" at 197
                            "-----------------------" at 222 skip.                  
                     end.
                end /* else */.
                assign v_des_tot_grp_relat = "Total Plano Centros Custo" /*l_tot_plano_centro_custo*/  + " " + plano_ccusto.cod_plano_ccusto.
            end /* if */.
            if  entry(1, dwb_rpt_param.cod_dwb_order) = "CCusto Responsab"
            then do:
            /* BUSCANDO A DESCRIÄ«O DO CENTRO DE CUSTO DO BEM PATRIMONIAL */
                find ccusto no-lock
                     where ccusto.cod_ccusto = tt_rpt_bem_pat.cod_ccusto_respons
                       and ccusto.cod_empresa = tt_rpt_bem_pat.cod_empresa
                       and ccusto.cod_plano_ccusto = tt_rpt_bem_pat.cod_plano_ccusto
                      no-error.
                if  (line-counter(s_1) + 6) > v_rpt_s_1_bottom
                then do:
                    page stream s_1.
                end /* if */.
                if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "Centro Custo: " at 1
                    ccusto.cod_ccusto at 15 format "x(11)"
                    "-" at 27
                    ccusto.des_tit_ctbl at 31 format "x(40)" skip (1).
                if  not avail(sdo_bem_pat)
                then do:
                   if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                       page stream s_1.
                   put stream s_1 unformatted 
                       "Conta Pat" at 1
                       "Bem Pat" to 28
                       "Seq" to 34
                       "Descriá∆o" at 36
                       "Data Aquis" at 77
                       "Data Calc" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                       "Est" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                       "Est" at 99
        &ENDIF
                       "Un Neg" at 105
                       "Plano CCusto" at 123 skip
                       "------------------" at 1
                       "---------" to 28
                       "-----" to 34
                       "----------------------------------------" at 36
                       "----------" at 77
                       "----------" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                       "---" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                       "-----" at 99
        &ENDIF
                       "------" at 105
                       "------------" at 123 skip.
                end /* if */.
                else do:
                    if  v_log_quant_bem then do:
                        if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            "Conta Pat" at 1
                            "Bem Pat" to 28
                            "Seq" to 34
                            "Descriá∆o" at 36
                            "Data Aquis" at 77
                            "Data Calc" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            "Est" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            "Est" at 99
        &ENDIF
                            "Un Neg" at 105
                            "Data Saldo" at 112
                            "Plano CCusto" at 123
                            "Valor Imobilizado" at 148
                            "Valor Depreciado" at 172
                            "Valor Amortizado" at 197
                            "Valor Liquido" at 222
                            "Quantidade" to 255 skip
                            "------------------" at 1
                            "---------" to 28
                            "-----" to 34
                            "----------------------------------------" at 36
                            "----------" at 77
                            "----------" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            "---" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            "-----" at 99
        &ENDIF
                            "------" at 105
                            "----------" at 112
                            "------------" at 123
                            "-----------------------" at 148
                            "-----------------------" at 172
                            "-----------------------" at 197
                            "-----------------------" at 222
                            "----------" to 255 skip.
                    end.
                    else do:
                    if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "Conta Pat" at 1
                        "Bem Pat" to 28
                        "Seq" to 34
                        "Descriá∆o" at 36
                        "Data Aquis" at 77
                        "Data Calc" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                        "Est" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        "Est" at 99
        &ENDIF
                        "Un Neg" at 105
                        "Data Saldo" at 112
                        "Plano CCusto" at 123
                        "Valor Imobilizado" at 148
                        "Valor Depreciado" at 172
                        "Valor Amortizado" at 197
                        "Valor Liquido" at 222 skip
                        "------------------" at 1
                        "---------" to 28
                        "-----" to 34
                        "----------------------------------------" at 36
                        "----------" at 77
                        "----------" at 88
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                        "---" at 99
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        "-----" at 99
        &ENDIF
                        "------" at 105
                        "----------" at 112
                        "------------" at 123
                        "-----------------------" at 148
                        "-----------------------" at 172
                        "-----------------------" at 197
                        "-----------------------" at 222 skip.                  
                    end.
                end /* else */.
                assign v_des_tot_grp_relat = "Total CCusto Responsab" /*l_tot_ccusto_respon*/  + " " + ccusto.cod_ccusto.
            end /* if */.
            if  entry(1, dwb_rpt_param.cod_dwb_order) = "Bem Patrimonial"
            then do:
                assign v_des_tot_grp_relat = "Total Bem" /*l_tot_bem*/  + " " + string(tt_rpt_bem_pat.num_bem_pat).
            end /* if */.
            if  entry(1, dwb_rpt_param.cod_dwb_order) = "Descriá∆o Bem Pat"
            then do:
                assign v_des_tot_grp_relat = "Total Descriá∆o" /*l_tot_descricao*/  + " " + string(tt_rpt_bem_pat.des_bem_pat).
            end /* if */.
            if  entry(1, dwb_rpt_param.cod_dwb_order) = "Data Aquisiá∆o"
            then do:
                assign v_des_tot_grp_relat = "Total Data Aquisiá∆o" /*l_tot_data_aquisicao*/  + " " + string(tt_rpt_bem_pat.dat_aquis_bem_pat).
            end /* if */.
            if  entry(1, dwb_rpt_param.cod_dwb_order) = "Data C†lculo"
            then do:
                assign v_des_tot_grp_relat = "Total Data C†lculo" /*l_tot_data_calculo*/  + " " + string(tt_rpt_bem_pat.dat_calc_pat).
            end /* if */.
            assign v_log_cabec_relat = no.
        end /* if */.
    
        if  v_log_resum_cta = no
        then do:
            run pi_rpt_bem_pat_sit_geral_pat_2_mais /*pi_rpt_bem_pat_sit_geral_pat_2_mais*/.
        end /* if */.
        run pi_rpt_bem_pat_sit_geral_pat_2_more /*pi_rpt_bem_pat_sit_geral_pat_2_more*/.

    END.
    ELSE DO:
        FIND FIRST PARAM_calc_bem_pat NO-LOCK
            where param_calc_bem_pat.num_id_bem_pat   = tt_rpt_bem_pat.num_id_bem_pat 
              and param_calc_bem_pat.cod_cenar_ctbl   = TRIM (v_cod_cenar_ctbl)
              and param_calc_bem_pat.cod_finalid_econ = TRIM (v_cod_finalid_econ)
              and param_calc_bem_pat.cod_tip_calc    <> " "
              and param_calc_bem_pat.cod_tip_calc    <> "CM" NO-ERROR.

        FIND FIRST US_PARAM_calc_bem_pat NO-LOCK
            where US_param_calc_bem_pat.num_id_bem_pat   = tt_rpt_bem_pat.num_id_bem_pat 
              and US_param_calc_bem_pat.cod_cenar_ctbl   = TRIM (v_cod_cenar_ctbl)
              and US_param_calc_bem_pat.cod_finalid_econ = "Dolar"
              and US_param_calc_bem_pat.cod_tip_calc    <> " "
              and US_param_calc_bem_pat.cod_tip_calc    <> "CM" NO-ERROR.

        FIND FIRST b-cta_pat NO-LOCK
            WHERE b-cta_pat.cod_empresa = v_cod_empres_usuar
            AND   b-cta_pat.cod_cta_pat = tt_rpt_bem_pat.cod_cta_pat NO-ERROR.

        FIND FIRST usuar_mestre NO-LOCK
            WHERE usuar_mestre.cod_usuario = tt_rpt_bem_pat.cod_usuar_ult_atualiz NO-ERROR.

        FIND FIRST emitente NO-LOCK
            WHERE emitente.cod-emitente = tt_rpt_bem_pat.cdn_fornecedor NO-ERROR.

        FIND FIRST unid_negoc NO-LOCK
            WHERE unid_negoc.cod_unid_negoc = tt_rpt_bem_pat.cod_unid_negoc NO-ERROR.

        ASSIGN tt_rpt_bem_pat.des_narrat_bem_pat = REPLACE(tt_rpt_bem_pat.des_narrat_bem_pat, CHR(10), "")
               tt_rpt_bem_pat.des_narrat_bem_pat = REPLACE(tt_rpt_bem_pat.des_narrat_bem_pat, CHR(11), "")
               tt_rpt_bem_pat.des_narrat_bem_pat = REPLACE(tt_rpt_bem_pat.des_narrat_bem_pat, CHR(12), "")
               tt_rpt_bem_pat.des_narrat_bem_pat = REPLACE(tt_rpt_bem_pat.des_narrat_bem_pat, CHR(13), "")
               tt_rpt_bem_pat.des_narrat_bem_pat = REPLACE(tt_rpt_bem_pat.des_narrat_bem_pat, ";", "").

        ASSIGN tt_rpt_bem_pat.des_bem_pat = REPLACE(tt_rpt_bem_pat.des_bem_pat, CHR(10), "")
               tt_rpt_bem_pat.des_bem_pat = REPLACE(tt_rpt_bem_pat.des_bem_pat, CHR(11), "")
               tt_rpt_bem_pat.des_bem_pat = REPLACE(tt_rpt_bem_pat.des_bem_pat, CHR(12), "")
               tt_rpt_bem_pat.des_bem_pat = REPLACE(tt_rpt_bem_pat.des_bem_pat, CHR(13), "")
               tt_rpt_bem_pat.des_bem_pat = REPLACE(tt_rpt_bem_pat.des_bem_pat, ";", "").

        IF tt_rpt_bem_pat.cb3_ident_visual = ?   OR 
           tt_rpt_bem_pat.cb3_ident_visual = "?" OR 
           tt_rpt_bem_pat.cb3_ident_visual = "'" OR 
           tt_rpt_bem_pat.cb3_ident_visual = '"' OR 
           tt_rpt_bem_pat.cb3_ident_visual = '""' THEN
            ASSIGN tt_rpt_bem_pat.cb3_ident_visual = " ".

        /*
        ASSIGN chExcelApplication:Range("A" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.cod_cta_pat
               chExcelApplication:Range("B" + STRING (i-linha)):VALUE = b-cta_pat.des_cta_pat
               chExcelApplication:Range("C" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.num_bem_pat
               chExcelApplication:Range("D" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.num_seq_bem_pat
               chExcelApplication:Range("E" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.des_bem_pat
               chExcelApplication:Range("F" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.cod_unid_negoc
               chExcelApplication:Range("G" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.cod_estab
               chExcelApplication:Range("H" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.cod_plano_ccusto
               chExcelApplication:Range("I" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.cod_ccusto_respons
               chExcelApplication:Range("J" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.dat_aquis_bem_pat
               chExcelApplication:Range("K" + STRING (i-linha)):VALUE = IF AVAILABLE PARAM_calc_bem_pat THEN PARAM_calc_bem_pat.dat_inic_calc ELSE ?
               chExcelApplication:Range("L" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.cod_grp_calc                                        
               chExcelApplication:Range("M" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.cdn_fornecedor                                      
               chExcelApplication:Range("N" + STRING (i-linha)):VALUE = (DECIMAL (v_des_val_dpr_aux) + DECIMAL (v_des_val_amort_aux)) / DECIMAL (v_des_val_imobdo_orig) * 100
               chExcelApplication:Range("O" + STRING (i-linha)):VALUE = DECIMAL (v_des_val_imobdo_orig)
               chExcelApplication:Range("P" + STRING (i-linha)):VALUE = DECIMAL (v_des_val_dpr_aux)
               chExcelApplication:Range("Q" + STRING (i-linha)):VALUE = DECIMAL (v_des_val_amort_aux)
               chExcelApplication:Range("R" + STRING (i-linha)):VALUE = DECIMAL (v_des_val_liq_bem_pat_aux)
               chExcelApplication:Range("S" + STRING (i-linha)):VALUE = IF AVAILABLE PARAM_calc_bem_pat THEN PARAM_calc_bem_pat.val_resid_min ELSE 0
               chExcelApplication:Range("T" + STRING (i-linha)):VALUE = DECIMAL (v_des_val_imobdo_orig) - (if available PARAM_calc_bem_pat then PARAM_calc_bem_pat.val_resid_min else 0)
               chExcelApplication:Range("U" + STRING (i-linha)):VALUE = IF AVAILABLE PARAM_calc_bem_pat THEN PARAM_calc_bem_pat.val_perc_anual_dpr ELSE 0
               chExcelApplication:Range("V" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.dat_calc_pat
               chExcelApplication:Range("W" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.cb3_ident_visual
               chExcelApplication:Range("X" + STRING (i-linha)):VALUE = ""
               chExcelApplication:Range("Y" + STRING (i-linha)):VALUE = ""
               chExcelApplication:Range("Z" + STRING (i-linha)):VALUE = ""
               chExcelApplication:Range("AA" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.des_narrat_bem_pat
               .
               */

        /*
        ASSIGN chExcelApplication:Range("A" + STRING (i-linha)):VALUE  = tt_rpt_bem_pat.cod_empresa           /*"Empresa                " */
               chExcelApplication:Range("B" + STRING (i-linha)):VALUE  = tt_rpt_bem_pat.cod_cta_pat           /*"Conta Patrimonial      " */
               chExcelApplication:Range("C" + STRING (i-linha)):VALUE  = b-cta_pat.des_cta_pat                /*"Descriá∆o Conta Pat    " */
               chExcelApplication:Range("D" + STRING (i-linha)):VALUE  = tt_rpt_bem_pat.num_bem_pat           /*"Bem Patrimonial        " */
               chExcelApplication:Range("E" + STRING (i-linha)):VALUE  = tt_rpt_bem_pat.num_seq_bem_pat       /*"Sequància Bem          " */
               chExcelApplication:Range("F" + STRING (i-linha)):VALUE  = tt_rpt_bem_pat.num_id_bem_pat        /*"Identificaá∆o Bem      " */
               chExcelApplication:Range("G" + STRING (i-linha)):VALUE  = tt_rpt_bem_pat.des_bem_pat           /*"Descriá∆o Bem Pat      " */
               chExcelApplication:Range("H" + STRING (i-linha)):VALUE  = qtd_bem_pat_represen                 /*"Quantidade             " */
               chExcelApplication:Range("I" + STRING (i-linha)):VALUE  = tt_rpt_bem_pat.dat_aquis_bem_pat     /*"Data Aquisiá∆o         " */
               chExcelApplication:Range("J" + STRING (i-linha)):VALUE  = '"' + STRING (tt_rpt_bem_pat.cod_estab) + '"'             /*"Estabel                " */
               chExcelApplication:Range("K" + STRING (i-linha)):VALUE  = tt_rpt_bem_pat.cod_grp_calc          /*"Grupo C†lculo          " */
               chExcelApplication:Range("L" + STRING (i-linha)):VALUE  = tt_rpt_bem_pat.cb3_ident_visual      /*"N£mero Plaqueta        " */
               chExcelApplication:Range("M" + STRING (i-linha)):VALUE  = tt_rpt_bem_pat.cod_espec_bem         /*"EspÇcie Bem Patrimonia " */
               chExcelApplication:Range("N" + STRING (i-linha)):VALUE  = tt_rpt_bem_pat.cod_marca             /*"Marca                  " */
               chExcelApplication:Range("O" + STRING (i-linha)):VALUE  = tt_rpt_bem_pat.cod_modelo            /*"Modelo                 " */
               chExcelApplication:Range("P" + STRING (i-linha)):VALUE  = tt_rpt_bem_pat.cod_licenc_uso        /*"Licenáa Uso            " */
               chExcelApplication:Range("Q" + STRING (i-linha)):VALUE  = tt_rpt_bem_pat.cod_especif_tec       /*"Especificaá∆o TÇcnica  " */
               chExcelApplication:Range("R" + STRING (i-linha)):VALUE  = tt_rpt_bem_pat.ind_sit_bem_pat       /*"Estado F°sico          " */
               chExcelApplication:Range("S" + STRING (i-linha)):VALUE  = tt_rpt_bem_pat.cdn_fornecedor        /*"Fornecedor             " */
               chExcelApplication:Range("T" + STRING (i-linha)):VALUE  = IF AVAILABLE emitente THEN '"' + STRING (emitente.cgc) + '"' ELSE ""
               chExcelApplication:Range("U" + STRING (i-linha)):VALUE  = tt_rpt_bem_pat.cod_docto_entr        /*"Docto Entrada          " */                                                                
               chExcelApplication:Range("V" + STRING (i-linha)):VALUE  = tt_rpt_bem_pat.cod_ser_nota          /*"SÇrie Nota             " */                                                                
               chExcelApplication:Range("W" + STRING (i-linha)):VALUE  = tt_rpt_bem_pat.num_item_docto_entr   /*"Numero Item            " */                                                                
               chExcelApplication:Range("X" + STRING (i-linha)):VALUE  = tt_rpt_bem_pat.cod_localiz           /*"Localizaá∆o            " */                                                                
               chExcelApplication:Range("Y" + STRING (i-linha)):VALUE  = tt_rpt_bem_pat.cod_usuar_ult_atualiz /*"Usu†rio                " */                                                                
               chExcelApplication:Range("Z" + STRING (i-linha)):VALUE  = IF AVAILABLE usuar_mestre THEN usuar_mestre.nom_usuario ELSE ""            /* Nome Usu†rio */
               chExcelApplication:Range("AA" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.cod_plano_ccusto      /*"Plano Centros Custo    " */                                                                
               chExcelApplication:Range("AB" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.cod_ccusto_respons    /*"CCusto                 " */                                                                
               chExcelApplication:Range("AC" + STRING (i-linha)):VALUE = '"' + tt_rpt_bem_pat.cod_unid_negoc + '"'        /*"Unid Neg¢cio           " */                                                                
               chExcelApplication:Range("AD" + STRING (i-linha)):VALUE = IF AVAILABLE unid_negoc THEN unid_negoc.des_unid_negoc ELSE ""
               chExcelApplication:Range("AE" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.cod_imagem            /*"Imagem                 " */                                                                
               chExcelApplication:Range("AF" + STRING (i-linha)):VALUE = v_dat_fim_period                     /*"Calculado AtÇ          " */                                                                
               chExcelApplication:Range("AG" + STRING (i-linha)):VALUE = DECIMAL (v_des_val_imobdo_orig)      /*"VL Corrigido R$        " */                                                                
               chExcelApplication:Range("AH" + STRING (i-linha)):VALUE = DECIMAL (v_des_val_dpr_aux) + DECIMAL (v_des_val_amort_aux)                        /*"Deprec. Acum R$        " */                  
               chExcelApplication:Range("AI" + STRING (i-linha)):VALUE = IF AVAILABLE PARAM_calc_bem_pat THEN DECIMAL (PARAM_calc_bem_pat.val_perc_anual_dpr) ELSE 0 /*"Taxa Deprec Anual R$   " */                   
               chExcelApplication:Range("AJ" + STRING (i-linha)):VALUE = ""                                   /*"VL Corrigido US$       " */                                                                
               chExcelApplication:Range("AK" + STRING (i-linha)):VALUE = ""                                   /*"Deprec. Acum US$       " */                                                                
               chExcelApplication:Range("AL" + STRING (i-linha)):VALUE = ""                                   /*"Taxa Deprec Anual US$  " */                                                                
               chExcelApplication:Range("AM" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.log_bem_imptdo        /*"Bem Importado          " */                                                                
               chExcelApplication:Range("AN" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.log_cr_pis            /*"Credita PIS            " */                                                                
               chExcelApplication:Range("AO" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.log_cr_cofins         /*"Credita COFINS         " */                                                                
               chExcelApplication:Range("AP" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.num_parc_pis_cofins   /*"Nr Parcelas            " */                                                                
               chExcelApplication:Range("AQ" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.val_cr_pis            /*"Valor Cred PIS/PASEP   " */                                                                
               chExcelApplication:Range("AR" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.val_cr_cofins         /*"Valor CrÇdito COFINS   " */                                                                
               chExcelApplication:Range("AS" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.log_cr_csll           /*"Credita CSLL           " */                                                                
               chExcelApplication:Range("AT" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.num_exerc_cr_csll     /*"Exercicios CrÇd CSLL   " */                                                          
               chExcelApplication:Range("AU" + STRING (i-linha)):VALUE = tt_rpt_bem_pat.des_narrat_bem_pat    /*"Narrativa Bem          " */                                                          
               .

        ASSIGN i-linha = i-linha + 1.
        */

        PUT UNFORMATTED
                     tt_rpt_bem_pat.cod_empresa           /*"Empresa                " */
                 ";" tt_rpt_bem_pat.cod_cta_pat           /*"Conta Patrimonial      " */
                 ";" b-cta_pat.des_cta_pat                /*"Descriá∆o Conta Pat    " */
                 ";" tt_rpt_bem_pat.num_bem_pat           /*"Bem Patrimonial        " */
                 ";" tt_rpt_bem_pat.num_seq_bem_pat       /*"Sequància Bem          " */
                 ";" tt_rpt_bem_pat.num_id_bem_pat        /*"Identificaá∆o Bem      " */
                 ";" tt_rpt_bem_pat.des_bem_pat           /*"Descriá∆o Bem Pat      " */
                 ";" qtd_bem_pat_represen                 /*"Quantidade             " */
                 ";" tt_rpt_bem_pat.dat_aquis_bem_pat     /*"Data Aquisiá∆o         " */
                 ";" '"' + STRING (tt_rpt_bem_pat.cod_estab) + '"'             /*"Estabel                " */
                 ";" tt_rpt_bem_pat.cod_grp_calc          /*"Grupo C†lculo          " */
                 ";" tt_rpt_bem_pat.cb3_ident_visual      /*"N£mero Plaqueta        " */
                 ";" tt_rpt_bem_pat.cod_espec_bem         /*"EspÇcie Bem Patrimonia " */
                 ";" tt_rpt_bem_pat.cod_marca             /*"Marca                  " */
                 ";" tt_rpt_bem_pat.cod_modelo            /*"Modelo                 " */
                 ";" tt_rpt_bem_pat.cod_licenc_uso        /*"Licenáa Uso            " */
                 ";" tt_rpt_bem_pat.cod_especif_tec       /*"Especificaá∆o TÇcnica  " */
                 ";" tt_rpt_bem_pat.ind_sit_bem_pat       /*"Estado F°sico          " */
                 ";" tt_rpt_bem_pat.cdn_fornecedor        /*"Fornecedor             " */
                 ";" IF AVAILABLE emitente THEN ' " ' + STRING (emitente.cgc) + ' " ' ELSE ' " " '
                 ";" tt_rpt_bem_pat.cod_docto_entr        /*"Docto Entrada          " */                                                                
                 ";" tt_rpt_bem_pat.cod_ser_nota          /*"SÇrie Nota             " */                                                                
                 ";" tt_rpt_bem_pat.num_item_docto_entr   /*"Numero Item            " */                                                                
                 ";" tt_rpt_bem_pat.cod_localiz           /*"Localizaá∆o            " */                                                                
                 ";" tt_rpt_bem_pat.cod_usuar_ult_atualiz /*"Usu†rio                " */                                                                
                 ";" IF AVAILABLE usuar_mestre THEN usuar_mestre.nom_usuario ELSE ""            /* Nome Usu†rio */
                 ";" tt_rpt_bem_pat.cod_plano_ccusto      /*"Plano Centros Custo    " */                                                                
                 ";" tt_rpt_bem_pat.cod_ccusto_respons    /*"CCusto                 " */                                                                
                 ";" '"' + tt_rpt_bem_pat.cod_unid_negoc + '"'        /*"Unid Neg¢cio           " */                                                                
                 ";" IF AVAILABLE unid_negoc THEN unid_negoc.des_unid_negoc ELSE ""
                 ";" tt_rpt_bem_pat.cod_imagem            /*"Imagem                 " */                                                                
                 ";" v_dat_fim_period                     /*"Calculado AtÇ          " */                                                                
                 ";" DECIMAL (v_des_val_imobdo_orig)      /*"VL Corrigido R$        " */                                                                
                 ";" DECIMAL (v_des_val_dpr_aux) + DECIMAL (v_des_val_amort_aux) /*"Deprec. Acum R$        " */                  
                 ";" IF AVAILABLE PARAM_calc_bem_pat THEN DECIMAL (PARAM_calc_bem_pat.val_perc_anual_dpr) ELSE 0 /*"Taxa Deprec Anual R$   " */                   
                 ";" DECIMAL (v_des_val_US_imobdo_orig)   /*"VL Corrigido US$       " */                                                                
                 ";" DECIMAL (v_des_val_US_dpr_aux) + DECIMAL (v_des_val_US_amort_aux) /*"Deprec. Acum US$       " */                                                                
                 ";" IF AVAILABLE US_PARAM_calc_bem_pat THEN DECIMAL (US_PARAM_calc_bem_pat.val_perc_anual_dpr) ELSE 0 /*"Taxa Deprec Anual US$  " */                                                                
                 ";" tt_rpt_bem_pat.log_bem_imptdo        /*"Bem Importado          " */                                                                
                 ";" tt_rpt_bem_pat.log_cr_pis            /*"Credita PIS            " */                                                                
                 ";" tt_rpt_bem_pat.log_cr_cofins         /*"Credita COFINS         " */                                                                
                 ";" tt_rpt_bem_pat.num_parc_pis_cofins   /*"Nr Parcelas            " */                                                                
                 ";" tt_rpt_bem_pat.val_cr_pis            /*"Valor Cred PIS/PASEP   " */                                                                
                 ";" tt_rpt_bem_pat.val_cr_cofins         /*"Valor CrÇdito COFINS   " */                                                                
                 ";" tt_rpt_bem_pat.log_cr_csll           /*"Credita CSLL           " */                                                                
                 ";" tt_rpt_bem_pat.num_exerc_cr_csll     /*"Exercicios CrÇd CSLL   " */                                                          
                 ";" tt_rpt_bem_pat.des_narrat_bem_pat    /*"Narrativa Bem          " */                                                          
                 SKIP.
    END.
END PROCEDURE. /* pi_rpt_bem_pat_sit_geral_pat_2 */
/*****************************************************************************
** Procedure Interna.....: pi_version_extract
** Descricao.............: pi_version_extract
** Criado por............: jaison
** Criado em.............: 31/07/1998 09:33:22
** Alterado por..........: tech14020
** Alterado em...........: 12/06/2006 09:09:21
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
/*****************************************************************************
** Procedure Interna.....: pi_rpt_bem_pat_sit_geral_pat_2_more
** Descricao.............: pi_rpt_bem_pat_sit_geral_pat_2_more
** Criado por............: bre17264
** Criado em.............: 03/09/1998 14:17:17
** Alterado por..........: fut33243
** Alterado em...........: 22/12/2010 16:19:10
*****************************************************************************/
PROCEDURE pi_rpt_bem_pat_sit_geral_pat_2_more:

    if  v_log_resum_cta = no
    then do:  
        if  entry(1, dwb_rpt_param.cod_dwb_order) = "Data C†lculo"
        then do:
            if  v_dat_calc_pat <> tt_rpt_bem_pat.dat_calc_pat
            then do:
                if  v_log_impr_narrat = no
                then do:
                     if  not avail(sdo_bem_pat)
                     then do:
                        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.dat_calc_pat at 1 format "99/99/9999"
                            tt_rpt_bem_pat.cod_cta_pat at 12 format "x(18)"
                            tt_rpt_bem_pat.num_bem_pat to 39 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 45 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 47 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN.
    put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                            tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)" skip.
                    end /* if */.
                    else do:
                        if  v_log_quant_bem then do:
                            if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                page stream s_1.
                            put stream s_1 unformatted 
                                tt_rpt_bem_pat.dat_calc_pat at 1 format "99/99/9999"
                                tt_rpt_bem_pat.cod_cta_pat at 12 format "x(18)"
                                tt_rpt_bem_pat.num_bem_pat to 39 format ">>>>>>>>9"
                                tt_rpt_bem_pat.num_seq_bem_pat to 45 format ">>>>9"
                                tt_rpt_bem_pat.des_bem_pat at 47 format "x(40)"
                                tt_rpt_bem_pat.dat_aquis_bem_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN.
    put stream s_1 unformatted 
                                tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                                tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                                sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                                tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                                tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)"
                                v_des_val_imobdo_orig at 148 format "x(23)".
    put stream s_1 unformatted 
                                v_des_val_dpr_aux at 172 format "x(23)"
                                v_des_val_amort_aux at 197 format "x(23)"
                                v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                                tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip.
                        end.
                        else do:
                            if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                page stream s_1.
                            put stream s_1 unformatted 
                                tt_rpt_bem_pat.dat_calc_pat at 1 format "99/99/9999"
                                tt_rpt_bem_pat.cod_cta_pat at 12 format "x(18)"
                                tt_rpt_bem_pat.num_bem_pat to 39 format ">>>>>>>>9"
                                tt_rpt_bem_pat.num_seq_bem_pat to 45 format ">>>>9"
                                tt_rpt_bem_pat.des_bem_pat at 47 format "x(40)"
                                tt_rpt_bem_pat.dat_aquis_bem_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN.
    put stream s_1 unformatted 
                                tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                                tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                                sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                                tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                                tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)"
                                v_des_val_imobdo_orig at 148 format "x(23)".
    put stream s_1 unformatted 
                                v_des_val_dpr_aux at 172 format "x(23)"
                                v_des_val_amort_aux at 197 format "x(23)"
                                v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip.
                        end.
                    end /* else */.
                end /* if */.
                else do:
                     if  not avail(sdo_bem_pat)
                     then do:
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.dat_calc_pat at 1 format "99/99/9999"
                            tt_rpt_bem_pat.cod_cta_pat at 12 format "x(18)"
                            tt_rpt_bem_pat.num_bem_pat to 39 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 45 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 47 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                            tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)" skip
                            entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                    end /* if */.
                    else do:
                        if  v_log_quant_bem then do:
                            run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                            put stream s_1 unformatted 
                                tt_rpt_bem_pat.dat_calc_pat at 1 format "99/99/9999"
                                tt_rpt_bem_pat.cod_cta_pat at 12 format "x(18)"
                                tt_rpt_bem_pat.num_bem_pat to 39 format ">>>>>>>>9"
                                tt_rpt_bem_pat.num_seq_bem_pat to 45 format ">>>>9"
                                tt_rpt_bem_pat.des_bem_pat at 47 format "x(40)"
                                tt_rpt_bem_pat.dat_aquis_bem_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                                tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                                sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                                tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                                tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)"
                                v_des_val_imobdo_orig at 148 format "x(23)"
                                v_des_val_dpr_aux at 172 format "x(23)".
    put stream s_1 unformatted 
                                v_des_val_amort_aux at 197 format "x(23)"
                                v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                                tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip
                                entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                            run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                        end.
                        else do:
                            run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                            put stream s_1 unformatted 
                                tt_rpt_bem_pat.dat_calc_pat at 1 format "99/99/9999"
                                tt_rpt_bem_pat.cod_cta_pat at 12 format "x(18)"
                                tt_rpt_bem_pat.num_bem_pat to 39 format ">>>>>>>>9"
                                tt_rpt_bem_pat.num_seq_bem_pat to 45 format ">>>>9"
                                tt_rpt_bem_pat.des_bem_pat at 47 format "x(40)"
                                tt_rpt_bem_pat.dat_aquis_bem_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                                tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                                sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                                tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                                tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)"
                                v_des_val_imobdo_orig at 148 format "x(23)"
                                v_des_val_dpr_aux at 172 format "x(23)".
    put stream s_1 unformatted 
                                v_des_val_amort_aux at 197 format "x(23)"
                                v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip
                                entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                            run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                        end.
                    end /* else */.                
                end /* else */.
                assign v_dat_calc_pat = tt_rpt_bem_pat.dat_calc_pat.
            end /* if */.
            else do:
                if  v_log_impr_narrat = no
                then do:
                        if  v_log_quant_bem then do:
                            if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                page stream s_1.
                            put stream s_1 unformatted 
                                tt_rpt_bem_pat.cod_cta_pat at 12 format "x(18)"
                                tt_rpt_bem_pat.num_bem_pat to 39 format ">>>>>>>>9"
                                tt_rpt_bem_pat.num_seq_bem_pat to 45 format ">>>>9"
                                tt_rpt_bem_pat.des_bem_pat at 47 format "x(40)"
                                tt_rpt_bem_pat.dat_aquis_bem_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                                tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                                sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                                tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                                tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)"
                                v_des_val_imobdo_orig at 148 format "x(23)"
                                v_des_val_dpr_aux at 172 format "x(23)".
    put stream s_1 unformatted 
                                v_des_val_amort_aux at 197 format "x(23)"
                                v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                                tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip. /* weniton */
                        end.
                        else do:
                            if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                page stream s_1.
                            put stream s_1 unformatted 
                                tt_rpt_bem_pat.cod_cta_pat at 12 format "x(18)"
                                tt_rpt_bem_pat.num_bem_pat to 39 format ">>>>>>>>9"
                                tt_rpt_bem_pat.num_seq_bem_pat to 45 format ">>>>9"
                                tt_rpt_bem_pat.des_bem_pat at 47 format "x(40)"
                                tt_rpt_bem_pat.dat_aquis_bem_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                                tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                                sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                                tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                                tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)"
                                v_des_val_imobdo_orig at 148 format "x(23)"
                                v_des_val_dpr_aux at 172 format "x(23)".
    put stream s_1 unformatted 
                                v_des_val_amort_aux at 197 format "x(23)"
                                v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip. /* weniton */
                        end.

                end /* if */.
                else do:
                        if  v_log_quant_bem then do:                
                            run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                            put stream s_1 unformatted 
                                tt_rpt_bem_pat.cod_cta_pat at 12 format "x(18)"
                                tt_rpt_bem_pat.num_bem_pat to 39 format ">>>>>>>>9"
                                tt_rpt_bem_pat.num_seq_bem_pat to 45 format ">>>>9"
                                tt_rpt_bem_pat.des_bem_pat at 47 format "x(40)"
                                tt_rpt_bem_pat.dat_aquis_bem_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF.
    put stream s_1 unformatted 
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                                tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                                sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                                tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                                tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)"
                                v_des_val_imobdo_orig at 148 format "x(23)"
                                v_des_val_dpr_aux at 172 format "x(23)"
                                v_des_val_amort_aux at 197 format "x(23)".
    put stream s_1 unformatted 
                                v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                                tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip
                                entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                            run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", ""). /* weniton */
                        end.
                        else do:
                            run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                            put stream s_1 unformatted 
                                tt_rpt_bem_pat.cod_cta_pat at 12 format "x(18)"
                                tt_rpt_bem_pat.num_bem_pat to 39 format ">>>>>>>>9"
                                tt_rpt_bem_pat.num_seq_bem_pat to 45 format ">>>>9"
                                tt_rpt_bem_pat.des_bem_pat at 47 format "x(40)"
                                tt_rpt_bem_pat.dat_aquis_bem_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF.
    put stream s_1 unformatted 
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                                tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                                sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                                tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                                tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)"
                                v_des_val_imobdo_orig at 148 format "x(23)"
                                v_des_val_dpr_aux at 172 format "x(23)"
                                v_des_val_amort_aux at 197 format "x(23)".
    put stream s_1 unformatted 
                                v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip
                                entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                            run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", ""). /* weniton */
                        end.
                end /* else */.
            end /* else */.
        end /* if */.
        if  entry(1, dwb_rpt_param.cod_dwb_order) = &IF '{&emsfin_version}' >= '5.01' AND '{&emsfin_version}' <= '5.07' &THEN "Estabelecimento" &ELSE "Estabelecimento" &ENDIF
        then do:
            if  v_cod_cta_pat <> tt_rpt_bem_pat.cod_cta_pat
            then do:
                if  v_log_impr_narrat = no
                then do:
                     if  not avail(sdo_bem_pat)
                     then do:
                        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_cta_pat at 1 format "x(18)"
                            tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
                            tt_rpt_bem_pat.cod_unid_negoc at 99 format "x(3)".
    put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_plano_ccusto at 117 format "x(8)"
                            tt_rpt_bem_pat.cod_ccusto_respons at 130 format "x(11)" skip.
                    end /* if */.
                    else do:
                        if  v_log_quant_bem then do:
                            if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                page stream s_1.
                            put stream s_1 unformatted 
                                tt_rpt_bem_pat.cod_cta_pat at 1 format "x(18)"
                                tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                                tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                                tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                                tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                                tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
                                tt_rpt_bem_pat.cod_unid_negoc at 99 format "x(3)".
    put stream s_1 unformatted 
                                sdo_bem_pat.dat_sdo_bem_pat at 106 format "99/99/9999"
                                tt_rpt_bem_pat.cod_plano_ccusto at 117 format "x(8)"
                                tt_rpt_bem_pat.cod_ccusto_respons at 130 format "x(11)"
                                v_des_val_imobdo_orig at 148 format "x(23)"
                                v_des_val_dpr_aux at 172 format "x(23)"
                                v_des_val_amort_aux at 197 format "x(23)"
                                v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                                tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip.
                        end.
                        else do:
                            if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                page stream s_1.
                            put stream s_1 unformatted 
                                tt_rpt_bem_pat.cod_cta_pat at 1 format "x(18)"
                                tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                                tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                                tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                                tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                                tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
                                tt_rpt_bem_pat.cod_unid_negoc at 99 format "x(3)".
    put stream s_1 unformatted 
                                sdo_bem_pat.dat_sdo_bem_pat at 106 format "99/99/9999"
                                tt_rpt_bem_pat.cod_plano_ccusto at 117 format "x(8)"
                                tt_rpt_bem_pat.cod_ccusto_respons at 130 format "x(11)"
                                v_des_val_imobdo_orig at 148 format "x(23)"
                                v_des_val_dpr_aux at 172 format "x(23)"
                                v_des_val_amort_aux at 197 format "x(23)"
                                v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip.
                        end.
                    end /* else */.                
                end /* if */.
                else do:
                     if  not avail(sdo_bem_pat)
                     then do:
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_cta_pat at 1 format "x(18)"
                            tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
                            tt_rpt_bem_pat.cod_unid_negoc at 99 format "x(3)"
                            tt_rpt_bem_pat.cod_plano_ccusto at 117 format "x(8)".
    put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_ccusto_respons at 130 format "x(11)" skip
                            entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                    end /* if */.
                    else do:
                        if  v_log_quant_bem then do:
                            run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                            put stream s_1 unformatted 
                                tt_rpt_bem_pat.cod_cta_pat at 1 format "x(18)"
                                tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                                tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                                tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                                tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                                tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
                                tt_rpt_bem_pat.cod_unid_negoc at 99 format "x(3)"
                                sdo_bem_pat.dat_sdo_bem_pat at 106 format "99/99/9999".
    put stream s_1 unformatted 
                                tt_rpt_bem_pat.cod_plano_ccusto at 117 format "x(8)"
                                tt_rpt_bem_pat.cod_ccusto_respons at 130 format "x(11)"
                                v_des_val_imobdo_orig at 148 format "x(23)"
                                v_des_val_dpr_aux at 172 format "x(23)"
                                v_des_val_amort_aux at 197 format "x(23)"
                                v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                                tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip
                                entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                            run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                        end.
                        else do:
                            run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                            put stream s_1 unformatted 
                                tt_rpt_bem_pat.cod_cta_pat at 1 format "x(18)"
                                tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                                tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                                tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                                tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                                tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
                                tt_rpt_bem_pat.cod_unid_negoc at 99 format "x(3)"
                                sdo_bem_pat.dat_sdo_bem_pat at 106 format "99/99/9999".
    put stream s_1 unformatted 
                                tt_rpt_bem_pat.cod_plano_ccusto at 117 format "x(8)"
                                tt_rpt_bem_pat.cod_ccusto_respons at 130 format "x(11)"
                                v_des_val_imobdo_orig at 148 format "x(23)"
                                v_des_val_dpr_aux at 172 format "x(23)"
                                v_des_val_amort_aux at 197 format "x(23)"
                                v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip
                                entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                            run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                        end.    
                    end /* else */.                
                end /* else */.
                assign v_cod_cta_pat = tt_rpt_bem_pat.cod_cta_pat.
            end /* if */.
            else do:
                if  v_log_impr_narrat = no
                then do:
                        if v_log_quant_bem then do:
                            if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                page stream s_1.
                            put stream s_1 unformatted 
                                tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                                tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                                tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                                tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                                tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
                                tt_rpt_bem_pat.cod_unid_negoc at 99 format "x(3)"
                                sdo_bem_pat.dat_sdo_bem_pat at 106 format "99/99/9999".
    put stream s_1 unformatted 
                                tt_rpt_bem_pat.cod_plano_ccusto at 117 format "x(8)"
                                tt_rpt_bem_pat.cod_ccusto_respons at 130 format "x(11)"
                                v_des_val_imobdo_orig at 148 format "x(23)"
                                v_des_val_dpr_aux at 172 format "x(23)"
                                v_des_val_amort_aux at 197 format "x(23)"
                                v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                                tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip. /* weniton teste*/
                        end.    
                        else do:
                             if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                 page stream s_1.
                             put stream s_1 unformatted 
                                 tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                                 tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                                 tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                                 tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                                 tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
                                 tt_rpt_bem_pat.cod_unid_negoc at 99 format "x(3)"
                                 sdo_bem_pat.dat_sdo_bem_pat at 106 format "99/99/9999".
    put stream s_1 unformatted 
                                 tt_rpt_bem_pat.cod_plano_ccusto at 117 format "x(8)"
                                 tt_rpt_bem_pat.cod_ccusto_respons at 130 format "x(11)"
                                 v_des_val_imobdo_orig at 148 format "x(23)"
                                 v_des_val_dpr_aux at 172 format "x(23)"
                                 v_des_val_amort_aux at 197 format "x(23)"
                                 v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip. /* weniton teste*/
                        end.
                end /* if */.
                else do:
                    if  not avail(sdo_bem_pat)
                    then do:
                            run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                            put stream s_1 unformatted 
                                tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                                tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                                tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                                tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                                tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
                                tt_rpt_bem_pat.cod_unid_negoc at 99 format "x(3)"
                                tt_rpt_bem_pat.cod_plano_ccusto at 117 format "x(8)"
                                tt_rpt_bem_pat.cod_ccusto_respons at 130 format "x(11)" skip.
    put stream s_1 unformatted 
                                entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                            run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                    end /* if */.
                    else do:
                        if v_log_quant_bem then do:
                            run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                            put stream s_1 unformatted 
                                tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                                tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                                tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                                tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                                tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
                                tt_rpt_bem_pat.cod_unid_negoc at 99 format "x(3)"
                                sdo_bem_pat.dat_sdo_bem_pat at 106 format "99/99/9999"
                                tt_rpt_bem_pat.cod_plano_ccusto at 117 format "x(8)".
    put stream s_1 unformatted 
                                tt_rpt_bem_pat.cod_ccusto_respons at 130 format "x(11)"
                                v_des_val_imobdo_orig at 148 format "x(23)"
                                v_des_val_dpr_aux at 172 format "x(23)"
                                v_des_val_amort_aux at 197 format "x(23)"
                                v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                                tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip
                                entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                            run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", ""). /* weniton */
                        end.
                        else do:
                          run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                          put stream s_1 unformatted 
                              tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                              tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                              tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                              tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                              tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
                              tt_rpt_bem_pat.cod_unid_negoc at 99 format "x(3)"
                              sdo_bem_pat.dat_sdo_bem_pat at 106 format "99/99/9999"
                              tt_rpt_bem_pat.cod_plano_ccusto at 117 format "x(8)".
    put stream s_1 unformatted 
                              tt_rpt_bem_pat.cod_ccusto_respons at 130 format "x(11)"
                              v_des_val_imobdo_orig at 148 format "x(23)"
                              v_des_val_dpr_aux at 172 format "x(23)"
                              v_des_val_amort_aux at 197 format "x(23)"
                              v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip
                              entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                          run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", ""). /* weniton */
                        end.        
                    end.
                end /* else */.
            end /* else */.
        end /* if */.
        if  entry(1, dwb_rpt_param.cod_dwb_order) = "Unid Neg¢cio"
        then do:
            if  v_cod_cta_pat <> tt_rpt_bem_pat.cod_cta_pat
            then do:
                if  v_log_impr_narrat = no
                then do:
                     if  not avail(sdo_bem_pat)
                     then do:
                        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_cta_pat at 1 format "x(18)"
                            tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN.
    put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_plano_ccusto at 116 format "x(8)"
                            tt_rpt_bem_pat.cod_ccusto_respons at 129 format "x(11)" skip.
                    end /* if */.
                    else do:
                        if  v_log_quant_bem then do:
                            if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                page stream s_1.
                            put stream s_1 unformatted 
                                tt_rpt_bem_pat.cod_cta_pat at 1 format "x(18)"
                                tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                                tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                                tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                                tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                                tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN.
    put stream s_1 unformatted 
                                tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                                sdo_bem_pat.dat_sdo_bem_pat at 105 format "99/99/9999"
                                tt_rpt_bem_pat.cod_plano_ccusto at 116 format "x(8)"
                                tt_rpt_bem_pat.cod_ccusto_respons at 129 format "x(11)"
                                v_des_val_imobdo_orig at 148 format "x(23)"
                                v_des_val_dpr_aux at 172 format "x(23)".
    put stream s_1 unformatted 
                                v_des_val_amort_aux at 197 format "x(23)"
                                v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                                tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip.                
                        end.
                        else do:
                            if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                page stream s_1.
                            put stream s_1 unformatted 
                                tt_rpt_bem_pat.cod_cta_pat at 1 format "x(18)"
                                tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                                tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                                tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                                tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                                tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN.
    put stream s_1 unformatted 
                                tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                                sdo_bem_pat.dat_sdo_bem_pat at 105 format "99/99/9999"
                                tt_rpt_bem_pat.cod_plano_ccusto at 116 format "x(8)"
                                tt_rpt_bem_pat.cod_ccusto_respons at 129 format "x(11)"
                                v_des_val_imobdo_orig at 148 format "x(23)"
                                v_des_val_dpr_aux at 172 format "x(23)".
    put stream s_1 unformatted 
                                v_des_val_amort_aux at 197 format "x(23)"
                                v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip.                
                        end.
                    end /* else */.                
                end /* if */.
                else do:
                     if  not avail(sdo_bem_pat)
                     then do:
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_cta_pat at 1 format "x(18)"
                            tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_plano_ccusto at 116 format "x(8)"
                            tt_rpt_bem_pat.cod_ccusto_respons at 129 format "x(11)" skip
                            entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                    end /* if */.
                    else do:
                        if  v_log_quant_bem then do:
                            run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                            put stream s_1 unformatted 
                                tt_rpt_bem_pat.cod_cta_pat at 1 format "x(18)"
                                tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                                tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                                tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                                tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                                tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                                sdo_bem_pat.dat_sdo_bem_pat at 105 format "99/99/9999"
                                tt_rpt_bem_pat.cod_plano_ccusto at 116 format "x(8)"
                                tt_rpt_bem_pat.cod_ccusto_respons at 129 format "x(11)"
                                v_des_val_imobdo_orig at 148 format "x(23)"
                                v_des_val_dpr_aux at 172 format "x(23)"
                                v_des_val_amort_aux at 197 format "x(23)".
    put stream s_1 unformatted 
                                v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                                tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip
                                entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                            run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                        end.
                        else do:
                            run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                            put stream s_1 unformatted 
                                tt_rpt_bem_pat.cod_cta_pat at 1 format "x(18)"
                                tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                                tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                                tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                                tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                                tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                                sdo_bem_pat.dat_sdo_bem_pat at 105 format "99/99/9999"
                                tt_rpt_bem_pat.cod_plano_ccusto at 116 format "x(8)"
                                tt_rpt_bem_pat.cod_ccusto_respons at 129 format "x(11)"
                                v_des_val_imobdo_orig at 148 format "x(23)"
                                v_des_val_dpr_aux at 172 format "x(23)"
                                v_des_val_amort_aux at 197 format "x(23)".
    put stream s_1 unformatted 
                                v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip
                                entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                            run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                        end.
                    end /* else */.                              
                end /* else */.
                assign v_cod_cta_pat = tt_rpt_bem_pat.cod_cta_pat.
            end /* if */.
            else do:
                if  v_log_impr_narrat = no
                then do:
                        if  v_log_quant_bem then do:
                            if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                page stream s_1.
                            put stream s_1 unformatted 
                                tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                                tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                                tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                                tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                                tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                                sdo_bem_pat.dat_sdo_bem_pat at 105 format "99/99/9999"
                                tt_rpt_bem_pat.cod_plano_ccusto at 116 format "x(8)"
                                tt_rpt_bem_pat.cod_ccusto_respons at 129 format "x(11)"
                                v_des_val_imobdo_orig at 148 format "x(23)"
                                v_des_val_dpr_aux at 172 format "x(23)"
                                v_des_val_amort_aux at 197 format "x(23)".
    put stream s_1 unformatted 
                                v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                                tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip. /* weniton */
                        end.
                        else do:
                            if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                                page stream s_1.
                            put stream s_1 unformatted 
                                tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                                tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                                tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                                tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                                tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                                sdo_bem_pat.dat_sdo_bem_pat at 105 format "99/99/9999"
                                tt_rpt_bem_pat.cod_plano_ccusto at 116 format "x(8)"
                                tt_rpt_bem_pat.cod_ccusto_respons at 129 format "x(11)"
                                v_des_val_imobdo_orig at 148 format "x(23)"
                                v_des_val_dpr_aux at 172 format "x(23)"
                                v_des_val_amort_aux at 197 format "x(23)".
    put stream s_1 unformatted 
                                v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip. /* weniton */
                        end.                    
                    end /* if */.
                    else do:                
                        if  v_log_quant_bem then do:                
                            run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                            put stream s_1 unformatted 
                                tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                                tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                                tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                                tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                                tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF.
    put stream s_1 unformatted 
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                                sdo_bem_pat.dat_sdo_bem_pat at 105 format "99/99/9999"
                                tt_rpt_bem_pat.cod_plano_ccusto at 116 format "x(8)"
                                tt_rpt_bem_pat.cod_ccusto_respons at 129 format "x(11)"
                                v_des_val_imobdo_orig at 148 format "x(23)"
                                v_des_val_dpr_aux at 172 format "x(23)"
                                v_des_val_amort_aux at 197 format "x(23)"
                                v_des_val_liq_bem_pat_aux at 222 format "x(23)".
    put stream s_1 unformatted 
                                tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip
                                entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                            run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", ""). /* weniton */
                        end.
                        else do:
                            run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                            put stream s_1 unformatted 
                                tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                                tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                                tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                                tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                                tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF.
    put stream s_1 unformatted 
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                                tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                                sdo_bem_pat.dat_sdo_bem_pat at 105 format "99/99/9999"
                                tt_rpt_bem_pat.cod_plano_ccusto at 116 format "x(8)"
                                tt_rpt_bem_pat.cod_ccusto_respons at 129 format "x(11)"
                                v_des_val_imobdo_orig at 148 format "x(23)"
                                v_des_val_dpr_aux at 172 format "x(23)"
                                v_des_val_amort_aux at 197 format "x(23)"
                                v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip.
    put stream s_1 unformatted 
                                entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                            run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", ""). /* weniton */
                        end.
                end /* else */.
            end /* else */.
        end /* if */.
        run pi_rpt_bem_pat_sit_geral_pat_mais /*pi_rpt_bem_pat_sit_geral_pat_mais*/.
    end /* if */.
END PROCEDURE. /* pi_rpt_bem_pat_sit_geral_pat_2_more */
/*****************************************************************************
** Procedure Interna.....: pi_ler_tt_rpt_bem_pat_sit_geral_pat_excec
** Descricao.............: pi_ler_tt_rpt_bem_pat_sit_geral_pat_excec
** Criado por............: Puccini
** Criado em.............: 28/05/1999 11:03:06
** Alterado por..........: Puccini
** Alterado em...........: 28/05/1999 11:11:17
*****************************************************************************/
PROCEDURE pi_ler_tt_rpt_bem_pat_sit_geral_pat_excec:

    if  dwb_rpt_select.cod_dwb_field = "Conta Patrimonial"
    then do:
        blk_tt:
        for each tt_rpt_bem_pat
            where tt_rpt_bem_pat.cod_empresa = v_cod_empres_usuar
            and   tt_rpt_bem_pat.cod_cta_pat >= dwb_rpt_select.cod_dwb_initial
            and   tt_rpt_bem_pat.cod_cta_pat <= dwb_rpt_select.cod_dwb_final:
            delete tt_rpt_bem_pat.
        end /* for blk_tt */.
    end /* if */.
    if  dwb_rpt_select.cod_dwb_field = "Bem Patrimonial"
    then do:
        blk_tt:
        for each tt_rpt_bem_pat
            where tt_rpt_bem_pat.cod_empresa = v_cod_empres_usuar
            and   tt_rpt_bem_pat.num_bem_pat >= int(dwb_rpt_select.cod_dwb_initial)
            and   tt_rpt_bem_pat.num_bem_pat <= int(dwb_rpt_select.cod_dwb_final):
            delete tt_rpt_bem_pat.
        end /* for blk_tt */.
    end /* if */.
    if  dwb_rpt_select.cod_dwb_field = "Descriá∆o Bem Pat"
    then do:
        blk_tt:
        for each tt_rpt_bem_pat
            where tt_rpt_bem_pat.cod_empresa = v_cod_empres_usuar
            and   tt_rpt_bem_pat.des_bem_pat >= dwb_rpt_select.cod_dwb_initial
            and   tt_rpt_bem_pat.des_bem_pat <= dwb_rpt_select.cod_dwb_final:
            delete tt_rpt_bem_pat.
        end /* for blk_tt */.
    end /* if */.
    if  dwb_rpt_select.cod_dwb_field = "Data Aquisiá∆o"
    then do:
        blk_tt:
        for each tt_rpt_bem_pat
            where tt_rpt_bem_pat.cod_empresa = v_cod_empres_usuar
            and   tt_rpt_bem_pat.dat_aquis_bem_pat >= date(dwb_rpt_select.cod_dwb_initial)
            and   tt_rpt_bem_pat.dat_aquis_bem_pat <= date(dwb_rpt_select.cod_dwb_final):
            delete tt_rpt_bem_pat.
        end /* for blk_tt */.
    end /* if */.
    if  dwb_rpt_select.cod_dwb_field = "Data C†lculo"
    then do:
        blk_tt:
        for each tt_rpt_bem_pat
            where tt_rpt_bem_pat.cod_empresa = v_cod_empres_usuar
            and   tt_rpt_bem_pat.dat_calc_pat >= date(dwb_rpt_select.cod_dwb_initial)
            and   tt_rpt_bem_pat.dat_calc_pat <= date(dwb_rpt_select.cod_dwb_final):
            delete tt_rpt_bem_pat.
        end /* for blk_tt */.
    end /* if */.
    if  dwb_rpt_select.cod_dwb_field = &IF '{&emsfin_version}' >= '5.01' AND '{&emsfin_version}' <= '5.07' &THEN "Estabelecimento" &ELSE "Estabelecimento" &ENDIF
    then do:
        blk_tt:
        for each tt_rpt_bem_pat
            where tt_rpt_bem_pat.cod_empresa = v_cod_empres_usuar
            and   tt_rpt_bem_pat.cod_estab >= dwb_rpt_select.cod_dwb_initial
            and   tt_rpt_bem_pat.cod_estab <= dwb_rpt_select.cod_dwb_final:
            delete tt_rpt_bem_pat.
        end /* for blk_tt */.
    end /* if */.
    if  dwb_rpt_select.cod_dwb_field = "Unid Neg¢cio"
    then do:
        blk_tt:
        for each tt_rpt_bem_pat
            where tt_rpt_bem_pat.cod_empresa = v_cod_empres_usuar
            and   tt_rpt_bem_pat.cod_unid_negoc >= dwb_rpt_select.cod_dwb_initial
            and   tt_rpt_bem_pat.cod_unid_negoc <= dwb_rpt_select.cod_dwb_final:
            delete tt_rpt_bem_pat.
        end /* for blk_tt */.
    end /* if */.
    if  dwb_rpt_select.cod_dwb_field = "Plano Centros Custo"
    then do:
        blk_tt:
        for each tt_rpt_bem_pat
            where tt_rpt_bem_pat.cod_empresa = v_cod_empres_usuar
            and   tt_rpt_bem_pat.cod_plano_ccusto >= dwb_rpt_select.cod_dwb_initial
            and   tt_rpt_bem_pat.cod_plano_ccusto <= dwb_rpt_select.cod_dwb_final:
            delete tt_rpt_bem_pat.
        end /* for blk_tt */.
    end /* if */.
    if  dwb_rpt_select.cod_dwb_field = "CCusto Responsab"
    then do:
        blk_tt:
        for each tt_rpt_bem_pat
            where tt_rpt_bem_pat.cod_empresa = v_cod_empres_usuar
            and   tt_rpt_bem_pat.cod_ccusto_respons >= dwb_rpt_select.cod_dwb_initial
            and   tt_rpt_bem_pat.cod_ccusto_respons <= dwb_rpt_select.cod_dwb_final:
            delete tt_rpt_bem_pat.
        end /* for blk_tt */.
    end /* if */.
END PROCEDURE. /* pi_ler_tt_rpt_bem_pat_sit_geral_pat_excec */
/*****************************************************************************
** Procedure Interna.....: pi_ler_tt_rpt_bem_pat_sit_geral_pat_regra
** Descricao.............: pi_ler_tt_rpt_bem_pat_sit_geral_pat_regra
** Criado por............: Puccini
** Criado em.............: 28/05/1999 11:02:43
** Alterado por..........: fut41162
** Alterado em...........: 10/03/2009 10:11:40
*****************************************************************************/
PROCEDURE pi_ler_tt_rpt_bem_pat_sit_geral_pat_regra:

    /************************* Variable Definition Begin ************************/

    def var v_val_percentual
        as decimal
        format "->>,>>>,>>>,>>9.99":U
        decimals 2
        label "Percentual"
        no-undo.
    def var v_val_perc_aux
        as decimal
        format ">>9.99":U
        decimals 2
        label "Perc Abat"
        column-label "Perc Abat"
        no-undo.


    /************************** Variable Definition End *************************/

    if  dwb_rpt_select.cod_dwb_field = "Conta Patrimonial"
    then do:
        block:
        for each bem_pat no-lock
         where bem_pat.cod_empresa = v_cod_empres_usuar
           and bem_pat.cod_cta_pat >= dwb_rpt_select.cod_dwb_initial
           and bem_pat.cod_cta_pat <= dwb_rpt_select.cod_dwb_final /*cl_rpt_bem_pat_conta of bem_pat*/:
            if  v_log_consid_bem_bxado = no and bem_pat.val_perc_bxa >= 100
            then do:
                find last movto_bem_pat
                    where movto_bem_pat.num_id_bem_pat = bem_pat.num_id_bem_pat 
                    and   movto_bem_pat.num_seq_incorp_bem_pat = 0 
                    and   movto_bem_pat.ind_trans_calc_bem_pat = 'Baixa'
                    and   movto_bem_pat.ind_orig_calc_bem_pat <> "Transferància" /*l_transferencia*/  no-lock no-error.
                if  avail movto_bem_pat then do:
                    if  movto_bem_pat.dat_movto_bem_pat <= v_dat_fim_period then do:
                        next block.
                    end.
                end.
                assign v_val_percentual = 0.
            end /* if */.
            find first tt_cta_pat_sit_geral 
                 where tt_cta_pat_sit_geral.tta_cod_empresa  = bem_pat.cod_empresa
                 and   tt_cta_pat_sit_geral.tta_cod_cta_pat  = bem_pat.cod_cta_pat
                 and   tt_cta_pat_sit_geral.tta_cod_grp_calc = bem_pat.cod_grp_calc
                 no-lock no-error.
            if  not avail tt_cta_pat_sit_geral
            then do:
               find first cta_pat 
                   where cta_pat.cod_empresa  = bem_pat.cod_empresa
                   and   cta_pat.cod_cta_pat  = bem_pat.cod_cta_pat
                   and   cta_pat.cod_grp_calc = bem_pat.cod_grp_calc
                   no-lock no-error.
               if  avail cta_pat
               then do:
                  create tt_cta_pat_sit_geral.
                  update tt_cta_pat_sit_geral.tta_cod_empresa      = cta_pat.cod_empresa
                         tt_cta_pat_sit_geral.tta_cod_cta_pat      = cta_pat.cod_cta_pat
                         tt_cta_pat_sit_geral.tta_cod_grp_calc     = cta_pat.cod_grp_calc
                         tt_cta_pat_sit_geral.tta_log_control_ctbl = cta_pat.log_control_ctbl.
               end /* if */.
            end /* if */.
            if  avail tt_cta_pat_sit_geral
            then do:
                if  (tt_cta_pat_sit_geral.tta_log_control_ctbl = no) and (v_log_bem_imobdo = no)
                then do:
                    next block.
                end /* if */.
            end /* if */.
            find first reg_calc_bem_pat of bem_pat 
                where reg_calc_bem_pat.num_seq_incorp_bem_pat = 0 
                and reg_calc_bem_pat.ind_trans = "Implantaá∆o" /*l_implantacao*/  no-lock no-error.

            if  avail reg_calc_bem_pat and reg_calc_bem_pat.dat_calc_pat > v_dat_fim_period
            then do: 
               next block.
            end /* if */.

            if  v_ind_dwb_run_mode = "Batch" /*l_batch*/ 
            then do:
                assign v_cod_ult_obj_procesdo = bem_pat.cod_cta_pat + "," +
                                                string(bem_pat.num_bem_pat) + "," +
                                                string(bem_pat.num_seq_bem_pat).
                run prgtec/btb/btb908ze.py (Input 1,
                                            Input v_cod_ult_obj_procesdo) /*prg_api_atualizar_ult_obj*/.
            end /* if */.
            run pi_tratar_tt_rpt_bem_pat_sit_geral_pat /*pi_tratar_tt_rpt_bem_pat_sit_geral_pat*/.
        end /* for block */.
    end /* if */.
    if  dwb_rpt_select.cod_dwb_field = "Bem Patrimonial"
    then do:
        block:
        for each bem_pat no-lock
         where bem_pat.cod_empresa = v_cod_empres_usuar
           and bem_pat.num_bem_pat >= integer(dwb_rpt_select.cod_dwb_initial)
           and bem_pat.num_bem_pat <= integer(dwb_rpt_select.cod_dwb_final) /*cl_rpt_bem_pat_num of bem_pat*/:
            if  v_log_consid_bem_bxado = no and bem_pat.val_perc_bxa >= 100
            then do:
                find last movto_bem_pat
                    where movto_bem_pat.num_id_bem_pat = bem_pat.num_id_bem_pat 
                    and   movto_bem_pat.num_seq_incorp_bem_pat = 0 
                    and   movto_bem_pat.ind_trans_calc_bem_pat = 'Baixa' 
                    and   movto_bem_pat.ind_orig_calc_bem_pat <> "Transferància" /*l_transferencia*/  no-lock no-error.
                if  avail movto_bem_pat then do:
                    if  movto_bem_pat.dat_movto_bem_pat <= v_dat_fim_period then do:
                        next block.
                    end.
                end.
                assign v_val_percentual = 0.
            end /* if */.
            find first tt_cta_pat_sit_geral 
                 where tt_cta_pat_sit_geral.tta_cod_empresa  = bem_pat.cod_empresa
                 and   tt_cta_pat_sit_geral.tta_cod_cta_pat  = bem_pat.cod_cta_pat
                 and   tt_cta_pat_sit_geral.tta_cod_grp_calc = bem_pat.cod_grp_calc
                 no-lock no-error.
            if  not avail tt_cta_pat_sit_geral
            then do:
               find first cta_pat 
                    where cta_pat.cod_empresa  = bem_pat.cod_empresa
                    and   cta_pat.cod_cta_pat  = bem_pat.cod_cta_pat
                    and   cta_pat.cod_grp_calc = bem_pat.cod_grp_calc
                    no-lock no-error.
               if  avail cta_pat
               then do:
                  create tt_cta_pat_sit_geral.
                  update tt_cta_pat_sit_geral.tta_cod_empresa      = cta_pat.cod_empresa
                         tt_cta_pat_sit_geral.tta_cod_cta_pat      = cta_pat.cod_cta_pat
                         tt_cta_pat_sit_geral.tta_cod_grp_calc     = cta_pat.cod_grp_calc
                         tt_cta_pat_sit_geral.tta_log_control_ctbl = cta_pat.log_control_ctbl.
               end /* if */.
            end /* if */.
            if  avail tt_cta_pat_sit_geral
            then do:
                 if  (tt_cta_pat_sit_geral.tta_log_control_ctbl = no) and (v_log_bem_imobdo = no)
                 then do:
                    next block.
                end /* if */.
            end /* if */.

            find first reg_calc_bem_pat of bem_pat 
                where reg_calc_bem_pat.num_seq_incorp_bem_pat = 0 
                and reg_calc_bem_pat.ind_trans = "Implantaá∆o" /*l_implantacao*/  no-lock no-error.

            if  avail reg_calc_bem_pat and reg_calc_bem_pat.dat_calc_pat > v_dat_fim_period
            then do: 
               next block.
            end /* if */.

            if  v_ind_dwb_run_mode = "Batch" /*l_batch*/ 
            then do:
                assign v_cod_ult_obj_procesdo = bem_pat.cod_cta_pat + "," +
                                                string(bem_pat.num_bem_pat) + "," +
                                                string(bem_pat.num_seq_bem_pat).
                run prgtec/btb/btb908ze.py (Input 1,
                                            Input v_cod_ult_obj_procesdo) /*prg_api_atualizar_ult_obj*/.
            end /* if */.
            run pi_tratar_tt_rpt_bem_pat_sit_geral_pat /*pi_tratar_tt_rpt_bem_pat_sit_geral_pat*/.
        end /* for block */.
    end /* if */.
    if  dwb_rpt_select.cod_dwb_field = "Descriá∆o Bem Pat"
    then do:
        block:
        for each bem_pat no-lock
         where bem_pat.cod_empresa = v_cod_empres_usuar
           and bem_pat.des_bem_pat >= dwb_rpt_select.cod_dwb_initial
           and bem_pat.des_bem_pat <= dwb_rpt_select.cod_dwb_final /*cl_rpt_bem_pat_descr of bem_pat*/:
            if  v_log_consid_bem_bxado = no and bem_pat.val_perc_bxa >= 100
            then do:
                find last movto_bem_pat
                    where movto_bem_pat.num_id_bem_pat = bem_pat.num_id_bem_pat 
                    and   movto_bem_pat.num_seq_incorp_bem_pat = 0 
                    and   movto_bem_pat.ind_trans_calc_bem_pat = 'Baixa' 
                    and   movto_bem_pat.ind_orig_calc_bem_pat <> "Transferància" /*l_transferencia*/  no-lock no-error.
                if  avail movto_bem_pat then do:
                    if  movto_bem_pat.dat_movto_bem_pat <= v_dat_fim_period then do:
                        next block.
                    end.
                end.
                assign v_val_percentual = 0.
            end /* if */.
            find first tt_cta_pat_sit_geral 
                 where tt_cta_pat_sit_geral.tta_cod_empresa  = bem_pat.cod_empresa
                 and   tt_cta_pat_sit_geral.tta_cod_cta_pat  = bem_pat.cod_cta_pat
                 and   tt_cta_pat_sit_geral.tta_cod_grp_calc = bem_pat.cod_grp_calc
                 no-lock no-error.
            if  not avail tt_cta_pat_sit_geral
            then do:
                find first cta_pat 
                     where cta_pat.cod_empresa  = bem_pat.cod_empresa
                     and   cta_pat.cod_cta_pat  = bem_pat.cod_cta_pat
                     and   cta_pat.cod_grp_calc = bem_pat.cod_grp_calc
                     no-lock no-error.
                if  avail cta_pat
                then do:
                    create tt_cta_pat_sit_geral.
                    update tt_cta_pat_sit_geral.tta_cod_empresa      = cta_pat.cod_empresa
                           tt_cta_pat_sit_geral.tta_cod_cta_pat      = cta_pat.cod_cta_pat
                           tt_cta_pat_sit_geral.tta_cod_grp_calc     = cta_pat.cod_grp_calc
                           tt_cta_pat_sit_geral.tta_log_control_ctbl = cta_pat.log_control_ctbl.
                end /* if */.
            end /* if */.
            if  avail tt_cta_pat_sit_geral
            then do:
                if  (tt_cta_pat_sit_geral.tta_log_control_ctbl = no) and (v_log_bem_imobdo = no)
                then do:
                    next block.
                end /* if */.
            end /* if */.

            find first reg_calc_bem_pat of bem_pat 
                where reg_calc_bem_pat.num_seq_incorp_bem_pat = 0 
                and reg_calc_bem_pat.ind_trans = "Implantaá∆o" /*l_implantacao*/  no-lock no-error.

            if  avail reg_calc_bem_pat and reg_calc_bem_pat.dat_calc_pat > v_dat_fim_period
            then do: 
               next block.
            end /* if */.

            if  v_ind_dwb_run_mode = "Batch" /*l_batch*/ 
            then do:
                assign v_cod_ult_obj_procesdo = bem_pat.cod_cta_pat + "," +
                                                string(bem_pat.num_bem_pat) + "," +
                                                string(bem_pat.num_seq_bem_pat).
                run prgtec/btb/btb908ze.py (Input 1,
                                            Input v_cod_ult_obj_procesdo) /*prg_api_atualizar_ult_obj*/.
            end /* if */.
            run pi_tratar_tt_rpt_bem_pat_sit_geral_pat /*pi_tratar_tt_rpt_bem_pat_sit_geral_pat*/.
        end /* for block */.
    end /* if */.
    if  dwb_rpt_select.cod_dwb_field = "Data Aquisiá∆o"
    then do:
        block:
        for each bem_pat no-lock
         where bem_pat.cod_empresa = v_cod_empres_usuar
           and bem_pat.dat_aquis_bem_pat >= date(dwb_rpt_select.cod_dwb_initial)
           and bem_pat.dat_aquis_bem_pat <= date(dwb_rpt_select.cod_dwb_final) /*cl_rpt_bem_pat_dat_aquis of bem_pat*/:
            if  v_log_consid_bem_bxado = no and bem_pat.val_perc_bxa >= 100
            then do:
                find last movto_bem_pat
                    where movto_bem_pat.num_id_bem_pat = bem_pat.num_id_bem_pat 
                    and   movto_bem_pat.num_seq_incorp_bem_pat = 0 
                    and   movto_bem_pat.ind_trans_calc_bem_pat = 'Baixa'
                    and   movto_bem_pat.ind_orig_calc_bem_pat <> "Transferància" /*l_transferencia*/  no-lock no-error.
                if  avail movto_bem_pat then do:
                    if  movto_bem_pat.dat_movto_bem_pat <= v_dat_fim_period then do:
                        next block.
                    end.
                end.
                assign v_val_percentual = 0.
            end /* if */.
            find first tt_cta_pat_sit_geral 
                 where tt_cta_pat_sit_geral.tta_cod_empresa  = bem_pat.cod_empresa
                 and   tt_cta_pat_sit_geral.tta_cod_cta_pat  = bem_pat.cod_cta_pat
                 and   tt_cta_pat_sit_geral.tta_cod_grp_calc = bem_pat.cod_grp_calc
                 no-lock no-error.
            if  not avail tt_cta_pat_sit_geral
            then do:
               find first cta_pat 
                    where cta_pat.cod_empresa  = bem_pat.cod_empresa
                    and   cta_pat.cod_cta_pat  = bem_pat.cod_cta_pat
                    and   cta_pat.cod_grp_calc = bem_pat.cod_grp_calc
                    no-lock no-error.
               if  avail cta_pat
               then do:
                    create tt_cta_pat_sit_geral.
                    update tt_cta_pat_sit_geral.tta_cod_empresa      = cta_pat.cod_empresa
                           tt_cta_pat_sit_geral.tta_cod_cta_pat      = cta_pat.cod_cta_pat
                           tt_cta_pat_sit_geral.tta_cod_grp_calc     = cta_pat.cod_grp_calc
                           tt_cta_pat_sit_geral.tta_log_control_ctbl = cta_pat.log_control_ctbl.
               end /* if */.
            end /* if */.
            if  avail tt_cta_pat_sit_geral
            then do:
                if  (tt_cta_pat_sit_geral.tta_log_control_ctbl = no) and (v_log_bem_imobdo = no)
                then do:
                    next block.
                end /* if */.
            end /* if */.

            find first reg_calc_bem_pat of bem_pat 
                where reg_calc_bem_pat.num_seq_incorp_bem_pat = 0 
                and reg_calc_bem_pat.ind_trans = "Implantaá∆o" /*l_implantacao*/  no-lock no-error.

            if  avail reg_calc_bem_pat and reg_calc_bem_pat.dat_calc_pat > v_dat_fim_period
            then do: 
               next block.
            end /* if */.

            if  v_ind_dwb_run_mode = "Batch" /*l_batch*/ 
            then do:
                assign v_cod_ult_obj_procesdo = bem_pat.cod_cta_pat + "," +
                                                string(bem_pat.num_bem_pat) + "," +
                                                string(bem_pat.num_seq_bem_pat).
                run prgtec/btb/btb908ze.py (Input 1,
                                            Input v_cod_ult_obj_procesdo) /*prg_api_atualizar_ult_obj*/.
            end /* if */.
            run pi_tratar_tt_rpt_bem_pat_sit_geral_pat /*pi_tratar_tt_rpt_bem_pat_sit_geral_pat*/.
        end /* for block */.
    end /* if */.
    if  dwb_rpt_select.cod_dwb_field = "Data C†lculo"
    then do:
        block:
        for each bem_pat no-lock
         where bem_pat.cod_empresa = v_cod_empres_usuar
           and bem_pat.dat_calc_pat >= date(dwb_rpt_select.cod_dwb_initial)
           and bem_pat.dat_calc_pat <= date(dwb_rpt_select.cod_dwb_final) /*cl_rpt_bem_pat_dat_calc of bem_pat*/:
            if  v_log_consid_bem_bxado = no and bem_pat.val_perc_bxa >= 100
            then do:
                find last movto_bem_pat
                    where movto_bem_pat.num_id_bem_pat = bem_pat.num_id_bem_pat 
                    and   movto_bem_pat.num_seq_incorp_bem_pat = 0 
                    and   movto_bem_pat.ind_trans_calc_bem_pat = 'Baixa' 
                    and   movto_bem_pat.ind_orig_calc_bem_pat <> "Transferància" /*l_transferencia*/  no-lock no-error.
                if  avail movto_bem_pat then do:
                    if  movto_bem_pat.dat_movto_bem_pat <= v_dat_fim_period then do:
                        next block.
                    end.
                end.
                if v_val_percentual >= 100 then do:
                    assign v_val_percentual = 0.
                    next block.
                end.
                assign v_val_percentual = 0.
            end /* if */.
            find first tt_cta_pat_sit_geral 
                 where tt_cta_pat_sit_geral.tta_cod_empresa  = bem_pat.cod_empresa
                 and   tt_cta_pat_sit_geral.tta_cod_cta_pat  = bem_pat.cod_cta_pat
                 and   tt_cta_pat_sit_geral.tta_cod_grp_calc = bem_pat.cod_grp_calc
                 no-lock no-error.
            if  not avail tt_cta_pat_sit_geral
            then do:
               find first cta_pat 
                    where cta_pat.cod_empresa  = bem_pat.cod_empresa
                    and   cta_pat.cod_cta_pat  = bem_pat.cod_cta_pat
                    and   cta_pat.cod_grp_calc = bem_pat.cod_grp_calc
                    no-lock no-error.
               if  avail cta_pat
               then do:
                    create tt_cta_pat_sit_geral.
                    update tt_cta_pat_sit_geral.tta_cod_empresa      = cta_pat.cod_empresa
                           tt_cta_pat_sit_geral.tta_cod_cta_pat      = cta_pat.cod_cta_pat
                           tt_cta_pat_sit_geral.tta_cod_grp_calc     = cta_pat.cod_grp_calc
                           tt_cta_pat_sit_geral.tta_log_control_ctbl = cta_pat.log_control_ctbl.
               end /* if */.
            end /* if */.
            if  avail tt_cta_pat_sit_geral
            then do:
                if  (tt_cta_pat_sit_geral.tta_log_control_ctbl = no) and (v_log_bem_imobdo = no)
                then do:
                    next block.
                end /* if */.
            end /* if */.

            find first reg_calc_bem_pat of bem_pat 
                where reg_calc_bem_pat.num_seq_incorp_bem_pat = 0 
                and reg_calc_bem_pat.ind_trans = "Implantaá∆o" /*l_implantacao*/  no-lock no-error.

            if  avail reg_calc_bem_pat and reg_calc_bem_pat.dat_calc_pat > v_dat_fim_period
            then do: 
               next block.
            end /* if */.

            if  v_ind_dwb_run_mode = "Batch" /*l_batch*/ 
            then do:
                assign v_cod_ult_obj_procesdo = bem_pat.cod_cta_pat + "," +
                                                string(bem_pat.num_bem_pat) + "," +
                                                string(bem_pat.num_seq_bem_pat).
                run prgtec/btb/btb908ze.py (Input 1,
                                            Input v_cod_ult_obj_procesdo) /*prg_api_atualizar_ult_obj*/.
            end /* if */.
            run pi_tratar_tt_rpt_bem_pat_sit_geral_pat /*pi_tratar_tt_rpt_bem_pat_sit_geral_pat*/.
        end /* for block */.
    end /* if */.
    if  dwb_rpt_select.cod_dwb_field = &IF '{&emsfin_version}' >= '5.01' AND '{&emsfin_version}' <= '5.07' &THEN "Estabelecimento" &ELSE "Estabelecimento" &ENDIF
    then do:
        block:
        for each  b_movto_bem_pat_transf no-lock use-index mvtbmpt_estab
            where b_movto_bem_pat_transf.cod_estab            >= dwb_rpt_select.cod_dwb_initial
            and   b_movto_bem_pat_transf.cod_estab            <= dwb_rpt_select.cod_dwb_final
            and   b_movto_bem_pat_transf.num_seq_incorp_bem_pat = 0
            and   b_movto_bem_pat_transf.ind_orig_calc_bem_pat = "Transferància" /*l_transferencia*/ 
            and   b_movto_bem_pat_transf.dat_movto_bem_pat     > v_dat_fim_period:

            find bem_pat no-lock
                where bem_pat.num_id_bem_pat      = b_movto_bem_pat_transf.num_id_bem_pat
                no-error.
            if not avail bem_pat
            or bem_pat.cod_empresa  <> v_cod_empres_usuar then        
                next block.

            /* procura o pr¢ximo movimento de transferencia ap¢s a data fim, se for diferente da faixa de estabelecimento, n∆o deve ser considerado */

            find first b_movto_bem_pat no-lock use-index mvtbmpt_estorn
                where b_movto_bem_pat.num_id_bem_pat         = b_movto_bem_pat_transf.num_id_bem_pat
                and   b_movto_bem_pat.num_seq_incorp_bem_pat = 0
                and   b_movto_bem_pat.log_estorn_movto_bem_pat = no
                and   b_movto_bem_pat.dat_movto_bem_pat      > v_dat_fim_period
                and   b_movto_bem_pat_transf.ind_orig_calc_bem_pat = "Transferància" /*l_transferencia*/  
                no-error.
            if  not avail b_movto_bem_pat      
            or  b_movto_bem_pat.cod_estab     < dwb_rpt_select.cod_dwb_initial
            or  b_movto_bem_pat.cod_estab     > dwb_rpt_select.cod_dwb_final then
                next block.

            if  v_log_consid_bem_bxado = no and bem_pat.val_perc_bxa >= 100
            then do:
                find last movto_bem_pat
                where movto_bem_pat.num_id_bem_pat = bem_pat.num_id_bem_pat 
                and   movto_bem_pat.num_seq_incorp_bem_pat = 0 
                and   movto_bem_pat.log_estorn_movto_bem_pat = no
                and   movto_bem_pat.dat_movto_bem_pat <= v_dat_fim_period
                and   movto_bem_pat.ind_trans_calc_bem_pat = 'Baixa' 
                and   movto_bem_pat.ind_orig_calc_bem_pat <> "Transferància" /*l_transferencia*/  no-lock no-error.

                if avail movto_bem_pat then do:         
                    assign v_val_perc_aux = v_val_perc_aux +  movto_bem_pat.val_perc_movto_bem_pat. 
                    if  v_val_perc_aux >= 100 then 
                        next block.
                end.        
                assign v_val_percentual = 0.
            end /* if */.
            find first tt_cta_pat_sit_geral 
                 where tt_cta_pat_sit_geral.tta_cod_empresa  = bem_pat.cod_empresa
                 and   tt_cta_pat_sit_geral.tta_cod_cta_pat  = bem_pat.cod_cta_pat
                 and   tt_cta_pat_sit_geral.tta_cod_grp_calc = bem_pat.cod_grp_calc
                 no-lock no-error.
            if  not avail tt_cta_pat_sit_geral
            then do:
               find first cta_pat 
                    where cta_pat.cod_empresa  = bem_pat.cod_empresa
                    and   cta_pat.cod_cta_pat  = bem_pat.cod_cta_pat
                    and   cta_pat.cod_grp_calc = bem_pat.cod_grp_calc
                    no-lock no-error.
               if  avail cta_pat
               then do:
                    create tt_cta_pat_sit_geral.
                    assign tt_cta_pat_sit_geral.tta_cod_empresa      = cta_pat.cod_empresa
                           tt_cta_pat_sit_geral.tta_cod_cta_pat      = cta_pat.cod_cta_pat
                           tt_cta_pat_sit_geral.tta_cod_grp_calc     = cta_pat.cod_grp_calc
                           tt_cta_pat_sit_geral.tta_log_control_ctbl = cta_pat.log_control_ctbl.
               end /* if */.
            end /* if */.
            if  avail tt_cta_pat_sit_geral
            then do:
                if  (tt_cta_pat_sit_geral.tta_log_control_ctbl = no) and (v_log_bem_imobdo = no)
                then do:
                    next block.
                end /* if */.
            end /* if */.

            find first reg_calc_bem_pat of bem_pat 
                where reg_calc_bem_pat.num_seq_incorp_bem_pat = 0 
                and reg_calc_bem_pat.ind_trans = "Implantaá∆o" /*l_implantacao*/  no-lock no-error.

            if  avail reg_calc_bem_pat and reg_calc_bem_pat.dat_calc_pat > v_dat_fim_period
            then do: 
               next block.
            end /* if */.

            if  v_ind_dwb_run_mode = "Batch" /*l_batch*/ 
            then do:
                assign v_cod_ult_obj_procesdo = bem_pat.cod_cta_pat + "," +
                                                string(bem_pat.num_bem_pat) + "," +
                                                string(bem_pat.num_seq_bem_pat).
                run prgtec/btb/btb908ze.py (Input 1,
                                            Input v_cod_ult_obj_procesdo) /*prg_api_atualizar_ult_obj*/.
            end /* if */.
            run pi_tratar_tt_rpt_bem_pat_sit_geral_pat /*pi_tratar_tt_rpt_bem_pat_sit_geral_pat*/.
        end.

        block:
        for each bem_pat no-lock
         where bem_pat.cod_empresa = v_cod_empres_usuar
           and bem_pat.cod_estab >= dwb_rpt_select.cod_dwb_initial
           and bem_pat.cod_estab <= dwb_rpt_select.cod_dwb_final /*cl_rpt_bem_pat_estab of bem_pat*/:
            find first movto_bem_pat no-lock use-index mvtbmpt_estorn
                where movto_bem_pat.num_id_bem_pat         = bem_pat.num_id_bem_pat
                and   movto_bem_pat.num_seq_incorp_bem_pat = 0
                and   movto_bem_pat.log_estorn_movto_bem_pat = no
                and   movto_bem_pat.dat_movto_bem_pat      > v_dat_fim_period
                and   movto_bem_pat.ind_orig_calc_bem_pat  = "Transferància" /*l_transferencia*/   no-error.
            if  avail movto_bem_pat      
            and (movto_bem_pat.cod_estab     < dwb_rpt_select.cod_dwb_initial
            or  movto_bem_pat.cod_estab     > dwb_rpt_select.cod_dwb_final) then
                next block.

            if  v_log_consid_bem_bxado = no and bem_pat.val_perc_bxa >= 100
            then do:
                find last movto_bem_pat no-lock
                    where movto_bem_pat.num_id_bem_pat = bem_pat.num_id_bem_pat 
                    and   movto_bem_pat.num_seq_incorp_bem_pat = 0 
                    and   movto_bem_pat.log_estorn_movto_bem_pat = no
                    and   movto_bem_pat.dat_movto_bem_pat <= v_dat_fim_period
                    and   movto_bem_pat.ind_trans_calc_bem_pat = 'Baixa' 
                    and   movto_bem_pat.ind_orig_calc_bem_pat <> "Transferància" /*l_transferencia*/  no-error.
                if avail movto_bem_pat then do:    
                    assign v_val_perc_aux = v_val_perc_aux + movto_bem_pat.val_perc_movto_bem_pat.  
                    if  v_val_perc_aux >= 100 then 
                        next block.
                end.
                assign v_val_percentual = 0.
            end /* if */.
            find first tt_cta_pat_sit_geral 
                 where tt_cta_pat_sit_geral.tta_cod_empresa  = bem_pat.cod_empresa
                 and   tt_cta_pat_sit_geral.tta_cod_cta_pat  = bem_pat.cod_cta_pat
                 and   tt_cta_pat_sit_geral.tta_cod_grp_calc = bem_pat.cod_grp_calc
                 no-lock no-error.
            if  not avail tt_cta_pat_sit_geral
            then do:
               find first cta_pat 
                    where cta_pat.cod_empresa  = bem_pat.cod_empresa
                    and   cta_pat.cod_cta_pat  = bem_pat.cod_cta_pat
                    and   cta_pat.cod_grp_calc = bem_pat.cod_grp_calc
                    no-lock no-error.
               if  avail cta_pat
               then do:
                   create tt_cta_pat_sit_geral.
                   assign tt_cta_pat_sit_geral.tta_cod_empresa      = cta_pat.cod_empresa
                          tt_cta_pat_sit_geral.tta_cod_cta_pat      = cta_pat.cod_cta_pat
                          tt_cta_pat_sit_geral.tta_cod_grp_calc     = cta_pat.cod_grp_calc
                          tt_cta_pat_sit_geral.tta_log_control_ctbl = cta_pat.log_control_ctbl.
               end /* if */.
            end /* if */.
            if  avail tt_cta_pat_sit_geral
            then do:
                if  (tt_cta_pat_sit_geral.tta_log_control_ctbl = no) and (v_log_bem_imobdo = no)
                then do:
                    next block.
                end /* if */.
            end /* if */.

            find first reg_calc_bem_pat of bem_pat 
                where reg_calc_bem_pat.num_seq_incorp_bem_pat = 0 
                and reg_calc_bem_pat.ind_trans = "Implantaá∆o" /*l_implantacao*/  no-lock no-error.

            if  avail reg_calc_bem_pat and reg_calc_bem_pat.dat_calc_pat > v_dat_fim_period
            then do: 
               next block.
            end /* if */.

            if  v_ind_dwb_run_mode = "Batch" /*l_batch*/ 
            then do:
                assign v_cod_ult_obj_procesdo = bem_pat.cod_cta_pat + "," +
                                                string(bem_pat.num_bem_pat) + "," +
                                                string(bem_pat.num_seq_bem_pat).
                run prgtec/btb/btb908ze.py (Input 1,
                                            Input v_cod_ult_obj_procesdo) /*prg_api_atualizar_ult_obj*/.
            end /* if */.
            run pi_tratar_tt_rpt_bem_pat_sit_geral_pat /*pi_tratar_tt_rpt_bem_pat_sit_geral_pat*/.
        end /* for block */.
    end /* if */.

    run pi_ler_tt_rpt_bem_pat_sit_geral_pat_regra2 /*pi_ler_tt_rpt_bem_pat_sit_geral_pat_regra2*/.

END PROCEDURE. /* pi_ler_tt_rpt_bem_pat_sit_geral_pat_regra */
/*****************************************************************************
** Procedure Interna.....: pi_rpt_bem_pat_sit_geral_pat_2_mais
** Descricao.............: pi_rpt_bem_pat_sit_geral_pat_2_mais
** Criado por............: daniela
** Criado em.............: 31/07/2000 16:16:42
** Alterado por..........: fut33243
** Alterado em...........: 22/12/2010 17:23:04
*****************************************************************************/
PROCEDURE pi_rpt_bem_pat_sit_geral_pat_2_mais:

    if  entry(1, dwb_rpt_param.cod_dwb_order) = "Conta Patrimonial"
    then do:  /* weniton */
        if  v_log_impr_narrat = no
        then do:
            if  not avail(sdo_bem_pat)
            then do:
                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    tt_rpt_bem_pat.num_bem_pat to 9 format ">>>>>>>>9"
                    tt_rpt_bem_pat.num_seq_bem_pat to 15 format ">>>>9"
                    tt_rpt_bem_pat.des_bem_pat at 17 format "x(40)"
                    tt_rpt_bem_pat.dat_aquis_bem_pat at 58 format "99/99/9999"
                    tt_rpt_bem_pat.dat_calc_pat at 69 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                    tt_rpt_bem_pat.cod_estab at 80 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                    tt_rpt_bem_pat.cod_estab at 80 format "x(5)"
    &ENDIF
                    tt_rpt_bem_pat.cod_unid_negoc at 86 format "x(3)"
                    tt_rpt_bem_pat.cod_plano_ccusto at 104 format "x(8)"
                    tt_rpt_bem_pat.cod_ccusto_respons at 117 format "x(11)" skip.
            end /* if */.
            else do:
                if  v_log_quant_bem then do:
                    if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        tt_rpt_bem_pat.num_bem_pat to 9 format ">>>>>>>>9"
                        tt_rpt_bem_pat.num_seq_bem_pat to 15 format ">>>>9"
                        tt_rpt_bem_pat.des_bem_pat at 17 format "x(40)"
                        tt_rpt_bem_pat.dat_aquis_bem_pat at 58 format "99/99/9999"
                        tt_rpt_bem_pat.dat_calc_pat at 69 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                        tt_rpt_bem_pat.cod_estab at 80 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        tt_rpt_bem_pat.cod_estab at 80 format "x(5)"
    &ENDIF
                        tt_rpt_bem_pat.cod_unid_negoc at 86 format "x(3)"
                        sdo_bem_pat.dat_sdo_bem_pat at 93 format "99/99/9999"
                        tt_rpt_bem_pat.cod_plano_ccusto at 104 format "x(8)"
                        tt_rpt_bem_pat.cod_ccusto_respons at 117 format "x(11)"
                        v_des_val_imobdo_orig at 148 format "x(23)"
                        v_des_val_dpr_aux at 172 format "x(23)".
    put stream s_1 unformatted 
                        v_des_val_amort_aux at 197 format "x(23)"
                        v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                        tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip.
                end.
                else do:
                    if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        tt_rpt_bem_pat.num_bem_pat to 9 format ">>>>>>>>9"
                        tt_rpt_bem_pat.num_seq_bem_pat to 15 format ">>>>9"
                        tt_rpt_bem_pat.des_bem_pat at 17 format "x(40)"
                        tt_rpt_bem_pat.dat_aquis_bem_pat at 58 format "99/99/9999"
                        tt_rpt_bem_pat.dat_calc_pat at 69 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                        tt_rpt_bem_pat.cod_estab at 80 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        tt_rpt_bem_pat.cod_estab at 80 format "x(5)"
    &ENDIF
                        tt_rpt_bem_pat.cod_unid_negoc at 86 format "x(3)"
                        sdo_bem_pat.dat_sdo_bem_pat at 93 format "99/99/9999"
                        tt_rpt_bem_pat.cod_plano_ccusto at 104 format "x(8)"
                        tt_rpt_bem_pat.cod_ccusto_respons at 117 format "x(11)"
                        v_des_val_imobdo_orig at 148 format "x(23)"
                        v_des_val_dpr_aux at 172 format "x(23)".
    put stream s_1 unformatted 
                        v_des_val_amort_aux at 197 format "x(23)"
                        v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip.
                end.
            end /* else */.
        end /* if */.
        else do:
            if  not avail(sdo_bem_pat)
            then do:
                run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                put stream s_1 unformatted 
                    tt_rpt_bem_pat.num_bem_pat to 9 format ">>>>>>>>9"
                    tt_rpt_bem_pat.num_seq_bem_pat to 15 format ">>>>9"
                    tt_rpt_bem_pat.des_bem_pat at 17 format "x(40)"
                    tt_rpt_bem_pat.dat_aquis_bem_pat at 58 format "99/99/9999"
                    tt_rpt_bem_pat.dat_calc_pat at 69 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                    tt_rpt_bem_pat.cod_estab at 80 format "x(3)"
    &ENDIF.
    put stream s_1 unformatted 
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                    tt_rpt_bem_pat.cod_estab at 80 format "x(5)"
    &ENDIF
                    tt_rpt_bem_pat.cod_unid_negoc at 86 format "x(3)"
                    tt_rpt_bem_pat.cod_plano_ccusto at 104 format "x(8)"
                    tt_rpt_bem_pat.cod_ccusto_respons at 117 format "x(11)" skip
                    if avail tt_rpt_bem_pat then entry(1, return-value, chr(255)) else "" at 1 format "x(60)" skip.
                run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
            end /* if */.
            else do:
                if  v_log_quant_bem then do:
                    run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                    put stream s_1 unformatted 
                        tt_rpt_bem_pat.num_bem_pat to 9 format ">>>>>>>>9"
                        tt_rpt_bem_pat.num_seq_bem_pat to 15 format ">>>>9"
                        tt_rpt_bem_pat.des_bem_pat at 17 format "x(40)"
                        tt_rpt_bem_pat.dat_aquis_bem_pat at 58 format "99/99/9999"
                        tt_rpt_bem_pat.dat_calc_pat at 69 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                        tt_rpt_bem_pat.cod_estab at 80 format "x(3)"
    &ENDIF.
    put stream s_1 unformatted 
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        tt_rpt_bem_pat.cod_estab at 80 format "x(5)"
    &ENDIF
                        tt_rpt_bem_pat.cod_unid_negoc at 86 format "x(3)"
                        sdo_bem_pat.dat_sdo_bem_pat at 93 format "99/99/9999"
                        tt_rpt_bem_pat.cod_plano_ccusto at 104 format "x(8)"
                        tt_rpt_bem_pat.cod_ccusto_respons at 117 format "x(11)"
                        v_des_val_imobdo_orig at 148 format "x(23)"
                        v_des_val_dpr_aux at 172 format "x(23)"
                        v_des_val_amort_aux at 197 format "x(23)".
    put stream s_1 unformatted 
                        v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                        tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip
                        if avail tt_rpt_bem_pat then entry(1, return-value, chr(255)) else "" at 1 format "x(60)" skip.
                    run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                end.
                else do:
                    run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                    put stream s_1 unformatted 
                        tt_rpt_bem_pat.num_bem_pat to 9 format ">>>>>>>>9"
                        tt_rpt_bem_pat.num_seq_bem_pat to 15 format ">>>>9"
                        tt_rpt_bem_pat.des_bem_pat at 17 format "x(40)"
                        tt_rpt_bem_pat.dat_aquis_bem_pat at 58 format "99/99/9999"
                        tt_rpt_bem_pat.dat_calc_pat at 69 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                        tt_rpt_bem_pat.cod_estab at 80 format "x(3)"
    &ENDIF.
    put stream s_1 unformatted 
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        tt_rpt_bem_pat.cod_estab at 80 format "x(5)"
    &ENDIF
                        tt_rpt_bem_pat.cod_unid_negoc at 86 format "x(3)"
                        sdo_bem_pat.dat_sdo_bem_pat at 93 format "99/99/9999"
                        tt_rpt_bem_pat.cod_plano_ccusto at 104 format "x(8)"
                        tt_rpt_bem_pat.cod_ccusto_respons at 117 format "x(11)"
                        v_des_val_imobdo_orig at 148 format "x(23)"
                        v_des_val_dpr_aux at 172 format "x(23)"
                        v_des_val_amort_aux at 197 format "x(23)".
    put stream s_1 unformatted 
                        v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip
                        if avail tt_rpt_bem_pat then entry(1, return-value, chr(255)) else "" at 1 format "x(60)" skip.
                    run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                end.
            end /* else */.
        end /* else */.
    end /* if */.
    /* bem*/
    if  entry(1, dwb_rpt_param.cod_dwb_order) = "Bem Patrimonial"
    then do:
        if  v_num_bem_pat <> tt_rpt_bem_pat.num_bem_pat
        then do:
            if  v_log_impr_narrat = no
            then do:
                if  not avail(sdo_bem_pat)
                then do:
                    if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        tt_rpt_bem_pat.num_bem_pat to 9 format ">>>>>>>>9"
                        tt_rpt_bem_pat.num_seq_bem_pat to 15 format ">>>>9"
                        tt_rpt_bem_pat.cod_cta_pat at 17 format "x(18)"
                        tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                        tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                        tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN.
    put stream s_1 unformatted 
                        tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                        tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                        tt_rpt_bem_pat.cod_plano_ccusto at 121 format "x(8)"
                        tt_rpt_bem_pat.cod_ccusto_respons at 134 format "x(11)" skip.
                end /* if */.
                else do:
                    if  v_log_quant_bem then do:
                        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.num_bem_pat to 9 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 15 format ">>>>9"
                            tt_rpt_bem_pat.cod_cta_pat at 17 format "x(18)"
                            tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN.
    put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 110 format "99/99/9999"
                            tt_rpt_bem_pat.cod_plano_ccusto at 121 format "x(8)"
                            tt_rpt_bem_pat.cod_ccusto_respons at 134 format "x(11)"
                            v_des_val_imobdo_orig at 148 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_dpr_aux at 172 format "x(23)"
                            v_des_val_amort_aux at 197 format "x(23)"
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                            tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip.
                    end.
                    else do:
                        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.num_bem_pat to 9 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 15 format ">>>>9"
                            tt_rpt_bem_pat.cod_cta_pat at 17 format "x(18)"
                            tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN.
    put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 110 format "99/99/9999"
                            tt_rpt_bem_pat.cod_plano_ccusto at 121 format "x(8)"
                            tt_rpt_bem_pat.cod_ccusto_respons at 134 format "x(11)"
                            v_des_val_imobdo_orig at 148 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_dpr_aux at 172 format "x(23)"
                            v_des_val_amort_aux at 197 format "x(23)"
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip.
                    end.
                end /* else */.
            end /* if */.
            else do:
                if  not avail(sdo_bem_pat)
                then do:
                     run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                     put stream s_1 unformatted 
                         tt_rpt_bem_pat.num_bem_pat to 9 format ">>>>>>>>9"
                         tt_rpt_bem_pat.num_seq_bem_pat to 15 format ">>>>9"
                         tt_rpt_bem_pat.cod_cta_pat at 17 format "x(18)"
                         tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                         tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                         tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                         tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                         tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                         tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                         tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                         tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)" skip
                         entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                     run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                end /* if */.
                else do:
                     if  v_log_quant_bem then do:
                         run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                         put stream s_1 unformatted 
                             tt_rpt_bem_pat.num_bem_pat to 9 format ">>>>>>>>9"
                             tt_rpt_bem_pat.num_seq_bem_pat to 15 format ">>>>9"
                             tt_rpt_bem_pat.cod_cta_pat at 17 format "x(18)"
                             tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                             tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                             tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                             tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                             tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                             tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                             sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                             tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                             tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)"
                             v_des_val_imobdo_orig at 148 format "x(23)"
                             v_des_val_dpr_aux at 172 format "x(23)".
    put stream s_1 unformatted 
                             v_des_val_amort_aux at 197 format "x(23)"
                             v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                             tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip
                             entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                         run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                     end.
                     else do:
                         run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                         put stream s_1 unformatted 
                             tt_rpt_bem_pat.num_bem_pat to 9 format ">>>>>>>>9"
                             tt_rpt_bem_pat.num_seq_bem_pat to 15 format ">>>>9"
                             tt_rpt_bem_pat.cod_cta_pat at 17 format "x(18)"
                             tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                             tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                             tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                             tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                             tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                             tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                             sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                             tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                             tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)"
                             v_des_val_imobdo_orig at 148 format "x(23)"
                             v_des_val_dpr_aux at 172 format "x(23)".
    put stream s_1 unformatted 
                             v_des_val_amort_aux at 197 format "x(23)"
                             v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip
                             entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                         run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                     end.
                end /* else */.
            end /* else */.
            assign v_num_bem_pat = tt_rpt_bem_pat.num_bem_pat.
        end /* if */.
        else do:
            if  v_log_impr_narrat = no
            then do:
                if  not avail(sdo_bem_pat)
                then do:
                    if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        tt_rpt_bem_pat.num_seq_bem_pat to 15 format ">>>>9"
                        tt_rpt_bem_pat.cod_cta_pat at 17 format "x(18)"
                        tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                        tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                        tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                        tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                        tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                        tt_rpt_bem_pat.cod_plano_ccusto at 121 format "x(8)"
                        tt_rpt_bem_pat.cod_ccusto_respons at 134 format "x(11)" skip.
                end /* if */.
                else do:
                    if  v_log_quant_bem then do:
                        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.num_seq_bem_pat to 15 format ">>>>9"
                            tt_rpt_bem_pat.cod_cta_pat at 17 format "x(18)"
                            tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 110 format "99/99/9999"
                            tt_rpt_bem_pat.cod_plano_ccusto at 121 format "x(8)"
                            tt_rpt_bem_pat.cod_ccusto_respons at 134 format "x(11)"
                            v_des_val_imobdo_orig at 148 format "x(23)"
                            v_des_val_dpr_aux at 172 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_amort_aux at 197 format "x(23)"
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                            tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip. /* weniton */
                    end.
                    else do:
                        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.num_seq_bem_pat to 15 format ">>>>9"
                            tt_rpt_bem_pat.cod_cta_pat at 17 format "x(18)"
                            tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 110 format "99/99/9999"
                            tt_rpt_bem_pat.cod_plano_ccusto at 121 format "x(8)"
                            tt_rpt_bem_pat.cod_ccusto_respons at 134 format "x(11)"
                            v_des_val_imobdo_orig at 148 format "x(23)"
                            v_des_val_dpr_aux at 172 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_amort_aux at 197 format "x(23)"
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip.
                    end.
                end /* else */.
            end /* if */.
            else do:
                if  not avail(sdo_bem_pat)
                then do:
                    run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                    put stream s_1 unformatted 
                        tt_rpt_bem_pat.num_seq_bem_pat to 15 format ">>>>9"
                        tt_rpt_bem_pat.cod_cta_pat at 17 format "x(18)"
                        tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                        tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                        tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                        tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF.
    put stream s_1 unformatted 
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                        tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                        tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                        tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)" skip
                        entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                    run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                end /* if */.
                else do:
                    if  v_log_quant_bem then do:
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.num_seq_bem_pat to 15 format ">>>>9"
                            tt_rpt_bem_pat.cod_cta_pat at 17 format "x(18)"
                            tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF.
    put stream s_1 unformatted 
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                            tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                            tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)"
                            v_des_val_imobdo_orig at 148 format "x(23)"
                            v_des_val_dpr_aux at 172 format "x(23)"
                            v_des_val_amort_aux at 197 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                            tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip
                            entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                    end.
                    else do:
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.num_seq_bem_pat to 15 format ">>>>9"
                            tt_rpt_bem_pat.cod_cta_pat at 17 format "x(18)"
                            tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF.
    put stream s_1 unformatted 
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                            tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                            tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)"
                            v_des_val_imobdo_orig at 148 format "x(23)"
                            v_des_val_dpr_aux at 172 format "x(23)"
                            v_des_val_amort_aux at 197 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip
                            entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                    end.
                end /* else */.
            end /* else */.
        end /* else */.
    end /* if */.

    if  entry(1, dwb_rpt_param.cod_dwb_order) = "Descriá∆o Bem Pat"
    then do:
        if  v_log_impr_narrat = no
        then do:
            if  not avail(sdo_bem_pat)
            then do:
                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    tt_rpt_bem_pat.des_bem_pat at 1 format "x(40)"
                    tt_rpt_bem_pat.num_bem_pat to 50 format ">>>>>>>>9"
                    tt_rpt_bem_pat.num_seq_bem_pat to 56 format ">>>>9"
                    tt_rpt_bem_pat.cod_cta_pat at 58 format "x(18)"
                    tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                    tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN.
    put stream s_1 unformatted 
                    tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                    tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                    tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                    tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                    tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)" skip.
            end /* if */.
            else do:
                if  v_log_quant_bem then do:
                    if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        tt_rpt_bem_pat.des_bem_pat at 1 format "x(40)"
                        tt_rpt_bem_pat.num_bem_pat to 50 format ">>>>>>>>9"
                        tt_rpt_bem_pat.num_seq_bem_pat to 56 format ">>>>9"
                        tt_rpt_bem_pat.cod_cta_pat at 58 format "x(18)"
                        tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                        tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN.
    put stream s_1 unformatted 
                        tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                        tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                        sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                        tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                        tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)"
                        v_des_val_imobdo_orig at 148 format "x(23)".
    put stream s_1 unformatted 
                        v_des_val_dpr_aux at 172 format "x(23)"
                        v_des_val_amort_aux at 197 format "x(23)"
                        v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                        tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip.
                end.
                else do:
                    if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        tt_rpt_bem_pat.des_bem_pat at 1 format "x(40)"
                        tt_rpt_bem_pat.num_bem_pat to 50 format ">>>>>>>>9"
                        tt_rpt_bem_pat.num_seq_bem_pat to 56 format ">>>>9"
                        tt_rpt_bem_pat.cod_cta_pat at 58 format "x(18)"
                        tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                        tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN.
    put stream s_1 unformatted 
                        tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                        tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                        sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                        tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                        tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)"
                        v_des_val_imobdo_orig at 148 format "x(23)".
    put stream s_1 unformatted 
                        v_des_val_dpr_aux at 172 format "x(23)"
                        v_des_val_amort_aux at 197 format "x(23)"
                        v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip.
                end.
            end /* else */.
        end /* if */.
        else do:
            if  not avail(sdo_bem_pat)
            then do:
                run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                put stream s_1 unformatted 
                    tt_rpt_bem_pat.des_bem_pat at 1 format "x(40)"
                    tt_rpt_bem_pat.num_bem_pat to 50 format ">>>>>>>>9"
                    tt_rpt_bem_pat.num_seq_bem_pat to 56 format ">>>>9"
                    tt_rpt_bem_pat.cod_cta_pat at 58 format "x(18)"
                    tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                    tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                    tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                    tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                    tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                    tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                    tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)" skip
                    entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
            end /* if */.
            else do:
                if  v_log_quant_bem then do:
                    run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                    put stream s_1 unformatted 
                        tt_rpt_bem_pat.des_bem_pat at 1 format "x(40)"
                        tt_rpt_bem_pat.num_bem_pat to 50 format ">>>>>>>>9"
                        tt_rpt_bem_pat.num_seq_bem_pat to 56 format ">>>>9"
                        tt_rpt_bem_pat.cod_cta_pat at 58 format "x(18)"
                        tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                        tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                        tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                        tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                        sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                        tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                        tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)"
                        v_des_val_imobdo_orig at 148 format "x(23)"
                        v_des_val_dpr_aux at 172 format "x(23)".
    put stream s_1 unformatted 
                        v_des_val_amort_aux at 197 format "x(23)"
                        v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                        tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip
                        entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                    run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                end.
                else do:
                    run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                    put stream s_1 unformatted 
                        tt_rpt_bem_pat.des_bem_pat at 1 format "x(40)"
                        tt_rpt_bem_pat.num_bem_pat to 50 format ">>>>>>>>9"
                        tt_rpt_bem_pat.num_seq_bem_pat to 56 format ">>>>9"
                        tt_rpt_bem_pat.cod_cta_pat at 58 format "x(18)"
                        tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                        tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                        tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                        tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                        sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                        tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                        tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)"
                        v_des_val_imobdo_orig at 148 format "x(23)"
                        v_des_val_dpr_aux at 172 format "x(23)".
    put stream s_1 unformatted 
                        v_des_val_amort_aux at 197 format "x(23)"
                        v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip
                        entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                    run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                end.
            end /* else */.
        end /* else */.
    end /* if */.
    if  entry(1, dwb_rpt_param.cod_dwb_order) = "Data Aquisiá∆o"
    then do:
        if  v_dat_aquis_bem_pat <> tt_rpt_bem_pat.dat_aquis_bem_pat
        then do:
            if  v_log_impr_narrat = no
            then do:
                if  not avail(sdo_bem_pat)
                then do:
                    if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        tt_rpt_bem_pat.dat_aquis_bem_pat at 1 format "99/99/9999"
                        tt_rpt_bem_pat.cod_cta_pat at 12 format "x(18)"
                        tt_rpt_bem_pat.num_bem_pat to 39 format ">>>>>>>>9"
                        tt_rpt_bem_pat.num_seq_bem_pat to 45 format ">>>>9"
                        tt_rpt_bem_pat.des_bem_pat at 47 format "x(40)"
                        tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN.
    put stream s_1 unformatted 
                        tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                        tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                        tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                        tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)" skip.
                end /* if */.
                else do:
                    if  v_log_quant_bem then do:
                        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 1 format "99/99/9999"
                            tt_rpt_bem_pat.cod_cta_pat at 12 format "x(18)"
                            tt_rpt_bem_pat.num_bem_pat to 39 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 45 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 47 format "x(40)"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN.
    put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                            tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                            tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)"
                            v_des_val_imobdo_orig at 148 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_dpr_aux at 172 format "x(23)"
                            v_des_val_amort_aux at 197 format "x(23)"
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                            tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip.
                    end.
                    else do:
                        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 1 format "99/99/9999"
                            tt_rpt_bem_pat.cod_cta_pat at 12 format "x(18)"
                            tt_rpt_bem_pat.num_bem_pat to 39 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 45 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 47 format "x(40)"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN.
    put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                            tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                            tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)"
                            v_des_val_imobdo_orig at 148 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_dpr_aux at 172 format "x(23)"
                            v_des_val_amort_aux at 197 format "x(23)"
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip.
                    end.
                end /* else */.
            end /* if */.
            else do:
                if  not avail(sdo_bem_pat)
                then do:
                    run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                    put stream s_1 unformatted 
                        tt_rpt_bem_pat.dat_aquis_bem_pat at 1 format "99/99/9999"
                        tt_rpt_bem_pat.cod_cta_pat at 12 format "x(18)"
                        tt_rpt_bem_pat.num_bem_pat to 39 format ">>>>>>>>9"
                        tt_rpt_bem_pat.num_seq_bem_pat to 45 format ">>>>9"
                        tt_rpt_bem_pat.des_bem_pat at 47 format "x(40)"
                        tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                        tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                        tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                        tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                        tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)" skip
                        entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                    run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                end /* if */.
                else do:
                    if  v_log_quant_bem then do:
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 1 format "99/99/9999"
                            tt_rpt_bem_pat.cod_cta_pat at 12 format "x(18)"
                            tt_rpt_bem_pat.num_bem_pat to 39 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 45 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 47 format "x(40)"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                            tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                            tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)"
                            v_des_val_imobdo_orig at 148 format "x(23)"
                            v_des_val_dpr_aux at 172 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_amort_aux at 197 format "x(23)"
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                            tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip
                            entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                    end.
                    else do:
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 1 format "99/99/9999"
                            tt_rpt_bem_pat.cod_cta_pat at 12 format "x(18)"
                            tt_rpt_bem_pat.num_bem_pat to 39 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 45 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 47 format "x(40)"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                            tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                            tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)"
                            v_des_val_imobdo_orig at 148 format "x(23)"
                            v_des_val_dpr_aux at 172 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_amort_aux at 197 format "x(23)"
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip
                            entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                    end.
                end /* else */.
            end /* else */.
            assign v_dat_aquis_bem_pat = tt_rpt_bem_pat.dat_aquis_bem_pat.
        end /* if */.
        else do:
            if  v_log_impr_narrat = no
            then do:
                    if  v_log_quant_bem then do:
                        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_cta_pat at 12 format "x(18)"
                            tt_rpt_bem_pat.num_bem_pat to 39 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 45 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 47 format "x(40)"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                            tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                            tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)"
                            v_des_val_imobdo_orig at 148 format "x(23)"
                            v_des_val_dpr_aux at 172 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_amort_aux at 197 format "x(23)"
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                            tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip.
                    end.
                    else do:
                        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_cta_pat at 12 format "x(18)"
                            tt_rpt_bem_pat.num_bem_pat to 39 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 45 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 47 format "x(40)"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                            tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                            tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)"
                            v_des_val_imobdo_orig at 148 format "x(23)"
                            v_des_val_dpr_aux at 172 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_amort_aux at 197 format "x(23)"
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip.
                    end.
            end /* if */.
            else do:
                    if  v_log_quant_bem then do:
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_cta_pat at 12 format "x(18)"
                            tt_rpt_bem_pat.num_bem_pat to 39 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 45 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 47 format "x(40)"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    &ENDIF.
    put stream s_1 unformatted 
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                            tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                            tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)"
                            v_des_val_imobdo_orig at 148 format "x(23)"
                            v_des_val_dpr_aux at 172 format "x(23)"
                            v_des_val_amort_aux at 197 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                            tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip
                            entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                    end.
                    else do:
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_cta_pat at 12 format "x(18)"
                            tt_rpt_bem_pat.num_bem_pat to 39 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 45 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 47 format "x(40)"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF.
    put stream s_1 unformatted 
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                            tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                            tt_rpt_bem_pat.cod_ccusto_respons at 136 format "x(11)"
                            v_des_val_imobdo_orig at 148 format "x(23)"
                            v_des_val_dpr_aux at 172 format "x(23)"
                            v_des_val_amort_aux at 197 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip
                            entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                    end.
            end /* else */.
        end /* else */.
    end /* if */.
END PROCEDURE. /* pi_rpt_bem_pat_sit_geral_pat_2_mais */
/*****************************************************************************
** Procedure Interna.....: pi_rpt_bem_pat_sit_geral_pat_mais
** Descricao.............: pi_rpt_bem_pat_sit_geral_pat_mais
** Criado por............: daniela
** Criado em.............: 31/07/2000 18:24:52
** Alterado por..........: fut33243
** Alterado em...........: 22/12/2010 18:50:08
*****************************************************************************/
PROCEDURE pi_rpt_bem_pat_sit_geral_pat_mais:

    if  entry(1, dwb_rpt_param.cod_dwb_order) = "Plano Centros Custo"
    then do:
        if  v_cod_cta_pat <> tt_rpt_bem_pat.cod_cta_pat
        then do:
            if  v_log_impr_narrat = no
            then do:
                if  not avail(sdo_bem_pat)
                then do:
                    if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        tt_rpt_bem_pat.cod_cta_pat at 1 format "x(18)"
                        tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                        tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                        tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                        tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                        tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN.
    put stream s_1 unformatted 
                        tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                        tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                        tt_rpt_bem_pat.cod_ccusto_respons at 123 format "x(11)" skip.                
                end /* if */.
                else do:
                    if  v_log_quant_bem then do:
                        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_cta_pat at 1 format "x(18)"
                            tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN.
    put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                            tt_rpt_bem_pat.cod_ccusto_respons at 123 format "x(11)"
                            v_des_val_imobdo_orig at 148 format "x(23)"
                            v_des_val_dpr_aux at 172 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_amort_aux at 197 format "x(23)"
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                            tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip.
                    end.
                    else do:
                        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_cta_pat at 1 format "x(18)"
                            tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN.
    put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                            tt_rpt_bem_pat.cod_ccusto_respons at 123 format "x(11)"
                            v_des_val_imobdo_orig at 148 format "x(23)"
                            v_des_val_dpr_aux at 172 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_amort_aux at 197 format "x(23)"
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip.
                    end.
                end /* else */.                                          
            end /* if */.
            else do:
                if  not avail(sdo_bem_pat)
                then do:
                    run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                    put stream s_1 unformatted 
                        tt_rpt_bem_pat.cod_cta_pat at 1 format "x(18)"
                        tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                        tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                        tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                        tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                        tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                        tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                        tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                        tt_rpt_bem_pat.cod_ccusto_respons at 123 format "x(11)" skip
                        entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                    run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                end /* if */.
                else do:
                    if  v_log_quant_bem then do:     
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_cta_pat at 1 format "x(18)"
                            tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                            tt_rpt_bem_pat.cod_ccusto_respons at 123 format "x(11)"
                            v_des_val_imobdo_orig at 148 format "x(23)"
                            v_des_val_dpr_aux at 172 format "x(23)"
                            v_des_val_amort_aux at 197 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                            tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip
                            entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                    end.
                    else do:
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_cta_pat at 1 format "x(18)"
                            tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                            tt_rpt_bem_pat.cod_ccusto_respons at 123 format "x(11)"
                            v_des_val_imobdo_orig at 148 format "x(23)"
                            v_des_val_dpr_aux at 172 format "x(23)"
                            v_des_val_amort_aux at 197 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip
                            entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                    end.
                end /* else */.
            end /* else */.
            assign v_cod_cta_pat = tt_rpt_bem_pat.cod_cta_pat.
        end /* if */.
        else do:
            if  v_log_impr_narrat = no
            then do:
                    if  v_log_quant_bem then do: 
                        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                            tt_rpt_bem_pat.cod_ccusto_respons at 123 format "x(11)"
                            v_des_val_imobdo_orig at 148 format "x(23)"
                            v_des_val_dpr_aux at 172 format "x(23)"
                            v_des_val_amort_aux at 197 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                            tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip. /* weniton */
                    end.
                    else do:
                        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                            tt_rpt_bem_pat.cod_ccusto_respons at 123 format "x(11)"
                            v_des_val_imobdo_orig at 148 format "x(23)"
                            v_des_val_dpr_aux at 172 format "x(23)"
                            v_des_val_amort_aux at 197 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip. /* weniton */
                    end.
            end /* if */.
            else do:
                    if  v_log_quant_bem then do:
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF.
    put stream s_1 unformatted 
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                            tt_rpt_bem_pat.cod_ccusto_respons at 123 format "x(11)"
                            v_des_val_imobdo_orig at 148 format "x(23)"
                            v_des_val_dpr_aux at 172 format "x(23)"
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                            tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip.
    put stream s_1 unformatted 
                            entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", ""). /* weniton */
                    end.
                    else do:
                         run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                         put stream s_1 unformatted 
                             tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                             tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                             tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                             tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                             tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                             tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF.
    put stream s_1 unformatted 
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                             tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                             tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                             sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                             tt_rpt_bem_pat.cod_ccusto_respons at 123 format "x(11)"
                             v_des_val_imobdo_orig at 148 format "x(23)"
                             v_des_val_dpr_aux at 172 format "x(23)"
                             v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip
                             entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                         run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", ""). /* weniton */
                    end.
                end /* else */.
        end /* else */.
    end /* if */.
    if  entry(1, dwb_rpt_param.cod_dwb_order) = "CCusto Responsab"
    then do:
        if  v_cod_cta_pat <> tt_rpt_bem_pat.cod_cta_pat
        then do:
            if  v_log_impr_narrat = no
            then do:
                 if  not avail(sdo_bem_pat)
                 then do:
                    if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        tt_rpt_bem_pat.cod_cta_pat at 1 format "x(18)"
                        tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                        tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                        tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                        tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                        tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN.
    put stream s_1 unformatted 
                        tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                        tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                        tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)" skip.
                end /* if */.
                else do:
                    if  v_log_quant_bem then do:
                        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_cta_pat at 1 format "x(18)"
                            tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN.
    put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                            tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                            v_des_val_imobdo_orig at 148 format "x(23)"
                            v_des_val_dpr_aux at 172 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_amort_aux at 197 format "x(23)"
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                            tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip.
                    end.
                    else do:
                        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_cta_pat at 1 format "x(18)"
                            tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN.
    put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                            tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                            v_des_val_imobdo_orig at 148 format "x(23)"
                            v_des_val_dpr_aux at 172 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_amort_aux at 197 format "x(23)"
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip.
                    end.
                end /* else */.                                          
            end /* if */.
            else do:
                 if  not avail(sdo_bem_pat)
                 then do:
                    run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                    put stream s_1 unformatted 
                        tt_rpt_bem_pat.cod_cta_pat at 1 format "x(18)"
                        tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                        tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                        tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                        tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                        tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                        tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                        tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                        tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)" skip
                        entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                    run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                end /* if */.
                else do:
                    if  v_log_quant_bem then do:
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_cta_pat at 1 format "x(18)"
                            tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                            tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                            v_des_val_imobdo_orig at 148 format "x(23)"
                            v_des_val_dpr_aux at 172 format "x(23)"
                            v_des_val_amort_aux at 197 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                            tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip
                            entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                    end.
                    else do:
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.cod_cta_pat at 1 format "x(18)"
                            tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                            tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                            v_des_val_imobdo_orig at 148 format "x(23)"
                            v_des_val_dpr_aux at 172 format "x(23)"
                            v_des_val_amort_aux at 197 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip
                            entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", "").
                    end.
                end /* else */.                                          
            end /* else */.
        assign v_cod_cta_pat = tt_rpt_bem_pat.cod_cta_pat.
        end /* if */.
        else do:
            if  v_log_impr_narrat = no
            then do:
                    if  v_log_quant_bem then do:
                        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                            tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                            v_des_val_imobdo_orig at 148 format "x(23)"
                            v_des_val_dpr_aux at 172 format "x(23)"
                            v_des_val_amort_aux at 197 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)"
                            tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip. /* weniton */
                    end.
                    else do:
                        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                            tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                            v_des_val_imobdo_orig at 148 format "x(23)"
                            v_des_val_dpr_aux at 172 format "x(23)"
                            v_des_val_amort_aux at 197 format "x(23)".
    put stream s_1 unformatted 
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip. /* weniton */
                    end.
            end /* if */.
            else do:
                    if  v_log_quant_bem then do:
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                        put stream s_1 unformatted 
                            tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                            tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                            tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                            tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                            tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF.
    put stream s_1 unformatted 
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                            tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                            tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                            sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                            tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                            v_des_val_imobdo_orig at 148 format "x(23)"
                            v_des_val_dpr_aux at 172 format "x(23)"
                            v_des_val_amort_aux at 197 format "x(23)"
                            v_des_val_liq_bem_pat_aux at 222 format "x(23)".
    put stream s_1 unformatted 
                            tt_rpt_bem_pat.qtd_bem_pat_represen to 255 format ">>>>>>>>9" skip
                            entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                        run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", ""). /* weniton */
                    end.
                    else do:
                          run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "     060", "", "     ", "", "     ").
                          put stream s_1 unformatted 
                              tt_rpt_bem_pat.num_bem_pat to 28 format ">>>>>>>>9"
                              tt_rpt_bem_pat.num_seq_bem_pat to 34 format ">>>>9"
                              tt_rpt_bem_pat.des_bem_pat at 36 format "x(40)"
                              tt_rpt_bem_pat.dat_aquis_bem_pat at 77 format "99/99/9999"
                              tt_rpt_bem_pat.dat_calc_pat at 88 format "99/99/9999"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                              tt_rpt_bem_pat.cod_estab at 99 format "x(3)"
    &ENDIF.
    put stream s_1 unformatted 
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                              tt_rpt_bem_pat.cod_estab at 99 format "x(5)"
    &ENDIF
                              tt_rpt_bem_pat.cod_unid_negoc at 105 format "x(3)"
                              sdo_bem_pat.dat_sdo_bem_pat at 112 format "99/99/9999"
                              tt_rpt_bem_pat.cod_plano_ccusto at 123 format "x(8)"
                              v_des_val_imobdo_orig at 148 format "x(23)"
                              v_des_val_dpr_aux at 172 format "x(23)"
                              v_des_val_amort_aux at 197 format "x(23)"
                              v_des_val_liq_bem_pat_aux at 222 format "x(23)" skip.
    put stream s_1 unformatted 
                              entry(1, return-value, chr(255)) at 1 format "x(60)" skip.
                          run pi_print_editor ("s_1", tt_rpt_bem_pat.des_narrat_bem_pat, "at001060", "", "", "", ""). /* weniton */
                    end.
            end /* else */.
        end /* else */.
    end /* if */.
END PROCEDURE. /* pi_rpt_bem_pat_sit_geral_pat_mais */
/*****************************************************************************
** Procedure Interna.....: pi_ler_tt_rpt_bem_pat_sit_geral_pat_regra2
** Descricao.............: pi_ler_tt_rpt_bem_pat_sit_geral_pat_regra2
** Criado por............: src12115
** Criado em.............: 03/10/2001 09:20:44
** Alterado por..........: fut41162
** Alterado em...........: 23/02/2009 12:19:00
*****************************************************************************/
PROCEDURE pi_ler_tt_rpt_bem_pat_sit_geral_pat_regra2:

    /************************* Variable Definition Begin ************************/

    def var v_val_perc_aux
        as decimal
        format ">>9.99":U
        decimals 2
        label "Perc Abat"
        column-label "Perc Abat"
        no-undo.
    def var v_val_percentual                 as decimal         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    if  dwb_rpt_select.cod_dwb_field = "Unid Neg¢cio"
    then do:
        block:
        for each bem_pat no-lock
         where bem_pat.cod_empresa = v_cod_empres_usuar
           and bem_pat.cod_unid_negoc >= dwb_rpt_select.cod_dwb_initial
           and bem_pat.cod_unid_negoc <= dwb_rpt_select.cod_dwb_final /*cl_rpt_bem_pat_unid_negoc of bem_pat*/:
            if  v_log_consid_bem_bxado = no and bem_pat.val_perc_bxa >= 100
            then do:
                blk_movto_bem_pat:
                for each movto_bem_pat no-lock  where 
                     movto_bem_pat.num_id_bem_pat = bem_pat.num_id_bem_pat and
                     movto_bem_pat.num_seq_incorp_bem_pat = 0 and
                     movto_bem_pat.ind_trans_calc_bem_pat = "Baixa" /*l_baixa*/  and
                     movto_bem_pat.ind_orig_calc_bem_pat  <> "Transferància" /*l_transferencia*/  and
                     movto_bem_pat.dat_movto_bem_pat <= v_dat_fim_period:
                     assign v_val_perc_aux = v_val_perc_aux +  movto_bem_pat.val_perc_movto_bem_pat.
                end /* for blk_movto_bem_pat */.
                if v_val_perc_aux >= 100 then do:
                    assign v_val_percentual = 0.
                    next block.
                end.
                assign v_val_percentual = 0.
            end /* if */.
            find first tt_cta_pat_sit_geral 
                 where tt_cta_pat_sit_geral.tta_cod_empresa  = bem_pat.cod_empresa
                 and   tt_cta_pat_sit_geral.tta_cod_cta_pat  = bem_pat.cod_cta_pat
                 and   tt_cta_pat_sit_geral.tta_cod_grp_calc = bem_pat.cod_grp_calc
                 no-lock no-error.
            if  not avail tt_cta_pat_sit_geral
            then do:
               find first cta_pat 
                    where cta_pat.cod_empresa  = bem_pat.cod_empresa
                    and   cta_pat.cod_cta_pat  = bem_pat.cod_cta_pat
                    and   cta_pat.cod_grp_calc = bem_pat.cod_grp_calc
                    no-lock no-error.
               if  avail cta_pat
               then do:
                  create tt_cta_pat_sit_geral.
                  update tt_cta_pat_sit_geral.tta_cod_empresa      = cta_pat.cod_empresa
                         tt_cta_pat_sit_geral.tta_cod_cta_pat      = cta_pat.cod_cta_pat
                         tt_cta_pat_sit_geral.tta_cod_grp_calc     = cta_pat.cod_grp_calc
                         tt_cta_pat_sit_geral.tta_log_control_ctbl = cta_pat.log_control_ctbl.
               end /* if */.
            end /* if */.
            if  avail tt_cta_pat_sit_geral
            then do:
                if  (tt_cta_pat_sit_geral.tta_log_control_ctbl = no) and (v_log_bem_imobdo = no)
                then do:
                    next block.
                end /* if */.
            end /* if */.

            find first reg_calc_bem_pat of bem_pat 
                where reg_calc_bem_pat.num_seq_incorp_bem_pat = 0 
                and reg_calc_bem_pat.ind_trans = "Implantaá∆o" /*l_implantacao*/  no-lock no-error.

            if  avail reg_calc_bem_pat and reg_calc_bem_pat.dat_calc_pat > v_dat_fim_period
            then do: 
               next block.
            end /* if */.

            if  v_ind_dwb_run_mode = "Batch" /*l_batch*/ 
            then do:
                assign v_cod_ult_obj_procesdo = bem_pat.cod_cta_pat + "," +
                                                string(bem_pat.num_bem_pat) + "," +
                                                string(bem_pat.num_seq_bem_pat).
                run prgtec/btb/btb908ze.py (Input 1,
                                            Input v_cod_ult_obj_procesdo) /*prg_api_atualizar_ult_obj*/.
            end /* if */.
            run pi_tratar_tt_rpt_bem_pat_sit_geral_pat /*pi_tratar_tt_rpt_bem_pat_sit_geral_pat*/.
        end /* for block */.
    end /* if */.

    if  dwb_rpt_select.cod_dwb_field = "Plano Centros Custo"
    then do:
        block:
        for each bem_pat no-lock
         where bem_pat.cod_empresa = v_cod_empres_usuar
           and bem_pat.cod_plano_ccusto >= dwb_rpt_select.cod_dwb_initial
           and bem_pat.cod_plano_ccusto <= dwb_rpt_select.cod_dwb_final /*cl_rpt_bem_pat_plano_ccusto of bem_pat*/:
            if  v_log_consid_bem_bxado = no and bem_pat.val_perc_bxa >= 100
            then do:
                blk_movto_bem_pat:
                for each movto_bem_pat no-lock  where 
                     movto_bem_pat.num_id_bem_pat = bem_pat.num_id_bem_pat and
                     movto_bem_pat.num_seq_incorp_bem_pat = 0 and
                     movto_bem_pat.ind_trans_calc_bem_pat = "Baixa" /*l_baixa*/  and
                     movto_bem_pat.ind_orig_calc_bem_pat <> "Transferància" /*l_transferencia*/  and
                     movto_bem_pat.dat_movto_bem_pat <= v_dat_fim_period:
                     assign v_val_perc_aux = v_val_perc_aux +  movto_bem_pat.val_perc_movto_bem_pat.
                end /* for blk_movto_bem_pat */.
                if v_val_perc_aux >= 100 then do:
                    assign v_val_percentual = 0.
                    next block.
                end.
                assign v_val_percentual = 0.
            end /* if */.
            find first tt_cta_pat_sit_geral 
                 where tt_cta_pat_sit_geral.tta_cod_empresa  = bem_pat.cod_empresa
                 and   tt_cta_pat_sit_geral.tta_cod_cta_pat  = bem_pat.cod_cta_pat
                 and   tt_cta_pat_sit_geral.tta_cod_grp_calc = bem_pat.cod_grp_calc
                 no-lock no-error.
            if  not avail tt_cta_pat_sit_geral
            then do:
               find first cta_pat 
                    where cta_pat.cod_empresa  = bem_pat.cod_empresa
                    and   cta_pat.cod_cta_pat  = bem_pat.cod_cta_pat
                    and   cta_pat.cod_grp_calc = bem_pat.cod_grp_calc
                    no-lock no-error.
               if  avail cta_pat
               then do:
                  create tt_cta_pat_sit_geral.
                  update tt_cta_pat_sit_geral.tta_cod_empresa      = cta_pat.cod_empresa
                         tt_cta_pat_sit_geral.tta_cod_cta_pat      = cta_pat.cod_cta_pat
                         tt_cta_pat_sit_geral.tta_cod_grp_calc     = cta_pat.cod_grp_calc
                         tt_cta_pat_sit_geral.tta_log_control_ctbl = cta_pat.log_control_ctbl.
               end /* if */.
            end /* if */.
            if  avail tt_cta_pat_sit_geral
            then do:
                if  (tt_cta_pat_sit_geral.tta_log_control_ctbl = no) and (v_log_bem_imobdo = no)
                then do:
                    next block.
                end /* if */.
            end /* if */.

            find first reg_calc_bem_pat of bem_pat 
                where reg_calc_bem_pat.num_seq_incorp_bem_pat = 0 
                and reg_calc_bem_pat.ind_trans = "Implantaá∆o" /*l_implantacao*/  no-lock no-error.

            if  avail reg_calc_bem_pat and reg_calc_bem_pat.dat_calc_pat > v_dat_fim_period
            then do: 
               next block.
            end /* if */.

            if  v_ind_dwb_run_mode = "Batch" /*l_batch*/ 
            then do:
                assign v_cod_ult_obj_procesdo = bem_pat.cod_cta_pat + "," +
                                                string(bem_pat.num_bem_pat) + "," +
                                                string(bem_pat.num_seq_bem_pat).
                run prgtec/btb/btb908ze.py (Input 1,
                                            Input v_cod_ult_obj_procesdo) /*prg_api_atualizar_ult_obj*/.
            end /* if */.
            run pi_tratar_tt_rpt_bem_pat_sit_geral_pat /*pi_tratar_tt_rpt_bem_pat_sit_geral_pat*/.
        end /* for block */.
    end /* if */.
    if  dwb_rpt_select.cod_dwb_field = "CCusto Responsab"
    then do:
        block:
        for each bem_pat no-lock
         where bem_pat.cod_empresa = v_cod_empres_usuar
           and bem_pat.cod_ccusto_respons >= dwb_rpt_select.cod_dwb_initial
           and bem_pat.cod_ccusto_respons <= dwb_rpt_select.cod_dwb_final /*cl_rpt_bem_pat_ccusto of bem_pat*/:
            if  v_log_consid_bem_bxado = no and bem_pat.val_perc_bxa >= 100
            then do:
                blk_movto_bem_pat:
                for each movto_bem_pat no-lock  where 
                     movto_bem_pat.num_id_bem_pat = bem_pat.num_id_bem_pat and
                     movto_bem_pat.num_seq_incorp_bem_pat = 0 and
                     movto_bem_pat.ind_trans_calc_bem_pat = "Baixa" /*l_baixa*/  and
                     movto_bem_pat.ind_orig_calc_bem_pat  <> "Transferància" /*l_transferencia*/  and
                     movto_bem_pat.dat_movto_bem_pat <= v_dat_fim_period:
                     assign v_val_perc_aux = v_val_perc_aux +  movto_bem_pat.val_perc_movto_bem_pat.
                end /* for blk_movto_bem_pat */.
                if v_val_perc_aux >= 100 then do:
                    assign v_val_percentual = 0.
                    next block.
                end.
                assign v_val_percentual = 0.
            end /* if */.
            find first tt_cta_pat_sit_geral 
                 where tt_cta_pat_sit_geral.tta_cod_empresa  = bem_pat.cod_empresa
                 and   tt_cta_pat_sit_geral.tta_cod_cta_pat  = bem_pat.cod_cta_pat
                 and   tt_cta_pat_sit_geral.tta_cod_grp_calc = bem_pat.cod_grp_calc
                 no-lock no-error.
            if  not avail tt_cta_pat_sit_geral
            then do:
               find first cta_pat 
                    where cta_pat.cod_empresa  = bem_pat.cod_empresa
                    and   cta_pat.cod_cta_pat  = bem_pat.cod_cta_pat
                    and   cta_pat.cod_grp_calc = bem_pat.cod_grp_calc
                    no-lock no-error.
               if  avail cta_pat
               then do:
                  create tt_cta_pat_sit_geral.
                  update tt_cta_pat_sit_geral.tta_cod_empresa      = cta_pat.cod_empresa
                         tt_cta_pat_sit_geral.tta_cod_cta_pat      = cta_pat.cod_cta_pat
                         tt_cta_pat_sit_geral.tta_cod_grp_calc     = cta_pat.cod_grp_calc
                         tt_cta_pat_sit_geral.tta_log_control_ctbl = cta_pat.log_control_ctbl.
               end /* if */.
            end /* if */.
            if  avail tt_cta_pat_sit_geral
            then do:
                if  (tt_cta_pat_sit_geral.tta_log_control_ctbl = no) and (v_log_bem_imobdo = no)
                then do:
                    next block.
                end /* if */.
            end /* if */.

            find first reg_calc_bem_pat of bem_pat 
                where reg_calc_bem_pat.num_seq_incorp_bem_pat = 0 
                and reg_calc_bem_pat.ind_trans = "Implantaá∆o" /*l_implantacao*/  no-lock no-error.

            if  avail reg_calc_bem_pat and reg_calc_bem_pat.dat_calc_pat > v_dat_fim_period
            then do: 
               next block.
            end /* if */.

            if  v_ind_dwb_run_mode = "Batch" /*l_batch*/ 
            then do:
                assign v_cod_ult_obj_procesdo = bem_pat.cod_cta_pat + "," +
                                                string(bem_pat.num_bem_pat) + "," +
                                                string(bem_pat.num_seq_bem_pat).
                run prgtec/btb/btb908ze.py (Input 1,
                                            Input v_cod_ult_obj_procesdo) /*prg_api_atualizar_ult_obj*/.
            end /* if */.
            run pi_tratar_tt_rpt_bem_pat_sit_geral_pat /*pi_tratar_tt_rpt_bem_pat_sit_geral_pat*/.
        end /* for block */.
    end /* if */.

END PROCEDURE. /* pi_ler_tt_rpt_bem_pat_sit_geral_pat_regra2 */


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
/*********************  End of esfas004 ********************/

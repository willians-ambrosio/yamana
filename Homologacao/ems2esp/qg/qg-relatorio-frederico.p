def temp-table tt_log_erros_apl_emp no-undo
    field ttv_num_cod_erro                 as integer format ">>>>,>>9" label "N£mero" column-label "N£mero"
    field ttv_des_msg_ajuda                as character format "x(40)" label "Mensagem Ajuda" column-label "Mensagem Ajuda"
    field ttv_des_msg_erro                 as character format "x(60)" label "Mensagem Erro" column-label "Inconsistˆncia"
    field tta_cod_banco                    as character format "x(8)" label "Banco" column-label "Banco"
    field tta_cod_produt_financ            as character format "x(8)" label "Produto Financeiro" column-label "Produto Financeiro"
    field tta_cod_operac_financ            as character format "x(10)" label "Opera‡Æo Financeira" column-label "Opera‡Æo Financeira"
    field tta_dat_transacao                as date format "99/99/9999" initial today label "Data Transa‡Æo" column-label "Dat Transac"
    field tta_ind_tip_trans_apl            as character format "X(20)" initial "Aplica‡Æo" label "Tipo Transa‡Æo" column-label "Tipo Transa‡Æo"
    .

def temp-table tt_resumo_apl no-undo
    field tta_cod_banco                    as character format "x(8)" label "Banco" column-label "Banco"
    field tta_cod_produt_financ            as character format "x(8)" label "Produto Financeiro" column-label "Produto Financeiro"
    field tta_cod_tip_produt_financ        as character format "x(8)" label "Tipo Prod Financeiro" column-label "Tipo Prod Fin"
    field tta_cod_operac_financ            as character format "x(10)" label "Opera‡Æo Financeira" column-label "Opera‡Æo Financeira"
    field tta_dat_operac_financ            as date format "99/99/9999" initial ? label "Data Opera‡Æo" column-label "Data Opera‡Æo"
    field tta_dat_vencto_operac_financ     as date format "99/99/9999" initial ? label "Data Vencto" column-label "Data Vencto"
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
    field ttv_val_operac_financ            as decimal format ">>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Opera‡Æo" column-label "Saldo Opera‡Æo"
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
    field ttv_val_operac_financ            as decimal format ">>>,>>>,>>9.9999999999" decimals 10 initial 0 label "Valor Opera‡Æo" column-label "Saldo Opera‡Æo"
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
    field ttv_val_tot_aplic_dispon         as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Dispon¡vel" column-label "Dispon¡vel"
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
    field ttv_val_tot_aplic_dispon         as decimal format "->>,>>>,>>>,>>9.9999999999" decimals 10 initial 0 label "Saldo Dispon¡vel" column-label "Dispon¡vel"
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
    field ttv_val_tot_aplic_carenc         as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo em Carˆncia" column-label "Carˆncia"
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
    field ttv_val_tot_aplic_carenc         as decimal format "->>,>>>,>>>,>>9.9999999999" decimals 10 initial 0 label "Saldo em Carˆncia" column-label "Carˆncia"
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
    field ttv_val_emprest_curto_praz       as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Curto Prazo" column-label "Curto Prazo"
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
    field ttv_val_emprest_curto_praz       as decimal format "->>,>>>,>>>,>>9.9999999999" decimals 10 initial 0 label "Saldo Curto Prazo" column-label "Curto Prazo"
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
    field ttv_val_sdo_longo_praz           as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Longo Prazo" column-label "Longo Prazo"
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
    field ttv_val_sdo_longo_praz           as decimal format "->>,>>>,>>>,>>9.9999999999" decimals 10 initial 0 label "Saldo Longo Prazo" column-label "Longo Prazo"
&ENDIF
    field tta_ind_operac_financ            as character format "X(12)" label "Opera‡Æo" column-label "Opera‡Æo"
    field tta_log_emprest_concedid         as logical format "Sim/NÆo" initial no label "Empr‚stimo Concedido" column-label "Empr‚stimo Concedido"
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
    field ttv_val_sdo_princ                as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Principal" column-label "Saldo Principal"
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
    field ttv_val_sdo_princ                as decimal format "->>,>>>,>>>,>>9.9999999999" decimals 10 initial 0 label "Saldo Principal" column-label "Saldo Principal"
&ENDIF
    field ttv_log_swap_emprest             as logical format "Sim/NÆo" initial no label "Classif Prod Financ" column-label "Classif Prod Financ"
    .
/* --------------------------------------------------------------------------------------------- */
def new global shared var v_cod_aplicat_dtsul_corren
    as character
    format "x(3)":U
    no-undo.
def var v_cod_banco_fim
    as character
    format "x(8)":U
    initial "ZZZZZZZZ" /*l_zzzzzzzz*/
    label "at‚"
    column-label "at‚"
    no-undo.
def var v_cod_banco_ini
    as character
    format "x(8)":U
    label "Banco"
    column-label "Banco"
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
    label "Cen rio Cont bil"
    column-label "Cen rio Cont bil"
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
    label "Usu rio"
    column-label "Usu rio"
    no-undo.
def var v_cod_empresa
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
def var v_cod_erro_resum
    as character
    format "x(120)":U
    no-undo.
def new global shared var v_cod_estab_usuar
    as character
    format "x(3)":U
    label "Estabelecimento"
    column-label "Estab"
    no-undo.
def new global shared var v_cod_funcao_negoc_empres
    as character
    format "x(50)":U
    no-undo.
def new global shared var v_cod_grp_usuar_lst
    as character
    format "x(3)":U
    label "Grupo Usu rios"
    column-label "Grupo"
    no-undo.
def new global shared var v_cod_idiom_usuar
    as character
    format "x(8)":U
    label "Idioma"
    column-label "Idioma"
    no-undo.
def var v_cod_indic_econ_apres
    as character
    format "x(8)":U
    label "Moeda Apresenta‡Æo"
    column-label "Moeda Apresenta‡Æo"
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
    label "Pa¡s Empresa Usu rio"
    column-label "Pa¡s"
    no-undo.
def new global shared var v_cod_plano_ccusto_corren
    as character
    format "x(8)":U
    label "Plano CCusto"
    column-label "Plano CCusto"
    no-undo.
def var v_cod_produt_financ_fim
    as character
    format "x(8)":U
    initial "ZZZZZZZZ" /*l_zzzzzzzz*/
    label "at‚"
    column-label "at‚"
    no-undo.
def var v_cod_produt_financ_ini
    as character
    format "x(8)":U
    label "Produto Financeiro"
    column-label "Produto Financeiro"
    no-undo.
def new shared var v_cod_release
    as character
    format "x(12)":U
    no-undo.
def var v_cod_tip_produt_financ_fim
    as character
    format "x(8)":U
    initial "ZZZZZZZZ"
    label "Final"
    column-label "Final"
    no-undo.
def var v_cod_tip_produt_financ_ini
    as character
    format "x(8)":U
    label "Tipo Produto"
    column-label "Tipo Prod Fin"
    no-undo.
def new global shared var v_cod_unid_negoc_usuar
    as character
    format "x(3)":U
    view-as combo-box
    &if "{&FNC_MULTI_IDIOMA}" = "YES" &then
    list-item-pairs "",""
    &else
    list-items ""
    &endif
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
    label "Fim Per¡odo"
    no-undo.
def new shared var v_dat_inic_period
    as date
    format "99/99/9999":U
    label "In¡cio Per¡odo"
    column-label "Per¡odo"
    no-undo.
def var v_dat_refer
    as date
    format "99/99/9999":U
    initial today
    label "Data Referˆncia"
    column-label "Data Refer"
    no-undo.
def var v_des_tip_det
    as character
    format "x(40)":U
    label "Descri‡Æo"
    column-label "Descri‡Æo"
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
def new global shared var v_log_execution
    as logical
    format "Sim/NÆo"
    initial yes
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
def var v_log_print_par
    as logical
    format "Sim/NÆo"
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
    label "Arquivo ImpressÆo"
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
def var v_num_casas_dec
    as integer
    format "99":U
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
    label "P gina"
    column-label "P gina"
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
def var v_qtd_dias_carenc_1
    as decimal
    format ">>9":U
    decimals 0
    label "Qtd Dias 1"
    no-undo.
def var v_qtd_dias_carenc_2
    as decimal
    format ">>9":U
    decimals 0
    label "Qtd Dias 2"
    no-undo.
def var v_qtd_dias_carenc_3
    as decimal
    format ">>9":U
    decimals 0
    label "Qtd Dias 3"
    no-undo.
def var v_qtd_dias_curto_praz
    as decimal
    format ">,>>9":U
    decimals 0
    label "Qtd Dias Curto Prazo"
    column-label "Qtd Dias Curto Prazo"
    no-undo.
def var v_qtd_dias_vencto_1
    as decimal
    format ">>9":U
    decimals 0
    label "Qtd Dias 1"
    no-undo.
def var v_qtd_dias_vencto_2
    as decimal
    format ">>9":U
    decimals 0
    label "Qtd Dias 2"
    no-undo.
def var v_qtd_dias_vencto_3
    as decimal
    format ">>9":U
    decimals 0
    label "Qtd Dias 3"
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
def new global shared var v_rec_empresa
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_indic_econ
    as recid
    format ">>>>>>9":U
    no-undo.
def var v_rec_log
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_operac_financ
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.
def var v_rec_table_epc
    as recid
    format ">>>>>>9":U
    no-undo.
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_emprest_curto_praz
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Saldo Curto Prazo"
    column-label "Curto Prazo"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_emprest_curto_praz
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Saldo Curto Prazo"
    column-label "Curto Prazo"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_operac_financ_aux
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Valor Principal"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_operac_financ_aux
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Valor Principal"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_saida_carenc_1
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Em ?1 dias"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_saida_carenc_1
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Em ?1 dias"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_saida_carenc_2
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Em ?2 Dias"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_saida_carenc_2
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Em ?2 Dias"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_saida_carenc_3
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Em ?3 Dias"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_saida_carenc_3
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Em ?3 Dias"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_saida_carenc_acima_3
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Acima de ?3 Dias"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_saida_carenc_acima_3
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Acima de ?3 Dias"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_sdo_juros
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Saldo Juros"
    column-label "Saldo Juros"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_sdo_juros
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Saldo Juros"
    column-label "Saldo Juros"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_sdo_juros_aplic
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Sdo Juros"
    column-label "Sdo Juros"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_sdo_juros_aplic
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Sdo Juros"
    column-label "Sdo Juros"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_sdo_longo_praz
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Saldo Longo Prazo"
    column-label "Longo Prazo"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_sdo_longo_praz
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Saldo Longo Prazo"
    column-label "Longo Prazo"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_sdo_princ
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Saldo Principal"
    column-label "Saldo Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_sdo_princ
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Saldo Principal"
    column-label "Saldo Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_sdo_princ_aplic
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Saldo Principal"
    column-label "Saldo Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_sdo_princ_aplic
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Saldo Principal"
    column-label "Saldo Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_tot_aplic
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Saldo Aplica‡äes"
    column-label "Saldo Total"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_tot_aplic
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Saldo Aplica‡äes"
    column-label "Saldo Total"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_tot_aplic_carenc
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Saldo em Carˆncia"
    column-label "Carˆncia"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_tot_aplic_carenc
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Saldo em Carˆncia"
    column-label "Carˆncia"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_tot_aplic_dispon
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Saldo Dispon¡vel"
    column-label "Dispon¡vel"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_tot_aplic_dispon
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Saldo Dispon¡vel"
    column-label "Dispon¡vel"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_tot_emprest
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Saldo Empr‚stimos"
    column-label "Saldo Total"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_tot_emprest
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Saldo Empr‚stimos"
    column-label "Saldo Total"
    no-undo.
&ENDIF
def var v_val_tot_operac_swap
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Opera‡äes Swap"
    column-label "Opera‡äes Swap"
    no-undo.
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_tot_vencto_2_mes
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Vencimento Bimestre"
    column-label "Vencimento Bimestre"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_tot_vencto_2_mes
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Vencimento Bimestre"
    column-label "Vencimento Bimestre"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_tot_vencto_3_mes
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Vencimento Trimestre"
    column-label "Vencimento Trimestre"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_tot_vencto_3_mes
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Vencimento Trimestre"
    column-label "Vencimento Trimestre"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_tot_vencto_dia
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Vencimento Dia"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_tot_vencto_dia
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Vencimento Dia"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_tot_vencto_quinz
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Vencimento Quinzena"
    column-label "Vencimento Quinzena"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_tot_vencto_quinz
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Vencimento Quinzena"
    column-label "Vencimento Quinzena"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_tot_vencto_sema
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Vencimento Semana"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_tot_vencto_sema
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Vencimento Semana"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_tot_vencto_semt
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Vencimento Semestre"
    column-label "Vencimento Semestre"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_tot_vencto_semt
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Vencimento Semestre"
    column-label "Vencimento Semestre"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_vencto_1
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Em ?1 Dias"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_vencto_1
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Em ?1 Dias"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_vencto_2
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Em ?2 Dias"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_vencto_2
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Em ?2 Dias"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_vencto_3
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Em 3? Dias"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_vencto_3
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Em 3? Dias"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_vencto_acima_3
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Acima de ?3 Dias"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_vencto_acima_3
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Acima de ?3 Dias"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_vencto_mes
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Vencimento Mˆs"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
def var v_val_vencto_mes
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Vencimento Mˆs"
    column-label "Valor Principal"
    no-undo.
&ENDIF
/* --------------------------------------------------------------------------------------------- */
run prgfin/apl/apl318za.py (Input 1,
                            Input "MMR",
                            Input "REAL",
                            Input 04/30/2019,
                            Input 0,
                            Input "",
                            Input "ZZZZZZZZ",
                            Input "",
                            Input "ZZZZZZZZ",
                            Input "",
                            Input "ZZZZZZZZ",
                            Input 0,
                            Input 0,
                            Input 0,
                            Input 0,
                            Input 0,
                            Input 0,
                            output v_val_tot_aplic,
                            output v_val_tot_aplic_dispon,
                            output v_val_tot_aplic_carenc,
                            output v_val_saida_carenc_1,
                            output v_val_saida_carenc_2,
                            output v_val_saida_carenc_3,
                            output v_val_saida_carenc_acima_3,
                            output v_val_vencto_1,
                            output v_val_vencto_2,
                            output v_val_vencto_3,
                            output v_val_vencto_acima_3,
                            output v_val_tot_emprest,
                            output v_val_emprest_curto_praz,
                            output v_val_sdo_longo_praz,
                            output v_val_tot_vencto_dia,
                            output v_val_tot_vencto_sema,
                            output v_val_tot_vencto_quinz,
                            output v_val_vencto_mes,
                            output v_val_tot_vencto_2_mes,
                            output v_val_tot_vencto_3_mes,
                            output v_val_tot_vencto_semt,
                            output table tt_resumo_apl,
                            Input no,
                            output table tt_log_erros_apl_emp,
                            output v_val_sdo_princ,
                            Input "",
                            output v_val_tot_operac_swap) /*prg_api_rpt_resumo_apl*/.                           

FOR EACH tt_resumo_apl WHERE
         tt_resumo_apl.tta_cod_operac_financ = "MMR-80151":

    assign v_val_tot_emprest  = tt_resumo_apl.ttv_val_operac_financ
           v_val_sdo_princ    = tt_resumo_apl.ttv_val_sdo_princ
           v_val_sdo_juros    = v_val_tot_emprest - v_val_sdo_princ.
         
    DISP v_val_sdo_juros WITH 1 COL SCROLLABLE.
END.


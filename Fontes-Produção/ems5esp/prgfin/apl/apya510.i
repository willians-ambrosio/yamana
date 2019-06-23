/****************************************************************************************** 
** 	   Programa: apya510.i
**   	  Autor: Sergio 
** 	 Fornecedor: DKP
**         Data: 12/2017
** Change/Chamado: 
**      Objetivo: Defini‡oes para 
**
******************************** CONTROLE DE ALTERA€åES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: N/A
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

def temp-table tt_correcoes no-undo
    field tta_cod_indic_econ_juros         as character format "x(8)" label "Indic Econ Juros" column-label "Indic Econ Juros"
    field ttv_dat_correc                   as date format "99/99/9999" initial today label "Data Corre»’o" column-label "Data Corre»’o"
    field tta_num_id_movto_operac_financ   as integer format ">>>>,>>9" initial 0 label "Id Movto Oper Financ" column-label "Id Movto Oper Financ"
    field tta_num_seq                      as integer format ">>>,>>9" initial 0 label "Sequ¼ncia" column-label "NumSeq"
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
    field ttv_val_correc_apl               as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Corre»’o" column-label "Valor Corre»’o"
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
    field ttv_val_correc_apl               as decimal format "->>,>>>,>>>,>>9.9999999999" decimals 10 initial 0 label "Valor Corre»’o" column-label "Valor Corre»’o"
&ENDIF
    field ttv_val_tax_pre_novo             as decimal format "->>,>>>,>>>,>>9.9999999999" decimals 10 label "Val.Tax.Pre" column-label "Val.Tax.Pre"
    field ttv_val_tax_pos_novo             as decimal format "->>>,>>9.9999999999" decimals 10 label "Val.Tax.Pos" column-label "Val.Tax.Pos"
    field ttv_val_cota_inic_novo           as decimal format "->>,>>>,>>>,>>9.9999999999" decimals 10 label "Cota»’o Inic" column-label "Cota»’o Inic"
    field ttv_val_cota_fim_novo            as decimal format "->>,>>>,>>>,>>9.9999999999" decimals 10 label "Cota»’o Fim" column-label "Cota»’o Fim"
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
    field ttv_val_juros_apl_1              as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Juros" column-label "Valor Juros"
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
    field ttv_val_juros_apl_1              as decimal format "->>>,>>>,>>9.9999999999" decimals 10 initial 0 label "Valor Juros" column-label "Valor Juros"
&ENDIF
    field ttv_val_comis_delcred            as decimal format "->>,>>>,>>>,>>9.99" decimals 2 label "Val Del Credere" column-label "Val Del Credere"
    field ttv_val_tax_delcred              as decimal format "->>>,>>9.999999999" decimals 9
    .

def temp-table tt_despes_operac_financ no-undo like despes_operac_financ
    field tta_ind_forma_aprop_despes       as character format "X(15)" initial "Abate Sdo Princ" label "Apropria»’o Despesa" column-label "Apropria»’o Despesa"
    field ttv_val_despes                   as decimal format "->>,>>>,>>>,>>9.99" decimals 2 label "Val Despesa" column-label "Val Despesa"
    field tta_val_cotac_contrat            as decimal format ">>>>>,>>9.9999999999" decimals 10 initial 0 label "Cota»’o Contratada" column-label "Cota»’o Contratada"
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
    field tta_val_movto_indic_econ_movto   as decimal format ">>>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Vl Movto na Moeda" column-label "Vl Movto na Moeda"
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
    field tta_val_movto_indic_econ_movto   as decimal format ">>>,>>>,>>>,>>9.9999999999" decimals 10 initial 0 label "Vl Movto na Moeda" column-label "Vl Movto na Moeda"
&ENDIF
    .

def temp-table tt_documen_operac_financ no-undo like documen_operac_financ
    .

def temp-table tt_erros_correc no-undo
    field ttv_num_cod_erro                 as integer format ">>>>,>>9" label "Nœmero" column-label "Nœmero"
    field ttv_des_msg_ajuda                as character format "x(40)" label "Mensagem Ajuda" column-label "Mensagem Ajuda"
    field ttv_des_msg_erro                 as character format "x(60)" label "Mensagem Erro" column-label "Inconsist¼ncia"
    field tta_num_id_movto_prev_operac     as integer format ">>>>,>>9" initial 0 label "Id Movto Previsto" column-label "Id Movto Previsto"
    field ttv_ind_tab_nom                  as character format "X(25)"
    .

def temp-table tt_fiador_operac_financ no-undo like fiador_operac_financ
    .

def temp-table tt_gartia_operac_financ no-undo like gartia_operac_financ
    .

def temp-table tt_imposto_apl no-undo
    field ttv_dat_imposto                  as date format "99/99/9999" label "Dat.Imposto"
    field ttv_val_imposto                  as decimal format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Imposto" column-label "Valor Imposto"
    field tta_cod_pais                     as character format "x(3)" label "Pa­s" column-label "Pa­s"
    field tta_cod_unid_federac             as character format "x(3)" label "Unidade Federa»’o" column-label "UF"
    field tta_cod_imposto                  as character format "x(5)" label "Imposto" column-label "Imposto"
    field tta_cod_classif_impto            as character format "x(05)" initial "00000" label "Class Imposto" column-label "Class Imposto"
    field ttv_num_seq_correc_pai           as integer format ">>>>,>>9"
    field ttv_ind_forma_aprop_impto        as character format "X(08)"
    .

def temp-table tt_indic_econ_operac_financ no-undo like indic_econ_operac_financ
    .

def temp-table tt_log_erro no-undo
    field ttv_num_cod_erro                 as integer format ">>>>,>>9" label "Nœmero" column-label "Nœmero"
    field ttv_des_msg_ajuda                as character format "x(40)" label "Mensagem Ajuda" column-label "Mensagem Ajuda"
    field ttv_des_msg_erro                 as character format "x(60)" label "Mensagem Erro" column-label "Inconsist¼ncia"
    .

def temp-table tt_movto_operac_financ no-undo like movto_operac_financ
    .

def temp-table tt_operac_financ no-undo like operac_financ
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
    field ttv_val_sdo_curto_praz           as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Curto Prazo" column-label "Saldo Curto Prazo"
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
    field ttv_val_sdo_curto_praz           as decimal format "->>,>>>,>>>,>>9.9999999999" decimals 10 initial 0 label "Saldo Curto Prazo" column-label "Saldo Curto Prazo"
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
    field ttv_val_sdo_longo_praz           as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Longo Prazo" column-label "Longo Prazo"
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
    field ttv_val_sdo_longo_praz           as decimal format "->>,>>>,>>>,>>9.9999999999" decimals 10 initial 0 label "Saldo Longo Prazo" column-label "Longo Prazo"
&ENDIF
    .

def temp-table tt_parc_operac_financ no-undo like parc_operac_financ
    .

def temp-table tt_totaliz_razao_apl no-undo
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
    field tta_val_operac_financ            as decimal format ">>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Opera»’o" column-label "Valor Opera»’o"
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
    field tta_val_operac_financ            as decimal format ">>>,>>>,>>9.9999999999" decimals 10 initial 0 label "Valor Opera»’o" column-label "Valor Opera»’o"
&ENDIF
    field tta_val_despes_bcia              as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Vl Desp Banc" column-label "Vl Desp Banc"
    field tta_val_impto_operac_financ      as decimal format ">>>>,>>>,>>9.99" decimals 2 initial 0 label "Imposto Opera»’o" column-label "Imposto Opera»’o"
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
    field tta_val_juros_operac_financ      as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Juros" column-label "Juros"
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
    field tta_val_juros_operac_financ      as decimal format "->>>,>>>,>>9.9999999999" decimals 10 initial 0 label "Juros" column-label "Juros"
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
    field tta_val_sdo_operac_financ        as decimal format ">>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Opera»’o" column-label "Saldo Opera»’o"
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
    field tta_val_sdo_operac_financ        as decimal format ">>>,>>>,>>9.9999999999" decimals 10 initial 0 label "Saldo Opera»’o" column-label "Saldo Opera»’o"
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
    field ttv_val_sdo_curto_praz           as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Curto Prazo" column-label "Saldo Curto Prazo"
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
    field ttv_val_sdo_curto_praz           as decimal format "->>,>>>,>>>,>>9.9999999999" decimals 10 initial 0 label "Saldo Curto Prazo" column-label "Saldo Curto Prazo"
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
    field ttv_val_sdo_longo_praz           as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Longo Prazo" column-label "Longo Prazo"
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
    field ttv_val_sdo_longo_praz           as decimal format "->>,>>>,>>>,>>9.9999999999" decimals 10 initial 0 label "Saldo Longo Prazo" column-label "Longo Prazo"
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
    field ttv_val_sdo_princ_operac_financ  as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Principal" column-label "Saldo Principal"
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
    field ttv_val_sdo_princ_operac_financ  as decimal format "->>>,>>>,>>9.9999999999" decimals 10 initial 0 label "Saldo Principal" column-label "Saldo Principal"
&ENDIF
    field tta_cod_banco                    as character format "x(8)" label "Banco" column-label "Banco"
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
    field ttv_val_resg_aplic               as decimal format ">>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Resgate" column-label "Valor Resgate"
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
    field ttv_val_resg_aplic               as decimal format ">>>,>>>,>>9.9999999999" decimals 10 initial 0 label "Valor Resgate" column-label "Valor Resgate"
&ENDIF
    field ttv_val_tot_pagto_aplic          as decimal format "->>,>>>,>>>,>>9.99" decimals 10
    field tta_cod_indic_econ               as character format "x(8)" label "Moeda" column-label "Moeda"
    index tt_banco                         is primary
          tta_cod_banco                    ascending
    .


def new global shared var v_cod_aplicat_dtsul_corren
    as character
    format "x(3)":U
    no-undo.
def var v_cod_banco_fim
    as character
    format "x(8)":U
    initial "ZZZZZZZZ" /*l_zzzzzzzz*/
    label "at²"
    column-label "at²"
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
def var v_cod_cta_corren_apl
    as character
    format "x(10)":U
    label "Conta Corrente"
    column-label "Conta Corrente"
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
    label "Usuÿrio"
    column-label "Usuÿrio"
    no-undo.
def var v_cod_empresa
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.
def var v_cod_empres_fim
    as character
    format "x(3)":U
    initial "ZZZ"
    label "at²"
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
def var v_cod_indic_econ_apres
    as character
    format "x(8)":U
    label "Moeda Apresenta»’o"
    column-label "Moeda Apresenta»’o"
    no-undo.
def var v_cod_indic_econ_tax_pos
    as character
    format "x(8)":U
    label "Indicador EconËmico"
    column-label "Indicador EconËmico"
    no-undo.
def new global shared var v_cod_modul_dtsul_corren
    as character
    format "x(3)":U
    label "M½dulo Corrente"
    column-label "M½dulo Corrente"
    no-undo.
def new global shared var v_cod_modul_dtsul_empres
    as character
    format "x(100)":U
    no-undo.
def var v_cod_operac_financ_fim
    as character
    format "x(10)":U
    initial "ZZZZZZZZZZ" /*l_zzzzzzzzzz*/
    label "at²"
    column-label "at²"
    no-undo.
def var v_cod_operac_financ_ini
    as character
    format "x(10)":U
    label "Opera»’o Financeira"
    column-label "Opera»’o Financeira"
    no-undo.
def new global shared var v_cod_pais_empres_usuar
    as character
    format "x(3)":U
    label "Pa­s Empresa Usuÿrio"
    column-label "Pa­s"
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
    label "at²"
    column-label "at²"
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
    list-items ""
    inner-lines 5
    bgcolor 15 font 2
    label "Unidade Neg½cio"
    column-label "Unid Neg½cio"
    no-undo.
def new global shared var v_cod_usuar_corren
    as character
    format "x(12)":U
    label "Usuÿrio Corrente"
    column-label "Usuÿrio Corrente"
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
    label "Fim Per­odo"
    no-undo.
def var v_dat_final_movimen
    as date
    format "99/99/9999":U
    label "Data Final Movto"
    column-label "Data Final Movto"
    no-undo.
def var v_dat_inic_movimen
    as date
    format "99/99/9999":U
    initial today
    label "Data In­cio Movto"
    column-label "Data In­cio Movto"
    no-undo.
def new shared var v_dat_inic_period
    as date
    format "99/99/9999":U
    label "In­cio Per­odo"
    column-label "Per­odo"
    no-undo.
def var v_dat_operac_financ_fim
    as date
    format "99/99/9999":U
    initial 12/31/9999
    label "at²"
    column-label "at²"
    no-undo.
def var v_dat_operac_financ_ini
    as date
    format "99/99/9999":U
    initial &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 01/01/0001 &ENDIF
    label "Data Opera»’o"
    column-label "Data Opera»’o"
    no-undo.
def var v_dat_transacao
    as date
    format "99/99/9999":U
    label "Data Transa»’o"
    column-label "Data Transa»’o"
    no-undo.
def var v_dat_valid
    as date
    format "99/99/9999":U
    no-undo.
def var v_dat_vencto_operac_financ_fim
    as date
    format "99/99/9999":U
    initial 12/31/9999
    label "Final"
    column-label "Final"
    no-undo.
def var v_dat_vencto_operac_financ_ini
    as date
    format "99/99/9999":U
    initial &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 01/01/0001 &ENDIF
    label "Data Vencto"
    column-label "Data Vencto"
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
def var v_ind_capitaliz_tax_pos_fix
    as character
    format "x(07)":U
    view-as radio-set Horizontal
    radio-buttons "Nominal", "Nominal","Efetiva", "Efetiva"
     /*l_nominal*/ /*l_nominal*/ /*l_efetiva*/ /*l_efetiva*/
    bgcolor 8 
    label "Tipo Capitaliza»’o"
    column-label "Capitaliz Taxa P½s"
    no-undo.
def var v_ind_capitaliz_tax_pre_fix
    as character
    format "X(07)":U
    view-as radio-set Horizontal
    radio-buttons "Nominal", "Nominal","Efetiva", "Efetiva"
     /*l_nominal*/ /*l_nominal*/ /*l_efetiva*/ /*l_efetiva*/
    bgcolor 8 
    label "Tipo Capitaliza»’o"
    column-label "Capitaliz Taxa Pr²"
    no-undo.
def var v_ind_dat_correc
    as character
    format "X(15)":U
    initial "Dia Movimento" /*l_dia_movimento*/
    view-as combo-box
    list-items "Dia Movimento","Dia Anterior"
     /*l_dia_movimento*/ /*l_dia_anterior*/
    inner-lines 5
    bgcolor 15 font 2
    label "Data Cota»’o"
    column-label "Data Cota»’o"
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
def var v_ind_tip_tax_pos_fix
    as character
    format "X(30)":U
    no-undo.
def var v_ind_tip_trans_apl
    as character
    format "X(20)":U
    view-as combo-box
    list-items "Aplica»’o","Resgate","Libera»’o","Pagamento","Renova»’o","Corre»’o","Despesa Bancÿria","Imposto","Equaliza»’o","Corre»’o Compet¼ncia"
     /*l_aplicacao*/ /*l_resgate*/ /*l_liberacao*/ /*l_pagamento*/ /*l_renovacao*/ /*l_correcao*/ /*l_despes_bcia*/ /*l_imposto*/ /*l_equalizacao*/ /*l_correcao_competencia*/
    inner-lines 10
    bgcolor 15 font 2
    label "Tipo Transa»’o"
    column-label "Tip Transa»’o"
    no-undo.
def var v_ind_ump_tax_juros_pos_fixad_re
    as character
    format "X(12)":U
    view-as combo-box
    list-items "Dia","Semana","Quinzena","M¼s","Bimestre","Trimestre","Quadrimestre","Semestre","Ano"
     /*l_dia*/ /*l_semana*/ /*l_quinzena*/ /*l_mes*/ /*l_bimestre*/ /*l_trimestre*/ /*l_quadrimestre*/ /*l_semestre*/ /*l_ano*/
    inner-lines 5
    bgcolor 15 font 2
    label "UMP Taxa P½s"
    column-label "UMP Taxa P½s"
    no-undo.
def var v_ind_ump_tax_juros_pre_fixad_re
    as character
    format "X(12)":U
    view-as combo-box
    list-items "Dia","Semana","Quinzena","M¼s","Bimestre","Trimestre","Quadrimestre","Semestre","Ano"
     /*l_dia*/ /*l_semana*/ /*l_quinzena*/ /*l_mes*/ /*l_bimestre*/ /*l_trimestre*/ /*l_quadrimestre*/ /*l_semestre*/ /*l_ano*/
    inner-lines 5
    bgcolor 15 font 2
    label "UMP Taxa Pr²"
    column-label "UMP Taxa Pr²"
    no-undo.
def var v_log_calc_correc_mon
    as logical
    format "Sim/N’o"
    initial no
    view-as toggle-box
    label "Cÿlc. Correc Mon."
    column-label "Cÿlc. Correc Mon."
    no-undo.
def var v_log_correc_parcial
    as logical
    format "Sim/N’o"
    initial no
    view-as toggle-box
    label "Corre»’o Parcial"
    column-label "Corre»’o Parcial"
    no-undo.
def var v_log_empres_concedid
    as logical
    format "Sim/N’o"
    initial no
    no-undo.
def new global shared var v_log_execution
    as logical
    format "Sim/N’o"
    initial yes
    no-undo.
def var v_log_funcao_dat_correc
    as logical
    format "Sim/N’o"
    initial no
    no-undo.
def var v_log_funcao_juros_princ_apl
    as logical
    format "Sim/N’o"
    initial no
    no-undo.
def var v_log_funcao_liber
    as logical
    format "Sim/N’o"
    initial NO
    no-undo.
def var v_log_funcao_tot_razao_aplic
    as logical
    format "Sim/N’o"
    initial no
    no-undo.
def var v_log_integr_mec
    as logical
    format "Sim/N’o"
    initial no
    no-undo.
def var v_log_method
    as logical
    format "Sim/N’o"
    initial yes
    no-undo.
def var v_log_mostra_comptcia
    as logical
    format "Sim/N’o"
    initial yes
    view-as toggle-box
    label "Compet¼ncia"
    column-label "Compet¼ncia"
    no-undo.
def var v_log_mostra_despes_operac
    as logical
    format "Sim/N’o"
    initial yes
    view-as toggle-box
    label "Despesas Opera»’o"
    no-undo.
def var v_log_mostra_docto_operac
    as logical
    format "Sim/N’o"
    initial yes
    view-as toggle-box
    label "Documentos Opera»’o"
    column-label "Documentos Opera»’o"
    no-undo.
def var v_log_mostra_extrat
    as logical
    format "Sim/N’o"
    initial no
    view-as toggle-box
    label "Extrato"
    column-label "Extrato"
    no-undo.
def var v_log_mostra_fiador_operac
    as logical
    format "Sim/N’o"
    initial yes
    view-as toggle-box
    label "Fiadores Opera»’o"
    column-label "Fiadores Opera»’o"
    no-undo.
def var v_log_mostra_gartia_operac
    as logical
    format "Sim/N’o"
    initial yes
    view-as toggle-box
    label "Garantias Opera»’o"
    column-label "Garantias Opera»’o"
    no-undo.
def var v_log_mostra_movto_operac
    as logical
    format "Sim/N’o"
    initial yes
    view-as toggle-box
    label "Movimentos Opera»’o"
    no-undo.
def var v_log_mostra_operac_detdo
    as logical
    format "Sim/N’o"
    initial yes
    view-as toggle-box
    label "Detalhado"
    no-undo.
def var v_log_mostra_operac_sem_sdo
    as logical
    format "Sim/N’o"
    initial yes
    view-as toggle-box
    label "Opera»„es sem Saldo"
    no-undo.
def var v_log_mostra_parc_operac
    as logical
    format "Sim/N’o"
    initial yes
    view-as toggle-box
    label "Parcelas Opera»’o"
    column-label "Parcelas Opera»’o"
    no-undo.
def var v_log_mostra_tax_operac
    as logical
    format "Sim/N’o"
    initial yes
    view-as toggle-box
    label "Taxas Opera»’o"
    no-undo.
def var v_log_nao_consid_sdo
    as logical
    format "Sim/N’o"
    initial no
    no-undo.
def var v_log_operac_financ_com_sdo
    as logical
    format "Sim/N’o"
    initial yes
    view-as toggle-box
    label "Opera»„es com Saldo"
    no-undo.
def var v_log_pre_pagto
    as logical
    format "Sim/N’o"
    initial no
    view-as toggle-box
    label "Pr²-Pagamento"
    column-label "Pr²-Pagamento"
    no-undo.
def var v_log_print
    as logical
    format "Sim/N’o"
    initial no
    no-undo.
def var v_log_print_par
    as logical
    format "Sim/N’o"
    initial yes
    view-as toggle-box
    no-undo.
def var v_log_repas_pend
    as logical
    format "Sim/N’o"
    initial no
    view-as toggle-box
    label "Repasses Pendentes"
    column-label "Repasses Pendentes"
    no-undo.
def var v_nom_dwb_printer
    as character
    format "x(30)":U
    no-undo.
def var v_nom_dwb_print_file
    as character
    format "x(100)":U
    label "Arquivo Impress’o"
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
def var v_nom_pessoa
    as character
    format "x(40)":U
    label "Nome"
    column-label "Nome"
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
def var v_num_calc_1
    as integer
    format ">>>>,>>9":U
    no-undo.
def var v_num_calc_2
    as integer
    format ">>>>,>>9":U
    no-undo.
def var v_num_casas_dec
    as integer
    format "99":U
    no-undo.
def var v_qtd_dias_curto_praz
    as decimal
    format ">,>>9":U
    decimals 0
    label "Qtd Dias Curto Prazo"
    column-label "Qtd Dias Curto Prazo"
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
def var v_val_cotac_indic_econ_fim_rel
    as decimal
    format ">>>>,>>9.9999999999":U
    decimals 10
    label "Cota»’o Final"
    column-label "Cota»’o Final"
    no-undo.
def var v_val_cotac_indic_econ_inic_rel
    as decimal
    format ">>>>,>>9.9999999999":U
    decimals 10
    label "Cota»’o Inicial"
    column-label "Cota»’o Inicial"
    no-undo.

def var v_val_despes_acum
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    no-undo.
def var v_val_impto_acum
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    no-undo.
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
def var v_val_juros_apl_1
    as decimal
    format "->>>,>>>,>>9.99":U
    decimals 2
    label "Valor Juros"
    column-label "Valor Juros"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
def var v_val_juros_apl_1
    as decimal
    format "->>>,>>>,>>9.9999999999":U
    decimals 10
    label "Valor Juros"
    column-label "Valor Juros"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
def var v_val_movto_apl_3
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Valor Movimento"
    column-label "Valor Movimento"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
def var v_val_movto_apl_3
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Valor Movimento"
    column-label "Valor Movimento"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
def var v_val_movto_sdo_princ
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Valor Principal"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
def var v_val_movto_sdo_princ
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Valor Principal"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
def var v_val_princ_apl
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Valor Principal"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
def var v_val_princ_apl
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Valor Principal"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
def var v_val_princ_movto
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Valor do Principal"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
def var v_val_princ_movto
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Valor do Principal"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
def var v_val_sdo_curto_praz
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Saldo Curto Prazo"
    column-label "Saldo Curto Prazo"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
def var v_val_sdo_curto_praz
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Saldo Curto Prazo"
    column-label "Saldo Curto Prazo"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
def var v_val_sdo_fim_apl
    as decimal
    format "->>>,>>>,>>9.99":U
    decimals 2
    initial 999999999.99
    label "at²"
    column-label "Valor Opera»’o"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
def var v_val_sdo_fim_apl
    as decimal
    format "->>>,>>>,>>9.9999999999":U
    decimals 10
    initial 999999999.99
    label "at²"
    column-label "Valor Opera»’o"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
def var v_val_sdo_inicial_apl
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Saldo Inicial"
    column-label "Saldo Inicial"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
def var v_val_sdo_inicial_apl
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Saldo Inicial"
    column-label "Saldo Inicial"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
def var v_val_sdo_inic_apl
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Saldo"
    column-label "Saldo"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
def var v_val_sdo_inic_apl
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Saldo"
    column-label "Saldo"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
def var v_val_sdo_longo_praz
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Saldo Longo Prazo"
    column-label "Longo Prazo"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
def var v_val_sdo_longo_praz
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Saldo Longo Prazo"
    column-label "Longo Prazo"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
def var v_val_sdo_princ_operac_financ
    as decimal
    format "->>>,>>>,>>9.99":U
    decimals 2
    label "Saldo Principal"
    column-label "Saldo Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
def var v_val_sdo_princ_operac_financ
    as decimal
    format "->>>,>>>,>>9.9999999999":U
    decimals 10
    label "Saldo Principal"
    column-label "Saldo Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
def var v_val_tax_delcred_apl
    as decimal
    format ">9.99":U
    decimals 2
    label "Taxa Del credere"
    column-label "Taxa Del credere"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
def var v_val_tax_delcred_apl
    as decimal
    format ">9.99999":U
    decimals 5
    label "Taxa Del credere"
    column-label "Taxa Del credere"
    no-undo.
&ENDIF
def var v_val_tax_juros_pos_fixad_rel
    as decimal
    format ">>9.99999999":U
    decimals 8
    label "Taxa P½s-Fixada"
    column-label "Taxa P½s-Fixada"
    no-undo.
def var v_val_tax_juros_pre_fixad_rel
    as decimal
    format ">>9.99999999":U
    decimals 8
    label "Taxa Pr²-Fixada"
    column-label "Taxa Pr²-Fixada"
    no-undo.
def var v_val_tot_despes_bcia
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Despesa Bancÿria"
    column-label "Despesa Bancÿria"
    no-undo.
def var v_val_tot_impto_operac_financ
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    no-undo.
def var v_val_tot_juros_operac_financ
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 10
    no-undo.
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
def var v_val_tot_operac_financ
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    initial 0
    label "Saldo Principal"
    column-label "Saldo Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
def var v_val_tot_operac_financ
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    initial 0
    label "Saldo Principal"
    column-label "Saldo Principal"
    no-undo.
&ENDIF
def var v_val_tot_pagto_aplic
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 10
    no-undo.
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
def var v_val_tot_sdo_operac_financ
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Saldo Principal"
    column-label "Saldo Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
def var v_val_tot_sdo_operac_financ
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Saldo Principal"
    column-label "Saldo Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
def var v_val_tot_sdo_princ_operac
    as decimal
    format "->>>,>>>,>>9.99":U
    decimals 2
    label "Juros"
    column-label "Juros"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
def var v_val_tot_sdo_princ_operac
    as decimal
    format "->>>,>>>,>>9.9999999999":U
    decimals 10
    label "Juros"
    column-label "Juros"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
def var v_val_varcamb_juros_apl
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Valor Principal"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
def var v_val_varcamb_juros_apl
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Valor Principal"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" <= "5.05" &THEN
def var v_val_varcamb_princ_apl
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Valor Principal"
    column-label "Valor Principal"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" <= "9.99" &THEN
def var v_val_varcamb_princ_apl
    as decimal
    format "->>,>>>,>>>,>>9.9999999999":U
    decimals 10
    label "Valor Principal"
    column-label "Valor Principal"
    no-undo.
&ENDIF
def var v_wgh_focus
    as widget-handle
    format ">>>>>>9":U
    no-undo.
def var v_wgh_frame_epc
    as widget-handle
    format ">>>>>>9":U
    no-undo.



/* Tabelas do RP */

DEF VAR p_val_sdo_longo_praz       AS DEC.
DEF VAR p_val_sdo_juros_longo_praz AS DEC.
DEF VAR p_val_sdo_juros_curto_praz AS DEC.
DEF VAR p_val_sdo_operac_financ    AS DEC.
DEF VAR p_val_sdo_princ_operac_financ AS DEC.
DEF VAR p_val_movto_sdo_princ      AS DEC.     
DEF VAR p_val_tot_aplic            AS DEC.
DEF VAR p_val_tot_aplic_dispon     AS DEC.
DEF VAR p_val_tot_aplic_carenc     AS DEC.
DEF VAR p_val_saida_carenc_1       AS DEC.
DEF VAR p_val_saida_carenc_2       AS DEC.
DEF VAR p_val_saida_carenc_3       AS DEC.
DEF VAR p_val_saida_carenc_acima_3 AS DEC.
DEF VAR p_val_vencto_1             AS DEC.
DEF VAR p_val_vencto_2             AS DEC.
DEF VAR p_val_vencto_3             AS DEC.
DEF VAR p_val_vencto_acima_3       AS DEC.
DEF VAR p_val_tot_emprest          AS DEC.
DEF VAR p_val_emprest_curto_praz   AS DEC.
DEF VAR p_val_tot_vencto_dia       AS DEC.
DEF VAR p_val_tot_vencto_sema      AS DEC.
DEF VAR p_val_tot_vencto_quinz     AS DEC.
DEF VAR p_val_vencto_mes           AS DEC.
DEF VAR p_val_tot_vencto_2_mes     AS DEC.
DEF VAR p_val_tot_vencto_3_mes     AS DEC.
DEF VAR p_val_tot_vencto_semt      AS DEC.
DEF VAR p_val_tot_princ            AS DEC.
DEF VAR v_val_tot_operac_swap      AS DEC.
DEFINE VARIABLE v_impto_ir         AS DECIMAL NO-UNDO DECIMALS 6.
DEFINE VARIABLE v_perc_impto       AS DECIMAL NO-UNDO DECIMALS 6.
DEFINE VARIABLE v_val_juros_sdo-ini AS DECIMAL NO-UNDO DECIMALS 6 EXTENT 2.

def temp-table tt_resumo_apl no-undo
    field tta_cod_banco                    as character format "x(8)" label "Banco" column-label "Banco"
    field tta_cod_produt_financ            as character format "x(8)" label "Produto Financeiro" column-label "Produto Financeiro"
    field tta_cod_tip_produt_financ        as character format "x(8)" label "Tipo Prod Financeiro" column-label "Tipo Prod Fin"
    field tta_cod_operac_financ            as character format "x(10)" label "Opera»’o Financeira" column-label "Opera»’o Financeira"
    field tta_dat_operac_financ            as date format "99/99/9999" initial ? label "Data Opera»’o" column-label "Data Opera»’o"
    field tta_dat_vencto_operac_financ     as date format "99/99/9999" initial ? label "Data Vencto" column-label "Data Vencto"
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
    field ttv_val_operac_financ            as decimal format ">>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Opera»’o" column-label "Saldo Opera»’o"
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
    field ttv_val_operac_financ            as decimal format ">>>,>>>,>>9.9999999999" decimals 10 initial 0 label "Valor Opera»’o" column-label "Saldo Opera»’o"
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
    field ttv_val_tot_aplic_dispon         as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Dispon­vel" column-label "Dispon­vel"
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
    field ttv_val_tot_aplic_dispon         as decimal format "->>,>>>,>>>,>>9.9999999999" decimals 10 initial 0 label "Saldo Dispon­vel" column-label "Dispon­vel"
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
    field ttv_val_tot_aplic_carenc         as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo em Car¼ncia" column-label "Car¼ncia"
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
    field ttv_val_tot_aplic_carenc         as decimal format "->>,>>>,>>>,>>9.9999999999" decimals 10 initial 0 label "Saldo em Car¼ncia" column-label "Car¼ncia"
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
    field tta_ind_operac_financ            as character format "X(12)" label "Opera»’o" column-label "Opera»’o"
    field tta_log_emprest_concedid         as logical format "Sim/N’o" initial no label "Empr²stimo Concedido" column-label "Empr²stimo Concedido"
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.06" &THEN
    field ttv_val_sdo_princ                as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Principal" column-label "Saldo Principal"
&ENDIF
&IF "{&emsfin_version}" >= "5.06" AND "{&emsfin_version}" < "9.99" &THEN
    field ttv_val_sdo_princ                as decimal format "->>,>>>,>>>,>>9.9999999999" decimals 10 initial 0 label "Saldo Principal" column-label "Saldo Principal"
&ENDIF
    field ttv_log_swap_emprest             as logical format "Sim/N’o" initial no label "Classif Prod Financ" column-label "Classif Prod Financ"
    .

DEF TEMP-TABLE tt_resumo_apl_emp_ini LIKE tt_resumo_apl
    FIELD tta_cod_empresa AS CHAR.

DEF TEMP-TABLE tt_resumo_apl_emp_fim LIKE tt_resumo_apl
    FIELD tta_cod_empresa AS CHAR.

DEF TEMP-TABLE tt_log_erros_apl_emp NO-UNDO
    FIELD ttv_num_cod_erro      AS integer
    FIELD ttv_des_msg_ajuda     AS character
    FIELD ttv_des_msg_erro      AS character
    FIELD tta_cod_banco         AS character
    FIELD tta_cod_produt_financ AS character
    FIELD tta_cod_operac_financ AS character
    FIELD tta_dat_transacao     AS date
    FIELD tta_ind_tip_trans_apl AS character.

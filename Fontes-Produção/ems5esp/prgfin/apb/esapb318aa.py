/*****************************************************************************
** Copyright DATASUL S.A. (1994)
** Todos os Direitos Reservados.
** 
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so' podera ser feita mediante
** autorizacao expressa.
**
** Programa..............: rpt_compl_impto_retid_ap
** Descricao.............: Relat Imposto Retido a Recolher
** Versao................:  1.00.00.034
** Procedimento..........: rel_impto_retid_recolh_apb
** Nome Externo..........: prgfin/apb/apb318aa.py
** Data Geracao..........: 21/01/2011 - 15:57:08
** Criado por............: Uno
** Criado em.............: 24/05/1996 15:10:29
** Alterado por..........: fut43120
** Alterado em...........: 03/11/2010 15:20:55
** Gerado por............: fut42929
*****************************************************************************/

def var c-versao-prg as char initial " 1.00.00.034":U no-undo.

{include/i_dbinst.i}
{include/i_dbtype.i}
{include/i_fcldef.i}


&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i rpt_compl_impto_retid_ap APB}
&ENDIF

/******************************* Private-Data *******************************/
assign this-procedure:private-data = "HLP=5":U.
/*************************************  *************************************/

&if "{&emsfin_dbinst}" <> "yes" &then
run pi_messages (input "show",
                 input 5884,
                 input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9", 
                                    "EMSFIN")) /*msg_5884*/.
&elseif "{&emsfin_version}" < "5.01" &then
run pi_messages (input "show",
                 input 5009,
                 input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9", 
                                    "RPT_COMPL_IMPTO_RETID_AP","~~EMSFIN", "~~{~&emsfin_version}", "~~5.01")) /*msg_5009*/.
&else

/********************* Temporary Table Definition Begin *********************/

def temp-table tt_compl_impto_retid_ap no-undo
    field tta_cod_unid_federac             as character format "x(3)" label "Unidade Federaá∆o" column-label "UF"
    field tta_cod_classif_impto            as character format "x(05)" initial "00000" label "Class Imposto" column-label "Class Imposto"
    field tta_cod_pais                     as character format "x(3)" label "Pa°s" column-label "Pa°s"
    field tta_cod_imposto                  as character format "x(5)" label "Imposto" column-label "Imposto"
    field tta_dat_vencto_tit_ap            as date format "99/99/9999" initial today label "Data Vencimento" column-label "Dt Vencto"
    field tta_cod_indic_econ_impto         as character format "x(8)" label "Indicador Economico" column-label "Indicador Economico"
    field tta_val_aliq_impto               as decimal format ">9.99" decimals 2 initial 0.00 label "Al°quota" column-label "Aliq"
    field tta_val_rendto_tribut            as decimal format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Rendto Tribut†vel" column-label "Vl Rendto Tribut"
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_espec_docto              as character format "x(3)" label "EspÇcie Documento" column-label "EspÇcie"
    field tta_cod_ser_docto                as character format "x(3)" label "SÇrie Documento" column-label "SÇrie"
    field tta_cdn_fornecedor               as Integer format ">>>,>>>,>>9" initial 0 label "Fornecedor" column-label "Fornecedor"
    field tta_cod_tit_ap                   as character format "x(10)" label "T°tulo" column-label "T°tulo"
    field tta_cod_parcela                  as character format "x(02)" label "Parcela" column-label "Parc"
    field ttv_val_imposto                  as decimal format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Imposto" column-label "Valor Imposto"
    field ttv_cod_dwb_field_rpt            as character extent 13 format "x(32)" label "Conjunto" column-label "Conjunto"
    field ttv_val_impto_indic_econ_pais    as decimal format "->>>,>>>,>>9.99" decimals 2 label "Imposto IE Pa°s" column-label "Impto IE Pa°s"
    field ttv_dat_fato_gerador             as date format "99/99/9999" label "Dat Fato Gerad" column-label "Dat Fato Gerad"
    field ttv_dat_recolhto                 as date format "99/99/9999" label "Data Recolhimento" column-label "Dat Recolhimento"
    field ttv_cod_indic_econ_rdt           as character format "x(8)" label "IE Rendto Tribut" column-label "IE Rendto Tribut"
    field tta_nom_pessoa                   as character format "x(40)" label "Nome" column-label "Nome"
    field ttv_cod_tit_ap                   as character format "x(10)" label "T°tulo Ap" column-label "T°tulo Ap"
    field ttv_cod_espec_docto              as character format "x(3)" label "EspÇcie Documento" column-label "EspÇcie"
    field ttv_cod_ser_docto                as character format "x(3)" label "SÇrie Docto" column-label "SÇrie"
    field ttv_cod_parcela                  as character format "x(02)" label "Parcela" column-label "Parc"
    field ttv_cod_id_feder                 as character format "x(20)" initial ? label "Fornecedor" column-label "ID Federal"
    field ttv_cdn_fornecedor               as Integer format ">>>,>>>,>>9" initial 0 label "Fornecedor" column-label "Fornecedor"
    field ttv_dat_transacao                as date format "99/99/9999" initial today label "Data Transaá∆o" column-label "Data Transaá∆o"
    field tta_cod_id_feder                 as character format "x(20)" initial ? label "ID Federal" column-label "ID Federal"
    field ttv_nom_pessoa                   as character format "x(40)" label "Nome" column-label "Nome"
    field ttv_val_origin_tit_ap            as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Original" column-label "Valor Original"
    field ttv_val_origin_tit_ap_2          as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Original" column-label "Vl Impto Orig"
    field ttv_val_imposto_rel              as decimal format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Imposto" column-label "Val Sdo Impto"
    field ttv_dat_emis                     as date format "99/99/9999" initial today label "Data Emiss∆o" column-label "Data Emiss∆o"
    field ttv_log_recolh                   as logical format "Sim/N∆o" initial yes label "Recolher" column-label "Recolher"
    .

def temp-table tt_compl_impto_retid_ap_menor no-undo
    field tta_cod_empresa                  as character format "x(3)" label "Empresa" column-label "Empresa"
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_num_id_tit_ap                as integer format "9999999999" initial 0 label "Token Tit AP" column-label "Token Tit AP"
    field tta_val_rendto_tribut            as decimal format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Rendto Tribut†vel" column-label "Vl Rendto Tribut"
    field tta_cod_unid_federac             as character format "x(3)" label "Unidade Federaá∆o" column-label "UF"
    field tta_cod_classif_impto            as character format "x(05)" initial "00000" label "Class Imposto" column-label "Class Imposto"
    field tta_cod_pais                     as character format "x(3)" label "Pa°s" column-label "Pa°s"
    field tta_cod_imposto                  as character format "x(5)" label "Imposto" column-label "Imposto"
    field tta_dat_vencto_tit_ap            as date format "99/99/9999" initial today label "Data Vencimento" column-label "Dt Vencto"
    field tta_cod_indic_econ_impto         as character format "x(8)" label "Indicador Economico" column-label "Indicador Economico"
    field tta_cdn_fornecedor               as Integer format ">>>,>>>,>>9" initial 0 label "Fornecedor" column-label "Fornecedor"
    .

def temp-table tt_converter_finalid_econ no-undo
    field tta_cod_finalid_econ             as character format "x(10)" label "Finalidade" column-label "Finalidade"
    field tta_cod_indic_econ               as character format "x(8)" label "Moeda" column-label "Moeda"
    field tta_dat_cotac_indic_econ         as date format "99/99/9999" initial ? label "Data Cotaá∆o" column-label "Data Cotaá∆o"
    field tta_val_cotac_indic_econ         as decimal format ">>>>,>>9.9999999999" decimals 10 initial 0 label "Cotaá∆o" column-label "Cotaá∆o"
    field tta_val_cotac_tax_juros          as decimal format ">>>>,>>9.9999999999" decimals 10 initial 0 label "Cotac Taxa Juros" column-label "Cotac Taxa Juros"
    field tta_val_prev_cotac_fasb          as decimal format ">>>>,>>9.9999999999" decimals 10 initial 0 label "Cotac Previs Fasb" column-label "Cotac Previs Fasb"
    field tta_val_cotac_cm_emis            as decimal format ">>>>,>>9.9999999999" decimals 10 initial 0 label "Cotac Cm Emiss" column-label "Cotac Cm Emiss"
    field tta_val_cotac_cm_vencto          as decimal format ">>>>,>>9.9999999999" decimals 10 initial 0 label "Cotac Cm Vencto" column-label "Cotac Cm Vencto"
    field tta_val_cotac_cm_pagto           as decimal format ">>>>,>>9.9999999999" decimals 10 initial 0 label "Cotac Cm Pagto" column-label "Cotac CM Pagto"
    field tta_val_transacao                as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Transaá∆o" column-label "Transaá∆o"
    field tta_val_variac_cambial           as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Vl Varic Cambial" column-label "Variac Cambial"
    field tta_val_acerto_cmcac             as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Vl Acerto CMCAC" column-label "Vl Acerto CMCAC"
    field tta_val_fatorf                   as decimal format "->999.9999999999" decimals 10 initial 0 label "Fator F" column-label "Fator F"
    field tta_val_fatorx                   as decimal format "->999.9999999999" decimals 10 initial 0 label "Fator X" column-label "Fator X"
    field tta_val_fatory                   as decimal format "->999.9999999999" decimals 10 initial 0 label "Fator Y" column-label "Fator Y"
    field tta_val_ganho_perda_cm           as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "G/P CM" column-label "G/P CM"
    field tta_val_ganho_perda_projec       as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "G/P Projeá∆o" column-label "G/P Projeá∆o"
    field tta_ind_forma_conver             as character format "X(10)" initial "Direta" label "Forma Convers∆o" column-label "Forma Convers∆o"
    field ttv_val_multa                    as decimal format "->>>,>>>,>>9.99" decimals 2 label "Vl Multa" column-label "Vl Multa"
    field ttv_val_desc                     as decimal format "->>>,>>>,>>9.99" decimals 2 label "Vl Desc" column-label "Vl Desc"
    field ttv_val_juros                    as decimal format "->>>,>>>,>>9.99" decimals 2 label "Valor Juros" column-label "Valor Juros"
    field ttv_val_abat                     as decimal format "->>>,>>>,>>9.99" decimals 2 label "Valor Abatimento" column-label "Valor Abatimento"
    field ttv_val_cm                       as decimal format "->>>>>,>>>,>>9.99" decimals 4 initial 0 label "Correá∆o Monet†ria" column-label "Correá∆o Monet†ria"
    field tta_val_despes_bcia              as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Vl Desp Banc" column-label "Vl Desp Banc"
    .



/********************** Temporary Table Definition End **********************/

/************************** Buffer Definition Begin *************************/

&if "{&emsfin_version}" >= "5.01" &then
def buffer b_movto_tit_ap
    for movto_tit_ap.
&endif
&if "{&emsuni_version}" >= "1.00" &then
def buffer b_pais
    for pais.
&endif
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

def var v_cdn_fornecedor_fim
    as Integer
    format ">>>,>>>,>>9":U
    initial 999999999
    label "atÇ"
    column-label "Fornecedor"
    no-undo.
def var v_cdn_fornecedor_ini
    as Integer
    format ">>>,>>>,>>9":U
    initial 0
    label "Fornecedor"
    column-label "Fornecedor"
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
def var v_cod_classif_impto_faixa_fim
    as character
    format "x(5)":U
    initial "ZZZZZ"
    label "atÇ"
    column-label "atÇ"
    no-undo.
def var v_cod_classif_impto_faixa_inic
    as character
    format "x(5)":U
    label "Classificaá∆o"
    column-label "Calssificaá∆o"
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
def var v_cod_impto_faixa_fim
    as character
    format "x(5)":U
    initial "ZZZZZ"
    label "Imposto"
    column-label "Imposto"
    no-undo.
def var v_cod_impto_faixa_inic
    as character
    format "x(5)":U
    label "Imposto"
    column-label "Imposto"
    no-undo.
def var v_cod_indic_econ_fim
    as character
    format "x(8)":U
    initial "ZZZZZZZZ"
    label "atÇ"
    column-label "Final"
    no-undo.
def var v_cod_indic_econ_ini
    as character
    format "x(8)":U
    label "Moeda"
    column-label "Inicial"
    no-undo.
def var v_cod_indic_econ_rdt
    as character
    format "x(8)":U
    label "IE Rendto Tribut"
    column-label "IE Rendto Tribut"
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
def var v_cod_pais_faixa_fim
    as character
    format "x(3)":U
    initial "ZZZ"
    label "Pa°s Final"
    column-label "Pa°s Final"
    no-undo.
def var v_cod_pais_faixa_inic
    as character
    format "x(03)":U
    label "Pa°s"
    column-label "Pa°s"
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
def var v_cod_return
    as character
    format "x(40)":U
    no-undo.
def var v_cod_unid_federac_faixa_fim
    as character
    format "x(3)":U
    initial "ZZZ"
    label "UF Final"
    column-label "UF Final"
    no-undo.
def var v_cod_unid_federac_faixa_inic
    as character
    format "x(3)":U
    label "Unidade Federaá∆o"
    column-label "Unidade Federaá∆o"
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
def var v_dat_fato_gerador
    as date
    format "99/99/9999":U
    label "Dat Fato Gerad"
    column-label "Dat Fato Gerad"
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
def var v_dat_recolhto
    as date
    format "99/99/9999":U
    label "Data Recolhimento"
    column-label "Dat Recolhimento"
    no-undo.
def var v_dat_vencto_docto_fim
    as date
    format "99/99/9999":U
    initial today
    label "atÇ"
    column-label "atÇ"
    no-undo.
def var v_dat_vencto_docto_inic
    as date
    format "99/99/9999":U
    label "Vencimento"
    column-label "Vencimento"
    no-undo.
def var v_des_fornec_impto
    as character
    format "x(80)":U
    label "Fornecedor"
    column-label "Fornecedor"
    no-undo.
def var v_des_msg_relat
    as character
    format "x(80)":U
    view-as editor max-chars 2000 scrollbar-vertical
    size 80 by 4
    bgcolor 15 font 2
    label "Mensagem"
    column-label "Mensagem"
    no-undo.
def var v_hdl_frame
    as Handle
    format ">>>>>>9":U
    no-undo.
def var v_hdl_frame_aux
    as Handle
    format ">>>>>>9":U
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
def var v_ind_classif_rpt_impto_retid_ap
    as character
    format "X(40)":U
    view-as radio-set Vertical
    radio-buttons "Por Classificaá∆o do Imposto", "Por Classificaá∆o do Imposto", "Por Estab/Fornecedor Origem", "Por Estab/Fornecedor Origem", "Por Estab/Fornecedor Favorecido", "Por Estab/Fornecedor Favorecido", "Por Fornecedor Origem/Por Estab", "Por Fornecedor Origem/Por Estab"
     /*l_por_classificacao_do_imposto*/ /*l_por_classificacao_do_imposto*/ /*l_por_estabfornec_origem*/ /*l_por_estabfornec_origem*/ /*l_por_estabfornec_favorec*/ /*l_por_estabfornec_favorec*/ /*l_por_fornecedor_origempor_estab*/ /*l_por_fornecedor_origempor_estab*/
    bgcolor 8 
    label "Classificaá∆o"
    column-label "Classificaá∆o"
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
    format "Sim/N∆o"
    initial yes
    no-undo.
def var v_log_impto_sem_sdo
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    label "Imposto sem Saldo"
    column-label "Imposto sem Saldo"
    no-undo.
def var v_log_method
    as logical
    format "Sim/N∆o"
    initial yes
    no-undo.
def var v_log_mostra_impto_retid
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    label "Imposto Retido"
    column-label "Imposto Retido"
    no-undo.
def var v_log_mostra_impto_taxado
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    label "Imposto Taxado"
    column-label "Imposto Taxado"
    no-undo.
def var v_log_mostra_inform_fornec
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    no-undo.
def var v_log_por_estab
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
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
def var v_log_quebra_fornec
    as logical
    format "Sim/N∆o"
    initial No
    view-as toggle-box
    label "Por Fornecedor"
    no-undo.
def var v_log_recolh
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    label "Recolher"
    column-label "Recolher"
    no-undo.
def var v_log_tot_classif_impto
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    no-undo.
def var v_log_tot_fornec_dest
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    no-undo.
def var v_log_tot_fornec_orig
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
def var v_rec_log
    as recid
    format ">>>>>>9":U
    no-undo.
def var v_rec_table_epc
    as recid
    format ">>>>>>9":U
    no-undo.
def var v_val_imposto
    as decimal
    format ">,>>>,>>>,>>9.99":U
    decimals 2
    label "Valor Imposto"
    column-label "Valor Imposto"
    no-undo.
def var v_val_impto_indic_econ_pais
    as decimal
    format "->>>,>>>,>>9.99":U
    decimals 2
    label "Imposto IE Pa°s"
    column-label "Impto IE Pa°s"
    no-undo.
def var v_val_tot_classif_impto
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    extent 4
    label "Total Classif Impto"
    column-label "Total Classif Impto"
    no-undo.
def var v_val_tot_dat_vencto
    as decimal
    format "->,>>>,>>>,>>9.99":U
    decimals 2
    label "Total Data Vencto"
    column-label "Total Data Vencto"
    no-undo.
def var v_val_tot_fornec_favorec
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    extent 4
    no-undo.
def var v_val_tot_fornec_orig
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    extent 4
    no-undo.
def var v_val_tot_impto_estab
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    initial [0]
    extent 4
    label "Total Estabeleciment"
    no-undo.
def var v_val_tot_indic_econ
    as decimal
    format "->,>>>,>>>,>>9.99":U
    decimals 2
    label "Total Indic Econ"
    column-label "Total Indic Econ"
    no-undo.
def var v_val_tot_indic_econ_pais
    as decimal
    format "->,>>>,>>>,>>9.99":U
    decimals 2
    label "Total IE Pa°s"
    column-label "Total IE Pa°s"
    no-undo.
def var v_wgh_focus
    as widget-handle
    format ">>>>>>9":U
    no-undo.
def var v_wgh_frame_epc
    as widget-handle
    format ">>>>>>9":U
    no-undo.
def var v_log_faixa_fornec_orig          as logical         no-undo. /*local*/


/************************** Variable Definition End *************************/

/*************************** Menu Definition Begin **************************/

.

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
def rectangle rt_009
    size 1 by 1
    edge-pixels 2.
def rectangle rt_cxcf
    size 1 by 1
    fgcolor 1 edge-pixels 2.
def rectangle rt_dimensions
    size 1 by 1
    edge-pixels 2.
def rectangle rt_run
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
    radio-buttons "Terminal", "Terminal", "Arquivo", "Arquivo", "Impressora", "Impressora"
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
def new shared var v_rpt_s_1_columns as integer initial 215.
def new shared var v_rpt_s_1_bottom as integer initial 65.
def new shared var v_rpt_s_1_page as integer.
def new shared var v_rpt_s_1_name as character initial "Impostos a Recolher".
def frame f_rpt_s_1_header_period header
    "---------------------------------------------------------------------------------------------------------------------------------------------------------------------------" at 1
    "P†gina:" at 173
    (page-number (s_1) + v_rpt_s_1_page) at 180 format ">>>>>9" skip
    v_nom_enterprise at 1 format "x(40)"
    v_nom_report_title at 145 format "x(40)" skip
    "Per°odo: " at 1
    v_dat_inic_period at 10 format "99/99/9999"
    "A" at 21
    v_dat_fim_period at 23 format "99/99/9999"
    "-------------------------------------------------------------------------------------------------------------------------------------" at 34
    v_dat_execution at 168 format "99/99/9999" "- "
    v_hra_execution at 181 format "99:99" skip (1)
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_header_unique header
    "---------------------------------------------------------------------------------------------------------------------------------------------------------------------------" at 1
    'P†gina:' at 173
    (page-number (s_1) + v_rpt_s_1_page) at 180 format '>>>>>9' skip
    v_nom_enterprise at 1 format 'x(40)'
    v_nom_report_title at 146 format 'x(40)' skip
    '----------------------------------------------------------------------------------------------------------------------------------------------------------------------' at 1
    v_dat_execution at 168 format '99/99/9999' '- '
    v_hra_execution at 181 format "99:99" skip (1)
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_footer_last_page header
    "Èltima p†gina " at 1
    "---------------------------------------------------------------------------------------------------------------------------------------------------" at 15
    v_nom_prog_ext at 163 format "x(08)" "- "
    v_cod_release at 174 format "x(12)" skip
    with no-box no-labels width 185 page-bottom stream-io.
def frame f_rpt_s_1_footer_normal header
    "----------------------------------------------------------------------------------------------------------------------------------------------------------------" at 1
    "- " at 161
    v_nom_prog_ext at 163 format "x(08)" "- "
    v_cod_release at 174 format "x(12)" skip
    with no-box no-labels width 185 page-bottom stream-io.
def frame f_rpt_s_1_footer_param_page header
    "P†gina ParÉmetros " at 1
    "-----------------------------------------------------------------------------------------------------------------------------------------------" at 19
    v_nom_prog_ext at 163 format "x(08)" "- "
    v_cod_release at 174 format "x(12)" skip
    with no-box no-labels width 185 page-bottom stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_detdois header
    "Esp" at 1
    "SÇrie" at 5
    "T°tulo" at 11
    "/P" at 28
    "Fornec" to 41
    "Esp" at 43
    "SÇrie" at 47
    "T°tulo" at 53
    "/P" at 70
    "Dat Emis" at 73
    "Recolhim" at 84
    "Rend Tri" at 95
    "Aliq" to 108
    "Class" at 110
    "Rend Tribut" to 131
    "Vl Impto Orig" to 145
    "Val Sdo Impto" to 162
    "Impto IE Pa°s" to 176
    "Recolher" at 178 skip
    "---" at 1
    "-----" at 5
    "----------------" at 11
    "--" at 28
    "-----------" to 41
    "---" at 43
    "-----" at 47
    "----------------" at 53
    "--" at 70
    "----------" at 73
    "----------" at 84
    "--------" at 95
    "-----" to 108
    "-----" at 110
    "----------------" to 131
    "-------------" to 145
    "----------------" to 162
    "-------------" to 176
    "--------" at 178 skip
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_detquatro header
    "Fornec" to 9
    "Esp" at 11
    "SÇrie" at 15
    "T°tulo" at 21
    "/P" at 37
    "Fornec" to 48
    "Esp" at 50
    "SÇrie" at 54
    "T°tulo" at 60
    "/P" at 76
    "Dat Emis" at 79
    "Recolhim" at 88
    "Rend T" at 97
    "Aliq" to 108
    "Class" at 110
    "Rend Tribut" to 131
    "Vl Impto Orig" to 145
    "Val Sdo Impto" to 162
    "Impto IE Pa°s" to 176
    "Recolher" at 178 skip
    "---------" to 9
    "---" at 11
    "-----" at 15
    "----------------" at 21
    "--" at 37
    "---------" to 48
    "---" at 50
    "-----" at 54
    "----------------" at 60
    "--" at 76
    "--------" at 79
    "--------" at 88
    "------" at 97
    "-----" to 108
    "-----" at 110
    "----------------" to 131
    "-------------" to 145
    "----------------" to 162
    "-------------" to 176
    "--------" at 178 skip
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_detum header
    "Esp" at 1
    "SÇrie" at 5
    "T°tulo" at 11
    "/P" at 28
    "Fornec" to 41
    "Esp" at 43
    "SÇrie" at 47
    "T°tulo" at 53
    "/P" at 70
    "Dat Emis" at 73
    "Recolhim" at 84
    "Rend Tri" at 95
    "Aliq" to 108
    "Class" at 110
    "Rend Tribut" to 131
    "Vl Impto Orig" to 145
    "Val Sdo Impto" to 162
    "Impto IE Pa°s" to 176
    "Recolher" at 178 skip
    "---" at 1
    "-----" at 5
    "----------------" at 11
    "--" at 28
    "-----------" to 41
    "---" at 43
    "-----" at 47
    "----------------" at 53
    "--" at 70
    "----------" at 73
    "----------" at 84
    "--------" at 95
    "-----" to 108
    "-----" at 110
    "----------------" to 131
    "-------------" to 145
    "----------------" to 162
    "-------------" to 176
    "--------" at 178 skip
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_erro header
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Estab" at 1
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Estab" at 1
&ENDIF
    "Esp" at 7
    "SÇrie" at 11
    "Fornec" to 27
    "T°tulo" at 29
    "/P" at 48
    "Mensagem" at 53 skip
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "-----" at 1
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "-----" at 1
&ENDIF
    "---" at 7
    "-----" at 11
    "-----------" to 27
    "----------------" at 29
    "--" at 48
    "--------------------------------------------------------------------------------" at 53 skip
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_normal header
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Estab" at 2
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Estab" at 2
&ENDIF
    "Esp" at 8
    "SÇrie" at 12
    "Fornecedor" to 28
    "T°tulo" at 31
    "/P" at 50
    "Fato Gerad" at 54
    "Recolhim" at 66
    "Rend Trib" at 78
    "Aliq" to 93
    "Classif" at 96
    "Rend Tribut" to 131
    "Vl Impto Orig" to 145
    "Val Sdo Impto" to 162
    "Impto IE Pa°s" to 176
    "Recolher" at 178 skip
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "-----" at 2
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "-----" at 2
&ENDIF
    "---" at 8
    "-----" at 12
    "-----------" to 28
    "----------------" at 31
    "--" at 50
    "----------" at 54
    "----------" at 66
    "---------" at 78
    "-----" to 93
    "-------" at 96
    "----------------" to 131
    "-------------" to 145
    "----------------" to 162
    "-------------" to 176
    "--------" at 178 skip
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_tres header
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Estab" at 1
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Estab" at 1
&ENDIF
    "Esp" at 7
    "SÇrie" at 11
    "T°tulo" at 17
    "/P" at 34
    "Fornec" to 47
    "Esp" at 49
    "SÇrie" at 53
    "T°tulo" at 59
    "/P" at 76
    "Dat Emis" at 79
    "Recolhim" at 88
    "Rend T" at 97
    "Aliq" to 108
    "Class" at 110
    "Rend Tribut" to 131
    "Vl Impto Orig" to 145
    "Val Sdo Impto" to 162
    "Impto IE Pa°s" to 176
    "Recolher" at 178 skip
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "-----" at 1
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "-----" at 1
&ENDIF
    "---" at 7
    "-----" at 11
    "----------------" at 17
    "--" at 34
    "-----------" to 47
    "---" at 49
    "-----" at 53
    "----------------" at 59
    "--" at 76
    "--------" at 79
    "--------" at 88
    "------" at 97
    "-----" to 108
    "-----" at 110
    "----------------" to 131
    "-------------" to 145
    "----------------" to 162
    "-------------" to 176
    "--------" at 178 skip
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_parametros_Lay_mensagem header
    skip (1)
    entry(1, return-value, chr(255)) at 27 format "x(80)" skip
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_parametros_Lay_parametros header
    skip (1)
    "   Classificaá∆o: " at 37
    v_ind_classif_rpt_impto_retid_ap at 55 format "X(40)" view-as text skip
    "Totaliza Classificaá∆o do Imposto:" at 20
    v_log_tot_classif_impto at 55 format "Sim/N∆o" view-as text skip
    "Totaliza Fornecedor Origem:" at 27
    v_log_tot_fornec_orig at 55 format "Sim/N∆o" view-as text skip
    "Totaliza Fornecedor Favorecido:" at 23
    v_log_tot_fornec_dest at 55 format "Sim/N∆o" view-as text skip
    "   Imposto Retido: " at 36
    v_log_mostra_impto_retid at 55 format "Sim/N∆o" view-as text skip
    "   Imposto Taxado: " at 36
    v_log_mostra_impto_taxado at 55 format "Sim/N∆o" view-as text skip
    "    Imposto sem Saldo: " at 32
    v_log_impto_sem_sdo at 55 format "Sim/N∆o" view-as text skip
    "    Apenas a Recolher: " at 32
    v_log_recolh at 55 format "Sim/N∆o" view-as text skip
    "  Pa°s: " at 47
    v_cod_pais_faixa_inic at 55 format "x(03)" view-as text
    "atÇ: " at 72
    v_cod_pais_faixa_fim at 77 format "x(3)" view-as text skip
    "  Unidade Federaá∆o: " at 34
    v_cod_unid_federac_faixa_inic at 55 format "x(3)" view-as text
    "atÇ: " at 72
    v_cod_unid_federac_faixa_fim at 77 format "x(3)" view-as text skip
    "  Imposto: " at 44
    v_cod_impto_faixa_inic at 55 format "x(5)" view-as text
    "atÇ: " at 72
    v_cod_impto_faixa_fim at 77 format "x(5)" view-as text skip
    "Classificaá∆o: " at 40
    v_cod_classif_impto_faixa_inic at 55 format "x(5)" view-as text
    "atÇ: " at 72
    v_cod_classif_impto_faixa_fim at 77 format "x(5)" view-as text skip
    "  Vencimento: " at 41
    v_dat_vencto_docto_inic at 55 format "99/99/9999" view-as text
    "atÇ: " at 72
    v_dat_vencto_docto_fim at 77 format "99/99/9999" view-as text skip
    "  Moeda: " at 46
    v_cod_indic_econ_ini at 55 format "x(8)" view-as text
    "atÇ: " at 72
    v_cod_indic_econ_fim at 77 format "x(8)" view-as text skip
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_param_fornec_Lay_fornecedor header
    "Fornec Origem:" at 40
    v_cdn_fornecedor_ini to 64 format ">>>,>>>,>>9" view-as text
    "atÇ: " at 72
    v_cdn_fornecedor_fim to 87 format ">>>,>>>,>>9" view-as text skip
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_quebra_Lay_branco header skip (1)
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_quebra_Lay_classif header
    "Classificaá∆o" at 13
    ":" at 26
    /* Atributo tt_compl_impto_retid_ap.tta_cod_classif_impto ignorado */ skip
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_quebra_Lay_estab header
    /* Atributo tt_compl_impto_retid_ap.tta_cod_estab ignorado */ skip (1)
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_quebra_Lay_fato_dois header
    "----- Fato Gerador Imposto ------" at 24 skip
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_quebra_Lay_fato_quatro header
    "----- Fato Gerador Imposto ------" at 1 skip
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_quebra_Lay_fato_tres header
    "-- Fato Gerador Impto ---" at 1 skip
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_quebra_Lay_fato_um header
    "- Fato Gerad Impto --" at 1 skip
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_quebra_Lay_fornec_favor header
    "Fornecedor Favorecido:" at 5
    /* Atributo tt_compl_impto_retid_ap.ttv_cdn_fornecedor ignorado */
    "-" at 40
    /* Atributo tt_compl_impto_retid_ap.ttv_nom_pessoa ignorado */ skip
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_quebra_Lay_fornec_orig header
    "Fornecedor Origem:" at 9
    /* Atributo tt_compl_impto_retid_ap.tta_cdn_fornecedor ignorado */
    "-" at 40
    /* Atributo tt_compl_impto_retid_ap.tta_nom_pessoa ignorado */ skip
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_quebra_Lay_id_fed_fav header
    "ID Federal" at 16
    ":" at 26
    /* Atributo tt_compl_impto_retid_ap.ttv_cod_id_feder ignorado */ skip
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_quebra_Lay_id_fed_orig header
    "ID Federal" at 16
    ":" at 26
    /* Atributo tt_compl_impto_retid_ap.tta_cod_id_feder ignorado */ skip
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_quebra_Lay_impto header
    /* Atributo tt_compl_impto_retid_ap.tta_cod_imposto ignorado */ skip (1)
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_quebra_Lay_pais header
    /* Atributo tt_compl_impto_retid_ap.tta_cod_pais ignorado */ skip (1)
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_quebra_Lay_uf header
    /* Atributo tt_compl_impto_retid_ap.tta_cod_unid_federac ignorado */ skip (1)
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_total_Lay_classif header
    "----------------" at 116
    "-------------" at 133
    "----------------" at 147
    "-------------" at 164 skip
    "Totais Classificaá∆o do Imposto:" at 77
    v_val_tot_classif_impto[1] to 131 format ">,>>>,>>>,>>9.99" view-as text
    v_val_tot_classif_impto[4] to 145 format ">,>>>,>>9.99" view-as text
    v_val_tot_classif_impto[2] to 162 format ">,>>>,>>>,>>9.99" view-as text
    v_val_tot_classif_impto[3] to 176 format ">,>>>,>>9.99" view-as text skip
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_total_Lay_estab header
    "----------------" at 116
    "-------------" at 133
    "----------------" at 147
    "-------------" at 164 skip
    "Total do Estabelecimento:" at 84
    v_val_tot_impto_estab[1] to 131 format ">,>>>,>>>,>>9.99" view-as text
    v_val_tot_impto_estab[4] to 145 format ">>,>>>,>>9.99" view-as text
    v_val_tot_impto_estab[2] to 162 format ">,>>>,>>>,>>9.99" view-as text
    v_val_tot_impto_estab[3] to 176 format ">>,>>>,>>9.99" view-as text skip
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_total_Lay_fornecfavor header
    "----------------" at 116
    "-------------" at 133
    "----------------" at 147
    "-------------" at 164 skip
    "Totais Fornecedor Favorecido:" at 80
    v_val_tot_fornec_favorec[1] to 131 format ">,>>>,>>>,>>9.99" view-as text
    v_val_tot_fornec_favorec[4] to 145 format ">,>>>,>>9.99" view-as text
    v_val_tot_fornec_favorec[2] to 162 format ">,>>>,>>>,>>9.99" view-as text
    v_val_tot_fornec_favorec[3] to 176 format ">,>>>,>>9.99" view-as text skip
    with no-box no-labels width 185 page-top stream-io.
def frame f_rpt_s_1_Grp_total_Lay_fornecorig header
    "----------------" at 116
    "-------------" at 133
    "----------------" at 147
    "-------------" at 164 skip
    "Totais Fornecedor Origem:" at 84
    v_val_tot_fornec_orig[1] to 131 format ">,>>>,>>>,>>9.99" view-as text
    v_val_tot_fornec_orig[4] to 145 format ">,>>>,>>9.99" view-as text
    v_val_tot_fornec_orig[2] to 162 format ">,>>>,>>>,>>9.99" view-as text
    v_val_tot_fornec_orig[3] to 176 format ">,>>>,>>9.99" view-as text skip
    with no-box no-labels width 185 page-top stream-io.


/*************************** Report Definition End **************************/

/************************** Frame Definition Begin **************************/

def frame f_rpt_41_compl_impto_retid_ap
    rt_001
         at row 01.50 col 44.57
    " Seleá∆o " view-as text
         at row 01.20 col 46.57 bgcolor 8 
    rt_002
         at row 01.50 col 02.00
    " ParÉmetros " view-as text
         at row 01.20 col 04.00 bgcolor 8 
    rt_007
         at row 05.63 col 46.00
    " Faixa " view-as text
         at row 05.33 col 48.00 bgcolor 8 
    rt_006
         at row 06.88 col 03.29
    " Totalizaá∆o " view-as text
         at row 06.58 col 05.29 bgcolor 8 
    rt_005
         at row 02.25 col 03.29
    " Classificaá∆o " view-as text
         at row 01.95 col 05.29 bgcolor 8 
    rt_target
         at row 14.67 col 02.00
    " Destino " view-as text
         at row 14.37 col 04.00 bgcolor 8 
    rt_run
         at row 14.67 col 48.00
    " Execuá∆o " view-as text
         at row 14.37 col 50.00
    rt_dimensions
         at row 14.67 col 72.72
    " Dimens‰es " view-as text
         at row 14.37 col 74.72
    rt_003
         at row 02.08 col 46.00
    " Filtro " view-as text
         at row 01.78 col 48.00 bgcolor 8 
    rt_cxcf
         at row 18.17 col 02.00 bgcolor 7 
    rt_009
         at row 12.00 col 03.29
    " Quebra " view-as text
         at row 11.70 col 05.29 bgcolor 8 
    v_cod_pais_faixa_inic
         at row 06.21 col 56.43 colon-aligned label "Pa°s"
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_pais_faixa_fim
         at row 06.21 col 71.86 colon-aligned label "atÇ"
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_unid_federac_faixa_inic
         at row 07.21 col 56.43 colon-aligned label "UF"
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_unid_federac_faixa_fim
         at row 07.21 col 71.86 colon-aligned label "atÇ"
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_impto_faixa_inic
         at row 08.21 col 56.43 colon-aligned label "Imposto"
         view-as fill-in
         size-chars 6.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_impto_faixa_fim
         at row 08.21 col 71.86 colon-aligned label "atÇ"
         view-as fill-in
         size-chars 6.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_classif_impto_faixa_inic
         at row 09.21 col 56.43 colon-aligned label "Classificaá∆o"
         view-as fill-in
         size-chars 6.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_classif_impto_faixa_fim
         at row 09.21 col 71.86 colon-aligned label "atÇ"
         view-as fill-in
         size-chars 6.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_dat_vencto_docto_inic
         at row 10.21 col 56.43 colon-aligned label "Vencimento"
         help "Data de vencimento inicial"
         view-as fill-in
         size-chars 11.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_dat_vencto_docto_fim
         at row 10.21 col 71.86 colon-aligned label "atÇ"
         view-as fill-in
         size-chars 11.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_indic_econ_ini
         at row 11.21 col 56.43 colon-aligned label "Moeda"
         help "Indicador Econìmico Inicial"
         view-as fill-in
         size-chars 9.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_indic_econ_fim
         at row 11.21 col 71.86 colon-aligned label "atÇ"
         help "Indicador Econìmico Final"
         view-as fill-in
         size-chars 9.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cdn_fornecedor_ini
         at row 12.21 col 58.43 no-label
         help "C¢digo Fornecedor"
         view-as fill-in
         size-chars 12.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cdn_fornecedor_fim
         at row 12.21 col 71.86 colon-aligned label "atÇ"
         help "C¢digo Fornecedor"
         view-as fill-in
         size-chars 12.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_log_mostra_impto_retid
         at row 02.46 col 51.00 label "Imposto Retido"
         help "Considera Impostos Retidos ?"
         view-as toggle-box
    v_log_mostra_impto_taxado
         at row 02.46 col 69.14 label "Imposto Taxado"
         help "Considera Impostos Taxados ?"
         view-as toggle-box
    v_log_impto_sem_sdo
         at row 03.46 col 51.00 label "Impostos sem Saldo"
         help "Imposto sem Saldo"
         view-as toggle-box
    v_log_recolh
         at row 03.46 col 69.14 label "Apenas a Recolher"
         view-as toggle-box
    v_ind_classif_rpt_impto_retid_ap
         at row 02.75 col 05.72 no-label
         help "Classificaá∆o do Relat¢rio"
         view-as radio-set Vertical
         radio-buttons "Por Classificaá∆o do Imposto", "Por Classificaá∆o do Imposto", "Por Estab/Fornecedor Origem", "Por Estab/Fornecedor Origem", "Por Estab/Fornecedor Favorecido", "Por Estab/Fornecedor Favorecido", "Por Fornecedor Origem/Por Estab", "Por Fornecedor Origem/Por Estab"
          /*l_por_classificacao_do_imposto*/ /*l_por_classificacao_do_imposto*/ /*l_por_estabfornec_origem*/ /*l_por_estabfornec_origem*/ /*l_por_estabfornec_favorec*/ /*l_por_estabfornec_favorec*/ /*l_por_fornecedor_origempor_estab*/ /*l_por_fornecedor_origempor_estab*/
         bgcolor 8 
    v_log_tot_classif_impto
         at row 07.46 col 06.00 label "Classificaá∆o do Imposto"
         view-as toggle-box
    v_log_tot_fornec_orig
         at row 08.46 col 06.00 label "Fornecedor Origem"
         view-as toggle-box
    v_log_tot_fornec_dest
         at row 09.46 col 06.00 label "Fornecedor Favorecido"
         view-as toggle-box
    v_log_por_estab
         at row 10.46 col 06.00 label "Por Estabelecimento"
         view-as toggle-box
    v_log_quebra_fornec
         at row 12.42 col 05.86 label "Por Fornecedor"
         view-as toggle-box
    rs_cod_dwb_output
         at row 15.38 col 03.00
         help "" no-label
    ed_1x40
         at row 16.33 col 03.00
         help "" no-label
    bt_get_file
         at row 16.33 col 42.00 font ?
         help "Pesquisa Arquivo"
    bt_set_printer
         at row 16.33 col 42.00 font ?
         help "Define Impressora e Layout de Impress∆o"
    rs_ind_run_mode
         at row 15.38 col 49.00
         help "" no-label
    v_log_print_par
         at row 16.38 col 49.00 label "Imprime ParÉmetros"
         view-as toggle-box
    v_qtd_line
         at row 15.38 col 81.00 colon-aligned
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_qtd_column
         at row 16.38 col 81.00 colon-aligned
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_close
         at row 18.38 col 03.00 font ?
         help "Fecha"
    bt_print
         at row 18.38 col 14.00 font ?
         help "Imprime"
    bt_can
         at row 18.38 col 25.00 font ?
         help "Cancela"
    bt_hel2
         at row 18.38 col 77.57 font ?
         help "Ajuda"
    "Fornec Origem:"
         at row 12.33 col 47.57 font 1
         view-as text /*l_fornec_origem*/
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 90.00 by 20.00
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Relat¢rio de Impostos a Recolher".
    /* adjust size of objects in this frame */
    assign bt_can:width-chars          in frame f_rpt_41_compl_impto_retid_ap = 10.00
           bt_can:height-chars         in frame f_rpt_41_compl_impto_retid_ap = 01.00
           bt_close:width-chars        in frame f_rpt_41_compl_impto_retid_ap = 10.00
           bt_close:height-chars       in frame f_rpt_41_compl_impto_retid_ap = 01.00
           bt_get_file:width-chars     in frame f_rpt_41_compl_impto_retid_ap = 04.00
           bt_get_file:height-chars    in frame f_rpt_41_compl_impto_retid_ap = 01.08
           bt_hel2:width-chars         in frame f_rpt_41_compl_impto_retid_ap = 10.00
           bt_hel2:height-chars        in frame f_rpt_41_compl_impto_retid_ap = 01.00
           bt_print:width-chars        in frame f_rpt_41_compl_impto_retid_ap = 10.00
           bt_print:height-chars       in frame f_rpt_41_compl_impto_retid_ap = 01.00
           bt_set_printer:width-chars  in frame f_rpt_41_compl_impto_retid_ap = 04.00
           bt_set_printer:height-chars in frame f_rpt_41_compl_impto_retid_ap = 01.08
           ed_1x40:width-chars         in frame f_rpt_41_compl_impto_retid_ap = 38.00
           ed_1x40:height-chars        in frame f_rpt_41_compl_impto_retid_ap = 01.00
           rt_001:width-chars          in frame f_rpt_41_compl_impto_retid_ap = 43.86
           rt_001:height-chars         in frame f_rpt_41_compl_impto_retid_ap = 12.38
           rt_002:width-chars          in frame f_rpt_41_compl_impto_retid_ap = 41.86
           rt_002:height-chars         in frame f_rpt_41_compl_impto_retid_ap = 12.38
           rt_003:width-chars          in frame f_rpt_41_compl_impto_retid_ap = 41.29
           rt_003:height-chars         in frame f_rpt_41_compl_impto_retid_ap = 02.42
           rt_005:width-chars          in frame f_rpt_41_compl_impto_retid_ap = 39.29
           rt_005:height-chars         in frame f_rpt_41_compl_impto_retid_ap = 04.04
           rt_006:width-chars          in frame f_rpt_41_compl_impto_retid_ap = 39.43
           rt_006:height-chars         in frame f_rpt_41_compl_impto_retid_ap = 04.58
           rt_007:width-chars          in frame f_rpt_41_compl_impto_retid_ap = 41.14
           rt_007:height-chars         in frame f_rpt_41_compl_impto_retid_ap = 07.83
           rt_009:width-chars          in frame f_rpt_41_compl_impto_retid_ap = 39.57
           rt_009:height-chars         in frame f_rpt_41_compl_impto_retid_ap = 01.42
           rt_cxcf:width-chars         in frame f_rpt_41_compl_impto_retid_ap = 86.57
           rt_cxcf:height-chars        in frame f_rpt_41_compl_impto_retid_ap = 01.42
           rt_dimensions:width-chars   in frame f_rpt_41_compl_impto_retid_ap = 15.72
           rt_dimensions:height-chars  in frame f_rpt_41_compl_impto_retid_ap = 03.00
           rt_run:width-chars          in frame f_rpt_41_compl_impto_retid_ap = 23.86
           rt_run:height-chars         in frame f_rpt_41_compl_impto_retid_ap = 03.00
           rt_target:width-chars       in frame f_rpt_41_compl_impto_retid_ap = 45.00
           rt_target:height-chars      in frame f_rpt_41_compl_impto_retid_ap = 03.00.
    /* set return-inserted = yes for editors */
    assign ed_1x40:return-inserted in frame f_rpt_41_compl_impto_retid_ap = yes.
    /* set private-data for the help system */
    assign v_cod_pais_faixa_inic:private-data            in frame f_rpt_41_compl_impto_retid_ap = "HLP=000022145":U
           v_cod_pais_faixa_fim:private-data             in frame f_rpt_41_compl_impto_retid_ap = "HLP=000022146":U
           v_cod_unid_federac_faixa_inic:private-data    in frame f_rpt_41_compl_impto_retid_ap = "HLP=000022147":U
           v_cod_unid_federac_faixa_fim:private-data     in frame f_rpt_41_compl_impto_retid_ap = "HLP=000022148":U
           v_cod_impto_faixa_inic:private-data           in frame f_rpt_41_compl_impto_retid_ap = "HLP=000022143":U
           v_cod_impto_faixa_fim:private-data            in frame f_rpt_41_compl_impto_retid_ap = "HLP=000022144":U
           v_cod_classif_impto_faixa_inic:private-data   in frame f_rpt_41_compl_impto_retid_ap = "HLP=000022142":U
           v_cod_classif_impto_faixa_fim:private-data    in frame f_rpt_41_compl_impto_retid_ap = "HLP=000022141":U
           v_dat_vencto_docto_inic:private-data          in frame f_rpt_41_compl_impto_retid_ap = "HLP=000016631":U
           v_dat_vencto_docto_fim:private-data           in frame f_rpt_41_compl_impto_retid_ap = "HLP=000016632":U
           v_cod_indic_econ_ini:private-data             in frame f_rpt_41_compl_impto_retid_ap = "HLP=000018872":U
           v_cod_indic_econ_fim:private-data             in frame f_rpt_41_compl_impto_retid_ap = "HLP=000018873":U
           v_cdn_fornecedor_ini:private-data             in frame f_rpt_41_compl_impto_retid_ap = "HLP=000018870":U
           v_cdn_fornecedor_fim:private-data             in frame f_rpt_41_compl_impto_retid_ap = "HLP=000018871":U
           v_log_mostra_impto_retid:private-data         in frame f_rpt_41_compl_impto_retid_ap = "HLP=000022097":U
           v_log_mostra_impto_taxado:private-data        in frame f_rpt_41_compl_impto_retid_ap = "HLP=000022392":U
           v_log_impto_sem_sdo:private-data              in frame f_rpt_41_compl_impto_retid_ap = "HLP=000018824":U
           v_log_recolh:private-data                     in frame f_rpt_41_compl_impto_retid_ap = "HLP=000018824":U
           v_ind_classif_rpt_impto_retid_ap:private-data in frame f_rpt_41_compl_impto_retid_ap = "HLP=000018824":U
           v_log_tot_classif_impto:private-data          in frame f_rpt_41_compl_impto_retid_ap = "HLP=000018824":U
           v_log_tot_fornec_orig:private-data            in frame f_rpt_41_compl_impto_retid_ap = "HLP=000018824":U
           v_log_tot_fornec_dest:private-data            in frame f_rpt_41_compl_impto_retid_ap = "HLP=000018824":U
           v_log_por_estab:private-data                  in frame f_rpt_41_compl_impto_retid_ap = "HLP=000018824":U
           v_log_quebra_fornec:private-data              in frame f_rpt_41_compl_impto_retid_ap = "HLP=000018824":U
           rs_cod_dwb_output:private-data                in frame f_rpt_41_compl_impto_retid_ap = "HLP=000018824":U
           ed_1x40:private-data                          in frame f_rpt_41_compl_impto_retid_ap = "HLP=000018824":U
           bt_get_file:private-data                      in frame f_rpt_41_compl_impto_retid_ap = "HLP=000008782":U
           bt_set_printer:private-data                   in frame f_rpt_41_compl_impto_retid_ap = "HLP=000008785":U
           rs_ind_run_mode:private-data                  in frame f_rpt_41_compl_impto_retid_ap = "HLP=000018824":U
           v_log_print_par:private-data                  in frame f_rpt_41_compl_impto_retid_ap = "HLP=000024662":U
           v_qtd_line:private-data                       in frame f_rpt_41_compl_impto_retid_ap = "HLP=000024737":U
           v_qtd_column:private-data                     in frame f_rpt_41_compl_impto_retid_ap = "HLP=000024669":U
           bt_close:private-data                         in frame f_rpt_41_compl_impto_retid_ap = "HLP=000009420":U
           bt_print:private-data                         in frame f_rpt_41_compl_impto_retid_ap = "HLP=000010815":U
           bt_can:private-data                           in frame f_rpt_41_compl_impto_retid_ap = "HLP=000011050":U
           bt_hel2:private-data                          in frame f_rpt_41_compl_impto_retid_ap = "HLP=000011326":U
           frame f_rpt_41_compl_impto_retid_ap:private-data                                     = "HLP=000018824".



{include/i_fclfrm.i f_rpt_41_compl_impto_retid_ap }
/*************************** Frame Definition End ***************************/

/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
def var v_prog_filtro_pdf as handle no-undo.

function getCodTipoRelat returns character in v_prog_filtro_pdf.

run prgtec/btb/btb920aa.py persistent set v_prog_filtro_pdf.

run pi_define_objetos in v_prog_filtro_pdf (frame f_rpt_41_compl_impto_retid_ap:handle,
                       rs_cod_dwb_output:handle in frame f_rpt_41_compl_impto_retid_ap,
                       bt_get_file:row in frame f_rpt_41_compl_impto_retid_ap,
                       bt_get_file:col in frame f_rpt_41_compl_impto_retid_ap).

&endif
/* tech38629 - Fim da alteraá∆o */


/*********************** User Interface Trigger Begin ***********************/


ON CHOOSE OF bt_get_file IN FRAME f_rpt_41_compl_impto_retid_ap
DO:

    system-dialog get-file v_cod_dwb_file
        title "Imprimir" /*l_imprimir*/ 
        filters '*.rpt' '*.rpt',
                "*.*"   "*.*"
        save-as
        create-test-file
        ask-overwrite.
        assign dwb_rpt_param.cod_dwb_file             = v_cod_dwb_file
               ed_1x40:screen-value in frame f_rpt_41_compl_impto_retid_ap = v_cod_dwb_file.

END. /* ON CHOOSE OF bt_get_file IN FRAME f_rpt_41_compl_impto_retid_ap */

ON CHOOSE OF bt_hel2 IN FRAME f_rpt_41_compl_impto_retid_ap
DO:


    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.


    /* End_Include: i_context_help_frame */

END. /* ON CHOOSE OF bt_hel2 IN FRAME f_rpt_41_compl_impto_retid_ap */

ON CHOOSE OF bt_print IN FRAME f_rpt_41_compl_impto_retid_ap
DO:

    if  input frame f_rpt_41_compl_impto_retid_ap v_log_mostra_impto_retid = no and
        input frame f_rpt_41_compl_impto_retid_ap v_log_mostra_impto_taxado = no
    then do:
        /* &1 deve ser informado(a). */
        run pi_messages (input "show",
                         input 9612,
                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                           "Imposto" /*l_imposto*/)) /*msg_9612*/.
        return no-apply.
    end.

do:
/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
    run pi_restricoes in v_prog_filtro_pdf (input rs_cod_dwb_output:screen-value in frame f_rpt_41_compl_impto_retid_ap).
    if return-value = 'nok' then 
        return no-apply.
&endif
/* tech38629 - Fim da alteraá∆o */
    assign v_log_print = yes.
end.
END. /* ON CHOOSE OF bt_print IN FRAME f_rpt_41_compl_impto_retid_ap */

ON CHOOSE OF bt_set_printer IN FRAME f_rpt_41_compl_impto_retid_ap
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
               ed_1x40:screen-value in frame f_rpt_41_compl_impto_retid_ap = v_nom_dwb_printer
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
                with frame f_rpt_41_compl_impto_retid_ap.
    end /* if */.

END. /* ON CHOOSE OF bt_set_printer IN FRAME f_rpt_41_compl_impto_retid_ap */

ON LEAVE OF ed_1x40 IN FRAME f_rpt_41_compl_impto_retid_ap
DO:

    /************************* Variable Definition Begin ************************/

    def var v_cod_filename_final             as character       no-undo. /*local*/
    def var v_cod_filename_initial           as character       no-undo. /*local*/


    /************************** Variable Definition End *************************/

    block:
    do with frame f_rpt_41_compl_impto_retid_ap:
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

            find dwb_rpt_param
                where dwb_rpt_param.cod_dwb_user    = v_cod_usuar_corren
                and   dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
                exclusive-lock no-error.
            assign dwb_rpt_param.cod_dwb_file = ed_1x40:screen-value.
        end /* if */.
    end /* do block */.

END. /* ON LEAVE OF ed_1x40 IN FRAME f_rpt_41_compl_impto_retid_ap */

ON VALUE-CHANGED OF rs_cod_dwb_output IN FRAME f_rpt_41_compl_impto_retid_ap
DO:

    initout:
    do with frame f_rpt_41_compl_impto_retid_ap:
        /* block: */
        case self:screen-value:
            when "Terminal" /*l_terminal*/ then ter:
             do:
                if  rs_cod_dwb_output <> "Impressora" /*l_printer*/ 
                then do:
                    assign v_qtd_line_ant = input frame f_rpt_41_compl_impto_retid_ap v_qtd_line.
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
                        with frame f_rpt_41_compl_impto_retid_ap.
                assign ed_1x40:screen-value   = ""
                       ed_1x40:sensitive      = no
                       bt_get_file:visible    = no
                       bt_set_printer:visible = no.
            end /* do ter */.
            when "Arquivo" /*l_file*/ then fil:
             do:
                if  rs_cod_dwb_output <> "Impressora" /*l_printer*/ 
                then do:
                    assign v_qtd_line_ant = input frame f_rpt_41_compl_impto_retid_ap v_qtd_line.
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
                        with frame f_rpt_41_compl_impto_retid_ap.
                assign ed_1x40:screen-value   = ""
                       ed_1x40:sensitive      = yes
                       bt_set_printer:visible = no
                       bt_get_file:visible    = yes.

                /* define arquivo default */
                find usuar_mestre no-lock
                     where usuar_mestre.cod_usuario = v_cod_dwb_user
    &if "{&emsbas_version}" >= "5.01" &then
                     use-index srmstr_id
    &endif
                      /*cl_current_user of usuar_mestre*/ no-error.
                do  transaction:                
                    find dwb_rpt_param
                        where dwb_rpt_param.cod_dwb_user    = v_cod_usuar_corren
                        and   dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
                        exclusive-lock no-error.

                    assign dwb_rpt_param.cod_dwb_file = "".

                    if  rs_ind_run_mode:screen-value in frame f_rpt_41_compl_impto_retid_ap <> "Batch" /*l_batch*/ 
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
                                                          + caps("apb318aa":U)
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
                    assign v_qtd_line_ant = input frame f_rpt_41_compl_impto_retid_ap v_qtd_line.
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
                        with frame f_rpt_41_compl_impto_retid_ap.
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
                with frame f_rpt_41_compl_impto_retid_ap.
    end /* if */.
    else do:
        enable v_qtd_line
               with frame f_rpt_41_compl_impto_retid_ap.
    end /* else */.
    assign rs_cod_dwb_output.

END. /* ON VALUE-CHANGED OF rs_cod_dwb_output IN FRAME f_rpt_41_compl_impto_retid_ap */

ON VALUE-CHANGED OF rs_ind_run_mode IN FRAME f_rpt_41_compl_impto_retid_ap
DO:

    do  transaction:
        find dwb_rpt_param
            where dwb_rpt_param.cod_dwb_user    = v_cod_usuar_corren
            and   dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
            exclusive-lock no-error.
        assign dwb_rpt_param.ind_dwb_run_mode = input frame f_rpt_41_compl_impto_retid_ap rs_ind_run_mode.

        if  dwb_rpt_param.ind_dwb_run_mode = "Batch" /*l_batch*/ 
        then do:
            if  rs_cod_dwb_output:disable("Terminal" /*l_terminal*/ ) in frame f_rpt_41_compl_impto_retid_ap
            then do:
            end /* if */.
        end /* if */.
        else do:
            if  rs_cod_dwb_output:enable("Terminal" /*l_terminal*/ ) in frame f_rpt_41_compl_impto_retid_ap
            then do:
            end /* if */.
        end /* else */.
        if  rs_ind_run_mode = "Batch" /*l_batch*/ 
        then do:
           assign v_qtd_line = v_qtd_line_ant.
           display v_qtd_line
                   with frame f_rpt_41_compl_impto_retid_ap.
        end /* if */.
        assign rs_ind_run_mode.
        apply "value-changed" to rs_cod_dwb_output in frame f_rpt_41_compl_impto_retid_ap.
    end.    

END. /* ON VALUE-CHANGED OF rs_ind_run_mode IN FRAME f_rpt_41_compl_impto_retid_ap */

ON VALUE-CHANGED OF v_ind_classif_rpt_impto_retid_ap IN FRAME f_rpt_41_compl_impto_retid_ap
DO:

    if input frame f_rpt_41_compl_impto_retid_ap v_ind_classif_rpt_impto_retid_ap = ("Por Fornecedor Origem/Por Estab" /*l_por_fornecedor_origempor_estab*/ ) then do:
        enable v_log_quebra_fornec with frame f_rpt_41_compl_impto_retid_ap.
        assign v_log_quebra_fornec = yes.
        disp   v_log_quebra_fornec with frame f_rpt_41_compl_impto_retid_ap.

        disable v_log_por_estab with frame f_rpt_41_compl_impto_retid_ap.
        assign  v_log_por_estab = no.
        disp    v_log_por_estab with frame f_rpt_41_compl_impto_retid_ap.
    end.
    else do:
        disable v_log_quebra_fornec with frame f_rpt_41_compl_impto_retid_ap.
        assign v_log_quebra_fornec = no.
        disp   v_log_quebra_fornec with frame f_rpt_41_compl_impto_retid_ap.

        enable v_log_por_estab with frame f_rpt_41_compl_impto_retid_ap.
    end.

END. /* ON VALUE-CHANGED OF v_ind_classif_rpt_impto_retid_ap IN FRAME f_rpt_41_compl_impto_retid_ap */


/************************ User Interface Trigger End ************************/

/**************************** Frame Trigger Begin ***************************/


ON ENDKEY OF FRAME f_rpt_41_compl_impto_retid_ap
DO:


    /* Begin_Include: i_exec_program_epc */
    &if '{&emsbas_version}' > '1.00' &then
    if  v_nom_prog_upc <> '' then
    do:
        assign v_rec_table_epc = recid(compl_impto_retid_ap).    
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
        assign v_rec_table_epc = recid(compl_impto_retid_ap).    
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
        assign v_rec_table_epc = recid(compl_impto_retid_ap).    
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

END. /* ON ENDKEY OF FRAME f_rpt_41_compl_impto_retid_ap */

ON GO OF FRAME f_rpt_41_compl_impto_retid_ap
DO:

    do transaction:
        find dwb_rpt_param
            where dwb_rpt_param.cod_dwb_user    = v_cod_usuar_corren
            and   dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
            exclusive-lock no-error.
        assign dwb_rpt_param.cod_dwb_output     = rs_cod_dwb_output:screen-value in frame f_rpt_41_compl_impto_retid_ap
               dwb_rpt_param.qtd_dwb_line       = input frame f_rpt_41_compl_impto_retid_ap v_qtd_line
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
    end.    

END. /* ON GO OF FRAME f_rpt_41_compl_impto_retid_ap */

ON HELP OF FRAME f_rpt_41_compl_impto_retid_ap ANYWHERE
DO:


    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */

END. /* ON HELP OF FRAME f_rpt_41_compl_impto_retid_ap */

ON RIGHT-MOUSE-DOWN OF FRAME f_rpt_41_compl_impto_retid_ap ANYWHERE
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

END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_rpt_41_compl_impto_retid_ap */

ON RIGHT-MOUSE-UP OF FRAME f_rpt_41_compl_impto_retid_ap ANYWHERE
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

END. /* ON RIGHT-MOUSE-UP OF FRAME f_rpt_41_compl_impto_retid_ap */

ON WINDOW-CLOSE OF FRAME f_rpt_41_compl_impto_retid_ap
DO:

    apply "end-error" to self.
END. /* ON WINDOW-CLOSE OF FRAME f_rpt_41_compl_impto_retid_ap */


/***************************** Frame Trigger End ****************************/

/**************************** Menu Trigger Begin ****************************/


ON CHOOSE OF MENU-ITEM mi_conteudo IN MENU m_help
DO:


        apply "choose" to bt_hel2 in frame f_rpt_41_compl_impto_retid_ap.





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


        assign v_nom_prog     = substring(frame f_rpt_41_compl_impto_retid_ap:title, 1, max(1, length(frame f_rpt_41_compl_impto_retid_ap:title) - 10)).
        if  v_nom_prog = ? then
            assign v_nom_prog = "".

        assign v_nom_prog     = v_nom_prog
                              + chr(10)
                              + "rpt_compl_impto_retid_ap":U.




    assign v_nom_prog_ext = "prgfin/apb/apb318aa.py":U
           v_cod_release  = trim(" 1.00.00.034":U).
/*    run prgtec/btb/btb901zb.p (Input v_nom_prog,
                               Input v_nom_prog_ext,
                               Input v_cod_release) /*prg_fnc_about*/. */
{include/sobre5.i}
END. /* ON CHOOSE OF MENU-ITEM mi_sobre IN MENU m_help */


/***************************** Menu Trigger End *****************************/


/****************************** Main Code Begin *****************************/


/* Begin_Include: i_version_extract */
{include/i-ctrlrp5.i rpt_compl_impto_retid_ap}


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
    run pi_version_extract ('rpt_compl_impto_retid_ap':U, 'prgfin/apb/apb318aa.py':U, '1.00.00.034':U, 'pro':U).
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
    run prgtec/men/men901za.py (Input 'rpt_compl_impto_retid_ap') /*prg_fnc_verify_security*/.
if  return-value = "2014"
then do:
    /* Programa a ser executado n∆o Ç um programa v†lido Datasul ! */
    run pi_messages (input "show",
                     input 2014,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'rpt_compl_impto_retid_ap')) /*msg_2014*/.
    return.
end /* if */.
if  return-value = "2012"
then do:
    /* Usu†rio sem permiss∆o para acessar o programa. */
    run pi_messages (input "show",
                     input 2012,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'rpt_compl_impto_retid_ap')) /*msg_2012*/.
    return.
end /* if */.
/* End_Include: i_verify_security */



/* Begin_Include: i_log_exec_prog_dtsul_ini */
assign v_rec_log = ?.

if can-find(prog_dtsul
       where prog_dtsul.cod_prog_dtsul = 'rpt_compl_impto_retid_ap' 
         and prog_dtsul.log_gera_log_exec = yes) then do transaction:
    create log_exec_prog_dtsul.
    assign log_exec_prog_dtsul.cod_prog_dtsul           = 'rpt_compl_impto_retid_ap'
           log_exec_prog_dtsul.cod_usuario              = v_cod_usuar_corren
           log_exec_prog_dtsul.dat_inic_exec_prog_dtsul = today
           log_exec_prog_dtsul.hra_inic_exec_prog_dtsul = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":":U,"":U).
    assign v_rec_log = recid(log_exec_prog_dtsul).
    release log_exec_prog_dtsul no-error.
end.


/* End_Include: i_log_exec_prog_dtsul_ini */

/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
run pi_permissoes in v_prog_filtro_pdf (input 'rpt_compl_impto_retid_ap':U).
&endif
/* tech38629 - Fim da alteraá∆o */




/* Begin_Include: i_verify_program_epc */
&if '{&emsbas_version}' > '1.00' &then
assign v_rec_table_epc = ?
       v_wgh_frame_epc = ?.

find prog_dtsul
    where prog_dtsul.cod_prog_dtsul = "rpt_compl_impto_retid_ap":U
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


assign v_wgh_frame_epc = frame f_rpt_41_compl_impto_retid_ap:handle.



assign v_nom_table_epc = 'compl_impto_retid_ap':U
       v_rec_table_epc = recid(compl_impto_retid_ap).

&endif

/* End_Include: i_verify_program_epc */


/* redefiniá‰es do frame */

/* Begin_Include: i_std_dialog_box */
/* tratamento do titulo e vers∆o */
assign frame f_rpt_41_compl_impto_retid_ap:title = frame f_rpt_41_compl_impto_retid_ap:title
                            + chr(32)
                            + chr(40)
                            + trim(" 1.00.00.034":U)
                            + chr(41).
/* menu pop-up de ajuda e sobre */
assign menu m_help:popup-only = yes
       bt_hel2:popup-menu in frame f_rpt_41_compl_impto_retid_ap = menu m_help:handle.


/* End_Include: i_std_dialog_box */
{include/title5.i f_rpt_41_compl_impto_retid_ap FRAME}


/* inicializa vari†veis */
find empresa no-lock
     where empresa.cod_empresa = v_cod_empres_usuar /*cl_empres_usuar of empresa*/ no-error.
find dwb_rpt_param
     where dwb_rpt_param.cod_dwb_program = "rel_impto_retid_recolh_apb":U
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
assign v_cod_dwb_proced   = "rel_impto_retid_recolh_apb":U
       v_cod_dwb_program  = "rel_impto_retid_recolh_apb":U
       v_cod_release      = trim(" 1.00.00.034":U)
       v_ind_dwb_run_mode = "On-Line" /*l_online*/ 
       v_qtd_column       = v_rpt_s_1_columns
       v_qtd_bottom       = v_rpt_s_1_bottom.
if (avail empresa) then
    assign v_nom_enterprise   = empresa.nom_razao_social.
else
    assign v_nom_enterprise   = 'DATASUL'.


/* Begin_Include: ix_p00_rpt_compl_impto_retid_ap */

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

        output stream s-arq to value(v_cod_arq) append.

        put stream s-arq unformatted
            SPP      at 1 
            v_log_retorno  at 43 skip.

        output stream s-arq close.

    end /* if */.
    /* End_Include: i_funcao_extract */
    .

    RETURN v_log_retorno.
END FUNCTION.
/* End_Include: i_declara_GetDefinedFunction */



/* Begin_Include: i_declara_GetEntryField */
FUNCTION GetEntryField RETURNS CHARACTER (input p_num_posicao     AS INTEGER,
                                          INPUT p_cod_campo       AS CHARACTER,
                                          input p_cod_separador   AS CHARACTER):

/* ************* Parametros da FUNÄ«O *******************************
** Funá∆o para tratamento dos Entries dos c¢digos livres
** 
**  p_num_posicao     - N£mero do Entry que ser† atualizado
**  p_cod_campo       - Campo / Vari†vel que ser† atualizada
**  p_cod_separador   - Separador que ser† utilizado
*******************************************************************/

    if  p_num_posicao <= 0  then do:
        assign p_num_posicao  = 1.
    end.
    if num-entries(p_cod_campo,p_cod_separador) >= p_num_posicao  then do:
       return entry(p_num_posicao,p_cod_campo,p_cod_separador).
    end.
    return "" /*l_*/ .

END FUNCTION.

/* End_Include: i_declara_GetEntryField */


ASSIGN v_log_faixa_fornec_orig = &IF DEFINED(BF_FIN_FAIXA_FORNEC_ORIGEM) &THEN YES &ELSE GetDefinedFunction('SPP_FAIXA_FORNEC_ORIGEM') &ENDIF.
if not v_cod_dwb_user begins 'es_' then do:
    run prgfin/apb/apb894aa.p /*prg_spp_compl_impto_retid_ap_dt*/.
end.    
/* End_Include: i_declara_GetEntryField */


if  v_cod_dwb_user begins 'es_'
then do:
    find dwb_rpt_param no-lock
         where dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
           and dwb_rpt_param.cod_dwb_user = v_cod_dwb_user /*cl_dwb_rpt_param of dwb_rpt_param*/ no-error.
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
    if (ped_exec.cod_release_prog_dtsul <> trim(" 1.00.00.034":U)) then
        return "Vers‰es do programa diferente." /*1994*/ + " (" + "1994" + ")" + chr(10)
                                     + substitute("A vers∆o do programa (&3) que gerou o pedido de execuá∆o batch (&1) Ç diferente da vers∆o do programa que deveria executar o pedido batch (&2)." /*1994*/,ped_exec.cod_release_prog_dtsul,
                                                  trim(" 1.00.00.034":U),
                                                  "prgfin/apb/apb318aa.py":U).
    assign v_nom_prog_ext     = caps("apb318aa":U)
           v_dat_execution    = today
           v_hra_execution    = replace(string(time, "hh:mm:ss" /*l_hh:mm:ss*/ ), ":", "")
           v_cod_dwb_file     = dwb_rpt_param.cod_dwb_file
           v_nom_report_title = fill(" ", 40 - length(v_rpt_s_1_name)) + v_rpt_s_1_name
           v_ind_dwb_run_mode = "Batch" /*l_batch*/ .


    /* Begin_Include: ix_p02_rpt_compl_impto_retid_ap */
    if  num-entries( dwb_rpt_param.cod_dwb_parameters , chr(10) ) >= 22
    then do:
        assign v_cod_pais_faixa_inic            = entry(1, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_cod_pais_faixa_fim             = entry(2, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_cod_unid_federac_faixa_inic    = entry(3, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_cod_unid_federac_faixa_fim     = entry(4, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_cod_impto_faixa_inic           = entry(5, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_cod_impto_faixa_fim            = entry(6, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_cod_classif_impto_faixa_inic   = entry(7, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_cod_classif_impto_faixa_fim    = entry(8, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_dat_vencto_docto_inic          = date(entry(9, dwb_rpt_param.cod_dwb_parameters, chr(10)))
               v_dat_vencto_docto_fim           = date(entry(10, dwb_rpt_param.cod_dwb_parameters, chr(10)))
               v_cod_indic_econ_ini             = entry(11, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_cod_indic_econ_fim             = entry(12, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_log_mostra_impto_retid         = if  entry(13, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" /*l_yes*/  then
                                                      yes
                                                  else
                                                      no
               v_log_mostra_impto_taxado        = if  entry(14, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" /*l_yes*/  then
                                                      yes
                                                  else
                                                      no
               v_log_tot_classif_impto          = if  entry(15, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" /*l_yes*/  then
                                                      yes
                                                  else
                                                      no
               v_log_tot_fornec_orig            = if  entry(16, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" /*l_yes*/  then
                                                      yes
                                                  else
                                                      no
               v_log_tot_fornec_dest            = if  entry(17, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" /*l_yes*/  then
                                                      yes
                                                  else
                                                      no
               v_log_por_estab                  = if  entry(18, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" /*l_yes*/  then
                                                      yes
                                                  else
                                                      no
               v_ind_classif_rpt_impto_retid_ap = entry(19, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_log_impto_sem_sdo              = if  entry(20, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" /*l_yes*/  then
                                                      yes
                                                  else
                                                      no
               v_log_quebra_fornec              = if  entry(21, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" /*l_yes*/  then
                                                      yes
                                                  else
                                                      no
               v_log_recolh                     = if  entry(22, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" /*l_yes*/  then
                                                     yes
                                                  else
                                                     no.

        if num-entries( dwb_rpt_param.cod_dwb_parameters , chr(10) ) >= 24 then 
            assign v_cdn_fornecedor_ini = INT(entry(23, dwb_rpt_param.cod_dwb_parameters, chr(10)))
                   v_cdn_fornecedor_fim = INT(entry(24, dwb_rpt_param.cod_dwb_parameters, chr(10))).
        else
            assign v_cdn_fornecedor_ini = 0
                   v_cdn_fornecedor_fim = 999999999.


    end /* if */.
    /* End_Include: ix_p02_rpt_compl_impto_retid_ap */


    /* configura e define destino de impress∆o */
    if (dwb_rpt_param.cod_dwb_output = "Impressora" /*l_printer*/ ) then
        assign v_qtd_line_ant = v_qtd_line.

    run pi_output_reports /*pi_output_reports*/.

    if  dwb_rpt_param.log_dwb_print_parameters = yes
    then do:
        if (page-number (s_1) > 0) then
            page stream s_1.

        /* ix_p29_rpt_compl_impto_retid_ap */

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


        /* Begin_Include: ix_p30_rpt_compl_impto_retid_ap */
        if (line-counter(s_1) + 15) > v_rpt_s_1_bottom then
            page stream s_1.
        put stream s_1 unformatted 
            skip (1)
            "   Classificaá∆o: " at 37
            v_ind_classif_rpt_impto_retid_ap at 55 format "X(40)" skip
            "Totaliza Classificaá∆o do Imposto:" at 20
            v_log_tot_classif_impto at 55 format "Sim/N∆o" skip
            "Totaliza Fornecedor Origem:" at 27
            v_log_tot_fornec_orig at 55 format "Sim/N∆o" skip.
        put stream s_1 unformatted 
            "Totaliza Fornecedor Favorecido:" at 23
            v_log_tot_fornec_dest at 55 format "Sim/N∆o" skip
            "   Imposto Retido: " at 36
            v_log_mostra_impto_retid at 55 format "Sim/N∆o" skip
            "   Imposto Taxado: " at 36
            v_log_mostra_impto_taxado at 55 format "Sim/N∆o" skip
            "    Imposto sem Saldo: " at 32
            v_log_impto_sem_sdo at 55 format "Sim/N∆o" skip
            "    Apenas a Recolher: " at 32
            v_log_recolh at 55 format "Sim/N∆o" skip.
        put stream s_1 unformatted 
            "  Pa°s: " at 47
            v_cod_pais_faixa_inic at 55 format "x(03)"
            "atÇ: " at 72
            v_cod_pais_faixa_fim at 77 format "x(3)" skip
            "  Unidade Federaá∆o: " at 34
            v_cod_unid_federac_faixa_inic at 55 format "x(3)"
            "atÇ: " at 72
            v_cod_unid_federac_faixa_fim at 77 format "x(3)" skip
            "  Imposto: " at 44
            v_cod_impto_faixa_inic at 55 format "x(5)".
        put stream s_1 unformatted 
            "atÇ: " at 72
            v_cod_impto_faixa_fim at 77 format "x(5)" skip
            "Classificaá∆o: " at 40
            v_cod_classif_impto_faixa_inic at 55 format "x(5)"
            "atÇ: " at 72
            v_cod_classif_impto_faixa_fim at 77 format "x(5)" skip
            "  Vencimento: " at 41
            v_dat_vencto_docto_inic at 55 format "99/99/9999"
            "atÇ: " at 72
            v_dat_vencto_docto_fim at 77 format "99/99/9999" skip.
        put stream s_1 unformatted 
            "  Moeda: " at 46
            v_cod_indic_econ_ini at 55 format "x(8)"
            "atÇ: " at 72
            v_cod_indic_econ_fim at 77 format "x(8)" skip.

        IF (v_log_faixa_fornec_orig = YES) THEN do:
            if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                page stream s_1.
            put stream s_1 unformatted 
                "Fornec Origem:" at 40
                v_cdn_fornecedor_ini to 64 format ">>>,>>>,>>9"
                "atÇ: " at 72
                v_cdn_fornecedor_fim to 87 format ">>>,>>>,>>9" skip.
        end.    

        /* End_Include: ix_p30_rpt_compl_impto_retid_ap */


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
view frame f_rpt_41_compl_impto_retid_ap.

/* Begin_Include: i_exec_program_epc */
&if '{&emsbas_version}' > '1.00' &then
if  v_nom_prog_upc <> '' then
do:
    assign v_rec_table_epc = recid(compl_impto_retid_ap).    
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
    assign v_rec_table_epc = recid(compl_impto_retid_ap).    
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
    assign v_rec_table_epc = recid(compl_impto_retid_ap).    
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


super_block:
repeat
    on stop undo super_block, retry super_block:

    if (retry) then
       output stream s_1 close.

    param_block:
    do transaction:

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
                   dwb_rpt_param.ind_dwb_run_mode        = "On-Line" /*l_online*/ 
                   dwb_rpt_param.cod_dwb_file            = ""
                   dwb_rpt_param.nom_dwb_printer         = ""
                   dwb_rpt_param.cod_dwb_print_layout    = ""
                   v_cod_dwb_file_temp                   = "".
        end /* if */.
        assign v_qtd_line = (if dwb_rpt_param.qtd_dwb_line <> 0 then dwb_rpt_param.qtd_dwb_line else v_rpt_s_1_lines).
    end /* do param_block */.

    init:
    do with frame f_rpt_41_compl_impto_retid_ap:
        assign rs_cod_dwb_output:screen-value   = dwb_rpt_param.cod_dwb_output
               rs_ind_run_mode:screen-value     = dwb_rpt_param.ind_dwb_run_mode.

        if  dwb_rpt_param.cod_dwb_output = "Arquivo" /*l_file*/ 
        then do:
            assign v_cod_dwb_file_temp = replace(dwb_rpt_param.cod_dwb_file, "~\", "~/").
            if (index(v_cod_dwb_file_temp, "~/") <> 0) then
                assign v_cod_dwb_file_temp = substring(v_cod_dwb_file_temp, r-index(v_cod_dwb_file_temp, "~/") + 1).
            else
                assign v_cod_dwb_file_temp = dwb_rpt_param.cod_dwb_file.
            assign ed_1x40:screen-value = v_cod_dwb_file_temp.
        end /* if */.

        if  dwb_rpt_param.cod_dwb_output = "Impressora" /*l_printer*/ 
        then do:
            if (not can-find(imprsor_usuar
                            where imprsor_usuar.nom_impressora = dwb_rpt_param.nom_dwb_printer
                              and imprsor_usuar.cod_usuario = dwb_rpt_param.cod_dwb_user
&if "{&emsbas_version}" >= "5.01" &then
                            use-index imprsrsr_id
&endif
                             /*cl_get_printer of imprsor_usuar*/)
            or   not can-find(layout_impres
                             where layout_impres.nom_impressora = dwb_rpt_param.nom_dwb_printer
                               and layout_impres.cod_layout_impres = dwb_rpt_param.cod_dwb_print_layout /*cl_get_layout of layout_impres*/)) then
                run pi_set_print_layout_default /*pi_set_print_layout_default*/.
            assign ed_1x40:screen-value = dwb_rpt_param.nom_dwb_printer
                                        + ":"
                                        + dwb_rpt_param.cod_dwb_print_layout.
        end /* if */.
        assign v_log_print_par = dwb_rpt_param.log_dwb_print_parameters.
        display v_log_print_par
                with frame f_rpt_41_compl_impto_retid_ap.
    end /* do init */.

    display v_qtd_column
            v_qtd_line
            with frame f_rpt_41_compl_impto_retid_ap.


    /* Begin_Include: i_exec_program_epc */
    &if '{&emsbas_version}' > '1.00' &then
    if  v_nom_prog_upc <> '' then
    do:
        assign v_rec_table_epc = recid(compl_impto_retid_ap).    
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
        assign v_rec_table_epc = recid(compl_impto_retid_ap).    
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
        assign v_rec_table_epc = recid(compl_impto_retid_ap).    
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


    enable rs_cod_dwb_output
           v_log_print_par
           bt_get_file
           bt_set_printer
           bt_close
           bt_print
           bt_can
           bt_hel2
           with frame f_rpt_41_compl_impto_retid_ap.


    /* Begin_Include: i_exec_program_epc */
    &if '{&emsbas_version}' > '1.00' &then
    if  v_nom_prog_upc <> '' then
    do:
        assign v_rec_table_epc = recid(compl_impto_retid_ap).    
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
        assign v_rec_table_epc = recid(compl_impto_retid_ap).    
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
        assign v_rec_table_epc = recid(compl_impto_retid_ap).    
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


/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
    run pi_posiciona_dwb_rpt_param in v_prog_filtro_pdf (input rowid(dwb_rpt_param)).
    run pi_load_params in v_prog_filtro_pdf.
&endif
/* tech38629 - Fim da alteraá∆o */



    apply "value-changed" to rs_cod_dwb_output in frame f_rpt_41_compl_impto_retid_ap.


    if  yes = yes
    then do:
       enable rs_ind_run_mode
              with frame f_rpt_41_compl_impto_retid_ap.
       apply "value-changed" to rs_ind_run_mode in frame f_rpt_41_compl_impto_retid_ap.
    end /* if */.



    /* Begin_Include: ix_p10_rpt_compl_impto_retid_ap */
    enable v_cod_classif_impto_faixa_fim
           v_cod_classif_impto_faixa_inic
           v_cod_impto_faixa_fim
           v_cod_impto_faixa_inic
           v_cod_indic_econ_fim
           v_cod_indic_econ_ini
           v_cod_pais_faixa_fim
           v_cod_pais_faixa_inic
           v_cod_unid_federac_faixa_fim
           v_cod_unid_federac_faixa_inic
           v_dat_vencto_docto_fim
           v_dat_vencto_docto_inic
           v_log_mostra_impto_retid
           v_log_mostra_impto_taxado
           v_log_impto_sem_sdo
           v_log_tot_fornec_dest
           v_log_tot_fornec_orig
           v_log_tot_classif_impto
           v_log_por_estab
           v_ind_classif_rpt_impto_retid_ap
           v_log_quebra_fornec
           v_log_recolh
           with frame f_rpt_41_compl_impto_retid_ap.
    if  num-entries( dwb_rpt_param.cod_dwb_parameters , chr(10) ) >= 22
    then do:
        assign v_cod_pais_faixa_inic            = entry(1, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_cod_pais_faixa_fim             = entry(2, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_cod_unid_federac_faixa_inic    = entry(3, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_cod_unid_federac_faixa_fim     = entry(4, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_cod_impto_faixa_inic           = entry(5, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_cod_impto_faixa_fim            = entry(6, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_cod_classif_impto_faixa_inic   = entry(7, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_cod_classif_impto_faixa_fim    = entry(8, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_dat_vencto_docto_inic          = date(entry(9, dwb_rpt_param.cod_dwb_parameters, chr(10)))
               v_dat_vencto_docto_fim           = date(entry(10, dwb_rpt_param.cod_dwb_parameters, chr(10)))
               v_cod_indic_econ_ini             = entry(11, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_cod_indic_econ_fim             = entry(12, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_log_mostra_impto_retid         = if  entry(13, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" /*l_yes*/  then
                                                     yes
                                                  else
                                                     no
               v_log_mostra_impto_taxado        = if  entry(14, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" /*l_yes*/  then
                                                     yes
                                                  else
                                                     no
               v_log_tot_classif_impto          = if  entry(15, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" /*l_yes*/  then
                                                     yes
                                                  else
                                                     no
               v_log_tot_fornec_orig            = if  entry(16, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" /*l_yes*/  then
                                                     yes
                                                  else
                                                     no
               v_log_tot_fornec_dest            = if  entry(17, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" /*l_yes*/  then
                                                     yes
                                                  else
                                                     no
               v_log_por_estab                  = if  entry(18, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" /*l_yes*/  then
                                                     yes
                                                  else
                                                     no
               v_ind_classif_rpt_impto_retid_ap = entry(19, dwb_rpt_param.cod_dwb_parameters, chr(10))
               v_log_impto_sem_sdo              = if  entry(20, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" /*l_yes*/  then
                                                     yes
                                                  else
                                                     no
               v_log_quebra_fornec              = if  entry(21, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" /*l_yes*/  then
                                                     yes
                                                  else
                                                     no
               v_log_recolh                     = if  entry(22, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" /*l_yes*/  then
                                                     yes
                                                  else
                                                     no.

        if num-entries( dwb_rpt_param.cod_dwb_parameters , chr(10) ) > 22 then
            assign v_cdn_fornecedor_ini = INT(entry(23, dwb_rpt_param.cod_dwb_parameters, chr(10)))
                   v_cdn_fornecedor_fim = INT(entry(24, dwb_rpt_param.cod_dwb_parameters, chr(10))).
    end /* if */.
    display v_cod_classif_impto_faixa_fim
            v_cod_classif_impto_faixa_inic
            v_cod_impto_faixa_fim
            v_cod_impto_faixa_inic
            v_cod_indic_econ_fim
            v_cod_indic_econ_ini
            v_cod_pais_faixa_fim
            v_cod_pais_faixa_inic
            v_cod_unid_federac_faixa_fim
            v_cod_unid_federac_faixa_inic
            v_dat_vencto_docto_fim
            v_dat_vencto_docto_inic
            v_log_mostra_impto_retid
            v_log_mostra_impto_taxado
            v_log_impto_sem_sdo
            v_ind_classif_rpt_impto_retid_ap
            v_log_tot_fornec_dest
            v_log_tot_fornec_orig
            v_log_tot_classif_impto
            v_log_por_estab
            v_log_print_par
            v_qtd_column
            v_qtd_line
            v_log_quebra_fornec
            v_log_recolh
            with frame f_rpt_41_compl_impto_retid_ap.

    apply "value-changed" to v_ind_classif_rpt_impto_retid_ap in frame f_rpt_41_compl_impto_retid_ap.

    IF (v_log_faixa_fornec_orig = YES) THEN DO:
        enable v_cdn_fornecedor_ini
               v_cdn_fornecedor_fim
               with frame f_rpt_41_compl_impto_retid_ap.
        display v_cdn_fornecedor_ini
                v_cdn_fornecedor_fim
                with frame f_rpt_41_compl_impto_retid_ap.
    END.
    else do:
        hide v_cdn_fornecedor_ini in frame f_rpt_41_compl_impto_retid_ap
             v_cdn_fornecedor_fim in frame f_rpt_41_compl_impto_retid_ap.
        assign v_cdn_fornecedor_ini = 0
               v_cdn_fornecedor_fim = 999999999.
    end.

    ASSIGN v_hdl_frame_aux = frame f_rpt_41_compl_impto_retid_ap:handle
           v_hdl_frame     = v_hdl_frame_aux:FIRST-CHILD
           v_hdl_frame     = v_hdl_frame:FIRST-CHILD.
    DO WHILE v_hdl_frame <> ? :
       if  v_hdl_frame:type <> 'field-group'  then do:  
            IF  v_hdl_frame:type = 'literal'
            and v_hdl_frame:screen-value = 'Fornec Origem:' THEN DO :
               assign v_hdl_frame:visible = v_log_faixa_fornec_orig.
            END.
            IF  v_hdl_frame:name = 'v_cdn_fornecedor_ini' THEN DO :
                assign v_hdl_frame:help = "C¢digo Fornecedor" /*l_codigo_fornecedor*/ .
            END.         
            ASSIGN v_hdl_frame = v_hdl_frame:NEXT-SIBLING.
       end. 
       else
           assign v_hdl_frame = v_hdl_frame:first-child.
    END.



    /* End_Include: ix_p10_rpt_compl_impto_retid_ap */


    block1:
    repeat on error undo block1, retry block1:

        main_block:
        repeat on error undo super_block, retry super_block
                        on endkey undo super_block, leave super_block
                        on stop undo super_block, retry super_block
                        with frame f_rpt_41_compl_impto_retid_ap:

            if (retry) then
                output stream s_1 close.
            assign v_log_print = no.
            if  valid-handle(v_wgh_focus) then
                wait-for go of frame f_rpt_41_compl_impto_retid_ap focus v_wgh_focus.
            else
                wait-for go of frame f_rpt_41_compl_impto_retid_ap.

            param_block:
            do transaction:

                /* Begin_Include: ix_p15_rpt_compl_impto_retid_ap */
                assign input frame f_rpt_41_compl_impto_retid_ap v_cod_classif_impto_faixa_fim
                       input frame f_rpt_41_compl_impto_retid_ap v_cod_classif_impto_faixa_inic
                       input frame f_rpt_41_compl_impto_retid_ap v_cod_impto_faixa_fim
                       input frame f_rpt_41_compl_impto_retid_ap v_cod_impto_faixa_inic
                       input frame f_rpt_41_compl_impto_retid_ap v_cod_indic_econ_fim
                       input frame f_rpt_41_compl_impto_retid_ap v_cod_indic_econ_ini
                       input frame f_rpt_41_compl_impto_retid_ap v_cod_pais_faixa_fim
                       input frame f_rpt_41_compl_impto_retid_ap v_cod_pais_faixa_inic
                       input frame f_rpt_41_compl_impto_retid_ap v_cod_unid_federac_faixa_fim
                       input frame f_rpt_41_compl_impto_retid_ap v_cod_unid_federac_faixa_inic
                       input frame f_rpt_41_compl_impto_retid_ap v_dat_vencto_docto_fim
                       input frame f_rpt_41_compl_impto_retid_ap v_dat_vencto_docto_inic
                       input frame f_rpt_41_compl_impto_retid_ap v_log_mostra_impto_retid
                       input frame f_rpt_41_compl_impto_retid_ap v_log_mostra_impto_taxado
                       input frame f_rpt_41_compl_impto_retid_ap v_log_impto_sem_sdo
                       input frame f_rpt_41_compl_impto_retid_ap v_log_tot_fornec_orig
                       input frame f_rpt_41_compl_impto_retid_ap v_log_tot_fornec_dest
                       input frame f_rpt_41_compl_impto_retid_ap v_log_tot_classif_impto
                       input frame f_rpt_41_compl_impto_retid_ap v_log_por_estab
                       input frame f_rpt_41_compl_impto_retid_ap v_ind_classif_rpt_impto_retid_ap
                       input frame f_rpt_41_compl_impto_retid_ap v_log_quebra_fornec
                       input frame f_rpt_41_compl_impto_retid_ap v_log_recolh.

                if v_log_faixa_fornec_orig = YES then
                    assign input frame f_rpt_41_compl_impto_retid_ap v_cdn_fornecedor_ini
                           input frame f_rpt_41_compl_impto_retid_ap v_cdn_fornecedor_fim.

                assign dwb_rpt_param.cod_dwb_parameters =  v_cod_pais_faixa_inic             + chr(10) +
                                                           v_cod_pais_faixa_fim              + chr(10) +
                                                           v_cod_unid_federac_faixa_inic     + chr(10) +
                                                           v_cod_unid_federac_faixa_fim      + chr(10) +
                                                           v_cod_impto_faixa_inic            + chr(10) +
                                                           v_cod_impto_faixa_fim             + chr(10) +
                                                           v_cod_classif_impto_faixa_inic    + chr(10) +
                                                           v_cod_classif_impto_faixa_fim     + chr(10) +
                                                           string(v_dat_vencto_docto_inic)   + chr(10) +
                                                           string(v_dat_vencto_docto_fim)    + chr(10) +
                                                           v_cod_indic_econ_ini              + chr(10) +
                                                           v_cod_indic_econ_fim              + chr(10) +
                                                           string(v_log_mostra_impto_retid)  + chr(10) +
                                                           string(v_log_mostra_impto_taxado) + chr(10) +
                                                           string(v_log_tot_classif_impto)   + chr(10) +
                                                           string(v_log_tot_fornec_orig)     + chr(10) +
                                                           string(v_log_tot_fornec_dest)     + chr(10) +
                                                           string(v_log_por_estab)           + chr(10) +
                                                           v_ind_classif_rpt_impto_retid_ap  + chr(10) +
                                                           string(v_log_impto_sem_sdo)       + chr(10) + 
                                                           string(v_log_quebra_fornec)       + chr(10) +
                                                           string(v_log_recolh).

                if (v_log_faixa_fornec_orig = YES) then Do:
                    assign dwb_rpt_param.cod_dwb_parameters = dwb_rpt_param.cod_dwb_parameters + chr(10) +
                                                              STRING(v_cdn_fornecedor_ini)     + chr(10) +
                                                              STRING(v_cdn_fornecedor_fim).
                end.
                /* End_Include: ix_p15_rpt_compl_impto_retid_ap */

                assign dwb_rpt_param.log_dwb_print_parameters = input frame f_rpt_41_compl_impto_retid_ap v_log_print_par
                       dwb_rpt_param.ind_dwb_run_mode         = input frame f_rpt_41_compl_impto_retid_ap rs_ind_run_mode
                       input frame f_rpt_41_compl_impto_retid_ap v_qtd_line.

                /* ix_p20_rpt_compl_impto_retid_ap */
            end /* do param_block */.

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
                                              Input 41,
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
                            assign v_cod_dwb_file   = session:temp-directory + substring ("prgfin/apb/apb318aa.py", 12, 6) + '.tmp'
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
                    assign v_nom_prog_ext  = caps(substring("prgfin/apb/apb318aa.py",12,8))
                           v_cod_release   = trim(" 1.00.00.034":U)
                           v_dat_execution = today
                           v_hra_execution = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":","").
                    run pi_rpt_compl_impto_retid_ap /*pi_rpt_compl_impto_retid_ap*/.
                end /* else */.
                if  dwb_rpt_param.log_dwb_print_parameters = yes
                then do:
                    if (page-number (s_1) > 0) then
                        page stream s_1.
                    /* ix_p29_rpt_compl_impto_retid_ap */    
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

                    /* Begin_Include: ix_p30_rpt_compl_impto_retid_ap */
                    if (line-counter(s_1) + 15) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        skip (1)
                        "   Classificaá∆o: " at 37
                        v_ind_classif_rpt_impto_retid_ap at 55 format "X(40)" skip
                        "Totaliza Classificaá∆o do Imposto:" at 20
                        v_log_tot_classif_impto at 55 format "Sim/N∆o" skip
                        "Totaliza Fornecedor Origem:" at 27
                        v_log_tot_fornec_orig at 55 format "Sim/N∆o" skip.
                    put stream s_1 unformatted 
                        "Totaliza Fornecedor Favorecido:" at 23
                        v_log_tot_fornec_dest at 55 format "Sim/N∆o" skip
                        "   Imposto Retido: " at 36
                        v_log_mostra_impto_retid at 55 format "Sim/N∆o" skip
                        "   Imposto Taxado: " at 36
                        v_log_mostra_impto_taxado at 55 format "Sim/N∆o" skip
                        "    Imposto sem Saldo: " at 32
                        v_log_impto_sem_sdo at 55 format "Sim/N∆o" skip
                        "    Apenas a Recolher: " at 32
                        v_log_recolh at 55 format "Sim/N∆o" skip.
                    put stream s_1 unformatted 
                        "  Pa°s: " at 47
                        v_cod_pais_faixa_inic at 55 format "x(03)"
                        "atÇ: " at 72
                        v_cod_pais_faixa_fim at 77 format "x(3)" skip
                        "  Unidade Federaá∆o: " at 34
                        v_cod_unid_federac_faixa_inic at 55 format "x(3)"
                        "atÇ: " at 72
                        v_cod_unid_federac_faixa_fim at 77 format "x(3)" skip
                        "  Imposto: " at 44
                        v_cod_impto_faixa_inic at 55 format "x(5)".
                    put stream s_1 unformatted 
                        "atÇ: " at 72
                        v_cod_impto_faixa_fim at 77 format "x(5)" skip
                        "Classificaá∆o: " at 40
                        v_cod_classif_impto_faixa_inic at 55 format "x(5)"
                        "atÇ: " at 72
                        v_cod_classif_impto_faixa_fim at 77 format "x(5)" skip
                        "  Vencimento: " at 41
                        v_dat_vencto_docto_inic at 55 format "99/99/9999"
                        "atÇ: " at 72
                        v_dat_vencto_docto_fim at 77 format "99/99/9999" skip.
                    put stream s_1 unformatted 
                        "  Moeda: " at 46
                        v_cod_indic_econ_ini at 55 format "x(8)"
                        "atÇ: " at 72
                        v_cod_indic_econ_fim at 77 format "x(8)" skip.

                    IF (v_log_faixa_fornec_orig = YES) THEN do:
                        if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            "Fornec Origem:" at 40
                            v_cdn_fornecedor_ini to 64 format ">>>,>>>,>>9"
                            "atÇ: " at 72
                            v_cdn_fornecedor_fim to 87 format ">>>,>>>,>>9" skip.
                    end.    

                    /* End_Include: ix_p30_rpt_compl_impto_retid_ap */

                end /* if */.
                output stream s_1 close.

/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
run pi_call_convert_object in v_prog_filtro_pdf (input no,
                                                 input rs_cod_dwb_output:screen-value in frame f_rpt_41_compl_impto_retid_ap,
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
                if (dwb_rpt_param.cod_dwb_output = "Terminal" /*l_terminal*/ ) then do:
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
                end.

                leave main_block.

            end /* if */.
            else do:
                leave super_block.
            end /* else */.

        end /* repeat main_block */.

        /* ix_p32_rpt_compl_impto_retid_ap */

        if  v_num_ped_exec <> 0
        then do:
            /* Criado pedido &1 para execuá∆o batch. */
            run pi_messages (input "show",
                             input 3556,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                v_num_ped_exec)) /*msg_3556*/.
            assign v_num_ped_exec = 0.
        end /* if */.

        /* ix_p35_rpt_compl_impto_retid_ap */

    end /* repeat block1 */.
end /* repeat super_block */.


/* Begin_Include: ix_p40_rpt_compl_impto_retid_ap */

/* Begin_Include: i_exec_program_epc */
&if '{&emsbas_version}' > '1.00' &then
if  v_nom_prog_upc <> '' then
do:
    assign v_rec_table_epc = recid(compl_impto_retid_ap).    
    run value(v_nom_prog_upc) (input 'Before-close',
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
    assign v_rec_table_epc = recid(compl_impto_retid_ap).    
    run value(v_nom_prog_appc) (input 'Before-close',
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
    assign v_rec_table_epc = recid(compl_impto_retid_ap).    
    run value(v_nom_prog_dpc) (input 'Before-close',
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

/* End_Include: i_exec_program_epc */


hide frame f_rpt_41_compl_impto_retid_ap.

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
    do with frame f_rpt_41_compl_impto_retid_ap:

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

    run pi_rpt_compl_impto_retid_ap /*pi_rpt_compl_impto_retid_ap*/.
END PROCEDURE. /* pi_output_reports */
/*****************************************************************************
** Procedure Interna.....: pi_rpt_compl_impto_retid_ap
** Descricao.............: pi_rpt_compl_impto_retid_ap
** Criado por............: Uno
** Criado em.............: 27/05/1996 08:23:26
** Alterado por..........: log43291
** Alterado em...........: 12/03/2010 09:06:43
*****************************************************************************/
PROCEDURE pi_rpt_compl_impto_retid_ap:

    /************************* Variable Definition Begin ************************/

    def var v_cod_estab_ant                  as character       no-undo. /*local*/
    def var v_log_convtdo                    as logical         no-undo. /*local*/
    def var v_log_impr_cabec                 as logical         no-undo. /*local*/
    def var v_log_impr_dados                 as logical         no-undo. /*local*/
    def var v_log_impr_tot                   as logical         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    assign v_val_tot_fornec_favorec[1] = 0
           v_val_tot_fornec_favorec[2] = 0
           v_val_tot_fornec_favorec[3] = 0
           v_val_tot_fornec_favorec[4] = 0
           v_val_tot_fornec_orig[1]    = 0
           v_val_tot_fornec_orig[2]    = 0
           v_val_tot_fornec_orig[3]    = 0
           v_val_tot_fornec_orig[4]    = 0
           v_val_tot_classif_impto[1]  = 0
           v_val_tot_classif_impto[2]  = 0
           v_val_tot_classif_impto[3]  = 0
           v_val_tot_classif_impto[4]  = 0
           v_val_tot_impto_estab[1]    = 0
           v_val_tot_impto_estab[2]    = 0
           v_val_tot_impto_estab[3]    = 0
           v_val_tot_impto_estab[4]    = 0
           v_log_impr_dados            = no
           v_log_impr_tot              = no
           v_log_impr_cabec            = yes.

    hide stream s_1 frame f_rpt_s_1_header_period.
    view stream s_1 frame f_rpt_s_1_header_unique.
    hide stream s_1 frame f_rpt_s_1_footer_last_page.
    hide stream s_1 frame f_rpt_s_1_footer_param_page.
    view stream s_1 frame f_rpt_s_1_footer_normal.

    grc_block:
    for each classif_impto no-lock
        where classif_impto.cod_pais          >= v_cod_pais_faixa_inic
        and   classif_impto.cod_pais          <= v_cod_pais_faixa_fim
        and   classif_impto.cod_unid_federac  >= v_cod_unid_federac_faixa_inic
        and   classif_impto.cod_unid_federac  <= v_cod_unid_federac_faixa_fim
        and   classif_impto.cod_imposto       >= v_cod_impto_faixa_inic
        and   classif_impto.cod_imposto       <= v_cod_impto_faixa_fim
        and   classif_impto.cod_classif_impto >= v_cod_classif_impto_faixa_inic
        and   classif_impto.cod_classif_impto <= v_cod_classif_impto_faixa_fim:

        if classif_impto.ind_tip_retenc_impto = "Tabela Progressiva" /*l_tabela_progressiva*/  then do:

            grp_block_1:
            for each compl_impto_retid_ap no-lock
                where compl_impto_retid_ap.cod_empresa           = v_cod_empres_usuar
                and   compl_impto_retid_ap.cod_pais              = classif_impto.cod_pais
                and   compl_impto_retid_ap.cod_unid_federac      = classif_impto.cod_unid_federac
                and   compl_impto_retid_ap.cod_imposto           = classif_impto.cod_imposto
                and   compl_impto_retid_ap.cod_classif_impto     = classif_impto.cod_classif_impto
                and   compl_impto_retid_ap.dat_vencto_tit_ap    >= v_dat_vencto_docto_inic
                and   compl_impto_retid_ap.dat_vencto_tit_ap    <= v_dat_vencto_docto_fim
                and   compl_impto_retid_ap.cod_indic_econ_impto >= v_cod_indic_econ_ini
                and   compl_impto_retid_ap.cod_indic_econ_impto <= v_cod_indic_econ_fim
                and   compl_impto_retid_ap.cdn_fornecedor       >= v_cdn_fornecedor_ini
                and   compl_impto_retid_ap.cdn_fornecedor       <= v_cdn_fornecedor_fim
                use-index cmplmptr_class_impto:

                find fornecedor
                    where fornecedor.cod_empresa    = v_cod_empres_usuar
                    and   fornecedor.cdn_fornecedor = compl_impto_retid_ap.cdn_fornecedor
                    no-lock no-error.

                if  fornecedor.num_pessoa modulo 2 <> 0 then next grp_block_1.

                &if '{&emsuni_version}' >= '5.05' &then
                    if classif_impto.ind_tip_acum_pagto = "Por Empresa" /*l_por_empresa*/ 
                &else
                    if classif_impto.cod_livre_1        = "Por Empresa" /*l_por_empresa*/ 
                &endif
                then do:

                    find first tt_compl_impto_retid_ap_menor no-lock
                         where tt_compl_impto_retid_ap_menor.tta_cod_empresa          = compl_impto_retid_ap.cod_empresa         
                           and tt_compl_impto_retid_ap_menor.tta_cod_pais             = compl_impto_retid_ap.cod_pais            
                           and tt_compl_impto_retid_ap_menor.tta_cod_unid_federac     = compl_impto_retid_ap.cod_unid_federac
                           and tt_compl_impto_retid_ap_menor.tta_cod_imposto          = compl_impto_retid_ap.cod_imposto    
                           and tt_compl_impto_retid_ap_menor.tta_cod_classif_impto    = compl_impto_retid_ap.cod_classif_impto   
                           and tt_compl_impto_retid_ap_menor.tta_dat_vencto_tit_ap    = compl_impto_retid_ap.dat_vencto_tit_ap   
                           and tt_compl_impto_retid_ap_menor.tta_cod_indic_econ_impto = compl_impto_retid_ap.cod_indic_econ_impto
                           and tt_compl_impto_retid_ap_menor.tta_cdn_fornecedor       = compl_impto_retid_ap.cdn_fornecedor no-error.

                end.
                else do:

                    find first tt_compl_impto_retid_ap_menor no-lock
                         where tt_compl_impto_retid_ap_menor.tta_cod_empresa          = compl_impto_retid_ap.cod_empresa
                           and tt_compl_impto_retid_ap_menor.tta_cod_estab            = compl_impto_retid_ap.cod_estab
                           and tt_compl_impto_retid_ap_menor.tta_cod_pais             = compl_impto_retid_ap.cod_pais            
                           and tt_compl_impto_retid_ap_menor.tta_cod_unid_federac     = compl_impto_retid_ap.cod_unid_federac
                           and tt_compl_impto_retid_ap_menor.tta_cod_imposto          = compl_impto_retid_ap.cod_imposto    
                           and tt_compl_impto_retid_ap_menor.tta_cod_classif_impto    = compl_impto_retid_ap.cod_classif_impto   
                           and tt_compl_impto_retid_ap_menor.tta_dat_vencto_tit_ap    = compl_impto_retid_ap.dat_vencto_tit_ap   
                           and tt_compl_impto_retid_ap_menor.tta_cod_indic_econ_impto = compl_impto_retid_ap.cod_indic_econ_impto
                           and tt_compl_impto_retid_ap_menor.tta_cdn_fornecedor       = compl_impto_retid_ap.cdn_fornecedor no-error.

                end.

                if avail tt_compl_impto_retid_ap_menor then do:
                    if tt_compl_impto_retid_ap_menor.tta_val_rendto_tribut > compl_impto_retid_ap.val_rendto_tribut then do:

                        assign tt_compl_impto_retid_ap_menor.tta_cod_estab            = compl_impto_retid_ap.cod_estab        
                               tt_compl_impto_retid_ap_menor.tta_num_id_tit_ap        = compl_impto_retid_ap.num_id_tit_ap    
                               tt_compl_impto_retid_ap_menor.tta_val_rendto_tribut    = compl_impto_retid_ap.val_rendto_tribut.

                    end.
                end.
                else do:

                    create tt_compl_impto_retid_ap_menor.
                    assign tt_compl_impto_retid_ap_menor.tta_cod_empresa          = compl_impto_retid_ap.cod_empresa      
                           tt_compl_impto_retid_ap_menor.tta_cod_estab            = compl_impto_retid_ap.cod_estab         
                           tt_compl_impto_retid_ap_menor.tta_num_id_tit_ap        = compl_impto_retid_ap.num_id_tit_ap 
                           tt_compl_impto_retid_ap_menor.tta_val_rendto_tribut    = compl_impto_retid_ap.val_rendto_tribut    
                           tt_compl_impto_retid_ap_menor.tta_cod_pais             = compl_impto_retid_ap.cod_pais
                           tt_compl_impto_retid_ap_menor.tta_cod_unid_federac     = compl_impto_retid_ap.cod_unid_federac
                           tt_compl_impto_retid_ap_menor.tta_cod_imposto          = compl_impto_retid_ap.cod_imposto
                           tt_compl_impto_retid_ap_menor.tta_cod_classif_impto    = compl_impto_retid_ap.cod_classif_impto
                           tt_compl_impto_retid_ap_menor.tta_dat_vencto_tit_ap    = compl_impto_retid_ap.dat_vencto_tit_ap
                           tt_compl_impto_retid_ap_menor.tta_cod_indic_econ_impto = compl_impto_retid_ap.cod_indic_econ_impto
                           tt_compl_impto_retid_ap_menor.tta_cdn_fornecedor       = compl_impto_retid_ap.cdn_fornecedor.

                end.
            end /* for grp_block_1 */.
        end.

        grp_block:
        for each compl_impto_retid_ap no-lock
            where compl_impto_retid_ap.cod_empresa           = v_cod_empres_usuar
            and   compl_impto_retid_ap.cod_pais              = classif_impto.cod_pais
            and   compl_impto_retid_ap.cod_unid_federac      = classif_impto.cod_unid_federac
            and   compl_impto_retid_ap.cod_imposto           = classif_impto.cod_imposto
            and   compl_impto_retid_ap.cod_classif_impto     = classif_impto.cod_classif_impto
            and   compl_impto_retid_ap.dat_vencto_tit_ap    >= v_dat_vencto_docto_inic
            and   compl_impto_retid_ap.dat_vencto_tit_ap    <= v_dat_vencto_docto_fim
            and   compl_impto_retid_ap.cod_indic_econ_impto >= v_cod_indic_econ_ini
            and   compl_impto_retid_ap.cod_indic_econ_impto <= v_cod_indic_econ_fim
            and   compl_impto_retid_ap.cdn_fornecedor       >= v_cdn_fornecedor_ini
            and   compl_impto_retid_ap.cdn_fornecedor       <= v_cdn_fornecedor_fim
            use-index cmplmptr_class_impto:

            run pi_verifica_faixa_compl_impto_retid_ap.
            if return-value <> "OK" /*l_ok*/  then next grp_block.

            run pi_cria_tt_compl_impto_retid_ap.

        end /* for grp_block */.        
    end /* for grc_block */.

    if connected('emsgra') then do:
       run ggp/ggapi110.p (input-output table tt_compl_impto_retid_ap).
    end.

    if  v_ind_classif_rpt_impto_retid_ap = ("Por Fornecedor Origem/Por Estab" /*l_por_fornecedor_origempor_estab*/ ) then do:

        /* Begin_Include: i_rpt_compl_impto_retido_quebra_fornec */
        grt_block:
        for each tt_compl_impto_retid_ap
            break by tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[1]
                  by tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[2]
                  by tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[3]
                  by date(tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[4]):

            if  first-of(tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[1] )
            then do:
                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "Fornecedor Origem:" at 9
                    tt_compl_impto_retid_ap.tta_cdn_fornecedor to 38 format ">>>,>>>,>>9"
                    "-" at 40
                    tt_compl_impto_retid_ap.tta_nom_pessoa at 42 format "x(40)" skip.
                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "ID Federal" at 16
                    ":" at 26
                    tt_compl_impto_retid_ap.tta_cod_id_feder at 28 format "x(20)" skip.
                if  (line-counter(s_1) + 5) > v_rpt_s_1_bottom
                then do:
                    page stream s_1.
                end /* if */.
                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted  skip (1).
                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "-- Fato Gerador Impto ---" at 1 skip.
                if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                    "Estab" at 1
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                    "Estab" at 1
        &ENDIF
                    "Esp" at 7
                    "SÇrie" at 11
                    "T°tulo" at 17
                    "/P" at 34
                    "Fornec" to 47
                    "Esp" at 49
                    "SÇrie" at 53
                    "T°tulo" at 59
                    "/P" at 76
                    "Dat Emis" at 79
                    "Recolhim" at 88
                    "Rend T" at 97
                    "Aliq" to 108
                    "Class" at 110
                    "Rend Tribut" to 131
                    "Vl Impto Orig" to 145
                    "Val Sdo Impto" to 162
                    "Impto IE Pa°s" to 176
                    "Recolher" at 178 skip
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                    "-----" at 1
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                    "-----" at 1
        &ENDIF
                    "---" at 7
                    "-----" at 11
                    "----------------" at 17
                    "--" at 34
                    "-----------" to 47
                    "---" at 49
                    "-----" at 53
                    "----------------" at 59
                    "--" at 76
                    "--------" at 79
                    "--------" at 88
                    "------" at 97
                    "-----" to 108
                    "-----" at 110
                    "----------------" to 131
                    "-------------" to 145
                    "----------------" to 162
                    "-------------" to 176
                    "--------" at 178 skip.
                assign v_log_impr_cabec = no
                       v_log_impr_dados = yes.
            end /* if */.    
            if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                page stream s_1.
            put stream s_1 unformatted 
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                tt_compl_impto_retid_ap.tta_cod_estab at 1 format "x(3)"
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                tt_compl_impto_retid_ap.tta_cod_estab at 1 format "x(5)"
        &ENDIF
                tt_compl_impto_retid_ap.tta_cod_espec_docto at 7 format "x(3)".
        put stream s_1 unformatted 
                tt_compl_impto_retid_ap.tta_cod_ser_docto at 11 format "x(5)"
                tt_compl_impto_retid_ap.tta_cod_tit_ap at 17 format "x(16)"
                tt_compl_impto_retid_ap.tta_cod_parcela at 34 format "x(02)"
                tt_compl_impto_retid_ap.ttv_cdn_fornecedor to 47 format ">>>,>>>,>>9"
                tt_compl_impto_retid_ap.ttv_cod_espec_docto at 49 format "x(3)"
                tt_compl_impto_retid_ap.ttv_cod_ser_docto at 53 format "x(5)"
                tt_compl_impto_retid_ap.ttv_cod_tit_ap at 59 format "x(16)"
                tt_compl_impto_retid_ap.ttv_cod_parcela at 76 format "x(02)"
                tt_compl_impto_retid_ap.ttv_dat_emis at 79 format "99/99/99"
                tt_compl_impto_retid_ap.ttv_dat_recolhto at 88 format "99/99/99".
        put stream s_1 unformatted 
                tt_compl_impto_retid_ap.ttv_cod_indic_econ_rdt at 97 format "x(6)"
                tt_compl_impto_retid_ap.tta_val_aliq_impto to 108 format ">9.99"
                tt_compl_impto_retid_ap.tta_cod_classif_impto at 110 format "x(05)"
                tt_compl_impto_retid_ap.tta_val_rendto_tribut to 131 format ">,>>>,>>>,>>9.99"
                tt_compl_impto_retid_ap.ttv_val_origin_tit_ap_2 to 145 format ">,>>>,>>9.99"
                tt_compl_impto_retid_ap.ttv_val_imposto_rel to 162 format ">,>>>,>>>,>>9.99"
                tt_compl_impto_retid_ap.ttv_val_impto_indic_econ_pais to 176 format ">,>>>,>>9.99"
                tt_compl_impto_retid_ap.ttv_log_recolh at 178 format "Sim/N∆o" skip.
            assign v_val_tot_classif_impto[1]  = v_val_tot_classif_impto[1]  + tt_compl_impto_retid_ap.tta_val_rendto_tribut
                   v_val_tot_classif_impto[2]  = v_val_tot_classif_impto[2]  + tt_compl_impto_retid_ap.ttv_val_imposto_rel
                   v_val_tot_classif_impto[3]  = v_val_tot_classif_impto[3]  + tt_compl_impto_retid_ap.ttv_val_impto_indic_econ_pais
                   v_val_tot_classif_impto[4]  = v_val_tot_classif_impto[4]  + tt_compl_impto_retid_ap.ttv_val_origin_tit_ap_2
                   v_val_tot_fornec_favorec[1] = v_val_tot_fornec_favorec[1] + tt_compl_impto_retid_ap.tta_val_rendto_tribut
                   v_val_tot_fornec_favorec[2] = v_val_tot_fornec_favorec[2] + tt_compl_impto_retid_ap.ttv_val_imposto_rel
                   v_val_tot_fornec_favorec[3] = v_val_tot_fornec_favorec[3] + tt_compl_impto_retid_ap.ttv_val_impto_indic_econ_pais
                   v_val_tot_fornec_favorec[4] = v_val_tot_fornec_favorec[4] + tt_compl_impto_retid_ap.ttv_val_origin_tit_ap_2
                   v_val_tot_fornec_orig[1]    = v_val_tot_fornec_orig[1]    + tt_compl_impto_retid_ap.tta_val_rendto_tribut
                   v_val_tot_fornec_orig[2]    = v_val_tot_fornec_orig[2]    + tt_compl_impto_retid_ap.ttv_val_imposto_rel
                   v_val_tot_fornec_orig[3]    = v_val_tot_fornec_orig[3]    + tt_compl_impto_retid_ap.ttv_val_impto_indic_econ_pais
                   v_val_tot_fornec_orig[4]    = v_val_tot_fornec_orig[4]    + tt_compl_impto_retid_ap.ttv_val_origin_tit_ap_2.
            if  last-of( tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[3] )
            then do:
                if  v_log_tot_classif_impto = yes
                then do:
                    if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "----------------" at 116
                        "-------------" at 133
                        "----------------" at 147
                        "-------------" at 164 skip
                        "Totais Classificaá∆o do Imposto:" at 77
                        v_val_tot_classif_impto[1] to 131 format ">,>>>,>>>,>>9.99"
                        v_val_tot_classif_impto[4] to 145 format ">,>>>,>>9.99".
        put stream s_1 unformatted 
                        v_val_tot_classif_impto[2] to 162 format ">,>>>,>>>,>>9.99"
                        v_val_tot_classif_impto[3] to 176 format ">,>>>,>>9.99" skip.
                    assign v_val_tot_classif_impto[1] = 0
                           v_val_tot_classif_impto[2] = 0
                           v_val_tot_classif_impto[3] = 0
                           v_val_tot_classif_impto[4] = 0                              
                           v_log_impr_tot             = yes.                              
                end /* if */.
            end /* if */.
            if  last-of( tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[2] )
            then do:
                if  v_log_tot_fornec_dest = yes
                then do:
                    if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "----------------" at 116
                        "-------------" at 133
                        "----------------" at 147
                        "-------------" at 164 skip
                        "Totais Fornecedor Favorecido:" at 80
                        v_val_tot_fornec_favorec[1] to 131 format ">,>>>,>>>,>>9.99"
                        v_val_tot_fornec_favorec[4] to 145 format ">,>>>,>>9.99".
        put stream s_1 unformatted 
                        v_val_tot_fornec_favorec[2] to 162 format ">,>>>,>>>,>>9.99"
                        v_val_tot_fornec_favorec[3] to 176 format ">,>>>,>>9.99" skip.
                    assign v_val_tot_fornec_favorec[1] = 0
                           v_val_tot_fornec_favorec[2] = 0
                           v_val_tot_fornec_favorec[3] = 0                              
                           v_val_tot_fornec_favorec[4] = 0                              
                           v_log_impr_tot              = yes.                              
                end /* if */.
            end /* if */.
            if  last-of( tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[1] )
            then do:
                if  v_log_tot_fornec_orig = yes
                then do:
                    if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "----------------" at 116
                        "-------------" at 133
                        "----------------" at 147
                        "-------------" at 164 skip
                        "Totais Fornecedor Origem:" at 84
                        v_val_tot_fornec_orig[1] to 131 format ">,>>>,>>>,>>9.99"
                        v_val_tot_fornec_orig[4] to 145 format ">,>>>,>>9.99".
        put stream s_1 unformatted 
                        v_val_tot_fornec_orig[2] to 162 format ">,>>>,>>>,>>9.99"
                        v_val_tot_fornec_orig[3] to 176 format ">,>>>,>>9.99" skip.
                    assign v_val_tot_fornec_orig[1] = 0
                           v_val_tot_fornec_orig[2] = 0
                           v_val_tot_fornec_orig[3] = 0                              
                           v_val_tot_fornec_orig[4] = 0                              
                           v_log_impr_tot           = yes.
                end /* if */.    
            end /* if */.
            if v_log_quebra_fornec then do:
                if  last-of( tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[1] )
                then do:
                    page stream s_1.
                end /* if */.
            end.
            if  v_log_impr_tot = yes 
            or last-of( tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[1] )
            then do:
                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted  skip (1). 
                assign v_log_impr_tot = no.
            end /* if */.    
        end /* for grt_block */.        
        /* End_Include: i_rpt_compl_impto_retido_quebra_fornec */

    end.
    else do:

        /* Begin_Include: i_rpt_compl_impto_retido */
        assign v_cod_estab_ant = "".

        grt_block:
        for each tt_compl_impto_retid_ap
            break by tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[1]
                  by tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[2]
                  by tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[3]
                  by date(tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[4]):

            if  v_log_por_estab = yes
            and first-of(tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[1])
            and substr(tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[1],1,3) <> v_cod_estab_ant then do:
                if  v_cod_estab_ant <> "" then do:
                    if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "----------------" at 116
                        "-------------" at 133
                        "----------------" at 147
                        "-------------" at 164 skip
                        "Total do Estabelecimento:" at 84
                        v_val_tot_impto_estab[1] to 131 format ">,>>>,>>>,>>9.99"
                        v_val_tot_impto_estab[4] to 145 format ">>,>>>,>>9.99".
        put stream s_1 unformatted 
                        v_val_tot_impto_estab[2] to 162 format ">,>>>,>>>,>>9.99"
                        v_val_tot_impto_estab[3] to 176 format ">>,>>>,>>9.99" skip.
                    if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted  skip (1). 
                    assign v_val_tot_impto_estab[1] = 0
                           v_val_tot_impto_estab[2] = 0
                           v_val_tot_impto_estab[3] = 0
                           v_val_tot_impto_estab[4] = 0.
                end.
                assign v_cod_estab_ant = substr(tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[1],1,3).
            end.

            if  first-of(tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[1] )
            then do:
                if  v_log_por_estab = yes
                then do:
                    if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                        "Estabelecimento: " at 11
                        fill(" ", 3 - length(trim(tt_compl_impto_retid_ap.tta_cod_estab))) + trim(tt_compl_impto_retid_ap.tta_cod_estab) to 30 format "x(3)"
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        "Estabelecimento: " at 9
                        fill(" ", 5 - length(trim(tt_compl_impto_retid_ap.tta_cod_estab))) + trim(tt_compl_impto_retid_ap.tta_cod_estab) to 30 format "x(5)".
        put stream s_1 unformatted 
        &ENDIF skip.
                end /* if */.
                if  v_ind_classif_rpt_impto_retid_ap = "Por Classificaá∆o do Imposto" /*l_por_classificacao_do_imposto*/ 
                then do:
                    if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "Pa°s: " at 22
                        fill(" ", 3 - length(trim(tt_compl_impto_retid_ap.tta_cod_pais))) + trim(tt_compl_impto_retid_ap.tta_cod_pais) to 30 format "x(3)" skip.
                    if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "Unidade Federaá∆o: " at 9
                        fill(" ", 3 - length(trim(tt_compl_impto_retid_ap.tta_cod_unid_federac))) + trim(tt_compl_impto_retid_ap.tta_cod_unid_federac) to 30 format "x(3)" skip.
                    if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "Imposto: " at 19
                        fill(" ", 5 - length(trim(tt_compl_impto_retid_ap.tta_cod_imposto))) + trim(tt_compl_impto_retid_ap.tta_cod_imposto) to 32 format "x(5)" skip.
                    if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "Classificaá∆o" at 13
                        ":" at 26
                        tt_compl_impto_retid_ap.tta_cod_classif_impto at 28 format "x(05)" skip.
                end /* if */.
                if  v_ind_classif_rpt_impto_retid_ap = "Por Estab/Fornecedor Favorecido" /*l_por_estabfornec_favorec*/ 
                then do:
                    if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "Fornecedor Favorecido:" at 5
                        tt_compl_impto_retid_ap.ttv_cdn_fornecedor to 38 format ">>>,>>>,>>9"
                        "-" at 40
                        tt_compl_impto_retid_ap.ttv_nom_pessoa at 42 format "x(40)" skip.
                    if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "ID Federal" at 16
                        ":" at 26
                        tt_compl_impto_retid_ap.ttv_cod_id_feder at 28 format "x(20)" skip.
                end /* if */.      
                if  v_ind_classif_rpt_impto_retid_ap = "Por Estab/Fornecedor Origem" /*l_por_estabfornec_origem*/ 
                then do:
                    if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "Fornecedor Origem:" at 9
                        tt_compl_impto_retid_ap.tta_cdn_fornecedor to 38 format ">>>,>>>,>>9"
                        "-" at 40
                        tt_compl_impto_retid_ap.tta_nom_pessoa at 42 format "x(40)" skip.
                    if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "ID Federal" at 16
                        ":" at 26
                        tt_compl_impto_retid_ap.tta_cod_id_feder at 28 format "x(20)" skip.
                end /* if */.      

                if  (line-counter(s_1) + 5) > v_rpt_s_1_bottom
                then do:
                    page stream s_1.
                end /* if */.
                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted  skip (1).
                if  v_ind_classif_rpt_impto_retid_ap = "Por Estab/Fornecedor Origem" /*l_por_estabfornec_origem*/ 
                then do:
                    if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "- Fato Gerad Impto --" at 1 skip.
                    if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "Esp" at 1
                        "SÇrie" at 5
                        "T°tulo" at 11
                        "/P" at 28
                        "Fornec" to 41
                        "Esp" at 43
                        "SÇrie" at 47
                        "T°tulo" at 53
                        "/P" at 70
                        "Dat Emis" at 73
                        "Recolhim" at 84
                        "Rend Tri" at 95
                        "Aliq" to 108
                        "Class" at 110
                        "Rend Tribut" to 131
                        "Vl Impto Orig" to 145
                        "Val Sdo Impto" to 162
                        "Impto IE Pa°s" to 176
                        "Recolher" at 178 skip
                        "---" at 1
                        "-----" at 5
                        "----------------" at 11
                        "--" at 28
                        "-----------" to 41
                        "---" at 43
                        "-----" at 47
                        "----------------" at 53
                        "--" at 70
                        "----------" at 73
                        "----------" at 84
                        "--------" at 95
                        "-----" to 108
                        "-----" at 110
                        "----------------" to 131
                        "-------------" to 145
                        "----------------" to 162
                        "-------------" to 176
                        "--------" at 178 skip.
                end /* if */.
                if  v_ind_classif_rpt_impto_retid_ap = "Por Classificaá∆o do Imposto" /*l_por_classificacao_do_imposto*/ 
                then do:
                    if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "----- Fato Gerador Imposto ------" at 1 skip.
                    if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "Fornec" to 9
                        "Esp" at 11
                        "SÇrie" at 15
                        "T°tulo" at 21
                        "/P" at 37
                        "Fornec" to 48
                        "Esp" at 50
                        "SÇrie" at 54
                        "T°tulo" at 60
                        "/P" at 76
                        "Dat Emis" at 79
                        "Recolhim" at 88
                        "Rend T" at 97
                        "Aliq" to 108
                        "Class" at 110
                        "Rend Tribut" to 131
                        "Vl Impto Orig" to 145
                        "Val Sdo Impto" to 162
                        "Impto IE Pa°s" to 176
                        "Recolher" at 178 skip
                        "---------" to 9
                        "---" at 11
                        "-----" at 15
                        "----------------" at 21
                        "--" at 37
                        "---------" to 48
                        "---" at 50
                        "-----" at 54
                        "----------------" at 60
                        "--" at 76
                        "--------" at 79
                        "--------" at 88
                        "------" at 97
                        "-----" to 108
                        "-----" at 110
                        "----------------" to 131
                        "-------------" to 145
                        "----------------" to 162
                        "-------------" to 176
                        "--------" at 178 skip.
                end /* if */.
                if  v_ind_classif_rpt_impto_retid_ap = "Por Estab/Fornecedor Favorecido" /*l_por_estabfornec_favorec*/ 
                then do:
                    if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "----- Fato Gerador Imposto ------" at 24 skip.
                    if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "Esp" at 1
                        "SÇrie" at 5
                        "T°tulo" at 11
                        "/P" at 28
                        "Fornec" to 41
                        "Esp" at 43
                        "SÇrie" at 47
                        "T°tulo" at 53
                        "/P" at 70
                        "Dat Emis" at 73
                        "Recolhim" at 84
                        "Rend Tri" at 95
                        "Aliq" to 108
                        "Class" at 110
                        "Rend Tribut" to 131
                        "Vl Impto Orig" to 145
                        "Val Sdo Impto" to 162
                        "Impto IE Pa°s" to 176
                        "Recolher" at 178 skip
                        "---" at 1
                        "-----" at 5
                        "----------------" at 11
                        "--" at 28
                        "-----------" to 41
                        "---" at 43
                        "-----" at 47
                        "----------------" at 53
                        "--" at 70
                        "----------" at 73
                        "----------" at 84
                        "--------" at 95
                        "-----" to 108
                        "-----" at 110
                        "----------------" to 131
                        "-------------" to 145
                        "----------------" to 162
                        "-------------" to 176
                        "--------" at 178 skip.
                end /* if */.

                assign v_log_impr_cabec = no
                       v_log_impr_dados = yes.
            end /* if */.

            if  v_ind_classif_rpt_impto_retid_ap = "Por Estab/Fornecedor Origem" /*l_por_estabfornec_origem*/ 
            then do:
                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    tt_compl_impto_retid_ap.tta_cod_espec_docto at 1 format "x(3)"
                    tt_compl_impto_retid_ap.tta_cod_ser_docto at 5 format "x(5)"
                    tt_compl_impto_retid_ap.tta_cod_tit_ap at 11 format "x(16)"
                    tt_compl_impto_retid_ap.tta_cod_parcela at 28 format "x(02)"
                    tt_compl_impto_retid_ap.ttv_cdn_fornecedor to 41 format ">>>,>>>,>>9"
                    tt_compl_impto_retid_ap.ttv_cod_espec_docto at 43 format "x(3)"
                    tt_compl_impto_retid_ap.ttv_cod_ser_docto at 47 format "x(5)".
        put stream s_1 unformatted 
                    tt_compl_impto_retid_ap.ttv_cod_tit_ap at 53 format "x(16)"
                    tt_compl_impto_retid_ap.ttv_cod_parcela at 70 format "x(02)"
                    tt_compl_impto_retid_ap.ttv_dat_emis at 73 format "99/99/9999"
                    tt_compl_impto_retid_ap.ttv_dat_recolhto at 84 format "99/99/9999"
                    tt_compl_impto_retid_ap.ttv_cod_indic_econ_rdt at 95 format "x(8)"
                    tt_compl_impto_retid_ap.tta_val_aliq_impto to 108 format ">9.99"
                    tt_compl_impto_retid_ap.tta_cod_classif_impto at 110 format "x(05)"
                    tt_compl_impto_retid_ap.tta_val_rendto_tribut to 131 format ">,>>>,>>>,>>9.99"
                    tt_compl_impto_retid_ap.ttv_val_origin_tit_ap_2 to 145 format ">,>>>,>>9.99"
                    tt_compl_impto_retid_ap.ttv_val_imposto_rel to 162 format ">,>>>,>>>,>>9.99".
        put stream s_1 unformatted 
                    tt_compl_impto_retid_ap.ttv_val_impto_indic_econ_pais to 176 format ">,>>>,>>9.99"
                    tt_compl_impto_retid_ap.ttv_log_recolh at 178 format "Sim/N∆o" skip.
            end /* if */.
            if  v_ind_classif_rpt_impto_retid_ap = "Por Classificaá∆o do Imposto" /*l_por_classificacao_do_imposto*/ 
            then do:
                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    tt_compl_impto_retid_ap.ttv_cdn_fornecedor to 9 format ">>>>>>>>9"
                    tt_compl_impto_retid_ap.ttv_cod_espec_docto at 11 format "x(3)"
                    tt_compl_impto_retid_ap.ttv_cod_ser_docto at 15 format "x(5)"
                    tt_compl_impto_retid_ap.ttv_cod_tit_ap at 21 format "x(16)"
                    tt_compl_impto_retid_ap.ttv_cod_parcela at 37 format "x(02)"
                    tt_compl_impto_retid_ap.tta_cdn_fornecedor to 48 format ">>>>>>>>9"
                    tt_compl_impto_retid_ap.tta_cod_espec_docto at 50 format "x(3)".
        put stream s_1 unformatted 
                    tt_compl_impto_retid_ap.tta_cod_ser_docto at 54 format "x(5)"
                    tt_compl_impto_retid_ap.tta_cod_tit_ap at 60 format "x(16)"
                    tt_compl_impto_retid_ap.tta_cod_parcela at 76 format "x(02)"
                    tt_compl_impto_retid_ap.ttv_dat_emis at 79 format "99/99/99"
                    tt_compl_impto_retid_ap.ttv_dat_recolhto at 88 format "99/99/99"
                    tt_compl_impto_retid_ap.ttv_cod_indic_econ_rdt at 97 format "x(6)"
                    tt_compl_impto_retid_ap.tta_val_aliq_impto to 108 format ">9.99"
                    tt_compl_impto_retid_ap.tta_cod_classif_impto at 110 format "x(05)"
                    tt_compl_impto_retid_ap.tta_val_rendto_tribut to 131 format ">,>>>,>>>,>>9.99"
                    tt_compl_impto_retid_ap.ttv_val_origin_tit_ap_2 to 145 format ">,>>>,>>9.99".
        put stream s_1 unformatted 
                    tt_compl_impto_retid_ap.ttv_val_imposto_rel to 162 format ">,>>>,>>>,>>9.99"
                    tt_compl_impto_retid_ap.ttv_val_impto_indic_econ_pais to 176 format ">,>>>,>>9.99"
                    tt_compl_impto_retid_ap.ttv_log_recolh at 178 format "Sim/N∆o" skip.
            end /* if */.
            if  v_ind_classif_rpt_impto_retid_ap = "Por Estab/Fornecedor Favorecido" /*l_por_estabfornec_favorec*/ 
            then do:
                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    tt_compl_impto_retid_ap.ttv_cod_espec_docto at 1 format "x(3)"
                    tt_compl_impto_retid_ap.ttv_cod_ser_docto at 5 format "x(5)"
                    tt_compl_impto_retid_ap.ttv_cod_tit_ap at 11 format "x(16)"
                    tt_compl_impto_retid_ap.ttv_cod_parcela at 28 format "x(02)"
                    tt_compl_impto_retid_ap.tta_cdn_fornecedor to 41 format ">>>,>>>,>>9"
                    tt_compl_impto_retid_ap.tta_cod_espec_docto at 43 format "x(3)"
                    tt_compl_impto_retid_ap.tta_cod_ser_docto at 47 format "x(5)".
        put stream s_1 unformatted 
                    tt_compl_impto_retid_ap.tta_cod_tit_ap at 53 format "x(16)"
                    tt_compl_impto_retid_ap.tta_cod_parcela at 70 format "x(02)"
                    tt_compl_impto_retid_ap.ttv_dat_emis at 73 format "99/99/9999"
                    tt_compl_impto_retid_ap.ttv_dat_recolhto at 84 format "99/99/9999"
                    tt_compl_impto_retid_ap.ttv_cod_indic_econ_rdt at 95 format "x(8)"
                    tt_compl_impto_retid_ap.tta_val_aliq_impto to 108 format ">9.99"
                    tt_compl_impto_retid_ap.tta_cod_classif_impto at 110 format "x(05)"
                    tt_compl_impto_retid_ap.tta_val_rendto_tribut to 131 format ">,>>>,>>>,>>9.99"
                    tt_compl_impto_retid_ap.ttv_val_origin_tit_ap_2 to 145 format ">,>>>,>>9.99"
                    tt_compl_impto_retid_ap.ttv_val_imposto_rel to 162 format ">,>>>,>>>,>>9.99".
        put stream s_1 unformatted 
                    tt_compl_impto_retid_ap.ttv_val_impto_indic_econ_pais to 176 format ">,>>>,>>9.99"
                    tt_compl_impto_retid_ap.ttv_log_recolh at 178 format "Sim/N∆o" skip.
            end /* if */.            

            assign v_val_tot_impto_estab[1]    = v_val_tot_impto_estab[1]    + tt_compl_impto_retid_ap.tta_val_rendto_tribut
                   v_val_tot_impto_estab[2]    = v_val_tot_impto_estab[2]    + tt_compl_impto_retid_ap.ttv_val_imposto_rel
                   v_val_tot_impto_estab[3]    = v_val_tot_impto_estab[3]    + tt_compl_impto_retid_ap.ttv_val_impto_indic_econ_pais
                   v_val_tot_impto_estab[4]    = v_val_tot_impto_estab[4]    + tt_compl_impto_retid_ap.ttv_val_origin_tit_ap_2
                   v_val_tot_classif_impto[1]  = v_val_tot_classif_impto[1]  + tt_compl_impto_retid_ap.tta_val_rendto_tribut
                   v_val_tot_classif_impto[2]  = v_val_tot_classif_impto[2]  + tt_compl_impto_retid_ap.ttv_val_imposto_rel
                   v_val_tot_classif_impto[3]  = v_val_tot_classif_impto[3]  + tt_compl_impto_retid_ap.ttv_val_impto_indic_econ_pais
                   v_val_tot_classif_impto[4]  = v_val_tot_classif_impto[4]  + tt_compl_impto_retid_ap.ttv_val_origin_tit_ap_2
                   v_val_tot_fornec_favorec[1] = v_val_tot_fornec_favorec[1] + tt_compl_impto_retid_ap.tta_val_rendto_tribut
                   v_val_tot_fornec_favorec[2] = v_val_tot_fornec_favorec[2] + tt_compl_impto_retid_ap.ttv_val_imposto_rel
                   v_val_tot_fornec_favorec[3] = v_val_tot_fornec_favorec[3] + tt_compl_impto_retid_ap.ttv_val_impto_indic_econ_pais
                   v_val_tot_fornec_favorec[4] = v_val_tot_fornec_favorec[4] + tt_compl_impto_retid_ap.ttv_val_origin_tit_ap_2
                   v_val_tot_fornec_orig[1]    = v_val_tot_fornec_orig[1]    + tt_compl_impto_retid_ap.tta_val_rendto_tribut
                   v_val_tot_fornec_orig[2]    = v_val_tot_fornec_orig[2]    + tt_compl_impto_retid_ap.ttv_val_imposto_rel
                   v_val_tot_fornec_orig[3]    = v_val_tot_fornec_orig[3]    + tt_compl_impto_retid_ap.ttv_val_impto_indic_econ_pais
                   v_val_tot_fornec_orig[4]    = v_val_tot_fornec_orig[4]    + tt_compl_impto_retid_ap.ttv_val_origin_tit_ap_2.

            if  last-of( tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[3] )
            then do:
                if  v_ind_classif_rpt_impto_retid_ap = "Por Classificaá∆o do Imposto" /*l_por_classificacao_do_imposto*/ 
                then do:
                    if  v_log_tot_fornec_orig = yes
                    then do:
                        if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            "----------------" at 116
                            "-------------" at 133
                            "----------------" at 147
                            "-------------" at 164 skip
                            "Totais Fornecedor Origem:" at 84
                            v_val_tot_fornec_orig[1] to 131 format ">,>>>,>>>,>>9.99"
                            v_val_tot_fornec_orig[4] to 145 format ">,>>>,>>9.99".
        put stream s_1 unformatted 
                            v_val_tot_fornec_orig[2] to 162 format ">,>>>,>>>,>>9.99"
                            v_val_tot_fornec_orig[3] to 176 format ">,>>>,>>9.99" skip.
                        assign v_val_tot_fornec_orig[1] = 0
                               v_val_tot_fornec_orig[2] = 0
                               v_val_tot_fornec_orig[3] = 0                              
                               v_val_tot_fornec_orig[4] = 0                              
                               v_log_impr_tot           = yes.                              
                    end /* if */.
                end /* if */. 
                if  v_ind_classif_rpt_impto_retid_ap = "Por Estab/Fornecedor Origem" /*l_por_estabfornec_origem*/ 
                then do:
                    if  v_log_tot_classif_impto = yes
                    then do:
                        if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            "----------------" at 116
                            "-------------" at 133
                            "----------------" at 147
                            "-------------" at 164 skip
                            "Totais Classificaá∆o do Imposto:" at 77
                            v_val_tot_classif_impto[1] to 131 format ">,>>>,>>>,>>9.99"
                            v_val_tot_classif_impto[4] to 145 format ">,>>>,>>9.99".
        put stream s_1 unformatted 
                            v_val_tot_classif_impto[2] to 162 format ">,>>>,>>>,>>9.99"
                            v_val_tot_classif_impto[3] to 176 format ">,>>>,>>9.99" skip.
                        assign v_val_tot_classif_impto[1] = 0
                               v_val_tot_classif_impto[2] = 0
                               v_val_tot_classif_impto[3] = 0         
                               v_val_tot_classif_impto[4] = 0         
                               v_log_impr_tot             = yes.                              
                    end /* if */.
                end /* if */. 
                if  v_ind_classif_rpt_impto_retid_ap = "Por Estab/Fornecedor Favorecido" /*l_por_estabfornec_favorec*/ 
                then do:
                    if  v_log_tot_fornec_orig = yes
                    then do:
                        if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            "----------------" at 116
                            "-------------" at 133
                            "----------------" at 147
                            "-------------" at 164 skip
                            "Totais Fornecedor Origem:" at 84
                            v_val_tot_fornec_orig[1] to 131 format ">,>>>,>>>,>>9.99"
                            v_val_tot_fornec_orig[4] to 145 format ">,>>>,>>9.99".
        put stream s_1 unformatted 
                            v_val_tot_fornec_orig[2] to 162 format ">,>>>,>>>,>>9.99"
                            v_val_tot_fornec_orig[3] to 176 format ">,>>>,>>9.99" skip.
                        assign v_val_tot_fornec_orig[1] = 0
                               v_val_tot_fornec_orig[2] = 0
                               v_val_tot_fornec_orig[3] = 0                              
                               v_val_tot_fornec_orig[4] = 0                              
                               v_log_impr_tot           = yes.                              
                    end /* if */.
                end /* if */.         
            end /* if */.


            if  last-of( tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[2] )
            then do:
                if  v_ind_classif_rpt_impto_retid_ap = "Por Classificaá∆o do Imposto" /*l_por_classificacao_do_imposto*/ 
                then do:
                    if  v_log_tot_fornec_dest = yes
                    then do:
                        if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            "----------------" at 116
                            "-------------" at 133
                            "----------------" at 147
                            "-------------" at 164 skip
                            "Totais Fornecedor Favorecido:" at 80
                            v_val_tot_fornec_favorec[1] to 131 format ">,>>>,>>>,>>9.99"
                            v_val_tot_fornec_favorec[4] to 145 format ">,>>>,>>9.99".
        put stream s_1 unformatted 
                            v_val_tot_fornec_favorec[2] to 162 format ">,>>>,>>>,>>9.99"
                            v_val_tot_fornec_favorec[3] to 176 format ">,>>>,>>9.99" skip.
                        assign v_val_tot_fornec_favorec[1] = 0
                               v_val_tot_fornec_favorec[2] = 0
                               v_val_tot_fornec_favorec[3] = 0                              
                               v_val_tot_fornec_favorec[4] = 0                              
                               v_log_impr_tot              = yes.                              
                    end /* if */.
                end /* if */. 
                if  v_ind_classif_rpt_impto_retid_ap = "Por Estab/Fornecedor Origem" /*l_por_estabfornec_origem*/ 
                then do:
                    if  v_log_tot_fornec_dest = yes
                    then do:
                        if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            "----------------" at 116
                            "-------------" at 133
                            "----------------" at 147
                            "-------------" at 164 skip
                            "Totais Fornecedor Favorecido:" at 80
                            v_val_tot_fornec_favorec[1] to 131 format ">,>>>,>>>,>>9.99"
                            v_val_tot_fornec_favorec[4] to 145 format ">,>>>,>>9.99".
        put stream s_1 unformatted 
                            v_val_tot_fornec_favorec[2] to 162 format ">,>>>,>>>,>>9.99"
                            v_val_tot_fornec_favorec[3] to 176 format ">,>>>,>>9.99" skip.
                        assign v_val_tot_fornec_favorec[1] = 0
                               v_val_tot_fornec_favorec[2] = 0
                               v_val_tot_fornec_favorec[3] = 0
                               v_val_tot_fornec_favorec[4] = 0                                                                                   
                               v_log_impr_tot              = yes.                              
                    end /* if */.
                end /* if */. 
                if  v_ind_classif_rpt_impto_retid_ap = "Por Estab/Fornecedor Favorecido" /*l_por_estabfornec_favorec*/ 
                then do:
                    if  v_log_tot_classif_impto = yes
                    then do:
                        if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            "----------------" at 116
                            "-------------" at 133
                            "----------------" at 147
                            "-------------" at 164 skip
                            "Totais Classificaá∆o do Imposto:" at 77
                            v_val_tot_classif_impto[1] to 131 format ">,>>>,>>>,>>9.99"
                            v_val_tot_classif_impto[4] to 145 format ">,>>>,>>9.99".
        put stream s_1 unformatted 
                            v_val_tot_classif_impto[2] to 162 format ">,>>>,>>>,>>9.99"
                            v_val_tot_classif_impto[3] to 176 format ">,>>>,>>9.99" skip.
                        assign v_val_tot_classif_impto[1] = 0
                               v_val_tot_classif_impto[2] = 0
                               v_val_tot_classif_impto[3] = 0                              
                               v_val_tot_classif_impto[4] = 0                              
                               v_log_impr_tot             = yes.                              
                    end /* if */.
                end /* if */.         
            end /* if */.


            if  last-of( tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[1] )
            then do:
                if  v_ind_classif_rpt_impto_retid_ap = "Por Classificaá∆o do Imposto" /*l_por_classificacao_do_imposto*/ 
                then do:
                    if  v_log_tot_classif_impto = yes
                    then do:
                        if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            "----------------" at 116
                            "-------------" at 133
                            "----------------" at 147
                            "-------------" at 164 skip
                            "Totais Classificaá∆o do Imposto:" at 77
                            v_val_tot_classif_impto[1] to 131 format ">,>>>,>>>,>>9.99"
                            v_val_tot_classif_impto[4] to 145 format ">,>>>,>>9.99".
        put stream s_1 unformatted 
                            v_val_tot_classif_impto[2] to 162 format ">,>>>,>>>,>>9.99"
                            v_val_tot_classif_impto[3] to 176 format ">,>>>,>>9.99" skip.
                        assign v_val_tot_classif_impto[1] = 0
                               v_val_tot_classif_impto[2] = 0
                               v_val_tot_classif_impto[3] = 0                              
                               v_val_tot_classif_impto[4] = 0                                                     
                               v_log_impr_tot             = yes.
                    end /* if */.    
                end /* if */.
                if  v_ind_classif_rpt_impto_retid_ap = "Por Estab/Fornecedor Favorecido" /*l_por_estabfornec_favorec*/ 
                then do:
                    if  v_log_tot_fornec_dest = yes
                    then do:
                        if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            "----------------" at 116
                            "-------------" at 133
                            "----------------" at 147
                            "-------------" at 164 skip
                            "Totais Fornecedor Favorecido:" at 80
                            v_val_tot_fornec_favorec[1] to 131 format ">,>>>,>>>,>>9.99"
                            v_val_tot_fornec_favorec[4] to 145 format ">,>>>,>>9.99".
        put stream s_1 unformatted 
                            v_val_tot_fornec_favorec[2] to 162 format ">,>>>,>>>,>>9.99"
                            v_val_tot_fornec_favorec[3] to 176 format ">,>>>,>>9.99" skip.
                        assign v_val_tot_fornec_favorec[1] = 0
                               v_val_tot_fornec_favorec[2] = 0
                               v_val_tot_fornec_favorec[3] = 0                              
                               v_val_tot_fornec_favorec[4] = 0                                                     
                               v_log_impr_tot              = yes.                              
                    end /* if */.
                end /* if */.
                if  v_ind_classif_rpt_impto_retid_ap = "Por Estab/Fornecedor Origem" /*l_por_estabfornec_origem*/ 
                then do:
                    if  v_log_tot_fornec_orig = yes
                    then do:
                        if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            "----------------" at 116
                            "-------------" at 133
                            "----------------" at 147
                            "-------------" at 164 skip
                            "Totais Fornecedor Origem:" at 84
                            v_val_tot_fornec_orig[1] to 131 format ">,>>>,>>>,>>9.99"
                            v_val_tot_fornec_orig[4] to 145 format ">,>>>,>>9.99".
        put stream s_1 unformatted 
                            v_val_tot_fornec_orig[2] to 162 format ">,>>>,>>>,>>9.99"
                            v_val_tot_fornec_orig[3] to 176 format ">,>>>,>>9.99" skip.
                        assign v_val_tot_fornec_orig[1] = 0
                               v_val_tot_fornec_orig[2] = 0
                               v_val_tot_fornec_orig[3] = 0                              
                               v_val_tot_fornec_orig[4] = 0                              
                               v_log_impr_tot           = yes.
                    end /* if */.    
                end /* if */.
            end /* if */.

            if  v_log_impr_tot = yes 
            or last-of( tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[1] )
            then do:
                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted  skip (1). 
                assign v_log_impr_tot = no.
            end /* if */.    

        end /* for grt_block */.  

        if  v_log_impr_dados =  yes
        and v_log_por_estab  =  yes
        and v_cod_estab_ant  <> "" then do:
            if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                page stream s_1.
            put stream s_1 unformatted 
                "----------------" at 116
                "-------------" at 133
                "----------------" at 147
                "-------------" at 164 skip
                "Total do Estabelecimento:" at 84
                v_val_tot_impto_estab[1] to 131 format ">,>>>,>>>,>>9.99"
                v_val_tot_impto_estab[4] to 145 format ">>,>>>,>>9.99".
        put stream s_1 unformatted 
                v_val_tot_impto_estab[2] to 162 format ">,>>>,>>>,>>9.99"
                v_val_tot_impto_estab[3] to 176 format ">>,>>>,>>9.99" skip.
        end.

        /* End_Include: i_rpt_compl_impto_retido */

    end.
    if  v_log_impr_dados = no
    then do:
        assign v_des_msg_relat = "Relat¢rio de" /*l_relat_de*/  + " " + v_rpt_s_1_name.
               v_des_msg_relat = "Relat¢rio em branco." /*2555*/ + chr(10) + chr(10) +
                                 substitute("Segundo os parÉmetros e/ou seleá∆o informados, n∆o foram encontrados valores para compor o(a) &1." /*2555*/, v_des_msg_relat).
        run pi_print_editor ("s_1", v_des_msg_relat, "     080", "", "     ", "", "     ").
        put stream s_1 unformatted 
            skip (1)
            entry(1, return-value, chr(255)) at 27 format "x(80)" skip.
        run pi_print_editor ("s_1", v_des_msg_relat, "at027080", "", "", "", "").
    end /* if */.

    hide stream s_1 frame f_rpt_s_1_footer_normal.
    hide stream s_1 frame f_rpt_s_1_footer_param_page.
    view stream s_1 frame f_rpt_s_1_footer_last_page.

    for each tt_compl_impto_retid_ap:
        delete tt_compl_impto_retid_ap.
    end.
    for each tt_compl_impto_retid_ap_menor:
        delete tt_compl_impto_retid_ap_menor.
    end.
END PROCEDURE. /* pi_rpt_compl_impto_retid_ap */
/*****************************************************************************
** Procedure Interna.....: pi_converter_indic_econ_finalid
** Descricao.............: pi_converter_indic_econ_finalid
** Criado por............: karla
** Criado em.............: // 
** Alterado por..........: fut12201
** Alterado em...........: 30/07/2008 10:08:19
*****************************************************************************/
PROCEDURE pi_converter_indic_econ_finalid:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_indic_econ
        as character
        format "x(8)"
        no-undo.
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
    def Input param p_dat_transacao
        as date
        format "99/99/9999"
        no-undo.
    def Input param p_val_transacao
        as decimal
        format "->>,>>>,>>>,>>9.99"
        decimals 2
        no-undo.
    def Input param p_cod_finalid_econ
        as character
        format "x(10)"
        no-undo.
    def output param p_cod_return
        as character
        format "x(40)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_cod_return
        as character
        format "x(40)":U
        no-undo.
    def var v_dat_cotac_indic_econ
        as date
        format "99/99/9999":U
        initial today
        label "Data Cotaá∆o"
        column-label "Data Cotaá∆o"
        no-undo.
    def var v_val_cotac_indic_econ
        as decimal
        format "->>,>>>,>>>,>>9.9999999999":U
        decimals 10
        label "Cotaá∆o"
        column-label "Cotaá∆o"
        no-undo.


    /************************** Variable Definition End *************************/



    elimina:
    for each tt_converter_finalid_econ exclusive-lock: 
        delete tt_converter_finalid_econ. 
    end /* for elimina */. 

    run pi_validar_indic_econ_valid (Input p_cod_indic_econ,
                                     Input p_dat_transacao,
                                     output p_cod_return) /*pi_validar_indic_econ_valid*/. 
    if  p_cod_return <> "OK" /*l_ok*/ 
    then do:
        return. 
    end /* if */. 



    if  p_cod_finalid_econ <> ""
    then do: 
        /* OBS: Esta alteraá∆o deve permanecer no c¢digo fonte, pois esta sendo feita sob demanda. Conforme acordo com a equipe do Test Center, 
               devido lista de impacto muito extensa. Atividade 138100 */
        find first compos_finalid no-lock
             where compos_finalid.cod_indic_econ_base = p_cod_indic_econ
               and compos_finalid.cod_finalid_econ = p_cod_finalid_econ
               and compos_finalid.dat_inic_valid <= p_dat_transacao
               and compos_finalid.dat_fim_valid > p_dat_transacao
             use-index cmpsfnld_parid_indic_econ no-error.
        if  not avail compos_finalid
        then do:
            assign p_cod_return = "782" + "," + p_cod_finalid_econ + "," + p_cod_indic_econ + "," + string(p_dat_transacao).
            return. 
        end /* if */. 

        if  compos_finalid.cod_indic_econ_base <> compos_finalid.cod_indic_econ_idx
        then do: 
            find finalid_econ no-lock 
                 where finalid_econ.cod_finalid_econ = compos_finalid.cod_finalid_econ no-error. 
            if  /* finalid_econ.ind_armaz_val = "Contabilidade" /*l_contabilidade*/ 
            or */ finalid_econ.ind_armaz_val = "N∆o" /*l_nao*/ 
            then do:
                assign p_cod_return = "1389" + "," + finalid_econ.cod_finalid_econ. 
                return. 
            end /* if */. 
            run pi_validar_finalid_unid_organ (Input compos_finalid.cod_finalid_econ,
                                               Input p_cod_unid_organ,
                                               Input p_dat_transacao,
                                               output p_cod_return) /*pi_validar_finalid_unid_organ*/. 
            if  p_cod_return <> "OK" /*l_ok*/ 
            then do:
                return. 
            end /* if */. 

            find first compos_finalid_cmcmm no-lock 
                  where compos_finalid_cmcmm.cod_finalid_econ = compos_finalid.cod_finalid_econ 
                    and compos_finalid_cmcmm.dat_inic_valid_finalid = compos_finalid.dat_inic_valid_finalid 
                    and compos_finalid_cmcmm.cod_indic_econ_base = compos_finalid.cod_indic_econ_base 
                    and compos_finalid_cmcmm.cod_indic_econ_idx = compos_finalid.cod_indic_econ_idx 
                    and compos_finalid_cmcmm.dat_inic_valid_compos = compos_finalid.dat_inic_valid 
                    and compos_finalid_cmcmm.dat_inic_valid <= p_dat_transacao 
                    and compos_finalid_cmcmm.dat_fim_valid > p_dat_transacao no-error. 

             if  avail compos_finalid_cmcmm
             then do: 
                  run pi_achar_cotac_indic_econ 

                                                (Input compos_finalid.cod_indic_econ_base,
                                                 Input compos_finalid.cod_indic_econ_idx,
                                                 Input p_dat_transacao,
                                                 Input "Real" /*l_real*/ ,
                                                 output v_dat_cotac_indic_econ,
                                                 output v_val_cotac_indic_econ,
                                                 output v_cod_return) /* pi_achar_cotac_indic_econ*/.

                  if  entry(1,v_cod_return) = "358"
                  then do: 
                      elimina:
                      for each tt_converter_finalid_econ exclusive-lock: 
                          delete tt_converter_finalid_econ. 
                      end /* for elimina */. 
                      assign p_cod_return = v_cod_return. 
                      return. 
                  end /* if */. 
                  create tt_converter_finalid_econ. 
                  assign tt_converter_finalid_econ.tta_cod_finalid_econ     = compos_finalid.cod_finalid_econ 
                         tt_converter_finalid_econ.tta_dat_cotac_indic_econ = v_dat_cotac_indic_econ 
                         tt_converter_finalid_econ.tta_val_cotac_indic_econ = v_val_cotac_indic_econ 
                         tt_converter_finalid_econ.tta_val_transacao        = p_val_transacao / v_val_cotac_indic_econ

                         .
                  run pi_retornar_indic_econ_finalid 

                                                     (Input compos_finalid.cod_finalid_econ,
                                                      Input p_dat_transacao,
                                                      output tt_converter_finalid_econ.tta_cod_indic_econ) /* pi_retornar_indic_econ_finalid*/.
             end /* if */. 
             else do: 
                  elimina:
                  for each tt_converter_finalid_econ exclusive-lock: 
                      delete tt_converter_finalid_econ. 
                  end /* for elimina */. 
                  /* alteraá∆o por demanda, deve permanecer no fonte, atividade 159924 (fo 1345764) */   
                  assign p_cod_return = "1200" + "," + 
                                        compos_finalid.cod_indic_econ_base + "," + 
                                        compos_finalid.cod_indic_econ_idx + "," + 
                                        string(p_dat_transacao)  + "," + 
                                        compos_finalid.cod_finalid_econ + "," + 
                                        string(compos_finalid.dat_inic_valid_finalid) + "," +
                                        string(compos_finalid.dat_inic_valid).                                     
                  return. 
             end /* else */. 
        end /* if */. 
        else do: 
            create tt_converter_finalid_econ. 
            assign tt_converter_finalid_econ.tta_cod_finalid_econ      = compos_finalid.cod_finalid_econ 
                   tt_converter_finalid_econ.tta_dat_cotac_indic_econ  = p_dat_transacao 
                   tt_converter_finalid_econ.tta_val_cotac_indic_econ  = 1 
                   tt_converter_finalid_econ.tta_val_transacao         = p_val_transacao 
                   tt_converter_finalid_econ.tta_cod_indic_econ        = compos_finalid.cod_indic_econ_idx. 
        end /* else */. 
    end /* if */. 
    else do: 
        composicoes: 
        for each compos_finalid no-lock 
         where compos_finalid.cod_indic_econ_base = p_cod_indic_econ 
           and compos_finalid.dat_inic_valid <= p_dat_transacao 
           and compos_finalid.dat_fim_valid > p_dat_transacao 
         use-index cmpsfnld_parid_indic_econ : 
           if  compos_finalid.cod_indic_econ_base <> compos_finalid.cod_indic_econ_idx
           then do: 
               find finalid_econ no-lock 
                    where finalid_econ.cod_finalid_econ = compos_finalid.cod_finalid_econ no-error. 
               if  finalid_econ.ind_armaz_val = "Contabilidade" /*l_contabilidade*/ 
               or  finalid_econ.ind_armaz_val = "N∆o" /*l_nao*/ 
               then do:
                   next composicoes. 
               end /* if */. 
               run pi_validar_finalid_unid_organ (Input compos_finalid.cod_finalid_econ, 
                                                  Input p_cod_unid_organ, 
                                                  Input p_dat_transacao, 
                                                  output v_cod_return) . 
               if  v_cod_return = "338"
               then do: 
                   next composicoes. 
               end /* if */. 
                find first compos_finalid_cmcmm no-lock 
                     where compos_finalid_cmcmm.cod_finalid_econ = compos_finalid.cod_finalid_econ 
                       and compos_finalid_cmcmm.dat_inic_valid_finalid = compos_finalid.dat_inic_valid_finalid 
                       and compos_finalid_cmcmm.cod_indic_econ_base = compos_finalid.cod_indic_econ_base 
                       and compos_finalid_cmcmm.cod_indic_econ_idx = compos_finalid.cod_indic_econ_idx 
                       and compos_finalid_cmcmm.dat_inic_valid_compos = compos_finalid.dat_inic_valid 
                       and compos_finalid_cmcmm.dat_inic_valid <= p_dat_transacao 
                       and compos_finalid_cmcmm.dat_fim_valid > p_dat_transacao no-error. 

                if  avail compos_finalid_cmcmm
                then do: 
                     run pi_achar_cotac_indic_econ 

                                                   (Input compos_finalid.cod_indic_econ_base, 
                                                    Input compos_finalid.cod_indic_econ_idx, 
                                                    Input p_dat_transacao, 
                                                    Input "Real" /*l_real*/ ,
                                                    output v_dat_cotac_indic_econ, 
                                                    output v_val_cotac_indic_econ, 
                                                    output v_cod_return) . 
                     if  entry(1,v_cod_return) = "358"
                     then do:
                        elimina: 
                        for 
                            each tt_converter_finalid_econ exclusive-lock: 
                            delete tt_converter_finalid_econ. 
                        end . 
                        assign p_cod_return = v_cod_return. 
                        return. 
                     end /* if */. 
                     create tt_converter_finalid_econ. 
                     assign tt_converter_finalid_econ.tta_cod_finalid_econ     = compos_finalid.cod_finalid_econ 
                            tt_converter_finalid_econ.tta_dat_cotac_indic_econ = v_dat_cotac_indic_econ 
                            tt_converter_finalid_econ.tta_val_cotac_indic_econ = v_val_cotac_indic_econ 
                            tt_converter_finalid_econ.tta_val_transacao        = p_val_transacao / v_val_cotac_indic_econ

                            .
                     run pi_retornar_indic_econ_finalid

                                                        (Input compos_finalid.cod_finalid_econ, 
                                                         Input p_dat_transacao, 
                                                         output tt_converter_finalid_econ.tta_cod_indic_econ) . 

                end /* if */. 
                else do: 
                   elimina: 
                   for 
                       each tt_converter_finalid_econ exclusive-lock: 
                       delete tt_converter_finalid_econ. 
                   end . 
                   /* alteraá∆o por demanda, deve permanecer no fonte, atividade 159924 (fo 1345764) */   
                   assign p_cod_return = "1200" + "," + 
                                         compos_finalid.cod_indic_econ_base + "," + 
                                         compos_finalid.cod_indic_econ_idx + "," + 
                                         string(p_dat_transacao) + "," +
                                         compos_finalid.cod_finalid_econ + "," + 
                                         string(compos_finalid.dat_inic_valid_finalid) + "," +
                                         string(compos_finalid.dat_inic_valid).                                     
                   return. 
                end /* else */. 
           end /* if */. 
           else do: 
                create tt_converter_finalid_econ. 
                assign tt_converter_finalid_econ.tta_cod_finalid_econ      = compos_finalid.cod_finalid_econ 
                       tt_converter_finalid_econ.tta_dat_cotac_indic_econ  = p_dat_transacao 
                       tt_converter_finalid_econ.tta_val_cotac_indic_econ  = 1 
                       tt_converter_finalid_econ.tta_val_transacao         = p_val_transacao 
                       tt_converter_finalid_econ.tta_cod_indic_econ        = compos_finalid.cod_indic_econ_idx. 
           end /* else */. 
        end . 
        find first tt_converter_finalid_econ no-lock no-error. 
        if  not avail tt_converter_finalid_econ
        then do: 
            assign p_cod_return = "1568". 
            return. 
        end /* if */. 
    end /* else */. 

    assign p_cod_return = "OK" /*l_ok*/ .
END PROCEDURE. /* pi_converter_indic_econ_finalid */
/*****************************************************************************
** Procedure Interna.....: pi_validar_indic_econ_valid
** Descricao.............: pi_validar_indic_econ_valid
** Criado por............: 
** Criado em.............: // 
** Alterado por..........: 
** Alterado em...........: 26/09/1995 10:04:56
*****************************************************************************/
PROCEDURE pi_validar_indic_econ_valid:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_indic_econ
        as character
        format "x(8)"
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

    find indic_econ no-lock
         where indic_econ.cod_indic_econ = p_cod_indic_econ /*cl_indic_econ_valid of indic_econ*/ no-error.
    if  not avail indic_econ
    then do:
       assign p_cod_return = "241".
       return.
    end /* if */.

    if  p_dat_transacao <  indic_econ.dat_inic_valid or
       p_dat_transacao >= indic_econ.dat_fim_valid
    then do:
       assign p_cod_return = "1199".
       return.
    end /* if */.

    assign p_cod_return = "OK" /*l_ok*/ .

END PROCEDURE. /* pi_validar_indic_econ_valid */
/*****************************************************************************
** Procedure Interna.....: pi_retornar_indic_econ_finalid
** Descricao.............: pi_retornar_indic_econ_finalid
** Criado por............: vladimir
** Criado em.............: // 
** Alterado por..........: Menna
** Alterado em...........: 06/05/1999 10:21:29
*****************************************************************************/
PROCEDURE pi_retornar_indic_econ_finalid:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_finalid_econ
        as character
        format "x(10)"
        no-undo.
    def Input param p_dat_transacao
        as date
        format "99/99/9999"
        no-undo.
    def output param p_cod_indic_econ
        as character
        format "x(8)"
        no-undo.


    /************************* Parameter Definition End *************************/

    find first histor_finalid_econ no-lock
         where histor_finalid_econ.cod_finalid_econ = p_cod_finalid_econ
           and histor_finalid_econ.dat_inic_valid_finalid <= p_dat_transacao
           and histor_finalid_econ.dat_fim_valid_finalid > p_dat_transacao
    &if "{&emsuni_version}" >= "5.01" &then
         use-index hstrfnld_id
    &endif
          /*cl_finalid_ativa of histor_finalid_econ*/ no-error.
    if  avail histor_finalid_econ then
        assign p_cod_indic_econ = histor_finalid_econ.cod_indic_econ.

END PROCEDURE. /* pi_retornar_indic_econ_finalid */
/*****************************************************************************
** Procedure Interna.....: pi_validar_finalid_unid_organ
** Descricao.............: pi_validar_finalid_unid_organ
** Criado por............: 
** Criado em.............: // 
** Alterado por..........: fut1228
** Alterado em...........: 21/01/2004 19:31:53
*****************************************************************************/
PROCEDURE pi_validar_finalid_unid_organ:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_finalid_econ
        as character
        format "x(10)"
        no-undo.
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
    def Input param p_dat_transacao
        as date
        format "99/99/9999"
        no-undo.
    def output param p_cod_return
        as character
        format "x(40)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /* Alterado para validar "finalid_unid_organ.dat_fim_valid >= p_dat_transacao", conforme Atividade 107753.
      Devido a lista de impacto desta PI ser muito extenáa, foi acordado que os demais 
      programas deveram ser alterados sobre demanda.*/

    find first finalid_unid_organ no-lock
         where finalid_unid_organ.cod_unid_organ = p_cod_unid_organ
           and finalid_unid_organ.cod_finalid_econ = p_cod_finalid_econ
           and finalid_unid_organ.dat_inic_valid <= p_dat_transacao
           and finalid_unid_organ.dat_fim_valid >= p_dat_transacao no-error.

    if  not avail finalid_unid_organ
    then do:
        assign p_cod_return = "338".
    end /* if */.
    else do:
        assign p_cod_return = "OK" /*l_ok*/ .
    end /* else */.
END PROCEDURE. /* pi_validar_finalid_unid_organ */
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
** Procedure Interna.....: pi_achar_cotac_indic_econ
** Descricao.............: pi_achar_cotac_indic_econ
** Criado por............: vladimir
** Criado em.............: // 
** Alterado por..........: fut1309_4
** Alterado em...........: 08/02/2006 16:12:34
*****************************************************************************/
PROCEDURE pi_achar_cotac_indic_econ:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_indic_econ_base
        as character
        format "x(8)"
        no-undo.
    def Input param p_cod_indic_econ_idx
        as character
        format "x(8)"
        no-undo.
    def Input param p_dat_transacao
        as date
        format "99/99/9999"
        no-undo.
    def Input param p_ind_tip_cotac_parid
        as character
        format "X(09)"
        no-undo.
    def output param p_dat_cotac_indic_econ
        as date
        format "99/99/9999"
        no-undo.
    def output param p_val_cotac_indic_econ
        as decimal
        format ">>>>,>>9.9999999999"
        decimals 10
        no-undo.
    def output param p_cod_return
        as character
        format "x(40)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_dat_cotac_mes
        as date
        format "99/99/9999":U
        no-undo.
    def var v_log_indic
        as logical
        format "Sim/N∆o"
        initial no
        no-undo.
    def var v_cod_indic_econ_orig            as character       no-undo. /*local*/
    def var v_val_cotac_indic_econ_base      as decimal         no-undo. /*local*/
    def var v_val_cotac_indic_econ_idx       as decimal         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    /* alteraá∆o sob demanda da atividade 148.681*/
    release cotac_parid.

    if  p_cod_indic_econ_base = p_cod_indic_econ_idx
    then do:
        /* **
         Quando a Base e o ÷ndice forem iguais, significa que a cotaá∆o pode ser percentual,
         portanto n∆o basta apenas retornar 1 e deve ser feita toda a pesquisa abaixo para
         encontrar a taxa da moeda no dia informado.
         Exemplo: D¢lar - D¢lar, poder°amos retornar 1
                  ANBID - ANBID, devemos retornar a taxa do dia.
        ***/
        find indic_econ no-lock
             where indic_econ.cod_indic_econ  = p_cod_indic_econ_base
               and indic_econ.dat_inic_valid <= p_dat_transacao
               and indic_econ.dat_fim_valid  >  p_dat_transacao
             no-error.
        if  avail indic_econ then do:
            if  indic_econ.ind_tip_cotac = "Valor" /*l_valor*/  then do:
                assign p_dat_cotac_indic_econ = p_dat_transacao
                       p_val_cotac_indic_econ = 1
                       p_cod_return           = "OK" /*l_ok*/ .
            end.
            else do:
                find cotac_parid no-lock
                     where cotac_parid.cod_indic_econ_base = p_cod_indic_econ_base
                       and cotac_parid.cod_indic_econ_idx = p_cod_indic_econ_idx
                       and cotac_parid.dat_cotac_indic_econ = p_dat_transacao
                       and cotac_parid.ind_tip_cotac_parid = p_ind_tip_cotac_parid
    &if "{&emsuni_version}" >= "5.01" &then
                     use-index ctcprd_id
    &endif
                      /*cl_acha_cotac of cotac_parid*/ no-error.
                if  not avail cotac_parid
                then do:
                    find parid_indic_econ no-lock
                         where parid_indic_econ.cod_indic_econ_base = p_cod_indic_econ_base
                           and parid_indic_econ.cod_indic_econ_idx = p_cod_indic_econ_idx
    &if "{&emsuni_version}" >= "5.01" &then
                         use-index prdndccn_id
    &endif
                          /*cl_acha_parid_param of parid_indic_econ*/ no-error.
                    /* block: */
                    case parid_indic_econ.ind_criter_busca:
                        when "Anterior" /*l_anterior*/ then find prev cotac_parid no-lock
                              where cotac_parid.cod_indic_econ_base = p_cod_indic_econ_base
                                and cotac_parid.cod_indic_econ_idx = p_cod_indic_econ_idx
                                and cotac_parid.dat_cotac_indic_econ < p_dat_transacao
                                and cotac_parid.ind_tip_cotac_parid = p_ind_tip_cotac_parid
                                and cotac_parid.val_cotac_indic_econ <> 0.0
    &if "{&emsuni_version}" >= "5.01" &then
                              use-index ctcprd_id
    &endif
                               /*cl_acha_cotac_anterior of cotac_parid*/ no-error.
                        when "Pr¢ximo" /*l_proximo*/ then  find next cotac_parid no-lock
                               where cotac_parid.cod_indic_econ_base = p_cod_indic_econ_base
                                 and cotac_parid.cod_indic_econ_idx = p_cod_indic_econ_idx
                                 and cotac_parid.dat_cotac_indic_econ > p_dat_transacao
                                 and cotac_parid.ind_tip_cotac_parid = p_ind_tip_cotac_parid
                                 and cotac_parid.val_cotac_indic_econ <> 0.0
    &if "{&emsuni_version}" >= "5.01" &then
                               use-index ctcprd_id
    &endif
                                /*cl_acha_cotac_posterior of cotac_parid*/ no-error.
                    end /* case block */.
                    if  not avail cotac_parid
                    then do:
                        assign p_cod_return = "358"                   + "," +
                                              p_cod_indic_econ_base   + "," +
                                              p_cod_indic_econ_idx    + "," +
                                              string(p_dat_transacao) + "," +
                                              p_ind_tip_cotac_parid.
                    end /* if */.
                    else do:
                        assign p_dat_cotac_indic_econ = cotac_parid.dat_cotac_indic_econ
                               p_val_cotac_indic_econ = cotac_parid.val_cotac_indic_econ
                               p_cod_return           = "OK" /*l_ok*/ .
                    end /* else */.
                end /* if */.
                else do:
                    assign p_dat_cotac_indic_econ = cotac_parid.dat_cotac_indic_econ
                           p_val_cotac_indic_econ = cotac_parid.val_cotac_indic_econ
                           p_cod_return           = "OK" /*l_ok*/ .
                end /* else */.
            end.
        end.
        else do:
            assign p_cod_return = "335".
        end.
    end /* if */.
    else do:
        find parid_indic_econ no-lock
             where parid_indic_econ.cod_indic_econ_base = p_cod_indic_econ_base
               and parid_indic_econ.cod_indic_econ_idx = p_cod_indic_econ_idx
             use-index prdndccn_id no-error.
        if  avail parid_indic_econ
        then do:


            /* Begin_Include: i_verifica_cotac_parid */
            /* verifica as cotacoes da moeda p_cod_indic_econ_base para p_cod_indic_econ_idx 
              cadastrada na base, de acordo com a periodicidade da cotacao (obtida na 
              parid_indic_econ, que deve estar avail)*/

            /* period_block: */
            case parid_indic_econ.ind_periodic_cotac:
                when "Di†ria" /*l_diaria*/ then
                    diaria_block:
                    do:
                        find cotac_parid no-lock
                            where cotac_parid.cod_indic_econ_base  = p_cod_indic_econ_base
                              and cotac_parid.cod_indic_econ_idx   = p_cod_indic_econ_idx
                              and cotac_parid.dat_cotac_indic_econ = p_dat_transacao
                              and cotac_parid.ind_tip_cotac_parid  = p_ind_tip_cotac_parid
                            use-index ctcprd_id no-error.
                        if  not avail cotac_parid or cotac_parid.val_cotac_indic_econ = 0
                        then do:
                            find parid_indic_econ no-lock
                                where parid_indic_econ.cod_indic_econ_base = p_cod_indic_econ_base
                                  and parid_indic_econ.cod_indic_econ_idx  = p_cod_indic_econ_idx
                                use-index prdndccn_id no-error.
                            /* block: */
                            case parid_indic_econ.ind_criter_busca:
                                when "Anterior" /*l_anterior*/ then 
                                    find prev cotac_parid no-lock
                                        where cotac_parid.cod_indic_econ_base  = p_cod_indic_econ_base
                                          and cotac_parid.cod_indic_econ_idx   = p_cod_indic_econ_idx
                                          and cotac_parid.dat_cotac_indic_econ < p_dat_transacao
                                          and cotac_parid.ind_tip_cotac_parid  = p_ind_tip_cotac_parid
                                          and cotac_parid.val_cotac_indic_econ <> 0.0
                                          &if '{&emsuni_version}' >= '5.01' &then
                                          use-index ctcprd_id
                                          &endif
                                          no-error.
                                when "Pr¢ximo" /*l_proximo*/ then  
                                    find next cotac_parid no-lock
                                        where cotac_parid.cod_indic_econ_base  = p_cod_indic_econ_base
                                          and cotac_parid.cod_indic_econ_idx   = p_cod_indic_econ_idx
                                          and cotac_parid.dat_cotac_indic_econ > p_dat_transacao
                                          and cotac_parid.ind_tip_cotac_parid  = p_ind_tip_cotac_parid
                                          and cotac_parid.val_cotac_indic_econ <> 0.0
                                          &if '{&emsuni_version}' >= '5.01' &then
                                          use-index ctcprd_id
                                          &endif
                                          no-error.
                            end /* case block */.
                        end /* if */.
                    end /* do diaria_block */.
                when "Mensal" /*l_mensal*/ then
                    mensal_block:
                    do:
                        assign v_dat_cotac_mes = date(month(p_dat_transacao), 1, year(p_dat_transacao))
                               &if yes = yes &then 
                               v_log_indic     = yes
                               &endif .
                        find cotac_parid no-lock
                            where cotac_parid.cod_indic_econ_base  = p_cod_indic_econ_base
                              and cotac_parid.cod_indic_econ_idx   = p_cod_indic_econ_idx
                              and cotac_parid.dat_cotac_indic_econ = v_dat_cotac_mes
                              and cotac_parid.ind_tip_cotac_parid  = p_ind_tip_cotac_parid
                            use-index ctcprd_id no-error.
                        if  not avail cotac_parid or cotac_parid.val_cotac_indic_econ = 0
                        then do:
                            /* block: */
                            case parid_indic_econ.ind_criter_busca:
                                when "Anterior" /*l_anterior*/ then
                                    find prev cotac_parid no-lock
                                        where cotac_parid.cod_indic_econ_base  = p_cod_indic_econ_base
                                          and cotac_parid.cod_indic_econ_idx   = p_cod_indic_econ_idx
                                          and cotac_parid.dat_cotac_indic_econ < v_dat_cotac_mes
                                          and cotac_parid.ind_tip_cotac_parid  = p_ind_tip_cotac_parid
                                          and cotac_parid.val_cotac_indic_econ <> 0.0
                                        use-index ctcprd_id no-error.
                                when "Pr¢ximo" /*l_proximo*/ then
                                    find next cotac_parid no-lock
                                        where cotac_parid.cod_indic_econ_base  = p_cod_indic_econ_base
                                          and cotac_parid.cod_indic_econ_idx   = p_cod_indic_econ_idx
                                          and cotac_parid.dat_cotac_indic_econ > v_dat_cotac_mes
                                          and cotac_parid.ind_tip_cotac_parid  = p_ind_tip_cotac_parid
                                          and cotac_parid.val_cotac_indic_econ <> 0.0
                                        use-index ctcprd_id no-error.
                            end /* case block */.
                        end /* if */.
                    end /* do mensal_block */.
                when "Bimestral" /*l_bimestral*/ then
                    bimestral_block:
                    do:
                    end /* do bimestral_block */.
                when "Trimestral" /*l_trimestral*/ then
                    trimestral_block:
                    do:
                    end /* do trimestral_block */.
                when "Quadrimestral" /*l_quadrimestral*/ then
                    quadrimestral_block:
                    do:
                    end /* do quadrimestral_block */.
                when "Semestral" /*l_semestral*/ then
                    semestral_block:
                    do:
                    end /* do semestral_block */.
                when "Anual" /*l_anual*/ then
                    anual_block:
                    do:
                    end /* do anual_block */.
            end /* case period_block */.
            /* End_Include: i_verifica_cotac_parid */


            if  parid_indic_econ.ind_orig_cotac_parid = "Outra Moeda" /*l_outra_moeda*/  and
                 parid_indic_econ.cod_finalid_econ_orig_cotac <> "" and
                 (not avail cotac_parid or cotac_parid.val_cotac_indic_econ = 0)
            then do:
                /* Cotaá∆o Ponte */
                run pi_retornar_indic_econ_finalid (Input parid_indic_econ.cod_finalid_econ_orig_cotac,
                                                    Input p_dat_transacao,
                                                    output v_cod_indic_econ_orig) /*pi_retornar_indic_econ_finalid*/.
                find parid_indic_econ no-lock
                    where parid_indic_econ.cod_indic_econ_base = v_cod_indic_econ_orig
                    and parid_indic_econ.cod_indic_econ_idx = p_cod_indic_econ_base
                    use-index prdndccn_id no-error.
                run pi_achar_cotac_indic_econ_2 (Input v_cod_indic_econ_orig,
                                                 Input p_cod_indic_econ_base,
                                                 Input p_dat_transacao,
                                                 Input p_ind_tip_cotac_parid,
                                                 Input p_cod_indic_econ_base,
                                                 Input p_cod_indic_econ_idx) /*pi_achar_cotac_indic_econ_2*/.

                if  avail cotac_parid and cotac_parid.val_cotac_indic_econ <> 0
                then do:
                    assign v_val_cotac_indic_econ_base = cotac_parid.val_cotac_indic_econ.
                    find parid_indic_econ no-lock
                        where parid_indic_econ.cod_indic_econ_base = v_cod_indic_econ_orig
                        and parid_indic_econ.cod_indic_econ_idx = p_cod_indic_econ_idx
                        use-index prdndccn_id no-error.
                    run pi_achar_cotac_indic_econ_2 (Input v_cod_indic_econ_orig,
                                                     Input p_cod_indic_econ_idx,
                                                     Input p_dat_transacao,
                                                     Input p_ind_tip_cotac_parid,
                                                     Input p_cod_indic_econ_base,
                                                     Input p_cod_indic_econ_idx) /*pi_achar_cotac_indic_econ_2*/.

                    if  avail cotac_parid and cotac_parid.val_cotac_indic_econ <> 0
                    then do:
                        assign v_val_cotac_indic_econ_idx = cotac_parid.val_cotac_indic_econ
                               p_val_cotac_indic_econ = v_val_cotac_indic_econ_idx / v_val_cotac_indic_econ_base
                               p_dat_cotac_indic_econ = cotac_parid.dat_cotac_indic_econ
                               p_cod_return = "OK" /*l_ok*/ .
                        return.
                    end /* if */.
                end /* if */.
            end /* if */.
            if  parid_indic_econ.ind_orig_cotac_parid = "Inversa" /*l_inversa*/  and
                 (not avail cotac_parid or cotac_parid.val_cotac_indic_econ = 0)
            then do:
                find parid_indic_econ no-lock
                    where parid_indic_econ.cod_indic_econ_base = p_cod_indic_econ_idx
                    and parid_indic_econ.cod_indic_econ_idx = p_cod_indic_econ_base
                    use-index prdndccn_id no-error.
                run pi_achar_cotac_indic_econ_2 (Input p_cod_indic_econ_idx,
                                                 Input p_cod_indic_econ_base,
                                                 Input p_dat_transacao,
                                                 Input p_ind_tip_cotac_parid,
                                                 Input p_cod_indic_econ_base,
                                                 Input p_cod_indic_econ_idx) /*pi_achar_cotac_indic_econ_2*/.

                if  avail cotac_parid and cotac_parid.val_cotac_indic_econ <> 0
                then do:
                    assign p_dat_cotac_indic_econ = cotac_parid.dat_cotac_indic_econ
                           p_val_cotac_indic_econ = 1 / cotac_parid.val_cotac_indic_econ
                           p_cod_return = "OK" /*l_ok*/ .
                    return.
                end /* if */.
            end /* if */.
        end /* if */.
        if v_log_indic = yes then do:
           if  not avail cotac_parid or cotac_parid.val_cotac_indic_econ = 0
           then do:
               assign p_cod_return = "358"                 + "," +
                      p_cod_indic_econ_base   + "," +
                      p_cod_indic_econ_idx    + "," +
                      string(v_dat_cotac_mes) + "," +
                      p_ind_tip_cotac_parid.
           end /* if */.
           else do:
               assign p_dat_cotac_indic_econ = cotac_parid.dat_cotac_indic_econ
                      p_val_cotac_indic_econ = cotac_parid.val_cotac_indic_econ
                      p_cod_return           = "OK" /*l_ok*/ .
           end /* else */.
        end.
        else do:   
           if  not avail cotac_parid or cotac_parid.val_cotac_indic_econ = 0
           then do:
               assign p_cod_return = "358"                 + "," +
                      p_cod_indic_econ_base   + "," +
                      p_cod_indic_econ_idx    + "," +
                      string(p_dat_transacao) + "," +
                      p_ind_tip_cotac_parid.
           end /* if */.
           else do:
               assign p_dat_cotac_indic_econ = cotac_parid.dat_cotac_indic_econ
                      p_val_cotac_indic_econ = cotac_parid.val_cotac_indic_econ
                      p_cod_return           = "OK" /*l_ok*/ .
           end /* else */.
        end.
        assign v_log_indic = no.
    end /* else */.
END PROCEDURE. /* pi_achar_cotac_indic_econ */
/*****************************************************************************
** Procedure Interna.....: pi_achar_cotac_indic_econ_2
** Descricao.............: pi_achar_cotac_indic_econ_2
** Criado por............: src531
** Criado em.............: 29/07/2003 11:10:10
** Alterado por..........: bre17752
** Alterado em...........: 30/07/2003 12:46:24
*****************************************************************************/
PROCEDURE pi_achar_cotac_indic_econ_2:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_param_1
        as character
        format "x(8)"
        no-undo.
    def Input param p_cod_param_2
        as character
        format "x(50)"
        no-undo.
    def Input param p_dat_transacao
        as date
        format "99/99/9999"
        no-undo.
    def Input param p_ind_tip_cotac_parid
        as character
        format "X(09)"
        no-undo.
    def Input param p_cod_indic_econ_base
        as character
        format "x(8)"
        no-undo.
    def Input param p_cod_indic_econ_idx
        as character
        format "x(8)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_dat_cotac_mes                  as date            no-undo. /*local*/


    /************************** Variable Definition End *************************/

    /* period_block: */
    case parid_indic_econ.ind_periodic_cotac:
        when "Di†ria" /*l_diaria*/ then
            diaria_block:
            do:
                find cotac_parid no-lock
                     where cotac_parid.cod_indic_econ_base = p_cod_param_1
                       and cotac_parid.cod_indic_econ_idx = p_cod_param_2
                       and cotac_parid.dat_cotac_indic_econ = p_dat_transacao
                       and cotac_parid.ind_tip_cotac_parid = p_ind_tip_cotac_parid
                     use-index ctcprd_id no-error.
                if  not avail cotac_parid or cotac_parid.val_cotac_indic_econ = 0
                then do:
                    find parid_indic_econ no-lock
                         where parid_indic_econ.cod_indic_econ_base = p_cod_param_1
                           and parid_indic_econ.cod_indic_econ_idx = p_cod_param_2
                         use-index prdndccn_id no-error.
                    /* block: */
                    case parid_indic_econ.ind_criter_busca:
                        when "Anterior" /*l_anterior*/ then find prev cotac_parid no-lock
                              where cotac_parid.cod_indic_econ_base = p_cod_indic_econ_base
                                and cotac_parid.cod_indic_econ_idx = p_cod_indic_econ_idx
                                and cotac_parid.dat_cotac_indic_econ < p_dat_transacao
                                and cotac_parid.ind_tip_cotac_parid = p_ind_tip_cotac_parid
                                and cotac_parid.val_cotac_indic_econ <> 0.0
    &if "{&emsuni_version}" >= "5.01" &then
                              use-index ctcprd_id
    &endif
                               /*cl_acha_cotac_anterior of cotac_parid*/ no-error.
                        when "Pr¢ximo" /*l_proximo*/ then  find next cotac_parid no-lock
                               where cotac_parid.cod_indic_econ_base = p_cod_indic_econ_base
                                 and cotac_parid.cod_indic_econ_idx = p_cod_indic_econ_idx
                                 and cotac_parid.dat_cotac_indic_econ > p_dat_transacao
                                 and cotac_parid.ind_tip_cotac_parid = p_ind_tip_cotac_parid
                                 and cotac_parid.val_cotac_indic_econ <> 0.0
    &if "{&emsuni_version}" >= "5.01" &then
                               use-index ctcprd_id
    &endif
                                /*cl_acha_cotac_posterior of cotac_parid*/ no-error.
                    end /* case block */.
                end /* if */.
            end /* do diaria_block */.
        when "Mensal" /*l_mensal*/ then
            mensal_block:
            do:
                assign v_dat_cotac_mes = date(month(p_dat_transacao), 1, year(p_dat_transacao)).
                find cotac_parid no-lock
                     where cotac_parid.cod_indic_econ_base = p_cod_param_1
                       and cotac_parid.cod_indic_econ_idx = p_cod_param_2
                       and cotac_parid.dat_cotac_indic_econ = v_dat_cotac_mes
                       and cotac_parid.ind_tip_cotac_parid = p_ind_tip_cotac_parid
                     use-index ctcprd_id no-error.
                if  not avail cotac_parid or cotac_parid.val_cotac_indic_econ = 0
                then do:
                    /* block: */
                    case parid_indic_econ.ind_criter_busca:
                        when "Anterior" /*l_anterior*/ then
                        find prev cotac_parid no-lock
                                           where cotac_parid.cod_indic_econ_base = p_cod_param_1
                                             and cotac_parid.cod_indic_econ_idx = p_cod_param_2
                                             and cotac_parid.dat_cotac_indic_econ < v_dat_cotac_mes
                                             and cotac_parid.ind_tip_cotac_parid = p_ind_tip_cotac_parid
                                             and cotac_parid.val_cotac_indic_econ <> 0.0
                                           use-index ctcprd_id no-error.
                        when "Pr¢ximo" /*l_proximo*/ then
                        find next cotac_parid no-lock
                                           where cotac_parid.cod_indic_econ_base = p_cod_param_1
                                             and cotac_parid.cod_indic_econ_idx = p_cod_param_2
                                             and cotac_parid.dat_cotac_indic_econ > v_dat_cotac_mes
                                             and cotac_parid.ind_tip_cotac_parid = p_ind_tip_cotac_parid
                                             and cotac_parid.val_cotac_indic_econ <> 0.0
                                           use-index ctcprd_id no-error.
                    end /* case block */.
                end /* if */.
            end /* do mensal_block */.
        when "Bimestral" /*l_bimestral*/ then
            bimestral_block:
            do:
            end /* do bimestral_block */.
        when "Trimestral" /*l_trimestral*/ then
            trimestral_block:
            do:
            end /* do trimestral_block */.
        when "Quadrimestral" /*l_quadrimestral*/ then
            quadrimestral_block:
            do:
            end /* do quadrimestral_block */.
        when "Semestral" /*l_semestral*/ then
            semestral_block:
            do:
            end /* do semestral_block */.
        when "Anual" /*l_anual*/ then
            anual_block:
            do:
            end /* do anual_block */.
    end /* case period_block */.
END PROCEDURE. /* pi_achar_cotac_indic_econ_2 */
/*****************************************************************************
** Procedure Interna.....: pi_cria_tt_compl_impto_retid_ap
** Descricao.............: pi_cria_tt_compl_impto_retid_ap
** Criado por............: bre18490
** Criado em.............: 23/04/1999 10:05:29
** Alterado por..........: fut40574
** Alterado em...........: 16/07/2008 13:49:39
*****************************************************************************/
PROCEDURE pi_cria_tt_compl_impto_retid_ap:

    /************************* Variable Definition Begin ************************/

    def var v_log_convtdo                    as logical         no-undo. /*local*/
    def var v_val_rendto_tribut              as decimal         no-undo. /*local*/


    /************************** Variable Definition End *************************/


    /* Begin_Include: i_cria_tt_compl_impto_retid_ap */
    find fornecedor
        where fornecedor.cod_empresa    = v_cod_empres_usuar
        and   fornecedor.cdn_fornecedor = compl_impto_retid_ap.cdn_fornecedor
        no-lock no-error.

    if classif_impto.ind_tip_retenc_impto = "Tabela Progressiva" /*l_tabela_progressiva*/  
    and fornecedor.num_pessoa modulo 2 = 0 then do:
        find first tt_compl_impto_retid_ap_menor no-lock
             where tt_compl_impto_retid_ap_menor.tta_cod_estab     = compl_impto_retid_ap.cod_estab
               and tt_compl_impto_retid_ap_menor.tta_num_id_tit_ap = compl_impto_retid_ap.num_id_tit_ap no-error.
        if avail tt_compl_impto_retid_ap_menor then
            assign v_val_rendto_tribut = tt_compl_impto_retid_ap_menor.tta_val_rendto_tribut.
        else do:
            &if '{&emsfin_version}' > '5.05' &then
                find first relacto_acum_pagto no-lock
                     where relacto_acum_pagto.cod_estab           = compl_impto_retid_ap.cod_estab
                       and relacto_acum_pagto.num_id_movto_tit_ap = compl_impto_retid_ap.num_id_movto_tit_ap_pai
                       and relacto_acum_pagto.cod_pais            = compl_impto_retid_ap.cod_pais
                       and relacto_acum_pagto.cod_unid_federac    = compl_impto_retid_ap.cod_unid_federac
                       and relacto_acum_pagto.cod_imposto         = compl_impto_retid_ap.cod_imposto
                       and relacto_acum_pagto.cod_classif_impto   = compl_impto_retid_ap.cod_classif_impto no-error.
                if avail relacto_acum_pagto then
                    assign v_val_rendto_tribut = relacto_acum_pagto.val_dif_impto.
                else
                    assign v_val_rendto_tribut = compl_impto_retid_ap.val_rendto_tribut.
            &else
                find first tab_livre_emsfin no-lock
                     where tab_livre_emsfin.cod_modul_dtsul   = "APB" /*l_apb*/  
                       and tab_livre_emsfin.cod_tab_dic_dtsul = 'relacto_acum_pagto'
                       and entry(1, tab_livre_emsfin.cod_compon_2_idx_tab, chr(10)) = compl_impto_retid_ap.cod_estab
                       and tab_livre_emsfin.cod_compon_1_idx_tab                    = string(compl_impto_retid_ap.num_id_movto_tit_ap_pai) 
                       and entry(3, tab_livre_emsfin.cod_livre_1, chr(10)) = compl_impto_retid_ap.cod_pais
                       and entry(4, tab_livre_emsfin.cod_livre_1, chr(10)) = compl_impto_retid_ap.cod_unid_federac
                       and entry(5, tab_livre_emsfin.cod_livre_1, chr(10)) = compl_impto_retid_ap.cod_imposto
                       and entry(6, tab_livre_emsfin.cod_livre_1, chr(10)) = compl_impto_retid_ap.cod_classif_impto no-error.
                if avail tab_livre_emsfin then
                    assign v_val_rendto_tribut = dec(GetEntryField(9, tab_livre_emsfin.cod_livre_2, chr(10))) / 100.
                else
                    assign v_val_rendto_tribut = compl_impto_retid_ap.val_rendto_tribut.
            &endif
        end.   
    end.
    else
        assign v_val_rendto_tribut = compl_impto_retid_ap.val_rendto_tribut.
    /* End_Include: i_cria_tt_compl_impto_retid_ap */

    create tt_compl_impto_retid_ap.
    assign tt_compl_impto_retid_ap.tta_cod_pais             = compl_impto_retid_ap.cod_pais
           tt_compl_impto_retid_ap.tta_cod_unid_federac     = compl_impto_retid_ap.cod_unid_federac
           tt_compl_impto_retid_ap.tta_cod_classif_impto    = compl_impto_retid_ap.cod_classif_impto
           tt_compl_impto_retid_ap.tta_cod_imposto          = compl_impto_retid_ap.cod_imposto
           tt_compl_impto_retid_ap.tta_dat_vencto_tit_ap    = compl_impto_retid_ap.dat_vencto_tit_ap
           tt_compl_impto_retid_ap.tta_cod_indic_econ_impto = compl_impto_retid_ap.cod_indic_econ_impto
           tt_compl_impto_retid_ap.tta_val_aliq_impto       = compl_impto_retid_ap.val_aliq_impto
           tt_compl_impto_retid_ap.tta_val_rendto_tribut    = v_val_rendto_tribut
           tt_compl_impto_retid_ap.ttv_cod_indic_econ_rdt   = compl_impto_retid_ap.cod_indic_econ_orig
           tt_compl_impto_retid_ap.tta_cdn_fornecedor       = compl_impto_retid_ap.cdn_fornecedor
           tt_compl_impto_retid_ap.ttv_cdn_fornecedor       = compl_impto_retid_ap.cdn_fornec_favorec.

    if avail tit_ap then do:
        assign tt_compl_impto_retid_ap.ttv_cod_espec_docto      = tit_ap.cod_espec_docto
               tt_compl_impto_retid_ap.ttv_cod_ser_docto        = tit_ap.cod_ser_docto
               tt_compl_impto_retid_ap.ttv_cod_parcela          = tit_ap.cod_parcela
               tt_compl_impto_retid_ap.ttv_cod_tit_ap           = tit_ap.cod_tit_ap
               tt_compl_impto_retid_ap.tta_cod_estab            = tit_ap.cod_estab
               tt_compl_impto_retid_ap.ttv_dat_emis             = tit_ap.dat_emis_docto
               tt_compl_impto_retid_ap.ttv_dat_recolhto         = tit_ap.dat_vencto_tit_ap
               tt_compl_impto_retid_ap.ttv_val_origin_tit_ap_2  = tit_ap.val_origin_tit_ap
               tt_compl_impto_retid_ap.ttv_val_imposto_rel      = tit_ap.val_sdo_tit_ap.       
    end.

    if  v_ind_classif_rpt_impto_retid_ap = "Por Classificaá∆o do Imposto" /*l_por_classificacao_do_imposto*/ 
    then do:
        if  v_log_por_estab = yes
        then do:
            assign tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[1] = string(compl_impto_retid_ap.cod_estab, &IF '{&emsfin_version}' >= '5.01' AND '{&emsfin_version}' <= '5.07' &THEN "x(3)":U &ELSE "x(5)":U &ENDIF) +
                                                                      string(compl_impto_retid_ap.cod_pais, "x(3)":U)  +
                                                                      string(compl_impto_retid_ap.cod_unid_federac, "x(3)":U) +
                                                                      string(compl_impto_retid_ap.cod_imposto, "x(5)":U) +
                                                                      string(compl_impto_retid_ap.cod_classif_impto, "x(05)":U). 
        end /* if */.
        else do:
            assign tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[1] = string(compl_impto_retid_ap.cod_pais, "x(3)":U)  +
                                                                      string(compl_impto_retid_ap.cod_unid_federac, "x(3)":U) +
                                                                      string(compl_impto_retid_ap.cod_imposto, "x(5)":U) + 
                                                                      string(compl_impto_retid_ap.cod_classif_impto, "x(05)":U). 
        end /* else */.                                                                  
        assign tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[2] = string(compl_impto_retid_ap.cdn_fornecedor, ">>>,>>>,>>9":U)
               tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[3] = string(compl_impto_retid_ap.cdn_fornec_favorec, ">>>,>>>,>>9":U)
               tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[4] = string(tit_ap.dat_vencto_tit_ap, "99/99/9999":U).
    end /* if */.           

    if  v_ind_classif_rpt_impto_retid_ap = "Por Estab/Fornecedor Favorecido" /*l_por_estabfornec_favorec*/ 
    then do:
        if  v_log_por_estab = yes
        then do:
            assign tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[1] = string(compl_impto_retid_ap.cod_estab, &IF '{&emsfin_version}' >= '5.01' AND '{&emsfin_version}' <= '5.07' &THEN "x(3)":U &ELSE "x(5)":U &ENDIF) + 
                                                                      string(compl_impto_retid_ap.cdn_fornec_favorec, ">>>,>>>,>>9":U). 
        end /* if */.
        else do:
            assign tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[1] = string(compl_impto_retid_ap.cdn_fornec_favorec, ">>>,>>>,>>9":U).
        end /* else */.                                                              
        assign tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[2] = string(compl_impto_retid_ap.cdn_fornecedor, ">>>,>>>,>>9":U)
               tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[3] = string(compl_impto_retid_ap.cod_pais, "x(3)":U)  +
                                                                  string(compl_impto_retid_ap.cod_unid_federac, "x(3)":U) +
                                                                  string(compl_impto_retid_ap.cod_imposto, "x(5)":U) +
                                                                  string(compl_impto_retid_ap.cod_classif_impto, "x(05)":U)                                                              
               tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[4] = string(tit_ap.dat_vencto_tit_ap, "99/99/9999":U).

    end /* if */.                                 

    if  v_ind_classif_rpt_impto_retid_ap = "Por Estab/Fornecedor Origem" /*l_por_estabfornec_origem*/ 
    then do:
        if  v_log_por_estab = yes
        then do:
            assign tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[1] = string(compl_impto_retid_ap.cod_estab, &IF '{&emsfin_version}' >= '5.01' AND '{&emsfin_version}' <= '5.07' &THEN "x(3)":U &ELSE "x(5)":U &ENDIF) +
                                                                      string(compl_impto_retid_ap.cdn_fornecedor).
        end /* if */.
        else do:
            assign tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[1] = string(compl_impto_retid_ap.cdn_fornecedor).
        end /* else */.                                                              
        assign tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[2] = string(compl_impto_retid_ap.cod_pais, "x(3)":U)  +
                                                                  string(compl_impto_retid_ap.cod_unid_federac, "x(3)":U) +
                                                                  string(compl_impto_retid_ap.cod_imposto, "x(5)":U) +
                                                                  string(compl_impto_retid_ap.cod_classif_impto, "x(05)":U)
               tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[3] = string(compl_impto_retid_ap.cdn_fornec_favorec, ">>>,>>>,>>9":U)
               tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[4] = string(tit_ap.dat_vencto_tit_ap, "99/99/9999":U).           

    end /* if */.                      

    if  v_ind_classif_rpt_impto_retid_ap = "Por Fornecedor Origem/Por Estab" /*l_por_fornecedor_origempor_estab*/ 
    then do:
        assign tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[1] = string(compl_impto_retid_ap.cdn_fornecedor).
        assign tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[2] = string(compl_impto_retid_ap.cod_estab, &IF '{&emsfin_version}' >= '5.01' AND '{&emsfin_version}' <= '5.07' &THEN "x(3)":U &ELSE "x(5)":U &ENDIF) +
                                                                  string(compl_impto_retid_ap.cod_pais, "x(3)":U)  +
                                                                  string(compl_impto_retid_ap.cod_unid_federac, "x(3)":U) +
                                                                  string(compl_impto_retid_ap.cod_imposto, "x(5)":U) +
                                                                  string(compl_impto_retid_ap.cod_classif_impto, "x(05)":U)
               tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[3] = string(compl_impto_retid_ap.cdn_fornec_favorec, ">>>,>>>,>>9":U)
               tt_compl_impto_retid_ap.ttv_cod_dwb_field_rpt[4] = string(tit_ap.dat_vencto_tit_ap, "99/99/9999":U).           

    end /* if */.                      

    /* Busca fornecedor origem */
    find fornecedor
        where fornecedor.cod_empresa    = v_cod_empres_usuar
        and   fornecedor.cdn_fornecedor = tt_compl_impto_retid_ap.tta_cdn_fornecedor
        no-lock no-error.
    find b_pais
        where b_pais.cod_pais = fornecedor.cod_pais no-lock no-error.
    if  fornecedor.num_pessoa modulo 2 <> 0
    then do:
        assign tt_compl_impto_retid_ap.tta_nom_pessoa = fornecedor.nom_pessoa
               tt_compl_impto_retid_ap.tta_cod_id_feder   = string(fornecedor.cod_id_feder, b_pais.cod_format_id_feder_jurid).
    end /* if */.
    else do:
        assign tt_compl_impto_retid_ap.tta_nom_pessoa = fornecedor.nom_pessoa
               tt_compl_impto_retid_ap.tta_cod_id_feder   = string(fornecedor.cod_id_feder, b_pais.cod_format_id_feder_fisic).
    end /* else */.

    /* busca fornecedor favorecido */
    find fornecedor
        where fornecedor.cod_empresa    = v_cod_empres_usuar
        and   fornecedor.cdn_fornecedor = tt_compl_impto_retid_ap.ttv_cdn_fornecedor
        no-lock no-error.
    find b_pais
        where b_pais.cod_pais = fornecedor.cod_pais no-lock no-error.
    if  fornecedor.num_pessoa modulo 2 <> 0
    then do:
        assign tt_compl_impto_retid_ap.ttv_nom_pessoa   = fornecedor.nom_pessoa
               tt_compl_impto_retid_ap.ttv_cod_id_feder = string(fornecedor.cod_id_feder, b_pais.cod_format_id_feder_jurid).
    end /* if */.
    else do:
        assign tt_compl_impto_retid_ap.ttv_nom_pessoa   = fornecedor.nom_pessoa
               tt_compl_impto_retid_ap.ttv_cod_id_feder = string(fornecedor.cod_id_feder, b_pais.cod_format_id_feder_fisic).
    end /* else */.

    find indic_econ no-lock
         where indic_econ.cod_indic_econ = compl_impto_retid_ap.cod_indic_econ_impto
          no-error.       

    assign v_log_convtdo = no.
    run pi_converter_indic_econ_finalid (Input tit_ap.cod_indic_econ,
                                         Input tit_ap.cod_empresa,
                                         Input tit_ap.dat_vencto_tit_ap,
                                         Input tit_ap.val_sdo_tit_ap,
                                         Input b_pais.cod_finalid_econ_pais,
                                         output v_cod_return) /*pi_converter_indic_econ_finalid*/.

    /* case_block: */
    case entry(1,v_cod_return):
        when "241" then
        code_block:
        do:
            assign v_des_msg_relat = "Indicador Econìmico Inexistente !" /*241*/ + chr(10) + "Verifique se o indicador econìmico existe na tabela de indicadores econìmicos" /*241*/.
        end /* do code_block */.
        when "338" then
        code_block:
        do:
            assign v_des_msg_relat = "Finalidade n∆o liberada para a Unidade Organizacional !" /*338*/ + chr(10) + "A finalidade econìmica associada ao indicador econìmico informado, n∆o est† habilitada para a unidade organizacional informada." /*338*/.
        end /* do code_block */.
        when "358" then
        code_block:
        do:
            assign v_des_msg_relat = substitute("Cotaá∆o entre &1 e &2 n∆o existe para dia &3 !" /*784*/, entry(2, v_cod_return),entry(3, v_cod_return),entry(4, v_cod_return)).
        end /* do code_block */.
        when "782" then
        code_block:
        do:
            assign v_des_msg_relat = "Finalidade econìmica n∆o relacionada para o Indic Econìmico !" /*782*/ + chr(10) + "A finalidade econìmica &1 n∆o possui relaá∆o com o indicador econìmico &2, ou seja, n∆o existe nenhuma composiá∆o de finalidade que os une, na data de transaá∆o &3." /*782*/.
        end /* do code_block */.
        when "1199" then
        code_block:
        do:
            assign v_des_msg_relat = "Indicador Econìmico n∆o habilitado na Data Transaá∆o &1 !" /*1199*/ + chr(10) + "A Data de Transaá∆o est† fora da faixa de validade do Indicador Econìmico." /*1199*/.
        end /* do code_block */.
        when "1200" then
        code_block:
        do:
            assign v_des_msg_relat = "ParÉmetros de convers∆o da composiá∆o inexistentes !" /*1200*/ + chr(10) +
                                     substitute("Verifique se existem parÉmetros de convers∆o v†lidos para a composiá∆o da finalidade econìmica com a Moeda Base &1 e Moeda ÷ndice &2 na Data &3. &7 &4 &8 &5 &9 &6" /*1200*/, entry(2, v_cod_return),entry(3, v_cod_return),entry(4, v_cod_return)).
        end /* do code_block */.
        when "1389" then
        code_block:
        do:
            assign v_des_msg_relat = substitute("Finalidade Econìmica n∆o armazena valores no M¢dulo !" /*1389*/, entry(2, v_cod_return)).
        end /* do code_block */.
        when "1568" then
        code_block:
        do:
            assign v_des_msg_relat = "Valor n∆o convertido para outras finalidades !" /*1568*/ + chr(10) + "O valor informado n∆o foi convertido para as outras finalidades. Verifique se existem finalidades dispon°veis para a unidade organizacional informada." /*1568*/.
        end /* do code_block */.
        otherwise
        code_block:
        do:
            assign v_log_convtdo = yes.
        end /* do code_block */.
    end /* case case_block */.

    /* busca titulo gerador do imposto */
    find movto_tit_ap no-lock
         where movto_tit_ap.cod_estab = compl_impto_retid_ap.cod_estab
           and movto_tit_ap.num_id_movto_tit_ap = compl_impto_retid_ap.num_id_movto_tit_ap_pai
          no-error.       
    find tit_ap no-lock
         where tit_ap.cod_estab = movto_tit_ap.cod_estab
           and tit_ap.num_id_tit_ap = movto_tit_ap.num_id_tit_ap
          no-error.       
    if avail tit_ap then do:
        assign tt_compl_impto_retid_ap.tta_cod_espec_docto      = tit_ap.cod_espec_docto
               tt_compl_impto_retid_ap.tta_cod_ser_docto        = tit_ap.cod_ser_docto
               tt_compl_impto_retid_ap.tta_cod_parcela          = tit_ap.cod_parcela
               tt_compl_impto_retid_ap.tta_cod_tit_ap           = tit_ap.cod_tit_ap.

        /* Verificar se h† movto de pagamento e que n∆o esteja estornado */
           find first imposto no-lock
               where imposto.cod_pais = classif_impto.cod_pais
                 and imposto.cod_unid_federac = classif_impto.cod_unid_federac
                 and imposto.cod_imposto = classif_impto.cod_imposto no-error.
        if  avail imposto and imposto.ind_tip_impto = "Imposto COFINS  PIS  CSLL Retido" /*l_Imposto_cofins_pis_csll_retido*/  and
            v_cod_pais_empres_usuar = "BRA" /*l_bra*/ 
        then do:
            assign tt_compl_impto_retid_ap.ttv_log_recolh = NO.

            if tit_ap.val_sdo_tit_ap = 0 then do:
                assign tt_compl_impto_retid_ap.ttv_log_recolh = YES.
            end.
            else do:
                for each movto_tit_ap no-lock
                   where movto_tit_ap.cod_estab     = tit_ap.cod_estab
                     and movto_tit_ap.num_id_tit_ap = tit_ap.num_id_tit_ap:
                    if  (movto_tit_ap.ind_trans_ap = "Baixa" /*l_baixa*/                   OR
                         movto_tit_ap.ind_trans_ap = "Baixa por Transf Estab" /*l_baixa_por_transf_estab*/  OR
                         movto_tit_ap.ind_trans_ap = "Baixa por Substituiá∆o" /*l_baixa_por_substituicao*/  OR
                         movto_tit_ap.ind_trans_ap = "Pagto Encontro Contas" /*l_pagto_encontro_contas*/  OR
                         (movto_tit_ap.ind_trans_ap = "Implantaá∆o" /*l_implantacao*/  and tit_ap.ind_tip_espec_docto = "Antecipaá∆o" /*l_antecipacao*/ )) and
                        movto_tit_ap.log_movto_estordo = no
                    then do:
                        assign tt_compl_impto_retid_ap.ttv_log_recolh = YES.
                        leave.
                    end.
                end.
            end.
        end.
        else
            assign tt_compl_impto_retid_ap.ttv_log_recolh = YES.

        IF v_log_recolh AND tt_compl_impto_retid_ap.ttv_log_recolh = NO THEN DO:
            DELETE tt_compl_impto_retid_ap.
            RETURN.
        END.
    end.

    if  v_log_convtdo = yes
    then do:
        find first tt_converter_finalid_econ no-lock no-error.
        assign tt_compl_impto_retid_ap.ttv_val_impto_indic_econ_pais = tt_converter_finalid_econ.tta_val_transacao.
    end /* if */.
END PROCEDURE. /* pi_cria_tt_compl_impto_retid_ap */
/*****************************************************************************
** Procedure Interna.....: pi_rpt_compl_impto_retido
** Descricao.............: pi_rpt_compl_impto_retido
** Criado por............: bre17191
** Criado em.............: 30/07/1999 09:29:11
** Alterado por..........: bre17191
** Alterado em...........: 30/07/1999 10:30:23
*****************************************************************************/
PROCEDURE pi_rpt_compl_impto_retido:


END PROCEDURE. /* pi_rpt_compl_impto_retido */
/*****************************************************************************
** Procedure Interna.....: pi_rpt_compl_impto_retido_quebra_fornec
** Descricao.............: pi_rpt_compl_impto_retido_quebra_fornec
** Criado por............: bre17191
** Criado em.............: 30/07/1999 09:29:27
** Alterado por..........: bre17191
** Alterado em...........: 30/07/1999 10:30:35
*****************************************************************************/
PROCEDURE pi_rpt_compl_impto_retido_quebra_fornec:


END PROCEDURE. /* pi_rpt_compl_impto_retido_quebra_fornec */
/*****************************************************************************
** Procedure Interna.....: pi_verifica_faixa_compl_impto_retid_ap
** Descricao.............: pi_verifica_faixa_compl_impto_retid_ap
** Criado por............: fut40574
** Criado em.............: 16/07/2008 14:11:46
** Alterado por..........: fut40574
** Alterado em...........: 16/07/2008 14:15:32
*****************************************************************************/
PROCEDURE pi_verifica_faixa_compl_impto_retid_ap:

    /* Busca titulo do imposto */
    find tit_ap no-lock
         where tit_ap.cod_estab = compl_impto_retid_ap.cod_estab
           and tit_ap.num_id_tit_ap = compl_impto_retid_ap.num_id_tit_ap
          no-error.

    if not avail tit_ap then
        return "NOK" /*l_nok*/ .

    if tit_ap.log_tit_Ap_estordo = yes then do:
        return "NOK" /*l_nok*/ .
    end.

    /* Busca movto para verificar somente o t°tulo de implantaá∆o */
    find first movto_tit_ap no-lock
        where movto_tit_ap.cod_estab     = compl_impto_retid_ap.cod_estab
        and   movto_tit_ap.num_id_tit_ap = compl_impto_retid_ap.num_id_tit_ap no-error.
    if movto_tit_ap.ind_trans_ap <> "Implantaá∆o" /*l_implantacao*/  then do:
        return "NOK" /*l_nok*/ .
    end.

    if  (compl_impto_retid_ap.ind_clas_impto = "Retido" /*l_retido*/  
    and v_log_mostra_impto_retid             = no)
    or (compl_impto_retid_ap.ind_clas_impto  = "Taxado" /*l_taxado*/  
    and v_log_mostra_impto_taxado            = no)
    or (tit_ap.val_sdo_tit_ap                = 0
    and v_log_impto_sem_sdo                  = no)
    then do:
        return "NOK" /*l_nok*/ .
    end /* if */.
    return "OK" /*l_ok*/ .
END PROCEDURE. /* pi_verifica_faixa_compl_impto_retid_ap */


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
/*********************  End of rpt_compl_impto_retid_ap *********************/

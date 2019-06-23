/*****************************************************************************
** Copyright KRAFT CONSULTING
** Todos os Direitos Reservados.
** 
** Este fonte e de propriedade exclusiva da KRAFT CONSULTING, sua reproducao
** parcial ou total por qualquer meio, so' podera ser feita mediante
** autorizacao expressa.
**
** Programa..............: fnc_sdo_cta_ctbl_balanct_impr
** Descricao.............: FunªÑes Balancete Cont†bil
** Versao................:  1.00.02.058
** Procedimento..........: rel_balanct_ctbl
** Nome Externo..........: prgfin/fgl/fgl900c.p
** Data Geracao..........: 15/03/2011
** Criado por............: Augusto Guimar∆es
*****************************************************************************/
def buffer histor_exec_especial for ems5.histor_exec_especial.
def buffer unid_negoc           for ems5.unid_negoc.
def buffer unid_organ           for ems5.unid_organ.
def buffer segur_unid_organ     for ems5.segur_unid_organ.

DEF INPUT PARAMETER v_cod_dwb_output AS CHAR NO-UNDO.

def var c-versao-prg as char initial " 1.00.02.058":U no-undo.

def var chExcelApplication as com-handle no-undo.
def var chWorkbook         as com-handle no-undo.
def var chWorksheet        as com-handle no-undo.
def var i-linha            as integer    no-undo.

DEF VAR v_cod_arq_excel    AS CHAR       NO-UNDO.

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
                                    "FNC_SDO_CTA_CTBL_BALANCT_IMPR","~~EMSFIN", "~~{~&emsfin_version}", "~~1.00")) /*msg_5009*/.
&else

/********************* Temporary Table Definition Begin *********************/

def new shared temp-table tt_acumul_demonst_cadastro no-undo
    field tta_cod_demonst_ctbl             as character format "x(8)" label "Demonstrativo" column-label "Demonstrativo"
    field tta_num_seq_demonst_ctbl         as integer format ">>>,>>9" initial 0 label "Sequància" column-label "Sequància"
    field tta_cod_acumul_ctbl              as character format "x(8)" label "Acumulador Cont†bil" column-label "Acumulador"
    field tta_log_zero_acumul_ctbl         as logical format "Sim/N∆o" initial no label "Zera Acumulador" column-label "Zera Acumulador"
    index tt_id                            is primary unique
          tta_cod_demonst_ctbl             ascending
          tta_num_seq_demonst_ctbl         ascending
          tta_cod_acumul_ctbl              ascending
    .

def new shared temp-table tt_acumul_demonst_ctbl_result no-undo
    field tta_cod_col_demonst_ctbl         as character format "x(2)" label "Coluna" column-label "Coluna"
    field tta_cod_acumul_ctbl              as character format "x(8)" label "Acumulador Cont†bil" column-label "Acumulador"
    field tta_log_zero_acumul_ctbl         as logical format "Sim/N∆o" initial no label "Zera Acumulador" column-label "Zera Acumulador"
    field ttv_val_sdo_ctbl_fim_sint_acumul as decimal format "->>,>>>,>>>,>>9.99" decimals 4
    field ttv_log_ja_procesdo              as logical format "Sim/N∆o" initial no
    index tt_id                            is primary unique
          tta_cod_acumul_ctbl              ascending
          tta_cod_col_demonst_ctbl         ascending
    .

def temp-table tt_ccusto no-undo like ems5.ccusto
    field ttv_rec_ccusto                   as recid format ">>>>>>9" initial ?
    field ttv_cod_format_ccusto            as character format "x(11)" label "Formato ccusto" column-label "Formato ccusto"
    index tt_ccusto_id                     is primary unique
          cod_empresa                      ascending
          cod_plano_ccusto                 ascending
          cod_ccusto                       ascending
    .

def new shared temp-table tt_ccustos_demonst no-undo
    field tta_cod_empresa                  as character format "x(3)" label "Empresa" column-label "Empresa"
    field tta_cod_plano_ccusto             as character format "x(8)" label "Plano Centros Custo" column-label "Plano Centros Custo"
    field tta_cod_ccusto                   as Character format "x(11)" label "Centro Custo" column-label "Centro Custo"
    field ttv_cod_ccusto_pai               as Character format "x(11)" label "Centro Custo Pai" column-label "Centro Custo Pai"
    field ttv_log_proces                   as logical format "Sim/Nao" initial no label "&prc(" column-label "&prc("
    index tt_cod_ccusto_pai               
          ttv_cod_ccusto_pai               ascending
    index tt_log_proces                   
          ttv_log_proces                   ascending
    index tt_select_id                     is primary unique
          tta_cod_empresa                  ascending
          tta_cod_plano_ccusto             ascending
          tta_cod_ccusto                   ascending
    .

def new shared temp-table tt_col_demonst_ctbl no-undo like col_demonst_ctbl
    field ttv_rec_col_demonst_ctbl         as recid format ">>>>>>9"
    .

def new shared temp-table tt_compos_demonst_cadastro no-undo like compos_demonst_ctbl
    field tta_cod_proj_financ_excec        as character format "x(20)" label "Projeto Exceá∆o" column-label "Projeto Exceá∆o"
    field tta_cod_proj_financ_inicial      as character format "x(20)" label "Projeto Financ Inic" column-label "Projeto Financ Inic"
    .

def new shared temp-table tt_controla_analise_vertical no-undo
    field tta_cod_col_demonst_ctbl         as character format "x(2)" label "Coluna" column-label "Coluna"
    field tta_cod_acumul_ctbl              as character format "x(8)" label "Acumulador Cont†bil" column-label "Acumulador"
    field ttv_cod_linha                    as character format "x(730)"
    field ttv_val_sdo_ctbl_analis_vert     as decimal format "->>,>>>,>>>,>>9.99" decimals 2
    index tt_id                            is primary unique
          tta_cod_col_demonst_ctbl         ascending
          tta_cod_acumul_ctbl              ascending
          ttv_cod_linha                    ascending
    .

def temp-table tt_cta_ctbl_505 no-undo
    field ttv_rec_cta_ctbl                 as recid format ">>>>>>9"
    .

def temp-table tt_cta_ctbl_analit no-undo
    field tta_cod_cta_ctbl                 as character format "x(20)" label "Conta Cont†bil" column-label "Conta Cont†bil"
    index tt_id                            is primary unique
          tta_cod_cta_ctbl                 ascending
    .

def temp-table tt_cta_ctbl_aux no-undo
    field tta_cod_cta_ctbl                 as character format "x(20)" label "Conta Cont†bil" column-label "Conta Cont†bil"
    field ttv_rec_cta_ctbl                 as recid format ">>>>>>9"
    field ttv_num_seq                      as integer format ">>>,>>9" label "SeqÅància" column-label "Seq"
    index tt_id                           
          tta_cod_cta_ctbl                 ascending
    index tt_seq                          
          ttv_num_seq                      descending
    .

DEF NEW shared temp-table tt_cta_ctbl_demonst NO-UNDO
    field tta_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field tta_cod_cta_ctbl                 as character format "x(20)" label "Conta Cont†bil" column-label "Conta Cont†bil"
    field ttv_cod_cta_ctbl_pai             as character format "x(20)" label "Conta Ctbl Pai" column-label "Conta Ctbl Pai"
    field ttv_log_consid_apurac            as logical format "Sim/N∆o" initial no
    field tta_ind_espec_cta_ctbl           as character format "X(10)" initial "Anal°tica" label "EspÇcie Conta" column-label "EspÇcie"
    index tt_cod_cta_ctbl_pai             
          tta_cod_plano_cta_ctbl           ascending
          ttv_cod_cta_ctbl_pai             ascending
    index tt_select_id                     is primary unique
          tta_cod_plano_cta_ctbl           ascending
          tta_cod_cta_ctbl                 ascending
    .

def NEW shared temp-table tt_relacto_item_retorna_cons no-undo
    field tta_num_seq                      as integer format ">>>,>>9" initial 0 label "Sequància" column-label "NumSeq"
    field ttv_rec_ret_orig                 as recid format ">>>>>>9"
    field ttv_rec_ret_dest                 as recid format ">>>>>>9"
    index tt_id                           
          tta_num_seq                      ascending
          ttv_rec_ret_orig                 ascending
          ttv_rec_ret_dest                 ascending
    index tt_recid_item                   
          ttv_rec_ret_orig                 ascending
    .

def NEW shared temp-table tt_retorna_sdo_orcto_ccusto no-undo
    field ttv_rec_ret_sdo_ctbl             as recid format ">>>>>>9"
    field tta_cod_plano_ccusto             as character format "x(8)" label "Plano Centros Custo" column-label "Plano Centros Custo"
    field tta_cod_ccusto                   as Character format "x(11)" label "Centro Custo" column-label "Centro Custo"
    field tta_val_orcado                   as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Oráado" column-label "Valor Oráado"
    field tta_val_orcado_sdo               as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Oráado" column-label "Saldo Oráado"
    field tta_qtd_orcado                   as decimal format "->>>>,>>9.9999" decimals 4 initial 0 label "Qtdade Oráada" column-label "Qtdade Oráada"
    field tta_qtd_orcado_sdo               as decimal format "->>>>,>>9.9999" decimals 4 initial 0 label "Saldo Quantidade" column-label "Saldo Quantidade"
    index tt_id                            is primary unique
          ttv_rec_ret_sdo_ctbl             ascending
          tta_cod_plano_ccusto             ascending
          tta_cod_ccusto                   ascending
    .

def new shared temp-table tt_demonst_ctbl_video no-undo
    field tta_num_seq_demonst_ctbl         as integer format ">>>,>>9" initial 0 label "Sequància" column-label "Sequància"
    field ttv_rec_demonst_ctbl_video       as recid format ">>>>>>9"
    field ttv_cod_lin_demonst              as character format "x(2000)"
    field ttv_num_seq_item_demonst_ctbl    as integer format ">>>>,>>9"
    field ttv_rec_item_demonst_ctbl_cad    as recid format ">>>>>>9"
    field ttv_rec_item_demonst_ctbl_video  as recid format ">>>>>>9"
    field ttv_des_lista_estab              as character format "x(2000)" label "Estabelecimentos" column-label "Estabelecimentos"
    field ttv_num_nivel                    as integer format ">>>>,>>9"
    index tt_id                            is primary unique
          tta_num_seq_demonst_ctbl         ascending
    .

def temp-table tt_estab_unid_negoc_select no-undo like estab_unid_negoc
    index tt_estab_unid_negoc_select_id    is primary unique
          cod_estab                        ascending
          cod_unid_negoc                   ascending
    .

def temp-table tt_estrut_ctbl_505 no-undo
    field ttv_rec_estrut_ctbl              as recid format ">>>>>>9"
    .

def new shared temp-table tt_estrut_visualiz_ctbl_cad         like estrut_visualiz_ctbl
    .

def new shared temp-table tt_grp_col_demonst_video no-undo
    field tta_qtd_period_relac_base        as decimal format ">9" initial 0 label "Per°odos da Base" column-label "Per°odo da Base"
    field tta_qtd_exerc_relac_base         as decimal format ">9" initial 0 label "Exerc°cios da Base" column-label "Exerc°cios da Base"
    field tta_ind_tip_relac_base           as character format "X(20)" label "Tipo Relaá∆o" column-label "Tipo Relaá∆o"
    field tta_num_conjto_param_ctbl        as integer format ">9" initial 1 label "Conjunto ParÉmetros" column-label "Conjunto ParÉmetros"
    field tta_ind_tip_val_consolid         as character format "X(18)" initial "Total" label "Tipo Valor Consolid" column-label "Tipo Valor Consolid"
    field ttv_cod_cenar_ctbl               as character format "x(8)" label "Cen†rio Cont†bil" column-label "Cen†rio Cont†bil"
    field ttv_val_cotac_indic_econ         as decimal format "->>,>>>,>>>,>>9.9999999999" decimals 10 label "Cotaá∆o" column-label "Cotaá∆o"
    field ttv_cod_cta_ctbl_fim             as character format "x(20)" label "Conta Final" column-label "Final"
    field ttv_cod_cta_ctbl_ini             as character format "x(20)" label "Conta Inicial" column-label "Inicial"
    field ttv_cod_cta_prefer_excec         as character format "x(20)" initial "####################" label "Exceá∆o" column-label "Exceá∆o"
    field ttv_cod_cta_prefer_pfixa         as character format "x(20)" label "Parte Fixa" column-label "Parte Fixa"
    field ttv_cod_unid_negoc_fim           as character format "x(3)" label "atÇ" column-label "Final"
    field ttv_cod_unid_negoc_ini           as character format "x(3)" label "Unid Neg¢cio" column-label "Inicial"
    field ttv_cod_estab_fim                as character format "x(3)" label "atÇ" column-label "Estab Final"
    field ttv_cod_estab_inic               as character format "x(3)" label "Estab Inicial" column-label "Estab Inicial"
    field ttv_cod_unid_organ_prefer        as character format "x(3)"
    field ttv_num_period                   as integer format ">>>>,>>9" label "Per°odo Cont†bil" column-label "Per°odo Cont†bil"
    field ttv_cod_unid_organ_prefer_inic   as character format "x(3)"
    field ttv_cod_unid_organ_prefer_fim    as character format "x(3)"
    field ttv_cod_unid_organ_fim           as character format "x(3)" label "Final" column-label "Unid Organizacional"
    field ttv_cod_unid_organ_ini           as character format "x(3)" label "UO Inicial" column-label "Unid Organizacional"
    field ttv_cod_cenar_orctario           as character format "x(8)" label "Cenar Orctario" column-label "Cen†rio Oráament†rio"
    field ttv_cod_vers_orcto_ctbl          as character format "x(10)" label "Vers∆o Oráamento" column-label "Vers∆o Oráamento"
    field ttv_cod_finalid_econ             as character format "x(10)" label "Finalidade Econìmica" column-label "Finalidade Econìmica"
    field ttv_dat_fim_period               as date format "99/99/9999" label "Fim Per°odo"
    field ttv_cod_plano_ccusto             as character format "x(8)" label "Plano ccusto" column-label "Plano ccusto"
    field ttv_cod_ccusto_inic              as character format "x(11)" label "C.Custo Inicial" column-label "C.Custo Inicial"
    field ttv_cod_ccusto_fim               as Character format "x(11)" label "CCusto Final" column-label "Centro Custo"
    field ttv_cod_ccusto_prefer_pfixa      as character format "x(11)" label "Parte Fixa"
    field ttv_cod_ccusto_prefer_excec      as character format "x(11)" label "Exceá∆o"
    field ttv_cod_proj_financ_inic         as character format "x(20)" label "Projeto Inicial"
    field ttv_cod_proj_financ_fim          as character format "x(20)" label "Projeto Final" column-label "Projeto"
    field ttv_cod_proj_financ_prefer_pfixa as character format "x(20)" label "Parte Fixa"
    field ttv_cod_proj_financ_prefer_excec as character format "x(20)" label "Exceá∆o"
    field tta_cod_unid_orctaria            as character format "x(8)" label "Unid Oráament†ria" column-label "Unid Oráament†ria"
    field tta_num_seq_orcto_ctbl           as integer format ">>>>>>>>9" initial 0 label "Seq Orcto Cont†bil" column-label "Seq Orcto Cont†bil"
    field ttv_cod_exec_period_1            as character format "x(6)"
    field ttv_cod_exec_period_2            as character format "x(6)"
    field ttv_des_col_demonst              as character format "x(40)"
    index tt_id                            is primary unique
          tta_qtd_period_relac_base        ascending
          tta_qtd_exerc_relac_base         ascending
          tta_ind_tip_relac_base           ascending
          tta_num_conjto_param_ctbl        ascending
          tta_ind_tip_val_consolid         ascending
    .

def temp-table tt_grp_cta_ctbl no-undo
    field tta_cod_grp_cta_ctbl             as character format "x(8)" label "Grupo Contas" column-label "Grupo Contas"
    field tta_val_sdo_ctbl_inic            as decimal format "->>>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Inicial" column-label "Saldo Inicial"
    field tta_val_sdo_ctbl_fim             as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Cont†bil Final" column-label "Saldo Cont†bil Final"
    field ttv_val_sdo_ctbl_inic_ant        as decimal format "->>,>>>,>>>,>>9.99" decimals 2 label "Sdo Inicial Ant" column-label "Sdo Inicial Ant"
    field ttv_val_sdo_ctbl_fim_ant         as decimal format "->>,>>>,>>>,>>9.99" decimals 2 label "Sdo Final Ant" column-label "Sdo Final Ant"
    index tt_grp_cta_ctbl_id               is primary unique
          tta_cod_grp_cta_ctbl             ascending
    .

def temp-table tt_input_leitura_sdo no-undo
    field ttv_cod_label                    as character format "x(8)" label "Label" column-label "Label"
    field ttv_des_conteudo                 as character format "x(40)" label "Texto" column-label "Texto"
    field ttv_num_seq_1                    as integer format ">>>,>>9"
    field ttv_num_seq_2                    as integer format ">>>>,>>9"
    index tt_ID                            is primary
          ttv_num_seq_1                    ascending
    .

def temp-table tt_input_leitura_sdo_demonst no-undo
    field ttv_cod_label                    as character format "x(8)" label "Label" column-label "Label"
    field ttv_des_conteudo                 as character format "x(40)" label "Texto" column-label "Texto"
    field ttv_num_seq_1                    as integer format ">>>,>>9"
    index tt_ID                            is primary
          ttv_num_seq_1                    ascending
    .

def temp-table tt_input_sdo no-undo
    field tta_cod_unid_organ_inic          as character format "x(3)" label "UO Inicial" column-label "UO Unicial"
    field tta_cod_unid_organ_fim           as character format "x(3)" label "UO Final" column-label "UO FInal"
    field ttv_cod_unid_organ_orig_ini      as character format "x(3)" label "UO Origem" column-label "UO Origem"
    field ttv_cod_unid_organ_orig_fim      as character format "x(3)" label "UO Origem" column-label "UO Origem"
    field tta_cod_finalid_econ             as character format "x(10)" label "Finalidade" column-label "Finalidade"
    field tta_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field tta_cod_cta_ctbl_inic            as character format "x(20)" label "Conta Contabil" column-label "Conta Contab Inicial"
    field tta_cod_cta_ctbl_fim             as character format "x(20)" label "atÇ" column-label "Conta Cont†bil Final"
    field tta_cod_plano_ccusto             as character format "x(8)" label "Plano Centros Custo" column-label "Plano Centros Custo"
    field tta_cod_ccusto_inic              as Character format "x(11)" label "Centro Custo" column-label "Centro Custo Inicial"
    field tta_cod_ccusto_fim               as Character format "x(11)" label "atÇ" column-label "Centro Custo Final"
    field tta_cod_proj_financ_inic         as character format "x(8)" label "N∆o Utilizar..." column-label "Projeto"
    field tta_cod_proj_financ_fim          as character format "x(20)" label "Projeto Final" column-label "Projeto Final"
    field tta_cod_cenar_ctbl               as character format "x(8)" label "Cen†rio Cont†bil" column-label "Cen†rio Cont†bil"
    field tta_cod_estab_inic               as character format "x(3)" label "Estabelecimento" column-label "Estab Inicial"
    field tta_cod_estab_fim                as character format "x(3)" label "atÇ" column-label "Estabel Final"
    field tta_cod_unid_negoc_inic          as character format "x(3)" label "Unid Negoc" column-label "UN Inicial"
    field tta_cod_unid_negoc_fim           as character format "x(3)" label "atÇ" column-label "UN Final"
    field ttv_ind_espec_sdo_tot            as character format "X(15)"
    field ttv_log_consid_apurac_restdo     as logical format "Sim/N∆o" initial yes label "Consid Apurac Restdo" column-label "Apurac Restdo"
    field ttv_cod_elimina_intercomp        as character format "x(20)"
    field ttv_log_espec_sdo_ccusto         as logical format "Sim/N∆o" initial no
    field ttv_log_restric_estab            as logical format "Sim/N∆o" initial no label "Usa Segur Estab" column-label "Usa Segur Estab"
    field ttv_ind_espec_cta                as character format "X(10)"
    field ttv_cod_leitura                  as character format "x(8)"
    field ttv_cod_condicao                 as character format "x(20)"
    field ttv_cod_cenar_orctario           as character format "x(8)" label "Cenar Orctario" column-label "Cen†rio Oráament†rio"
    field ttv_cod_unid_orctaria            as character format "x(8)" label "Unid Oráament†ria" column-label "Unid Oráament†ria"
    field ttv_num_seq_orcto_ctbl           as integer format ">>>>>>>>9" label "Seq Orcto Cont†bil" column-label "Seq Orcto Cont†bil"
    field ttv_cod_vers_orcto_ctbl          as character format "x(10)" label "Vers∆o Oráamento" column-label "Vers∆o Oráamento"
    field ttv_cod_cta_ctbl_pfixa           as character format "x(20)" label "Parte Fixa" column-label "Parte Fixa Cta Ctbl"
    field ttv_cod_ccusto_pfixa             as character format "x(11)" label "Parte Fixa ccusto" column-label "Parte Fixa ccusto"
    field ttv_cod_proj_financ_pfixa        as character format "x(20)" label "Parte Fixa"
    field ttv_cod_cta_ctbl_excec           as character format "x(20)" initial "...................." label "Parte Exceá∆o" column-label "Parte Exceá∆o"
    field ttv_cod_ccusto_excec             as character format "x(11)" initial "..........." label "Parte Exceá∆o" column-label "Parte Exceá∆o"
    field ttv_cod_proj_financ_excec        as character format "x(20)" initial "...................." label "Exceá∆o" column-label "Exceá∆o"
    field ttv_num_seq_demonst_ctbl         as integer format ">>>,>>9" label "Sequància" column-label "Sequància"
    field ttv_num_seq_compos_demonst       as integer format ">>>>,>>9"
    field ttv_cod_chave                    as character format "x(40)"
    field ttv_cod_seq                      as character format "x(200)"
    field ttv_cod_dat_sdo_ctbl_inic        as character format "x(200)"
    field ttv_cod_dat_sdo_ctbl_fim         as character format "x(200)"
    field ttv_cod_exerc_ctbl               as character format "9999" label "Exerc°cio Cont†bil" column-label "Exerc°cio Cont†bil"
    field ttv_cod_period_ctbl              as character format "x(08)" label "Per°odo Cont†bil" column-label "Per°odo Cont†bil"
    .

def new shared temp-table tt_item_demonst_ctbl_cadastro         like item_demonst_ctbl
    field ttv_log_ja_procesdo              as logical format "Sim/N∆o" initial no
    field ttv_rec_item_demonst_ctbl_cad    as recid format ">>>>>>9"
    index tt_id                            is primary unique
          cod_demonst_ctbl                 ascending
          num_seq_demonst_ctbl             ascending
    index tt_recid                        
          ttv_rec_item_demonst_ctbl_cad    ascending
    .

def new shared temp-table tt_item_demonst_ctbl_video no-undo
    field ttv_val_seq_demonst_ctbl         as decimal format ">>>,>>9.99" decimals 2
    field ttv_rec_item_demonst_ctbl_cad    as recid format ">>>>>>9"
    field ttv_rec_item_demonst_ctbl_video  as recid format ">>>>>>9"
    field ttv_cod_chave_1                  as character format "x(20)"
    field ttv_cod_chave_2                  as character format "x(20)"
    field ttv_cod_chave_3                  as character format "x(20)"
    field ttv_cod_chave_4                  as character format "x(20)"
    field ttv_cod_chave_5                  as character format "x(20)"
    field ttv_cod_chave_6                  as character format "x(20)"
    field ttv_des_chave_1                  as character format "x(40)"
    field ttv_des_chave_2                  as character format "x(40)"
    field ttv_des_chave_3                  as character format "x(40)"
    field ttv_des_chave_4                  as character format "x(40)"
    field ttv_des_chave_5                  as character format "x(40)"
    field ttv_des_chave_6                  as character format "x(40)"
    field tta_des_tit_ctbl                 as character format "x(40)" label "T°tulo Cont†bil" column-label "T°tulo Cont†bil"
    field ttv_des_valpres                  as character format "x(40)"
    field ttv_log_tit_ctbl_vld             as logical format "Sim/N∆o" initial no
    field tta_ind_funcao_col_demonst_ctbl  as character format "X(12)" initial "Impress∆o" label "Funá∆o Coluna" column-label "Funá∆o Coluna"
    field tta_ind_orig_val_col_demonst     as character format "X(12)" initial "T°tulo" label "Origem Valores" column-label "Origem Valores"
    field tta_cod_format_col_demonst_ctbl  as character format "x(40)" label "Formato Coluna" column-label "Formato Coluna"
    field ttv_cod_identif_campo            as character format "x(40)"
    field ttv_log_cta_sint                 as logical format "Sim/N∆o" initial no
    field tta_cod_plano_ccusto             as character format "x(8)" label "Plano Centros Custo" column-label "Plano Centros Custo"
    index tt_id                            is primary
          ttv_cod_chave_1                  ascending
          ttv_cod_chave_2                  ascending
          ttv_cod_chave_3                  ascending
          ttv_cod_chave_4                  ascending
          ttv_cod_chave_5                  ascending
          ttv_cod_chave_6                  ascending
          ttv_val_seq_demonst_ctbl         ascending
    index tt_recid                        
          ttv_rec_item_demonst_ctbl_video  ascending
    index tt_recid_cad                    
          ttv_rec_item_demonst_ctbl_cad    ascending
    .

def new shared temp-table tt_label_demonst_ctbl_video no-undo
    field tta_num_seq_demonst_ctbl         as integer format ">>>,>>9" initial 0 label "Sequància" column-label "Sequància"
    field tta_cod_col_demonst_ctbl         as character format "x(2)" label "Coluna" column-label "Coluna"
    field ttv_num_pos_col_demonst_ctbl     as integer format ">>>>,>>9" label "N£mero Posiá‰es Col"
    field ttv_cod_format_col_demonst_ctbl  as character format "x(40)" label "Formato Coluna" column-label "Formato Coluna"
    field ttv_des_label_col_demonst_ctbl   as character format "x(40)"
    field tta_ind_orig_val_col_demonst     as character format "X(12)" initial "T°tulo" label "Origem Valores" column-label "Origem Valores"
    index tt_label_demonst                 is primary unique
          tta_num_seq_demonst_ctbl         ascending
    .

def temp-table tt_log_erros no-undo
    field ttv_num_seq                      as integer format ">>>,>>9" label "SeqÅància" column-label "Seq"
    field ttv_num_cod_erro                 as integer format ">>>>,>>9" label "N£mero" column-label "N£mero"
    field ttv_des_erro                     as character format "x(50)" label "Inconsistància" column-label "Inconsistància"
    field ttv_des_ajuda                    as character format "x(50)" label "Ajuda" column-label "Ajuda"
    .

def new shared temp-table tt_proj_financ_demonst no-undo
    field tta_cod_proj_financ              as character format "x(20)" label "Projeto" column-label "Projeto"
    field ttv_cod_proj_financ_pai          as character format "x(20)" label "Projeto Pai" column-label "Projeto Pai"
    field ttv_log_proces                   as logical format "Sim/Nao" initial no label "&prc(" column-label "&prc("
    field ttv_ind_espec_proj_financ        as character format "X(10)"
    index tt_cod_proj_financ_pai          
          ttv_cod_proj_financ_pai          ascending
    index tt_log_proces                   
          ttv_log_proces                   ascending
    index tt_select_id                     is primary unique
          tta_cod_proj_financ              ascending
    .

def temp-table tt_proj_financ_select no-undo
    field tta_cod_proj_financ              as character format "x(20)" label "Projeto" column-label "Projeto"
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_dat_inic_valid               as date format "99/99/9999" initial &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 01/01/0001 &ENDIF label "In°cio Validade" column-label "Inic Validade"
    field tta_dat_fim_valid                as date format "99/99/9999" initial 12/31/9999 label "Fim Validade" column-label "Fim Validade"
    field tta_cod_livre_1                  as character format "x(100)" label "Livre 1" column-label "Livre 1"
    field tta_val_livre_1                  as decimal format ">>>,>>>,>>9.9999" decimals 4 initial 0 label "Livre 1" column-label "Livre 1"
    field tta_log_livre_1                  as logical format "Sim/N∆o" initial no label "Livre 1" column-label "Livre 1"
    field tta_dat_livre_1                  as date format "99/99/9999" initial ? label "Livre 1" column-label "Livre 1"
    field tta_num_livre_1                  as integer format ">>>>>9" initial 0 label "Livre 1" column-label "Livre 1"
    index tt_proj_financ_select_id         is primary unique
          tta_cod_estab                    ascending
          tta_cod_proj_financ              ascending
    .
/*
def new shared temp-table tt_relacto_item_retorna no-undo
    field tta_num_seq                      as integer format ">>>,>>9" initial 0 label "Sequància" column-label "NumSeq"
    field ttv_rec_item_demonst             as recid format ">>>>>>9"
    field ttv_rec_ret                      as recid format ">>>>>>9"
    index tt_id                           
          tta_num_seq                      ascending
          ttv_rec_item_demonst             ascending
    index tt_recid_item                   
          ttv_rec_item_demonst             ascending
    .
    */


def NEW shared temp-table tt_relacto_item_retorna no-undo
    field tta_num_seq                      as integer format ">>>,>>9" initial 0 label "Sequància" column-label "NumSeq"
    field ttv_rec_item_demonst             as recid format ">>>>>>9"
    field ttv_rec_ret                      as recid format ">>>>>>9"
    index tt_id                           
          tta_num_seq                      ascending
          ttv_rec_item_demonst             ascending
          ttv_rec_ret                      ascending
    index tt_id_2                         
          ttv_rec_item_demonst             ascending
          ttv_rec_ret                      ascending
    index tt_recid_item                   
          ttv_rec_item_demonst             ascending
    .





def new shared temp-table tt_rel_grp_col_compos_demonst no-undo
    field ttv_num_seq_sdo                  as integer format ">>>>,>>9"
    field ttv_rec_grp_col_demonst_ctbl     as recid format ">>>>>>9"
    field ttv_rec_compos_demonst_ctbl      as recid format ">>>>>>9" initial ?
    index tt_id                            is primary
          ttv_rec_grp_col_demonst_ctbl     ascending
    index tt_id_2                         
          ttv_num_seq_sdo                  ascending
    .

def temp-table tt_retorna_sdo_ctbl_aux no-undo
    field tta_num_seq                      as integer format ">>>,>>9" initial 0 label "Sequància" column-label "NumSeq"
    field tta_cod_empresa                  as character format "x(3)" label "Empresa" column-label "Empresa"
    field tta_cod_finalid_econ             as character format "x(10)" label "Finalidade" column-label "Finalidade"
    field tta_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field tta_cod_cta_ctbl                 as character format "x(20)" label "Conta Cont†bil" column-label "Conta Cont†bil"
    field tta_cod_plano_ccusto             as character format "x(8)" label "Plano Centros Custo" column-label "Plano Centros Custo"
    field tta_cod_ccusto                   as Character format "x(11)" label "Centro Custo" column-label "Centro Custo"
    field tta_cod_proj_financ              as character format "x(20)" label "Projeto" column-label "Projeto"
    field tta_cod_cenar_ctbl               as character format "x(8)" label "Cen†rio Cont†bil" column-label "Cen†rio Cont†bil"
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_unid_negoc               as character format "x(3)" label "Unid Neg¢cio" column-label "Un Neg"
    field tta_dat_sdo_ctbl                 as date format "99/99/9999" initial ? label "Data Saldo Cont†bil" column-label "Data Saldo Cont†bil"
    field tta_val_sdo_ctbl_db              as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Movto DÇbito" column-label "Movto DÇbito"
    field tta_val_sdo_ctbl_cr              as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Movto CrÇdito" column-label "Movto CrÇdito"
    field tta_val_sdo_ctbl_fim             as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Cont†bil Final" column-label "Saldo Cont†bil Final"
    field tta_val_apurac_restdo            as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Apuraá∆o Resultado" column-label "Apuraá∆o Resultado"
    field tta_val_apurac_restdo_db         as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Apuraá∆o Restdo DB" column-label "Apuraá∆o Restdo DB"
    field tta_val_apurac_restdo_cr         as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Apuraá∆o Restdo CR" column-label "Apuraá∆o Restdo CR"
    field tta_val_apurac_restdo_acum       as decimal format "->>>>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Apuracao Final" column-label "Apuracao Final"
    field tta_val_sdo_ctbl_db_sint         as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Movto DÇbito Sint" column-label "Movto DÇbito Sint"
    field tta_val_sdo_ctbl_cr_sint         as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Movto CrÇdito Sint" column-label "Movto CrÇdito Sint"
    field tta_val_sdo_ctbl_fim_sint        as decimal format "->>>>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo SintÇtico" column-label "Saldo SintÇtico"
    field tta_val_apurac_restdo_sint       as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Apuracao Resultado" column-label "Apuracao Resultado"
    field tta_val_apurac_restdo_sint_db    as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Apur Restdo Sint DB" column-label "Apur Restdo Sint DB"
    field tta_val_apurac_restdo_sint_cr    as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Apur Restdo Sint CR" column-label "Apur Restdo Sint CR"
    field tta_val_apurac_restdo_sint_acum  as decimal format "->>>>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Apur Result Sint" column-label "Apur Result Sint"
    field tta_val_movto_empenh             as decimal format "->>,>>>,>>>,>>9.99" decimals 9 initial 0 label "Movto Empenhado" column-label "Movto Empenhado"
    field tta_qtd_sdo_ctbl_db              as decimal format ">>>,>>>,>>9.99" decimals 2 initial 0 label "Quantidade DB" column-label "Quantidade DB"
    field tta_qtd_sdo_ctbl_cr              as decimal format ">>>,>>>,>>9.99" decimals 2 initial 0 label "Quantidade CR" column-label "Quantidade CR"
    field tta_qtd_sdo_ctbl_fim             as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Quantidade Final" column-label "Quantidade Final"
    field ttv_val_movto_ctbl               as decimal format ">>>,>>>,>>>,>>9.99" decimals 2
    field tta_qtd_movto_empenh             as decimal format "->>>>,>>9.9999" decimals 4 initial 0 label "Qtde Movto Empenhado" column-label "Qtde Movto Empenhado"
    index tt_cta                          
          tta_cod_plano_cta_ctbl           ascending
          tta_cod_cta_ctbl                 ascending
    index tt_id                            is primary unique
          tta_cod_empresa                  ascending
          tta_cod_finalid_econ             ascending
          tta_cod_plano_cta_ctbl           ascending
          tta_cod_cta_ctbl                 ascending
          tta_cod_plano_ccusto             ascending
          tta_cod_ccusto                   ascending
          tta_cod_proj_financ              ascending
          tta_cod_cenar_ctbl               ascending
          tta_cod_estab                    ascending
          tta_cod_unid_negoc               ascending
          tta_dat_sdo_ctbl                 ascending
          tta_num_seq                      ascending
    index tt_id2                          
          tta_cod_empresa                  ascending
          tta_cod_finalid_econ             ascending
          tta_cod_plano_cta_ctbl           ascending
          tta_cod_cta_ctbl                 ascending
          tta_cod_proj_financ              ascending
          tta_cod_cenar_ctbl               ascending
          tta_cod_estab                    ascending
          tta_cod_unid_negoc               ascending
          tta_dat_sdo_ctbl                 ascending
    index tt_seq                          
          tta_num_seq                      ascending
    .

def NEW shared temp-table tt_retorna_sdo_ctbl_demonst no-undo
    field tta_num_seq                      as integer format ">>>,>>9" initial 0 label "Sequància" column-label "NumSeq"
    field tta_cod_empresa                  as character format "x(3)" label "Empresa" column-label "Empresa"
    field tta_cod_finalid_econ             as character format "x(10)" label "Finalidade" column-label "Finalidade"
    field tta_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field tta_cod_cta_ctbl                 as character format "x(20)" label "Conta Cont†bil" column-label "Conta Cont†bil"
    field tta_cod_plano_ccusto             as character format "x(8)" label "Plano Centros Custo" column-label "Plano Centros Custo"
    field tta_cod_ccusto                   as Character format "x(11)" label "Centro Custo" column-label "Centro Custo"
    field tta_cod_proj_financ              as character format "x(20)" label "Projeto" column-label "Projeto"
    field tta_cod_cenar_ctbl               as character format "x(8)" label "Cen†rio Cont†bil" column-label "Cen†rio Cont†bil"
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_unid_negoc               as character format "x(3)" label "Unid Neg¢cio" column-label "Un Neg"
    field tta_dat_sdo_ctbl                 as date format "99/99/9999" initial ? label "Data Saldo Cont†bil" column-label "Data Saldo Cont†bil"
    field tta_cod_unid_organ_orig          as character format "x(3)" label "UO Origem" column-label "UO Origem"
    field ttv_ind_espec_sdo                as character format "X(20)"
    field tta_val_sdo_ctbl_db              as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Movto DÇbito" column-label "Movto DÇbito"
    field tta_val_sdo_ctbl_cr              as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Movto CrÇdito" column-label "Movto CrÇdito"
    field tta_val_sdo_ctbl_fim             as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Cont†bil Final" column-label "Saldo Cont†bil Final"
    field tta_val_apurac_restdo            as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Apuraá∆o Resultado" column-label "Apuraá∆o Resultado"
    field tta_val_apurac_restdo_db         as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Apuraá∆o Restdo DB" column-label "Apuraá∆o Restdo DB"
    field tta_val_apurac_restdo_cr         as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Apuraá∆o Restdo CR" column-label "Apuraá∆o Restdo CR"
    field tta_val_apurac_restdo_acum       as decimal format "->>>>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Apuracao Final" column-label "Apuracao Final"
    field tta_val_movto_empenh             as decimal format "->>,>>>,>>>,>>9.99" decimals 9 initial 0 label "Movto Empenhado" column-label "Movto Empenhado"
    field tta_qtd_sdo_ctbl_db              as decimal format ">>>,>>>,>>9.99" decimals 2 initial 0 label "Quantidade DB" column-label "Quantidade DB"
    field tta_qtd_sdo_ctbl_cr              as decimal format ">>>,>>>,>>9.99" decimals 2 initial 0 label "Quantidade CR" column-label "Quantidade CR"
    field tta_qtd_sdo_ctbl_fim             as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Quantidade Final" column-label "Quantidade Final"
    field ttv_val_movto_ctbl               as decimal format ">>>,>>>,>>>,>>9.99" decimals 2
    field ttv_qtd_movto_ctbl               as decimal format "->>>>,>>9.9999" decimals 4
    field tta_qtd_movto_empenh             as decimal format "->>>>,>>9.9999" decimals 4 initial 0 label "Qtde Movto Empenhado" column-label "Qtde Movto Empenhado"
    field tta_val_orcado                   as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Oráado" column-label "Valor Oráado"
    field tta_val_orcado_sdo               as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Oráado" column-label "Saldo Oráado"
    field tta_qtd_orcado                   as decimal format "->>>>,>>9.9999" decimals 4 initial 0 label "Qtdade Oráada" column-label "Qtdade Oráada"
    field tta_qtd_orcado_sdo               as decimal format "->>>>,>>9.9999" decimals 4 initial 0 label "Saldo Quantidade" column-label "Saldo Quantidade"
    field ttv_rec_ret_sdo_ctbl             as recid format ">>>>>>9"
    field ttv_log_sdo_orcado_sint          as logical format "Sim/N∆o" initial no
    field ttv_val_perc_criter_distrib      as decimal format ">>9.99" decimals 6 initial 0 label "Percentual" column-label "Percentual"
    index tt_busca                        
          tta_cod_empresa                  ascending
          tta_cod_finalid_econ             ascending
          tta_cod_cenar_ctbl               ascending
          tta_cod_plano_cta_ctbl           ascending
          tta_cod_cta_ctbl                 ascending
          tta_cod_estab                    ascending
          tta_cod_unid_negoc               ascending
          tta_cod_plano_ccusto             ascending
          tta_cod_ccusto                   ascending
          tta_dat_sdo_ctbl                 ascending
    index tt_busca_proj                   
          tta_cod_empresa                  ascending
          tta_cod_finalid_econ             ascending
          tta_cod_cenar_ctbl               ascending
          tta_cod_plano_cta_ctbl           ascending
          tta_cod_cta_ctbl                 ascending
          tta_cod_proj_financ              ascending
          tta_cod_estab                    ascending
          tta_cod_unid_negoc               ascending
          tta_cod_plano_ccusto             ascending
          tta_cod_ccusto                   ascending
          tta_dat_sdo_ctbl                 ascending
    index tt_id                            is primary unique
          tta_cod_empresa                  ascending
          tta_cod_finalid_econ             ascending
          tta_cod_plano_cta_ctbl           ascending
          tta_cod_cta_ctbl                 ascending
          tta_cod_plano_ccusto             ascending
          tta_cod_ccusto                   ascending
          tta_cod_proj_financ              ascending
          tta_cod_cenar_ctbl               ascending
          tta_cod_estab                    ascending
          tta_cod_unid_negoc               ascending
          tta_dat_sdo_ctbl                 ascending
          tta_num_seq                      ascending
          ttv_ind_espec_sdo                ascending
          tta_cod_unid_organ_orig          ascending
    index tt_id2                          
          tta_cod_empresa                  ascending
          tta_cod_finalid_econ             ascending
          tta_cod_plano_cta_ctbl           ascending
          tta_cod_cta_ctbl                 ascending
          tta_cod_plano_ccusto             ascending
          tta_cod_ccusto                   ascending
          tta_cod_cenar_ctbl               ascending
          tta_cod_estab                    ascending
          tta_cod_unid_negoc               ascending
          tta_cod_proj_financ              ascending
          tta_dat_sdo_ctbl                 ascending
          ttv_ind_espec_sdo                ascending
          tta_num_seq                      ascending
    index tt_rec                          
          ttv_rec_ret_sdo_ctbl             ascending
    .
/*
def new shared temp-table tt_retorna_sdo_ctbl_demonst no-undo
    field tta_num_seq                      as integer format ">>>,>>9" initial 0 label "Sequància" column-label "NumSeq"
    field tta_cod_empresa                  as character format "x(3)" label "Empresa" column-label "Empresa"
    field tta_cod_finalid_econ             as character format "x(10)" label "Finalidade" column-label "Finalidade"
    field tta_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field tta_cod_cta_ctbl                 as character format "x(20)" label "Conta Cont†bil" column-label "Conta Cont†bil"
    field tta_cod_plano_ccusto             as character format "x(8)" label "Plano Centros Custo" column-label "Plano Centros Custo"
    field tta_cod_ccusto                   as Character format "x(11)" label "Centro Custo" column-label "Centro Custo"
    field tta_cod_proj_financ              as character format "x(20)" label "Projeto" column-label "Projeto"
    field tta_cod_cenar_ctbl               as character format "x(8)" label "Cen†rio Cont†bil" column-label "Cen†rio Cont†bil"
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_unid_negoc               as character format "x(3)" label "Unid Neg¢cio" column-label "Un Neg"
    field tta_dat_sdo_ctbl                 as date format "99/99/9999" initial ? label "Data Saldo Cont†bil" column-label "Data Saldo Cont†bil"
    field tta_cod_unid_organ_orig          as character format "x(3)" label "UO Origem" column-label "UO Origem"
    field ttv_ind_espec_sdo                as character format "X(20)"
    field tta_val_sdo_ctbl_db              as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Movto DÇbito" column-label "Movto DÇbito"
    field tta_val_sdo_ctbl_cr              as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Movto CrÇdito" column-label "Movto CrÇdito"
    field tta_val_sdo_ctbl_fim             as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Cont†bil Final" column-label "Saldo Cont†bil Final"
    field tta_val_apurac_restdo            as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Apuraá∆o Resultado" column-label "Apuraá∆o Resultado"
    field tta_val_apurac_restdo_db         as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Apuraá∆o Restdo DB" column-label "Apuraá∆o Restdo DB"
    field tta_val_apurac_restdo_cr         as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Apuraá∆o Restdo CR" column-label "Apuraá∆o Restdo CR"
    field tta_val_apurac_restdo_acum       as decimal format "->>>>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Apuracao Final" column-label "Apuracao Final"
    field tta_val_movto_empenh             as decimal format "->>,>>>,>>>,>>9.99" decimals 9 initial 0 label "Movto Empenhado" column-label "Movto Empenhado"
    field tta_qtd_sdo_ctbl_db              as decimal format ">>>,>>>,>>9.99" decimals 2 initial 0 label "Quantidade DB" column-label "Quantidade DB"
    field tta_qtd_sdo_ctbl_cr              as decimal format ">>>,>>>,>>9.99" decimals 2 initial 0 label "Quantidade CR" column-label "Quantidade CR"
    field tta_qtd_sdo_ctbl_fim             as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Quantidade Final" column-label "Quantidade Final"
    field ttv_val_movto_ctbl               as decimal format ">>>,>>>,>>>,>>9.99" decimals 2
    field ttv_qtd_movto_ctbl               as decimal format "->>>>,>>9.9999" decimals 4
    field tta_qtd_movto_empenh             as decimal format "->>>>,>>9.9999" decimals 4 initial 0 label "Qtde Movto Empenhado" column-label "Qtde Movto Empenhado"
    field tta_val_orcado                   as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Oráado" column-label "Valor Oráado"
    field tta_val_orcado_sdo               as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Oráado" column-label "Saldo Oráado"
    field tta_qtd_orcado                   as decimal format "->>>>,>>9.9999" decimals 4 initial 0 label "Qtdade Oráada" column-label "Qtdade Oráada"
    field tta_qtd_orcado_sdo               as decimal format "->>>>,>>9.9999" decimals 4 initial 0 label "Saldo Quantidade" column-label "Saldo Quantidade"
    field ttv_rec_ret_sdo_ctbl             as recid format ">>>>>>9"
    index tt_id                            is primary unique
          tta_cod_empresa                  ascending
          tta_cod_finalid_econ             ascending
          tta_cod_plano_cta_ctbl           ascending
          tta_cod_cta_ctbl                 ascending
          tta_cod_plano_ccusto             ascending
          tta_cod_ccusto                   ascending
          tta_cod_proj_financ              ascending
          tta_cod_cenar_ctbl               ascending
          tta_cod_estab                    ascending
          tta_cod_unid_negoc               ascending
          tta_dat_sdo_ctbl                 ascending
          tta_num_seq                      ascending
          tta_cod_unid_organ_orig          ascending
    index tt_rec                          
          ttv_rec_ret_sdo_ctbl             ascending
    .
*/
def temp-table tt_sdo_cta_ctbl_balanct no-undo
    FIELD tta_cod_empresa                  LIKE sdo_cta_ctbl_ccusto.cod_empresa
    FIELD tta_cod_plano_cta_ctbl           LIKE cta_ctbl.cod_plano_cta_ctbl
    field tta_cod_cta_ctbl                 as character format "x(20)" label "Conta Cont†bil" column-label "Conta Cont†bil"
    FIELD tta_cod_plano_ccusto             LIKE ems5.ccusto.cod_plano_ccusto
    field tta_cod_ccusto                   as Character format "x(11)" label "Centro Custo" column-label "Centro Custo"
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_unid_negoc               as character format "x(3)" label "Unid Neg¢cio" column-label "Un Neg"
    field tta_val_sdo_ctbl_inic            as decimal format "->>>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Inicial" column-label "Saldo Inicial"
    field tta_val_sdo_ctbl_db              as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Movto DÇbito" column-label "Movto DÇbito"
    field tta_val_sdo_ctbl_cr              as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Movto CrÇdito" column-label "Movto CrÇdito"
    field tta_val_sdo_ctbl_fim             as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Cont†bil Final" column-label "Saldo Cont†bil Final"
    field ttv_val_sdo_ctbl_inic_ant        as decimal format "->>,>>>,>>>,>>9.99" decimals 2 label "Sdo Inicial Ant" column-label "Sdo Inicial Ant"
    field ttv_val_sdo_ctbl_fim_ant         as decimal format "->>,>>>,>>>,>>9.99" decimals 2 label "Sdo Final Ant" column-label "Sdo Final Ant"
    field tta_cod_proj_financ              as character format "x(20)" label "Projeto" column-label "Projeto"
    field ttv_val_sdo_ctbl_db_1            as decimal format "->>,>>>,>>>,>>9.99" decimals 2 label "DÇbito" column-label "DÇbito"
    field ttv_val_sdo_ctbl_cr_1            as decimal format "->>,>>>,>>>,>>9.99" decimals 2 label "CrÇdito" column-label "CrÇdito"
    .

def temp-table tt_sdo_cta_ctbl_balanct_aux no-undo
    field tta_cod_cta_ctbl                 as character format "x(20)" label "Conta Cont†bil" column-label "Conta Cont†bil"
    field tta_cod_ccusto                   as Character format "x(11)" label "Centro Custo" column-label "Centro Custo"
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_unid_negoc               as character format "x(3)" label "Unid Neg¢cio" column-label "Un Neg"
    field tta_val_sdo_ctbl_inic            as decimal format "->>>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Inicial" column-label "Saldo Inicial"
    field tta_val_sdo_ctbl_db              as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Movto DÇbito" column-label "Movto DÇbito"
    field tta_val_sdo_ctbl_cr              as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Movto CrÇdito" column-label "Movto CrÇdito"
    field tta_val_sdo_ctbl_fim             as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Cont†bil Final" column-label "Saldo Cont†bil Final"
    field ttv_val_sdo_ctbl_inic_ant        as decimal format "->>,>>>,>>>,>>9.99" decimals 2 label "Sdo Inicial Ant" column-label "Sdo Inicial Ant"
    field ttv_val_sdo_ctbl_fim_ant         as decimal format "->>,>>>,>>>,>>9.99" decimals 2 label "Sdo Final Ant" column-label "Sdo Final Ant"
    field tta_cod_proj_financ              as character format "x(20)" label "Projeto" column-label "Projeto"
    field ttv_val_sdo_ctbl_db_1            as decimal format "->>,>>>,>>>,>>9.99" decimals 2 label "DÇbito" column-label "DÇbito"
    field ttv_val_sdo_ctbl_cr_1            as decimal format "->>,>>>,>>>,>>9.99" decimals 2 label "CrÇdito" column-label "CrÇdito"
    field ttv_des_tit_ctbl_balanct         as character format "x(55)" column-label "T°tulo"
    .

def temp-table tt_sdo_cta_ctbl_balanct_aux2 no-undo
    field tta_cod_cta_ctbl                 as character format "x(20)" label "Conta Cont†bil" column-label "Conta Cont†bil"
    field tta_cod_ccusto                   as Character format "x(11)" label "Centro Custo" column-label "Centro Custo"
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_unid_negoc               as character format "x(3)" label "Unid Neg¢cio" column-label "Un Neg"
    field tta_val_sdo_ctbl_inic            as decimal format "->>>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Inicial" column-label "Saldo Inicial"
    field tta_val_sdo_ctbl_db              as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Movto DÇbito" column-label "Movto DÇbito"
    field tta_val_sdo_ctbl_cr              as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Movto CrÇdito" column-label "Movto CrÇdito"
    field tta_val_sdo_ctbl_fim             as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Cont†bil Final" column-label "Saldo Cont†bil Final"
    field ttv_val_sdo_ctbl_inic_ant        as decimal format "->>,>>>,>>>,>>9.99" decimals 2 label "Sdo Inicial Ant" column-label "Sdo Inicial Ant"
    field ttv_val_sdo_ctbl_fim_ant         as decimal format "->>,>>>,>>>,>>9.99" decimals 2 label "Sdo Final Ant" column-label "Sdo Final Ant"
    field tta_cod_proj_financ              as character format "x(20)" label "Projeto" column-label "Projeto"
    field ttv_val_sdo_ctbl_db_1            as decimal format "->>,>>>,>>>,>>9.99" decimals 2 label "DÇbito" column-label "DÇbito"
    field ttv_val_sdo_ctbl_cr_1            as decimal format "->>,>>>,>>>,>>9.99" decimals 2 label "CrÇdito" column-label "CrÇdito"
    field ttv_des_tit_ctbl_balanct         as character format "x(55)" column-label "T°tulo"
    .

def temp-table tt_sdo_cta_ctbl_sem_aprop no-undo
    field tta_cod_cta_ctbl                 as character format "x(20)" label "Conta Cont†bil" column-label "Conta Cont†bil"
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_unid_negoc               as character format "x(3)" label "Unid Neg¢cio" column-label "Un Neg"
    field tta_val_sdo_ctbl_inic            as decimal format "->>>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Inicial" column-label "Saldo Inicial"
    field tta_val_sdo_ctbl_db              as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Movto DÇbito" column-label "Movto DÇbito"
    field tta_val_sdo_ctbl_cr              as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Movto CrÇdito" column-label "Movto CrÇdito"
    field tta_val_sdo_ctbl_fim             as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Cont†bil Final" column-label "Saldo Cont†bil Final"
    field ttv_val_sdo_ctbl_inic_ant        as decimal format "->>,>>>,>>>,>>9.99" decimals 2 label "Sdo Inicial Ant" column-label "Sdo Inicial Ant"
    field ttv_val_sdo_ctbl_fim_ant         as decimal format "->>,>>>,>>>,>>9.99" decimals 2 label "Sdo Final Ant" column-label "Sdo Final Ant"
    field tta_cod_proj_financ              as character format "x(20)" label "Projeto" column-label "Projeto"
    index tt_id                            is primary
          tta_cod_cta_ctbl                 ascending
          tta_cod_estab                    ascending
          tta_cod_unid_negoc               ascending
    .

def new shared temp-table tt_unid_negocio no-undo
    field tta_cod_unid_negoc               as character format "x(3)" label "Unid Neg¢cio" column-label "Un Neg"
    field ttv_cod_unid_negoc_pai           as character format "x(3)" label "Un Neg Pai" column-label "Un Neg Pai"
    field ttv_log_proces                   as logical format "Sim/Nao" initial no label "&prc(" column-label "&prc("
    field ttv_ind_espec_unid_negoc         as character format "X(10)" label "EspÇcie UN" column-label "EspÇcie UN"
    index tt_cod_unid_negoc_pai           
          ttv_cod_unid_negoc_pai           ascending
    index tt_log_proces                   
          ttv_log_proces                   ascending
    index tt_select_id                     is primary unique
          tta_cod_unid_negoc               ascending
    .

def new shared temp-table tt_valor_demonst_ctbl_video no-undo
    field tta_cod_col_demonst_ctbl         as character format "x(2)" label "Coluna" column-label "Coluna"
    field ttv_rec_item_demonst_ctbl        as recid format ">>>>>>9"
    field ttv_val_coluna                   as decimal format "->>,>>>,>>>,>>9.99" decimals 2
    index tt_coluna                       
          tta_cod_col_demonst_ctbl         ascending
    index tt_id                            is primary unique
          tta_cod_col_demonst_ctbl         ascending
          ttv_rec_item_demonst_ctbl        ascending
    index tt_linha                        
          ttv_rec_item_demonst_ctbl        ascending
    .



/********************** Temporary Table Definition End **********************/

/************************** Buffer Definition Begin *************************/

def buffer btt_estab_unid_negoc_select
    for tt_estab_unid_negoc_select.
def buffer btt_retorna_sdo_ctbl_demonst
    for tt_retorna_sdo_ctbl_demonst.
def buffer b_dwb_rpt_select
    for dwb_rpt_select.
def buffer b_period_ctbl
    for period_ctbl.
def buffer b_sdo_cta_ctbl
    for sdo_cta_ctbl.
def buffer b_sdo_cta_ctbl_ccusto
    for sdo_cta_ctbl_ccusto.


/*************************** Buffer Definition End **************************/

/************************** Stream Definition Begin *************************/

def shared stream s_1.
/*def stream s_planilha.*/
DEF STREAM s-csv.



/*************************** Stream Definition End **************************/

/************************* Variable Definition Begin ************************/

def new global shared var v_cod_aplicat_dtsul_corren
    as character
    format "x(3)":U
    no-undo.
def shared var v_cod_arq_planilha
    as character
    format "x(40)":U
    label "Arq Planilha"
    column-label "Arq Planilha"
    no-undo.
def shared var v_cod_carac_lim
    as character
    format "x(1)":U
    initial ";"
    label "Caracter Delimitador"
    no-undo.
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
    label "Parte Fixa ccusto"
    column-label "Parte Fixa ccusto"
    no-undo.
def shared var v_cod_cenar_ctbl
    as character
    format "x(8)":U
    label "Cen†rio Cont†bil"
    column-label "Cen†rio Cont†bil"
    no-undo.
def var v_cod_cenar_orctario
    as character
    format "x(8)":U
    label "Cenar Orctario"
    column-label "Cen†rio Oráament†rio"
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
def shared var v_cod_dwb_program
    as character
    format "x(32)":U
    label "Programa"
    column-label "Programa"
    no-undo.
def shared var v_cod_dwb_select
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
def var v_cod_estab_fim
    as character
    format "x(3)":U
    initial "ZZZ"
    label "atÇ"
    column-label "Estab Final"
    no-undo.
def var v_cod_estab_ini
    as character
    format "x(3)":U
    label "Estabelecimento"
    column-label "Estab Inicial"
    no-undo.
def new global shared var v_cod_estab_usuar
    as character
    format "x(3)":U
    label "Estabelecimento"
    column-label "Estab"
    no-undo.
def shared var v_cod_exerc_ctbl
    as character
    format "9999":U
    label "Exerc°cio Cont†bil"
    column-label "Exerc°cio Cont†bil"
    no-undo.
def var v_cod_exerc_ctbl_aux
    as character
    format "9999":U
    label "Exerc°cio Cont†bil"
    column-label "Exerc°cio Cont†bil"
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
def var v_cod_grp_cta_ctbl_nok
    as character
    format "x(69)":U
    label "Grupo Conta Ctbl"
    column-label "Grupo Conta Ctbl"
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
def var v_cod_label
    as character
    format "x(8)":U
    label "Label"
    column-label "Label"
    no-undo.
def var v_cod_label_1
    as character
    format "x(16)":U
    column-label "Campo"
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
def new global shared var v_cod_pag_folha
    as character
    format "x(6)":U
    initial "P†gina" /*l_pagina*/
    view-as radio-set Horizontal
    radio-buttons "P†gina", "P†gina","Folha", "Folha"
     /*l_pagina*/ /*l_pagina*/ /*l_folha*/ /*l_folha*/
    bgcolor 8 
    label "Label Cabeáalho"
    column-label "Label Cabeáalho"
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
    label "Plano ccusto"
    column-label "Plano ccusto"
    no-undo.
def new global shared var v_cod_plano_ccusto_corren
    as character
    format "x(8)":U
    label "Plano ccusto"
    column-label "Plano ccusto"
    no-undo.
def shared var v_cod_plano_cta_ctbl
    as character
    format "x(8)":U
    label "Plano Contas"
    column-label "Plano Contas"
    no-undo.
def var v_cod_proj_financ_fim
    as character
    format "x(20)":U
    initial "ZZZZZZZZZZZZZZZZZZZZ"
    label "Projeto Final"
    column-label "Projeto"
    no-undo.
def var v_cod_proj_financ_ini
    as character
    format "x(20)":U
    label "Inicial"
    column-label "Projeto"
    no-undo.
def shared var v_cod_release
    as character
    format "x(12)":U
    no-undo.
def shared var v_cod_tempo_exec
    as character
    format "x(8)":U
    label "Tempo Execuá∆o"
    column-label "Tempo"
    no-undo.
def var v_cod_unid_negoc_fim
    as character
    format "x(3)":U
    initial "ZZZ"
    label "atÇ"
    column-label "Final"
    no-undo.
def var v_cod_unid_negoc_ini
    as character
    format "x(3)":U
    label "Unid Neg¢cio"
    column-label "Inicial"
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
def var v_cod_unid_orctaria
    as character
    format "x(8)":U
    label "Unid Oráament†ria"
    column-label "Unid Oráament†ria"
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
def var v_cod_vers_orcto_ctbl
    as character
    format "x(10)":U
    label "Vers∆o Oráamento"
    column-label "Vers∆o Oráamento"
    no-undo.
def shared var v_dat_cotac_indic_econ
    as date
    format "99/99/9999":U
    initial today
    label "Data Cotaá∆o"
    column-label "Data Cotaá∆o"
    no-undo.
def shared var v_dat_execution
    as date
    format "99/99/9999":U
    no-undo.
def shared var v_dat_execution_end
    as date
    format "99/99/9999":U
    no-undo.
def shared var v_dat_fim_period
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
def shared var v_dat_inic_period
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
def var v_dat_inic_period_ctbl_ant
    as date
    format "99/99/9999":U
    label "In°cio Per°odo"
    column-label "In°cio Per°odo"
    no-undo.
def shared var v_des_lista_estab
    as character
    format "x(2000)":U
    view-as editor max-chars 2000 scrollbar-vertical
    size 80 by 12
    bgcolor 15 font 2
    label "Estabelecimentos"
    column-label "Estabelecimentos"
    no-undo.
def var v_des_lista_termo_abert
    as character
    format "x(60)":U
    no-undo.
def var v_des_lista_termo_encert
    as character
    format "x(60)":U
    no-undo.
def var v_des_percent_complete
    as character
    format "x(06)":U
    no-undo.
def var v_des_percent_complete_fnd
    as character
    format "x(08)":U
    no-undo.
def new global shared var v_des_termo_abert_dia
    as character
    format "x(2000)":U
    view-as editor max-chars 2000 scrollbar-vertical
    size 60 by 3
    bgcolor 15 font 2
    label "Termo Abertura"
    column-label "Termo Abertura"
    no-undo.
def var v_des_termo_diario
    as character
    format "x(2000)":U
    view-as editor max-chars 2000 scrollbar-vertical
    size 60 by 3
    bgcolor 15 font 2
    no-undo.
def new global shared var v_des_termo_encert_dia
    as character
    format "x(2000)":U
    view-as editor max-chars 2000 scrollbar-vertical
    size 60 by 3
    bgcolor 15 font 2
    label "Termo Encerramento"
    column-label "Termo Encerramento"
    no-undo.
def shared var v_hra_execution
    as Character
    format "99:99":U
    no-undo.
def shared var v_hra_execution_end
    as Character
    format "99:99:99":U
    label "Tempo Exec"
    no-undo.
def var v_ind_cr_sdo_fim_1
    as character
    format "X(01)":U
    no-undo.
def var v_ind_cr_sdo_fim_2
    as character
    format "X(01)":U
    no-undo.
def var v_ind_cr_sdo_inic_1
    as character
    format "X(01)":U
    no-undo.
def var v_ind_cr_sdo_inic_2
    as character
    format "X(01)":U
    no-undo.
def var v_ind_tip_plano_cta_ctbl
    as character
    format "X(12)":U
    view-as combo-box
    list-items "Prim†rio","Secund†rio","Consolidaá∆o","Oráament†rio"
     /*l_primario*/ /*l_secundario*/ /*l_consolidacao*/ /*l_orcamentario*/
    inner-lines 5
    bgcolor 15 font 2
    label "Tipo Plano Contas"
    column-label "Tipo Plano Contas"
    no-undo.
def shared var v_log_ccusto_sum
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    label "Sumaria ccusto"
    column-label "Sum ccusto"
    no-undo.


def new global shared var v_log_cenar_fisc
    as logical
    format "Sim/N∆o"
    initial yes
    no-undo.
def shared var v_log_consid_apurac_restdo
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    label "Consid Apurac Restdo"
    column-label "Apurac Restdo"
    no-undo.
def new global shared var v_log_control_pag_livro
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    label "Contr Pag Livro"
    column-label "Contr Pag Livro"
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
def var v_log_fim
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    label "Operaá∆o  Final"
    no-undo.
def shared var v_log_gerac_planilha
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    label "Gera Planilha"
    no-undo.
def var v_log_idioma
    as logical
    format "Sim/N∆o"
    initial yes
    no-undo.
def new global shared var v_log_impr_termo_encert
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    label "Impr Termo Encert"
    column-label "Impr Termo Encert"
    no-undo.
def var v_log_livro_novo
    as logical
    format "Sim/N∆o"
    initial no
    no-undo.
def var v_log_method
    as logical
    format "Sim/N∆o"
    initial yes
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
def var v_log_plano_ccusto_uni
    as logical
    format "Sim/N∆o"
    initial no
    no-undo.
def shared var v_log_proj_financ
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    label "Sumaria Projeto"
    no-undo.
def var v_log_restric_estab
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    label "Usa Segur Estab"
    column-label "Usa Segur Estab"
    no-undo.
def shared var v_log_unid_negoc_sum
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    label "Sumaria Unid Negoc"
    column-label "Sum Un Neg"
    no-undo.
def shared var v_nom_enterprise
    as character
    format "x(40)":U
    no-undo.
def new global shared var v_nom_prog
    as character
    format "x(8)":U
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
def shared var v_nom_prog_ext
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
def shared var v_nom_report_title
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
def var v_num_aux_1
    as integer
    format ">>>>>9":U
    initial 1
    no-undo.
def var v_num_aux_2
    as integer
    format ">>>>>9":U
    initial 1
    no-undo.
def var v_num_cont
    as integer
    format ">,>>9":U
    initial 0
    no-undo.
def new global shared var v_num_livro_fisc
    as integer
    format ">>>>,>>9":U
    no-undo.
def new global shared var v_num_livro_fisc_2
    as integer
    format ">>>>,>>9":U
    no-undo.
def shared var v_num_niv_estrut
    as integer
    format ">>9":U
    label "N°vel Estrutura"
    no-undo.
def shared var v_num_page_number
    as integer
    format ">>>>>9":U
    label "P†gina"
    column-label "P†gina"
    no-undo.
def new global shared var v_num_pagina
    as integer
    format ">>>9":U
    label "P†gina"
    column-label "P†gina"
    no-undo.
def new global shared var v_num_pag_livro_fisc
    as integer
    format ">>9":U
    initial 0
    label "Pag Livro Fisc"
    column-label "Pag Livro Fisc"
    no-undo.
def new global shared var v_num_ped_exec_corren
    as integer
    format ">>>>9":U
    no-undo.
def shared var v_num_period_ctbl
    as integer
    format ">99":U
    initial 01
    label "Per°odo Atual"
    column-label "Period"
    no-undo.
def var v_num_period_ctbl_aux
    as integer
    format ">>>>,>>9":U
    no-undo.
def new global shared var v_num_primei_pag
    as integer
    format ">,>>9":U
    label "Primeira P†gina"
    column-label "Primeira P†gina"
    no-undo.
def var v_num_seq
    as integer
    format ">>>,>>9":U
    label "SeqÅància"
    column-label "Seq"
    no-undo.
def var v_num_seq_aux
    as integer
    format ">>>>,>>9":U
    no-undo.
def var v_num_seq_orcto_ctbl
    as integer
    format ">>>>>>>>9":U
    label "Seq Orcto Cont†bil"
    column-label "Seq Orcto Cont†bil"
    no-undo.
def new global shared var v_num_ult_pag_tel
    as integer
    format ">>>,>>9":U
    label "Èltima P†gina"
    column-label "Èltima P†gina"
    no-undo.
def shared var v_qtd_line_aux
    as decimal
    format "->>>>,>>9.9999":U
    decimals 4
    initial 66
    no-undo.
def shared var v_rec_dwb_rpt_param
    as recid
    format ">>>>>>9":U
    no-undo.
def var v_rec_table_epc
    as recid
    format ">>>>>>9":U
    no-undo.
def var v_val_apurac_restdo_505
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Apuraá∆o Resultado"
    column-label "Apuraá∆o Resultado"
    no-undo.
def var v_val_apurac_restdo_acum_505
    as decimal
    format "->>>>,>>>,>>>,>>9.99":U
    decimals 2
    label "Apuracao Final"
    column-label "Apuracao Final"
    no-undo.
def var v_val_apurac_restdo_cr_505
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Apuraá∆o Restdo CR"
    column-label "Apuraá∆o Restdo CR"
    no-undo.
def var v_val_apurac_restdo_db_505
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Apuraá∆o Restdo DB"
    column-label "Apuraá∆o Restdo DB"
    no-undo.
def var v_val_sdo_cta_tot_1
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Saldo Inicial"
    column-label "Saldo Inicial"
    no-undo.
def var v_val_sdo_cta_tot_2
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "DÇbito"
    column-label "DÇbito"
    no-undo.
def var v_val_sdo_cta_tot_3
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "CrÇdito"
    column-label "CrÇdito"
    no-undo.
def var v_val_sdo_cta_tot_4
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Sdo Inicial Ant"
    column-label "Sdo Inicial Ant"
    no-undo.
def var v_val_sdo_cta_tot_5
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Sdo Final Ant"
    column-label "Sdo Final Ant"
    no-undo.
def var v_val_sdo_cta_tot_6
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Saldo Final"
    column-label "Saldo Final"
    no-undo.
def var v_val_sdo_ctbl_cr_505
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Movto CrÇdito"
    column-label "Movto CrÇdito"
    no-undo.
def var v_val_sdo_ctbl_db_505
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Movto DÇbito"
    column-label "Movto DÇbito"
    no-undo.
def var v_val_sdo_ctbl_fim_505
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Saldo Cont†bil Final"
    column-label "Saldo Cont†bil Final"
    no-undo.
def var v_val_tot_sdo_fim_apurac
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    no-undo.
def var v_val_tot_sdo_fim_dem
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    no-undo.
def var v_val_tot_sdo_inic_apurac
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    no-undo.
def var v_val_tot_sdo_inic_dem
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    no-undo.
def var v_wgh_frame_epc
    as widget-handle
    format ">>>>>>9":U
    no-undo.
def var v_cod_ccusto                     as character       no-undo. /*local*/
def var v_cod_ccusto_aux                 as character       no-undo. /*local*/
def var v_cod_estab                      as character       no-undo. /*local*/
def var v_cod_grp_cta_ctbl               as character       no-undo. /*local*/
def var v_cod_indic_econ_apres           as character       no-undo. /*local*/
def var v_cod_indic_econ_base            as character       no-undo. /*local*/
def var v_cod_proj_financ                as character       no-undo. /*local*/
def var v_cod_proj_financ_max            as character       no-undo. /*local*/
def var v_cod_return                     as character       no-undo. /*local*/
def var v_cod_tip_grp_cta_ctbl           as character       no-undo. /*local*/
def var v_cod_unid_negoc                 as character       no-undo. /*local*/
def var v_dat_fim_period_ctbl_ant        as date            no-undo. /*local*/
def var v_des_cabec_planilha             as character       no-undo. /*local*/
def var v_des_carac_lim                  as character       no-undo. /*local*/
def var v_des_tit_ctbl_balanct           as character       no-undo. /*local*/
def var v_des_tit_ctbl_compl             as character       no-undo. /*local*/
def var v_ind_cr_sdo_fim                 as character       no-undo. /*local*/
def var v_ind_cr_sdo_fim_ant             as character       no-undo. /*local*/
def var v_ind_cr_sdo_inic                as character       no-undo. /*local*/
def var v_ind_cr_sdo_inic_ant            as character       no-undo. /*local*/
def var v_ind_tip_sdo                    as character       no-undo. /*local*/
def var v_log_exec_sdo_cta_ctbl_balanct  as logical         no-undo. /*local*/
def var v_log_impr_tot                   as logical         no-undo. /*local*/
def var v_log_sdo_ccusto                 as logical         no-undo. /*local*/
def var v_log_sdo_cta_ctbl_ccusto        as logical         no-undo. /*local*/
def var v_log_tot_balanct                as logical         no-undo. /*local*/
def var v_num_dwb_order                  as integer         no-undo. /*local*/
def var v_num_max                        as integer         no-undo. /*local*/
def var v_num_niv_aux                    as integer         no-undo. /*local*/
def var v_num_proj_financ_max            as integer         no-undo. /*local*/
def var v_num_seq_sdo                    as integer         no-undo. /*local*/
def var v_num_tempo_exec                 as integer         no-undo. /*local*/
def var v_num_tit_ctbl                   as integer         no-undo. /*local*/
def var v_num_ult_pag                    as integer         no-undo. /*local*/
def var v_val_cotac_indic_econ           as decimal         no-undo. /*local*/
def var v_val_current_value              as decimal         no-undo. /*local*/
def var v_val_maximum                    as decimal         no-undo. /*local*/
def var v_val_sdo_cta_ctbl_cr            as decimal         no-undo. /*local*/
def var v_val_sdo_cta_ctbl_db            as decimal         no-undo. /*local*/
def var v_val_sdo_cta_ctbl_fim           as decimal         no-undo. /*local*/
def var v_val_sdo_cta_ctbl_fim_ant       as decimal         no-undo. /*local*/
def var v_val_sdo_cta_ctbl_inic          as decimal         no-undo. /*local*/
def var v_val_sdo_cta_ctbl_inic_ant      as decimal         no-undo. /*local*/
def var v_val_tot_sdo_ctbl_cr            as decimal         no-undo. /*local*/
def var v_val_tot_sdo_ctbl_db            as decimal         no-undo. /*local*/


/************************** Variable Definition End *************************/

/************************ Rectangle Definition Begin ************************/

def rectangle rt_001
    size 1 by 1
    edge-pixels 2.


/************************* Rectangle Definition End *************************/

/************************** Button Definition Begin *************************/

def button bt_can2
    label "Cancela"
    tooltip "Cancela"
    size 1 by 1.
/****************************** Function Button *****************************/


/*************************** Button Definition End **************************/

/************************** Report Definition Begin *************************/

def new shared var v_rpt_s_1_lines as integer initial 0.
def new shared var v_rpt_s_1_columns as integer initial 194.
def new shared var v_rpt_s_1_bottom as integer initial 0.
def new shared var v_rpt_s_1_page as integer.
def new shared var v_rpt_s_1_name as character initial "Planilha do Balancete Ctbl".
def frame f_rpt_s_1_Grp_cabec_Lay_cabec header skip skip
    with no-box no-labels width 194 page-top stream-io.
def frame f_rpt_s_1_Grp_cabec_Lay_dms_grps header
    skip (1) skip skip skip
    with no-box no-labels width 194 page-top stream-io.
def frame f_rpt_s_1_Grp_cabec_Lay_Lucr_Per header
    skip (1) skip skip skip
    with no-box no-labels width 194 page-top stream-io.
def frame f_rpt_s_1_Grp_cabec_Lay_period header
    skip (1) skip skip
    with no-box no-labels width 194 page-top stream-io.
def frame f_rpt_s_1_Grp_cabec_Lay_period_ant header
    skip (1) skip skip
    with no-box no-labels width 194 page-top stream-io.
def frame f_rpt_s_1_Grp_cabec_Lay_tot header skip skip skip
    with no-box no-labels width 194 page-top stream-io.
def frame f_rpt_s_1_Grp_unico_Lay_sdo_cta header
    trim(v_des_tit_ctbl_balanct) at 1
    trim(v_des_tit_ctbl_compl) at 63
    /* Atributo tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_inic ignorado */
    v_cod_carac_lim at 104 format "x(1)" view-as text
    /* Atributo tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_db ignorado */
    v_cod_carac_lim at 127 format "x(1)" view-as text
    /* Atributo tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_cr ignorado */
    v_cod_carac_lim at 150 format "x(1)" view-as text
    /* Atributo tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim ignorado */
    v_cod_carac_lim at 173 format "x(1)" view-as text
    /* Atributo cta_ctbl.ind_espec_cta_ctbl ignorado */ skip
    with no-box no-labels width 194 page-top stream-io.
def frame f_rpt_s_1_Grp_unico_Lay_sdo_cta_ant header
    trim(v_des_tit_ctbl_balanct) at 1
    trim(v_des_tit_ctbl_compl) at 62
    /* Atributo tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant ignorado */
    v_cod_carac_lim at 104 format "x(1)" view-as text
    /* Atributo tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant ignorado */
    v_cod_carac_lim at 127 format "x(1)" view-as text
    /* Atributo tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_inic ignorado */
    v_cod_carac_lim at 150 format "x(1)" view-as text
    /* Atributo tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim ignorado */
    v_cod_carac_lim at 173 format "x(1)" view-as text
    /* Atributo cta_ctbl.ind_espec_cta_ctbl ignorado */ skip
    with no-box no-labels width 194 page-top stream-io.
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
    "Plano Contas: " at 38
    v_cod_plano_cta_ctbl at 52 format "x(8)" view-as text skip
    "Plano ems5.ccusto: " at 38
    v_cod_plano_ccusto at 52 format "x(8)" view-as text skip
    "Cen†rio Cont†bil: " at 34
    v_cod_cenar_ctbl at 52 format "x(8)" view-as text skip
    "Exerc°cio Cont†bil: " at 32
    v_cod_exerc_ctbl at 52 format "9999" view-as text skip
    "Per°odo Atual: " at 37
    v_num_period_ctbl to 54 format ">99" view-as text skip
    "Sumaria Estab: " at 37
    v_log_estab_sum at 52 format "Sim/N∆o" view-as text skip
    "Sumaria Unid Negoc: " at 32
    v_log_unid_negoc_sum at 52 format "Sim/N∆o" view-as text skip
    "Sumaria ems5.ccusto: " at 36
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
    "Parte Fixa ems5.ccusto: " at 33
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
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_parametros_Lay_param_505 header
    skip (1)
    "Plano Contas: " at 38
    v_cod_plano_cta_ctbl at 52 format "x(8)" view-as text skip
    "Plano ems5.ccusto: " at 38
    v_cod_plano_ccusto at 52 format "x(8)" view-as text skip
    "Cen†rio Cont†bil: " at 34
    v_cod_cenar_ctbl at 52 format "x(8)" view-as text skip
    "Exerc°cio Cont†bil: " at 32
    v_cod_exerc_ctbl at 52 format "9999" view-as text skip
    "Per°odo Atual: " at 37
    v_num_period_ctbl to 54 format ">99" view-as text skip
    "Sumaria Estab: " at 37
    v_log_estab_sum at 52 format "Sim/N∆o" view-as text skip
    "Sumaria Unid Negoc: " at 32
    v_log_unid_negoc_sum at 52 format "Sim/N∆o" view-as text skip
    "Sumaria Projeto: " at 35
    v_log_proj_financ at 52 format "Sim/N∆o" view-as text skip
    "Sumaria ccusto: " at 36
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
    "Parte Fixa ems5.ccusto: " at 33
    v_cod_ccusto_pfixa at 52 format "x(11)" view-as text skip
    "Parte Exceá∆o: " at 37
    v_cod_ccusto_excec at 52 format "x(11)" view-as text
    /* Vari†vel v_cod_proj_financ_pfixa ignorada. N∆o esta definida no programa */
    /* Vari†vel v_cod_proj_financ_excec ignorada. N∆o esta definida no programa */
    skip (2)
    "Gera Planilha: " at 37
    v_log_gerac_planilha at 52 format "Sim/N∆o" view-as text skip
    "Arq Planilha: " at 38
    v_cod_arq_planilha at 52 format "x(40)" view-as text skip
    "Caracter Delimitador: " at 30
    v_cod_carac_lim at 52 format "x(1)" view-as text skip
    "Tempo Execuá∆o: " at 36
    v_cod_tempo_exec at 52 format "x(8)" view-as text skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_parametros_Lay_segur_estab header
    skip (1)
    entry(1, return-value, chr(255)) at 15 format "x(80)" skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_termo_Lay_termo header
    entry(1, return-value, chr(255)) at 35 format "x(60)" skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_tot_balanct_Lay_demai_grp header
    skip (1)
    "Lucros e Perdas" at 1
    v_val_tot_sdo_inic_apurac to 99 format "->>,>>>,>>>,>>9.99" view-as text
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */
    v_val_tot_sdo_fim_apurac to 156 format "->,>>>,>>>,>>9.99" view-as text
    /* Vari†vel v_ind_cr_sdo_fim ignorada. N∆o esta definida no programa */ skip
    v_cod_grp_cta_ctbl_nok at 1 format "x(69)" view-as text
    v_val_tot_sdo_inic_dem to 99 format "->>,>>>,>>>,>>9.99" view-as text
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */
    v_val_tot_sdo_fim_dem to 156 format "->,>>>,>>>,>>9.99" view-as text
    /* Vari†vel v_ind_cr_sdo_fim ignorada. N∆o esta definida no programa */ skip
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
    v_val_tot_sdo_inic_apurac to 99 format "->>,>>>,>>>,>>9.99" view-as text
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */
    v_val_tot_sdo_fim_apurac to 156 format "->>,>>>,>>>,>>9.99" view-as text
    /* Vari†vel v_ind_cr_sdo_fim ignorada. N∆o esta definida no programa */ skip
    v_cod_grp_cta_ctbl_nok at 1 format "x(69)" view-as text
    v_val_tot_sdo_inic_dem to 99 format "->>,>>>,>>>,>>9.99" view-as text
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */
    v_val_tot_sdo_fim_dem to 156 format "->>,>>>,>>>,>>9.99" view-as text
    /* Vari†vel v_ind_cr_sdo_fim ignorada. N∆o esta definida no programa */ skip
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
    v_val_tot_sdo_inic_apurac to 73 format "->,>>>,>>>,>>9.99" view-as text
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */
    v_val_tot_sdo_fim_apurac to 131 format "->,>>>,>>>,>>9.99" view-as text
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */ skip
    v_cod_grp_cta_ctbl_nok at 1 format "X(55)" view-as text
    v_val_tot_sdo_inic_dem to 73 format "->,>>>,>>>,>>9.99" view-as text
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */
    v_val_tot_sdo_fim_dem to 131 format "->,>>>,>>>,>>9.99" view-as text
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */ skip
    with no-box no-labels width 157 page-top stream-io.
def frame f_rpt_s_1_Grp_tot_blnct_di_Lay_lucr_perd_an header
    skip (1)
    "Lucros e Perdas" at 1
    v_val_tot_sdo_inic_apurac to 113 format "->>>,>>>,>>9.99" view-as text
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */
    v_val_tot_sdo_fim_apurac to 131 format "->,>>>,>>>,>>9.99" view-as text
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */ skip
    v_cod_grp_cta_ctbl_nok at 1 format "x(55)" view-as text
    v_val_tot_sdo_inic_dem to 113 format "->>>,>>>,>>9.99" view-as text
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */
    v_val_tot_sdo_fim_dem to 131 format "->,>>>,>>>,>>9.99" view-as text
    /* Vari†vel v_ind_cr_sdo_inic ignorada. N∆o esta definida no programa */ skip
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


/*************************** Report Definition End **************************/

/************************** Frame Definition Begin **************************/

def frame f_dlg_02_percent_update
    rt_001
         at row 01.29 col 02.00
    " Percentual Completo " view-as text
         at row 01.00 col 04.00
    v_des_percent_complete_fnd
         at row 02.04 col 03.00 no-label
         view-as fill-in
         size-chars 9.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_des_percent_complete
         at row 02.04 col 03.00 no-label
         view-as fill-in
         size-chars 7.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_can2
         at row 03.50 col 20.00 font ?
         help "Cancela"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 50.00 by 05.00
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "".
    /* adjust size of objects in this frame */
    assign bt_can2:width-chars  in frame f_dlg_02_percent_update = 10.00
           bt_can2:height-chars in frame f_dlg_02_percent_update = 01.00
           rt_001:width-chars   in frame f_dlg_02_percent_update = 46.72
           rt_001:height-chars  in frame f_dlg_02_percent_update = 01.92.
    /* set private-data for the help system */
    assign v_des_percent_complete_fnd:private-data in frame f_dlg_02_percent_update = "HLP=000022169":U
           v_des_percent_complete:private-data     in frame f_dlg_02_percent_update = "HLP=000022167":U
           bt_can2:private-data                    in frame f_dlg_02_percent_update = "HLP=000011451":U
           frame f_dlg_02_percent_update:private-data                               = "HLP=000010975".



/*************************** Frame Definition End ***************************/

/*********************** User Interface Trigger Begin ***********************/


ON CHOOSE OF bt_can2 IN FRAME f_dlg_02_percent_update
DO:

    hide frame f_dlg_02_percent_update.

    stop.
END. /* ON CHOOSE OF bt_can2 IN FRAME f_dlg_02_percent_update */


/************************ User Interface Trigger End ************************/

/**************************** Frame Trigger Begin ***************************/


ON HELP OF FRAME f_dlg_02_percent_update ANYWHERE
DO:


    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */

END. /* ON HELP OF FRAME f_dlg_02_percent_update */

ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_02_percent_update ANYWHERE
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

END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_02_percent_update */

ON RIGHT-MOUSE-UP OF FRAME f_dlg_02_percent_update ANYWHERE
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

END. /* ON RIGHT-MOUSE-UP OF FRAME f_dlg_02_percent_update */

ON WINDOW-CLOSE OF FRAME f_dlg_02_percent_update
DO:

    apply "end-error" to self.
END. /* ON WINDOW-CLOSE OF FRAME f_dlg_02_percent_update */


/***************************** Frame Trigger End ****************************/


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
    run pi_version_extract ('fnc_sdo_cta_ctbl_balanct_impr', 'prgfin/fgl/fgl900c.p', '1.00.02.058', 'pro').
end /* if */.
/* End_Include: i_version_extract */

/* Begin_Include: i_verifica_funcao_tot_balanct */
&if defined(BF_FIN_TOT_BALANCT) &then
    assign v_log_tot_balanct = yes.
&else
    &if '{&emsfin_version}' >= '5.04' &then
        find histor_exec_especial no-lock
            where histor_exec_especial.cod_modul_dtsul = 'UFN'
            and   histor_exec_especial.cod_prog_dtsul  = 'SPP_TOT_BALANCT'
            no-error.
        if  avail histor_exec_especial then
            assign v_log_tot_balanct = yes.

        /* Begin_Include: i_funcao_extract */
        /*if  v_cod_arq <> '' and v_cod_arq <> ?
        then do:

            output stream s-arq to value(v_cod_arq) append.

            put stream s-arq unformatted
                "SPP_TOT_BALANCT" /* l_spp_tot_balanct*/      at 1 
                v_log_tot_balanct  at 43 skip.

            output stream s-arq close.

        end /* if */.*/
        /* End_Include: i_funcao_extract */

    &endif
&endif

/* End_Include: i_funcao_extract */

for each tt_sdo_cta_ctbl_balanct_aux:
    delete tt_sdo_cta_ctbl_balanct_aux.
end.

if  PROGRAM-NAME(2) begins "pi_rpt_lancto_ctbl_diario_contabil" /*l_pi_rpt_lancto_ctbl_diario_ctbl*/ 
then do:
    assign v_nom_prog = "Di†rio" /*l_diario*/ 
           v_des_lista_termo_abert  = v_des_termo_abert_dia
           v_des_lista_termo_encert = v_des_termo_encert_dia.

    /* Begin_Include: i_define_header_blct_diar */
    def frame f_rpt_s_1_header_period_diario header
        "------------------------------------------------------------" at 1
        "--------------------------------------------------" at 61
        "---------" at 111
        trim(v_cod_label) + ':' at 121
        (page-number (s_1) + v_num_pagina) to 132 format ">>>9" skip
        v_nom_enterprise at 1 format "x(40)"
        fill(" ", 40 - length(trim(v_nom_report_title))) + trim(v_nom_report_title) to 132 format "x(40)" skip
        "Per°odo: " at 1
        v_dat_inic_period at 10 format "99/99/9999"
        "A" at 21
        v_dat_fim_period at 23 format "99/99/9999"
        "------------------------------------------------------------" at 34
        "--------------------" at 94
        v_dat_execution at 115 format "99/99/9999"
        "-" at 126
        v_hra_execution at 128 format "99:99" skip (1)
        with no-box no-labels width 132 page-top stream-io.
    def frame f_rpt_s_1_header_unique_diario header
        "------------------------------------------------------------" at 1
        "--------------------------------------------------" at 61
        "---------" at 111
        trim(v_cod_label) + ':' at 121
        (page-number (s_1) + v_num_pagina) to 132 format ">>>9" skip
        v_nom_enterprise at 1 format "x(40)"
        fill(" ", 40 - length(trim(v_nom_report_title))) + trim(v_nom_report_title) to 132 format "x(40)" skip
        "------------------------------------------------------------" at 1
        "--------------------------------------------------" at 61
        "---" at 111
        v_dat_execution at 115 format "99/99/9999"
        "-" at 126
        v_hra_execution at 128 format "99:99" skip (1)
        with no-box no-labels width 132 page-top stream-io.
    def frame f_rpt_s_1_footer_last_page_diario header
        skip (1)
        trim(v_cod_label_1) at 1
        "------------------------------------------------------------" at 15
        "------------------------------" at 75
        "----" at 105
        v_nom_prog_ext at 110 format "x(8)"
        "-" at 119
        v_cod_release at 121 format "x(12)" skip
        with no-box no-labels width 132 page-bottom stream-io.
    def frame f_rpt_s_1_footer_normal_diario header
        skip (1)
        "------------------------------------------------------------" at 1
        "-----------------------------------------------" at 61
        "-" at 109
        v_nom_prog_ext at 111 format "x(8)"
        "-" at 119
        v_cod_release at 121 format "x(12)" skip
        with no-box no-labels width 132 page-bottom stream-io.
    def frame f_rpt_s_1_footer_param_page_diario header
        skip (1)
        "P†gina ParÉmetros" at 1
        "------------------------------------------------------------" at 19
        "---------------------------" at 79
        "---" at 107
        v_nom_prog_ext at 111 format "x(8)"
        "-" at 119
        v_cod_release at 121 format "x(12)" skip
        with no-box no-labels width 132 page-bottom stream-io.

        /*hide stream s_1 frame f_rpt_s_1_header_unique_diario.
        view stream s_1 frame f_rpt_s_1_header_period_diario.
        hide stream s_1 frame f_rpt_s_1_footer_last_page_diario.
        hide stream s_1 frame f_rpt_s_1_footer_param_page_diario.
        view stream s_1 frame f_rpt_s_1_footer_normal_diario.*/

    /* End_Include: i_define_header_blct_diar */
    .
end /* if */.
else assign v_nom_prog = "Balancete" /*l_balancete*/ .

if v_cod_pag_folha = "P†gina" /*l_pagina*/  then 
    assign v_cod_label   = "P†gina" /*l_pagina*/ 
           v_cod_label_1 = "Èltima p†gina" /*l_last_page*/ .
else
    assign v_cod_label   = "Folha" /*l_folha*/ 
           v_cod_label_1 = "Èltima Folha" /*l_ultima_folha*/ .

assign v_num_tempo_exec = etime(yes)
       v_rpt_s_1_bottom = v_qtd_line_aux
       v_des_cabec_planilha = "T°tulo Cont†bil" + v_cod_carac_lim
       v_des_carac_lim  = v_cod_carac_lim
       v_cod_cta_ctbl_pfixa = replace (v_cod_cta_ctbl_pfixa, '#', '.')
       v_cod_cta_ctbl_excec = replace (v_cod_cta_ctbl_excec, '#', '.')
       v_cod_ccusto_pfixa   = replace (v_cod_ccusto_pfixa, '#', '.')
       v_cod_ccusto_excec   = replace (v_cod_ccusto_excec, '#', '.')
       v_des_lista_estab    = "".

if v_nom_prog = "Di†rio" /*l_diario*/ 
   then assign v_rpt_s_1_name = "Di†rio" /*l_diario*/ .

/* --- Verifica se est† parametrizado para utilizar o controle de restriá∆o
      de acesso por estabelecimentos relacionados ao usu†rio corrente  ---*/
assign v_log_restric_estab = no.
run pi_retorna_usa_segur_estab (Input "",
                                output v_log_restric_estab) /*pi_retorna_usa_segur_estab*/.

/* --- Otimiza a impress∆o ---*/
&IF DEFINED(BF_ADM_FIN_PROJ) &THEN
    find last emsuni.param_geral_ems no-lock.
    if avail emsuni.param_geral_ems then
        assign v_cod_format_proj_financ = emsuni.param_geral_ems.cod_format_proj_financ.

    assign v_num_max = 0.
    for each proj_financ no-lock:
        if length(proj_financ.cod_proj_financ) > v_num_max then do:
            assign v_cod_proj_financ_max = trim(string(proj_financ.cod_proj_financ,v_cod_format_proj_financ))
                   v_num_max = length(proj_financ.cod_proj_financ).
        end.
    end.
    assign v_num_proj_financ_max = length(v_cod_proj_financ_max).

    assign v_num_tit_ctbl = 60 - (v_num_proj_financ_max + 1).
&ELSE
    assign v_num_tit_ctbl = 60.
&ENDIF
if v_log_estab_sum = yes then assign v_num_tit_ctbl = v_num_tit_ctbl + 4.
else assign v_des_carac_lim      =  v_des_carac_lim + v_cod_carac_lim
            v_des_cabec_planilha = v_des_cabec_planilha + "Est" + v_cod_carac_lim.
if (v_log_unid_negoc_sum = yes) then assign v_num_tit_ctbl = v_num_tit_ctbl + 4.
else assign v_des_carac_lim      = v_des_carac_lim + v_cod_carac_lim
            v_des_cabec_planilha = v_des_cabec_planilha + "Un N" + v_cod_carac_lim.
if (v_log_ccusto_sum = yes) then assign v_num_tit_ctbl = v_num_tit_ctbl + 12.
else assign v_des_carac_lim      =  v_des_carac_lim + v_cod_carac_lim
            v_des_cabec_planilha = v_des_cabec_planilha + "CCusto" + v_cod_carac_lim.
&IF DEFINED(BF_ADM_FIN_PROJ) &THEN
if (v_log_proj_financ = yes) then assign v_num_tit_ctbl = v_num_tit_ctbl + (v_num_proj_financ_max + 1). 
else assign v_des_carac_lim      = v_des_carac_lim + v_cod_carac_lim 
            v_des_cabec_planilha = v_des_cabec_planilha + "Proj" + v_cod_carac_lim.
&ENDIF

find dwb_rpt_param where recid(dwb_rpt_param) = v_rec_dwb_rpt_param no-lock no-error.

find plano_cta_ctbl where plano_cta_ctbl.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl no-lock no-error.
if (avail plano_cta_ctbl) then 
    assign v_ind_tip_plano_cta_ctbl = plano_cta_ctbl.ind_tip_plano_cta_ctbl
           v_cod_tip_grp_cta_ctbl   = plano_cta_ctbl.cod_tip_grp_cta_ctbl.

    /* --- Verifica as datas do per°odo anterior, se impresso ---*/
    find period_ctbl no-lock
         where period_ctbl.cod_cenar_ctbl = v_cod_cenar_ctbl
           and period_ctbl.cod_exerc_ctbl = v_cod_exerc_ctbl
           and period_ctbl.num_period_ctbl = v_num_period_ctbl /*cl_cenar_exerc_period_ctbl of period_ctbl*/ no-error.
    if  avail period_ctbl
    then do:
        assign v_dat_inic_period_ctbl = period_ctbl.dat_inic_period_ctbl
               v_dat_fim_period_ctbl  = period_ctbl.dat_fim_period_ctbl.

        assign v_dat_inic_period      = v_dat_inic_period_ctbl
               v_dat_fim_period       = v_dat_fim_period_ctbl.

        /*if v_dat_inic_period = &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 01/01/0001 &ENDIF or substring(string(v_dat_inic_period),1,1) = "?" then do:
            assign v_dat_inic_period = emsuni.period_ctbl.dat_inic_period_ctbl 
                   v_dat_fim_period  = emsuni.period_ctbl.dat_fim_period_ctbl.
        end.*/        

        if  v_log_period_ctbl_ant_impr = yes and substring(string(v_dat_inic_period_ctbl),1,1) <> "?"
        then do: 
            find last b_period_ctbl no-lock where
                      b_period_ctbl.cod_cenar_ctbl = v_cod_cenar_ctbl and 
                      b_period_ctbl.dat_fim_period_ctbl < v_dat_inic_period_ctbl no-error.
            if avail(b_period_ctbl) then
                assign v_dat_inic_period_ctbl_ant = b_period_ctbl.dat_inic_period_ctbl
                       v_dat_fim_period_ctbl_ant  = b_period_ctbl.dat_fim_period_ctbl
                       v_dat_inic_period          = b_period_ctbl.dat_inic_period_ctbl
                       v_num_period_ctbl_aux      = b_period_ctbl.num_period_ctbl
                       v_cod_exerc_ctbl_aux       = b_period_ctbl.cod_exerc_ctbl.
        end /* if */.
        else assign v_dat_fim_period_ctbl_ant = ?.
    end /* if */.

/* --- Sa°da p/Planilha ---*/

assign v_log_gerac_planilha = yes
       v_cod_carac_lim      = ";".

if  v_log_gerac_planilha = yes
then do:
    
    IF v_cod_dwb_output = "Arquivo" THEN
    DO:
        OUTPUT STREAM s-csv TO VALUE(v_cod_arq_planilha).
        PUT STREAM s-csv
            "Estabelec;Un Negocio;Mes;Conta;CCUSTO;Descr Conta;Descr CCUSTO;Saldo Inicial;Debito;Credito;Saldo Final" SKIP.
    END.
    ELSE
    DO:
        CREATE "Excel.Application" chExcelApplication.
        chworkbook  = chexcelapplication:workbooks:add.
        chworksheet = chexcelapplication:sheets:item(1).
        chExcelApplication:Visible = FALSE.

        assign i-linha = 1.

        /*output stream s_planilha to value(lc(v_cod_arq_planilha)) convert target 'iso8859-1'.
        if (line-counter(s_planilha) + 2) > v_rpt_s_1_bottom then 
            page stream s_planilha.*/

        /*put stream s_planilha unformatted  
            'UO: ' at 1 
            v_cod_unid_organ at 5 format 'x(3)' 
            '-' at 9 
            v_nom_enterprise at 11 format 'x(40)' skip (1).*/

        /*chWorkSheet:Range("A" + STRING(i-linha)):numberformat = "@".
        chWorkSheet:Range("A" + STRING(i-linha)):VALUE = "UO: " + 
                                                         TRIM(v_cod_unid_organ) +
                                                         "-" +
                                                         TRIM(v_nom_enterprise).
        assign i-linha = i-linha + 2.*/

        chWorkSheet:Range("A" + STRING(i-linha)):numberformat = "@".
        chWorkSheet:Range("A" + STRING(i-linha)):VALUE = "Estabelec".

        chWorkSheet:Range("B" + STRING(i-linha)):numberformat = "@".
        chWorkSheet:Range("B" + STRING(i-linha)):VALUE = "Un Negocio".

        chWorkSheet:Range("C" + STRING(i-linha)):numberformat = "@".
        chWorkSheet:Range("C" + STRING(i-linha)):VALUE = "Mes".

        chWorkSheet:Range("D" + STRING(i-linha)):numberformat = "@".
        chWorkSheet:Range("D" + STRING(i-linha)):VALUE = "Conta".

        chWorkSheet:Range("E" + STRING(i-linha)):numberformat = "@".
        chWorkSheet:Range("E" + STRING(i-linha)):VALUE = "CCUSTO".

        chWorkSheet:Range("F" + STRING(i-linha)):numberformat = "@".
        chWorkSheet:Range("F" + STRING(i-linha)):VALUE = "Descr Conta".

        chWorkSheet:Range("G" + STRING(i-linha)):numberformat = "@".
        chWorkSheet:Range("G" + STRING(i-linha)):VALUE = "Descr CCUSTO".

        chWorkSheet:Range("H" + STRING(i-linha)):numberformat = "@".
        chWorkSheet:Range("H" + STRING(i-linha)):VALUE = "Saldo Inicial".

        chWorkSheet:Range("I" + STRING(i-linha)):numberformat = "@".
        chWorkSheet:Range("I" + STRING(i-linha)):VALUE = "Debito".

        chWorkSheet:Range("J" + STRING(i-linha)):numberformat = "@".
        chWorkSheet:Range("J" + STRING(i-linha)):VALUE = "Credito".

        chWorkSheet:Range("K" + STRING(i-linha)):numberformat = "@".
        chWorkSheet:Range("K" + STRING(i-linha)):VALUE = "Saldo Final".

        assign i-linha = i-linha + 1.

    END.



end.

/* --- Encontrar cotacao entre Indic Base de Indic Apresentaá∆o ---*/
if  v_cod_finalid_econ_bas <> v_cod_finalid_econ_apr
then do:
   run pi_retornar_indic_econ_finalid (Input v_cod_finalid_econ_bas,
                                       Input v_dat_cotac_indic_econ,
                                       output v_cod_indic_econ_base) /*pi_retornar_indic_econ_finalid*/.
   run pi_retornar_indic_econ_finalid (Input v_cod_finalid_econ_apr,
                                       Input v_dat_cotac_indic_econ,
                                       output v_cod_indic_econ_apres) /*pi_retornar_indic_econ_finalid*/.
   run pi_achar_cotac_indic_econ (Input v_cod_indic_econ_base,
                                  Input v_cod_indic_econ_apres,
                                  Input v_dat_cotac_indic_econ,
                                  Input "Real" /*l_real*/,
                                  output v_dat_cotac_indic_econ,
                                  output v_val_cotac_indic_econ,
                                  output v_cod_return) /*pi_achar_cotac_indic_econ*/.
end /* if */.
else assign v_val_cotac_indic_econ = 1.

/* --- Verificar Idioma de Apresentaá∆o ---*/
find last param_geral_ems no-lock no-error. 
if avail param_geral_ems then
    if (param_geral_ems.cod_idiom_princ <> v_cod_idioma_apr) then
        assign v_log_idioma = no.  /* --- N∆o Ç o mesmo do param_geral_ems, valor inicial = sim ---*/
find last param_geral_gld no-lock no-error.
if  avail param_geral_gld and
    param_geral_gld.ind_trad_tit_ctbl = "N∆o Utiliza" /*l_nao_utiliza*/  then
    assign v_log_idioma = yes.

/* --- Eliminar tt_grp_cta_ctbl, tt_ccusto e tt_estab_unid_negoc_select ---*/
/* --- Verifica se ha faixa para Ctas Ctbls ---*/
/* --- Atualiz Val M†x de Ocorràncias (%) ---*/
run pi_init_rpt_sdo_cta_ctbl_balanct /*pi_init_rpt_sdo_cta_ctbl_balanct*/.

run pi_percent_update (Input v_val_maximum,
                       Input 0,
                       Input "Executando" /*l_executando*/) /*pi_percent_update*/.

/* --- Extraá∆o de Saldos Cta Ctbl ou Saldos Cta Ctbl ems5.ccusto ---*/
run pi_sdo_cta_ctbl_balanct_impr /*pi_sdo_cta_ctbl_balanct_impr*/.
/*put stream s_1 skip(1).*/

if  v_cod_dwb_program = 'rel_diario_ctbl':U then do:
    for each dwb_rpt_select
        where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
          and dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
          and dwb_rpt_select.cod_dwb_field   = "Projeto" /*l_projeto*/ 
          and dwb_rpt_select.log_dwb_rule    = yes exclusive-lock:
        delete dwb_rpt_select.
    end.
end.
/* --- Imprimir Totais DB e CR ---*/
if  v_log_period_ctbl_ant_impr = no
then do:
    If v_nom_prog = "Di†rio" /*l_diario*/  then do:
        run pi_verifica_pagina_impr_termos_diario /*pi_verifica_pagina_impr_termos_diario*/.
        /*if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
            page stream s_1.
        put stream s_1 unformatted 
            "Total de DÇbitos" at 40
            v_val_tot_sdo_ctbl_db to 95 format "->>,>>>,>>>,>>9.99" skip
            "Total de CrÇditos" at 39
            v_val_tot_sdo_ctbl_cr to 112 format "->>,>>>,>>>,>>9.99" skip.*/
    end.    
    /*else do:
        if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
            page stream s_1.
        put stream s_1 unformatted 
            "Total de DÇbitos" at 40
            v_val_tot_sdo_ctbl_db to 119 format "->>,>>>,>>>,>>9.99" skip
            "Total de CrÇditos" at 39
            v_val_tot_sdo_ctbl_cr to 137 format "->>,>>>,>>>,>>9.99" skip.
    end.*/

    /*put stream s_1 skip(1).*/
    /*if v_log_gerac_planilha = yes then do:
        put stream s_planilha unformatted
            skip(1)
            "Total de DÇbitos"   at 1
            v_cod_carac_lim  at 18
            v_cod_carac_lim  at 19.

            if not v_log_estab_sum then 
                put stream s_planilha unformatted 
                    v_cod_carac_lim.

            if not v_log_unid_negoc_sum then 
                put stream s_planilha unformatted
                    v_cod_carac_lim.

            if not v_log_ccusto_sum then 
                put stream s_planilha unformatted 
                    v_cod_carac_lim.

            &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                if not v_log_proj_financ = yes then
                    put stream s_planilha unformatted 
                        v_cod_carac_lim.
            &endif

            put stream s_planilha unformatted         
                v_val_tot_sdo_ctbl_db to 148 format '->>,>>>,>>>,>>9.99'
                v_cod_carac_lim at 150 skip(1)
                "Total de CrÇditos"  at 1
                v_cod_carac_lim at 18
                v_cod_carac_lim at 19
                v_cod_carac_lim at 20.

           if not v_log_estab_sum then 
               put stream s_planilha unformatted 
                   v_cod_carac_lim.

           if not v_log_unid_negoc_sum then 
               put stream s_planilha unformatted
                   v_cod_carac_lim.

           if not v_log_ccusto_sum then 
               put stream s_planilha unformatted 
                   v_cod_carac_lim.


            &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                if not v_log_proj_financ = yes then
                    put stream s_planilha unformatted 
                        v_cod_carac_lim.
            &endif

            put stream s_planilha unformatted         
               v_val_tot_sdo_ctbl_cr to 150 format '->>,>>>,>>>,>>9.99'
               v_cod_carac_lim at 152.
    end.*/               
end .

/*if v_log_gerac_planilha = yes then do:
    put stream s_planilha unformatted
        skip(1).
end.*/

/* --- Imprimir Totais por Grupo de Contas ---*/
grp_cta_ctbl_block:
for
    each grp_cta_ctbl no-lock
      by grp_cta_ctbl.cod_inic_cta_ctbl:

    find first tt_grp_cta_ctbl exclusive-lock
        where tt_grp_cta_ctbl.tta_cod_grp_cta_ctbl = grp_cta_ctbl.cod_grp_cta_ctbl no-error.
    if  avail tt_grp_cta_ctbl
    then do:
        if  v_log_period_ctbl_ant_impr = no
        then do:
            /* --- Retornar Indicador ---*/
            run pi_retornar_indic_cr_sdo (input-output tt_grp_cta_ctbl.tta_val_sdo_ctbl_inic,
                                          output v_ind_cr_sdo_inic) /*pi_retornar_indic_cr_sdo*/.
            run pi_retornar_indic_cr_sdo (input-output tt_grp_cta_ctbl.tta_val_sdo_ctbl_fim,
                                          output v_ind_cr_sdo_fim) /*pi_retornar_indic_cr_sdo*/.
            If  v_nom_prog = "Di†rio" /*l_diario*/  then do:
                run pi_verifica_pagina_impr_termos_diario /*pi_verifica_pagina_impr_termos_diario*/.
                /*if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    tt_grp_cta_ctbl.tta_cod_grp_cta_ctbl at 10 format "x(8)"
                    "-" at 59
                    tt_grp_cta_ctbl.tta_val_sdo_ctbl_inic to 77 format "->,>>>,>>>,>>9.99"
                    v_ind_cr_sdo_inic at 78 format "X(01)"
                    tt_grp_cta_ctbl.tta_val_sdo_ctbl_fim to 131 format "->,>>>,>>>,>>9.99"
                    v_ind_cr_sdo_fim at 132 format "X(01)" skip.*/
            end.
            /*Else do:
                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    tt_grp_cta_ctbl.tta_cod_grp_cta_ctbl at 10 format "x(8)"
                    "-" at 59
                    tt_grp_cta_ctbl.tta_val_sdo_ctbl_inic to 99 format "->,>>>,>>>,>>9.99"
                    v_ind_cr_sdo_inic at 100 format "X(01)"
                    tt_grp_cta_ctbl.tta_val_sdo_ctbl_fim to 156 format "->,>>>,>>>,>>9.99"
                    v_ind_cr_sdo_fim at 157 format "X(01)" skip.
            end.*/   

            /*if (v_log_gerac_planilha = yes) then do:

                put stream s_planilha unformatted
                    tt_grp_cta_ctbl.tta_cod_grp_cta_ctbl
                    v_cod_carac_lim.

                if not v_log_estab_sum then 
                    put stream s_planilha unformatted 
                        v_cod_carac_lim.

                if not v_log_unid_negoc_sum then 
                    put stream s_planilha unformatted
                        v_cod_carac_lim.

                if not v_log_ccusto_sum then 
                    put stream s_planilha unformatted 
                        v_cod_carac_lim.

                &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                    if not v_log_proj_financ = yes then
                        put stream s_planilha unformatted 
                            v_cod_carac_lim.
                &endif

                put stream s_planilha unformatted         
                    tt_grp_cta_ctbl.tta_val_sdo_ctbl_inic to 102 format '->>>>>,>>>,>>9.99'
                    v_cod_carac_lim at 104
                    v_cod_carac_lim at 105
                    v_cod_carac_lim at 106
                    tt_grp_cta_ctbl.tta_val_sdo_ctbl_fim  to 173 format '->>,>>>,>>>,>>9.99' 
                    v_cod_carac_lim at 175 skip.
            end.*/                   
        end /* if */.
        else do:
            /* --- Retornar Indicador ---*/
            run pi_retornar_indic_cr_sdo (input-output tt_grp_cta_ctbl.tta_val_sdo_ctbl_inic,
                                          output v_ind_cr_sdo_inic) /*pi_retornar_indic_cr_sdo*/.
            run pi_retornar_indic_cr_sdo (input-output tt_grp_cta_ctbl.tta_val_sdo_ctbl_fim,
                                          output v_ind_cr_sdo_fim) /*pi_retornar_indic_cr_sdo*/.
            run pi_retornar_indic_cr_sdo (input-output tt_grp_cta_ctbl.ttv_val_sdo_ctbl_inic_ant,
                                          output v_ind_cr_sdo_inic_ant) /*pi_retornar_indic_cr_sdo*/.
            run pi_retornar_indic_cr_sdo (input-output tt_grp_cta_ctbl.ttv_val_sdo_ctbl_fim_ant,
                                          output v_ind_cr_sdo_fim_ant) /*pi_retornar_indic_cr_sdo*/.

            If  v_nom_prog = "Di†rio" /*l_diario*/  THEN do:
                run pi_verifica_pagina_impr_termos_diario /*pi_verifica_pagina_impr_termos_diario*/.
                /*if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    tt_grp_cta_ctbl.tta_cod_grp_cta_ctbl at 10 format "x(8)"
                    "-" at 59
                    tt_grp_cta_ctbl.ttv_val_sdo_ctbl_inic_ant to 77 format "->,>>>,>>>,>>9.99"
                    v_ind_cr_sdo_inic_ant at 79 format "X(01)"
                    tt_grp_cta_ctbl.ttv_val_sdo_ctbl_fim_ant to 95 format "->>>,>>>,>>9.99"
                    v_ind_cr_sdo_fim_ant at 97 format "X(01)"
                    tt_grp_cta_ctbl.tta_val_sdo_ctbl_inic to 113 format "->>>,>>>,>>9.99"
                    v_ind_cr_sdo_inic at 114 format "X(01)"
                    tt_grp_cta_ctbl.tta_val_sdo_ctbl_fim to 131 format "->,>>>,>>>,>>9.99"
                    v_ind_cr_sdo_fim at 133 format "X(01)" skip.*/
            end.    
            /*Else do:
                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    tt_grp_cta_ctbl.tta_cod_grp_cta_ctbl at 10 format "x(8)"
                    "-" at 78
                    tt_grp_cta_ctbl.ttv_val_sdo_ctbl_inic_ant to 99 format "->,>>>,>>>,>>9.99"
                    v_ind_cr_sdo_inic_ant at 100 format "X(01)"
                    tt_grp_cta_ctbl.ttv_val_sdo_ctbl_fim_ant to 119 format "->>>,>>>,>>9.99"
                    v_ind_cr_sdo_fim_ant at 120 format "X(01)"
                    tt_grp_cta_ctbl.tta_val_sdo_ctbl_inic to 137 format "->>>,>>>,>>9.99"
                    v_ind_cr_sdo_inic at 138 format "X(01)"
                    tt_grp_cta_ctbl.tta_val_sdo_ctbl_fim to 156 format "->,>>>,>>>,>>9.99"
                    v_ind_cr_sdo_fim at 157 format "X(01)" skip.
            end.*/

            /*if v_log_gerac_planilha = yes then do:
                put stream s_planilha unformatted 
                    tt_grp_cta_ctbl.tta_cod_grp_cta_ctbl at 1
                    v_cod_carac_lim.

                if not v_log_estab_sum then 
                    put stream s_planilha unformatted 
                        v_cod_carac_lim.

                if not v_log_unid_negoc_sum then 
                    put stream s_planilha unformatted
                        v_cod_carac_lim.

                if not v_log_ccusto_sum then 
                    put stream s_planilha unformatted 
                        v_cod_carac_lim.

                &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                    if not v_log_proj_financ = yes then
                        put stream s_planilha unformatted 
                            v_cod_carac_lim.
                &endif               

                put stream s_planilha unformatted         
                    tt_grp_cta_ctbl.ttv_val_sdo_ctbl_inic_ant to 101 format '->>,>>>,>>>,>>9.99'
                    v_cod_carac_lim at 103
                    tt_grp_cta_ctbl.ttv_val_sdo_ctbl_fim_ant to 124 format '->>,>>>,>>>,>>9.99'
                    v_cod_carac_lim at 126
                    tt_grp_cta_ctbl.tta_val_sdo_ctbl_inic to 147 format '->>>>>,>>>,>>9.99'
                    v_cod_carac_lim at 149
                    tt_grp_cta_ctbl.tta_val_sdo_ctbl_fim to 170 format '->>,>>>,>>>,>>9.99'
                    v_cod_carac_lim at 172 skip.
            end.*/                    
        end /* else */.

        run pi_acumula_valores_lucros_perdas.
        delete tt_grp_cta_ctbl.
    end /* if */.
end /* for grp_cta_ctbl_block */.

if (v_log_gerac_planilha = yes) then 
do:
        /*output stream s_planilha close.*/
    
        ASSIGN v_cod_arq_excel = v_cod_arq_planilha. /* + "/fgl900.csv"
               v_cod_arq_excel = REPLACE(v_cod_arq_excel,"\","/")
               v_cod_arq_excel = REPLACE(v_cod_arq_excel,"//","/"). */

        IF v_cod_dwb_output = "Terminal" THEN
        DO:
            chExcelApplication:VISIBLE = TRUE.

            /* release com-handles */
            RELEASE OBJECT chExcelApplication.      
            RELEASE OBJECT chWorkbook.
            RELEASE OBJECT chWorksheet.

        END.
        ELSE IF v_cod_dwb_output = "Arquivo" THEN
        DO:
          
            OUTPUT STREAM s-csv CLOSE.
            /*
            chExcelApplication:Workbooks:Item(1):SaveAs(v_cod_arq_excel,,,,,,).
    
            chExcelApplication:QUIT().
            */
        END.
        ELSE IF v_cod_dwb_output = "Impressora" THEN
        DO:
            chExcelApplication:worksheets:ITEM(1):SELECT. /* selecionar a(s) planilha(s) a ser(em) impressa(s) */
            chExcelApplication:VISIBLE = FALSE.
            chExcelApplication:ActiveWindow:SelectedSheets:Printout.
            chExcelApplication:APPLICATION:DISPLAYALERTS = FALSE.
    
            chExcelApplication:QUIT().

            /* release com-handles */
            RELEASE OBJECT chExcelApplication.      
            RELEASE OBJECT chWorkbook.
            RELEASE OBJECT chWorksheet.

        END.
    

END.


run pi_imprime_valores_lucro_perda.

/*/* --- Fecha Arquivo Planilha ---*/
if (v_log_gerac_planilha = yes) then 
do:
    /*output stream s_planilha close.*/

    ASSIGN v_cod_arq_excel = v_cod_arq_planilha + "/fgl900.csv".

    IF v_cod_dwb_output = "Terminal" THEN
    DO:
        chExcelApplication:VISIBLE = TRUE.
    END.
    ELSE IF v_cod_dwb_output = "Arquivo" THEN
    DO:
        chExcelApplication:Workbooks:Item(1):SaveAs(v_cod_arq_excel,,,,,,).

        chExcelApplication:QUIT().
    END.
    ELSE IF v_cod_dwb_output = "Impressora" THEN
    DO:
        chExcelApplication:worksheets:ITEM(1):SELECT. /* selecionar a(s) planilha(s) a ser(em) impressa(s) */
        chExcelApplication:VISIBLE = FALSE.
        chExcelApplication:ActiveWindow:SelectedSheets:Printout.
        chExcelApplication:APPLICATION:DISPLAYALERTS = FALSE.

        chExcelApplication:QUIT().
    END.

    /* release com-handles */
    RELEASE OBJECT chExcelApplication.      
    RELEASE OBJECT chWorkbook.
    RELEASE OBJECT chWorksheet.

end.*/
assign v_num_tempo_exec = etime / 1000
       v_cod_tempo_exec = string(v_num_tempo_exec,'HH:MM:SS')
       v_cod_cta_ctbl_pfixa = replace (v_cod_cta_ctbl_pfixa, '.' , '#')
       v_cod_cta_ctbl_excec = replace (v_cod_cta_ctbl_excec, '.' , '#')
       v_cod_ccusto_pfixa   = replace (v_cod_ccusto_pfixa, '.' , '#')
       v_cod_ccusto_excec   = replace (v_cod_ccusto_excec, '.' , '#').

if v_log_cenar_fisc        = yes and
    v_log_control_pag_livro = yes and
    v_nom_prog              = "Di†rio" /*l_diario*/  then
    run pi_atualizar_livro_diario_balancete /* pi_atualizar_livro_diario_balancete*/.

assign v_num_pagina = v_num_ult_pag.
assign v_num_ult_pag = 0.
return.


/******************************* Main Code End ******************************/

/************************* Internal Procedure Begin *************************/

/*****************************************************************************
** Procedure Interna.....: pi_retornar_indic_econ_finalid
** Descricao.............: pi_retornar_indic_econ_finalid
** Criado por............: vladimir
** Criado em.............: // 
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
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
** Procedure Interna.....: pi_achar_cotac_indic_econ
** Descricao.............: pi_achar_cotac_indic_econ
** Criado por............: vladimir
** Criado em.............: // 
** Alterado por..........: Augusto Guimar∆es    
** Alterado em...........: 15/03/2011
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

    def var v_cod_indic_econ_orig            as character       no-undo. /*local*/
    def var v_dat_cotac_mes                  as date            no-undo. /*local*/
    def var v_val_cotac_indic_econ_base      as decimal         no-undo. /*local*/
    def var v_val_cotac_indic_econ_idx       as decimal         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    if  p_cod_indic_econ_base = p_cod_indic_econ_idx
    then do:

        /* **
         Quando a Base e o ⁄ndice forem iguais, significa que a cotaá∆o pode ser percentual,
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
            /* period_block: */
            case parid_indic_econ.ind_periodic_cotac:
                when "Di†ria" /*l_diaria*/ then
                    diaria_block:
                    do:
                        find cotac_parid no-lock
                             where cotac_parid.cod_indic_econ_base = p_cod_indic_econ_base
                               and cotac_parid.cod_indic_econ_idx = p_cod_indic_econ_idx
                               and cotac_parid.dat_cotac_indic_econ = p_dat_transacao
                               and cotac_parid.ind_tip_cotac_parid = p_ind_tip_cotac_parid
                             use-index ctcprd_id no-error.
                        if  not avail cotac_parid or cotac_parid.val_cotac_indic_econ = 0
                        then do:
                            find parid_indic_econ no-lock
                                 where parid_indic_econ.cod_indic_econ_base = p_cod_indic_econ_base
                                   and parid_indic_econ.cod_indic_econ_idx = p_cod_indic_econ_idx
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
                             where cotac_parid.cod_indic_econ_base = p_cod_indic_econ_base
                               and cotac_parid.cod_indic_econ_idx = p_cod_indic_econ_idx
                               and cotac_parid.dat_cotac_indic_econ = v_dat_cotac_mes
                               and cotac_parid.ind_tip_cotac_parid = p_ind_tip_cotac_parid
                             use-index ctcprd_id no-error.
                        if  not avail cotac_parid or cotac_parid.val_cotac_indic_econ = 0
                        then do:
                            /* block: */
                            case parid_indic_econ.ind_criter_busca:
                                when "Anterior" /*l_anterior*/ then
                                find prev cotac_parid no-lock
                                                   where cotac_parid.cod_indic_econ_base = p_cod_indic_econ_base
                                                     and cotac_parid.cod_indic_econ_idx = p_cod_indic_econ_idx
                                                     and cotac_parid.dat_cotac_indic_econ < v_dat_cotac_mes
                                                     and cotac_parid.ind_tip_cotac_parid = p_ind_tip_cotac_parid
                                                     and cotac_parid.val_cotac_indic_econ <> 0.0
                                                   use-index ctcprd_id no-error.
                                when "Pr¢ximo" /*l_proximo*/ then
                                find next cotac_parid no-lock
                                                   where cotac_parid.cod_indic_econ_base = p_cod_indic_econ_base
                                                     and cotac_parid.cod_indic_econ_idx = p_cod_indic_econ_idx
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

                /* Begin_Include: i_achar_cotac_indic_econ */
                /* period_block: */
                case parid_indic_econ.ind_periodic_cotac:
                    when "Di†ria" /*l_diaria*/ then
                        diaria_block:
                        do:
                            find cotac_parid no-lock
                                 where cotac_parid.cod_indic_econ_base = v_cod_indic_econ_orig
                                   and cotac_parid.cod_indic_econ_idx = p_cod_indic_econ_base
                                   and cotac_parid.dat_cotac_indic_econ = p_dat_transacao
                                   and cotac_parid.ind_tip_cotac_parid = p_ind_tip_cotac_parid
                                 use-index ctcprd_id no-error.
                            if  not avail cotac_parid or cotac_parid.val_cotac_indic_econ = 0
                            then do:
                                find parid_indic_econ no-lock
                                     where parid_indic_econ.cod_indic_econ_base = v_cod_indic_econ_orig
                                       and parid_indic_econ.cod_indic_econ_idx = p_cod_indic_econ_base
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
                                 where cotac_parid.cod_indic_econ_base = v_cod_indic_econ_orig
                                   and cotac_parid.cod_indic_econ_idx = p_cod_indic_econ_base
                                   and cotac_parid.dat_cotac_indic_econ = v_dat_cotac_mes
                                   and cotac_parid.ind_tip_cotac_parid = p_ind_tip_cotac_parid
                                 use-index ctcprd_id no-error.
                            if  not avail cotac_parid or cotac_parid.val_cotac_indic_econ = 0
                            then do:
                                /* block: */
                                case parid_indic_econ.ind_criter_busca:
                                    when "Anterior" /*l_anterior*/ then
                                    find prev cotac_parid no-lock
                                                       where cotac_parid.cod_indic_econ_base = v_cod_indic_econ_orig
                                                         and cotac_parid.cod_indic_econ_idx = p_cod_indic_econ_base
                                                         and cotac_parid.dat_cotac_indic_econ < v_dat_cotac_mes
                                                         and cotac_parid.ind_tip_cotac_parid = p_ind_tip_cotac_parid
                                                         and cotac_parid.val_cotac_indic_econ <> 0.0
                                                       use-index ctcprd_id no-error.
                                    when "Pr¢ximo" /*l_proximo*/ then
                                    find next cotac_parid no-lock
                                                       where cotac_parid.cod_indic_econ_base = v_cod_indic_econ_orig
                                                         and cotac_parid.cod_indic_econ_idx = p_cod_indic_econ_base
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

                /* End_Include: i_achar_cotac_indic_econ */

                if  avail cotac_parid and cotac_parid.val_cotac_indic_econ <> 0
                then do:
                    assign v_val_cotac_indic_econ_base = cotac_parid.val_cotac_indic_econ.
                    find parid_indic_econ no-lock
                        where parid_indic_econ.cod_indic_econ_base = v_cod_indic_econ_orig
                        and parid_indic_econ.cod_indic_econ_idx = p_cod_indic_econ_idx
                        use-index prdndccn_id no-error.

                    /* Begin_Include: i_achar_cotac_indic_econ */
                    /* period_block: */
                    case parid_indic_econ.ind_periodic_cotac:
                        when "Di†ria" /*l_diaria*/ then
                            diaria_block:
                            do:
                                find cotac_parid no-lock
                                     where cotac_parid.cod_indic_econ_base = v_cod_indic_econ_orig
                                       and cotac_parid.cod_indic_econ_idx = p_cod_indic_econ_idx
                                       and cotac_parid.dat_cotac_indic_econ = p_dat_transacao
                                       and cotac_parid.ind_tip_cotac_parid = p_ind_tip_cotac_parid
                                     use-index ctcprd_id no-error.
                                if  not avail cotac_parid or cotac_parid.val_cotac_indic_econ = 0
                                then do:
                                    find parid_indic_econ no-lock
                                         where parid_indic_econ.cod_indic_econ_base = v_cod_indic_econ_orig
                                           and parid_indic_econ.cod_indic_econ_idx = p_cod_indic_econ_idx
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
                                     where cotac_parid.cod_indic_econ_base = v_cod_indic_econ_orig
                                       and cotac_parid.cod_indic_econ_idx = p_cod_indic_econ_idx
                                       and cotac_parid.dat_cotac_indic_econ = v_dat_cotac_mes
                                       and cotac_parid.ind_tip_cotac_parid = p_ind_tip_cotac_parid
                                     use-index ctcprd_id no-error.
                                if  not avail cotac_parid or cotac_parid.val_cotac_indic_econ = 0
                                then do:
                                    /* block: */
                                    case parid_indic_econ.ind_criter_busca:
                                        when "Anterior" /*l_anterior*/ then
                                        find prev cotac_parid no-lock
                                                           where cotac_parid.cod_indic_econ_base = v_cod_indic_econ_orig
                                                             and cotac_parid.cod_indic_econ_idx = p_cod_indic_econ_idx
                                                             and cotac_parid.dat_cotac_indic_econ < v_dat_cotac_mes
                                                             and cotac_parid.ind_tip_cotac_parid = p_ind_tip_cotac_parid
                                                             and cotac_parid.val_cotac_indic_econ <> 0.0
                                                           use-index ctcprd_id no-error.
                                        when "Pr¢ximo" /*l_proximo*/ then
                                        find next cotac_parid no-lock
                                                           where cotac_parid.cod_indic_econ_base = v_cod_indic_econ_orig
                                                             and cotac_parid.cod_indic_econ_idx = p_cod_indic_econ_idx
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

                    /* End_Include: i_achar_cotac_indic_econ */

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

                /* Begin_Include: i_achar_cotac_indic_econ */
                /* period_block: */
                case parid_indic_econ.ind_periodic_cotac:
                    when "Di†ria" /*l_diaria*/ then
                        diaria_block:
                        do:
                            find cotac_parid no-lock
                                 where cotac_parid.cod_indic_econ_base = p_cod_indic_econ_idx
                                   and cotac_parid.cod_indic_econ_idx = p_cod_indic_econ_base
                                   and cotac_parid.dat_cotac_indic_econ = p_dat_transacao
                                   and cotac_parid.ind_tip_cotac_parid = p_ind_tip_cotac_parid
                                 use-index ctcprd_id no-error.
                            if  not avail cotac_parid or cotac_parid.val_cotac_indic_econ = 0
                            then do:
                                find parid_indic_econ no-lock
                                     where parid_indic_econ.cod_indic_econ_base = p_cod_indic_econ_idx
                                       and parid_indic_econ.cod_indic_econ_idx = p_cod_indic_econ_base
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
                                 where cotac_parid.cod_indic_econ_base = p_cod_indic_econ_idx
                                   and cotac_parid.cod_indic_econ_idx = p_cod_indic_econ_base
                                   and cotac_parid.dat_cotac_indic_econ = v_dat_cotac_mes
                                   and cotac_parid.ind_tip_cotac_parid = p_ind_tip_cotac_parid
                                 use-index ctcprd_id no-error.
                            if  not avail cotac_parid or cotac_parid.val_cotac_indic_econ = 0
                            then do:
                                /* block: */
                                case parid_indic_econ.ind_criter_busca:
                                    when "Anterior" /*l_anterior*/ then
                                    find prev cotac_parid no-lock
                                                       where cotac_parid.cod_indic_econ_base = p_cod_indic_econ_idx
                                                         and cotac_parid.cod_indic_econ_idx = p_cod_indic_econ_base
                                                         and cotac_parid.dat_cotac_indic_econ < v_dat_cotac_mes
                                                         and cotac_parid.ind_tip_cotac_parid = p_ind_tip_cotac_parid
                                                         and cotac_parid.val_cotac_indic_econ <> 0.0
                                                       use-index ctcprd_id no-error.
                                    when "Pr¢ximo" /*l_proximo*/ then
                                    find next cotac_parid no-lock
                                                       where cotac_parid.cod_indic_econ_base = p_cod_indic_econ_idx
                                                         and cotac_parid.cod_indic_econ_idx = p_cod_indic_econ_base
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

                /* End_Include: i_achar_cotac_indic_econ */

                if  avail cotac_parid and cotac_parid.val_cotac_indic_econ <> 0
                then do:
                    assign p_dat_cotac_indic_econ = cotac_parid.dat_cotac_indic_econ
                           p_val_cotac_indic_econ = 1 / cotac_parid.val_cotac_indic_econ
                           p_cod_return = "OK" /*l_ok*/ .
                    return.
                end /* if */.
            end /* if */.
        end /* if */.

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
    end /* else */.
END PROCEDURE. /* pi_achar_cotac_indic_econ */
/*****************************************************************************
** Procedure Interna.....: pi_init_rpt_sdo_cta_ctbl_balanct
** Descricao.............: pi_init_rpt_sdo_cta_ctbl_balanct
** Criado por............: Uno
** Criado em.............: 22/05/2000 18:18:56
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_init_rpt_sdo_cta_ctbl_balanct:

    /************************* Variable Definition Begin ************************/

    def var v_num_final                      as integer         no-undo. /*local*/
    def var v_num_inicial                    as integer         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    /* --- Eliminar tt_grp_cta_ctbl, tt_ccusto e tt_estab_unid_negoc_select ---*/
    grp_cta_ctbl_block:
    for each tt_grp_cta_ctbl exclusive-lock:
       delete tt_grp_cta_ctbl.
    end /* for grp_cta_ctbl_block */.

    ccusto_block:
    for each tt_ccusto exclusive-lock:
        delete tt_ccusto.
    end /* for ems5.ccusto_block */.

    estab_unid_negoc_block:
    for each tt_estab_unid_negoc_select no-lock:
        delete tt_estab_unid_negoc_select.
    end /* for estab_unid_negoc_block */.

    for each tt_cta_ctbl_505:
        delete tt_cta_ctbl_505.
    end.
    for each tt_estrut_ctbl_505:
        delete tt_estrut_ctbl_505.
    end.

    &if '{&emsbas_version}' >= '5.05' &then
        &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
            for each tt_proj_financ_select:
                delete tt_proj_financ_select.
            end.
        &ENDIF
        for each tt_cta_ctbl_aux:
            delete tt_cta_ctbl_aux.
        end.
        for each tt_cta_ctbl_analit:
            delete tt_cta_ctbl_analit.
        end.
    &endif
    
    /* --- Caso Plano Cta Ctbl <> Consolidaá∆o ---*/
    if  v_ind_tip_plano_cta_ctbl <> "Consolidaá∆o" /*l_consolidacao*/ 
    then do:
        /* ---  Criar tt_estab_unid_negoc_select ---*/

        run pi_criar_estab_unid_negoc_select /*pi_criar_estab_unid_negoc_select*/.

        &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
            /* --- Criar tt_proj_financ_select ---*/
            run pi_cria_tt_proj_financ_select /*pi_cria_tt_proj_financ_select*/.
        &ENDIF
        
        /* --- Verifica se existe faixa para ems5.ccusto ---*/
        find first b_dwb_rpt_select no-lock
             where b_dwb_rpt_select.cod_dwb_program = V_COD_DWB_PROGRAM
               and b_dwb_rpt_select.cod_dwb_user = v_cod_dwb_user
               and b_dwb_rpt_select.cod_dwb_field   = "Centro Custo"
               and b_dwb_rpt_select.log_dwb_rule = yes /*cl_b_dwb_rpt_select of b_dwb_rpt_select*/ no-error.
        if (avail b_dwb_rpt_select) then
            assign v_log_sdo_ccusto = yes.
        else
            assign v_log_sdo_ccusto = no.

        /* --- Criar tt_ccusto ---*/
        if  v_log_sdo_ccusto = yes or v_log_ccusto_sum = no
        then do:
           
            run pi_criar_ccusto_select /*pi_criar_ccusto_select*/.

            /* --- Verifica PFixa e PExec do ems5.ccusto ---*/
            ccusto_block:
            for each tt_ccusto exclusive-lock:
                 if ((not tt_ccusto.cod_ccusto matches v_cod_ccusto_pfixa)
                 or (v_cod_ccusto_excec <> fill(chr(46), length(tt_ccusto.cod_ccusto))
                 and tt_ccusto.cod_ccusto matches v_cod_ccusto_excec)) then
                    delete tt_ccusto.
            end /* for ems5.ccusto_block */.
        end /* if */.
    end /* if */.
    else do:
        /* --- Caso ÿ exista Regra com Unid Negoc ---*/

        /* Begin_Include: i_cria_dwb_rpt_select */
        find first b_dwb_rpt_select
           where b_dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
           and   b_dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
           and   b_dwb_rpt_select.cod_dwb_field   = "Unid Neg¢cio"
           and   b_dwb_rpt_select.log_dwb_rule    = yes no-lock no-error.
        if  not avail b_dwb_rpt_select
        then do:
            criar_dwb_rpt_select:
            do transaction:
                find last dwb_rpt_select no-lock
                     where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
                       and dwb_rpt_select.cod_dwb_user = v_cod_dwb_user
                       and dwb_rpt_select.log_dwb_rule = yes /*cl_dwb_rpt_select_rule of dwb_rpt_select*/ no-error.
                if (avail dwb_rpt_select) then 
                     assign v_num_dwb_order =  dwb_rpt_select.num_dwb_order + 10.
                else assign v_num_dwb_order = 10.

                create b_dwb_rpt_select.
                assign b_dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
                       b_dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
                       b_dwb_rpt_select.cod_dwb_field   = "Unid Neg¢cio"
                       b_dwb_rpt_select.num_dwb_order   = v_num_dwb_order
                       b_dwb_rpt_select.log_dwb_rule    = yes.

                find first unid_negoc no-lock no-error.
                if (avail unid_negoc) then
                    assign b_dwb_rpt_select.cod_dwb_initial = unid_negoc.cod_unid_negoc.
                find last unid_negoc no-lock no-error.
                if (avail unid_negoc) then
                    assign b_dwb_rpt_select.cod_dwb_final = unid_negoc.cod_unid_negoc.
            end /* do criar_dwb_rpt_select */.
        end /* if */.
        /* End_Include: i_cria_dwb_rpt_select */

        assign v_cod_estab       = "" /*l_null*/ 
               v_cod_ccusto      = "" /*l_null*/ 
               v_cod_proj_financ = "" /*l_null*/ 
               v_log_estab_sum   = yes
               v_log_proj_financ = yes
               v_log_ccusto_sum  = yes.
    end /* else */.

    /* --- Verifica se ha faixa para Ctas Ctbls ---*/
    find first b_dwb_rpt_select no-lock
         where b_dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
           and b_dwb_rpt_select.cod_dwb_user = v_cod_dwb_user
           and (b_dwb_rpt_select.cod_dwb_field   = "Conta Cont†bil"
            or b_dwb_rpt_select.cod_dwb_field   = "Alternativa Conta")
           and b_dwb_rpt_select.log_dwb_rule = yes /*cl_b_dwb_rpt_select_2 of b_dwb_rpt_select*/ no-error.
    if  not avail b_dwb_rpt_select
    then do:
        assign v_log_exec_sdo_cta_ctbl_balanct = yes.
        criar_dwb_rpt_select:
        do transaction:
            find last dwb_rpt_select no-lock
               where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program no-error.

            create b_dwb_rpt_select.
            if (avail dwb_rpt_select) then
                 assign b_dwb_rpt_select.num_dwb_order =  dwb_rpt_select.num_dwb_order + 10.
            else assign b_dwb_rpt_select.num_dwb_order = 10.
            assign b_dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
                   b_dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
                   b_dwb_rpt_select.cod_dwb_field   = "Conta Cont†bil"
                   b_dwb_rpt_select.log_dwb_rule    = yes.

            find first cta_ctbl
                where cta_ctbl.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl no-lock no-error.
            if (avail cta_ctbl) then
                assign b_dwb_rpt_select.cod_dwb_initial = cta_ctbl.cod_cta_ctbl.
            find last cta_ctbl
                where cta_ctbl.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl no-lock no-error.
            if (avail cta_ctbl) then
                assign b_dwb_rpt_select.cod_dwb_final = cta_ctbl.cod_cta_ctbl.
        end /* do criar_dwb_rpt_select */.
    end /* if */.
    else
        assign v_log_exec_sdo_cta_ctbl_balanct = no.

    &if '{&emsbas_version}' >= '5.05' &then
    for each dwb_rpt_select no-lock
       where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program 
       and   dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user 
       and   dwb_rpt_select.log_dwb_rule    = yes :
       case dwb_rpt_select.cod_dwb_field:
         when "Estabelecimento" /*l_estabelecimento*/ then
             assign v_cod_estab_ini = dwb_rpt_select.cod_dwb_initial
                    v_cod_estab_fim = dwb_rpt_select.cod_dwb_final.
         &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
         when "Projeto" /*l_projeto*/ then
             assign v_cod_proj_financ_ini = dwb_rpt_select.cod_dwb_initial
                    v_cod_proj_financ_fim = dwb_rpt_select.cod_dwb_final.
         &ENDIF
         when "Unid Neg¢cio" /*l_Unid_Neg¢cio*/ then
             assign v_cod_unid_negoc_ini = dwb_rpt_select.cod_dwb_initial
                    v_cod_unid_negoc_fim = dwb_rpt_select.cod_dwb_final.
       end. 
    end.
    &endif

    /* --- Atualiz Val M†x de Ocorràncias (%) ---*/
    selecao_block:
    for each dwb_rpt_select exclusive-lock
        where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
        and   dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
        and  (dwb_rpt_select.cod_dwb_field   = "Conta Cont†bil" 
        or    dwb_rpt_select.cod_dwb_field   = "Alternativa Conta")
        and   dwb_rpt_select.log_dwb_rule    = yes:

        if  dwb_rpt_select.cod_dwb_field = "Conta Cont†bil"
        then do:
            if  PROGRAM-NAME(3) begins "pi_rpt_lancto_ctbl_diario_contabil" /*l_pi_rpt_lancto_ctbl_diario_ctbl*/ 
            then do:
                assign dwb_rpt_select.cod_dwb_initial = replace(dwb_rpt_select.cod_dwb_initial,'-','')
                       dwb_rpt_select.cod_dwb_initial = replace(dwb_rpt_select.cod_dwb_initial,'.','')
                       dwb_rpt_select.cod_dwb_final   = replace(dwb_rpt_select.cod_dwb_final,'-','')
                       dwb_rpt_select.cod_dwb_final   = replace(dwb_rpt_select.cod_dwb_final,'.','').
            end /* if */.
            else do:
                 if  entry(1,dwb_rpt_param.cod_dwb_order) <> "Estrutura" /*l_estrutura*/ 
                 then do:
                     assign v_num_final = 0.
                     do v_num_inicial = 1 to length(plano_cta_ctbl.cod_format_cta_ctbl):
                        if substring(plano_cta_ctbl.cod_format_cta_ctbl, v_num_inicial, 1) = '.'
                        or substring(plano_cta_ctbl.cod_format_cta_ctbl, v_num_inicial, 1) = '-'
                        then do:
                             substring(dwb_rpt_select.cod_dwb_initial, v_num_inicial - v_num_final, 1, "character" /*l_character*/ ) = ''.
                             substring(dwb_rpt_select.cod_dwb_final  , v_num_inicial - v_num_final, 1, "character" /*l_character*/ ) = ''.
                             assign v_num_final = v_num_final + 1.
                        end.
                     end.
                 end /* if */.
            end.
        end /* if */.

        if  entry(1,dwb_rpt_param.cod_dwb_order) <> "Estrutura" /*l_estrutura*/ 
        then do:
            cta_ctbl_block:
            for each cta_ctbl no-lock
                where cta_ctbl.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl:

                if  dwb_rpt_select.cod_dwb_field = "Conta Cont†bil"
                then do:
                    if cta_ctbl.cod_cta_ctbl < dwb_rpt_select.cod_dwb_initial or
                       cta_ctbl.cod_cta_ctbl > dwb_rpt_select.cod_dwb_final then
                       next cta_ctbl_block.
                end /* if */.
                else do:
                    if cta_ctbl.cod_altern_cta_ctbl < dwb_rpt_select.cod_dwb_initial or
                       cta_ctbl.cod_altern_cta_ctbl > dwb_rpt_select.cod_dwb_final then
                       next cta_ctbl_block.
                end /* else */.

                /* ** Verifica PFixa e PExec da Conta ***/
                if (not cta_ctbl.cod_cta_ctbl matches v_cod_cta_ctbl_pfixa)
                or (v_cod_cta_ctbl_excec <> fill(chr(46), length(cta_ctbl.cod_cta_ctbl))
                and cta_ctbl.cod_cta_ctbl matches v_cod_cta_ctbl_excec)
                then
                    /* ** Verifica que programa chama esta piÏ FO 300076 - bre16721 ***/
                    if v_nom_prog <> "Di†rio" /*l_diario*/  then
                       next cta_ctbl_block.

                /* ** Carrega temp-table com as contas v†lidas ***/
                find tt_cta_ctbl_505
                   where tt_cta_ctbl_505.ttv_rec_cta_ctbl = recid(cta_ctbl)
                   no-error.
                if not avail tt_cta_ctbl_505 then do:

                    create tt_cta_ctbl_505.
                    assign tt_cta_ctbl_505.ttv_rec_cta_ctbl = recid(cta_ctbl)
                           v_val_maximum = v_val_maximum + 1.
                end.
            end /* for cta_ctbl_block */.
        end /* if */.
        else do:
            estrut_cta_ctbl_block:
            for each estrut_cta_ctbl no-lock
                where estrut_cta_ctbl.cod_plano_cta_ctbl  = v_cod_plano_cta_ctbl
                and estrut_cta_ctbl.cod_cta_ctbl_pai    = '':

                if  dwb_rpt_select.cod_dwb_field = "Conta Cont†bil" and
                    estrut_cta_ctbl.cod_cta_ctbl_filho >= dwb_rpt_select.cod_dwb_initial and
                    estrut_cta_ctbl.cod_cta_ctbl_filho <= dwb_rpt_select.cod_dwb_final
                then do:
                    find cta_ctbl
                      where cta_ctbl.cod_plano_cta_ctbl = estrut_cta_ctbl.cod_plano_cta_ctbl
                      and   cta_ctbl.cod_cta_ctbl       = estrut_cta_ctbl.cod_cta_ctbl_filho
                      no-lock no-error.
                    if not avail cta_ctbl then
                       next.

                    find tt_cta_ctbl_505
                       where tt_cta_ctbl_505.ttv_rec_cta_ctbl = recid(cta_ctbl)
                       no-error.
                    if not avail tt_cta_ctbl_505 then do:
                        create tt_cta_ctbl_505.
                        assign tt_cta_ctbl_505.ttv_rec_cta_ctbl = recid(cta_ctbl).
                    end.

                    create tt_estrut_ctbl_505.
                    assign v_val_maximum = v_val_maximum + 1
                           tt_estrut_ctbl_505.ttv_rec_estrut_ctbl = recid(estrut_cta_ctbl).
                    run pi_estrut_cta_ctbl_balanct (Input estrut_cta_ctbl.cod_cta_ctbl_filho,
                                                    Input yes) /*pi_estrut_cta_ctbl_balanct*/.
                end /* if */.
                else do: /* Alternativa Conta */
                    if  can-find(first cta_ctbl where
                        cta_ctbl.cod_plano_cta_ctbl   = estrut_cta_ctbl.cod_plano_cta_ctbl and
                        cta_ctbl.cod_cta_ctbl         = estrut_cta_ctbl.cod_cta_ctbl_filho and
                        cta_ctbl.cod_altern_cta_ctbl >= dwb_rpt_select.cod_dwb_initial and
                        cta_ctbl.cod_altern_cta_ctbl <= dwb_rpt_select.cod_dwb_final)
                    then do:
                        find cta_ctbl
                          where cta_ctbl.cod_plano_cta_ctbl = estrut_cta_ctbl.cod_plano_cta_ctbl
                          and   cta_ctbl.cod_cta_ctbl       = estrut_cta_ctbl.cod_cta_ctbl_filho
                          no-lock no-error.
                        if not avail cta_ctbl then
                           next.

                        find tt_cta_ctbl_505
                           where tt_cta_ctbl_505.ttv_rec_cta_ctbl = recid(cta_ctbl)
                           no-error.
                        if not avail tt_cta_ctbl_505 then do:
                            create tt_cta_ctbl_505.
                            assign tt_cta_ctbl_505.ttv_rec_cta_ctbl = recid(cta_ctbl).
                        end.

                        create tt_estrut_ctbl_505.
                        assign v_val_maximum = v_val_maximum + 1
                               tt_estrut_ctbl_505.ttv_rec_estrut_ctbl = recid(estrut_cta_ctbl).
                        run pi_estrut_cta_ctbl_balanct (Input estrut_cta_ctbl.cod_cta_ctbl_filho,
                                                        Input yes) /*pi_estrut_cta_ctbl_balanct*/.
                    end /* if */.
                end /* else */.
            end /* for estrut_cta_ctbl_block */.
        end /* else */.
    end /* for selecao_block */.

   /* --- Exerc°cio e Per°odo Cont†bil ---*/
    if  v_cod_exerc_ctbl  <> '' and
        v_num_period_ctbl <> 0
    then do:
        find exerc_ctbl no-lock
           where exerc_ctbl.cod_cenar_ctbl = v_cod_cenar_ctbl
             and exerc_ctbl.cod_exerc_ctbl = v_cod_exerc_ctbl no-error.
        find period_ctbl no-lock
           where period_ctbl.cod_cenar_ctbl = v_cod_cenar_ctbl
             and period_ctbl.cod_exerc_ctbl = v_cod_exerc_ctbl
             and period_ctbl.num_period_ctbl = v_num_period_ctbl no-error.
    end.
    else do:
        /* **   Busca o exerc°cio e per°odo  ***/
        find first exerc_ctbl no-lock
           where exerc_ctbl.cod_cenar_ctbl  = v_cod_cenar_ctbl
             and exerc_ctbl.cod_exerc_ctbl >= string(year(today)) no-error.
        if  not avail exerc_ctbl or
            not can-find(first period_ctbl
                where period_ctbl.cod_cenar_ctbl = exerc_ctbl.cod_cenar_ctbl
                  and period_ctbl.cod_exerc_ctbl = exerc_ctbl.cod_exerc_ctbl
                )
                then do:
            find first period_ctbl no-lock
                 where period_ctbl.cod_cenar_ctbl = cenar_ctbl.cod_cenar_ctbl no-error.
            find exerc_ctbl no-lock
                 where exerc_ctbl.cod_cenar_ctbl = period_ctbl.cod_cenar_ctbl
                   and exerc_ctbl.cod_exerc_ctbl = period_ctbl.cod_exerc_ctbl
                  no-error.
        end.
        /* Per°odo Cont†bil Inicial */
        if  avail exerc_ctbl
        then do:
            find last period_ctbl no-lock
                 where period_ctbl.cod_cenar_ctbl = exerc_ctbl.cod_cenar_ctbl
                   and period_ctbl.cod_exerc_ctbl = exerc_ctbl.cod_exerc_ctbl
                   and period_ctbl.dat_fim_period_ctbl < date(month(today),day(today),int(exerc_ctbl.cod_exerc_ctbl)) no-error.
        end.
    end.

    if  avail period_ctbl
    then 
        assign v_dat_fim_period_ctbl = period_ctbl.dat_fim_period_ctbl.
    else 
        assign v_dat_fim_period_ctbl = today.
END PROCEDURE. /* pi_init_rpt_sdo_cta_ctbl_balanct */
/*****************************************************************************
** Procedure Interna.....: pi_sdo_cta_ctbl_balanct_impr
** Descricao.............: pi_sdo_cta_ctbl_balanct_impr
** Criado por............: Henke
** Criado em.............: 13/11/1996 15:55:24
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_sdo_cta_ctbl_balanct_impr:

    assign v_ind_tip_sdo = ""
           v_num_seq_sdo = 0.

    &if '{&emsbas_version}' >= '5.05' &then
    /* Elimina todos os registros da temp-table*/
    EMPTY TEMP-TABLE tt_input_sdo.
    EMPTY TEMP-TABLE tt_input_leitura_sdo_demonst.
    &endif

    assign v_ind_tip_sdo = if v_ind_tip_plano_cta_ctbl <> "Consolidaá∆o" /*l_consolidacao*/  then "Cont†bil" /*l_contabil*/  
                           else "Consolidaá∆o" /*l_consolidacao*/ .

    run pi_retorna_saldos_contas_analiticas /*pi_retorna_saldos_contas_analiticas*/.
    
    run pi_retorna_saldos_contas_analiticas_more /*pi_retorna_saldos_contas_analiticas_more*/.
    /* --- Classificado por Conta Ctbl ---*/
    if  entry(1,dwb_rpt_param.cod_dwb_order) <> "Estrutura" /*l_estrutura*/  
    then do:
        cta_ctbl_block:
        for each tt_cta_ctbl_505:

            find first cta_ctbl no-lock
                 where recid(cta_ctbl) = tt_cta_ctbl_505.ttv_rec_cta_ctbl no-error.
            if  v_log_cta_ctbl_analit  = yes and      /* --- S¢ Anal°ticas ---*/
                cta_ctbl.ind_espec_cta_ctbl = "SintÇtica" /*l_sintetica*/ 
            then do:
                assign v_val_current_value = v_val_current_value + 1.
                run pi_percent_update (Input v_val_maximum,
                                       Input v_val_current_value,
                                       Input "") /*pi_percent_update*/.
                next cta_ctbl_block.
            end /* if */.
            /* --- Montagem do T°tulo ---*/
            assign v_des_tit_ctbl_balanct = "".
            run pi_des_tit_cta_ctbl_balanct /*pi_des_tit_cta_ctbl_balanct*/.
            /* --- Pesquisa registros de Saldos Cont†beis ---*/
            if  v_ind_tip_plano_cta_ctbl <> "Consolidaá∆o" /*l_consolidacao*/ 
            then do:
               /* --- Tipo de Saldo ---*/

               if  v_log_sdo_ccusto = yes or v_log_ccusto_sum = no
               then do:
                   &if '{&emsbas_version}' >= '5.05' &then
                       run pi_ler_sdo_cta_ctbl_ccusto_balanct_1 /*pi_ler_sdo_cta_ctbl_ccusto_balanct_1*/.
                   &else
                       run pi_ler_sdo_cta_ctbl_ccusto_balanct /*pi_ler_sdo_cta_ctbl_ccusto_balanct*/.
                   &endif
               end /* if */.
               else
                   run pi_ler_sdo_cta_ctbl_balanct_1 /* pi_ler_sdo_cta_ctbl_balanct_1*/.
            end /* if */.
            else do: /* --- Sdo Cta Ctbl Consolid ---*/
                &if '{&emsbas_version}' >= '5.05' &then
                    run pi_ler_sdo_cta_ctbl_balanct_1 /*pi_ler_sdo_cta_ctbl_balanct_1*/.
                &else
                    run pi_ler_sdo_cta_consolid_balanct /*pi_ler_sdo_cta_consolid_balanct*/.
               &endif
            end /* else */.
            /* --- Imprimir Saldos da Conta ---*/
            run pi_imprimir_sdo_cta_ctbl_balanct /*pi_imprimir_sdo_cta_ctbl_balanct*/.
            assign v_val_current_value = v_val_current_value + 1.
            run pi_percent_update (Input v_val_maximum,
                                   Input v_val_current_value,
                                   Input "") /*pi_percent_update*/.
        end /* for cta_ctbl_block */.
    end.
    else do:  /* --- Classificado por Estrutura Conta Ctbl ---*/
       selecao_block:
       for
          each dwb_rpt_select exclusive-lock
             where  dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
               and  dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
               and (dwb_rpt_select.cod_dwb_field   = "Conta Cont†bil"
               or   dwb_rpt_select.cod_dwb_field   = "Alternativa Conta")
               and  dwb_rpt_select.log_dwb_rule    = yes:

          estrut_cta_ctbl_block:
          for each tt_estrut_ctbl_505:
             find estrut_cta_ctbl no-lock
                  where recid(estrut_cta_ctbl) = tt_estrut_ctbl_505.ttv_rec_estrut_ctbl no-error.

             /* ** VALIDA¯∞O FAIXAS DE SELE¯∞O ***/
             if  dwb_rpt_select.cod_dwb_field = "Conta Cont†bil" /*l_conta_contabil*/ 
             then do:
                if not(estrut_cta_ctbl.cod_cta_ctbl_filho >= dwb_rpt_select.cod_dwb_initial and
                       estrut_cta_ctbl.cod_cta_ctbl_filho <= dwb_rpt_select.cod_dwb_final) then
                   next estrut_cta_ctbl_block.
             end /* if */.
             else do:
                if not(can-find(first cta_ctbl where
                       cta_ctbl.cod_plano_Cta_ctbl   = estrut_cta_ctbl.cod_plano_cta_ctbl and
                       cta_ctbl.cod_cta_ctbl         = estrut_cta_ctbl.cod_cta_ctbl_filho and
                       cta_ctbl.cod_altern_cta_ctbl >= dwb_rpt_select.cod_dwb_initial and
                       cta_ctbl.cod_altern_cta_ctbl <= dwb_rpt_select.cod_dwb_final)) then
                    next estrut_cta_ctbl_block.
             end /* else */.

             find cta_ctbl
                 where cta_ctbl.cod_plano_cta_ctbl = estrut_cta_ctbl.cod_plano_cta_ctbl
                   and cta_ctbl.cod_cta_ctbl       = estrut_cta_ctbl.cod_cta_ctbl_filho
                    no-lock no-error.
             /* --- Montagem do T°tulo ---*/
             assign v_des_tit_ctbl_balanct = "".
             run pi_des_tit_cta_ctbl_balanct /*pi_des_tit_cta_ctbl_balanct*/.
             /* --- Pesquisa registros de Saldos Cont†beis ---*/
             if  v_ind_tip_plano_cta_ctbl <> "Consolidaá∆o" /*l_consolidacao*/ 
             then do:
                 /* --- Tipo de Saldo ---*/
                 if  v_log_sdo_ccusto = yes or v_log_ccusto_sum = no
                 then do:
                     &if '{&emsbas_version}' >= '5.05' &then
                         run pi_ler_sdo_cta_ctbl_ccusto_balanct_1 /*pi_ler_sdo_cta_ctbl_ccusto_balanct_1*/.
                     &else
                         run pi_ler_sdo_cta_ctbl_ccusto_balanct /*pi_ler_sdo_cta_ctbl_ccusto_balanct*/.
                     &endif
                 end /* if */.
                 else
                     run pi_ler_sdo_cta_ctbl_balanct_1 /*pi_ler_sdo_cta_ctbl_balanct_1*/.
             end /* if */.
             else do: /* --- Sdo Cta Ctbl Consolid ---*/
                 &if '{&emsbas_version}' >= '5.05' &then
                     run pi_ler_sdo_cta_ctbl_balanct_1 /*pi_ler_sdo_cta_ctbl_balanct_1*/.
                 &else         
                     run pi_ler_sdo_cta_consolid_balanct /*pi_ler_sdo_cta_consolid_balanct*/.
                 &endif
             end /* else */.
             /* --- Imprimir Saldos da Conta ---*/
             run pi_imprimir_sdo_cta_ctbl_balanct /*pi_imprimir_sdo_cta_ctbl_balanct*/.
             assign v_val_current_value = v_val_current_value + 1.
             run pi_percent_update (Input v_val_maximum,
                                    Input v_val_current_value,
                                    Input "") /*pi_percent_update*/.
             /* --- Outros n°veis da estrutura ---*/
             run pi_estrut_cta_ctbl_balanct (Input estrut_cta_ctbl.cod_cta_ctbl_filho,
                                             Input no) /*pi_estrut_cta_ctbl_balanct*/.
          end /* for estrut_cta_ctbl_block */.
       end /* for selecao_block */.
    end /* else */.

    assign v_log_method = session:set-wait-state('').
END PROCEDURE. /* pi_sdo_cta_ctbl_balanct_impr */
/*****************************************************************************
** Procedure Interna.....: pi_criar_estab_unid_negoc_select
** Descricao.............: pi_criar_estab_unid_negoc_select
** Criado por............: Henke
** Criado em.............: 24/07/1996 08:20:36
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_criar_estab_unid_negoc_select:

    /************************* Variable Definition Begin ************************/

    def var v_log_possui_permis              as logical         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    /* --- Caso n∆o exista Regra contendo Estabelecimento ---*/
    find first b_dwb_rpt_select exclusive-lock
       where b_dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
       and   b_dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
       and   b_dwb_rpt_select.cod_dwb_field   = "Estabelecimento"
       and   b_dwb_rpt_select.log_dwb_rule    = yes no-error.
    if  not avail b_dwb_rpt_select
    then do:
       find last dwb_rpt_select no-lock
           where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
             and dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
             and dwb_rpt_select.log_dwb_rule    = yes
           no-error.
        if avail dwb_rpt_select then
            assign v_num_dwb_order =  dwb_rpt_select.num_dwb_order + 10.
        else 
            assign v_num_dwb_order = 10.
        create b_dwb_rpt_select.
        assign b_dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
               b_dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
               b_dwb_rpt_select.cod_dwb_field   = "Estabelecimento"
               b_dwb_rpt_select.num_dwb_order   = v_num_dwb_order
               b_dwb_rpt_select.log_dwb_rule    = yes.
        find first estabelecimento no-lock
           where estabelecimento.cod_empresa = v_cod_unid_organ
           use-index stblcmnt_id no-error.
        if  avail estabelecimento
        then do:
            if b_dwb_rpt_select.cod_dwb_initial = "" then 
               assign b_dwb_rpt_select.cod_dwb_initial = estabelecimento.cod_estab.
            else 
               if estabelecimento.cod_estab < b_dwb_rpt_select.cod_dwb_initial then 
                  assign b_dwb_rpt_select.cod_dwb_initial = estabelecimento.cod_estab.
        end /* if */.
        find last estabelecimento no-lock
           where estabelecimento.cod_empresa = v_cod_unid_organ
           use-index stblcmnt_id no-error.
        if  avail estabelecimento
        then do:
            if b_dwb_rpt_select.cod_dwb_final = "" then 
               assign b_dwb_rpt_select.cod_dwb_final = estabelecimento.cod_estab.
            else 
               if estabelecimento.cod_estab > b_dwb_rpt_select.cod_dwb_final then 
                  assign b_dwb_rpt_select.cod_dwb_final = estabelecimento.cod_estab.
        end /* if */.
    end /* if */.

    /* --- Caso n∆o exista Regra contendo Unidades de Neg¢cio ---*/
    find first b_dwb_rpt_select
       where b_dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
       and   b_dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
       and   b_dwb_rpt_select.cod_dwb_field   = "Unid Neg¢cio"
       and   b_dwb_rpt_select.log_dwb_rule    = yes no-lock no-error.
    if  not avail b_dwb_rpt_select
    then do:
        find last dwb_rpt_select no-lock
             where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
               and dwb_rpt_select.cod_dwb_user = v_cod_dwb_user
               and dwb_rpt_select.log_dwb_rule = yes /*cl_dwb_rpt_select_rule of dwb_rpt_select*/ no-error.
        if avail dwb_rpt_select then
            assign v_num_dwb_order =  dwb_rpt_select.num_dwb_order + 10.
        else
            assign v_num_dwb_order = 10.
        create b_dwb_rpt_select.
        assign b_dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
               b_dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
               b_dwb_rpt_select.cod_dwb_field   = "Unid Neg¢cio"
               b_dwb_rpt_select.num_dwb_order   = v_num_dwb_order
               b_dwb_rpt_select.log_dwb_rule    = yes.
        find first unid_negoc no-lock no-error.
        if avail unid_negoc then 
            assign b_dwb_rpt_select.cod_dwb_initial = unid_negoc.cod_unid_negoc.
        find last unid_negoc no-lock no-error.
        if avail unid_negoc then
            assign b_dwb_rpt_select.cod_dwb_final = unid_negoc.cod_unid_negoc.
    end /* if */.

    /* --- Criaá∆o tt_estab_unid_negoc_select ---*/
    for each b_dwb_rpt_select
        where b_dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
          and b_dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
          and b_dwb_rpt_select.cod_dwb_field   = "Estabelecimento"
          and b_dwb_rpt_select.log_dwb_rule    = yes no-lock:

        estab_block:
        for each estabelecimento no-lock
            where estabelecimento.cod_empresa = v_cod_unid_organ
              and estabelecimento.cod_estab  >= b_dwb_rpt_select.cod_dwb_initial
              and estabelecimento.cod_estab  <= b_dwb_rpt_select.cod_dwb_final:

            /* ** SE RESTRINGE ACESSO A ESTABELECIMENTOS DO USU∆RIO EMISSOR DO RELATπRIO,
                 ESTE N∞O CONSEGUIR∆ ENXERGAR VALORES DE OUTROS ESTABELECIMENTOS ***/
            if  v_log_restric_estab = yes then do:

                if  can-do(v_des_lista_estab, estabelecimento.cod_estab) then
                    next estab_block.

                assign v_log_possui_permis = no.
                if  can-find(first segur_unid_organ no-lock
                    where segur_unid_organ.cod_unid_organ = estabelecimento.cod_estab
                    and   segur_unid_organ.cod_grp_usuar  = "*") then do:
                    assign v_log_possui_permis = yes.
                end.
                else do:
                    loop_block:
                    for each usuar_grp_usuar no-lock
                        where usuar_grp_usuar.cod_usuario = v_cod_usuar_corren:
                        if  can-find(first segur_unid_organ no-lock
                            where segur_unid_organ.cod_unid_organ = estabelecimento.cod_estab
                            and   segur_unid_organ.cod_grp_usuar  = usuar_grp_usuar.cod_grp_usuar) then do:
                            assign v_log_possui_permis = yes.
                            leave loop_block.
                        end.
                    end.
                end.

                if  v_log_possui_permis = no then do:
                    /* ** MONTA LISTA DE ESTABELECIMENTOS N∞O-LISTADOS POR FALTA DE ACESSO ***/
                    assign v_des_lista_estab = v_des_lista_estab
                                             + (if v_des_lista_estab <> "" then "," else "")
                                             + estabelecimento.cod_estab.
                    next estab_block.
                end.
            end.

            unid_negoc_block:
            for each dwb_rpt_select
                where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
                  and dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
                  and dwb_rpt_select.cod_dwb_field   = "Unid Neg¢cio"
                  and dwb_rpt_select.log_dwb_rule    = yes no-lock:

                estab_un_block:
                for each estab_unid_negoc no-lock
                    where estab_unid_negoc.cod_estab       = estabelecimento.cod_estab
                      and estab_unid_negoc.cod_unid_negoc >= dwb_rpt_select.cod_dwb_initial
                      and estab_unid_negoc.cod_unid_negoc <= dwb_rpt_select.cod_dwb_final:

                    find tt_estab_unid_negoc_select no-lock
                        where tt_estab_unid_negoc_select.cod_estab      = estab_unid_negoc.cod_estab
                        and   tt_estab_unid_negoc_select.cod_unid_negoc = estab_unid_negoc.cod_unid_negoc no-error.
                    if  not avail tt_estab_unid_negoc_select then do:
                        create tt_estab_unid_negoc_select.
                        assign tt_estab_unid_negoc_select.cod_estab      = estab_unid_negoc.cod_estab
                               tt_estab_unid_negoc_select.cod_unid_negoc = estab_unid_negoc.cod_unid_negoc.
                    end.
                end.
            end.
        end.
    end.
END PROCEDURE. /* pi_criar_estab_unid_negoc_select */
/*****************************************************************************
** Procedure Interna.....: pi_ler_sdo_cta_ctbl_ccusto_balanct
** Descricao.............: pi_ler_sdo_cta_ctbl_ccusto_balanct
** Criado por............: Henke
** Criado em.............: 14/11/1996 09:22:12
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_ler_sdo_cta_ctbl_ccusto_balanct:

    /************************* Variable Definition Begin ************************/

    def var v_val_sdo_ctbl_cr                as decimal         no-undo. /*local*/
    def var v_val_sdo_ctbl_db                as decimal         no-undo. /*local*/
    def var v_val_sdo_ctbl_fim               as decimal         no-undo. /*local*/
    def var v_val_soma_cr                    as decimal         no-undo. /*local*/
    def var v_val_soma_db                    as decimal         no-undo. /*local*/
    def var v_val_soma_fim                   as decimal         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    /* Deletando temp-table para n∆o duplicar,triplicar.. valor DB,CR conta pai*/
    for each tt_sdo_cta_ctbl_balanct exclusive-lock:
        delete tt_sdo_cta_ctbl_balanct.
    end.

    blk_estab_unid:
    for each tt_estab_unid_negoc_select:

        /* Begin_Include: i_verifica_se_sumaria */
        &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
            if not can-find(first tt_proj_financ_select
                where tt_proj_financ_select.tta_cod_proj_financ = tt_retorna_sdo_ctbl_demonst.tta_cod_proj_financ
                and   tt_proj_financ_select.tta_cod_estab       = tt_retorna_sdo_ctbl_demonst.tta_cod_estab) then next blk_estab_unid.
        &ENDIF

        /* --- Sumaria ou n∆o Estabelecimento ---*/
        if v_log_estab_sum = yes then
            assign v_cod_estab = "".
        else
            assign v_cod_estab = tt_estab_unid_negoc_select.cod_estab.

        /* --- Sumaria ou n∆o Unid Negoc ---*/
        if v_log_unid_negoc_sum = yes then
            assign v_cod_unid_negoc = "".
        else
            assign v_cod_unid_negoc = tt_estab_unid_negoc_select.cod_unid_negoc.

        &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
            /* --- Sumaria ou n∆o Projeto ---*/
            if v_log_proj_financ = yes then
                assign v_cod_proj_financ = "".
            else
                assign v_cod_proj_financ = tt_proj_financ_select.tta_cod_proj_financ.
        &ENDIF

        /* End_Include: i_verifica_se_sumaria */


        if can-find(first sdo_cta_ctbl_ccusto
         where sdo_cta_ctbl_ccusto.cod_empresa        = v_cod_unid_organ
         and   sdo_cta_ctbl_ccusto.cod_finalid_econ   = v_cod_finalid_econ_bas
         and   sdo_cta_ctbl_ccusto.cod_cenar_ctbl     = v_cod_cenar_ctbl
         and   sdo_cta_ctbl_ccusto.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
         and   sdo_cta_ctbl_ccusto.cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl
         and   sdo_cta_ctbl_ccusto.cod_estab          = tt_estab_unid_negoc_select.cod_estab
         and   sdo_cta_ctbl_ccusto.cod_unid_negoc     = tt_estab_unid_negoc_select.cod_unid_negoc
         and   sdo_cta_ctbl_ccusto.cod_plano_ccusto   = v_cod_plano_ccusto
         and   sdo_cta_ctbl_ccusto.dat_sdo_ctbl       = v_dat_fim_period_ctbl)
         then do:
            blk_sdo_ccusto:
            for each sdo_cta_ctbl_ccusto
                where sdo_cta_ctbl_ccusto.cod_empresa        = v_cod_unid_organ
                and   sdo_cta_ctbl_ccusto.cod_finalid_econ   = v_cod_finalid_econ_bas
                and   sdo_cta_ctbl_ccusto.cod_cenar_ctbl     = v_cod_cenar_ctbl
                and   sdo_cta_ctbl_ccusto.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
                and   sdo_cta_ctbl_ccusto.cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl
                and   sdo_cta_ctbl_ccusto.cod_estab          = tt_estab_unid_negoc_select.cod_estab
                and   sdo_cta_ctbl_ccusto.cod_unid_negoc     = tt_estab_unid_negoc_select.cod_unid_negoc
                and   sdo_cta_ctbl_ccusto.cod_plano_ccusto   = v_cod_plano_ccusto
                and   sdo_cta_ctbl_ccusto.dat_sdo_ctbl       = v_dat_fim_period_ctbl no-lock.

                find first tt_ccusto
                    where tt_ccusto.cod_ccusto = sdo_cta_ctbl_ccusto.cod_ccusto exclusive-lock no-error.
                if not avail tt_ccusto then next blk_sdo_ccusto.

                if  cta_ctbl.ind_espec_cta_ctbl = "Anal°tica" /*l_analitica*/ 
                then do:
                    if  not can-find(estrut_ctbl_movto_analit
                        where estrut_ctbl_movto_analit.cod_empresa        = v_cod_unid_organ
                        and   estrut_ctbl_movto_analit.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
                        and   estrut_ctbl_movto_analit.cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl
                        and   estrut_ctbl_movto_analit.cod_plano_ccusto   = v_cod_plano_ccusto
                        and   estrut_ctbl_movto_analit.cod_ccusto         = tt_ccusto.cod_ccusto
                        and   estrut_ctbl_movto_analit.cod_estab          = tt_estab_unid_negoc_select.cod_estab
                        and   estrut_ctbl_movto_analit.cod_unid_negoc     = tt_estab_unid_negoc_select.cod_unid_negoc)
                    then next blk_sdo_ccusto.
                end /* if */.
                /* --- Sumaria ou n∆o Unid Negoc ---*/
                if  v_log_ccusto_sum = yes then
                    assign v_cod_ccusto = "".
                else
                    assign v_cod_ccusto = tt_ccusto.cod_ccusto.

                find first tt_sdo_cta_ctbl_balanct
                    where tt_sdo_cta_ctbl_balanct.tta_cod_estab      = v_cod_estab
                    and   tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc = v_cod_unid_negoc
                    and   tt_sdo_cta_ctbl_balanct.tta_cod_ccusto     = v_cod_ccusto
                    exclusive-lock no-error.
                if  not avail tt_sdo_cta_ctbl_balanct
                then do:
                    create tt_sdo_cta_ctbl_balanct.
                    assign tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl   = sdo_cta_ctbl_ccusto.cod_cta_ctbl
                           tt_sdo_cta_ctbl_balanct.tta_cod_ccusto     = v_cod_ccusto
                           tt_sdo_cta_ctbl_balanct.tta_cod_estab      = v_cod_estab
                           tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc = v_cod_unid_negoc
                           tt_sdo_cta_ctbl_balanct.tta_cod_empresa    = sdo_cta_ctbl_ccusto.cod_empresa
                           tt_sdo_cta_ctbl_balanct.tta_cod_plano_cta_ctbl = sdo_cta_ctbl_ccusto.cod_plano_cta_ctbl
                           tt_sdo_cta_ctbl_balanct.tta_cod_plano_ccusto = IF v_cod_ccusto = "" THEN "" ELSE sdo_cta_ctbl_ccusto.cod_plano_ccusto.


                end /* if */.
                assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                                                                   + (sdo_cta_ctbl_ccusto.val_sdo_ctbl_db / v_val_cotac_indic_econ)
                       tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                                                                   + (sdo_cta_ctbl_ccusto.val_sdo_ctbl_cr / v_val_cotac_indic_econ)
                       tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim = tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim
                                                                    + (sdo_cta_ctbl_ccusto.val_sdo_ctbl_fim / v_val_cotac_indic_econ).
                /* --- Caso considere apuraá∆o de resultado ---*/
                if  v_log_consid_apurac_restdo = yes
                or cta_ctbl.cod_cta_ctbl = emsuni.plano_cta_ctbl.cod_cta_ctbl_apurac_restdo
                then do:
                    if  sdo_cta_ctbl_ccusto.val_apurac_restdo_db = 0 and sdo_cta_ctbl_ccusto.val_apurac_restdo_cr = 0
                    then do:
                        if sdo_cta_ctbl_ccusto.val_apurac_restdo > 0 then
                            assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                                                                               + (sdo_cta_ctbl_ccusto.val_apurac_restdo / v_val_cotac_indic_econ).
                        else
                            assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                                                                               + (sdo_cta_ctbl_ccusto.val_apurac_restdo / v_val_cotac_indic_econ * -1).
                    end /* if */.
                    else
                        assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                                                                           + (sdo_cta_ctbl_ccusto.val_apurac_restdo_db / v_val_cotac_indic_econ)
                               tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                                                                           + (sdo_cta_ctbl_ccusto.val_apurac_restdo_cr / v_val_cotac_indic_econ).
                    assign tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim = tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim
                                                                            + (sdo_cta_ctbl_ccusto.val_apurac_restdo_acum / v_val_cotac_indic_econ).
                end /* if */.
                /* --- Totais de DB e CR ---*/
                if  cta_ctbl.ind_espec_cta_ctbl = "Anal°tica" /*l_analitica*/ 
                then do:
                    assign v_val_tot_sdo_ctbl_db = v_val_tot_sdo_ctbl_db
                                                 + (sdo_cta_ctbl_ccusto.val_sdo_ctbl_db / v_val_cotac_indic_econ)
                           v_val_tot_sdo_ctbl_cr = v_val_tot_sdo_ctbl_cr
                                                 + (sdo_cta_ctbl_ccusto.val_sdo_ctbl_cr / v_val_cotac_indic_econ).
                     /* --- Caso considere apuraá∆o de resultado ---*/
                     if  v_log_consid_apurac_restdo = yes
                     or cta_ctbl.cod_cta_ctbl = emsuni.plano_cta_ctbl.cod_cta_ctbl_apurac_restdo
                     then do:
                         if  sdo_cta_ctbl_ccusto.val_apurac_restdo_db = 0 and sdo_cta_ctbl_ccusto.val_apurac_restdo_cr = 0
                         then do:
                             if sdo_cta_ctbl_ccusto.val_apurac_restdo > 0 then
                                 assign v_val_tot_sdo_ctbl_db = v_val_tot_sdo_ctbl_db
                                                              + (sdo_cta_ctbl_ccusto.val_apurac_restdo / v_val_cotac_indic_econ).
                             else
                                 assign v_val_tot_sdo_ctbl_cr = v_val_tot_sdo_ctbl_cr
                                                              + (sdo_cta_ctbl_ccusto.val_apurac_restdo / v_val_cotac_indic_econ * -1).
                         end /* if */.
                         else
                             assign v_val_tot_sdo_ctbl_db = v_val_tot_sdo_ctbl_db
                                                          + (sdo_cta_ctbl_ccusto.val_apurac_restdo_db / v_val_cotac_indic_econ)
                                    v_val_tot_sdo_ctbl_cr = v_val_tot_sdo_ctbl_cr
                                                          + (sdo_cta_ctbl_ccusto.val_apurac_restdo_cr / v_val_cotac_indic_econ).
                     end /* if */.
                end /* if */.
                /* --- Saldo do Per°odo Anterior ---*/
                if  v_dat_fim_period_ctbl_ant <> ?
                then do:
                    find b_sdo_cta_ctbl_ccusto
                        where b_sdo_cta_ctbl_ccusto.cod_empresa        = sdo_cta_ctbl_ccusto.cod_empresa
                        and   b_sdo_cta_ctbl_ccusto.cod_finalid_econ   = sdo_cta_ctbl_ccusto.cod_finalid_econ
                        and   b_sdo_cta_ctbl_ccusto.cod_cenar_ctbl     = sdo_cta_ctbl_ccusto.cod_cenar_ctbl
                        and   b_sdo_cta_ctbl_ccusto.cod_plano_cta_ctbl = sdo_cta_ctbl_ccusto.cod_plano_cta_ctbl
                        and   b_sdo_cta_ctbl_ccusto.cod_cta_ctbl       = sdo_cta_ctbl_ccusto.cod_cta_ctbl
                        and   b_sdo_cta_ctbl_ccusto.cod_estab          = sdo_cta_ctbl_ccusto.cod_estab
                        and   b_sdo_cta_ctbl_ccusto.cod_unid_negoc     = sdo_cta_ctbl_ccusto.cod_unid_negoc
                        and   b_sdo_cta_ctbl_ccusto.cod_plano_ccusto   = sdo_cta_ctbl_ccusto.cod_plano_ccusto
                        and   b_sdo_cta_ctbl_ccusto.cod_ccusto         = sdo_cta_ctbl_ccusto.cod_ccusto
                        and   b_sdo_cta_ctbl_ccusto.dat_sdo_ctbl       = v_dat_fim_period_ctbl_ant
                        no-lock no-error.
                    if  avail b_sdo_cta_ctbl_ccusto
                    then do:
                        assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant
                                                                                + (b_sdo_cta_ctbl_ccusto.val_sdo_ctbl_fim / v_val_cotac_indic_econ)
                               tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant
                                                                                 + ((b_sdo_cta_ctbl_ccusto.val_sdo_ctbl_fim
                                                                                 -  b_sdo_cta_ctbl_ccusto.val_sdo_ctbl_db + b_sdo_cta_ctbl_ccusto.val_sdo_ctbl_cr)
                                                                                 /  v_val_cotac_indic_econ).
                        /* --- Caso considere apuraá∆o de resultado ---*/
                        if v_log_consid_apurac_restdo = yes
                        or cta_ctbl.cod_cta_ctbl = emsuni.plano_cta_ctbl.cod_cta_ctbl_apurac_restdo then
                            assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant
                                                                                    + (sdo_cta_ctbl_ccusto.val_apurac_restdo_acum / v_val_cotac_indic_econ)
                                   /* FUT1082 - 09/07/2002
                                   Faltou somar o valor de apuraá∆o acumulado.*/                                            
                                   tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant
                                                                                     + ((sdo_cta_ctbl_ccusto.val_apurac_restdo_acum - sdo_cta_ctbl_ccusto.val_apurac_restdo) / v_val_cotac_indic_econ).
                    end /* if */.
                end /* if */.
            end.

            /* INICIO: ATIVIDADE: 36.289 - TASK: 5.646 */

            if v_log_mostra_sem_aprop_cc then do:

                find first sdo_cta_ctbl
                    where sdo_cta_ctbl.cod_empresa        = v_cod_unid_organ
                    and   sdo_cta_ctbl.cod_finalid_econ   = v_cod_finalid_econ_bas
                    and   sdo_cta_ctbl.cod_cenar_ctbl     = v_cod_cenar_ctbl
                    and   sdo_cta_ctbl.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
                    and   sdo_cta_ctbl.cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl
                    and   sdo_cta_ctbl.cod_estab          = tt_estab_unid_negoc_select.cod_estab
                    and   sdo_cta_ctbl.cod_unid_negoc     = tt_estab_unid_negoc_select.cod_unid_negoc
                    and   sdo_cta_ctbl.dat_sdo_ctbl       = v_dat_fim_period_ctbl
                    no-lock no-error.
                if  avail sdo_cta_ctbl
                then do:
                    assign v_val_soma_cr     = 0  v_val_soma_db     = 0  v_val_soma_fim     = 0
                           v_val_sdo_ctbl_cr = 0  v_val_sdo_ctbl_db = 0  v_val_sdo_ctbl_fim = 0.

                      for each tt_ccusto:                   
                        for each b_sdo_cta_ctbl_ccusto no-lock
                            where b_sdo_cta_ctbl_ccusto.cod_empresa        = v_cod_unid_organ
                            and   b_sdo_cta_ctbl_ccusto.cod_finalid_econ   = v_cod_finalid_econ_bas
                            and   b_sdo_cta_ctbl_ccusto.cod_cenar_ctbl     = v_cod_cenar_ctbl
                            and   b_sdo_cta_ctbl_ccusto.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
                            and   b_sdo_cta_ctbl_ccusto.cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl
                            and   b_sdo_cta_ctbl_ccusto.cod_plano_ccusto   = v_cod_plano_ccusto
                            and   b_sdo_cta_ctbl_ccusto.cod_ccusto         = tt_ccusto.cod_ccusto
                            and   b_sdo_cta_ctbl_ccusto.cod_estab          = tt_estab_unid_negoc_select.cod_estab
                            and   b_sdo_cta_ctbl_ccusto.cod_unid_negoc     = tt_estab_unid_negoc_select.cod_unid_negoc
                            and   b_sdo_cta_ctbl_ccusto.dat_sdo_ctbl       = v_dat_fim_period_ctbl:

                             assign v_val_soma_fim = v_val_soma_fim + (b_sdo_cta_ctbl_ccusto.val_sdo_ctbl_fim / v_val_cotac_indic_econ)
                                    v_val_soma_db  = v_val_soma_db  + (b_sdo_cta_ctbl_ccusto.val_sdo_ctbl_db / v_val_cotac_indic_econ)
                                    v_val_soma_cr  = v_val_soma_cr  + (b_sdo_cta_ctbl_ccusto.val_sdo_ctbl_cr / v_val_cotac_indic_econ).
                        end.    
                     end.   
                    /* --- Caso considere apuraá∆o de resultado ---*/
                    assign v_val_sdo_ctbl_cr  = v_val_sdo_ctbl_cr  + (sdo_Cta_Ctbl.val_sdo_Ctbl_Cr  / v_val_cotac_indic_econ)
                           v_val_sdo_ctbl_db  = v_val_sdo_ctbl_db  + (sdo_Cta_Ctbl.val_sdo_Ctbl_Db  / v_val_cotac_indic_econ)
                           v_val_sdo_ctbl_fim = v_val_sdo_ctbl_fim + (sdo_Cta_Ctbl.val_sdo_Ctbl_Fim / v_val_cotac_indic_econ).

                    if  v_log_consid_apurac_restdo = yes
                    or cta_ctbl.cod_cta_ctbl = emsuni.plano_cta_ctbl.cod_cta_ctbl_apurac_restdo
                    then do:
                        if  sdo_cta_ctbl.val_apurac_restdo_db = 0 and sdo_cta_ctbl.val_apurac_restdo_cr = 0
                        then do:
                            if sdo_cta_ctbl.val_apurac_restdo > 0 then
                                assign v_val_sdo_ctbl_db = v_val_sdo_ctbl_db + (sdo_cta_ctbl.val_apurac_restdo / v_val_cotac_indic_econ).
                            else
                                assign v_val_sdo_ctbl_cr = v_val_sdo_ctbl_cr + (sdo_cta_ctbl.val_apurac_restdo / v_val_cotac_indic_econ * -1).
                        end /* if */.
                        else
                            assign v_val_sdo_ctbl_db = v_val_sdo_ctbl_db + (sdo_cta_ctbl.val_apurac_restdo_db / v_val_cotac_indic_econ)
                                   v_val_sdo_ctbl_cr = v_val_sdo_ctbl_cr + (sdo_cta_ctbl.val_apurac_restdo_cr / v_val_cotac_indic_econ).

                        assign v_val_sdo_ctbl_fim = v_val_sdo_ctbl_fim + (sdo_cta_ctbl.val_apurac_restdo_acum / v_val_cotac_indic_econ).
                    end /* if */.

                    if v_val_soma_Fim <> v_val_sdo_Ctbl_fim or
                       v_val_soma_db  <> v_val_sdo_Ctbl_db  or
                       v_val_soma_cr  <> v_val_sdo_Ctbl_cr  then do:
                            find first tt_sdo_cta_ctbl_balanct
                                where tt_sdo_cta_ctbl_balanct.tta_cod_estab      = v_cod_estab
                                and   tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc = v_cod_unid_negoc
                                and   tt_sdo_cta_ctbl_balanct.tta_cod_ccusto     = ""
                                exclusive-lock no-error.
                            if  not avail tt_sdo_cta_ctbl_balanct then do:
                                create tt_sdo_cta_ctbl_balanct.
                                assign tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl   = sdo_cta_ctbl.cod_cta_ctbl
                                       tt_sdo_cta_ctbl_balanct.tta_cod_ccusto     = ""
                                       tt_sdo_cta_ctbl_balanct.tta_cod_estab      = v_cod_estab
                                       tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc = v_cod_unid_negoc
                                       tt_sdo_cta_ctbl_balanct.tta_cod_empresa    = sdo_cta_ctbl.cod_empresa
                                       tt_sdo_cta_ctbl_balanct.tta_cod_plano_cta_ctbl = sdo_cta_ctbl.cod_plano_cta_ctbl
                                       tt_sdo_cta_ctbl_balanct.tta_cod_plano_ccusto = "".

                            end.
                            if (v_val_soma_db - v_val_sdo_ctbl_db) > 0 then
                                assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                                                                               + (v_val_soma_db - v_val_sdo_Ctbl_Db).
                            else
                                assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                                                                               + (v_val_sdo_Ctbl_Db - v_val_soma_db).
                            if (v_val_soma_cr - v_val_sdo_ctbl_cr) > 0 then
                                 assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                                                                               + (v_val_soma_cr - v_val_sdo_Ctbl_cr).
                            else
                                 assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                                                                               + (v_val_sdo_Ctbl_cr - v_val_soma_cr).
                            if (v_val_soma_fim - v_val_sdo_ctbl_fim) > 0 then
                                assign tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim = tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim
                                                                                + (v_val_soma_fim - v_val_sdo_ctbl_fim).
                            else
                                assign tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim = tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim
                                                                                + (v_val_sdo_ctbl_fim - v_val_soma_fim).
                            /* --- Totais de DB e CR ---*/
                            if cta_ctbl.ind_espec_cta_ctbl = "Anal°tica" /*l_analitica*/  then
                                assign v_val_tot_sdo_ctbl_db = v_val_tot_sdo_ctbl_db
                                                             + (tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 / v_val_cotac_indic_econ)
                                       v_val_tot_sdo_ctbl_cr = v_val_tot_sdo_ctbl_cr
                                                             + (tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 / v_val_cotac_indic_econ).
                    end.
                end /* if */.
                /* FIM: ATIVIDADE: 36.289 - TASK: 5.646 */
            end.    
        end.
        else do:
            if  v_log_sdo_ccusto = yes then
            next blk_estab_unid.

            if  v_log_cta_ctbl_sdo = yes then  /* --- Conta sem Saldos ---*/
                assign v_cod_ccusto_aux = v_cod_ccusto.

            assign v_cod_ccusto = "".

            if v_log_exec_sdo_cta_ctbl_balanct then
               run pi_ler_sdo_cta_ctbl_balanct /*pi_ler_sdo_cta_ctbl_balanct*/. 
        end.
    end.    
END PROCEDURE. /* pi_ler_sdo_cta_ctbl_ccusto_balanct */
/*****************************************************************************
** Procedure Interna.....: pi_ler_sdo_cta_ctbl_balanct
** Descricao.............: pi_ler_sdo_cta_ctbl_balanct
** Criado por............: Henke
** Criado em.............: 14/11/1996 09:21:53
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_ler_sdo_cta_ctbl_balanct:

    assign v_val_sdo_ctbl_db_505        = 0
           v_val_sdo_ctbl_cr_505        = 0
           v_val_sdo_ctbl_fim_505       = 0
           v_val_apurac_restdo_db_505   = 0
           v_val_apurac_restdo_cr_505   = 0
           v_val_apurac_restdo_505      = 0
           v_val_apurac_restdo_acum_505 = 0.

    &if '{&emsbas_version}' >= '5.05' &then
        for each tt_retorna_sdo_ctbl_demonst
            where tt_retorna_sdo_ctbl_demonst.tta_cod_empresa        = v_cod_unid_organ
            and   tt_retorna_sdo_ctbl_demonst.tta_cod_finalid_econ   = v_cod_finalid_econ_bas
            and   tt_retorna_sdo_ctbl_demonst.tta_cod_cenar_ctbl     = v_cod_cenar_ctbl
            and   tt_retorna_sdo_ctbl_demonst.tta_cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
            and   tt_retorna_sdo_ctbl_demonst.tta_cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl
            and   tt_retorna_sdo_ctbl_demonst.tta_cod_estab          = tt_estab_unid_negoc_select.cod_estab
            and   tt_retorna_sdo_ctbl_demonst.tta_cod_unid_negoc     = tt_estab_unid_negoc_select.cod_unid_negoc
            and   tt_retorna_sdo_ctbl_demonst.tta_dat_sdo_ctbl       = v_dat_fim_period_ctbl
            and   tt_retorna_sdo_ctbl_demonst.tta_cod_plano_ccusto   = ''
            &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
            and   tt_retorna_sdo_ctbl_demonst.tta_cod_proj_financ    = tt_proj_financ_select.tta_cod_proj_financ
            &ENDIF
            :

            assign v_val_sdo_ctbl_db_505      = v_val_sdo_ctbl_db_505  + tt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_db
                   v_val_sdo_ctbl_cr_505      = v_val_sdo_ctbl_cr_505  + tt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_cr
                   v_val_sdo_ctbl_fim_505     = v_val_sdo_ctbl_fim_505 + tt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_fim.
        end.
        /* ** VERIFICA SE CONTAS TEM SALDO ***/
        if v_val_sdo_ctbl_db_505  <> 0
        or v_val_sdo_ctbl_cr_505  <> 0
        or v_val_sdo_ctbl_fim_505 <> 0 then do:
    &else
    find first sdo_cta_ctbl
        where sdo_cta_ctbl.cod_empresa        = v_cod_unid_organ
        and   sdo_cta_ctbl.cod_finalid_econ   = v_cod_finalid_econ_bas
        and   sdo_cta_ctbl.cod_cenar_ctbl     = v_cod_cenar_ctbl
        and   sdo_cta_ctbl.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
        and   sdo_cta_ctbl.cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl
        and   sdo_cta_ctbl.cod_estab          = tt_estab_unid_negoc_select.cod_estab
        and   sdo_cta_ctbl.cod_unid_negoc     = tt_estab_unid_negoc_select.cod_unid_negoc
        and   sdo_cta_ctbl.dat_sdo_ctbl       = v_dat_fim_period_ctbl 
        no-lock no-error.
    if  avail sdo_cta_ctbl
    then do:
        assign v_val_sdo_ctbl_db_505      = sdo_cta_ctbl.val_sdo_ctbl_db
               v_val_sdo_ctbl_cr_505      = sdo_cta_ctbl.val_sdo_ctbl_cr
               v_val_sdo_ctbl_fim_505     = sdo_cta_ctbl.val_sdo_ctbl_fim
               v_val_apurac_restdo_db_505 = sdo_cta_ctbl.val_apurac_restdo_db
               v_val_apurac_restdo_cr_505 = sdo_cta_ctbl.val_apurac_restdo_cr
               v_val_apurac_restdo_505    = sdo_cta_ctbl.val_apurac_restdo
               v_val_apurac_restdo_acum_505 = sdo_cta_ctbl.val_apurac_restdo_acum.
    &endif
        find first tt_sdo_cta_ctbl_balanct
            where tt_sdo_cta_ctbl_balanct.tta_cod_estab      = v_cod_estab
            and   tt_sdo_cta_ctbl_balanct.tta_cod_ccusto     = v_cod_ccusto
            and   tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc = v_cod_unid_negoc
            &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
            and   tt_sdo_cta_ctbl_balanct.tta_cod_proj_financ = v_cod_proj_financ
            &ENDIF
            exclusive-lock no-error.
        if  not avail tt_sdo_cta_ctbl_balanct
        then do:
            create tt_sdo_cta_ctbl_balanct.
            assign tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl   = cta_ctbl.cod_cta_ctbl
                   tt_sdo_cta_ctbl_balanct.tta_cod_ccusto     = ""
                   tt_sdo_cta_ctbl_balanct.tta_cod_estab      = v_cod_estab
                   tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc = v_cod_unid_negoc
                   tt_sdo_cta_ctbl_balanct.tta_cod_empresa    = v_cod_unid_organ
                   tt_sdo_cta_ctbl_balanct.tta_cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
                   tt_sdo_cta_ctbl_balanct.tta_cod_plano_ccusto = "".

            &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                assign tt_sdo_cta_ctbl_balanct.tta_cod_proj_financ = v_cod_proj_financ.
            &ENDIF
        end /* if */.
        assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1  = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                                                            + (v_val_sdo_ctbl_db_505 / v_val_cotac_indic_econ)
               tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1  = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                                                            + (v_val_sdo_ctbl_cr_505 / v_val_cotac_indic_econ)
               tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim = tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim
                                                            + (v_val_sdo_ctbl_fim_505 / v_val_cotac_indic_econ).
        /* --- Caso considere apuraá∆o de resultado ---*/
        if  v_log_consid_apurac_restdo = yes
        then do:
            if  v_val_apurac_restdo_db_505 = 0 and v_val_apurac_restdo_cr_505 = 0
            then do:
                if  v_val_apurac_restdo_505 > 0
                then do:
                    assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                                                                       + (v_val_apurac_restdo_505 / v_val_cotac_indic_econ).
                end /* if */.
                else do:
                    assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                                                                       + (v_val_apurac_restdo_505 / v_val_cotac_indic_econ * -1).
                end /* else */.
            end /* if */.
            else do:
                assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                                                                   + (v_val_apurac_restdo_db_505 / v_val_cotac_indic_econ)
                       tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                                                                   + (v_val_apurac_restdo_cr_505 / v_val_cotac_indic_econ).
            end /* else */.

            assign tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim = tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim
                                                                + (v_val_apurac_restdo_acum_505 / v_val_cotac_indic_econ).
        end /* if */.
        else do:
            FIND GRP_CTA_CTBL OF cta_ctbl NO-LOCK.
            if cta_ctbl.cod_cta_ctbl = plano_cta_Ctbl.cod_cta_ctbl_apurac_restdo OR GRP_cTA_cTBL.LOG_CONSID_APURAC = NO then do:
               assign tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim = tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim 
                                                                   + (v_val_apurac_restdo_acum_505 / v_val_cotac_indic_econ)
                                                                   - (v_val_apurac_restdo_505  / v_val_cotac_indic_econ).
            end.
        end.
        /* --- Totais de DB e CR ---*/
        if  cta_ctbl.ind_espec_cta_ctbl = "Anal°tica" /*l_analitica*/ 
        then do:
            assign v_val_tot_sdo_ctbl_db = v_val_tot_sdo_ctbl_db
                                         + (v_val_sdo_ctbl_db_505 / v_val_cotac_indic_econ)
                   v_val_tot_sdo_ctbl_cr = v_val_tot_sdo_ctbl_cr
                                         + (v_val_sdo_ctbl_cr_505 / v_val_cotac_indic_econ).
            /* --- Caso considere apuraá∆o de resultado ---*/
            if  v_log_consid_apurac_restdo = yes
            then do:
                if  v_val_apurac_restdo_db_505 = 0 and v_val_apurac_restdo_cr_505 = 0
                then do:
                    if  v_val_apurac_restdo_505 > 0
                    then do:
                        assign v_val_tot_sdo_ctbl_db = v_val_tot_sdo_ctbl_db
                                                     + (v_val_apurac_restdo_505 / v_val_cotac_indic_econ).
                    end /* if */.
                    else do:
                        assign v_val_tot_sdo_ctbl_cr = v_val_tot_sdo_ctbl_cr
                                                     + (v_val_apurac_restdo_505 / v_val_cotac_indic_econ * -1).
                    end /* else */.
                end /* if */.
                else do:
                    assign v_val_tot_sdo_ctbl_db = v_val_tot_sdo_ctbl_db
                                                 + (v_val_apurac_restdo_db_505 / v_val_cotac_indic_econ)
                           v_val_tot_sdo_ctbl_cr = v_val_tot_sdo_ctbl_cr
                                                 + (v_val_apurac_restdo_cr_505 / v_val_cotac_indic_econ).
                end /* else */.
            end /* if */.
        end /* if */.
        /* --- Saldo do Per°odo Anterior ---*/
        if  v_dat_fim_period_ctbl_ant <> ?
        then do:
            /* FUT1082 - 09/07/2002
            N∆o zerava todas as vari†veis. */

            assign v_val_sdo_ctbl_db_505        = 0
                   v_val_sdo_ctbl_cr_505        = 0
                   v_val_sdo_ctbl_fim_505       = 0
                   v_val_apurac_restdo_db_505   = 0
                   v_val_apurac_restdo_cr_505   = 0
                   v_val_apurac_restdo_505      = 0
                   v_val_apurac_restdo_acum_505 = 0.

        &if '{&emsbas_version}' >= '5.05' &then
            for each tt_retorna_sdo_ctbl_demonst
                where tt_retorna_sdo_ctbl_demonst.tta_cod_empresa        = v_cod_unid_organ
                and   tt_retorna_sdo_ctbl_demonst.tta_cod_finalid_econ   = v_cod_finalid_econ_bas
                and   tt_retorna_sdo_ctbl_demonst.tta_cod_cenar_ctbl     = v_cod_cenar_ctbl
                and   tt_retorna_sdo_ctbl_demonst.tta_cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
                and   tt_retorna_sdo_ctbl_demonst.tta_cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl
                and   tt_retorna_sdo_ctbl_demonst.tta_cod_estab          = tt_estab_unid_negoc_select.cod_estab
                and   tt_retorna_sdo_ctbl_demonst.tta_cod_unid_negoc     = tt_estab_unid_negoc_select.cod_unid_negoc
                and   tt_retorna_sdo_ctbl_demonst.tta_dat_sdo_ctbl       = v_dat_fim_period_ctbl
                and   tt_retorna_sdo_ctbl_demonst.tta_cod_plano_ccusto   = ''
                &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                and   tt_retorna_sdo_ctbl_demonst.tta_cod_proj_financ    = tt_proj_financ_select.tta_cod_proj_financ
                &ENDIF
                :

                assign v_val_sdo_ctbl_db_505        = v_val_sdo_ctbl_db_505  + tt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_db
                       v_val_sdo_ctbl_cr_505        = v_val_sdo_ctbl_cr_505  + tt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_cr
                       v_val_sdo_ctbl_fim_505       = v_val_sdo_ctbl_fim_505 + tt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_fim
                       v_val_apurac_restdo_db_505   = v_val_apurac_restdo_db_505 + tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_db
                       v_val_apurac_restdo_cr_505   = v_val_apurac_restdo_cr_505 + tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_cr
                       v_val_apurac_restdo_505      = v_val_apurac_restdo_505 + tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo
                       v_val_apurac_restdo_acum_505 = v_val_apurac_restdo_acum_505 + tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_acum.
            end.
            /* ** VERIFICA SE CONTAS TEM SALDO ***/
            if v_val_sdo_ctbl_db_505  <> 0
            or v_val_sdo_ctbl_cr_505  <> 0
            or v_val_sdo_ctbl_fim_505 <> 0 then do:
        &else
            find b_sdo_cta_ctbl
                where b_sdo_cta_ctbl.cod_empresa        = sdo_cta_ctbl.cod_empresa
                and   b_sdo_cta_ctbl.cod_finalid_econ   = sdo_cta_ctbl.cod_finalid_econ
                and   b_sdo_cta_ctbl.cod_cenar_ctbl     = sdo_cta_ctbl.cod_cenar_ctbl
                and   b_sdo_cta_ctbl.cod_plano_cta_ctbl = sdo_cta_ctbl.cod_plano_cta_ctbl
                and   b_sdo_cta_ctbl.cod_cta_ctbl       = sdo_cta_ctbl.cod_cta_ctbl
                and   b_sdo_cta_ctbl.cod_estab          = sdo_cta_ctbl.cod_estab
                and   b_sdo_cta_ctbl.cod_unid_negoc     = sdo_cta_ctbl.cod_unid_negoc
                and   b_sdo_cta_ctbl.dat_sdo_ctbl       = v_dat_fim_period_ctbl_ant
                no-lock no-error.
            if  avail b_sdo_cta_ctbl
            then do:
                /* AndrÇ S.
                Atualizar todas as vari†veis pelo buffer da tabela SDO, a TT_retorna_sdo_ctbl Ç usada somente 5.05*/

                assign v_val_sdo_ctbl_db_505        = b_sdo_cta_ctbl.val_sdo_ctbl_db
                       v_val_sdo_ctbl_cr_505        = b_sdo_cta_ctbl.val_sdo_ctbl_cr
                       v_val_sdo_ctbl_fim_505       = b_sdo_cta_ctbl.val_sdo_ctbl_fim
                       v_val_apurac_restdo_db_505   = b_sdo_cta_ctbl.val_apurac_restdo_db
                       v_val_apurac_restdo_cr_505   = b_sdo_cta_ctbl.val_apurac_restdo_cr
                       v_val_apurac_restdo_505      = b_sdo_cta_ctbl.val_apurac_restdo
                       v_val_apurac_restdo_acum_505 = b_sdo_cta_ctbl.val_apurac_restdo_acum.
        &endif
                assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant  = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant
                                                                         + (v_val_sdo_ctbl_fim_505
                                                                         / v_val_cotac_indic_econ)
                       tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant
                                                                         + ((v_val_sdo_ctbl_fim_505
                                                                         -  v_val_sdo_ctbl_db_505 + v_val_sdo_ctbl_cr_505)
                                                                         /  v_val_cotac_indic_econ).
                /* --- Caso considere apuraá∆o de resultado ---*/
                if  v_log_consid_apurac_restdo = yes
                then do:
                    assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant  = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant
                                                                             + (v_val_apurac_restdo_acum_505 / v_val_cotac_indic_econ)
                           tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant
                                                                             - (v_val_apurac_restdo_505 / v_val_cotac_indic_econ).
                end /* if */.
                else do:
                    FIND GRP_CTA_CTBL OF cta_ctbl NO-LOCK.
                    if cta_ctbl.cod_cta_ctbl = plano_Cta_ctbl.cod_cta_ctbl_apurac_restdo
                         or grp_Cta_Ctbl.log_consid_apurac = no then do:
                        assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant 
                                                                                + (v_val_apurac_restdo_acum_505  / v_val_cotac_indic_econ) 
                                                                                - (v_val_apurac_restdo_505  / v_val_cotac_indic_econ). 
                    end.
                end.
            end /* if */.
        end /* if */.
    end /* if */.
    else do:
        /* Retirado essa validaá∆o pois j† valida na 
        hora de imprimir o balancete. Fut1090 - 04/10/02
        @if(v_log_cta_ctbl_sdo = yes)  /*--- Conta sem Saldos ---*/ */
        find tt_sdo_cta_ctbl_balanct
            where tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl   = cta_ctbl.cod_cta_ctbl
              and tt_sdo_cta_ctbl_balanct.tta_cod_estab      = v_cod_estab
              and tt_sdo_cta_ctbl_balanct.tta_cod_ccusto     = v_cod_ccusto_aux
              and tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc = v_cod_unid_negoc
              &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
              and tt_sdo_cta_ctbl_balanct.tta_cod_proj_financ = v_cod_proj_financ
              &ENDIF
              no-lock no-error.
        if  not avail tt_sdo_cta_ctbl_balanct
        then do:
            create tt_sdo_cta_ctbl_balanct.
            assign tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl   = cta_ctbl.cod_cta_ctbl
                   tt_sdo_cta_ctbl_balanct.tta_cod_ccusto     = v_cod_ccusto_aux
                   tt_sdo_cta_ctbl_balanct.tta_cod_estab      = v_cod_estab
                   tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc = v_cod_unid_negoc
                   tt_sdo_cta_ctbl_balanct.tta_cod_empresa    = v_cod_unid_organ
                   tt_sdo_cta_ctbl_balanct.tta_cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
                   tt_sdo_cta_ctbl_balanct.tta_cod_plano_ccusto = IF v_cod_ccusto_aux = "" THEN "" ELSE v_cod_plano_ccusto. 

            &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                assign tt_sdo_cta_ctbl_balanct.tta_cod_proj_financ = v_cod_proj_financ.
            &ENDIF
        end /* if */.
    end /* else */.
END PROCEDURE. /* pi_ler_sdo_cta_ctbl_balanct */
/*****************************************************************************
** Procedure Interna.....: pi_imprimir_sdo_cta_ctbl_balanct
** Descricao.............: pi_imprimir_sdo_cta_ctbl_balanct
** Criado por............: Henke
** Criado em.............: 14/11/1996 11:58:52
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_imprimir_sdo_cta_ctbl_balanct:

    /************************** Buffer Definition Begin *************************/

    def buffer b_cta_ctbl
        for cta_ctbl.


    /*************************** Buffer Definition End **************************/

    /************************* Variable Definition Begin ************************/

    def var v_cod_cta_ctbl_2                 as character       no-undo. /*local*/
    def var v_log_cta_ctbl_sdo_aux           as logical         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    for each tt_sdo_cta_ctbl_sem_aprop:
        delete tt_sdo_cta_ctbl_sem_aprop.
    end.
    if  v_log_cta_ctbl_sdo <> yes
    then do:
        assign v_log_cta_ctbl_sdo_aux = no.
        del_sem_sdo:
        for each tt_sdo_cta_ctbl_balanct:

            if  tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim = 0
            and tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1  = 0
            and tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1  = 0 then do:
                if  entry(1,dwb_rpt_param.cod_dwb_order) = "Estrutura" /*l_estrutura*/  then do:
                    if v_log_cta_ctbl_sdo_aux = no then do:
                        find  b_cta_ctbl no-lock
                        where b_cta_ctbl.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
                        and   b_cta_ctbl.cod_cta_ctbl       = tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl no-error.
                        if  avail b_cta_ctbl 
                        and b_cta_ctbl.ind_espec_cta_ctbl = "SintÇtica" /*l_sintetica*/ 
                        then do:
                            create tt_sdo_cta_ctbl_balanct_aux.
                            buffer-copy tt_sdo_cta_ctbl_balanct to tt_sdo_cta_ctbl_balanct_aux.
                            assign tt_sdo_cta_ctbl_balanct_aux.ttv_des_tit_ctbl_balanct = v_des_tit_ctbl_balanct.
                        end.
                    end.
                end.
                if tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant = 0 and
                   tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant  = 0 and
                   tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_inic     = 0 and
                   tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim      = 0 then do:
                    if avail tt_sdo_cta_ctbl_balanct then
                        delete tt_sdo_cta_ctbl_balanct.
                end.
            end.
            else do:
                assign v_log_cta_ctbl_sdo_aux = yes.
                for each tt_sdo_cta_ctbl_balanct_aux
                where tt_sdo_cta_ctbl_balanct_aux.tta_cod_cta_ctbl = tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl:
                    delete tt_sdo_cta_ctbl_balanct_aux.
                end.
            end.
            if avail tt_sdo_cta_ctbl_balanct then
                assign v_cod_cta_ctbl_2 = tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl.
        end /* for del_sem_sdo */.
    end /* if */.
    
    run pi_imprimir_sdo_cta_ctbl_balanct_2 (Input v_cod_cta_ctbl_2,
                                            Input yes) /*pi_imprimir_sdo_cta_ctbl_balanct_2*/.
END PROCEDURE. /* pi_imprimir_sdo_cta_ctbl_balanct */
/*****************************************************************************
** Procedure Interna.....: pi_retornar_indic_cr_sdo
** Descricao.............: pi_retornar_indic_cr_sdo
** Criado por............: Jane
** Criado em.............: // 
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_retornar_indic_cr_sdo:

    /************************ Parameter Definition Begin ************************/

    def input-output param p_val_sdo_ctbl
        as decimal
        format "->>,>>>,>>>,>>9.99"
        decimals 2
        no-undo.
    def output param p_ind_cr_sdo_ctbl
        as character
        format "X(01)"
        no-undo.


    /************************* Parameter Definition End *************************/

    if  p_val_sdo_ctbl < 0
    then do:
         assign p_val_sdo_ctbl    = p_val_sdo_ctbl * -1
                p_ind_cr_sdo_ctbl = "C" /*l_letra_C*/ .
    end /* if */.
    else do:
         if  p_val_sdo_ctbl > 0
         then do:
            assign p_ind_cr_sdo_ctbl = "D" /*l_D*/ .
         end /* if */.
    end /* else */.
END PROCEDURE. /* pi_retornar_indic_cr_sdo */
/*****************************************************************************
** Procedure Interna.....: pi_estrut_cta_ctbl_balanct
** Descricao.............: pi_estrut_cta_ctbl_balanct
** Criado por............: Henke
** Criado em.............: 17/11/1996 17:35:01
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_estrut_cta_ctbl_balanct:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_cta_ctbl
        as character
        format "x(20)"
        no-undo.
    def Input param p_log_val_maximum
        as logical
        format "Sim/N∆o"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************** Buffer Definition Begin *************************/

    def buffer b_estrut_cta_ctbl
        for estrut_cta_ctbl.


    /*************************** Buffer Definition End **************************/

    assign v_num_niv_aux = v_num_niv_aux + 1. 
    if  v_num_niv_aux <= v_num_niv_estrut
    then do:
        b_estrut_block:
        for
           each b_estrut_cta_ctbl no-lock
              where b_estrut_cta_ctbl.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
                and b_estrut_cta_ctbl.cod_cta_ctbl_pai   = p_cod_cta_ctbl:
             if  p_log_val_maximum = no
             then do: /* --- Extraá∆o de Saldos ---*/
                 find cta_ctbl
                    where cta_ctbl.cod_plano_cta_ctbl = b_estrut_cta_ctbl.cod_plano_cta_ctbl
                      and cta_ctbl.cod_cta_ctbl       = b_estrut_cta_ctbl.cod_cta_ctbl_filho
                         no-lock no-error.
                 /* --- Montagem do T°tulo ---*/
                 assign v_des_tit_ctbl_balanct = "".
                 run pi_des_tit_cta_ctbl_balanct /*pi_des_tit_cta_ctbl_balanct*/.
                 /* --- Pesquisa registros de Saldos Cont†beis ---*/
                 if  v_ind_tip_plano_cta_ctbl <> "Consolidaá∆o" /*l_consolidacao*/ 
                 then do:
                     /* --- Tipo de Saldo ---*/
                     if  v_log_sdo_ccusto = yes or v_log_ccusto_sum = no
                     then do:
                         &if '{&emsbas_version}' >= '5.05' &then
                             run pi_ler_sdo_cta_ctbl_ccusto_balanct_1 /*pi_ler_sdo_cta_ctbl_ccusto_balanct_1*/.
                         &else
                             run pi_ler_sdo_cta_ctbl_ccusto_balanct /*pi_ler_sdo_cta_ctbl_ccusto_balanct*/.
                         &endif
                         /* if return-value = @%(l_nok) then
                             @next(estab_unid_negoc_block).*/
                     end /* if */.
                     else
                         run pi_ler_sdo_cta_ctbl_balanct_1 /*pi_ler_sdo_cta_ctbl_balanct_1*/.
                 end /* if */.
                 else /* --- Sdo Cta Ctbl Consolid ---*/
                      run pi_ler_sdo_cta_consolid_balanct /*pi_ler_sdo_cta_consolid_balanct*/.
                 /* --- Imprimir Saldos da Conta ---*/
                 run pi_imprimir_sdo_cta_ctbl_balanct /*pi_imprimir_sdo_cta_ctbl_balanct*/.
                 assign v_val_current_value = v_val_current_value + 1.
                 run pi_percent_update (Input v_val_maximum,
                                        Input v_val_current_value,
                                        Input "") /*pi_percent_update*/.
                 /* --- Outros n°veis da Estrut Cta Ctbl ---*/
                 run pi_estrut_cta_ctbl_balanct (Input b_estrut_cta_ctbl.cod_cta_ctbl_filho,
                                                 Input no) /*pi_estrut_cta_ctbl_balanct*/.
             end /* if */.
             else do: /* --- Verificar quantidade de Contas selecionadas ---*/
                 find cta_ctbl
                   where cta_ctbl.cod_plano_cta_ctbl = b_estrut_cta_ctbl.cod_plano_cta_ctbl
                   and   cta_ctbl.cod_cta_ctbl       = b_estrut_cta_ctbl.cod_cta_ctbl_filho
                   no-lock no-error.
                 if not avail cta_ctbl then
                    next.

                 find tt_cta_ctbl_505
                    where tt_cta_ctbl_505.ttv_rec_cta_ctbl = recid(cta_ctbl)
                    no-error.
                 if not avail tt_cta_ctbl_505 then do:
                     create tt_cta_ctbl_505.
                     assign tt_cta_ctbl_505.ttv_rec_cta_ctbl = recid(cta_ctbl).
                 end.           

                 assign v_val_maximum = v_val_maximum + 1.
                 run pi_estrut_cta_ctbl_balanct (Input b_estrut_cta_ctbl.cod_cta_ctbl_filho,
                                                 Input yes) /*pi_estrut_cta_ctbl_balanct*/.
             end /* else */.
        end /* for b_estrut_block */.
    end /* if */.
    assign v_num_niv_aux = v_num_niv_aux - 1.
END PROCEDURE. /* pi_estrut_cta_ctbl_balanct */
/*****************************************************************************
** Procedure Interna.....: pi_des_tit_cta_ctbl_balanct
** Descricao.............: pi_des_tit_cta_ctbl_balanct
** Criado por............: Henke
** Criado em.............: 17/11/1996 17:48:05
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_des_tit_cta_ctbl_balanct:

    assign v_des_tit_ctbl_balanct = "". 
    if  v_log_cta_ctbl_internac = yes
    then do:  /* --- Considera Conta Internacional ---*/
        /* --- Classificado pela Conta Ctbl ---*/
        if  entry(1,dwb_rpt_param.cod_dwb_order) <> "Estrutura" /*l_estrutura*/ 
        then do:
            assign v_des_tit_ctbl_balanct = string(cta_ctbl.cod_cta_ctbl_padr_internac, 'x(30)').
        end /* if */.
        else do: /* --- Classificado pela Estrut Conta Ctbl ---*/
            assign v_des_tit_ctbl_balanct = string((fill(chr(32), (v_num_niv_aux - 1) * 2)
                                          + cta_ctbl.cod_cta_ctbl_padr_internac), 'x(30)').
        end /* else */.
    end /* if */.
    else do:
        find plano_cta_ctbl
           where plano_cta_ctbl.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl no-lock no-error.
        if  avail plano_cta_ctbl
        then do:
            /* --- Classificado pela Conta Ctbl ---*/
            if  entry(1,dwb_rpt_param.cod_dwb_order) <> "Estrutura" /*l_estrutura*/ 
            then do:
                assign v_des_tit_ctbl_balanct = string(cta_ctbl.cod_cta_ctbl, plano_cta_ctbl.cod_format_cta_ctbl).
            end /* if */.
            else do: /* --- Classificado pela Estrut Conta Ctbl ---*/
                assign v_des_tit_ctbl_balanct = fill(chr(32), (v_num_niv_aux - 1) * 2)
                                              + string(cta_ctbl.cod_cta_ctbl, plano_cta_ctbl.cod_format_cta_ctbl).
            end /* else */.
        end /* if */.
    end /* else */.
    if  v_log_idioma = yes
    then do: /* --- Idioma igual ao do param_geral_ems ---*/
       assign v_des_tit_ctbl_balanct = v_des_tit_ctbl_balanct
                                     + chr(32) + chr(45) + chr(32)
                                     + cta_ctbl.des_tit_ctbl.
    end /* if */.
    else do:
       find tit_ctbl
          where tit_ctbl.des_tit_ctbl = cta_ctbl.des_tit_ctbl no-lock no-error.
       find trad_tit_ctbl
          where trad_tit_ctbl.cod_idioma      = v_cod_idioma_apr
            and trad_tit_ctbl.num_id_tit_ctbl = tit_ctbl.num_id_tit_ctbl no-lock no-error.
       if  avail trad_tit_ctbl
       then do:
           assign v_des_tit_ctbl_balanct = v_des_tit_ctbl_balanct
                                         + chr(32) + chr(45) + chr(32)
                                         + trad_tit_ctbl.des_trad_tit_ctbl.
       end /* if */.
    end /* else */.
END PROCEDURE. /* pi_des_tit_cta_ctbl_balanct */
/*****************************************************************************
** Procedure Interna.....: pi_criar_ccusto_select
** Descricao.............: pi_criar_ccusto_select
** Criado por............: Henke
** Criado em.............: 18/11/1996 10:20:36
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_criar_ccusto_select:

    /************************* Variable Definition Begin ************************/

    def var v_num_dwb_order
        as integer
        format ">>>>,>>9":U
        no-undo.
    def var v_log_return                     as logical         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    /* --- Verifica se existe faixa para ems5.ccusto ---*/
    find first b_dwb_rpt_select
       where b_dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
       and   b_dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
       and   b_dwb_rpt_select.cod_dwb_field   = "Centro Custo"
       and   b_dwb_rpt_select.log_dwb_rule    = yes no-lock no-error.
    if  not avail b_dwb_rpt_select
    then do:
        find last dwb_rpt_select no-lock
           where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
             and dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
             and dwb_rpt_select.log_dwb_rule    = yes
           no-error.
        if  avail dwb_rpt_select
        then do:
            assign v_num_dwb_order =  dwb_rpt_select.num_dwb_order + 10.
        end /* if */.
        else do:
            assign v_num_dwb_order = 10.
        end /* else */.
        create b_dwb_rpt_select.
        assign b_dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
               b_dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
               b_dwb_rpt_select.cod_dwb_field   = "Centro Custo"
               b_dwb_rpt_select.num_dwb_order   = v_num_dwb_order
               b_dwb_rpt_select.log_dwb_rule    = yes.
        find first ems5.ccusto
            where ems5.ccusto.cod_empresa       = v_cod_unid_organ
              AND ems5.ccusto.cod_plano_ccusto = v_cod_plano_ccusto no-lock no-error.
        if  avail ems5.ccusto
        then do:
            assign b_dwb_rpt_select.cod_dwb_initial = ems5.ccusto.cod_ccusto.
        end /* if */.
        find last ems5.ccusto 
            where ems5.ccusto.cod_empresa      = v_cod_unid_organ
              AND ems5.ccusto.cod_plano_ccusto = v_cod_plano_ccusto no-lock no-error.
        if  avail ems5.ccusto
        then do:
            assign b_dwb_rpt_select.cod_dwb_final = ems5.ccusto.cod_ccusto.
        end /* if */.
        /* --- Plano ems5.ccusto ---*/
        if  v_cod_plano_ccusto <> ? and
            v_cod_plano_ccusto <> ""
        then do:
            find plano_ccusto
                where plano_ccusto.cod_empresa      = v_cod_unid_organ
                  and plano_ccusto.cod_plano_ccusto = v_cod_plano_ccusto no-lock no-error.
        end /* if */.
        if  avail plano_ccusto
        then do:
            ccusto_block:
            for
               each ems5.ccusto no-lock
                  where ems5.ccusto.cod_empresa      = v_cod_unid_organ   
                    and ems5.ccusto.cod_plano_ccusto = plano_ccusto.cod_plano_ccusto
                    and ems5.ccusto.cod_ccusto      >= b_dwb_rpt_select.cod_dwb_initial
                    and ems5.ccusto.cod_ccusto      <= b_dwb_rpt_select.cod_dwb_final:
               /* --- Verifica se Usu†rio tem permiss∆o de acesso ---*/
               run pi_verifica_segur_ccusto (buffer ems5.ccusto,
                                             output v_log_return) /*pi_verifica_segur_ccusto*/.
               if  v_log_return = no
               then do:
                   next ccusto_block.
               end /* if */.
               create tt_ccusto.
               assign tt_ccusto.ttv_rec_ccusto   = recid(ccusto)
                      tt_ccusto.cod_empresa      = ems5.ccusto.cod_empresa
                      tt_ccusto.cod_plano_ccusto = ems5.ccusto.cod_plano_ccusto
                      tt_ccusto.cod_ccusto       = ems5.ccusto.cod_ccusto
                      tt_ccusto.ttv_cod_format_ccusto = plano_ccusto.cod_format_ccusto.
            end /* for ems5.ccusto_block */.
        end /* if */.
        /* --- Elimina reg dwb_rpt_select ---*/
        delete b_dwb_rpt_select.
    end /* if */.
    else do:
        /* --- Plano ems5.ccusto ---*/
        if  v_cod_plano_ccusto <> ? and
            v_cod_plano_ccusto <> ""
        then do:
            find plano_ccusto
                where plano_ccusto.cod_empresa      = v_cod_unid_organ
                  and plano_ccusto.cod_plano_ccusto = v_cod_plano_ccusto no-lock no-error.
        end /* if */.
        if  avail plano_ccusto
        then do:
            select_block:
            for
               each dwb_rpt_select  exclusive-lock
                  where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
                  and   dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
                  and   dwb_rpt_select.cod_dwb_field   = "Centro Custo"
                  and   dwb_rpt_select.log_dwb_rule    = yes:
                assign dwb_rpt_select.cod_dwb_initial = replace(dwb_rpt_select.cod_dwb_initial,'-','')
                       dwb_rpt_select.cod_dwb_initial = replace(dwb_rpt_select.cod_dwb_initial,'.','')
                       dwb_rpt_select.cod_dwb_final   = replace(dwb_rpt_select.cod_dwb_final,'-','')
                       dwb_rpt_select.cod_dwb_final   = replace(dwb_rpt_select.cod_dwb_final,'.','').
                ccusto_block:
                for
                   each ems5.ccusto no-lock
                      where ems5.ccusto.cod_empresa      = v_cod_unid_organ  
                        and ems5.ccusto.cod_plano_ccusto = plano_ccusto.cod_plano_ccusto
                        and ems5.ccusto.cod_ccusto      >= dwb_rpt_select.cod_dwb_initial
                        and ems5.ccusto.cod_ccusto      <= dwb_rpt_select.cod_dwb_final:
                   /* --- Verifica se Usu†rio tem permiss∆o de acesso ---*/
                   run pi_verifica_segur_ccusto (buffer ems5.ccusto,
                                                 output v_log_return) /*pi_verifica_segur_ccusto*/.
                   if  v_log_return = no
                   then do:
                       next ccusto_block.
                   end /* if */.
                   find tt_ccusto
                      where tt_ccusto.ttv_rec_ccusto = recid(ccusto)
                      no-error.
                   if not avail tt_ccusto then do:
                       create tt_ccusto.
                       assign tt_ccusto.ttv_rec_ccusto   = recid(ccusto)
                              tt_ccusto.cod_empresa      = ems5.ccusto.cod_empresa
                              tt_ccusto.cod_plano_ccusto = ems5.ccusto.cod_plano_ccusto
                              tt_ccusto.cod_ccusto       = ems5.ccusto.cod_ccusto
                              tt_ccusto.ttv_cod_format_ccusto = plano_ccusto.cod_format_ccusto.
                   end.
                end /* for ems5.ccusto_block */.
            end /* for select_block */.
        end /* if */.
    end /* else */.
END PROCEDURE. /* pi_criar_ccusto_select */
/*****************************************************************************
** Procedure Interna.....: pi_verifica_segur_ccusto
** Descricao.............: pi_verifica_segur_ccusto
** Criado por............: Henke
** Criado em.............: 02/02/1996 16:26:15
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_verifica_segur_ccusto:

    /************************ Parameter Definition Begin ************************/

    def param buffer p_ccusto
        for ems5.ccusto.
    def output param p_log_return
        as logical
        format "Sim/N∆o"
        no-undo.


    /************************* Parameter Definition End *************************/

    assign p_log_return = no.
    /* default Ç n∆o ter permiss∆o */

    if can-find (first segur_ccusto
       where segur_ccusto.cod_empresa      = p_ccusto.cod_empresa
         and segur_ccusto.cod_plano_ccusto = p_ccusto.cod_plano_ccusto
         and segur_ccusto.cod_ccusto       = p_ccusto.cod_ccusto
         and segur_ccusto.cod_grp_usuar    = "*")
    then
        assign p_log_return = yes.
    else do:
        loop_block:
        for each usuar_grp_usuar no-lock
            where usuar_grp_usuar.cod_usuario = v_cod_usuar_corren
            &if '{&emsbas_version}' >= '5.01' &then
               use-index srgrpsr_usuario
            &endif:
            find first segur_ccusto no-lock
                 where segur_ccusto.cod_empresa = p_ccusto.cod_empresa
                   and segur_ccusto.cod_plano_ccusto = p_ccusto.cod_plano_ccusto
                   and segur_ccusto.cod_ccusto  = p_ccusto.cod_ccusto
                   and segur_ccusto.cod_grp_usuar = usuar_grp_usuar.cod_grp_usuar no-error.
            if  avail segur_ccusto
            then do:
                assign p_log_return = yes.
                leave loop_block.
            end /* if */.
        end /* for loop_block */.
    end /* else */.
END PROCEDURE. /* pi_verifica_segur_ccusto */
/*****************************************************************************
** Procedure Interna.....: pi_ler_sdo_cta_consolid_balanct
** Descricao.............: pi_ler_sdo_cta_consolid_balanct
** Criado por............: Henke
** Criado em.............: 17/12/1996 10:40:40
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_ler_sdo_cta_consolid_balanct:

    /************************** Buffer Definition Begin *************************/

    def buffer b_sdo_cta_ctbl_consolid
        for sdo_cta_ctbl_consolid.


    /*************************** Buffer Definition End **************************/

    b_selecao_block:
    for
       each b_dwb_rpt_select
          where b_dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
          and   b_dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
          and   b_dwb_rpt_select.cod_dwb_field   = "Unid Neg¢cio"
          and   b_dwb_rpt_select.log_dwb_rule    = yes no-lock:
        uneg_block:
        for
           each unid_negoc
              where unid_negoc.cod_unid_negoc >= b_dwb_rpt_select.cod_dwb_initial
              and   unid_negoc.cod_unid_negoc <= b_dwb_rpt_select.cod_dwb_final:
           /* --- Sumaria ou n∆o Unid Negoc ---*/
           if  v_log_unid_negoc_sum = yes
           then do:
               assign v_cod_unid_negoc = "".
           end /* if */.
           else do:
               assign v_cod_unid_negoc = unid_negoc.cod_unid_negoc.
           end /* else */.
           consolid_block:
           for
               each sdo_cta_ctbl_consolid no-lock
                  where sdo_cta_ctbl_consolid.cod_unid_organ   = v_cod_unid_organ
                    and sdo_cta_ctbl_consolid.cod_finalid_econ = v_cod_finalid_econ_bas
                    and sdo_cta_ctbl_consolid.cod_cenar_ctbl   = v_cod_cenar_ctbl
                    and sdo_cta_ctbl_consolid.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
                    and sdo_cta_ctbl_consolid.cod_cta_ctbl     = cta_ctbl.cod_cta_ctbl
                    and sdo_cta_ctbl_consolid.cod_unid_negoc   = unid_negoc.cod_unid_negoc
                    and sdo_cta_ctbl_consolid.dat_sdo_ctbl     = v_dat_fim_period_ctbl:
                find first tt_sdo_cta_ctbl_balanct
                    where tt_sdo_cta_ctbl_balanct.tta_cod_estab      = v_cod_estab
                      and tt_sdo_cta_ctbl_balanct.tta_cod_ccusto     = v_cod_ccusto
                      and tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc = v_cod_unid_negoc exclusive-lock no-error.
                if  not avail tt_sdo_cta_ctbl_balanct
                then do:
                    create tt_sdo_cta_ctbl_balanct.
                    assign tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl   = sdo_cta_ctbl_consolid.cod_cta_ctbl
                           tt_sdo_cta_ctbl_balanct.tta_cod_ccusto     = v_cod_ccusto
                           tt_sdo_cta_ctbl_balanct.tta_cod_estab      = v_cod_estab
                           tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc = v_cod_unid_negoc
                           tt_sdo_cta_ctbl_balanct.tta_cod_empresa        = v_cod_unid_organ          
                           tt_sdo_cta_ctbl_balanct.tta_cod_plano_cta_ctbl = v_cod_plano_cta_ctbl      
                           tt_sdo_cta_ctbl_balanct.tta_cod_plano_ccusto = IF v_cod_ccusto = "" THEN "" ELSE v_cod_plano_ccusto.

                end /* if */.
                assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                                                               + (sdo_cta_ctbl_consolid.val_sdo_ctbl_db / v_val_cotac_indic_econ)
                       tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                                                               + (sdo_cta_ctbl_consolid.val_sdo_ctbl_cr / v_val_cotac_indic_econ)
                       tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim = tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim
                                                               + (sdo_cta_ctbl_consolid.val_sdo_ctbl_fim / v_val_cotac_indic_econ).
                /* --- Caso considere apuraá∆o de resultado ---*/
                if  v_log_consid_apurac_restdo = yes
                then do:
                    if  sdo_cta_ctbl_consolid.val_apurac_restdo_db = 0 and sdo_cta_ctbl_consolid.val_apurac_restdo_cr = 0
                    then do:
                        if  sdo_cta_ctbl_consolid.val_apurac_restdo > 0
                        then do:
                            assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                                                                   + (sdo_cta_ctbl_consolid.val_apurac_restdo / v_val_cotac_indic_econ).
                        end /* if */.
                        else do:
                            assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                                                                   + (sdo_cta_ctbl_consolid.val_apurac_restdo / v_val_cotac_indic_econ * -1).
                        end /* else */.
                    end /* if */.
                    else do:
                        assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                                                                   + (sdo_cta_ctbl_consolid.val_apurac_restdo_db / v_val_cotac_indic_econ)
                               tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                                                                   + (sdo_cta_ctbl_consolid.val_apurac_restdo_cr / v_val_cotac_indic_econ).
                    end /* else */.

                    assign tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim = tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim
                                                               + (sdo_cta_ctbl_consolid.val_apurac_restdo_acum / v_val_cotac_indic_econ).
                end /* if */.
                else do:
                    FIND GRP_CTA_CTBL OF cta_ctbl NO-LOCK.
                    if sdo_cta_ctbl_consolid.cod_Cta_ctbl = plano_Cta_ctbl.cod_cta_ctbl_apurac_restdo
                         or grp_Cta_Ctbl.log_consid_apurac = no then do:
                       assign tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim = tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim 
                                                                           + (sdo_cta_ctbl_consolid.val_apurac_restdo_acum / v_val_cotac_indic_econ).
                    end.
                end.
                /* --- Totais de DB e CR ---*/
                if  cta_ctbl.ind_espec_cta_ctbl = "Anal°tica" /*l_analitica*/ 
                then do:
                    assign v_val_tot_sdo_ctbl_db = v_val_tot_sdo_ctbl_db
                                                 + (sdo_cta_ctbl_consolid.val_sdo_ctbl_db / v_val_cotac_indic_econ)
                           v_val_tot_sdo_ctbl_cr = v_val_tot_sdo_ctbl_cr
                                                 + (sdo_cta_ctbl_consolid.val_sdo_ctbl_cr / v_val_cotac_indic_econ).
                    /* --- Caso considere apuraá∆o de resultado ---*/
                    if  v_log_consid_apurac_restdo = yes
                    then do:
                        if  sdo_cta_ctbl_consolid.val_apurac_restdo_db = 0 and sdo_cta_ctbl_consolid.val_apurac_restdo_cr = 0
                        then do:
                           if  sdo_cta_ctbl_consolid.val_apurac_restdo > 0
                           then do:
                               assign v_val_tot_sdo_ctbl_db = v_val_tot_sdo_ctbl_db
                                                            + (sdo_cta_ctbl_consolid.val_apurac_restdo / v_val_cotac_indic_econ).
                           end /* if */.
                           else do:
                               assign v_val_tot_sdo_ctbl_cr = v_val_tot_sdo_ctbl_cr
                                                            + (sdo_cta_ctbl_consolid.val_apurac_restdo / v_val_cotac_indic_econ * -1).
                           end /* else */.
                        end /* if */.
                        else do:
                           assign v_val_tot_sdo_ctbl_db = v_val_tot_sdo_ctbl_db
                                                        + (sdo_cta_ctbl_consolid.val_apurac_restdo_db / v_val_cotac_indic_econ)
                                  v_val_tot_sdo_ctbl_cr = v_val_tot_sdo_ctbl_cr
                                                        + (sdo_cta_ctbl_consolid.val_apurac_restdo_cr / v_val_cotac_indic_econ).
                        end /* else */.
                    end /* if */.
                end /* if */.
                /* --- Saldo do Per°odo Anterior ---*/
                if  v_dat_fim_period_ctbl_ant <> ?
                then do:
                    b_consolid_block:
                    for
                        each b_sdo_cta_ctbl_consolid no-lock
                           where b_sdo_cta_ctbl_consolid.cod_unid_organ   = v_cod_unid_organ
                             and b_sdo_cta_ctbl_consolid.cod_finalid_econ = v_cod_finalid_econ_bas
                             and b_sdo_cta_ctbl_consolid.cod_cenar_ctbl   = v_cod_cenar_ctbl
                             and b_sdo_cta_ctbl_consolid.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
                             and b_sdo_cta_ctbl_consolid.cod_cta_ctbl     = cta_ctbl.cod_cta_ctbl
                             and b_sdo_cta_ctbl_consolid.cod_unid_negoc   = unid_negoc.cod_unid_negoc
                             and b_sdo_cta_ctbl_consolid.dat_sdo_ctbl     = v_dat_fim_period_ctbl_ant:
                        assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant
                                                                              + (b_sdo_cta_ctbl_consolid.val_sdo_ctbl_fim /
                                                                                 v_val_cotac_indic_econ)
                               tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant
                                                                              + ((tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant
                                                                              -  b_sdo_cta_ctbl_consolid.val_sdo_ctbl_db + b_sdo_cta_ctbl_consolid.val_sdo_ctbl_cr)
                                                                              /  v_val_cotac_indic_econ).
                        /* --- Caso considere apuraá∆o de resultado ---*/
                        if  v_log_consid_apurac_restdo = yes
                        then do:
                            assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant
                                                                       + (sdo_cta_ctbl_consolid.val_apurac_restdo_acum / v_val_cotac_indic_econ)
                                   /* FUT1082 - 09/07/2002
                                   Faltou somar o valor de apuraá∆o acumulado na f¢rmula. */
                                   tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant
                                                                           + ((sdo_cta_ctbl_consolid.val_apurac_restdo_acum - sdo_cta_ctbl_consolid.val_apurac_restdo) / v_val_cotac_indic_econ).
                        end /* if */.
                        else do:
                            FIND GRP_CTA_CTBL OF cta_ctbl NO-LOCK.
                            if b_sdo_cta_ctbl_consolid.cod_cta_ctbl = plano_Cta_ctbl.cod_cta_ctbl_apurac_restdo
                                 or grp_Cta_Ctbl.log_consid_apurac = no then do:
                               tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant
                                                                                + (b_sdo_cta_ctbl_consolid.val_apurac_restdo_acum / v_val_cotac_indic_econ). 
                            end.                                                                                   
                        end.
                    end /* for b_consolid_block */.
                end /* if */.
           end /* for consolid_block */.
        end /* for uneg_block */.
        if  v_log_cta_ctbl_sdo = yes
        then do:  /* --- Conta sem Saldos ---*/
            find first tt_sdo_cta_ctbl_balanct
                where tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl   = cta_ctbl.cod_cta_ctbl
                  and tt_sdo_cta_ctbl_balanct.tta_cod_estab      = v_cod_estab
                  and tt_sdo_cta_ctbl_balanct.tta_cod_ccusto     = v_cod_ccusto_aux
                  and tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc = v_cod_unid_negoc no-lock no-error.
            if  not avail tt_sdo_cta_ctbl_balanct
            then do:
                create tt_sdo_cta_ctbl_balanct.
                assign tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl   = cta_ctbl.cod_cta_ctbl
                       tt_sdo_cta_ctbl_balanct.tta_cod_ccusto     = v_cod_ccusto_aux
                       tt_sdo_cta_ctbl_balanct.tta_cod_estab      = v_cod_estab
                       tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc = v_cod_unid_negoc
                       tt_sdo_cta_ctbl_balanct.tta_cod_empresa        = v_cod_unid_organ    
                       tt_sdo_cta_ctbl_balanct.tta_cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
                       tt_sdo_cta_ctbl_balanct.tta_cod_plano_ccusto = IF v_cod_ccusto_aux = "" THEN "" ELSE v_cod_plano_ccusto.

            end /* if */.
        end /* if */.
    end /* for b_selecao_block */.
END PROCEDURE. /* pi_ler_sdo_cta_consolid_balanct */
/*****************************************************************************
** Procedure Interna.....: pi_verifica_pagina_impr_termos_diario
** Descricao.............: pi_verifica_pagina_impr_termos_diario
** Criado por............: Julio
** Criado em.............: 18/01/1999 09:19:17
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_verifica_pagina_impr_termos_diario:

    /************************* Variable Definition Begin ************************/

    def var v_log_impr_termo                 as logical         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    assign v_log_impr_termo = no.

       /* **Verifica encerrou livro p/imprimir termos encerramento abertura***/
       if  (line-counter(s_1) + 1) > v_rpt_s_1_bottom
       then do:
           /*page stream s_1.*/
           assign v_num_ult_pag = v_num_ult_pag + 1.
       end.    
       if  v_log_impr_termo_encert = no
       then do:
            if  v_log_control_pag_livro = yes
            then do:
                if  (page-number (s_1)) + v_num_pagina = v_num_pag_livro_fisc
                then do:
                    
                    if  v_des_lista_termo_encert <> ""
                    then do:
                        /*put stream s_1 skip(6).*/
                        termo:
                        do v_num_cont = 1 to (num-entries(v_des_lista_termo_encert,chr(10))):
                            assign v_des_termo_diario = entry(v_num_cont,v_des_lista_termo_encert,chr(10)).
                            run pi_print_editor ("s_1", v_des_termo_diario, "     060", "", "     ", "", "     ").
                            /*put stream s_1 unformatted 
                                entry(1, return-value, chr(255)) at 35 format "x(60)" skip.*/
                            run pi_print_editor ("s_1", v_des_termo_diario, "at035060", "", "", "", "").
                        end .
                    end .
                    
                    if  v_log_control_pag_livro = yes
                    then do:
                        assign v_log_livro_novo = yes.
                        run pi_atualizar_livro_diario_balancete .
                        assign v_log_livro_novo = no.
                    end. 
                    else do:
                        assign v_num_livro_fisc = v_num_livro_fisc + 1.
                        assign v_nom_report_title = fill(" ", 40 - length(v_rpt_s_1_name) - 3 - length(string(v_num_livro_fisc)))
                                                  + v_rpt_s_1_name + chr(32) + "-" + chr(32) + string(v_num_livro_fisc).
                    end. 

                    assign v_nom_report_title = fill(" ", 40 - length(v_rpt_s_1_name) - 3 - length(string(v_num_livro_fisc)))
                                              + v_rpt_s_1_name + chr(32) + "-" + chr(32) + string(v_num_livro_fisc).

                    
                    assign v_num_pagina = (page-number (s_1)) * -1. 
                    /*page stream s_1.*/
                    assign v_num_ult_pag = v_num_ult_pag + 1.
                    assign v_log_impr_termo = yes.
                end.
                if  v_log_impr_termo
                then do:
                    if  v_des_lista_termo_abert <> ""
                    then do:
                        /*put stream s_1 skip(6).*/
                        termo:
                        do v_num_cont = 1 to (num-entries(v_des_lista_termo_abert,chr(10))):
                             assign v_des_termo_diario = entry(v_num_cont,v_des_lista_termo_abert,chr(10)).
                            run pi_print_editor ("s_1", v_des_termo_diario, "     060", "", "     ", "", "     ").
                            /*put stream s_1 unformatted 
                                entry(1, return-value, chr(255)) at 35 format "x(60)" skip.*/
                            run pi_print_editor ("s_1", v_des_termo_diario, "at035060", "", "", "", "").
                        end .
                        /*page stream s_1.*/
                    end .
                end.
            end.
       end.    
       else do:
           assign v_nom_report_title = fill(" ", 40 - length(v_rpt_s_1_name) - 3 - length(string(v_num_livro_fisc)))
                                     + v_rpt_s_1_name + chr(32) + "-" + chr(32) + string(v_num_livro_fisc).

           if  (page-number (s_1)) + v_num_pagina = v_num_pag_livro_fisc 
           then do:
               
               if  v_des_lista_termo_encert <> ""
               then do:
                   /*put stream s_1 skip(6).*/
                   termo:
                   do v_num_cont = 1 to (num-entries(v_des_lista_termo_encert,chr(10))):
                       assign v_des_termo_diario = entry(v_num_cont,v_des_lista_termo_encert,chr(10)).
                       if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                           /*page stream s_1.
                       put stream s_1 unformatted 
                           v_des_termo_diario at 35 format 'x(60)' skip.*/
                   end.
                   /*page stream s_1.*/
               end.
               
               assign v_num_pagina = ((page-number (s_1)) * -1).
               /*page stream s_1.        */
               assign v_num_ult_pag = v_num_ult_pag + 1
                      v_num_pagina  = v_num_pagina  + 1.
               
               if  v_log_control_pag_livro = yes
               then do:
                   assign v_log_livro_novo = yes.
                   run pi_atualizar_livro_diario_balancete.
                   assign v_log_livro_novo = no.
               end.
               else do:
                   assign v_num_livro_fisc = v_num_livro_fisc + 1.
                   
                   assign v_nom_report_title = fill(" ", 40 - length(v_rpt_s_1_name) - 3 - length(string(v_num_livro_fisc)))
                                             + v_rpt_s_1_name + chr(32) + "-" + chr(32) + string(v_num_livro_fisc).
               end.
               
               if  v_des_lista_termo_abert <> ""
               then do:
                   /*put stream s_1 skip(6).*/
                   termo:
                   do v_num_cont = 1 to (num-entries(v_des_lista_termo_abert,chr(10))):
                       assign v_des_termo_diario = entry(v_num_cont,v_des_lista_termo_abert,chr(10)).
                       if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                       /*page stream s_1.
                       put stream s_1 unformatted 
                       v_des_termo_diario at 35 format 'x(60)' skip.*/
                   end .
               end.            
               /*page stream s_1.*/
           end .
       end.

       assign v_nom_report_title = fill(" ", 40 - length(v_rpt_s_1_name) - 3 - length(string(v_num_livro_fisc)))
                                 + v_rpt_s_1_name + chr(32) + "-" + chr(32) + string(v_num_livro_fisc).
END PROCEDURE. /* pi_verifica_pagina_impr_termos_diario */
/*****************************************************************************
** Procedure Interna.....: pi_atualizar_livro_diario_balancete
** Descricao.............: pi_atualizar_livro_diario_balancete
** Criado por............: Julio
** Criado em.............: 18/01/1999 10:51:14
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_atualizar_livro_diario_balancete:

    /************************** Buffer Definition Begin *************************/

    def buffer b_pag_livro_fisc
        for pag_livro_fisc.


    /*************************** Buffer Definition End **************************/

    /* Verifica se j† existe controle de p†gina para o per°odo*/
    find last  b_pag_livro_fisc exclusive-lock
         where b_pag_livro_fisc.cod_unid_organ = v_cod_empres_usuar
           and b_pag_livro_fisc.cod_modul_dtsul = "FGL" /*l_fgl*/ 
           and b_pag_livro_fisc.ind_tip_livro_fisc = "Di†rio" /*l_diario*/ 
           and b_pag_livro_fisc.num_livro_fisc = v_num_livro_fisc
           and b_pag_livro_fisc.dat_fim_emis = v_dat_fim_period_ctbl
           no-error.
    if  avail b_pag_livro_fisc
    then do:
        assign b_pag_livro_fisc.dat_inic_emis       = v_dat_inic_period_ctbl.
        if  v_log_impr_termo_encert = yes
        then do:
           assign b_pag_livro_fisc.num_ult_pag      = v_num_pagina + page-number (s_1). 
           if b_pag_livro_fisc.num_ult_pag < 0 then
              assign b_pag_livro_fisc.num_ult_pag   = v_num_pagina + page-number (s_1) + 1.
        end /* if */.
        else  
           assign b_pag_livro_fisc.num_ult_pag           = v_num_pagina + page-number (s_1).  
        assign b_pag_livro_fisc.cod_usuar_gerac_movto = v_cod_usuar_corren
               b_pag_livro_fisc.cod_usuar_gerac_movto = v_cod_usuar_corren
               b_pag_livro_fisc.dat_gerac_movto       = today
               b_pag_livro_fisc.hra_gerac_movto       = string(time,'999999')
               v_num_livro_fisc                       = b_pag_livro_fisc.num_livro_fisc.
        assign v_num_ult_pag = b_pag_livro_fisc.num_ult_pag.       
    end /* if */.
    else do:
        find first b_pag_livro_fisc exclusive-lock
             where b_pag_livro_fisc.cod_unid_organ = v_cod_empres_usuar
               and b_pag_livro_fisc.cod_modul_dtsul = "FGL" /*l_fgl*/ 
               and b_pag_livro_fisc.ind_tip_livro_fisc = "Di†rio" /*l_diario*/ 
               and b_pag_livro_fisc.dat_fim_emis <= v_dat_inic_period_ctbl no-error.
        if  avail b_pag_livro_fisc
        then do:
            create pag_livro_fisc.
            assign pag_livro_fisc.cod_unid_organ        = v_cod_empres_usuar
                   pag_livro_fisc.cod_modul_dtsul       = "FGL" /*l_fgl*/ 
                   pag_livro_fisc.ind_tip_livro_fisc    = "Di†rio" /*l_diario*/ 
                   pag_livro_fisc.dat_inic_emis         = v_dat_inic_period_ctbl
                   pag_livro_fisc.dat_fim_emis          = v_dat_fim_period_ctbl
                   pag_livro_fisc.num_ult_pag           = v_num_pagina + page-number (s_1)
                   pag_livro_fisc.num_primei_pag        = b_pag_livro_fisc.num_ult_pag + 1
                   pag_livro_fisc.cod_usuar_gerac_movto = v_cod_usuar_corren
                   pag_livro_fisc.dat_gerac_movto       = today
                   pag_livro_fisc.hra_gerac_movto       = string(time,'999999')
                   pag_livro_fisc.num_livro_fisc        = b_pag_livro_fisc.num_livro_fisc.
            if recid(pag_livro_fisc) <> ? then.
            assign v_num_ult_pag = pag_livro_fisc.num_ult_pag.               

        end /* if */.
        else do:
            assign v_num_ult_pag_tel    =  v_num_ult_pag_tel + 1.
            create pag_livro_fisc.
            assign pag_livro_fisc.cod_unid_organ        = v_cod_empres_usuar
                   pag_livro_fisc.cod_modul_dtsul       = "FGL" /*l_fgl*/ 
                   pag_livro_fisc.ind_tip_livro_fisc    = "Di†rio" /*l_diario*/ 
                   pag_livro_fisc.dat_inic_emis         = v_dat_inic_period_ctbl
                   pag_livro_fisc.dat_fim_emis          = v_dat_fim_period_ctbl
                   pag_livro_fisc.num_ult_pag           = v_num_pagina + page-number (s_1)
                   pag_livro_fisc.num_primei_pag        = v_num_ult_pag_tel
                   pag_livro_fisc.cod_usuar_gerac_movto = v_cod_usuar_corren
                   pag_livro_fisc.dat_gerac_movto       = today
                   pag_livro_fisc.hra_gerac_movto       = string(time,'999999')
                   pag_livro_fisc.num_livro_fisc        = v_num_livro_fisc.
            if recid(pag_livro_fisc) <> ? then.
           assign v_num_ult_pag = pag_livro_fisc.num_ult_pag.        
        end /* else */.
    end /* else */.
    if  v_log_livro_novo = yes
    then do:
        assign v_num_livro_fisc = v_num_livro_fisc + 1.
        find last b_pag_livro_fisc exclusive-lock
             where b_pag_livro_fisc.cod_unid_organ = v_cod_empres_usuar
               and b_pag_livro_fisc.cod_modul_dtsul = "fgl"
               and b_pag_livro_fisc.ind_tip_livro_fisc = "Di†rio"
               and b_pag_livro_fisc.num_livro_fisc = v_num_livro_fisc
               and b_pag_livro_fisc.dat_fim_emis = v_dat_fim_period_ctbl /*cl_pag_igual_diario of b_pag_livro_fisc*/ no-error.
        if  avail b_pag_livro_fisc
        then do:
            assign b_pag_livro_fisc.num_ult_pag           = v_num_pagina + page-number (s_1)
                   b_pag_livro_fisc.cod_usuar_gerac_movto = v_cod_usuar_corren
                   b_pag_livro_fisc.dat_gerac_movto       = today
                   b_pag_livro_fisc.hra_gerac_movto       = string(time,'999999').
            assign v_num_ult_pag = b_pag_livro_fisc.num_ult_pag.       
        end /* if */.
        else do:
            create pag_livro_fisc.
            assign pag_livro_fisc.cod_unid_organ        = v_cod_empres_usuar
                   pag_livro_fisc.cod_modul_dtsul       = "FGL" /*l_fgl*/ 
                   pag_livro_fisc.ind_tip_livro_fisc    = "Di†rio" /*l_diario*/ 
                   pag_livro_fisc.dat_inic_emis         = v_dat_inic_period_ctbl
                   pag_livro_fisc.dat_fim_emis          = v_dat_fim_period_ctbl
                   pag_livro_fisc.num_ult_pag           = v_num_ult_pag
                   pag_livro_fisc.num_primei_pag        = 1
                   pag_livro_fisc.cod_usuar_gerac_movto = v_cod_usuar_corren
                   pag_livro_fisc.dat_gerac_movto       = today
                   pag_livro_fisc.hra_gerac_movto       = string(time,'999999')
                   pag_livro_fisc.num_livro_fisc        = v_num_livro_fisc.
            if recid(pag_livro_fisc) <> ? then.
            assign v_num_ult_pag = pag_livro_fisc.num_ult_pag.       
        end /* else */.
    end /* if */.
END PROCEDURE. /* pi_atualizar_livro_diario_balancete */
/*****************************************************************************
** Procedure Interna.....: pi_acumula_valores_lucros_perdas
** Descricao.............: pi_acumula_valores_lucros_perdas
** Criado por............: Julio
** Criado em.............: 11/02/1999 15:39:22
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_acumula_valores_lucros_perdas:

    if grp_cta_ctbl.log_consid_apurac_restdo = yes then do:
       if v_ind_cr_sdo_inic <> "D" /*l_D*/  then 
          assign v_val_tot_sdo_inic_apurac = v_val_tot_sdo_inic_apurac - tt_grp_cta_ctbl.tta_val_sdo_ctbl_inic.
       else           
          assign v_val_tot_sdo_inic_apurac = v_val_tot_sdo_inic_apurac + tt_grp_cta_ctbl.tta_val_sdo_ctbl_inic.
       if v_ind_cr_sdo_fim <> "D" /*l_D*/  then
          assign v_val_tot_sdo_fim_apurac  = v_val_tot_sdo_fim_apurac  - tt_grp_cta_ctbl.tta_val_sdo_ctbl_fim.
       else 
          assign v_val_tot_sdo_fim_apurac  = v_val_tot_sdo_fim_apurac  + tt_grp_cta_ctbl.tta_val_sdo_ctbl_fim.
    end.          
    else do:
        if v_ind_cr_sdo_inic <> "D" /*l_D*/  then
           assign v_val_tot_sdo_inic_dem    = v_val_tot_sdo_inic_dem   - tt_grp_cta_ctbl.tta_val_sdo_ctbl_inic.
        else   
           assign v_val_tot_sdo_inic_dem    = v_val_tot_sdo_inic_dem   + tt_grp_cta_ctbl.tta_val_sdo_ctbl_inic.
        if v_ind_cr_sdo_fim <> "D" /*l_D*/  then
           assign v_val_tot_sdo_fim_dem     = v_val_tot_sdo_fim_dem    - tt_grp_cta_ctbl.tta_val_sdo_ctbl_fim.
        else
           assign v_val_tot_sdo_fim_dem     = v_val_tot_sdo_fim_dem    + tt_grp_cta_ctbl.tta_val_sdo_ctbl_fim.           
       if  v_cod_grp_cta_ctbl_nok = "" then
           assign  v_cod_grp_cta_ctbl_nok =  tt_grp_cta_ctbl.tta_cod_grp_cta_ctbl.  
       else    
           if not can-do(v_cod_grp_cta_ctbl_nok, tt_grp_cta_ctbl.tta_cod_grp_cta_ctbl) then
              assign v_cod_grp_cta_ctbl_nok        = v_cod_grp_cta_ctbl_nok + '/' + tt_grp_cta_ctbl.tta_cod_grp_cta_ctbl.
    end /* else */. 
END PROCEDURE. /* pi_acumula_valores_lucros_perdas */
/*****************************************************************************
** Procedure Interna.....: pi_imprime_valores_lucro_perda
** Descricao.............: pi_imprime_valores_lucro_perda
** Criado por............: Julio
** Criado em.............: 11/02/1999 15:39:52
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_imprime_valores_lucro_perda:

    if  v_log_period_ctbl_ant_impr = yes
    then do:
        if  v_nom_prog = "Di†rio" /*l_diario*/ 
        then do:
            /*if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                page stream s_1.*/

            run pi_verifica_pagina_impr_termos_diario /*pi_verifica_pagina_impr_termos_diario*/.

            /*put stream s_1 unformatted 
                skip (1).*/
            if  v_val_tot_sdo_inic_apurac <> 0 OR
                v_val_tot_sdo_fim_apurac  <> 0
            then do:
                run pi_retornar_indic_cr_sdo (input-output v_val_tot_sdo_inic_apurac,
                                              output v_ind_cr_sdo_inic) /*pi_retornar_indic_cr_sdo*/.
                /*put stream s_1 unformatted 
                    'Lucros e Perdas' at 1
                    v_val_tot_sdo_inic_apurac at 61 format '->,>>>,>>>,>>9.99'
                    v_ind_cr_sdo_inic at 78 format 'X(01)'.*/
                run pi_retornar_indic_cr_sdo (input-output v_val_tot_sdo_fim_apurac,
                                              output v_ind_cr_sdo_fim) /*pi_retornar_indic_cr_sdo*/.            
                /*put stream s_1 unformatted            
                    v_val_tot_sdo_fim_apurac at 115 format '->,>>>,>>>,>>9.99'
                    v_ind_cr_sdo_inic at 132 format 'X(01)' skip.*/
            end /* if */.                
            if  v_cod_grp_cta_ctbl_nok <> "" and
                v_val_tot_sdo_inic_dem <> 0  OR
                v_val_tot_sdo_fim_dem  <> 0
            then do:    
                run pi_retornar_indic_cr_sdo (input-output v_val_tot_sdo_inic_dem,
                                              output v_ind_cr_sdo_inic) /*pi_retornar_indic_cr_sdo*/.            
                /*put stream s_1 unformatted    
                    v_cod_grp_cta_ctbl_nok at 1 format 'X(55)'
                    v_val_tot_sdo_inic_dem at 61 format '->,>>>,>>>,>>9.99'
                    v_ind_cr_sdo_inic at 78 format 'X(01)'.*/
                run pi_retornar_indic_cr_sdo (input-output v_val_tot_sdo_fim_dem,
                                              output v_ind_cr_sdo_fim) /*pi_retornar_indic_cr_sdo*/.            
                /*put stream s_1 unformatted    
                    v_val_tot_sdo_fim_dem at 115 format '->,>>>,>>>,>>9.99'
                    v_ind_cr_sdo_inic at 132 format 'X(01)' skip.*/
            end /* if */.                
        end /* if */.            
        else do:
            /*if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                page stream s_1.
            put stream s_1 unformatted 
                skip (1).    */
            if  v_val_tot_sdo_inic_apurac <> 0 OR
                v_val_tot_sdo_fim_apurac  <> 0
            then do:
                run pi_retornar_indic_cr_sdo (input-output v_val_tot_sdo_inic_apurac,
                                              output v_ind_cr_sdo_inic) /*pi_retornar_indic_cr_sdo*/.
                /*put stream s_1 unformatted 
                    'Lucros e Perdas' at 1
                    v_val_tot_sdo_inic_apurac to 138 format '->>,>>>,>>>,>>9.99'
                    v_ind_cr_sdo_inic at 139 format 'X(01)'.*/
                run pi_retornar_indic_cr_sdo (input-output v_val_tot_sdo_fim_apurac,
                                              output v_ind_cr_sdo_fim) /*pi_retornar_indic_cr_sdo*/.
                /*put stream s_1 unformatted        
                    v_val_tot_sdo_fim_apurac to 156 format '->,>>>,>>>,>>9.99'
                    v_ind_cr_sdo_fim at 158 format 'X(01)' skip.*/
            end /* if */.                
            if  v_cod_grp_cta_ctbl_nok <> "" and
                v_val_tot_sdo_inic_dem <> 0  OR
                v_val_tot_sdo_fim_dem  <> 0
            then do:
                run pi_retornar_indic_cr_sdo (input-output v_val_tot_sdo_inic_dem,
                                              output v_ind_cr_sdo_inic) /*pi_retornar_indic_cr_sdo*/.    
                /*put stream s_1 unformatted    
                    v_cod_grp_cta_ctbl_nok at 1 format 'x(69)'
                    v_val_tot_sdo_inic_dem to 138 format '->>,>>>,>>>,>>9.99'
                    v_ind_cr_sdo_inic at 139 format 'X(01)'.*/
                run pi_retornar_indic_cr_sdo (input-output v_val_tot_sdo_fim_dem,
                                              output v_ind_cr_sdo_fim) /*pi_retornar_indic_cr_sdo*/.        
                /*put stream s_1 unformatted        
                    v_val_tot_sdo_fim_dem to 156 format '->,>>>,>>>,>>9.99'
                    v_ind_cr_sdo_fim at 158 format 'X(01)' skip.*/
            end /* if */.            
        end /* else */.    
    end /* if */.
    else do:    
        if  v_nom_prog = "Di†rio" /*l_diario*/ 
        then do:
            /*if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                page stream s_1.*/

            run pi_verifica_pagina_impr_termos_diario /*pi_verifica_pagina_impr_termos_diario*/.

            /*put stream s_1 unformatted 
                skip (1).    */
            if  v_val_tot_sdo_inic_apurac <> 0 OR
                v_val_tot_sdo_fim_apurac  <> 0
            then do:
                run pi_retornar_indic_cr_sdo (input-output v_val_tot_sdo_inic_apurac,
                                              output v_ind_cr_sdo_inic) /*pi_retornar_indic_cr_sdo*/.
                /*put stream s_1 unformatted 
                    'Lucros e Perdas' at 1
                    v_val_tot_sdo_inic_apurac at 61 format '->,>>>,>>>,>>9.99'
                    v_ind_cr_sdo_inic at 78 format 'X(01)'.*/
                run pi_retornar_indic_cr_sdo (input-output v_val_tot_sdo_fim_apurac,
                                              output v_ind_cr_sdo_fim) /*pi_retornar_indic_cr_sdo*/.            
                /*put stream s_1 unformatted            
                    v_val_tot_sdo_fim_apurac at 115 format '->,>>>,>>>,>>9.99'
                    v_ind_cr_sdo_inic at 132 format 'X(01)' skip.*/
            end /* if */.                
            if  v_cod_grp_cta_ctbl_nok <> "" and
                v_val_tot_sdo_inic_dem <> 0  OR
                v_val_tot_sdo_fim_dem  <> 0
            then do:
                run pi_retornar_indic_cr_sdo (input-output v_val_tot_sdo_inic_dem,
                                              output v_ind_cr_sdo_inic) /*pi_retornar_indic_cr_sdo*/.            
                /*put stream s_1 unformatted    
                    v_cod_grp_cta_ctbl_nok at 1 format 'X(55)'
                    v_val_tot_sdo_inic_dem at 61 format '->,>>>,>>>,>>9.99'
                    v_ind_cr_sdo_inic at 78 format 'X(01)'.*/
                run pi_retornar_indic_cr_sdo (input-output v_val_tot_sdo_fim_dem,
                                              output v_ind_cr_sdo_fim) /*pi_retornar_indic_cr_sdo*/.            
                /*put stream s_1 unformatted    
                    v_val_tot_sdo_fim_dem at 115 format '->,>>>,>>>,>>9.99'
                    v_ind_cr_sdo_inic at 132 format 'X(01)' skip.*/
            end /* if */.                
        end /* if */.            
        else do:
            /*if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                page stream s_1.
            put stream s_1 unformatted 
                skip (1).*/    
            if  v_val_tot_sdo_inic_apurac <> 0 OR
                v_val_tot_sdo_fim_apurac  <> 0
            then do:
                run pi_retornar_indic_cr_sdo (input-output v_val_tot_sdo_inic_apurac,
                                              output v_ind_cr_sdo_inic) /*pi_retornar_indic_cr_sdo*/.
                /*put stream s_1 unformatted 
                    'Lucros e Perdas' at 1
                    v_val_tot_sdo_inic_apurac at 82 format '->>,>>>,>>>,>>9.99'
                    v_ind_cr_sdo_inic at 100 format 'X(01)'.*/
                run pi_retornar_indic_cr_sdo (input-output v_val_tot_sdo_fim_apurac,
                                              output v_ind_cr_sdo_fim) /*pi_retornar_indic_cr_sdo*/.        
                /*put stream s_1 unformatted
                    v_val_tot_sdo_fim_apurac at 139 format '->>,>>>,>>>,>>9.99'
                    v_ind_cr_sdo_fim at 157 format 'X(01)' skip.*/
            end /* if */.                
            if  v_cod_grp_cta_ctbl_nok <> "" and
                v_val_tot_sdo_inic_dem <> 0  OR
                v_val_tot_sdo_fim_dem  <> 0
            then do:
                run pi_retornar_indic_cr_sdo (input-output v_val_tot_sdo_inic_dem,
                                              output v_ind_cr_sdo_inic) /*pi_retornar_indic_cr_sdo*/.            
                /*put stream s_1 unformatted        
                    v_cod_grp_cta_ctbl_nok at 1 format 'x(69)'
                    v_val_tot_sdo_inic_dem at 82 format '->>,>>>,>>>,>>9.99'
                    v_ind_cr_sdo_inic at 100 format 'X(01)'.*/
                run pi_retornar_indic_cr_sdo (input-output v_val_tot_sdo_fim_dem,
                                              output v_ind_cr_sdo_fim) /*pi_retornar_indic_cr_sdo*/.    
                /*put stream s_1 unformatted        
                    v_val_tot_sdo_fim_dem at 139 format '->>,>>>,>>>,>>9.99'
                    v_ind_cr_sdo_fim at 157 format 'X(01)' skip.*/
            end /* if */.                
        end /* else */.
    end /* else */.    
    /*if  (v_log_gerac_planilha = yes)
    then do:
        if  v_log_period_ctbl_ant_impr = yes
        then do:
            if (line-counter(s_planilha) + 4) > v_rpt_s_1_bottom then 
                page stream s_planilha. 
            put stream s_planilha unformatted  
            skip (1) 
            'Lucros e Perdas' at 1 
            v_cod_carac_lim at 70
            v_cod_carac_lim at 71
            v_cod_carac_lim at 72.

            if not v_log_estab_sum then 
                put stream s_planilha unformatted 
                    v_cod_carac_lim.

            if not v_log_unid_negoc_sum then 
                put stream s_planilha unformatted
                    v_cod_carac_lim.

            if not v_log_ccusto_sum then 
                put stream s_planilha unformatted 
                    v_cod_carac_lim.

            &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                if not v_log_proj_financ = yes then
                    put stream s_planilha unformatted 
                        v_cod_carac_lim.
            &endif

            put stream s_planilha unformatted         
                v_val_tot_sdo_inic_apurac to 126 format '->>,>>>,>>>,>>9.99' 
                v_cod_carac_lim at 128 format 'x(1)' 
                v_val_tot_sdo_fim_apurac to 173 format '->>,>>>,>>>,>>9.99'
                v_cod_carac_lim at 175 format 'x(1)' skip 
                v_cod_grp_cta_ctbl_nok at 1 format 'x(69)' 
                v_cod_carac_lim at 70
                v_cod_carac_lim at 71
                v_cod_carac_lim at 72.

            if not v_log_estab_sum then 
                put stream s_planilha unformatted 
                    v_cod_carac_lim.

            if not v_log_unid_negoc_sum then 
                put stream s_planilha unformatted
                    v_cod_carac_lim.

            if not v_log_ccusto_sum then 
                put stream s_planilha unformatted 
                    v_cod_carac_lim.

            &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                if not v_log_proj_financ = yes then
                    put stream s_planilha unformatted 
                        v_cod_carac_lim.
            &endif

            put stream s_planilha unformatted                     
                v_val_tot_sdo_inic_dem to 126 format '->>,>>>,>>>,>>9.99' 
                v_cod_carac_lim at 128 format 'x(1)' 
                v_val_tot_sdo_fim_dem to 173 format '->>,>>>,>>>,>>9.99' 
                v_cod_carac_lim at 175 format 'x(1)' skip (1). 
        end .
        else do:   
            if (line-counter(s_planilha) + 4) > v_rpt_s_1_bottom then 
                page stream s_planilha. 
            put stream s_planilha unformatted  
                skip (1) 
                'Lucros e Perdas' at 1 
                v_cod_carac_lim at 70.

            if not v_log_estab_sum then 
                put stream s_planilha unformatted 
                    v_cod_carac_lim.

            if not v_log_unid_negoc_sum then 
                put stream s_planilha unformatted
                    v_cod_carac_lim.

            if not v_log_ccusto_sum then 
                put stream s_planilha unformatted 
                    v_cod_carac_lim.

            &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                if not v_log_proj_financ = yes then
                    put stream s_planilha unformatted 
                        v_cod_carac_lim.
            &endif

            put stream s_planilha unformatted                  
                v_val_tot_sdo_inic_apurac to 93 format '->>,>>>,>>>,>>9.99' 
                v_cod_carac_lim at 109 format 'x(1)' 
                v_cod_carac_lim at 110 format 'x(1)'
                v_cod_carac_lim at 111 format 'x(1)'
                v_val_tot_sdo_fim_apurac to 133 format '->>,>>>,>>>,>>9.99' 
                v_cod_carac_lim at 135 format 'x(1)' skip 
                v_cod_grp_cta_ctbl_nok at 1 format 'x(69)' .

            if not v_log_estab_sum then 
                put stream s_planilha unformatted 
                    v_cod_carac_lim.

            if not v_log_unid_negoc_sum then 
                put stream s_planilha unformatted
                    v_cod_carac_lim.

            if not v_log_ccusto_sum then 
                put stream s_planilha unformatted 
                    v_cod_carac_lim.

            &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                if not v_log_proj_financ = yes then
                    put stream s_planilha unformatted 
                        v_cod_carac_lim.
            &endif

            put stream s_planilha unformatted
                v_cod_carac_lim
                v_val_tot_sdo_inic_dem to 93 format '->>,>>>,>>>,>>9.99' 
                v_cod_carac_lim at 109 format 'x(1)' 
                v_cod_carac_lim at 110 format 'x(1)'
                v_cod_carac_lim at 111 format 'x(1)'
                v_val_tot_sdo_fim_dem to 133 format '->>,>>>,>>>,>>9.99' 
                v_cod_carac_lim at 135 format 'x(1)' skip (1). 
        end.
    end. */
END PROCEDURE. /* pi_imprime_valores_lucro_perda */
/*****************************************************************************
** Procedure Interna.....: pi_retorna_val_sdo_cta_ctbl_balanct
** Descricao.............: pi_retorna_val_sdo_cta_ctbl_balanct
** Criado por............: Menna
** Criado em.............: 30/12/1999 11:56:17
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_retorna_val_sdo_cta_ctbl_balanct:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_cta_ctbl
        as character
        format "x(20)"
        no-undo.
    def Input param p_cod_estab
        as character
        format "x(3)"
        no-undo.
    def Input param p_cod_unid_negoc
        as character
        format "x(3)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_log_reg                        as logical         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    assign v_val_sdo_ctbl_db_505        = 0
           v_val_sdo_ctbl_cr_505        = 0
           v_val_sdo_ctbl_fim_505       = 0
           v_val_apurac_restdo_db_505   = 0
           v_val_apurac_restdo_cr_505   = 0
           v_val_apurac_restdo_505      = 0
           v_val_apurac_restdo_acum_505 = 0
           v_log_reg = no.

    &if '{&emsbas_version}' >= '5.05' &then
        for each tt_retorna_sdo_ctbl_demonst
            where tt_retorna_sdo_ctbl_demonst.tta_cod_empresa        = v_cod_unid_organ
            and   tt_retorna_sdo_ctbl_demonst.tta_cod_finalid_econ   = v_cod_finalid_econ_bas
            and   tt_retorna_sdo_ctbl_demonst.tta_cod_cenar_ctbl     = v_cod_cenar_ctbl
            and   tt_retorna_sdo_ctbl_demonst.tta_cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
            and   tt_retorna_sdo_ctbl_demonst.tta_cod_cta_ctbl       = p_cod_cta_ctbl
            and   tt_retorna_sdo_ctbl_demonst.tta_cod_estab          = tt_estab_unid_negoc_select.cod_estab
            and   tt_retorna_sdo_ctbl_demonst.tta_cod_unid_negoc     = tt_estab_unid_negoc_select.cod_unid_negoc
            and   tt_retorna_sdo_ctbl_demonst.tta_dat_sdo_ctbl       = v_dat_fim_period_ctbl
            and   tt_retorna_sdo_ctbl_demonst.tta_cod_plano_ccusto   = "" /*l_null*/ 
            &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
            and   tt_retorna_sdo_ctbl_demonst.tta_cod_proj_financ    = tt_proj_financ_select.tta_cod_proj_financ
            &ENDIF :

            assign v_val_sdo_ctbl_db_505        = v_val_sdo_ctbl_db_505        + tt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_db 
                   v_val_sdo_ctbl_cr_505        = v_val_sdo_ctbl_cr_505        + tt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_cr 
                   v_val_sdo_ctbl_fim_505       = v_val_sdo_ctbl_fim_505       + tt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_fim 
                   v_val_apurac_restdo_505      = v_val_apurac_restdo_505      + tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo 
                   v_val_apurac_restdo_db_505   = v_val_apurac_restdo_db_505   + tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_db 
                   v_val_apurac_restdo_cr_505   = v_val_apurac_restdo_cr_505   + tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_cr 
                   v_val_apurac_restdo_acum_505 = v_val_apurac_restdo_acum_505 + tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_acum.
                   v_log_reg = yes /* * log utilizado para indicar que existe registro de saldo (independente do valor) **/.
        end.
        if  not v_log_reg then return.

    &else
    find first sdo_cta_ctbl
        where sdo_cta_ctbl.cod_empresa        = v_cod_unid_organ
        and   sdo_cta_ctbl.cod_finalid_econ   = v_cod_finalid_econ_bas
        and   sdo_cta_ctbl.cod_cenar_ctbl     = v_cod_cenar_ctbl
        and   sdo_cta_ctbl.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
        and   sdo_cta_ctbl.cod_cta_ctbl       = p_cod_cta_ctbl
        and   sdo_cta_ctbl.cod_estab          = tt_estab_unid_negoc_select.cod_estab
        and   sdo_cta_ctbl.cod_unid_negoc     = tt_estab_unid_negoc_select.cod_unid_negoc
        and   sdo_cta_ctbl.dat_sdo_ctbl       = v_dat_fim_period_ctbl
        no-lock no-error.
    if  not avail sdo_cta_ctbl then return.

    assign v_val_sdo_ctbl_db_505        = sdo_cta_ctbl.val_sdo_ctbl_db
           v_val_sdo_ctbl_cr_505        = sdo_cta_ctbl.val_sdo_ctbl_cr
           v_val_sdo_ctbl_fim_505       = sdo_cta_ctbl.val_sdo_ctbl_fim
           v_val_apurac_restdo_505      = sdo_cta_ctbl.val_apurac_restdo
           v_val_apurac_restdo_db_505   = sdo_cta_ctbl.val_apurac_restdo_db
           v_val_apurac_restdo_cr_505   = sdo_cta_ctbl.val_apurac_restdo_cr
           v_val_apurac_restdo_acum_505 = sdo_cta_ctbl.val_apurac_restdo_acum.
    &endif

    find first tt_sdo_cta_ctbl_sem_aprop
        where tt_sdo_cta_ctbl_sem_aprop.tta_cod_cta_ctbl   = p_cod_cta_ctbl
        and   tt_sdo_cta_ctbl_sem_aprop.tta_cod_estab      = v_cod_estab
        and   tt_sdo_cta_ctbl_sem_aprop.tta_cod_unid_negoc = v_cod_unid_negoc
        &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
        and   tt_sdo_cta_ctbl_sem_aprop.tta_cod_unid_negoc = v_cod_proj_financ
        &ENDIF    
        exclusive-lock no-error.
    if  not avail tt_sdo_cta_ctbl_sem_aprop then do:
        create tt_sdo_cta_ctbl_sem_aprop.
        assign tt_sdo_cta_ctbl_sem_aprop.tta_cod_cta_ctbl   = p_cod_cta_ctbl
               tt_sdo_cta_ctbl_sem_aprop.tta_cod_estab      = v_cod_estab
               tt_sdo_cta_ctbl_sem_aprop.tta_cod_unid_negoc = v_cod_unid_negoc.
              &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
              assign tt_sdo_cta_ctbl_sem_aprop.tta_cod_unid_negoc = v_cod_proj_financ.
              &ENDIF

    end.

    assign tt_sdo_cta_ctbl_sem_aprop.tta_val_sdo_ctbl_db = tt_sdo_cta_ctbl_sem_aprop.tta_val_sdo_ctbl_db
                                                         + (v_val_sdo_ctbl_db_505 / v_val_cotac_indic_econ)
           tt_sdo_cta_ctbl_sem_aprop.tta_val_sdo_ctbl_cr = tt_sdo_cta_ctbl_sem_aprop.tta_val_sdo_ctbl_cr
                                                         + (v_val_sdo_ctbl_cr_505 / v_val_cotac_indic_econ)
           tt_sdo_cta_ctbl_sem_aprop.tta_val_sdo_ctbl_fim = tt_sdo_cta_ctbl_sem_aprop.tta_val_sdo_ctbl_fim
                                                          + (v_val_sdo_ctbl_fim_505 / v_val_cotac_indic_econ).

    /* --- Caso considere apuraá∆o de resultado ---*/
    if  v_log_consid_apurac_restdo = yes
    then do:
        if  v_val_apurac_restdo_db_505 = 0
        and v_val_apurac_restdo_cr_505 = 0 then do:
            if  v_val_apurac_restdo_505 > 0 then
                assign tt_sdo_cta_ctbl_sem_aprop.tta_val_sdo_ctbl_db = tt_sdo_cta_ctbl_sem_aprop.tta_val_sdo_ctbl_db
                                                                     + (v_val_apurac_restdo_505 / v_val_cotac_indic_econ).
            else
                assign tt_sdo_cta_ctbl_sem_aprop.tta_val_sdo_ctbl_cr = tt_sdo_cta_ctbl_sem_aprop.tta_val_sdo_ctbl_cr
                                                                     + (v_val_apurac_restdo_505 / v_val_cotac_indic_econ * -1).
        end.
        else
            assign tt_sdo_cta_ctbl_sem_aprop.tta_val_sdo_ctbl_db = tt_sdo_cta_ctbl_sem_aprop.tta_val_sdo_ctbl_db
                                                                 + (v_val_apurac_restdo_db_505 / v_val_cotac_indic_econ)
                   tt_sdo_cta_ctbl_sem_aprop.tta_val_sdo_ctbl_cr = tt_sdo_cta_ctbl_sem_aprop.tta_val_sdo_ctbl_cr
                                                                 + (v_val_apurac_restdo_cr_505 / v_val_cotac_indic_econ).

        assign tt_sdo_cta_ctbl_sem_aprop.tta_val_sdo_ctbl_fim = tt_sdo_cta_ctbl_sem_aprop.tta_val_sdo_ctbl_fim
                                                              + (v_val_apurac_restdo_acum_505 / v_val_cotac_indic_econ).
    end.
    else do:
        FIND GRP_CTA_CTBL OF cta_ctbl NO-LOCK.
        if cta_ctbl.cod_cta_ctbl = plano_cta_ctbl.cod_cta_ctbl_apurac_restdo
             or grp_Cta_Ctbl.log_consid_apurac = no then do:
           assign tt_sdo_cta_ctbl_sem_aprop.tta_val_sdo_ctbl_fim = tt_sdo_cta_ctbl_sem_aprop.tta_val_sdo_ctbl_fim 
                                                                 + (v_val_apurac_restdo_acum_505 / v_val_cotac_indic_econ)
                                                                 - (v_val_apurac_restdo_505  / v_val_cotac_indic_econ).
        end.
    end.

    /* --- Saldo do Per°odo Anterior ---*/
    if  v_dat_fim_period_ctbl_ant <> ? then do:
        assign v_val_sdo_ctbl_db_505        = 0
               v_val_sdo_ctbl_cr_505        = 0
               v_val_sdo_ctbl_fim_505       = 0
               v_val_apurac_restdo_505      = 0
               v_val_apurac_restdo_acum_505 = 0.

    &if '{&emsbas_version}' >= '5.05' &then
        for each btt_retorna_sdo_ctbl_demonst
            where btt_retorna_sdo_ctbl_demonst.tta_cod_empresa        = v_cod_unid_organ
            and   btt_retorna_sdo_ctbl_demonst.tta_cod_finalid_econ   = v_cod_finalid_econ_bas
            and   btt_retorna_sdo_ctbl_demonst.tta_cod_cenar_ctbl     = v_cod_cenar_ctbl
            and   btt_retorna_sdo_ctbl_demonst.tta_cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
            and   btt_retorna_sdo_ctbl_demonst.tta_cod_cta_ctbl       = p_cod_cta_ctbl
            and   btt_retorna_sdo_ctbl_demonst.tta_cod_estab          = tt_estab_unid_negoc_select.cod_estab
            and   btt_retorna_sdo_ctbl_demonst.tta_cod_unid_negoc     = tt_estab_unid_negoc_select.cod_unid_negoc
            and   btt_retorna_sdo_ctbl_demonst.tta_dat_sdo_ctbl       = v_dat_fim_period_ctbl_ant
            and   btt_retorna_sdo_ctbl_demonst.tta_cod_plano_ccusto   = "" /*l_null*/ 
            &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
            and   btt_retorna_sdo_ctbl_demonst.tta_cod_proj_financ    = tt_proj_financ_select.tta_cod_proj_financ
            &ENDIF :

            assign v_val_sdo_ctbl_db_505        = v_val_sdo_ctbl_db_505        + btt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_db
                   v_val_sdo_ctbl_cr_505        = v_val_sdo_ctbl_cr_505        + btt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_cr
                   v_val_sdo_ctbl_fim_505       = v_val_sdo_ctbl_fim_505       + btt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_fim
                   v_val_apurac_restdo_505      = v_val_apurac_restdo_505      + btt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo
                   v_val_apurac_restdo_acum_505 = v_val_apurac_restdo_acum_505 + btt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_acum.
        end.    
        if  v_log_reg
        then do:
    &else
        find b_sdo_cta_ctbl
            where b_sdo_cta_ctbl.cod_empresa        = sdo_cta_ctbl.cod_empresa
            and   b_sdo_cta_ctbl.cod_finalid_econ   = sdo_cta_ctbl.cod_finalid_econ
            and   b_sdo_cta_ctbl.cod_cenar_ctbl     = sdo_cta_ctbl.cod_cenar_ctbl
            and   b_sdo_cta_ctbl.cod_plano_cta_ctbl = sdo_cta_ctbl.cod_plano_cta_ctbl
            and   b_sdo_cta_ctbl.cod_cta_ctbl       = sdo_cta_ctbl.cod_cta_ctbl
            and   b_sdo_cta_ctbl.cod_estab          = sdo_cta_ctbl.cod_estab
            and   b_sdo_cta_ctbl.cod_unid_negoc     = sdo_cta_ctbl.cod_unid_negoc
            and   b_sdo_cta_ctbl.dat_sdo_ctbl       = v_dat_fim_period_ctbl_ant
            no-lock no-error.
        if  avail b_sdo_cta_ctbl
        then do:
            assign v_val_sdo_ctbl_db_505        = b_sdo_cta_ctbl.val_sdo_ctbl_db
                   v_val_sdo_ctbl_cr_505        = b_sdo_cta_ctbl.val_sdo_ctbl_cr
                   v_val_sdo_ctbl_fim_505       = b_sdo_cta_ctbl.val_sdo_ctbl_fim
                   v_val_apurac_restdo_505      = b_sdo_cta_ctbl.val_apurac_restdo
                   v_val_apurac_restdo_acum_505 = b_sdo_cta_ctbl.val_apurac_restdo_acum.
    &endif
            assign tt_sdo_cta_ctbl_sem_aprop.ttv_val_sdo_ctbl_fim_ant = tt_sdo_cta_ctbl_sem_aprop.ttv_val_sdo_ctbl_fim_ant
                                                                      + (v_val_sdo_ctbl_fim_505
                                                                      / v_val_cotac_indic_econ)
                   tt_sdo_cta_ctbl_sem_aprop.ttv_val_sdo_ctbl_inic_ant = tt_sdo_cta_ctbl_sem_aprop.ttv_val_sdo_ctbl_inic_ant
                                                                       + ((v_val_sdo_ctbl_fim_505
                                                                       -  v_val_sdo_ctbl_db_505 + v_val_sdo_ctbl_cr_505)
                                                                       /  v_val_cotac_indic_econ).
            /* --- Caso considere apuraá∆o de resultado ---*/
            if  v_log_consid_apurac_restdo = yes
            then do:
                assign tt_sdo_cta_ctbl_sem_aprop.ttv_val_sdo_ctbl_fim_ant = tt_sdo_cta_ctbl_sem_aprop.ttv_val_sdo_ctbl_fim_ant
                                                                          + (v_val_apurac_restdo_acum_505 / v_val_cotac_indic_econ)
                       tt_sdo_cta_ctbl_sem_aprop.ttv_val_sdo_ctbl_inic_ant = tt_sdo_cta_ctbl_sem_aprop.ttv_val_sdo_ctbl_inic_ant
                                                                           - (v_val_apurac_restdo_505 / v_val_cotac_indic_econ).
            end.
            else do:
                FIND GRP_CTA_CTBL OF cta_ctbl NO-LOCK.
                    if cta_ctbl.cod_Cta_ctbl = plano_Cta_ctbl.cod_cta_ctbl_apurac_restdo
                         or grp_Cta_ctbl.log_consid_apurac = no  then do:
                       assign tt_sdo_cta_ctbl_sem_aprop.ttv_val_sdo_ctbl_fim_ant = tt_sdo_cta_ctbl_sem_aprop.ttv_val_sdo_ctbl_fim_ant 
                                                                                 + (v_val_apurac_restdo_acum_505  / v_val_cotac_indic_econ) 
                                                                                 - (v_val_apurac_restdo_505  / v_val_cotac_indic_econ).
                    end.
            end.
        end.
    end.
END PROCEDURE. /* pi_retorna_val_sdo_cta_ctbl_balanct */
/*****************************************************************************
** Procedure Interna.....: pi_retorna_usa_segur_estab
** Descricao.............: pi_retorna_usa_segur_estab
** Criado por............: Barth
** Criado em.............: 17/04/2000 22:23:16
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_retorna_usa_segur_estab:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_demonst_ctbl
        as character
        format "x(8)"
        no-undo.
    def output param p_log_restric_estab
        as logical
        format "Sim/N∆o"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************** Buffer Definition Begin *************************/

    def buffer b_demonst_ctbl
        for demonst_ctbl.
    def buffer b_param_geral_gld
        for param_geral_gld.


    /*************************** Buffer Definition End **************************/

    /* *****
    **
    **  O PARAMETRO P_COD_DEMONST_CTBL PODE SER USADO DE TRES FORMAS:
    **
    **    ?  - RETORNA SE A FUN¯∞O BF_FIN_SEGUR_DEMONST_MGL EST∆ HABILITADA (OU EXECUTOU PROGRAMA ESPECIAL QUE LIBERA).
    **    "" - RETORNA SE NO PARAMETRO GERAL DA CONTABILIDADE USA O CONTROLE DE ESTABELECIMENTO.
    **    <cod_demonst> - RETORNA SE O DEMONSTRATIVO CADASTRADO USA O CONTROLE.
    **
    *******/

    &if  defined(BF_FIN_SEGUR_DEMONST_MGL) &then

        if  p_cod_demonst_ctbl = ? then do:        /* ** RETORNA SE FUN¯∞O HABILITADA PARA ESTA VERS∞O ***/
            assign p_log_restric_estab = yes.
        end.
        else if  p_cod_demonst_ctbl <> ""          /* ** RETORNA SE DEMONSTRATIVO USA CONTROLE ***/
        then do:
            find b_demonst_ctbl where b_demonst_ctbl.cod_demonst_ctbl = p_cod_demonst_ctbl no-lock no-error.
            if  avail b_demonst_ctbl then
                assign p_log_restric_estab = b_demonst_ctbl.log_restric_estab.
        end.
        else do:                                   /* ** RETORNA SE PARAM GERAL CTBL USA CONTROLE ***/
            find last b_param_geral_gld no-lock no-error.
            assign p_log_restric_estab = b_param_geral_gld.log_restric_estab.
        end.

    &elseif '{&emsfin_version}' >= '5.02' &then

        find histor_exec_especial
            where histor_exec_especial.cod_modul_dtsul = 'MGL'
            and   histor_exec_especial.cod_prog_dtsul  = "fnc_segur_demonst_mgl":U
            no-lock no-error.
        if  not avail histor_exec_especial then
            return.

        if  p_cod_demonst_ctbl = ? then do:        /* ** RETORNA SE FUN¯∞O HABILITADA PARA ESTA VERS∞O ***/
            assign p_log_restric_estab = yes.
        end.
        else if  p_cod_demonst_ctbl <> ""          /* ** RETORNA SE DEMONSTRATIVO USA CONTROLE ***/
        then do:
            find b_demonst_ctbl where b_demonst_ctbl.cod_demonst_ctbl = p_cod_demonst_ctbl no-lock no-error.
            if  avail b_demonst_ctbl then
                assign p_log_restric_estab = b_demonst_ctbl.log_livre_1.
        end.
        else do:                                   /* ** RETORNA SE PARAM GERAL CTBL USA CONTROLE ***/
            find last b_param_geral_gld no-lock no-error.
            assign p_log_restric_estab = (b_param_geral_gld.num_livre_1 = 1).
        end.

    &endif

END PROCEDURE. /* pi_retorna_usa_segur_estab */
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
            find emsbas.prog_dtsul 
                where emsbas.prog_dtsul.cod_prog_dtsul = p_cod_program 
                no-lock no-error.
            if  avail emsbas.prog_dtsul
            then do:
                &if '{&emsbas_version}' > '5.00' &then
                    if  emsbas.prog_dtsul.nom_prog_dpc <> '' then
                        put stream s-arq 'DPC : ' at 5 emsbas.prog_dtsul.nom_prog_dpc  at 15 skip.
                &endif
                if  emsbas.prog_dtsul.nom_prog_appc <> '' then
                    put stream s-arq 'APPC: ' at 5 emsbas.prog_dtsul.nom_prog_appc at 15 skip.
                if  emsbas.prog_dtsul.nom_prog_upc <> '' then
                    put stream s-arq 'UPC : ' at 5 emsbas.prog_dtsul.nom_prog_upc  at 15 skip.
            end /* if */.
            &endif
        end.

        if  p_cod_program_type = 'dic' then do:
            &if '{&emsbas_version}' > '1.00' &then
            find emsbas.tab_dic_dtsul 
                where emsbas.tab_dic_dtsul.cod_tab_dic_dtsul = p_cod_program 
                no-lock no-error.
            if  avail emsbas.tab_dic_dtsul
            then do:
                &if '{&emsbas_version}' > '5.00' &then
                    if  emsbas.tab_dic_dtsul.nom_prog_dpc_gat_delete <> '' then
                        put stream s-arq 'DPC-DELETE : ' at 5 emsbas.tab_dic_dtsul.nom_prog_dpc_gat_delete  at 25 skip.
                &endif
                if  emsbas.tab_dic_dtsul.nom_prog_appc_gat_delete <> '' then
                    put stream s-arq 'APPC-DELETE: ' at 5 emsbas.tab_dic_dtsul.nom_prog_appc_gat_delete at 25 skip.
                if  emsbas.tab_dic_dtsul.nom_prog_upc_gat_delete <> '' then
                    put stream s-arq 'UPC-DELETE : ' at 5 emsbas.tab_dic_dtsul.nom_prog_upc_gat_delete  at 25 skip.
                &if '{&emsbas_version}' > '5.00' &then
                    if  emsbas.tab_dic_dtsul.nom_prog_dpc_gat_write <> '' then
                        put stream s-arq 'DPC-WRITE : ' at 5 emsbas.tab_dic_dtsul.nom_prog_dpc_gat_write  at 25 skip.
                &endif
                if  emsbas.tab_dic_dtsul.nom_prog_appc_gat_write <> '' then
                    put stream s-arq 'APPC-WRITE: ' at 5 emsbas.tab_dic_dtsul.nom_prog_appc_gat_write at 25 skip.
                if  emsbas.tab_dic_dtsul.nom_prog_upc_gat_write <> '' then
                    put stream s-arq 'UPC-WRITE : ' at 5 emsbas.tab_dic_dtsul.nom_prog_upc_gat_write  at 25 skip.
            end /* if */.
            &endif
        end.

        output stream s-arq close.*/
    end /* if */.

END PROCEDURE. /* pi_version_extract */
/*****************************************************************************
** Procedure Interna.....: pi_ler_sdo_cta_ctbl_ccusto_balanct_1
** Descricao.............: pi_ler_sdo_cta_ctbl_ccusto_balanct_1
** Criado por............: src388
** Criado em.............: 26/03/2001 11:45:26
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_ler_sdo_cta_ctbl_ccusto_balanct_1:

    /************************* Variable Definition Begin ************************/

    def var v_val_sdo_ctbl_cr                as decimal         no-undo. /*local*/
    def var v_val_sdo_ctbl_db                as decimal         no-undo. /*local*/
    def var v_val_sdo_ctbl_fim               as decimal         no-undo. /*local*/
    def var v_val_soma_cr                    as decimal         no-undo. /*local*/
    def var v_val_soma_db                    as decimal         no-undo. /*local*/
    def var v_val_soma_fim                   as decimal         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    &if '{&emsbas_version}' >= '5.05' &then

        blk_estab_unid:
        for each tt_estab_unid_negoc_select
            &IF DEFINED(BF_ADM_FIN_PROJ) &THEN 
            ,
            each tt_proj_financ_select 
               where tt_proj_financ_select.tta_cod_estab = tt_estab_unid_negoc_select.cod_estab
            &ENDIF
            :

            /* --- Sumaria ou n“o Estabelecimento ---*/
            if v_log_estab_sum = yes then
                assign v_cod_estab = "".
            else
                assign v_cod_estab = tt_estab_unid_negoc_select.cod_estab.

            /* --- Sumaria ou n“o Unid Negoc ---*/
            if v_log_unid_negoc_sum = yes then
                assign v_cod_unid_negoc = "".
            else
                assign v_cod_unid_negoc = tt_estab_unid_negoc_select.cod_unid_negoc.

            &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                /* --- Sumaria ou n“o Projeto ---*/
                if v_log_proj_financ = yes then
                    assign v_cod_proj_financ = "".
                else
                    assign v_cod_proj_financ = tt_proj_financ_select.tta_cod_proj_financ.
            &ENDIF

            find first tt_retorna_sdo_ctbl_demonst
                 where tt_retorna_sdo_ctbl_demonst.tta_cod_empresa        = v_cod_unid_organ
                   and tt_retorna_sdo_ctbl_demonst.tta_cod_finalid_econ   = v_cod_finalid_econ_bas
                   and tt_retorna_sdo_ctbl_demonst.tta_cod_cenar_ctbl     = v_cod_cenar_ctbl
                   and tt_retorna_sdo_ctbl_demonst.tta_cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
                   and tt_retorna_sdo_ctbl_demonst.tta_cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl
                   &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                   and tt_retorna_sdo_ctbl_demonst.tta_cod_proj_financ    = tt_proj_financ_select.tta_cod_proj_financ
                   &ENDIF
                   and tt_retorna_sdo_ctbl_demonst.tta_cod_estab          = tt_estab_unid_negoc_select.cod_estab
                   and tt_retorna_sdo_ctbl_demonst.tta_cod_unid_negoc     = tt_estab_unid_negoc_select.cod_unid_negoc
                   and tt_retorna_sdo_ctbl_demonst.tta_cod_plano_ccusto  <> "" /*l_null*/ 
                   and tt_retorna_sdo_ctbl_demonst.tta_cod_ccusto        <> "" /*l_null*/ 
                   and tt_retorna_sdo_ctbl_demonst.tta_dat_sdo_ctbl       = v_dat_fim_period_ctbl no-error.
            if  not avail tt_retorna_sdo_ctbl_demonst
            then do:
                if  v_log_sdo_ccusto = yes then 
                    next blk_estab_unid.
                if  v_log_cta_ctbl_sdo = yes then  /* --- Conta sem Saldos ---*/
                    assign v_cod_ccusto_aux = v_cod_ccusto.
                assign v_cod_ccusto = "".
                run pi_ler_sdo_cta_ctbl_balanct /*pi_ler_sdo_cta_ctbl_balanct*/.
            end /* if */.
            else do:
                blk_sdo_ccusto:
                for each tt_retorna_sdo_ctbl_demonst no-lock
                   where tt_retorna_sdo_ctbl_demonst.tta_cod_empresa        = v_cod_unid_organ
                     and tt_retorna_sdo_ctbl_demonst.tta_cod_finalid_econ   = v_cod_finalid_econ_bas
                     and tt_retorna_sdo_ctbl_demonst.tta_cod_cenar_ctbl     = v_cod_cenar_ctbl
                     and tt_retorna_sdo_ctbl_demonst.tta_cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
                     and tt_retorna_sdo_ctbl_demonst.tta_cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl
                     and tt_retorna_sdo_ctbl_demonst.tta_cod_estab          = tt_estab_unid_negoc_select.cod_estab
                     and tt_retorna_sdo_ctbl_demonst.tta_cod_unid_negoc     = tt_estab_unid_negoc_select.cod_unid_negoc
                     and tt_retorna_sdo_ctbl_demonst.tta_cod_plano_ccusto   = v_cod_plano_ccusto
                     &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                     and tt_retorna_sdo_ctbl_demonst.tta_cod_proj_financ    = tt_proj_financ_select.tta_cod_proj_financ
                     &ENDIF
                     and tt_retorna_sdo_ctbl_demonst.tta_dat_sdo_ctbl       = v_dat_fim_period_ctbl :

                    find first tt_ccusto
                        where tt_ccusto.cod_ccusto = tt_retorna_sdo_ctbl_demonst.tta_cod_ccusto 
                        exclusive-lock no-error.
                    if not avail tt_ccusto then 
                        next blk_sdo_ccusto.

                    /* --- Sumaria ou n“o Centro Custo ---*/
                    if  v_log_ccusto_sum = yes then
                        assign v_cod_ccusto = "".
                    else
                        assign v_cod_ccusto = tt_ccusto.cod_ccusto.                

                    find first tt_sdo_cta_ctbl_balanct
                         where tt_sdo_cta_ctbl_balanct.tta_cod_estab       = v_cod_estab
                         and   tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc  = v_cod_unid_negoc
                         and   tt_sdo_cta_ctbl_balanct.tta_cod_ccusto      = v_cod_ccusto
                         &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                         and   tt_sdo_cta_ctbl_balanct.tta_cod_proj_financ = v_cod_proj_financ
                         &ENDIF
                        exclusive-lock no-error.
                    if  not avail tt_sdo_cta_ctbl_balanct
                    then do:
                        create tt_sdo_cta_ctbl_balanct.
                        assign tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl    = tt_retorna_sdo_ctbl_demonst.tta_cod_cta_ctbl
                               tt_sdo_cta_ctbl_balanct.tta_cod_ccusto      = v_cod_ccusto
                               tt_sdo_cta_ctbl_balanct.tta_cod_estab       = v_cod_estab
                               tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc  = v_cod_unid_negoc

                               tt_sdo_cta_ctbl_balanct.tta_cod_empresa     = tt_retorna_sdo_ctbl_demonst.tta_cod_empresa
                               tt_sdo_cta_ctbl_balanct.tta_cod_plano_cta_ctbl = tt_retorna_sdo_ctbl_demonst.tta_cod_plano_cta_ctbl
                               tt_sdo_cta_ctbl_balanct.tta_cod_plano_ccusto = IF v_cod_ccusto = "" THEN "" ELSE tt_retorna_sdo_ctbl_demonst.tta_cod_plano_ccusto.





                               &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                               tt_sdo_cta_ctbl_balanct.tta_cod_proj_financ = v_cod_proj_financ
                               &ENDIF.
                    end /* if */.
                    assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                                                                       + (tt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_db / v_val_cotac_indic_econ)
                           tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                                                                       + (tt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_cr / v_val_cotac_indic_econ)
                           tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim = tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim
                                                                        + (tt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_fim / v_val_cotac_indic_econ).
                    if  v_log_consid_apurac_restdo = yes
                    or cta_ctbl.cod_cta_ctbl = emsuni.plano_cta_ctbl.cod_cta_ctbl_apurac_restdo
                    then do:
                        if  tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_db = 0 and tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_cr = 0
                        then do:
                            if tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo > 0 then
                                assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                                                                                   + (tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo / v_val_cotac_indic_econ).
                            else
                                assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                                                                                   + (tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo / v_val_cotac_indic_econ * -1).
                        end /* if */.
                        else
                            assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                                                                               + (tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_db / v_val_cotac_indic_econ)
                                   tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                                                                               + (tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_cr / v_val_cotac_indic_econ).

                        assign tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim = tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim
                                                                            + (tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_acum / v_val_cotac_indic_econ).
                    end /* if */.
                    /* --- Totais de DB e CR ---*/
                    if  cta_ctbl.ind_espec_cta_ctbl = "Anal°tica" /*l_analitica*/ 
                    then do:
                        assign v_val_tot_sdo_ctbl_db = v_val_tot_sdo_ctbl_db
                                                     + (tt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_db / v_val_cotac_indic_econ)
                               v_val_tot_sdo_ctbl_cr = v_val_tot_sdo_ctbl_cr
                                                     + (tt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_cr / v_val_cotac_indic_econ).
                        if  v_log_consid_apurac_restdo = yes
                        or cta_ctbl.cod_cta_ctbl = emsuni.plano_cta_ctbl.cod_cta_ctbl_apurac_restdo
                        then do:
                            if  tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_db = 0 and tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_cr = 0
                            then do:
                                if tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo > 0 then
                                    assign v_val_tot_sdo_ctbl_db = v_val_tot_sdo_ctbl_db
                                                                 + (tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo / v_val_cotac_indic_econ).
                                else
                                    assign v_val_tot_sdo_ctbl_cr = v_val_tot_sdo_ctbl_cr
                                                                 + (tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo / v_val_cotac_indic_econ * -1).
                            end /* if */.
                            else
                                assign v_val_tot_sdo_ctbl_db = v_val_tot_sdo_ctbl_db
                                                             + (tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_db / v_val_cotac_indic_econ)
                                       v_val_tot_sdo_ctbl_cr = v_val_tot_sdo_ctbl_cr
                                                             + (tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_cr / v_val_cotac_indic_econ).
                        end /* if */.
                    end /* if */.
                    /* --- Saldo do Per°odo Anterior ---*/
                    if  v_dat_fim_period_ctbl_ant <> ?
                    then do:
                        find first btt_retorna_sdo_ctbl_demonst no-lock
                             where btt_retorna_sdo_ctbl_demonst.tta_cod_empresa        = tt_retorna_sdo_ctbl_demonst.tta_cod_empresa
                               and btt_retorna_sdo_ctbl_demonst.tta_cod_finalid_econ   = tt_retorna_sdo_ctbl_demonst.tta_cod_finalid_econ
                               and btt_retorna_sdo_ctbl_demonst.tta_cod_cenar_ctbl     = tt_retorna_sdo_ctbl_demonst.tta_cod_cenar_ctbl
                               and btt_retorna_sdo_ctbl_demonst.tta_cod_plano_cta_ctbl = tt_retorna_sdo_ctbl_demonst.tta_cod_plano_cta_ctbl
                               and btt_retorna_sdo_ctbl_demonst.tta_cod_cta_ctbl       = tt_retorna_sdo_ctbl_demonst.tta_cod_cta_ctbl
                               and btt_retorna_sdo_ctbl_demonst.tta_cod_estab          = tt_retorna_sdo_ctbl_demonst.tta_cod_estab
                               and btt_retorna_sdo_ctbl_demonst.tta_cod_unid_negoc     = tt_retorna_sdo_ctbl_demonst.tta_cod_unid_negoc
                               and btt_retorna_sdo_ctbl_demonst.tta_cod_plano_ccusto   = tt_retorna_sdo_ctbl_demonst.tta_cod_plano_ccusto
                               and btt_retorna_sdo_ctbl_demonst.tta_cod_ccusto         = tt_retorna_sdo_ctbl_demonst.tta_cod_ccusto
                               &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                               and btt_retorna_sdo_ctbl_demonst.tta_cod_proj_financ    = tt_retorna_sdo_ctbl_demonst.tta_cod_proj_financ
                               &ENDIF
                               and btt_retorna_sdo_ctbl_demonst.tta_dat_sdo_ctbl       = v_dat_fim_period_ctbl_ant no-error.
                        if  avail btt_retorna_sdo_ctbl_demonst then do:
                            assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant
                                                                                    + (btt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_fim / v_val_cotac_indic_econ)
                                   tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant
                                                                                     + ((btt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_fim
                                                                                     -  btt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_db
                                                                                     +  btt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_cr)
                                                                                     /  v_val_cotac_indic_econ).
                            if v_log_consid_apurac_restdo = yes
                            or cta_ctbl.cod_cta_ctbl = emsuni.plano_cta_ctbl.cod_cta_ctbl_apurac_restdo then
                                assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant
                                                                                        + (btt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_acum / v_val_cotac_indic_econ)    
                                   tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant
                                                                                        + ((btt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_acum - btt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo) / v_val_cotac_indic_econ).
                        end /* if */.
                    end /* if */.
                end /* for ems5.ccusto_block */.

                /* ATIVIDADE: 36.289 - TASK: 5.646 */
                if v_log_mostra_sem_aprop_cc then do:
                    find first tt_retorna_sdo_ctbl_demonst
                         where tt_retorna_sdo_ctbl_demonst.tta_cod_empresa        = v_cod_unid_organ
                           and tt_retorna_sdo_ctbl_demonst.tta_cod_finalid_econ   = v_cod_finalid_econ_bas
                           and tt_retorna_sdo_ctbl_demonst.tta_cod_cenar_ctbl     = v_cod_cenar_ctbl
                           and tt_retorna_sdo_ctbl_demonst.tta_cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
                           and tt_retorna_sdo_ctbl_demonst.tta_cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl
                           &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                           and tt_retorna_sdo_ctbl_demonst.tta_cod_proj_financ    = tt_proj_financ_select.tta_cod_proj_financ
                           &ENDIF
                           and tt_retorna_sdo_ctbl_demonst.tta_cod_estab          = tt_estab_unid_negoc_select.cod_estab
                           and tt_retorna_sdo_ctbl_demonst.tta_cod_unid_negoc     = tt_estab_unid_negoc_select.cod_unid_negoc
                           and tt_retorna_sdo_ctbl_demonst.tta_cod_plano_ccusto   = ""
                           and tt_retorna_sdo_ctbl_demonst.tta_cod_ccusto         = ""
                           and tt_retorna_sdo_ctbl_demonst.tta_dat_sdo_ctbl       = v_dat_fim_period_ctbl no-error.
                    if  avail tt_retorna_sdo_ctbl_demonst
                    then do:
                        assign v_val_soma_cr     = 0  v_val_soma_db     = 0  v_val_soma_fim     = 0
                               v_val_sdo_ctbl_cr = 0  v_val_sdo_ctbl_db = 0  v_val_sdo_ctbl_fim = 0.
                        for each btt_retorna_sdo_ctbl_demonst
                            where btt_retorna_sdo_ctbl_demonst.tta_cod_empresa        = v_cod_unid_organ
                              and btt_retorna_sdo_ctbl_demonst.tta_cod_finalid_econ   = v_cod_finalid_econ_bas
                              and btt_retorna_sdo_ctbl_demonst.tta_cod_cenar_ctbl     = v_cod_cenar_ctbl
                              and btt_retorna_sdo_ctbl_demonst.tta_cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
                              and btt_retorna_sdo_ctbl_demonst.tta_cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl
                              &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                              and btt_retorna_sdo_ctbl_demonst.tta_cod_proj_financ    = tt_proj_financ_select.tta_cod_proj_financ
                              &ENDIF
                              and btt_retorna_sdo_ctbl_demonst.tta_cod_estab          = tt_estab_unid_negoc_select.cod_estab
                              and btt_retorna_sdo_ctbl_demonst.tta_cod_unid_negoc     = tt_estab_unid_negoc_select.cod_unid_negoc
                              and btt_retorna_sdo_ctbl_demonst.tta_cod_plano_ccusto  <> ""
                              and btt_retorna_sdo_ctbl_demonst.tta_cod_ccusto        <> ""
                              and btt_retorna_sdo_ctbl_demonst.tta_dat_sdo_ctbl       = v_dat_fim_period_ctbl:
                            assign v_val_soma_fim = v_val_soma_fim + (btt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_fim / v_val_cotac_indic_econ)
                                   v_val_soma_db  = v_val_soma_db  + (btt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_db / v_val_cotac_indic_econ)
                                   v_val_soma_cr  = v_val_soma_cr  + (btt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_cr / v_val_cotac_indic_econ).
                        end.
                        assign v_val_sdo_ctbl_cr  = v_val_sdo_ctbl_cr  + (tt_retorna_sdo_ctbl_demonst.tta_val_sdo_Ctbl_Cr  / v_val_cotac_indic_econ)
                               v_val_sdo_ctbl_db  = v_val_sdo_ctbl_db  + (tt_retorna_sdo_ctbl_demonst.tta_val_sdo_Ctbl_Db  / v_val_cotac_indic_econ)
                               v_val_sdo_ctbl_fim = v_val_sdo_ctbl_fim + (tt_retorna_sdo_ctbl_demonst.tta_val_sdo_Ctbl_Fim / v_val_cotac_indic_econ).
                        if  v_log_consid_apurac_restdo = yes
                        or cta_ctbl.cod_cta_ctbl = emsuni.plano_cta_ctbl.cod_cta_ctbl_apurac_restdo
                        then do:
                            if  tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_db = 0 and tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_cr = 0
                            then do:
                                if tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo > 0 then
                                    assign v_val_sdo_ctbl_db = v_val_sdo_ctbl_db + (tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo / v_val_cotac_indic_econ).
                                else
                                    assign v_val_sdo_ctbl_cr = v_val_sdo_ctbl_cr + (tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo / v_val_cotac_indic_econ * -1).
                            end /* if */.
                            else
                                assign v_val_sdo_ctbl_db = v_val_sdo_ctbl_db + (tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_db / v_val_cotac_indic_econ)
                                       v_val_sdo_ctbl_cr = v_val_sdo_ctbl_cr + (tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_cr / v_val_cotac_indic_econ).
                            assign v_val_sdo_ctbl_fim = v_val_sdo_ctbl_fim   + (tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_acum / v_val_cotac_indic_econ).
                        end /* if */.
                        if v_val_soma_Fim <> v_val_sdo_Ctbl_fim or
                           v_val_soma_db  <> v_val_sdo_Ctbl_db  or
                           v_val_soma_cr  <> v_val_sdo_Ctbl_cr  then do:
                                find first tt_sdo_cta_ctbl_balanct
                                    where tt_sdo_cta_ctbl_balanct.tta_cod_estab       = v_cod_estab
                                    and   tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc  = v_cod_unid_negoc
                                    and   tt_sdo_cta_ctbl_balanct.tta_cod_ccusto      = ""
                                    &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                                    and   tt_sdo_cta_ctbl_balanct.tta_cod_proj_financ = v_cod_proj_financ
                                    &ENDIF
                                    exclusive-lock no-error.
                                if  not avail tt_sdo_cta_ctbl_balanct then do:
                                    create tt_sdo_cta_ctbl_balanct.
                                    assign tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl    = tt_retorna_sdo_ctbl_demonst.tta_cod_cta_ctbl
                                           tt_sdo_cta_ctbl_balanct.tta_cod_ccusto      = ""
                                           tt_sdo_cta_ctbl_balanct.tta_cod_estab       = v_cod_estab
                                           tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc  = v_cod_unid_negoc
                                           tt_sdo_cta_ctbl_balanct.tta_cod_empresa        = v_cod_unid_organ     
                                           tt_sdo_cta_ctbl_balanct.tta_cod_plano_cta_ctbl = v_cod_plano_cta_ctbl 
                                           tt_sdo_cta_ctbl_balanct.tta_cod_plano_ccusto = "".

                                           &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                                           tt_sdo_cta_ctbl_balanct.tta_cod_proj_financ = v_cod_proj_financ
                                           &ENDIF.
                                end.
                                if (v_val_soma_db - v_val_sdo_ctbl_db) > 0 then
                                    assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                                                                                   + (v_val_soma_db - v_val_sdo_Ctbl_Db).
                                else
                                    assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                                                                                   + (v_val_sdo_Ctbl_Db - v_val_soma_db).
                                if (v_val_soma_cr - v_val_sdo_ctbl_cr) > 0 then
                                 assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                                                                                   + (v_val_soma_cr - v_val_sdo_Ctbl_cr).
                                else
                                 assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                                                                                   + (v_val_sdo_Ctbl_cr - v_val_soma_cr).
                                if (v_val_soma_fim - v_val_sdo_ctbl_fim) > 0 then
                                    assign tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim = tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim
                                                                                    + (v_val_soma_fim - v_val_sdo_ctbl_fim).
                                else
                                    assign tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim = tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim
                                                                                    + (v_val_sdo_ctbl_fim - v_val_soma_fim).
                                /* --- Totais de DB e CR ---*/
                                if cta_ctbl.ind_espec_cta_ctbl = "Anal°tica" /*l_analitica*/  then
                                    assign v_val_tot_sdo_ctbl_db = v_val_tot_sdo_ctbl_db
                                                                 + (tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 / v_val_cotac_indic_econ)
                                           v_val_tot_sdo_ctbl_cr = v_val_tot_sdo_ctbl_cr
                                                                 + (tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 / v_val_cotac_indic_econ).
                        end.
                    end.
                end.
                /* FIM: ATIVIDADE: 36.289*/
            end /* if */.
        end.
    &endif
END PROCEDURE. /* pi_ler_sdo_cta_ctbl_ccusto_balanct_1 */
/*****************************************************************************
** Procedure Interna.....: pi_cria_tt_proj_financ_select
** Descricao.............: pi_cria_tt_proj_financ_select
** Criado por............: src388
** Criado em.............: 27/03/2001 10:51:56
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_cria_tt_proj_financ_select:

    &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
    /* --- Caso n∆o exista Regra contendo Projeto ---*/

    /* Begin_Include: i_cria_dwb_rpt_select */
    find first b_dwb_rpt_select
       where b_dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
       and   b_dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
       and   b_dwb_rpt_select.cod_dwb_field   = "Projeto"
       and   b_dwb_rpt_select.log_dwb_rule    = yes no-lock no-error.
    if  not avail b_dwb_rpt_select
    then do:
        criar_dwb_rpt_select:
        do transaction:
            find last dwb_rpt_select no-lock
                 where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
                   and dwb_rpt_select.cod_dwb_user = v_cod_dwb_user
                   and dwb_rpt_select.log_dwb_rule = yes /*cl_dwb_rpt_select_rule of dwb_rpt_select*/ no-error.
            if (avail dwb_rpt_select) then 
                 assign v_num_dwb_order =  dwb_rpt_select.num_dwb_order + 10.
            else assign v_num_dwb_order = 10.

            create b_dwb_rpt_select.
            assign b_dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
                   b_dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
                   b_dwb_rpt_select.cod_dwb_field   = "Projeto"
                   b_dwb_rpt_select.num_dwb_order   = v_num_dwb_order
                   b_dwb_rpt_select.log_dwb_rule    = yes.

            find first proj_financ no-lock no-error.
            if (avail proj_financ) then
                assign b_dwb_rpt_select.cod_dwb_initial = proj_financ.cod_proj_financ.
            find last proj_financ no-lock no-error.
            if (avail proj_financ) then
                assign b_dwb_rpt_select.cod_dwb_final = proj_financ.cod_proj_financ.
        end /* do criar_dwb_rpt_select */.
    end /* if */.
    /* End_Include: i_cria_dwb_rpt_select */


    /* --- Criaá∆o tt_proj_financ_select ---*/
    for each b_dwb_rpt_select
        where b_dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
          and b_dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
          and b_dwb_rpt_select.cod_dwb_field   = "Estabelecimento"
          and b_dwb_rpt_select.log_dwb_rule    = yes no-lock:

        estab_block:
        for each estabelecimento no-lock
            where estabelecimento.cod_empresa = v_cod_unid_organ
              and estabelecimento.cod_estab  >= b_dwb_rpt_select.cod_dwb_initial
              and estabelecimento.cod_estab  <= b_dwb_rpt_select.cod_dwb_final:

            for each dwb_rpt_select
                where dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
                  and dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
                  and dwb_rpt_select.cod_dwb_field   = "Projeto"
                  and dwb_rpt_select.log_dwb_rule    = yes exclusive-lock:

                for each  proj_financ_estab no-lock
                    where proj_financ_estab.cod_estab       = estabelecimento.cod_estab
                      and proj_financ_estab.cod_proj_financ >= dwb_rpt_select.cod_dwb_initial
                      and proj_financ_estab.cod_proj_financ <= dwb_rpt_select.cod_dwb_final:

                    if  not can-find(first tt_estab_unid_negoc_select
                                 where tt_estab_unid_negoc_select.cod_estab = estabelecimento.cod_estab) then
                        next.

                    find tt_proj_financ_select no-lock
                        where tt_proj_financ_select.tta_cod_estab       = proj_financ_estab.cod_estab
                        and   tt_proj_financ_select.tta_cod_proj_financ = proj_financ_estab.cod_proj_financ no-error.
                    if  not avail tt_proj_financ_select then do:
                        create tt_proj_financ_select.
                        assign tt_proj_financ_select.tta_cod_estab       = proj_financ_estab.cod_estab
                               tt_proj_financ_select.tta_cod_proj_financ = proj_financ_estab.cod_proj_financ.
                    end.
                end.
            end.
        end.
    end.
    &ENDIF
END PROCEDURE. /* pi_cria_tt_proj_financ_select */
/*****************************************************************************
** Procedure Interna.....: pi_carrega_sdo_cta_sintetica
** Descricao.............: pi_carrega_sdo_cta_sintetica
** Criado por............: src388
** Criado em.............: 24/04/2001 08:39:00
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_carrega_sdo_cta_sintetica:

    &if '{&emsfin_version}' >= '5.05' &then
    /* ** RETORNA CONTAS ANAL⁄TICAS QUE FAZEM PARTE DA SINT‘TICA INFORMADA ***/
    cta_ctbl_block:
    for each tt_cta_ctbl_505:
        find first cta_ctbl no-lock
             where recid(cta_ctbl) = tt_cta_ctbl_505.ttv_rec_cta_ctbl no-error.
        if cta_ctbl.ind_espec_cta_ctbl = "Anal°tica" /*l_analitica*/  then
            next cta_ctbl_block.
        run pi_localiza_cta_ctbl_analitica_505 (Input cta_ctbl.cod_cta_ctbl) /*pi_localiza_cta_ctbl_analitica_505*/.
    end /* for cta_ctbl_block */.

    /* ** Carrega saldos para as contas SintÇticas ***/
    cta_ctbl_block:
    for each tt_cta_ctbl_aux by tt_cta_ctbl_aux.ttv_num_seq descending:
        if can-find(first cta_ctbl where cta_ctbl.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
                                     and cta_ctbl.cod_cta_ctbl = tt_cta_ctbl_aux.tta_cod_cta_ctbl
                                     and cta_ctbl.ind_espec_cta_ctbl = "Anal°tica" /*l_analitica*/ )
        then next cta_ctbl_block.

        for each  estrut_cta_ctbl no-lock
            where estrut_cta_ctbl.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
            and   estrut_cta_ctbl.cod_cta_ctbl_pai = tt_cta_ctbl_aux.tta_cod_cta_ctbl,
            each tt_retorna_sdo_ctbl_demonst
                 where tt_retorna_sdo_ctbl_demonst.tta_cod_plano_cta_ctbl = estrut_cta_ctbl.cod_plano_cta_ctbl
                   and tt_retorna_sdo_ctbl_demonst.tta_cod_cta_ctbl = estrut_cta_ctbl.cod_cta_ctbl_filho:

            find first btt_retorna_sdo_ctbl_demonst
                 where btt_retorna_sdo_ctbl_demonst.tta_cod_empresa        = tt_retorna_sdo_ctbl_demonst.tta_cod_empresa 
                   and btt_retorna_sdo_ctbl_demonst.tta_cod_finalid_econ   = tt_retorna_sdo_ctbl_demonst.tta_cod_finalid_econ 
                   and btt_retorna_sdo_ctbl_demonst.tta_cod_plano_cta_ctbl = tt_retorna_sdo_ctbl_demonst.tta_cod_plano_cta_ctbl 
                   and btt_retorna_sdo_ctbl_demonst.tta_cod_cta_ctbl       = tt_cta_ctbl_aux.tta_cod_cta_ctbl 
                   and btt_retorna_sdo_ctbl_demonst.tta_cod_plano_ccusto   = tt_retorna_sdo_ctbl_demonst.tta_cod_plano_ccusto 
                   and btt_retorna_sdo_ctbl_demonst.tta_cod_ccusto         = tt_retorna_sdo_ctbl_demonst.tta_cod_ccusto 
                   &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                   and btt_retorna_sdo_ctbl_demonst.tta_cod_proj_financ    = tt_retorna_sdo_ctbl_demonst.tta_cod_proj_financ 
                   &ENDIF
                   and btt_retorna_sdo_ctbl_demonst.tta_cod_cenar_ctbl     = tt_retorna_sdo_ctbl_demonst.tta_cod_cenar_ctbl 
                   and btt_retorna_sdo_ctbl_demonst.tta_cod_estab          = tt_retorna_sdo_ctbl_demonst.tta_cod_estab 
                   and btt_retorna_sdo_ctbl_demonst.tta_cod_unid_negoc     = tt_retorna_sdo_ctbl_demonst.tta_cod_unid_negoc 
                   and btt_retorna_sdo_ctbl_demonst.tta_dat_sdo_ctbl       = tt_retorna_sdo_ctbl_demonst.tta_dat_sdo_ctbl
                   and btt_retorna_sdo_ctbl_demonst.ttv_ind_espec_sdo      = tt_retorna_sdo_ctbl_demonst.ttv_ind_espec_sdo

                 use-index tt_id no-error.

            if  not avail btt_retorna_sdo_ctbl_demonst
            then do:

                create btt_retorna_sdo_ctbl_demonst. 
                assign btt_retorna_sdo_ctbl_demonst.tta_num_seq            = tt_retorna_sdo_ctbl_demonst.tta_num_seq /* RAFA v_num_seq + 1*/
                       btt_retorna_sdo_ctbl_demonst.tta_cod_empresa        = tt_retorna_sdo_ctbl_demonst.tta_cod_empresa
                       btt_retorna_sdo_ctbl_demonst.tta_cod_finalid_econ   = tt_retorna_sdo_ctbl_demonst.tta_cod_finalid_econ
                       btt_retorna_sdo_ctbl_demonst.tta_cod_plano_cta_ctbl = tt_retorna_sdo_ctbl_demonst.tta_cod_plano_cta_ctbl
                       btt_retorna_sdo_ctbl_demonst.tta_cod_cta_ctbl       = tt_cta_ctbl_aux.tta_cod_cta_ctbl
                       btt_retorna_sdo_ctbl_demonst.tta_cod_plano_ccusto   = tt_retorna_sdo_ctbl_demonst.tta_cod_plano_ccusto
                       btt_retorna_sdo_ctbl_demonst.tta_cod_ccusto         = tt_retorna_sdo_ctbl_demonst.tta_cod_ccusto
                       btt_retorna_sdo_ctbl_demonst.tta_cod_cenar_ctbl     = tt_retorna_sdo_ctbl_demonst.tta_cod_cenar_ctbl
                       btt_retorna_sdo_ctbl_demonst.tta_cod_estab          = tt_retorna_sdo_ctbl_demonst.tta_cod_estab
                       btt_retorna_sdo_ctbl_demonst.tta_cod_unid_negoc     = tt_retorna_sdo_ctbl_demonst.tta_cod_unid_negoc
                       &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                       btt_retorna_sdo_ctbl_demonst.tta_cod_proj_financ    = tt_retorna_sdo_ctbl_demonst.tta_cod_proj_financ
                       &ENDIF
                       btt_retorna_sdo_ctbl_demonst.tta_dat_sdo_ctbl       = tt_retorna_sdo_ctbl_demonst.tta_dat_sdo_ctbl
                       btt_retorna_sdo_ctbl_demonst.ttv_ind_espec_sdo      = tt_retorna_sdo_ctbl_demonst.ttv_ind_espec_sdo.
            end.

            assign btt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_db        =
                   btt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_db        + tt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_db
                   btt_retorna_sdo_ctbl_demonst.tta_val_orcado             =
                   btt_retorna_sdo_ctbl_demonst.tta_val_orcado             + tt_retorna_sdo_ctbl_demonst.tta_val_orcado
                   btt_retorna_sdo_ctbl_demonst.tta_val_orcado_sdo         =
                   btt_retorna_sdo_ctbl_demonst.tta_val_orcado_sdo         + tt_retorna_sdo_ctbl_demonst.tta_val_orcado_sdo
                   btt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_cr        =
                   btt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_cr        + tt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_cr
                   btt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_fim       =
                   btt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_fim       + tt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_fim
                   btt_retorna_sdo_ctbl_demonst.tta_qtd_sdo_ctbl_db        =
                   btt_retorna_sdo_ctbl_demonst.tta_qtd_sdo_ctbl_db        + tt_retorna_sdo_ctbl_demonst.tta_qtd_sdo_ctbl_db
                   btt_retorna_sdo_ctbl_demonst.tta_qtd_sdo_ctbl_cr        =
                   btt_retorna_sdo_ctbl_demonst.tta_qtd_sdo_ctbl_cr        + tt_retorna_sdo_ctbl_demonst.tta_qtd_sdo_ctbl_cr
                   btt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo      =
                   btt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo      + tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo
                   btt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_cr   =
                   btt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_cr   + tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_cr
                   btt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_db   =
                   btt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_db   + tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_db
                   btt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_acum =
                   btt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_acum + tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_acum
                   btt_retorna_sdo_ctbl_demonst.tta_val_movto_empenh       =
                   btt_retorna_sdo_ctbl_demonst.tta_val_movto_empenh       + tt_retorna_sdo_ctbl_demonst.tta_val_movto_empenh
                   btt_retorna_sdo_ctbl_demonst.tta_qtd_movto_empenh       =
                   btt_retorna_sdo_ctbl_demonst.tta_qtd_movto_empenh       + tt_retorna_sdo_ctbl_demonst.tta_qtd_movto_empenh.
        end.
    end /* for cta_ctbl_block */.
    &endif
END PROCEDURE. /* pi_carrega_sdo_cta_sintetica */
/*****************************************************************************
** Procedure Interna.....: pi_ler_saldo_ctbl
** Descricao.............: pi_ler_saldo_ctbl
** Criado por............: src388
** Criado em.............: 24/04/2001 14:19:28
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_ler_saldo_ctbl:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_cta_ctbl_ini
        as character
        format "x(20)"
        no-undo.
    def Input param p_cod_cta_ctbl_fim
        as character
        format "x(20)"
        no-undo.


    /************************* Parameter Definition End *************************/

    &if '{&emsfin_version}' >= '5.05' &then

        run pi_cria_tt_input_leitura_sdo_faixa (Input v_cod_unid_organ,
                                                Input v_cod_estab_ini,
                                                Input v_cod_estab_fim,
                                                Input v_cod_finalid_econ_bas,
                                                Input v_cod_plano_cta_ctbl,
                                                Input p_cod_cta_ctbl_ini,
                                                Input p_cod_cta_ctbl_fim,
                                                Input "",
                                                Input "",
                                                Input "ZZZZZZZZZZZ" /*l_ZZZZZZZZZZZ*/,
                                                Input v_cod_unid_negoc_ini,
                                                Input v_cod_unid_negoc_fim,
                                                Input v_cod_cenar_ctbl,
                                                Input v_cod_proj_financ_ini,
                                                Input v_cod_proj_financ_fim,
                                                Input v_dat_fim_period_ctbl,
                                                Input v_dat_fim_period_ctbl,
                                                Input no,
                                                Input "Igual" /*l_igual*/,
                                                Input "for each" /*l_for_each*/) /*pi_cria_tt_input_leitura_sdo_faixa*/.

        /* --- Saldo do Per°odo Anterior ---*/
        if  v_dat_fim_period_ctbl_ant <> ?
        then do:
            run pi_cria_tt_input_leitura_sdo_faixa (Input v_cod_unid_organ,
                                                    Input v_cod_estab_ini,
                                                    Input v_cod_estab_fim,
                                                    Input v_cod_finalid_econ_bas,
                                                    Input v_cod_plano_cta_ctbl,
                                                    Input p_cod_cta_ctbl_ini,
                                                    Input p_cod_cta_ctbl_fim,
                                                    Input "",
                                                    Input "",
                                                    Input "ZZZZZZZZZZZ" /*l_ZZZZZZZZZZZ*/,
                                                    Input v_cod_unid_negoc_ini,
                                                    Input v_cod_unid_negoc_fim,
                                                    Input v_cod_cenar_ctbl,
                                                    Input v_cod_proj_financ_ini,
                                                    Input v_cod_proj_financ_fim,
                                                    Input v_dat_fim_period_ctbl_ant,
                                                    Input v_dat_fim_period_ctbl_ant,
                                                    Input no,
                                                    Input "Igual" /*l_igual*/,
                                                    Input "for each" /*l_for_each*/) /*pi_cria_tt_input_leitura_sdo_faixa*/.
        end.

    &endif
END PROCEDURE. /* pi_ler_saldo_ctbl */
/*****************************************************************************
** Procedure Interna.....: pi_localiza_cta_ctbl_analitica_505
** Descricao.............: pi_localiza_cta_ctbl_analitica_505
** Criado por............: src388
** Criado em.............: 25/04/2001 17:41:05
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_localiza_cta_ctbl_analitica_505:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_cta_ctbl
        as character
        format "x(20)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /* --- Verifica se a conta Ç Filha na Estrutura ---*/ 
    if  not can-find(first estrut_cta_ctbl no-lock 
        where estrut_cta_ctbl.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
        and   estrut_cta_ctbl.cod_cta_ctbl_pai = p_cod_cta_ctbl)
        and not can-find(first tt_cta_ctbl_aux where
        tt_cta_ctbl_aux.tta_cod_cta_ctbl = p_cod_cta_ctbl)
    then do:
        assign v_num_seq_aux = v_num_seq_aux + 1.
        Create tt_cta_ctbl_aux.
        Assign tt_cta_ctbl_aux.tta_cod_cta_ctbl = p_cod_cta_ctbl
               tt_cta_ctbl_aux.ttv_rec_cta_ctbl = recid(tt_cta_ctbl_aux)
               tt_cta_ctbl_aux.ttv_num_seq = v_num_seq_aux.
    end /* if */.
    else do:
        for each  estrut_cta_ctbl no-lock
            where estrut_cta_ctbl.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
            and   estrut_cta_ctbl.cod_cta_ctbl_pai = p_cod_cta_ctbl:
            if  not can-find(first tt_cta_ctbl_aux where 
                tt_cta_ctbl_aux.tta_cod_cta_ctbl = p_cod_cta_ctbl)
            then do:
                assign v_num_seq_aux = v_num_seq_aux + 1.
                Create tt_cta_ctbl_aux.
                Assign tt_cta_ctbl_aux.tta_cod_cta_ctbl = p_cod_cta_ctbl
                       tt_cta_ctbl_aux.ttv_rec_cta_ctbl = recid(tt_cta_ctbl_aux)
                       tt_cta_ctbl_aux.ttv_num_seq = v_num_seq_aux.
            end /* if */.
            run pi_localiza_cta_ctbl_analitica_505 (Input estrut_cta_ctbl.cod_cta_ctbl_filho) /*pi_localiza_cta_ctbl_analitica_505*/.
        end.
    end /* else */.
END PROCEDURE. /* pi_localiza_cta_ctbl_analitica_505 */
/*****************************************************************************
** Procedure Interna.....: pi_create_tt_sdo_cta_ctbl_balanct
** Descricao.............: pi_create_tt_sdo_cta_ctbl_balanct
** Criado por............: src12337
** Criado em.............: 25/05/2001 16:21:23
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_create_tt_sdo_cta_ctbl_balanct:

    /************************ Parameter Definition Begin ************************/

    def Input param p_val_sdo_db
        as decimal
        format "->>,>>>,>>>,>>9.99"
        decimals 2
        no-undo.
    def Input param p_val_sdo_cr
        as decimal
        format "->>,>>>,>>>,>>9.99"
        decimals 2
        no-undo.
    def Input param p_val_sdo_fim
        as decimal
        format ">>>,>>>,>>9.99"
        decimals 2
        no-undo.
    def Input param p_val_sdo_fim_2
        as decimal
        format "->>,>>>,>>>,>>9.99"
        decimals 2
        no-undo.
    def Input param p_val_sdo_fim_ant
        as decimal
        format "->>,>>>,>>>,>>9.99"
        decimals 2
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************** Buffer Definition Begin *************************/

    def buffer btt_sdo_cta_ctbl_balanct
        for tt_sdo_cta_ctbl_balanct.


    /*************************** Buffer Definition End **************************/

    find first btt_sdo_cta_ctbl_balanct 
        where btt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl   = tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl
        and   btt_sdo_cta_ctbl_balanct.tta_cod_estab      = tt_sdo_cta_ctbl_balanct.tta_cod_estab 
        and   btt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc = tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc 
        and   btt_sdo_cta_ctbl_balanct.tta_cod_ccusto     = "" 
        exclusive-lock no-error. 
    if  not avail btt_sdo_cta_ctbl_balanct then do: 
        create btt_sdo_cta_ctbl_balanct. 
        assign btt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl          = tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl 
               btt_sdo_cta_ctbl_balanct.tta_cod_ccusto            = "" 
               btt_sdo_cta_ctbl_balanct.tta_cod_estab             = tt_sdo_cta_ctbl_balanct.tta_cod_estab 
               btt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc        = tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc 
               &IF DEFINED(BF_ADM_FIN_PROJ) &THEN 
               btt_sdo_cta_ctbl_balanct.tta_cod_proj_financ       = tt_sdo_cta_ctbl_balanct.tta_cod_proj_financ 
               &ENDIF 
               btt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_db       = p_val_sdo_db 
               btt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_cr       = p_val_sdo_cr 
               btt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim      = p_val_sdo_fim 
               btt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant = p_val_sdo_fim_2 
               btt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant  = p_val_sdo_fim_ant. 
    end.
    else do:
        assign btt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_db       = btt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_db + p_val_sdo_db 
               btt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_cr       = btt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_cr + p_val_sdo_cr 
               btt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim      = btt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim + p_val_sdo_fim 
               btt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant = btt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant + p_val_sdo_fim_2 
               btt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant  = btt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant + p_val_sdo_fim_ant.                     
    end.
    find cta_ctbl
        where cta_ctbl.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
        and   cta_ctbl.cod_cta_ctbl       = tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl
        no-lock no-error.
    if  cta_ctbl.ind_espec_cta_ctbl = "Anal°tica" /*l_analitica*/  then do:
        assign v_val_tot_sdo_ctbl_db = v_val_tot_sdo_ctbl_db
                                     + btt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_db
               v_val_tot_sdo_ctbl_cr = v_val_tot_sdo_ctbl_cr
                                     + btt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_cr.
        /* no caso de considere apuraá∆o de resultado, o valor de apuracao j† foi adicionado
           na criacao da temp-table contendo o saldo */
    end.

END PROCEDURE. /* pi_create_tt_sdo_cta_ctbl_balanct */
/*****************************************************************************
** Procedure Interna.....: pi_ler_sdo_cta_ctbl_balanct_1
** Descricao.............: pi_ler_sdo_cta_ctbl_balanct_1
** Criado por............: src12337
** Criado em.............: 10/05/2002 13:38:57
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_ler_sdo_cta_ctbl_balanct_1:

    /************************** Buffer Definition Begin *************************/

    def buffer btt_retorna_sdo_ctbl_demonst
        for tt_retorna_sdo_ctbl_demonst.
    def buffer b_sdo_cta_ctbl_1
        for sdo_cta_ctbl.


    /*************************** Buffer Definition End **************************/

    /************************* Variable Definition Begin ************************/

    def var v_log_aux
        as logical
        format "Sim/N∆o"
        initial yes
        no-undo.


    /************************** Variable Definition End *************************/

    assign v_val_sdo_ctbl_db_505        = 0
           v_val_sdo_ctbl_cr_505        = 0
           v_val_sdo_ctbl_fim_505       = 0
           v_val_apurac_restdo_db_505   = 0
           v_val_apurac_restdo_cr_505   = 0
           v_val_apurac_restdo_505      = 0
           v_val_apurac_restdo_acum_505 = 0.

    &if '{&emsbas_version}' >= '5.05' &then
        blk_tt_retorna:
        for each tt_retorna_sdo_ctbl_demonst
            where tt_retorna_sdo_ctbl_demonst.tta_cod_empresa        = v_cod_unid_organ
            and   tt_retorna_sdo_ctbl_demonst.tta_cod_finalid_econ   = v_cod_finalid_econ_bas
            and   tt_retorna_sdo_ctbl_demonst.tta_cod_cenar_ctbl     = v_cod_cenar_ctbl
            and   tt_retorna_sdo_ctbl_demonst.tta_cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
            and   tt_retorna_sdo_ctbl_demonst.tta_cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl
            and   tt_retorna_sdo_ctbl_demonst.tta_dat_sdo_ctbl       = v_dat_fim_period_ctbl
            and   tt_retorna_sdo_ctbl_demonst.tta_cod_plano_ccusto   = ''
            use-index tt_id:

            if  v_ind_tip_plano_cta_ctbl <> "Consolidaá∆o" /*l_consolidacao*/  then do:
                find first tt_estab_unid_negoc_select
                    where tt_estab_unid_negoc_select.cod_estab = tt_retorna_sdo_ctbl_demonst.tta_cod_estab
                    and   tt_estab_unid_negoc_select.cod_unid_negoc = tt_retorna_sdo_ctbl_demonst.tta_cod_unid_negoc no-error.
                if not avail tt_estab_unid_negoc_select then next blk_tt_retorna.

                &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                    find first tt_proj_financ_select
                         where tt_proj_financ_select.tta_cod_proj_financ = tt_retorna_sdo_ctbl_demonst.tta_cod_proj_financ
                         and   tt_proj_financ_select.tta_cod_estab       = tt_retorna_sdo_ctbl_demonst.tta_cod_estab no-lock no-error.
                    if not avail tt_proj_financ_select then 
                        next blk_tt_retorna.
                &ENDIF

                /* --- Sumaria ou n∆o Estabelecimento ---*/
                if v_log_estab_sum = yes then
                    assign v_cod_estab = "".
                else
                    assign v_cod_estab = tt_estab_unid_negoc_select.cod_estab.

                /* --- Sumaria ou n∆o Unid Negoc ---*/
                if v_log_unid_negoc_sum = yes then
                    assign v_cod_unid_negoc = "".
                else
                    assign v_cod_unid_negoc = tt_estab_unid_negoc_select.cod_unid_negoc.

                &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                    /* --- Sumaria ou n∆o Projeto ---*/
                    if v_log_proj_financ = yes then
                        assign v_cod_proj_financ = "".
                    else
                        assign v_cod_proj_financ = tt_proj_financ_select.tta_cod_proj_financ.
                &ENDIF
            end.
            else do:
                assign v_log_aux = no.
                for each b_dwb_rpt_select
                   where b_dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
                   and   b_dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
                   and   b_dwb_rpt_select.cod_dwb_field   = "Unid Neg¢cio" /*l_Unid_Neg¢cio*/ 
                   and   b_dwb_rpt_select.log_dwb_rule    = yes no-lock:

                    if  tt_retorna_sdo_ctbl_demonst.tta_cod_unid_negoc >= b_dwb_rpt_select.cod_dwb_initial
                    and tt_retorna_sdo_ctbl_demonst.tta_cod_unid_negoc <= b_dwb_rpt_select.cod_dwb_final then do:
                       assign v_log_aux = yes.
                       /* --- Sumaria ou n∆oo Unid Negoc ---*/
                       if  v_log_unid_negoc_sum = yes
                       then do:
                           assign v_cod_unid_negoc = "".
                       end /* if */.
                       else do:
                           assign v_cod_unid_negoc = tt_retorna_sdo_ctbl_demonst.tta_cod_unid_negoc.
                       end /* else */.
                    end.
                end.
                if v_log_aux = no then
                    next blk_tt_retorna.
            end.


            assign v_val_sdo_ctbl_db_505        = tt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_db
                   v_val_sdo_ctbl_cr_505        = tt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_cr
                   v_val_sdo_ctbl_fim_505       = tt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_fim
                   v_val_apurac_restdo_db_505   = tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_db
                   v_val_apurac_restdo_cr_505   = tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_cr
                   v_val_apurac_restdo_505      = tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo
                   v_val_apurac_restdo_acum_505 = tt_retorna_sdo_ctbl_demonst.tta_val_apurac_restdo_acum.

            /* Begin_Include: i_ler_sdo_cta_ctbl_balanct */
            find first tt_sdo_cta_ctbl_balanct
                where tt_sdo_cta_ctbl_balanct.tta_cod_estab      = v_cod_estab
                and   tt_sdo_cta_ctbl_balanct.tta_cod_ccusto     = v_cod_ccusto
                and   tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc = v_cod_unid_negoc
                &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                and   tt_sdo_cta_ctbl_balanct.tta_cod_proj_financ = v_cod_proj_financ
                &ENDIF
                exclusive-lock no-error.
            if  not avail tt_sdo_cta_ctbl_balanct
            then do:
                create tt_sdo_cta_ctbl_balanct.
                assign tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl   = cta_ctbl.cod_cta_ctbl
                       tt_sdo_cta_ctbl_balanct.tta_cod_ccusto     = ""
                       tt_sdo_cta_ctbl_balanct.tta_cod_estab      = v_cod_estab
                       tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc = v_cod_unid_negoc
                       tt_sdo_cta_ctbl_balanct.tta_cod_empresa        = v_cod_unid_organ     
                       tt_sdo_cta_ctbl_balanct.tta_cod_plano_cta_ctbl = v_cod_plano_cta_ctbl 
                       tt_sdo_cta_ctbl_balanct.tta_cod_plano_ccusto = "".

                &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                    assign tt_sdo_cta_ctbl_balanct.tta_cod_proj_financ = v_cod_proj_financ.
                &ENDIF
            end /* if */.
            assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1  = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                                                                + (v_val_sdo_ctbl_db_505 / v_val_cotac_indic_econ)
                   tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1  = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                                                                + (v_val_sdo_ctbl_cr_505 / v_val_cotac_indic_econ)
                   tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim = tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim
                                                                + (v_val_sdo_ctbl_fim_505 / v_val_cotac_indic_econ).
            /* --- Caso considere apuraá∆o de resultado ---*/
            if  v_log_consid_apurac_restdo = yes
            then do:
                if  v_val_apurac_restdo_db_505 = 0 and v_val_apurac_restdo_cr_505 = 0
                then do:
                    if  v_val_apurac_restdo_505 > 0
                    then do:
                        assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                                                                           + (v_val_apurac_restdo_505 / v_val_cotac_indic_econ).
                    end /* if */.
                    else do:
                        assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                                                                           + (v_val_apurac_restdo_505 / v_val_cotac_indic_econ * -1).
                    end /* else */.
                end /* if */.
                else do:
                    assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                                                                       + (v_val_apurac_restdo_db_505 / v_val_cotac_indic_econ)
                           tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                                                                       + (v_val_apurac_restdo_cr_505 / v_val_cotac_indic_econ).
                end /* else */.
                assign tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim = tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim
                                                                    + (v_val_apurac_restdo_acum_505 / v_val_cotac_indic_econ).
            end /* if */.
            else do:
                FIND GRP_CTA_CTBL OF cta_ctbl NO-LOCK.
                if cta_ctbl.cod_cta_ctbl = plano_cta_Ctbl.cod_cta_ctbl_apurac_restdo OR GRP_cTA_cTBL.LOG_CONSID_APURAC = NO then do:
                    assign tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim = tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim 
                                                                        + (v_val_apurac_restdo_acum_505 / v_val_cotac_indic_econ)
                                                                        - (v_val_apurac_restdo_505  / v_val_cotac_indic_econ).
                end.
            end.
            /* --- Totais de DB e CR ---*/
            if  cta_ctbl.ind_espec_cta_ctbl = "Anal°tica" /*l_analitica*/ 
            then do:
                assign v_val_tot_sdo_ctbl_db = v_val_tot_sdo_ctbl_db + (v_val_sdo_ctbl_db_505 / v_val_cotac_indic_econ)
                       v_val_tot_sdo_ctbl_cr = v_val_tot_sdo_ctbl_cr + (v_val_sdo_ctbl_cr_505 / v_val_cotac_indic_econ).
                /* --- Caso considere apuraá∆o de resultado ---*/
                if  v_log_consid_apurac_restdo = yes
                then do:
                    if  v_val_apurac_restdo_db_505 = 0 and v_val_apurac_restdo_cr_505 = 0
                    then do:
                        if  v_val_apurac_restdo_505 > 0
                        then do:
                            assign v_val_tot_sdo_ctbl_db = v_val_tot_sdo_ctbl_db + (v_val_apurac_restdo_505 / v_val_cotac_indic_econ).
                        end /* if */.
                        else do:
                            assign v_val_tot_sdo_ctbl_cr = v_val_tot_sdo_ctbl_cr + (v_val_apurac_restdo_505 / v_val_cotac_indic_econ * -1).
                        end /* else */.
                    end /* if */.
                    else do:
                        assign v_val_tot_sdo_ctbl_db = v_val_tot_sdo_ctbl_db + (v_val_apurac_restdo_db_505 / v_val_cotac_indic_econ)
                               v_val_tot_sdo_ctbl_cr = v_val_tot_sdo_ctbl_cr + (v_val_apurac_restdo_cr_505 / v_val_cotac_indic_econ).
                    end /* else */.
                end /* if */.
            end /* if */.
            /* --- Saldo do Per°odo Anterior ---*/
            if  v_dat_fim_period_ctbl_ant <> ?
            then do:
                /* FUT1082 - 09/07/2002
                N∆o estava zerando todas as vari†veis. */

                /* assign v_val_sdo_ctbl_db_505      = 0
                  v_val_sdo_ctbl_cr_505      = 0
                  v_val_sdo_ctbl_fim_505     = 0.*/

                 assign v_val_sdo_ctbl_db_505        = 0
                        v_val_sdo_ctbl_cr_505        = 0
                        v_val_sdo_ctbl_fim_505       = 0
                        v_val_apurac_restdo_db_505   = 0
                        v_val_apurac_restdo_cr_505   = 0
                        v_val_apurac_restdo_505      = 0
                        v_val_apurac_restdo_acum_505 = 0.


                &if '{&emsbas_version}' >= '5.05' &then
                    for each btt_retorna_sdo_ctbl_demonst
                        where btt_retorna_sdo_ctbl_demonst.tta_cod_empresa        = v_cod_unid_organ
                        and   btt_retorna_sdo_ctbl_demonst.tta_cod_finalid_econ   = v_cod_finalid_econ_bas
                        and   btt_retorna_sdo_ctbl_demonst.tta_cod_cenar_ctbl     = v_cod_cenar_ctbl
                        and   btt_retorna_sdo_ctbl_demonst.tta_cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
                        and   btt_retorna_sdo_ctbl_demonst.tta_cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl
                        and   btt_retorna_sdo_ctbl_demonst.tta_cod_estab          = tt_estab_unid_negoc_select.cod_estab
                        and   btt_retorna_sdo_ctbl_demonst.tta_cod_unid_negoc     = tt_estab_unid_negoc_select.cod_unid_negoc
                        and   btt_retorna_sdo_ctbl_demonst.tta_dat_sdo_ctbl       = v_dat_fim_period_ctbl_ant
                        and   btt_retorna_sdo_ctbl_demonst.tta_cod_plano_ccusto   = ''
                        &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                            and   btt_retorna_sdo_ctbl_demonst.tta_cod_proj_financ    = tt_proj_financ_select.tta_cod_proj_financ
                        &ENDIF
                        use-index tt_id:
                        assign v_val_sdo_ctbl_db_505      = v_val_sdo_ctbl_db_505  + btt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_db
                               v_val_sdo_ctbl_cr_505      = v_val_sdo_ctbl_cr_505  + btt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_cr
                               v_val_sdo_ctbl_fim_505     = v_val_sdo_ctbl_fim_505 + btt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_fim.
                    end.
                    /* ** VERIFICA SE CONTAS TEM SALDO ***/
                    if v_val_sdo_ctbl_db_505  <> 0
                    or v_val_sdo_ctbl_cr_505  <> 0
                    or v_val_sdo_ctbl_fim_505 <> 0 then do:
                &else
                    find b_sdo_cta_ctbl_1
                        where b_sdo_cta_ctbl_1.cod_empresa        = sdo_cta_ctbl.cod_empresa
                        and   b_sdo_cta_ctbl_1.cod_finalid_econ   = sdo_cta_ctbl.cod_finalid_econ
                        and   b_sdo_cta_ctbl_1.cod_cenar_ctbl     = sdo_cta_ctbl.cod_cenar_ctbl
                        and   b_sdo_cta_ctbl_1.cod_plano_cta_ctbl = sdo_cta_ctbl.cod_plano_cta_ctbl
                        and   b_sdo_cta_ctbl_1.cod_cta_ctbl       = sdo_cta_ctbl.cod_cta_ctbl
                        and   b_sdo_cta_ctbl_1.cod_estab          = sdo_cta_ctbl.cod_estab
                        and   b_sdo_cta_ctbl_1.cod_unid_negoc     = sdo_cta_ctbl.cod_unid_negoc
                        and   b_sdo_cta_ctbl_1.dat_sdo_ctbl       = v_dat_fim_period_ctbl_ant
                        no-lock no-error.
                    if  avail b_sdo_cta_ctbl_1
                    then do:
                        /* FUT1082 - 09/07/2002
                        N∆o estava atualizando todas as vari†veis. */

                       /* assign v_val_sdo_ctbl_db_505  = b_sdo_cta_ctbl_1.val_sdo_ctbl_db
                                v_val_sdo_ctbl_cr_505  = b_sdo_cta_ctbl_1.val_sdo_ctbl_cr
                                v_val_sdo_ctbl_fim_505 = b_sdo_cta_ctbl_1.val_sdo_ctbl_fim.*/

                         assign v_val_sdo_ctbl_db_505        = b_sdo_cta_ctbl_1.val_sdo_ctbl_db
                                v_val_sdo_ctbl_cr_505        = b_sdo_cta_ctbl_1.val_sdo_ctbl_cr
                                v_val_sdo_ctbl_fim_505       = b_sdo_cta_ctbl_1.val_sdo_ctbl_fim
                                v_val_apurac_restdo_db_505   = b_sdo_cta_ctbl_1.val_apurac_restdo_db
                                v_val_apurac_restdo_cr_505   = b_sdo_cta_ctbl_1.val_apurac_restdo_cr
                                v_val_apurac_restdo_505      = b_sdo_cta_ctbl_1.val_apurac_restdo
                                v_val_apurac_restdo_acum_505 = b_sdo_cta_ctbl_1.val_apurac_restdo_acum.
                &endif    
                        assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant  = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant
                                                                                 + (v_val_sdo_ctbl_fim_505
                                                                                 / v_val_cotac_indic_econ)
                               tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant
                                                                                 + ((v_val_sdo_ctbl_fim_505
                                                                                 -  v_val_sdo_ctbl_db_505 + v_val_sdo_ctbl_cr_505)
                                                                                 /  v_val_cotac_indic_econ).

                        /* --- Caso considere apuraá∆o de resultado ---*/
                        if  v_log_consid_apurac_restdo = yes
                        then do:
                        /* FUT1082 - 09/07/2002
                        Faltou diminuir o valor de apuraá∆o acumulado do valor de apuraá∆o */

                            assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant  = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant
                                                                                     + (v_val_apurac_restdo_acum_505 / v_val_cotac_indic_econ)
                                   tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant
                                                                                     + ((v_val_apurac_restdo_acum_505 - v_val_apurac_restdo_505) / v_val_cotac_indic_econ).
                        end /* if */.
                        else do:
                            FIND GRP_CTA_CTBL OF cta_ctbl NO-LOCK.
                            if cta_ctbl.cod_cta_ctbl = plano_Cta_ctbl.cod_cta_ctbl_apurac_restdo
                                 or grp_Cta_Ctbl.log_consid_apurac = no then do:
                                assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant 
                                                                                        + (v_val_apurac_restdo_acum_505  / v_val_cotac_indic_econ) 
                                                                                        - (v_val_apurac_restdo_505  / v_val_cotac_indic_econ). 
                        end.
                    end.            
                end.
            end.            
            /* End_Include: i_ler_sdo_cta_ctbl_balanct */

        end.
    &else
        blk_sdo:
        for each sdo_cta_ctbl 
            where sdo_cta_ctbl.cod_empresa        = v_cod_unid_organ
            and   sdo_cta_ctbl.cod_finalid_econ   = v_cod_finalid_econ_bas
            and   sdo_cta_ctbl.cod_cenar_ctbl     = v_cod_cenar_ctbl
            and   sdo_cta_ctbl.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
            and   sdo_cta_ctbl.cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl
            and   sdo_cta_ctbl.dat_sdo_ctbl       = v_dat_fim_period_ctbl 
            no-lock.
            find first tt_estab_unid_negoc_select
                where tt_estab_unid_negoc_select.cod_estab      = sdo_cta_ctbl.cod_estab
                and   tt_estab_unid_negoc_select.cod_unid_negoc = sdo_cta_ctbl.cod_unid_negoc no-error.
            if not avail tt_estab_unid_negoc_select then next blk_sdo.

            /* --- Sumaria ou n∆o Estabelecimento ---*/
            if v_log_estab_sum = yes then
                assign v_cod_estab = "".
            else
                assign v_cod_estab = tt_estab_unid_negoc_select.cod_estab.
            /* --- Sumaria ou n∆o Unid Negoc ---*/
            if v_log_unid_negoc_sum = yes then
                assign v_cod_unid_negoc = "".
            else
                assign v_cod_unid_negoc = tt_estab_unid_negoc_select.cod_unid_negoc.

            assign v_val_sdo_ctbl_db_505        = sdo_cta_ctbl.val_sdo_ctbl_db
                   v_val_sdo_ctbl_cr_505        = sdo_cta_ctbl.val_sdo_ctbl_cr
                   v_val_sdo_ctbl_fim_505       = sdo_cta_ctbl.val_sdo_ctbl_fim
                   v_val_apurac_restdo_db_505   = sdo_cta_ctbl.val_apurac_restdo_db
                   v_val_apurac_restdo_cr_505   = sdo_cta_ctbl.val_apurac_restdo_cr
                   v_val_apurac_restdo_505      = sdo_cta_ctbl.val_apurac_restdo
                   v_val_apurac_restdo_acum_505 = sdo_cta_ctbl.val_apurac_restdo_acum.

            /* Begin_Include: i_ler_sdo_cta_ctbl_balanct */
            find first tt_sdo_cta_ctbl_balanct
                where tt_sdo_cta_ctbl_balanct.tta_cod_estab      = v_cod_estab
                and   tt_sdo_cta_ctbl_balanct.tta_cod_ccusto     = v_cod_ccusto
                and   tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc = v_cod_unid_negoc
                &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                and   tt_sdo_cta_ctbl_balanct.tta_cod_proj_financ = v_cod_proj_financ
                &ENDIF
                exclusive-lock no-error.
            if  not avail tt_sdo_cta_ctbl_balanct
            then do:
                create tt_sdo_cta_ctbl_balanct.
                assign tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl   = cta_ctbl.cod_cta_ctbl
                       tt_sdo_cta_ctbl_balanct.tta_cod_ccusto     = ""
                       tt_sdo_cta_ctbl_balanct.tta_cod_estab      = v_cod_estab
                       tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc = v_cod_unid_negoc
                       tt_sdo_cta_ctbl_balanct.tta_cod_empresa    = sdo_cta_ctbl.cod_empresa
                       tt_sdo_cta_ctbl_balanct.tta_cod_plano_cta_ctbl = sdo_cta_ctbl.cod_plano_cta_tbl
                       tt_sdo_cta_ctbl_balanct.tta_cod_plano_ccusto = "".

                &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                    assign tt_sdo_cta_ctbl_balanct.tta_cod_proj_financ = v_cod_proj_financ.
                &ENDIF
            end /* if */.
            assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1  = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                                                                + (v_val_sdo_ctbl_db_505 / v_val_cotac_indic_econ)
                   tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1  = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                                                                + (v_val_sdo_ctbl_cr_505 / v_val_cotac_indic_econ)
                   tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim = tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim
                                                                + (v_val_sdo_ctbl_fim_505 / v_val_cotac_indic_econ).
            /* --- Caso considere apuraá∆o de resultado ---*/
            if  v_log_consid_apurac_restdo = yes
            then do:
                if  v_val_apurac_restdo_db_505 = 0 and v_val_apurac_restdo_cr_505 = 0
                then do:
                    if  v_val_apurac_restdo_505 > 0
                    then do:
                        assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                                                                           + (v_val_apurac_restdo_505 / v_val_cotac_indic_econ).
                    end /* if */.
                    else do:
                        assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                                                                           + (v_val_apurac_restdo_505 / v_val_cotac_indic_econ * -1).
                    end /* else */.
                end /* if */.
                else do:
                    assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                                                                       + (v_val_apurac_restdo_db_505 / v_val_cotac_indic_econ)
                           tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                                                                       + (v_val_apurac_restdo_cr_505 / v_val_cotac_indic_econ).
                end /* else */.
                assign tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim = tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim
                                                                    + (v_val_apurac_restdo_acum_505 / v_val_cotac_indic_econ).
            end /* if */.
            else do:
                FIND GRP_CTA_CTBL OF cta_ctbl NO-LOCK.
                if cta_ctbl.cod_cta_ctbl = plano_cta_Ctbl.cod_cta_ctbl_apurac_restdo OR GRP_cTA_cTBL.LOG_CONSID_APURAC = NO then do:
                    assign tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim = tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim 
                                                                        + (v_val_apurac_restdo_acum_505 / v_val_cotac_indic_econ)
                                                                        - (v_val_apurac_restdo_505  / v_val_cotac_indic_econ).
                end.
            end.
            /* --- Totais de DB e CR ---*/
            if  cta_ctbl.ind_espec_cta_ctbl = "Anal°tica" /*l_analitica*/ 
            then do:
                assign v_val_tot_sdo_ctbl_db = v_val_tot_sdo_ctbl_db + (v_val_sdo_ctbl_db_505 / v_val_cotac_indic_econ)
                       v_val_tot_sdo_ctbl_cr = v_val_tot_sdo_ctbl_cr + (v_val_sdo_ctbl_cr_505 / v_val_cotac_indic_econ).
                /* --- Caso considere apuraá∆o de resultado ---*/
                if  v_log_consid_apurac_restdo = yes
                then do:
                    if  v_val_apurac_restdo_db_505 = 0 and v_val_apurac_restdo_cr_505 = 0
                    then do:
                        if  v_val_apurac_restdo_505 > 0
                        then do:
                            assign v_val_tot_sdo_ctbl_db = v_val_tot_sdo_ctbl_db + (v_val_apurac_restdo_505 / v_val_cotac_indic_econ).
                        end /* if */.
                        else do:
                            assign v_val_tot_sdo_ctbl_cr = v_val_tot_sdo_ctbl_cr + (v_val_apurac_restdo_505 / v_val_cotac_indic_econ * -1).
                        end /* else */.
                    end /* if */.
                    else do:
                        assign v_val_tot_sdo_ctbl_db = v_val_tot_sdo_ctbl_db + (v_val_apurac_restdo_db_505 / v_val_cotac_indic_econ)
                               v_val_tot_sdo_ctbl_cr = v_val_tot_sdo_ctbl_cr + (v_val_apurac_restdo_cr_505 / v_val_cotac_indic_econ).
                    end /* else */.
                end /* if */.
            end /* if */.
            /* --- Saldo do Per°odo Anterior ---*/
            if  v_dat_fim_period_ctbl_ant <> ?
            then do:
                /* FUT1082 - 09/07/2002
                N∆o estava zerando todas as vari†veis. */

                /* assign v_val_sdo_ctbl_db_505      = 0
                  v_val_sdo_ctbl_cr_505      = 0
                  v_val_sdo_ctbl_fim_505     = 0.*/

                 assign v_val_sdo_ctbl_db_505        = 0
                        v_val_sdo_ctbl_cr_505        = 0
                        v_val_sdo_ctbl_fim_505       = 0
                        v_val_apurac_restdo_db_505   = 0
                        v_val_apurac_restdo_cr_505   = 0
                        v_val_apurac_restdo_505      = 0
                        v_val_apurac_restdo_acum_505 = 0.


                &if '{&emsbas_version}' >= '5.05' &then
                    for each btt_retorna_sdo_ctbl_demonst
                        where btt_retorna_sdo_ctbl_demonst.tta_cod_empresa        = v_cod_unid_organ
                        and   btt_retorna_sdo_ctbl_demonst.tta_cod_finalid_econ   = v_cod_finalid_econ_bas
                        and   btt_retorna_sdo_ctbl_demonst.tta_cod_cenar_ctbl     = v_cod_cenar_ctbl
                        and   btt_retorna_sdo_ctbl_demonst.tta_cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
                        and   btt_retorna_sdo_ctbl_demonst.tta_cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl
                        and   btt_retorna_sdo_ctbl_demonst.tta_cod_estab          = tt_estab_unid_negoc_select.cod_estab
                        and   btt_retorna_sdo_ctbl_demonst.tta_cod_unid_negoc     = tt_estab_unid_negoc_select.cod_unid_negoc
                        and   btt_retorna_sdo_ctbl_demonst.tta_dat_sdo_ctbl       = v_dat_fim_period_ctbl_ant
                        and   btt_retorna_sdo_ctbl_demonst.tta_cod_plano_ccusto   = ''
                        &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                            and   btt_retorna_sdo_ctbl_demonst.tta_cod_proj_financ    = tt_proj_financ_select.tta_cod_proj_financ
                        &ENDIF
                        use-index tt_id:
                        assign v_val_sdo_ctbl_db_505      = v_val_sdo_ctbl_db_505  + btt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_db
                               v_val_sdo_ctbl_cr_505      = v_val_sdo_ctbl_cr_505  + btt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_cr
                               v_val_sdo_ctbl_fim_505     = v_val_sdo_ctbl_fim_505 + btt_retorna_sdo_ctbl_demonst.tta_val_sdo_ctbl_fim.
                    end.
                    /* ** VERIFICA SE CONTAS TEM SALDO ***/
                    if v_val_sdo_ctbl_db_505  <> 0
                    or v_val_sdo_ctbl_cr_505  <> 0
                    or v_val_sdo_ctbl_fim_505 <> 0 then do:
                &else
                    find b_sdo_cta_ctbl_1
                        where b_sdo_cta_ctbl_1.cod_empresa        = sdo_cta_ctbl.cod_empresa
                        and   b_sdo_cta_ctbl_1.cod_finalid_econ   = sdo_cta_ctbl.cod_finalid_econ
                        and   b_sdo_cta_ctbl_1.cod_cenar_ctbl     = sdo_cta_ctbl.cod_cenar_ctbl
                        and   b_sdo_cta_ctbl_1.cod_plano_cta_ctbl = sdo_cta_ctbl.cod_plano_cta_ctbl
                        and   b_sdo_cta_ctbl_1.cod_cta_ctbl       = sdo_cta_ctbl.cod_cta_ctbl
                        and   b_sdo_cta_ctbl_1.cod_estab          = sdo_cta_ctbl.cod_estab
                        and   b_sdo_cta_ctbl_1.cod_unid_negoc     = sdo_cta_ctbl.cod_unid_negoc
                        and   b_sdo_cta_ctbl_1.dat_sdo_ctbl       = v_dat_fim_period_ctbl_ant
                        no-lock no-error.
                    if  avail b_sdo_cta_ctbl_1
                    then do:
                        /* FUT1082 - 09/07/2002
                        N∆o estava atualizando todas as vari†veis. */

                       /* assign v_val_sdo_ctbl_db_505  = b_sdo_cta_ctbl_1.val_sdo_ctbl_db
                                v_val_sdo_ctbl_cr_505  = b_sdo_cta_ctbl_1.val_sdo_ctbl_cr
                                v_val_sdo_ctbl_fim_505 = b_sdo_cta_ctbl_1.val_sdo_ctbl_fim.*/

                         assign v_val_sdo_ctbl_db_505        = b_sdo_cta_ctbl_1.val_sdo_ctbl_db
                                v_val_sdo_ctbl_cr_505        = b_sdo_cta_ctbl_1.val_sdo_ctbl_cr
                                v_val_sdo_ctbl_fim_505       = b_sdo_cta_ctbl_1.val_sdo_ctbl_fim
                                v_val_apurac_restdo_db_505   = b_sdo_cta_ctbl_1.val_apurac_restdo_db
                                v_val_apurac_restdo_cr_505   = b_sdo_cta_ctbl_1.val_apurac_restdo_cr
                                v_val_apurac_restdo_505      = b_sdo_cta_ctbl_1.val_apurac_restdo
                                v_val_apurac_restdo_acum_505 = b_sdo_cta_ctbl_1.val_apurac_restdo_acum.
                &endif    
                        assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant  = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant
                                                                                 + (v_val_sdo_ctbl_fim_505
                                                                                 / v_val_cotac_indic_econ)
                               tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant
                                                                                 + ((v_val_sdo_ctbl_fim_505
                                                                                 -  v_val_sdo_ctbl_db_505 + v_val_sdo_ctbl_cr_505)
                                                                                 /  v_val_cotac_indic_econ).

                        /* --- Caso considere apuraá∆o de resultado ---*/
                        if  v_log_consid_apurac_restdo = yes
                        then do:
                        /* FUT1082 - 09/07/2002
                        Faltou diminuir o valor de apuraá∆o acumulado do valor de apuraá∆o */

                            assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant  = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant
                                                                                     + (v_val_apurac_restdo_acum_505 / v_val_cotac_indic_econ)
                                   tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant
                                                                                     + ((v_val_apurac_restdo_acum_505 - v_val_apurac_restdo_505) / v_val_cotac_indic_econ).
                        end /* if */.
                        else do:
                            FIND GRP_CTA_CTBL OF cta_ctbl NO-LOCK.
                            if cta_ctbl.cod_cta_ctbl = plano_Cta_ctbl.cod_cta_ctbl or grp_Cta_Ctbl.log_consid_apurac = no then do:
                                assign tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant = tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant 
                                                                                        + (v_val_apurac_restdo_acum_505  / v_val_cotac_indic_econ) 
                                                                                        - (v_val_apurac_restdo_505  / v_val_cotac_indic_econ). 
                        end.
                    end.            
                end.
            end.            
            /* End_Include: i_ler_sdo_cta_ctbl_balanct */

        end.               
    &endif

    if  v_log_cta_ctbl_sdo = yes
    then do:  /* --- Conta sem Saldos ---*/  
        if  v_ind_tip_plano_cta_ctbl <> "Consolidaá∆o" /*l_consolidacao*/  then do:
            for each tt_estab_unid_negoc_select
            &IF DEFINED(BF_ADM_FIN_PROJ) &THEN 
            ,
            each tt_proj_financ_select 
                where tt_proj_financ_select.tta_cod_estab = tt_estab_unid_negoc_select.cod_estab
            &ENDIF
            :

                /* --- Sumaria ou n∆o Estabelecimento ---*/
                if v_log_estab_sum = yes then
                    assign v_cod_estab = "".
                else
                    assign v_cod_estab = tt_estab_unid_negoc_select.cod_estab.
                /* --- Sumaria ou n∆o Unid Negoc ---*/
                if v_log_unid_negoc_sum = yes then
                    assign v_cod_unid_negoc = "" /*l_null*/ .
                else
                    assign v_cod_unid_negoc = tt_estab_unid_negoc_select.cod_unid_negoc.

                &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                    /* --- Sumaria ou n∆o Projeto ---*/
                    if v_log_proj_financ = yes then
                        assign v_cod_proj_financ = "" /*l_null*/ .
                    else
                        assign v_cod_proj_financ = tt_proj_financ_select.tta_cod_proj_financ.
                &ENDIF

                if not can-find(tt_sdo_cta_ctbl_balanct
                    where tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl   = cta_ctbl.cod_cta_ctbl
                      and tt_sdo_cta_ctbl_balanct.tta_cod_estab      = v_cod_estab
                      and tt_sdo_cta_ctbl_balanct.tta_cod_ccusto     = v_cod_ccusto_aux
                      and tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc = v_cod_unid_negoc
                      &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                      and tt_sdo_cta_ctbl_balanct.tta_cod_proj_financ = v_cod_proj_financ
                      &ENDIF) then do:
                      create tt_sdo_cta_ctbl_balanct.
                      assign tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl   = cta_ctbl.cod_cta_ctbl
                             tt_sdo_cta_ctbl_balanct.tta_cod_ccusto     = v_cod_ccusto_aux
                             tt_sdo_cta_ctbl_balanct.tta_cod_estab      = v_cod_estab
                             tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc = v_cod_unid_negoc
                             tt_sdo_cta_ctbl_balanct.tta_cod_empresa        = v_cod_unid_organ     
                             tt_sdo_cta_ctbl_balanct.tta_cod_plano_cta_ctbl = v_cod_plano_cta_ctbl 
                             tt_sdo_cta_ctbl_balanct.tta_cod_plano_ccusto = IF v_cod_ccusto_aux = "" THEN "" ELSE v_cod_plano_ccusto.

                      &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                          assign tt_sdo_cta_ctbl_balanct.tta_cod_proj_financ = v_cod_proj_financ.
                      &ENDIF
                end /* if */.
            end.        
        end.
        else do:
            /* --- Sumaria ou n“o Unid Negoc ---*/
            if  v_log_unid_negoc_sum = no then do:
                for each b_dwb_rpt_select
                   where b_dwb_rpt_select.cod_dwb_program = v_cod_dwb_program
                   and   b_dwb_rpt_select.cod_dwb_user    = v_cod_dwb_user
                   and   b_dwb_rpt_select.cod_dwb_field   = "Unid Neg¢cio" /*l_Unid_Neg¢cio*/ 
                   and   b_dwb_rpt_select.log_dwb_rule    = yes no-lock:
                    for each unid_negoc no-lock
                       where unid_negoc.cod_unid_negoc >= b_dwb_rpt_select.cod_dwb_initial
                       and   unid_negoc.cod_unid_negoc <= b_dwb_rpt_select.cod_dwb_final:

                        assign v_cod_unid_negoc = unid_negoc.cod_unid_negoc.

                        find first tt_sdo_cta_ctbl_balanct
                            where tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl   = cta_ctbl.cod_cta_ctbl
                              and tt_sdo_cta_ctbl_balanct.tta_cod_estab      = v_cod_estab
                              and tt_sdo_cta_ctbl_balanct.tta_cod_ccusto     = v_cod_ccusto_aux
                              and tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc = v_cod_unid_negoc no-lock no-error.
                        if  not avail tt_sdo_cta_ctbl_balanct
                        then do:
                            create tt_sdo_cta_ctbl_balanct.
                            assign tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl   = cta_ctbl.cod_cta_ctbl
                                   tt_sdo_cta_ctbl_balanct.tta_cod_ccusto     = v_cod_ccusto_aux
                                   tt_sdo_cta_ctbl_balanct.tta_cod_estab      = v_cod_estab
                                   tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc = v_cod_unid_negoc
                                   tt_sdo_cta_ctbl_balanct.tta_cod_empresa        = v_cod_unid_organ     
                                   tt_sdo_cta_ctbl_balanct.tta_cod_plano_cta_ctbl = v_cod_plano_cta_ctbl 
                                   tt_sdo_cta_ctbl_balanct.tta_cod_plano_ccusto = IF v_cod_ccusto_aux = "" THEN "" ELSE v_cod_plano_ccusto.

                        end /* if */.
                    end.
                end.
            end.
            else do:
                find first tt_sdo_cta_ctbl_balanct
                    where tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl   = cta_ctbl.cod_cta_ctbl
                      and tt_sdo_cta_ctbl_balanct.tta_cod_estab      = "" /*l_null*/ 
                      and tt_sdo_cta_ctbl_balanct.tta_cod_ccusto     = "" /*l_null*/ 
                      and tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc = "" /*l_null*/  no-lock no-error.
                if  not avail tt_sdo_cta_ctbl_balanct
                then do:
                    create tt_sdo_cta_ctbl_balanct.
                    assign tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl   = cta_ctbl.cod_cta_ctbl
                           tt_sdo_cta_ctbl_balanct.tta_cod_ccusto     = "" /*l_null*/ 
                           tt_sdo_cta_ctbl_balanct.tta_cod_estab      = "" /*l_null*/ 
                           tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc = "" /*l_null*/ .
                end /* if */.
            end.
        end.
    end.


END PROCEDURE. /* pi_ler_sdo_cta_ctbl_balanct_1 */
/*****************************************************************************
** Procedure Interna.....: pi_estrut_cta_ctbl
** Descricao.............: pi_estrut_cta_ctbl
** Criado por............: fut1090
** Criado em.............: 26/09/2002 09:01:59
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_estrut_cta_ctbl:

    /************************ Parameter Definition Begin ************************/

    def input-output param table 
        for tt_sdo_cta_ctbl_balanct.


    /************************* Parameter Definition End *************************/

    /************************** Buffer Definition Begin *************************/

    def buffer btt_sdo_cta_ctbl_balanct
        for tt_sdo_cta_ctbl_balanct.
    def buffer btt_sdo_cta_ctbl_balanct_aux
        for tt_sdo_cta_ctbl_balanct_aux.


    /*************************** Buffer Definition End **************************/

    /************************* Variable Definition Begin ************************/

    def var v_des_tit_ctbl_balanct_aux       as character       no-undo. /*local*/


    /************************** Variable Definition End *************************/

    for each tt_sdo_cta_ctbl_balanct 
        break by tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl:
        find estrut_cta_ctbl no-lock
            where estrut_cta_ctbl.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
            and   estrut_cta_ctbl.cod_cta_ctbl_filho = tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl no-error.
        if  avail estrut_cta_ctbl then do:

            /* Copia o registro da tt_oficial para tt_aux*/
            create btt_sdo_cta_ctbl_balanct_aux.
            buffer-copy tt_sdo_cta_ctbl_balanct to btt_sdo_cta_ctbl_balanct_aux.
            if first-of(tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl) then
                assign btt_sdo_cta_ctbl_balanct_aux.ttv_des_tit_ctbl_balanct = v_des_tit_ctbl_balanct
                       v_des_tit_ctbl_balanct_aux = v_des_tit_ctbl_balanct.
            else 
                assign btt_sdo_cta_ctbl_balanct_aux.ttv_des_tit_ctbl_balanct = v_des_tit_ctbl_balanct_aux.

            /* elimina tt_oficial*/
            delete tt_sdo_cta_ctbl_balanct.

            for each tt_sdo_cta_ctbl_balanct_aux
            where tt_sdo_cta_ctbl_balanct_aux.tta_cod_cta_ctbl   = estrut_cta_ctbl.cod_cta_ctbl_pai :

                /* Copia da o registro da tt_aux para a tt_oficial */
                create btt_sdo_cta_ctbl_balanct.
                buffer-copy tt_sdo_cta_ctbl_balanct_aux to btt_sdo_cta_ctbl_balanct.
                assign v_des_tit_ctbl_balanct = tt_sdo_cta_ctbl_balanct_aux.ttv_des_tit_ctbl_balanct.

                /* elimina tt_aux para que o registro n∆o seja impresso novamente*/
                delete tt_sdo_cta_ctbl_balanct_aux.
            end.
        end.
    end.

    find estrut_cta_ctbl no-lock
        where estrut_cta_ctbl.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
        and   estrut_cta_ctbl.cod_cta_ctbl_filho = btt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl no-error.
    if  avail estrut_cta_ctbl then do:
        find first tt_sdo_cta_ctbl_balanct_aux no-lock
        where tt_sdo_cta_ctbl_balanct_aux.tta_cod_cta_ctbl = estrut_cta_ctbl.cod_cta_ctbl_pai no-error.
        if  avail tt_sdo_cta_ctbl_balanct_aux then
            run pi_estrut_cta_ctbl (input-output table btt_sdo_cta_ctbl_balanct) /*pi_estrut_cta_ctbl*/.
    end.
END PROCEDURE. /* pi_estrut_cta_ctbl */
/*****************************************************************************
** Procedure Interna.....: pi_imprimir_sdo_cta_ctbl_balanct_2
** Descricao.............: pi_imprimir_sdo_cta_ctbl_balanct_2
** Criado por............: fut1090
** Criado em.............: 26/09/2002 07:55:14
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_imprimir_sdo_cta_ctbl_balanct_2:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_cta_ctbl
        as character
        format "x(20)"
        no-undo.
    def Input param p_log_criac_reg
        as logical
        format "Sim/N∆o"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************** Buffer Definition Begin *************************/

    def buffer btt_sdo_cta_ctbl_balanct_aux
        for tt_sdo_cta_ctbl_balanct_aux.
    def buffer btt_sdo_cta_ctbl_balanct_aux2
        for tt_sdo_cta_ctbl_balanct_aux2.
    def buffer b_estrut_cta_ctbl
        for estrut_cta_ctbl.


    /*************************** Buffer Definition End **************************/

    /************************* Variable Definition Begin ************************/

    def var v_cod_ccusto_form                as character       no-undo. /*local*/
    def var v_cod_cta_ctbl_ant               as character       no-undo. /*local*/
    def var v_des_tit_ctbl_aux               as character       no-undo. /*local*/
    def var v_num_tit_estab                  as integer         no-undo. /*local*/
    def var v_num_tit_proj_financ            as integer         no-undo. /*local*/
    def var v_num_tit_unid_negoc             as integer         no-undo. /*local*/
    def var v_val_sdo_cr                     as decimal         no-undo. /*local*/
    def var v_val_sdo_cr_2                   as decimal         no-undo. /*local*/
    def var v_val_sdo_db                     as decimal         no-undo. /*local*/
    def var v_val_sdo_db_2                   as decimal         no-undo. /*local*/
    def var v_val_sdo_fim                    as decimal         no-undo. /*local*/
    def var v_val_sdo_fim_2                  as decimal         no-undo. /*local*/
    def var v_val_sdo_fim_ant                as decimal         no-undo. /*local*/
    def var v_val_sdo_fim_ant_2              as decimal         no-undo. /*local*/
    def var v_val_sdo_inic                   as decimal         no-undo. /*local*/
    def var v_val_sdo_inic_2                 as decimal         no-undo. /*local*/
    def var v_val_sdo_inic_ant               as decimal         no-undo. /*local*/
    def var v_val_sdo_inic_ant_2             as decimal         no-undo. /*local*/


    /************************** Variable Definition End *************************/
    /* --- Classificado por Estrutura ---*/
    if  entry(1,dwb_rpt_param.cod_dwb_order) = "Estrutura" /*l_estrutura*/  then do:
        if p_log_criac_reg = yes then
            for each tt_sdo_cta_ctbl_balanct:
                create tt_sdo_cta_ctbl_balanct_aux2.
                buffer-copy tt_sdo_cta_ctbl_balanct to tt_sdo_cta_ctbl_balanct_aux2
                assign tt_sdo_cta_ctbl_balanct_aux2.ttv_des_tit_ctbl_balanct = v_des_tit_ctbl_balanct.    
            end.
        /* Feito com find para que n∆o entre em loop devido ˚ pi recursiva que Ç chamada.*/
        find first tt_sdo_cta_ctbl_balanct no-error.
        if avail tt_sdo_cta_ctbl_balanct then do:
            find  b_estrut_cta_ctbl no-lock
            where b_estrut_cta_ctbl.cod_plano_cta_ctbl = v_cod_plano_cta_ctbl
            and   b_estrut_cta_ctbl.cod_cta_ctbl_filho = tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl no-error.
            if  avail b_estrut_cta_ctbl then do:
                find first btt_sdo_cta_ctbl_balanct_aux no-lock
                where btt_sdo_cta_ctbl_balanct_aux.tta_cod_cta_ctbl = b_estrut_cta_ctbl.cod_cta_ctbl_pai no-error.
                if  avail btt_sdo_cta_ctbl_balanct_aux then
                    run pi_estrut_cta_ctbl (input-output table tt_sdo_cta_ctbl_balanct) /*pi_estrut_cta_ctbl*/.
            end.
        end.  
    end.
    /* INICIO: ATIVIDADE: 36.289 - TASK: 5.646 */
    if  v_log_mostra_sem_aprop_cc = no and
        v_log_ccusto_sum = no
    then do:
        del_sem_aprop:
        for each tt_sdo_cta_ctbl_balanct exclusive-lock:
            if  tt_sdo_cta_ctbl_balanct.tta_cod_ccusto = ""
            then do:
                 if cta_ctbl.ind_espec_cta_ctbl = "Anal°tica" /*l_analitica*/  then do:
                     assign v_val_tot_sdo_ctbl_db = v_val_tot_sdo_ctbl_db - tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                            v_val_tot_sdo_ctbl_cr = v_val_tot_sdo_ctbl_cr - tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1.
                 end.
                 delete tt_sdo_cta_ctbl_balanct.
            end.
        end.
    end /* if */.
    /* FIM: ATIVIDADE: 36.289 - TASK: 5.646 */
    if v_nom_prog = "Di†rio" /*l_diario*/  then
        run pi_verifica_pagina_impr_termos_diario /*pi_verifica_pagina_impr_termos_diario*/.
    assign v_cod_ccusto_form = fill("9",11).
    find unid_organ 
        where unid_organ.cod_unid_organ = v_cod_unid_organ 
        no-lock no-error.
    if  avail unid_organ
    then do:
        find plano_ccusto 
            where plano_ccusto.cod_empresa      = unid_organ.cod_unid_organ
            and   plano_ccusto.cod_plano_ccusto = v_cod_plano_ccusto 
            no-lock no-error.
        if  avail plano_ccusto then
            assign v_cod_ccusto_form = plano_ccusto.cod_format_ccusto.
    end.
    &if '{&emsbas_version}' <= '5.04' &then
        if  v_log_ccusto_sum = no
        and v_log_mostra_sem_aprop_cc = yes 
        and v_ind_tip_plano_cta_ctbl <> "Consolidaá∆o" /*l_consolidacao*/ 
        then do:
            for each tt_sdo_cta_ctbl_balanct exclusive-lock
                break by tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl:
                if  last-of(tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl) then do:
                    for each tt_estab_unid_negoc_select
                    &IF DEFINED(BF_ADM_FIN_PROJ) &THEN 
                    ,
                    each tt_proj_financ_select 
                        where tt_proj_financ_select.tta_cod_estab = tt_estab_unid_negoc_select.cod_estab
                    &ENDIF
                    :
                        if  v_log_estab_sum = yes then
                            assign v_cod_estab = "".
                        else
                            assign v_cod_estab = tt_estab_unid_negoc_select.cod_estab.
                        if  v_log_unid_negoc_sum = yes then
                            assign v_cod_unid_negoc = "".
                        else
                            assign v_cod_unid_negoc = tt_estab_unid_negoc_select.cod_unid_negoc.

                        &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                        if  v_log_proj_financ = yes then 
                            assign v_cod_proj_financ = "". 
                        else 
                            assign v_cod_proj_financ = tt_proj_financ_select.tta_cod_proj_financ.
                        &ENDIF
                        run pi_retorna_val_sdo_cta_ctbl_balanct (Input tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl,
                                                                 Input v_cod_estab,
                                                                 Input v_cod_unid_negoc) /*pi_retorna_val_sdo_cta_ctbl_balanct*/.
                    end.
                end.
            end.
            assign v_val_sdo_inic = 0 v_val_sdo_fim = 0      v_val_sdo_cr = 0
                   v_val_sdo_db = 0   v_val_sdo_inic_ant = 0 v_val_sdo_fim_ant = 0.
            for each tt_sdo_cta_ctbl_balanct exclusive-lock
                break by tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl
                      by tt_sdo_cta_ctbl_balanct.tta_cod_estab
                      by tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc
                      &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                      by tt_sdo_cta_ctbl_balanct.tta_cod_proj_financ
                      &ENDIF
                      :
                if  tt_sdo_cta_ctbl_balanct.tta_cod_ccusto = v_cod_ccusto_form then
                    next.
                assign v_val_sdo_inic     = v_val_sdo_inic + tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim
                                          + tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                                          - tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                       v_val_sdo_fim      = v_val_sdo_fim + tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim
                       v_val_sdo_cr       = v_val_sdo_cr + tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                       v_val_sdo_db       = v_val_sdo_db + tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
                       v_val_sdo_inic_ant = v_val_sdo_inic_ant + tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant
                       v_val_sdo_fim_ant  = v_val_sdo_fim_ant + tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant.
                if  last-of(tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl)
                or  last-of(tt_sdo_cta_ctbl_balanct.tta_cod_estab)
                or  last-of(tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc)
                &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                or  last-of(tt_sdo_cta_ctbl_balanct.tta_cod_proj_financ)
                &ENDIF
                then do:
                    /* faz a comparacao de totais da temp-table tt_sdo_cta_ctbl_balanct com a tt_sdo_cta_ctbl_sem_aprop, caso haja diferenca nos totais,
                       Ç criado um registro na tt_sdo_cta_ctbl_balanct contendo a diferenca de valores */
                    assign v_val_sdo_inic_2 = 0
                           v_val_sdo_fim_2 = 0
                           v_val_sdo_cr_2 = 0
                           v_val_sdo_db_2 = 0
                           v_val_sdo_inic_ant_2 = 0
                           v_val_sdo_fim_ant_2 = 0.
                    for each tt_sdo_cta_ctbl_sem_aprop exclusive-lock
                        where tt_sdo_cta_ctbl_sem_aprop.tta_cod_cta_ctbl   = tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl
                        and   tt_sdo_cta_ctbl_sem_aprop.tta_cod_estab      = tt_sdo_cta_ctbl_balanct.tta_cod_estab
                        and   tt_sdo_cta_ctbl_sem_aprop.tta_cod_unid_negoc = tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc
                        &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
                        and   tt_sdo_cta_ctbl_sem_aprop.tta_cod_proj_financ = tt_sdo_cta_ctbl_balanct.tta_cod_proj_financ
                        &ENDIF
                        :
                        assign v_val_sdo_inic_2     = v_val_sdo_inic_2
                                                    + tt_sdo_cta_ctbl_sem_aprop.tta_val_sdo_ctbl_fim
                                                    + tt_sdo_cta_ctbl_sem_aprop.tta_val_sdo_ctbl_cr
                                                    - tt_sdo_cta_ctbl_sem_aprop.tta_val_sdo_ctbl_db
                               v_val_sdo_fim_2      = v_val_sdo_fim_2
                                                    + tt_sdo_cta_ctbl_sem_aprop.tta_val_sdo_ctbl_fim
                               v_val_sdo_cr_2       = v_val_sdo_cr_2
                                                    + tt_sdo_cta_ctbl_sem_aprop.tta_val_sdo_ctbl_cr
                               v_val_sdo_db_2       = v_val_sdo_db_2
                                                    + tt_sdo_cta_ctbl_sem_aprop.tta_val_sdo_ctbl_db
                               v_val_sdo_inic_ant_2 = v_val_sdo_inic_ant_2
                                                    + tt_sdo_cta_ctbl_sem_aprop.ttv_val_sdo_ctbl_inic_ant
                               v_val_sdo_fim_ant_2  = v_val_sdo_fim_ant_2
                                                    + tt_sdo_cta_ctbl_sem_aprop.ttv_val_sdo_ctbl_fim_ant.
                    end.
                    if  v_val_sdo_inic <> v_val_sdo_inic_2
                    or  v_val_sdo_fim <> v_val_sdo_fim_2
                    or  v_val_sdo_cr <> v_val_sdo_cr_2
                    or  v_val_sdo_db <> v_val_sdo_db_2
                    or  v_val_sdo_inic_ant <> v_val_sdo_inic_ant_2
                    or  v_val_sdo_fim_ant <> v_val_sdo_fim_ant_2 then do:
                        run pi_create_tt_sdo_cta_ctbl_balanct(input (v_val_sdo_db_2 - v_val_sdo_db),
                                                              input (v_val_sdo_cr_2 - v_val_sdo_cr),
                                                              input (v_val_sdo_fim_2 - v_val_sdo_fim),
                                                              input (v_val_sdo_inic_ant_2 - v_val_sdo_inic_ant),
                                                              input (v_val_sdo_fim_ant_2 - v_val_sdo_fim_ant)).
                    end.
                    assign v_val_sdo_inic = 0
                           v_val_sdo_fim = 0
                           v_val_sdo_cr = 0
                           v_val_sdo_db = 0
                           v_val_sdo_inic_ant = 0
                           v_val_sdo_fim_ant = 0.
                end.
            end.
        end.
    &endif
    
    sdo_cta_ctbl_block:
    for each tt_sdo_cta_ctbl_balanct EXCLUSIVE-LOCK 
        break by tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl
              by tt_sdo_cta_ctbl_balanct.tta_cod_estab
              by tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc
              by tt_sdo_cta_ctbl_balanct.tta_cod_ccusto
              &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
              by tt_sdo_cta_ctbl_balanct.tta_cod_proj_financ
              &ENDIF
              :
        
        if v_nom_prog = "Di†rio" /*l_diario*/  then
            run pi_verifica_pagina_impr_termos_diario /*pi_verifica_pagina_impr_termos_diario*/.
        /* --- Totalizaá∆o dos Grupos de Contas ---*/
        if  v_cod_grp_cta_ctbl <> cta_ctbl.cod_grp_cta_ctbl
        then do:
            assign v_cod_grp_cta_ctbl = cta_ctbl.cod_grp_cta_ctbl.
            find tt_grp_cta_ctbl
                where tt_grp_cta_ctbl.tta_cod_grp_cta_ctbl = cta_ctbl.cod_grp_cta_ctbl no-lock no-error.
            if  not avail tt_grp_cta_ctbl
            then do:
                create tt_grp_cta_ctbl.
                assign tt_grp_cta_ctbl.tta_cod_grp_cta_ctbl = cta_ctbl.cod_grp_cta_ctbl.
            end /* if */.
        end /* if */.
        assign tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_inic = tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim
                                                             + tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1
                                                             - tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1.
        if  cta_ctbl.ind_espec_cta_ctbl = "Anal°tica" /*l_analitica*/ 
        then do:
            find tt_grp_cta_ctbl
                where tt_grp_cta_ctbl.tta_cod_grp_cta_ctbl = cta_ctbl.cod_grp_cta_ctbl
                exclusive-lock no-error.
            if  avail tt_grp_cta_ctbl
            then do:
                assign tt_grp_cta_ctbl.tta_val_sdo_ctbl_inic = tt_grp_cta_ctbl.tta_val_sdo_ctbl_inic
                                                             + tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_inic
                       tt_grp_cta_ctbl.tta_val_sdo_ctbl_fim = tt_grp_cta_ctbl.tta_val_sdo_ctbl_fim
                                                             + tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim.
                if v_log_period_ctbl_ant_impr = yes then
                    assign tt_grp_cta_ctbl.ttv_val_sdo_ctbl_inic_ant = tt_grp_cta_ctbl.ttv_val_sdo_ctbl_inic_ant
                                                                 + tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant
                           tt_grp_cta_ctbl.ttv_val_sdo_ctbl_fim_ant = tt_grp_cta_ctbl.ttv_val_sdo_ctbl_fim_ant
                                                               + tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant.
            end /* if */.
        end /* if */.
        if first-of(tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl) then
            assign v_des_tit_ctbl_balanct = substr(v_des_tit_ctbl_balanct,1,v_num_tit_ctbl)
                   v_des_tit_ctbl_aux = v_des_tit_ctbl_balanct.
        else do:
            if line-counter(s_1) = v_rpt_s_1_bottom then
                assign v_des_tit_ctbl_balanct = v_des_tit_ctbl_aux.
            else
                assign v_des_tit_ctbl_balanct = fill(chr(32),v_num_tit_ctbl).
        end /* else */.
        assign v_des_tit_ctbl_compl = v_cod_carac_lim.
        if  tt_sdo_cta_ctbl_balanct.tta_cod_estab <> ""
        then do:
            if first-of(tt_sdo_cta_ctbl_balanct.tta_cod_estab) or
                line-counter(s_1) = v_rpt_s_1_bottom then
                assign substr(v_des_tit_ctbl_balanct, (v_num_tit_ctbl + 2), 4) = tt_sdo_cta_ctbl_balanct.tta_cod_estab
                       v_num_tit_estab = 4.
            else
                assign substr(v_des_tit_ctbl_balanct, v_num_tit_ctbl + 2, 4) = fill(chr(32),3).
            assign v_des_tit_ctbl_compl = v_des_tit_ctbl_compl + tt_sdo_cta_ctbl_balanct.tta_cod_estab + v_cod_carac_lim.
        end /* if */.
        if  tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc <> ""
        then do:
            if first-of(tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc) or
                line-counter(s_1) = v_rpt_s_1_bottom then
                assign substr(v_des_tit_ctbl_balanct, (v_num_tit_ctbl + v_num_tit_estab + 2), 4) = tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc
                       v_num_tit_unid_negoc = 4.
            else
                assign substr(v_des_tit_ctbl_balanct, (v_num_tit_ctbl + v_num_tit_estab + 2), 4) = fill(chr(32),3).
            assign v_des_tit_ctbl_compl = v_des_tit_ctbl_compl + tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc + v_cod_carac_lim.
        end /* if */.
        &IF DEFINED(BF_ADM_FIN_PROJ) &THEN
            if  tt_sdo_cta_ctbl_balanct.tta_cod_proj_financ <> ""
            then do:
                if  first-of(tt_sdo_cta_ctbl_balanct.tta_cod_proj_financ) or 
                    line-counter(s_1) = v_rpt_s_1_bottom 
                then do:
                    assign substr(v_des_tit_ctbl_balanct, (v_num_tit_ctbl + v_num_tit_estab + v_num_tit_unid_negoc + 2), 4) = string(tt_sdo_cta_ctbl_balanct.tta_cod_proj_financ, v_cod_format_proj_financ)
                           v_num_tit_proj_financ = v_num_proj_financ_max + 1.
                end /* if */.
                else do:
                    assign substr(v_des_tit_ctbl_balanct, (v_num_tit_ctbl + v_num_tit_estab + v_num_tit_unid_negoc + 2), 4) = fill(chr(32),20). 
                end /* else */. 
                assign v_des_tit_ctbl_compl = v_des_tit_ctbl_compl + tt_sdo_cta_ctbl_balanct.tta_cod_proj_financ + v_cod_carac_lim. 
            end.
        &ENDIF
        if  v_log_ccusto_sum = NO
        then do:
            if tt_sdo_cta_ctbl_balanct.tta_cod_ccusto <> "" then
                assign substr(v_des_tit_ctbl_balanct, (v_num_tit_ctbl + v_num_tit_estab + v_num_tit_unid_negoc + v_num_tit_proj_financ + 2), 11) =
                                                      string(tt_sdo_cta_ctbl_balanct.tta_cod_ccusto, v_cod_ccusto_form)
                       v_des_tit_ctbl_compl = v_des_tit_ctbl_compl + string(tt_sdo_cta_ctbl_balanct.tta_cod_ccusto, v_cod_ccusto_form) + v_cod_carac_lim.
            else
                assign substr(v_des_tit_ctbl_balanct, (v_num_tit_ctbl + v_num_tit_estab + v_num_tit_unid_negoc + v_num_tit_proj_financ + 2), 11) = ""
                       v_des_tit_ctbl_compl = v_des_tit_ctbl_compl + "" + v_cod_carac_lim.
        end /* if */.

        /* --- N∆o imprime Per°odo Anterior ---*/
        /* --- Retornar Indicador ---*/
        run pi_retornar_indic_cr_sdo (input-output tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_inic,
                                      output v_ind_cr_sdo_inic) /*pi_retornar_indic_cr_sdo*/.
        run pi_retornar_indic_cr_sdo (input-output tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim,
                                      output v_ind_cr_sdo_fim) /*pi_retornar_indic_cr_sdo*/.
        if  (v_nom_prog = "Di†rio" /*l_diario*/  ) and v_num_aux_1 = 1
        then do:
            assign v_num_aux_1 = 0.
            /*if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                page stream s_1.
            put stream s_1 unformatted 
                "T°tulo" at 1
                "Saldo Inicial" to 73
                "DÇbito" to 94
                "CrÇdito" to 113
                "Saldo Final" to 131 skip
                "-------------------------------------------------------" at 1
                "-----------------" to 73
                "------------------" to 94
                "------------------" to 113
                "-----------------" to 131 skip.*/
        end /* if */.
        if  line-counter(s_1) = 1 or
           (line-counter(s_1) + 1) > v_rpt_s_1_bottom
        then do:
            if  v_nom_prog = "Di†rio" /*l_diario*/ 
            then do:
                /*if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "T°tulo" at 1
                    "Saldo Inicial" to 73
                    "DÇbito" to 94
                    "CrÇdito" to 113
                    "Saldo Final" to 131 skip
                    "-------------------------------------------------------" at 1
                    "-----------------" to 73
                    "------------------" to 94
                    "------------------" to 113
                    "-----------------" to 131 skip.*/
            end /* if */.
            else do:
                /*if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "T°tulo" at 1
                    "Saldo Inicial" to 99
                    "DÇbito" to 119
                    "CrÇdito" to 138
                    "Saldo Final" to 156 skip
                    "--------------------------------------------------------------------------------" at 1
                    "-----------------" to 99
                    "------------------" to 119
                    "------------------" to 138
                    "-----------------" to 156 skip.*/
            end /* else */.
        end /* if */.
        /*if  v_nom_prog = "Di†rio" 
        then do:
            if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                page stream s_1.
            put stream s_1 unformatted 
                v_des_tit_ctbl_balanct at 1 format "x(55)"
                tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_inic to 73 format "->,>>>,>>>,>>9.99"
                v_ind_cr_sdo_inic at 75 format "X(01)"
                tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 to 94 format "->>,>>>,>>>,>>9.99"
                tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 to 113 format "->>,>>>,>>>,>>9.99"
                tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim to 131 format "->,>>>,>>>,>>9.99"
                v_ind_cr_sdo_fim at 132 format "X(01)" skip.
        end.
        else do:
            if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                page stream s_1.
            put stream s_1 unformatted 
                v_des_tit_ctbl_balanct at 1 format "x(80)"
                tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_inic to 99 format "->>>>>,>>>,>>9.99"
                v_ind_cr_sdo_inic at 100 format "X(01)"
                tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 to 119 format "->>,>>>,>>>,>>9.99"
                tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 to 138 format "->>,>>>,>>>,>>9.99"
                tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim to 156 format "->>>>>,>>>,>>9.99"
                v_ind_cr_sdo_fim at 157 format "X(01)" skip.
        end.*/
        if  v_log_tot_balanct and 
           (v_log_estab_sum = no or v_log_unid_negoc_sum = no or v_log_ccusto_sum = no)
        then do:
            assign v_log_impr_tot = if  last-of(tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl)
                                    then yes
                                    else no.
            run pi_imprimir_tot_balanct_ccusto_sum (Input v_log_period_ctbl_ant_impr) /*pi_imprimir_tot_balanct_ccusto_sum*/.
        end /* if */.

        if  v_log_gerac_planilha = yes
        then do:
            assign v_des_tit_ctbl_balanct = v_des_tit_ctbl_aux.
            if  v_ind_cr_sdo_inic = "C" /*l_letra_c*/  then
                assign tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_inic = tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_inic * -1.
            if  v_ind_cr_sdo_fim = "C" /*l_letra_c*/  then
                assign tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim = tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim * -1.
            /*if (line-counter(s_planilha) + 1) > v_rpt_s_1_bottom then 
                page stream s_planilha.*/ 

            /* EXCEL AQUI */

            FIND FIRST cta_ctbl WHERE cta_ctbl.cod_cta_ctbl = tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl NO-LOCK NO-ERROR.

            FIND FIRST ems5.ccusto 
                     WHERE ems5.ccusto.cod_empresa = v_cod_unid_organ
                       AND ems5.ccusto.cod_plano_ccusto = tt_sdo_cta_ctbl_balanct.tta_cod_plano_ccusto
                       AND ems5.ccusto.cod_ccusto  = tt_sdo_cta_ctbl_balanct.tta_cod_ccusto NO-LOCK NO-ERROR. 

            IF v_cod_dwb_output = "Arquivo" THEN
            DO:
                
                PUT STREAM s-csv UNFORMATTED
                    TRIM(tt_sdo_cta_ctbl_balanct.tta_cod_estab)      ";"
                    TRIM(tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc) ";"
                    STRING(v_num_period_ctbl, "99")                  ";"
                    TRIM(tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl)   ";"
                    TRIM(tt_sdo_cta_ctbl_balanct.tta_cod_ccusto)     ";".


                IF AVAILABLE cta_ctbl THEN 
                    PUT STREAM s-csv UNFORMATTED TRIM(cta_ctbl.des_tit_ctbl) ";".
                ELSE
                    PUT STREAM s-csv UNFORMATTED "Nao Cadastrado;".


                IF AVAILABLE ems5.ccusto THEN 
                   PUT STREAM s-csv UNFORMATTED TRIM(ems5.ccusto.des_tit_ctbl) ";".
                ELSE
                   PUT STREAM s-csv UNFORMATTED " ;".
                 
                PUT STREAM s-csv UNFORMATTED
                    tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_inic FORMAT "->>,>>>,>>>,>>9.99" ";"
                    tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 FORMAT "->>,>>>,>>>,>>9.99" ";"
                    tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 FORMAT "->>,>>>,>>>,>>9.99" ";"
                    tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim  FORMAT "->>,>>>,>>>,>>9.99" SKIP.

            END.
            ELSE
            DO:

                chWorkSheet:Range("A" + STRING(i-linha)):numberformat = "@".
                chWorkSheet:Range("A" + STRING(i-linha)):VALUE = TRIM(tt_sdo_cta_ctbl_balanct.tta_cod_estab).
    
                chWorkSheet:Range("B" + STRING(i-linha)):numberformat = "@".
                chWorkSheet:Range("B" + STRING(i-linha)):VALUE = TRIM(tt_sdo_cta_ctbl_balanct.tta_cod_unid_negoc).
                
                chWorkSheet:Range("C" + STRING(i-linha)):numberformat = "@".
                chWorkSheet:Range("C" + STRING(i-linha)):VALUE = STRING(v_num_period_ctbl, "99").
                
                chWorkSheet:Range("D" + STRING(i-linha)):numberformat = "@".
                chWorkSheet:Range("D" + STRING(i-linha)):VALUE = TRIM(tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl).
                
                chWorkSheet:Range("E" + STRING(i-linha)):numberformat = "@".
                chWorkSheet:Range("E" + STRING(i-linha)):VALUE = TRIM(tt_sdo_cta_ctbl_balanct.tta_cod_ccusto).
    
                chWorkSheet:Range("F" + STRING(i-linha)):numberformat = "@".
                chWorkSheet:Range("F" + STRING(i-linha)):VALUE = IF AVAILABLE cta_ctbl 
                                                                 THEN TRIM(cta_ctbl.des_tit_ctbl)
                                                                 ELSE "Nao Cadastrado".
                
                chWorkSheet:Range("G" + STRING(i-linha)):numberformat = "@".
                chWorkSheet:Range("G" + STRING(i-linha)):VALUE = IF AVAILABLE ems5.ccusto
                                                                 THEN TRIM(ems5.ccusto.des_tit_ctbl)
                                                                 ELSE "".
    
                /* chWorkSheet:Range("F" + STRING(i-linha)):numberformat = "@". */
                chWorkSheet:Range("H" + STRING(i-linha)):VALUE = STRING(tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_inic, "->>,>>>,>>>,>>9.99").
                
                /* chWorkSheet:Range("G" + STRING(i-linha)):numberformat = "@". */
                chWorkSheet:Range("I" + STRING(i-linha)):VALUE = STRING(tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1, "->>,>>>,>>>,>>9.99").
                
                /* chWorkSheet:Range("H" + STRING(i-linha)):numberformat = "@". */
                chWorkSheet:Range("J" + STRING(i-linha)):VALUE = STRING(tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1, "->>,>>>,>>>,>>9.99").
                
                /* chWorkSheet:Range("I" + STRING(i-linha)):numberformat = "@". */
                chWorkSheet:Range("K" + STRING(i-linha)):VALUE = STRING(tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim, "->>,>>>,>>>,>>9.99").
    
                assign i-linha = i-linha + 1.

        END.

            /*put stream s_planilha unformatted  
                trim(v_des_tit_ctbl_balanct) at 1
                trim(v_des_tit_ctbl_compl) at 63 
                tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_inic to 102 format '->>,>>>,>>>,>>9.99' 
                v_cod_carac_lim at 104 format 'x(1)' 
                tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1 to 125 format '->>,>>>,>>>,>>9.99' 
                v_cod_carac_lim at 127 format 'x(1)' 
                tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1 to 148 format '->>,>>>,>>>,>>9.99' 
                v_cod_carac_lim at 150 format 'x(1)' 
                tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim to 171 format '->>,>>>,>>>,>>9.99'
                v_cod_carac_lim at 173 format 'x(1)' 
                fill(' ', 10 - length(trim(cta_ctbl.ind_espec_cta_ctbl))) + trim(cta_ctbl.ind_espec_cta_ctbl) to 185 format 'X(10)' skip.*/ 
        end /* if */.
        
        assign v_cod_cta_ctbl_ant = tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl.
        delete tt_sdo_cta_ctbl_balanct.
    end /* for sdo_cta_ctbl_block */.

    /* --- Classificado por Estrutura ---*/
    if  entry(1,dwb_rpt_param.cod_dwb_order) = "Estrutura" /*l_estrutura*/  then do:
        if trim(v_cod_cta_ctbl_ant) <> "" then do:
            for each tt_sdo_cta_ctbl_balanct_aux2
                break by tt_sdo_cta_ctbl_balanct_aux2.tta_cod_cta_ctbl:
                if v_cod_cta_ctbl_ant <> tt_sdo_cta_ctbl_balanct_aux2.tta_cod_cta_ctbl then do:
                    create tt_sdo_cta_ctbl_balanct.
                    buffer-copy tt_sdo_cta_ctbl_balanct_aux2 to tt_sdo_cta_ctbl_balanct
                    assign v_des_tit_ctbl_balanct =  tt_sdo_cta_ctbl_balanct_aux2.ttv_des_tit_ctbl_balanct.
                    if last-of(tt_sdo_cta_ctbl_balanct_aux2.tta_cod_cta_ctbl) then do:
                        if v_cod_cta_ctbl_ant <> tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl then
                            run pi_imprimir_sdo_cta_ctbl_balanct_2 (Input tt_sdo_cta_ctbl_balanct.tta_cod_cta_ctbl,
                                                                    Input no) /*pi_imprimir_sdo_cta_ctbl_balanct_2*/.
                    end.
                end.
                else do:
                    for each tt_sdo_cta_ctbl_balanct_aux
                    where tt_sdo_cta_ctbl_balanct_aux.tta_cod_cta_ctbl = tt_sdo_cta_ctbl_balanct_aux2.tta_cod_cta_ctbl:
                        delete tt_sdo_cta_ctbl_balanct_aux.
                    end.
                    delete tt_sdo_cta_ctbl_balanct_aux2.
                end.                                    
            end.
        end.
    end.
/*
    if (v_log_gerac_planilha = yes) then 
    do:
        /*output stream s_planilha close.*/
    
        ASSIGN v_cod_arq_excel = v_cod_arq_planilha + "/fgl900.csv".
    
        MESSAGE "v_cod_arq_excel " v_cod_arq_excel SKIP 
                "v_cod_arq_planilha " v_cod_arq_planilha SKIP 
            "v_cod_dwb_output " v_cod_dwb_output SKIP
            "valid handle " VALID-HANDLE(chExcelApplication)


            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        IF v_cod_dwb_output = "Terminal" THEN
        DO:
            chExcelApplication:VISIBLE = TRUE.
        END.
        ELSE IF v_cod_dwb_output = "Arquivo" THEN
        DO:
            chExcelApplication:Workbooks:Item(1):SaveAs(v_cod_arq_excel,,,,,,).
    
            chExcelApplication:QUIT().
        END.
        ELSE IF v_cod_dwb_output = "Impressora" THEN
        DO:
            chExcelApplication:worksheets:ITEM(1):SELECT. /* selecionar a(s) planilha(s) a ser(em) impressa(s) */
            chExcelApplication:VISIBLE = FALSE.
            chExcelApplication:ActiveWindow:SelectedSheets:Printout.
            chExcelApplication:APPLICATION:DISPLAYALERTS = FALSE.
    
            chExcelApplication:QUIT().
        END.
    
        /* release com-handles */
        RELEASE OBJECT chExcelApplication.      
        RELEASE OBJECT chWorkbook.
        RELEASE OBJECT chWorksheet.
    
    end. */
END PROCEDURE. /* pi_imprimir_sdo_cta_ctbl_balanct_2 */
/*****************************************************************************
** Procedure Interna.....: pi_retorna_saldos_contas_analiticas
** Descricao.............: pi_retorna_saldos_contas_analiticas
** Criado por............: src507
** Criado em.............: 26/10/2002 09:40:56
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_retorna_saldos_contas_analiticas:

    &if '{&emsbas_version}' >= '5.05' &then
    def buffer b_dwb_rpt_select_conta  for dwb_rpt_select.
    def buffer b_dwb_rpt_select_un     for dwb_rpt_select.
    def buffer b_dwb_rpt_select_ccusto for dwb_rpt_select.
    def buffer b_dwb_rpt_select_proj   for dwb_rpt_select.
    def buffer b_dwb_rpt_select_estab  for dwb_rpt_select.
    &endif


    assign v_log_method  = session:set-wait-state('general')
           v_num_niv_aux = 1
           v_num_seq     = 0.

    &if '{&emsbas_version}' >= '5.05' &then

    /* ** RETORNA SALDO DAS CONTAS ANALÈTICAS ***/
    for each b_dwb_rpt_select_conta no-lock
       where b_dwb_rpt_select_conta.cod_dwb_program = V_COD_DWB_PROGRAM
         and b_dwb_rpt_select_conta.cod_dwb_user    = v_cod_dwb_user
         and b_dwb_rpt_select_conta.cod_dwb_field   = "Conta Cont†bil" /*l_conta_contabil*/ 
         and b_dwb_rpt_select_conta.log_dwb_rule    = yes:
        for each b_dwb_rpt_select_un no-lock
           where b_dwb_rpt_select_un.cod_dwb_program = V_COD_DWB_PROGRAM
             and b_dwb_rpt_select_un.cod_dwb_user    = v_cod_dwb_user
             and b_dwb_rpt_select_un.cod_dwb_field   = "Unid Neg¢cio" /*l_Unid_Neg¢cio*/ 
             and b_dwb_rpt_select_un.log_dwb_rule    = yes:
            for each b_dwb_rpt_select_proj no-lock
               where b_dwb_rpt_select_proj.cod_dwb_program = V_COD_DWB_PROGRAM
                 and b_dwb_rpt_select_proj.cod_dwb_user    = v_cod_dwb_user
                 and b_dwb_rpt_select_proj.cod_dwb_field   = "Projeto" /*l_projeto*/ 
                 and b_dwb_rpt_select_proj.log_dwb_rule    = yes:
                for each b_dwb_rpt_select_estab no-lock
                   where b_dwb_rpt_select_estab.cod_dwb_program = V_COD_DWB_PROGRAM
                     and b_dwb_rpt_select_estab.cod_dwb_user    = v_cod_dwb_user
                     and b_dwb_rpt_select_estab.cod_dwb_field   =  "Estabelecimento" /*l_estabelecimento*/ 
                     and b_dwb_rpt_select_estab.log_dwb_rule    = yes:

                    assign v_num_seq_sdo = v_num_seq_sdo + 1.

                    find first tt_input_sdo
                         where tt_input_sdo.tta_cod_unid_organ_ini       = v_cod_unid_organ
                         and   tt_input_sdo.tta_cod_unid_organ_fim       = v_cod_unid_organ
                         and   tt_input_sdo.ttv_cod_unid_organ_orig_ini  = ""
                         and   tt_input_sdo.ttv_cod_unid_organ_orig_fim  = 'ZZZ'
                         and   tt_input_sdo.tta_cod_cenar_ctbl           = v_cod_cenar_ctbl
                         and   tt_input_sdo.tta_cod_finalid_econ         = v_cod_finalid_econ_bas
                         and   tt_input_sdo.tta_cod_plano_cta_ctbl       = v_cod_plano_cta_ctbl
                         and   tt_input_sdo.tta_cod_plano_ccusto         = ""
                         and   tt_input_sdo.tta_cod_cta_ctbl_inic        = b_dwb_rpt_select_conta.cod_dwb_initial
                         and   tt_input_sdo.tta_cod_cta_ctbl_fim         = b_dwb_rpt_select_conta.cod_dwb_final
                         and   tt_input_sdo.tta_cod_proj_financ_inic     = b_dwb_rpt_select_proj.cod_dwb_initial
                         and   tt_input_sdo.tta_cod_proj_financ_fim      = b_dwb_rpt_select_proj.cod_dwb_final
                         and   tt_input_sdo.tta_cod_estab_inic           = b_dwb_rpt_select_estab.cod_dwb_initial
                         and   tt_input_sdo.tta_cod_estab_fim            = b_dwb_rpt_select_estab.cod_dwb_final
                         and   tt_input_sdo.tta_cod_unid_negoc_inic      = b_dwb_rpt_select_un.cod_dwb_initial
                         and   tt_input_sdo.tta_cod_unid_negoc_fim       = b_dwb_rpt_select_un.cod_dwb_final
                         and   tt_input_sdo.tta_cod_ccusto_inic          = ""
                         and   tt_input_sdo.tta_cod_ccusto_fim           = ""
                         and   tt_input_sdo.ttv_cod_elimina_intercomp    = "Total" /*l_total*/ 
                         and   tt_input_sdo.ttv_log_espec_sdo_ccusto     = no
                         and   tt_input_sdo.ttv_log_restric_estab        = no
                         and   tt_input_sdo.ttv_ind_espec_cta            = "Todas" /*l_todas*/ 
                         and   tt_input_sdo.ttv_ind_espec_sdo_tot        = v_ind_tip_sdo
                         and   tt_input_sdo.ttv_cod_leitura              = 'for each'
                         and   tt_input_sdo.ttv_cod_condicao             = 'Igual'
                         and   tt_input_sdo.ttv_cod_cenar_orctario       = v_cod_cenar_orctario
                         and   tt_input_sdo.ttv_cod_unid_orctaria        = v_cod_unid_orctaria
                         and   tt_input_sdo.ttv_num_seq_orcto_ctbl       = v_num_seq_orcto_ctbl
                         and   tt_input_sdo.ttv_cod_vers_orcto_ctbl      = v_cod_vers_orcto_ctbl
                         no-lock no-error.
                    if not avail tt_input_sdo then do:
                        create tt_input_sdo.
                        assign tt_input_sdo.ttv_cod_seq                  = string(v_num_seq_sdo)
                               tt_input_sdo.tta_cod_unid_organ_ini       = v_cod_unid_organ
                               tt_input_sdo.tta_cod_unid_organ_fim       = v_cod_unid_organ
                               tt_input_sdo.ttv_cod_unid_organ_orig_ini  = ""
                               tt_input_sdo.ttv_cod_unid_organ_orig_fim  = 'ZZZ'
                               tt_input_sdo.tta_cod_cenar_ctbl           = v_cod_cenar_ctbl
                               tt_input_sdo.tta_cod_finalid_econ         = v_cod_finalid_econ_bas
                               tt_input_sdo.tta_cod_plano_cta_ctbl       = v_cod_plano_cta_ctbl
                               tt_input_sdo.tta_cod_plano_ccusto         = ""
                               tt_input_sdo.tta_cod_cta_ctbl_inic        = b_dwb_rpt_select_conta.cod_dwb_initial
                               tt_input_sdo.tta_cod_cta_ctbl_fim         = b_dwb_rpt_select_conta.cod_dwb_final
                               tt_input_sdo.tta_cod_proj_financ_inic     = b_dwb_rpt_select_proj.cod_dwb_initial
                               tt_input_sdo.tta_cod_proj_financ_fim      = b_dwb_rpt_select_proj.cod_dwb_final
                               tt_input_sdo.tta_cod_estab_inic           = b_dwb_rpt_select_estab.cod_dwb_initial
                               tt_input_sdo.tta_cod_estab_fim            = b_dwb_rpt_select_estab.cod_dwb_final
                               tt_input_sdo.tta_cod_unid_negoc_inic      = b_dwb_rpt_select_un.cod_dwb_initial
                               tt_input_sdo.tta_cod_unid_negoc_fim       = b_dwb_rpt_select_un.cod_dwb_final
                               tt_input_sdo.tta_cod_ccusto_inic          = ""
                               tt_input_sdo.tta_cod_ccusto_fim           = ""
                               tt_input_sdo.ttv_cod_elimina_intercomp    = "Total" /*l_total*/ 
                               tt_input_sdo.ttv_ind_espec_cta            = "Todas" /*l_todas*/ 
                               tt_input_sdo.ttv_log_espec_sdo_ccusto     = no
                               tt_input_sdo.ttv_log_restric_estab        = no
                               tt_input_sdo.ttv_ind_espec_sdo_tot        = v_ind_tip_sdo
                               tt_input_sdo.ttv_cod_leitura              = 'for each'
                               tt_input_sdo.ttv_cod_condicao             = 'Igual'
                               tt_input_sdo.ttv_cod_dat_sdo_ctbl_inic    = string(v_dat_fim_period_ctbl)
                               tt_input_sdo.ttv_cod_dat_sdo_ctbl_fim     = string(v_dat_fim_period_ctbl)
                               tt_input_sdo.ttv_cod_exerc_ctbl           = v_cod_exerc_ctbl
                               tt_input_sdo.ttv_cod_period_ctbl          = string(v_num_period_ctbl)                           
                               tt_input_sdo.ttv_cod_cta_ctbl_excec       = ""
                               tt_input_sdo.ttv_cod_ccusto_excec         = ""
                               tt_input_sdo.ttv_cod_proj_financ_excec    = ""
                               tt_input_sdo.ttv_cod_cenar_orctario       = v_cod_cenar_orctario
                               tt_input_sdo.ttv_cod_unid_orctaria        = v_cod_unid_orctaria
                               tt_input_sdo.ttv_num_seq_orcto_ctbl       = v_num_seq_orcto_ctbl
                               tt_input_sdo.ttv_cod_vers_orcto_ctbl      = v_cod_vers_orcto_ctbl.
                        if v_dat_fim_period_ctbl_ant <> ? then
                            assign tt_input_sdo.ttv_cod_dat_sdo_ctbl_inic = tt_input_sdo.ttv_cod_dat_sdo_ctbl_inic
                                                                          + chr(10) + string(v_dat_fim_period_ctbl_ant)
                                   tt_input_sdo.ttv_cod_dat_sdo_ctbl_fim  = tt_input_sdo.ttv_cod_dat_sdo_ctbl_fim
                                                                          + chr(10) + string(v_dat_fim_period_ctbl_ant)
                                   tt_input_sdo.ttv_cod_seq               = tt_input_sdo.ttv_cod_seq
                                                                          + chr(10) + string(v_num_seq_sdo)
                                   tt_input_sdo.ttv_cod_exerc_ctbl        = tt_input_sdo.ttv_cod_exerc_ctbl 
                                                                          + chr(10) + string(v_num_period_ctbl_aux)
                                   tt_input_sdo.ttv_cod_period_ctbl       = tt_input_sdo.ttv_cod_period_ctbl
                                                                          + chr(10) + v_cod_exerc_ctbl_aux.


                    end.

                    /* Busca Saldo ems5.ccusto */
                    if   v_ind_tip_sdo <> "Consolidaá∆o" /*l_consolidacao*/ 
                    and (v_log_sdo_ccusto = yes 
                    or   v_log_ccusto_sum = no) then do:
                        if v_log_sdo_ccusto = yes then do:
                            for each b_dwb_rpt_select_ccusto no-lock
                               where b_dwb_rpt_select_ccusto.cod_dwb_program = V_COD_DWB_PROGRAM
                                 and b_dwb_rpt_select_ccusto.cod_dwb_user    = v_cod_dwb_user
                                 and b_dwb_rpt_select_ccusto.cod_dwb_field   = "Centro Custo" /*l_centro_custo*/ 
                                 and b_dwb_rpt_select_ccusto.log_dwb_rule    = yes:

                                assign v_num_seq_sdo = v_num_seq_sdo + 1.

                                find first tt_input_sdo
                                     where tt_input_sdo.tta_cod_unid_organ_ini       = v_cod_unid_organ
                                     and   tt_input_sdo.tta_cod_unid_organ_fim       = v_cod_unid_organ
                                     and   tt_input_sdo.ttv_cod_unid_organ_orig_ini  = ""
                                     and   tt_input_sdo.ttv_cod_unid_organ_orig_fim  = 'ZZZ'
                                     and   tt_input_sdo.tta_cod_cenar_ctbl           = v_cod_cenar_ctbl
                                     and   tt_input_sdo.tta_cod_finalid_econ         = v_cod_finalid_econ_bas
                                     and   tt_input_sdo.tta_cod_plano_cta_ctbl       = v_cod_plano_cta_ctbl
                                     and   tt_input_sdo.tta_cod_plano_ccusto         = v_cod_plano_ccusto
                                     and   tt_input_sdo.tta_cod_cta_ctbl_inic        = b_dwb_rpt_select_conta.cod_dwb_initial
                                     and   tt_input_sdo.tta_cod_cta_ctbl_fim         = b_dwb_rpt_select_conta.cod_dwb_final
                                     and   tt_input_sdo.tta_cod_proj_financ_inic     = b_dwb_rpt_select_proj.cod_dwb_initial
                                     and   tt_input_sdo.tta_cod_proj_financ_fim      = b_dwb_rpt_select_proj.cod_dwb_final
                                     and   tt_input_sdo.tta_cod_estab_inic           = b_dwb_rpt_select_estab.cod_dwb_initial
                                     and   tt_input_sdo.tta_cod_estab_fim            = b_dwb_rpt_select_estab.cod_dwb_final
                                     and   tt_input_sdo.tta_cod_unid_negoc_inic      = b_dwb_rpt_select_un.cod_dwb_initial
                                     and   tt_input_sdo.tta_cod_unid_negoc_fim       = b_dwb_rpt_select_un.cod_dwb_final
                                     and   tt_input_sdo.tta_cod_ccusto_inic          = b_dwb_rpt_select_ccusto.cod_dwb_initial
                                     and   tt_input_sdo.tta_cod_ccusto_fim           = b_dwb_rpt_select_ccusto.cod_dwb_final
                                     and   tt_input_sdo.ttv_cod_elimina_intercomp    = "Total" /*l_total*/ 
                                     and   tt_input_sdo.ttv_log_espec_sdo_ccusto     = no
                                     and   tt_input_sdo.ttv_ind_espec_cta            = "Todas" /*l_todas*/  
                                     and   tt_input_sdo.ttv_log_restric_estab        = no
                                     and   tt_input_sdo.ttv_ind_espec_sdo_tot        = v_ind_tip_sdo
                                     and   tt_input_sdo.ttv_cod_leitura              = 'for each'
                                     and   tt_input_sdo.ttv_cod_condicao             = 'Igual'                                 
                                     and   tt_input_sdo.ttv_cod_cenar_orctario       = v_cod_cenar_orctario
                                     and   tt_input_sdo.ttv_cod_unid_orctaria        = v_cod_unid_orctaria
                                     and   tt_input_sdo.ttv_num_seq_orcto_ctbl       = v_num_seq_orcto_ctbl
                                     and   tt_input_sdo.ttv_cod_vers_orcto_ctbl      = v_cod_vers_orcto_ctbl
                                     no-lock no-error.
                                if not avail tt_input_sdo then do:
                                    create tt_input_sdo.
                                    assign tt_input_sdo.ttv_cod_seq                  = string(v_num_seq_sdo)
                                           tt_input_sdo.tta_cod_unid_organ_ini       = v_cod_unid_organ
                                           tt_input_sdo.tta_cod_unid_organ_fim       = v_cod_unid_organ
                                           tt_input_sdo.ttv_cod_unid_organ_orig_ini  = ""
                                           tt_input_sdo.ttv_cod_unid_organ_orig_fim  = 'ZZZ'
                                           tt_input_sdo.tta_cod_cenar_ctbl           = v_cod_cenar_ctbl
                                           tt_input_sdo.tta_cod_finalid_econ         = v_cod_finalid_econ_bas
                                           tt_input_sdo.tta_cod_plano_cta_ctbl       = v_cod_plano_cta_ctbl
                                           tt_input_sdo.tta_cod_plano_ccusto         = v_cod_plano_ccusto
                                           tt_input_sdo.tta_cod_cta_ctbl_inic        = b_dwb_rpt_select_conta.cod_dwb_initial
                                           tt_input_sdo.tta_cod_cta_ctbl_fim         = b_dwb_rpt_select_conta.cod_dwb_final
                                           tt_input_sdo.tta_cod_proj_financ_inic     = b_dwb_rpt_select_proj.cod_dwb_initial
                                           tt_input_sdo.tta_cod_proj_financ_fim      = b_dwb_rpt_select_proj.cod_dwb_final
                                           tt_input_sdo.tta_cod_estab_inic           = b_dwb_rpt_select_estab.cod_dwb_initial
                                           tt_input_sdo.tta_cod_estab_fim            = b_dwb_rpt_select_estab.cod_dwb_final
                                           tt_input_sdo.tta_cod_unid_negoc_inic      = b_dwb_rpt_select_un.cod_dwb_initial
                                           tt_input_sdo.tta_cod_unid_negoc_fim       = b_dwb_rpt_select_un.cod_dwb_final
                                           tt_input_sdo.tta_cod_ccusto_inic          = b_dwb_rpt_select_ccusto.cod_dwb_initial
                                           tt_input_sdo.tta_cod_ccusto_fim           = b_dwb_rpt_select_ccusto.cod_dwb_final
                                           tt_input_sdo.ttv_cod_elimina_intercomp    = "Total" /*l_total*/ 
                                           tt_input_sdo.ttv_log_espec_sdo_ccusto     = no
                                           tt_input_sdo.ttv_ind_espec_cta            = "Todas" /*l_todas*/ 
                                           tt_input_sdo.ttv_log_restric_estab        = no
                                           tt_input_sdo.ttv_ind_espec_sdo_tot        = v_ind_tip_sdo
                                           tt_input_sdo.ttv_cod_leitura              = 'for each'
                                           tt_input_sdo.ttv_cod_condicao             = 'Igual'
                                           tt_input_sdo.ttv_cod_dat_sdo_ctbl_inic    = string(v_dat_fim_period_ctbl)
                                           tt_input_sdo.ttv_cod_dat_sdo_ctbl_fim     = string(v_dat_fim_period_ctbl)
                                           tt_input_sdo.ttv_cod_cta_ctbl_excec       = ""
                                           tt_input_sdo.ttv_cod_ccusto_excec         = ""
                                           tt_input_sdo.ttv_cod_proj_financ_excec    = ""
                                           tt_input_sdo.ttv_cod_cenar_orctario       = v_cod_cenar_orctario
                                           tt_input_sdo.ttv_cod_unid_orctaria        = v_cod_unid_orctaria
                                           tt_input_sdo.ttv_num_seq_orcto_ctbl       = v_num_seq_orcto_ctbl
                                           tt_input_sdo.ttv_cod_vers_orcto_ctbl      = v_cod_vers_orcto_ctbl
                                           tt_input_sdo.ttv_cod_exerc_ctbl           = v_cod_exerc_ctbl
                                           tt_input_sdo.ttv_cod_period_ctbl          = string(v_num_period_ctbl).
                                    if v_dat_fim_period_ctbl_ant <> ? then
                                        assign tt_input_sdo.ttv_cod_dat_sdo_ctbl_inic = tt_input_sdo.ttv_cod_dat_sdo_ctbl_inic
                                                                                      + chr(10) + string(v_dat_fim_period_ctbl_ant)
                                               tt_input_sdo.ttv_cod_dat_sdo_ctbl_fim  = tt_input_sdo.ttv_cod_dat_sdo_ctbl_fim
                                                                                      + chr(10) + string(v_dat_fim_period_ctbl_ant)
                                               tt_input_sdo.ttv_cod_seq               = tt_input_sdo.ttv_cod_seq
                                                                                      + chr(10) + string(v_num_seq_sdo)
                                               tt_input_sdo.ttv_cod_exerc_ctbl        = tt_input_sdo.ttv_cod_exerc_ctbl 
                                                                                      + chr(10) + string(v_num_period_ctbl_aux)
                                               tt_input_sdo.ttv_cod_period_ctbl       = tt_input_sdo.ttv_cod_period_ctbl 
                                                                                      + chr(10) + v_cod_exerc_ctbl_aux.

                                end.
                            end.
                        end.
                        else do:
                            assign v_num_seq_sdo = v_num_seq_sdo + 1.

                            find first tt_input_sdo
                                 where tt_input_sdo.tta_cod_unid_organ_ini       = v_cod_unid_organ
                                 and   tt_input_sdo.tta_cod_unid_organ_fim       = v_cod_unid_organ
                                 and   tt_input_sdo.ttv_cod_unid_organ_orig_ini  = ""
                                 and   tt_input_sdo.ttv_cod_unid_organ_orig_fim  = 'ZZZ'
                                 and   tt_input_sdo.tta_cod_cenar_ctbl           = v_cod_cenar_ctbl
                                 and   tt_input_sdo.tta_cod_finalid_econ         = v_cod_finalid_econ_bas
                                 and   tt_input_sdo.tta_cod_plano_cta_ctbl       = v_cod_plano_cta_ctbl
                                 and   tt_input_sdo.tta_cod_plano_ccusto         = v_cod_plano_ccusto
                                 and   tt_input_sdo.tta_cod_cta_ctbl_inic        = b_dwb_rpt_select_conta.cod_dwb_initial
                                 and   tt_input_sdo.tta_cod_cta_ctbl_fim         = b_dwb_rpt_select_conta.cod_dwb_final
                                 and   tt_input_sdo.tta_cod_proj_financ_inic     = b_dwb_rpt_select_proj.cod_dwb_initial
                                 and   tt_input_sdo.tta_cod_proj_financ_fim      = b_dwb_rpt_select_proj.cod_dwb_final
                                 and   tt_input_sdo.tta_cod_estab_inic           = b_dwb_rpt_select_estab.cod_dwb_initial
                                 and   tt_input_sdo.tta_cod_estab_fim            = b_dwb_rpt_select_estab.cod_dwb_final
                                 and   tt_input_sdo.tta_cod_unid_negoc_inic      = b_dwb_rpt_select_un.cod_dwb_initial
                                 and   tt_input_sdo.tta_cod_unid_negoc_fim       = b_dwb_rpt_select_un.cod_dwb_final
                                 and   tt_input_sdo.tta_cod_ccusto_inic          = ""
                                 and   tt_input_sdo.tta_cod_ccusto_fim           = 'ZZZZZZZZZZZ'
                                 and   tt_input_sdo.ttv_cod_elimina_intercomp    = "Total" /*l_total*/ 
                                 and   tt_input_sdo.ttv_log_espec_sdo_ccusto     = no
                                 and   tt_input_sdo.ttv_ind_espec_cta            = "Todas" /*l_todas*/  
                                 and   tt_input_sdo.ttv_log_restric_estab        = no
                                 and   tt_input_sdo.ttv_ind_espec_sdo_tot        = v_ind_tip_sdo
                                 and   tt_input_sdo.ttv_cod_leitura              = 'for each'
                                 and   tt_input_sdo.ttv_cod_condicao             = 'Igual'                             
                                 and   tt_input_sdo.ttv_cod_cenar_orctario       = v_cod_cenar_orctario
                                 and   tt_input_sdo.ttv_cod_unid_orctaria        = v_cod_unid_orctaria
                                 and   tt_input_sdo.ttv_num_seq_orcto_ctbl       = v_num_seq_orcto_ctbl
                                 and   tt_input_sdo.ttv_cod_vers_orcto_ctbl      = v_cod_vers_orcto_ctbl
                                 no-lock no-error.
                            if not avail tt_input_sdo then do:
                                create tt_input_sdo.
                                assign tt_input_sdo.ttv_cod_seq                  = string(v_num_seq_sdo)
                                       tt_input_sdo.tta_cod_unid_organ_ini       = v_cod_unid_organ
                                       tt_input_sdo.tta_cod_unid_organ_fim       = v_cod_unid_organ
                                       tt_input_sdo.ttv_cod_unid_organ_orig_ini  = ""
                                       tt_input_sdo.ttv_cod_unid_organ_orig_fim  = 'ZZZ'
                                       tt_input_sdo.tta_cod_cenar_ctbl           = v_cod_cenar_ctbl
                                       tt_input_sdo.tta_cod_finalid_econ         = v_cod_finalid_econ_bas
                                       tt_input_sdo.tta_cod_plano_cta_ctbl       = v_cod_plano_cta_ctbl
                                       tt_input_sdo.tta_cod_plano_ccusto         = v_cod_plano_ccusto
                                       tt_input_sdo.tta_cod_cta_ctbl_inic        = b_dwb_rpt_select_conta.cod_dwb_initial
                                       tt_input_sdo.tta_cod_cta_ctbl_fim         = b_dwb_rpt_select_conta.cod_dwb_final
                                       tt_input_sdo.tta_cod_proj_financ_inic     = b_dwb_rpt_select_proj.cod_dwb_initial
                                       tt_input_sdo.tta_cod_proj_financ_fim      = b_dwb_rpt_select_proj.cod_dwb_final
                                       tt_input_sdo.tta_cod_estab_inic           = b_dwb_rpt_select_estab.cod_dwb_initial
                                       tt_input_sdo.tta_cod_estab_fim            = b_dwb_rpt_select_estab.cod_dwb_final
                                       tt_input_sdo.tta_cod_unid_negoc_inic      = b_dwb_rpt_select_un.cod_dwb_initial
                                       tt_input_sdo.tta_cod_unid_negoc_fim       = b_dwb_rpt_select_un.cod_dwb_final
                                       tt_input_sdo.tta_cod_ccusto_inic          = ""
                                       tt_input_sdo.tta_cod_ccusto_fim           = 'ZZZZZZZZZZZ'
                                       tt_input_sdo.ttv_cod_elimina_intercomp    = "Total" /*l_total*/ 
                                       tt_input_sdo.ttv_log_espec_sdo_ccusto     = no
                                       tt_input_sdo.ttv_ind_espec_cta            = "Todas" /*l_todas*/ 
                                       tt_input_sdo.ttv_log_restric_estab        = no
                                       tt_input_sdo.ttv_ind_espec_sdo_tot        = v_ind_tip_sdo
                                       tt_input_sdo.ttv_cod_leitura              = 'for each'
                                       tt_input_sdo.ttv_cod_condicao             = 'Igual'
                                       tt_input_sdo.ttv_cod_dat_sdo_ctbl_inic    = string(v_dat_fim_period_ctbl)
                                       tt_input_sdo.ttv_cod_dat_sdo_ctbl_fim     = string(v_dat_fim_period_ctbl)
                                       tt_input_sdo.ttv_cod_cta_ctbl_excec       = ""
                                       tt_input_sdo.ttv_cod_ccusto_excec         = ""
                                       tt_input_sdo.ttv_cod_proj_financ_excec    = ""
                                       tt_input_sdo.ttv_cod_cenar_orctario       = v_cod_cenar_orctario
                                       tt_input_sdo.ttv_cod_unid_orctaria        = v_cod_unid_orctaria
                                       tt_input_sdo.ttv_num_seq_orcto_ctbl       = v_num_seq_orcto_ctbl
                                       tt_input_sdo.ttv_cod_vers_orcto_ctbl      = v_cod_vers_orcto_ctbl
                                       tt_input_sdo.ttv_cod_exerc_ctbl           = v_cod_exerc_ctbl
                                       tt_input_sdo.ttv_cod_period_ctbl          = string(v_num_period_ctbl).
                                if v_dat_fim_period_ctbl_ant <> ? then
                                    assign tt_input_sdo.ttv_cod_dat_sdo_ctbl_inic = tt_input_sdo.ttv_cod_dat_sdo_ctbl_inic
                                                                                  + chr(10) + string(v_dat_fim_period_ctbl_ant)
                                           tt_input_sdo.ttv_cod_dat_sdo_ctbl_fim  = tt_input_sdo.ttv_cod_dat_sdo_ctbl_fim
                                                                                  + chr(10) + string(v_dat_fim_period_ctbl_ant)
                                           tt_input_sdo.ttv_cod_seq               = tt_input_sdo.ttv_cod_seq
                                                                                  + chr(10) + string(v_num_seq_sdo)
                                           tt_input_sdo.ttv_cod_exerc_ctbl        = tt_input_sdo.ttv_cod_exerc_ctbl 
                                                                                  + chr(10) + string(v_num_period_ctbl_aux)
                                           tt_input_sdo.ttv_cod_period_ctbl       = tt_input_sdo.ttv_cod_period_ctbl
                                                                                  + chr(10) + v_cod_exerc_ctbl_aux.
                            end.
                        end.
                    end.
                end.
            end.
        end.
    end.
    &endif
END PROCEDURE. /* pi_retorna_saldos_contas_analiticas */
/*****************************************************************************
** Procedure Interna.....: pi_cria_tt_input_leitura_sdo_faixa
** Descricao.............: pi_cria_tt_input_leitura_sdo_faixa
** Criado por............: src388
** Criado em.............: 24/04/2001 08:28:14
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_cria_tt_input_leitura_sdo_faixa:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_empresa
        as character
        format "x(3)"
        no-undo.
    def Input param p_cod_estab_ini
        as character
        format "x(3)"
        no-undo.
    def Input param p_cod_estab_fim
        as character
        format "x(3)"
        no-undo.
    def Input param p_cod_finalid_econ
        as character
        format "x(10)"
        no-undo.
    def Input param p_cod_plano_cta_ctbl
        as character
        format "x(8)"
        no-undo.
    def Input param p_cod_cta_ctbl_ini
        as character
        format "x(20)"
        no-undo.
    def Input param p_cod_cta_ctbl_fim
        as character
        format "x(20)"
        no-undo.
    def Input param p_cod_plano_ccusto
        as character
        format "x(8)"
        no-undo.
    def Input param p_cod_ccusto_ini
        as Character
        format "x(11)"
        no-undo.
    def Input param p_cod_ccusto_fim
        as Character
        format "x(11)"
        no-undo.
    def Input param p_cod_unid_negoc_ini
        as character
        format "x(3)"
        no-undo.
    def Input param p_cod_unid_negoc_fim
        as character
        format "x(3)"
        no-undo.
    def Input param p_cod_cenar_ctbl
        as character
        format "x(8)"
        no-undo.
    def Input param p_cod_proj_financ_ini
        as character
        format "x(20)"
        no-undo.
    def Input param p_cod_proj_financ_fim
        as character
        format "x(20)"
        no-undo.
    def Input param p_dat_lancto_ctbl_ini
        as date
        format "99/99/9999"
        no-undo.
    def Input param p_dat_lancto_ctbl_fim
        as date
        format "99/99/9999"
        no-undo.
    def Input param p_log_consid_apurac_restdo
        as logical
        format "Sim/N∆o"
        no-undo.
    def Input param p_cod_cond
        as character
        format "x(8)"
        no-undo.
    def Input param p_cod_leitura
        as character
        format "x(8)"
        no-undo.


    /************************* Parameter Definition End *************************/

    assign v_num_seq = v_num_seq + 1.


    /* Begin_Include: i_carrega_tt_input_leitura_sdo */
    CREATE tt_input_leitura_sdo.
    ASSIGN tt_input_leitura_sdo.ttv_cod_label    = "Empresa" /*l_empresa*/ 
           tt_input_leitura_sdo.ttv_des_conteudo = p_cod_empresa
           tt_input_leitura_sdo.ttv_num_seq_1    = v_num_seq.

    CREATE tt_input_leitura_sdo.
    ASSIGN tt_input_leitura_sdo.ttv_cod_label    = "Estabelecimento Inicial" /*l_estab_inicial*/ 
           tt_input_leitura_sdo.ttv_des_conteudo = p_cod_estab_ini
           tt_input_leitura_sdo.ttv_num_seq_1    = v_num_seq.

    CREATE tt_input_leitura_sdo.
    ASSIGN tt_input_leitura_sdo.ttv_cod_label    = "Estabelecimento Final" /*l_estab_final*/ 
           tt_input_leitura_sdo.ttv_des_conteudo = p_cod_estab_fim
           tt_input_leitura_sdo.ttv_num_seq_1    = v_num_seq.

    CREATE tt_input_leitura_sdo.
    ASSIGN tt_input_leitura_sdo.ttv_cod_label    = "Finalidade Econìmica" /*l_finalidade_economica*/ 
           tt_input_leitura_sdo.ttv_des_conteudo = p_cod_finalid_econ
           tt_input_leitura_sdo.ttv_num_seq_1    = v_num_seq. 

    CREATE tt_input_leitura_sdo.
    ASSIGN tt_input_leitura_sdo.ttv_cod_label    = "Plano Contas" /*l_plano_contas*/ 
           tt_input_leitura_sdo.ttv_des_conteudo = p_cod_plano_cta_ctbl
           tt_input_leitura_sdo.ttv_num_seq_1    = v_num_seq.

    CREATE tt_input_leitura_sdo. 
    ASSIGN tt_input_leitura_sdo.ttv_cod_label    = "Conta Contabil Inicial" /*l_cta_ctbl_inicial*/ 
           tt_input_leitura_sdo.ttv_des_conteudo = p_cod_cta_ctbl_ini
           tt_input_leitura_sdo.ttv_num_seq_1    = v_num_seq.

    CREATE tt_input_leitura_sdo. 
    ASSIGN tt_input_leitura_sdo.ttv_cod_label    = "Conta Contabil Final" /*l_cta_ctbl_final*/ 
           tt_input_leitura_sdo.ttv_des_conteudo = p_cod_cta_ctbl_fim
           tt_input_leitura_sdo.ttv_num_seq_1    = v_num_seq.

    CREATE tt_input_leitura_sdo.
    ASSIGN tt_input_leitura_sdo.ttv_cod_label    = "Plano Centro Custo" /*l_plan_ccusto*/ 
           tt_input_leitura_sdo.ttv_des_conteudo = p_cod_plano_ccusto
           tt_input_leitura_sdo.ttv_num_seq_1    = v_num_seq.

    CREATE tt_input_leitura_sdo.
    ASSIGN tt_input_leitura_sdo.ttv_cod_label    = "Centro Custo Inicial" /*l_ccusto_inicial*/ 
           tt_input_leitura_sdo.ttv_des_conteudo = p_cod_ccusto_ini
           tt_input_leitura_sdo.ttv_num_seq_1    = v_num_seq.

    CREATE tt_input_leitura_sdo.
    ASSIGN tt_input_leitura_sdo.ttv_cod_label    = "Centro Custo Final" /*l_ccusto_final*/ 
           tt_input_leitura_sdo.ttv_des_conteudo = p_cod_ccusto_fim
           tt_input_leitura_sdo.ttv_num_seq_1    = v_num_seq.

    CREATE tt_input_leitura_sdo. 
    ASSIGN tt_input_leitura_sdo.ttv_cod_label    = "UN Inicial" /*l_un_inicial*/ 
           tt_input_leitura_sdo.ttv_des_conteudo = p_cod_unid_negoc_ini
           tt_input_leitura_sdo.ttv_num_seq_1    = v_num_seq.

    CREATE tt_input_leitura_sdo. 
    ASSIGN tt_input_leitura_sdo.ttv_cod_label    = "UN Final" /*l_un_final*/ 
           tt_input_leitura_sdo.ttv_des_conteudo = p_cod_unid_negoc_fim
           tt_input_leitura_sdo.ttv_num_seq_1    = v_num_seq.

    CREATE tt_input_leitura_sdo.
    ASSIGN tt_input_leitura_sdo.ttv_cod_label    = "Cen†rio" /*l_cenario*/ 
           tt_input_leitura_sdo.ttv_des_conteudo = p_cod_cenar_ctbl
           tt_input_leitura_sdo.ttv_num_seq_1    = v_num_seq.

    CREATE tt_input_leitura_sdo. 
    ASSIGN tt_input_leitura_sdo.ttv_cod_label    = "Projeto Inicial" /*l_proj_inicial*/ 
           tt_input_leitura_sdo.ttv_des_conteudo = p_cod_proj_financ_ini
           tt_input_leitura_sdo.ttv_num_seq_1    = v_num_seq.

    CREATE tt_input_leitura_sdo. 
    ASSIGN tt_input_leitura_sdo.ttv_cod_label    = "Projeto Final" /*l_proj_final*/ 
           tt_input_leitura_sdo.ttv_des_conteudo = p_cod_proj_financ_fim
           tt_input_leitura_sdo.ttv_num_seq_1    = v_num_seq.

    CREATE tt_input_leitura_sdo.
    ASSIGN tt_input_leitura_sdo.ttv_cod_label    = "Data Inicial" /*l_data_inicial*/ 
           tt_input_leitura_sdo.ttv_des_conteudo = string(p_dat_lancto_ctbl_ini)
           tt_input_leitura_sdo.ttv_num_seq_1    = v_num_seq.

    CREATE tt_input_leitura_sdo.
    ASSIGN tt_input_leitura_sdo.ttv_cod_label    = "Data Final" /*l_data_final*/ 
           tt_input_leitura_sdo.ttv_des_conteudo = string(p_dat_lancto_ctbl_fim)
           tt_input_leitura_sdo.ttv_num_seq_1    = v_num_seq.

    CREATE tt_input_leitura_sdo.
    ASSIGN tt_input_leitura_sdo.ttv_cod_label    = "Apuraá∆o de Resultados" /*l_apuracao_resultados*/ 
           tt_input_leitura_sdo.ttv_des_conteudo = string(p_log_consid_apurac_restdo)
           tt_input_leitura_sdo.ttv_num_seq_1    = v_num_seq.

    CREATE tt_input_leitura_sdo.
    ASSIGN tt_input_leitura_sdo.ttv_cod_label    = "Leitura" /*l_leitura*/ 
           tt_input_leitura_sdo.ttv_des_conteudo = p_cod_leitura
           tt_input_leitura_sdo.ttv_num_seq_1    = v_num_seq.

    CREATE tt_input_leitura_sdo.
    ASSIGN tt_input_leitura_sdo.ttv_cod_label    = "Condiá∆o" /*l_cond*/ 
           tt_input_leitura_sdo.ttv_des_conteudo = p_cod_cond
           tt_input_leitura_sdo.ttv_num_seq_1    = v_num_seq.

    if index(program-name(1),'fgl715pa':U) <> 0 then do:
        CREATE tt_input_leitura_sdo.
        ASSIGN tt_input_leitura_sdo.ttv_cod_label    = 'Conta e ems5.ccusto' 
               tt_input_leitura_sdo.ttv_des_conteudo = ''
               tt_input_leitura_sdo.ttv_num_seq_1    = v_num_seq.
    end.
    /* End_Include: i_carrega_tt_input_leitura_sdo */
    .
END PROCEDURE. /* pi_cria_tt_input_leitura_sdo_faixa */
/*****************************************************************************
** Procedure Interna.....: pi_retorna_saldos_contas_analiticas_more
** Descricao.............: pi_retorna_saldos_contas_analiticas_more
** Criado por............: src507
** Criado em.............: 20/01/2003 21:04:31
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_retorna_saldos_contas_analiticas_more:

    &if '{&emsbas_version}' >= '5.05' &then
        if  can-find(first tt_input_sdo) then do:
            if  search("prgfin/fgl/fgl905zck.r") = ? and search("prgfin/fgl/fgl905zck.p") = ? then do:
                if  v_cod_dwb_user begins 'es_' then
                    return "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgfin/fgl/fgl905zck.p".
                else do:
                    message "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgfin/fgl/fgl905zck.p"
                           view-as alert-box error buttons ok.
                    return.
                end.
            end.
            else
                run prgfin/fgl/fgl905zck.p (Input 1,
                                        input table tt_input_sdo,
                                        Input table tt_input_leitura_sdo_demonst,
                                        output v_des_lista_estab,
                                        output table tt_log_erros) /*prg_api_retornar_sdo_ctbl_demonst*/.
            /* Elimina todos os registros da temp-table */
            EMPTY TEMP-TABLE tt_input_sdo.
            EMPTY TEMP-TABLE tt_input_leitura_sdo_demonst.

            /* ** ATUALIZA SALDOS SINT‚TICOS PARA AS CONTAS DA FAIXA DE SELE¯∞O ***/
            if not v_log_cta_ctbl_analit then /* S¢ Anal°ticas */
                RUN pi_carrega_sdo_cta_sintetica.
        end.
    &endif

END PROCEDURE. /* pi_retorna_saldos_contas_analiticas_more */
/*****************************************************************************
** Procedure Interna.....: pi_imprimir_tot_balanct_ccusto_sum
** Descricao.............: pi_imprimir_tot_balanct_ccusto_sum
** Criado por............: fut12183
** Criado em.............: 09/05/2003 15:56:47
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_imprimir_tot_balanct_ccusto_sum:

    /************************ Parameter Definition Begin ************************/

    def Input param p_log_period
        as logical
        format "Sim/N∆o"
        no-undo.


    /************************* Parameter Definition End *************************/

    if  p_log_period = no
    then do: /* --- N∆o imprime Per°odo Anterior ---*/
        if  v_ind_cr_sdo_inic <> "D" /*l_D*/  then
            assign v_val_sdo_cta_ctbl_inic = v_val_sdo_cta_ctbl_inic - tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_inic
                   v_val_sdo_cta_ctbl_fim  = v_val_sdo_cta_ctbl_fim  - tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim.
        else
            assign v_val_sdo_cta_ctbl_inic = v_val_sdo_cta_ctbl_inic + tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_inic
                   v_val_sdo_cta_ctbl_fim  = v_val_sdo_cta_ctbl_fim  + tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim.

        assign v_val_sdo_cta_ctbl_db   = v_val_sdo_cta_ctbl_db   + tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_db_1
               v_val_sdo_cta_ctbl_cr   = v_val_sdo_cta_ctbl_cr   + tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_cr_1.
    end /* if */.
    else do: /* --- Imprime Saldo Period Anterior ---*/
        if  v_ind_cr_sdo_inic <> "D" /*l_D*/  then
            assign v_val_sdo_cta_ctbl_inic     = v_val_sdo_cta_ctbl_inic - tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_inic
                   v_val_sdo_cta_ctbl_fim      = v_val_sdo_cta_ctbl_fim  - tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim
                   v_val_sdo_cta_ctbl_inic_ant = v_val_sdo_cta_ctbl_inic_ant - tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant
                   v_val_sdo_cta_ctbl_fim_ant  = v_val_sdo_cta_ctbl_fim_ant  - tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant.
        else
            assign v_val_sdo_cta_ctbl_inic     = v_val_sdo_cta_ctbl_inic     + tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_inic
                   v_val_sdo_cta_ctbl_fim      = v_val_sdo_cta_ctbl_fim      + tt_sdo_cta_ctbl_balanct.tta_val_sdo_ctbl_fim
                   v_val_sdo_cta_ctbl_inic_ant = v_val_sdo_cta_ctbl_inic_ant + tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_inic_ant
                   v_val_sdo_cta_ctbl_fim_ant  = v_val_sdo_cta_ctbl_fim_ant  + tt_sdo_cta_ctbl_balanct.ttv_val_sdo_ctbl_fim_ant.
    end /* else */.

    if  v_log_impr_tot
    then do:
        assign v_val_sdo_cta_tot_1 = v_val_sdo_cta_ctbl_inic
               v_val_sdo_cta_tot_2 = v_val_sdo_cta_ctbl_db
               v_val_sdo_cta_tot_3 = v_val_sdo_cta_ctbl_cr
               v_val_sdo_cta_tot_4 = v_val_sdo_cta_ctbl_inic_ant
               v_val_sdo_cta_tot_5 = v_val_sdo_cta_ctbl_fim_ant
               v_val_sdo_cta_tot_6 = v_val_sdo_cta_ctbl_fim.

        if  p_log_period = no
        then do:
            run pi_retornar_indic_cr_sdo (input-output v_val_sdo_cta_tot_1,
                                          output v_ind_cr_sdo_inic_1) /*pi_retornar_indic_cr_sdo*/.
            run pi_retornar_indic_cr_sdo (input-output v_val_sdo_cta_tot_6,
                                          output v_ind_cr_sdo_fim_1) /*pi_retornar_indic_cr_sdo*/.
            /*if  v_nom_prog = "Di†rio"
            then do:
                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "Totais  " at 48
                    v_val_sdo_cta_tot_1 to 73 format "->,>>>,>>>,>>9.99"
                    v_ind_cr_sdo_inic_1 at 75 format "X(01)"
                    v_val_sdo_cta_tot_2 to 94 format "->>,>>>,>>>,>>9.99"
                    v_val_sdo_cta_tot_3 to 113 format "->>,>>>,>>>,>>9.99"
                    v_val_sdo_cta_tot_6 to 131 format "->,>>>,>>>,>>9.99"
                    v_ind_cr_sdo_fim_1 at 132 format "X(01)" skip.
            end.
            else do:
                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "Totais  " at 74
                    v_val_sdo_cta_tot_1 to 99 format "->>>>>,>>>,>>9.99"
                    v_ind_cr_sdo_inic_1 at 100 format "X(01)"
                    v_val_sdo_cta_tot_2 to 119 format "->>,>>>,>>>,>>9.99"
                    v_val_sdo_cta_tot_3 to 138 format "->>,>>>,>>>,>>9.99"
                    v_val_sdo_cta_tot_6 to 156 format "->>>>>,>>>,>>9.99"
                    v_ind_cr_sdo_fim_1 at 157 format "X(01)" skip.
            end.*/
        end /* if */.
        else do:
            run pi_retornar_indic_cr_sdo (input-output v_val_sdo_cta_tot_1,
                                          output v_ind_cr_sdo_inic_1) /*pi_retornar_indic_cr_sdo*/.
            run pi_retornar_indic_cr_sdo (input-output v_val_sdo_cta_tot_6,
                                          output v_ind_cr_sdo_fim_1) /*pi_retornar_indic_cr_sdo*/.
            run pi_retornar_indic_cr_sdo (input-output v_val_sdo_cta_tot_4,
                                          output v_ind_cr_sdo_inic_2) /*pi_retornar_indic_cr_sdo*/.
            run pi_retornar_indic_cr_sdo (input-output v_val_sdo_cta_tot_5,
                                          output v_ind_cr_sdo_fim_2) /*pi_retornar_indic_cr_sdo*/.
            /*if  v_nom_prog = "Di†rio" 
            then do:
                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "Totais  " at 52
                    v_val_sdo_cta_tot_4 to 77 format "->,>>>,>>>,>>9.99"
                    v_ind_cr_sdo_inic_2 at 79 format "X(01)"
                    v_val_sdo_cta_tot_5 to 95 format "->>>,>>>,>>9.99"
                    v_ind_cr_sdo_fim_2 at 97 format "X(01)"
                    v_val_sdo_cta_tot_1 to 113 format "->>>,>>>,>>9.99"
                    v_ind_cr_sdo_inic_1 at 114 format "X(01)"
                    v_val_sdo_cta_tot_6 to 132 format "->,>>>,>>>,>>9.99"
                    v_ind_cr_sdo_fim_1 at 133 format "X(01)" skip.
            end.
            else do:
                if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "Totais  " at 74
                    v_val_sdo_cta_tot_4 to 99 format "->>>>>,>>>,>>9.99"
                    v_ind_cr_sdo_inic_2 at 100 format "X(01)"
                    v_val_sdo_cta_tot_5 to 118 format "->>>>>,>>>,>>9.99"
                    v_ind_cr_sdo_fim_2 at 119 format "X(01)"
                    v_val_sdo_cta_tot_1 to 137 format "->>>>>,>>>,>>9.99"
                    v_ind_cr_sdo_inic_1 at 138 format "X(01)"
                    v_val_sdo_cta_tot_6 to 156 format "->>>>>,>>>,>>9.99"
                    v_ind_cr_sdo_fim_1 at 157 format "X(01)" skip.
            end.*/
        end /* else */.

        assign v_val_sdo_cta_ctbl_inic     = 0
               v_val_sdo_cta_ctbl_db       = 0
               v_val_sdo_cta_ctbl_cr       = 0
               v_val_sdo_cta_ctbl_inic_ant = 0
               v_val_sdo_cta_ctbl_fim_ant  = 0
               v_val_sdo_cta_ctbl_fim      = 0.
    end /* if */.
END PROCEDURE. /* pi_imprimir_tot_balanct_ccusto_sum */
/*****************************************************************************
** Procedure Interna.....: pi_percent_update
** Descricao.............: pi_percent_update
** Criado por............: 
** Criado em.............: // 
** Alterado por..........: Augusto Guimar∆es
** Alterado em...........: 15/03/2011
*****************************************************************************/
PROCEDURE pi_percent_update:

    /************************ Parameter Definition Begin ************************/

    def Input param p_val_maximum
        as decimal
        format "->>,>>>,>>>,>>9.99"
        decimals 2
        no-undo.
    def Input param p_val_current_value
        as decimal
        format "->>,>>>,>>>,>>9.99"
        decimals 2
        no-undo.
    def Input param p_nom_frame_title
        as character
        format "x(32)"
        no-undo.


    /************************* Parameter Definition End *************************/

    if  p_val_maximum = 0
    then do:
       assign p_val_maximum = 1.
    end /* if */.

    assign v_des_percent_complete     = string(integer(p_val_current_value * 100 / p_val_maximum))
                                      + chr(32) + chr(37)
           v_des_percent_complete_fnd = v_des_percent_complete.

    if  not v_cod_dwb_user begins 'es_'
    then do:
        if  p_val_current_value = 0
        then do:
            assign v_des_percent_complete:width-pixels     in frame f_dlg_02_percent_update = 1
                   v_des_percent_complete:bgcolor          in frame f_dlg_02_percent_update = 1
                   v_des_percent_complete:fgcolor          in frame f_dlg_02_percent_update = 15
                   v_des_percent_complete:font             in frame f_dlg_02_percent_update = 1
                   rt_001:bgcolor                          in frame f_dlg_02_percent_update = 8
                   v_des_percent_complete_fnd:width-pixels in frame f_dlg_02_percent_update = 315
                   v_des_percent_complete_fnd:font         in frame f_dlg_02_percent_update = 1.
            if  p_nom_frame_title <> ""
            then do:
                assign frame f_dlg_02_percent_update:title = p_nom_frame_title.
            end /* if */.
            else do:
                assign frame f_dlg_02_percent_update:title = "Aguarde, em processamento..." /*l_aguarde_em_processamento*/ .
            end /* else */.
            view frame f_dlg_02_percent_update.
        end /* if */.
        else do:
            assign v_des_percent_complete:width-pixels = max(((315 * p_val_current_value)
                                                       / p_val_maximum), 1).
        end /* else */.
        display v_des_percent_complete
                v_des_percent_complete_fnd
                with frame f_dlg_02_percent_update.
        enable all with frame f_dlg_02_percent_update.
        process events.
    end /* if */.
    else do:
        run prgtec/btb/btb908ze.py (Input 1,
                                    Input v_des_percent_complete) /*prg_api_atualizar_ult_obj*/.
    end /* else */.



END PROCEDURE. /* pi_percent_update */


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
                            when "s_planilha" then
                                if c_at[i_ind] = "at" then
                                    put stream s_planilha unformatted c_aux at i_pos[i_ind].
                                else
                                    put stream s_planilha unformatted c_aux to i_pos[i_ind].
                        end.*/
            end.
        end.
        /*case p_stream:
        when "s_1" then
            put stream s_1 unformatted skip.
        when "s_planilha" then
            put stream s_planilha unformatted skip.
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
                "Programa Mensagem" c_prg_msg "n∆o encontrado."
                view-as alert-box error.
        return error.
    end.

    run value(c_prg_msg + ".p":U) (input c_action, input c_param).
    return return-value.
END PROCEDURE.  /* pi_messages */
/*******************  End of fnc_sdo_cta_ctbl_balanct_impr ******************/



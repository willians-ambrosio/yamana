/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa     for ems2cadme.empresa.
def buffer fornecedor  for ems5.fornecedor.
def buffer portador    for ems2cadme.portador.

{include/i-prgvrs.i ESAP004rp 2.06.00.001}  /*** 010010 ***/
{include/i_dbinst.i}
{utp/ut-glob.i}

 /*****************************************************************************
** 
**   Programa: ESAP004rp
** 
**   Data....: 05/11/2012
** 
**   Autor...: Bruno Bertulli (DSC)
**
**   Objetivo: 
**
******************************************************************************/

/*Definiá∆o temp-tables tt-param*/

/* ----------------------------[    DEFINIÄ«O DE TEMP-TABLE"S    ]--------------------------------*/
def new shared temp-table tt_integr_apb_lote_impl no-undo
    field tta_cod_estab                 as char format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_refer                 as char format "x(10)" label "Referància" column-label "Referància"
    field tta_cod_espec_docto           as char format "x(3)" label "EspÇcie Documento" column-label "EspÇcie"
    field tta_dat_transacao             as date format "99/99/9999" initial today label "Data Transaá∆o" column-label "Dat Transac"
    field tta_ind_origin_tit_ap         as char format "X(03)" initial "APB" label "Origem" column-label "Origem"
    field tta_cod_estab_ext             as char format "x(8)" label "Estabelecimento Exte" column-label "Estabelecimento Ext"
    field tta_val_tot_lote_impl_tit_ap  as dec  format ">>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Total  Movimento" column-label "Total Movto"
    field tta_cod_empresa               as char format "x(3)" label "Empresa" column-label "Empresa"
    field ttv_cod_empresa_ext           as char format "x(3)" label "C¢digo Empresa Ext" column-label "C¢d Emp Ext"
    field tta_cod_finalid_econ_ext      as char format "x(8)" label "Finalid Econ Externa" column-label "Finalidade Externa"
    field tta_cod_indic_econ            as char format "x(8)" label "Moeda" column-label "Moeda"
    index tt_lote_impl_tit_ap_integr_id    is primary unique
          tta_cod_estab                    ascending
          tta_cod_refer                    ascending
          tta_cod_estab_ext                ascending.

def new shared temp-table tt_integr_apb_impto_impl_pend no-undo
    field ttv_rec_integr_apb_item_lote   as recid format ">>>>>>9" help "Oculto"
    field ttv_rec_antecip_pef_pend       as recid format ">>>>>>9" help "Oculto"
    field tta_cod_pais                   as char  format "x(3)" label "Pa°s" column-label "Pa°s"
    field tta_cod_unid_federac           as char  format "x(3)" label "Unidade Federaá∆o" column-label "UF"
    field tta_cod_imposto                as char  format "x(5)" label "Imposto" column-label "Imposto"
    field tta_cod_classif_impto          as char  format "x(05)" initial "00000" label "Class Imposto" column-label "Class Imposto"
    field tta_ind_clas_impto             as char  format "X(14)" initial "Retido" label "Classe Imposto" column-label "Classe Imposto"
    field tta_cod_plano_cta_ctbl         as char  format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field tta_cod_cta_ctbl               as char  format "x(20)" label "Conta Cont†bil" column-label "Conta Cont†bil"
    field tta_cod_espec_docto            as char  format "x(3)" label "EspÇcie Documento" column-label "EspÇcie"
    field tta_cod_ser_docto              as char  format "x(3)" label "SÇrie Documento" column-label "SÇrie"
    field tta_cod_tit_ap                 as char  format "x(10)" label "T°tulo" column-label "T°tulo"
    field tta_cod_parcela                as char  format "x(02)" label "Parcela" column-label "Parc"
    field tta_val_rendto_tribut          as dec   format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Rendto Tribut†vel" column-label "Vl Rendto Tribut"
    field tta_val_deduc_inss             as dec   format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Deduá∆o Inss" column-label "Deduá∆o Inss"
    field tta_val_deduc_depend           as dec   format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Deduá∆o Dependentes" column-label "Deduá∆o Dependentes"
    field tta_val_deduc_pensao           as dec   format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Deducao Pens∆o" column-label "Deducao Pens∆o"
    field tta_val_outras_deduc_impto     as dec   format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Outras Deduá‰es" column-label "Outras Deduá‰es"
    field tta_val_base_liq_impto         as dec   format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Base L°quida Imposto" column-label "Base L°quida Imposto"
    field tta_val_aliq_impto             as dec   format ">9.99" decimals 2 initial 0.00 label "Al°quota" column-label "Aliq"
    field tta_val_impto_ja_recolhid      as dec   format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Imposto J† Recolhido" column-label "Imposto J† Recolhido"
    field tta_val_imposto                as dec   format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Imposto" column-label "Vl Imposto"
    field tta_dat_vencto_tit_ap          as date  format "99/99/9999" initial today label "Data Vencimento" column-label "Dt Vencto"
    field tta_cod_indic_econ             as char  format "x(8)" label "Moeda" column-label "Moeda"
    field tta_val_impto_indic_econ_impto as dec   format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Val Finalid Impto" column-label "Val Finalid Impto"
    field tta_des_text_histor            as char  format "x(2000)" label "Hist¢rico" column-label "Hist¢rico"
    field tta_cdn_fornec_favorec         as int   format ">>>,>>>,>>9" initial 0 label "Fornec Favorecido" column-label "Fornec Favorecido"
    field tta_val_deduc_faixa_impto      as dec   format ">,>>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Deducao" column-label "Valor Deduá∆o"
    field tta_num_id_tit_ap              as int   format "9999999999" initial 0 label "Token Tit AP" column-label "Token Tit AP" help "Oculto"
    field tta_num_id_movto_tit_ap        as int   format "9999999999" initial 0 label "Token Movto Tit AP" column-label "Id Tit AP" help "Oculto"
    field tta_num_id_movto_cta_corren    as int   format "9999999999" initial 0 label "ID Movto Conta" column-label "ID Movto Conta" help "Oculto"
    field tta_cod_pais_ext               as char  format "x(20)" label "Pa°s Externo" column-label "Pa°s Externo"
    field tta_cod_cta_ctbl_ext           as char  format "x(20)" label "Conta Contab Extern" column-label "Conta Contab Extern"
    field tta_cod_sub_cta_ctbl_ext       as char  format "x(15)" label "Sub Conta Externa" column-label "Sub Conta Externa"
    field ttv_cod_tip_fluxo_financ_ext   as char  format "x(12)" label "Tipo Fluxo Financ" column-label "Tipo Fluxo Ext"
    index tt_impto_impl_pend_ap_integr     is primary unique
          ttv_rec_integr_apb_item_lote     ascending
          tta_cod_pais                     ascending
          tta_cod_unid_federac             ascending
          tta_cod_imposto                  ascending
          tta_cod_classif_impto            ascending
    index tt_impto_impl_pend_ap_integr_ant is unique
          ttv_rec_antecip_pef_pend         ascending
          tta_cod_pais                     ascending
          tta_cod_unid_federac             ascending
          tta_cod_imposto                  ascending
          tta_cod_classif_impto            ascending.

def new shared temp-table tt_integr_apb_aprop_ctbl_pend no-undo
    field ttv_rec_integr_apb_item_lote  as recid format ">>>>>>9" help "Oculto"
    field ttv_rec_antecip_pef_pend      as recid format ">>>>>>9" help "Oculto"
    field ttv_rec_integr_apb_impto_pend as recid format ">>>>>>9" help "Oculto"
    field tta_cod_plano_cta_ctbl        as char  format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field tta_cod_cta_ctbl              as char  format "x(20)" label "Conta Cont†bil" column-label "Conta Cont†bil"
    field tta_cod_unid_negoc            as char  format "x(3)" label "Unid Neg¢cio" column-label "Un Neg"
    field tta_cod_plano_ccusto          as char  format "x(8)" label "Plano Centros Custo" column-label "Plano Centros Custo"
    field tta_cod_ccusto                as char  format "x(11)" label "Centro Custo" column-label "Centro Custo"
    field tta_cod_tip_fluxo_financ      as char  format "x(12)" label "Tipo Fluxo Financ" column-label "Tipo Fluxo Financ"
    field tta_val_aprop_ctbl            as dec   format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Aprop Ctbl" column-label "Vl Aprop Ctbl"
    field tta_cod_pais                  as char  format "x(3)" label "Pa°s" column-label "Pa°s"
    field tta_cod_unid_federac          as char  format "x(3)" label "Unidade Federaá∆o" column-label "UF"
    field tta_cod_imposto               as char  format "x(5)" label "Imposto" column-label "Imposto"
    field tta_cod_classif_impto         as char  format "x(05)" initial "00000" label "Class Imposto" column-label "Class Imposto"
    field ttv_cod_tip_fluxo_financ_ext  as char  format "x(12)" label "Tipo Fluxo Financ" column-label "Tipo Fluxo Financ"
    field tta_cod_cta_ctbl_ext          as char  format "x(20)" label "Conta Contab Extern" column-label "Conta Contab Extern"
    field tta_cod_sub_cta_ctbl_ext      as char  format "x(15)" label "Sub Conta Externa" column-label "Sub Conta Externa"
    field tta_cod_ccusto_ext            as char  format "x(8)" label "Centro Custo Externo" column-label "CCusto Externo"
    field tta_cod_unid_negoc_ext        as char  format "x(8)" label "Unid Neg¢cio Externa" column-label "Unid Neg¢cio Externa"
    index tt_aprop_ctbl_pend_ap_integr_ant
          ttv_rec_antecip_pef_pend         ascending
          ttv_rec_integr_apb_impto_pend    ascending
          tta_cod_plano_cta_ctbl           ascending
          tta_cod_cta_ctbl                 ascending
          tta_cod_unid_negoc               ascending
          tta_cod_plano_ccusto             ascending
          tta_cod_ccusto                   ascending
          tta_cod_tip_fluxo_financ         ascending
    index tt_aprop_ctbl_pend_ap_integr_id
          ttv_rec_integr_apb_item_lote     ascending
          ttv_rec_integr_apb_impto_pend    ascending
          tta_cod_plano_cta_ctbl           ascending
          tta_cod_cta_ctbl                 ascending
          tta_cod_unid_negoc               ascending
          tta_cod_plano_ccusto             ascending
          tta_cod_ccusto                   ascending
          tta_cod_tip_fluxo_financ         ascending
    .

def new shared temp-table tt_integr_apb_item_lote_impl no-undo
    field ttv_rec_integr_apb_lote_impl  as recid format ">>>>>>9"
    field tta_num_seq_refer             as int   format ">>>9" initial 0 label "Sequància" column-label "Seq"
    field tta_cdn_fornecedor            as int   format ">>>,>>>,>>9" initial 0 label "Fornecedor" column-label "Fornecedor"
    field tta_cod_espec_docto           as char  format "x(3)" label "EspÇcie Documento" column-label "EspÇcie"
    field tta_cod_ser_docto             as char  format "x(3)" label "SÇrie Documento" column-label "SÇrie"
    field tta_cod_tit_ap                as char  format "x(10)" label "T°tulo" column-label "T°tulo"
    field tta_cod_parcela               as char  format "x(02)" label "Parcela" column-label "Parc"
    field tta_dat_emis_docto            as date  format "99/99/9999" initial today label "Data  Emiss∆o" column-label "Dt Emiss∆o"
    field tta_dat_vencto_tit_ap         as date  format "99/99/9999" initial today label "Data Vencimento" column-label "Dt Vencto"
    field tta_dat_prev_pagto            as date  format "99/99/9999" initial today label "Data Prevista Pgto" column-label "Dt Prev Pagto"
    field tta_dat_desconto              as date  format "99/99/9999" initial ? label "Data Desconto" column-label "Dt Descto"
    field tta_cod_indic_econ            as char  format "x(8)" label "Moeda" column-label "Moeda"
    field tta_val_tit_ap                as dec   format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor T°tulo" column-label "Valor T°tulo"
    field tta_val_desconto              as dec   format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Desconto" column-label "Valor Desconto"
    field tta_val_perc_desc             as dec   format ">9.9999" decimals 4 initial 0 label "Percentual Desconto" column-label "Perc Descto"
    field tta_num_dias_atraso           as int   format ">9" initial 0 label "Dias Atraso" column-label "Dias Atr"
    field tta_val_juros_dia_atraso      as dec   format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Juro" column-label "Vl Juro"
    field tta_val_perc_juros_dia_atraso as dec   format ">9.999999" decimals 6 initial 00.00 label "Perc Jur Dia Atraso" column-label "Perc Dia"
    field tta_val_perc_multa_atraso     as dec   format ">9.99" decimals 2 initial 00.00 label "Perc Multa Atraso" column-label "Multa Atr"
    field tta_cod_portador              as char  format "x(5)" label "Portador" column-label "Portador"
    field tta_cod_apol_seguro           as char  format "x(12)" label "Ap¢lice Seguro" column-label "Apolice Seguro"
    field tta_cod_seguradora            as char  format "x(8)" label "Seguradora" column-label "Seguradora"
    field tta_cod_arrendador            as char  format "x(6)" label "Arrendador" column-label "Arrendador"
    field tta_cod_contrat_leas          as char  format "x(12)" label "Contrato Leasing" column-label "Contr Leas"
    field tta_des_text_histor           as char  format "x(2000)" label "Hist¢rico" column-label "Hist¢rico"
    field tta_num_id_tit_ap             as int   format "9999999999" initial 0 label "Token Tit AP" column-label "Token Tit AP"
    field tta_num_id_movto_tit_ap       as int   format "9999999999" initial 0 label "Token Movto Tit AP" column-label "Id Tit AP"
    field tta_num_id_movto_cta_corren   as int   format "9999999999" initial 0 label "ID Movto Conta" column-label "ID Movto Conta"
    field ttv_qtd_parc_tit_ap           as dec   format ">>9" initial 1 label "Quantidade Parcelas" column-label "Quantidade Parcelas"
    field ttv_num_dias                  as int   format ">>>>,>>9" label "N£mero de Dias" column-label "N£mero de Dias"
    field ttv_ind_vencto_previs         as char  format "X(4)" initial "Màs" label "C†lculo Vencimento" column-label "C†lculo Vencimento"
    field ttv_log_gerad                 as log   format "Sim/N∆o" initial no
    field tta_cod_finalid_econ_ext      as char  format "x(8)" label "Finalid Econ Externa" column-label "Finalidade Externa"
    field tta_cod_portad_ext            as char  format "x(8)" label "Portador Externo" column-label "Portador Externo"
    field tta_cod_modalid_ext           as char  format "x(8)" label "Modalidade Externa" column-label "Modalidade Externa"
    field tta_cod_cart_bcia             as char  format "x(3)" label "Carteira" column-label "Carteira"
    field tta_cod_forma_pagto           as char  format "x(3)" label "Forma Pagamento" column-label "F Pagto"
    index tt_item_lote_impl_ap_integr_id   is primary unique
          ttv_rec_integr_apb_lote_impl     ascending
          tta_num_seq_refer                ascending
    .

/* Definiá∆o da TEMP-TABLE 'tt_integr_apb_item_lote_impl_3' acrescentando os novos campos da API evolu°da **************************************************/
def temp-table tt_integr_apb_item_lote_impl_3 no-undo
    field ttv_rec_integr_apb_lote_impl  as recid format ">>>>>>9"
    field tta_num_seq_refer             as int   format ">>>9" initial 0 label "Sequància" column-label "Seq"
    field tta_cdn_fornecedor            as int   format ">>>,>>>,>>9" initial 0 label "Fornecedor" column-label "Fornecedor"
    field tta_cod_espec_docto           as char  format "x(3)" label "EspÇcie Documento" column-label "EspÇcie"
    field tta_cod_ser_docto             as char  format "x(3)" label "SÇrie Documento" column-label "SÇrie"
    field tta_cod_tit_ap                as char  format "x(10)" label "T°tulo" column-label "T°tulo"
    field tta_cod_parcela               as char  format "x(02)" label "Parcela" column-label "Parc"
    field tta_dat_emis_docto            as date  format "99/99/9999" initial today label "Data  Emiss∆o" column-label "Dt Emiss∆o"
    field tta_dat_vencto_tit_ap         as date  format "99/99/9999" initial today label "Data Vencimento" column-label "Dt Vencto"
    field tta_dat_prev_pagto            as date  format "99/99/9999" initial today label "Data Prevista Pgto" column-label "Dt Prev Pagto"
    field tta_dat_desconto              as date  format "99/99/9999" initial ? label "Data Desconto" column-label "Dt Descto"
    field tta_cod_indic_econ            as char  format "x(8)" label "Moeda" column-label "Moeda"
    field tta_val_tit_ap                as dec   format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor T°tulo" column-label "Valor T°tulo"
    field tta_val_desconto              as dec   format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Desconto" column-label "Valor Desconto"
    field tta_val_perc_desc             as dec   format ">9.9999" decimals 4 initial 0 label "Percentual Desconto" column-label "Perc Descto"
    field tta_num_dias_atraso           as int   format ">9" initial 0 label "Dias Atraso" column-label "Dias Atr"
    field tta_val_juros_dia_atraso      as dec   format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Juro" column-label "Vl Juro"
    field tta_val_perc_juros_dia_atraso as dec   format ">9.999999" decimals 6 initial 00.00 label "Perc Jur Dia Atraso" column-label "Perc Dia"
    field tta_val_perc_multa_atraso     as dec   format ">9.99" decimals 2 initial 00.00 label "Perc Multa Atraso" column-label "Multa Atr"
    field tta_cod_portador              as char  format "x(5)" label "Portador" column-label "Portador"
    field tta_cod_apol_seguro           as char  format "x(12)" label "Ap¢lice Seguro" column-label "Apolice Seguro"
    field tta_cod_seguradora            as char  format "x(8)" label "Seguradora" column-label "Seguradora"
    field tta_cod_arrendador            as char  format "x(6)" label "Arrendador" column-label "Arrendador"
    field tta_cod_contrat_leas          as char  format "x(12)" label "Contrato Leasing" column-label "Contr Leas"
    field tta_des_text_histor           as char  format "x(2000)" label "Hist¢rico" column-label "Hist¢rico"
    field tta_num_id_tit_ap             as int   format "9999999999" initial 0 label "Token Tit AP" column-label "Token Tit AP"
    field tta_num_id_movto_tit_ap       as int   format "9999999999" initial 0 label "Token Movto Tit AP" column-label "Id Tit AP"
    field tta_num_id_movto_cta_corren   as int   format "9999999999" initial 0 label "ID Movto Conta" column-label "ID Movto Conta"
    field ttv_qtd_parc_tit_ap           as dec   format ">>9" initial 1 label "Quantidade Parcelas" column-label "Quantidade Parcelas"
    field ttv_num_dias                  as int   format ">>>>,>>9" label "N£mero de Dias" column-label "N£mero de Dias"
    field ttv_ind_vencto_previs         as char  format "X(4)" initial "Màs" label "C†lculo Vencimento" column-label "C†lculo Vencimento"
    field ttv_log_gerad                 as log   format "Sim/N∆o" initial no
    field tta_cod_finalid_econ_ext      as char  format "x(8)" label "Finalid Econ Externa" column-label "Finalidade Externa"
    field tta_cod_portad_ext            as char  format "x(8)" label "Portador Externo" column-label "Portador Externo"
    field tta_cod_modalid_ext           as char  format "x(8)" label "Modalidade Externa" column-label "Modalidade Externa"
    field tta_cod_cart_bcia             as char  format "x(3)" label "Carteira" column-label "Carteira"
    field tta_cod_forma_pagto           as char  format "x(3)" label "Forma Pagamento" column-label "F Pagto"
    field tta_val_cotac_indic_econ      as dec   format ">>>>,>>9.9999999999" decimals 10 initial 0 label "Cotaá∆o" column-label "Cotaá∆o"
    field ttv_num_ord_invest            as int   format ">>>>>,>>9" initial 0 label "Ordem Investimento" column-label "Ordem Invest"
    field tta_cod_livre_1               as char  format "x(100)" label "Livre 1" column-label "Livre 1"
    field tta_cod_livre_2               as char  format "x(100)" label "Livre 2" column-label "Livre 2"
    field tta_dat_livre_1               as date  format "99/99/9999" initial ? label "Livre 1" column-label "Livre 1"
    field tta_dat_livre_2               as date  format "99/99/9999" initial ? label "Livre 2" column-label "Livre 2"
    field tta_log_livre_1               as log   format "Sim/N∆o" initial no label "Livre 1" column-label "Livre 1"
    field tta_log_livre_2               as log   format "Sim/N∆o" initial no label "Livre 2" column-label "Livre 2"
    field tta_num_livre_1               as int   format ">>>>>9" initial 0 label "Livre 1" column-label "Livre 1"
    field tta_num_livre_2               as int   format ">>>>>9" initial 0 label "Livre 2" column-label "Livre 2"
    field tta_val_livre_1               as dec   format ">>>,>>>,>>9.9999" decimals 4 initial 0 label "Livre 1" column-label "Livre 1"
    field tta_val_livre_2               as dec   format ">>>,>>>,>>9.9999" decimals 4 initial 0 label "Livre 2" column-label "Livre 2"
    field ttv_rec_integr_apb_item_lote  as recid format ">>>>>>9"
    field ttv_val_1099                  as dec   format "->>,>>>,>>>,>>9.99" decimals 2
    field tta_cod_tax_ident_number      as char  format "x(15)" label "Tax Id Number" column-label "Tax Id Number"
    field tta_ind_tip_trans_1099        as char  format "X(50)" initial "Rents" label "Tipo Transacao 1099" column-label "Tipo Transacao 1099"
    index tt_item_lote_impl_ap_integr_id   is primary unique
          ttv_rec_integr_apb_lote_impl     ascending
          tta_num_seq_refer                ascending
    .

def new shared temp-table tt_integr_apb_abat_antecip_vouc no-undo
    field ttv_rec_integr_apb_item_lote as recid format ">>>>>>9"
    field tta_cod_estab                as char  format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_espec_docto          as char  format "x(3)" label "EspÇcie Documento" column-label "EspÇcie"
    field tta_cod_ser_docto            as char  format "x(3)" label "SÇrie Documento" column-label "SÇrie"
    field tta_cdn_fornecedor           as int   format ">>>,>>>,>>9" initial 0 label "Fornecedor" column-label "Fornecedor"
    field tta_cod_tit_ap               as char  format "x(10)" label "T°tulo" column-label "T°tulo"
    field tta_cod_parcela              as char  format "x(02)" label "Parcela" column-label "Parc"
    field tta_val_abat_tit_ap          as dec   format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Abatimento" column-label "Vl Abatimento"
    index tt_integr_apb_abat_antecip_vouc  is primary unique
          ttv_rec_integr_apb_item_lote     ascending
          tta_cod_estab                    ascending
          tta_cod_espec_docto              ascending
          tta_cod_ser_docto                ascending
          tta_cdn_fornecedor               ascending
          tta_cod_tit_ap                   ascending
          tta_cod_parcela                  ascending
    .

def new shared temp-table tt_integr_apb_abat_prev_provis no-undo
    field ttv_rec_integr_apb_item_lote as recid format ">>>>>>9"
    field ttv_rec_antecip_pef_pend     as recid format ">>>>>>9"
    field tta_cod_estab                as char  format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_espec_docto          as char  format "x(3)" label "EspÇcie Documento" column-label "EspÇcie"
    field tta_cod_ser_docto            as char  format "x(3)" label "SÇrie Documento" column-label "SÇrie"
    field tta_cdn_fornecedor           as int   format ">>>,>>>,>>9" initial 0 label "Fornecedor" column-label "Fornecedor"
    field tta_cod_tit_ap               as char  format "x(10)" label "T°tulo" column-label "T°tulo"
    field tta_cod_parcela              as char  format "x(02)" label "Parcela" column-label "Parc"
    field tta_val_abat_tit_ap          as dec   format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Abatimento" column-label "Vl Abatimento"
    index tt_integr_apb_abat_prev          is unique
          ttv_rec_antecip_pef_pend         ascending
          tta_cod_estab                    ascending
          tta_cod_espec_docto              ascending
          tta_cod_ser_docto                ascending
          tta_cdn_fornecedor               ascending
          tta_cod_tit_ap                   ascending
          tta_cod_parcela                  ascending
    index tt_integr_apb_abat_prev_provis   is primary unique
          ttv_rec_integr_apb_item_lote     ascending
          tta_cod_estab                    ascending
          tta_cod_espec_docto              ascending
          tta_cod_ser_docto                ascending
          tta_cdn_fornecedor               ascending
          tta_cod_tit_ap                   ascending
          tta_cod_parcela                  ascending
    .

def new shared temp-table tt_integr_apb_aprop_relacto no-undo
    field ttv_rec_integr_apb_relacto_pend as recid format ">>>>>>9"
    field tta_cod_plano_cta_ctbl          as char  format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field tta_cod_cta_ctbl                as char  format "x(20)" label "Conta Cont†bil" column-label "Conta Cont†bil"
    field tta_val_aprop_ctbl              as dec   format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Aprop Ctbl" column-label "Vl Aprop Ctbl"
    field tta_ind_tip_aprop_ctbl          as char  format "x(30)" initial "Saldo" label "Tipo Aprop Ctbl" column-label "Tipo Aprop Ctbl"
    index tt_integr_apb_aprop_relacto      is primary unique
          ttv_rec_integr_apb_relacto_pend  ascending
          tta_cod_plano_cta_ctbl           ascending
          tta_cod_cta_ctbl                 ascending
    .

def new shared temp-table tt_integr_apb_relacto_pend no-undo
    field ttv_rec_integr_apb_item_lote as recid format ">>>>>>9"
    field tta_cod_estab_tit_ap_pai     as char  format "x(3)" label "Estab Tit Pai" column-label "Estab Tit Pai"
    field tta_num_id_tit_ap_pai        as int   format "9999999999" initial 0 label "Token" column-label "Token"
    field tta_val_relacto_tit_ap       as dec   format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor" column-label "Valor"
    field tta_ind_motiv_acerto_val     as char  format "X(12)" initial "Alteraá∆o" label "Motivo Acerto Valor" column-label "Motivo Acerto Valor"
    index tt_integr_apb_relacto_pend       is primary unique
          ttv_rec_integr_apb_item_lote     ascending
          tta_cod_estab_tit_ap_pai         ascending
          tta_num_id_tit_ap_pai            ascending
    .

def new shared temp-table tt_log_erros_atualiz no-undo
    field tta_cod_estab                    as char format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_refer                    as char format "x(10)" label "Referància" column-label "Referància"
    field tta_num_seq_refer                as int  format ">>>9" initial 0 label "Sequància" column-label "Seq"
    field ttv_num_mensagem                 as int  format ">>>>,>>9" label "N£mero" column-label "N£mero Mensagem"
    field ttv_des_msg_erro                 as char format "x(60)" label "Mensagem Erro" column-label "Inconsistància"
    field ttv_des_msg_ajuda                as char format "x(200)" label "Mensagem Ajuda" column-label "Mensagem Ajuda"
    field ttv_ind_tip_relacto              as char format "X(15)" label "Tipo Relacionamento" column-label "Tipo Relac"
    field ttv_num_relacto                  as int  format ">>>>,>>9" label "Relacionamento" column-label "Relacionamento"
    .

/* Temp-table 1: conter† as informaá‰es das antecipaá‰es ou PEFs a serem integradas com o APB. */
def temp-table tt_integr_apb_antecip_pef_pend no-undo
    field tta_cod_empresa           as char  format "x(3)" label "Empresa" column-label "Empresa"
    field tta_cod_estab             as char  format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_refer             as char  format "x(10)" label "Referància" column-label "Referància"
    field tta_cod_espec_docto       as char  format "x(3)" label "EspÇcie Documento" column-label "EspÇcie"
    field tta_cod_ser_docto         as char  format "x(3)" label "SÇrie Documento" column-label "SÇrie"
    field tta_cdn_fornecedor        as int   format ">>>,>>>,>>9" init 0 label "Fornecedor" column-label "Fornecedor"
    field tta_cod_tit_ap            as char  format "x(10)" label "T°tulo" column-label "T°tulo"
    field tta_cod_parcela           as char  format "x(2)" label "Parcela" column-label "Parc"
    field tta_cod_portador          as char  format "x(5)" label "Portador" column-label "Portador"
    field tta_cod_indic_econ        as char  format "x(8)" label "Moeda" column-label "Moeda"
    field tta_num_talon_cheq        as int   format ">>>,>>>,>>9" init 0 label "Talon†rio Cheques" column-label "Talon†rio Cheques"
    field tta_num_cheque            as int   format ">>>>,>>>,>>9" init ? label "Num Cheque" column-label "Num Cheque"
    field tta_ind_favorec_cheq      as char  format "x(15)" init "Portador" label "Favorecido" column-label "Favorecido"
    field tta_nom_favorec_cheq      as char  format "x(40)" label "Nome Favorecido" column-label "Nome Favorecido"
    field tta_val_tit_ap            as dec   format "->>>,>>>,>>9.99" decimals 2 init 0 label "Valor T°tulo" column-label "Valor T°tulo"
    field tta_val_cotac_indic_econ  as dec   format ">>>>,>>9.9999999999" decimals 10 init 0 label "Cotaá∆o" column-label "Cotaá∆o"
    field tta_dat_emis_docto        as date  format "99/99/9999" init today label "Data  Emiss∆o" column-label "Dt Emiss∆o"
    field tta_dat_vencto_tit_ap     as date  format "99/99/9999" init today label "Data Vencimento" column-label "Dt Vencto"
    field tta_ind_tip_refer         as char  format "x(22)" label "Tipo Referància" column-label "Tipo"
    field tta_cod_seguradora        as char  format "x(8)" label "Seguradora" column-label "Seguradora"
    field tta_cod_apol_seguro       as char  format "x(12)" label "Ap¢lice Seguro" column-label "Apolice Seguro"
    field tta_cod_arrendador        as char  format "x(6)" label "Arrendador" column-label "Arrendador"
    field tta_cod_contrat_leas      as char  format "x(12)" label "Contrato Leasing" column-label "Contr Leas"
    field tta_cod_histor_padr       as char  format "x(8)" label "Hist¢rico Padr∆o" column-label "Hist¢rico Padr∆o"
    field tta_des_text_histor       as char  format "x(2000)" label "Hist¢rico" column-label "Hist¢rico"
    field tta_ind_natur_cta_ctbl    as char  format "x(8)" init "DB" label "Natureza Cont†bil" column-label "Natureza Cont†bil"
    field tta_cod_usuar_gerac_movto as char  format "x(12)" label "Usuario Gerac Movto" column-label "Usuario"
    field ttv_cod_empresa_ext       as char  format "x(3)" label "C¢digo Empresa Ext" column-label "C¢d Emp Ext"
    field tta_cod_estab_ext         as char  format "x(8)" label "Estabelecimento Exte" column-label "Estabelecimento Ext"
    field tta_cod_portad_ext        as char  format "x(8)" label "Portador Externo" column-label "Portador Externo"
    field tta_cod_modalid_ext       as char  format "x(8)" label "Modalidade Externa" column-label "Modalidade Externa"
    field ttv_rec_antecip_pef_pend  as recid format ">>>>>>9"
    field tta_ind_origin_tit_ap     as char  format "x(3)" init "APB" label "Origem" column-label "Origem"
    field tta_cod_cart_bcia         as char  format "x(3)" label "Carteira" column-label "Carteira"
    index tt_antcppfp_id is primary unique
          tta_cod_estab
          tta_cod_refer
    index tt_antcppfp_tip_refer
          tta_cod_empresa
          tta_ind_tip_refer
          tta_cod_estab
          tta_cod_refer
    index tt_recid
          ttv_rec_antecip_pef_pend.

/* Temp-table 6: foi incluida nesta evoluá∆o para que a api faáa o tratamento do 1099para os clientes Datasul dos EUA. */
def temp-table tt_1099 no-undo
    field ttv_rec_table_parent     as recid format ">>>>>>9"
    field ttv_val_1099             as dec   format "->>,>>>,>>>,>>9.99" decimals 2
    field tta_cod_tax_ident_number as char  format "x(15)" label "Tax Id Number" column-label "Tax Id Number"
    field tta_ind_tip_trans_1099   as char  format "x(50)" init "Rents" label "Tipo Transacao 1099" column-label "Tipo Transacao 1099"
    index tt_rec_index is primary unique
          ttv_rec_table_parent.

/* Temp-table 7: foi inclu°da nesta evoluá∆o para a importaá∆o de Ordens de Compras. */
def temp-table tt_ord_compra_tit_ap_pend_1 no-undo
    field tta_cod_estab             as char  format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_refer             as char  format "x(10)" label "Referància" column-label "Referància"
    field tta_num_seq_refer         as int   format ">>>9" init 0 label "Sequància" column-label "Seq"
    field tta_cod_ord_compra        as char  format "x(8)" label "Ordem Compra" column-label "Ordem Compra"
    field tta_val_perc_ord_compra   as dec   format ">>9.99" decimals 2 init 0 label "Perc Ordem Compra" column-label "Perc Ordem Compra"
    field tta_val_origin_ord_compra as dec   format "->>,>>>,>>>,>>9.99" decimals 2 init 0 label "Original Ordem Compr" column-label "Original Ordem Compr"
    field tta_val_sdo_ord_compra    as dec   format "->>,>>>,>>>,>>9.99" decimals 2 init 0 label "Saldo Ordem Compra" column-label "Saldo Ordem Compra"
    index tt_codigo is primary unique
          tta_cod_estab
          tta_cod_refer
          tta_num_seq_refer
          tta_cod_ord_compra.

def temp-table tt_integr_apb_antecip_pef_p1 no-undo
    field tta_cod_empresa                  as char  format "x(3)" label "Empresa" column-label "Empresa"
    field tta_cod_estab                    as char  format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_refer                    as char  format "x(10)" label "Referància" column-label "Referància"
    field tta_cod_espec_docto              as char  format "x(3)" label "EspÇcie Documento" column-label "EspÇcie"
    field tta_cod_ser_docto                as char  format "x(3)" label "SÇrie Documento" column-label "SÇrie"
    field tta_cdn_fornecedor               as int   format ">>>,>>>,>>9" initial 0 label "Fornecedor" column-label "Fornecedor"
    field tta_cod_tit_ap                   as char  format "x(10)" label "T°tulo" column-label "T°tulo"
    field tta_cod_parcela                  as char  format "x(2)" label "Parcela" column-label "Parc"
    field tta_cod_portador                 as char  format "x(5)" label "Portador" column-label "Portador"
    field tta_cod_indic_econ               as char  format "x(8)" label "Moeda" column-label "Moeda"
    field tta_num_talon_cheq               as int   format ">>>,>>>,>>9" initial 0 label "Talon†rio Cheques" column-label "Talon†rio Cheques"
    field tta_num_cheque                   as int   format ">>>>,>>>,>>9" initial ? label "Num Cheque" column-label "Num Cheque"
    field tta_ind_favorec_cheq             as char  format "X(15)" init "Portador" label "Favorecido" column-label "Favorecido"
    field tta_nom_favorec_cheq             as char  format "x(40)" label "Nome Favorecido" column-label "Nome Favorecido"
    field tta_val_tit_ap                   as dec   format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor T°tulo" column-label "Valor T°tulo"
    field tta_val_cotac_indic_econ         as dec   format ">>>>,>>9.9999999999" decimals 10 initial 0 label "Cotaá∆o" column-label "Cotaá∆o"
    field tta_dat_emis_docto               as date  format "99/99/9999" initial today label "Data  Emiss∆o" column-label "Dt Emiss∆o"
    field tta_dat_vencto_tit_ap            as date  format "99/99/9999" initial today label "Data Vencimento" column-label "Dt Vencto"
    field tta_ind_tip_refer                as char  format "X(22)" label "Tipo Referància" column-label "Tipo"
    field tta_cod_seguradora               as char  format "x(8)" label "Seguradora" column-label "Seguradora"
    field tta_cod_apol_seguro              as char  format "x(12)" label "Ap¢lice Seguro" column-label "Apolice Seguro"
    field tta_cod_arrendador               as char  format "x(6)" label "Arrendador" column-label "Arrendador"
    field tta_cod_contrat_leas             as char  format "x(12)" label "Contrato Leasing" column-label "Contr Leas"
    field tta_cod_histor_padr              as char  format "x(8)" label "Hist¢rico Padr∆o" column-label "Hist¢rico Padr∆o"
    field tta_des_text_histor              as char  format "x(2000)" label "Hist¢rico" column-label "Hist¢rico"
    field tta_ind_natur_cta_ctbl           as char  format "X(8)" init "DB" label "Natureza Cont†bil" column-label "Natureza Cont†bil"
    field tta_cod_usuar_gerac_movto        as char  format "x(12)" label "Usuario Gerac Movto" column-label "Usuario"
    field ttv_cod_empresa_ext              as char  format "x(3)" label "C¢digo Empresa Ext" column-label "C¢d Emp Ext"
    field tta_cod_estab_ext                as char  format "x(8)" label "Estabelecimento Exte" column-label "Estabelecimento Ext"
    field tta_cod_portad_ext               as char  format "x(8)" label "Portador Externo" column-label "Portador Externo"
    field tta_cod_modalid_ext              as char  format "x(8)" label "Modalidade Externa" column-label "Modalidade Externa"
    field ttv_rec_antecip_pef_pend         as recid format ">>>>>>9"
    field tta_ind_origin_tit_ap            as char  format "X(3)" init "APB" label "Origem" column-label "Origem"
    field tta_cod_cart_bcia                as char  format "x(3)" label "Carteira" column-label "Carteira"
    field ttv_ind_tip_cod_barra            as char  format "X(1)"
    field tta_cb4_tit_ap_bco_cobdor        as char  format "x(50)" label "Titulo Bco Cobrador" column-label "Titulo Bco Cobrador"
    field tta_cod_tit_ap_bco_cobdor        as char  format "x(20)" label "T°tulo Banco Cobdor" column-label "T°tulo Banco Cobdor"
    index tt_antcppfp_id                   is primary unique
          tta_cod_estab
          tta_cod_refer
    index tt_antcppfp_tip_refer           
          tta_cod_empresa
          tta_ind_tip_refer
          tta_cod_estab
          tta_cod_refer
    index tt_recid                        
          ttv_rec_antecip_pef_pend.

{esp/ESAP004.i}
/*{app/apapi001.i}*/

def var cErro                  as char   no-undo.
def var c-estabel              as char   no-undo.
def var c-especie              as char   no-undo.
def var c-portador             as char   no-undo.
def var c-carteira             as char   no-undo.
def var c-forma-pgto           as char   no-undo.
def var i-seq                  as int    no-undo.
def var h-acomp                as handle no-undo.
def var c_cod_refer            as char   no-undo.
def var l_log_refer_unica      as log    no-undo.
def var v_hdl_aux              as handle no-undo.

def temp-table tt-raw-digita
field raw-digita as raw.    

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.               

{include/i-rpvar.i}

find param-global no-lock no-error.          

assign c-programa = "ESAP004rp"
       c-versao   = "1.00"
       c-revisao  = "000"
       c-sistema  = "Especifico"
       c-empresa  = v_cod_empres_usuar.

assign c-titulo-relat = "Consistància da Atualizaá∆o".

{include/i-rpcab.i}
{include/i-rpout.i}
    
view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp (INPUT "Processando...").

find first plano_ccusto no-lock
     where plano_ccusto.cod_empresa     = v_cod_empres_usuar
       and plano_ccusto.dat_inic_valid <= today
       and plano_ccusto.dat_fim_valid  >= today no-error.
if  not avail plano_ccusto then
    run utp/ut-msgs.p(input "show":U, input 17006,
                      input "Plano Centro de Custo~~Plano Centro de Custo n„o encontrado":U).

RUN pi-processa-ant.

IF tt-param.destino = 2 THEN       /*Modificado por Daniel 14/05*/
   ASSIGN tt-param.dt-ini = TODAY  /*a execuá∆o do RPW n∆o esta*/
          tt-param.dt-fim = TODAY. /*importando o titulo */

/* ----------------------------[      DEFINIÄ«O DE FUNÄÂES         ]--------------------------------*/
function fncLogReferUnicaAntecip returns logical
  ( p_cod_estab  as char,
    p_cod_refer  as char,
    p_cod_table  as char,
    p_rec_tabela as recid):

    def buffer bf-antecip_pef_pend   for antecip_pef_pend.
    def buffer bf-aprop_ctbl_pend_ap for aprop_ctbl_pend_ap.
    def buffer bf-movto_tit_ap       for movto_tit_ap.

    if  p_cod_table <> "antecip_pef_pend" and
        can-find(first bf-antecip_pef_pend use-index antcppfp_id
                 where bf-antecip_pef_pend.cod_estab = p_cod_estab
                   and bf-antecip_pef_pend.cod_refer = p_cod_refer
                   and recid(bf-antecip_pef_pend)   <> p_rec_tabela) then
        return no.

    if  p_cod_table <> "aprop_ctbl_pend_ap" and
        can-find(first bf-aprop_ctbl_pend_ap use-index aprpctba_id
                 where bf-aprop_ctbl_pend_ap.cod_estab = p_cod_estab
                   and bf-aprop_ctbl_pend_ap.cod_refer = p_cod_refer
                   and recid(bf-aprop_ctbl_pend_ap)   <> p_rec_tabela) then
        return no.

    if  can-find(first bf-movto_tit_ap use-index mvtttp_refer
                 where bf-movto_tit_ap.cod_estab = p_cod_estab
                   and bf-movto_tit_ap.cod_refer = p_cod_refer
                   and recid(bf-movto_tit_ap)   <> p_rec_tabela) then
        return no.

    return yes.
end function.

function fncLogReferUnicaNormal returns logical
  ( p_cod_estab  as char,
    p_cod_refer  as char,
    p_cod_table  as char,
    p_rec_tabela as recid):

    def buffer bf-antecip_pef_pend   for antecip_pef_pend.
    def buffer bf-impto_impl_pend_ap for impto_impl_pend_ap.
    def buffer bf-item_lote_impl_ap  for item_lote_impl_ap.
    def buffer bf-nota_pend_cartcred for nota_pend_cartcred.
    def buffer bf-lote_pagto         for lote_pagto.

    def buffer bf-lote_impl_tit_ap for lote_impl_tit_ap.
    def buffer bf-movto_tit_ap     for movto_tit_ap.

    if  p_cod_table <> "antecip_pef_pend" and
        can-find(first bf-antecip_pef_pend no-lock use-index antcppfp_id
                 where bf-antecip_pef_pend.cod_estab = p_cod_estab
                   and bf-antecip_pef_pend.cod_refer = p_cod_refer
                   and recid(bf-antecip_pef_pend)   <> p_rec_tabela) then
        return no.

    if  p_cod_table <> "impto_impl_pend_ap" and
        can-find(first bf-impto_impl_pend_ap no-lock use-index imptmplp_lote
                 where bf-impto_impl_pend_ap.cod_estab_refer = p_cod_estab
                   and bf-impto_impl_pend_ap.cod_refer       = p_cod_refer
                   and recid(bf-impto_impl_pend_ap)         <> p_rec_tabela) then
        return no.

    if  p_cod_table <> "item_lote_impl_ap" and
        can-find(first bf-item_lote_impl_ap no-lock use-index itmltmpl_id
                 where bf-item_lote_impl_ap.cod_estab = p_cod_estab
                   and bf-item_lote_impl_ap.cod_refer = p_cod_refer
                   and recid(bf-item_lote_impl_ap)   <> p_rec_tabela) then
        return no.

    if  p_cod_table <> "nota_pend_cartcred" and
        can-find(first bf-nota_pend_cartcred no-lock use-index ntpndcrt_id
                 where bf-nota_pend_cartcred.cod_estab = p_cod_estab
                   and bf-nota_pend_cartcred.cod_refer = p_cod_refer
                   and recid(bf-nota_pend_cartcred)   <> p_rec_tabela) then
        return no.

    if  p_cod_table <> "lote_pagto" and
        can-find(first bf-lote_pagto no-lock use-index ltpgt_id
                 where bf-lote_pagto.cod_estab_refer = p_cod_estab
                   and bf-lote_pagto.cod_refer       = p_cod_refer
                   and recid(bf-lote_pagto)         <> p_rec_tabela) then
        return no.

    if  p_cod_table <> "lote_impl_tit_ap" and
        can-find(first bf-lote_impl_tit_ap no-lock use-index ltmplttp_id
                 where bf-lote_impl_tit_ap.cod_estab = p_cod_estab
                   and bf-lote_impl_tit_ap.cod_refer = p_cod_refer
                   and recid(bf-lote_impl_tit_ap)   <> p_rec_tabela) then
        return no.

    if  can-find(first bf-movto_tit_ap no-lock use-index mvtttp_refer
                 where bf-movto_tit_ap.cod_estab = p_cod_estab
                   and bf-movto_tit_ap.cod_refer = p_cod_refer
                   and recid(bf-movto_tit_ap)   <> p_rec_tabela) then
        return no.

    if  can-find(first his_movto_tit_ap_histor no-lock use-index hsmvtttp_id
                 where his_movto_tit_ap_histor.cod_estab = p_cod_estab
                   and his_movto_tit_ap_histor.cod_refer = p_cod_refer) then
        return no.

    return yes.
end function.

assign i-seq = 0.
FOR EACH impTitulo exclusive-lock
    WHERE impTitulo.tpTransac      = '1'
    AND   impTitulo.dataTransacao >= tt-param.dt-ini
    AND   impTitulo.dataTransacao <= tt-param.dt-fim
    /*AND   impTitulo.empresa       >= tt-param.ep-codigo-ini
    AND   impTitulo.empresa       <= tt-param.ep-codigo-fim*/
    BREAK BY impTitulo.empresa:
    empty temp-table tt_log_erros_atualiz.
    empty temp-table tt_integr_apb_lote_impl.
    empty temp-table tt_integr_apb_item_lote_impl_3.
    empty temp-table tt_integr_apb_aprop_ctbl_pend.
    empty temp-table tt_integr_apb_impto_impl_pend.
    empty temp-table tt_integr_apb_abat_antecip_vouc.
    empty temp-table tt_integr_apb_abat_prev_provis.
    empty temp-table tt_integr_apb_aprop_relacto.
    empty temp-table tt_integr_apb_item_lote_impl.
    empty temp-table tt_integr_apb_relacto_pend.

    find first estabelecimento no-lock
         where estabelecimento.cod_estab = impTitulo.estabelecimento no-error.
    if  avail estabelecimento then
        assign c-empresa = estabelecimento.cod_empresa.
    else
        assign cErro =(if  cErro = "" then "" else " / ") + "n∆o encontrado Empresa para Estabelecimento" + impTitulo.estabelecimento.

    FIND FIRST impAntecip NO-LOCK WHERE
               impAntecip.empresa          = impTitulo.empresa          AND
               impAntecip.estabelecimento  = impTitulo.estabelecimento  AND
               impAntecip.especie          = impTitulo.especie          AND
               impAntecip.serie            = impTitulo.serie            AND
               impAntecip.numeroDocumento  = impTitulo.numeroDocumento  AND
               impAntecip.parcela          = impTitulo.parcela          AND
               impAntecip.codigoFornecedor = impTitulo.codigoFornecedor NO-ERROR.

    find first trad_finalid_econ_ext no-lock
         where trad_finalid_econ_ext.cod_matriz_trad_finalid_ext = "MOEDAS"
           and trad_finalid_econ_ext.cod_finalid_econ_ext        = "0" no-error.
    if  not avail trad_finalid_econ_ext then
        assign cErro =(if  cErro = "" then "" else " / ") + "n∆o encontrado trad_finalid_econ_ext 0".

    find first histor_finalid_econ no-lock
         where histor_finalid_econ.cod_finalid_econ        = trad_finalid_econ_ext.cod_finalid_econ
           and histor_finalid_econ.dat_inic_valid_finalid <= today
           and histor_finalid_econ.dat_fim_valid_finalid  >= today no-error.
    if  not avail histor_finalid_econ then
        assign cErro =(if  cErro = "" then "" else " / ") + "n∆o encontrado histor_finalid_econ" + trad_finalid_econ_ext.cod_finalid_econ.

    find first trad_portad_ext no-lock
         where trad_portad_ext.cod_matriz_trad_portad_ext = "PORTADOR"
           and trad_portad_ext.cod_portad_ext             = string(impTitulo.codigoPortador)
/*            and trad_portad_ext.cod_modalid_ext            = string(tit-ap.modalidade) */ no-error.
    if  not avail trad_portad_ext then
        assign cErro =(if  cErro = "" then "" else " / ") + "n∆o encontrado trad_portad_ext" + string(impTitulo.codigoPortador).

    assign c-portador = trad_portad_ext.cod_portador when avail trad_portad_ext
           c-carteira = "APB" /* trad_portad_ext.cod_cart_bcia */.
    if  avail histor_finalid_econ and histor_finalid_econ.cod_finalid_econ <> "Corrente" then
        assign c-portador = "80000" c-carteira = "APB".
    if  trim(cErro) <> "" then do:
        ASSIGN impTitulo.tptransac  = "3"
               impTitulo.observacao = trim(impTitulo.observacao) + CHR(10) + trim(cErro).

        next.
    end.

    assign i-seq = i-seq + 1.

    run pi-acompanhar in h-acomp(INPUT "T°tulo " + STRING(i-seq) + " " + impTitulo.numeroDocumento).

    assign l_log_refer_unica = no
           c_cod_refer       = ''.
    repeat while not l_log_refer_unica:
        run pi_retorna_sugestao_referencia(input "M",
                                           input today,
                                           output c_cod_refer) /*pi_retorna_sugestao_referencia*/.

        assign l_log_refer_unica = fncLogReferUnicaNormal(impTitulo.estabelecimento, c_cod_refer, '', ?).
    end.

/*     IF FIRST-OF(impTitulo.empresa) THEN DO: */
        create tt_integr_apb_lote_impl.
        assign tt_integr_apb_lote_impl.tta_cod_empresa              = c-empresa
               tt_integr_apb_lote_impl.tta_cod_estab                = impTitulo.estabelecimento
               tt_integr_apb_lote_impl.tta_cod_refer                = c_cod_refer
               tt_integr_apb_lote_impl.tta_dat_transacao            = today
               tt_integr_apb_lote_impl.tta_ind_origin_tit_ap        = "APB"
/*                tt_integr_apb_lote_impl.tta_val_tot_lote_impl_tit_ap = 0 */
               tt_integr_apb_lote_impl.tta_cod_indic_econ           = histor_finalid_econ.cod_indic_econ
               i-seq                                                = 1.
/*     END. */

    create tt_integr_apb_item_lote_impl.
    assign tt_integr_apb_item_lote_impl.ttv_rec_integr_apb_lote_impl  = recid(tt_integr_apb_lote_impl)
           tt_integr_apb_item_lote_impl.tta_num_seq_refer             = i-seq
           tt_integr_apb_item_lote_impl.tta_cdn_fornecedor            = impTitulo.codigoFornecedor
           tt_integr_apb_item_lote_impl.tta_cod_espec_docto           = impTitulo.especie
           tt_integr_apb_item_lote_impl.tta_cod_ser_docto             = impTitulo.serie
           tt_integr_apb_item_lote_impl.tta_cod_tit_ap                = impTitulo.numeroDocumento
           tt_integr_apb_item_lote_impl.tta_cod_parcela               = impTitulo.parcela
           tt_integr_apb_item_lote_impl.tta_dat_emis_docto            = impTitulo.dataEmissao
           tt_integr_apb_item_lote_impl.tta_dat_vencto_tit_ap         = impTitulo.dataVencimento
           tt_integr_apb_item_lote_impl.tta_dat_prev_pagto            = impTitulo.dataVencimento
           tt_integr_apb_item_lote_impl.tta_cod_indic_econ            = histor_finalid_econ.cod_indic_econ
           tt_integr_apb_item_lote_impl.tta_val_tit_ap                = impTitulo.valor
           tt_integr_apb_item_lote_impl.tta_val_desconto              = 0
           tt_integr_apb_item_lote_impl.tta_val_perc_desc             = 0
           tt_integr_apb_item_lote_impl.tta_num_dias_atraso           = 0
           tt_integr_apb_item_lote_impl.tta_val_juros_dia_atraso      = 0
           tt_integr_apb_item_lote_impl.tta_val_perc_juros_dia_atraso = 0
           tt_integr_apb_item_lote_impl.tta_val_perc_multa_atraso     = 0
           tt_integr_apb_item_lote_impl.tta_cod_portador              = IF AVAIL impAntecip THEN string(impAntecip.codigoPortador) ELSE ""
           tt_integr_apb_item_lote_impl.tta_cod_cart_bcia             = c-carteira
           tt_integr_apb_item_lote_impl.tta_des_text_histor           = "TITULO INTEGRADO PELO PROGRAMA ESPEC÷FICO ESAP004"
           tt_integr_apb_item_lote_impl.ttv_log_gerad                 = no
           tt_integr_apb_item_lote_impl.tta_cod_forma_pagto           = IF AVAIL impAntecip THEN string(impAntecip.tpPagamento) ELSE '002'.

    create tt_integr_apb_item_lote_impl_3.
    assign tt_integr_apb_item_lote_impl_3.ttv_rec_integr_apb_lote_impl = recid(tt_integr_apb_lote_impl)
           tt_integr_apb_item_lote_impl_3.tta_num_seq_refer            = tt_integr_apb_item_lote_impl.tta_num_seq_refer
           tt_integr_apb_item_lote_impl_3.tta_cdn_fornecedor           = tt_integr_apb_item_lote_impl.tta_cdn_fornecedor
           tt_integr_apb_item_lote_impl_3.tta_cod_espec_docto          = tt_integr_apb_item_lote_impl.tta_cod_espec_docto
           tt_integr_apb_item_lote_impl_3.tta_cod_ser_docto            = tt_integr_apb_item_lote_impl.tta_cod_ser_docto
           tt_integr_apb_item_lote_impl_3.tta_cod_tit_ap               = tt_integr_apb_item_lote_impl.tta_cod_tit_ap
           tt_integr_apb_item_lote_impl_3.tta_cod_parcela              = tt_integr_apb_item_lote_impl.tta_cod_parcela
           tt_integr_apb_item_lote_impl_3.tta_dat_emis_docto           = tt_integr_apb_item_lote_impl.tta_dat_emis_docto
           tt_integr_apb_item_lote_impl_3.tta_dat_vencto_tit_ap        = tt_integr_apb_item_lote_impl.tta_dat_vencto_tit_ap
           tt_integr_apb_item_lote_impl_3.tta_dat_prev_pagto           = tt_integr_apb_item_lote_impl.tta_dat_prev_pagto
           tt_integr_apb_item_lote_impl_3.tta_val_tit_ap               = tt_integr_apb_item_lote_impl.tta_val_tit_ap
           tt_integr_apb_item_lote_impl_3.tta_val_cotac_indic_econ     = 1
           tt_integr_apb_item_lote_impl_3.ttv_qtd_parc_tit_ap          = tt_integr_apb_item_lote_impl.ttv_qtd_parc_tit_ap
           tt_integr_apb_item_lote_impl_3.ttv_ind_vencto_previs        = tt_integr_apb_item_lote_impl.ttv_ind_vencto_previs
           tt_integr_apb_item_lote_impl_3.ttv_rec_integr_apb_item_lote = recid(tt_integr_apb_item_lote_impl)
           tt_integr_apb_item_lote_impl_3.tta_ind_tip_trans_1099       = 'Rents'
           tt_integr_apb_item_lote_impl_3.tta_cod_indic_econ           = tt_integr_apb_item_lote_impl.tta_cod_indic_econ
           tt_integr_apb_item_lote_impl_3.tta_cod_portador             = tt_integr_apb_item_lote_impl.tta_cod_portador
           tt_integr_apb_item_lote_impl_3.tta_cod_forma_pagto          = tt_integr_apb_item_lote_impl.tta_cod_forma_pagto.

    find first fornec_financ no-lock use-index frncfnnc_id
         where fornec_financ.cod_empresa    = tt_integr_apb_lote_impl.tta_cod_empresa
           and fornec_financ.cdn_fornecedor = tt_integr_apb_item_lote_impl_3.tta_cdn_fornecedor no-error.

    FOR EACH wrateio NO-LOCK
       WHERE wrateio.ep_codigo   = impTitulo.empresa
         AND wrateio.cod_estabel = impTitulo.estabelecimento 
         AND wrateio.cod_esp     = impTitulo.especie         
         AND wrateio.serie       = impTitulo.serie           
         AND wrateio.nr_docto    = impTitulo.numeroDocumento 
         AND wrateio.parcela     = impTitulo.parcela         
         AND wrateio.cod_fornec  = impTitulo.codigoFornecedor:
        create tt_integr_apb_aprop_ctbl_pend.
        assign tt_integr_apb_aprop_ctbl_pend.ttv_rec_integr_apb_item_lote  = recid(tt_integr_apb_item_lote_impl)
               tt_integr_apb_aprop_ctbl_pend.ttv_rec_antecip_pef_pend      = ?
               tt_integr_apb_aprop_ctbl_pend.ttv_rec_integr_apb_impto_pend = ?
               tt_integr_apb_aprop_ctbl_pend.tta_cod_plano_cta_ctbl        = 'CONTSOC'
               tt_integr_apb_aprop_ctbl_pend.tta_cod_cta_ctbl              = substr(wrateio.conta_debito,1,8)
               tt_integr_apb_aprop_ctbl_pend.tta_cod_unid_negoc            = substr(wrateio.conta_debito,9,2)
               tt_integr_apb_aprop_ctbl_pend.tta_cod_plano_ccusto          =(if  substr(wrateio.conta_debito,11,6) = "000000" then "" else plano_ccusto.cod_plano_ccusto)
               tt_integr_apb_aprop_ctbl_pend.tta_cod_ccusto                =(if  substr(wrateio.conta_debito,11,6) = "000000" then "" else substr(wrateio.conta_debito,11,6))
               tt_integr_apb_aprop_ctbl_pend.tta_cod_tip_fluxo_financ      = fornec_financ.cod_tip_fluxo_financ when avail fornec_financ
               tt_integr_apb_aprop_ctbl_pend.tta_val_aprop_ctbl            = wrateio.vl_movto.
    END.

    find first tt_integr_apb_lote_impl no-error.
    find first tt_integr_apb_aprop_ctbl_pend no-error.

    if  not avail tt_integr_apb_lote_impl then do:
        ASSIGN impTitulo.tptransac  = "3"
               impTitulo.observacao = trim(impTitulo.observacao) + CHR(10) + "Lote inexistente".
    end.

    run prgfin/apb/apb900zf.py(input 4,
                               input "EMS2",
                               input-output table tt_integr_apb_item_lote_impl_3).

    /*** Mostra Erros da tt_log_erros_atualiz **/                                                                                
    for each tt_log_erros_atualiz no-lock:
        if  tt_log_erros_atualiz.ttv_des_msg_ajuda = "" then
            assign tt_log_erros_atualiz.ttv_des_msg_ajuda = tt_log_erros_atualiz.ttv_des_msg_erro.

        ASSIGN impTitulo.tptransac  = "3"
               impTitulo.observacao = trim(impTitulo.observacao) + CHR(10) + trim(tt_log_erros_atualiz.ttv_des_msg_ajuda).
    end.

    find first tt_log_erros_atualiz no-lock no-error.
    if  not avail tt_log_erros_atualiz then
        ASSIGN impTitulo.tptransac = "2".

    release tt_log_erros_atualiz.
    release tt_integr_apb_lote_impl.
    release tt_integr_apb_item_lote_impl_3.
    release tt_integr_apb_aprop_ctbl_pend.
    release tt_integr_apb_impto_impl_pend.

    release tt_integr_apb_abat_antecip_vouc.
    release tt_integr_apb_abat_prev_provis.
    release tt_integr_apb_aprop_relacto.
    release tt_integr_apb_item_lote_impl.
    release tt_integr_apb_relacto_pend.
end.

run pi-finalizar in h-acomp.

{include/i-rpclo.i}

return "OK":U.


/* ===> Procedures <=== */

PROCEDURE pi-processa-ant :
    DEFINE VARIABLE d-cotacao AS DECIMAL     NO-UNDO.

    empty temp-table tt_log_erros_atualiz.

    IF tt-param.destino = 2 THEN       /*Modificado por Daniel 14/05*/
       ASSIGN tt-param.dt-ini = TODAY  /*a execuá∆o do RPW n∆o esta*/
              tt-param.dt-fim = TODAY. /*importando o titulo */

    ASSIGN i-seq = 0.
    FOR EACH impAntecip exclusive-lock
        WHERE impAntecip.tpTransac      = '1'
        AND   impAntecip.vinculado      = '1'
        AND   impAntecip.dataTransacao >= tt-param.dt-ini
        AND   impAntecip.dataTransacao <= tt-param.dt-fim
        /*AND   impAntecip.empresa       >= STRING (tt-param.ep-codigo-ini)
        AND   impAntecip.empresa       <= STRING (tt-param.ep-codigo-fim)*/ :

        assign cErro = "".

        /* elimina as tabelas tempor†rias */
        empty temp-table tt_integr_apb_antecip_pef_p1.
        empty temp-table tt_integr_apb_antecip_pef_pend.
        empty temp-table tt_integr_apb_abat_prev_provis.
        empty temp-table tt_integr_apb_impto_impl_pend.
        empty temp-table tt_integr_apb_aprop_ctbl_pend.
        empty temp-table tt_log_erros_atualiz.
        empty temp-table tt_1099.
        empty temp-table tt_ord_compra_tit_ap_pend_1.

        find first estabelecimento no-lock
             where estabelecimento.cod_estab = impAntecip.estabelecimento no-error.
        if  avail estabelecimento then
            assign c-empresa = estabelecimento.cod_empresa.
        else
            assign cErro =(if  cErro = "" then "" else " / ") + "n∆o encontrado Empresa para Estabelecimento" + impAntecip.estabelecimento.

        find first trad_finalid_econ_ext no-lock
             where trad_finalid_econ_ext.cod_matriz_trad_finalid_ext = "MOEDAS"
               and trad_finalid_econ_ext.cod_finalid_econ_ext        = string(impAntecip.tpMoeda) no-error.
        if  not avail trad_finalid_econ_ext then
            assign cErro =(if  cErro = "" then "" else " / ") + "n∆o encontrado trad_finalid_econ_ext" + string(impAntecip.tpMoeda).

        if  avail trad_finalid_econ_ext then do:
            find first histor_finalid_econ no-lock
                 where histor_finalid_econ.cod_finalid_econ        = trad_finalid_econ_ext.cod_finalid_econ
                   and histor_finalid_econ.dat_inic_valid_finalid <= today
                   and histor_finalid_econ.dat_fim_valid_finalid  >= today no-error.
            if  not avail histor_finalid_econ then
                assign cErro =(if  cErro = "" then "" else " / ") + "n∆o encontrado histor_finalid_econ" + trad_finalid_econ_ext.cod_finalid_econ.
        end.

        find first trad_portad_ext no-lock
             where trad_portad_ext.cod_matriz_trad_portad_ext = "PORTADOR"
               and trad_portad_ext.cod_portad_ext             = string(impAntecip.codigoPortador)
/*                and trad_portad_ext.cod_modalid_ext            = string(tit-ap.modalidade)*/ no-error.
        if  not avail trad_portad_ext then
            assign cErro =(if  cErro = "" then "" else " / ") + "n∆o encontrado trad_portad_ext" + string(impAntecip.codigoPortador).

        assign c-portador = trad_portad_ext.cod_portador
               c-carteira = "APB" /* trad_portad_ext.cod_cart_bcia */.
        if  avail histor_finalid_econ and histor_finalid_econ.cod_finalid_econ <> "Corrente" then
            assign c-portador = "80000" c-carteira = "APB".

        find first fornec_financ no-lock use-index frncfnnc_id
             where fornec_financ.cod_empresa    = c-empresa
               and fornec_financ.cdn_fornecedor = impAntecip.codigoFornecedor no-error.
        if  not avail fornec_financ then
            assign cErro =(if  cErro = "" then "" else " / ") + "n∆o encontrado fornec_financ".
/*
        if  avail fornec_financ and
            avail trad_finalid_econ_ext then do:
            find first fornecedor no-lock
                 where fornecedor.cod_empresa    = fornec_financ.cod_empresa
                   and fornecedor.cdn_fornecedor = fornec_financ.cdn_fornecedor no-error.
            if  not avail fornecedor then
                assign cErro =(if  cErro = "" then "" else " / ") + "n∆o encontrado fornecedor".

            if  avail fornecedor then do:
                find first cta_grp_fornec no-lock
                     where cta_grp_fornec.cod_empresa            = fornecedor.cod_empresa
                       and cta_grp_fornec.ind_tip_espec_docto    = "Nenhum"
                       and cta_grp_fornec.cod_espec_docto        = impAntecip.especie
                       and cta_grp_fornec.cod_grp_fornec         = fornecedor.cod_grp_fornec
                       and cta_grp_fornec.cod_finalid_econ       = trad_finalid_econ_ext.cod_finalid_econ
                       and cta_grp_fornec.ind_finalid_ctbl       = "Saldo"
                       and cta_grp_fornec.num_seq_cta_grp_fornec = 10 no-error.
                if  not avail cta_grp_fornec then
                    assign cErro =(if  cErro = "" then "" else " / ") + "n∆o encontrado cta_grp_fornec".
            end.
        end.
*/
        if  trim(cErro) <> "" then do:
            assign impAntecip.tptransac  = "3"
                   impAntecip.observacao = trim(impAntecip.observacao) + CHR(10) + trim(cErro).

            next.
        end.

        /*---------- Cria Referància ----------*/
        assign l_log_refer_unica = no
               c_cod_refer       = ''.
        repeat while not l_log_refer_unica:
            run pi_retorna_sugestao_referencia(input "M",
                                               input today,
                                               output c_cod_refer) /*pi_retorna_sugestao_referencia*/.

            assign l_log_refer_unica = fncLogReferUnicaAntecip(impAntecip.estabelecimento, c_cod_refer, "", ?).
        end.

        assign i-seq = i-seq + 1.

        run pi-acompanhar in h-acomp(INPUT "Antecipaá∆o " + STRING(i-seq) + " " + impAntecip.numeroDocumento).

        create tt_integr_apb_antecip_pef_p1.
        assign tt_integr_apb_antecip_pef_p1.tta_cod_empresa                 = c-empresa
               tt_integr_apb_antecip_pef_p1.tta_cod_estab                   = impAntecip.estabelecimento
               tt_integr_apb_antecip_pef_p1.tta_cod_refer                   = c_cod_refer
               tt_integr_apb_antecip_pef_p1.tta_cod_espec_docto             = impAntecip.especie
               tt_integr_apb_antecip_pef_p1.tta_cod_ser_docto               = impAntecip.serie
               tt_integr_apb_antecip_pef_p1.tta_cdn_fornecedor              = impAntecip.codigoFornecedor
               tt_integr_apb_antecip_pef_p1.tta_cod_tit_ap                  = impAntecip.numeroDocumento
               tt_integr_apb_antecip_pef_p1.tta_cod_parcela                 = impAntecip.parcela

               tt_integr_apb_antecip_pef_p1.tta_cod_portador                = c-portador
               tt_integr_apb_antecip_pef_p1.tta_cod_indic_econ              = histor_finalid_econ.cod_indic_econ
               tt_integr_apb_antecip_pef_p1.tta_num_talon_cheq              = 0
               tt_integr_apb_antecip_pef_p1.tta_num_cheque                  = 0
               tt_integr_apb_antecip_pef_p1.tta_ind_favorec_cheq            = ""
               tt_integr_apb_antecip_pef_p1.tta_nom_favorec_cheq            = ""
               tt_integr_apb_antecip_pef_p1.tta_val_tit_ap                  = impAntecip.valor
               tt_integr_apb_antecip_pef_p1.tta_val_cotac_indic_econ        = dec(impAntecip.compl1)
               tt_integr_apb_antecip_pef_p1.tta_dat_emis_docto              = impAntecip.dataEmissao
               tt_integr_apb_antecip_pef_p1.tta_dat_vencto_tit_ap           = impAntecip.dataVencimento
               tt_integr_apb_antecip_pef_p1.tta_ind_tip_refer               = "Antecipaá∆o"
               tt_integr_apb_antecip_pef_p1.tta_cod_seguradora              = ""
               tt_integr_apb_antecip_pef_p1.tta_cod_apol_seguro             = ""
               tt_integr_apb_antecip_pef_p1.tta_cod_arrendador              = ""
               tt_integr_apb_antecip_pef_p1.tta_cod_contrat_leas            = ""
               tt_integr_apb_antecip_pef_p1.tta_cod_histor_padr             = ""
               tt_integr_apb_antecip_pef_p1.tta_des_text_histor             = impAntecip.historico
               tt_integr_apb_antecip_pef_p1.tta_ind_natur_cta_ctbl          = "DB" /* (if  tit-ap.lancamento = 1 then "DB" else "CR") */ /* ajustar - verificar informaá∆o correta */
               tt_integr_apb_antecip_pef_p1.tta_cod_usuar_gerac_movto       = v_cod_usuar_corren
               tt_integr_apb_antecip_pef_p1.ttv_cod_empresa_ext             = ""
               tt_integr_apb_antecip_pef_p1.tta_cod_estab_ext               = ""
               tt_integr_apb_antecip_pef_p1.tta_cod_portad_ext              = ""
               tt_integr_apb_antecip_pef_p1.tta_cod_modalid_ext             = ""
               tt_integr_apb_antecip_pef_p1.ttv_rec_antecip_pef_pend        = recid(tt_integr_apb_antecip_pef_p1)
               tt_integr_apb_antecip_pef_p1.tta_ind_origin_tit_ap           = "APB"
               tt_integr_apb_antecip_pef_p1.tta_cod_cart_bcia               = c-carteira
               tt_integr_apb_antecip_pef_p1.ttv_ind_tip_cod_barra           = ""
               tt_integr_apb_antecip_pef_p1.tta_cb4_tit_ap_bco_cobdor       = ""
               tt_integr_apb_antecip_pef_p1.tta_cod_tit_ap_bco_cobdor       = "".
    /*
        run pCriaImposto.
    */
        create tt_integr_apb_aprop_ctbl_pend.
        assign tt_integr_apb_aprop_ctbl_pend.ttv_rec_integr_apb_item_lote   = ?
               tt_integr_apb_aprop_ctbl_pend.ttv_rec_antecip_pef_pend       = recid(tt_integr_apb_antecip_pef_p1)
               tt_integr_apb_aprop_ctbl_pend.ttv_rec_integr_apb_impto_pend  = if avail tt_integr_apb_impto_impl_pend then
                                                                                 recid(tt_integr_apb_impto_impl_pend) else ?
               tt_integr_apb_aprop_ctbl_pend.tta_cod_plano_cta_ctbl         = "" /* cta_grp_fornec.cod_plano_cta_ctbl */
               tt_integr_apb_aprop_ctbl_pend.tta_cod_cta_ctbl               = "" /* cta_grp_fornec.cod_cta_ctbl */ 
               tt_integr_apb_aprop_ctbl_pend.tta_cod_unid_negoc             = "00"
               tt_integr_apb_aprop_ctbl_pend.tta_cod_plano_ccusto           = ""
               tt_integr_apb_aprop_ctbl_pend.tta_cod_ccusto                 = ""
               tt_integr_apb_aprop_ctbl_pend.tta_cod_tip_fluxo_financ       = fornec_financ.cod_tip_fluxo_financ when avail fornec_financ
               tt_integr_apb_aprop_ctbl_pend.tta_val_aprop_ctbl             = tt_integr_apb_antecip_pef_p1.tta_val_tit_ap
               tt_integr_apb_aprop_ctbl_pend.tta_cod_pais                   = "BRA"
               tt_integr_apb_aprop_ctbl_pend.tta_cod_unid_federac           = ""
               tt_integr_apb_aprop_ctbl_pend.tta_cod_imposto                = ""
               tt_integr_apb_aprop_ctbl_pend.tta_cod_classif_impto          = ""
               tt_integr_apb_aprop_ctbl_pend.ttv_cod_tip_fluxo_financ_ext   = ""
               tt_integr_apb_aprop_ctbl_pend.tta_cod_cta_ctbl_ext           = ""
               tt_integr_apb_aprop_ctbl_pend.tta_cod_sub_cta_ctbl_ext       = ""
               tt_integr_apb_aprop_ctbl_pend.tta_cod_ccusto_ext             = ""
               tt_integr_apb_aprop_ctbl_pend.tta_cod_unid_negoc_ext         = "".

        assign v_cod_empres_usuar = c-empresa.

        disable triggers for load of antecip_pef_pend.

        run prgfin/apb/apb905zd.py persistent set v_hdl_aux.

        run pi_main_block_antecip_pef_pend_2 in v_hdl_aux (input 3,
                                                           input "EMS2", /*p_cod_matriz_trad_org_ext*/
                                                           input-output table tt_integr_apb_antecip_pef_p1,
                                                           input table tt_integr_apb_aprop_ctbl_pend,
                                                           input table tt_integr_apb_impto_impl_pend,
                                                           input table tt_integr_apb_abat_prev_provis,
                                                           output table tt_log_erros_atualiz,
                                                           input table tt_1099,
                                                           input table tt_ord_compra_tit_ap_pend_1).

        delete procedure v_hdl_aux.

        /*** Mostra Erros da tt_log_erros_atualiz **/                                                                                
        for each tt_log_erros_atualiz no-lock:
            if  tt_log_erros_atualiz.ttv_des_msg_ajuda = "" then
                assign tt_log_erros_atualiz.ttv_des_msg_ajuda = tt_log_erros_atualiz.ttv_des_msg_erro.

            ASSIGN impAntecip.tptransac  = "3"
                   impAntecip.observacao = trim(impAntecip.observacao) + CHR(10) + trim(tt_log_erros_atualiz.ttv_des_msg_ajuda).
        end.

        find first tt_log_erros_atualiz no-lock no-error.
        if  not avail tt_log_erros_atualiz then
            ASSIGN impAntecip.tptransac = "2".

        release tt_integr_apb_antecip_pef_p1.
        release tt_integr_apb_antecip_pef_pend.
        release tt_integr_apb_abat_prev_provis.
        release tt_integr_apb_impto_impl_pend.
        release tt_integr_apb_aprop_ctbl_pend.
        release tt_log_erros_atualiz.
        release tt_1099.
        release tt_ord_compra_tit_ap_pend_1.
    END.
END PROCEDURE.

PROCEDURE pi_retorna_sugestao_referencia:

    /************************ Parameter Definition Begin ************************/

    def Input param p_ind_tip_atualiz
        as character
        format "X(08)"
        no-undo.
    def Input param p_dat_refer
        as date
        format "99/99/9999"
        no-undo.
    def output param p_cod_refer
        as character
        format "x(10)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_des_dat                        as character       no-undo. /*local*/
    def var v_num_aux                        as integer         no-undo. /*local*/
    def var v_num_aux_2                      as integer         no-undo. /*local*/
    def var v_num_cont                       as integer         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    assign v_des_dat   = string(p_dat_refer,"99999999")
           p_cod_refer = substring(v_des_dat,7,2)
                       + substring(v_des_dat,3,2)
                       + substring(v_des_dat,1,2)
                       + substring(p_ind_tip_atualiz,1,1)
           v_num_aux_2 = integer(this-procedure:handle).

    do  v_num_cont = 1 to 3:
        assign v_num_aux   = (random(0,v_num_aux_2) mod 26) + 97
               p_cod_refer = p_cod_refer + chr(v_num_aux).
    end.
END PROCEDURE. /* pi_retorna_sugestao_referencia */


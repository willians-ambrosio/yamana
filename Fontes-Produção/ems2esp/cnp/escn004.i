/****************************************************************************************** 
** 	   Programa: escn004.i
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 06/11/2018
** Change/Chamado: 
**      Objetivo: Defini‡Æo das tempor rias para a chamada da api de altera‡Æo de t¡tulos
******************************** CONTROLE DE ALTERA€åES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: N/A
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: cn0201a
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
DEF TEMP-TABLE tt-retencao
    FIELD atualiza          AS LOG
    FIELD ap-atual          AS LOG
    FIELD row-medicao       AS ROWID
    FIELD cdn_fornec        LIKE tit_ap.cdn_fornec
    FIELD cod_tit_ap        LIKE tit_ap.cod_tit_ap           
    FIELD cod_ser_docto     LIKE tit_ap.cod_ser_docto        
    FIELD cod_espec_docto   LIKE tit_ap.cod_espec_docto      
    FIELD val_origin_tit_ap   LIKE tit_ap.val_origin_tit_ap 
    FIELD dat_emis_docto    LIKE tit_ap.dat_emis_docto       
    FIELD dat_transacao     LIKE tit_ap.dat_transacao        
    FIELD dat_vencto_tit_ap LIKE tit_ap.dat_vencto_tit_ap    
    FIELD cod_parcela       LIKE tit_ap.cod_parcela          
    FIELD cod_estab         LIKE tit_ap.cod_estab            
    FIELD nr-contrato       LIKE ext-contrato-for.nr-contrato 
    FIELD num_id_tit_ap     LIKE tit_ap.num_id_tit_ap
    FIELD perc_corr   AS DECIMAL FORMAT ">>9.99"
    FIELD vlr_corr    LIKE tit_ap.val_origin_tit_ap 
  INDEX idx1 IS PRIMARY UNIQUE cod_estab num_id_tit_ap.

def temp-table tt_tit_ap_alteracao_base_aux_3 no-undo
    field ttv_cod_usuar_corren             as character format "x(12)" label "Usu rio Corrente" column-label "Usu rio Corrente"
    field tta_cod_empresa                  as character format "x(3)" label "Empresa" column-label "Empresa"
    field tta_cod_estab                    as character format "x(5)" label "Estabelecimento" column-label "Estab"
    field tta_num_id_tit_ap                as integer format "9999999999" initial 0 label "Token Tit AP" column-label "Token Tit AP"
    field ttv_rec_tit_ap                   as recid format ">>>>>>9" initial ?
    field tta_cdn_fornecedor               as Integer format ">>>,>>>,>>9" initial 0 label "Fornecedor" column-label "Fornecedor"
    field tta_cod_espec_docto              as character format "x(3)" label "Esp‚cie Documento" column-label "Esp‚cie"
    field tta_cod_ser_docto                as character format "x(3)" label "S‚rie Documento" column-label "S‚rie"
    field tta_cod_tit_ap                   as character format "x(10)" label "T¡tulo" column-label "T¡tulo"
    field tta_cod_parcela                  as character format "x(02)" label "Parcela" column-label "Parc"
    field ttv_dat_transacao                as date format "99/99/9999" initial today label "Data Transa‡Æo" column-label "Data Transa‡Æo"
    field ttv_cod_refer                    as character format "x(10)" label "Referˆncia" column-label "Referˆncia"
    field tta_val_sdo_tit_ap               as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Saldo" column-label "Valor Saldo"
    field tta_dat_emis_docto               as date format "99/99/9999" initial today label "Data  EmissÆo" column-label "Dt EmissÆo"
    field tta_dat_vencto_tit_ap            as date format "99/99/9999" initial today label "Data Vencimento" column-label "Dt Vencto"
    field tta_dat_prev_pagto               as date format "99/99/9999" initial today label "Data Prevista Pgto" column-label "Dt Prev Pagto"
    field tta_dat_ult_pagto                as date format "99/99/9999" initial ? label "Data éltimo Pagto" column-label "Data éltimo Pagto"
    field tta_num_dias_atraso              as integer format ">9" initial 0 label "Dias Atraso" column-label "Dias Atr"
    field tta_val_perc_multa_atraso        as decimal format ">9.99" decimals 2 initial 00.00 label "Perc Multa Atraso" column-label "Multa Atr"
    field tta_val_juros_dia_atraso         as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Juro" column-label "Vl Juro"
    field tta_val_perc_juros_dia_atraso    as decimal format ">9.999999" decimals 6 initial 00.00 label "Perc Jur Dia Atraso" column-label "Perc Dia"
    field tta_dat_desconto                 as date format "99/99/9999" initial ? label "Data Desconto" column-label "Dt Descto"
    field tta_val_perc_desc                as decimal format ">9.9999" decimals 4 initial 0 label "Percentual Desconto" column-label "Perc Descto"
    field tta_val_desconto                 as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Desconto" column-label "Valor Desconto"
    field tta_cod_portador                 as character format "x(5)" label "Portador" column-label "Portador"
    field ttv_cod_portador_mov             as character format "x(5)" label "Portador Movto" column-label "Portador Movto"
    field tta_log_pagto_bloqdo             as logical format "Sim/NÆo" initial no label "Bloqueia Pagamento" column-label "Pagto Bloqdo"
    field tta_cod_seguradora               as character format "x(8)" label "Seguradora" column-label "Seguradora"
    field tta_cod_apol_seguro              as character format "x(12)" label "Ap¢lice Seguro" column-label "Apolice Seguro"
    field tta_cod_arrendador               as character format "x(6)" label "Arrendador" column-label "Arrendador"
    field tta_cod_contrat_leas             as character format "x(12)" label "Contrato Leasing" column-label "Contr Leas"
    field tta_ind_tip_espec_docto          as character format "X(17)" initial "Normal" label "Tipo Esp‚cie" column-label "Tipo Esp‚cie"
    field tta_cod_indic_econ               as character format "x(8)" label "Moeda" column-label "Moeda"
    field tta_num_seq_refer                as integer format ">>>9" initial 0 label "Sequˆncia" column-label "Seq"
    field ttv_ind_motiv_alter_val_tit_ap   as character format "X(09)" initial "Altera‡Æo" label "Motivo Altera‡Æo" column-label "Motivo Altera‡Æo"
    field ttv_wgh_lista                    as widget-handle extent 15 format ">>>>>>9"
    field ttv_log_gera_ocor_alter_valores  as logical format "Sim/NÆo" initial no
    field tta_cb4_tit_ap_bco_cobdor        as Character format "x(50)" label "Titulo Bco Cobrador" column-label "Titulo Bco Cobrador"
    field tta_cod_histor_padr              as character format "x(8)" label "Hist¢rico PadrÆo" column-label "Hist¢rico PadrÆo"
    field tta_des_histor_padr              as character format "x(40)" label "Descri‡Æo" column-label "Descri‡Æo Hist¢rico PadrÆo"
    field tta_ind_sit_tit_ap               as character format "X(13)" label "Situa‡Æo" column-label "Situa‡Æo"
    field tta_cod_forma_pagto              as character format "x(3)" label "Forma Pagamento" column-label "F Pagto"
    field tta_cod_tit_ap_bco_cobdor        as character format "x(20)" label "T¡tulo Banco Cobdor" column-label "T¡tulo Banco Cobdor"
    field tta_cod_estab_ext                as character format "x(8)" label "Estabelecimento Exte" column-label "Estabelecimento Ext"
    field tta_num_ord_invest               as integer format ">>>>,>>9" initial 0 label "Ordem Investimento" column-label "Ordem Investimento"
    field ttv_num_ped_compra               as integer format ">>>>>,>>9" initial 0 label "Ped Compra" column-label "Ped Compra"
    field tta_num_ord_compra               as integer format ">>>>>9,99" initial 0 label "Ordem Compra" column-label "Ordem Compra"
    field ttv_num_event_invest             as integer format ">,>>9" label "Evento Investimento" column-label "Evento Investimento"
    field ttv_val_1099                     as decimal format "->>,>>>,>>>,>>9.99" decimals 2
    field tta_cod_tax_ident_number         as character format "x(15)" label "Tax Id Number" column-label "Tax Id Number"
    field tta_ind_tip_trans_1099           as character format "X(50)" initial "Rents" label "Tipo Transacao 1099" column-label "Tipo Transacao 1099"
    field ttv_log_atualiz_tit_impto_vinc   as logical format "Sim/NÆo" initial no
    index tt_titap_id                    
            tta_cod_estab                    ascending
            tta_cdn_fornecedor               ascending
            tta_cod_espec_docto              ascending
            tta_cod_ser_docto                ascending
            tta_cod_tit_ap                   ascending
            tta_cod_parcela                  ascending.


DEF temp-table tt_tit_ap_alteracao_rateio no-undo
 field ttv_rec_tit_ap                   as recid format ">>>>>>9" initial ?
 field tta_cod_estab                    as character format "x(5)" label "Estabelecimento" column-label "Estab"
 field tta_cod_refer                    as character format "x(10)" label "Referˆncia" column-label "Referˆncia"
 field tta_num_seq_refer                as integer format ">>>9" initial 0 label "Sequˆncia" column-label "Seq"
 field tta_cod_tip_fluxo_financ         as character format "x(12)" label "Tipo Fluxo Financ" column-label "Tipo Fluxo Financ"
 field tta_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
 field tta_cod_cta_ctbl                 as character format "x(20)" label "Conta Cont bil" column-label "Conta Cont bil"
 field tta_cod_unid_negoc               as character format "x(3)" label "Unid Neg¢cio" column-label "Un Neg"
 field tta_cod_plano_ccusto             as character format "x(8)" label "Plano Centros Custo" column-label "Plano Centros Custo"
 field tta_cod_ccusto                   as Character format "x(11)" label "Centro Custo" column-label "Centro Custo"
 field tta_val_aprop_ctbl               as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor Aprop Ctbl" column-label "Vl Aprop Ctbl"
 field ttv_ind_tip_rat                  as character format "X(08)"
 field tta_num_id_tit_ap                as integer format "9999999999" initial 0 label "Token Tit AP" column-label "Token Tit AP"
 field tta_num_id_aprop_ctbl_ap         as integer format "9999999999" initial 0 label "Id Aprop Ctbl AP" column-label "Id Aprop Ctbl AP"
 index tt_aprpctba_id                   is primary unique
         tta_cod_estab                    ascending
         tta_cod_refer                    ascending
         tta_num_seq_refer                ascending
         tta_cod_plano_cta_ctbl           ascending
         tta_cod_cta_ctbl                 ascending
         tta_cod_unid_negoc               ascending
         tta_cod_plano_ccusto             ascending
         tta_cod_ccusto                   ascending
         tta_cod_tip_fluxo_financ         ascending
         ttv_rec_tit_ap                   ascending.


def temp-table tt_log_erros_tit_ap_alteracao no-undo
    field tta_cod_estab                    as character format "x(5)" label "Estabelecimento" column-label "Estab"
    field tta_cdn_fornecedor               as Integer format ">>>,>>>,>>9" initial 0 label "Fornecedor" column-label "Fornecedor"
    field tta_cod_espec_docto              as character format "x(3)" label "Esp‚cie Documento" column-label "Esp‚cie"
    field tta_cod_ser_docto                as character format "x(3)" label "S‚rie Documento" column-label "S‚rie"
    field tta_cod_tit_ap                   as character format "x(10)" label "T¡tulo" column-label "T¡tulo"
    field tta_cod_parcela                  as character format "x(02)" label "Parcela" column-label "Parc"
    field tta_num_id_tit_ap                as integer format "9999999999" initial 0 label "Token Tit AP" column-label "Token Tit AP"
    field ttv_num_mensagem                 as integer format ">>>>,>>9" label "N£mero" column-label "N£mero Mensagem"
    field ttv_cod_tip_msg_dwb              as character format "x(12)" label "Tipo Mensagem" column-label "Tipo Mensagem"
    field ttv_des_msg_erro                 as character format "x(60)" label "Mensagem Erro" column-label "Inconsistˆncia"
    field ttv_des_msg_ajuda_1              as character format "x(360)"
    field ttv_wgh_focus                    as widget-handle format ">>>>>>9".

def temp-table tt_params_generic_api no-undo
   field ttv_rec_id                       as recid format ">>>>>>9"
   field ttv_cod_tabela                   as character format "x(28)" label "Tabela" column-label "Tabela"
   field ttv_cod_campo                    as character format "x(25)" label "Campo" column-label "Campo"
   field ttv_cod_valor                    as character format "x(8)" label "Valor" column-label "Valor"
   index tt_idx_param_generic             is primary unique
       ttv_cod_tabela                   ascending
       ttv_rec_id                       ascending
       ttv_cod_campo                    ascending.

/*****************************************************************************
** Copyright DATASUL S.A. (1994)
** Todos os Direitos Reservados.
** 
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so' podera ser feita mediante
** autorizacao expressa.
**
** Programa..............: rpt_aprop_ctbl_ap_diario_aux
** Descricao.............: Di†rio Auxiliar - Contas a Pagar
** Versao................:  1.00.02.057
** Procedimento..........: rel_emitir_diario_aux_apb
** Nome Externo..........: prgfin/apb/apb319aa.py
** Data Geracao..........: 15/02/2013 - 17:41:30
** Criado por............: Uno
** Criado em.............: 08/05/1996 09:08:53
** Alterado por..........: andrefossile
** Alterado em...........: 17/04/2012 17:55:39
** Gerado por............: corp46173
*****************************************************************************/
def buffer empresa              for ems5.empresa.
def buffer histor_exec_especial for ems5.histor_exec_especial.
def buffer unid_negoc           for ems5.unid_negoc.
def buffer fornecedor           for ems5.fornecedor.
def buffer pais                 for ems5.pais.

def var c-versao-prg as char initial " 1.00.02.057":U no-undo.

{include/i_dbinst.i}
{include/i_dbtype.i}
{include/i_fcldef.i}


&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i rpt_aprop_ctbl_ap_diario_aux APB}
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
                                    "RPT_APROP_CTBL_AP_DIARIO_AUX","~~EMSFIN", "~~{~&emsfin_version}", "~~5.01")) /*msg_5009*/.
&else

/********************* Temporary Table Definition Begin *********************/

def temp-table tt_log_erros no-undo
    field ttv_num_seq                      as integer format ">>>,>>9" label "SeqÅància" column-label "Seq"
    field ttv_num_cod_erro                 as integer format ">>>>,>>9" label "N£mero" column-label "N£mero"
    field ttv_des_erro                     as character format "x(50)" label "Inconsistància" column-label "Inconsistància"
    field ttv_des_ajuda                    as character format "x(50)" label "Ajuda" column-label "Ajuda"
    index tt_id                           
          ttv_num_seq                      ascending
          ttv_num_cod_erro                 ascending
    .

def temp-table tt_log_erros_chi no-undo
    field ttv_num_seq                      as integer format ">>>,>>9" label "SeqÅància" column-label "Seq"
    field ttv_num_cod_erro                 as integer format ">>>>,>>9" label "N£mero" column-label "N£mero"
    field ttv_des_erro                     as character format "x(50)" label "Inconsistància" column-label "Inconsistància"
    field ttv_des_ajuda                    as character format "x(50)" label "Ajuda" column-label "Ajuda"
    index tt_id                           
          ttv_num_seq                      ascending
          ttv_num_cod_erro                 ascending
    .

def temp-table tt_movimentos no-undo
    field tta_cod_empresa                  as character format "x(3)" label "Empresa" column-label "Empresa"
    field tta_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field tta_cod_cta_ctbl                 as character format "x(20)" label "Conta Cont†bil" column-label "Conta Cont†bil"
    field ttv_dat_trans_diario             as date format "99/99/9999" label "Movimentos do Dia"
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_unid_negoc               as character format "x(3)" label "Unid Neg¢cio" column-label "Un Neg"
    field tta_cod_plano_ccusto             as character format "x(8)" label "Plano Centros Custo" column-label "Plano Centros Custo"
    field tta_cod_ccusto                   as Character format "x(11)" label "Centro Custo" column-label "Centro Custo"
    field ttv_val_movto                    as decimal format "->,>>>,>>>,>>9.99" decimals 2 label "Movimento" column-label "Valor Movto"
    field tta_ind_natur_lancto_ctbl        as character format "X(02)" initial "DB" label "Natureza" column-label "Natureza"
    field ttv_des_historico                as character format "x(150)" label "Hist¢rico" column-label "Hist¢rico"
    field tta_cod_docto_movto_cta_bco      as character format "x(20)" label "Documento Banco" column-label "Documento Banco"
    field tta_num_id_aprop_lancto_ctbl     as integer format "9999999999" initial 0 label "Apropriacao Lanáto" column-label "Apropriacao Lanáto"
    field tta_cod_finalid_econ             as character format "x(10)" label "Finalidade" column-label "Finalidade"
    field ttv_num_ascii                    as integer format ">>>>,>>9"
    field ttv_cod_arq_ems2                 as character format "x(16)"
    field ttv_cod_lancto_ctbl              as character format "x(20)"
    field ttv_cdn_clien_fornec             as Integer format ">>>,>>9" initial 0 column-label "Codigo Cli\Fornc"
    field ttv_cod_cta_ctbl_contra          as character format "x(20)" label "Contra Partida" column-label "Contra Partida"
    index tt_cta                          
          tta_cod_cta_ctbl                 ascending
    index tt_estab                        
          tta_cod_estab                    ascending
    .

def temp-table tt_movimentos_dec no-undo
    field tta_cod_empresa                  as character format "x(3)" label "Empresa" column-label "Empresa"
    field tta_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field tta_cod_cta_ctbl                 as character format "x(20)" label "Conta Cont†bil" column-label "Conta Cont†bil"
    field ttv_dat_trans_diario             as date format "99/99/9999" label "Movimentos do Dia"
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_unid_negoc               as character format "x(3)" label "Unid Neg¢cio" column-label "Un Neg"
    field tta_cod_plano_ccusto             as character format "x(8)" label "Plano Centros Custo" column-label "Plano Centros Custo"
    field tta_cod_ccusto                   as Character format "x(11)" label "Centro Custo" column-label "Centro Custo"
    field ttv_val_movto                    as decimal format "->,>>>,>>>,>>9.99" decimals 2 label "Movimento" column-label "Valor Movto"
    field tta_ind_natur_lancto_ctbl        as character format "X(02)" initial "DB" label "Natureza" column-label "Natureza"
    field ttv_des_historico                as character format "x(150)" label "Hist¢rico" column-label "Hist¢rico"
    field tta_cod_docto_movto_cta_bco      as character format "x(20)" label "Documento Banco" column-label "Documento Banco"
    field tta_num_id_aprop_lancto_ctbl     as integer format "9999999999" initial 0 label "Apropriacao Lanáto" column-label "Apropriacao Lanáto"
    field tta_cod_finalid_econ             as character format "x(10)" label "Finalidade" column-label "Finalidade"
    field ttv_num_ascii                    as integer format ">>>>,>>9"
    field ttv_cod_arq_ems2                 as character format "x(16)"
    field ttv_cod_lancto_ctbl              as character format "x(20)"
    field ttv_cdn_clien_fornec             as Integer format ">>>,>>9" initial 0 column-label "Codigo Cli\Fornc"
    field ttv_cod_cta_ctbl_contra          as character format "x(20)" label "Contra Partida" column-label "Contra Partida"
    index tt_cta                          
          tta_cod_cta_ctbl                 ascending
    index tt_estab                        
          tta_cod_estab                    ascending
    .

def temp-table tt_rpt_diario_auxiliar_apb no-undo
    field ttv_rec_tit_ap                   as recid format ">>>>>>9" initial ?
    field ttv_rec_movto_tit_ap             as recid format ">>>>>>9"
    field ttv_rec_aprop_ctbl_ap            as recid format ">>>>>>9"
    field tta_dat_transacao                as date format "99/99/9999" initial today label "Data Transaá∆o" column-label "Dat Transac"
    field tta_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field tta_cod_cta_ctbl                 as character format "x(20)" label "Conta Cont†bil" column-label "Conta Cont†bil"
    field ttv_ind_trans_ap_abrev           as character format "X(04)" label "Transaá∆o" column-label "Transaá∆o"
    field tta_cod_refer                    as character format "x(10)" label "Referància" column-label "Referància"
    field tta_cdn_fornecedor               as Integer format ">>>,>>>,>>9" initial 0 label "Fornecedor" column-label "Fornecedor"
    field tta_nom_abrev                    as character format "x(15)" label "Nome Abreviado" column-label "Nome Abreviado"
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_portador                 as character format "x(5)" label "Portador" column-label "Portador"
    field tta_num_cheque                   as integer format ">>>>,>>>,>>9" initial ? label "Num Cheque" column-label "Num Cheque"
    field tta_num_bord_ap                  as integer format ">>>>>9" initial 0 label "N£mero Borderì" column-label "Borderì"
    field tta_ind_natur_lancto_ctbl        as character format "X(02)" initial "DB" label "Natureza" column-label "Natureza"
    field ttv_val_aprop_ctbl_cr            as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor CrÇdito" column-label "Valor CrÇdito"
    field ttv_val_aprop_ctbl_db            as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor DÇbito" column-label "Valor DÇbito"
    field tta_cod_ccusto                   as Character format "x(11)" label "Centro Custo" column-label "Centro Custo"
    field tta_cod_plano_ccusto             as character format "x(8)" label "Plano Centros Custo" column-label "Plano Centros Custo"
    field tta_cod_unid_negoc               as character format "x(3)" label "Unid Neg¢cio" column-label "Un Neg"
    field tta_des_unid_negoc               as character format "x(40)" label "Descriá∆o" column-label "Descriá∆o"
    field ttv_cod_cta_ctbl_contra          as character format "x(20)" label "Contra Partida" column-label "Contra Partida"
    index tt_rpt_diario_auxiliar_apb      
          ttv_rec_aprop_ctbl_ap            ascending
    .

def temp-table tt_rpt_diario_auxiliar_apb_dec no-undo
    field ttv_rec_tit_ap                   as recid format ">>>>>>9" initial ?
    field ttv_rec_movto_tit_ap             as recid format ">>>>>>9"
    field ttv_rec_aprop_ctbl_ap            as recid format ">>>>>>9"
    field tta_dat_transacao                as date format "99/99/9999" initial today label "Data Transaá∆o" column-label "Dat Transac"
    field tta_cod_plano_cta_ctbl           as character format "x(8)" label "Plano Contas" column-label "Plano Contas"
    field tta_cod_cta_ctbl                 as character format "x(20)" label "Conta Cont†bil" column-label "Conta Cont†bil"
    field ttv_ind_trans_ap_abrev           as character format "X(04)" label "Transaá∆o" column-label "Transaá∆o"
    field tta_cod_refer                    as character format "x(10)" label "Referància" column-label "Referància"
    field tta_cdn_fornecedor               as Integer format ">>>,>>>,>>9" initial 0 label "Fornecedor" column-label "Fornecedor"
    field tta_nom_abrev                    as character format "x(15)" label "Nome Abreviado" column-label "Nome Abreviado"
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_portador                 as character format "x(5)" label "Portador" column-label "Portador"
    field tta_num_cheque                   as integer format ">>>>,>>>,>>9" initial ? label "Num Cheque" column-label "Num Cheque"
    field tta_num_bord_ap                  as integer format ">>>>>9" initial 0 label "N£mero Borderì" column-label "Borderì"
    field tta_ind_natur_lancto_ctbl        as character format "X(02)" initial "DB" label "Natureza" column-label "Natureza"
    field ttv_val_aprop_ctbl_cr            as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor CrÇdito" column-label "Valor CrÇdito"
    field ttv_val_aprop_ctbl_db            as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor DÇbito" column-label "Valor DÇbito"
    field tta_cod_ccusto                   as Character format "x(11)" label "Centro Custo" column-label "Centro Custo"
    field tta_cod_plano_ccusto             as character format "x(8)" label "Plano Centros Custo" column-label "Plano Centros Custo"
    field tta_cod_unid_negoc               as character format "x(3)" label "Unid Neg¢cio" column-label "Un Neg"
    field tta_des_unid_negoc               as character format "x(40)" label "Descriá∆o" column-label "Descriá∆o"
    field ttv_cod_cta_ctbl_contra          as character format "x(20)" label "Contra Partida" column-label "Contra Partida"
    index tt_rpt_diario_auxiliar_apb      
          ttv_rec_aprop_ctbl_ap            ascending
    .



/********************** Temporary Table Definition End **********************/

/************************** Buffer Definition Begin *************************/

&if "{&emsfin_version}" >= "5.01" &then
def buffer b_aprop_ctbl_ap
    for aprop_ctbl_ap.
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

def var v_cdn_estab
    as Integer
    format ">>9":U
    label "N£mero Estabelec"
    column-label "N£mero Estab"
    no-undo.
def var v_cdn_fornecedor
    as Integer
    format ">>>,>>>,>>9":U
    label "Fornecedor"
    column-label "Fornecedor"
    no-undo.
def var v_cdn_quant_cr
    as Integer
    format ">>>,>>9":U
    no-undo.
def var v_cdn_quant_db
    as Integer
    format ">>>,>>9":U
    no-undo.
def new global shared var v_cod_aplicat_dtsul_corren
    as character
    format "x(3)":U
    no-undo.
def var v_cod_cabec_label
    as character
    format "x(7)":U
    no-undo.
def var v_cod_cabec_label_aux
    as character
    format "x(14)":U
    no-undo.
def new global shared var v_cod_ccusto_corren
    as character
    format "x(11)":U
    label "Centro Custo"
    column-label "Centro Custo"
    no-undo.
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07" &THEN
def var v_cod_ccusto_fim
    as Character
    format "x(11)":U
    initial "ZZZZZZZZZZZ" /*l_ZZZZZZZZZZZ*/
    label "atÇ"
    column-label "Centro Custo"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.07" AND "{&emsfin_version}" < "9.99" &THEN
def var v_cod_ccusto_fim
    as character
    format "x(20)":U
    initial "ZZZZZZZZZZZ" /*l_ZZZZZZZZZZZ*/
    label "atÇ"
    column-label "CCusto"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07" &THEN
def var v_cod_ccusto_ini
    as Character
    format "x(11)":U
    label "CCusto Inicial"
    column-label "Centro Custo"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.07" AND "{&emsfin_version}" < "9.99" &THEN
def var v_cod_ccusto_ini
    as character
    format "x(20)":U
    label "CCusto Inicial"
    column-label "CCusto"
    no-undo.
&ENDIF
def var v_cod_cta_ajust_dec_chi_return
    as character
    format "x(8)":U
    label "Conta Cont†bil"
    column-label "Conta Cont†bil"
    no-undo.
def var v_cod_cta_ctbl
    as character
    format "x(20)":U
    label "Conta Cont†bil"
    column-label "Conta Cont†bil"
    no-undo.
def var v_cod_cta_ctbl_contra
    as character
    format "x(20)":U
    label "Contra Partida"
    column-label "Contra Partida"
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
def var v_cod_espec_docto_fim
    as character
    format "x(3)":U
    initial "ZZZ"
    label "atÇ"
    column-label "C¢digo Final"
    no-undo.
def var v_cod_espec_docto_ini
    as character
    format "x(3)":U
    label "EspÇcie"
    column-label "C¢digo Inicial"
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
def var v_cod_finalid_econ
    as character
    format "x(10)":U
    label "Finalidade Econìmica"
    column-label "Finalidade Econìmica"
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
def var v_cod_label
    as character
    format "x(8)":U
    label "Label"
    column-label "Label"
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
    radio-buttons "P†gina", "P†gina", "Folha", "Folha"
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
def new global shared var v_cod_plano_ccusto_corren
    as character
    format "x(8)":U
    label "Plano CCusto"
    column-label "Plano CCusto"
    no-undo.
def var v_cod_plano_ccusto_fim_1
    as character
    format "x(8)":U
    initial "ZZZZZZZZ"
    label "Final"
    column-label "Final"
    no-undo.
def var v_cod_plano_ccusto_ini
    as character
    format "x(8)":U
    label "Plano CCusto"
    column-label "Plano Centros Custo"
    no-undo.
def var v_cod_plano_cta_ctbl
    as character
    format "x(8)":U
    label "Plano Contas"
    column-label "Plano Contas"
    no-undo.
def var v_cod_plano_cta_ctbl_fim
    as character
    format "x(8)":U
    initial "ZZZZZZZZ" /*l_zzzzzzzz*/
    label "Final"
    column-label "Final"
    no-undo.
def var v_cod_plano_cta_ctbl_ini
    as character
    format "x(8)":U
    label "Inicial"
    column-label "Inicial"
    no-undo.
def var v_cod_portador
    as character
    format "x(5)":U
    label "Portador"
    column-label "Portador"
    no-undo.
def new shared var v_cod_release
    as character
    format "x(12)":U
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
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
def var v_cod_unid_organ
    as character
    format "x(3)":U
    label "Unid Organizacional"
    column-label "Unid Organizacional"
    no-undo.
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
def var v_cod_unid_organ
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
def var v_dat_erro
    as date
    format "99/99/9999":U
    label "Data"
    column-label "Data"
    no-undo.
def new shared var v_dat_execution
    as date
    format "99/99/9999":U
    no-undo.
def new shared var v_dat_execution_end
    as date
    format "99/99/9999":U
    no-undo.
def var v_dat_fim_diario_aux_apb
    as date
    format "99/99/9999":U
    label "Fim"
    column-label "Fim"
    no-undo.
def new shared var v_dat_fim_period
    as date
    format "99/99/9999":U
    label "Fim Per°odo"
    no-undo.
def var v_dat_inicio
    as date
    format "99/99/9999":U
    initial &IF "{&ems_dbtype}":U = "MSS":U &THEN 01/01/1800 &ELSE 01/01/0001 &ENDIF
    label "Data"
    column-label "Inicial"
    no-undo.
def var v_dat_inic_congel
    as date
    format "99/99/9999":U
    label "Dat In°cio Congel"
    column-label "Dat In°cio Congel"
    no-undo.
def var v_dat_inic_diario_aux_apb
    as date
    format "99/99/9999":U
    label "In°cio"
    column-label "In°cio"
    no-undo.
def new shared var v_dat_inic_period
    as date
    format "99/99/9999":U
    label "In°cio Per°odo"
    column-label "Per°odo"
    no-undo.
def var v_des_histor
    as character
    format "x(40)":U
    label "ContÇm"
    column-label "Hist¢rico"
    no-undo.
def var v_des_histor_diario_aux
    as character
    format "x(74)":U
    label "Hist¢rico"
    column-label "Hist¢rico"
    no-undo.
def var v_des_termo_abert
    as character
    format "x(2000)":U
    view-as editor max-chars 2000 scrollbar-vertical
    size 60 by 3
    bgcolor 15 font 2
    label "Termo Abertura"
    column-label "Termo Abertura"
    no-undo.
def var v_des_termo_encert
    as character
    format "x(2000)":U
    view-as editor max-chars 2000 scrollbar-vertical
    size 60 by 3
    bgcolor 15 font 2
    label "Termo Encerramento"
    column-label "Termo Encerramento"
    no-undo.
def var v_des_unid_negoc
    as character
    format "x(40)":U
    label "Unid Neg¢cio"
    column-label "Unid Neg¢cio"
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
def var v_ind_natur_lancto_ctbl_return
    as character
    format "X(08)":U
    initial "DB" /*l_db*/
    label "Natureza"
    column-label "Natureza"
    no-undo.
def var v_ind_trans_ap_abrev
    as character
    format "X(04)":U
    label "Transaá∆o"
    column-label "Transaá∆o"
    no-undo.
def var v_log_answer
    as logical
    format "Sim/N∆o"
    initial yes
    view-as toggle-box
    no-undo.
def var v_log_atualiz_numer_pag
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    label "Atualiza P†gina"
    column-label "Atualiza P†gina"
    no-undo.
def var v_log_contabilizacao
    as logical
    format "Sim/N∆o"
    initial ?
    no-undo.
def var v_log_dados_fisco
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    no-undo.
def var v_log_emis_termo_encert
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    label "Emite Termo Encert"
    column-label "Termo Encerramento"
    no-undo.
def new global shared var v_log_execution
    as logical
    format "Sim/N∆o"
    initial yes
    no-undo.
def var v_log_funcao_sel_cc
    as logical
    format "Sim/N∆o"
    initial no
    no-undo.
def var v_log_impr_cta_contra_partida
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    no-undo.
def var v_log_impr_histor
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    label "Imprime Hist¢rico"
    column-label "Imprime Hist¢rico"
    no-undo.
def var v_log_localiz_chi
    as logical
    format "Sim/N∆o"
    initial no
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
def var v_nom_empres_param_relat
    as character
    format "x(29)":U
    no-undo.
def new shared var v_nom_enterprise
    as character
    format "x(40)":U
    no-undo.
def var v_nom_estab_param_relat
    as character
    format "x(23)":U
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
def var v_num_acum
    as integer
    format ">>>>,>>9":U
    label "Acumulado"
    column-label "Acumulado"
    no-undo.
def var v_num_bord_ap
    as integer
    format ">>>>>9":U
    label "N£mero Borderì"
    column-label "Borderì"
    no-undo.
def var v_num_cheque
    as integer
    format ">>>>,>>>,>>9":U
    label "Num Cheque"
    column-label "Num Cheque"
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
def var v_num_ult_pag
    as integer
    format ">>>,>>9":U
    label "Èltima P†gina"
    column-label "Èltima P†gina"
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
def var v_rec_table
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.
def var v_rec_table_epc
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_usuar_financ_estab_apb
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.
def var v_val_ajust_dec_chi_return
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    no-undo.
def var v_val_movto_return
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    no-undo.
def var v_val_tot_cr_cta_ctbl
    as decimal
    format "->,>>>,>>>,>>9.99":U
    decimals 2
    label "Total CrÇdito"
    column-label "Total CrÇdito"
    no-undo.
def var v_val_tot_cr_dia
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Total CrÇditos Dia"
    column-label "Total CrÇditos Dia"
    no-undo.
def var v_val_tot_cr_period
    as decimal
    format "->>>>,>>>,>>>,>>9.99":U
    decimals 2
    label "CrÇditos Per°odo"
    column-label "CrÇditos Per°odo"
    no-undo.
def var v_val_tot_cr_plano
    as decimal
    format "->>>,>>>,>>>,>>9.99":U
    decimals 2
    label "Total CR Plano"
    column-label "CrÇditos Plano"
    no-undo.
def var v_val_tot_db_cta_ctbl
    as decimal
    format "->,>>>,>>>,>>9.99":U
    decimals 2
    label "Total DÇbito"
    column-label "Total DÇbito"
    no-undo.
def var v_val_tot_db_dia
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Total DÇbitos Dia"
    column-label "Total DÇbitos Dia"
    no-undo.
def var v_val_tot_db_period
    as decimal
    format "->>>>,>>>,>>>,>>9.99":U
    decimals 2
    label "DÇbitos Per°odo"
    column-label "DÇbitos Per°odo"
    no-undo.
def var v_val_tot_db_plano
    as decimal
    format "->>>,>>>,>>>,>>9.99":U
    decimals 2
    label "Total DB Plano"
    column-label "DÇbitos Plano"
    no-undo.
def var v_wgh_focus
    as widget-handle
    format ">>>>>>9":U
    no-undo.
def var v_wgh_frame_epc
    as widget-handle
    format ">>>>>>9":U
    no-undo.
def var v_cod_cta_ctbl_fim_aux           as character       no-undo. /*local*/
def var v_cod_cta_ctbl_inic_aux          as character       no-undo. /*local*/
def var v_cod_estab_aux                  as character       no-undo. /*local*/
def var v_dat_ult_emis                   as date            no-undo. /*local*/
def var v_log_impr_termo                 as logical         no-undo. /*local*/
def var v_log_pesq_aprop_ctbl_ap         as logical         no-undo. /*local*/
def var v_log_primei_cta_ctbl            as logical         no-undo. /*local*/
def var v_log_primei_plano               as logical         no-undo. /*local*/
def var v_log_reemis                     as logical         no-undo. /*local*/
def var v_nom_tit_aux                    as character       no-undo. /*local*/
def var v_num_cont                       as integer         no-undo. /*local*/
def var v_num_livro                      as integer         no-undo. /*local*/
def var v_num_pag                        as integer         no-undo. /*local*/
def var v_num_page                       as integer         no-undo. /*local*/


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
def rectangle rt_006
    size 1 by 1
    edge-pixels 2.
def rectangle rt_010
    size 1 by 1
    edge-pixels 2.
def rectangle rt_011
    size 1 by 1
    edge-pixels 2.
def rectangle rt_cxcf
    size 1 by 1
    fgcolor 1 edge-pixels 2.
def rectangle rt_dimensions
    size 1 by 1
    edge-pixels 2.
def rectangle rt_mold
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
def button bt_ok
    label "OK"
    tooltip "OK"
    size 1 by 1
    auto-go.
def button bt_print
    label "&Imprime"
    tooltip "Imprime"
    size 1 by 1
    auto-go.
def button bt_ran2
    label "Faixa"
    tooltip "Faixa"
&if "{&window-system}" <> "TTY" &then
    image-up file "image/im-ran"
    image-insensitive file "image/ii-ran"
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
/****************************** Function Button *****************************/
def button bt_zoo_121506
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
def new shared var v_rpt_s_1_name as character initial "Di†rio Auxiliar - APB".
def frame f_rpt_s_1_header_period header
    "------------------------------------------------------------" at 1
    "------------------------------------------------------------" at 61
    "------------------------------------------------------------" at 121
    "------------------" at 181
    "--" at 199
    "P†gina: " at 202
    (page-number (s_1) + v_rpt_s_1_page) to 215 format ">>>>>9" skip
    v_nom_enterprise at 1 format "x(40)"
    v_nom_report_title at 176 format "x(40)" skip
    "Per°odo: " at 1
    v_dat_inic_period at 10 format "99/99/9999"
    "A" at 21
    v_dat_fim_period at 23 format "99/99/9999"
    "------------------------------------------------------------" at 34
    "------------------------------------------------------------" at 94
    "----------------------------------------" at 154
    "---" at 194
    v_dat_execution at 198 format "99/99/9999"
    "-" at 209
    v_hra_execution at 211 format "99:99" skip (1)
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_header_unique header
    "------------------------------------------------------------" at 1
    "------------------------------------------------------------" at 61
    "------------------------------------------------------------" at 121
    "------------------" at 181
    "--" at 199
    "P†gina: " at 202
    (page-number (s_1) + v_rpt_s_1_page) to 215 format ">>>>>9" skip
    v_nom_enterprise at 1 format "x(40)"
    fill(" ", 40 - length(trim(v_nom_report_title))) + trim(v_nom_report_title) to 215 format "x(40)" skip
    "------------------------------------------------------------" at 1
    "------------------------------------------------------------" at 61
    "------------------------------------------------------------" at 121
    "----------" at 181
    "------" at 191
    v_dat_execution at 198 format "99/99/9999"
    "-" at 209
    v_hra_execution at 211 format "99:99" skip (1)
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_footer_last_page header
    skip (1)
    "Èltima p†gina" at 1
    "------------------------------------------------------------" at 15
    "---------------------------------------------------------" at 75
    "------------------------------------------------------------" at 132
    v_nom_prog_ext at 193 format "x(8)"
    "-" at 202
    v_cod_release at 204 format "x(12)" skip
    with no-box no-labels width 215 page-bottom stream-io.
def frame f_rpt_s_1_footer_normal header
    skip (1)
    "---------------------------------------------------------" at 1
    "------------------------------------------------------------" at 58
    "------------------------------------------------------------" at 118
    "----------" at 178
    "----" at 188
    v_nom_prog_ext at 193 format "x(8)"
    "-" at 202
    v_cod_release at 204 format "x(12)" skip
    with no-box no-labels width 215 page-bottom stream-io.
def frame f_rpt_s_1_footer_param_page header
    skip (1)
    "P†gina ParÉmetros" at 1
    "------------------------------------------------------" at 22
    "------------------------------------------------------------" at 76
    "--------------------------------------------------" at 136
    "------" at 186
    v_nom_prog_ext at 193 format "x(8)"
    "-" at 202
    v_cod_release at 204 format "x(12)" skip
    with no-box no-labels width 215 page-bottom stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_cont_partida header
    "Conta Cont†bil" at 1
    "Trans" at 22
    "Refer" at 28
    "Fornecedor" to 49
    "Nome Abrev" at 51
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Estab" at 67
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Estab" at 67
&ENDIF
    "Esp" at 73
    "SÇrie" at 77
    "T°tulo" to 98
    "/P" at 102
    "Port" at 105
    "Num Cheque" to 122
    "Valor DÇbito" to 138
    "Valor CrÇdito" to 154 skip
    "Contra Partida" at 3
    "Hist¢rico" at 24 skip
    "----------------------------------------------------------------------------------------------------------------------------------------------------------" at 1 skip
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_cont_part_cc header
    "Conta Cont†bil" at 1
    "Trans" at 22
    "Refer" at 28
    "Fornecedor" to 49
    "Nome Abrev" at 51
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Estab" at 67
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Estab" at 67
&ENDIF
    "Plano CCusto" at 73
    "Centro de Custo" at 86
    "Esp" at 107
    "SÇrie" at 111
    "T°tulo" at 117
    "/P" at 136
    "Port" at 139
    "Num Cheque" to 156
    "Valor DB" to 172
    "Valor CR" to 188 skip
    "Contra Partida" at 3
    "Hist¢rico" at 24 skip
    "--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" at 1 skip
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_diario header
    "Conta Cont†bil" at 1
    "Trans" at 22
    "Refer" at 28
    "Fornecedor" to 49
    "Nome Abrev" at 51
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Estab" at 67
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Estab" at 67
&ENDIF
    "Esp" at 73
    "SÇrie" at 77
    "T°tulo" to 98
    "/P" at 102
    "Port" at 105
    "Num Cheque" to 122
    "Valor DÇbito" to 138
    "Valor CrÇdito" to 154 skip
    "--------------------" at 1
    "-----" at 22
    "----------" at 28
    "-----------" to 49
    "---------------" at 51
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "-----" at 67
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "-----" at 67
&ENDIF
    "---" at 73
    "-----" at 77
    "----------------" to 98
    "--" at 102
    "-----" at 105
    "------------" to 122
    "---------------" to 138
    "---------------" to 154 skip
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_diario_cc header
    "Conta Cont†bil" at 1
    "Trans" at 22
    "Refer" at 28
    "Fornecedor" to 49
    "Nome Abrev" at 51
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "Estab" at 67
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "Estab" at 67
&ENDIF
    "Plano CCusto" at 73
    "Centro de Custo" at 86
    "Esp" at 107
    "SÇrie" at 111
    "T°tulo" at 117
    "/P" at 136
    "Port" at 139
    "Num Cheque" to 156
    "Valor DB" to 172
    "Valor CR" to 188 skip
    "--------------------" at 1
    "-----" at 22
    "----------" at 28
    "-----------" to 49
    "---------------" at 51
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "-----" at 67
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "-----" at 67
&ENDIF
    "------------" at 73
    "--------------------" at 86
    "---" at 107
    "-----" at 111
    "----------------" at 117
    "--" at 136
    "-----" at 139
    "------------" to 156
    "---------------" to 172
    "---------------" to 188 skip
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_detalhe_Lay_histor header skip skip
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_erro_Lay_cabecalho header skip skip skip skip (1) skip
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_erro_Lay_erro header
    "Erro" to 8
    "Inconsistància" at 10
    "Ajuda" at 61 skip
    "--------" to 8
    "--------------------------------------------------" at 10
    "--------------------------------------------------" at 61 skip
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_erro_chi_Lay_erro_ajust_c header
    "Erro" to 8
    "Inconsistància" at 10
    "Ajuda" at 86 skip
    "--------" to 8
    "---------------------------------------------------------------------------" at 10
    "--------------------------------------------------" at 86 skip
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_livro_Lay_abert header
    skip (3)
    "T E R M O  D E  A B E R T U R A     " at 51
    skip (1)
    entry(1, return-value, chr(255)) at 37 format "x(60)" skip
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_livro_Lay_encert header
    skip (3)
    "T E R M O  D E  E N C E R R A M E N T O     " at 47
    skip (1)
    entry(1, return-value, chr(255)) at 37 format "x(60)" skip
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_parametros_Lay_parametros header
    skip (1)
    "Empresa: " at 1
    v_cod_empresa at 10 format "x(3)" view-as text
    "In°cio: " at 20
    v_dat_inic_diario_aux_apb at 28 format "99/99/9999" view-as text
    "    Imprime Hist¢rico: " at 51
    v_log_impr_histor to 76 format "Sim/N∆o" view-as text
    "Gera Dados FISCO: " at 93
    v_log_dados_fisco at 111 format "Sim/N∆o" view-as text
    "   Label Cabeáalho: " at 120
    v_cod_pag_folha at 140 format "x(6)" view-as text skip
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    "  Estab: " at 1
    v_cod_estab at 10 format "x(3)" view-as text
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    "  Estab: " at 1
    v_cod_estab at 10 format "x(5)" view-as text
&ENDIF
    "Fim: " at 23
    v_dat_fim_diario_aux_apb at 28 format "99/99/9999" view-as text
    "   Atualiza P†gina: " at 54
    v_log_atualiz_numer_pag to 76 format "Sim/N∆o" view-as text
    "    Emite Termo Encert: " at 87
    v_log_emis_termo_encert to 113 format "Sim/N∆o" view-as text
    "Conta Contra Partida: " at 118
    v_log_impr_cta_contra_partida at 140 format "Sim/N∆o" view-as text skip
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_parametros_Lay_selecao header
    skip (1)
    skip (1)
    "Seleá∆o" at 58
    skip (1)
    "EspÇcie Docto   " at 1
    "-" at 18
    "Inicial: " at 23
    v_cod_espec_docto_ini at 32 format "x(3)" view-as text
    "Final: " at 53
    v_cod_espec_docto_fim at 60 format "x(3)" view-as text
    skip (1)
    "Planos de Contas" at 1
    "-" at 18
    "Inicial: " at 23
    v_cod_plano_cta_ctbl_ini at 32 format "x(8)" view-as text
    "Final: " at 53
    v_cod_plano_cta_ctbl_fim at 60 format "x(8)" view-as text
    skip (1)
    "Conta Ctbl  " at 1
    "-" at 18
    "Inicial: " at 23
    v_cod_cta_ctbl_ini at 32 format "x(20)" view-as text
    "   Conta Final: " at 53
    v_cod_cta_ctbl_fim at 69 format "x(20)" view-as text
    skip (1)
    "Unidade Neg¢cio" at 1
    "-" at 18
    v_cod_unid_negoc at 23 format "x(3)" view-as text skip
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_parametros_Lay_selecao_cc header
    skip (1)
    skip (1)
    "Seleá∆o" at 58
    skip (1)
    "EspÇcie Docto   " at 1
    "-" at 18
    "Inicial: " at 23
    v_cod_espec_docto_ini at 32 format "x(3)" view-as text
    "Final: " at 62
    v_cod_espec_docto_fim at 69 format "x(3)" view-as text
    skip (1)
    "Plano de Contas" at 1
    "-" at 18
    "Inicial: " at 23
    v_cod_plano_cta_ctbl_ini at 32 format "x(8)" view-as text
    "Final: " at 62
    v_cod_plano_cta_ctbl_fim at 69 format "x(8)" view-as text
    skip (1)
    "Conta Ctbl  " at 1
    "-" at 18
    "Inicial: " at 23
    v_cod_cta_ctbl_ini at 32 format "x(20)" view-as text
    "   Conta Final: " at 53
    v_cod_cta_ctbl_fim at 69 format "x(20)" view-as text
    skip (1)
    "Plano CCusto   " at 1
    "-" at 18
    "Inicial: " at 23
    v_cod_plano_ccusto_ini at 32 format "x(8)" view-as text
    "Final: " at 62
    v_cod_plano_ccusto_fim_1 at 69 format "x(8)" view-as text
    skip (1)
    "Centros de Custo" at 1
    "-" at 18
    "  Inicial: " at 21
    v_cod_ccusto_ini at 32 format "x(20)" view-as text
    "  Final: " at 60
    v_cod_ccusto_fim at 69 format "x(20)" view-as text
    skip (1)
    "Unidade Neg¢cio" at 1
    "-" at 18
    v_cod_unid_negoc at 23 format "x(3)" view-as text skip
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_quebra_Lay_movto header
    "Movimento do Dia:    " at 1
    /* Atributo tt_rpt_diario_auxiliar_apb.tta_dat_transacao ignorado */
    "--" at 39
    "--------------------------------" at 41
    "----------------------------------------------------------------------------------" at 73 skip (1)
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_quebra_Lay_movto_cc header
    "Movimento do Dia:    " at 1
    /* Atributo tt_rpt_diario_auxiliar_apb.tta_dat_transacao ignorado */
    "--" at 39
    "--------------------------------" at 41
    "--------------------------------------------------------------------------------------------------------------------" at 73 skip (1)
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_quebra_Lay_plano header
    /* Atributo tt_rpt_diario_auxiliar_apb.tta_cod_plano_cta_ctbl ignorado */
    "--" at 39
    "--------------------------------" at 41
    "----------------------------------------------------------------------------------" at 73 skip (1)
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_quebra_Lay_plano_cc header
    /* Atributo tt_rpt_diario_auxiliar_apb.tta_cod_plano_cta_ctbl ignorado */
    "--" at 39
    "--------------------------------" at 41
    "--------------------------------------------------------------------------------------------------------------------" at 73 skip (1)
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_total_Lay_conta header
    "---------------" to 138
    "---------------" to 154 skip
    "Total da Conta  " at 106
    ":" at 122
    v_val_tot_db_cta_ctbl to 138 format ">>>>>>>>,>>9.99" view-as text
    v_val_tot_cr_cta_ctbl to 154 format ">>>>>>>>,>>9.99" view-as text skip (1)
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_total_Lay_conta_cc header
    "---------------" at 158
    "---------------" at 174 skip
    "Total da Conta  " at 140
    ":" at 156
    v_val_tot_db_cta_ctbl to 172 format ">>>>>>>>,>>9.99" view-as text
    v_val_tot_cr_cta_ctbl to 188 format ">>>>>>>>,>>9.99" view-as text skip (1)
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_total_Lay_dia header
    "Total do Dia   " at 97
    /* Atributo tt_rpt_diario_auxiliar_apb.tta_dat_transacao ignorado */
    ":" at 122
    v_val_tot_db_dia to 138 format ">>>>>>>>,>>9.99" view-as text
    v_val_tot_cr_dia to 154 format ">>>>>>>>,>>9.99" view-as text skip (1)
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_total_Lay_dia_cc header
    "Total do Dia   " at 131
    /* Atributo tt_rpt_diario_auxiliar_apb.tta_dat_transacao ignorado */
    ":" at 156
    v_val_tot_db_dia to 172 format ">>>>>>>>,>>9.99" view-as text
    v_val_tot_cr_dia to 188 format ">>>>>>>>,>>9.99" view-as text skip (1)
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_total_Lay_period header
    "Total do Per°odo" at 103
    ":" at 122
    v_val_tot_db_period to 138 format ">>>>>>>>,>>9.99" view-as text
    v_val_tot_cr_period to 154 format ">>>>>>>>,>>9.99" view-as text skip (1)
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_total_Lay_period_cc header
    "Total do Per°odo" at 137
    ":" at 156
    v_val_tot_db_period to 172 format ">>>>>>>>,>>9.99" view-as text
    v_val_tot_cr_period to 188 format ">>>>>>>>,>>9.99" view-as text skip (1)
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_total_Lay_plano header
    "Total do Plano  " at 98
    /* Atributo tt_rpt_diario_auxiliar_apb.tta_cod_plano_cta_ctbl ignorado */
    ":" at 122
    v_val_tot_db_plano to 138 format ">>>>>>>>,>>9.99" view-as text
    v_val_tot_cr_plano to 154 format ">>>>>>>>,>>9.99" view-as text skip (1)
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_total_Lay_plano_cc header
    "Total do Plano  " at 132
    /* Atributo tt_rpt_diario_auxiliar_apb.tta_cod_plano_cta_ctbl ignorado */
    ":" at 156
    v_val_tot_db_plano to 172 format ">>>>>>>>,>>9.99" view-as text
    v_val_tot_cr_plano to 188 format ">>>>>>>>,>>9.99" view-as text skip (1)
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_Grp_unid_negoc_Lay_unid_negoc header
    /* Atributo tt_rpt_diario_auxiliar_apb.tta_cod_unid_negoc ignorado */
    "-" at 19
    /* Atributo tt_rpt_diario_auxiliar_apb.tta_des_unid_negoc ignorado */
    "----------------------------------------------------------------------------" at 69 skip (1)
    with no-box no-labels width 215 page-top stream-io.


/*************************** Report Definition End **************************/

/************************** Frame Definition Begin **************************/

def frame f_ran_01_aprop_ctbl_ap_diario_aux
    rt_mold
         at row 01.21 col 02.00
    rt_cxcf
         at row 09.08 col 02.00 bgcolor 7 
    v_cod_plano_ccusto_ini
         at row 01.42 col 17.00 colon-aligned label "Plano CCusto"
         help "C¢digo Plano Centros Custo"
         view-as fill-in
         size-chars 9.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_plano_ccusto_fim_1
         at row 01.42 col 43.29 colon-aligned label "Final"
         view-as fill-in
         size-chars 9.14 by .88
         fgcolor ? bgcolor 15 font 2
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07" &THEN
    v_cod_ccusto_ini
         at row 02.42 col 17.00 colon-aligned label "CCusto Inicial"
         help "C¢digo Centro Custo"
         view-as fill-in
         size-chars 12.14 by .88
         fgcolor ? bgcolor 15 font 2
&ENDIF
&IF "{&emsfin_version}" >= "5.07" AND "{&emsfin_version}" < "9.99" &THEN
    v_cod_ccusto_ini
         at row 02.42 col 17.00 colon-aligned label "CCusto Inicial"
         help "C¢digo Centro de Custo"
         view-as fill-in
         size-chars 21.14 by .88
         fgcolor ? bgcolor 15 font 2
&ENDIF
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07" &THEN
    v_cod_ccusto_fim
         at row 02.42 col 43.29 colon-aligned label "atÇ"
         help "C¢digo Centro Custo"
         view-as fill-in
         size-chars 12.14 by .88
         fgcolor ? bgcolor 15 font 2
&ENDIF
&IF "{&emsfin_version}" >= "5.07" AND "{&emsfin_version}" < "9.99" &THEN
    v_cod_ccusto_fim
         at row 02.42 col 43.29 colon-aligned label "atÇ"
         help "C¢digo Centro de Custo"
         view-as fill-in
         size-chars 21.14 by .88
         fgcolor ? bgcolor 15 font 2
&ENDIF
    v_cod_espec_docto_ini
         at row 03.42 col 17.00 colon-aligned label "EspÇcie"
         help "C¢digo Inicial"
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_espec_docto_fim
         at row 03.42 col 43.43 colon-aligned label "atÇ"
         help "C¢digo Final"
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_plano_cta_ctbl_ini
         at row 04.42 col 17.00 colon-aligned label "Inicial"
         help "C¢digo Plano Contas"
         view-as fill-in
         size-chars 9.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_plano_cta_ctbl_fim
         at row 04.42 col 43.43 colon-aligned label "Final"
         help "C¢digo Plano Contas"
         view-as fill-in
         size-chars 9.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_cta_ctbl_ini
         at row 05.42 col 17.00 colon-aligned label "Conta Inicial"
         help "C¢digo Conta Cont†bil"
         view-as fill-in
         size-chars 21.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_cta_ctbl_fim
         at row 05.42 col 43.43 colon-aligned label "Conta Final"
         help "C¢digo Conta Cont†bil"
         view-as fill-in
         size-chars 21.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_unid_negoc
         at row 06.42 col 17.00 colon-aligned label "Unid Neg¢cio"
         help "C¢digo Unidade Neg¢cio"
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_ok
         at row 09.29 col 03.00 font ?
         help "OK"
    bt_can
         at row 09.29 col 14.00 font ?
         help "Cancela"
    bt_hel2
         at row 09.29 col 56.14 font ?
         help "Ajuda"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 68.57 by 10.92 default-button bt_ok
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Seleá∆o".
    /* adjust size of objects in this frame */
    assign bt_can:width-chars   in frame f_ran_01_aprop_ctbl_ap_diario_aux = 10.00
           bt_can:height-chars  in frame f_ran_01_aprop_ctbl_ap_diario_aux = 01.00
           bt_hel2:width-chars  in frame f_ran_01_aprop_ctbl_ap_diario_aux = 10.00
           bt_hel2:height-chars in frame f_ran_01_aprop_ctbl_ap_diario_aux = 01.00
           bt_ok:width-chars    in frame f_ran_01_aprop_ctbl_ap_diario_aux = 10.00
           bt_ok:height-chars   in frame f_ran_01_aprop_ctbl_ap_diario_aux = 01.00
           rt_cxcf:width-chars  in frame f_ran_01_aprop_ctbl_ap_diario_aux = 65.14
           rt_cxcf:height-chars in frame f_ran_01_aprop_ctbl_ap_diario_aux = 01.42
           rt_mold:width-chars  in frame f_ran_01_aprop_ctbl_ap_diario_aux = 65.14
           rt_mold:height-chars in frame f_ran_01_aprop_ctbl_ap_diario_aux = 07.29.
    /* set private-data for the help system */
    assign v_cod_plano_ccusto_ini:private-data   in frame f_ran_01_aprop_ctbl_ap_diario_aux = "HLP=000018357":U
           v_cod_plano_ccusto_fim_1:private-data in frame f_ran_01_aprop_ctbl_ap_diario_aux = "HLP=000014957":U
           v_cod_ccusto_ini:private-data         in frame f_ran_01_aprop_ctbl_ap_diario_aux = "HLP=000019463":U
           v_cod_ccusto_fim:private-data         in frame f_ran_01_aprop_ctbl_ap_diario_aux = "HLP=000019467":U
           v_cod_espec_docto_ini:private-data    in frame f_ran_01_aprop_ctbl_ap_diario_aux = "HLP=000016628":U
           v_cod_espec_docto_fim:private-data    in frame f_ran_01_aprop_ctbl_ap_diario_aux = "HLP=000016629":U
           v_cod_plano_cta_ctbl_ini:private-data in frame f_ran_01_aprop_ctbl_ap_diario_aux = "HLP=000021819":U
           v_cod_plano_cta_ctbl_fim:private-data in frame f_ran_01_aprop_ctbl_ap_diario_aux = "HLP=000021821":U
           v_cod_cta_ctbl_ini:private-data       in frame f_ran_01_aprop_ctbl_ap_diario_aux = "HLP=000019246":U
           v_cod_cta_ctbl_fim:private-data       in frame f_ran_01_aprop_ctbl_ap_diario_aux = "HLP=000019247":U
           v_cod_unid_negoc:private-data         in frame f_ran_01_aprop_ctbl_ap_diario_aux = "HLP=000018383":U
           bt_ok:private-data                    in frame f_ran_01_aprop_ctbl_ap_diario_aux = "HLP=000010721":U
           bt_can:private-data                   in frame f_ran_01_aprop_ctbl_ap_diario_aux = "HLP=000011050":U
           bt_hel2:private-data                  in frame f_ran_01_aprop_ctbl_ap_diario_aux = "HLP=000011326":U
           frame f_ran_01_aprop_ctbl_ap_diario_aux:private-data                             = "HLP=000014957".

def frame f_rpt_41_aprop_ctbl_ap_diario_aux
    rt_002
         at row 01.21 col 02.00
    " ParÉmetros " view-as text
         at row 01.00 col 04.00 bgcolor 8 
    rt_target
         at row 10.38 col 02.00
    " Destino " view-as text
         at row 10.08 col 04.00 bgcolor 8 
    rt_run
         at row 10.38 col 48.00
    " Execuá∆o " view-as text
         at row 10.08 col 50.00
    rt_dimensions
         at row 10.38 col 72.72
    " Dimens‰es " view-as text
         at row 10.08 col 74.72
    rt_001
         at row 04.54 col 03.57
    " Per°odo " view-as text
         at row 04.24 col 05.57 bgcolor 8 
    rt_003
         at row 01.21 col 48.14
    " Seleá∆o " view-as text
         at row 01.00 col 50.14 bgcolor 8 
    rt_006
         at row 05.58 col 48.14
    " Emiss∆o Anterior " view-as text
         at row 05.28 col 50.14 bgcolor 8 
    rt_011
         at row 07.88 col 48.14
    " Label Cabeáalho " view-as text
         at row 07.58 col 50.14 bgcolor 8 
    rt_010
         at row 03.50 col 48.14
    " Fiscalizaá∆o " view-as text
         at row 03.20 col 50.14 bgcolor 8 
    rt_cxcf
         at row 13.88 col 02.00 bgcolor 7 
    v_cod_empresa
         at row 01.71 col 09.00 colon-aligned label "Emp"
         help "C¢digo Empresa"
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_nom_empres_param_relat
         at row 01.71 col 16.00 no-label
         view-as fill-in
         size-chars 30.14 by .88
         fgcolor ? bgcolor 15 font 2
&IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
    v_cod_estab
         at row 02.71 col 09.00 colon-aligned label "Estab"
         help "C¢digo Estabelecimento"
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_zoo_121506
         at row 02.71 col 15.14
&ENDIF
&IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
    v_cod_estab
         at row 02.71 col 09.00 colon-aligned label "Estab"
         help "C¢digo Estabelecimento"
         view-as fill-in
         size-chars 6.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_zoo_121506
         at row 02.71 col 17.14
&ENDIF
    v_nom_estab_param_relat
         at row 02.71 col 22.00 no-label
         view-as fill-in
         size-chars 24.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_ran2
         at row 01.71 col 51.00 font ?
         help "Faixa"
    v_dat_inic_diario_aux_apb
         at row 04.96 col 07.14 colon-aligned label "In°cio"
         help "Data de in°cio para emiss∆o do Di†rio Auxiliar."
         view-as fill-in
         size-chars 11.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_dat_fim_diario_aux_apb
         at row 05.04 col 30.00 colon-aligned label "Fim"
         help "Data final para a emiss∆o do Di†rio Auxiliar APB"
         view-as fill-in
         size-chars 11.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_log_dados_fisco
         at row 04.21 col 51.00 label "Gerar dados para arquivo de Fiscalizaá∆o"
         view-as toggle-box
    v_log_impr_histor
         at row 07.13 col 03.72 label "Imprime Hist¢rico Original"
         help "Imprime Hist¢rico Original"
         view-as toggle-box
    v_log_emis_termo_encert
         at row 07.04 col 24.00 label "Emite Termo Encer Per°odo"
         help "Emite o Termo de Encerramento ?"
         view-as toggle-box
    v_log_atualiz_numer_pag
         at row 07.88 col 03.57 label "Atualiza P†gina"
         help "Atualiza a Numeraá∆o das P†ginas?"
         view-as toggle-box
    v_log_impr_cta_contra_partida
         at row 07.88 col 21.00 label "Imprime Conta de Contra-Partida"
         view-as toggle-box
    v_num_ult_pag
         at row 06.42 col 59.57 colon-aligned label "Èltima P†gina"
         help "Èltima P†gina j† emitida."
         view-as fill-in
         size-chars 8.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_cod_pag_folha
         at row 08.50 col 51.00 no-label
         view-as radio-set Horizontal
         radio-buttons "P†gina", "P†gina", "Folha", "Folha"
          /*l_pagina*/ /*l_pagina*/ /*l_folha*/ /*l_folha*/
         bgcolor 8 
    rs_cod_dwb_output
         at row 11.08 col 03.00
         help "" no-label
    ed_1x40
         at row 12.04 col 03.00
         help "" no-label
    bt_set_printer
         at row 12.04 col 42.00 font ?
         help "Define Impressora e Layout de Impress∆o"
    bt_get_file
         at row 12.04 col 42.00 font ?
         help "Pesquisa Arquivo"
    rs_ind_run_mode
         at row 11.08 col 49.00
         help "" no-label
    v_log_print_par
         at row 12.08 col 49.00 label "Imprime ParÉmetros"
         view-as toggle-box
    v_qtd_line
         at row 11.08 col 81.00 colon-aligned
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    v_qtd_column
         at row 12.08 col 81.00 colon-aligned
         view-as fill-in
         size-chars 4.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_close
         at row 14.08 col 03.00 font ?
         help "Fecha"
    bt_print
         at row 14.08 col 14.00 font ?
         help "Imprime"
    bt_can
         at row 14.08 col 25.00 font ?
         help "Cancela"
    bt_hel2
         at row 14.08 col 77.57 font ?
         help "Ajuda"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 90.00 by 15.71
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "Di†rio Auxiliar - Contas a Pagar".
    /* adjust size of objects in this frame */
    assign bt_can:width-chars          in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 10.00
           bt_can:height-chars         in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 01.00
           bt_close:width-chars        in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 10.00
           bt_close:height-chars       in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 01.00
           bt_get_file:width-chars     in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 04.00
           bt_get_file:height-chars    in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 01.08
           bt_hel2:width-chars         in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 10.00
           bt_hel2:height-chars        in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 01.00
           bt_print:width-chars        in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 10.00
           bt_print:height-chars       in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 01.00
           bt_ran2:width-chars         in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 04.00
           bt_ran2:height-chars        in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 01.13
           bt_set_printer:width-chars  in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 04.00
           bt_set_printer:height-chars in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 01.08
           ed_1x40:width-chars         in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 38.00
           ed_1x40:height-chars        in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 01.00
           rt_001:width-chars          in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 43.00
           rt_001:height-chars         in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 01.88
           rt_002:width-chars          in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 45.00
           rt_002:height-chars         in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 08.54
           rt_003:width-chars          in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 41.43
           rt_003:height-chars         in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 01.88
           rt_006:width-chars          in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 41.43
           rt_006:height-chars         in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 01.88
           rt_010:width-chars          in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 41.43
           rt_010:height-chars         in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 01.67
           rt_011:width-chars          in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 41.43
           rt_011:height-chars         in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 01.88
           rt_cxcf:width-chars         in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 86.57
           rt_cxcf:height-chars        in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 01.42
           rt_dimensions:width-chars   in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 15.72
           rt_dimensions:height-chars  in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 03.00
           rt_run:width-chars          in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 23.86
           rt_run:height-chars         in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 03.00
           rt_target:width-chars       in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 45.00
           rt_target:height-chars      in frame f_rpt_41_aprop_ctbl_ap_diario_aux = 03.00.
    /* set return-inserted = yes for editors */
    assign ed_1x40:return-inserted in frame f_rpt_41_aprop_ctbl_ap_diario_aux = yes.
    /* set private-data for the help system */
    assign v_cod_empresa:private-data                 in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000026910":U
           v_nom_empres_param_relat:private-data      in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000019250":U
           bt_zoo_121506:private-data                 in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000009431":U
           bt_zoo_121506:private-data                 in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000009431":U
           v_cod_estab:private-data                   in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000013078":U
           v_nom_estab_param_relat:private-data       in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000019251":U
           bt_ran2:private-data                       in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000008773":U
           v_dat_inic_diario_aux_apb:private-data     in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000019249":U
           v_dat_fim_diario_aux_apb:private-data      in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000019248":U
           v_log_dados_fisco:private-data             in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000014957":U
           v_log_impr_histor:private-data             in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000019252":U
           v_log_emis_termo_encert:private-data       in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000019254":U
           v_log_atualiz_numer_pag:private-data       in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000019253":U
           v_log_impr_cta_contra_partida:private-data in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000014957":U
           v_num_ult_pag:private-data                 in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000019255":U
           v_cod_pag_folha:private-data               in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000014957":U
           rs_cod_dwb_output:private-data             in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000014957":U
           ed_1x40:private-data                       in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000014957":U
           bt_set_printer:private-data                in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000008785":U
           bt_get_file:private-data                   in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000008782":U
           rs_ind_run_mode:private-data               in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000014957":U
           v_log_print_par:private-data               in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000024662":U
           v_qtd_line:private-data                    in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000024737":U
           v_qtd_column:private-data                  in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000024669":U
           bt_close:private-data                      in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000009420":U
           bt_print:private-data                      in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000010815":U
           bt_can:private-data                        in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000011050":U
           bt_hel2:private-data                       in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "HLP=000011326":U
           frame f_rpt_41_aprop_ctbl_ap_diario_aux:private-data                                  = "HLP=000014957".
    /* enable function buttons */
    assign bt_zoo_121506:sensitive in frame f_rpt_41_aprop_ctbl_ap_diario_aux = yes
           bt_zoo_121506:sensitive in frame f_rpt_41_aprop_ctbl_ap_diario_aux = yes.
    /* move buttons to top */
    bt_zoo_121506:move-to-top().
    bt_zoo_121506:move-to-top().



{include/i_fclfrm.i f_ran_01_aprop_ctbl_ap_diario_aux f_rpt_41_aprop_ctbl_ap_diario_aux }
/*************************** Frame Definition End ***************************/

/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
def var v_prog_filtro_pdf as handle no-undo.

function getCodTipoRelat returns character in v_prog_filtro_pdf.

run prgtec/btb/btb920aa.py persistent set v_prog_filtro_pdf.

run pi_define_objetos in v_prog_filtro_pdf (frame f_rpt_41_aprop_ctbl_ap_diario_aux:handle,
                       rs_cod_dwb_output:handle in frame f_rpt_41_aprop_ctbl_ap_diario_aux,
                       bt_get_file:row in frame f_rpt_41_aprop_ctbl_ap_diario_aux,
                       bt_get_file:col in frame f_rpt_41_aprop_ctbl_ap_diario_aux).

&endif
/* tech38629 - Fim da alteraá∆o */


/*********************** User Interface Trigger Begin ***********************/


ON CHOOSE OF bt_hel2 IN FRAME f_ran_01_aprop_ctbl_ap_diario_aux
DO:


    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.


    /* End_Include: i_context_help_frame */

END. /* ON CHOOSE OF bt_hel2 IN FRAME f_ran_01_aprop_ctbl_ap_diario_aux */

ON CHOOSE OF bt_ok IN FRAME f_ran_01_aprop_ctbl_ap_diario_aux
DO:


END. /* ON CHOOSE OF bt_ok IN FRAME f_ran_01_aprop_ctbl_ap_diario_aux */

ON CHOOSE OF bt_get_file IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux
DO:

    system-dialog get-file v_cod_dwb_file
        title "Imprimir" /*l_imprimir*/ 
        filters '*.rpt' '*.rpt',
                "*.*"   "*.*"
        save-as
        create-test-file
        ask-overwrite.
        assign dwb_rpt_param.cod_dwb_file             = v_cod_dwb_file
               ed_1x40:screen-value in frame f_rpt_41_aprop_ctbl_ap_diario_aux = v_cod_dwb_file.

END. /* ON CHOOSE OF bt_get_file IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux */

ON CHOOSE OF bt_hel2 IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux
DO:


    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.


    /* End_Include: i_context_help_frame */

END. /* ON CHOOSE OF bt_hel2 IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux */

ON CHOOSE OF bt_print IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux
DO:

    run pi_choose_bt_print_rpt_aprop_ctbl_ap_diario_aux.
    if return-value = "NOK" /*l_nok*/  then
        return no-apply.
END. /* ON CHOOSE OF bt_print IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux */

ON CHOOSE OF bt_ran2 IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux
DO:

    run pi_seleciona_aprop_ctbl_ap_diario_aux /*pi_seleciona_aprop_ctbl_ap_diario_aux*/.
END. /* ON CHOOSE OF bt_ran2 IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux */

ON CHOOSE OF bt_set_printer IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux
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
               ed_1x40:screen-value in frame f_rpt_41_aprop_ctbl_ap_diario_aux = v_nom_dwb_printer
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
                with frame f_rpt_41_aprop_ctbl_ap_diario_aux.
    end /* if */.

END. /* ON CHOOSE OF bt_set_printer IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux */

ON LEAVE OF ed_1x40 IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux
DO:

    /************************* Variable Definition Begin ************************/

    def var v_cod_filename_final             as character       no-undo. /*local*/
    def var v_cod_filename_initial           as character       no-undo. /*local*/


    /************************** Variable Definition End *************************/

    block:
    do with frame f_rpt_41_aprop_ctbl_ap_diario_aux:
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

END. /* ON LEAVE OF ed_1x40 IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux */

ON VALUE-CHANGED OF rs_cod_dwb_output IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux
DO:

    initout:
    do with frame f_rpt_41_aprop_ctbl_ap_diario_aux:
        /* block: */
        case self:screen-value:
            when "Terminal" /*l_terminal*/ then ter:
             do:
                if  rs_cod_dwb_output <> "Impressora" /*l_printer*/ 
                then do:
                    assign v_qtd_line_ant = input frame f_rpt_41_aprop_ctbl_ap_diario_aux v_qtd_line.
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
                        with frame f_rpt_41_aprop_ctbl_ap_diario_aux.
                assign ed_1x40:screen-value   = ""
                       ed_1x40:sensitive      = no
                       bt_get_file:visible    = no
                       bt_set_printer:visible = no.
            end /* do ter */.
            when "Arquivo" /*l_file*/ then fil:
             do:
                if  rs_cod_dwb_output <> "Impressora" /*l_printer*/ 
                then do:
                    assign v_qtd_line_ant = input frame f_rpt_41_aprop_ctbl_ap_diario_aux v_qtd_line.
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
                        with frame f_rpt_41_aprop_ctbl_ap_diario_aux.
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

                    if  rs_ind_run_mode:screen-value in frame f_rpt_41_aprop_ctbl_ap_diario_aux <> "Batch" /*l_batch*/ 
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
                                                          + caps("apb319aa":U)
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
                    assign v_qtd_line_ant = input frame f_rpt_41_aprop_ctbl_ap_diario_aux v_qtd_line.
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
                        with frame f_rpt_41_aprop_ctbl_ap_diario_aux.
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
                with frame f_rpt_41_aprop_ctbl_ap_diario_aux.
    end /* if */.
    else do:
        enable v_qtd_line
               with frame f_rpt_41_aprop_ctbl_ap_diario_aux.
    end /* else */.
    assign rs_cod_dwb_output.

END. /* ON VALUE-CHANGED OF rs_cod_dwb_output IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux */

ON VALUE-CHANGED OF rs_ind_run_mode IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux
DO:

    do  transaction:
        find dwb_rpt_param
            where dwb_rpt_param.cod_dwb_user    = v_cod_usuar_corren
            and   dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
            exclusive-lock no-error.
        assign dwb_rpt_param.ind_dwb_run_mode = input frame f_rpt_41_aprop_ctbl_ap_diario_aux rs_ind_run_mode.

        if  dwb_rpt_param.ind_dwb_run_mode = "Batch" /*l_batch*/ 
        then do:
            if  rs_cod_dwb_output:disable("Terminal" /*l_terminal*/ ) in frame f_rpt_41_aprop_ctbl_ap_diario_aux
            then do:
            end /* if */.
        end /* if */.
        else do:
            if  rs_cod_dwb_output:enable("Terminal" /*l_terminal*/ ) in frame f_rpt_41_aprop_ctbl_ap_diario_aux
            then do:
            end /* if */.
        end /* else */.
        if  rs_ind_run_mode = "Batch" /*l_batch*/ 
        then do:
           assign v_qtd_line = v_qtd_line_ant.
           display v_qtd_line
                   with frame f_rpt_41_aprop_ctbl_ap_diario_aux.
        end /* if */.
        assign rs_ind_run_mode.
        apply "value-changed" to rs_cod_dwb_output in frame f_rpt_41_aprop_ctbl_ap_diario_aux.
    end.    

END. /* ON VALUE-CHANGED OF rs_ind_run_mode IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux */

ON LEAVE OF v_cod_estab IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux
DO:

    find estabelecimento no-lock
         where estabelecimento.cod_estab = input frame f_rpt_41_aprop_ctbl_ap_diario_aux v_cod_estab
    &if "{&emsuni_version}" >= "5.01" &then
         use-index stblcmnt_id
    &endif
          /*cl_frame_v_cod_estab of estabelecimento*/ no-error.
    if  not avail estabelecimento
    then do:
        run pi_converter_para_inteiro (Input input frame f_rpt_41_aprop_ctbl_ap_diario_aux v_cod_estab,
                                       output v_cdn_estab) /*pi_converter_para_inteiro*/.
        if  v_cdn_estab <> 0
        then do:
            find estabelecimento no-lock
                 where estabelecimento.cdn_estab = v_cdn_estab /*cl_cdn_estab of estabelecimento*/ no-error.
            if  avail estabelecimento
            then do:
                assign v_cod_estab:screen-value in frame f_rpt_41_aprop_ctbl_ap_diario_aux = estabelecimento.cod_estab.
            end /* if */.
        end /* if */.
    end /* if */.

    if  avail estabelecimento
    then do:
        assign v_nom_estab_param_relat:screen-value = estabelecimento.nom_pessoa.
        if  estabelecimento.cod_empresa = input frame f_rpt_41_aprop_ctbl_ap_diario_aux v_cod_empresa
        then do:
            assign v_cod_unid_organ = estabelecimento.cod_estab.
            find livro_fisc no-lock
                 where livro_fisc.cod_unid_organ = v_cod_unid_organ
                   and livro_fisc.cod_modul_dtsul = "APB"
                   and livro_fisc.ind_tip_livro_fisc = "Di†rio Auxiliar"
    &if "{&emsuni_version}" >= "5.01" &then
                 use-index lvrfsca_id
    &endif
                  /*cl_relat_diario_aux_apb of livro_fisc*/ no-error.
            if  avail livro_fisc
            then do:
                find first pag_livro_fisc no-lock
                     where pag_livro_fisc.cod_modul_dtsul = livro_fisc.cod_modul_dtsul
                       and pag_livro_fisc.cod_unid_organ = livro_fisc.cod_unid_organ
                       and pag_livro_fisc.ind_tip_livro_fisc = livro_fisc.ind_tip_livro_fisc
                      no-error.
                if  avail pag_livro_fisc
                then do:
                    assign v_num_ult_pag             = pag_livro_fisc.num_ult_pag
                           v_dat_inic_diario_aux_apb = pag_livro_fisc.dat_fim_emis + 1.
                    run pi_retornar_ult_dia_mes (Input v_dat_inic_diario_aux_apb,
                                                 output v_dat_fim_diario_aux_apb) /*pi_retornar_ult_dia_mes*/.
                    display v_num_ult_pag
                            v_dat_inic_diario_aux_apb
                            v_dat_fim_diario_aux_apb
                            with frame f_rpt_41_aprop_ctbl_ap_diario_aux.
                end /* if */.
            end /* if */.
        end /* if */.
    end /* if */.
    else do:
        assign v_nom_estab_param_relat:screen-value in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "".
    end /* else */.
END. /* ON LEAVE OF v_cod_estab IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux */

ON LEAVE OF v_dat_fim_diario_aux_apb IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux
DO:

    assign v_dat_erro = date(v_dat_fim_diario_aux_apb:screen-value in frame f_rpt_41_aprop_ctbl_ap_diario_aux) no-error.
    if error-status:error then do:
       /* Data invalida ! */
       run pi_messages (input "show",
                        input 14189,
                        input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_14189*/.
       apply 'entry' to v_dat_fim_diario_aux_apb in frame f_rpt_41_aprop_ctbl_ap_diario_aux.
       return no-apply.
    end.
END. /* ON LEAVE OF v_dat_fim_diario_aux_apb IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux */

ON LEAVE OF v_dat_inic_diario_aux_apb IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux
DO:

    assign v_dat_erro = date(v_dat_inic_diario_aux_apb:screen-value in frame f_rpt_41_aprop_ctbl_ap_diario_aux) no-error.
    if error-status:error then do:
       /* Data invalida ! */
       run pi_messages (input "show",
                        input 14189,
                        input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_14189*/.
       apply 'entry' to v_dat_inic_diario_aux_apb in frame f_rpt_41_aprop_ctbl_ap_diario_aux.
       return no-apply.
    end.
END. /* ON LEAVE OF v_dat_inic_diario_aux_apb IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux */

ON VALUE-CHANGED OF v_log_atualiz_numer_pag IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux
DO:

    if v_log_dados_fisco:checked then
      return no-apply.

    if  v_log_atualiz_numer_pag:checked = yes
    then do:
        enable v_log_emis_termo_encert
               with frame f_rpt_41_aprop_ctbl_ap_diario_aux.
        disable bt_ran2
                with frame f_rpt_41_aprop_ctbl_ap_diario_aux.
    end /* if */.
    else do:
        assign v_log_emis_termo_encert:checked = no.
        disable v_log_emis_termo_encert
                with frame f_rpt_41_aprop_ctbl_ap_diario_aux.
        enable bt_ran2
               with frame f_rpt_41_aprop_ctbl_ap_diario_aux.                     
    end /* else */.
END. /* ON VALUE-CHANGED OF v_log_atualiz_numer_pag IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux */

ON VALUE-CHANGED OF v_log_dados_fisco IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux
DO:

    assign input frame f_rpt_41_aprop_ctbl_ap_diario_aux v_log_dados_fisco.
    if  v_log_dados_fisco:checked = yes
    then do:
        assign v_cod_espec_docto_ini    = "":U
               v_cod_espec_docto_fim    = "ZZZ":U
               v_cod_cta_ctbl_ini       = "":U
               v_cod_cta_ctbl_fim       = "ZZZZZZZZZZZZZZZZZZZZ":U
               v_cod_plano_cta_ctbl_ini = "":U
               v_cod_plano_cta_ctbl_fim = "ZZZZZZZZ":U
               v_cod_unid_negoc         = "":U.

        if v_log_funcao_sel_cc then do:
            assign v_cod_plano_ccusto_ini   = "":U
                   v_cod_plano_ccusto_fim_1 = "ZZZZZZZZ":U
                   v_cod_ccusto_ini         = "":U
                   v_cod_ccusto_fim         = 'ZZZZZZZZZZZZZZZZZZZZ':U. 
        end.                    
        disable bt_ran2
                with frame f_rpt_41_aprop_ctbl_ap_diario_aux.    
    end /* if */.
    else do:
        if  not v_log_atualiz_numer_pag:checked
        then do:
            enable bt_ran2
                   with frame f_rpt_41_aprop_ctbl_ap_diario_aux.
        end /* if */.    
    end /* else */.    
END. /* ON VALUE-CHANGED OF v_log_dados_fisco IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux */


/************************ User Interface Trigger End ************************/

/************************** Function Trigger Begin **************************/


ON  CHOOSE OF bt_zoo_121506 IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux
OR F5 OF v_cod_estab IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux DO:

    /* fn_generic_zoom_variable */
    if  search("prgfin/apb/apb004nb.r") = ? and search("prgfin/apb/apb004nb.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgfin/apb/apb004nb.p".
        else do:
            message "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgfin/apb/apb004nb.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run prgfin/apb/apb004nb.p (Input yes,
                               Input yes,
                               Input yes,
                               Input yes,
                               Input yes,
                               Input yes,
                               Input yes,
                               Input yes,
                               Input yes,
                               Input yes,
                               Input yes,
                               Input yes,
                               Input yes,
                               Input yes,
                               Input yes,
                               Input yes,
                               Input yes,
                               Input yes,
                               Input yes,
                               Input yes,
                               Input yes,
                               Input yes,
                               Input yes,
                               Input yes,
                               Input yes,
                               Input yes,
                               Input yes,
                               Input yes) /*prg_see_usuar_financ_estab_apb*/.
    if  v_rec_usuar_financ_estab_apb <> ?
    then do:
        find estabelecimento where recid(estabelecimento) = v_rec_usuar_financ_estab_apb no-lock no-error.
        assign v_cod_estab:screen-value in frame f_rpt_41_aprop_ctbl_ap_diario_aux =
               string(estabelecimento.cod_estab).

        apply "entry" to v_cod_estab in frame f_rpt_41_aprop_ctbl_ap_diario_aux.
    end /* if */.

end. /* ON  CHOOSE OF bt_zoo_121506 IN FRAME f_rpt_41_aprop_ctbl_ap_diario_aux */


/*************************** Function Trigger End ***************************/

/**************************** Frame Trigger Begin ***************************/


ON HELP OF FRAME f_ran_01_aprop_ctbl_ap_diario_aux ANYWHERE
DO:


    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */

END. /* ON HELP OF FRAME f_ran_01_aprop_ctbl_ap_diario_aux */

ON RIGHT-MOUSE-DOWN OF FRAME f_ran_01_aprop_ctbl_ap_diario_aux ANYWHERE
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

END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_ran_01_aprop_ctbl_ap_diario_aux */

ON RIGHT-MOUSE-UP OF FRAME f_ran_01_aprop_ctbl_ap_diario_aux ANYWHERE
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

END. /* ON RIGHT-MOUSE-UP OF FRAME f_ran_01_aprop_ctbl_ap_diario_aux */

ON WINDOW-CLOSE OF FRAME f_ran_01_aprop_ctbl_ap_diario_aux
DO:

    apply "end-error" to self.
END. /* ON WINDOW-CLOSE OF FRAME f_ran_01_aprop_ctbl_ap_diario_aux */

ON ENDKEY OF FRAME f_rpt_41_aprop_ctbl_ap_diario_aux
DO:


    /* Begin_Include: i_exec_program_epc */
    &if '{&emsbas_version}' > '1.00' &then
    if  v_nom_prog_upc <> '' then
    do:
        assign v_rec_table_epc = recid(aprop_ctbl_ap).    
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
        assign v_rec_table_epc = recid(aprop_ctbl_ap).    
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
        assign v_rec_table_epc = recid(aprop_ctbl_ap).    
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

END. /* ON ENDKEY OF FRAME f_rpt_41_aprop_ctbl_ap_diario_aux */

ON GO OF FRAME f_rpt_41_aprop_ctbl_ap_diario_aux
DO:

    do transaction:
        find dwb_rpt_param
            where dwb_rpt_param.cod_dwb_user    = v_cod_usuar_corren
            and   dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
            exclusive-lock no-error.
        assign dwb_rpt_param.cod_dwb_output     = rs_cod_dwb_output:screen-value in frame f_rpt_41_aprop_ctbl_ap_diario_aux
               dwb_rpt_param.qtd_dwb_line       = input frame f_rpt_41_aprop_ctbl_ap_diario_aux v_qtd_line
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

END. /* ON GO OF FRAME f_rpt_41_aprop_ctbl_ap_diario_aux */

ON HELP OF FRAME f_rpt_41_aprop_ctbl_ap_diario_aux ANYWHERE
DO:


    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */

END. /* ON HELP OF FRAME f_rpt_41_aprop_ctbl_ap_diario_aux */

ON RIGHT-MOUSE-DOWN OF FRAME f_rpt_41_aprop_ctbl_ap_diario_aux ANYWHERE
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

END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_rpt_41_aprop_ctbl_ap_diario_aux */

ON RIGHT-MOUSE-UP OF FRAME f_rpt_41_aprop_ctbl_ap_diario_aux ANYWHERE
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

END. /* ON RIGHT-MOUSE-UP OF FRAME f_rpt_41_aprop_ctbl_ap_diario_aux */

ON WINDOW-CLOSE OF FRAME f_rpt_41_aprop_ctbl_ap_diario_aux
DO:

    apply "end-error" to self.
END. /* ON WINDOW-CLOSE OF FRAME f_rpt_41_aprop_ctbl_ap_diario_aux */


/***************************** Frame Trigger End ****************************/

/**************************** Menu Trigger Begin ****************************/


ON CHOOSE OF MENU-ITEM mi_conteudo IN MENU m_help
DO:


        apply "choose" to bt_hel2 in frame f_rpt_41_aprop_ctbl_ap_diario_aux.





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


        assign v_nom_prog     = substring(frame f_rpt_41_aprop_ctbl_ap_diario_aux:title, 1, max(1, length(frame f_rpt_41_aprop_ctbl_ap_diario_aux:title) - 10)).
        if  v_nom_prog = ? then
            assign v_nom_prog = "".

        assign v_nom_prog     = v_nom_prog
                              + chr(10)
                              + "rpt_aprop_ctbl_ap_diario_aux":U.




    assign v_nom_prog_ext = "prgfin/apb/apb319aa.py":U
           v_cod_release  = trim(" 1.00.02.057":U).
/*    run prgtec/btb/btb901zb.p (Input v_nom_prog,
                               Input v_nom_prog_ext,
                               Input v_cod_release) /*prg_fnc_about*/. */
{include/sobre5.i}
END. /* ON CHOOSE OF MENU-ITEM mi_sobre IN MENU m_help */


/***************************** Menu Trigger End *****************************/


/****************************** Main Code Begin *****************************/


/* Begin_Include: i_version_extract */
/* {include/i-ctrlrp5.i rpt_aprop_ctbl_ap_diario_aux} */


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
    run pi_version_extract ('rpt_aprop_ctbl_ap_diario_aux':U, 'prgfin/apb/apb319aa.py':U, '1.00.02.057':U, 'pro':U).
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
    run prgtec/men/men901za.py (Input 'rpt_aprop_ctbl_ap_diario_aux') /*prg_fnc_verify_security*/.
if  return-value = "2014"
then do:
    /* Programa a ser executado n∆o Ç um programa v†lido Datasul ! */
    run pi_messages (input "show",
                     input 2014,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'rpt_aprop_ctbl_ap_diario_aux')) /*msg_2014*/.
    return.
end /* if */.
if  return-value = "2012"
then do:
    /* Usu†rio sem permiss∆o para acessar o programa. */
    run pi_messages (input "show",
                     input 2012,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'rpt_aprop_ctbl_ap_diario_aux')) /*msg_2012*/.
    return.
end /* if */.
/* End_Include: i_verify_security */



/* Begin_Include: i_log_exec_prog_dtsul_ini */
assign v_rec_log = ?.

if can-find(prog_dtsul
       where prog_dtsul.cod_prog_dtsul = 'rpt_aprop_ctbl_ap_diario_aux' 
         and prog_dtsul.log_gera_log_exec = yes) then do transaction:
    create log_exec_prog_dtsul.
    assign log_exec_prog_dtsul.cod_prog_dtsul           = 'rpt_aprop_ctbl_ap_diario_aux'
           log_exec_prog_dtsul.cod_usuario              = v_cod_usuar_corren
           log_exec_prog_dtsul.dat_inic_exec_prog_dtsul = today
           log_exec_prog_dtsul.hra_inic_exec_prog_dtsul = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":":U,"":U).
    assign v_rec_log = recid(log_exec_prog_dtsul).
    release log_exec_prog_dtsul no-error.
end.


/* End_Include: i_log_exec_prog_dtsul_ini */

/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
run pi_permissoes in v_prog_filtro_pdf (input 'rpt_aprop_ctbl_ap_diario_aux':U).
&endif
/* tech38629 - Fim da alteraá∆o */




/* Begin_Include: i_verify_program_epc */
&if '{&emsbas_version}' > '1.00' &then
assign v_rec_table_epc = ?
       v_wgh_frame_epc = ?.

find prog_dtsul
    where prog_dtsul.cod_prog_dtsul = "rpt_aprop_ctbl_ap_diario_aux":U
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


assign v_wgh_frame_epc = frame f_rpt_41_aprop_ctbl_ap_diario_aux:handle.



assign v_nom_table_epc = 'aprop_ctbl_ap':U
       v_rec_table_epc = recid(aprop_ctbl_ap).

&endif

/* End_Include: i_verify_program_epc */


/* redefiniá‰es do frame */

/* Begin_Include: i_std_dialog_box */
/* tratamento do titulo e vers∆o */
assign frame f_rpt_41_aprop_ctbl_ap_diario_aux:title = frame f_rpt_41_aprop_ctbl_ap_diario_aux:title
                            + chr(32)
                            + chr(40)
                            + trim(" 1.00.02.057":U)
                            + chr(41).
/* menu pop-up de ajuda e sobre */
assign menu m_help:popup-only = yes
       bt_hel2:popup-menu in frame f_rpt_41_aprop_ctbl_ap_diario_aux = menu m_help:handle.


/* End_Include: i_std_dialog_box */
{include/title5.i f_rpt_41_aprop_ctbl_ap_diario_aux FRAME}


/* inicializa vari†veis */
find empresa no-lock
     where empresa.cod_empresa = v_cod_empres_usuar /*cl_empres_usuar of empresa*/ no-error.
find dwb_rpt_param
     where dwb_rpt_param.cod_dwb_program = "rel_emitir_diario_aux_apb":U
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
assign v_cod_dwb_proced   = "rel_emitir_diario_aux_apb":U
       v_cod_dwb_program  = "rel_emitir_diario_aux_apb":U
       v_cod_release      = trim(" 1.00.02.057":U)
       v_ind_dwb_run_mode = "On-Line" /*l_online*/ 
       v_qtd_column       = v_rpt_s_1_columns
       v_qtd_bottom       = v_rpt_s_1_bottom.
if (avail empresa) then
    assign v_nom_enterprise   = empresa.nom_razao_social.
else
    assign v_nom_enterprise   = 'DATASUL'.


/* Begin_Include: ix_p00_rpt_aprop_ctbl_ap_diario_aux */
/* Definiá∆o das frames baseada no acr709aa. As frames eram padr‰es e sua alteraá∆o iria impactar em diversos outros programas por isso foram definidas aqui. */
def frame f_rpt_s_1_header_period_auxiliar header
    "------------------------------------------------------------" at 1
    "------------------------------------------------------------" at 61
    "------------------------------------------------------------" at 121
    "------------------" at 181
    "--" at 199
    v_cod_cabec_label at 202
    (page-number (s_1) + v_rpt_s_1_page) to 215 format ">>>>>9" skip
    v_nom_enterprise at 1 format "x(40)"
    v_nom_report_title at 176 format "x(40)" skip
    "Per°odo: " at 1
    v_dat_inic_period at 10 format "99/99/9999"
    "A" at 21
    v_dat_fim_period at 23 format "99/99/9999"
    "------------------------------------------------------------" at 34
    "------------------------------------------------------------" at 94
    "----------------------------------------" at 154
    "---" at 194
    v_dat_execution at 198 format "99/99/9999"
    "-" at 209
    v_hra_execution at 211 format "99:99" skip (1)
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_header_unique_auxiliar header
    "------------------------------------------------------------" at 1
    "------------------------------------------------------------" at 61
    "------------------------------------------------------------" at 121
    "------------------" at 181
    "--" at 199
    v_cod_cabec_label at 202
    (page-number (s_1) + v_rpt_s_1_page) to 215 format ">>>>>9" skip
    v_nom_enterprise at 1 format "x(40)"
    fill(" ", 40 - length(trim(v_nom_report_title))) + trim(v_nom_report_title) to 215 format "x(40)" skip
    "------------------------------------------------------------" at 1
    "------------------------------------------------------------" at 61
    "------------------------------------------------------------" at 121
    "----------" at 181
    "------" at 191
    v_dat_execution at 198 format "99/99/9999"
    "-" at 209
    v_hra_execution at 211 format "99:99" skip (1)
    with no-box no-labels width 215 page-top stream-io.
def frame f_rpt_s_1_footer_last_page_auxiliar header
    skip (1)
    v_cod_cabec_label_aux at 1
    "------------------------------------------------------------" at 15
    "---------------------------------------------------------" at 75
    "------------------------------------------------------------" at 132
    v_nom_prog_ext at 193 format "x(8)"
    "-" at 202
    v_cod_release at 204 format "x(12)" skip
    with no-box no-labels width 215 page-bottom stream-io.

assign v_cod_ccusto_ini:format in frame f_ran_01_aprop_ctbl_ap_diario_aux = 'x(20)':U
       v_cod_ccusto_fim:format in frame f_ran_01_aprop_ctbl_ap_diario_aux = 'x(20)':U
       v_cod_ccusto_ini:width in frame f_ran_01_aprop_ctbl_ap_diario_aux = 21.14
       v_cod_ccusto_fim:width in frame f_ran_01_aprop_ctbl_ap_diario_aux = 21.14
       v_cod_ccusto_fim = 'ZZZZZZZZZZZZZZZZZZZZ':U.

/* N∆o rodar o gatilho de write, erro progress*/
  on write of aprop_ctbl_ap override do:
  end.
if not v_cod_dwb_user begins 'es_'
then do:
    run prgfin/apb/apb833aa.p /*prg_spp_aprop_ctbl_ap_acerto*/.
    if  search("prgfin/apb/apb886ac.r") = ? and search("prgfin/apb/apb886ac.py") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgfin/apb/apb886ac.py".
        else do:
            message "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgfin/apb/apb886ac.py"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run prgfin/apb/apb886ac.py /*prg_spp_aprop_ctbl_ap_dat_transa*/.
end.    

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

FUNCTION fc-transacao RETURNS INT (input p_trans AS CHARACTER):
    case p_trans :
        when "AVCR" /* Acerto Valor a Credito               */ then return 05. /* AVA - Acerto de Valor */
        when "AVDB" /* Acerto Valor a Debito                */ then return 05. /* AVA - Acerto de Valor */
        when "AVMA" /* Acerto Valor a Maior                 */ then return 05. /* AVA - Acerto de Valor */
        when "AVMN" /* Acerto Valor a Menor                 */ then return 05. /* AVA - Acerto de Valor */
        when "ADEM" /* Alteracao Data Emissao               */ then return 00.
        when "ADVN" /* Alteracao Data Vencimento            */ then return 00.
        when "ALNC" /* Alteracao nao Contabil               */ then return 00.
        when "BXSB" /* Baixa por Substituicao               */ then return 00.
        when "BXTE" /* Baixa por Transf Estab               */ then return 00.
        when "BXA"  /* Baixa                                */ then return 02. /* BND - Baixa de documentos */
        when "CPCC" /* Compra Cartao de Credito             */ then return 00.
        when "CVAL" /* Correcao de Valor                    */ then return 17. /* VC+ - Variacao cambial a maior */
        when "CVLP" /* Correcao Valor no Pagto              */ then return 00.
        when "EVCR" /* Estorno Acerto Val Credito           */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "EVDB" /* Estorno Acerto Val Debito            */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "EVMA" /* Estorno Acerto Val Maior             */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "EVMN" /* Estorno Acerto Val Menor             */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "EBXS" /* Estorno Baixa por Subst              */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "EBTE" /* Estorno Bxa Transf Estab             */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "ECVP" /* Estorno Correcao Val Pagto           */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "ECVL" /* Estorno Correcao Valor               */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "EBXA" /* Estorno de Titulo "Estorno de Baixa" */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "EPEF" /* Estorno Pagto Extra Fornecedor       */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "ESND" /* Estorno Subst Nota Dupl              */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "ETRE" /* Estorno Transf Estab                 */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "ETRU" /* Estorno Transf Unid Negoc            */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "IMCR" /* Implantacao a Credito                */ then return 16. /* NDC - Nota de debito/credito */
        when "IMDB" /* Implantacao a Debito                 */ then return 16. /* NDC - Nota de debito/credito */
        when "IMPL" /* Implantacao                          */ then return 01. /* IMD - Implantacao de documentos */
        when "PGEC" /* Pagto Encontro Contas                */ then return 00.
        when "PECR" /* Pagto Extra Fornecedor CR            */ then return 04. /* PEC - Pagto extra-fornecedor (credito) */
        when "PGEF" /* Pagto Extra Fornecedor               */ then return 03. /* PEF - Pagto extra-fornecedor (debito) */
        when "PFCC" /* PEF Cartao de Credito                */ then return 00.
        when "SBND" /* Subst Nota por Duplicata             */ then return 00. /* SND - Substituicao de Notas por Duplicatas */
        when "TRES" /* Transf Estabelecimento               */ then return 00.
        when "TRUN" /* Transf Unidade Negocio               */ then return 00.
        when "ESTT" /*                                      */ then return 00.
/*
        when "" /*  */ then return 11. /* SUS - Escritural - Sustar cancelamento titulo */
        when "" /*  */ then return 12. /* CSP - Escritural - Cancelar /Sustar Protesto */
        when "" /*  */ then return 13. /* DSV - Escritural - Alterar data de vencimento */
        when "" /*  */ then return 14. /* DSD - Escritural - Alterar data de desconto */
        when "" /*  */ then return 15. /* ALT - Escritural - Alterar outros dados */
        when "" /*  */ then return 18. /* VC- - Variacao cambial a menor */
        when "" /*  */ then return 19. /* IVA - Utilizado na Argentina para iva-liberado contabilizacao */
*/
    end case.
END FUNCTION.

/* Begin_Include: i_vrf_selecao_ccusto */
/* Rossi*/
assign v_log_funcao_sel_cc = no.
&IF DEFINED(BF_FIN_SELECAO_CCUSTO) &THEN
    assign v_log_funcao_sel_cc = yes.
&ELSE
    if  can-find(histor_exec_especial
           where histor_exec_especial.cod_modul_dtsul = "UFN" /*l_ufn*/ 
           and   histor_exec_especial.cod_prog_dtsul  = 'SPP_SELECAO_CCUSTO':U) then
        assign v_log_funcao_sel_cc = yes.
    /* ** Imprime no Extrato de Vers∆o se a funá∆o est† ou n∆o habilitada ***/

    /* Begin_Include: i_funcao_extract */
    if  v_cod_arq <> '' and v_cod_arq <> ?
    then do:

        output stream s-arq to value(v_cod_arq) append.

        put stream s-arq unformatted
            'SPP_SELECAO_CCUSTO':U      at 1 
            v_log_funcao_sel_cc  at 43 skip.

        output stream s-arq close.

    end /* if */.
    /* End_Include: i_funcao_extract */
    .
&ENDIF
/* End_Include: i_funcao_extract */


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

assign v_log_localiz_chi = GetDefinedFunction('SPP_LOCALIZ_CHILE':U).

/* Begin_Include: i_fn_retorna_ultimo_dia_mes_ano */
FUNCTION fn_retorna_ultimo_dia_mes_ano RETURN DATE (INPUT p_mes AS INT,
                                                    INPUT p_ano AS INT):
    DEF VAR v_dat_retorno AS DATE NO-UNDO.

    CASE p_mes:
        WHEN 1 OR WHEN 3 OR WHEN 5  OR
        WHEN 7 OR WHEN 8 OR WHEN 10 OR WHEN 12 THEN
            ASSIGN v_dat_retorno = DATE(p_mes,31,p_ano).
        WHEN 4  OR WHEN 6  OR
        WHEN 9  OR WHEN 11 THEN
            ASSIGN v_dat_retorno = DATE(p_mes,30,p_ano).
        WHEN 2 THEN /* Verifica se o ano Ç bissexto */
            ASSIGN v_dat_retorno = IF p_ano MODULO 4 = 0 THEN DATE(p_mes,29,p_ano)
                                   ELSE DATE(p_mes,28,p_ano).
    END CASE.

    RETURN v_dat_retorno.
END.
/* End_Include: i_fn_retorna_ultimo_dia_mes_ano */

/* End_Include: i_fn_retorna_ultimo_dia_mes_ano */

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
    if (ped_exec.cod_release_prog_dtsul <> trim(" 1.00.02.057":U)) then
        return "Vers‰es do programa diferente." /*1994*/ + " (" + "1994" + ")" + chr(10)
                                     + substitute("A vers∆o do programa (&3) que gerou o pedido de execuá∆o batch (&1) Ç diferente da vers∆o do programa que deveria executar o pedido batch (&2)." /*1994*/,ped_exec.cod_release_prog_dtsul,
                                                  trim(" 1.00.02.057":U),
                                                  "prgfin/apb/apb319aa.py":U).
    assign v_nom_prog_ext     = caps("apb319aa":U)
           v_dat_execution    = today
           v_hra_execution    = replace(string(time, "hh:mm:ss" /*l_hh:mm:ss*/ ), ":", "")
           v_cod_dwb_file     = dwb_rpt_param.cod_dwb_file
           v_nom_report_title = fill(" ", 40 - length(v_rpt_s_1_name)) + v_rpt_s_1_name
           v_ind_dwb_run_mode = "Batch" /*l_batch*/ .


    /* Begin_Include: ix_p02_rpt_aprop_ctbl_ap_diario_aux */
    /* fut36887 - atividade 139621 - inclus∆o da v_log_dados_fisco na recuperaá∆o de parÉmetros*/
    run pi_recupera_param_rpt_aprop_ctbl_ap_diario_aux /*pi_recupera_param_rpt_aprop_ctbl_ap_diario_aux*/.

    assign v_cod_cta_ctbl_inic_aux = ""
           v_cod_cta_ctbl_fim_aux  = "".

    do v_num_cont = 1 to length(v_cod_cta_ctbl_ini):
        if  substring(v_cod_cta_ctbl_ini, v_num_cont, 1) <> "." 
        and substring(v_cod_cta_ctbl_ini, v_num_cont, 1) <> "-" then
            assign v_cod_cta_ctbl_inic_aux = v_cod_cta_ctbl_inic_aux + substring(v_cod_cta_ctbl_ini, v_num_cont, 1).
    end /* do verifica_block */.
    do v_num_cont = 1 to length(v_cod_cta_ctbl_fim):
        if  substring(v_cod_cta_ctbl_fim, v_num_cont, 1) <> "." 
        and substring(v_cod_cta_ctbl_fim, v_num_cont, 1) <> "-" then
            assign v_cod_cta_ctbl_fim_aux = v_cod_cta_ctbl_fim_aux + substring(v_cod_cta_ctbl_fim, v_num_cont, 1).
    end /* do verifica_block */.
    assign v_cod_unid_organ = v_cod_empresa.
    find livro_fisc no-lock
         where livro_fisc.cod_unid_organ = v_cod_unid_organ
           and livro_fisc.cod_modul_dtsul = "APB"
           and livro_fisc.ind_tip_livro_fisc = "Di†rio Auxiliar"
    &if "{&emsuni_version}" >= "5.01" &then
         use-index lvrfsca_id
    &endif
          /*cl_relat_diario_aux_apb of livro_fisc*/ no-error.
    if  not avail livro_fisc
    then do:
        assign v_cod_unid_organ = v_cod_estab.
        find livro_fisc no-lock
             where livro_fisc.cod_unid_organ = v_cod_unid_organ
               and livro_fisc.cod_modul_dtsul = "APB"
               and livro_fisc.ind_tip_livro_fisc = "Di†rio Auxiliar"
    &if "{&emsuni_version}" >= "5.01" &then
             use-index lvrfsca_id
    &endif
              /*cl_relat_diario_aux_apb of livro_fisc*/ no-error.
        if  not avail livro_fisc
        then do:
            return "Livro Fiscal n∆o foi encontrado !" /*2439*/.
        end /* if */.
    end /* if */.

    /* Retirado o tratamento para n‰ ocorrer erro no RPW. - conforme conversado com AndrÇia e Rita*/


    find first pag_livro_fisc no-lock
         where pag_livro_fisc.cod_modul_dtsul = livro_fisc.cod_modul_dtsul
           and pag_livro_fisc.cod_unid_organ = livro_fisc.cod_unid_organ
           and pag_livro_fisc.ind_tip_livro_fisc = livro_fisc.ind_tip_livro_fisc
          no-error.

    if v_cod_estab <> "" then
        run pi_vld_aprop_ctbl_ap_diario_aux (Input yes) /*pi_vld_aprop_ctbl_ap_diario_aux*/.
    else
        run pi_vld_aprop_ctbl_ap_diario_aux (Input no) /*pi_vld_aprop_ctbl_ap_diario_aux*/.
    /* End_Include: ix_p02_rpt_aprop_ctbl_ap_diario_aux */


    /* configura e define destino de impress∆o */
    if (dwb_rpt_param.cod_dwb_output = "Impressora" /*l_printer*/ ) then
        assign v_qtd_line_ant = v_qtd_line.

    run pi_output_reports /*pi_output_reports*/.

    if  dwb_rpt_param.log_dwb_print_parameters = yes
    then do:
        if (page-number (s_1) > 0) then
            page stream s_1.


        /* Begin_Include: ix_p29_rpt_aprop_ctbl_ap_diario_aux */
        if  v_log_funcao_sel_cc and
        not v_log_funcao_sel_cc then do:
        /* End_Include: ix_p29_rpt_aprop_ctbl_ap_diario_aux */


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


        /* Begin_Include: ix_p30_rpt_aprop_ctbl_ap_diario_aux */
        end.
        if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
            page stream s_1.
        put stream s_1 unformatted 
            skip (1)
            "Usu†rio: " at 1
            v_cod_usuar_corren at 10 format "x(12)" skip (1).
        hide stream s_1 frame f_rpt_s_1_header_period_auxiliar.
        view stream s_1 frame f_rpt_s_1_header_unique_auxiliar.
        hide stream s_1 frame f_rpt_s_1_footer_last_page_auxiliar.
        hide stream s_1 frame f_rpt_s_1_footer_normal.
        view stream s_1 frame f_rpt_s_1_footer_param_page.

        if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
            page stream s_1.
        put stream s_1 unformatted 
            skip (1)
            "Empresa: " at 1
            v_cod_empresa at 10 format "x(3)"
            "In°cio: " at 20
            v_dat_inic_diario_aux_apb at 28 format "99/99/9999"
            "    Imprime Hist¢rico: " at 51
            v_log_impr_histor to 76 format "Sim/N∆o".
        put stream s_1 unformatted 
            "Gera Dados FISCO: " at 93
            v_log_dados_fisco at 111 format "Sim/N∆o"
            "   Label Cabeáalho: " at 120
            v_cod_pag_folha at 140 format "x(6)" skip
        &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
            "  Estab: " at 1
            v_cod_estab at 10 format "x(3)"
        &ENDIF
        &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
            "  Estab: " at 1.
        put stream s_1 unformatted 
            v_cod_estab at 10 format "x(5)"
        &ENDIF
            "Fim: " at 23
            v_dat_fim_diario_aux_apb at 28 format "99/99/9999"
            "   Atualiza P†gina: " at 54
            v_log_atualiz_numer_pag to 76 format "Sim/N∆o"
            "    Emite Termo Encert: " at 87
            v_log_emis_termo_encert to 113 format "Sim/N∆o"
            "Conta Contra Partida: " at 118
            v_log_impr_cta_contra_partida at 140 format "Sim/N∆o" skip.
        if not v_log_funcao_sel_cc then do:
            if (line-counter(s_1) + 11) > v_rpt_s_1_bottom then
                page stream s_1.
            put stream s_1 unformatted 
                skip (1)
                skip (1)
                "Seleá∆o" at 58
                skip (1)
                "EspÇcie Docto   " at 1
                "-" at 18
                "Inicial: " at 23.
        put stream s_1 unformatted 
                v_cod_espec_docto_ini at 32 format "x(3)"
                "Final: " at 53
                v_cod_espec_docto_fim at 60 format "x(3)"
                skip (1)
                "Planos de Contas" at 1
                "-" at 18
                "Inicial: " at 23
                v_cod_plano_cta_ctbl_ini at 32 format "x(8)"
                "Final: " at 53
                v_cod_plano_cta_ctbl_fim at 60 format "x(8)".
        put stream s_1 unformatted 
                skip (1)
                "Conta Ctbl  " at 1
                "-" at 18
                "Inicial: " at 23
                v_cod_cta_ctbl_ini at 32 format "x(20)"
                "   Conta Final: " at 53
                v_cod_cta_ctbl_fim at 69 format "x(20)"
                skip (1)
                "Unidade Neg¢cio" at 1
                "-" at 18.
        put stream s_1 unformatted 
                v_cod_unid_negoc at 23 format "x(3)" skip.
        end.
        else do:
            if (line-counter(s_1) + 15) > v_rpt_s_1_bottom then
                page stream s_1.
            put stream s_1 unformatted 
                skip (1)
                skip (1)
                "Seleá∆o" at 58
                skip (1)
                "EspÇcie Docto   " at 1
                "-" at 18
                "Inicial: " at 23.
        put stream s_1 unformatted 
                v_cod_espec_docto_ini at 32 format "x(3)"
                "Final: " at 62
                v_cod_espec_docto_fim at 69 format "x(3)"
                skip (1)
                "Plano de Contas" at 1
                "-" at 18
                "Inicial: " at 23
                v_cod_plano_cta_ctbl_ini at 32 format "x(8)"
                "Final: " at 62
                v_cod_plano_cta_ctbl_fim at 69 format "x(8)".
        put stream s_1 unformatted 
                skip (1)
                "Conta Ctbl  " at 1
                "-" at 18
                "Inicial: " at 23
                v_cod_cta_ctbl_ini at 32 format "x(20)"
                "   Conta Final: " at 53
                v_cod_cta_ctbl_fim at 69 format "x(20)"
                skip (1)
                "Plano CCusto   " at 1
                "-" at 18.
        put stream s_1 unformatted 
                "Inicial: " at 23
                v_cod_plano_ccusto_ini at 32 format "x(8)"
                "Final: " at 62
                v_cod_plano_ccusto_fim_1 at 69 format "x(8)"
                skip (1)
                "Centros de Custo" at 1
                "-" at 18
                "  Inicial: " at 21
                v_cod_ccusto_ini at 32 format "x(20)"
                "  Final: " at 60.
        put stream s_1 unformatted 
                v_cod_ccusto_fim at 69 format "x(20)"
                skip (1)
                "Unidade Neg¢cio" at 1
                "-" at 18
                v_cod_unid_negoc at 23 format "x(3)" skip.
        end.
        assign v_des_unid_negoc = "".
        /* End_Include: ix_p30_rpt_aprop_ctbl_ap_diario_aux */


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
view frame f_rpt_41_aprop_ctbl_ap_diario_aux.

/* Begin_Include: i_exec_program_epc */
&if '{&emsbas_version}' > '1.00' &then
if  v_nom_prog_upc <> '' then
do:
    assign v_rec_table_epc = recid(aprop_ctbl_ap).    
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
    assign v_rec_table_epc = recid(aprop_ctbl_ap).    
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
    assign v_rec_table_epc = recid(aprop_ctbl_ap).    
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
    do with frame f_rpt_41_aprop_ctbl_ap_diario_aux:
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
                with frame f_rpt_41_aprop_ctbl_ap_diario_aux.
    end /* do init */.

    display v_qtd_column
            v_qtd_line
            with frame f_rpt_41_aprop_ctbl_ap_diario_aux.


    /* Begin_Include: i_exec_program_epc */
    &if '{&emsbas_version}' > '1.00' &then
    if  v_nom_prog_upc <> '' then
    do:
        assign v_rec_table_epc = recid(aprop_ctbl_ap).    
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
        assign v_rec_table_epc = recid(aprop_ctbl_ap).    
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
        assign v_rec_table_epc = recid(aprop_ctbl_ap).    
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
           with frame f_rpt_41_aprop_ctbl_ap_diario_aux.


    /* Begin_Include: i_exec_program_epc */
    &if '{&emsbas_version}' > '1.00' &then
    if  v_nom_prog_upc <> '' then
    do:
        assign v_rec_table_epc = recid(aprop_ctbl_ap).    
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
        assign v_rec_table_epc = recid(aprop_ctbl_ap).    
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
        assign v_rec_table_epc = recid(aprop_ctbl_ap).    
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



    apply "value-changed" to rs_cod_dwb_output in frame f_rpt_41_aprop_ctbl_ap_diario_aux.


    if  yes = yes
    then do:
       enable rs_ind_run_mode
              with frame f_rpt_41_aprop_ctbl_ap_diario_aux.
       apply "value-changed" to rs_ind_run_mode in frame f_rpt_41_aprop_ctbl_ap_diario_aux.
    end /* if */.



    /* Begin_Include: ix_p10_rpt_aprop_ctbl_ap_diario_aux */
    assign v_cod_unid_organ = v_cod_empres_usuar.
    find livro_fisc no-lock
         where livro_fisc.cod_unid_organ = v_cod_unid_organ
           and livro_fisc.cod_modul_dtsul = "APB"
           and livro_fisc.ind_tip_livro_fisc = "Di†rio Auxiliar"
    &if "{&emsuni_version}" >= "5.01" &then
         use-index lvrfsca_id
    &endif
          /*cl_relat_diario_aux_apb of livro_fisc*/ no-error.
    if  not avail livro_fisc
    then do:
        assign v_cod_unid_organ = v_cod_estab_usuar.
        find livro_fisc no-lock
             where livro_fisc.cod_unid_organ = v_cod_unid_organ
               and livro_fisc.cod_modul_dtsul = "APB"
               and livro_fisc.ind_tip_livro_fisc = "Di†rio Auxiliar"
    &if "{&emsuni_version}" >= "5.01" &then
             use-index lvrfsca_id
    &endif
              /*cl_relat_diario_aux_apb of livro_fisc*/ no-error.
        if  not avail livro_fisc
        then do:
            /* Livro Fiscal n∆o foi encontrado ! */
            run pi_messages (input "show",
                             input 2439,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_2439*/.
            return.
        end /* if */.
    end /* if */.

    enable v_log_dados_fisco
           v_cod_estab
           v_dat_inic_diario_aux_apb
           v_dat_fim_diario_aux_apb
           v_log_impr_histor
           v_log_atualiz_numer_pag
           v_log_impr_histor
           v_cod_pag_folha
           v_log_impr_cta_contra_partida
           with frame f_rpt_41_aprop_ctbl_ap_diario_aux.

    assign v_dat_inic_congel = today.
    find sit_movimen_modul no-lock
         where sit_movimen_modul.cod_modul_dtsul = "APB"
           and sit_movimen_modul.cod_unid_organ = v_cod_empres_usuar
           and sit_movimen_modul.ind_sit_movimen = "Congelado"
    &if "{&emsuni_version}" >= "5.01" &then
         use-index stmvmnmd_id
    &endif
          /*cl_relat_diario_aux_apb of sit_movimen_modul*/ no-error.
    if  avail sit_movimen_modul
    then do:
        assign v_dat_inic_congel = sit_movimen_modul.dat_inic_sit_movimen.
    end /* if */.
    else do:
        disable v_log_atualiz_numer_pag
                with frame f_rpt_41_aprop_ctbl_ap_diario_aux.
    end /* else */.

    find empresa no-lock
         where empresa.cod_empresa = v_cod_empres_usuar /*cl_empres_usuar of empresa*/ no-error.
    assign v_cod_empresa:screen-value            = empresa.cod_empresa
           v_nom_empres_param_relat:screen-value = empresa.nom_razao_social.

    if  livro_fisc.ind_niv_abert_livro_fisc = "Estabelecimento" /*l_estabelecimento*/ 
    then do:
        find estabelecimento no-lock
             where estabelecimento.cod_estab = v_cod_estab_usuar
    &if "{&emsuni_version}" >= "5.01" &then
             use-index stblcmnt_id
    &endif
              /*cl_cod_estab_usuar of estabelecimento*/ no-error.
        assign v_cod_estab:screen-value             = estabelecimento.cod_estab
               v_nom_estab_param_relat:screen-value = estabelecimento.nom_pessoa.
        find last param_geral_apb no-lock no-error.
        if  param_geral_apb.ind_control_estab = "Assumido Direto" /*l_assumido_direto*/ 
        then do:
            disable v_cod_estab
                    bt_zoo_121506
                    with frame f_rpt_41_aprop_ctbl_ap_diario_aux.
        end /* if */.
    end /* if */.
    else do:
        disable v_cod_estab
                bt_zoo_121506
                with frame f_rpt_41_aprop_ctbl_ap_diario_aux.
        assign v_cod_estab:screen-value             in frame f_rpt_41_aprop_ctbl_ap_diario_aux = ""
               v_nom_estab_param_relat:screen-value in frame f_rpt_41_aprop_ctbl_ap_diario_aux = "".
    end /* else */.

    find first pag_livro_fisc no-lock
         where pag_livro_fisc.cod_modul_dtsul = livro_fisc.cod_modul_dtsul
           and pag_livro_fisc.cod_unid_organ = livro_fisc.cod_unid_organ
           and pag_livro_fisc.ind_tip_livro_fisc = livro_fisc.ind_tip_livro_fisc
          no-error.
    if  avail pag_livro_fisc
    then do:
        assign v_num_ult_pag             = pag_livro_fisc.num_ult_pag
               v_dat_inic_diario_aux_apb = pag_livro_fisc.dat_fim_emis + 1.
    end /* if */.
    else do:
        assign v_num_ult_pag = 0
               v_dat_inic_diario_aux_apb = v_dat_inic_congel.
    end /* else */.
    run pi_retornar_ult_dia_mes (Input v_dat_inic_diario_aux_apb,
                                 output v_dat_fim_diario_aux_apb) /*pi_retornar_ult_dia_mes*/.
    run pi_recupera_param_rpt_aprop_ctbl_ap_diario_aux /*pi_recupera_param_rpt_aprop_ctbl_ap_diario_aux*/.
    assign v_log_dados_fisco = no.  

    display v_log_dados_fisco
            v_dat_inic_diario_aux_apb
            v_dat_fim_diario_aux_apb
            v_num_ult_pag
            v_cod_pag_folha
            with frame f_rpt_41_aprop_ctbl_ap_diario_aux.
    apply 'value-changed' to v_log_dados_fisco in frame f_rpt_41_aprop_ctbl_ap_diario_aux.                          

    /* End_Include: ix_p10_rpt_aprop_ctbl_ap_diario_aux */


    block1:
    repeat on error undo block1, retry block1:

        main_block:
        repeat on error undo super_block, retry super_block
                        on endkey undo super_block, leave super_block
                        on stop undo super_block, retry super_block
                        with frame f_rpt_41_aprop_ctbl_ap_diario_aux:

            if (retry) then
                output stream s_1 close.
            assign v_log_print = no.
            if  valid-handle(v_wgh_focus) then
                wait-for go of frame f_rpt_41_aprop_ctbl_ap_diario_aux focus v_wgh_focus.
            else
                wait-for go of frame f_rpt_41_aprop_ctbl_ap_diario_aux.

            param_block:
            do transaction:

                /* Begin_Include: ix_p15_rpt_aprop_ctbl_ap_diario_aux */

                /* End_Include: ix_p15_rpt_aprop_ctbl_ap_diario_aux */

                assign dwb_rpt_param.log_dwb_print_parameters = input frame f_rpt_41_aprop_ctbl_ap_diario_aux v_log_print_par
                       dwb_rpt_param.ind_dwb_run_mode         = input frame f_rpt_41_aprop_ctbl_ap_diario_aux rs_ind_run_mode
                       input frame f_rpt_41_aprop_ctbl_ap_diario_aux v_qtd_line.

                assign dwb_rpt_param.cod_dwb_parameters = v_cod_empresa                     + chr(10) +
v_cod_estab                       + chr(10) +
string(v_dat_inic_diario_aux_apb) + chr(10) +
string(v_dat_fim_diario_aux_apb)  + chr(10) +
string(v_log_impr_histor)         + chr(10) +
string(v_log_atualiz_numer_pag)   + chr(10) +
string(v_log_emis_termo_encert)   + chr(10) +
v_cod_espec_docto_ini             + chr(10) +
v_cod_espec_docto_fim             + chr(10) +
v_cod_cta_ctbl_ini                + chr(10) +
v_cod_cta_ctbl_fim                + chr(10) +
v_cod_plano_cta_ctbl_ini          + chr(10) +
v_cod_plano_cta_ctbl_fim          + chr(10) +
string(v_log_reemis)              + chr(10) +
v_cod_plano_ccusto_ini            + chr(10) +
v_cod_plano_ccusto_fim_1          + chr(10) +
v_cod_ccusto_ini                  + chr(10) +
v_cod_ccusto_fim                  + chr(10) +
string(v_log_dados_fisco)         + chr(10) +
v_cod_pag_folha                   + chr(10) +
v_cod_unid_negoc                  + chr(10) +
string(v_log_impr_cta_contra_partida).

                /* ix_p20_rpt_aprop_ctbl_ap_diario_aux */
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
                            assign v_cod_dwb_file   = session:temp-directory + substring ("prgfin/apb/apb319aa.py", 12, 6) + '.tmp'
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
                    assign v_nom_prog_ext  = caps(substring("prgfin/apb/apb319aa.py",12,8))
                           v_cod_release   = trim(" 1.00.02.057":U)
                           v_dat_execution = today
                           v_hra_execution = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":","").
                    run pi_rpt_aprop_ctbl_ap_diario_aux /*pi_rpt_aprop_ctbl_ap_diario_aux*/.
                end /* else */.
                if  dwb_rpt_param.log_dwb_print_parameters = yes
                then do:
                    if (page-number (s_1) > 0) then
                        page stream s_1.

                    /* Begin_Include: ix_p29_rpt_aprop_ctbl_ap_diario_aux */
                    if  v_log_funcao_sel_cc and
                    not v_log_funcao_sel_cc then do:
                    /* End_Include: ix_p29_rpt_aprop_ctbl_ap_diario_aux */

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

                    /* Begin_Include: ix_p30_rpt_aprop_ctbl_ap_diario_aux */
                    end.
                    if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        skip (1)
                        "Usu†rio: " at 1
                        v_cod_usuar_corren at 10 format "x(12)" skip (1).
                    hide stream s_1 frame f_rpt_s_1_header_period_auxiliar.
                    view stream s_1 frame f_rpt_s_1_header_unique_auxiliar.
                    hide stream s_1 frame f_rpt_s_1_footer_last_page_auxiliar.
                    hide stream s_1 frame f_rpt_s_1_footer_normal.
                    view stream s_1 frame f_rpt_s_1_footer_param_page.

                    if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        skip (1)
                        "Empresa: " at 1
                        v_cod_empresa at 10 format "x(3)"
                        "In°cio: " at 20
                        v_dat_inic_diario_aux_apb at 28 format "99/99/9999"
                        "    Imprime Hist¢rico: " at 51
                        v_log_impr_histor to 76 format "Sim/N∆o".
                    put stream s_1 unformatted 
                        "Gera Dados FISCO: " at 93
                        v_log_dados_fisco at 111 format "Sim/N∆o"
                        "   Label Cabeáalho: " at 120
                        v_cod_pag_folha at 140 format "x(6)" skip
                    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                        "  Estab: " at 1
                        v_cod_estab at 10 format "x(3)"
                    &ENDIF
                    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                        "  Estab: " at 1.
                    put stream s_1 unformatted 
                        v_cod_estab at 10 format "x(5)"
                    &ENDIF
                        "Fim: " at 23
                        v_dat_fim_diario_aux_apb at 28 format "99/99/9999"
                        "   Atualiza P†gina: " at 54
                        v_log_atualiz_numer_pag to 76 format "Sim/N∆o"
                        "    Emite Termo Encert: " at 87
                        v_log_emis_termo_encert to 113 format "Sim/N∆o"
                        "Conta Contra Partida: " at 118
                        v_log_impr_cta_contra_partida at 140 format "Sim/N∆o" skip.
                    if not v_log_funcao_sel_cc then do:
                        if (line-counter(s_1) + 11) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            skip (1)
                            skip (1)
                            "Seleá∆o" at 58
                            skip (1)
                            "EspÇcie Docto   " at 1
                            "-" at 18
                            "Inicial: " at 23.
                    put stream s_1 unformatted 
                            v_cod_espec_docto_ini at 32 format "x(3)"
                            "Final: " at 53
                            v_cod_espec_docto_fim at 60 format "x(3)"
                            skip (1)
                            "Planos de Contas" at 1
                            "-" at 18
                            "Inicial: " at 23
                            v_cod_plano_cta_ctbl_ini at 32 format "x(8)"
                            "Final: " at 53
                            v_cod_plano_cta_ctbl_fim at 60 format "x(8)".
                    put stream s_1 unformatted 
                            skip (1)
                            "Conta Ctbl  " at 1
                            "-" at 18
                            "Inicial: " at 23
                            v_cod_cta_ctbl_ini at 32 format "x(20)"
                            "   Conta Final: " at 53
                            v_cod_cta_ctbl_fim at 69 format "x(20)"
                            skip (1)
                            "Unidade Neg¢cio" at 1
                            "-" at 18.
                    put stream s_1 unformatted 
                            v_cod_unid_negoc at 23 format "x(3)" skip.
                    end.
                    else do:
                        if (line-counter(s_1) + 15) > v_rpt_s_1_bottom then
                            page stream s_1.
                        put stream s_1 unformatted 
                            skip (1)
                            skip (1)
                            "Seleá∆o" at 58
                            skip (1)
                            "EspÇcie Docto   " at 1
                            "-" at 18
                            "Inicial: " at 23.
                    put stream s_1 unformatted 
                            v_cod_espec_docto_ini at 32 format "x(3)"
                            "Final: " at 62
                            v_cod_espec_docto_fim at 69 format "x(3)"
                            skip (1)
                            "Plano de Contas" at 1
                            "-" at 18
                            "Inicial: " at 23
                            v_cod_plano_cta_ctbl_ini at 32 format "x(8)"
                            "Final: " at 62
                            v_cod_plano_cta_ctbl_fim at 69 format "x(8)".
                    put stream s_1 unformatted 
                            skip (1)
                            "Conta Ctbl  " at 1
                            "-" at 18
                            "Inicial: " at 23
                            v_cod_cta_ctbl_ini at 32 format "x(20)"
                            "   Conta Final: " at 53
                            v_cod_cta_ctbl_fim at 69 format "x(20)"
                            skip (1)
                            "Plano CCusto   " at 1
                            "-" at 18.
                    put stream s_1 unformatted 
                            "Inicial: " at 23
                            v_cod_plano_ccusto_ini at 32 format "x(8)"
                            "Final: " at 62
                            v_cod_plano_ccusto_fim_1 at 69 format "x(8)"
                            skip (1)
                            "Centros de Custo" at 1
                            "-" at 18
                            "  Inicial: " at 21
                            v_cod_ccusto_ini at 32 format "x(20)"
                            "  Final: " at 60.
                    put stream s_1 unformatted 
                            v_cod_ccusto_fim at 69 format "x(20)"
                            skip (1)
                            "Unidade Neg¢cio" at 1
                            "-" at 18
                            v_cod_unid_negoc at 23 format "x(3)" skip.
                    end.
                    assign v_des_unid_negoc = "".
                    /* End_Include: ix_p30_rpt_aprop_ctbl_ap_diario_aux */

                end /* if */.
                output stream s_1 close.

/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
run pi_call_convert_object in v_prog_filtro_pdf (input no,
                                                 input rs_cod_dwb_output:screen-value in frame f_rpt_41_aprop_ctbl_ap_diario_aux,
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

        /* ix_p32_rpt_aprop_ctbl_ap_diario_aux */

        if  v_num_ped_exec <> 0
        then do:
            /* Criado pedido &1 para execuá∆o batch. */
            run pi_messages (input "show",
                             input 3556,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                v_num_ped_exec)) /*msg_3556*/.
            assign v_num_ped_exec = 0.
        end /* if */.

        /* ix_p35_rpt_aprop_ctbl_ap_diario_aux */

    end /* repeat block1 */.
end /* repeat super_block */.

/* ix_p40_rpt_aprop_ctbl_ap_diario_aux */

hide frame f_rpt_41_aprop_ctbl_ap_diario_aux.

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
    do with frame f_rpt_41_aprop_ctbl_ap_diario_aux:

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
/*
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
*/
    run pi_rpt_aprop_ctbl_ap_diario_aux /*pi_rpt_aprop_ctbl_ap_diario_aux*/.
END PROCEDURE. /* pi_output_reports */
/*****************************************************************************
** Procedure Interna.....: pi_ler_tt_rpt_aprop_ctbl_ap_diario_aux
** Descricao.............: pi_ler_tt_rpt_aprop_ctbl_ap_diario_aux
** Criado por............: Uno
** Criado em.............: 08/05/1996 09:47:39
** Alterado por..........: fut41506
** Alterado em...........: 28/08/2008 16:42:10
*****************************************************************************/
PROCEDURE pi_ler_tt_rpt_aprop_ctbl_ap_diario_aux:

    /************************* Variable Definition Begin ************************/

    def var v_num_seq_chi_aux
        as integer
        format ">>>>,>>9":U
        label "SeqÅància"
        column-label "Seq"
        no-undo.


    /************************** Variable Definition End *************************/

    assign v_log_pesq_aprop_ctbl_ap = no.
    &if '{&emsfin_version}' >= "5.02" &then
        if v_cod_cta_ctbl_inic_aux = v_cod_cta_ctbl_fim_aux then do:
            assign v_log_pesq_aprop_ctbl_ap = yes.
        end.
    &endif.

    find unid_negoc
      where unid_negoc.cod_unid_negoc = v_cod_unid_negoc no-lock no-error.
    if avail unid_negoc then do:
        assign v_des_unid_negoc  = unid_negoc.des_unid_negoc.
    end. 
    else do:
        assign v_des_unid_negoc = "".
    end.

    assign v_cod_estab_aux = ?.
    data_block:
    do v_dat_inicio = v_dat_inic_diario_aux_apb to v_dat_fim_diario_aux_apb:
        if  v_log_pesq_aprop_ctbl_ap = no
        then do:
            if  livro_fisc.ind_niv_abert_livro_fisc = "Estabelecimento" /*l_estabelecimento*/ 
            then do:
                movto_block:
                for each movto_tit_ap no-lock
                 where movto_tit_ap.cod_estab = v_cod_estab
                   and movto_tit_ap.dat_transacao = v_dat_inicio
    &if "{&emsfin_version}" >= "5.01" &then
                 use-index mvtttp_estab_dat_trans
    &endif
                  /*cl_relat_diario_aux_estab of movto_tit_ap*/:
                    if  movto_tit_ap.log_ctbz_aprop_ctbl = yes
                    then do:
                        assign v_ind_trans_ap_abrev = movto_tit_ap.ind_trans_ap_abrev.
                        run pi_tratar_tt_rpt_aprop_ctbl_ap_diario_aux /*pi_tratar_tt_rpt_aprop_ctbl_ap_diario_aux*/.
                    end /* if */.
                end /* for movto_block */.
            end /* if */.
            else do:
                for each estabelecimento no-lock
                    where estabelecimento.cod_empresa = v_cod_empres_usuar:
                    movto_block:
                    for each movto_tit_ap no-lock
                     where movto_tit_ap.cod_estab = estabelecimento.cod_estab
                       and movto_tit_ap.dat_transacao = v_dat_inicio /*cl_relat_diario_aux_empres of movto_tit_ap*/:
                        if  movto_tit_ap.log_ctbz_aprop_ctbl = yes
                        then do:
                            assign v_ind_trans_ap_abrev = movto_tit_ap.ind_trans_ap_abrev.
                            run pi_tratar_tt_rpt_aprop_ctbl_ap_diario_aux /*pi_tratar_tt_rpt_aprop_ctbl_ap_diario_aux*/.
                        end /* if */.
                    end /* for movto_block */.
                end.
            end /* else */.
        end /* if */.
        else do:
            &if '{&emsfin_version}' >= "5.02" &then
                for each plano_cta_ctbl no-lock
                    where plano_cta_ctbl.cod_plano_cta_ctbl >= v_cod_plano_cta_ctbl_ini
                    and   plano_cta_ctbl.cod_plano_cta_ctbl <= v_cod_plano_cta_ctbl_fim:
                    if  livro_fisc.ind_niv_abert_livro_fisc = "Estabelecimento" /*l_estabelecimento*/ 
                    then do:
                        for each aprop_ctbl_ap no-lock
                            where aprop_ctbl_ap.cod_estab           = v_cod_estab
                            and   aprop_ctbl_ap.cod_plano_cta_ctbl  = plano_cta_ctbl.cod_plano_cta_ctbl
                            and   aprop_ctbl_ap.cod_cta_ctbl        = v_cod_cta_ctbl_inic_aux
                            and   aprop_ctbl_ap.dat_transacao       = v_dat_inicio                        
                            and   aprop_ctbl_ap.log_ctbz_aprop_ctbl = yes
                            use-index aprpctbl_estab_cta_ctbl_data:
                            if v_cod_unid_negoc <> "" and aprop_ctbl_ap.cod_unid_negoc <> v_cod_unid_negoc then next.
                            run pi_tratar_tt_rpt_movto_tit_ap_diario_aux /*pi_tratar_tt_rpt_movto_tit_ap_diario_aux*/.
                        end.    
                    end /* if */.
                    else do:
                        for each estabelecimento no-lock
                            where estabelecimento.cod_empresa = v_cod_empres_usuar:
                            for each aprop_ctbl_ap no-lock
                                where aprop_ctbl_ap.cod_estab           = estabelecimento.cod_estab
                                and   aprop_ctbl_ap.cod_plano_cta_ctbl  = plano_cta_ctbl.cod_plano_cta_ctbl
                                and   aprop_ctbl_ap.cod_cta_ctbl        = v_cod_cta_ctbl_inic_aux
                                and   aprop_ctbl_ap.dat_transacao       = v_dat_inicio                   
                                and   aprop_ctbl_ap.log_ctbz_aprop_ctbl = yes
                                use-index aprpctbl_estab_cta_ctbl_data:
                                if v_cod_unid_negoc <> "" and aprop_ctbl_ap.cod_unid_negoc <> v_cod_unid_negoc then next.
                                run pi_tratar_tt_rpt_movto_tit_ap_diario_aux /*pi_tratar_tt_rpt_movto_tit_ap_diario_aux*/.
                            end.
                        end.    
                    end /* else */.
                end.    
            &endif        
        end /* else */.
    end /* do data_block */.
    if  v_log_dados_fisco = yes
    then do:

        if  livro_fisc.ind_niv_abert_livro_fisc = "Estabelecimento" /*l_estabelecimento*/  then do:

            find first tt_rpt_diario_auxiliar_apb no-lock
                 where tt_rpt_diario_auxiliar_apb.tta_cod_estab = v_cod_estab no-error.
            if not avail tt_rpt_diario_auxiliar_apb then do:

                create tt_movimentos.
                assign tt_movimentos.tta_cod_empresa           = v_cod_empres_usuar          
                       tt_movimentos.tta_cod_estab             = v_cod_estab                        
                       tt_movimentos.tta_cod_unid_negoc        = ""          
                       tt_movimentos.tta_cod_plano_cta_ctbl    = ""     
                       tt_movimentos.tta_cod_cta_ctbl          = ""          
                       tt_movimentos.ttv_dat_trans_diario      = v_dat_inic_diario_aux_apb           
                       tt_movimentos.tta_cod_plano_ccusto      = ""        
                       tt_movimentos.tta_cod_ccusto            = ""               
                       tt_movimentos.ttv_val_movto             = 0    
                       tt_movimentos.tta_ind_natur_lancto_ctbl = ""
                       tt_movimentos.ttv_des_historico         = ""
                       tt_movimentos.tta_cod_finalid_econ      = ""  
                       tt_movimentos.ttv_cod_arq_ems2          = ""                  
                       tt_movimentos.ttv_cod_lancto_ctbl       = ""
                       tt_movimentos.ttv_cdn_clien_fornec      = 0
                       tt_movimentos.ttv_cod_cta_ctbl_contra   = "".

            end.
        end.
        else do:
            for each estabelecimento no-lock
                where estabelecimento.cod_empresa = v_cod_empres_usuar:

                find first tt_rpt_diario_auxiliar_apb no-lock
                     where tt_rpt_diario_auxiliar_apb.tta_cod_estab = estabelecimento.cod_estab no-error.
                if not avail tt_rpt_diario_auxiliar_apb then do:

                    create tt_movimentos.
                    assign tt_movimentos.tta_cod_empresa           = v_cod_empres_usuar          
                           tt_movimentos.tta_cod_estab             = estabelecimento.cod_estab                        
                           tt_movimentos.tta_cod_unid_negoc        = ""          
                           tt_movimentos.tta_cod_plano_cta_ctbl    = ""     
                           tt_movimentos.tta_cod_cta_ctbl          = ""          
                           tt_movimentos.ttv_dat_trans_diario      = v_dat_inic_diario_aux_apb           
                           tt_movimentos.tta_cod_plano_ccusto      = ""        
                           tt_movimentos.tta_cod_ccusto            = ""               
                           tt_movimentos.ttv_val_movto             = 0    
                           tt_movimentos.tta_ind_natur_lancto_ctbl = ""
                           tt_movimentos.ttv_des_historico         = ""
                           tt_movimentos.tta_cod_finalid_econ      = ""  
                           tt_movimentos.ttv_cod_arq_ems2          = ""                  
                           tt_movimentos.ttv_cod_lancto_ctbl       = ""
                           tt_movimentos.ttv_cdn_clien_fornec      = 0
                           tt_movimentos.ttv_cod_cta_ctbl_contra   = "".

                end.    
            end.
        end.
        if v_log_localiz_chi then do:
            run pi_tratar_tt_movimentos_chile_apb.
        end.    
        if  search("prgfin/fgl/fgl700zb.r") = ? and search("prgfin/fgl/fgl700zb.py") = ? then do:
            if  v_cod_dwb_user begins 'es_' then
                return "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgfin/fgl/fgl700zb.py".
            else do:
                message "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgfin/fgl/fgl700zb.py"
                       view-as alert-box error buttons ok.
                return.
            end.
        end.
        else
            run prgfin/fgl/fgl700zb.py (Input 1,
                                    Input "APB" /*l_apb*/,
                                    Input table tt_movimentos,
                                    Input v_dat_inic_diario_aux_apb,
                                    Input v_dat_fim_diario_aux_apb,
                                    output table tt_log_erros) /*prg_api_sdo_cta_ctbl_modul*/.

        &IF INTEGER(ENTRY(1,PROVERSION,".")) >= 9 &THEN
            EMPTY TEMP-TABLE tt_movimentos NO-ERROR.
        &ELSE
            for each tt_movimentos exclusive-lock:
                delete tt_movimentos.
            end.
        &ENDIF
    end /* if */.
    if v_log_localiz_chi then do:
        assign v_num_seq_chi_aux = 0.
        for each tt_log_erros_chi no-lock:
            find last tt_log_erros no-lock no-error.
                if avail tt_log_erros then
                    assign v_num_seq_chi_aux = tt_log_erros.ttv_num_seq + 1.
                else
                    assign v_num_seq_chi_aux = 1.

                create tt_log_erros. 
                assign tt_log_erros.ttv_num_seq      = v_num_seq_chi_aux
                       tt_log_erros.ttv_num_cod_erro = tt_log_erros_chi.ttv_num_cod_erro
                       tt_log_erros.ttv_des_erro     = tt_log_erros_chi.ttv_des_erro
                       tt_log_erros.ttv_des_ajuda     = tt_log_erros_chi.ttv_des_ajuda.
        end.
    end.
END PROCEDURE. /* pi_ler_tt_rpt_aprop_ctbl_ap_diario_aux */
/*****************************************************************************
** Procedure Interna.....: pi_tratar_tt_rpt_aprop_ctbl_ap_diario_aux
** Descricao.............: pi_tratar_tt_rpt_aprop_ctbl_ap_diario_aux
** Criado por............: Uno
** Criado em.............: 08/05/1996 09:48:15
** Alterado por..........: fut41506
** Alterado em...........: 13/02/2008 19:33:24
*****************************************************************************/
PROCEDURE pi_tratar_tt_rpt_aprop_ctbl_ap_diario_aux:

    /* TRATANDO A TRANSAÄ«O DO TIPO PAGAMENTO EXTRA FORNECEDOR */
    if  v_ind_trans_ap_abrev <> "PGEF" /*l_pgef*/ 
    and v_ind_trans_ap_abrev <> "PECR" /*l_pecr*/ 
    and v_ind_trans_ap_abrev <> "EPEF" /*l_epef*/ 
    then do:
        find tit_ap no-lock
             where tit_ap.cod_estab = movto_tit_ap.cod_estab
               and tit_ap.num_id_tit_ap = movto_tit_ap.num_id_tit_ap
              no-error.
        if avail tit_ap 
        then  do:
            /* TRATANDO A SELEÄ«O DE ESPêCIE DE DOCUMENTO */
            if  tit_ap.cod_espec_docto < v_cod_espec_docto_ini or
                tit_ap.cod_espec_docto > v_cod_espec_docto_fim
            then do:
                return.
            end /* if */.
            assign v_rec_table      = recid(tit_ap)
                   v_cdn_fornecedor = tit_ap.cdn_fornecedor.
        end.           
    end /* if */.
    else do:
        if  movto_tit_ap.cod_espec_docto < v_cod_espec_docto_ini or
            movto_tit_ap.cod_espec_docto > v_cod_espec_docto_fim
        then do:
            return.
        end /* if */.
        assign v_rec_table      = ?
               v_cdn_fornecedor = movto_tit_ap.cdn_fornec_pef.
    end /* else */.

    /* RETORNAR A FINALIDADE CORRENTE DO ESTABELECIMENTO, QUE SERµ */
    /* UTILIZADA NA PROCURA DO VALOR DA APROPRIAÄ«O CONTµBIL       */
    if  v_cod_estab_aux <> movto_tit_ap.cod_estab
    then do:
        assign v_cod_estab_aux = movto_tit_ap.cod_estab.
        run pi_retornar_finalid_econ_corren_estab (Input v_cod_estab_aux,
                                                   output v_cod_finalid_econ) /*pi_retornar_finalid_econ_corren_estab*/.
    end /* if */.

    assign v_cod_portador = ""
           v_num_cheque   = 0
           v_num_bord_ap  = 0.

    if  v_ind_trans_ap_abrev = "BXA" /*l_bxa*/ 
    or  v_ind_trans_ap_abrev = "PGEF" /*l_pgef*/ 
    or  v_ind_trans_ap_abrev = "PECR" /*l_pecr*/ 
    then do:
        find compl_movto_pagto no-lock
             where compl_movto_pagto.cod_estab = movto_tit_ap.cod_estab
               and compl_movto_pagto.num_id_movto_tit_ap = movto_tit_ap.num_id_movto_tit_ap
              no-error.
        if  avail compl_movto_pagto
        then do:
            assign v_cod_portador = compl_movto_pagto.cod_portador
                   v_num_cheque   = compl_movto_pagto.num_cheque
                   v_num_bord_ap  = compl_movto_pagto.num_bord_ap.
        end /* if */.
    end /* if */.

    for each plano_cta_ctbl no-lock
        where plano_cta_ctbl.cod_plano_cta_ctbl >= v_cod_plano_cta_ctbl_ini
        and   plano_cta_ctbl.cod_plano_cta_ctbl <= v_cod_plano_cta_ctbl_fim:

        aprop_block:
        for each aprop_ctbl_ap no-lock
            where aprop_ctbl_ap.cod_estab           = movto_tit_ap.cod_estab
            and   aprop_ctbl_ap.num_id_movto_tit_ap = movto_tit_ap.num_id_movto_tit_ap
            and   aprop_ctbl_ap.cod_plano_cta_ctbl  = plano_cta_ctbl.cod_plano_cta_ctbl:
            if v_cod_unid_negoc <> "" and aprop_ctbl_ap.cod_unid_negoc <> v_cod_unid_negoc then next.

            /* TRATANDO A SELEÄ«O DE CONTA CONTµBIL */
            if  aprop_ctbl_ap.cod_cta_ctbl < v_cod_cta_ctbl_inic_aux or
                aprop_ctbl_ap.cod_cta_ctbl > v_cod_cta_ctbl_fim_aux
            then do:
                next aprop_block.
            end /* if */.

            if v_log_funcao_sel_cc then do:
                /* TRATANDO A SELEÄ«O DE PLANO E C‡DIGO DE CENTRO DE CUSTO */
                if  aprop_ctbl_ap.cod_plano_ccusto < v_cod_plano_ccusto_ini or
                    aprop_ctbl_ap.cod_plano_ccusto > v_cod_plano_ccusto_fim_1
                then do:
                    next aprop_block.
                end /* if */.
                if  aprop_ctbl_ap.cod_ccusto < v_cod_ccusto_ini or
                    aprop_ctbl_ap.cod_ccusto > v_cod_ccusto_fim
                then do:
                    next aprop_block.
                end /* if */.
            end.
            run pi_cria_tt_rpt_diario_auxiliar_apb /*pi_cria_tt_rpt_diario_auxiliar_apb*/.
        end /* for aprop_block */.
    end.
END PROCEDURE. /* pi_tratar_tt_rpt_aprop_ctbl_ap_diario_aux */
/*****************************************************************************
** Procedure Interna.....: pi_cria_tt_rpt_diario_auxiliar_apb
** Descricao.............: pi_cria_tt_rpt_diario_auxiliar_apb
** Criado por............: bre17892
** Criado em.............: 03/02/1999 16:19:52
** Alterado por..........: fut41422
** Alterado em...........: 15/12/2009 08:02:23
*****************************************************************************/
PROCEDURE pi_cria_tt_rpt_diario_auxiliar_apb:

    /************************* Variable Definition Begin ************************/

    def var v_cod_arq_ems2
        as character
        format "x(16)":U
        no-undo.


    /************************** Variable Definition End *************************/

    find val_aprop_ctbl_ap no-lock
         where val_aprop_ctbl_ap.cod_estab = movto_tit_ap.cod_estab
           and val_aprop_ctbl_ap.num_id_aprop_ctbl_ap = aprop_ctbl_ap.num_id_aprop_ctbl_ap
           and val_aprop_ctbl_ap.cod_finalid_econ = v_cod_finalid_econ
           and val_aprop_ctbl_ap.val_aprop_ctbl > 0
    &if "{&emsfin_version}" /*l_{&emsfin_version}*/  >= "5.01" &then
         use-index vlprpctb_id
    &endif
          /* cl_relat_diario_aux_apb of val_aprop_ctbl_ap*/ no-error.
    if  not avail val_aprop_ctbl_ap
    then do:
        return.
    end /* if */.

    /* Busca as contrapartidas */
    /* para quando Ç N:N / 1:N / 1:1 */

    assign  v_cdn_quant_db = 0
            v_cdn_quant_cr = 0
            v_cod_cta_ctbl_contra = "".

    for each b_aprop_ctbl_ap no-lock
        where b_aprop_ctbl_ap.cod_estab = movto_tit_ap.cod_estab
          and   b_aprop_ctbl_ap.num_id_movto_tit_ap  = movto_tit_ap.num_id_movto_tit_ap:

        if b_aprop_ctbl_ap.ind_natur_lancto_ctbl = "DB" /*l_db*/  then
            assign v_cdn_quant_db = v_cdn_quant_db + 1.

        if b_aprop_ctbl_ap.ind_natur_lancto_ctbl = "CR" /*l_cr*/  then
            assign v_cdn_quant_cr = v_cdn_quant_cr + 1.            
    end.    

    if v_cdn_quant_db = 1 or v_cdn_quant_cr = 1 then do:

        for each b_aprop_ctbl_ap no-lock
             where b_aprop_ctbl_ap.cod_estab = movto_tit_ap.cod_estab
               and b_aprop_ctbl_ap.num_id_movto_tit_ap   = movto_tit_ap.num_id_movto_tit_ap
               and b_aprop_ctbl_ap.ind_natur_lancto_ctbl <> aprop_ctbl_ap.ind_natur_lancto_ctbl:

            if v_cdn_quant_db = 1 and v_cdn_quant_cr > 1 then do:
                if aprop_ctbl_ap.ind_natur_lancto_ctbl = "DB" /*l_db*/  then
                    assign v_cod_cta_ctbl_contra = "".
                else do:
                    if b_aprop_ctbl_ap.ind_natur_lancto_ctbl <> aprop_ctbl_ap.ind_natur_lancto_ctbl then
                        assign v_cod_cta_ctbl_contra = b_aprop_ctbl_ap.cod_cta_ctbl.
                end.
            end.

            if v_cdn_quant_cr = 1 and  v_cdn_quant_db > 1 then do:
                if aprop_ctbl_ap.ind_natur_lancto_ctbl = "CR" /*l_cr*/  then
                    assign v_cod_cta_ctbl_contra = "".
                else do:
                    if b_aprop_ctbl_ap.ind_natur_lancto_ctbl <> aprop_ctbl_ap.ind_natur_lancto_ctbl then
                        assign v_cod_cta_ctbl_contra = b_aprop_ctbl_ap.cod_cta_ctbl.
                end.                    
            end.   

            if v_cdn_quant_cr = 1 and  v_cdn_quant_db = 1 then do:
                if aprop_ctbl_ap.ind_natur_lancto_ctbl = "CR" /*l_cr*/  then do:
                    if b_aprop_ctbl_ap.ind_natur_lancto_ctbl = "DB" /*l_db*/  then
                        assign v_cod_cta_ctbl_contra = b_aprop_ctbl_ap.cod_cta_ctbl.
                end.
                else do:
                    if b_aprop_ctbl_ap.ind_natur_lancto_ctbl = "CR" /*l_cr*/  then
                        assign v_cod_cta_ctbl_contra = b_aprop_ctbl_ap.cod_cta_ctbl.
                end.               
            end.
        end.                
    end.
    else do:
        assign v_cod_cta_ctbl_contra = "".
    end.

    create tt_rpt_diario_auxiliar_apb.
    assign tt_rpt_diario_auxiliar_apb.ttv_rec_tit_ap            = v_rec_table
           tt_rpt_diario_auxiliar_apb.ttv_rec_movto_tit_ap      = recid(movto_tit_ap)
           tt_rpt_diario_auxiliar_apb.ttv_rec_aprop_ctbl_ap     = recid(aprop_ctbl_ap)
           tt_rpt_diario_auxiliar_apb.tta_dat_transacao         = movto_tit_ap.dat_transacao
           tt_rpt_diario_auxiliar_apb.tta_cod_plano_cta_ctbl    = aprop_ctbl_ap.cod_plano_cta_ctbl
           tt_rpt_diario_auxiliar_apb.tta_cod_cta_ctbl          = aprop_ctbl_ap.cod_cta_ctbl
           tt_rpt_diario_auxiliar_apb.ttv_ind_trans_ap_abrev    = v_ind_trans_ap_abrev
           tt_rpt_diario_auxiliar_apb.tta_cod_refer             = movto_tit_ap.cod_refer
           tt_rpt_diario_auxiliar_apb.tta_cdn_fornecedor        = v_cdn_fornecedor
           tt_rpt_diario_auxiliar_apb.tta_cod_estab             = aprop_ctbl_ap.cod_estab_aprop_ctbl
           tt_rpt_diario_auxiliar_apb.tta_cod_portador          = v_cod_portador
           tt_rpt_diario_auxiliar_apb.tta_num_cheque            = v_num_cheque
           tt_rpt_diario_auxiliar_apb.tta_num_bord_ap           = v_num_bord_ap
           tt_rpt_diario_auxiliar_apb.tta_ind_natur_lancto_ctbl = aprop_ctbl_ap.ind_natur_lancto_ctbl
           tt_rpt_diario_auxiliar_apb.tta_cod_unid_negoc        = aprop_ctbl_ap.cod_unid_negoc.

        find first plano_cta_ctbl no-lock
            where plano_cta_ctbl.cod_plano_cta_ctbl = aprop_ctbl_ap.cod_plano_cta_ctbl no-error.
        if avail plano_cta_ctbl then        
           assign tt_rpt_diario_auxiliar_apb.ttv_cod_cta_ctbl_contra   = string(v_cod_cta_ctbl_contra, plano_cta_ctbl.cod_format_cta_ctbl).

    if v_log_funcao_sel_cc then
        assign tt_rpt_diario_auxiliar_apb.tta_cod_ccusto        = aprop_ctbl_ap.cod_ccusto
               tt_rpt_diario_auxiliar_apb.tta_cod_plano_ccusto  = aprop_ctbl_ap.cod_plano_ccusto.

    find fornecedor 
        where fornecedor.cod_empresa    = movto_tit_ap.cod_empresa
        and   fornecedor.cdn_fornecedor = v_cdn_fornecedor 
    no-lock no-error.
    if avail fornecedor then do:
        assign tt_rpt_diario_auxiliar_apb.tta_nom_abrev = fornecedor.nom_abrev.
    end.

    if avail unid_negoc then do:
        assign tt_rpt_diario_auxiliar_apb.tta_des_unid_negoc = unid_negoc.des_unid_negoc.
    end. 
    else do:
        assign tt_rpt_diario_auxiliar_apb.tta_des_unid_negoc = "".
    end.  

    if  aprop_ctbl_ap.ind_natur_lancto_ctbl = "CR" /*l_cr*/ 
    then do:
        assign tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_cr = val_aprop_ctbl_ap.val_aprop_ctbl.
    end /* if */.
    else do:
        assign tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_db = val_aprop_ctbl_ap.val_aprop_ctbl.
    end /* else */.

    if v_log_localiz_chi then do:
        run pi_tratar_tt_rpt_diario_auxiliar_apb_chile.
    end.    

    /* fut36887 - atividade 139621*/
    if  v_log_dados_fisco = yes
    then do:
        run pi_tratar_impressao_historico (Input v_log_impr_histor,
                                           Input v_rec_table,
                                           input-output v_des_histor_diario_aux) /*pi_tratar_impressao_historico*/.

        if  avail tit_ap then
            assign v_cod_arq_ems2 = tit_ap.cod_tit_ap.
        else
            assign v_cod_arq_ems2 = movto_tit_ap.cod_estab + movto_tit_ap.cod_refer.

        create tt_movimentos.
        assign tt_movimentos.tta_cod_empresa           = aprop_ctbl_ap.cod_empresa          
               tt_movimentos.tta_cod_estab             = aprop_ctbl_ap.cod_estab
               tt_movimentos.tta_cod_unid_negoc        = aprop_ctbl_ap.cod_unid_negoc          
               tt_movimentos.tta_cod_plano_cta_ctbl    = aprop_ctbl_ap.cod_plano_cta_ctbl       
               tt_movimentos.tta_cod_cta_ctbl          = aprop_ctbl_ap.cod_cta_ctbl          
               tt_movimentos.ttv_dat_trans_diario      = movto_tit_ap.dat_transacao           
               tt_movimentos.tta_cod_plano_ccusto      = aprop_ctbl_ap.cod_plano_ccusto        
               tt_movimentos.tta_cod_ccusto            = aprop_ctbl_ap.cod_ccusto               
               tt_movimentos.ttv_val_movto             = val_aprop_ctbl_ap.val_aprop_ctbl    
               tt_movimentos.tta_ind_natur_lancto_ctbl = aprop_ctbl_ap.ind_natur_lancto_ctbl
               tt_movimentos.ttv_des_historico         = substring(v_des_histor_diario_aux, 1, 60)
               tt_movimentos.tta_cod_finalid_econ      = val_aprop_ctbl_ap.cod_finalid_econ  
               tt_movimentos.ttv_cod_arq_ems2          = v_cod_arq_ems2
               tt_movimentos.ttv_cdn_clien_fornec      = movto_tit_ap.cdn_fornecedor
               tt_movimentos.ttv_cod_lancto_ctbl       = string(aprop_ctbl_ap.num_id_movto_tit_ap)
               tt_movimentos.ttv_cod_cta_ctbl_contra   = v_cod_cta_ctbl_contra.
        if  v_log_localiz_chi then do:
            run pi_tratar_tt_movimentos_chile_apb.
        end.
    end /* if */.           
    /* fut36887 - atividade 139621*/
END PROCEDURE. /* pi_cria_tt_rpt_diario_auxiliar_apb */
/*****************************************************************************
** Procedure Interna.....: pi_rpt_aprop_ctbl_ap_diario_aux
** Descricao.............: pi_rpt_aprop_ctbl_ap_diario_aux
** Criado por............: Uno
** Criado em.............: 08/05/1996 09:48:53
** Alterado por..........: andrefossile
** Alterado em...........: 17/04/2012 17:58:55
*****************************************************************************/
PROCEDURE pi_rpt_aprop_ctbl_ap_diario_aux:
    def var i-transacao as int no-undo.

    assign v_dat_inic_period     = v_dat_inic_diario_aux_apb
           v_dat_fim_period      = v_dat_fim_diario_aux_apb
           v_log_primei_plano    = yes
           v_log_primei_cta_ctbl = yes.
    run pi_ler_tt_rpt_aprop_ctbl_ap_diario_aux /*pi_ler_tt_rpt_aprop_ctbl_ap_diario_aux*/.

    /* VERIFICA A EXIST“NCIA DE MOVIMENTOS PARA COMPOR O DIµRIO AUXILIAR */
    if  not can-find(first tt_rpt_diario_auxiliar_apb)
            then do:
        if v_cod_dwb_user begins 'es_' then
            return "Di†rio Auxiliar sem movimentos." /*2442*/.
        else do:
            /* Di†rio Auxiliar sem movimentos. */
            run pi_messages (input "show",
                             input 2442,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_2442*/.
            return.
        end.
    end /* if */.

    assign v_val_tot_cr_cta_ctbl = 0
           v_val_tot_db_cta_ctbl = 0
           v_val_tot_cr_dia      = 0
           v_val_tot_db_dia      = 0
           v_val_tot_cr_plano    = 0
           v_val_tot_db_plano    = 0
           v_val_tot_cr_period   = 0
           v_val_tot_db_period   = 0.              

    /* ATUALIZAÄ«O DE PµGINA, CONFORME A ÈLTIMA EMISS«O */
    if v_log_atualiz_numer_pag = yes then
        run pi_atualiz_num_pag_livro_fiscal.

    hide stream s_1 frame f_rpt_s_1_header_unique_auxiliar.
    view stream s_1 frame f_rpt_s_1_header_period_auxiliar.
    hide stream s_1 frame f_rpt_s_1_footer_last_page_auxiliar.
    hide stream s_1 frame f_rpt_s_1_footer_param_page.
    view stream s_1 frame f_rpt_s_1_footer_normal.


    if v_log_localiz_chi then do:
        for each tt_log_erros_chi:
            delete tt_log_erros_chi.
        end.    
        if v_log_dados_fisco then do:
           run pi_trata_erros_diario_aux_apb.
        end.
        else do:
           run pi_trata_erros_diario_aux_chile.
        end. 
    end.
    else do:
        run pi_trata_erros_diario_aux.
    end.
/*
    /* VERIFICA NECESSIDADE DE IMPRIMIR OS TERMOS NA PRIMEIRA PµGINA DO RELAT‡RIO(chegou no fim do livro anterior)*/
    if  v_log_atualiz_numer_pag = yes and v_num_pag = livro_fisc.num_pag_livro then
        run pi_atualiz_num_pag_livro_fiscal_2 /*pi_atualiz_num_pag_livro_fiscal_2*/.

    if not v_log_funcao_sel_cc then do:
        if v_log_impr_cta_contra_partida then do:
            if (line-counter(s_1) + 5) > v_rpt_s_1_bottom then
                page stream s_1.
            put stream s_1 unformatted 
                "Conta Cont†bil" at 1
                "Trans" at 22
                "Refer" at 28
                "Fornecedor" to 49
                "Nome Abrev" at 51
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                "Estab" at 67
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                "Estab" at 67
    &ENDIF
                "Esp" at 73
                "SÇrie" at 77
                "T°tulo" to 98
                "/P" at 102
                "Port" at 105
                "Num Cheque" to 122
                "Valor DÇbito" to 138
                "Valor CrÇdito" to 154 skip
                "Contra Partida" at 3
                "Hist¢rico" at 24 skip
                "----------------------------------------------------------------------------------------------------------------------------------------------------------" at 1 skip.
            hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_cont_part_cc.
            hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_diario.
            hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_diario_cc.
            hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_histor.
            view stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_cont_partida.    
        end.
        else do:
            if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                page stream s_1.
            put stream s_1 unformatted 
                "Conta Cont†bil" at 1
                "Trans" at 22
                "Refer" at 28
                "Fornecedor" to 49
                "Nome Abrev" at 51
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                "Estab" at 67
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                "Estab" at 67
    &ENDIF
                "Esp" at 73
                "SÇrie" at 77
                "T°tulo" to 98
                "/P" at 102
                "Port" at 105
                "Num Cheque" to 122
                "Valor DÇbito" to 138
                "Valor CrÇdito" to 154 skip
                "--------------------" at 1
                "-----" at 22
                "----------" at 28
                "-----------" to 49
                "---------------" at 51
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                "-----" at 67
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                "-----" at 67
    &ENDIF
                "---" at 73
                "-----" at 77
                "----------------" to 98
                "--" at 102
                "-----" at 105
                "------------" to 122
                "---------------" to 138
                "---------------" to 154 skip.
            hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_cont_partida.
            hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_cont_part_cc.
            hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_diario_cc.
            hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_histor.
            view stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_diario.
        end.    
    end.
    else do:
        if v_log_impr_cta_contra_partida then do:
            if (line-counter(s_1) + 5) > v_rpt_s_1_bottom then
                page stream s_1.
            put stream s_1 unformatted 
                "Conta Cont†bil" at 1
                "Trans" at 22
                "Refer" at 28
                "Fornecedor" to 49
                "Nome Abrev" at 51
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                "Estab" at 67
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                "Estab" at 67
    &ENDIF
                "Plano CCusto" at 73
                "Centro de Custo" at 86
                "Esp" at 107
                "SÇrie" at 111
                "T°tulo" at 117
                "/P" at 136
                "Port" at 139
                "Num Cheque" to 156
                "Valor DB" to 172
                "Valor CR" to 188 skip
                "Contra Partida" at 3
                "Hist¢rico" at 24 skip
                "--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" at 1 skip.
            hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_cont_partida.
            hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_diario.
            hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_diario_cc.
            hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_histor.
            view stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_cont_part_cc.     
        end.
        else do:
            if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                page stream s_1.
            put stream s_1 unformatted 
                "Conta Cont†bil" at 1
                "Trans" at 22
                "Refer" at 28
                "Fornecedor" to 49
                "Nome Abrev" at 51
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                "Estab" at 67
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                "Estab" at 67
    &ENDIF
                "Plano CCusto" at 73
                "Centro de Custo" at 86
                "Esp" at 107
                "SÇrie" at 111
                "T°tulo" at 117
                "/P" at 136
                "Port" at 139
                "Num Cheque" to 156
                "Valor DB" to 172
                "Valor CR" to 188 skip
                "--------------------" at 1
                "-----" at 22
                "----------" at 28
                "-----------" to 49
                "---------------" at 51
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                "-----" at 67
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                "-----" at 67
    &ENDIF
                "------------" at 73
                "--------------------" at 86
                "---" at 107
                "-----" at 111
                "----------------" at 117
                "--" at 136
                "-----" at 139
                "------------" to 156
                "---------------" to 172
                "---------------" to 188 skip.
            hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_cont_partida.
            hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_cont_part_cc.
            hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_diario.
            hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_histor.
            view stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_diario_cc.    
        end.    
    end.
*/
    /************************************ Limpa Ilha ******************************/
    find first es_diario2 no-lock
         where es_diario2.ep_codigo = v_cod_empresa no-error.
    if  avail es_diario2 then
        for each es_diario2 exclusive-lock
           where es_diario2.ep_codigo = v_cod_empresa:
            delete es_diario2.
        end.

    release es_diario2 no-error.
    /************************************ Limpa Ilha ******************************/

    grp_block:
    for
       each tt_rpt_diario_auxiliar_apb no-lock
       break by tt_rpt_diario_auxiliar_apb.tta_cod_plano_cta_ctbl
             by tt_rpt_diario_auxiliar_apb.tta_dat_transacao
             by tt_rpt_diario_auxiliar_apb.tta_cod_cta_ctbl
             by tt_rpt_diario_auxiliar_apb.tta_cod_portador
             by tt_rpt_diario_auxiliar_apb.tta_num_cheque:

       assign v_rec_table = tt_rpt_diario_auxiliar_apb.ttv_rec_tit_ap.
       find tit_ap where recid(tit_ap) = v_rec_table no-lock no-error.
/*
       /* CONTROLE DAS QUEBRAS DO RELAT‡RIO */
       if  first-of(tt_rpt_diario_auxiliar_apb.tta_cod_plano_cta_ctbl)
       then do:
           find plano_cta_ctbl no-lock
                where plano_cta_ctbl.cod_plano_cta_ctbl = tt_rpt_diario_auxiliar_apb.tta_cod_plano_cta_ctbl
    &if "{&emsuni_version}" >= "5.01" &then
                use-index plnctctl_id
    &endif
                 /*cl_tt_rpt_diario_auxiliar_apb of plano_cta_ctbl*/ no-error.

           if(v_log_primei_plano = yes) then
               assign v_log_primei_plano = no.
           else
               page stream s_1.

            if  not v_log_funcao_sel_cc
            then do:
                if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "Plano Contas: " at 1
                    tt_rpt_diario_auxiliar_apb.tta_cod_plano_cta_ctbl at 15 format "x(8)"
                    "--" at 39
                    "--------------------------------" at 41
                    "----------------------------------------------------------------------------------" at 73 skip (1).
                if v_cod_unid_negoc <> "" then do:
                    if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "Unid Neg¢cio: " at 1
                        tt_rpt_diario_auxiliar_apb.tta_cod_unid_negoc at 15 format "x(3)"
                        "-" at 19
                        tt_rpt_diario_auxiliar_apb.tta_des_unid_negoc at 21 format "x(40)"
                        "----------------------------------------------------------------------------" at 69 skip (1).        
                end.    
            end /* if */.           
            else do:
                if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "Plano Contas: " at 1
                    tt_rpt_diario_auxiliar_apb.tta_cod_plano_cta_ctbl at 15 format "x(8)"
                    "--" at 39
                    "--------------------------------" at 41
                    "--------------------------------------------------------------------------------------------------------------------" at 73 skip (1).
                if v_cod_unid_negoc <> "" then do:
                    if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                        page stream s_1.
                    put stream s_1 unformatted 
                        "Unid Neg¢cio: " at 1
                        tt_rpt_diario_auxiliar_apb.tta_cod_unid_negoc at 15 format "x(3)"
                        "-" at 19
                        tt_rpt_diario_auxiliar_apb.tta_des_unid_negoc at 21 format "x(40)"
                        "----------------------------------------------------------------------------" at 69 skip (1).
                end.
            end /* else */.           

       end /* if */.

       if  first-of(tt_rpt_diario_auxiliar_apb.tta_dat_transacao)
       then do:
            if  not v_log_funcao_sel_cc
            then do:
                if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "Movimento do Dia:    " at 1
                    tt_rpt_diario_auxiliar_apb.tta_dat_transacao at 23 format "99/99/9999"
                    "--" at 39
                    "--------------------------------" at 41
                    "----------------------------------------------------------------------------------" at 73 skip (1).
            end /* if */.           
            else do:
                if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "Movimento do Dia:    " at 1
                    tt_rpt_diario_auxiliar_apb.tta_dat_transacao at 23 format "99/99/9999"
                    "--" at 39
                    "--------------------------------" at 41
                    "--------------------------------------------------------------------------------------------------------------------" at 73 skip (1).
            end /* else */.           
       end /* if */.

       if  first-of(tt_rpt_diario_auxiliar_apb.tta_cod_cta_ctbl)
       then do:       
           assign v_log_primei_cta_ctbl = yes.
       end.

    /* ----------- TRATA A IMPRESS«O DO HIST‡RICO ------------ */
        run pi_tratar_impressao_historico (Input v_log_impr_histor,
                                           Input v_rec_table,
                                           input-output v_des_histor_diario_aux) /*pi_tratar_impressao_historico*/.

       /* MµSCARA PARA O C‡DIGO DA CONTA CONTµBIL */
       if  avail plano_cta_ctbl
       then do:
           assign v_cod_cta_ctbl = string(tt_rpt_diario_auxiliar_apb.tta_cod_cta_ctbl, plano_cta_ctbl.cod_format_cta_ctbl).
       end /* if */.
       else do:
           assign v_cod_cta_ctbl = "".
       end /* else */.

       /* TESTA A QUEBRA DA CONTA CTBL PARA A IMPRESS«O DO TOTAL */
       if  last-of(tt_rpt_diario_auxiliar_apb.tta_cod_cta_ctbl)
       and ((line-counter(s_1) + 5) > v_rpt_s_1_bottom)
       then do:
           page stream s_1.
       end /* if */.

       /* IMPRESS«O DETALHE DO RELAT‡RIO */
       if  tt_rpt_diario_auxiliar_apb.tta_ind_natur_lancto_ctbl = "CR" /*l_cr*/ 
       then do:
           if  v_log_primei_cta_ctbl = yes
           then do:
               if not v_log_funcao_sel_cc then do:
                   if v_log_impr_cta_contra_partida then do:
                       if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                           page stream s_1.
                       put stream s_1 unformatted 
                           v_cod_cta_ctbl at 1 format "x(20)"
                           tt_rpt_diario_auxiliar_apb.ttv_ind_trans_ap_abrev at 22 format "X(04)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_refer at 28 format "x(10)"
                           tt_rpt_diario_auxiliar_apb.tta_cdn_fornecedor to 49 format ">>>,>>>,>>9"
                           tt_rpt_diario_auxiliar_apb.tta_nom_abrev at 51 format "x(15)"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(5)"
    &ENDIF
                           if avail tit_ap then tit_ap.cod_espec_docto else "" at 73 format "x(3)"
                           if avail tit_ap then tit_ap.cod_ser_docto else "" at 77 format "x(5)"
                           if avail tit_ap then fill(" ", 16 - length(trim(fill(" ",16 - length (trim(tit_ap.cod_tit_ap))) + trim(tit_ap.cod_tit_ap)))) + trim(fill(" ",16 - length (trim(tit_ap.cod_tit_ap))) + trim(tit_ap.cod_tit_ap)) else "" to 98 format "x(16)"
                           "/" at 100
                           if avail tit_ap then tit_ap.cod_parcela else "" at 102 format "x(02)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_portador at 105 format "x(5)".
    put stream s_1 unformatted 
                           tt_rpt_diario_auxiliar_apb.tta_num_cheque to 122 format ">>>>,>>>,>>>"
                           tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_cr to 154 format "->>>,>>>,>>9.99" skip
                           tt_rpt_diario_auxiliar_apb.ttv_cod_cta_ctbl_contra at 3 format "x(20)"
                           v_des_histor_diario_aux at 24 format "x(74)" skip.                          
                   end.
                   else do:
                       if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                           page stream s_1.
                       put stream s_1 unformatted 
                           v_cod_cta_ctbl at 1 format "x(20)"
                           tt_rpt_diario_auxiliar_apb.ttv_ind_trans_ap_abrev at 22 format "X(04)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_refer at 28 format "x(10)"
                           tt_rpt_diario_auxiliar_apb.tta_cdn_fornecedor to 49 format ">>>,>>>,>>9"
                           tt_rpt_diario_auxiliar_apb.tta_nom_abrev at 51 format "x(15)"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(5)"
    &ENDIF
                           if avail tit_ap then tit_ap.cod_espec_docto else "" at 73 format "x(3)"
                           if avail tit_ap then tit_ap.cod_ser_docto else "" at 77 format "x(5)"
                           if avail tit_ap then fill(" ", 16 - length(trim(fill(" ",16 - length (trim(tit_ap.cod_tit_ap))) + trim(tit_ap.cod_tit_ap)))) + trim(fill(" ",16 - length (trim(tit_ap.cod_tit_ap))) + trim(tit_ap.cod_tit_ap)) else "" to 98 format "x(16)"
                           "/" at 100
                           if avail tit_ap then tit_ap.cod_parcela else "" at 102 format "x(02)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_portador at 105 format "x(5)".
    put stream s_1 unformatted 
                           tt_rpt_diario_auxiliar_apb.tta_num_cheque to 122 format ">>>>,>>>,>>>"
                           tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_cr to 154 format "->>>,>>>,>>9.99" skip.
                   end.               
               end.           
               else do:   
                   if v_log_impr_cta_contra_partida then do:
                       if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                           page stream s_1.
                       put stream s_1 unformatted 
                           v_cod_cta_ctbl at 1 format "x(20)"
                           tt_rpt_diario_auxiliar_apb.ttv_ind_trans_ap_abrev at 22 format "X(04)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_refer at 28 format "x(10)"
                           tt_rpt_diario_auxiliar_apb.tta_cdn_fornecedor to 49 format ">>>,>>>,>>9"
                           tt_rpt_diario_auxiliar_apb.tta_nom_abrev at 51 format "x(15)"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(5)"
    &ENDIF
                           tt_rpt_diario_auxiliar_apb.tta_cod_plano_ccusto at 73 format "x(8)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_ccusto at 86 format "x(20)"
                           if avail tit_ap then tit_ap.cod_espec_docto else "" at 107 format "x(3)"
                           if avail tit_ap then tit_ap.cod_ser_docto else "" at 111 format "x(5)"
                           if avail tit_ap then fill(" ",16 - length (trim(tit_ap.cod_tit_ap))) + trim(tit_ap.cod_tit_ap) else "" at 117 format "x(16)"
                           if avail tit_ap then tit_ap.cod_parcela else "" at 136 format "x(02)".
    put stream s_1 unformatted 
                           tt_rpt_diario_auxiliar_apb.tta_cod_portador at 139 format "x(5)"
                           tt_rpt_diario_auxiliar_apb.tta_num_cheque to 156 format ">>>>,>>>,>>9"
                           tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_cr to 188 format "->>>,>>>,>>9.99" skip
                           tt_rpt_diario_auxiliar_apb.ttv_cod_cta_ctbl_contra at 3 format "x(20)"
                           v_des_histor_diario_aux at 24 format "x(74)" skip.                
                   end.
                   else do:                     
                       if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                           page stream s_1.
                       put stream s_1 unformatted 
                           v_cod_cta_ctbl at 1 format "x(20)"
                           tt_rpt_diario_auxiliar_apb.ttv_ind_trans_ap_abrev at 22 format "X(04)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_refer at 28 format "x(10)"
                           tt_rpt_diario_auxiliar_apb.tta_cdn_fornecedor to 49 format ">>>,>>>,>>9"
                           tt_rpt_diario_auxiliar_apb.tta_nom_abrev at 51 format "x(15)"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(5)"
    &ENDIF
                           tt_rpt_diario_auxiliar_apb.tta_cod_plano_ccusto at 73 format "x(8)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_ccusto at 86 format "x(20)"
                           if avail tit_ap then tit_ap.cod_espec_docto else "" at 107 format "x(3)"
                           if avail tit_ap then tit_ap.cod_ser_docto else "" at 111 format "x(5)"
                           if avail tit_ap then fill(" ",16 - length (trim(tit_ap.cod_tit_ap))) + trim(tit_ap.cod_tit_ap) else "" at 117 format "x(16)"
                           if avail tit_ap then tit_ap.cod_parcela else "" at 136 format "x(02)".
    put stream s_1 unformatted 
                           tt_rpt_diario_auxiliar_apb.tta_cod_portador at 139 format "x(5)"
                           tt_rpt_diario_auxiliar_apb.tta_num_cheque to 156 format ">>>>,>>>,>>9"
                           tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_cr to 188 format "->>>,>>>,>>9.99" skip. 
                   end.                   
               end.   
               assign v_log_primei_cta_ctbl = no.
           end /* if */.
           else do:
               if not v_log_funcao_sel_cc then do:
                   if v_log_impr_cta_contra_partida then do:
                       if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                           page stream s_1.
                       put stream s_1 unformatted 
                           v_cod_cta_ctbl at 1 format "x(20)"
                           tt_rpt_diario_auxiliar_apb.ttv_ind_trans_ap_abrev at 22 format "X(04)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_refer at 28 format "x(10)"
                           tt_rpt_diario_auxiliar_apb.tta_cdn_fornecedor to 49 format ">>>,>>>,>>9"
                           tt_rpt_diario_auxiliar_apb.tta_nom_abrev at 51 format "x(15)"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(5)"
    &ENDIF
                           if avail tit_ap then tit_ap.cod_espec_docto else "" at 73 format "x(3)"
                           if avail tit_ap then tit_ap.cod_ser_docto else "" at 77 format "x(5)"
                           if avail tit_ap then fill(" ", 16 - length(trim(fill(" ",16 - length (trim(tit_ap.cod_tit_ap))) + trim(tit_ap.cod_tit_ap)))) + trim(fill(" ",16 - length (trim(tit_ap.cod_tit_ap))) + trim(tit_ap.cod_tit_ap)) else "" to 98 format "x(16)"
                           "/" at 100
                           if avail tit_ap then tit_ap.cod_parcela else "" at 102 format "x(02)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_portador at 105 format "x(5)".
    put stream s_1 unformatted 
                           tt_rpt_diario_auxiliar_apb.tta_num_cheque to 122 format ">>>>,>>>,>>>"
                           tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_cr to 154 format "->>>,>>>,>>9.99" skip
                           tt_rpt_diario_auxiliar_apb.ttv_cod_cta_ctbl_contra at 3 format "x(20)"
                           v_des_histor_diario_aux at 24 format "x(74)" skip.               
                   end.
                   else do:           
                       if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                           page stream s_1.
                       put stream s_1 unformatted 
                           tt_rpt_diario_auxiliar_apb.ttv_ind_trans_ap_abrev at 22 format "X(04)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_refer at 28 format "x(10)"
                           tt_rpt_diario_auxiliar_apb.tta_cdn_fornecedor to 49 format ">>>,>>>,>>9"
                           tt_rpt_diario_auxiliar_apb.tta_nom_abrev at 51 format "x(15)"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(3)"
    &ENDIF.
    put stream s_1 unformatted 
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(5)"
    &ENDIF
                           if avail tit_ap then tit_ap.cod_espec_docto else "" at 73 format "x(3)"
                           if avail tit_ap then tit_ap.cod_ser_docto else "" at 77 format "x(5)"
                           if avail tit_ap then fill(" ", 16 - length(trim(fill(" ",16 - length (trim(tit_ap.cod_tit_ap))) + trim(tit_ap.cod_tit_ap)))) + trim(fill(" ",16 - length (trim(tit_ap.cod_tit_ap))) + trim(tit_ap.cod_tit_ap)) else "" to 98 format "x(16)"
                           "/" at 100
                           if avail tit_ap then tit_ap.cod_parcela else "" at 102 format "x(02)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_portador at 105 format "x(5)"
                           tt_rpt_diario_auxiliar_apb.tta_num_cheque to 122 format ">>>>,>>>,>>>".
    put stream s_1 unformatted 
                           tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_cr to 154 format "->>>,>>>,>>9.99" skip.
                   end.                   
               end.
               else do:
                   if v_log_impr_cta_contra_partida then do:
                       if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                           page stream s_1.
                       put stream s_1 unformatted 
                           v_cod_cta_ctbl at 1 format "x(20)"
                           tt_rpt_diario_auxiliar_apb.ttv_ind_trans_ap_abrev at 22 format "X(04)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_refer at 28 format "x(10)"
                           tt_rpt_diario_auxiliar_apb.tta_cdn_fornecedor to 49 format ">>>,>>>,>>9"
                           tt_rpt_diario_auxiliar_apb.tta_nom_abrev at 51 format "x(15)"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(5)"
    &ENDIF
                           tt_rpt_diario_auxiliar_apb.tta_cod_plano_ccusto at 73 format "x(8)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_ccusto at 86 format "x(20)"
                           if avail tit_ap then tit_ap.cod_espec_docto else "" at 107 format "x(3)"
                           if avail tit_ap then tit_ap.cod_ser_docto else "" at 111 format "x(5)"
                           if avail tit_ap then fill(" ",16 - length (trim(tit_ap.cod_tit_ap))) + trim(tit_ap.cod_tit_ap) else "" at 117 format "x(16)"
                           if avail tit_ap then tit_ap.cod_parcela else "" at 136 format "x(02)".
    put stream s_1 unformatted 
                           tt_rpt_diario_auxiliar_apb.tta_cod_portador at 139 format "x(5)"
                           tt_rpt_diario_auxiliar_apb.tta_num_cheque to 156 format ">>>>,>>>,>>9"
                           tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_cr to 188 format "->>>,>>>,>>9.99" skip
                           tt_rpt_diario_auxiliar_apb.ttv_cod_cta_ctbl_contra at 3 format "x(20)"
                           v_des_histor_diario_aux at 24 format "x(74)" skip.               
                   end.
                   else do:           
                       if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                           page stream s_1.
                       put stream s_1 unformatted 
                           tt_rpt_diario_auxiliar_apb.ttv_ind_trans_ap_abrev at 22 format "X(04)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_refer at 28 format "x(10)"
                           tt_rpt_diario_auxiliar_apb.tta_cdn_fornecedor to 49 format ">>>,>>>,>>9"
                           tt_rpt_diario_auxiliar_apb.tta_nom_abrev at 51 format "x(15)"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(3)"
    &ENDIF.
    put stream s_1 unformatted 
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(5)"
    &ENDIF
                           tt_rpt_diario_auxiliar_apb.tta_cod_plano_ccusto at 73 format "x(8)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_ccusto at 86 format "x(20)"
                           if avail tit_ap then tit_ap.cod_espec_docto else "" at 107 format "x(3)"
                           if avail tit_ap then tit_ap.cod_ser_docto else "" at 111 format "x(5)"
                           if avail tit_ap then fill(" ",16 - length (trim(tit_ap.cod_tit_ap))) + trim(tit_ap.cod_tit_ap) else "" at 117 format "x(16)"
                           if avail tit_ap then tit_ap.cod_parcela else "" at 136 format "x(02)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_portador at 139 format "x(5)".
    put stream s_1 unformatted 
                           tt_rpt_diario_auxiliar_apb.tta_num_cheque to 156 format ">>>>,>>>,>>9"
                           tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_cr to 188 format "->>>,>>>,>>9.99" skip.
                   end.                   
               end.
           end /* else */.
           assign v_val_tot_cr_cta_ctbl = v_val_tot_cr_cta_ctbl + tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_cr.
       end /* if */.
       else do:
           if  v_log_primei_cta_ctbl = yes
           then do:
               if not v_log_funcao_sel_cc then do:
                   if v_log_impr_cta_contra_partida then do:
                       if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                           page stream s_1.
                       put stream s_1 unformatted 
                           v_cod_cta_ctbl at 1 format "x(20)"
                           tt_rpt_diario_auxiliar_apb.ttv_ind_trans_ap_abrev at 22 format "X(04)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_refer at 28 format "x(10)"
                           tt_rpt_diario_auxiliar_apb.tta_cdn_fornecedor to 49 format ">>>,>>>,>>9"
                           tt_rpt_diario_auxiliar_apb.tta_nom_abrev at 51 format "x(15)"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(5)"
    &ENDIF
                           if avail tit_ap then tit_ap.cod_espec_docto else "" at 73 format "x(3)"
                           if avail tit_ap then tit_ap.cod_ser_docto else "" at 77 format "x(5)"
                           if avail tit_ap then fill(" ", 16 - length(trim(fill(" ",16 - length (trim(tit_ap.cod_tit_ap))) + trim(tit_ap.cod_tit_ap)))) + trim(fill(" ",16 - length (trim(tit_ap.cod_tit_ap))) + trim(tit_ap.cod_tit_ap)) else "" to 98 format "x(16)"
                           "/" at 100
                           if avail tit_ap then tit_ap.cod_parcela else "" at 102 format "x(02)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_portador at 105 format "x(5)".
    put stream s_1 unformatted 
                           tt_rpt_diario_auxiliar_apb.tta_num_cheque to 122 format ">>>>,>>>,>>>"
                           tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_db to 138 format "->>>,>>>,>>9.99" skip
                           tt_rpt_diario_auxiliar_apb.ttv_cod_cta_ctbl_contra at 3 format "x(20)"
                           v_des_histor_diario_aux at 24 format "x(74)" skip.               
                   end.
                   else do:
                       if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                           page stream s_1.
                       put stream s_1 unformatted 
                           v_cod_cta_ctbl at 1 format "x(20)"
                           tt_rpt_diario_auxiliar_apb.ttv_ind_trans_ap_abrev at 22 format "X(04)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_refer at 28 format "x(10)"
                           tt_rpt_diario_auxiliar_apb.tta_cdn_fornecedor to 49 format ">>>,>>>,>>9"
                           tt_rpt_diario_auxiliar_apb.tta_nom_abrev at 51 format "x(15)"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(5)"
    &ENDIF
                           if avail tit_ap then tit_ap.cod_espec_docto else "" at 73 format "x(3)"
                           if avail tit_ap then tit_ap.cod_ser_docto else "" at 77 format "x(5)"
                           if avail tit_ap then fill(" ", 16 - length(trim(fill(" ",16 - length (trim(tit_ap.cod_tit_ap))) + trim(tit_ap.cod_tit_ap)))) + trim(fill(" ",16 - length (trim(tit_ap.cod_tit_ap))) + trim(tit_ap.cod_tit_ap)) else "" to 98 format "x(16)"
                           "/" at 100
                           if avail tit_ap then tit_ap.cod_parcela else "" at 102 format "x(02)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_portador at 105 format "x(5)".
    put stream s_1 unformatted 
                           tt_rpt_diario_auxiliar_apb.tta_num_cheque to 122 format ">>>>,>>>,>>>"
                           tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_db to 138 format "->>>,>>>,>>9.99" skip.
                   end.               
               end.
               else do:
                   if v_log_impr_cta_contra_partida then do:
                       if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                           page stream s_1.
                       put stream s_1 unformatted 
                           v_cod_cta_ctbl at 1 format "x(20)"
                           tt_rpt_diario_auxiliar_apb.ttv_ind_trans_ap_abrev at 22 format "X(04)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_refer at 28 format "x(10)"
                           tt_rpt_diario_auxiliar_apb.tta_cdn_fornecedor to 49 format ">>>,>>>,>>9"
                           tt_rpt_diario_auxiliar_apb.tta_nom_abrev at 51 format "x(15)"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(5)"
    &ENDIF
                           tt_rpt_diario_auxiliar_apb.tta_cod_plano_ccusto at 73 format "x(8)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_ccusto at 86 format "x(20)"
                           if avail tit_ap then tit_ap.cod_espec_docto else "" at 107 format "x(3)"
                           if avail tit_ap then tit_ap.cod_ser_docto else "" at 111 format "x(5)"
                           if avail tit_ap then fill(" ",16 - length (trim(tit_ap.cod_tit_ap))) + trim(tit_ap.cod_tit_ap) else "" at 117 format "x(16)"
                           if avail tit_ap then tit_ap.cod_parcela else "" at 136 format "x(02)".
    put stream s_1 unformatted 
                           tt_rpt_diario_auxiliar_apb.tta_cod_portador at 139 format "x(5)"
                           tt_rpt_diario_auxiliar_apb.tta_num_cheque to 156 format ">>>>,>>>,>>9"
                           tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_db to 172 format "->>>,>>>,>>9.99" skip
                           tt_rpt_diario_auxiliar_apb.ttv_cod_cta_ctbl_contra at 3 format "x(20)"
                           v_des_histor_diario_aux at 24 format "x(74)" skip.
                   end.           
                   else do:
                       if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                           page stream s_1.
                       put stream s_1 unformatted 
                           v_cod_cta_ctbl at 1 format "x(20)"
                           tt_rpt_diario_auxiliar_apb.ttv_ind_trans_ap_abrev at 22 format "X(04)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_refer at 28 format "x(10)"
                           tt_rpt_diario_auxiliar_apb.tta_cdn_fornecedor to 49 format ">>>,>>>,>>9"
                           tt_rpt_diario_auxiliar_apb.tta_nom_abrev at 51 format "x(15)"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(5)"
    &ENDIF
                           tt_rpt_diario_auxiliar_apb.tta_cod_plano_ccusto at 73 format "x(8)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_ccusto at 86 format "x(20)"
                           if avail tit_ap then tit_ap.cod_espec_docto else "" at 107 format "x(3)"
                           if avail tit_ap then tit_ap.cod_ser_docto else "" at 111 format "x(5)"
                           if avail tit_ap then fill(" ",16 - length (trim(tit_ap.cod_tit_ap))) + trim(tit_ap.cod_tit_ap) else "" at 117 format "x(16)"
                           if avail tit_ap then tit_ap.cod_parcela else "" at 136 format "x(02)".
    put stream s_1 unformatted 
                           tt_rpt_diario_auxiliar_apb.tta_cod_portador at 139 format "x(5)"
                           tt_rpt_diario_auxiliar_apb.tta_num_cheque to 156 format ">>>>,>>>,>>9"
                           tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_db to 172 format "->>>,>>>,>>9.99" skip. 
                   end.                                 
               end.
               assign v_log_primei_cta_ctbl = no.
           end /* if */.
           else do:
               if not v_log_funcao_sel_cc then do:
                   if v_log_impr_cta_contra_partida then do:
                   if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                       page stream s_1.
                   put stream s_1 unformatted 
                       v_cod_cta_ctbl at 1 format "x(20)"
                       tt_rpt_diario_auxiliar_apb.ttv_ind_trans_ap_abrev at 22 format "X(04)"
                       tt_rpt_diario_auxiliar_apb.tta_cod_refer at 28 format "x(10)"
                       tt_rpt_diario_auxiliar_apb.tta_cdn_fornecedor to 49 format ">>>,>>>,>>9"
                       tt_rpt_diario_auxiliar_apb.tta_nom_abrev at 51 format "x(15)"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                       tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                       tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(5)"
    &ENDIF
                       if avail tit_ap then tit_ap.cod_espec_docto else "" at 73 format "x(3)"
                       if avail tit_ap then tit_ap.cod_ser_docto else "" at 77 format "x(5)"
                       if avail tit_ap then fill(" ", 16 - length(trim(fill(" ",16 - length (trim(tit_ap.cod_tit_ap))) + trim(tit_ap.cod_tit_ap)))) + trim(fill(" ",16 - length (trim(tit_ap.cod_tit_ap))) + trim(tit_ap.cod_tit_ap)) else "" to 98 format "x(16)"
                       "/" at 100
                       if avail tit_ap then tit_ap.cod_parcela else "" at 102 format "x(02)"
                       tt_rpt_diario_auxiliar_apb.tta_cod_portador at 105 format "x(5)".
    put stream s_1 unformatted 
                       tt_rpt_diario_auxiliar_apb.tta_num_cheque to 122 format ">>>>,>>>,>>>"
                       tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_db to 138 format "->>>,>>>,>>9.99" skip
                       tt_rpt_diario_auxiliar_apb.ttv_cod_cta_ctbl_contra at 3 format "x(20)"
                       v_des_histor_diario_aux at 24 format "x(74)" skip.               
                   end.           
                   else do:               
                       if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                           page stream s_1.
                       put stream s_1 unformatted 
                           tt_rpt_diario_auxiliar_apb.ttv_ind_trans_ap_abrev at 22 format "X(04)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_refer at 28 format "x(10)"
                           tt_rpt_diario_auxiliar_apb.tta_cdn_fornecedor to 49 format ">>>,>>>,>>9"
                           tt_rpt_diario_auxiliar_apb.tta_nom_abrev at 51 format "x(15)"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(3)"
    &ENDIF.
    put stream s_1 unformatted 
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(5)"
    &ENDIF
                           if avail tit_ap then tit_ap.cod_espec_docto else "" at 73 format "x(3)"
                           if avail tit_ap then tit_ap.cod_ser_docto else "" at 77 format "x(5)"
                           if avail tit_ap then fill(" ", 16 - length(trim(fill(" ",16 - length (trim(tit_ap.cod_tit_ap))) + trim(tit_ap.cod_tit_ap)))) + trim(fill(" ",16 - length (trim(tit_ap.cod_tit_ap))) + trim(tit_ap.cod_tit_ap)) else "" to 98 format "x(16)"
                           "/" at 100
                           if avail tit_ap then tit_ap.cod_parcela else "" at 102 format "x(02)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_portador at 105 format "x(5)"
                           tt_rpt_diario_auxiliar_apb.tta_num_cheque to 122 format ">>>>,>>>,>>>".
    put stream s_1 unformatted 
                           tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_db to 138 format "->>>,>>>,>>9.99" skip.
                   end.                   
               end. 
               else do: 
                   if v_log_impr_cta_contra_partida then do:
                       if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                           page stream s_1.
                       put stream s_1 unformatted 
                           v_cod_cta_ctbl at 1 format "x(20)"
                           tt_rpt_diario_auxiliar_apb.ttv_ind_trans_ap_abrev at 22 format "X(04)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_refer at 28 format "x(10)"
                           tt_rpt_diario_auxiliar_apb.tta_cdn_fornecedor to 49 format ">>>,>>>,>>9"
                           tt_rpt_diario_auxiliar_apb.tta_nom_abrev at 51 format "x(15)"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(3)".
    put stream s_1 unformatted 
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(5)"
    &ENDIF
                           tt_rpt_diario_auxiliar_apb.tta_cod_plano_ccusto at 73 format "x(8)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_ccusto at 86 format "x(20)"
                           if avail tit_ap then tit_ap.cod_espec_docto else "" at 107 format "x(3)"
                           if avail tit_ap then tit_ap.cod_ser_docto else "" at 111 format "x(5)"
                           if avail tit_ap then fill(" ",16 - length (trim(tit_ap.cod_tit_ap))) + trim(tit_ap.cod_tit_ap) else "" at 117 format "x(16)"
                           if avail tit_ap then tit_ap.cod_parcela else "" at 136 format "x(02)".
    put stream s_1 unformatted 
                           tt_rpt_diario_auxiliar_apb.tta_cod_portador at 139 format "x(5)"
                           tt_rpt_diario_auxiliar_apb.tta_num_cheque to 156 format ">>>>,>>>,>>9"
                           tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_db to 172 format "->>>,>>>,>>9.99" skip
                           tt_rpt_diario_auxiliar_apb.ttv_cod_cta_ctbl_contra at 3 format "x(20)"
                           v_des_histor_diario_aux at 24 format "x(74)" skip.               
                   end.           
                   else do:
                       if (line-counter(s_1) + 1) > v_rpt_s_1_bottom then
                           page stream s_1.
                       put stream s_1 unformatted 
                           tt_rpt_diario_auxiliar_apb.ttv_ind_trans_ap_abrev at 22 format "X(04)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_refer at 28 format "x(10)"
                           tt_rpt_diario_auxiliar_apb.tta_cdn_fornecedor to 49 format ">>>,>>>,>>9"
                           tt_rpt_diario_auxiliar_apb.tta_nom_abrev at 51 format "x(15)"
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(3)"
    &ENDIF.
    put stream s_1 unformatted 
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                           tt_rpt_diario_auxiliar_apb.tta_cod_estab at 67 format "x(5)"
    &ENDIF
                           tt_rpt_diario_auxiliar_apb.tta_cod_plano_ccusto at 73 format "x(8)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_ccusto at 86 format "x(20)"
                           if avail tit_ap then tit_ap.cod_espec_docto else "" at 107 format "x(3)"
                           if avail tit_ap then tit_ap.cod_ser_docto else "" at 111 format "x(5)"
                           if avail tit_ap then fill(" ",16 - length (trim(tit_ap.cod_tit_ap))) + trim(tit_ap.cod_tit_ap) else "" at 117 format "x(16)"
                           if avail tit_ap then tit_ap.cod_parcela else "" at 136 format "x(02)"
                           tt_rpt_diario_auxiliar_apb.tta_cod_portador at 139 format "x(5)".
    put stream s_1 unformatted 
                           tt_rpt_diario_auxiliar_apb.tta_num_cheque to 156 format ">>>>,>>>,>>9"
                           tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_db to 172 format "->>>,>>>,>>9.99" skip.
                   end.                   
               end.
           end /* else */.
           assign v_val_tot_db_cta_ctbl = v_val_tot_db_cta_ctbl + tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_db.
       end /* else */.

    /* --------- IMPRESS«O DO HIST‡RICO ------------ */


       /* Begin_Include: i_tt_rpt_diario_auxiliar_apb_ajuste_dec */
       if v_log_localiz_chi then do: 
           if avail tt_rpt_diario_auxiliar_apb then do:
               if tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_cr = 0 then do:               
                   run prgint/ufn/ufn201aa.py(input v_cod_finalid_econ,                                                                                                  
                                              input tt_rpt_diario_auxiliar_apb.tta_dat_transacao,                                                                                  
                                              input tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_db,                                                                                         
                                              input tt_rpt_diario_auxiliar_apb.tta_ind_natur_lancto_ctbl,                                                                             
                                              input tt_rpt_diario_auxiliar_apb.tta_cod_plano_cta_ctbl,
                                              input tt_rpt_diario_auxiliar_apb.tta_cod_estab,                                                                                
                                              output v_cod_cta_ajust_dec_chi_return,                                                                                     
                                              output v_ind_natur_lancto_ctbl_return,                                                                                     
                                              output v_val_ajust_dec_chi_return,                                                                                         
                                              output v_val_movto_return).

                   if v_val_ajust_dec_chi_return <> 0 then do:
                        do  v_num_acum = 1 to num-entries(v_des_histor_diario_aux, chr(10)):   
                            assign v_des_histor_diario_aux = replace(v_des_histor_diario_aux,chr(10),chr(32)) + ' Ajustes Decimales':U.
                        end.    
                   end.
                   else do:
                       do  v_num_acum = 1 to num-entries(v_des_histor_diario_aux, chr(10)):   
                           assign v_des_histor_diario_aux = replace(v_des_histor_diario_aux,chr(10),chr(32)).
                       end.    
                   end.          
               end.
               else do:
                   run prgint/ufn/ufn201aa.py(input v_cod_finalid_econ,                                                                                                  
                                              input tt_rpt_diario_auxiliar_apb.tta_dat_transacao,                                                                                  
                                              input tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_cr,                                                                                         
                                              input tt_rpt_diario_auxiliar_apb.tta_ind_natur_lancto_ctbl,                                                                             
                                              input tt_rpt_diario_auxiliar_apb.tta_cod_plano_cta_ctbl,
                                              input tt_rpt_diario_auxiliar_apb.tta_cod_estab,                                                                                
                                              output v_cod_cta_ajust_dec_chi_return,                                                                                     
                                              output v_ind_natur_lancto_ctbl_return,                                                                                     
                                              output v_val_ajust_dec_chi_return,                                                                                         
                                              output v_val_movto_return).

                   if v_val_ajust_dec_chi_return <> 0 then do:
                       do  v_num_acum = 1 to num-entries(v_des_histor_diario_aux, chr(10)):   
                           assign v_des_histor_diario_aux = replace(v_des_histor_diario_aux,chr(10),chr(32)) + ' Ajustes Decimales':U.
                       end.    
                   end.              
                   else do:
                       do  v_num_acum = 1 to num-entries(v_des_histor_diario_aux, chr(10)):   
                           assign v_des_histor_diario_aux = replace(v_des_histor_diario_aux,chr(10),chr(32)).
                       end.    
                   end.          
               end.
           end.    
       end.    
       else do: 
           do  v_num_acum = 1 to num-entries(v_des_histor_diario_aux, chr(10) ):
               assign v_des_histor_diario_aux = replace(v_des_histor_diario_aux,chr(10),chr(32)).
           end.
       end.                           
       /* End_Include: i_tt_rpt_diario_auxiliar_apb_ajuste_dec */


       assign v_des_histor_diario_aux = replace(v_des_histor_diario_aux,chr(13),chr(32))
              v_num_page = round((length(v_des_histor_diario_aux)) / 75,0).

       if (line-counter(s_1) + v_num_page) > v_rpt_s_1_bottom then
           page stream s_1.

       if v_log_impr_cta_contra_partida = no then do:
           do v_num_cont = 1 to length(v_des_histor_diario_aux):
               put stream s_1 unformatted substring(v_des_histor_diario_aux,v_num_cont,75) at 25 skip.
               assign v_num_cont = v_num_cont + 74.
           end.
       end.

       /* ATUALIZAÄ«O DAS APROPRIAÄÂES, CASO CONTABILIZE SINTETICAMENTE */
       if  v_log_atualiz_numer_pag = yes
       then do:
           do transaction:
               if  param_ctbz.ind_tip_ctbz = "SintÇtico" /*l_sintetico*/ 
               then do:
                   assign v_rec_table = tt_rpt_diario_auxiliar_apb.ttv_rec_aprop_ctbl_ap.
                   find aprop_ctbl_ap where recid(aprop_ctbl_ap) = v_rec_table exclusive-lock no-error.
                   assign aprop_ctbl_ap.num_livro_fisc     = v_num_livro
                          aprop_ctbl_ap.num_pag_livro_fisc = (page-number (s_1) + v_rpt_s_1_page).
               end /* if */.
           end.
       end /* if */.

       /* IMPRESS«O DOS TOTAIS */
       if  last-of(tt_rpt_diario_auxiliar_apb.tta_cod_cta_ctbl)
       then do:
            if  not v_log_funcao_sel_cc
            then do:
                if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "---------------" to 138
                    "---------------" to 154 skip
                    "Total da Conta  " at 106
                    ":" at 122
                    v_val_tot_db_cta_ctbl to 138 format ">>>>>>>>,>>9.99"
                    v_val_tot_cr_cta_ctbl to 154 format ">>>>>>>>,>>9.99" skip (1).
            end /* if */.           
            else do:
                if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "---------------" at 158
                    "---------------" at 174 skip
                    "Total da Conta  " at 140
                    ":" at 156
                    v_val_tot_db_cta_ctbl to 172 format ">>>>>>>>,>>9.99"
                    v_val_tot_cr_cta_ctbl to 188 format ">>>>>>>>,>>9.99" skip (1).
            end /* else */.           
            assign v_val_tot_cr_dia      = v_val_tot_cr_dia + v_val_tot_cr_cta_ctbl
                   v_val_tot_db_dia      = v_val_tot_db_dia + v_val_tot_db_cta_ctbl
                   v_val_tot_cr_cta_ctbl = 0
                   v_val_tot_db_cta_ctbl = 0
                   v_log_primei_cta_ctbl = yes.
       end /* if */.

       if  last-of(tt_rpt_diario_auxiliar_apb.tta_dat_transacao)
       then do:
            if  not v_log_funcao_sel_cc
            then do:
                if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "Total do Dia   " at 97
                    tt_rpt_diario_auxiliar_apb.tta_dat_transacao at 112 format "99/99/9999"
                    ":" at 122
                    v_val_tot_db_dia to 138 format ">>>>>>>>,>>9.99"
                    v_val_tot_cr_dia to 154 format ">>>>>>>>,>>9.99" skip (1).
            end /* if */.           
            else do:
                if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "Total do Dia   " at 131
                    tt_rpt_diario_auxiliar_apb.tta_dat_transacao at 146 format "99/99/9999"
                    ":" at 156
                    v_val_tot_db_dia to 172 format ">>>>>>>>,>>9.99"
                    v_val_tot_cr_dia to 188 format ">>>>>>>>,>>9.99" skip (1).
            end /* else */.           
            assign v_val_tot_cr_plano = v_val_tot_cr_plano + v_val_tot_cr_dia
                   v_val_tot_db_plano = v_val_tot_db_plano + v_val_tot_db_dia
                   v_val_tot_cr_dia   = 0
                   v_val_tot_db_dia   = 0.
       end /* if */.
       if  last-of(tt_rpt_diario_auxiliar_apb.tta_cod_plano_cta_ctbl)
       then do:
            if  not v_log_funcao_sel_cc
            then do:
                if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "Total do Plano  " at 98
                    tt_rpt_diario_auxiliar_apb.tta_cod_plano_cta_ctbl at 114 format "x(8)"
                    ":" at 122
                    v_val_tot_db_plano to 138 format ">>>>>>>>,>>9.99"
                    v_val_tot_cr_plano to 154 format ">>>>>>>>,>>9.99" skip (1).
            end /* if */.           
            else do:
                if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "Total do Plano  " at 132
                    tt_rpt_diario_auxiliar_apb.tta_cod_plano_cta_ctbl at 148 format "x(8)"
                    ":" at 156
                    v_val_tot_db_plano to 172 format ">>>>>>>>,>>9.99"
                    v_val_tot_cr_plano to 188 format ">>>>>>>>,>>9.99" skip (1).
            end /* else */.           
            assign v_val_tot_cr_period = v_val_tot_cr_period + v_val_tot_cr_plano
                   v_val_tot_db_period = v_val_tot_db_period + v_val_tot_db_plano
                   v_val_tot_cr_plano  = 0
                   v_val_tot_db_plano  = 0.

           if  v_log_atualiz_numer_pag = yes
           and v_num_pag = livro_fisc.num_pag_livro - 1
           then do:
               assign v_log_primei_plano = yes.
           end /* if */.
       end /* if */.

       /* VERIFICA A NECESSIDADE DE IMPRESS«O DOS TERMOS DO LIVRO */
       if v_log_atualiz_numer_pag = yes then
           run pi_imprimir_termos_diario_aux_apb /*pi_imprimir_termos_diario_aux_apb*/.
*/

/* gerando registros tabela - ini */
       assign i-transacao = fc-transacao(tt_rpt_diario_auxiliar_apb.ttv_ind_trans_ap_abrev).

       find first tit_ap no-lock
            where recid(tit_ap) = tt_rpt_diario_auxiliar_apb.ttv_rec_tit_ap no-error.

       create es_diario2.
       assign es_diario2.ep_codigo    = tit_ap.cod_empresa
              es_diario2.data_mov     = tt_rpt_diario_auxiliar_apb.tta_dat_transacao
              es_diario2.cod_cta_ctbl = tt_rpt_diario_auxiliar_apb.tta_cod_cta_ctbl +
                                        tt_rpt_diario_auxiliar_apb.tta_cod_unid_negoc +
                                       (if  tt_rpt_diario_auxiliar_apb.tta_cod_ccusto = "" then "000000"
                                        else tt_rpt_diario_auxiliar_apb.tta_cod_ccusto)
              es_diario2.cod_emitente = tit_ap.cdn_fornecedor
              es_diario2.cod_estabe   = tit_ap.cod_estab
              es_diario2.cod_esp      = tit_ap.cod_espec_docto
              es_diario2.serie        = tit_ap.cod_ser_docto
              es_diario2.nr_docto     = tit_ap.cod_tit_ap
              es_diario2.parcela      = tit_ap.cod_parcela
              es_diario2.portador     = tt_rpt_diario_auxiliar_apb.tta_cod_portador
              es_diario2.transacao    = i-transacao
              es_diario2.referencia   = tt_rpt_diario_auxiliar_apb.tta_cod_refer
              es_diario2.c_lancamento = tt_rpt_diario_auxiliar_apb.tta_ind_natur_lancto_ctbl
              es_diario2.vl           =(if  tt_rpt_diario_auxiliar_apb.tta_ind_natur_lancto_ctbl = "DB" then
                                            tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_db
                                        else tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_cr).

        release es_diario2 no-error.    

/* gerando registros tabela - fim */

       delete tt_rpt_diario_auxiliar_apb.
    end /* for grp_block */.
/*
    if  not v_log_funcao_sel_cc
    then do:
        if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
            page stream s_1.
        put stream s_1 unformatted 
            "Total do Per°odo" at 103
            ":" at 122
            v_val_tot_db_period to 138 format ">>>>>>>>,>>9.99"
            v_val_tot_cr_period to 154 format ">>>>>>>>,>>9.99" skip (1).
    end /* if */.           
    else do:
        if (line-counter(s_1) + 2) > v_rpt_s_1_bottom then
            page stream s_1.
        put stream s_1 unformatted 
            "Total do Per°odo" at 137
            ":" at 156
            v_val_tot_db_period to 172 format ">>>>>>>>,>>9.99"
            v_val_tot_cr_period to 188 format ">>>>>>>>,>>9.99" skip (1).
    end /* else */.           

    if not v_log_funcao_sel_cc then do:
        if v_log_impr_cta_contra_partida then  
            hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_cont_partida.
        else  
            hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_diario.
    end.        
    else do: 
        if v_log_impr_cta_contra_partida then 
            hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_cont_part_cc.
        else      
            hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_diario_cc.
    end.    

    /* ATUALIZAÄ«O FINAL, PODENDO ENCERRAR O LIVRO */
    if v_log_atualiz_numer_pag = yes then
        run pi_atualiz_num_pag_livro_fiscal_3 /*pi_atualiz_num_pag_livro_fiscal_3*/.

    if  v_log_atualiz_numer_pag = yes
    then do:
        if pag_livro_fisc.log_livro_fisc_encerdo = no then
            assign v_rpt_s_1_page     = - 1 - page-number(s_1)
                   v_nom_report_title = v_rpt_s_1_name.
        else           
            assign v_rpt_s_1_page     = - page-number(s_1)
                   v_nom_report_title = v_rpt_s_1_name.
    end /* if */.
*/
END PROCEDURE. /* pi_rpt_aprop_ctbl_ap_diario_aux */
/*****************************************************************************
** Procedure Interna.....: pi_tratar_impressao_historico
** Descricao.............: pi_tratar_impressao_historico
** Criado por............: fut36887
** Criado em.............: 12/08/2005 18:39:38
** Alterado por..........: fut41061
** Alterado em...........: 06/05/2009 14:46:03
*****************************************************************************/
PROCEDURE pi_tratar_impressao_historico:

    /************************ Parameter Definition Begin ************************/

    def Input param p_log_impr_histor
        as logical
        format "Sim/N∆o"
        no-undo.
    def Input param p_rec_table
        as recid
        format ">>>>>>9"
        no-undo.
    def input-output param p_des_histor_diario_aux
        as character
        format "x(74)"
        no-undo.


    /************************* Parameter Definition End *************************/

    if  p_log_impr_histor = no
    then do:
       assign p_rec_table = tt_rpt_diario_auxiliar_apb.ttv_rec_movto_tit_ap.
       find movto_tit_ap where recid(movto_tit_ap) = p_rec_table no-lock no-error.

       run pi_montar_histor_apb (buffer tit_ap,
                                 buffer movto_tit_ap,
                                 Input tt_rpt_diario_auxiliar_apb.tta_num_cheque,
                                 Input tt_rpt_diario_auxiliar_apb.tta_num_bord_ap,
                                 output p_des_histor_diario_aux) /*pi_montar_histor_apb*/.

       end /* if */.
    else do: 
       assign p_rec_table = tt_rpt_diario_auxiliar_apb.ttv_rec_movto_tit_ap.
       find movto_tit_ap where recid(movto_tit_ap) = p_rec_table no-lock no-error.

       find first histor_tit_movto_ap no-lock
            where histor_tit_movto_ap.cod_estab           = movto_tit_ap.cod_estab
              and histor_tit_movto_ap.num_id_tit_ap       = movto_tit_ap.num_id_tit_ap
              and histor_tit_movto_ap.num_id_movto_tit_ap = movto_tit_ap.num_id_movto_tit_ap
              and histor_tit_movto_ap.ind_orig_histor_ap  <> "Sistema" /*l_sistema*/ 
              and histor_tit_movto_ap.ind_orig_histor_ap  <> "Erro" /*l_erro*/  no-error.
       if not avail histor_tit_movto_ap
       or histor_tit_movto_ap.des_text_histor = "" 
       then do:
           assign p_rec_table = tt_rpt_diario_auxiliar_apb.ttv_rec_movto_tit_ap.
           find movto_tit_ap where recid(movto_tit_ap) = p_rec_table no-lock no-error.

           run pi_montar_histor_apb (buffer tit_ap,
                                     buffer movto_tit_ap,
                                     Input tt_rpt_diario_auxiliar_apb.tta_num_cheque,
                                     Input tt_rpt_diario_auxiliar_apb.tta_num_bord_ap,
                                     output p_des_histor_diario_aux) /*pi_montar_histor_apb*/.
       end.    
       else do:
           assign p_des_histor_diario_aux = replace(histor_tit_movto_ap.des_text_histor,chr(10),chr(32))
                  p_des_histor_diario_aux = replace(p_des_histor_diario_aux,chr(13),chr(32)).
       end.
    end /* else */.
END PROCEDURE. /* pi_tratar_impressao_historico */
/*****************************************************************************
** Procedure Interna.....: pi_imprimir_termos_diario_aux_apb
** Descricao.............: pi_imprimir_termos_diario_aux_apb
** Criado por............: Uno
** Criado em.............: 20/05/1996 10:34:55
** Alterado por..........: fut41061
** Alterado em...........: 29/08/2008 15:20:08
*****************************************************************************/
PROCEDURE pi_imprimir_termos_diario_aux_apb:

    if  v_num_pag = livro_fisc.num_pag_livro - 1
    and ((line-counter(s_1) + 8) > v_rpt_s_1_bottom)
    then do:
        page stream s_1.
    end /* if */.

    if  v_num_pag <> (page-number (s_1) + v_rpt_s_1_page)
    then do:
        assign v_num_pag = v_num_pag + 1.
        if  v_num_pag = livro_fisc.num_pag_livro
        then do:
            assign pag_livro_fisc.num_ult_pag = v_num_pag.
            if not v_log_funcao_sel_cc then
                hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_diario.
            else hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_diario_cc.

            assign v_des_termo_encert = replace(livro_fisc.des_termo_encert,chr(64) + "Pag" /*l_pag*/  + chr(64),string(v_num_pag))
                   v_des_termo_abert  = replace(livro_fisc.des_termo_abert,chr(64) + "Pag" /*l_pag*/  + chr(64),string(v_num_pag)) no-error.

            run pi_print_editor ('s_1', v_des_termo_encert, '     060', '', '     ', '', '     ').
            put stream s_1 unformatted 
                skip (3)
                'T E R M O  D E  E N C E R R A M E N T O     ' at 47
                skip (1)
                entry(1, return-value, chr(255)) at 37 format 'x(60)' skip.
            run pi_print_editor ('s_1', v_des_termo_encert, 'at037060', '', '', '', '').

            assign v_rpt_s_1_page = (page-number (s_1)) * -1
                   v_num_pag      = 1.

            page stream s_1.

            pag_livro_fisc.log_livro_fisc_encerdo = yes.

            run pi_print_editor ("s_1", v_des_termo_abert, "     060", "", "     ", "", "     ").
            put stream s_1 unformatted 
                skip (3)
                "T E R M O  D E  A B E R T U R A     " at 51
                skip (1)
                entry(1, return-value, chr(255)) at 37 format "x(60)" skip.
            run pi_print_editor ("s_1", v_des_termo_abert, "at037060", "", "", "", "").
            page stream s_1.

            assign v_num_livro = v_num_livro + 1.

            assign v_nom_tit_aux = v_rpt_s_1_name + "  " + "Livro" /*l_livro*/  + ": " + string(v_num_livro,">>>9")
                   v_nom_report_title = fill(" ", 40 - length(v_nom_tit_aux)) + v_nom_tit_aux.

            create pag_livro_fisc.
            assign 
                   pag_livro_fisc.cod_unid_organ     = livro_fisc.cod_unid_organ
                   pag_livro_fisc.cod_modul_dtsul    = livro_fisc.cod_modul_dtsul
                   pag_livro_fisc.ind_tip_livro_fisc = livro_fisc.ind_tip_livro_fisc.
            assign pag_livro_fisc.dat_inic_emis         = v_dat_inic_diario_aux_apb
                   pag_livro_fisc.dat_fim_emis          = v_dat_fim_diario_aux_apb
                   pag_livro_fisc.num_livro_fisc        = v_num_livro
                   pag_livro_fisc.num_primei_pag        = v_num_pag
                   pag_livro_fisc.num_ult_pag           = v_num_pag
                   pag_livro_fisc.cod_usuar_gerac_movto = v_cod_usuar_corren
                   pag_livro_fisc.dat_gerac_movto       = today
                   pag_livro_fisc.hra_gerac_movto       = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":","").

            assign v_num_pag = v_num_pag + 1
                   v_log_primei_cta_ctbl = yes.
            if not v_log_funcao_sel_cc then do:               
                if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "Conta Cont†bil" at 1
                    "Trans" at 22
                    "Refer" at 28
                    "Fornecedor" to 49
                    "Nome Abrev" at 51
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                    "Estab" at 67
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                    "Estab" at 67
    &ENDIF
                    "Esp" at 73
                    "SÇrie" at 77
                    "T°tulo" to 98
                    "/P" at 102
                    "Port" at 105
                    "Num Cheque" to 122
                    "Valor DÇbito" to 138
                    "Valor CrÇdito" to 154 skip
                    "--------------------" at 1
                    "-----" at 22
                    "----------" at 28
                    "-----------" to 49
                    "---------------" at 51
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                    "-----" at 67
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                    "-----" at 67
    &ENDIF
                    "---" at 73
                    "-----" at 77
                    "----------------" to 98
                    "--" at 102
                    "-----" at 105
                    "------------" to 122
                    "---------------" to 138
                    "---------------" to 154 skip.
                hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_cont_partida.
                hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_cont_part_cc.
                hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_diario_cc.
                hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_histor.
                view stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_diario.
            end.
            else do:
                if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
                    page stream s_1.
                put stream s_1 unformatted 
                    "Conta Cont†bil" at 1
                    "Trans" at 22
                    "Refer" at 28
                    "Fornecedor" to 49
                    "Nome Abrev" at 51
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                    "Estab" at 67
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                    "Estab" at 67
    &ENDIF
                    "Plano CCusto" at 73
                    "Centro de Custo" at 86
                    "Esp" at 107
                    "SÇrie" at 111
                    "T°tulo" at 117
                    "/P" at 136
                    "Port" at 139
                    "Num Cheque" to 156
                    "Valor DB" to 172
                    "Valor CR" to 188 skip
                    "--------------------" at 1
                    "-----" at 22
                    "----------" at 28
                    "-----------" to 49
                    "---------------" at 51
    &IF "{&emsfin_version}" >= "" AND "{&emsfin_version}" < "5.07A" &THEN
                    "-----" at 67
    &ENDIF
    &IF "{&emsfin_version}" >= "5.07A" AND "{&emsfin_version}" < "9.99" &THEN
                    "-----" at 67
    &ENDIF
                    "------------" at 73
                    "--------------------" at 86
                    "---" at 107
                    "-----" at 111
                    "----------------" at 117
                    "--" at 136
                    "-----" at 139
                    "------------" to 156
                    "---------------" to 172
                    "---------------" to 188 skip.
                hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_cont_partida.
                hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_cont_part_cc.
                hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_diario.
                hide stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_histor.
                view stream s_1 frame f_rpt_s_1_Grp_detalhe_Lay_diario_cc.
            end.
        end /* if */.
    end /* if */.

END PROCEDURE. /* pi_imprimir_termos_diario_aux_apb */
/*****************************************************************************
** Procedure Interna.....: pi_retornar_finalid_econ_corren_estab
** Descricao.............: pi_retornar_finalid_econ_corren_estab
** Criado por............: 
** Criado em.............: // 
** Alterado por..........: fut41061
** Alterado em...........: 27/04/2009 08:43:48
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
       find pais no-lock
            where pais.cod_pais = estabelecimento.cod_pais
             no-error.
       assign p_cod_finalid_econ = pais.cod_finalid_econ_pais.
    end.
END PROCEDURE. /* pi_retornar_finalid_econ_corren_estab */
/*****************************************************************************
** Procedure Interna.....: pi_converter_para_inteiro
** Descricao.............: pi_converter_para_inteiro
** Criado por............: vanei
** Criado em.............: 24/11/1995 14:34:38
** Alterado por..........: fut1236
** Alterado em...........: 23/06/2005 15:25:06
*****************************************************************************/
PROCEDURE pi_converter_para_inteiro:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_initial
        as character
        format "x(8)"
        no-undo.
    def output param p_num_return
        as integer
        format ">>>>,>>9"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_cod_count                      as character       no-undo. /*local*/
    def var v_num_count                      as integer         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    assign p_num_return = 0.
    if  p_cod_initial = ""
    then do:
        return.
    end /* if */.

    verifica_block:
    do v_num_count = 1 to length(p_cod_initial):
        assign v_cod_count = substring(p_cod_initial, v_num_count, 1).
        if  index("0123456789", v_cod_count) = 0
        then do:
            return.
        end /* if */.
    end /* do verifica_block */.
    /* * PI Alterada sob demanda n∆o retirar o "no-error" * **/
    assign p_num_return = integer(p_cod_initial) no-error.
END PROCEDURE. /* pi_converter_para_inteiro */
/*****************************************************************************
** Procedure Interna.....: pi_retornar_ult_dia_mes
** Descricao.............: pi_retornar_ult_dia_mes
** Criado por............: Uno
** Criado em.............: 30/04/1996 11:00:43
** Alterado por..........: Uno
** Alterado em...........: 30/04/1996 13:42:24
*****************************************************************************/
PROCEDURE pi_retornar_ult_dia_mes:

    /************************ Parameter Definition Begin ************************/

    def Input param p_dat_inic
        as date
        format "99/99/9999"
        no-undo.
    def output param p_dat_fim
        as date
        format "99/99/9999"
        no-undo.


    /************************* Parameter Definition End *************************/

    assign p_dat_fim = date(month(p_dat_inic), 15, year(p_dat_inic)) + 30
           p_dat_fim = date(month(p_dat_fim), 01, year(p_dat_fim)) - 1.

END PROCEDURE. /* pi_retornar_ult_dia_mes */
/*****************************************************************************
** Procedure Interna.....: pi_montar_histor_apb
** Descricao.............: pi_montar_histor_apb
** Criado por............: Uno
** Criado em.............: 09/05/1996 11:11:11
** Alterado por..........: fut1090
** Alterado em...........: 16/09/2004 09:32:16
*****************************************************************************/
PROCEDURE pi_montar_histor_apb:

    /************************ Parameter Definition Begin ************************/

    def param buffer p_tit_ap
        for tit_ap.
    def param buffer p_movto_tit_ap
        for movto_tit_ap.
    def Input param p_num_cheque
        as integer
        format ">>>>,>>>,>>9"
        no-undo.
    def Input param p_num_bord_ap
        as integer
        format ">>>>>9"
        no-undo.
    def output param p_cod_return
        as character
        format "x(40)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_cdn_fornecedor
        as Integer
        format ">>>,>>>,>>9":U
        label "Fornecedor"
        column-label "Fornecedor"
        no-undo.
    def var v_cod_empresa
        as character
        format "x(3)":U
        label "Empresa"
        column-label "Empresa"
        no-undo.
    def var v_ind_trans_ap_abrev
        as character
        format "X(04)":U
        label "Transaá∆o"
        column-label "Transaá∆o"
        no-undo.
    def var v_ind_lancto                     as character       no-undo. /*local*/
    def var v_log_achou                      as logical         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    assign p_cod_return = " ".
    run pi_retornar_abrev_trans_ap (Input p_movto_tit_ap.ind_trans_ap,
                                    output v_ind_trans_ap_abrev) /*pi_retornar_abrev_trans_ap*/.

    if  v_ind_trans_ap_abrev = "PGEF" /*l_pgef*/ 
    or  v_ind_trans_ap_abrev = "PECR" /*l_pecr*/  
    or  v_ind_trans_ap_abrev = "EPEF" /*l_EPEF*/ 
    or  v_ind_trans_ap_abrev = "PFCC" /*l_PFCC*/  then
        assign v_cdn_fornecedor = p_movto_tit_ap.cdn_fornec_pef
               v_cod_empresa    = p_movto_tit_ap.cod_empresa.    
    else 
        assign v_cdn_fornecedor = p_tit_ap.cdn_fornecedor
               v_cod_empresa    = p_tit_ap.cod_empresa.    

    assign v_log_achou = no.
    if  avail fornecedor then do:
        if  fornecedor.cod_empresa    = v_cod_empresa
        and fornecedor.cdn_fornecedor = v_cdn_fornecedor then
            assign v_log_achou = yes.
    end.
    if  v_log_achou = no then
        find fornecedor
            where fornecedor.cod_empresa  = v_cod_empresa
            and fornecedor.cdn_fornecedor = v_cdn_fornecedor
            no-lock no-error.

    if  avail p_tit_ap
    and v_ind_trans_ap_abrev <> "EPEF" /*l_epef*/ 
    and v_ind_trans_ap_abrev <> "PGEF" /*l_pgef*/ 
    and v_ind_trans_ap_abrev <> "PECR" /*l_pecr*/ 
    and v_ind_trans_ap_abrev <> "PFCC" /*l_pfcc*/  then do:
        assign p_cod_return = v_ind_trans_ap_abrev + "/" + p_movto_tit_ap.cod_estab.
        if  avail fornecedor then
            assign p_cod_return = p_cod_return + "/" + fornecedor.nom_abrev.

        assign p_cod_return = p_cod_return + "/" + "NR:" /*l_nr:*/  + p_tit_ap.cod_espec_docto + "/" +
                              p_tit_ap.cod_ser_docto + "/" + p_tit_ap.cod_tit_ap + "/" +
                              string(p_tit_ap.cod_parcela).

        if  p_num_cheque > 0 then
            assign p_cod_return = p_cod_return + "/" + "CH:" /*l_ch:*/  + string(p_num_cheque).
        if  p_num_bord_ap > 0 then
            assign p_cod_return = p_cod_return + "/" + "BORD:" /*l_bord:*/  + string(p_num_bord_ap).

        if  v_ind_trans_ap_abrev = "AVCR" /*l_avcr*/  then
            assign p_cod_return = p_cod_return + "/" + "Lancto a" /*l_lancto_a*/  + "CrÇdito" /*l_credito*/ .

        if  v_ind_trans_ap_abrev = "AVDB" /*l_avdb*/  then
            assign p_cod_return = p_cod_return + "/" + "Lancto a" /*l_lancto_a*/  + "DÇbito" /*l_debito*/ .
    end.
    else do:
        assign p_cod_return = v_ind_trans_ap_abrev + "/" + p_movto_tit_ap.cod_estab.
        if  avail fornecedor then
            assign p_cod_return = p_cod_return + "/" + fornecedor.nom_abrev.

        if  v_ind_trans_ap_abrev = "PECR" /*l_pecr*/  then
            assign p_cod_return = p_cod_return + "/" + "Lancto a" /*l_lancto_a*/  + "CrÇdito" /*l_credito*/ .
        if  v_ind_trans_ap_abrev = "PGEF" /*l_pgef*/  then
            assign p_cod_return = p_cod_return + "/" + "Lancto a" /*l_lancto_a*/  + "DÇbito" /*l_debito*/ .
    end.

END PROCEDURE. /* pi_montar_histor_apb */
/*****************************************************************************
** Procedure Interna.....: pi_retornar_abrev_trans_ap
** Descricao.............: pi_retornar_abrev_trans_ap
** Criado por............: Uno
** Criado em.............: 06/05/1996 09:37:50
** Alterado por..........: fut1236
** Alterado em...........: 24/10/2006 08:49:33
*****************************************************************************/
PROCEDURE pi_retornar_abrev_trans_ap:

    /************************ Parameter Definition Begin ************************/

    def Input param p_ind_trans_ap
        as character
        format "X(26)"
        no-undo.
    def output param p_ind_trans_ap_abrev
        as character
        format "X(04)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /* trans_block: */
    case p_ind_trans_ap:
        when "Acerto Valor a CrÇdito" /*l_acerto_valor_a_credito*/ then
            assign p_ind_trans_ap_abrev = "AVCR" /*l_avcr*/ . /* Acerto Valor a CrÇdito */
        when "Acerto Valor a DÇbito" /*l_acerto_valor_a_debito*/ then
            assign p_ind_trans_ap_abrev = "AVDB" /*l_avdb*/ . /* Acerto Valor a DÇbito */
        when "Acerto Valor a Maior" /*l_acerto_valor_a_maior*/ then
            assign p_ind_trans_ap_abrev = "AVMA" /*l_avma*/ . /* Acerto Valor a Maior */
        when "Acerto Valor a Menor" /*l_acerto_valor_a_menor*/ then
            assign p_ind_trans_ap_abrev = "AVMN" /*l_avmn*/ . /* Acerto Valor a Menor */
        when "Alteraá∆o Data Emiss∆o" /*l_alteracao_data_emissao*/ then
            assign p_ind_trans_ap_abrev = "ADEM" /*l_adem*/ . /* Alteraá∆o Data Emiss∆o */
        when "Alteraá∆o Data Vencimento" /*l_alteracao_data_vencimento*/ then
            assign p_ind_trans_ap_abrev = "ADVN" /*l_advn*/ . /* Alteraá∆o Data Vencimento */
        when "Alteraá∆o n∆o Cont†bil" /*l_alteracao_nao_contabil*/ then
            assign p_ind_trans_ap_abrev = "ALNC" /*l_alnc*/ . /* Alteraá∆o n∆o Cont†bil */
        when "Baixa" /*l_baixa*/ then
            assign p_ind_trans_ap_abrev = "BXA" /*l_bxa*/ .  /* Baixa */
        when "Baixa por Substituiá∆o" /*l_baixa_por_substituicao*/ then
            assign p_ind_trans_ap_abrev = "BXSB" /*l_bxsb*/ . /* Baixa por Substituiá∆o */
        when "Baixa por Transf Estab" /*l_baixa_por_transf_estab*/ then
            assign p_ind_trans_ap_abrev = "BXTE" /*l_bxte*/ . /* Baixa por Transf Estab */
        when "Correá∆o de Valor" /*l_correcao_de_valor*/ then
            assign p_ind_trans_ap_abrev = "CVAL" /*l_cval*/ . /* Correá∆o de Valor */
        when "Correá∆o Valor no Pagto" /*l_correcao_valor_no_pagto*/ then
            assign p_ind_trans_ap_abrev = "CVLP" /*l_cvlp*/ . /* Correá∆o Valor no Pagto */
        when "Estorno Acerto Val CrÇdito" /*l_estorno_acerto_val_credito*/ then
            assign p_ind_trans_ap_abrev = "EVCR" /*l_evcr*/ . /* Estorno Acerto Val CrÇdito */
        when "Estorno Acerto Val DÇbito" /*l_estorno_acerto_val_debito*/ then
            assign p_ind_trans_ap_abrev = "EVDB" /*l_evdb*/ . /* Estorno Acerto Val DÇbito */
        when "Estorno Acerto Val Maior" /*l_estorno_acerto_val_maior*/ then
            assign p_ind_trans_ap_abrev = "EVMA" /*l_evma*/ . /* Estorno Acerto Val Maior */
        when "Estorno Acerto Val Menor" /*l_estorno_acerto_val_menor*/ then
            assign p_ind_trans_ap_abrev = "EVMN" /*l_evmn*/ . /* Estorno Acerto Val Menor */
        when "Estorno Baixa por Subst" /*l_estorno_baixa_por_subst*/ then
            assign p_ind_trans_ap_abrev = "EBXS" /*l_ebxs*/ . /* Estorno Baixa por Subst */
        when "Estorno Bxa Transf Estab" /*l_estorno_bxa_transf_estab*/ then
            assign p_ind_trans_ap_abrev = "EBTE" /*l_ebte*/ . /* Estorno Bxa Transf Estab */
        when "Estorno de Baixa" /*l_estorno_de_baixa*/ then
            assign p_ind_trans_ap_abrev = "EBXA" /*l_ebxa*/ . /* Estorno de Baixa */
        when "Estorno Correá∆o Valor" /*l_estorno_correcao_valor*/ then
            assign p_ind_trans_ap_abrev = "ECVL" /*l_ecvl*/ . /* Estorno Correá∆o Valor */
        when "Estorno Correá∆o Val Pagto" /*l_estorno_correcao_val_pagto*/ then
            assign p_ind_trans_ap_abrev = "ECVP" /*l_ecvp*/ . /* Estorno Correá∆o Val Pagto */
        when "Estorno Subst Nota Dupl" /*l_estorno_subst_nota_dupl*/ then
            assign p_ind_trans_ap_abrev = "ESND" /*l_esnd*/ . /* Estorno Subst Nota Dupl */
        when "Estorno Transf Estab" /*l_estorno_transf_estab*/ then
            assign p_ind_trans_ap_abrev = "ETRE" /*l_etre*/ . /* Estorno Transf Estab */
        when "Estorno Transf Unid Negoc" /*l_estorno_transf_unid_negoc*/ then
            assign p_ind_trans_ap_abrev = "ETRU" /*l_etru*/ . /* Estorno Transf Unid Negoc */
        when "Estorno de T°tulo" /*l_estorno_de_titulo*/ then
            assign p_ind_trans_ap_abrev = "ESTT" /*l_estt*/ . /* Estorno de T°tulo */
        when "Implantaá∆o" /*l_implantacao*/ then
            assign p_ind_trans_ap_abrev = "IMPL" /*l_impl*/ . /* Implantaá∆o */
        when "Implantaá∆o a CrÇdito" /*l_implantacao_a_credito*/ then
            assign p_ind_trans_ap_abrev = "IMCR" /*l_imcr*/ . /* Implantaá∆o a CrÇdito */
        when "Implantaá∆o a DÇbito" /*l_implantacao_a_debito*/ then
            assign p_ind_trans_ap_abrev = "IMDB" /*l_imdb*/ . /* Implantaá∆o a DÇbito */
        when "Pagto Extra Fornecedor" /*l_pagto_extra_fornecedor*/ then
            assign p_ind_trans_ap_abrev = "PGEF" /*l_pgef*/ . /* Pagto Extra Fornecedor */
        when "Pagto Extra Fornecedor CR" /*l_pagto_extra_fornecedor_cr*/ then
            assign p_ind_trans_ap_abrev = "PECR" /*l_pecr*/ . /* Pagto Extra Fornecedor CR */
        when "Subst Nota por Duplicata" /*l_subst_nota_por_duplicata*/ then
            assign p_ind_trans_ap_abrev = "SBND" /*l_sbnd*/ . /* Subst Nota por Duplicata */
        when "Transf Estabelecimento" /*l_transf_estabelecimento*/ then
            assign p_ind_trans_ap_abrev = "TRES" /*l_tres*/ . /* Transf Estabelecimento */
        when "Transf Unidade Neg¢cio" /*l_transf_unidade_negocio*/ then
            assign p_ind_trans_ap_abrev = "TRUN" /*l_trun*/ . /* Transf Unidade Neg¢cio */
        when "PEF Cart∆o de CrÇdito" /*l_pef_cartao_credito*/ then
            assign p_ind_trans_ap_abrev = "PFCC" /*l_pfcc*/ . /* Pagamento Extra Fornecedor Cart∆o de CrÇdito */
        when "Compra Cart∆o de CrÇdito" /*l_compra_cartao_credito*/ then
            assign p_ind_trans_ap_abrev = "CPCC" /*l_CPCC*/ . /* Compra Cart∆o de CrÇdito */
        when "Pagto Encontro Contas" /*l_pagto_encontro_contas*/ then
            assign p_ind_trans_ap_abrev = "PGEC" /*l_PGEC*/ . /* Pagamento via Encontro de Contas*/
        when "Estorno Pagto Extra Fornec" /*l_estorno_pagto_extra_fornec*/ then
            assign p_ind_trans_ap_abrev = "EPEF" /*l_EPEF*/ . /* Estorno de Pagto Extra Fornec*/
        when "Conciliaá∆o Banc†ria" /*l_conciliacao_bancaria*/ then
            assign p_ind_trans_ap_abrev = "CBC" /*l_cbc*/ .
        when "Estorno Conciliaá∆o Banc†ria" /*l_estorno_conciliacao_bancaria*/ then
            assign p_ind_trans_ap_abrev = "ECBC" /*l_ecbc*/ .
    end /* case trans_block */.

END PROCEDURE. /* pi_retornar_abrev_trans_ap */
/*****************************************************************************
** Procedure Interna.....: pi_eliminar_pag_livro_fisc
** Descricao.............: pi_eliminar_pag_livro_fisc
** Criado por............: Uno
** Criado em.............: 07/05/1996 11:35:30
** Alterado por..........: Uno
** Alterado em...........: 13/05/1996 08:07:52
*****************************************************************************/
PROCEDURE pi_eliminar_pag_livro_fisc:

    /************************ Parameter Definition Begin ************************/

    def param buffer p_livro_fisc
        for livro_fisc.
    def Input param p_dat_transacao
        as date
        format "99/99/9999"
        no-undo.


    /************************* Parameter Definition End *************************/

    del_pag_block:
    for
    each pag_livro_fisc exclusive-lock
    where pag_livro_fisc.cod_modul_dtsul = p_livro_fisc.cod_modul_dtsul
      and pag_livro_fisc.cod_unid_organ = p_livro_fisc.cod_unid_organ
      and pag_livro_fisc.ind_tip_livro_fisc = p_livro_fisc.ind_tip_livro_fisc

      and pag_livro_fisc.dat_inic_emis >= p_dat_transacao
    &if "{&emsuni_version}" >= "5.01" &then
    use-index pglvrfs_dat_inic
    &endif
     /*cl_eliminar_pag_livro_fisc of pag_livro_fisc*/:
        delete pag_livro_fisc.
    end /* for del_pag_block */.
END PROCEDURE. /* pi_eliminar_pag_livro_fisc */
/*****************************************************************************
** Procedure Interna.....: pi_atualiza_aprop_ctbl_ap_reemis
** Descricao.............: pi_atualiza_aprop_ctbl_ap_reemis
** Criado por............: Uno
** Criado em.............: 07/05/1996 13:32:29
** Alterado por..........: bre17892
** Alterado em...........: 03/02/1999 17:12:27
*****************************************************************************/
PROCEDURE pi_atualiza_aprop_ctbl_ap_reemis:

    /************************ Parameter Definition Begin ************************/

    def Input param p_ind_niv_abert_livro_fisc
        as character
        format "X(15)"
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
    def Input param p_dat_initial
        as date
        format "99/99/9999"
        no-undo.
    def Input param p_dat_final
        as date
        format "99/99/9999"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_dat_transacao
        as date
        format "99/99/9999":U
        label "Data Transaá∆o"
        column-label "Data Transaá∆o"
        no-undo.


    /************************** Variable Definition End *************************/

    data_block:
    do v_dat_transacao = p_dat_initial to p_dat_final:
        if  p_ind_niv_abert_livro_fisc = "Estabelecimento" /*l_estabelecimento*/ 
        then do:
            for each estabelecimento no-lock
                where estabelecimento.cod_empresa = p_cod_unid_organ:
                movto_block:
                for each movto_tit_ap no-lock
                    where movto_tit_ap.cod_estab = estabelecimento.cod_estab
                    and   movto_tit_ap.dat_transacao = v_dat_transacao
                    use-index mvtttp_estab_dat_trans:
                    if  movto_tit_ap.log_ctbz_aprop_ctbl = yes
                    then do:
                        aprop_block:
                        for each aprop_ctbl_ap exclusive-lock
                         where aprop_ctbl_ap.cod_estab = movto_tit_ap.cod_estab
                           and aprop_ctbl_ap.num_id_movto_tit_ap = movto_tit_ap.num_id_movto_tit_ap
                         :
                            assign aprop_ctbl_ap.num_livro_fisc     = 0
                                   aprop_ctbl_ap.num_pag_livro_fisc = 0.
                        end /* for aprop_block */.
                    end /* if */.
                end /* for movto_block */.
            end.    
        end /* if */.
        else do:
            movto_block:
            for each movto_tit_ap no-lock
                where movto_tit_ap.cod_estab     = p_cod_unid_organ
                and   movto_tit_ap.dat_transacao = v_dat_transacao:
                if  movto_tit_ap.log_ctbz_aprop_ctbl = yes
                then do:
                    aprop_block:
                    for each aprop_ctbl_ap exclusive-lock
                     where aprop_ctbl_ap.cod_estab = movto_tit_ap.cod_estab
                       and aprop_ctbl_ap.num_id_movto_tit_ap = movto_tit_ap.num_id_movto_tit_ap
                     :
                        assign aprop_ctbl_ap.num_livro_fisc     = 0
                               aprop_ctbl_ap.num_pag_livro_fisc = 0.
                    end /* for aprop_block */.
                end /* if */.
            end /* for movto_block */.
        end /* else */.
    end /* do data_block */.

END PROCEDURE. /* pi_atualiza_aprop_ctbl_ap_reemis */
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
** Procedure Interna.....: pi_tratar_tt_rpt_movto_tit_ap_diario_aux
** Descricao.............: pi_tratar_tt_rpt_movto_tit_ap_diario_aux
** Criado por............: bre17892
** Criado em.............: 03/02/1999 15:55:53
** Alterado por..........: fut965
** Alterado em...........: 10/09/2003 17:50:21
*****************************************************************************/
PROCEDURE pi_tratar_tt_rpt_movto_tit_ap_diario_aux:

    if  v_log_funcao_sel_cc
    then do:
    /* TRATANDO A SELEÄ«O DE PLANO E C‡DIGO DE CENTRO DE CUSTO - ROSSI */
        if aprop_ctbl_ap.cod_plano_ccusto < v_cod_plano_ccusto_ini or
           aprop_ctbl_ap.cod_plano_ccusto > v_cod_plano_ccusto_fim_1 then
           return.

        if aprop_ctbl_ap.cod_ccusto < v_cod_ccusto_ini or
           aprop_ctbl_ap.cod_ccusto > v_cod_ccusto_fim then
           return.
    end.

    find movto_tit_ap no-lock
        where movto_tit_ap.cod_estab           = aprop_ctbl_ap.cod_estab
        and   movto_tit_ap.num_id_movto_tit_ap = aprop_ctbl_ap.num_id_movto_tit_ap
        no-error.    
    if  avail movto_tit_ap
    then do:

        assign v_ind_trans_ap_abrev = movto_tit_ap.ind_trans_ap_abrev.

        /* TRATANDO A TRANSAÄ«O DO TIPO PAGAMENTO EXTRA FORNECEDOR */
        if  v_ind_trans_ap_abrev <> "PGEF" /*l_pgef*/ 
        and v_ind_trans_ap_abrev <> "PECR" /*l_pecr*/ 
        and v_ind_trans_ap_abrev <> "EPEF" /*l_epef*/ 
        then do:
            find tit_ap no-lock
                 where tit_ap.cod_estab = movto_tit_ap.cod_estab
                   and tit_ap.num_id_tit_ap = movto_tit_ap.num_id_tit_ap
                  no-error.
            if avail tit_ap 
            then  do:
                /* TRATANDO A SELEÄ«O DE ESPêCIE DE DOCUMENTO */
                if  tit_ap.cod_espec_docto < v_cod_espec_docto_ini or
                    tit_ap.cod_espec_docto > v_cod_espec_docto_fim
                then do:
                    return.
                end /* if */.
                assign v_rec_table      = recid(tit_ap)
                       v_cdn_fornecedor = tit_ap.cdn_fornecedor.
            end.           
        end /* if */.
        else do:
            if  movto_tit_ap.cod_espec_docto < v_cod_espec_docto_ini or
                movto_tit_ap.cod_espec_docto > v_cod_espec_docto_fim
            then do:
                return.
            end /* if */.
            assign v_rec_table      = ?
                   v_cdn_fornecedor = movto_tit_ap.cdn_fornec_pef.
        end /* else */.

        /* RETORNAR A FINALIDADE CORRENTE DO ESTABELECIMENTO, QUE SERµ */
        /* UTILIZADA NA PROCURA DO VALOR DA APROPRIAÄ«O CONTµBIL       */
        if  v_cod_estab_aux <> movto_tit_ap.cod_estab
        then do:
            assign v_cod_estab_aux = movto_tit_ap.cod_estab.
            run pi_retornar_finalid_econ_corren_estab (Input v_cod_estab_aux,
                                                       output v_cod_finalid_econ) /*pi_retornar_finalid_econ_corren_estab*/.
        end /* if */.

        assign v_cod_portador = ""
               v_num_cheque   = 0
               v_num_bord_ap  = 0.

        if  v_ind_trans_ap_abrev = "BXA" /*l_bxa*/ 
        or  v_ind_trans_ap_abrev = "PGEF" /*l_pgef*/ 
        or  v_ind_trans_ap_abrev = "PECR" /*l_pecr*/ 
        then do:
            find compl_movto_pagto no-lock
                 where compl_movto_pagto.cod_estab = movto_tit_ap.cod_estab
                   and compl_movto_pagto.num_id_movto_tit_ap = movto_tit_ap.num_id_movto_tit_ap
                  no-error.
            if  avail compl_movto_pagto
            then do:
                assign v_cod_portador = compl_movto_pagto.cod_portador
                       v_num_cheque   = compl_movto_pagto.num_cheque
                       v_num_bord_ap  = compl_movto_pagto.num_bord_ap.
            end /* if */.
        end /* if */.

        run pi_cria_tt_rpt_diario_auxiliar_apb /*pi_cria_tt_rpt_diario_auxiliar_apb*/.
    end /* if */.    
END PROCEDURE. /* pi_tratar_tt_rpt_movto_tit_ap_diario_aux */
/*****************************************************************************
** Procedure Interna.....: pi_vld_aprop_ctbl_ap_diario_aux
** Descricao.............: pi_vld_aprop_ctbl_ap_diario_aux
** Criado por............: bre17760
** Criado em.............: 08/07/1999 10:44:29
** Alterado por..........: fut41162
** Alterado em...........: 24/05/2007 13:46:33
*****************************************************************************/
PROCEDURE pi_vld_aprop_ctbl_ap_diario_aux:

    /************************ Parameter Definition Begin ************************/

    def Input param p_log_verific
        as logical
        format "Sim/N∆o"
        no-undo.


    /************************* Parameter Definition End *************************/

    if p_log_verific = yes then do:

        if  v_cod_estab = " " 
        or  v_cod_estab = ? then do:
            if v_cod_dwb_user begins 'es_' then
                return "Estabelecimento deve ser informado !" /*12*/.
            else do:            
                /* Estabelecimento deve ser informado ! */
                run pi_messages (input "show",
                                 input 12,
                                 input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_12*/.
                assign v_wgh_focus = v_dat_inic_diario_aux_apb:handle in frame f_rpt_41_aprop_ctbl_ap_diario_aux.
                return error.
            end.
        end.

        find estabelecimento no-lock
             where estabelecimento.cod_estab = v_cod_estab
    &if "{&emsuni_version}" >= "5.01" &then
             use-index stblcmnt_id
    &endif
              /*cl_validar_estabelecimento of estabelecimento*/ no-error.
        if  avail estabelecimento
        then do:
            if  estabelecimento.cod_empresa = v_cod_empresa
            then do:
                assign v_cod_unid_organ = estabelecimento.cod_estab.
                find livro_fisc no-lock
                     where livro_fisc.cod_unid_organ = v_cod_unid_organ
                       and livro_fisc.cod_modul_dtsul = "APB"
                       and livro_fisc.ind_tip_livro_fisc = "Di†rio Auxiliar"
    &if "{&emsuni_version}" >= "5.01" &then
                     use-index lvrfsca_id
    &endif
                      /*cl_relat_diario_aux_apb of livro_fisc*/ no-error.
                if  not avail livro_fisc
                then do:
                    if v_cod_dwb_user begins 'es_' then
                        return "Livro Fiscal inexistente !" /*2437*/.
                    else do:            
                        /* Livro Fiscal inexistente ! */
                        run pi_messages (input "show",
                                         input 2437,
                                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                            "Di†rio Auxiliar" /*l_diario_auxiliar*/ , v_cod_unid_organ, "APB" /*l_apb*/)) /*msg_2437*/.
                        assign v_wgh_focus = v_dat_inic_diario_aux_apb:handle in frame f_rpt_41_aprop_ctbl_ap_diario_aux.
                        return error.
                    end.
                end /* if */.
            end /* if */.
            else do:
                if v_cod_dwb_user begins 'es_' then
                    return "Empresa do Estabelecimento Diferente da Empresa do Usu†rio !" /*512*/.
                else do:            
                    /* Empresa do Estabelecimento Diferente da Empresa do Usu†rio ! */
                    run pi_messages (input "show",
                                     input 512,
                                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_512*/.
                    assign v_wgh_focus = v_dat_inic_diario_aux_apb:handle in frame f_rpt_41_aprop_ctbl_ap_diario_aux.
                    return error.
                end.
            end /* else */.
        end /* if */.
        else do:
            if v_cod_dwb_user begins 'es_' then
                return "Estabelecimento Inexistente !" /*120*/.
            else do:    
                /* Estabelecimento Inexistente ! */
                run pi_messages (input "show",
                                 input 120,
                                 input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_120*/.
                assign v_wgh_focus = v_dat_inic_diario_aux_apb:handle in frame f_rpt_41_aprop_ctbl_ap_diario_aux.
                return error.
            end.
        end /* else */.
    end /* if */.

    assign v_log_reemis   = no
           v_dat_ult_emis = ?.

    if  v_log_atualiz_numer_pag = yes
    then do:
        find param_ctbz no-lock
             where param_ctbz.cod_modul_dtsul = "APB"
               and param_ctbz.cod_empresa = v_cod_empres_usuar
    &if "{&emsuni_version}" >= "5.01" &then
             use-index prmctbz_id
    &endif
              /*cl_relat_diario_aux_apb of param_ctbz*/ no-error.
        if  not avail param_ctbz
        then do:
            if v_cod_dwb_user begins 'es_' then
                return "ParÉmetros de Contabilizaá∆o do M¢dulo n∆o encontrados !" /*1095*/.
            else do:
                /* ParÉmetros de Contabilizaá∆o do M¢dulo n∆o encontrados ! */
                run pi_messages (input "show",
                                 input 1095,
                                 input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_1095*/.
                assign v_wgh_focus = v_dat_inic_diario_aux_apb:handle in frame f_rpt_41_aprop_ctbl_ap_diario_aux.
                return error.
            end.
        end /* if */.
        find sit_movimen_modul no-lock
                 where sit_movimen_modul.cod_modul_dtsul = "APB" /*l_apb*/ 
                   and sit_movimen_modul.cod_unid_organ  = v_cod_empres_usuar
                   and sit_movimen_modul.ind_sit_movimen = "Congelado" /*l_congelado*/ 
        &if '{&emsuni_version}' >= "5.01" &then
             use-index stmvmnmd_id
        &endif
              /* cl_relat_diario_aux_apb of sit_movimen_modul*/ no-error.
              if not avail sit_movimen_modul
              then do:
                if v_cod_dwb_user begins 'es_' then
                    return "Situaá∆o do m¢dulo n∆o est† Congelado !" /*8715*/.
                else do:
                    /* Situaá∆o do m¢dulo n∆o est† Congelado ! */
                    run pi_messages (input "show",
                                     input 8715,
                                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_8715*/.                
                    assign v_wgh_focus = v_dat_inic_diario_aux_apb:handle in frame f_rpt_41_aprop_ctbl_ap_diario_aux.
                    return error.
                end.
            end.
        if  v_dat_inic_diario_aux_apb < sit_movimen_modul.dat_inic_sit_movimen
        then do:
            if v_cod_dwb_user begins 'es_' then
                return "Data In°cio abrange o m¢dulo fora da situaá∆o de Congelado !" /*2440*/.
            else do:
                /* Data In°cio abrange o m¢dulo fora da situaá∆o de Congelado ! */
                run pi_messages (input "show",
                                 input 2440,
                                 input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                    v_dat_inic_diario_aux_apb, "APB" /*l_apb*/ , sit_movimen_modul.dat_inic_sit_movimen)) /*msg_2440*/.
                assign v_wgh_focus = v_dat_inic_diario_aux_apb:handle in frame f_rpt_41_aprop_ctbl_ap_diario_aux.
                return error.
            end.
        end /* if */.
        if  v_dat_fim_diario_aux_apb > sit_movimen_modul.dat_fim_sit_movimen
        then do:
            if v_cod_dwb_user begins 'es_' then
                return "Data Final abrange o m¢dulo fora da situaá∆o de Congelado !" /*2441*/.
            else do:
                /* Data Final abrange o m¢dulo fora da situaá∆o de Congelado ! */
                run pi_messages (input "show",
                                 input 2441,
                                 input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                    v_dat_fim_diario_aux_apb, "APB" /*l_apb*/ , sit_movimen_modul.dat_fim_sit_movimen)) /*msg_2441*/.
                assign v_wgh_focus = v_dat_fim_diario_aux_apb:handle in frame f_rpt_41_aprop_ctbl_ap_diario_aux.
                return error.
            end.
        end /* if */.
        if  avail pag_livro_fisc
        then do:
            if  v_dat_inic_diario_aux_apb > pag_livro_fisc.dat_fim_emis + 1
            then do:
                if v_cod_dwb_user begins 'es_' then
                    return "Data In°cio emiss∆o Ç incompat°vel com a £ltima emiss∆o !" /*2443*/.
                else do:
                    /* Data In°cio emiss∆o Ç incompat°vel com a £ltima emiss∆o ! */
                    run pi_messages (input "show",
                                     input 2443,
                                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                        v_dat_inic_diario_aux_apb, pag_livro_fisc.dat_fim_emis)) /*msg_2443*/.
                    assign v_wgh_focus = v_dat_inic_diario_aux_apb:handle in frame f_rpt_41_aprop_ctbl_ap_diario_aux.
                    return error.
                end.
            end /* if */.
            if  v_dat_inic_diario_aux_apb < pag_livro_fisc.dat_fim_emis + 1
            then do:
                if  not can-find(first pag_livro_fisc
                        where pag_livro_fisc.cod_modul_dtsul = livro_fisc.cod_modul_dtsul
                          and pag_livro_fisc.cod_unid_organ = livro_fisc.cod_unid_organ
                          and pag_livro_fisc.ind_tip_livro_fisc = livro_fisc.ind_tip_livro_fisc

                          and pag_livro_fisc.dat_inic_emis = v_dat_inic_diario_aux_apb
    &if "{&emsuni_version}" >= "5.01" &then
                        use-index pglvrfs_dat_inic
    &endif
                         /*cl_reemis_diario_aux_apb of pag_livro_fisc*/)
                        then do:
                    if v_cod_dwb_user begins 'es_' then
                        return "Data In°cio diferente de outras emiss‰es j† efetuadas !" /*2444*/.
                    else do:
                        /* Data In°cio diferente de outras emiss‰es j† efetuadas ! */
                        run pi_messages (input "show",
                                         input 2444,
                                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_2444*/.
                        assign v_wgh_focus = v_dat_inic_diario_aux_apb:handle in frame f_rpt_41_aprop_ctbl_ap_diario_aux.
                        return error.
                    end.
                end /* if */.
                assign v_log_reemis   = yes
                       v_dat_ult_emis = pag_livro_fisc.dat_fim_emis.
            end /* if */.
        end /* if */.
    end /* if */.

do:
/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
    run pi_restricoes in v_prog_filtro_pdf (input rs_cod_dwb_output:screen-value in frame f_rpt_41_aprop_ctbl_ap_diario_aux).
    if return-value = 'nok' then 
        return no-apply.
&endif
/* tech38629 - Fim da alteraá∆o */
    assign v_log_print = yes.
end.

END PROCEDURE. /* pi_vld_aprop_ctbl_ap_diario_aux */
/*****************************************************************************
** Procedure Interna.....: pi_atualiz_num_pag_livro_fiscal
** Descricao.............: pi_atualiz_num_pag_livro_fiscal
** Criado por............: fut1090_2
** Criado em.............: 20/12/2004 14:51:13
** Alterado por..........: fut1090_2
** Alterado em...........: 20/12/2004 14:54:08
*****************************************************************************/
PROCEDURE pi_atualiz_num_pag_livro_fiscal:

    if  v_log_reemis = yes
    then do:
        run pi_eliminar_pag_livro_fisc (buffer livro_fisc,
                                        Input v_dat_inic_diario_aux_apb) /*pi_eliminar_pag_livro_fisc*/.
    end /* if */.
    find first pag_livro_fisc no-lock
         where pag_livro_fisc.cod_modul_dtsul = livro_fisc.cod_modul_dtsul
           and pag_livro_fisc.cod_unid_organ = livro_fisc.cod_unid_organ
           and pag_livro_fisc.ind_tip_livro_fisc = livro_fisc.ind_tip_livro_fisc
          no-error.
    if  avail pag_livro_fisc
    then do:
        assign v_rpt_s_1_page = pag_livro_fisc.num_ult_pag
               v_num_pag      = pag_livro_fisc.num_ult_pag + 1
               v_num_livro    = pag_livro_fisc.num_livro_fisc.

        if  v_num_pag > livro_fisc.num_pag_livro or pag_livro_fisc.log_livro_fisc_encerdo = yes
        then do:
            assign v_rpt_s_1_page   = livro_fisc.num_primei_pag
                   v_num_livro      = v_num_livro + 1
                   v_num_pag        = livro_fisc.num_primei_pag.
        end /* if */.
    end /* if */.
    else do:
        assign v_rpt_s_1_page   = livro_fisc.num_primei_pag
               v_num_livro      = livro_fisc.num_primei_livro_fisc
               v_num_pag        = livro_fisc.num_primei_pag.
    end /* else */.

    assign v_nom_tit_aux = v_rpt_s_1_name + "  " + "Livro" /*l_livro*/  + ": " + string(v_num_livro,">>>9")
           v_nom_report_title = fill(" ", 40 - length(v_nom_tit_aux)) + v_nom_tit_aux.

    run pi_create_pag_livro_fisc /*pi_create_pag_livro_fisc*/.
END PROCEDURE. /* pi_atualiz_num_pag_livro_fiscal */
/*****************************************************************************
** Procedure Interna.....: pi_atualiz_num_pag_livro_fiscal_2
** Descricao.............: pi_atualiz_num_pag_livro_fiscal_2
** Criado por............: fut1090_2
** Criado em.............: 20/12/2004 14:55:25
** Alterado por..........: fut1090_2
** Alterado em...........: 20/12/2004 15:00:12
*****************************************************************************/
PROCEDURE pi_atualiz_num_pag_livro_fiscal_2:

    assign v_des_termo_encert = replace(livro_fisc.des_termo_encert, chr(64) + "Pag" /*l_pag*/  + chr(64) ,string(v_num_pag)) no-error.
    run pi_print_editor ("s_1", v_des_termo_encert, "     060", "", "     ", "", "     ").
    put stream s_1 unformatted 
        skip (3)
        "T E R M O  D E  E N C E R R A M E N T O     " at 47
        skip (1)
        entry(1, return-value, chr(255)) at 37 format "x(60)" skip.
    run pi_print_editor ("s_1", v_des_termo_encert, "at037060", "", "", "", "").
    page stream s_1.
    /* VERIFICA A NECESSIDADE DE IMPRESS«O DO TERMO DE ABERTURA */
    assign v_rpt_s_1_page = livro_fisc.num_primei_pag - page-number(s_1).        
    assign v_des_termo_abert  = replace(livro_fisc.des_termo_abert, chr(64) + "Pag" /*l_pag*/  + chr(64) ,string(v_num_pag)) no-error.
    run pi_print_editor ("s_1", v_des_termo_abert, "     060", "", "     ", "", "     ").
    put stream s_1 unformatted 
        skip (3)
        "T E R M O  D E  A B E R T U R A     " at 51
        skip (1)
        entry(1, return-value, chr(255)) at 37 format "x(60)" skip.
    run pi_print_editor ("s_1", v_des_termo_abert, "at037060", "", "", "", "").
    page stream s_1.
    assign v_num_pag                             = 1
           v_num_livro                           = v_num_livro + 1 
           pag_livro_fisc.log_livro_fisc_encerdo = yes
           pag_livro_fisc.num_ult_pag            = v_num_pag.

    assign v_nom_tit_aux = v_rpt_s_1_name + "  " + "Livro" /*l_livro*/  + ": " + string(v_num_livro,">>>9")
           v_nom_report_title = fill(" ", 40 - length(v_nom_tit_aux)) + v_nom_tit_aux.

    run pi_create_pag_livro_fisc /*pi_create_pag_livro_fisc*/.

END PROCEDURE. /* pi_atualiz_num_pag_livro_fiscal_2 */
/*****************************************************************************
** Procedure Interna.....: pi_create_pag_livro_fisc
** Descricao.............: pi_create_pag_livro_fisc
** Criado por............: fut1090_2
** Criado em.............: 20/12/2004 14:53:15
** Alterado por..........: fut1090_2
** Alterado em...........: 20/12/2004 15:05:40
*****************************************************************************/
PROCEDURE pi_create_pag_livro_fisc:

    do transaction:
        create pag_livro_fisc.
        assign 
               pag_livro_fisc.cod_unid_organ     = livro_fisc.cod_unid_organ
               pag_livro_fisc.cod_modul_dtsul    = livro_fisc.cod_modul_dtsul
               pag_livro_fisc.ind_tip_livro_fisc = livro_fisc.ind_tip_livro_fisc.
        assign pag_livro_fisc.dat_inic_emis         = v_dat_inic_diario_aux_apb
               pag_livro_fisc.dat_fim_emis          = v_dat_fim_diario_aux_apb
               pag_livro_fisc.num_livro_fisc        = v_num_livro
               pag_livro_fisc.num_primei_pag        = v_num_pag
               pag_livro_fisc.num_ult_pag           = v_num_pag
               pag_livro_fisc.cod_usuar_gerac_movto = v_cod_usuar_corren
               pag_livro_fisc.dat_gerac_movto       = today
               pag_livro_fisc.hra_gerac_movto       = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":","").
    end.
END PROCEDURE. /* pi_create_pag_livro_fisc */
/*****************************************************************************
** Procedure Interna.....: pi_atualiz_num_pag_livro_fiscal_3
** Descricao.............: pi_atualiz_num_pag_livro_fiscal_3
** Criado por............: fut1090_2
** Criado em.............: 20/12/2004 15:02:00
** Alterado por..........: fut1090_2
** Alterado em...........: 20/12/2004 15:02:25
*****************************************************************************/
PROCEDURE pi_atualiz_num_pag_livro_fiscal_3:

    assign v_num_pag = (page-number (s_1) + v_rpt_s_1_page).
    if  v_num_pag = livro_fisc.num_pag_livro - 1
    then do:
        assign v_des_termo_encert = replace(livro_fisc.des_termo_encert, chr(64) + "Pag" /*l_pag*/  + chr(64) ,string(v_num_pag + 1)) no-error.
        page stream s_1.
        run pi_print_editor ("s_1", v_des_termo_encert, "     060", "", "     ", "", "     ").
        put stream s_1 unformatted 
            skip (3)
            "T E R M O  D E  E N C E R R A M E N T O     " at 47
            skip (1)
            entry(1, return-value, chr(255)) at 37 format "x(60)" skip.
        run pi_print_editor ("s_1", v_des_termo_encert, "at037060", "", "", "", "").
        page stream s_1.        
        /* VERIFICA A NECESSIDADE DE IMPRESS«O DO TERMO DE ABERTURA */
        assign v_rpt_s_1_page = livro_fisc.num_primei_pag - page-number(s_1).        
        assign v_des_termo_abert  = replace(livro_fisc.des_termo_abert, chr(64) + "Pag" /*l_pag*/  + chr(64) ,string(v_num_pag + 1)) no-error.
        run pi_print_editor ("s_1", v_des_termo_abert, "     060", "", "     ", "", "     ").
        put stream s_1 unformatted 
            skip (3)
            "T E R M O  D E  A B E R T U R A     " at 51
            skip (1)
            entry(1, return-value, chr(255)) at 37 format "x(60)" skip.
        run pi_print_editor ("s_1", v_des_termo_abert, "at037060", "", "", "", "").
        page stream s_1.            
        assign v_num_pag = v_num_pag + 1
               pag_livro_fisc.log_livro_fisc_encerdo = yes.        
    end /* if */.
    else do:
        if  v_log_emis_termo_encert = yes
        then do:
            assign v_des_termo_encert = replace(livro_fisc.des_termo_encert, chr(64) + "Pag" /*l_pag*/  + chr(64) ,string(v_num_pag + 1)) no-error.
            page stream s_1.
            run pi_print_editor ("s_1", v_des_termo_encert, "     060", "", "     ", "", "     ").
            put stream s_1 unformatted 
                skip (3)
                "T E R M O  D E  E N C E R R A M E N T O     " at 47
                skip (1)
                entry(1, return-value, chr(255)) at 37 format "x(60)" skip.
            run pi_print_editor ("s_1", v_des_termo_encert, "at037060", "", "", "", "").
            page stream s_1.
            /* VERIFICA A NECESSIDADE DE IMPRESS«O DO TERMO DE ABERTURA */            
            assign v_rpt_s_1_page = livro_fisc.num_primei_pag - page-number(s_1).
            assign v_des_termo_abert  = replace(livro_fisc.des_termo_abert, chr(64) + "Pag" /*l_pag*/  + chr(64) ,string(v_num_pag + 1)) no-error.
            run pi_print_editor ("s_1", v_des_termo_abert, "     060", "", "     ", "", "     ").
            put stream s_1 unformatted 
                skip (3)
                "T E R M O  D E  A B E R T U R A     " at 51
                skip (1)
                entry(1, return-value, chr(255)) at 37 format "x(60)" skip.
            run pi_print_editor ("s_1", v_des_termo_abert, "at037060", "", "", "", "").
            page stream s_1.                
            assign v_num_pag = v_num_pag + 1
                   pag_livro_fisc.log_livro_fisc_encerdo = yes.
        end /* if */.
    end /* else */.        

    assign pag_livro_fisc.num_ult_pag = v_num_pag.

    if  v_log_reemis = yes
    and param_ctbz.ind_tip_ctbz = "SintÇtico" /*l_sintetico*/ 
    then do:
        run pi_atualiza_aprop_ctbl_ap_reemis (Input livro_fisc.ind_niv_abert_livro_fisc,
                                              Input v_cod_unid_organ,
                                              Input v_dat_fim_diario_aux_apb + 1,
                                              Input v_dat_ult_emis) /*pi_atualiza_aprop_ctbl_ap_reemis*/.
    end /* if */.

END PROCEDURE. /* pi_atualiz_num_pag_livro_fiscal_3 */
/*****************************************************************************
** Procedure Interna.....: pi_consiste_validacoes_fisco
** Descricao.............: pi_consiste_validacoes_fisco
** Criado por............: fut36887
** Criado em.............: 08/08/2005 16:09:29
** Alterado por..........: corp46173
** Alterado em...........: 15/02/2013 17:32:48
*****************************************************************************/
PROCEDURE pi_consiste_validacoes_fisco:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_modulo
        as character
        format "x(50)"
        no-undo.
    def Input param p_dat_inicial
        as date
        format "99/99/9999"
        no-undo.
    def Input param p_dat_final
        as date
        format "99/99/9999"
        no-undo.
    def Input param p_log_dados_fisco
        as logical
        format "Sim/N∆o"
        no-undo.
    def Input param p_cod_empresa
        as character
        format "x(3)"
        no-undo.
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


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_cod_empres_ems2                as character       no-undo. /*local*/
    def var v_dat_fim                        as date            no-undo. /*local*/
    def var v_dat_final_validac              as date            no-undo. /*local*/
    def var v_dat_inicial_validac            as date            no-undo. /*local*/
    def var v_hdl_fisco_ems2                 as handle          no-undo. /*local*/
    def var v_log_achou                      as logical         no-undo. /*local*/
    def var v_log_answer                     as logical         no-undo. /*local*/
    def var v_log_connect                    as logical         no-undo. /*local*/
    def var v_log_connect_ems2               as logical         no-undo. /*local*/
    def var v_log_consist                    as logical         no-undo. /*local*/
    def var v_log_funcao_acerto              as logical         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    assign v_log_consist = no.    
    if  p_log_dados_fisco
    then do:
        /* fut36887 - atividade 139621 - verifica integraá∆o com o EMS2*/ 
        if  &if defined(BF_FIN_FIXAR_PARAM) = 0 &then
            CAN-FIND(first 
                        param_integr_ems no-lock 
                    where 
                        param_integr_ems.ind_param_integr_ems = "Integra Contabilizaá∆o 2.00" /*l_integra_contabilizacao_2.00*/ )
            AND
            &endif
            CAN-FIND(first 
                        param_integr_ems no-lock 
                    where 
                        param_integr_ems.ind_param_integr_ems = "Contabilizaá‰es 2.00" /*l_contabilizacoes_2.00*/ )
            AND
            CAN-FIND(first 
                        param_ctbz NO-LOCK
                     where 
                        param_ctbz.cod_modul_dtsul = "FGL" /*l_fgl*/  and 
                        param_ctbz.cod_empresa = v_cod_empres_usuar and 
                        param_ctbz.log_integr_ctbl_ativ = NO)
        then do:
            if  search("prgint/utb/utb720za.r") = ? and search("prgint/utb/utb720za.py") = ? then do:
                if  v_cod_dwb_user begins 'es_' then
                    return "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb720za.py".
                else do:
                    message "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb720za.py"
                           view-as alert-box error buttons ok.
                    return.
                end.
            end.
            else
                run prgint/utb/utb720za.py (Input 1,
                                        Input "On-Line" /*l_online*/,
                                        output v_log_connect_ems2,
                                        output v_log_connect) /*prg_fnc_conecta_bases_externas*/.
            if  v_log_connect = no and v_log_connect_ems2 = no
            then do:
                if  search("prgint/utb/utb720za.r") = ? and search("prgint/utb/utb720za.py") = ? then do:
                    if  v_cod_dwb_user begins 'es_' then
                        return "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb720za.py".
                    else do:
                        message "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb720za.py"
                               view-as alert-box error buttons ok.
                        return.
                    end.
                end.
                else
                    run prgint/utb/utb720za.py (Input 2,
                                            Input "On-Line" /*l_online*/,
                                            output v_log_connect_ems2,
                                            output v_log_connect) /*prg_fnc_conecta_bases_externas*/.
                /* Erro ao conectar bases do EMS 2 ! */
                run pi_messages (input "show",
                                 input 9989,
                                 input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_9989*/.
                return "NOK" /*l_nok*/ .
            end /* if */.
            else do:

                run cdp/cd0101h.p (output v_log_contabilizacao).

                if not v_log_contabilizacao then do:

                    RUN cdp/cdapi411.p PERSISTENT SET v_hdl_fisco_ems2.
                    RUN pi-verif-saldo-fisc IN v_hdl_fisco_ems2 (OUTPUT v_log_achou) .
                    if  not v_log_achou
                    then do:
                        /* Arquivos Fiscalizaá∆o ! */
                        run pi_messages (input "show",
                                         input 14148,
                                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                            'spap960.p', 'spp/app/spap960.p')) /*msg_14148*/. /* Alteraá∆o por demanda*/
                        return "NOK" /*l_nok*/ .    
                    end /* if */.

                    FIND first 
                        param_integr_ems  
                    where 
                        param_integr_ems.ind_param_integr_ems = "Contabilizaá‰es 2.00" /*l_contabilizacoes_2.00*/  no-lock no-error.

                    FIND 
                        tip_unid_organ 
                    WHERE
                        tip_unid_organ.num_niv_unid_organ = 998 NO-LOCK NO-ERROR.

                    if  AVAIL tip_unid_organ
                    then do: 
                        FIND FIRST 
                            trad_org_ext 
                        USE-INDEX 
                            trdrgxt_id 
                        WHERE
                            trad_org_ext.cod_matriz_trad_org_ext = param_integr_ems.des_contdo_param_integr_ems AND
                            trad_org_ext.cod_tip_unid_organ      = tip_unid_organ.cod_tip_unid_organ            AND
                            trad_org_ext.cod_unid_organ          = p_cod_empresa
                        NO-LOCK NO-ERROR.
                        IF AVAIL trad_org_ext THEN
                            ASSIGN v_cod_empres_ems2 = trad_org_ext.cod_unid_organ_ext.
                        ELSE DO:
                           /* Empresa &1 n∆o cadastrada na Matriz Traduá∆o Organizacional ! */
                           run pi_messages (input "show",
                                            input 13272,
                                            input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                               p_cod_empresa,param_integr_ems.des_contdo_param_integr_ems)) /*msg_13272*/. /* Alteraá∆o por demanda*/
                           return "NOK" /*l_nok*/ .
                        END.

                        RUN cdp/cdapi411.p PERSISTENT SET v_hdl_fisco_ems2.
                        RUN pi-verif-movto-saldo-fisc IN v_hdl_fisco_ems2 (input p_cod_estab,
                                                                           &IF '{&emsfin_version}' < '5.07A' &THEN
                                                                           INPUT int(v_cod_empres_ems2),
                                                                           &ELSE
                                                                           INPUT v_cod_empres_ems2,
                                                                           &ENDIF
                                                                           INPUT p_cod_modulo, 
                                                                           INPUT p_dat_final, 
                                                                           OUTPUT v_log_achou) .
                        if  v_log_achou
                        then do:
                            run pi_messages (input "show",
                                             input 14153,
                                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")).
                            assign v_log_answer = (if   return-value = "yes" then yes
                                                   else if return-value = "no" then no
                                                   else ?) /*msg_14153*/.
                            if v_log_answer <> yes then
                                return "NOK" /*l_nok*/ .
                        end /* if */.
                    end /* if */.
                end.   
            end /* else */.
        end /* if */.                    

        assign v_dat_inicial_validac = date(month(p_dat_inicial), 01, year(p_dat_inicial)).
        ASSIGN v_dat_final_validac = fn_retorna_ultimo_dia_mes_ano(month(p_dat_final),year(p_dat_final)).

        if p_dat_inicial <> v_dat_inicial_validac 
           or
           p_dat_final <> v_dat_final_validac 
           or
           month(p_dat_final) <> month(p_dat_inicial) 
           or
           year(p_dat_final) <> year(p_dat_inicial) then
        do:
            return '14151'.
        end.
        assign v_log_funcao_acerto = can-find(first 
                                                  histor_exec_especial
                                              where
                                                  histor_exec_especial.cod_modul_dtsul = 'FGL' and
                                                  histor_exec_especial.cod_prog_dtsul  = 'SPP_SDO_FISC').

        if  not v_log_funcao_acerto
        then do:
            /* Arquivos Fiscalizaá∆o ! */
            run pi_messages (input "show",
                             input 14148,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                'fgl702ad.p', 'prgfin/fgl/fgl702ad.p')) /*msg_14148*/. /* Alteraá∆o por demanda*/
            return "NOK" /*l_nok*/ .    
        end /* if */.

        if p_cod_estab <> "" then
        do:
            if can-find (first movto_fisc no-lock
                where movto_fisc.cod_empresa = p_cod_empresa 
                  and movto_fisc.cod_modul_dtsul = p_cod_modulo
                  and movto_fisc.cod_estab = p_cod_estab 
                  and movto_fisc.dat_trans_diario > p_dat_final) then
                    assign v_log_consist = yes.   

            if v_log_consist = no then do:
                if can-find (first sdo_cta_ctbl_modul no-lock
                    where sdo_cta_ctbl_modul.cod_empresa = p_cod_empresa 
                      and sdo_cta_ctbl_modul.cod_modul_dtsul = p_cod_modulo 
                      and sdo_cta_ctbl_modul.cod_estab = p_cod_estab 
                      and sdo_cta_ctbl_modul.dat_fim_period_ctbl > p_dat_final) then                
                        assign v_log_consist = yes.    
            end.      
        end.
        else
        do:
            if can-find (first movto_fisc no-lock
                where movto_fisc.cod_empresa = p_cod_empresa 
                  and movto_fisc.cod_modul_dtsul = p_cod_modulo
                  and movto_fisc.dat_trans_diario > p_dat_final) then
                    assign v_log_consist = yes.                            

            if v_log_consist = no then do:
                if can-find (first sdo_cta_ctbl_modul no-lock
                    where sdo_cta_ctbl_modul.cod_empresa = p_cod_empresa 
                      and sdo_cta_ctbl_modul.cod_modul_dtsul = p_cod_modulo
                      and sdo_cta_ctbl_modul.dat_fim_period_ctbl > p_dat_final) then                
                        assign v_log_consist = yes.                
            end.
        end.
        if v_log_consist = yes then
        do:
            run pi_messages (input "show",
                             input 14153,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")).
            assign v_log_answer = (if   return-value = "yes" then yes
                                   else if return-value = "no" then no
                                   else ?) /*msg_14153*/.
            if v_log_answer <> yes then
                return "NOK" /*l_nok*/ .
        end.
    end /* if */.    
    return "OK" /*l_ok*/ .
END PROCEDURE. /* pi_consiste_validacoes_fisco */
/*****************************************************************************
** Procedure Interna.....: pi_recupera_param_rpt_aprop_ctbl_ap_diario_aux
** Descricao.............: pi_recupera_param_rpt_aprop_ctbl_ap_diario_aux
** Criado por............: fut36887
** Criado em.............: 23/09/2005 10:54:17
** Alterado por..........: fut42929
** Alterado em...........: 23/11/2010 14:56:48
*****************************************************************************/
PROCEDURE pi_recupera_param_rpt_aprop_ctbl_ap_diario_aux:

    assign v_cod_empresa                 = getentryfield(1, dwb_rpt_param.cod_dwb_parameters, chr(10))
           v_cod_estab                   = getentryfield(2, dwb_rpt_param.cod_dwb_parameters, chr(10))
           v_dat_inic_diario_aux_apb     = date(getentryfield(3, dwb_rpt_param.cod_dwb_parameters, chr(10)))
           v_dat_fim_diario_aux_apb      = date(getentryfield(4, dwb_rpt_param.cod_dwb_parameters, chr(10)))
           v_log_impr_histor             = (getentryfield(5, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" /*l_yes*/ )
           v_log_atualiz_numer_pag       = (getentryfield(6, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" /*l_yes*/ )
           v_log_emis_termo_encert       = (getentryfield(7, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" /*l_yes*/ )
           v_cod_espec_docto_ini         = getentryfield(8, dwb_rpt_param.cod_dwb_parameters, chr(10))
           v_cod_espec_docto_fim         = getentryfield(9, dwb_rpt_param.cod_dwb_parameters, chr(10))
           v_cod_cta_ctbl_ini            = getentryfield(10, dwb_rpt_param.cod_dwb_parameters, chr(10))
           v_cod_cta_ctbl_fim            = getentryfield(11, dwb_rpt_param.cod_dwb_parameters, chr(10))
           v_cod_plano_cta_ctbl_ini      = getentryfield(12, dwb_rpt_param.cod_dwb_parameters, chr(10))
           v_cod_plano_cta_ctbl_fim      = getentryfield(13, dwb_rpt_param.cod_dwb_parameters, chr(10))
           v_log_reemis                  = (getentryfield(14, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" /*l_yes*/ )
           v_cod_unid_negoc              = getentryfield(21, dwb_rpt_param.cod_dwb_parameters, chr(10))
           v_cod_plano_ccusto_ini        = getentryfield(15, dwb_rpt_param.cod_dwb_parameters, chr(10))
           v_cod_plano_ccusto_fim_1      = getentryfield(16, dwb_rpt_param.cod_dwb_parameters, chr(10))    
           v_cod_ccusto_ini              = getentryfield(17, dwb_rpt_param.cod_dwb_parameters, chr(10))
           v_cod_ccusto_fim              = getentryfield(18, dwb_rpt_param.cod_dwb_parameters, chr(10))
           v_log_dados_fisco             = (getentryfield(19, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" /*l_yes*/ )
           v_cod_pag_folha               = getentryfield(20, dwb_rpt_param.cod_dwb_parameters, chr(10))
           v_log_impr_cta_contra_partida = (getentryfield(22, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" /*l_yes*/ ).

    IF  dwb_rpt_param.ind_dwb_run_mode = "Batch" THEN DO:
        assign v_dat_inic_diario_aux_apb = date(month(today), 1, year(today)).
        run pi_retornar_ult_dia_mes (Input v_dat_inic_diario_aux_apb,
                                     output v_dat_fim_diario_aux_apb) /*pi_retornar_ult_dia_mes*/.
    END.

        IF v_cod_espec_docto_ini    = "" and
           v_cod_espec_docto_fim    = "" and
           v_cod_cta_ctbl_ini       = "" and
           v_cod_cta_ctbl_fim       = "" and
           v_cod_plano_cta_ctbl_ini = "" and
           v_cod_plano_cta_ctbl_fim = "" and
           v_cod_unid_negoc         = "" and
           v_cod_plano_ccusto_ini   = "" and
           v_cod_plano_ccusto_fim_1 = "" and
           v_cod_ccusto_ini         = "" and
           v_cod_ccusto_fim         = "" then do:

           assign v_cod_espec_docto_ini    = "":U
                  v_cod_espec_docto_fim    = 'ZZZ':U
                  v_cod_cta_ctbl_ini       = "":U
                  v_cod_cta_ctbl_fim       = 'ZZZZZZZZZZZZZZZZZZZZ':U
                  v_cod_plano_cta_ctbl_ini = "":U
                  v_cod_plano_cta_ctbl_fim = 'ZZZZZZZZ':U
                  v_cod_unid_negoc         = "":U.

           if v_log_funcao_sel_cc then do:
               assign v_cod_plano_ccusto_ini   = "":U
                      v_cod_plano_ccusto_fim_1 = 'ZZZZZZZZ':U
                      v_cod_ccusto_ini         = "":U
                      v_cod_ccusto_fim         = 'ZZZZZZZZZZZZZZZZZZZZ':U. 
           end.
        end. /* IF*/       

    if v_cod_pag_folha = "Folha" /*l_folha*/  then
        assign v_cod_cabec_label     = 'Folha:'
               v_cod_cabec_label_aux = 'Èltima Folha:'.
    else
        assign v_cod_cabec_label     = 'P†gina:'
               v_cod_cabec_label_aux = 'Èltima P†gina:'.

END PROCEDURE. /* pi_recupera_param_rpt_aprop_ctbl_ap_diario_aux */
/*****************************************************************************
** Procedure Interna.....: pi_trata_erros_diario_aux
** Descricao.............: pi_trata_erros_diario_aux
** Criado por............: fut36887
** Criado em.............: 18/10/2005 15:34:49
** Alterado por..........: fut36887
** Alterado em...........: 19/10/2005 09:25:17
*****************************************************************************/
PROCEDURE pi_trata_erros_diario_aux:

    /************************* Variable Definition Begin ************************/

    def var v_des_help_erro_diario_aux       as character       no-undo. /*local*/
    def var v_des_msg                        as character       no-undo. /*local*/


    /************************** Variable Definition End *************************/

    /* fut36887 - atividade 139621 - tratamento de erros*/    
    if  CAN-FIND(FIRST tt_log_erros)
    then do:
        assign v_des_msg = "DADOS PARA ARQUIVOS FISCALIZAÄ«O." /*14298*/
               v_des_help_erro_diario_aux = "Os dados para arquivos de fiscalizaá∆o n∆o foram gerados devido aos erros listados abaixo:" /*14298*/.
        if (line-counter(s_1) + 5) > v_rpt_s_1_bottom then
            page stream s_1.
        put stream s_1 unformatted 
            '********************************' at 1 skip
            v_des_msg at 1 format "x(40)" skip
            '********************************' at 1 skip
            v_des_help_erro_diario_aux at 1 format "x(100)" skip (1).
        if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
            page stream s_1.
        put stream s_1 unformatted 
            "Erro" to 8
            "Inconsistància" at 10
            "Ajuda" at 61 skip
            "--------" to 8
            "--------------------------------------------------" at 10
            "--------------------------------------------------" at 61 skip.    
        hide stream s_1 frame f_rpt_s_1_Grp_erro_Lay_cabecalho.
        view stream s_1 frame f_rpt_s_1_Grp_erro_Lay_erro.
        FOR EACH 
            tt_log_erros
        break by
            tt_log_erros.ttv_des_erro:
            if  first-of(tt_log_erros.ttv_des_erro)
            then do:
                run pi_print_editor ("s_1", tt_log_erros.ttv_des_ajuda, "     050", "", "     ", "", "     ").
                put stream s_1 unformatted 
                    tt_log_erros.ttv_num_cod_erro to 8 format ">>>>,>>9"
                    tt_log_erros.ttv_des_erro at 10 format "x(50)"
                    entry(1, return-value, chr(255)) at 61 format "x(50)" skip.
                run pi_print_editor ("s_1", tt_log_erros.ttv_des_ajuda, "at061050", "", "", "", "").
            end /* if */.    
        END.
        hide stream s_1 frame f_rpt_s_1_Grp_erro_Lay_erro.
        page stream s_1.
    end /* if */.
END PROCEDURE. /* pi_trata_erros_diario_aux */
/*****************************************************************************
** Procedure Interna.....: pi_seleciona_aprop_ctbl_ap_diario_aux
** Descricao.............: pi_seleciona_aprop_ctbl_ap_diario_aux
** Criado por............: fut41506
** Criado em.............: 29/01/2008 17:15:47
** Alterado por..........: fut41506
** Alterado em...........: 12/03/2008 15:02:24
*****************************************************************************/
PROCEDURE pi_seleciona_aprop_ctbl_ap_diario_aux:

    view frame f_ran_01_aprop_ctbl_ap_diario_aux.
    display bt_can
            bt_hel2
            bt_ok
            v_cod_cta_ctbl_fim
            v_cod_cta_ctbl_ini
            v_cod_espec_docto_fim
            v_cod_espec_docto_ini
            v_cod_plano_cta_ctbl_fim
            v_cod_plano_cta_ctbl_ini
            v_cod_unid_negoc
            with frame f_ran_01_aprop_ctbl_ap_diario_aux.
    if v_log_funcao_sel_cc then do:
        display v_cod_plano_ccusto_ini
                v_cod_plano_ccusto_fim_1
                v_cod_ccusto_ini
                v_cod_ccusto_fim
                with frame f_ran_01_aprop_ctbl_ap_diario_aux.
    end. 

    enable bt_can
           bt_hel2
           bt_ok
           v_cod_cta_ctbl_fim
           v_cod_cta_ctbl_ini
           v_cod_espec_docto_fim
           v_cod_espec_docto_ini
           v_cod_plano_cta_ctbl_fim
           v_cod_plano_cta_ctbl_ini
           v_cod_unid_negoc
           with frame f_ran_01_aprop_ctbl_ap_diario_aux.
    if v_log_funcao_sel_cc then do: 
        enable v_cod_plano_ccusto_ini
               v_cod_plano_ccusto_fim_1
               v_cod_ccusto_ini
               v_cod_ccusto_fim
               with frame f_ran_01_aprop_ctbl_ap_diario_aux.
    end.

    if not v_log_funcao_sel_cc then do:
        assign v_cod_ccusto_ini:visible         in frame f_ran_01_aprop_ctbl_ap_diario_aux = no
               v_cod_ccusto_fim:visible         in frame f_ran_01_aprop_ctbl_ap_diario_aux = no
               v_cod_plano_ccusto_ini:visible   in frame f_ran_01_aprop_ctbl_ap_diario_aux = no
               v_cod_plano_ccusto_fim_1:visible in frame f_ran_01_aprop_ctbl_ap_diario_aux = no.
    end.


    assign v_cod_cta_ctbl_ini:label in frame f_ran_01_aprop_ctbl_ap_diario_aux = "Conta Ctbl" /*l_conta_ctbl*/ 
           v_cod_cta_ctbl_fim:label in frame f_ran_01_aprop_ctbl_ap_diario_aux = "atÇ" /*l_ate*/ 
           v_cod_plano_cta_ctbl_ini:label in frame f_ran_01_aprop_ctbl_ap_diario_aux = "Plano de Contas" /*l_plano_de_contas*/ 
           v_cod_plano_cta_ctbl_fim:label in frame f_ran_01_aprop_ctbl_ap_diario_aux = "atÇ" /*l_ate*/ .

    if  v_log_funcao_sel_cc
    then do:
        assign v_cod_plano_ccusto_fim_1:label in frame f_ran_01_aprop_ctbl_ap_diario_aux = "atÇ" /*l_ate*/ 
               v_cod_ccusto_ini:label in frame f_ran_01_aprop_ctbl_ap_diario_aux = "Centro de Custo" /*l_centro_de_custo*/ .
    end.                  


    filter_block:
    do on endkey undo filter_block, leave filter_block on error undo filter_block, retry filter_block:

        if  valid-handle(v_wgh_focus)
        then do:
            wait-for go of frame f_ran_01_aprop_ctbl_ap_diario_aux focus v_wgh_focus.
        end.
        else do:
            wait-for go of frame f_ran_01_aprop_ctbl_ap_diario_aux.
        end.

        assign input frame f_ran_01_aprop_ctbl_ap_diario_aux v_cod_plano_cta_ctbl_ini
               input frame f_ran_01_aprop_ctbl_ap_diario_aux v_cod_plano_cta_ctbl_fim
               input frame f_ran_01_aprop_ctbl_ap_diario_aux v_cod_espec_docto_ini
               input frame f_ran_01_aprop_ctbl_ap_diario_aux v_cod_espec_docto_fim
               input frame f_ran_01_aprop_ctbl_ap_diario_aux v_cod_cta_ctbl_ini
               input frame f_ran_01_aprop_ctbl_ap_diario_aux v_cod_cta_ctbl_fim
               input frame f_ran_01_aprop_ctbl_ap_diario_aux v_cod_unid_negoc.

        if v_cod_plano_cta_ctbl_ini > v_cod_plano_cta_ctbl_fim then do:     
            assign v_wgh_focus = v_cod_plano_cta_ctbl_ini:handle in frame f_ran_01_aprop_ctbl_ap_diario_aux.
            /* &1 Inicial deve ser menor ou igual a(o) &1 Final ! */
            run pi_messages (input "show",
                             input 5123,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                "Plano de Conta Cont†bil" /*l_plano_cta_ctbl_1*/)) /*msg_5123*/.
            undo filter_block, retry filter_block.
        end.

        if v_cod_espec_docto_ini > v_cod_espec_docto_fim then do:     
            assign v_wgh_focus = v_cod_espec_docto_ini:handle in frame f_ran_01_aprop_ctbl_ap_diario_aux.
            /* &1 Inicial deve ser menor ou igual a(o) &1 Final ! */
            run pi_messages (input "show",
                             input 5123,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                "EspÇcie" /*l_especie*/)) /*msg_5123*/.
            undo filter_block, retry filter_block.
        end.    

        if v_cod_cta_ctbl_inic_aux > v_cod_cta_ctbl_fim_aux then do:     
            assign v_wgh_focus = v_cod_cta_ctbl_ini:handle in frame f_ran_01_aprop_ctbl_ap_diario_aux.
            /* &1 Inicial deve ser menor ou igual a(o) &1 Final ! */
            run pi_messages (input "show",
                             input 5123,
                             input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                "Conta Cont†bil" /*l_conta_contabil*/)) /*msg_5123*/.
            undo filter_block, retry filter_block.
        end.     

        if v_log_funcao_sel_cc then do:       
            assign input frame f_ran_01_aprop_ctbl_ap_diario_aux v_cod_plano_ccusto_ini
                   input frame f_ran_01_aprop_ctbl_ap_diario_aux v_cod_plano_ccusto_fim_1
                   input frame f_ran_01_aprop_ctbl_ap_diario_aux v_cod_ccusto_ini
                   input frame f_ran_01_aprop_ctbl_ap_diario_aux v_cod_ccusto_fim.  

            if v_cod_plano_ccusto_ini > v_cod_plano_ccusto_fim_1 then do:     
                assign v_wgh_focus = v_cod_plano_ccusto_ini:handle in frame f_ran_01_aprop_ctbl_ap_diario_aux.
                /* &1 Inicial deve ser menor ou igual a(o) &1 Final ! */
                run pi_messages (input "show",
                                 input 5123,
                                 input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                    "Plano de Centro de Custo" /*l_plano_ccusto*/)) /*msg_5123*/.
                undo filter_block, retry filter_block.
            end.   

            if v_cod_ccusto_ini > v_cod_ccusto_fim then do:     
                assign v_wgh_focus = v_cod_ccusto_ini:handle in frame f_ran_01_aprop_ctbl_ap_diario_aux.
                /* &1 Inicial deve ser menor ou igual a(o) &1 Final ! */
                run pi_messages (input "show",
                                 input 5123,
                                 input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                    "Centro de Custo" /*l_centro_de_custo*/)) /*msg_5123*/.
                undo filter_block, retry filter_block.
            end.    
        end.      
    end.

    hide frame f_ran_01_aprop_ctbl_ap_diario_aux.    
END PROCEDURE. /* pi_seleciona_aprop_ctbl_ap_diario_aux */
/*****************************************************************************
** Procedure Interna.....: pi_choose_bt_print_rpt_aprop_ctbl_ap_diario_aux
** Descricao.............: pi_choose_bt_print_rpt_aprop_ctbl_ap_diario_aux
** Criado por............: fut41506
** Criado em.............: 11/02/2008 10:02:43
** Alterado por..........: fut41506
** Alterado em...........: 20/08/2008 15:37:30
*****************************************************************************/
PROCEDURE pi_choose_bt_print_rpt_aprop_ctbl_ap_diario_aux:

    assign input frame f_rpt_41_aprop_ctbl_ap_diario_aux v_cod_empresa
           input frame f_rpt_41_aprop_ctbl_ap_diario_aux v_cod_estab
           input frame f_rpt_41_aprop_ctbl_ap_diario_aux v_dat_inic_diario_aux_apb
           input frame f_rpt_41_aprop_ctbl_ap_diario_aux v_dat_fim_diario_aux_apb
           input frame f_rpt_41_aprop_ctbl_ap_diario_aux v_log_impr_histor
           input frame f_rpt_41_aprop_ctbl_ap_diario_aux v_log_atualiz_numer_pag
           input frame f_rpt_41_aprop_ctbl_ap_diario_aux v_log_emis_termo_encert
           input frame f_rpt_41_aprop_ctbl_ap_diario_aux v_cod_pag_folha
           input frame f_rpt_41_aprop_ctbl_ap_diario_aux v_log_impr_cta_contra_partida.
    assign v_cod_cta_ctbl_inic_aux = ""
           v_cod_cta_ctbl_fim_aux  = "".

    if not v_log_atualiz_numer_pag:sensitive in frame f_rpt_41_aprop_ctbl_ap_diario_aux then
            assign v_log_atualiz_numer_pag = no.
    if not v_log_impr_histor:sensitive in frame f_rpt_41_aprop_ctbl_ap_diario_aux then
            assign v_log_impr_histor = no.
    if not v_log_emis_termo_encert:sensitive in frame f_rpt_41_aprop_ctbl_ap_diario_aux then
            assign v_log_emis_termo_encert = no.

    if v_cod_pag_folha = "Folha" /*l_folha*/  then
        assign v_cod_cabec_label     = 'Folha:'
               v_cod_cabec_label_aux = 'Èltima Folha:'.
    else
        assign v_cod_cabec_label     = 'P†gina:'
               v_cod_cabec_label_aux = 'Èltima P†gina:'.

    run pi_consiste_validacoes_fisco (Input "APB" /*l_apb*/,
                                      Input v_dat_inic_diario_aux_apb,
                                      Input v_dat_fim_diario_aux_apb,
                                      Input v_log_dados_fisco:checked in frame f_rpt_41_aprop_ctbl_ap_diario_aux,
                                      Input v_cod_empresa,
                                      Input v_cod_estab) /*pi_consiste_validacoes_fisco*/.       
    if return-value = 'NOK' then
        return "NOK" /*l_nok*/ .
    if return-value = '14151' then    
    do:
        /* A faixa da datas informada n∆o forma um per°odo completo ! */
        run pi_messages (input "show",
                         input 14151,
                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_14151*/.    
        apply 'entry' to v_dat_inic_diario_aux_apb in frame f_rpt_41_aprop_ctbl_ap_diario_aux.
        return "NOK" /*l_nok*/ .    
    end.

    /* @run(pi_consiste_validacoes_fisco(@%(l_app), v_dat_inic_diario_aux_apb, v_dat_fim_diario_aux_apb, v_log_dados_fisco:checked in frame @&(frame), v_cod_empresa, v_cod_estab)).       
    if return-value = 'NOK' then
        return @%(l_nok).
    if return-value = '14151' then    
    do:
        @cx_message(14151).    
        apply 'entry' to v_dat_inic_diario_aux_apb in frame @&(frame).
        return @%(l_nok).    
    end.
    */

    do v_num_cont = 1 to length(v_cod_cta_ctbl_ini):
        if  substring(v_cod_cta_ctbl_ini, v_num_cont, 1) <> "." 
        and substring(v_cod_cta_ctbl_ini, v_num_cont, 1) <> "-" then
            assign v_cod_cta_ctbl_inic_aux = v_cod_cta_ctbl_inic_aux + substring(v_cod_cta_ctbl_ini, v_num_cont, 1).
    end /* do verifica_block */.
    do v_num_cont = 1 to length(v_cod_cta_ctbl_fim):
        if  substring(v_cod_cta_ctbl_fim, v_num_cont, 1) <> "." 
        and substring(v_cod_cta_ctbl_fim, v_num_cont, 1) <> "-" then
            assign v_cod_cta_ctbl_fim_aux = v_cod_cta_ctbl_fim_aux + substring(v_cod_cta_ctbl_fim, v_num_cont, 1).
    end /* do verifica_block */.

    if  v_dat_inic_diario_aux_apb > v_dat_fim_diario_aux_apb
    then do:
        /* In°cio Per°odo maior que Fim Per°odo ! */
        run pi_messages (input "show",
                         input 1371,
                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_1371*/.
        return "NOK" /*l_nok*/ .
    end /* if */.

    if  v_cod_estab:visible   in frame f_rpt_41_aprop_ctbl_ap_diario_aux 
    and v_cod_estab:sensitive in frame f_rpt_41_aprop_ctbl_ap_diario_aux then

        run pi_vld_aprop_ctbl_ap_diario_aux (Input yes) /*pi_vld_aprop_ctbl_ap_diario_aux*/.
    else
        run pi_vld_aprop_ctbl_ap_diario_aux (Input no) /*pi_vld_aprop_ctbl_ap_diario_aux*/.

    assign v_log_print = no.

    if  v_log_reemis = yes
    then do:
        run pi_messages (input "show",
                         input 2446,
                         input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")).
        assign v_log_answer = (if   return-value = "yes" then yes
                               else if return-value = "no" then no
                               else ?) /*msg_2446*/.
        if  v_log_answer = no
        then do:
            return "NOK" /*l_nok*/ .
        end /* if */.
    end /* if */.

do:
/* tech38629 - Alteraá∆o efetuada via filtro */
&if '{&emsbas_version}':U >= '5.05':U &then
    run pi_restricoes in v_prog_filtro_pdf (input rs_cod_dwb_output:screen-value in frame f_rpt_41_aprop_ctbl_ap_diario_aux).
    if return-value = 'nok' then 
        return no-apply.
&endif
/* tech38629 - Fim da alteraá∆o */
    assign v_log_print = yes.
end.

    return "OK" /*l_ok*/ .
END PROCEDURE. /* pi_choose_bt_print_rpt_aprop_ctbl_ap_diario_aux */
/*****************************************************************************
** Procedure Interna.....: pi_tratar_tt_rpt_diario_auxiliar_apb_chile
** Descricao.............: pi_tratar_tt_rpt_diario_auxiliar_apb_chile
** Criado por............: fut41420
** Criado em.............: 07/05/2008 09:13:23
** Alterado por..........: fut43120
** Alterado em...........: 13/01/2009 16:20:47
*****************************************************************************/
PROCEDURE pi_tratar_tt_rpt_diario_auxiliar_apb_chile:

    if aprop_ctbl_ap.ind_natur_lancto_ctbl = "DB" /*l_db*/   
        then do:
        run prgint/ufn/ufn201aa.py(input v_cod_finalid_econ,                                                                                              
                                  input tt_rpt_diario_auxiliar_apb.tta_dat_transacao,                                                                    
                                  input tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_db,                                                                
                                  input tt_rpt_diario_auxiliar_apb.tta_ind_natur_lancto_ctbl,                                                            
                                  input tt_rpt_diario_auxiliar_apb.tta_cod_plano_cta_ctbl,
                                  input tt_rpt_diario_auxiliar_apb.tta_cod_estab,                                                               
                                  output v_cod_cta_ajust_dec_chi_return,                                                                                 
                                  output v_ind_natur_lancto_ctbl_return,                                                                                 
                                  output v_val_ajust_dec_chi_return,                                                                                     
                                  output v_val_movto_return).                         
    end.
    else do:
        run prgint/ufn/ufn201aa.py(input v_cod_finalid_econ,                                                                                              
                                  input tt_rpt_diario_auxiliar_apb.tta_dat_transacao,                                                                    
                                  input tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_cr,                                                                
                                  input tt_rpt_diario_auxiliar_apb.tta_ind_natur_lancto_ctbl,                                                            
                                  input tt_rpt_diario_auxiliar_apb.tta_cod_plano_cta_ctbl,
                                  input tt_rpt_diario_auxiliar_apb.tta_cod_estab,                                                               
                                  output v_cod_cta_ajust_dec_chi_return,                                                                                 
                                  output v_ind_natur_lancto_ctbl_return,                                                                                 
                                  output v_val_ajust_dec_chi_return,                                                                                     
                                  output v_val_movto_return).                           
    end.                                                                                            

    if v_val_ajust_dec_chi_return <> 0 then do:  

        run pi_tratar_dec_ajust_chile (input tt_rpt_diario_auxiliar_apb.tta_cod_plano_cta_ctbl,
                                       input tt_rpt_diario_auxiliar_apb.tta_cod_estab).
        if aprop_ctbl_ap.ind_natur_lancto_ctbl = "DB" /*l_db*/   then          
            assign tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_db = v_val_movto_return.
        else 
            assign tt_rpt_diario_auxiliar_apb.ttv_val_aprop_ctbl_cr = v_val_movto_return.

        /* contra partida - na conta de decimais chile*/                                                                                  
        create tt_rpt_diario_auxiliar_apb_dec.                                                                                            
        assign tt_rpt_diario_auxiliar_apb_dec.tta_ind_natur_lancto_ctbl = if v_ind_natur_lancto_ctbl_return =  "DB" /*l_db*/  then "CR" /*l_cr*/  else "DB" /*l_db*/      
               tt_rpt_diario_auxiliar_apb_dec.tta_cod_cta_ctbl          = v_cod_cta_ajust_dec_chi_return                                          
               tt_rpt_diario_auxiliar_apb_dec.tta_cod_ccusto            = ""                                                              
               tt_rpt_diario_auxiliar_apb_dec.tta_cod_plano_ccusto      = ""                                                              
               tt_rpt_diario_auxiliar_apb_dec.ttv_rec_movto_tit_ap      = tt_rpt_diario_auxiliar_apb.ttv_rec_movto_tit_ap                
               tt_rpt_diario_auxiliar_apb_dec.ttv_rec_tit_ap            = tt_rpt_diario_auxiliar_apb.ttv_rec_tit_ap                      
               tt_rpt_diario_auxiliar_apb_dec.tta_cod_estab             = tt_rpt_diario_auxiliar_apb.tta_cod_estab                                                    
               tt_rpt_diario_auxiliar_apb_dec.tta_dat_transacao         = tt_rpt_diario_auxiliar_apb.tta_dat_transacao
               tt_rpt_diario_auxiliar_apb_dec.ttv_rec_aprop_ctbl_ap     = tt_rpt_diario_auxiliar_apb.ttv_rec_aprop_ctbl_ap     
               tt_rpt_diario_auxiliar_apb_dec.tta_cod_plano_cta_ctbl    = tt_rpt_diario_auxiliar_apb.tta_cod_plano_cta_ctbl 
               tt_rpt_diario_auxiliar_apb_dec.ttv_ind_trans_ap_abrev    = tt_rpt_diario_auxiliar_apb.ttv_ind_trans_ap_abrev
               tt_rpt_diario_auxiliar_apb_dec.tta_cod_refer             = tt_rpt_diario_auxiliar_apb.tta_cod_refer
               tt_rpt_diario_auxiliar_apb_dec.tta_cdn_fornecedor        = tt_rpt_diario_auxiliar_apb.tta_cdn_fornecedor 
               tt_rpt_diario_auxiliar_apb_dec.tta_nom_abrev             = tt_rpt_diario_auxiliar_apb.tta_nom_abrev
               tt_rpt_diario_auxiliar_apb_dec.tta_cod_portador          = tt_rpt_diario_auxiliar_apb.tta_cod_portador
               tt_rpt_diario_auxiliar_apb_dec.tta_num_cheque            = tt_rpt_diario_auxiliar_apb.tta_num_cheque
               tt_rpt_diario_auxiliar_apb_dec.tta_num_bord_ap           = tt_rpt_diario_auxiliar_apb.tta_num_bord_ap
               tt_rpt_diario_auxiliar_apb_dec.tta_cod_unid_negoc        = tt_rpt_diario_auxiliar_apb.tta_cod_unid_negoc
               tt_rpt_diario_auxiliar_apb_dec.tta_des_unid_negoc        = tt_rpt_diario_auxiliar_apb.tta_des_unid_negoc
               tt_rpt_diario_auxiliar_apb_dec.ttv_cod_cta_ctbl_contra   = string(tt_rpt_diario_auxiliar_apb.ttv_cod_cta_ctbl_contra, plano_cta_ctbl.cod_format_cta_ctbl). 

        if v_ind_natur_lancto_ctbl_return = "DB" /*l_db*/   then
            tt_rpt_diario_auxiliar_apb_dec.ttv_val_aprop_ctbl_cr     = v_val_ajust_dec_chi_return.
        else
            tt_rpt_diario_auxiliar_apb_dec.ttv_val_aprop_ctbl_db     = v_val_ajust_dec_chi_return. 

       create tt_rpt_diario_auxiliar_apb.
       buffer-copy tt_rpt_diario_auxiliar_apb_dec to tt_rpt_diario_auxiliar_apb.             
    end.
END PROCEDURE. /* pi_tratar_tt_rpt_diario_auxiliar_apb_chile */
/*****************************************************************************
** Procedure Interna.....: pi_trata_erros_diario_aux_chile
** Descricao.............: pi_trata_erros_diario_aux_chile
** Criado por............: fut41420
** Criado em.............: 15/05/2008 18:08:38
** Alterado por..........: fut41420
** Alterado em...........: 09/06/2008 17:20:31
*****************************************************************************/
PROCEDURE pi_trata_erros_diario_aux_chile:

    if  can-find(first tt_log_erros) then do:
        if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
            page stream s_1.
        put stream s_1 unformatted 
            "Erro" to 8
            "Inconsistància" at 10
            "Ajuda" at 86 skip
            "--------" to 8
            "---------------------------------------------------------------------------" at 10
            "--------------------------------------------------" at 86 skip.    
        view stream s_1 frame f_rpt_s_1_Grp_erro_chi_Lay_erro_ajust_c.
        for each tt_log_erros
            break by
            tt_log_erros.ttv_des_erro:
            if  first-of(tt_log_erros.ttv_des_erro)
            then do:
                run pi_print_editor ("s_1", tt_log_erros.ttv_des_ajuda, "     050", "", "     ", "", "     ").
                put stream s_1 unformatted 
                    tt_log_erros.ttv_num_cod_erro to 8 format ">>>>,>>9"
                    tt_log_erros.ttv_des_erro at 10 format "x(75)"
                    entry(1, return-value, chr(255)) at 86 format "x(50)" skip.
                run pi_print_editor ("s_1", tt_log_erros.ttv_des_ajuda, "at086050", "", "", "", "").
            end /* if */.    
        end.
        hide stream s_1 frame f_rpt_s_1_Grp_erro_chi_Lay_erro_ajust_c.
        page stream s_1.
    end.    

END PROCEDURE. /* pi_trata_erros_diario_aux_chile */
/*****************************************************************************
** Procedure Interna.....: pi_tratar_dec_ajust_chile
** Descricao.............: pi_tratar_dec_ajust_chile
** Criado por............: fut41420
** Criado em.............: 15/05/2008 18:05:19
** Alterado por..........: fut41420
** Alterado em...........: 06/06/2008 08:53:22
*****************************************************************************/
PROCEDURE pi_tratar_dec_ajust_chile:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_plano_cta_ctbl
        as character
        format "x(8)"
        no-undo.
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


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_num_seq_chi
        as integer
        format ">>>>,>>9":U
        label "SeqÅància"
        column-label "Seq"
        no-undo.


    /************************** Variable Definition End *************************/

    find first cta_ctbl
        where cta_ctbl.cod_cta_ctbl = v_cod_cta_ajust_dec_chi_return
          and cta_ctbl.cod_plano_cta_ctbl = p_cod_plano_cta_ctbl no-lock no-error.
    if not avail cta_ctbl then do:
    assign v_num_seq_chi = 0.
           find last tt_log_erros_chi no-lock no-error.
           if avail tt_log_erros_chi then 
              assign v_num_seq_chi = tt_log_erros_chi.ttv_num_seq + 1.
           else 
              assign v_num_seq_chi = 1.

           create tt_log_erros_chi. 
           assign tt_log_erros_chi.ttv_num_seq   = v_num_seq_chi
               tt_log_erros_chi.ttv_num_cod_erro = 19268
               tt_log_erros_chi.ttv_des_erro     = substitute("Conta Ajuste Dec inv†lida Plano &1 !" /*19268*/, p_cod_plano_cta_ctbl) 
               tt_log_erros_chi.ttv_des_ajuda    = substitute("A conta &1 utilizada para Ajustes Decimais, n∆o Ç uma conta v†lida relacionada ao plano de contas &2. Acesse a Manutená∆o de Planos de Contas e informe uma conta v†lida para Ajuste Decimais. " /*19268*/, v_cod_cta_ajust_dec_chi_return, p_cod_plano_cta_ctbl).
    end.
    else do:
        if cta_ctbl.log_cta_ctbl_exclus_analit = no then do:
            find last tt_log_erros_chi no-lock no-error.
            if avail tt_log_erros_chi then 
                assign v_num_seq_chi = tt_log_erros_chi.ttv_num_seq + 1.
            else 
                assign v_num_seq_chi = 1.
                create tt_log_erros_chi. 
                assign tt_log_erros_chi.ttv_num_seq  = v_num_seq_chi
                       tt_log_erros_chi.ttv_num_cod_erro = 19269
                       tt_log_erros_chi.ttv_des_erro     = substitute("Conta Ajuste Dec deve ser Anal°tica Exclusiva Plano &1 !" /*19269*/, p_cod_plano_cta_ctbl) 
                       tt_log_erros_chi.ttv_des_ajuda    = substitute("Dever† ser informada uma Conta Ajuste Dec relacionada ao plano &1 que seja Anal°tica Exclusiva. Acesse a Manutená∆o de Planos de Contas e informe uma conta v†lida para Ajuste Decimais." /*19269*/, p_cod_plano_cta_ctbl).
        end.                                       
        find first criter_distrib_cta_ctbl                                                       
             where criter_distrib_cta_ctbl.cod_plano_cta_ctbl = p_cod_plano_cta_ctbl
               and criter_distrib_cta_ctbl.cod_estab          = p_cod_estab      
               and criter_distrib_cta_ctbl.cod_cta_ctbl       = v_cod_cta_ajust_dec_chi_return no-lock no-error.
        if avail criter_distrib_cta_ctbl then do:
            if criter_distrib_cta_ctbl.ind_criter_distrib_ccusto = "Definidos" /*l_definidos*/   
              or criter_distrib_cta_ctbl.ind_criter_distrib_ccusto = "Utiliza Todos" /*l_utiliza_todos*/  then do:
              find last tt_log_erros_chi no-lock no-error.
                  if avail tt_log_erros_chi then 
                     assign v_num_seq_chi = tt_log_erros_chi.ttv_num_seq + 1.
                  else 
                     assign v_num_seq_chi = 1.

                  create tt_log_erros_chi. 
                  assign tt_log_erros_chi.ttv_num_seq   = v_num_seq_chi
                         tt_log_erros_chi.ttv_num_cod_erro = 19270
                         tt_log_erros_chi.ttv_des_erro     = substitute("Conta Ajuste Dec &1 n∆o pode ter critÇrio de distribuiá∆o !" /*19270*/, p_cod_plano_cta_ctbl) 
                         tt_log_erros_chi.ttv_des_ajuda    = substitute("A conta Ajuste Decimais &1 do plano &2 n∆o poder† utilizar critÇrio de distribuiá∆o por centro de custo. Acesse a Manutená∆o de Planos de Contas e informe uma conta v†lida para Ajuste Decimais." /*19270*/, v_cod_cta_ajust_dec_chi_return, p_cod_plano_cta_ctbl).
            end.
        end.
    end.


END PROCEDURE. /* pi_tratar_dec_ajust_chile */
/*****************************************************************************
** Procedure Interna.....: pi_trata_erros_diario_aux_apb
** Descricao.............: pi_trata_erros_diario_aux_apb
** Criado por............: fut41420
** Criado em.............: 05/06/2008 14:59:04
** Alterado por..........: fut41420
** Alterado em...........: 09/06/2008 11:17:57
*****************************************************************************/
PROCEDURE pi_trata_erros_diario_aux_apb:

    /************************* Variable Definition Begin ************************/

    def var v_des_help_erro_diario_aux
        as character
        format "x(100)":U
        no-undo.
    def var v_des_msg
        as character
        format "x(40)":U
        no-undo.


    /************************** Variable Definition End *************************/

    /* fut36887 - atividade 139621 - tratamento de erros*/    
    if  CAN-FIND(FIRST tt_log_erros)
    then do:
        assign v_des_msg = "DADOS PARA ARQUIVOS FISCALIZAÄ«O." /*14298*/
               v_des_help_erro_diario_aux = "Os dados para arquivos de fiscalizaá∆o n∆o foram gerados devido aos erros listados abaixo:" /*14298*/.
        if (line-counter(s_1) + 5) > v_rpt_s_1_bottom then
            page stream s_1.
        put stream s_1 unformatted 
            '********************************' at 1 skip
            v_des_msg at 1 format "x(40)" skip
            '********************************' at 1 skip
            v_des_help_erro_diario_aux at 1 format "x(100)" skip (1).
        if (line-counter(s_1) + 3) > v_rpt_s_1_bottom then
            page stream s_1.
        put stream s_1 unformatted 
            "Erro" to 8
            "Inconsistància" at 10
            "Ajuda" at 86 skip
            "--------" to 8
            "---------------------------------------------------------------------------" at 10
            "--------------------------------------------------" at 86 skip.    
        view stream s_1 frame f_rpt_s_1_Grp_erro_chi_Lay_erro_ajust_c.
        FOR EACH 
            tt_log_erros
        break by
            tt_log_erros.ttv_des_erro:
            if  first-of(tt_log_erros.ttv_des_erro)
            then do:
                run pi_print_editor ("s_1", tt_log_erros.ttv_des_ajuda, "     050", "", "     ", "", "     ").
                put stream s_1 unformatted 
                    tt_log_erros.ttv_num_cod_erro to 8 format ">>>>,>>9"
                    tt_log_erros.ttv_des_erro at 10 format "x(75)"
                    entry(1, return-value, chr(255)) at 86 format "x(50)" skip.
                run pi_print_editor ("s_1", tt_log_erros.ttv_des_ajuda, "at086050", "", "", "", "").
            end /* if */.    
        END.
        hide stream s_1 frame f_rpt_s_1_Grp_erro_chi_Lay_erro_ajust_c.
        page stream s_1.
    end /* if */.
END PROCEDURE. /* pi_trata_erros_diario_aux_apb */
/*****************************************************************************
** Procedure Interna.....: pi_tratar_tt_movimentos_chile_apb
** Descricao.............: pi_tratar_tt_movimentos_chile_apb
** Criado por............: fut41420
** Criado em.............: 05/06/2008 15:15:48
** Alterado por..........: fut41506
** Alterado em...........: 28/08/2008 16:37:22
*****************************************************************************/
PROCEDURE pi_tratar_tt_movimentos_chile_apb:

    run prgint/ufn/ufn201aa.py(input v_cod_finalid_econ,                                                                                                  
                               input tt_movimentos.ttv_dat_trans_diario,                                                                                  
                               input tt_movimentos.ttv_val_movto,                                                                                         
                               input tt_movimentos.tta_ind_natur_lancto_ctbl,                                                                             
                               input tt_movimentos.tta_cod_plano_cta_ctbl,
                               input tt_movimentos.tta_cod_estab,                                                                                
                               output v_cod_cta_ajust_dec_chi_return,                                                                                     
                               output v_ind_natur_lancto_ctbl_return,                                                                                     
                               output v_val_ajust_dec_chi_return,                                                                                         
                               output v_val_movto_return).                                                                                                

    if v_val_ajust_dec_chi_return <> 0 
    then do:

        run pi_tratar_dec_ajust_chile (input tt_movimentos.tta_cod_plano_cta_ctbl,
                                       input tt_movimentos.tta_cod_estab). 

        assign tt_movimentos.ttv_val_movto = v_val_movto_return.

        /* contra partida - na conta de decimais chile*/                                                                                      
        create tt_movimentos_dec.                                                                                                             
        assign tt_movimentos_dec.tta_ind_natur_lancto_ctbl = if v_ind_natur_lancto_ctbl_return = "DB" /*l_db*/  then "CR" /*l_cr*/  else "DB" /*l_db*/     
               tt_movimentos_dec.tta_cod_cta_ctbl          = v_cod_cta_ajust_dec_chi_return                                   
               tt_movimentos_dec.tta_cod_plano_ccusto      = ""                                                               
               tt_movimentos_dec.tta_cod_ccusto            = ""                                                               
               tt_movimentos_dec.ttv_val_movto             = v_val_ajust_dec_chi_return                                               
               tt_movimentos_dec.ttv_dat_trans_diario      = tt_movimentos.ttv_dat_trans_diario                               
               tt_movimentos_dec.tta_cod_finalid_econ      = tt_movimentos.tta_cod_finalid_econ                               
               tt_movimentos_dec.tta_cod_plano_cta_ctbl    = tt_movimentos.tta_cod_plano_cta_ctbl                            
               tt_movimentos_dec.tta_cod_empresa           = tt_movimentos.tta_cod_empresa                                       
               tt_movimentos_dec.tta_cod_estab             = tt_movimentos.tta_cod_estab                                         
               tt_movimentos_dec.tta_cod_unid_negoc        = tt_movimentos.tta_cod_unid_negoc                                    
               tt_movimentos_dec.ttv_cod_arq_ems2          = tt_movimentos.ttv_cod_arq_ems2                                              
               tt_movimentos_dec.ttv_cdn_clien_fornec      = tt_movimentos.ttv_cdn_clien_fornec                                         
               tt_movimentos_dec.ttv_cod_lancto_ctbl       = tt_movimentos.ttv_cod_lancto_ctbl
               tt_movimentos_dec.ttv_des_historico         = tt_movimentos.ttv_des_historico
               tt_movimentos_dec.ttv_cod_cta_ctbl_contra   = tt_movimentos.ttv_cod_cta_ctbl_contra.

        create tt_movimentos.
        buffer-copy tt_movimentos_dec to tt_movimentos.                                                                                                                                       

    end.
END PROCEDURE. /* pi_tratar_tt_movimentos_chile_apb */


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
/*******************  End of rpt_aprop_ctbl_ap_diario_aux *******************/

/****************************************************************************************** 
** 	   Programa: prgint/edf/edf900zi-237.py
**   	  Autor: Daniela Campos - DPC
** 	 Fornecedor: DKP
**         Data: 27/06/2018
** Change/Chamado: 
**      Objetivo: Envio pagamento escritural Bradesco - InclusÆo de tratativas para envio de impostos
**
******************************** CONTROLE DE ALTERA€åES **********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: N/A
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: Mapa do EDI
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

/* Variaveis projeto Tesouraria - DPC */
def var v_cod_segto       as char no-undo.
def var v_cdn_forma_pagto as int  no-undo.
def var v_cod_sacado      as char no-undo.
def var v_nome_sacado     as char no-undo.
def var v_cod_cedente     as char no-undo.
def var v_nome_cedente    as char no-undo.
def var v_cod_favorecido  as char no-undo.
def var v_nome_favorecido as char no-undo.
def var v_tip_inscr       as char no-undo.
def var v_tip_inscr_ced   as char no-undo.
def var v_num_seq_j52     as int  no-undo.
def var v_inf_compl_trib  as char no-undo.
def var v_cod_receita     as char no-undo.
def var v_id_contrib      as char no-undo.
def var v_cod_id_contrib  as char no-undo.
def var v_dt_vencto-aux   as char no-undo.
def var v_vl_princ        as char no-undo.
def var v_vl_multa        as char no-undo.
def var v_juros_encargos  as char no-undo.
def var v_dt_vencto       as char no-undo.
def var c-aux             as char no-undo.
def var v_vl_tributo      as char  no-undo.
def var v_vl_out_entid    as char  no-undo.
def var v_vl_corr_monet        as char no-undo.
def var v_dt_periodo           AS char NO-UNDO.
def var v_vl_receita_bruta     as char no-undo.
def var v_percentual           as char no-undo.
def var v_cod_barra_O          as char no-undo.
def var v_nom_conces           as char no-undo.
def var v_compl_tp_servico     as char no-undo.
def var v_cod_finalid_ted      as char no-undo.
def var v_compl_finalid_pagto  as char no-undo.
def var v_nom_contrib          as char no-undo.
def var v_dt_pagto             as char no-undo.
def var v_val_pagto            as dec  no-undo.
/*def var v_inf_compl_trib       as char no-undo.*/
def var v_cod_id_feder_favorec as char no-undo. 
def var v_num_tip_id_feder     as int  no-undo. 
def var v_forma_pagto          as char no-undo.
def var v_vl_tot_pago          as char no-undo. 
def var v_des_conteudo         as char no-undo.
def var data-faturamento-aux   as date no-undo.

/*-- Filtro Multi-idioma Aplicado --*/

def var c-versao-prg as char initial " 1.00.00.031":U no-undo.
def var c-versao-rcode as char initial "[[[1.00.00.031[[[":U no-undo. /* Controle de Versao R-CODE - Nao retirar do Fonte */

{include/i_dbinst.i}
{include/i_dbtype.i}

{include/i_fcldef.i}

&IF "{&EMSFND_VERSION}" >= "1.00"
&THEN
{include/i-license-manager.i  fnc_extracao_banco_brasil EDF}
&ENDIF



/******************************* Private-Data *******************************/
assign this-procedure:private-data = "HLP=0":U.
/*************************************  *************************************/

/********************* Temporary Table Definition Begin *********************/

def temp-table tt_param_program_formul no-undo
    field tta_cdn_segment_edi              as Integer format ">>>>>9" initial 0 label "Segmento" column-label "Segmento"
    field tta_cdn_element_edi              as Integer format ">>>>>9" initial 0 label "Elemento" column-label "Elemento"
    field tta_des_label_utiliz_formul_edi  as character format "x(10)" label "Label Utiliz Formula" column-label "Label Utiliz Formula"
    field ttv_des_contdo                   as character format "x(100)" label "Conteudo" column-label "Conteudo"
    index tt_param_program_formul_id       is primary
          tta_cdn_segment_edi              ascending
          tta_cdn_element_edi              ascending.

def shared temp-table tt_segment_tot no-undo
    field tta_cdn_segment_edi              as Integer format ">>>>>9" initial 0 label "Segmento" column-label "Segmento"
    field ttv_qtd_proces_edi               as decimal format "->>>>,>>9.9999" decimals 4
    field ttv_qtd_bloco_docto              as decimal format "99999"
    field ttv_log_trailler_edi             as logical format "Sim/NÆo" initial no label "Trailler" column-label "Trailler"
    field ttv_log_header_edi               as logical format "Sim/NÆo" initial no label "Header" column-label "Header"
    .



/********************** Temporary Table Definition End **********************/

/************************ Parameter Definition Begin ************************/

def Input param p_cdn_mapa_edi
    as Integer
    format ">>>>>9"
    no-undo.
def Input param p_cdn_segment_edi
    as Integer
    format ">>>>>9"
    no-undo.
def Input param p_cdn_element_edi
    as Integer
    format ">>>>>9"
    no-undo.
def Input param table 
    for tt_param_program_formul.


/************************* Parameter Definition End *************************/

/************************* Variable Definition Begin ************************/

def var v_cdn_contador
    as Integer
    format ">>>,>>9":U
    no-undo.
def var v_cdn_espaco
    as Integer
    format ">>>,>>9":U
    no-undo.
def var v_cdn_inicial
    as Integer
    format ">>>9":U
    label "Inicial"
    column-label "Inicial"
    no-undo.
def var v_cdn_tip_forma_pagto
    as Integer
    format ">>9":U
    no-undo.
def var v_cod_agenc_bcia
    as character
    format "x(10)":U
    label "Agˆncia Banc ria"
    column-label "Agˆncia Banc ria"
    no-undo.
def var v_cod_agenc_bcia_fav
    as character
    format "x(10)":U
    label "Agˆncia Banc ria"
    column-label "Agˆncia Banc ria"
    no-undo.
def var v_cod_agenc_favorec_1
    as character
    format "x(8)":U
    no-undo.
def new global shared var v_cod_aplicat_dtsul_corren
    as character
    format "x(3)":U
    no-undo.
def var v_cod_banco
    as character
    format "x(8)":U
    label "Banco"
    column-label "Banco"
    no-undo.
def var v_cod_barra
    as character
    format "x(44)":U
    no-undo.
def var v_cod_barra_2
    as character
    format "99999.999999":U
    no-undo.
def var v_cod_campo_ctro
    as character
    format "999":U
    no-undo.
def new global shared var v_cod_ccusto_corren
    as character
    format "x(11)":U
    label "Centro Custo"
    column-label "Centro Custo"
    no-undo.
def var v_cod_cta_corren
    as character
    format "x(10)":U
    label "Conta Corrente"
    column-label "Conta Corrente"
    no-undo.
def var v_cod_cta_corren_fav
    as character
    format "x(10)":U
    label "Conta Corrente"
    column-label "Cta Corrente"
    no-undo.
def var v_cod_det
    as character
    format "x(308)":U
    no-undo.
def var v_cod_digito_agenc_bcia
    as character
    format "x(8)":U
    no-undo.
def var v_cod_digito_agenc_bcia_fav
    as character
    format "x(2)":U
    label "D¡gito Ag Bcia"
    column-label "Dig Ag"
    no-undo.
def var v_cod_digito_agenc_cta_corren
    as character
    format "x(2)":U
    label "D¡gito Agˆncia + Cta"
    column-label "D¡g Agˆncia + Cta"
    no-undo.
def var v_cod_digito_cta_corren
    as character
    format "x(2)":U
    label "D¡gito Cta Corrente"
    column-label "D¡gito Cta Corrente"
    no-undo.
def var v_cod_digito_cta_corren_fav
    as character
    format "x(2)":U
    label "D¡gito Cta Corrente"
    column-label "D¡gito Cta Corrente"
    no-undo.
def new global shared var v_cod_dwb_user
    as character
    format "x(21)":U
    label "Usu rio"
    column-label "Usu rio"
    no-undo.
def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.
def var v_cod_espaco_branco
    as character
    format "x(1)":U
    no-undo.
def new global shared var v_cod_estab_usuar
    as character
    format "x(3)":U
    label "Estabelecimento"
    column-label "Estab"
    no-undo.
def var v_cod_forma
    as character
    format "x(3)":U
    no-undo.
def new global shared var v_cod_funcao_negoc_empres
    as character
    format "x(50)":U
    no-undo.
def var v_cod_gravac
    as character
    format "x(8)":U
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
def var v_cod_inicial
    as character
    format "x(9)":U
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
def var v_cod_ocor_bcia
    as character
    format "x(30)":U
    label "Ocorrˆncia Bcia"
    column-label "Ocorrˆncia Bcia"
    no-undo.
def var v_cod_pagto
    as character
    format "x(16)":U
    no-undo.
def var v_cod_pagto_ocor
    as character
    format "x(7)":U
    no-undo.
def var v_cod_pagto_remes
    as character
    format "x(6)":U
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
def var v_cod_sist_nac_bcio_fav
    as character
    format "x(8)":U
    label "C¢digo Sist Banc rio"
    column-label "C¢digo Sist Banc rio"
    no-undo.
def var v_cod_tip_reg_boleto
    as character
    format "x(1)":U
    initial "2"
    no-undo.
def var v_cod_tit
    as character
    format "x(18)":U
    no-undo.
def var v_cod_titulo
    as character
    format "x(8)":U
    label "Titulo"
    column-label "Titulo"
    no-undo.
def var v_cod_tit_ap
    as character
    format "x(10)":U
    label "T¡tulo Ap"
    column-label "T¡tulo Ap"
    no-undo.

def var v_cod_tit_ap_2
    as character
    format "x(10)":U
    label "T¡tulo Ap"
    column-label "T¡tulo Ap"
    no-undo.

def var v_cod_tit_ap_bco
    as character
    format "x(20)":U
    label "T¡tulo  Banco"
    column-label "T¡tulo Banco"
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
def var v_cod_vencto
    as character
    format "x(8)":U
    no-undo.
def var v_dat_gravac
    as date
    format "99/99/9999":U
    no-undo.
def var v_ind_tip_instruc
    as character
    format "X(15)":U
    no-undo.
def var v_nom_empresa
    as character
    format "x(40)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.
def var v_nom_favorec
    as character
    format "x(30)":U
    no-undo.
def var v_nom_title_aux
    as character
    format "x(60)":U
    no-undo.
def var v_num_bco
    as char
    format ">>>>,>>9":U
    no-undo.
def new global shared var v_num_cont
    as integer
    format ">,>>9":U
    initial 0
    no-undo.
def new global shared var v_num_contador
    as integer
    format ">>>>,>>9":U
    initial 0
    no-undo.
def var v_num_control_apres
    as integer
    format ">>>>,>>9":U
    no-undo.
def new global shared var v_num_cont_val
    as integer
    format ">>>>,>>9":U
    initial 0
    no-undo.
def var v_num_convenio
    as integer
    format "999999":U
    no-undo.
def new global shared var v_num_ped_exec_corren
    as integer
    format ">>>>9":U
    no-undo.
def var v_num_seq
    as integer
    format ">>>,>>9":U
    label "Seq?ˆncia"
    column-label "Seq"
    no-undo.
def var v_num_seq_header
    as integer
    format ">>>>>":U
    no-undo.
def var v_rec_log
    as recid
    format ">>>>>>9":U
    no-undo.
def var v_val_abat
    as decimal
    format "->>>,>>>,>>9.99":U
    decimals 2
    label "Valor Abatimento"
    column-label "Valor Abatimento"
    no-undo.
def var v_val_acresc
    as decimal
    format "->>>,>>>,>>9.99":U
    decimals 2
    label "Valor Acr‚scimo"
    column-label "Valor Acr‚scimo"
    no-undo.
def var v_val_cgc_cpf
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    no-undo.
def var v_val_correc
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Valor Corre‡Æo"
    column-label "Valor Corre‡Æo"
    no-undo.
def var v_val_desconto
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Desconto"
    column-label "Desconto"
    no-undo.
def var v_val_inicial
    as decimal
    format "->>,>>>,>>>,>>9.999":U
    decimals 3
    label "Valor Inicial"
    column-label "Valor Inicial"
    no-undo.
def var v_val_juros
    as decimal
    format "->>>,>>>,>>9.99":U
    decimals 2
    label "Valor Juros"
    column-label "Valor Juros"
    no-undo.
def var v_val_multa
    as decimal
    format "->>>,>>>,>>9.99":U
    decimals 2
    label "Vl Multa"
    column-label "Vl Multa"
    no-undo.

/*
def var v_val_pagto
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Valor Pagamento"
    column-label "Valor Pagamento"
    no-undo.*/
    

def var v_val_titulo
    as decimal
    format ">>>,>>>,>>9.99999":U
    decimals 5
    label "Valor"
    column-label "Valor"
    no-undo.
def var v_val_tot_desc_abat
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Descto/Abat"
    column-label "Descto/Abat"
    no-undo.
def var v_num_servico as integer no-undo. /*local*/
/************************** Variable Definition End *************************/


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

def stream s-arq.

if  v_cod_arq <> '' and v_cod_arq <> ?
then do:
    run pi_version_extract ('fnc_extracao_banco_brasil':U, 'prgint/edf/edf900zi.py':U, '1.00.00.031':U, 'pro':U).
end /* if */.



/* End_Include: i_version_extract */


/* -------------- DETALHE ---------------*/
run pi_extracao_banco_brasil_detalhe /*pi_extracao_banco_brasil_detalhe*/.
/* --------------------------------------*/

return v_cod_det.


/******************************* Main Code End ******************************/

/************************* Internal Procedure Begin *************************/

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
** Procedure Interna.....: pi_retorna_cod_barra_leitora
** Descricao.............: pi_retorna_cod_barra_leitora
** Criado por............: bre17191
** Criado em.............: 15/06/2000 10:07:42
** Alterado por..........: bre18732
** Alterado em...........: 19/03/2002 11:06:36
*****************************************************************************/
PROCEDURE pi_retorna_cod_barra_leitora:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_barra_2
        as character
        format "99999.999999"
        no-undo.
    def output param p_cod_barra
        as character
        format "x(44)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_num_tam_format
        as integer
        format ">>9":U
        no-undo.
    def var v_val_tit_barra
        as decimal
        format "->>,>>>,>>>,>>9.99":U
        decimals 2
        no-undo.


    /************************** Variable Definition End *************************/

    assign v_val_tit_barra = 0.

    &if defined(BF_FIN_ALTER_CODIGO_BARRA) &then
        assign v_num_tam_format = 14.
    &else
        find ems5.histor_exec_especial no-lock
             where histor_exec_especial.cod_modul_dtsul = 'UFN'
               and histor_exec_especial.cod_prog_dtsul  = 'SPP_alter_codigo_barra'
             no-error.
        if   avail histor_exec_especial then
             assign v_num_tam_format = 14.
        else assign v_num_tam_format = 12.
    &endif

    assign v_val_tit_barra = dec(substring(p_cod_barra_2, 38, 10))
           p_cod_barra = substring(p_cod_barra_2,01,03)
                       + substring(p_cod_barra_2, 04,01)
                       + substring(p_cod_barra_2, 33,01)
                       + substring(p_cod_barra_2, 34,04)
                       + string(v_val_tit_barra,"9999999999")
                       + substring(p_cod_barra_2, 05,05)
                       + substring(p_cod_barra_2, 11,10)
                       + substring(p_cod_barra_2, 22,10).

        output to c:\temp\barra.txt.

        put unformatted p_cod_barra skip.

        output close.

END PROCEDURE. /* pi_retorna_cod_barra_leitora */

def var v_cod_referencia as char no-undo.
def var v_per_apuracao   as char no-undo.
def var v_lacre          as char no-undo.
def var v_digla          as char no-undo.
def var v_Id_fgts        as char no-undo.

PROCEDURE pi_extracao_banco_brasil_detalhe:

    /************************* Variable Definition Begin ************************/

    def var v_cod_bairro_favorec             as character       no-undo. /*local*/
    def var v_cod_cep_favorec                as character       no-undo. /*local*/
    def var v_cod_cidad_favorec              as character       no-undo. /*local*/
    def var v_cod_ender_favorec              as character       no-undo. /*local*/
    def var v_cod_estado_favorec             as character       no-undo. /*local*/
    def var v_num_bloco                      as integer         no-undo. /*local*/
    def var v_num_reg_bloco                  as integer         no-undo. /*local*/
    def var v_num_tip_id_feder               as integer         no-undo. /*local*/

                                  /*
    output to c:\temp\dados_citi_detalhe.txt append.

        PUT UNFORMATTED "NOVO REGISTRO" SKIP.

        for each tt_param_program_formul:

                put unformatted 
            tt_param_program_formul.tta_cdn_segment_edi                      ";"
            tt_param_program_formul.tta_cdn_element_edi                      ";"
            tt_param_program_formul.tta_des_label_utiliz_formul_edi          ";"
            tt_param_program_formul.ttv_des_contdo                    skip.

        end.

        put skip(02).
    output close.
    */
    
      
    /************************** Variable Definition End *************************/
    /* DPC - PROJETO TESOURARIA */

    if  p_cdn_segment_edi = 371
    then do: /* --- DETALHE ---*/

        /* DPC */
        /* DADOS CEDENTE */
        find first tt_param_program_formul
               where tt_param_program_formul.tta_cdn_segment_edi = 289
               and   tt_param_program_formul.tta_cdn_element_edi = 003916  no-error.
        assign v_cod_cedente = string(tt_param_program_formul.ttv_des_contdo) when avail tt_param_program_formul.
    
        find first tt_param_program_formul
               where tt_param_program_formul.tta_cdn_segment_edi = 289
               and   tt_param_program_formul.tta_cdn_element_edi = 003734 no-error.
        assign v_nome_cedente = string(tt_param_program_formul.ttv_des_contdo) when avail tt_param_program_formul.
  
        /* DADOS FAVORECIDO */
        find first tt_param_program_formul
               where tt_param_program_formul.tta_cdn_segment_edi = 289
               and   tt_param_program_formul.tta_cdn_element_edi = 003916  no-error.
        assign v_cod_favorecido = string(tt_param_program_formul.ttv_des_contdo) when avail tt_param_program_formul.

        find first tt_param_program_formul
               where tt_param_program_formul.tta_cdn_segment_edi = 289
               and   tt_param_program_formul.tta_cdn_element_edi = 003734 no-error.
        assign v_nome_favorecido = string(tt_param_program_formul.ttv_des_contdo) when avail tt_param_program_formul.
        /* FIM DPC */

        /* ---- C¢digo do banco ----*/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 288
            and   tt_param_program_formul.tta_cdn_element_edi = 4681 no-error. 
        assign v_cod_banco = string(tt_param_program_formul.ttv_des_contdo).

        /* --- Tipo de Pagamento ---*/
        find first tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 3729 no-error.
        assign v_cdn_tip_forma_pagto = int(tt_param_program_formul.ttv_des_contdo).

        /* --- Agˆncia Banc ria Empresa ---*/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 288
            and   tt_param_program_formul.tta_cdn_element_edi = 3901 no-error.
        if length(tt_param_program_formul.ttv_des_contdo) > 4       
            then assign v_cdn_inicial = (length(tt_param_program_formul.ttv_des_contdo) - 3).                
            else assign v_cdn_inicial = 1. 
        assign v_cod_agenc_bcia = substring(tt_param_program_formul.ttv_des_contdo, v_cdn_inicial,4).

        /* --- D¡gito Agˆncia Banc ria Empresa ---*/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 288
            and   tt_param_program_formul.tta_cdn_element_edi = 3902 no-error.
        assign v_cod_digito_agenc_bcia = substring(tt_param_program_formul.ttv_des_contdo, 1,1).

        /* --- Conta Corrente Empresa ---*/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 288
            and   tt_param_program_formul.tta_cdn_element_edi = 3903 no-error.
        if length(tt_param_program_formul.ttv_des_contdo) > 9
            then assign v_cdn_inicial = (length(tt_param_program_formul.ttv_des_contdo) - 8).
            else assign v_cdn_inicial = 1.
        assign v_cod_cta_corren = substring(tt_param_program_formul.ttv_des_contdo,v_cdn_inicial,9).

        /* --- D¡gito Conta Corrente Empresa ---*/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 288
            and   tt_param_program_formul.tta_cdn_element_edi = 3904 no-error.
        assign v_cod_digito_cta_corren = substring(tt_param_program_formul.ttv_des_contdo,1,1).

        /* --- N£mero do T¡tulo ---*/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 3743 no-error.
        assign v_cod_tit_ap_bco = substring(tt_param_program_formul.ttv_des_contdo,1,20).

        /* --- N£mero do T¡tulo ---*/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 3928 no-error.
        assign v_cdn_contador = 20 - length(substring(tt_param_program_formul.ttv_des_contdo,1,20)).

        assign v_cod_tit_ap = substring(tt_param_program_formul.ttv_des_contdo,1,20) + fill(' ',v_cdn_contador).

        /* Codigo do t¡tulo para o Citibank nÆo pode ter o separador ; */
        assign v_cod_tit_ap_2 = replace(v_cod_tit_ap,";"," ").
        
        /* --- Banco do Favorecido ---*/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 3737 no-error.
        if length(tt_param_program_formul.ttv_des_contdo) > 3
            then assign v_cdn_inicial = (length(tt_param_program_formul.ttv_des_contdo) - 2).
            else assign v_cdn_inicial = 1.
        assign v_cod_sist_nac_bcio_fav = string(int(substring(tt_param_program_formul.ttv_des_contdo,v_cdn_inicial,3)),'999').

        /* --- Agˆncia do Favorecido ---*/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 3922 no-error.
        if length(tt_param_program_formul.ttv_des_contdo) > 4
            then assign v_cdn_inicial = (length(tt_param_program_formul.ttv_des_contdo) - 3).
            else assign v_cdn_inicial = 1.
        assign v_cod_agenc_bcia_fav = substring(tt_param_program_formul.ttv_des_contdo,v_cdn_inicial,4).

        /* --- D¡gito Agˆncia Favorecido ---*/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 5143 no-error.
        assign v_cod_digito_agenc_bcia_fav = substring(tt_param_program_formul.ttv_des_contdo,1,1).

        /* --- Conta Corrente Favorecido ---*/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 3796 no-error.
        if length(tt_param_program_formul.ttv_des_contdo) > 10
            then assign v_cdn_inicial = (length(tt_param_program_formul.ttv_des_contdo) - 9).
            else assign v_cdn_inicial = 1.
        assign v_cod_cta_corren_fav = substring(tt_param_program_formul.ttv_des_contdo,v_cdn_inicial,10).

        /* --- D¡gito Conta Favorecido ---*/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 3927 no-error.
        assign v_cod_digito_cta_corren_fav = trim(tt_param_program_formul.ttv_des_contdo).

        if length(v_cod_digito_cta_corren_fav) = 2 then do:
            assign v_cod_cta_corren_fav        = trim(v_cod_cta_corren_fav) + substring(tt_param_program_formul.ttv_des_contdo,1,1)
                   v_cod_digito_cta_corren_fav = substring(tt_param_program_formul.ttv_des_contdo,2,1).
        end.
        if length(v_cod_digito_cta_corren_fav) = 0 then
            assign v_cod_digito_cta_corren_fav = ' '.
        assign v_cdn_contador      = 0.

        /* --- Nome do Favorecido ---*/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 3734 no-error.
        assign v_cdn_contador = 35 - length(substring(tt_param_program_formul.ttv_des_contdo,1,35)).
        assign v_nom_favorec = substring(tt_param_program_formul.ttv_des_contdo,1,35) + fill(' ',v_cdn_contador).

        /* --- Data Pagamento ---*/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 3709 no-error.
        assign v_cod_pagto_remes = substring(tt_param_program_formul.ttv_des_contdo,1,2) + substring(tt_param_program_formul.ttv_des_contdo,3,2) + substring(tt_param_program_formul.ttv_des_contdo,5,4)
               v_cod_pagto_ocor  = substring(tt_param_program_formul.ttv_des_contdo,1,2) + substring(tt_param_program_formul.ttv_des_contdo,3,2) + substring(tt_param_program_formul.ttv_des_contdo,5,4).

        /* --- Valor Pagamento ---*/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 4436 no-error.
        assign v_val_pagto = dec(tt_param_program_formul.ttv_des_contdo).

        /* --- C¢digo do Servi‡o da Empresa no Banco ---*/
        if v_cod_sist_nac_bcio_fav = '   ' then do: /* --- Banco do Brasil ---*/
            find tt_param_program_formul
                where tt_param_program_formul.tta_cdn_segment_edi = 288
                and   tt_param_program_formul.tta_cdn_element_edi = 4641 no-error.
            assign v_num_servico = int(substring(tt_param_program_formul.ttv_des_contdo, 1,3)).
        end.
        else assign v_num_servico = 17. /* --- Outros Bancos ---*/

        /* * N£mero do Banco Destinat rio **/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 4022 no-error.
        assign v_num_bco = (substring(tt_param_program_formul.ttv_des_contdo, 1,3)).

        /** Data de Vencimento do T¡tulo **/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 3606 no-error.
        assign v_cod_vencto = substring(tt_param_program_formul.ttv_des_contdo,1,2) + substring(tt_param_program_formul.ttv_des_contdo,3,2) + substring(tt_param_program_formul.ttv_des_contdo,5,4).

        
        /* * Valor do T¡tulo **/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 4421 no-error.
        assign v_val_titulo = dec(substring(tt_param_program_formul.ttv_des_contdo, 1,17)).   /* alterado de int para dec */

        /* * Valor de Abatimentos **/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 4425 no-error.
        assign v_val_abat = int(substring(tt_param_program_formul.ttv_des_contdo, 1,17)).

        /* * Valor de Descontos **/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 4423 no-error.
        assign v_val_desconto = int(substring(tt_param_program_formul.ttv_des_contdo, 1,17)).
        assign v_val_tot_desc_abat = v_val_abat + v_val_desconto.

        /* * Juros **/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 4422 no-error.
        assign v_val_juros = int(substring(tt_param_program_formul.ttv_des_contdo, 1,17)).

        /** Multa **/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 4426 no-error.
        assign v_val_multa = int(substring(tt_param_program_formul.ttv_des_contdo, 1,17)).

        /** Corre‡Æo Monet ria **/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 4437 no-error.
        assign v_val_correc = int(substring(tt_param_program_formul.ttv_des_contdo, 1,17)).
        assign v_val_acresc = v_val_juros + v_val_multa + v_val_correc.

        assign v_cdn_contador = 0.
        /* * C¢digo de Barras **/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 2807 no-error.
        assign v_cdn_contador = 47 - length(substring(tt_param_program_formul.ttv_des_contdo,1,47)).
        assign v_cod_barra_2 = substring(tt_param_program_formul.ttv_des_contdo,1,47) + fill(' ',v_cdn_contador)
               v_cod_barra_o = substring(tt_param_program_formul.ttv_des_contdo,1,47) + fill(' ',v_cdn_contador).

        run pi_retorna_cod_barra_leitora (input v_cod_barra_2, output v_cod_barra).

        /* * C¢digo Movimento **/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 3933 no-error.
        assign v_cod_ocor_bcia = substring(tt_param_program_formul.ttv_des_contdo, 1,2).

        /* --- Agˆncia C¢digo do Cedente ---*/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 3922 no-error.
        if length(tt_param_program_formul.ttv_des_contdo) > 20
            then assign v_cdn_inicial = (length(tt_param_program_formul.ttv_des_contdo) - 19).
            else assign v_cdn_inicial = 1.
        assign v_cod_agenc_favorec_1 = substring(tt_param_program_formul.ttv_des_contdo,v_cdn_inicial,20).

        /* tipo CGC/CPF Favorecido */
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 3915 no-error.
        assign v_num_tip_id_feder = int(tt_param_program_formul.ttv_des_contdo).

        /* CGC/CPF favorecido */
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 3916 no-error.
        assign v_cod_id_feder_favorec = tt_param_program_formul.ttv_des_contdo.

        /* Endere‡o Favorecido */
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 3917 no-error.
        assign v_cod_ender_favorec = tt_param_program_formul.ttv_des_contdo.

        /* Bairro favorecido */
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 3621 no-error.
        assign v_cod_bairro_favorec = tt_param_program_formul.ttv_des_contdo.

        /* Cidade Favorecido */
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 3918 no-error.
        assign v_cod_cidad_favorec = tt_param_program_formul.ttv_des_contdo.

        /* Cep Favorecido */
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 3920 no-error.
        assign v_cod_cep_favorec = tt_param_program_formul.ttv_des_contdo.

        /* Estado Favorecido */
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 3919 no-error.
        assign v_cod_estado_favorec = tt_param_program_formul.ttv_des_contdo.

        if v_cdn_tip_forma_pagto    = 2 then /* DOC */    
            assign v_cod_forma      = '03'
                   v_cod_campo_ctro = '700'.

        if v_cdn_tip_forma_pagto    = 3 then /* Cr²dito Conta Corrente */
            assign v_cod_forma      = '01'
                   v_cod_campo_ctro = '000'.

        if v_cdn_tip_forma_pagto    = 4 then /* Cheque Administrativo  */
            assign v_cod_forma      = '02'
                   v_cod_campo_ctro = '000'.

        /* Alterado por Amarildo para tratar Ordem de Pagamento*/
        if v_cdn_tip_forma_pagto    = 6 then /* ORDEM DE PAGAMENTO  */
           assign v_cod_forma      = '10'
                  v_cod_campo_ctro = '000'.
        /* Fim Amarildo */

        /* Alterado por Elisangela para tratar TED*/
        if v_cdn_tip_forma_pagto    = 7 then /* TED CIP  */
            assign v_cod_forma      = '03'
                   v_cod_campo_ctro = '018'.

        if v_cdn_tip_forma_pagto    = 8 then /* TED STR  */
            assign v_cod_forma      = '03'
                   v_cod_campo_ctro = '018'.

        /* Fim - Elisangela*/    
        /* DPC - Projeto Tesourari - Valida formas de pagamento */
        /* INICIO GERA€ÇO DO LAYOUT - TRATAMENTO DOS DADOS - TESOURARIA - DPC  */

        /* --- Forma de Pagamento --- DPC - Tesouraria*/
        find first tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 4838 no-error.
        IF AVAIL tt_param_program_formul THEN
        assign v_cdn_forma_pagto = int(tt_param_program_formul.ttv_des_contdo).

        /* Busca forma de pagamento */
        if TRIM(entry(3,v_cod_tit_ap,';')) = "N" /*l_n*/ then 
        do:
             find first item_bord_ap no-lock
                  where item_bord_ap.cod_estab_bord = TRIM(entry(1,v_cod_tit_ap,';'))
                    and item_bord_ap.num_id_item_bord_ap = int(entry(2,v_cod_tit_ap,';')) no-error.
        end.
        else 
        do:
             find first item_bord_ap_agrup no-lock
                  where item_bord_ap_agrup.cod_estab_bord =  TRIM(entry(1,v_cod_tit_ap,';'))
                    and item_bord_ap_agrup.num_id_agrup_item_bord_ap = int(entry(2,v_cod_tit_ap,';'))  no-error.
             if avail item_bord_ap_agrup then do:
                 blk_bord:
                 for each item_bord_ap no-lock
                    where item_bord_ap.cod_estab_bord            = item_bord_ap_agrup.cod_estab_bord
                      and item_bord_ap.num_id_agrup_item_bord_ap = item_bord_ap_agrup.num_id_agrup_item_bord_ap
                      break by item_bord_ap.cod_estab:
                      if first-of (item_bord_ap.cod_estab) then 
                              leave blk_bord.
                 end.
             end.
        end.
        
        if avail item_bord_ap then
            assign v_cdn_forma_pagto = int(item_bord_ap.cod_forma_pagto) when avail item_bord_ap
                   v_cod_pagto_remes = string(item_bord_ap.dat_pagto,"99999999").
        
        assign v_cod_segto = ''.
        case v_cdn_forma_pagto:

            when 11 then assign v_cod_segto = 'O'. /* Tributos com cod de barras */
            when 40 then assign v_cod_segto = 'O'. /* FGTS */
            when 16 then assign v_cod_segto = 'N'.
            when 17 then assign v_cod_segto = 'N'.
            when 18 then assign v_cod_segto = 'N'.
            when 21 then assign v_cod_segto = 'N'.
            when 22 then assign v_cod_segto = 'N'.
            when 23 then assign v_cod_segto = 'N'.
            when 24 then assign v_cod_segto = 'N'.
            otherwise assign v_cod_segto = ''.
            
        end case.
       

        /* localiza a instru‡Æo de cobran‡a para saber se ‚ implanta‡Æo/altera‡Æo ou estorno */
        find first ocor_bcia_bco no-lock
            where ocor_bcia_bco.cod_banco               = v_cod_banco
            and   ocor_bcia_bco.cod_modul_dtsul         = "APB" /*l_apb*/ 
            and   ocor_bcia_bco.ind_ocor_bcia_remes_ret = "Remessa" /*l_remessa*/ 
            and   ocor_bcia_bco.cod_ocor_bcia_bco       = v_cod_ocor_bcia no-error.
        if avail ocor_bcia_bco then do:
            if ocor_bcia_bco.ind_tip_ocor_bcia = "Implanta‡Æo" /*l_implantacao*/  then
                assign v_ind_tip_instruc = '0'. /* implanta‡Æo */
            else
                if ocor_bcia_bco.ind_tip_ocor_bcia begins "Altera‡Æo" /*l_alteracao*/ 
                or ocor_bcia_bco.ind_tip_ocor_bcia begins "Acerto" /*l_acerto*/  then
                    assign v_ind_tip_instruc = '5'. /* altera‡Æo */
                else
                    assign v_ind_tip_instruc = '9'. /* estorno */
        end.
        else assign v_ind_tip_instruc = '0'. /* sempre ser  implanta‡Æo */ 

        /* Quando esta vari vel for vazia os tipos de documentos nÆo serÆo impostos */
        if v_cod_segto = '' then do:

            if  v_cdn_tip_forma_pagto = 1 or  v_cdn_tip_forma_pagto = 5
            then do:
                if entry(4, v_cod_tit_ap, ';') = "" then 
                   assign v_cod_tip_reg_boleto = '2'.
                else assign v_cod_tip_reg_boleto = '8'.
            end /* if */.
            /* --- 2- DOC / 1-CRDITO CONTA CORRENTE / 7-CHEQUE ADMINISTRATIVO / ORDEM DE PAGAMENTO ---*/   
            if  v_cdn_tip_forma_pagto = 2 or  v_cdn_tip_forma_pagto = 3 or  v_cdn_tip_forma_pagto = 4
            or  v_cdn_tip_forma_pagto = 6 or  v_cdn_tip_forma_pagto = 7 or  v_cdn_tip_forma_pagto = 8
            then do:    

                assign v_cod_det = caps('A' 
                                        + string(v_ind_tip_instruc,'x(1)')                         /* Tipo movimento - 15 a 15 */
                                        + string(v_cod_ocor_bcia,'99')                             /* Cod. de Instru‡Æo - 16 a 17 - 2 */
                                        + string(v_cod_campo_ctro,'999')                           /* Cƒmara Centralizadora - 18 a 20 - 3 */
                                        + string(int(v_cod_sist_nac_bcio_fav),'999')               /* C¢digo do Banco do Favorecido 21 23 - 3 */
                                        + string(int(v_cod_agenc_bcia_fav),'99999')                /* D¡gito Verificador da Agˆncia 24 a 28 - 5 */
                                        + fill(" ",1)                                              /* D¡gito Verificador da Agˆncia 29 29 1 */
                                        + string(dec(v_cod_cta_corren_fav),'99999999999') + string(substring(v_cod_digito_cta_corren_fav,1,1),'x(01)')       /* N£mero da Conta Corrente 30 41 12 */ 
                                        + fill(' ',1)                                               /* D¡gito Verificador da Conta 42 42 1 - Para Citibank deve ser branco*/
                                        + fill(' ',1)                                              /* D¡gito Verificador da AG/Conta 43 43 1 */
                                        + string(v_nom_favorec,'x(30)')                            /* Nome do Favorecido 44 73 30 */
                                        + string(v_cod_tit_ap_2,'x(20)')                             /* Nr Docto 20 */
                                        + string(v_cod_pagto_ocor,'x(8)')                          /* Data do Pagamento 94 101 8 */
                                        + 'BRL'                                                    /* Tipo da Moeda 102 104 3 */
                                        + fill('0',15)                                             /* Quantidade da Moeda 105 119 10 */
                                        + string(v_val_pagto,'999999999999999')                    /* Valor do Pagamento 120 134 15 */
                                        + fill(' ',20)                                             /* Nr Docto Banco - Tam 20 135 a 154*/
                                        + fill('0',23)                                             /* Data real eftiva pagto e valor efetiva pagto */
                                        + fill(' ',40)                                             /* Outras infos */
                                        + fill(' ',02)                                             /* Compl tipo servi‡o - 218 a 219 - Brancos*/
                                        + fill(' ',05)                                             /* Cod. Finalidade TED -5 - 220 a 224*/
                                        + fill(' ',05)                                             /* Febraban - 225 a 229 */
                                        + fill('0',1)                                              /* Aviso favorecido - 1*/  
                                        + fill(' ',10)).                                            /* Ocorrencias 231 a 240 - tam 10*/
                                                     
                /* gera segunda parte - procura sequencia do registro */ 
                find first tt_segment_tot
                     where tt_segment_tot.tta_cdn_segment_edi = 371 no-error.
                if avail tt_segment_tot
                then assign  tt_segment_tot.ttv_qtd_proces_edi  = tt_segment_tot.ttv_qtd_proces_edi  + 1
                            tt_segment_tot.ttv_qtd_bloco_docto = tt_segment_tot.ttv_qtd_bloco_docto + 1
                            v_num_reg_bloco                    = tt_segment_tot.ttv_qtd_bloco_docto.
                 
                /* Atualiza somat¢ria dos blocos */
                find first tt_segment_tot
                     where tt_segment_tot.tta_cdn_segment_edi = 999999 no-error.
                if avail tt_segment_tot
                then assign  tt_segment_tot.ttv_qtd_proces_edi  = tt_segment_tot.ttv_qtd_proces_edi  + 1
                             tt_segment_tot.ttv_qtd_bloco_docto = tt_segment_tot.ttv_qtd_bloco_docto + 1.
                
                /* localiza o bloco */
                find first tt_param_program_formul
                     where tt_param_program_formul.tta_cdn_segment_edi = 0
                       and tt_param_program_formul.tta_cdn_element_edi = 2
                       and tt_param_program_formul.tta_des_label_utiliz_formul_edi = 'QTD BLOCOS' no-error.
                if avail tt_param_program_formul then
                     assign v_num_bloco = int(tt_param_program_formul.ttv_des_contdo) + 1.
                                                                                                                            
                assign  v_cod_det = caps(v_cod_det + 
                                         chr(10) +  
                                         string(int(v_cod_banco),"999") +                               /* C¢digo do Banco na Compensa‡Æo 1 a 3 */                                                                                               
                                         string(v_num_bloco,'9999') +                                   /* Lote de servi‡o 4 a 7 */                                                                                                              
                                         '3' +                                                          /* Tipo de Registro */                                                                                                                   
                                         string(v_num_reg_bloco,'99999') +                              /* Sequencial Registro do lote - 9 a 13 - 5*/
                                         'B' +                                                          /* C¢digo de Segmento do Reg. Detalhe 14 a 14 1 */                                                                                       
                                         fill(' ',3) +                                                  /* Uso Exclusivo FEBRABAN/CNAB 15 a 17 - 3 */                                                                                            
                                         string(v_num_tip_id_feder,'9') +                               /* Tipo de Inscri‡Æo do Favorecido 18 a 18 1-Num CPF = '1' CNPJ = '2' */                                                                 
                                         string(dec(v_cod_id_feder_favorec),'99999999999999') +         /* N§ de Inscri‡Æo do Favorecido 19 a 32 14-Num */                                                                                       
                                         string(v_cod_ender_favorec,'x(30)') +                          /* Nome da Rua, Av, P‡a, Etc 33 a 62 30 */                                                                                               
                                         '00000' +                                                      /* N§ do Local 63 a 67 - 5-Num Campo opcional, nÆo tratado pelo Banco do Brasil. Caso nÆo preenchido informar '00000' */                 
                                         fill(' ',15) +                                                 /* Casa, Apto, Etc688215-Alfa Campo opcional, nÆo tratado pelo Banco do Brasil. Caso nÆo preenchido informar 'brancos'. */               
                                         string(v_cod_bairro_favorec,'x(15)') +                         /* Nome do Bairro839715-Alfa Campo opcional, nÆo tratado pelo Banco do Brasil. Caso nÆo preenchido informar 'brancos' */                 
                                         string(v_cod_cidad_favorec,'x(20)') +                          /* Nome da Cidade9811720-Alfa Campo opcional, nÆo tratado pelo Banco do Brasil. Caso nÆo preenchido informar 'brancos'. */               
                                         string(v_cod_cep_favorec,'x(8)') +                             /* CEP1181225-Num Campo opcional, nÆo tratado pelo Banco do Brasil. Caso nÆo preenchido informar '00000'. */                             
                                         string(v_cod_estado_favorec,'x(2)') +                          /* Sigla do Estado 126 a 127 2-Alfa */                                                                                                   
                                         string(v_cod_pagto_ocor,'99999999') +                          /* Data do Vencimento (Nominal) 128 a 135 8 */                                                                                           
                                         string(v_val_titulo,'999999999999999') +                       /* Valor do Documento (Nominal) 136 a 150 132 Num */                                                                                     
                                         string(v_val_abat,'999999999999999') +                         /* Valor do Abatimento 151 a 165 132 Num */                                                                                              
                                         string(v_val_desconto,'999999999999999') +                     /* Valor do Desconto 166 a 180 132 Num */                                                                                                
                                         string(v_val_juros,'999999999999999') +                        /* Valor da Mora 181 a 195 132 Num */                                                                                                    
                                         string(v_val_multa,'999999999999999') +                        /* Valor da Multa 196 a 210 132 Num */                                                                                                   
                                         fill(' ',15) +                                                 /* C¢digo/Documento do Favorecido 211 a 225 15 */                                                                                        
                                         "0" +                                                          /* Aviso ao Favorecido 226 a 226 1-Num'0' */                                                                                             
                                         fill('0',6) +                                                  /* Uso Exclusivo para o SIAPE 227 a 232 6-Num Campo opcional, nÆo tratado pelo Banco do Brasil. Caso nÆo preenchido informar '000000'. */ 
                                         fill(' ',8) + chr(10)).                                        /* C¢digo ISPB 233 a 240 - 8*/

                   assign v_num_cont = v_num_cont + 1.  
            end. /* if 2- DOC / 1-CRDITO CONTA CORRENTE / 7-CHEQUE ...*/

            /* --- BOLETO ---*/
            if  v_cdn_tip_forma_pagto = 1 or  v_cdn_tip_forma_pagto = 5
            then do:

                assign v_num_contador  = v_num_contador + 1
                       v_num_cont_val  = v_val_pagto + v_num_cont_val.
                    
                assign v_cod_det = caps('J'                                                  /* C¢digo de Segmento no Reg. Detalhe 14 a 14 1-Alfa'J' */                             
                                        + string(v_ind_tip_instruc,'x(1)')                   /* Tipo de Movimento 15 a 15 1-Num 0-InclusÆo 9-exclusÆo */                            
                                        + string(v_cod_ocor_bcia,'x(2)')                     /* C¢digo da Instru‡Æo p/ Movimento 16 a 17 - 2-Num InclusÆo = '00' ExclusÆo = '99' */ 
                                        + string(v_cod_barra,'x(44)')                        /* C¢digo de Barras 18 a 61 - 44-Num */                                                
                                        + string(v_nom_favorec,'x(30)')                      /* Nome do Benefici rio 62 a 91 - 30-Alfa */                                           
                                        + v_cod_vencto                                       /* Data do Vencimento (Nominal)92 a 99 8-Num DDMMAAAA */                               
                                        + string(v_val_titulo,'999999999999999')             /* Valor do T¡tulo (Nominal) 100 a 114 13,2 Num */                                     
                                        + string(v_val_tot_desc_abat,'999999999999999')      /* Valor do Desconto + Abatimento 115 a 129 - 13,2 Num */                              
                                        + string(v_val_acresc,'999999999999999')             /* Valor da Mora + Multa130144132Num */                                                
                                        + v_cod_pagto_remes                                  /* Data do Pagamento1451528-NumDDMMAAAA */                                             
                                        + string(v_val_pagto,'999999999999999')              /* Valor do Pagamento 153 167 13,2 NumValor */                                         
                                        + fill('0',15)                                       /* Quantidade da Moeda 168 a 182 10,5 Num'000000000000000'  */                         
                                        + string(v_cod_tit_ap_2,'x(20)')                       /* Nø do Docto Atribu¡do pela Empresa18320220 */                                       
                                        + string(v_cod_tit_ap_bco,'x(20)')                   /* Nø do Docto Atribu¡do pelo Banco 203 a 222 -20 */                                       
                                        + "09"                                               /* C¢digo de Moeda 223 a 224 2-Num'09'        */                                       
                                        + fill(' ',6)                                        /* CNAB 225 a 230 6 - Brancos */                                                       
                                        + fill(' ',10))                                      /* C¢digo das Ocorrˆncias p/ Retorno 231 240 -10-Alfa */
                                        + chr(10).
    
                /* Para Bradesco - Valores Acima de 250 mil dever  ser enviado o Seguimento J52 */
                /* DPC */
    
                IF int(v_val_pagto) > 25000000 THEN DO:
    
                    /* Localiza nome do Sacado - Cod. Empresa e nome */
                    /* Pesquisa empresa */
                    find first tt_param_program_formul 
                            where tt_param_program_formul.tta_cdn_segment_edi = 289
                            and tt_param_program_formul.tta_cdn_element_edi = 3928 no-error.
                    if avail tt_param_program_formul then do:
                    
                        find ems5.estabel no-lock where
                             ems5.estabel.cod_estab = entry(1,tt_param_program_formul.ttv_des_contdo,";") no-error.
                       
                       find pessoa_jurid no-lock where
                            pessoa_jurid.num_pessoa_jurid = estabel.num_pessoa_jurid no-error.
    
                        ASSIGN v_cod_sacado  = pessoa_jurid.cod_id_feder when avail pessoa_jurid
                               v_nome_sacado = estabel.nom_pessoa when avail estabel.

                        IF length(v_cod_sacado) < 15 
                        then assign v_cod_sacado = fill('0',15 - length(v_cod_sacado)) + v_cod_sacado.
                               
                    end.
    
                    /* Numero Sequencial */
                    find first tt_param_program_formul
                     where tt_param_program_formul.tta_cdn_segment_edi = 0
                       and tt_param_program_formul.tta_cdn_element_edi = 2
                       and tt_param_program_formul.tta_des_label_utiliz_formul_edi = 'QTD BLOCOS' no-error.
                    if avail tt_param_program_formul then
                        assign v_num_bloco = int(tt_param_program_formul.ttv_des_contdo) + 1.

                   find first tt_segment_tot
                         where tt_segment_tot.tta_cdn_segment_edi = 371 
                           and tt_segment_tot.ttv_log_trailler_edi = no
                           and tt_segment_tot.ttv_log_header_edi   = no no-error.
                    if avail tt_segment_tot
                    then assign  tt_segment_tot.ttv_qtd_proces_edi  = tt_segment_tot.ttv_qtd_proces_edi  + 1
                                 tt_segment_tot.ttv_qtd_bloco_docto = tt_segment_tot.ttv_qtd_bloco_docto + 1
                                 v_num_reg_bloco                    = tt_segment_tot.ttv_qtd_bloco_docto.

                    /* Atualiza somatoria dos blocos*/
                    find first tt_segment_tot
                         where tt_segment_tot.tta_cdn_segment_edi = 999999 no-error.

                    if avail tt_segment_tot
                    then assign  tt_segment_tot.ttv_qtd_proces_edi  = tt_segment_tot.ttv_qtd_proces_edi  + 1
                                 tt_segment_tot.ttv_qtd_bloco_docto = tt_segment_tot.ttv_qtd_bloco_docto + 1.
                                
                    assign v_tip_inscr = string((if length(v_cod_sacado) >= 14 then 2 else 1))
                           v_tip_inscr_ced = string((if length(v_cod_cedente) >= 14 then 2 else 1))   /* 1-cpf 2-cnpj*/
                           v_cod_cedente = fill('0',15 - length(v_cod_cedente)) + v_cod_cedente.
                           
            
                    assign v_cod_det = v_cod_det + chr(10)  /* Soma a linha do J + o J 52 */ 
                                       + CAPS(string(v_cod_banco,'x(03)')         /* Cod Banco        1 a 3 - v_cod_banco                */
                                       + string(v_num_bloco,'9999')               /* Lote do servi‡o  4 a 7 - v_num_bloco                */
                                       + '3'                                      /* Tipo do Registro 8     - 3 fixo                     */
                                       + string(v_num_reg_bloco,'99999')          /* Nr Seq reg lote  9 a 13 - v_num_reg_bloco           */
                                       + 'J'                                      /* Cod. Seq Reg     14     - J fixo                    */
                                       + ' '                                      /* Uso Exclusivo    15     - ""                        */
                                       +  string(v_cod_ocor_bcia,'x(02)')         /* Cod. Movto       16 a 17 - v_cod_ocor_bcia          */
                                       + '52'                                     /* Id Reg           18 a 19 - 52 fixo                  */
                                       + string(v_tip_inscr,'x(01)')              /* Tipo Inscr Sacado 20     - v_tip_inscr              */                             
                                       + string(v_cod_sacado,'x(15)')             /* Nr Inscr         21 a 35 - v_cod_sacado-edi-237     */
                                       + string(v_nome_sacado,'x(40)')            /* Nome             36 a 75 -  v_nome_sacado-edi-237   */
                                       + string(v_tip_inscr_ced,'x(01)')          /* Tipo Inscricao   76      - v_tip_inscr_ced          */
                                       + string(v_cod_cedente,'x(15)')            /* Nr Inscri        77 a 91 - v_cod_cedente            */                                    
                                       + string(v_nome_cedente,'x(40)')           /* Nome             92 a 131 - v_nome_cedente          */
                                       + '0'                                      /* Tipo de inscri‡Æo sacador - 132 a 132               */
                                       + fill(' ',15)                             /* N£mero de inscri‡Æo sacardo - 133 a 147 - 15        */
                                       + fill(' ',40)                             /* Nome do Sacado - 148 a 187 - 40                    */
                                       + fill(' ',53))                            /* Uso Febraban - 188 a 240 - 53                       */                     
                                       + chr(10).
                END.                                                                      
            end. /* BOLETO */                                                                                              
        end. /* if v_cod_segto = '' */
        else do:

           RUN pi_processa_padrao.

           case v_cod_segto:
                 when "N" /*l_n*/  then  /* Pagamento Tributos (GPS / DARF / DARF SIMPLES) */
                 do: 
                     /* Busca nome do Contribuinte */
                     find first tt_param_program_formul
                     where tt_param_program_formul.tta_cdn_segment_edi = 289
                       and tt_param_program_formul.tta_cdn_element_edi = 3734 
                       and tt_param_program_formul.tta_des_label_utiliz_formul_edi = 'Nome_fav,' no-error.
                    if avail tt_param_program_formul then
                        assign v_nom_contrib = tt_param_program_formul.ttv_des_contdo.

                     run pi_segto_tipo_N.

                     assign v_cod_det  = ('N'                                        +   /* C¢digo de Segmento no Reg. Detalhe 14 a 41-Alfa'O' */
                                          string(v_ind_tip_instruc,'x(1)')           +   /* Tipo de Movimento 15 a 15 1 -Num - InclusÆo = '0' ExclusÆo = '9' */  
                                          string(v_cod_ocor_bcia,'x(2)')             +   /* C¢digo da Instru‡Æo p/ Movimento 16 a 17 tam 2 - Num InclusÆo = '00' ExclusÆo = '99' */
                                          string(v_cod_tit_ap_2,'x(20)')               +   /*Num. do Docto Atribu­do pela Empresa                                */ 
                                          fill(' ',20)                               +   /*Num. do Docto Atribu­do pelo Banco                                  */ 
                                          string(v_nom_contrib,'x(30)')              +   /*Nome do Contribuinte                                              */ 
                                          string(v_dt_pagto,'x(8)')                  +   /*Data do Pagamento                                                 */ 
                                          string(v_val_pagto,'999999999999999')      +   /*Valor do Total do Pagamento                                       */ 
                                          string(v_inf_compl_trib,'x(120)'))         +   /*Informacoes Complementares de acordo com o respectivo tributo     */ 
                                          fill(' ',10)                               +   /*Codigos das Ocorr?ncias p/ Retorno                                */ 
                                          chr(10).
                     
                 end.
                 when "O" /*l_o*/  then  /* Pagamento Contas Concessionarias */
                 do:
    
                     /* TRIBUTOS TRATADOS 
                        DARF PRETO, GPS, GARE-SP ICMS, GARE-SP DR, GARE-SP ITCMD, FGTS
                        DARF, DARF NORMAL, DARF SIMPLES, DARFJ
                        GARE DR/ITCMD/ICMS, GARE-SP DR/ITCMD/ICMS, IPVA
                        LICENCIAMENTO, DPVAT, IPTU */
                      /* DPC - Projeto Tesouraria - Projeto nr PRJ0010160 */
    
                     run pi_segto_tipo_O.
                     assign v_cod_det  = ('O'                                        +   /* C¢digo de Segmento no Reg. Detalhe14141-Alfa'O' */
                                          string(v_ind_tip_instruc,'x(1)')           +   /* Tipo de Movimento 15 15 1 -Num - InclusÆo = '0' ExclusÆo = '9' */  
                                          string(v_cod_ocor_bcia,'x(2)')             +   /* C¢digo da Instru‡Æo p/ Movimento 16 a 17 tam 2 - Num InclusÆo = '00' ExclusÆo = '99' */
                                          string(v_cod_barra,'x(44)')                +   /* Cod. de Barras - Informar o c¢digo de barras conforme captura da leitora ¢tica. NÆo informar a linha digit vel. */
                                          string(v_nom_favorec,'x(30)')              +   /* Nome da Concession ria/àrgÆo P£blico 62 a 91 30 -Alfa */  
                                          string(v_dt_vencto,'x(8)')                 +   /* Data Nominal do Vencimento 92 a 99 8-Num DDMMAAAA     */  
                                          string(v_dt_pagto,'x(8)')                  +   /* Data do Pagamento 100 a 107 8- Num DDMMAAAA           */  
                                          string(v_val_pagto,'999999999999999')      +   /* Valor do Pagamento 108 a 122 13,2 Num                 */ 
                                          string(v_cod_tit_ap_2,'x(20)')             +   /* Nø Docto Atribu¡do pela Empresa 123 a 142 20-Alfa    */ 
                                          fill(' ',20)                               +   /* Nø Docto Atribu¡do pelo Banco 143 a 162 - 20 -Alfa  */
                                          fill(' ',68)                               +   /* Uso Exclusivo FEBRABAN/CNAB 163 a 230 68-Alfa Brancos */
                                          fill(' ',10)                               +   /* C¢digo das Ocorrˆncias p/ Retorno 231 a 240 10-Alfa Arquivo Remessa = 0000000000 */
                                          chr(10)).

                     /* Para pagamento de FGTS - Seguimento W e W1 - Info complementares*/
                     IF v_cdn_forma_pagto = 40 THEN DO:

                        assign v_num_seq_j52 = v_num_seq_j52 + 1. 

                        /* Atualiza totais do bloco */
                        /* Numero Sequencial */
                        find first tt_param_program_formul
                         where tt_param_program_formul.tta_cdn_segment_edi = 0
                           and tt_param_program_formul.tta_cdn_element_edi = 2
                           and tt_param_program_formul.tta_des_label_utiliz_formul_edi = 'QTD BLOCOS' no-error.
                        if avail tt_param_program_formul then
                            assign v_num_bloco = int(tt_param_program_formul.ttv_des_contdo) + 1.
    
                        find first tt_segment_tot
                             where tt_segment_tot.tta_cdn_segment_edi = 371 
                               and tt_segment_tot.ttv_log_trailler_edi = no
                               and tt_segment_tot.ttv_log_header_edi   = no no-error.
                        if avail tt_segment_tot
                        then assign  tt_segment_tot.ttv_qtd_proces_edi  = tt_segment_tot.ttv_qtd_proces_edi  + 1
                                     tt_segment_tot.ttv_qtd_bloco_docto = tt_segment_tot.ttv_qtd_bloco_docto + 1
                                     v_num_reg_bloco                    = tt_segment_tot.ttv_qtd_bloco_docto.
    
                        /* Atualiza somatoria dos blocos*/
                        find first tt_segment_tot
                             where tt_segment_tot.tta_cdn_segment_edi = 999999 no-error.
                        if avail tt_segment_tot
                        then assign  tt_segment_tot.ttv_qtd_proces_edi  = tt_segment_tot.ttv_qtd_proces_edi  + 1
                                     tt_segment_tot.ttv_qtd_bloco_docto = tt_segment_tot.ttv_qtd_bloco_docto + 1.

                        
                            assign  v_cod_det = v_cod_det 
                                                + chr(10)
                                                + caps(string(v_cod_banco,'x(03)')              /* Cod Banco        1 a 3 - v_cod_banco                */       
                                                        + string(v_num_bloco,'9999')               /* Lote do servi‡o  4 a 7 - v_num_bloco                */       
                                                        + '3'                                      /* Tipo do Registro 8     - 3 fixo                     */       
                                                        + string(v_num_reg_bloco,'99999')          /* Nr Seq reg lote  9 a 13 - v_num_reg_bloco           */       
                                                        + 'W'                                      /* Cod. Seq Reg     14     - w fixo                    */       
                                                        + string(v_num_seq_j52,"9")                /* Nr seguimento reg complementar - 15 a 15            */
                                                        + "9"                                      /* Identifica uso info complementar */ 
                                                        + fill(' ',80)                  
                                                        + fill(' ',80)
                                                        + string(v_inf_compl_trib,'x(52)')         /* Info W1 */
                                                        + fill(' ',12)                             /* Ocorrˆncia - Verificar o nr de caracteres que este campo possui*/
                                                        + chr(10)).
                     END.
                end.
           end case.                
       END. /* Else Tributos */
    end /* if */.
END PROCEDURE. /* pi_extracao_banco_brasil_detalhe */


PROCEDURE pi_segto_tipo_N:

    CASE v_cdn_forma_pagto:
        WHEN 16 THEN /* DARF Normal - N2 */          
        DO:
            ASSIGN v_inf_compl_trib = string(v_cod_receita,"X(06)")              + /* "Codigo da Receita do Tributo           " */
                                      STRING(v_id_contrib,"X(02)")               + /* "Tipo de Identifica‡Æo do Contribuinte  " */
                                      STRING(v_cod_id_contrib,"99999999999999")  + /* "Identifica‡Æo do Contribuinte          " */
                                      '16'                                       + /* "Codigo de Identifica‡Æo do Tributo     " */
                                      string(v_dt_vencto-aux,"X(8)")             + /*string(v_dt_periodo,"99999999")            +  "Per­odo de Apura‡Æo                    " */
                                      string(v_cod_referencia,'x(17)')           + /* "N£mero de Referˆncia                   " */
                                      string(v_vl_princ,"999999999999999")       + /* "Valor Principal                        " */
                                      string(v_vl_multa,"999999999999999")       + /* "Valor da Multa                         " */
                                      string(v_juros_encargos,"999999999999999") + /* "Valor dos Juros / Encargos             " */
                                      string(v_dt_vencto,"x(8)")                 + /* "Data do Vencimento                     " */
                                      fill(' ',18).                                /* "Uso Exclusivo FEBRABAN/CNAB            " */
                                      
        END.
        WHEN 17 THEN /* Pagamento GPS - N1*/ 
        DO:
            ASSIGN v_inf_compl_trib = string(v_cod_receita,"X(06)")              + /* "Codigo da Receita do Tributo           " */ 
                                      STRING(v_id_contrib,"X(02)")               + /* "Tipo de Identifica‡Æo do Contribuinte  " */ 
                                      STRING(v_cod_id_contrib,"99999999999999")  + /* "Identifica‡Æo do Contribuinte          " */ 
                                      '17'                                       + /* Codigo de Identifica‡Æo do Tributo    " */
                                      string(c-aux,"x(6)")                       + /* M?s e ano de compet?ncia              " */
                                      string(v_vl_tributo   ,"999999999999999")  + /* Valor Previsto do pagamento do INSS   " */
                                      string(v_vl_out_entid ,"999999999999999")  + /* Valor de Outras Entidades             " */
                                      string(v_vl_corr_monet,"999999999999999")  + /* Atualiza‡Æo Monetÿria                 " */
                                      fill(' ',45)                                 /* Uso Exclusivo FEBRABAN/CNAB           " */
                                      .
        END.
        WHEN 18 THEN /* DARF Simples */  
        DO:
            ASSIGN v_inf_compl_trib = string(v_cod_receita,"X(06)")                + /* "Codigo da Receita do Tributo                   " */ 
                                      STRING(v_id_contrib,"X(02)")                 + /* "Tipo de Identifica‡Æo do Contribuinte          " */ 
                                      STRING(v_cod_id_contrib,"99999999999999")    + /* "Identifica‡Æo do Contribuinte                  " */ 
                                      '16'                                         + /* "Codigo de Identifica‡Æo do Tributo             " */
                                      string(v_dt_periodo,"99999999")              + /* "Per­odo de Apura‡Æo                            " */
                                      string(v_cod_referencia,'x(17)')             + /* "N£mero de Referˆncia                   " */
                                      string(v_vl_princ,"999999999999999")         + /* "Valor Principal                                " */
                                      string(v_vl_multa,"999999999999999")         + /* "Valor da Multa                                 " */
                                      string(v_juros_encargos,"999999999999999")   + /* "Valor dos Juros / Encargos                     " */
                                      string(v_dt_vencto,"99999999")               + /* "Data Vencimento                                " */
                                      fill(' ',18).                                  /* "Uso Exclusivo FEBRABAN/CNAB                    " */
                                      
        END.
        WHEN 22 OR 
        WHEN 23 OR
        WHEN 24 THEN /* GARE-SP (ICMS/DR/ITCMD) N4 */  
        DO:
            ASSIGN v_inf_compl_trib = string(v_cod_receita,"X(06)")                + /* "Codigo da Receita do Tributo                   " */ 
                                      STRING(v_id_contrib,"X(02)")                 + /* "Tipo de Identifica‡Æo do Contribuinte          " */ 
                                      STRING(v_cod_id_contrib,"99999999999999")    + /* "Identifica‡Æo do Contribuinte                  " */ 
                                      STRING(v_forma_pagto,'99')                   + /* "Codigo de Identifica‡Æo do Tributo  -22-Gare SP ICMS, 23- Gare SP DR, 24- Gare SP ITCMD " */
                                      string(v_dt_vencto,"99999999")               + /* "Data Vencimento                                " */
                                      FILL(' ',12)                                 + /* IE/MUNIC/DECLAR Inscri‡Æo Estadual/C¢digo do Munic¡pio/N£mero Declara‡Æo - obrigat¢rio */
                                      Fill(' ',13)                                 + /* D¡vida Ativa / N§ Etiqueta */
                                      string(c-aux,"x(6)")                         + /* Periodo de Referˆncia */  
                                      fill(' ',13)                                 + /* Nr da Parcela / notifica‡Æo */
                                      string(v_vl_receita_bruta,"999999999999999") + /* "Valor da Receita Bruta Acumulada               " */
                                      string(v_juros_encargos,"99999999999999")    + /* "Valor dos Juros / Encargos                     " */
                                      string(v_vl_multa,"99999999999999")          + /* "Valor da Multa                                 " */
                                      fill(' ',1).                                   /* "Uso Exclusivo FEBRABAN/CNAB                    " */
                                      
        END.
    END CASE.
END PROCEDURE. /* pi_segto_tipo_N */

PROCEDURE pi_segto_tipo_O:

    run pi_retorna_cod_barra_leitora_O (input v_cod_barra_O, output v_cod_barra).

    /** Codigo de Barras **/
    assign v_cod_barra_O  = string(v_cod_barra,"x(44)")
           v_cdn_contador = 0.

    find first tt_param_program_formul where
         tt_param_program_formul.tta_cdn_segment_edi = 289  and
         tt_param_program_formul.tta_cdn_element_edi = 3734 no-lock no-error.
    if avail tt_param_program_formul then
    assign v_nom_conces = string(tt_param_program_formul.ttv_des_contdo,"x(30)"). /* posicao 066 a 095 (Nome Concessionÿria) */

    /* Inf Complementar para FGTS por c¢digo de barras - Convˆnio Caixa 181, FGTS 418, Filantr¢pico 604 e Caixa 0182, */
    IF v_cdn_forma_pagto = 40 THEN DO: /* Pagamento FGTS - W1 */ 

            ASSIGN v_inf_compl_trib = "01"                                       + /* Identificador de Tributo                  */
                                      string(v_cod_receita,"X(06)")              + /* "Codigo da Receita do Tributo           " */ 
                                      STRING(v_id_contrib,"X(02)")               + /* "Tipo de Identifica‡Æo do Contribuinte  " */ 
                                      STRING(v_cod_id_contrib,"99999999999999")  + /* "Identifica‡Æo do Contribuinte          " */ 
                                      string(v_Id_fgts,"x(16)")                  + /* Identificador do FGTS - Fundo de Garantia */
                                      string(v_lacre,'x(9)')                     + /* Lacre da Conectividade Social          */
                                      string(v_digla,'x(02)')                    + /* D¡gito do Lacre                        */
                                      fill(' ',3).                                /* Uso Exclusivo FEBRABAN/CNAB           " */
    END.
                                      
    
END PROCEDURE. /* pi_segto_tipo_O */

PROCEDURE pi_processa_padrao:

    DEFINE VARIABLE mask AS CHARACTER   NO-UNDO.
    DEF VAR cont AS INT  NO-UNDO.
    def var v_dec_aux as dec no-undo.
    def var dt-aux as date no-undo.
    def var i-mes as int no-undo.
    def var i-ano as int no-undo.
    
    /************************** Buffer Definition Begin *************************/

    &if "{&emsuni_version}" >= "1.00" &then
    def buffer b_pessoa_jurid
        for pessoa_jurid.
    &endif

    /*************************** Buffer Definition End **************************/
    
    /* Busca o Nome do Fornecedor do titulo */
    find pessoa_jurid no-lock 
        where pessoa_jurid.cod_id_feder = v_cod_id_feder_favorec no-error.
    if avail pessoa_jurid then
        assign v_cod_id_contrib = string(pessoa_jurid.cod_id_feder,'99999999999999')      /* Identificador Contribuinte */
               v_id_contrib     = '01'                                                    /* Tipo de Identifica‡Æo do Contribuinte */
               v_nom_contrib    = string(pessoa_jurid.nom_pessoa,"x(30)").                /* Nome Contribuiente */            
    
    find tt_param_program_formul
         where tt_param_program_formul.tta_cdn_segment_edi = 288
         and   tt_param_program_formul.tta_cdn_element_edi = 4838 no-error.
    if avail tt_param_program_formul then
        assign v_forma_pagto = trim(tt_param_program_formul.ttv_des_contdo)
               v_vl_tot_pago = ''
               v_dt_pagto    = ''.

    /* =-=-=-=-=-=- Begins Nome do Contribuiente =-=-=-=-=-=-=-=-=-*/
    find first tt_param_program_formul where
         tt_param_program_formul.tta_cdn_segment_edi = 289  and
         tt_param_program_formul.tta_cdn_element_edi = 3928 no-lock no-error.
    if avail tt_param_program_formul then
    do:
         assign v_des_conteudo = tt_param_program_formul.ttv_des_contdo.

         if entry(3,v_des_conteudo,';') = "N" /*l_n*/ then 
         do:
             find first item_bord_ap no-lock
                  where item_bord_ap.cod_estab_bord = entry(1,v_des_conteudo,';')
                    and item_bord_ap.num_id_item_bord_ap = int(entry(2,v_des_conteudo,';')) no-error.
             if avail item_bord_ap then do:

                 FIND FIRST tit_ap OF ITEM_bord_ap NO-LOCK NO-ERROR.

                 find first compl_impto_retid_ap
                      where compl_impto_retid_ap.num_id_tit_ap = tit_ap.num_id_tit_ap
                        and compl_impto_retid_ap.cod_estab     = item_bord_ap.cod_estab no-lock no-error.
             end.   
         end.
         else 
         do:
             find first item_bord_ap_agrup no-lock
                  where item_bord_ap_agrup.cod_estab_bord =  entry(1,v_des_conteudo,';')
                    and item_bord_ap_agrup.num_id_agrup_item_bord_ap =  int(entry(2,v_des_conteudo,';'))  no-error.
             if avail item_bord_ap_agrup then do:
                 blk_teste:
                 for each item_bord_ap no-lock
                    where item_bord_ap.cod_estab_bord            = item_bord_ap_agrup.cod_estab_bord
                      and item_bord_ap.num_id_agrup_item_bord_ap = item_bord_ap_agrup.num_id_agrup_item_bord_ap
                      break by item_bord_ap.cod_estab:
                      if first-of (item_bord_ap.cod_estab) then do:
                          
                         FIND FIRST tit_ap OF ITEM_bord_ap NO-LOCK NO-ERROR.

                         find first compl_impto_retid_ap
                              where compl_impto_retid_ap.num_id_tit_ap = tit_ap.num_id_tit_ap
                                and compl_impto_retid_ap.cod_estab     = item_bord_ap.cod_estab no-lock no-error.
                          if avail compl_impto_retid_ap then
                              leave blk_teste.
                      end.
                 end.
             end.
         end.
    end.

    if avail compl_impto_retid_ap then 
       ASSIGN v_cod_receita = STRING(compl_impto_retid_ap.cod_classif_impto). 


    /*Begins: 26/03/2019 */

    FIND tit_ap WHERE 
        tit_ap.cod_estab       = item_bord_ap.cod_estab          AND   
        tit_ap.cod_espec_docto = item_bord_ap.cod_espec_docto    AND   
        tit_ap.cod_ser_docto   = item_bord_ap.cod_ser_docto      AND   
        tit_ap.cod_tit_ap      = item_bord_ap.cod_tit_ap         AND   
        tit_ap.cdn_fornec      = item_bord_ap.cdn_fornec         AND   
        tit_ap.cod_parcela     = item_bord_ap.cod_parcela        NO-LOCK NO-ERROR.
   IF AVAIL tit_ap THEN 
   DO:
      FIND item_lote_impl_ap WHERE 
           item_lote_impl_ap.cod_estab     = tit_ap.cod_estab     AND 
           item_lote_impl_ap.cod_refer     = tit_ap.cod_refer     AND 
           item_lote_impl_ap.num_seq_refer = tit_ap.num_seq_refer NO-LOCK NO-ERROR.
       IF AVAIL ITEM_lote_impl_ap THEN 
          FIND ext_ITEM_lote_impl_ap WHERE 
               ext_ITEM_lote_impl_ap.cod_estab     = item_lote_impl_ap.cod_estab     AND   
               ext_ITEM_lote_impl_ap.cod_refer     = item_lote_impl_ap.cod_refer     AND   
               ext_ITEM_lote_impl_ap.num_seq_refer = item_lote_impl_ap.num_seq_refer NO-LOCK NO-ERROR.  

       IF NOT AVAIL ext_item_lote_impl_ap THEN
          FIND ext_ITEM_lote_impl_ap WHERE
               ext_ITEM_lote_impl_ap.cod_estab     = item_bord_ap.cod_estab_bord       AND
               ext_ITEM_lote_impl_ap.cod_refer     = STRING(item_bord_ap.cod_portador) AND
               ext_ITEM_lote_impl_ap.num_seq_refer = item_bord_ap.num_id_item_bord_ap  NO-LOCK NO-ERROR.
       IF NOT AVAIL ext_item_lote_impl_ap THEN
          FIND FIRST ext_ITEM_lote_impl_ap WHERE
                     ext_ITEM_lote_impl_ap.cod_estab     = item_bord_ap.cod_estab_bord       AND
                     ext_ITEM_lote_impl_ap.cod_refer     = STRING(item_bord_ap.cod_portador) NO-ERROR.
    END.

    find ext_item_bord_ap of item_bord_ap no-lock no-error.  
    /* end: 22/02/2019 - Willians Ambrosio / DKP - Busca o t¡tulo para encontrar a tabela espec¡fica */    
    assign l_v_cod_id_contrib_esp = no.

    /* Verifica se existe - Busca valores de cod de tributa‡Æo qdo for pagamento de imposto sem reten‡Æo */                                                                                                               
    if avail ext_item_lote_impl_ap then                                                                                                                                                                                   
        ASSIGN v_cod_receita      = STRING(ext_item_lote_impl_ap.cod_receita,"999999")                                                                                                                                    
               v_cod_referencia   = fill('0',17 - length(trim(ext_item_lote_impl_ap.referencia))) + trim(ext_item_lote_impl_ap.referencia)                                                                                
               v_per_apuracao     = string(ext_item_lote_impl_ap.competencia)                                                                                                                                             
               v_dt_vencto-aux    = string(ext_item_lote_impl_ap.competencia)                                                                                                                                             
               v_Id_fgts          = ext_item_lote_impl_ap.id_fgts                                                                                                                                                         
               v_cod_id_contrib   = ext_item_lote_impl_ap.id_contrib                                                                                                                                                      
               v_cod_id_contrib   = replace(replace(replace(v_cod_id_contrib,"-",""),".",""),"/","")                                                                                                                      
               v_lacre            = ext_item_lote_impl_ap.lacre_conect                                                                                                                                                    
               v_digla            = ext_item_lote_impl_ap.dig_lacre                                                                                                                                                       
               v_vl_tributo       = string(item_bord_ap.val_pagto * 100,'999999999999999') /* Valor Tributo */ when avail item_bord_ap                                                                                    
               v_vl_out_entid     = string(ext_item_lote_impl_ap.vl_outras * 100,'999999999999999') /* Valor Outras Entidades */                                                                                          
               v_vl_receita_bruta = string(ext_item_lote_impl_ap.vl_receita_bruta * 100,'999999999999999') /* Receita Bruta */                                                                                            
               v_percentual       = string(ext_item_lote_impl_ap.perc_impto * 100,'9999999')        /* Percentual */                                                                                                      
               c-aux              = substr(string(ext_item_lote_impl_ap.competencia,"99999999"),3,6) /* usado para GPS */                                                                                                 
               l_v_cod_id_contrib_esp  = yes                                                                                                                                                                              
               v_dt_periodo       = string(day(ext_item_lote_impl_ap.competencia),"99") + string(month(ext_item_lote_impl_ap.competencia),"99") + string(year(ext_item_lote_impl_ap.competencia),"9999") .                
                                                                                                                                                                                                                          
    if avail ext_item_bord_ap then                                                                                                                                                                                        
       ASSIGN v_cod_receita      = STRING(ext_item_bord_ap.cod_receita,"999999")                                                                                                                                          
              v_cod_referencia   = fill('0',17 - length(trim(ext_item_bord_ap.referencia))) + trim(ext_item_bord_ap.referencia)                                                                                           
              v_per_apuracao     = string(ext_item_bord_ap.per_apur)                                                                                                                                                      
              v_dt_vencto-aux    = string(ext_item_bord_ap.per_apur)                                                                                                                                                      
              v_Id_fgts          = ext_item_bord_ap.id_fgts                                                                                                                                                               
              v_cod_id_contrib   = ext_item_bord_ap.id_contrib                                                                                                                                                            
              v_cod_id_contrib   = replace(replace(replace(v_cod_id_contrib,"-",""),".",""),"/","")                                                                                                                       
              v_lacre            = ext_item_bord_ap.lacre_conect                                                                                                                                                          
              v_digla            = ext_item_bord_ap.dig_lacre                                                                                                                                                             
              v_vl_tributo       = string(item_bord_ap.val_pagto * 100,'999999999999999') /* Valor Tributo */ when avail item_bord_ap                                                                                     
              v_vl_out_entid     = string(ext_item_bord_ap.vl_outras * 100,'999999999999999') /* Valor Outras Entidades */                                                                                                
              v_vl_receita_bruta = string(ext_item_bord_ap.vl_receita_bruta * 100,'999999999999999') /* Receita Bruta */                                                                                                  
              v_percentual       = string(ext_item_bord_ap.perc_impto * 100,'9999999')        /* Percentual */                                                                                                            
              c-aux              = substr(string(ext_item_bord_ap.per_apur,"99999999"),3,6) /* usado para GPS */                                                                                                          
              l_v_cod_id_contrib_esp  = yes                                                                                                                                                                               
              v_dt_periodo       = string(day(ext_item_bord_ap.competencia),"99") + string(month(ext_item_bord_ap.competencia),"99") + string(year(ext_item_bord_ap.competencia),"9999") .                                


    /************************************************************** COMENTADA A REGRA EM 02052019
    find ext_item_bord_ap of item_bord_ap no-lock no-error.

    if avail ext_item_lote_impl_ap then
       ASSIGN v_cod_receita      = STRING(ext_item_lote_impl_ap.cod_receita,"999999")
              v_cod_referencia   = fill('0',17 - length(trim(ext_item_lote_impl_ap.referencia))) + trim(ext_item_lote_impl_ap.referencia)
              v_per_apuracao     = string(ext_item_lote_impl_ap.competencia)
              v_dt_vencto-aux    = string(ext_item_lote_impl_ap.competencia)  
              v_Id_fgts          = ext_item_lote_impl_ap.id_fgts
              v_lacre            = ext_item_lote_impl_ap.lacre_conect  
              v_digla            = ext_item_lote_impl_ap.dig_lacre
              v_vl_tributo       = string(item_bord_ap.val_pagto * 100,'999999999999999') /* Valor Tributo */ when avail item_bord_ap 
              v_vl_out_entid     = string(ext_item_lote_impl_ap.vl_outras * 100,'999999999999999') /* Valor Outras Entidades */           
              v_vl_receita_bruta = string(ext_item_lote_impl_ap.vl_receita_bruta * 100,'999999999999999') /* Receita Bruta */
              v_percentual       = string(ext_item_lote_impl_ap.perc_impto * 100,'9999999')          /* Percentual */        
              v_cod_id_contrib   = ext_item_lote_impl_ap.id_contrib
              v_cod_id_contrib   = replace(replace(replace(v_cod_id_contrib,"-",""),".",""),"/","").
            
        /* Verifica se existe - Busca valores de cod de tributa‡Æo qdo for pagamento de imposto sem reten‡Æo */
        find ext_item_bord_ap no-lock of item_bord_ap no-error.
        if avail ext_item_bord_ap then do:

           ASSIGN v_cod_receita      = STRING(ext_item_bord_ap.cod_receita)
                   v_cod_referencia   = ext_item_bord_ap.referencia
                   v_per_apuracao     = ext_item_bord_ap.per_apur
                   v_dt_vencto-aux    = ext_item_bord_ap.per_apur  
                   v_Id_fgts          = ext_item_bord_ap.id_fgts
                   v_lacre            = ext_item_bord_ap.lacre_conect  
                   v_digla            = ext_item_bord_ap.dig_lacre
                   v_vl_tributo       = string(item_bord_ap.val_pagto * 100,'999999999999999') /* Valor Tributo */ when avail item_bord_ap 
                   v_vl_out_entid     = string(v_dec_aux,'999999999999999') /* Valor Outras Entidades */           
                   v_vl_receita_bruta = string(ext_item_bord_ap.vl_receita_bruta * 100,'999999999999999') /* Receita Bruta */
                   v_percentual       = string(ext_item_bord_ap.perc_impto * 100,'9999999').        /* Percentual */

        end.



        */
  /*  end.*/
    /*end: 26/03/2019 */
    

    /* =-=-=-=-=-=- End Nome do Contribuiente =-=-=-=-=-=-=-=-=-*/

    /* =-=-=-=-=-=- Begins Valor Total Pagamento =-=-=-=-=-=-=-=-=-*/
    find first tt_param_program_formul where
               tt_param_program_formul.tta_cdn_segment_edi = 289  and
               tt_param_program_formul.tta_cdn_element_edi = 4436 no-lock no-error.
    if avail tt_param_program_formul then
    assign v_dec_aux     = dec(tt_param_program_formul.ttv_des_contdo)
           v_vl_tot_pago = string(v_dec_aux,'999999999999999'). /* Valor Total */
    /* =-=-=-=-=-=- End  Valor Total Pagamento =-=-=-=-=-=-=-=-=-*/

    /* =-=-=-=-=-=- Begins Data Pagamento =-=-=-=-=-=-=-=-=-*/
    find first tt_param_program_formul where
         tt_param_program_formul.tta_cdn_segment_edi = 289   and
         tt_param_program_formul.tta_cdn_element_edi = 3709  no-lock no-error.
    if avail tt_param_program_formul then
    assign v_dt_pagto = tt_param_program_formul.ttv_des_contdo. /* Data Pagamento */
    /* =-=-=-=-=-=- End Data Pagamento  =-=-=-=-=-=-=-=-=-*/

    /*=-=-=-=-=-=- Begins Valor Principal =-=-=-=-=-=-=-=-=-*/
    find first tt_param_program_formul where
         tt_param_program_formul.tta_cdn_segment_edi = 289   and  
         tt_param_program_formul.tta_cdn_element_edi = 4421  no-lock no-error.
    if avail tt_param_program_formul then
    assign v_dec_aux   = dec(tt_param_program_formul.ttv_des_contdo).
           v_vl_princ  = string(v_dec_aux,'999999999999999'). /* Valor Principal */
    /*=-=-=-=-=-=- End   Valor Principal  =-=-=-=-=-=-=-=-=-*/

    /*=-=-=-=-=-=- Begins Data emissÆo do t¡tulo =-=-=-=-=-=-=-=-=-*/
    find first tt_param_program_formul where
         tt_param_program_formul.tta_cdn_segment_edi = 289  and
         tt_param_program_formul.tta_cdn_element_edi = 3709 no-lock no-error.
    if avail tt_param_program_formul then 
        assign v_dt_periodo = tt_param_program_formul.ttv_des_contdo. /* Periodo */
    
    /*=-=-=-=-=-=- End Periodo =-=-=-=-=-=-=-=-=-*/

    /*=-=-=-=-=-=- Begins Vl.Multa =-=-=-=-=-=-=-=-=-*/
    find first tt_param_program_formul where
         tt_param_program_formul.tta_cdn_segment_edi = 289  and
         tt_param_program_formul.tta_cdn_element_edi = 4426 no-lock no-error.
    if avail tt_param_program_formul then
    assign v_dec_aux  = dec(tt_param_program_formul.ttv_des_contdo)
           v_vl_multa = string(v_dec_aux,'999999999999999'). /* Multa */
    /*=-=-=-=-=-=- End    Vl.Multa =-=-=-=-=-=-=-=-=-*/

    /*=-=-=-=-=-=- Begins Vl.Juros / Encargo =-=-=-=-=-=-=-=-=-*/
    find first tt_param_program_formul where
         tt_param_program_formul.tta_cdn_segment_edi = 289   and
         tt_param_program_formul.tta_cdn_element_edi = 4422  no-lock no-error.
    if avail tt_param_program_formul then
    assign v_dec_aux        = dec(tt_param_program_formul.ttv_des_contdo).
           v_juros_encargos = string(v_dec_aux,'999999999999999'). /* Juros / Encargos */
    /*=-=-=-=-=-=- End Vl.Juros / Encargos   =-=-=-=-=-=-=-=-=-*/

    /*=-=-=-=-=-=- Begins Data Vencimento =-=-=-=-=-=-=-=-=-*/
    find first tt_param_program_formul where
         tt_param_program_formul.tta_cdn_segment_edi = 289  and
         tt_param_program_formul.tta_cdn_element_edi = 3606 no-lock no-error.
    if avail tt_param_program_formul then do:
            assign v_dt_vencto = tt_param_program_formul.ttv_des_contdo. /* Data Vencimento */
          
            ASSIGN data-faturamento-aux = DATE(int(SUBSTRING(v_dt_vencto,3,2)) - 1 ,DAY(DATE(int(SUBSTRING(v_dt_vencto,3,2)) - 1 MOD 12 + 1, 1, int(SUBSTRING(v_dt_vencto,5,4)) - 1 ) - 1),int(SUBSTRING(v_dt_vencto,5,4))).

            ASSIGN v_dt_vencto-aux = STRING(day(data-faturamento-aux),"99") + STRING(MONTH(data-faturamento-aux),"99") + STRING(year(data-faturamento-aux),"9999").
            
    end.
    /*=-=-=-=-=-=- Begins Data Vencimento  =-=-=-=-=-=-=-=-=-*/

    
    /*=-=-=-=-=-=- Begins Mes e Ano Competencia =-=-=-=-=-=-=-=-=-*/
    find first tt_param_program_formul where
               tt_param_program_formul.tta_cdn_segment_edi = 289  and
               tt_param_program_formul.tta_cdn_element_edi = 3709 no-lock no-error.
    if avail tt_param_program_formul then
        ASSIGN c-aux = SUBSTR(tt_param_program_formul.ttv_des_contdo,3,2) + SUBSTR(tt_param_program_formul.ttv_des_contdo,5,4).
    /*=-=-=-=-=-=- End Mes e Ano Competenciato =-=-=-=-=-=-=-=-=-*/

    /*=-=-=-=-=-=- Begins Vl Previsto Pagto. INSS / Outras Entidades =-=-=-=-=-=-=-=-=-*/
    find first tt_param_program_formul where
               tt_param_program_formul.tta_cdn_segment_edi = 289  and
               tt_param_program_formul.tta_cdn_element_edi = 4421 no-lock no-error.
    if avail tt_param_program_formul then
    assign v_dec_aux = dec(tt_param_program_formul.ttv_des_contdo).
    
    if avail compl_impto_retid_ap then
    find first classif_impto
         where classif_impto.cod_pais          = compl_impto_retid_ap.cod_pais
           and classif_impto.cod_unid_federac  = compl_impto_retid_ap.cod_unid_federac
           and classif_impto.cod_imposto       = compl_impto_retid_ap.cod_imposto
           and classif_impto.cod_classif_impto = compl_impto_retid_ap.cod_classif_impto no-lock no-error.

    if avail classif_impto then do:
        find first imposto
             where imposto.cod_pais         = classif_impto.cod_pais
               and imposto.cod_unid_federac = classif_impto.cod_unid_federac
               and imposto.cod_imposto      = classif_impto.cod_imposto no-lock no-error.

       if avail imposto then 
       do:
           if imposto.ind_tip_impto = 'SEST/SENAT':U then 
               assign v_vl_tributo   = '000000000000000'                    /* Valor Tributo */
                      v_vl_out_entid = string(v_dec_aux,'999999999999999'). /* Valor Outras Entidades */           
           else assign v_vl_tributo   = string(v_dec_aux,'999999999999999')  /* Valor Tributo */
                       v_vl_out_entid = '000000000000000'.                   /* Valor Outras Entidades */              
       end.
    end. 
    /*=-=-=-=-=-=- End Vl Previsto Pagto. INSS / Outras Entidades =-=-=-=-=-=-=-=-=-*/

    /*=-=-=-=-=-=- Begins Vl Core‡Æo Monetÿria =-=-=-=-=-=-=-=-=-*/    
    FIND FIRST tt_param_program_formul where 
               tt_param_program_formul.tta_cdn_segment_edi = 289  AND   
               tt_param_program_formul.tta_cdn_element_edi = 4437 NO-LOCK NO-ERROR.
    if avail tt_param_program_formul then
    assign v_dec_aux        = int(substring(tt_param_program_formul.ttv_des_contdo, 1,17))
           v_vl_corr_monet  = string(v_dec_aux,'999999999999999'). /* Corre‡Æo Monetÿria */
    /*=-=-=-=-=-=- End Vl Core‡Æo Monetÿria =-=-=-=-=-=-=-=-=-*/


    /*=-=-=-=-=-=- Begins Receita Bruta / Pecentual =-=-=-=-=-=-=-=-=-*/    
    if avail compl_impto_retid_ap then
    ASSIGN v_vl_receita_bruta = string((compl_impto_retid_ap.val_rendto_tribut * 100),'999999999999999') /* Receita Bruta */
           v_percentual       = string((compl_impto_retid_ap.val_aliq_impto * 100),'9999999').        /* Percentual */
    /*=-=-=-=-=-=- End Receita Bruta / Pecentual   =-=-=-=-=-=-=-=-=-*/   
END PROCEDURE.

/************************** Internal Procedure End **************************/

/************************* External Procedure Begin *************************/



/************************** External Procedure End **************************/

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
      /*  message getStrTrans("Mensagem nr. ", "EDF") i_msg "!!!":U skip
                getStrTrans("Programa Mensagem", "EDF") c_prg_msg getStrTrans("nÆo encontrado.", "EDF")
                view-as alert-box error. */
        return error.
    end.

    run value(c_prg_msg + ".p":U) (input c_action, input c_param).
    return return-value.
END PROCEDURE.  /* pi_messages */

PROCEDURE pi_retorna_cod_barra_leitora_O:

    /* Para tributos segue o atual padrÆo */

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_barra_2
        as character
        format "99999.999999"
        no-undo.
    def output param p_cod_barra
        as character
        format "x(44)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_num_tam_format
        as integer
        format ">>9":U
        no-undo.
    def var v_val_tit_barra
        as decimal
        format "->>,>>>,>>>,>>9.99":U
        decimals 2
        no-undo.


    /************************** Variable Definition End *************************/

    assign v_val_tit_barra = 0.

    &if defined(BF_FIN_ALTER_CODIGO_BARRA) &then
        assign v_num_tam_format = 14.
    &else
        find ems5.histor_exec_especial no-lock
             where histor_exec_especial.cod_modul_dtsul = 'UFN'
               and histor_exec_especial.cod_prog_dtsul  = 'SPP_alter_codigo_barra'
             no-error.
        if   avail histor_exec_especial then
             assign v_num_tam_format = 14.
        else assign v_num_tam_format = 12.
    &endif   

    assign v_val_tit_barra = dec(substring(p_cod_barra_2, 38, 10))
           p_cod_barra = substring(p_cod_barra_2,01,11)
                       + substring(p_cod_barra_2, 13,11)
                       + substring(p_cod_barra_2, 25,11)
                       + substring(p_cod_barra_2, 37,11).
                       
END PROCEDURE. /* pi_retorna_cod_barra_leitora */


/*********************  End of fnc_extracao_banco_brasil ********************/

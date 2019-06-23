/****************************************************************************************** 
** 	   Programa: edf901zq.p
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 24/07/2018
** Change/Chamado: 
**      Objetivo: Banco do Brasil - Retorna a forma de lanáamento para o trailer do lote - posiá∆o de 12 a 13 
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: Mapa do EDI 200100
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

def var c-versao-prg as char initial " 1.00.00.010":U no-undo.

{include/i_dbinst.i}
{include/i_dbtype.i}

&IF "{&EMSFND_VERSION}" >= "1.00"
&THEN
{include/i-license-manager.i  fnc_id_bloco EDF}
&ENDIF

{include/i_fcldef.i}


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

def buffer bf_tt_param_program_formul for tt_param_program_formul.


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
def var v_cdn_forma_pagto as char.
def var v_cod_tit_ap as char.
/************************* Variable Definition Begin ************************/

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
def new global shared var v_cod_plano_ccusto_corren
    as character
    format "x(8)":U
    label "Plano CCusto"
    column-label "Plano CCusto"
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
def var v_nom_title_aux
    as character
    format "x(60)":U
    no-undo.
def new global shared var v_num_ped_exec_corren
    as integer
    format ">>>>9":U
    no-undo.
def var v_rec_log
    as recid
    format ">>>>>>9":U
    no-undo.


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

def new global shared var v_des_flag_public_geral
    as character
    format "x(15)":U
    extent 10
    no-undo.

def stream s-arq.

if  v_cod_arq <> '' and v_cod_arq <> ?
then do:
    run pi_version_extract ('fnc_id_bloco':U, 'prgint/edf/edf900zq.py':U, '1.00.00.010':U, 'pro':U).
end /* if */.
/* End_Include: i_version_extract */

/*
output to "c:\temp\cabec_bb_forma_lancto_2.txt".

     for each tt_param_program_formul:

        put unformatted 
            string(time,"hh:mm:ss") ';'
            tt_param_program_formul.tta_cdn_segment_edi                      ";"
            tt_param_program_formul.tta_cdn_element_edi                      ";"
            tt_param_program_formul.tta_des_label_utiliz_formul_edi          ";"
            tt_param_program_formul.ttv_des_contdo                    skip.
     end.

     put skip(02).

output close. 
*/
/* '01' = CrÇdito em Conta Corrente  '02' = Cheque Pagamento / Administrativo 
      '03' = DOC/TED (1) (2)            '04' = Cart∆o Sal†rio (somente para Tipo de Serviáo = '30') 
      '05' = CrÇdito em Conta Poupanáa  '10' = OP Ö Disposiá∆o 
      …11  = Pagamento de Contas e Tributos com C¢digo de Barras 
      …16  = Tributo - DARF Normal 
      …17  = Tributo - GPS (Guia da Previdància Social) …18  = Tributo - DARF Simples 
      …19  = Tributo - IPTU Œ Prefeituras '20' = Pagamento com Autenticaá∆o 
      …21  = Tributo Œ DARJ               …22  = Tributo - GARE-SP ICMS 
      …23  = Tributo - GARE-SP DR         …24  = Tributo - GARE-SP ITCMD 
      …25  = Tributo - IPVA               …26  = Tributo - Licenciamento 
      …27  = Tributo Œ DPVAT   */

find first tt_param_program_formul
    where tt_param_program_formul.tta_cdn_segment_edi = 0
    and   tt_param_program_formul.tta_cdn_element_edi = 0  no-error.
if avail tt_param_program_formul then do:

    case tt_param_program_formul.ttv_des_contdo:

        when 'Boleto' + 'Outros' /* l_boleto l_outros*/  then do:
            find first bf_tt_param_program_formul
                where bf_tt_param_program_formul.tta_cdn_segment_edi = 288
                and   bf_tt_param_program_formul.tta_cdn_element_edi = 3729 no-error.
            if avail bf_tt_param_program_formul and int(bf_tt_param_program_formul.ttv_des_contdo) = 20 
                then return "11". /* Tributo com c¢digo de barras */
            else do:
                find first bf_tt_param_program_formul                                                      
                    where bf_tt_param_program_formul.tta_cdn_segment_edi = 288                             
                    and   bf_tt_param_program_formul.tta_cdn_element_edi = 4838 no-error.                  
                if avail bf_tt_param_program_formul then do:
                  case int(bf_tt_param_program_formul.ttv_des_contdo):
                    when 16 then return "16". /* 16 a 24 - tributo sem c¢digo de barras */
                    when 17 then return "17".
                    when 18 then return "18".
                    when 21 then return "21".
                    when 22 then return "22".
                    when 23 then return "23".
                    when 24 then return "24".
                    otherwise return '31'.    /* Pagamento Fornecedor */
                  end case.
                end.
                else return '31'.
            end.
        end.
        when "Boleto" /*l_boleto*/  + "Banco" /*l_banco*/  then
            return "30".
        when "DOC" /*l_doc*/  then
            return "03".
        when "Credito C/C" /*l_credito_cc*/  then
            return "01".
        when "Cheque ADM" /*l_cheque_adm*/  then
            return "02".
        when "Ordem Pagto" /*l_ordem_pagto*/  then
            return "10".    
        when "TED CIP" /*l_ted_cip*/  then
            return "03".    
        when "TED STR" /*l_ted_str*/   then
            return "03".
        when "Pagto Tributos" /* l_Pagto Tributos*/  then do:

            find first bf_tt_param_program_formul                                                      
                where bf_tt_param_program_formul.tta_cdn_segment_edi = 288                             
                and   bf_tt_param_program_formul.tta_cdn_element_edi = 4838 no-error.                  
            if avail bf_tt_param_program_formul then 
                return bf_tt_param_program_formul.ttv_des_contdo.
            else RETURN "11". /* Verificar se Ç somente 11 */
        end.
    end.
end.


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
        message "Mensagem nr. " i_msg "!!!":U skip
                "Programa Mensagem" c_prg_msg "n∆o encontrado."
                view-as alert-box error.
        return error.
    end.

    run value(c_prg_msg + ".p":U) (input c_action, input c_param).
    return return-value.
END PROCEDURE.  /* pi_messages */
/***************************  End of fnc_id_bloco ***************************/

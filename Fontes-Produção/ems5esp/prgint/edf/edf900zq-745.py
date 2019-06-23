/****************************************************************************************** 
** 	   Programa: edf901zQ-237.p
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 24/07/2018
** Change/Chamado: 
**      Objetivo: Retorna a forma de lan�amento para o header do lote - posi��o de 12 a 13   e
**                A vers�o do layout posi��o 14 a 16 
**
******************************** CONTROLE DE ALTERA��ES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri��o da Altera��o
**
**
****************************** INFORMA��ES ADICIONAIS ************************************
** PAR�METROS DE ENTRADA: N/A
** PAR�METROS DE SA�DA: N/A
** CADASTRADO NO FONTE TOTVS: Mapa do EDI 200064
** CADASTRADO NA TABELA: N/A
******************************************************************************************/


/*-- Filtro Multi-idioma Aplicado --*/

def var c-versao-prg as char initial " 1.00.00.010":U no-undo.
def var c-versao-rcode as char initial "[[[1.00.00.010[[[":U no-undo. /* Controle de Versao R-CODE - Nao retirar do Fonte */
def var c_tipo_pagto as char format "x(02)".

{include/i_dbinst.i}
{include/i_dbtype.i}

{include/i_fcldef.i}

&IF "{&EMSFND_VERSION}" >= "1.00"
&THEN
{include/i-license-manager.i  fnc_id_bloco EDF}
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


def buffer bf-tt_param_program_formul for tt_param_program_formul.

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
    label "Usu�rio"
    column-label "Usu�rio"
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
    label "Grupo Usu�rios"
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
    label "M�dulo Corrente"
    column-label "M�dulo Corrente"
    no-undo.
def new global shared var v_cod_modul_dtsul_empres
    as character
    format "x(100)":U
    no-undo.
def new global shared var v_cod_pais_empres_usuar
    as character
    format "x(3)":U
    label "Pa�s Empresa Usu�rio"
    column-label "Pa�s"
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
    &if "{&FNC_MULTI_IDIOMA}" = "YES" &then
    list-item-pairs "",""
    &else
    list-items ""
    &endif
    inner-lines 5
    bgcolor 15 font 2
    label "Unidade Neg�cio"
    column-label "Unid Neg�cio"
    no-undo.
def new global shared var v_cod_usuar_corren
    as character
    format "x(12)":U
    label "Usu�rio Corrente"
    column-label "Usu�rio Corrente"
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

def var v_retorno as char no-undo.
def var v_cod_tit_ap as char.

def stream s-arq.

if  v_cod_arq <> '' and v_cod_arq <> ?
then do:
    run pi_version_extract ('fnc_id_bloco':U, 'prgint/edf/edf900zq-237.py':U, '1.00.00.010':U, 'pro':U).
end /* if */.
/* End_Include: i_version_extract */

/*
output to "c:\temp\cabec_irf.txt" append.

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


   /* '01' = Cr�dito em Conta Corrente  '02' = Cheque Pagamento / Administrativo 
      '03' = DOC/TED (1) (2)            '04' = Cart�o Sal�rio (somente para Tipo de Servi�o = '30') 
      '05' = Cr�dito em Conta Poupan�a  '10' = OP � Disposi��o 
      �11� = Pagamento de Contas e Tributos com C�digo de Barras 
      �16� = Tributo - DARF Normal 
      �17� = Tributo - GPS (Guia da Previd�ncia Social) �18� = Tributo - DARF Simples 
      �19� = Tributo - IPTU � Prefeituras '20' = Pagamento com Autentica��o 
      �21� = Tributo � DARJ               �22� = Tributo - GARE-SP ICMS 
      �23� = Tributo - GARE-SP DR         �24� = Tributo - GARE-SP ITCMD 
      �25� = Tributo - IPVA               �26� = Tributo - Licenciamento 
      �27� = Tributo � DPVAT   */



/* Tratamento para a forma de lan�amento e vers�o do layout */
find first tt_param_program_formul
    where tt_param_program_formul.tta_cdn_segment_edi = 0
    and   tt_param_program_formul.tta_cdn_element_edi = 0  no-error.
if avail tt_param_program_formul then do:

    find first reg_proces_entr_edi 
        where reg_proces_entr_edi.cdn_proces_edi  = int(v_des_flag_public_geral[1]) 
          and reg_proces_entr_edi.cdn_segment_edi = 289 no-lock no-error.
    if avail reg_proces_entr_edi then do:
           assign v_cod_tit_ap = entry(21,reg_proces_entr_edi.dsl_dados_entr_edi,chr(24)).

           if TRIM(entry(3,v_cod_tit_ap,';')) = "N" /*l_n*/ then 
           do:
                find first item_bord_ap no-lock
                     where item_bord_ap.cod_estab_bord = TRIM(entry(1,v_cod_tit_ap,';'))
                       and item_bord_ap.num_id_item_bord_ap = int(entry(2,v_cod_tit_ap,';')) no-error.
           end.
           else do: /* Qdo movimento � agrupado */
                find first item_bord_ap_agrup no-lock
                     where item_bord_ap_agrup.cod_estab_bord            =  TRIM(entry(1,v_cod_tit_ap,';'))
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
               assign v_retorno = item_bord_ap.cod_forma_pagto when avail item_bord_ap.
    end.
    else do:
           find first bf-tt_param_program_formul where bf-tt_param_program_formul.tta_cdn_segment_edi = 288
                                                   and bf-tt_param_program_formul.tta_cdn_element_edi = 4838 no-error.
           if avail bf-tt_param_program_formul then
              assign v_retorno = bf-tt_param_program_formul.ttv_des_contdo.
    end. 

    CASE v_retorno:
                
         WHEN "11" THEN assign v_retorno = v_retorno + "012".
         WHEN "16" THEN assign v_retorno = v_retorno + "012". /* DARF Normal - N2 */ 
         WHEN "17" THEN assign v_retorno = v_retorno + "012". /* Pagamento GPS - N1*/ 
         WHEN "18" THEN assign v_retorno = "16012". /* DARF Simples */ 
         WHEN "19" THEN assign v_retorno = v_retorno + "012". /* DARF Simples */ 
         WHEN "21" THEN assign v_retorno = v_retorno + "012". /* DARF Simples */ 
         WHEN "22" THEN assign v_retorno = v_retorno + "012".                                      
         WHEN "23" THEN assign v_retorno = v_retorno + "012".                                     
         WHEN "24" THEN assign v_retorno = v_retorno + "012". /* GARE-SP (ICMS/DR/ITCMD) N4 */  
         WHEN "25" THEN assign v_retorno = v_retorno + "012".
         WHEN "26" THEN assign v_retorno = v_retorno + "012".
         WHEN "27" THEN assign v_retorno = v_retorno + "012".
         WHEN "40" THEN assign v_retorno = "11012". /* tributos com c�digo de barras - FGTS */

         OTHERWISE DO:
             CASE tt_param_program_formul.ttv_des_contdo:
                when "BoletoOutros" /*l_outros*/   then
                     v_retorno = "31031".
                when "BoletoBanco" /*l_banco*/   then
                     v_retorno = "31031".
                when "DOC" /*l_doc*/   then
                     v_retorno = "03031".
                when "Cr�dito C/C" /*l_credito_cc*/  then
                     v_retorno = "01031".
                when "Cheque ADM" /*l_cheque_adm*/  then
                     v_retorno = "02031".
                when "Ordem Pagto" /*l_ordem_pagto*/ then
                     v_retorno = "98031". 
                when 'TED CIP' /* l_ted_cip*/   then
                     v_retorno = "03031".
                when 'TED STR' /* l_ted_str*/   then
                     v_retorno = "03031".     
                when "Pagto Tributos" /* l_Pagto Tributos*/  
                    then assign v_retorno = v_retorno + "012".
             END CASE.
         END.
    end case.
end.

return v_retorno.



                

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
                "Programa Mensagem" c_prg_msg "n�o encontrado."
                view-as alert-box error.
        return error.
    end.

    run value(c_prg_msg + ".p":U) (input c_action, input c_param).
    return return-value.
END PROCEDURE.  /* pi_messages */
/***************************  End of fnc_id_bloco ***************************/


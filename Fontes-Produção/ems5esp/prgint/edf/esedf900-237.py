/*****************************************************************************
** Copyright DATASUL S.A. (1994)
** Todos os Direitos Reservados.
** 
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so' podera ser feita mediante
** autorizacao expressa.
**
** Programa..............: fnc_monta_banco_agencia_conta
** Descricao.............: Fun‡äes
** Versao................:  1.00.00.009
** Procedimento..........: utl_formula_edi
** Nome Externo..........: prgint/edf/edf900zy.py
** Data Geracao..........: 04/08/2006 - 13:06:25
** Criado por............: bre18490
** Criado em.............: 21/01/2000 11:53:29
** Alterado por..........: tech14116
** Alterado em...........: 26/03/2003 11:33:55
** Gerado por............: tech14020
*****************************************************************************/

def var c-versao-prg as char initial " 1.00.00.009":U no-undo.

{include/i_dbinst.i}
{include/i_dbtype.i}

&IF "{&EMSFND_VERSION}" >= "1.00"
&THEN
{include/i-license-manager.i  fnc_monta_banco_agencia_conta EDF}
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
          tta_cdn_element_edi              ascending
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

def var v_cdn_agenc
    as Integer
    format ">>>>>>>>>>>>9":U
    no-undo.
def var v_cdn_digito
    as Integer
    format ">>>,>>9":U
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
def new global shared var v_cod_aplicat_dtsul_corren
    as character
    format "x(3)":U
    no-undo.
def var v_cod_barra_1
    as character
    format "99999.99999":U
    no-undo.
def var v_cod_barra_2
    as character
    format "99999.999999":U
    no-undo.
def var v_cod_barra_3
    as character
    format "99999.999999":U
    no-undo.
def var v_cod_barra_4
    as character
    format "9":U
    no-undo.
def var v_cod_barra_5
    as character
    format "99999999999999":U
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
def var v_cod_det
    as character
    format "x(308)":U
    no-undo.
def var v_cod_digito
    as character
    format "x(8)":U
    no-undo.
def var v_cod_digito_1
    as character
    format "x(02)":U
    no-undo.
def var v_cod_digito_cta_corren
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
    label "Usu rio Corrente"
    column-label "Usu rio Corrente"
    no-undo.
def new global shared var v_cod_usuar_corren_criptog
    as character
    format "x(16)":U
    no-undo.
def var v_nom_title_aux
    as character
    format "x(60)":U
    no-undo.
def var v_num_digito
    as integer
    format "9":U
    no-undo.
def new global shared var v_num_ped_exec_corren
    as integer
    format ">>>>9":U
    no-undo.
def var v_rec_log
    as recid
    format ">>>>>>9":U
    no-undo.
def var v_val_cm
    as decimal
    format "->>>>>,>>>,>>9.99":U
    decimals 4
    label "Corre‡Æo Monet ria"
    column-label "Corre‡Æo Monet ria"
    no-undo.
def var v_val_cta_arq
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 0
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
    no-undo.
def var v_val_resto
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    no-undo.
def var v_cdn_bco                        as integer         no-undo. /*local*/


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
    run pi_version_extract ('fnc_monta_banco_agencia_conta':U, 'prgint/edf/edf900zy.py':U, '1.00.00.009':U, 'pro':U).
end /* if */.
/* End_Include: i_version_extract */

/* Mapa Banco Bradesco */

if  p_cdn_mapa_edi <> 200112 then next.

/* ** Valor do Acr‚scimo ***/
if p_cdn_element_edi = 4659 then do:
    /* ** Juros ***/
    find tt_param_program_formul
        where tt_param_program_formul.tta_cdn_segment_edi = 289
        and   tt_param_program_formul.tta_cdn_element_edi = 4422 no-error.
    if avail tt_param_program_formul then
        assign v_val_juros = dec(tt_param_program_formul.ttv_des_contdo).

    /* ** Multa ***/
    find tt_param_program_formul
        where tt_param_program_formul.tta_cdn_segment_edi = 289
        and   tt_param_program_formul.tta_cdn_element_edi = 4426 no-error.
    if avail tt_param_program_formul then
        assign v_val_multa = dec(tt_param_program_formul.ttv_des_contdo).

    /* ** Corre‡Æo Monet ria ***/
    find tt_param_program_formul
        where tt_param_program_formul.tta_cdn_segment_edi = 289
        and   tt_param_program_formul.tta_cdn_element_edi = 4437 no-error.
    if avail tt_param_program_formul then
        assign v_val_cm = dec(tt_param_program_formul.ttv_des_contdo).

    assign v_cod_det = string(v_val_juros + v_val_multa + v_val_cm).
end.
else do:
    /* * Tipo de Pagamento **/
    find tt_param_program_formul
        where tt_param_program_formul.tta_cdn_segment_edi = 289
        and   tt_param_program_formul.tta_cdn_element_edi = 3729 no-error.
    assign v_cdn_tip_forma_pagto = int(tt_param_program_formul.ttv_des_contdo).

    /* ** C¢digo de Barras ***/
    find tt_param_program_formul
        where tt_param_program_formul.tta_cdn_segment_edi = 289
        and   tt_param_program_formul.tta_cdn_element_edi = 2807 no-error.    
    assign v_cod_barra_1 = substring(tt_param_program_formul.ttv_des_contdo,1,10)
           v_cod_barra_2 = substring(tt_param_program_formul.ttv_des_contdo,11,11)
           v_cod_barra_3 = substring(tt_param_program_formul.ttv_des_contdo,22,11)
           v_cod_barra_4 = substring(tt_param_program_formul.ttv_des_contdo,33,1)
           v_cod_barra_5 = substring(tt_param_program_formul.ttv_des_contdo,34,14).

    if p_cdn_element_edi = 6184 then do :
        /* * Banco **/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 3737 no-error.
        assign v_cdn_bco = int(tt_param_program_formul.ttv_des_contdo).

        /* * Agencia **/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 3922 no-error.
        assign v_cdn_agenc = int(tt_param_program_formul.ttv_des_contdo).

        /* * Digito Agencia **/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 5143 no-error.
        assign v_cod_digito_1 = tt_param_program_formul.ttv_des_contdo.

        /* * Conta Corrente **/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 3796 no-error.
        assign v_val_cta_arq = dec(tt_param_program_formul.ttv_des_contdo).

        /* * Digito Conta Corrente **/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 3927 no-error.
        assign v_cod_digito_cta_corren = tt_param_program_formul.ttv_des_contdo.

        /* para Ordem de Pagamento grava banco fixo 237 e conta e digito = 0 */
        if  v_cdn_tip_forma_pagto = 6 then
            assign v_cdn_bco    = 237 
                   v_val_cta_arq  = 0
                   v_cod_digito_cta_corren = '00'. 

        if v_cdn_tip_forma_pagto = 1
        or v_cdn_tip_forma_pagto = 5 then do:
            assign v_cdn_bco = int(string(substring(v_cod_barra_1,1,3),'999')).
        end.

        if (v_cdn_tip_forma_pagto = 1
        and substring(v_cod_barra_1,1,3) = "237")
        or v_cdn_tip_forma_pagto = 5 then do:
            run pi_calcula_digito_verificador (Input int(substr(v_cod_barra_1,5,4)),
                                               output v_num_digito,
                                               output v_val_resto) /*pi_calcula_digito_verificador*/.
            assign v_cod_digito     = string(v_num_digito,"9").
                   v_cod_agenc_bcia = '00' + substr(v_cod_barra_1,5,4) + v_cod_digito.

            run pi_calcula_digito_verificador (Input int(substr(v_cod_barra_3,3,7)),
                                               output v_num_digito,
                                               output v_val_resto) /*pi_calcula_digito_verificador*/.
            if v_val_resto = 1 then
                assign v_cod_digito = 'P'.
            else assign v_cod_digito = string(v_num_digito,"9").

            assign v_cod_cta_corren = '00' + substr(v_cod_barra_3,3,7) + v_cod_digito
                   v_cod_det = string(v_cdn_bco,'999')
                             + string(int(substr(v_cod_agenc_bcia,2,5)),"99999")
                             + string(substr(v_cod_agenc_bcia,7,1),'x')
                             + string(int(substr(v_cod_cta_corren,1,9)),"9999999999999")
                             + string(substr(v_cod_cta_corren,10,2),'xx').

        end.
        else do:
            if (v_cdn_tip_forma_pagto = 1
            and substring(v_cod_barra_1,1,3) <> "237") then do:
                assign v_cdn_agenc     = 0
                       v_cod_digito_1  = ""
                       v_val_cta_arq   = 0
                       v_cod_digito_cta_corren = '00'.
            end.

            if length(string(v_cod_digito_1),'character') = 2 then
                assign v_cod_det = string(v_cdn_bco,'999') +
                                   string(v_cdn_agenc,'99999') +
                                   string(v_cod_digito_1,'X(2)') +
                                   string(v_val_cta_arq,'9999999999999') + 
                                   string(trim(v_cod_digito_cta_corren),'XX').
            else
                assign v_cod_det = string(v_cdn_bco,'999') +
                               string(v_cdn_agenc,'99999') +
                               string(v_cod_digito_1,'X(1)') +
                               string(v_val_cta_arq,'9999999999999') + 
                               string(trim(v_cod_digito_cta_corren),'XX').
        end.
    end.    

    /* ** Carteira Posi‡Æo 136 - 138 ***/
    if p_cdn_element_edi = 4711 then do:
        if (v_cdn_tip_forma_pagto = 1
        and substring(v_cod_barra_1,1,3) = "237")
        or v_cdn_tip_forma_pagto = 5 then do:
            assign v_cod_det = '0' + substring(v_cod_barra_1,9,1) + substring(v_cod_barra_2,1,1).
        end.
        else
            assign v_cod_det = string(fill('0',3)).
    end.

    /* ** Ano Nosso N£mero e nosso numero 139 - 150 ***/
    if p_cdn_element_edi = 6209 then do:
        if (v_cdn_tip_forma_pagto = 1
        and substring(v_cod_barra_1,1,3) = "237")
        or v_cdn_tip_forma_pagto = 5 then
            assign v_cod_det = '0' + substring(v_cod_barra_2,2,9) + substring(v_cod_barra_3,1,2).
        else
            assign v_cod_det = string(fill('0',12)).
    end.

    /* Valor nominal do t¡tulo 190 - 204 */
    if p_cdn_element_edi = 4657 then do:
        if v_cdn_tip_forma_pagto = 1
        or v_cdn_tip_forma_pagto = 5 then
            assign v_cod_det = '00' + substring(v_cod_barra_5,1,14).
        else do:
            /* * Localiza o valor do t¡tulo **/
            find tt_param_program_formul
                where tt_param_program_formul.tta_cdn_segment_edi = 289
                and   tt_param_program_formul.tta_cdn_element_edi = 4421 no-error.
            assign v_cod_det = string(tt_param_program_formul.ttv_des_contdo).
        end.
    end.

    /* ** Informa‡äes Complementares 374 - 413 ***/
    if p_cdn_element_edi = 2961 then do:
        if v_cdn_tip_forma_pagto = 1
        or v_cdn_tip_forma_pagto = 5 then do:
            assign v_cod_det = substring(v_cod_barra_1,5,5)
                             + substring(v_cod_barra_2,1,10)
                             + substring(v_cod_barra_3,1,10)
                             + v_cod_barra_4
                             + substring(v_cod_barra_1,4,1).
        end.
        if v_cdn_tip_forma_pagto = 2 then
            assign v_cod_det = 'C0000000101'. /* acrescenta o Tipo de Conta */

        if v_cdn_tip_forma_pagto = 7
        or v_cdn_tip_forma_pagto = 8 then
            assign v_cod_det = 'C0000000101'. /* acrescenta o Tipo de Conta */

        /* * Tipo de Pagamento **/
        find tt_param_program_formul
            where tt_param_program_formul.tta_cdn_segment_edi = 289
            and   tt_param_program_formul.tta_cdn_element_edi = 3729 no-error.
        assign v_cdn_tip_forma_pagto = int(tt_param_program_formul.ttv_des_contdo).
    end.
end.

return string(v_cod_det).



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
** Procedure Interna.....: pi_calcula_digito_verificador
** Descricao.............: pi_calcula_digito_verificador
** Criado por............: Claudia
** Criado em.............: 13/09/2000 11:55:09
** Alterado por..........: Claudia
** Alterado em...........: 13/09/2000 18:15:25
*****************************************************************************/
PROCEDURE pi_calcula_digito_verificador:

    /************************ Parameter Definition Begin ************************/

    def Input param p_val_campo
        as decimal
        format "->>,>>>,>>>,>>9.99"
        decimals 2
        no-undo.
    def output param p_num_digito
        as integer
        format "9"
        no-undo.
    def output param p_val_resto
        as decimal
        format "->>,>>>,>>>,>>9.99"
        decimals 2
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_num_cont
        as integer
        format ">,>>9":U
        initial 0
        no-undo.
    def var v_val_soma_digito
        as decimal
        format "->>,>>>,>>>,>>9.99":U
        decimals 2
        no-undo.
    def var v_num_indice                     as integer         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    /* ** Esta procedure calcula o d¡gito verificador no M¢dulo 11, base 7 (Bradesco) ***/

    assign v_num_indice = 2.
    repeat v_num_cont = 1 to int(length(string(p_val_campo))):
        if length(string(p_val_campo)) - v_num_cont > 0
        and v_num_cont = 7 then
            assign v_num_indice = 2.
        assign v_val_soma_digito = v_val_soma_digito + int(substr(string(p_val_campo),length(string(p_val_campo))
                             - v_num_cont + 1,1)) * v_num_indice.
        assign v_num_indice = v_num_indice + 1.
    end.

    assign p_num_digito = absolute(11 - v_val_soma_digito modulo 11)
           p_val_resto  = v_val_soma_digito modulo 11.

    if p_num_digito = 10
    or p_num_digito = 11 then
        assign p_num_digito = 0.       
END PROCEDURE. /* pi_calcula_digito_verificador */


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
                "Programa Mensagem" c_prg_msg "nÆo encontrado."
                view-as alert-box error.
        return error.
    end.

    run value(c_prg_msg + ".p":U) (input c_action, input c_param).
    return return-value.
END PROCEDURE.  /* pi_messages */
/*******************  End of fnc_monta_banco_agencia_conta ******************/

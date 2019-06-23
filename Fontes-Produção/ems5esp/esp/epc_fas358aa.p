/*****************************************************************************
** Copyright DATASUL S.A. (1994)
** Todos os Direitos Reservados.
** 
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so' podera ser feita mediante
** autorizacao expressa.
**
** Programa..............: upc_rpt_bem_pat_diario_aux
** Descricao.............: 
** 1.00.00.001 - Tratar registro de invent rio ado‡Æo inicial para o di rio auxiliar FAS
** Nome Externo..........: esp/epc_fas358aa.p
** Criado por............:
** Criado em.............: 20/10/2016
** Programa Chamador.....: prgfin/fas/fas358aa.p
*****************************************************************************/

def var c-versao-prg as char initial " 5.06.00.001":U no-undo.

{include/i_dbinst.i}
{include/i_dbtype.i}

/******************************* Private-Data *******************************/
assign this-procedure:private-data = "HLP=0":U.
/*************************************  *************************************/

/********************* Temporary Table Definition Begin *********************/

/********************** Temporary Table Definition End **********************/

/****************************** Parameter Begin *****************************/

def input param p_ind_event     as char             no-undo.
def input param p_ind_object    as char             no-undo.
def input param p_wgh_object    as handle           no-undo.
def input param p_wgh_frame     as widget-handle    no-undo.
def input param p_cod_table     as char             no-undo.
def input param p_rec_table     as recid            no-undo.

/******************************* Parameter End ******************************/

/******************************* Variable Begin *****************************/

def new global shared var v_cod_arq
    as char  
    format 'x(60)'
    no-undo.

def new global shared var v_cod_tip_prog
    as character
    format 'x(8)'
    no-undo.

def var v_h_query                       as handle        no-undo.
def var v_bh_tt_diar_aux_pat            as widget-handle no-undo.
def var v_bh_tta_num_id_bem_pat         as widget-handle no-undo.
def var v_bh_ttv_des_narrat_bem_pat_tmp as widget-handle no-undo.

/******************************** Variable End ******************************/

/****************************** Main Code Begin *****************************/

def stream s-arq.

if  v_cod_arq <> '' and v_cod_arq <> ?
then do:
    run pi_version_extract ('upc_rpt_bem_pat_diario_aux':U, 'esp/epc_fas358aa.p':U, '5.06.00.001':U, 'pro':U).
end.


if  p_ind_event = "control_adoc_inicial" then do:

    find first adoc_inicial no-lock
        where  recid(adoc_inicial) = p_rec_table no-error.
    if  avail adoc_inicial
    and adoc_inicial.ind_movto_adoc_inicial = "Implanta‡Æo":U then do:

        find first esp_adoc_inicial_invent no-lock
            where  esp_adoc_inicial_invent.cod_empresa                 = adoc_inicial.cod_empresa
            and    esp_adoc_inicial_invent.cod_cenar_ctbl              = adoc_inicial.cod_cenar_ctbl
            and    esp_adoc_inicial_invent.cod_finalid_econ            = adoc_inicial.cod_finalid_econ
            and    esp_adoc_inicial_invent.cod_cta_pat_dest            = adoc_inicial.cod_cta_pat
            and    esp_adoc_inicial_invent.num_bem_pat_dest            = adoc_inicial.num_bem_pat
            and    esp_adoc_inicial_invent.num_seq_bem_pat_dest        = adoc_inicial.num_seq_bem_pat
            and    esp_adoc_inicial_invent.num_seq_incorp_bem_pat_dest = adoc_inicial.num_seq_incorp_bem_pat no-error.
        if  avail esp_adoc_inicial_invent then do:

            find first bem_pat no-lock
                where bem_pat.cod_empresa     = adoc_inicial.cod_empresa
                and   bem_pat.cod_cta_pat     = adoc_inicial.cod_cta_pat
                and   bem_pat.num_bem_pat     = adoc_inicial.num_bem_pat
                and   bem_pat.num_seq_bem_pat = adoc_inicial.num_seq_bem_pat no-error.
            if  avail bem_pat then do:
    
                assign v_bh_tt_diar_aux_pat            = p_wgh_frame:default-buffer-handle
                       v_bh_tta_num_id_bem_pat         = v_bh_tt_diar_aux_pat:buffer-field("tta_num_id_bem_pat")
                       v_bh_ttv_des_narrat_bem_pat_tmp = v_bh_tt_diar_aux_pat:buffer-field("ttv_des_narrat_bem_pat_tmp").
        
                if  not valid-handle(v_bh_tta_num_id_bem_pat)
                or  not valid-handle(v_bh_ttv_des_narrat_bem_pat_tmp) then return "NOK".
    
                create query v_h_query.
                v_h_query:add-buffer(v_bh_tt_diar_aux_pat).
                v_h_query:query-prepare("for each  " + v_bh_tt_diar_aux_pat:name + chr(10) +
                                        "    where " + v_bh_tt_diar_aux_pat:name + ".tta_num_id_bem_pat     = " + quoter(bem_pat.num_id_bem_pat) + chr(10) +
                                        "    and   " + v_bh_tt_diar_aux_pat:name + ".ttv_des_narrat_bem_pat_tmp = " + quoter("Implanta‡Æo Ado‡Æo Inicial")).
                v_h_query:query-open.
                v_h_query:get-first().

                do  while not v_h_query:query-off-end:
                    v_bh_tt_diar_aux_pat:buffer-delete.
                    v_h_query:get-next.
                end.

                v_h_query:query-close().
                delete object v_h_query.
            end.
        end.
    end.
end.

return 'OK'.

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

/*****************************************************************************
** Copyright DATASUL S.A. (1994)
** Todos os Direitos Reservados.
** 
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so' podera ser feita mediante
** autorizacao expressa.
**
** Programa..............: epc_fas341aa_aux.p
** Descricao.............: C†lculos Per°odo - EPC - AUX
** Versao................:  1.00.00.000
** Nome Externo..........: prgfin/epc/epc_fas341aa_aux.p
** Criado por............: Duane
** Criado em.............: 14/12/2012
** Ficha de Ocorrància...: TGDIFV
** Programa chamador.....: prgfin/epc/epc_fas341aa.p
** Observaá∆o............: O objetivo Ç exportar o relat¢rio em CSV
**
*****************************************************************************/

def var c-versao-prg as char initial " 1.00.00.000":U no-undo.

{include/i_dbinst.i}
{include/i_dbtype.i}

/************************** Stream Definition Begin *************************/

def stream s-arq.


/*************************** Stream Definition End **************************/

/************************* Variable Definition Begin ************************/

def var v_hdl_funcao_padr
    as Handle
    format ">>>>>>9":U
    no-undo.
def shared var v_cod_dwb_program
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
def new global shared var v_cod_usuar_corren
    as character
    format "x(12)":U
    label "Usu†rio Corrente"
    column-label "Usu†rio Corrente"
    no-undo.

def new global shared var v_cod_arq as char format 'x(60)' no-undo.

def new global shared var h_bt_print          as handle no-undo.
def new global shared var h_fl_dest_csv         as handle no-undo.
def new global shared var h_tg_dest_csv         as handle no-undo.
def new global shared var h_rs_ind_run_mode   as handle no-undo.
def new global shared var h_query              as handle no-undo.
def new global shared var h_incorp_query              as handle no-undo.

def shared var v_hdl_tt_rpt_bem_pat_calc_period             as handle no-undo.
def shared var v_hdl_tt_rpt_bem_pat_calc_incorp   as handle no-undo.

/*Period*/
def var h_cod_cta_pat                as handle no-undo.
def var h_cod_estab                  as handle no-undo.
def var h_cod_unid_negoc             as handle no-undo.
def var h_cod_plano_ccusto           as handle no-undo.
def var h_cod_ccusto_respons         as handle no-undo.
def var h_num_bem_pat                as handle no-undo.
def var h_num_seq_bem_pat            as handle no-undo.
def var h_dat_aquis_bem_pat          as handle no-undo.
def var h_val_origin_corrig          as handle no-undo.
def var h_val_resid_min              as handle no-undo.
def var h_val_cm                     as handle no-undo.
def var h_val_perc_anual_dpr         as handle no-undo.
def var h_val_perc_dpr_acum          as handle no-undo.
def var h_val_perc_amort_acum        as handle no-undo.
def var h_val_dpr_val_origin         as handle no-undo.
def var h_val_dpr_val_origin_amort   as handle no-undo.
def var h_val_dpr_cm                 as handle no-undo.
def var h_val_dpr_cm_amort           as handle no-undo.
def var h_val_cm_dpr                 as handle no-undo.
def var h_val_cm_dpr_amort           as handle no-undo.
def var h_val_cr_pis_rel             as handle no-undo.
def var h_val_cr_cofins_rea          as handle no-undo.
def var h_val_cr_csll_rea            as handle no-undo.
def var h_val_dpr                    as handle no-undo.
def var h_val_amort                  as handle no-undo.
def var h_val_tot_dpr                as handle no-undo.
def var h_val_dpr_amort              as handle no-undo.
def var h_val_dpr_amort_cm           as handle no-undo.
def var h_v_val_cm_dpr_amort         as handle no-undo.
def var h_num_id_bem_pat             as handle no-undo.

/*Chave*/
DEFINE VARIABLE v_num_id_bem_pat AS INTEGER     NO-UNDO.

/*Incorp*/
def var h_num_seq_incorp_bem_pat         as handle no-undo.
def var h_ind_incorp_bem_pat             as handle no-undo.
def var h_dat_incorp_bem_pat             as handle no-undo.
def var h_inc_val_origin_corrig          as handle no-undo.
def var h_inc_val_resid_min              as handle no-undo.
def var h_inc_val_cm                     as handle no-undo.
def var h_inc_val_perc_anual_dpr         as handle no-undo.
def var h_inc_val_perc_dpr_acum          as handle no-undo.
def var h_inc_val_perc_amort_acum        as handle no-undo. 
def var h_inc_val_dpr_val_origin         as handle no-undo.
def var h_inc_val_dpr_val_origin_amort   as handle no-undo.
def var h_inc_val_dpr_cm                 as handle no-undo.
def var h_inc_val_dpr_cm_amort           as handle no-undo.
def var h_inc_val_cm_dpr                 as handle no-undo.
def var h_inc_val_cm_dpr_amort           as handle no-undo.
def var h_inc_val_cr_pis_rel             as handle no-undo.
def var h_inc_val_cr_cofins_rea          as handle no-undo.
def var h_inc_val_cr_csll_rea            as handle no-undo.
def var h_inc_val_dpr                    as handle no-undo.
def var h_inc_val_amort                  as handle no-undo.
def var h_inc_val_tot_dpr                as handle no-undo.
def var h_inc_val_dpr_amort              as handle no-undo.
def var h_inc_val_dpr_amort_cm           as handle no-undo.
def var h_inc_v_val_cm_dpr_amort         as handle no-undo.
def var h_inc_num_id_bem_pat             as handle no-undo.

DEFINE VARIABLE v_calc_pis_cof_cls AS CHARACTER   NO-UNDO.


/************************** Variable Definition End *************************/

/****************************** Main Code Begin *****************************/  

/* listar impress∆o extrato de vers∆o */
if  v_cod_arq <> '' and v_cod_arq <> ? then do: 
    output stream s-arq to value(v_cod_arq) append.
    
        put stream s-arq unformatted
            "Chamada epc para tratamento especifico" at 1 
            "prgfin/epc/epc_fas341aa_aux.p"        at 43 
            "1.00.00.000"               at 69 
            today                       at 84 format "99/99/99"
            string(time, 'HH:MM:SS')    at 94 skip.
    
    output stream s-arq close.
end.

run prgint/utb/utb922za.py persistent set v_hdl_funcao_padr /* prg_fnc_funcoes_padrao*/.

FUNCTION setEntryField      RETURN CHARACTER (input p_num_posicao as int, input p_cod_campo as char, input p_cod_separador as char, input p_cod_valor as char) in v_hdl_funcao_padr.
FUNCTION getEntryField      RETURN CHARACTER (input p_num_posicao as int, input p_cod_campo as char, input p_cod_separador as char) in v_hdl_funcao_padr.

/******************************* Main Code End ******************************/

procedure pi_save_params:

    find dwb_rpt_param exclusive-lock
         where dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
           and dwb_rpt_param.cod_dwb_user    = v_cod_dwb_user /*cl_dwb_rpt_param of dwb_rpt_param*/ no-error.

    if h_tg_dest_csv:screen-value = "yes" then
        assign dwb_rpt_param.cod_dwb_parameters = setentryfield(18, dwb_rpt_param.cod_dwb_parameters, chr(10), h_fl_dest_csv:screen-value).
    else 
        assign dwb_rpt_param.cod_dwb_parameters = setentryfield(18, dwb_rpt_param.cod_dwb_parameters, chr(10), "").

    release dwb_rpt_param.
end.

procedure pi_get_params:
    find dwb_rpt_param no-lock
         where dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
           and dwb_rpt_param.cod_dwb_user    = v_cod_dwb_user /*cl_dwb_rpt_param of dwb_rpt_param*/ no-error.

    if getentryfield(18, dwb_rpt_param.cod_dwb_parameters, chr(10)) <> "" then 
        assign h_tg_dest_csv:screen-value = "yes"
               h_fl_dest_csv:screen-value = getentryfield(18, dwb_rpt_param.cod_dwb_parameters, chr(10)).
end.

procedure pi_choose_bt_print_epc:

    if not v_cod_dwb_user begins 'es_'
   and valid-handle (h_fl_dest_csv)
   and valid-handle (h_rs_ind_run_mode) then do:

        apply "leave" to h_fl_dest_csv.
        apply "leave" to h_rs_ind_run_mode.
    end.

    run pi_save_params.

    apply "choose" to h_bt_print.
end.

procedure pi_leave_rs_ind_run_mode:
    if h_rs_ind_run_mode:screen-value = "Batch" then do:
        
        if h_fl_dest_csv:screen-value <> "" then do:

            assign h_fl_dest_csv:screen-value = replace(h_fl_dest_csv:screen-value, '~\', '~/').
            assign h_fl_dest_csv:screen-value = entry(num-entries(h_fl_dest_csv:screen-value, '/'), h_fl_dest_csv:screen-value, '/').
        end.
    end.

end.

procedure pi_choose_bt_get_file_csv:

    def var v_cod_arq as char no-undo. /* local */

    system-dialog get-file v_cod_arq
        title "Imprimir" /*l_imprimir*/ 
        filters '*.csv' '*.csv',
                "*.*"   "*.*"
        save-as
        create-test-file
        ask-overwrite.

    assign h_fl_dest_csv:screen-value = v_cod_arq.
end.

procedure pi_leave_fl_dest_csv:

    def var v_cod_filename_initial as char no-undo.
    def var v_cod_filename_final   as char no-undo.

    if h_tg_dest_csv:screen-value = "yes" then do:

        if  h_rs_ind_run_mode:screen-value <> "Batch" /*l_batch*/ then do:

            if  h_fl_dest_csv:screen-value <> "" then do:

                assign h_fl_dest_csv:screen-value = replace(h_fl_dest_csv:screen-value, '~\', '/')
                       v_cod_filename_initial   = entry(num-entries(h_fl_dest_csv:screen-value, '/'), h_fl_dest_csv:screen-value, '/')
                       v_cod_filename_final     = substring(h_fl_dest_csv:screen-value, 1, length(h_fl_dest_csv:screen-value) - length(v_cod_filename_initial) - 1)
                       file-info:file-name      = v_cod_filename_final.

                if  file-info:file-type = ? then do:

                     /* O diretΩrio &1 nío existe ! */
                     run pi_messages (input "show",
                                      input 4354,
                                      input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                                         v_cod_filename_final)) /*msg_4354*/.
                     return no-apply.
                end /* if */.
            end /* if */.
        end /* if */.
        else do:
            
            run pi_filename_validation(input h_fl_dest_csv:screen-value).
            if return-value <> "OK" then do:
                apply "entry" to h_fl_dest_csv.
                /* Nome do arquivo incorreto ! */
                 run pi_messages (input "show",
                                  input 1064,
                                  input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9")) /*msg_1064*/.
                return no-apply.
            end.
        end.
    end.
end.

procedure pi_antes_print_csv:
    
    def var v_cod_arq_csv   as char no-undo.

    find dwb_rpt_param no-lock
         where dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
           and dwb_rpt_param.cod_dwb_user    = v_cod_dwb_user /*cl_dwb_rpt_param of dwb_rpt_param*/ no-error.

    if index( dwb_rpt_param.cod_dwb_file ,'~\') <> 0 then
        assign file-info:file-name = replace(dwb_rpt_param.cod_dwb_file, '~\', '~/').
    else
        assign file-info:file-name = dwb_rpt_param.cod_dwb_file.

    assign file-info:file-name = substring(file-info:file-name, 1,
                                 r-index(file-info:file-name, '~/') - 1).


    if  v_cod_dwb_user begins 'es_' then do:
        assign v_cod_arq_csv = file-info:full-pathname + "~/" + getentryfield(18, dwb_rpt_param.cod_dwb_parameters, chr(10)).
    end.
    else do:
        assign v_cod_arq_csv = getentryfield(18, dwb_rpt_param.cod_dwb_parameters, chr(10)).
    end.

    if file-info:full-pathname <> ? then do:
        os-delete value(v_cod_arq_csv) no-error.
    end.

end.

procedure pi_print_csv:

    def var v_cod_arq_csv        as char no-undo.

    find dwb_rpt_param no-lock
         where dwb_rpt_param.cod_dwb_program = v_cod_dwb_program
           and dwb_rpt_param.cod_dwb_user    = v_cod_dwb_user /*cl_dwb_rpt_param of dwb_rpt_param*/ no-error.

    assign v_calc_pis_cof_cls = "no".

    if getentryfield(12, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes"
    or getentryfield(11, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes"
    or getentryfield(16, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" then

        assign v_calc_pis_cof_cls = "yes".

    if index( dwb_rpt_param.cod_dwb_file ,'~\') <> 0 then
        assign file-info:file-name = replace(dwb_rpt_param.cod_dwb_file, '~\', '~/').
    else
        assign file-info:file-name = dwb_rpt_param.cod_dwb_file.

    assign file-info:file-name = substring(file-info:file-name, 1,
                                 r-index(file-info:file-name, '~/') - 1).

    if  v_cod_dwb_user begins 'es_' then do:
        assign v_cod_arq_csv = file-info:full-pathname + "~/" + getentryfield(18, dwb_rpt_param.cod_dwb_parameters, chr(10)).
    end.
    else do:
        assign v_cod_arq_csv = getentryfield(18, dwb_rpt_param.cod_dwb_parameters, chr(10)).
    end.

    if (file-info:full-pathname <> ? or not v_cod_dwb_user begins 'es_')
   and getentryfield(18, dwb_rpt_param.cod_dwb_parameters, chr(10)) <> "" then do:

        output to value(v_cod_arq_csv) no-convert.
  
        put unformat
            "Conta Pat;Estab;Un N;Pl CCusto;CCusto Respons;Bem Pat;Seq;".
            
            if getentryfield(05, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" then do:

                put unformat
                    "Incorp;Tp Incorp;".
            end.
            
        put unformat    
            "Dt Aquis;VO Corrigido;Residual;Corr Monet;% Ano Dpr/Am;".

        if getentryfield(07, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes"
       and getentryfield(08, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" then 

        put unformat
            "Total Depreciado" skip.

        if getentryfield(07, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes"
       and getentryfield(08, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "no" then 
            put unformat
            "Perc Dpr Ac;Perc Amort;Valor Depreciado;Valor Amortizado" skip.

        if getentryfield(07, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "no"
       and getentryfield(08, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" then 
            put unformat
            " Dpr/Amort Val Origin;Dpr/Amort CM;CM Dpr/Amort" skip.

        if getentryfield(07, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "no"
       and getentryfield(08, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "no" then do:
            put unformat
            "Perc Dpr Ac; Perc Amort;Dpr Val Orig;Amort VO;Dpr CM;Amort CM;CM Dpr;CM Amort;".
        
            if v_calc_pis_cof_cls = "yes" then 
            put unformat
                "Vl CR PIS;Vl CR COFINS;Vl CR CSLL" skip.
            else
                put unformat
                    skip.
        end.

        CREATE QUERY h_query.
        h_query:SET-BUFFERS(v_hdl_tt_rpt_bem_pat_calc_period).
        h_query:QUERY-PREPARE("for each tt_rpt_bem_pat_calc_period").
        h_query:QUERY-OPEN.
        h_query:get-first(). 

        assign h_cod_cta_pat                   = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("tta_cod_cta_pat") 
               h_cod_estab                     = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("tta_cod_estab")
               h_cod_unid_negoc                = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("tta_cod_unid_negoc")
               h_cod_plano_ccusto              = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("tta_cod_plano_ccusto")
               h_cod_ccusto_respons            = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("tta_cod_ccusto_respons")
               h_num_bem_pat                   = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("tta_num_bem_pat")    
               h_num_seq_bem_pat               = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("tta_num_seq_bem_pat")
               h_dat_aquis_bem_pat             = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("tta_dat_aquis_bem_pat")
               h_val_origin_corrig             = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("tta_val_origin_corrig")
               h_val_resid_min                 = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("tta_val_resid_min")
               h_val_cm                        = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("tta_val_cm")
               h_val_perc_anual_dpr            = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("ttv_val_perc_anual_dpr") 
               h_val_perc_dpr_acum             = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("tta_val_perc_dpr_acum") /*1*/
               h_val_perc_amort_acum           = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("tta_val_perc_amort_acum ") 
               h_val_dpr_val_origin            = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("tta_val_dpr_val_origin") 
               h_val_dpr_val_origin_amort      = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("tta_val_dpr_val_origin_amort") 
               h_val_dpr_cm                    = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("tta_val_dpr_cm") 
               h_val_dpr_cm_amort              = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("tta_val_dpr_cm_amort") 
               h_val_cm_dpr                    = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("tta_val_cm_dpr") 
               h_val_cm_dpr_amort              = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("tta_val_cm_dpr_amort")
               h_val_cr_pis_rel                = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("ttv_val_cr_pis_rel")
               h_val_cr_cofins_rea             = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("ttv_val_cr_cofins_rea")
               h_val_cr_csll_rea               = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("ttv_val_cr_csll_rea")
               h_val_dpr_amort                 = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("ttv_val_dpr_amort")
               h_val_dpr_amort_cm              = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("ttv_val_dpr_amort_cm")
               h_v_val_cm_dpr_amort            = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("ttv_val_cm_dpr_amort")
               h_val_dpr                       = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("ttv_val_dpr")
               h_val_amort                     = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("ttv_val_amort")
               h_val_tot_dpr                   = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("ttv_val_tot_dpr")
               h_num_id_bem_pat                = v_hdl_tt_rpt_bem_pat_calc_period:buffer-field("tta_num_id_bem_pat").
                                                 
        h_query:get-first().

        DO WHILE (NOT h_query:QUERY-OFF-END):

            assign v_num_id_bem_pat = h_num_id_bem_pat:buffer-value.

            put unformat 
                h_cod_cta_pat:buffer-value              ";"
                h_cod_estab:buffer-value                ";"
                h_cod_unid_negoc:buffer-value           ";"
                h_cod_plano_ccusto:buffer-value         ";"
                h_cod_ccusto_respons:buffer-value       ";"
                h_num_bem_pat:buffer-value              ";"
                h_num_seq_bem_pat:buffer-value          ";".


            if getentryfield(05, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" then

                put unformat
                    ";;".

                put unformat
                    h_dat_aquis_bem_pat:buffer-value        ";"
                    h_val_origin_corrig:buffer-value        ";"
                    h_val_resid_min:buffer-value            ";"    
                    h_val_cm:buffer-value                   ";"
                    h_val_perc_anual_dpr:buffer-value       ";".

            if getentryfield(07, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes"
           and getentryfield(08, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" then
                
                put unformat
                    h_val_tot_dpr:buffer-value          skip.
            
            if getentryfield(07, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes"
           and getentryfield(08, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "no" then 
            
                put unformat
                    h_val_perc_dpr_acum:buffer-value    ";"
                    h_val_perc_amort_acum:buffer-value  ";"
                    h_val_dpr:buffer-value              ";"
                    h_val_amort:buffer-value            skip.

            if getentryfield(07, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "no"
           and getentryfield(08, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" then 
                put unformat
                    h_val_dpr_amort:buffer-value         ";"
                    h_val_dpr_amort_cm:buffer-value      ";"
                    h_v_val_cm_dpr_amort:buffer-value    skip.

            if getentryfield(07, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "no"
           and getentryfield(08, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "no" then do: 

                put unformat
                    h_val_perc_dpr_acum:buffer-value        ";"
                    h_val_perc_amort_acum:buffer-value      ";"
                    h_val_dpr_val_origin:buffer-value       ";"
                    h_val_dpr_val_origin_amort:buffer-value ";"
                    h_val_dpr_cm:buffer-value               ";"
                    h_val_dpr_cm_amort:buffer-value         ";"
                    h_val_cm_dpr:buffer-value               ";"
                    h_val_cm_dpr_amort:buffer-value         ";".

                if v_calc_pis_cof_cls = "yes" then do:
    
                    put unformat
                        h_val_cr_pis_rel:buffer-value           ";"  
                        h_val_cr_cofins_rea:buffer-value        ";"
                        h_val_cr_csll_rea:buffer-value          skip.
                    
                end.
    
                else
                    put unformat
                        skip.

            end.

            /*Se incorp bem*/

            if getentryfield(05, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" then do:

                create query h_incorp_query.
                h_incorp_query:set-buffers(v_hdl_tt_rpt_bem_pat_calc_incorp).
                h_incorp_query:query-prepare("for each tt_rpt_bem_pat_calc_incorp where tt_rpt_bem_pat_calc_incorp.tta_num_id_bem_pat = " + string(v_num_id_bem_pat)).
                h_incorp_query:QUERY-OPEN.
                h_incorp_query:get-first().

                assign h_ind_incorp_bem_pat               = v_hdl_tt_rpt_bem_pat_calc_incorp:buffer-field("tta_ind_incorp_bem_pat")
                       h_num_seq_incorp_bem_pat           = v_hdl_tt_rpt_bem_pat_calc_incorp:buffer-field("tta_num_seq_incorp_bem_pat")
                       h_dat_incorp_bem_pat               = v_hdl_tt_rpt_bem_pat_calc_incorp:buffer-field("tta_dat_incorp_bem_pat")
                       h_inc_val_origin_corrig            = v_hdl_tt_rpt_bem_pat_calc_incorp:buffer-field("tta_val_origin_corrig")
                       h_inc_val_resid_min                = v_hdl_tt_rpt_bem_pat_calc_incorp:buffer-field("tta_val_resid_min")
                       h_inc_val_cm                       = v_hdl_tt_rpt_bem_pat_calc_incorp:buffer-field("tta_val_cm")
                       h_inc_val_perc_anual_dpr           = v_hdl_tt_rpt_bem_pat_calc_incorp:buffer-field("ttv_val_perc_anual_dpr")

                       h_inc_val_perc_dpr_acum            = v_hdl_tt_rpt_bem_pat_calc_incorp:buffer-field("tta_val_perc_dpr_acum")
                       h_inc_val_perc_amort_acum          = v_hdl_tt_rpt_bem_pat_calc_incorp:buffer-field("tta_val_perc_amort_acum")
                       h_inc_val_dpr_val_origin           = v_hdl_tt_rpt_bem_pat_calc_incorp:buffer-field("tta_val_dpr_val_origin")
                       h_inc_val_dpr_val_origin_amort     = v_hdl_tt_rpt_bem_pat_calc_incorp:buffer-field("tta_val_dpr_val_origin_amort")
                       h_inc_val_dpr_cm                   = v_hdl_tt_rpt_bem_pat_calc_incorp:buffer-field("tta_val_dpr_cm")
                       h_inc_val_dpr_cm_amort             = v_hdl_tt_rpt_bem_pat_calc_incorp:buffer-field("tta_val_dpr_cm_amort")
                       h_inc_val_cm_dpr                   = v_hdl_tt_rpt_bem_pat_calc_incorp:buffer-field("tta_val_cm_dpr")
                       h_inc_val_cm_dpr_amort             = v_hdl_tt_rpt_bem_pat_calc_incorp:buffer-field("tta_val_cm_dpr_amort")
                       h_inc_val_cr_pis_rel               = v_hdl_tt_rpt_bem_pat_calc_incorp:buffer-field("ttv_val_cr_pis_rel")
                       h_inc_val_cr_cofins_rea            = v_hdl_tt_rpt_bem_pat_calc_incorp:buffer-field("ttv_val_cr_cofins_rea")
                       h_inc_val_cr_csll_rea              = v_hdl_tt_rpt_bem_pat_calc_incorp:buffer-field("ttv_val_cr_csll_rea")
                       h_inc_val_dpr                      = v_hdl_tt_rpt_bem_pat_calc_incorp:buffer-field("ttv_val_dpr")
                       h_inc_val_amort                    = v_hdl_tt_rpt_bem_pat_calc_incorp:buffer-field("ttv_val_amort")
                       h_inc_val_tot_dpr                  = v_hdl_tt_rpt_bem_pat_calc_incorp:buffer-field("ttv_val_tot_dpr")
                       h_inc_val_dpr_amort                = v_hdl_tt_rpt_bem_pat_calc_incorp:buffer-field("ttv_val_dpr_amort")
                       h_inc_val_dpr_amort_cm             = v_hdl_tt_rpt_bem_pat_calc_incorp:buffer-field("ttv_val_dpr_amort_cm")
                       h_inc_v_val_cm_dpr_amort           = v_hdl_tt_rpt_bem_pat_calc_incorp:buffer-field("ttv_val_cm_dpr_amort").

                h_incorp_query:get-first().

                DO WHILE (NOT h_incorp_query:QUERY-OFF-END):

                    put unformat
                        h_cod_cta_pat:buffer-value                       ";"   
                        h_cod_estab:buffer-value                         ";" 
                        h_cod_unid_negoc:buffer-value                    ";" 
                        h_cod_plano_ccusto:buffer-value                  ";" 
                        h_cod_ccusto_respons:buffer-value                ";" 
                        h_num_bem_pat:buffer-value                       ";" 
                        h_num_seq_bem_pat:buffer-value                   ";" 
                        h_num_seq_incorp_bem_pat:buffer-value            ";"
                        h_ind_incorp_bem_pat:buffer-value                ";" 
                        h_dat_incorp_bem_pat:buffer-value                ";"
                        h_inc_val_origin_corrig:buffer-value             ";"
                        h_inc_val_resid_min:buffer-value                 ";"
                        h_inc_val_cm:buffer-value                        ";"
                        h_inc_val_perc_anual_dpr:buffer-value            ";".

                      if getentryfield(07, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes"          
                     and getentryfield(08, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" then     
                                                                                                       
                          put unformat                                                                 
                              h_inc_val_tot_dpr:buffer-value          skip.                                
                                                                                                       
                      if getentryfield(07, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes"          
                     and getentryfield(08, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "no" then      
                                                                                                       
                          put unformat                                                                 
                              h_inc_val_perc_dpr_acum:buffer-value    ";"                                  
                              h_inc_val_perc_amort_acum:buffer-value  ";"                                  
                              h_inc_val_dpr:buffer-value              ";"                                  
                              h_inc_val_amort:buffer-value            skip.                                
                                                                                                       
                      if getentryfield(07, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "no"           
                     and getentryfield(08, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "yes" then     
                          put unformat                                                                 
                              h_inc_val_dpr_amort:buffer-value         ";"                                 
                              h_inc_val_dpr_amort_cm:buffer-value      ";"                                 
                              h_inc_v_val_cm_dpr_amort:buffer-value    skip.                               
                                                                                                       
                      if getentryfield(07, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "no"           
                     and getentryfield(08, dwb_rpt_param.cod_dwb_parameters, chr(10)) = "no" then do:  
                                                                                                       
                          put unformat                                                                 
                              h_inc_val_perc_dpr_acum:buffer-value        ";"                              
                              h_inc_val_perc_amort_acum:buffer-value      ";"                              
                              h_inc_val_dpr_val_origin:buffer-value       ";"                              
                              h_inc_val_dpr_val_origin_amort:buffer-value ";"                              
                              h_inc_val_dpr_cm:buffer-value               ";"                              
                              h_inc_val_dpr_cm_amort:buffer-value         ";"                              
                              h_inc_val_cm_dpr:buffer-value               ";"                              
                              h_inc_val_cm_dpr_amort:buffer-value         ";".                             
                                                                                                       
                          if v_calc_pis_cof_cls = "yes" then do:                                       
                                                                                                       
                              put unformat                                                             
                                  h_inc_val_cr_pis_rel:buffer-value           ";"                          
                                  h_inc_val_cr_cofins_rea:buffer-value        ";"                          
                                  h_inc_val_cr_csll_rea:buffer-value          skip.                        
                                                                                                       
                          end.                                                                         
                                                                                                       
                          else                                                                         
                              put unformat                                                             
                                  skip.                                                                
                                                                                                       
                      end.                                                                             

                    h_incorp_query:GET-NEXT.

                end.

                h_incorp_query:QUERY-CLOSE().

                DELETE OBJECT h_incorp_query.

            end.

            /*Incorp fim*/

            h_query:GET-NEXT.
        END.
        
        h_query:QUERY-CLOSE().
        
        DELETE OBJECT h_query.
        
        output close.
    end.
end.

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
    def var v_num_1                          as integer         no-undo. /*local*/
    
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

    if  num-entries(v_cod_1, ".") > 2
    then do:
        return "NOK" /*l_nok*/ .
    end /* if */.

    if  num-entries(v_cod_1, ".") = 2 and length(entry(2,v_cod_1,".")) > 3
    then do:
        return "NOK" /*l_nok*/ .
    end /* if */.

    return "OK" /*l_ok*/ .

END PROCEDURE. /* pi_filename_validation */
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
/*******************  End of rpt_aprop_ctbl_acr_diario_aux ******************/

/*****************************************************************************
** Copyright DATASUL S.A. (1994)
** Todos os Direitos Reservados.
** 
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so' podera ser feita mediante
** autorizacao expressa.
**
** Programa..............: esp/epc_fas037aa.p
** Descricao.............: 
** 1.00.00.001 - Tratar registro de invent rio ado‡Æo inicial
** Nome Externo..........: esp/epc_fas037aa.p
** Criado por............:
** Criado em.............: 07/10/2016
** Programa Chamador.....: prgfin/fas/fas037aa.p
*****************************************************************************/

/* Definicao de Parametros */
def input param p_ind_event     as char             no-undo.
def input param p_ind_object    as char             no-undo.
def input param p_wgh_object    as handle           no-undo.
def input param p_wgh_frame     as widget-handle    no-undo.
def input param p_cod_table     as char             no-undo.
def input param p_rec_table     as recid            no-undo.

define variable wh_objeto                    as widget-handle no-undo.
define variable wh_browser                   as widget-handle no-undo.
define variable wh-bf-cod_cta_pat            as widget-handle no-undo.
define variable wh-bf-num_bem_pat            as widget-handle no-undo.
define variable wh-bf-num_seq_bem_pat        as widget-handle no-undo.
define variable wh-bf-num_seq_incorp_bem_pat as widget-handle no-undo.
define variable wh-bf-ind_adoc_inicial       as widget-handle no-undo.
define variable wh-ind_adoc_inicial          as widget-handle no-undo.
define variable wh-log_calc_vida_util        as widget-handle no-undo.
define variable v_num_count                  as integer       no-undo.

if  p_ind_event  = "Initialize"
and p_ind_object = "Viewer" then do:
    assign wh_objeto = p_wgh_frame:first-child.
    blk_handle:
    do  while valid-handle(wh_objeto):
        if  wh_objeto:type <> "field-group" then do:
            case wh_objeto:name:
                when "br_bas_adoc_inicial" then do:
                    assign wh_browser = wh_objeto.
                    do  v_num_count = 1 to wh_browser:num-columns:
                        case wh_browser:get-browse-column(v_num_count):name:
                            when "cod_cta_pat" then
                                assign wh-bf-cod_cta_pat = wh_browser:get-browse-column(v_num_count):buffer-field.
                            when "num_bem_pat" then
                                assign wh-bf-num_bem_pat = wh_browser:get-browse-column(v_num_count):buffer-field.
                            when "num_seq_bem_pat" then
                                assign wh-bf-num_seq_bem_pat = wh_browser:get-browse-column(v_num_count):buffer-field.
                            when "num_seq_incorp_bem_pat" then
                                assign wh-bf-num_seq_incorp_bem_pat = wh_browser:get-browse-column(v_num_count):buffer-field.
                            when "ind_movto_adoc_inicial" then
                                assign wh-bf-ind_adoc_inicial = wh_browser:get-browse-column(v_num_count):buffer-field
                                       wh-ind_adoc_inicial    = wh_browser:get-browse-column(v_num_count).
                        end case.
                        if  valid-handle(wh-bf-cod_cta_pat)
                        and valid-handle(wh-bf-num_bem_pat)
                        and valid-handle(wh-bf-num_seq_bem_pat)
                        and valid-handle(wh-bf-num_seq_incorp_bem_pat)
                        and valid-handle(wh-bf-ind_adoc_inicial) then
                            leave blk_handle.
                    end.
                end.
                otherwise
                    assign wh_objeto = wh_objeto:next-sibling.
            end case.
        end.
        else do:
            assign wh_objeto = wh_objeto:first-child.
        end.
    end.

    if valid-handle(wh_browser) then do:
        assign wh-log_calc_vida_util = wh_browser:add-calc-column("logical","Sim/NÆo","NÆo","Calc Vida étil",(wh_browser:num-columns + 1),"toggle-box").
        on row-display of wh_browser
            persistent run esp/epc_fas037aa1.p (input "row-display",
                                                input wh-bf-cod_cta_pat,
                                                input wh-bf-num_bem_pat,
                                                input wh-bf-num_seq_bem_pat,
                                                input wh-bf-num_seq_incorp_bem_pat,
                                                input wh-bf-ind_adoc_inicial,
                                                input wh-ind_adoc_inicial,
                                                input wh-log_calc_vida_util).
    end.
end.

return "OK".

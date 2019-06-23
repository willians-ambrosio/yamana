/*****************************************************************************
** Programa..............: esp/epc_fas037aa1.p
** Programa Chamador.....: esp/epc_fas037aa.p
** Criado por............: 
** Criado em.............: 10/10/2016
*****************************************************************************/
def input param p_ind_event                    as character     no-undo.
def input param p_wh-bf-cod_cta_pat            as widget-handle no-undo.
def input param p_wh-bf-num_bem_pat            as widget-handle no-undo.
def input param p_wh-bf-num_seq_bem_pat        as widget-handle no-undo.
def input param p_wh-bf-num_seq_incorp_bem_pat as widget-handle no-undo.
def input param p_wh-bf-ind_adoc_inicial       as widget-handle no-undo.
def input param p_wh-ind_adoc_inicial          as widget-handle no-undo.
def input param p_wh-log_calc_vida_util        as widget-handle no-undo.

def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.

if  valid-handle(p_wh-bf-cod_cta_pat)
and valid-handle(p_wh-bf-num_bem_pat)
and valid-handle(p_wh-bf-num_seq_bem_pat)
and valid-handle(p_wh-bf-num_seq_incorp_bem_pat)
and valid-handle(p_wh-bf-ind_adoc_inicial) then do:
    find first esp_adoc_inicial_invent no-lock
        where  esp_adoc_inicial_invent.cod_empresa                 = v_cod_empres_usuar
        and    esp_adoc_inicial_invent.cod_cta_pat_dest            = p_wh-bf-cod_cta_pat:buffer-value
        and    esp_adoc_inicial_invent.num_bem_pat_dest            = p_wh-bf-num_bem_pat:buffer-value
        and    esp_adoc_inicial_invent.num_seq_bem_pat_dest        = p_wh-bf-num_seq_bem_pat:buffer-value
        and    esp_adoc_inicial_invent.num_seq_incorp_bem_pat_dest = p_wh-bf-num_seq_incorp_bem_pat:buffer-value no-error.
    if  avail esp_adoc_inicial_invent then do:
        case p_ind_event:
            when "row-display" then do:
                if  p_wh-bf-ind_adoc_inicial:buffer-value = "Implanta‡Æo" then
                    assign p_wh-ind_adoc_inicial:screen-value = "Invent rio".

                if  valid-handle(p_wh-log_calc_vida_util) then
                    assign p_wh-log_calc_vida_util:screen-value = string(esp_adoc_inicial_invent.log_calc_vida_util,"Sim/NÆo").
            end.
        end case.
    end.
end.

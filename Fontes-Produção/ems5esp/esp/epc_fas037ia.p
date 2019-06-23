/*****************************************************************************
** Copyright DATASUL S.A. (1994)
** Todos os Direitos Reservados.
** 
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so' podera ser feita mediante
** autorizacao expressa.
**
** Programa..............:esp/epc_fas037ia.p
** Descricao.............: 
** 1.00.00.001 - Tratar registro de invent rio ado‡Æo inicial
** Nome Externo..........: esp/epc_fas037ia.p
** Criado por............:
** Criado em.............: 10/10/2016
** Programa Chamador.....: prgfin/fas/fas037ia.p
*****************************************************************************/

/* Definicao de Parametros */
def input param p_ind_event     as char             no-undo.
def input param p_ind_object    as char             no-undo.
def input param p_wgh_object    as handle           no-undo.
def input param p_wgh_frame     as widget-handle    no-undo.
def input param p_cod_table     as char             no-undo.
def input param p_rec_table     as recid            no-undo.

def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.

define variable wh_objeto                    as widget-handle no-undo.
define variable wh-cod_cta_pat            as widget-handle no-undo.
define variable wh-num_bem_pat            as widget-handle no-undo.
define variable wh-num_seq_bem_pat        as widget-handle no-undo.
define variable wh-num_seq_incorp_bem_pat as widget-handle no-undo.
define variable wh-ind_adoc_inicial          as widget-handle no-undo.

if  p_ind_event  = "Display"
and p_ind_object = "Viewer" then do:
    assign wh_objeto = p_wgh_frame:first-child.
    blk_handle:
    do  while valid-handle(wh_objeto):
        if  wh_objeto:type <> "field-group" then do:
            case wh_objeto:name:
                when "cod_cta_pat" then
                    assign wh-cod_cta_pat = wh_objeto.
                when "num_bem_pat" then
                    assign wh-num_bem_pat = wh_objeto.
                when "num_seq_bem_pat" then
                    assign wh-num_seq_bem_pat = wh_objeto.
                when "num_seq_incorp_bem_pat" then
                    assign wh-num_seq_incorp_bem_pat = wh_objeto.
                when "ind_movto_adoc_inicial" then
                    assign wh-ind_adoc_inicial = wh_objeto.
            end case.
            if  valid-handle(wh-cod_cta_pat)
            and valid-handle(wh-num_bem_pat)
            and valid-handle(wh-num_seq_bem_pat)
            and valid-handle(wh-num_seq_incorp_bem_pat)
            and valid-handle(wh-ind_adoc_inicial) then
                leave blk_handle.
            else
                assign wh_objeto = wh_objeto:next-sibling.
        end.
        else do:
            assign wh_objeto = wh_objeto:first-child.
        end.
    end.

    if  valid-handle(wh-cod_cta_pat)
    and valid-handle(wh-num_bem_pat)
    and valid-handle(wh-num_seq_bem_pat)
    and valid-handle(wh-num_seq_incorp_bem_pat)
    and valid-handle(wh-ind_adoc_inicial) then do:
        if  wh-ind_adoc_inicial:input-value = "Implanta‡Æo" then do:
            find first esp_adoc_inicial_invent no-lock
                where  esp_adoc_inicial_invent.cod_empresa                 = v_cod_empres_usuar
                and    esp_adoc_inicial_invent.cod_cta_pat_dest            = wh-cod_cta_pat:input-value
                and    esp_adoc_inicial_invent.num_bem_pat_dest            = wh-num_bem_pat:input-value
                and    esp_adoc_inicial_invent.num_seq_bem_pat_dest        = wh-num_seq_bem_pat:input-value
                and    esp_adoc_inicial_invent.num_seq_incorp_bem_pat_dest = wh-num_seq_incorp_bem_pat:input-value no-error.
            if  avail esp_adoc_inicial_invent then
                assign wh-ind_adoc_inicial:screen-value = "Invent rio".
        end.
    end.
end.

return "OK".

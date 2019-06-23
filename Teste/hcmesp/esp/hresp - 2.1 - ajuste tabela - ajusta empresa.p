def var c-emp as char no-undo.

FUNCTION fc-empresa RETURNS CHARACTER
  ( c-emp-de as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    def var c-emp-para as char no-undo.

    case c-emp-de:
        when "200" then assign c-emp-para = "201".
        when "210" then assign c-emp-para = "211".
        when "220" then assign c-emp-para = "221".
        when "300" then assign c-emp-para = "301".
        when "500" then assign c-emp-para = "401".
        otherwise       assign c-emp-para = c-emp-de.
    end case.

    return c-emp-para.
END FUNCTION.

/* -------------------------------------------------------------------- */
disable triggers for load of bco_hrs_compens_func_esp.

for each bco_hrs_compens_func_esp exclusive-lock:
    assign c-emp = fc-empresa(string(bco_hrs_compens_func_esp.cdn_empresa)).

    assign bco_hrs_compens_func_esp.cdn_empresa = c-emp.
end.

/* -------------------------------------------------------------------- */
disable triggers for load of estab_benef_prev_brad.

for each estab_benef_prev_brad exclusive-lock:
    assign c-emp = fc-empresa(string(estab_benef_prev_brad.cdn_empresa)).

    assign estab_benef_prev_brad.cdn_empresa = c-emp.
end.

/* -------------------------------------------------------------------- */
disable triggers for load of func_desligto.

for each func_desligto exclusive-lock:
    assign c-emp = fc-empresa(string(func_desligto.cdn_empresa)).

    assign func_desligto.cdn_empresa = c-emp.
end.

/* -------------------------------------------------------------------- */
disable triggers for load of grp_usuar_esp.

for each grp_usuar_esp exclusive-lock:
    assign c-emp = fc-empresa(string(grp_usuar_esp.cdn_empresa)).

    assign grp_usuar_esp.cdn_empresa = c-emp.
end.

/* -------------------------------------------------------------------- */
disable triggers for load of ocor_nao_autoriz.

for each ocor_nao_autoriz exclusive-lock:
    assign c-emp = fc-empresa(string(ocor_nao_autoriz.cdn_empresa)).

    assign ocor_nao_autoriz.cdn_empresa = c-emp.
end.

/* -------------------------------------------------------------------- */
disable triggers for load of sit_afast_nao_autoriz.

for each sit_afast_nao_autoriz exclusive-lock:
    assign c-emp = fc-empresa(string(sit_afast_nao_autoriz.cdn_empresa)).

    assign sit_afast_nao_autoriz.cdn_empresa = c-emp.
end.


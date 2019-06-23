{include/buffers_RH.i}

{include/i-prgvrs.i pe3130dymup 1.00.00.000} 

def input param p-ind-event     as char             no-undo.
def input param p-ind-object    as char             no-undo.
def input param p-wgh-object    as handle           no-undo.
def input param p-wgh-frame     as widget-handle    no-undo.
def input param p-cod-table     as char             no-undo.
def input param p-row-table     as rowid            no-undo.

def        var c-objeto                as char      no-undo.

DEF shared TEMP-TABLE tt_ocor_posit
    FIELD cdn_ocor  AS INT  FORMAT "zzzzzzzzz9"
    FIELD des_ocor  AS CHAR FORMAT "x(55)"
    FIELD cdn_banco AS INT.

{utp/ut-glob.i}

/**** l¢gica ******/
assign c-objeto = entry(num-entries(p-wgh-object:private-data, "~/"), p-wgh-object:private-data, "~/").

if p-ind-event = "BEFORE-INITIALIZE" and 
   p-ind-object = "CONTAINER" and 
   (c-objeto = "pe3130d.w" or 
    c-objeto = "prghur\pep\pe3130d.w") then do:
    find first usuar_grp_usuar no-lock
        where usuar_grp_usuar.cod_usuario = v_cod_usuar_corren no-error.
    if avail usuar_grp_usuar then do:
        for each tt_ocor_posit:
           find first ocor_nao_autoriz
                where ocor_nao_autoriz.cdn_empresa   = v_cdn_empres_usuar
                  AND ocor_nao_autoriz.cod_grp_usuar = usuar_grp_usuar.cod_grp_usuar
                  and ocor_nao_autoriz.cdn_ocor_mpe  = tt_ocor_posit.cdn_ocor no-error.
           if avail ocor_nao_autoriz then do:
               delete tt_ocor_posit.
           end.
        end.
    END.
end.




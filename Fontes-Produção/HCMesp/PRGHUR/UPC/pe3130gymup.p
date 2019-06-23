{include/buffers_RH.i}

{include/i-prgvrs.i pe3130gymup 1.00.00.000} 

def input param p-ind-event     as char             no-undo.
def input param p-ind-object    as char             no-undo.
def input param p-wgh-object    as handle           no-undo.
def input param p-wgh-frame     as widget-handle    no-undo.
def input param p-cod-table     as char             no-undo.
def input param p-row-table     as rowid            no-undo.

def        var c-objeto                as char      no-undo.

DEF shared TEMP-TABLE tt_sit_hora
    FIELD cdn_sit  AS INT  FORMAT "zzzzzzzzz9"
    FIELD des_sit  AS CHAR FORMAT "x(55)".

{utp/ut-glob.i}

/**** l¢gica ******/
assign c-objeto = entry(num-entries(p-wgh-object:private-data, "~/"), p-wgh-object:private-data, "~/").

if p-ind-event = "BEFORE-INITIALIZE" and 
   p-ind-object = "CONTAINER" and 
   (c-objeto = "pe3130g.w" or 
    c-objeto = "prghur\pep\pe3130g.w") then do:
    find first usuar_grp_usuar no-lock
        where usuar_grp_usuar.cod_usuario = v_cod_usuar_corren no-error.
    if avail usuar_grp_usuar then do:
        for each tt_sit_hora:
            find first sit_afast_nao_autoriz
                where sit_afast_nao_autoriz.cdn_empresa        = v_cdn_empres_usuar           
                AND sit_afast_nao_autoriz.cod_grp_usuar        = usuar_grp_usuar.cod_grp_usuar
                and sit_afast_nao_autoriz.cdn_sit_afast_func   = tt_sit_hora.cdn_sit NO-ERROR.
            if avail sit_afast_nao_autoriz then do:
               delete tt_sit_hora.
           end.
        end.
    END.
end.


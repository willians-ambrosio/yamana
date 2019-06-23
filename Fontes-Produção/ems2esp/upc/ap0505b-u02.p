DEFINE NEW GLOBAL SHARED VARIABLE wh-ap0505b-ccusto   AS HANDLE        NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-ap0505b-conta    AS HANDLE        NO-UNDO.

def new global shared var v_cod_dwb_user
    as character
    format "x(21)"
    label "Usu rio"
    column-label "Usu rio"
    no-undo.

def new global shared var v_rec_plano_cta_ctbl
    as recid
    format ">>>>>>9":U
    no-undo.

def new global shared var v_rec_cta_ctbl
    as recid
    format ">>>>>>9":U
    no-undo.

    assign v_rec_plano_cta_ctbl = recid(plano_cta_ctbl).
    if  search("prgint/utb/utb080nc.r") = ? and search("prgint/utb/utb080nc.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut vel não foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb080nc.p".
        else do:
            message "Programa execut vel não foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb080nc.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run prgint/utb/utb080nc.p /*prg_see_cta_ctbl_plano*/.
    if  v_rec_cta_ctbl <> ? then do:
        find cta_ctbl where recid(cta_ctbl) = v_rec_cta_ctbl no-lock no-error.
        assign wh-ap0505b-conta:screen-value = string(cta_ctbl.cod_cta_ctbl,plano_cta_ctbl.cod_format_cta_ctbl).
    end.
    

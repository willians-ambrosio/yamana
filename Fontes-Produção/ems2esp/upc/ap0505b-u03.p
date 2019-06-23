def buffer ccusto for ems5.ccusto.

DEFINE NEW GLOBAL SHARED VARIABLE wh-ap0505b-ccusto   AS HANDLE        NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-ap0505b-conta    AS HANDLE        NO-UNDO.

def new global shared var v_cod_dwb_user
    as character
    format "x(21)"
    label "Usu rio"
    column-label "Usu rio"
    no-undo.

def new global shared var v_rec_plano_ccusto
    as recid
    format ">>>>>>9":U
    no-undo.

def new global shared var v_rec_ccusto
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.

    assign v_rec_plano_ccusto = recid(plano_ccusto).
    if  search("prgint/utb/utb066ka.r") = ? and search("prgint/utb/utb066ka.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut vel não foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb066ka.p".
        else do:
            message "Programa execut vel não foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb066ka.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run prgint/utb/utb066ka.p /*prg_sea_ccusto*/.

    if  v_rec_ccusto <> ?
    then do:
        find ccusto where recid(ccusto) = v_rec_ccusto no-lock no-error.
	assign wh-ap0505b-ccusto:screen-value = string(ccusto.cod_ccusto).
    end.


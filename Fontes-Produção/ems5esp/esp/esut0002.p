def new global shared var v_cod_dwb_program
    as character
    format "x(32)":U
    label "Programa"
    column-label "Programa"
    no-undo.

Assign v_cod_dwb_program = 'tar_apuracao_variac_econ'.

Run esp\esut0001.w.



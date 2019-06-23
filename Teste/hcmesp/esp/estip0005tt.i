define temp-table tt-param          no-undo
    field destino                   as integer
    field arquivo                   as char format "x(35)"
    field usuario                   as char format "x(12)"
    field data-exec                 as date
    field hora-exec                 as integer
    field classifica                as integer
    field desc-classifica           as char format "x(40)"
    field cdn_empresa_ini           like funcionario.cdn_empresa
    field cdn_empresa_fim           like funcionario.cdn_empresa
    field cdn_estab_ini             like funcionario.cdn_estab
    field cdn_estab_fim             like funcionario.cdn_estab
    field cdn_funcionario_ini       like funcionario.cdn_funcionario
    field cdn_funcionario_fim       like funcionario.cdn_funcionario
    field cdn_niv_hier_funcnal_ini  like niv_hier_funcnal.cdn_niv_hier_funcnal
    field cdn_niv_hier_funcnal_fim  like niv_hier_funcnal.cdn_niv_hier_funcnal
    field cdn_cargo_basic_ini       like cargo_basic.cdn_cargo_basic
    field cdn_cargo_basic_fim       like cargo_basic.cdn_cargo_basic
    field num_anotip                as integer
    FIELD idi_tip_relat             AS INTEGER.

define temp-table tt-digita         no-undo
    field ordem                     as integer   format ">>>>9"
    field exemplo                   as character format "x(30)"
    index id ordem.


def temp-table tt-raw-digita
   field raw-digita      as raw.

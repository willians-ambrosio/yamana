/*************************************************************************************
** Include  : cbp\cbapi002.i tt-layout
** Autor    : DATASUL S.A.
**************************************************************************************/
define temp-table tt-layout
  field banco              like banco.cod-banco
  field tp-registro        as int
  field cod-registro       as int 
  field empresa-h          like apl-empr-banco.ep-codigo
  field banco-h            like banco.cod-banco
  field agencia-h          like cta-corrente.agencia
  field conta-h            like cta-corrente.conta-corren
  field diferen-inicial    as int
  field saldo-inicial      like cta-corrente.saldo-atual
  field db-cr-inicial      as char
  field saldo-final        like cta-corrente.saldo-atual
  field db-cr-final        as char
  field cod-lancto         as int
  field historico          as char
  field nr-docto           as char
  field dt-movto           like cta-corrente.data-saldo
  field vl-lancto          like cta-corrente.saldo-atual
  field db-cr-lancto       as char
  field cod-saldo-ant      as int
  field cod-lancamento     as int
  field cod-saldo-final    as int
  field dt-saldo-inicial   like cta-corrente.data-saldo
  field dt-saldo-final     like cta-corrente.data-saldo
  field diferen-final      as int
  field diferen-lan        as int
  field dt-geracao-h       like cta-corrente.data-saldo
  field agencia-lan        like cta-corrente.agencia
  field banco-inicial      like banco.cod-banco
  field agencia-inicial    like cta-corrente.agencia
  field conta-inicial      like cta-corrente.conta-corren
  field banco-final        like banco.cod-banco
  field agencia-final      like cta-corrente.agencia
  field banco-lan          like banco.cod-banco
  field conta-lan          like cta-corrente.conta-corren
  field conta-final        like cta-corrente.conta-corren
  field rowid-extrato      as rowid
  field id-registro        as int
  field id-processo        as int
  field cod-processo       as int.

define temp-table tt-layout-1
  field banco              like banco.cod-banco
  field tp-registro        as int
  field cod-registro       as int 
  field empresa-h          like apl-empr-banco.ep-codigo
  field banco-h            like banco.cod-banco
  field agencia-h          like cta-corrente.agencia
  field conta-h            like cta-corrente.conta-corren
  field diferen-inicial    as int
  field saldo-inicial      like cta-corrente.saldo-atual
  field db-cr-inicial      as char
  field saldo-final        like cta-corrente.saldo-atual
  field db-cr-final        as char
  field cod-lancto         as int
  field historico          as char
  field nr-docto           as char
  field dt-movto           like cta-corrente.data-saldo
  field vl-lancto          like cta-corrente.saldo-atual
  field db-cr-lancto       as char
  field cod-saldo-ant      as int
  field cod-lancamento     as int
  field cod-saldo-final    as int
  field dt-saldo-inicial   like cta-corrente.data-saldo
  field dt-saldo-final     like cta-corrente.data-saldo
  field diferen-final      as int
  field diferen-lan        as int
  field dt-geracao-h       like cta-corrente.data-saldo
  field agencia-lan        like cta-corrente.agencia
  field banco-inicial      like banco.cod-banco
  field agencia-inicial    like cta-corrente.agencia
  field conta-inicial      like cta-corrente.conta-corren
  field banco-final        like banco.cod-banco
  field agencia-final      like cta-corrente.agencia
  field banco-lan          like banco.cod-banco
  field conta-lan          like cta-corrente.conta-corren
  field conta-final        like cta-corrente.conta-corren
  field rowid-extrato      as rowid
  field id-registro        as int
  field id-processo        as int
  field cod-processo       as int
  field nr-extrato         like extrato-banco.nr-extrato.

/* cbapi002.i */

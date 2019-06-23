/*******************************************
** {1} = Nome da temp-table
** {2} = Define o tipo de temp-table (ex: shared)
*******************************************/

/** Dados **/
def {2} temp-table {1} no-undo
    field sequencia         as   integer
    field cod-dimensao      as   character format "x(300)"
    field cod-oficial       as   character format "X(16)"
    field desc-dimensao     as   character format "x(100)"
    field cod-dimens-pai    as   character format "x(300)"
    field l-marcado         as   logical
    field ep-codigo         like mab-eqpto.ep-codigo
    field cod-eqpto         like mab-eqpto.cod-eqpto
    field c-des-eqpto       as   character format "x(100)"
    field cod-setor         like mmv-ord-manut.cod-setor-ofici
    field cod-ofici         like mmv-ord-manut.cod-ofici
    field cod-evento        like mab-event.cod-evento
    field idi-tip-evento    as   integer
    field garantia          as decimal format ">,>>>,>>9.9"
    field atualizacao       as date format "99/99/9999"
    field ordem             as integer format ">>>,>>>,>>9"
    field tipo              as character format "x(13)"
    field cod-sub-sist      like mab-sub-sist.cod-sub-sist
    field cd-tipo           like mmv-tar-ord-manut.cd-tipo
    field cod-plano         like mmv-plano-prevent.cod-plano
    field cod-model         like mmv-plano-prevent.cod-model
    field num-seq-plano     like mmv-tar-plano.num-seq
    field narrativa         like mmv-tar-ord-manut.dsl-obs
    field c-cod-sub-sist    like mab-sub-sist.cod-sub-sist
    field num-docto         as   integer format ">>>,>>>,>>9"
    field uso-real          as   decimal format ">>,>>>,>>9.9"
    field uso-padrao        as   decimal format "->>>,>>>,>>9.9"
    field ung          as   character format "x(3)"
    field diferenca         as   decimal format "->>>,>>>,>>9.9"
    field dat-vencto        as   date    format "99/99/9999"
    field dat-atualiz       as   date    format "99/99/9999"
    field un                as   character format "x(3)"
    field val-uso-padr      like mab-period-manut.val-km-padr
    field i-origem          as   integer
    field i-estado          as   integer
    field lVencido          as   logical
    field lVencer           as   logical
    field r-rowid           as   rowid
    field p-image           as   integer
    field seq-tree          as   integer
    index id is primary unique sequencia ascending
                               cod-dimensao ascending.

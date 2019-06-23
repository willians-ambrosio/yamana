/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

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
    field cod-setor         like mmv-ord-manut.cod-setor-ofici
    field cod-ofici         like mmv-ord-manut.cod-ofici
    field cod-evento        like mab-event.cod-evento
    field idi-tip-evento    as   integer
    field cod-sub-sist      like mab-sub-sist.cod-sub-sist
    field cd-tipo           like mmv-tar-ord-manut.cd-tipo
    field cod-plano         like mmv-plano-prevent.cod-plano
    field cod-model         like mmv-plano-prevent.cod-model
    field num-seq-plano     like mmv-tar-plano.num-seq
    field narrativa         like mmv-tar-ord-manut.dsl-obs
    field num-docto         as   integer format ">>>,>>>,>>9"
    field uso-real          as   decimal format "->>>,>>>,>>9.9"
    field uso-padrao        as   decimal format "->>>,>>>,>>9.9"
    field diferenca         as   decimal format "->>>,>>>,>>9.9"
    field dat-vencto        as   date    format "99/99/9999"
    field dat-atualiz       as   date    format "99/99/9999"
    field un                as   character format "x(3)"
    field i-origem          as   integer
    field i-estado          as   integer
    field lVencido          as   logical
    field lVencer           as   logical
    field r-rowid           as   rowid
    field p-image           as   integer
    field seq-tree          as   integer
    field lMostra           as   logical
    field val-km-padr       like mmv-plano-prevent.val-km-padr
    index id is primary unique sequencia ascending
                               cod-dimensao ascending
    index vencto dat-vencto  ascending
                 diferenca   descending
                 cod-oficial ascending.

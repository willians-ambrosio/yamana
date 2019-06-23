/* 
** 
** BOCX220C.I - temp-table tempor ria para cria‡Æo da tt-itens para gera‡Æo documento 
**
*/


{cdp/cdcfgmat.i}

define temp-table {1} no-undo
    field numero-ordem       like ordens-embarque.numero-ordem
    field parcela            like ordens-embarque.parcela
    field sequencial         as   integer
    field it-codigo          like item.it-codigo
    field nr-proc-imp        like ordens-embarque.nr-proc-imp
    field quantidade         like ordens-embarque.quantidade
    field lote               like item-doc-est.lote           
    field dt-vali-lote       like item-doc-est.dt-vali-lote
    field cod-refer          like item-doc-est.cod-refer
    field tipo-con-est       as   integer
    field encerra-pa         like item-doc-est.encerra-pa
    field situacao           as   logical
    field quant-total        like ordens-embarque.quantidade
    field r-rowid            as   rowid
    field class-fiscal       like classif-fisc.class-fiscal
    field nr-ato-concessorio as   char format "x(20)"
    field preco-total        like item-doc-est.preco-total extent 0
    field preco-total-mo     like item-doc-est.preco-total extent 0
    FIELD desconto           LIKE item-doc-est.desconto    EXTENT 0
    field num-pedido         like pedido-compr.num-pedido
    field tp-despesa         like ordem-compra.tp-despesa
    field cod-depos          like ordem-compra.dep-almoxar
    FIELD peso-bruto         AS   DECIMAL FORMAT ">>>,>>>,>>9.99999"
    FIELD peso-liquido       AS   DECIMAL FORMAT ">>>,>>>,>>9.99999"
    &if "{&bf_mat_versao_ems}" >= "2.05" &then
        &if "{&bf_mat_versao_ems}" >= "2.062" &then
            field val-cub-tot    as dec format ">>>>>,>>>,>>9.999999"
            &if '{&bf_lote_avancado_liberado}' = 'yes' &then /*Campos Projeto FDA*/
                field cod-lote-fabrican             like item-doc-est.cod-lote-fabrican
                field dat-fabricc-lote              like item-doc-est.dat-fabricc-lote
                field dat-valid-lote-fabrican       like item-doc-est.dat-valid-lote-fabrican
                field nom-fabrican                  like item-doc-est.nom-fabrican
            &ENDIF
        &else
            field val-cub-uni    as dec format ">>>>>,>>>,>>9.999999"
        &endif
    &endif
        
    FIELD cd-trib-ii         AS   INTEGER
    FIELD cd-trib-ipi        AS   INTEGER
    FIELD cd-trib-icms       AS   INTEGER
    FIELD aliquota-ii        AS   DECIMAL FORMAT ">>9.99"
    FIELD aliquota-ipi       AS   DECIMAL FORMAT ">>9.99"
    FIELD aliquota-icms      AS   DECIMAL FORMAT ">>9.99"
    field l-regime           AS   LOGICAL
    FIELD l-ordens-embarque  AS   LOGICAL
    field un                 as   character
    field qtd-do-forn        as   decimal format ">>>>,>>9.9999"
    field un-fornec          as   CHARACTER

    /******************************************************
    * Engenharia IPI Suspensao Incorporar Base PIS COFINS *
    ******************************************************/
    &IF DEFINED(bf-mat-comex) &THEN
    &ELSE
        FIELD suspensao-II  AS LOGICAL
        FIELD suspensao-IPI AS LOGICAL
    &ENDIF
    
    /* campo incluso para filtrar o browse da tela do im0100a */
    FIELD l-selecionado     AS LOGICAL

    index ordem is primary unique
          numero-ordem 
          parcela 
          sequencial.


/* BOCX220C.I */

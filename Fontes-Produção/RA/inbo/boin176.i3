/********************************************************************************
*
*    BOIN176.I3 - Defini‡Æo TT-ORDEM-ITEM
*                 Utilizado na Gera‡Æo de Itens a Partir da Ordem de Compra
*
*********************************************************************************/

def temp-table {1} no-undo
    field rw-ordem-compra   as rowid
    field rw-item-doc-est   as rowid
    field quant-fornec      like item-doc-est.quantidade
    field quantidade        like item-doc-est.quantidade
    field preco-total       like item-doc-est.preco-total extent 0
    field cod-depos         like item-doc-est.cod-depos
    field aliquota-ipi      like item-doc-est.aliquota-ipi
    field class-fiscal      like item-doc-est.class-fiscal.
    
    
/* fim da include */    

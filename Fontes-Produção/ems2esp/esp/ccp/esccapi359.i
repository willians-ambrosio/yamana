DEFINE TEMP-TABLE tt-pedido-compr-processado NO-UNDO
    FIELD num-pedido   LIKE pedido-compr.num-pedido  
    FIELD data-pedido  LIKE pedido-compr.data-pedido 
    FIELD cod-emitente LIKE pedido-compr.cod-emitente
    FIELD cod-estabel  LIKE pedido-compr.cod-estabel 
    FIELD responsavel  LIKE pedido-compr.responsavel 
    FIELD situacao     LIKE pedido-compr.situacao    
    FIELD r-rowid      AS ROWID.

DEFINE TEMP-TABLE tt-ordem-compra-processado NO-UNDO
    FIELD numero-ordem  LIKE ordem-compra.numero-ordem
    FIELD num-pedido    LIKE ordem-compra.num-pedido
    FIELD it-codigo     LIKE ordem-compra.it-codigo
    FIELD qt-solic      LIKE ordem-compra.qt-solic
    FIELD situacao      LIKE ordem-compra.situacao
    FIELD r-rowid       AS ROWID.

DEFINE TEMP-TABLE tt-prazo-compra-processado NO-UNDO LIKE prazo-compra
    FIELD r-rowid       AS ROWID.

DEFINE TEMP-TABLE tt-ordem-compra NO-UNDO LIKE ordem-compra
    FIELD r-rowid       AS ROWID.

DEFINE TEMP-TABLE tt-prazo-compra NO-UNDO LIKE tt-prazo-compra-processado.
DEFINE TEMP-TABLE tt-prazo-compra-origem NO-UNDO LIKE tt-prazo-compra.
DEFINE TEMP-TABLE tt-prazo-compra-aux    NO-UNDO LIKE tt-prazo-compra.

DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence     AS INTEGER
    FIELD ErrorNumber       AS INTEGER
    FIELD ErrorDescription  AS CHARACTER
    FIELD ErrorParameters   AS CHARACTER
    FIELD ErrorType         AS CHARACTER
    FIELD ErrorHelp         AS CHARACTER
    FIELD ErrorSubType      AS CHARACTER
    index seq ErrorSequence.

DEFINE TEMP-TABLE RowErrorsParcelas NO-UNDO LIKE RowErrors
    FIELD numero-ordem  LIKE ordem-compra.numero-ordem
    FIELD parcela       LIKE prazo-compra.parcela
    FIELD tipo          AS CHARACTER.


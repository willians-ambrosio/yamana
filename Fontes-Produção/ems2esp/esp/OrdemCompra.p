/*******************************************************************************
Extracao de Usuario Material
Kraft Consulting
*******************************************************************************/

/*{D:\Fontes\quarentena\Kraft\KRAFT.I}*/
{esp\KRAFT.I}

DEFINE INPUT PARAMETER p-dt-inicial AS DATE NO-UNDO.
DEFINE INPUT PARAMETER p-dt-final AS DATE NO-UNDO.
DEFINE INPUT PARAMETER pHandle AS HANDLE NO-UNDO.

DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.

FOR EACH ordem-compra NO-LOCK 
    WHERE ordem-compra.data-emissao >= p-dt-inicial
      AND ordem-compra.data-emissao <= p-dt-final
    USE-INDEX data-emissao:

    FIND ITEM USE-INDEX codigo
        WHERE ITEM.it-codigo = ordem-compra.it-codigo NO-LOCK NO-ERROR.

    IF NOT AVAIL ITEM THEN NEXT.

    FIND FIRST es_ordem_compra
        WHERE es_ordem_compra.codEstabelecimento    = ordem-compra.cod-estabel
          AND es_ordem_compra.numeroOrde            = ordem-compra.numero-ordem NO-ERROR.

    IF NOT AVAIL es_ordem_compra THEN DO:
        CREATE es_ordem_compra.
        ASSIGN es_ordem_compra.codEstabelecimento   = string(ordem-compra.cod-estabel)
               es_ordem_compra.numeroOrde           = ordem-compra.numero-ordem.
    END.
    
    
    ASSIGN es_ordem_compra.codEmitente          = (ordem-compra.cod-emitente)
           es_ordem_compra.codItem              = string(ordem-compra.it-codigo)
           es_ordem_compra.Natureza             = STRING(ordem-compra.natureza)
           es_ordem_compra.codContaContabil     = STRING(ordem-compra.conta-contabil)
           es_ordem_compra.codOrdemInvestimento = (ordem-compra.num-ord-inv)
           es_ordem_compra.codUsuarioComprador  = STRING(ordem-compra.cod-comprado)
           es_ordem_compra.codContrato          = (ordem-compra.nr-contrato)
           es_ordem_compra.DataEmissao          = (ordem-compra.data-emissao)
           es_ordem_compra.codMoeda             = (ordem-compra.mo-codigo)
           es_ordem_compra.codPedidoCompra      = (ordem-compra.num-pedido)
           es_ordem_compra.UnidadeMedida        = STRING(ITEM.un)
           es_ordem_compra.Situacao             = STRING({ininc/i02in274.i 4 ordem-compra.situacao})
           es_ordem_compra.Status_              = STRING({ininc/i05in065.i 4 sit-ordem-contrat})
           es_ordem_compra.precoUnitario        = DECIMAL(ordem-compra.preco-unit)
           es_ordem_compra.quantidade           = DECIMAL(ordem-compra.qt-solic) .
           
    
END. /*MOVTO-ESTOQ*/


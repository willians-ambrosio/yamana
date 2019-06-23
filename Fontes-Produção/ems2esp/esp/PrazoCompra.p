/*******************************************************************************
Extracao de Pedido de Compra
Kraft Consulting
17/01/2011
*******************************************************************************/

{esp\KRAFT.I}

DEFINE INPUT PARAMETER p-dt-inicial AS DATE NO-UNDO.
DEFINE INPUT PARAMETER p-dt-final AS DATE NO-UNDO.
DEFINE INPUT PARAMETER pHandle AS HANDLE NO-UNDO.

def new global shared var i-ep-codigo-usuario  like mguni.empresa.ep-codigo no-undo.
FOR EACH estabelec NO-LOCK WHERE estabelec.ep-codigo = i-ep-codigo-usuario:
    DELETE FROM es_prazo_compr
          WHERE es_prazo_compr.estab        = estabelec.cod-estabel. /*modificado por Daniel 09/09/11 
                                                                    pois deve ser apagado todo registro 
                                                                    do estabelecimento e nÆo apenas os
                                                                    dentro do periodo*/                    
/*             AND es_prazo_compr.dataEntrega >= p-dt-inicial */
/*             AND es_prazo_compr.dataEntrega <= p-dt-final.  */
END.

FOR EACH  prazo-compr FIELDS(numero-ordem parcela data-entrega quant-receb) NO-LOCK
    WHERE prazo-compr.data-entrega >= p-dt-inicial 
    AND   prazo-compr.data-entrega <= p-dt-final,
    FIRST ordem-compra FIELDS(cod-estabel) NO-LOCK OF prazo-compr
    WHERE ordem-compra.ep-codigo = i-ep-codigo-usuario:

    Create es_prazo_compr.
    Assign es_prazo_compr.codOrdemCompra   = prazo-compr.numero-ordem
           es_prazo_compr.parcela          = prazo-compr.parcela
           es_prazo_compr.dataEntrega      = prazo-compr.data-entrega
           es_prazo_compr.quantidade       = prazo-compr.quant-receb
           es_prazo_compr.estab            = ordem-compra.cod-estabel.

END.


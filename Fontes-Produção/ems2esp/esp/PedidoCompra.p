/*******************************************************************************
Extracao de Pedido de Compra
Kraft Consulting
17/01/2011
*******************************************************************************/

/*{D:\Fontes\quarentena\Kraft\KRAFT.I}*/
{esp\KRAFT.I}

DEFINE INPUT PARAMETER p-dt-inicial AS DATE NO-UNDO.    
DEFINE INPUT PARAMETER p-dt-final AS DATE NO-UNDO.    
DEFINE INPUT PARAMETER pHandle AS HANDLE NO-UNDO.

DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.

DEF VAR i AS INTEGER INITIAL 1.

FOR EACH pedido-compr 
    WHERE pedido-compr.data-pedido >= p-dt-inicial 
      AND pedido-compr.data-pedido <= p-dt-final NO-LOCK:

    FIND FIRST doc-pend-aprov 
         WHERE doc-pend-aprov.num-pedido = pedido-compr.num-pedido NO-LOCK NO-ERROR.

    FIND FIRST es_ped_compra
        WHERE  es_ped_compra.numeroPedido   = pedido-compr.num-pedido NO-ERROR.

    IF NOT AVAIL es_ped_compra THEN DO:
        Create es_ped_compra.
        Assign es_ped_compra.NumeroPedido       = (pedido-compr.num-pedido). /*chave primaria e unica*/
        
    END.
    
    ASSIGN es_ped_compra.codEmitente        = (pedido-compr.cod-emitente)
           es_ped_compra.codEstabelecimentoEntrega  = string(pedido-compr.end-entrega)
           es_ped_compra.DataPedido         = (pedido-compr.data-pedido)
           es_ped_compra.Emergencial        = IF pedido-compr.emergencial = YES THEN 1 ELSE 2  /* YES "EMERGENCIAL" NO "N/EMERGENCIAL"*/
           es_ped_compra.codComprador       = (pedido-compr.responsavel)
           es_ped_compra.DataAprovacao      = IF AVAIL doc-pend-aprov THEN (doc-pend-aprov.dt-aprova) ELSE 01/01/1900
           es_ped_compra.codContrato        = (pedido-compr.nr-contrato) .
END.





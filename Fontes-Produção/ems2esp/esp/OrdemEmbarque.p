/*******************************************************************************
Extracao de Ordem Embarque
Kraft Consulting
29/12/2010
*******************************************************************************/

/*{D:\Fontes\quarentena\Kraft\KRAFT.I}*/
{esp\KRAFT.I}

DEF VAR c-arquivo AS CHAR NO-UNDO.

FOR EACH ordens-embarque NO-LOCK:

    FIND FIRST es_ordem_embarque
        WHERE es_ordem_embarque.codEstabelecimento  = ordens-embarque.cod-estabel 
          AND es_ordem_embarque.codImportacao       = ordens-embarque.embarque    
          AND es_ordem_embarque.codOrdemCompra      = ordens-embarque.numero-ordem
          AND es_ordem_embarque.Parcela             = ordens-embarque.parcela NO-ERROR.    

    IF NOT AVAIL es_ordem_embarque THEN DO:
        CREATE es_ordem_embarque.
        ASSIGN es_ordem_embarque.codEstabelecimento = ordens-embarque.cod-estabel  /*chave primaria e unica*/
               es_ordem_embarque.codImportacao      = ordens-embarque.embarque     /*chave primaria e unica*/
               es_ordem_embarque.codOrdemCompra     = ordens-embarque.numero-ordem /*chave primaria e unica*/
               es_ordem_embarque.Parcela            = ordens-embarque.parcela.     /*chave primaria e unica*/
    END.
    ASSIGN
       es_ordem_embarque.quantidade = ordens-embarque.quantidade.

    
END.





/*******************************************************************************
Extracao de Recebimento
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

DEF BUFFER b-docum-est FOR docum-est.

FOR EACH docum-est 
    WHERE docum-est.dt-emissao >= p-dt-inicial
      AND docum-est.dt-emissao <= p-dt-final NO-LOCK:

    RUN pi-acompanhar IN pHandle(INPUT docum-est.nro-docto + " " + STRING(docum-est.dt-emissao)).

    FOR EACH item-doc-est OF docum-est NO-LOCK.

        FIND FIRST es_recebimento
            WHERE es_recebimento.serieDocumento     = item-doc-est.serie-docto
              AND es_recebimento.NumeroDocumento    = item-doc-est.nro-docto
              AND es_recebimento.codEmitente        = item-doc-est.cod-emitente
              AND es_recebimento.codNaturezaOper    = item-doc-est.nat-operacao
              AND es_recebimento.sequencia          = item-doc-est.sequencia NO-ERROR.
    
        IF NOT AVAIL es_recebimento THEN DO:
            Create es_recebimento.
            ASSIGN es_recebimento.serieDocumento     = item-doc-est.serie-docto
                   es_recebimento.NumeroDocumento    = item-doc-est.nro-docto
                   es_recebimento.codEmitente        = item-doc-est.cod-emitente
                   es_recebimento.codNaturezaOper    = item-doc-est.nat-operacao
                   es_recebimento.sequencia          = item-doc-est.sequencia.
    
        END.
    
        Assign es_recebimento.codItem               = item-doc-est.it-codigo
               es_recebimento.codEstabel            = (docum-est.cod-estabel)  
               es_recebimento.DataDocumento         = (docum-est.dt-emissao)
               es_recebimento.DataTransacao         = (docum-est.dt-trans)          /*chave primaria e unica*/
               es_recebimento.DataEmissao           = (docum-est.dt-emissao)   
               es_recebimento.numPedido             = item-doc-est.num-pedido       /*chave primaria e unica*/
               es_recebimento.parcela               = item-doc-est.parcela          /*chave primaria e unica*/
               es_recebimento.codOrdemCompra        = (item-doc-est.numero-ordem)   /*chave primaria e unica*/
               es_recebimento.codContaContabil      = (item-doc-est.conta-contab)
               es_recebimento.ValorTotal            = DECIMAL(item-doc-est.preco-total[1] + item-doc-est.valor-ipi[1])
               es_recebimento.quantidade            = DECIMAL(item-doc-est.quantidade)
               es_recebimento.codembarque           = TRIM(SUBSTR(docum-est.char-1,1,12)).

        FIND FIRST natur-oper
            WHERE natur-oper.nat-operacao = item-doc-est.nat-operacao NO-LOCK NO-ERROR.

        IF  AVAIL natur-oper THEN DO:
            IF natur-oper.tipo = 1 THEN         
                ASSIGN es_recebimento.EntradaSaida   = "Entrada".
            ELSE 
                ASSIGN es_recebimento.EntradaSaida = "Saida".
        END.

        IF  es_recebimento.codembarque <> "" AND
            NOT CAN-FIND ( FIRST embarque-imp NO-LOCK 
                           WHERE embarque-imp.embarque = es_recebimento.codembarque ) THEN DO:

            rateio:
            for each rat-docum FIELDS() NO-LOCK use-index documento
               where rat-docum.serie-docto  = docum-est.serie-docto 
                 and rat-docum.nro-docto    = docum-est.nro-docto   
                 and rat-docum.cod-emitente = docum-est.cod-emitente
                 and rat-docum.nat-operacao = docum-est.nat-operacao,
                each b-docum-est fields( char-1 ) NO-LOCK
               where b-docum-est.serie-docto  = rat-docum.nf-serie   
                 and b-docum-est.nro-docto    = rat-docum.nf-nro     
                 and b-docum-est.cod-emitente = rat-docum.nf-emitente
                 and b-docum-est.nat-operacao = rat-docum.nf-nat-oper:
                     es_recebimento.codembarque = TRIM(SUBSTR(b-docum-est.char-1,1,12)).
                     LEAVE rateio.
            END.

        END.

    END.

END.



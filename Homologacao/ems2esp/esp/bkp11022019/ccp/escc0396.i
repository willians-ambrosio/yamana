define temp-table tt-param  no-undo
    field destino           as integer
    field arquivo           as char format "x(35)":U
    field usuario           as char format "x(12)":U
    field data-exec         as date
    field hora-exec         as integer
    field classifica        as integer
    field desc-classifica   as char format "x(40)":U
    field modelo            AS char format "x(35)":U
    /*Alterado 15/02/2005 - tech1007 - Criado campo l¢gico para verificar se o RTF foi habilitado*/
    field l-habilitaRtf     as LOG
    field execution         as int   
    /*******************************/
    FIELD iPedidoIni           LIKE pedido-compr.num-pedido
    FIELD iPedidoFim           LIKE pedido-compr.num-pedido
    FIELD daDataIni            LIKE pedido-compr.data-pedido
    FIELD daDataFim            LIKE pedido-compr.data-pedido
    FIELD iEmitenteIni         LIKE pedido-compr.cod-emitente
    FIELD iEmitenteFim         LIKE pedido-compr.cod-emitente
    FIELD cEstabelecIni        LIKE pedido-compr.cod-estabel
    FIELD cEstabelecFim        LIKE pedido-compr.cod-estabel
    FIELD cResponsavelIni      LIKE pedido-compr.responsavel
    FIELD cResponsavelFim      LIKE pedido-compr.responsavel
    FIELD iOrdemIni            LIKE ordem-compra.numero-ordem
    FIELD iOrdemFim            LIKE ordem-compra.numero-ordem
    FIELD iParcelaIni          LIKE prazo-compra.parcela
    FIELD iParcelaFim          LIKE prazo-compra.parcela
    FIELD daDataEntregaIni     LIKE prazo-compra.data-entrega
    FIELD daDataEntregaFim     LIKE prazo-compra.data-entrega
    FIELD lZeraSaldo           AS LOGICAL
    FIELD lAtualizaSituacao    AS LOGICAL
    FIELD lAtualizaParcLote    AS LOGICAL
    FIELD lAtualizaParcLoteMin AS LOGICAL
    FIELD lNGeraPend           AS LOGICAL
    FIELD lSimula              AS LOGICAL
    FIELD daIntervaloIni       AS DATE FORMAT "99/99/9999"
    FIELD daIntervaloFim       AS DATE FORMAT "99/99/9999"
.

def temp-table tt-raw-digita
   field raw-digita      as raw.

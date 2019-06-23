define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field dat-ordem-ini    like ordem-compra.dat-ordem
    field dat-ordem-fim    like ordem-compra.dat-ordem
    field cod-estabel-ini  like ordem-compra.cod-estabel
    field cod-estabel-fim  like ordem-compra.cod-estabel
    field fm-codigo-ini    like item.fm-codigo
    field fm-codigo-fim    like item.fm-codigo
    field it-codigo-ini    like ordem-compra.it-codigo
    field it-codigo-fim    like ordem-compra.it-codigo
    field cod-comprado-ini like ordem-compra.cod-comprado
    field cod-comprado-fim like ordem-compra.cod-comprado
    field nr-contrato-ini  like ordem-compra.nr-contrato
    field nr-contrato-fim  like ordem-compra.nr-contrato
    FIELD i-execucao       AS   INTEGER
    FIELDS l-csv           AS   LOGICAL.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

define buffer b-tt-digita for tt-digita.

DEF TEMP-TABLE  tt-movto
    FIELD  tt-cod-estabel       LIKE ordem-compra.cod-estabel 
    FIELD  tt-fm-codigo         AS   CHAR FORMAT "X(8)" /*LIKE item.fm-codigo */   
    FIELD  tt-nr-ord-produc     LIKE req-ord-produc.nr-ord-produ    
    FIELD  tt-it-codigo         AS   CHAR FORMAT "x(16)" /* LIKE item.it-codigo */                 
    FIELD  tt-qt-requisitada    LIKE it-requisicao.qt-requisitada   
    FIELD  tt-nr-requisicao     LIKE ordem-compra.nr-requisicao     
    FIELD  tt-numero-ordem      LIKE ordem-compra.numero-ordem      
    FIELD  tt-num-pedido        LIKE ordem-compra.num-pedido        
    FIELD  tt-preco-fornec      LIKE ordem-compra.preco-fornec      
    FIELD  tt-data-entrega      LIKE prazo-compra.data-entrega      
    FIELD  tt-situacao          LIKE prazo-compra.situacao             
    FIELD  tt-ct-codigo         LIKE mmv-ord-manut.ct-codigo        
    FIELD  tt-cc-codigo         LIKE mmv-ord-manut.cc-codigo        
    FIELD  tt-num-ord-inv       LIKE ordem-compra.num-ord-inv       
    FIELD  tt-desc-item         LIKE item.desc-item
    FIELD  tt-cod-emit          LIKE ordem-compra.cod-emitente
    FIELD  tt-quant-receb       LIKE prazo-compra.quant-receb
    FIELD  tt-preco-unit        LIKE ordem-compra.preco-unit
    FIELD  tt-aliquota-ipi      LIKE ordem-compra.aliquota-ipi      
    FIELD  tt-aliquota-icm      LIKE ordem-compra.aliquota-icm 
    FIELD  tt-aliquota-iss      LIKE ordem-compra.aliquota-iss 
    FIELD  tt-valor-frete       LIKE ordem-compra.valor-frete
    FIELD  tt-preco-orig        LIKE ordem-compra.preco-orig
    FIELD  tt-data-emis         LIKE ordem-compra.data-emis
    FIELD  tt-data-pedido       LIKE ordem-compra.data-pedido
    FIELD  tt-qtidade-atu       LIKE saldo-estoq.qtidade-atu
    FIELD  tt-dt-emissao        LIKE doc-fisico.dt-emissao
    FIELD  tt-dt-trans          LIKE doc-fisico.dt-trans
    FIELD  tt-perc-descto       LIKE ordem-compra.perc-descto
    FIELD  tt-prazo-entreg      LIKE cotacao-item.prazo-entreg
    FIELD  tt-cod-comprado      LIKE ordem-compra.cod-comprado
    FIELD  tt-dat-trans         LIKE req-ord-produc.dat-trans
    FIELD  tt-emergencial       AS   CHAR FORMAT "x(3)"
    FIELD  tt-requisitante      LIKE requisicao.nome-abrev 
    FIELD  tt-dt-requisicao     LIKE requisicao.dt-requisicao
    FIELD  tt-dt-aprov-req      LIKE ordem-compra.dat-ordem
    FIELD  tt-dt-ordem          LIKE ordem-compra.dat-ordem
    field  tt-nr-contrato       like ordem-compra.nr-contrato
    FIELD  tt-cond-pag          AS   CHAR
    FIELD  tt-ge-codigo         LIKE ITEM.ge-codigo
    FIELD  tt-un                LIKE ITEM.un
    FIELD  tt-des-serv-mat      AS CHAR
    FIELD  tt-cod-obsoleto      AS CHAR
    FIELD  tt-tipo-contr        AS CHAR
    FIELD  tt-depto             AS CHAR.

/* Transfer Definitions */
def temp-table tt-raw-digita
   field raw-digita      as raw.

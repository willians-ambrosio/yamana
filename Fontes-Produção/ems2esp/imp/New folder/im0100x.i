/*---[ im0100x.i - Include Parƒmetros Valida‡Æo Desembarque/Gera‡Æo Documentos Entrada (im0100) ]---*/ 

def temp-table tt-parametros no-undo
    field cod-estabel           like estabelec.cod-estabel
    field embarque              like embarque-imp.embarque
    field cod-emitente          like emitente.cod-emitente
    field da-transacao          as   date
    field da-emissao            as   date
    field da-venc-ipi           as   date
    field da-venc-icms          as   date
    field nro-docto             like docum-est.nro-docto
    field serie                 like docum-est.serie
    field nat-operacao          like docum-est.nat-operacao
    field estab-fisc            like processo-imp.estab-fisc
    /*field conta-transit         like param-imp.conta-transit-import*/
    FIELD ct-transit            AS CHAR FORMAT "x(20)"
    FIELD sc-transit            AS CHAR FORMAT "x(20)"
    FIELD ct-codigo             AS CHAR FORMAT "X(20)"
    FIELD sc-codigo             AS CHAR FORMAT "X(20)"
    field declaracao-import     like embarque-imp.declaracao-import
    field da-di                 as   date
    field de-tot-desconto       like docum-est.tot-valor
    field da-cotacao            as   date
    field observacao            as   char format "x(2000)"
    field usuario               like param-re.usuario
    field i-pais-impto-usuario  as   integer
    field l-chama-carregado     as   logical
    field l-icms-diferido       as   logical
    
    field l-nf-simples-remessa  as   logical
    field idi-nf-simples-remes  as   integer /* like docum-est.idi-nf-simples-remes */
    field conta-contabil        AS   CHAR FORMAT "x(17)":U
    field num-di-ems            AS   INT
    
    field cod-emitente-desp     like emitente.cod-emitente
    field estado-emite          like emitente.estado
    field aliquota-ipi          as   decimal format ">>9.99"
    field aliquota-icms         as   decimal format ">>9.99"
    field i-tributacao-ipi      as   integer
    field i-tributacao-icms     as   integer
    field cod-observacao        as   integer
    field l-acomp               as   logical
    field l-agrupar-unico-forn  as   logical
    field chave-nfe             AS   CHARACTER FORMAT "x(60)"
    
    field nff                   LIKE docum-est.nff
    
    index codigo is unique primary
        cod-estabel
        embarque.

define temp-table tt-itens-aux-serie no-undo
    field numero-ordem      like ordens-embarque.numero-ordem
    field parcela           like ordens-embarque.parcela
    field preco-unit        like item-doc-est.preco-unit  extent 0
    field preco-unit-acum   like item-doc-est.preco-unit  extent 0
    field preco-total-acum  like item-doc-est.preco-unit  extent 0
    field peso-liquido      like item-doc-est.peso-liquido
    field peso-liquido-acum like item-doc-est.peso-liquido
    field peso-bruto        like item-doc-est.peso-liquido
    field peso-bruto-acum   like item-doc-est.peso-liquido
    index id
          numero-ordem
          parcela.

def temp-table {1}-aux no-undo
    field cod-estabel           like estabelec.cod-estabel
    field embarque              like embarque-imp.embarque
    field cod-emitente          like emitente.cod-emitente
    field da-transacao          as   date
    field da-emissao            as   date
    field da-venc-ipi           as   date
    field da-venc-icms          as   date
    field nro-docto             like docum-est.nro-docto
    field serie                 like docum-est.serie
    field nat-operacao          like docum-est.nat-operacao
    field estab-fisc            like processo-imp.estab-fisc
    field conta-transit         like param-imp.conta-transit-import
    field declaracao-import     like embarque-imp.declaracao-import
    field da-di                 as   date
    field de-tot-desconto       like docum-est.tot-valor
    field da-cotacao            as   date
    field observacao            as   char format "x(2000)"
    field usuario               like param-re.usuario
    field i-pais-impto-usuario  as   integer
    field l-chama-carregado     as   logical
    field l-icms-diferido       as   logical
    
    field l-nf-simples-remessa  as   logical
    field idi-nf-simples-remes  as   integer /* like docum-est.idi-nf-simples-remes */
    field conta-contabil        AS   CHAR FORMAT "x(17)":U
    field num-di-ems            AS   INT
    
    field cod-emitente-desp     like emitente.cod-emitente
    field estado-emite          like emitente.estado
    field aliquota-ipi          as   decimal format ">>9.99"
    field aliquota-icms         as   decimal format ">>9.99"
    field i-tributacao-ipi      as   integer
    field i-tributacao-icms     as   integer
    field cod-observacao        as   integer
    field l-acomp               as   logical
    field l-agrupar-unico-forn  as   logical
    field chave-nfe             AS   CHARACTER FORMAT "x(60)"
    
    field nff                   LIKE docum-est.nff
    
    field imp-nota              LIKE natur-oper.imp-nota
    field inc-seq               LIKE param-re.inc-seq
    field seq-item-um           LIKE param-re.seq-item-um
    field valor-duplic          LIKE docum-est.valor-mercad EXTENT 0
    field valor-mercad          LIKE docum-est.valor-mercad EXTENT 0
    field cod-cond-pag          LIKE cond-pagto.cod-cond-pag
    field emite-duplic          AS LOGICAL
    field mo-codigo             LIKE moeda.mo-codigo
    field valor-duplic-me       LIKE docum-est.valor-mercad EXTENT 0
    field da-partida            AS DATE FORMAT "99/99/9999":U
    field tp-despesa            AS INTEGER
    field esp-tit-rec           LIKE param-imp.esp-tit-rec
    field consum-final          LIKE natur-oper.consum-final
    field cod-mensagem          LIKE natur-oper.cod-mensagem
    field natur-oper-char-2     LIKE natur-oper.char-2
    field cd-trib-iss           LIKE natur-oper.cd-trib-iss
    field cotacao-dia           LIKE cotacao.cotacao EXTENT 0
    field estado-estab          LIKE estabelec.estado
    field cidade                LIKE estabelec.cidade
    field pais                  LIKE estabelec.pais
    field portador              LIKE emitente.portador
    field modalidade            LIKE emitente.modalidade
    field nome-abrev            LIKE emitente.nome-abrev
    field natureza              LIKE emitente.natureza

    index codigo is unique primary
        cod-estabel
        embarque.


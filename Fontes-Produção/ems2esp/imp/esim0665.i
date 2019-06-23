/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/******************************************************************************
**
**     Include ..: esim0665.i 
**     Objetivo .: Cont‚m os a temp table de parametros 
**                 e as forms do programa esIM0665 .
**
*******************************************************************************/
DEF VAR c-destino    AS CHAR LABEL "Destino" NO-UNDO.
def var c-desc-moeda like moeda.descricao    no-undo.

define temp-table tt-param no-undo
    field destino                as integer
    field arquivo                as char
    field usuario                as char format "x(12)" LABEL "Usu rio"
    field data-exec              as DATE FORMAT "99/99/9999"
    field hora-exec              as integer
    field classifica             as int 
    field desc-classifica        as CHAR FORMAT "x(40)"
    field c-cod-estabel-ini      like estabelec.cod-estabel LABEL "Estabelecimento"
    field c-cod-estabel-fim      like estabelec.cod-estabel
    field c-embarque-ini         like embarque-imp.embarque
    field c-embarque-fim         like embarque-imp.embarque
    field c-it-codigo-ini        like item.it-codigo
    field c-it-codigo-fim        like item.it-codigo
    field i-ge-codigo-ini        like grup-estoque.ge-codigo
    field i-ge-codigo-fim        like grup-estoque.ge-codigo
    field c-fm-codigo-ini        like familia.fm-codigo
    field c-fm-codigo-fim        like familia.fm-codigo
    field i-cod-emitente-ini     like emitente.cod-emitente LABEL "Fornecedor(Exportador)"
    field i-cod-emitente-fim     like emitente.cod-emitente
    field c-nr-proc-imp-ini      like processo-imp.nr-proc-imp LABEL "Processo"
    field c-nr-proc-imp-fim      like processo-imp.nr-proc-imp
    field i-cod-itiner-ini       like itinerario.cod-itiner   LABEL "Itiner rio"
    field i-cod-itiner-fim       like itinerario.cod-itiner
    field da-dt-nac-ini          as date    label "Data Nacionaliza‡Æo"             FORMAT "99/99/9999"
    field da-dt-nac-fim          as date                                            FORMAT "99/99/9999"
    field moeda                  like moeda.mo-codigo
    field da-dt-conv             as date    label "Data ConversÆo"                  FORMAT "99/99/9999"
    field l-dt-nacional          as logical label "Data de Nacionaliza‡Æo"          FORMAT "sim/nÆo"
    field l-orc-real             as logical label "Or‡ado X Realizado"              FORMAT "sim/nÆo"
    field l-desp-imposto         as logical label "Detalha Despesas/Impostos"       FORMAT "sim/nÆo"
    field l-itens                as logical label "Detalha Itens"                   FORMAT "sim/nÆo"
    field l-desemb-parc          as logical label "Considera Desembarque Parcial"   FORMAT "sim/nÆo"
    field l-nf-compl             as logical label "Considera NF Complementar"       FORMAT "sim/nÆo"
    field i-custo-item           as int     
    FIELD c-custo-item           AS CHAR    LABEL "Custo Item"
    field i-desp-imposto         as int
    FIELD c-desp-imposto         AS CHAR    LABEL "Considera".
    
define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

def temp-table tt-raw-digita no-undo
    field raw-digita as raw.

define temp-table tt-detalha-item no-undo
    FIELD sequencia                         LIKE item-doc-est.sequencia
    field embarque                          like embarque-imp.embarque
    field embarque-orig                     like embarque-imp.embarque
    field cod-estabel                       like embarque-imp.cod-estabel
    field nro-docto                         like item-doc-est.nro-docto
    field it-codigo                         like item.it-codigo
    field descricao                         like item.desc-item
    field numero-ordem                      like ordem-compra.numero-ordem
    FIELD parcela                           LIKE ordens-embarque.parcela
    field quantidade                        like item-doc-est.quantidade  
    field un                                like item.un
    field desc-despesa                      like desp-imp.descricao
    field situacao                          as   int
    field aliquota-ii                       as   decimal format ">>9.99":U
    field aliquota-ipi                      as   decimal format ">>9.99":U
    field aliquota-icms                     as   decimal format ">>9.99":U
    field aliquota-pis                      as   decimal format ">>9.99":U
    field aliquota-cofins                   as   decimal format ">>9.99":U
    field aliquota-ii2                      as   decimal format ">>9.99":U
    field vl-mercado-orc                    as   decimal format ">>>,>>>,>>9.99999":U
    field vl-mercado-orc-total              as   decimal format ">>>,>>>,>>9.99999":U
    field vl-ii-orc                         as   decimal format ">>>,>>>,>>9.99999":U 
    field vl-ipi-orc                        as   decimal format ">>>,>>>,>>9.99999":U
    field vl-icms-orc                       as   decimal format ">>>,>>>,>>9.99999":U
    field vl-pis-orc                        as   decimal format ">>>,>>>,>>9.99999":U
    field vl-cofins-orc                     as   decimal format ">>>,>>>,>>9.99999":U
    field vl-tot-imp-orc                    as   decimal format ">>>,>>>,>>9.99999":U
    field vl-ii2-orc                        as   decimal format ">>>,>>>,>>9.99999":U
    field vl-ii2-orc-aux                    as   decimal format ">>>,>>>,>>9.99999":U
    field vl-ii-orc-aux                     as   decimal format ">>>,>>>,>>9.99999":U
    field vl-des-orc                        as   decimal format ">>>,>>>,>>9.99999":U
    field vl-des-orc-total                  as   decimal format ">>>,>>>,>>9.99999":U
    field vl-tot-des-orc                    as   decimal format ">>>,>>>,>>9.99999":U
    field vl-tot-des-orc-total              as   decimal format ">>>,>>>,>>9.99999":U
    field vl-tot-imp-des-orc                as   decimal format ">>>,>>>,>>9.99999":U
    field vl-tot-imp-des-orc-total          as   decimal format ">>>,>>>,>>9.99999":U
    field vl-tot-com-ipi-icms-orc           as   decimal format ">>>,>>>,>>9.99999":U
    field vl-tot-com-ipi-icms-orc-total     as   decimal format ">>>,>>>,>>9.99999":U
    field vl-tot-sem-ipi-icms-orc           as   decimal format ">>>,>>>,>>9.99999":U
    field vl-tot-sem-ipi-icms-orc-total     as   decimal format ">>>,>>>,>>9.99999":U
    field vl-mercado-rea                    as   decimal format ">>>,>>>,>>9.99999":U
    field vl-mercado-rea-total              as   decimal format ">>>,>>>,>>9.99999":U
    field vl-ii-rea                         as   decimal format ">>>,>>>,>>9.99999":U
    field vl-ii-rea-total                   as   decimal format ">>>,>>>,>>9.99999":U
    field vl-ipi-rea                        as   decimal format ">>>,>>>,>>9.99999":U
    field vl-ipi-rea-total                  as   decimal format ">>>,>>>,>>9.99999":U
    field vl-icms-rea                       as   decimal format ">>>,>>>,>>9.99999":U
    field vl-pis-rea                        as   decimal format ">>>,>>>,>>9.99999":U
    field vl-cofins-rea                     as   decimal format ">>>,>>>,>>9.99999":U
    field vl-icms-rea-total                 as   decimal format ">>>,>>>,>>9.99999":U
    field vl-tot-imp-rea                    as   decimal format ">>>,>>>,>>9.99999":U
    field vl-tot-imp-rea-total              as   decimal format ">>>,>>>,>>9.99999":U
    field vl-ii2-rea                        as   decimal format ">>>,>>>,>>9.99999":U
    field vl-ii2-rea-total                  as   decimal format ">>>,>>>,>>9.99999":U
    field vl-des-rea                        as   decimal format ">>>,>>>,>>9.99999":U
    field vl-des-rea-total                  as   decimal format ">>>,>>>,>>9.99999":U
    field vl-tot-des-rea                    as   decimal format ">>>,>>>,>>9.99999":U
    field vl-tot-des-rea-total              as   decimal format ">>>,>>>,>>9.99999":U
    field vl-tot-des-rea-total-aux          as   decimal format ">>>,>>>,>>9.99999":U
    field vl-tot-imp-des-rea                as   decimal format ">>>,>>>,>>9.99999":U
    field vl-tot-imp-des-rea-total          as   decimal format ">>>,>>>,>>9.99999":U
    field vl-tot-com-ipi-icms-rea           as   decimal format ">>>,>>>,>>9.99999":U
    field vl-tot-com-ipi-icms-rea-total     as   decimal format ">>>,>>>,>>9.99999":U
    field vl-tot-com-ipi-icms-dsp-merc-rea  as   decimal format ">>>,>>>,>>9.99999":U
    field vl-tot-sem-ipi-icms-dsp-merc-rea  as   decimal format ">>>,>>>,>>9.99999":U
    field vl-tot-sem-ipi-icms-rea           as   decimal format ">>>,>>>,>>9.99999":U
    field vl-tot-sem-ipi-icms-rea-total     as   decimal format ">>>,>>>,>>9.99999":U
    field per-mercado-orc                   as   decimal format ">>9.99":U  
    field per-mercado-rea                   as   decimal format ">>9.99":U  
    field per-ii-orc                        as   decimal format ">>9.99":U  
    field per-ii-rea                        as   decimal format ">>9.99":U  
    field per-ipi-orc                       as   decimal format ">>9.99":U  
    field per-ipi-rea                       as   decimal format ">>9.99":U  
    field per-icms-orc                      as   decimal format ">>9.99":U  
    field per-icms-rea                      as   decimal format ">>9.99":U  
    field per-tot-imp-orc                   as   decimal format ">>9.99":U  
    field per-tot-imp-rea                   as   decimal format ">>9.99":U  
    field per-ii2-orc                       as   decimal format ">>9.99":U  
    field per-ii2-rea                       as   decimal format ">>9.99":U  
    field per-des-orc                       as   decimal format ">>9.99":U  
    field per-des-rea                       as   decimal format ">>9.99":U  
    field per-tot-des-orc                   as   decimal format ">>9.99":U  
    field per-tot-des-rea                   as   decimal format ">>9.99":U  
    field per-tot-imp-des-orc               as   decimal format ">>9.99":U  
    field per-tot-imp-des-rea               as   decimal format ">>9.99":U  
    field per-tot-com-ipi-icms-orc          as   decimal format ">>9.99":U  
    field per-tot-com-ipi-icms-rea          as   decimal format ">>9.99":U  
    field per-tot-sem-ipi-icms-orc          as   decimal format ">>9.99":U  
    field per-tot-sem-ipi-icms-rea          as   decimal format ">>9.99":U  
    field vl-des-frete                      like desp-imp.val-desp
    field vl-des-embalagem                  like desp-imp.val-desp
    field vl-des-seguro                     like desp-imp.val-desp
    field vl-des-outras                     like desp-imp.val-desp
    field vl-tot-despesa                    like desp-imp.val-desp
    FIELD vl-pis-rea-total                  AS   DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD vl-cofins-rea-total               AS   DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD per-pis-rea                       AS   DECIMAL FORMAT ">>9.99"
    FIELD per-cofins-rea                    AS   DECIMAL FORMAT ">>9.99"
    FIELD per-pis-orc                       AS   DECIMAL FORMAT ">>9.99"
    FIELD i-numero-itens                    AS   INTEGER
    FIELD tot-quantidade                    LIKE item-doc-est.quantidade
    FIELD per-cofins-orc                    AS   DECIMAL FORMAT ">>9.99"
    index ch-codigo
          cod-estabel
          embarque
          it-codigo
          numero-ordem
          parcela
    index ch-item
          it-codigo
          embarque.

define temp-table tt-embarque-cabec  no-undo
    field cod-estabel    like estabelec.cod-estabel
    field desc-estab     as   char format "x(34)":U
    field embarque       like embarque-imp.embarque
    field cod-emitente   like emitente.cod-emitente
    field desc-emit      like emitente.nome-abrev
    field pais           like emitente.pais
    field cod-incoterm   like embarque-imp.cod-incoterm
    field cod-via-transp like embarque-imp.cod-via-transp
    field pto-embarque   like pto-contr.descricao
    field data-embarque  as   date format "99/99/9999":U
    field pto-nacional   like pto-contr.descricao
    field data-nacional  as   date format "99/99/9999":U
    field data-chegada   as   date format "99/99/9999":U
    field data-cotacao   as   date format "99/99/9999":U
    field desc-moeda     like moeda.descricao
    field mo-taxa        as   decimal format "99999.99999999":U
    field data-convercao as   date format "99/99/9999":U
    index ch-codigo is unique primary
          embarque 
          cod-estabel 
          cod-emitente.

define temp-table tt-embarque-corpo no-undo
    field embarque                          like embarque-imp.embarque
    field it-codigo                         like item.it-codigo
    field desc-item                         like item.desc-item
    field cod-estabel                       like embarque-imp.cod-estabel
    field desc-estabel                      like estabelec.nome
    field cod-emitente                      like processo-imp.cod-exportador
    field desc-emitente                     like emitente.nome-abrev
    field numero-ordem                      like ordem-compra.numero-ordem
    field nr-fatura                         like invoice-emb-imp.nr-invoice
    field parcela                           like invoice-emb-imp.parcela
    field dt-venc-fat                       like invoice-emb-imp.dt-vencim
    field nr-nota-fis                       like docum-est.nro-docto
    field dt-emissao                        like docum-est.dt-emissao
    field nr-di                             like embarque-imp.declaracao-import
    field dt-di                             like embarque-imp.data-di
    field embarque-rel                      like docum-est.nro-docto
    field nr-nota-rel                       like docum-est.nro-docto
    field dt-emis-rel                       as   date format "99/99/9999":U
    field nr-di-rel                         like embarque-imp.declaracao-import
    field dt-di-rel                         like embarque-imp.data-di
    field vl-ger-mercado-orc                as decimal format ">>>,>>>,>>9.99":U
    field vl-ger-ii-orc                     as decimal format ">>>,>>>,>>9.99":U
    field vl-ger-ipi-orc                    as decimal format ">>>,>>>,>>9.99":U
    field vl-ger-icms-orc                   as decimal format ">>>,>>>,>>9.99":U
    field vl-ger-tot-imp-orc                as decimal format ">>>,>>>,>>9.99":U
    field vl-ger-ii2-orc                    as decimal format ">>>,>>>,>>9.99":U
    field vl-ger-des-orc                    as decimal format ">>>,>>>,>>9.99":U
    field vl-ger-tot-des-orc                as decimal format ">>>,>>>,>>9.99":U
    field vl-ger-tot-des-orc-aux            as decimal format ">>>,>>>,>>9.99":U
    field vl-ger-tot-imp-des-orc            as decimal format ">>>,>>>,>>9.99":U
    field vl-ger-tot-com-ipi-icms-orc       as decimal format ">>>,>>>,>>9.99":U
    field vl-ger-tot-sem-ipi-icms-orc       as decimal format ">>>,>>>,>>9.99":U
    field per-ger-mercado-orc               as decimal format ">>9.99":U
    field per-ger-ii-orc                    as decimal format ">>9.99":U
    field per-ger-ipi-orc                   as decimal format ">>9.99":U
    field per-ger-icms-orc                  as decimal format ">>9.99":U
    field per-ger-tot-imp-orc               as decimal format ">>9.99":U
    field per-ger-ii2-orc                   as decimal format ">>9.99":U
    field per-ger-des-orc                   as decimal format ">>9.99":U
    field per-ger-tot-des-orc               as decimal format ">>9.99":U
    field per-ger-tot-imp-des-orc           as decimal format ">>9.99":U
    field per-ger-tot-com-ipi-icms-orc      as decimal format ">>9.99":U
    field per-ger-tot-sem-ipi-icms-orc      as decimal format ">>9.99":U
    field vl-ger-mercado-rea                as decimal format ">>>,>>>,>>9.99":U
    field vl-ger-ii-rea                     as decimal format ">>>,>>>,>>9.99":U
    field vl-ger-ipi-rea                    as decimal format ">>>,>>>,>>9.99":U
    field vl-ger-icms-rea                   as decimal format ">>>,>>>,>>9.99":U
    field vl-ger-tot-imp-rea                as decimal format ">>>,>>>,>>9.99":U
    field vl-ger-ii2-rea                    as decimal format ">>>,>>>,>>9.99":U
    field vl-ger-des-rea                    as decimal format ">>>,>>>,>>9.99":U
    field vl-ger-tot-des-rea                as decimal format ">>>,>>>,>>9.99":U
    field vl-ger-tot-imp-des-rea            as decimal format ">>>,>>>,>>9.99":U
    field vl-ger-tot-com-ipi-icms-rea       as decimal format ">>>,>>>,>>9.99":U
    field vl-ger-tot-sem-ipi-icms-rea       as decimal format ">>>,>>>,>>9.99":U
    field vl-ger-tot-com-ipi-icms-merc-rea  as decimal format ">>>,>>>,>>9.99":U
    field vl-ger-tot-sem-ipi-icms-merc-rea  as decimal format ">>>,>>>,>>9.99":U
    field per-ger-mercado-rea               as decimal format ">>9.99":U
    field per-ger-ii-rea                    as decimal format ">>9.99":U
    field per-ger-ipi-rea                   as decimal format ">>9.99":U
    field per-ger-icms-rea                  as decimal format ">>9.99":U
    field per-ger-tot-imp-rea               as decimal format ">>9.99":U
    field per-ger-ii2-rea                   as decimal format ">>9.99":U
    field per-ger-des-rea                   as decimal format ">>9.99":U
    field per-ger-tot-des-rea               as decimal format ">>9.99":U
    field per-ger-tot-imp-des-rea           as decimal format ">>9.99":U
    field per-ger-tot-com-ipi-icms-rea      as decimal format ">>9.99":U
    field per-ger-tot-sem-ipi-icms-rea      as decimal format ">>9.99":U
    field total-vl-frete                    as decimal format ">>>,>>>,>>9.99":U
    field total-vl-embalagem                as decimal format ">>>,>>>,>>9.99":U
    field total-vl-seguro                   as decimal format ">>>,>>>,>>9.99":U
    field total-vl-outras                   as decimal format ">>>,>>>,>>9.99":U
    field per-mercado                       as decimal format ">>9.99":U
    field per-despesa                       as decimal format ">>9.99":U
    field per-imposto                       as decimal format ">>9.99":U
    field per-ii                            as decimal format ">>9.99":U
    FIELD vl-ger-pis-orc                    AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD vl-ger-cofins-orc                 AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD vl-ger-pis-rea                    AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD vl-ger-cofins-rea                 AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD per-ger-pis-rea                   AS DECIMAL FORMAT ">>9.99"
    FIELD per-ger-cofins-rea                AS DECIMAL FORMAT ">>9.99"
    FIELD per-ger-pis-orc                   AS DECIMAL FORMAT ">>9.99"
    FIELD per-ger-cofins-orc                AS DECIMAL FORMAT ">>9.99"
    FIELD r-docum-est                       AS ROWID 
    index ch-embarque is unique primary
          embarque
          cod-estabel.

DEFINE TEMP-TABLE tt-embarque-nota-mae NO-UNDO
    field embarque                          like embarque-imp.embarque      
    field cod-estabel                       like embarque-imp.cod-estabel   
    field nr-nota-fis                       like docum-est.nro-docto
    field dt-emissao                        like docum-est.dt-emissao
    field nr-di                             like embarque-imp.declaracao-import
    field dt-di                             like embarque-imp.data-di
    FIELD r-docum-est                       AS ROWID.
define temp-table tt-embarque-fatura no-undo
    field embarque                     like embarque-imp.embarque
    field cod-estabel                  like embarque-imp.cod-estabel
    field nr-fatura                    like invoice-emb-imp.nr-invoice
    field parcela                      like invoice-emb-imp.parcela
    field dt-venc-fat                  like invoice-emb-imp.dt-vencim.

define temp-table tt-detalha-despesa no-undo
    field embarque      like embarque-imp.embarque
    field cod-estabel   like embarque-imp.cod-estabel
    field nro-docto     like item-doc-est.nro-docto
    field numero-ordem  like cotacao-item.numero-ordem
    field sequencia     like item-doc-est.sequencia
    field it-codigo     like item-doc-est.it-codigo
    field cod-despesa   like desp-imp.cod-desp
    field desc-despesa  like desp-imp.descricao
    field situacao      as   int
    field vl-des-orc    as   decimal format ">>>,>>>,>>9.99999":U
    field vl-des-rea    as   decimal format ">>>,>>>,>>9.99999":U
    field per-des-orc   as   decimal format ">>9.99":U
    field per-des-rea   as   decimal format ">>9.99":U.

DEFINE TEMP-TABLE tt-detalha-despesa-aux NO-UNDO
    FIELD embarque        LIKE embarque-imp.embarque 
    FIELD cod-estabel     LIKE embarque-imp.cod-estabel
    FIELD cod-despesa     LIKE desp-imp.cod-desp 
    FIELD desc-despesa    LIKE desp-imp.descricao 
    FIELD vl-des-orc      AS   DECIMAL FORMAT ">>>,>>>,>>9.99":U
    FIELD vl-des-rea      AS   DECIMAL FORMAT ">>>,>>>,>>9.99":U
    FIELD per-des-orc     AS   DECIMAL FORMAT ">>9.99":U
    FIELD per-des-rea     AS   DECIMAL FORMAT ">>9.99":U
    FIELD sequencia       LIKE item-doc-est.sequencia
    index ch-codigo is unique primary
          embarque
          cod-estabel
          cod-despesa
          sequencia.

define temp-table tt-embarque-rel no-undo
    field embarque      like embarque-imp.embarque
    field cod-estabel   like embarque-imp.cod-estabel
    field embarque-rel  like docum-est.nro-docto
    field nr-nota-rel   like docum-est.nro-docto
    field dt-emis-rel   as   date format "99/99/9999":U
    field nr-di-rel     like embarque-imp.declaracao-import
    field dt-di-rel     like embarque-imp.data-di.

define temp-table tt-nf-comp no-undo
    field embarque      like embarque-imp.embarque
    field cod-estabel   like embarque-imp.cod-estabel
    field nr-nf-comp    like docum-est.nro-docto
    field dt-emis-comp  like docum-est.dt-emissao
    FIELD r-docum-est   AS ROWID.

def temp-table tt-embarque-corpo-agr like tt-embarque-corpo
    index ch-emitente
          cod-estabel 
          cod-emitente
          embarque.

/* Fim da include */

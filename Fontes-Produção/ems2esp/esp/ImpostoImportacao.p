/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa     for ems2cadme.empresa.
def buffer moeda       for ems2cadme.moeda.

{include/i-prgvrs.i IM0665RP 2.00.00.020}  /*** 010020 ***/
{include/i_fnctrad.i}
/******************************************************************************
**
**       PROGRAMA:  IM0665RP.P
**
**       DATA....:  FEVEREIRO DE 2003
**
**       OBJETIVO:  Custo Real de Importa»’o
**
**       VERSAO..:  1.00.000 - Ivonei Vock
**
******************************************************************************/

{cdp/cdcfgmat.i}
DEF VAR c-destino    AS CHAR LABEL "Destino" NO-UNDO.
def var c-desc-moeda like moeda.descricao    no-undo.

{esp\KRAFT.I}

DEFINE INPUT PARAMETER p-dt-inicial AS DATE NO-UNDO.
DEFINE INPUT PARAMETER p-dt-final AS DATE NO-UNDO.
DEFINE INPUT PARAMETER pHandle AS HANDLE NO-UNDO.

/*DEFINE VAR dir-extrat AS CHARACTER   NO-UNDO.
DEFINE VAR dias-extrat AS integer   NO-UNDO.


ASSIGN dias-extrat = 120.*/
/*ASSIGN dir-extrat = "D:\fontes\quarentena\kraft\txt".*/

define temp-table tt-param no-undo
    field destino                as integer
    field arquivo                as char
    field usuario                as char format "x(12)" LABEL "Usuÿrio"
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
    field i-cod-itiner-ini       like itinerario.cod-itiner   LABEL "Itinerÿrio"
    field i-cod-itiner-fim       like itinerario.cod-itiner
    field da-dt-nac-ini          as date    label "Data Nacionaliza»’o"             FORMAT "99/99/9999"
    field da-dt-nac-fim          as date                                            FORMAT "99/99/9999"
    field moeda                  like moeda.mo-codigo
    field da-dt-conv             as date    label "Data Convers’o"                  FORMAT "99/99/9999"
    field l-dt-nacional          as logical label "Data de Nacionaliza»’o"          FORMAT "sim/n’o"
    field l-orc-real             as logical label "Or»ado X Realizado"              FORMAT "sim/n’o"
    field l-desp-imposto         as logical label "Detalha Despesas/Impostos"       FORMAT "sim/n’o"
    field l-itens                as logical label "Detalha Itens"                   FORMAT "sim/n’o"
    field l-desemb-parc          as logical label "Considera Desembarque Parcial"   FORMAT "sim/n’o"
    field l-nf-compl             as logical label "Considera NF Complementar"       FORMAT "sim/n’o"
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
    field vl-mercado-orc                    as   decimal format ">>>,>>>,>>9.99":U
    field vl-mercado-orc-total              as   decimal format ">>>,>>>,>>9.99":U
    field vl-ii-orc                         as   decimal format ">>>,>>>,>>9.99":U 
    field vl-ipi-orc                        as   decimal format ">>>,>>>,>>9.99":U
    field vl-icms-orc                       as   decimal format ">>>,>>>,>>9.99":U
    field vl-pis-orc                        as   decimal format ">>>,>>>,>>9.99":U
    field vl-cofins-orc                     as   decimal format ">>>,>>>,>>9.99":U
    field vl-tot-imp-orc                    as   decimal format ">>>,>>>,>>9.99":U
    field vl-ii2-orc                        as   decimal format ">>>,>>>,>>9.99":U
    field vl-ii2-orc-aux                    as   decimal format ">>>,>>>,>>9.99":U
    field vl-ii-orc-aux                     as   decimal format ">>>,>>>,>>9.99":U
    field vl-des-orc                        as   decimal format ">>>,>>>,>>9.99":U
    field vl-des-orc-total                  as   decimal format ">>>,>>>,>>9.99":U
    field vl-tot-des-orc                    as   decimal format ">>>,>>>,>>9.99":U
    field vl-tot-des-orc-total              as   decimal format ">>>,>>>,>>9.99":U
    field vl-tot-imp-des-orc                as   decimal format ">>>,>>>,>>9.99":U
    field vl-tot-imp-des-orc-total          as   decimal format ">>>,>>>,>>9.99":U
    field vl-tot-com-ipi-icms-orc           as   decimal format ">>>,>>>,>>9.99":U
    field vl-tot-com-ipi-icms-orc-total     as   decimal format ">>>,>>>,>>9.99":U
    field vl-tot-sem-ipi-icms-orc           as   decimal format ">>>,>>>,>>9.99":U
    field vl-tot-sem-ipi-icms-orc-total     as   decimal format ">>>,>>>,>>9.99":U
    field vl-mercado-rea                    as   decimal format ">>>,>>>,>>9.99":U
    field vl-mercado-rea-total              as   decimal format ">>>,>>>,>>9.99":U
    field vl-ii-rea                         as   decimal format ">>>,>>>,>>9.99":U
    field vl-ii-rea-total                   as   decimal format ">>>,>>>,>>9.99":U
    field vl-ipi-rea                        as   decimal format ">>>,>>>,>>9.99":U
    field vl-ipi-rea-total                  as   decimal format ">>>,>>>,>>9.99":U
    field vl-icms-rea                       as   decimal format ">>>,>>>,>>9.99":U
    field vl-pis-rea                        as   decimal format ">>>,>>>,>>9.99":U
    field vl-cofins-rea                     as   decimal format ">>>,>>>,>>9.99":U
    field vl-icms-rea-total                 as   decimal format ">>>,>>>,>>9.99":U
    field vl-tot-imp-rea                    as   decimal format ">>>,>>>,>>9.99":U
    field vl-tot-imp-rea-total              as   decimal format ">>>,>>>,>>9.99":U
    field vl-ii2-rea                        as   decimal format ">>>,>>>,>>9.99":U
    field vl-ii2-rea-total                  as   decimal format ">>>,>>>,>>9.99":U
    field vl-des-rea                        as   decimal format ">>>,>>>,>>9.99":U
    field vl-des-rea-total                  as   decimal format ">>>,>>>,>>9.99":U
    field vl-tot-des-rea                    as   decimal format ">>>,>>>,>>9.99":U
    field vl-tot-des-rea-total              as   decimal format ">>>,>>>,>>9.99":U
    field vl-tot-des-rea-total-aux          as   decimal format ">>>,>>>,>>9.99":U
    field vl-tot-imp-des-rea                as   decimal format ">>>,>>>,>>9.99":U
    field vl-tot-imp-des-rea-total          as   decimal format ">>>,>>>,>>9.99":U
    field vl-tot-com-ipi-icms-rea           as   decimal format ">>>,>>>,>>9.99":U
    field vl-tot-com-ipi-icms-rea-total     as   decimal format ">>>,>>>,>>9.99":U
    field vl-tot-com-ipi-icms-dsp-merc-rea  as   decimal format ">>>,>>>,>>9.99":U
    field vl-tot-sem-ipi-icms-dsp-merc-rea  as   decimal format ">>>,>>>,>>9.99":U
    field vl-tot-sem-ipi-icms-rea           as   decimal format ">>>,>>>,>>9.99":U
    field vl-tot-sem-ipi-icms-rea-total     as   decimal format ">>>,>>>,>>9.99":U
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
    FIELD vl-pis-rea-total                  AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD vl-cofins-rea-total               AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD per-pis-rea                       AS DECIMAL FORMAT ">>9.99"
    FIELD per-cofins-rea                    AS DECIMAL FORMAT ">>9.99"
    FIELD per-pis-orc                       AS DECIMAL FORMAT ">>9.99"
    FIELD per-cofins-orc                    AS DECIMAL FORMAT ">>9.99"
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
    field cod-estabel                       like embarque-imp.cod-estabel
    field cod-emitente                      like processo-imp.cod-exportador
    field numero-ordem                      like ordem-compra.numero-ordem
    field dt-venc-fat                       like invoice-emb-imp.dt-vencim
    field nr-nota-fis                       like docum-est.nro-docto
    field dt-emissao                        like docum-est.dt-emissao
    field nr-di                             like embarque-imp.declaracao-import
    field dt-di                             like embarque-imp.data-di
    field vl-ger-mercado-orc                as decimal format ">>>,>>>,>>9.99":U
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
    field per-mercado                       as decimal format ">>9.99":U
    field per-despesa                       as decimal format ">>9.99":U
    field per-imposto                       as decimal format ">>9.99":U
    field per-ii                            as decimal format ">>9.99":U
    FIELD vl-ger-pis-rea                    AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD vl-ger-cofins-rea                 AS DECIMAL FORMAT ">>>,>>>,>>9.99"
    FIELD per-ger-pis-rea                   AS DECIMAL FORMAT ">>9.99"
    FIELD per-ger-cofins-rea                AS DECIMAL FORMAT ">>9.99"
    index ch-embarque is unique primary
          embarque
          cod-estabel.

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
    field vl-des-orc    as   decimal format ">>>,>>>,>>9.99":U
    field vl-des-rea    as   decimal format ">>>,>>>,>>9.99":U
    field per-des-orc   as   decimal format ">>9.99":U
    field per-des-rea   as   decimal format ">>9.99":U.
/*     INDEX ch-despesa IS UNIQUE PRIMARY */
/*          embarque                      */
/*          cod-estabel                   */
/*          sequencia                     */
/*          cod-despesa.                  */

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
    field dt-emis-comp  like docum-est.dt-emissao.

def temp-table tt-embarque-corpo-agr like tt-embarque-corpo
    index ch-emitente
          cod-estabel 
          cod-emitente
          embarque.

/* Fim da include */

/* Defini»’o de Forms */
def var valor-total-despesa as decimal format ">>>,>>>,>>9.99" no-undo.
def var valor-total-imposto as decimal format ">>>,>>>,>>9.99" no-undo.
def var tot-est-importado   as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-est-mercado     as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-est-despesa     as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-est-imposto     as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-est-ii          as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-est-ipi         as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-est-pis         as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-est-cofins      as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-est-icms        as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-est-frete       as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-est-embalagem   as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-est-seguro      as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-est-outras      as decimal format ">>>,>>>,>>9.99" no-undo. 
def var per-est-mercado     as decimal format ">>9.99"         no-undo. 
def var per-est-despesa     as decimal format ">>9.99"         no-undo. 
def var per-est-imposto     as decimal format ">>9.99"         no-undo. 
def var per-est-ii          as decimal format ">>9.99"         no-undo. 
def var tot-ger-importado   as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-ger-mercado     as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-ger-despesa     as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-ger-imposto     as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-ger-ii          as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-ger-ipi         as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-ger-pis         as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-ger-cofins      as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-ger-icms        as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-ger-frete       as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-ger-embalagem   as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-ger-seguro      as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-ger-outras      as decimal format ">>>,>>>,>>9.99" no-undo. 
def var per-ger-mercado     as decimal format ">>9.99"         no-undo. 
def var per-ger-despesa     as decimal format ">>9.99"         no-undo. 
def var per-ger-imposto     as decimal format ">>9.99"         no-undo. 
def var per-ger-ii          as decimal format ">>9.99"         no-undo. 
def var tot-exp-importado   as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-exp-mercado     as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-exp-despesa     as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-exp-imposto     as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-exp-ii          as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-exp-ipi         as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-exp-icms        as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-exp-pis         as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-exp-cofins      as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-exp-frete       as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-exp-embalagem   as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-exp-seguro      as decimal format ">>>,>>>,>>9.99" no-undo. 
def var tot-exp-outras      as decimal format ">>>,>>>,>>9.99" no-undo. 
def var per-exp-mercado     as decimal format ">>9.99"         no-undo. 
def var per-exp-despesa     as decimal format ">>9.99"         no-undo. 
def var per-exp-imposto     as decimal format ">>9.99"         no-undo. 
def var per-exp-ii          as decimal format ">>9.99"         no-undo. 
def var tot-item-importado  as decimal format ">>>,>>>,>>9.99" no-undo.
def var tot-item-mercado    as decimal format ">>>,>>>,>>9.99" no-undo.
def var tot-item-despesa    as decimal format ">>>,>>>,>>9.99" no-undo.
def var tot-item-imposto    as decimal format ">>>,>>>,>>9.99" no-undo.
def var tot-item-ii         as decimal format ">>>,>>>,>>9.99" no-undo.
def var tot-item-ipi        as decimal format ">>>,>>>,>>9.99" no-undo.
def var tot-item-icms       as decimal format ">>>,>>>,>>9.99" no-undo.
def var tot-item-pis        as decimal format ">>>,>>>,>>9.99" no-undo.
def var tot-item-cofins     as decimal format ">>>,>>>,>>9.99" no-undo.
def var tot-item-frete      as decimal format ">>>,>>>,>>9.99" no-undo.
def var tot-item-embalagem  as decimal format ">>>,>>>,>>9.99" no-undo.
def var tot-item-seguro     as decimal format ">>>,>>>,>>9.99" no-undo.
def var tot-item-outras     as decimal format ">>>,>>>,>>9.99" no-undo.
def var per-item-mercado    as decimal format ">>9.99"         no-undo.
def var per-item-despesa    as decimal format ">>9.99"         no-undo.
def var per-item-imposto    as decimal format ">>9.99"         no-undo.
def var per-item-ii         as decimal format ">>9.99"         no-undo.
def var c-desc-item         like item.desc-item                no-undo.

def buffer b-docum-est          for docum-est.
def buffer b-docum-est-aux      for docum-est.
def buffer b-embarque-imp       for embarque-imp.
def buffer b-ordens-embarque    for ordens-embarque.
def buffer b-item-doc-est       for item-doc-est.
def buffer b-item-doc-est-aux   for item-doc-est.
def buffer b-item-doc-est-aux2  for item-doc-est.
def buffer b-historico-embarque for historico-embarque.

/*Defini»’o de Variÿveis*/
def var c-desc-estabel                like estabelec.nome               no-undo.
def var c-desc-emitente               like emitente.nome-abrev          no-undo.
def var d-taxa-moeda                  as   decimal                      no-undo.
def var d-data-conv                   as   date                         no-undo.
def var d-data-emb                    as   date                         no-undo.
def var d-data-nac                    as   date                         no-undo.
def var c-taxa                        as   char                         no-undo.
def var c-embarque                    as   char format "x(20)"          no-undo.
def var i-cod-desp-ii                 like docum-est-cex.cod-desp       no-undo.
def var c-desc-pto-emb                like pto-contr.descricao          no-undo.
def var c-desc-pto-nac                like pto-contr.descricao          no-undo.
def var vl-ii-orcad-det               like cotacao-item-cex.val-desp    no-undo.
def var vl-ii2-orcad-det              like cotacao-item-cex.val-desp    no-undo.
def var vl-ii-reali-det               like item-doc-est-cex.val-desp    no-undo.
def var vl-ii2-reali-det              like item-doc-est-cex.val-desp    no-undo.
def var vl-ipi-orcad-det              like cotacao-item-cex.val-desp    no-undo.
def var vl-icms-orcad-det             like cotacao-item-cex.val-desp    no-undo.
def var vl-pis-orcad-det              like cotacao-item-cex.val-desp    no-undo.
def var vl-cofins-orcad-det           like cotacao-item-cex.val-desp    no-undo.
def var vl-desp-orcad-det             like cotacao-item-cex.val-desp    no-undo.
def var vl-despesa-frete              like item-doc-est-cex.val-desp    no-undo.
def var vl-despesa-embalagem          like item-doc-est-cex.val-desp    no-undo.
def var vl-despesa-seguro             like item-doc-est-cex.val-desp    no-undo.
def var vl-despesa-outras             like item-doc-est-cex.val-desp    no-undo.
def var tot-despesa-orc               like item-doc-est-cex.val-desp    no-undo.
def var tot-despesa-rea               like item-doc-est-cex.val-desp    no-undo.
def var tot-despesa-orc-total         like item-doc-est-cex.val-desp    no-undo.
def var tot-despesa-rea-total         like item-doc-est-cex.val-desp    no-undo.
def var var-preco-unit                like item-doc-est-cex.val-desp    no-undo.
def var var-moeda-conv-orc            like item-doc-est-cex.mo-codigo   no-undo.
def var var-moeda-conv-rea            like item-doc-est-cex.mo-codigo   no-undo.
def var var-preco-item-rea            like item-doc-est-cex.val-desp    no-undo.
def var var-valor-ipi-rea             like item-doc-est-cex.val-desp    no-undo.
def var var-valor-icm-rea             like item-doc-est-cex.val-desp    no-undo.
def var var-valor-pis-rea             like item-doc-est-cex.val-desp    no-undo.
def var var-valor-cofins-rea          like item-doc-est-cex.val-desp    no-undo.
def var var-preco-unit-aux            like item-doc-est-cex.val-desp    no-undo.
def var var-valor-ipi-mattran-rea     like item-doc-est-cex.val-desp    no-undo.
def var var-valor-icm-mattran-rea     like item-doc-est-cex.val-desp    no-undo.
def var var-valor-pis-mattran-rea     like item-doc-est-cex.val-desp    no-undo.
def var var-valor-cofins-mattran-rea  like item-doc-est-cex.val-desp    no-undo.
DEF VAR de-vl-cofins-rea              LIKE cotacao-item-cex.val-desp    NO-UNDO.  
DEF VAR de-vl-pis-rea                 LIKE cotacao-item-cex.val-desp    NO-UNDO.
DEF VAR de-vl-cofins-rea-total        LIKE cotacao-item-cex.val-desp    NO-UNDO.  
DEF VAR de-vl-pis-rea-total           LIKE cotacao-item-cex.val-desp    NO-UNDO.
DEF VAR vl-desp-orcad-aux             LIKE cotacao-item-cex.val-desp    NO-UNDO.
DEF VAR vl-ii2-orcad-det-aux          LIKE cotacao-item-cex.val-desp    NO-UNDO.
DEF VAR vl-ii-orcad-det-aux           LIKE cotacao-item-cex.val-desp    NO-UNDO.
DEF VAR de-preco-unit-sem-ipi         LIKE ordem-compra.preco-unit      NO-UNDO.
DEF VAR de-teste                      AS   DECIMAL                      NO-UNDO.

DEF VAR de-val-desp-nfc2              AS   DECIMAL                      NO-UNDO.
DEF VAR de-val-desp-aux-rea           AS   DECIMAL                      NO-UNDO.
DEF VAR de-tot-orc                    AS   DECIMAL                      NO-UNDO.
DEF VAR de-tot-rea                    AS   DECIMAL                      NO-UNDO.

def var no-despesa                    as   logical                      no-undo.
def var l-desemb-parc-modif           as   logical initial no           no-undo.
def var l-cria-embarque-corpo         as   logical initial no           no-undo.
def var h-acomp                       as   handle                       no-undo.
def var i-parcela                     as   integer                      no-undo.



/*run utp/ut-acomp.p persistent set h-acomp.*/

{include/i-rpvar.i}
{include/i-rpcab.i}




    create tt-param.
    assign tt-param.usuario         = "c-seg-usuario"
           tt-param.destino         = 1
           tt-param.data-exec       = today
           tt-param.hora-exec       = time
           tt-param.classifica      = 1
           tt-param.desc-classifica = ""
           tt-param.arquivo         = "" 
           tt-param.c-cod-estabel-ini       =    ""
           tt-param.c-cod-estabel-fim       =    "zzzzzzzzzzzzz"
           tt-param.c-embarque-ini          =    ""
           tt-param.c-embarque-fim          =    "zzzzzzzzzzzzz"
           tt-param.c-it-codigo-ini         =    ""             
           tt-param.c-it-codigo-fim         =    "zzzzzzzzzzzzz"
           tt-param.i-ge-codigo-ini         =    0
           tt-param.i-ge-codigo-fim         =    9999999
           tt-param.c-fm-codigo-ini         =    ""             
           tt-param.c-fm-codigo-fim         =    "zzzzzzzzzzzzz"
           tt-param.i-cod-emitente-ini      =    0
           tt-param.i-cod-emitente-fim      =    9999999
           tt-param.c-nr-proc-imp-ini       =    ""             
           tt-param.c-nr-proc-imp-fim       =    "zzzzzzzzzzzzz"
           tt-param.i-cod-itiner-ini        =    0
           tt-param.i-cod-itiner-fim        =    9999999
           tt-param.da-dt-nac-ini           =    01/01/1900
           tt-param.da-dt-nac-fim           =    12/31/2099
           tt-param.moeda                   =    0
           tt-param.da-dt-conv              =    TODAY
           tt-param.l-dt-nacional           =    NO
           tt-param.l-orc-real              =    NO
           tt-param.l-desp-imposto          =    YES
           tt-param.l-itens                 =    NO
           tt-param.l-desemb-parc           =    YES
           tt-param.l-nf-compl              =    YES
           tt-param.i-custo-item            =    0
           tt-param.i-desp-imposto          =    0 /**/. 

    if tt-param.i-custo-item = 1 then
        assign tt-param.c-custo-item = "Unitÿrio".
    else
        assign tt-param.c-custo-item = "Total".

    if tt-param.i-desp-imposto = 1 then
        assign tt-param.c-desp-imposto = "Despesa".
    else
        assign tt-param.c-desp-imposto = "Imposto".




find first tt-param     no-lock no-error.
find first param-global no-lock no-error.


find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error.
if  avail empresa then
    assign c-empresa = empresa.razao-social.
else do:

    return error.
end.

for each tt-detalha-item:
    delete tt-detalha-item.
end.
for each tt-embarque-cabec:
    delete tt-embarque-cabec.
end.
for each tt-embarque-corpo:
    delete tt-embarque-corpo.
end.

FOR EACH tt-detalha-despesa-aux:
    DELETE tt-detalha-despesa-aux.
END.

/*run pi-inicializar in h-acomp (input "Preparando Dados...").*/

for each embarque-imp no-lock
    WHERE embarque-imp.data-di >= p-dt-inicial 
      AND embarque-imp.data-di <= p-dt-final:
   /*
   where embarque-imp.cod-estabel >= tt-param.c-cod-estabel-ini
     and embarque-imp.cod-estabel <= tt-param.c-cod-estabel-fim
     and embarque-imp.embarque    >= tt-param.c-embarque-ini
     and embarque-imp.embarque    <= tt-param.c-embarque-fim:
    IF embarque-imp.data-di < p-dt-inicial OR embarque-imp.data-di > p-dt-final THEN NEXT.
   */


    RUN pi-acompanhar IN pHandle (INPUT "Embarque " + embarque-imp.embarque).

    /*IF embarque-imp.embarque <> "31040/2" THEN NEXT.*/


    /*run pi-acompanhar in h-acomp (input "Embarque: " + string(embarque-imp.embarque)).*/

    for first ordens-embarque fields( embarque  cod-estabel  numero-ordem
                                      char-2    quantidade )
        where ordens-embarque.embarque    = embarque-imp.embarque
          and ordens-embarque.cod-estabel = embarque-imp.cod-estabel no-lock:
    end.
    if  not avail ordens-embarque then next.
    
    for first ordem-compra fields( numero-ordem  num-pedido )
        where ordem-compra.numero-ordem = ordens-embarque.numero-ordem no-lock:
    end.
    if  not avail ordem-compra then next.
    
    for first b-historico-embarque fields(cod-itiner)
        where b-historico-embarque.cod-estabel = embarque-imp.cod-estabel
          and b-historico-embarque.embarque    = embarque-imp.embarque no-lock:
    end.

    if  b-historico-embarque.cod-itiner <= tt-param.i-cod-itiner-ini
    and b-historico-embarque.cod-itiner >= tt-param.i-cod-itiner-fim then next.
    
    for first processo-imp fields( num-pedido   cod-exportador
                                   nr-proc-imp  cod-itiner )
        where processo-imp.num-pedido          =  ordem-compra.num-pedido
          and processo-imp.nr-proc-imp        >= tt-param.c-nr-proc-imp-ini
          and processo-imp.nr-proc-imp        <= tt-param.c-nr-proc-imp-fim
          and processo-imp.cod-exportador     >= tt-param.i-cod-emitente-ini
          and processo-imp.cod-exportador     <= tt-param.i-cod-emitente-fim no-lock:
    end.
    if  not avail processo-imp then next.
    
    /****** DESCRICAO ******/
    for first estabelec fields( cod-estabel  nome )
        where estabelec.cod-estabel = embarque-imp.cod-estabel no-lock:
    end.
    assign c-desc-estabel = if avail estabelec then estabelec.nome else "":U.

    for first emitente fields( cod-emitente  nome-abrev  pais )
        where emitente.cod-emitente = processo-imp.cod-exportador no-lock:
    end.
    assign c-desc-emitente = if avail emitente then emitente.nome-abrev else "":U.
    /****** ********* ******/

    /******** MOEDA ********/
    for first moeda fields( mo-codigo descricao )
        where moeda.mo-codigo = tt-param.moeda no-lock:
    end.
    if  not avail moeda then next.
    assign d-data-conv  = tt-param.da-dt-conv
           c-desc-moeda = moeda.descricao.
    if  l-dt-nacional then
        assign d-data-conv = embarque-imp.data-1.
    run cdp/cd0812.p (input  tt-param.moeda, /*moeda origem*/
                      input  0,              /*moeda destino*/
                      input  1,              /*valor origem*/
                      input  d-data-conv,    /*data conversao*/
                      output d-taxa-moeda).  /*valor destino*/
    /******* ******* *******/

    for first itinerario fields( cod-itiner  pto-embarque  pto-desembarque )
        where itinerario.cod-itiner = b-historico-embarque.cod-itiner no-lock:
    end.
    if  not avail itinerario then next.

    /* Data e Ponto de Embarque */
    &if "{&bf_mat_versao_ems}" >= "2.05" &then
        for first historico-embarque fields( cod-estabel  embarque  cod-pto-contr
                                             dt-previsao   cod-itiner )
            where historico-embarque.cod-estabel   =  ordens-embarque.cod-estabel
              and historico-embarque.embarque      =  ordens-embarque.embarque
              and historico-embarque.cod-itiner    =  itinerario.cod-itiner
              and historico-embarque.cod-pto-contr =  itinerario.pto-embarque no-lock:
        end.
    &else
        for first historico-embarque fields( cod-estabel  embarque  cod-pto-contr
                                             dt-efetiva  cod-itiner )
            where historico-embarque.cod-estabel   =  ordens-embarque.cod-estabel
              and historico-embarque.embarque      =  ordens-embarque.embarque
              and historico-embarque.cod-itiner    =  itinerario.cod-itiner
              and historico-embarque.cod-pto-contr =  itinerario.pto-embarque no-lock:
        end.
    &endif
    if  not avail historico-embarque then next.

    for first pto-contr fields( cod-pto-contr  descricao )
        where pto-contr.cod-pto-contr = itinerario.pto-embarque no-lock:
    end.
    if  not avail pto-contr then next.
    
    &if "{&bf_mat_versao_ems}" >= "2.05" &then
        assign d-data-emb     = historico-embarque.dt-previsao
               c-desc-pto-emb = pto-contr.descricao.
    &else
        assign d-data-emb     = historico-embarque.dt-efetiva
               c-desc-pto-emb = pto-contr.descricao.
    &endif
    /* ****************************** */
    
    /* Data e Ponto de Nacionaliza»’o */
    &if "{&bf_mat_versao_ems}" >= "2.05" &then
        for first historico-embarque fields( cod-estabel  embarque  cod-pto-contr
                                             dt-previsao  cod-itiner )
            where historico-embarque.cod-estabel    =  ordens-embarque.cod-estabel
              and historico-embarque.embarque       =  ordens-embarque.embarque
              and historico-embarque.cod-itiner     =  itinerario.cod-itiner
              and historico-embarque.cod-pto-contr  =  itinerario.pto-desembarque
              and historico-embarque.dt-previsao   >= tt-param.da-dt-nac-ini
              and historico-embarque.dt-previsao   <= tt-param.da-dt-nac-fim no-lock:
        end.
    &else
        for first historico-embarque fields( cod-estabel  embarque  cod-pto-contr
                                             dt-efetiva   cod-itiner )
            where historico-embarque.cod-estabel    =  ordens-embarque.cod-estabel
              and historico-embarque.embarque       =  ordens-embarque.embarque
              and historico-embarque.cod-itiner     =  itinerario.cod-itiner
              and historico-embarque.cod-pto-contr  =  itinerario.pto-desembarque
              and historico-embarque.dt-efetiva    >= tt-param.da-dt-nac-ini
              and historico-embarque.dt-efetiva    <= tt-param.da-dt-nac-fim no-lock:
        end.
    &endif
    if  not avail historico-embarque then next.

    for first pto-contr fields( cod-pto-contr  descricao )
        where pto-contr.cod-pto-contr = itinerario.pto-desembarque no-lock:
    end.
    if  not avail pto-contr then next.

    &if "{&bf_mat_versao_ems}" >= "2.05" &then
        assign d-data-nac     = historico-embarque.dt-previsao
               c-desc-pto-nac = pto-contr.descricao.
    &else
        assign d-data-nac     = historico-embarque.dt-efetiva
               c-desc-pto-nac = pto-contr.descricao.
    &endif
    /* ****************************** */

    
    for first param-imp fields( cod-estabel char-1 
                                &if "{&bf_mat_versao_ems}" >= "2.062" &then
                                    cdn-despes-impto-ipi cdn-despes-impto-icms cdn-despes-impto-pis cdn-despes-impto-cofins
                                &endif)
        where param-imp.cod-estabel = embarque-imp.cod-estabel no-lock:
    end.
    if  not avail param-imp then next.
    assign i-cod-desp-ii   = int(substring(param-imp.char-1,21,20)). 
    
    
    /* => CABE°ALHO *******************************/
    create tt-embarque-cabec.
    assign tt-embarque-cabec.cod-estabel    = embarque-imp.cod-estabel
           tt-embarque-cabec.desc-estab     = if avail estabelec then estabelec.nome else "":U
           tt-embarque-cabec.embarque       = embarque-imp.embarque
           tt-embarque-cabec.cod-emitente   = processo-imp.cod-exportador
           tt-embarque-cabec.desc-emit      = if avail emitente then emitente.nome-abrev else "":U
           tt-embarque-cabec.pais           = if avail emitente then emitente.pais else "":U
           tt-embarque-cabec.cod-incoterm   = embarque-imp.cod-incoterm
           tt-embarque-cabec.cod-via-transp = embarque-imp.cod-via-transp
           tt-embarque-cabec.pto-embarque   = c-desc-pto-emb
           tt-embarque-cabec.data-embarque  = d-data-emb
           tt-embarque-cabec.pto-nacional   = c-desc-pto-nac
           tt-embarque-cabec.data-nacional  = d-data-nac

           &if "{&bf_mat_versao_ems}" >= "2.05" &then
               tt-embarque-cabec.data-chegada   = if   historico-embarque.dt-previsao = ? then historico-embarque.dt-previsao
                                                  else historico-embarque.dt-previsao
           &else
               tt-embarque-cabec.data-chegada   = if   historico-embarque.dt-efetiva = ? then historico-embarque.dt-previsao
                                                  else historico-embarque.dt-efetiva
           &endif
           
           tt-embarque-cabec.data-cotacao   = embarque-imp.data-1
           tt-embarque-cabec.desc-moeda     = c-desc-moeda
           tt-embarque-cabec.mo-taxa        = d-taxa-moeda
           tt-embarque-cabec.data-convercao = d-data-conv.

    /******** NOTA FISCAL DE FATURA ********/
    for each invoice-emb-imp no-lock
        where invoice-emb-imp.cod-estabel = embarque-imp.cod-estabel
          and invoice-emb-imp.embarque    = embarque-imp.embarque:
        create tt-embarque-fatura.
        assign tt-embarque-fatura.embarque      = embarque-imp.embarque
               tt-embarque-fatura.cod-estabel   = embarque-imp.cod-estabel
               tt-embarque-fatura.nr-fatura     = invoice-emb-imp.nr-invoice
               tt-embarque-fatura.parcela       = invoice-emb-imp.parcela
               tt-embarque-fatura.dt-venc-fat   = invoice-emb-imp.dt-vencim.
    end.

    /****** NOTA FISCAL PRINCIPAL ******/
    for first docum-est fields( char-1  cod-estabel  serie-docto  nro-docto
                                &if "{&bf_mat_versao_ems}" >= "2.06" &then
                                    idi-nf-simples-remes
                                &else
                                    char-2
                                &endif 
                                cod-emitente  nat-operacao  dt-emissao )
        where trim(substr(docum-est.char-1,1,12)) = embarque-imp.embarque no-lock:

    end.

    if  avail docum-est then do:
        /****** NOTA PRINCIPAL ******/
        &if "{&bf_mat_versao_ems}" >= "2.06" &then
            if docum-est.idi-nf-simples-remes = 1 
        &else
            if int(substring(docum-est.char-2,101,1)) = 1 
        &endif 
                and tt-param.l-desemb-parc = no then
                assign tt-param.l-desemb-parc = yes
                       l-desemb-parc-modif    = yes.

        RUN gera-valores (INPUT docum-est.serie-docto,
                          INPUT docum-est.nro-docto,     
                          INPUT docum-est.cod-emitente,
                          INPUT docum-est.nat-operacao,
                          INPUT embarque-imp.embarque,
                          INPUT docum-est.cod-estabel,
                          INPUT 1). /*Nota Principal*/
        /****************************/

        
        /****** EMBARQUES RELACIONADOS ******/
        IF  tt-param.l-desemb-parc THEN DO:
        &if "{&bf_mat_versao_ems}" >= "2.06" &then
            if docum-est.idi-nf-simples-remes <> 2 then do:
        &else
            if int(substring(docum-est.char-2,101,1)) <> 2 then do:
        &endif
            &if "{&bf_mat_versao_ems}" >= "2.05" &then
                for each  b-embarque-imp fields( embarque-ori  cod-estabel  embarque 
                                                 declaracao-import  data-di)
                    where b-embarque-imp.embarque-ori = embarque-imp.embarque no-lock:
            &else
                for each  b-embarque-imp fields( char-1  cod-estabel  embarque 
                                                 declaracao-import  data-di)
                    where trim(substring(b-embarque-imp.char-1,21,20)) = embarque-imp.embarque no-lock:
            &endif
            
                create tt-embarque-rel.
                assign tt-embarque-rel.embarque      = embarque-imp.embarque
                       tt-embarque-rel.cod-estabel   = b-embarque-imp.cod-estabel
                       tt-embarque-rel.embarque-rel  = b-embarque-imp.embarque.
                for first b-docum-est fields( char-1  serie-docto  nro-docto cod-estabel
                                              cod-emitente  nat-operacao  dt-emissao )
                    where trim(substr(b-docum-est.char-1,1,12)) = b-embarque-imp.embarque no-lock:
                end.
                if  avail b-docum-est then do:

                    assign tt-embarque-rel.nr-nota-rel = b-docum-est.nro-docto
                           tt-embarque-rel.dt-emis-rel = b-docum-est.dt-emissao
                           tt-embarque-rel.nr-di-rel   = b-embarque-imp.declaracao-import
                           tt-embarque-rel.dt-di-rel   = b-embarque-imp.data-di.

                    RUN gera-valores (INPUT b-docum-est.serie-docto,
                                      INPUT b-docum-est.nro-docto,     
                                      INPUT b-docum-est.cod-emitente,
                                      INPUT b-docum-est.nat-operacao,
                                      INPUT b-embarque-imp.embarque, /*embarque relacionado*/
                                      INPUT b-docum-est.cod-estabel,
                                      INPUT 2). /*Nota Relacionada*/
                end.
            end.
        END.
        END.
        /****** ********* ******/
        /*os pr½ximos embarques que poder’o n’o ser de simples de remessa, ser’o mostrados corretamente*/
        if l-desemb-parc-modif then
            assign tt-param.l-desemb-parc = no.

        /****** NOTAS COMPLEMENTARES ******/

        IF  tt-param.l-nf-compl THEN DO:
            for each b-docum-est fields( nro-docto  serie-docto  cod-emitente  nat-operacao char-1)
                where trim(substring(b-docum-est.char-1,1,20)) = embarque-imp.embarque no-lock:

              for first b-item-doc-est
                  where b-item-doc-est.serie-docto     = b-docum-est.serie-docto
                    and b-item-doc-est.nro-docto       = b-docum-est.nro-docto    
                    and b-item-doc-est.cod-emitente    = b-docum-est.cod-emitente 
                    and b-item-doc-est.nat-operacao    = b-docum-est.nat-operacao 
                    and b-item-doc-est.baixa-ce = yes no-lock:

                    run busca-nf-complementar (input b-item-doc-est.serie-docto,
                                               input b-item-doc-est.nro-docto,
                                               input b-item-doc-est.cod-emitente,
                                               input b-item-doc-est.nat-operacao,
                                               input embarque-imp.embarque,
                                               input embarque-imp.cod-estabel).
                end.
            end.

            &if "{&bf_mat_versao_ems}" >= "2.05" &then
            for each  b-embarque-imp fields( embarque-ori  cod-estabel  embarque 
                                             declaracao-import  data-di)
                where b-embarque-imp.embarque-ori = embarque-imp.embarque no-lock:
            &else
            for each  b-embarque-imp fields( char-1  cod-estabel  embarque 
                                             declaracao-import  data-di)
                where trim(substring(b-embarque-imp.char-1,21,20)) = embarque-imp.embarque no-lock:
            &endif
            
                for first b-docum-est fields( nro-docto  serie-docto  cod-emitente  nat-operacao )
                    where trim(substring(b-docum-est.char-1,1,20)) = b-embarque-imp.embarque no-lock:

                    run busca-nf-complementar (input b-docum-est.serie-docto,
                                               input b-docum-est.nro-docto,
                                               input b-docum-est.cod-emitente,
                                               input b-docum-est.nat-operacao,
                                               input b-embarque-imp.embarque,
                                               input b-embarque-imp.cod-estabel).
                end.
            end.
        END. /*fim tt-param.l-nf-com*/
        /****** ********* ******/
    end. /*docum-est*/      

    ASSIGN de-val-desp-aux-rea = 0.

    FOR EACH  tt-embarque-corpo
        WHERE tt-embarque-corpo.embarque    = embarque-imp.embarque
          AND tt-embarque-corpo.cod-estabel = embarque-imp.cod-estabel:

        FOR EACH  tt-detalha-item
            WHERE tt-detalha-item.embarque     = tt-embarque-corpo.embarque
             AND  tt-detalha-item.cod-estabel  = tt-embarque-corpo.cod-estabel
            use-index ch-codigo:

            ASSIGN tt-embarque-corpo.vl-ger-mercado-orc           = tt-embarque-corpo.vl-ger-mercado-orc           + tt-detalha-item.vl-mercado-orc-total
                   /*tt-embarque-corpo.vl-ger-ii-orc                = tt-embarque-corpo.vl-ger-ii-orc                + tt-detalha-item.vl-ii-orc-aux*/

                   /*&IF "{&bf_mat_versao_ems}" < "2.062" &THEN
                   tt-embarque-corpo.vl-ger-ipi-orc               = tt-embarque-corpo.vl-ger-ipi-orc               + tt-detalha-item.vl-ipi-orc
                   tt-embarque-corpo.vl-ger-icms-orc              = tt-embarque-corpo.vl-ger-icms-orc              + tt-detalha-item.vl-icms-orc
                   tt-embarque-corpo.vl-ger-pis-orc               = tt-embarque-corpo.vl-ger-pis-orc               + tt-detalha-item.vl-pis-orc
                   tt-embarque-corpo.vl-ger-cofins-orc            = tt-embarque-corpo.vl-ger-cofins-orc            + tt-detalha-item.vl-cofins-orc
                   tt-embarque-corpo.vl-ger-tot-imp-orc           = tt-embarque-corpo.vl-ger-tot-imp-orc         
                                                                    + IF tt-param.i-desp-imposto = 1 THEN  /*Despesa*/   
                                                                       tt-detalha-item.vl-ipi-orc + tt-detalha-item.vl-icms-orc + tt-detalha-item.vl-pis-orc + tt-detalha-item.vl-cofins-orc
                                                                    ELSE /*Imposto*/ 
                                                                       tt-detalha-item.vl-ii-orc-aux + tt-detalha-item.vl-ipi-orc   + tt-detalha-item.vl-icms-orc + tt-detalha-item.vl-pis-orc + tt-detalha-item.vl-cofins-orc
                   &ELSE
                   tt-embarque-corpo.vl-ger-ipi-orc               = tt-embarque-corpo.vl-ger-ipi-orc               
                                                                    + IF  tt-param.i-custo-item = 1 THEN 
                                                                          tt-detalha-item.vl-ipi-orc * tt-detalha-item.quantidade
                                                                      ELSE
                                                                          tt-detalha-item.vl-ipi-orc
                   tt-embarque-corpo.vl-ger-icms-orc              = tt-embarque-corpo.vl-ger-icms-orc              
                                                                    + IF  tt-param.i-custo-item = 1 THEN 
                                                                          tt-detalha-item.vl-icms-orc * tt-detalha-item.quantidade
                                                                      ELSE
                                                                          tt-detalha-item.vl-icms-orc
                   tt-embarque-corpo.vl-ger-pis-orc               = tt-embarque-corpo.vl-ger-pis-orc               
                                                                    + IF  tt-param.i-custo-item = 1 THEN 
                                                                          (tt-detalha-item.vl-pis-orc * tt-detalha-item.quantidade)
                                                                      ELSE
                                                                          tt-detalha-item.vl-pis-orc
                   tt-embarque-corpo.vl-ger-cofins-orc            = tt-embarque-corpo.vl-ger-cofins-orc              
                                                                    + IF  tt-param.i-custo-item = 1 THEN 
                                                                          tt-detalha-item.vl-cofins-orc * tt-detalha-item.quantidade
                                                                      ELSE
                                                                          tt-detalha-item.vl-cofins-orc
                   tt-embarque-corpo.vl-ger-tot-imp-orc           = IF tt-param.i-desp-imposto = 1 THEN  /*Despesa*/ 
                                                                       tt-embarque-corpo.vl-ger-ipi-orc + tt-embarque-corpo.vl-ger-icms-orc + tt-embarque-corpo.vl-ger-pis-orc + tt-embarque-corpo.vl-ger-cofins-orc
                                                                    ELSE /*Imposto*/ 
                                                                       tt-embarque-corpo.vl-ger-ii-orc + tt-embarque-corpo.vl-ger-ipi-orc + tt-embarque-corpo.vl-ger-icms-orc + tt-embarque-corpo.vl-ger-pis-orc + tt-embarque-corpo.vl-ger-cofins-orc 
                   &ENDIF
                   tt-embarque-corpo.vl-ger-ii2-orc               = tt-embarque-corpo.vl-ger-ii2-orc               + tt-detalha-item.vl-ii2-orc-aux
                   tt-embarque-corpo.vl-ger-des-orc               = tt-embarque-corpo.vl-ger-des-orc               + tt-detalha-item.vl-des-orc-total
                   */                                                                                                                      
                   tt-embarque-corpo.vl-ger-mercado-rea           = tt-embarque-corpo.vl-ger-mercado-rea           + tt-detalha-item.vl-mercado-rea-total
                   tt-embarque-corpo.vl-ger-ii-rea                = tt-embarque-corpo.vl-ger-ii-rea                + tt-detalha-item.vl-ii-rea-total
                   tt-embarque-corpo.vl-ger-ipi-rea               = tt-embarque-corpo.vl-ger-ipi-rea               + tt-detalha-item.vl-ipi-rea-total
                   tt-embarque-corpo.vl-ger-icms-rea              = tt-embarque-corpo.vl-ger-icms-rea              + tt-detalha-item.vl-icms-rea-total            
                   tt-embarque-corpo.vl-ger-pis-rea               = tt-embarque-corpo.vl-ger-pis-rea               + tt-detalha-item.vl-pis-rea-total
                   tt-embarque-corpo.vl-ger-cofins-rea            = tt-embarque-corpo.vl-ger-cofins-rea            + tt-detalha-item.vl-cofins-rea-total                 
                   tt-embarque-corpo.vl-ger-tot-imp-rea           = tt-embarque-corpo.vl-ger-tot-imp-rea           + tt-detalha-item.vl-tot-imp-rea-total         
                   tt-embarque-corpo.vl-ger-ii2-rea               = tt-embarque-corpo.vl-ger-ii2-rea               + tt-detalha-item.vl-ii2-rea-total
                   tt-embarque-corpo.vl-ger-des-rea               = tt-embarque-corpo.vl-ger-des-rea               + tt-detalha-item.vl-des-rea-total
                   tt-embarque-corpo.vl-ger-tot-com-ipi-icms-rea  = tt-embarque-corpo.vl-ger-tot-com-ipi-icms-rea  + tt-detalha-item.vl-tot-com-ipi-icms-rea-total
                   tt-embarque-corpo.vl-ger-tot-sem-ipi-icms-rea  = tt-embarque-corpo.vl-ger-tot-sem-ipi-icms-rea  + tt-detalha-item.vl-tot-sem-ipi-icms-rea-total
                                                                                                                                         
                   /*tt-embarque-corpo.total-vl-frete               = tt-embarque-corpo.total-vl-frete               + tt-detalha-item.vl-des-frete
                   tt-embarque-corpo.total-vl-embalagem           = tt-embarque-corpo.total-vl-embalagem           + tt-detalha-item.vl-des-embalagem
                   tt-embarque-corpo.total-vl-seguro              = tt-embarque-corpo.total-vl-seguro              + tt-detalha-item.vl-des-seguro
                   tt-embarque-corpo.total-vl-outras              = tt-embarque-corpo.total-vl-outras              + tt-detalha-item.vl-des-outras*/
                   
                   tt-embarque-corpo.per-mercado                  = (tt-embarque-corpo.vl-ger-mercado-rea * 100)   / tt-embarque-corpo.vl-ger-mercado-rea
                   tt-embarque-corpo.per-imposto                  = (tt-embarque-corpo.vl-ger-tot-imp-rea * 100)   / tt-embarque-corpo.vl-ger-mercado-rea
                   tt-embarque-corpo.per-ii                       = (tt-embarque-corpo.vl-ger-ii-rea      * 100)   / tt-embarque-corpo.vl-ger-mercado-rea.
                
            IF tt-embarque-corpo.vl-ger-mercado-orc = ? THEN
                ASSIGN tt-embarque-corpo.vl-ger-mercado-orc = 0.

            ASSIGN de-tot-rea = 0.
            FOR EACH tt-detalha-despesa-aux
               WHERE tt-detalha-despesa-aux.embarque    = tt-embarque-corpo.embarque
                 AND tt-detalha-despesa-aux.cod-estabel = tt-embarque-corpo.cod-estabel:

                ASSIGN de-tot-rea = de-tot-rea + tt-detalha-despesa-aux.vl-des-rea.

            END.
            
            IF tt-param.i-desp-imposto = 1 THEN
                ASSIGN tt-embarque-corpo.vl-ger-tot-des-rea = de-tot-rea + tt-embarque-corpo.vl-ger-ii2-rea.
            ELSE
                ASSIGN tt-embarque-corpo.vl-ger-tot-des-rea = de-tot-rea.

            IF tt-param.l-desp-imposto THEN DO: /*detalha despesa/impostos*/
                ASSIGN tt-embarque-corpo.per-despesa     = (tt-embarque-corpo.vl-ger-tot-des-rea * 100) /
                                                            tt-embarque-corpo.vl-ger-mercado-rea.
            END.
            ELSE  DO:
                ASSIGN tt-embarque-corpo.per-despesa     = (tt-embarque-corpo.vl-ger-tot-des-rea * 100) /
                                                            tt-embarque-corpo.vl-ger-mercado-rea.
            END.

            /* Convers’o */
            IF tt-param.moeda <> var-moeda-conv-rea THEN DO:
                IF tt-param.l-desp-imposto THEN DO:
                    ASSIGN tt-embarque-corpo.vl-ger-tot-imp-des-rea = tt-embarque-corpo.vl-ger-tot-des-rea     +
                                                                      tt-embarque-corpo.vl-ger-tot-imp-rea.
                END.
                ELSE DO:
                    ASSIGN tt-embarque-corpo.vl-ger-tot-imp-des-rea = tt-embarque-corpo.vl-ger-tot-des-rea     +
                                                                      tt-embarque-corpo.vl-ger-tot-imp-rea.
                END.
            END.
            ELSE DO:
                IF NOT tt-param.l-desp-imposto THEN DO:
                    ASSIGN tt-embarque-corpo.per-despesa              = (tt-embarque-corpo.vl-ger-tot-des-rea * 100) /
                                                                         tt-embarque-corpo.vl-ger-mercado-rea.
                END.
                ASSIGN tt-embarque-corpo.vl-ger-tot-imp-des-rea       = tt-embarque-corpo.vl-ger-tot-imp-rea
                                                                      + tt-embarque-corpo.vl-ger-tot-des-rea.
            END.

            ASSIGN tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea   = tt-detalha-item.vl-tot-des-rea +
                                                                        tt-detalha-item.vl-tot-imp-rea +
                                                                        tt-detalha-item.vl-mercado-rea
                   tt-detalha-item.vl-tot-sem-ipi-icms-dsp-merc-rea   = tt-detalha-item.vl-tot-des-rea +
                                                                        tt-detalha-item.vl-mercado-rea
                   tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea = tt-embarque-corpo.vl-ger-tot-des-rea +
                                                                        tt-embarque-corpo.vl-ger-tot-imp-rea +
                                                                        tt-embarque-corpo.vl-ger-mercado-rea
                   tt-embarque-corpo.vl-ger-tot-sem-ipi-icms-merc-rea = tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea -
                                                                        tt-embarque-corpo.vl-ger-tot-imp-rea.

            /*PERCENTUAL TT-DETALHA-ITEM - REALIZADO*/
            ASSIGN tt-detalha-item.per-ipi-rea                = (tt-detalha-item.vl-ipi-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100
                   tt-detalha-item.per-icms-rea               = (tt-detalha-item.vl-icms-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100
                   tt-detalha-item.per-pis-rea                = (tt-detalha-item.vl-pis-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100
                   tt-detalha-item.per-cofins-rea             = (tt-detalha-item.vl-cofins-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100
                   tt-detalha-item.per-mercado-orc            = (tt-detalha-item.vl-mercado-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-mercado-rea            = (tt-detalha-item.vl-mercado-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100
                   tt-detalha-item.per-ii-orc                 = (tt-detalha-item.vl-ii-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-ii-rea                 = (tt-detalha-item.vl-ii-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100
                   tt-detalha-item.per-ipi-orc                = (tt-detalha-item.vl-ipi-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-icms-orc               = (tt-detalha-item.vl-icms-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-pis-orc                = (tt-detalha-item.vl-pis-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-cofins-orc             = (tt-detalha-item.vl-cofins-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-tot-imp-orc            = (tt-detalha-item.vl-tot-imp-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-tot-imp-rea            = (tt-detalha-item.vl-tot-imp-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100
                   tt-detalha-item.per-ii2-orc                = (tt-detalha-item.vl-ii2-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-ii2-rea                = (tt-detalha-item.vl-ii2-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100
                   tt-detalha-item.per-tot-des-orc            = (tt-detalha-item.vl-tot-des-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-tot-des-rea            = (tt-detalha-item.vl-tot-des-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100
                   tt-detalha-item.per-tot-imp-des-orc        = (tt-detalha-item.vl-tot-imp-des-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-tot-imp-des-rea        = (tt-detalha-item.vl-tot-imp-des-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100
                   tt-detalha-item.per-tot-com-ipi-icms-orc   = (tt-detalha-item.vl-tot-com-ipi-icms-orc-total
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-tot-com-ipi-icms-rea   = (tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100
                   tt-detalha-item.per-tot-sem-ipi-icms-orc   = (tt-detalha-item.vl-tot-sem-ipi-icms-orc-total
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-tot-sem-ipi-icms-rea   = (tt-detalha-item.vl-tot-sem-ipi-icms-dsp-merc-rea / 
                                                                 tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100.

            IF tt-detalha-item.vl-tot-com-ipi-icms-orc-total = ? THEN
                ASSIGN tt-detalha-item.vl-tot-com-ipi-icms-orc-total = 0.

            IF tt-detalha-item.vl-tot-sem-ipi-icms-orc-total = ? THEN
                ASSIGN tt-detalha-item.vl-tot-sem-ipi-icms-orc-total = 0.

            IF tt-detalha-item.vl-mercado-orc = ? THEN
                ASSIGN tt-detalha-item.vl-mercado-orc = 0.

            IF tt-detalha-item.per-mercado-orc = ? THEN
                ASSIGN tt-detalha-item.per-mercado-orc = 0.

            IF tt-detalha-item.per-mercado-orc = ? THEN
                ASSIGN tt-detalha-item.per-mercado-orc = 0.

            IF tt-detalha-item.per-ii-orc = ? THEN
                ASSIGN tt-detalha-item.per-ii-orc = 0.

            IF tt-detalha-item.per-ipi-orc = ? THEN
                ASSIGN tt-detalha-item.per-ipi-orc = 0.

            IF tt-detalha-item.per-icms-orc = ? THEN
                ASSIGN tt-detalha-item.per-icms-orc = 0.

            IF tt-detalha-item.per-pis-orc = ? THEN
                ASSIGN tt-detalha-item.per-pis-orc = 0.

            IF tt-detalha-item.per-cofins-orc = ? THEN
                ASSIGN tt-detalha-item.per-cofins-orc = 0.

            IF tt-detalha-item.per-tot-imp-orc = ? THEN
                ASSIGN tt-detalha-item.per-tot-imp-orc = 0.

            IF tt-detalha-item.per-ii2-orc = ? THEN
                ASSIGN tt-detalha-item.per-ii2-orc = 0.

            IF tt-detalha-item.per-tot-des-orc = ? THEN
                ASSIGN tt-detalha-item.per-tot-des-orc = 0.

            IF tt-detalha-item.per-tot-imp-des-orc = ? THEN
                ASSIGN tt-detalha-item.per-tot-imp-des-orc = 0.

            IF tt-detalha-item.per-tot-com-ipi-icms-orc = ? THEN
                ASSIGN tt-detalha-item.per-tot-com-ipi-icms-orc = 0.

            IF tt-detalha-item.per-tot-sem-ipi-icms-orc = ? THEN
                ASSIGN tt-detalha-item.per-tot-sem-ipi-icms-orc = 0.

            /*Cÿlculo dos percentuais das despesas do tt-detalha-item - Or»ado e Realizado*/
            FOR EACH  tt-detalha-despesa
                WHERE tt-detalha-despesa.embarque     = embarque-imp.embarque
                  AND tt-detalha-despesa.cod-estabel  = embarque-imp.cod-estabel
                  AND tt-detalha-despesa.numero-ordem = tt-detalha-item.numero-ordem
                  AND tt-detalha-despesa.it-codigo    = tt-detalha-item.it-codigo
                  AND tt-detalha-despesa.sequencia    = tt-detalha-item.sequencia:

                ASSIGN tt-detalha-despesa.per-des-orc = (tt-detalha-despesa.vl-des-orc 
                                                      /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                       tt-detalha-despesa.per-des-rea = (tt-detalha-despesa.vl-des-rea
                                                      /  tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100.
                IF tt-detalha-despesa.per-des-orc = ? THEN
                    ASSIGN tt-detalha-despesa.per-des-orc = 0.

            END.
            ASSIGN de-tot-orc = 0.
            FOR EACH tt-detalha-despesa-aux
               WHERE tt-detalha-despesa-aux.embarque    = tt-embarque-corpo.embarque
                 AND tt-detalha-despesa-aux.cod-estabel = tt-embarque-corpo.cod-estabel
                 AND tt-detalha-despesa-aux.sequencia   = tt-detalha-despesa-aux.sequencia:
                 
                ASSIGN de-tot-orc = de-tot-orc + tt-detalha-despesa-aux.vl-des-orc.
            END.
        END. /*tt-detalha-item*/

        /*Cÿlculo dos percentuais das despesas do tt-embarque-corpo - Or»ado e Realizado*/
        /*IF tt-param.i-desp-imposto = 1 THEN /*Despesa*/
            ASSIGN tt-embarque-corpo.vl-ger-tot-des-orc  = tt-embarque-corpo.vl-ger-ii2-orc
                                                         + de-tot-orc.
        ELSE /*Imposto*/
            ASSIGN tt-embarque-corpo.vl-ger-tot-des-orc  = de-tot-orc.*/

        /*ASSIGN tt-embarque-corpo.vl-ger-tot-imp-des-orc       = tt-embarque-corpo.vl-ger-tot-imp-orc 
                                                              + tt-embarque-corpo.vl-ger-tot-des-orc
               tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc  = tt-embarque-corpo.vl-ger-mercado-orc              
                                                              + tt-embarque-corpo.vl-ger-tot-imp-orc
                                                              + tt-embarque-corpo.vl-ger-tot-des-orc
               tt-embarque-corpo.vl-ger-tot-sem-ipi-icms-orc  = tt-embarque-corpo.vl-ger-mercado-orc
                                                              + tt-embarque-corpo.vl-ger-tot-des-orc.*/
        FOR EACH tt-detalha-despesa-aux
           WHERE tt-detalha-despesa-aux.embarque    = embarque-imp.embarque
             AND tt-detalha-despesa-aux.cod-estabel = embarque-imp.cod-estabel:
            IF tt-param.l-desp-imposto THEN DO:
               /*ASSIGN tt-detalha-despesa-aux.per-des-orc  = (tt-detalha-despesa-aux.vl-des-orc / 
                                                             tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
                      tt-detalha-despesa-aux.per-des-rea  = (tt-detalha-despesa-aux.vl-des-rea /
                                                             tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100.*/
               IF tt-detalha-despesa-aux.per-des-orc = ? THEN
                   ASSIGN tt-detalha-despesa-aux.per-des-orc = 0.
            END.
        END.
                
        /* PERCENTUAIS */ 
        assign /* -------------- OR°ADO -------------- */
               /*tt-embarque-corpo.per-ger-mercado-orc          = (tt-embarque-corpo.vl-ger-mercado-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
               tt-embarque-corpo.per-ger-ii-orc               = (tt-embarque-corpo.vl-ger-ii-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
               tt-embarque-corpo.per-ger-ipi-orc              = (tt-embarque-corpo.vl-ger-ipi-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
               tt-embarque-corpo.per-ger-icms-orc             = (tt-embarque-corpo.vl-ger-icms-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
               tt-embarque-corpo.per-ger-pis-orc              = (tt-embarque-corpo.vl-ger-pis-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
               tt-embarque-corpo.per-ger-cofins-orc           = (tt-embarque-corpo.vl-ger-cofins-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
               tt-embarque-corpo.per-ger-tot-imp-orc          = (tt-embarque-corpo.vl-ger-tot-imp-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
               tt-embarque-corpo.per-ger-ii2-orc              = (tt-embarque-corpo.vl-ger-ii2-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
               tt-embarque-corpo.per-ger-des-orc              = (tt-embarque-corpo.vl-ger-des-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
               tt-embarque-corpo.per-ger-tot-des-orc          = (tt-embarque-corpo.vl-ger-tot-des-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
               tt-embarque-corpo.per-ger-tot-imp-des-orc      = (tt-embarque-corpo.vl-ger-tot-imp-des-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
               tt-embarque-corpo.per-ger-tot-com-ipi-icms-orc = (tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
               tt-embarque-corpo.per-ger-tot-sem-ipi-icms-orc = (tt-embarque-corpo.vl-ger-tot-sem-ipi-icms-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100*/
    
               /* -------------- REALIZADO -------------- */
               tt-embarque-corpo.per-ger-mercado-rea          = (tt-embarque-corpo.vl-ger-mercado-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100
               tt-embarque-corpo.per-ger-ii-rea               = (tt-embarque-corpo.vl-ger-ii-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100
               tt-embarque-corpo.per-ger-ipi-rea              = (tt-embarque-corpo.vl-ger-ipi-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100
               tt-embarque-corpo.per-ger-icms-rea             = (tt-embarque-corpo.vl-ger-icms-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100
               tt-embarque-corpo.per-ger-pis-rea              = (tt-embarque-corpo.vl-ger-pis-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100
               tt-embarque-corpo.per-ger-cofins-rea           = (tt-embarque-corpo.vl-ger-cofins-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100
               tt-embarque-corpo.per-ger-tot-imp-rea          = (tt-embarque-corpo.vl-ger-tot-imp-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100
               tt-embarque-corpo.per-ger-ii2-rea              = (tt-embarque-corpo.vl-ger-ii2-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100
               tt-embarque-corpo.per-ger-des-rea              = (tt-embarque-corpo.vl-ger-des-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100
               tt-embarque-corpo.per-ger-tot-des-rea          = (tt-embarque-corpo.vl-ger-tot-des-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100
               tt-embarque-corpo.per-ger-tot-imp-des-rea      = (tt-embarque-corpo.vl-ger-tot-imp-des-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100
               tt-embarque-corpo.per-ger-tot-com-ipi-icms-rea = (tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100
               tt-embarque-corpo.per-ger-tot-sem-ipi-icms-rea = (tt-embarque-corpo.vl-ger-tot-sem-ipi-icms-merc-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100.


               /*IF tt-embarque-corpo.per-ger-mercado-orc = ? THEN
                   ASSIGN tt-embarque-corpo.per-ger-mercado-orc = 0.*/
                                                              
               /*IF tt-embarque-corpo.per-ger-ii-orc = ? THEN 
                   ASSIGN tt-embarque-corpo.per-ger-ii-orc = 0.*/
                                                              
               /*IF tt-embarque-corpo.per-ger-ipi-orc = ? THEN
                   ASSIGN tt-embarque-corpo.per-ger-ipi-orc = 0.*/
                                                              
               /*IF tt-embarque-corpo.per-ger-icms-orc = ? THEN
                   ASSIGN tt-embarque-corpo.per-ger-icms-orc = 0.*/
                                                              
               /*IF tt-embarque-corpo.per-ger-pis-orc = ? THEN
                   ASSIGN tt-embarque-corpo.per-ger-pis-orc = 0.*/
                                                              
               /*IF tt-embarque-corpo.per-ger-cofins-orc = ? THEN
                   ASSIGN tt-embarque-corpo.per-ger-cofins-orc = 0.*/
                                                              
               /*IF tt-embarque-corpo.per-ger-tot-imp-orc = ? THEN
                   ASSIGN tt-embarque-corpo.per-ger-tot-imp-orc = 0.*/
                                                              
               /*IF tt-embarque-corpo.per-ger-ii2-orc = ? THEN
                   ASSIGN tt-embarque-corpo.per-ger-ii2-orc = 0.*/
                                                              
               /*IF tt-embarque-corpo.per-ger-des-orc = ? THEN
                   ASSIGN tt-embarque-corpo.per-ger-des-orc = 0.*/
                                                              
               /*IF tt-embarque-corpo.per-ger-tot-des-orc = ? THEN
                   ASSIGN tt-embarque-corpo.per-ger-tot-des-orc = 0.*/
                                                              
               /*IF tt-embarque-corpo.per-ger-tot-imp-des-orc = ? THEN
                   ASSIGN tt-embarque-corpo.per-ger-tot-imp-des-orc = 0.*/
                                                              
               /*IF tt-embarque-corpo.per-ger-tot-com-ipi-icms-orc = ? THEN
                   ASSIGN tt-embarque-corpo.per-ger-tot-com-ipi-icms-orc = 0.*/
                                                              
               /*IF tt-embarque-corpo.per-ger-tot-sem-ipi-icms-orc = ? THEN
                   ASSIGN tt-embarque-corpo.per-ger-tot-sem-ipi-icms-orc = 0.*/
                                                              
    END.
    
END. /*embarque-imp*/

PROCEDURE converte-moeda:
    def input-output param p-vl-conversao as decimal no-undo.
    def input parameter p-moeda like moeda.mo-codigo no-undo.

    run cdp/cd0812.p (input  p-moeda,         /*moeda origem*/
                      input  tt-param.moeda,  /*moeda destino*/
                      input  p-vl-conversao,  /*valor origem*/
                      input  d-data-conv,     /*data conversao*/
                      output p-vl-conversao). /*valor destino*/


END PROCEDURE.


PROCEDURE gera-valores:
    define input parameter p-serie        like docum-est.serie          no-undo.
    define input parameter p-nro-docto    like docum-est.nro-docto      no-undo. 
    define input parameter p-cod-emitente like docum-est.cod-emitente   no-undo. 
    define input parameter p-nat-operacao like docum-est.nat-operacao   no-undo. 
    DEFINE INPUT PARAMETER pc-embarque    LIKE embarque-imp.embarque    NO-UNDO.
    DEFINE INPUT PARAMETER pc-cod-estabel LIKE embarque-imp.cod-estabel NO-UNDO.
    define input parameter p-situacao     as int                        no-undo. 
    
    DEF VAR de-tot-des-orc-aux AS DECIMAL NO-UNDO.
    DEF VAR de-tot-desp        AS DECIMAL NO-UNDO.
    DEF VAR de-tot-desp-aux    AS DECIMAL NO-UNDO. 
    DEF VAR de-val-desp-aux    AS DECIMAL NO-UNDO.
    DEF VAR de-val-desp-nfc    AS DECIMAL NO-UNDO.
    def var de-val-desp        as decimal no-undo.
    DEF VAR de-tot-imp-rea     AS DECIMAL NO-UNDO.

    /* p-situacao..: 1- Nota Principal    **
    **               2- Nota Relacionada  **
    **               3- Nota Complementar */

    for each  item-doc-est fields( serie-docto   nro-docto      cod-emitente   nat-operacao  sequencia
                                   it-codigo     preco-unit[1]  numero-ordem   quantidade    qt-do-forn
                                   aliquota-ipi  aliquota-icm   valor-ipi[1]   valor-icm[1]  un parcela
                                   &if "{&bf_mat_versao_ems}" >= "2.06" &then
                                       num-ord-import num-parc-import valor-pis val-cofins val-aliq-pis  val-aliq-cofins 
                                   &else
                                       char-2
                                   &endif
                                   )
        where item-doc-est.serie-docto  =  p-serie
          and item-doc-est.nro-docto    =  p-nro-docto
          and item-doc-est.cod-emitente =  p-cod-emitente
          and item-doc-est.nat-operacao =  p-nat-operacao no-lock
        use-index documento
        break by item-doc-est.it-codigo
              by &if "{&bf_mat_versao_ems}" >= "2.06" &then
                     item-doc-est.num-ord-import
                 &ELSE
                     int(substr(item-doc-est.char-2,145,8))
                 &endif
              by item-doc-est.numero-ordem:

        FIND FIRST item-doc-est-cex OF item-doc-est NO-LOCK NO-ERROR.
        IF  AVAIL item-doc-est-cex THEN DO:
                IF  p-situacao = 3 AND
                &if "{&bf_mat_versao_ems}" >= "2.06" &THEN
                    item-doc-est-cex.embarque <> pc-embarque
                &ELSE
                    (trim(substr(item-doc-est-cex.char-1,1,12)) <> pc-embarque)
                &ENDIF
                THEN
                NEXT.
        END.

        if item-doc-est.it-codigo < tt-param.c-it-codigo-ini or
           item-doc-est.it-codigo > tt-param.c-it-codigo-fim then next.

        for first item fields(it-codigo  ge-codigo  fm-codigo desc-item un)
            where item.it-codigo =  item-doc-est.it-codigo
              and item.ge-codigo >= tt-param.i-ge-codigo-ini
              and item.ge-codigo <= tt-param.i-ge-codigo-fim
              and item.fm-codigo >= tt-param.c-fm-codigo-ini
              and item.fm-codigo <= tt-param.c-fm-codigo-fim no-lock:
        end.
        if  not avail item then next.

        /*Notas Complementares*/
        if  p-situacao = 3 then do:
        
            for first  tt-detalha-item
                 where tt-detalha-item.embarque    = embarque-imp.embarque
                 and   tt-detalha-item.cod-estabel = embarque-imp.cod-estabel
                 and   tt-detalha-item.it-codigo   = item-doc-est.it-codigo 
                 AND   tt-detalha-item.sequencia   = item-doc-est.sequencia no-lock
                use-index ch-codigo:
            end.
            if  not avail tt-detalha-item then next.
            
            for first ordens-embarque fields(numero-ordem)
                where ordens-embarque.embarque    = embarque-imp.embarque
                  and ordens-embarque.cod-estabel = embarque-imp.cod-estabel no-lock: end. 
            if  not avail ordens-embarque then next. 
            
            for first ordem-compra 
                where ordem-compra.numero-ordem = ordens-embarque.numero-ordem no-lock: end. 
            if  not avail ordem-compra then next.

            &if "{&bf_mat_versao_ems}" >= "2.05" &then
                for first cotacao-item fields( numero-ordem  cod-emitente  mo-codigo  aliquota-ii
                                               data-cotacao  cot-aprovada  it-codigo  seq-cotac char-1)
                    where cotacao-item.numero-ordem = ordem-compra.numero-ordem
                      and cotacao-item.cod-emitente = ordem-compra.cod-emitente
                      and cotacao-item.data-cotacao = ordem-compra.data-cotacao
                      and cotacao-item.cot-aprovada = yes no-lock:
                end.
            &else
                for first cotacao-item fields( numero-ordem  cod-emitente  mo-codigo  char-1
                                               data-cotacao  cot-aprovada  it-codigo  seq-cotac )
                    where cotacao-item.numero-ordem = ordem-compra.numero-ordem
                      and cotacao-item.cod-emitente = ordem-compra.cod-emitente
                      and cotacao-item.data-cotacao = ordem-compra.data-cotacao
                      and cotacao-item.cot-aprovada = yes no-lock:
                end.
            &endif
            if  not avail cotacao-item then
                next.
            
            assign tot-despesa-rea = 0
                   de-val-desp     = 0
                   de-tot-desp-aux = 0.
            
            for each  item-doc-est-cex fields(val-desp nro-docto sequencia)
                    where item-doc-est-cex.serie-docto  = item-doc-est.serie-docto
                    and   item-doc-est-cex.nro-docto    = item-doc-est.nro-docto
                    and   item-doc-est-cex.cod-emitente = item-doc-est.cod-emitente
                    and   item-doc-est-cex.nat-operacao = item-doc-est.nat-operacao
                    and   item-doc-est-cex.sequencia    = item-doc-est.sequencia
                    &if "{&bf_mat_versao_ems}" <= "2.05" &then
                        and  trim(substr(item-doc-est-cex.char-1,1,12)) = embarque-imp.embarque
                        and  trim(substr(item-doc-est-cex.char-1,13,3)) = pc-cod-estabel no-lock
                    &else
                        and  item-doc-est-cex.embarque    = embarque-imp.embarque
                        and  item-doc-est-cex.cod-estabel = pc-cod-estabel no-lock
                     &endif
                     use-index codigo:

                     assign de-val-desp = de-val-desp + item-doc-est-cex.val-desp.
            end.
            
            for each  item-doc-est-cex fields(mo-codigo cod-desp val-desp nro-docto sequencia)
                where item-doc-est-cex.serie-docto  = item-doc-est.serie-docto   
                and   item-doc-est-cex.nro-docto    = item-doc-est.nro-docto     
                and   item-doc-est-cex.cod-emitente = item-doc-est.cod-emitente 
                and   item-doc-est-cex.nat-operacao = item-doc-est.nat-operacao 
                and   item-doc-est-cex.sequencia    = item-doc-est.sequencia 
                &if "{&bf_mat_versao_ems}" <= '2.05' &then
                    and  trim(substr(item-doc-est-cex.char-1,1,12)) = embarque-imp.embarque
                    and  trim(substr(item-doc-est-cex.char-1,13,3)) = pc-cod-estabel no-lock
                &else
                    and  item-doc-est-cex.embarque    = embarque-imp.embarque
                    and  item-doc-est-cex.cod-estabel = pc-cod-estabel no-lock
                &endif
                use-index codigo:

                /****OR°ADO-INI**/
                for each  cotacao-item-cex fields( numero-ordem  cod-emitente  mapa-cotacao
                                                   cod-incoterm  it-codigo     seq-cotac
                                                   data-cotacao  cod-itiner    cod-desp
                                                   val-desp      mo-codigo)
                    where cotacao-item-cex.numero-ordem = cotacao-item.numero-ordem
                    and   cotacao-item-cex.cod-emitente = cotacao-item.cod-emitente
                    and   cotacao-item-cex.it-codigo    = item-doc-est.it-codigo
                    and   cotacao-item-cex.mapa-cotacao = int(trim(substring(cotacao-item.char-1,1,20)))
                    and   cotacao-item-cex.cod-incoterm = embarque-imp.cod-incoterm
                    and   cotacao-item-cex.cod-itiner   = itinerario.cod-itiner no-lock:
                    
                    for first desp-imp fields (cod-desp tipo descricao)
                         where desp-imp.cod-desp = cotacao-item-cex.cod-desp no-lock: 
                    end.
                    if  not avail desp-imp then next.

                    if  tt-param.i-custo-item = 2 and desp-imp.tipo <> 1 then /*Total e Diferente de Fixo*/
                        assign vl-desp-orcad-det = cotacao-item-cex.val-desp
                                                 * item-doc-est.quantidade.
                    else
                        assign vl-desp-orcad-det = cotacao-item-cex.val-desp.
                        
                    if  tt-param.i-custo-item = 2 and desp-imp.tipo <> 1 then /*Total e Diferente de Fixo*/
                        ASSIGN vl-desp-orcad-aux = cotacao-item-cex.val-desp
                                                 * item-doc-est.quantidade.
                    ELSE
                        ASSIGN vl-desp-orcad-aux = cotacao-item-cex.val-desp.

                    /*Or»ado*/
                    IF  desp-imp.cod-desp <> i-cod-desp-ii 
                    &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
                    AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-ipi 
                    AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-icms 
                    AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-pis
                    AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-cofins
                    &ENDIF
                    THEN DO:

                        for first tt-detalha-despesa-aux
                             where tt-detalha-despesa-aux.embarque     = embarque-imp.embarque
                               and tt-detalha-despesa-aux.cod-estabel  = embarque-imp.cod-estabel
                               and tt-detalha-despesa-aux.cod-despesa  = desp-imp.cod-desp 
                               AND tt-detalha-despesa-aux.sequencia    = item-doc-est.sequencia no-lock
                            use-index ch-codigo:
                        end.
                        if not avail tt-detalha-despesa-aux then do:
                            CREATE tt-detalha-despesa-aux.
                            ASSIGN tt-detalha-despesa-aux.embarque     = embarque-imp.embarque
                                   tt-detalha-despesa-aux.cod-estabel  = embarque-imp.cod-estabel
                                   tt-detalha-despesa-aux.cod-despesa  = desp-imp.cod-desp
                                   tt-detalha-despesa-aux.desc-despesa = desp-imp.descricao
                                   tt-detalha-despesa-aux.vl-des-orc   = vl-desp-orcad-aux
                                   tt-detalha-despesa-aux.sequencia    = item-doc-est.sequencia.
                            
                            if tt-param.moeda <> var-moeda-conv-orc THEN DO:
                               IF AVAIL tt-detalha-despesa-aux THEN
                                  run converte-moeda (input-output tt-detalha-despesa-aux.vl-des-orc,
                                                      input var-moeda-conv-orc).
                            END.
                            ASSIGN de-tot-desp-aux = de-tot-desp-aux + tt-detalha-despesa-aux.vl-des-orc.
                        END.
                        ELSE DO:
                            ASSIGN de-tot-desp-aux = de-tot-desp-aux + tt-detalha-despesa-aux.vl-des-orc.
                        END.
                    END.

                    /*----- Notas Complementares -----*/
                    IF  desp-imp.cod-desp <> i-cod-desp-ii 
                    &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
                    AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-ipi  
                    AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-icms 
                    AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-pis
                    AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-cofins
                    &ENDIF
                    THEN DO:
                        find FIRST tt-detalha-despesa 
                             WHERE tt-detalha-despesa.embarque    = embarque-imp.embarque 
                               AND tt-detalha-despesa.cod-estabel = embarque-imp.cod-estabel
                               AND tt-detalha-despesa.cod-despesa = desp-imp.cod-desp no-lock no-error.
                        if not avail tt-detalha-despesa then do:
                            create tt-detalha-despesa.
                            assign tt-detalha-despesa.embarque      = embarque-imp.embarque
                                   tt-detalha-despesa.cod-estabel   = embarque-imp.cod-estabel
                                   tt-detalha-despesa.numero-ordem  = cotacao-item-cex.numero-ordem
                                   tt-detalha-despesa.nro-docto     = item-doc-est.nro-docto
                                   tt-detalha-despesa.cod-despesa   = desp-imp.cod-desp
                                   tt-detalha-despesa.desc-despesa  = desp-imp.descricao
                                   tt-detalha-despesa.situacao      = p-situacao
                                   tt-detalha-despesa.vl-des-orc    = vl-desp-orcad-det
                                   tot-despesa-orc                  = tot-despesa-orc
                                                                    + vl-desp-orcad-det
                                   tt-detalha-despesa.it-codigo     = item-doc-est.it-codigo.
        
                            assign var-moeda-conv-orc = cotacao-item-cex.mo-codigo.
        
                            if  tt-param.moeda <> var-moeda-conv-orc THEN 
                                run converte-moeda (input-output tt-detalha-despesa.vl-des-orc,
                                                    input var-moeda-conv-orc).
                        END.
                    END.
                end. /*cotacao-item-cex*/

                /*Convers’o do Valor Adicional*/
                if  tt-param.i-custo-item = 1 then
                    assign var-preco-unit = de-val-desp / tt-detalha-item.quantidade.
                else 
                    assign var-preco-unit = de-val-desp.


                assign var-preco-unit-aux = de-val-desp. /*item-doc-est.preco-unit[1].*/
                

                if  tt-param.moeda <> item-doc-est-cex.mo-codigo then do:
                    run converte-moeda (input-output var-preco-unit,
                                        input item-doc-est-cex.mo-codigo).
                
                    run converte-moeda (input-output var-preco-unit-aux,
                                        input item-doc-est-cex.mo-codigo).
                end.

                assign var-valor-ipi-mattran-rea    = item-doc-est.valor-ipi[1]
                       var-valor-icm-mattran-rea    = item-doc-est.valor-icm[1]
                       var-valor-pis-mattran-rea    = &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
                                                          item-doc-est.valor-pis &ELSE DEC(substring(item-doc-est.char-2,41,14)) &ENDIF
                       var-valor-cofins-mattran-rea = &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
                                                          item-doc-est.val-cofins &ELSE DEC(substring(item-doc-est.char-2,103,14)) &ENDIF. 

                if  tt-param.i-custo-item = 1 then /*Unitario*/
                    assign var-valor-ipi-mattran-rea    = var-valor-ipi-mattran-rea    / tt-detalha-item.quantidade
                           var-valor-icm-mattran-rea    = var-valor-icm-mattran-rea    / tt-detalha-item.quantidade
                           var-valor-pis-mattran-rea    = var-valor-pis-mattran-rea    / tt-detalha-item.quantidade
                           var-valor-cofins-mattran-rea = var-valor-cofins-mattran-rea / tt-detalha-item.quantidade.
                
                for first desp-imp
                    where desp-imp.cod-desp = item-doc-est-cex.cod-desp no-lock: end.

                FOR FIRST b-item-doc-est-aux2
                    WHERE b-item-doc-est-aux2.serie-docto  = b-item-doc-est.serie-docto 
                      AND b-item-doc-est-aux2.nro-docto    = b-item-doc-est.nro-docto   
                      AND b-item-doc-est-aux2.cod-emitente = b-item-doc-est.cod-emitente
                      AND b-item-doc-est-aux2.nat-operacao = b-item-doc-est.nat-operacao
                      AND b-item-doc-est-aux2.sequencia    = item-doc-est.sequencia NO-LOCK:
                END.

                if  avail b-item-doc-est THEN
                    for first b-item-doc-est-aux 
                        where b-item-doc-est-aux.serie-docto  = b-item-doc-est.serie-docto
                        and   b-item-doc-est-aux.nro-docto    = b-item-doc-est.nro-docto   
                        and   b-item-doc-est-aux.cod-emitente = b-item-doc-est.cod-emitente 
                        and   b-item-doc-est-aux.nat-operacao = b-item-doc-est.nat-operacao 
                        and   b-item-doc-est-aux.it-codigo    = item-doc-est.it-codigo no-lock: 
                    end.

                find first tt-detalha-despesa
                    where  tt-detalha-despesa.embarque     = embarque-imp.embarque
                    and    tt-detalha-despesa.cod-estabel  = embarque-imp.cod-estabel
                    and    tt-detalha-despesa.numero-ordem = (&if "{&bf_mat_versao_ems}" >= "2.06" &then 
                                                                  if  docum-est.idi-nf-simples-remes = 1 then
                                                                      b-item-doc-est-aux2.num-ord-import 
                                                              &else 
                                                                  if  int(substring(docum-est.char-2,101,1)) = 1 then
                                                                      int(substring(b-item-doc-est.char-2,145,9))
                                                              &endif
                                                                  else if avail b-item-doc-est-aux2 then 
                                                                      b-item-doc-est-aux2.numero-ordem 
                                                                      else 
                                                                          b-item-doc-est.numero-ordem)
                    and    tt-detalha-despesa.nro-docto    = item-doc-est.nro-docto 
                    and    tt-detalha-despesa.cod-despesa  = desp-imp.cod-desp 
                    and    tt-detalha-despesa.it-codigo    = item-doc-est.it-codigo
                    AND    tt-detalha-despesa.sequencia    = b-item-doc-est-aux2.sequencia no-lock no-error.

                if  not avail tt-detalha-despesa then do:
                    create tt-detalha-despesa.
                    assign tt-detalha-despesa.embarque      = embarque-imp.embarque
                           tt-detalha-despesa.cod-estabel   = embarque-imp.cod-estabel
                           tt-detalha-despesa.numero-ordem  = (&if "{&bf_mat_versao_ems}" >= "2.06" &then 
                                                                   if  docum-est.idi-nf-simples-remes = 1 then
                                                                       b-item-doc-est-aux2.num-ord-import 
                                                               &else 
                                                                   if  int(substring(docum-est.char-2,101,1)) = 1 then 
                                                                       int(substring(b-item-doc-est.char-2,145,9))
                                                               &endif
                                                                   else if avail b-item-doc-est-aux2 then
                                                                       b-item-doc-est-aux2.numero-ordem 
                                                                       else
                                                                           b-item-doc-est.numero-ordem)
                           tt-detalha-despesa.nro-docto     = item-doc-est.nro-docto
                           tt-detalha-despesa.cod-despesa   = desp-imp.cod-desp
                           tt-detalha-despesa.desc-despesa  = desp-imp.descricao
                           tt-detalha-despesa.it-codigo     = item-doc-est.it-codigo
                           tt-detalha-despesa.situacao      = p-situacao
                           tot-despesa-orc                  = tot-despesa-orc
                                                            + vl-desp-orcad-det.
                           tt-detalha-despesa.sequencia     = b-item-doc-est-aux2.sequencia.
                END.
                IF tt-detalha-despesa.sequencia = tt-detalha-item.sequencia THEN DO:
                    IF tt-param.i-custo-item = 1 then DO: /*Unitario*/
                        assign tt-detalha-despesa.vl-des-rea = TRUNC((item-doc-est-cex.val-desp / tt-detalha-item.quantidade),2).
                        IF tt-param.moeda <> var-moeda-conv-rea THEN DO:
                           RUN converte-moeda (INPUT-OUTPUT tt-detalha-despesa.vl-des-rea,
                                               INPUT var-moeda-conv-rea).
                        END.
                        ASSIGN de-tot-desp  = de-tot-desp + tt-detalha-despesa.vl-des-rea.
                    END.
                    ELSE DO:
                        assign tt-detalha-despesa.vl-des-rea = item-doc-est-cex.val-desp.
                        if  tt-param.moeda <> var-moeda-conv-rea THEN DO:
                            RUN converte-moeda (INPUT-OUTPUT tt-detalha-despesa.vl-des-rea,
                                                INPUT var-moeda-conv-rea).
                        END.
                        ASSIGN de-tot-desp  = de-tot-desp + tt-detalha-despesa.vl-des-rea.
                    END.
                END.

                /*DESPESAS DA NF COMPLEMENTAR*/ 
                ASSIGN de-val-desp-nfc = item-doc-est-cex.val-desp.
                if  tt-param.moeda <> var-moeda-conv-rea THEN DO:
                    RUN converte-moeda (INPUT-OUTPUT de-val-desp-nfc,
                                        INPUT var-moeda-conv-rea).
                END.
                for first tt-detalha-despesa-aux
                     where tt-detalha-despesa-aux.embarque     = tt-detalha-despesa.embarque
                       and tt-detalha-despesa-aux.cod-estabel  = tt-detalha-despesa.cod-estabel
                       and tt-detalha-despesa-aux.cod-despesa  = desp-imp.cod-desp no-lock
                    use-index ch-codigo:
                end.

                if not avail tt-detalha-despesa-aux then do:
                   CREATE tt-detalha-despesa-aux.
                   ASSIGN tt-detalha-despesa-aux.embarque     = embarque-imp.embarque
                          tt-detalha-despesa-aux.cod-estabel  = embarque-imp.cod-estabel
                          tt-detalha-despesa-aux.cod-despesa  = desp-imp.cod-desp
                          tt-detalha-despesa-aux.desc-despesa = desp-imp.descricao
                          tt-detalha-despesa-aux.vl-des-rea   = de-val-desp-nfc.
                    
                END.
                ELSE DO:
                    ASSIGN tt-detalha-despesa-aux.vl-des-rea = tt-detalha-despesa-aux.vl-des-rea + de-val-desp-nfc.
                END.
                
                ASSIGN de-val-desp-nfc2 = de-val-desp-nfc2 + de-val-desp-nfc.

                assign tt-detalha-item.vl-ipi-rea     = tt-detalha-item.vl-ipi-rea     + var-valor-ipi-mattran-rea
                       tt-detalha-item.vl-icms-rea    = tt-detalha-item.vl-icms-rea    + var-valor-icm-mattran-rea
                       tt-detalha-item.vl-pis-rea     = tt-detalha-item.vl-pis-rea     + var-valor-pis-mattran-rea
                       tt-detalha-item.vl-cofins-rea  = tt-detalha-item.vl-cofins-rea  + var-valor-cofins-mattran-rea
                       tt-detalha-item.vl-tot-imp-rea = tt-detalha-item.vl-tot-imp-rea + var-valor-ipi-mattran-rea + var-valor-icm-mattran-rea + var-valor-pis-mattran-rea + var-valor-cofins-mattran-rea .

                ASSIGN de-tot-imp-rea = tt-detalha-item.vl-tot-imp-rea.

                /*Re-calcular Percentuais*/
                assign tt-detalha-item.per-mercado-rea            = (tt-detalha-item.vl-mercado-rea
                                                                  /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                       tt-detalha-item.per-ii-rea                 = (tt-detalha-item.vl-ii-rea
                                                                  /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                       tt-detalha-item.per-ipi-rea                = (tt-detalha-item.vl-ipi-rea
                                                                  / tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                       tt-detalha-item.per-icms-rea               = (tt-detalha-item.vl-icms-rea
                                                                  /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                       tt-detalha-item.per-pis-rea                = (tt-detalha-item.vl-pis-rea
                                                                  /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                       tt-detalha-item.per-cofins-rea             = (tt-detalha-item.vl-cofins-rea
                                                                  /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                       tt-detalha-item.per-tot-imp-rea            = (tt-detalha-item.vl-tot-imp-rea
                                                                  /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                       tt-detalha-item.per-ii2-rea                = (tt-detalha-item.vl-ii2-rea
                                                                  /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                       tt-detalha-item.per-tot-des-rea            = (tt-detalha-item.vl-tot-des-rea
                                                                  /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                       tt-detalha-item.per-tot-imp-des-rea        = (tt-detalha-item.vl-tot-imp-des-rea
                                                                  /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                       tt-detalha-item.per-tot-com-ipi-icms-rea   = (tt-detalha-item.vl-tot-com-ipi-icms-rea
                                                                  /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                       tt-detalha-item.per-tot-sem-ipi-icms-rea   = (tt-detalha-item.vl-tot-sem-ipi-icms-rea
                                                                  /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100.

            end.  /*item-doc-est-cex*/
            
            IF tt-param.i-custo-item = 1  then do: /*Unitario*/
                assign tt-detalha-item.vl-des-rea                    = tt-detalha-item.vl-des-rea     
                                                                     + de-tot-desp
                       tt-detalha-item.vl-tot-despesa                = tt-detalha-item.vl-tot-despesa 
                                                                     + de-tot-desp
                       tt-detalha-item.vl-tot-des-rea                = tt-detalha-item.vl-tot-des-rea 
                                                                     + de-tot-desp 
                       tt-detalha-item.vl-des-rea-total              = tt-detalha-item.vl-des-rea-total
                                                                     + (de-tot-desp * tt-detalha-item.quantidade)
                       tt-detalha-item.vl-tot-imp-des-rea            = tt-detalha-item.vl-tot-imp-des-rea 
                                                                     + de-tot-desp
                       tt-detalha-item.vl-tot-des-rea-total          = tt-detalha-item.vl-tot-des-rea-total 
                                                                     + (de-tot-desp * tt-detalha-item.quantidade)
                       tt-detalha-item.vl-tot-imp-des-rea-total      = tt-detalha-item.vl-tot-imp-des-rea-total 
                                                                     + (de-tot-desp * tt-detalha-item.quantidade)
                       tt-detalha-item.vl-tot-com-ipi-icms-rea-total = tt-detalha-item.vl-tot-com-ipi-icms-rea-total
                                                                     + (de-tot-desp * tt-detalha-item.quantidade) 
                                                                     - de-tot-desp
                       tt-detalha-item.vl-tot-sem-ipi-icms-rea-total = tt-detalha-item.vl-tot-sem-ipi-icms-rea-total 
                                                                     + (de-tot-desp * tt-detalha-item.quantidade)
                                                                     - de-tot-desp
                       de-tot-desp = 0.
            end.
            else do:
                assign tt-detalha-item.vl-tot-des-rea                = tt-detalha-item.vl-tot-des-rea 
                                                                     + de-tot-desp
                       tt-detalha-item.vl-des-rea-total              = tt-detalha-item.vl-des-rea-total
                                                                     + (de-tot-desp)
                       tt-detalha-item.vl-tot-imp-des-rea            = de-tot-imp-rea 
                                                                     + IF tt-detalha-item.vl-des-rea = 0 THEN de-tot-desp ELSE tt-detalha-item.vl-des-rea
                       tt-detalha-item.vl-tot-des-rea-total          = tt-detalha-item.vl-tot-des-rea-total
                                                                     + (de-tot-desp)
                       tt-detalha-item.vl-tot-imp-des-rea-total      = tt-detalha-item.vl-tot-imp-des-rea-total  
                                                                     + (de-tot-desp)
                       tt-detalha-item.vl-tot-com-ipi-icms-rea-total = tt-detalha-item.vl-tot-com-ipi-icms-rea-total
                       tt-detalha-item.vl-tot-sem-ipi-icms-rea-total = tt-detalha-item.vl-tot-sem-ipi-icms-rea-total
                       tt-detalha-item.vl-des-rea                    = tt-detalha-item.vl-des-rea 
                                                                     + de-tot-desp
                       tt-detalha-item.vl-tot-despesa                = tt-detalha-item.vl-tot-despesa 
                                                                     + de-tot-desp.
                ASSIGN de-tot-imp-rea = 0
                       de-tot-desp    = 0.
                
            end.
        end. /*nota fiscal complementar*/
        
        /*Nota Principal e Relacionadas*/
        ELSE DO: 
            /*aqui or»ado*/
            IF  tt-param.l-desemb-parc and p-situacao <> 1 and
                trim(substring(embarque-imp.char-1,21,20)) <> "" then next.

            for first ordem-compra fields(numero-ordem  char-2  num-pedido  cod-emitente
                                           data-cotacao  preco-fornec preco-unit aliquota-ipi)
                where ordem-compra.numero-ordem = (&if "{&bf_mat_versao_ems}" >= "2.06" &then 
                                                     if docum-est.idi-nf-simples-remes = 1 then
                                                         item-doc-est.num-ord-import 
                                                   &else 
                                                     if int(substring(docum-est.char-2,101,1)) = 1 then 
                                                         int(substring(item-doc-est.char-2,145,8)) 
                                                   &endif 
                                                     else item-doc-est.numero-ordem)
                                                  no-lock:
            end.

            if  not avail ordem-compra then next.
            
            if  p-situacao = 1 then do: 
                &if "{&bf_mat_versao_ems}" >= "2.06" &then
                    if docum-est.idi-nf-simples-remes = 1 then do:
                        if first-of(item-doc-est.num-ord-import) then

                &else
                    if int(substring(docum-est.char-2,101,1)) = 1 then do:
                        if first-of(int(substring(item-doc-est.char-2,145,8))) then
                &endif
                        assign l-cria-embarque-corpo = yes.
                end.
                else if first-of(item-doc-est.numero-ordem) then
                    assign l-cria-embarque-corpo = yes.
                
                if l-cria-embarque-corpo then do : /*Nota Principal*/
                    create tt-embarque-corpo.
                    assign tt-embarque-corpo.embarque      = embarque-imp.embarque
                           tt-embarque-corpo.cod-estabel   = embarque-imp.cod-estabel
                           /*tt-embarque-corpo.desc-estabel  = c-desc-estabel*/
                           tt-embarque-corpo.cod-emitente  = processo-imp.cod-exportador
                           /*tt-embarque-corpo.desc-emitente = c-desc-emitente*/
                           tt-embarque-corpo.nr-nota-fis   = docum-est.nro-docto
                           tt-embarque-corpo.dt-emissao    = docum-est.dt-emissao
                           tt-embarque-corpo.nr-di         = embarque-imp.declaracao-import
                           tt-embarque-corpo.dt-di         = embarque-imp.data-di
                           tt-embarque-corpo.numero-ordem  = (&if "{&bf_mat_versao_ems}" >= "2.06" &then 
                                                                if docum-est.idi-nf-simples-remes = 1 then
                                                                    item-doc-est.num-ord-import
                                                              &else
                                                                if int(substring(docum-est.char-2,101,1)) = 1 then 
                                                                    int(substring(item-doc-est.char-2,145,8))
                                                              &endif
                                                                else
                                                                    ordem-compra.numero-ordem)
                           tt-embarque-corpo.it-codigo     = item-doc-est.it-codigo
                           /*tt-embarque-corpo.desc-item     = item.desc-item.*/ .
                    assign p-situacao = 2.
                end.
            end.
            
            
            assign /*tot-despesa-orc = 0 */
                   tot-despesa-rea = 0.

            /* => DETALHA ITEM *******************************/
            &if "{&bf_mat_versao_ems}" >= "2.05" &then
                for first cotacao-item fields( numero-ordem  cod-emitente  mo-codigo  aliquota-ii char-1
                                               data-cotacao  cot-aprovada  it-codigo  seq-cotac aliquota-ipi)
                    where cotacao-item.numero-ordem = ordem-compra.numero-ordem
                      and cotacao-item.cod-emitente = ordem-compra.cod-emitente
                      and cotacao-item.data-cotacao = ordem-compra.data-cotacao
                      and cotacao-item.cot-aprovada = yes no-lock:
                end.
            &else
                for first cotacao-item fields( numero-ordem  cod-emitente  mo-codigo  char-1
                                               data-cotacao  cot-aprovada  it-codigo  seq-cotac aliquota-ipi)
                    where cotacao-item.numero-ordem = ordem-compra.numero-ordem
                      and cotacao-item.cod-emitente = ordem-compra.cod-emitente
                      and cotacao-item.data-cotacao = ordem-compra.data-cotacao
                      and cotacao-item.cot-aprovada = yes no-lock:
                end.
            &endif
            if  not avail cotacao-item then next.

            ASSIGN i-parcela = &if "{&bf_mat_versao_ems}" >= "2.06" &then 
                                    if docum-est.idi-nf-simples-remes = 1 then
                                        item-doc-est.num-parc-import
                               &else
                                    if int(substring(docum-est.char-2,101,1)) = 1 then 
                                        int(substring(item-doc-est.char-2,154,5)) 
                               &endif
                                    else item-doc-est.parcela.

            FOR FIRST b-ordens-embarque FIELDS(char-2 numero-ordem) WHERE
                      b-ordens-embarque.cod-estabel  = embarque-imp.cod-estabel  AND
                      b-ordens-embarque.embarque     = embarque-imp.embarque     AND
                      b-ordens-embarque.numero-ordem = (&if "{&bf_mat_versao_ems}" >= "2.06" &then 
                                                            if docum-est.idi-nf-simples-remes = 1 then
                                                                item-doc-est.num-ord-import
                                                        &else
                                                            if int(substring(docum-est.char-2,101,1)) = 1 then 
                                                                int(substring(item-doc-est.char-2,145,8)) 
                                                        &endif
                                                            else item-doc-est.numero-ordem)
                    and b-ordens-embarque.parcela    = i-parcela      NO-LOCK:
            END.
            
            ASSIGN de-preco-unit-sem-ipi = (ordem-compra.preco-unit * 100) / (100 + ordem-compra.aliquota-ipi).
            create tt-detalha-item.
            assign tt-detalha-item.embarque                   = embarque-imp.embarque
                   tt-detalha-item.cod-estabel                = embarque-imp.cod-estabel  
                   tt-detalha-item.embarque-orig              = pc-embarque
                   tt-detalha-item.nro-docto                  = item-doc-est.nro-docto
                   tt-detalha-item.sequencia                  = item-doc-est.sequencia
                   tt-detalha-item.it-codigo                  = ITEM.it-codigo
                   tt-detalha-item.descricao                  = ITEM.desc-item
                   tt-detalha-item.numero-ordem               = ordem-compra.numero-ordem
                   tt-detalha-item.parcela                    = i-parcela
                   tt-detalha-item.quantidade                 = item-doc-est.quantidade
                   tt-detalha-item.un                         = ITEM.un
                   tt-detalha-item.vl-mercado-orc             = de-preco-unit-sem-ipi
                   tt-detalha-item.aliquota-ipi               = item-doc-est.aliquota-ipi
                   tt-detalha-item.aliquota-icms              = item-doc-est.aliquota-icm
                   tt-detalha-item.aliquota-pis               = &IF "{&bf_mat_versao_ems}":U >= "2.06":U &THEN item-doc-est.val-aliq-pis
                                                                &ELSE DEC(SUBSTRING(item-doc-est.char-2,022,05)) &endif
                   tt-detalha-item.aliquota-cofins            = &IF "{&bf_mat_versao_ems}":U >= "2.06":U &THEN item-doc-est.val-aliq-cofins
                                                                &ELSE DEC(SUBSTRING(item-doc-est.char-2,084,05)) &endif                                             
                   tt-detalha-item.situacao                   = p-situacao.

            IF AVAIL b-ordens-embarque THEN
               ASSIGN &if "{&bf_mat_versao_ems}" >= "2.05" &then
                         tt-detalha-item.aliquota-ii      = IF  TRIM(SUBSTRING(b-ordens-embarque.char-2,1,6)) = " " THEN cotacao-item.aliquota-ii
                                                            ELSE DEC(TRIM(SUBSTRING(b-ordens-embarque.char-2,1,6)))
                         tt-detalha-item.aliquota-ii2     = IF  TRIM(SUBSTRING(b-ordens-embarque.char-2,1,6)) = " " THEN cotacao-item.aliquota-ii
                                                            ELSE DEC(TRIM(SUBSTRING(b-ordens-embarque.char-2,1,6))).

                      &else
                          tt-detalha-item.aliquota-ii     = IF  TRIM(SUBSTRING(b-ordens-embarque.char-2,1,6)) = " " THEN DEC(TRIM(SUBSTRING(cotacao-item.char-1,61,20)))
                                                            ELSE DEC(TRIM(SUBSTRING(b-ordens-embarque.char-2,1,6)))
                          tt-detalha-item.aliquota-ii2    = IF  TRIM(SUBSTRING(b-ordens-embarque.char-2,1,6)) = " " THEN DEC(TRIM(SUBSTRING(cotacao-item.char-1,61,20)))
                                                            ELSE DEC(TRIM(SUBSTRING(b-ordens-embarque.char-2,1,6))).

                      &endif
            ELSE
                ASSIGN &if "{&bf_mat_versao_ems}" >= "2.05" &then
                          tt-detalha-item.aliquota-ii     = cotacao-item.aliquota-ii
                          tt-detalha-item.aliquota-ii2    = cotacao-item.aliquota-ii.
                       &else
                           tt-detalha-item.aliquota-ii    = DEC(TRIM(SUBSTRING(cotacao-item.char-1,61,20)))
                           tt-detalha-item.aliquota-ii2   = DEC(TRIM(SUBSTRING(cotacao-item.char-1,61,20))).
                       &endif
            
            if  tt-param.i-custo-item = 2 then /*Total*/
                assign tt-detalha-item.vl-mercado-orc = de-preco-unit-sem-ipi
                                                      * item-doc-est.quantidade.
            ASSIGN de-preco-unit-sem-ipi = 0.
            /*Convers’o*/
            
            if  tt-param.moeda <> cotacao-item.mo-codigo THEN DO:
                run converte-moeda (input-output tt-detalha-item.vl-mercado-orc,
                                    input cotacao-item.mo-codigo).
                
            END.
            
            assign vl-ii-orcad-det  = 0
                   vl-ii2-orcad-det = 0
                   no-despesa       = no
                   var-moeda-conv-orc = tt-param.moeda
                   tt-detalha-item.vl-mercado-orc-total = if tt-param.i-custo-item = 2 then tt-detalha-item.vl-mercado-orc
                                                          else (tt-detalha-item.vl-mercado-orc * item-doc-est.quantidade).

            &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
            ASSIGN vl-ipi-orcad-det    = 0
                   vl-icms-orcad-det   = 0
                   vl-pis-orcad-det    = 0
                   vl-cofins-orcad-det = 0.
            &ENDIF
            
             /****OR°ADO-INI*/
            for each  cotacao-item-cex fields( numero-ordem  cod-emitente  mapa-cotacao
                                               cod-incoterm  it-codigo     seq-cotac
                                               data-cotacao  cod-itiner    cod-desp
                                               val-desp      mo-codigo)
                where cotacao-item-cex.numero-ordem = cotacao-item.numero-ordem
                  and cotacao-item-cex.cod-emitente = cotacao-item.cod-emitente
                  and cotacao-item-cex.it-codigo    = cotacao-item.it-codigo
                  and cotacao-item-cex.seq-cotac    = cotacao-item.seq-cotac
                  and cotacao-item-cex.data-cotacao = cotacao-item.data-cotacao
                  and cotacao-item-cex.mapa-cotacao = int(trim(substring(cotacao-item.char-1,1,20)))
                  and cotacao-item-cex.cod-incoterm = embarque-imp.cod-incoterm
                  and cotacao-item-cex.cod-itiner   = itinerario.cod-itiner no-lock
                use-index codigo:

                assign var-moeda-conv-orc = cotacao-item-cex.mo-codigo.

                /* II e II2 - ORCADO */
                if  cotacao-item-cex.cod-desp = i-cod-desp-ii and
                    tt-param.i-desp-imposto = 1 then /*Despesa*/
                    assign vl-ii2-orcad-det = cotacao-item-cex.val-desp.

                if  cotacao-item-cex.cod-desp = i-cod-desp-ii and
                    tt-param.i-desp-imposto = 2 then /*Imposto*/ 
                    assign vl-ii-orcad-det  = cotacao-item-cex.val-desp.

                &IF "{&BF_MAT_VERSAO_EMS}" >= "2.062" &THEN
                    IF  AVAIL param-imp THEN DO:
                        IF  param-imp.cdn-despes-impto-ipi = cotacao-item-cex.cod-desp THEN DO:
                            ASSIGN vl-ipi-orcad-det = cotacao-item-cex.val-desp
                                   no-despesa       = YES.
                        END.

                        IF  param-imp.cdn-despes-impto-icms = cotacao-item-cex.cod-desp THEN DO:
                            ASSIGN vl-icms-orcad-det = cotacao-item-cex.val-desp
                                   no-despesa        = YES.
                        END.
                        IF  param-imp.cdn-despes-impto-pis = cotacao-item-cex.cod-desp THEN DO:
                            ASSIGN vl-pis-orcad-det = cotacao-item-cex.val-desp
                                   no-despesa        = YES.
                        END.
                        IF  param-imp.cdn-despes-impto-cofins = cotacao-item-cex.cod-desp THEN DO:
                            ASSIGN vl-cofins-orcad-det = cotacao-item-cex.val-desp
                                   no-despesa        = YES.
                        END.
                    END.                                   

                &ELSE
                    /* IPI - ORCADO */
                    for first desp-imp fields( cod-desp  descricao )
                        where desp-imp.cod-desp = cotacao-item-cex.cod-desp
                          and substring(desp-imp.descricao,1,3) = "IPI":U no-lock:
                    end.
                    if  avail desp-imp then
                        assign vl-ipi-orcad-det = cotacao-item-cex.val-desp
                               no-despesa       = YES.
                    else 
                        assign vl-ipi-orcad-det = 0.
                    
                    /* ICMS - ORCADO */
                    for first desp-imp fields( cod-desp  descricao )
                        where desp-imp.cod-desp = cotacao-item-cex.cod-desp
                          and substring(desp-imp.descricao,1,4) = "ICMS":U no-lock:
                    end.
                    if  avail desp-imp then
                        assign vl-icms-orcad-det = cotacao-item-cex.val-desp
                               no-despesa        = YES.
                    else 
                        assign vl-icms-orcad-det = 0.

                    /* PIS - ORCADO */
                    for first desp-imp fields( cod-desp  descricao )
                        where desp-imp.cod-desp = cotacao-item-cex.cod-desp
                          and substring(desp-imp.descricao,1,3) = "PIS":U no-lock:
                    end.
                    if  avail desp-imp THEN
                        assign vl-pis-orcad-det = cotacao-item-cex.val-desp
                               no-despesa        = YES.
                        
                    for first desp-imp fields( cod-desp  descricao )
                        where desp-imp.cod-desp = cotacao-item-cex.cod-desp
                          and substring(desp-imp.descricao,1,6) = "COFINS":U no-lock:
                    end.
                    if  avail desp-imp then
                        assign vl-cofins-orcad-det = cotacao-item-cex.val-desp
                               no-despesa        = YES.
                &ENDIF
                
                /* Despesas - ORCADO */
                for first desp-imp fields ( cod-desp  tipo  descricao )
                    where desp-imp.cod-desp = cotacao-item-cex.cod-desp no-lock:
                end.
                
                if  not avail desp-imp then 
                    next.

                if  tt-param.i-custo-item = 2 and desp-imp.tipo <> 1 then /*Total e Diferente de Fixo*/
                    assign vl-desp-orcad-det = cotacao-item-cex.val-desp
                                             * item-doc-est.quantidade.
                else
                    assign vl-desp-orcad-det = cotacao-item-cex.val-desp.



                if desp-imp.tipo <> 1 then /*Total e Diferente de Fixo*/
                   ASSIGN vl-desp-orcad-aux = cotacao-item-cex.val-desp * 
                                              item-doc-est.quantidade.
                ELSE
                    ASSIGN vl-desp-orcad-aux = cotacao-item-cex.val-desp.

                IF  desp-imp.cod-desp <> i-cod-desp-ii 
                &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-ipi                     
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-icms                    
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-pis
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-cofins
                &ENDIF
                AND NOT no-despesa THEN DO:
                    find FIRST tt-detalha-despesa
                         WHERE tt-detalha-despesa.embarque    = embarque-imp.embarque
                           AND tt-detalha-despesa.cod-estabel = embarque-imp.cod-estabel
                           AND tt-detalha-despesa.cod-despesa = desp-imp.cod-desp no-lock no-error.
                    if not avail tt-detalha-despesa then do:
                        create tt-detalha-despesa.
                        assign tt-detalha-despesa.embarque      = embarque-imp.embarque
                               tt-detalha-despesa.cod-estabel   = embarque-imp.cod-estabel
                               tt-detalha-despesa.numero-ordem  = cotacao-item-cex.numero-ordem
                               tt-detalha-despesa.nro-docto     = item-doc-est.nro-docto
                               tt-detalha-despesa.cod-despesa   = desp-imp.cod-desp
                               tt-detalha-despesa.desc-despesa  = desp-imp.descricao
                               tt-detalha-despesa.situacao      = p-situacao
                               tt-detalha-despesa.vl-des-orc    = vl-desp-orcad-det
                               tot-despesa-orc                  = tot-despesa-orc
                                                                + vl-desp-orcad-det
                               tt-detalha-despesa.it-codigo     = cotacao-item.it-codigo
                               tt-detalha-despesa.sequencia     = item-doc-est.sequencia. 
    
                        if  tt-param.moeda <> var-moeda-conv-orc THEN
                            IF AVAIL tt-detalha-despesa THEN
                                run converte-moeda (input-output tt-detalha-despesa.vl-des-orc,
                                                    input var-moeda-conv-orc).
                            IF desp-imp.tipo = 1 AND tt-param.i-custo-item = 1 THEN
                                ASSIGN tt-detalha-despesa.vl-des-orc = tt-detalha-despesa.vl-des-orc / item-doc-est.quantidade.

                        ASSIGN de-tot-des-orc-aux = de-tot-des-orc-aux + tt-detalha-despesa.vl-des-orc.
                    END. 
                    ELSE DO:
                        ASSIGN de-tot-des-orc-aux = de-tot-des-orc-aux + tt-detalha-despesa.vl-des-orc. 
                    END.
                END.
                
                IF  desp-imp.cod-desp <> i-cod-desp-ii 
                &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-ipi 
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-icms 
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-pis
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-cofins
                &ENDIF
                THEN DO:
                    
                    IF tt-param.moeda <> var-moeda-conv-orc THEN DO:
                        RUN converte-moeda (INPUT-OUTPUT vl-desp-orcad-aux,
                                            INPUT var-moeda-conv-orc).
                    END.
                    for first tt-detalha-despesa-aux
                         where tt-detalha-despesa-aux.embarque     = embarque-imp.embarque
                           and tt-detalha-despesa-aux.cod-estabel  = embarque-imp.cod-estabel
                           and tt-detalha-despesa-aux.cod-despesa  = desp-imp.cod-desp 
                           AND tt-detalha-despesa-aux.sequencia    = item-doc-est.sequencia no-lock /**/
                        use-index ch-codigo:
                    end.
                    if not avail tt-detalha-despesa-aux then do:
                        CREATE tt-detalha-despesa-aux.
                        ASSIGN tt-detalha-despesa-aux.embarque     = embarque-imp.embarque
                               tt-detalha-despesa-aux.cod-estabel  = embarque-imp.cod-estabel
                               tt-detalha-despesa-aux.cod-despesa  = desp-imp.cod-desp
                               tt-detalha-despesa-aux.desc-despesa = desp-imp.descricao
                               tt-detalha-despesa-aux.vl-des-orc   = vl-desp-orcad-aux
                               tt-detalha-despesa-aux.sequencia    = item-doc-est.sequencia.
                                                
                        /*acumula valor total das despesas or»adas convertidas ou nao*/
                        ASSIGN de-tot-desp-aux = de-tot-desp-aux + tt-detalha-despesa-aux.vl-des-orc.
                    END.
                    ELSE DO: /*Soma as despesas dos Desembarques parciais*/
                        ASSIGN tt-detalha-despesa-aux.vl-des-orc = tt-detalha-despesa-aux.vl-des-orc + vl-desp-orcad-aux.
                        ASSIGN de-tot-desp-aux = de-tot-desp-aux + tt-detalha-despesa-aux.vl-des-orc.
                    END.
                END.
                /*Total do Embarque*/
                if  tt-param.i-custo-item = 1 then do:
                    if  desp-imp.tipo = 1 then
                        assign tt-detalha-item.vl-des-orc-total     = tt-detalha-item.vl-des-orc-total
                                                                    + vl-desp-orcad-det.
                    else do:
                        if  not no-despesa then
                            assign tt-detalha-item.vl-des-orc-total = tt-detalha-item.vl-des-orc-total
                                                                    + (vl-desp-orcad-det * item-doc-est.quantidade).
                    end.
                end.
                else 
                    assign tt-detalha-item.vl-des-orc-total = tot-despesa-orc.

                /***********************/

                assign tt-detalha-item.vl-tot-des-orc-total = tt-detalha-item.vl-des-orc-total.
                
                IF tt-param.i-desp-imposto = 1 THEN
                    ASSIGN vl-ii2-orcad-det-aux = vl-ii2-orcad-det * tt-detalha-item.quantidade.
                ELSE
                    ASSIGN vl-ii-orcad-det-aux  = vl-ii-orcad-det * tt-detalha-item.quantidade.

            end. /*cotacao-item-cex*/

            assign tt-detalha-item.vl-ii-orc                  = IF tt-param.i-custo-item = 2 THEN vl-ii-orcad-det * tt-detalha-item.quantidade
                                                                ELSE vl-ii-orcad-det
                   &IF  "{&bf_mat_versao_ems}" < "2.062" &THEN
                   tt-detalha-item.vl-ipi-orc                 = vl-ipi-orcad-det
                   tt-detalha-item.vl-icms-orc                = vl-icms-orcad-det
                   tt-detalha-item.vl-pis-orc                 = vl-pis-orcad-det
                   tt-detalha-item.vl-cofins-orc              = vl-cofins-orcad-det
                   tt-detalha-item.vl-tot-imp-orc             = tt-detalha-item.vl-ii-orc /*Total Imposto  II + IPI + ICMS*/  
                                                              + vl-ipi-orcad-det
                                                              + vl-icms-orcad-det
                                                              + vl-pis-orcad-det
                                                              + vl-cofins-orcad-det
                   &ELSE
                   tt-detalha-item.vl-ipi-orc                 = IF tt-param.i-custo-item = 2 THEN vl-ipi-orcad-det * tt-detalha-item.quantidade
                                                                ELSE vl-ipi-orcad-det
                   tt-detalha-item.vl-icms-orc                = IF tt-param.i-custo-item = 2 THEN vl-icms-orcad-det * tt-detalha-item.quantidade
                                                                ELSE vl-icms-orcad-det
                   tt-detalha-item.vl-pis-orc                = IF tt-param.i-custo-item = 2 THEN vl-pis-orcad-det * tt-detalha-item.quantidade
                                                                ELSE vl-pis-orcad-det
                   tt-detalha-item.vl-cofins-orc             = IF tt-param.i-custo-item = 2 THEN vl-cofins-orcad-det * tt-detalha-item.quantidade
                                                                ELSE vl-cofins-orcad-det
                   tt-detalha-item.vl-tot-imp-orc             = tt-detalha-item.vl-ii-orc /*Total Imposto  II + IPI + ICMS*/  
                                                                + tt-detalha-item.vl-ipi-orc
                                                                + tt-detalha-item.vl-icms-orc
                                                                + tt-detalha-item.vl-pis-orc
                                                                + tt-detalha-item.vl-cofins-orc
                   &ENDIF
                   tt-detalha-item.vl-ii2-orc                 = IF tt-param.i-custo-item = 2 THEN vl-ii2-orcad-det * tt-detalha-item.quantidade 
                                                                ELSE vl-ii2-orcad-det /*II2*/
                   tt-detalha-item.vl-ii2-orc-aux             = vl-ii2-orcad-det-aux
                   tt-detalha-item.vl-ii-orc-aux              = vl-ii-orcad-det-aux
                   tt-detalha-item.vl-des-orc                 = de-tot-des-orc-aux /*tot-despesa-orc*/ /*Despesas*/
                   
                   tt-detalha-item.vl-tot-com-ipi-icms-orc    = tt-detalha-item.vl-mercado-orc  /*Valor Mercado + (Total Imposto + Total Despesa)*/
                                                              + tt-detalha-item.vl-tot-imp-des-orc
                   tt-detalha-item.vl-tot-sem-ipi-icms-orc    = tt-detalha-item.vl-tot-com-ipi-icms-orc /*Valor Mercado + Imposto + Despesa - IPI - ICMS*/
                                                              - vl-ipi-orcad-det
                                                              - vl-icms-orcad-det
                                                              - vl-pis-orcad-det
                                                              - vl-cofins-orcad-det
                   tt-detalha-item.vl-tot-imp-des-orc-total   = tt-detalha-item.vl-tot-imp-orc
                                                              + tt-detalha-item.vl-tot-des-orc-total.


            /****OR°ADO-FIM*/
            assign vl-ii-reali-det  = 0
                   vl-ii2-reali-det = 0
                   de-val-desp-aux  = 0
                   vl-pis-orcad-det = 0
                   vl-cofins-orcad-det = 0. 
            /****REALIZADO-INI*/

            for each  item-doc-est-cex fields( serie-docto  nro-docto  cod-emitente  sequencia
                                               nat-operacao  cod-desp  val-desp  mo-codigo )
                where item-doc-est-cex.serie-docto  = item-doc-est.serie-docto
                  and item-doc-est-cex.nro-docto    = item-doc-est.nro-docto
                  and item-doc-est-cex.cod-emitente = item-doc-est.cod-emitente
                  and item-doc-est-cex.nat-operacao = item-doc-est.nat-operacao
                  and item-doc-est-cex.sequencia    = item-doc-est.sequencia no-lock
                use-index codigo:
           
                /* II e II2 - REALIZADO */
                if  item-doc-est-cex.cod-desp = i-cod-desp-ii and
                    tt-param.i-desp-imposto = 1 then /*Despesa*/
                    assign vl-ii2-reali-det  = item-doc-est-cex.val-desp.
                if  item-doc-est-cex.cod-desp = i-cod-desp-ii and
                    tt-param.i-desp-imposto = 2 then /*Imposto*/
                    assign vl-ii-reali-det = item-doc-est-cex.val-desp.

                for first desp-imp fields( cod-desp  inc-val-frete  inc-val-embal  tipo
                                           inc-val-seguro  inc-val-outras-desp  descricao )
                    where desp-imp.cod-desp = item-doc-est-cex.cod-desp no-lock:
                end.
                if  not avail desp-imp then next.

                &IF "{&BF_MAT_VERSAO_EMS}" >= "2.062" &THEN
                    IF  param-imp.cdn-despes-impto-ipi    = item-doc-est-cex.cod-desp 
                    OR  param-imp.cdn-despes-impto-icms   = item-doc-est-cex.cod-desp 
                    OR  param-imp.cdn-despes-impto-pis    = item-doc-est-cex.cod-desp 
                    OR  param-imp.cdn-despes-impto-cofins = item-doc-est-cex.cod-desp     THEN
                        NEXT.
                &ELSE
                    IF  SUBSTRING(desp-imp.descricao,1,3) = "IPI"
                    OR  SUBSTRING(desp-imp.descricao,1,4) = "ICMS"
                    OR  SUBSTRING(desp-imp.descricao,1,3) = "PIS"    
                    OR  SUBSTRING(desp-imp.descricao,1,6) = "COFINS" THEN 
                        NEXT.
                &ENDIF
    
                /* Outras Despesas */
                if  desp-imp.inc-val-frete then
                    assign vl-despesa-frete = item-doc-est-cex.val-desp.
                if  desp-imp.inc-val-embal then
                    assign vl-despesa-embalagem = item-doc-est-cex.val-desp.
                if  desp-imp.inc-val-seguro then
                    assign vl-despesa-seguro = item-doc-est-cex.val-desp.
                if  desp-imp.inc-val-outras-desp then
                    assign vl-despesa-outras  = item-doc-est-cex.val-desp.

                if  desp-imp.cod-desp <> i-cod-desp-ii 
                &IF "{&BF_MAT_VERSAO_EMS}" >= "2.062" &THEN
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-ipi                     
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-icms 
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-pis
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-cofins 
                &ENDIF
                then do:
                    find first tt-detalha-despesa
                         where tt-detalha-despesa.embarque     = embarque-imp.embarque
                           and tt-detalha-despesa.cod-estabel  = embarque-imp.cod-estabel
                           and tt-detalha-despesa.numero-ordem = ordem-compra.numero-ordem
                           and tt-detalha-despesa.cod-despesa  = desp-imp.cod-desp no-lock no-error.
                    if not avail tt-detalha-despesa then do:
                        create tt-detalha-despesa.
                        assign tt-detalha-despesa.embarque     = embarque-imp.embarque
                               tt-detalha-despesa.cod-estabel  = embarque-imp.cod-estabel
                               tt-detalha-despesa.numero-ordem = ordem-compra.numero-ordem
                               tt-detalha-despesa.nro-docto    = item-doc-est.nro-docto
                               tt-detalha-despesa.cod-despesa  = desp-imp.cod-desp
                               tt-detalha-despesa.desc-despesa = desp-imp.descricao
                               tt-detalha-despesa.situacao     = p-situacao 
                               tt-detalha-despesa.it-codigo    = item-doc-est.it-codigo
                               tt-detalha-despesa.sequencia    = item-doc-est.sequencia.

                        if  tt-param.i-custo-item = 1 /*Unitario*/ THEN DO:
                            assign tt-detalha-despesa.vl-des-rea = item-doc-est-cex.val-desp / item-doc-est.quantidade
                                   tot-despesa-rea               = tot-despesa-rea + 
                                                                   (item-doc-est-cex.val-desp / item-doc-est.quantidade).
    
                        END.
                        ELSE 
                            assign tt-detalha-despesa.sequencia   = item-doc-est.sequencia
                                   tt-detalha-despesa.vl-des-rea  = item-doc-est-cex.val-desp 
                                   tot-despesa-rea                = tot-despesa-rea
                                                                   + item-doc-est-cex.val-desp.

                    end.

                    /*Moeda Converte Realizado*/
                    assign var-moeda-conv-rea = item-doc-est-cex.mo-codigo.
                    if  tt-param.moeda <> var-moeda-conv-rea THEN
                        run converte-moeda (input-output tt-detalha-despesa.vl-des-rea,
                                            input var-moeda-conv-rea).
                end.

                if not avail tt-detalha-despesa then do:
                    find first tt-detalha-despesa
                         where tt-detalha-despesa.embarque     = embarque-imp.embarque
                           and tt-detalha-despesa.cod-estabel  = embarque-imp.cod-estabel
                           and tt-detalha-despesa.numero-ordem = ordem-compra.numero-ordem
                           and tt-detalha-despesa.nro-docto    = item-doc-est.nro-docto
                           and tt-detalha-despesa.cod-despesa  = desp-imp.cod-desp no-lock no-error.
        
                    if  not avail tt-detalha-despesa then do:
                        create tt-detalha-despesa.
                        assign tt-detalha-despesa.embarque     = embarque-imp.embarque
                               tt-detalha-despesa.cod-estabel  = embarque-imp.cod-estabel
                               tt-detalha-despesa.numero-ordem = ordem-compra.numero-ordem
                               tt-detalha-despesa.nro-docto    = item-doc-est.nro-docto
                               tt-detalha-despesa.cod-despesa  = desp-imp.cod-desp
                               tt-detalha-despesa.desc-despesa = desp-imp.descricao
                               tt-detalha-despesa.situacao     = p-situacao.
                               tt-detalha-despesa.sequencia    = item-doc-est.sequencia.
                    end.
                end.
                
                assign var-moeda-conv-rea = item-doc-est-cex.mo-codigo.

                /*Total do Embarque*/
                if  desp-imp.cod-desp <> i-cod-desp-ii 
                &IF "{&BF_MAT_VERSAO_EMS}" >= "2.062" &THEN
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-ipi                 
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-icms 
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-pis 
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-cofins 
                &ENDIF    
                then 
                    assign tt-detalha-item.vl-des-rea-total = tt-detalha-item.vl-des-rea-total
                                                            + item-doc-est-cex.val-desp.

                /*Sem NF complementar - Realizado*/
                IF  desp-imp.cod-desp <> i-cod-desp-ii 
                &IF "{&BF_MAT_VERSAO_EMS}" >= "2.062" &THEN
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-ipi                     
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-icms 
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-pis
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-cofins 
                &ENDIF
                THEN DO:
                    ASSIGN de-val-desp-aux = item-doc-est-cex.val-desp.
                    IF tt-param.moeda <> var-moeda-conv-rea THEN DO:
                        RUN converte-moeda (INPUT-OUTPUT de-val-desp-aux,
                                            INPUT var-moeda-conv-rea).
                    END.

                    for first tt-detalha-despesa-aux
                         where tt-detalha-despesa-aux.embarque     = embarque-imp.embarque
                           and tt-detalha-despesa-aux.cod-estabel  = embarque-imp.cod-estabel
                           and tt-detalha-despesa-aux.cod-despesa  = item-doc-est-cex.cod-desp no-lock
                        use-index ch-codigo:
                    end.
                    if not avail tt-detalha-despesa-aux then do:
                        CREATE tt-detalha-despesa-aux.
                        ASSIGN tt-detalha-despesa-aux.embarque     = embarque-imp.embarque
                               tt-detalha-despesa-aux.cod-estabel  = embarque-imp.cod-estabel
                               tt-detalha-despesa-aux.cod-despesa  = desp-imp.cod-desp
                               tt-detalha-despesa-aux.desc-despesa = desp-imp.descricao
                               tt-detalha-despesa-aux.vl-des-rea   = de-val-desp-aux
                               tt-detalha-despesa-aux.sequencia    = item-doc-est-cex.sequencia.
                    END.
                    ELSE DO:
                        ASSIGN tt-detalha-despesa-aux.vl-des-rea = tt-detalha-despesa-aux.vl-des-rea + de-val-desp-aux.
                    END.
                    ASSIGN de-val-desp-aux-rea = de-val-desp-aux-rea + de-val-desp-aux. 
                END.
            end.
            
            assign var-preco-item-rea    = item-doc-est.preco-unit[1] * item-doc-est.qt-do-forn
                   var-valor-ipi-rea     = item-doc-est.valor-ipi[1]
                   var-valor-icm-rea     = item-doc-est.valor-icm[1]
                   var-valor-pis-rea     = &IF "{&BF_MAT_VERSAO_EMS}" >= "2.062" &THEN 
                                               item-doc-est.valor-pis 
                                           &ELSE
                                               DEC(SUBSTRING(item-doc-est.CHAR-2,41,14)) &ENDIF
                   var-valor-cofins-rea  = &IF "{&BF_MAT_VERSAO_EMS}" >= "2.062" &THEN
                                               item-doc-est.val-cofins 
                                           &ELSE
                                               DEC(SUBSTRING(item-doc-est.CHAR-2,103,14)) &ENDIF.

            if  tt-param.i-custo-item = 1 then do: /*Unitario*/


                assign vl-ii2-reali-det     = vl-ii2-reali-det     / item-doc-est.quantidade
                       vl-ii-reali-det      = vl-ii-reali-det      / item-doc-est.quantidade
                       var-valor-ipi-rea    = var-valor-ipi-rea    / item-doc-est.quantidade
                       var-valor-icm-rea    = var-valor-icm-rea    / item-doc-est.quantidade
                       var-valor-pis-rea    = var-valor-pis-rea    / item-doc-est.quantidade
                       var-valor-cofins-rea = var-valor-cofins-rea / item-doc-est.quantidade
                       var-preco-item-rea   = var-preco-item-rea   / item-doc-est.quantidade.
            END.
            
            /*tt-detalha-item REALIZADO - NOTA PRINCIPAL*/
            assign tt-detalha-item.vl-mercado-rea           = var-preco-item-rea /*Valor Merado Realizado*/
                   tt-detalha-item.vl-ii-rea                = vl-ii-reali-det
                   tt-detalha-item.vl-ipi-rea               = round(var-valor-ipi-rea,2)
                   tt-detalha-item.vl-icms-rea              = round(var-valor-icm-rea,2)
                   tt-detalha-item.vl-pis-rea               = round(var-valor-pis-rea ,2)
                   tt-detalha-item.vl-cofins-rea            = round(var-valor-cofins-rea,2)
                   tt-detalha-item.vl-tot-imp-rea           = vl-ii-reali-det /*Total Imposto  II + IPI + ICMS*/
                                                            + var-valor-ipi-rea
                                                            + var-valor-icm-rea
                                                            + var-valor-pis-rea
                                                            + var-valor-cofins-rea
                   tt-detalha-item.vl-ii2-rea               = vl-ii2-reali-det
                   tt-detalha-item.vl-des-rea               = tot-despesa-rea
                   tt-detalha-item.vl-tot-des-rea           = vl-ii2-reali-det
                                                            + tot-despesa-rea
                   tt-detalha-item.vl-tot-imp-des-rea       = tt-detalha-item.vl-tot-imp-rea
                                                            + tt-detalha-item.vl-tot-des-rea    
                   tt-detalha-item.vl-tot-com-ipi-icms-rea  = tt-detalha-item.vl-mercado-rea
                                                            + tt-detalha-item.vl-tot-imp-des-rea
                   tt-detalha-item.vl-tot-sem-ipi-icms-rea  = tt-detalha-item.vl-tot-com-ipi-icms-rea
                                                            - var-valor-ipi-rea
                                                            - var-valor-icm-rea
                                                            - var-valor-pis-rea    
                                                            - var-valor-cofins-rea
                   tt-detalha-item.vl-mercado-rea-total     = if tt-param.i-custo-item = 2 then var-preco-item-rea
                                                              else var-preco-item-rea * item-doc-est.quantidade
                   tt-detalha-item.vl-ii-rea-total          = if tt-param.i-custo-item = 2 then vl-ii-reali-det
                                                              else vl-ii-reali-det * item-doc-est.quantidade
                   tt-detalha-item.vl-ii2-rea-total         = if tt-param.i-custo-item = 2 then vl-ii2-reali-det
                                                              else vl-ii2-reali-det * item-doc-est.quantidade
                   tt-detalha-item.vl-ipi-rea-total         = if tt-param.i-custo-item = 2 then var-valor-ipi-rea
                                                              else var-valor-ipi-rea * item-doc-est.quantidade
                   tt-detalha-item.vl-icms-rea-total        = if tt-param.i-custo-item = 2 then var-valor-icm-rea
                                                              else var-valor-icm-rea * item-doc-est.quantidade
                   tt-detalha-item.vl-pis-rea-total         = if tt-param.i-custo-item = 2 then var-valor-pis-rea
                                                              else var-valor-pis-rea * item-doc-est.quantidade
                   tt-detalha-item.vl-cofins-rea-total      = if tt-param.i-custo-item = 2 then var-valor-cofins-rea
                                                              else var-valor-cofins-rea * item-doc-est.quantidade
                   tt-detalha-item.vl-tot-imp-rea-total     = if tt-param.i-custo-item = 2 then tt-detalha-item.vl-tot-imp-rea
                                                              else tt-detalha-item.vl-tot-imp-rea * item-doc-est.quantidade
                   tt-detalha-item.vl-tot-des-rea-total     = if tt-param.i-custo-item = 1 then tt-detalha-item.vl-des-rea-total + tt-detalha-item.vl-ii2-rea-total
                                                              else tt-detalha-item.vl-tot-des-rea
                   tt-detalha-item.vl-tot-imp-des-rea-total = if tt-param.i-custo-item = 1 then tt-detalha-item.vl-tot-imp-rea-total + tt-detalha-item.vl-tot-des-rea-total
                                                              else tt-detalha-item.vl-tot-imp-des-rea
                   tt-detalha-item.vl-tot-com-ipi-icms-rea-total = tt-detalha-item.vl-mercado-rea-total
                                                                 + tt-detalha-item.vl-tot-imp-des-rea-total 
                   tt-detalha-item.vl-tot-sem-ipi-icms-rea-total = tt-detalha-item.vl-tot-com-ipi-icms-rea-total
                                                                 - tt-detalha-item.vl-ipi-rea-total
                                                                 - tt-detalha-item.vl-icms-rea-total
                                                                 - tt-detalha-item.vl-pis-rea-total
                                                                 - tt-detalha-item.vl-cofins-rea-total.

            /****REALIZADO-FIM*/
            
            /*Convers’o Or»ado*/
            
            IF  tt-param.moeda <> var-moeda-conv-orc THEN DO:
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-ii-orc,
                                    input var-moeda-conv-orc).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-ipi-orc,
                                    input var-moeda-conv-orc).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-icms-orc,
                                    input var-moeda-conv-orc).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-pis-orc,
                                    input var-moeda-conv-orc).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-cofins-orc,
                                    input var-moeda-conv-orc).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-imp-orc,
                                    input var-moeda-conv-orc).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-ii2-orc,
                                    input var-moeda-conv-orc).
                run converte-moeda (input-output tt-detalha-item.vl-des-orc,
                                    input var-moeda-conv-orc).
                run converte-moeda (input-output tt-detalha-item.vl-des-orc-total,
                                    input var-moeda-conv-orc).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-des-orc-total,
                                    input var-moeda-conv-orc).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-imp-des-orc-total,
                                    input var-moeda-conv-orc).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-com-ipi-icms-orc,
                                    input var-moeda-conv-orc).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-sem-ipi-icms-orc-total,
                                    input var-moeda-conv-orc).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-ii2-orc-aux,
                                    input var-moeda-conv-orc).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-ii-orc-aux,
                                    input var-moeda-conv-orc).
            end.

            /*Cÿlculo do valor total do or»ado ap½s valores convertidos*/
            ASSIGN tt-detalha-item.vl-tot-des-orc                = tt-detalha-item.vl-ii2-orc /*Total Despesa II2 + Despesa*/ 
                                                                 + de-tot-des-orc-aux /*tot-despesa-orc*/
                   tt-detalha-item.vl-tot-imp-des-orc            = tt-detalha-item.vl-tot-imp-orc /*Total Imposto + Depesa*/
                                                                 + tt-detalha-item.vl-tot-des-orc.
            ASSIGN tt-detalha-item.vl-tot-com-ipi-icms-orc-total = tt-detalha-item.vl-tot-imp-orc +
                                                                   tt-detalha-item.vl-mercado-orc +
                                                                   tt-detalha-item.vl-tot-des-orc.

            ASSIGN tt-detalha-item.vl-tot-sem-ipi-icms-orc-total = tt-detalha-item.vl-tot-com-ipi-icms-orc-total -
                                                                   tt-detalha-item.vl-tot-imp-orc.

            /* Convers’o Realizado */
            if  tt-param.moeda <> var-moeda-conv-rea then do:
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-mercado-rea,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-mercado-rea-total,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-ii-rea,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-ii-rea-total,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-ipi-rea,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-ipi-rea-total,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-icms-rea,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-icms-rea-total,
                                    input var-moeda-conv-rea).
                ASSIGN de-vl-pis-rea = tt-detalha-item.vl-pis-rea.
                RUN converte-moeda (INPUT-OUTPUT de-vl-pis-rea,
                                    input var-moeda-conv-rea).
                ASSIGN de-vl-pis-rea-total = tt-detalha-item.vl-pis-rea-total.
                RUN converte-moeda (INPUT-OUTPUT de-vl-pis-rea-total,
                                    input var-moeda-conv-rea).
                ASSIGN de-vl-cofins-rea = tt-detalha-item.vl-cofins-rea.
                RUN converte-moeda (INPUT-OUTPUT de-vl-cofins-rea,
                                    input var-moeda-conv-rea).
                ASSIGN de-vl-cofins-rea-total = tt-detalha-item.vl-cofins-rea-total.
                RUN converte-moeda (INPUT-OUTPUT de-vl-cofins-rea-total,
                                    input var-moeda-conv-rea).
                ASSIGN tt-detalha-item.vl-pis-rea-total    = de-vl-pis-rea-total
                       tt-detalha-item.vl-pis-rea          = de-vl-pis-rea
                       tt-detalha-item.vl-cofins-rea-total = de-vl-cofins-rea-total
                       tt-detalha-item.vl-cofins-rea       = de-vl-cofins-rea.

                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-imp-rea,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-imp-rea-total,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-ii2-rea,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-ii2-rea-total,
                                    input var-moeda-conv-rea).
                run converte-moeda (input-output tt-detalha-item.vl-des-rea,
                                    input var-moeda-conv-rea).
                run converte-moeda (input-output tt-detalha-item.vl-des-rea-total,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-des-rea,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-des-rea-total,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-imp-des-rea,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-imp-des-rea-total,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-com-ipi-icms-rea,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-com-ipi-icms-rea-total,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-sem-ipi-icms-rea,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-sem-ipi-icms-rea-total,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT vl-despesa-frete,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT vl-despesa-embalagem,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT vl-despesa-seguro,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT vl-despesa-outras,
                                    input var-moeda-conv-rea).
            END.
            
            /* PERCENTUAL */
            assign tt-detalha-item.per-mercado-orc            = (tt-detalha-item.vl-mercado-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-mercado-rea            = (tt-detalha-item.vl-mercado-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                   tt-detalha-item.per-ii-orc                 = (tt-detalha-item.vl-ii-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-ii-rea                 = (tt-detalha-item.vl-ii-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                   tt-detalha-item.per-ipi-orc                = (tt-detalha-item.vl-ipi-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-icms-orc               = (tt-detalha-item.vl-icms-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                   tt-detalha-item.per-pis-orc                = (tt-detalha-item.vl-pis-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                   tt-detalha-item.per-cofins-orc             = (tt-detalha-item.vl-cofins-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                   tt-detalha-item.per-tot-imp-orc            = (tt-detalha-item.vl-tot-imp-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-tot-imp-rea            = (tt-detalha-item.vl-tot-imp-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                   tt-detalha-item.per-ii2-orc                = (tt-detalha-item.vl-ii2-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-ii2-rea                = (tt-detalha-item.vl-ii2-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                   tt-detalha-item.per-tot-des-orc            = (tt-detalha-item.vl-tot-des-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-tot-des-rea            = (tt-detalha-item.vl-tot-des-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                   tt-detalha-item.per-tot-imp-des-orc        = (tt-detalha-item.vl-tot-imp-des-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-tot-imp-des-rea        = (tt-detalha-item.vl-tot-imp-des-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                   tt-detalha-item.per-tot-com-ipi-icms-orc   = (tt-detalha-item.vl-tot-com-ipi-icms-orc-total
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-tot-com-ipi-icms-rea   = (tt-detalha-item.vl-tot-com-ipi-icms-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                   tt-detalha-item.per-tot-sem-ipi-icms-orc   = (tt-detalha-item.vl-tot-sem-ipi-icms-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-tot-sem-ipi-icms-rea   = (tt-detalha-item.vl-tot-sem-ipi-icms-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100

            /* DESPESAS */
                   tt-detalha-item.vl-des-frete               = vl-despesa-frete
                   tt-detalha-item.vl-des-embalagem           = vl-despesa-embalagem
                   tt-detalha-item.vl-des-seguro              = vl-despesa-seguro
                   tt-detalha-item.vl-des-outras              = vl-despesa-outras
                   tt-detalha-item.vl-tot-despesa             = vl-despesa-frete
                                                              + vl-despesa-embalagem
                                                              + vl-despesa-seguro
                                                              + vl-despesa-outras.
        END.
    END.

END PROCEDURE.

procedure busca-nf-complementar:
    define input parameter p-serie        like docum-est.serie-docto    no-undo.
    define input parameter p-nro-docto    like docum-est.nro-docto      no-undo. 
    define input parameter p-cod-emitente like docum-est.cod-emitente   no-undo. 
    define input parameter p-nat-operacao like docum-est.nat-operacao   no-undo. 
    define input parameter p-embarque     like embarque-imp.embarque    no-undo. 
    define input parameter p-cod-estabel  like embarque-imp.cod-estabel no-undo. 

    for each rat-docum no-lock
       where rat-docum.nf-serie    = p-serie
         and rat-docum.nf-nro      = p-nro-docto
         and rat-docum.nf-emitente = p-cod-emitente
         and rat-docum.nf-nat-oper = p-nat-operacao
        use-index nf-docto:
        
        for each b-docum-est-aux fields( nro-docto  serie-docto  dt-emissao
                                         cod-emitente  nat-operacao cod-estabel)
            where b-docum-est-aux.serie-docto  = rat-docum.serie-docto
              and b-docum-est-aux.nro-docto    = rat-docum.nro-docto
              and b-docum-est-aux.cod-emitente = rat-docum.cod-emitente
              and b-docum-est-aux.nat-operacao = rat-docum.nat-operacao no-lock
            use-index documento:

            FOR FIRST b-item-doc-est WHERE
                      b-item-doc-est.serie-docto  = rat-docum.nf-serie    AND
                      b-item-doc-est.nro-docto    = rat-docum.nf-nro      AND
                      b-item-doc-est.cod-emitente = rat-docum.nf-emitente AND
                      b-item-doc-est.nat-operacao = rat-docum.nf-nat-oper NO-LOCK:
            END.

            create tt-nf-comp.
            assign tt-nf-comp.embarque      = p-embarque
                   tt-nf-comp.cod-estabel   = p-cod-estabel
                   tt-nf-comp.nr-nf-comp    = b-docum-est-aux.nro-docto
                   tt-nf-comp.dt-emis-comp  = b-docum-est-aux.dt-emissao.

            RUN gera-valores (INPUT b-docum-est-aux.serie-docto,
                              INPUT b-docum-est-aux.nro-docto,
                              INPUT b-docum-est-aux.cod-emitente,
                              INPUT b-docum-est-aux.nat-operacao,
                              INPUT p-embarque,
                              INPUT b-docum-est-aux.cod-estabel,
                              INPUT 3). /*Nota Complementar*/
        end.
    end.
end procedure.

DEFINE TEMP-TABLE ttExtrator LIKE tt-embarque-corpo.
DEFINE VARIABLE c-arquivo AS CHAR.



FOR EACH tt-embarque-corpo NO-LOCK.

    FIND FIRST es_importacao_imposto
        WHERE es_importacao_imposto.cod_estabel     = tt-embarque-corpo.cod-estabel
          AND es_importacao_imposto.embarque        = tt-embarque-corpo.embarque NO-ERROR.

    IF NOT AVAIL es_importacao_imposto THEN DO:
        CREATE es_importacao_imposto.
        ASSIGN es_importacao_imposto.cod_estabel                        = tt-embarque-corpo.cod-estabel   /*chave primaria e unica*/                  
               es_importacao_imposto.embarque                           = tt-embarque-corpo.embarque. /*chave primaria e unica*/
    END.
    
    
    ASSIGN es_importacao_imposto.cod_emitente                       = tt-embarque-corpo.cod-emitente
           es_importacao_imposto.dtdi                               = tt-embarque-corpo.dt-di                    
           es_importacao_imposto.dt_emissao                         = tt-embarque-corpo.dt-emissao                    
           es_importacao_imposto.dt_venc_fat                        = tt-embarque-corpo.dt-venc-fat
           es_importacao_imposto.it_codigo                          = tt-embarque-corpo.it-codigo
           es_importacao_imposto.nr_nota_fis                        = tt-embarque-corpo.nr-nota-fis                          
           es_importacao_imposto.numerodi                           = tt-embarque-corpo.nr-di
           es_importacao_imposto.numero_ordem                       = STRING(tt-embarque-corpo.numero-ordem)
           es_importacao_imposto.per_despesa                        = tt-embarque-corpo.per-despesa
           es_importacao_imposto.per_ger_cofins_rea                 = tt-embarque-corpo.per-ger-cofins-rea               
           es_importacao_imposto.per_ger_des_rea                    = tt-embarque-corpo.per-ger-tot-des-rea                    
           es_importacao_imposto.per_ger_icms_rea                   = tt-embarque-corpo.per-ger-icms-rea                   
           es_importacao_imposto.per_ger_ii2_rea                    = tt-embarque-corpo.per-ger-ii2-rea                  
           es_importacao_imposto.per_ger_ii_rea                     = tt-embarque-corpo.per-ger-ii-rea               
           es_importacao_imposto.per_ger_ipi_rea                    = tt-embarque-corpo.per-ger-ipi-rea                   
           es_importacao_imposto.per_ger_mercado_rea                = tt-embarque-corpo.per-ger-mercado-rea                   
           es_importacao_imposto.per_ger_pis_rea                    = tt-embarque-corpo.per-ger-pis-rea      
           es_importacao_imposto.per_ger_tot_com_ipi_icms_rea       = tt-embarque-corpo.per-ger-tot-com-ipi-icms-rea           
           es_importacao_imposto.per_ger_tot_des_rea                = tt-embarque-corpo.per-ger-des-rea      
           es_importacao_imposto.per_ger_tot_imp_des_rea            = tt-embarque-corpo.per-ger-tot-imp-des-rea      
           es_importacao_imposto.per_ger_tot_imp_rea                = tt-embarque-corpo.per-ger-tot-imp-rea
           es_importacao_imposto.per_ger_tot_sem_ipi_icms_rea       = tt-embarque-corpo.per-ger-tot-sem-ipi-icms-rea                 
           es_importacao_imposto.per_ii                             = tt-embarque-corpo.per-ii                       
           es_importacao_imposto.per_imposto                        = tt-embarque-corpo.per-imposto                            
           es_importacao_imposto.per_mercado                        = tt-embarque-corpo.per-mercado        
           es_importacao_imposto.vl_ger_cofins_rea                  = tt-embarque-corpo.vl-ger-cofins-rea                
           es_importacao_imposto.vl_ger_des_rea                     = tt-embarque-corpo.vl-ger-des-rea                                 
           es_importacao_imposto.vl_ger_icms_rea                    = tt-embarque-corpo.vl-ger-icms-rea                                      
           es_importacao_imposto.vl_ger_ii2_rea                     = tt-embarque-corpo.vl-ger-ii2-rea                                     
           es_importacao_imposto.vl_ger_ii_rea                      = tt-embarque-corpo.vl-ger-ii-rea                                    
           es_importacao_imposto.vl_ger_ipi_rea                     = tt-embarque-corpo.vl-ger-ipi-rea                                 
           es_importacao_imposto.vl_ger_mercado_orc                 = tt-embarque-corpo.vl-ger-mercado-orc                                     
           es_importacao_imposto.vl_ger_mercado_rea                 = tt-embarque-corpo.vl-ger-mercado-rea                                     
           es_importacao_imposto.vl_ger_pis_rea                     = tt-embarque-corpo.vl-ger-pis-rea                                 
           es_importacao_imposto.vl_ger_tot_com_ipi_icms_merc_rea   = tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea                            
           es_importacao_imposto.vl_ger_tot_com_ipi_icms_orc        = 0 /*tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc */
           es_importacao_imposto.vl_ger_tot_com_ipi_icms_rea        = tt-embarque-corpo.vl-ger-tot-com-ipi-icms-rea                       
           es_importacao_imposto.vl_ger_tot_des_rea                 = tt-embarque-corpo.vl-ger-tot-des-rea                   
           es_importacao_imposto.vl_ger_tot_imp_des_rea             = tt-embarque-corpo.vl-ger-tot-imp-des-rea                   
           es_importacao_imposto.vl_ger_tot_imp_rea                 = tt-embarque-corpo.vl-ger-tot-imp-rea                       
           es_importacao_imposto.vl_ger_tot_sem_ipi_icms_merc_rea   = tt-embarque-corpo.vl-ger-tot-sem-ipi-icms-merc-rea
           es_importacao_imposto.vl_ger_tot_sem_ipi_icms_orc        = 0 /*tt-embarque-corpo.vl-ger-tot-sem-ipi-icms-orc*/     
           es_importacao_imposto.vl_ger_tot_sem_ipi_icms_rea        = tt-embarque-corpo.vl-ger-tot-sem-ipi-icms-merc-rea.
      
END.



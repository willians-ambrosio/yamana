/* variaveis-nfe-receb.i
** definicao padrao de variaveis recebimento
** {include\variaveis-nfe-receb.i}
**/

DEFINE NEW GLOBAL SHARED VARIABLE gsvr-user-valido          AS ROWID      NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE gsvc-transacao            AS CHARACTER  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE gsvr-transacao            AS ROWID      NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE gsvc-tabela-pesquisa      AS CHARACTER  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE gsvc-campo-sort           AS CHARACTER  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE gsvl-erro-transacao       AS logical    NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE gsvc-tipo-mensagem        AS CHARACTER  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE gsvc-desc-mensagem        AS CHARACTER  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE gsvc-acao-mensagem        AS CHARACTER  NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE gsvc-transacao-pai        AS CHARACTER  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE gsvr-transacao-pai        AS ROWID      NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE gsvc-tabela-pesquisa-pai  AS CHARACTER  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE gsvc-campo-sort-pai       AS CHARACTER  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE gsvl-erro-transacao-pai   AS logical    NO-UNDO.

DEF VAR h_bonfe001      AS HANDLE.
DEF VAR h_boin176       AS HANDLE.
DEF VAR h_boin367       AS HANDLE.
DEF VAR h_boin090       AS HANDLE.
DEF VAR h_boin223       AS HANDLE.
DEF VAR h_boin223a      AS HANDLE.

DEF NEW GLOBAL SHARED VAR c-seg-usuario         AS CHAR     NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-executa-bonfe001    AS CHAR     NO-UNDO.

DEF BUFFER bf-mitente FOR emitente.
DEF BUFFER bf5-natur-oper FOR natur-oper.
DEF BUFFER b-item-doc-est FOR item-doc-est.
def buffer bf6-nfe-it-nota-fisc-rec for nfe-it-nota-fisc-rec.
DEF VAR i-rel-doc AS INT.
DEF VAR c-mensagem-erro             AS CHAR FORMAT "x(200)"             NO-UNDO.
DEF VAR i-acao                      AS INTE                             NO-UNDO.
DEF VAR c-arquivo                   AS CHAR                             NO-UNDO.
DEF VAR c-diretorio                 AS CHAR                             NO-UNDO.
DEF VAR c-nome-emit                 AS CHAR FORMAT "x(050)"             NO-UNDO.
DEF VAR c-nome-dir-rec              AS CHAR                             NO-UNDO.
DEF VAR c-nome-dir-rec-dest         AS CHAR                             NO-UNDO.
DEF VAR c-nome-dir-rec-sefaz-dest   AS CHAR                             NO-UNDO.
DEF VAR c-nome-dir-erro             AS CHAR                             NO-UNDO.
DEF VAR c-file                      AS CHAR                             NO-UNDO.
DEF VAR c-lista                     AS CHAR                             NO-UNDO.
DEF VAR c-valor                     AS CHAR                             NO-UNDO.
DEF VAR i-cont                      AS INTE                             NO-UNDO.
DEF VAR i-sequencia                 AS INTE                             NO-UNDO.
DEF VAR c-situacao                  AS CHAR                             NO-UNDO.

DEF VAR i-cod-emitente-ini          AS INTE FORMAT ">>>>>>>>9"          NO-UNDO.
DEF VAR i-cod-emitente-fim          AS INTE FORMAT ">>>>>>>>9"          NO-UNDO.
DEF VAR c-cod-estabel-ini           AS CHAR FORMAT "x(003)"             NO-UNDO.
DEF VAR c-cod-estabel-fim           AS CHAR FORMAT "x(003)"             NO-UNDO.
DEF VAR c-serie-ini                 AS CHAR FORMAT "x(003)"             NO-UNDO.
DEF VAR c-serie-fim                 AS CHAR FORMAT "x(003)"             NO-UNDO.
DEF VAR c-nota-fiscal-ini           AS CHAR                             NO-UNDO.
DEF VAR c-nota-fiscal-fim           AS CHAR                             NO-UNDO.
DEF VAR c-nome-abrev-ini            AS CHAR FORMAT "x(012)"             NO-UNDO.
DEF VAR c-nome-abrev-fim            AS CHAR FORMAT "x(012)"             NO-UNDO.
DEF VAR da-dt-emissao-ini           AS DATE FORMAT "99/99/9999"         NO-UNDO.
DEF VAR da-dt-emissao-fim           AS DATE FORMAT "99/99/9999"         NO-UNDO.
DEF VAR log-autorizada              AS LOG                              NO-UNDO.
DEF VAR log-nao-autorizada          AS LOG                              NO-UNDO.
DEF VAR log-implantada              AS LOG                              NO-UNDO.
DEF VAR log-manual                  AS LOG                              NO-UNDO.
DEF VAR log-pendente                AS LOG                              NO-UNDO.
DEF VAR log-cancelada               AS LOG                              NO-UNDO.
DEF VAR log-aceita                  AS LOG                              NO-UNDO.
DEF VAR log-rejeitada               AS LOG                              NO-UNDO.
DEF VAR log-carregada               AS LOG                              NO-UNDO.
DEF VAR de-fat-conv                 AS DECI                             NO-UNDO.
DEF VAR log-cte                     AS LOG                              NO-UNDO.
DEF VAR log-nfe                     AS LOG                              NO-UNDO.

/* --- 1 - Compra / 2 - Devol Cli / 3 - Retorno Terceiro / 4 - Remessa Terceiro --- */
DEF VAR i-tipo-nfe                  AS INTE                             NO-UNDO.
DEF VAR i-tp-oper-terc              AS INTE                             NO-UNDO.
DEF VAR c-aux                       AS CHAR FORMAT "x(200)"             NO-UNDO.
DEF VAR c-tag                       AS CHAR FORMAT "x(050)"             NO-UNDO.
DEF VAR i-aux                       AS INTE                             NO-UNDO.
DEF VAR i-aux2                      AS INTE                             NO-UNDO.
DEF VAR c-chave-acesso-nfe          AS CHAR FORMAT "x(50)"              NO-UNDO.
DEF VAR rw-saldo-terc               AS ROWID                            NO-UNDO.
DEF VAR rw-aux                      AS ROWID                            NO-UNDO.
DEF VAR i-erro                      AS INTE                             NO-UNDO.
DEF VAR l-erro                      AS LOG                              NO-UNDO.
DEF VAR de-qtde-it-terc             AS DECI                             NO-UNDO.
DEF VAR de-coeficiente              AS DECI                             NO-UNDO.
DEF VAR i-num-casas                 AS INTE                             NO-UNDO.
/*DEF VAR i-fator                     AS INTE FORMAT ">>>>>>>>>9"         NO-UNDO.*/
DEF VAR i-fator                     AS DEC FORMAT ">>>>>>>>>>>>>>9"     NO-UNDO.

DEF VAR de-base-calculo             AS DECI                             NO-UNDO.
DEF VAR de-item-vProd               AS DECI                             NO-UNDO.
DEF VAR de-vl-total-prod-nat        AS DECI                             NO-UNDO.
def var de-vl-total-vbicmssub       as deci                             no-undo.
def var de-vl-total-vicmssub        as deci                             no-undo.
def var de-vl-total-vbipi           as deci                             no-undo.
def var de-vl-total-vipi            as deci                             no-undo.
def var de-vl-total-vicms           as deci                             no-undo.
def var de-vl-total-vbicms          as deci                             no-undo.                         
def var de-vl-total                 as deci                             no-undo.                         

DEF VAR l-aux                       AS LOG                              NO-UNDO.
DEF VAR l-aux2                      AS LOG                              NO-UNDO.
DEF VAR de-item-vFrete              AS DECI                             NO-UNDO.
DEF VAR de-item-despesas            AS DECI                             NO-UNDO.
DEF VAR de-item-desconto            AS DECI                             NO-UNDO.
DEF VAR de-vFrete                   AS DECI                             NO-UNDO.
DEF VAR de-vSeg                     AS DECI                             NO-UNDO.
DEF VAR de-vDesc                    AS DECI                             NO-UNDO.
DEF VAR de-vOutro                   AS DECI                             NO-UNDO.
DEF VAR de-total                    AS DECI                             NO-UNDO.
DEF VAR c-item-nro-docto-ini        AS CHAR FORMAT "x(016)"             NO-UNDO.
DEF VAR c-item-nro-docto-fim        AS CHAR FORMAT "x(016)"             NO-UNDO.
DEF VAR c-item-serie-docto-ini      AS CHAR FORMAT "x(005)"             NO-UNDO.
DEF VAR c-item-serie-docto-fim      AS CHAR FORMAT "x(005)"             NO-UNDO.
DEF VAR l-ordem                     AS LOG INIT NO                      NO-UNDO.
DEF VAR l-ordem-ser-lote            AS LOG INIT NO                      NO-UNDO.
DEF VAR l-ser-lote                  AS LOG INIT NO                      NO-UNDO.
DEF VAR l-docto-nota                AS LOG INIT NO                      NO-UNDO.
DEF VAR l-docto-ser-lote            AS LOG INIT NO                      NO-UNDO.
DEF VAR l-nota-beneficiamento       AS LOG INIT NO                      NO-UNDO.
DEF VAR de-saldo-ordem              AS DECI                             NO-UNDO.
DEF VAR de-saldo-ser-lote           AS DECI                             NO-UNDO.
DEF VAR de-saldo-doc-nota           AS DECI                             NO-UNDO.
DEF VAR de-quantidade               AS DECI                             NO-UNDO.
DEF VAR de-quant-variacao           AS DECI                             NO-UNDO.

DEF VAR rDocumEstMovtoPend          AS ROWID                            NO-UNDO.
DEF VAR i-cod-emitente              AS INTE FORMAT ">>>>>>>>9"          NO-UNDO.
DEF VAR c-serie                     AS CHAR FORMAT "x(010)"             NO-UNDO.
DEF VAR c-nro-docto                 AS CHAR FORMAT "x(016)"             NO-UNDO.
DEF VAR c-natur-oper                AS CHAR FORMAT "x(008)"             NO-UNDO.
DEF VAR c-depos                     AS CHAR                             NO-UNDO.

DEF VAR l-erro2                     AS LOG INITIAL NO                   NO-UNDO.
DEF VAR c-erro                      AS CHAR                             NO-UNDO.

DEF VAR c-handle-obj                AS CHAR                            NO-UNDO.
DEF VAR c-nat-operacao                 LIKE natur-oper.nat-operacao    NO-UNDO.
DEF VAR cb-cod-observa                 LIKE docum-est.cod-observa      NO-UNDO.
DEF VAR rw-nota                     AS ROWID                           NO-UNDO.
def var l-soma-desconto-preco       as LOG                             no-undo.
DEF VAR l-gera-duplic               AS LOG                             NO-UNDO.
def var l-CST-ICMS-xml              as LOG                             no-undo.
def var l-CST-ICMS-erp              as LOG                             no-undo.
DEF VAR l-IPI-dev-xml               AS   LOGICAL                       NO-UNDO.
DEF VAR l-IPI-xml                   AS   LOGICAL                       NO-UNDO.
DEF VAR l-PIS-dev-xml               AS   LOGICAL                       NO-UNDO.
DEF VAR l-PIS-xml                   AS   LOGICAL                       NO-UNDO.
DEF VAR l-COFINS-dev-xml            AS   LOGICAL                       NO-UNDO.
DEF VAR l-COFINS-xml                AS   LOGICAL                       NO-UNDO.
DEF VAR l-ICMS-dev-xml              AS   LOGICAL                       NO-UNDO.
DEF VAR l-ICMS-xml                  AS   LOGICAL                       NO-UNDO.
DEF VAR l-ICMS-ST-dev-xml           AS   LOGICAL                       NO-UNDO.
DEF VAR l-ICMS-ST-xml               AS   LOGICAL                       NO-UNDO.
DEF VAR l-multi_natureza            AS   LOGICAL                       NO-UNDO.
DEF VAR l-CEST                      AS   LOGICAL                       NO-UNDO.
DEF VAR c-orig-cst                  AS CHAR NO-UNDO.
DEF VAR c-cst-icms                  AS CHAR NO-UNDO.
DEF VAR c-nat-retorno               AS CHAR NO-UNDO.
DEFINE VARIABLE h_cd4337            AS HANDLE.
DEFINE VARIABLE pi-trib-pis         AS INTEGER.
DEFINE VARIABLE pi-aliquota-pis     AS DECIMAL.
DEFINE VARIABLE pi-valor-un-pis     AS DECIMAL.
DEFINE VARIABLE pi-reduz-pis        AS DECIMAL.
DEFINE VARIABLE pi-trib-cofins      AS INTEGER.
DEFINE VARIABLE pi-aliquota-cofins  AS DECIMAL.
DEFINE VARIABLE pi-valor-un-cofins  AS DECIMAL.
DEFINE VARIABLE pi-reduz-cofins     AS DECIMAL.

DEFINE VARIABLE de-var-val-re-maior AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-var-qtd-re       AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-var-max-perc     AS DECIMAL     FORMAT ">>>,>>>,>>9.999999999"   NO-UNDO.
DEFINE VARIABLE de-valor-orig       AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-valor-min        AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-var-max-lim      AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-lim-var-valor    AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-vl-merc-liq-aux  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-lim-var-qtd      AS DECIMAL     NO-UNDO.

DEFINE VARIABLE hXML                AS HANDLE   NO-UNDO.
DEFINE VARIABLE hRoot               AS HANDLE   NO-UNDO.
DEFINE VARIABLE hRootFilho          AS HANDLE   NO-UNDO.
DEFINE VARIABLE hFIELDName          AS HANDLE   NO-UNDO.
DEFINE VARIABLE hFIELDValue         AS HANDLE   NO-UNDO.
DEFINE VARIABLE hRecordPai          AS HANDLE   NO-UNDO.
DEFINE VARIABLE hRecordFilho        AS HANDLE   NO-UNDO.       
DEFINE VARIABLE hRecordAux          AS HANDLE   NO-UNDO.
DEFINE VARIABLE hRecordAuxvalue     AS HANDLE   NO-UNDO.
DEFINE VARIABLE hText               AS HANDLE   NO-UNDO.
DEFINE VARIABLE hRootCon            AS HANDLE   NO-UNDO.
DEFINE VARIABLE hXMLCon             AS HANDLE   NO-UNDO.

CREATE X-DOCUMENT hXML.
CREATE X-NODEREF  hRoot.
CREATE X-NODEREF  hRootFilho.
CREATE X-NODEREF  hRecordPai.
CREATE X-NODEREF  hRecordFilho.
CREATE X-NODEREF  hFIELDName.
CREATE X-NODEREF  hFIELDValue.
CREATE X-NODEREF  hText.          
CREATE X-NODEREF  hRecordAux.
CREATE X-NODEREF  hRecordAuxValue.

CREATE X-DOCUMENT hXMLCon.
CREATE X-NODEREF  hRootCon.




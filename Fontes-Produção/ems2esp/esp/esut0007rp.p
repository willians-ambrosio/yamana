/********************************************************************************************
**  Programa: ESUT0007
**  Data....: 08 de Setembro de 2016
**  Autor...: Sergio Luiz Neto da Silveira - DSC
**  Objetivo: Relat¢rio LOG de Elimina‡Æo de Ordens de Compra
**  Versão..: 2.06.00.001 - Versao Inicial
*********************************************************************************************/

/************************************* INCLUDES PADRAO **************************************/
/* include de controle de versão */
{include/i-prgvrs.i ESUT0007RP 2.06.00.001}
/********************************************************************************************/

/********************************* DEFINICAO DE TEMP-TABLES *********************************/
/* definiîÒo das temp-tables para recebimento de par±metros */
define temp-table tt-param no-undo
    field destino              as integer
    field arquivo              as char format "x(35)"
    field usuario              as char format "x(12)"
    field data-exec            as date
    field hora-exec            as integer
    field classifica           as integer
    field desc-classifica      as char format "x(40)"
    field modelo-rtf           as char format "x(35)"
    field l-habilitaRtf        as LOG
    FIELD numero-ordem-ini     LIKE ordem-compra.numero-ordem
    FIELD numero-ordem-fim     LIKE ordem-compra.numero-ordem
    FIELD num-pedido-ini       LIKE ordem-compra.num-pedido
    FIELD num-pedido-fim       LIKE ordem-compra.num-pedido
    FIELD it-codigo-ini        LIKE ordem-compra.it-codigo
    FIELD it-codigo-fim        LIKE ordem-compra.it-codigo
    FIELD nr-requisicao-ini    LIKE ordem-compra.nr-requisicao
    FIELD nr-requisicao-fim    LIKE ordem-compra.nr-requisicao
    FIELD data-ini             AS   DATE FORMAT "99/99/9999"
    FIELD data-fim             AS   DATE FORMAT "99/99/9999"
    FIELD cod-usuario-ini      AS   CHARACTER FORMAT "x(12)"
    FIELD cod-usuario-fim      AS   CHARACTER FORMAT "x(12)".
    
DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita       AS RAW.

DEFINE STREAM st-excel.
/********************************************************************************************/

/********************************* DEFINICAO DE PARAMETROS **********************************/
/* recebimento de par±metros */
DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FIND FIRST tt-param.
/********************************************************************************************/

/********************************** DEFINICAO DE VARIAVEIS **********************************/
define new global shared variable v_cod_usuar_corren as character no-undo.
define variable h-acomp        as handle    no-undo.

DEFINE VARIABLE i-cont AS INTEGER NO-UNDO.

&IF "{&mguni_version}" >= "2.071" &THEN
def new global shared var i-ep-codigo-usuario  LIKE ems2cadme.empresa.ep-codigo format "x(03)" no-undo.
&ELSE
def new global shared var i-ep-codigo-usuario  as integer format ">>9" no-undo.
&ENDIF
def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.

{include/i-rpvar.i}



/********************************************************************************************/

/************************************ DEFINICAO DE FUNCOES **********************************/
/********************************************************************************************/

/************************************ DEFINICAO DE FRAMES ***********************************/
FIND FIRST  ems2cadme.empresa 
     NO-LOCK NO-ERROR.

/* bloco principal do programa */
ASSIGN c-programa 	    = "ESUT0007RP"
	   c-versao	        = "2.06"
	   c-revisao	    = "00.001"
	   c-empresa 	    = empresa.razao-social
	   c-sistema	    = "UTP"
	   c-titulo-relat   = "Relat¢rio LOG de Elimina‡Æo de Ordens de Compra".

FORM HEADER 
     FILL("-", 132)        FORMAT "x(132)" SKIP 
     c-empresa 
     c-titulo-relat AT 050
     "Pagina:":U    AT 120 
     page-number    AT 128 FORMAT ">>>>9" SKIP 
     FILL("-", 112)        FORMAT "x(110)" 
     TODAY                 FORMAT "99/99/9999"
     "-" 
     STRING(TIME,"HH:MM:SS":U) SKIP 
     "                       " SKIP
     "-----------------------"
WITH STREAM-IO NO-BOX NO-LABEL OVERLAY PAGE-TOP WIDTH 132 FRAME f-cabec.

ASSIGN c-rodape = "DATASUL - " + c-sistema + " - " + c-programa + " - V:" + c-versao + "." + c-revisao
       c-rodape = FILL("-", 132 - LENGTH(c-rodape)) + c-rodape.

FORM HEADER 
     c-rodape   FORMAT "x(132)"
  WITH STREAM-IO WIDTH 132 NO-LABELS NO-BOX PAGE-BOTTOM FRAME f-rodape.
/********************************************************************************************/
      
/************************************* BLOCO PRINCIPAL **************************************/
run utp/ut-acomp.p PERSISTENT SET h-acomp.

{utp/ut-liter.i Imprimindo *}

run pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

/* include padrão para output de relat©rios */
{include/i-rpout.i}

RUN pi-impressao.

/* fechamento do output do relat©rio  */
{include/i-rpclo.i}

run pi-finalizar IN h-acomp.
RETURN "OK":U.
/********************************************************************************************/

/********************************* DEFINICAO DE procedureS **********************************/
procedure pi-impressao:
   DEFINE VARIABLE c-narrativa      AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE c-arquivo        AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE ch-excel         AS    COMPONENT-HANDLE      NO-UNDO.
   DEFINE VARIABLE i-cont-linha     AS    INTEGER               NO-UNDO.
   DEFINE VARIABLE c-tipo           AS    CHARACTER             NO-UNDO.
   DEFINE VARIABLE c-ordem          AS    CHARACTER             NO-UNDO FORMAT "x(300)".

   ASSIGN c-arquivo = ENTRY(1,tt-param.arquivo,".") + ".csv".
   
   FIND FIRST param-global
        NO-LOCK NO-ERROR.

   FILE-INFO:FILE-NAME = c-arquivo.

   IF SEARCH(FILE-INFO:FULL-PATHNAME) <> ? THEN
      OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).

   OUTPUT STREAM st-excel TO VALUE(c-arquivo) NO-CONVERT NO-MAP.

   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada numero-ordem              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada data                      1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada hora                      1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada cod-usuario               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada it-codigo                 1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada natureza                  1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada situacao                  1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada origem                    1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada op-codigo                 1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada data-emissao              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada ct-codigo                 1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada sc-codigo                 1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada requisitante              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada dep-almoxar               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada ordem-servic              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada cod-comprado              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada narrativa                 1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada num-pedido                1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada data-pedido               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada cod-emitente              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada data-cotacao              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada preco-orig                1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada preco-unit                1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada pre-unit-for              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada preco-fornec              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada nr-alt-preco              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada mo-codigo                 1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada codigo-ipi                1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada aliquota-ipi              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada codigo-icm                1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada aliquota-icm              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada aliquota-iss              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada frete                     1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada valor-frete               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada taxa-financ               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada valor-taxa                1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada saldo-emb                 1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada perc-descto               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada saldo-gi                  1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada cod-cond-pag              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada prazo-entreg              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada contato                   1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada impr-ficha                1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada comentarios               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada usuario                   1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada data-atualiz              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada hora-atualiz              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada nr-ord-orig               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada cod-estabel               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada ind-reajuste              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada linha                     1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada cod-refer                 1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada nr-processo               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada valor-descto              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada nr-dias-taxa              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada tp-despesa                1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada qt-acum-nec               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada qt-acum-rec               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada qt-acum-dev               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada ind-extrac                1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada cons-mrp                  1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada cons-pmp                  1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada item-pai                  1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada cod-roteiro               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada op-seq                    1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada num-ord-inv               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada nr-requisicao             1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada sequencia                 1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada reaj-tabela               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada nr-tab                    1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada ep-codigo                 1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada conta-contabil            1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada nr-seq-contr              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada ordem-emitida             1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada expectativa               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada qt-solic                  1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada cota-ordem                1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada seq-evento                1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada pend-aprov                1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada perc-vat                  1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada perc-sales-tax            1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada cod-maq-origem            1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada num-processo-mp           1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada cod-transp                1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada num-id-documento          1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada nr-contrato               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada num-seq-item              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada sit-ordem-contrat         1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada dat-ordem                 1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada check-sum                 1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada prioridade-aprov          1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada origem-aprov              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada gera-edi                  1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada cod-estab-gestor          1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada licenca-import            1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada loc-entrega               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada cod-entrega               1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada estab-entrega             1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada nr-pedcli                 1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada seq-ped-venda             1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada local-entrega             1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada cod-estab-ctr             1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada nr-seq-contr-it           1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada nr-contrato-venda         1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada cod-refer-b2b             1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada dat-inicio-leilao-rfq     1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada dat-fim-leilao-rfq        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada hra-inicio-leilao-rfq     1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada hra-fim-leilao-rfq        1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada log-cot-aberta            1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada log-leilao                1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada cod-grp-compra            1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada cdn-fabrican              1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada des-referencia            1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   {utp/ut-field.i ems5_esp es-ordem-compra-eliminada cod-unid-negoc            1}. PUT STREAM st-excel RETURN-VALUE FORMAT "X(25)" ";".              
   
   PUT STREAM st-excel SKIP.
       
   blk1:
   FOR EACH es-ordem-compra-eliminada
            WHERE es-ordem-compra-eliminada.numero-ordem   >= tt-param.numero-ordem-ini  AND
                  es-ordem-compra-eliminada.numero-ordem   <= tt-param.numero-ordem-fim  AND
                  es-ordem-compra-eliminada.num-pedido     >= tt-param.num-pedido-ini    AND
                  es-ordem-compra-eliminada.num-pedido     <= tt-param.num-pedido-fim    AND
                  es-ordem-compra-eliminada.it-codigo      >= tt-param.it-codigo-ini     AND
                  es-ordem-compra-eliminada.it-codigo      <= tt-param.it-codigo-fim     AND
                  es-ordem-compra-eliminada.nr-requisicao  >= tt-param.nr-requisicao-ini AND
                  es-ordem-compra-eliminada.nr-requisicao  <= tt-param.nr-requisicao-fim AND
                  es-ordem-compra-eliminada.data           >= tt-param.data-ini          AND
                  es-ordem-compra-eliminada.data           <= tt-param.data-fim          AND
                  es-ordem-compra-eliminada.cod-usuario    >= tt-param.cod-usuario-ini   AND
                  es-ordem-compra-eliminada.cod-usuario    <= tt-param.cod-usuario-fim  
            NO-LOCK:

      ASSIGN i-cont = i-cont + 1.

      RUN pi-acompanhar IN h-acomp ("Listando: " + STRING(i-cont)).

      PUT STREAM st-excel
          es-ordem-compra-eliminada.numero-ordem            ";"
          es-ordem-compra-eliminada.data                    ";"
          es-ordem-compra-eliminada.hora                    ";"
          es-ordem-compra-eliminada.cod-usuario             ";"
          es-ordem-compra-eliminada.it-codigo               ";"
          es-ordem-compra-eliminada.natureza                ";"
          es-ordem-compra-eliminada.situacao                ";"
          es-ordem-compra-eliminada.origem                  ";"
          es-ordem-compra-eliminada.op-codigo               ";"
          es-ordem-compra-eliminada.data-emissao            ";"
          es-ordem-compra-eliminada.ct-codigo               ";"
          es-ordem-compra-eliminada.sc-codigo               ";"
          es-ordem-compra-eliminada.requisitante            ";"
          es-ordem-compra-eliminada.dep-almoxar             ";"
          es-ordem-compra-eliminada.ordem-servic            ";"
          es-ordem-compra-eliminada.cod-comprado            ";"
          REPLACE(REPLACE(REPLACE(es-ordem-compra-eliminada.narrativa,";",":"),CHR(10),""),CHR(13),"") FORMAT "x(2000)" ";"
          es-ordem-compra-eliminada.num-pedido              ";"
          es-ordem-compra-eliminada.data-pedido             ";"
          es-ordem-compra-eliminada.cod-emitente            ";"
          es-ordem-compra-eliminada.data-cotacao            ";"
          es-ordem-compra-eliminada.preco-orig              ";"
          es-ordem-compra-eliminada.preco-unit              ";"
          es-ordem-compra-eliminada.pre-unit-for            ";"
          es-ordem-compra-eliminada.preco-fornec            ";"
          es-ordem-compra-eliminada.nr-alt-preco            ";"
          es-ordem-compra-eliminada.mo-codigo               ";"
          es-ordem-compra-eliminada.codigo-ipi              ";"
          es-ordem-compra-eliminada.aliquota-ipi            ";"
          es-ordem-compra-eliminada.codigo-icm              ";"
          es-ordem-compra-eliminada.aliquota-icm            ";"
          es-ordem-compra-eliminada.aliquota-iss            ";"
          es-ordem-compra-eliminada.frete                   ";"
          es-ordem-compra-eliminada.valor-frete             ";"
          es-ordem-compra-eliminada.taxa-financ             ";"
          es-ordem-compra-eliminada.valor-taxa              ";"
          es-ordem-compra-eliminada.saldo-emb               ";"
          es-ordem-compra-eliminada.perc-descto             ";"
          es-ordem-compra-eliminada.saldo-gi                ";"
          es-ordem-compra-eliminada.cod-cond-pag            ";"
          es-ordem-compra-eliminada.prazo-entreg            ";"
          es-ordem-compra-eliminada.contato                 ";"
          es-ordem-compra-eliminada.impr-ficha              ";"
          es-ordem-compra-eliminada.comentarios             ";"
          es-ordem-compra-eliminada.usuario                 ";"
          es-ordem-compra-eliminada.data-atualiz            ";"
          es-ordem-compra-eliminada.hora-atualiz            ";"
          es-ordem-compra-eliminada.nr-ord-orig             ";"
          es-ordem-compra-eliminada.cod-estabel             ";"
          es-ordem-compra-eliminada.ind-reajuste            ";"
          es-ordem-compra-eliminada.linha                   ";"
          es-ordem-compra-eliminada.cod-refer               ";"
          es-ordem-compra-eliminada.nr-processo             ";"
          es-ordem-compra-eliminada.valor-descto            ";"
          es-ordem-compra-eliminada.nr-dias-taxa            ";"
          es-ordem-compra-eliminada.tp-despesa              ";"
          es-ordem-compra-eliminada.qt-acum-nec             ";"
          es-ordem-compra-eliminada.qt-acum-rec             ";"
          es-ordem-compra-eliminada.qt-acum-dev             ";"
          es-ordem-compra-eliminada.ind-extrac              ";"
          es-ordem-compra-eliminada.cons-mrp                ";"
          es-ordem-compra-eliminada.cons-pmp                ";"
          es-ordem-compra-eliminada.item-pai                ";"
          es-ordem-compra-eliminada.cod-roteiro             ";"
          es-ordem-compra-eliminada.op-seq                  ";"
          es-ordem-compra-eliminada.num-ord-inv             ";"
          es-ordem-compra-eliminada.nr-requisicao           ";"
          es-ordem-compra-eliminada.sequencia               ";"
          es-ordem-compra-eliminada.reaj-tabela             ";"
          es-ordem-compra-eliminada.nr-tab                  ";"
          es-ordem-compra-eliminada.ep-codigo               ";"
          es-ordem-compra-eliminada.conta-contabil          ";"
          es-ordem-compra-eliminada.nr-seq-contr            ";"
          es-ordem-compra-eliminada.ordem-emitida           ";"
          es-ordem-compra-eliminada.expectativa             ";"
          es-ordem-compra-eliminada.qt-solic                ";"
          es-ordem-compra-eliminada.cota-ordem              ";"
          es-ordem-compra-eliminada.seq-evento              ";"
          es-ordem-compra-eliminada.pend-aprov              ";"
          es-ordem-compra-eliminada.perc-vat                ";"
          es-ordem-compra-eliminada.perc-sales-tax          ";"
          es-ordem-compra-eliminada.cod-maq-origem          ";"
          es-ordem-compra-eliminada.num-processo-mp         ";"
          es-ordem-compra-eliminada.cod-transp              ";"
          es-ordem-compra-eliminada.num-id-documento        ";"
          es-ordem-compra-eliminada.nr-contrato             ";"
          es-ordem-compra-eliminada.num-seq-item            ";"
          es-ordem-compra-eliminada.sit-ordem-contrat       ";"
          es-ordem-compra-eliminada.dat-ordem               ";"
          es-ordem-compra-eliminada.check-sum               ";"
          es-ordem-compra-eliminada.prioridade-aprov        ";"
          es-ordem-compra-eliminada.origem-aprov            ";"
          es-ordem-compra-eliminada.gera-edi                ";"
          es-ordem-compra-eliminada.cod-estab-gestor        ";"
          es-ordem-compra-eliminada.licenca-import          ";"
          es-ordem-compra-eliminada.loc-entrega             ";"
          es-ordem-compra-eliminada.cod-entrega             ";"
          es-ordem-compra-eliminada.estab-entrega           ";"
          es-ordem-compra-eliminada.nr-pedcli               ";"
          es-ordem-compra-eliminada.seq-ped-venda           ";"
          es-ordem-compra-eliminada.local-entrega           ";"
          es-ordem-compra-eliminada.cod-estab-ctr           ";"
          es-ordem-compra-eliminada.nr-seq-contr-it         ";"
          es-ordem-compra-eliminada.nr-contrato-venda       ";"
          es-ordem-compra-eliminada.cod-refer-b2b           ";"
          es-ordem-compra-eliminada.dat-inicio-leilao-rfq   ";"
          es-ordem-compra-eliminada.dat-fim-leilao-rfq      ";"
          es-ordem-compra-eliminada.hra-inicio-leilao-rfq   ";"
          es-ordem-compra-eliminada.hra-fim-leilao-rfq      ";"
          es-ordem-compra-eliminada.log-cot-aberta          ";"
          es-ordem-compra-eliminada.log-leilao              ";"
          es-ordem-compra-eliminada.cod-grp-compra          ";"
          es-ordem-compra-eliminada.cdn-fabrican            ";"
          es-ordem-compra-eliminada.des-referencia          ";"
          es-ordem-compra-eliminada.cod-unid-negoc          ";".

      PUT STREAM st-excel SKIP.                                                                                       
   END.                                                                                                   

   OUTPUT STREAM st-excel CLOSE.
   DOS SILENT START excel.exe VALUE(c-arquivo).
END PROCEDURE.
/********************************************************************************************/
/************************************** FIM DO PROGRAMA *************************************/
/********************************************************************************************/


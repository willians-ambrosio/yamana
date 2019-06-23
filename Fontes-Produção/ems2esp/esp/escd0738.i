/********************************************************************************
**   Autor: Joao B. C. Bisneto
** Empresa: DSC-PRAXIS
**    Data: Maio/2016
**Objetivo: Gera‡Æo de relat¢rio em Excel para levantamento de 
**          requisi‡äes por t‚cnico.
*******************************************************************************/

DEFINE VARIABLE c-modelo          AS CHAR FORMAT "X(256)"              NO-UNDO.
DEFINE VARIABLE excelappl         AS COM-HANDLE                        NO-UNDO.
DEFINE VARIABLE worksheets        AS COM-HANDLE                        NO-UNDO.
DEFINE VARIABLE workbooks         AS COM-HANDLE                        NO-UNDO.
DEFINE VARIABLE i-lin             AS INTEGER                           NO-UNDO.

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG
    /*---------------------------------------------*/
    FIELD tt-ini-cd-tecnico      LIKE mmi-tec-req-prod.cd-tecnico
    FIELD tt-fim-cd-tecnico      LIKE mmi-tec-req-prod.cd-tecnico
    FIELD tt-ini-dat-trans       LIKE req-ord-produc.dat-trans
    FIELD tt-fim-dat-trans       LIKE req-ord-produc.dat-trans
    FIELD tt-ini-nr-ord-produ    LIKE req-ord-produc.nr-ord-produ
    FIELD tt-fim-nr-ord-produ    LIKE req-ord-produc.nr-ord-produ
    FIELD tt-ini-nr-requisicao   LIKE req-ord-produc.nr-requisicao
    FIELD tt-fim-nr-requisicao   LIKE req-ord-produc.nr-requisicao
    FIELD tt-ini-cd-equipto      LIKE ord-manut.cd-equipto
    FIELD tt-fim-cd-equipto      LIKE ord-manut.cd-equipto
    /*---------------------------------------------*/
    .

def temp-table tt-raw-digita
   field raw-digita      as raw.

DEF TEMP-TABLE tt-relat NO-UNDO
  FIELD cd-tecnico      LIKE mmi-tec-req-prod.cd-tecnico   /*  T‚cnico                    */ 
  FIELD nr-ord-produ    LIKE req-ord-produc.nr-ord-produ   /*  Numero da Ordem            */
  FIELD nr-requisicao   LIKE req-ord-produc.nr-requisicao  /*  Numero da Requisi‡Æo       */
  FIELD sequencia       LIKE req-ord-produc.sequencia      /*  Sequencia da Requisi‡Æo    */
  FIELD cd-equipto      LIKE equipto.cd-equipto            /*  C¢digo do Equipamento      */
  FIELD descricao       LIKE equipto.descricao             /*  Descri‡Æo do Equipamento   */
  FIELD cod-estabel     LIKE req-ord-produc.cod-estabel    /*  Estabelecimento            */
  FIELD dat-trans       LIKE req-ord-produc.dat-trans      /*  Data Requisi‡Æo            */
  FIELD situacao        AS CHAR                            /*  Situa‡Æo da Requisi‡Æo     */ /*LIKE it-requisicao.situacao */
  FIELD estado          AS CHAR                            /*  Estado da Requisi‡Æo       */ /*LIKE it-requisicao.estado*/
  FIELD it-codigo       LIKE it-requisicao.it-codigo       /*  C¢digo do Item             */
  FIELD desc-item       LIKE item.desc-item                /*  Descri‡Æo do Item          */
  FIELD un              LIKE ITEM.un                       /*  Unidade                    */
  FIELD qt-requisitada  LIKE it-requisicao.qt-requisitada  /*  Quantidade do item         */
  FIELD preco-unit      LIKE it-requisicao.preco-unit      /*  Pre‡o Unit rio             */
  FIELD dt-entrega      LIKE it-requisicao.dt-entrega      /*  Data Entrega               */
  FIELD tp-requis       AS CHAR                            /*  Tipo de Requisi‡Æo         */ /*LIKE req-ord-produc.tp-requis*/
  FIELD requisitante    LIKE it-requisicao.nome-abrev      /*  Requisitante               */
  FIELD fm-codigo       LIKE item.fm-codigo                /*  Familia                    */
  FIELD sc-desp         LIKE ord-manut.sc-desp             /*  C Custo Despesa */
  FIELD ct-desp         LIKE ord-manut.ct-desp             /*  Conta Despesa   */
  FIELD numero-ordem    LIKE ordem-compra.numero-ordem     /*  Numero Ordem */
  FIELD pedido-compr    LIKE pedido-compr.num-pedido       /*  Numero do Pedido de Compra */
  FIELD nro-docto       LIKE item-doc-est.nro-docto        /*  Numero da Nota Fiscal do recbimento */
  FIELD narrativa       LIKE it-requisicao.narrativa       /*  Narrativa */
  FIELD nome-compl      LIKE tecn-mi.nome-compl
  FIELD des-man-ocor    LIKE ord-manut.des-man-corr
  FIELD parada-descricao LIKE mi-parada.descricao
  .

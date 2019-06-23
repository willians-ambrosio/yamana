{include/i_dbvers.i}
/******************************************************************************
** Include:   yg0006.i -  Defini‡Æo da tt-param                              **
** Programas: yg0006.w                                                       **
**            yg0006rp.p                                                     **
******************************************************************************/

{cdp/cdcfgmat.i}

DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD item-ini           LIKE movto-estoq.it-codigo
    FIELD item-fim           LIKE movto-estoq.it-codigo
    FIELD cod-estabel-ini    LIKE movto-estoq.cod-estabel
    FIELD cod-estabel-fim    LIKE movto-estoq.cod-estabel
    FIELD dt-trans-ini       LIKE movto-estoq.dt-trans
    FIELD dt-trans-fim       LIKE movto-estoq.dt-trans
    FIELD cod-refer-ini      LIKE movto-estoq.cod-refer
    FIELD cod-refer-fim      LIKE movto-estoq.cod-refer
    FIELD cod-depos-ini      LIKE movto-estoq.cod-depos
    FIELD cod-depos-fim      LIKE movto-estoq.cod-depos
    FIELD lote-ini           LIKE movto-estoq.lote
    FIELD lote-fim           LIKE movto-estoq.lote
    FIELD cod-localiz-ini    LIKE movto-estoq.cod-localiz
    FIELD cod-localiz-fim    LIKE movto-estoq.cod-localiz
    FIELD especie-ini        AS INTEGER   /*movto.esp-docto*/
    FIELD especie-fim        AS INTEGER   /*movto.esp-docto*/
    FIELD desc-especie-ini   AS CHAR FORMAT "x(10)":U
    FIELD desc-especie-fim   AS CHAR FORMAT "x(10)":U
    FIELD serie-docto-ini    LIKE movto-estoq.serie-docto
    FIELD serie-docto-fim    LIKE movto-estoq.serie-docto
    FIELD nro-docto-ini      LIKE movto-estoq.nro-docto
    FIELD nro-docto-fim      LIKE movto-estoq.nro-docto
    FIELD tipo-custo         AS INTEGER FORMAT "9"
    FIELD desc-tipo-custo    AS CHAR FORMAT "x(07)" LABEL "Tipo Custo"
    FIELD moeda              AS INTEGER FORMAT "99"
    FIELD desc-moeda         AS CHAR FORMAT "x(15)":U
    FIELD ativo              AS LOGICAL
    FIELD obsol-ord-aut      AS LOGICAL
    FIELD obsol-todas-ordens AS LOGICAL
    FIELD obsol-total        AS LOGICAL
    &if "{&BF_MAT_VERSAO_EMS}" >= "2.062" &then
        FIELD l-imp-param        LIKE movto-estoq.log-1
        FIELD c-unid-neg-ini     AS CHAR
        FIELD c-unid-neg-fim     AS CHAR
    &ENDIF
    FIELD destino            AS INTEGER
    FIELD arquivo            AS CHAR FORMAT "x(35)"
    FIELD usuario            AS CHAR FORMAT "x(12)"
    FIELD data-exec          AS DATE
    FIELD hora-exec          AS INTEGER
    FIELD classifica         AS INTEGER
    FIELD desc-classifica    AS CHAR FORMAT "x(60)" LABEL "Classifica"
    FIELD modelo-rtf         AS CHAR FORMAT "x(35)":U
    FIELD l-habilitaRtf      AS LOG.

DEFINE TEMP-TABLE tt-movto-estoque NO-UNDO
    FIELD it-codigo          LIKE movto-estoq.it-codigo
    FIELD desc-item          LIKE ITEM.desc-item
    FIELD un-item            LIKE ITEM.un
    FIELD cod-obsoleto       LIKE ITEM.cod-obsoleto
    FIELD cod-estabel-item   LIKE ITEM.cod-estabel
    FIELD desc-cod-obsoleto  AS CHAR FORMAT "x(30)":U
    FIELD cod-estabel        LIKE movto-estoq.cod-estabel
    FIELD dt-trans           LIKE movto-estoq.dt-trans
    FIELD cod-depos          LIKE movto-estoq.cod-depos
    FIELD cod-localiz        LIKE movto-estoq.cod-localiz
    FIELD tipo-trans         LIKE movto-estoq.tipo-trans
    FIELD desc-tipo-trans    AS CHAR FORMAT "X(07)":U
    field desc-esp-docto     as char format "x(03)":U
    FIELD quantidade         LIKE movto-estoq.quantidade
    FIELD valor-MAT          AS DECIMAL FORMAT ">>>>,>>>,>>9.9999"
    FIELD valor-MOB          AS DECIMAL FORMAT ">>>>,>>>,>>9.9999"
    FIELD valor-GGF          AS DECIMAL FORMAT ">>>>,>>>,>>9.9999"
    FIELD lote               LIKE movto-estoq.lote
    FIELD cod-refer          LIKE movto-estoq.cod-refer
    FIELD serie-docto        LIKE movto-estoq.serie-docto
    FIELD nro-docto          LIKE movto-estoq.nro-docto
    FIELD ct-codigo		     LIKE movto-estoq.ct-codigo
	FIELD sc-codigo			 LIKE movto-estoq.sc-codigo
    &if "{&BF_MAT_VERSAO_EMS}" >= "2.062" &then
    FIELD cod-unid-negoc     LIKE movto-estoq.cod-unid-negoc
    &ENDIF
    INDEX item-estab it-codigo   ASCENDING
                     cod-estabel ASCENDING
                     dt-trans    ASCENDING
    INDEX item-data  it-codigo   ASCENDING
                     dt-trans    ASCENDING
    INDEX item-estab-dep it-codigo   ASCENDING
                         cod-estabel ASCENDING
                         cod-depos   ASCENDING
                         cod-localiz ASCENDING
                         lote        ASCENDING
                         dt-trans    ASCENDING.

DEFINE TEMP-TABLE tt-digita NO-UNDO
    FIELD esp-docto   LIKE movto-estoq.esp-docto
    FIELD descricao   AS CHARACTER FORMAT "x(40)":U
    FIELD selecionado AS CHARACTER FORMAT "x"
    INDEX id IS PRIMARY esp-docto.



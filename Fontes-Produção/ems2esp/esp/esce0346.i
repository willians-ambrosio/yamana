/********************************************************************************
**
**   CE0346.I - Display do cabecalho
**
********************************************************************************/

def var c-desc-freq            as char format "x(15)" no-undo.
def var c-lb-item              as char format "x(6)"  no-undo.
def var c-lb-narra             as char no-undo.
def var c-lb-un                as char format "x(3)"  no-undo.
def var c-lb-freq              as char format "x(11)"  no-undo.
def var c-lb-ponto-encomenda   as char no-undo.
def var c-lb-desc              as char format "x(9)"  no-undo.
def var c-lb-estabelec         as char format "x(3)"  no-undo.
def var c-lb-familia           as char format "x(7)"  no-undo.
def var c-lb-classif           as char format "x(9)"  no-undo.
def var c-lb-consumo-prev      as char /*format "x(9)"*/  no-undo.
def var c-lb-crit-cc           as char format "x(7)"  no-undo.
def var c-lb-crit-ce           as char format "x(7)"  no-undo.
def var c-lb-dt-ult-ressup     as char format "x(10)" no-undo.
def var c-lb-dt-entrega        as char format "x(10)" no-undo.
def var c-lb-estoq-segur       as char format "x(14)" no-undo.
def var c-lb-estoq-disp        as char format "x(14)" no-undo.
def var c-lb-lote-economi      as char no-undo.
def var c-lb-lote-minimo       as char no-undo.
def var c-lb-lote-multipl      as char no-undo.
def var c-lb-lote-per-max      as char format "x(12)" no-undo.
def var c-lb-tp-ressup         as char format "x(9)"  no-undo.
def var c-lb-ge                as char no-undo.
def var c-lb-ge-aux            as char format "x(2)"  no-undo.
def var c-lb-dt-geracao        as char format "x(09)" no-undo.
def var c-lb-qt-orig           as char format "x(15)" no-undo.
def var c-lb-tp-geracao        as char format "x(10)"   no-undo.
def var c-lb-param             as char no-undo.
def var c-lb-param1             as char no-undo.
def var c-lb-digit             as char no-undo.
def var c-lb-impr              as char no-undo.
def var c-lb-dest              as char no-undo.
def var c-lb-usuar             as char no-undo.
def var c-lb-selec             as char no-undo.
def var c-lb-tit-par           as char no-undo.
def var c-lb-tit-sel           as char no-undo.
def var c-lb-tit-imp           as char no-undo.
def var c-lb-destino           as char no-undo.
def var c-lb-usuario           as char no-undo.
def var c-lb-msg               as char no-undo.
def var c-lb-erro              as char no-undo.
def var c-lb-compr             as char no-undo.
def var c-lb-orig              as char no-undo.
def var c-lb-data              as char no-undo.
def var c-lb-conta             as char no-undo.
def var c-lb-pedi              as char no-undo.
def var c-lb-cdp               as char no-undo.
def var c-lb-tipo              as char no-undo.
def var c-lb-pto-enc           as char format "x(27)" no-undo.
def var c-lb-periodico         as char format "x(21)" no-undo.
def var c-lb-qt-compras        as char format "x(8)" no-undo.
def var c-lb-tempo-ressup      as char format "x(12)" no-undo.
DEF VAR c-format               AS CHAR NO-UNDO.
DEF VAR c-AtinPontoEnco        AS CHAR format "x(27)" NO-UNDO.
DEF VAR c-RompiEstoque         AS CHAR format "x(21)" NO-UNDO.
DEF VAR c-ImpNarrativa         AS CHAR format "x(17)"  NO-UNDO.

def var l-per-cc       as logical init no no-undo.
def var i-per-corrente as integer no-undo.
def var i-ano-corrente as integer no-undo.
def var da-iniper-fech like param-estoq.ult-fech-dia no-undo.
def var da-fimper-fech like param-estoq.ult-fech-dia no-undo.
def var i-mes-aux      as integer no-undo format "99".
def var c-acompanha    as char    NO-UNDO.
def var l-resposta     as logical init no  no-undo.
def var l-primeira     as logical init no  no-undo.
def var l-erro         as logical no-undo.
def var i-seq1         as integer no-undo.
def var c-estabelec    as char format "x(3)" no-undo.
def var de-estoq-segur like item-mat-estab.quant-segur no-undo.
def var c-narrativa    as char   format "x(12)"  no-undo.

def var c-desc-it1            as char format "x(18)" no-undo.
def var c-desc-it2            as char format "x(18)" no-undo.
def var c-classif-abc         as char no-undo.
def var c-tp-ressup           as char format "x(18)" no-undo.
def var i-cont                as i no-undo init 0.
def var i-cont-aux            as i no-undo init 0.
def var c-tp-geracao          as char format "x(10)" no-undo.
def var c-rompimento          as char format "x(11)" no-undo.
def var c-pto-enc             as char format "x(11)" no-undo.
def var c-per-cc              as char format "x(11)" no-undo.
def var de-tempo-ressup       like item-mat-estab.res-cq-comp no-undo.

/* Variaveis auxiliares p/ algoritmos */
def var c-it-codigo         like item.it-codigo            no-undo.
def var c-cod-estabel       like estabelec.cod-estabel     no-undo.
def var de-consumo-prev     like item.consumo-prev         no-undo.
def var i-tipo-est-seg      like item.tipo-est-seg         no-undo.
def var de-quant-segur      like item.quant-segur          no-undo.
def var i-tempo-segur       like item.tempo-segur          no-undo.
def var i-res-int-comp      like item.res-int-comp         no-undo.
def var i-res-for-comp      like item.res-for-comp         no-undo.
def var i-res-cq-comp       like item.res-cq-comp          no-undo.
def var de-pto-enc          like item-mat.ponto-encomenda  no-undo.
def var de-fator-refugo     like item.fator-refugo         no-undo.
def var de-quant-perda      like item.quant-perda          no-undo.
def var de-lote-economi     like item.lote-economi         no-undo.
def var de-lote-minimo      like item.lote-minimo          no-undo.
def var de-lote-multipl     like item.lote-multipl         no-undo.
def var i-cd-freq           like item-mat.cd-freq          no-undo.
def var da-ult-ressup       like item-mat.data-ult-ressup  no-undo.
def var de-lote-max         like item-mat.lote-per-max     no-undo.
def var i-classif-abc       like item.classif-abc          no-undo.
def var i-crit-cc           like item-mat.crit-cc          no-undo.
def var i-crit-ce           like item-mat.crit-ce          no-undo.

&if '{&bf_mat_versao_ems}' >= '2.062' &then 
    /* Variaveis para Calculo e impressao do Valor Previsto da Quantidade a Comprar */
    def var c-imp-tp-custo  as char format "x(13)" no-undo.
    def var c-tp-preco      as char no-undo.

    def var de-preco-u      like item-uni-estab.preco-base no-undo.
    def var de-val-prev     like item-uni-estab.preco-base no-undo.
    def var c-lb-vlr-prev   as char format "x(13)" no-undo.

    {utp/ut-liter.i _Valor_Prev * L}
    assign c-lb-vlr-prev = return-value.
&endif

{utp/ut-liter.i Tempo_Ressup * L}
assign c-lb-tempo-ressup = return-value.
{utp/ut-field.i mgind necessidade-oc qt-orig 1}
assign c-lb-qt-orig = return-value.
{utp/ut-field.i mgind necessidade-oc tp-geracao 2}
assign c-lb-tp-geracao = return-value.
{utp/ut-liter.i Estoque_Segur * L}
assign c-lb-estoq-segur = return-value.
{utp/ut-field.i mgind necessidade-oc estoque-dispo 2}
assign c-lb-estoq-disp = return-value.
{utp/ut-field.i mgind necessidade-oc qt-compras 2}
assign c-lb-qt-compras = return-value.
{utp/ut-field.i mgind necessidade-oc data-geracao 1}
assign c-lb-dt-geracao = return-value.
{utp/ut-field.i mgind necessidade-oc data-entrega 1}
assign c-lb-dt-entrega = return-value.
{utp/ut-field.i mgind item it-codigo 1}
assign c-lb-item = return-value.
{utp/ut-field.i mgind item un 2}
assign c-lb-un = return-value.
{utp/ut-field.i mgind item-mat-estab cd-freq 1}
assign c-lb-freq = return-value.
{utp/ut-field.i mgind item ge-codigo 1}
assign c-lb-ge = return-value.
{utp/ut-field.i mgind item ge-codigo 2}
assign c-lb-ge-aux = return-value.
{utp/ut-field.i mgind item desc-item 1}
assign c-lb-desc = return-value.
{utp/ut-field.i mgadm estabelec cod-estabel 2}
assign c-lb-estabelec = return-value.
{utp/ut-field.i mgind familia fm-codigo 1}
assign c-lb-familia = return-value.
{utp/ut-field.i mgind item classif-abc 2}
assign c-lb-classif = return-value.
{utp/ut-field.i mgind item-mat-estab consumo-prev 2}
assign c-lb-consumo-prev = return-value.
{utp/ut-field.i mgind item-mat-estab crit-cc 2}
assign c-lb-crit-cc = return-value.
{utp/ut-field.i mgind item-mat-estab crit-ce 2}
assign c-lb-crit-ce = return-value.
{utp/ut-field.i mgind item-mat-estab data-ult-ressup 2}
assign c-lb-dt-ult-ressup = return-value.
{utp/ut-field.i mgind item-mat-estab lote-economi 2}
assign c-lb-lote-economi = return-value.
{utp/ut-field.i mgind item-mat-estab lote-minimo 2}
assign c-lb-lote-minimo = return-value.
{utp/ut-field.i mgind item-mat-estab lote-multipl 2}
assign c-lb-lote-multipl = return-value.
{utp/ut-field.i mgind item-mat-estab lote-per-max 2}
assign c-lb-lote-per-max = return-value.
{utp/ut-field.i mgind item-mat-estab ponto-encomenda 2}
assign c-lb-ponto-encomenda = return-value.
{utp/ut-field.i mgind item-mat-estab tp-ressup 2}
assign c-lb-tp-ressup = return-value.

{utp/ut-field.i mgind item narrativa 1}
assign c-lb-narra = trim(return-value) + ": ".
{utp/ut-liter.i Rompimento * L}
assign c-rompimento = return-value.
{utp/ut-field.i mgind item-mat-estab ponto-encomenda 2}
assign c-pto-enc = return-value.
{utp/ut-liter.i Per°odo_CC * L}
assign c-per-cc = return-value.


{utp/ut-liter.i SELEÄ«O * r}
assign c-lb-selec = trim(return-value).
{utp/ut-liter.i PAR∂METROS_RELAT‡RIO * r}
assign c-lb-param = trim(return-value).
{utp/ut-liter.i Considerar_Ressuprimento * L}
assign c-lb-param1 = return-value.
{utp/ut-liter.i DIGITAÄ«O * r}
assign c-lb-digit = trim(return-value).
{utp/ut-liter.i IMPRESS«O * r}
assign c-lb-impr = trim(return-value).
{utp/ut-liter.i Destino * r}
assign c-lb-dest = trim(return-value).
{utp/ut-liter.i Usu†rio * r}
assign c-lb-usuar = trim(return-value).
{utp/ut-liter.i Msg * r}
assign c-lb-msg = trim(return-value).
{utp/ut-liter.i Erro * r}
assign c-lb-erro = trim(return-value).
{utp/ut-field.i mgind item narrativa 1}
assign c-narrativa = trim(return-value) + ':'.

&if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
   DEFINE VARIABLE cAuxTraducao001 AS CHARACTER NO-UNDO.
   ASSIGN cAuxTraducao001 = {ininc/i01in658.i 04 1}.
   run utp/ut-liter.p (INPUT REPLACE(TRIM(cAuxTraducao001)," ","_"),
                       INPUT "",
                       INPUT "").
    ASSIGN c-lb-pto-enc   = RETURN-VALUE.
&else
   ASSIGN c-lb-pto-enc   = {ininc/i01in658.i 04 1}.
&endif

&if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
   DEFINE VARIABLE cAuxTraducao002 AS CHARACTER NO-UNDO.
   ASSIGN cAuxTraducao002 = {ininc/i01in658.i 04 3}.
   run utp/ut-liter.p (INPUT REPLACE(TRIM(cAuxTraducao002)," ","_"),
                       INPUT "",
                       INPUT "").
    ASSIGN c-lb-periodico   = RETURN-VALUE.
&else
   ASSIGN c-lb-periodico   = {ininc/i01in658.i 04 3}.
&endif

{utp/ut-liter.i Sim/N∆o * L}
assign c-format = return-value.

form
     skip(1)
     estabelec.cod-estabel "-"
     estabelec.nome no-label skip(1)
     with stream-io no-box side-label width 132 frame f1-est. 
run utp/ut-trfrrp.p (input frame f1-est:handle).
     
form
     skip(1)
     grup-estoque.ge-codigo "-"
     grup-estoque.descricao no-label skip(1)
     with stream-io no-box side-label width 132 frame f1-ge.
run utp/ut-trfrrp.p (input frame f1-ge:handle).

form
     skip(1)
     familia.fm-codigo "-"
     familia.descricao no-label skip(1)
     with stream-io no-box side-label width 132 frame f1-fam.
run utp/ut-trfrrp.p (input frame f1-fam:handle).

{include/tt-edit.i}

form c-lb-narra         at 1  format "x(10)" 
     tt-editor.conteudo at 12 format "x(80)"
     with stream-io no-box no-label width 132 frame f-narrativa.
run utp/ut-trfrrp.p (input frame f-narrativa:handle).


{include/pi-edit.i}

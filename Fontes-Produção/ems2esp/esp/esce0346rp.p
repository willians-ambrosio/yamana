/******************************************************************************
**
**       PROGRAMA: ESCE0346RP.P
**       DATA....: Marco de 2011
**       OBJETIVO: Resusprimento do Estoque - Xtivity
**       Autor: Fabio Gentile Amorim Antonio - Kraft Consulting
**       VERSAO..: 1.00.000
** 
******************************************************************************/

{include/i-prgvrs.i ESCE0346RP 2.06.00.002}  /*** 010005 ***/
{include/i_fnctrad.i}
{include/i-rpvar.i}
{include/i-rpcab.i}

{cdp/cdcfgmat.i}

def temp-table tt-param
    field destino       as integer
    field arquivo       as char
    field usuario       as char
    field data-exec     as date
    field hora-exec     as integer
    field ge-ini        as integer
    field ge-fim        as integer
    field familia-ini   as char
    field familia-fim   as char
    field item-ini      as char
    field item-fim      as char
    field estabelec-ini as char
    field estabelec-fim as char
    field pto-enc    like estabelec.usa-mensal
    field periodico  like estabelec.usa-mensal
    field c-classe   as char format "x(40)"
    field c-destino  as char
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field narrativa  like estabelec.usa-mensal
 /**
&if '{&bf_mat_versao_ems}' >= '2.062' &then 
    field ind-preco    as integer
&endif    
**/
    .

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.
    
def temp-table tt-raw-digita
    field raw-digita as raw.

DEF VAR h_acomp_rp               AS HANDLE                             NO-UNDO.
DEF VAR ct                       AS INT                                NO-UNDO.
DEF VAR d-dias-aux               AS INT                                NO-UNDO.
DEF VAR d-data-entrega-ini       AS DATE                               NO-UNDO.
DEF VAR d-qt-saldo               AS DECIMAL FORMAT "->>>,>>>,>>9.99"   NO-UNDO.
DEF VAR d-qt-saldo-30            AS DECIMAL FORMAT "->>>,>>>,>>9.99"   NO-UNDO.
DEF VAR d-qt-saldo-60            AS DECIMAL FORMAT "->>>,>>>,>>9.99"   NO-UNDO.
DEF VAR de-reservas              AS DECIMAL FORMAT "->>>>,>>>,>>9.99"  NO-UNDO.

DEF VAR de-qtde-orig             AS DECIMAL FORMAT "->>>>,>>>,>>9.9999"  NO-UNDO.

{esp/esce0346.i}

{utp/ut-liter.i Sim/NÆo}
ASSIGN c-format = RETURN-VALUE.

form
    c-AtinPontoEnco at 05 ":" at 32 tt-param.pto-enc   FORMAT "Sim/NÆo" SKIP
    c-RompiEstoque  at 11 ":" at 32 tt-param.periodico FORMAT "Sim/NÆo" SKIP
    c-ImpNarrativa  at 15 ":" at 32 tt-param.narrativa FORMAT "Sim/NÆo" 
  /*
  &if '{&bf_mat_versao_ems}' >= '2.062' &then 
    c-imp-tp-custo  at 19 ":" at 32 c-tp-preco         FORMAT "x(20)"
  &endif
  */
    skip(1)
    with width 132 no-box NO-LABELS stream-io frame f-imp-param.

RUN utp/ut-trfrrp.p (input Frame f-imp-param:Handle).

{utp/ut-liter.i Atingimento_Ponto_Encomenda}
ASSIGN c-AtinPontoEnco = RETURN-VALUE.
{utp/ut-liter.i Rompimento_de_Estoque}       
ASSIGN c-RompiEstoque =  RETURN-VALUE.
{utp/ut-liter.i Imprime}
ASSIGN c-ImpNarrativa = RETURN-VALUE.
{utp/ut-liter.i Narrativa}
ASSIGN c-ImpNarrativa = c-ImpNarrativa + " ":U + RETURN-VALUE.

ASSIGN tt-param.pto-enc:FORMAT IN FRAME f-imp-param   = c-format
       tt-param.periodico:FORMAT IN FRAME f-imp-param = c-format 
       tt-param.narrativa:FORMAT IN FRAME f-imp-param = c-format.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

def var h-acomp as handle no-undo.

find first param-estoq  no-lock no-error.
find first param-global no-lock no-error.
find first param-compra no-lock no-error.
run cdp/cdapi005.p (input  param-estoq.ult-per-fech,
                    output da-iniper-x,
                    output da-fimper-x,
                    output i-per-corrente,
                    output i-ano-corrente,
                    output da-iniper-fech,
                    output da-fimper-fech).

assign c-programa = "CE/0346"
       c-versao   = "1.00"
       c-revisao  = "000"
       i-mes-aux  = month(da-fimper-x)
       i-numper-x = i-mes-aux
       c-empresa  = param-global.grupo.

find first tt-param no-lock no-error.
/*
&if '{&bf_mat_versao_ems}' >= '2.062' &then 
    {utp/ut-liter.i Tipo_de_Custo *L}
    ASSIGN c-Imp-tp-custo = RETURN-VALUE.
    CASE tt-param.ind-preco:
        WHEN 1 THEN DO:
            {utp/ut-liter.i Mensal}
            ASSIGN c-tp-preco = RETURN-VALUE.
        END.
        WHEN 2 THEN DO:
            {utp/ut-liter.i On_-_line}
            ASSIGN c-tp-preco = RETURN-VALUE.
        END.
        WHEN 3 THEN DO:
            {utp/ut-liter.i Ultima_Entrada}
            ASSIGN c-tp-preco = RETURN-VALUE.
        END.
        WHEN 4 THEN DO:
            {utp/ut-liter.i Reposi‡Æo}
            ASSIGN c-tp-preco = RETURN-VALUE.
        END.
        WHEN 5 THEN DO:
            {utp/ut-liter.i Base}
            ASSIGN c-tp-preco = RETURN-VALUE.
        END.
    END CASE.
&endif
*/
DEF VAR c-fill AS c FORMAT "x(132)" NO-UNDO.
ASSIGN
   c-fill = FILL("-",132).

form HEADER 
     c-lb-item            space(11)
     c-lb-classif
     c-lb-un 
     c-lb-dt-ult-ressup   space(2)
     c-lb-freq space(17)
     c-lb-lote-economi    space(14)
     c-lb-ponto-encomenda space(9)
     c-lb-estoq-disp
     c-lb-crit-cc skip    
     space(19)      
     c-lb-tp-geracao      space(2)
     c-lb-dt-geracao      space(3)
     c-lb-qt-orig         space(13)
     c-lb-lote-minimo     space(8)
     c-lb-estoq-segur     space(11)     
     c-lb-qt-compras      space(5)
     c-lb-crit-ce         skip 
     space(19)
     c-lb-estabelec       space(9)
     c-lb-dt-entrega      space(5)
     c-lb-tempo-ressup    space(13)
     c-lb-lote-multipl    space(9)
     c-lb-lote-per-max    space(8)
     c-lb-consumo-prev    SKIP
     "                                              Tempo Ressup (Xtivity)                         Pto Enc (Xtivity)" skip
     "                                           Quantidade Orig (Xtivity)                     Estoque Seg (Xtivity)" skip
     c-fill
     with stream-io no-box no-label width 132 frame f-cab-nec PAGE-TOP.


form necessidade-oc.it-codigo
     c-classif-abc                     space(2)
     item.un                           space(2)
     da-ult-ressup
     i-cd-freq " -  "
     c-desc-freq
     de-lote-economi
     de-pto-enc
     necessidade-oc.estoque-dispo      space(13)
     i-crit-cc
     c-desc-it1
     c-tp-geracao                      space(2)
     necessidade-oc.data-geracao
     necessidade-oc.qt-orig            space(8)
     de-lote-minimo      space(8)
     de-estoq-segur                    space(2)           
     necessidade-oc.qt-compras         space(13)
     i-crit-ce          skip 
     c-desc-it2                        
     necessidade-oc.cod-estabel      space(9)
     necessidade-oc.data-entrega       space(13)
     de-tempo-ressup                   space(8)
     de-lote-multipl     space(5)
     de-lote-max     space(5)
     de-consumo-prev skip
     log-ext-item-uni-estab01.tempo-ressuprimento to 058
     log-ext-item-uni-estab01.ponto-encomenda     to 100 skip
     de-qtde-orig                                 to 058
     log-ext-item-uni-estab01.estoque-seguranca   to 100 skip
     with stream-io no-box no-label width 132 frame f-corpo-nec.


{include/i-rpout.i}

{utp/ut-liter.i Ressuprimento_Estoques * L}
assign c-titulo-relat = trim(return-value).
{utp/ut-liter.i ESTOQUE * L}
assign c-sistema = trim(return-value).

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Ressuprimento_Estoque * L}
run pi-inicializar in h-acomp (input return-value).

view frame f-cabper.
VIEW FRAME f-cab-nec.
view frame f-rodape.

DO  ON STOP UNDO, LEAVE:

    for each  estabelec fields (cod-estabel nome) no-lock 
        where estabelec.cod-estabel >= tt-param.estabelec-ini 
        and   estabelec.cod-estabel <= tt-param.estabelec-fim,
        each  necessidade-oc no-lock 
        where necessidade-oc.cod-estabel = estabelec.cod-estabel 
        and   necessidade-oc.it-codigo  >= tt-param.item-ini     
        and   necessidade-oc.it-codigo  <= tt-param.item-fim,
        first item no-lock 
        where item.it-codigo = necessidade-oc.it-codigo:

        run pi-acompanhar in h-acomp (subst("Estabelecimento: &1 / Item: &2",estabelec.cod-estabel,item.it-codigo)).

        if  item.ge-codigo < tt-param.ge-ini or
            item.ge-codigo > tt-param.ge-fim or
            item.fm-codigo < tt-param.familia-ini or
            item.fm-codigo > tt-param.familia-fim then next.

        if (necessidade-oc.tp-geracao = 1 and tt-param.pto-enc) or
           (necessidade-oc.tp-geracao = 2 and tt-param.periodico) or
           (necessidade-oc.tp-geracao = 3 and tt-param.periodico) then do:

            assign de-qtde-orig = 0.

            find first item-uni-estab no-lock 
                 where item-uni-estab.it-codigo   = necessidade-oc.it-codigo 
                 and   item-uni-estab.cod-estabel = necessidade-oc.cod-estabel no-error.

            if  not avail item-uni-estab then do:

                assign i-classif-abc   = item.classif-abc
                       i-res-int-comp  = item.res-int-comp
                       i-res-for-comp  = item.res-for-comp
                       i-res-cq-comp   = item.res-cq-comp
                       i-tipo-est-seg  = item.tipo-est-seg
                       de-quant-segur  = item.quant-segur
                       i-tempo-segur   = item.tempo-segur
                       de-lote-economi = item.lote-economi
                       de-lote-multipl = item.lote-multipl
                       de-lote-minimo  = item.lote-minimo.

                for first item-mat no-lock where item-mat.it-codigo = item.it-codigo:
                    assign i-cd-freq     = item-mat.cd-freq
                           da-ult-ressup = item-mat.data-ult-ressup
                           de-pto-enc    = item-mat.ponto-encomenda
                           i-crit-cc     = item-mat.crit-cc
                           i-crit-ce     = item-mat.crit-ce
                           de-lote-max   = item-mat.lote-per-max.
                end.

            end.
            else
                assign i-cd-freq       = item-uni-estab.cd-freq
                       i-classif-abc   = item-uni-estab.classif-abc
                       i-res-int-comp  = item-uni-estab.res-int-comp
                       i-res-for-comp  = item-uni-estab.res-for-comp
                       i-res-cq-comp   = item-uni-estab.res-cq-comp
                       i-tipo-est-seg  = item-uni-estab.tipo-est-seg
                       de-quant-segur  = item-uni-estab.quant-segur
                       i-tempo-segur   = item-uni-estab.tempo-segur
                       da-ult-ressup   = item-uni-estab.data-ult-ressup
                       de-lote-economi = item-uni-estab.lote-economi
                       de-lote-multipl = item-uni-estab.lote-multipl
                       de-pto-enc      = item-uni-estab.ponto-encomenda
                       de-lote-minimo  = item-uni-estab.lote-minimo
                       i-crit-cc       = item-uni-estab.crit-cc
                       i-crit-ce       = item-uni-estab.crit-ce
                       de-lote-max     = item-uni-estab.lote-per-max.

            assign de-consumo-prev = necessidade-oc.dec-1.

            for first frequencia fields (cd-freq descricao) no-lock 
                where frequencia.cd-freq = i-cd-freq:
            end.

            assign de-tempo-ressup = 0
                   c-classif-abc   = {ininc/i03in172.i 04 i-classif-abc}
                   c-desc-it1      = item.descricao-1
                   c-desc-it2      = item.descricao-2
                   de-tempo-ressup = (i-res-int-comp + i-res-for-comp)
                   de-estoq-segur  = (if i-tipo-est-seg = 1 then de-quant-segur
                                      else i-tempo-segur  * de-consumo-prev / 30).
            assign c-tp-geracao    = if necessidade-oc.tp-geracao = 1 then c-pto-enc
                                     else if necessidade-oc.tp-geracao = 2 then c-per-cc
                                     else c-rompimento.
            assign c-desc-freq     = if avail frequencia then frequencia.descricao else "".

            disp necessidade-oc.it-codigo
                 necessidade-oc.cod-estabel
                 de-tempo-ressup
                 c-classif-abc 
                 item.un 
                 da-ult-ressup
                 i-cd-freq 
                 c-desc-freq
                 de-lote-economi
                 de-pto-enc
                 necessidade-oc.estoque-dispo
                 necessidade-oc.qt-compras
                 c-desc-it1
                 necessidade-oc.data-geracao
                 de-lote-minimo
                 necessidade-oc.qt-orig 
                 de-estoq-segur
                 necessidade-oc.data-entrega
                 i-crit-cc
                 i-crit-ce
                 de-lote-max
                 c-desc-it2
                 de-consumo-prev
                 de-lote-multipl
                 c-tp-geracao
                 with frame f-corpo-nec.

            for first log-ext-item-uni-estab01 no-lock 
                where log-ext-item-uni-estab01.cod-estabel = estabelec.cod-estabel 
                and   log-ext-item-uni-estab01.it-codigo   = item.it-codigo
                with frame f-corpo-nec:
                assign de-qtde-orig = log-ext-item-uni-estab01.ponto-encomenda - ( necessidade-oc.estoque-dispo + necessidade-oc.qt-compras ).
                display log-ext-item-uni-estab01.ponto-encomenda     
                        log-ext-item-uni-estab01.estoque-seguranca
                        de-qtde-orig
                        log-ext-item-uni-estab01.tempo-ressuprimento skip(1).
            end.

            down with frame f-corpo-nec.

        end.
        
    END.

    PAGE.
    HIDE FRAME f-cab-nec.
    {utp/ut-liter.i Imprime_Narrativa * L}
    assign c-narrativa = return-value.
    put unformatted 
        c-lb-param skip(1).
    put c-lb-pto-enc   at 5  ": " tt-param.pto-enc .
    put c-lb-periodico at 11 ": " tt-param.periodico.
    
    put unformatted
        c-lb-selec skip(1)         
        c-lb-ge               at 19  ":"
        tt-param.ge-ini       at 34 "|<  >| " at 51 tt-param.ge-fim
        c-lb-familia          at 25  ":"
        tt-param.familia-ini  at 34 "|<  >| " at 51 tt-param.familia-fim
        c-lb-item             at 28  ":"
        tt-param.item-ini     at 34 "|<  >| " at 51 tt-param.item-fim skip(1)
        c-lb-impr          skip(1)
        c-lb-dest             at 25  ": " tt-param.c-destino " - " tt-param.arquivo
        c-lb-usuar            at 25  ": " tt-param.usuario skip(1).

END.
{include/i-rpclo.i}
run pi-finalizar in h-acomp.

RETURN "OK":U.

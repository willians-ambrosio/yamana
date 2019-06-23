/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
********************************************************************************/
{include/i-prgvrs.i ESCC0406D 2.00.00.045 } /*** 010045 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ESCC0406d MCC}
&ENDIF

/***************************************************************************
**
**   ESCC0406D.P - relatorio de pedidos emitidos ordem de comprador.
**
***************************************************************************/
{cdp/cdcfgmat.i}
/* Variaveis e temp-tables comuns */
{ccp/ESCC0406.i5}

{cep/ce1234.i} /* Valida‡Æo de Decimais */

def input param raw-param as raw no-undo.

def buffer b-emitente for emitente.

def var c-quebra   like comprador.cod-comprado.
def var l-prim     as logical init yes.

DEFINE VARIABLE de-vl-descto  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-vl-total   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-preco-orig AS DECIMAL     NO-UNDO.

form c-lb-compr format "x(9)" ": "
     comprador.cod-comprado " - " comprador.nome skip(1)
     with stream-io no-box no-label width 132 frame f-quebra.

{include/i-rpvar.i}

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Pedidos_Emitidos *}
run pi-inicializar in h-acomp (input  Return-value ).

create tt-param.
raw-transfer raw-param to tt-param.

find first param-global no-lock no-error.
if  avail param-global then
    assign c-empresa = grupo.

assign c-programa  = "CC/0406"
       c-versao    = "1.00"
       c-revisao   = "000"
       l-imprimiu  = no
       c-quebra    = ""
       de-tot-ped  = 0
       de-tot-liq  = 0
       de-tot-ipi  = 0
       i-nr-pedido = 0
       i-narrativa = 0
       de-qt-tot-ped = 0.

{utp/ut-liter.i COMPRAS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Pedidos_Emitidos_de * r}
assign c-titulo-relat = trim(return-value) + " "
                      + string(tt-param.da-iniper, "99/99/9999") + " ".
{utp/ut-liter.i a * r}
assign c-titulo-relat = c-titulo-relat
                      + trim(return-value) + " "
                      + string(tt-param.da-fimper,"99/99/9999").

{include/i-rpcab.i}                      

for each pedido-compr
    where pedido-compr.num-pedido   >= tt-param.i-pedido-i
    and   pedido-compr.num-pedido   <= tt-param.i-pedido-f
    and   pedido-compr.data-pedido  >= tt-param.da-iniper
    and   pedido-compr.data-pedido  <= tt-param.da-fimper
    and   pedido-compr.cod-emitente >= tt-param.i-fornec-i
    and   pedido-compr.cod-emitente <= tt-param.i-fornec-f
    and   pedido-compr.situacao     <> 3 no-lock,
    each emitente
        where emitente.cod-emitente = pedido-compr.cod-emitente no-lock,
    each ordem-compra
        where ordem-compra.num-pedido    = pedido-compr.num-pedido
        and   ordem-compra.cod-estabel  >= tt-param.c-estabel-i
        and   ordem-compra.cod-estabel  <= tt-param.c-estabel-f
        and   ordem-compra.cod-comprado >= tt-param.c-comp-i
        and   ordem-compra.cod-comprado <= tt-param.c-comp-f
        and ((ordem-compra.situacao    <> 4 and tt-param.l-receb)
        or    ordem-compra.situacao     = 2) no-lock
    break by ordem-compra.cod-comprado
          by ordem-compra.num-pedido
          by ordem-compra.numero-ordem:

    if l-emergencial = yes and pedido-compr.emergencial = no then next.

    if  tt-param.l-alter
    and ordem-compra.nr-alt-preco = 0 then next.

    run pi-acompanhar in h-acomp (input ordem-compra.it-codigo).

    assign l-imp-ped = first-of(ordem-compr.num-pedido).
    find item
        where item.it-codigo = ordem-compra.it-codigo no-lock no-error.

    if avail item then
        assign c-desc-item = item.desc-item.

    &if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
        DEFINE VARIABLE cAuxTraducao001 AS CHARACTER NO-UNDO.
        ASSIGN cAuxTraducao001 = {ininc/i01in274.i 04 ordem-compra.natureza}.
        run utp/ut-liter.p (INPUT REPLACE(TRIM(cAuxTraducao001)," ","_"),
                            INPUT "",
                            INPUT "").
        ASSIGN  c-natureza = RETURN-VALUE.
    &else
        ASSIGN c-natureza = {ininc/i01in274.i 04 ordem-compra.natureza}.
    &endif

    find moeda where moeda.mo-codigo = tt-param.i-moeda no-lock no-error.
    assign tt-param.c-moeda = moeda.descricao.

    run cdp/cd0812.p (input ordem-compra.mo-codigo,
                      input tt-param.i-moeda,
                      input ordem-compra.preco-unit,
                      input pedido-compr.data-pedido,
                      output de-preco-conv).
    if  de-preco-conv = ? then do:
        assign de-preco-conv = ordem-compra.preco-unit.
        find moeda where moeda.mo-codigo = ordem-compra.mo-codigo no-lock no-error.
        assign tt-param.c-moeda = moeda.descricao.
    end.
    assign l-imp-ord = yes.
    for each prazo-compra
        where prazo-compra.numero-ordem = ordem-compra.numero-ordem
        and ((prazo-compra.situacao    <> 4 and tt-param.l-receb)
        or    prazo-compra.situacao     = 2) no-lock:

        assign i-narrativa = 0.
        if  ordem-compra.cod-comprado <> c-quebra
        or  line-counter > 62
        or  l-prim = yes then do:
            if  l-prim = yes then
                assign l-prim = no.
            page.
            find comprador
                where comprador.cod-comprado = ordem-compra.cod-comprado
                no-lock no-error.
            if  avail comprador then do:
                disp c-lb-compr
                     comprador.cod-comprado
                     comprador.nome with stream-io frame f-quebra.
                assign c-quebra = comprador.cod-comprado.
            end.
            disp c-lb-pedido  c-lb-dt-ped
                 c-lb-nat     c-lb-fornec
                 c-lb-cond    c-lb-ordem
                 c-lb-est     c-lb-item
                 c-lb-descr   c-lb-un
                 c-lb-preco   c-lb-moeda
                 c-lb-alt     
                 &if defined (bf_mat_oper_triangular) &then 
                     c-lb-emit-ent    
                 &endif
                 /*Unidade de Neg¢cio*/
                 &IF '{&bf_mat_versao_ems}' >= '2.062' &THEN
                 c-lb-unineg
                 &ENDIF
                 c-lb-pa
                 c-lb-sit     c-lb-dt-emis
                 c-lb-dt-entr c-lb-qtd
                 c-lb-saldo   c-lb-pr-merc
                 c-lb-valor   c-lb-total
                 c-lb-cliente c-lb-seq
                 c-lb-atraso
                 with stream-io frame f-cab-corpo.
        end.
        repeat:
            if  l-preco-bruto = no then
                assign de-valor-ipi = (de-preco-conv
                                    * ordem-compra.aliquota-ipi)
                                    / (100 + ordem-compra.aliquota-ipi).
            else do:
                if  ordem-compra.perc-descto > 0 then
                    assign de-val-orig = (de-preco-conv * 100)
                                       / (100 - ordem-compra.perc-descto).
                else
                    assign de-val-orig = de-preco-conv.

                assign de-valor-ipi = (de-val-orig
                                    * ordem-compra.aliquota-ipi)
                                    / (100 + ordem-compra.aliquota-ipi).
            end.
            assign de-preco-unit   = de-preco-conv - de-valor-ipi.                  

            /*** fn_ajust_dec => fun‡Æo definida no include ce1234.i, 
                 utilizada para valida‡Æo de decimais ***/
            assign de-preco-merc   = fn_ajust_dec((prazo-compra.quantidade * de-preco-unit), 
                                     tt-param.i-moeda).                                        
            assign de-valor-ipi    = fn_ajust_dec(round((prazo-compra.quantidade * de-valor-ipi),5), 
                                     tt-param.i-moeda).                   
            run cdp/cd0812.p (input ordem-compra.mo-codigo,   
                                  input tt-param.i-moeda,         
                                  input ordem-compra.preco-fornec,  
                                  input ordem-compra.data-cotacao,
                                  output de-preco-orig). 

            ASSIGN de-vl-total = de-preco-orig * prazo-compra.quantidade.

            IF de-vl-total >= de-valor-ipi THEN
 
                ASSIGN de-valor-descto = (de-vl-total * ordem-compra.perc-descto) / 100.

/*                 assign de-valor-descto = fn_ajust_dec((prazo-compra.quantidade * ordem-compra.valor-descto), */
/*                                          tt-param.i-moeda).                                                  */
            assign de-preco-total  = de-preco-merc + de-valor-ipi.     

            find first recebimento use-index ordem
                where recebimento.numero-ordem = prazo-compra.numero-ordem
                and   recebimento.parcela      = prazo-compra.parcela
                no-lock no-error.
            if  avail recebimento and ordem-compra.situacao = 6 then
                assign i-atraso = recebimento.data-movto
                                - prazo-compra.data-entrega.
            else
                assign i-atraso = today - prazo-compra.data-entrega.

            if  l-imp-ped or l-imp-ord then do:
                if  tt-param.l-narrativa = yes then do:
                    /* Verifica o Nœmero de Linhas da Narrativa */
                    {ccp/ESCC0406.i6}
                end.
            end.

            if  l-imp-ped then do:
                if  line-counter > 61 - i-narrativa then do:
                    page.
                    if  avail comprador then
                        disp c-lb-compr
                             comprador.cod-comprado
                             comprador.nome with stream-io frame f-quebra.
                    disp c-lb-pedido  c-lb-dt-ped
                         c-lb-nat     c-lb-fornec
                         c-lb-cond    c-lb-ordem
                         c-lb-est     c-lb-item
                         c-lb-descr   c-lb-un
                         c-lb-preco   c-lb-moeda
                         c-lb-alt     
                         &if defined (bf_mat_oper_triangular) &then 
                            c-lb-emit-ent    
                         &endif
                         /*Unidade de Neg¢cio*/
                         &IF '{&bf_mat_versao_ems}' >= '2.062' &THEN
                         c-lb-unineg
                         &ENDIF
                         c-lb-pa
                         c-lb-sit     c-lb-dt-emis
                         c-lb-dt-entr c-lb-qtd
                         c-lb-saldo   c-lb-pr-merc
                         c-lb-valor   c-lb-total
                         c-lb-cliente c-lb-seq
                         c-lb-atraso
                         with stream-io frame f-cab-corpo.
                end.
                put pedido-compr.num-pedido   at 1
                    pedido-compr.data-pedido  at 11
                    c-natureza                at 22
                    emitente.nome-abrev       at 32
                    pedido-compr.cod-cond-pag at 45.
                assign l-imp-ped  = no
                       l-imprimiu = yes.
            end.
            if  l-imp-ord then do:
                if  line-counter > 61 - i-narrativa then do:
                    page.
                    if  avail comprador then
                        disp c-lb-compr
                             comprador.cod-comprado
                             comprador.nome with stream-io frame f-quebra.
                    disp c-lb-pedido  c-lb-dt-ped
                         c-lb-nat     c-lb-fornec
                         c-lb-cond    c-lb-ordem
                         c-lb-est     c-lb-item
                         c-lb-descr   c-lb-un
                         c-lb-preco   c-lb-moeda
                         c-lb-alt     
                         &if defined (bf_mat_oper_triangular) &then 
                            c-lb-emit-ent    
                         &endif
                         /*Unidade de Neg¢cio*/
                         &IF '{&bf_mat_versao_ems}' >= '2.062' &THEN
                         c-lb-unineg
                         &ENDIF
                         c-lb-pa
                         c-lb-sit     c-lb-dt-emis
                         c-lb-dt-entr c-lb-qtd
                         c-lb-saldo   c-lb-pr-merc
                         c-lb-valor   c-lb-total
                         c-lb-cliente c-lb-seq
                         c-lb-atraso
                         with stream-io frame f-cab-corpo.
                    assign l-imp-ped = yes.
                    next.
                end.
                put ordem-compra.numero-ordem  at 49
                    ordem-compra.cod-estabel   at 59.
                if  avail item then
                    put item.it-codigo         at 65
                        c-desc-item         at 82 format "x(51)" skip.
                /*******************************************/
                &if defined (bf_mat_oper_triangular) &then                      
                    if pedido-compr.cod-emit-terc <> 0 then 
                        for first b-emitente fields(nome-abrev) 
                             where b-emitente.cod-emitente = pedido-compr.cod-emit-terc no-lock:
                                assign c-nome-abrev = b-emitente.nome-abrev.
                        end.
                    else assign c-nome-abrev = "".                                                                                                                                                                                                                                                                                                                                                        
                &endif
                /*******************************************/        
                put prazo-compra.un            at 1
                    de-preco-unit              at 4
                    tt-param.c-moeda           at 23
                    prazo-compra.cod-alt       at 32.
                &if defined (bf_mat_oper_triangular) &then
                    put c-nome-abrev            at 36. 
                &endif     
                /*Unidade de Neg¢cio*/
                &IF '{&bf_mat_versao_ems}' >= '2.062' &THEN
                    put ordem-compra.cod-unid-negoc at 49.
                &ENDIF
                put skip.    

                if  tt-param.l-narrativa = yes then do:
                    /* Imprime a Narrativa */
                    {ccp/ESCC0406.i7}
                end.
                assign l-imp-ord = no.
            end.
            if  line-counter > 62 then do:
                page.
                disp c-lb-pedido  c-lb-dt-ped
                     c-lb-nat     c-lb-fornec
                     c-lb-cond    c-lb-ordem
                     c-lb-est     c-lb-item
                     c-lb-descr   c-lb-un
                     c-lb-preco   c-lb-moeda
                     c-lb-alt     
                     &if defined (bf_mat_oper_triangular) &then 
                         c-lb-emit-ent    
                     &endif
                     /*Unidade de Neg¢cio*/
                     &IF '{&bf_mat_versao_ems}' >= '2.062' &THEN
                     c-lb-unineg
                     &ENDIF
                     c-lb-pa
                     c-lb-sit     c-lb-dt-emis
                     c-lb-dt-entr c-lb-qtd
                     c-lb-saldo   c-lb-pr-merc
                     c-lb-valor   c-lb-total
                     c-lb-cliente c-lb-seq
                     c-lb-atraso
                     with stream-io frame f-cab-corpo.
                assign l-imp-ped = yes
                       l-imp-ord = yes.
                next.
            end.
            else leave.
        end.
        if  prazo-compra.situacao = 2 then do:
            {utp/ut-liter.i Conf * r}
        end.
        else do:
            {utp/ut-liter.i Rece * r}
        end.
        assign c-sit = trim(return-value).

        {ccp/ESCC0406.i} /* put da linha da parcela */
        assign de-tot-ped    = de-tot-ped    + de-preco-total
               de-tot-liq    = de-tot-liq    + de-preco-merc
               de-tot-ipi    = de-tot-ipi    + de-valor-ipi
               de-tot-descto = de-tot-descto + de-valor-descto
               de-qt-tot-ped = de-qt-tot-ped + prazo-compra.quantidade.
    end.

    /* Integracao Modulo Importacao */
    if param-global.modulo-07 then
        if tt-param.l-despesas and last-of(ordem-compra.num-pedido) then do:
            run imp/im9035.p (input pedido-compr.num-pedido,
                              input ?,
                              input ?,
                              input tt-param.i-moeda,
                              input tt-param.i-despesas-pag,
                              input tt-param.l-despesas-inc,
                              output de-val-desp).
            if tt-param.l-despesas-inc then
                assign de-tot-ped = de-tot-ped + de-val-desp.                              
        end.    

    if  l-imprimiu then do:
        if  line-counter > 60 then do:
            page.
            disp c-lb-pedido  c-lb-dt-ped
                 c-lb-nat     c-lb-fornec
                 c-lb-cond    c-lb-ordem
                 c-lb-est     c-lb-item
                 c-lb-descr   c-lb-un
                 c-lb-preco   c-lb-moeda
                 c-lb-alt     
                 &if defined (bf_mat_oper_triangular) &then 
                     c-lb-emit-ent    
                 &endif
                 /*Unidade de Neg¢cio*/
                 &IF '{&bf_mat_versao_ems}' >= '2.062' &THEN
                 c-lb-unineg
                 &ENDIF
                 c-lb-pa
                 c-lb-sit     c-lb-dt-emis
                 c-lb-dt-entr c-lb-qtd
                 c-lb-saldo   c-lb-pr-merc
                 c-lb-valor   c-lb-total
                 c-lb-cliente c-lb-seq
                 c-lb-atraso
                 with stream-io frame f-cab-corpo.
        end.
        if  last-of(ordem-compra.num-pedido) then do:
            put c-lb-tot-liq    to 58 format "x(13)" ": " de-tot-liq at 61 
                c-lb-tot-ipi    to 93 format "x(12)" ": " de-tot-ipi at 96 skip
                c-lb-tot-descto to 58 format "x(14)" ": " de-tot-descto at 63                 
                c-lb-tot-ped    to 93 format "x(12)" ": " de-tot-ped at 96 skip(1).
            assign de-tot-liq-res = de-tot-liq-res + de-tot-liq
                   de-tot-ipi-res = de-tot-ipi-res + de-tot-ipi
                   de-tot-des-res = de-tot-des-res + de-tot-descto
                   de-tot-ped-res = de-tot-ped-res + de-tot-ped
                   l-imprimiu     = no
                   de-tot-ped     = 0
                   de-tot-liq     = 0
                   de-tot-ipi     = 0
                   de-tot-descto  = 0
                   i-nr-pedido    = i-nr-pedido + 1.
        end.
    end.
    if  last-of(ordem-compra.cod-comprado) then do:
        put c-lb-tot-liq    to 53 format "x(13)" ": " de-tot-liq-res at 56 
            c-lb-tot-ipi    to 93 format "x(12)" ": " de-tot-ipi-res at 96 skip
            c-lb-tot-descto to 53 format "x(14)" ": " de-tot-des-res at 58             
            c-lb-tot-res    to 93 format "x(17)" ": " de-tot-ped-res at 96 skip(1).
        assign de-ger-liq     = de-ger-liq + de-tot-liq-res
               de-ger-ipi     = de-ger-ipi + de-tot-ipi-res
               de-ger-descto  = de-ger-descto + de-tot-des-res
               de-tot-ger     = de-tot-ger + de-tot-ped-res
               de-tot-ped-res = 0
               de-tot-liq-res = 0
               de-tot-des-res = 0
               de-tot-ipi-res = 0.
    end.
end.
if  i-nr-pedido <> 0 then do:
    if  line-counter > 60 then do:
        page.
        disp c-lb-pedido  c-lb-dt-ped
             c-lb-nat     c-lb-fornec
             c-lb-cond    c-lb-ordem
             c-lb-est     c-lb-item
             c-lb-descr   c-lb-un
             c-lb-preco   c-lb-moeda
             c-lb-alt     
             &if defined (bf_mat_oper_triangular) &then 
                 c-lb-emit-ent    
             &endif
             /*Unidade de Neg¢cio*/
             &IF '{&bf_mat_versao_ems}' >= '2.062' &THEN
             c-lb-unineg
             &ENDIF
             c-lb-pa
             c-lb-sit     c-lb-dt-emis
             c-lb-dt-entr c-lb-qtd
             c-lb-saldo   c-lb-pr-merc
             c-lb-valor   c-lb-total
             c-lb-cliente c-lb-seq
             c-lb-atraso
             with stream-io frame f-cab-corpo.
    end.
    put c-lb-num-ped to 53 format "x(17)" ": " i-nr-pedido   at 57
        c-lb-qtde    to 93 format "x(16)" ": " de-qt-tot-ped at 96.
    if  line-counter < 8 then
        page.

    put c-lb-tot-liq    to 58 format "x(13)" ": " de-ger-liq at 61 
        c-lb-tot-ipi    to 93 format "x(12)" ": " de-ger-ipi at 96 skip
        c-lb-tot-descto to 58 format "x(14)" ": " de-ger-descto at 63         
        c-lb-tot-ger    to 93 format "x(11)" ": " de-tot-ger at 96 skip.
    assign de-ger-liq    = 0
           de-ger-ipi    = 0
           de-ger-descto = 0
           de-tot-ger    = 0.
end.
put skip(1).

run pi-finalizar in h-acomp.

{include/pi-edit.i}



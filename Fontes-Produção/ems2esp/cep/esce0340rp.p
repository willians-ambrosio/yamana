/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa     for ems2cadme.empresa.

{include/i-prgvrs.i ESCE0340RP 2.00.00.000}  /*** 010000 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i esce0340rp MCE}
&ENDIF

{include/i_fnctrad.i}
/******************************************************************************
**
**       PROGRAMA: ESCE0340RP.P
**
**       DATA....: Abril de 1999
**
**       OBJETIVO: Ressuprimento do Estoque com os dados do Xtivity.
**
**       VERSAO..: 1.00.000
** 
******************************************************************************/

{include/i-rpvar.i}
{include/i-rpcab.i}


{include/i-epc200.i "ESCE0340RP"} /* Defini‡Æo TT-EPC */

/* {cep/ce0340.i} /*Defini‡Æo da tt-param*/ */
/*-------------------------------------------------------------------------*/
    define temp-table tt-param no-undo
        field destino          as integer
        field arquivo          as char    format "x(35)"
        field usuario          as char    format "x(12)"
        field data-exec        as date
        field hora-exec        as integer
        field estabelec-ini    as char    format "x(3)"
        field estabelec-fim    as char    format "x(3)"
        field ge-ini           as int     format "99"
        field ge-fim           as int     format "99"
        field familia-ini      as char    format "x(8)"
        field familia-fim      as char    format "x(8)"
        field item-ini         as char    format "x(16)"
        field item-fim         as char    format "x(16)"
        field depos-ini        as char    format "x(3)"
        field depos-fim        as char    format "x(3)"
        field periodico        as logical format "Sim/NÆo"
        field pto-enc          as logical format "Sim/NÆo"
        field elimina          as integer format "9"
        field l-depositos      as logical format "Sim/NÆo"
        field l-solcomp        as logical format "Sim/NÆo"
        field l-reqest         as logical format "Sim/NÆo"
        field i-sldaloc        as integer format "9"
        field i-contacont      as integer format "9"
        field i-consprev       as integer format "9"
        field desc-elimina     as char    format "x(20)"
        field desc-sldaloc     as char    format "x(22)"
        field desc-contacont   as char    format "x(20)"
        field desc-consprev    as char    format "x(10)".

/*-------------------------------------------------------------------------*/
define temp-table tt-digita
    field cod-estabel like estabelec.cod-estabel
    field cod-depos   like deposito.cod-depos
    field descricao   like deposito.nome 
    field cons-saldo  like deposito.cons-saldo
    field lista       as log FORMAT "*~/ "
    index id is primary cod-estabel.

def temp-table tt-raw-digita
    field raw-digita as raw.

def temp-table tt-entrega
        field cd-freq        like frequencia.cd-freq /*cod. da frequencia de */
        field nr-sequencia   as integer /* seq. das entregas de cada cod. freq.*/
        field dat-entrega    as date.

def temp-table tt-item-uni-estab no-undo
        field it-codigo      as char format "x(16)"
&IF "{&mguni_version}" >= "2.071" &THEN
        field cod-estabel    as char format "x(05)"
&ELSE
        field cod-estabel    as char format "x(3)"
&ENDIF
        index codigo it-codigo
                     cod-estabel.        

def buffer b-estab-mat for estab-mat.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

def var i-per-corrente  as integer no-undo.
def var i-ano-corrente  as integer no-undo.
def var da-iniper-fech  like param-estoq.ult-fech-dia no-undo.
def var da-fimper-fech  like param-estoq.ult-fech-dia no-undo.
def var i-mes-aux       as integer no-undo format "99".
def var h-acomp         as handle  no-undo.
def var c-acompanha     as char    no-undo.
def var de-cons-proj   like item-uni-estab.consumo-prev no-undo. /* Consumo previsto projetado */
def var de-qt-nec      as decimal   no-undo.
def var de-qt-estoque  as decimal   no-undo.
def var de-qt-cc       as decimal   no-undo.
def var de-qt-req      as decimal   no-undo.
def var de-qt-sol      as decimal   no-undo.
def var de-nr-lotes    as decimal   no-undo.
def var de-pto-enc     as decimal   no-undo.
def var da-data        as date      format "99/99/9999" no-undo.
DEF VAR l-gerou-necessidade AS LOGICAL INIT NO NO-UNDO.

/* Variaveis auxiliares p/ algoritmos */
def var c-it-codigo-aux     like item.it-codigo            no-undo.
def var c-cod-estabel-aux   like estabelec.cod-estabel     no-undo.
def var de-consumo-prev-aux like item.consumo-prev         no-undo.
def var i-tipo-est-seg-aux  like item.tipo-est-seg         no-undo.
def var de-quant-segur-aux  like item.quant-segur          no-undo.
def var i-tempo-segur-aux   like item.tempo-segur          no-undo.
def var i-res-int-comp-aux  like item.res-int-comp         no-undo.
def var i-res-for-comp-aux  like item.res-for-comp         no-undo.
def var i-res-cq-comp-aux   like item.res-cq-comp          no-undo.
def var de-pto-enc-aux      like item-mat.ponto-encomenda  no-undo.
def var de-fator-refugo-aux like item.fator-refugo         no-undo.
def var de-quant-perda-aux  like item.quant-perda          no-undo.
def var de-lote-economi-aux like item.lote-economi         no-undo.
def var de-lote-minimo-aux  like item.lote-minimo          no-undo.
def var de-lote-multipl-aux like item.lote-multipl         no-undo.
def var i-cd-freq-aux       like item-mat.cd-freq          no-undo.
def var da-ult-ressup-aux   like item-mat.data-ult-ressup  no-undo.
def var de-lote-max-aux     like item-mat.lote-per-max     no-undo.
def var i-ind               as integer                     no-undo.
def var i-situacao          as integer extent 3            no-undo.


/* Vari veis p/ tradu‡Æo */
def var c-lb-tit-par   as char format "x(15)" no-undo.
def var c-lb-tit-sel   as char                no-undo.
def var c-lb-pto-enc   as char format "x(47)" no-undo.
def var c-lb-depositos as char format "x(19)" no-undo. 
def var c-lb-periodico as char format "x(40)" no-undo.
def var c-lb-estab     as char                no-undo.
def var c-lb-ge        as char                no-undo.
def var c-lb-fam       as char                no-undo.
def var c-lb-item      as char                no-undo.
def var c-lb-tit-imp   as char                no-undo.
def var c-lb-destino   as char                no-undo.
def var c-lb-usuario   as char                no-undo.
def var c-lb-elimina   as char format "x(20)" no-undo.
def var c-lb-sol-comp  as char format "x(22)" no-undo.
def var c-lb-req-est   as char format "x(22)" no-undo.
def var c-lb-sld-aloc  as char format "x(13)" no-undo.
def var c-lb-conta     as char format "x(21)" no-undo.
def var c-lb-gera      as char                no-undo.
def var c-mensagem     like &IF "{&emsfnd_version}" >= "1.00" &THEN cadast_msg.dsl_help_msg VIEW-AS EDITOR SIZE 70 BY 10 &ELSE cadast_msg.dsl_help_msg &ENDIF no-undo.
def var c-dig          as char                no-undo.

FORM 
    c-lb-tit-par skip(1)
    c-lb-pto-enc   at 5 FORMAT "x(40)"  no-label ":" SPACE tt-param.pto-enc        NO-LABEL SKIP
    c-lb-periodico at 5 FORMAT "x(40)"  no-label ":" SPACE tt-param.periodico      no-label SKIP
    c-lb-depositos at 5 FORMAT "x(40)"  no-label ":" SPACE tt-param.l-depositos    no-label SKIP
    c-lb-elimina   at 5 FORMAT "x(40)"  no-label ":" SPACE tt-param.desc-elimina   no-label SKIP
    c-lb-sol-comp  at 5 FORMAT "x(40)"  no-label ":" SPACE tt-param.l-solcomp      no-label SKIP
    c-lb-req-est   at 5 FORMAT "x(40)"  no-label ":" SPACE tt-param.l-reqest       no-label SKIP
    c-lb-sld-aloc  at 5 FORMAT "x(40)"  no-label ":" SPACE tt-param.desc-sldaloc   no-label SKIP
    c-lb-conta     at 5 FORMAT "x(40)"  no-label ":" SPACE tt-param.desc-contacont no-label SKIP
    c-lb-gera      at 5 FORMAT "x(40)"  no-label ":" SPACE tt-param.desc-consprev  NO-LABEL SKIP
    skip(1)
    with stream-io no-box no-label width 132 frame f-param.

FORM 
    tt-digita.cod-estabel at 5
    tt-digita.cod-depos  space(1) 
    tt-digita.descricao  space(1)
    tt-digita.cons-saldo  
    with stream-io no-box down width 80 frame f-digita.

find first param-estoq  no-lock no-error.
find first param-global no-lock no-error.
find first empresa
    where empresa.ep-codigo = param-global.empresa-prin no-lock no-error.

run cdp/cdapi005.p (input  param-estoq.ult-per-fech,
                    output da-iniper-x,
                    output da-fimper-x,
                    output i-per-corrente,
                    output i-ano-corrente,
                    output da-iniper-fech,
                    output da-fimper-fech).

assign c-programa = "CE/0340"
       c-versao   = "1.00"
       c-revisao  = "000"
       i-mes-aux  = month(da-fimper-x)
       i-numper-x = i-mes-aux
       c-empresa  = empresa.razao-social.

find first tt-param no-lock no-error.
{include/i-rpout.i}
{utp/ut-liter.i Ressuprimento_Estoques * L}
assign c-titulo-relat = trim(return-value).
{utp/ut-liter.i ESTOQUE * L}
assign c-sistema = trim(return-value).

view frame f-cabper.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Eliminando_Necessidades * L}
run pi-inicializar in h-acomp (input return-value).

if tt-param.elimina = 1 then
    for each esp-necessidade-oc EXCLUSIVE-LOCK:

       /* CHAMADA EPC */
    ............................................................................
          for each tt-epc
              where tt-epc.cod-event = "Delete-generation":
              delete tt-epc.
          end.

          create tt-epc.
          assign tt-epc.cod-event     = "Delete-generation"
                 tt-epc.cod-parameter = "TABLE-ROWID"
                 tt-epc.val-parameter = string(rowid(necessidade-oc)).
          {include/i-epc201.i "Delete-generation"}

    ............................................................................

        delete esp-necessidade-oc.
    end.
else do:
    for each grup-estoque fields (ge-codigo) 
        WHERE grup-estoque.ge-codigo >= tt-param.ge-ini 
          AND grup-estoque.ge-codigo <= tt-param.ge-fim no-lock:

        for each item fields (it-codigo ge-codigo fm-codigo) 
            WHERE item.ge-codigo = grup-estoque.ge-codigo 
              AND item.it-codigo >= tt-param.item-ini     
              AND item.it-codigo <= tt-param.item-fim     
              AND item.fm-codigo >= tt-param.familia-ini  
              AND item.fm-codigo <= tt-param.familia-fim no-lock:

            for each item-uni-estab FIELDS(it-codigo cod-estabel deposito-pad)
                where item-uni-estab.it-codigo     = item.it-codigo         
                  AND item-uni-estab.cod-estabel  >= tt-param.estabelec-ini 
                  AND item-uni-estab.cod-estabel  <= tt-param.estabelec-fim 
                  AND item-uni-estab.deposito-pad >= tt-param.depos-ini     
                  AND item-uni-estab.deposito-pad <= tt-param.depos-fim  no-lock:

                for each esp-necessidade-oc 
                    WHERE esp-necessidade-oc.it-codigo   = item.it-codigo 
                      AND esp-necessidade-oc.cod-estabel = item-uni-estab.cod-estabel exclusive-lock:
                    delete esp-necessidade-oc.
                end.
            end.
        end.
    end.
end.

{utp/ut-liter.i Ressuprimento_Estoque * L}
run pi-inicializar in h-acomp (input return-value).
/* Come»o do algoritmo de ressuprimento */

for each grup-estoque fields (ge-codigo) 
    WHERE grup-estoque.ge-codigo >= tt-param.ge-ini 
      AND grup-estoque.ge-codigo <= tt-param.ge-fim no-lock:

    bloco-item:
    for each item fields (it-codigo    fm-codigo    ge-codigo       demanda 
                          cod-obsoleto compr-fabric emissao-ord     consumo-prev
                          tipo-est-seg quant-segur  tempo-segur     res-int-comp
                          res-for-comp res-cq-comp  fator-refugo    quant-perda  
                          lote-economi lote-minimo  lote-multipl    tipo-contr
                          deposito-pad) 
        WHERE item.ge-codigo   =  grup-estoque.ge-codigo         
          AND item.it-codigo   >= tt-param.item-ini              
          AND item.it-codigo   <= tt-param.item-fim              
          AND item.fm-codigo   >= tt-param.familia-ini           
          AND item.fm-codigo   <= tt-param.familia-fim no-lock:

        {utp/ut-field.i mgind grup-estoque ge-codigo 1}
        assign c-acompanha = return-value + ": " + string(grup-estoque.ge-codigo).
        {utp/ut-field.i mgind item it-codigo 1} 
        assign c-acompanha = c-acompanha  + " " + return-value + ": " + item.it-codigo.
        run pi-acompanhar in h-acomp (input c-acompanha).

        EMPTY TEMP-TABLE tt-item-uni-estab.
        
        find item-mat where
             item-mat.it-codigo = item.it-codigo no-lock no-error.

        bloco-estabelec:
        for each estab-mat fields (cod-estabel cod-estabel-prin) 
            WHERE estab-mat.cod-estabel-prin >= tt-param.estabelec-ini 
              AND estab-mat.cod-estabel-prin <= tt-param.estabelec-fim no-lock:

            assign de-qt-nec     = 0
                   de-qt-estoque = 0
                   de-qt-cc      = 0
                   de-qt-req     = 0
                   de-qt-sol     = 0.

            /*Verifica os saldos existentes nos estabelecimentos relacionados*/
            if can-find(first item-uni-estab 
                        WHERE item-uni-estab.it-codigo = item-mat.it-codigo) then do:                
                if can-find(first tt-item-uni-estab 
                            WHERE tt-item-uni-estab.it-codigo   = item.it-codigo 
                              AND tt-item-uni-estab.cod-estabel = estab-mat.cod-estabel-prin) then
                next.
                
                find first item-uni-estab NO-LOCK
                    WHERE item-uni-estab.it-codigo = item-mat.it-codigo 
                      and  item-uni-estab.cod-estabel = estab-mat.cod-estabel-prin no-error.
                if  avail item-uni-estab then do:
                    if  tt-param.depos-ini > item-uni-estab.deposito-pad 
                    or  tt-param.depos-fim < item-uni-estab.deposito-pad 
                    then 
                        next.

                    if not ((((item-uni-estab.tp-ressup     = 1 and  tt-param.pto-enc) or
                              item-uni-estab.tp-ressup     = 3 and tt-param.periodico)) and
                            item-uni-estab.demanda       = 2 and
                            item-uni-estab.cod-obsoleto  = 1 and 
                            item.compr-fabric            = 1 and   
                            item-uni-estab.emissao-ord   = 1 ) then next.

                    if (item-uni-estab.consumo-prev <= 0 and 
                        tt-param.i-consprev = 1) and        /* Gerar com Consumo Previsto Zero */
                        item-uni-estab.tp-ressup = 1 then   /* Ressup por Ponto de Encomenda   */
                        undo bloco-estabelec, next bloco-estabelec.

                    for each  b-estab-mat fields (cod-estabel-prin cod-estabel)
                        where b-estab-mat.cod-estabel-prin = estab-mat.cod-estabel no-lock:
                              create tt-item-uni-estab.
                              assign tt-item-uni-estab.it-codigo   = item.it-codigo
                                     tt-item-uni-estab.cod-estabel = b-estab-mat.cod-estabel-prin.
                       {cep/ce0340.i3 item-uni-estab b-estab-mat.cod-estabel} /* Verifica saldo dispon¡vel p/ o item */
                       /*Alterado para ler saldos do estabelecimento lido e n’o do principal */ 
                    end.

                    /* caso o estabelecimento nao seja principal, nao ha saldo disponivel */
                    IF  NOT CAN-FIND (FIRST tt-item-uni-estab 
                                      WHERE tt-item-uni-estab.it-codigo   = ITEM.it-codigo 
                                      AND   tt-item-uni-estab.cod-estabel = estab-mat.cod-estabel)
                    THEN NEXT.

                    assign de-qt-nec = de-qt-estoque + de-qt-cc. 

                    if item-uni-estab.tp-ressup = 1 then do transaction on stop undo, leave: 
                        {cep/ce0340.i4}
                        run pi-pto-encomenda (rowid(item-uni-estab), 1).
                    end.
                    else if item-uni-estab.tp-ressup = 3 then do transaction on stop undo, leave: 
                        if item-uni-estab.data-ult-ressup = ? or
                            string(item-uni-estab.data-ult-ressup) = " " then
                            next.
                        {cep/ce0340.i4}
                        assign da-data = ?.
                        run pi-periodico (output da-data).
                        if da-data = ? then next.
                        find current item-uni-estab exclusive-lock.
                        assign item-uni-estab.data-ult-ressup = da-data.

                    end.
                end.
            end.
            else do:
                if  tt-param.depos-ini < item.deposito-pad 
                or  tt-param.depos-fim > item.deposito-pad 
                then next.

                if not ((((item-mat.tp-ressup     = 1 and  tt-param.pto-enc) or
                          item-mat.tp-ressup     = 3 and tt-param.periodico)) and
                         item.demanda       = 2 and
                         item.cod-obsoleto  = 1 and 
                         item.compr-fabric  = 1 and   
                         item.emissao-ord   = 1 ) then next.

                if (item.consumo-prev  <= 0  and
                    tt-param.i-consprev = 1) and
                    item-mat.tp-ressup  = 1  then
                    undo bloco-item, next bloco-item.

                if can-find(first tt-item-uni-estab 
                            WHERE tt-item-uni-estab.it-codigo   = item.it-codigo 
                              AND tt-item-uni-estab.cod-estabel = estab-mat.cod-estabel-prin) then
                    next.

                for each b-estab-mat fields (cod-estabel-prin cod-estabel) 
                    where b-estab-mat.cod-estabel-prin = estab-mat.cod-estabel no-lock:
                    create tt-item-uni-estab.
                    assign tt-item-uni-estab.it-codigo   = item.it-codigo
                           tt-item-uni-estab.cod-estabel = b-estab-mat.cod-estabel-prin.
                    {cep/ce0340.i3 item-mat b-estab-mat.cod-estabel-prin} /* Verifica saldo dispon­vel p/ o item */
                end.
                assign de-qt-nec = de-qt-estoque + de-qt-cc. 
                if item-mat.tp-ressup  = 1 then do transaction on stop undo, leave: 
                    {cep/ce0340.i5}
                    run pi-pto-encomenda (rowid(item-mat), 0).
                end.
                else if item-mat.tp-ressup = 3 then do transaction on stop undo, leave: 
                    if item-mat.data-ult-ressup = ? or
                        string(item-mat.data-ult-ressup) = " " then
                        next.
                    {cep/ce0340.i5}
                    run pi-periodico (output da-data).
                    if da-data = ? then next.
                    find current item-mat exclusive-lock.
                    assign item-mat.data-ult-ressup = da-data.
                end.
            end.        
        end.
    end.
end.

/********************************
* Chamada EPC para Multi-Planta *
********************************/

for each tt-epc
    where tt-epc.cod-event = "CE0340RP":
    delete tt-epc.
end.

create tt-epc.
assign tt-epc.cod-event     = "CE0340RP"
       tt-epc.cod-parameter = ""
       tt-epc.val-parameter = "".

{include/i-epc201.i "ESCE0340RP"}

{cep/esce0340.i1} /* procedure pi-pto-encomenda */

{cep/ce0340.i2} /* procedure pi-periodico */

if can-find(first esp-necessidade-oc) AND l-gerou-necessidade then do:
    run utp/ut-msgs.p (input "help", 
                       input 17372 , 
                       input ""   ).
    assign c-mensagem = return-value
           l-gerou-necessidade = NO.
    disp c-mensagem with frame f-aviso no-label stream-io centered.
end.

{utp/ut-liter.i DIGITACAO * r}
assign c-dig = trim(return-value).
{utp/ut-liter.i PARAMETROS * r}
assign c-lb-tit-par = trim(return-value).
{utp/ut-liter.i SELECAO * r}
assign c-lb-tit-sel = trim(return-value).
{utp/ut-liter.i IMPRESSAO * r}
assign c-lb-tit-imp = trim(return-value).
{utp/ut-liter.i Usuario * r}
assign c-lb-usuario = trim(return-value).
{utp/ut-liter.i Destino * r}
assign c-lb-destino = trim(return-value).
{utp/ut-liter.i Grupo_de_Estoque * r}
assign c-lb-ge = trim(return-value).
{utp/ut-liter.i Estabelecimento * r}
assign c-lb-estab = trim(return-value).
{utp/ut-liter.i Item * r}
assign c-lb-item = trim(return-value).
{utp/ut-field.i mgind familia fm-codigo 1}
assign c-lb-fam = trim(return-value).
{utp/ut-liter.i Considera_Ressuprimento_Peri½dico * L}
assign c-lb-periodico = trim(return-value).
{utp/ut-liter.i Considera_Ressuprimento_Ponto_Encomenda * L}
assign c-lb-pto-enc = trim(return-value).
{utp/ut-liter.i Informa_Dep¢sitos * L}
assign c-lb-depositos = trim(return-value).
{utp/ut-liter.i "Elimina Necessidades" *}
assign c-lb-elimina  = trim(return-value).
{utp/ut-liter.i "Solicita‡äes de Compra" *}
assign c-lb-sol-comp = trim(return-value).
{utp/ut-liter.i "Requisi‡äes de Estoque" *}
assign c-lb-req-est  = trim(return-value).
{utp/ut-liter.i "Saldo Estoque" *}
assign c-lb-sld-aloc = trim(return-value).
{utp/ut-liter.i "Ordens de Compra" *}
assign c-lb-conta    = trim(return-value).
{utp/ut-liter.i "Consumo Previsto Zero" *}
assign c-lb-gera     = trim(return-value).
{utp/ut-liter.i Sim/NÆo *}
ASSIGN tt-param.pto-enc:FORMAT IN FRAME f-param      = TRIM(RETURN-VALUE)
       tt-param.periodico:FORMAT IN FRAME f-param    = TRIM(RETURN-VALUE)
       tt-param.l-depositos:FORMAT IN FRAME f-param  = TRIM(RETURN-VALUE)
       tt-param.l-solcomp:FORMAT IN FRAME f-param    = TRIM(RETURN-VALUE)
       tt-param.l-reqest:FORMAT IN FRAME f-param     = TRIM(RETURN-VALUE)
       tt-digita.cons-saldo:FORMAT IN FRAME f-digita = TRIM(RETURN-VALUE).
{utp/ut-liter.i Est *}
ASSIGN tt-digita.cod-estabel:LABEL IN FRAME f-digita = TRIM(RETURN-VALUE).
{utp/ut-liter.i Dep *}
ASSIGN tt-digita.cod-depos:LABEL IN FRAME f-digita = TRIM(RETURN-VALUE).
{utp/ut-liter.i Descri‡Æo *}
ASSIGN tt-digita.descricao:LABEL IN FRAME f-digita = TRIM(RETURN-VALUE).
{utp/ut-liter.i Saldo_Disp *}
ASSIGN tt-digita.cons-saldo:LABEL IN FRAME f-digita = TRIM(RETURN-VALUE).

DISP c-lb-tit-par 
     c-lb-pto-enc
     tt-param.pto-enc
     c-lb-periodico
     tt-param.periodico
     c-lb-depositos
     tt-param.l-depositos
     c-lb-elimina
     tt-param.desc-elimina
     c-lb-sol-comp
     tt-param.l-solcomp
     c-lb-req-est
     tt-param.l-reqest
     c-lb-sld-aloc
     tt-param.desc-sldaloc
     c-lb-conta
     tt-param.desc-contacont
     c-lb-gera
     tt-param.desc-consprev WITH FRAME f-param.
DOWN WITH FRAME f-param.

put unformatted
    c-lb-tit-sel           skip(1)
    c-lb-estab             at 5  '.:'
    tt-param.estabelec-ini at 23 '|<  >| ' at 40 tt-param.estabelec-fim
    c-lb-ge                at 5  ':'
    tt-param.ge-ini        at 23 '|<  >| ' at 40 tt-param.ge-fim    
    c-lb-fam               at 5  '.........:'
    tt-param.familia-ini   at 23 '|<  >| ' at 40 tt-param.familia-fim
    c-lb-item              at 5  '............:'
    tt-param.item-ini      at 23 '|<  >| ' at 40 tt-param.item-fim skip(1)
    c-lb-tit-imp           skip(1)
    c-lb-destino           at 5  ': ' tt-param.arquivo
    c-lb-usuario           at 5  ': ' tt-param.usuario.


if  tt-param.l-depositos = yes then do:
    find first tt-digita no-error.
    if  avail tt-digita then
        put unformatted SKIP(1) c-dig skip(1).

    for each tt-digita:
        disp tt-digita.cod-estabel at 5
             tt-digita.cod-depos  space(1) 
             tt-digita.descricao  space(1)
             tt-digita.cons-saldo  
             with frame f-digita.
        down with frame f-digita.                              
    end.
    put "" skip.
end.            

{include/i-rpclo.i}   

run pi-finalizar in h-acomp.

return "OK":U.


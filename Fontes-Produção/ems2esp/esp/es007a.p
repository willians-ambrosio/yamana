/*******************************************************************************
**
**       PROGRAMA:  ES007A
**       DATA....:  MAIO/2012
**       AUTOR...:  
**       OBJETIVO:  Refazer saldos em contratos
**
*******************************************************************************/
{include/i-prgvrs.i ES007A 2.00.00.000}  

def new shared var c-cod-estabel like estabelec.cod-estabel.

{cdp/cdcfgmat.i}
{cdp/cd9731.i2} /*l-matriz-medicao       */

DEF VAR i-cont-med AS INTE.
def var l-erro-co         as logical initial no.
def var de-indice         as int.
def var l-contrato        as logical init no no-undo.
def var de-valor-conver   like item-contrat.val-total.
def var de-valor-liber    like item-contrat.val-total.
def var d-sld-rec-medicao LIKE medicao-contrat.qtd-prevista  no-undo.
def var d-qtd-rec-medicao LIKE medicao-contrat.qtd-prevista  no-undo.
DEF VAR de-qtd-lib LIKE medicao-contrat.qtd-prevista NO-UNDO.
DEF VAR de-qtd-rec LIKE medicao-contrat.qtd-prevista NO-UNDO.
def var de-val-mov LIKE medicao-contrat.val-previsto no-undo.
def var de-val-lib LIKE medicao-contrat.val-previsto no-undo.
def var de-val-rec LIKE medicao-contrat.val-previsto no-undo.
DEF VAR de-pre-unit-for LIKE item-contrat.pre-unit-for NO-UNDO.

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field l-atualiza       as logical
    field l-medicoes       as logical
    field desc-classifica  as char format "x(40)"
    field i-nr-contr-ini   like item-contrat.nr-contrat
    field i-nr-contr-fim   like item-contrat.nr-contrat
    FIELD l-atualiza-medicoes AS LOG
    .

def temp-table tt-raw-digita
    field raw-digita as raw.
    
def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

/* Var novas */
def var de-valor-mov like medicao-contrat.sld-val-medicao no-undo.
def var de-valor-lib like medicao-contrat.sld-val-medicao no-undo.
def var de-valor-rec like medicao-contrat.sld-val-medicao no-undo.
def var de-rec-qtd-medicao like medicao-contrat.qtd-prevista no-undo.
def var de-sld-qtd-medicao like medicao-contrat.qtd-prevista no-undo.

def var de-qtd-mov like evento-ped.qtd-prevista.
DEF VAR l-cn-tem-redutor AS LOG INIT NO NO-UNDO.
def buffer b-item-contrat for item-contrat.

form
    "Contrato: " tt-param.i-nr-contr-ini 
     tt-param.i-nr-contr-fim  
    with stream-io side-labels no-labels title "Selecao" row 17 frame f-selecao.

def var h-acomp           as handle  no-undo.
def var c-acomp           as char    no-undo.

{cdp/cd0666.i} /* tt-erro */
def var l-msg-tela as logical. 
/* --------------------------------------------------------------------------- */
{utp/ut-glob.i}

create tt-param.
raw-transfer raw-param to tt-param.

{include/i-rpvar.i}

find first param-estoq no-lock no-error.
find first param-global no-lock no-error.

assign c-empresa  = (if avail param-global then param-global.grupo else "").

{utp/ut-liter.i CONTRATOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Correá∆o_Saldos_Contratos * r}
assign c-titulo-relat = trim(return-value).

{include/i-rpcab.i}

{include/i-rpout.i}

find first param-contrat no-lock no-error.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Atualizando_Saldos_do_Contrato}
assign c-acomp = trim(return-value) + "...".
run pi-inicializar in h-acomp (input c-acomp).

view  frame f-cabec.
view  frame f-rodape.
for each contrato-for where 
    contrato-for.nr-contrato >= tt-param.i-nr-contr-ini and
    contrato-for.nr-contrato <= tt-param.i-nr-contr-fim exclusive-lock:
    
    {utp/ut-liter.i Acertando_Itens_MediÁ„o}
    assign c-acomp = trim(return-value) + " " + string(contrato-for.nr-contrato).
    run pi-inicializar in h-acomp (input c-acomp).   
        
    find first item-contrat where item-contrat.nr-contrato = contrato-for.nr-contrato
        no-lock no-error.
    if  not avail item-contrat 
    or  can-find (first b-item-contrat where b-item-contrat.nr-contrato = contrato-for.nr-contrato
                                       and   b-item-contrat.ind-tipo-control <> item-contrat.ind-tipo-control)
    then do:
        put skip(1) "Contrato: " contrato-for.nr-contrato " possui mais de um tipo de controle(medicao/ordem/programacao).".
        next.
    end.
    if  contrato-for.ind-control-rec = 1 then 
    ASSIGN l-cn-tem-redutor = CAN-FIND(item-contrat WHERE
                                       item-contrat.nr-contrato = contrato-for.nr-contrato AND
                                       item-contrat.ind-caract-item = 2). /* redutor */
    ELSE
    ASSIGN l-cn-tem-redutor = NO.

    PUT SKIP(1)
        "  Contrato........: " contrato-for.nr-contrato SKIP
        "  Controle Total..: " contrato-for.ind-control-rec = 1 FORMAT "Sim/Nao" SKIP
        "  Tem item redutor: " l-cn-tem-redutor FORMAT "Sim/Nao" SKIP
        "  --------- Antes Atualizacao ----------- " SKIP
        "  Saldo Movimentado: " contrato-for.sld-val SKIP
        "  Saldo Liberado...: " contrato-for.sld-val-liber SKIP
        "  Saldo Recebido...: " contrato-for.sld-val-rec   SKIP "".
    /*  Acerta Itens Medicao */
    if  contrato-for.ind-control-rec = 1 then 
        run pi-medicao-total.
    else
    if  can-find (first item-contrat where item-contrat.nr-contrato = contrato-for.nr-contrato and
                        item-contrat.ind-tipo-control = 1) then
        run pi-medicao-item.    
    
    {utp/ut-liter.i Acertando_Itens_Ordem_e_ProgramaÁ„o}
    assign c-acomp = trim(return-value) + " " + string(contrato-for.nr-contrato).
    run pi-inicializar in h-acomp (input c-acomp).
    
    /* Acerta Itens por ordem/programacao */
    if  not can-find (first item-contrat where item-contrat.nr-contrato = contrato-for.nr-contrato and 
                            item-contrat.ind-tipo-control = 1) 
    then 
        run pi-refaz-saldo. 

    PUT  "  --------- Apos Atualizacao ------------ "
    SKIP "  Saldo Movimentado: " contrato-for.sld-val 
    SKIP "  Saldo Liberado...: " contrato-for.sld-val-liber 
    SKIP "  Saldo Recebido...: " contrato-for.sld-val-rec .

end. 

PUT SKIP(3) "".

if  can-find (first tt-erro) then do:
    for each tt-erro no-lock:
        put string(tt-erro.cd-erro,">>>>>9") at 43.
        put tt-erro.mensagem at 53 format "X(78)" skip.
    end.
end.
else
    put "Atualizacao Efetuada com Sucesso!" at 5 format "X(78)".

/**********************************************************/
PUT skip(5) "------------------------------" SKIP
            "Contr Ini........: " tt-param.i-nr-contr-ini SKIP
            "Contr Fim........: " tt-param.i-nr-contr-fim SKIP
            "Atualiza Medicoes: " tt-param.l-atualiza-medicoes FORMAT "Sim/Nao" SKIP
            "------------------------------"
    SKIP(1) "".
/**********************************************************/

PUT SKIP(8) c-rodape format "x(132)".

run pi-finalizar in h-acomp.

return "OK".

/* -------------------------------- Procedures -------------------------------- */

Procedure pi-refaz-saldo:

    assign l-contrato = no.
    assign contrato-for.sld-val   = 0
           contrato-for.sld-val-liber = 0 
           contrato-for.sld-val-receb = 0.
    
    for each item-contrat where 
        item-contrat.nr-contrat = contrato-for.nr-contrato exclusive-lock:
        
        /* Qdo controlado por Medicao o calculo e efetuado na pi-acerta-medicao */
        if  item-contrat.ind-tipo-control = 1 then 
            next.
        
        /* acerta somente contrato por ordem/programacao */
        if  contrato-for.ind-control-rec   = 2 and
           (item-contrat.ind-tipo-control = 2 or
            item-contrat.ind-tipo-control = 3 ) then do:
            
            ASSIGN item-contrat.sld-qtd = 0
                   item-contrat.sld-val = 0
                   item-contrat.sld-qtd-liber = 0 
                   item-contrat.sld-val-liber = 0
                   item-contrat.sld-qtd-receb = 0
                   item-contrat.sld-val-receb = 0.
            FOR EACH item-contrat-estab where 
                     item-contrat-estab.cod-emitente = item-contrat.cod-emitente and
                     item-contrat-estab.nr-contrato  = item-contrat.nr-contrato  and
                     item-contrat-estab.it-codigo    = item-contrat.it-codigo    and
                     item-contrat-estab.num-seq-item = item-contrat.num-seq-item exclusive-lock:
                 ASSIGN item-contrat-estab.sld-qtd       = 0
                        item-contrat-estab.sld-val       = 0
                        item-contrat-estab.sld-qtd-liber = 0
                        item-contrat-estab.sld-val-liber = 0
                        item-contrat-estab.sld-qtd-receb = 0
                        item-contrat-estab.sld-val-receb = 0 .
            END.
            
            for each ordem-compra 
                where ordem-compra.nr-contrato = item-contrat.nr-contrat
                and   ordem-compra.num-seq-item = item-contrat.num-seq-item 
                no-lock:
                
                find item-contrat-estab where 
                     item-contrat-estab.cod-estabel  = ordem-compra.cod-estabel  and
                     item-contrat-estab.cod-emitente = item-contrat.cod-emitente and
                     item-contrat-estab.nr-contrato  = item-contrat.nr-contrato  and
                     item-contrat-estab.it-codigo    = item-contrat.it-codigo    and
                     item-contrat-estab.num-seq-item = item-contrat.num-seq-item exclusive-lock no-error.
                assign c-cod-estabel = ordem-compra.cod-estabel.
                for each prazo-compr where 
                    prazo-compra.numero-ordem = 
                    ordem-compra.numero-ordem no-lock:
                    assign de-valor-conver = 0.
                    
                    IF  prazo-compra.qtd-do-forn = 0 THEN
                        PUT SKIP(1) "Ordem: " ordem-compra.numero-ordem " - parcela: " prazo-compra.parcela " esta com a quantidade do fornecedor zerada." .
                    
                    assign de-valor-conver = (ordem-compra.pre-unit-for * prazo-compra.qtd-do-forn) 
                           de-valor-liber  = (ordem-compra.pre-unit-for *  prazo-compra.qtd-sal-forn). 
                    assign item-contrat.sld-qtd             = item-contrat.sld-qtd + prazo-compra.qtd-do-forn
                           item-contrat.sld-val             = item-contrat.sld-val + de-valor-conver
                           item-contrat.sld-qtd-liber       = item-contrat.sld-qtd-liber + prazo-compra.qtd-sal-forn
                           item-contrat.sld-val-liber       = item-contrat.sld-val-liber + de-valor-liber 
                           contrato-for.sld-val             = contrato-for.sld-val       + de-valor-conver
                           contrato-for.sld-val-liber       = contrato-for.sld-val-liber + de-valor-liber.
                    if  avail item-contrat-estab then 
                        assign item-contrat-estab.sld-qtd       = item-contrat-estab.sld-qtd       + prazo-compra.qtd-do-forn
                               item-contrat-estab.sld-val       = item-contrat-estab.sld-val       + de-valor-conver
                               item-contrat-estab.sld-qtd-liber = item-contrat-estab.sld-qtd-liber + prazo-compra.qtd-sal-forn 
                               item-contrat-estab.sld-val-liber = item-contrat-estab.sld-val-liber + de-valor-liber.
                end.
                
                for each recebimento where 
                         recebimento.numero-ordem = ordem-compra.numero-ordem no-lock:
                    assign de-valor-conver = 0.
                    if  item-contrat.mo-codigo <> 0 then 
                        run cdp/cd0812.p (input  0,
                                          input  item-contrat.mo-codigo,                      
                                          input  recebimento.valor-total,
                                          input  recebimento.data-nota,
                                          output de-valor-conver).
                    if  de-valor-conver = 0 or
                        de-valor-conver = ? then assign
                        de-valor-conver = recebimento.valor-total.
                    
                    assign item-contrat.sld-qtd-receb = item-contrat.sld-qtd-receb + 
                                                        if recebimento.cod-movto = 1 then /* Recebido */
                                                        recebimento.qtd-rec-forn else   
                                                        recebimento.qtd-rej-forn * (-1)
                           item-contrat.sld-val-receb = item-contrat.sld-val-receb + 
                                                        if recebimento.cod-movto = 1 then
                                                        de-valor-conver else
                                                        de-valor-conver * (-1)
                           contrato-for.sld-val-receb = contrato-for.sld-val-receb + 
                                                        if recebimento.cod-movto = 1 then
                                                        de-valor-conver else
                                                        de-valor-conver * (-1).
                           
                    if avail item-contrat-estab then 
                    assign item-contrat-estab.sld-qtd-receb = item-contrat-estab.sld-qtd-receb + 
                                                            if recebimento.cod-movto = 1 then /* Recebido */
                                                               recebimento.qtd-rec-forn else
                                                               recebimento.qtd-rej-forn * (-1)
                           item-contrat-estab.sld-val-receb = item-contrat-estab.sld-val-receb + 
                                                              if recebimento.cod-movto = 1 then
                                                              de-valor-conver else
                                                              de-valor-conver * (-1).
                end.
            end.
        end.
    end.

End Procedure.


/* ---------------------------------------------------------------------------- */
Procedure pi-Medicao-total:
/*  -------------------------------------------------------------
    OBJETIVO: Refaz saldo das medicoes quando o contrato e' TOTAL 
    ------------------------------------------------------------- */
    
    def var d-val-rec-total as dec INIT 0 NO-UNDO.
    
    &IF "{&bf_mat_versao_ems}" >= "2.04" &THEN
       if l-matriz-medicao then do:
          run pi-verifica-rateio-medicao.
          if return-value = "NOK":U then return.
       end.
    &ENDIF
    
    FIND FIRST param-global NO-LOCK NO-ERROR.
    IF param-global.modulo-bh THEN
    FOR EACH medicao-contrat WHERE
        medicao-contrat.nr-contrato = contrato-for.nr-contrato NO-LOCK
        BY medicao-contrat.num-seq-medicao:
        IF  medicao-contrat.num-seq-medicao <> i-cont-med THEN
        ASSIGN i-cont-med = i-cont-med + 1.
        IF medicao-contrat.num-seq-medicao <> i-cont-med THEN DO:
            PUT SKIP(1) " ## Contrato: " contrato-for.nr-contrato
                        " n∆o processado. Seq medicao: " i-cont-med
                        " n∆o est† na base (provavelmente banco hist¢rico). " 
                SKIP(1) "".
            RETURN "NOK".
        END.
    END.

    FOR EACH item-contrat WHERE
        item-contrat.nr-contrat = contrato-for.nr-contrato
        EXCLUSIVE-LOCK:
        ASSIGN item-contrat.sld-val       = 0
               item-contrat.sld-val-liber = 0
               item-contrat.sld-val-receb = 0
               item-contrat.sld-qtd       = 0
               item-contrat.sld-qtd-lib   = 0
               item-contrat.sld-qtd-receb = 0.
        /*  Caso tenha eventos, considera os eventos pendentes sem
            medicoes parciais no saldo movimentado                 */
        IF  item-contrat.log-control-event THEN DO:
            FOR EACH evento-ped WHERE
                evento-ped.nr-contrato = contrato-for.nr-contrato AND
                evento-ped.num-seq-item = item-contrat.num-seq-item AND
                evento-ped.ind-sit-event = 1 /* Pendente */   NO-LOCK: 
                IF  item-contrat.ind-caract-item = 1 THEN
                ASSIGN item-contrat.sld-val = item-contrat.sld-val + evento-ped.val-previsto
                       item-contrat.sld-qtd = item-contrat.sld-qtd + evento-ped.qtd-prevista.
                ELSE /* Redutor */
                ASSIGN item-contrat.sld-val = item-contrat.sld-val - evento-ped.val-previsto
                       item-contrat.sld-qtd = item-contrat.sld-qtd - evento-ped.qtd-prevista.

                /*  As medicoes parciais devera descontar, pois j† sera considerado nos proximos calculos */
                FOR EACH medicao-contrat WHERE
                    medicao-contrat.nr-contrato   = evento-ped.nr-contrato AND
                    medicao-contrat.num-seq-item  = evento-ped.num-seq-item AND
                    medicao-contrat.numero-ordem  = evento-ped.numero-ordem AND
                    medicao-contrat.num-seq-event = evento-ped.seq-evento NO-LOCK:
                    IF  item-contrat.ind-caract-item = 1 THEN
                    ASSIGN item-contrat.sld-val = item-contrat.sld-val - medicao-contrat.val-previsto
                           item-contrat.sld-qtd = item-contrat.sld-qtd - medicao-contrat.qtd-prevista.
                    ELSE /* Item Redutor c/ Medicoes Parciais */
                    ASSIGN item-contrat.sld-val = item-contrat.sld-val + medicao-contrat.val-previsto
                           item-contrat.sld-qtd = item-contrat.sld-qtd + medicao-contrat.qtd-prevista.
                END.
            END.
        END.
    END.

    /* Apura por estabelecimento - Existe uma ordem p/ cada estabelecimento */
    for each ordem-compra where 
        ordem-compra.nr-contrato = contrato-for.nr-contrato no-lock:
        
        /* Apura Valor Recebimento no Contrato */
        ASSIGN d-sld-rec-medicao = 0           
               d-val-rec-total   = 0.
        for each recebimento
            where recebimento.numero-ordem = ordem-compra.numero-ordem no-lock:
            ASSIGN de-valor-conver = recebimento.valor-total.
            IF  contrato-for.mo-codigo <> 0 THEN DO:
                run cdp/cd0812.p (input 0,
                                  input contrato-for.mo-codigo,
                                  input  recebimento.valor-total,
                                  input  recebimento.data-nota,
                                  output de-valor-conver).
                if  de-valor-conver = 0 or
                    de-valor-conver = ? then assign
                    de-valor-conver = recebimento.valor-total.
            END. 
            IF  recebimento.cod-movto = 1 THEN
                assign d-val-rec-total   = d-val-rec-total + de-valor-conver.
            ELSE
                assign d-val-rec-total   = d-val-rec-total - de-valor-conver.
        end.
        assign d-sld-rec-medicao = d-val-rec-total.
        
        for each medicao-contrat exclusive-lock
            where medicao-contrat.numero-ordem = ordem-compra.numero-ordem
            by medicao-contrat.dat-prev-medicao:
            
            IF  CAN-FIND(FIRST evento-ped 
                         WHERE evento-ped.nr-contrato   = medicao-contrat.nr-contrato
                           AND evento-ped.num-seq-item  = medicao-contrat.num-seq-item
                           AND evento-ped.numero-ordem  = medicao-contrat.numero-ordem 
                           AND evento-ped.seq-evento    = medicao-contrat.num-seq-event
                           AND evento-ped.ind-sit-event = 3 /* cancelado */ ) THEN
                NEXT.

            FIND item-contrat WHERE 
                 item-contrat.nr-contrat = medicao-contrat.nr-contrat AND
                 item-contrat.num-seq-item = medicao-contrat.num-seq-item 
                 EXCLUSIVE-LOCK NO-ERROR.
            
            IF  tt-param.l-atualiza-medicoes THEN DO:
                IF  medicao-contrat.ind-sit-medicao = 1 THEN DO: 
                    /* ------- medicoes nao liberadas ------- */
                    ASSIGN medicao-contrat.sld-val-medicao = medicao-contrat.val-previsto
                           medicao-contrat.sld-rec-medicao = 0.
                END.
                ELSE /* IF  medicao-contrat.ind-sit-medicao = 2 */ DO: 
                    /* ------- medicoes liberadadas ------- */
                    IF  d-sld-rec-medicao <= 0 THEN
                        assign medicao-contrat.sld-val-medicao = medicao-contrat.val-previsto
                               medicao-contrat.sld-rec-medicao = 0.
                    IF  item-contrat.ind-caract-item = 1 THEN DO:
                        IF  d-sld-rec-medicao > 0 
                        and d-sld-rec-medicao <= medicao-contrat.val-previsto then
                            assign medicao-contrat.sld-val-medicao = medicao-contrat.val-previsto - d-sld-rec-medicao
                                   medicao-contrat.sld-rec-medicao = d-sld-rec-medicao                                   d-sld-rec-medicao = 0.

                        ELSE
                        IF  d-sld-rec-medicao > 0 THEN
                            assign medicao-contrat.sld-val-medicao = 0
                                   medicao-contrat.sld-rec-medicao = medicao-contrat.val-previsto
                                   d-sld-rec-medicao = d-sld-rec-medicao - medicao-contrat.sld-rec-medicao.
                    END.    
                    ELSE DO:                    
                        assign medicao-contrat.sld-val-medicao = medicao-contrat.val-previsto
                               medicao-contrat.sld-rec-medicao = 0.
                    END.
                END.
            END.
            
            ASSIGN de-pre-unit-for            = medicao-contrat.val-previsto / medicao-contrat.qtd-prevista.
            ASSIGN item-contrat.sld-val       = item-contrat.sld-val       + medicao-contrat.val-previsto
                   item-contrat.sld-val-liber = item-contrat.sld-val-liber + medicao-contrat.sld-val-medicao   
                   item-contrat.sld-qtd-lib   = item-contrat.sld-val-liber / de-pre-unit-for 
                   item-contrat.sld-qtd       = item-contrat.sld-qtd       + medicao-contrat.qtd-prevista.
            
            IF  item-contrat.ind-caract-item = 1 THEN
                ASSIGN item-contrat.sld-val-receb = item-contrat.sld-val-receb + medicao-contrat.sld-rec-medicao
                       item-contrat.sld-qtd-receb = item-contrat.sld-val-receb / de-pre-unit-for.
        end.
    end.

    /* Evita problemas de indice - ATENCAO: nao efetuar esta gravacao no for each acima */
    IF tt-param.l-atualiza-medicoes THEN
    FOR EACH medicao-contrat WHERE
        medicao-contrat.nr-contrato  = contrato-for.nr-contrato EXCLUSIVE-LOCK:

        IF medicao-contrat.ind-sit-medicao = 1 THEN DO:
           ASSIGN medicao-contrat.log-rec-medicao = no
                  medicao-contrat.sld-rec-medicao = 0.
           NEXT.
        END.
        ELSE DO:
           IF medicao-contrat.sld-rec-medicao > 0 and
              medicao-contrat.sld-val-medicao = 0 THEN
              ASSIGN medicao-contrat.log-rec-medicao = YES.
           ELSE
              ASSIGN medicao-contrat.log-rec-medicao = NO.
           /* se for redutor e liberado */
           IF item-contrat.ind-caract-item = 2 THEN 
              ASSIGN medicao-contrat.sld-val-medicao = medicao-contrat.val-previsto
                     medicao-contrat.sld-rec-medicao = 0.
        END.
    END.        

    ASSIGN contrato-for.sld-val       = 0
           contrato-for.sld-qtd       = 0
           contrato-for.sld-val-liber = 0
           contrato-for.sld-qtd-lib   = 0
           contrato-for.sld-val-receb = 0.
    FOR EACH item-contrat WHERE
        item-contrat.nr-contrato = contrato-for.nr-contrato NO-LOCK:
        IF  AVAIL item-contrat AND item-contrat.ind-caract-item = 1 THEN
            ASSIGN contrato-for.sld-val       = contrato-for.sld-val       + item-contrat.sld-val
                   contrato-for.sld-qtd       = contrato-for.sld-qtd       + item-contrat.sld-qtd
                   contrato-for.sld-val-liber = contrato-for.sld-val-liber + item-contrat.sld-val-liber
                   contrato-for.sld-qtd-lib   = contrato-for.sld-qtd-lib   + item-contrat.sld-qtd-lib   
                   contrato-for.sld-val-receb = contrato-for.sld-val-receb + item-contrat.sld-val-receb.
        ELSE
            ASSIGN contrato-for.sld-val       = contrato-for.sld-val       - item-contrat.sld-val
                   contrato-for.sld-qtd       = contrato-for.sld-qtd       - item-contrat.sld-qtd
                   contrato-for.sld-val-liber = contrato-for.sld-val-liber - item-contrat.sld-val-liber
                   contrato-for.sld-qtd-lib   = contrato-for.sld-qtd-lib   - item-contrat.sld-qtd-lib   .
  /* ver se este devera ser considerado... contrato-for.sld-val-receb = contrato-for.sld-val-receb - item-contrat.sld-val-receb.*/
    END.

End Procedure.

/* ---------------------------------------------------------------------------- */

Procedure pi-Medicao-item:
/* ----------------------------------------------------------------
   OBJETIVO: Refaz saldo das medicoes quando o contrato e' por ITEM 
   ---------------------------------------------------------------- */


    &IF "{&bf_mat_versao_ems}" >= "2.04" &THEN
       if l-matriz-medicao then do:
          run pi-verifica-rateio-medicao.
          if return-value = "NOK":U then return.
       end.
    &ENDIF

    FIND FIRST param-global NO-LOCK NO-ERROR.
    IF param-global.modulo-bh THEN
    FOR EACH medicao-contrat WHERE
        medicao-contrat.nr-contrato = contrato-for.nr-contrato NO-LOCK
        BY medicao-contrat.num-seq-medicao:
        IF  medicao-contrat.num-seq-medicao <> i-cont-med THEN
        ASSIGN i-cont-med = i-cont-med + 1.
        IF medicao-contrat.num-seq-medicao <> i-cont-med THEN DO:
            PUT SKIP(1) " ## Contrato: " contrato-for.nr-contrato
                        " n∆o processado. Seq medicao: " i-cont-med
                        " n∆o est† na base (provavelmente banco hist¢rico). " 
                SKIP(1) "".
            RETURN "NOK".
        END.
    END.
    FOR EACH ordem-compra WHERE
        ordem-compra.nr-contrato = contrato-for.nr-contrato NO-LOCK
        BREAK BY ordem-compra.num-seq-item :

        FIND item-contrat WHERE
            item-contrat.nr-contrato = ordem-compra.nr-contrato AND
            item-contrat.num-seq-item = ordem-compra.num-seq-item
            EXCLUSIVE-LOCK NO-ERROR.
        IF  NOT AVAIL item-contrat THEN DO:
            /* PUT "Item: " ordem-compra.num-seq-item " n∆o cadastrado para o contrato " ordem-compra.nr-contrato. */
            NEXT.
        END.

        /*  Nao controlado por medicao */
        if  item-contrat.ind-tipo-control <> 1 then 
            NEXT.

        IF FIRST-OF(ordem-compra.num-seq-item) THEN
           ASSIGN item-contrat.sld-val-receb = 0
                  item-contrat.sld-qtd-receb = 0
                  item-contrat.sld-val       = 0
                  item-contrat.sld-qtd       = 0
                  item-contrat.sld-val-liber = 0
                  item-contrat.sld-qtd-lib   = 0
                  item-contrat.sld-val-receb = 0
                  item-contrat.sld-qtd-receb = 0.

        /*  Caso tenha eventos, considera os eventos pendentes sem
            medicoes parciais no saldo movimentado                 */
        IF  item-contrat.log-control-event THEN DO:
            FOR EACH evento-ped WHERE
                evento-ped.nr-contrato = contrato-for.nr-contrato AND
                evento-ped.numero-ordem = ordem-compra.numero-ordem AND
                evento-ped.ind-sit-event = 1 /* Pendente */   NO-LOCK:                
                ASSIGN item-contrat.sld-val = item-contrat.sld-val + evento-ped.val-previsto
                       item-contrat.sld-qtd = item-contrat.sld-qtd + evento-ped.qtd-prevista.
                /*  As medicoes parciais devera descontar, pois j† sera considerado nos proximos calculos */
                FOR EACH medicao-contrat WHERE
                    medicao-contrat.nr-contrato = evento-ped.nr-contrato AND
                    medicao-contrat.num-seq-item = evento-ped.num-seq-item AND
                    medicao-contrat.numero-ordem = evento-ped.numero-ordem AND
                    medicao-contrat.num-seq-event = evento-ped.seq-evento  NO-LOCK:
                    ASSIGN item-contrat.sld-val = item-contrat.sld-val - medicao-contrat.val-previsto
                           item-contrat.sld-qtd = item-contrat.sld-qtd - medicao-contrat.qtd-prevista.
                END.
            END.
        END.
        /* Apura Valor Recebimento no Contrato */
        ASSIGN d-sld-rec-medicao = 0
               d-qtd-rec-medicao = 0
               de-qtd-mov = 0
               de-qtd-lib = 0
               de-qtd-rec = 0
               de-val-mov = 0
               de-val-lib = 0 
               de-val-rec = 0.
        FOR EACH recebimento WHERE
            recebimento.numero-ordem = ordem-compra.numero-ordem
            NO-LOCK:

            ASSIGN de-valor-conver = recebimento.valor-total.
            IF  contrato-for.mo-codigo <> 0 THEN DO:
                run cdp/cd0812.p (input 0,
                                  input contrato-for.mo-codigo,
                                  input  recebimento.valor-total,
                                  input  recebimento.data-movto,
                                  output de-valor-conver).
                if  de-valor-conver = 0 or
                    de-valor-conver = ? then assign
                    de-valor-conver = recebimento.valor-total.
            END.            
            
            IF  recebimento.cod-movto = 1 THEN
                ASSIGN d-sld-rec-medicao = d-sld-rec-medicao + de-valor-conver 
                       d-qtd-rec-medicao = d-qtd-rec-medicao + recebimento.qtd-rec. /*recebimento.quant-receb.*/ 
            ELSE
                ASSIGN d-sld-rec-medicao = d-sld-rec-medicao - de-valor-conver
                       d-qtd-rec-medicao = d-qtd-rec-medicao - recebimento.qtd-rej-forn . /*recebimento.quant-receb.*/
        END.

        /**
        IF  item-contrat.sld-val-receb <> d-sld-rec-medicao THEN 
            PUT SKIP(2) "## ERRO - Saldo Recebido no contrato: " item-contrat.sld-val-receb SKIP
                            "nao confere com o saldo calculado: " d-sld-rec-medicao.
        **/
        ASSIGN item-contrat.sld-val-receb = item-contrat.sld-val-receb + d-sld-rec-medicao
               item-contrat.sld-qtd-receb = item-contrat.sld-qtd-receb + d-qtd-rec-medicao.
        
        FOR EACH medicao-contrat EXCLUSIVE-LOCK WHERE
            medicao-contrat.numero-ordem = ordem-compra.numero-ordem AND
            medicao-contrat.num-seq-item = ordem-compra.num-seq-item AND
            medicao-contrat.nr-contrat   = ordem-compra.nr-contrato
            by medicao-contrat.dat-prev-medicao:
            
            IF  CAN-FIND(FIRST evento-ped 
                         WHERE evento-ped.nr-contrato   = medicao-contrat.nr-contrato
                           AND evento-ped.num-seq-item  = medicao-contrat.num-seq-item
                           AND evento-ped.numero-ordem  = medicao-contrat.numero-ordem 
                           AND evento-ped.seq-evento    = medicao-contrat.num-seq-event
                           AND evento-ped.ind-sit-event = 3 /* cancelado */ ) THEN
                NEXT.

            IF  tt-param.l-atualiza-medicoes THEN DO:
                IF  medicao-contrat.ind-sit-medicao = 1 THEN DO: 
                    /* ------- medicoes nao liberadas ------- */
                    ASSIGN medicao-contrat.sld-val-medicao = medicao-contrat.val-previsto
                           medicao-contrat.sld-rec-medicao = 0
                           medicao-contrat.log-rec-medicao = no.
                END.            
                
                IF  medicao-contrat.ind-sit-medicao = 2 THEN DO: 
                    /* ------- medicoes liberadas ------- */
                    IF d-sld-rec-medicao <= 0 THEN
                        assign medicao-contrat.sld-val-medicao = medicao-contrat.val-previsto
                               medicao-contrat.sld-rec-medicao = 0.
                    
                    IF  d-sld-rec-medicao > 0 THEN DO:
                        
                        if  d-sld-rec-medicao <= medicao-contrat.val-previsto then
                            assign medicao-contrat.sld-val-medicao = medicao-contrat.val-previsto - d-sld-rec-medicao
                                   medicao-contrat.sld-rec-medicao = d-sld-rec-medicao
                                   d-sld-rec-medicao = 0.
                        else
                            assign medicao-contrat.sld-val-medicao = 0
                                   medicao-contrat.sld-rec-medicao = medicao-contrat.val-previsto
                                   d-sld-rec-medicao = d-sld-rec-medicao - medicao-contrat.sld-rec-medicao.
                    END.
                END. 
            END.
            
            ASSIGN de-pre-unit-for = medicao-contrat.val-previsto / medicao-contrat.qtd-prevista
                   de-val-mov      = de-val-mov + medicao-contrat.val-previsto
                   de-qtd-mov      = de-qtd-mov + medicao-contrat.qtd-prevista.
            IF  medicao-contrat.ind-sit-medicao = 2 THEN DO:
                ASSIGN de-val-lib = de-val-lib + medicao-contrat.sld-val-medicao
                       de-qtd-lib = de-qtd-lib + medicao-contrat.sld-val-medicao / de-pre-unit-for
                       /* Estas 2 quantidades estao sendo atualizadas no bloco acima, de acordo com os recebimentos */
                       de-val-rec = de-val-rec + medicao-contrat.sld-rec-medicao    
                       de-qtd-rec = de-qtd-rec + medicao-contrat.sld-rec-medicao / de-pre-unit-for.
            END.
        END.
        
        /* Evita problemas de indice - ATENCAO: nao efetuar esta gravacao no for each acima */
        IF  tt-param.l-atualiza-medicoes THEN 
        FOR EACH medicao-contrat WHERE
            medicao-contrat.nr-contrato = item-contrat.nr-contrato AND
            medicao-contrat.num-seq-item = item-contrat.num-seq-item EXCLUSIVE-LOCK:
            if  medicao-contrat.sld-val-medicao = 0 then
                assign medicao-contrat.log-rec-medicao = yes.
            else
                assign medicao-contrat.log-rec-medicao = no.
        END.

        ASSIGN item-contrat.sld-val       = item-contrat.sld-val  + de-val-mov 
               item-contrat.sld-qtd       = item-contrat.sld-qtd + de-qtd-mov /* o preco pode ser alterado item-contrat.sld-val     / item-contrat.pre-unit-for */
               item-contrat.sld-val-liber = item-contrat.sld-val-liber + de-val-lib
               item-contrat.sld-qtd-lib   = item-contrat.sld-qtd-lib + de-qtd-lib. /* o preco pode ser alterado item-contrat.sld-val-lib / item-contrat.pre-unit-for. */
                        /** Validaá∆o Mercantil - Eliane ***/
        IF (item-contrat.sld-val - item-contrat.sld-val-rec = 0) AND 
           item-contrat.sld-val-liber <> 0 THEN
           ASSIGN item-contrat.sld-val-liber = 0.
        IF (item-contrat.sld-qtd - item-contrat.sld-qtd-rec = 0) AND 
           item-contrat.sld-qtd-liber <> 0 THEN
           ASSIGN item-contrat.sld-qtd-liber = 0.
        /*************************************/
    END.
    
    ASSIGN contrato-for.sld-qtd       = 0
           contrato-for.sld-val       = 0 
           contrato-for.sld-val-liber = 0
           contrato-for.sld-qtd-lib   = 0
           contrato-for.sld-val-receb = 0.
    FOR EACH item-contrat WHERE
        item-contrat.nr-contrato = contrato-for.nr-contrato NO-LOCK:
        ASSIGN contrato-for.sld-val       = contrato-for.sld-val + item-contrat.sld-val
               contrato-for.sld-qtd       = contrato-for.sld-qtd + item-contrat.sld-qtd
               contrato-for.sld-val-liber = contrato-for.sld-val-liber + item-contrat.sld-val-liber
               contrato-for.sld-qtd-lib   = contrato-for.sld-qtd-lib   + item-contrat.sld-qtd-lib   
               contrato-for.sld-val-receb = contrato-for.sld-val-receb + item-contrat.sld-val-receb.
    END.

End Procedure.



Procedure pi-verifica-rateio-medicao:
     /* Quando uma das mediá‰es j† est† recebida, e existe rateio de matriz por mediá∆o, o rec†lculo 
        se torna um procedimento muito complexo, pois para cada mediá∆o e matriz de rateio deveria haver uma 
        determinada contabilizaá∆o. Se o recalculo for feito sem considerar estes fatores, a concialiaá∆o 
        entre o que foi contabilizado e as mediá‰es n∆o ir† mais fechar. A liberaá∆o do programa spcn001 nestes
        casos deve ser feita apenas quando as mediá‰es estiverem 100% recebidas, e mesmo
        assim h† o risco da concialiá∆o n∆o fechar. Como este recalculo Ç um procedimento que teoricamente que n∆o deveria
        ser necess†rio, o importante Ç descobrir a causa, para ent∆o providenciar soluá∆o e melhor correá∆o. 
        No desenvolvimento da matriz da mediá∆o, v†rios causas de problemas de saldo foram corrigidas. A estrutura
        desta funá∆o tambÇm foi montada para garantir maior seguranáa no saldo. 
        Outra observaá∆o importante Ç que quando a matriz est† integrada com investimentos, este programa n∆o corrige
        as informaá‰es de investimentos, mais um motivo que justifica a liberaá∆o restrita deste programa para matriz 
        de rateio por mediá∆o. */

     &IF "{&bf_mat_versao_ems}" >= "2.04" &THEN
       if l-matriz-medicao and tt-param.l-atualiza-medicoes then do:
          if can-find (first matriz-rat-med where
                             matriz-rat-med.nr-contrato = contrato-for.nr-contrato) then do:
             for each ordem-compra
                where ordem-compra.nr-contrato = ordem-compra.nr-contrato no-lock:
                    if can-find (first recebimento where
                                       recebimento.numero-ordem = ordem-compra.numero-ordem) then do:

                        put unformatted skip(1) "******************************************************************************" skip
                                                "   Contrato " + string(contrato-for.nr-contrato) +
                                                " com matriz de rateio por mediá∆o, e j† possui recebimento(s)." skip
                                                space(15) "Saldo das mediá‰es n∆o pode ser recalculado !" skip
                                                "******************************************************************************" skip(1).
                        return "NOK":U.
                    end.

             end.
          end.
       end.
    &ENDIF
    return "OK":U.
end.

/* fim-do-programa */


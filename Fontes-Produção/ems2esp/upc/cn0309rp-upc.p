/*****************************************************************************************
** Programa: 
** Autor...: Log¡stica (log339640)
** Data....: 07/2008
** OBS.....: 
** Objetivo: 
*****************************************************************************************/

{include/i-prgvrs.i cn0309rp-upc 2.00.00.000}  /*** 010000 ***/

{include/i-epc200.i}

DEF INPUT PARAM p-ind-event  AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

DEFINE VARIABLE r-contrato-for AS ROWID      NO-UNDO.
DEFINE VARIABLE r-item-contrat AS ROWID      NO-UNDO.
DEFINE VARIABLE h-esapi001     AS HANDLE     NO-UNDO.

DEFINE VARIABLE cod-empresa  like param-global.empresa-prin no-undo.
/*{cdp\cd0669.i}*/
/*******************************************************************
**
**  CD0666.I - Definicao temp-table de erros
**
*******************************************************************/
def new global shared var l-multi as logical initial yes.

def var l-del-erros as logical init yes.
def var v-nom-arquivo-cb as char format "x(50)" no-undo.
def var c-mensagem-cb    as char format "x(132)" no-undo.

def   temp-table tt-erro no-undo
    field i-sequen as int             
    field cd-erro  as int
    field mensagem as char format "x(255)"
    field c-param  as char.

form
    space(04)
    tt-erro.cd-erro 
    space (02)
    c-mensagem-cb
    with width 132 no-box down stream-io frame f-consiste.

{utp/ut-liter.i Mensagem}
assign tt-erro.cd-erro:label in frame f-consiste = trim(return-value).

{utp/ut-liter.i Descri‡Æo}
assign c-mensagem-cb:label in frame f-consiste = trim(return-value).

if p-ind-event = "delete-extensao" then do:

    for each  tt-epc
        where tt-epc.cod-event = p-ind-event:

        if tt-epc.cod-parameter = "rowid-contrato-for" then
            assign r-contrato-for = to-rowid(tt-epc.val-parameter).
        if tt-epc.cod-parameter = "rowid-item-contrat" then
            assign r-item-contrat = to-rowid(tt-epc.val-parameter).
    end.

    run esp\esapi001.p persistent set h-esapi001.
    find first param-global no-lock no-error.

    find first item-contrat no-lock
         where rowid(item-contrat) = r-item-contrat no-error.

    if avail item-contrat then do:

        find first contrato-for no-lock
             where contrato-for.nr-contrato = item-contrat.nr-contrato no-error.
        if avail contrato-for then do:
            find first estabelec no-lock
                 where estabelec.cod-estabel = contrato-for.cod-estabel no-error.
            if avail estabelec then
                assign cod-empresa = estabelec.ep-codigo.
        end.
        else do:
            find first param-global no-lock no-error.
            assign cod-empresa = param-global.empresa-prin.
        end.

        find first item-contrat-ext EXCLUSIVE-LOCK
             where item-contrat-ext.nr-contrato       = item-contrat.nr-contrato
             and   item-contrat-ext.num-seq-item      = item-contrat.num-seq-item no-error.

        if avail item-contrat-ext then do:
            for each  controle-inv-esp exclusive-lock
                where controle-inv-esp.ep-codigo    = item-contrat-ext.ep-codigo
                and   controle-inv-esp.num-ord-inv  = item-contrat-ext.num-ord-inv
                and   controle-inv-esp.nr-contrato  = item-contrat-ext.nr-contrato
                and   controle-inv-esp.num-seq-item = item-contrat-ext.num-seq-item:

                if valid-handle(h-esapi001) then do: 
                    run pi-principal in h-esapi001 (input 2,
                                                    input rowid(item-contrat),
                                                    input 3,
                                                    output table tt-erro).
                    if can-find (first tt-erro) then do:
                        if valid-handle(h-esapi001) then do:
                            delete procedure h-esapi001.
                            assign h-esapi001 = ?.
                        end.
                        return "NOK":U.
                    end.
                end.
                delete controle-inv-esp.
            end.
            delete item-contrat-ext.
        end.
    end.

    find first contrato-for no-lock
         where rowid(contrato-for) = r-contrato-for no-error.

    if avail contrato-for then do:
        find first contrato-for-ext EXCLUSIVE-LOCK
             WHERE contrato-for-ext.nr-contrato = contrato-for.nr-contrato no-error.

        if avail contrato-for-ext then do:
            for each  controle-inv-esp exclusive-lock
                where controle-inv-esp.ep-codigo    = contrato-for-ext.ep-codigo
                and   controle-inv-esp.num-ord-inv  = contrato-for-ext.num-ord-inv
                and   controle-inv-esp.nr-contrato  = contrato-for-ext.nr-contrato:
        
                if valid-handle(h-esapi001) then do: 
                    run pi-principal in h-esapi001 (input 1,
                                                    input rowid(contrato-for),
                                                    input 3,
                                                    output table tt-erro).
                    if can-find (first tt-erro) then do:
                        if valid-handle(h-esapi001) then do:
                            delete procedure h-esapi001.
                            assign h-esapi001 = ?.
                        end.
                        return "NOK":U.
                    end.
                end.
                delete controle-inv-esp.
            end.
            delete contrato-for-ext.
        END.
    end.

    if valid-handle(h-esapi001) then do:
        delete procedure h-esapi001.
        assign h-esapi001 = ?.
    end.

end.

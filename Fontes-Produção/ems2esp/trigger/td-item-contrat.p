/**************************************************************************
**
**  Eliminar a tabela de extensao da item-contrat
**  Eliminar a tabela controle-inv-esp relacionada
**
***************************************************************************/
DEF PARAM BUFFER bfitem-contrat FOR item-contrat.

DEFINE VARIABLE h-esapi001   AS HANDLE NO-UNDO.

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

def temp-table tt-erro no-undo
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

run esp\esapi001.p persistent set h-esapi001.
find first param-global no-lock no-error.

FOR EACH  item-contrat-ext EXCLUSIVE-LOCK
    WHERE item-contrat-ext.nr-contrato       = bfitem-contrat.nr-contrato
    AND   item-contrat-ext.num-seq-item      = bfitem-contrat.num-seq-item:

    for each  controle-inv-esp exclusive-lock
        where controle-inv-esp.ep-codigo    = item-contrat-ext.ep-codigo
        and   controle-inv-esp.num-ord-inv  = item-contrat-ext.num-ord-inv
        and   controle-inv-esp.nr-contrato  = item-contrat-ext.nr-contrato
        and   controle-inv-esp.num-seq-item = item-contrat-ext.num-seq-item:

        if valid-handle(h-esapi001) then do: 
            find first param-inv no-lock 
                 where param-inv.ep-codigo = controle-inv-esp.ep-codigo no-error.

            run pi-atualiza-verba in h-esapi001 (input 3,
                                                 input controle-inv-esp.ep-codigo,
                                                 input controle-inv-esp.num-ord-inv,
                                                 input controle-inv-esp.dt-trans,
                                                 input param-inv.moeda-inv,
                                                 input controle-inv-esp.ent-comp * -1,
                                                 input controle-inv-esp.ent-real * -1,
                                                 output table tt-erro).
            if can-find (first tt-erro) then do:
                run cdp/cd0669.w (input table tt-erro).
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
END.

if valid-handle(h-esapi001) then do:
    delete procedure h-esapi001.
    assign h-esapi001 = ?.
end.

RETURN "OK".

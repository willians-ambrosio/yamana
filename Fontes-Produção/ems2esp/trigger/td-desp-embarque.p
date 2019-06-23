/**************************************************************************
**
**  Eliminar a tabela de extensao da desp-embarque
**  Eliminar a tabela controle-inv-esp relacionada
**
***************************************************************************/

DEF PARAM BUFFER bfdesp-embarque FOR desp-embarque.

DEFINE VARIABLE h-esapi001   AS HANDLE NO-UNDO.

/*******************************************************************
**
**  CD0666.I - Definicao temp-table de erros
**
*******************************************************************/
def new global shared var l-multi as logical initial yes.

def var l-del-erros as logical init yes.
def var v-nom-arquivo-cb as char format "x(50)" no-undo.
def var c-mensagem-cb    as char format "x(132)" no-undo.

def  temp-table tt-erro no-undo
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

/**********************************************************************
**
**  UT-LITER.I - Chamada pardr’o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Mensagem",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign tt-erro.cd-erro:label in frame f-consiste = trim(return-value).

/**********************************************************************
**
**  UT-LITER.I - Chamada pardr’o para UT-LITER.P
**
*********************************************************************/

run utp/ut-liter.p (input "Descri»’o",
                    input "",
                    input "") no-error.
                    
                    
/* ut-liter.i */                    
 
assign c-mensagem-cb:label in frame f-consiste = trim(return-value).
 

run esp\esapi001.p persistent set h-esapi001.

FOR EACH  desp-embarque-ext EXCLUSIVE-LOCK
    WHERE desp-embarque-ext.cod-estabel       = bfdesp-embarque.cod-estabel
    AND   desp-embarque-ext.embarque          = bfdesp-embarque.embarque
    AND   desp-embarque-ext.cod-itiner        = bfdesp-embarque.cod-itiner
    AND   desp-embarque-ext.cod-pto-contr     = bfdesp-embarque.cod-pto-contr
    AND   desp-embarque-ext.cod-desp          = bfdesp-embarque.cod-desp
    AND   desp-embarque-ext.cod-emitente-desp = bfdesp-embarque.cod-emitente-desp:

    for each  controle-inv-esp exclusive-lock
        where controle-inv-esp.ep-codigo   = desp-embarque-ext.ep-codigo
        and   controle-inv-esp.num-ord-inv = desp-embarque-ext.num-ord-inv:

        if valid-handle(h-esapi001) then do: 
            run pi-principal in h-esapi001 (input 4,
                                            input rowid(desp-embarque),
                                            input 3,
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
    delete desp-embarque-ext.
END.

if valid-handle(h-esapi001) then do:
    delete procedure h-esapi001.
    assign h-esapi001 = ?.
end.

RETURN "OK".



/*****************************************************************************************
** Programa: re0402rf-upc.p
** Autor...: Log°stica (log339640)
** Data....: 07/2008
** OBS.....: UPC utilizada pelo programa RE0402RF.P
** Objetivo: Ao efetuar a desatualizaá∆o da nota de importaá∆o (embarque vinculado 
             Ö despesa) ser† atualizado o valor COMPROMISSADO da verba. Ser† retirado 
             o valor realizado do documento e inclu°do o valor compromissado para 
             as despesas de importaá∆o na tabela espec°fica. O controle de estouro 
             de verba ser† o mesmo do produto padr∆o -  n∆o ser† exibida mensagem e 
             sempre ser† gravado as informaá‰es.
*****************************************************************************************/

{include/i-prgvrs.i re0402rf-upc 2.00.00.000}  /*** 010000 ***/

{include/i-epc200.i}

DEF INPUT PARAM p-ind-event  AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

DEFINE VARIABLE r-rowid     AS ROWID      NO-UNDO.
DEFINE VARIABLE h-esapi001  AS HANDLE     NO-UNDO.

{cdp\cd0669.i}

run esp\esapi001.p persistent set h-esapi001.

if p-ind-event = "antes-desatualizacao-investimentos" then do:
    find first tt-epc no-lock
         where tt-epc.cod-event     = p-ind-event
         and   tt-epc.cod-parameter = "docum-est-rowid" no-error.
    if avail tt-epc then
        assign r-rowid = to-rowid(tt-epc.val-parameter).

    for each  tt-epc 
        where tt-epc.cod-event = "tt-erro":
        delete tt-epc.
    end.

    find first docum-est no-lock
         where rowid(docum-est) = r-rowid no-error.

    if avail docum-est then do:
        if valid-handle(h-esapi001) then do: 
            
            run pi-principal in h-esapi001 (input 5,  /*Atualiza NF Despesas*/
                                            input rowid(docum-est),
                                            input 3,  /*Modificaá∆o*/
                                            output table tt-erro).

            if can-find (first tt-erro) then do:
                run pi-cria-erro in this-procedure (input tt-erro.cd-erro,
                                                    input tt-erro.mensagem).
                delete procedure h-esapi001.
                assign h-esapi001 = ?.
                return "NOK":U.
            end.
            delete procedure h-esapi001.
            assign h-esapi001 = ?.
        end.
    end.
    return "OK":U.
end.

procedure pi-cria-erro:

    DEFINE INPUT  PARAMETER i-cd-erro  AS INTEGER    NO-UNDO.
    DEFINE INPUT  PARAMETER c-mensagem AS CHARACTER  NO-UNDO.

    create tt-epc.
    assign tt-epc.cod-event = "tt-erro"
           tt-epc.cod-parameter = string(i-cd-erro)
           tt-epc.val-parameter = c-mensagem.

end procedure.

/*******************************************************
** Autor...: Gustavo Eduardo Tamanini
** Programa: BOIN738-UPC
** UPC cadastrada para programa: BOIN738
*******************************************************/
{include/i-prgvrs.i BOIN738-UPC 2.06.00.000}
{include/i-epc200.i}

define temp-table tt-req-ord-produc no-undo like req-ord-produc
    field r-Rowid as rowid.

def input param p-ind-event as char no-undo.
def input-output param table for tt-epc.

define variable h-bo as handle  no-undo.

find first tt-epc 
     where tt-epc.cod-event     = p-ind-event
     and   tt-epc.cod-parameter = "OBJECT-HANDLE" no-lock no-error.

if avail tt-epc then do:
    assign h-bo = widget-handle(tt-epc.val-parameter).
end.

/** Global - FO 1716.704 **/
define new global shared var wh-fi-tecnico-0738 as widget-handle no-undo.

/** FO 1716.638 - Obriga o usuario a informar uma tarefa
    na solicitaá∆o de compra 
    FO 1716.704 - Valida Tecnico informado  **/
if (p-ind-event = "beforeCreateRecord":U  or
    p-ind-event = "beforeUpdateRecord":U) then do:
    /** Busca motorista corrente da BO **/
    run getRecord in h-bo (output table tt-req-ord-produc).

    for first tt-req-ord-produc no-lock:
        if tt-req-ord-produc.num-tarefa = 0 then do:
            {utp/ut-liter.i ê_necess†rio_informar_uma_Tarefa_para_a_solicitaá∆o_de_serviáo *}
            run pi-cria-erro (input return-value + ".":U).
            return "NOK":U.
        end.

        for first ord-prod
            where ord-prod.nr-ord-produ = tt-req-ord-produc.nr-ord-produ no-lock:
        end.
        if avail ord-prod then do:
            if ord-prod.origem = "MI":U then do:
                /** Valida se tarefa existe **/
                if not can-find(first ord-taref
                                where ord-taref.nr-ord-produ = tt-req-ord-produc.nr-ord-produ 
                                and   ord-taref.cd-tarefa    = tt-req-ord-produc.num-tarefa no-lock) then do:
                    {utp/ut-liter.i Tarefa_n∆o_cadastrada_para_a_Ordem_de_Mantená∆o *}
                    run pi-cria-erro (input return-value + "!":U).
                    return "NOK":U.
                end.
                if valid-handle(wh-fi-tecnico-0738) then do:
                    if wh-fi-tecnico-0738:screen-value <> "":U then do:
                        /** Valida se tecnico existe **/
                        if not can-find(first tecn-mi
                                        where tecn-mi.cd-tecnico = wh-fi-tecnico-0738:screen-value no-lock) then do:
                            {utp/ut-liter.i TÇcnico_informado_inexistente *}
                            run pi-cria-erro (input return-value + "!":U).
                            return "NOK":U.
                        end.
                    end.
                end.
            end.
            if ord-prod.origem = "MV":U then do:
                /** Valida se tarefa existe **/
                if not can-find (first  mmv-tar-ord-manut
                                 where  mmv-tar-ord-manut.nr-ord-produ = tt-req-ord-produc.nr-ord-produ
                                 and    mmv-tar-ord-manut.num-seq      = tt-req-ord-produc.num-tarefa no-lock) then do:
                    {utp/ut-liter.i Tarefa_n∆o_cadastrada_para_a_Ordem_de_Mantená∆o *}
                    run pi-cria-erro (input return-value + "!":U).
                    return "NOK":U.
                end.
                if valid-handle(wh-fi-tecnico-0738) then do:
                    if wh-fi-tecnico-0738:screen-value <> "":U then do:
                        /** Valida se tecnico existe **/
                        if not can-find(first mmv-func-ofici
                                        where mmv-func-ofici.cod-matr = wh-fi-tecnico-0738:screen-value no-lock) then do:
                            {utp/ut-liter.i TÇcnico_informado_inexistente *}
                            run pi-cria-erro (input return-value + "!":U).
                            return "NOK":U.
                        end.
                    end.
                end.
            end.
        end.
    end.
end.
/****/

/** FO 1716.704 - Criar tabela especifica para guardar
    o Tecnico da Requisicao. **/
if (p-ind-event = "afterCreateRecord":U  or
    p-ind-event = "afterUpdateRecord":U) then do:

    run getRecord in h-bo (output table tt-req-ord-produc).
    
    if  valid-handle(wh-fi-tecnico-0738) then do:
        for first tt-req-ord-produc no-lock:
            if not can-find(first mmi-tec-req-prod 
                            where mmi-tec-req-prod.nr-requisicao = tt-req-ord-produc.nr-requisicao
                            and   mmi-tec-req-prod.sequencia     = tt-req-ord-produc.sequencia no-lock) then do:
                create mmi-tec-req-prod.
                assign mmi-tec-req-prod.nr-requisicao = tt-req-ord-produc.nr-requisicao
                       mmi-tec-req-prod.sequencia     = tt-req-ord-produc.sequencia
                       mmi-tec-req-prod.cd-tecnico    = wh-fi-tecnico-0738:screen-value.
            end.
            else do:
                for first mmi-tec-req-prod 
                    where mmi-tec-req-prod.nr-requisicao = tt-req-ord-produc.nr-requisicao
                    and   mmi-tec-req-prod.sequencia     = tt-req-ord-produc.sequencia exclusive-lock:
                    assign mmi-tec-req-prod.cd-tecnico   = wh-fi-tecnico-0738:screen-value.
                end.
            end.
        end.
    end.
end.


procedure pi-cria-erro:
define input parameter msg as char no-undo.
    create tt-epc.
    assign tt-epc.cod-event     = "ERROR"
           tt-epc.cod-parameter = "EPC-ERROR" 
           tt-epc.val-parameter = msg.
end.

return "OK":U.

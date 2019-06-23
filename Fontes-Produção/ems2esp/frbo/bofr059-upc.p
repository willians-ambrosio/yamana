/*******************************************************
** Autor...: Gustavo Eduardo Tamanini
** Programa: bofr059-UPC
** UPC cadastrada para programa: bofr059
*******************************************************/
{include/i-prgvrs.i BOFR059-UPC 2.06.00.000}
{include/i-epc200.i}

def temp-table tt-mab-movto-event no-undo like mab-movto-event
    field r-Rowid as rowid.

def input param p-ind-event  as char no-undo.
def input-output param table for tt-epc.

/** Variaveis do programa AB0303-UPC **/
def new global shared var wh-fi-espec-0303       as widget-handle no-undo.
def new global shared var wh-fi-horas-espec-0303 as widget-handle no-undo.
def new global shared var wh-combo-box-0303      as widget-handle no-undo.

def var h-bo as handle no-undo.

find first tt-epc 
     where tt-epc.cod-event     = p-ind-event
     and   tt-epc.cod-parameter = "OBJECT-HANDLE" no-lock no-error.
if  avail tt-epc then
    assign h-bo = widget-handle(tt-epc.val-parameter).

/** FO 1716.308 **/
if (p-ind-event = "beforeCreateRecord":U  or
    p-ind-event = "beforeUpdateRecord":U) then do:

    /** Retorna temp-table da BO **/
    run getRecord in h-bo (output table tt-mab-movto-event).

    if  valid-handle(wh-fi-espec-0303)       and
        valid-handle(wh-fi-horas-espec-0303) then do:
        /** Valida Especialidade**/
        for first tt-mab-movto-event no-lock:
            if wh-fi-espec-0303:screen-value <> "":U then
                if not can-find(first mmv-especialid-func
                                where mmv-especialid-func.cod-especialid = wh-fi-espec-0303:screen-value) then do:
                    {utp/ut-liter.i "Especialidade_informada_inexistente." *}
                    run pi-cria-erro (input return-value).
                    return "NOK":U.
                end.
            if wh-fi-espec-0303:screen-value = "":U then do:
                {utp/ut-liter.i "Especialidade_deve_ser_informada!" *}
                run pi-cria-erro (input return-value).
                return "NOK":U.
            end.
            if wh-fi-horas-espec-0303:screen-value = "":U then do:
                {utp/ut-liter.i "Horas_Especialidade_deve_ser_informada!" *}
                run pi-cria-erro (input return-value).
                return "NOK":U.
            end.
        end.
    end.
end.
/****/

/** FO 1716.308 - Criar tabela especifica para guardar
    a Especialidade e Horas. **/
if (p-ind-event = "afterCreateRecord":U  or
    p-ind-event = "afterUpdateRecord":U) then do:

    /** Retorna temp-table da BO **/
    run getRecord in h-bo (output table tt-mab-movto-event).

    /** Guarda Horas da Especialidade **/
    if  valid-handle(wh-fi-espec-0303)       and
        valid-handle(wh-fi-horas-espec-0303) then do:

        for first tt-mab-movto-event no-lock:
            if not can-find(first mab-evento
                            where mab-evento.num-docto = tt-mab-movto-event.num-docto no-lock) then do:
                create mab-evento.
                assign mab-evento.num-docto      = tt-mab-movto-event.num-docto
                       mab-evento.cod-especialid = wh-fi-espec-0303:screen-value
                       mab-evento.horas-espec    = decimal(wh-fi-horas-espec-0303:screen-value).
            end.
            else do:
                for first mab-evento
                    where mab-evento.num-docto       = tt-mab-movto-event.num-docto exclusive-lock:
                    assign mab-evento.num-docto      = tt-mab-movto-event.num-docto
                           mab-evento.cod-especialid = wh-fi-espec-0303:screen-value
                           mab-evento.horas-espec    = decimal(wh-fi-horas-espec-0303:screen-value).
                end.
            end.
        end.
    end.

    /** Guardar o Status do Evento **/
    IF VALID-HANDLE(wh-combo-box-0303) THEN DO:
        FOR FIRST mab-event-status
            WHERE mab-event-status.num-docto = tt-mab-movto-event.num-docto EXCLUSIVE-LOCK:
        END.
        IF NOT AVAIL mab-event-status THEN DO:
            CREATE mab-event-status.
            ASSIGN mab-event-status.num-docto = tt-mab-movto-event.num-docto.
        END.
        IF INT({ydminc/i00ydm001.i 06 wh-combo-box-0303:SCREEN-VALUE}) <> mab-event-status.idi-status-ord THEN
            ASSIGN mab-event-status.data           = TODAY
                   mab-event-status.hora           = SUBSTRING(STRING(TIME,"HH:MM:SS"),1,2) + SUBSTRING(STRING(TIME,"HH:MM:SS"),4,2)
                   mab-event-status.idi-status-ord = {ydminc/i00ydm001.i 06 wh-combo-box-0303:SCREEN-VALUE}.
    END.
end.

/** FO 1716.308 - Deleta tabela especifica **/
if p-ind-event = "afterDeleteRecord":U then do:
    run getRecord in h-bo (output table tt-mab-movto-event).
    for first tt-mab-movto-event no-lock:
        for first mab-evento
            where mab-evento.num-docto = tt-mab-movto-event.num-docto exclusive-lock:
            delete mab-evento.
        END.
        FOR FIRST mab-event-status
            WHERE mab-event-status.num-docto = tt-mab-movto-event.num-docto EXCLUSIVE-LOCK:
            DELETE mab-event-status.
        END.
        FOR EACH mab-evento-mat
            WHERE mab-evento-mat.num-docto = tt-mab-movto-event.num-docto EXCLUSIVE-LOCK:
            DELETE mab-evento-mat.
        END.
    end.
end.

procedure pi-cria-erro :
    def input param msg as char no-undo.

    create tt-epc.
    assign tt-epc.cod-event     = "ERROR"
           tt-epc.cod-parameter = "EPC-ERROR" 
           tt-epc.val-parameter = msg.
end.

return "OK":U.

/*******************************************************
** Autor...: Gustavo Eduardo Tamanini
** Data....: 03-10-2008
** Programa: bofr016-UPC
** UPC cadastrada para programa: bofr016
*******************************************************/
{include/i-prgvrs.i BOFR016-UPC 2.00.00.000}
{include/i-epc200.i}

DEF TEMP-TABLE tt-mab-model NO-UNDO LIKE mab-model
    FIELD r-Rowid AS ROWID.

DEF INPUT PARAM p-ind-event AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

DEF VAR h-bo AS HANDLE NO-UNDO.

/** Variaveis do programa AB0104A-UPC **/
DEF NEW GLOBAL SHARED VAR wh-fi-fator-0104 AS WIDGET-HANDLE NO-UNDO.

FIND FIRST tt-epc 
     WHERE tt-epc.cod-event     = p-ind-event
       AND tt-epc.cod-parameter = "OBJECT-HANDLE" NO-LOCK NO-ERROR.
IF AVAIL tt-epc THEN
    ASSIGN h-bo = WIDGET-HANDLE(tt-epc.val-parameter).

/** FO 1708.134 - Criar tabela especifica para guardar
    o fator de conversao para os indicadores. **/
IF (p-ind-event = "afterCreateRecord":U  OR
    p-ind-event = "afterUpdateRecord":U) THEN DO:
    /** Retorna temp-table da BO **/
    RUN getRecord IN h-bo (OUTPUT TABLE tt-mab-model).

    IF  VALID-HANDLE(wh-fi-fator-0104) THEN DO:        
        FOR FIRST tt-mab-model NO-LOCK:
            IF NOT CAN-FIND(FIRST mab-model-esp
                            WHERE mab-model-esp.cod-model = tt-mab-model.cod-model NO-LOCK) THEN DO:
                CREATE mab-model-esp.
                ASSIGN mab-model-esp.cod-model    = tt-mab-model.cod-model
                       mab-model-esp.fator-conver = DECIMAL(wh-fi-fator-0104:SCREEN-VALUE).
            END.
            ELSE
                FOR FIRST mab-model-esp
                    WHERE mab-model-esp.cod-model = tt-mab-model.cod-model EXCLUSIVE-LOCK:
                    ASSIGN mab-model-esp.fator-conver = DECIMAL(wh-fi-fator-0104:SCREEN-VALUE).
                END.
        END.
    END.
END.

/** FO 1708.134 - Deleta tabela especifica do Fator de Operacao. **/
IF p-ind-event = "afterDeleteRecord":U THEN DO:
    RUN getRecord IN h-bo (OUTPUT TABLE tt-mab-model).
    FOR FIRST tt-mab-model NO-LOCK,
        FIRST mab-model-esp
        WHERE mab-model-esp.cod-model = tt-mab-model.cod-model EXCLUSIVE-LOCK:
        DELETE mab-model-esp.
    END.
END.

PROCEDURE pi-cria-erro :
    DEFINE INPUT PARAMETER msg AS CHAR NO-UNDO.

    CREATE tt-epc.
    ASSIGN tt-epc.cod-event     = "ERROR"
           tt-epc.cod-parameter = "EPC-ERROR" 
           tt-epc.val-parameter = msg.
END.

RETURN "OK":U.

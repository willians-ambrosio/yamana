/*******************************************************
** Autor...: Gustavo Eduardo Tamanini
** Programa: bofr007-UPC
** UPC cadastrada para programa: bofr007
*******************************************************/
{include/i-prgvrs.i BOFR007-UPC 2.00.00.000}
{include/i-epc200.i}

DEF TEMP-TABLE tt-mab-eqpto NO-UNDO LIKE mab-eqpto
    FIELD r-Rowid AS ROWID.

def input param p-ind-event  as char no-undo.
def input-output param table for tt-epc.

def var h-bo as handle no-undo.

/** Variaveis do programa AB0138A-UPC **/
DEF NEW GLOBAL SHARED VAR rs-modelo-0138 AS WIDGET-HANDLE NO-UNDO.

FIND FIRST tt-epc 
     WHERE tt-epc.cod-event     = p-ind-event
       AND tt-epc.cod-parameter = "OBJECT-HANDLE" NO-LOCK NO-ERROR.
IF AVAIL tt-epc THEN
    ASSIGN h-bo = WIDGET-HANDLE(tt-epc.val-parameter).

/** FO 1716.308 **/
IF (p-ind-event = "afterCreateRecord":U  OR
    p-ind-event = "afterUpdateRecord":U) THEN DO:
    /** Retorna temp-table da BO **/
    RUN getRecord IN h-bo (OUTPUT TABLE tt-mab-eqpto).

    IF VALID-HANDLE(rs-modelo-0138) THEN DO:
        FOR FIRST tt-mab-eqpto NO-LOCK:
            IF NOT CAN-FIND(FIRST mmv-equipto-esp
                            WHERE mmv-equipto-esp.ep-codigo   = tt-mab-eqpto.ep-codigo
                            AND   mmv-equipto-esp.cod-equipto = tt-mab-eqpto.cod-eqpto NO-LOCK) THEN DO:
                CREATE mmv-equipto-esp.
                ASSIGN mmv-equipto-esp.ep-codigo   = tt-mab-eqpto.ep-codigo
                       mmv-equipto-esp.cod-equipto = tt-mab-eqpto.cod-eqpto
                       mmv-equipto-esp.critic      = INT(rs-modelo-0138:SCREEN-VALUE).
            END.
            ELSE
                FOR FIRST mmv-equipto-esp
                    WHERE mmv-equipto-esp.ep-codigo   = tt-mab-eqpto.ep-codigo
                      AND mmv-equipto-esp.cod-equipto = tt-mab-eqpto.cod-eqpto EXCLUSIVE-LOCK:
                    ASSIGN mmv-equipto-esp.critic     = INT(rs-modelo-0138:SCREEN-VALUE).
                END.
        END.
    END.
END.

/** FO 1716.308 - Deleta tabela especifica. **/
IF p-ind-event = "afterDeleteRecord":U THEN DO:
    RUN getRecord IN h-bo (OUTPUT TABLE tt-mab-eqpto).
    FOR FIRST tt-mab-eqpto NO-LOCK,
        FIRST mmv-equipto-esp
        WHERE mmv-equipto-esp.ep-codigo   = tt-mab-eqpto.ep-codigo
          AND mmv-equipto-esp.cod-equipto = tt-mab-eqpto.cod-eqpto EXCLUSIVE-LOCK:
        DELETE mmv-equipto-esp.
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

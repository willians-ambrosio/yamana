/*******************************************************
** Autor...: Gustavo Eduardo Tamanini
** Data....: 03-10-2008
** Programa: BOMN152-UPC
** UPC cadastrada para programa: BOMN152
*******************************************************/
{include/i-prgvrs.i BOMN152-UPC 2.00.00.000}
{include/i-epc200.i}

DEF TEMP-TABLE tt-tag NO-UNDO LIKE tag
    FIELD RowNum AS INTEGER INIT 1
    FIELD r-Rowid AS ROWID
    INDEX i-Seq IS PRIMARY RowNum.

DEF INPUT PARAM p-ind-event AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

DEF VAR h-bo AS HANDLE NO-UNDO.

/** Variaveis do programa BOMN088-UPC **/
DEF NEW GLOBAL SHARED VAR wh-fi-hr-op-0581 AS WIDGET-HANDLE NO-UNDO.

FIND FIRST tt-epc 
     WHERE tt-epc.cod-event     = p-ind-event
       AND tt-epc.cod-parameter = "OBJECT-HANDLE" NO-LOCK NO-ERROR.
IF AVAIL tt-epc THEN
    ASSIGN h-bo = WIDGET-HANDLE(tt-epc.val-parameter).

/** FO 1708.134 - Criar tabela especifica para guardar
    as Horas de Opera‡Æo do Eqpto. **/
IF (p-ind-event = "afterCreateRecord":U  OR
    p-ind-event = "afterUpdateRecord":U) THEN DO:
    /** Retorna temp-table da BO **/
    RUN getRecord IN h-bo (OUTPUT TABLE tt-tag).

    IF  VALID-HANDLE(wh-fi-hr-op-0581) THEN DO:        
        FOR FIRST tt-tag NO-LOCK:
            /** tipo = 2 -> TAG **/
            IF NOT CAN-FIND(FIRST mmi-horas-oper
                            WHERE mmi-horas-oper.cd-tag = tt-tag.cd-tag 
                            AND   mmi-horas-oper.tipo   = 2 NO-LOCK) THEN DO:
                CREATE mmi-horas-oper.
                ASSIGN mmi-horas-oper.cd-tag         = tt-tag.cd-tag
                       mmi-horas-oper.horas-oper-tag = DECIMAL(wh-fi-hr-op-0581:SCREEN-VALUE)
                       mmi-horas-oper.tipo           = 2.
            END.
            ELSE
                FOR FIRST mmi-horas-oper
                    WHERE mmi-horas-oper.cd-tag     = tt-tag.cd-tag 
                      AND mmi-horas-oper.tipo       = 2 EXCLUSIVE-LOCK:
                    ASSIGN mmi-horas-oper.horas-oper-tag = DECIMAL(wh-fi-hr-op-0581:SCREEN-VALUE).
                END.
        END.
    END.
END.

/** FO 1708.134 - Deleta tabela especifica de Horas de Operacao do Eqpto. **/
IF p-ind-event = "afterDeleteRecord":U THEN DO:
    RUN getRecord IN h-bo (OUTPUT TABLE tt-tag).
    FOR FIRST tt-tag NO-LOCK:
        FOR FIRST  mmi-horas-oper
            WHERE  mmi-horas-oper.cd-tag = tt-tag.cd-tag 
            AND    mmi-horas-oper.tipo   = 2 EXCLUSIVE-LOCK:
            DELETE mmi-horas-oper.
        END.
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

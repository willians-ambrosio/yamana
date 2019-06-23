/*******************************************************
** Autor...: Gustavo Eduardo Tamanini
** Data....: 03-10-2008
** Programa: BOMN088-UPC
** UPC cadastrada para programa: BOMN088
*******************************************************/
{include/i-prgvrs.i BOMN088-UPC 2.00.00.000}
{include/i-epc200.i}

DEFINE TEMP-TABLE tt-equipto NO-UNDO LIKE equipto
    FIELD rowNum  AS INTEGER
    FIELD r-Rowid AS ROWID
    INDEX i-seq AS PRIMARY rowNum.

DEF INPUT PARAM p-ind-event AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

DEFINE VARIABLE h-bo         AS HANDLE  NO-UNDO.
/** Variaveis do programa BOMN088-UPC **/
DEFINE NEW GLOBAL SHARED VAR wh-fi-hr-op-0910 AS WIDGET-HANDLE NO-UNDO.

FIND FIRST tt-epc 
     WHERE tt-epc.cod-event     = p-ind-event
     AND   tt-epc.cod-parameter = "OBJECT-HANDLE" NO-LOCK NO-ERROR.

IF AVAIL tt-epc THEN DO:
    ASSIGN h-bo = WIDGET-HANDLE(tt-epc.val-parameter).
END.

/** FO 1708.134 - Criar tabela especifica para guardar
    as Horas de Opera‡Æo do Eqpto. **/
IF (p-ind-event = "afterCreateRecord":U  OR
    p-ind-event = "afterUpdateRecord":U) THEN DO:

    /** Retorna temp-table da BO **/
    RUN getRecord IN h-bo (OUTPUT TABLE tt-equipto).

    IF  VALID-HANDLE(wh-fi-hr-op-0910) THEN DO:        
        FOR FIRST tt-equipto NO-LOCK:
            /** Tipo = 1 -> Eqpto **/
            IF NOT CAN-FIND(FIRST mmi-horas-oper
                            WHERE mmi-horas-oper.cd-equipto = tt-equipto.cd-equipto
                            AND   mmi-horas-oper.cd-tag     = tt-equipto.cd-tag 
                            AND   mmi-horas-oper.tipo       = 1 NO-LOCK) THEN DO:
                CREATE mmi-horas-oper.
                ASSIGN mmi-horas-oper.cd-equipto       = tt-equipto.cd-equipto
                       mmi-horas-oper.cd-tag           = tt-equipto.cd-tag
                       mmi-horas-oper.tipo             = 1
                       mmi-horas-oper.horas-oper-eqpto = DECIMAL(wh-fi-hr-op-0910:SCREEN-VALUE)
                       mmi-horas-oper.horas-oper-tag   = 0. /** NÆo utiliza quando Eqpto **/
            END.
            ELSE DO:
                FOR FIRST mmi-horas-oper
                    WHERE mmi-horas-oper.cd-equipto = tt-equipto.cd-equipto
                    AND   mmi-horas-oper.cd-tag     = tt-equipto.cd-tag 
                    AND   mmi-horas-oper.tipo       = 1 EXCLUSIVE-LOCK:
                    ASSIGN mmi-horas-oper.horas-oper-eqpto = DECIMAL(wh-fi-hr-op-0910:SCREEN-VALUE).
                END.
            END.
        END.
    END.
END.

/** FO 1708.134 - Deleta tabela especifica de Horas de Operacao do Eqpto. **/
IF p-ind-event = "afterDeleteRecord":U THEN DO:
    RUN getRecord IN h-bo (OUTPUT TABLE tt-equipto).
    FOR FIRST tt-equipto NO-LOCK:
        FOR FIRST mmi-horas-oper
            WHERE mmi-horas-oper.cd-equipto = tt-equipto.cd-equipto
            AND   mmi-horas-oper.cd-tag     = tt-equipto.cd-tag 
            AND   mmi-horas-oper.tipo       = 1 EXCLUSIVE-LOCK:
            DELETE mmi-horas-oper.
        END.
    END.
END.

PROCEDURE pi-cria-erro:
DEFINE INPUT PARAMETER msg AS CHAR NO-UNDO.
    CREATE tt-epc.
    ASSIGN tt-epc.cod-event     = "ERROR"
           tt-epc.cod-parameter = "EPC-ERROR" 
           tt-epc.val-parameter = msg.
END.

RETURN "OK":U.

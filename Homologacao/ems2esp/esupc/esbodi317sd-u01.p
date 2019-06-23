/*******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i BODI317SD-UPC 2.000.00.00}

{cdp/cdcfgdis.i}
{include/i-epc200.i1}

def input        param p-ind-event  as char no-undo.
def input-output param table for tt-epc.

DEF VAR h-BO               AS HANDLE NO-UNDO.
DEF VAR h-boesp            AS HANDLE NO-UNDO.
DEF VAR c-natur-oper-nota  AS CHARACTER NO-UNDO.

IF  p-ind-event = "BuscaNatOper" THEN 
DO:
    FIND FIRST tt-epc
        WHERE tt-epc.cod-event     = p-ind-event
        AND   tt-epc.cod-parameter = "ch-leaveNomeAbrev" NO-LOCK NO-ERROR.
    IF AVAIL tt-epc THEN DO:

        RUN esbo/boes001.p PERSISTENT SET h-boesp.

        RUN getNaturezaNotaFiscal IN h-boesp (INPUT ENTRY(1,tt-epc.val-parameter,";"),
                                              INPUT ENTRY(2,tt-epc.val-parameter,";"), 
                                              OUTPUT c-natur-oper-nota).
        IF c-natur-oper-nota <> "" THEN DO:
            CREATE tt-epc.
            ASSIGN tt-epc.cod-event     = p-ind-event
                   tt-epc.cod-parameter = "nat-oper"
                   tt-epc.val-parameter = c-natur-oper-nota.
        END.

        DELETE OBJECT h-boesp.
        ASSIGN h-boesp = ?.
    END.    
END.



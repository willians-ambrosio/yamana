/*******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i BODI159COM-UPC-SUG-NATUR 2.00.00.004 } /*** 010004 ***/

{include/i-epc200.i} /*Defini‡Æo tt-EPC*/

DEF INPUT PARAM p-ind-event AS CHAR   NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

DEF BUFFER bf-ped-venda FOR ped-venda.
DEF BUFFER bf-ped-item  FOR ped-item.

DEFINE VARIABLE l-troca-natur AS LOGICAL     NO-UNDO.
DEFINE VARIABLE c-natur-item  AS CHARACTER   NO-UNDO.

IF  p-ind-event = "beforeCompleteOrder2" THEN DO:

    FOR FIRST tt-epc
        WHERE tt-epc.cod-event = p-ind-event
        AND   tt-epc.cod-parameter = "TABLE-ROWID" NO-LOCK:

        ASSIGN l-troca-natur = YES.

        FOR FIRST bf-ped-venda NO-LOCK
            WHERE ROWID(bf-ped-venda) = TO-ROWID(tt-epc.val-parameter),
            EACH bf-ped-item NO-LOCK
            WHERE bf-ped-item.nome-abrev = bf-ped-venda.nome-abrev
            AND   bf-ped-item.nr-pedcli  = bf-ped-venda.nr-pedcli:

            IF c-natur-item = "" THEN
                ASSIGN c-natur-item = bf-ped-item.nat-operacao.

            IF bf-ped-venda.nat-operacao = bf-ped-item.nat-operacao THEN
                ASSIGN l-troca-natur = NO.
        END.

        IF l-troca-natur THEN
            FOR FIRST bf-ped-venda EXCLUSIVE-LOCK
                WHERE ROWID(bf-ped-venda) = TO-ROWID(tt-epc.val-parameter):
                ASSIGN bf-ped-venda.nat-operacao = c-natur-item.
            END.
    END.    
END.



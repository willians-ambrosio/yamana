/**********************************************************************************
**
**    Programa    : upc-re1904-u00
**
**    Objetivo    : Programa Chamador de UPC
**
**    Autor       : Renato Oliveira
**
**    Data        : Agosto / 2018
**
**    Versao      : 2.12.00.000 - Desenvolvimento Inicial
**
***********************************************************************************/
{include/i-prgvrs.i upc-re1904-u00 2.12.00.000}

/* Variaveis de Parametros */
DEFINE INPUT PARAMETER p-ind-event  AS CHAR          NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHAR          NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHAR          NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

RUN upc/upc-re1904-u01.p (INPUT p-ind-event ,
                          INPUT p-ind-object,
                          INPUT p-wgh-object,
                          INPUT p-wgh-frame ,
                          INPUT p-cod-table ,
                          INPUT p-row-table ).

IF RETURN-VALUE = "NOK":U THEN
    RETURN "NOK":U.

/* Fim do programa */

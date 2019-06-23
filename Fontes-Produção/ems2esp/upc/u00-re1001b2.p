
/*-----------------------------------------------------------------------------------
    PROGRAMA : 
    OBJETIVO : 
    AUTOR    : Rog‚rio Dias (DSC)
    DATA     : 
-----------------------------------------------------------------------------------*/


/* -----------------[      Defini‡Æo de Parƒmetros    ]----------------------*/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.  
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.


/* Parametriza‡Æo de Seguran‡a para RE1001b2 - PIS/ COFINS */
RUN upc\u01-re1001b2.p (INPUT p-ind-event ,
                       INPUT p-ind-object,
                       INPUT p-wgh-object,
                       INPUT p-wgh-frame ,
                       INPUT p-cod-table ,
                       INPUT p-row-table).

/* Desabilita‡Æo da Conta Cont bil para Pedidos/Ordens de compra de Contratos*/

IF SEARCH("upc\u02-re1001b2.p") <> ?
OR SEARCH("upc\u02-re1001b2.r") <> ? THEN DO:
    RUN upc\u02-re1001b2.p (INPUT p-ind-event ,
                           INPUT p-ind-object,
                           INPUT p-wgh-object,
                           INPUT p-wgh-frame ,
                           INPUT p-cod-table ,
                           INPUT p-row-table).

END.

/* Informar CFOP e Modelo CTe/NFe da Nota fiscal recebida */
IF SEARCH("upc/u03-re1001b2.p") <> ?
OR SEARCH("upc/u03-re1001b2.r") <> ? THEN DO:
    RUN upc/u03-re1001b2.p (INPUT p-ind-event ,
                           INPUT p-ind-object,
                           INPUT p-wgh-object,
                           INPUT p-wgh-frame ,
                           INPUT p-cod-table ,
                           INPUT p-row-table).
    IF LOOKUP(RETURN-VALUE,",OK") = 0 THEN DO:
        RETURN ERROR.
    END.        

END.


/* Bloquear Campos Rateio */
IF SEARCH("upc/u04-re1001b2.p") <> ?
OR SEARCH("upc/u04-re1001b2.r") <> ? THEN DO:
    RUN upc/u04-re1001b2.p (INPUT p-ind-event ,
                            INPUT p-ind-object,
                            INPUT p-wgh-object,
                            INPUT p-wgh-frame ,
                            INPUT p-cod-table ,
                            INPUT p-row-table).
    IF LOOKUP(RETURN-VALUE,",OK") = 0 THEN DO:
        RETURN ERROR.
    END.        

END.

RETURN 'OK'.

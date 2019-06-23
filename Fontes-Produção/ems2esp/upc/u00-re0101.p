

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


/* Parametriza‡Æo de Seguran‡a para RE0101 - PIS/ COFINS */
RUN upc/u01-re0101.p (INPUT p-ind-event ,
                      INPUT p-ind-object,
                      INPUT p-wgh-object,
                      INPUT p-wgh-frame ,
                      INPUT p-cod-table ,
                      INPUT p-row-table).
IF LOOKUP(RETURN-VALUE,",OK") = 0 THEN
    RETURN RETURN-VALUE.

/* 10/07/2015 - Cleilton - DSC - 
   Parametriza‡Æo de EXCE€ÇO RECEBIMENTO NA NATUREZA DE OPERA€ÇO */
RUN upc/u02-re0101.p (INPUT p-ind-event ,
                      INPUT p-ind-object,
                      INPUT p-wgh-object,
                      INPUT p-wgh-frame ,
                      INPUT p-cod-table ,
                      INPUT p-row-table).
RETURN RETURN-VALUE.

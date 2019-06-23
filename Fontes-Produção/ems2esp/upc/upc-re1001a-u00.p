/**============================================================**
** Altera‡Æo...: Chamada UPCs inclusÆo de documento RE1001A
** Empresa.....: Cleilton / DSC
** Data........: 02/02/2015
** Objetivo....:  
** ............:  
**=============================================================**/
{include/i-prgvrs.i UPC-RE1001A-U00 2.06.00.000}

/** Parƒmetros **/                                    
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.


/** Global **/


RUN upc/upc-re1001a-u01.p(INPUT p-ind-event,
                          INPUT p-ind-object,
                          INPUT p-wgh-object,
                          INPUT p-wgh-frame,
                          INPUT p-cod-table,
                          INPUT p-row-table). 
IF RETURN-VALUE = "NOK" THEN
    RETURN RETURN-VALUE.

RUN dsc/ra/upc/esnfere1001a-u00.p (INPUT p-ind-event,
                                   INPUT p-ind-object,
                                   INPUT p-wgh-object,
                                   INPUT p-wgh-frame,
                                   INPUT p-cod-table,
                                   INPUT p-row-table). 
IF RETURN-VALUE = "NOK" THEN
    RETURN RETURN-VALUE.

RETURN "OK".

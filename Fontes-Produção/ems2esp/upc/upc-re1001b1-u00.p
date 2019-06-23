/**============================================================**
** Altera‡Æo...: Chamada UPCs inclusÆo de documento RE1001B1
** Empresa.....: Cleilton / DSC
** Data........: 06/02/2015
** Objetivo....:  
** ............:  
**=============================================================**/
{include/i-prgvrs.i UPC-RE1001B1-U00 11.5.11.000}

/** Parƒmetros **/                                    
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.


/** Global **/


RUN upc/upc-re1001b1-u01.p(INPUT p-ind-event,
                           INPUT p-ind-object,
                           INPUT p-wgh-object,
                           INPUT p-wgh-frame,
                           INPUT p-cod-table,
                           INPUT p-row-table). 
RETURN RETURN-VALUE.

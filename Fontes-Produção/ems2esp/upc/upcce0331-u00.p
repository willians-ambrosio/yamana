/* =========================================================================== 
   PROGRAMA      : UPCCE0331-U00.P
   DATA          : JUNHO/2010
   DESENVOLVEDOR : Augusto Guimar∆es (Kraft Consulting)
   VERSAO        : 1.00
   OBJETIVO      :  
   =========================================================================== */
{include/i-prgvrs.i upcce0331-u00 2.06.00.000} 

/* ==========================================================================
                PARAMENTROS DE ENTRADA DOS OBJETOS,EVENTOS E TABELAS
   ==========================================================================*/

DEFINE INPUT PARAMETER p-ind-event     AS CHAR           NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object    AS CHAR           NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object    AS HANDLE         NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame     AS WIDGET-HANDLE  NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table     AS CHAR           NO-UNDO.
DEFINE INPUT PARAMETER p-row-table     AS ROWID          NO-UNDO.
 
/*
MESSAGE "Event  = " p-ind-event SKIP
        "Object = " p-ind-object
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/

IF  p-ind-event  = "BEFORE-INITIALIZE" OR 
    p-ind-event  = "AFTER-DISPLAY"      AND
    p-ind-object = "CONTAINER"  THEN
DO:
/*     RUN upc/upcce0331-u01.p  (INPUT p-ind-event,  */
/*                               INPUT p-ind-object, */
/*                               INPUT p-wgh-object, */
/*                               INPUT p-wgh-frame,  */
/*                               INPUT p-cod-table,  */
/*                               INPUT p-row-table). */

    RUN upc/upcce0331-u02.p  (INPUT p-ind-event,
                              INPUT p-ind-object,
                              INPUT p-wgh-object,
                              INPUT p-wgh-frame,
                              INPUT p-cod-table,
                              INPUT p-row-table).

    IF RETURN-VALUE = "NOK" THEN 
        RETURN "NOK":U.
END.


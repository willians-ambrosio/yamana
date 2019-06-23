/*************************************************************************/
/*     Empresa.......: V‚rtice Tecnologia                                */
/*     Autor.........: Roger Labadessa.                                  */
/*     Data..........: 12/04/2010                                        */
/*     Objetivo......:                                                   */ 
/*************************************************************************/

{include/i-prgvrs.i esap5510-u00 2.06.00.001}

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

RUN upc/esap5510-u01.p (p-ind-event,
                        p-ind-object,
                        p-wgh-object,
                        p-wgh-frame,
                        p-cod-table,
                        p-row-table).

IF RETURN-VALUE = "NOK":u THEN
  RETURN "NOK".

RETURN "OK".

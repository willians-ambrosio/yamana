/***********************************************************************
**
**  PROGRAMA - up-fp3000
**
**  OBJETIVO - Validar saldo de estoque antes de confirmar requisicao
**
**  AUTOR    - Gilberto Rissati Garcia (Datasul)
**
**  DATA     - 09/10/2003
**
*************************************************************************/

DEFINE INPUT  PARAMETER p-ind-event      AS CHARACTER       NO-UNDO.
DEFINE INPUT  PARAMETER p-ind-object     AS CHARACTER       NO-UNDO.
DEFINE INPUT  PARAMETER p-wgh-object     AS HANDLE          NO-UNDO.
DEFINE INPUT  PARAMETER p-wgh-frame      AS WIDGET-HANDLE   NO-UNDO.
DEFINE INPUT  PARAMETER p-cod-table      AS CHARACTER       NO-UNDO.
DEFINE INPUT  PARAMETER p-row-table      AS ROWID           NO-UNDO.

DEFINE VARIABLE h_Frame                  AS WIDGET-HANDLE   NO-UNDO.
DEFINE VARIABLE c-objeto                 AS CHARACTER       NO-UNDO.
DEFINE VARIABLE wh_campo                 AS WIDGET-HANDLE   NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE b02py040_rowid            AS ROWID NO-UNDO.

IF VALID-HANDLE(p-wgh-object) THEN
   ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/*
IF c-objeto     = "b02py040.w" THEN
   MESSAGE "Evento    " p-ind-event   SKIP
           "Objeto    " p-ind-object  SKIP
           "nome obj  " p-wgh-object  SKIP
           "Objeto    " c-objeto      SKIP
           "Frame     " p-wgh-frame   SKIP
           "Tabela    " p-cod-table   SKIP
           "ROWID     " string(p-row-table)
           view-as alert-box information.
*/

IF p-ind-event  = "INITIALIZE" AND
   p-ind-object = "BROWSER"    AND  
   c-objeto     = "b02py040.w" THEN DO:
 
   ASSIGN h_frame = p-wgh-frame:FIRST-CHILD
         h_frame = h_frame:FIRST-CHILD.

   DO WHILE h_frame <> ?:
      IF h_frame:NAME = "bt_alt_msg" THEN
         ON 'CHOOSE':U OF h_frame PERSISTENT
         RUN upc\esfp3000.p (INPUT h_frame,INPUT p-wgh-frame).
 
      ASSIGN h_frame = h_frame:NEXT-SIBLING.
   END.
END.

IF (p-ind-event  = "AFTER-VALUE-CHANGED" OR 
    p-ind-event  = "AFTER-OPEN-QUERY")   AND
    p-ind-object = "BROWSER"             AND  
    c-objeto     = "b02py040.w"          THEN DO:

   ASSIGN b02py040_rowid = p-row-table.
   
END.

/***********************************************************************
**
**  PROGRAMA - up-cd1409
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

{include/i-prgvrs.i up-cd1409 2.06.00.000}

DEFINE VARIABLE h_Frame                  AS WIDGET-HANDLE   NO-UNDO.
DEFINE VARIABLE h_Browse                 AS WIDGET-HANDLE   NO-UNDO.
DEFINE VARIABLE h_browser                AS WIDGET-HANDLE   NO-UNDO.
DEFINE VARIABLE c-objeto                 AS CHARACTER       NO-UNDO.

IF VALID-HANDLE(p-wgh-object) THEN
   ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

DEFINE NEW GLOBAL SHARED VARIABLE up-cd1409_de-saldo-req    AS WIDGET-HANDLE NO-UNDO.

 
/*
IF c-objeto     = "b06in385.w" THEN
MESSAGE "Evento    " p-ind-event   SKIP
        "Objeto    " p-ind-object  SKIP
        "nome obj  " p-wgh-object  SKIP
        "Objeto    " c-objeto      SKIP
        "Frame     " p-wgh-frame   SKIP
        "Tabela    " p-cod-table   SKIP
        "ROWID     " string(p-row-table)
        view-as alert-box information.
*/

IF p-ind-event  = "BEFORE-OPEN-QUERY" AND 
   p-ind-object = "BROWSER"            AND 
   c-objeto     = "b06in385.w"         THEN DO:

   
   ASSIGN h_frame = p-wgh-frame:FIRST-CHILD
          h_frame = h_frame:FIRST-CHILD.

   DO WHILE h_frame <> ?:

      IF h_frame:NAME = "de-saldo-req" THEN DO:
         ASSIGN up-cd1409_de-saldo-req = h_frame.
         LEAVE.
      END.
      ASSIGN h_frame = h_frame:NEXT-SIBLING.
   END.   
END.


IF p-ind-event  = "INITIALIZE" AND
   p-ind-object = "BROWSER"    AND 
   c-objeto     = "b06in385.w" THEN DO:

   ASSIGN h_frame = p-wgh-frame:FIRST-CHILD 
          h_frame = h_frame:FIRST-CHILD.
  
   DO WHILE h_frame <> ?:
      IF h_frame:TYPE = "BROWSE" THEN DO:
         ASSIGN h_Browse = h_frame.
         LEAVE.
      END.
      ASSIGN h_frame = h_frame:NEXT-SIBLING.
   END.

   IF VALID-HANDLE(h_Browse) AND h_Browse:TYPE = "BROWSE" THEN DO:
      ASSIGN h_Browse = h_Browse:FIRST-COLUMN.
      DO WHILE h_Browse <> ?:
         IF h_Browse:NAME = "qtidade-req" THEN DO:
            ON 'MOUSE-SELECT-CLICK':U OF h_Browse PERSISTENT 
            RUN upc\escd1409.p (INPUT h_Browse,INPUT p-wgh-frame).
            LEAVE.
         END.
         ASSIGN h_Browse = h_Browse:NEXT-COLUMN.
      END.
   END.
END.

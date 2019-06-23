/***********************************************************************
**
**  PROGRAMA - up-cd1406a1
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

IF VALID-HANDLE(p-wgh-object) THEN
   ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").


DEFINE NEW GLOBAL SHARED VARIABLE up-cd1406a1_it-codigo      LIKE ITEM.it-codigo NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE up-cd1406a1_p-wgh-frame    AS WIDGET-HANDLE    NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE gr-requisicao              AS ROWID NO-UNDO.

DEFINE BUFFER b_requisicao FOR requisicao.

/*
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
   p-ind-object = "CONTAINER"  THEN DO:


   FIND FIRST b_requisicao NO-LOCK
        WHERE ROWID(b_requisicao) = gr-requisicao 
        NO-ERROR.
   IF AVAIL b_requisicao AND
      b_requisicao.tp-requis = 1 THEN DO:

      ASSIGN up-cd1406a1_p-wgh-frame = p-wgh-frame
             h_frame = p-wgh-frame:FIRST-CHILD
             h_frame = h_frame:FIRST-CHILD.

      DO WHILE h_frame <> ?:

         IF h_frame:NAME = "bt-ok" THEN DO:
            ON 'RETURN':U,'MOUSE-SELECT-CLICK':U OF h_frame PERSISTENT 
            RUN upc\escd1406A1.p (INPUT h_frame,INPUT p-wgh-frame).
         END.
         IF h_frame:NAME = "bt-save" THEN DO:
            ON 'RETURN':U,'MOUSE-SELECT-CLICK':U OF h_frame PERSISTENT 
            RUN upc\escd1406A1.p (INPUT h_frame,INPUT p-wgh-frame).
         END.
         ASSIGN h_frame = h_frame:NEXT-SIBLING.
      END.
   END.
END.


IF p-ind-event  = "INITIALIZE" AND
   p-ind-object = "VIEWER"     AND 
   c-objeto     = "v10in385.w" THEN DO:

   ASSIGN h_frame = p-wgh-frame:FIRST-CHILD
          h_frame = h_frame:FIRST-CHILD.

   DO WHILE h_frame <> ?:
      IF h_frame:NAME = "qt-a-atender" THEN DO:
         ON 'LEAVE':U OF h_frame PERSISTENT 
         RUN upc\escd1406A1.p (INPUT h_frame,INPUT p-wgh-frame).    
      END.
      IF h_frame:NAME = "it-codigo" THEN DO:
         ON 'VALUE-CHANGED':U OF h_frame PERSISTENT 
         RUN upc\escd1406A1.p (INPUT h_frame,INPUT p-wgh-frame).    
      END.
      ASSIGN h_frame = h_frame:NEXT-SIBLING.
   END.
END.

/*IF p-ind-event  = "DISPLAY"    AND
   p-ind-object = "VIEWER"     AND  */

IF p-ind-event  = "leave-it-codigo"    AND
   p-ind-object = "it-codigo"     AND
   c-objeto     = "v10in385.w" THEN DO:

   ASSIGN h_frame = p-wgh-frame:FIRST-CHILD
          h_frame = h_frame:FIRST-CHILD.
  
   DO WHILE h_frame <> ?:
      IF h_frame:NAME = "it-codigo" THEN DO:
         ASSIGN up-cd1406a1_it-codigo = h_frame:SCREEN-VALUE.
         LEAVE.
      END.
      ASSIGN h_frame = h_frame:NEXT-SIBLING.
   END.
END.

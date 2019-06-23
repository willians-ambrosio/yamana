/***********************************************************************
**
**  PROGRAMA - up-cn0201A
**
**  OBJETIVO - Fixar parametro de impressao de contrato
**
**  AUTOR    - Gilberto Rissati Garcia (Datasul)
**
**  DATA     - 13/10/2003
**
*************************************************************************/

DEFINE INPUT  PARAMETER p-ind-event      AS CHARACTER       NO-UNDO.
DEFINE INPUT  PARAMETER p-ind-object     AS CHARACTER       NO-UNDO.
DEFINE INPUT  PARAMETER p-wgh-object     AS HANDLE          NO-UNDO.
DEFINE INPUT  PARAMETER p-wgh-frame      AS WIDGET-HANDLE   NO-UNDO.
DEFINE INPUT  PARAMETER p-cod-table      AS CHARACTER       NO-UNDO.
DEFINE INPUT  PARAMETER p-row-table      AS ROWID           NO-UNDO.

{include/i-prgvrs.i up-cn0201A 2.00.00.000}

DEFINE VARIABLE h_Frame                  AS WIDGET-HANDLE   NO-UNDO.
DEFINE VARIABLE c-objeto                 AS CHARACTER       NO-UNDO.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/*
IF c-objeto     = "v13in065.w" THEN
MESSAGE "Evento    " p-ind-event   SKIP
        "Objeto    " p-ind-object  SKIP
        "nome obj  " p-wgh-object  SKIP
        "Objeto    " c-objeto      SKIP
        "Frame     " p-wgh-frame   SKIP
        "Tabela    " p-cod-table   SKIP
        "ROWID     " string(p-row-table)
        view-as alert-box information.
*/

IF p-ind-event  = "AFTER-ENABLE" AND
   p-ind-object = "VIEWER"     AND 
   c-objeto     = "v13in065.w" THEN DO:

   ASSIGN h_frame = p-wgh-frame:FIRST-CHILD
          h_frame = h_frame:FIRST-CHILD.
  
   DO WHILE h_frame <> ?:
      IF h_frame:NAME = "impr-contrat" THEN DO:
         ASSIGN h_frame:SCREEN-VALUE = "Sim" 
                h_frame:SENSITIVE = NO.
         LEAVE.
      END.
      
      ASSIGN h_frame = h_frame:NEXT-SIBLING.
   END.
END.

/***********************************************************************
**
**  PROGRAMA - up-CC0300A
**
**  OBJETIVO - DEIXAR SEMPRE SELECIONADO A OPCAO DE IMPRIME PEDIDO.
**
**  AUTOR    - Gilberto Rissati Garcia (Datasul)
**
**  DATA     - 07/10/2003
**
*************************************************************************/

DEFINE INPUT  PARAMETER p-ind-event      AS CHARACTER       NO-UNDO.
DEFINE INPUT  PARAMETER p-ind-object     AS CHARACTER       NO-UNDO.
DEFINE INPUT  PARAMETER p-wgh-object     AS HANDLE          NO-UNDO.
DEFINE INPUT  PARAMETER p-wgh-frame      AS WIDGET-HANDLE   NO-UNDO.
DEFINE INPUT  PARAMETER p-cod-table      AS CHARACTER       NO-UNDO.
DEFINE INPUT  PARAMETER p-row-table      AS ROWID           NO-UNDO.
                                         
DEFINE VARIABLE c-objeto                 AS CHARACTER       NO-UNDO.
DEFINE VARIABLE h_Frame                  AS WIDGET-HANDLE   NO-UNDO.

{include/i-prgvrs.i up-cc0300a 2.00.00.000}

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

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

IF p-ind-event  = "AFTER-INITIALIZE" AND
   p-ind-object = "CONTAINER"        THEN DO:

   ASSIGN h_frame = p-wgh-frame:FIRST-CHILD
          h_frame = h_frame:FIRST-CHILD.
  
   DO WHILE h_frame <> ?:
      IF h_frame:NAME = "impr-pedido" THEN DO:
      
         ASSIGN h_frame:SCREEN-VALUE = "Sim" 
                h_frame:SENSITIVE = NO
                .
         LEAVE.
      END.
      ASSIGN h_frame = h_frame:NEXT-SIBLING.
   END.
END.

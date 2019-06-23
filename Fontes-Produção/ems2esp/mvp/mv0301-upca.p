DEFINE VARIABLE h-program    AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER wh-nr-ord-produ     AS WIDGET NO-UNDO. 
DEFINE INPUT PARAMETER wh-cd-tarefa-mv0301 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE wh-coluna AS WIDGET-HANDLE NO-UNDO.

DEFINE VARIABLE r-rowid                     AS ROWID      NO-UNDO.
DEF NEW GLOBAL SHARED VAR wgh-coluna-teste  AS WIDGET-HANDLE NO-UNDO.


ASSIGN wh-coluna = wh-cd-tarefa-mv0301:FIRST-COLUMN.

RUN mvp/esmv0301.w PERSISTENT SET h-program.

RUN initializeInterface IN h-program.

RUN setarPosicao IN h-program (INPUT INT(wh-nr-ord-produ:SCREEN-VALUE),
                               INPUT int(wh-coluna:SCREEN-VALUE)).

RETURN "OK":U.

DEFINE INPUT PARAMETER p-ind-event   AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object  AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object  AS HANDLE         NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame   AS WIDGET-HANDLE  NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table   AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-row-table   AS ROWID          NO-UNDO.

DEFINE VAR c-objeto      AS CHARACTER       NO-UNDO.
DEFINE VARIABLE wh-frame AS WIDGET-HANDLE   NO-UNDO. 

DEFINE NEW GLOBAL SHARED VARIABLE l-add                 AS LOGICAL       NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cod-eqpto-ab0138a  AS WIDGET-HANDLE NO-UNDO.

ASSIGN c-objeto   = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

FUNCTION getWidgetHandle RETURNS WIDGET-HANDLE (INPUT pWidget AS CHAR) FORWARD.


IF  p-ind-event  = "BEFORE-INITIALIZE"  AND
    p-ind-object = "CONTAINER"          THEN DO:
    ASSIGN l-add = NO.
END.


IF (p-ind-event  = "BEFORE-COPY"  OR
    p-ind-event  = "BEFORE-ADD" ) AND
    p-ind-object = "CONTAINER" THEN DO:
    ASSIGN l-add = YES.
END.

IF  p-ind-event  = "AFTER-ENABLE" AND
    p-ind-object = "CONTAINER"    THEN DO:
    ASSIGN wh-cod-eqpto-ab0138a    = getWidgetHandle("cod-eqpto").
END.



IF p-ind-event = "BOTAO-OK" THEN DO:
    IF LENGTH(wh-cod-eqpto-ab0138a:SCREEN-VALUE) > 14 THEN DO:

        RUN utp/ut-msgs.p (INPUT 'show',
                           INPUT 17006,
                           INPUT "Nome do Equipamento Incorreto." + "~~" + 
                                 "Somente ser  permitido cadastro no nome do equipamento com at‚ 14 caracteres.").

        RETURN "NOK":u.
    END.
END.


FUNCTION getWidgetHandle RETURNS WIDGET-HANDLE
    (INPUT pWidget  AS CHAR).
    DEFINE VARIABLE wh-WIDGET-HANDLE    AS WIDGET-HANDLE    NO-UNDO.
    ASSIGN wh-frame = p-wgh-frame:FIRST-CHILD
           wh-frame = wh-frame:FIRST-CHILD.
    DO WHILE wh-frame <> ?:
        IF wh-frame:NAME = pWidget THEN DO:
            ASSIGN wh-WIDGET-HANDLE = wh-frame:HANDLE.
            LEAVE.
        END.
        ASSIGN wh-frame = wh-frame:NEXT-SIBLING.
    END.
    RETURN wh-WIDGET-HANDLE.
END FUNCTION.

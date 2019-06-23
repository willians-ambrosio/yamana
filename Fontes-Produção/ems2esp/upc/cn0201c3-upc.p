DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.
             
DEFINE VARIABLE c-objeto AS CHARACTER        NO-UNDO. 
DEFINE VARIABLE hObject  AS HANDLE           NO-UNDO.
DEFINE VARIABLE wh-frame AS WIDGET-HANDLE    NO-UNDO. 

DEFINE NEW GLOBAL SHARED VARIABLE wh-i-ordem-invest-cn0201c3 AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-nr-contrato-cn0201c3    AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-num-seq-item-cn0201c3   AS WIDGET-HANDLE NO-UNDO.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

FUNCTION getWidgetHandle RETURNS WIDGET-HANDLE (INPUT pWidget AS CHAR) FORWARD.

DO:

    IF NOT VALID-HANDLE(wh-nr-contrato-cn0201c3)     AND
       NOT VALID-HANDLE(wh-i-ordem-invest-cn0201c3)  THEN
        ASSIGN wh-nr-contrato-cn0201c3    = getWidgetHandle("nr-contrato")
               wh-i-ordem-invest-cn0201c3 = getWidgetHandle("fi-i-num-ord-inv")
               wh-num-seq-item-cn0201c3   = getWidgetHandle("num-seq-item").

    IF VALID-HANDLE(wh-nr-contrato-cn0201c3)     AND
       VALID-HANDLE(wh-i-ordem-invest-cn0201c3)  AND 
       VALID-HANDLE(wh-num-seq-item-cn0201c3)    THEN  DO:

        ASSIGN wh-i-ordem-invest-cn0201c3:SENSITIVE    = NO.

        FIND FIRST contrato-for-ext 
             WHERE contrato-for-ext.nr-contrato  = INT(wh-nr-contrato-cn0201c3:SCREEN-VALUE) NO-LOCK NO-ERROR.

        FIND FIRST item-contrat-ext 
             WHERE item-contrat-ext.nr-contrato  = INT(wh-nr-contrato-cn0201c3:SCREEN-VALUE)
               AND item-contrat-ext.num-seq-item = INT(wh-num-seq-item-cn0201c3:SCREEN-VALUE)
               AND item-contrat-ext.num-ord-inv  <> 0 NO-LOCK NO-ERROR.

        IF AVAIL item-contrat-ext THEN
            ASSIGN wh-i-ordem-invest-cn0201c3:SCREEN-VALUE = STRING(item-contrat-ext.num-ord-inv) .
            
        IF NOT AVAIL item-contrat-ext
           AND AVAIL contrato-for-ext THEN 
            ASSIGN wh-i-ordem-invest-cn0201c3:SCREEN-VALUE = STRING(contrato-for-ext.num-ord-inv) .
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

RETURN "OK":U.

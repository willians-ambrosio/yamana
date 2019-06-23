DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.
             
DEFINE VARIABLE c-objeto AS CHARACTER        NO-UNDO. 
DEFINE VARIABLE hObject  AS HANDLE           NO-UNDO.
DEFINE VARIABLE wh-frame AS WIDGET-HANDLE    NO-UNDO. 

DEFINE NEW GLOBAL SHARED VARIABLE wh-i-ordem-invest-cn0302a AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-nr-contrato-cn0302a    AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-sequencia-cn0302a      AS WIDGET-HANDLE NO-UNDO.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

FUNCTION getWidgetHandle RETURNS WIDGET-HANDLE (INPUT pWidget AS CHAR) FORWARD.

IF NOT VALID-HANDLE(wh-nr-contrato-cn0302a)     AND
   NOT VALID-HANDLE(wh-i-ordem-invest-cn0302a)  AND 
   NOT VALID-HANDLE(wh-sequencia-cn0302a)       THEN
    ASSIGN wh-nr-contrato-cn0302a    = getWidgetHandle("i-nr-contrato")
           wh-i-ordem-invest-cn0302a = getWidgetHandle("i-num-ord-inv")
           wh-sequencia-cn0302a      = getWidgetHandle("i-num-seq-item").

IF p-ind-event = "leave-data-previsao" THEN DO:

    IF VALID-HANDLE(wh-nr-contrato-cn0302a)     AND
       VALID-HANDLE(wh-i-ordem-invest-cn0302a)  THEN  DO:

        ASSIGN wh-i-ordem-invest-cn0302a:SENSITIVE    = NO
               wh-i-ordem-invest-cn0302a:SCREEN-VALUE = "".

        FIND FIRST contrato-for-ext 
             WHERE contrato-for-ext.nr-contrato  = INT(wh-nr-contrato-cn0302a:SCREEN-VALUE) NO-LOCK NO-ERROR.

        FIND FIRST item-contrat-ext 
             WHERE item-contrat-ext.nr-contrato  = INT(wh-nr-contrato-cn0302a:SCREEN-VALUE)
               AND item-contrat-ext.num-ord-inv  = INT(wh-sequencia-cn0302a:SCREEN-VALUE)  NO-LOCK NO-ERROR.

        IF AVAIL item-contrat-ext THEN
            ASSIGN wh-i-ordem-invest-cn0302a:SCREEN-VALUE = STRING(item-contrat-ext.num-ord-inv) 
                   wh-i-ordem-invest-cn0302a:SENSITIVE    = NO.
            
        IF NOT AVAIL item-contrat-ext
           AND AVAIL contrato-for-ext THEN 
            ASSIGN wh-i-ordem-invest-cn0302a:SCREEN-VALUE = STRING(contrato-for-ext.num-ord-inv) 
                   wh-i-ordem-invest-cn0302a:SENSITIVE    = NO.

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

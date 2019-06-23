DEFINE INPUT PARAMETER p-ind-event   AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object  AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object  AS HANDLE         NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame   AS WIDGET-HANDLE  NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table   AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-row-table   AS ROWID          NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE wgh-conta-contabil-re1001b2        AS HANDLE          NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-fPage2-re1001b2                 AS HANDLE          NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-contrato-re1001b2                AS LOGICAL         NO-UNDO.

DEFINE VARIABLE wh-frame AS WIDGET-HANDLE NO-UNDO. 

FUNCTION getWidgetHandle RETURNS WIDGET-HANDLE (INPUT pWidget AS CHAR) FORWARD.
FUNCTION getWidgetHandleSub RETURNS WIDGET-HANDLE (INPUT pWidget AS CHAR, INPUT pp-wgh-frame AS HANDLE ) FORWARD.

IF p-ind-event = "AFTER-INITIALIZE" THEN DO:

    ASSIGN wh-fPage2-re1001b2               = getWidgetHandle("fPage2")
           wgh-conta-contabil-re1001b2      = getWidgetHandleSub("conta-contabil",wh-fPage2-re1001b2)
           l-contrato-re1001b2              = NO.

    FIND FIRST item-doc-est
         WHERE ROWID(item-doc-est) = p-row-table NO-LOCK NO-ERROR.

    IF AVAIL item-doc-est THEN DO:
        
        FIND FIRST ordem-compra
             WHERE ordem-compra.num-pedido    = item-doc-est.num-pedido
              AND  ordem-compra.numero-ordem  = item-doc-est.numero-ordem
              AND  ordem-compra.nr-contrato   <> 0  NO-LOCK NO-ERROR.

        IF NOT AVAIL ordem-compra THEN DO:

            blk_rat-ordem:
            FOR EACH rat-ordem 
                  OF item-doc-est NO-LOCK:

                FIND FIRST ordem-compra
                     WHERE ordem-compra.num-pedido    = rat-ordem.num-pedido
                      AND  ordem-compra.numero-ordem  = rat-ordem.numero-ordem
                      AND  ordem-compra.nr-contrato   <> 0  NO-LOCK NO-ERROR.

                IF AVAIL ordem-compra THEN
                    LEAVE blk_rat-ordem.

            END.
        END.
        
        IF AVAIL ordem-compra THEN DO:
    
            FIND FIRST contrato-for-ext
                 WHERE contrato-for-ext.nr-contrato  = ordem-compra.nr-contrato
                   AND contrato-for-ext.num-ord-inv <> 0 NO-LOCK NO-ERROR.
    
            IF NOT AVAIL contrato-for-ext THEN DO:
    
                FIND FIRST item-contrat-ext
                     WHERE item-contrat-ext.nr-contrato = ordem-compra.nr-contrato
                       AND item-contrat-ext.num-ord-inv <> 0 NO-LOCK NO-ERROR.
    
                IF AVAIL item-contrat-ext THEN
                    ASSIGN l-contrato-re1001b2 = YES.
    
            END.
            ELSE
                ASSIGN l-contrato-re1001b2 = YES.
        END.
        ELSE
            ASSIGN l-contrato-re1001b2 = NO.
    END.
END.

IF VALID-HANDLE(wgh-conta-contabil-re1001b2) AND l-contrato-re1001b2 THEN
    ASSIGN wgh-conta-contabil-re1001b2:SENSITIVE = NO.

FUNCTION getWidgetHandle RETURNS WIDGET-HANDLE
    (INPUT pWidget  AS CHAR).
    DEFINE VARIABLE wh-widget-handle    AS WIDGET-HANDLE    NO-UNDO.
    ASSIGN wh-frame = p-wgh-frame:FIRST-CHILD
           wh-frame = wh-frame:FIRST-CHILD.
    DO WHILE wh-frame <> ?:
        IF wh-frame:NAME = pWidget THEN DO:
            ASSIGN wh-widget-handle = wh-frame:HANDLE.
            LEAVE.
        END.
        ASSIGN wh-frame = wh-frame:NEXT-SIBLING.
    END.
    RETURN wh-widget-handle.
END FUNCTION.

FUNCTION getWidgetHandleSub RETURNS WIDGET-HANDLE
    (INPUT pWidget  AS CHAR,
     INPUT pp-wgh-frame AS HANDLE).
    DEFINE VARIABLE wh-widget-handle    AS WIDGET-HANDLE    NO-UNDO.
    ASSIGN wh-frame = pp-wgh-frame:FIRST-CHILD
           wh-frame = wh-frame:FIRST-CHILD.
    DO WHILE wh-frame <> ?:
        IF wh-frame:NAME = pWidget THEN DO:
            ASSIGN wh-widget-handle = wh-frame:HANDLE.
            LEAVE.
        END.
        ASSIGN wh-frame = wh-frame:NEXT-SIBLING.
    END.
    RETURN wh-widget-handle.
END FUNCTION.

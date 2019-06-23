/*******************************************************************************
** Programa: cn0201-upc
** Autor...: Log¡stica (log31237)
** Data....: 07/2008
** OBS.....: UPC utilizada pelo programa cn0201.w
** Objetivo: 
*******************************************************************************/

DEF INPUT PARAM p-ind-event  AS   CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object AS   CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object AS   HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame  AS   WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table  AS   CHAR          NO-UNDO.
DEF INPUT PARAM p-row-table  AS   ROWID         NO-UNDO.

DEF BUFFER bfcontrato-for FOR contrato-for.
DEF BUFFER bfitem-contrat FOR item-contrat.

DEFINE VARIABLE h-esapi001   AS HANDLE          NO-UNDO.
DEFINE VARIABLE wh-frame     AS WIDGET-HANDLE   NO-UNDO. 
DEFINE VARIABLE c-objeto     AS CHAR            NO-UNDO.

DEF NEW GLOBAL SHARED VAR h-cn0201-upc    AS HANDLE        NO-UNDO.
DEF NEW GLOBAL SHARED VAR wgh-bt-export   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wgh-bt-perc-it  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-contrato-for AS ROWID         NO-UNDO.

def  temp-table tt-erro no-undo
    field i-sequen as int             
    field cd-erro  as int
    field mensagem as char format "x(255)"
    field c-param  as char.

FUNCTION getWidgetHandle RETURNS WIDGET-HANDLE (INPUT pWidget AS CHAR) FORWARD.

IF  p-ind-event  = "INITIALIZE"
AND p-ind-object = "CONTAINER" THEN DO:
    
    RUN upc/cn0201-upc.p PERSISTENT SET h-cn0201-upc (INPUT "",            
                                                      INPUT "",            
                                                      INPUT p-wgh-object,  
                                                      INPUT p-wgh-frame,   
                                                      INPUT "",            
                                                      INPUT p-row-table).

    CREATE BUTTON wgh-bt-export
    ASSIGN NAME      = "bt-export"
           FRAME     = p-wgh-frame
           WIDTH     = 5.00
           HEIGHT    = 1.25
           LABEL     = ""
           ROW       = 1.13
           COL       = 51.43
           TOOLTIP   = "Exporta dados Excel"
           VISIBLE   = YES
           SENSITIVE = YES
           TRIGGERS:
            ON CHOOSE PERSISTENT RUN pi-bt-export IN h-cn0201-upc.
           END TRIGGERS.
           
    wgh-bt-export:LOAD-IMAGE-UP("image\toolbar\mip-csv.bmp").
    wgh-bt-export:MOVE-TO-TOP().

    CREATE BUTTON wgh-bt-perc-it
    ASSIGN NAME      = "bt-perc-it"
           FRAME     = p-wgh-frame
           WIDTH     = 5.00
           HEIGHT    = 1.25
           LABEL     = ""
           ROW       = 1.13
           COL       = 56.43
           TOOLTIP   = "Item x Fornec"
           VISIBLE   = YES
           SENSITIVE = YES
           TRIGGERS:
            ON CHOOSE PERSISTENT RUN pi-bt-perc-it IN h-cn0201-upc.
           END TRIGGERS.
           
    wgh-bt-perc-it:LOAD-IMAGE-UP("image\toolbar\im-npula.bmp").
    wgh-bt-perc-it:MOVE-TO-TOP().

END.

IF  p-ind-event  = "DESTROY"
AND p-ind-object = "CONTAINER" THEN DO:
    
    ASSIGN wgh-bt-export  = ?
           wgh-bt-perc-it = ?.

    DELETE PROCEDURE h-cn0201-upc.    

END.

IF  p-ind-event = "CANCELAR":U THEN DO:
    RUN esp\esapi001.p PERSISTENT SET h-esapi001.
    
    IF  p-cod-table = "CONTRATO-FOR":U THEN DO:
        FIND FIRST bfcontrato-for NO-LOCK
             WHERE ROWID(bfcontrato-for) = p-row-table NO-ERROR.
        
        FOR EACH  contrato-for-ext EXCLUSIVE-LOCK
            WHERE contrato-for-ext.nr-contrato = bfcontrato-for.nr-contrato:
            
            FOR EACH  controle-inv-esp EXCLUSIVE-LOCK
                WHERE controle-inv-esp.ep-codigo    = contrato-for-ext.ep-codigo
                  AND controle-inv-esp.num-ord-inv  = contrato-for-ext.num-ord-inv
                  AND controle-inv-esp.nr-contrato  = contrato-for-ext.nr-contrato:
                
                RUN pi-eliminar.
                
                IF  RETURN-VALUE = "NOK":U THEN
                    RETURN RETURN-VALUE.
            END.
            
            DELETE contrato-for-ext.
        END.
    END.
    
    IF  p-cod-table = "ITEM-CONTRAT":U THEN DO:
        FIND FIRST bfitem-contrat NO-LOCK
             WHERE ROWID(bfitem-contrat) = p-row-table NO-ERROR.
        
        FOR EACH  item-contrat-ext EXCLUSIVE-LOCK
            WHERE item-contrat-ext.nr-contrato       = bfitem-contrat.nr-contrato
              AND item-contrat-ext.num-seq-item      = bfitem-contrat.num-seq-item:
            FOR EACH  controle-inv-esp EXCLUSIVE-LOCK
                WHERE controle-inv-esp.ep-codigo    = item-contrat-ext.ep-codigo
                  AND controle-inv-esp.num-ord-inv  = item-contrat-ext.num-ord-inv
                  AND controle-inv-esp.nr-contrato  = item-contrat-ext.nr-contrato
                  AND controle-inv-esp.num-seq-item = item-contrat-ext.num-seq-item:
                
                RUN pi-eliminar.
                
                IF  RETURN-VALUE = "NOK":U THEN
                    RETURN RETURN-VALUE.
            END.
            
            DELETE item-contrat-ext.
        END.
    END.
    
    IF  VALID-HANDLE(h-esapi001) THEN DO:
        DELETE PROCEDURE h-esapi001.
        ASSIGN h-esapi001 = ?.
    END.
END.

RETURN "OK".


PROCEDURE pi-eliminar.
    
    IF  VALID-HANDLE(h-esapi001) THEN DO:
        FIND FIRST param-inv NO-LOCK
             WHERE param-inv.ep-codigo = controle-inv-esp.ep-codigo NO-ERROR.
        RUN pi-atualiza-verba IN h-esapi001 (INPUT 2,
                                             INPUT controle-inv-esp.ep-codigo,
                                             INPUT controle-inv-esp.num-ord-inv,
                                             INPUT controle-inv-esp.dt-trans,
                                             INPUT param-inv.moeda-inv,
                                             INPUT controle-inv-esp.ent-comp * -1,
                                             INPUT controle-inv-esp.ent-real * -1,
                                             OUTPUT TABLE tt-erro).
        IF  CAN-FIND (FIRST tt-erro) THEN DO:
            RUN cdp/cd0669.w (INPUT TABLE tt-erro).
            IF  VALID-HANDLE(h-esapi001) THEN DO:
                DELETE PROCEDURE h-esapi001.
                ASSIGN h-esapi001 = ?.
            END.
            RETURN "NOK":U.
        END.
        DELETE controle-inv-esp.
    END.
    
    RETURN "OK":U.
    
END PROCEDURE.

PROCEDURE pi-bt-export:

    RUN cnp/esp/escn0001.w.
    
END PROCEDURE.

PROCEDURE pi-bt-perc-it:
    
    FOR FIRST contrato-for NO-LOCK
        WHERE ROWID(contrato-for) = gr-contrato-for:

        RUN ccp/esp/escc0105.w (INPUT contrato-for.nr-contrato).
    END.
    
END PROCEDURE.

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

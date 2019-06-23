/*******************************************************************************
** Programa: pe0180-upc
** Autor...: Log­stica (log31237)
** Data....: 07/2008
** OBS.....: UPC utilizada pelo programa pe0180.w
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

DEF NEW GLOBAL SHARED VAR h-pe0180-upc    AS HANDLE        NO-UNDO.
DEF NEW GLOBAL SHARED VAR wgh-bt-parametros   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wgh-bt-perc-it  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-contrato-for AS ROWID         NO-UNDO.

def  temp-table tt-erro no-undo
    field i-sequen as int             
    field cd-erro  as int
    field mensagem as char format "x(255)"
    field c-param  as char.

FUNCTION getWidgetHandle RETURNS WIDGET-HANDLE (INPUT pWidget AS CHAR) FORWARD.

DEFINE NEW GLOBAL SHARED VARIABLE gr-categ_ptoelet AS ROWID NO-UNDO.

ASSIGN gr-categ_ptoelet = p-row-table.

IF  p-ind-event  = "INITIALIZE"
AND p-ind-object = "CONTAINER" THEN DO:
    
    RUN prghur/upc/pe0180-upc.p PERSISTENT SET h-pe0180-upc (INPUT "",            
                                                      INPUT "",            
                                                      INPUT p-wgh-object,  
                                                      INPUT p-wgh-frame,   
                                                      INPUT "",            
                                                      INPUT p-row-table).

    ASSIGN gr-categ_ptoelet = ?.

    CREATE BUTTON wgh-bt-parametros
    ASSIGN NAME      = "bt-parametros"
           FRAME     = p-wgh-frame
           WIDTH     = 5.00
           HEIGHT    = 1.25
           LABEL     = ""
           ROW       = 1.26
           COL       = 55.43
           TOOLTIP   = "Parƒmetros Horas Intineres"
           VISIBLE   = YES
           SENSITIVE = YES
           TRIGGERS:
            ON CHOOSE PERSISTENT RUN pi-bt-parametros IN h-pe0180-upc.
           END TRIGGERS.
           
    wgh-bt-parametros:LOAD-IMAGE-UP("prghur\image\horas_in_intinere.bmp").
    wgh-bt-parametros:MOVE-TO-TOP().

END.

IF  p-ind-event  = "DESTROY"
AND p-ind-object = "CONTAINER" THEN DO:
    
    ASSIGN wgh-bt-parametros  = ?.

    DELETE PROCEDURE h-pe0180-upc.    

END.


RETURN "OK".


PROCEDURE pi-bt-parametros:

    IF gr-categ_ptoelet <> ? THEN
       RUN prghur/esp/yg0008.w.
    
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

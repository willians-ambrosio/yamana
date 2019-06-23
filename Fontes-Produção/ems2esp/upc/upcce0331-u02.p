/* =========================================================================== 
   PROGRAMA      : UPCCE0331-U01.P
   DATA          : JULHO/2010
   DESENVOLVEDOR : Augusto Guimar∆es (Kraft Consulting)
   VERSAO        : 1.00
   OBJETIVO      :  
   =========================================================================== */
{include/i-prgvrs.i upcce0331-u01 2.06.00.000} 

/* ==========================================================================
                PARAMENTROS DE ENTRADA DOS OBJETOS,EVENTOS E TABELAS
   ==========================================================================*/

DEFINE INPUT PARAMETER p-ind-event     AS CHAR           NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object    AS CHAR           NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object    AS HANDLE         NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame     AS WIDGET-HANDLE  NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table     AS CHAR           NO-UNDO.
DEFINE INPUT PARAMETER p-row-table     AS ROWID          NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE whBtQuery     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE whBtArq    AS WIDGET-HANDLE NO-UNDO.

DEF VAR wgh-grupo       AS WIDGET-HANDLE NO-UNDO.
DEF VAR wgh-child       AS WIDGET-HANDLE NO-UNDO.


IF p-ind-event = "BEFORE-INITIALIZE" THEN
DO:
    ASSIGN wgh-grupo = p-wgh-frame:FIRST-CHILD.
    ASSIGN wgh-child = wgh-grupo:FIRST-CHILD. 
    
    DO WHILE VALID-HANDLE(wgh-child):
        IF  wgh-child:TYPE <> "field-group" THEN 
        DO:
            /* handle dos botoes */
            IF  wgh-child:NAME = "btQueryJoins" THEN 
            DO:
                ASSIGN whBtQuery = wgh-child.
            END.
    
            ASSIGN wgh-child = wgh-child:NEXT-SIBLING.
        END.
        ELSE
        DO:
            ASSIGN wgh-child = wgh-child:FIRST-CHILD.
        END.
    END.
    
    CREATE BUTTON whBtArq 
        ASSIGN FRAME     = p-wgh-frame
               WIDTH     = whBtQuery:WIDTH
               HEIGHT    = whBtQuery:height
               ROW       = whBtQuery:ROW
               COL       = whBtQuery:COL - 8
               VISIBLE   = YES
               SENSITIVE = NO
               TOOLTIP   = "Arq do Item".
    
        IF  whBtArq:LOAD-IMAGE("image\pbrush.bmp") THEN.
        IF  whBtArq:MOVE-TO-TOP() then.
END.
ELSE
DO:
        
    FIND item-uni-estab WHERE ROWID(item-uni-estab) = p-row-table NO-LOCK NO-ERROR.
    FIND FIRST ITEM WHERE ITEM.it-codigo = item-uni-estab.it-codigo NO-LOCK NO-ERROR.
    IF AVAILABLE ITEM THEN
        FIND FIRST mgesp.ext-item-arq NO-LOCK WHERE mgesp.ext-item-arq.it-codigo = ITEM.it-codigo NO-ERROR.
    IF AVAIL mgesp.ext-item-arq THEN
    DO:
        IF mgesp.ext-item-arq.image-pdf <> "" THEN
        DO:            
            ASSIGN whBtArq:SENSITIVE = YES.
        END.
        ELSE
        DO:            
            ASSIGN whBtArq:SENSITIVE = NO.
        END.
    END.
    ELSE ASSIGN whBtArq:SENSITIVE = NO.

    ON CHOOSE of whBtArq PERSISTENT RUN  upc/cd0204-upc05.p (INPUT rowid(ITEM)).
END.




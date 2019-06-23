/*----------------------------------------------------------
    PROGRAMA : upc-utb028aa-u00.p - bas_mapa_distrib_ccusto
    OBJETIVO : Bot∆o para copiar capa do Mapa de Distribuiá∆o de C.C.
    AUTOR    : Bruno Bertulli
    DATA     : 26/11/2012
------------------------------------------------------------*/

/*{include/i-prgvrs.i esfas701aa 5.06.00.000}*/
{tools/fc-handle-obj.i}
{tools/fc-falso.i}

DEF NEW GLOBAL SHARED VAR v_cod_empres_usuar AS CHAR NO-UNDO.

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-rec-table  AS RECID         NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE wh-utb028aa-bt-replic         AS WIDGET-HANDLE  NO-UNDO. 
DEFINE NEW GLOBAL SHARED VARIABLE wh-utb028aa-bt-copia          AS WIDGET-HANDLE  NO-UNDO. 

DEFINE  VARIABLE c-handle-obj  AS CHARACTER          NO-UNDO.
DEFINE  VARIABLE h-objeto      AS WIDGET-HANDLE      NO-UNDO.


/*
 
MESSAGE "1)   Teste de Pontos de UPC"                                   SKIP
        "2)   p-ind-event           " STRING(p-ind-event )              SKIP
        "3)   p-ind-object          " STRING(p-ind-object)              SKIP
        "4)   p-wgh-object          " STRING(p-wgh-object)              SKIP
        "4.1) p-wgh-object          " STRING(p-wgh-object:file-name)    SKIP
    
        "4.2) p-wgh-object          " STRING(p-wgh-object:type)         SKIP
        "4.3) p-wgh-object          " STRING(p-wgh-object:PRIVATE-DATA) SKIP
        "5)   p-wgh-FRAME           " STRING(p-wgh-FRAME )              SKIP
        "5.1) p-wgh-FRAME           " STRING(p-wgh-FRAME:name)          SKIP
        "5.2) p-wgh-FRAME           " STRING(p-wgh-FRAME:window)        SKIP
        "6)   p-cod-table           " STRING(p-cod-table )              SKIP
        "7)   p-rec-table           " STRING(p-rec-table )              SKIP
        "8)   self:type:            " self:type                         SKIP
        "9)   self:FRAME-name:      " self:FRAME-name                   SKIP
        "10)  self:name:            " self:name                         SKIP
        "11)                        " PROGRAM-NAME(0)                   SKIP
        "12)                        " PROGRAM-NAME(1)                   SKIP
        "13)                        " PROGRAM-NAME(2)                   SKIP
        "14)                        " PROGRAM-NAME(3)                   SKIP
        "15)                        " PROGRAM-NAME(4)                   SKIP
        "16)                        " PROGRAM-NAME(5)                   SKIP
            
        FRAME-value VIEW-AS ALERT-BOX.
   
*/

IF p-ind-event  = 'INITIALIZE' AND
   p-ind-object = 'VIEWER'     THEN DO:
  
    c-handle-obj                 = fc-handle-obj("bt_replicar",p-wgh-frame).
    wh-utb028aa-bt-replic        = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.


    CREATE BUTTON wh-utb028aa-bt-copia
    ASSIGN FRAME     = p-wgh-frame
           WIDTH     = wh-utb028aa-bt-replic:WIDTH
           HEIGHT    = wh-utb028aa-bt-replic:HEIGHT
           TOOLTIP   = "Copia Capa do Mapa"
           HELP      = "Copia Capa do Mapa"
           ROW       = wh-utb028aa-bt-replic:ROW
           COL       = wh-utb028aa-bt-replic:COL + 5
           FONT      = wh-utb028aa-bt-replic:FONT
           VISIBLE   = YES
           SENSITIVE = YES
           .

    wh-utb028aa-bt-copia:LOAD-IMAGE-UP("image/im-cop.bmp").
    
    ON "CHOOSE" OF wh-utb028aa-bt-copia
        PERSISTENT RUN upc\upc-utb028aa-u01.p ("CHOOSE",
                                               "bt-copia",
                                               wh-utb028aa-bt-copia,
                                               p-wgh-frame,
                                               "",
                                               p-rec-table).
    /* bisneto 15/07/2015
    wh-utb028aa-bt-copia:MOVE-AFTER-TAB-ITEM(wh-utb028aa-bt-replic:HANDLE).
    
    */
    wh-utb028aa-bt-copia:MOVE-TO-TOP().
END.

IF p-ind-event  = 'CHOOSE' AND
   p-ind-object = 'bt-copia' THEN DO:

    RUN esp/esutb028za.w.

END.


/* FIM DO PROGRAMA */ 

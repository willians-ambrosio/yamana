/*----------------------------------------------------------
    PROGRAMA : esfgl020aa-u00.p - bas_rat_ctbl
    OBJETIVO : Bot∆o para copiar Rateios Cont†beis
    AUTOR    : Bruno Bertulli
    DATA     : 10/06/2013
------------------------------------------------------------*/

{tools/fc-handle-obj.i}
{tools/fc-falso.i}

DEF NEW GLOBAL SHARED VAR v_cod_empres_usuar AS CHAR NO-UNDO.

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-rec-table  AS RECID         NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE wh-fgl020aa-bt-replic         AS WIDGET-HANDLE  NO-UNDO. 
DEFINE NEW GLOBAL SHARED VARIABLE wh-fgl020aa-bt-copia          AS WIDGET-HANDLE  NO-UNDO. 

DEFINE  VARIABLE c-handle-obj  AS CHARACTER          NO-UNDO.
DEFINE  VARIABLE h-objeto      AS WIDGET-HANDLE      NO-UNDO.

IF p-ind-event  = 'INITIALIZE' AND
   p-ind-object = 'VIEWER'     THEN DO:
  
    c-handle-obj                 = fc-handle-obj("bt_cop1",p-wgh-frame).
    wh-fgl020aa-bt-replic        = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.


    CREATE BUTTON wh-fgl020aa-bt-copia
    ASSIGN FRAME     = p-wgh-frame
           WIDTH     = wh-fgl020aa-bt-replic:WIDTH
           HEIGHT    = wh-fgl020aa-bt-replic:HEIGHT
           TOOLTIP   = "Copia Rateios Cont†beis"
           HELP      = "Copia Rateios Cont†beis"
           ROW       = wh-fgl020aa-bt-replic:ROW
           COL       = wh-fgl020aa-bt-replic:COL + 5
           FONT      = wh-fgl020aa-bt-replic:FONT
           VISIBLE   = YES
           SENSITIVE = YES
           .

    wh-fgl020aa-bt-copia:LOAD-IMAGE-UP("image/im-cop.bmp").
    
    ON "CHOOSE" OF wh-fgl020aa-bt-copia
        PERSISTENT RUN upc\esfgl020aa-u00.p ("CHOOSE",
                                             "bt-copia",
                                             wh-fgl020aa-bt-copia,
                                             p-wgh-frame,
                                             "",
                                             p-rec-table).
        
    wh-fgl020aa-bt-copia:MOVE-TO-TOP() no-error.
END.

IF p-ind-event  = 'CHOOSE' AND
   p-ind-object = 'bt-copia' THEN DO:

    RUN esp\esfgl002.w.

END.

/* FIM DO PROGRAMA */ 


/*-----------------------------------------------------------------------------------
    PROGRAMA : ft4003-upc01.p
    OBJETIVO : Chamanda de UPC do ft4003
    AUTOR    : Wellington Aparecido  (DSC)
    DATA     : 25/08/2008
-----------------------------------------------------------------------------------*/
/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{tools/fc-handle-obj.i}
{tools/fc-falso.i}                                

/*************************************************************************************
                                     PARAMETROS
*************************************************************************************/

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

/*************************************************************************************
                                    VARIAVEIS GLOBAIS
*************************************************************************************/
DEFINE NEW GLOBAL SHARED VARIABLE wh-ft4003-upc01-serie             AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-ft4003-upc01-cod-estabel       AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-ft4003-upc01-btOk              AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-ft4003-upc01-btOk-fs           AS WIDGET-HANDLE NO-UNDO.



/*************************************************************************************
                                    VARIAVEIS LOCAIS
*************************************************************************************/

DEFINE VARIABLE c-handle-obj  AS CHARACTER        NO-UNDO.
DEFINE VARIABLE rw-ext-nf-e   AS ROWID            NO-UNDO.
  
/*RUN pi-msg.      */

PROCEDURE pi-msg:
MESSAGE "p-ind-event..:" p-ind-event                  SKIP
        "p-ind-object.:" p-ind-object                 SKIP
        "p-cod-table..:" STRING(p-cod-table)          SKIP
        "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    SKIP
        "p-wgh-frame..:" STRING(p-wgh-frame)          SKIP
        "p-row-table..:" string(p-row-table)          skip
        VIEW-AS ALERT-BOX INFO BUTTONS OK.                    
END.



IF p-ind-event = "AFTER-INITIALIZE" AND p-ind-object = "CONTAINER" THEN DO:

  c-handle-obj   = fc-handle-obj ("serie",p-wgh-frame).
  wh-ft4003-upc01-serie = WIDGET-HANDLE(ENTRY(01,c-handle-obj)) NO-ERROR.
  c-handle-obj   = fc-handle-obj ("cod-estabel",p-wgh-frame).
  wh-ft4003-upc01-cod-estabel = WIDGET-HANDLE(ENTRY(01,c-handle-obj)) NO-ERROR.
  c-handle-obj   = fc-handle-obj ("btOk",p-wgh-frame).
  wh-ft4003-upc01-btOk = WIDGET-HANDLE(ENTRY(01,c-handle-obj)) NO-ERROR.
  
  wh-ft4003-upc01-btOk-fs = fc-falso(wh-ft4003-upc01-btOk ,
                                     wh-ft4003-upc01-btOk:FRAME,
                                     "upc/ft4003-upc02.p").
  
  wh-ft4003-upc01-btOk-fs:MOVE-TO-TOP().
  wh-ft4003-upc01-btOk-fs:MOVE-AFTER(wh-ft4003-upc01-btOk).
  wh-ft4003-upc01-btOk-fs:SENSITIVE = YES. 
  wh-ft4003-upc01-btOk-fs:COLUMN = wh-ft4003-upc01-btOk:COLUMN.
  wh-ft4003-upc01-btOk:SENSITIVE = NO.
END.                                  
                                    

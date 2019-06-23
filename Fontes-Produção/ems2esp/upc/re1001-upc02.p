/*****************************************************************************
* Empresa  : DKP
* Cliente  : YAMANA
* Programa : upc/re1001-upc02.p
* Descricao: Botao bt-geracao , criar desconto - Conforme acionamento botao 
* Autor    : Ramon - DKP
* Data     : 03/2018
*
******************************************************************************/

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.
  
DEF VAR c-objeto        AS CHAR           NO-UNDO.
DEF VAR h-objeto        AS WIDGET-HANDLE  NO-UNDO.
DEF VAR h_frame         AS WIDGET-HANDLE  NO-UNDO.
  
 
DEF NEW GLOBAL SHARED VARIABLE wh-handle-obj      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-container       AS WIDGET-HANDLE NO-UNDO.
   
DEF NEW GLOBAL SHARED VARIABLE wh-brSon2          AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-bt-geracao      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-bt-geracao-esp  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-bt-confirma-esp AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VARIABLE wh-bt-confirma AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VARIABLE wh-re1001-bt-ger-row  AS ROWID NO-UNDO.

/* message p-ind-event */
/* p-ind-object        */
/* p-wh-object         */
/* p-wh-frame          */
/* p-cod-table         */
/* string(p-row-table) */
/* view-as alert-box.  */


IF p-ind-event  = "AFTER-DISPLAY" AND 
   p-ind-object = "CONTAINER"     AND
   p-cod-table  = "docum-est"     AND 
   p-row-table  <> ?              THEN DO: 

    FIND FIRST docum-est WHERE
        ROWID(docum-est) = p-row-table NO-LOCK NO-ERROR.
    IF AVAIL docum-est THEN
        ASSIGN wh-re1001-bt-ger-row = p-row-table.
END.


IF p-ind-event  = "AFTER-OPEN-QUERY" AND 
   p-ind-object = "CONTAINER"       AND
   p-cod-table  = "dupli-apagar"    AND 
   p-row-table  <> ?              
    THEN ASSIGN wh-container =  p-wh-object.

IF  VALID-HANDLE(wh-bt-geracao)
AND VALID-HANDLE(wh-bt-geracao-esp) THEN DO:

    ASSIGN wh-bt-geracao-esp:VISIBLE   = wh-bt-geracao:VISIBLE
           wh-bt-geracao-esp:SENSITIVE = wh-bt-geracao:SENSITIVE.
  
    IF wh-bt-geracao-esp:MOVE-TO-TOP() THEN.
  
END.

IF p-ind-event      = "before-initialize" AND
   p-ind-object     = "container"         THEN DO:
 
    ASSIGN wh-handle-obj = p-wh-frame:FIRST-CHILD.
           
    DO WHILE VALID-HANDLE(wh-handle-obj):

        IF wh-handle-obj:NAME = "fpage2" THEN DO:
    
             ASSIGN p-wh-frame = wh-handle-obj.
        
             ASSIGN wh-handle-obj = p-wh-frame:FIRST-CHILD.
        
             DO WHILE VALID-HANDLE(wh-handle-obj):
        
                 IF wh-handle-obj:NAME = "brSon2" THEN
                    ASSIGN wh-brSon2 = wh-handle-obj.

                 IF wh-handle-obj:NAME = "bt-geracao" THEN DO:
                   
                     ASSIGN wh-bt-geracao = wh-handle-obj.
 
                     CREATE BUTTON wh-bt-geracao-esp
                     ASSIGN  FRAME      = wh-bt-geracao:FRAME    
                             VISIBLE    = wh-bt-geracao:VISIBLE
                             SENSITIVE  = YES
                             ROW        = wh-bt-geracao:ROW      
                             COLUMN     = wh-bt-geracao:COLUMN /*( wh-bt-geracao:COLUMN - 4 )*/  
                             HEIGHT     = wh-bt-geracao:HEIGHT   
                             WIDTH      = wh-bt-geracao:WIDTH    
                             LABEL      = "*Gera‡Æo*"
                             FONT       = wh-bt-geracao:FONT.  
                 
              
                     If wh-bt-geracao-esp:Move-to-top() Then.                                                   

                     wh-bt-geracao-esp:MOVE-TO-TOP(). 
                  
                     ON 'choose':U OF wh-bt-geracao-esp PERSISTENT RUN upc/re1001-upc02a.p (INPUT p-ind-event ,
                                                                                            INPUT p-ind-object,
                                                                                            INPUT p-wh-object ,
                                                                                            INPUT p-wh-frame  ,
                                                                                            INPUT p-cod-table ,
                                                                                            INPUT p-row-table).
  
                 END.
        
                IF wh-handle-obj:TYPE = "field-group" THEN 
                   ASSIGN wh-handle-obj = wh-handle-obj:FIRST-CHILD.
                ELSE 
                   ASSIGN wh-handle-obj = wh-handle-obj:NEXT-SIBLING.
    
            END.
            LEAVE.
    
        END.
        
        IF wh-handle-obj:TYPE = "field-group" THEN 
           ASSIGN wh-handle-obj = wh-handle-obj:FIRST-CHILD.
        ELSE 
           ASSIGN wh-handle-obj = wh-handle-obj:NEXT-SIBLING.

    END.
END.    

/* fim - re1001-upc02.p */

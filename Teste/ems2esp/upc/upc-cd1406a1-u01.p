/***************************************************************************************************
**    Programa: upc-cd1406-u01.p
**    Objetivo: Validar Campos OI x Conta
**       Autor: Willians Moreira Ambrosio - Grupo DKP
**   Descri‡Æo: InclusÆo de valida‡Æo do campo ordem investimento x conta contabil
***************************************************************************************************/

{include/i-prgvrs.i upc-cd1406-U01 12.01.19.001} 
{tools/fc-handle-obj.i}

DEF INPUT PARAM p-ind-event  AS CHAR          no-undo.
DEF INPUT PARAM p-ind-object AS CHAR          no-undo.
DEF INPUT PARAM p-wgh-object AS HANDLE        no-undo.
DEF INPUT PARAM p-wgh-frame  AS WIDGET-HANDLE no-undo.
DEF INPUT PARAM p-cod-table  AS CHAR          no-undo.
DEF INPUT PARAM p-row-table  AS ROWID         no-undo.

def new global shared var wh-cd1406-num-ord-inv   AS widget-handle no-undo.
def new global shared var wh-cd1406-ct-codigo     AS widget-handle no-undo.
def new global shared var wh-cd1406-bt-ok         AS widget-handle no-undo. 
def new global shared var wh-cd1406-bt-ok-falso   AS widget-handle no-undo. 
def new global shared var wh-cd1406-bt-save       AS widget-handle no-undo. 
def new global shared var wh-cd1406-bt-save-falso AS widget-handle no-undo. 
/* --------------------------------------------------------------------- */
DEF VAR c-handle-obj             AS CHARACTER                  NO-UNDO.
DEF VAR c-ct-conta               AS CHARACTER                  NO-UNDO.
def var c-aux                    AS CHARACTER                  NO-UNDO.
/* --------------------------------------------------------------------- */
IF p-ind-event  = "INITIALIZE" AND
   p-ind-object = "CONTAINER"  THEN 
DO:
   c-handle-obj          = fc-handle-obj("num-ord-inv,ct-codigo,bt-ok,bt-save", p-wgh-frame:PARENT).
   wh-cd1406-num-ord-inv = WIDGET-HANDLE(ENTRY(1,c-handle-obj)).
   wh-cd1406-ct-codigo   = WIDGET-HANDLE(ENTRY(2,c-handle-obj)).
   wh-cd1406-bt-ok       = WIDGET-HANDLE(ENTRY(3,c-handle-obj)).
   wh-cd1406-bt-save     = WIDGET-HANDLE(ENTRY(4,c-handle-obj)).
      
   IF VALID-HANDLE(wh-cd1406-bt-ok) THEN 
   DO:
      CREATE BUTTON wh-cd1406-bt-ok-falso
      ASSIGN ROW       = wh-cd1406-bt-ok:ROW
             COLUMN    = wh-cd1406-bt-ok:COL
             WIDTH     = wh-cd1406-bt-ok:WIDTH 
             HEIGHT    = wh-cd1406-bt-ok:HEIGHT
             FRAME     = wh-cd1406-bt-ok:FRAME
             SENSITIVE = wh-cd1406-bt-ok:SENSITIVE
             VISIBLE   = wh-cd1406-bt-ok:VISIBLE
             LABEL     = "*" + wh-cd1406-bt-ok:LABEL
             TOOLTIP   = wh-cd1406-bt-ok:TOOLTIP
             TRIGGERS:
                  ON CHOOSE PERSISTENT RUN upc/upc-cd1406a1-u01.p (INPUT "wh-cd1406-bt-ok-falso",   
                                                                   INPUT "upc-cd1406-u01"    ,  
                                                                   INPUT p-wgh-object        ,  
                                                                   INPUT p-wgh-frame         ,  
                                                                   INPUT p-cod-table         ,  
                                                                   INPUT p-row-table         ).     
             END TRIGGERS.             
   END.

   IF VALID-HANDLE(wh-cd1406-bt-save) THEN 
   DO:
      CREATE BUTTON wh-cd1406-bt-save-falso
      ASSIGN ROW       = wh-cd1406-bt-save:ROW
             COLUMN    = wh-cd1406-bt-save:COL
             WIDTH     = wh-cd1406-bt-save:WIDTH 
             HEIGHT    = wh-cd1406-bt-save:HEIGHT
             FRAME     = wh-cd1406-bt-save:FRAME
             SENSITIVE = wh-cd1406-bt-save:SENSITIVE
             VISIBLE   = wh-cd1406-bt-save:VISIBLE
             LABEL     = "*" + wh-cd1406-bt-save:LABEL
             TOOLTIP   = wh-cd1406-bt-save:TOOLTIP
             TRIGGERS:
                  ON CHOOSE PERSISTENT RUN upc/upc-cd1406a1-u01.p (INPUT "wh-cd1406-bt-save-falso",   
                                                                   INPUT "upc-cd1406-u01"    ,  
                                                                   INPUT p-wgh-object        ,  
                                                                   INPUT p-wgh-frame         ,  
                                                                   INPUT p-cod-table         ,  
                                                                   INPUT p-row-table         ).     
             END TRIGGERS.             
   END.
END. 

IF  p-ind-event   = "wh-cd1406-bt-ok-falso" AND
    p-ind-object  = "upc-cd1406-u01"        THEN 
DO:
    ASSIGN c-ct-conta = wh-cd1406-ct-codigo:SCREEN-VALUE
           c-ct-conta = REPLACE(c-ct-conta,".","")
           c-aux      = IF wh-cd1406-num-ord-inv:SCREEN-VALUE = "0" THEN "sem" ELSE "com".



     IF ((wh-cd1406-num-ord-inv:SCREEN-VALUE <> "0" AND (c-ct-conta <> "17202014" AND SUBSTRING(c-ct-conta,1,1) <> "9"))  OR
         (wh-cd1406-num-ord-inv:SCREEN-VALUE =  "0" AND (c-ct-conta  = "17202014" OR  SUBSTRING(c-ct-conta,1,1)  = "9"))) THEN
     DO:
        RUN utp/ut-msgs.p (INPUT "show",
                           INPUT 17006,
                           INPUT "Conta Cont bil Inv lida!~~Conta Cont bil 17202014 ou iniciadas com 9, devem ser utilizadas " + c-aux + " ordem investimento.").
        
        APPLY "ENTRY" TO wh-cd1406-ct-codigo.                
        RETURN "NOK".
    END.

    APPLY "CHOOSE" TO wh-cd1406-bt-ok.
END.

IF  p-ind-event   = "wh-cd1406-bt-save-falso" AND
    p-ind-object  = "upc-cd1406-u01"        THEN 
DO:
    ASSIGN c-ct-conta = wh-cd1406-ct-codigo:SCREEN-VALUE
           c-ct-conta = REPLACE(c-ct-conta,".","")
           c-aux      = IF wh-cd1406-num-ord-inv:SCREEN-VALUE = "0" THEN "sem" ELSE "com".



     IF ((wh-cd1406-num-ord-inv:SCREEN-VALUE <> "0" AND (c-ct-conta <> "17202014" AND SUBSTRING(c-ct-conta,1,1) <> "9"))  OR
         (wh-cd1406-num-ord-inv:SCREEN-VALUE =  "0" AND (c-ct-conta  = "17202014" OR  SUBSTRING(c-ct-conta,1,1)  = "9"))) THEN
     DO:
        RUN utp/ut-msgs.p (INPUT "show",
                           INPUT 17006,
                           INPUT "Conta Cont bil Inv lida!~~Conta Cont bil 17202014 ou iniciadas com 9, devem ser utilizadas " + c-aux + " ordem investimento.").
        
        APPLY "ENTRY" TO wh-cd1406-ct-codigo.                
        RETURN "NOK".
    END.

    APPLY "CHOOSE" TO wh-cd1406-bt-save.
END.


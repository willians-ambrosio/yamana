/***************************************************************************************************
**    Programa: upc-cn0302d-u01.p
**    Objetivo: Validar Campos OI x Conta
**       Autor: Willians Moreira Ambrosio - Grupo DKP
**   Descriá∆o: Inclus∆o de validaá∆o do campo ordem investimento x conta contabil
***************************************************************************************************/
{include/i-prgvrs.i upc-cn0302d-U01 12.01.19.001} 
{tools/fc-handle-obj.i}
/* --------------------------------------------------------------------- */
DEF INPUT PARAM p-ind-event  AS CHAR          no-undo.
DEF INPUT PARAM p-ind-object AS CHAR          no-undo.
DEF INPUT PARAM p-wgh-object AS HANDLE        no-undo.
DEF INPUT PARAM p-wgh-frame  AS WIDGET-HANDLE no-undo.
DEF INPUT PARAM p-cod-table  AS CHAR          no-undo.
DEF INPUT PARAM p-row-table  AS ROWID         no-undo.
/* --------------------------------------------------------------------- */
def new global shared var wh-cn0302d-ct-codigo         AS widget-handle no-undo.
def new global shared var wh-cn0302d-bt-ok             AS widget-handle no-undo. 
def new global shared var wh-cn0302d-bt-ok-falso       AS widget-handle no-undo.
def new global shared var wh-cn0302d-br-target-browse AS widget-handle no-undo. 
def new global shared var wh-cn0302d-query             AS widget-handle no-undo. 
def new global shared var wh-cn0302d-buffer            AS widget-handle no-undo. 
def new global shared var wh-cn0302d-ct-codigo         AS widget-handle no-undo. 
def new global shared var wh-i-ordem-invest-cn0302d    AS WIDGET-HANDLE NO-UNDO.
/* --------------------------------------------------------------------- */
DEFINE VARIABLE c-handle-obj  AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-ct-conta    AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-aux         AS CHARACTER NO-UNDO.
DEFINE VARIABLE l-ult         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE l-erro        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE i-cont        AS INTEGER   NO-UNDO.
/* --------------------------------------------------------------------- */
IF p-ind-event  = "INITIALIZE" AND
   p-ind-object = "CONTAINER"  THEN 
DO:
   c-handle-obj                 = fc-handle-obj("br-target-browse,bt-ok,fi-i-num-ord-inv", p-wgh-frame:PARENT).   
   wh-cn0302d-br-target-browse  = WIDGET-HANDLE(ENTRY(1,c-handle-obj)).
   wh-cn0302d-bt-ok             = WIDGET-HANDLE(ENTRY(2,c-handle-obj)).
   wh-i-ordem-invest-cn0302d    = WIDGET-HANDLE(ENTRY(3,c-handle-obj)).
      
   IF VALID-HANDLE(wh-cn0302d-bt-ok) THEN 
   DO:
      CREATE BUTTON wh-cn0302d-bt-ok-falso
      ASSIGN ROW       = wh-cn0302d-bt-ok:ROW
             COLUMN    = wh-cn0302d-bt-ok:COL
             WIDTH     = wh-cn0302d-bt-ok:WIDTH 
             HEIGHT    = wh-cn0302d-bt-ok:HEIGHT
             FRAME     = wh-cn0302d-bt-ok:FRAME
             SENSITIVE = wh-cn0302d-bt-ok:SENSITIVE
             VISIBLE   = wh-cn0302d-bt-ok:VISIBLE
             LABEL     = "*" + wh-cn0302d-bt-ok:LABEL
             TOOLTIP   = wh-cn0302d-bt-ok:TOOLTIP
             TRIGGERS:
                  ON CHOOSE PERSISTENT RUN upc/upc-cn0302d-u01.p(INPUT "wh-cn0302d-bt-ok-falso",   
                                                                 INPUT "upc-cn0302d-u01"       ,  
                                                                 INPUT p-wgh-object            ,  
                                                                 INPUT p-wgh-frame             ,  
                                                                 INPUT p-cod-table             ,  
                                                                 INPUT p-row-table             ).     
             END TRIGGERS.             
   END.

   IF VALID-HANDLE(wh-cn0302d-br-target-browse) THEN 
   DO:

      ON 'row-display':U OF wh-cn0302d-br-target-browse PERSISTEN RUN upc/upc-cn0302d-u01.p(INPUT "row-display",
                                                                                            INPUT "wh-cn0302d-br-target-browse",
                                                                                            INPUT p-wgh-object,
                                                                                            INPUT p-wgh-frame,
                                                                                            INPUT ?,
                                                                                            INPUT ?).
      wh-cn0302d-query        = wh-cn0302d-br-target-browse:QUERY.
      wh-cn0302d-buffer       = wh-cn0302d-query:GET-BUFFER-HANDLE(1).

   END.
END. 


IF p-ind-event   = "row-display"                 AND
   p-ind-object  = "wh-cn0302d-br-target-browse" THEN 
DO:    
  ASSIGN c-ct-conta = wh-cn0302d-buffer:BUFFER-FIELD(8):BUFFER-VALUE().
END.

IF p-ind-event   = "wh-cn0302d-bt-ok-falso" AND
   p-ind-object  = "upc-cn0302d-u01"        THEN 
DO:    

  ASSIGN l-erro = NO.

  wh-cn0302d-query:GET-FIRST(NO-LOCK).
  DO i-cont = 1 TO wh-cn0302d-query:NUM-RESULTS:

     ASSIGN c-ct-conta = wh-cn0302d-buffer:BUFFER-FIELD(8):BUFFER-VALUE()
            c-ct-conta = REPLACE(c-ct-conta,".","").

     IF ((wh-i-ordem-invest-cn0302d:SCREEN-VALUE <> "0" AND (c-ct-conta <> "17202014" AND SUBSTRING(c-ct-conta,1,1) <> "9"))  OR
         (wh-i-ordem-invest-cn0302d:SCREEN-VALUE =  "0" AND (c-ct-conta  = "17202014" OR  SUBSTRING(c-ct-conta,1,1)  = "9"))) THEN
        ASSIGN l-erro = YES.

     wh-cn0302d-query:GET-NEXT().
  END.

  IF l-erro THEN
  DO:
     ASSIGN c-aux      = IF wh-i-ordem-invest-cn0302d:SCREEN-VALUE = "0" THEN "sem" ELSE "com".

     RUN utp/ut-msgs.p (INPUT "show",
                        INPUT 17006,
                        INPUT "Conta Cont†bil Inv†lida!~~Conta Cont†bil 17202014 ou iniciadas com 9, devem ser utilizadas " + c-aux + " ordem investimento.").

     APPLY "ENTRY" TO wh-cn0302d-bt-ok-falso.
     RETURN "NOK".
  END.

  APPLY "CHOOSE" TO wh-cn0302d-bt-ok.
END.

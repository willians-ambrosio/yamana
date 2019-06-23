/***************************************************************************************************
**    Programa: upc-cn0201c3-u01.p
**    Objetivo: Validar Campos OI x Conta
**       Autor: Willians Moreira Ambrosio - Grupo DKP
**   Descriá∆o: Inclus∆o de validaá∆o do campo ordem investimento x conta contabil
***************************************************************************************************/
{include/i-prgvrs.i upc-cn0201c3-U01 12.01.19.001} 
{tools/fc-handle-obj.i}
/* --------------------------------------------------------------------- */
DEF INPUT PARAM p-ind-event  AS CHAR          no-undo.
DEF INPUT PARAM p-ind-object AS CHAR          no-undo.
DEF INPUT PARAM p-wgh-object AS HANDLE        no-undo.
DEF INPUT PARAM p-wgh-frame  AS WIDGET-HANDLE no-undo.
DEF INPUT PARAM p-cod-table  AS CHAR          no-undo.
DEF INPUT PARAM p-row-table  AS ROWID         no-undo.
/* --------------------------------------------------------------------- */
def new global shared var wh-cn0201c3-ct-codigo         AS widget-handle no-undo.
def new global shared var wh-cn0201c3-bt-ok             AS widget-handle no-undo. 
def new global shared var wh-cn0201c3-bt-ok-falso       AS widget-handle no-undo.
def new global shared var wh-cn0201c3-br-target-browse AS widget-handle no-undo. 
def new global shared var wh-cn0201c3-query             AS widget-handle no-undo. 
def new global shared var wh-cn0201c3-buffer            AS widget-handle no-undo. 
def new global shared var wh-cn0201c3-ct-codigo         AS widget-handle no-undo. 
def new global shared var wh-i-ordem-invest-cn0201c3    AS WIDGET-HANDLE NO-UNDO.
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
   c-handle-obj                 = fc-handle-obj("br-target-browse,bt-ok", p-wgh-frame:PARENT).   
   wh-cn0201c3-br-target-browse  = WIDGET-HANDLE(ENTRY(1,c-handle-obj)).
   wh-cn0201c3-bt-ok             = WIDGET-HANDLE(ENTRY(2,c-handle-obj)).
      
   IF VALID-HANDLE(wh-cn0201c3-bt-ok) THEN 
   DO:
      CREATE BUTTON wh-cn0201c3-bt-ok-falso
      ASSIGN ROW       = wh-cn0201c3-bt-ok:ROW
             COLUMN    = wh-cn0201c3-bt-ok:COL
             WIDTH     = wh-cn0201c3-bt-ok:WIDTH 
             HEIGHT    = wh-cn0201c3-bt-ok:HEIGHT
             FRAME     = wh-cn0201c3-bt-ok:FRAME
             SENSITIVE = wh-cn0201c3-bt-ok:SENSITIVE
             VISIBLE   = wh-cn0201c3-bt-ok:VISIBLE
             LABEL     = "*" + wh-cn0201c3-bt-ok:LABEL
             TOOLTIP   = wh-cn0201c3-bt-ok:TOOLTIP
             TRIGGERS:
                  ON CHOOSE PERSISTENT RUN upc/upc-cn0201c3-u01.p(INPUT "wh-cn0201c3-bt-ok-falso",   
                                                                 INPUT "upc-cn0201c3-u01"       ,  
                                                                 INPUT p-wgh-object            ,  
                                                                 INPUT p-wgh-frame             ,  
                                                                 INPUT p-cod-table             ,  
                                                                 INPUT p-row-table             ).     
             END TRIGGERS.             
   END.

   IF VALID-HANDLE(wh-cn0201c3-br-target-browse) THEN 
   DO:

      ON 'row-display':U OF wh-cn0201c3-br-target-browse PERSISTEN RUN upc/upc-cn0201c3-u01.p(INPUT "row-display",
                                                                                            INPUT "wh-cn0201c3-br-target-browse",
                                                                                            INPUT p-wgh-object,
                                                                                            INPUT p-wgh-frame,
                                                                                            INPUT ?,
                                                                                            INPUT ?).
      wh-cn0201c3-query        = wh-cn0201c3-br-target-browse:QUERY.
      wh-cn0201c3-buffer       = wh-cn0201c3-query:GET-BUFFER-HANDLE(1).

   END.
END. 


IF p-ind-event   = "row-display"                 AND
   p-ind-object  = "wh-cn0201c3-br-target-browse" THEN 
DO:    
  ASSIGN c-ct-conta = wh-cn0201c3-buffer:BUFFER-FIELD(3):BUFFER-VALUE().
END.

IF p-ind-event   = "wh-cn0201c3-bt-ok-falso" AND
   p-ind-object  = "upc-cn0201c3-u01"        THEN 
DO:    
  ASSIGN l-erro = NO.

  wh-cn0201c3-query:GET-FIRST(NO-LOCK).
  DO i-cont = 1 TO wh-cn0201c3-query:NUM-RESULTS:

     ASSIGN c-ct-conta = wh-cn0201c3-buffer:BUFFER-FIELD(3):BUFFER-VALUE()
            c-ct-conta = REPLACE(c-ct-conta,".","").

     IF ((wh-i-ordem-invest-cn0201c3:SCREEN-VALUE <> "0" AND (c-ct-conta <> "17202014" AND SUBSTRING(c-ct-conta,1,1) <> "9"))  OR
         (wh-i-ordem-invest-cn0201c3:SCREEN-VALUE =  "0" AND (c-ct-conta  = "17202014" OR  SUBSTRING(c-ct-conta,1,1)  = "9"))) THEN
        ASSIGN l-erro = YES.

     wh-cn0201c3-query:GET-NEXT().
  END.

  IF l-erro THEN
  DO:
     ASSIGN c-aux      = IF wh-i-ordem-invest-cn0201c3:SCREEN-VALUE = "0" THEN "sem" ELSE "com".

     RUN utp/ut-msgs.p (INPUT "show",
                        INPUT 17006,
                        INPUT "Conta Cont†bil Inv†lida!~~Conta Cont†bil 17202014 ou iniciadas com 9, devem ser utilizadas " + c-aux + " ordem investimento.").

     APPLY "ENTRY" TO wh-cn0201c3-bt-ok-falso.
     RETURN "NOK".
  END.

  APPLY "CHOOSE" TO wh-cn0201c3-bt-ok.
END.

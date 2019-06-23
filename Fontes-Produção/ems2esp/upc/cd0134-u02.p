/* =========================================================================== 
   PROGRAMA    : CD0134-U02.p
   DATA        :  05/05/2015
   DESENVOLVIDO: Carlos Souza - DSC
   VERSAO      : 001
   OBJETIVO    : UPC no programa manut familias materiais. 
   =========================================================================== */
{include/i-prgvrs.i  cd0134-u02.P 2.06.00.000}

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

DEF VAR h-objeto                 AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0134-cb-tipo-contr   AS WIDGET-HANDLE NO-UNDO.



/*  RUN pi-msg. */

 run tela-upc(input p-wgh-frame,      
                 input p-ind-event,      
                 input p-wgh-object:type,
                 input p-wgh-object:name,
                 input no,             
                 output p-wgh-object).



IF VALID-HANDLE(wh-cd0134-cb-tipo-contr) THEN DO:

    wh-cd0134-cb-tipo-contr:SENSITIVE = NO. 

END.





IF RETURN-VALUE = "NOK" THEN 
  RETURN "NOK":U.
                                                
RETURN RETURN-VALUE.


procedure pi-msg:
message "p-ind-event..:" p-ind-event                  skip 
        "p-ind-object.:" p-ind-object                 skip 
        "p-cod-table..:" STRING(p-cod-table)          skip 
        "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    skip 
        "p-wgh-frame..:" STRING(p-wgh-frame)          skip 
        "p-row-table..:" string(p-row-table)          skip 
        VIEW-AS ALERT-BOX INFO BUTTONS OK.                    
END PROCEDURE.


PROCEDURE tela-upc:
    DEFINE INPUT  PARAMETER  pWghFrame    AS WIDGET-HANDLE NO-UNDO.  
    DEFINE INPUT  PARAMETER  pIndEvent    AS CHARACTER     NO-UNDO.      
    DEFINE INPUT  PARAMETER  pObjType     AS CHARACTER     NO-UNDO.  
    DEFINE INPUT  PARAMETER  pObjName     AS CHARACTER     NO-UNDO.   
    DEFINE INPUT  PARAMETER  pApresMsg    AS LOGICAL       NO-UNDO.  
    DEFINE OUTPUT PARAMETER  phObj        AS HANDLE        NO-UNDO.  
    
    DEFINE VARIABLE wgh-obj AS WIDGET-HANDLE NO-UNDO.

    ASSIGN wgh-obj = pWghFrame:FIRST-CHILD.

    DO  WHILE VALID-HANDLE(wgh-obj):


      
        IF wgh-obj:NAME = "cb-tipo-contr" THEN wh-cd0134-cb-tipo-contr = wgh-obj.


        
        IF pApresMsg = YES and wgh-obj:name = "responsavel" then 
            do:

            
                MESSAGE "Nome do Objeto" wgh-obj:NAME SKIP
                        "Type do Objeto" wgh-obj:TYPE SKIP
                        "P-Ind-Event"    pIndEvent VIEW-AS ALERT-BOX.
            end.
        
        IF wgh-obj:TYPE = pObjType AND
           wgh-obj:NAME = pObjName THEN DO:
            ASSIGN phObj = wgh-obj:HANDLE.
            LEAVE.
        END.
        IF wgh-obj:TYPE = "field-group" THEN
            ASSIGN wgh-obj = wgh-obj:FIRST-CHILD.
        ELSE
            if wgh-obj:TYPE = "frame" and
               wgh-obj:name = "fpage1" then 
                ASSIGN wgh-obj = wgh-obj:FIRST-CHILD.
            else
                ASSIGN wgh-obj = wgh-obj:NEXT-SIBLING.

    END.

END PROCEDURE. 


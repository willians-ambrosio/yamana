/*
**    
**    
**    
** Atualizaá∆o:  
**
*/
{tools/fc-handle-obj.i}
{tools/fc-falso.i}


DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO. 
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

/* parametros de execuá∆o */

/* variaveis locais */
DEF VAR c-handle-obj           AS CHAR                NO-UNDO.
DEF VAR wh-menu                AS WIDGET-HANDLE       NO-UNDO.
DEF VAR wh-menu-item           AS WIDGET-HANDLE       NO-UNDO.
DEF VAR wh-objeto              AS WIDGET-HANDLE       NO-UNDO.
DEF VAR wh-label               AS WIDGET-HANDLE       NO-UNDO.
DEF VAR wh-button              AS WIDGET-HANDLE       NO-UNDO.

DEF VAR wh-pesquisa            AS WIDGET-HANDLE       NO-UNDO.
DEF VAR l-implanta             AS LOG                 NO-UNDO.
DEF VAR tot-limite-diarias     AS INT                 NO-UNDO.
DEFINE VARIABLE i-conta2 AS INTEGER     NO-UNDO.




/* variaveis globais */

DEF NEW GLOBAL SHARED VAR wh-cd0204-upc        AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-classe1         AS WIDGET-HANDLE NO-UNDO.


DEF NEW GLOBAL SHARED VAR wh-bt-cfa         AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-it-codigo      AS WIDGET-HANDLE NO-UNDO.

/* definiá∆o de buffers */

/* definiá∆o temp-table */
/* includes padr∆o */

DEFINE VARIABLE c-objeto  AS CHARACTER   NO-UNDO.


assign c-objeto   = entry(num-entries(p-wgh-object:file-name, "~/"), p-wgh-object:file-name, "~/").

RUN findWidget (INPUT "it-codigo",INPUT "FILL-IN",INPUT p-wgh-frame, OUTPUT wh-it-codigo).







IF p-ind-event = "INITIALIZE" AND
   P-IND-OBJECT = "container"    THEN DO:

 
 
           CREATE BUTTON wh-bt-cfa                                                                       
               ASSIGN  ROW         = 1.21                                                      
                       COLUMN      = 61                                                    
                       WIDTH       = 4                                                          
                       HEIGHT      = 1                                                         
                       LABEL       = "CFA"                                                    
                       FRAME       = p-wgh-frame                                                          
                       FLAT-BUTTON = TRUE                                                   
                       TOOLTIP     = ""                                                       
                       HELP        = ""                                                         
                       NAME        = "wh-bt-cfa"                                                           
                       SENSITIVE = TRUE                                                                   
                       VISIBLE   = TRUE  .                                                             
          
                                                                                                          
           ON "CHOOSE" OF wh-bt-cfa PERSISTENT RUN esp\ymof0116.w (INPUT wh-it-codigo).


                   
    

END.
 
IF p-ind-event = "DISPLAY" AND
   P-IND-OBJECT = "VIEWER" THEN DO:


 

   
     
END.





PROCEDURE findWidget:
    /*
    * PARAMETROS:
    *   c-widget-name:  nome do widget a ser localizado
    *   c-widget-type:  tipo do widget a ser localizado
    *   h-start-widget: container para procurar o widget
    *   h-widget:       widget encontrado 
    */

    define input  parameter c-widget-name  as char   no-undo.
    define input  parameter c-widget-type  as char   no-undo.
    define input  parameter h-start-widget as handle no-undo.
    define output parameter h-widget       as handle no-undo.

    do while valid-handle(h-start-widget):
        if h-start-widget:name = c-widget-name and
           h-start-widget:type = c-widget-type then do:
            assign h-widget = h-start-widget:handle.
            leave.
        end.

        if h-start-widget:type = "field-group":u or
           h-start-widget:type = "frame":u or
           h-start-widget:type = "dialog-box":u then do:
            run findWidget (input  c-widget-name,
                            input  c-widget-type,
                            input  h-start-widget:first-child,
                            output h-widget).

            if valid-handle(h-widget) then
                leave.
        end.
        assign h-start-widget = h-start-widget:next-sibling.
    end.
END PROCEDURE.


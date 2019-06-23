/* =========================================================================== 
   PROGRAMA    : CD0204-U05.p
   DATA        :  05/05/2015
   DESENVOLVIDO: Carlos Souza - DSC
   VERSAO      : 001
   OBJETIVO    : UPC no programa manut familias materiais. 
   =========================================================================== */
{include/i-prgvrs.i  cd0204-u05.P 2.06.00.000}

/* Parameter Definitions ****************************************************/
def input param p-ind-event                             as character.
def input param p-ind-object                            as character.
def input param p-wgh-object                            as handle.
def input param p-wgh-frame                             as widget-handle.
def input param p-cod-table                             as character.
def input param p-row-table                             as rowid.

def var h_campo                                         as widget-handle    no-undo.

/***********************************************************************************/

DEFINE NEW GLOBAL SHARED VAR wh-cd0204-it-codigo                             as widget-handle    no-undo.
DEFINE NEW GLOBAL SHARED VAR wh-cd0204-tx-it-codigo-new                      as widget-handle    no-undo.
DEFINE NEW GLOBAL SHARED VAR wh-cd0204-fl-it-codigo-new                      as widget-handle    no-undo.
DEFINE NEW GLOBAL SHARED VAR wh-cd0204-bt-cop                                as widget-handle    no-undo.
DEFINE NEW GLOBAL SHARED VAR wh-cd0204-COD-OBSOLETO                          as widget-handle    no-undo.
DEFINE NEW GLOBAL SHARED VAR wh-cd0204-UN                                    as widget-handle    no-undo.
DEFINE NEW GLOBAL SHARED VAR wh-cd0204-ge-codigo                             as widget-handle    no-undo.
DEFINE NEW GLOBAL SHARED VAR wh-cd0204-fm-codigo                             as widget-handle    no-undo.
DEFINE NEW GLOBAL SHARED VAR wh-cd0204-desc-item                             as widget-handle    no-undo.





RUN findWidget (INPUT "desc-item",INPUT "FILL-IN",INPUT p-wgh-frame, OUTPUT wh-cd0204-desc-item).   
IF VALID-HANDLE(wh-cd0204-desc-item) THEN DO:                                                
  wh-cd0204-desc-item:SENSITIVE = NO.                                                        
END.                                                                                  

RUN findWidget (INPUT "fm-codigo",INPUT "FILL-IN",INPUT p-wgh-frame, OUTPUT wh-cd0204-fm-codigo).     
IF VALID-HANDLE(wh-cd0204-fm-codigo) THEN DO:                                                         
  wh-cd0204-fm-codigo:SENSITIVE = NO.                                                                 
END.                                                                                                  


RUN findWidget (INPUT "un",INPUT "FILL-IN",INPUT p-wgh-frame, OUTPUT wh-cd0204-UN).
IF VALID-HANDLE(wh-cd0204-UN) THEN DO:                  
  wh-cd0204-UN:SENSITIVE = NO.                          
END.                                                    

RUN findWidget (INPUT "ge-codigo",INPUT "FILL-IN",INPUT p-wgh-frame, OUTPUT wh-cd0204-ge-codigo).       
IF VALID-HANDLE(wh-cd0204-ge-codigo) THEN DO:                                                    
  wh-cd0204-ge-codigo:SENSITIVE = NO.                                                            
END.                                                                                      

run pi-busca-widget (input  "bt-add",
                     input  p-wgh-frame,
                     output wh-cd0204-fl-it-codigo-new).

IF VALID-HANDLE(wh-cd0204-fl-it-codigo-new) THEN DO:

   wh-cd0204-fl-it-codigo-new:SENSITIVE = NO.
   HIDE wh-cd0204-fl-it-codigo-new.
END.

run pi-busca-widget (input  "bt-cop",
                     input  p-wgh-frame,
                     output wh-cd0204-bt-cop).

IF VALID-HANDLE(wh-cd0204-bt-cop) THEN DO:
  wh-cd0204-bt-cop:SENSITIVE = NO.
 HIDE wh-cd0204-bt-cop.
END.



run pi-busca-widget (input  "CB-COD-OBSOLETO",           
                     input  p-wgh-frame,        
                     output wh-cd0204-COD-OBSOLETO).  
                                                
/* IF VALID-HANDLE(wh-cd0204-COD-OBSOLETO) THEN DO:         */
/*   wh-cd0204-COD-OBSOLETO:SENSITIVE = NO.                 */
/*                                                          */
/* END.                                                     */


     
                                                         

 
 




/***********************************************************************************/

if p-ind-event            = "BEFORE-INITIALIZE" and
   p-ind-object           = "VIEWER"            and
   p-wgh-object:file-name = "invwr/v34in172.w"  and 
    valid-handle(wh-cd0204-fl-it-codigo-new) then do:
   
   

end.

if p-ind-event            = "AFTER-ENABLE"  then do:
 
 
 
end.





if p-ind-event  = "AFTER-DISABLE" then do:

END.




if p-ind-event  = "AFTER-CANCEL" then do:

END.





procedure pi-busca-widget:

    def input  param p-nome     as char.
    def input  param p-frame    as widget-handle.
    def output param p-object   as widget-handle.
    
    def var h-frame             as widget-handle.
    def var wh-objeto           as widget-handle.
    
    assign h-frame = p-frame:first-child no-error.

    do while valid-handle(h-frame):

        if h-frame:type <> 'field-group' then do:



          
            if h-frame:type = 'frame' then do:
                
                run pi-busca-widget(input  p-nome,
                                    input  h-frame,
                                    output wh-objeto).

                if wh-objeto <> ? then do:
                    
                    assign p-object = wh-objeto.
                    leave.

                end.

            end.

            if h-frame:type = "Literal" and 
               h-frame:screen-value matches "*" + p-nome + "*" then do:
                assign p-object = h-frame.
                leave.
            end.
            if h-frame:name = p-nome then do:

                assign p-object = h-frame.
                leave.
            end.
            assign h-frame = h-frame:next-sibling.
        end.
        else
            assign h-frame = h-frame:first-child.

           
          
          
           


    end.

end procedure.




return "Ok".


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

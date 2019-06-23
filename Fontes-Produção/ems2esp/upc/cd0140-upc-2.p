/****************************************************************************
** Programa: cd0140-upc.p                                                  **
** Autor...: Camila Steffen                                                **
** Data....: Set/2010                                                      **
** Objetivo: Adicionar o campo Ressupr (Xtivity), com as informa‡äes    **
** gravadas na tabela especifica ext-item-uni-estab0101 - Yamana              **
****************************************************************************/
{include/i-prgvrs.i cd0140-upc 2.00.00.001}

/* Parametros de Entrada da UPC */
define input parameter p-ind-event  as character     no-undo.
define input parameter p-ind-object as character     no-undo.
define input parameter p-wgh-object as handle        no-undo.
define input parameter p-wgh-frame  as widget-handle no-undo.
define input parameter p-cod-table  as character     no-undo.
define input parameter p-row-table  as rowid         no-undo.

define variable c-objeto     as character no-undo.
define variable h-objeto     as handle    no-undo.
define variable wgh-objeto   as handle    no-undo.
define variable wgh-objeto-2 as handle    no-undo.
def var wh-object as handle no-undo.
define variable c-formato     like ext-item-uni-estab01.tempo-ressuprimento no-undo.

define new global shared variable h-upc-cd0140             as handle no-undo.
define new global shared variable wgh-cd0140-it-codigo     as widget-handle no-undo.
define new global shared variable wgh-cd0140-cod-estabel   as widget-handle no-undo.
define new global shared variable wgh-cd0140-variacao-perm as widget-handle no-undo.
define new global shared variable wgh-cd0140-ressup-fabri  as widget-handle no-undo.
define new global shared variable wgh-cd0140-res-for-comp  as widget-handle no-undo.
define new global shared variable wgh-cd0140-res-cq-fabri  as widget-handle no-undo.
define new global shared variable wgh-cd0140-res-int-comp  as widget-handle no-undo.
define new global shared variable wgh-cd0140-res-cq-comp   as widget-handle no-undo.
define new global shared variable wgh-cd0140-loc-unica     as widget-handle no-undo.
define new global shared variable wh-label                 as widget-handle no-undo.
define new global shared variable wgh-cd0140-fPage1        as widget-handle no-undo.
define new global shared variable text-cd0140-Ressupr      as widget-handle no-undo.
define new global shared variable fi-cd0140-Ressupr        as widget-handle no-undo.

/*message "p-ind-event  " p-ind-event  skip
        "p-ind-object " p-ind-object skip
    view-as alert-box info buttons ok.*/

if p-ind-event  = "BEFORE-INITIALIZE":U and
   p-ind-object = "CONTAINER":U then do: 

    run upc\cd0140-upc.p persistent set h-upc-cd0140 (input "",            
                                                      input "",            
                                                      input p-wgh-object,  
                                                      input p-wgh-frame,   
                                                      input "",            
                                                      input p-row-table).
    run pi-armazena-handle.

    if valid-handle(wgh-cd0140-variacao-perm) and
       valid-handle(wgh-cd0140-ressup-fabri)  and   
       valid-handle(wgh-cd0140-res-for-comp)  and
       valid-handle(wgh-cd0140-res-cq-fabri)  and    
       valid-handle(wgh-cd0140-res-int-comp)  and
       valid-handle(wgh-cd0140-res-cq-comp)   then do:

        assign wgh-cd0140-loc-unica    :row = 3.54
            
               wgh-cd0140-variacao-perm:row = 4.50
               wgh-cd0140-variacao-perm:col = 70  
               wh-label                     = wgh-cd0140-variacao-perm:side-label-handle
               wh-label                :row = 4.50 
               wh-label                :col = 70

               wgh-cd0140-ressup-fabri :row = 5.50
               wgh-cd0140-ressup-fabri :col = 70
               wh-label                     = wgh-cd0140-ressup-fabri:side-label-handle
               wh-label                :row = 5.50 
               wh-label                :col = 70 

               wgh-cd0140-res-for-comp :row = 6.50
               wgh-cd0140-res-for-comp :col = 70 
               wh-label                     = wgh-cd0140-res-for-comp:side-label-handle
               wh-label                :row = 6.50 
               wh-label                :col = 70 

               wgh-cd0140-res-cq-fabri :row = 7.50
               wgh-cd0140-res-cq-fabri :col = 70 
               wh-label                     = wgh-cd0140-res-cq-fabri:side-label-handle
               wh-label                :row = 7.50 
               wh-label                :col = 70 

               wgh-cd0140-res-int-comp :row = 8.50
               wgh-cd0140-res-int-comp :col = 70 
               wh-label                     = wgh-cd0140-res-int-comp:side-label-handle
               wh-label                :row = 8.50  
               wh-label                :col = 70

               wgh-cd0140-res-cq-comp  :row = 9.50
               wgh-cd0140-res-cq-comp  :col = 70 
               wh-label                     = wgh-cd0140-res-cq-comp:side-label-handle
               wh-label                :row = 9.50  
               wh-label                :col = 70 .


    end.

    find first ext-item-uni-estab01 no-lock no-error.

    create text text-cd0140-Ressupr
    assign frame        = wgh-cd0140-fPage1
           format       = "x(18)"
           width        = 18
           screen-value = "Ressupr (Xtivity): "
           row          = 10.6
           col          = 58.5
           fgcolor      = 0
           visible      = yes.      
    
    create fill-in fi-cd0140-Ressupr
    assign frame              = wgh-cd0140-fPage1
           side-label-handle  = text-cd0140-Ressupr:handle
           format             = "x(06)"
           width              = 8
           height             = 0.88
           row                = 10.50
           col                = 70
           label              = "Ressupr (Xtivity): "
           visible            = yes
           sensitive          = NO  .

    MESSAGE p-wgh-frame:name "name"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    assign wgh-cd0140-fPage1:height = wgh-cd0140-fPage1:height + 1
           p-wgh-frame:height = p-wgh-frame:height + 3.

end. /**BEFORE-INITIALIZE**/


if p-ind-event  = "AFTER-INITIALIZE":U and
   p-ind-object = "CONTAINER":U then do: 

    /*assign p-wgh-frame:height = p-wgh-frame:height + 2.*/
           
    define new global shared variable a as widget-handle no-undo.

    assign a = p-wgh-frame:parent
           a:height = a:height + 5.

end.

if p-ind-event  = "BEFORE-CHANGE-PAGE":U and
   p-ind-object = "CONTAINER":U then do: 
    
    run pi-armazena-handle.

    if valid-handle(wgh-cd0140-cod-estabel) and
       valid-handle(wgh-cd0140-it-codigo) then do:

        find first ext-item-uni-estab01 where ext-item-uni-estab01.cod-estabel = wgh-cd0140-cod-estabel:screen-value and
                                             ext-item-uni-estab01.it-codigo   = wgh-cd0140-it-codigo:screen-value no-lock no-error.
        if avail ext-item-uni-estab01 then do:
            assign fi-cd0140-Ressupr:screen-value = string(ext-item-uni-estab01.tempo-ressuprimento,">>>>>9").
        end.
    end.

end. /**BEFORE-CHANGE-PAGE"**/


if p-ind-event  = "BEFORE-CONTROL-TOOL-BAR":U and
   p-ind-object = "CONTAINER":U then do:

    run pi-armazena-handle.

    find first ext-item-uni-estab01 where ext-item-uni-estab01.cod-estabel = wgh-cd0140-cod-estabel:screen-value and
                                         ext-item-uni-estab01.it-codigo   = wgh-cd0140-it-codigo:screen-value no-lock no-error.
    if avail ext-item-uni-estab01 then do:

        assign fi-cd0140-Ressupr:screen-value = string(ext-item-uni-estab01.tempo-ressuprimento,">>>>>9").
    end.
    else 
        assign fi-cd0140-Ressupr:screen-value = "0".

end. /**BEFORE-CONTROL-TOOL-BAR**/
procedure pi-armazena-handle:
    
    assign wgh-objeto = p-wgh-frame:first-child.
    
    do while valid-handle(wgh-objeto):

        case wgh-objeto:type:
            when "fill-in":U then do:
                if wgh-objeto:name = "it-codigo" then
                    assign wgh-cd0140-it-codigo = wgh-objeto:handle.
                if wgh-objeto:name = "cod-estabel" then
                    assign wgh-cd0140-cod-estabel = wgh-objeto:handle.
            end.
            when "frame" then do:
                if wgh-objeto:name = "fPage1" then
                    assign wgh-cd0140-fPage1 = wgh-objeto:handle
                           /*wgh-cd0140-fPage1:height = wgh-cd0140-fPage1:height + 0.1*/.
            end.
        end case.

        if valid-handle(wgh-objeto) then
            if wgh-objeto:type = "field-group" then 
                assign wgh-objeto = wgh-objeto:first-child.
            else 
                assign wgh-objeto = wgh-objeto:next-sibling.
        
    end.

    assign wgh-objeto-2 = wgh-cd0140-fPage1:first-child.

    do while valid-handle(wgh-objeto-2):

        case wgh-objeto-2:type:
            when "fill-in":U then do:
                if wgh-objeto-2:name = "variacao-perm" then
                    assign wgh-cd0140-variacao-perm = wgh-objeto-2:handle.
                if wgh-objeto-2:name = "ressup-fabri" then
                    assign wgh-cd0140-ressup-fabri = wgh-objeto-2:handle.
                if wgh-objeto-2:name = "res-for-comp" then
                    assign wgh-cd0140-res-for-comp = wgh-objeto-2:handle.
                if wgh-objeto-2:name = "res-cq-fabri" then
                    assign wgh-cd0140-res-cq-fabri = wgh-objeto-2:handle.
                if wgh-objeto-2:name = "res-int-comp" then
                    assign wgh-cd0140-res-int-comp = wgh-objeto-2:handle.
                if wgh-objeto-2:name = "res-cq-comp" then
                    assign wgh-cd0140-res-cq-comp = wgh-objeto-2:handle.
            end.
            when "toggle-box" then do:
                if wgh-objeto-2:name = "loc-unica" then
                    assign wgh-cd0140-loc-unica = wgh-objeto-2:handle.
            end.  
            when "rectangle" then do:                                
                if wgh-objeto-2:name = "RECT-6" then               
                    assign wgh-objeto-2:height = wgh-objeto-2:height + 0.2.
            end.                                                      
        end case.
        
        if valid-handle(wgh-objeto-2) then
            if wgh-objeto-2:type = "field-group" then 
                assign wgh-objeto-2 = wgh-objeto-2:first-child.
            else 
                assign wgh-objeto-2 = wgh-objeto-2:next-sibling.
    end.  
    
end procedure.


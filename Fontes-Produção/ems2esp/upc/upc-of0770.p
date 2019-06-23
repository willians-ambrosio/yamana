/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i UPC-OF0770 2.00.00.001}  /*** 010001 ***/
/**************************************************************************
**    Programa....: upc-of0770.p
**    Objetivo....: Demonstrativo de PIS/COFINS (Customizado)
**    Data........: Maio/2009
**    Responsavel.: Diogo Cezar Amaral
**************************************************************************/
define input  parameter p-ind-event  as character     no-undo.
define input  parameter p-ind-object as character     no-undo.
define input  parameter p-wgh-object as handle        no-undo.
define input  parameter p-wgh-frame  as widget-handle no-undo.
define input  parameter p-cod-table  as character     no-undo.
define input  parameter p-row-table  as rowid         no-undo.

define variable h-objeto             as widget-handle no-undo.
define variable c-objeto             as character     no-undo.

assign c-objeto = entry(num-entries(p-wgh-object:private-data, "~/"),p-wgh-object:private-data, "~/").

{include/i_fclpreproc.i}
define variable h-frame         as handle        no-undo.
define variable h-bt-ok-old     as handle        no-undo.
define variable h-bt-ok         as handle        no-undo.
define variable h-destiny-old   as handle        no-undo.
define variable h-destiny       as handle        no-undo.

if p-ind-event  = "before-initialize":u and 
   p-ind-object = "container":u     then do :

    run getFieldHandle (input p-wgh-frame:first-child, 
                        input  "btOK", 
                        output h-bt-ok-old).

    assign h-bt-ok-old:sensitive = yes.

    create button h-bt-ok
    assign frame     = h-bt-ok-old:frame
           width     = h-bt-ok-old:width
           height    = h-bt-ok-old:height
           row       = h-bt-ok-old:row
           col       = h-bt-ok-old:col
           name      = "bt-executar-upc":U
           label     = "Executar"
           sensitive = yes
           hidden    = no
        triggers:
            on choose persistent run "upc/upc-of0770a.p" (input p-wgh-frame, input 1).
        end triggers.   

    run getFieldHandle (input p-wgh-frame:first-child, 
                        input  "fPage6", 
                        output h-frame).

    run getFieldHandle (input h-frame, 
                        input  "rsDestiny", 
                        output h-destiny-old).

    create radio-set h-destiny
    assign frame         = h-destiny-old:frame
           width         = h-destiny-old:width
           height        = h-destiny-old:height
           row           = h-destiny-old:row
           col           = h-destiny-old:col
           name          = "h-destiny-upc"
           radio-buttons = h-destiny-old:radio-buttons
           sensitive     = yes
           horizontal    = yes
        triggers:
            on value-changed persistent run "upc/upc-of0770a.p" (input p-wgh-frame, input 2).
        end triggers.
        
    h-destiny:add-last("Excel", 4).

    &if "{&aplica_facelift}" = "yes" &then
		 {include/i_fcldin.i h-destiny}
    &endif
end.

procedure getFieldHandle private :

    define input  parameter pHandleFrame as handle     no-undo.
    define input  parameter pCharField   as character  no-undo.
    define output parameter pHandleField as handle     no-undo.

    define variable hField as handle     no-undo.
    
    assign hField = pHandleFrame:first-child.
    
    do while hField <> ?:
        if hField:type <> "field-group" then do :
            IF hField:NAME = pCharField then do :
               assign pHandleField = hField.
               leave.
            end.
            assign hField = hField:next-sibling.
        end. 
        else do:
            assign hField = hField:first-child.
        end.
    end.

    return "ok":u.

end procedure.

return "ok":u.

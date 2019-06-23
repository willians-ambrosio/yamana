/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i of0770-upc-trigger 2.00.00.001}  /*** 010001 ***/
/**************************************************************************
**    Programa....: of0770-upc-trigger.p
**    Objetivo....: Demonstrativo de PIS/COFINS (Customizado)
**    Data........: Maio/2009
**    Responsavel.: Diogo Cezar Amaral
**************************************************************************/
define input  parameter p-wgh-frame  as widget-handle no-undo.
define input  parameter p-ind-event  as integer     no-undo.

define variable h-frame        as handle     no-undo.
define variable h-periodo-ini  as handle     no-undo.
define variable h-periodo-fim  as handle     no-undo.
define variable h-cod-estabel  as handle     no-undo.
define variable h-rs-modo      as handle     no-undo.
define variable h-rs-relatorio as handle     no-undo.
define variable h-destiny      as handle     no-undo.

case p-ind-event :
    when 1 then run pi-execute-button.
    when 2 then run pi-execute-radio-set.
end case.

procedure pi-execute-button private :

    run getFieldHandle (input p-wgh-frame:first-child, input  "fPage6", output h-frame).
    run getFieldHandle (input h-frame,input  "h-destiny-upc", output h-destiny).

    if integer(h-destiny:screen-value) = 4 then do :
        run getFieldHandle (input p-wgh-frame:first-child, input  "fPage2", output h-frame).
        run getFieldHandle (input h-frame,input  "periodo-ini", output h-periodo-ini).
        run getFieldHandle (input h-frame,input  "periodo-fim", output h-periodo-fim).
    
        run getFieldHandle (input p-wgh-frame:first-child, input  "fPage4", output h-frame).
        run getFieldHandle (input h-frame,input  "c-estabel", output h-cod-estabel).

        run upc/upc-of0770rp.p (input date(h-periodo-ini:screen-value),
                                input date(h-periodo-fim:screen-value),
                                input h-cod-estabel:screen-value     ).
    end.
    else do :
        run getFieldHandle (input p-wgh-frame:first-child, input "btOK", output h-frame).
        if valid-handle(h-frame) THEN
            apply "choose":u TO h-frame.
    end.

    return "ok":u.

end procedure.

procedure pi-execute-radio-set private :
    
    run getFieldHandle (input p-wgh-frame:first-child, input  "fPage6", output h-frame).
    run getFieldHandle (input h-frame,input  "h-destiny-upc", output h-destiny).

    run getFieldHandle (input p-wgh-frame:first-child, input  "fPage4", output h-frame).
    run getFieldHandle (input h-frame,input  "rs-modo"      , output h-rs-modo).
    run getFieldHandle (input h-frame,input  "rs-relatorio ", output h-rs-relatorio).

    if integer(h-destiny:screen-value) = 4 then
         assign h-rs-modo     :sensitive    = no
                h-rs-modo     :screen-value = "3"
                h-rs-relatorio:sensitive    = no
                h-rs-relatorio:screen-value = "1".
    else
        assign h-rs-modo     :sensitive    = yes
               h-rs-relatorio:sensitive    = yes.

    return "ok":u.

end procedure.

return "ok":u.

procedure getFieldHandle private :

    define input  parameter pHandleFrame as handle     no-undo.
    define input  parameter pCharField   as character  no-undo.
    define output parameter pHandleField as handle     no-undo.

    define variable hField as handle     no-undo.
    
    assign hField = pHandleFrame:first-child.
    
    do while hField <> ?:
        if hField:type <> "field-group" then do :
            IF hField:name = pCharField then do :
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

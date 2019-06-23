/******************************************************************
**
** Programa: UPC-RE0101-U01
**
** Objetivo: Programa chamador de UPC
**
**    Autor: Renato
**
**     Data: Dezembro/2018
**
**   Versao: 2.12.00.000 - Desenvolvimento Inicial
**
******************************************************************/
{include/i-prgvrs.i UPC-RE0101-U01 2.12.00.000}

define input parameter p-ind-event   as char          no-undo.
define input parameter p-ind-object  as char          no-undo.
define input parameter p-wgh-object  as handle        no-undo.
define input parameter p-wgh-frame   as widget-handle no-undo.
define input parameter p-cod-table   as char          no-undo.
define input parameter p-row-table   as rowid         no-undo.

{tools/fc-handle-obj.i}

def new global shared var wh-re0101-u01-seq-item-um   as widget-handle no-undo.
def new global shared var wh-re0101-u01-usuario-aprov as widget-handle no-undo.
def new global shared var wh-re0101-u01-p-wgh-object  as widget-handle no-undo.

def var c-handle-obj as char no-undo.

/*     MESSAGE "p-ind-event..:" p-ind-event            SKIP */
/*         "p-ind-object.:" p-ind-object           SKIP     */
/*         "p-cod-table..:" STRING(p-cod-table)    SKIP     */
/*         "p-row-table..:" STRING(p-row-table)    SKIP     */
/*         "p-wgh-object.:" p-wgh-object:FILE-NAME SKIP     */
/*         "p-wgh-frame..:" STRING(p-wgh-frame)    SKIP     */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                   */

if  p-ind-event  = "initialize" 
and p-ind-object = "container" then do:

    assign wh-re0101-u01-p-wgh-object = p-wgh-object.

end.

if  p-ind-event  = "BEFORE-DISPLAY" 
and p-ind-object = "viewer"     
and p-wgh-object:file-name MATCHES "*v07in292*" then do:

    IF NOT(VALID-HANDLE(wh-re0101-u01-seq-item-um)) THEN DO:

        ASSIGN c-handle-obj = fc-handle-obj("seq-item-um",p-wgh-frame)
               wh-re0101-u01-seq-item-um = WIDGET-HANDLE(ENTRY(1,c-handle-obj)).

        if  valid-handle(wh-re0101-u01-seq-item-um) then do:

            create toggle-box wh-re0101-u01-usuario-aprov
            assign frame              = wh-re0101-u01-seq-item-um:frame
                   row                = wh-re0101-u01-seq-item-um:row + 1
                   col                = wh-re0101-u01-seq-item-um:col
                   label              = "Aprova Item Contagem Divergente"
                   width              = 28
                   visible            = yes
                   sensitive          = no /*
                   checked            = no */.

            if  valid-handle(wh-re0101-u01-p-wgh-object) then do:
                run select-page in wh-re0101-u01-p-wgh-object (input 4).
                run select-page in wh-re0101-u01-p-wgh-object (input 1).
            end.

        end.

    END.

end.

if  p-ind-event  = "add" 
and p-ind-object = "viewer" 
and p-wgh-object:file-name MATCHES "*v07in292*" then do:

    if valid-handle(wh-re0101-u01-usuario-aprov) THEN
        assign wh-re0101-u01-seq-item-um:sensitive = yes
               wh-re0101-u01-seq-item-um:checked   = no.
end.

if  p-ind-event  = "cancel" 
and p-ind-object = "viewer"  
and p-wgh-object:file-name MATCHES "*v07in292*" then do:

    IF VALID-HANDLE(wh-re0101-u01-usuario-aprov) THEN
        assign wh-re0101-u01-usuario-aprov:checked = no.

end.

if  p-ind-event  = "after-enable" 
and p-ind-object = "viewer"
and p-wgh-object:file-name MATCHES "*v07in292*" then do:

    IF VALID-HANDLE(wh-re0101-u01-usuario-aprov) THEN
        assign wh-re0101-u01-usuario-aprov:sensitive = yes.

end.

if  p-ind-event  = "after-disable" 
and p-ind-object = "viewer"
and p-wgh-object:file-name MATCHES "*v07in292*" then do:

    if valid-handle(wh-re0101-u01-usuario-aprov) THEN
        assign wh-re0101-u01-usuario-aprov:sensitive = no.

end.

if  p-ind-event  = "assign"
and p-ind-object = "viewer"
and p-wgh-object:file-name MATCHES "*v07in292*" then do:

    if valid-handle(wh-re0101-u01-usuario-aprov) THEN DO:

        find first param-re exclusive-lock
             where rowid(param-re) = p-row-table no-error.
        IF avail param-re then do:
             ASSIGN param-re.log-livre-2 = wh-re0101-u01-usuario-aprov:CHECKED.
        end.

    END.

end.

if  p-ind-event  = "after-end-update"
and p-ind-object = "viewer"
and p-wgh-object:file-name MATCHES "*v07in292*" then do:

    if valid-handle(wh-re0101-u01-usuario-aprov) THEN
        assign wh-re0101-u01-seq-item-um:sensitive = no.

end.

if  p-ind-event  = "display"
and p-ind-object = "viewer"
and p-wgh-object:file-name MATCHES "*v07in292*" then do:

    if valid-handle(wh-re0101-u01-usuario-aprov) then do:

        assign wh-re0101-u01-usuario-aprov:CHECKED = NO.

        find first param-re no-lock where
             rowid(param-re) = p-row-table no-error.
        if  avail param-re then do:
               assign wh-re0101-u01-usuario-aprov:checked = param-re.log-livre-2.
        end.

    end.

END.

/* Fim do Programa */

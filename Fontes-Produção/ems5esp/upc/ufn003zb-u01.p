/**************************************************************************
 ** Programa: upc/ufn003zb-u01.p
 ** Autor   : Hilton Borba
 ** Data    : 13/11/2014
 **************************************************************************/
def input param p_ind_event  as char          no-undo.
def input param p_ind_object as char          no-undo.
def input param p_wgh_object as handle        no-undo.
def input param p_wgh_frame  as widget-handle no-undo.
def input param p_cod_table  as char          no-undo.
def input param p_rec_table  as recid         no-undo.

def var wh-ufn003zb-bt_incluir  as widget-handle no-undo.
def var wh-ufn003zb-bt_alterar  as widget-handle no-undo.
def var wh-ufn003zb-bt_eliminar as widget-handle no-undo.

{tools/fc-handle-obj.i}

def var c-handle-obj as char no-undo.
def var i-aux        as int  no-undo init 1.

run pi-handle.

if  p_ind_event = "ENABLE" and
    p_ind_object = "viewer" then do:
    repeat while program-name(i-aux) <> ?:
        if  program-name(i-aux) = "pi_bt_zoo20_cta_fornec prgfin/apb/apb710zu.p" then
            assign wh-ufn003zb-bt_incluir :sensitive = no
                   wh-ufn003zb-bt_alterar :sensitive = no
                   wh-ufn003zb-bt_eliminar:sensitive = no
                   wh-ufn003zb-bt_incluir :hidden    = yes
                   wh-ufn003zb-bt_alterar :hidden    = yes
                   wh-ufn003zb-bt_eliminar:hidden    = yes.
    
        assign i-aux = i-aux + 1.
    end.
end.

procedure pi-handle :
    assign c-handle-obj = fc-handle-obj("bt_incluir,bt_alterar,bt_eliminar",p_wgh_frame)
           wh-ufn003zb-bt_incluir  = widget-handle(entry(1,c-handle-obj))
           wh-ufn003zb-bt_alterar  = widget-handle(entry(2,c-handle-obj))
           wh-ufn003zb-bt_eliminar = widget-handle(entry(3,c-handle-obj)) no-error.
end procedure.

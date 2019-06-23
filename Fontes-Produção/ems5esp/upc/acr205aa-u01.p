/**************************************************************************
 ** Programa: upc/acr205aa-u01.p
 ** Autor   : Hilton Borba
 ** Data    : 13/11/2014
 **************************************************************************/
def input param p_ind_event  as char          no-undo.
def input param p_ind_object as char          no-undo.
def input param p_wgh_object as handle        no-undo.
def input param p_wgh_frame  as widget-handle no-undo.
def input param p_cod_table  as char          no-undo.
def input param p_rec_table  as recid         no-undo.

def new global shared var gr-emitente as rowid no-undo.

def var wh-acr205aa-bt_exi            as widget-handle no-undo.
def var wh-acr205aa-bt_consulta_ems2  as widget-handle no-undo.
def var wh-acr205aa-v_cod_clien_infor as widget-handle no-undo.

{tools/fc-handle-obj.i}

def var c-handle-obj as char no-undo.

run pi-handle.

if  p_ind_event  = "CHOOSE" and
    p_ind_object = "bt_consulta_ems2" then do:
    find first emitente no-lock
         where emitente.cod-emitente = wh-acr205aa-v_cod_clien_infor:input-value no-error.

    assign gr-emitente = rowid(emitente).

    run crp/cr0706.w.
end.

procedure pi-handle :
    assign c-handle-obj = fc-handle-obj("v_cod_clien_infor,bt_consulta_ems2",p_wgh_frame)
           wh-acr205aa-v_cod_clien_infor = widget-handle(entry(1,c-handle-obj))
           wh-acr205aa-bt_consulta_ems2  = widget-handle(entry(2,c-handle-obj)) no-error.

    if  not valid-handle(wh-acr205aa-bt_consulta_ems2) then do:
        assign c-handle-obj = fc-handle-obj("bt_exi",p_wgh_frame)
               wh-acr205aa-bt_exi = widget-handle(entry(1,c-handle-obj)) no-error.

        if  valid-handle(wh-acr205aa-bt_exi) then do:
            create button wh-acr205aa-bt_consulta_ems2
            assign frame     = wh-acr205aa-bt_exi:frame
                   name      = "bt_consulta_ems2"
                   width     = 13
                   height    = 1
                   col       = wh-acr205aa-bt_exi:col - wh-acr205aa-bt_consulta_ems2:width
                   row       = wh-acr205aa-bt_exi:row
                   label     = "Consulta EMS2"
                   visible   = yes
                   sensitive = yes.

            wh-acr205aa-bt_consulta_ems2:move-to-top().
            wh-acr205aa-bt_consulta_ems2:move-before-tab-item(wh-acr205aa-bt_exi:handle) no-error.
            wh-acr205aa-bt_consulta_ems2:sensitive = yes.

            on 'CHOOSE':U of wh-acr205aa-bt_consulta_ems2 persistent
                run upc/acr205aa-u01.p(input "CHOOSE",
                                       input "bt_consulta_ems2",
                                       input wh-acr205aa-bt_consulta_ems2,
                                       input p_wgh_frame,
                                       input "",
                                       input p_rec_table). 
        end.
    end.
end procedure.

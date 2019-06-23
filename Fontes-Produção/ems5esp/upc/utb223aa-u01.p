/**************************************************************************
 ** Programa: upc/utb223aa-u01.p
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

def var wh-utb223aa-bt_exi             as widget-handle no-undo.
def var wh-utb223aa-bt_consulta_ems2   as widget-handle no-undo.
def var wh-utb223aa-v_cod_fornec_infor as widget-handle no-undo.

{tools/fc-handle-obj.i}

def var c-handle-obj as char no-undo.

run pi-handle.

if  p_ind_event  = "CHOOSE" and
    p_ind_object = "bt_consulta_ems2" then do:
    find first emitente no-lock
         where emitente.cod-emitente = wh-utb223aa-v_cod_fornec_infor:input-value no-error.

    assign gr-emitente = rowid(emitente).

    run app/ap0804.w.
end.

procedure pi-handle :
    assign c-handle-obj = fc-handle-obj("v_cod_fornec_infor,bt_consulta_ems2",p_wgh_frame)
           wh-utb223aa-v_cod_fornec_infor = widget-handle(entry(1,c-handle-obj))
           wh-utb223aa-bt_consulta_ems2   = widget-handle(entry(2,c-handle-obj)) no-error.

    if  not valid-handle(wh-utb223aa-bt_consulta_ems2) then do:
        assign c-handle-obj = fc-handle-obj("bt_exi",p_wgh_frame)
               wh-utb223aa-bt_exi = widget-handle(entry(1,c-handle-obj)) no-error.

        if  valid-handle(wh-utb223aa-bt_exi) then do:
            create button wh-utb223aa-bt_consulta_ems2
            assign frame     = wh-utb223aa-bt_exi:frame
                   name      = "bt_consulta_ems2"
                   width     = 13
                   height    = 1
                   col       = wh-utb223aa-bt_exi:col - wh-utb223aa-bt_consulta_ems2:width
                   row       = wh-utb223aa-bt_exi:row
                   label     = "Consulta EMS2"
                   visible   = yes
                   sensitive = yes.

            wh-utb223aa-bt_consulta_ems2:move-to-top().
            wh-utb223aa-bt_consulta_ems2:move-before-tab-item(wh-utb223aa-bt_exi:handle) no-error.
            wh-utb223aa-bt_consulta_ems2:sensitive = yes.

            on 'CHOOSE':U of wh-utb223aa-bt_consulta_ems2 persistent
                run upc/utb223aa-u01.p(input "CHOOSE",
                                       input "bt_consulta_ems2",
                                       input wh-utb223aa-bt_consulta_ems2,
                                       input p_wgh_frame,
                                       input "",
                                       input p_rec_table). 
        end.
    end.
end procedure.

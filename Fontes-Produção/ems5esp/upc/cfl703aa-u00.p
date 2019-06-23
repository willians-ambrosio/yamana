/***********************************************************************
** PROGRAMA.....: upc/cfl703aa-u00.p
** DESCRIÄ«O. ..: Integraá∆o Fluxo CX x EMS 2
************************************************************************/
def input param p-ind-event  as char          no-undo.
def input param p-ind-object as char          no-undo.
def input param p-wgh-object as handle        no-undo.
def input param p-wgh-frame  as widget-handle no-undo.
def input param p-cod-table  as char          no-undo.
def input param p-rec-table  as recid         no-undo. 

def var c-handle-obj                 as char          no-undo.
def var wh-v_cod_histor_padr         as widget-handle no-undo.
def var wh-v_log_consid_ord_compra   as widget-handle no-undo.
def var wh-v_ind_tip_fluxo_cx_integr as widget-handle no-undo.
def var wh-bt_print                  as widget-handle no-undo.
def var wh-bt_print-falso            as widget-handle no-undo.

{tools/fc-handle-obj.i}
{tools/fc-falso.i}

run pi-inicializa-campos.

if  p-ind-event  = "CHOOSE" and
    p-ind-object = "bt_print-falso" then do:
    find first histor_padr no-lock
         where histor_padr.cod_histor_padr = wh-v_cod_histor_padr:input-value
           and histor_padr.des_text_histor_padr matches "*Ordem de Compra: #origem# *" no-error.
    if  not avail histor_padr then do:
        run utp/ut-msgs.p(input "show", input 17006,
                          input 'Hist¢rico Padr∆o~~Hist¢rico Padr∆o n∆o valido para Ordem de Compra, Hist¢rico precisa possuir "Ordem de Compra: #origem# "').
        return.
    end.

    if  wh-v_log_consid_ord_compra:input-value and
        wh-v_ind_tip_fluxo_cx_integr:input-value <> "Detalhado" then do:
        run utp/ut-msgs.p(input "show", input 17006,
                          input "Ordem de Compra~~Para Ordem de Compra Ç necess†rio o tipo de fluxo ser Detalhado").
        return.
    end.

    apply "CHOOSE":U to wh-bt_print.
end.

procedure pi-inicializa-campos :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
    assign c-handle-obj = fc-handle-obj("v_cod_histor_padr,v_log_consid_ord_compra,v_ind_tip_fluxo_cx_integr,bt_print,falso-bt_print",p-wgh-frame)
           wh-v_cod_histor_padr         = widget-handle(entry(1,c-handle-obj))
           wh-v_log_consid_ord_compra   = widget-handle(entry(2,c-handle-obj))
           wh-v_ind_tip_fluxo_cx_integr = widget-handle(entry(3,c-handle-obj))
           wh-bt_print                  = widget-handle(entry(4,c-handle-obj))
           wh-bt_print-falso            = widget-handle(entry(5,c-handle-obj)) no-error.

    if  not valid-handle(wh-bt_print-falso) then do:
        assign wh-bt_print-falso = fc-falso(wh-bt_print,wh-bt_print:frame,"").

        wh-bt_print-falso:move-to-top().
        wh-bt_print-falso:move-after(wh-bt_print).
        wh-bt_print-falso:sensitive = yes.

        on 'CHOOSE':U of wh-bt_print-falso persistent
            run upc/cfl703aa-u00.p(input "CHOOSE",
                                   input "bt_print-falso",
                                   input wh-bt_print-falso,
                                   input p-wgh-frame,
                                   input "",
                                   input p-rec-table). 
    end.
end procedure. /* PROCEDURE pi-inicializa-campos */

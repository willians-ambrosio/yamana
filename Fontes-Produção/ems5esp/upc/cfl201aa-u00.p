/***********************************************************************
** PROGRAMA.....: upc/cfl201aa-u00.p
** DESCRIÄ«O. ..: Integraá∆o Fluxo CX x EMS 2
************************************************************************/
def input param p-ind-event  as char          no-undo.
def input param p-ind-object as char          no-undo.
def input param p-wgh-object as handle        no-undo.
def input param p-wgh-frame  as widget-handle no-undo.
def input param p-cod-table  as char          no-undo.
def input param p-rec-table  as recid         no-undo. 

def new global shared var gr-ordem-compra as rowid no-undo.

def var c-handle-obj              as char          no-undo.
def var wh-num_fluxo_cx           as widget-handle no-undo.
def var wh-dat_movto_fluxo_cx     as widget-handle no-undo.
def var wh-num_seq_movto_fluxo_cx as widget-handle no-undo.
def var wh-cod_modul_dtsul        as widget-handle no-undo.
def var wh-bt_origem_movto_fluxo  as widget-handle no-undo.
def var wh-bt_detalhe             as widget-handle no-undo.
def var i-posicao-ordem           as int           no-undo.
def var c-texto-ordem             as char          no-undo.

{tools/fc-handle-obj.i}

find first param-global no-lock no-error.

run pi-inicializa-campos.

/*
Ordem de Compra: 
Ordem de Compra: 4372800 - 183 /    Met Life

for each movto_fluxo_cx /* no-lock */
   where movto_fluxo_cx.num_fluxo_cx           = 0
     and movto_fluxo_cx.dat_movto_fluxo_cx     = 05/15/2012
     and movto_fluxo_cx.num_seq_movto_fluxo_cx = 1
     and movto_fluxo_cx.cod_modul_dtsul        = "CCP":
    update movto_fluxo_cx.des_histor_movto_fluxo_cx.
end.
*/
if  p-ind-event  = "CHOOSE" and
    p-ind-object = "bt_detalhe" then do:
    if  wh-cod_modul_dtsul:input-value = "CCP" then do:
        find first movto_fluxo_cx no-lock
             where movto_fluxo_cx.num_fluxo_cx           = wh-num_fluxo_cx:input-value
               and movto_fluxo_cx.dat_movto_fluxo_cx     = wh-dat_movto_fluxo_cx:input-value
               and movto_fluxo_cx.num_seq_movto_fluxo_cx = wh-num_seq_movto_fluxo_cx:input-value
               and movto_fluxo_cx.cod_modul_dtsul        = wh-cod_modul_dtsul:input-value no-error.
        if  avail movto_fluxo_cx then do:
            find first param_integr_ems no-lock
                 where param_integr_ems.ind_param_integr_ems         = "ContabilizaÁıes 2.00"
                   and param_integr_ems.des_contdo_param_integr_ems <> "" no-error.
            if  avail param_integr_ems then do:
                find first trad_org_ext no-lock use-index trdrgxt_id
                     where trad_org_ext.cod_matriz_trad_org_ext = param_integr_ems.des_contdo_param_integr_ems
                       and trad_org_ext.cod_tip_unid_organ      = "998" /* Empresa */
                       and trad_org_ext.cod_unid_organ          = movto_fluxo_cx.cod_empresa /* EMS5 */
                       and trad_org_ext.cod_unid_organ_ext      = string(param-global.empresa-prin) /* EMS2 */ no-error.
                if  not avail trad_org_ext then do:
                    run utp/ut-msgs.p(input "show", input 17006,
                                      input "Empresa~~Empresa EMS2 n∆o conectada").
                    return.
                end.
            end.

            assign i-posicao-ordem = lookup("Ordem de Compra:",movto_fluxo_cx.des_histor_movto_fluxo_cx," ")
                   c-texto-ordem   = replace(movto_fluxo_cx.des_histor_movto_fluxo_cx,"Ordem de Compra: ","").

            find first ordem-compra no-lock
                 where ordem-compra.numero-ordem = int(entry(i-posicao-ordem,c-texto-ordem," ")) no-error.
            if  avail ordem-compra then
                assign gr-ordem-compra = rowid(ordem-compra).
            else do:
                run utp/ut-msgs.p(input "show", input 17006, input "Ordem de Compra n∆o Localizada").
                return.
            end.

            run ccp/cc0505.w.
        end.
        else do:
            run utp/ut-msgs.p(input "show", input 17006, input "Fluxo de Caixa n∆o Localizado").
            return.
        end.
    end.
    else do:
        run utp/ut-msgs.p(input "show", input 17006, input "Fluxo n∆o Ç de Ordem de Compra").
        return.
    end.
end.

procedure pi-inicializa-campos :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
    assign c-handle-obj = fc-handle-obj("num_fluxo_cx,dat_movto_fluxo_cx,num_seq_movto_fluxo_cx,cod_modul_dtsul,bt_origem_movto_fluxo,wh-bt_detalhe",p-wgh-frame)
           wh-num_fluxo_cx           = widget-handle(entry(1,c-handle-obj))
           wh-dat_movto_fluxo_cx     = widget-handle(entry(2,c-handle-obj))
           wh-num_seq_movto_fluxo_cx = widget-handle(entry(3,c-handle-obj))
           wh-cod_modul_dtsul        = widget-handle(entry(4,c-handle-obj))
           wh-bt_origem_movto_fluxo  = widget-handle(entry(5,c-handle-obj))
           wh-bt_detalhe             = widget-handle(entry(6,c-handle-obj)) no-error.

    if  not valid-handle(wh-bt_detalhe) then do:
        create button wh-bt_detalhe
        assign frame     = wh-bt_origem_movto_fluxo:frame
               name      = "wh-bt_detalhe"
               col       = wh-bt_origem_movto_fluxo:col + wh-bt_origem_movto_fluxo:width
               row       = wh-bt_origem_movto_fluxo:row
               width     = 10
               height    = 1
               label     = "Detalhe"
               visible   = yes
               sensitive = yes.

        wh-bt_detalhe:move-to-top().
        wh-bt_detalhe:move-after(wh-bt_origem_movto_fluxo).
        wh-bt_detalhe:sensitive = yes.

        on 'CHOOSE':U of wh-bt_detalhe persistent
            run upc/cfl201aa-u00.p(input "CHOOSE",
                                   input "bt_detalhe",
                                   input wh-bt_detalhe,
                                   input p-wgh-frame,
                                   input "",
                                   input p-rec-table). 
    end.
end procedure. /* PROCEDURE pi-inicializa-campos */

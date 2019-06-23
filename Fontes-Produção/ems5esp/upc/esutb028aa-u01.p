/******************************************************************
** Programa: esutb028aa-u01
** Objetivo: Upc para o Programa prgint/utb/utb028aa.py
**     Data: Fevereiro/2008
******************************************************************/
def input param p-ind-event  as char          no-undo.
def input param p-ind-object as char          no-undo.
def input param p-wgh-object as handle        no-undo.
def input param p-wgh-frame  as widget-handle no-undo.
def input param p-cod-table  as char          no-undo.
def input param p-rec-table  as recid         no-undo. 

{tools/fc-handle-obj.i}

def new global shared var wh-utb028aa-bt_mod1 as widget-handle no-undo.
def new global shared var wh-utb028aa-bt_era1 as widget-handle no-undo.
def new global shared var wh-utb028aa-bt_mov1 as widget-handle no-undo.

def new global shared var v_cod_usuar_corren as char format "x(12)":U
    label "Usu rio Corrente" column-label "Usu rio Corrente" no-undo.

def var c-handle-obj as char no-undo.

if  p-ind-event = "INITIALIZE" and
    p-ind-object = "VIEWER" then do:
    /*----- BUSCA HANDLE DE OBJ -----*/
    assign c-handle-obj        = fc-handle-obj("bt_mod1,bt_era1,bt_mov1",p-wgh-frame)
           wh-utb028aa-bt_mod1 = widget-handle(entry(1,c-handle-obj))
           wh-utb028aa-bt_era1 = widget-handle(entry(2,c-handle-obj))
           wh-utb028aa-bt_mov1 = widget-handle(entry(3,c-handle-obj)) no-error.
end.

if  p-ind-event = "DISPLAY" and
    p-ind-object = "VIEWER" then do:
    find first mapa_distrib_ccusto no-lock
         where recid(mapa_distrib_ccusto) = p-rec-table no-error.
    if  avail mapa_distrib_ccusto and
        mapa_distrib_ccusto.ind_tip_mapa_distrib_ccusto = 'lista' and
        not can-find(first usuar_grp_usuar no-lock
                     where usuar_grp_usuar.cod_usuario   = v_cod_usuar_corren
                       and(usuar_grp_usuar.cod_grp_usuar = 'OR1'
                        or usuar_grp_usuar.cod_grp_usuar = 'CF0'
                        or usuar_grp_usuar.cod_grp_usuar = 'SUP')) then
        assign wh-utb028aa-bt_mod1:sensitive = no
               wh-utb028aa-bt_era1:sensitive = no
               wh-utb028aa-bt_mov1:sensitive = no.
    else
        assign wh-utb028aa-bt_mod1:sensitive = yes
               wh-utb028aa-bt_era1:sensitive = yes
               wh-utb028aa-bt_mov1:sensitive = yes.
end.


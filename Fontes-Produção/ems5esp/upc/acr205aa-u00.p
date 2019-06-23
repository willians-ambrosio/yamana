/**************************************************************************
 ** Programa: upc/acr205aa-u00.p
 ** Autor   : Hilton Borba
 ** Data    : 13/11/2014
 **************************************************************************/
def input param p_ind_event  as char          no-undo.
def input param p_ind_object as char          no-undo.
def input param p_wgh_object as handle        no-undo.
def input param p_wgh_frame  as widget-handle no-undo.
def input param p_cod_table  as char          no-undo.
def input param p_rec_table  as recid         no-undo.

run upc/acr205aa-u01.p(input p_ind_event, 
                       input p_ind_object,
                       input p_wgh_object,
                       input p_wgh_frame, 
                       input p_cod_table, 
                       input p_rec_table).

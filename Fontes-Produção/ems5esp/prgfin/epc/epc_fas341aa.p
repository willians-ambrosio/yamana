/*****************************************************************************
** Copyright DATASUL S.A. (1994)
** Todos os Direitos Reservados.
** 
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so' podera ser feita mediante
** autorizacao expressa.
**
** Programa..............: epc_fas341aa.p
** Descricao.............: C lculos Per¡odo - EPC
** Versao................:  1.00.00.000
** Nome Externo..........: prgfin/epc/epc_fas341aa.p
** Criado por............: Duane
** Criado em.............: 14/12/2012
** Ficha de Ocorrˆncia...: TGDIFV
** Programa chamador.....: prgfin/fas/fas341aa.p
** Observa‡Æo............: O objetivo ‚ exportar o relat¢rio em CSV
**
*****************************************************************************/

def var c-versao-prg as char initial " 1.00.00.000":U no-undo.

{include/i_dbinst.i}
{include/i_dbtype.i}

/************************** Stream Definition Begin *************************/

def stream s-arq.


/*************************** Stream Definition End **************************/

/************************ Parameter Definition Begin ************************/

def input       param p_ind_event        as char          no-undo.
def input       param p_ind_object       as char          no-undo.
def input       param p_wgh_object       as handle        no-undo.
def input       param p_wgh_frame        as widget-handle no-undo.
def input       param p_cod_table        as char          no-undo.
def input       param p_rec_table        as recid         no-undo.

/************************* Parameter Definition End *************************/

/************************* Variable Definition Begin ************************/

def var h_objeto          as handle no-undo.
def var h_bt_print_epc    as handle no-undo.
def var h_tx_dest_csv     as handle no-undo.
def var h_rt_dest_csv     as handle no-undo.
def var h_bt_get_file_csv as handle no-undo.

def new global shared var v_cod_arq as char format 'x(60)' no-undo.

def new global shared var h_prog_aux        as handle no-undo.
def new global shared var h_bt_print        as handle no-undo.
def new global shared var h_fl_dest_csv     as handle no-undo.
def new global shared var h_tg_dest_csv     as handle no-undo.
def new global shared var h_rs_ind_run_mode as handle no-undo.


/************************** Variable Definition End *************************/

/****************************** Main Code Begin *****************************/

function copyWidget return handle (input p-handle as handle):

    def var h-handle as handle no-undo.

    if p-handle:type = "button" then do:
        create button h-handle
            assign frame        = p-handle:frame
                   row          = p-handle:row
                   col          = p-handle:col
                   width-chars  = p-handle:width-chars
                   height-chars = p-handle:height-chars
                   label        = p-handle:label
                   name         = p-handle:name + "_esp"
                   visible      = yes
                   sensitive    = yes.
    
        h-handle:load-image(p-handle:IMAGE).
        h-handle:load-image-insensitive(p-handle:IMAGE-INSENSITIVE).
        h-handle:move-to-top().
    end.

    return h-handle.
end.    

/* listar impressÆo extrato de versÆo */
if  v_cod_arq <> '' and v_cod_arq <> ? and p_ind_event  = "INITIALIZE" then do: 
    output stream s-arq to value(v_cod_arq) append.

        put stream s-arq unformatted
            "Chamada epc para tratamento especifico" at 1 
            "prgfin/epc/epc_fas341aa.p"        at 43 
            "1.00.00.000"               at 69 
            today                       at 84 format "99/99/99"
            string(time, 'HH:MM:SS')    at 94 skip.
    
    output stream s-arq close.
end.

/* Inicializa programa auxiliar */
run prgfin/epc/epc_fas341aa_aux.p persistent set h_prog_aux.

if  p_ind_event  = "INITIALIZE" 
and p_ind_object = "Viewer" then do:

    /* Aumentar o tamanho da tela */
    assign p_wgh_frame:height = p_wgh_frame:height + 2.5.
    
    assign h_objeto = p_wgh_frame:first-child.
    assign h_objeto = h_objeto:first-child.
    do while valid-handle(h_objeto):
        if h_objeto:type <> "field-group" then do:

            /* Descer os botäes do rodap‚ */
            if h_objeto:name = "bt_can" 
            or h_objeto:name = "bt_print" 
            or h_objeto:name = "bt_close" 
            or h_objeto:name = "bt_hel2" 
            or h_objeto:name = "rt_cxcf" then do:
                assign h_objeto:row = h_objeto:row + 2.3.
            end.

            /* Substituindo o bt_print */
            if h_objeto:name = "bt_print" then do:
                create button h_bt_print_epc.

                assign h_bt_print_epc = copyWidget(input h_objeto)
                       h_bt_print = h_objeto.
            end.

            /* Copia o bt_get_file */
            if h_objeto:name = "bt_get_file" then do:
                assign h_bt_get_file_csv = copyWidget(input h_objeto).
            end.

            /* Pega o handle do output para verificar se ‚ batch */
            if h_objeto:name = "rs_ind_run_mode" then do:
                assign h_rs_ind_run_mode = h_objeto.
            end.

            assign h_objeto = h_objeto:next-sibling.
        end.
        else do:
            leave.
        end.
    end.
   
    /* Criar os campos novos */
    create rectangle h_rt_dest_csv 
        assign frame             = p_wgh_frame
               height            = 1.75
               width             = 86.44
               row               = 15.05
               fgcolor           = 19
               col               = 2
               visible           = yes.

    create text h_tx_dest_csv
        assign frame             = p_wgh_frame
               format            = "x(17)"
               width             = 17
               row               = 14.75
               col               = 4
               fgcolor           = ?
               side-label-handle = h_tx_dest_csv:handle
               label             = " Destino Excel "
               visible           = yes.

    create toggle-box h_tg_dest_csv 
        assign frame             = p_wgh_frame
               width             = 22
               row               = 15.5
               col               = 3
               fgcolor           = ?
               bgcolor           = ?
               visible           = yes
               sensitive         = yes
               label             = "Imprime Arquivo Excel (.CSV)".

    create fill-in h_fl_dest_csv
        assign frame        = p_wgh_frame
               format       = "x(60)"
               height       = 1
               width        = 38
               row          = 15.5
               col          = 26
               fgcolor      = ?
               bgcolor      = 255
               visible      = yes
               sensitive    = yes
               screen-value = session:temp-directory + "fas341aa.csv".

    assign h_bt_get_file_csv:row = h_fl_dest_csv:row 
           h_bt_get_file_csv:col = h_fl_dest_csv:col + 39.

    
    /* Eventos */

    on 'choose' of h_bt_print_epc    persistent run pi_choose_bt_print_epc    in h_prog_aux.
    on 'choose' of h_bt_get_file_csv persistent run pi_choose_bt_get_file_csv in h_prog_aux.
    on 'leave'  of h_fl_dest_csv     persistent run pi_leave_fl_dest_csv      in h_prog_aux.
    on 'leave'  of h_rs_ind_run_mode persistent run pi_leave_rs_ind_run_mode  in h_prog_aux.

    /* Carraga Parƒmetros */

    run pi_get_params in h_prog_aux.
end.


if p_ind_event = 'imprimir' then do:
    run pi_print_csv in h_prog_aux.
end.

if p_ind_event = 'antes_imprimir' then do:
    run pi_antes_print_csv in h_prog_aux.
end.


if p_ind_event = 'parametros' then do:
    run pi_save_params in h_prog_aux.
end.

/******************************* Main Code End ******************************/

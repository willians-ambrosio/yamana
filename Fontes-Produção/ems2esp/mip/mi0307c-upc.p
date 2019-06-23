/*****************************************************************
** Autor...: Gustavo Eduardo Tamanini
** Empresa.: Yamana
** Programa: MI0307C-UPC
** UPC cadastrada para programa: MI0307C
** Objetivo: 
******************************************************************/
{include/i-prgvrs.i MI0307C-UPC 2.00.00.000}

def buffer ccusto for ems5.ccusto.

/** Parƒmetros **/
def input param p-ind-event  as character     no-undo.
def input param p-ind-object as character     no-undo.
def input param p-wgh-object as handle        no-undo.
def input param p-wgh-frame  as widget-handle no-undo.
def input param p-cod-table  as character     no-undo.
def input param p-row-table  as rowid         no-undo.

/** Handle **/
def var h-fPage1            as handle        no-undo.
def var h-tx-label1         as handle        no-undo.
def var h-tx-label2         as handle        no-undo.
def var h-mi0307c-upc       as handle        no-undo.
def var h-cd-tarefa         as handle        no-undo.
/** Global **/
def new global shared var wh-fi-tipo-manut-0307 as widget-handle no-undo.
def new global shared var wh-fi-desc-manut-0307 as widget-handle no-undo.
def new global shared var wh-fi-ct-despesa-0307 as widget-handle no-undo.
def new global shared var wh-fi-sc-despesa-0307 as widget-handle no-undo.

/** Global do MI0307-UPC **/
def new global shared var wh-fi-nr-ord-produ-mi0307 as widget-handle no-undo.
/** Zoom **/
def new global shared var adm-broker-hdl as handle no-undo.
def var wh-pesquisa                      as handle no-undo.

{utp/ut-glob.i}

def new global shared var v_rec_plano_cta_ctbl
    as recid
    format ">>>>>>9":U
    no-undo.

def new global shared var v_rec_cta_ctbl
    as recid
    format ">>>>>>9":U
    no-undo.

def new global shared var v_rec_plano_ccusto
    as recid
    format ">>>>>>9":U
    no-undo.

def new global shared var v_rec_ccusto
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.

def new global shared var v_cod_dwb_user
    as character
    format "x(21)"
    label "Usu rio"
    column-label "Usu rio"
    no-undo.

run findWidget (input "fPage1",    input "FRAME",   input p-wgh-frame, output h-fPage1).
run findWidget (input "cd-tarefa", input "FILL-IN", input p-wgh-frame, output h-cd-tarefa).

if  p-ind-event  = "AFTER-INITIALIZE":U and
    p-ind-object = "CONTAINER":U        then do:
    run mip/mi0307c-upc.p persistent set h-mi0307c-upc (input "",
                                                        input "",
                                                        input p-wgh-object,
                                                        input p-wgh-frame,
                                                        input "",
                                                        input p-row-table).
    /** Label Tipo Manutencao **/
    create text h-tx-label1
    assign frame        = h-fPage1
           format       = "x(16)"
           width        = 16
           screen-value = "Tipo Manuten‡Æo:":U
           row          = 6.40
           col          = 10.00
           visible      = yes
           font         = 1.

    /** Create Fill-in Tipo Manut **/
    create fill-in wh-fi-tipo-manut-0307
    assign frame     = h-fPage1
           name      = "wh-fi-tipo-manut-0307"
           format    = "x(05)"
           width     = 10.00
           height    = 0.88
           row       = 6.38
           col       = 23.00
           visible   = yes
           sensitive = yes
           font      = 1
           triggers:
                on "MOUSE-SELECT-DBLCLICK":U persistent run piZoomTipo  in h-mi0307c-upc.
                on "F5":U                    persistent run piZoomTipo  in h-mi0307c-upc.
                on "LEAVE":U                 persistent run piDescManut in h-mi0307c-upc.
           end.
           wh-fi-tipo-manut-0307:load-mouse-pointer("image~\lupa.cur":U).

    /** Create Fill-in Descricao Manut **/
    create fill-in wh-fi-desc-manut-0307
    assign frame     = h-fPage1
           name      = "wh-fi-desc-manut-0307"
           format    = "x(30)"
           width     = 25.00
           height    = 0.88
           row       = 6.38
           col       = 33.00
           visible   = yes
           sensitive = no
           font      = 1.        

    /** Label Despesa **/
    create text h-tx-label2
    assign frame        = h-fPage1
           format       = "x(08)"
           width        = 08
           screen-value = "Despesa:":U
           row          = 7.40
           col          = 16.00
           visible      = yes
           font         = 1.

    /** Create Fill-in Conta Despesa **/
    create fill-in wh-fi-ct-despesa-0307
    assign frame     = h-fPage1
           name      = "wh-fi-ct-despesa-0307"
           format    = "x(08)"
           width     = 10.00
           height    = 0.88
           row       = 7.38
           col       = 23.00
           visible   = yes
           sensitive = yes
           font      = 1
           triggers:
                on "MOUSE-SELECT-DBLCLICK":U  persistent run piZoomConta in h-mi0307c-upc.
                on "F5":U persistent run piZoomConta in h-mi0307c-upc.
           end.
           wh-fi-ct-despesa-0307:load-mouse-pointer("image~\lupa.cur":U).

    /** Create Fill-in Ccusto Despesa **/
    create fill-in wh-fi-sc-despesa-0307
    assign frame     = h-fPage1
           name      = "wh-fi-sc-despesa-0307"
           format    = "x(08)"
           width     = 10.00
           height    = 0.88
           row       = 7.38
           col       = 33.00
           visible   = yes
           sensitive = yes
           font      = 1
           triggers:
                on "MOUSE-SELECT-DBLCLICK":U  persistent run piZoomConta in h-mi0307c-upc.
                on "F5":U persistent run piZoomCcusto in h-mi0307c-upc.
           end.
           wh-fi-sc-despesa-0307:load-mouse-pointer("image~\lupa.cur":U).

     /** Busca em tabela especifica e atualiza os campos 
         Tipo de Manutencao e Conta Despesa **/
     for first mi-tar-ord
         where mi-tar-ord.nr-ord-produ = int(wh-fi-nr-ord-produ-mi0307:screen-value)
         and   mi-tar-ord.cd-tarefa    = int(h-cd-tarefa:screen-value) no-lock:
     end.
     if avail mi-tar-ord then
         assign wh-fi-tipo-manut-0307:screen-value = string(mi-tar-ord.tipo-manut)
                wh-fi-ct-despesa-0307:screen-value = mi-tar-ord.ct-despesa
                wh-fi-sc-despesa-0307:screen-value = mi-tar-ord.sc-despesa.
     else
         /** Sugeri Tipo e Conta da OM**/
         for first ord-manut
             where ord-manut.nr-ord-produ = int(wh-fi-nr-ord-produ-mi0307:screen-value) no-lock:
             assign wh-fi-tipo-manut-0307:screen-value = string(ord-manut.cd-tipo)
                    wh-fi-ct-despesa-0307:screen-value = ord-manut.ct-desp 
                    wh-fi-sc-despesa-0307:screen-value = ord-manut.sc-desp.
         end.

    /** Mostra descri‡Æo do Tipo de Manuten‡Æo **/
    for first tipo-manut
        where tipo-manut.cd-tipo = int(wh-fi-tipo-manut-0307:screen-value) no-lock:
        assign wh-fi-desc-manut-0307:screen-value = tipo-manut.descricao.
    end.
end.

procedure piZoomConta:
    find first plano_cta_ctbl no-lock
         where plano_cta_ctbl.dat_inic_valid        <= today
           and plano_cta_ctbl.dat_fim_valid         >= today
           and plano_cta_ctbl.ind_tip_plano_cta_ctbl = "Primario" no-error.

    assign v_rec_plano_cta_ctbl = recid(plano_cta_ctbl).
    if  search("prgint/utb/utb080nc.r") = ? and search("prgint/utb/utb080nc.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut vel não foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb080nc.p".
        else do:
            message "Programa execut vel não foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb080nc.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run prgint/utb/utb080nc.p /*prg_see_cta_ctbl_plano*/.
    if  v_rec_cta_ctbl <> ? then do:
        find cta_ctbl where recid(cta_ctbl) = v_rec_cta_ctbl no-lock no-error.
        assign wh-fi-ct-despesa-0307:screen-value = string(cta_ctbl.cod_cta_ctbl,plano_cta_ctbl.cod_format_cta_ctbl).
        apply 'entry' to wh-fi-ct-despesa-0307.
    end.
end procedure.

procedure piZoomCcusto:
    find first plano_ccusto no-lock
         where plano_ccusto.cod_empresa     = v_cod_empres_usuar
           and plano_ccusto.dat_inic_valid <= today
           and plano_ccusto.dat_fim_valid  >= today no-error.

    assign v_rec_plano_ccusto = recid(plano_ccusto).
    if  search("prgint/utb/utb066ka.r") = ? and search("prgint/utb/utb066ka.p") = ? then do:
        if  v_cod_dwb_user begins 'es_' then
            return "Programa execut vel não foi encontrado:" /*l_programa_nao_encontrado*/  + "prgint/utb/utb066ka.p".
        else do:
            message "Programa execut vel não foi encontrado:" /*l_programa_nao_encontrado*/  "prgint/utb/utb066ka.p"
                   view-as alert-box error buttons ok.
            return.
        end.
    end.
    else
        run prgint/utb/utb066ka.p /*prg_sea_ccusto*/.

    if  v_rec_ccusto <> ?
    then do:
        find ccusto where recid(ccusto) = v_rec_ccusto no-lock no-error.
        assign wh-fi-sc-despesa-0307:screen-value = string(ccusto.cod_ccusto).
        apply 'entry' to wh-fi-sc-despesa-0307.
    end.
end procedure.

procedure piZoomTipo:
    def var hProgramZoom as handle no-undo.
    {method/zoomfields.i 
           &ProgramZoom="mnzoom/z01mn158.w"
           &FieldZoom1="cd-tipo"
           &Frame1="fPage1"
           &FieldHandle1=wh-fi-tipo-manut-0307
           &FieldZoom2="descricao"
           &Frame2="fPage1"
           &FieldHandle2=wh-fi-desc-manut-0307}
end procedure.

procedure piDescManut:
    for first tipo-manut
        where tipo-manut.cd-tipo = int(wh-fi-tipo-manut-0307:screen-value) no-lock:
    end.
    if avail tipo-manut then
        assign wh-fi-desc-manut-0307:screen-value = tipo-manut.descricao
               wh-fi-ct-despesa-0307:screen-value = tipo-manut.ct-despesa
               wh-fi-sc-despesa-0307:screen-value = tipo-manut.sc-despesa.
    else
        assign wh-fi-desc-manut-0307:screen-value = "":U
               wh-fi-ct-despesa-0307:screen-value = "":U
               wh-fi-sc-despesa-0307:screen-value = "":U.
end procedure.

PROCEDURE findWidget:
    /*
    * PARAMETROS:
    *   c-widget-name:  nome do widget a ser localizado
    *   c-widget-type:  tipo do widget a ser localizado
    *   h-start-widget: container para procurar o widget
    *   h-widget:       widget encontrado 
    */

    define input  parameter c-widget-name  as char   no-undo.
    define input  parameter c-widget-type  as char   no-undo.
    define input  parameter h-start-widget as handle no-undo.
    define output parameter h-widget       as handle no-undo.

    do while valid-handle(h-start-widget):
        if h-start-widget:name = c-widget-name and
           h-start-widget:type = c-widget-type then do:
            assign h-widget = h-start-widget:handle.
            leave.
        end.

        if h-start-widget:type = "field-group":u or
           h-start-widget:type = "frame":u or
           h-start-widget:type = "dialog-box":u then do:
            run findWidget (input  c-widget-name,
                            input  c-widget-type,
                            input  h-start-widget:first-child,
                            output h-widget).
    
            if valid-handle(h-widget) then
                leave.
        end.
        assign h-start-widget = h-start-widget:next-sibling.
    end.
END PROCEDURE.

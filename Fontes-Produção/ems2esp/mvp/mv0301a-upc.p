/*****************************************************************
** Autor...: Gustavo Eduardo Tamanini
** Empresa.: Yamana
** Programa: MV0301A-UPC
** UPC cadastrada para programa: MV0301A
** Objetivo: 
******************************************************************/
{include/i-prgvrs.i MV0301A-UPC 2.06.00.000}

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
def var h-tx-label2         as handle        no-undo.
def var h-mv0301a-upc       as handle        no-undo.
def var h-fiDescManut       as handle        no-undo.
def var h-cd-tipo           as handle        no-undo.
def var h-cod-setor-ofici   as handle        no-undo.
def var h-nr-ord-produ      as handle        no-undo.
def var h-num-seq           as handle        no-undo.
/** Global **/
def new global shared var wh-fi-ct-despesa-0301   as widget-handle no-undo.
def new global shared var wh-fi-sc-despesa-0301   as widget-handle no-undo.

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

run findWidget (input "fPage1",          input "FRAME",   input p-wgh-frame, output h-fPage1).
run findWidget (input "fiDescManut",     input "FILL-IN", input h-fPage1,    output h-fiDescManut).
run findWidget (input "cd-tipo",         input "FILL-IN", input h-fPage1,    output h-cd-tipo).
run findWidget (input "cod-setor-ofici", input "FILL-IN", input h-fPage1,    output h-cod-setor-ofici).
run findWidget (input "nr-ord-produ",    input "FILL-IN", input p-wgh-frame, output h-nr-ord-produ).
run findWidget (input "num-seq",         input "FILL-IN", input p-wgh-frame, output h-num-seq).

if  p-ind-event  = "AFTER-INITIALIZE":U and
    p-ind-object = "CONTAINER":U        then do:
    run mvp/mv0301a-upc.p persistent set h-mv0301a-upc (input "",
                                                        input "",
                                                        input p-wgh-object,
                                                        input p-wgh-frame,
                                                        input "",
                                                        input p-row-table).

    /** Diminui o tamanho do campo Descricao do Tipo da Manutencao **/
    assign h-fiDescManut:width = 30.

    /** Label Despesas **/
    create text h-tx-label2
    assign frame        = h-fPage1
           format       = "x(08)"
           width        = 8
           screen-value = "Despesa:":U
           row          = 4.32
           col          = 58.00
           visible      = yes
           font         = 1.

    /** Create Fill-in Conta Despesa **/
    create fill-in wh-fi-ct-despesa-0301
    assign frame     = h-fPage1
           name      = "wh-fi-ct-despesa-0301"
           format    = "x(08)"
           width     = 10.00
           height    = 0.88
           row       = 4.25
           col       = 65.00
           visible   = yes
           sensitive = yes
           font      = 1
           triggers:
                on "MOUSE-SELECT-DBLCLICK":U  persistent run piZoomConta in h-mv0301a-upc.
                on "F5":U persistent run piZoomConta in h-mv0301a-upc.
                on "ENTRY":U persistent run piSugeriConta in h-mv0301a-upc.
           end.
           wh-fi-ct-despesa-0301:load-mouse-pointer("image~\lupa.cur":U).

    /** Create Fill-in Ccusto Despesa **/
    create fill-in wh-fi-sc-despesa-0301
    assign frame     = h-fPage1
           name      = "wh-fi-sc-despesa-0301"
           format    = "x(08)"
           width     = 10.00
           height    = 0.88
           row       = 4.25
           col       = 75.25
           visible   = yes
           sensitive = yes
           font      = 1
           triggers:
                on "MOUSE-SELECT-DBLCLICK":U  persistent run piZoomCcusto in h-mv0301a-upc.
                on "F5":U persistent run piZoomCcusto in h-mv0301a-upc.
           end.
           wh-fi-sc-despesa-0301:load-mouse-pointer("image~\lupa.cur":U).

     wh-fi-ct-despesa-0301:move-after-tab-item(h-cd-tipo).
     wh-fi-sc-despesa-0301:move-after-tab-item(wh-fi-ct-despesa-0301).
     h-cod-setor-ofici:move-after-tab-item(wh-fi-sc-despesa-0301).

     /** Exibi Conta e Tipo Manuten‡Æo **/
     for first mv-tar-ord
         where mv-tar-ord.nr-ord-produ = int(h-nr-ord-produ:screen-value)
         and   mv-tar-ord.cd-tarefa    = int(h-num-seq:screen-value) no-lock:
     end.
     if avail mv-tar-ord then
         assign wh-fi-ct-despesa-0301:screen-value = mv-tar-ord.ct-despesa
                wh-fi-sc-despesa-0301:screen-value = mv-tar-ord.sc-despesa.
     else
         run piSugeriContaOM in this-procedure.
end.

/** Sugeri Conta Despesa do Tipo de Manuten‡Æo **/
procedure piSugeriConta:
    for first tipo-manut
        where tipo-manut.cd-tipo = int(h-cd-tipo:screen-value) no-lock:
    end.
    if avail tipo-manut then
        assign wh-fi-ct-despesa-0301:screen-value = tipo-manut.ct-despesa
               wh-fi-sc-despesa-0301:screen-value = tipo-manut.sc-despesa.
    else
        assign wh-fi-ct-despesa-0301:screen-value = "":U
               wh-fi-sc-despesa-0301:screen-value = "":U.
end procedure.

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
        assign wh-fi-ct-despesa-0301:screen-value = string(cta_ctbl.cod_cta_ctbl,plano_cta_ctbl.cod_format_cta_ctbl).
        apply 'entry' to wh-fi-ct-despesa-0301.
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
        assign wh-fi-sc-despesa-0301:screen-value = string(ccusto.cod_ccusto).
        apply 'entry' to wh-fi-sc-despesa-0301.
    end.
end procedure.

/** Sugeri Conta Despesa da Ordem de Manutencao **/
procedure piSugeriContaOM:
    for first mmv-ord-manut
        where mmv-ord-manut.nr-ord-produ = int(h-nr-ord-produ:screen-value) no-lock:
    end.
    if avail mmv-ord-manut then
        assign wh-fi-ct-despesa-0301:screen-value = mmv-ord-manut.ct-codigo
               wh-fi-sc-despesa-0301:screen-value = mmv-ord-manut.cc-codigo.
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


/******************************************************************************
**
**  Programa: upc/escc0311-u01.p
**  Data....: Junho 2013
**  Autor...: Fernando Campos - DSC
**  Objetivo: Bloqueia condi»’o de pagamento desativada
**
******************************************************************************/
 
{include/i-prgvrs.i escc0311-u01 2.06.00.001}
{tools/fc-handle-obj.i}
{tools/fc-falso.i}

/*-----> Define de Parametros <---------------------------------------*/
def input parameter p-ind-event              as char          no-undo.
def input parameter p-ind-object             as char          no-undo.
def input parameter p-wgh-object             as handle        no-undo.
def input parameter p-wgh-frame              as widget-handle no-undo.
def input parameter p-cod-table              as char          no-undo.
def input parameter p-row-table              as rowid         no-undo.

/*-----> Define de Variaveis <----------------------------------------*/

def new global shared var wh-escc0311-cod-cond-pag  as widget-handle   no-undo.
def new global shared var wh-escc0311-bt-executar   as widget-handle   no-undo.
def new global shared var wh-escc0311-bt-executar-f as widget-handle   no-undo.

def var c-handle-obj        as character        no-undo.

/*-----> Main Block <-------------------------------------------------*/

/* assign c-handle-obj              = fc-handle-obj("i-condicao", p-wgh-frame)       */
/*        wh-escc0311-cod-cond-pag = widget-handle(entry(1, c-handle-obj)) no-error. */
/* if valid-handle(wh-escc0311-cod-cond-pag) then                                    */
/* message "p-ind-event..:" p-ind-event               skip                           */
/*         "p-ind-object.:" p-ind-object              skip                           */
/*         "p-wgh-object.:" p-wgh-object:name skip                                   */
/*         "p-wgh-frame..:" string(p-wgh-frame)       skip                           */
/*         "p-cod-table..:" string(p-cod-table)       skip                           */
/*         "p-row-table..:" string(p-row-table)       skip                           */
/*         view-as alert-box info buttons ok.                                        */

if p-ind-event  = "BEFORE-INITIALIZE"        and
   p-ind-object = "CONTAINER"                and
   p-wgh-object:file-name matches "*cc0311*" then do:

    assign c-handle-obj            = fc-handle-obj("bt-executar", p-wgh-frame)
           wh-escc0311-bt-executar = widget-handle(entry(1, c-handle-obj))
           no-error.

    assign wh-escc0311-bt-executar-f = fc-falso(wh-escc0311-bt-executar,
                                                wh-escc0311-bt-executar:frame, "").

    if wh-escc0311-bt-executar-f:move-to-top() then.

    on choose of wh-escc0311-bt-executar-f persistent run upc/escc0311-u01.p (input "VALIDATE",
                                                                              input "BT-EXECUTAR",
                                                                              input p-wgh-object,
                                                                              input p-wgh-frame,
                                                                              input p-cod-table,
                                                                              input p-row-table).

end.

if p-ind-event       = "CHANGE-PAGE" and
   p-ind-object      = "CONTAINER"   and
   p-wgh-object:name = "f-pg-par"    then do:

    assign c-handle-obj             = fc-handle-obj("i-condicao", p-wgh-frame)
           wh-escc0311-cod-cond-pag = widget-handle(entry(1, c-handle-obj)) no-error.

end.

if p-ind-event  = "VALIDATE"    and
   p-ind-object = "BT-EXECUTAR" then do:

    if valid-handle(wh-escc0311-cod-cond-pag) then do:

        find es-cond-pagto no-lock
            where es-cond-pagto.cod-cond-pag = int(wh-escc0311-cod-cond-pag:screen-value) no-error.
        if avail es-cond-pagto    and
           es-cond-pagto.cd-ativa then do:
            run utp/ut-msgs.p("show",
                              17006,
                              "Condi‡Æo de pagamento desativada~~Condi‡Æo de pagamento desativada. Favor informar uma condi‡Æo de pagamento valida").
            return "NOK".
        end.
        else do:

            apply 'choose' to wh-escc0311-bt-executar.

        end.

    end.
    else do:

        apply 'choose' to wh-escc0311-bt-executar.

    end.


end.


/******************************************************************************
**
**  Programa: upc/escn0201a-u01.p
**  Data....: Junho 2013
**  Autor...: Fernando Campos - DSC
**  Objetivo: Bloqueia condi‡Æo de pagamento desativada
**
******************************************************************************/
 
{include/i-prgvrs.i escn0201a-u01 2.06.00.001}
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

def new global shared var wh-escn0201a-cod-cond-pag as handle   no-undo.

def var c-handle-obj        as character       no-undo.

/*-----> Main Block <-------------------------------------------------*/

/* assign c-handle-obj              = fc-handle-obj("cod-cond-pag", p-wgh-frame)      */
/*        wh-escn0201a-cod-cond-pag = widget-handle(entry(1, c-handle-obj)) no-error. */
/* if valid-handle(wh-escn0201a-cod-cond-pag) then                                    */
    
/* message "p-ind-event..:" p-ind-event            skip     */
/*             "p-ind-object.:" p-ind-object           skip */
/*             "p-wgh-object.:" p-wgh-object:file-name skip */
/*             "p-wgh-frame..:" string(p-wgh-frame)    skip */
/*             "p-cod-table..:" string(p-cod-table)    skip */
/*             "p-row-table..:" string(p-row-table)    skip */
/*             view-as alert-box info buttons ok.           */

if p-ind-event  = "BEFORE-INITIALIZE"         and
   p-ind-object = "CONTAINER"                 and
   p-wgh-object:file-name matches "*cn0201a*" then do:

    assign c-handle-obj              = fc-handle-obj("cod-cond-pag", p-wgh-frame)
           wh-escn0201a-cod-cond-pag = widget-handle(entry(1, c-handle-obj)) no-error.

end.

if p-ind-event  = "VALIDATE"                      and
   p-ind-object = "VIEWER"                        and
   p-wgh-object:file-name matches "*cn0201a-v01*" then do:

    find es-cond-pagto no-lock
        where es-cond-pagto.cod-cond-pag = int(wh-escn0201a-cod-cond-pag:screen-value) no-error.
    if avail es-cond-pagto    and
       es-cond-pagto.cd-ativa then do:
        run utp/ut-msgs.p("show",
                          17006,
                          "Condi‡Æo de pagamento desativada~~Condi‡Æo de pagamento desativada. Favor informar uma condi‡Æo de pagamento valida").
        return "NOK".
    end.

end.

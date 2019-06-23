/******************************************************************************
**
**  Programa: upc/escd0404-u01.p
**  Data....: Junho 2013
**  Autor...: Fernando Campos - DSC
**  Objetivo: Flag para situa‡Æo da condi‡Æo de pagamento
**
******************************************************************************/
 
{include/i-prgvrs.i escd0404-u01 2.06.00.001}
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

def new global shared var wh-escd0404-tg-ativa          as handle   no-undo.
def new global shared var wh-escd0404-lAtualizaIndice   as handle   no-undo.

def var c-handle-obj        as character       no-undo.

/*-----> Main Block <-------------------------------------------------*/

/* message "p-ind-event..:" p-ind-event            skip */
/*         "p-ind-object.:" p-ind-object           skip */
/*         "p-wgh-object.:" p-wgh-object:file-name skip */
/*         "p-wgh-frame..:" string(p-wgh-frame)    skip */
/*         "p-cod-table..:" string(p-cod-table)    skip */
/*         "p-row-table..:" string(p-row-table)    skip */
/*         view-as alert-box info buttons ok.           */

if p-ind-event  = "BEFORE-DISPLAY"             and
   p-ind-object = "VIEWER"                     and
   p-wgh-object:file-name matches "*V03ad039*" then do:

    assign c-handle-obj                = fc-handle-obj("lAtualizaIndice", p-wgh-frame)
           wh-escd0404-lAtualizaIndice = widget-handle(entry(1, c-handle-obj)) no-error.

    create toggle-box wh-escd0404-tg-ativa
        assign frame       = wh-escd0404-lAtualizaIndice:frame
               width       = wh-escd0404-lAtualizaIndice:width
               height      = wh-escd0404-lAtualizaIndice:height
               row         = wh-escd0404-lAtualizaIndice:row + 1
               column      = wh-escd0404-lAtualizaIndice:column
               format      = "yes/no"
               label       = "Condi‡Æo de Pagto Desativada"
               name        = "tg-ativa"
               sensitive   = yes
               visible     = yes
        triggers:
            on "VALUE-CHANGED" persistent run upc\escd0404-u01.p(input "value-changed",
                                                                 input "wh-escd0404-tg-ativa",
                                                                 input p-wgh-object,
                                                                 input p-wgh-frame,
                                                                 input p-cod-table,
                                                                 input p-row-table).
        end triggers.

end.

if p-ind-event  = "DISPLAY"                    and
   p-ind-object = "VIEWER"                     and
   p-wgh-object:file-name matches "*V03ad039*" then do:

    for first cond-pagto  fields(cod-cond-pag)
        where rowid(cond-pagto) = p-row-table no-lock,
        first es-cond-pagto fields(cd-ativa)
        where es-cond-pagto.cod-cond-pag = cond-pagto.cod-cond-pag no-lock:

    end.

    if avail es-cond-pagto then
        assign wh-escd0404-tg-ativa:checked = es-cond-pagto.cd-ativa.
    else
        assign wh-escd0404-tg-ativa:checked = no.

end.

if p-ind-event  = "END-UPDATE"                 and
   p-ind-object = "VIEWER"                     and
   p-wgh-object:file-name matches "*V03ad039*" then do:

    for first cond-pagto  fields(cod-cond-pag)
        where rowid(cond-pagto) = p-row-table no-lock,
        first es-cond-pagto fields(cd-ativa)
        where es-cond-pagto.cod-cond-pag = cond-pagto.cod-cond-pag exclusive-lock:

    end.

    if not avail es-cond-pagto then do:
        create es-cond-pagto.
        assign es-cond-pagto.cod-cond-pag = cond-pagto.cod-cond-pag.
    end.

    assign es-cond-pagto.cd-ativa = wh-escd0404-tg-ativa:checked.

end.

if valid-handle(wh-escd0404-tg-ativa) then
    assign wh-escd0404-tg-ativa:sensitive = wh-escd0404-lAtualizaIndice:sensitive.

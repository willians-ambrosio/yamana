/******************************************************************************
**
**  Programa: upc/ucc0300a-u01.p
**  Data....: Junho 2013
**  Autor...: Fernando Campos - DSC
**  Objetivo: Bloqueia condi‡Æo de pagamento desativada
**
******************************************************************************/
 
{include/i-prgvrs.i ucc0300a-u01 2.06.00.001}
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

def new global shared var wh-ucc0300a-btOk         as widget-handle   no-undo.
def new global shared var wh-ucc0300a-btOk-f       as widget-handle   no-undo.
def new global shared var wh-ucc0300a-btSave       as widget-handle   no-undo.
def new global shared var wh-ucc0300a-btSave-f     as widget-handle   no-undo.
def new global shared var wh-ucc0300a-cod-cond-pag as widget-handle   no-undo.

def var c-handle-obj        as character        no-undo.
def var h_frame             as widget-handle    no-undo.
def var h_page              as widget-handle    no-undo.
def var c-campo             as character        no-undo.
def var i-cont              as integer          no-undo.

DEFINE TEMP-TABLE RowErros NO-UNDO
    FIELD errorSequence      AS INTEGER
    FIELD errorNumber        AS INTEGER
    FIELD errorDescription   AS CHARACTER 
    FIELD errorParameters    AS CHARACTER
    FIELD errorType          AS CHARACTER
    FIELD errorHelp          AS CHARACTER
    FIELD errorsubtype       AS CHARACTER.

/*-----> Main Block <-------------------------------------------------*/

/* assign c-handle-obj              = fc-handle-obj("cod-cond-pag", p-wgh-frame)      */
/*        wh-ucc0300a-cod-cond-pag = widget-handle(entry(1, c-handle-obj)) no-error. */
/* if valid-handle(wh-ucc0300a-cod-cond-pag) then                                    */
/* message "p-ind-event..:" p-ind-event            skip                               */
/*         "p-ind-object.:" p-ind-object           skip                               */
/*         "p-wgh-object.:" p-wgh-object:file-name skip                               */
/*         "p-wgh-frame..:" string(p-wgh-frame)    skip                               */
/*         "p-cod-table..:" string(p-cod-table)    skip                               */
/*         "p-row-table..:" string(p-row-table)    skip                               */
/*         view-as alert-box info buttons ok.                                         */

if p-ind-event  = "BEFORE-INITIALIZE"           and
   p-ind-object = "CONTAINER"                   and
   p-wgh-object:file-name matches "*cc0300a.w*" then do:

    assign c-handle-obj              = fc-handle-obj("cod-cond-pag,btOK,btSave", p-wgh-frame)
           wh-ucc0300a-cod-cond-pag = widget-handle(entry(1, c-handle-obj))
           wh-ucc0300a-btOk         = widget-handle(entry(2, c-handle-obj))
           wh-ucc0300a-btSave       = widget-handle(entry(3, c-handle-obj))
           no-error.

    assign wh-ucc0300a-btOk-f = fc-falso(wh-ucc0300a-btOk,
                                          wh-ucc0300a-btOk:frame, "").

    if wh-ucc0300a-btOk-f:move-to-top() then.

    on choose of wh-ucc0300a-btOk-f persistent run upc/ucc0300a-u01.p (input "CHOOSE",
                                                                       input "BT-OK",
                                                                       input p-wgh-object,
                                                                       input p-wgh-frame,
                                                                       input p-cod-table,
                                                                       input p-row-table).

    assign wh-ucc0300a-btSave-f = fc-falso(wh-ucc0300a-btSave,
                                            wh-ucc0300a-btSave:frame, "").

    if wh-ucc0300a-btSave-f:move-to-top() then.

    on choose of wh-ucc0300a-btSave-f persistent run upc/ucc0300a-u01.p (input "CHOOSE",
                                                                         input "BT-SAVE",
                                                                         input p-wgh-object,
                                                                         input p-wgh-frame,
                                                                         input p-cod-table,
                                                                         input p-row-table).

    assign c-campo = "nr-processo,cb-frete,cod-transp,cb-via-transp,end-entrega,end-cobranca,cod-cond-pag,responsavel,cod-mensagem,cod-estab-gestor".
    do i-cont = 1 to num-entries(c-campo):
        assign c-handle-obj = fc-handle-obj(entry(i-cont,c-campo), p-wgh-frame)
               h_frame      = widget-handle(entry(1, c-handle-obj)).
        on return of h_frame persistent run upc/ucc0300a-u01.p(input "RETURN",
                                                               input "BT-SAVE",
                                                               input p-wgh-object,
                                                               input p-wgh-frame,
                                                               input p-cod-table,
                                                               input p-row-table).
    end.

end.

if p-ind-event  = "BEFORE-INITIALIZE"           and
   p-ind-object = "CONTAINER"                   and
   p-wgh-object:file-name matches "*cc0300a.w*" then do:

    assign h_frame = p-wgh-frame:first-child
           h_frame = h_frame    :first-child.
    do while valid-handle(h_frame):
        if h_frame:type <> "field-group" then do:
            if lookup(h_frame:name,"num-pedido,data-pedido,cod-emitente,cod-emit-terc,cb-natureza,emergencial,impr-pedido,nr-processo,cb-frete") > 0 then do:
                on return of h_frame persistent run upc/ucc0300a-u01.p(input "RETURN",
                                                                       input "BT-SAVE",
                                                                       input p-wgh-object,
                                                                       input p-wgh-frame,
                                                                       input p-cod-table,
                                                                       input p-row-table).
            end.
            assign h_frame = h_frame:next-sibling.
        end.
        else
            assign h_frame = h_frame:first-child.
    end.

end.

if p-ind-event  = "RETURN"  and
   p-ind-object = "BT-SAVE" then do:

    run upc/ucc0300a-u01.p(input "CHOOSE",
                           input "BT-SAVE",
                           input p-wgh-object,
                           input p-wgh-frame,
                           input p-cod-table,
                           input p-row-table).

end.

if p-ind-event  = "CHOOSE"    and
   (p-ind-object = "BT-OK"    or
    p-ind-object = "BT-SAVE") then do:

    find es-cond-pagto no-lock
        where es-cond-pagto.cod-cond-pag = int(wh-ucc0300a-cod-cond-pag:screen-value) no-error.
    if avail es-cond-pagto    and
       es-cond-pagto.cd-ativa then do:

        run showErrorsDBO in p-wgh-object(input this-procedure).

    end.
    else do:

        if p-ind-object = "BT-OK" then
            apply "CHOOSE" to wh-ucc0300a-btOk.

        if p-ind-object = "BT-SAVE" then
            apply "CHOOSE" to wh-ucc0300a-btSave.

    end.

end.

if valid-handle(wh-ucc0300a-btOk) then
    assign wh-ucc0300a-btOk:sensitive = no.

if valid-handle(wh-ucc0300a-btSave) then
    assign wh-ucc0300a-btSave:sensitive = no.

procedure getRowErrors:

    def output parameter table for RowErros.

    create RowErros.
    assign RowErros.errorSequence      = 1
           RowErros.errorNumber        = 17006
           RowErros.errorDescription   = "Condi‡Æo de pagamento desativada"
           RowErros.errorSubType       = "ERROR"
           RowErros.errorHelp          = "Condi‡Æo de pagamento desativada. Favor informar uma condi‡Æo de pagamento valida"
/*         FIELD errorParameters    AS CHARACTER */
/*         FIELD errorType          AS CHARACTER */
           .

end.

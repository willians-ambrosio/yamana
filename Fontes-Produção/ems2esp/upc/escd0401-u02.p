/******************************************************************************
**
**  Programa: upc/escd0401-u02.p
**  Data....: 11/06/2013
**  Autor...: Bruno Bertulli (DSC)
**  Objetivo: Bloqueia flag retem pagto
**
******************************************************************************/
 
{include/i-prgvrs.i escd0401-u02 2.06.00.001}
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

def new global shared var wh-escd0401-retem-pagto   as widget-handle   no-undo.
def new global shared var wh-escd0401-l-cooperativa as widget-handle   no-undo.

def var c-handle-obj        as character        no-undo.

/*-----> Main Block <-------------------------------------------------*/

/* MESSAGE 'p-ind-event  ' p-ind-event   skip          */
/*         'p-ind-object ' p-ind-object  skip          */
/*         'p-wgh-object ' p-wgh-object  skip          */
/*         'p-wgh-frame  ' p-wgh-frame   skip          */
/*         'p-cod-table  ' p-cod-table   skip          */
/*         'p-row-table  ' STRING (p-row-table)   skip */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.              */

if p-ind-event  = "INITIALIZE" and
   p-ind-object = "VIEWER"     then do:
    assign c-handle-obj             = fc-handle-obj("l-retem-pagto", p-wgh-frame)
           wh-escd0401-retem-pagto  = widget-handle(entry(1, c-handle-obj))
           no-error.

    assign c-handle-obj              = fc-handle-obj("l-cooperativa", p-wgh-frame)
           wh-escd0401-l-cooperativa = widget-handle(entry(1, c-handle-obj))
           no-error.
end.
    
if p-ind-event  = "ADD"    and
   p-ind-object = "VIEWER" then do:
    IF VALID-HANDLE (wh-escd0401-retem-pagto) THEN
        wh-escd0401-retem-pagto:SCREEN-VALUE = "YES".
END.


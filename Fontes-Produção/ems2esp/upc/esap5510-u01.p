
/*******************************************************************************
**       Programa: upc/esapi5510-u.p
**       Data....: Abril/2010
**       Autor...: Roger Labadessa. (Vertice Tecnologia S.A.)
**       Objetivo: UPC - Criar campo("Valor") na pasta de sele‡Æo.
*******************************************************************************/

{include/i-prgvrs.i espap5510-u01 2.06.00.000}

/************ Definicao de Parametros **************/
define input             parameter p-ind-event                   as character.
define input             parameter p-ind-object                  as character.
define input             parameter p-wgh-object                  as handle.
define input             parameter p-wgh-frame                   as widget-handle.
define input             parameter p-cod-table                   as character.
define input             parameter p-row-table                   as rowid.

define                   variable c-objeto                       as character      no-undo.

/****************** Definicoes de Variaveis Globais **********************/

def new global shared var wgh-child              as widget-handle no-undo.
def new global shared var wgh-im-pg-sel          as widget-handle no-undo.
def new global shared var wgh-im-pg-par          as widget-handle no-undo.
def new global shared var wgh_tx_val_doc_ap5510  as widget-handle no-undo.
def new global shared var wgh_val_ini_doc_ap5510 as widget-handle no-undo.
def new global shared var wgh_val_fim_doc_ap5510 as widget-handle no-undo.
def new global shared var wgh-image1-ap5510      as widget-handle no-undo.
def new global shared var wgh-image2-ap5510      as widget-handle no-undo.

/************************** Bloco Principal *****************************/
assign c-objeto = entry(num-entries(p-wgh-object:file-name,"~/"),p-wgh-object:file-name,"~/") no-error.

if p-ind-event      = "CHANGE-PAGE" and
   p-ind-object     = "CONTAINER"   AND
   p-wgh-frame:NAME = "f-pg-sel"    and
   NOT(valid-handle(wgh_tx_val_doc_ap5510)) then do:

    /* ----- Cria o label do campo "Valor" ----- */
    create TEXT wgh_tx_val_doc_ap5510
        assign frame         = p-wgh-frame
               format        = "x(13)"
               SCREEN-VALUE  = "Valor:"
               width         = 9.5
               row           = 10.5
               col           = 22.67
               HEIGHT        = 0.88
               fgcolor       = 0
               VISIBLE       = YES.

    /* ----- Cria o campo "Codigo Valor" ----- */
    create fill-in wgh_val_ini_doc_ap5510
        assign frame         = p-wgh-frame
               width         = 14.5
               height        = 0.88
               row           = 10.5
               help          = "Valor Documento - Ini"
               col           = 27
               name          = "v_val_doc_ini"
               data-type     = "decimal"
               format        = ">>>,>>>,>>>,>>9.99"
               visible       = YES
               sensitive     = YES.

    /* ----- Cria o campo "Codigo Valor" ----- */
    create fill-in wgh_val_fim_doc_ap5510
        assign frame         = p-wgh-frame
               width         = 15
               height        = 0.88
               row           = 10.5
               help          = "Valor Documento - Fim"
               col           = 49.57
               name          = "v_val_doc_fim"
               data-type     = "Decimal"
               format        = ">>>,>>>,>>>,>>9.99"
               visible       = YES
               sensitive     = YES.

    ASSIGN wgh_val_ini_doc_ap5510:SCREEN-VALUE  = "0"
           wgh_val_fim_doc_ap5510:SCREEN-VALUE  = "999999999999".

    create image wgh-image1-ap5510
         assign frame            = p-wgh-frame
                row              = wgh_val_ini_doc_ap5510:ROW
                col              = 42.
    wgh-image1-ap5510:load-image("image\im-fir").
    
    create image wgh-image2-ap5510
         assign frame            = p-wgh-frame
                row              = wgh_val_ini_doc_ap5510:ROW
                col              = 45.43.
    wgh-image2-ap5510:load-image("image\im-las").

    
END.

if p-ind-event      = "initialize" and
   p-ind-object     = "CONTAINER"  then do:

    DO WHILE VALID-HANDLE(p-wgh-frame):
        ASSIGN wgh-child = p-wgh-frame:FIRST-CHILD /* field grupo*/
               wgh-child = wgh-child:FIRST-CHILD. /* field */

        DO WHILE VALID-HANDLE(wgh-child):
            CASE wgh-child:NAME:
                WHEN "im-pg-par" THEN
                    ASSIGN wgh-im-pg-par = wgh-child.
                WHEN "im-pg-sel" THEN
                    ASSIGN wgh-im-pg-sel = wgh-child.
            END CASE.
            /* ----- Busca proximo objeto ----- */
            ASSIGN wgh-child = wgh-child:NEXT-SIBLING.
        end.
        leave.
    end.
    
    apply 'MOUSE-SELECT-CLICK' TO wgh-im-pg-par.
    apply 'MOUSE-SELECT-CLICK' TO wgh-im-pg-sel.

END.

if p-ind-event      = "DESTROY"   and
   p-ind-object     = "CONTAINER" then do:
    ASSIGN wgh-child               = ?
           wgh-im-pg-sel           = ?
           wgh-im-pg-par           = ?
           wgh_tx_val_doc_ap5510   = ?
           wgh_val_ini_doc_ap5510  = ?
           wgh_val_fim_doc_ap5510  = ?
           wgh-image1-ap5510       = ?
           wgh-image2-ap5510       = ?.
END.

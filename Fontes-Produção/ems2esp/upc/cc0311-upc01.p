/* --------------------------------------------------------------------------------------- *\
|                                                                                           |

|  Programa...............: cc0311-upc01.P                                                 |
|  Sub Programa...........:                                                                 |
|  Descricao..............: UPC INCLUSÇO DE PEDIDO DE COMPRA                                |
|  Entidade Desenvolvedora: DSC                                                             |
|                                                                                           |
|  Historico Programa -------------------------------------------------------------------+  |
|  | Data       | Autor               | Descricao                                        |  |
|  +----------- +---------------------+--------------------------------------------------+  |
|  | 21|05|2009 | Luiz CRUZ           | Desenvolvimento do Programa                      |  |
|  +------------+---------------------+--------------------------------------------------+  |
|  | Parametros :                                                                        |  |
|  |                                                                                     |  |
|  | Observacao :                                                                        |  |
|  |                                                                                     |  |
|  +-------------------------------------------------------------------------------------+  |
\  --------------------------------------------------------------------------------------- */
{include/i-prgvrs.i cc0311-upc01.p 2.06.00.001}

{tools/fc-handle-obj.i}
{tools/fc-falso.i}       
{utp/ut-glob.i}

define input parameter p-ind-event  as character     no-undo.
define input parameter p-ind-object as character     no-undo.
define input parameter p-wgh-object as handle        no-undo.
define input parameter p-wgh-frame  as widget-handle no-undo.
define input parameter p-cod-table  as character     no-undo.
define input parameter p-row-table  as rowid         no-undo.

define new global shared variable wh-cc0311-responsavel as widget-handle no-undo.                                                                  
define new global shared variable wh-cc0311-rs-frete    as widget-handle no-undo.                                                                  

define variable c-handle-obj        as character     no-undo.
define variable c-objeto            as character     no-undo. 

IF  p-ind-event  = "change-page" 
AND p-ind-object = "CONTAINER"
and p-wgh-frame:name = "f-pg-par" THEN DO:

    ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-frame:PRIVATE-DATA, "~/"), p-wgh-frame:PRIVATE-DATA, "~/").


    ASSIGN c-handle-obj = fc-handle-obj("c-resp,rs-frete",p-wgh-frame).
    ASSIGN 
      wh-cc0311-responsavel = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
      wh-cc0311-rs-frete    = WIDGET-HANDLE(ENTRY(2,c-handle-obj)).

      if valid-handle(wh-cc0311-responsavel) then
        assign wh-cc0311-responsavel:sensitive    = no
               wh-cc0311-responsavel:screen-value = c-seg-usuario.

      apply 'leave' to wh-cc0311-responsavel.

      if valid-handle(wh-cc0311-rs-frete) then
        assign wh-cc0311-rs-frete:screen-value = string(2).




end.

/* fim do programa */

/* --------------------------------------------------------------------------------------- *\
|                                                                                           |

|  Programa...............: cc0316a-upc01.P                                                 |
|  Sub Programa...........:                                                                 |
|  Descricao..............: UPC INCLUSÇO DE PROCESSO DE COMPRA                                |
|  Entidade Desenvolvedora: DSC                                                             |
|                                                                                           |
|  Historico Programa -------------------------------------------------------------------+  |
|  | Data       | Autor               | Descricao                                        |  |
|  +----------- +---------------------+--------------------------------------------------+  |
|  | 22|05|2009 | Luiz CRUZ           | Desenvolvimento do Programa                      |  |
|  +------------+---------------------+--------------------------------------------------+  |
|  | Parametros :                                                                        |  |
|  |                                                                                     |  |
|  | Observacao :                                                                        |  |
|  |                                                                                     |  |
|  +-------------------------------------------------------------------------------------+  |
\  --------------------------------------------------------------------------------------- */
{include/i-prgvrs.i cc0316a-upc01.p 2.06.00.001}

{tools/fc-handle-obj.i}
{tools/fc-falso.i}       
{utp/ut-glob.i}

define input parameter p-ind-event  as character     no-undo.
define input parameter p-ind-object as character     no-undo.
define input parameter p-wgh-object as handle        no-undo.
define input parameter p-wgh-frame  as widget-handle no-undo.
define input parameter p-cod-table  as character     no-undo.
define input parameter p-row-table  as rowid         no-undo.

define new global shared variable wh-cc0316a-comprador     as widget-handle no-undo.                                                                  

define variable c-handle-obj        as character no-undo.
define variable c-objeto            as character no-undo. 

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-frame:PRIVATE-DATA, "~/"), p-wgh-frame:PRIVATE-DATA, "~/").

IF    p-ind-event = "BEFORE-INITIALIZE" 
  AND p-ind-object = "CONTAINER"        THEN DO:

  ASSIGN c-handle-obj = fc-handle-obj("cod-comprado",p-wgh-frame).
  ASSIGN 
    wh-cc0316a-comprador    = WIDGET-HANDLE(ENTRY(1,c-handle-obj)).
END.

IF p-ind-event  = "AFTER-ENABLE" AND
   p-ind-object = "CONTAINER"     THEN DO:

    if valid-handle(wh-cc0316a-comprador) then
        assign wh-cc0316a-comprador:SENSITIVE = no
               wh-cc0316a-comprador:screen-value = c-seg-usuario.
        
    apply 'leave' to wh-cc0316a-comprador.

END.
  
/* fim do programa */

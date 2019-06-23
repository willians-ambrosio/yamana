/* --------------------------------------------------------------------------------------- *\
|                                                                                           |

|  Programa...............: cn0201a-upc01.P                                                 |
|  Sub Programa...........:                                                                 |
|  Descricao..............: UPC NA MANUTENCAO DO CONTRATO                                   |
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
{include/i-prgvrs.i cn0201a-upc01.p 2.06.00.001}

{tools/fc-handle-obj.i}
{tools/fc-falso.i}       
{utp/ut-glob.i}

define input parameter p-ind-event  as character     no-undo.
define input parameter p-ind-object as character     no-undo.
define input parameter p-wgh-object as handle        no-undo.
define input parameter p-wgh-frame  as widget-handle no-undo.
define input parameter p-cod-table  as character     no-undo.
define input parameter p-row-table  as rowid         no-undo.

define new global shared variable wh-cn0201a-comprador     as widget-handle no-undo.                                                                  

define variable c-handle-obj        as character no-undo.
define variable c-objeto            as character no-undo. 

assign c-objeto = entry(num-entries(p-wgh-frame:private-data, "~/"), p-wgh-frame:private-data, "~/").

if   p-ind-event = "after-change-page" 
and p-ind-object = "CONTAINER"   then do:

  assign  c-handle-obj = fc-handle-obj("cod-comprado",p-wgh-frame)
            wh-cn0201a-comprador = widget-handle(entry(1,c-handle-obj))
            wh-cn0201a-comprador:sensitive = no
            wh-cn0201a-comprador:screen-value = c-seg-usuario. 
    
    apply 'leave' to wh-cn0201a-comprador.
end.

/* fim do programa */

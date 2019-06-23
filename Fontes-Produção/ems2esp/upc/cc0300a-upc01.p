/* --------------------------------------------------------------------------------------- *\
|                                                                                           |

|  Programa...............: cc0300a-upc01.P                                                 |
|  Sub Programa...........:                                                                 |
|  Descricao..............: UPC INCLUSÇO DE PEDIDO DE COMPRA                                |
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
{include/i-prgvrs.i cc0300a-upc01.p 2.06.00.001}

{tools/fc-handle-obj.i}
{tools/fc-falso.i}       
{utp/ut-glob.i}

define input parameter p-ind-event  as character     no-undo.
define input parameter p-ind-object as character     no-undo.
define input parameter p-wgh-object as handle        no-undo.
define input parameter p-wgh-frame  as widget-handle no-undo.
define input parameter p-cod-table  as character     no-undo.
define input parameter p-row-table  as rowid         no-undo.

define new global shared variable wh-cc0300a-responsavel as widget-handle no-undo.                                                                  

define variable c-handle-obj    as character no-undo.
DEFINE VARIABLE c-objeto       AS CHARACTER            NO-UNDO. 

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-frame:PRIVATE-DATA, "~/"), p-wgh-frame:PRIVATE-DATA, "~/").

IF    p-ind-event = "BEFORE-INITIALIZE" 
  AND p-ind-object = "CONTAINER"        THEN DO:

  ASSIGN c-handle-obj = fc-handle-obj("responsavel",p-wgh-frame).
  ASSIGN 
    wh-cc0300a-responsavel    = WIDGET-HANDLE(ENTRY(1,c-handle-obj)).
END.

IF p-ind-event  = "AFTER-ENABLE" AND
   p-ind-object = "CONTAINER"     THEN DO:

    if valid-handle(wh-cc0300a-responsavel) then do:
        assign wh-cc0300a-responsavel:SENSITIVE = no
               wh-cc0300a-responsavel:screen-value = c-seg-usuario.

        apply 'leave' to wh-cc0300a-responsavel.
    end.
END.

/* fim do programa */

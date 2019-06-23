/* --------------------------------------------------------------------------------------- *\
|                                                                                           |

|  Programa...............: cc0311-upc01.P                                                 |
|  Sub Programa...........:                                                                 |
|  Descricao..............: UPC INCLUS€O DE PEDIDO DE COMPRA                                |
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

define new global shared variable wh-cc0311-da-data-ped         as widget-handle no-undo.                                                                  
define new global shared variable wh-cc0311-tg-emergencial      as widget-handle no-undo.

DEFINE NEW GLOBAL SHARED VARIABLE l-emergencial-cc0311 AS LOGICAL   NO-UNDO.

define variable c-handle-obj        as character     no-undo.
define variable c-objeto            as character     no-undo. 

IF  p-ind-event  = "change-page" 
AND p-ind-object = "CONTAINER"
and p-wgh-frame:name = "f-pg-par" THEN DO:

    ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-frame:PRIVATE-DATA, "~/"), p-wgh-frame:PRIVATE-DATA, "~/").

    ASSIGN c-handle-obj                = fc-handle-obj("da-data-ped,tg-gera-proc-imp",p-wgh-frame).
    ASSIGN wh-cc0311-da-data-ped       = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
        NO-ERROR.


    IF VALID-HANDLE(wh-cc0311-da-data-ped) THEN DO:
       CREATE TOGGLE-BOX wh-cc0311-tg-emergencial               
       ASSIGN FRAME     = wh-cc0311-da-data-ped:FRAME      
              LABEL     = "Pedido Emergˆncial"             
              ROW       = wh-cc0311-da-data-ped:ROW   
              COL       = wh-cc0311-da-data-ped:COL + 20  
              WIDTH     = 20                              
              HEIGHT    = 0.4                              
              VISIBLE   = YES                              
              SENSITIVE = wh-cc0311-da-data-ped:SENSITIVE  
              CHECKED   = FALSE. 

       ASSIGN wh-cc0311-tg-emergencial:CHECKED = l-emergencial-cc0311.
    END.

end.

IF VALID-HANDLE(wh-cc0311-tg-emergencial) THEN DO:
   ASSIGN l-emergencial-cc0311 = wh-cc0311-tg-emergencial:CHECKED.
END.




/* fim do programa */

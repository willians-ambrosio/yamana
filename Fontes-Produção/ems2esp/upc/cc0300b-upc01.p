/* --------------------------------------------------------------------------------------- *\
|                                                                                           |

|  Programa...............: cc0300b-upc01.P                                                 |
|  Sub Programa...........:                                                                 |
|  Descricao..............: UPC INCLUSÇO DE PEDIDO DE COMPRA                                |
|  Entidade Desenvolvedora: DSC                                                             |
|                                                                                           |
|  Historico Programa -------------------------------------------------------------------+  |
|  | Data       | Autor               | Descricao                                        |  |
|  +----------- +---------------------+--------------------------------------------------+  |
|  | 22|05|2009 | Luiz CRUZ           | Desenvolvimento do Programa                      |  |
|  +------------+---------------------+--------------------------------------------------+  |
|  | 29|06|2009 | Luiz CRUZ           | Altera‡Æo no leave do campo It-codigo, para que  |  |
|  |            |                     |  que nao altere o campo Comprador                |  |
|  +------------+---------------------+--------------------------------------------------+  |
|  | Parametros :                                                                        |  |
|  |                                                                                     |  |
|  | Observacao :                                                                        |  |
|  |                                                                                     |  |
|  +-------------------------------------------------------------------------------------+  |
\  --------------------------------------------------------------------------------------- */
{include/i-prgvrs.i cc0300b-upc01.p 2.06.00.002}

{tools/fc-handle-obj.i}
{tools/fc-falso.i}       
{utp/ut-glob.i}

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO. 
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE wh-cc0300b-comprador           AS WIDGET-HANDLE NO-UNDO.                                                                  
DEFINE NEW GLOBAL SHARED VARIABLE wh-cc0300b-comprador-fc        AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cc0300b-c-desc-comprador    AS WIDGET-HANDLE NO-UNDO.                                                                  
DEFINE NEW GLOBAL SHARED VARIABLE wh-cc0300b-c-desc-comprador-fc AS WIDGET-HANDLE NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE wh-cc0300b-requisitante AS WIDGET-HANDLE NO-UNDO.

DEFINE VARIABLE  c-handle-obj  AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-objeto       AS CHARACTER NO-UNDO. 

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-frame:PRIVATE-DATA, "~/"), p-wgh-frame:PRIVATE-DATA, "~/").
/*                        
message "ind-event"  p-ind-event    skip
        "ind-object" p-ind-object   skip
        "wgh-object" p-wgh-object   skip
        "wgh-frame"  p-wgh-frame    skip
        "frame-name " p-wgh-frame:NAME SKIP
        "cod-table"  p-cod-table    skip
        "row-table"  string(p-row-table)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.  
  */  
    
IF  p-ind-event  = "BEFORE-INITIALIZE" 
AND p-ind-object = "CONTAINER" THEN DO:

  ASSIGN c-handle-obj = fc-handle-obj("cod-comprado,requisitante,it-codigo,c-desc-comprador",p-wgh-frame).
  ASSIGN 
    wh-cc0300b-comprador     = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
 wh-cc0300b-requisitante     = WIDGET-HANDLE(ENTRY(2,c-handle-obj))
 wh-cc0300b-c-desc-comprador = WIDGET-HANDLE(ENTRY(4,c-handle-obj)).

  CREATE FILL-IN wh-cc0300b-comprador-fc
  ASSIGN FRAME              = wh-cc0300b-comprador:FRAME                                 
         DATA-TYPE          = wh-cc0300b-comprador:DATA-TYPE                       
         FORMAT             = wh-cc0300b-comprador:FORMAT                        
         WIDTH              = wh-cc0300b-comprador:WIDTH                                          
         HEIGHT             = wh-cc0300b-comprador:HEIGHT                                        
         ROW                = wh-cc0300b-comprador:ROW                  
         COL                = wh-cc0300b-comprador:COL
         VISIBLE            = YES
         SENSITIVE          = wh-cc0300b-comprador:SENSITIVE.

  CREATE FILL-IN wh-cc0300b-c-desc-comprador-fc
  ASSIGN FRAME              = wh-cc0300b-c-desc-comprador:FRAME                                 
         DATA-TYPE          = wh-cc0300b-c-desc-comprador:DATA-TYPE                       
         FORMAT             = wh-cc0300b-c-desc-comprador:FORMAT                        
         WIDTH              = wh-cc0300b-c-desc-comprador:WIDTH                                          
         HEIGHT             = wh-cc0300b-c-desc-comprador:HEIGHT                                        
         ROW                = wh-cc0300b-c-desc-comprador:ROW                  
         COL                = wh-cc0300b-c-desc-comprador:COL
         VISIBLE            = YES        
         SENSITIVE          = wh-cc0300b-c-desc-comprador:SENSITIVE.
END.

IF p-ind-event  = "AFTER-INITIALIZE" AND
   p-ind-object = "CONTAINER"    THEN DO:

    IF VALID-HANDLE(wh-cc0300b-comprador) THEN DO:

        FIND usuar_mestre WHERE usuar_mestre.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.
   
        ASSIGN wh-cc0300b-comprador:SENSITIVE              = NO
               wh-cc0300b-comprador:SCREEN-VALUE           = c-seg-usuario
               wh-cc0300b-comprador-fc:SCREEN-VALUE        = c-seg-usuario
               wh-cc0300b-c-desc-comprador-fc:SCREEN-VALUE = IF AVAIL usuar_mestre THEN usuar_mestre.nom_usuario ELSE "".

        APPLY 'LEAVE' TO wh-cc0300b-comprador.

    END.
END.
IF p-ind-event = "BEFORE-SAVE-FIELDS" AND 
   p-ind-object = "CONTAINER"    THEN DO:

    ASSIGN wh-cc0300b-comprador:SCREEN-VALUE = c-seg-usuario.
    
END.
RETURN 'OK'.

/* fim do programa */

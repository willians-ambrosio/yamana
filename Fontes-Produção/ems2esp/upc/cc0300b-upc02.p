/* --------------------------------------------------------------------------------------- *\
|                                                                                           |

|  Programa...............: cc0300b-upc02.P                                                 |
|  Sub Programa...........:                                                                 |
|  Descricao..............: UPC INCLUSÇO DE PEDIDO DE COMPRA                                |
|  Entidade Desenvolvedora: DSC                                                             |
|                                                                                           |
|  |
|  |
|  |
|  |
|  |
|  |
|  |
|  |
|  |
|  |
|  |
|  |
|  |
\  --------------------------------------------------------------------------------------- */
{include/i-prgvrs.i cc0300b-upc02.p 2.06.00.002}

{tools/fc-handle-obj.i}
{tools/fc-falso.i}       
{utp/ut-glob.i}

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO. 
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.



DEFINE VARIABLE  c-handle-obj  AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-objeto       AS CHARACTER NO-UNDO. 

DEF TEMP-TABLE tt-beneficio
    FIELD cod-beneficio AS INT
    FIELD rid-ben       AS ROWID
    FIELD tp-tax        AS CHAR
    FIELD prior         AS INT.


DEFINE VARIABLE c-hora AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-data AS CHARACTER   NO-UNDO.



ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-frame:PRIVATE-DATA, "~/"), p-wgh-frame:PRIVATE-DATA, "~/").
                       
/* message "ind-event"  p-ind-event    skip    */
/*         "ind-object" p-ind-object   skip    */
/*         "wgh-object" p-wgh-object   skip    */
/*         "wgh-frame"  p-wgh-frame    skip    */
/*         "frame-name " p-wgh-frame:NAME SKIP */
/*         "cod-table"  p-cod-table    skip    */
/*         "row-table"  string(p-row-table)    */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.      */





DEFINE NEW GLOBAL SHARED VAR wh-cc0300b-it-codigo   AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wh-cc0300b-numero-ordem   AS WIDGET-HANDLE NO-UNDO.


RUN findWidget (INPUT "it-codigo",        INPUT "FILL-IN",    INPUT p-wgh-frame, OUTPUT wh-cc0300b-it-codigo).
RUN findWidget (INPUT "numero-ordem",     INPUT "FILL-IN",    INPUT p-wgh-frame, OUTPUT wh-cc0300b-numero-ordem).





















  

IF p-ind-event  = "before-piSaveAndOK"THEN DO:


   


    FIND ordem-compra WHERE ordem-compra.numero-ordem = INT(wh-cc0300b-numero-ordem:SCREEN-VALUE)
        NO-LOCK NO-ERROR.

         RUN esp\procura-beneficio.p (INPUT ordem-compra.cod-estabel,                 
                                      INPUT ordem-compra.it-codigo,                   
                                      INPUT ordem-compra.cod-emitente,    
                                      OUTPUT TABLE tt-beneficio ).  




         FOR EACH TT-BENEFICIO  BREAK BY  TT-BENEFICIO.tp-tax BY TT-BENEFICIO.PRIOR:


              IF FIRST-OF(TT-BENEFICIO.tp-tax) THEN DO:
            
             FIND es-ben-estab WHERE ROWID(es-ben-estab) = TT-BENEFICIO.rid-ben NO-LOCK NO-ERROR.

             IF AVAIL es-ben-estab THEN DO:

                 IF es-ben-estab.compras-comprador-icms <> 0 AND TT-BENEFICIO.tp-tax = "ICMS" THEN DO:
                     FIND es-mensagem-ben WHERE es-mensagem-ben.cod-mensagem = es-ben-estab.compras-comprador-icms NO-LOCK NO-ERROR.
                     IF AVAIL es-mensagem-ben THEN DO:
                         run utp/ut-msgs.p (input "show",                                                                                              
                                           input 17006,                                                                                                      
                                           input es-mensagem-ben.desc-mensagem +  "~~" + es-mensagem-ben.narrativa ).
                     END.
                 END.
                 IF  es-ben-estab.compras-comprador-pis-confis <> 0  AND TT-BENEFICIO.tp-tax = "PIS" THEN DO:

                   

                      FIND es-mensagem-ben WHERE es-mensagem-ben.cod-mensagem = es-ben-estab.compras-comprador-pis-confis NO-LOCK NO-ERROR.   
                      IF AVAIL es-mensagem-ben THEN DO:                                                                                 
                          run utp/ut-msgs.p (input "show",                                                                              
                                             input 17006,                                                                               
                                             input es-mensagem-ben.desc-mensagem +  "~~" + es-mensagem-ben.narrativa ).                 
                      END.                                                                                                              
                 END.

                 ASSIGN c-hora = STRING(TIME,"hh:mm").                                      
                      c-data = STRING(TODAY,"99/99/9999").                                  
                 CREATE ext-ordem-compra.                                                      
                 ASSIGN ext-ordem-compra.numero-ordem = ordem-compra.numero-ordem              
                        ext-ordem-compra.usuario   = c-seg-usuario                          
                        ext-ordem-compra.data      = DATE(c-data)                              
                        ext-ordem-compra.hora      = c-hora 
                        ext-ordem-compra.cod-mensagem = es-mensagem-ben.cod-mensagem
                        ext-ordem-compra.it-codigo = ordem-compra.it-codigo                    
                        ext-ordem-compra.mensagem  = es-mensagem-ben.narrativa.
             END.
                END.
          END.




























END.








RETURN 'OK'.


PROCEDURE findWidget:
    /*
    * PARAMETROS:
    *   c-widget-name:  nome do widget a ser localizado
    *   c-widget-type:  tipo do widget a ser localizado
    *   h-start-widget: container para procurar o widget
    *   h-widget:       widget encontrado 
    */

    define input  parameter c-widget-name  as char   no-undo.
    define input  parameter c-widget-type  as char   no-undo.
    define input  parameter h-start-widget as handle no-undo.
    define output parameter h-widget       as handle no-undo.

    do while valid-handle(h-start-widget):
        if h-start-widget:name = c-widget-name and
           h-start-widget:type = c-widget-type then do:
            assign h-widget = h-start-widget:handle.
            leave.
        end.

        if h-start-widget:type = "field-group":u or
           h-start-widget:type = "frame":u or
           h-start-widget:type = "dialog-box":u then do:
            run findWidget (input  c-widget-name,
                            input  c-widget-type,
                            input  h-start-widget:first-child,
                            output h-widget).

            if valid-handle(h-widget) then
                leave.
        end.
        assign h-start-widget = h-start-widget:next-sibling.
    end.
END PROCEDURE.

procedure pi-msg:
message "p-ind-event..:" p-ind-event                  skip 
        "p-ind-object.:" p-ind-object                 skip 
        "p-cod-table..:" STRING(p-cod-table)          skip 
        "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    skip 
        "p-wgh-frame..:" STRING(p-wgh-frame)          skip 
        "p-row-table..:" string(p-row-table)          skip 
        VIEW-AS ALERT-BOX INFO BUTTONS OK.                    
END PROCEDURE.




















/* fim do programa */

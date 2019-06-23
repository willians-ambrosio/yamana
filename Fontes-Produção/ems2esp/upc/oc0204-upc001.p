
/**============================================================**
** Altera‡Æo...:  
** Empresa.....:  
** Data........: 
** Objetivo....:  
** ............:  
**=============================================================**/
/*{include/i-prgvrs.i oc0204-upc001 2.06.00.002}*/

{tools/fc-handle-obj.i}
{tools/fc-falso.i}       
/*{utp/ut-glob.i}*/

/** Parƒmetros **/                                

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.


DEF VAR c-objeto      AS CHAR          NO-UNDO.
DEF VAR h_frame       AS WIDGET-HANDLE NO-UNDO.
DEF VAR c-nr-processo AS CHAR          NO-UNDO.

DEF TEMP-TABLE tt-beneficio
    FIELD cod-beneficio AS INT
    FIELD rid-ben       AS ROWID
    FIELD tp-tax        AS CHAR
    FIELD prior         AS INT.

DEFINE VARIABLE c-hora AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-data AS CHARACTER   NO-UNDO.

define new global shared variable wh_oc0204_query     AS WIDGET-HANDLE NO-UNDO.                 
define new global shared variable wh_oc0204_browse    AS WIDGET-HANDLE NO-UNDO.

define new global shared variable wh_oc0204_buffer    AS WIDGET-HANDLE NO-UNDO.
define new global shared variable wh_oc0204_nr_ordem  AS WIDGET-HANDLE NO-UNDO.

define new global shared variable wh-oc0204_bt_conf   as WIDGET-HANDLE NO-UNDO. 
define new global shared variable wh-oc0204_btn_conf  as WIDGET-HANDLE NO-UNDO. 

    /*message "p-ind-event..:" p-ind-event                  skip 
        "p-ind-object.:" p-ind-object                 skip 
        "p-cod-table..:" STRING(p-cod-table)          skip 
        "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    skip 
        "p-wgh-frame..:" STRING(p-wgh-frame)          skip 
        "p-row-table..:" string(p-row-table)          skip 
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

IF p-ind-event  = "AFTER-OPEN-QUERY"    AND 
   p-ind-object = "browser"     THEN do:


    ASSIGN h_Frame = p-wgh-frame:FIRST-CHILD. 
    ASSIGN h_Frame = h_Frame:FIRST-CHILD. 

    

    DO WHILE h_Frame <> ? :

        
        IF h_Frame:NAME = "br_table" THEN DO:

            ASSIGN wh_oc0204_browse = h_frame:HANDLE.
            Assign wh_oc0204_query  = wh_oc0204_browse:Query.
        
        END.
        

        if h_frame:type <> "field-group" then do:

            IF  h_Frame:name = "bt-conf" THEN DO:
                  ASSIGN wh-oc0204_bt_conf = h_frame:HANDLE.      
            end.                                 
            ASSIGN h_Frame = h_Frame:NEXT-SIBLING.
        END.
        else do:
            assign h_frame = h_frame:FIRST-CHILD.
        end.

    END.

END.

IF VALID-HANDLE(wh-oc0204_bt_conf) THEN DO:


    CREATE BUTTON wh-oc0204_btn_conf
    ASSIGN FRAME      = wh-oc0204_bt_conf:FRAME
    ROW               = wh-oc0204_bt_conf:ROW
    COL               = wh-oc0204_bt_conf:COL
    FONT              = wh-oc0204_bt_conf:FONT
    LABEL             = wh-oc0204_bt_conf:LABEL
    WIDTH             = wh-oc0204_bt_conf:WIDTH
    HEIGHT            = wh-oc0204_bt_conf:HEIGHT
    NAME              = "wh-oc0204_bt_conf"
    TOOLTIP           = "*Confirma"
    VISIBLE           = YES
    SENSITIVE         = TRUE
        TRIGGERS:
           ON CHOOSE PERSISTENT RUN upc/oc0204-upc001.p(INPUT "choose",
                                                        INPUT "wh-oc0204_btn_conf",
                                                        INPUT p-wgh-object,
                                                        INPUT p-wgh-frame,
                                                        INPUT p-cod-table,
                                                        INPUT p-row-table).

        END TRIGGERS.
    IF wh-oc0204_btn_conf:MOVE-TO-TOP() THEN .

END.

IF  p-ind-event  = "choose"
AND p-ind-object = "wh-oc0204_btn_conf" THEN DO:

    wh_oc0204_buffer = wh_oc0204_query:GET-BUFFER-HANDLE(1).
    wh_oc0204_nr_ordem = wh_oc0204_buffer:BUFFER-FIELD("numero-ordem").


    FIND FIRST ordem-compra NO-LOCK
         WHERE ordem-compra.numero-ordem = INT(wh_oc0204_nr_ordem:buffer-value) NO-ERROR.

         RUN esp\procura-beneficio.p (INPUT ordem-compra.cod-estabel,                 
                                      INPUT ordem-compra.it-codigo,                   
                                      INPUT ordem-compra.cod-emitente,    
                                      OUTPUT TABLE tt-beneficio ).  

         FIND FIRST tt-beneficio NO-LOCK NO-ERROR.
         

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
                     /*CREATE ext-ordem-compra.                                                      
                     ASSIGN ext-ordem-compra.numero-ordem = ordem-compra.numero-ordem              
                            ext-ordem-compra.usuario   = USER("ems2cadme")                          
                            ext-ordem-compra.data      = DATE(c-data)                              
                            ext-ordem-compra.hora      = c-hora 
                            ext-ordem-compra.cod-mensagem = es-mensagem-ben.cod-mensagem
                            ext-ordem-compra.it-codigo = ordem-compra.it-codigo                    
                            ext-ordem-compra.mensagem  = es-mensagem-ben.narrativa.*/
                 END.
             END.
         END.

        APPLY "choose" TO wh-oc0204_bt_conf.

END.


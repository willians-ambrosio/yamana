/*****************************************************************************
 ** PROGRAMA..: ESCD0300a-U01.P
 ** OBJETIVO..: UPC NA MANUTEN€ÇO PEDIDOS DE COMPRA - CC0300A
 ** AUTOR.....: DSC
 ** CLIENTE...: YAMANA
 ** VERSAO....: 2.00.00.000 - 21/05/2009 - Luiz CRUZ.
 ** ALTERA€åES:
 ******************************************************************************/

/*************************** PAR¶METROS PADRÇO *************************************/
DEFINE INPUT PARAMETER p-ind-event   AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object  AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object  AS HANDLE         NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame   AS WIDGET-HANDLE  NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table   AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-row-table   AS ROWID          NO-UNDO.

/*************************** GLOBAL VARIABLE DEFINITIONS **************************/
DEFINE NEW GLOBAL SHARED VARIABLE wh-cc0300a-cod-tipo         AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cc0300a-label-cod-tipo   AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cc0300a-cod-priori       AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cc0300a-label-cod-priori AS WIDGET-HANDLE  NO-UNDO.
define new global shared variable wh-cc0300a-label-desc-verba   as widget-handle  no-undo.
define new global shared variable wh-cc0300a-desc-verba         as widget-handle  no-undo.
define new global shared variable wh-cc0300a-label-nr-ped-verba as widget-handle  no-undo.
define new global shared variable wh-cc0300a-nr-ped-verba       as widget-handle  no-undo.
define new global shared variable wh-cc0300a-responsavel        as widget-handle  no-undo.
define new global shared variable wh-cc0300a-responsavel-fc     as widget-handle  no-undo.


DEFINE NEW GLOBAL SHARED VARIABLE h-fpage4                    AS WIDGET-HANDLE  NO-UNDO. 

DEFINE NEW GLOBAL SHARED VARIABLE c-pedido                    AS INTEGER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE c-emitente                  AS INTEGER NO-UNDO.


/*************************** LOCAL VARIABLE DEFINITIONS **************************/
DEFINE VARIABLE c-objeto       AS CHARACTER      NO-UNDO.
DEFINE VARIABLE c-objects      AS CHARACTER      NO-UNDO.
DEFINE VARIABLE i-objects      AS INTEGER        NO-UNDO.
DEFINE VARIABLE h-frame        AS WIDGET-HANDLE  NO-UNDO. 
DEFINE VARIABLE wh-objeto      AS WIDGET-HANDLE  NO-UNDO.
DEFINE VARIABLE wgh-child      AS WIDGET-HANDLE  NO-UNDO.
DEFINE VARIABLE c-field        AS WIDGET-HANDLE  NO-UNDO.

DEFINE VARIABLE h-handle          AS HANDLE NO-UNDO.
DEFINE VARIABLE h-handle-pedido   AS HANDLE NO-UNDO.
DEFINE VARIABLE h-handle-emitente AS HANDLE NO-UNDO.
DEFINE VARIABLE h-object          AS HANDLE NO-UNDO.
DEFINE VARIABLE h-objeto          AS HANDLE NO-UNDO.

IF p-wgh-object <> ? THEN
    ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:FILE-NAME,'/'),
                                        p-wgh-object:FILE-NAME,'/').


/*  IF c-objeto     = "cc0300a.w" THEN  

  MESSAGE "Evento..........:"   string(p-ind-event)    skip
            "Objeto..........:" string(p-ind-object)   skip
            "Handle do Objeto:" string(p-wgh-object)   skip
            "Handle da Frame.:" string(p-wgh-frame)    skip
            "Tabela..........:" p-cod-table            skip
            "Rowid...........:" string(p-row-table)    skip
            "p-wgh-object....:" p-wgh-object:file-name skip
            "c-objeto..........:" c-objeto view-as alert-box.
                                        */
                
FUNCTION buscarHandleCampo RETURNS WIDGET-HANDLE (INPUT pcCampo   AS CHARACTER,
                                                  INPUT whPointer AS WIDGET-HANDLE).   


    DEF VAR wh-grupo AS WIDGET-HANDLE NO-UNDO.
    DEF VAR wh-child AS WIDGET-HANDLE NO-UNDO .
    DEF VAR l-prim-frame AS LOG NO-UNDO INIT YES.

    IF whPointer <> ? THEN 
        wh-grupo = whPointer:FIRST-CHILD.
    ELSE 
        wh-grupo = p-wgh-frame:FIRST-CHILD.
    
    DO WHILE VALID-HANDLE(wh-grupo):
        IF  wh-grupo:NAME = "f-main" THEN DO:
            IF  l-prim-frame = yes THEN 
                ASSIGN l-prim-frame = NO.
            ELSE DO:
                ASSIGN wh-grupo = wh-grupo:FIRST-CHILD.
            END.
        END. 

        CASE wh-grupo:NAME:
            WHEN pcCampo THEN DO:
                RETURN wh-grupo.
            END.
        END.

        IF wh-grupo:TYPE = "field-group" THEN 
            wh-grupo = wh-grupo:FIRST-CHILD.
        ELSE 
            wh-grupo = wh-grupo:NEXT-SIBLING.
    END.

END FUNCTION.



IF  p-ind-event  = "AFTER-INITIALIZE"
AND c-objeto     = 'cc0300a.w'
AND p-ind-object = "CONTAINER" THEN DO:

    run tela-upc(input p-wgh-frame,      
                 input p-ind-event,      
                 input p-wgh-object:type,
                 input p-wgh-object:name,
                 input no,             
                 output p-wgh-object).


    /*
    if p-wgh-frame =  then 

    DO WHILE VALID-HANDLE(wh-cc0300a-responsavel): 
        if wh-cc0300a-responsavel:name = "responsavel" then do:
        
            CREATE FILL-IN  wh-cc0300a-responsavel-fc
            ASSIGN FRAME             = wh-cc0300a-responsavel:FRAME                               
                   LABEL             = wh-cc0300a-responsavel:LABEL            
                   DATA-TYPE         = wh-cc0300a-responsavel:DATA-TYPE        
                   FORMAT            = wh-cc0300a-responsavel:FORMAT           
                   WIDTH             = wh-cc0300a-responsavel:WIDTH            
                   ROW               = wh-cc0300a-responsavel:ROW              
                   COL               = wh-cc0300a-responsavel:COL              
                   HEIGHT            = wh-cc0300a-responsavel:HEIGHT           
                   FONT              = wh-cc0300a-responsavel:FONT             
                   visible           = yes
                   sensitive         = no.
                                                                                        
        end.
    end.
    
                           */
END.

/*
IF  p-ind-event  = "BEFORE-ASSIGN" 
AND p-ind-object = "CONTAINER"
AND c-objeto     = "cc0300a.w"     THEN DO:
    ASSIGN wgh-child = p-wgh-frame:FIRST-CHILD.                                                  
    ASSIGN wgh-child = wgh-child:FIRST-CHILD.                                                      
    DO WHILE VALID-HANDLE(wgh-child):
        IF wgh-child:TYPE = "fill-in" THEN DO:
            IF wgh-child:NAME = "num-pedido" THEN  
                ASSIGN h-handle-pedido = wgh-child:HANDLE.    
            IF wgh-child:NAME = "cod-emitente" THEN  
                ASSIGN h-handle-emitente = wgh-child:HANDLE.     
        END.
        ASSIGN wgh-child = wgh-child:NEXT-SIBLING.
    END. 

    IF VALID-HANDLE(h-handle-pedido) THEN 
        ASSIGN c-pedido = INT(h-handle-pedido:SCREEN-VALUE).
    IF VALID-HANDLE(h-handle-emitente) THEN 
        ASSIGN c-emitente = INT(h-handle-emitente:SCREEN-VALUE).

END.

IF  p-ind-event  = "AFTER-ASSIGN"
AND p-ind-object = "CONTAINER" 
AND c-objeto     = "cc0300a.w" THEN DO:
    FIND first pedido-compr 
        WHERE pedido-compr.num-pedido = c-pedido EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL pedido-compr THEN DO:
    END.
        
END.

IF (p-ind-event  = "AFTER-CHANGE-PAGE" 
OR  p-ind-event   = "AFTER-DISPLAY") 
AND c-objeto     = 'cc0300a.w'
AND p-ind-object = "CONTAINER" THEN DO:
    ASSIGN wgh-child = p-wgh-frame:FIRST-CHILD.                                                  
    ASSIGN wgh-child = wgh-child:FIRST-CHILD.                                                      
    DO WHILE VALID-HANDLE(wgh-child):
        IF wgh-child:TYPE = "fill-in" THEN DO:
            IF wgh-child:NAME = "num-pedido" THEN  
                ASSIGN h-handle-pedido = wgh-child:HANDLE.    
            IF wgh-child:NAME = "cod-emitente" THEN  
                ASSIGN h-handle-emitente = wgh-child:HANDLE.     
        END.
        ASSIGN wgh-child = wgh-child:NEXT-SIBLING.
    END. 

    IF VALID-HANDLE(h-handle-pedido) THEN 
        ASSIGN c-pedido = INT(h-handle-pedido:SCREEN-VALUE).
    IF VALID-HANDLE(h-handle-emitente) THEN 
        ASSIGN c-emitente = INT(h-handle-emitente:SCREEN-VALUE).

    
        IF VALID-HANDLE(wh-cc0300a-cod-tipo) THEN 
            ASSIGN wh-cc0300a-cod-tipo:SCREEN-VALUE     = "A"
                   wh-cc0300a-cod-priori:SCREEN-VALUE   = "B"
                   wh-cc0300a-nr-ped-verba:screen-value = "C"
                   wh-cc0300a-desc-verba:checked        = no.  
    
END.
*/

IF (p-ind-event  = "BEFORE-CHANGE-PAGE" 
/*OR  p-ind-event  = "BEFORE-ENABLE" */) 
/*AND c-objeto     = 'cc0300a.w'*/

AND p-ind-object = "CONTAINER" THEN DO:
    

END.


PROCEDURE tela-upc:
    DEFINE INPUT  PARAMETER  pWghFrame    AS WIDGET-HANDLE NO-UNDO.  
    DEFINE INPUT  PARAMETER  pIndEvent    AS CHARACTER     NO-UNDO.      
    DEFINE INPUT  PARAMETER  pObjType     AS CHARACTER     NO-UNDO.  
    DEFINE INPUT  PARAMETER  pObjName     AS CHARACTER     NO-UNDO.   
    DEFINE INPUT  PARAMETER  pApresMsg    AS LOGICAL       NO-UNDO.  
    DEFINE OUTPUT PARAMETER  phObj        AS HANDLE        NO-UNDO.  
    
    DEFINE VARIABLE wgh-obj AS WIDGET-HANDLE NO-UNDO.

    ASSIGN wgh-obj = pWghFrame:FIRST-CHILD.

    DO  WHILE VALID-HANDLE(wgh-obj): 
        
        IF pApresMsg = YES and wgh-obj:name = "responsavel" then 
            do:

            assign wh-cc0300a-responsavel = wgh-obj.
                MESSAGE "Nome do Objeto" wgh-obj:NAME SKIP
                        "Type do Objeto" wgh-obj:TYPE SKIP
                        "P-Ind-Event"    pIndEvent VIEW-AS ALERT-BOX.
            end.
        
        IF wgh-obj:TYPE = pObjType AND
           wgh-obj:NAME = pObjName THEN DO:
            ASSIGN phObj = wgh-obj:HANDLE.
            LEAVE.
        END.
        IF wgh-obj:TYPE = "field-group" THEN
            ASSIGN wgh-obj = wgh-obj:FIRST-CHILD.
        ELSE
            if wgh-obj:TYPE = "frame" and
               wgh-obj:name = "fpage1" then 
                ASSIGN wgh-obj = wgh-obj:FIRST-CHILD.
            else
                ASSIGN wgh-obj = wgh-obj:NEXT-SIBLING.

    END.

END PROCEDURE. 

        
        





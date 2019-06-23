/***************************************************************************************
** Programa: mv0701-upc.p
** Objetivo: Colocar botao em tela para chamar o programa ESMV0701
** Autor...: Daniel Steiner de Sousa
** Data....: 07/2008
****************************************************************************************
** Objetivo: Migra‡Æo para 2.06
** Autor...: Thiago Coutinho
** Data....: Mar‡o/2012

****************************************************************************************/

{include/i-prgvrs.i mv0608-upc 2.06.00.000}

DEFINE INPUT PARAMETER p-ind-event                          AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object                         AS CHARACTE       NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object                         AS HANDLE         NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame                          AS WIDGET-HANDLE  NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table                          AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-row-table                          AS ROWID          NO-UNDO.
/********************************* WIDGET-HANDLE **************************************/
DEFINE NEW GLOBAL SHARED VARIABLE wh-btTecnico-mv0608       AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-nr-ord-produ-mv0608    AS WIDGET-HANDLE  NO-UNDO.      
DEFINE NEW GLOBAL SHARED VARIABLE h-objeto                  AS HANDLE         NO-UNDO.



/** Ao iniciar a tela, eventos para campos espec¡ficos **/ 
if p-ind-event = "AFTER-INITIALIZE":U then do: 

    RUN findWidget (INPUT "nr-ord-produ", 
                    INPUT "FILL-IN", 
                    INPUT p-wgh-frame, 
                    OUTPUT wh-nr-ord-produ-mv0608).

   

    /** Botao T‚cnico **/
    CREATE BUTTON wh-btTecnico-mv0608
    ASSIGN FRAME     = p-wgh-frame
           LABEL     = "&Teste":U
           WIDTH     = 4
           HEIGHT    = 1.18
           ROW       = 1.13
           COL       = 70.72
           FONT      = 1
           VISIBLE   = yes
           SENSITIVE = YES
    TRIGGERS:   
        on choose persistent run mvp\esmv0701.w.
    END TRIGGERS. 
    wh-btTecnico-mv0608:load-image("image/toolbar/im-prigr.bmp").


    
END. /* After-Initialize */ 


PROCEDURE findWidget:
    
    DEFINE INPUT  PARAMETER c-widget-name  AS CHAR   NO-UNDO.
    DEFINE INPUT  PARAMETER c-widget-type  AS CHAR   NO-UNDO.
    DEFINE INPUT  PARAMETER h-start-widget AS HANDLE NO-UNDO.
    DEFINE OUTPUT PARAMETER h-widget       AS HANDLE NO-UNDO.
    
    DO WHILE VALID-HANDLE(h-start-widget):
        if h-start-widget:NAME = c-widget-name AND
           h-start-widget:TYPE = c-widget-type THEN DO:
            ASSIGN h-widget = h-start-widget:HANDLE.
            LEAVE.
        END.
    
        IF h-start-widget:TYPE = "field-group":u OR
           h-start-widget:TYPE = "frame":u OR
           h-start-widget:TYPE = "dialog-box":u THEN DO:
            RUN findWidget (INPUT  c-widget-name,
                            INPUT  c-widget-type,
                            INPUT  h-start-widget:FIRST-CHILD,
                            OUTPUT h-widget).
    
            IF VALID-HANDLE(h-widget) THEN
                LEAVE.
        END.
        ASSIGN h-start-widget = h-start-widget:NEXT-SIBLING.
    END.
END PROCEDURE.

return "OK":U.


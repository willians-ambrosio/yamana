/*{utp/ut-glob.i}*/

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO. 
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

/*def new global shared var gr-documento  as rowid no-undo.*/


DEFINE NEW GLOBAL SHARED VAR wh-ab0306-btdel           AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wh-ab0306-btdelesp        AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wh-cs0102-valor-base      AS WIDGET-HANDLE NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE wh-ab0306-fi-ct-codigo AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-ab0306-fi-cc-codigo AS WIDGET-HANDLE NO-UNDO.

RUN findWidget (INPUT "btDelete",         INPUT "button",    INPUT p-wgh-frame, OUTPUT wh-ab0306-btdel).
RUN findWidget (INPUT "dat-movto",        INPUT "FILL-IN",   INPUT p-wgh-frame, OUTPUT wh-cs0102-valor-base).
RUN findWidget (INPUT "fi-ct-codigo",     INPUT "FILL-IN",   INPUT p-wgh-frame, OUTPUT wh-ab0306-fi-ct-codigo).
RUN findWidget (INPUT "fi-cc-codigo",     INPUT "FILL-IN",   INPUT p-wgh-frame, OUTPUT wh-ab0306-fi-cc-codigo).

/*message "ind-event"  p-ind-event    skip   
        "ind-object" p-ind-object   skip   
        "wgh-object" p-wgh-object   skip   
        "wgh-frame"  p-wgh-frame    skip   
        "frame-name " p-wgh-frame:NAME SKIP
        "cod-table"  p-cod-table    skip   
        "row-table"  string(p-row-table)    skip
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

IF p-ind-event = "btdelete-ab0306" THEN DO:
    FIND FIRST param-estoq NO-LOCK NO-ERROR.
    IF AVAIL(param-estoq) THEN DO:
        IF date(wh-cs0102-valor-base:SCREEN-VALUE) <= param-estoq.contab-ate THEN DO:

            RUN utp/ut-msgs.p (INPUT "SHOW",
                               INPUT 17006,
                               INPUT "Per¡odo j  encerrado!~~Movimento nÆo pode ser eliminado.").
                
            RETURN "NOK".


        END.
        ELSE DO:
            APPLY "choose" TO wh-ab0306-btdel.
        END.
    END.                                  


   

END.

IF  p-ind-event  = "BEFORE-INITIALIZE":U  THEN DO:
    IF VALID-HANDLE(wh-ab0306-btdel ) THEN DO:
        create button wh-ab0306-btdelesp
        assign frame        = wh-ab0306-btdel:frame
               width        = wh-ab0306-btdel:width 
               height       = wh-ab0306-btdel:height
               row          = wh-ab0306-btdel:row
               col          = wh-ab0306-btdel:col 
               visible      = yes
               sensitive    = YES
               triggers:
                    ON CHOOSE persistent run upc/ab0306-u01.p   (input "btdelete-ab0306",
                                                                 input p-ind-object ,
                                                                 input p-wgh-object ,
                                                                 input p-wgh-frame  ,
                                                                 input p-cod-table  ,
                                                                 input p-row-table  ).
               end triggers.
               
        wh-ab0306-btdelesp:LOAD-IMAGE-UP(wh-ab0306-btdel:IMAGE-UP).
        wh-ab0306-btdelesp:LOAD-IMAGE-INSENSITIVE(wh-ab0306-btdel:IMAGE-INSENSITIVE).
        wh-ab0306-btdel:VISIBLE = NO.
    END.
    
END.

/*
IF VALID-HANDLE(wh-ab0306-fi-ct-codigo) AND
   VALID-HANDLE(wh-ab0306-fi-cc-codigo) THEN DO:
   ASSIGN  wh-ab0306-fi-ct-codigo:SENSITIVE  = FALSE
           wh-ab0306-fi-cc-codigo:SENSITIVE = FALSE.
END.
*/


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

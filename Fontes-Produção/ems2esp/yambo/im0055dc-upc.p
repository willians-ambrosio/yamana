/*******************************************************************************
** Programa: im0055dc-upc.p
** Autor...: Log¡stica (log339640)
** Data....: 07/2008
** OBS.....: UPC utilizada pelo programa im0055dc
** Objetivo: Chamada da API de gera‡Æo de movimento e Grva‡Æo da a‡Æo tomada 
             pelo usu rio.
*******************************************************************************/

DEF INPUT PARAM p-ind-event  AS   CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object AS   CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object AS   HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame  AS   WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table  AS   CHAR          NO-UNDO.
DEF INPUT PARAM p-row-table  AS   ROWID         NO-UNDO.

DEFINE VARIABLE wh-objeto   AS   HANDLE        NO-UNDO.
DEFINE VARIABLE wgh-child   AS   HANDLE        NO-UNDO.
DEFINE VARIABLE wh-grupo    AS   WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE wh-child    AS   WIDGET-HANDLE NO-UNDO.
define variable wh-pesquisa as   widget-handle no-undo.

DEFINE NEW GLOBAL SHARED VAR wgh-btAddSon1-im0055dc       AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-btAddSon1UPC-im0055dc    AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-btMod-im0055dc           AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-btModUPC-im0055dc        AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-im0055dc-upc               AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-fPage1-im0055dc                   AS WIDGET-HANDLE NO-UNDO.

DEFINE NEW GLOBAL SHARED VAR l-inclui-im0055dc   as logical no-undo.
DEFINE NEW GLOBAL SHARED VAR l-modifica-im0055dc as logical no-undo.

IF  p-ind-event = "after-initialize":U THEN DO:
    RUN upc\im0055dc-upc.p PERSISTENT SET h-im0055dc-upc (INPUT "", INPUT "", INPUT p-wgh-object, INPUT p-wgh-frame, INPUT "", INPUT p-row-table).

    ASSIGN wh-grupo = p-wgh-frame:FIRST-CHILD.
    DO  WHILE VALID-HANDLE(wh-grupo):
        ASSIGN  wh-child = wh-grupo:FIRST-CHILD.
        DO  WHILE VALID-HANDLE(wh-child):
            CASE wh-child:TYPE:
                WHEN "frame":U THEN DO:
                    IF wh-child:NAME = "fPage1":U THEN 
                        ASSIGN wgh-fPage1-im0055dc = wh-child.
                END.
            END.
            ASSIGN wh-child = wh-child:NEXT-SIBLING.
        END. 
        ASSIGN wh-grupo = wh-grupo:NEXT-SIBLING.
    END.

    ASSIGN wh-grupo = wgh-fPage1-im0055dc:FIRST-CHILD. 
    DO  WHILE VALID-HANDLE(wh-grupo):
        ASSIGN  wh-child = wh-grupo:FIRST-CHILD.
        DO  WHILE VALID-HANDLE(wh-child):
            CASE wh-child:TYPE:
                WHEN "button":U THEN DO:
                    IF wh-child:NAME = "btAddSon1":U THEN 
                        ASSIGN wgh-btAddSon1-im0055dc = wh-child.
                    IF wh-child:NAME = "btMod":U THEN 
                        ASSIGN wgh-btMod-im0055dc = wh-child.
                END.
            END.
            ASSIGN wh-child = wh-child:NEXT-SIBLING.
        END. 
        ASSIGN wh-grupo = wh-grupo:NEXT-SIBLING.
    END.

    if valid-handle(wgh-btAddSon1-im0055dc   ) and  
       valid-handle(wgh-btMod-im0055dc       ) then do:

        create button wgh-btAddSon1UPC-im0055dc
        ASSIGN FRAME        = wgh-fPage1-im0055dc
               WIDTH        = wgh-btAddSon1-im0055dc:width
               HEIGHT       = wgh-btAddSon1-im0055dc:HEIGHT
               LABEL        = wgh-btAddSon1-im0055dc:label
               ROW          = wgh-btAddSon1-im0055dc:ROW
               COL          = wgh-btAddSon1-im0055dc:col
               FONT         = wgh-btAddSon1-im0055dc:FONT
               VISIBLE      = yes
               SENSITIVE    = yes.
    
        create button wgh-btModUPC-im0055dc
        ASSIGN FRAME        = wgh-fPage1-im0055dc
               WIDTH        = wgh-btMod-im0055dc:width
               HEIGHT       = wgh-btMod-im0055dc:HEIGHT
               LABEL        = wgh-btMod-im0055dc:label
               ROW          = wgh-btMod-im0055dc:ROW
               COL          = wgh-btMod-im0055dc:col
               FONT         = wgh-btMod-im0055dc:FONT
               VISIBLE      = yes
               SENSITIVE    = yes.
    
        wgh-btAddSon1UPC-im0055dc:MOVE-TO-TOP().
        wgh-btModUPC-im0055dc:MOVE-TO-TOP().
        
        on "CHOOSE" of wgh-btAddSon1UPC-im0055dc    persistent run pi-ADD-UPC in h-im0055dc-upc.
        on "CHOOSE" of wgh-btModUPC-im0055dc        persistent run pi-MOD-UPC in h-im0055dc-upc.
    end.

END.

IF  p-ind-event  = "AFTER-DESTROY-INTERFACE" THEN DO:
    if valid-handle(h-im0055dc-upc) then do:
        delete procedure h-im0055dc-upc.
        assign h-im0055dc-upc = ?.
    end.
end.

procedure pi-ADD-UPC:

    assign l-inclui-im0055dc   = yes
           l-modifica-im0055dc = no.

    apply "choose" to wgh-btAddSon1-im0055dc.

end procedure.

procedure pi-MOD-UPC:

    assign l-inclui-im0055dc   = no
           l-modifica-im0055dc = yes.

    apply "choose" to wgh-btMod-im0055dc.

end procedure.

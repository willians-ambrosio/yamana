/********************************************
*
* Programa: esut0001.p
* Objetivo: Buscar handle de um objeto a partir de seu nome e um handle parente.
* Autor...: Joao B. C. Bisneto
*
*********************************************/  
  
/* parametros */
DEF INPUT  PARAMETER p-wgh-frame AS WIDGET-HANDLE NO-UNDO.
DEF INPUT  PARAMETER p-nome      AS CHAR          NO-UNDO.
DEF OUTPUT PARAMETER p-wh        AS WIDGET-HANDLE NO-UNDO.

RUN busca_widget(p-wgh-frame, p-nome, OUTPUT p-wh).

PROCEDURE busca_widget.
    DEF INPUT  PARAMETER p-wgh-frame AS WIDGET-HANDLE NO-UNDO.
    DEF INPUT  PARAMETER p-nome      AS CHAR          NO-UNDO.
    DEF OUTPUT PARAMETER p-wh        AS WIDGET-HANDLE NO-UNDO.
    DEF              VAR wh-objeto   AS WIDGET-HANDLE NO-UNDO.

    IF p-wgh-frame:NAME = p-nome THEN DO:
        p-wh = p-wgh-frame.
        RETURN.
    END.

    ASSIGN wh-objeto     = p-wgh-frame:FIRST-CHILD.
    DO WHILE VALID-HANDLE(wh-objeto):
        IF INDEX(LIST-QUERY-ATTRS(wh-objeto),"NAME") > 0 THEN DO:
            IF wh-objeto:NAME = p-nome THEN DO:
                p-wh = wh-objeto.
                LEAVE.
            END.
        END.
        IF wh-objeto:TYPE = 'field-group' OR
           wh-objeto:TYPE = 'frame'       THEN DO:
            RUN busca_widget(wh-objeto, p-nome, OUTPUT p-wh).
            IF VALID-HANDLE(p-wh) THEN LEAVE.
        END.
        ASSIGN wh-objeto = wh-objeto:NEXT-SIBLING.
    END.
END PROCEDURE.

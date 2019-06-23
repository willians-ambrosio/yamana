DEFINE INPUT PARAM p-row-table    AS ROWID           NO-UNDO.

FIND FIRST ITEM WHERE ROWID(ITEM) =  p-row-table NO-ERROR.
IF AVAIL ITEM THEN
DO :    
    FIND FIRST ext-item-arq WHERE ext-item-arq.it-codigo = ITEM.it-codigo NO-ERROR.
    IF AVAIL ext-item-arq THEN
    DO:
        IF search(ext-item-arq.image-pdf) <> ? AND ext-item-arq.image-pdf <> "" THEN
        DO:
            DOS SILENT START value(ext-item-arq.image-pdf).
        END.
        ELSE
            MESSAGE "Arquivo PDF N∆o Cadastrado para Item" SKIP
                    "Ou Arquivo n∆o encontrado"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    ELSE
        MESSAGE "Nenhum Arquivo/Image cadastrado para este item"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

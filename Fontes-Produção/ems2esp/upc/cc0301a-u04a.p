/**============================================================**
** Altera‡Æo...: Cleilton Conte
** Empresa.....: DSC
** Data........: 25/02/2015
** Objetivo....: Disponibilizar de-para dos itens harmonizados
**=============================================================**/
{utp/ut-glob.i}

DEFINE INPUT  PARAMETER  h_objeto                    AS WIDGET-HANDLE NO-UNDO. 
DEFINE INPUT  PARAMETER  p-wgh-frame                 AS WIDGET-HANDLE NO-UNDO.

DEFINE VARIABLE h_frame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE h_campo AS WIDGET-HANDLE NO-UNDO.

DEFINE VARIABLE wh-pesquisa   AS WIDGET-HANDLE      NO-UNDO.  
DEFINE VARIABLE l-implanta    AS LOGICAL            NO-UNDO.

DEFINE BUFFER bitem FOR ITEM.

IF h_objeto:NAME = "it-codigo" THEN DO:

    FOR FIRST es-de-para-it-padr
        WHERE es-de-para-it-padr.ep-codigo      = i-ep-codigo-usuario
          AND es-de-para-it-padr.it-codigo-orig = SELF:SCREEN-VALUE NO-LOCK:
    END.
    IF AVAIL es-de-para-it-padr THEN
        run utp/ut-msgs.p (input "show",  
                           input 27979,
                           input "Item padronizado~~" + 
                                 "O item " + QUOTER(SELF:SCREEN-VALUE) + " foi padronizado e seu novo c¢digo ‚ " +
                                 QUOTER(es-de-para-it-padr.it-codigo-padr)).
    FOR FIRST bitem FIELDS(cod-obsoleto)
        WHERE bitem.it-codigo = SELF:SCREEN-VALUE NO-LOCK:
    END.
    IF AVAIL es-de-para-it-padr AND
       (NOT AVAIL bitem         OR
        (AVAIL bitem AND bitem.cod-obsoleto = 4)) THEN DO:
        ASSIGN SELF:SCREEN-VALUE = es-de-para-it-padr.it-codigo-padr.
/*         APPLY "ENTRY" TO SELF. */
    END.

END.
IF h_objeto:NAME = "bt-depara" THEN DO:

    /*Identificar campo item */
    ASSIGN h_frame = p-wgh-frame:FIRST-CHILD
           h_frame = h_frame:FIRST-CHILD.
    DO WHILE h_frame <> ?:
       IF h_frame:NAME = "it-codigo" THEN DO:
          ASSIGN h_campo = h_frame.
          LEAVE.
       END.
       ASSIGN h_frame = h_frame:NEXT-SIBLING.
    END.
    IF NOT h_campo:SENSITIVE THEN DO:
        ASSIGN SELF:SENSITIVE = NO.
        RETURN "OK".
    END.
        
    /* Rodar Pesquisa do DE-PARA dos itens harmonizados */
    {include/zoomvar.i &prog-zoom=esp/ymcd0205-z01.w 
                       &proghandle   = wh-pesquisa
                       &campohandle  = h_campo
                       &campozoom    = it-codigo-padr }
END.

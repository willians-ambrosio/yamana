/**============================================================**
** Alteraá∆o...: 
** Empresa.....: Cleilton / DSC
** Data........: 02/02/2015
** Objetivo....: Validar Natureza de Operaá∆o
** ............: 11/07/2015 - Considerar usu†rios exceá∆o 
**=============================================================**/
{include/i-prgvrs.i UPC-RE1001A-U01 2.06.00.001}
{utp/ut-glob.i}

/** ParÉmetros **/                                    
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

/** Local **/
DEFINE VARIABLE hnat-operacao AS HANDLE      NO-UNDO.
DEFINE VARIABLE hbtok         AS HANDLE      NO-UNDO.
DEFINE VARIABLE hbtok_esp     AS HANDLE      NO-UNDO.

DEFINE VARIABLE l-natur-nota-propria AS LOGICAL     NO-UNDO.

DEFINE BUFFER bnatur-oper FOR natur-oper.

ASSIGN l-natur-nota-propria = FALSE.

/** Global **/


/**** Main Block ****/
/* message "p-ind-event..:" p-ind-event                  skip */
/*         "p-ind-object.:" p-ind-object                 skip */
/*         "p-cod-table..:" STRING(p-cod-table)          skip */
/*         "p-wgh-object.:" p-wgh-object:NAME            skip */
/*         "p-wgh-frame..:" p-wgh-frame:NAME             skip */
/*         "p-row-table..:" string(p-row-table)          skip */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                 */

IF p-ind-event  = "BEFORE-INITIALIZE" AND
   p-ind-object = "CONTAINER"         AND
   p-cod-table  = "docum-est"         THEN DO:

    RUN pi-LocalizaCampo(INPUT p-wgh-frame).

    /* Cria bot∆o especifico para validar */
    IF VALID-HANDLE(hbtOk) THEN DO:
        CREATE BUTTON hbtOk_esp
            ASSIGN  ROW         = hbtok:ROW
                    COLUMN      = hbtok:COLUMN
                    WIDTH       = hbtok:WIDTH
                    HEIGHT      = hbtok:HEIGHT
                    LABEL       = "*" + hbtok:LABEL
                    FRAME       = hbtok:FRAME
                    FLAT-BUTTON = hbtok:FLAT-BUTTON
                    TOOLTIP     = hbtok:TOOLTIP
                    HELP        = hbtok:HELP
                    NAME        = "btOk_esp"
                    SENSITIVE = TRUE  
                    VISIBLE   = TRUE  .
        ON RETURN OF p-wgh-frame ANYWHERE PERSISTENT RUN upc/upc-re1001a-u01.p(INPUT "UPCRE1001A-btOk_esp",
                                                                               INPUT p-ind-object,
                                                                               INPUT p-wgh-object,
                                                                               INPUT p-wgh-frame,
                                                                               INPUT p-cod-table,
                                                                               INPUT p-row-table).
            
        ON "CHOOSE" OF hbtOk_esp PERSISTENT RUN upc/upc-re1001a-u01.p(INPUT "UPCRE1001A-btOk_esp",
                                                                      INPUT p-ind-object,
                                                                      INPUT p-wgh-object,
                                                                      INPUT p-wgh-frame,
                                                                      INPUT p-cod-table,
                                                                      INPUT p-row-table).
        hbtOk_esp:MOVE-AFTER-TAB-ITEM(hbtok).
        ASSIGN hbtOk:SENSITIVE = NO
               hbtok:TAB-STOP  = NO.


    END.
END. /* p-ind-event  = "BEFORE-INITIALIZE" */
IF p-ind-event = "UPCRE1001A-btOK_esp" THEN DO:
    RUN pi-LocalizaCampo(INPUT p-wgh-frame).

    FOR FIRST bnatur-oper FIELDS(nat-operacao transf tipo-compra especie-doc mercado)
        WHERE bnatur-oper.nat-operacao = hnat-operacao:SCREEN-VALUE NO-LOCK: END.
    IF NOT AVAIL bnatur-oper THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 2, 
                           input "Natureza de operaá∆o~~" + QUOTER(hnat-operacao:SCREEN-VALUE)).
        RETURN "NOK".
    END.

    FIND FIRST es-natoper-rec 
        WHERE es-natoper-rec.ep-codigo = i-ep-codigo-usuario NO-LOCK NO-ERROR.
    IF NOT AVAIL es-natoper-rec THEN DO:
        run utp/ut-msgs.p (input "show":U, 
                           input 2, 
                           input "Natureza de operaá∆o recebimento(placebo)~~" + "Empresa: " + QUOTER(i-ep-codigo-usuario)).
        RETURN "NOK".
    END.

    FIND FIRST es-param-re
        WHERE es-param-re.ep-codigo = i-ep-codigo-usuario
          AND es-param-re.usuario   = c-seg-usuario NO-LOCK NO-ERROR.

    IF bnatur-oper.int-1 = 1 THEN
        ASSIGN l-natur-nota-propria = TRUE.
    ELSE
        ASSIGN l-natur-nota-propria = FALSE.


    IF bnatur-oper.transf                     <> es-natoper-rec.exc-transf      AND
       bnatur-oper.especie-doc                <> es-natoper-rec.exc-especie-doc AND 
       bnatur-oper.mercado                    <> es-natoper-rec.exc-mercado     AND 
   /*NOT bnatur-oper.nota-rateio                                                  AND*/
       es-natoper-rec.nat-operacao-est        <> hnat-operacao:SCREEN-VALUE     AND
       es-natoper-rec.nat-operacao-int        <> hnat-operacao:SCREEN-VALUE     AND
       es-natoper-rec.nat-operacao-rateio     <> hnat-operacao:SCREEN-VALUE     AND 
       es-natoper-rec.nat-operacao-rateio-int <> hnat-operacao:SCREEN-VALUE     AND
       bnatur-oper.terceiros                  <> es-natoper-rec.l-op-terceiros  AND
       l-natur-nota-propria                   <> es-natoper-rec.l-nota-propria  AND
       (NOT AVAIL es-param-re OR          
        (AVAIL es-param-re AND NOT es-param-re.rec-nat-exc))             THEN DO:
       
        run utp/ut-msgs.p (input "show":U, 
                           input 17006, 
                           input "Natureza de operaá∆o n∆o permitida~~" + 
                                 "Natureza de operaá∆o deve ser 'Temporaria' (" + es-natoper-rec.nat-operacao-est + " ou " + es-natoper-rec.nat-operacao-int + " ou " + es-natoper-rec.nat-operacao-rateio + " ou " + es-natoper-rec.nat-operacao-rateio-int + ") ou 'Transferància' ou 'Devoluá∆o' ou 'Controla Terceiros'.").
        RETURN "NOK".
    END.
    ELSE
        APPLY "CHOOSE" TO hbtok.
END. /* p-ind-event = "UPCRE1001A-btOK_esp" */

RETURN "OK".
/**** END Main Block ****/

PROCEDURE pi-LocalizaCampo:
    DEFINE INPUT  PARAMETER ph_frame AS HANDLE     NO-UNDO.

    ASSIGN ph_frame = ph_frame:FIRST-CHILD.
    blk_frame:
    DO WHILE ph_frame <> ?:
       IF ph_frame:TYPE <> "field-group" THEN DO:
          CASE ph_frame:TYPE:
             WHEN "frame" THEN DO:
                 IF  ph_frame:NAME = "fPage0" THEN
                    RUN pi-LocalizaCampo(INPUT ph_frame).                 
                 IF ph_frame:NAME = "fPage1" THEN LEAVE blk_frame.
             END.
             WHEN "FILL-IN" THEN DO:
                 IF ph_frame:NAME = "nat-operacao"  THEN ASSIGN hnat-operacao = ph_frame.
             END.
             WHEN "BUTTON" THEN DO:
                 IF ph_frame:NAME = "btOk"      THEN ASSIGN hbtOk     = ph_frame.
                 IF ph_frame:NAME = "btOk_esp"  THEN ASSIGN hbtOk_esp = ph_frame.
             END.
          END CASE.
          ASSIGN ph_frame = ph_frame:NEXT-SIBLING.         
       END.
       ELSE ASSIGN ph_frame = ph_frame:FIRST-CHILD.
    END.
END PROCEDURE. /* pi-LocalizaCampo */


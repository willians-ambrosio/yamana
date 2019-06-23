/**============================================================**
** Altera»’o...: 
** Empresa.....: Sergio Luiz Neto da Silveira / DSC
** Data........: 02/02/2015
** Objetivo....: UPC na tela do programa ft4004b
** ............:  
**=============================================================**/
{include/i-prgvrs.i ft4004b-u01 2.06.00.001}
{utp/ut-glob.i}

DEFINE TEMP-TABLE tt-wt-it-docto LIKE wt-it-docto
       FIELDS cod-usuario LIKE usuar-mater.cod-usuario.

/** Par³metros **/                                    
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

/** Local **/
DEFINE NEW GLOBAL SHARED VARIABLE hnr-sequencia   AS HANDLE      NO-UNDO.


/* /**** Main Block ****/                                     */
/* message "p-ind-event..:" p-ind-event                  skip */
/*         "p-ind-object.:" p-ind-object                 skip */
/*         "p-cod-table..:" STRING(p-cod-table)          skip */
/*         "p-wgh-object.:" p-wgh-object:NAME            skip */
/*         "p-wgh-frame..:" p-wgh-frame:NAME             skip */
/*         "p-row-table..:" string(p-row-table)          skip */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                 */

IF p-ind-event  = "AFTER-INITIALIZE"  AND
   p-ind-object = "CONTAINER"         THEN DO:

   RUN pi-LocalizaCampo(INPUT p-wgh-frame).
END. 


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
                 IF  ph_frame:NAME = "fPage1" THEN
                    RUN pi-LocalizaCampo(INPUT ph_frame).                 
             END.
             WHEN "FILL-IN" THEN DO:
                 
                 IF ph_frame:NAME = "nr-sequencia"   THEN ASSIGN hnr-sequencia   = ph_frame.
             END.
             WHEN "RADIO-SET" THEN DO:
             END.
             WHEN "COMBO-BOX" THEN DO:
             END.
             WHEN "BUTTON" THEN DO:
             END.
          END CASE.
          ASSIGN ph_frame = ph_frame:NEXT-SIBLING.         
       END.
       ELSE ASSIGN ph_frame = ph_frame:FIRST-CHILD.
    END.
END PROCEDURE. /* pi-LocalizaCampo */


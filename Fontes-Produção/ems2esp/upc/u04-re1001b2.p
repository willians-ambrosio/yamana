/**============================================================**
** Altera‡Æo...: 
** Empresa.....: Cleilton / DSC
** Data........: 02/02/2015
** Objetivo....: Validar Natureza de Opera‡Æo
** ............:  
**=============================================================**/
{include/i-prgvrs.i U03-RE1001B2 11.5.11.002}
{utp/ut-glob.i}

/** Parƒmetros **/                                    
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

/** Local **/
DEFINE VARIABLE h-page-imposto   AS HANDLE      NO-UNDO.
DEFINE VARIABLE hd-aliq-cofins   AS HANDLE      NO-UNDO.
DEFINE VARIABLE hd-aliq-pis      AS HANDLE      NO-UNDO.
DEFINE VARIABLE hcd-trib-pis     AS HANDLE      NO-UNDO.
DEFINE VARIABLE hcd-trib-cofins  AS HANDLE      NO-UNDO.
                 
/** Global **/

DEF NEW GLOBAL SHARED VAR v_cod_usuar_corren AS CHAR NO-UNDO.

FIND FIRST param-re NO-LOCK 
     WHERE param-re.usuario = v_cod_usuar_corren NO-ERROR.

FIND FIRST ext-param-re NO-LOCK 
     WHERE ext-param-re.usuario = v_cod_usuar_corren NO-ERROR.

/**** Main Block ****/
/* message "p-ind-event..:" p-ind-event                  skip */
/*         "p-ind-object.:" p-ind-object                 skip */
/*         "p-cod-table..:" STRING(p-cod-table)          skip */
/*         "p-wgh-object.:" p-wgh-object:NAME            skip */
/*         "p-wgh-frame..:" p-wgh-frame:NAME             skip */
/*         "p-row-table..:" string(p-row-table)          skip */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                 */




/* Atualizar dados na tela */
IF p-ind-event  = "AFTER-INITIALIZE" AND
   p-ind-object = "CONTAINER"        THEN DO:

    RUN pi-LocalizaCampo(INPUT p-wgh-frame).

    IF VALID-HANDLE(h-page-imposto) THEN DO:
        RUN pi-LocalizaCampo(INPUT p-wgh-frame).

        IF VALID-HANDLE(hd-aliq-cofins ) THEN DO:

            IF AVAIL(ext-param-re) AND NOT ext-param-re.pd-imp-rateio THEN DO:
                ASSIGN hd-aliq-cofins  :sensitive = false.
                       hd-aliq-pis     :sensitive = false.
                       hcd-trib-pis    :sensitive = false.
                       hcd-trib-cofins :sensitive = false.
            END.
        END.
    END.

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
                 IF  ph_frame:NAME = "fPage0" THEN
                    RUN pi-LocalizaCampo(INPUT ph_frame).                 
                 IF  ph_frame:NAME = "fPage4" THEN DO:
                    RUN pi-LocalizaCampo(INPUT ph_frame).                 
                    ASSIGN h-page-imposto = ph_frame.
                    LEAVE blk_frame.
                 END.
                    
                  /*IF ph_frame:NAME = "fPage1" THEN LEAVE blk_frame. */
             END.
             WHEN "FILL-IN" THEN DO:
                 IF ph_frame:NAME = "d-aliq-cofins"  THEN ASSIGN hd-aliq-cofins   = ph_frame.
                 IF ph_frame:NAME = "d-aliq-pis"     THEN ASSIGN hd-aliq-pis      = ph_frame.
             END.
             WHEN "COMBO-BOX" THEN DO:
                 IF ph_frame:NAME = "cd-trib-pis"    THEN ASSIGN hcd-trib-pis    = ph_frame.
                 IF ph_frame:NAME = "cd-trib-cofins" THEN ASSIGN hcd-trib-cofins = ph_frame.
             END.
          END CASE.
          ASSIGN ph_frame = ph_frame:NEXT-SIBLING.         
       END.
       ELSE ASSIGN ph_frame = ph_frame:FIRST-CHILD.
    END.
END PROCEDURE. /* pi-LocalizaCampo */


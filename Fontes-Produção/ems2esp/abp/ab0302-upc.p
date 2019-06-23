/**============================================================**
** Altera¯Êo...: 
** Empresa.....: Sergio Luiz Neto da Silveira / DSC
** Data........: 02/02/2015
** Objetivo....: UPC na tela do programa ab0302
** ............:  
**=============================================================**/
{include/i-prgvrs.i ab0302-upc 2.06.00.001}
{utp/ut-glob.i}



/** Parümetros **/                                    
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

/** Local **/
DEFINE NEW GLOBAL SHARED VARIABLE hdat-movto       AS HANDLE      NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE hbtDeleteSon1       AS HANDLE      NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE hbtDeleteSon1-aux   AS HANDLE      NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE hbtDeleteSon2       AS HANDLE      NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE hbtDeleteSon2-aux   AS HANDLE      NO-UNDO.


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

   


    IF VALID-HANDLE(hbtDeleteSon1 ) THEN DO:
        create button hbtDeleteSon1-aux
        assign frame        = hbtDeleteSon1:frame
               width        = hbtDeleteSon1:width 
               height       = hbtDeleteSon1:height
               LABEL        = hbtDeleteSon1:LABEL  + "*"
               row          = hbtDeleteSon1:row
               col          = hbtDeleteSon1:col 
               visible      = yes
               sensitive    = YES
               triggers:
                    ON CHOOSE persistent run abp/ab0302-upc.p   (input "hbtDeleteSon1-aux",
                                                                 input p-ind-object ,
                                                                 input p-wgh-object ,
                                                                 input p-wgh-frame  ,
                                                                 input p-cod-table  ,
                                                                 input p-row-table  ).
               end triggers.
    END.

    IF VALID-HANDLE(hbtDeleteSon2 ) THEN DO:
        create button hbtDeleteSon2-aux
        assign frame        = hbtDeleteSon2:frame
               width        = hbtDeleteSon2:width 
               height       = hbtDeleteSon2:height
               LABEL        = hbtDeleteSon2:LABEL  + "*"
               row          = hbtDeleteSon2:row
               col          = hbtDeleteSon2:col 
               visible      = yes
               sensitive    = YES
               triggers:
                    ON CHOOSE persistent run abp/ab0302-upc.p   (input "hbtDeleteSon2-aux",
                                                                 input p-ind-object ,
                                                                 input p-wgh-object ,
                                                                 input p-wgh-frame  ,
                                                                 input p-cod-table  ,
                                                                 input p-row-table  ).
               end triggers.
    END.

END.


IF p-ind-event = "hbtDeleteSon1-aux" OR 
   p-ind-event = "hbtDeleteSon2-aux" THEN DO:

    FIND FIRST param-estoq NO-LOCK NO-ERROR.
    IF AVAIL(param-estoq) THEN DO:
        IF date(hdat-movto:SCREEN-VALUE) <= param-estoq.contab-ate THEN DO:
            RUN utp/ut-msgs.p (INPUT "SHOW",
                               INPUT 17006,
                               INPUT "Per¡odo j  encerrado!~~Movimento nÆo pode ser eliminado.").
                
            RETURN "NOK".

        END.
        ELSE DO:
            IF p-ind-event = "hbtDeleteSon1-aux" THEN
               APPLY "choose" TO hbtDeleteSon1.
            ELSE
               APPLY "choose" TO hbtDeleteSon2.

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
                 IF  ph_frame:NAME = "fPage1" THEN
                    RUN pi-LocalizaCampo(INPUT ph_frame).                 
                 IF  ph_frame:NAME = "fPage2" THEN
                    RUN pi-LocalizaCampo(INPUT ph_frame).                 
             END.
             WHEN "FILL-IN" THEN DO:
                 
                 IF ph_frame:NAME = "dat-movto"   THEN ASSIGN hdat-movto   = ph_frame.
             END.
             WHEN "RADIO-SET" THEN DO:
             END.
             WHEN "COMBO-BOX" THEN DO:
             END.
             WHEN "BUTTON" THEN DO:
                 IF ph_frame:NAME = "btDeleteSon1"   THEN ASSIGN hbtDeleteSon1   = ph_frame.
                 IF ph_frame:NAME = "btDeleteSon2"   THEN ASSIGN hbtDeleteSon2   = ph_frame.
             END.
          END CASE.
          ASSIGN ph_frame = ph_frame:NEXT-SIBLING.         
       END.
       ELSE ASSIGN ph_frame = ph_frame:FIRST-CHILD.
    END.
END PROCEDURE. /* pi-LocalizaCampo */


/**============================================================**
** Altera‡Æo...: 
** Empresa.....: Cleilton / DSC
** Data........: 02/02/2015
** Objetivo....: Validar/Trocar Natureza de Opera‡Æo
** ............:  
**=============================================================**/
{include/i-prgvrs.i U02-RE0101 11.5.11.000}
{utp/ut-glob.i}
{cdp/cdcfgdis.i}
{cdp/cdcfgmat.i}
{cdp/cdcfgcex.i}

/** Parƒmetros **/                                    
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

/** Local **/
DEFINE VARIABLE hpd-seq AS HANDLE      NO-UNDO.
DEFINE VARIABLE hrec-nat-exc AS HANDLE      NO-UNDO.

DEFINE BUFFER bparam-re FOR param-re.

/** Global **/



/**** Main Block ****/
/* IF p-wgh-object:NAME BEGINS "invwr/v07in292" THEN DO:          */
/*     message "p-ind-event..:" p-ind-event                  skip */
/*             "p-ind-object.:" p-ind-object                 skip */
/*             "p-cod-table..:" STRING(p-cod-table)          skip */
/*             "p-wgh-object.:" p-wgh-object:NAME            skip */
/*             "p-wgh-frame..:" p-wgh-frame:NAME             skip */
/*             "p-row-table..:" string(p-row-table)          skip */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                 */
/* END.                                                           */

/* Criar Objeto */
IF p-wgh-object:NAME BEGINS "invwr/v07in292" AND
   p-ind-event            = "BEFORE-DISPLAY" THEN DO:
    RUN pi-LocalizaCampo(INPUT p-wgh-frame).
    IF NOT VALID-HANDLE(hrec-nat-exc) AND VALID-HANDLE(hpd-seq) THEN DO:
        CREATE TOGGLE-BOX hrec-nat-exc
            ASSIGN FRAME              = hpd-seq:FRAME                           
                   FORMAT             = "Sim/NÆo" 
                   WIDTH              = 30
                   HEIGHT             = hpd-seq:HEIGHT
                   ROW                = hpd-seq:ROW + 2
                   COL                = hpd-seq:COL 
                   LABEL              = "Recebe com Nat. Opera‡Æo Exce‡Æo"
                   HELP               = "Indica se usu rio pode receber com qualquer nat. opera‡Æo"
                   TOOLTIP            = "Indica se usu rio pode receber com qualquer nat. opera‡Æo"
                   VISIBLE            = YES 
                   SENSITIVE          = NO
                   CHECKED            = FALSE
                   NAME               = "rec-nat-exc".
        hpd-seq:FRAME:HIDDEN = TRUE.
    END.

END.
/* Atualizar campos */
IF p-wgh-object:NAME BEGINS "invwr/v07in292" AND
   p-ind-event            = "DISPLAY" THEN DO:
    RUN pi-LocalizaCampo(INPUT p-wgh-frame).
    IF VALID-HANDLE(hrec-nat-exc) THEN DO:
        ASSIGN hrec-nat-exc:CHECKED = NO.
        FOR FIRST bparam-re FIELDS(usuario)
            WHERE ROWID(bparam-re) = p-row-table NO-LOCK:
            FOR FIRST es-param-re
                WHERE es-param-re.ep-codigo = STRING(i-ep-codigo-usuario)
                  AND es-param-re.usuario   = bparam-re.usuario NO-LOCK:
                ASSIGN hrec-nat-exc:CHECKED = es-param-re.rec-nat-exc.
            END.
        END.
    END.

END. /* Atualizar campos */

/* Gravar campos */
IF p-wgh-object:NAME BEGINS "invwr/v07in292" AND
   p-ind-event            = "ASSIGN" THEN DO:
    RUN pi-LocalizaCampo(INPUT p-wgh-frame).
    IF VALID-HANDLE(hrec-nat-exc) THEN DO:
        FOR FIRST bparam-re FIELDS(usuario)
            WHERE ROWID(bparam-re) = p-row-table NO-LOCK:
            FIND FIRST es-param-re
                WHERE es-param-re.ep-codigo = STRING(i-ep-codigo-usuario)
                  AND es-param-re.usuario   = bparam-re.usuario NO-ERROR.
            IF NOT AVAIL es-param-re THEN DO:
                CREATE es-param-re.
                ASSIGN es-param-re.ep-codigo = STRING(i-ep-codigo-usuario)
                       es-param-re.usuario   = bparam-re.usuario.
            END.
            ASSIGN es-param-re.rec-nat-exc = hrec-nat-exc:CHECKED.
        END.
    END.

END.

/* Ativar e desativar */
IF p-wgh-object:NAME BEGINS "invwr/v07in292" AND
   (p-ind-event = "AFTER-ENABLE"  OR
    p-ind-event = "AFTER-DISABLE" OR
    p-ind-event = "ENABLE"        OR
    p-ind-event = "DISABLE"       ) THEN DO:
    RUN pi-LocalizaCampo(INPUT p-wgh-frame).
    IF VALID-HANDLE(hrec-nat-exc) AND VALID-HANDLE(hpd-seq) THEN DO:
        ASSIGN hrec-nat-exc:SENSITIVE = hpd-seq:SENSITIVE.
    END.
END. /* Ativar e desativar */


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
             END.
              WHEN "TOGGLE-BOX" THEN DO:
                  IF ph_frame:NAME = "pd-seq"      THEN ASSIGN hpd-seq      = ph_frame.
                  IF ph_frame:NAME = "rec-nat-exc" THEN ASSIGN hrec-nat-exc = ph_frame.
              END.
          END CASE.
          ASSIGN ph_frame = ph_frame:NEXT-SIBLING.         
       END.
       ELSE ASSIGN ph_frame = ph_frame:FIRST-CHILD.
    END.
END PROCEDURE. /* pi-LocalizaCampo */

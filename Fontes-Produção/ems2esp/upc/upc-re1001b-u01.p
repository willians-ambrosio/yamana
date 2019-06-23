/**============================================================**
** Altera‡Æo...: 
** Empresa.....: Cleilton / DSC
** Data........: 02/02/2015
** Objetivo....: Validar/Trocar Natureza de Opera‡Æo
** ............:  
**=============================================================**/
{include/i-prgvrs.i UPC-RE1001-U01 11.5.11.001}
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

DEFINE BUFFER bf-item-doc-est FOR item-doc-est.

DEFINE VARIABLE d-base AS DECIMAL NO-UNDO.

/** Global **/



/* /**** Main Block ****/                                     */
/* message "p-ind-event..:" p-ind-event                  skip */
/*         "p-ind-object.:" p-ind-object                 skip */
/*         "p-cod-table..:" STRING(p-cod-table)          skip */
/*         "p-wgh-object.:" p-wgh-object:NAME            skip */
/*         "p-wgh-frame..:" p-wgh-frame:NAME             skip */
/*         "p-row-table..:" string(p-row-table)          skip */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                 */



IF p-ind-event  = "BEFORE-DESTROY-INTERFACE" AND
   p-ind-object = "CONTAINER"        AND
   p-cod-table  = "item-doc-est"        THEN DO:

   FIND FIRST bf-item-doc-est
        WHERE ROWID(bf-item-doc-est) = p-row-table
        NO-LOCK NO-ERROR.
   IF AVAILABLE(bf-item-doc-est) THEN DO:
      FIND FIRST docum-est OF bf-item-doc-est
           NO-LOCK NO-ERROR.
      IF AVAILABLE(docum-est) THEN DO:
          ASSIGN d-base = 0.
          FOR EACH item-doc-est OF docum-est:
              ASSIGN d-base = DECIMAL(SUBSTRING(item-doc-est.char-2,860,20)).

/*                    MESSAGE "d-base" d-base SKIP                                                                              */
/*                        'DECIMAL(SUBSTRING(item-doc-est.char-2,860,20)) ' DECIMAL(SUBSTRING(item-doc-est.char-2,860,20)) SKIP */
/*                        'SUBSTRING(item-doc-est.char-2,860,20)' SUBSTRING(item-doc-est.char-2,860,20)                         */
/*                        VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                    */
          END.

/*                MESSAGE 'vai gravar : ' d-base         */
/*                    VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*                                                       */
          OVERLAY(docum-est.char-2,191,20,'character') = STRING(d-base,'>>>>>>9.99').

      END.
   END.
END.

/*****************************************************************************
 ** PROGRAMA..: cd0620-upc01.p
 ** OBJETIVO..: UPC Recebimento - cd0620
 ** AUTOR.....: DKP 
 ** CLIENTE...: YAMANA
 ** VERSAO....: 2.00.00.001   
 ** ALTERA€åES:
 ******************************************************************************/

{utp/ut-glob.i}

def input param p-ind-event  as char          no-undo.
def input param p-ind-object as char          no-undo.
def input param p-wgh-object as handle        no-undo.
def input param p-wgh-frame  as widget-handle no-undo.
def input param p-cod-table  as char          no-undo.
def input param p-row-table  as rowid         no-undo.

def new global shared var rtToolBar as widget-handle no-undo.
def new global shared var tg-valida as widget-handle no-undo.

DEFINE VARIABLE wh-objet AS HANDLE      NO-UNDO.
ASSIGN wh-objet = p-wgh-frame:first-child.

FIND cfop-natur WHERE ROWID(cfop-natur) = p-row-table NO-LOCK NO-ERROR.
IF NOT AVAILABLE cfop-natur THEN RETURN.

DO WHILE VALID-HANDLE(wh-objet):
  
  IF wh-objet:NAME = "rtToolBar" THEN rtToolBar = wh-objet.

  IF wh-objet:TYPE = 'Field-group' THEN DO:
    /*....e vai para o primeiro campo*/
    ASSIGN wh-objet = wh-objet:first-child.
  END.
  ELSE DO: /*Caso contrario vai para o proximo.*/
    ASSIGN wh-objet = wh-objet:NEXT-SIBLING.
  END.

END.

CASE p-ind-event:

    WHEN "AFTER-ADD" OR WHEN "AFTER-UPDATE" OR WHEN "AFTER-COPY" THEN DO:
        tg-valida:SENSITIVE = YES.
    END.

    WHEN "AFTER-ASSIGN" THEN DO:
        IF AVAILABLE cfop-natur THEN DO:
            FIND mgesp.es-cfop-val-nf OF cfop-natur EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE mgesp.es-cfop-val-nf THEN DO:
                CREATE mgesp.es-cfop-val-nf.
                mgesp.es-cfop-val-nf.cod-cfop = cfop-natur.cod-cfop.
            END.
            mgesp.es-cfop-val-nf.log-valcon-nf = LOGICAL(tg-valida:SCREEN-VALUE).
        END.
    END.

    WHEN "AFTER-DISABLE" THEN DO:
        tg-valida:SENSITIVE = NO.
    END.

    WHEN "BEFORE-DELETE" THEN DO:
        FIND mgesp.es-cfop-val-nf OF cfop-natur EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE mgesp.es-cfop-val-nf THEN
            DELETE mgesp.es-cfop-val-nf.
    END.

    /* coloca o novo campo na tela */
    WHEN "AFTER-DISPLAY" THEN DO:
       CREATE TOGGLE-BOX tg-valida
       ASSIGN frame              = rtToolBar:FRAME
              width              = 40
              height             = 1.00
              row                = 3.00
              col                = 50
              LABEL              = "Valida Ato Concession rio na Nota Fiscal"
              visible            = YES
              sensitive          = NO.

       tg-valida:SCREEN-VALUE = STRING(CAN-FIND(mgesp.es-cfop-val-nf OF cfop-natur WHERE
                                                mgesp.es-cfop-val-nf.log-valcon-nf = YES
                                                )
                                       ).
    END.
END CASE.

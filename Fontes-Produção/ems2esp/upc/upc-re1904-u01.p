/*************************************************************************************
**
** Programa: upc-re1904-u01
**
** Objetivo: Bloquear a tela caso o item nÆo tenha saldo em estoque
**
** Autor   : Renato Oliveira
**
** Data    : Agosto/2018
**
** Versao  : 2.12.00.000 - Desenvolvimento Inicial
**
*************************************************************************************/
{include/i-prgvrs.i upc-re1904-u01 2.12.00.000}

/* Definicao de Funcoes */
{tools/fc-handle-obj.i}
{tools/fc-falso.i}

/* Variaveis de Parametros */
DEFINE INPUT PARAMETER p-ind-event  AS CHAR          NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHAR          NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHAR          NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

/* Variaveis Globais */
DEF NEW GLOBAL SHARED VAR wh-re1904-btOK      AS WIDGET-HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re1904-btOK-f    AS WIDGET-HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re1904-c-it-serv AS WIDGET-HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-docum-est        AS ROWID          NO-UNDO.

/* Variaveis Globais */
DEF VAR c-handle-obj AS CHAR    NO-UNDO.
DEF VAR l-ok         AS LOGICAL NO-UNDO.

/* Buffer */

/*     MESSAGE "p-ind-event..:" p-ind-event            SKIP */
/*         "p-ind-object.:" p-ind-object           SKIP     */
/*         "p-cod-table..:" STRING(p-cod-table)    SKIP     */
/*         "p-row-table..:" STRING(p-row-table)    SKIP     */
/*         "p-wgh-object.:" p-wgh-object:FILE-NAME SKIP     */
/*         "p-wgh-frame..:" STRING(p-wgh-frame)    SKIP     */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                   */

IF  p-ind-event  = "AFTER-INITIALIZE"
AND p-ind-object = "CONTAINER"
AND p-wgh-object:FILE-NAME MATCHES "*re1904*" THEN DO:

    ASSIGN c-handle-obj = fc-handle-obj("btOK,c-it-serv",p-wgh-frame)
           wh-re1904-c-it-serv = WIDGET-HANDLE(ENTRY(2,c-handle-obj)) 
           wh-re1904-btOK = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
           wh-re1904-btOk-f = fc-falso(wh-re1904-btOk,wh-re1904-btOk:FRAME,"").
           wh-re1904-btOk-f:LOAD-IMAGE-UP(wh-re1904-btOk:IMAGE-UP ).
           wh-re1904-btOk-f:LOAD-IMAGE-DOWN(wh-re1904-btOk:IMAGE-DOWN ).
           wh-re1904-btOk-f:SENSITIVE = YES.
           wh-re1904-btOk-f:MOVE-TO-TOP().
           wh-re1904-btOk:SENSITIVE = NO.

    ON "choose":U OF wh-re1904-btOk-f PERSISTENT RUN upc/upc-re1904-u01.p(INPUT "choose",
                                                                            INPUT "wh-re1904-btOk-f",
                                                                            INPUT p-wgh-object,
                                                                            INPUT p-wgh-frame,
                                                                            INPUT "",
                                                                            INPUT ?).
END.

IF  p-ind-event  = "choose"
AND p-ind-object = "wh-re1904-btOk-f" THEN DO:

    ASSIGN l-ok = NO.
    FIND FIRST docum-est NO-LOCK
         WHERE ROWID(docum-est) = gr-docum-est NO-ERROR.
    IF AVAIL docum-est THEN
        FOR EACH saldo-estoq NO-LOCK
           WHERE saldo-estoq.cod-estabel = docum-est.cod-estabel
             AND saldo-estoq.it-codigo   = wh-re1904-c-it-serv:SCREEN-VALUE
             AND (saldo-estoq.qtidade-atu - (saldo-estoq.qt-alocada + saldo-estoq.qt-aloc-ped + saldo-estoq.qt-aloc-prod)) > 0:
            ASSIGN l-ok = YES.
            LEAVE.
        END.

    IF l-ok = NO THEN DO:
        RUN utp/ut-msgs.p(INPUT "show",
                          INPUT 17006,
                          INPUT "Item sem Saldo em Estoque" + "~~" + "Item sem Saldo em Estoque, Verifique.").
        RETURN "NOK".
    END.
    ELSE DO:
        APPLY "choose":U TO wh-re1904-btOk.
    END.

END.

RETURN "OK":U.
/* Fim do Programa */

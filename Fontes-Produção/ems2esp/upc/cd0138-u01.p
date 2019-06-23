/* =========================================================================== 
   PROGRAMA    : CD0138-U01.p
   DATA        : 24/05/2010
   DESENVOLVIDO: Marcio Sacramoni - Kraft
   VERSAO      : 000
   OBJETIVO    : UPC no programa Itens x Materiais. 
   =========================================================================== */

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i  cd0138-u01.P 2.06.00.000}
{tools/fc-handle-obj.i}
{utp\ut-glob.i}

/* ==========> Parametros obrigatorios para UPC. <========== */
DEF INPUT PARAMETER p-ind-event  AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-ind-object AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-wgh-object AS HANDLE            NO-UNDO.
DEF INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE     NO-UNDO.
DEF INPUT PARAMETER p-cod-table  AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-row-table  AS ROWID             NO-UNDO.

/* ==========> Definicao das Variavel Globais.   <========== */
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0138-res-for-comp AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-res-for-comp-ant     AS INTEGER       NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0138-deposito-pad     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-deposito-pad-ant         AS CHARACTER     NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0138-cod-localiz      AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-cod-localiz-ant          AS CHARACTER     NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0138-res-int-comp     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-res-int-comp-ant         AS INTEGER       NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0138-cod-unid-negoc   AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-cod-unid-negoc-ant       AS CHARACTER     NO-UNDO.


DEFINE VARIABLE c-handle-obj   AS CHAR NO-UNDO.
DEFINE VARIABLE c-return-value AS CHAR NO-UNDO.

DEFINE BUFFER b-item FOR ITEM.

/*
MESSAGE "p-ind-event..:" p-ind-event                  SKIP 
        "p-ind-object.:" p-ind-object                 SKIP 
        "p-cod-table..:" STRING(p-cod-table)          SKIP 
        "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    SKIP 
        "p-wgh-frame..:" STRING(p-wgh-frame)          SKIP 
        "p-row-table..:" STRING(p-row-table) SKIP
        VIEW-AS ALERT-BOX INFO BUTTONS OK. */

/* ==========> Mapeador de  Enderecos de Objeos  <========== */
IF NOT VALID-HANDLE(wh-cd0138-res-for-comp)  THEN 
DO:
   ASSIGN c-handle-obj = fc-handle-obj("res-for-comp",p-wgh-frame).
   ASSIGN wh-cd0138-res-for-comp = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
          i-res-for-comp-ant     = 0.
END.

IF NOT VALID-HANDLE(wh-cd0138-deposito-pad)  THEN 
DO:
   ASSIGN c-handle-obj = fc-handle-obj("deposito-pad",p-wgh-frame).
   ASSIGN wh-cd0138-deposito-pad = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
          i-deposito-pad-ant     = ''.
END.

IF NOT VALID-HANDLE(wh-cd0138-cod-localiz)  THEN 
DO:
   ASSIGN c-handle-obj = fc-handle-obj("cod-localiz",p-wgh-frame).
   ASSIGN wh-cd0138-cod-localiz = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
          i-cod-localiz-ant     = ''.
END.

IF NOT VALID-HANDLE(wh-cd0138-res-int-comp)  THEN 
DO:
   ASSIGN c-handle-obj = fc-handle-obj("res-int-comp",p-wgh-frame).
   ASSIGN wh-cd0138-res-int-comp = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
          i-res-int-comp-ant     = 0.
END.

IF NOT VALID-HANDLE(wh-cd0138-cod-unid-negoc)  THEN 
DO:
   ASSIGN c-handle-obj = fc-handle-obj("c-cod-unid-negoc",p-wgh-frame).
   ASSIGN wh-cd0138-cod-unid-negoc = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
          i-cod-unid-negoc-ant     = ''.
END.


IF p-ind-event  = "BEFORE-ASSIGN" AND
   p-ind-object = "CONTAINER"     THEN
DO:
    FIND b-item NO-LOCK WHERE
         ROWID(b-item) = p-row-table NO-ERROR.
    IF AVAIL b-item THEN DO:
       ASSIGN i-res-for-comp-ant = b-item.res-for-comp
              i-deposito-pad-ant   = b-item.deposito-pad
              i-cod-localiz-ant    = b-item.cod-localiz
              i-res-int-comp-ant   = b-item.res-int-comp
              i-cod-unid-negoc-ant   = b-item.cod-unid-negoc.

    END.
END. /* p-ind-event  = "BEFORE-ASSIGN" */

IF p-ind-event  = "AFTER-ASSIGN" AND
   p-ind-object = "CONTAINER"    THEN
DO:

    FIND b-item NO-LOCK WHERE
         ROWID(b-item) = p-row-table NO-ERROR.
    IF AVAIL b-item THEN do:

        IF b-item.res-for-comp <> i-res-for-comp-ant THEN DO:

            CREATE ext-audit-item-fam.
            ASSIGN ext-audit-item-fam.it-codigo = b-item.it-codigo
                   ext-audit-item-fam.campo     = "res-for-comp"
                   ext-audit-item-fam.data      = TODAY
                   ext-audit-item-fam.hora      = TIME
                   ext-audit-item-fam.fm-codigo = ""
                   ext-audit-item-fam.programa  = "CD0138"
                   ext-audit-item-fam.usuario   = c-seg-usuario
                   ext-audit-item-fam.valor-ant = STRING(i-res-for-comp-ant)
                   ext-audit-item-fam.valor-atu = STRING(b-item.res-for-comp).

        END.

        IF b-item.deposito-pad <> i-deposito-pad-ant THEN DO:

            CREATE ext-audit-item-fam.
            ASSIGN ext-audit-item-fam.it-codigo = b-item.it-codigo
                   ext-audit-item-fam.campo     = "deposito-pad"
                   ext-audit-item-fam.data      = TODAY
                   ext-audit-item-fam.hora      = TIME
                   ext-audit-item-fam.fm-codigo = ""
                   ext-audit-item-fam.programa  = "cd0138"
                   ext-audit-item-fam.usuario   = c-seg-usuario
                   ext-audit-item-fam.valor-ant = STRING(i-deposito-pad-ant)
                   ext-audit-item-fam.valor-atu = STRING(b-item.deposito-pad).

        END.

        IF b-item.cod-localiz <> i-cod-localiz-ant THEN DO:

            CREATE ext-audit-item-fam.
            ASSIGN ext-audit-item-fam.it-codigo = b-item.it-codigo
                   ext-audit-item-fam.campo     = "cod-localiz"
                   ext-audit-item-fam.data      = TODAY
                   ext-audit-item-fam.hora      = TIME
                   ext-audit-item-fam.fm-codigo = ""
                   ext-audit-item-fam.programa  = "cd0138"
                   ext-audit-item-fam.usuario   = c-seg-usuario
                   ext-audit-item-fam.valor-ant = STRING(i-cod-localiz-ant)
                   ext-audit-item-fam.valor-atu = STRING(b-item.cod-localiz).

        END.

        IF b-item.res-int-comp <> i-res-int-comp-ant THEN DO:

            CREATE ext-audit-item-fam.
            ASSIGN ext-audit-item-fam.it-codigo = b-item.it-codigo
                   ext-audit-item-fam.campo     = "res-int-comp"
                   ext-audit-item-fam.data      = TODAY
                   ext-audit-item-fam.hora      = TIME
                   ext-audit-item-fam.fm-codigo = ""
                   ext-audit-item-fam.programa  = "cd0138"
                   ext-audit-item-fam.usuario   = c-seg-usuario
                   ext-audit-item-fam.valor-ant = STRING(i-res-int-comp-ant)
                   ext-audit-item-fam.valor-atu = STRING(b-item.res-int-comp).

        END.

        IF b-item.cod-unid-negoc <> i-cod-unid-negoc-ant THEN DO:

            CREATE ext-audit-item-fam.
            ASSIGN ext-audit-item-fam.it-codigo = b-item.it-codigo
                   ext-audit-item-fam.campo     = "cod-unid-negoc"
                   ext-audit-item-fam.data      = TODAY
                   ext-audit-item-fam.hora      = TIME
                   ext-audit-item-fam.fm-codigo = ""
                   ext-audit-item-fam.programa  = "cd0138"
                   ext-audit-item-fam.usuario   = c-seg-usuario
                   ext-audit-item-fam.valor-ant = STRING(i-cod-unid-negoc-ant)
                   ext-audit-item-fam.valor-atu = STRING(b-item.cod-unid-negoc).

        END.

    END. /* AVAIL b-item */
END. /* p-ind-event  = "AFTER-ASSIGN"  */

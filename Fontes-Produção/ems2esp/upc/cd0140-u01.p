/* =========================================================================== 
   PROGRAMA    : CD0140-U01.p
   DATA        : 24/05/2010
   DESENVOLVIDO: Marcio Sacramoni - Kraft
   VERSAO      : 000
   OBJETIVO    : UPC no programa Itens x Materiais x Estabelecimento. 
   =========================================================================== */

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i  cd0140-u01.P 2.06.00.000}
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
DEFINE NEW GLOBAL SHARED VARIABLE wh-cd0140-res-for-comp AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-res-for-comp-ant     AS INTEGER       NO-UNDO.

DEFINE VARIABLE c-handle-obj   AS CHAR NO-UNDO.
DEFINE VARIABLE c-return-value AS CHAR NO-UNDO.

DEFINE BUFFER b-item-uni-estab FOR item-uni-estab.

/*
MESSAGE "p-ind-event..:" p-ind-event                  SKIP 
        "p-ind-object.:" p-ind-object                 SKIP 
        "p-cod-table..:" STRING(p-cod-table)          SKIP 
        "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    SKIP 
        "p-wgh-frame..:" STRING(p-wgh-frame)          SKIP 
        "p-row-table..:" STRING(p-row-table) SKIP
        VIEW-AS ALERT-BOX INFO BUTTONS OK. */

/* ==========> Mapeador de  Enderecos de Objeos  <========== */
IF NOT VALID-HANDLE(wh-cd0140-res-for-comp)  THEN 
DO:
   ASSIGN c-handle-obj = fc-handle-obj("res-for-comp",p-wgh-frame).
   ASSIGN wh-cd0140-res-for-comp = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
          i-res-for-comp-ant     = 0.
END.

IF p-ind-event  = "BEFORE-ASSIGN" AND
   p-ind-object = "CONTAINER"     THEN
DO:
    FIND b-item-uni-estab NO-LOCK WHERE
         ROWID(b-item-uni-estab) = p-row-table NO-ERROR.
    IF AVAIL b-item-uni-estab THEN
       ASSIGN i-res-for-comp-ant = b-item-uni-estab.res-for-comp.
END. /* p-ind-event  = "BEFORE-ASSIGN" */

IF p-ind-event  = "AFTER-ASSIGN" AND
   p-ind-object = "CONTAINER"    THEN
DO:
    FIND b-item-uni-estab NO-LOCK WHERE
         ROWID(b-item-uni-estab) = p-row-table NO-ERROR.
    IF AVAIL b-item-uni-estab AND b-item-uni-estab.res-for-comp <> i-res-for-comp-ant THEN
    DO:
        CREATE ext-audit-item-fam.
        ASSIGN ext-audit-item-fam.it-codigo = b-item-uni-estab.it-codigo
               ext-audit-item-fam.campo     = "res-for-comp"
               ext-audit-item-fam.data      = TODAY
               ext-audit-item-fam.hora      = TIME
               ext-audit-item-fam.fm-codigo = ""
               ext-audit-item-fam.programa  = "CD0140"
               ext-audit-item-fam.usuario   = c-seg-usuario
               ext-audit-item-fam.valor-ant = STRING(i-res-for-comp-ant)
               ext-audit-item-fam.valor-atu = STRING(b-item-uni-estab.res-for-comp).
    END. /* AVAIL b-item */
END. /* p-ind-event  = "AFTER-ASSIGN"  */

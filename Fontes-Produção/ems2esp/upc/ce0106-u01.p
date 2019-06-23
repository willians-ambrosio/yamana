/* =========================================================================== 
   PROGRAMA    : CE0106-U01.p
   DATA        : 24/05/2010
   DESENVOLVIDO: Roberto Fernandes - DSC
   VERSAO      : 000
   OBJETIVO    : UPC no programa Itens x Estab Estoque.
   =========================================================================== */

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i  CE0106-u01.P 2.06.00.000}
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
DEFINE NEW GLOBAL SHARED VARIABLE wh-CE0106-cb-tp-item     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-tp-item-ant            AS character     NO-UNDO.


DEFINE VARIABLE c-handle-obj   AS CHAR NO-UNDO.
DEFINE VARIABLE c-return-value AS CHAR NO-UNDO.

DEFINE BUFFER b-item     FOR item.

/* ==========> Mapeador de  Enderecos de Objeos  <========== */
IF NOT VALID-HANDLE(wh-CE0106-cb-tp-item)  THEN 
DO:
   ASSIGN c-handle-obj = fc-handle-obj("cb-tp-item",p-wgh-frame).
   ASSIGN wh-CE0106-cb-tp-item = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
          i-tp-item-ant     = ''.
END.

IF p-ind-event  = "BEFORE-ASSIGN" AND
   p-ind-object = "CONTAINER"     THEN
DO:

    FIND b-item NO-LOCK WHERE
         ROWID(b-item) = p-row-table NO-ERROR.
    IF AVAIL b-item THEN DO:
       ASSIGN i-tp-item-ant     = substring(b-item.char-2,212,1).

    END.
END. /* p-ind-event  = "BEFORE-ASSIGN" */

IF p-ind-event  = "AFTER-ASSIGN" AND
   p-ind-object = "CONTAINER"    THEN
DO:

    FIND b-item NO-LOCK WHERE
         ROWID(b-item) = p-row-table NO-ERROR.
    IF AVAIL b-item THEN DO:

        IF substring(b-item.char-2,212,1) <> i-tp-item-ant THEN DO:

            CREATE ext-audit-item-fam.
            ASSIGN ext-audit-item-fam.it-codigo = b-item.it-codigo
                   ext-audit-item-fam.campo     = "tp-item"
                   ext-audit-item-fam.data      = TODAY
                   ext-audit-item-fam.hora      = TIME
                   ext-audit-item-fam.fm-codigo = ""
                   ext-audit-item-fam.programa  = "CE0106"
                   ext-audit-item-fam.usuario   = c-seg-usuario.


            CASE STRING(i-tp-item-ant):
                 WHEN ""  THEN ASSIGN ext-audit-item-fam.valor-ant = "NÆo informado".
                 WHEN "0" THEN ASSIGN ext-audit-item-fam.valor-ant = "0 - Mercadoria para Revenda".
                 WHEN "1" THEN ASSIGN ext-audit-item-fam.valor-ant = "1 - Mat‚ria-prima".
                 WHEN "2" THEN ASSIGN ext-audit-item-fam.valor-ant = "2 - Embalagem".
                 WHEN "3" THEN ASSIGN ext-audit-item-fam.valor-ant = "3 - Produto em Processo".
                 WHEN "4" THEN ASSIGN ext-audit-item-fam.valor-ant = "4 - Produto Acabado".
                 WHEN "5" THEN ASSIGN ext-audit-item-fam.valor-ant = "5 - Subproduto".
                 WHEN "6" THEN ASSIGN ext-audit-item-fam.valor-ant = "6 - Produto Intermediario".
                 WHEN "7" THEN ASSIGN ext-audit-item-fam.valor-ant = "7 - Material de Uso e Consumo".
                 WHEN "8" THEN ASSIGN ext-audit-item-fam.valor-ant = "8 - Ativo Imobilizado".
                 WHEN "9" THEN ASSIGN ext-audit-item-fam.valor-ant = "9 - Servi‡os".
                 WHEN "a" THEN ASSIGN ext-audit-item-fam.valor-ant = "10 - Outros Insumos".
                 WHEN "b" THEN ASSIGN ext-audit-item-fam.valor-ant = "99 - Outras".
            END CASE.

            CASE substring(b-item.char-2,212,1):
                 WHEN ""  THEN ASSIGN ext-audit-item-fam.valor-atu = "NÆo informado".
                 WHEN "0" THEN ASSIGN ext-audit-item-fam.valor-atu = "0 - Mercadoria para Revenda".
                 WHEN "1" THEN ASSIGN ext-audit-item-fam.valor-atu = "1 - Mat‚ria-prima".
                 WHEN "2" THEN ASSIGN ext-audit-item-fam.valor-atu = "2 - Embalagem".
                 WHEN "3" THEN ASSIGN ext-audit-item-fam.valor-atu = "3 - Produto em Processo".
                 WHEN "4" THEN ASSIGN ext-audit-item-fam.valor-atu = "4 - Produto Acabado".
                 WHEN "5" THEN ASSIGN ext-audit-item-fam.valor-atu = "5 - Subproduto".
                 WHEN "6" THEN ASSIGN ext-audit-item-fam.valor-atu = "6 - Produto Intermediario".
                 WHEN "7" THEN ASSIGN ext-audit-item-fam.valor-atu = "7 - Material de Uso e Consumo".
                 WHEN "8" THEN ASSIGN ext-audit-item-fam.valor-atu = "8 - Ativo Imobilizado".
                 WHEN "9" THEN ASSIGN ext-audit-item-fam.valor-atu = "9 - Servi‡os".
                 WHEN "a" THEN ASSIGN ext-audit-item-fam.valor-atu = "10 - Outros Insumos".
                 WHEN "b" THEN ASSIGN ext-audit-item-fam.valor-atu = "99 - Outras".
            END CASE.

        END.

    END. /* AVAIL b-item */

END. /* p-ind-event  = "AFTER-ASSIGN"  */

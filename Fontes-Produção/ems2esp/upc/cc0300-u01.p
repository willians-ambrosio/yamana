/****************************************************************************************** 
** 	   Programa: cc0300-u01.p
**   	  Autor: 
** 	 Fornecedor: 
**         Data: 
** Change/Chamado:
**      Objetivo: Desabilitar botäes quando o contrato for da ilha de dados (CM)
**
******************************** CONTROLE DE ALTERA€åES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
**
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: p-ind-event, p-ind-object, p-wgh-object, p-wgh-frame, p-cod-table e p-row-table
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

DEFINE INPUT PARAMETER p-ind-event   AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object  AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object  AS HANDLE         NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame   AS WIDGET-HANDLE  NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table   AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-row-table   AS ROWID          NO-UNDO.

define new global shared variable l-disable as logical no-undo.

DEF NEW GLOBAL SHARED VAR wh-panel-frame-1 AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-bt-copy       AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-bt-ordens     AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-bt-narrativa  AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-bt-del        AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-bt-integracao AS HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-bt-addSon     AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-bt-updSon     AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-bt-delSon     AS HANDLE NO-UNDO.


DEFINE VARIABLE i-cont AS INTEGER     NO-UNDO.

def new global shared var h-frame as handle no-undo.

/*************************** LOCAL VARIABLE DEFINITIONS **************************/
DEFINE VARIABLE c-objeto       AS CHARACTER      NO-UNDO.

IF p-wgh-object <> ? THEN
    ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:FILE-NAME,'/'),
                                        p-wgh-object:FILE-NAME,'/').


if  p-ind-event  = "BEFORE-INITIALIZE" 
and p-ind-object = "CONTAINER" then do:

    RUN getFieldHandle (INPUT "btCopy",
                        OUTPUT wh-bt-copy).

    RUN getFieldHandle (INPUT "btDelete",
                        OUTPUT wh-bt-del).

    RUN getFieldHandle (INPUT "bt-narrativa",
                        OUTPUT wh-bt-narrativa).

    RUN getFieldHandle (INPUT "btOrdens",
                        OUTPUT wh-bt-ordens).

    RUN getFieldHandle (INPUT "bt-integracao",
                        OUTPUT wh-bt-integracao).
                               
    RUN getFieldHandle (input "fPage1", 
                        output h-frame).

end.

if  p-ind-event  = "BEFORE-DISPLAY" then do:
    assign l-disable = no.
    find first pedido-compr
         where rowid(pedido-compr) = p-row-table no-lock no-error.
    if avail pedido-compr then do:
       IF pedido-compr.check-sum = "CM":U THEN
          assign l-disable = yes.
    end.
end.


if p-ind-event  = "BEFORE-OPEN-QUERY" then do:
   run getFieldHandle (input "btAddSon1"   , output wh-bt-addSon).
   run getFieldHandle (input "btUpdateSon1", output wh-bt-updSon).
   run getFieldHandle (input "btDeleteSon1", output wh-bt-delSon).
end.

if valid-handle(wh-bt-copy) then
   assign wh-bt-copy      :SENSITIVE  = NOT l-disable.
 
if valid-handle(wh-bt-del) then
   assign wh-bt-del       :SENSITIVE  = NOT l-disable.

if valid-handle(wh-bt-narrativa) then
   assign wh-bt-narrativa :SENSITIVE  = NOT l-disable.

if valid-handle(wh-bt-ordens) then
   assign wh-bt-ordens    :SENSITIVE  = NOT l-disable.

if valid-handle(wh-bt-integracao) then
   assign wh-bt-integracao:SENSITIVE  = NOT l-disable.

if valid-handle(wh-bt-addSon) then
   assign wh-bt-addSon    :SENSITIVE  = NOT l-disable.

if valid-handle(wh-bt-updSon) then
   assign wh-bt-updSon    :SENSITIVE  = NOT l-disable.

if valid-handle(wh-bt-delSon) then
   assign wh-bt-delSon    :SENSITIVE  = NOT l-disable.

PROCEDURE getFieldHandle:
    DEF INPUT  PARAMETER p-field   AS CHAR   NO-UNDO.
    DEF OUTPUT PARAMETER p-h-field AS HANDLE NO-UNDO.
    DEF VAR h-f AS HANDLE NO-UNDO.

    ASSIGN h-f = p-wgh-frame:FIRST-CHILD.
    ASSIGN h-f = h-f:FIRST-CHILD.

    if valid-handle(h-frame) then
        h-f = h-frame:FIRST-CHILD.

    DO WHILE h-f <> ? :
       IF h-f:TYPE <> "field-group" THEN DO:
            IF h-f:NAME = p-field THEN DO:
               ASSIGN p-h-field = h-f.
               LEAVE.
            END.
            ASSIGN h-f = h-f:NEXT-SIBLING.
       END. 
       ELSE DO:
         ASSIGN h-f = h-f:FIRST-CHILD.
       END.
    END.
END PROCEDURE.

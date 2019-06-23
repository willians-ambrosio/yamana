/*------------------------------------------------------------------------------------------------------------
  Objetivo: Trigger de Write da Tabela pedido-compr
  Autor: Sergio Luiz Neto da Silveira / DSC PRAXIS
  Data: 20/02/2017
  ------------------------------------------------------------------------------------------------------------*/

def param buffer p-pedido-compr     for pedido-compr.
def param buffer p-old-pedido-compr for pedido-compr.

DEFINE BUFFER b_pedido-compr FOR pedido-compr.

DEFINE NEW GLOBAL SHARED VARIABLE l-emergencial-cc0311 AS LOGICAL NO-UNDO.
DEFINE                   VARIABLE i-cont               AS INTEGER NO-UNDO.

IF R-INDEX(STRING(PROGRAM-NAME(6)),"cc0305") > 0 THEN DO:
   FIND FIRST b_pedido-compr EXCLUSIVE-LOCK
        WHERE b_pedido-compr.num-pedido = p-pedido-compr.num-pedido
        NO-ERROR.
   ASSIGN b_pedido-compr.impr-pedido = YES.
END.

blk:
DO i-cont = 1 TO 10:
   IF R-INDEX(STRING(PROGRAM-NAME(i-cont)),"cc0311") > 0 THEN DO:
      FIND FIRST b_pedido-compr EXCLUSIVE-LOCK
           WHERE b_pedido-compr.num-pedido = p-pedido-compr.num-pedido
           NO-ERROR.
      ASSIGN b_pedido-compr.emergencial = l-emergencial-cc0311.

      ASSIGN l-emergencial-cc0311 = NO.
      
      LEAVE blk.
   END.
END.

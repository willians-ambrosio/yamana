/*******************************************************************************
Extracao de Departamento x Item
Kraft Consulting
18/01/2011
*******************************************************************************/

{esp\KRAFT.I}

DEF VAR i-trans AS INTEGER NO-UNDO.
DELETE FROM es_tipo_transacao.
REPEAT i-trans = 1 TO 37:
   Create es_tipo_transacao.
   Assign es_tipo_transacao.abreviacao = string(i-trans,"99")
          es_tipo_transacao.nome       = {ininc/i03in218.i 04 i-trans}.
END.

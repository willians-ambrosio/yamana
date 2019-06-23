/*******************************************************************************
Extracao de Unidade
Kraft Consulting
*******************************************************************************/
def buffer moeda       for ems2cadme.moeda.

{esp\KRAFT.I}


DELETE FROM es_moeda.
FOR EACH moeda FIELDS(mo-codigo descricao) NO-LOCK:
    Create es_moeda.
    Assign es_moeda.codigo = moeda.mo-codigo
           es_moeda.nome   = moeda.descricao.
END.

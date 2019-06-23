/*******************************************************************************
Extracao de Ccustos
Kraft Consulting
29/12/2010
*******************************************************************************/
def buffer ccusto for ems5.ccusto.

{esp\KRAFT.I}

DELETE FROM es_sub_conta.
FOR EACH ccusto FIELDS(cod_ccusto des_tit_ctbl) NO-LOCK:
    Create es_sub_conta.
    Assign es_sub_conta.codigo = "00" + ccusto.cod_ccusto
           es_sub_conta.nome   = ccusto.des_tit_ctbl.
END.

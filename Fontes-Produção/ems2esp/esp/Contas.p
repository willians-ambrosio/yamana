/*******************************************************************************
Extracao de Conta
Kraft Consulting
29/12/2010
*******************************************************************************/

{esp\KRAFT.I}

DELETE FROM es_conta.
FOR EACH cta_ctbl FIELDS(cod_cta_ctbl des_tit_ctbl) no-lock
   where cta_ctbl.cod_plano_cta_ctbl = "CONTSOC":
    Create es_conta.
    Assign es_conta.codigo = cta_ctbl.cod_cta_ctbl
           es_conta.nome   = cta_ctbl.des_tit_ctbl. 
END.

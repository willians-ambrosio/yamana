/*******************************************************************************
Extracao de Centro de Custos
Kraft Consulting
29/12/2010
*******************************************************************************/
def buffer empresa for ems2cadme.empresa.
def buffer ccusto for ems5.ccusto.

{esp\KRAFT.I}

FIND FIRST param-global NO-LOCK NO-ERROR.

DELETE FROM es_centro_custo WHERE es_centro_custo.empresa = string(empresa-prin).

def new global shared var v_cod_empres_usuar  like mguni.empresa.ep-codigo no-undo.
def new global shared var i-ep-codigo-usuario like mguni.empresa.ep-codigo no-undo.

find first plano_ccusto no-lock
     where plano_ccusto.cod_empresa     = v_cod_empres_usuar
       and plano_ccusto.dat_inic_valid <= today
       and plano_ccusto.dat_fim_valid  >= today no-error.

FOR EACH ccusto no-lock
   where ccusto.cod_empresa      = plano_ccusto.cod_empresa
     and ccusto.cod_plano_ccusto = plano_ccusto.cod_plano_ccusto:
    Create es_centro_custo.
    Assign es_centro_custo.codigo  = '00' + ccusto.cod_ccusto
           es_centro_custo.nome    = ccusto.des_tit_ctbl
           es_centro_custo.empresa = i-ep-codigo-usuario.
END.

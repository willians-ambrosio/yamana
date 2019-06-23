/*******************************************************************************
Extracao de Contas Contabil
Kraft Consulting
29/12/2010
*******************************************************************************/

/*{D:\Fontes\quarentena\Kraft\KRAFT.I}*/
{esp\KRAFT.I}

DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.

/*FOR EACH es_conta_ctbl.
    DELETE es_conta_ctbl.
END.

FOR EACH conta-contab NO-LOCK:
    Create es_conta_ctbl.
    Assign es_conta_ctbl.id             = string(conta-contab.conta-contab)
           es_conta_ctbl.ep_codigo      = (conta-contab.ep-codigo)       /*chave primaria e unica*/
           es_conta_ctbl.cod_cta_ctbl   = STRING(conta-contab.ct-codigo) /*chave primaria e unica*/
           es_conta_ctbl.sub_cta_ctbl   = STRING(conta-contab.sc-codigo) /*chave primaria e unica*/
           es_conta_ctbl.des_tit_ctbl   = STRING(conta-contab.titulo).
END.*/

/*RUN D:\DadosBI\Extratores\disconnect.p.*/

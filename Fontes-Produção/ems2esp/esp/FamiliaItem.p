/*******************************************************************************
Extracao de Familia Item
Kraft Consulting
04/01/2011
*******************************************************************************/

{esp\KRAFT.I}

DELETE FROM es_familia.
FOR EACH familia FIELDS(fm-codigo descricao) NO-LOCK USE-INDEX codigo:
    Create es_familia.
    Assign es_familia.codigo = familia.fm-codigo
           es_familia.nome   = familia.descricao.
END.

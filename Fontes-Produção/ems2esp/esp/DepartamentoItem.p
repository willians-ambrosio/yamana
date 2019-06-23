/*******************************************************************************
Extracao de Departamento x Item
Kraft Consulting
18/01/2011
*******************************************************************************/

/*{D:\Fontes\quarentena\Kraft\KRAFT.I}*/
{esp\KRAFT.I}
                                        

DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.

FOR EACH es-it-depto NO-LOCK:

    FIND FIRST ITEM
        WHERE ITEM.it-codigo = STRING(es-it-depto.it-codigo) NO-LOCK NO-ERROR.

    IF NOT AVAIL ITEM  THEN NEXT.

    FIND FIRST es_it_departamento
        WHERE es_it_departamento.codigo = es-it-depto.it-codigo  NO-ERROR.

    IF NOT AVAIL es_it_departamento THEN DO:
        Create es_it_departamento.
        Assign es_it_departamento.codigo    =  STRING(es-it-depto.it-codigo). /*chave primaria e unica*/
    END.

    ASSIGN es_it_departamento.nome      =  string(es-it-depto.cod-depto).
    
END.


/*RUN D:\DadosBI\Extratores\disconnect.p.*/




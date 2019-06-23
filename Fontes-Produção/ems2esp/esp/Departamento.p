/*******************************************************************************
Extracao de Departamentos
Kraft Consulting
18/01/2011
*******************************************************************************/

/*{D:\Fontes\quarentena\Kraft\KRAFT.I}*/
{esp\KRAFT.I}

DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.

FOR EACH es-depto NO-LOCK:

    FIND FIRST es_departamento
        WHERE es_departamento.codigo = es-depto.codigo NO-ERROR. /*chave primaria e unica*/
        
    IF NOT AVAIL es_departamento THEN DO:
        Create es_departamento.
        Assign es_departamento.codigo      =  es-depto.codigo .
    END. 

    ASSIGN es_departamento.nome        =  STRING(es-depto.descricao).
    
END.

/*RUN D:\DadosBI\Extratores\disconnect.p.*/




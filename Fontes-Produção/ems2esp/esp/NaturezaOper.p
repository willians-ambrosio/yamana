/*******************************************************************************
Extracao de Natureza d Operacao
Kraft Consulting
*******************************************************************************/

/*{D:\Fontes\quarentena\Kraft\KRAFT.I}*/
{esp\KRAFT.I}

DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.

FOR EACH natur-oper NO-LOCK:

    FIND FIRST es_natur_oper
        WHERE es_natur_oper.codigo  = natur-oper.nat-operacao  NO-ERROR.

    IF NOT AVAIL es_natur_oper THEN DO:
        Create es_natur_oper.
        Assign es_natur_oper.codigo     =  string(natur-oper.nat-operacao). /*chave primaria e unica*/


    END.

    ASSIGN es_natur_oper.descricao      = string(natur-oper.denominacao)
           es_natur_oper.entradaSaida   = natur-oper.tipo.  /*1 - Entrada , 2 - Saida*/
        
END.



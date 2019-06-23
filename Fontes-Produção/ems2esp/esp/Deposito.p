/*******************************************************************************
Extracao de Estabelecimentos
Kraft Consulting
30/12/2010
*******************************************************************************/

/*{D:\Fontes\quarentena\Kraft\KRAFT.I}*/
{esp\KRAFT.I}

DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.

FOR EACH estabelec NO-LOCK.
    FOR EACH deposito NO-LOCK:
    
        FIND FIRST es_deposito
            WHERE es_deposito.codEstab = estabelec.cod-estabel
              AND es_deposito.codigo   = deposito.cod-depos  NO-ERROR.
    
        IF NOT AVAIL es_deposito THEN DO:
            Create es_deposito.
            Assign es_deposito.codEstab  =  STRING(estabelec.cod-estabel) /*chave primaria e unica*/   
                   es_deposito.codigo    =  STRING(deposito.cod-depos).   /*chave primaria e unica*/   
        END.
    
        ASSIGN es_deposito.nome      =  string(deposito.nome).
        
    END.
END.



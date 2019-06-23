/*******************************************************************************
Extracao de Unidade
Kraft Consulting
*******************************************************************************/
/*{D:\Fontes\quarentena\Kraft\KRAFT.I}*/
{esp\KRAFT.I}

DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.


FOR EACH tab-unidade NO-LOCK:

    FIND FIRST es_tab_unidade
        WHERE es_tab_unidade.codigo =  tab-unidade.un NO-ERROR.

    IF NOT AVAIL es_tab_unidade THEN DO:
        Create es_tab_unidade.
        Assign es_tab_unidade.codigo    = (tab-unidade.un).
    END.

    ASSIGN es_tab_unidade.descricao     = string(tab-unidade.descricao).
                              
    
END.


/*RUN D:\DadosBI\Extratores\disconnect.p.*/




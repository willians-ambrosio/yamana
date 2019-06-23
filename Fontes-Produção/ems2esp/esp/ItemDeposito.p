/*******************************************************************************
Extracao de Item Deposito
Kraft Consulting
30/12/2010
*******************************************************************************/

/*{D:\Fontes\quarentena\Kraft\KRAFT.I}*/
{esp\KRAFT.I}

DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.


FOR EACH item-uni-estab NO-LOCK:

    FIND FIRST es_item_uni_estab
        WHERE es_item_uni_estab.codEstab   = item-uni-estab.cod-estabel
          AND es_item_uni_estab.codItem    = item-uni-estab.it-codigo NO-ERROR.

    IF NOT AVAIL es_item_uni_estab THEN DO:
        Create es_item_uni_estab.
        Assign es_item_uni_estab.codEstab     = string(item-uni-estab.cod-estabel)  
               es_item_uni_estab.codItem      = string(item-uni-estab.it-codigo)    .

    END.
    
    ASSIGN es_item_uni_estab.codDeposito  = STRING(item-uni-estab.deposito-pad) .
           
    
END.


/*RUN D:\DadosBI\Extratores\disconnect.p.*/




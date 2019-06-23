/*******************************************************************************
Extracao de Cotacao
Kraft Consulting
*******************************************************************************/

/*{D:\Fontes\quarentena\Kraft\KRAFT.I}*/
{esp\KRAFT.I}


DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.



/* a tabela es_cotacao esta sendo preenchida por uma outra rotina executada diariamente
DEFINE TEMP-TABLE ttExtrator
    FIELD dimMoeda          AS CHAR
    FIELD dimPeriodo        AS CHAR
    FIELD medCotacaoMedia   AS DECIMAL
    FIELD medCotacaoMensal  AS DECIMAL
    FIELD medCotacaoValor   AS DECIMAL EXTENT 31 
    .

DEF VAR i AS INTEGER INITIAL 1.

FOR EACH cotacao NO-LOCK:

    Create ttExtrator.
    Assign dimMoeda           = string(cotacao.mo-codigo)
           dimPeriodo         = string(cotacao.ano-periodo)
           medCotacaoMedia    = cotacao.cota-media   
           medCotacaoMensal   = cotacao.cota-mensal.

    REPEAT i = 1 TO 31:
        medCotacaoValor[i]    = cotacao.cotacao[i].
    END.
           

END.

ASSIGN c-arquivo = TRIM(dir-extrat) + "\BICotacaoMoeda.txt".
ASSIGN c-arquivo = REPLACE(c-arquivo,"\\","\").

RUN piExportaExtrator(INPUT "FOR EACH ttExtrator", "ttExtrator", c-arquivo).

for each ttExtrator:
    delete ttExtrator.
end.
*/


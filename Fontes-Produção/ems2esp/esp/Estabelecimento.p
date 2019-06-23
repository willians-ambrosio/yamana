/*******************************************************************************
Extracao de Estabelecimentos
Kraft Consulting
28/12/2010
*******************************************************************************/

/*{D:\Fontes\quarentena\Kraft\KRAFT.I}*/
{esp\KRAFT.I}

/* Esta sendo gerado por outro processo
DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE ttExtrator
    FIELD dimEstabelec         AS CHAR
    FIELD dimNomeEstabelec     AS CHAR.

FOR EACH estabelec NO-LOCK:

    Create ttExtrator.
    Assign dimEstabelec       = STRING(estabelec.cod-estabel) 
           dimNomeEstabelec   = string(estabelec.nome)        .
    
END.

ASSIGN c-arquivo = TRIM(dir-extrat) + "\BIEstabelecimento.txt".
ASSIGN c-arquivo = REPLACE(c-arquivo,"\\","\").

RUN piExportaExtrator(INPUT "FOR EACH ttExtrator", "ttExtrator", c-arquivo).


for each ttExtrator:
    delete ttExtrator.
end.

/*RUN D:\DadosBI\Extratores\disconnect.p.*/

*/

/*******************************************************************************
Extracao de Emitentes
Kraft Consulting
28/12/2010
*******************************************************************************/

/*{D:\Fontes\quarentena\Kraft\KRAFT.I}*/
{esp\KRAFT.I}

/* esta sendo gerado por um outro processo
DEFINE INPUT  PARAMETER dir-extrat AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-arquivo AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE ttExtrator
    FIELD dimCodEmit         AS CHAR
    FIELD dimNomeEmit        AS CHAR
    FIELD dimUf              AS CHAR
    FIELD dimCidade          AS CHAR
    FIELD dimPais            AS CHAR.

FOR EACH emitente NO-LOCK:

    Create ttExtrator.
    Assign dimCodEmit      = STRING(emitente.cod-emitente)
           dimNomeEmit     = STRING(emitente.nome-emit)
           dimUf           = STRING(emitente.estado)
           dimCidade       = STRING(emitente.cidade)
           dimPais         = STRING(emitente.pais).

END.

ASSIGN c-arquivo = TRIM(dir-extrat) + "\BIEmitente.txt".
ASSIGN c-arquivo = REPLACE(c-arquivo,"\\","\").

RUN piExportaExtrator(INPUT "FOR EACH ttExtrator", "ttExtrator", c-arquivo).

for each ttExtrator:
    delete ttExtrator.
end.

/*RUN D:\DadosBI\Extratores\disconnect.p.*/
*/

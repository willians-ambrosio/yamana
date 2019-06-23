{include/i-prgvrs.i ES0002 2.00.00.000} /*nome e vers∆o do programa*/

define buffer doc-orig-nfe for ems2cademp.doc-orig-nfe.

define temp-table tt-param NO-UNDO /*definiá∆o de temp-tables*/
    field destino          as integer
    field arquivo          as char format "x(35)":U
    field usuario          as char format "x(12)":U
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)":U
    field modelo           AS char format "x(35)":U
    /*Alterado 15/02/2005 - tech1007 - Criado campo l¢gico para verificar se o RTF foi habilitado*/
    field l-habilitaRtf    as LOG
    /*Fim alteracao 15/02/2005*/
    FIELD cod-estabel-ini  LIKE doc-orig-nfe.cod-estabel
    FIELD cod-estabel-fim  LIKE doc-orig-nfe.cod-estabel
    FIELD cod-emitente-ini LIKE doc-orig-nfe.cod-emitente
    FIELD cod-emitente-fim LIKE doc-orig-nfe.cod-emitente
    FIELD serie-ini        LIKE doc-orig-nfe.serie
    FIELD serie-fim        LIKE doc-orig-nfe.serie
    FIELD nro-docto-ini    LIKE doc-orig-nfe.nro-docto
    FIELD nro-docto-fim    LIKE doc-orig-nfe.nro-docto
    FIELD dt-emissao-ini   LIKE doc-orig-nfe.dt-emissao
    FIELD dt-emissao-fim   LIKE doc-orig-nfe.dt-emissao
    FIELD l-excel          AS LOGICAL.

define temp-table tt-raw-digita no-undo
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

DEFINE VARIABLE h-acomp    AS HANDLE                   NO-UNDO.
DEFINE VARIABLE c-situacao AS CHARACTER FORMAT "x(25)" NO-UNDO.
DEFINE VARIABLE data-aux   AS DATE                     NO-UNDO.
DEFINE VARIABLE c-destino  AS CHARACTER                NO-UNDO.
DEFINE VARIABLE c-arq-csv  AS CHARACTER                NO-UNDO.

DEFINE STREAM s-csv-req.
CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

CASE tt-param.destino:
    WHEN 1 THEN
        ASSIGN c-destino = "Impressora".
    WHEN 2 THEN
        ASSIGN c-destino = "Arquivo".
    WHEN 3 THEN
        ASSIGN c-destino = "Terminal".
END CASE.

ASSIGN c-arq-csv = REPLACE(tt-param.arquivo, "tmp", "csv").

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

FORM
    doc-orig-nfe.cod-estabel
    doc-orig-nfe.cod-emitente
    doc-orig-nfe.serie-docto
    doc-orig-nfe.nro-docto
    c-situacao COLUMN-LABEL "Situaá∆o"
    WITH WIDTH 132 NO-BOX DOWN STREAM-IO FRAME f-situacao.

{include/i-rpvar.i}
{include/i-rpout.i &STREAM="stream str-rp"}
{include/i-rpcab.i &STREAM="str-rp"}

RUN pi-inicializar IN h-acomp(INPUT "Inicializando").
RUN pi-seta-titulo IN h-acomp(INPUT "Verificando Documentos").
VIEW STREAM str-rp FRAME f-cabec.
VIEW STREAM str-rp FRAME f-rodape.

IF tt-param.l-excel THEN DO:
    OUTPUT STREAM s-csv-req TO VALUE(c-arq-csv) CONVERT TARGET 'iso8859-1'.
    PUT STREAM s-csv-req UNFORMATTED "Estabelecimento;Emitente;Serie;Documento;Situacao;". 
    PUT STREAM s-csv-req SKIP.

END.


FOR EACH doc-orig-nfe
   WHERE doc-orig-nfe.cod-estabel  >= tt-param.cod-estabel-ini
    AND  doc-orig-nfe.cod-estabel  <= tt-param.cod-estabel-fim
    AND  doc-orig-nfe.cod-emitente >= tt-param.cod-emitente-ini
    AND  doc-orig-nfe.cod-emitente <= tt-param.cod-emitente-fim
    AND  doc-orig-nfe.serie-docto  >= tt-param.serie-ini
    AND  doc-orig-nfe.serie-docto  <= tt-param.serie-fim
    AND  doc-orig-nfe.nro-docto    >= tt-param.nro-docto-ini
    AND  doc-orig-nfe.nro-docto    <= tt-param.nro-docto-fim
    AND  doc-orig-nfe.dt-emissao   >=  tt-param.dt-emissao-ini 
    AND  doc-orig-nfe.dt-emissao   <=  tt-param.dt-emissao-fim
    AND  doc-orig-nfe.idi-orig-trad = 2 NO-LOCK:

    IF tt-param.l-excel = NO THEN DO:

        DISP STREAM str-rp
             doc-orig-nfe.cod-estabel
             doc-orig-nfe.cod-emitente
             doc-orig-nfe.serie-docto
             doc-orig-nfe.nro-docto
             {ininc/i01in847.i 04 doc-orig-nfe.idi-situacao} @ c-situacao
             WITH STREAM-IO FRAME f-situacao.
        DOWN WITH STREAM-IO FRAME f-situacao.

    END.

    ELSE DO:

        PUT STREAM s-csv-req UNFORMATTED
             doc-orig-nfe.cod-estabel ";"
             doc-orig-nfe.cod-emitente ";"
             doc-orig-nfe.serie-docto ";"
             doc-orig-nfe.nro-docto ";"
             {ininc/i01in847.i 04 doc-orig-nfe.idi-situacao} ";"
             .

        PUT STREAM s-csv-req SKIP.

    END.

    
END.

PAGE STREAM str-rp.

PUT STREAM str-rp
           "SELECAO"                  AT 01 SKIP(1)
           "Estabelecimento:"         AT 05 tt-param.cod-estabel-ini           AT 28 "|< >|" AT 38 tt-param.cod-estabel-fim AT 45 SKIP
           "Emitente:"                AT 05 tt-param.cod-emitente-ini          AT 28 "|< >|" AT 38 tt-param.cod-emitente-fim AT 45 SKIP
           "Serie:"                   AT 05 tt-param.serie-ini                 AT 28 "|< >|" AT 38 tt-param.serie-fim AT 45 SKIP
           "Nrß Docto:"               AT 05 right-trim(tt-param.nro-docto-ini) AT 28 "|< >|" AT 38 tt-param.nro-docto-fim AT 45 SKIP
           "Data Emiss∆o:"            AT 05 tt-param.dt-emissao-ini            AT 28 "|< >|" AT 38 tt-param.dt-emissao-fim AT 45 SKIP(1)
           "IMPRESSAO"                AT 01 SKIP(1)
           "Destino:"                 AT 05 c-destino " - " tt-param.arquivo SKIP
           "Usuario:"                 AT 05 tt-param.usuario.

{include/i-rpclo.i &STREAM="stream str-rp"}

RUN pi-finalizar IN h-acomp.

/******************************************************************
**
** Programa: UPC-ESRA100-U01
**
** Objetivo: Programa chamador de UPC
**
**    Autor: Renato
**
**     Data: Dezembro/2018
**
**   Versao: 2.12.00.000 - Desenvolvimento Inicial
**
******************************************************************/
{include/i-prgvrs.i UPC-ESRA100-U01 12.01.22.000}

{tools/fc-handle-obj.i}

/* Definicao de Parametros */
define input parameter p-ind-event   as char          no-undo.
define input parameter p-ind-object  as char          no-undo.
define input parameter p-wgh-object  as handle        no-undo.
define input parameter p-wgh-frame   as widget-handle no-undo.
define input parameter p-cod-table   as char          no-undo.
define input parameter p-row-table   as rowid         no-undo.

/* Definicao de Variaveis Globais */
DEF NEW GLOBAL SHARED VAR wh-esra100-br-cond    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-esra100-qr-cond    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-esra100-situacao   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-esra100-serie      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-esra100-nr-docto   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-esra100-nome-abrev AS WIDGET-HANDLE NO-UNDO.

/* Definicao de Variaveis Locais */
DEF VAR i-contador            AS INT           NO-UNDO.
def var c-handle-obj          AS CHAR          NO-UNDO.
DEF VAR wh-buffer             AS WIDGET-HANDLE NO-UNDO.
DEF VAR wh-field-cod-cond-pag AS WIDGET-HANDLE NO-UNDO.
DEF VAR wh-field-nr-pedido    AS WIDGET-HANDLE NO-UNDO.

/*     MESSAGE "p-ind-event..:" p-ind-event            SKIP */
/*         "p-ind-object.:" p-ind-object           SKIP     */
/*         "p-cod-table..:" STRING(p-cod-table)    SKIP     */
/*         "p-row-table..:" STRING(p-row-table)    SKIP     */
/*         "p-wgh-object.:" p-wgh-object:FILE-NAME SKIP     */
/*         "p-wgh-frame..:" STRING(p-wgh-frame)    SKIP     */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                   */

IF  p-ind-event  = "initialize" 
AND p-ind-object = "container" THEN DO:

    IF NOT(VALID-HANDLE(wh-esra100-br-cond)) THEN DO:
        assign c-handle-obj       = fc-handle-obj("br-doctos",p-wgh-frame).
               wh-esra100-br-cond = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
               wh-esra100-qr-cond = wh-esra100-br-cond:QUERY.

       /*----- CRIA CAMPO PARA BROWSE ------------------------*/
       wh-esra100-situacao = wh-esra100-br-cond:ADD-CALC-COLUMN("CHAR","X(15)","","Sit.RE",10).

       ON "row-display" OF wh-esra100-br-cond PERSISTENT RUN upc/upc-esra100-u01.p (INPUT "ROW-DISPLAY",
                                                                                    INPUT "BROWSER",
                                                                                    INPUT  p-wgh-object,
                                                                                    INPUT  p-wgh-frame,
                                                                                    INPUT  p-cod-table,
                                                                                    INPUT  p-row-table).
    END.

END.

IF  p-ind-event  = "ROW-DISPLAY" 
AND p-ind-object = "BROWSER" THEN DO:

    IF VALID-HANDLE(wh-esra100-qr-cond) THEN DO:

        DO i-contador = 1 TO wh-esra100-qr-cond:NUM-BUFFERS:
    
            wh-buffer = wh-esra100-qr-cond:GET-buffer-handle(i-contador).

            IF wh-buffer:NAME = "nfe-dfe" THEN do:
    
                ASSIGN wh-esra100-situacao:SCREEN-VALUE = "Pendente".
                       wh-esra100-serie      = wh-buffer:BUFFER-FIELD("serie").
                       wh-esra100-nr-docto   = wh-buffer:BUFFER-FIELD("nr-docto").
                       wh-esra100-nome-abrev = wh-buffer:BUFFER-FIELD("nome-abrev").

                FIND FIRST emitente NO-LOCK
                     WHERE emitente.nome-abrev = wh-esra100-nome-abrev:BUFFER-VALUE NO-ERROR.
                IF AVAIL emitente THEN DO:

                    FIND FIRST docum-est NO-LOCK
                         WHERE docum-est.serie-docto  = wh-esra100-serie:BUFFER-VALUE
                           AND docum-est.nro-docto    = wh-esra100-nr-docto:BUFFER-VALUE 
                           AND docum-est.cod-emitente = emitente.cod-emitente NO-ERROR.
                    IF AVAIL docum-est THEN DO:

                        IF docum-est.ce-atual THEN
                            ASSIGN wh-esra100-situacao:SCREEN-VALUE = "Registrada".
                        ELSE
                            ASSIGN wh-esra100-situacao:SCREEN-VALUE = "Liberado Nota".

                    END.
                    ELSE DO:

                        FIND FIRST doc-fisico NO-LOCK
                             WHERE doc-fisico.serie-docto  = wh-esra100-serie:BUFFER-VALUE
                               AND doc-fisico.nro-docto    = wh-esra100-nr-docto:BUFFER-VALUE 
                               AND doc-fisico.cod-emitente = emitente.cod-emitente NO-ERROR.
                        IF AVAIL doc-fisico THEN DO:

                            FIND FIRST es-doc-fisico NO-LOCK
                                 WHERE es-doc-fisico.serie-docto  = wh-esra100-serie:BUFFER-VALUE
                                   AND es-doc-fisico.nro-docto    = wh-esra100-nr-docto:BUFFER-VALUE 
                                   AND es-doc-fisico.cod-emitente = emitente.cod-emitente NO-ERROR.
                            IF NOT AVAIL es-doc-fisico THEN DO:
                                CREATE es-doc-fisico.
                                ASSIGN es-doc-fisico.serie-docto  = wh-esra100-serie:BUFFER-VALUE
                                       es-doc-fisico.nro-docto    = wh-esra100-nr-docto:BUFFER-VALUE 
                                       es-doc-fisico.cod-emitente = emitente.cod-emitente
                                       es-doc-fisico.situacao     = "Contagem".
                            END.
    
                            ASSIGN wh-esra100-situacao:SCREEN-VALUE = es-doc-fisico.situacao.

                        END.

                    END.

                END.

    
            END.
    
        END.
    
    END.

END.

RETURN "OK".

/* Fim do Programa */

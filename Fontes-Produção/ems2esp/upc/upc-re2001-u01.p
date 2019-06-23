/*****************************************************************
**
**    Programa: upc-re2001-u01
**
**    Objetivo: Atualizacao de Notas no Recebimento
**
**       Autor: Renato Oliveira
**
** Atualizacao: Dezembro / 2018
**
**      Versao: 2.12.00.000 - Desenvolvimento Inicial
**
*****************************************************************/
{include/i-prgvrs.i UPC-RE2001-U01 2.12.00.000}

/* Definicao de Funcoes */
{tools/fc-handle-obj.i}

/* Definicao de Parametros */
DEF INPUT PARAMETER p-ind-event  AS CHAR          NO-UNDO.
DEF INPUT PARAMETER p-ind-object AS CHAR          NO-UNDO.
DEF INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEF INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAMETER p-cod-table  AS CHAR          NO-UNDO.
DEF INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

/* Definicao de Temp-Table */
define temp-table tt-param no-undo
    field destino   as integer
    field arquivo   as char
    field usuario   as char
    field data-exec as date
    field hora-exec as INTEGER.

{cdp/cd0666.i}

/* Definicao de Variaveis Globais */
DEF NEW GLOBAL SHARED VAR wh-re2001-status-txt     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2001-status         AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2001-bt-itens       AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2001-bt-libera      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2001-bt-atualizar   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2001-bt-manut-conta AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2001-bt-imp-status  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2001-c-emitente     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-doc-fisico            AS ROWID         NO-UNDO.

/* Definicao de Variaveis Locais */
DEF VAR c-handle-obj AS CHAR      NO-UNDO.
DEF VAR h-handle     AS HANDLE    NO-UNDO.
DEF VAR i-contador   AS INT       NO-UNDO.
DEF VAR raw-param    AS RAW       NO-UNDO.
DEF VAR c-arquivo    AS CHAR      NO-UNDO.
DEF VAR l-ok         AS LOGICAL   NO-UNDO.
DEF VAR h-acomp      AS HANDLE    NO-UNDO.
DEF VAR c-cabec1     AS CHAR      NO-UNDO.
DEF VAR c-cabec2     AS CHAR      NO-UNDO.

/* Definicao de Buffer */
DEF NEW GLOBAL SHARED VAR c-seg-usuario  AS CHAR FORMAT "x(12)" NO-UNDO.

/* MESSAGE "p-ind-event..:" p-ind-event            SKIP  */
/*         "p-ind-object.:" p-ind-object           SKIP  */
/*         "p-wgh-object.:" p-wgh-object:FILE-NAME SKIP  */
/*         "p-wgh-frame..:" p-wgh-frame:NAME       SKIP  */
/*         "p-cod-table..:" STRING(p-cod-table)    SKIP  */
/*         "p-row-table..:" STRING(p-row-table)    SKIP  */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                */

IF  p-ind-event  = "INITIALIZE"
AND p-ind-object = "CONTAINER" THEN DO:

    ASSIGN c-handle-obj = fc-handle-obj("bt-atualizar,bt-manut-conta",p-wgh-frame)
           wh-re2001-bt-atualizar   = WIDGET-HANDLE(ENTRY(1,c-handle-obj))
           wh-re2001-bt-manut-conta = WIDGET-HANDLE(ENTRY(2,c-handle-obj)) NO-ERROR.

    IF VALID-HANDLE(wh-re2001-bt-manut-conta) THEN DO:
    
        create button wh-re2001-bt-imp-status
        assign frame     = p-wgh-frame
               width     = wh-re2001-bt-manut-conta:WIDTH
               height    = wh-re2001-bt-manut-conta:HEIGHT
               row       = wh-re2001-bt-manut-conta:ROW
               col       = wh-re2001-bt-manut-conta:COL - 10
               tooltip   = "Imprime Movimentaá∆o do Documento"
               label     = "Impress∆o Itens"
               sensitive = YES
               visible   = yes
        triggers:
              ON CHOOSE PERSISTENT RUN upc/upc-re2001-u01.p(INPUT "choose",
                                                            INPUT "wh-re2001-bt-imp-status",
                                                            INPUT  p-wgh-object,
                                                            INPUT  p-wgh-frame,
                                                            INPUT  "",
                                                            INPUT  ?).
        end triggers.

      wh-re2001-bt-imp-status:LOAD-IMAGE-UP         (wh-re2001-bt-manut-conta:IMAGE-UP         ).
      wh-re2001-bt-imp-status:LOAD-IMAGE-DOWN       (wh-re2001-bt-manut-conta:IMAGE-DOWN       ).
      wh-re2001-bt-imp-status:LOAD-IMAGE-INSENSITIVE(wh-re2001-bt-manut-conta:IMAGE-INSENSITIVE).
    
    END.

END.

IF  p-ind-event  = "INITIALIZE"
AND p-ind-object = "VIEWER"
AND p-wgh-object:FILE-NAME MATCHES "*v09in163*" THEN DO:

    ASSIGN c-handle-obj = fc-handle-obj("c-emitente",p-wgh-frame)
           wh-re2001-c-emitente  = WIDGET-HANDLE(ENTRY(1,c-handle-obj)).

    CREATE TEXT wh-re2001-status-txt
    ASSIGN FRAME         =  p-wgh-frame
           WIDTH         =  11
           HEIGHT        =  0.88
           ROW           =  wh-re2001-c-emitente:ROW
           COL           =  wh-re2001-c-emitente:COL + 43
           FORMAT        = "x(11)"
           SCREEN-VALUE  = "Status:"
           SENSITIVE     =  NO
           VISIBLE       =  YES.

    CREATE FILL-IN wh-re2001-status
    ASSIGN FRAME             =  p-wgh-frame
           WIDTH             =  15
           HEIGHT            =  wh-re2001-status-txt:HEIGHT
           DATA-TYPE         = "CHARACTER"
           FORMAT            =  "X(14)"
           ROW               =  wh-re2001-status-txt:ROW
           COL               =  wh-re2001-status-txt:COL + 5
           SENSITIVE         =  NO
           SIDE-LABEL-HANDLE =  wh-re2001-status-txt:HANDLE
           VISIBLE           =  YES
           NAME              =  "status"
    TRIGGERS:
    END TRIGGERS.

END.

IF  p-ind-event  = "INITIALIZE"
AND p-ind-object = "BROWSER"
AND p-wgh-object:FILE-NAME MATCHES "*b06in163*"  THEN DO:

     ASSIGN c-handle-obj = fc-handle-obj("bt-itens",p-wgh-frame)
            wh-re2001-bt-itens  = WIDGET-HANDLE(ENTRY(1,c-handle-obj)).

     create button wh-re2001-bt-libera
     assign frame     = p-wgh-frame
            width     = wh-re2001-bt-itens:WIDTH
            height    = wh-re2001-bt-itens:HEIGHT
            row       = wh-re2001-bt-itens:ROW
            col       = wh-re2001-bt-itens:COL + 25.5
            label     = "Liberar Nota"
            sensitive = YES
            visible   = yes
     triggers:
           ON CHOOSE PERSISTENT RUN upc/upc-re2001-u01.p(INPUT "choose",
                                                         INPUT "wh-re2001-bt-libera",
                                                         INPUT  p-wgh-object,
                                                         INPUT  p-wgh-frame,
                                                         INPUT  "",
                                                         INPUT  ?).
     end triggers.

END.

IF  p-ind-event  = "Delete"
AND p-ind-object = "Viewer"
AND p-wgh-object:FILE-NAME MATCHES "*v09in163*" THEN DO:

    FOR FIRST doc-fisico NO-LOCK
        WHERE ROWID(doc-fisico) = p-row-table:

        FOR EACH es-doc-fisico EXCLUSIVE-LOCK OF doc-fisico:
            DELETE es-doc-fisico.
        END.

        FOR EACH es-doc-fisico-just EXCLUSIVE-LOCK OF doc-fisico:
            DELETE es-doc-fisico-just.
        END.

    END.

END.


IF  p-ind-event  = "Display"
AND p-ind-object = "Viewer"
AND p-wgh-object:FILE-NAME MATCHES "*v09in163*" THEN DO:

    IF VALID-HANDLE(wh-re2001-status) THEN DO:

        FOR FIRST doc-fisico NO-LOCK
            WHERE ROWID(doc-fisico) = p-row-table:

            FIND FIRST emitente
                 WHERE emitente.cod-emitente = doc-fisico.cod-emitente NO-LOCK NO-ERROR.

            FIND FIRST nfe-dfe NO-LOCK 
                 WHERE nfe-dfe.cod-estabel = doc-fisico.cod-estabel
                   AND nfe-dfe.serie       = doc-fisico.serie-docto
                   AND nfe-dfe.nr-docto    = doc-fisico.nro-docto
                   AND nfe-dfe.nome-abrev  = emitente.nome-abrev NO-ERROR.
            IF AVAIL nfe-dfe THEN DO:

                FIND FIRST es-doc-fisico NO-LOCK OF doc-fisico NO-ERROR.
                IF AVAIL es-doc-fisico THEN
                    ASSIGN wh-re2001-status:SCREEN-VALUE = es-doc-fisico.situacao.
                ELSE DO:
                    CREATE es-doc-fisico.
                    ASSIGN es-doc-fisico.serie-docto  = doc-fisico.serie-docto
                           es-doc-fisico.nro-docto    = doc-fisico.nro-docto
                           es-doc-fisico.cod-emitente = doc-fisico.cod-emitente
                           es-doc-fisico.tipo-nota    = doc-fisico.tipo-nota NO-ERROR.
                    ASSIGN wh-re2001-status:SCREEN-VALUE = "Contagem".
                END.

            END.
            ELSE
                ASSIGN wh-re2001-status:SCREEN-VALUE = "".

            /*
            Status:
            1)	Pendente - Utilizado todas as vezes que o documento for integrado do programa de Triagem para o RE2001.
            2)	Contagem - Utilizado todas as vezes que o almoxarifado iniciar o processo de contagem dos itens (RE2002)
            3)	Divergente - Utilizado todas as vezes que houver diferenáa entre o RE2002 - Contagem F°sica e informaá‰es da quantidade do pedido.
            4)	Liberado Nota - Utilizado todas as vezes que o documento for liberado atravÇs do bot∆o espec°fico que ser† criado no programa RE2001.
            */

        END.

    END.
    
END.

IF  p-ind-event  = "choose"
AND p-ind-object = "wh-re2001-bt-libera" THEN DO:

    FIND FIRST param-re NO-LOCK
         WHERE param-re.usuario = c-seg-usuario NO-ERROR.
    IF NOT AVAIL param-re THEN DO:
        RUN utp/ut-msgs.p(INPUT "show",
                          INPUT 17006,
                          INPUT "Usu†rio n∆o existe no ParÉmetro do Recebimento" + "~~" +
                                "Usu†rio n∆o existe no ParÉmetro do Recebimento. Favor cadastrar no programa RE0101").
        RETURN "NOK".
    END.

    IF param-re.log-livre-2 = NO THEN DO:
        RUN utp/ut-msgs.p(INPUT "show",
                          INPUT 17006,
                          INPUT "Usu†rio sem Permiss∆o para Liberar o Documento" + "~~" +
                                "Usu†rio sem Permiss∆o para Liberar o Documento. Favor cadastrar no programa RE0101 em ParÉmetros 4").
        RETURN "NOK".
    END.

    FOR FIRST doc-fisico NO-LOCK
        WHERE ROWID(doc-fisico) = gr-doc-fisico:

        FIND FIRST es-doc-fisico-just NO-LOCK OF doc-fisico NO-ERROR.
        IF AVAIL es-doc-fisico-just THEN DO:
            RUN utp/ut-msgs.p(INPUT "show",
                              INPUT 27979,
                              INPUT "Documento j† est† Liberado para Atualizaá∆o" + "~~" +
                                    "Documento j† est† Liberado para Atualizaá∆o").
            RETURN "NOK".
        END.
        ELSE DO:

            RUN upc/upc-re2001-u01a.w.
            FIND FIRST es-doc-fisico-just NO-LOCK OF doc-fisico NO-ERROR.
            IF AVAIL es-doc-fisico-just THEN DO:
                RUN utp/ut-msgs.p(INPUT "show",
                                  INPUT 15825,
                                  INPUT "Documento Liberado para Atualizaá∆o" + "~~" +
                                        "Documento Liberado para Atualizaá∆o").
                RETURN "OK".
            END.
            ELSE DO:
                RUN utp/ut-msgs.p(INPUT "show",
                                  INPUT 17006,
                                  INPUT "Documento n∆o foi Liberado para Atualizaá∆o" + "~~" +
                                        "Documento n∆o foi Liberado para Atualizaá∆o").
                RETURN "NOK".
            END.
        /*     APPLY "choose" TO wh-re2001-bt-atualizar. */

        END.

    END.

END.

IF  p-ind-event  = "choose"
AND p-ind-object = "wh-re2001-bt-imp-status" THEN DO:

    /* In°cio - Bloco Principal */
    find first param-global NO-LOCK NO-ERROR.
    find first param-compra NO-LOCK NO-ERROR.

    EMPTY TEMP-TABLE tt-param NO-ERROR.

    CREATE tt-param.
    ASSIGN tt-param.destino   = 3
           tt-param.arquivo   = SESSION:TEMP-DIRECTORY + "RE2001.TMP"
           tt-param.usuario   = c-seg-usuario
           tt-param.data-exec = TODAY
           tt-param.hora-exec = TIME.

    {include/i-rpvar.i}

    ASSIGN c-programa     = "UPC-RE2001-U01"
           c-versao       = "2.12"
           c-revisao      = "00.000"
           c-empresa      = param-global.grupo
           c-sistema      = "Especifico"
           c-titulo-relat = "Relat¢rio de Documentos".

    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

    RUN pi-inicializar IN h-acomp (INPUT "Aguarde, Gerando...").

    {include/i-rpout.i}
    {include/i-rpcab.i}

    VIEW FRAME f-cabec.
    VIEW FRAME f-rodape.

    FOR FIRST doc-fisico NO-LOCK
        WHERE ROWID(doc-fisico) = gr-doc-fisico:

        ASSIGN l-ok = NO.

        FOR EACH it-doc-fisico NO-LOCK OF doc-fisico,
            EACH rat-lote NO-LOCK OF it-doc-fisico,
           FIRST item NO-LOCK
           WHERE item.it-codigo = rat-lote.it-codigo:

            if line-counter > 62 then
                page.
    
            if line-counter <= 5 then do:
    
                assign c-cabec1 = "Seq Item             Descriá∆o                                Dep    Localiz      Qtde Digitada Lote/SÇrie Validade Lote Informaá∆o"
                       c-cabec2 = "--- ---------------- ---------------------------------------- ------ ---------- --------------- ---------- ------------- -----------------------------------------------------".
    
                put c-cabec1 at 0 FORMAT "x(160)" SKIP
                    c-cabec2 at 0 FORMAT "x(160)" skip.
    
            end.
    
            put it-doc-fisico.sequencia   at  1   FORMAT ">>9"
                it-doc-fisico.it-codigo   at  5   FORMAT "x(16)"
                item.desc-item            at 22   FORMAT "x(40)"
                rat-lote.cod-depos        at 63   FORMAT "x(03)"
                rat-lote.cod-localiz      at 70   FORMAT "x(10)"
                rat-lote.quantidade       at 83   FORMAT ">,>>>,>>9.999"
                rat-lote.lote             at 97   FORMAT "x(10)"
                rat-lote.dt-vali-lote     at 108  FORMAT "99/99/9999".
    
            IF it-doc-fisico.quantidade = it-doc-fisico.quant-conf THEN DO:

                PUT "Ok" AT 122.

                FIND FIRST tt-erro NO-ERROR.
                IF NOT AVAIL tt-erro THEN
                    ASSIGN l-ok = YES.
                ELSE
                    ASSIGN l-ok = NO.

            END.
            ELSE DO:

                CREATE tt-erro.
                ASSIGN tt-erro.cd-erro  = 27979
                       tt-erro.mensagem = "Quantidade Divergente".

                PUT "Quantidade Divergente" AT 122.

            END.

        END.

        FIND FIRST es-doc-fisico-just NO-LOCK OF doc-fisico NO-ERROR.
        IF NOT AVAIL es-doc-fisico-just THEN DO:

            FIND FIRST tt-erro NO-ERROR.
            IF AVAIL tt-erro THEN
                PUT SKIP(1)
                    "*** Documento n∆o foi Atualizado ***" AT 01
                    "*** Solicite Aprovaá∆o para o Depto Fiscal *** " AT 01
                    SKIP(1).

        END.
        ELSE DO:

            PUT SKIP(1)
                "*** Aprovaá∆o Efetuada pelo Depto Fiscal *** " AT 01 es-doc-fisico-just.char-1 FORMAT "x(30)"
                SKIP(1).

        END.

    END.

    {include/i-rpclo.i}

    RUN pi-finalizar IN h-acomp.

    RUN utp/ut-utils.p PERSISTENT SET h-acomp.

    RUN EXECUTE IN h-acomp(INPUT "notepad.exe",
                           INPUT tt-param.arquivo).

    DELETE PROCEDURE h-acomp.

END.

IF  p-ind-event  = "AFTER-OPEN-QUERY"
AND p-ind-object = "BROWSER" 
AND p-wgh-object:FILE-NAME MATCHES "*b06in163*" THEN DO:

    IF VALID-HANDLE(wh-re2001-bt-atualizar) THEN DO:

        FOR FIRST it-doc-fisico NO-LOCK
            WHERE ROWID(it-doc-fisico) = p-row-table:
    
            FOR FIRST doc-fisico NO-LOCK OF it-doc-fisico:
    
                FIND FIRST es-doc-fisico NO-LOCK OF doc-fisico NO-ERROR.
                IF AVAIL es-doc-fisico THEN DO:

                    ASSIGN wh-re2001-status:SCREEN-VALUE = es-doc-fisico.situacao.

                    IF es-doc-fisico.situacao = "Divergente" THEN DO:
                        ASSIGN wh-re2001-bt-atualizar:SENSITIVE = NO.
                    END.

                    IF es-doc-fisico.situacao = "Liberado Nota" THEN DO:
                        ASSIGN wh-re2001-bt-libera:SENSITIVE = NO
                               wh-re2001-bt-atualizar:SENSITIVE = YES.
                    END.
                    ELSE
                        ASSIGN wh-re2001-bt-libera:SENSITIVE = YES.
                END.

            END.
    
        END.

    END.

END.

RETURN "OK".

/* Fim do Programa */

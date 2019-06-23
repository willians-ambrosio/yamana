/*****************************************************************
**
**    Programa: upc-re2002-u01
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
{include/i-prgvrs.i upc-re2002-u01 2.12.00.000}

/* Definicao de Funcoes */
{tools/fc-handle-obj.i}
{tools/fc-falso-4.i}

/* Definicao de Parametros */
DEF INPUT PARAMETER p-ind-event  AS CHAR          NO-UNDO.
DEF INPUT PARAMETER p-ind-object AS CHAR          NO-UNDO.
DEF INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEF INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAMETER p-cod-table  AS CHAR          NO-UNDO.
DEF INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

/* Definicao de Temp-Table */
def var raw-param as raw no-undo.

define temp-table tt-param
    field destino            as integer
    field arquivo            as char
    field usuario            as char
    field data-exec          as date
    field hora-exec          as integer
    field classifica         as integer
    field c-cod-estabel-ini  as char
    field c-cod-estabel-fim  as char
    field i-cod-emitente-ini as integer
    field i-cod-emitente-fim as integer
    field c-nro-docto-ini    as char
    field c-nro-docto-fim    as char
    field c-serie-docto-ini  as char
    field c-serie-docto-fim  as char
    field i-tipo-nota-ini    as int
    field i-tipo-nota-fim    as int
    field da-dt-trans-ini    as date
    field da-dt-trans-fim    as date.

define temp-table tt-digita
    field r-doc-fisico        as rowid.

define temp-table tt-raw-digita
    field raw-digita as raw.

/* Definicao de Variaveis Globais */
DEF NEW GLOBAL SHARED VAR wh-re2002-status-txt    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2002-status        AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2002-bt-contagem   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2002-bt-atualizar  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-re2002-c-emitente    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-seg-usuario           AS CHAR FORMAT "x(12)" NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-doc-fisico           AS ROWID               NO-UNDO.

/* Definicao de Variaveis Locais */
DEF VAR c-handle-obj AS CHAR   NO-UNDO.
DEF VAR h-handle     AS HANDLE NO-UNDO.
/* Definicao de Buffer */

/* MESSAGE "p-ind-event..:" p-ind-event            SKIP */
/*         "p-ind-object.:" p-ind-object           SKIP */
/*         "p-wgh-object.:" p-wgh-object:FILE-NAME SKIP */
/*         "p-wgh-frame..:" p-wgh-frame:NAME       SKIP */
/*         "p-cod-table..:" STRING(p-cod-table)    SKIP */
/*         "p-row-table..:" STRING(p-row-table)    SKIP */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.               */


IF  p-ind-event  = "INITIALIZE"
AND p-ind-object = "BROWSER"
AND p-wgh-object:FILE-NAME MATCHES "*b09in163*"  THEN DO:

     ASSIGN c-handle-obj = fc-handle-obj("bt-contagem",p-wgh-frame)
            wh-re2002-bt-contagem  = WIDGET-HANDLE(ENTRY(1,c-handle-obj)).

     create button wh-re2002-bt-atualizar
     assign frame     = p-wgh-frame
            width     = wh-re2002-bt-contagem:WIDTH
            height    = wh-re2002-bt-contagem:HEIGHT
            row       = wh-re2002-bt-contagem:ROW
            col       = wh-re2002-bt-contagem:COL + 15
            label     = "Atualizar"
            sensitive = YES
            visible   = yes
     triggers:
           ON CHOOSE PERSISTENT RUN upc/upc-re2002-u01.p(INPUT "choose",
                                                         INPUT "wh-re2002-bt-atualizar",
                                                         INPUT  p-wgh-object,
                                                         INPUT  p-wgh-frame,
                                                         INPUT  "",
                                                         INPUT  ?).
     end triggers.

END.

IF  p-ind-event  = "Display"
AND p-ind-object = "Viewer"
AND p-wgh-object:FILE-NAME MATCHES "*v09in163*" THEN DO:

    IF VALID-HANDLE(wh-re2002-status) THEN DO:

        FOR FIRST doc-fisico NO-LOCK
            WHERE ROWID(doc-fisico) = p-row-table:
    
            ASSIGN wh-re2002-status:SCREEN-VALUE = "Pendente".
            /*
            Status:
            1)	Pendente - Utilizado todas as vezes que o documento for integrado do programa de Triagem para o RE2002.
            2)	Contagem - Utilizado todas as vezes que o almoxarifado iniciar o processo de contagem dos itens (RE2002)
            3)	Divergente - Utilizado todas as vezes que houver diferen‡a entre o RE2002 - Contagem F¡sica e informa‡äes da quantidade do pedido.
            4)	Liberado Nota - Utilizado todas as vezes que o documento for liberado atrav‚s do botÆo espec¡fico que ser  criado no programa RE2002.
            */
    
        END.

    END.
    
END.

IF  p-ind-event  = "choose"
AND p-ind-object = "wh-re2002-bt-atualizar" THEN DO:

    RUN utp/ut-msgs.p(INPUT "show",
                      INPUT 727,
                      INPUT "").
    IF RETURN-VALUE = "YES" THEN DO:

        FOR FIRST doc-fisico NO-LOCK
            WHERE ROWID(doc-fisico) = gr-doc-fisico:

            create tt-param.
            assign tt-param.usuario   = c-seg-usuario
                   tt-param.destino   = 3
                   tt-param.data-exec = today
                   tt-param.hora-exec = time
                   tt-param.arquivo   = session:temp-directory + "RE2005":U + ".tmp":U.
    
            for each tt-raw-digita:
                delete tt-raw-digita.
            end.
    
            create tt-digita.
            assign tt-digita.r-doc-fisico = rowid(doc-fisico).
    
            raw-transfer tt-param  to raw-param.
    
            for each tt-digita:
                create tt-raw-digita.
                raw-transfer tt-digita to tt-raw-digita.raw-digita.
            end.
    
            if session:set-wait-state("GENERAL":U) then .
    
            run rep/re2005rp.p (input raw-param, 
                                input table tt-raw-digita).                                        
            
            RUN utp/ut-utils.p PERSISTENT SET h-handle.
        
            RUN EXECUTE IN h-handle(INPUT "notepad.exe",
                                    INPUT tt-param.arquivo).
        
            DELETE PROCEDURE h-handle.
        

        END.

    END.


END.

/* PROCEDURE pi-atualiza:                                                                                                                             */
/*                                                                                                                                                    */
/*     DO TRANSACTION:                                                                                                                                */
/*                                                                                                                                                    */
/*         RUN pi-atualiza-documento IN wh-re2002-p-wgh-object.                                                                                     */
/*                                                                                                                                                    */
/*                                                                                                                                                    */
/*         RUN pi-atualiza-recebimento.                                                                                                               */
/*                                                                                                                                                    */
/*                                                                                                                                                    */
/*         FIND FIRST doc-fisico NO-LOCK                                                                                                              */
/*              WHERE ROWID(doc-fisico) = rw-re2002-u01 NO-ERROR.                                                                                   */
/*                                                                                                                                                    */
/*                                                                                                                                                    */
/*         FIND FIRST docum-est NO-LOCK                                                                                                               */
/*              WHERE docum-est.serie-docto  = doc-fisico.serie-docto                                                                                 */
/*                AND docum-est.nro-docto    = doc-fisico.nro-docto                                                                                   */
/*                AND docum-est.cod-emitente = doc-fisico.cod-emitente NO-ERROR.                                                                      */
/*         IF NOT AVAIL docum-est THEN UNDO,LEAVE.                                                                                                    */
/*                                                                                                                                                    */
/*         IF c-nr-lote <> "" THEN DO:                                                                                                                */
/*                                                                                                                                                    */
/*             /* Grava o novo lote nas es-lote e es-lote-nota e atualiza o recebimento fiscal */                                                     */
/*             IF AVAIL doc-fisico THEN                                                                                                               */
/*                                                                                                                                                    */
/*                 FIND FIRST es_lote_nota EXCLUSIVE-LOCK                                                                                             */
/*                      WHERE es_lote_nota.serie-docto  = doc-fisico.serie-docto                                                                      */
/*                        AND es_lote_nota.nro-docto    = doc-fisico.nro-docto                                                                        */
/*                        AND es_lote_nota.cod-emitente = doc-fisico.cod-emitente                                                                     */
/*                        AND es_lote_nota.tipo-nota    = doc-fisico.tipo-nota NO-ERROR.                                                              */
/*             IF AVAIL es_lote_nota THEN DO:                                                                                                         */
/*                                                                                                                                                    */
/*                 /* Elimina todos os relacionamentos caso troque o nro do lote quando                                                               */
/*                    o documento foi desatualizado e permaneceu com o mesmo nro do lote */                                                           */
/*                 IF es_lote_nota.nr-lote <> c-nr-lote THEN DO:                                                                                      */
/*                                                                                                                                                    */
/*                     FIND FIRST es_lote EXCLUSIVE-LOCK                                                                                              */
/*                          WHERE es_lote.nr-lote = es_lote_nota.nr-lote NO-ERROR.                                                                    */
/*                     IF AVAIL es_lote THEN DO:                                                                                                      */
/*                                                                                                                                                    */
/*                         FOR EACH es_item_lote EXCLUSIVE-LOCK                                                                                       */
/*                            WHERE es_item_lote.nr-lote = es_lote.nr-lote:                                                                           */
/*                             DELETE es_item_lote.                                                                                                   */
/*                         END.                                                                                                                       */
/*                                                                                                                                                    */
/*                         FOR EACH es-apura-classificacao EXCLUSIVE-LOCK                                                                             */
/*                            WHERE es-apura-classificacao.nr-lote = es_lote.nr-lote:                                                                 */
/*                             DELETE es-apura-classificacao.                                                                                         */
/*                         END.                                                                                                                       */
/*                                                                                                                                                    */
/*                         FOR EACH es-ormec-historico EXCLUSIVE-LOCK                                                                                 */
/*                            WHERE es-ormec-historico.nr-lote = es_lote.nr-lote:                                                                     */
/*                             DELETE es-ormec-historico.                                                                                             */
/*                         END.                                                                                                                       */
/*                                                                                                                                                    */
/*                         DELETE es_lote.                                                                                                            */
/*                                                                                                                                                    */
/*                     END.                                                                                                                           */
/*                                                                                                                                                    */
/*                 END.                                                                                                                               */
/*                                                                                                                                                    */
/*                 ASSIGN es_lote_nota.nr-lote = c-nr-lote.                                                                                           */
/*                 RELEASE es_lote_nota.                                                                                                              */
/*                                                                                                                                                    */
/*             END.                                                                                                                                   */
/*                                                                                                                                                    */
/*         END.                                                                                                                                       */
/*         ELSE DO:                                                                                                                                   */
/*                                                                                                                                                    */
/*             IF AVAIL doc-fisico THEN                                                                                                               */
/*                     FIND FIRST es_lote_nota EXCLUSIVE-LOCK                                                                                         */
/*                          WHERE es_lote_nota.serie-docto  = doc-fisico.serie-docto                                                                  */
/*                            AND es_lote_nota.nro-docto    = doc-fisico.nro-docto                                                                    */
/*                            AND es_lote_nota.cod-emitente = doc-fisico.cod-emitente                                                                 */
/*                            AND es_lote_nota.tipo-nota    = doc-fisico.tipo-nota NO-ERROR.                                                          */
/*                                                                                                                                                    */
/*             IF AVAIL es_lote_nota THEN DO:                                                                                                         */
/*                                                                                                                                                    */
/*                 CREATE es_lote.                                                                                                                    */
/*                                                                                                                                                    */
/*                 ASSIGN c-nr-lote = "".                                                                                                             */
/*                 FOR LAST b_es_lote NO-LOCK                                                                                                         */
/*                    WHERE b_es_lote.nr-lote BEGINS "S".                                                                                             */
/*                     ASSIGN c-nr-lote = SUBSTRING(b_es_lote.nr-lote,2,9).                                                                           */
/*                 END.                                                                                                                               */
/*                                                                                                                                                    */
/*                 ASSIGN es_lote.nr-lote      = "S" + STRING(INT(c-nr-lote) + 1,"999999999")                                                         */
/*                        es_lote.situacao     = "PENDENTE"                                                                                           */
/*                        es_lote_nota.nr-lote = es_lote.nr-lote                                                                                      */
/*                        es_lote.nome-transp  = es_lote_nota.nome-transp                                                                             */
/*                        es_lote.placa        = es_lote_nota.placa                                                                                   */
/*                        es_lote.uf           = es_lote_nota.uf                                                                                      */
/*                        es_lote.motorista    = es_lote_nota.motorista                                                                               */
/*                        es_lote.rg           = es_lote_nota.rg                                                                                      */
/*                        es_lote.dt-chegada   = es_lote_nota.dt-chegada                                                                              */
/*                        es_lote.hr-chegada   = es_lote_nota.hr-chegada                                                                              */
/*                        es_lote.dt-entrada   = es_lote_nota.dt-entrada                                                                              */
/*                        es_lote.hr-entrada   = es_lote_nota.hr-entrada                                                                              */
/*                        es_lote.hr-ini-frete = es_lote_nota.hr-ini-frete                                                                            */
/*                        es_lote.hr-fim-frete = es_lote_nota.hr-fim-frete                                                                            */
/*                        es_lote.tipo         = es_lote_nota.tipo                                                                                    */
/*                        es_lote.nr-hr-retira = es_lote_nota.nr-hr-retira                                                                            */
/*                        es_lote.usuario      = c-seg-usuario                                                                                        */
/*                        .                                                                                                                           */
/*                                                                                                                                                    */
/*                 IF NOT AVAIL ext-proc-compra THEN                                                                                                  */
/*                     ASSIGN es_lote.tp-pedido = "".                                                                                                 */
/*                 ELSE                                                                                                                               */
/*                     ASSIGN es_lote.tp-pedido = IF ext-proc-compra.ormec THEN "1-ORMEC" ELSE "".                                                    */
/*                                                                                                                                                    */
/*                 OUTPUT TO VALUE(c-arquivo) NO-CONVERT APPEND.                                                                                      */
/*                                                                                                                                                    */
/*                 PUT SKIP(1)                                                                                                                        */
/*                     "Lote Nro: " AT 01 es_lote_nota.nr-lote                                                                                        */
/*                     SKIP.                                                                                                                          */
/*                                                                                                                                                    */
/*                 OUTPUT CLOSE.                                                                                                                      */
/*                                                                                                                                                    */
/*                 RUN utp/ut-utils.p PERSISTENT SET h-handle.                                                                                        */
/*                                                                                                                                                    */
/*                 RUN EXECUTE IN h-handle(INPUT "notepad.exe",                                                                                       */
/*                                         INPUT c-arquivo).                                                                                          */
/*                                                                                                                                                    */
/*                 DELETE PROCEDURE h-handle.                                                                                                         */
/*                                                                                                                                                    */
/*                 RELEASE es_lote.                                                                                                                   */
/*                 RELEASE es_lote_nota.                                                                                                              */
/*                                                                                                                                                    */
/*             END.                                                                                                                                   */
/*                                                                                                                                                    */
/*         END.                                                                                                                                       */
/*                                                                                                                                                    */
/*     END.                                                                                                                                           */
/*                                                                                                                                                    */
/* END PROCEDURE.                                                                                                                                     */
/*                                                                                                                                                    */
/* PROCEDURE pi-atualiza-recebimento:                                                                                                                 */
/*                                                                                                                                                    */
/*     /* Definicao de Variaveis Locais */                                                                                                            */
/*     /* DEF VAR l-achou                AS LOGICAL  NO-UNDO. */                                                                                      */
/*     /* DEF VAR v_hdl_apb925za         AS HANDLE   NO-UNDO. */                                                                                      */
/*     /* DEF VAR r_integr_apb_lote_impl AS RECID    NO-UNDO. */                                                                                      */
/*                                                                                                                                                    */
/*     /* Definicoes de Variaveis */                                                                                                                  */
/*     DEF VAR l-ok         AS LOGICAL NO-UNDO.                                                                                                       */
/*     DEF VAR de-vl-fatura AS DEC     NO-UNDO.                                                                                                       */
/*     DEF VAR i-sequencia  AS INT     NO-UNDO.                                                                                                       */
/*                                                                                                                                                    */
/*     /* Processa... */                                                                                                                              */
/*     ASSIGN c-arquivo = SESSION:TEMP-DIRECTORY + "RE1001":U + ".tmp":U.                                                                             */
/*                                                                                                                                                    */
/*     OUTPUT TO VALUE(c-arquivo) NO-CONVERT.                                                                                                         */
/*                                                                                                                                                    */
/*     ASSIGN l-ok  = YES.                                                                                                                            */
/*                                                                                                                                                    */
/*     FIND FIRST b-doc-fisico NO-LOCK                                                                                                                */
/*          WHERE ROWID(b-doc-fisico) = rw-re2002-u01 NO-ERROR.                                                                                     */
/*                                                                                                                                                    */
/*     IF AVAIL b-doc-fisico AND b-doc-fisico.situacao <> 2 THEN DO:                                                                                  */
/*                                                                                                                                                    */
/*         PUT                                                     SKIP(1)                                                                            */
/*             "Documento N’o Atualizado no RE2002 e N’o Dispon­vel no RE1001" AT 01                                                                  */
/*             "-------------------------------------------------------------" AT 01 SKIP(1)                                                          */
/*             "Ser Docto             Emitente    Tipo     " AT 01                                                                                    */
/*             "--- ----------------- ----------- ---------" AT 01                                                                                    */
/*             b-doc-fisico.serie-docto                        AT 01                                                                                  */
/*             " " b-doc-fisico.nro-docto                                                                                                             */
/*             " " b-doc-fisico.cod-emitente                                                                                                          */
/*             " " INT(b-doc-fisico.tipo)                                                                                                             */
/*             SKIP.                                                                                                                                  */
/*                                                                                                                                                    */
/*         ASSIGN l-ok  = NO.                                                                                                                         */
/*                                                                                                                                                    */
/*     END.                                                                                                                                           */
/*                                                                                                                                                    */
/*     IF l-ok AND AVAIL b-doc-fisico THEN DO:                                                                                                        */
/*                                                                                                                                                    */
/*         FIND FIRST emitente NO-LOCK                                                                                                                */
/*              WHERE emitente.cod-emitente = b-doc-fisico.cod-emitente NO-ERROR.                                                                     */
/*                                                                                                                                                    */
/*         FIND FIRST estabelec NO-LOCK                                                                                                               */
/*              WHERE estabelec.cod-estabel = b-doc-fisico.cod-estabel NO-ERROR.                                                                      */
/*                                                                                                                                                    */
/*         IF estabelec.estado = emitente.estado THEN DO:                                                                                             */
/*                                                                                                                                                    */
/*             FIND FIRST natur-oper NO-LOCK                                                                                                          */
/*                  WHERE natur-oper.nat-operacao = ext-proc-compra.nat-op-entr-estad NO-ERROR.                                                       */
/*             IF NOT AVAIL natur-oper THEN DO:                                                                                                       */
/*                 ASSIGN l-ok = NO.                                                                                                                  */
/*                                                                                                                                                    */
/*                 PUT "Natureza de Opera»’o " AT 01                                                                                                  */
/*                      ext-proc-compra.nat-op-entr-estad " n’o Cadastrada."                                                                          */
/*                     SKIP.                                                                                                                          */
/*                                                                                                                                                    */
/*             END.                                                                                                                                   */
/*         END.                                                                                                                                       */
/*         ELSE DO:                                                                                                                                   */
/*                                                                                                                                                    */
/*             FIND FIRST natur-oper NO-LOCK                                                                                                          */
/*                  WHERE natur-oper.nat-operacao = ext-proc-compra.nat-op-entr-interest NO-ERROR.                                                    */
/*             IF NOT AVAIL natur-oper THEN DO:                                                                                                       */
/*                 ASSIGN l-ok = NO.                                                                                                                  */
/*                                                                                                                                                    */
/*                 PUT "Natureza de Opera»’o " AT 01                                                                                                  */
/*                     ext-proc-compra.nat-op-entr-interest " n’o Cadastrada."                                                                        */
/*                     SKIP.                                                                                                                          */
/*             END.                                                                                                                                   */
/*                                                                                                                                                    */
/*         END.                                                                                                                                       */
/*                                                                                                                                                    */
/*     END.                                                                                                                                           */
/*                                                                                                                                                    */
/*     /*    IF natur-oper.emite-duplic THEN DO:                                                  */                                                  */
/*     /*        ASSIGN l-ok = NO.                                                                */                                                  */
/*     /*                                                                                         */                                                  */
/*     /*        MESSAGE "Natureza nao pode Emitir Duplicata"                                     */                                                  */
/*     /*            VIEW-AS ALERT-BOX INFO BUTTONS OK.                                           */                                                  */
/*     /*                                                                                         */                                                  */
/*     /*    END.                                                                                 */                                                  */
/*                                                                                                                                                    */
/*     IF AVAIL b-doc-fisico AND l-ok THEN DO:                                                                                                        */
/*                                                                                                                                                    */
/*         ASSIGN de-vl-fatura = 0.                                                                                                                   */
/*                                                                                                                                                    */
/*         /* Grava o novo lote nas es-lote e es-lote-nota e atualiza o recebimento fiscal */                                                         */
/*         IF AVAIL doc-fisico THEN FIND FIRST es_lote_nota NO-LOCK                                                                                   */
/*                                       WHERE es_lote_nota.serie-docto  = doc-fisico.serie-docto                                                     */
/*                                         AND es_lote_nota.nro-docto    = doc-fisico.nro-docto                                                       */
/*                                         AND es_lote_nota.cod-emitente = doc-fisico.cod-emitente                                                    */
/*                                         AND es_lote_nota.tipo-nota    = doc-fisico.tipo-nota NO-ERROR.                                             */
/*                                                                                                                                                    */
/*                                                                                                                                                    */
/*         EMPTY TEMP-TABLE tt-docum-est    NO-ERROR.                                                                                                 */
/*         EMPTY TEMP-TABLE tt-item-doc-est NO-ERROR.                                                                                                 */
/*                                                                                                                                                    */
/*         CREATE tt-docum-est.                                                                                                                       */
/*         BUFFER-COPY b-doc-fisico TO tt-docum-est.                                                                                                  */
/*                                                                                                                                                    */
/*         ASSIGN tt-docum-est.nat-operacao    = natur-oper.nat-operacao                                                                              */
/*                tt-docum-est.uf              = emitente.estado                                                                                      */
/*                tt-docum-est.nome-transp     = IF AVAIL es_lote_nota THEN es_lote_nota.nome-transp ELSE ""                                          */
/*                tt-docum-est.cod-uf-placa[1] = IF AVAIL es_lote_nota THEN es_lote_nota.uf ELSE ""                                                   */
/*                c-placa                      = IF AVAIL es_lote_nota THEN es_lote_nota.placa ELSE ""                                                */
/*                c-placa                      = REPLACE(c-placa,"-","")                                                                              */
/*                c-placa                      = REPLACE(c-placa,".","")                                                                              */
/*                tt-docum-est.cod-placa[1]    = c-placa                                                                                              */
/*                tt-docum-est.esp-docto       = 21                                                                                                   */
/*                tt-docum-est.rec-fisico      = YES.                                                                                                 */
/*                                                                                                                                                    */
/*         FOR EACH b-it-doc-fisico NO-LOCK OF b-doc-fisico:                                                                                          */
/*                                                                                                                                                    */
/*                                                                                                                                                    */
/*              FIND FIRST item NO-LOCK                                                                                                               */
/*                   WHERE item.it-codigo = b-it-doc-fisico.it-codigo NO-ERROR.                                                                       */
/*                                                                                                                                                    */
/*              FIND FIRST item-uni-estab NO-LOCK                                                                                                     */
/*                   WHERE item-uni-estab.it-codigo = item.it-codigo NO-ERROR.                                                                        */
/*                                                                                                                                                    */
/*              FIND FIRST natur-oper NO-LOCK                                                                                                         */
/*                   WHERE natur-oper.nat-operacao = tt-docum-est.nat-operacao NO-ERROR.                                                              */
/*                                                                                                                                                    */
/*              CREATE tt-item-doc-est.                                                                                                               */
/*              BUFFER-COPY b-it-doc-fisico TO tt-item-doc-est.                                                                                       */
/*                                                                                                                                                    */
/*              ASSIGN tt-item-doc-est.nat-operacao   = natur-oper.nat-operacao                                                                       */
/*                     tt-item-doc-est.class-fiscal   = item.class-fiscal                                                                             */
/*                     tt-item-doc-est.cd-trib-iss    = natur-oper.cd-trib-iss                                                                        */
/*                     tt-item-doc-est.aliquota-icm   = natur-oper.aliquota-icm                                                                       */
/*                     tt-item-doc-est.cd-trib-ipi    = natur-oper.cd-trib-ipi                                                                        */
/*                     tt-item-doc-est.cd-trib-icm    = natur-oper.cd-trib-icm                                                                        */
/*                     tt-item-doc-est.cd-trib-iss    = natur-oper.cd-trib-iss                                                                        */
/*                     de-vl-fatura                   = de-vl-fatura + tt-item-doc-est.preco-total[1].                                                */
/*                                                                                                                                                    */
/*              FIND FIRST rat-lote NO-LOCK OF b-it-doc-fisico NO-ERROR.                                                                              */
/*              IF AVAIL rat-lote THEN                                                                                                                */
/*                  ASSIGN tt-item-doc-est.lote         = rat-lote.lote                                                                               */
/*                         tt-item-doc-est.dt-vali-lote = rat-lote.dt-vali-lote.                                                                      */
/*                                                                                                                                                    */
/*         END.                                                                                                                                       */
/*                                                                                                                                                    */
/*         DO TRANSACTION:                                                                                                                            */
/*                                                                                                                                                    */
/*             FIND FIRST tt-docum-est NO-ERROR.                                                                                                      */
/*             ASSIGN tt-docum-est.tot-valor        = de-vl-fatura                                                                                    */
/*                    tt-docum-est.valor-mercad     = tt-docum-est.tot-valor                                                                          */
/*                    tt-docum-est.peso-bruto-tot   = tt-docum-est.tot-peso                                                                           */
/*                    tt-docum-est.peso-liquido-tot = tt-docum-est.tot-peso.                                                                          */
/*                                                                                                                                                    */
/*             /* Rotina para For»ar a criar um registro caso a base esteja VAZIA para poder replicar os dados para o Recebimento Fiscal */           */
/*             ASSIGN c-nro-docto = "".                                                                                                               */
/*             FIND FIRST docum-est NO-LOCK NO-ERROR.                                                                                                 */
/*             IF NOT AVAIL docum-est THEN DO:                                                                                                        */
/*                 CREATE docum-est.                                                                                                                  */
/*                 BUFFER-COPY tt-docum-est EXCEPT tt-docum-est.nro-docto TO docum-est.                                                               */
/*                 ASSIGN docum-est.nro-docto = "9" + SUBSTRING(tt-docum-est.nro-docto,2,6).                                                          */
/*                                                                                                                                                    */
/*                 CREATE item-doc-est.                                                                                                               */
/*                 BUFFER-COPY tt-item-doc-est EXCEPT tt-item-doc-est.nro-docto TO item-doc-est.                                                      */
/*                 ASSIGN item-doc-est.nro-docto = "9" + SUBSTRING(tt-item-doc-est.nro-docto,2,6).                                                    */
/*                                                                                                                                                    */
/*                 ASSIGN c-nro-docto = docum-est.nro-docto.                                                                                          */
/*                                                                                                                                                    */
/*             END.                                                                                                                                   */
/*                                                                                                                                                    */
/*             /* chamando a api do recebimento */                                                                                                    */
/*             RUN rep/reapi316b.p (INPUT  "ADD",  /*cTranAction*/                                                                                    */
/*                                  input  TABLE tt-docum-est,                                                                                        */
/*                                  INPUT  TABLE tt-rat-docum,                                                                                        */
/*                                  input  TABLE tt-item-doc-est,                                                                                     */
/*                                  INPUT  TABLE tt-dupli-apagar,                                                                                     */
/*                                  INPUT  TABLE tt-dupli-imp,                                                                                        */
/*                                  OUTPUT TABLE tt-erro).                                                                                            */
/*                                                                                                                                                    */
/*             /* Rotina para Eliminar o Registro que foi criado For»adamente na base VAZIA para poder replicar os dados para o Recebimento Fiscal */ */
/*             IF c-nro-docto <> "" THEN DO:                                                                                                          */
/*                 FIND FIRST docum-est EXCLUSIVE-LOCK                                                                                                */
/*                      WHERE docum-est.serie-docto  = doc-fisico.serie-docto                                                                         */
/*                        AND docum-est.nro-docto    = c-nro-docto                                                                                    */
/*                        AND docum-est.cod-emitente = doc-fisico.cod-emitente                                                                        */
/*                        AND docum-est.nat-operacao = natur-oper.nat-operacao NO-ERROR.                                                              */
/*                 IF AVAIL docum-est THEN DO:                                                                                                        */
/*                                                                                                                                                    */
/*                     FOR EACH item-doc-est EXCLUSIVE-LOCK OF docum-est:                                                                             */
/*                         DELETE item-doc-est.                                                                                                       */
/*                     END.                                                                                                                           */
/*                                                                                                                                                    */
/*                     DELETE docum-est.                                                                                                              */
/*                 END.                                                                                                                               */
/*                                                                                                                                                    */
/*             END.                                                                                                                                   */
/*                                                                                                                                                    */
/*             FIND FIRST tt-erro NO-ERROR.                                                                                                           */
/*             IF AVAIL tt-erro THEN DO:                                                                                                              */
/*                                                                                                                                                    */
/*                 PUT                                                     SKIP(1)                                                                    */
/*                     "Documento N’o Atualizado e N’o Dispon­vel no RE1001" AT 01                                                                    */
/*                     "---------------------------------------------------" AT 01 SKIP(1)                                                            */
/*                     "Ser Docto             Emitente    Tipo     " AT 01                                                                            */
/*                     "--- ----------------- ----------- ---------" AT 01                                                                            */
/*                     b-doc-fisico.serie-docto                        AT 01                                                                          */
/*                     " " b-doc-fisico.nro-docto                                                                                                     */
/*                     " " b-doc-fisico.cod-emitente                                                                                                  */
/*                     " " INT(b-doc-fisico.tipo)                                                                                                     */
/*                     SKIP.                                                                                                                          */
/*                                                                                                                                                    */
/*                 FOR EACH tt-erro:                                                                                                                  */
/*                                                                                                                                                    */
/*                     ASSIGN i-sequencia = i-sequencia + 1.                                                                                          */
/*                                                                                                                                                    */
/*                     PUT i-sequencia AT 01 " "                                                                                                      */
/*                         tt-erro.cd-erro   " "                                                                                                      */
/*                         tt-erro.desc-erro.                                                                                                         */
/*                                                                                                                                                    */
/*                 END.                                                                                                                               */
/*                                                                                                                                                    */
/*                 /* dpo - abrir o erro */                                                                                                           */
/*                 RUN utp/ut-utils.p PERSISTENT SET h-handle.                                                                                        */
/*                                                                                                                                                    */
/*                 RUN EXECUTE IN h-handle(INPUT "notepad.exe",                                                                                       */
/*                                         INPUT c-arquivo).                                                                                          */
/*                                                                                                                                                    */
/*                 DELETE PROCEDURE h-handle.                                                                                                         */
/*                                                                                                                                                    */
/*             END.                                                                                                                                   */
/*             ELSE DO:                                                                                                                               */
/*                                                                                                                                                    */
/*                 FIND CURRENT b-doc-fisico EXCLUSIVE-LOCK NO-ERROR.                                                                                 */
/*                 IF AVAIL b-doc-fisico THEN DO:                                                                                                     */
/*                     ASSIGN b-doc-fisico.situacao = 3.                                                                                              */
/*                                                                                                                                                    */
/*                     PUT                                                     SKIP(1)                                                                */
/*                         "Documento Atualizado e Dispon­vel no RE1001" AT 01                                                                        */
/*                         "-------------------------------------------" AT 01 SKIP(1)                                                                */
/*                         "Ser Docto             Emitente    Tipo     " AT 01                                                                        */
/*                         "--- ----------------- ----------- ---------" AT 01                                                                        */
/*                         b-doc-fisico.serie-docto                        AT 01                                                                      */
/*                         " " b-doc-fisico.nro-docto                                                                                                 */
/*                         " " b-doc-fisico.cod-emitente                                                                                              */
/*                         " " INT(b-doc-fisico.tipo)                                                                                                 */
/*                         SKIP.                                                                                                                      */
/*                                                                                                                                                    */
/*                     RELEASE b-doc-fisico.                                                                                                          */
/*                                                                                                                                                    */
/*                 END.                                                                                                                               */
/*                                                                                                                                                    */
/*                 /* Atualiza Recebimento */                                                                                                         */
/*                 CREATE tt-param.                                                                                                                   */
/*                 ASSIGN tt-param.usuario   = c-seg-usuario                                                                                          */
/*                        tt-param.destino   = 3                                                                                                      */
/*                        tt-param.data-exec = TODAY                                                                                                  */
/*                        tt-param.hora-exec = TIME                                                                                                   */
/*                        tt-param.arquivo   = SESSION:TEMP-DIRECTORY + "RE1005":U + ".tmp".                                                          */
/*                 RAW-TRANSFER tt-param TO raw-param.                                                                                                */
/*                                                                                                                                                    */
/*                 FOR EACH tt-docum-est,                                                                                                             */
/*                    FIRST docum-est NO-LOCK OF tt-docum-est:                                                                                        */
/*                                                                                                                                                    */
/*                     /* Limpa Temp Table's Utilizadas Pelo RE1005 */                                                                                */
/*                     EMPTY TEMP-TABLE tt-digita     NO-ERROR.                                                                                       */
/*                     EMPTY TEMP-TABLE tt-raw-digita NO-ERROR.                                                                                       */
/*                                                                                                                                                    */
/*                     /* Cria Dados para Atualizacao */                                                                                              */
/*                     CREATE tt-digita.                                                                                                              */
/*                     ASSIGN tt-digita.r-docum-est = ROWID(docum-est).                                                                               */
/*                                                                                                                                                    */
/*                     CREATE tt-raw-digita.                                                                                                          */
/*                     RAW-TRANSFER tt-digita TO tt-raw-digita.raw-digita.                                                                            */
/*                                                                                                                                                    */
/*                     /* Envia Dados */                                                                                                              */
/*                     RUN rep/re1005rp.p (INPUT raw-param,                                                                                           */
/*                                         INPUT TABLE tt-raw-digita).                                                                                */
/*                 END.                                                                                                                               */
/*                                                                                                                                                    */
/*                 /* Verifica se todos os documento foram atualizados */                                                                             */
/*                 ASSIGN l-achou     = NO                                                                                                            */
/*                        i-sequencia = 0.                                                                                                            */
/*                 FOR EACH tt-docum-est,                                                                                                             */
/*                    FIRST docum-est NO-LOCK OF tt-docum-est:                                                                                        */
/*                                                                                                                                                    */
/*                      IF docum-est.ce-atual = NO THEN DO:                                                                                           */
/*                          ASSIGN i-sequencia = i-sequencia + 1.                                                                                     */
/*                                                                                                                                                    */
/*                          PUT i-sequencia AT 01                                                                                                     */
/*                              " Docto: " docum-est.nro-docto " nao Atualizado no RE1001."                                                           */
/*                              .                                                                                                                     */
/*                          ASSIGN l-achou = YES.                                                                                                     */
/*                                                                                                                                                    */
/*                      END.                                                                                                                          */
/*                                                                                                                                                    */
/*                 END.                                                                                                                               */
/*                                                                                                                                                    */
/*                 IF l-achou THEN DO:                                                                                                                */
/*                     ASSIGN l-ok  = NO.                                                                                                             */
/*                     RUN utp/ut-utils.p PERSISTENT SET h-handle.                                                                                    */
/*                                                                                                                                                    */
/*                     RUN EXECUTE IN h-handle(INPUT "notepad.exe",                                                                                   */
/*                                             INPUT tt-param.arquivo).                                                                               */
/*                     DELETE PROCEDURE h-handle.                                                                                                     */
/*                 END.                                                                                                                               */
/*                 ELSE                                                                                                                               */
/*                     PUT i-sequencia AT 01                                                                                                          */
/*                         " Docto: " docum-est.nro-docto " Atualizado no RE1001."                                                                    */
/*                         .                                                                                                                          */
/*                                                                                                                                                    */
/*             END.                                                                                                                                   */
/*                                                                                                                                                    */
/*         END.                                                                                                                                       */
/*                                                                                                                                                    */
/*     END.                                                                                                                                           */
/*                                                                                                                                                    */
/*     IF l-ok = NO THEN DO:                                                                                                                          */
/*                                                                                                                                                    */
/*         PUT                                                     SKIP(1)                                                                            */
/*             "Documento N’o Atualizado no RE2002 e N’o Dispon­vel no RE1001" AT 01                                                                  */
/*             "-------------------------------------------------------------" AT 01 SKIP(1)                                                          */
/*             "Ser Docto             Emitente    Tipo     " AT 01                                                                                    */
/*             "--- ----------------- ----------- ---------" AT 01                                                                                    */
/*             b-doc-fisico.serie-docto                        AT 01                                                                                  */
/*             " " b-doc-fisico.nro-docto                                                                                                             */
/*             " " b-doc-fisico.cod-emitente                                                                                                          */
/*             " " INT(b-doc-fisico.tipo)                                                                                                             */
/*             SKIP.                                                                                                                                  */
/*                                                                                                                                                    */
/*     END.                                                                                                                                           */
/*                                                                                                                                                    */
/*     OUTPUT CLOSE.                                                                                                                                  */
/*                                                                                                                                                    */
/*     IF l-ok = NO THEN DO:                                                                                                                          */
/*                                                                                                                                                    */
/*         RUN utp/ut-utils.p PERSISTENT SET h-handle.                                                                                                */
/*                                                                                                                                                    */
/*         RUN EXECUTE IN h-handle(INPUT "notepad.exe",                                                                                               */
/*                                 INPUT c-arquivo).                                                                                                  */
/*         DELETE PROCEDURE h-handle.                                                                                                                 */
/*                                                                                                                                                    */
/*     END.                                                                                                                                           */
/*                                                                                                                                                    */
/* END PROCEDURE.                                                                                                                                     */

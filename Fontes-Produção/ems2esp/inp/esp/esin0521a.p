DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino          AS INTEGER
    FIELD arquivo          AS CHAR FORMAT "x(35)":U
    FIELD usuario          AS CHAR FORMAT "x(12)":U
    FIELD data-exec        AS DATE
    FIELD hora-exec        AS INTEGER
    FIELD classifica       AS INTEGER
    FIELD desc-classifica  AS CHAR FORMAT "x(40)":U
    FIELD modelo           AS CHAR FORMAT "x(35)":U
    FIELD cod-ep-ini       AS CHAR
    FIELD cod-ep-fim       AS CHAR
    FIELD cod-estab-ini    LIKE estabelec.cod-estabel
    FIELD cod-estab-fim    LIKE estabelec.cod-estabel
    FIELD num-proj-ini     LIKE proj-inv.num-projeto
    FIELD num-proj-fim     LIKE proj-inv.num-projeto
    FIELD num-ord-inv-ini  LIKE mat-rat-contr-inv.num-ord-inv
    FIELD num-ord-inv-fim  LIKE mat-rat-contr-inv.num-ord-inv
    FIELD i-moeda          AS   INTEGER
    FIELD arquivo-excel    AS   CHAR FORMAT "x(60)":U
    /*Alterado 15/02/2005 - tech1007 - Criado campo l¢gico para verificar se o RTF foi habilitado*/
    FIELD l-habilitaRtf    AS LOG.
    /*Fim alteracao 15/02/2005*/
  /*tt-param*/

DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-param.

{utp/ut-glob.i}

DEFINE TEMP-TABLE tt-relat
    FIELD estab             LIKE pedido-compr.end-cobranca
    FIELD nome-estab        LIKE estabelec.nome
    FIELD num-projeto       LIKE proj-inv.num-projeto
    FIELD desc-proj         LIKE proj-inv.descricao
    FIELD num-ord-inv       LIKE mat-rat-contr-inv.num-ord-inv
    FIELD desc-oi           LIKE ordem-inv.descricao
    FIELD d-valor-total     AS DECIMAL FORMAT ">>>>>,>>>,>>9.9999"
    FIELD d-valor-compr     AS DECIMAL FORMAT ">>>>>,>>>,>>9.9999"
    FIELD d-valor-saldo     AS DECIMAL FORMAT ">>>>>,>>>,>>9.9999"
    FIELD nr-trans          AS INTEGER
    INDEX id nr-trans.

DEFINE TEMP-TABLE tt-projetos-relat
    FIELD dt-atualiza       LIKE recebimento.data-atualiz
    FIELD descricao         LIKE ITEM.desc-item
    FIELD nr-contrato       LIKE contrato-for.nr-contrato
    FIELD num-pedido        LIKE pedido-compr.num-pedido
    FIELD documento         AS CHARACTER
    FIELD natureza          AS CHARACTER FORMAT "X(16)"
    FIELD cod-emitente      LIKE contrato-for.cod-emitente
    FIELD nome-emit         LIKE emitente.nome-emit
    FIELD sc-codigo         LIKE mat-rat-contr-inv.sc-codigo
    FIELD ct-codigo         LIKE mat-rat-contr-inv.ct-codigo
    FIELD num-seq-item      LIKE medicao-contrat.num-seq-item
    FIELD usuario           LIKE recebimento.usuario
    FIELD d-valor-item-1    AS DECIMAL
    FIELD nr-trans          AS INTEGER
    FIELD tipo              AS CHARACTER FORMAT "x(20)".

DEFINE VARIABLE nome-emit       LIKE emitente.nome-emit          NO-UNDO.
DEFINE VARIABLE i-moeda-par     AS INTEGER                       NO-UNDO.
DEFINE VARIABLE c-nome-estab    LIKE estabelec.nome              NO-UNDO.
DEFINE VARIABLE c-desc-proj     LIKE proj-inv.descricao          NO-UNDO.
DEFINE VARIABLE c-desc-oi       LIKE ordem-inv.descricao         NO-UNDO.
DEFINE VARIABLE de-calc-item-1  AS DECIMAL                       NO-UNDO.
DEFINE VARIABLE d-valor-item-1  AS DECIMAL                       NO-UNDO.
DEFINE VARIABLE de-total-item-1 AS DECIMAL                       NO-UNDO.
DEFINE VARIABLE d-valor-total   AS DECIMAL                       NO-UNDO. 
DEFINE VARIABLE d-valor-compr   AS DECIMAL                       NO-UNDO. 
DEFINE VARIABLE d-valor-saldo   AS DECIMAL                       NO-UNDO. 
DEFINE VARIABLE h-acomp         AS HANDLE                        NO-UNDO.
DEFINE VARIABLE i-nr-trans      AS INTEGER                       NO-UNDO.
DEFINE VARIABLE l-gerou-excel   AS LOGICAL                       NO-UNDO.
DEFINE VARIABLE c-data          AS CHARACTER                     NO-UNDO.
DEFINE VARIABLE c-desc-item     LIKE item.desc-item              NO-UNDO.
DEFINE VARIABLE c-conta-contabil LIKE movto-estoq.conta-contabil NO-UNDO.
DEFINE VARIABLE c-ct-codigo     LIKE movto-estoq.ct-codigo       NO-UNDO.
DEFINE VARIABLE c-sc-codigo     LIKE movto-estoq.ct-codigo       NO-UNDO.

DEF NEW GLOBAL SHARED VAR i-moeda-g AS INTEGER FORMAT ">9"                                                     NO-UNDO.
DEF NEW GLOBAL SHARED VAR da-conv-g AS DATE FORMAT "99/99/9999" INITIAL TODAY                                  NO-UNDO.
DEF NEW GLOBAL SHARED VAR l-conv-g  AS LOG VIEW-AS RADIO-SET HORIZONTAL RADIO-BUTTONS "Cotacao",YES,"Atual",NO NO-UNDO.

/* VARIµVEIS EXCEL */
DEFINE VARIABLE ch-Excel        AS COMPONENT-HANDLE     NO-UNDO.
DEFINE VARIABLE ch-WorkSheet    AS COMPONENT-HANDLE     NO-UNDO.
DEFINE VARIABLE ch-arquivo      AS COMPONENT-HANDLE     NO-UNDO.
DEFINE VARIABLE ch-planilha     AS COMPONENT-HANDLE     NO-UNDO.
DEFINE VARIABLE i-cont          AS INTEGER              NO-UNDO.
DEFINE VARIABLE c-arquivo       AS CHARACTER            NO-UNDO.

FIND FIRST tt-param EXCLUSIVE-LOCK NO-ERROR.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i "Inicializando" "*"}
 
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

ASSIGN i-moeda-par = 1.

ASSIGN i-nr-trans    = 0
       l-gerou-excel = YES.

FOR EACH sub-div-ordem
   WHERE sub-div-ordem.ep-codigo       >= tt-param.cod-ep-ini
     AND sub-div-ordem.ep-codigo       <= tt-param.cod-ep-fim
     AND sub-div-ordem.num-ord-magnus  >= tt-param.num-ord-inv-ini
     AND sub-div-ordem.num-ord-magnus  <= tt-param.num-ord-inv-fim
     AND sub-div-ordem.num-projeto     >= tt-param.num-proj-ini
     AND sub-div-ordem.num-projeto     <= tt-param.num-proj-fim 
     AND sub-div-ordem.cod-est-exec    >= tt-param.cod-estab-ini
     AND sub-div-ordem.cod-est-exec    <= tt-param.cod-estab-fim  NO-LOCK:
    
    FOR FIRST proj-inv 
        WHERE proj-inv.ep-codigo    = sub-div-ordem.ep-codigo
          AND proj-inv.cod-est-exec = sub-div-ordem.cod-est-exec
          AND proj-inv.num-projeto  = sub-div-ordem.num-projeto NO-LOCK: END.
    IF AVAIL proj-inv THEN DO:
        ASSIGN c-desc-proj = proj-inv.descricao.

        FOR FIRST controle-verba NO-LOCK
            WHERE controle-verba.ep-codigo    = sub-div-ordem.ep-codigo
              AND controle-verba.cod-est-exec = sub-div-ordem.cod-est-exec
              AND controle-verba.num-projeto  = sub-div-ordem.num-projeto
              AND controle-verba.num-ordem    = sub-div-ordem.num-ordem:
            ASSIGN d-valor-total = controle-verba.vl-verba[i-moeda-par]
                   d-valor-compr = controle-verba.vl-comp[i-moeda-par] + controle-verba.vl-real[i-moeda-par]
                   d-valor-saldo = d-valor-total - (controle-verba.vl-comp[i-moeda-par] + controle-verba.vl-real[i-moeda-par]).
        END.

        FOR FIRST estabelec
            WHERE estabelec.cod-estabel = sub-div-ordem.cod-est-exec NO-LOCK: END.
        IF AVAIL estabelec THEN
            ASSIGN c-nome-estab = estabelec.nome.

        FOR FIRST ordem-inv
            WHERE ordem-inv.ep-codigo    = sub-div-ordem.ep-codigo
              AND ordem-inv.cod-est-exec = sub-div-ordem.cod-est-exec
              AND ordem-inv.num-projeto  = sub-div-ordem.num-projeto 
              AND ordem-inv.num-ordem    = sub-div-ordem.num-ordem NO-LOCK: END.
        IF AVAIL ordem-inv THEN
            ASSIGN c-desc-oi = ordem-inv.descricao.

        RUN pi-acompanhar IN h-acomp (INPUT "Criando Registros").
            
        IF NOT CAN-FIND(FIRST tt-relat
                        WHERE tt-relat.estab           = sub-div-ordem.cod-est-exec
                          AND tt-relat.num-projeto     = proj-inv.num-projeto
                          AND tt-relat.num-ord-inv     = sub-div-ordem.num-ord-magnus) THEN DO:
            ASSIGN i-nr-trans = i-nr-trans + 1.

            CREATE tt-relat.
            ASSIGN tt-relat.estab           = sub-div-ordem.cod-est-exec
                   tt-relat.nome-estab      = c-nome-estab
                   tt-relat.num-projeto     = proj-inv.num-projeto
                   tt-relat.desc-proj       = c-desc-proj
                   tt-relat.num-ord-inv     = sub-div-ordem.num-ord-magnus
                   tt-relat.desc-oi         = c-desc-oi
                   tt-relat.d-valor-total   = d-valor-total
                   tt-relat.d-valor-compr   = d-valor-compr
                   tt-relat.d-valor-saldo   = d-valor-saldo
                   tt-relat.nr-trans        = i-nr-trans.
        END.

        FOR EACH movto-nf
           WHERE movto-nf.ep-codigo       = sub-div-ordem.ep-codigo
             AND movto-nf.cod-est-exec    = sub-div-ordem.cod-est-exec
             AND movto-nf.num-projeto     = sub-div-ordem.num-projeto
             AND movto-nf.num-ordem       = sub-div-ordem.num-ordem 
             AND movto-nf.num-ord-magnus  = sub-div-ordem.num-ord-magnus NO-LOCK:

            FOR FIRST emitente
                WHERE emitente.cod-emitente = movto-nf.cod-emitente NO-LOCK. END.
            ASSIGN nome-emit = IF AVAIL emitente THEN emitente.nome-abrev ELSE "".

            FIND FIRST item 
                 WHERE item.it-codigo = movto-nf.it-codigo NO-LOCK NO-ERROR.
            ASSIGN c-desc-item = IF AVAIL item THEN item.desc-item ELSE "".

            FIND FIRST pedido-compr
                 WHERE pedido-compr.num-pedido = movto-nf.num-pedido NO-LOCK NO-ERROR.

            ASSIGN c-ct-codigo = ""
                   c-sc-codigo = "".

            IF AVAIL pedido-compr THEN DO:

                IF pedido-compr.nr-contrato <> 0 THEN 
                    FOR FIRST recebimento
                        WHERE recebimento.serie-docto  = movto-nf.serie-docto
                          AND recebimento.numero-nota  = movto-nf.nro-docto
                          AND recebimento.cod-emitente = movto-nf.cod-emitente
                          AND recebimento.nat-operacao = movto-nf.nat-operacao
                          AND recebimento.int-1        = movto-nf.num-seq-item-nf /*INT(movto-nf.dec-2)*/ NO-LOCK:
                        ASSIGN c-ct-codigo = recebimento.ct-codigo
                               c-sc-codigo = recebimento.sc-codigo.

                        /*FIND FIRST medicao-contrat
                             WHERE medicao-contrat.nr-contrato     = INT(SUBSTRING(recebimento.char-2,1,9))
                               AND medicao-contrat.num-seq-item    = INT(SUBSTRING(recebimento.char-2,10,4))
                               AND medicao-contrat.num-seq-event   = INT(SUBSTRING(recebimento.char-2,14,4))
                               AND medicao-contrat.num-seq-medicao = INT(SUBSTRING(recebimento.char-2,18,4)) NO-LOCK NO-ERROR.*/
                    END.

                ELSE
                    FOR FIRST item-doc-est
                        WHERE item-doc-est.serie-docto  = movto-nf.serie-docto
                          AND item-doc-est.nro-docto    = movto-nf.nro-docto
                          AND item-doc-est.cod-emitente = movto-nf.cod-emitente
                          AND item-doc-est.nat-operacao = movto-nf.nat-operacao
                          AND item-doc-est.sequencia    = movto-nf.sequencia   NO-LOCK.  
                        ASSIGN c-ct-codigo = item-doc-est.ct-codigo
                               c-sc-codigo = item-doc-est.sc-codigo.
                    END.
            END.

            FIND FIRST medicao-contrat                       
                 WHERE medicao-contrat.nr-contrato     = INT(substring(movto-nf.char-1,1,9))
                   AND medicao-contrat.num-seq-item    = INT(substring(movto-nf.char-1,10,4))
                   AND medicao-contrat.numero-ordem    = movto-nf.num-ord-comp
                   AND medicao-contrat.num-seq-event   = INT(substring(movto-nf.char-1,14,4))
                   AND medicao-contrat.num-seq-medicao = INT(substring(movto-nf.char-1,18,4)) NO-LOCK NO-ERROR.
            
            ASSIGN c-data = STRING(DAY(movto-nf.dt-trans), "99") + "/" + STRING(MONTH(movto-nf.dt-trans), "99") + "/" + STRING(YEAR(movto-nf.dt-trans), "9999").

            CREATE tt-projetos-relat.
            ASSIGN tt-projetos-relat.nr-trans          = i-nr-trans
                   tt-projetos-relat.dt-atualiza       = DATE(c-data)  /*movto-nf.dt-trans*/
                   tt-projetos-relat.descricao         = IF AVAIL medicao-contrat THEN medicao-contrat.observacao ELSE(IF AVAIL item THEN ITEM.desc-item ELSE "")
                   /*tt-projetos-relat.nr-contrato       = movto-nf.nro-docto + "/" + movto-nf.nat-operacao*/
                   tt-projetos-relat.documento         = movto-nf.nro-docto + "/" + movto-nf.serie-docto /*+ "/" + STRING(movto-nf.cod-emitente)*/
                   tt-projetos-relat.nr-contrato       = IF AVAIL pedido-compr THEN pedido-compr.nr-contrato ELSE 0
                   tt-projetos-relat.num-pedido        = movto-nf.num-pedido
                   tt-projetos-relat.natureza          = IF AVAIL pedido-compr THEN {ininc/i01in295.i 4 pedido-compr.natureza} ELSE " "
                   tt-projetos-relat.cod-emitente      = movto-nf.cod-emitente
                   tt-projetos-relat.nome-emit         = nome-emit
                   tt-projetos-relat.sc-codigo         = c-sc-codigo
                   tt-projetos-relat.ct-codigo         = c-ct-codigo
                   tt-projetos-relat.num-seq-item      = IF AVAIL medicao-contrat THEN medicao-contrat.num-seq-item ELSE 0
                   tt-projetos-relat.d-valor-item-1    = movto-nf.vl-nota[1]
                   tt-projetos-relat.usuario           = movto-nf.usuario
                   tt-projetos-relat.tipo              = "Nota Fiscal".
                   /*tt-projetos-relat.tipo              = "Contrato/Pedido".*/

            IF AVAIL medicao-contrat THEN
                RELEASE medicao-contrat.
        END.

        FOR EACH movto-apr
           WHERE movto-apr.ep-codigo       = sub-div-ordem.ep-codigo
             AND movto-apr.cod-est-exec    = sub-div-ordem.cod-est-exec
             AND movto-apr.num-projeto     = sub-div-ordem.num-projeto
             AND movto-apr.num-ordem       = sub-div-ordem.num-ordem 
             AND movto-apr.num-ord-magnus  = sub-div-ordem.num-ord-magnus NO-LOCK:

            ASSIGN c-ct-codigo = ""
                   c-sc-codigo = "".
            
            IF movto-apr.transacao = "IMD"
            OR movto-apr.transacao = "PEF" THEN DO:

                FIND FIRST tit-ap
                     WHERE tit-ap.ep-codigo   = movto-apr.ep-codigo
                       AND tit-ap.cod-estabel = movto-apr.cod-estabel
                       AND tit-ap.cod-esp     = movto-apr.esp-docto
                       AND tit-ap.cod-fornec  = movto-apr.cod-emitente
                       AND tit-ap.serie       = movto-apr.serie-docto
                       AND tit-ap.nr-docto    = movto-apr.nro-docto
                       AND tit-ap.parcela     = movto-apr.parcela  NO-LOCK NO-ERROR.
                IF AVAIL tit-ap THEN
                    ASSIGN c-ct-codigo = tit-ap.ct-conta-db
                           c-sc-codigo = tit-ap.sc-conta-db.
            END.
            ELSE DO:
                FIND FIRST movto-estoq 
                     WHERE movto-estoq.cod-estabel  = movto-apr.cod-estabel  
                       AND movto-estoq.cod-emitente = movto-apr.cod-emitente 
                       AND movto-estoq.serie-docto  = movto-apr.serie-docto  
                       AND movto-estoq.nro-docto    = movto-apr.nro-docto    
                       AND movto-estoq.it-codigo    = movto-apr.it-codigo NO-LOCK NO-ERROR.
                IF AVAIL movto-estoq THEN
                    ASSIGN c-ct-codigo = movto-estoq.ct-codigo
                           c-sc-codigo = movto-estoq.sc-codigo.
            END.

            FOR FIRST emitente
                WHERE emitente.cod-emitente = movto-apr.cod-emitente NO-LOCK. END.
            IF AVAIL emitente THEN
                ASSIGN nome-emit = emitente.nome-abrev.

            FIND FIRST item 
                 WHERE item.it-codigo = movto-apr.it-codigo NO-LOCK NO-ERROR.
            ASSIGN c-desc-item = IF AVAIL item THEN item.desc-item ELSE "".

            FIND FIRST pedido-compr
                 WHERE pedido-compr.num-pedido = movto-apr.num-pedido NO-LOCK NO-ERROR.

            ASSIGN d-valor-item-1 = movto-apr.vl-mat[1] + movto-apr.vl-mob[1].

            IF movto-apr.tipo-trans = 2 THEN
                ASSIGN d-valor-item-1 = d-valor-item-1 * -1.

            IF AVAIL pedido-compr THEN DO:
                IF CAN-FIND(FIRST contrato-for
                            WHERE contrato-for.nr-contrato = pedido-compr.nr-contrato) THEN DO:
                    FIND FIRST item-contrat
                         WHERE item-contrat.nr-contrato = pedido-compr.nr-contrato
                           AND item-contrat.it-codigo   = movto-apr.it-codigo NO-LOCK NO-ERROR.
                    IF AVAIL item-contrat THEN
                        FIND FIRST medicao-contrat
                             WHERE medicao-contrat.nr-contrato  = item-contrat.nr-contrato
                               AND medicao-contrat.num-seq-item = item-contrat.num-seq-item NO-LOCK NO-ERROR.
                END.
            END.
            ELSE DO:
                RELEASE item-contrat.
                RELEASE medicao-contrat.
            END.

            FIND FIRST mov-ap
                 WHERE mov-ap.ep-codigo    = movto-apr.ep-codigo
                   AND mov-ap.cod-fornec   = movto-apr.cod-emitente
                   AND mov-ap.cod-estabel  = movto-apr.cod-estabel
                   AND mov-ap.cod-esp      = movto-apr.esp-docto
                   AND mov-ap.serie        = movto-apr.serie-docto
                   AND mov-ap.nr-docto     = movto-apr.nro-docto
                   AND mov-ap.parcela      = movto-apr.parcela
                   AND mov-ap.dt-transacao = movto-apr.dt-trans NO-LOCK NO-ERROR.
            IF AVAIL mov-ap THEN
                ASSIGN c-data = STRING(DAY(mov-ap.dt-today), "99") + "/" + STRING(MONTH(mov-ap.dt-today), "99") + "/" + STRING(YEAR(mov-ap.dt-today), "9999").
            ELSE
                ASSIGN c-data = STRING(DAY(movto-apr.dt-trans), "99") + "/" + STRING(MONTH(movto-apr.dt-trans), "99") + "/" + STRING(YEAR(movto-apr.dt-trans), "9999").

            CREATE tt-projetos-relat.
            ASSIGN tt-projetos-relat.nr-trans          = i-nr-trans
                   tt-projetos-relat.dt-atualiza       = DATE(c-data) /*movto-apr.dt-trans*/
                   tt-projetos-relat.descricao         = IF AVAIL medicao-contrat THEN medicao-contrat.observacao ELSE(IF AVAIL item THEN ITEM.desc-item ELSE "")
                   tt-projetos-relat.nr-contrato       = IF AVAIL pedido-compr THEN pedido-compr.nr-contrato ELSE 0
                   tt-projetos-relat.num-pedido        = IF AVAIL pedido-compr THEN pedido-compr.num-pedido  ELSE 0
                   tt-projetos-relat.documento         = STRING(movto-apr.esp-docto) + "/" + STRING(movto-apr.nro-docto)
                   tt-projetos-relat.natureza          = IF AVAIL pedido-compr THEN {ininc/i01in295.i 4 pedido-compr.natureza} ELSE " "
                   tt-projetos-relat.cod-emitente      = movto-apr.cod-emitente
                   tt-projetos-relat.nome-emit         = nome-emit
                   tt-projetos-relat.sc-codigo         = c-sc-codigo
                   tt-projetos-relat.ct-codigo         = c-ct-codigo
                   tt-projetos-relat.num-seq-item      = IF AVAIL item-contrat THEN item-contrat.num-seq-item ELSE 0
                   tt-projetos-relat.d-valor-item-1    = d-valor-item-1
                   tt-projetos-relat.usuario           = movto-apr.usuario
                   tt-projetos-relat.tipo              = IF AVAIL pedido-compr THEN "Contrato/Pedido" ELSE "Esp‚cie/Documento".
        END.
    END.
END.

IF CAN-FIND(FIRST tt-projetos-relat) THEN DO:
    RUN pi-inicializa-excel.
    RUN pi-exporta-excel.
    EMPTY TEMP-TABLE tt-relat NO-ERROR.
    EMPTY TEMP-TABLE tt-projetos-relat NO-ERROR.
    RUN pi-finaliza-excel.
END.

RUN pi-finalizar IN h-acomp.

PROCEDURE pi-exporta-excel:
    FOR EACH tt-relat
        BREAK BY tt-relat.nr-trans:
        IF NOT CAN-FIND (FIRST tt-projetos-relat
                         WHERE tt-projetos-relat.nr-trans = tt-relat.nr-trans) THEN NEXT.

        ASSIGN l-gerou-excel = YES.

        RUN pi-acompanhar IN h-acomp (INPUT "Exportando Projeto - " + tt-relat.desc-proj).

        ch-Excel:Range("A" + STRING(i-cont) + ":M" + STRING(i-cont)):Merge.
        ch-Excel:Range("A" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("A" + STRING(i-cont)):VALUE = "REALIZADO - " + STRING(tt-relat.desc-proj).
        ch-Excel:Range("A" + STRING(i-cont) + ":M" + STRING(i-cont)):FONT:bold             = TRUE.
        
        RUN pi-bordas (INPUT "").
        
        ASSIGN i-cont = i-cont + 1.
        RUN pi-bordas (INPUT "Cabecalho").

        ch-Excel:Range("A" + STRING(i-cont) + ":E" + STRING(i-cont)):Merge.
        ch-Excel:Range("A" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("A" + STRING(i-cont)):VALUE = "Estabelecimento: " + STRING(tt-relat.estab) + " - " + STRING(tt-relat.nome-estab).
        ch-Excel:Range("F" + STRING(i-cont) + ":I" + STRING(i-cont)):Merge.
        ch-Excel:Range("F" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("F" + STRING(i-cont)):VALUE = "Projeto: " + STRING(tt-relat.num-projeto) + " - " + STRING(tt-relat.desc-proj).
        ch-Excel:Range("J" + STRING(i-cont) + ":M" + STRING(i-cont)):Merge.
        ch-Excel:Range("J" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("J" + STRING(i-cont)):VALUE = "OI: " + STRING(tt-relat.num-ord-inv) + " - " + STRING(tt-relat.desc-oi).
        ch-Excel:Range("A" + STRING(i-cont) + ":M" + STRING(i-cont)):FONT:bold = TRUE.

        ASSIGN i-cont = i-cont + 1.
        RUN pi-bordas (INPUT "Cabecalho").
        ch-Excel:Range("A" + STRING(i-cont) + ":E" + STRING(i-cont)):Merge.
        ch-Excel:Range("A" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("A" + STRING(i-cont)):VALUE = "Valor Total por Projeto:".
        ch-Excel:Range("A" + STRING(i-cont) + ":E" + STRING(i-cont)):FONT:bold = TRUE.
        ch-Excel:Range("F" + STRING(i-cont) + ":I" + STRING(i-cont)):Merge.
        ch-Excel:Range("F" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("F" + STRING(i-cont)):VALUE = "Valor Compromissado por Projeto:".
        ch-Excel:Range("F" + STRING(i-cont) + ":I" + STRING(i-cont)):FONT:bold = TRUE.
        ch-Excel:Range("J" + STRING(i-cont) + ":M" + STRING(i-cont)):Merge.
        ch-Excel:Range("J" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("J" + STRING(i-cont)):VALUE = "Saldo por Projeto:".
        ch-Excel:Range("J" + STRING(i-cont) + ":M" + STRING(i-cont)):FONT:bold = TRUE.
        
        ASSIGN i-cont = i-cont + 1.
        RUN pi-bordas (INPUT "Cabecalho").
        ch-Excel:Range("A" + STRING(i-cont) + ":E" + STRING(i-cont)):Merge.
        ch-Excel:Range("A" + STRING(i-cont)):VALUE = tt-relat.d-valor-total.
        ch-Excel:Range("A" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("A" + STRING(i-cont)):numberformat = "R$ #.##0,00". 
        ch-Excel:Range("F" + STRING(i-cont) + ":I" + STRING(i-cont)):Merge.
        ch-Excel:Range("F" + STRING(i-cont)):VALUE = tt-relat.d-valor-compr.
        ch-Excel:Range("F" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("F" + STRING(i-cont)):numberformat = "R$ #.##0,00". 
        ch-Excel:Range("J" + STRING(i-cont) + ":M" + STRING(i-cont)):Merge.
        ch-Excel:Range("J" + STRING(i-cont)):VALUE = tt-relat.d-valor-saldo.
        ch-Excel:Range("J" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("J" + STRING(i-cont)):numberformat = "R$ #.##0,00". 

        ASSIGN i-cont = i-cont + 1.
        RUN pi-bordas (INPUT "Relatorio").
        ch-Excel:Range("A" + STRING(i-cont) + ":M" + STRING(i-cont)):FONT:bold = TRUE.
        ch-Excel:Range("A" + STRING(i-cont) + ":M" + STRING(i-cont)):Interior:ColorIndex = 15.
        ch-Excel:Range("A" + STRING(i-cont)):VALUE = "Data Atualiza‡Æo".
        ch-Excel:Range("A" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("B" + STRING(i-cont)):VALUE = "Contrato".
        ch-Excel:Range("B" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("C" + STRING(i-cont)):VALUE = "Pedido".
        ch-Excel:Range("C" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("D" + STRING(i-cont)):VALUE = "Tipo".
        ch-Excel:Range("D" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("E" + STRING(i-cont)):VALUE = "Docto/Serie".
        ch-Excel:Range("E" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("F" + STRING(i-cont)):VALUE = "Nome Fornecedor".
        ch-Excel:Range("F" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("G" + STRING(i-cont)):VALUE = "Seq Item".
        ch-Excel:Range("G" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("H" + STRING(i-cont)):VALUE = "Natureza".
        ch-Excel:Range("H" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("I" + STRING(i-cont)):VALUE = "Descri‡Æo".
        ch-Excel:Range("I" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("J" + STRING(i-cont)):VALUE = "C Custo".
        ch-Excel:Range("J" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("K" + STRING(i-cont)):VALUE = "C Cont bil".
        ch-Excel:Range("K" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("L" + STRING(i-cont)):VALUE = "Valor".
        ch-Excel:Range("L" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("M" + STRING(i-cont)):VALUE = "Usu rio".
        ch-Excel:Range("M" + STRING(i-cont)):HorizontalAlignment = 7.
        ASSIGN i-cont = i-cont + 1.
        RUN pi-bordas (INPUT "Relatorio").

        FOR EACH tt-projetos-relat
           WHERE tt-projetos-relat.nr-trans = tt-relat.nr-trans
            BREAK BY tt-projetos-relat.dt-atualiza:
            ch-Excel:Range("A" + STRING(i-cont)):NumberFormat = "@".
            ch-Excel:Range("A" + STRING(i-cont)):VALUE = STRING(tt-projetos-relat.dt-atualiza, "99/99/9999").
            ch-Excel:Range("A" + STRING(i-cont)):HorizontalAlignment = 7.
            ch-Excel:Range("B" + STRING(i-cont)):VALUE = tt-projetos-relat.nr-contrato.
            ch-Excel:Range("B" + STRING(i-cont)):HorizontalAlignment = 7.
            ch-Excel:Range("C" + STRING(i-cont)):VALUE = tt-projetos-relat.num-pedido.
            ch-Excel:Range("C" + STRING(i-cont)):HorizontalAlignment = 7.
            ch-Excel:Range("D" + STRING(i-cont)):VALUE = tt-projetos-relat.tipo.
            ch-Excel:Range("D" + STRING(i-cont)):HorizontalAlignment = 7.
            ch-Excel:Range("E" + STRING(i-cont)):VALUE = tt-projetos-relat.documento.
            ch-Excel:Range("E" + STRING(i-cont)):HorizontalAlignment = 7.
            ch-Excel:Range("F" + STRING(i-cont)):VALUE = STRING(tt-projetos-relat.cod-emitente) + " - " + STRING(tt-projetos-relat.nome-emit).
            ch-Excel:Range("F" + STRING(i-cont)):HorizontalAlignment = 2.
            ch-Excel:Range("G" + STRING(i-cont)):VALUE = STRING(tt-projetos-relat.num-seq-item).
            ch-Excel:Range("G" + STRING(i-cont)):HorizontalAlignment = 7.
            ch-Excel:Range("G" + STRING(i-cont)):WrapText = NO.
            ch-Excel:Range("H" + STRING(i-cont)):VALUE = STRING(tt-projetos-relat.natureza) + " ".
            ch-Excel:Range("H" + STRING(i-cont)):HorizontalAlignment = 7.
            ch-Excel:Range("I" + STRING(i-cont)):VALUE = IF tt-projetos-relat.descricao <> "" THEN STRING(tt-projetos-relat.descricao) ELSE "  ".
            ch-Excel:Range("I" + STRING(i-cont)):HorizontalAlignment = 2.
            ch-Excel:Range("J" + STRING(i-cont)):VALUE = STRING(tt-projetos-relat.sc-codigo).
            ch-Excel:Range("J" + STRING(i-cont)):HorizontalAlignment = 7.
            ch-Excel:Range("K" + STRING(i-cont)):VALUE = STRING(tt-projetos-relat.ct-codigo).
            ch-Excel:Range("K" + STRING(i-cont)):HorizontalAlignment = 7.
            ch-Excel:Range("L" + STRING(i-cont)):VALUE = tt-projetos-relat.d-valor-item-1.
            ch-Excel:Range("L" + STRING(i-cont)):numberformat = "#.##0,00".
            ch-Excel:Range("M" + STRING(i-cont)):VALUE = STRING(tt-projetos-relat.usuario).
            ch-Excel:Range("M" + STRING(i-cont)):HorizontalAlignment = 7.

            RUN pi-bordas (INPUT "Relatorio").
            ASSIGN i-cont = i-cont + 1.
        END.
        ASSIGN i-cont = i-cont + 1.
    END.
    ASSIGN i-cont = i-cont + 1.

    ch-Excel:COLUMNS("A:M"):AutoFit.
    ch-Excel:COLUMNS("I:I"):ColumnWidth = 42.
    ch-Excel:COLUMNS("L:L"):ColumnWidth = 18.
    ch-Excel:COLUMNS("M:M"):ColumnWidth = 18.
END PROCEDURE.

PROCEDURE pi-bordas:
    DEFINE INPUT PARAMETER p-borda AS CHARACTER NO-UNDO.

    ch-Excel:Range("A" + STRING(i-cont) + ":M" + STRING(i-cont)):Borders(7):LineStyle  = 1.
    ch-Excel:Range("A" + STRING(i-cont) + ":M" + STRING(i-cont)):Borders(7):Weight     = 2.
    ch-Excel:Range("A" + STRING(i-cont) + ":M" + STRING(i-cont)):Borders(8):LineStyle  = 1.
    ch-Excel:Range("A" + STRING(i-cont) + ":M" + STRING(i-cont)):Borders(8):Weight     = 2.
    ch-Excel:Range("A" + STRING(i-cont) + ":M" + STRING(i-cont)):Borders(9):LineStyle  = 1.
    ch-Excel:Range("A" + STRING(i-cont) + ":M" + STRING(i-cont)):Borders(9):Weight     = 2.
    ch-Excel:Range("A" + STRING(i-cont) + ":M" + STRING(i-cont)):Borders(10):LineStyle = 1.
    ch-Excel:Range("A" + STRING(i-cont) + ":M" + STRING(i-cont)):Borders(10):Weight    = 2.

    IF p-borda = "Cabecalho" THEN DO:
        ch-Excel:Range("A" + STRING(i-cont) + ":E" + STRING(i-cont)):Borders(10):LineStyle = 1.
        ch-Excel:Range("A" + STRING(i-cont) + ":E" + STRING(i-cont)):Borders(10):Weight    = 2.
        ch-Excel:Range("A" + STRING(i-cont) + ":I" + STRING(i-cont)):Borders(10):LineStyle = 1.
        ch-Excel:Range("A" + STRING(i-cont) + ":I" + STRING(i-cont)):Borders(10):Weight    = 2.
        ch-Excel:Range("A" + STRING(i-cont) + ":M" + STRING(i-cont)):Borders(10):LineStyle = 1.
        ch-Excel:Range("A" + STRING(i-cont) + ":M" + STRING(i-cont)):Borders(10):Weight    = 2.
    END.
    ELSE DO:
        IF p-borda = "Relatorio" THEN DO:
            ch-Excel:Range("A" + STRING(i-cont) + ":A" + STRING(i-cont)):Borders(10):LineStyle = 1.
            ch-Excel:Range("A" + STRING(i-cont) + ":A" + STRING(i-cont)):Borders(10):Weight    = 2.
            ch-Excel:Range("A" + STRING(i-cont) + ":B" + STRING(i-cont)):Borders(10):LineStyle = 1.
            ch-Excel:Range("A" + STRING(i-cont) + ":B" + STRING(i-cont)):Borders(10):Weight    = 2.
            ch-Excel:Range("A" + STRING(i-cont) + ":C" + STRING(i-cont)):Borders(10):LineStyle = 1.
            ch-Excel:Range("A" + STRING(i-cont) + ":C" + STRING(i-cont)):Borders(10):Weight    = 2.
            ch-Excel:Range("A" + STRING(i-cont) + ":D" + STRING(i-cont)):Borders(10):LineStyle = 1.
            ch-Excel:Range("A" + STRING(i-cont) + ":D" + STRING(i-cont)):Borders(10):Weight    = 2.
            ch-Excel:Range("A" + STRING(i-cont) + ":E" + STRING(i-cont)):Borders(10):LineStyle = 1.
            ch-Excel:Range("A" + STRING(i-cont) + ":E" + STRING(i-cont)):Borders(10):Weight    = 2.
            ch-Excel:Range("A" + STRING(i-cont) + ":F" + STRING(i-cont)):Borders(10):LineStyle = 1.
            ch-Excel:Range("A" + STRING(i-cont) + ":F" + STRING(i-cont)):Borders(10):Weight    = 2.
            ch-Excel:Range("A" + STRING(i-cont) + ":G" + STRING(i-cont)):Borders(10):LineStyle = 1.
            ch-Excel:Range("A" + STRING(i-cont) + ":G" + STRING(i-cont)):Borders(10):Weight    = 2.
            ch-Excel:Range("A" + STRING(i-cont) + ":H" + STRING(i-cont)):Borders(10):LineStyle = 1.
            ch-Excel:Range("A" + STRING(i-cont) + ":H" + STRING(i-cont)):Borders(10):Weight    = 2.
            ch-Excel:Range("A" + STRING(i-cont) + ":I" + STRING(i-cont)):Borders(10):LineStyle = 1.
            ch-Excel:Range("A" + STRING(i-cont) + ":I" + STRING(i-cont)):Borders(10):Weight    = 2.
            ch-Excel:Range("A" + STRING(i-cont) + ":J" + STRING(i-cont)):Borders(10):LineStyle = 1.
            ch-Excel:Range("A" + STRING(i-cont) + ":J" + STRING(i-cont)):Borders(10):Weight    = 2.
            ch-Excel:Range("A" + STRING(i-cont) + ":K" + STRING(i-cont)):Borders(10):LineStyle = 1.
            ch-Excel:Range("A" + STRING(i-cont) + ":K" + STRING(i-cont)):Borders(10):Weight    = 2.
            ch-Excel:Range("A" + STRING(i-cont) + ":L" + STRING(i-cont)):Borders(10):LineStyle = 1.
            ch-Excel:Range("A" + STRING(i-cont) + ":L" + STRING(i-cont)):Borders(10):Weight    = 2.
            ch-Excel:Range("A" + STRING(i-cont) + ":M" + STRING(i-cont)):Borders(10):LineStyle = 1.
            ch-Excel:Range("A" + STRING(i-cont) + ":M" + STRING(i-cont)):Borders(10):Weight    = 2.
        END.
    END.
END PROCEDURE.

PROCEDURE pi-inicializa-excel:
    ASSIGN c-arquivo = SESSION:TEMP-DIRECTORY + "esin0521-" + STRING(TIME).
    
    CREATE "excel.application" ch-Excel.
    ch-Excel:workbooks:ADD(). 

    ch-Excel:worksheets:ITEM(1):SELECT.
    ch-WorkSheet = ch-Excel:Sheets:Item(1).
    ch-Excel:VISIBLE = FALSE.

    ch-Excel:ActiveWindow:Zoom = 100. 
    ch-Excel:Range("A1"):SELECT.
    ASSIGN i-cont = 1.
END PROCEDURE.

PROCEDURE pi-finaliza-excel:
    IF i-cont = 1 THEN
        ASSIGN tt-param.arquivo-excel = "".
    ELSE
        ASSIGN tt-param.arquivo-excel = c-arquivo + ".xlsx".

    IF i-cont = 1 THEN
        ch-Excel:VISIBLE = FALSE.
    ELSE
        ch-Excel:VISIBLE = TRUE.

    ch-WorkSheet:Saveas(c-arquivo).

    IF l-gerou-excel = NO THEN
        ch-Excel:QUIT().

    /*retira o handle da mem¢ria*/
    RELEASE OBJECT ch-Excel NO-ERROR.
    RELEASE OBJECT ch-WorkSheet NO-ERROR.
END PROCEDURE.

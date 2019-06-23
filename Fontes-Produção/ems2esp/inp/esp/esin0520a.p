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
    FIELD dt-contrato       LIKE contrato-for.dt-contrato
    FIELD dt-ter-validade   LIKE contrato-for.dt-ter-validade
    FIELD nr-contrato       LIKE contrato-for.nr-contrato
    FIELD nr-outros         AS CHARACTER
    FIELD num-pedido        LIKE pedido-compr.num-pedido
    FIELD cod-observa       LIKE docum-est.cod-observa
    FIELD cod-emitente      LIKE contrato-for.cod-emitente
    FIELD nome-emit         LIKE emitente.nome-emit
    FIELD sc-codigo         LIKE mat-rat-contr-inv.sc-codigo
    FIELD ct-codigo         LIKE mat-rat-contr-inv.ct-codigo
    FIELD seq-item          LIKE item-contrat.num-seq-item
    FIELD desc-item         LIKE item.desc-item
    FIELD d-valor-item-1    AS DECIMAL FORMAT ">>>>>,>>>,>>9.9999"
    FIELD nr-trans          AS INTEGER
    FIELD tipo              AS CHARACTER FORMAT "x(20)".

DEFINE VARIABLE nome-emit       LIKE emitente.nome-emit          NO-UNDO.
DEFINE VARIABLE cod-observa     LIKE docum-est.cod-observa       NO-UNDO.
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
DEFINE VARIABLE c-data          AS CHARACTER                     NO-UNDO.
DEFINE VARIABLE c-desc-item     LIKE item.desc-item              NO-UNDO.
DEFINE VARIABLE c-conta-contab  LIKE movto-estoq.conta-contabil  NO-UNDO.
DEFINE VARIABLE c-ct-codigo     LIKE movto-estoq.ct-codigo       NO-UNDO.
DEFINE VARIABLE c-sc-codigo     LIKE movto-estoq.ct-codigo       NO-UNDO.
DEFINE VARIABLE i-nr-contrato   LIKE contrato-for.nr-contrato    NO-UNDO.
DEFINE VARIABLE i-seq-it-cont   AS INTEGER                       NO-UNDO.

DEFINE VARIABLE d-valor-item-2  AS DECIMAL     NO-UNDO.

DEF NEW GLOBAL SHARED VAR i-moeda-g AS INTEGER FORMAT ">9"                                                     NO-UNDO.
DEF NEW GLOBAL SHARED VAR da-conv-g AS DATE FORMAT "99/99/9999" INITIAL TODAY                                  NO-UNDO.
DEF NEW GLOBAL SHARED VAR l-conv-g  AS LOG VIEW-AS RADIO-SET HORIZONTAL RADIO-BUTTONS "Cotacao",YES,"Atual",NO NO-UNDO.

/* VARIµVEIS EXCEL */
DEFINE VARIABLE ch-Excel        AS COMPONENT-HANDLE     NO-UNDO.
DEFINE VARIABLE ch-WorkSheet    AS COMPONENT-HANDLE     NO-UNDO.
DEFINE VARIABLE ch-arquivo      AS COMPONENT-HANDLE     NO-UNDO.
DEFINE VARIABLE ch-planilha     AS COMPONENT-HANDLE     NO-UNDO.
DEFINE VARIABLE i-cont          AS INTEGER              NO-UNDO.
DEFINE VARIABLE c-natur         AS CHARACTER            NO-UNDO.
DEFINE VARIABLE c-arquivo       AS CHARACTER            NO-UNDO.

FIND FIRST tt-param EXCLUSIVE-LOCK NO-ERROR.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i "Inicializando" "*"}
 
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

ASSIGN i-moeda-par = 1.

ASSIGN i-nr-trans = 0.

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

        /*Solicita‡Æo*/
        FOR EACH it-requisicao
           WHERE it-requisicao.ep-codigo    = sub-div-ordem.ep-codigo
             AND it-requisicao.num-ord-inv  = sub-div-ordem.num-ord-magnus
             AND it-requisicao.numero-ordem = 0 NO-LOCK:

            FIND FIRST requisicao 
                 WHERE requisicao.nr-requisicao = it-requisicao.nr-requisicao NO-LOCK NO-ERROR.

            FIND FIRST item 
                 WHERE item.it-codigo = it-requisicao.it-codigo NO-LOCK NO-ERROR.
            ASSIGN c-desc-item = IF AVAIL item THEN item.desc-item ELSE "".

            FIND FIRST plano-aprov
                WHERE plano-aprov.num-solicitacao = it-requisicao.nr-requisicao
                  AND plano-aprov.seq-solic       = it-requisicao.sequencia NO-LOCK NO-ERROR.
            IF AVAIL plano-aprov THEN 
                ASSIGN c-data = STRING(DAY(plano-aprov.dt-emiss-solic), "99") + "/" + STRING(MONTH(plano-aprov.dt-emiss-solic), "99") + "/" + STRING(YEAR(plano-aprov.dt-emiss-solic), "9999").
            ELSE
                ASSIGN c-data = STRING(DAY(requisicao.dt-requisicao), "99") + "/" + STRING(MONTH(requisicao.dt-requisicao), "99") + "/" + STRING(YEAR(requisicao.dt-requisicao), "9999").

            CREATE tt-projetos-relat.
            ASSIGN tt-projetos-relat.nr-trans        = i-nr-trans
                   tt-projetos-relat.dt-contrato     = DATE(c-data)
                   tt-projetos-relat.dt-ter-validade = 01/01/2999
                   tt-projetos-relat.nr-outros       = STRING(requisicao.nr-requisicao)
                   tt-projetos-relat.nr-contrato     = 0
                   tt-projetos-relat.num-pedido      = 0
                   tt-projetos-relat.cod-observa     = 0
                   tt-projetos-relat.cod-emitente    = 0 
                   tt-projetos-relat.nome-emit       = ""
                   tt-projetos-relat.sc-codigo       = it-requisicao.sc-codigo
                   tt-projetos-relat.ct-codigo       = it-requisicao.ct-codigo
                   tt-projetos-relat.seq-item        = it-requisicao.sequencia
                   tt-projetos-relat.desc-item       = c-desc-item
                   tt-projetos-relat.d-valor-item-1  = it-requisicao.val-item
                   tt-projetos-relat.tipo            = "Solicita‡Æo".
        END.

        /*Ordem de Compra/ Pedidos sem contrato*/
        FOR EACH ordem-compra
           WHERE ordem-compra.ep-codigo   = sub-div-ordem.ep-codigo
             AND ordem-compra.num-ord-inv = sub-div-ordem.num-ord-magnus 
             AND ordem-compra.nr-contrato = 0 NO-LOCK:

            IF ordem-compra.situacao = 1 THEN DO:
                FIND FIRST item 
                     WHERE item.it-codigo = ordem-compra.it-codigo NO-LOCK NO-ERROR.
                ASSIGN c-desc-item = IF AVAIL item THEN item.desc-item ELSE "".

                FOR FIRST emitente
                    WHERE emitente.cod-emitente = ordem-compra.cod-emitente NO-LOCK. END.
                ASSIGN nome-emit = IF AVAIL emitente THEN emitente.nome-emit ELSE "".

                ASSIGN d-valor-item-1 = 0.

                FIND FIRST ord-ped
                    WHERE ord-ped.num-ord-comp = ordem-compra.numero-ordem NO-LOCK NO-ERROR.
                IF AVAIL ord-ped THEN
                    ASSIGN d-valor-item-1 = ord-ped.dec-1 * ord-ped.quant-comp.

                IF AVAIL ord-ped THEN 
                    ASSIGN c-data = STRING(DAY(ord-ped.data-1), "99") + "/" + STRING(MONTH(ord-ped.data-1), "99") + "/" + STRING(YEAR(ord-ped.data-1), "9999").
                ELSE 
                    ASSIGN c-data = STRING(DAY(ordem-compra.data-atualiz), "99") + "/" + STRING(MONTH(ordem-compra.data-atualiz), "99") + "/" + STRING(YEAR(ordem-compra.data-atualiz), "9999").

                CREATE tt-projetos-relat.
                ASSIGN tt-projetos-relat.nr-trans        = i-nr-trans
                       tt-projetos-relat.dt-contrato     = DATE(c-data)
                       tt-projetos-relat.dt-ter-validade = 01/01/2999
                       tt-projetos-relat.nr-outros       = STRING(ordem-compra.numero-ordem, "zzzzz9,99")
                       tt-projetos-relat.nr-contrato     = 0
                       tt-projetos-relat.num-pedido      = 0
                       tt-projetos-relat.cod-observa     = 0
                       tt-projetos-relat.cod-emitente    = ordem-compra.cod-emitente 
                       tt-projetos-relat.nome-emit       = nome-emit
                       tt-projetos-relat.sc-codigo       = ordem-compra.sc-codigo
                       tt-projetos-relat.ct-codigo       = ordem-compra.ct-codigo
                       tt-projetos-relat.seq-item        = 0
                       tt-projetos-relat.desc-item       = c-desc-item
                       tt-projetos-relat.d-valor-item-1  = d-valor-item-1
                       tt-projetos-relat.tipo            = "Ordem Compra".
            END.

            IF ordem-compra.situacao = 2 OR  
               ordem-compra.situacao = 3 OR 
               ordem-compra.situacao = 6 THEN DO:
                FIND FIRST item 
                     WHERE item.it-codigo = ordem-compra.it-codigo NO-LOCK NO-ERROR.
                ASSIGN c-desc-item = IF AVAIL item THEN item.desc-item ELSE "".

                FOR FIRST emitente
                    WHERE emitente.cod-emitente = ordem-compra.cod-emitente NO-LOCK. END.
                ASSIGN nome-emit = IF AVAIL emitente THEN emitente.nome-emit ELSE "".

                FIND FIRST pedido-compr
                     WHERE pedido-compr.num-pedido  = ordem-compra.num-pedido 
                       AND pedido-compr.nr-contrato = 0 NO-LOCK NO-ERROR.

                FIND FIRST ord-ped
                     WHERE ord-ped.ep-codigo    = ordem-compra.ep-codigo
                       AND ord-ped.cod-estabel  = ordem-compra.cod-estabel
                       AND ord-ped.num-ord-comp = ordem-compra.numero-ordem
                       AND ord-ped.seq-comp     = 0
                       AND ord-ped.cod-area     = 0 NO-LOCK NO-ERROR.

                ASSIGN d-valor-item-1 = 0
                       d-valor-item-2 = 0.

                IF AVAIL pedido-compr THEN DO:
                    IF AVAIL ord-ped THEN
                        ASSIGN d-valor-item-1 = ord-ped.vl-item[1].

                    FOR EACH movto-nf
                        WHERE movto-nf.num-ord-comp = ordem-compra.numero-ordem NO-LOCK:
                        IF ord-ped.cod-sit-comp <> "E" AND ord-ped.cod-sit-ped <> "E" THEN                  
                           ASSIGN d-valor-item-2 = d-valor-item-2 + (movto-nf.quant-doc-ent * (ord-ped.vl-item[1] / ord-ped.quant-comp)).
                    END.

                    ASSIGN d-valor-item-1 = d-valor-item-1 - d-valor-item-2.     
                END.
                ELSE DO:
                    IF AVAIL ord-ped THEN
                        ASSIGN d-valor-item-1 = ord-ped.dec-1 * ord-ped.quant-comp.
                END.

                IF d-valor-item-1 <= 0 THEN
                    NEXT.

                IF AVAIL ord-ped THEN 
                    ASSIGN c-data = STRING(DAY(ord-ped.data-1), "99") + "/" + STRING(MONTH(ord-ped.data-1), "99") + "/" + STRING(YEAR(ord-ped.data-1), "9999").
                ELSE 
                    ASSIGN c-data = STRING(DAY(ordem-compra.data-atualiz), "99") + "/" + STRING(MONTH(ordem-compra.data-atualiz), "99") + "/" + STRING(YEAR(ordem-compra.data-atualiz), "9999").

                CREATE tt-projetos-relat.
                ASSIGN tt-projetos-relat.nr-trans        = i-nr-trans
                       tt-projetos-relat.dt-contrato     = DATE(c-data)
                       tt-projetos-relat.dt-ter-validade = 01/01/2999
                       tt-projetos-relat.nr-outros       = STRING(ordem-compra.numero-ordem, "zzzzz9,99")
                       tt-projetos-relat.nr-contrato     = 0
                       tt-projetos-relat.num-pedido      = ordem-compra.num-pedido
                       tt-projetos-relat.cod-observa     = IF AVAIL pedido-compr THEN pedido-compr.natureza ELSE 0
                       tt-projetos-relat.cod-emitente    = ordem-compra.cod-emitente 
                       tt-projetos-relat.nome-emit       = nome-emit
                       tt-projetos-relat.sc-codigo       = ordem-compra.sc-codigo
                       tt-projetos-relat.ct-codigo       = ordem-compra.ct-codigo
                       tt-projetos-relat.seq-item        = 0
                       tt-projetos-relat.desc-item       = c-desc-item
                       tt-projetos-relat.d-valor-item-1  = d-valor-item-1
                       tt-projetos-relat.tipo            = "Ordem Compra" /*IF AVAIL pedido-compr THEN "Ordem Compra/Pedido" ELSE "Ordem Compra"*/ .            
            END.
        END.

        ASSIGN d-valor-item-1 = 0.

        FOR EACH  controle-inv-esp 
            WHERE controle-inv-esp.ep-codigo    = sub-div-ordem.ep-codigo   
              /*AND controle-inv-esp.cod-est-exec = sub-div-ordem.cod-est-exec*/
              AND controle-inv-esp.num-projeto  = sub-div-ordem.num-projeto 
              AND controle-inv-esp.num-ordem    = sub-div-ordem.num-ordem
              AND controle-inv-esp.num-ord-inv  = sub-div-ordem.num-ord-magnus
              /*AND controle-inv-esp.tipo-doc     = RETURN-VALUE */
              AND controle-inv-esp.serie-docto  = "" 
              AND controle-inv-esp.nat-operacao = "" NO-LOCK:

            {utp/ut-liter.i "Item_Contrato"}
 
            IF controle-inv-esp.tipo-doc = RETURN-VALUE THEN DO:
                ASSIGN d-valor-item-1 = 0
                       i-nr-contrato  = 0
                       i-seq-it-cont  = 0.
    
                IF controle-inv-esp.nro-docto <> "" THEN 
                    ASSIGN i-nr-contrato = INT(ENTRY(1, controle-inv-esp.nro-docto, "/")).
                IF NUM-ENTRIES(controle-inv-esp.nro-docto, "/") > 1 THEN
                    ASSIGN i-seq-it-cont = INT(ENTRY(2, controle-inv-esp.nro-docto, "/")).
                ELSE 
                    ASSIGN i-seq-it-cont = 1.
    
                /*ASSIGN d-valor-item-1 = controle-inv-esp.ent-comp.*/
    
                FIND FIRST contrato-for 
                     WHERE contrato-for.nr-contrato = i-nr-contrato NO-LOCK NO-ERROR.
                IF AVAIL contrato-for THEN DO:
                    FIND FIRST item-contrat
                         WHERE item-contrat.nr-contrato  = contrato-for.nr-contrato
                           AND item-contrat.num-seq-item = i-seq-it-cont NO-LOCK NO-ERROR.
                    
                    FIND FIRST pedido-compr
                         WHERE pedido-compr.nr-contrato = contrato-for.nr-contrato NO-LOCK NO-ERROR.
                    IF AVAIL pedido-compr THEN
                        ASSIGN cod-observa = pedido-compr.natureza.
                        
                    FOR FIRST emitente
                        WHERE emitente.cod-emitente = contrato-for.cod-emitente NO-LOCK. END.
                    ASSIGN nome-emit = IF AVAIL emitente then emitente.nome-abrev else "".
                
                    RUN pi-acompanhar IN h-acomp (INPUT "Criando Registros").
    
                    IF AVAIL item-contrat THEN DO:
                        ASSIGN d-valor-item-1 = item-contrat.val-total.

                        FIND FIRST item 
                            WHERE item.it-codigo = item-contrat.it-codigo NO-LOCK NO-ERROR.
                        ASSIGN c-desc-item = IF AVAIL item THEN item.desc-item ELSE "".
                    END.
                    
                    ASSIGN c-ct-codigo = ""
                           c-sc-codigo = "".
                    FIND FIRST mat-rat-contr-inv
                         WHERE mat-rat-contr-inv.nr-contrato = contrato-for.nr-contrato NO-LOCK NO-ERROR.
                    IF AVAIL mat-rat-contr-inv THEN
                        ASSIGN c-ct-codigo = mat-rat-contr-inv.ct-codigo
                               c-sc-codigo = mat-rat-contr-inv.sc-codigo.
                    ELSE
                        FIND FIRST mat-rat-item-inv
                             WHERE mat-rat-item-inv.nr-contrato = contrato-for.nr-contrato NO-LOCK NO-ERROR.
                        IF AVAIL mat-rat-item-inv THEN
                            ASSIGN c-ct-codigo = mat-rat-item-inv.ct-codigo
                                   c-sc-codigo = mat-rat-item-inv.sc-codigo.
                        ELSE
                            FIND FIRST mat-rat-med-inv
                                 WHERE mat-rat-med-inv.nr-contrato     = contrato-for.nr-contrato 
                                  AND mat-rat-med-inv.conta-contabil <> "" NO-LOCK NO-ERROR.
                            IF AVAIL mat-rat-med-inv THEN
                                ASSIGN c-ct-codigo = mat-rat-med-inv.ct-codigo
                                       c-sc-codigo = mat-rat-med-inv.sc-codigo.

                    ASSIGN c-data = STRING(DAY(controle-inv-esp.dt-trans), "99") + "/" + STRING(MONTH(controle-inv-esp.dt-trans), "99") + "/" + STRING(YEAR(controle-inv-esp.dt-trans), "9999").
                    
                    CREATE tt-projetos-relat.
                    ASSIGN tt-projetos-relat.nr-trans        = i-nr-trans
                           tt-projetos-relat.dt-contrato     = DATE(c-data)
                           tt-projetos-relat.dt-ter-validade = contrato-for.dt-ter-validade
                           tt-projetos-relat.nr-outros       = " "
                           tt-projetos-relat.nr-contrato     = contrato-for.nr-contrato
                           tt-projetos-relat.num-pedido      = IF AVAIL pedido-compr THEN pedido-compr.num-pedido ELSE 0
                           tt-projetos-relat.cod-observa     = cod-observa
                           tt-projetos-relat.cod-emitente    = contrato-for.cod-emitente
                           tt-projetos-relat.nome-emit       = nome-emit
                           tt-projetos-relat.sc-codigo       = c-sc-codigo
                           tt-projetos-relat.ct-codigo       = c-ct-codigo
                           tt-projetos-relat.seq-item        = IF AVAIL item-contrat THEN item-contrat.num-seq-item ELSE 0
                           tt-projetos-relat.desc-item       = c-desc-item
                           tt-projetos-relat.d-valor-item-1  = d-valor-item-1
                           tt-projetos-relat.tipo            = " " /*IF AVAIL pedido-compr THEN "Contrato/Pedido" ELSE "Item/Contrato"*/.
                END.
            END.
            
            IF controle-inv-esp.tipo-doc = "Contrato" THEN DO:
                ASSIGN d-valor-item-1 = 0
                       i-nr-contrato  = 0
                       i-seq-it-cont  = 0.

                IF controle-inv-esp.nro-docto <> "" THEN 
                    ASSIGN i-nr-contrato = INT(ENTRY(1, controle-inv-esp.nro-docto, "/")).
                IF NUM-ENTRIES(controle-inv-esp.nro-docto, "/") > 1 THEN
                    ASSIGN i-seq-it-cont = INT(ENTRY(2, controle-inv-esp.nro-docto, "/")).
                ELSE 
                    ASSIGN i-seq-it-cont = 1.
    
                FIND FIRST contrato-for 
                     WHERE contrato-for.nr-contrato = i-nr-contrato NO-LOCK NO-ERROR.
                IF AVAIL contrato-for THEN DO:
                    IF CAN-FIND (FIRST item-contrat
                                 WHERE item-contrat.nr-contrato  = contrato-for.nr-contrato) THEN DO:

                        FOR EACH item-contrat
                           WHERE item-contrat.nr-contrato  = contrato-for.nr-contrato NO-LOCK
                           BREAK BY item-contrat.num-seq-item:
                            IF item-contrat.num-seq-item = i-seq-it-cont THEN
                                ASSIGN d-valor-item-1 = contrato-for.dec-2.
                                /*ASSIGN d-valor-item-1 = controle-inv-esp.ent-comp.*/
                            ELSE
                                ASSIGN d-valor-item-1 = 0.
                        
                            FIND FIRST pedido-compr
                                 WHERE pedido-compr.nr-contrato = contrato-for.nr-contrato NO-LOCK NO-ERROR.
                            IF AVAIL pedido-compr THEN
                                ASSIGN cod-observa = pedido-compr.natureza.
                                
                            FOR FIRST emitente
                                WHERE emitente.cod-emitente = contrato-for.cod-emitente NO-LOCK. END.
                            IF AVAIL emitente THEN
                                ASSIGN nome-emit = emitente.nome-abrev.

                            RUN pi-acompanhar IN h-acomp (INPUT "Criando Registros").

                            FIND FIRST item 
                                 WHERE item.it-codigo = item-contrat.it-codigo NO-LOCK NO-ERROR.
                            ASSIGN c-desc-item = IF AVAIL item THEN item.desc-item ELSE "".

                            FIND FIRST mat-rat-contr-inv
                                 WHERE mat-rat-contr-inv.nr-contrato = contrato-for.nr-contrato NO-LOCK NO-ERROR.
                            IF AVAIL mat-rat-contr-inv THEN
                                ASSIGN c-ct-codigo = mat-rat-contr-inv.ct-codigo
                                       c-sc-codigo = mat-rat-contr-inv.sc-codigo.
                            ELSE
                                FIND FIRST mat-rat-item-inv
                                     WHERE mat-rat-item-inv.nr-contrato = contrato-for.nr-contrato NO-LOCK NO-ERROR.
                                IF AVAIL mat-rat-item-inv THEN
                                    ASSIGN c-ct-codigo = mat-rat-item-inv.ct-codigo
                                           c-sc-codigo = mat-rat-item-inv.sc-codigo.
                                ELSE
                                    FIND FIRST mat-rat-med-inv
                                         WHERE mat-rat-med-inv.nr-contrato     = contrato-for.nr-contrato 
                                          AND mat-rat-med-inv.conta-contabil <> "" NO-LOCK NO-ERROR.
                                    IF AVAIL mat-rat-med-inv THEN
                                        ASSIGN c-ct-codigo = mat-rat-med-inv.ct-codigo
                                               c-sc-codigo = mat-rat-med-inv.sc-codigo.

                            ASSIGN c-data = STRING(DAY(controle-inv-esp.dt-trans), "99") + "/" + STRING(MONTH(controle-inv-esp.dt-trans), "99") + "/" + STRING(YEAR(controle-inv-esp.dt-trans), "9999").
                            
                            CREATE tt-projetos-relat.
                            ASSIGN tt-projetos-relat.nr-trans        = i-nr-trans
                                   tt-projetos-relat.dt-contrato     = DATE(c-data)
                                   tt-projetos-relat.dt-ter-validade = contrato-for.dt-ter-validade
                                   tt-projetos-relat.nr-outros       = " "
                                   tt-projetos-relat.nr-contrato     = contrato-for.nr-contrato
                                   tt-projetos-relat.num-pedido      = IF AVAIL pedido-compr THEN pedido-compr.num-pedido ELSE 0
                                   tt-projetos-relat.cod-observa     = cod-observa
                                   tt-projetos-relat.cod-emitente    = contrato-for.cod-emitente
                                   tt-projetos-relat.nome-emit       = nome-emit
                                   tt-projetos-relat.sc-codigo       = c-sc-codigo
                                   tt-projetos-relat.ct-codigo       = c-ct-codigo
                                   tt-projetos-relat.seq-item        = IF AVAIL item-contrat THEN item-contrat.num-seq-item ELSE 0
                                   tt-projetos-relat.desc-item       = c-desc-item
                                   tt-projetos-relat.d-valor-item-1  = d-valor-item-1
                                   tt-projetos-relat.tipo            = " " /*IF AVAIL pedido-compr THEN "Contrato/Pedido" ELSE "Contrato"*/ .
                        END.
                    END.
                ELSE DO:
                    ASSIGN d-valor-item-1 = controle-inv-esp.ent-comp.
                    
                    FIND FIRST pedido-compr
                         WHERE pedido-compr.nr-contrato = contrato-for.nr-contrato NO-LOCK NO-ERROR.
                    IF AVAIL pedido-compr THEN
                        ASSIGN cod-observa = pedido-compr.natureza.
                    
                    FOR FIRST emitente
                        WHERE emitente.cod-emitente = contrato-for.cod-emitente NO-LOCK. END.
                    IF AVAIL emitente THEN
                        ASSIGN nome-emit = emitente.nome-abrev.
                    
                    RUN pi-acompanhar IN h-acomp (INPUT "Criando Registros").
                    
                    ASSIGN c-desc-item = "".
                    
                    FIND FIRST mat-rat-contr-inv
                         WHERE mat-rat-contr-inv.nr-contrato = contrato-for.nr-contrato NO-LOCK NO-ERROR.
                    IF AVAIL mat-rat-contr-inv THEN
                        ASSIGN c-ct-codigo = mat-rat-contr-inv.ct-codigo
                               c-sc-codigo = mat-rat-contr-inv.sc-codigo.
                    ELSE
                        FIND FIRST mat-rat-item-inv
                            WHERE mat-rat-item-inv.nr-contrato = contrato-for.nr-contrato NO-LOCK NO-ERROR.
                        IF AVAIL mat-rat-item-inv THEN
                            ASSIGN c-ct-codigo = mat-rat-item-inv.ct-codigo
                                   c-sc-codigo = mat-rat-item-inv.sc-codigo.
                        ELSE
                            FIND FIRST mat-rat-med-inv
                                WHERE mat-rat-med-inv.nr-contrato     = contrato-for.nr-contrato 
                                  AND mat-rat-med-inv.conta-contabil <> "" NO-LOCK NO-ERROR.
                            IF AVAIL mat-rat-med-inv THEN
                                ASSIGN c-ct-codigo = mat-rat-med-inv.ct-codigo
                                       c-sc-codigo = mat-rat-med-inv.sc-codigo.
                    
                    ASSIGN c-data = STRING(DAY(controle-inv-esp.dt-trans), "99") + "/" + STRING(MONTH(controle-inv-esp.dt-trans), "99") + "/" + STRING(YEAR(controle-inv-esp.dt-trans), "9999").

                    CREATE tt-projetos-relat.
                    ASSIGN tt-projetos-relat.nr-trans        = i-nr-trans
                           tt-projetos-relat.dt-contrato     = DATE(c-data)
                           tt-projetos-relat.dt-ter-validade = contrato-for.dt-ter-validade
                           tt-projetos-relat.nr-contrato     = contrato-for.nr-contrato
                           tt-projetos-relat.nr-outros       = " "
                           tt-projetos-relat.num-pedido      = IF AVAIL pedido-compr THEN pedido-compr.num-pedido ELSE 0
                           tt-projetos-relat.cod-observa     = cod-observa
                           tt-projetos-relat.cod-emitente    = contrato-for.cod-emitente
                           tt-projetos-relat.nome-emit       = nome-emit
                           tt-projetos-relat.sc-codigo       = c-sc-codigo
                           tt-projetos-relat.ct-codigo       = c-ct-codigo
                           tt-projetos-relat.seq-item        = IF AVAIL item-contrat THEN item-contrat.num-seq-item ELSE 0
                           tt-projetos-relat.desc-item       = c-desc-item
                           tt-projetos-relat.d-valor-item-1  = d-valor-item-1
                           tt-projetos-relat.tipo            = " " /*IF AVAIL pedido-compr THEN "Contrato/Pedido" ELSE "Contrato"*/ .
                    END.
                END.
            END.
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

        RUN pi-acompanhar IN h-acomp (INPUT "Exportando Projeto - " + tt-relat.desc-proj).

        ch-Excel:Range("A" + STRING(i-cont) + ":M" + STRING(i-cont)):Merge.
        ch-Excel:Range("A" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("A" + STRING(i-cont)):VALUE = "COMPROMISSADO - " + STRING(tt-relat.desc-proj).
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
        ch-Excel:Range("A" + STRING(i-cont)):VALUE = "Valor Total do Projeto:".
        ch-Excel:Range("F" + STRING(i-cont) + ":I" + STRING(i-cont)):Merge.
        ch-Excel:Range("F" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("F" + STRING(i-cont)):VALUE = "Valor Comprometido do Projeto:".
        ch-Excel:Range("J" + STRING(i-cont) + ":M" + STRING(i-cont)):Merge.
        ch-Excel:Range("J" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("J" + STRING(i-cont)):VALUE = "Saldo por Projeto:".
        ch-Excel:Range("A" + STRING(i-cont) + ":M" + STRING(i-cont)):FONT:bold = TRUE.
        
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
        ch-Excel:Range("A" + STRING(i-cont)):VALUE = "Data Contrato/Pedido".
        ch-Excel:Range("A" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("B" + STRING(i-cont)):VALUE = "Data T‚rmino".
        ch-Excel:Range("B" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("C" + STRING(i-cont)):VALUE = "Contrato".
        ch-Excel:Range("C" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("D" + STRING(i-cont)):VALUE = "Pedido".
        ch-Excel:Range("D" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("E" + STRING(i-cont)):VALUE = "Tipo Outros".
        ch-Excel:Range("E" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("F" + STRING(i-cont)):VALUE = "N£mero Outros".
        ch-Excel:Range("F" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("G" + STRING(i-cont)):VALUE = "Natureza".
        ch-Excel:Range("G" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("H" + STRING(i-cont)):VALUE = "Nome Fornecedor".
        ch-Excel:Range("H" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("I" + STRING(i-cont)):VALUE = "C Custo".
        ch-Excel:Range("I" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("J" + STRING(i-cont)):VALUE = "C Cont bil".
        ch-Excel:Range("J" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("K" + STRING(i-cont)):VALUE = "Seq Item".
        ch-Excel:Range("K" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("L" + STRING(i-cont)):VALUE = "Descri‡Æo".
        ch-Excel:Range("L" + STRING(i-cont)):HorizontalAlignment = 7.
        ch-Excel:Range("M" + STRING(i-cont)):VALUE = "Valor".
        ch-Excel:Range("M" + STRING(i-cont)):HorizontalAlignment = 7.
        ASSIGN i-cont = i-cont + 1.
        RUN pi-bordas (INPUT "Relatorio").

        FOR EACH tt-projetos-relat
           WHERE tt-projetos-relat.nr-trans = tt-relat.nr-trans
            BREAK BY tt-projetos-relat.dt-contrato:
            ASSIGN c-natur = IF tt-projetos-relat.cod-observa <> 0 THEN {ininc/i01in295.i 4 tt-projetos-relat.cod-observa} ELSE " ".

            ch-Excel:Range("A" + STRING(i-cont)):NumberFormat = "@".
            ch-Excel:Range("A" + STRING(i-cont)):VALUE = STRING(tt-projetos-relat.dt-contrato, "99/99/9999").
            ch-Excel:Range("A" + STRING(i-cont)):HorizontalAlignment = 7.
            ch-Excel:Range("B" + STRING(i-cont)):VALUE = STRING(tt-projetos-relat.dt-ter-validade, "99/99/9999").
            ch-Excel:Range("B" + STRING(i-cont)):HorizontalAlignment = 7.
            ch-Excel:Range("C" + STRING(i-cont)):VALUE = IF tt-projetos-relat.nr-contrato <> 0 THEN STRING(tt-projetos-relat.nr-contrato) ELSE " ".
            ch-Excel:Range("C" + STRING(i-cont)):HorizontalAlignment = 7.
            ch-Excel:Range("D" + STRING(i-cont)):VALUE = IF tt-projetos-relat.num-pedido <> 0 THEN STRING(tt-projetos-relat.num-pedido) ELSE " ".
            ch-Excel:Range("D" + STRING(i-cont)):HorizontalAlignment = 7.
            ch-Excel:Range("E" + STRING(i-cont)):VALUE = tt-projetos-relat.tipo.
            ch-Excel:Range("E" + STRING(i-cont)):HorizontalAlignment = 7.
            ch-Excel:Range("F" + STRING(i-cont)):NumberFormat = "@".
            ch-Excel:Range("F" + STRING(i-cont)):VALUE = tt-projetos-relat.nr-outros.
            ch-Excel:Range("F" + STRING(i-cont)):HorizontalAlignment = 7.
            ch-Excel:Range("G" + STRING(i-cont)):VALUE = STRING(c-natur).
            ch-Excel:Range("G" + STRING(i-cont)):HorizontalAlignment = 7.
            ch-Excel:Range("H" + STRING(i-cont)):VALUE = STRING(tt-projetos-relat.cod-emitente) + " - " + STRING(tt-projetos-relat.nome-emit).
            ch-Excel:Range("H" + STRING(i-cont)):HorizontalAlignment = 2.
            ch-Excel:Range("I" + STRING(i-cont)):VALUE = STRING(tt-projetos-relat.sc-codigo).
            ch-Excel:Range("I" + STRING(i-cont)):HorizontalAlignment = 7.
            ch-Excel:Range("J" + STRING(i-cont)):VALUE = STRING(tt-projetos-relat.ct-codigo).
            ch-Excel:Range("J" + STRING(i-cont)):HorizontalAlignment = 7.
            ch-Excel:Range("K" + STRING(i-cont)):VALUE = STRING(tt-projetos-relat.seq-item).
            ch-Excel:Range("K" + STRING(i-cont)):HorizontalAlignment = 7.
            ch-Excel:Range("L" + STRING(i-cont)):VALUE = IF tt-projetos-relat.desc-item <> "" THEN STRING(tt-projetos-relat.desc-item) ELSE "  ".
            ch-Excel:Range("L" + STRING(i-cont)):WrapText = NO.
            ch-Excel:Range("L" + STRING(i-cont)):HorizontalAlignment = 2.
            ch-Excel:Range("M" + STRING(i-cont)):VALUE = tt-projetos-relat.d-valor-item-1.
            ch-Excel:Range("M" + STRING(i-cont)):numberformat = "#.##0,00". 
            /*ch-Excel:Range("M" + STRING(i-cont)):HorizontalAlignment = 7.*/

            RUN pi-bordas (INPUT "Relatorio").
            ASSIGN i-cont = i-cont + 1.
        END.
        ASSIGN i-cont = i-cont + 1.
    END.

    ch-Excel:COLUMNS("A:M"):AutoFit.
    ch-Excel:COLUMNS("L:L"):ColumnWidth = 42.
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
    ASSIGN c-arquivo = SESSION:TEMP-DIRECTORY + "esin0520-" + STRING(TIME).
    
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

    IF i-cont = 1 THEN
        ch-Excel:QUIT().

    /*retira o handle da mem¢ria*/
    RELEASE OBJECT ch-Excel NO-ERROR.
    RELEASE OBJECT ch-WorkSheet NO-ERROR.
END PROCEDURE.

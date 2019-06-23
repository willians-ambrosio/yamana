/********************************************************************************
** Copyright DATASUL S.A. (2014)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquenarrativar meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESCCAPI359 2.00.00.001 } /*** 010001 ***/
/*****************************************************************************
**
**       Programa: CCAPI359.p
**
**       Data....: 03/03/2016
**
**       Autor...: TOTVS S.A.
**
**       Objetivo: Falicitador para zerar saldo de pedidos de compra
**       
*******************************************************************************/
{esp/ccp/escc0396.i}    /* TT Param */
{esp/ccp/esccapi359.i}  /* TT's */
{utp/ut-glob.i}   /* Vari†veis do relat¢rios (padr∆o) */
{cdp/cdcfgmat.i} 
{esp/ccp/escc0396.i2 tt-alt-ped-aux}

/* API Parameters */
DEFINE INPUT  PARAM TABLE FOR tt-param.
DEFINE OUTPUT PARAM TABLE FOR tt-pedido-compr-processado.
DEFINE OUTPUT PARAM TABLE FOR tt-ordem-compra-processado.
DEFINE OUTPUT PARAM TABLE FOR tt-prazo-compra-processado.
DEFINE OUTPUT PARAM TABLE FOR RowErrorsParcelas.

/* Local variables */
DEFINE VARIABLE de-qtd-antigo        LIKE prazo-compra.qtd-sal-forn     NO-UNDO.
DEFINE VARIABLE da-data-antiga       LIKE prazo-compra.data-entrega     NO-UNDO.
DEFINE VARIABLE i-situacao-antiga    LIKE prazo-compra.situacao         NO-UNDO.
DEFINE VARIABLE l-importacao         AS LOGICAL                         NO-UNDO.
DEFINE VARIABLE i-natureza           AS INTEGER                         NO-UNDO.
DEFINE VARIABLE l-im0010             AS LOGICAL                         NO-UNDO. 
DEFINE VARIABLE c-return             AS CHARACTER                       NO-UNDO.
DEFINE VARIABLE c-servico            AS CHARACTER                       NO-UNDO.
DEFINE VARIABLE l-efetivou           AS LOGICAL                         NO-UNDO.
DEFINE VARIABLE l-possui-ordem       AS LOGICAL                         NO-UNDO.
DEFINE VARIABLE l-possui-parcela     AS LOGICAL                         NO-UNDO.
DEFINE VARIABLE r-parcela-ap         AS ROWID                           NO-UNDO.
DEFINE VARIABLE c-lbl-erro           AS CHARACTER                       NO-UNDO.
DEFINE VARIABLE c-lbl-informacao     AS CHARACTER                       NO-UNDO.
DEFINE VARIABLE c-lbl-aviso          AS CHARACTER                       NO-UNDO.
DEFINE VARIABLE h-boin356vl          AS HANDLE                          NO-UNDO.
DEFINE VARIABLE h-boin356            AS HANDLE                          NO-UNDO.
DEFINE VARIABLE h-boin274vl          AS HANDLE                          NO-UNDO.
DEFINE VARIABLE hDBOParam-global     AS HANDLE                          NO-UNDO.
DEFINE VARIABLE hDBOEmitente         AS HANDLE                          NO-UNDO.
DEFINE VARIABLE h-cdapi050           AS HANDLE                          NO-UNDO.

DEFINE BUFFER bf-prazo-compra FOR prazo-compra.

{utp/ut-liter.i "Erro"}
ASSIGN c-lbl-erro = RETURN-VALUE.
{utp/ut-liter.i "Informaá∆o"}
ASSIGN c-lbl-informacao = RETURN-VALUE.
{utp/ut-liter.i "Aviso"}
ASSIGN c-lbl-aviso = RETURN-VALUE.

/********* E X E C U Ä « O  **************/
FIND FIRST tt-param NO-LOCK NO-ERROR.
IF NOT AVAIL tt-param THEN RETURN.
main_block:
DO TRANS ON ERROR  UNDO main_block, LEAVE main_block
         ON ENDKEY UNDO main_block, LEAVE main_block
         ON STOP   UNDO main_block, LEAVE main_block:
    RUN pi-processa.
    IF tt-param.lSimula THEN UNDO main_block.
END.
RETURN "OK":U.

/*=======
** P R O C E D U R E S
*********************/
PROCEDURE pi-processa:
/*------------------------------------------------------------------------------
  Purpose:     Processamento geral do relat¢rio.
  Parameters:  
  Notes:       Verifica os parÉmetros recebidos pelo API para zerar os saldos dos 
               pedidos de compras, atualizar situaá∆o das parcelas/ordem para os
               registro dentro da faixa.
------------------------------------------------------------------------------*/
    RUN initializeDBOs IN THIS-PROCEDURE.

    RUN openQueryStatic IN hDBOParam-global (INPUT "Main":U) NO-ERROR.
    RUN findFirst       IN hDBOParam-global.
    RUN getLogField     IN hDBOParam-global (input "modulo-07":U,output l-importacao).
    RUN openQueryStatic IN hDBOEmitente (INPUT "Main":U) NO-ERROR.

    FOR FIRST param-aprov  NO-LOCK: END.
    FOR FIRST param-compra NO-LOCK: END.
    FOR EACH pedido-compr NO-LOCK
       WHERE pedido-compr.num-pedido   >= tt-param.iPedidoIni     
         AND pedido-compr.num-pedido   <= tt-param.iPedidoFim     
         AND pedido-compr.data-pedido  >= tt-param.daDataIni      
         AND pedido-compr.data-pedido  <= tt-param.daDataFim      
         AND pedido-compr.cod-emitente >= tt-param.iEmitenteIni   
         AND pedido-compr.cod-emitente <= tt-param.iEmitenteFim   
         AND pedido-compr.cod-estabel  >= tt-param.cEstabelecIni  
         AND pedido-compr.cod-estabel  <= tt-param.cEstabelecFim  
         AND pedido-compr.responsavel  >= tt-param.cResponsavelIni
         AND pedido-compr.responsavel  <= tt-param.cResponsavelFim
         AND pedido-compr.situacao     <> 3:

        run gotoKey         IN hDBOEmitente (input pedido-compr.cod-emitente).
        run getIntfield     IN hDBOEmitente (input "natureza":U, output i-natureza).

        CREATE tt-pedido-compr-processado.
        BUFFER-COPY pedido-compr TO tt-pedido-compr-processado.
        ASSIGN tt-pedido-compr-processado.r-rowid = ROWID(pedido-compr)
               l-possui-ordem = NO.
        
        FOR EACH ordem-compra NO-LOCK
           WHERE ordem-compra.num-pedido    = pedido-compr.num-pedido
             AND ordem-compra.numero-ordem >= tt-param.iOrdemIni 
             AND ordem-compra.numero-ordem <= tt-param.iOrdemFim
             AND ordem-compra.situacao     <> 4:
            
            CREATE tt-ordem-compra.
            BUFFER-COPY ordem-compra TO tt-ordem-compra.
            ASSIGN tt-ordem-compra.r-rowid = ROWID(ordem-compra)
                   l-efetivou = NO
                   l-possui-parcela = NO.

            BLOCK_update:
            DO TRANS: 
                FOR EACH prazo-compra NO-LOCK
                   WHERE prazo-compra.numero-ordem  = ordem-compra.numero-ordem
                     AND prazo-compra.parcela      >= tt-param.iParcelaIni     
                     AND prazo-compra.parcela      <= tt-param.iParcelaFim     
                     AND prazo-compra.data-entrega >= tt-param.daDataEntregaIni
                     AND prazo-compra.data-entrega <= tt-param.daDataEntregaFim
                     AND prazo-compra.situacao     <> 4:
                    
                    IF (tt-param.lAtualizaSituacao AND prazo-compra.situacao <> 6) OR
                       (tt-param.lZeraSaldo AND prazo-compra.qtd-sal-forn <> 0) THEN DO:
                        ASSIGN l-possui-parcela = YES
                               l-possui-ordem   = YES.
                    
                        /* Limpa a temp-table */
                        FOR EACH tt-prazo-compra:
                            DELETE tt-prazo-compra.
                        END.
        
                        FOR EACH RowErrors:
                            DELETE RowErrors.
                        END.
        
                        CREATE tt-prazo-compra.
                        BUFFER-COPY prazo-compra TO tt-prazo-compra.
        
                        CREATE tt-prazo-compra-origem.
                        BUFFER-COPY prazo-compra TO tt-prazo-compra-origem.
        
                        ASSIGN de-qtd-antigo     = prazo-compra.qtd-sal-forn
                               da-data-antiga    = prazo-compra.data-entrega
                               i-situacao-antiga = prazo-compra.situacao. 
        
                        IF tt-param.lAtualizaSituacao THEN
                            ASSIGN tt-prazo-compra.situacao = 6.
        
                        IF tt-param.lZeraSaldo THEN
                            ASSIGN tt-prazo-compra.qtd-sal-forn = 0.
        
                        IF i-situacao-antiga <> prazo-compra.situacao THEN DO:
                            ASSIGN i-situacao-antiga = prazo-compra.situacao. 
                            IF pedido-compr.situacao = 1 THEN
                                ASSIGN tt-prazo-compra.usuario-alt = c-seg-usuario.
                        END.
        
                        run pi-seta-estado in h-boin274vl (input "UPDATE":U).
        
                        /*--- Transfere a tabela pai para testes com o filho ---*/
                        run inputTableOrdemCompra in h-boin356vl (input table tt-ordem-compra).
        
                        /*--- Limpa a temp-table rowErrors ---*/
                        run emptyRowErrors in h-boin356vl.
        
                        /*--- Efeuta as validacoes da parcela ---*/  
                        RUN openQueryStatic IN h-boin356 (INPUT "Main":U).
                        run gotokey in h-boin356(input tt-prazo-compra.numero-ordem,
                                                 input tt-prazo-compra.parcela).
                        run getRowid in h-boin356(output tt-prazo-compra.r-rowid).
        
                        run validateUpdateManutOrdCompra in h-boin356vl (input  table tt-prazo-compra,
                                                                         input  table tt-prazo-compra-origem,
                                                                         input  "UPDATE":U,
                                                                         input  tt-prazo-compra.r-rowid,
                                                                         input  tt-param.daIntervaloIni,
                                                                         input  tt-param.daIntervaloFim,
                                                                         input  "",  /* usuario do sistema */
                                                                         input  tt-param.lAtualizaParcLoteMin,
                                                                         input  NO,
                                                                         input  NO,
                                                                         input  tt-param.lAtualizaParcLote,
                                                                         output table RowErrors).
                        /* Erros ocorridos no processo */
                        RUN pi-cria-RowErrorsParcelas IN THIS-PROCEDURE (INPUT ordem-compra.numero-ordem).
        
                        IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorSubtype = "ERROR":U) THEN DO:
                            ASSIGN tt-prazo-compra.qtd-sal-forn = de-qtd-antigo
                                   tt-prazo-compra.situacao     = i-situacao-antiga.
                            /* Cria temp-table pra impress∆o */
                            CREATE tt-prazo-compra-processado.
                            BUFFER-COPY tt-prazo-compra TO tt-prazo-compra-processado.
                            NEXT.
                        END.
        
                        /*--- Retorna a temp-table com as quantidades recalculadas ---*/
                        run getRecord in h-boin356vl(output table tt-prazo-compra).
                        find first tt-prazo-compra no-error.
        
                        RUN RecalculoPlanejNetchange in h-boin356vl (input  table tt-ordem-compra,
                                                                     input  pedido-compr.emergencial,
                                                                     input  de-qtd-antigo,
                                                                     input  da-data-antiga,
                                                                     input  tt-prazo-compra.qtd-sal-forn,
                                                                     input  tt-prazo-compra.data-entrega).
                        /* Cria temp-table pra impress∆o */
                        CREATE tt-prazo-compra-processado.
                        BUFFER-COPY tt-prazo-compra TO tt-prazo-compra-processado.
                        ASSIGN l-efetivou = YES.
                    END.
                END. /* each prazo-compra */
    
                FIND FIRST tt-ordem-compra NO-ERROR.
                IF pedido-compr.situacao = 1 THEN DO:
                    {utp/ut-liter.i "Alteraá∆o pelo ESCC0396 ESPEC÷FICO"}
                    ASSIGN tt-ordem-compra.comentarios = RETURN-VALUE.
                END.
    
                run emptyRowErrors in h-boin274vl. 
                run validateUpdatePedEmerg in h-boin274vl (input table tt-ordem-compra, 
                                                           input tt-ordem-compra.r-rowid,
                                                           output table RowErrors).

                RUN pi-cria-RowErrorsParcelas IN THIS-PROCEDURE (INPUT tt-ordem-compra.numero-ordem).
                
                IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorSubtype = "ERROR":U) THEN DO:
                    UNDO BLOCK_update, NEXT.
                END.
                ELSE DO:
                    run emptyRowErrors in h-boin274vl.
                    run executeUpdatePedEmerg in h-boin274vl.
                    RUN getRowErrors IN h-boin274vl (OUTPUT TABLE RowErrors).
                    RUN pi-cria-RowErrorsParcelas IN THIS-PROCEDURE (INPUT tt-ordem-compra.numero-ordem).
                    IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorSubtype = "ERROR":U) THEN
                        UNDO BLOCK_update, NEXT.
                END.
    
                RUN setConstraintNumOrdem in h-boin356 (input tt-ordem-compra.numero-ordem).
                RUN openQueryStatic IN h-boin356 (INPUT "byOrdem":U).
                run emptyRowErrors in h-boin356vl.
    
                RUN verificaEstabelOCxOS IN h-boin274vl. 
                RUN getRowErrors IN h-boin274vl (OUTPUT TABLE RowErrors).
                RUN pi-cria-RowErrorsParcelas IN THIS-PROCEDURE (INPUT tt-ordem-compra.numero-ordem).
    
                IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorSubtype = "ERROR":U) THEN
                    UNDO BLOCK_update, NEXT.
    
                EMPTY TEMP-TABLE RowErrors.
                FOR EACH tt-prazo-compra-processado
                   WHERE tt-prazo-compra-processado.numero-ordem = ordem-compra.numero-ordem:
                    /* Repassa as informacoes da Ordem para as parcelas */
                    EMPTY TEMP-TABLE tt-prazo-compra-aux.
                    CREATE tt-prazo-compra-aux. 
                    BUFFER-COPY tt-prazo-compra-processado TO tt-prazo-compra-aux NO-ERROR.
    
                    run emptyRowObject in h-boin356.
                    run gotoKey in h-boin356 (input tt-prazo-compra-processado.numero-ordem,
                                              input tt-prazo-compra-processado.parcela).
    
                    def var r-aux as rowid no-undo.
                    run getRowid  in h-boin356 (output r-aux).
                    run setRecord in h-boin356vl (input table tt-prazo-compra-aux).
                    run inputTableOrdemCompra in h-boin356vl (input table tt-ordem-compra).
                    run executeUpdateManutOrdCompra IN h-boin356vl(input r-aux). 
                    run getRowErrors in h-boin356vl (output table RowErrors APPEND).
                END.
                RUN pi-cria-RowErrorsParcelas IN THIS-PROCEDURE (INPUT tt-ordem-compra.numero-ordem).
                
                IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorSubtype = "ERROR":U) THEN
                    UNDO BLOCK_update, NEXT.
    
                RUN setConstraintNumOrdem in h-boin356 (input tt-ordem-compra.numero-ordem).  
                RUN openQueryStatic IN h-boin356 (INPUT "byOrdem":U).  
    
                run atualizaSituacaoOrdemCompra in h-boin356vl (input tt-ordem-compra.numero-ordem).

                /* Gera alteraá∆o do pedido */
                FOR EACH tt-prazo-compra-origem:
                    run goToKey in h-boin356 (input tt-prazo-compra-origem.numero-ordem,
                                              input tt-prazo-compra-origem.parcela).

                    if  return-value = "NOK":U then next.
                    run getRowid     in h-boin356 (output r-parcela-ap).

                    find first tt-alt-ped-aux
                         where tt-alt-ped-aux.char-1 = string(r-parcela-ap) no-error.
                    if  not avail tt-alt-ped-aux then
                        find tt-alt-ped-aux where
                             tt-alt-ped-aux.num-pedido = tt-ordem-compra.num-pedido and
                             tt-alt-ped-aux.numero-ordem = tt-ordem-compra.numero-ordem and
                             tt-alt-ped-aux.parcela = tt-prazo-compra-origem.parcela and
                             tt-alt-ped-aux.data = today and
                             tt-alt-ped-aux.hora = string(time,"hh:mm:ss") no-error.
                    if  not avail tt-alt-ped-aux then do:
                        create tt-alt-ped-aux.
                        assign tt-alt-ped-aux.quantidade   = ?.
                        assign tt-alt-ped-aux.data-entrega = ?.
                        assign tt-alt-ped-aux.preco        = ?.
                    end.

                    assign tt-alt-ped-aux.num-pedido   = tt-ordem-compra.num-pedido
                           tt-alt-ped-aux.numero-ordem = tt-prazo-compra-origem.numero-ordem
                           tt-alt-ped-aux.parcela      = tt-prazo-compra-origem.parcela
                           tt-alt-ped-aux.data         = today
                           tt-alt-ped-aux.hora         = string(time,"hh:mm:ss")
                           tt-alt-ped-aux.char-1       = string(r-parcela-ap). /* estˇ gravando o rowid da parcela */

                    if tt-prazo-compra-origem.quantidade   <> ? then assign tt-alt-ped-aux.quantidade   = tt-prazo-compra-origem.qtd-sal-forn.
                    if tt-prazo-compra-origem.data-entrega <> ? then assign tt-alt-ped-aux.data-entrega = tt-prazo-compra-origem.data-entrega.

                    FOR FIRST cotacao-item FIELDS(pre-unit-for)
                        WHERE cotacao-item.numero-ordem = ordem-compra.numero-ordem 
                          AND cotacao-item.cod-emitente = ordem-compra.cod-emitente 
                          AND cotacao-item.it-codigo    = ordem-compra.it-codigo    
                          AND cotacao-item.cot-aprovada NO-LOCK: 
                        ASSIGN tt-alt-ped-aux.preco = cotacao-item.pre-unit-for.
                    END.
                END.

                IF tt-pedido-compr-processado.situacao = 1 /* impresso */ THEN DO:
                    for each tt-alt-ped-aux:
                        {utp/ut-liter.i "Alteraá∆o pelo ESCC0396 ESPEC÷FICO"}
                        assign tt-alt-ped-aux.observacao = RETURN-VALUE.
  
                        run geraAltPedPedidoImpresso in h-boin274vl (input to-rowid(tt-alt-ped-aux.char-1),
                                                                     input "":U, /* usu†rio do sistema */
                                                                     input tt-alt-ped-aux.preco,
                                                                     input tt-alt-ped-aux.quantidade,
                                                                     input tt-alt-ped-aux.data-entrega,
                                                                     input tt-alt-ped-aux.observacao). /* observaá∆o */
                        delete tt-alt-ped-aux.
                    end.
                END.

                /*
                In°cio Siscoserv
                */
                IF CAN-FIND (FIRST emitente
                             WHERE emitente.cod-emitente = tt-ordem-compra.cod-emitente 
                               AND UPPER(emitente.pais) <> "BRASIL") THEN DO:
                    FOR FIRST item-uni-estab FIELDS(char-1) /*Verifica se existe o item-uni-estab, e pega o tipo do item*/
            		    WHERE item-uni-estab.it-codigo   = tt-ordem-compra.it-codigo
                          AND item-uni-estab.cod-estabel = tt-ordem-compra.cod-estabel NO-LOCK:
                        ASSIGN c-servico = SUBSTRING(item-uni-estab.char-1,133,1).
                    END.
            	    IF NOT AVAIL item-uni-estab THEN DO: /*Se n∆o existir pega a informaá∆o da tabela de Item*/
                        FOR FIRST ITEM FIELDS(char-2)
                            WHERE ITEM.it-codigo = tt-ordem-compra.it-codigo NO-LOCK:
                            ASSIGN c-servico = SUBSTRING(item.char-2,212,1).
                        END.
                    END.
                    
                    IF c-servico = "9":U OR tt-ordem-compra.natureza = 2 THEN DO:             
                        RUN pi-siscoserv-ativo IN h-cdapi050.
                        IF RETURN-VALUE = "OK":U THEN DO:
                             FOR FIRST bf-prazo-compra
                                 WHERE bf-prazo-compra.numero-ordem = tt-ordem-compra.numero-ordem NO-LOCK:
                                RUN pi-grava-ord-compra-sis IN h-cdapi050 (INPUT tt-ordem-compra.numero-ordem,
                                                                           INPUT tt-ordem-compra.num-pedido,   
                                                                           INPUT STRING(tt-ordem-compra.cod-emitente),
                                                                           INPUT tt-ordem-compra.cod-estabel,
                                                                           INPUT bf-prazo-compra.data-entrega).
                             END.
                        END.
                    END.
                END.
                /*  Fim Siscoserv  */
    
                RUN atualizaSituacaoProcessoImportacao IN h-boin356vl (INPUT pedido-compr.num-pedido,
                                                                       INPUT pedido-compr.cod-emitente,
                                                                       INPUT pedido-compr.cod-estabel).
                
                /* Se for Aprovaá∆o por item, e deve-se gerar pendàncias, se n∆o Ç uma simulaá∆o e nenhum erro ocorreu. */
                IF (NOT param-aprov.aprov-total-pedido AND NOT tt-param.lNGeraPend) AND NOT tt-param.lSimula AND l-efetivou THEN
                    RUN pi-gera-pendencia IN THIS-PROCEDURE (INPUT ROWID(ordem-compra)).
            END.

            IF l-possui-parcela THEN DO:
                CREATE tt-ordem-compra-processado.
                BUFFER-COPY ordem-compra TO tt-ordem-compra-processado.
            END.
            FOR LAST tt-ordem-compra:
                DELETE tt-ordem-compra.
            END.
        END. /* each ordem-compra */

        IF NOT l-possui-ordem THEN DO:
            FIND LAST tt-pedido-compr-processado NO-ERROR.
            DELETE tt-pedido-compr-processado.
        END.

        /* Se for Aprovaá∆o por total, e deve-se gerar pendàncias, se n∆o Ç uma simulaá∆o e nenhum erro ocorreu */
        IF (param-aprov.aprov-total-pedido AND NOT tt-param.lNGeraPend) AND NOT tt-param.lSimula AND l-efetivou THEN DO:
            FOR FIRST ordem-compra FIELDS() NO-LOCK /* Rowid da primeira ordem de compra do pedido */
                WHERE ordem-compra.num-pedido = pedido-compr.num-pedido:
                RUN pi-gera-pendencia IN THIS-PROCEDURE (INPUT ROWID(ordem-compra)). 
            END.
        END.
    END. /* each pedido-compr */

    RUN destroyDBOs IN THIS-PROCEDURE.

    RETURN "OK":U.
END PROCEDURE.

PROCEDURE pi-gera-pendencia:
/*------------------------------------------------------------------------------
  Purpose:     Gera uma pendància de aprovaá∆o
  Parameters:  (input) prw-ordem-compra: Rowid da ordem de compra para geraá∆o da pendància (rowid)
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER prw-ordem-compra AS ROWID       NO-UNDO.

    IF param-compra.log-1 THEN DO:
        IF pedido-compr.emergencial THEN DO:
            RUN cdp/cdapi171.p (6,2,prw-ordem-compra).
        END.
        ELSE DO:
            RUN cdp/cdapi171.p (4,2,prw-ordem-compra).
        END.
    END.

    RETURN "OK":U.
END PROCEDURE.

PROCEDURE pi-cria-RowErrorsParcelas:
/*------------------------------------------------------------------------------
  Purpose:     Realiza a criaá∆o da temp-table de erros para cada parcela.
  Parameters:  (input) pi-numero-ordem: N£mero da ordem de compra em que o erro ocorreu. (integer)
  Notes:        
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pi-numero-ordem LIKE ordem-compra.numero-ordem NO-UNDO.

    FOR EACH RowErrors:

        IF RowErrors.ErrorSubType = "ERROR":U /*AND
          (RowErrors.ErrorNumber  = 563 /* Item  est† obsoleto */
            OR
           RowErrors.ErrorNumber  = 47 /* Requisitante n∆o cadastrado(a) */
            OR
           RowErrors.ErrorNumber  = 688 /* Centro de Custo deve ser informado(a) */
            OR
           RowErrors.ErrorNumber  = 1882 /* Ordem de produá∆o finalizada! */
            OR
           RowErrors.ErrorNumber  = 5668 /* Data deve ser maior ou igual a Data de Implantaá∆o do Pedido */
            OR
           RowErrors.ErrorNumber  = 52046 /* Conta Cont†bil  Inexistente ou Inv†lida ! */
            OR
           RowErrors.ErrorNumber  = 17538 /* Data de entrega fora do Per°odo de validade do contrato. */
            OR
           RowErrors.ErrorNumber  = 3297 /* Conta deve ser de Despesa, Passivo ou Ativo e de Sistema! */
            OR
           RowErrors.ErrorNumber  = 3298 /* Conta n∆o aceita movimento! */
            OR
           RowErrors.ErrorNumber  = 52046 /* Conta Cont†bil  Inexistente ! */
            OR
           RowErrors.ErrorNumber  = 265 /* Unidade de Neg¢cio deve ser informado(a). */
           )*/ THEN DO:
            DELETE RowErrors.
        END.
    END.

    /* Erros ocorridos no processo */
    FOR EACH RowErrors:

        CREATE RowErrorsParcelas.
        BUFFER-COPY RowErrors TO RowErrorsParcelas.
        ASSIGN RowErrorsParcelas.numero-ordem = pi-numero-ordem.
        CASE RowErrors.ErrorSubType:
            WHEN "ERROR":U THEN DO:
                ASSIGN RowErrorsParcelas.tipo = c-lbl-erro.
            END.
            WHEN "INFORMATION":U THEN DO:
                ASSIGN RowErrorsParcelas.tipo = c-lbl-informacao.
            END.
            WHEN "WARNING":U THEN DO:
                ASSIGN RowErrorsParcelas.tipo = c-lbl-aviso.
            END.
            OTHERWISE DO:
                ASSIGN RowErrorsParcelas.tipo = c-lbl-erro.
            END.
        END CASE.
    END.

    RETURN "OK":U.
END PROCEDURE.

PROCEDURE initializeDBOs:
/*------------------------------------------------------------------------------
  Purpose:     Inicializa as handles das BO's
  Parameters:  
  Notes:        
------------------------------------------------------------------------------*/
    IF NOT VALID-HANDLE(h-boin356vl) THEN
        run inbo/boin356vl.p persistent set h-boin356vl.

    IF NOT VALID-HANDLE(h-boin356) THEN
        RUN inbo/boin356.p PERSISTENT SET h-boin356.

    IF NOT VALID-HANDLE(h-boin274vl) THEN
        RUN inbo/boin274vl.p PERSISTENT SET h-boin274vl.

    IF NOT VALID-HANDLE(hDBOParam-global) THEN
        RUN inbo/boin682.p PERSISTENT SET hDBOParam-global.

    IF NOT VALID-HANDLE(hDBOEmitente) THEN
        RUN adbo/boad098na.p PERSISTENT SET hDBOEmitente.
    
    IF NOT VALID-HANDLE(h-cdapi050) THEN
        RUN cdp/cdapi050.p PERSISTENT SET h-cdapi050.

    RETURN "OK":U.
END PROCEDURE.
          
PROCEDURE destroyDBOs:
/*------------------------------------------------------------------------------
  Purpose:     Remove as handles das BO's ativas.
  Parameters:  
  Notes:        
------------------------------------------------------------------------------*/
    IF VALID-HANDLE(h-boin356vl) THEN DO:
        DELETE PROCEDURE h-boin356vl.
        ASSIGN h-boin356vl = ?.
    END.

    IF VALID-HANDLE(h-boin356) THEN DO:
        DELETE PROCEDURE h-boin356.
        ASSIGN h-boin356 = ?.
    END.

    IF VALID-HANDLE(h-boin274vl) THEN DO:
        DELETE PROCEDURE h-boin274vl.
        ASSIGN h-boin274vl = ?.
    END.

    IF VALID-HANDLE(hDBOParam-global) THEN DO:
        DELETE PROCEDURE hDBOParam-global.
        ASSIGN hDBOParam-global = ?.
    END.

    IF VALID-HANDLE(hDBOEmitente) THEN DO:
        DELETE PROCEDURE hDBOEmitente.
        ASSIGN hDBOEmitente = ?.
    END.

    IF VALID-HANDLE(h-cdapi050) THEN DO:
        DELETE PROCEDURE h-cdapi050.
        ASSIGN h-cdapi050 = ?.
    END.

    RETURN "OK":U.
END PROCEDURE.

/********************************************************************************
** Copyright DATASUL S.A. (2014)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquenarrativar meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESCC0396RP 2.00.00.000 } /*** 010000 ***/
/*****************************************************************************
**
**       Programa: ESCC0396RP.p
**
**       Data....: 03/03/2016
**
**       Autor...: TOTVS S.A.
**
**       Objetivo: Falicitador para zerar saldo de pedidos de compra
**   
*******************************************************************************/
{esp/ccp/escc0396.i} /* TT Param */
{esp/ccp/esccapi359.i} /* TT's */
{include/i-rpvar.i} /* Vari†veis do relat¢rios (padr∆o) */
{utp/ut-glob.i}
{esp/ccp/escc0396.i1} /* FORM'S */

/* Transfer Definitions */
DEFINE INPUT PARAM raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAM TABLE FOR tt-raw-digita.
CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

/* Variables */
DEFINE VARIABLE c-execution          AS CHARACTER                       NO-UNDO.

/********* E X E C U Ä « O  **************/
RUN esp/ccp/esccapi359.p (INPUT  TABLE tt-param,
                          OUTPUT TABLE tt-pedido-compr-processado,
                          OUTPUT TABLE tt-ordem-compra-processado,
                          OUTPUT TABLE tt-prazo-compra-processado,
                          OUTPUT TABLE RowErrorsParcelas).

{include/i-rpcab.i}
{include/i-rpout.i}
view frame f-cabec.
view frame f-rodape.

RUN pi-impressao-relat IN THIS-PROCEDURE.
RUN pi-impressao-param IN THIS-PROCEDURE.

{include/i-rpclo.i}
RETURN "OK":U.
/*=======
** F U N C T I O N S
*********************/
FUNCTION fnTraduzLog RETURNS CHARACTER (INPUT logValue AS LOGICAL):
    RETURN IF logValue THEN c-lbl-sim ELSE c-lbl-nao.
END FUNCTION.

/*=======
** P R O C E D U R E S
*********************/
PROCEDURE pi-impressao-relat:
/*------------------------------------------------------------------------------
  Purpose:     Realizar a impress∆o dos registros do relat¢rio.
  Parameters: 
  Notes:
------------------------------------------------------------------------------*/
    FOR EACH tt-pedido-compr-processado:
        FOR FIRST emitente FIELDS(nome-abrev cod-emitente) NO-LOCK
            WHERE emitente.cod-emitente = tt-pedido-compr-processado.cod-emitente: 
            ASSIGN c-emitente-pedido = STRING(emitente.cod-emitente) + " - " + emitente.nome-abrev.
        END.
        DISPLAY tt-pedido-compr-processado.num-pedido
                tt-pedido-compr-processado.data-pedido
                c-emitente-pedido
                tt-pedido-compr-processado.cod-estabel
                tt-pedido-compr-processado.responsavel WITH FRAME f-pedido.
        DOWN WITH FRAME f-pedido.
        
        FOR EACH tt-ordem-compra-processado
           WHERE tt-ordem-compra-processado.num-pedido = tt-pedido-compr-processado.num-pedido:
            FOR FIRST prazo-compra FIELDS(un) NO-LOCK
                WHERE prazo-compra.numero-ordem = tt-ordem-compra-processado.numero-ordem: END. /* Pega a unidade de medida de primeira entrega */
            FOR FIRST item FIELDS(desc-item) NO-LOCK
                WHERE item.it-codigo = tt-ordem-compra-processado.it-codigo: END. /* Descriá∆o do item */
            ASSIGN c-situacao-ordem = {ininc/i02in274.i 04 tt-ordem-compra-processado.situacao}.

            DISPLAY tt-ordem-compra-processado.numero-ordem
                    tt-ordem-compra-processado.it-codigo   
                    item.desc-item WHEN AVAIL item
                    prazo-compra.un WHEN AVAIL prazo-compra
                    tt-ordem-compra-processado.qt-solic
                    c-situacao-ordem WITH FRAME f-ordem.
            DOWN WITH FRAME f-ordem.

            FOR EACH tt-prazo-compra-processado
               WHERE tt-prazo-compra-processado.numero-ordem = tt-ordem-compra-processado.numero-ordem:
                ASSIGN c-situacao-parcela = {ininc/i02in274.i 04 tt-prazo-compra-processado.situacao}.

                DISPLAY tt-prazo-compra-processado.parcela
                        tt-prazo-compra-processado.data-entrega
                        c-situacao-parcela
                        tt-prazo-compra-processado.quantidade
                        tt-prazo-compra-processado.qtd-sal-forn WITH FRAME f-parcela.
                DOWN WITH FRAME f-parcela.
            END.
            
            FOR EACH RowErrorsParcelas
               WHERE RowErrorsParcelas.numero-ordem = tt-ordem-compra-processado.numero-ordem
            BREAK BY RowErrorsParcelas.numero-ordem:
                IF FIRST-OF(RowErrorsParcelas.numero-ordem) THEN
                    DISP c-lbl-erros AT 7 NO-LABEL.

                DISP RowErrorsParcelas.ErrorNumber 
                     RowErrorsParcelas.ErrorDescription
                     RowErrorsParcelas.tipo WITH FRAME f-erros.
                DOWN WITH FRAME f-erros.
            END.

        END.
    END.

    RETURN "OK":U.
END PROCEDURE.

PROCEDURE pi-impressao-param:
/*------------------------------------------------------------------------------
  Purpose:     Realizar a impress∆o da p†gina de parÉmetros.
  Parameters:  
  Notes:
------------------------------------------------------------------------------*/
    PAGE.
    DISPLAY c-lbl-selecao
            c-lbl-pedido                                    
            tt-param.iPedidoIni
            tt-param.iPedidoFim 
            c-lbl-data
            tt-param.daDataIni
            tt-param.daDataFim
            c-lbl-fornecedor     
            tt-param.iEmitenteIni    
            tt-param.iEmitenteFim  
            c-lbl-estabelecimento
            tt-param.cEstabelecIni   
            tt-param.cEstabelecFim   
            c-lbl-responsavel    
            tt-param.cResponsavelIni 
            tt-param.cResponsavelFim 
            c-lbl-ordem          
            tt-param.iOrdemIni       
            tt-param.iOrdemFim       
            c-lbl-parcela        
            tt-param.iParcelaIni     
            tt-param.iParcelaFim   
            c-lbl-data-entrega   
            tt-param.daDataEntregaIni
            tt-param.daDataEntregaFim
        WITH FRAME f-selecao.

    DISPLAY c-lbl-parametros
            c-lbl-zera-saldo       
            c-lbl-atualiza-situacao
            c-lbl-atualiza-parc-lote
            c-lbl-atualiza-parc-lote-min
            c-lbl-nao-gera-pend    
            c-lbl-intervalo-validacao
            c-lbl-simular    
            fnTraduzLog(tt-param.lZeraSaldo)        @ tt-param.lZeraSaldo       
            fnTraduzLog(tt-param.lAtualizaSituacao) @ tt-param.lAtualizaSituacao
            fnTraduzLog(tt-param.lAtualizaParcLote) @ tt-param.lAtualizaParcLote
            fnTraduzLog(tt-param.lAtualizaParcLoteMin) @ tt-param.lAtualizaParcLoteMin
            fnTraduzLog(tt-param.lNGeraPend)        @ tt-param.lNGeraPend       
            tt-param.daIntervaloIni
            tt-param.daIntervaloFim
            fnTraduzLog(tt-param.lSimula)           @ tt-param.lSimula          
        WITH FRAME f-param.

    /*  Impress∆o */
    IF tt-param.destino = 1 THEN DO:
       {utp/ut-liter.i Impressora *}
       ASSIGN c-destino-impressao = RETURN-VALUE.
    ENd.

    IF tt-param.destino = 2 THEN DO:
       {utp/ut-liter.i Arquivo *}
       ASSIGN c-destino-impressao = RETURN-VALUE.
    END.

    if tt-param.destino = 3 THEN DO:
       {utp/ut-liter.i Terminal *}
       ASSIGN c-destino-impressao = RETURN-VALUE.
    END.

    if tt-param.destino = 4 THEN DO:
       {utp/ut-liter.i Excel *}
       ASSIGN c-destino-impressao = RETURN-VALUE.
    END.
    
    ASSIGN c-execution = IF tt-param.execution = 1 THEN "On-line" ELSE "Batch".

    DISPLAY c-lbl-impressao
            c-lbl-destino
            c-destino-impressao
            c-lbl-arquivo
            tt-param.arquivo
            c-lbl-usuario
            tt-param.usuario
            c-lbl-execucao
            c-execution @ tt-param.execution
            WITH FRAME f-impressao. 

    RETURN "OK":U.
END PROCEDURE.

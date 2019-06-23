/****************************************************************************************** 
** 	   Programa: tw-ordem-compra.p
**   	  Autor: Gilberto Rissati Garcia
** 	 Fornecedor: GWA
**         Data: 13/10/2003
** Change/Chamado: 
**      Objetivo: Gravar o usuario corrente ao incluir ou modificar ordem de compra
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
** 15/11/2018   Vando Ribeioro  Grupo DKP   REQ01/Preparar dados para enviar e-mail ao Financeiro
**                                          de pedidos de  compras e Contrato de Fornecimento
**                                          com condiá∆o de pagamento inferior a 21 dias. 
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: p-ind-event, p-ind-object, p-wgh-object, p-wgh-frame, p-cod-table e p-row-table
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: cc0300
** CADASTRADO NA TABELA: ordem-compra
******************************************************************************************/

def param buffer p-ordem-compra     for ordem-compra.
def param buffer p-old-ordem-compra for ordem-compra.

DEFINE NEW GLOBAL SHARED VARIABLE c-seg-usuario AS CHARACTER NO-UNDO.
DEFINE VARIABLE i-dias-prazo AS INTEGER     NO-UNDO.
DEFINE BUFFER b_ordem-compra FOR ordem-compra.

/*
MESSAGE  "PROGRAM-NAME(1)" PROGRAM-NAME(1) SKIP
         "PROGRAM-NAME(2)" PROGRAM-NAME(2) SKIP
         "PROGRAM-NAME(3)" PROGRAM-NAME(3) SKIP
         "PROGRAM-NAME(4)" PROGRAM-NAME(4) SKIP
         "PROGRAM-NAME(5)" PROGRAM-NAME(5) SKIP
         "PROGRAM-NAME(6)" PROGRAM-NAME(6) SKIP
         "PROGRAM-NAME(7)" PROGRAM-NAME(7) SKIP
 VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/

IF R-INDEX(STRING(PROGRAM-NAME(4)),"cc0301") > 0 THEN DO:
   ASSIGN p-ordem-compra.requisitante = c-seg-usuario.
END.


/*Situaá∆o: 1-N∆o Confirmada, 2-Confirmada, 3-Cotada, 4-Eliminada, 5-Em Cotaá∆o 6-Recebida
  Vando Ribeiro*/

FIND cond-pagto OF p-ordem-compra NO-LOCK NO-ERROR.
IF AVAIL cond-pagto THEN
DO:
    IF p-ordem-compra.situacao = 2 AND cond-pagto.prazos[1] < 21 THEN
    DO:
        FIND FIRST esp_hist_email_pd EXCLUSIVE-LOCK
             WHERE esp_hist_email_pd.numero-ordem = p-ordem-compra.numero-ordem NO-ERROR.
        IF NOT AVAIL esp_hist_email_pd THEN
        DO:
            CREATE esp_hist_email_pd.
            ASSIGN
                esp_hist_email_pd.numero-orde  = p-ordem-compra.numero-ordem
                esp_hist_email_pd.num-pedido   = p-ordem-compra.num-pedido
                esp_hist_email_pd.nr-contrato  = p-ordem-compra.nr-contrato
                esp_hist_email_pd.cod-estabel  = p-ordem-compra.cod-estabel
                esp_hist_email_pd.cod-emitente = p-ordem-compra.cod-emitente
                esp_hist_email_pd.cod-cond-pag = p-ordem-compra.cod-cond-pag
                esp_hist_email_pd.cod-usuario  = c-seg-usuario.
        END.
        ELSE
            ASSIGN
                esp_hist_email_pd.cod-cond-pag = p-ordem-compra.cod-cond-pag
                esp_hist_email_pd.cod-usuario  = c-seg-usuario.
    
    END.
    IF p-ordem-compra.situacao = 2 AND cond-pagto.prazos[1] >= 21 THEN
    DO:
        /*situaá∆o em que o usu†rio informou uma condiá∆o de pagto inferior a 21 dias e
         antes do RPW enviar o e-mail o usuario mudou a codiá∆o pagto para igual ou superior a 21 dias,
         ent∆o o sistema apaga o registro porque n∆o enquadraria na regra inferior a 21 dias.*/
    
        FIND FIRST esp_hist_email_pd EXCLUSIVE-LOCK
             WHERE esp_hist_email_pd.numero-ordem = p-ordem-compra.numero-ordem NO-ERROR.
        IF AVAIL esp_hist_email_pd AND esp_hist_email_pd.dt-envio-email = ? THEN
            DELETE esp_hist_email_pd.
    END.
END.

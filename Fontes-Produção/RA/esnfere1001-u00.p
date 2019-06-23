/****************************************************************************************** 
** 	       Programa: esnfere1001-u00.p
**   	      Autor: Alberto Duzi
**   	 Fornecedor: DKP
**    	 Data: 15/10/2009
** Change/Chamado: REQ06
**       Objetivo: UPC para Inserir Botao de Busca de DANFE para Digitacao
**                 Automatica - Recebimento Fiscal 
******************************** CONTROLE DE ALTERA∞ÜES *********************************
** 
** Data         Autor   		Fornecedor  Change/Chamado          Descriªío da Alteraªío
** 17/11/2018   Daniela Campos  DKP         Projeto Tesouraria      Efetua chamadas da epc re1005rp-e02.p
**

****************************** INFORMA∞ÜES ADICIONAIS ************************************
** PARÙMETROS DE ENTRADA: p-ind-event
** PARÙMETROS DE SAôDA: tt-epc
** CADASTRADO NO FONTE TOTVS: RE1005RP
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
DEFINE INPUT PARAM p-ind-event      AS CHAR             NO-UNDO.
DEFINE INPUT PARAM p-ind-object     AS CHAR             NO-UNDO.
DEFINE INPUT PARAM p-wgh-object     AS HANDLE           NO-UNDO.
DEFINE INPUT PARAM p-wgh-frame      AS WIDGET-HANDLE    NO-UNDO.
DEFINE INPUT PARAM p-cod-table      AS CHAR             NO-UNDO.
DEFINE INPUT PARAM p-row-table      AS ROWID            NO-UNDO.
    
/* --- Cria Botao para Busca de DANFE - Recebimento Fiscal --- */

RUN dsc\ra\upc\esnfere1001-u01.p (INPUT p-ind-event,
                                  INPUT p-ind-object,
                                  INPUT p-wgh-object,
                                  INPUT p-wgh-frame,
                                  INPUT p-cod-table,
                                  INPUT p-row-table).


RUN epc\re1001-epc00.p (INPUT p-ind-event,
                        INPUT p-ind-object,
                        INPUT p-wgh-object,
                        INPUT p-wgh-frame,
                        INPUT p-cod-table,
                        INPUT p-row-table).

/* /*Vando Ribeiro*/                         */
RUN upc\re1001-U01.p (INPUT p-ind-event,
                      INPUT p-ind-object,
                      INPUT p-wgh-object,
                      INPUT p-wgh-frame,
                      INPUT p-cod-table,
                      INPUT p-row-table).

/* UPC RETENCOES RECEBIMENTO CONTRATOS */
RUN upc/re1001-upc02.P(input p-ind-event,
                        input p-ind-object,
                        input p-wgh-object,
                        input p-wgh-frame,
                        input p-cod-table,
                        input p-row-table).

 

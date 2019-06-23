
/*****************************************************************************
 ** PROGRAMA..: CC0311-U00.P
 ** OBJETIVO..: UPC NA GERACAO AUTOMATICA DE PEDIDOS - CC0311
 ** AUTOR.....: DSC
 ** CLIENTE...: YAMANA
 ** VERSAO....: 2.00.00.001 - 21/05/2009 - Luiz CRUZ.
 ** ALTERA€åES:
 ******************************************************************************/

/* *** DEFINICAO DE PARAMETROS *** */
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.


RUN upc/cc0311-upc01.P (INPUT p-ind-event, 
                        INPUT p-ind-object,
                        INPUT p-wgh-object,
                        INPUT p-wgh-frame, 
                        INPUT p-cod-table, 
                        INPUT p-row-table).
                           
                            
 

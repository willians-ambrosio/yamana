/*****************************************************************************
 ** PROGRAMA..: CN0302a-upc00.P
 ** OBJETIVO..: UPC MEDICAO DE CONTRATOS - CN0302A
 ** AUTOR.....: DSC
 ** CLIENTE...: YAMANA
 ** VERSAO....: 2.00.00.001   
 ** ALTERA€åES:
 ******************************************************************************/

/* *** DEFINICAO DE PARAMETROS *** */
define input parameter p-ind-event  as character.
define input parameter p-ind-object as character.
define input parameter p-wgh-object as handle.
define input parameter p-wgh-frame  as widget-handle.
define input parameter p-cod-table  as character.
define input parameter p-row-table  as rowid.
 
/*******************************************************************************
** Chamada do Programa: cn0302a-upc
**            Autor...:  
**            Data....:  
**            OBS.....:  
**            Objetivo: 
*******************************************************************************/

    run upc/cn0302a-upc.p(input p-ind-event, 
                          input p-ind-object,
                          input p-wgh-object,
                          input p-wgh-frame, 
                          input p-cod-table, 
                          input p-row-table).   

    IF RETURN-VALUE = 'NOK'  THEN RETURN-VALUE.
 
                               
   /*UPC CADASTRAR % RETENCOES RECEBIMENTO REF MEDICOES */ 
    run upc/cn0302a-upc02.P(input p-ind-event, 
                            input p-ind-object,
                            input p-wgh-object,
                            input p-wgh-frame, 
                            input p-cod-table, 
                            input p-row-table).
                           
    IF RETURN-VALUE = 'NOK'  THEN RETURN-VALUE.


    RETURN 'OK'.

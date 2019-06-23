/*-----------------------------------------------------------------------------------
    PROGRAMA  : cd0204-upc00.p
    OBJETIVO  : Chamanda de UPC do cd0204
    AUTOR     : Wellington Aparecido - WSA (DSC)
    DATA      : 22/07/2008
    
    ALTERACAO : LUIZ CRUZ
    DATA      : 12/mai/2009
    EMPRESA   : DSC CONSULTORIA
    DESCRICAO : Inclusao da chamada de UPC cd0204-upc02, referente a criacao do campo
                de Departamento a que o Item est  vinculado.        
    
    ALTERACAO : Felipe Vieira
    DATA      : 12/03/2019
    EMPRESA   : Grupo DKP
    DESCRICAO : Inclusao da chamada de UPC cd0204-upc07, referente a criacao do campo
                de EPI/EPC.        
        
    
-----------------------------------------------------------------------------------*/

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i cd0204-upc00.p 2.06.00.000} 

/*************************************************************************************
                                     PARAMETROS
*************************************************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

/*************************************************************************************
                                    MAIN-BLOCK
*************************************************************************************/
/* ---> Defini se a Ser X Est ira imprimir NF-e  
RUN upc/cd0204-upc01.p  (INPUT p-ind-event,
                         INPUT p-ind-object,
                         INPUT p-wgh-object,
                         INPUT p-wgh-frame,
                         INPUT p-cod-table,
                         INPUT p-row-table).     <--- */

/* RUN upc/cd0204-upc02.p  (INPUT p-ind-event,  */  /*Retirado do cd0204 e movido para o cd0138 */
/*                          INPUT p-ind-object, */
/*                          INPUT p-wgh-object, */
/*                          INPUT p-wgh-frame,  */
/*                          INPUT p-cod-table,  */
/*                          INPUT p-row-table). */

RUN upc/cd0204-upc03.p  (INPUT p-ind-event,
                         INPUT p-ind-object,
                         INPUT p-wgh-object,
                         INPUT p-wgh-frame,
                         INPUT p-cod-table,
                         INPUT p-row-table).


RUN upc/cd0204-upc04.p  (INPUT p-ind-event,    
                         INPUT p-ind-object,   
                         INPUT p-wgh-object,   
                         INPUT p-wgh-frame,    
                         INPUT p-cod-table,    
                         INPUT p-row-table).




/* RUN upc/cd0204-upc05.p  (INPUT p-ind-event,   */
/*                          INPUT p-ind-object,  */
/*                          INPUT p-wgh-object,  */
/*                          INPUT p-wgh-frame,   */
/*                          INPUT p-cod-table,   */
/*                          INPUT p-row-table).  */


RUN upc/cd0204-upc06.p  (INPUT p-ind-event,  
                         INPUT p-ind-object, 
                         INPUT p-wgh-object, 
                         INPUT p-wgh-frame,  
                         INPUT p-cod-table,  
                         INPUT p-row-table). 

/**/
RUN upc/cd0204-upc07.p  (INPUT p-ind-event,  
                         INPUT p-ind-object, 
                         INPUT p-wgh-object, 
                         INPUT p-wgh-frame,  
                         INPUT p-cod-table,  
                         INPUT p-row-table). 





IF RETURN-VALUE = "NOK":U THEN
  RETURN "NOK":U.




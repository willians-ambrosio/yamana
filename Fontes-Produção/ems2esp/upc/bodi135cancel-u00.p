/* --------------------------------------------------------------------------------------- *\
|                                                                                           |

|  Modulo.................: EYM - Especificos Yamana                                        |
|  Programa...............: bodi135cancel0-u00.P                                            |
|  Sub Programa...........:                                                                 |
|  Descricao..............: EPC NA BO DE CANCELAMENTO DA NF                                 |
|  Entidade Desenvolvedora: DSC                                                             |
|                                                                                           |
|  Historico Programa -------------------------------------------------------------------+  |
|  | Data       | Autor               | Descricao                                        |  |
|  +----------- +---------------------+--------------------------------------------------+  |
|  | 08|08|2008 | Wellington Ap       | Desenvolvimento do Programa                      |  |
|  +------------+---------------------+--------------------------------------------------+  |
|  | Parametros :                                                                        |  |
|  |                                                                                     |  |
|  | Observacao :                                                                        |  |
|  |                                                                                     |  |
|  +-------------------------------------------------------------------------------------+  |

\  --------------------------------------------------------------------------------------- */
{include/i-prgvrs.i bodi135cancel-u00 2.06.00.000}

{include/i-epc200.i  bodi135cancel}
    

DEFINE INPUT PARAM  p-ind-event AS  CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAM TABLE FOR tt-epc.      
DEFINE VARIABLE l-erro AS LOGICAL.

/* ---> Wellington DSC (Tratamento Layout NF-e) <--- */
RUN upc/bodi135cancel-u01.p (INPUT p-ind-event,
                             INPUT-OUTPUT TABLE tt-epc,
                             INPUT-OUTPUT l-erro) .   
                             
IF l-erro THEN 
  RETURN "NOK".
  


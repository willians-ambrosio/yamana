/* --------------------------------------------------------------------------------------- *\
|                                                                                           |

|  Modulo.................: EYM - Especificos Yamana                                        |
|  Programa...............: ft20100-u00.P                                                |
|  Sub Programa...........:                                                                 |
|  Descricao..............: EPC NA BO DE EFETIVACAO DA NF                                   |
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
{include/i-prgvrs.i ft2010-u00 2.06.00.000}

/* --------------------------------------------------------------------------------------- *|
|*                                DEFINIÇÃO TEMP-TBLE                                      *| 
\* --------------------------------------------------------------------------------------- */                      
{include/i-epc200.i  ft2010}

DEFINE INPUT PARAM  p-ind-event AS  CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAM TABLE FOR tt-epc.      
DEFINE VARIABLE l-erro AS LOGICAL.

/* ---> Wellington DSC (Tratamento Layout NF-e) <--- */
RUN upc/ft2010-u01.p (INPUT p-ind-event,
                      INPUT-OUTPUT TABLE tt-epc,
                      INPUT-OUTPUT l-erro) .   
                             
IF l-erro THEN 
  RETURN "NOK".

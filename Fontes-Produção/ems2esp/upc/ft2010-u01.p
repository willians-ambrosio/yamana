/* --------------------------------------------------------------------------------------- *\
|                                                                                           |

|  Modulo.................: EYM - Especificos Yamana                                        |
|  Programa...............: ft2010-u00.P                                                    |
|  Sub Programa...........:                                                                 |
|  Descricao..............: EPC NA BO DE EFETIVACAO DA NF DE IMPORTACAO                     |
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
{include/i-prgvrs.i ft2010-U01 2.06.00.000} 

/* --------------------------------------------------------------------------------------- *|
|*                                DEFINIÇÃO TEMP-TBLE                                      *| 
\* --------------------------------------------------------------------------------------- */                      
{include/i-epc200.i  ft2010}
{include/i-epc200.i2 ft2010}

  /*
DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence     AS INTEGER
    FIELD ErrorNumber       AS INTEGER
    FIELD ErrorDescription  AS CHARACTER
    FIELD ErrorParameters   AS CHARACTER
    FIELD ErrorType         AS CHARACTER
    FIELD ErrorHelp         AS CHARACTER
    FIELD ErrorSubType      AS CHARACTER. */

/* --------------------------------------------------------------------------------------- *|
|*                                DEFINIÇÃO PARAMETROS                                     *| 
\* --------------------------------------------------------------------------------------- */                      
DEFINE INPUT PARAM  p-ind-event  AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAM TABLE  FOR tt-epc.
DEFINE INPUT-OUTPUT PARAM l-erro AS LOGICAL.

/* --------------------------------------------------------------------------------------- *|
|*                                DEFINIÇÃO VARIAVEIS GLOBAIS                              *| 
\* --------------------------------------------------------------------------------------- */                      


/* --------------------------------------------------------------------------------------- *|
|*                                DEFINIÇÃO VARIAVEIS LOCAIS                               *| 
\* --------------------------------------------------------------------------------------- */                      
DEFINE VARIABLE r-nota-fiscal                                 AS ROWID              NO-UNDO.
DEFINE VARIABLE h-ft2010                                   AS HANDLE             NO-UNDO.
DEFINE VARIABLE c-lista-prog                                  AS CHAR               NO-UNDO.
DEFINE VARIABLE i-cont                                        AS INTEGER            NO-UNDO.
DEFINE VARIABLE c-chave-origem                                AS CHARACTER          NO-UNDO.
/* --------------------------------------------------------------------------------------- *|
|*                                    DEFINI€ÇOÃBUFFER                                     *| 
\* --------------------------------------------------------------------------------------- */                      
DEF BUFFER b-nota-fiscal FOR nota-fiscal.

/* --------------------------------------------------------------------------------------- *|
|*                                     INICIO PROCESSO                                     *| 
\* --------------------------------------------------------------------------------------- */                      

/* RUN pi-msg. */

ASSIGN i-cont = 1.

REPEAT:
  IF PROGRAM-NAME(i-cont) = ? THEN LEAVE.
  ASSIGN c-lista-prog = c-lista-prog + (IF c-lista-prog <> "" THEN "," ELSE "") + PROGRAM-NAME(i-cont)
         i-cont       = i-cont + 1.
END.

/* ---> PROCESSAMENTO DAS INFORMACOES <---  */
FOR EACH tt-epc
  WHERE tt-epc.cod-event     = "Single-Point"
  AND   tt-epc.cod-parameter = "Nota-Fiscal Rowid":

  FIND FIRST nota-fiscal NO-LOCK
    WHERE ROWID(nota-fiscal) = TO-ROWID(tt-epc.val-parameter) NO-ERROR.

  RUN upc\bodi317ef-u01a.p(INPUT ROWID(nota-fiscal),
                           INPUT 'N',
                           INPUT ?,
                           input yes).
  
  
END.

/* ---> MSG DE ACOMPANHAMENTO DE PONTOS EPC <--- */
PROCEDURE pi-msg:

  FOR EACH tt-epc:
    MESSAGE 
      tt-epc.cod-event SKIP
      tt-epc.cod-parameter SKIP
      tt-epc.val-parameter
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
END PROCEDURE.

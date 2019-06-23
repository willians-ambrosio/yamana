
/* --------------------------------------------------------------------------------------- *\
|                                                                                           |
|  Sistema................: EMS2.0X                                                         |
|  Modulo.................:                                                                 |
|  Programa...............: esra002-epc.p                                                  |
|  Sub Programa...........:                                                                 |
|  Descricao..............: Programa com as rotinas de EPCs das TTs de XML.                 |
|  Entidade Desenvolvedora: DSC                                                             |
|                                                                                           |
|  Historico Programa -------------------------------------------------------------------+  |
|  | Data       | Autor               | Descricao                                        |  |
|  +----------- +---------------------+--------------------------------------------------+  |

|  +------------+---------------------+--------------------------------------------------+  |
|  | Parametros :                                                                        |  |
|  |                                                                                     |  |
|  | Observacao :                                                                        |  |
|  |                                                                                     |  |
|  +-------------------------------------------------------------------------------------+  |
|  Vers o: 3.00.000                                                                         |
\  --------------------------------------------------------------------------------------- */

{include/i-epc200.i  esra002}
{include/i-epc200.i2 esra002}


DEFINE INPUT PARAM  p-ind-event  AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAM TABLE  FOR tt-epc.



IF p-ind-event = "FimCarregaCTE" THEN DO:


    FIND FIRST tt-epc WHERE tt-epc.cod-event     = "FimCarregaCTE"
                      AND   tt-epc.cod-parameter = "chave-acesso"
                      NO-LOCK NO-ERROR.
    IF AVAIL tt-epc THEN DO:

        FIND FIRST nfe-cte-inf EXCLUSIVE-LOCK
            WHERE nfe-cte-inf.chave-acesso = tt-epc.val-parameter NO-ERROR.

        IF AVAIL nfe-cte-inf THEN DO:

            FIND FIRST nfe-dfe NO-LOCK
                WHERE nfe-dfe.chave-acesso = nfe-cte-inf.chave-acesso NO-ERROR.
            IF AVAIL nfe-dfe THEN DO:

                FIND FIRST emitente NO-LOCK
                    WHERE emitente.nome-abrev = nfe-dfe.nome-abrev NO-ERROR.

                IF AVAIL emitente THEN DO:

                    CASE emitente.cod-emitente:
                        WHEN 8801  THEN DO:

                            ASSIGN nfe-cte-inf.tipo-entrada = 2
                                   nfe-cte-inf.desp-it-codigo = "99050038"
                                   nfe-cte-inf.nat-operacao   = "1352IR".

                        END.
                        WHEN 26516 THEN DO:
                            ASSIGN nfe-cte-inf.tipo-entrada = 2
                                   nfe-cte-inf.desp-it-codigo = "99050038"
                                   nfe-cte-inf.nat-operacao   = "1352TV".

                        END.
                        WHEN 20473 THEN DO:
                            ASSIGN nfe-cte-inf.tipo-entrada = 2
                                   nfe-cte-inf.desp-it-codigo = "99050015"
                                   nfe-cte-inf.nat-operacao   = "1352TV".

                        END.
                        WHEN 13106 THEN DO:
                            ASSIGN nfe-cte-inf.tipo-entrada = 2
                                   nfe-cte-inf.desp-it-codigo = "99050038"
                                   nfe-cte-inf.nat-operacao   = "1352TV".
                        END.
                        WHEN 9867  THEN DO:
                            ASSIGN nfe-cte-inf.tipo-entrada = 2
                                   nfe-cte-inf.desp-it-codigo = "99050038"
                                   nfe-cte-inf.nat-operacao   = "1352TV".
                        END.
                    END CASE.

                END.

            END.

        END.
        
    END.
    
    RETURN "OK".

END.





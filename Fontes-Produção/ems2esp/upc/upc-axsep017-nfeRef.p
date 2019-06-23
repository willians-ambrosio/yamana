/*******************************************************************************

 UPC para o programa -> axsep017 (Envio NF-e)
 
 Objetivo: Manipular informacoes geradas no XML das Notas Fiscais Referenciadas.
                       
*******************************************************************************/

{cdp/cdcfgdis.i}
{include/i-epc200.i1}


DEFINE INPUT        PARAM p-ind-event AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAM TABLE FOR tt-epc.

DEFINE VARIABLE h-ttNFRef    AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE hQueryBuffer AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE h-query      AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE h-serie      AS WIDGET-HANDLE NO-UNDO.


IF  p-ind-event = "AtualizaDadosNFe":U THEN
    FOR FIRST tt-epc NO-LOCK
        WHERE tt-epc.cod-event     = "AtualizaDadosNFe"
          AND tt-epc.cod-parameter = "ttNFRef" :  /* Tabela ttNFRef - Notas Fiscais Referenciadas */

        ASSIGN h-ttNFRef    = WIDGET-HANDLE(tt-epc.val-parameter)
               hQueryBuffer = h-ttNFRef:DEFAULT-BUFFER-HANDLE.

        IF  VALID-HANDLE(hQueryBuffer) THEN DO:

            CREATE QUERY h-query.
                         h-query:SET-BUFFERS(hQueryBuffer).
                         h-query:QUERY-PREPARE('for each ' + h-ttNFRef:NAME + ' no-lock').
                         h-query:QUERY-OPEN().

            REPEAT ON ERROR UNDO, LEAVE:

                h-query:GET-NEXT() NO-ERROR.
                IF  h-query:QUERY-OFF-END THEN LEAVE.

                ASSIGN h-serie = hQueryBuffer:BUFFER-FIELD('serie') NO-ERROR.

                /* Troca serie 021 e 21 */
                IF  h-serie:BUFFER-VALUE = "021" THEN
                    ASSIGN h-serie:BUFFER-VALUE = "21".

            END.
        END.
    END.

RETURN "OK":U.


/*----------------------------------------- FIM upc -----------------------------------------*/

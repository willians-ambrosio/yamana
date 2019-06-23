/**============================================================**
** Altera‡Æo...: 
** Empresa.....: Cleilton / DSC
** Data........: 04/02/2015
** Objetivo....: Para os casos de atribui‡Æo de natureza no recebimento for‡ar
**                atribui‡Æo das aliquotas e tributa‡äes
** ............:  
**=============================================================**/
{utp/ut-glob.i}
{include/i-prgvrs.i upc-boin176-u00 11.5.11.000}

{include/i-epc200.i boin176}
    

DEFINE INPUT PARAM  p-ind-event AS  CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAM TABLE FOR tt-epc.      

DEFINE BUFFER bdocum-est FOR docum-est.
DEFINE VARIABLE hBoin176    AS HANDLE       NO-UNDO.
DEFINE VARIABLE rDocum-est  AS ROWID        NO-UNDO.

/*Begins 10/05/2018 - Para evitar o bloqueio de saldo nos contratos de medi‡Æo dever  liberar o saldo do contrato de medi‡Æo neste ponto */
IF p-ind-event = "validateRecord" THEN
DO:
    FOR FIRST tt-epc
        WHERE tt-epc.cod-event     = p-ind-event
          AND tt-epc.cod-parameter = "Object-Handle" :
    	ASSIGN hBoin176 = HANDLE(tt-epc.val-parameter).

        RUN getRowidDocumEst IN hBoin176 (OUTPUT rDocum-est).

        FIND FIRST docum-est  WHERE 
                   ROWID(docum-est) = rDocum-est NO-LOCK NO-ERROR.
        IF AVAIL docum-est THEN
        DO:      
            FOR EACH item-doc-est OF docum-est NO-LOCK:
                               
                FIND FIRST ordem-compra WHERE
                           ordem-compra.numero-ordem = item-doc-est.numero-ordem NO-LOCK NO-ERROR.
                IF AVAIL ordem-compra THEN
                   FIND FIRST contrato-for where
                              contrato-for.nr-contrato = ordem-compra.nr-contrato NO-LOCK NO-ERROR.
                IF AVAIL contrato-for THEN
                DO:       

                    if contrato-for.ind-control-rec = 2 then
                    do:         
                        find item-contrat
                            where item-contrat.num-seq-item = ordem-compra.num-seq-item
                              and item-contrat.nr-contrato  = ordem-compra.nr-contrato
                            no-lock no-error.
                    end.
            
                    if contrato-for.ind-control-rec = 2 and avail item-contrat then
                    do:
                        for each medicao-contrat use-index dat-prev
                           where medicao-contrat.numero-ordem  = ordem-compra.numero-ordem
                             and medicao-contrat.ind-sit-medicao = 2
                             and medicao-contrat.dat-prev-medicao <= docum-est.dt-emissao
                             and medicao-contrat.log-rec-medicao = NO EXCLUSIVE-LOCK:
            
                            ASSIGN medicao-contrat.val-sdo-aloc-med = medicao-contrat.val-sdo-aloc-med - item-doc-est.preco-total[1].
                        END.
                    END.
                END.
            END.
        END.
    END.
END.
/*end 10/05/2018 - Para evitar o bloqueio de saldo nos contratos de medi‡Æo dever  liberar o saldo do contrato de medi‡Æo neste ponto */

IF p-ind-event = "lDefaultAliquotaNO" THEN DO:
    FOR FIRST tt-epc
        WHERE tt-epc.cod-event     = p-ind-event
          AND tt-epc.cod-parameter = "Object-Handle" :
    	ASSIGN hBoin176 = HANDLE(tt-epc.val-parameter).

        RUN getRowidDocumEst IN hBoin176 (OUTPUT rDocum-est).

        FIND FIRST bdocum-est
            WHERE ROWID(bdocum-est) = rDocum-est NO-LOCK NO-ERROR.
        IF AVAIL bdocum-est THEN DO:
            /* Se encontrar atribui‡Æo for‡a os defaults de imposto */
            IF CAN-FIND(FIRST es-item-doc-est-natoper
                        WHERE es-item-doc-est-natoper.ep-codigo    = i-ep-codigo-usuario
                          AND es-item-doc-est-natoper.serie-docto  = bdocum-est.serie-docto   
                          AND es-item-doc-est-natoper.nro-docto    = bdocum-est.nro-docto     
                          AND es-item-doc-est-natoper.cod-emitente = bdocum-est.cod-emitente  
                          AND es-item-doc-est-natoper.nat-operacao = bdocum-est.nat-operacao NO-LOCK) THEN DO:
                RUN setDefaultCalculoImpostos IN hBoin176 (INPUT YES,  /*** c¢digo de tributa‡Æo ***/
                                                           INPUT YES).  /*** al¡quota             ***/
            END.
        END. /* AVAIL bdocum-est */
    END. /* FIRST tt-epc */
END. /*  p-ind-event = "lDefaultAliquotaNO"  */
RETURN "OK".

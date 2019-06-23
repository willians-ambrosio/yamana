/*====================================================================================
**    Programa: escnapi001
**    Objetivo: Informar saldo do contrato
**       Autor: Bruno Bertulli (DSC)
**        Data: 09/04/2014
**                 
**=====================================================================================*/

DEFINE INPUT  PARAM p-Contrato LIKE contrato-for.nr-contrato NO-UNDO.
DEFINE OUTPUT PARAM p-Saldo-Contrato AS DECIMAL NO-UNDO.

DEFINE VARIABLE de-saldo-contrato       AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de-saldo-it-contrato    AS DECIMAL     NO-UNDO.

FIND FIRST contrato-for NO-LOCK
    WHERE contrato-for.nr-contrato = p-Contrato NO-ERROR.
  
ASSIGN de-saldo-contrato    = 0
       de-saldo-it-contrato = 0.

IF AVAILABLE contrato-for THEN DO:
    FOR EACH ITEM-contrat
        WHERE item-contrat.nr-contrato = contrato-for.nr-contrato 
        AND   item-contrat.ind-caract-item = 1 NO-LOCK:
        ASSIGN de-saldo-it-contrato = de-saldo-it-contrato + item-contrat.sld-val.
    END.

    if avail contrato-for and contrato-for.dec-2 <> 0 THEN
        assign de-saldo-contrato = contrato-for.dec-2 - de-saldo-it-contrato.
    ELSE
        ASSIGN de-saldo-contrato = 0.
END.

ASSIGN p-Saldo-Contrato = de-saldo-contrato.

RETURN "OK":U.


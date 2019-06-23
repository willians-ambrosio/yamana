{include/i-epc200.i CN9174}
DEF INPUT PARAM p-ind-event AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

def var  i-ordem       like ordem-compra.numero-ordem  no-undo.
def var  i-ponto       as int                          no-undo.
def var  i-evento      as int                          no-undo.
def var  de-val-aux    as decimal                      no-undo.
def var  da-dt-nota    as date                         no-undo.
def var  de-val-erro   as decimal                      no-undo.
def var  l-erro-co     as logical                      no-undo.
def var  l-contrato    as logical                      no-undo.
def var  i-total       as int                          no-undo.
DEF VAR  raw-tt-erro   AS RAW                          NO-UNDO.

DEF TEMP-TABLE tt-erro NO-UNDO
    FIELD i-sequen AS INT             
    FIELD cd-erro  AS INT
    FIELD mensagem AS CHAR FORMAT "x(255)".

FIND FIRST tt-epc
     WHERE tt-epc.cod-event = "Tratamento-Contrato-cn9174" NO-LOCK NO-ERROR.
IF AVAIL tt-epc THEN DO:
    FOR EACH tt-epc
       WHERE tt-epc.cod-event = "Tratamento-Contrato-cn9174":
        IF tt-epc.cod-parameter = "i-ordem":U THEN
            ASSIGN i-ordem = INT(tt-epc.val-parameter).

        IF tt-epc.cod-parameter = "i-ponto":U THEN
            ASSIGN i-ponto = INT(tt-epc.val-parameter).

        IF tt-epc.cod-parameter = "i-evento":U THEN
            ASSIGN i-evento = INT(tt-epc.val-parameter).

        IF tt-epc.cod-parameter = "de-val-aux":U THEN
            ASSIGN de-val-aux = DECIMAL(tt-epc.val-parameter).

        IF tt-epc.cod-parameter = "da-dt-nota":U THEN
            ASSIGN da-dt-nota = IF tt-epc.val-parameter = "?" THEN ? ELSE DATE(tt-epc.val-parameter).
    END.

    FOR EACH tt-erro:
        DELETE tt-erro.
    END.

    RUN esp/cnp/espcn9174.p ( i-ordem,
                              i-ponto,
                              i-evento,
                              de-val-aux,
                              da-dt-nota,
                              OUTPUT de-val-erro,
                              OUTPUT l-erro-co,
                              OUTPUT l-contrato,
                              OUTPUT i-total,
                              INPUT-OUTPUT table tt-erro).

    FOR EACH tt-epc 
       WHERE tt-epc.cod-event = "Tratamento-Contrato-cn9174-retorno":
        DELETE tt-epc.
    END.

    IF CAN-FIND (FIRST tt-erro) THEN DO:
        FOR EACH tt-erro:
            RAW-TRANSFER tt-erro TO raw-tt-erro NO-ERROR.
            CREATE tt-epc.
            ASSIGN tt-epc.cod-event     = "Tratamento-Contrato-cn9174-retorno":U
                   tt-epc.cod-parameter = "Error":U
                   tt-epc.val-parameter = STRING(raw-tt-erro).
        END.
    END.
    ELSE DO:
        CREATE tt-epc.
        ASSIGN tt-epc.cod-event     = "Tratamento-Contrato-cn9174-retorno":U
               tt-epc.cod-parameter = "de-val-erro":U
               tt-epc.val-parameter = STRING(de-val-erro).
        CREATE tt-epc.
        ASSIGN tt-epc.cod-event     = "Tratamento-Contrato-cn9174-retorno":U
               tt-epc.cod-parameter = "l-erro-co":U
               tt-epc.val-parameter = STRING(l-erro-co).
        CREATE tt-epc.
        ASSIGN tt-epc.cod-event     = "Tratamento-Contrato-cn9174-retorno":U
               tt-epc.cod-parameter = "l-contrato":U
               tt-epc.val-parameter = STRING(l-contrato).
        CREATE tt-epc.
        ASSIGN tt-epc.cod-event     = "Tratamento-Contrato-cn9174-retorno":U
               tt-epc.cod-parameter = "i-total":U
               tt-epc.val-parameter = STRING(i-total).
    END.
END.

RETURN "OK":U.

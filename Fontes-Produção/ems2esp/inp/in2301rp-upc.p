/****************************************************************************
** Programa: IN2301RP-UPC
** Autor...: Juliana K. Oliveira
** Data....: 12/06/2009
****************************************************************************/
{include/i-prgvrs.i IN2301RP-UPC  1.00.00.000} 
{include/i-epc200.i1}
{esp/esapi002.i}
{cdp/cd0669.i}

/** PARAMETROS **/
DEFINE INPUT        PARAMETER p-ind-event AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-epc.

/** VARIAVEIS **/
DEFINE VARIABLE wgh-param     AS WIDGET-HANDLE      NO-UNDO.
DEFINE VARIABLE wgh-qry-param AS WIDGET-HANDLE      NO-UNDO.
DEFINE VARIABLE i-seq         AS INTEGER            NO-UNDO.
DEFINE VARIABLE h-buffer      AS HANDLE             NO-UNDO.
DEFINE VARIABLE h-destino     AS HANDLE             NO-UNDO.
DEFINE VARIABLE h-arquivo     AS HANDLE             NO-UNDO.
DEFINE VARIABLE h-usuario     AS HANDLE             NO-UNDO.
DEFINE VARIABLE h-data-exec   AS HANDLE             NO-UNDO.
DEFINE VARIABLE h-hora-exec   AS HANDLE             NO-UNDO.
DEFINE VARIABLE h-da-ini      AS HANDLE             NO-UNDO.
DEFINE VARIABLE h-da-fim      AS HANDLE             NO-UNDO.
DEFINE VARIABLE h-ordens      AS HANDLE             NO-UNDO.
DEFINE VARIABLE h-requis      AS HANDLE             NO-UNDO.

DEFINE BUFFER bf-param FOR tt-param.

IF p-ind-event = "fim-in2301":U THEN DO:
    FIND FIRST tt-epc
        WHERE tt-epc.cod-event = p-ind-event NO-ERROR.
    IF AVAIL tt-epc THEN DO:
        ASSIGN wgh-param = WIDGET-HANDLE(tt-epc.val-parameter).
    END.
    
    CREATE bf-param.

    CREATE QUERY wgh-qry-param.
    wgh-qry-param:SET-BUFFERS(wgh-param).
    wgh-qry-param:QUERY-PREPARE("for each tt-param").
    wgh-qry-param:QUERY-OPEN.
    wgh-qry-param:GET-NEXT().

    DO  WHILE NOT wgh-qry-param:QUERY-OFF-END:

        DO  i-seq = 1 TO wgh-qry-param:NUM-BUFFERS:
            ASSIGN h-buffer = wgh-qry-param:GET-BUFFER-HANDLE(i-seq).
    
            h-destino = h-buffer:BUFFER-FIELD("destino").
            ASSIGN bf-param.destino = h-destino:BUFFER-VALUE.
    
            h-arquivo  = h-buffer:BUFFER-FIELD("arquivo").
            ASSIGN bf-param.arquivo = h-arquivo:BUFFER-VALUE.
    
            h-usuario  = h-buffer:BUFFER-FIELD("usuario").
            ASSIGN bf-param.usuario = h-usuario:BUFFER-VALUE.
    
            h-data-exec  = h-buffer:BUFFER-FIELD("data-exec").
            ASSIGN bf-param.data-exec = h-data-exec:BUFFER-VALUE.
    
            h-hora-exec  = h-buffer:BUFFER-FIELD("hora-exec").
            ASSIGN bf-param.hora-exec = h-hora-exec:BUFFER-VALUE.

            h-da-ini = h-buffer:BUFFER-FIELD("da-ini").
            ASSIGN bf-param.da-ini = h-da-ini:BUFFER-VALUE.

            h-da-fim = h-buffer:BUFFER-FIELD("da-fim").
            ASSIGN bf-param.da-fim = h-da-fim:BUFFER-VALUE.

            h-ordens = h-buffer:BUFFER-FIELD("l-ordens").
            ASSIGN bf-param.l-ordens = h-ordens:BUFFER-VALUE.

            h-requis = h-buffer:BUFFER-FIELD("l-requisicoes").
            ASSIGN bf-param.l-requisicoes = h-requis:BUFFER-VALUE.
        END.

        wgh-qry-param:GET-NEXT().
    END.

    IF NUM-ENTRIES(bf-param.arquivo, ".") = 2 THEN
        ASSIGN bf-param.arquivo = ENTRY(1,bf-param.arquivo, ".") + "-frotas."
                                + ENTRY(2,bf-param.arquivo, ".").
    ELSE 
        ASSIGN bf-param.arquivo = ENTRY(1,bf-param.arquivo, ".") + "." 
                                + ENTRY(2,bf-param.arquivo, ".") + "-frotas."
                                + ENTRY(3,bf-param.arquivo, ".").
        
    IF bf-param.l-ordens THEN DO:
        RUN esp/esapi002.p (INPUT  5,
                            INPUT  0,
                            INPUT  0,
                            INPUT  TABLE bf-param,
                            OUTPUT TABLE tt-erro).
    END.
END.


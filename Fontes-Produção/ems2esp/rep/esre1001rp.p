/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESRE1001RP 2.00.00.002 } /*** 010002 ***/

&GLOBAL-DEFINE RTF NO
&SCOPED-DEFINE pagesize 43   

DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino          AS INTEGER 
    FIELD arquivo          AS CHARACTER FORMAT "x(35)":U
    FIELD usuario          AS CHARACTER FORMAT "x(12)":U
    FIELD data-exec        AS DATE
    FIELD hora-exec        AS INTEGER
    FIELD modelo           AS CHARACTER FORMAT "x(35)":U
    FIELD l-habilitaRtf    AS LOGICAL
    FIELD estab-ini        LIKE movto-estoq.cod-estabel
    FIELD estab-fim        LIKE movto-estoq.cod-estabel
    FIELD dt-trans         LIKE movto-estoq.dt-trans.
    
DEFINE TEMP-TABLE tt-raw-digita NO-UNDO
    FIELD raw-digita       AS RAW.

DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
DEFINE VARIABLE h-acomp         AS HANDLE                      NO-UNDO.
DEFINE VARIABLE da-param        AS DATE FORMAT "99/99/9999"    NO-UNDO.
DEFINE VARIABLE da-data         AS DATE                        NO-UNDO.
DEFINE VARIABLE da-iniper-x-1   AS DATE FORMAT "99/99/9999"    NO-UNDO.
DEFINE VARIABLE da-fimper-x-1   AS DATE FORMAT "99/99/9999"    NO-UNDO.
DEFINE VARIABLE i-per-corrente  AS INTEGER                     NO-UNDO.
DEFINE VARIABLE i-ano-corrente  AS INTEGER                     NO-UNDO.
DEFINE VARIABLE da-iniper-fech  LIKE param-estoq.ult-fech-dia  NO-UNDO.
DEFINE VARIABLE da-fimper-fech  LIKE param-estoq.ult-fech-dia  NO-UNDO.
DEFINE VARIABLE da-anterior     AS DATE FORMAT "99/99/9999"    NO-UNDO.
DEFINE VARIABLE da-atual        AS DATE FORMAT "99/99/9999"    NO-UNDO.

FORM 
    docum-est.cod-emitente 
    docum-est.serie-docto  AT 12
    docum-est.nro-docto    AT 19 
    docum-est.nat-operacao AT 37
    da-anterior            AT 47 LABEL "Data Anterior"
    da-atual               AT 62 LABEL "Data Atual   "   
    WITH DOWN OVERLAY ROW 3 COLUMN 1 STREAM-IO FRAME f-est.

{include/i-rpout.i}
{include/i-rpcab.i}

ASSIGN c-programa       = "ESRE1001RP"
       c-versao       = "1.00"
       c-revisao      = ".00.000"
       c-empresa      = ""
       c-titulo-relat = "Atualiza‡Æo Data Documentos".

IF tt-param.destino <> 4 THEN DO:
    VIEW FRAME f-cabec.
    VIEW FRAME f-rodape.
END.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Atualizando Documentos *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

FIND FIRST param-estoq NO-LOCK NO-ERROR.

IF param-estoq.tp-fech = 1 THEN DO:
   RUN cdp/cdapi005.p (INPUT param-estoq.ult-per-fech,
                       OUTPUT da-iniper-x-1,
                       OUTPUT da-fimper-x-1,
                       OUTPUT i-per-corrente,
                       OUTPUT i-ano-corrente,
                       OUTPUT da-iniper-fech,
                       OUTPUT da-fimper-fech).
   ASSIGN da-param = da-fimper-x-1.
END.

FOR EACH estab-mat
    WHERE estab-mat.cod-estabel >= tt-param.estab-ini
    AND   estab-mat.cod-estabel <= tt-param.estab-fim NO-LOCK:

    IF param-estoq.tp-fech = 2 THEN DO:
        RUN cdp/cdapi005.p (INPUT estab-mat.ult-per-fech,
                            OUTPUT da-iniper-x-1,
                            OUTPUT da-fimper-x-1,
                            OUTPUT i-per-corrente,
                            OUTPUT i-ano-corrente,
                            OUTPUT da-iniper-fech,
                            OUTPUT da-fimper-fech).
        ASSIGN da-param = da-fimper-x-1.
    END.

    FOR EACH docum-est 
        WHERE docum-est.ce-atual     = NO
        AND   docum-est.dt-trans    <= da-param
        AND   docum-est.cod-estabel  = estab-mat.cod-estabel EXCLUSIVE-LOCK:

        IF SUBSTRING(docum-est.nat-operacao,1,1) <> "3" THEN NEXT.

        IF NOT CAN-FIND(FIRST emitente
                        WHERE emitente.cod-emitente = docum-est.cod-emitente
                        AND   emitente.natureza     > 2) THEN NEXT.
    
        RUN pi-acompanhar IN h-acomp (INPUT docum-est.serie + "/" + docum-est.nro-docto).

        ASSIGN da-anterior        = docum-est.dt-trans
               docum-est.dt-trans = tt-param.dt-trans.

        DISPLAY docum-est.cod-emitente 
                docum-est.serie-docto         AT 12
                docum-est.nro-docto           AT 19 
                docum-est.nat-operacao        AT 37
                da-anterior                   AT 47 
                docum-est.dt-trans @ da-atual AT 62 
            WITH FRAME f-est.
        DOWN WITH FRAME f-est. 
    END.
END.

{include/i-rpclo.i}

RUN pi-finalizar IN h-acomp.

RETURN "OK":U.

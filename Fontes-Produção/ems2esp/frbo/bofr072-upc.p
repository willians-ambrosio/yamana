/***************************************************************************************
** Programa: bofr072-upc.p
** Objetivo:
** Autor........: Ricardo Costa Sutil - Manufatura - Datasul S.A.
** Data.........: 02/2008
** 2.00.00.001..: 06/2009 - Gustavo Eduardo Tamanini - FO 2006.829
****************************************************************************************/
{include/i-prgvrs.i BOFR072-UPC 2.00.00.001}
{include/i-epc200.i}
{cdp/cd0669.i}
{esp/esapi002.i}

/********************************* WIDGET-HANDLE **************************************/
def new global shared var wh-combo-box-mv0301    AS WIDGET-HANDLE  NO-UNDO.
def new global shared var wh-nr-ord-produ-mv0301 AS WIDGET-HANDLE  NO-UNDO.
def new global shared var wh-mv0301-ordem-inv    AS WIDGET-HANDLE  NO-UNDO. 

DEF VAR h-bo        AS HANDLE         NO-UNDO.
DEF VAR i-acao      AS INTEGER        NO-UNDO.
DEF VAR i-cd-tipo   AS INTEGER        NO-UNDO.
DEF VAR dt-dat-entr AS DATE           NO-UNDO.

/** TT para parƒmetros **/
DEF INPUT        PARAMETER p-ind-event AS CHARACTER   NO-UNDO.
DEF INPUT-OUTPUT PARAMETER TABLE FOR  tt-epc.

DEF VAR i          AS INT INITIAL 1  NO-UNDO.
DEF VAR j          AS INT INITIAL 1  NO-UNDO.
DEF VAR l-continua AS LOG INITIAL NO NO-UNDO.

/* Logica para descobrir se a UPC foi chamada pelo programa MV0301 */
DO WHILE PROGRAM-NAME(i) <> ?:
    DO WHILE j < (LENGTH(PROGRAM-NAME(i)) - 6):
        IF SUBSTRING(PROGRAM-NAME(i), j, 6) = "MV0301":U THEN
            ASSIGN l-continua = YES.
        j = j + 1.
    END.
    j = 1.
    i = i + 1.
END.
/* Se o programa foi chamado pelo MV0301, continua a execucÆo da UPC. */

/** Efetua logica somente se programa foi chamado pelo MV0301 **/
IF l-continua THEN DO:    
    FIND FIRST tt-epc 
         WHERE tt-epc.cod-event     = p-ind-event
           AND tt-epc.cod-parameter = "OBJECT-HANDLE" NO-LOCK NO-ERROR.
    IF  AVAIL tt-epc THEN
        ASSIGN h-bo = WIDGET-HANDLE(tt-epc.val-parameter).

    IF  p-ind-event = "beforeUpdateRecord" OR 
        p-ind-event = "beforeCreateRecord" THEN DO:
        
        /** NÆo permite desvincular uma Ordem de Invenstimento ou
            Alterar com o pre‡o m‚dio vigente calculado. **/
        IF p-ind-event = "beforeUpdateRecord" AND VALID-HANDLE(wh-mv0301-ordem-inv) THEN DO:
            FOR FIRST mmv-ord-status
                WHERE mmv-ord-status.nr-ord-produ = INT(wh-nr-ord-produ-mv0301:SCREEN-VALUE) NO-LOCK:
            END.
            IF AVAIL mmv-ord-status THEN DO:
                IF INT(mmv-ord-status.num-livre-1) <> 0 AND INT(wh-mv0301-ordem-inv:SCREEN-VALUE) = 0 THEN DO:
                    {utp/ut-liter.i NÆo_‚_poss¡vel_desvincular_Ordem_de_Investimento_da_Ordem_de_Manuten‡Æo.}
                    RUN pi-cria-erro (INPUT RETURN-VALUE).
                END.
                ELSE DO:
                    IF INT(mmv-ord-status.num-livre-1) <> INT(wh-mv0301-ordem-inv:SCREEN-VALUE) THEN DO:
                        /** So permite alterar a OM Invest. se o pre‡o medio vigente nao estiver calculado. **/
                        FOR FIRST param-estoq NO-LOCK:
                            /** Busca Data Entrada da OM**/
                            RUN getDateField IN h-bo (INPUT "dat-entr":U, OUTPUT dt-dat-entr).
                            /** Verifica se OM esta dentro do ultimo preco medio calculado. **/
                            IF dt-dat-entr <= param-estoq.mensal-ate THEN DO:
                                {utp/ut-liter.i Ordem_Investimento_nÆo_pode_ser_alterada,_pois_o_pre‡o_m‚dio_para_o_periodo_vigente_j _est _calculado.}
                                RUN pi-cria-erro (INPUT RETURN-VALUE).
                            END.
                        END.
                    END.
                END.
            END.
        END.
    
        RUN esp/esapi002.p (INPUT  4 /* Valida */ ,
                            INPUT  INT(wh-nr-ord-produ-mv0301:SCREEN-VALUE),
                            INPUT  INT(wh-mv0301-ordem-inv:SCREEN-VALUE),
                            INPUT  TABLE tt-param,
                            OUTPUT TABLE tt-erro).
    
        IF RETURN-VALUE = "NOK":U THEN DO:
            FOR EACH tt-erro:
                RUN pi-cria-erro (INPUT tt-erro.mensagem).
            END.
        END.
    
        RUN getIntField IN h-bo (INPUT "cd-tipo":U, OUTPUT i-cd-tipo).
        /** Verifica em tabela especifica o parametro
            "Obrigada Informar OM Investimento" **/
        FOR FIRST mmv-tipo-manut
            WHERE mmv-tipo-manut.tipo-manut = i-cd-tipo NO-LOCK:
        END.
        IF AVAIL mmv-tipo-manut THEN DO:
            /** Apresenta erro se ‚ obrigado a informar ordem invest e a mesma esta em branco **/
            IF mmv-tipo-manut.log-livre-1 AND INT(wh-mv0301-ordem-inv:SCREEN-VALUE) = 0 THEN DO:
                {utp/ut-liter.i Ordem_de_Investimento_nÆo_informada.}
                RUN pi-cria-erro (INPUT RETURN-VALUE).
            end.
        end.
    end.

    IF  p-ind-event = "AfterUpdateRecord" OR 
        p-ind-event = "afterCreateRecord" THEN DO:
        IF VALID-HANDLE(wh-combo-box-mv0301) THEN DO:
            FOR FIRST mmv-ord-status
                WHERE mmv-ord-status.nr-ord-produ = INT(wh-nr-ord-produ-mv0301:SCREEN-VALUE) EXCLUSIVE-LOCK:
            END.
            IF NOT AVAIL mmv-ord-status THEN DO:
                  CREATE mmv-ord-status.
                  ASSIGN mmv-ord-status.nr-ord-produ = INT(wh-nr-ord-produ-mv0301:SCREEN-VALUE).
            END.
            IF {ydminc/i00ydm001.i 06 wh-combo-box-mv0301:SCREEN-VALUE} <> mmv-ord-status.idi-status-ord THEN DO:
                ASSIGN mmv-ord-status.idi-status-ord = {ydminc/i00ydm001.i 06 wh-combo-box-mv0301:SCREEN-VALUE}  
                       mmv-ord-status.data-sit       = TODAY 
                       mmv-ord-status.hora-sit       = REPLACE(STRING(TIME,"HH:MM:SS"),":","").
            END.
            ASSIGN mmv-ord-status.num-livre-1 = INT(wh-mv0301-ordem-inv:SCREEN-VALUE).
        END.
    
        RUN esp/esapi002.p (INPUT  IF p-ind-event = "AfterUpdateRecord" THEN 2 ELSE 1,
                            INPUT  mmv-ord-status.nr-ord-produ,
                            INPUT  INT(wh-mv0301-ordem-inv:SCREEN-VALUE),
                            INPUT  TABLE tt-param,
                            OUTPUT TABLE tt-erro).
    END.
    
    IF p-ind-event = "AfterDeleteRecord" THEN DO:
        RUN esp/esapi002.p (INPUT  3,
                            INPUT  INT(wh-nr-ord-produ-mv0301:SCREEN-VALUE),
                            INPUT  INT(wh-mv0301-ordem-inv:SCREEN-VALUE),
                            INPUT  TABLE tt-param,
                            OUTPUT TABLE tt-erro).

        FOR FIRST  mmv-ord-status
            WHERE  mmv-ord-status.nr-ord-produ = INT(wh-nr-ord-produ-mv0301:SCREEN-VALUE) EXCLUSIVE-LOCK.
            DELETE mmv-ord-status.
        END.
    end.
end.

procedure pi-cria-erro :
    def input param msg as char no-undo.

    create tt-epc.
    assign tt-epc.cod-event     = "ERROR"
           tt-epc.cod-parameter = "EPC-ERROR" 
           tt-epc.val-parameter = msg.
end.

/************************* PARMETROS PADRAO **********************************/
DEFINE INPUT PARAMETER p-ind-event   AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object  AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object  AS HANDLE         NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame   AS WIDGET-HANDLE  NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table   AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-row-table   AS ROWID          NO-UNDO.

{cdp/cdcfgmat.i}

/**************************Variaveis Local ***********************************/
DEFINE VARIABLE h_Frame                         AS WIDGET-HANDLE   NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-bt-confirma    AS WIDGET-HANDLE   NO-UNDO.  
DEFINE NEW GLOBAL SHARED VAR wgh-bt-confirma-2  AS WIDGET-HANDLE   NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-br-table       AS WIDGET-HANDLE   NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-cn0402           AS HANDLE          NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR p-hBrw             AS WIDGET-HANDLE   NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-contrato       AS WIDGET-HANDLE   NO-UNDO.

DEFINE VARIABLE h-esapi001  AS   HANDLE        NO-UNDO.

def  temp-table tt-erro no-undo
    field i-sequen as int             
    field cd-erro  as int
    field mensagem as char format "x(255)"
    field c-param  as char.

DEFINE TEMP-TABLE ttDadosBrw NO-UNDO
    FIELD seq-item      AS INTEGER 
    FIELD it-codigo     LIKE item.it-codigo 
    FIELD ativo         AS LOGICAL
    FIELD alter-origem  AS DECIMAL
    FIELD alter-destino AS DECIMAL
    FIELD ativo-origem  AS CHARACTER
    FIELD ativo-destino AS CHARACTER.

/**************************Inicio Programa************************************/
IF p-ind-event             = "DISPLAY" 
    AND p-ind-object       = "VIEWER"
    AND p-wgh-object:NAME  = "cnp/cn0402-v01.w" THEN DO:

    ASSIGN h_Frame = p-wgh-frame:FIRST-CHILD.
    ASSIGN h_Frame = h_Frame:FIRST-CHILD.
    
    DO WHILE h_Frame <> ? :
        IF h_frame:TYPE <> "field-group" THEN DO:
            CASE h_Frame:NAME:
                WHEN "nr-contrato" THEN
                   ASSIGN wgh-contrato = h_Frame.
            END CASE.
            ASSIGN h_Frame = h_Frame:NEXT-SIBLING.
        END.
    END.
END.

IF  p-ind-event            = "INITIALIZE"
    AND p-ind-object       = "BROWSER"
    AND p-wgh-object:NAME  = "cnp/cn0402-b01.w" THEN DO:
    
    RUN upc/cn0402-upc.p PERSISTENT SET h-cn0402 (INPUT "",
                                                  INPUT "",
                                                  INPUT p-wgh-object,
                                                  INPUT p-wgh-frame,
                                                  INPUT "",
                                                  INPUT p-row-table).
    
    ASSIGN h_Frame = p-wgh-frame:FIRST-CHILD.
    ASSIGN h_Frame = h_Frame:FIRST-CHILD.
    
    DO WHILE h_Frame <> ? :
        IF h_frame:TYPE <> "field-group" THEN DO:
            CASE h_Frame:NAME:
               WHEN "bt-confirma" THEN
                    ASSIGN wgh-bt-confirma = h_Frame.
               WHEN "br_table" THEN 
                   ASSIGN p-hBrw = h_Frame.
            END CASE.
            ASSIGN h_Frame = h_Frame:NEXT-SIBLING.
        END.
    END.
END.

IF p-ind-event  = "AFTER-OPEN-QUERY" AND 
    p-ind-object = "BROWSER"         AND 
    p-wgh-object:NAME = "cnp/cn0402-b01.w" THEN DO:
    
    IF VALID-HANDLE(wgh-bt-confirma)
        AND NOT VALID-HANDLE(wgh-bt-confirma-2) THEN DO:

        CREATE BUTTON wgh-bt-confirma-2 NO-ERROR
            ASSIGN HEIGHT    = wgh-bt-confirma:HEIGHT   
                   WIDTH     = wgh-bt-confirma:WIDTH     
                   ROW       = wgh-bt-confirma:ROW  
                   COL       = wgh-bt-confirma:COL      
                   FRAME     = wgh-bt-confirma:FRAME    
                   LABEL     = wgh-bt-confirma:LABEL    
                   SENSITIVE = wgh-bt-confirma:SENSITIVE
                   VISIBLE   = wgh-bt-confirma:VISIBLE
            TRIGGERS:
                 ON CHOOSE PERSISTENT RUN pi-browse IN h-cn0402.
            END TRIGGERS.
    END.
END.

IF VALID-HANDLE(wgh-bt-confirma-2) THEN 
    ASSIGN wgh-bt-confirma-2:SENSITIVE = wgh-bt-confirma:SENSITIVE.


PROCEDURE pi-browse:

    IF VALID-HANDLE(p-hBrw) THEN DO:
        DEFINE VARIABLE i-linha     AS INTEGER INIT 0 NO-UNDO.
        DEFINE VARIABLE l-status    AS LOGICAL INIT NO NO-UNDO.
    
        EMPTY TEMP-TABLE ttDadosBrw.
        p-hBrw:SELECT-ROW(1) NO-ERROR.
        ASSIGN i-linha = 1.

        REPEAT:

            IF  p-hBrw:GET-BROWSE-COLUMN(1):SCREEN-VALUE = "*"            AND 
               (p-hBrw:GET-BROWSE-COLUMN(6):SCREEN-VALUE = "Limite Valor" OR
                p-hBrw:GET-BROWSE-COLUMN(6):SCREEN-VALUE = "Pre‡o Total") THEN DO:
                
                CREATE ttDadosBrw.
                ASSIGN ttDadosBrw.ativo           = NO
                       ttDadosBrw.seq-item        = INT(p-hBrw:GET-BROWSE-COLUMN(2):SCREEN-VALUE)
                       ttDadosBrw.it-codigo       = p-hBrw:GET-BROWSE-COLUMN(3):SCREEN-VALUE
                       ttDadosBrw.alter-origem    = DEC(p-hBrw:GET-BROWSE-COLUMN(8):SCREEN-VALUE)
                       ttDadosBrw.alter-destino   = DEC(p-hBrw:GET-BROWSE-COLUMN(7):SCREEN-VALUE).

            END.

            IF  p-hBrw:GET-BROWSE-COLUMN(1):SCREEN-VALUE = "*"            AND 
                p-hBrw:GET-BROWSE-COLUMN(6):SCREEN-VALUE  MATCHES("Ativo - Situa*") THEN DO:
                
                CREATE ttDadosBrw.
                ASSIGN ttDadosBrw.ativo           = YES
                       ttDadosBrw.seq-item        = INT(p-hBrw:GET-BROWSE-COLUMN(2):SCREEN-VALUE)
                       ttDadosBrw.it-codigo       = p-hBrw:GET-BROWSE-COLUMN(3):SCREEN-VALUE
                       ttDadosBrw.ativo-origem    = p-hBrw:GET-BROWSE-COLUMN(7):SCREEN-VALUE
                       ttDadosBrw.ativo-destino   = p-hBrw:GET-BROWSE-COLUMN(8):SCREEN-VALUE.

            END.
                
            ASSIGN l-status = p-hBrw:SELECT-NEXT-ROW() NO-ERROR.
                
            IF l-status = NO THEN LEAVE.
            ASSIGN i-linha = i-linha + 1.
        END.
    END.

    RUN pi-valida-browse.
    RETURN "OK":U.
END PROCEDURE.

PROCEDURE pi-valida-browse:

    IF NOT CAN-FIND(FIRST ttDadosBrw) THEN DO:
        APPLY "CHOOSE" TO wgh-bt-confirma.
        RETURN "OK":U.
    END.

    run esp\esapi001.p persistent set h-esapi001.

    FOR EACH ttDadosBrw
       WHERE ttDadosBrw.seq-item = 0
         AND ttDadosBrw.ativo    = NO:

        FIND FIRST contrato-for-ext
             WHERE contrato-for-ext.nr-contrato = int(wgh-contrato:SCREEN-VALUE)
               AND contrato-for-ext.num-ord-inv <> 0 NO-LOCK NO-ERROR.

        IF NOT AVAIL contrato-for-ext THEN DO:
            APPLY "CHOOSE" TO wgh-bt-confirma.
            RETURN "OK":U.
        END.
    
        IF AVAIL contrato-for-ext THEN DO:

            FIND FIRST contrato-for
                 WHERE contrato-for.nr-contrato = contrato-for-ext.nr-contrato NO-LOCK NO-ERROR.

            IF NOT AVAIL contrato-for THEN DO:
                APPLY "CHOOSE" TO wgh-bt-confirma.
                RETURN "OK":U.
            END.

            FIND FIRST param-inv NO-LOCK 
                 WHERE param-inv.ep-codigo = contrato-for-ext.ep-codigo NO-ERROR.

            IF NOT AVAIL param-inv THEN DO:
                APPLY "CHOOSE" TO wgh-bt-confirma.
                RETURN "OK":U.
            END.
    
            RUN pi-atualiza-verba IN h-esapi001 (INPUT 1, /* Valida Verba */
                                                 INPUT contrato-for-ext.ep-codigo,
                                                 INPUT contrato-for-ext.num-ord-inv,
                                                 INPUT contrato-for.dt-contrato,
                                                 INPUT param-inv.moeda-inv,
                                                 INPUT (ttDadosBrw.alter-origem - ttDadosBrw.alter-destino),
                                                 INPUT 0,
                                                 OUTPUT TABLE tt-erro).

            IF CAN-FIND (FIRST tt-erro) THEN DO:
                RUN cdp/cd0669.w (INPUT TABLE tt-erro).
                RETURN "NOK":U.
            END.

            DO TRANS:

                APPLY "CHOOSE" TO wgh-bt-confirma.

                RUN pi-principal in h-esapi001 (INPUT 1,
                                                INPUT ROWID(contrato-for),
                                                INPUT 2,
                                                OUTPUT TABLE tt-erro).
    
                IF CAN-FIND (FIRST tt-erro) THEN DO:
                    RUN cdp/cd0669.w (INPUT TABLE tt-erro).
                    UNDO, RETURN "NOK":U.
                END.
            END.
        END.
    END.

    FOR EACH ttDadosBrw
       WHERE ttDadosBrw.seq-item <> 0
         AND ttDadosBrw.ativo    = NO:

        FIND FIRST item-contrat-ext 
             WHERE item-contrat-ext.nr-contrato  = int(wgh-contrato:SCREEN-VALUE)
               AND item-contrat-ext.num-seq-item = ttDadosBrw.seq-item
               AND item-contrat-ext.num-ord-inv  <> 0 NO-LOCK NO-ERROR.

        IF NOT AVAIL item-contrat-ext THEN DO:
            APPLY "CHOOSE" TO wgh-bt-confirma.
            RETURN "OK":U.
        END.
    
        IF AVAIL item-contrat-ext THEN DO:

            FIND FIRST contrato-for
                 WHERE contrato-for.nr-contrato = item-contrat-ext.nr-contrato NO-LOCK NO-ERROR.

            IF NOT AVAIL contrato-for THEN DO:
                APPLY "CHOOSE" TO wgh-bt-confirma.
                RETURN "OK":U.
            END.

            FIND FIRST param-inv NO-LOCK 
                 WHERE param-inv.ep-codigo = item-contrat-ext.ep-codigo NO-ERROR.

            IF NOT AVAIL param-inv THEN DO:
                APPLY "CHOOSE" TO wgh-bt-confirma.
                RETURN "OK":U.
            END.

            FIND FIRST item-contrat
                 WHERE item-contrat.nr-contrato  = item-contrat-ext.nr-contrato
                   AND item-contrat.num-seq-item = item-contrat-ext.num-seq-item NO-LOCK NO-ERROR.

            IF NOT AVAIL item-contrat THEN DO:
                APPLY "CHOOSE" TO wgh-bt-confirma.
                RETURN "OK":U.
            END.

    
            run pi-atualiza-verba in h-esapi001 (input  1, /* Valida Verba */
                                                 input  item-contrat-ext.ep-codigo,
                                                 input  item-contrat-ext.num-ord-inv,
                                                 input  contrato-for.dt-contrato,
                                                 input  param-inv.moeda-inv,
                                                 input  (ttDadosBrw.alter-origem - ttDadosBrw.alter-destino),
                                                 input  0,
                                                 output table tt-erro).

            IF CAN-FIND (FIRST tt-erro) THEN DO:
                RUN cdp/cd0669.w (INPUT TABLE tt-erro).
                RETURN "NOK":U.
            END.

            DO TRANS:

                APPLY "CHOOSE" TO wgh-bt-confirma.

                RUN pi-principal IN h-esapi001 (INPUT 2,
                                                INPUT ROWID(item-contrat),
                                                INPUT 2,
                                                OUTPUT TABLE tt-erro).

                IF CAN-FIND (FIRST tt-erro) THEN DO:
                    RUN cdp/cd0669.w (INPUT TABLE tt-erro).
                    UNDO, RETURN "NOK":U.
                END.
            END.
        END.
    END.

    FOR EACH ttDadosBrw
       WHERE ttDadosBrw.seq-item = 0
         AND ttDadosBrw.ativo    = YES:

        IF ttDadosBrw.ativo-destino = "yes" OR 
           ttDadosBrw.ativo-destino = "sim" THEN DO:
            APPLY "CHOOSE" TO wgh-bt-confirma.
            RETURN "OK":U.
        END.

        FIND FIRST contrato-for-ext EXCLUSIVE-LOCK
             WHERE contrato-for-ext.nr-contrato  = INT(wgh-contrato:SCREEN-VALUE) 
               AND contrato-for-ext.num-ord-inv <> 0 NO-ERROR.

        IF NOT AVAIL contrato-for-ext THEN DO:
            APPLY "CHOOSE" TO wgh-bt-confirma.
            RETURN "OK":U.
        END.

        if avail contrato-for-ext then do:

            for each  controle-inv-esp exclusive-lock
                where controle-inv-esp.ep-codigo    = contrato-for-ext.ep-codigo
                and   controle-inv-esp.num-ord-inv  = contrato-for-ext.num-ord-inv
                and   controle-inv-esp.nr-contrato  = contrato-for-ext.nr-contrato:

                FIND FIRST param-inv NO-LOCK 
                     WHERE param-inv.ep-codigo = contrato-for-ext.ep-codigo NO-ERROR.
    
                IF NOT AVAIL param-inv THEN DO:
                    APPLY "CHOOSE" TO wgh-bt-confirma.
                    RETURN "OK":U.
                END.

                if valid-handle(h-esapi001) then do: 
                    run pi-atualiza-verba in h-esapi001 (input 2,
                                                         input controle-inv-esp.ep-codigo,
                                                         input controle-inv-esp.num-ord-inv,
                                                         input controle-inv-esp.dt-trans,
                                                         input param-inv.moeda-inv,
                                                         input controle-inv-esp.ent-comp * -1,
                                                         input controle-inv-esp.ent-real * -1,
                                                         output table tt-erro).
                    if can-find (first tt-erro) then do:
                        run cdp/cd0669.w (input table tt-erro).
                        if valid-handle(h-esapi001) then do:
                            delete procedure h-esapi001.
                            assign h-esapi001 = ?.
                        end.
                        return "NOK":U.
                    end.
                end.
                delete controle-inv-esp.
            end.
            delete contrato-for-ext.

            FIND FIRST contrato-for
                 WHERE contrato-for.nr-contrato = int(wgh-contrato:SCREEN-VALUE) EXCLUSIVE-LOCK NO-ERROR.
    
            IF AVAIL contrato-for THEN DO:
    
                run pi-principal in h-esapi001 (input 1,
                                                input rowid(contrato-for),
                                                input 2,
                                                output table tt-erro).
    
                &IF '{&bf_mat_versao_ems}' >= '2.07' &THEN
                    ASSIGN contrato-for.num-ord-inv = 0.
                &ELSE
                    ASSIGN OVERLAY(contrato-for.char-1,1,9) = "0".
                &ENDIF
            END.

            APPLY "CHOOSE" TO wgh-bt-confirma.
        END.
    END.


    FOR EACH ttDadosBrw
       WHERE ttDadosBrw.seq-item <> 0
         AND ttDadosBrw.ativo    = YES:

        IF ttDadosBrw.ativo-destino = "yes" OR 
           ttDadosBrw.ativo-destino = "sim" THEN DO:
            APPLY "CHOOSE" TO wgh-bt-confirma.
            RETURN "OK":U.
        END.
        
        FIND FIRST item-contrat-ext EXCLUSIVE-LOCK
             WHERE item-contrat-ext.nr-contrato   = INT(wgh-contrato:SCREEN-VALUE)
               AND item-contrat-ext.num-seq-item  = ttDadosBrw.seq-item
               AND item-contrat-ext.num-ord-inv   <> 0 NO-ERROR.

        IF NOT AVAIL item-contrat-ext THEN DO:
            APPLY "CHOOSE" TO wgh-bt-confirma.
            RETURN "OK":U.
        END.
        
        if avail item-contrat-ext then do:
            for each  controle-inv-esp exclusive-lock
                where controle-inv-esp.ep-codigo     = item-contrat-ext.ep-codigo
                and   controle-inv-esp.num-ord-inv   = item-contrat-ext.num-ord-inv
                and   controle-inv-esp.nr-contrato   = item-contrat-ext.nr-contrato
                and   controle-inv-esp.num-seq-item  = item-contrat-ext.num-seq-item:

                FIND FIRST param-inv NO-LOCK 
                     WHERE param-inv.ep-codigo = item-contrat-ext.ep-codigo NO-ERROR.
    
                IF NOT AVAIL param-inv THEN DO:
                    APPLY "CHOOSE" TO wgh-bt-confirma.
                    RETURN "OK":U.
                END.

                run pi-atualiza-verba in h-esapi001 (input 2,
                                                     input controle-inv-esp.ep-codigo,
                                                     input controle-inv-esp.num-ord-inv,
                                                     input controle-inv-esp.dt-trans,
                                                     input param-inv.moeda-inv,
                                                     input controle-inv-esp.ent-comp * -1,
                                                     input controle-inv-esp.ent-real * -1,
                                                     output table tt-erro).
                if can-find (first tt-erro) then do:
                    run cdp/cd0669.w (input table tt-erro).
                    if valid-handle(h-esapi001) then do:
                        delete procedure h-esapi001.
                        assign h-esapi001 = ?.
                    end.
                    return "NOK":U.
                end.
                delete controle-inv-esp.
            end.
            delete item-contrat-ext.

            APPLY "CHOOSE" TO wgh-bt-confirma.

            FIND FIRST item-contrat EXCLUSIVE-LOCK
                 WHERE item-contrat.nr-contrato   = INT(wgh-contrato:SCREEN-VALUE)
                   AND item-contrat.num-seq-item  = ttDadosBrw.seq-item NO-ERROR.

            IF AVAIL item-contrat THEN DO:

                run pi-principal in h-esapi001 (input 2,
                                                input rowid(item-contrat),
                                                input 2,
                                                output table tt-erro).

                &IF '{&bf_mat_versao_ems}' >= '2.07' &THEN
                    ASSIGN item-contrat.num-ord-inv = 0.
                &ELSE
                    ASSIGN OVERLAY(item-contrat.char-1,1,9) = STRING(0).
                &ENDIF
            END.
        end.
    END.



    if valid-handle(h-esapi001) then do:
        delete procedure h-esapi001.
        assign h-esapi001 = ?.
    end.

    

    
END PROCEDURE.

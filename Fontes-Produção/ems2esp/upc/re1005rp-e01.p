/****************************************************************************************** 
** 	   Programa: re1005rp-e01.p
**   	      Autor: Vando Ribeiro
**   	 Fornecedor: DKP
**    	 Data: 31/10/2018
** Change/Chamado: REQ03
**    Objetivo: Cria pendàcias de atualizaá‰es de documentos com prazo de pagamento inferior
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** 
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**

****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: p-ind-event
** PAR∂METROS DE SA÷DA: tt-epc
** CADASTRADO NO FONTE TOTVS: RE1005RP
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

{utp/ut-glob.i}
{include/i-prgvrs.i BOIN090 2.06.00.000}
{include/i-epc200.i} 

DEFINE INPUT PARAM p-ind-event  AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAM TABLE FOR tt-epc.

DEFINE NEW GLOBAL SHARED VARIABLE cria-Bloq    AS LOGICAL   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE numerodias   AS INTEGER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE bloq-at      AS LOGICAL   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE i-ep-codigo-usuario AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-per-babilit AS LOGICAL   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE l-cria-hist   AS LOGICAL   NO-UNDO.

{cdp/cd0666.i}
DEFINE VARIABLE h_tt-erro        AS HANDLE      NO-UNDO.
DEFINE VARIABLE H_tt-erro-field  AS HANDLE      NO-UNDO.
DEFINE VARIABLE H_tt-erro-field2 AS HANDLE      NO-UNDO.
DEFINE VARIABLE l-bloqueia       AS LOGICAL     NO-UNDO.

DEF BUFFER      b-tt-epc      FOR tt-epc.
DEF BUFFER      b-dupli-apagar   FOR dupli-apagar .
DEF BUFFER      b-docum-est      FOR docum-est.

    
IF p-ind-event = "inicio-verifica-documento" THEN DO:

    ASSIGN cria-Bloq   = NO
           bloq-at     = NO
           l-cria-hist = NO.

    DEFINE VARIABLE dataContador     AS DATE    NO-UNDO.
   
   
    FIND FIRST b-tt-epc 
         WHERE b-tt-epc.cod-event     = p-ind-event
           AND b-tt-epc.cod-parameter = "docum-est rowid" NO-ERROR.
    IF NOT AVAIL(b-tt-epc) THEN RETURN "NOK".

    FIND FIRST b-docum-est 
         WHERE ROWID(b-docum-est) = TO-ROWID(b-tt-epc.val-parameter) NO-LOCK NO-ERROR.
    IF NOT AVAIL b-docum-est THEN RETURN "NOK".

    FIND FIRST esp_param_bloq NO-LOCK
         WHERE esp_param_bloq.cod-estabel = b-docum-est.cod-estabel NO-ERROR.

    IF NOT AVAIL esp_param_bloq THEN RETURN "N∆o encontrado o parÉmetro de Bloqueio.".

    ASSIGN l-bloqueia = NO.

    FOR EACH b-dupli-apagar OF b-docum-est NO-LOCK:

        ASSIGN l-bloqueia = NO
               numerodias = 0.

        IF TODAY > b-dupli-apagar.dt-vencim THEN 
            numerodias = 0.
        ELSE DO:
            dataContador = TODAY.

            IF esp_param_bloq.dias-uteis-corridos THEN /*(sim = uteis, n∆o = corridos)*/

               FIND FIRST dia_calend_glob 
                    WHERE dia_calend_glob.dat_calend = dataContador NO-LOCK NO-ERROR. /*Pega o dia do calendario global*/

            DO WHILE(dataContador < b-dupli-apagar.dt-vencim):
                IF NOT esp_param_bloq.dias-uteis-corridos THEN
                    numerodias = numerodias + 1.
                ELSE
                DO:
                    IF dia_calend_glob.log_dia_util THEN
                        numerodias = numerodias + 1.
                    FIND FIRST dia_calend_glob 
                          WHERE dia_calend_glob.dat_calend = dataContador + 1 NO-LOCK NO-ERROR.
                END.
                dataContador = dataContador + 1.
            END.
        END.

        IF numerodias > esp_param_bloq.dias-venc THEN /*qtde dias no parametro*/
            ASSIGN l-bloqueia = NO.
        ELSE
        DO:
            IF numerodias > 0 AND esp_param_bloq.bloq-atualizar THEN /*numero de dias inferior ao parametro e est† para bloquear*/
            DO:
                IF NOT CAN-FIND(FIRST esp_pend_aprov 
                                WHERE esp_pend_aprov.serie-docto  = b-docum-est.serie-docto  /*Verifica se ja foi aprovado*/  
                                  AND esp_pend_aprov.nro-docto    = b-docum-est.nro-docto     
                                  AND esp_pend_aprov.cod-emitente = b-docum-est.cod-emitente  
                                  AND esp_pend_aprov.nat-operacao = b-docum-est.nat-operacao
                                  AND esp_pend_aprov.cod_usuario_aprovador1      <> ""
                                  AND esp_pend_aprov.data-aprovacao1             <> ?) THEN 

                DO:
                    ASSIGN
                        l-bloqueia = YES.
                        cria-Bloq  = YES.
                END.
                ELSE
                    l-bloqueia = NO. /*qde inferior mas achou aprovaá∆o, poss°vel desatualizaá∆o de docto (re0402)*/

            END.
            ELSE IF numerodias > 0 AND NOT esp_param_bloq.bloq-atualizar THEN
                ASSIGN l-cria-hist = YES. /*numero de dias inferior ao parametro mas n∆o est† para bloquear*/
                                          /*gerar† o hist¢rico para envio de e-mail*/
        END.
        LEAVE.
    END.

    FIND FIRST trad_org_ext 
         WHERE trad_org_ext.cod_matriz_trad_org_ext = "EMS2"
           AND trad_org_ext.cod_tip_unid_organ = "998" /*tp para empresa*/
           AND trad_org_ext.cod_unid_organ_ext = i-ep-codigo-usuario NO-LOCK NO-ERROR.
    IF AVAIL trad_org_ext THEN
    DO:
        FIND FIRST sit_period_ctbl 
             WHERE sit_period_ctbl.cod_modul_dtsul = "APB"
               AND sit_period_ctbl.cod_empresa     = trad_org_ext.cod_unid_organ
               AND sit_period_ctbl.cod_exerc_ctbl  = STRING(YEAR(b-docum-est.dt-trans), "9999")
               AND sit_period_ctbl.num_period_ctbl = MONTH(b-docum-est.dt-trans)
               AND sit_period_ctbl.ind_sit_period_ctbl <> "Habilitado" NO-LOCK NO-ERROR.

        IF AVAIL sit_period_ctbl THEN
        DO:
           l-per-babilit = NO.
    
            PUT SKIP(1)
                "Situaá∆o de movimentaá∆o do m¢dulo n∆o est† Habilitada !" FORMAT "x(100)" SKIP
                "A Empresa do Estabelecimento " + TRIM(b-docum-est.cod-estabel) + " n∆o est† habilitada a realizar movimentos no" FORMAT "x(100)" SKIP
                "M¢dulo APB em " + STRING(b-docum-est.dt-trans, "99/99/9999") + ". Movimentos somente podem ser efetuados em per°odos" FORMAT "x(100)" SKIP
                "cuja situaá∆o de movimentaá∆o para o m¢dulo esteja habilitada." FORMAT "x(100)" SKIP(1).

        END.
        ELSE
            l-per-babilit = YES.
    END.

    IF l-bloqueia THEN RETURN "NOK".

    RETURN "OK".
 END.
    

IF p-ind-event = "apos-finalizar" THEN DO:

    IF NOT l-per-babilit THEN RETURN "NOK". /*se periodo nao habilitado na ctbl, n∆o atualiza o docto e n∆o cria pendància*/

    FIND FIRST tt-epc WHERE 
               tt-epc.cod-event     = "apos-finalizar" AND 
               tt-epc.cod-parameter = "docum-est rowid" NO-ERROR.


    IF AVAIL tt-epc THEN
    DO:
        FIND FIRST b-docum-est 
             WHERE ROWID(b-docum-est) = TO-ROWID(tt-epc.val-parameter) NO-LOCK NO-ERROR.

        IF NOT AVAIL b-docum-est THEN RETURN "NOK".

        IF l-cria-hist THEN
        DO:
            FIND FIRST esp_hist_aprov 
                 WHERE esp_hist_aprov.cod-estabel  = b-docum-est.cod-estabel
                   AND esp_hist_aprov.serie-docto  = b-docum-est.serie-docto
                   AND esp_hist_aprov.nro-docto    = b-docum-est.nro-docto     
                   AND esp_hist_aprov.cod-emitente = b-docum-est.cod-emitente  
                   AND esp_hist_aprov.nat-operacao = b-docum-est.nat-operacao EXCLUSIVE-LOCK NO-ERROR.

            IF NOT AVAIL esp_hist_aprov THEN
            DO:
                CREATE esp_hist_aprov.
                ASSIGN esp_hist_aprov.cod-estabel               = b-docum-est.cod-estabel  
                       esp_hist_aprov.serie-docto               = b-docum-est.serie-docto  
                       esp_hist_aprov.nro-docto                 = b-docum-est.nro-docto    
                       esp_hist_aprov.cod-emitente              = b-docum-est.cod-emitente 
                       esp_hist_aprov.nat-operacao              = b-docum-est.nat-operacao 
                       esp_hist_aprov.data-geracao              = NOW
                       esp_hist_aprov.cod_usuario_geracao       = c-seg-usuario  
                       esp_hist_aprov.dias                      = numerodias.
            END.
            ELSE
            DO:
                ASSIGN esp_hist_aprov.data-geracao              = NOW
                       esp_hist_aprov.cod_usuario_geracao       = c-seg-usuario  
                       esp_hist_aprov.dias                      = numerodias.
                RELEASE esp_hist_aprov.
            END.
        END.

        IF cria-bloq THEN
        DO:
            /*procura se o docto est† aprovado*/
            IF NOT CAN-FIND(FIRST esp_pend_aprov 
                            WHERE esp_pend_aprov.serie-docto  = b-docum-est.serie-docto
                              AND esp_pend_aprov.nro-docto    = b-docum-est.nro-docto     
                              AND esp_pend_aprov.cod-emitente = b-docum-est.cod-emitente  
                              AND esp_pend_aprov.nat-operacao = b-docum-est.nat-operacao
                              AND esp_pend_aprov.cod_usuario_aprovador1      <> ""
                              AND esp_pend_aprov.data-aprovacao1             <> ?) THEN 
            DO:
                IF NOT CAN-FIND(FIRST esp_pend_aprov 
                                WHERE esp_pend_aprov.serie-docto  = b-docum-est.serie-docto
                                  AND esp_pend_aprov.nro-docto    = b-docum-est.nro-docto     
                                  AND esp_pend_aprov.cod-emitente = b-docum-est.cod-emitente  
                                  AND esp_pend_aprov.nat-operacao = b-docum-est.nat-operacao
                                  AND esp_pend_aprov.cod_usuario_aprovador1      = ""
                                  AND esp_pend_aprov.data-aprovacao1             = ?
                                  AND esp_pend_aprov.data-reprovacao             = ?) THEN 
                DO:
                    FIND FIRST esp_param_bloq NO-LOCK
                         WHERE esp_param_bloq.cod-estabel = b-docum-est.cod-estabel NO-ERROR.
    
                    CREATE esp_pend_aprov.
                    ASSIGN esp_pend_aprov.cod-estabel               = b-docum-est.cod-estabel  
                           esp_pend_aprov.serie-docto               = b-docum-est.serie-docto  
                           esp_pend_aprov.nro-docto                 = b-docum-est.nro-docto    
                           esp_pend_aprov.cod-emitente              = b-docum-est.cod-emitente 
                           esp_pend_aprov.nat-operacao              = b-docum-est.nat-operacao 
                           esp_pend_aprov.data-geracao              = NOW
                           esp_pend_aprov.cod_usuario_geracao       = c-seg-usuario  
                           esp_pend_aprov.dias                      = numerodias
                           esp_pend_aprov.bloq-atualizar            = esp_param_bloq.bloq-atualizar
                           esp_pend_aprov.modulo                    = 2
                           esp_pend_aprov.emailenviado              = NO.
    
                    run utp/ut-msgs.p (input "show":U, input 27979, 
                                        input "Documento Bloqueado~~Documento bloqueado dependendo de aprovaá∆o para atualizaá∆o!").
                END.
            END.
        END.
    END.
END.

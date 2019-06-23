/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESAPI002 2.00.00.000}  /*** 010000 ***/
/******************************************************************************
**
**       Programa: ESAPI002
**
**       Data....: 10/06/2009
**
**       Autor...: JULIANA K. OLIVEIRA
**
**       Objetivo: Integracao MI - Frotas x Investimentos
**
**       Versao..: 1.00.000 - Sandra Stadelhofer
**
*******************************************************************************/
{cdp/cd0669.i}
{cdp/cd9731.i6} /* Definicao da integraá∆o de invstimentos com a ordem de produá∆o / MI */
{include/i-rpvar.i}
{esp/esapi002.i}
{utp/ut-glob.i}

/** PARAMETROS **/
DEFINE  INPUT PARAMETER p-acao             AS INTEGER                      NO-UNDO.
DEFINE  INPUT PARAMETER p-nr-ord-produ   LIKE ord-prod.nr-ord-produ        NO-UNDO.
DEFINE  INPUT PARAMETER p-num-ord-magnus LIKE sub-div-ordem.num-ord-magnus NO-UNDO.
DEFINE  INPUT PARAMETER TABLE FOR tt-param.
DEFINE OUTPUT PARAMETER TABLE FOR tt-erro.
/****************/

DEFINE TEMP-TABLE tt-erro-aux NO-UNDO
    FIELD i-sequen AS INT
    FIELD cd-erro  AS INT
    FIELD mensagem AS CHAR FORMAT "x(255)"
    FIELD c-param  AS CHAR.

CASE p-acao:
    /** INCLUI **/
    WHEN 1 THEN DO:
        IF p-num-ord-magnus <> 0 THEN DO:
            FOR FIRST mmv-ord-manut FIELDS (nr-ord-produ ep-codigo)
                WHERE mmv-ord-manut.nr-ord-produ = p-nr-ord-produ NO-LOCK:
            END.
            
            IF NOT CAN-FIND(FIRST controle-inv-frotas
                            WHERE controle-inv-frotas.nr-ord-produ = mmv-ord-manut.nr-ord-produ) THEN DO:
            
                FOR FIRST sub-div-ordem FIELDS (num-ord-magnus cod-est-exec num-ordem num-projeto)
                    WHERE sub-div-ordem.ep-codigo      = mmv-ord-manut.ep-codigo
                      AND sub-div-ordem.num-ord-magnus = p-num-ord-magnus NO-LOCK:
                END.
                
                CREATE controle-inv-frotas.
                ASSIGN controle-inv-frotas.ep-codigo      = mmv-ord-manut.ep-codigo     
                       controle-inv-frotas.num-ord-magnus = sub-div-ordem.num-ord-magnus
                       controle-inv-frotas.tipo-doc       = "Frotas":U
                       controle-inv-frotas.ent-real       = 0
                       controle-inv-frotas.sai-real       = 0
                       controle-inv-frotas.nr-ord-produ   = mmv-ord-manut.nr-ord-produ
                       controle-inv-frotas.log-eliminada  = NO
                       controle-inv-frotas.log-processada = NO
                       controle-inv-frotas.cod-est-exec   = sub-div-ordem.cod-est-exec
                       controle-inv-frotas.num-ordem      = sub-div-ordem.num-ordem   
                       controle-inv-frotas.num-projeto    = sub-div-ordem.num-projeto 
                       controle-inv-frotas.data-trans     = ?.
            END.
        END.
    END.

    /** MODIFICA **/
    WHEN 2 THEN DO:
        FOR FIRST mmv-ord-manut FIELDS (ep-codigo nr-ord-produ)
            WHERE mmv-ord-manut.nr-ord-produ = p-nr-ord-produ NO-LOCK:
        END.
        
        IF  p-num-ord-magnus <> 0 THEN
            FOR FIRST sub-div-ordem FIELDS (num-ord-magnus cod-est-exec num-ordem num-projeto)
                WHERE sub-div-ordem.ep-codigo      = mmv-ord-manut.ep-codigo
                  AND sub-div-ordem.num-ord-magnus = p-num-ord-magnus NO-LOCK:
            END.
        
        FOR FIRST controle-inv-frotas
            WHERE controle-inv-frotas.nr-ord-produ = mmv-ord-manut.nr-ord-produ NO-LOCK:
        END.
        IF NOT AVAIL controle-inv-frotas THEN DO:
            IF p-num-ord-magnus <> 0 THEN DO:
                CREATE controle-inv-frotas.
                ASSIGN controle-inv-frotas.ep-codigo      = mmv-ord-manut.ep-codigo     
                       controle-inv-frotas.num-ord-magnus = sub-div-ordem.num-ord-magnus
                       controle-inv-frotas.tipo-doc       = "Frotas":U
                       controle-inv-frotas.ent-real       = 0
                       controle-inv-frotas.sai-real       = 0
                       controle-inv-frotas.nr-ord-produ   = mmv-ord-manut.nr-ord-produ
                       controle-inv-frotas.log-eliminada  = NO
                       controle-inv-frotas.log-processada = NO
                       controle-inv-frotas.cod-est-exec   = sub-div-ordem.cod-est-exec
                       controle-inv-frotas.num-ordem      = sub-div-ordem.num-ordem   
                       controle-inv-frotas.num-projeto    = sub-div-ordem.num-projeto 
                       controle-inv-frotas.data-trans     = ?.
            END.
        END.
        ELSE DO:
            FIND FIRST bf-controle-inv-frotas
                WHERE bf-controle-inv-frotas.nr-ord-produ = mmv-ord-manut.nr-ord-produ EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL bf-controle-inv-frotas THEN DO:
                ASSIGN bf-controle-inv-frotas.log-processada = NO.

                IF controle-inv-frotas.num-ord-magnus <> p-num-ord-magnus THEN
                    ASSIGN bf-controle-inv-frotas.ep-codigo      = mmv-ord-manut.ep-codigo     
                           bf-controle-inv-frotas.num-ord-magnus = p-num-ord-magnus
                           bf-controle-inv-frotas.log-eliminada  = NO
                           bf-controle-inv-frotas.cod-est-exec   = IF p-num-ord-magnus = 0 THEN "" ELSE sub-div-ordem.cod-est-exec
                           bf-controle-inv-frotas.num-ordem      = IF p-num-ord-magnus = 0 THEN 0  ELSE sub-div-ordem.num-ordem   
                           bf-controle-inv-frotas.num-projeto    = IF p-num-ord-magnus = 0 THEN 0  ELSE sub-div-ordem.num-projeto 
                           bf-controle-inv-frotas.data-trans     = ?.
            END.
                
            RELEASE bf-controle-inv-frotas.
        END.
    END.

    /** ELIMINA **/
    WHEN 3 THEN DO:
        IF CAN-FIND(FIRST controle-inv-frotas
                    WHERE controle-inv-frotas.nr-ord-produ = mmv-ord-manut.nr-ord-produ) THEN DO:
                
            FIND FIRST bf-controle-inv-frotas
                WHERE bf-controle-inv-frotas.nr-ord-produ = mmv-ord-manut.nr-ord-produ EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL bf-controle-inv-frotas THEN
                ASSIGN controle-inv-frotas.log-eliminada = YES.
            RELEASE bf-controle-inv-frotas.
        END.
    END.

    /** VALIDA **/
    WHEN 4 THEN DO:
        IF  p-num-ord-magnus <> 0 THEN DO:
            /** VERIFICA USUµRIO NO PROGRAMA IN9010 **/
            FOR FIRST user-inv FIELDS (dt-desat)
                WHERE user-inv.usuario = c-seg-usuario NO-LOCK:
            END.
            IF NOT AVAIL user-inv THEN DO:
                ASSIGN c-mensagem = "Usu†rio " + c-seg-usuario + " inexistente no programa IN9010. V°nculo n∆o permitido!".
                RUN pi-cria-erro (INPUT 17006, INPUT c-mensagem).
                RETURN "NOK":U.
            END.
    
            IF user-inv.dt-desat <> ? THEN DO:
                ASSIGN c-mensagem = "Usu†rio " + c-seg-usuario + " desativado no programa IN9010. V°nculo n∆o permitido!".
                RUN pi-cria-erro (INPUT 17006, INPUT c-mensagem).
                RETURN "NOK":U.
            END.
            /**********/
        
            IF NOT CAN-FIND(FIRST sub-div-ordem
                            WHERE sub-div-ordem.num-ord-magnus = p-num-ord-magnus) THEN DO:
                {utp/ut-liter.i Ordem_Investimento *}
                RUN pi-cria-erro (INPUT 56, INPUT RETURN-VALUE).
                RETURN "NOK":U.
            END.
        END.
    END.

    /** ATUALIZA VERBA **/
    WHEN 5 THEN DO:
        RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
        
        {utp/ut-liter.i Ordens_Manutená∆o_Frotas *}
        RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

        FIND FIRST tt-param NO-LOCK NO-ERROR.

        {esp/esapi002.i1}
        
        VIEW FRAME f-cab-prod.

        FOR EACH controle-inv-frotas
            WHERE controle-inv-frotas.log-processada = NO NO-LOCK
            BREAK BY controle-inv-frotas.nr-ord-produ: 

            RUN pi-acompanhar IN h-acomp (INPUT c-lb-ord + ": " + STRING(controle-inv-frotas.nr-ord-produ)).

            FOR FIRST param-inv  FIELDS (moeda-inv tem-moeda1 tem-moeda2)
                WHERE param-inv.ep-codigo = controle-inv-frotas.ep-codigo NO-LOCK:
            END.
            IF NOT AVAIL param-inv THEN NEXT.

            RUN pi-desatualiza-verba.
        
            IF NOT CAN-FIND(FIRST sub-div-ordem
                            WHERE sub-div-ordem.ep-codigo      = controle-inv-frotas.ep-codigo
                              AND sub-div-ordem.num-ord-magnus = controle-inv-frotas.num-ord-magnus) THEN NEXT.

            FOR FIRST ord-prod FIELDS (cod-estabel nr-ord-produ dt-emissao it-codigo)
                WHERE ord-prod.nr-ord-produ = controle-inv-frotas.nr-ord-produ NO-LOCK:
            END.
            IF NOT AVAIL ord-prod THEN NEXT.
        
            FOR FIRST estabelec FIELDS (custo-contab)
                WHERE estabelec.cod-estabel = ord-prod.cod-estabel NO-LOCK :
            END.
            IF NOT AVAIL estabelec THEN NEXT.
        
            ASSIGN de-vl-material = 0
                   de-vl-mat-tot  = 0
                   de-vl-mob      = 0
                   de-vl-mob-tot  = 0
                   de-vl-servico  = 0
                   de-vl-ser-tot  = 0
                   de-vl-ggf      = 0
                   de-vl-ggf-tot  = 0
                   de-horas       = 0
                   de-quantidade  = 0.
        
            FOR EACH movto-dir FIELDS (tempo-homem valor-mob-m[1] valor-mob-o[1] valor-mob-p[1])
                WHERE movto-dir.nr-ord-produ = controle-inv-frotas.nr-ord-produ 
                  AND movto-dir.dt-trans    >= tt-param.da-ini 
                  AND movto-dir.dt-trans    <= tt-param.da-fim NO-LOCK:
        
                RUN pi-atribui-val-mob.
        
                IF movto-dir.tipo-trans = 1 THEN
                    ASSIGN de-horas         = de-horas + movto-dir.tempo-homem
                           de-vl-mob-tot[1] = de-vl-mob-tot[1] + de-vl-mob[1]
                           de-vl-mob-tot[2] = de-vl-mob-tot[2] + de-vl-mob[2] 
                           de-vl-mob-tot[3] = de-vl-mob-tot[3] + de-vl-mob[3].
                ELSE
                    ASSIGN de-horas         = de-horas - movto-dir.tempo-homem
                           de-vl-mob-tot[1] = de-vl-mob-tot[1] - de-vl-mob[1]
                           de-vl-mob-tot[2] = de-vl-mob-tot[2] - de-vl-mob[2] 
                           de-vl-mob-tot[3] = de-vl-mob-tot[3] - de-vl-mob[3].                       
            END.
        
            FOR EACH movto-ggf FIELDS (horas-report     valor-ggf-1-m[1] valor-ggf-2-m[1] valor-ggf-3-m[1] 
                                       valor-ggf-4-m[1] valor-ggf-5-m[1] valor-ggf-6-m[1] valor-ggf-1-o[1] 
                                       valor-ggf-2-o[1] valor-ggf-3-o[1] valor-ggf-4-o[1] valor-ggf-5-o[1] 
                                       valor-ggf-6-o[1] valor-ggf-1-p[1] valor-ggf-2-p[1] valor-ggf-3-p[1] 
                                       valor-ggf-4-p[1] valor-ggf-5-p[1] valor-ggf-6-p[1])
                WHERE movto-ggf.nr-ord-produ = controle-inv-frotas.nr-ord-produ 
                  AND movto-ggf.dt-trans    >= tt-param.da-ini 
                  AND movto-ggf.dt-trans    <= tt-param.da-fim NO-LOCK:
        
                RUN pi-atribui-val-ggf.
        
                IF movto-ggf.tipo-trans = 1 THEN
                    ASSIGN de-horas         = de-horas + movto-ggf.horas-report
                           de-vl-ggf-tot[1] = de-vl-ggf-tot[1] + de-vl-ggf[1]
                           de-vl-ggf-tot[2] = de-vl-ggf-tot[2] + de-vl-ggf[2]
                           de-vl-ggf-tot[3] = de-vl-ggf-tot[3] + de-vl-ggf[3].
                ELSE
                    ASSIGN de-horas         = de-horas - movto-ggf.horas-report
                           de-vl-ggf-tot[1] = de-vl-ggf-tot[1] - de-vl-ggf[1]
                           de-vl-ggf-tot[2] = de-vl-ggf-tot[2] - de-vl-ggf[2]
                           de-vl-ggf-tot[3] = de-vl-ggf-tot[3] - de-vl-ggf[3].
            END.
        
            FOR EACH movto-mat FIELDS (nr-ord-produ num-sequen esp-docto it-codigo tipo-trans quantidade 
                                       valor-mat-m[1] valor-mob-m[1] valor-ggf-m[1] 
                                       valor-mat-o[1] valor-mob-o[1] valor-ggf-o[1] 
                                       valor-mat-p[1] valor-mob-p[1] valor-ggf-p[1])
                WHERE movto-mat.nr-ord-produ = controle-inv-frotas.nr-ord-produ 
                  AND movto-mat.dt-trans    >= tt-param.da-ini 
                  AND movto-mat.dt-trans    <= tt-param.da-fim NO-LOCK:
        
                IF l-min-mmi THEN DO:
                    FOR FIRST movto-estoq FIELDS (nr-ord-produ num-sequen nat-operacao num-ord-inv)
                        WHERE movto-estoq.nr-ord-produ = movto-mat.nr-ord-produ
                        AND   movto-estoq.num-sequen   = movto-mat.num-sequen NO-LOCK: 
                    END.
                    IF AVAIL movto-estoq
                    AND movto-estoq.num-ord-inv <> 0 THEN NEXT.
                END.
                
                IF movto-mat.esp-docto = 1 /*ACA*/ 
                OR movto-mat.esp-docto = 8 /*EAC*/ THEN NEXT.
        
                FOR FIRST ITEM FIELDS (ind-serv-mat)
                    WHERE item.it-codigo = movto-mat.it-codigo NO-LOCK:
                END.
                IF NOT AVAIL ITEM THEN NEXT.
        
                RUN pi-atribui-val-mat.
        
                IF movto-mat.tipo-trans = 1 THEN DO:
                    ASSIGN de-quantidade = de-quantidade - movto-mat.quantidade.
        
                    IF ITEM.ind-serv-mat = 1 THEN 
                        ASSIGN de-vl-ser-tot[1] = de-vl-ser-tot[1] - de-vl-servico[1]
                               de-vl-ser-tot[2] = de-vl-ser-tot[2] - de-vl-servico[2]
                               de-vl-ser-tot[3] = de-vl-ser-tot[3] - de-vl-servico[3].
                    ELSE
                        ASSIGN de-vl-mat-tot[1] = de-vl-mat-tot[1] -  de-vl-material[1]
                               de-vl-mat-tot[2] = de-vl-mat-tot[2] -  de-vl-material[2]
                               de-vl-mat-tot[3] = de-vl-mat-tot[3] -  de-vl-material[3].
                END.
                ELSE DO:
                    ASSIGN de-quantidade = de-quantidade + movto-mat.quantidade.
        
                    IF item.ind-serv-mat = 1 THEN
                        ASSIGN de-vl-ser-tot[1] = de-vl-ser-tot[1] + de-vl-servico[1]
                               de-vl-ser-tot[2] = de-vl-ser-tot[2] + de-vl-servico[2]
                               de-vl-ser-tot[3] = de-vl-ser-tot[3] + de-vl-servico[3].
                    ELSE
                        ASSIGN de-vl-mat-tot[1] = de-vl-mat-tot[1] +  de-vl-material[1]
                               de-vl-mat-tot[2] = de-vl-mat-tot[2] +  de-vl-material[2]
                               de-vl-mat-tot[3] = de-vl-mat-tot[3] +  de-vl-material[3].
                END.
            END.
        
            IF de-vl-material[1] = ? THEN ASSIGN de-vl-material[1] = 0.
            IF de-vl-material[2] = ? THEN ASSIGN de-vl-material[2] = 0.
            IF de-vl-material[3] = ? THEN ASSIGN de-vl-material[3] = 0.
            IF de-vl-servico[1]  = ? THEN ASSIGN de-vl-servico[1]  = 0.
            IF de-vl-servico[2]  = ? THEN ASSIGN de-vl-servico[2]  = 0.
            IF de-vl-servico[3]  = ? THEN ASSIGN de-vl-servico[3]  = 0.
            IF de-vl-mob[1]      = ? THEN ASSIGN de-vl-mob[1]      = 0.
            IF de-vl-mob[2]      = ? THEN ASSIGN de-vl-mob[2]      = 0.
            IF de-vl-mob[3]      = ? THEN ASSIGN de-vl-mob[3]      = 0.
            IF de-vl-ggf[1]      = ? THEN ASSIGN de-vl-ggf[1]      = 0.
            IF de-vl-ggf[2]      = ? THEN ASSIGN de-vl-ggf[2]      = 0.
            IF de-vl-ggf[3]      = ? THEN ASSIGN de-vl-ggf[3]      = 0.
            IF de-horas          = ? THEN ASSIGN de-horas          = 0.
            IF de-quantidade     = ? THEN ASSIGN de-quantidade     = 0.

            IF  de-vl-mat-tot[1] = 0
            AND de-vl-ser-tot[1] = 0
            AND de-vl-mob-tot[1] = 0
            AND de-vl-ggf-tot[1] = 0 THEN
                NEXT.
        
            RUN pi-controle-verba-frotas.

            DISP ord-prod.nr-ord-produ
                 ord-prod.dt-emissao
                 ord-prod.it-codigo
                 de-vl-mat-tot[1]
                 de-vl-ser-tot[1]
                 de-vl-mob-tot[1]
                 de-vl-ggf-tot[1]
                 de-quantidade
                 de-horas
                 WITH FRAME f-prod.
            DOWN WITH FRAME f-prod.

            FOR EACH tt-erro-aux:
                PUT "            Observaá∆o: ESTOURO VERBA - " tt-erro-aux.mensagem SKIP.
            END.
        END.

        HIDE FRAME f-cab-prod.

        &IF "{&FNC_MULTI_IDIOMA}" = "Yes" &THEN
            DEFINE VARIABLE cAuxTraducao001 AS CHARACTER NO-UNDO.
            ASSIGN cAuxTraducao001 = {varinc/var00002.i 04 tt-param.destino}.
            run utp/ut-liter.p (INPUT REPLACE(TRIM(cAuxTraducao001)," ","_"),
                                INPUT "",
                                INPUT "").
            ASSIGN  c-destino = RETURN-VALUE.  
        &ELSE
            ASSIGN c-destino = {varinc/var00002.i 04 tt-param.destino}.
        &ENDIF
        
        PAGE.

        VIEW FRAME f-cabec.
        VIEW FRAME f-rodape.
        
        PUT UNFORMATTED
            c-lb-selec              SKIP(1)
            c-lb-periodo            AT 5  ": "
            tt-param.da-ini         FORMAT "99/99/9999"
            " |<  >| "               tt-param.da-fim FORMAT "99/99/9999" SKIP(1).
        
        PUT UNFORMATTED
            c-lb-par                SKIP(1).

        PUT UNFORMATTED
            c-lb-requis             AT 5  ": " c-lb-par-requis 
            c-lb-ordens             AT 5  ": " c-lb-par-ordens SKIP(1)
            c-lb-impr               SKIP(1)
            c-lb-dest               AT 5  ": " c-destino " - " tt-param.arquivo
            c-lb-usuar              AT 5  ": " tt-param.usuario.

        {include/i-rpclo.i}
        RUN pi-finalizar IN h-acomp.   
    END.
END CASE.

RETURN "OK":U.

PROCEDURE pi-cria-erro:

    DEFINE INPUT PARAMETER p-cd-erro AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER p-complem AS CHAR    NO-UNDO.

    RUN utp/ut-msgs.p (INPUT "MSG":U,
                       INPUT p-cd-erro,
                       INPUT p-complem).  

    CREATE tt-erro.
    ASSIGN tt-erro.cd-erro  = p-cd-erro
           tt-erro.mensagem = RETURN-VALUE.

    RETURN "OK":U.
END PROCEDURE.

PROCEDURE pi-atribui-val-mob:

    CASE estabelec.custo-contab:
        WHEN 1 THEN
            ASSIGN de-vl-mob-base = movto-dir.valor-mob-m[1].
        WHEN 2 THEN
            ASSIGN de-vl-mob-base = movto-dir.valor-mob-o[1] .
        OTHERWISE
            ASSIGN de-vl-mob-base = movto-dir.valor-mob-p[1].
    END CASE.

    IF param-inv.moeda-inv <> 0 THEN
        RUN cdp/cd0812.p (0,
                          param-inv.moeda-inv,
                          INPUT de-vl-mob-base, 
                          INPUT ord-prod.dt-emissao,
                          OUTPUT de-vl-mob[1]).
    ELSE
        ASSIGN de-vl-mob[1] = de-vl-mob-base.
    
    IF param-inv.tem-moeda1 THEN
        RUN cdp/cd0812.p (0,
                          param-inv.moeda1,
                          INPUT de-vl-mob-base, 
                          INPUT ord-prod.dt-emissao,
                          OUTPUT de-vl-mob[2]).
    ELSE
        ASSIGN de-vl-mob[2] = 0.
    
    IF param-inv.tem-moeda2 THEN
        RUN cdp/cd0812.p (0,
                          param-inv.moeda2,
                          INPUT de-vl-mob-base, 
                          INPUT ord-prod.dt-emissao,
                          OUTPUT de-vl-mob[3]).
    ELSE
        ASSIGN de-vl-mob[3] = 0.
    
    IF de-vl-mob[1] = ? THEN ASSIGN de-vl-mob[1] = 0.
    IF de-vl-mob[2] = ? THEN ASSIGN de-vl-mob[2] = 0.
    IF de-vl-mob[3] = ? THEN ASSIGN de-vl-mob[3] = 0.

    RETURN "OK":U.

END PROCEDURE. 

PROCEDURE pi-atribui-val-ggf:

    CASE estabelec.custo-contab:
        WHEN 1 THEN
            ASSIGN de-vl-ggf-base = movto-ggf.valor-ggf-1-m[1] + movto-ggf.valor-ggf-2-m[1] + 
                                    movto-ggf.valor-ggf-3-m[1] + movto-ggf.valor-ggf-4-m[1] + 
                                    movto-ggf.valor-ggf-5-m[1] + movto-ggf.valor-ggf-6-m[1].
        WHEN 2 THEN
            ASSIGN de-vl-ggf-base = movto-ggf.valor-ggf-1-o[1] + movto-ggf.valor-ggf-2-o[1] + 
                                    movto-ggf.valor-ggf-3-o[1] + movto-ggf.valor-ggf-4-o[1] + 
                                    movto-ggf.valor-ggf-5-o[1] + movto-ggf.valor-ggf-6-o[1] .
        OTHERWISE
            ASSIGN de-vl-ggf-base = movto-ggf.valor-ggf-1-p[1] + movto-ggf.valor-ggf-2-p[1] + 
                                    movto-ggf.valor-ggf-3-p[1] + movto-ggf.valor-ggf-4-p[1] + 
                                    movto-ggf.valor-ggf-5-p[1] + movto-ggf.valor-ggf-6-p[1].
    END CASE.

    IF param-inv.moeda-inv <> 0 THEN
        RUN cdp/cd0812.p (0,
                          param-inv.moeda-inv,
                          INPUT de-vl-ggf-base, 
                          INPUT ord-prod.dt-emissao,
                          OUTPUT de-vl-ggf[1]).
     ELSE 
         ASSIGN de-vl-ggf[1] = de-vl-ggf-base.

     IF param-inv.tem-moeda1 THEN
         RUN cdp/cd0812.p (0,
                           param-inv.moeda1,
                           INPUT de-vl-ggf-base, 
                           INPUT ord-prod.dt-emissao,
                           OUTPUT de-vl-ggf[2]).
     ELSE 
         ASSIGN de-vl-ggf[2] = 0.
    
     IF param-inv.tem-moeda2 THEN
         RUN cdp/cd0812.p (0,
                           param-inv.moeda2,
                           INPUT de-vl-ggf-base, 
                           INPUT ord-prod.dt-emissao,
                           OUTPUT de-vl-ggf[3]).
     ELSE 
         ASSIGN de-vl-ggf[3] = 0.   
    
     IF de-vl-ggf[1] = ? THEN ASSIGN de-vl-ggf[1] = 0.
     IF de-vl-ggf[2] = ? THEN ASSIGN de-vl-ggf[2] = 0.
     IF de-vl-ggf[3] = ? THEN ASSIGN de-vl-ggf[3] = 0.

     RETURN "OK":U.

END PROCEDURE.   

PROCEDURE pi-atribui-val-mat:

    CASE estabelec.custo-contab:
        WHEN 1 THEN
            ASSIGN de-vl-mat-base = movto-mat.valor-mat-m[1] + 
                                    movto-mat.valor-mob-m[1] + 
                                    movto-mat.valor-ggf-m[1].
        WHEN 2 THEN
            ASSIGN de-vl-mat-base = movto-mat.valor-mat-o[1] + 
                                    movto-mat.valor-mob-o[1] +
                                    movto-mat.valor-ggf-o[1].
        OTHERWISE
            ASSIGN de-vl-mat-base = movto-mat.valor-mat-p[1] + 
                                    movto-mat.valor-mob-p[1] +
                                    movto-mat.valor-ggf-p[1].
    END CASE.

    IF param-inv.moeda-inv <> 0 THEN
        RUN cdp/cd0812.p (0,
                          param-inv.moeda-inv,
                          INPUT de-vl-mat-base, 
                          INPUT ord-prod.dt-emissao,
                          OUTPUT de-vl-material[1]).
    ELSE
        ASSIGN de-vl-material[1] = de-vl-mat-base.
    
    IF param-inv.tem-moeda1 THEN 
        RUN cdp/cd0812.p (0,
                          param-inv.moeda1,
                          INPUT de-vl-mat-base, 
                          INPUT ord-prod.dt-emissao,
                          OUTPUT de-vl-material[2]).
    ELSE
        ASSIGN de-vl-material[2] = 0.

    IF param-inv.tem-moeda2 THEN
        RUN cdp/cd0812.p (0,
                          param-inv.moeda2,
                          INPUT de-vl-mat-base, 
                          INPUT ord-prod.dt-emissao,
                          OUTPUT de-vl-material[3]).
    ELSE 
        ASSIGN de-vl-material[3] = 0.
    
    IF de-vl-material[1] = ? THEN ASSIGN de-vl-material[1] = 0.
    IF de-vl-material[2] = ? THEN ASSIGN de-vl-material[2] = 0.
    IF de-vl-material[3] = ? THEN ASSIGN de-vl-material[3] = 0.
    
    IF item.ind-serv-mat = 1 THEN 
        ASSIGN de-vl-servico[1] = de-vl-material[1]
               de-vl-servico[2] = de-vl-material[2]
               de-vl-servico[3] = de-vl-material[3]
               de-vl-material = 0.
    
    RETURN "OK":U.

END PROCEDURE. 

PROCEDURE pi-desatualiza-verba:

    IF  controle-inv-frotas.log-eliminada THEN DO:
        RUN inp/inapi048.p(3,
                           controle-inv-frotas.ep-codigo,
                           controle-inv-frotas.num-ord-magnus,
                           controle-inv-frotas.data-trans,
                           param-inv.moeda-inv,
                           0,
                           controle-inv-frotas.ent-real * -1,
                           OUTPUT TABLE tt-erro).

        FOR FIRST bf-controle-inv-frotas EXCLUSIVE-LOCK
            WHERE bf-controle-inv-frotas.nr-ord-produ = controle-inv-frotas.nr-ord-produ:
        END.
        IF AVAIL bf-controle-inv-frotas THEN
            DELETE bf-controle-inv-frotas.

        NEXT.
    END.

    IF  controle-inv-frotas.num-ord-magnus-ant <> 0
    AND controle-inv-frotas.num-ord-magnus-ant <> controle-inv-frotas.num-ord-magnus THEN DO:

        RUN inp/inapi048.p(3,
                           controle-inv-frotas.ep-codigo,
                           controle-inv-frotas.num-ord-magnus-ant,
                           controle-inv-frotas.data-trans,
                           param-inv.moeda-inv,
                           0,
                           controle-inv-frotas.ent-real * -1,
                           OUTPUT TABLE tt-erro).

        FOR FIRST bf-controle-inv-frotas EXCLUSIVE-LOCK
            WHERE bf-controle-inv-frotas.nr-ord-produ = controle-inv-frotas.nr-ord-produ:
        END.
        IF AVAIL bf-controle-inv-frotas THEN
            ASSIGN bf-controle-inv-frotas.sai-real = bf-controle-inv-frotas.ent-real
                   bf-controle-inv-frotas.ent-real = 0.
        RELEASE bf-controle-inv-frotas.
    END.                                                                                    

    RETURN "OK":U.

END PROCEDURE.

PROCEDURE pi-controle-verba-frotas:
    EMPTY TEMP-TABLE tt-erro-aux.

    RUN inp/inapi048.p(1,
                       controle-inv-frotas.ep-codigo,
                       controle-inv-frotas.num-ord-magnus,
                       ord-prod.dt-emissao,
                       param-inv.moeda-inv,
                       0,
                       de-vl-mat-tot[1] + de-vl-mob-tot[1] + de-vl-ggf-tot[1] + de-vl-ser-tot[1],
                       OUTPUT TABLE tt-erro). 

    FOR EACH tt-erro:
        RUN utp/ut-msgs.p (INPUT "MSG":U, INPUT tt-erro.cd-erro, INPUT tt-erro.c-param).
        CREATE tt-erro-aux.
        ASSIGN tt-erro-aux.cd-erro  = tt-erro.cd-erro
               tt-erro-aux.mensagem = RETURN-VALUE. 
    END.

    RUN inp/inapi048.p(3,
                       controle-inv-frotas.ep-codigo,
                       controle-inv-frotas.num-ord-magnus,
                       ord-prod.dt-emissao,
                       param-inv.moeda-inv,
                       0,
                       de-vl-mat-tot[1] + de-vl-mob-tot[1] + de-vl-ggf-tot[1] + de-vl-ser-tot[1],
                       OUTPUT TABLE tt-erro). 

    FOR FIRST bf-controle-inv-frotas EXCLUSIVE-LOCK
        WHERE bf-controle-inv-frotas.nr-ord-produ = controle-inv-frotas.nr-ord-produ:
    END.
    IF AVAIL bf-controle-inv-frotas THEN
        ASSIGN bf-controle-inv-frotas.ent-real           = de-vl-mat-tot[1] + de-vl-mob-tot[1] + de-vl-ggf-tot[1] + de-vl-ser-tot[1]
               bf-controle-inv-frotas.num-ord-magnus-ant = controle-inv-frotas.num-ord-magnus
               bf-controle-inv-frotas.log-processada     = YES
               bf-controle-inv-frotas.data-trans         = TODAY.
    RELEASE bf-controle-inv-frotas.

    RETURN "OK":U.

END PROCEDURE.

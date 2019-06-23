/*********************************************************************************
**
** rip/ri0404rp.i3 - pi-calcula-valores-bem
**
*********************************************************************************/    

PROCEDURE pi-calcula-valores-bem:

    /*
    ** TABELAS OBRIGAGORIAS
    ** ri-bem
    ** ri-bem-grupo
    */
    DEF INPUT  PARAMETER p-i-nr-parc-cred            AS   INT                        NO-UNDO.
    DEF INPUT  PARAMETER p-da-data-corte             as   date                       no-undo.
    DEF INPUT  PARAMETER p-da-data-ult-contab        AS   DATE                       NO-UNDO.
    DEF OUTPUT PARAMETER p-de-vl-tot-cred            like ri-valor-bem.val-imposto   no-undo.
    DEF OUTPUT PARAMETER p-de-vl-tot-recuperado      like ri-valor-bem.val-imposto   no-undo.            
    DEF OUTPUT PARAMETER p-de-vl-tot-nao-recuperado  like ri-valor-bem.val-imposto   no-undo.              
    DEF OUTPUT PARAMETER p-de-vl-tot-baixa           like ri-valor-bem.val-imposto   no-undo.                                
    DEF OUTPUT PARAMETER p-de-vl-saldo-cred          like ri-valor-bem.val-imposto   no-undo.                                
    
/*    def var de-vl-saldo-aux         like ri-valor-bem.val-imposto   no-undo. */

    DEF VAR de-valor-bem                LIKE ri-valor-bem.val-imposto       NO-UNDO.
    DEF VAR de-perc-baixa               AS   DECIMAL                        NO-UNDO.
    DEF VAR de-vl-recuperado            LIKE ri-valor-bem.val-imposto       NO-UNDO.
    DEF VAR de-vl-baixa                 LIKE ri-valor-bem.val-imposto       NO-UNDO.
    DEF VAR i-nr-parc-baixa             AS   INTEGER                        NO-UNDO.
    DEF VAR de-valor-atual-bem          LIKE ri-valor-bem.val-imposto       NO-UNDO.
    DEF VAR de-vl-parcela               LIKE ri-valor-bem.val-imposto       NO-UNDO.

    IF  p-da-data-ult-contab = ? THEN
        ASSIGN p-da-data-ult-contab = 01/01/2000.

    /* acumula os valores at‚ o mˆs anterior … data da £ltima contabiliza‡Æo */
    ASSIGN p-de-vl-tot-cred = 0.
    FOR EACH ri-valor-bem NO-LOCK
        WHERE ri-valor-bem.id-bem    = ri-bem-grupo.id-bem
        AND   ri-valor-bem.cod-grupo = ri-bem-grupo.cod-grupo
        AND   ri-valor-bem.dat-movto <= p-da-data-corte
        BREAK BY ri-valor-bem.cod-imposto:
    
        /* primeiro movimento de cada imposto */
        IF  FIRST-OF(ri-valor-bem.cod-imposto) THEN
            ASSIGN p-de-vl-tot-cred = p-de-vl-tot-cred + ri-valor-bem.val-imposto.
    END.

    ASSIGN p-de-vl-tot-recuperado     = 0
           p-de-vl-tot-nao-recuperado = 0.
    
    /* contabiliza‡Æo do cr‚dito mensal */
    FOR EACH ped-curva USE-INDEX ch-curva NO-LOCK 
        WHERE ped-curva.vl-aberto = 412
        AND   ped-curva.codigo  = ri-bem-grupo.id-bem
        AND   ped-curva.tot-ped = ri-bem-grupo.cod-grupo
        AND   ped-curva.regiao <= STRING(YEAR(p-da-data-corte), "9999") + "/" +
                                       STRING(MONTH(p-da-data-corte), "99") + "/01":
    
        ASSIGN p-de-vl-tot-recuperado     = p-de-vl-tot-recuperado + ped-curva.vl-lucro-br
               p-de-vl-tot-nao-recuperado = p-de-vl-tot-nao-recuperado + ped-curva.quantidade.
    END.
    
    /* contabilizacao da corre‡Æo na moeda alternativa */
    FOR EACH ped-curva USE-INDEX ch-curva NO-LOCK 
        WHERE ped-curva.vl-aberto = 415
        AND   ped-curva.codigo  = ri-bem-grupo.id-bem
        AND   ped-curva.tot-ped = ri-bem-grupo.cod-grupo
        AND   ped-curva.regiao <= STRING(YEAR(p-da-data-corte), "9999") + "/" +
                                       STRING(MONTH(p-da-data-corte), "99") + "/01":
    
        ASSIGN p-de-vl-tot-nao-recuperado = p-de-vl-tot-nao-recuperado + ped-curva.quantidade
               p-de-vl-tot-cred           = p-de-vl-tot-cred + (ped-curva.vl-lucro-br + ped-curva.quantidade).
    END.
    
    FOR FIRST ped-curva USE-INDEX ch-curva NO-LOCK 
        WHERE ped-curva.vl-aberto = 413
        AND   ped-curva.codigo  = ri-bem-grupo.id-bem
        AND   ped-curva.tot-ped = ri-bem-grupo.cod-grupo:
    END.

    IF  NOT AVAIL ped-curva 
    AND ri-bem.dat-entrada > ri-bem.dat-inic-cred THEN 
        /* bem transferido que nÆo tem hist¢rico do impsoto recuperado */
        FOR FIRST natur-oper NO-LOCK
            WHERE natur-oper.nat-operacao = ri-bem.nat-operacao
            AND   natur-oper.transf       = YES.
            RUN pi-gera-historico-transf.
        END.

    /* Valores transferidos do estabelecimento de origem  */
    FOR EACH ped-curva USE-INDEX ch-curva NO-LOCK 
        WHERE ped-curva.vl-aberto = 413
        AND   ped-curva.codigo  = ri-bem-grupo.id-bem
        AND   ped-curva.tot-ped = ri-bem-grupo.cod-grupo
        AND   ped-curva.regiao <= STRING(YEAR(p-da-data-corte), "9999") + "/" +
                                  STRING(MONTH(p-da-data-corte), "99") + "/" +
                                  STRING(DAY(p-da-data-corte), "99"):
    
        ASSIGN p-de-vl-tot-recuperado     = p-de-vl-tot-recuperado + ped-curva.vl-lucro-br
               p-de-vl-tot-nao-recuperado = p-de-vl-tot-nao-recuperado + ped-curva.quantidade.
    END.

/*     /* acumula os valores at‚ o mˆs anterior … data da £ltima contabiliza‡Æo */                                               */
/*     ASSIGN de-vl-saldo-aux = 0.                                                                                               */
/*     FOR EACH  ri-valor-bem NO-LOCK                                                                                            */
/*         WHERE ri-valor-bem.id-bem      = ri-bem-grupo.id-bem                                                                  */
/*         AND   ri-valor-bem.cod-grupo   = ri-bem-grupo.cod-grupo                                                               */
/*         AND   ri-valor-bem.dat-movto   <= p-da-data-corte                                                                     */
/*         BREAK BY ri-valor-bem.cod-imposto:                                                                                    */
/*                                                                                                                               */
/*         IF  LAST-OF(ri-valor-bem.cod-imposto) THEN                                                                            */
/*             ASSIGN de-vl-saldo-aux = de-vl-saldo-aux + ri-valor-bem.val-imposto.                                              */
/*     END.                                                                                                                      */
/*                                                                                                                               */
/*     ASSIGN p-de-vl-tot-baixa = IF de-vl-saldo-aux = 0                                                                         */
/*                              THEN p-de-vl-tot-cred - (p-de-vl-tot-recuperado + p-de-vl-tot-nao-recuperado)                    */
/*                              ELSE p-de-vl-tot-cred - de-vl-saldo-aux                                                          */
/*                                                                                                                               */
/*            p-de-vl-saldo-cred = p-de-vl-tot-cred - (p-de-vl-tot-recuperado + p-de-vl-tot-nao-recuperado + p-de-vl-tot-baixa). */
/*                                                                                                                               */
/*     IF  p-de-vl-tot-baixa < 0 THEN                                                                                            */
/*         ASSIGN p-de-vl-tot-baixa = 0.                                                                                         */
    
    IF  p-de-vl-tot-cred <> (p-de-vl-tot-recuperado + p-de-vl-tot-nao-recuperado) THEN DO:
        FOR FIRST ri-movto-bem
            WHERE ri-movto-bem.cod-estabel = ri-bem.cod-estabel
            AND   ri-movto-bem.id-bem      = ri-bem.id-bem
            AND   ri-movto-bem.idi-tipo-movto = 1:  /* Implanta‡Æo     */
            ASSIGN de-valor-bem = ri-movto-bem.val-movto.
        END.

        ASSIGN p-de-vl-tot-baixa = 0.
        FOR EACH ri-movto-bem NO-LOCK
            WHERE ri-movto-bem.cod-estabel = ri-bem.cod-estabel
            AND   ri-movto-bem.id-bem      = ri-bem.id-bem
            AND   ri-movto-bem.dat-movto   <= p-da-data-corte
            AND   ri-movto-bem.dat-movto   >  p-da-data-ult-contab
            AND (   ri-movto-bem.idi-tipo-movto = 4   /* Vendas          */
                 OR ri-movto-bem.idi-tipo-movto = 5   /* Transferencia   */
                 OR ri-movto-bem.idi-tipo-movto = 6   /* Baixa por Perda */
                 OR ri-movto-bem.idi-tipo-movto = 7   /* Devolucao       */
                 OR ri-movto-bem.idi-tipo-movto = 8   /* Inutilizacao    */
                 OR ri-movto-bem.idi-tipo-movto = 14  /* Exaustao        */)
    
            BREAK BY ri-movto-bem.nr-sequencia-movto:
    
            run rip/ri9999.p (INPUT ri-bem-grupo.data-1,
                              INPUT ri-movto-bem.dat-movto,
                              OUTPUT i-nr-parc-baixa).

            /* se nao contabiliza no mˆs da baixa, entÆo tem que substrair uma parcela */
            IF  ri-estab-grupos.log-1 = NO
            AND i-nr-parc-baixa >= 1 THEN
                ASSIGN i-nr-parc-baixa = i-nr-parc-baixa - 1.
            
            ASSIGN de-perc-baixa     = ri-movto-bem.val-movto / de-valor-bem
                   de-vl-recuperado  = ROUND(ri-bem-grupo.val-imposto / ri-bem-grupo.num-meses, 2) * i-nr-parc-baixa
                   de-vl-baixa       = ROUND(ri-bem-grupo.val-imposto * de-perc-baixa, 2).
            /*se for menor que 0 eh pq houve uma baixa parcial antes do inicio do credito do bem gerando assim um valor errado*/
            IF i-nr-parc-baixa >= 0 THEN 
                ASSIGN de-vl-baixa = de-vl-baixa - ROUND(de-vl-recuperado * de-perc-baixa, 2).
            ASSIGN p-de-vl-tot-baixa = p-de-vl-tot-baixa + de-vl-baixa.

        END.
    END.

  
    ASSIGN p-de-vl-saldo-cred = p-de-vl-tot-cred - (p-de-vl-tot-recuperado + p-de-vl-tot-nao-recuperado + p-de-vl-tot-baixa).

    IF  p-de-vl-saldo-cred < 0 
    /* OR  p-i-nr-parc-cred >= ri-bem-grupo.num-meses */ THEN
        ASSIGN p-de-vl-saldo-cred = 0.

END PROCEDURE.

/* rip/ri0404.i3 */

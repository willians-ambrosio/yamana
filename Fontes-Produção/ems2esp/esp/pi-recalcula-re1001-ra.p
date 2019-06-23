DEFINE INPUT PARAMETER  serie-docto     LIKE item-doc-est.serie-docto.   
DEFINE INPUT PARAMETER  nro-docto       LIKE item-doc-est.nro-docto.     
DEFINE INPUT PARAMETER  cod-emitente    LIKE item-doc-est.cod-emitente.  
DEFINE INPUT PARAMETER  nat-operacao    LIKE item-doc-est.nat-operacao.  
DEFINE INPUT PARAMETER  sequencia       LIKE item-doc-est.sequencia.    
DEFINE INPUT PARAMETER  it-codigo       LIKE item-doc-est.it-codigo.
DEFINE INPUT PARAMETER  qt-do-forn      LIKE item-doc-est.qt-do-forn.
DEFINE INPUT PARAMETER  preco-unitario  LIKE item-doc-est.preco-unit[1].
DEFINE INPUT PARAMETER  preco-total     LIKE item-doc-est.preco-total[1].
DEFINE INPUT PARAMETER  desconto        LIKE item-doc-est.desconto[1].   
DEFINE INPUT PARAMETER  despesas        LIKE item-doc-est.despesas[1].   
DEFINE INPUT PARAMETER  pr-total-cmi    LIKE item-doc-est.pr-total-cmi.  
DEFINE INPUT PARAMETER  peso-liquido    LIKE item-doc-est.peso-liquido.  
DEFINE INPUT PARAMETER  aliquota-ipi    LIKE item-doc-est.aliquota-ipi.  
DEFINE INPUT PARAMETER  cd-trib-ipi     LIKE item-doc-est.cd-trib-ipi.   
DEFINE INPUT PARAMETER  aliquota-iss    LIKE item-doc-est.aliquota-iss.  
DEFINE INPUT PARAMETER  cd-trib-iss     LIKE item-doc-est.cd-trib-iss.   
DEFINE INPUT PARAMETER  aliquota-icm    LIKE item-doc-est.aliquota-icm.  
DEFINE INPUT PARAMETER  cd-trib-icm     LIKE item-doc-est.cd-trib-icm.
DEFINE INPUT PARAMETER  char-2          AS DEC.
DEFINE INPUT PARAMETER  log-2           LIKE item-doc-est.log-2.
DEFINE INPUT PARAMETER  perc-red-icm    AS DEC FORMAT ">>9.9999".  
DEFINE INPUT PARAMETER  imp-vBCST       LIKE nfe-it-nota-fisc-rec.imp-vBCST.
DEFINE INPUT PARAMETER  imp-vICMSST     LIKE nfe-it-nota-fisc-rec.imp-vICMSST.
/*
MESSAGE "recebeu" SKIP

"serie-docto   " serie-docto   skip
"nro-docto     " nro-docto     skip
"cod-emitente  " cod-emitente  skip
"nat-operacao  " nat-operacao  skip
"sequencia     " sequencia     skip
"it-codigo     " it-codigo     skip
"qt-do-forn    " qt-do-forn    skip
"preco-total   " preco-total   skip
"desconto      " desconto      skip
"despesas      " despesas      skip
"pr-total-cmi  " pr-total-cmi  skip
"peso-liquido  " peso-liquido  skip
"aliquota-ipi  " aliquota-ipi  skip
"cd-trib-ipi   " cd-trib-ipi   skip
"aliquota-iss  " aliquota-iss  skip
"cd-trib-iss   " cd-trib-iss   skip
"aliquota-icm  " aliquota-icm  skip
"cd-trib-icm   " cd-trib-icm   skip
"char-2        " char-2        skip
"log-2         " log-2         skip
"perc-red-icm  " perc-red-icm  skip
"imp-vBCST     " imp-vBCST     skip
"imp-vICMSST   " imp-vICMSST   skip
    

    VIEW-AS ALERT-BOX INFO BUTTONS OK.

RETURN "ok".
 */

DEFINE BUFFER bf-item-doc-est    FOR item-doc-est.
DEFINE BUFFER bf-docum-est       FOR docum-est.

DEFINE VARIABLE c-aux AS CHARACTER   NO-UNDO.

DEFINE VARIABLE h_boin176 AS HANDLE      NO-UNDO.
DEFINE VARIABLE h_boin090 AS HANDLE      NO-UNDO.

    RUN inbo/boin176.p PERSISTENT SET h_boin176.
    RUN inbo/boin090.p PERSISTENT SET h_boin090.

    
        RUN openQueryStatic IN h_boin176 ("Main":U).
        RUN openQueryStatic IN h_boin090 ("Main":U).
        RUN findDocumento   IN h_boin176 (cod-emitente,
                                          serie-docto,
                                          nro-docto,
                                          nat-operacao).


        
        
        RUN setHandleDocumEst IN h_boin176 (input h_boin090).

        RUN goToKey           IN h_boin176 ( serie-docto ,
                                             nro-docto   ,
                                             cod-emitente,
                                             nat-operacao,
                                             sequencia   ).
        
        

        
        FIND FIRST natur-oper WHERE 
                   natur-oper.nat-operacao = nat-operacao NO-LOCK NO-ERROR.

        RUN findItem IN h_boin176 (INPUT it-codigo).

        
        run findNaturOper IN h_boin176 ( nat-operacao ).
                                 
        /*** Zera o valor de frete na it-docum-est para que seja recalculado***/
                          
        FIND FIRST natur-oper NO-LOCK
            WHERE natur-oper.nat-operacao = nat-operacao NO-ERROR.

        FIND FIRST bf-item-doc-est
             WHERE bf-item-doc-est.serie-docto  = serie-docto 
               AND bf-item-doc-est.nro-docto    = nro-docto   
               AND bf-item-doc-est.cod-emitente = cod-emitente
               AND bf-item-doc-est.nat-operacao = nat-operacao
               AND bf-item-doc-est.sequencia    = sequencia    NO-ERROR.   


 RUN recalculateImposto IN h_boin176 (INPUT qt-do-forn,
                                             INPUT preco-unitario,
                                             INPUT preco-total,
                                             INPUT desconto,
                                             INPUT despesas,
                                             INPUT pr-total-cmi,
                                             INPUT peso-liquido,
                                             INPUT 0, /*novo parametro pedagio*/
                                             INPUT aliquota-ipi,
                                             INPUT cd-trib-ipi,
                                             INPUT aliquota-iss,
                                             INPUT cd-trib-iss,
                                             INPUT aliquota-icm,
                                             INPUT cd-trib-icm,
                                             INPUT char-2, /* perc ipi */
                                             INPUT perc-red-icm, /* perc icm */
                                             INPUT log-2,
                                             INPUT IF AVAIL natur-oper THEN DECI(SUBSTR(natur-oper.char-1,86,1)) ELSE 2, /* cd-trib-pis  */
                                             INPUT IF AVAIL natur-oper THEN natur-oper.perc-pis[1]               ELSE 0, /* val-aliq-pis */
                                             INPUT IF AVAIL natur-oper THEN DECI(SUBSTR(natur-oper.char-1,87,1)) ELSE 2, /* cd-trib-cofins */
                                             INPUT IF AVAIL natur-oper THEN DECI(natur-oper.per-fin-soc[1])      ELSE 0, /* val-aliq-cofins */
                                             INPUT NO).  
        
/*         RUN recalculateImposto IN h_boin176 (INPUT qt-do-forn,                                                                                 */
/*                                              INPUT preco-total,                                                                                */
/*                                              INPUT desconto,                                                                                   */
/*                                              INPUT despesas,                                                                                   */
/*                                              INPUT pr-total-cmi,                                                                               */
/*                                              INPUT peso-liquido,                                                                               */
/*                                              INPUT 0, /*novo parametro pedagio*/                                                               */
/*                                              INPUT aliquota-ipi,                                                                               */
/*                                              INPUT cd-trib-ipi,                                                                                */
/*                                              INPUT aliquota-iss,                                                                               */
/*                                              INPUT cd-trib-iss,                                                                                */
/*                                              INPUT aliquota-icm,                                                                               */
/*                                              INPUT cd-trib-icm,                                                                                */
/*                                              INPUT char-2, /* perc ipi */                                                                      */
/*                                              INPUT aliquota-icm, /* perc icm */                                                                */
/*                                              INPUT perc-red-icm, /* perc icm */                                                                */
/*                                              INPUT log-2,                                                                                      */
/*                                              INPUT IF AVAIL natur-oper THEN DECI(SUBSTR(natur-oper.char-1,86,1)) ELSE 2, /* cd-trib-pis  */    */
/*                                              INPUT IF AVAIL natur-oper THEN natur-oper.perc-pis[1]               ELSE 0, /* val-aliq-pis */    */
/*                                              INPUT IF AVAIL natur-oper THEN DECI(SUBSTR(natur-oper.char-1,87,1)) ELSE 2, /* cd-trib-cofins */  */
/*                                              INPUT IF AVAIL natur-oper THEN DECI(natur-oper.per-fin-soc[1])      ELSE 0, /* val-aliq-cofins */ */
/*                                              INPUT NO).                                                                                        */



        RUN getDecField IN h_boin176 (INPUT "aliquota-icm":U,  OUTPUT c-aux).
        ASSIGN bf-item-doc-est.aliquota-icm     = DECI(c-aux).

        RUN getDecField IN h_boin176 (INPUT "valor-icm":U,  OUTPUT c-aux).
        ASSIGN bf-item-doc-est.valor-icm     = DECI(c-aux).

        RUN getDecField IN h_boin176 (INPUT "base-icm[1]":U,  OUTPUT c-aux).

        RUN getDecField IN h_boin176 (INPUT "aliquota-ipi":U,  OUTPUT c-aux).
        ASSIGN bf-item-doc-est.aliquota-ipi     = DECI(c-aux).
        
        RUN getDecField IN h_boin176 (INPUT "ipi-ntrib[1]":U,  OUTPUT c-aux).
        ASSIGN bf-item-doc-est.ipi-ntrib[1]     = DECI(c-aux).

        ASSIGN bf-item-doc-est.val-perc-red-icms = perc-red-icm.


        RUN getDecField IN h_boin176 (INPUT "cd-trib-icm":U,  OUTPUT c-aux).
        ASSIGN bf-item-doc-est.cd-trib-icm = cd-trib-icm.

        /*
        /*RUN getDecField IN h_boin176 (INPUT "base-subs[1]":U,  OUTPUT c-aux).*/
        ASSIGN bf-item-doc-est.base-subs[1] = imp-vBCST.

        /*RUN getDecField IN h_boin176 (INPUT "vl-subs[1]":U,  OUTPUT c-aux).*/
        ASSIGN bf-item-doc-est.vl-subs[1] = imp-vICMSST. */
                 

        /*Rontan Nota de comercio que divide o IPI por 50%*/

        IF natur-oper.log-2 = YES THEN DO: /*Nota Comercio*/

             RUN getDecField IN h_boin176 (INPUT "base-ipi[1]":U,   OUTPUT c-aux).
             ASSIGN bf-item-doc-est.base-ipi[1]      = DECI(c-aux) / 2.
             ASSIGN bf-item-doc-est.ipi-outras[1]    = DECI(c-aux) - bf-item-doc-est.base-ipi[1].

             RUN getDecField IN h_boin176 (INPUT "valor-ipi[1]":U,  OUTPUT c-aux).
             ASSIGN bf-item-doc-est.valor-ipi[1]     = DECI(c-aux) / 2.


        END.
        ELSE DO:

            RUN getDecField IN h_boin176 (INPUT "ipi-outras[1]":U, OUTPUT c-aux).
            ASSIGN bf-item-doc-est.ipi-outras[1]    = DECI(c-aux).
    
            RUN getDecField IN h_boin176 (INPUT "base-ipi[1]":U,   OUTPUT c-aux).
            ASSIGN bf-item-doc-est.base-ipi[1]      = DECI(c-aux).
    
            RUN getDecField IN h_boin176 (INPUT "valor-ipi[1]":U,  OUTPUT c-aux).
            ASSIGN bf-item-doc-est.valor-ipi[1]     = DECI(c-aux).
    
        END.

        RUN getDecField IN h_boin176 (INPUT "preco-unit[1]":U, OUTPUT c-aux).
        ASSIGN bf-item-doc-est.preco-unit[1]    = DECI(c-aux).

        RUN getDecField IN h_boin176 (INPUT "pr-total-cmi":U,  OUTPUT c-aux).
        ASSIGN bf-item-doc-est.pr-total-cmi     = DECI(c-aux).

        RUN getDecField IN h_boin176 (INPUT "despesas[1]":U,   OUTPUT c-aux).
        ASSIGN bf-item-doc-est.despesas[1]      = DECI(c-aux).

        RUN getDecField IN h_boin176 (INPUT "base-iss[1]":U,   OUTPUT c-aux).
        ASSIGN bf-item-doc-est.base-iss[1]      = DECI(c-aux).

        RUN getDecField IN h_boin176 (INPUT "valor-iss[1]":U,  OUTPUT c-aux).
        ASSIGN bf-item-doc-est.valor-iss[1]     = DECI(c-aux).

        RUN getDecField IN h_boin176 (INPUT "iss-ntrib[1]":U,  OUTPUT c-aux).
        ASSIGN bf-item-doc-est.iss-ntrib[1]     = DECI(c-aux).

        RUN getDecField IN h_boin176 (INPUT "iss-outras[1]":U, OUTPUT c-aux).
        ASSIGN bf-item-doc-est.iss-outras[1]    = DECI(c-aux).

        /* --- Recalcula Imposto da Nota --- */

        ASSIGN bf-item-doc-est.valor-icm[1] = bf-item-doc-est.base-icm[1] * (aliquota-icm / 100)
               bf-item-doc-est.valor-icm[2] = bf-item-doc-est.base-icm[2] * (aliquota-icm / 100).
        
        /*&IF '{&bf_dis_versao_ems}' >= '2.06' &THEN*/
        
             RUN getDecField IN h_boin176 (INPUT "base-pis":U, OUTPUT c-aux).             
             ASSIGN bf-item-doc-est.base-pis                 = DECI(c-aux).               

             RUN getDecField IN h_boin176 (INPUT "valor-pis":U, OUTPUT c-aux).            
             ASSIGN bf-item-doc-est.valor-pis                = DECI(c-aux).               
                                                                                          
             RUN getDecField IN h_boin176 (INPUT "val-base-calc-cofins":U, OUTPUT c-aux). 
             ASSIGN bf-item-doc-est.val-base-calc-cofins     = DECI(c-aux).               
                                                                                          
             RUN getDecField IN h_boin176 (input "val-cofins":U, OUTPUT c-aux). 
             ASSIGN bf-item-doc-est.val-cofins               = DECI(c-aux).               
            
        /*&ELSE
            RUN getCharField IN h_boin176 (INPUT "char-2":U, OUTPUT c-aux).
            
            /* Tributacao PIS */

            IF SUBSTR(c-aux,21,1) = "" THEN ASSIGN SUBSTR(bf-item-doc-est.char-2,21,1) = "2".
                                       ELSE ASSIGN SUBSTR(bf-item-doc-est.char-2,21,1) = SUBSTR(c-aux,21,1).

            /* Tributacao COFINS */

            IF SUBSTR(c-aux,83,1) = "" THEN ASSIGN SUBSTR(bf-item-doc-est.char-2,83,1) = "2".
                                       ELSE ASSIGN SUBSTR(bf-item-doc-est.char-2,83,1) = SUBSTR(c-aux,83,1).
            
            ASSIGN SUBSTR(bf-item-doc-est.char-2,22,5)   = STRING(DECI(SUBSTR(c-aux,22,5)),">9.99":U)               /* aliquota pis     */
                   SUBSTR(bf-item-doc-est.char-2,27,14)  = STRING(DECI(SUBSTR(c-aux,27,14)),">>>,>>>,>>9.99":U)     /* base pis         */
                   SUBSTR(bf-item-doc-est.char-2,41,14)  = STRING(DECI(SUBSTR(c-aux,41,14)),">>>,>>>,>>9.99":U)     /* valor pis        */
                   SUBSTR(bf-item-doc-est.char-2,84,5)   = STRING(DECI(SUBSTR(c-aux,84,5)),">9.99":U)               /* aliquota cofins  */
                   SUBSTR(bf-item-doc-est.char-2,89,14)  = STRING(DECI(SUBSTR(c-aux,89,14)),">>>,>>>,>>9.99":U)     /* base cofins      */
                   SUBSTR(bf-item-doc-est.char-2,103,14) = STRING(DECI(SUBSTR(c-aux,103,14)),">>>,>>>,>>9.99":U).   /* valor cofins     */
        &ENDIF*/


    FOR EACH consist-nota EXCLUSIVE-LOCK
         where consist-nota.cod-emitente = cod-emitente
           and consist-nota.nat-operacao = nat-operacao
           and consist-nota.serie-docto  = serie-docto
           and consist-nota.nro-docto    = nro-docto
           AND consist-nota.mensagem = 5506:

        DELETE consist-nota.

    END.
    


        FIND FIRST bf-docum-est WHERE 
                   bf-docum-est.serie-docto    = serie-docto  AND 
                   bf-docum-est.nro-docto      = nro-docto    AND 
                   bf-docum-est.cod-emitente   = cod-emitente AND 
                   bf-docum-est.nat-operacao   = nat-operacao NO-LOCK NO-ERROR.

        IF AVAIL bf-docum-est THEN 
        DO:
            
            RUN setHandleDocumEst      IN h_boin176 (INPUT h_boin090).
            RUN findDocumento          IN h_boin176 (cod-emitente,
                                                     serie-docto,
                                                     nro-docto,
                                                     nat-operacao).

            RUN getTotalizaNota        IN h_boin176 (INPUT 0).
            RUN transferTotalItensNota IN h_boin176 (INPUT bf-docum-est.cod-emitente,
                                                     INPUT bf-docum-est.serie-docto,
                                                     INPUT bf-docum-est.nro-docto,
                                                     INPUT bf-docum-est.nat-operacao).

            RUN validateValues IN h_boin090.
        END.

    IF VALID-HANDLE(h_boin176) THEN DELETE PROCEDURE h_boin176.
    IF VALID-HANDLE(h_boin090) THEN DELETE PROCEDURE h_boin090.
    
    









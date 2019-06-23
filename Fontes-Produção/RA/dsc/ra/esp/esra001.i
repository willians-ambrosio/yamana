{utp/utapi019.i}
def new Global shared var c-seg-usuario        as char format "x(12)" no-undo.

DEFINE VARIABLE c-erro-descr        AS CHARACTER   NO-UNDO EXTENT 100.
DEFINE VARIABLE p-dt-emissao-nf-aux AS DATE        NO-UNDO.

ASSIGN
    c-erro-descr[  1] = "NFE j† existe."
    c-erro-descr[  2] = "Erro de estrutura XML."
    c-erro-descr[  3] = "Erro estabelecimento."
    c-erro-descr[  4] = "tpAmb Inv†lido."
    c-erro-descr[  5] = "Emitente inativo/inv†lido."
    c-erro-descr[  6] = "Erro chave NFE."
    c-erro-descr[  7] = "Pedido de compra n∆o localizado. <xPed>"
/*     c-erro-descr[  8] = "Saldo no pedido inv†lido." */
    c-erro-descr[  8] = "Saldo da ordem de compra inv†lida."
    c-erro-descr[  9] = "Preáo unit†rio difere da ordem de compra. <vUnCom>"
    c-erro-descr[ 10] = "Ordem de compra possui pendància de aprovaá∆o."
    c-erro-descr[ 11] = "Erro na Triagem R.A." /* --- Esse erro somente Ç retornado na execuá∆o batch --- */
    /* Begins: V03 -> Inclus∆o de validaá∆o x mensagem que ser∆o enviados por email
       Data: 23/05/2018 */
    c-erro-descr[ 12] = "Prazo de Compra com situaá∆o confirmada, n∆o encontrado."
    /* End: V03 */
    c-erro-descr[100] = "Erro no processamento do XML."
    .

FUNCTION fc-saldo-ordem RETURNS DECIMAL
  ( p-numero-ordem AS INT , p-parcela AS INT , p_rw_item AS ROWID) FORWARD.


PROCEDURE p-bloq-rec:

    DEFINE INPUT  PARAMETER ipch-chave-acesso-nfe   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER p-msg-error             AS CHARACTER   NO-UNDO.

    FIND FIRST nfe-dfe WHERE nfe-dfe.chave-acesso = ipch-chave-acesso-nfe EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE nfe-dfe THEN nfe-dfe.sit-triagem = 4. /*Bloqueado*/
                                                                                                                           
    CREATE nfe-his-nota-fis-rec.                                                                                         
    ASSIGN 
        nfe-his-nota-fis-rec.chave-acesso-nfe = ipch-chave-acesso-nfe
        nfe-his-nota-fis-rec.codigo           = 99                                                                    
        nfe-his-nota-fis-rec.desc-status      = "Documento BLOQUEADO pela Rotina de Triagem do RA (esra001a)"
        nfe-his-nota-fis-rec.dt-retorno       = TODAY                                                                 
        nfe-his-nota-fis-rec.hr-retorno       = STRING(TIME,"HH:MM:SS")                                               
        nfe-his-nota-fis-rec.dt-Recbto        = TODAY                                                                 
        nfe-his-nota-fis-rec.hr-Recbto        = STRING(TIME,"HH:MM:SS")                                               
        nfe-his-nota-fis-rec.nr-protocolo     = "".                                                                   
    
END PROCEDURE.


PROCEDURE p-valida-pedido:
    DEFINE INPUT  PARAMETER ipch-xped-char      AS CHARACTER   NO-UNDO. /* "xPed XML"       */
    DEFINE INPUT  PARAMETER ipch-xped-char-aux  AS CHARACTER   NO-UNDO. /* "xPed"           */
    DEFINE INPUT  PARAMETER ipin-parcela        AS INTEGER     NO-UNDO. /* "nItemPed"       */
    DEFINE OUTPUT PARAMETER op-erro             AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER pMsgError           AS CHARACTER   NO-UNDO.
    
    DEFINE VARIABLE lg-valido AS LOGICAL     NO-UNDO.


    ASSIGN op-erro   = 0
           pMsgError = "".

    lg-valido =
        /*
        CAN-FIND(       prazo-compra 
                 WHERE  prazo-compra.numero-ordem = INTEGER(ipch-xped-char-aux) 
                 AND    prazo-compra.parcela      = ipin-parcela)
        OR
        */
        CAN-FIND(FIRST  ordem-compra
                 WHERE  ordem-compra.numero-ordem = INTEGER(ipch-xped-char-aux)
                 )
        /*
        OR
        CAN-FIND(FIRST  pedido-compr
                 WHERE  pedido-compr.num-pedido   = INTEGER(ipch-xped-char-aux)
                 )
        */
        .
    
    IF NOT lg-valido THEN 
       ASSIGN op-erro  = 7 /* --- Pedido/Ordem de Compra n∆o localizado --- */
              pMsgError = "ORDEM DE COMPRA DO XML = " + STRING(ipch-xped-char).
    
END PROCEDURE.

PROCEDURE p-ver-saldo:
    DEFINE INPUT  PARAMETER ipch-xped-char-aux      AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipin-parcela            AS INTEGER     NO-UNDO. /* "nItemPed"       */
    DEFINE INPUT  PARAMETER iprw-item               AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER ipde-item-qCom          AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER ipde-preco-unit         AS DECIMAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER op-erro                 AS INTEGER     NO-UNDO EXTENT 2.
    /* Begins: 15/05/2018 - Inclus∆o do campo parcela para retornar ao executar a rotina de saldo */
    DEFINE OUTPUT PARAMETER pCodParcela             AS INTEGER     INITIAL 0 NO-UNDO.
    /* end 15/05/2018 */
    /* Begins REV03 */
    DEFINE OUTPUT PARAMETER pMsgError               AS CHARACTER   NO-UNDO.
    /* end REV03 */

    DEFINE VARIABLE de-saldo-ra                     AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE de-qtde-saldo-forn              AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE de-saldo-tot                    AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE h_bonfe001                      AS HANDLE      NO-UNDO.
    DEFINE VARIABLE i-numero-ordem                  AS INTEGER     NO-UNDO.
    DEFINE VARIABLE de-quant-variacao               AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE l-carregado                     AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE de-vl-pr-unit                   LIKE ordem-compra.preco-unit     NO-UNDO.
    DEFINE VARIABLE de-valor-a                      AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE de-valor-b                      AS DECIMAL     NO-UNDO.

    ASSIGN de-valor-a = 0
           de-valor-b = 0.


    RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001.
    RUN pi_saldo_ordem_nfe IN h_bonfe001(INPUT  INTEGER(ipch-xped-char-aux), /* ordem-compra.numero-ordem */
                                         INPUT  ipin-parcela               ,
                                         INPUT  iprw-item                  ,
                                         OUTPUT de-saldo-ra                  /* Saldo do item atualizado no RA */
                                         ).
    DELETE PROCEDURE h_bonfe001.    

    i-numero-ordem = INTEGER(ipch-xped-char-aux).

    ASSIGN pMsgError = "".

    /* --- Valida o Saldo no pedido  --- */
    FIND ordem-compra WHERE
         ordem-compra.numero-ordem = i-numero-ordem NO-LOCK NO-ERROR.
       
    FOR EACH prazo-compra NO-LOCK
      WHERE prazo-compra.numero-ordem = i-numero-ordem
        AND prazo-compra.situacao     = 2 /* --- Confirmada --- */ :

        IF (prazo-compra.quant-saldo - prazo-compra.dec-1 - fc-saldo-ordem(i-numero-ordem, prazo-compra.parcela, iprw-item)) <= 0 THEN NEXT.

        de-qtde-saldo-forn = de-qtde-saldo-forn + (prazo-compra.quant-saldo - prazo-compra.dec-1 - (fc-saldo-ordem(i-numero-ordem, prazo-compra.parcela, iprw-item) ) ).
    END.
    
    ASSIGN de-vl-pr-unit = ordem-compra.preco-unit
           de-saldo-tot  = de-qtde-saldo-forn.  /*- de-saldo-ra. nao precisa tirar novamente o saldo do ra, na de-qtde-saldo-forn ele ja tira via funá∆o fc-saldo-ordem*/  

    IF ordem-compra.mo-codigo > 0 THEN
       RUN "cdp/cd0812.p"     (INPUT ordem-compra.mo-codigo,
                               INPUT 0,
                               INPUT de-vl-pr-unit,
                               INPUT p-dt-emissao-nf-aux,
                               OUTPUT de-vl-pr-unit).

    IF de-saldo-tot    < ipde-item-qCom          THEN 
       ASSIGN op-erro[1] = 8 /* --- Saldo na ordem  inv†lido                   --- */
              /* Begins: V03 */
              pMsgError  = "Saldo RA...: "  + STRING(de-saldo-ra)        + CHR(10) +
                           "Saldo Ordem: "  + STRING(de-qtde-saldo-forn) + CHR(10) +
                           "QTD XML....: "  + STRING(ipde-item-qCom)     + CHR(10) +
                           "Ordem XML..: "  + STRING(ipch-xped-char-aux).
              /* End: V03 */ 
    ELSE
    DO:   

/*         IF ordem-compra.mo-codigo = 0 THEN */
            ASSIGN de-valor-a = round(ipde-preco-unit, 2) 
                   de-valor-b = round(de-vl-pr-unit  , 2).
/*         ELSE                                                               */
/*             ASSIGN de-valor-a = round(ipde-preco-unit, 5)                  */
/*                    de-valor-b = round(de-vl-pr-unit  , 5).                 */
/*                                                                            */
            /*Variaá∆o de valor*/

            FIND FIRST item-mat  NO-LOCK
                WHERE item-mat.it-codigo = ordem-compra.it-codigo NO-ERROR.

            IF AVAIL item-mat THEN
                ASSIGN de-valor-b = de-valor-b + item-mat.lim-var-valor.


/*             MESSAGE                                                                    */
/*                     "ordem-compra.mo-codigo com variaá∆o"  ordem-compra.mo-codigo SKIP */
/*                     "de-valor-a " de-valor-a SKIP                                      */
/*                     "de-valor-b " de-valor-b                                           */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK.                                     */

        IF de-valor-a > de-valor-b THEN 
           ASSIGN op-erro[2] = 9 /* --- Preáo unit†rio difere da Ordem de Compra   --- */
                  /* Begins: V03 */
                  pMsgError  = "Preáo Ordem: "  + STRING(ordem-compra.preco-unit) + CHR(10) +
                               "Preáo XML..: "  + STRING(ipde-preco-unit)         + CHR(10) +
                               "Ordem XML..: "  + STRING(ipch-xped-char-aux). 
                  /* End: V03 */ 
    END.

    IF pMsgError <> "" THEN NEXT.    
    /* begins: 04/05/2018 ------------------------------------------------------------------------------ */
    ASSIGN de-quant-variacao = 0
           l-carregado       = NO.

    /* --- Valida Variacao --- */
    FIND FIRST param-re WHERE 
               param-re.usuario = c-seg-usuario NO-LOCK NO-ERROR.
    IF AVAIL param-re            AND
       param-re.aceita-var = YES THEN 
    DO:

        FIND FIRST item-uni-estab WHERE 
                   item-uni-estab.it-codigo    = ordem-compra.it-codigo          AND 
                   item-uni-estab.cod-estabel  = nfe-nota-fiscal-rec.cod-estabel NO-LOCK NO-ERROR.
        IF AVAIL item-uni-estab THEN DO:

            IF item-uni-estab.var-qtd-re <> 0 THEN 
                ASSIGN de-quant-variacao = nfe-it-nota-fisc-rec.item-qtde * (item-uni-estab.var-qtd-re / 100) /* variaá∆o percentual */
                       l-carregado       = YES.            

            IF item-uni-estab.lim-var-qtd <> 0 THEN
                ASSIGN de-quant-variacao    = item-uni-estab.lim-var-qtd /* variaá∆o qtde */
                       l-carregado          = YES.

            IF l-carregado = NO THEN 
            DO:

                FIND FIRST item-mat no-lock
                    WHERE item-mat.it-codigo = ordem-compra.it-codigo NO-ERROR.
                IF AVAIL item-mat THEN 
                DO:

                    IF item-mat.var-qtd-re <> 0 THEN
                        ASSIGN de-quant-variacao = nfe-it-nota-fisc-rec.item-qtde * (item-mat.var-qtd-re / 100). /* variaá∆o percentual */

                    IF item-mat.lim-var-qtd <> 0 THEN
                        ASSIGN de-quant-variacao = item-mat.lim-var-qtd. /* variaá∆o qtde */
                END.
            END.
        END.
    END.

    FIND FIRST prazo-compra WHERE 
               prazo-compra.numero-ordem     = i-numero-ordem              AND
               prazo-compra.situacao         = 2 /* --- Confirmada --- */  AND
              (prazo-compra.quant-saldo - prazo-compra.dec-1) >= ( nfe-it-nota-fisc-rec.item-qtde - de-quant-variacao) NO-LOCK NO-ERROR.
    IF NOT AVAIL prazo-compra THEN
    DO:
        ASSIGN pCodParcela = 0.

        FIND FIRST prazo-compra WHERE 
                   prazo-compra.numero-ordem     = i-numero-ordem  AND   
                   prazo-compra.situacao         = 2 /* --- Confirmada --- */ NO-LOCK NO-ERROR.
        IF NOT AVAIL prazo-compra THEN
           ASSIGN op-erro[1]  = 12.                   
        ELSE
           ASSIGN op-erro[1] = 8
                  /* Begins: V03 */
                  pMsgError = "Qtd Variaá∆o...............: "  + STRING(de-quant-variacao)              + CHR(10) +
                              "Qtd XML....................: "  + STRING(nfe-it-nota-fisc-rec.item-qtde) + CHR(10) +
                              "Qtd Prazo Compra Confirmada: "  + STRING(prazo-compra.quant-saldo - prazo-compra.dec-1) + CHR(10) + 
                              "Ordem XML..................: "  + STRING(ipch-xped-char-aux).
                  /* End: V03 */
    END.
    /* Begins: 15/05/2018 - Inclus∆o do campo parcela para retornar ao executar a rotina de saldo */
    ELSE
       ASSIGN pCodParcela = prazo-compra.parcela.
    /* end: 15/05/2018 */
    /* end: 04/05/2018 ------------------------------------------------------------------------------ */    
END PROCEDURE.


PROCEDURE pi-email:
    DEFINE INPUT  PARAMETER ipch-chave-acesso-nfe   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipch-nome-abrev         AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipch-remetente          AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipch-destinatario       AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipch-cod-estabel        AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipch-ide-Serie          AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipch-ide-nNf            AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipch-motivo-bloqueio    AS CHARACTER   NO-UNDO.    
                                                    
    DEFINE VARIABLE c-destino                       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE c-fornecedor                    AS CHARACTER   NO-UNDO.

    FIND FIRST param-global NO-LOCK NO-ERROR.

    EMPTY TEMP-TABLE tt-envio2  .
    EMPTY TEMP-TABLE tt-mensagem.
    EMPTY TEMP-TABLE tt-erros   .

    FIND emitente WHERE emitente.nome-abrev = ipch-nome-abrev NO-LOCK NO-ERROR.
    IF AVAILABLE emitente THEN
       ASSIGN c-fornecedor = STRING(emitente.cod-emitente) + " - " + emitente.nome-abrev.

    IF OPSYS = "win32" THEN DO:

        RUN utp/utapi019.p PERSISTENT SET h-utapi019.
        
        CREATE tt-envio2.
        ASSIGN 
            tt-envio2.versao-integracao = 1
            tt-envio2.servidor          = param-global.serv-mail
            tt-envio2.porta             = param-global.porta-mail
            tt-envio2.destino           = ipch-destinatario
            tt-envio2.remetente         = ipch-remetente
            tt-envio2.assunto           = "Triagem do Recebimento Autom†tico de NFE"
            tt-envio2.importancia       = 2 /* --- (S¢ influencia quando utilizado servidor Exchange) 0 = Prioridade Baixa, 1 = Prioridade Normal, 2 = Prioridade Alta. --- */
        NO-ERROR.

        CREATE tt-mensagem.
        ASSIGN 
            tt-mensagem.seq-mensagem = 1
            tt-mensagem.mensagem     = 
                                    "RA - Nota Fiscal Eletronica Bloqueada."             + CHR(13) + CHR(13) +
                                    "ESTABELECIMENTO..: " + ipch-cod-estabel             + CHR(13) +
                                    "SERIE............: " + ipch-ide-Serie               + CHR(13) +
                                    "NR.NOTA FISCAL...: " + ipch-ide-nNf                 + CHR(13) +
                                    "FORNECEDOR.......: " + c-fornecedor                 + CHR(13) +
                                    "CHAVE DE ACESSO..: " + TRIM(ipch-chave-acesso-nfe)  + CHR(13) + CHR(13) +
                                    ipch-motivo-bloqueio.


/*         MESSAGE                                         */
/*             "Email Remente: " tt-envio2.remetente  SKIP */
/*             "Email Destino: " tt-envio2.destino SKIP    */
/*             "Assunto: " tt-envio2.assunto SKIP          */
/*             "Mensagem " tt-mensagem.mensagem            */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.          */

        RUN pi-execute2 IN h-utapi019 ( INPUT  TABLE tt-envio2,
                                        INPUT  TABLE tt-mensagem,
                                        OUTPUT TABLE tt-erros).
        DELETE PROCEDURE h-utapi019.

    END.
    ELSE DO: /* unix */

        ASSIGN c-destino = TRIM(ipch-destinatario) +
                           TRIM(ipch-cod-estabel ) + "_" +
                           TRIM(ipch-ide-Serie   ) + "_" +
                           TRIM(ipch-ide-nNf     ) + ".lst".

        OUTPUT TO VALUE(c-destino).

        PUT "RA - Nota Fiscal Eletronica Bloqueada."               + CHR(13) + CHR(13) +
            "ESTABELECIMENTO..: " + ipch-cod-estabel               + CHR(13) +
            "SERIE............: " + ipch-ide-Serie                 + CHR(13) +
            "NR.NOTA FISCAL...: " + ipch-ide-nNf                   + CHR(13) +
            "FORNECEDOR.......: " + c-fornecedor                   + CHR(13) +
            "CHAVE DE ACESSO..: " + TRIM(ipch-chave-acesso-nfe)    + CHR(13) + CHR(13) +
            ipch-motivo-bloqueio. 

        OUTPUT TO CLOSE.        

        UNIX SILENT mail -s "RA - NFe Bloqueada" -c VALUE(nfe-it-param-rec.valor-3-item-parametro) < VALUE(c-destino).
    END.

END PROCEDURE.


PROCEDURE p-aprovacao:
    DEFINE INPUT  PARAMETER ipch-it-codigo      AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipch-xped-char-aux  AS CHARACTER   NO-UNDO. /* "xPed"           */
    DEFINE INPUT  PARAMETER ipin-num-pedido     AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER op-erro             AS INTEGER     NO-UNDO.
    /* Begins REV03 */
    DEFINE OUTPUT PARAMETER pMsgError           AS CHARACTER   NO-UNDO.
    /* end REV03 */

    FOR FIRST ordem-compra FIELDS (ordem-compra.it-codigo                                  
                                   ordem-compra.num-pedido                                 
                                   ordem-compra.situacao                                   
                                   ordem-compra.numero-ordem) NO-LOCK 
        WHERE ordem-compra.it-codigo  = ipch-it-codigo  AND
              ordem-compra.num-pedido = ipin-num-pedido AND
              ordem-compra.situacao   = 2 /* --- Confirmada --- */
        :
        
        FOR FIRST doc-pend-aprov FIELDS(doc-pend-aprov.numero-ordem                        
                                        doc-pend-aprov.ind-situacao) NO-LOCK 
            WHERE doc-pend-aprov.numero-ordem = ordem-compra.numero-ordem AND              
                 (doc-pend-aprov.ind-situacao = 1 OR doc-pend-aprov.ind-situacao = 3):
            ASSIGN op-erro   = 10 /* --- Ordem de Compra possui pendància de aprovaá∆o --- */
                   pMsgError = "ORDEM DE COMPRA " + STRING(doc-pend-aprov.numero-ordem).

        END.
    END.

    IF NOT AVAIL ordem-compra THEN
    DO:
        ASSIGN op-erro   = 7
               pMsgError = "ORDEM DE COMPRA da Tag <xPed> = " + STRING(ipch-xped-char-aux) + " Inv†lido e/ou n∆o localizado.".
    END.
    
END PROCEDURE.


FUNCTION fc-saldo-ordem RETURNS DECIMAL
  (INPUT p-numero-ordem AS INTEGER , INPUT p-parcela AS INTEGER , INPUT p_rw_item AS ROWID) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE de-quant AS DECIMAL NO-UNDO.

    DEFINE BUFFER bf-nfe-nota-fiscal-rec  FOR nfe-nota-fiscal-rec.
    DEFINE BUFFER bf-nfe-it-nota-fisc-rec FOR nfe-it-nota-fisc-rec.
        
    FIND nfe-it-nota-fisc-rec WHERE ROWID(nfe-it-nota-fisc-rec) = p_rw_item NO-LOCK NO-ERROR.
    IF AVAILABLE nfe-it-nota-fisc-rec  THEN DO:
        
        FOR EACH  bf-nfe-it-nota-fisc-rec NO-LOCK
            WHERE bf-nfe-it-nota-fisc-rec.item-num-ordem = p-numero-ordem
            AND   bf-nfe-it-nota-fisc-rec.item-parcela   = p-parcela
            :
            
            FIND FIRST bf-nfe-nota-fiscal-rec OF bf-nfe-it-nota-fisc-rec NO-LOCK NO-ERROR.
            FIND FIRST nfe-dfe NO-LOCK
                WHERE nfe-dfe.chave-acesso = bf-nfe-nota-fiscal-rec.chave-acesso-nfe NO-ERROR.
            IF NOT AVAILABLE nfe-dfe OR nfe-dfe.sit-erp >= 2 
            THEN NEXT.                            

            IF bf-nfe-it-nota-fisc-rec.chave-acesso-nfe = nfe-it-nota-fisc-rec.chave-acesso-nfe AND
               bf-nfe-it-nota-fisc-rec.seq-item         = nfe-it-nota-fisc-rec.seq-item 
            THEN NEXT.

            de-quant = de-quant + DEC(bf-nfe-it-nota-fisc-rec.item-qtde).    
        END.

        FOR EACH  nfe-relac-ordem-rec NO-LOCK
            WHERE nfe-relac-ordem-rec.numero-ordem =  p-numero-ordem
            AND   nfe-relac-ordem-rec.parcela-oc   =  p-parcela
            :

            FIND FIRST bf-nfe-nota-fiscal-rec WHERE 
                       bf-nfe-nota-fiscal-rec.chave-acesso-nfe = nfe-relac-ordem-rec.chave-acesso-nfe 
                       NO-LOCK NO-ERROR.
            
            FIND FIRST nfe-dfe WHERE 
                       nfe-dfe.chave-acesso = bf-nfe-nota-fiscal-rec.chave-acesso-nfe 
                       NO-LOCK NO-ERROR.
            IF NOT AVAILABLE nfe-dfe OR nfe-dfe.sit-erp >= 2 THEN NEXT.                                                

            IF nfe-relac-ordem-rec.chave-acesso-nfe = nfe-it-nota-fisc-rec.chave-acesso-nfe AND 
               nfe-relac-ordem-rec.seq-item         = nfe-it-nota-fisc-rec.seq-item        
            THEN NEXT.
           
            de-quant = de-quant + DEC(nfe-relac-ordem-rec.quantidade).                        
       END.        
                                                                                        
       FIND FIRST bf-nfe-nota-fiscal-rec OF nfe-it-nota-fisc-rec NO-LOCK NO-ERROR.
       FIND FIRST nfe-dfe WHERE 
                  nfe-dfe.chave-acesso = bf-nfe-nota-fiscal-rec.chave-acesso-nfe NO-LOCK NO-ERROR.
       IF AVAIL nfe-dfe THEN
          ASSIGN p-dt-emissao-nf-aux =  nfe-dfe.dt-emissao.
    END.  
    
    RETURN de-quant.

END FUNCTION.

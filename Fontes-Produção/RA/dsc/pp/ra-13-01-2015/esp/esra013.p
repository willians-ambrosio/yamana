/*****************************************************************************
**  Programa: esra013.p
**     Autor: 
**      Data: 
** Descricao: Sugestoes para itens no momento de carregar a NFE
** Alteracao: 

******************************************************************************/

{include\i-epc200.i esra013}                                                   
                                                   
DEFINE INPUT PARAMETER p-row-item AS ROWID NO-UNDO.

DEFINE VARIABLE de-fat-conv     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE c-xped-char-aux AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-contador      AS INTEGER     NO-UNDO.






FIND FIRST nfe-it-nota-fisc-rec EXCLUSIVE-LOCK
    WHERE ROWID(nfe-it-nota-fisc-rec) = p-row-item NO-ERROR.


IF AVAIL nfe-it-nota-fisc-rec THEN DO:

    FIND FIRST nfe-nota-fiscal-rec NO-LOCK
        WHERE nfe-nota-fiscal-rec.chave-acesso-nfe = nfe-it-nota-fisc-rec.chave-acesso-nfe NO-ERROR.

    FIND FIRST emitente NO-LOCK
        WHERE emitente.nome-abrev = nfe-nota-fiscal-rec.nome-abrev NO-ERROR.

    FIND FIRST item-fornec no-lock
        WHERE item-fornec.cod-emitente = emitente.cod-emitente        
        AND   item-fornec.item-do-forn = nfe-it-nota-fisc-rec.item-cprod NO-ERROR.

    /*Pegando Item-fornec e fator de conversao*/
    IF AVAIL item-fornec THEN DO:
        
        IF NOT CAN-FIND(nfe-item-fornec OF item-fornec) THEN DO:
            CREATE nfe-item-fornec.
            BUFFER-COPY item-fornec TO nfe-item-fornec. 

        END.
        
        FIND FIRST nfe-item-fornec OF item-fornec NO-LOCK NO-ERROR.
            
        ASSIGN de-fat-conv                      = nfe-item-fornec.fator-conver * EXP(10,(nfe-item-fornec.num-casa-dec * (-1)))
               nfe-it-nota-fisc-rec.item-qtde   = nfe-it-nota-fisc-rec.item-qCom / de-fat-conv
               nfe-it-nota-fisc-rec.it-codigo   = nfe-item-fornec.it-codigo
               nfe-it-nota-fisc-rec.item-vUnCom = nfe-it-nota-fisc-rec.item-vProd / nfe-it-nota-fisc-rec.item-qtde
               nfe-it-nota-fisc-rec.item-uCom   = nfe-item-fornec.unid-med-for.

    END.

    /* --- Busca Localizacao --- */
    RUN dsc/ra/esp/esra010.p(INPUT  nfe-it-nota-fisc-rec.it-codigo,
                             INPUT  nfe-nota-fiscal-rec.cod-estabel,
                             INPUT  0,
                             OUTPUT nfe-it-nota-fisc-rec.item-cod-depos, 
                             OUTPUT nfe-it-nota-fisc-rec.item-cod-localiz  ).


    /* --- Busca Relacionamento Emitente x CFOP x Natureza --- */
    FIND FIRST nfe-emit-cfop-nat no-lock
        WHERE nfe-emit-cfop-nat.cod-estabel  = nfe-nota-fiscal-rec.cod-estabel        
        and   nfe-emit-cfop-nat.cod-emitente = emitente.cod-emitente                  
        and   nfe-emit-cfop-nat.cod-cfop     = STRING(nfe-it-nota-fisc-rec.item-CFOP) 
        and   nfe-emit-cfop-nat.it-codigo    = nfe-it-nota-fisc-rec.it-codigo          NO-ERROR.
        
    IF NOT AVAIL nfe-emit-cfop-nat THEN DO:
        
        FIND FIRST nfe-emit-cfop-nat NO-LOCK
             WHERE nfe-emit-cfop-nat.cod-estabel  = nfe-nota-fiscal-rec.cod-estabel        
             and   nfe-emit-cfop-nat.cod-emitente = emitente.cod-emitente                  
             and   nfe-emit-cfop-nat.cod-cfop     = STRING(nfe-it-nota-fisc-rec.item-CFOP) 
             and   nfe-emit-cfop-nat.it-codigo    = "*"                                    NO-ERROR.

    END.
    
    IF AVAIL nfe-emit-cfop-nat                           AND
       TRIM(nfe-it-nota-fisc-rec.item-nat-operacao) = "" THEN DO:
        
        FIND FIRST natur-oper no-lock
            WHERE natur-oper.nat-operacao = nfe-emit-cfop-nat.nat-operacao NO-ERROR.
            
        IF AVAIL natur-oper THEN DO:
            
            ASSIGN nfe-it-nota-fisc-rec.item-nat-operacao = natur-oper.nat-operacao
                   nfe-it-nota-fisc-rec.item-esp-nat      = natur-oper.especie-doc.

        END.

    END.

    /*Ordem e Parcela*/
    ASSIGN c-xped-char-aux = ''.

    DO i-contador = 1 TO LENGTH(nfe-it-nota-fisc-rec.item-xped-char):

       IF (ASC(SUBSTRING(nfe-it-nota-fisc-rec.item-xped-char,i-contador,1)) >= 48) AND
          (ASC(SUBSTRING(nfe-it-nota-fisc-rec.item-xped-char,i-contador,1)) <= 57) THEN 
             ASSIGN c-xped-char-aux = c-xped-char-aux + (SUBSTRING(nfe-it-nota-fisc-rec.item-xped-char,i-contador,1)).
    END.

    FIND FIRST prazo-compra NO-LOCK
        WHERE prazo-compra.numero-ordem = int(c-xped-char-aux) /*int(nfe-it-nota-fisc-rec.item-xPed-Char)*/
        AND   prazo-compra.parcela      = nfe-it-nota-fisc-rec.item-nItemPEd  NO-ERROR.

    IF AVAIL prazo-compra  THEN DO:

        ASSIGN nfe-it-nota-fisc-rec.item-num-ordem = prazo-compra.numero-ordem
               nfe-it-nota-fisc-rec.item-parcela   = prazo-compra.parcela     .
    END.
    ELSE DO:

        FIND FIRST ordem-compra NO-LOCK
            WHERE ordem-compra.numero-ordem = int(c-xped-char-aux)/*int(nfe-it-nota-fisc-rec.item-xPed-Char)*/ NO-ERROR.
        IF AVAIL ordem-compra THEN
            ASSIGN nfe-it-nota-fisc-rec.item-num-ordem = ordem-compra.numero-ordem.


    END.




    /*Ponto de UPC*/
    EMPTY TEMP-TABLE tt-epc.

    CREATE tt-epc.
        
    ASSIGN tt-epc.cod-event     = "SugestaoItemCarrega"
           tt-epc.cod-parameter = "nfe-it-nota-fisc-rec(ROWID)"
           tt-epc.val-parameter = STRING(ROWID(nfe-it-nota-fisc-rec)).
    
    {include\i-epc201.i "SugestaoItemCarrega"}
    
END.

RETURN "OK".


                
                
            
                





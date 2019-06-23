/*****************************************************************************
**  Programa: esnfe2001z-u01.p
**     Autor: Alberto Duzi
**      Data: 13/10/2010
** Descricao: UPC na Atualiza?∆o de Documento do F°sico para o Fiscal
** Altera?∆o: Mod.1 - Baione - Quando existir ordens relacionadas aos itens 
**            Alterar o valor de cada item de cada item de acordo com a quanti-
**            dade e o valor unitario.
             
**            Lea - 26/09/14 - Verifica a versao e se for totvs11 atualiza a 
**            chave de acesso no novo campo docum-est.cod-chave-aces-nf-eletro      
******************************************************************************/
{dsc\ra\include\variaveis-nfe-receb.i}
{dsc\ra\include\fc-handle-obj.i}
{dsc\ra\include\fc-falso.i}
{dsc\ra\include\buffers.i}
{inbo\boin176.i  tt-item-doc-est}
{cdp\cdcfgdis.i} /* --- Definicao de Vers∆o do EMS --- */

DEFINE INPUT PARAM p-ind-event      AS CHAR             NO-UNDO.
DEFINE INPUT PARAM p-ind-object     AS CHAR             NO-UNDO.
DEFINE INPUT PARAM p-wgh-object     AS HANDLE           NO-UNDO.
DEFINE INPUT PARAM p-wgh-frame      AS WIDGET-HANDLE    NO-UNDO.
DEFINE INPUT PARAM p-cod-table      AS CHAR             NO-UNDO.
DEFINE INPUT PARAM p-row-table      AS ROWID            NO-UNDO.

DEFINE NEW GLOBAL SHARED VAR wh-re2001z-c-cgc           AS WIDGET-HANDLE    NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wh-re2001z-serie-docto     AS WIDGET-HANDLE    NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wh-re2001z-i-nro-docto     AS WIDGET-HANDLE    NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wh-re2001z-nat-operacao    AS WIDGET-HANDLE    NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wh-re2001z-cb-cod-observa  AS WIDGET-HANDLE    NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wh-re2001z-btOk            AS WIDGET-HANDLE    NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wh-re2001z-btSave          AS WIDGET-HANDLE    NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wh-re2001z-btOk-falso      AS WIDGET-HANDLE    NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wh-re2001z-btSave-falso    AS WIDGET-HANDLE    NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR c-seg-usuario              AS CHAR     NO-UNDO.
    
/*DISABLE TRIGGERS FOR LOAD OF item-doc-est.*/

{inbo\boin176.i item-doc-est}

/*MESSAGE "p-ind-event..:" p-ind-event                  SKIP
        "p-ind-object.:" p-ind-object                 SKIP
        "p-cod-table..:" STRING(p-cod-table)          SKIP
        "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    SKIP
        "p-wgh-frame..:" STRING(p-wgh-frame)          SKIP
       VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

FIND FIRST nfe-it-param-rec NO-LOCK
     WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
       AND nfe-it-param-rec.cod-item-parametro     = 'CEST'
       AND nfe-it-param-rec.valor-1-item-parametro = "SIM" NO-ERROR.
ASSIGN l-cest = AVAIL nfe-it-param-rec.     
    
FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
    AND   nfe-it-param-rec.cod-item-parametro     = 'Multi_natureza' 
    AND   nfe-it-param-rec.valor-1-item-parametro = "SIM" NO-ERROR.
ASSIGN l-multi_natureza = AVAIL nfe-it-param-rec.

FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
    AND   nfe-it-param-rec.cod-item-parametro     = 'soma_desconto_preco' 
    AND   nfe-it-param-rec.valor-1-item-parametro = "SIM" NO-ERROR.
ASSIGN l-soma-desconto-preco = AVAIL nfe-it-param-rec.

FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
    AND   nfe-it-param-rec.cod-item-parametro     = 'CST_ICMS' 
    AND nfe-it-param-rec.valor-1-item-parametro = "XML" NO-ERROR. 
ASSIGN l-CST-ICMS-xml = AVAIL nfe-it-param-rec.

FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
    AND   nfe-it-param-rec.cod-item-parametro     = 'CST_ICMS' 
    AND nfe-it-param-rec.valor-1-item-parametro = "ERP" NO-ERROR. 
ASSIGN l-CST-ICMS-erp = AVAIL nfe-it-param-rec. /*para apenas calcular como ERP o cliente que escrever ERP*/

FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
      AND nfe-it-param-rec.cod-item-parametro     = 'orig_aliq_IPI_dev'
      AND nfe-it-param-rec.valor-1-item-parametro = "XML"  NO-ERROR.
ASSIGN l-IPI-dev-xml = AVAIL nfe-it-param-rec.

FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
      AND nfe-it-param-rec.cod-item-parametro     = 'orig_aliq_IPI'
      AND nfe-it-param-rec.valor-1-item-parametro = "XML"  NO-ERROR.
ASSIGN l-IPI-xml = AVAIL nfe-it-param-rec.

FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
      AND nfe-it-param-rec.cod-item-parametro     = 'orig_aliq_PIS_dev'
      AND nfe-it-param-rec.valor-1-item-parametro = "XML"  NO-ERROR.
ASSIGN l-PIS-dev-xml = AVAIL nfe-it-param-rec.

FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
      AND nfe-it-param-rec.cod-item-parametro     = 'orig_aliq_PIS'
      AND nfe-it-param-rec.valor-1-item-parametro = "XML"  NO-ERROR.
ASSIGN l-PIS-xml = AVAIL nfe-it-param-rec.

FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
      AND nfe-it-param-rec.cod-item-parametro     = 'orig_aliq_COFINS_dev'
      AND nfe-it-param-rec.valor-1-item-parametro = "XML"  NO-ERROR.
ASSIGN l-COFINS-dev-xml = AVAIL nfe-it-param-rec.

FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
      AND nfe-it-param-rec.cod-item-parametro     = 'orig_aliq_COFINS'
      AND nfe-it-param-rec.valor-1-item-parametro = "XML"  NO-ERROR.
ASSIGN l-COFINS-xml = AVAIL nfe-it-param-rec.

FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
      AND nfe-it-param-rec.cod-item-parametro     = 'orig_aliq_ICMS_dev'
      AND nfe-it-param-rec.valor-1-item-parametro = "XML"  NO-ERROR.
ASSIGN l-ICMS-dev-xml = AVAIL nfe-it-param-rec.

FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
      AND nfe-it-param-rec.cod-item-parametro     = 'orig_aliq_ICMS'
      AND nfe-it-param-rec.valor-1-item-parametro = "XML"  NO-ERROR.
ASSIGN l-ICMS-xml = AVAIL nfe-it-param-rec.
    
FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
      AND nfe-it-param-rec.cod-item-parametro     = 'orig_aliq_ICMS_ST_dev'
      AND nfe-it-param-rec.valor-1-item-parametro = "XML"  NO-ERROR.
ASSIGN l-ICMS-ST-dev-xml = AVAIL nfe-it-param-rec.
    
FIND FIRST nfe-it-param-rec NO-LOCK
    WHERE nfe-it-param-rec.cod-parametro          = 'param_global'
      AND nfe-it-param-rec.cod-item-parametro     = 'orig_aliq_ICMS_ST'
      AND nfe-it-param-rec.valor-1-item-parametro = "XML"  NO-ERROR.
ASSIGN l-ICMS-ST-xml = AVAIL nfe-it-param-rec.

FIND FIRST nfe-it-param-rec 
    WHERE nfe-it-param-rec.cod-parametro      = 'param_global'
      AND nfe-it-param-rec.cod-item-parametro = "gera_duplicata"
      AND nfe-it-param-rec.valor-1-item-parametro = "SIM"  NO-LOCK NO-ERROR.
ASSIGN l-gera-duplic = AVAIL nfe-it-param-rec.

IF p-ind-event = 'AFTER-INITIALIZE' THEN DO:
    ASSIGN c-handle-obj = fc-handle-obj("c-cgc",p-wgh-frame)
           wh-re2001z-c-cgc         = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
    ASSIGN c-handle-obj = fc-handle-obj("serie-docto",p-wgh-frame)
           wh-re2001z-serie-docto   = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
    ASSIGN c-handle-obj = fc-handle-obj("nat-operacao",p-wgh-frame)
           wh-re2001z-nat-operacao  = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
    ASSIGN c-handle-obj = fc-handle-obj("btOk",p-wgh-frame)
           wh-re2001z-btOk          = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
    ASSIGN c-handle-obj = fc-handle-obj("btSave",p-wgh-frame)
           wh-re2001z-btSave        = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
    ASSIGN c-handle-obj = fc-handle-obj("nro-docto",p-wgh-frame)
           wh-re2001z-i-nro-docto   = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
    ASSIGN c-handle-obj = fc-handle-obj("cb-cod-observa",p-wgh-frame)
           wh-re2001z-cb-cod-observa = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.

    /* --- Trata Versao do EMS --- */
    &IF '{&bf_dis_versao_ems}' >= '2.06' &THEN
        ASSIGN c-handle-obj = fc-handle-obj("i-nro-docto",p-wgh-frame).
    &ELSE
        ASSIGN c-handle-obj = fc-handle-obj("nro-docto",p-wgh-frame).
    &ENDIF
    ASSIGN wh-re2001z-i-nro-docto   = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
        
    ASSIGN wh-re2001z-btOk-falso    = fc-falso(wh-re2001z-btOk, wh-re2001z-btOk:FRAME, "")
           wh-re2001z-btSave-falso  = fc-falso(wh-re2001z-btSave, wh-re2001z-btSave:FRAME, "").

    ON "ENTRY" OF wh-re2001z-nat-operacao
        PERSISTENT RUN dsc\ra\upc\esnfere2001z-u01.p ("ENTRY",
                                                      "nat-operacao",
                                                      wh-re2001z-nat-operacao,
                                                      p-wgh-frame,
                                                      "" ,
                                                      p-row-table).
    ON "CHOOSE" OF wh-re2001z-btOk-falso
        PERSISTENT RUN dsc\ra\upc\esnfere2001z-u01.p ("GERA-TT",
                                                      "btOk",
                                                      wh-re2001z-btOk-falso,
                                                      p-wgh-frame,
                                                      "" ,
                                                      p-row-table).
    ON "CHOOSE" OF wh-re2001z-btSave-falso
        PERSISTENT RUN dsc\ra\upc\esnfere2001z-u01.p ("GERA-TT",
                                                      "btSave",
                                                      wh-re2001z-btSave-falso,
                                                      p-wgh-frame,
                                                      "" ,
                                                      p-row-table).
    
END.

IF p-ind-event  = "ENTRY"           AND
   p-ind-object = "nat-operacao"    THEN DO:
    
    FIND FIRST nfe-nota-fiscal-rec WHERE nfe-nota-fiscal-rec.ide-Serie  = wh-re2001z-serie-docto:SCREEN-VALUE
                                     AND nfe-nota-fiscal-rec.ide-nNF    = wh-re2001z-i-nro-docto:SCREEN-VALUE
                                     AND nfe-nota-fiscal-rec.cgc        = wh-re2001z-c-cgc:SCREEN-VALUE
                                     NO-LOCK NO-ERROR.
    FIND FIRST nfe-dfe NO-LOCK
        WHERE nfe-dfe.chave-acesso = nfe-nota-fiscal-rec.chave-acesso-nfe NO-ERROR.

    IF AVAIL nfe-nota-fiscal-rec
        AND nfe-dfe.sit-erp = 2 /* --- Implantada --- */
         /*OR nfe-dfe.sit-erp = 3)*/ THEN DO: /* --- Implantada Manual --- */
        
        FIND FIRST nfe-it-nota-fisc-rec OF nfe-nota-fiscal-rec NO-LOCK NO-ERROR.
        IF AVAIL nfe-it-nota-fisc-rec THEN DO:
            
            ASSIGN wh-re2001z-nat-operacao:SCREEN-VALUE   = nfe-it-nota-fisc-rec.item-nat-operacao                                
                   wh-re2001z-cb-cod-observa:SCREEN-VALUE = ENTRY(int(nfe-nota-fiscal-rec.cod-observa), {ininc/i03in090.i 03}) .

            APPLY "LEAVE" TO wh-re2001z-nat-operacao.
        END.
    END.
END.

IF p-ind-event = "GERA-TT" THEN DO:
    ASSIGN i-cod-emitente = 0
           c-nat-operacao = ""
           rw-nota        = ?.
              
    FIND FIRST nfe-nota-fiscal-rec 
        WHERE nfe-nota-fiscal-rec.ide-Serie  = wh-re2001z-serie-docto:SCREEN-VALUE
          AND nfe-nota-fiscal-rec.ide-nNF    = wh-re2001z-i-nro-docto:SCREEN-VALUE
          AND nfe-nota-fiscal-rec.cgc        = wh-re2001z-c-cgc:SCREEN-VALUE NO-LOCK NO-ERROR.
    
    FIND FIRST nfe-dfe NO-LOCK
        WHERE nfe-dfe.chave-acesso = nfe-nota-fiscal-rec.chave-acesso-nfe NO-ERROR.

    IF AVAIL nfe-nota-fiscal-rec
        AND nfe-dfe.sit-erp = 2 THEN DO: /* --- Implantada --- */

        /* --- Busca Estabelecimento --- */
        FIND FIRST estabelec WHERE estabelec.cgc = nfe-nota-fiscal-rec.dest-cnpj
                             NO-LOCK NO-ERROR.
        
        /* --- Busca Fornecedor --- */
        FIND FIRST emitente 
            WHERE emitente.cgc = nfe-nota-fiscal-rec.cgc NO-LOCK NO-ERROR.
        
        FIND FIRST doc-fisico 
            WHERE doc-fisico.serie-docto  = nfe-nota-fiscal-rec.ide-Serie
              AND doc-fisico.nro-docto    = nfe-nota-fiscal-rec.ide-nNF
              AND doc-fisico.cod-emitente = emitente.cod-emitente NO-LOCK NO-ERROR.

        IF AVAIL doc-fisico AND doc-fisico.situacao  = 2 THEN DO: /* --- Atualizada no Fisico --- */
            ASSIGN i-cod-emitente = doc-fisico.cod-emitente
                   c-nat-operacao = wh-re2001z-nat-operacao:SCREEN-VALUE
                   rw-nota        = ROWID(nfe-nota-fiscal-rec).
        END.
                      
        IF l-multi_natureza THEN DO:
            /*Gerar tabela par enviar a MultiNatur*/
            IF AVAIL doc-fisico THEN DO:
                CREATE doc-orig-nfe.                                            
                ASSIGN doc-orig-nfe.cod-emitente       = doc-fisico.cod-emitente
                       doc-orig-nfe.cod-estabel        = doc-fisico.cod-estabel 
                       doc-orig-nfe.serie-docto        = doc-fisico.serie-docto 
                       doc-orig-nfe.nro-docto          = doc-fisico.nro-docto   
                       doc-orig-nfe.dt-emissao         = doc-fisico.dt-emissao  
                       doc-orig-nfe.ch-acesso-comp-nfe = nfe-dfe.chave-acesso
                       doc-orig-nfe.idi-orig-trad      = 2.
        
                FOR EACH it-doc-fisico EXCLUSIVE-LOCK
                    WHERE it-doc-fisico.serie-docto  = doc-fisico.serie-docto 
                    and   it-doc-fisico.nro-docto    = doc-fisico.nro-docto   
                    and   it-doc-fisico.cod-emitente = doc-fisico.cod-emitente:
        
                    FIND FIRST nfe-it-nota-fisc-rec NO-LOCK
                        WHERE nfe-it-nota-fisc-rec.chave-acesso  = nfe-dfe.chave-acesso
                        AND   nfe-it-nota-fisc-rec.seq-item * 10 = it-doc-fisico.sequencia
                        AND   nfe-it-nota-fisc-rec.it-codigo     = it-doc-fisico.it-codigo NO-ERROR.
        
                    IF AVAIL nfe-it-nota-fisc-rec THEN DO:
        
                        CREATE item-doc-orig-nfe.
                        ASSIGN item-doc-orig-nfe.ch-acesso-comp-nfe = doc-orig-nfe.ch-acesso-comp-nfe
                               item-doc-orig-nfe.idi-orig-trad      = 2 /* traduzido */
                               item-doc-orig-nfe.seq-item           = it-doc-fisico.sequencia
                     SUBSTRING(it-doc-fisico.char-2,42,5)           = string(it-doc-fisico.sequencia)
                               item-doc-orig-nfe.nat-operacao       = nfe-it-nota-fisc-rec.item-nat-operacao.
        
                    END.
                END.    
            END.
                               
            RELEASE doc-orig-nfe.
            RELEASE item-doc-orig-nfe.
        END.
    END.
    
    IF p-ind-object = "btOk"    THEN APPLY "CHOOSE" TO wh-re2001z-btOk.
    IF p-ind-object = "btSave"  THEN APPLY "CHOOSE" TO wh-re2001z-btSave.
     
    FIND FIRST nfe-nota-fiscal-rec WHERE ROWID(nfe-nota-fiscal-rec) = rw-nota
                                   NO-LOCK NO-ERROR.
    IF AVAIL nfe-nota-fiscal-rec AND nfe-dfe.sit-erp = 2 THEN DO:
        EMPTY TEMP-TABLE tt-item-doc-est.

        IF l-multi_natureza THEN DO:
            FOR EACH doc-orig-nfe
                WHERE doc-orig-nfe.ch-acesso-comp-nfe = nfe-nota-fiscal-rec.chave-acesso:
                DELETE doc-orig-nfe.
            END.        
                                        
            FOR EACH item-doc-orig-nfe
                WHERE item-doc-orig-nfe.ch-acesso-comp-nfe = nfe-nota-fiscal-rec.chave-acesso:
                
                DELETE item-doc-orig-nfe            .
            END.
        END.
    
        FIND FIRST docum-est
            WHERE docum-est.serie-docto    = nfe-nota-fiscal-rec.ide-Serie
              AND docum-est.nro-docto      = nfe-nota-fiscal-rec.ide-nNF
              AND docum-est.cod-emitente   = i-cod-emitente
              AND docum-est.nat-operacao   = c-nat-operacao EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL docum-est THEN DO:
            FIND FIRST estabelec NO-LOCK
                WHERE estabelec.cod-estabel = docum-est.cod-estabel NO-ERROR.

            IF AVAIL estabelec THEN
                FIND FIRST emitente NO-LOCK
                    WHERE emitente.cod-emitente = estabelec.cod-emitente NO-ERROR.    

            FOR EACH b-item-doc-est 
                WHERE b-item-doc-est.serie-docto = docum-est.serie-docto
                  AND b-item-doc-est.nro-docto   = docum-est.nro-docto
                  AND b-item-doc-est.cod-emitente = docum-est.cod-emitente
                  AND b-item-doc-est.nat-operacao = docum-est.nat-operacao EXCLUSIVE-LOCK:

                FIND FIRST nfe-it-nota-fisc-rec 
                    WHERE nfe-it-nota-fisc-rec.chave-acesso  = nfe-nota-fiscal-rec.chave-acesso
                      AND nfe-it-nota-fisc-rec.seq-item * 10 = b-item-doc-est.sequencia EXCLUSIVE-LOCK NO-ERROR.
                FIND FIRST nfe-it-imposto-alt NO-LOCK
                    WHERE nfe-it-imposto-alt.chave-acesso-nfe = nfe-it-nota-fisc-rec.chave-acesso-nfe
                      AND nfe-it-imposto-alt.seq-item         = nfe-it-nota-fisc-rec.seq-item NO-ERROR.

                IF AVAIL nfe-it-nota-fisc-rec THEN DO:
                    FIND FIRST ITEM NO-LOCK
                         WHERE ITEM.it-codigo = b-item-doc-est.it-codigo NO-ERROR.
                                                 
                    FIND FIRST classif-fisc NO-LOCK
                         WHERE classif-fisc.class-fiscal = ITEM.class-fiscal NO-ERROR.

                    FIND FIRST param-re NO-LOCK
                        WHERE param-re.usuario = c-seg-usuario NO-ERROR.

                    FIND FIRST natur-oper NO-LOCK
                        WHERE natur-oper.nat-operacao = IF l-multi_natureza THEN b-item-doc-est.nat-of ELSE docum-est.nat-operacao NO-ERROR.

                    /* --- Frete, Despesas e Desconto --- */
                    /*IF nfe-it-nota-fisc-rec.item-vDesc <> 0 THEN
                        ASSIGN b-item-doc-est.desconto[1] = nfe-it-nota-fisc-rec.item-vDesc.
            
                    IF nfe-it-nota-fisc-rec.item-vSeg   <> 0 OR 
                       nfe-it-nota-fisc-rec.item-vFrete <> 0 OR
                       nfe-it-nota-fisc-rec.item-vOutro <> 0 THEN
                        ASSIGN b-item-doc-est.despesas[1] = (nfe-it-nota-fisc-rec.item-vSeg + nfe-it-nota-fisc-rec.item-vFrete + nfe-it-nota-fisc-rec.item-vOutro).
            
                    IF nfe-it-nota-fisc-rec.item-vFrete <> 0 THEN
                        ASSIGN b-item-doc-est.pr-total-cmi = nfe-it-nota-fisc-rec.item-vFrete.*/

                    
                    /* --- Cria Tabela de Itens do Recebimento Fiscal --- */
                    RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001.
                    RUN pi_valida_tipo_nota_fisico IN h_bonfe001 (INPUT  docum-est.nat-operacao,
                                                                  OUTPUT i-tipo-nfe).
                    RUN cdp/cd4337.p PERSISTENT SET h_cd4337. 
                                                          
                    IF i-tipo-nfe = 2 THEN DO: /*devolucao*/
                        /*ICMS Devoluá∆o*/
                        IF l-ICMS-dev-xml = NO THEN
                            RUN pi_aliquota_icm IN h_bonfe001(INPUT emitente.contrib-icms     ,     
                                                              INPUT emitente.natureza         ,     
                                                              INPUT emitente.estado              ,
                                                              INPUT emitente.pais                , 
                                                              INPUT estabelec.estado             ,
                                                              INPUT b-item-doc-est.it-codigo    ,            
                                                              INPUT natur-oper.nat-operacao,
                                                              OUTPUT b-item-doc-est.aliquota-icm).
                        ELSE DO:
                            ASSIGN b-item-doc-est.aliquota-icm = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.icms-aliq ELSE IF nfe-nota-fiscal-rec.emit-crt = 1 THEN nfe-it-nota-fisc-rec.imp-pCredSN ELSE nfe-it-nota-fisc-rec.imp-pICMS /*emit-crt = 1 simples nacional*/
                                   b-item-doc-est.base-icm[1]  = nfe-it-nota-fisc-rec.imp-vBC
                                   b-item-doc-est.valor-icm[1] = IF nfe-nota-fiscal-rec.emit-crt = 1 THEN nfe-it-nota-fisc-rec.imp-vCredICMSSN ELSE nfe-it-nota-fisc-rec.imp-vICMS.   /*emit-crt = 1 simples nacional*/

                            IF b-item-doc-est.cd-trib-icm = 3 THEN /*Outros*/ DO:
                                                            
                                ASSIGN b-item-doc-est.icm-outras[1]  = nfe-it-nota-fisc-rec.imp-vBC
                                       b-item-doc-est.base-icm[1]    = 0.

                                ASSIGN de-vl-total-vicms                   = de-vl-total-vicms + b-item-doc-est.valor-icm[1]
                                       de-vl-total-vbicms                  = de-vl-total-vbicms + b-item-doc-est.icm-outras[1].

                            END.
                            ELSE DO:
                                ASSIGN de-vl-total-vicms                   = de-vl-total-vicms + b-item-doc-est.valor-icm[1]
                                       de-vl-total-vbicms                  = de-vl-total-vbicms + b-item-doc-est.base-icm[1].
                            END.

                            IF l-CST-ICMS-xml THEN DO:
                                IF nfe-nota-fiscal-rec.emit-crt = 1 THEN
                                    ASSIGN SUBSTR(b-item-doc-est.char-2,502,3) = string(nfe-it-nota-fisc-rec.imp-orig) + "99". /*CST ICMS*/
                                ELSE
                                    ASSIGN SUBSTR(b-item-doc-est.char-2,502,3) = string(string(nfe-it-nota-fisc-rec.imp-orig) + STRING(nfe-it-nota-fisc-rec.imp-CST,"99")) . /*CST ICMS*/
                            END.
                        END.
                        
                        /*ICMS ST Devoluá∆o*/
                        IF l-ICMS-ST-dev-xml THEN DO:
                            ASSIGN de-vl-total-vbicmssub     = de-vl-total-vbicmssub + b-item-doc-est.BASE-SUBS[1]
                                   de-vl-total-vicmssub      = de-vl-total-vicmssub + b-item-doc-est.VL-SUBS[1]
                                   de-vl-total               = de-vl-total + b-item-doc-est.VL-SUBS[1] + b-item-doc-est.valor-ipi[1] + b-item-doc-est.preco-total[1].
                        END. 

                        /*CST ICMS do ERP*/
                        IF l-CST-ICMS-erp THEN DO:
                            IF nfe-nota-fiscal-rec.emit-crt <> 1 THEN DO: /*cst simples nacional*/
                                ASSIGN c-orig-cst = string(nfe-it-nota-fisc-rec.imp-orig).
                                
                                CASE b-item-doc-est.cd-trib-icm :
                                    WHEN 1 THEN DO: /*tributado*/
                                        IF b-item-doc-est.log-icm-retido THEN
                                            ASSIGN c-cst-icms = "10". 
                                        ELSE
                                            ASSIGN c-cst-icms = "00". 
                                    END.
                                    WHEN 2 THEN DO: /*Isento*/
                                        IF b-item-doc-est.log-icm-retido THEN
                                            ASSIGN c-cst-icms = "30". 
                                        IF b-item-doc-est.log-icm-retido = NO
                                            AND natur-oper.ind-tipo-vat = NO THEN
                                            ASSIGN c-cst-icms = "40". 
                                        IF natur-oper.ind-tipo-vat THEN
                                            ASSIGN c-cst-icms = "41". 
                                    END.
                                    WHEN 3 THEN  /*Outros*/
                                        ASSIGN c-cst-icms = "90". 
                                    WHEN 4 THEN DO: /*Reduzido*/
                                        IF b-item-doc-est.log-icm-retido THEN
                                            ASSIGN c-cst-icms = "70". 
                                        ELSE
                                            ASSIGN c-cst-icms = "20". 
                                    END.
                                    OTHERWISE
                                        ASSIGN c-cst-icms = "90". 
                                END CASE.
                                    
                                IF natur-oper.log-icms-substto-antecip THEN
                                    ASSIGN c-cst-icms = "60". 
                                IF natur-oper.ind-it-sub-dif THEN
                                    ASSIGN c-cst-icms = "50". 
                                IF natur-oper.ind-it-sub-dif = ? THEN
                                    ASSIGN c-cst-icms = "51". 
                                
                                ASSIGN SUBSTR(b-item-doc-est.char-2,502,3) = c-orig-cst + c-cst-icms.
                            END.
                        END.
                        
                        /*IPI Devoluá∆o*/
                        IF nfe-it-nota-fisc-rec.imp-ipi-vBC = 0 THEN 
                            ASSIGN de-base-calculo = nfe-it-nota-fisc-rec.item-vProd.
                        ELSE 
                            ASSIGN de-base-calculo = nfe-it-nota-fisc-rec.imp-ipi-vBC.
                                  
                        IF b-item-doc-est.cd-trib-ipi   = 1
                            AND b-item-doc-est.aliquota-ipi  = 0 THEN 
                            ASSIGN b-item-doc-est.ipi-outras[1] = de-base-calculo.
                
                        IF b-item-doc-est.cd-trib-ipi   = 4
                            AND b-item-doc-est.aliquota-ipi  = 0 THEN 
                            ASSIGN b-item-doc-est.ipi-outras[1] = de-base-calculo.
                
                        IF b-item-doc-est.cd-trib-ipi   = 2 THEN
                            ASSIGN b-item-doc-est.ipi-ntrib  = de-base-calculo.
                           
                        IF b-item-doc-est.cd-trib-ipi   = 3 THEN 
                            ASSIGN b-item-doc-est.ipi-outras[1] = de-base-calculo.
        
                        IF l-IPI-dev-xml THEN DO:
/*                        rva     IF nfe-it-nota-fisc-rec.imp-ipi-vBC = 0 THEN /*se n∆o tem ipi, colocar isento*/ */
/*                                 ASSIGN b-item-doc-est.cd-trib-ipi = 2.     /* --- Isento           --- */   */
                            ASSIGN b-item-doc-est.aliquota-IPI = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-aliq ELSE nfe-it-nota-fisc-rec.imp-ipi-pIPI
                                   b-item-doc-est.valor-IPI[1] = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-valor ELSE nfe-it-nota-fisc-rec.imp-ipi-vIPI
                                   b-item-doc-est.base-ipi[1]  = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-base ELSE nfe-it-nota-fisc-rec.imp-ipi-vBC.
            

                            ASSIGN de-vl-total-vbipi            = de-vl-total-vbipi + b-item-doc-est.base-ipi[1] /*variavel ser† usada para criar a docum-est.baseipi*/
                                   de-vl-total-vipi             = de-vl-total-vipi  + b-item-doc-est.valor-IPI[1]. /*variavel ser† usada para criar a docum-est.ipi-deb-cre*/
                            
                            /*RVA BAuducco erro de IPI outras na docum-est*/
                            IF tt-item-doc-est.cd-trib-ipi = 3 THEN 
                                ASSIGN de-vl-total-vbipi            = de-vl-total-vbipi + b-item-doc-est.ipi-outras[1]. /*variavel ser† usada para criar a docum-est.baseipi*/
                            IF tt-item-doc-est.cd-trib-ipi   = 1 AND tt-item-doc-est.aliquota-ipi  = 0 THEN 
                                ASSIGN de-vl-total-vbipi            = de-vl-total-vbipi + b-item-doc-est.ipi-outras[1]. /*variavel ser† usada para criar a docum-est.baseipi*/
                            IF tt-item-doc-est.cd-trib-ipi   = 4 AND tt-item-doc-est.aliquota-ipi  = 0 THEN 
                                ASSIGN de-vl-total-vbipi            = de-vl-total-vbipi + b-item-doc-est.ipi-outras[1]. /*variavel ser† usada para criar a docum-est.baseipi*/
                        END.
                        
                        ELSE DO: /*Classificaá∆o Fiscal ou item*/
                            IF b-item-doc-est.cd-trib-ipi   = 1 
                                AND b-item-doc-est.aliquota-ipi <> 0 THEN 
                            ASSIGN b-item-doc-est.base-ipi[1] = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-base ELSE de-base-calculo.
                                                                                                                                  
                            IF AVAIL param-re
                                AND param-re.orig-aliq-ip  = 1
                                AND AVAIL classif-fisc THEN 
                                assign b-item-doc-est.aliquota-ipi = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-aliq ELSE classif-fisc.aliquota-ipi.
                            else
                                assign b-item-doc-est.aliquota-ipi = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-aliq ELSE item.aliquota-ipi.
                        END.                                

                        /*PIS Devoluá∆o*/
                        IF l-PIS-dev-xml THEN
                            ASSIGN b-item-doc-est.val-aliq-pis     = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.pis-aliq ELSE nfe-it-nota-fisc-rec.imp-pis-pPIS.
                        
                        RUN pi-tributa-aliquota-pis IN h_cd4337 (BUFFER estabelec,
                                                                 BUFFER item,       
                                                                 BUFFER emitente,   
                                                                 BUFFER natur-oper, 
                                                                 INPUT TODAY,
                                                                 OUTPUT pi-trib-pis,    
                                                                 OUTPUT pi-aliquota-pis,
                                                                 OUTPUT pi-valor-un-pis,
                                                                 OUTPUT pi-reduz-pis).  

                        ASSIGN b-item-doc-est.idi-tributac-pis = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.pis-tribut else pi-trib-pis.
                        
                        IF l-PIS-dev-xml = NO THEN 
                            ASSIGN b-item-doc-est.val-aliq-pis = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.pis-aliq ELSE pi-aliquota-pis
                                   b-item-doc-est.valor-pis    = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.pis-val ELSE pi-valor-un-pis.
                        
                        /*cofins Devoluá∆o*/
                        IF l-COFINS-dev-xml THEN
                            ASSIGN b-item-doc-est.val-aliq-cofins     = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.cofins-aliq ELSE nfe-it-nota-fisc-rec.imp-cofins-pCOFINS.

                        RUN pi-tributa-aliquota-cofins IN h_cd4337 (BUFFER estabelec,
                                                                    BUFFER item,       
                                                                    BUFFER emitente,   
                                                                    BUFFER natur-oper, 
                                                                    INPUT TODAY,
                                                                    OUTPUT pi-trib-cofins,    
                                                                    OUTPUT pi-aliquota-cofins,
                                                                    OUTPUT pi-valor-un-cofins,
                                                                    OUTPUT pi-reduz-cofins).  
                        ASSIGN b-item-doc-est.idi-tributac-cofins = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.cofins-tribut else pi-trib-cofins.
                        IF l-COFINS-dev-xml = NO THEN 
                            ASSIGN b-item-doc-est.val-aliq-cofins = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.cofins-aliq ELSE pi-aliquota-cofins 
                                   b-item-doc-est.val-cofins      = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.cofins-valor ELSE pi-valor-un-cofins.
                    END.
                    ELSE DO: /*Outras Notas diferente de Devoluá∆o*/
                        /*ICMS Compras*/
                                            
                        IF l-ICMS-xml = NO THEN
                            RUN pi_aliquota_icm IN h_bonfe001 (INPUT emitente.contrib-icms     ,     
                                                               INPUT emitente.natureza         ,     
                                                               INPUT emitente.estado              ,
                                                               INPUT emitente.pais                , 
                                                               INPUT estabelec.estado             ,
                                                               INPUT b-item-doc-est.it-codigo    ,            
                                                               INPUT natur-oper.nat-operacao,
                                                               OUTPUT b-item-doc-est.aliquota-icm).
                           
                        ELSE DO:
                            ASSIGN b-item-doc-est.aliquota-icm = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.icms-aliq ELSE IF nfe-nota-fiscal-rec.emit-crt = 1 THEN nfe-it-nota-fisc-rec.imp-pCredSN ELSE nfe-it-nota-fisc-rec.imp-pICMS /*emit-crt = 1 simples nacional*/
                                   b-item-doc-est.base-icm[1]  = nfe-it-nota-fisc-rec.imp-vBC
                                   b-item-doc-est.valor-icm[1] = IF nfe-nota-fiscal-rec.emit-crt = 1 THEN nfe-it-nota-fisc-rec.imp-vCredICMSSN ELSE nfe-it-nota-fisc-rec.imp-vICMS.   /*emit-crt = 1 simples nacional*/

                            IF b-item-doc-est.cd-trib-icm = 3 THEN /*Outros*/ DO:
                                
                                
                                ASSIGN b-item-doc-est.icm-outras[1]  = nfe-it-nota-fisc-rec.imp-vBC
                                       b-item-doc-est.base-icm[1]    = 0.

                                ASSIGN de-vl-total-vicms                   = de-vl-total-vicms + b-item-doc-est.valor-icm[1]
                                       de-vl-total-vbicms                  = de-vl-total-vbicms + b-item-doc-est.icm-outras[1].

                            END.
                            ELSE DO:
                                
                                ASSIGN de-vl-total-vicms                   = de-vl-total-vicms + b-item-doc-est.valor-icm[1]
                                       de-vl-total-vbicms                  = de-vl-total-vbicms + b-item-doc-est.base-icm[1].
                            END.

                            IF l-CST-ICMS-xml THEN DO:
                                IF nfe-nota-fiscal-rec.emit-crt = 1 THEN 
                                    ASSIGN SUBSTR(b-item-doc-est.char-2,502,3) = string(nfe-it-nota-fisc-rec.imp-orig) + "99" . /*CST ICMS*/
                                ELSE
                                    ASSIGN SUBSTR(b-item-doc-est.char-2,502,3) = string(string(nfe-it-nota-fisc-rec.imp-orig) + STRING(nfe-it-nota-fisc-rec.imp-CST,"99")) . /*CST ICMS*/
                            END.
                        END.

                        /*ICMS ST*/
                        IF l-ICMS-ST-xml THEN DO:
                            ASSIGN de-vl-total-vbicmssub     = de-vl-total-vbicmssub + b-item-doc-est.BASE-SUBS[1]
                                   de-vl-total-vicmssub      = de-vl-total-vicmssub + b-item-doc-est.VL-SUBS[1]
                                   de-vl-total               = de-vl-total + b-item-doc-est.VL-SUBS[1] + b-item-doc-est.valor-ipi[1] + b-item-doc-est.preco-total[1].
                        END.                      

                                                      
                        /*IPI Compras*/
                        IF nfe-it-nota-fisc-rec.imp-ipi-vBC = 0 THEN 
                            ASSIGN de-base-calculo = nfe-it-nota-fisc-rec.item-vProd.
                        ELSE 
                            ASSIGN de-base-calculo = nfe-it-nota-fisc-rec.imp-ipi-vBC.
                
                            
                        IF b-item-doc-est.cd-trib-ipi   = 1 AND
                           b-item-doc-est.aliquota-ipi  = 0 THEN 
                            ASSIGN b-item-doc-est.ipi-outras[1] = de-base-calculo.
                
                        IF b-item-doc-est.cd-trib-ipi   = 4 AND
                           b-item-doc-est.aliquota-ipi  = 0 THEN 
                            ASSIGN b-item-doc-est.ipi-outras[1] = de-base-calculo.
                
                        IF b-item-doc-est.cd-trib-ipi   = 2 THEN
                            ASSIGN b-item-doc-est.ipi-ntrib  = de-base-calculo.
                           
                        IF b-item-doc-est.cd-trib-ipi   = 3 THEN 
                            ASSIGN b-item-doc-est.ipi-outras[1] = de-base-calculo.
    
                        
                        IF l-IPI-xml THEN DO:
                            
                            ASSIGN b-item-doc-est.aliquota-IPI = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-aliq ELSE nfe-it-nota-fisc-rec.imp-ipi-pIPI
                                   b-item-doc-est.valor-IPI[1] = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-valor ELSE nfe-it-nota-fisc-rec.imp-ipi-vIPI
                                   b-item-doc-est.base-ipi[1]  = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-base ELSE nfe-it-nota-fisc-rec.imp-ipi-vBC.
    
                            ASSIGN de-vl-total-vbipi            = de-vl-total-vbipi + b-item-doc-est.base-ipi[1] /*variavel ser† usada para criar a docum-est.baseipi*/
                                   de-vl-total-vipi             = de-vl-total-vipi  + b-item-doc-est.valor-IPI[1]. /*variavel ser† usada para criar a docum-est.ipi-deb-cre*/
                            
                            /*RVA BAuducco erro de IPI outras na docum-est*/
                            IF b-item-doc-est.cd-trib-ipi = 3 THEN 
                                ASSIGN de-vl-total-vbipi            = de-vl-total-vbipi + b-item-doc-est.ipi-outras[1]. /*variavel ser† usada para criar a docum-est.baseipi*/
                            IF b-item-doc-est.cd-trib-ipi   = 1 AND b-item-doc-est.aliquota-ipi  = 0 THEN 
                                ASSIGN de-vl-total-vbipi            = de-vl-total-vbipi + b-item-doc-est.ipi-outras[1]. /*variavel ser† usada para criar a docum-est.baseipi*/
                            IF b-item-doc-est.cd-trib-ipi   = 4 AND b-item-doc-est.aliquota-ipi  = 0 THEN 
                                ASSIGN de-vl-total-vbipi            = de-vl-total-vbipi + b-item-doc-est.ipi-outras[1]. /*variavel ser† usada para criar a docum-est.baseipi*/
                            
                        END.
                        ELSE DO: /*Classificaá∆o Fiscal ou item*/
                            IF b-item-doc-est.cd-trib-ipi   = 1
                                AND b-item-doc-est.aliquota-ipi <> 0 THEN 
                                
                                ASSIGN b-item-doc-est.base-ipi[1] = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-base ELSE de-base-calculo.
                                                                
                            IF AVAIL param-re
                                AND param-re.orig-aliq-ip  = 1
                                AND avail classif-fisc THEN /*Classificaá∆o*/
                                
                                assign b-item-doc-est.aliquota-ipi = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-aliq ELSE classif-fisc.aliquota-ipi.
                            else /*Item*/
                                assign b-item-doc-est.aliquota-ipi = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.ipi-aliq ELSE item.aliquota-ipi.
                        END.

                        
                        /*PIS cOMPRAR*/
                        IF l-PIS-xml THEN
                            ASSIGN b-item-doc-est.val-aliq-pis     = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.pis-aliq ELSE nfe-it-nota-fisc-rec.imp-pis-pPIS.
                        
                        RUN pi-tributa-aliquota-pis IN h_cd4337 (BUFFER estabelec,
                                                                 BUFFER item,       
                                                                 BUFFER emitente,   
                                                                 BUFFER natur-oper, 
                                                                 INPUT TODAY,
                                                                 OUTPUT pi-trib-pis,    
                                                                 OUTPUT pi-aliquota-pis,
                                                                 OUTPUT pi-valor-un-pis,
                                                                 OUTPUT pi-reduz-pis).  

                        ASSIGN b-item-doc-est.idi-tributac-pis = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.pis-tribut else pi-trib-pis.
                        
                        IF l-PIS-xml = NO THEN 
                            ASSIGN b-item-doc-est.val-aliq-pis = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.pis-aliq ELSE pi-aliquota-pis
                                   b-item-doc-est.valor-pis    = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.pis-val ELSE pi-valor-un-pis.

                        /*cofins Compras*/
                        IF l-COFINS-xml THEN
                            ASSIGN b-item-doc-est.val-aliq-cofins     = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.cofins-aliq ELSE nfe-it-nota-fisc-rec.imp-cofins-pCOFINS.

                        RUN pi-tributa-aliquota-cofins IN h_cd4337 (BUFFER estabelec,
                                                                    BUFFER item,       
                                                                    BUFFER emitente,   
                                                                    BUFFER natur-oper, 
                                                                    INPUT TODAY,
                                                                    OUTPUT pi-trib-cofins,    
                                                                    OUTPUT pi-aliquota-cofins,
                                                                    OUTPUT pi-valor-un-cofins,
                                                                    OUTPUT pi-reduz-cofins).  
                        ASSIGN b-item-doc-est.idi-tributac-cofins = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.cofins-tribut else pi-trib-cofins.
                        IF l-COFINS-xml = NO THEN 
                            ASSIGN b-item-doc-est.val-aliq-cofins = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.cofins-aliq ELSE pi-aliquota-cofins 
                                   b-item-doc-est.val-cofins      = IF AVAIL nfe-it-imposto-alt THEN nfe-it-imposto-alt.cofins-valor ELSE pi-valor-un-cofins.
                    END.
                END.
                                                                          
                
                IF i-tipo-nfe = 2 THEN DO:
                    IF l-icms-st-dev-xml = YES THEN
                        /*ICMS ST*/
                        ASSIGN de-vl-total-vbicmssub     = de-vl-total-vbicmssub + nfe-it-nota-fisc-rec.imp-vBCST
                               de-vl-total-vicmssub      = de-vl-total-vicmssub + nfe-it-nota-fisc-rec.imp-vICMSST
                               b-item-doc-est.VL-SUBS[1]   = nfe-it-nota-fisc-rec.imp-vICMSST
                               b-item-doc-est.BASE-SUBS[1] = nfe-it-nota-fisc-rec.imp-vBCST.
                END.
                ELSE DO: 
                    IF l-icms-st-xml = YES THEN
                        /*ICMS ST*/
                        ASSIGN de-vl-total-vbicmssub     = de-vl-total-vbicmssub + nfe-it-nota-fisc-rec.imp-vBCST
                               de-vl-total-vicmssub      = de-vl-total-vicmssub + nfe-it-nota-fisc-rec.imp-vICMSST
                               b-item-doc-est.VL-SUBS[1]   = nfe-it-nota-fisc-rec.imp-vICMSST
                               b-item-doc-est.BASE-SUBS[1] = nfe-it-nota-fisc-rec.imp-vBCST.
                END.

                IF l-cest = YES THEN
                    ASSIGN SUBSTR(b-item-doc-est.char-2,840,7) = STRING(nfe-it-nota-fisc-rec.item-CEST). /* CEST*/

                CREATE tt-item-doc-est.
                BUFFER-COPY b-item-doc-est TO tt-item-doc-est.
            END. 
            

            IF de-vl-total-vicms <> 0 THEN
                ASSIGN docum-est.icm-deb-cre = de-vl-total-vicms.

            IF de-vl-total-vbicms <> 0 THEN
                ASSIGN  docum-est.base-icm    = de-vl-total-vbicms.
            
            IF de-vl-total-vicmssub <> 0 THEN 
                ASSIGN docum-est.VL-SUBS   = de-vl-total-vicmssub.

            IF de-vl-total-vbicmssub <> 0 THEN
                ASSIGN docum-est.BASE-SUBS = de-vl-total-vbicmssub.

            IF de-vl-total-vbipi <> 0 THEN
                ASSIGN docum-est.base-ipi = de-vl-total-vbipi.
            
            IF de-vl-total-vipi <> 0 THEN
                ASSIGN docum-est.ipi-deb-cre = de-vl-total-vipi.
                                                                                    
            &IF '{&bf_dis_versao_ems}' >= '2.08' &THEN 
               ASSIGN docum-est.cod-chave-aces-nf-eletro =  nfe-nota-fiscal-rec.chave-acesso-nfe.
            &ENDIF

               
            ASSIGN SUBSTR(docum-est.char-1,113,02) = "55"
                   SUBSTR(docum-est.char-1,093,44) = nfe-nota-fiscal-rec.chave-acesso-nfe
                          docum-est.cod-observa    = nfe-nota-fiscal-rec.cod-observa.
            
            IF VALID-HANDLE(h_bonfe001) = NO THEN
                RUN dsc\ra\esp\bonfe001.p PERSISTENT SET h_bonfe001.  
                  
            
            /* --- Recalcula Imposto da Nota no Recebimento Fiscal --- */
            RUN pi_recalcula_imposto IN h_bonfe001 (INPUT TABLE tt-item-doc-est,
                                                    OUTPUT c-mensagem-erro).

            IF l-gera-duplic THEN DO:
                FOR EACH dupli-apagar
                    WHERE dupli-apagar.serie-docto  = docum-est.serie-docto
                      AND dupli-apagar.nro-docto    = docum-est.nro-docto
                      AND dupli-apagar.cod-emitente = docum-est.cod-emitente
                      AND dupli-apagar.nat-operacao = docum-est.nat-operacao EXCLUSIVE-LOCK:
                    
                    DELETE dupli-apagar.
                END.

                FIND FIRST bf5-natur-oper NO-LOCK
                    WHERE bf5-natur-oper.nat-operacao = docum-est.nat-operacao NO-ERROR.

                IF bf5-natur-oper.emite-duplic = YES AND bf5-natur-oper.tipo = 1 THEN DO:
                    
                   RUN rep/re9341.p (INPUT ROWID(docum-est), INPUT NO).          
                END.                                                             
            END.
            
        END.
        DELETE PROCEDURE h_bonfe001.
    END.
END.






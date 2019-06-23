
DEFINE INPUT  PARAMETER c-chave       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER i-seq         AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER i-seq-aux     AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER i-nr-ordem    AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER c-cfop        AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER c-mod         AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER i-crt         AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER i-csosn       AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER d-pcredsn     AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER d-vcredicmssn AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER d-ICMS        AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER d-IPI         AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER i-cst-icms    AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER i-cst-piscof  AS INTEGER     NO-UNDO.

DEFINE OUTPUT PARAMETER d-icms-BC   LIKE nfe-it-nota-fisc-rec.imp-vBC.
DEFINE OUTPUT PARAMETER d-Vicms     LIKE nfe-it-nota-fisc-rec.imp-vICMS.
DEFINE OUTPUT PARAMETER d-VIPI      LIKE nfe-it-nota-fisc-rec.imp-ipi-vIPI.
DEFINE OUTPUT PARAMETER d-ipi-bc    LIKE nfe-it-nota-fisc-rec.imp-ipi-vBC.
DEFINE OUTPUT PARAMETER d-RedBc     LIKE nfe-it-nota-fisc-rec.imp-pRedBC.
DEFINE OUTPUT PARAMETER imp-pICMSST LIKE nfe-it-nota-fisc-rec.imp-pICMSST.
DEFINE OUTPUT PARAMETER imp-vBCST   LIKE nfe-it-nota-fisc-rec.imp-vBCST.
DEFINE OUTPUT PARAMETER imp-vICMSST LIKE nfe-it-nota-fisc-rec.imp-vICMSST.


DEFINE BUFFER bnfe-it-nota-fisc-rec FOR nfe-it-nota-fisc-rec.
DEFINE BUFFER bnfe-nota-fiscal-rec  FOR nfe-nota-fiscal-rec.
DEFINE BUFFER bnfe-relac-ordem-rec  FOR nfe-relac-ordem-rec.

DEFINE VARIABLE i-fator AS INTEGER NO-UNDO.

ASSIGN imp-pICMSST = 0.
ASSIGN i-fator  = 0.

DEFINE VARIABLE l-encontrou AS LOGICAL     NO-UNDO.

ASSIGN l-encontrou = FALSE.

FIND FIRST bnfe-it-nota-fisc-rec NO-LOCK 
    WHERE bnfe-it-nota-fisc-rec.chave-acesso-nfe = c-chave
      AND bnfe-it-nota-fisc-rec.seq-item         = i-seq-aux NO-ERROR.
IF NOT AVAIL(bnfe-it-nota-fisc-rec) THEN DO:

    ASSIGN l-encontrou = TRUE.

    FIND FIRST bnfe-it-nota-fisc-rec NO-LOCK 
        WHERE bnfe-it-nota-fisc-rec.chave-acesso-nfe = c-chave
          AND bnfe-it-nota-fisc-rec.seq-item         = int(string(i-seq-aux) + "0") NO-ERROR.

END.


IF AVAIL(bnfe-it-nota-fisc-rec) THEN DO:
    FOR FIRST bnfe-it-nota-fisc-rec /*FIELDS(chave-acesso-nfe seq-item  item-CFOP   imp-pcredsn imp-vcredicmssn 
                                           imp-csosn        imp-pICMS imp-pis-CST imp-CST)*/
        WHERE bnfe-it-nota-fisc-rec.chave-acesso-nfe = c-chave
          AND bnfe-it-nota-fisc-rec.seq-item         = (IF l-encontrou THEN int(string(i-seq-aux) + "0") ELSE i-seq-aux) NO-LOCK:
    
        
        FOR EACH nfe-relac-ordem-rec
           WHERE nfe-relac-ordem-rec.chave-acesso = bnfe-it-nota-fisc-rec.chave-acesso 
             AND nfe-relac-ordem-rec.seq-item     = i-seq-aux:
    
            ASSIGN i-fator = i-fator + 1.
    
        END.
    
    
        FIND FIRST bnfe-relac-ordem-rec NO-LOCK
             WHERE bnfe-relac-ordem-rec.chave-acesso = bnfe-it-nota-fisc-rec.chave-acesso 
               AND bnfe-relac-ordem-rec.numero-ordem = i-nr-ordem
               AND bnfe-relac-ordem-rec.seq-item     = i-seq-aux NO-ERROR.
                 
        ASSIGN c-cfop        = STRING(bnfe-it-nota-fisc-rec.item-CFOP)
               d-pCredSN     = bnfe-it-nota-fisc-rec.imp-pCredSN
               d-vCredICMSSN = bnfe-it-nota-fisc-rec.imp-vCredICMSSN
               i-CSOSN       = bnfe-it-nota-fisc-rec.imp-CSOSN
               
               d-icms-BC     = IF i-fator > 0 THEN (bnfe-it-nota-fisc-rec.imp-vBC * (100 / i-fator * bnfe-relac-ordem-rec.quantidade)) / 100 ELSE bnfe-it-nota-fisc-rec.imp-vBC
               d-ICMS        = bnfe-it-nota-fisc-rec.imp-pICMS
               d-Vicms       = IF i-fator > 0 THEN (bnfe-it-nota-fisc-rec.imp-vICMS * (100 / i-fator * bnfe-relac-ordem-rec.quantidade)) / 100 ELSE bnfe-it-nota-fisc-rec.imp-vICMS
               i-cst-icms    = bnfe-it-nota-fisc-rec.imp-CST
               d-RedBc       = bnfe-it-nota-fisc-rec.imp-pRedBC
               imp-pICMSST   = bnfe-it-nota-fisc-rec.imp-pICMSST
    
               d-IPI         = bnfe-it-nota-fisc-rec.imp-ipi-pIPI
               d-VIPI        = IF i-fator > 0 THEN (bnfe-it-nota-fisc-rec.imp-ipi-vIPI * (100 / i-fator * bnfe-relac-ordem-rec.quantidade)) / 100 ELSE bnfe-it-nota-fisc-rec.imp-ipi-vIPI
               d-ipi-bc      = IF i-fator > 0 THEN (bnfe-it-nota-fisc-rec.imp-ipi-vBC * (100 / i-fator * bnfe-relac-ordem-rec.quantidade)) / 100  ELSE bnfe-it-nota-fisc-rec.imp-ipi-vBC
    
               i-cst-piscof  = bnfe-it-nota-fisc-rec.imp-pis-CST
             
               imp-vBCST     = IF i-fator > 0 THEN (bnfe-it-nota-fisc-rec.imp-vBCST * (100 / i-fator * bnfe-relac-ordem-rec.quantidade)) / 100  ELSE bnfe-it-nota-fisc-rec.imp-vBCST     
               imp-vICMSST   = IF i-fator > 0 THEN (bnfe-it-nota-fisc-rec.imp-vICMSST * (100 / i-fator * bnfe-relac-ordem-rec.quantidade)) / 100 ELSE bnfe-it-nota-fisc-rec.imp-vICMSST. 
               
    END.
    FOR FIRST bnfe-nota-fiscal-rec FIELDS(chave-acesso-nfe ide-mod emit-crt)
        WHERE bnfe-nota-fiscal-rec.chave-acesso-nfe = c-chave NO-LOCK:
        ASSIGN c-mod = bnfe-nota-fiscal-rec.ide-mod
               i-crt = bnfe-nota-fiscal-rec.emit-crt. /* Trib Simples,1,Trib Normal,3,Red. ICMS,4 */
    END.
END.
ELSE DO: /*Verifica se ‚ uma nota CTE */


    FIND FIRST nfe-cte-inf NO-LOCK
         WHERE nfe-cte-inf.chave-acesso = c-chave NO-ERROR.
    IF AVAIL(nfe-cte-inf) THEN DO:

        FIND FIRST docum-est NO-LOCK
             WHERE docum-est.cod-chave-aces-nf-eletro = c-chave NO-ERROR.

        FOR EACH item-doc-est NO-LOCK
           WHERE item-doc-est.serie-docto  = docum-est.serie-docto 
             AND item-doc-est.nro-docto    = docum-est.nro-docto   
             AND item-doc-est.cod-emitente = docum-est.cod-emitente 
             AND item-doc-est.nat-operacao = docum-est.nat-operacao
             AND item-doc-est.sequencia    = int(string(i-seq-aux) + "0") :

            ASSIGN c-cfop        = STRING(nfe-cte-inf.ide-CFOP)
                   d-icms-BC     = IF nfe-cte-inf.imp-ICMS-pICMS = 0 THEN nfe-cte-inf.imp-ICMS-vBC ELSE item-doc-est.base-icm[1]
                   d-ICMS        = nfe-cte-inf.imp-ICMS-pICMS
                   d-Vicms       = IF nfe-cte-inf.imp-ICMS-pICMS = 0 THEN nfe-cte-inf.imp-ICMS-vICMS ELSE item-doc-est.valor-icm[1]
                   i-cst-icms    = nfe-cte-inf.imp-ICMS-CST
                   d-RedBc       = nfe-cte-inf.imp-ICMS-pRedBC.
  
            ASSIGN i-fator = i-fator + 1.
           
        END.       

    END.

END.


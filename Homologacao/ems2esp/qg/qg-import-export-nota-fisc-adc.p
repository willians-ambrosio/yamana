DEF VAR d-total     LIKE it-doc-fisc.vl-tot-item.   
DEF VAR d-total-liq LIKE it-doc-fisc.vl-merc-liq. 

DEF BUFFER b-it-doc-fisc FOR it-doc-fisc.
/**************************************************************************/
DEF VAR X-est LIKE doc-fiscal.cod-estabel .  
DEF VAR X-ser LIKE doc-fiscal.serie       . 
DEF VAR X-doc LIKE doc-fiscal.nr-doc-fis  .
DEF VAR i-emi LIKE doc-fiscal.cod-emitente.
DEF VAR X-nat LIKE doc-fiscal.nat-operacao.
DEF VAR x-uf  LIKE doc-fiscal.estado      .
DEF VAR d-dat LIKE doc-fiscal.dt-docto    .
DEF VAR d-bas LIKE nota-fisc-adc.val-base-calc-icms.
DEF VAR d-icm LIKE nota-fisc-adc.val-icms          .
DEF VAR d-alq LIKE nota-fisc-adc.val-aliq-icms     .
DEF VAR x-adc as char format "x(3)"                .
DEF VAR d-val LIKE nota-fisc-adc.val-icms          .
DEFINE VARIABLE i-seq AS INTEGER     NO-UNDO.

output to  \\Client\F$\Projetos\Yamana\Homologacao\ems2esp\qg\c197-log.txt.

input from \\Client\F$\Projetos\Yamana\Homologacao\ems2esp\qg\c197.txt.
repeat:   
    import delimiter ";"
        X-est
        X-ser
        x-doc
        i-emi
        x-nat
        x-uf
        d-dat
        d-val.

    CASE LENGTH(x-doc):
        WHEN 1 THEN ASSIGN x-doc = "000000" + x-doc.
        WHEN 2 THEN ASSIGN x-doc = "00000"  + x-doc.
        WHEN 3 THEN ASSIGN x-doc = "0000"   + x-doc.
        WHEN 4 THEN ASSIGN x-doc = "000"    + x-doc.
        WHEN 5 THEN ASSIGN x-doc = "00"     + x-doc.
        WHEN 6 THEN ASSIGN x-doc = "0"      + x-doc.
    END CASE.

    FIND FIRST docum-est WHERE
               docum-est.cod-estabel = X-est AND
               docum-est.serie       = x-ser AND
               docum-est.nro-docto   = x-doc NO-LOCK NO-ERROR.
    IF NOT AVAIL docum-est THEN NEXT.

    FIND FIRST natur-oper WHERE
               natur-oper.nat-operacao = docum-est.nat-operacao NO-LOCK NO-ERROR.    
    IF NOT AVAIL natur-oper THEN NEXT. 

    FOR EACH doc-fiscal WHERE
             doc-fiscal.cod-estabel  = docum-est.cod-estabel  and
             doc-fiscal.serie        = docum-est.serie        and
             doc-fiscal.nr-doc-fis   = docum-est.nro-docto    and             
             doc-fiscal.cod-emitente = docum-est.cod-emitente AND
             doc-fiscal.nat-operacao = docum-est.nat-operacao NO-LOCK,
        EACH it-doc-fisc OF doc-fiscal       NO-LOCK.
    
        IF it-doc-fisc.aliquota-icm = 4 THEN 
           ASSIGN d-alq = 13.    
        ELSE 
        DO:
          IF (doc-fiscal.estado = "SP"  OR 
              doc-fiscal.estado = "RJ"  OR
              doc-fiscal.estado = "MG"  OR
              doc-fiscal.estado = "SC"  OR
              doc-fiscal.estado = "PR"  OR
              doc-fiscal.estado = "RS") THEN 
              ASSIGN d-alq = 10.  
        END. 
        
        ASSIGN d-total = 0
               d-total-liq = 0
               d-icm = 0
               d-bas = 0.

        FOR EACH b-it-doc-fisc OF doc-fiscal  NO-LOCK:
    
            ASSIGN d-total     = d-total     + b-it-doc-fisc.vl-tot-item
                   d-total-liq = d-total-liq + b-it-doc-fisc.vl-merc-liq                        
                   d-icm       = (d-val / d-total-liq) * b-it-doc-fisc.vl-merc-liq
                   d-bas       = d-icm * 100 / d-alq. 
        END.

        ASSIGN i-seq = (it-doc-fisc.nr-seq-doc / 10).
            
        FIND nota-fisc-adc WHERE 
             nota-fisc-adc.cod-estab        = it-doc-fisc.cod-estabel   AND 
             nota-fisc-adc.cod-serie        = it-doc-fisc.serie         AND 
             nota-fisc-adc.cod-nota-fisc    = it-doc-fisc.nr-doc-fis    AND 
             nota-fisc-adc.cdn-emitente     = it-doc-fisc.cod-emitente  AND 
             nota-fisc-adc.cod-natur-operac = it-doc-fisc.nat-operacao  AND 
             nota-fisc-adc.idi-tip-dado     = 7                         AND 
             nota-fisc-adc.num-seq          = i-seq                     EXCLUSIVE-LOCK NO-ERROR.                            
        ASSIGN x-adc = IF AVAIL nota-fisc-adc THEN "sim" ELSE "nao".

        IF x-adc = "sim" THEN NEXT.

        IF d-bas = ? THEN
           ASSIGN d-bas = 0.
        
        EXPORT DELIMITER ";"              
             doc-fiscal.cod-estabel  
             doc-fiscal.serie        
             doc-fiscal.nr-doc-fis  
             doc-fiscal.cod-emitente 
             doc-fiscal.nat-operacao 
             it-doc-fisc.it-codigo
             it-doc-fisc.nr-seq-doc
             d-bas
             d-alq
             d-icm.
    END.
END.
INPUT CLOSE.
OUTPUT CLOSE.




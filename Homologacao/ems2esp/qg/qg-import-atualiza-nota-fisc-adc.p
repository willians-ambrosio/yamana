DEF VAR d-total     LIKE it-doc-fisc.vl-tot-item.   
DEF VAR d-total-liq LIKE it-doc-fisc.vl-merc-liq. 

DEF BUFFER b-it-doc-fisc FOR it-doc-fisc.
/**************************************************************************/
DEF VAR X-est             LIKE doc-fiscal.cod-estabel .  
DEF VAR X-ser             LIKE doc-fiscal.serie       . 
DEF VAR X-doc             LIKE doc-fiscal.nr-doc-fis  .
DEF VAR i-emi             LIKE doc-fiscal.cod-emitente.
DEF VAR X-nat             LIKE doc-fiscal.nat-operacao.
DEF VAR x-uf              LIKE doc-fiscal.estado      .
DEF VAR d-dat             LIKE doc-fiscal.dt-docto    .
DEF VAR d-bas             LIKE nota-fisc-adc.val-base-calc-icms.
DEF VAR d-icm             LIKE nota-fisc-adc.val-icms          .
DEF VAR d-alq             LIKE nota-fisc-adc.val-aliq-icms     .
DEF VAR x-adc             as char format "x(3)"                .
DEF VAR d-val             LIKE nota-fisc-adc.val-icms          .
DEF VAR x-cod-estabel     LIKE doc-fiscal.cod-estabel          .
DEF VAR x-serie           LIKE doc-fiscal.serie                .
DEF VAR x-nr-doc-fis      LIKE doc-fiscal.nr-doc-fis           .
DEF VAR x-cod-emitente    LIKE doc-fiscal.cod-emitente         .
DEF VAR x-nat-operacao    LIKE doc-fiscal.nat-operacao         .
DEF VAR x-it-codigo       LIKE it-doc-fisc.it-codigo           .
DEF VAR x-nr-seq-doc      LIKE it-doc-fisc.nr-seq-doc          .
DEF VAR x-d-bas           LIKE nota-fisc-adc.val-base-calc-icms. 
DEF VAR x-d-alq           LIKE nota-fisc-adc.val-aliq-icms     . 
DEF VAR x-d-icm           LIKE nota-fisc-adc.val-icms          .

DEFINE VARIABLE x-atualiza AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-seq AS INTEGER NO-UNDO.

output to  \\Client\F$\Projetos\Yamana\Homologacao\ems2esp\qg\c197-log-atualizados.txt.

input from \\Client\F$\Projetos\Yamana\Homologacao\ems2esp\qg\c197-log.txt.
repeat:   
    import delimiter ";"
                     x-cod-estabel                    /*doc-fiscal.cod-estabel               */
                     x-serie                          /*doc-fiscal.serie                     */
                     x-nr-doc-fis                     /*doc-fiscal.nr-doc-fis                */
                     x-cod-emitente                   /*doc-fiscal.cod-emitente              */
                     x-nat-operacao                   /*doc-fiscal.nat-operacao              */
                     x-it-codigo                      /*it-doc-fisc.it-codigo                */
                     x-nr-seq-doc                     /*it-doc-fisc.nr-seq-doc               */
                     x-d-bas                          /*d-bas                                */
                     x-d-alq                          /*x-d-alq                              */
                     x-d-icm.                         /*x-d-icm.                             */
    
    FOR EACH doc-fiscal WHERE 
             doc-fiscal.cod-estabel  = x-cod-estabel  AND 
             doc-fiscal.serie        = x-serie        AND 
             doc-fiscal.nr-doc-fis   = x-nr-doc-fis   AND 
             doc-fiscal.cod-emitente = x-cod-emitente AND 
             doc-fiscal.nat-operacao = x-nat-operacao NO-LOCK,
        EACH it-doc-fisc OF doc-fiscal  WHERE
             it-doc-fisc.it-codigo   = x-it-codigo  AND 
             it-doc-fisc.nr-seq-doc  = x-nr-seq-doc NO-LOCK.
    
/*         IF it-doc-fisc.aliquota-icm = 4 THEN                                         */
/*            ASSIGN x-d-alq = 13.                                                      */
/*         ELSE                                                                         */
/*         DO:                                                                          */
/*           IF (doc-fiscal.estado = "SP" OR                                            */
/*               doc-fiscal.estado = "RJ" OR                                            */
/*               doc-fiscal.estado = "MG" OR                                            */
/*               doc-fiscal.estado = "SC" OR                                            */
/*               doc-fiscal.estado = "PR" OR                                            */
/*               doc-fiscal.estado = "RS") THEN                                         */
/*               ASSIGN x-d-alq = 10.                                                   */
/*         END.                                                                         */
/*                                                                                      */
/*         ASSIGN d-total = 0                                                           */
/*                d-total-liq = 0                                                       */
/*                x-d-icm = 0                                                           */
/*                x-d-bas = 0.                                                          */
/*                                                                                      */
/*         FOR EACH b-it-doc-fisc OF doc-fiscal  NO-LOCK:                               */
/*                                                                                      */
/*             ASSIGN d-total     = d-total     + b-it-doc-fisc.vl-tot-item             */
/*                    d-total-liq = d-total-liq + b-it-doc-fisc.vl-merc-liq             */
/*                    x-d-icm       = (d-val / d-total-liq) * b-it-doc-fisc.vl-merc-liq */
/*                    x-d-bas       = x-d-icm * 100 / x-d-alq.                          */
/*         END.                                                                         */

        ASSIGN i-seq = (it-doc-fisc.nr-seq-doc / 10).
            
        FIND nota-fisc-adc WHERE 
             nota-fisc-adc.cod-estab        = it-doc-fisc.cod-estabel          AND 
             nota-fisc-adc.cod-serie        = it-doc-fisc.serie                AND 
             nota-fisc-adc.cod-nota-fisc    = it-doc-fisc.nr-doc-fis           AND
             nota-fisc-adc.cdn-emitente     = it-doc-fisc.cod-emitente         AND 
             nota-fisc-adc.cod-natur-operac = it-doc-fisc.nat-operacao         AND 
             nota-fisc-adc.idi-tip-dado     = 7                                AND 
             nota-fisc-adc.num-seq          = i-seq                            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL nota-fisc-adc THEN
        DO:        
           create nota-fisc-adc.
           assign nota-fisc-adc.cod-estab          = it-doc-fisc.cod-estabel         
                  nota-fisc-adc.cod-serie          = it-doc-fisc.serie               
                  nota-fisc-adc.cod-nota-fisc      = it-doc-fisc.nr-doc-fis          
                  nota-fisc-adc.cdn-emitente       = it-doc-fisc.cod-emitente        
                  nota-fisc-adc.cod-natur-operac   = it-doc-fisc.nat-operacao        
                  nota-fisc-adc.idi-tip-dado       = 7                               
                  nota-fisc-adc.num-seq            = i-seq
                  nota-fisc-adc.cod-item           = it-doc-fisc.it-codigo
                  nota-fisc-adc.cod-ajust          = "GO40000029"
                  nota-fisc-adc.val-base-calc-icms = x-d-bas  
                  nota-fisc-adc.val-icms           = x-d-icm
                  nota-fisc-adc.val-aliq-icms      = x-d-alq.

           
           DISPLAY nota-fisc-adc.cod-estab         
                   nota-fisc-adc.cod-serie         
                   nota-fisc-adc.cod-nota-fisc     
                   nota-fisc-adc.cdn-emitente      
                   nota-fisc-adc.cod-natur-operac  
                   nota-fisc-adc.idi-tip-dado      
                   nota-fisc-adc.num-seq           
                   nota-fisc-adc.cod-item          
                   nota-fisc-adc.cod-ajust         
                   nota-fisc-adc.val-base-calc-icms
                   nota-fisc-adc.val-icms          
                   nota-fisc-adc.val-aliq-icms WITH SCROLLABLE.


        END. 

        RELEASE nota-fisc-adc.

    END.
END.

INPUT CLOSE.
OUTPUT CLOSE.

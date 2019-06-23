/* 98101001 */
/* 54104011 */
/* 54104012 */
/* 54104013 */
/* 54104014 */
/* 54104011 */
/* 54104012 */
/* 54104013 */
/* 54104014 */

def var h-acomp               as handle                  no-undo.

DEFINE BUFFER bf-movto_tit_ap FOR movto_tit_ap.

/* run utp/ut-acomp.p persistent set h-acomp. */
/* run pi-inicializar in h-acomp (input "Filtrando, Aguarde..."). */
/*    */
/*    */
/* OUTPUT TO "c:\temp\bf-movto_tit_ap.csv" NO-CONVERT. */



FOR EACH bf-movto_tit_ap NO-LOCK WHERE 
          bf-movto_tit_ap.cod_estab >= "301"
      AND bf-movto_tit_ap.cod_estab <= "307"
      AND bf-movto_tit_ap.dat_transacao >= 01/01/2018
      AND bf-movto_tit_ap.dat_transacao <= TODAY,
    FIRST tit_ap OF bf-movto_tit_ap NO-LOCK
       BY bf-movto_tit_ap.dat_transacao:
       
     
/*     RUN pi-acompanhar IN h-acomp (input "Data: " + string(bf-movto_tit_ap.dat_transacao) + " - ID: " + string(bf-movto_tit_ap.num_id_tit_ap)).     */

        FIND FIRST movto_tit_ap OF tit_ap where 
                   movto_tit_ap.ind_trans_ap = "Baixa" NO-LOCK NO-ERROR. 
        IF AVAIL movto_tit_ap THEN
        do:          
           FIND FIRST aprop_ctbl_ap OF movto_tit_ap where
                      aprop_ctbl_ap.ind_natur_lancto_ctbl = "DB" NO-LOCK NO-ERROR.
           IF AVAIL aprop_ctbl_ap THEN
           DO:
              FIND FIRST es-cat-code-cta-ex WHERE
                         es-cat-code-cta-ex.cod_empresa        = aprop_ctbl_ap.cod_empresa          AND
                         es-cat-code-cta-ex.cod_plano_cta_ctbl = aprop_ctbl_ap.cod_plano_cta_ctbl   AND
                         es-cat-code-cta-ex.cod_cta_ctbl       = aprop_ctbl_ap.cod_cta_ctbl         NO-LOCK NO-ERROR.
              IF NOT AVAIL es-cat-code-cta-ex THEN
              DO:
                 FIND FIRST movto_tit_ap OF tit_ap where 
                            movto_tit_ap.ind_trans_ap = "Implanta‡Æo" NO-LOCK NO-ERROR.   
                 IF NOT AVAIL movto_tit_ap THEN NEXT.    
                 
                 FIND FIRST aprop_ctbl_ap OF movto_tit_ap where
                            aprop_ctbl_ap.ind_natur_lancto_ctbl = "DB" NO-LOCK NO-ERROR.
                 IF NOT AVAIL aprop_ctbl_ap THEN NEXT.
                         
                              
              END.
           END.
           ELSE
           DO:
              FIND FIRST movto_tit_ap OF tit_ap where 
                         movto_tit_ap.ind_trans_ap = "Implanta‡Æo" NO-LOCK NO-ERROR.   
              IF NOT AVAIL movto_tit_ap THEN NEXT.    
              
              FIND FIRST aprop_ctbl_ap OF movto_tit_ap where
                          aprop_ctbl_ap.ind_natur_lancto_ctbl = "DB" NO-LOCK NO-ERROR.
              IF NOT AVAIL aprop_ctbl_ap THEN NEXT.
                                        
           END.
        END.  
        ELSE
        DO:
            FIND FIRST movto_tit_ap OF tit_ap where 
                       movto_tit_ap.ind_trans_ap = "Implanta‡Æo" NO-LOCK NO-ERROR.   
            IF NOT AVAIL movto_tit_ap THEN NEXT.
            
            FIND FIRST aprop_ctbl_ap OF movto_tit_ap where
                       aprop_ctbl_ap.ind_natur_lancto_ctbl = "DB" NO-LOCK NO-ERROR.
            IF NOT AVAIL aprop_ctbl_ap THEN NEXT.
                    
        END.  
        
        

                         
        
        IF movto_tit_ap.ind_trans_ap = "Baixa" THEN
           disp bf-movto_tit_ap.dat_transacao movto_tit_ap.ind_trans_ap aprop_ctbl_ap.cod_cta_ctbl .          
/*    */
/*    */
/*    */
/*    */
/*                 FIND FIRST movto_tit_ap OF tit_ap where */
/*                            movto_tit_ap.ind_trans_ap = "Implanta?’o" NO-LOCK NO-ERROR. */
/*                 IF NOT AVAIL movto_tit_ap THEN */
/* /*                 IF NOT AVAIL movto_tit_ap THEN */ */
/* /*                     FIND FIRST movto_tit_ap OF tit_ap where */ */
/* /*                                movto_tit_ap.ind_trans_ap = "Estorno de Baixa" NO-LOCK NO-ERROR. */ */
/*                 IF NOT AVAIL movto_tit_ap THEN NEXT. */
/*    */
/*                 FIND FIRST aprop_ctbl_ap OF movto_tit_ap where */
/*                            aprop_ctbl_ap.ind_natur_lancto_ctbl = "DB" NO-LOCK NO-ERROR. */
/*                 IF NOT AVAIL aprop_ctbl_ap THEN NEXT. */
/*    */
/*     disp bf-movto_tit_ap.dat_transacao movto_tit_ap.ind_trans_ap. */
/*    */
/*     FOR EACH aprop_ctbl_ap OF bf-movto_tit_ap NO-LOCK: */
/*    */
/*    */
/*    */
/*         /* Quando n?o localizar o lan?amento dever? ser verificado o cadastro esapb013 */ */
/*            FIND FIRST es-cat-code-cta-ex WHERE */
/*                       es-cat-code-cta-ex.cod_empresa        = aprop_ctbl_ap.cod_empresa          AND */
/*                       es-cat-code-cta-ex.cod_plano_cta_ctbl = aprop_ctbl_ap.cod_plano_cta_ctbl   AND */
/*                       es-cat-code-cta-ex.cod_cta_ctbl       = aprop_ctbl_ap.cod_cta_ctbl         NO-LOCK NO-ERROR. */
/*            IF NOT AVAIL es-cat-code-cta-ex THEN NEXT. */
/*    */
/*            disp bf-movto_tit_ap.dat_transacao. */
/*    */
/* /*         EXPORT DELIMITER ";" */ */
/* /*              aprop_ctbl_ap.ind_natur_lancto_ctbl */ */
/* /*              es-cat-code-cta-ex.cod_cta_ctbl */ */
/* /*              bf-movto_tit_ap.ind_trans_ap. */ */
/*    */
/*    */
/*     END. */
END.

/* OUTPUT CLOSE. */
/*    */
/* RUN pi-finalizar in h-acomp. */

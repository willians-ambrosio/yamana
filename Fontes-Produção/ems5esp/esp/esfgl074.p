
DEF BUFFER bf_lote_ctbl FOR lote_ctbl.

FIND FIRST lote_ctbl WHERE lote_ctbl.num_lote_ctbl = 0 NO-LOCK NO-ERROR.
IF AVAIL lote_ctbl THEN DO:
    FIND FIRST es_cons_lotes EXCLUSIVE-LOCK 
        WHERE es_cons_lotes.cod_empresa_orig = lote_ctbl.cod_empresa
          AND es_cons_lotes.num_lote_orig    = lote_ctbl.num_lote_ctbl NO-ERROR.
    IF AVAIL es_cons_lotes THEN DO:
        FIND FIRST bf_lote_ctbl EXCLUSIVE-LOCK 
            WHERE bf_lote_ctbl.cod_empresa   = es_cons_lotes.cod_empresa_dest 
              AND bf_lote_ctbl.num_lote_ctbl = es_cons_lotes.num_lote_dest NO-ERROR.
        IF AVAIL bf_lote_ctbl THEN DO:            
            FOR EACH lancto_ctbl EXCLUSIVE-LOCK 
                WHERE lancto_ctbl.cod_empresa   = bf_lote_ctbl.cod_empresa
                  AND lancto_ctbl.num_lote_ctbl = bf_lote_ctbl.num_lote_ctbl:

                FOR EACH item_lancto_ctbl EXCLUSIVE-LOCK 
                    WHERE item_lancto_ctbl.num_lote_ctbl   = lancto_ctbl.num_lote_ctbl
                      AND item_lancto_ctbl.num_lancto_ctbl = lancto_ctbl.num_lancto_ctbl:

                    FOR EACH aprop_lancto_ctbl EXCLUSIVE-LOCK
                       WHERE aprop_lancto_ctbl.num_lote_ctbl       = item_lancto_ctbl.num_lote_ctbl          
                         AND aprop_lancto_ctbl.num_lancto_ctbl     = item_lancto_ctbl.num_lancto_ctbl        
                         AND aprop_lancto_ctbl.num_seq_lancto_ctbl = item_lancto_ctbl.num_seq_lancto_ctbl:

                        FOR EACH aprop_ctbl_cta_pat EXCLUSIVE-LOCK 
                            WHERE aprop_ctbl_cta_pat.num_id_aprop_lancto_ctbl_cr = aprop_lancto_ctbl.num_id_aprop_lancto_ctbl:
                            DELETE aprop_ctbl_cta_pat.
                        END.
                        DELETE aprop_lancto_ctbl.
                    END.
                    DELETE item_lancto_ctbl.
                END.
                FOR EACH localiz_lancto_ctbl EXCLUSIVE-LOCK 
                    WHERE localiz_lancto_ctbl.num_lote_ctbl   = lancto_ctbl.num_lote_ctbl  
                      AND localiz_lancto_ctbl.num_lancto_ctbl = lancto_ctbl.num_lancto_ctbl:

                    FOR EACH histor_localiz_lancto EXCLUSIVE-LOCK 
                        WHERE histor_localiz_lancto.num_lote_ctbl   = localiz_lancto_ctbl.num_lote_ctbl
                          AND histor_localiz_lancto.num_lancto_ctbl = localiz_lancto_ctbl.num_lancto_ctbl
                          AND histor_localiz_lancto.num_maq_envio   = localiz_lancto_ctbl.num_maq_envio:
                        DELETE histor_localiz_lancto.
                    END.
                    DELETE localiz_lancto_ctbl.
                END.
                FOR EACH tot_lancto_ctbl EXCLUSIVE-LOCK 
                    WHERE tot_lancto_ctbl.num_lote_ctbl = lancto_ctbl.num_lote_ctbl     
                      AND tot_lancto_ctbl.num_lancto_ctbl = lancto_ctbl.num_lancto_ctbl:
                    DELETE tot_lancto_ctbl.
                END.               
                DELETE lancto_ctbl.
            END.
            DELETE bf_lote_ctbl.
        END.
        DELETE es_cons_lotes.
    END.    
END.



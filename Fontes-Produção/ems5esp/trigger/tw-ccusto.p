/*------------------------------------------------------------------------
    File        : tw-ccusto
    Purpose     :

    Syntax      :

    Description : Trigger para criar centro de custo na empresa CAN

    Author(s)   : Rog‚rio Dias
    Created     : 
    Notes       :
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ------- Defini‡Æo de Parƒmetros ------ */
DEF PARAMETER BUFFER b-ccusto     FOR emsbas.ccusto.
DEF PARAMETER BUFFER b-old-ccusto FOR emsbas.ccusto.
 
/* ------- Defini‡Æo de Buffer ------ */
DEF BUFFER buf_ccusto     FOR emsbas.ccusto.
DEF BUFFER unid_negoc     FOR emsbas.unid_negoc.
DEF BUFFER b_lancto_ctbl_rec FOR lancto_ctbl.

DEF BUFFER bf_ccusto FOR ems5.ccusto.
DEF VAR c_cod_estab_orig LIKE estabelecimento.cod_estab NO-UNDO.

DEF BUFFER bf_restric_ccusto FOR restric_ccusto.
DEF BUFFER bf_ccusto_unid_negoc FOR ccusto_unid_negoc.
DEF BUFFER bf_segur_ccusto      FOR segur_ccusto.

IF b-ccusto.cod_empresa <> 'CAN' THEN DO:
    IF NOT CAN-FIND(FIRST bf_ccusto
                    WHERE bf_ccusto.cod_plano_ccusto = b-ccusto.cod_plano_ccusto
                      AND bf_ccusto.cod_ccusto       = b-ccusto.cod_ccusto
                      AND bf_ccusto.cod_empresa      = 'CAN' ) THEN DO:
        CREATE bf_ccusto.
        BUFFER-COPY b-ccusto EXCEPT cod_empresa TO bf_ccusto.
        ASSIGN bf_ccusto.cod_empresa    = 'CAN'
               bf_ccusto.dat_inic_valid = 01/01/0001 
               bf_ccusto.dat_fim_valid  = 12/31/9999.

        FOR EACH es_cons_estab_emp NO-LOCK:
            IF NOT CAN-FIND(FIRST es_cons_ccusto
                            WHERE es_cons_ccusto.cod_plano_ccusto = b-ccusto.cod_plano_ccusto
                              AND es_cons_ccusto.cod_ccusto       = b-ccusto.cod_ccusto
                              AND es_cons_ccusto.num_id_estab_emp = es_cons_estab_emp.num_id_estab_emp
                              AND es_cons_ccusto.cod_empresa      = 'CAN'  ) THEN DO:
                CREATE es_cons_ccusto.
                ASSIGN es_cons_ccusto.cod_ccusto            = b-ccusto.cod_ccusto
                       es_cons_ccusto.cod_ccusto_para       = b-ccusto.cod_ccusto
                       es_cons_ccusto.cod_empresa           = 'CAN'
                       es_cons_ccusto.cod_plano_ccusto      = b-ccusto.cod_plano_ccusto
                       es_cons_ccusto.cod_plano_ccusto_para = b-ccusto.cod_plano_ccusto
                       es_cons_ccusto.num_id_estab_emp      = es_cons_estab_emp.num_id_estab_emp.                          
            END.
        END.
        
        FOR EACH unid_negoc NO-LOCK:
            IF NOT(CAN-FIND(FIRST bf_ccusto_unid_negoc
                            WHERE bf_ccusto_unid_negoc.cod_empresa      = 'CAN'
                              AND bf_ccusto_unid_negoc.cod_ccusto       = b-ccusto.cod_ccusto
                              AND bf_ccusto_unid_negoc.cod_plano_ccusto = b-ccusto.cod_plano_ccusto
                              AND bf_ccusto_unid_negoc.cod_unid_negoc   = unid_negoc.cod_unid_negoc)) THEN DO:
                CREATE bf_ccusto_unid_negoc.
                ASSIGN bf_ccusto_unid_negoc.cod_ccusto       = b-ccusto.cod_ccusto
                       bf_ccusto_unid_negoc.cod_empresa      = 'CAN'
                       bf_ccusto_unid_negoc.cod_plano_ccusto = b-ccusto.cod_plano_ccusto
                       bf_ccusto_unid_negoc.cod_unid_negoc   = unid_negoc.cod_unid_negoc.                      
            END.
        END.

        IF NOT(CAN-FIND(FIRST bf_segur_ccusto
                        WHERE bf_segur_ccusto.cod_empresa = 'CAN'
                          AND bf_segur_ccusto.cod_ccusto  = b-ccusto.cod_ccusto
                          AND bf_segur_ccusto.cod_plano_ccusto = b-ccusto.cod_plano_ccusto )) THEN DO:
            CREATE bf_segur_ccusto.
            ASSIGN bf_segur_ccusto.cod_ccusto       = b-ccusto.cod_ccusto
                   bf_segur_ccusto.cod_empresa      = 'CAN'
                   bf_segur_ccusto.cod_grp_usuar    = '*'
                   bf_segur_ccusto.cod_plano_ccusto = b-ccusto.cod_plano_ccusto.                                                                        
        END.
    END.    
END.

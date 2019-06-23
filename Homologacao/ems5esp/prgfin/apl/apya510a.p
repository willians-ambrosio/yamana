{prgfin/apl/apya510.i}


DEFINE INPUT PARAMETER ip-log_operac_financ_com_sdo     LIKE  v_log_operac_financ_com_sdo     NO-UNDO. 
DEFINE INPUT PARAMETER ip-log_mostra_operac_sem_sdo     LIKE  v_log_mostra_operac_sem_sdo     NO-UNDO.
DEFINE INPUT PARAMETER ip-log_mostra_operac_detdo       LIKE  v_log_mostra_operac_detdo       NO-UNDO.
DEFINE INPUT PARAMETER ip-qtd_dias_curto_praz           LIKE  v_qtd_dias_curto_praz           NO-UNDO.
DEFINE INPUT PARAMETER ip-log_mostra_movto_operac       LIKE  v_log_mostra_movto_operac       NO-UNDO.
DEFINE INPUT PARAMETER ip-log_mostra_tax_operac         LIKE  v_log_mostra_tax_operac         NO-UNDO.
DEFINE INPUT PARAMETER ip-log_mostra_despes_operac      LIKE  v_log_mostra_despes_operac      NO-UNDO.
DEFINE INPUT PARAMETER ip-log_mostra_parc_operac        LIKE  v_log_mostra_parc_operac        NO-UNDO.
DEFINE INPUT PARAMETER ip-log_mostra_docto_operac       LIKE  v_log_mostra_docto_operac       NO-UNDO.
DEFINE INPUT PARAMETER ip-log_mostra_gartia_operac      LIKE  v_log_mostra_gartia_operac      NO-UNDO.
DEFINE INPUT PARAMETER ip-log_mostra_fiador_operac      LIKE  v_log_mostra_fiador_operac      NO-UNDO.
DEFINE INPUT PARAMETER ip-cod_banco_ini                 LIKE  v_cod_banco_ini                 NO-UNDO.
DEFINE INPUT PARAMETER ip-cod_banco_fim                 LIKE  v_cod_banco_fim                 NO-UNDO.
DEFINE INPUT PARAMETER ip-cod_produt_financ_ini         LIKE  v_cod_produt_financ_ini         NO-UNDO.
DEFINE INPUT PARAMETER ip-cod_produt_financ_fim         LIKE  v_cod_produt_financ_fim         NO-UNDO.
DEFINE INPUT PARAMETER ip-cod_operac_financ_ini         LIKE  v_cod_operac_financ_ini         NO-UNDO.
DEFINE INPUT PARAMETER ip-cod_operac_financ_fim         LIKE  v_cod_operac_financ_fim         NO-UNDO.
DEFINE INPUT PARAMETER ip-dat_operac_financ_ini         LIKE  v_dat_operac_financ_ini         NO-UNDO.
DEFINE INPUT PARAMETER ip-dat_operac_financ_fim         LIKE  v_dat_operac_financ_fim         NO-UNDO.
DEFINE INPUT PARAMETER ip-dat_vencto_operac_financ_ini  LIKE  v_dat_vencto_operac_financ_ini  NO-UNDO.
DEFINE INPUT PARAMETER ip-dat_vencto_operac_financ_fim  LIKE  v_dat_vencto_operac_financ_fim  NO-UNDO.

DEFINE OUTPUT PARAMETER TABLE FOR tt_operac_financ.           
DEFINE OUTPUT PARAMETER TABLE FOR tt_movto_operac_financ.     
DEFINE OUTPUT PARAMETER TABLE FOR tt_indic_econ_operac_financ.
DEFINE OUTPUT PARAMETER TABLE FOR tt_despes_operac_financ.    
DEFINE OUTPUT PARAMETER TABLE FOR tt_parc_operac_financ.      
DEFINE OUTPUT PARAMETER TABLE FOR tt_documen_operac_financ.   
DEFINE OUTPUT PARAMETER TABLE FOR tt_fiador_operac_financ.    
DEFINE OUTPUT PARAMETER TABLE FOR tt_gartia_operac_financ.    


ASSIGN v_log_operac_financ_com_sdo    = ip-log_operac_financ_com_sdo   
       v_log_mostra_operac_sem_sdo    = ip-log_mostra_operac_sem_sdo   
       v_log_mostra_operac_detdo      = ip-log_mostra_operac_detdo     
       v_qtd_dias_curto_praz          = ip-qtd_dias_curto_praz         
       v_log_mostra_movto_operac      = ip-log_mostra_movto_operac     
       v_log_mostra_tax_operac        = ip-log_mostra_tax_operac       
       v_log_mostra_despes_operac     = ip-log_mostra_despes_operac    
       v_log_mostra_parc_operac       = ip-log_mostra_parc_operac      
       v_log_mostra_docto_operac      = ip-log_mostra_docto_operac     
       v_log_mostra_gartia_operac     = ip-log_mostra_gartia_operac    
       v_log_mostra_fiador_operac     = ip-log_mostra_fiador_operac    
       v_cod_banco_ini                = ip-cod_banco_ini               
       v_cod_banco_fim                = ip-cod_banco_fim               
       v_cod_produt_financ_ini        = ip-cod_produt_financ_ini       
       v_cod_produt_financ_fim        = ip-cod_produt_financ_fim       
       v_cod_operac_financ_ini        = ip-cod_operac_financ_ini       
       v_cod_operac_financ_fim        = ip-cod_operac_financ_fim       
       v_dat_operac_financ_ini        = ip-dat_operac_financ_ini       
       v_dat_operac_financ_fim        = ip-dat_operac_financ_fim       
       v_dat_vencto_operac_financ_ini = ip-dat_vencto_operac_financ_ini
       v_dat_vencto_operac_financ_fim = ip-dat_vencto_operac_financ_fim.

run pi_inicializa_operac_financ_emprestimo.


PROCEDURE pi_inicializa_operac_financ_emprestimo:

    for each tt_operac_financ exclusive-lock:
        delete tt_operac_financ.
    end.
    for each tt_movto_operac_financ exclusive-lock:
        delete tt_movto_operac_financ.
    end.
    for each tt_indic_econ_operac_financ exclusive-lock:
        delete tt_indic_econ_operac_financ.
    end.
    for each tt_despes_operac_financ exclusive-lock:
        delete tt_despes_operac_financ.
    end. 
    for each tt_parc_operac_financ exclusive-lock:
        delete tt_parc_operac_financ.
    end.
    for each tt_documen_operac_financ exclusive-lock:
        delete tt_documen_operac_financ.
    end.
    for each tt_fiador_operac_financ exclusive-lock:
        delete tt_fiador_operac_financ.
    end.
    for each tt_gartia_operac_financ exclusive-lock:
        delete tt_gartia_operac_financ.
    end.
    assign v_log_empres_concedid = no
           v_cod_empresa         = '' 
           v_cod_empres_fim      = 'ZZZ' .
        
    run prgfin/apl/apl313za.py (Input  1,
                                Input  v_log_operac_financ_com_sdo,
                                Input  v_log_mostra_operac_sem_sdo,
                                Input  v_log_mostra_operac_detdo,
                                Input  v_qtd_dias_curto_praz,
                                Input  v_log_mostra_movto_operac,
                                Input  v_log_mostra_tax_operac,
                                Input  v_log_mostra_despes_operac,
                                Input  v_log_mostra_parc_operac,
                                Input  v_log_mostra_docto_operac,
                                Input  v_log_mostra_gartia_operac,
                                Input  v_log_mostra_fiador_operac,
                                Input  v_cod_banco_ini,
                                Input  v_cod_banco_fim,
                                Input  v_cod_produt_financ_ini,
                                Input  v_cod_produt_financ_fim,
                                Input  v_cod_operac_financ_ini,
                                Input  v_cod_operac_financ_fim,
                                Input  v_dat_operac_financ_ini,
                                Input  v_dat_operac_financ_fim,
                                Input  v_dat_vencto_operac_financ_ini,
                                Input  v_dat_vencto_operac_financ_fim,
                                output table tt_operac_financ,
                                output table tt_movto_operac_financ,
                                output table tt_indic_econ_operac_financ,
                                output table tt_despes_operac_financ,
                                output table tt_parc_operac_financ,
                                output table tt_documen_operac_financ,
                                output table tt_fiador_operac_financ,
                                output table tt_gartia_operac_financ,
                                Input  v_cod_tip_produt_financ_fim,
                                Input  v_cod_tip_produt_financ_ini,
                                Input  v_log_empres_concedid,
                                Input  v_cod_empresa,
                                Input  v_cod_empres_fim) /*prg_api_operac_financ_emprestimo*/.



    assign v_val_tot_operac_financ       = 0
           v_val_tot_despes_bcia         = 0
           v_val_tot_impto_operac_financ = 0
           v_val_tot_juros_operac_financ = 0
           v_val_tot_sdo_operac_financ   = 0
           v_val_sdo_curto_praz          = 0
           v_val_sdo_longo_praz          = 0
           v_val_sdo_princ_operac_financ = 0
           v_val_tot_pagto_aplic         = 0.

END PROCEDURE. /* pi_inicializa_operac_financ_emprestimo */

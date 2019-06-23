/****************************************************************************************** 
** 	     Programa: cn001-rp.p
**          Autor: Felipe Vieria Do Carmo Pereira
**     Fornecedor: DKP
**       	 Data: 20/11/2018
** Change/Chamado: CHxxxxx
**       Objetivo: Cria tabela de relacionamento de t¡tulos x contrato
**
******************************** CONTROLE DE ALTERA€åES *********************************
** 
** Data         Autor   		Fornecedor    Change/Chamado      Descri‡Æo da Altera‡Æo
** N/A          N/A         	 N/A               N/A	                   N/a
**
****************************** INFORMA€åES ADICIONAIS ************************************
**     PAR¶METROS DE ENTRADA: tt-param
**       PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
**      CADASTRADO NA TABELA: N/A
******************************************************************************************/

IF NOT CAN-FIND(FIRST tt_relacto_tit_ap WHERE
                      tt_relacto_tit_ap.cod_estab     = b_tit_ap.cod_estab AND 
                      tt_relacto_tit_ap.num_id_tit_ap = b_tit_ap.num_id_tit_ap) THEN DO:
    
    CREATE tt_relacto_tit_ap.
    BUFFER-COPY b_tit_ap TO tt_relacto_tit_ap
        ASSIGN tt_relacto_tit_ap.tta_cnd_fornec_orig    = tit_ap.cdn_fornec
               tt_relacto_tit_ap.tta_dat_gerac_movto    = movto_tit_ap.dat_gerac_movto
               tt_relacto_tit_ap.tta_hra_gerac_movto    = movto_tit_ap.hra_gerac_movto
               tt_relacto_tit_ap.nr_contrato            = contrato-for.nr-contrato
               tt_relacto_tit_ap.dt-ini-validade        = contrato-for.dt-ini-validade 
               tt_relacto_tit_ap.dt-ter-validade        = contrato-for.dt-ter-validade 
               tt_relacto_tit_ap.dt-termo               = ext-contrato-for.dt_termo_enc WHEN AVAIL ext-contrato-for 
               tt_relacto_tit_ap.nat-operacao           = docum-est.nat-operacao
               tt_relacto_tit_ap.num-seq-medicao        = medicao-contrat.num-seq-medicao
               tt_relacto_tit_ap.num-seq-item           = medicao-contrat.num-seq-item
               tt_relacto_tit_ap.dat-medicao            = medicao-contrat.dat-medicao 
               tt_relacto_tit_ap.val-medicao            = medicao-contrat.val-medicao
               tt_relacto_tit_ap.vl-glosa               = es-medicao-contrat.vl-glosa-desc WHEN AVAIL es-medicao-contrat
               tt_relacto_tit_ap.tta_row_tit_ap         = ROWID(tit_ap).
               tt_relacto_tit_ap.tta_status             = (IF contrato-for.dt-ini-validade <= TODAY AND contrato-for.dt-ter-validade >= TODAY THEN  
                                                           (IF contrato-for.ind-sit-contrat = 2 THEN "VIGENTE" ELSE ENTRY(contrato-for.ind-sit-contrat,{ininc/i05in065.i 03}))
                                                            ELSE "ENCERRADO").
    IF AVAIL ext-contrato-for AND ext-contrato-for.ind-status = 4 /* Processo Judicial */
         THEN tt_relacto_tit_ap.tta_status = "Processo Judicial".
                                                           
                                                           .
END.

  

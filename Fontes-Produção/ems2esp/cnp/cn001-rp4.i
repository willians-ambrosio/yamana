/****************************************************************************************** 
** 	     Programa: cn001-rp4.i
**          Autor: Daniela Campos
**     Fornecedor: DKP
**       	 Data: 20/11/2018
** Change/Chamado: CHxxxxx
**       Objetivo: Defini‡Æo das tempor rias para o Relat¢rio de Medi‡äes de contrato
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

def temp-table tt_relacto_pend_tit_ap        
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_cod_refer                    as character format "x(10)" label "Referˆncia" column-label "Referˆncia"
    field tta_num_seq_refer                as integer format ">>>9" initial 0 label "Sequˆncia" column-label "Seq"
    field tta_val_relacto_tit_ap           as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor" column-label "Valor"
    field ttv_ind_tip_relacto              as character format "X(15)"      label "Tipo Relacionamento" column-label "Tipo Relac"
    field tta_num_fatur_ap                 as integer format ">>>>,>>>,>>9" initial 0 label "N£mero  Fatura" column-label "Num Fatura"
    .

def temp-table tt_relacto_tit_ap no-undo like tit_ap
    FIELD nr_contrato                      LIKE contrato-for.nr-contrato
    FIELD dt-ter-validade                  LIKE contrato-for.dt-ter-validade
    FIELD dt-ini-validade                  LIKE contrato-for.dt-ini-validade
    FIELD tta_status                       AS CHAR
    FIELD tta_row_tit_ap                   AS ROWID
    FIELD nat-operacao                     LIKE docum-est.nat-operacao   
    field num-seq-medicao                  LIKE medicao-contrat.num-seq-medicao  
    field num-seq-item                     LIKE medicao-contrat.num-seq-item     
    field dat-medicao                      LIKE medicao-contrat.dat-medicao      
    FIELD dt-termo                         LIKE ext-contrato-for.dt_termo_enc
    field val-medicao                      LIKE medicao-contrat.val-medicao      
    field vl-glosa                         LIKE es-medicao-contrat.vl-glosa-desc 
    FIELD tta_cnd_fornec_orig              LIKE tit_ap.cdn_fornec
    field tta_val_relacto_tit_ap           as decimal format "->>>,>>>,>>9.99" decimals 2 initial 0 label "Valor" column-label "Valor"
    field tta_dat_gerac_movto              as date format "99/99/9999" initial ? label "Data Gera‡Æo Movto" column-label "Data Gera‡Æo Movto"
    field tta_hra_gerac_movto              as Character format "99:99:99" label "Hora Gera‡Æo Movto" column-label "Hora Gera‡Æo Movto".

/* Busca os t¡tulos relacionados com as duplicatas */
PROCEDURE pi_criar_tt_relacto_tit_ap:

    /************************* Variable Definition Begin ************************/
    def var v_cod_estab        as CHARACTER format "x(5)":U no-undo.
    def var v_cod_estab_fatur  as CHARACTER format "x(8)":U no-undo. 
    def var v_cod_finalid_econ as CHARACTER format "x(10)":U no-undo.
    def var v_cod_refer        as CHARACTER format "x(10)":U no-undo.
    def var v_log_bxa          as LOGICAL   format "Sim/NÆo" initial YES no-undo.

    DEF BUFFER b_tit_ap FOR tit_ap.
    DEF BUFFER b_movto_tit_ap FOR movto_tit_ap.
    /************************** Variable Definition End *************************/

    cria_temp_table:
    FOR EACH relacto_tit_ap NO-LOCK 
        where relacto_tit_ap.cod_estab     = tit_ap.cod_estab
          and relacto_tit_ap.num_id_tit_ap = tit_ap.num_id_tit_ap:

        /* Procura pelas notas de credito/debito */
        find movto_tit_ap no-lock
             where movto_tit_ap.cod_estab           = relacto_tit_ap.cod_estab_tit_ap_pai
               and movto_tit_ap.num_id_movto_tit_ap = relacto_tit_ap.num_id_movto_tit_ap_pai no-error.
        find b_tit_ap no-lock
             where b_tit_ap.cod_estab = movto_tit_ap.cod_estab
               and b_tit_ap.num_id_tit_ap = movto_tit_ap.num_id_tit_ap
              no-error.
        if avail b_tit_ap then do:
            {cnp\cn001-rp3.i}
        end.
    end /* for cria_temp_table */.

    cria_temp_table:
    FOR each movto_tit_ap no-lock
        where movto_tit_ap.cod_estab = tit_ap.cod_estab
          and movto_tit_ap.num_id_tit_ap = tit_ap.num_id_tit_ap:

        /* Procura pelas notas de credito/debito */
        block1:
        FOR each relacto_tit_ap no-lock
            where relacto_tit_ap.cod_estab_tit_ap_pai = movto_tit_ap.cod_estab
              and relacto_tit_ap.num_id_movto_tit_ap_pai = movto_tit_ap.num_id_movto_tit_ap:
            find b_tit_ap no-lock
                 where b_tit_ap.cod_estab     = relacto_tit_ap.cod_estab
                   and b_tit_ap.num_id_tit_ap = relacto_tit_ap.num_id_tit_ap
                  no-error.
            if avail b_tit_ap then do:
                  {cnp\cn001-rp3.i}  
            end.
        end /* for block1 */.

        /* Procura pelos movimentos pais */
        find b_movto_tit_ap no-lock
             where b_movto_tit_ap.cod_estab           = movto_tit_ap.cod_estab_tit_ap_pai
               and b_movto_tit_ap.num_id_movto_tit_ap = movto_tit_ap.num_id_movto_tit_ap_pai
               and  b_movto_tit_ap.num_id_tit_ap     <> movto_tit_ap.num_id_tit_ap
              no-error.
        if  avail b_movto_tit_ap
        then do:
            find b_tit_ap no-lock
                 where b_tit_ap.cod_estab     = b_movto_tit_ap.cod_estab
                   and b_tit_ap.num_id_tit_ap = b_movto_tit_ap.num_id_tit_ap
                  no-error.
            if  avail b_tit_ap
            then do:
                find tt_relacto_tit_ap no-lock
                     where tt_relacto_tit_ap.cod_estab       = b_tit_ap.cod_estab
                       and tt_relacto_tit_ap.cod_espec_docto = b_tit_ap.cod_espec_docto
                       and tt_relacto_tit_ap.cod_ser_docto   = b_tit_ap.cod_ser_docto
                       and tt_relacto_tit_ap.cdn_fornecedor  = b_tit_ap.cdn_fornecedor
                       and tt_relacto_tit_ap.cod_tit_ap      = b_tit_ap.cod_tit_ap
                       and tt_relacto_tit_ap.cod_parcela     = b_tit_ap.cod_parcela
                     no-error.
                if  not avail tt_relacto_tit_ap
                then do:
                    {cnp\cn001-rp3.i}
                end /* if */.
            end /* if */.
        end /* if */.

        /* Procura pelos movimentos filhos */
        for each b_movto_tit_ap no-lock
             where b_movto_tit_ap.cod_estab_tit_ap_pai    = movto_tit_ap.cod_estab
               and b_movto_tit_ap.num_id_movto_tit_ap_pai = movto_tit_ap.num_id_movto_tit_ap
               and b_movto_tit_ap.num_id_tit_ap          <> movto_tit_ap.num_id_tit_ap
               and not b_movto_tit_ap.ind_trans_ap begins "Estorno" /*l_estorno*/ :

            find b_tit_ap no-lock
                 where b_tit_ap.cod_estab     = b_movto_tit_ap.cod_estab
                   and b_tit_ap.num_id_tit_ap = b_movto_tit_ap.num_id_tit_ap
                  no-error.
            find tt_relacto_tit_ap no-lock
                 where tt_relacto_tit_ap.cod_estab       = b_tit_ap.cod_estab
                   and tt_relacto_tit_ap.cod_espec_docto = b_tit_ap.cod_espec_docto
                   and tt_relacto_tit_ap.cod_ser_docto   = b_tit_ap.cod_ser_docto
                   and tt_relacto_tit_ap.cdn_fornecedor  = b_tit_ap.cdn_fornecedor
                   and tt_relacto_tit_ap.cod_tit_ap      = b_tit_ap.cod_tit_ap
                   and tt_relacto_tit_ap.cod_parcela     = b_tit_ap.cod_parcela
                 no-error.
            if  not avail tt_relacto_tit_ap
            then do:
                {cnp\cn001-rp3.i}
            end /* if */.
        end.

        /* Procura pelos movimentos de IR Gerados */
        compl_block:
        for
            each compl_impto_retid_ap no-lock
                 where compl_impto_retid_ap.num_id_movto_tit_ap_pai = movto_tit_ap.num_id_movto_tit_ap
                   and compl_impto_retid_ap.cod_estab               = movto_tit_ap.cod_estab:

            find b_tit_ap no-lock
                 where b_tit_ap.cod_estab     = compl_impto_retid_ap.cod_estab
                   and b_tit_ap.num_id_tit_ap = compl_impto_retid_ap.num_id_tit_ap
                  no-error.
            if avail b_tit_ap then do:
                
                {cnp\cn001-rp3.i}

                if  b_tit_ap.log_tit_ap_estordo
                then do:
                    run pi_retornar_finalid_indic_econ (Input b_tit_ap.cod_indic_econ,
                                                        Input b_tit_ap.dat_transacao,
                                                        output v_cod_finalid_econ) /*pi_retornar_finalid_indic_econ*/.
                    for each val_tit_ap no-lock
                       where val_tit_ap.cod_estab        = b_tit_ap.cod_estab
                         and val_tit_ap.num_id_tit_ap    = b_tit_ap.num_id_tit_ap
                         and val_tit_ap.cod_finalid_econ = v_cod_finalid_econ:
                        assign tt_relacto_tit_ap.val_sdo_tit_ap = tt_relacto_tit_ap.val_sdo_tit_ap + val_tit_ap.val_sdo_tit_ap.
                    end.
                end.
                ELSE assign tt_relacto_tit_ap.val_sdo_tit_ap = b_tit_ap.val_sdo_tit_ap.
            end.
        end /* for compl_block */.
    end /* for cria_temp_table */.

    /* Procura as Duplicatas da Nota */
    assign v_cod_estab = ""
           v_cod_refer = "".
    find last b_movto_tit_ap   NO-LOCK WHERE
              b_movto_tit_ap.cod_estab     = tit_ap.cod_estab
          and b_movto_tit_ap.num_id_tit_ap = tit_ap.num_id_tit_ap no-error.
    if avail b_movto_tit_ap then do:
        if b_movto_tit_ap.ind_trans_ap_abrev = "BXSB" /*l_bxsb*/  then do:
            assign v_cod_estab = b_movto_tit_ap.cod_estab
                   v_cod_refer = b_movto_tit_ap.cod_refer.
        end.
    end.
    dupli_block:
    for
         each b_movto_tit_ap
              where b_movto_tit_ap.cod_estab          = v_cod_estab and
                    b_movto_tit_ap.cod_refer          = v_cod_refer and
                    b_movto_tit_ap.ind_trans_ap_abrev = "SBND" /*l_sbnd*/ 
              no-lock:
              find b_tit_ap
                   where b_tit_ap.cod_estab     = b_movto_tit_ap.cod_estab and
                         b_tit_ap.num_id_tit_ap = b_movto_tit_ap.num_id_tit_ap
                   no-lock no-error.
              if avail b_tit_ap then do:

                  {cnp\cn001-rp3.i}
              end.
    end /* for dupli_block */.

    /* Procura as Notas da Duplicata */
    assign v_log_bxa = no.
    find fatur_ap no-lock
    where fatur_ap.cod_estab      = tit_ap.cod_estab      and
          fatur_ap.cdn_fornecedor = tit_ap.cdn_fornecedor and
          fatur_ap.num_fatur_ap   = tit_ap.num_fatur_ap no-error.
    if avail fatur_ap then do:
        if fatur_ap.log_bxa_estab_tit_ap then
            assign v_log_bxa = yes
                   v_cod_estab_fatur = fatur_ap.cod_estab.
    end.    
    
    if v_log_bxa then do:
        
        for each estabelecimento no-lock
        where estabelecimento.cod_empresa = v_cod_empres_usuar:
            nota_block:
            FOR each movto_tit_ap no-lock
                 where movto_tit_ap.cod_estab          = estabelecimento.cod_estab and
                       movto_tit_ap.cod_refer          = tit_ap.cod_refer and
                       movto_tit_ap.num_id_tit_ap     <> tit_ap.num_id_tit_ap and
                       movto_tit_ap.ind_trans_ap_abrev = "BXSB" /*l_bxsb*/ :
                
                if movto_tit_ap.cod_estab_proces_bxa <> string(v_cod_estab_fatur) then
                        next nota_block.

                find b_tit_ap no-lock
                     where b_tit_ap.num_id_tit_ap = movto_tit_ap.num_id_tit_ap
                       and b_tit_ap.cod_estab     = movto_tit_ap.cod_estab no-error.
                if avail b_tit_ap then do:
                    {cnp\cn001-rp3.i}
                end.
            end /* for nota_block */.
        end.
    end.
    else do:      
        nota_block:
        for each movto_tit_ap no-lock
                 where movto_tit_ap.cod_estab          = tit_ap.cod_estab and
                       movto_tit_ap.cod_refer          = tit_ap.cod_refer and
                       movto_tit_ap.num_id_tit_ap     <> tit_ap.num_id_tit_ap and
                       movto_tit_ap.ind_trans_ap_abrev = "BXSB" /*l_bxsb*/ :
            find b_tit_ap no-lock
                 where b_tit_ap.num_id_tit_ap = movto_tit_ap.num_id_tit_ap
                   and b_tit_ap.cod_estab     = movto_tit_ap.cod_estab no-error.
            if avail b_tit_ap then do:
                {cnp\cn001-rp3.i}
            end.
        end /* for nota_block */.
    end.

    /* Procura pelos movimentos de IR Geradores */
    compl_block:
    for
        each compl_impto_retid_ap no-lock
             where compl_impto_retid_ap.num_id_tit_ap = tit_ap.num_id_tit_ap
               and compl_impto_retid_ap.cod_estab     = tit_ap.cod_estab:

        find first b_movto_tit_ap no-lock
             where b_movto_tit_ap.cod_estab           = compl_impto_retid_ap.cod_estab
               and b_movto_tit_ap.num_id_movto_tit_ap = compl_impto_retid_ap.num_id_movto_tit_ap_pai
             no-error.
        if  avail b_movto_tit_ap
        then do:
            find b_tit_ap no-lock
                 where b_tit_ap.cod_estab     = b_movto_tit_ap.cod_estab
                   and b_tit_ap.num_id_tit_ap = b_movto_tit_ap.num_id_tit_ap
                  no-error.
            if  avail b_tit_ap
            then do: /* ---- T¡tulos Normais ---*/
               {cnp\cn001-rp3.i}
            end /* if */.
            else do: /* --- Pagto Extra Fornecedor ---*/
                create tt_relacto_tit_ap.
                assign tt_relacto_tit_ap.cod_estab           = b_movto_tit_ap.cod_estab
                       tt_relacto_tit_ap.cod_espec_docto     = b_movto_tit_ap.cod_espec_docto
                       tt_relacto_tit_ap.ind_tip_espec_docto = "Pagamento Extra-Fornecedor" /*l_pagamento_extrafornecedor*/  
                       tt_relacto_tit_ap.cdn_fornecedor      = b_movto_tit_ap.cdn_fornecedor
                       tt_relacto_tit_ap.cod_empresa         = b_movto_tit_ap.cod_empresa
                       tt_relacto_tit_ap.num_fatur_ap        = b_movto_tit_ap.num_fatur_ap
                       tt_relacto_tit_ap.log_tit_ap_estordo  = b_movto_tit_ap.log_movto_estordo
                       tt_relacto_tit_ap.dat_transacao       = b_movto_tit_ap.dat_transacao
                       tt_relacto_tit_ap.val_desconto        = b_movto_tit_ap.val_desconto
                       tt_relacto_tit_ap.val_juros           = b_movto_tit_ap.val_juros
                       tt_relacto_tit_ap.val_multa_tit_ap    = b_movto_tit_ap.val_multa_tit_ap
                       tt_relacto_tit_ap.val_cm_tit_ap       = b_movto_tit_ap.val_cm_tit_ap
                       tt_relacto_tit_ap.val_pagto_tit_ap    = b_movto_tit_ap.val_movto_ap
                       tt_relacto_tit_ap.val_desc_tit_ap     = b_movto_tit_ap.val_desconto
                       tt_relacto_tit_ap.val_abat_tit_ap     = b_movto_tit_ap.val_abat_tit_ap
                       tt_relacto_tit_ap.cod_refer           = b_movto_tit_ap.cod_refer
                       tt_relacto_tit_ap.tta_dat_gerac_movto = b_movto_tit_ap.dat_gerac_movto
                       tt_relacto_tit_ap.tta_hra_gerac_movto = b_movto_tit_ap.hra_gerac_movto.
            end /* else */.
        end /* if */.
    end /* for compl_block */.

    find compl_pagto_cartcred
        where compl_pagto_cartcred.cod_estab     = tit_ap.cod_estab
        and   compl_pagto_cartcred.num_id_tit_ap = tit_ap.num_id_tit_ap
        no-lock no-error.
    if  avail compl_pagto_cartcred then do:
        find movto_tit_ap
            where movto_tit_ap.cod_estab           = compl_pagto_cartcred.cod_estab
            and   movto_tit_ap.num_id_movto_tit_ap = compl_pagto_cartcred.num_id_movto_tit_ap_pai
            no-lock no-error.

        if  movto_tit_ap.ind_trans_ap <> "PEF CartÆo de Cr‚dito" /*l_pef_cartao_credito*/  then do:
            find b_tit_ap
                where b_tit_ap.cod_estab     = movto_tit_ap.cod_estab
                and   b_tit_ap.num_id_tit_ap = movto_tit_ap.num_id_tit_ap
                no-lock no-error.
            if avail b_tit_ap then do:
                
                {cnp\cn001-rp3.i}
            end.
        end.
    end.

    /* Busca o t¡tulo de cartÆo atrav‚s do pai */
    find first movto_tit_ap
        where movto_tit_ap.cod_estab = tit_ap.cod_estab
        and   movto_tit_ap.num_id_tit_ap = tit_ap.num_id_tit_ap
        no-lock no-error.
    if avail movto_tit_ap then do:    
        find compl_pagto_cartcred
            where compl_pagto_cartcred.cod_estab               = movto_tit_ap.cod_estab
            and   compl_pagto_cartcred.num_id_movto_tit_ap_pai = movto_tit_ap.num_id_movto_tit_ap
            no-lock no-error.
        if  avail compl_pagto_cartcred then do:
            find b_tit_ap
                where b_tit_ap.cod_estab     = compl_pagto_cartcred.cod_estab
                and   b_tit_ap.num_id_tit_ap = compl_pagto_cartcred.num_id_tit_ap
                no-lock no-error.
            if avail b_tit_ap then do:
                {cnp\cn001-rp3.i}
            end.
        end.
    end.

    IF NOT CAN-FIND(FIRST tt_relacto_tit_ap WHERE
                          tt_relacto_tit_ap.cod_estab     = tit_ap.cod_estab AND 
                          tt_relacto_tit_ap.num_id_tit_ap = tit_ap.num_id_tit_ap) 
    THEN DO:

        CREATE tt_relacto_tit_ap.
        BUFFER-COPY tit_ap TO tt_relacto_tit_ap
            ASSIGN tt_relacto_tit_ap.tta_cnd_fornec_orig    = tit_ap.cdn_fornec                 
                   tt_relacto_tit_ap.nr_contrato            = contrato-for.nr-contrato
                   tt_relacto_tit_ap.dt-ini-validade        = contrato-for.dt-ini-validade 
                   tt_relacto_tit_ap.dt-ter-validade        = contrato-for.dt-ter-validade 
                   tt_relacto_tit_ap.nat-operacao           = docum-est.nat-operacao
                   tt_relacto_tit_ap.num-seq-medicao        = medicao-contrat.num-seq-medicao
                   tt_relacto_tit_ap.num-seq-item           = medicao-contrat.num-seq-item
                   tt_relacto_tit_ap.dat-medicao            = medicao-contrat.dat-medicao 
                   tt_relacto_tit_ap.val-medicao            = medicao-contrat.val-medicao
                   tt_relacto_tit_ap.vl-glosa               = es-medicao-contrat.vl-glosa-desc WHEN AVAIL es-medicao-contrat
                   tt_relacto_tit_ap.tta_row_tit_ap         = ROWID(tit_ap)
                   tt_relacto_tit_ap.tta_dat_gerac_movto    = TODAY
                   tt_relacto_tit_ap.tta_hra_gerac_movto    = STRING(TIME,'hh:mm:ss')
                   tt_relacto_tit_ap.nr_contrato            = contrato-for.nr-contrato          
                   tt_relacto_tit_ap.tta_row_tit_ap         = ROWID(tit_ap)
                   tt_relacto_tit_ap.tta_status             = (IF contrato-for.dt-ini-validade <= TODAY AND contrato-for.dt-ter-validade >= TODAY THEN  
                                                               (IF contrato-for.ind-sit-contrat = 2 THEN "VIGENTE" ELSE ENTRY(contrato-for.ind-sit-contrat,{ininc/i05in065.i 03}))
                                                                ELSE "ENCERRADO").
        IF AVAIL ext-contrato-for AND ext-contrato-for.ind-status = 9 /* Processo Judicial */
             THEN tt_relacto_tit_ap.tta_status = "Processo Judicial".
    END.

END PROCEDURE. /* pi_criar_tt_relacto_tit_ap */


PROCEDURE pi_retornar_finalid_indic_econ:

    /************************ Parameter Definition Begin ************************/
    def Input param p_cod_indic_econ    as CHARACTER format "x(8)" no-undo.
    def Input param p_dat_transacao     as DATE format "99/99/9999" no-undo.
    def output param p_cod_finalid_econ as CHARACTER format "x(10)" no-undo.

    /************************* Parameter Definition End *************************/
   /* alteracao sob demanda - atividade 195864*/
    find first histor_finalid_econ no-lock
        where histor_finalid_econ.cod_indic_econ          = p_cod_indic_econ
        and   histor_finalid_econ.dat_inic_valid_finalid <= p_dat_transacao
        and   histor_finalid_econ.dat_fim_valid_finalid  > p_dat_transacao no-error.
    if  avail histor_finalid_econ then 
        assign p_cod_finalid_econ = histor_finalid_econ.cod_finalid_econ.  

END PROCEDURE. /* pi_retornar_finalid_indic_econ */

        assign i-transacao = fc-transacao(movto_tit_ap.ind_trans_ap_abrev).

        if  tit_ap.ind_origin_tit_ap = "APB" then
            assign i-cod_cond_pag = 0.

        if  tit_ap.ind_origin_tit_ap = "REC" then do:
/*
    find first docum-est no-lock
         where /*docum-est.ep-codigo   = tit_ap.cod_empresa
           and*/ docum-est.cod-emitente = tit_ap.cdn_fornecedor
           and docum-est.cod-estabel = tit_ap.cod_estab
           and docum-est.cod-esp     = tit_ap.cod_espec_docto
           and docum-est.serie       = tit_ap.cod_ser_docto
           and docum-est.nr-docto    = tit_ap.cod_tit_ap
           and docum-est.parcela     = tit_ap.cod_parcela no-error.
    if  avail docum-est then
        disp docum-est.nat-operacao
             docum-est.cod-cond-pagto.
*/
            assign i-cod_cond_pag = 0.
        end.

        CREATE es_mov_ap.
        ASSIGN es_mov_ap.cod_cond_pag   = i-cod_cond_pag
               es_mov_ap.ep_codigo      = tit_ap.cod_empresa
               es_mov_ap.cod_esp        = tit_ap.cod_espec_docto
               es_mov_ap.cod_estabel    = tit_ap.cod_estab
               es_mov_ap.cod_fornec     = tit_ap.cdn_fornecedor
               es_mov_ap.dt_emissao     = IF tit_ap.dat_emis_docto < 01/01/1754 THEN date("") ELSE tit_ap.dat_emis_docto
               es_mov_ap.dt_emissao_ap  = IF tit_ap.dat_emis_docto < 01/01/1754 THEN date("") ELSE tit_ap.dat_emis_docto
               es_mov_ap.dt_pagamento   = IF tit_ap.dat_liquidac_tit_ap = 12/31/9999 THEN date("") ELSE tit_ap.dat_liquidac_tit_ap
               es_mov_ap.dt_transacao   = IF tit_ap.dat_transacao < 01/01/1754 THEN date("") ELSE movto_tit_ap.dat_transacao
               es_mov_ap.dt_vencimen    = IF tit_ap.dat_vencto_tit_ap < 01/01/1754 THEN date("") ELSE tit_ap.dat_vencto_tit_ap
               es_mov_ap.dt_original    = IF tit_ap.dat_vencto_tit_ap < 01/01/1754 THEN date("") ELSE tit_ap.dat_vencto_tit_ap
/*                es_mov_ap.nat_operac     = tit_ap. */
               es_mov_ap.nr_docto       = tit_ap.cod_tit_ap
               es_mov_ap.origem         = movto_tit_ap.ind_origin_tit_ap
               es_mov_ap.parcela        = tit_ap.cod_parcela
               es_mov_ap.pedido         = string(movto_tit_ap.num_ped_compra)
               es_mov_ap.serie          = tit_ap.cod_ser_docto
               es_mov_ap.valor_mov      = movto_tit_ap.val_movto_ap
               es_mov_ap.vl_abatimento  = movto_tit_ap.val_abat_tit_ap
               es_mov_ap.vl_desconto    = movto_tit_ap.val_desconto
               es_mov_ap.vl_multa       = movto_tit_ap.val_multa_tit_ap
               es_mov_ap.vl_original    = tit_ap.val_origin_tit_ap
               es_mov_ap.transacao      = i-transacao
               es_mov_ap.cod_chave      = v-num-reg-lidos
               es_mov_ap.valor_saldo    = movto_tit_ap.val_sdo_apos_movto. /*EM: 17/04/2013 para atender ao portal de fornecedores*/

        find first item_bord_ap no-lock
             where item_bord_ap.cod_estab            = tit_ap.cod_estab
               and item_bord_ap.cod_espec_docto      = tit_ap.cod_espec_docto
               and item_bord_ap.cod_ser_docto        = tit_ap.cod_ser_docto
               and item_bord_ap.cdn_fornecedor       = tit_ap.cdn_fornecedor
               and item_bord_ap.cod_tit_ap           = tit_ap.cod_tit_ap
               and item_bord_ap.cod_parcela          = tit_ap.cod_parcela
               and(item_bord_ap.ind_sit_item_bord_ap = "Em Aberto"
                or item_bord_ap.ind_sit_item_bord_ap = "Baixado") no-error.
        if  avail item_bord_ap then
            assign es_mov_ap.nr_bordero = item_bord_ap.num_bord_ap.

        for first relacto_tit_ap no-lock
            where relacto_tit_ap.cod_estab     = tit_ap.cod_estab
              and relacto_tit_ap.num_id_tit_ap = tit_ap.num_id_tit_ap:
            find first bf-movto_tit_ap no-lock
                 where bf-movto_tit_ap.cod_estab           = relacto_tit_ap.cod_estab_tit_ap_pai
                   and bf-movto_tit_ap.num_id_movto_tit_ap = relacto_tit_ap.num_id_movto_tit_ap_pai no-error.
            if  avail bf-movto_tit_ap then
                find first bf-tit_ap no-lock
                     where bf-tit_ap.cod_estab     = bf-movto_tit_ap.cod_estab
                       and bf-tit_ap.num_id_tit_ap = bf-movto_tit_ap.num_id_tit_ap no-error.

            assign es_mov_ap.vl_antecipacao = relacto_tit_ap.val_relacto_tit_ap /*Campos adicionados a extra‡Æo */
                   es_mov_ap.docto_antecip  = bf-tit_ap.cod_tit_ap /*para verificar as antecipa‡äes*/
                   es_mov_ap.esp_antecip    = bf-tit_ap.cod_espec_docto. /*Adicionados por Daniel        */
        end.

        assign v-num-reg-lidos = v-num-reg-lidos + 1.
        run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).

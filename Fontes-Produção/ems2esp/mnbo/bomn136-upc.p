/*******************************************************
** Autor...: Gustavo Eduardo Tamanini
** Programa: BOMN136-UPC
** UPC cadastrada para programa: BOMN136
*******************************************************/
{include/i-prgvrs.i BOMN136-UPC 2.06.00.000}
{include/i-epc200.i}

define temp-table tt-ord-taref no-undo like ord-taref
    field r-Rowid as rowid.

def input param p-ind-event as char no-undo.
def input-output param table for tt-epc.

define variable h-bo         as handle              no-undo.
define variable cTexto       as char format "x(50)" no-undo.
define variable c-sc-despesa like ord-manut.sc-desp no-undo.
/** Variaveis do programa MI0307C-UPC **/
define new global shared var wh-fi-tipo-manut-0307 as widget-handle no-undo.
define new global shared var wh-fi-ct-despesa-0307 as widget-handle no-undo.
define new global shared var wh-fi-sc-despesa-0307 as widget-handle no-undo.

find first tt-epc 
     where tt-epc.cod-event     = p-ind-event
     and   tt-epc.cod-parameter = "OBJECT-HANDLE" no-lock no-error.

if avail tt-epc then do:
    assign h-bo = widget-handle(tt-epc.val-parameter).
end.

/** FO 1716.638 - Validacoes: Tipo de Manutencao e Conta Despesa. **/
if (p-ind-event = "beforeCreateRecord":U  or
    p-ind-event = "beforeUpdateRecord":U) then do:

    run getRecord in h-bo (output table tt-ord-taref).
    
    if  valid-handle(wh-fi-tipo-manut-0307) and
        valid-handle(wh-fi-ct-despesa-0307) and
        valid-handle(wh-fi-sc-despesa-0307) then do:

        for first tt-ord-taref no-lock:
            /** C¢digo Tipo Manuten‡Æo em branco **/
            {utp/ut-liter.i "Tipo_Manuten‡Æo_est _em_branco"}
            if int(wh-fi-tipo-manut-0307:screen-value) = 0 then do:
                run pi-cria-erro (input return-value + ".":U).
                return "NOK":U.
            end.
            else do:
                {utp/ut-liter.i "Tipo_Manuten‡Æo_inexistente"}
                assign cTexto = return-value.
                {utp/ut-liter.i "Verifique_se_existe_uma_ocorrˆncia_para_o(a)_Tipo_Manuten‡Æo_informado(a)_em_seu_cadastro."}
                /** Tipo Manuten‡Æo nÆo cadastrado **/
                if not can-find(first tipo-manut
                                where tipo-manut.cd-tipo = int(wh-fi-tipo-manut-0307:screen-value) no-lock) then do:
                    run pi-cria-erro (input cTexto + ". ":U + return-value).
                    return "NOK":U.
                end.
            end.

            for first ord-manut
                where ord-manut.nr-ord-produ = tt-ord-taref.nr-ord-produ no-lock:
                for first tipo-manut
                    where tipo-manut.cd-tipo = ord-manut.cd-tipo no-lock:
                end.
            end.

            /** Se Ccusto = ? busca Centro Custo do Equipamento **/
            assign c-sc-despesa = wh-fi-sc-despesa-0307:screen-value.
            if c-sc-despesa = "?":U then
                for first equipto
                    where equipto.cd-equipto = ord-manut.cd-equipto no-lock:
                    assign c-sc-despesa = equipto.cc-codigo.
                end.

            for first cta_ctbl_integr no-lock
                where cta_ctbl_integr.cod_modul_dtsul = "CEP"
                  and cta_ctbl_integr.cod_cta_ctbl    = wh-fi-ct-despesa-0307:screen-value
                  and cta_ctbl_integr.dat_inic_valid <= today
                  and cta_ctbl_integr.dat_fim_valid  >= today:
            end.
            if  not avail cta_ctbl_integr then do:
                /** Erro Nro: 25109 **/
                {utp/ut-liter.i "Verifique_se_a_Conta_Despesa_est _cadastrada_para_empresa,_conforme_parametriza‡Æo._Esta_parametriza‡Æo_poder _ser_feita_nos_parƒmetros_globais_ou_no_cadastro_de_Estabelecimento." *}
                run pi-cria-erro (input return-value).
                return "NOK":U.
            end.
            else do: 
                /** Tipo Manuten‡Æo com Destino = Despesa **/
                if tipo-manut.dest-manut = 1 then do:
                    find first grp_cta_ctbl of tip_grp_cta_ctbl no-lock
                         where grp_cta_ctbl.cod_inic_cta_ctbl = substr(cta_ctbl_integr.cod_cta_ctbl,1,1) no-error.

                    if  grp_cta_ctbl.cdn_grp_cta_ctbl_ext <> 1 and
                        grp_cta_ctbl.cdn_grp_cta_ctbl_ext <> 5 then do: /* <> Despesa e <> Ativo */
                        if  ord-manut.num-ord-inv = 0 then do:
                            /** 4187 **/
                            {utp/ut-liter.i "Conta_Cont bil_deve_ser_de_Ativo_ou_Despesa." *}
                            run pi-cria-erro (input return-value).
                            return "NOK":U.
                        end.
                        else do:
                            /** Erro Nro: 19061 **/ 
                            {utp/ut-liter.i "Vocˆ_deve_informar_na_Conta_de_Despesa_da_Ordem_uma_Conta_do_tipo_de_Despesa_(tipo 1)_ou_de_Ativo_(tipo 5),_pois_trata-se_de_uma_Ordem_vinculada_com_uma_Ordem_de_Investimento." *}
                            run pi-cria-erro (input return-value).
                            return "NOK":U.
                        end.
                    end.
                    if (ord-manut.num-ord-inv = 0 and
                        cta_ctbl_integr.ind_finalid_ctbl <> "Consumo") then do:
                        /** Erro Nro: 622 **/
                        {utp/ut-liter.i "Conta_Cont bil_deve_ser_de_Consumo." *}
                        run pi-cria-erro (input return-value).
                        return "NOK":U.
                    end.
                    if  ord-manut.num-ord-inv <> 0 and
                       (cta_ctbl_integr.ind_finalid_ctbl <> "Consumo" and
                        cta_ctbl_integr.ind_finalid_ctbl <> "NÆo Consumo" and
                        cta_ctbl_integr.ind_finalid_ctbl <> "Ordem servi‡o" and 
                        cta_ctbl_integr.ind_finalid_ctbl <> "MÆo de Obra") then do:
                        /** Erro Nro: 25886 **/
                        {utp/ut-liter.i "A_conta_cont bil_deve_ser_de_Consumo,_NÆo_consumo,_Ordem_de_Servi‡o_ou_MÆo_de_Obra." *}
                        run pi-cria-erro (input return-value).
                        return "NOK":U.
                    end.
                end.
                /** Destino = Imobilizado **/
                else do:
                    find first grp_cta_ctbl of tip_grp_cta_ctbl no-lock
                         where grp_cta_ctbl.cod_inic_cta_ctbl = substr(cta_ctbl_integr.cod_cta_ctbl,1,1) no-error.

                    &IF  DEFINED (bf_mnt_ems204) &THEN
                        /** Conta deve ser de Ativo **/
                        if  grp_cta_ctbl.cdn_grp_cta_ctbl_ext <> 5 then do:
                            /** Erro Nro: 32772 **/
                            {utp/ut-liter.i "A_conta_cont bil_informada_deve_ser_do_tipo_Ativo." *}
                            run pi-cria-erro (input return-value).
                            return "NOK":U.
                        end.
                    &ELSE
                        /** Conta deve ser de Ativo ou Despesa **/
                        if  (grp_cta_ctbl.cdn_grp_cta_ctbl_ext <> 5 and 
                             grp_cta_ctbl.cdn_grp_cta_ctbl_ext <> 1) then do:
                            /** Erro Nro: 4187 **/
                            {utp/ut-liter.i "Conta_Cont bil_deve_ser_de_Ativo_ou_Despesa." *}
                            run pi-cria-erro (input return-value).
                            return "NOK":U.
                        end.
                    &ENDIF
                end.
            end.
        end.
    end.
end.
/****/

/** FO 1716.638 - Criar tabela especifica para guardar
    a Conta Despesa da Tarefa. **/
if (p-ind-event = "afterCreateRecord":U  or
    p-ind-event = "afterUpdateRecord":U) then do:
    run getRecord in h-bo (output table tt-ord-taref).

    if  valid-handle(wh-fi-tipo-manut-0307) and
        valid-handle(wh-fi-ct-despesa-0307) and
        valid-handle(wh-fi-sc-despesa-0307) then do:

        for first tt-ord-taref no-lock:
            /** Se Ccusto = ? busca Centro Custo do Equipamento **/
            assign c-sc-despesa = wh-fi-sc-despesa-0307:screen-value.
            if c-sc-despesa = "?":U then do:
                for first ord-manut
                    where ord-manut.nr-ord-produ = tt-ord-taref.nr-ord-produ no-lock:
                    for first equipto
                        where equipto.cd-equipto = ord-manut.cd-equipto no-lock:
                        assign c-sc-despesa = equipto.cc-codigo.
                    end.
                end.
            end.

            if not can-find(first mi-tar-ord
                            where mi-tar-ord.nr-ord-produ = tt-ord-taref.nr-ord-produ
                            and   mi-tar-ord.cd-tarefa    = tt-ord-taref.cd-tarefa no-lock) then do:
                create mi-tar-ord.
                assign mi-tar-ord.nr-ord-produ = tt-ord-taref.nr-ord-produ
                       mi-tar-ord.cd-tarefa    = tt-ord-taref.cd-tarefa
                       mi-tar-ord.tipo-manut   = int(wh-fi-tipo-manut-0307:screen-value)
                       mi-tar-ord.ct-despesa   = wh-fi-ct-despesa-0307:screen-value
                       mi-tar-ord.sc-despesa   = c-sc-despesa.
            end.
            else do:
                for first mi-tar-ord
                    where mi-tar-ord.nr-ord-produ = tt-ord-taref.nr-ord-produ
                    and   mi-tar-ord.cd-tarefa    = tt-ord-taref.cd-tarefa exclusive-lock:
                    assign mi-tar-ord.tipo-manut  = int(wh-fi-tipo-manut-0307:screen-value)
                           mi-tar-ord.ct-despesa  = wh-fi-ct-despesa-0307:screen-value
                           mi-tar-ord.sc-despesa  = c-sc-despesa.
                end.
            end.
        end.
    end.
end.

/** FO 1716.638 - Deleta tabela especifica da Conta Depesa**/
if p-ind-event = "afterDeleteRecord":U then do:
    run getRecord in h-bo (output table tt-ord-taref).
    for first tt-ord-taref no-lock:
        for first mi-tar-ord
            where mi-tar-ord.nr-ord-produ = tt-ord-taref.nr-ord-produ
            and   mi-tar-ord.cd-tarefa    = tt-ord-taref.cd-tarefa exclusive-lock:
            delete mi-tar-ord.
        end.
    end.
end.

procedure pi-cria-erro:
define input parameter msg as char no-undo.
    create tt-epc.
    assign tt-epc.cod-event     = "ERROR"
           tt-epc.cod-parameter = "EPC-ERROR" 
           tt-epc.val-parameter = msg.
end.

return "OK":U.

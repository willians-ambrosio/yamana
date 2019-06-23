/*******************************************************
** Autor...: Gustavo Eduardo Tamanini
** Programa: bofr073-UPC
** UPC cadastrada para programa: bofr073
*******************************************************/
{include/i-prgvrs.i BOFR073-UPC 2.06.00.000}
{include/i-epc200.i}

define temp-table tt-mmv-tar-ord-manut no-undo like mmv-tar-ord-manut
    field r-Rowid as rowid.

def input param p-ind-event as char no-undo.
def input-output param table for tt-epc.

define variable h-bo         as handle  no-undo.
define variable c-sc-despesa like mmv-ord-manut.cc-ordem no-undo.
/** Variaveis do programa MV0301A-UPC **/
define new global shared var wh-fi-ct-despesa-0301 as widget-handle no-undo.
define new global shared var wh-fi-sc-despesa-0301 as widget-handle no-undo.

find first tt-epc 
     where tt-epc.cod-event     = p-ind-event
     and   tt-epc.cod-parameter = "OBJECT-HANDLE" no-lock no-error.

if avail tt-epc then do:
    assign h-bo = widget-handle(tt-epc.val-parameter).
end.

/** FO 1716.638 - Valida‡äes: Conta Despesa. **/
if (p-ind-event = "beforeCreateRecord":U  or
    p-ind-event = "beforeUpdateRecord":U) then do:

    /** Retorna temp-table da BO **/
    run getRecord in h-bo (output table tt-mmv-tar-ord-manut).

    if  valid-handle(wh-fi-ct-despesa-0301) and
        valid-handle(wh-fi-sc-despesa-0301) then do:

        for first tt-mmv-tar-ord-manut no-lock:
            for first mmv-ord-manut
                where mmv-ord-manut.nr-ord-produ = tt-mmv-tar-ord-manut.nr-ord-produ no-lock:
            end.
            /** Se Ccusto = ? busca Centro Custo do Equipamento **/
            assign c-sc-despesa = wh-fi-sc-despesa-0301:screen-value.
            if c-sc-despesa = "?":U then
                for first mab-eqpto
                    where mab-eqpto.cod-eqpto = mmv-ord-manut.cod-eqpto no-lock:
                    assign c-sc-despesa = mab-eqpto.cc-codigo.
                end.
            /** Valida Conta Contabil **/
            for FIRST cta_ctbl_integr no-lock
                WHERE /*cta_ctbl_integr.cod_modul_dtsul = "CEP"
                  and*/ cta_ctbl_integr.cod_cta_ctbl    = wh-fi-ct-despesa-0301:screen-value
                  and cta_ctbl_integr.dat_inic_valid <= today
                  and cta_ctbl_integr.dat_fim_valid  >= today:
                find first grp_cta_ctbl of tip_grp_cta_ctbl no-lock
                     where grp_cta_ctbl.cod_inic_cta_ctbl = substr(cta_ctbl_integr.cod_cta_ctbl,1,1) no-error.

                if  grp_cta_ctbl.cdn_grp_cta_ctbl_ext <> 1 and
                    grp_cta_ctbl.cdn_grp_cta_ctbl_ext <> 5 then do: /* 1 - Despeza, 5 - Ativo */
                    {utp/ut-liter.i "Conta_Cont bil_deve_ser_de_Despesa_ou_Ativo." *}
                        run pi-cria-erro (input return-value).
                        return "NOK":U.
                end.
                if cta_ctbl_integr.ind_finalid_ctbl <> "Consumo" then do: /* <> Consumo */
                    {utp/ut-liter.i "A_Conta_Cont bil_de_Estoque_deve_ser_do_tipo_Consumo!" *}
                        run pi-cria-erro (input return-value).
                        return "NOK":U.
                end.
            end.
            if  not available cta_ctbl_integr then do:
                {utp/ut-liter.i "Conta_Ordem_inv lida._Verifique_se_a_Conta_Ordem_est _cadastrada_para_empresa,_conforme_parametriza‡Æo._Esta_parametriza‡Æo_poder _ser_feita_nos_parƒmetros_globais_ou_no_cadastro_de_Estabelecimento." *}
                run pi-cria-erro (input return-value).
                return "NOK":U.
            end.
        end.
    end.
end.
/****/

/** FO 1716.638 - Criar tabela especifica para guardar
    a Conta Despesa da Tarefa. **/
if (p-ind-event = "afterCreateRecord":U  or
    p-ind-event = "afterUpdateRecord":U) then do:

    /** Retorna temp-table da BO **/
    run getRecord in h-bo (output table tt-mmv-tar-ord-manut).

    if  valid-handle(wh-fi-ct-despesa-0301) and
        valid-handle(wh-fi-sc-despesa-0301) then do:

        for first tt-mmv-tar-ord-manut no-lock:
            /** Se Ccusto = ? busca Centro Custo do Equipamento **/
            assign c-sc-despesa = wh-fi-sc-despesa-0301:screen-value.
            if c-sc-despesa = "?":U then do:
                for first mmv-ord-manut
                    where mmv-ord-manut.nr-ord-produ = tt-mmv-tar-ord-manut.nr-ord-produ no-lock:
                    for first mab-eqpto
                        where mab-eqpto.cod-eqpto = mmv-ord-manut.cod-eqpto no-lock:
                        assign c-sc-despesa = mab-eqpto.cc-codigo.
                    end.
                end.
            end.
            if not can-find(first mv-tar-ord
                            where mv-tar-ord.nr-ord-produ = tt-mmv-tar-ord-manut.nr-ord-produ
                            and   mv-tar-ord.cd-tarefa    = tt-mmv-tar-ord-manut.num-seq no-lock) then do:
                create mv-tar-ord.
                assign mv-tar-ord.nr-ord-produ = tt-mmv-tar-ord-manut.nr-ord-produ
                       mv-tar-ord.cd-tarefa    = tt-mmv-tar-ord-manut.num-seq
                       mv-tar-ord.tipo-manut   = tt-mmv-tar-ord-manut.cd-tipo
                       mv-tar-ord.ct-despesa   = wh-fi-ct-despesa-0301:screen-value
                       mv-tar-ord.sc-despesa   = c-sc-despesa.
            end.
            else do:
                for first mv-tar-ord
                    where mv-tar-ord.nr-ord-produ = tt-mmv-tar-ord-manut.nr-ord-produ
                    and   mv-tar-ord.cd-tarefa    = tt-mmv-tar-ord-manut.num-seq exclusive-lock:
                    assign mv-tar-ord.tipo-manut  = tt-mmv-tar-ord-manut.cd-tipo
                           mv-tar-ord.ct-despesa  = wh-fi-ct-despesa-0301:screen-value
                           mv-tar-ord.sc-despesa  = c-sc-despesa.
                end.
            end.
        end.
    end.
end.

/** FO 1716.638 - Deleta tabela especifica da Conta Despesa. **/
if p-ind-event = "afterDeleteRecord":U then do:
    run getRecord in h-bo (output table tt-mmv-tar-ord-manut).
    for first tt-mmv-tar-ord-manut no-lock:
        for first mv-tar-ord
            where mv-tar-ord.nr-ord-produ = tt-mmv-tar-ord-manut.nr-ord-produ
            and   mv-tar-ord.cd-tarefa    = tt-mmv-tar-ord-manut.num-seq exclusive-lock:
            delete mv-tar-ord.
        end.
    end.
end.

procedure pi-cria-erro :
    def input param msg as char no-undo.

    create tt-epc.
    assign tt-epc.cod-event     = "ERROR"
           tt-epc.cod-parameter = "EPC-ERROR" 
           tt-epc.val-parameter = msg.
end.

return "OK":U.

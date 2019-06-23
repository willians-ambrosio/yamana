/******************************************************************************
** Programa       : ymfp0014rp.p
** Altera‡Æo      : 1789506  - 05/01/18 - FSW TOTVS Joinville HCM
** Objetivo       : Importa‡Æo Planilha PLR
*****************************************************************************/
{include/i-prgvrs.i YMFP0014RP 1.02.00.000 } /*** 010000 ***/

def new global shared var c-dir-spool-servid-exec as char no-undo.                     
def new global shared var i-num-ped-exec-rpw as int no-undo.      

&scoped-define YMFP0014RP
{prghur/esp/ymfp0014tt.i} /* <- todas temp-tables aqui */
{include/i-rpvar.i}

function fn_numerico returns char (input p_valor as char) forward.
function fn_decimal returns dec (input p_valor as char) forward.

def var c-destino-impressao as char format "x(15)"           no-undo.

form  skip
      "P gina de Parƒmetros"                  AT 55 SKIP(2)
      "SELE€ÇO"                               at 15 skip(1)
      tt_ini_cdn_estab                        colon 35
      "<|  |>"                                at 46
      tt_fim_cdn_estab no-label               skip(1)
      tt_ini_cdn_funcionario                  colon 35
      "<|  |>"                                at 46
      tt_fim_cdn_funcionario no-label         skip(1)
      skip(2)
      "PAR¶METROS"                            AT 15 SKIP(1)
      tt_cdn_param_calc_ppr                   colon 35 skip(1)
      tt_num_mes_refer_calc_efetd             colon 35 skip(1)
      tt_num_ano_refer_calc_efetd             colon 35 skip(1)
      tt_des_ind_importa label "Importa"      colon 35 skip(1)
      tt_arquivo_importa  label "Arquivo Importa‡Æo" colon 35

      SKIP(2)
      "IMPRESSÇO"                     at 15 skip(1)
      c-destino-impressao label "Destino" colon 35
      " - "
      tt-param.arquivo no-label  format "x(40)"       skip
      tt-param.usuario label "Usuario" colon 35 skip(1)

      with stream-io side-labels no-attr-space no-box width 255 1 down frame f-parametro. 


def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

def var h-acomp as handle no-undo.

define variable chExcel as office.iface.excel.ExcelWrapper no-undo.
define variable chWorkBook as office.iface.excel.WorkBook no-undo.
def var v_arquivo_entrada as char no-undo.
def var v_qti_registros_importados as int no-undo.
def var v_val_total_importados as dec format 'z,zzz,zzz,zzz,zz9.99' no-undo.

create tt-param.
raw-transfer raw-param to tt-param.

{include/i-rpout.i}
{include/i-rpcab.i}

view frame f-cabec.
view frame f-rodape.

file-info:filename = tt-param.tt_arquivo_importa.

if file-info:pathname = ? then do:

    &scoped-define MSG substitute("NÆo foi poss¡vel localizar o arquivo &1", tt-param.tt_arquivo_importa) 
    
    put {&MSG} format "x(120)" skip(1).

    {include/i-rpclo.i}

    if i-num-ped-exec-rpw = 0 then do:

        run utp/ut-msgs.p (input "show":U, input 17006, input {&MSG}).


    end.

    return "OK".

end.

/** Parƒmetros FP0900 considerados:
        Gerais >> Per¡odo Vigˆncia
        Gerais >> Evento PLR
        Aba Pagamentos: habilita‡Æo PLR
*/  

v_arquivo_entrada = file-info:pathname.

find first param_calc_ppr no-lock
    where param_calc_ppr.cdn_param_calc_ppr = tt-param.tt_cdn_param_calc_ppr no-error.

find first param_empres_rh no-lock
    where param_empres_rh.cdn_empresa = tt-param.v_cdn_empres_usuar no-error.

run utp/ut-acomp.p persistent set h-acomp.

run pi-carrega-planilha.

if tt-param.tt_ind_tip_importa = 1 then
    run pi-importa-provisao.
else
    run pi-importa-pagamento.

if return-value = "OK" then do:

    run pi-desabilita-cancela in h-acomp.

    if tt-param.tt_ind_tip_importa = 1 then do:

        for each ttfaixa_param_calc_ppr_func :

            find first faixa_param_calc_ppr_func of ttfaixa_param_calc_ppr_func exclusive-lock no-error.

            if not avail faixa_param_calc_ppr_func then create faixa_param_calc_ppr_func.

            buffer-copy ttfaixa_param_calc_ppr_func
                using cdn_param_calc_ppr     
                      cdn_empresa            
                      cdn_estab              
                      cdn_funcionario        
                      idi_tip_calc_ppr       
                      idi_tip_pagto_ppr_lotac
                      val_aplic_calc_ppr
                      val_base_calc_ppr
                to faixa_param_calc_ppr_func.

            validate faixa_param_calc_ppr_func.

            assign v_qti_registros_importados = v_qti_registros_importados + 1
                   v_val_total_importados     = v_val_total_importados + ttfaixa_param_calc_ppr_func.val_base_calc_ppr.

        end.

        {prghur/esp/ymfp0014rp.i &tip_func_indiv_factor="tip_func_indiv_factor_prov" &tiph_func_indiv_factor="tiph_func_indiv_factor_prov"}

    end.

    else do:

        for each ttmovto_ppr :

            find first movto_ppr of ttmovto_ppr exclusive-lock no-error.

            if not avail movto_ppr then create movto_ppr.

            buffer-copy ttmovto_ppr
                using cdn_param_calc_ppr      
                      cdn_empresa             
                      cdn_estab               
                      cdn_funcionario         
                      num_mes_refer_calc_efetd
                      num_ano_refer_calc_efetd
                      idi_tip_folha_pagto_ppr 
                      qti_parc_habilit_calc_fp
                      cdn_event_fp            
                      qtd_unid_event_fp       
                      val_calcul_efp          
                      idi_orig_movto_ppr      
                to movto_ppr.

            validate movto_ppr.

            assign v_qti_registros_importados = v_qti_registros_importados + 1
                   v_val_total_importados     = v_val_total_importados + ttmovto_ppr.val_calcul_efp.

        end.

        {prghur/esp/ymfp0014rp.i &tip_func_indiv_factor="tip_func_indiv_factor_pag" &tiph_func_indiv_factor="tiph_func_indiv_factor_pag"}

    end.

end.

find empresa no-lock where empresa.ep-codigo = tt-param.v_cdn_empres_usuar no-error.

assign c-empresa  = "Empresa " + empresa.razao-social
       c-sistema = "Folha de Pagamento".

put "Total de movimentos importados: " v_qti_registros_importados format "z,zzz9" skip(1)
    "Valor total importado:          " v_val_total_importados skip(1).

if can-find(first tt-erros) then do:

    put "Foram encontrados os seguintes problemas:" skip.

    for each tt-erros
        by tt-erros.seq :

        disp tt-erros.mensagem format "x(120)"
            with stream-io down no-labels frame f-erros width 132.

        down with frame f-erros.

    end.

end.

if tt-param.imprime_parametros then do:

    page.

    ASSIGN c-destino-impressao = {varinc/var00002.i 04 tt-param.destino}.

    disp tt_ini_cdn_estab      
         tt_fim_cdn_estab 
         tt_ini_cdn_funcionario
         tt_fim_cdn_funcionario
         tt_cdn_param_calc_ppr      
         tt_num_mes_refer_calc_efetd
         tt_num_ano_refer_calc_efetd
         tt_des_ind_importa 
         tt_arquivo_importa  
         c-destino-impressao
         tt-param.arquivo
         tt-param.usuario
         with frame f-parametro. 

end.

run pi-finalizar in  h-acomp.

{include/i-rpclo.i}

return "OK".

procedure pi-importa-provisao :

    def var v_data_limite as date no-undo.
    def var v_val_aplic_calc_ppr like faixa_param_calc_ppr_func.val_aplic_calc_ppr no-undo.
    def var v_cancelado as logical no-undo.
    def var v_cdn_indiv_factor like tip_func_indiv_factor_prov.cdn_indiv_factor no-undo.
    def var v_data_ini as date no-undo.
    def var v_chave as char no-undo.
    def var v_seq as int no-undo.
    DEF VAR v_valor_tip LIKE tt-tip-cash-awards.valor_tip NO-UNDO.
    DEF VAR v_valor_tip_total LIKE tt-tip-cash-awards.valor_tip NO-UNDO.
    def var v_qtd_unid_event_fp like movto_ppr.qtd_unid_event_fp no-undo.

    assign v_data_limite = date(if tt-param.tt_num_mes_refer_calc_efetd < 12 
                                then tt-param.tt_num_mes_refer_calc_efetd + 1 else 1, 1,
                                if tt-param.tt_num_mes_refer_calc_efetd < 12 
                                then tt-param.tt_num_ano_refer_calc_efetd
                                else tt-param.tt_num_ano_refer_calc_efetd + 1)
           v_data_ini    = date(tt-param.tt_num_mes_refer_calc_efetd, 1, 
                                tt-param.tt_num_ano_refer_calc_efetd).

    for each tt-tip-cash-awards,
        FIRST funcionario no-lock
        where funcionario.cod_id_feder       = tt-tip-cash-awards.cod_id_feder
          and funcionario.cdn_empresa        = tt-param.v_cdn_empres_usuar
          and funcionario.cdn_estab         >= tt-param.tt_ini_cdn_estab
          and funcionario.cdn_estab         <= tt-param.tt_fim_cdn_estab 
          and funcionario.cdn_funcionario   >= tt-param.tt_ini_cdn_funcionario
          and funcionario.cdn_funcionario   <= tt-param.tt_fim_cdn_funcionario 
          AND funcionario.idi_orig_contratac_func <> 6 
          AND (funcionario.dat_desligto_func = ? OR
               funcionario.dat_desligto_func <> ?
        and  can-find(first habilit_rescis of funcionario no-lock
                         where habilit_rescis.dat_desligto_func < v_data_limite
                           and year(habilit_rescis.dat_desligto_func) = tt-param.tt_num_ano_refer_calc_efetd))
        
        on stop undo, leave 
        break by funcionario.cod_id_feder :

        assign v_cancelado = true.

        run pi-acompanhar in h-acomp (input funcionario.cdn_funcionario).

        
        if funcionario.dat_desligto_func ne ?
        and not can-find(first habilit_rescis of funcionario no-lock
                         where habilit_rescis.dat_desligto_func < v_data_limite
                           and year(habilit_rescis.dat_desligto_func) = tt-param.tt_num_ano_refer_calc_efetd)
        then next.
        
        if funcionario.log_consid_calc_ppr = false then do:

            assign v_chave = 'FP1500' + funcionario.cdn_estab + string(funcionario.cdn_funcionario).

            if not can-find(first tt-erros
                            where tt-erros.chave = v_chave) then do:

                create tt-erros.
                assign v_seq = v_seq + 1
                       tt-erros.chave    = v_chave
                       tt-erros.seq      = v_seq
                       tt-erros.mensagem = substitute("Funcion rio &1 - &2 sem c lculo PLR (FP1500)",
                                                      funcionario.cdn_estab,
                                                      funcionario.cdn_funcionario).

            end.

            next.

        end.
        
        if first-of(funcionario.cod_id_feder) then do:
            create ttfaixa_param_calc_ppr_func.

            assign v_val_aplic_calc_ppr                                = 0
                   ttfaixa_param_calc_ppr_func.cdn_param_calc_ppr      = param_calc_ppr.cdn_param_calc_ppr
                   ttfaixa_param_calc_ppr_func.cdn_empresa             = funcionario.cdn_empresa
                   ttfaixa_param_calc_ppr_func.cdn_estab               = funcionario.cdn_estab
                   ttfaixa_param_calc_ppr_func.cdn_funcionario         = funcionario.cdn_funcionario
                   ttfaixa_param_calc_ppr_func.cdn_event_pagto_ppr     = param_calc_ppr.cdn_event_pagto_ppr
                   ttfaixa_param_calc_ppr_func.idi_tip_calc_ppr        = 2
                   ttfaixa_param_calc_ppr_func.idi_tip_pagto_ppr_lotac = 1
                   v_valor_tip                                         = 0
                   v_valor_tip_total                                   = 0
                   v_qtd_unid_event_fp                                 = 0.
            ASSIGN v_valor_tip_total = v_valor_tip_total + tt-tip-cash-awards.valor_tip_total.
            
        end. 

        
        ASSIGN v_valor_tip = v_valor_tip + tt-tip-cash-awards.valor_tip
               v_val_aplic_calc_ppr = 100
               v_qtd_unid_event_fp  = v_qtd_unid_event_fp + tt-tip-cash-awards.proporcao.
        

        if last-of(funcionario.cod_id_feder) then do:

            IF v_valor_tip_total <> tt-tip-cash-awards.valor_tip_total THEN
                ASSIGN v_valor_tip = (v_valor_tip * 12) / v_qtd_unid_event_fp.
            ELSE 
                ASSIGN v_valor_tip = v_valor_tip_total.
            
            assign ttfaixa_param_calc_ppr_func.val_aplic_calc_ppr = v_val_aplic_calc_ppr
                   ttfaixa_param_calc_ppr_func.val_base_calc_ppr  = v_valor_tip.

        end.

        assign v_cdn_indiv_factor = int(fn_numerico(entry(1, tt-tip-cash-awards.desempenho, '-'))) no-error.

        if v_cdn_indiv_factor > 0
        and not can-find(first tttip_func_indiv_factor_prov
                         where tttip_func_indiv_factor_prov.cdn_empresa       = funcionario.cdn_empresa    
                           and tttip_func_indiv_factor_prov.cdn_estab         = funcionario.cdn_estab      
                           and tttip_func_indiv_factor_prov.cdn_funcionario   = funcionario.cdn_funcionario
                           and tttip_func_indiv_factor_prov.cdn_indiv_factor  = v_cdn_indiv_factor         
                           and tttip_func_indiv_factor_prov.dt_inicio         = v_data_ini
                           and tttip_func_indiv_factor_prov.log_corporativo   = tt-tip-cash-awards.log_corporativo) then do:

            create tttip_func_indiv_factor_prov.
            assign tttip_func_indiv_factor_prov.cdn_empresa       = funcionario.cdn_empresa    
                   tttip_func_indiv_factor_prov.cdn_estab         = funcionario.cdn_estab      
                   tttip_func_indiv_factor_prov.cdn_funcionario   = funcionario.cdn_funcionario
                   tttip_func_indiv_factor_prov.cdn_indiv_factor  = v_cdn_indiv_factor
                   tttip_func_indiv_factor_prov.desc_indiv_factor = replace(trim(entry(2, tt-tip-cash-awards.desempenho, '-')), '"', '')
                   tttip_func_indiv_factor_prov.dt_inicio         = v_data_ini
                   tttip_func_indiv_factor_prov.dt_termino        = 12/31/9999
                   tttip_func_indiv_factor_prov.indice            = tt-tip-cash-awards.fator_individual
                   tttip_func_indiv_factor_prov.log_corporativo   = tt-tip-cash-awards.log_corporativo.


        end.

        assign v_cancelado = false.

    end.

    return string(v_cancelado, "NOK/OK").

end procedure.

procedure pi-importa-pagamento :

    def var v_data_limite as date no-undo.
    def var v_val_aplic_calc_ppr like faixa_param_calc_ppr_func.val_aplic_calc_ppr no-undo.
    def var v_qtd_unid_event_fp like movto_ppr.qtd_unid_event_fp no-undo.
    def var v_cancelado as logical no-undo.
    def var v_cdn_indiv_factor like tip_func_indiv_factor_pag.cdn_indiv_factor no-undo.
    def var v_chave as char no-undo.
    def var v_seq as int no-undo.
    def var v_data_ini as date no-undo.

    assign v_data_limite = date(if param_calc_ppr.num_mes_term_vigenc < 12 
                                then param_calc_ppr.num_mes_term_vigenc + 1 else 1, 1,
                                if param_calc_ppr.num_mes_term_vigenc < 12 
                                then param_calc_ppr.num_ano_term_vigenc
                                else param_calc_ppr.num_ano_term_vigenc + 1)
           v_data_ini    = date(tt-param.tt_num_mes_refer_calc_efetd, 1, 
                                tt-param.tt_num_ano_refer_calc_efetd).

    for each tt-tip-cash-awards,
        first funcionario no-lock
        where funcionario.cod_id_feder       = tt-tip-cash-awards.cod_id_feder
          and funcionario.cdn_empresa        = tt-param.v_cdn_empres_usuar
          and funcionario.cdn_estab         >= tt-param.tt_ini_cdn_estab
          and funcionario.cdn_estab         <= tt-param.tt_fim_cdn_estab 
          and funcionario.cdn_funcionario   >= tt-param.tt_ini_cdn_funcionario
          and funcionario.cdn_funcionario   <= tt-param.tt_fim_cdn_funcionario on stop undo, leave 
        break by funcionario.cod_id_feder :

        assign v_cancelado = true.

        run pi-acompanhar in h-acomp (input funcionario.cdn_funcionario).

        if funcionario.dat_desligto_func ne ?
        and not can-find(first habilit_rescis of funcionario no-lock
                         where habilit_rescis.dat_desligto_func < v_data_limite
                           and year(habilit_rescis.dat_desligto_func) = param_calc_ppr.num_ano_term_vigenc)
        then next.

        if funcionario.log_consid_calc_ppr = false then do:

            assign v_chave = 'FP1500' + funcionario.cdn_estab + string(funcionario.cdn_funcionario).

            if not can-find(first tt-erros
                            where tt-erros.chave = v_chave) then do:

                create tt-erros.
                assign v_seq = v_seq + 1
                       tt-erros.chave    = v_chave
                       tt-erros.seq      = v_seq
                       tt-erros.mensagem = substitute("Funcion rio &1 - &2 sem parƒmetro c lculo PLR (FP1500)",
                                                      funcionario.cdn_estab,
                                                      funcionario.cdn_funcionario).

            end.

            next.

        end.

        assign v_val_aplic_calc_ppr = v_val_aplic_calc_ppr + tt-tip-cash-awards.valor_tip
               v_qtd_unid_event_fp  = v_qtd_unid_event_fp + tt-tip-cash-awards.proporcao.

        if last-of(funcionario.cod_id_feder) then do:

            if v_val_aplic_calc_ppr > 0 then do:
                find last habilit_calc_fp no-lock
                    where habilit_calc_fp.num_ano_refer_fp_calcula = tt-param.tt_num_ano_refer_calc_efetd
                      and habilit_calc_fp.num_mes_refer_fp_calcula = tt-param.tt_num_mes_refer_calc_efetd
                      and habilit_calc_fp.cdn_empresa              = funcionario.cdn_empresa
                      and habilit_calc_fp.cdn_estab                = funcionario.cdn_estab  
                      and habilit_calc_fp.cdn_categ_sal            = funcionario.cdn_categ_sal
                      and habilit_calc_fp.idi_sit_calc_fp          = 1 no-error.

                if avail habilit_calc_fp then do:

                    find first habilit_calc_ppr no-lock
                        where habilit_calc_ppr.num_ano_refer_calc_efetd = tt-param.tt_num_ano_refer_calc_efetd
                          and habilit_calc_ppr.num_mes_refer_calc_efetd = tt-param.tt_num_mes_refer_calc_efetd
                          and habilit_calc_ppr.cdn_empresa              = funcionario.cdn_empresa
                          and habilit_calc_ppr.cdn_estab                = funcionario.cdn_estab  
                          and habilit_calc_ppr.idi_tip_folha_pagto_ppr  = habilit_calc_fp.idi_tip_fp 
                          and habilit_calc_ppr.qti_parc_habilit_calc_fp = habilit_calc_fp.qti_parc_habilit_calc_fp no-error.

                    if avail habilit_calc_ppr then do:

                        create ttmovto_ppr.

                        assign ttmovto_ppr.cdn_param_calc_ppr       = param_calc_ppr.cdn_param_calc_ppr
                               ttmovto_ppr.cdn_empresa              = funcionario.cdn_empresa          
                               ttmovto_ppr.cdn_estab                = funcionario.cdn_estab            
                               ttmovto_ppr.cdn_funcionario          = funcionario.cdn_funcionario      
                               ttmovto_ppr.num_mes_refer_calc_efetd = tt-param.tt_num_mes_refer_calc_efetd
                               ttmovto_ppr.num_ano_refer_calc_efetd = tt-param.tt_num_ano_refer_calc_efetd
                               ttmovto_ppr.idi_tip_folha_pagto_ppr  = habilit_calc_ppr.idi_tip_folha_pagto_ppr 
                               ttmovto_ppr.qti_parc_habilit_calc_fp = habilit_calc_ppr.qti_parc_habilit_calc_fp
                               ttmovto_ppr.cdn_event_fp             = param_calc_ppr.cdn_event_pagto_ppr
                               ttmovto_ppr.qtd_unid_event_fp        = v_qtd_unid_event_fp
                               ttmovto_ppr.val_calcul_efp           = v_val_aplic_calc_ppr
                               ttmovto_ppr.idi_orig_movto_ppr       = 1.

                    end.

                    else do:

                        assign v_chave = 'FP0900' + funcionario.cdn_estab + string(funcionario.cdn_categ_sal).

                        if not can-find(first tt-erros
                                        where tt-erros.chave = v_chave) then do:

                            create tt-erros.
                            assign v_seq = v_seq + 1
                                   tt-erros.chave    = v_chave
                                   tt-erros.seq      = v_seq
                                   tt-erros.mensagem = substitute("Habilita‡Æo de c lculo PLR nÆo encontrada para Estab &1 Categoria Sal &2 (FP0900)",
                                                                  funcionario.cdn_estab,
                                                                  funcionario.cdn_categ_sal).

                        end.

                    end.

                end.

                else do:

                    assign v_chave = 'FP3000' + funcionario.cdn_estab + string(funcionario.cdn_categ_sal).

                    if not can-find(first tt-erros
                                    where tt-erros.chave = v_chave) then do:

                        create tt-erros.
                        assign v_seq = v_seq + 1
                               tt-erros.chave    = v_chave
                               tt-erros.seq      = v_seq
                               tt-erros.mensagem = substitute("Habilita‡Æo de c lculo nÆo encontrada ou c lculo iniciado para Estab &1 Categoria Sal &2 (FP3000)",
                                                              funcionario.cdn_estab,
                                                              funcionario.cdn_categ_sal).

                    end.

                end.

            end.

            assign v_val_aplic_calc_ppr = 0
                   v_qtd_unid_event_fp  = 0.

        end.

        assign v_cdn_indiv_factor = int(fn_numerico(entry(1, tt-tip-cash-awards.desempenho, '-'))) no-error.

        if v_cdn_indiv_factor > 0
        and not can-find(first tttip_func_indiv_factor_pag
                         where tttip_func_indiv_factor_pag.cdn_empresa       = funcionario.cdn_empresa    
                           and tttip_func_indiv_factor_pag.cdn_estab         = funcionario.cdn_estab      
                           and tttip_func_indiv_factor_pag.cdn_funcionario   = funcionario.cdn_funcionario
                           and tttip_func_indiv_factor_pag.cdn_indiv_factor  = v_cdn_indiv_factor         
                           and tttip_func_indiv_factor_pag.dt_inicio         = v_data_ini
                           and tttip_func_indiv_factor_pag.log_corporativo   = tt-tip-cash-awards.log_corporativo) then do:

            create tttip_func_indiv_factor_pag.
            assign tttip_func_indiv_factor_pag.cdn_empresa       = funcionario.cdn_empresa    
                   tttip_func_indiv_factor_pag.cdn_estab         = funcionario.cdn_estab      
                   tttip_func_indiv_factor_pag.cdn_funcionario   = funcionario.cdn_funcionario
                   tttip_func_indiv_factor_pag.cdn_indiv_factor  = v_cdn_indiv_factor
                   tttip_func_indiv_factor_pag.desc_indiv_factor = replace(trim(entry(2, tt-tip-cash-awards.desempenho, '-')), '"', '')
                   tttip_func_indiv_factor_pag.dt_inicio         = v_data_ini
                   tttip_func_indiv_factor_pag.dt_termino        = 12/31/9999
                   tttip_func_indiv_factor_pag.indice            = tt-tip-cash-awards.fator_individual
                   tttip_func_indiv_factor_pag.log_corporativo   = tt-tip-cash-awards.log_corporativo.


        end.

        assign v_cancelado = false.

    end.

    return string(v_cancelado, "NOK/OK").

end procedure.

procedure pi-carrega-planilha :

    def var chExcelCom as com-handle no-undo.
    def var chWorkSheetCom as com-handle no-undo.

    def var v_arquivo_dif as char no-undo.
    def var v_registro as char no-undo.

    def var v_vectors_count as int64 no-undo.
    def var v_tuples_count as int no-undo.
    def var v_data_iniciado as logical no-undo.

    def var v_num_tuple as int no-undo.
    def var v_num_vector as int no-undo.
    def var v_indicator as char no-undo.
    def var v_value as char no-undo.

    def var v_seq as int no-undo.

    /* Referˆncias formato DIF - http://devel.archefire.org/mirrors/www.wotsit.org/listfa58.html?al=D */

    run pi-inicializar in h-acomp (input "Importando").

    file-info:filename = tt-param.tt_arquivo_importa.

    create "Excel.Application" chExcelCom.

    chExcelCom:DisplayAlerts = false.

    chExcelCom:Workbooks:open(file-info:pathname).

    chWorkSheetCom = chExcelCom:Sheets:item(1).

    chWorkSheetCom:select().

    &scoped-define xlDIF 9
    
    assign v_arquivo_dif = replace(file-info:pathname, ".xlsx", ".dif").
    
    chExcelCom:ActiveWorkbook:SaveAs(v_arquivo_dif, {&xlDIF},,,,,).

    chExcelCom:Quit().

    release object chExcelCom.
    release object chWorkSheetCom.

    assign chExcelCom     = ?
           chWorkSheetCom = ?.

    if search(v_arquivo_dif) ne ? then do:

        input from value(v_arquivo_dif) no-echo convert target session:charset.

        repeat:
            import unformatted v_registro.

            if v_data_iniciado = false then do:
                if v_registro = 'VECTORS' then do:

                    import unformatted v_registro.

                    assign v_vectors_count = int(entry(2, v_registro)) no-error.

                    next.

                end.

                if v_registro = 'TUPLES' then do:

                    import unformatted v_registro.

                    assign v_tuples_count = int(entry(2, v_registro)) no-error.

                    next.

                end.

                if v_registro = 'DATA' then do:

                    assign v_data_iniciado = true.

                    next.

                end.
            end.

            else do:

                if v_registro = '-1,0' then do:

                    import unformatted v_registro.

                    if v_registro = 'BOT' then do:

                        assign v_num_vector = v_num_vector + 1.

                        if v_num_vector > 5 then do:

                            create tt-tip-cash-awards.
                            assign v_seq = v_seq + 1
                                   tt-tip-cash-awards.seq = v_seq.

                            do v_num_tuple = 1 to v_tuples_count :

                                assign v_value = "".

                                import unformatted v_indicator.

                                if v_indicator = '1,0' then do:
                                    import unformatted v_value.
                                end.
                                else if entry(1, v_indicator) = '0' then do:

                                    assign v_value = substring(v_indicator, 3).

                                    import unformatted v_registro.

                                end.

                                case v_num_tuple :
                                    when 5 then assign tt-tip-cash-awards.log_corporativo   = trim(v_value) = '"Sim"'.
                                    when 6 then assign tt-tip-cash-awards.cod_id_feder      = fn_numerico(v_value).
                                    when 9 then assign tt-tip-cash-awards.dat_inicio        = date(v_value) no-error.
                                    when 10 then assign tt-tip-cash-awards.dat_termino      = date(v_value) no-error.
                                    when 12 then assign tt-tip-cash-awards.val_salario      = dec(v_value) no-error.
                                    when 13 then assign tt-tip-cash-awards.desempenho       = trim(v_value).
                                    when 14 then assign tt-tip-cash-awards.fator_individual = dec(v_value) no-error.
                                    when 15 then assign tt-tip-cash-awards.margem           = fn_decimal(v_value) no-error.
                                    when 16 then assign tt-tip-cash-awards.fator_margem     = dec(v_value) no-error.
                                    when 17 then assign tt-tip-cash-awards.resultado        = fn_decimal(v_value) no-error.
                                    when 18 then assign tt-tip-cash-awards.result_sal       = dec(v_value) no-error.
                                    when 20 then assign tt-tip-cash-awards.valor_tip_total  = dec(v_value) no-error.
                                    when 21 then assign tt-tip-cash-awards.proporcao        = int(v_value) no-error.
                                    when 22 then assign tt-tip-cash-awards.valor_tip        = dec(v_value) no-error.
                                end case.

                            end.
                        end.

                    end.

                    else if v_registro = 'EOD' then leave.

                end.

            end.

        end.

        input close.

        os-delete value(v_arquivo_dif).

    end.

end procedure.

function fn_numerico returns char (input p_valor as char) :

    def var v_ind as int no-undo.
    def var v_tamanho as int no-undo.
    def var v_resultado as char no-undo.
    def var v_car as char no-undo.

    assign v_tamanho = length(p_valor).

    do v_ind = 1 to v_tamanho :

        assign v_car = substring(p_valor, v_ind, 1).

        if v_car >= "0" and v_car <= "9" then 
            assign v_resultado = v_resultado + v_car.
    
    end.

    return v_resultado.

end function.

function fn_decimal returns dec (input p_valor as char) :

    def var v_ind as int no-undo.
    def var v_tamanho as int no-undo.
    def var v_resultado as char no-undo.
    def var v_car as char no-undo.

    assign v_tamanho = length(p_valor).

    do v_ind = 1 to v_tamanho :

        assign v_car = substring(p_valor, v_ind, 1).

        if (v_car >= "0" and v_car <= "9") or v_car = "," then 
            assign v_resultado = v_resultado + v_car.
    
    end.

    return dec(v_resultado).

end function.

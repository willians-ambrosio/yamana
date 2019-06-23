/******************************************************************************
** Programa       : ymfp0013rp.p
** Altera‡Æo      : 1789506  - 05/01/18 - FSW TOTVS Joinville HCM
** Objetivo       : Exporta‡Æo Planilha PLR
*****************************************************************************/
{include/i-prgvrs.i YMFP0013RP 1.02.00.001 } /*** 010001 ***/

/** Determina‡Æo da propor‡Æo e gera‡Æo de observa‡äes 

    Existem diferentes formas de apresenta‡Æo dessas informa‡äes. Cada uma est 
    dispon¡vel atrav‚s da ativa‡Æo de um pr‚-processador conforme descrito a seguir 

    ** Observa‡äes **
    
    Lista somente primeira observa‡Æo no mˆs - LISTA-PRIM-MES

    Lista somente £ltima observa‡Æo no mˆs - LISTA-ULT-MES
    
    Lista todas observa‡äes no mˆs - LISTA-TODAS-MES
    Aplica somente para provisäes -- SOMENTE-PROVISOES
    
    Default: Todas as observa‡äes 
    
    -----------------------------------------------------------------------------

    ** Migra‡Æo do avo **
    
    Regra Geral: o avo fica na origem se n£mero de dias trabalhados for superior
                 ao destino, caso contr rio fica no destino
    As op‡äes abaixo definem como deve ser caso ocorram mais de uma ocorrˆncia

    Regra Geral na primeira ocorrˆncia - AVO-PRIM-OCOR

    Regra Geral na £ltima ocorrˆncia - AVO-ULT-OCOR

    Default: Considera todas as ocorrˆncias para aplica‡Æo da Regra Geral             

*/    

/* &scoped-define LISTA-PRIM-MES */
/* &scoped-define LISTA-ULT-MES  */
 /*&scoped-define LISTA-TODAS-MES */
/* &scoped-define SOMENTE-PROVISOES */

/* &scoped-define AVO-PRIM-OCOR */
/* &scoped-define AVO-ULT-OCOR  */

def new global shared var i-num-ped-exec-rpw as int no-undo.      

&scoped-define YMFP0013RP
{prghur/esp/ymfp0013tt.i} /* <- todas temp-tables aqui */
{include/i-rpvar.i}

function fn_ultimo_dia_mes returns int (input p_mes as int) forward.
function fn_atualiza_dados_afastamentos returns logical (input p_cdn_empresa as char,
                                                         input p_cdn_estab as char,
                                                         input p_mes as int, 
                                                         input p_num_dias as int) forward.
function fn_dias_trabalhados return int (input p_data as date) forward.
function fn_dias_trabalhados_admis return int (input p_data as date) forward.
function fn_observacao returns char (input p_texto as char, input p_data as date) forward.
function fn_proporcao_com_aviso returns int (buffer bfuncionario for funcionario,
                                             input p_proporcao as int) forward.
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
      tt_des_ind_exporta label "Exporta"      colon 35 skip(1)
      tt_individual_factor label "Individual Factor" colon 35 skip(1)                                                     
      tt_arquivo_exporta  label "Arquivo Exporta‡Æo" colon 35

      SKIP(2)
      "IMPRESSÇO"                     at 15 skip(1)
      c-destino-impressao label "Destino" colon 35
      " - "
      tt-param.arquivo no-label  format "x(40)"       skip
      tt-param.usuario label "Usuario" colon 35 skip(1)

      with stream-io side-labels no-attr-space no-box width 255 1 down frame f-parametro. 

&scoped-define AFASTAMENTO 1
&scoped-define ENTRADA 2
&scoped-define PROMOCAO 3
&scoped-define CORP-ENTRADA 4
&scoped-define CORP-SAIDA 5
&scoped-define SOMA-AVO 0

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

def var h-acomp as handle no-undo.
def var v_arquivo_saida as char no-undo.
def var v_data_limite as date no-undo.
def var v_qti_funcionarios as int no-undo.

create tt-param.
raw-transfer raw-param to tt-param.

{include/i-rpout.i}
{include/i-rpcab.i}

view frame f-cabec.
view frame f-rodape.


find first param_calc_ppr no-lock
    where param_calc_ppr.cdn_param_calc_ppr = tt-param.tt_cdn_param_calc_ppr no-error.

find first param_empres_rh no-lock
     where param_empres_rh.cdn_empresa = tt-param.v_cdn_empres_usuar no-error.

file-info:filename = "doc-xls/F2017_TIP_Cash_Template_BR.xlsx".

if file-info:pathname = ? then do:

    &scoped-define MSG "NÆo foi poss¡vel localizar o arquivo doc-xls/F2017_TIP_Cash_Template_BR.xlsx"
    
    put {&MSG} format "x(120)" skip(1).

    {include/i-rpclo.i}

    if i-num-ped-exec-rpw = 0 then do:

        run utp/ut-msgs.p (input "show":U, input 17006, input {&MSG}).


    end.

    return "OK".

end.

assign v_arquivo_saida = tt-param.tt_arquivo_exporta.

os-delete value(v_arquivo_saida).

os-copy value(file-info:pathname) value(v_arquivo_saida).

assign file-info:filename = v_arquivo_saida.

if file-info:pathname = ? or os-error > 0 then do:

    &scoped-define NL chr(10)
    &scoped-define MSG substitute("NÆo foi poss¡vel criar o arquivo &1", v_arquivo_saida)
    &scoped-define HLP "Poss¡veis causas:" + {&NL} + {&NL} + ~
                       "O diret¢rio informado nÆo est  acess¡vel ou nÆo tem permissÆo para grava‡Æo" + {&NL} + {&NL} + ~
                       "Existe uma sessÆo do Microsoft Excel ou aplica‡Æo compat¡vel com o arquivo aberto" + {&NL} + {&NL} + ~
                       "Existe um arquivo no diret¢rio com mesmo nome que nÆo pode ser atualizado" + {&NL} + {&NL} + ~
                       "O nome do arquivo nÆo est  de acordo com o permitido pelo O.S."

    put {&MSG} format "x(120)" skip(1)
        {&HLP} format "x(350)".

    {include/i-rpclo.i}

    if i-num-ped-exec-rpw = 0 then do:

        run utp/ut-msgs.p (input "show":U, input 17006, input {&MSG} + "~~" + {&HLP}).


    end.

    return "OK".

end.

if tt-param.imprime_parametros then do:

    find empresa no-lock where empresa.ep-codigo = tt-param.v_cdn_empres_usuar no-error.
    assign c-empresa  = "Empresa " + empresa.razao-social
           c-sistema = "Folha de Pagamento".

    ASSIGN c-destino-impressao = {varinc/var00002.i 04 tt-param.destino}.

    disp tt_ini_cdn_estab      
         tt_fim_cdn_estab 
         tt_ini_cdn_funcionario
         tt_fim_cdn_funcionario
         tt_cdn_param_calc_ppr      
         tt_num_mes_refer_calc_efetd
         tt_num_ano_refer_calc_efetd
         tt_des_ind_exporta 
         tt_individual_factor 
         tt_arquivo_exporta  
         c-destino-impressao
         tt-param.arquivo
         tt-param.usuario
         with frame f-parametro. 

end.

/** Parƒmetros FP0900 considerados:
        Gerais >> Calend rio Mensalistas
        Recebe PLR >> Data de Desligamento com Aviso Pr‚vio
        Gerais >> Sal rio C lculo
*/        

IF tt-param.tt_num_ano_refer_calc_efetd < param_empres_rh.num_ano_refer_calc_efetd THEN DO:

    run pi-forma-tip-cash-awards-retro (output v_data_limite).
END.
ELSE DO:
    run pi-forma-tip-cash-awards (output v_data_limite).

END.

if return-value = "NOK" then return "OK".
    
run pi-cria-planilha (input v_data_limite).

return "OK".
    

procedure pi-forma-tip-cash-awards :

    def output param p_data_limite as date no-undo. 

    def buffer bfuncionario for funcionario.

    def var v_retroativo as logical no-undo.
    def var v_data_limite as date no-undo.
    def var v_data_inicio as date no-undo.
    def var v_cancelado as logical no-undo.
    def var v_cdn_empresa like funcionario.cdn_empresa no-undo.
    def var v_cdn_estab like funcionario.cdn_estab no-undo.
    def var v_cdn_funcionario like funcionario.cdn_funcionario no-undo.
    DEF VAR v_data_ini_param AS DATE NO-UNDO.
    DEF VAR v_data_fim_param AS DATE NO-UNDO.
    DEF VAR v_prox           AS LOG NO-UNDO.

    run utp/ut-acomp.p persistent set h-acomp.

    run pi-inicializar in h-acomp (input "Buscando Informa‡äes").

    assign v_data_limite = date(if tt-param.tt_num_mes_refer_calc_efetd < 12 
                                then tt-param.tt_num_mes_refer_calc_efetd + 1 else 1, 1,
                                if tt-param.tt_num_mes_refer_calc_efetd < 12 
                                then tt-param.tt_num_ano_refer_calc_efetd
                                else tt-param.tt_num_ano_refer_calc_efetd + 1)
           v_data_inicio = date(tt-param.tt_num_mes_refer_calc_efetd, 1, 
                                tt-param.tt_num_ano_refer_calc_efetd).

    assign v_retroativo = tt-param.tt_num_mes_refer_calc_efetd < param_empres_rh.num_mes_refer_calc_efetd
                          and tt-param.tt_num_ano_refer_calc_efetd = param_empres_rh.num_ano_refer_calc_efetd
                          or tt-param.tt_num_ano_refer_calc_efetd < param_empres_rh.num_ano_refer_calc_efetd.

    ASSIGN v_data_ini_param = date(param_calc_ppr.num_mes_inic_vigenc,01,param_calc_ppr.num_ano_inic_vigenc). 
        
    for each sit_afast no-lock :
        create tt-situacoes.
        assign tt-situacoes.cdn_sit_afast_func = sit_afast.cdn_sit_afast_func
               tt-situacoes.influi_plr         = substr(sit_afast.cod_livre_1,004,001) = "1".
    end.

    FOR EACH rh_pessoa_fisic
       WHERE rh_pessoa_fisic.cod_id_feder >= tt-param.v_cod_id_feder_ini
         AND rh_pessoa_fisic.cod_id_feder <= tt-param.v_cod_id_feder_fim,
        each funcionario OF rh_pessoa_fisic no-lock
       where funcionario.cdn_empresa      = tt-param.v_cdn_empres_usuar
         AND funcionario.dat_admis_func   < v_data_limite
         and funcionario.log_consid_calc_ppr
           on stop undo, leave 
        BY rh_pessoa_fisic.cod_id_feder
        BY funcionario.cdn_funcionario :

        ASSIGN v_prox = NO.

        IF tt-param.tp_func = 1 AND 
            (funcionario.dat_desligto_func <> ? AND
             funcionario.dat_desligto_func < v_data_inicio) THEN
            NEXT.

        IF tt-param.tp_func = 1 AND 
           (funcionario.dat_desligto_func <> ? AND
            MONTH(funcionario.dat_desligto_func) = MONTH(v_data_inicio)) THEN DO:
            IF DAY(funcionario.dat_desligto_func) < 15 THEN
                NEXT.
        END.

        IF tt-param.tp_func = 1 THEN DO:

            for each sit_afast_func of funcionario no-lock
               where sit_afast_func.cdn_func_orig > 0
                 and sit_afast_func.dat_inic_sit_afast = v_data_limite,
               first sit_afast of sit_afast_func no-lock
               where sit_afast.idi_signif_sit = 3 :

                ASSIGN v_prox = YES.
            END.

            IF v_prox = YES THEN
                NEXT.
        END.

        IF tt-param.tp_func = 2 THEN do:

            IF (funcionario.dat_desligto_func = ?) THEN
                NEXT.

           
            IF NOT CAN-FIND(FIRST habilit_rescis of funcionario NO-LOCK where
                                  habilit_rescis.dat_desligto_func  >= v_data_ini_param and
                                  habilit_rescis.dat_desligto_func  < v_data_limite ) THEN
                NEXT.

           IF v_prox = YES THEN
                NEXT.
           
        END.

        IF tt-param.tp_func = 3 THEN DO:


            IF funcionario.dat_desligto_func <> ? THEN DO:
                
                IF funcionario.dat_desligto_func < v_data_ini_param THEN
                    NEXT.
            END.
            
            IF v_prox = YES THEN
                NEXT.
        END.

            
         
        assign v_cancelado = true.

        run pi-acompanhar in h-acomp (input funcionario.cdn_funcionario).

        run pi-busca-entradas (buffer funcionario, input (v_data_limite)).
        
        if v_retroativo then do:

            assign v_cdn_empresa     = funcionario.cdn_empresa
                   v_cdn_estab       = funcionario.cdn_estab  
                   v_cdn_funcionario = funcionario.cdn_funcionario.

            find last tt-entradas
                where tt-entradas.dat_inic < v_data_limite no-error.

            if avail tt-entradas THEN DO:
            
                assign v_cdn_empresa     = tt-entradas.cdn_empresa    
                       v_cdn_estab       = tt-entradas.cdn_estab      
                       v_cdn_funcionario = tt-entradas.cdn_funcionario.
                 
            END.
            else do:

                find first tt-entradas
                    where tt-entradas.dat_inic > v_data_inicio no-error.

                if avail tt-entradas then
                    assign v_cdn_empresa     = tt-entradas.cdn_empres_orig
                           v_cdn_estab       = tt-entradas.cdn_estab_orig 
                           v_cdn_funcionario = tt-entradas.cdn_func_orig.

                if v_cdn_funcionario = 0 then next.
                 
            end.

            find first bfuncionario no-lock
                 where bfuncionario.cdn_empresa     = v_cdn_empresa    
                   and bfuncionario.cdn_estab       = v_cdn_estab      
                   and bfuncionario.cdn_funcionario = v_cdn_funcionario no-error.

            if bfuncionario.dat_desligto_func ne ?
            and bfuncionario.dat_desligto_func < v_data_ini_param then next. 
             
        end.
        else do: 
            find bfuncionario no-lock
              where rowid(bfuncionario) = rowid(funcionario) no-error.
        end.   
                                                                      
        run pi-cria-tip-cash-funcionario (buffer bfuncionario, input v_data_limite).

        assign v_cancelado = false.

    end.

    if v_cancelado = false then do:
        run pi-finaliza-tip-cash-awards (input v_data_limite).

        assign p_data_limite = v_data_limite.

    end.

    else os-delete value(v_arquivo_saida).

    run pi-finalizar in h-acomp.

    return string(v_cancelado, "NOK/OK").

end procedure.




procedure pi-finaliza-tip-cash-awards :

    def input param p_data_lim as date no-undo.

    def buffer btt-tip-cash-awards for tt-tip-cash-awards.

    def var v_seq as int no-undo.
    def var v_data_inicio as date no-undo.
    def var v_data_termino as date no-undo.

    /* Classifica‡Æo padrÆo */
    for each tt-tip-cash-awards
        where tt-tip-cash-awards.principal
        by tt-tip-cash-awards.nom_pessoa_fisic
        by tt-tip-cash-awards.dat_inic_lotac_func :

        assign v_seq = v_seq + 1
               tt-tip-cash-awards.seq = v_seq.

        for each btt-tip-cash-awards
           where btt-tip-cash-awards.row_table = rowid(tt-tip-cash-awards)
              by btt-tip-cash-awards.dat_inic_lotac_func :
            assign v_seq = v_seq + 1
                   btt-tip-cash-awards.seq = v_seq.
        end.                                       
    end.

    assign v_data_inicio = date(tt-param.tt_num_mes_refer_calc_efetd, 1, tt-param.tt_num_ano_refer_calc_efetd) - 1
           v_data_termino = p_data_lim.

    for each tt-tip-cash-awards
        break by tt-tip-cash-awards.cdn_empresa
              by tt-tip-cash-awards.cdn_estab
              by tt-tip-cash-awards.log_corporativo :

       if first-of(tt-tip-cash-awards.log_corporativo) then do:
            create tt-indiv-factor-lists.
            assign tt-indiv-factor-lists.cdn_empresa     = tt-tip-cash-awards.cdn_empresa    
                   tt-indiv-factor-lists.cdn_estab       = tt-tip-cash-awards.cdn_estab      
                   tt-indiv-factor-lists.log_corporativo = tt-tip-cash-awards.log_corporativo.

            if tt-param.tt_ind_tip_exporta = 1 then do:
                {prghur/esp/ymfp0013rp.i &tip_est_contrib_margin="tip_est_contrib_margin_prov" &tip_est_targets="tip_est_targets_prov"}
            end.
            else do:
                {prghur/esp/ymfp0013rp.i &tip_est_contrib_margin="tip_est_contrib_margin_pag" &tip_est_targets="tip_est_targets_pag"}
            end.
        end.    
    end.

end procedure.

procedure pi-cria-tip-cash-funcionario :

    def param buffer bfuncionario for funcionario.
    def input param p_data_lim as date no-undo.

    def buffer btt-tip-cash-awards for tt-tip-cash-awards.
    def buffer btt-ocorrencias for tt-ocorrencias.

    def var v_mes as int no-undo.
    def var v_mes_ini as int no-undo.
    def var v_mes_fim as int no-undo.
    def var v_data_lim as date no-undo.
    def var v_admitido as logical no-undo.
    def var v_desligado as logical no-undo.
    def var v_quebra as logical no-undo.
    def var v_dat_fim_lotac_func as date no-undo.
    def var v_ult_log_corporativo like tip_indiv_factor.log_corporativo no-undo.
    def var v_avo_migra as logical no-undo.
    def var v_qti_dias_max as int no-undo.
    def var v_row_table_migra as rowid no-undo.
    def var v_qti_dias as int no-undo.
    def var v_seq as int no-undo.
    DEF VAR v_considera AS LOG INIT NO NO-UNDO.

    do on stop undo, return "NOK" :
        run pi-busca-ocorrencias (buffer bfuncionario, input p_data_lim).

        assign v_qti_funcionarios = v_qti_funcionarios + 1.

        create tt-tip-cash-awards.
        assign tt-tip-cash-awards.cod_id_feder        = string(bfuncionario.cod_id_feder, "99999999999")
               tt-tip-cash-awards.nom_pessoa_fisic    = bfuncionario.nom_pessoa_fisic
               tt-tip-cash-awards.num_pessoa_fisic    = bfuncionario.num_pessoa_fisic
               tt-tip-cash-awards.cdn_estab           = bfuncionario.cdn_estab
               tt-tip-cash-awards.cdn_empresa         = bfuncionario.cdn_empresa
               tt-tip-cash-awards.cdn_funcionario     = bfuncionario.cdn_funcionario
               tt-tip-cash-awards.dat_inic_lotac_func = bfuncionario.dat_admis_func
               tt-tip-cash-awards.log_corporativo     = FALSE /*V*/
               tt-tip-cash-awards.dat_fim_lotac_func  = if bfuncionario.dat_desligto_func ne ?
                                                        then bfuncionario.dat_desligto_func
                                                        else 12/31/9999
               v_dat_fim_lotac_func                   = tt-tip-cash-awards.dat_fim_lotac_func. 
                                                             

        for last func_unid_lotac_plano no-lock
            where func_unid_lotac_plano.cdn_empresa         = bfuncionario.cdn_empresa
              and func_unid_lotac_plano.cdn_estab           = bfuncionario.cdn_estab
              and func_unid_lotac_plano.cdn_funcionario     = bfuncionario.cdn_funcionario
              and func_unid_lotac_plano.dat_inic_lotac_func < p_data_lim,
            first unid_lotac_plano of func_unid_lotac_plano no-lock,
            first unid_lotac of unid_lotac_plano no-lock :

            assign tt-tip-cash-awards.des_unid_lotac = unid_lotac.des_unid_lotac.  
        end.

        for last func_ccusto no-lock
            where func_ccusto.cdn_empresa         = bfuncionario.cdn_empresa          
              and func_ccusto.cdn_estab           = bfuncionario.cdn_estab            
              and func_ccusto.cdn_funcionario     = bfuncionario.cdn_funcionario      
              and func_ccusto.dat_inic_lotac_func < p_data_lim,
            first rh_ccusto of func_ccusto no-lock :

            assign tt-tip-cash-awards.des_rh_ccusto = rh_ccusto.des_rh_ccusto
                   tt-tip-cash-awards.cod_rh_ccusto = rh_ccusto.cod_rh_ccusto.
        end.

        IF tt-param.tt_num_ano_refer_calc_efetd < param_empres_rh.num_ano_refer_calc_efetd THEN DO:
            run pi-atualiza-informacoes-cargo-prim-2 (buffer bfuncionario,
                                                    input p_data_lim - 1,
                                                    buffer tt-tip-cash-awards).
        END.
        ELSE DO:
            run pi-atualiza-informacoes-cargo-prim (buffer bfuncionario,
                                                    input p_data_lim - 1,
                                                    buffer tt-tip-cash-awards).
        END.


        if tt-param.tt_num_ano_refer_calc_efetd = param_empres_rh.num_ano_refer_calc_efetd 
        then assign v_mes_fim = min(param_empres_rh.num_mes_refer_calc_efetd, tt-param.tt_num_mes_refer_calc_efetd).
        else assign v_mes_fim = tt-param.tt_num_mes_refer_calc_efetd.

        assign v_mes_ini   = 1
               v_admitido  = year(bfuncionario.dat_admis_func) = tt-param.tt_num_ano_refer_calc_efetd
               v_desligado = can-find(first habilit_rescis of bfuncionario no-lock
                                      where year(habilit_rescis.dat_desligto_func) = tt-param.tt_num_ano_refer_calc_efetd).

        if v_admitido then assign v_mes_ini = month(bfuncionario.dat_admis_func).

        do v_mes = v_mes_ini to v_mes_fim :

            assign v_data_lim = date(if v_mes < 12 then v_mes + 1 else 1, 1,
                                     if v_mes < 12 then tt-param.tt_num_ano_refer_calc_efetd
                                               else tt-param.tt_num_ano_refer_calc_efetd + 1).

                                                    
            if v_admitido and month(bfuncionario.dat_admis_func) = v_mes then do:

                if fn_dias_trabalhados(bfuncionario.dat_admis_func) < 15 then do:

                    run pi-cria-observacao (input rowid(tt-tip-cash-awards),
                                            input fn_observacao("Admitido em &1 de &2. ", bfuncionario.dat_admis_func),
                                            input v_mes).
                    next.
                end.

            end.

            if v_desligado and month(bfuncionario.dat_desligto_func) = v_mes then do:
                 
                if substr(param_calc_ppr.cod_livre_1,003,01) = "1" then do:

                    if day(bfuncionario.dat_desligto_func) > 14 then
                        assign tt-tip-cash-awards.proporcao = tt-tip-cash-awards.proporcao + 1.
                    else
                        run pi-cria-observacao (input rowid(tt-tip-cash-awards),
                                                input fn_observacao("Desligado em &1 de &2. ", bfuncionario.dat_desligto_func),
                                                input v_mes).
                        
                end.
                else if substr(param_calc_ppr.cod_livre_1,003,01) = "2" then do: /* Data de Desligamento com Aviso Pr‚vio */

                    assign tt-tip-cash-awards.proporcao = fn_proporcao_com_aviso(buffer bfuncionario, tt-tip-cash-awards.proporcao).

                    
                end.

                LEAVE.
            end.

            if can-find(first tt-ocorrencias
                        where tt-ocorrencias.mes_processo = v_mes  
                          and tt-ocorrencias.tipo         = {&AFASTAMENTO}) then do:

                find first tt-ocorrencias
                    where tt-ocorrencias.mes_processo = v_mes  
                      and tt-ocorrencias.tipo         = {&AFASTAMENTO} no-error.

                run pi-cria-observacao (input rowid(tt-tip-cash-awards),
                                        input tt-ocorrencias.observacoes,
                                        input v_mes).

                next.

            end.

            assign v_avo_migra = false.

            for each tt-ocorrencias
               where tt-ocorrencias.mes_processo = v_mes 
            break by tt-ocorrencias.dat_inic_lotac_func :

                case tt-ocorrencias.tipo :
                    when {&ENTRADA} or when {&PROMOCAO} or when {&CORP-ENTRADA} then
                        assign v_quebra = true.
                    otherwise assign v_quebra = false.
                end case.
                
                if v_mes = 1 and day(tt-ocorrencias.dat_inic_lotac_func) = 1 then do:

                    assign tt-tip-cash-awards.dat_inic_lotac_func = tt-ocorrencias.dat_inic_lotac_func
                               v_quebra = false.

                    IF v_desligado = NO THEN
                        ASSIGN tt-tip-cash-awards.dat_fim_lotac_func  = tt-ocorrencias.dat_fim_lotac_func.
                end.

                IF tt-ocorrencias.dat_inic_lotac_func = tt-tip-cash-awards.dat_inic_lotac_func and
                    (tt-ocorrencias.tipo = 4  )THEN DO:

                    ASSIGN v_quebra = FALSE.
                    
                END.

                if v_quebra then do:

                    
                    find last btt-tip-cash-awards
                        where btt-tip-cash-awards.row_table = rowid(tt-tip-cash-awards) no-error.

                    if avail btt-tip-cash-awards then do:

                        assign v_ult_log_corporativo = btt-tip-cash-awards.log_corporativo.

                        if tt-ocorrencias.dat_inic_lotac_func > btt-tip-cash-awards.dat_inic_lotac_func THEN DO:

                            assign btt-tip-cash-awards.dat_fim_lotac_func = tt-ocorrencias.dat_inic_lotac_func - 1.
                        END.
                            
                        else
                            assign v_quebra = false.
                    end.
                    else do:
                        
                       
                            assign tt-tip-cash-awards.dat_fim_lotac_func = tt-ocorrencias.dat_inic_lotac_func - 1
                                   v_ult_log_corporativo                 = tt-tip-cash-awards.log_corporativo.
                        
                    end.

                    if v_quebra then do:

                        create btt-tip-cash-awards.
                        buffer-copy tt-tip-cash-awards except des_unid_lotac   
                                                              cod_rh_ccusto    
                                                              des_rh_ccusto    
                                                              cod_id_feder     
                                                              nom_pessoa_fisic 
                                                              des_nivel_lotacao
                                                              proporcao
                            to btt-tip-cash-awards.
                        if param_calc_ppr.idi_salario_calc_ppr = 2 then do:

                            run pi-atualiza-informacoes-cargo (buffer bfuncionario,
                                                             input v_data_lim,
                                                             buffer btt-tip-cash-awards).

                        end.

                        
                        assign v_seq = v_seq + 1
                               btt-tip-cash-awards.seq                 = v_seq
                               btt-tip-cash-awards.dat_inic_lotac_func = tt-ocorrencias.dat_inic_lotac_func
                               btt-tip-cash-awards.dat_fim_lotac_func  = min(tt-ocorrencias.dat_fim_lotac_func, v_dat_fim_lotac_func)
                               btt-tip-cash-awards.cdn_empresa         = tt-ocorrencias.cdn_empresa
                               btt-tip-cash-awards.cdn_estab           = tt-ocorrencias.cdn_estab 
                               btt-tip-cash-awards.cdn_funcionario     = tt-ocorrencias.cdn_funcionario
                               btt-tip-cash-awards.principal           = FALSE
                               btt-tip-cash-awards.row_table           = rowid(tt-tip-cash-awards).

                        if tt-ocorrencias.tipo = {&ENTRADA} THEN DO:
                        
                            assign btt-tip-cash-awards.log_corporativo = can-find(first btt-ocorrencias
                                                                                  where btt-ocorrencias.tipo = {&CORP-ENTRADA}
                                                                                    and btt-ocorrencias.cdn_empresa     = tt-ocorrencias.cdn_empresa    
                                                                                    and btt-ocorrencias.cdn_estab       = tt-ocorrencias.cdn_estab      
                                                                                    and btt-ocorrencias.cdn_funcionario = tt-ocorrencias.cdn_funcionario
                                                                                    and btt-tip-cash-awards.dat_inic_lotac_func >=
                                                                                        btt-ocorrencias.dat_inic_lotac_func
                                                                                    and btt-tip-cash-awards.dat_inic_lotac_func <=
                                                                                        btt-ocorrencias.dat_fim_lotac_func).
                        END.
                        else DO:
                        
                            assign btt-tip-cash-awards.log_corporativo = v_ult_log_corporativo.
                        END.
                        assign v_avo_migra = true.

                        /*v*/
                        IF tt-param.tt_num_ano_refer_calc_efetd < param_empres_rh.num_ano_refer_calc_efetd THEN DO:
                            run pi-atualiza-informacoes-cargo-2 (buffer bfuncionario,
                                                                input v_data_lim /*btt-tip-cash-awards.dat_inic_lotac_func*/ ,
                                                               buffer btt-tip-cash-awards).
                        END.
                        ELSE DO:
                            run pi-atualiza-informacoes-cargo (buffer bfuncionario,
                                                            input v_data_lim /*btt-tip-cash-awards.dat_inic_lotac_func*/ ,
                                                           buffer btt-tip-cash-awards).
                        END.
                        

                    end.

                    run pi-cria-observacao (input rowid(btt-tip-cash-awards),
                                            input tt-ocorrencias.observacoes,
                                            input v_mes).

                end.

                case tt-ocorrencias.tipo :

                    when {&ENTRADA} then do:
                        
                        find first tt-entradas
                             where tt-entradas.dat_inic = tt-ocorrencias.dat_inic_lotac_func no-error.

                        if avail tt-entradas and tt-entradas.cdn_func_orig > 0 AND
                                 tt-entradas.dat_inic <> DATE(tt-param.tt_num_mes_refer_calc_efetd,01,tt-param.tt_num_ano_refer_calc_efetd) AND
                                 tt-entradas.dat_inic <> DATE (01,01,tt-param.tt_num_ano_refer_calc_efetd) and
                                 v_considera = NO THEN DO:

                            ASSIGN v_considera = YES.
                            
                            assign tt-tip-cash-awards.cdn_estab    = tt-entradas.cdn_estab_orig
                                   tt-tip-cash-awards.cdn_empresa  = tt-entradas.cdn_empres_orig
                                   tt-tip-cash-awards.cdn_funcionario = tt-entradas.cdn_func_orig.
                            

                            
                        END.

                    end.
                    when {&PROMOCAO} then do:

                        if v_quebra then do:

                            for EACH tt-promocoes
                               where tt-promocoes.dat_inic <= tt-ocorrencias.dat_inic_lotac_func,
                                first cargo no-lock
                                where cargo.cdn_cargo_basic = tt-promocoes.cdn_cargo_basic :

                                IF tt-promocoes.tipo = 3 THEN DO:
                                    ASSIGN btt-tip-cash-awards.principal = TRUE
                                           btt-tip-cash-awards.des_unid_lotac = tt-tip-cash-awards.des_unid_lotac
                                           btt-tip-cash-awards.cod_rh_ccusto     = tt-tip-cash-awards.cod_rh_ccusto    
                                           btt-tip-cash-awards.des_rh_ccusto     = tt-tip-cash-awards.des_rh_ccusto    
                                           btt-tip-cash-awards.cod_id_feder      = tt-tip-cash-awards.cod_id_feder     
                                           btt-tip-cash-awards.nom_pessoa_fisic  = tt-tip-cash-awards.nom_pessoa_fisic 
                                           btt-tip-cash-awards.des_nivel_lotacao = tt-tip-cash-awards.des_nivel_lotacao.      

                                END.

                                for LAST histor_sal_func no-lock 
                                    where histor_sal_func.cdn_empresa     = bfuncionario.cdn_empresa    
                                      and histor_sal_func.cdn_estab       = bfuncionario.cdn_estab      
                                      and histor_sal_func.cdn_funcionario = bfuncionario.cdn_funcionario
                                      and histor_sal_func.dat_liber_sal   < p_data_lim
                                      AND histor_sal_func.cdn_cargo_basic = cargo.cdn_cargo_basic:
                                    

                                    FOR FIRST ext_niv_hier_funcnal of cargo no-lock :
                                        IF btt-tip-cash-awards.cdn_tip_grupo_cargo <> tt-tip-cash-awards.cdn_tip_grupo_cargo  THEN DO:

                                            case ext_niv_hier_funcnal.cdn_tip_grupo_cargo :
                                                when 1 then assign btt-tip-cash-awards.des_nivel_lotacao = "Diretor"
                                                                   btt-tip-cash-awards.cdn_tip_grupo_cargo = 1.
                                                when 2 then assign btt-tip-cash-awards.des_nivel_lotacao = "Gerente"
                                                                   btt-tip-cash-awards.cdn_tip_grupo_cargo = 2.
                                                when 3 then assign btt-tip-cash-awards.des_nivel_lotacao = "Coordenador"
                                                                   btt-tip-cash-awards.cdn_tip_grupo_cargo = 3.
                                                when 4 then assign btt-tip-cash-awards.des_nivel_lotacao = "Supervisor e SR"
                                                                   btt-tip-cash-awards.cdn_tip_grupo_cargo = 4.
                                                when 5 then assign btt-tip-cash-awards.des_nivel_lotacao = "Demais"
                                                                   btt-tip-cash-awards.cdn_tip_grupo_cargo = 5.
                                    
                                            end case.

                                            assign btt-tip-cash-awards.des_cargo_basic = cargo.des_cargo  
                                                   btt-tip-cash-awards.val_salario     = histor_sal_func.val_salario_mensal .
                                        END.
                                        ELSE DO:

                                            assign btt-tip-cash-awards.des_cargo_basic = cargo.des_cargo  
                                                   btt-tip-cash-awards.val_salario     = histor_sal_func.val_salario_mensal
                                                   tt-tip-cash-awards.des_cargo_basic = cargo.des_cargo  
                                                   tt-tip-cash-awards.val_salario     = histor_sal_func.val_salario_mensal .
                                        END.
                                    end.    
                                END.
                            end.
                        end.

                    end.
                    when {&CORP-ENTRADA} then do:

                        if v_quebra then do:

                            find last btt-tip-cash-awards
                                where btt-tip-cash-awards.row_table = rowid(tt-tip-cash-awards) no-error.

                            assign btt-tip-cash-awards.log_corporativo = true.

                        end.
                        ELSE DO:
                             IF tt-ocorrencias.dat_inic_lotac_func = tt-tip-cash-awards.dat_inic_lotac_func and
                                tt-ocorrencias.tipo = 4 THEN 
                                 assign tt-tip-cash-awards.log_corporativo = TRUE. /*V*/
                             ELSE
                                 assign tt-tip-cash-awards.log_corporativo = FALSE. /*V*/
                        END.
                            

                            
                    end.
                    when {&CORP-SAIDA} then do:

                        find last btt-tip-cash-awards
                            where btt-tip-cash-awards.num_pessoa_fisic = bfuncionario.num_pessoa_fisic no-error.

                        if avail btt-tip-cash-awards then do:

                            if btt-tip-cash-awards.cdn_empresa = tt-ocorrencias.cdn_empresa
                            and btt-tip-cash-awards.cdn_estab  = tt-ocorrencias.cdn_estab then do: 

                                assign btt-tip-cash-awards.dat_fim_lotac_func = tt-ocorrencias.dat_fim_lotac_func.

                                create btt-tip-cash-awards.
                                buffer-copy tt-tip-cash-awards except des_unid_lotac   
                                                                      cod_rh_ccusto    
                                                                      des_rh_ccusto    
                                                                      cod_id_feder     
                                                                      nom_pessoa_fisic 
                                                                      des_nivel_lotacao
                                                                      proporcao
                                    to btt-tip-cash-awards.

                                if param_calc_ppr.idi_salario_calc_ppr = 2 then do:

                                    run pi-atualiza-informacoes-cargo (buffer bfuncionario,
                                                                     input v_data_lim,
                                                                     buffer btt-tip-cash-awards).

                                end.

                                assign v_seq = v_seq + 1
                                       btt-tip-cash-awards.seq                 = v_seq
                                       btt-tip-cash-awards.dat_inic_lotac_func = tt-ocorrencias.dat_fim_lotac_func + 1
                                       btt-tip-cash-awards.dat_fim_lotac_func  = v_dat_fim_lotac_func
                                       btt-tip-cash-awards.cdn_empresa         = tt-ocorrencias.cdn_empresa
                                       btt-tip-cash-awards.cdn_estab           = tt-ocorrencias.cdn_estab 
                                       btt-tip-cash-awards.cdn_funcionario     = tt-ocorrencias.cdn_funcionario
                                       btt-tip-cash-awards.principal           = false
                                       btt-tip-cash-awards.log_corporativo     = false
                                       btt-tip-cash-awards.row_table           = rowid(tt-tip-cash-awards).

                                run pi-cria-observacao (input rowid(btt-tip-cash-awards),
                                                        input tt-ocorrencias.observacoes,
                                                        input tt-ocorrencias.mes_processo).

                                assign v_avo_migra = true.
                            end.

                        end.

                    end.
                    when {&SOMA-AVO} then do:

                         find last btt-tip-cash-awards
                            where btt-tip-cash-awards.row_table = rowid(tt-tip-cash-awards)
                              and btt-tip-cash-awards.dat_inic_lotac_func < v_data_lim no-error.

                        if avail btt-tip-cash-awards then
                            assign btt-tip-cash-awards.proporcao = btt-tip-cash-awards.proporcao + 1 when v_avo_migra = false.

                        else
                            assign tt-tip-cash-awards.proporcao = tt-tip-cash-awards.proporcao + 1.
                            

                    end.
                end case.

            end.

            if v_avo_migra then do:

                assign v_qti_dias_max    = 0
                       v_row_table_migra = ?.

                &IF defined(AVO-PRIM-OCOR) > 0 &THEN
                for first btt-tip-cash-awards
                    where btt-tip-cash-awards.num_pessoa_fisic = bfuncionario.num_pessoa_fisic
                      and (month(btt-tip-cash-awards.dat_inic_lotac_func) = v_mes
                       or month(btt-tip-cash-awards.dat_fim_lotac_func) = v_mes) :
                    assign v_row_table_migra = rowid(btt-tip-cash-awards).
                end.
                &elseif defined(AVO-ULT-OCOR) > 0 &then
                for last btt-tip-cash-awards
                    where btt-tip-cash-awards.num_pessoa_fisic = bfuncionario.num_pessoa_fisic
                      and (month(btt-tip-cash-awards.dat_inic_lotac_func) = v_mes
                       or month(btt-tip-cash-awards.dat_fim_lotac_func) = v_mes) :
                    assign v_row_table_migra = rowid(btt-tip-cash-awards).
                end.
                &else /* Todas as ocorrˆncias combinadas */
                for each btt-tip-cash-awards
                    where btt-tip-cash-awards.num_pessoa_fisic = bfuncionario.num_pessoa_fisic
                      and (month(btt-tip-cash-awards.dat_inic_lotac_func) = v_mes
                       or month(btt-tip-cash-awards.dat_fim_lotac_func) = v_mes) :

                    assign v_qti_dias = 0.

                    if month(btt-tip-cash-awards.dat_inic_lotac_func) = v_mes then
                        assign v_qti_dias = fn_dias_trabalhados(btt-tip-cash-awards.dat_inic_lotac_func).

                    if month(btt-tip-cash-awards.dat_fim_lotac_func) = v_mes then do:

                        if v_qti_dias > 0 then
                            assign v_qti_dias = v_qti_dias - fn_dias_trabalhados(btt-tip-cash-awards.dat_fim_lotac_func).
                        else 
                            assign v_qti_dias = day(btt-tip-cash-awards.dat_fim_lotac_func).

                    end.

                    if v_qti_dias > v_qti_dias_max then do:

                        assign v_qti_dias_max    = v_qti_dias
                               v_row_table_migra = rowid(btt-tip-cash-awards).

                    end.

                end.
                &ENDIF

                if v_row_table_migra ne ? then do:
                    find first btt-tip-cash-awards
                        where rowid(btt-tip-cash-awards) = v_row_table_migra no-error.

                    assign btt-tip-cash-awards.proporcao = btt-tip-cash-awards.proporcao + 1.
                end.
            end.

        end.

        &IF defined(LISTA-PRIM-MES) > 0 &THEN
            for each tt-observacoes
                where tt-observacoes.mes_processo ne tt-param.tt_num_mes_refer_calc_efetd :
    
                assign tt-observacoes.mostra = false.
    
            end.
        
            for each tt-observacoes
                where tt-observacoes.mes_processo = tt-param.tt_num_mes_refer_calc_efetd 
                break by tt-observacoes.seq :

                if first(tt-observacoes.seq) then next.

                assign tt-observacoes.mostra = false.

            end.

        &elseif defined(LISTA-ULT-MES) > 0 &THEN
        
            for each tt-observacoes
                where tt-observacoes.mes_processo ne tt-param.tt_num_mes_refer_calc_efetd :

                assign tt-observacoes.mostra = false.

            end.

            for each tt-observacoes
                where tt-observacoes.mes_processo = tt-param.tt_num_mes_refer_calc_efetd 
                break by tt-observacoes.seq :

                if last(tt-observacoes.seq) then next.

                assign tt-observacoes.mostra = false.

            end.

        &elseif defined(LISTA-TODAS-MES) > 0 &then
        
            &IF defined(SOMENTE-PROVISOES) > 0 &THEN
            if tt-param.tt_ind_tip_exporta = 1 then do:
            &ENDIF
        
            for each tt-observacoes
                where tt-observacoes.mes_processo ne tt-param.tt_num_mes_refer_calc_efetd :

                assign tt-observacoes.mostra = false.

            end.

        &ENDIF
    end.

    return "OK".

end procedure.

procedure pi-cria-observacao :

    def input param p_row_table as rowid no-undo.
    def input param p_observacao as char no-undo.
    def input param p_mes_processo as int no-undo.

    def buffer btt-observacoes for tt-observacoes.

    find last btt-observacoes
        where btt-observacoes.row_table = p_row_table no-error.

    create tt-observacoes.

    assign tt-observacoes.row_table    = p_row_table
           tt-observacoes.observacoes  = p_observacao
           tt-observacoes.mes_processo = p_mes_processo.

    if avail btt-observacoes then
        assign tt-observacoes.seq = btt-observacoes.seq + 1.
    else
        assign tt-observacoes.seq = 1.

end procedure.

procedure pi-atualiza-informacoes-cargo-prim :

    def param buffer bfuncionario for funcionario.
    def input param p_data_lim as date no-undo.
    def param buffer btt-tip-cash-awards for tt-tip-cash-awards.

    DEF BUFFER bhistor_sal_func FOR histor_sal_func.

    DEF VAR v_cont_histor AS INT NO-UNDO.
    DEF VAR v_data AS DATE NO-UNDO.
    DEF VAR v_cdn_cargo_ant AS INT NO-UNDO.
    DEF VAR v_data_aux_fim AS DATE NO-UNDO.
    DEF VAR v_cont_histor_novo AS INT NO-UNDO.

    ASSIGN v_cdn_cargo_ant = 0
           v_cont_histor   = 0
           v_cont_histor_novo   = 0.
   
    IF month(p_data_lim) = 01 THEN DO:

        for LAST histor_sal_func no-lock
            where histor_sal_func.cdn_empresa     = bfuncionario.cdn_empresa    
              and histor_sal_func.cdn_estab       = bfuncionario.cdn_estab      
              and histor_sal_func.cdn_funcionario = bfuncionario.cdn_funcionario
              and histor_sal_func.cdn_cargo_basic > 0 
              and histor_sal_func.dat_liber_sal <  p_data_lim,
            first cargo of histor_sal_func NO-LOCK,
            first ext_niv_hier_funcnal of cargo NO-LOCK
            BREAK BY histor_sal_func.dat_liber_sal DESC:

            
                if ext_niv_hier_funcnal.cdn_tip_grupo_cargo <> v_cdn_cargo_ant  then do:
                    ASSIGN v_cont_histor = v_cont_histor + 1.
                   
                    IF LAST-OF(histor_sal_func.dat_liber_sal) THEN
                        ASSIGN v_data = histor_sal_func.dat_liber_sal. 
                    
                end.
    
                assign v_cdn_cargo_ant   = ext_niv_hier_funcnal.cdn_tip_grupo_cargo.
            
        end.

        /*V*/
        for LAST histor_sal_func no-lock 
            where histor_sal_func.cdn_empresa     = bfuncionario.cdn_empresa    
              and histor_sal_func.cdn_estab       = bfuncionario.cdn_estab      
              and histor_sal_func.cdn_funcionario = bfuncionario.cdn_funcionario
              and histor_sal_func.dat_liber_sal <= v_data,
            first cargo of histor_sal_func no-lock:

            assign btt-tip-cash-awards.des_cargo_basic = cargo.des_cargo  
                   btt-tip-cash-awards.val_salario     = histor_sal_func.val_salario_mensal .
    
            
            FOR FIRST ext_niv_hier_funcnal of cargo no-lock :
                case ext_niv_hier_funcnal.cdn_tip_grupo_cargo :
                    when 1 then assign btt-tip-cash-awards.des_nivel_lotacao = "Diretor"
                                       btt-tip-cash-awards.cdn_tip_grupo_cargo = 1.
                    when 2 then assign btt-tip-cash-awards.des_nivel_lotacao = "Gerente"
                                       btt-tip-cash-awards.cdn_tip_grupo_cargo = 2.
                    when 3 then assign btt-tip-cash-awards.des_nivel_lotacao = "Coordenador"
                                       btt-tip-cash-awards.cdn_tip_grupo_cargo = 3.
                    when 4 then assign btt-tip-cash-awards.des_nivel_lotacao = "Supervisor e SR"
                                       btt-tip-cash-awards.cdn_tip_grupo_cargo = 4.
                    when 5 then assign btt-tip-cash-awards.des_nivel_lotacao = "Demais"
                                       btt-tip-cash-awards.cdn_tip_grupo_cargo = 5.
    
                end case.
            end.       
            
        end.
    END.
    ELSE DO:

        hist_sal:
        for each histor_sal_func no-lock
            where histor_sal_func.cdn_empresa     = bfuncionario.cdn_empresa    
              and histor_sal_func.cdn_estab       = bfuncionario.cdn_estab      
              and histor_sal_func.cdn_funcionario = bfuncionario.cdn_funcionario
              and histor_sal_func.cdn_cargo_basic > 0 
              and (year(histor_sal_func.dat_liber_sal) = year(p_data_lim) 
                OR histor_sal_func.dat_liber_sal <  p_data_lim),
            first cargo of histor_sal_func NO-LOCK,
            first ext_niv_hier_funcnal of cargo NO-LOCK
            BY histor_sal_func.dat_liber_sal DESC:

            
            IF YEAR(histor_sal_func.dat_liber_sal - 1) = YEAR(p_data_lim)  THEN DO:

                if ext_niv_hier_funcnal.cdn_tip_grupo_cargo <> v_cdn_cargo_ant then do:

                    ASSIGN v_cont_histor = v_cont_histor + 1.
    
                    ASSIGN v_data = histor_sal_func.dat_liber_sal. 
                    
                end.
    
                assign v_cdn_cargo_ant   = ext_niv_hier_funcnal.cdn_tip_grupo_cargo.
            END.
            ELSE DO:
                LEAVE hist_sal.
            END.
        end.

        IF v_cont_histor < 1 THEN
            ASSIGN v_data = p_data_lim.
                
        /*V*/
        for LAST histor_sal_func no-lock 
            where histor_sal_func.cdn_empresa     = bfuncionario.cdn_empresa    
              and histor_sal_func.cdn_estab       = bfuncionario.cdn_estab      
              and histor_sal_func.cdn_funcionario = bfuncionario.cdn_funcionario
              and histor_sal_func.dat_liber_sal < v_data,
            first cargo of histor_sal_func no-lock:

            assign btt-tip-cash-awards.des_cargo_basic = cargo.des_cargo  
                   btt-tip-cash-awards.val_salario     = histor_sal_func.val_salario_mensal .
    
            FOR FIRST ext_niv_hier_funcnal of cargo no-lock :
                case ext_niv_hier_funcnal.cdn_tip_grupo_cargo :
                    when 1 then assign btt-tip-cash-awards.des_nivel_lotacao = "Diretor"
                                       btt-tip-cash-awards.cdn_tip_grupo_cargo = 1.
                    when 2 then assign btt-tip-cash-awards.des_nivel_lotacao = "Gerente"
                                       btt-tip-cash-awards.cdn_tip_grupo_cargo = 2.
                    when 3 then assign btt-tip-cash-awards.des_nivel_lotacao = "Coordenador"
                                       btt-tip-cash-awards.cdn_tip_grupo_cargo = 3.
                    when 4 then assign btt-tip-cash-awards.des_nivel_lotacao = "Supervisor e SR"
                                       btt-tip-cash-awards.cdn_tip_grupo_cargo = 4.
                    when 5 then assign btt-tip-cash-awards.des_nivel_lotacao = "Demais"
                                       btt-tip-cash-awards.cdn_tip_grupo_cargo = 5.
    
                end case.
            end.  
        end.
    END.      
end procedure.


procedure pi-atualiza-informacoes-cargo-prim-2 :

    def param buffer bfuncionario for funcionario.
    def input param p_data_lim as date no-undo.
    def param buffer btt-tip-cash-awards for tt-tip-cash-awards.

    DEF BUFFER bhistor_sal_func FOR histor_sal_func.

    DEF VAR v_cont_histor AS INT NO-UNDO.
    DEF VAR v_data AS DATE NO-UNDO.
    DEF VAR v_cdn_cargo_ant AS INT NO-UNDO.
    DEF VAR v_data_aux_fim AS DATE NO-UNDO.
    DEF VAR v_cont_histor_novo AS INT NO-UNDO.
    DEF VAR v_data_aux_ini AS DATE NO-UNDO.

    ASSIGN v_cdn_cargo_ant = 0
           v_cont_histor   = 0
           v_cont_histor_novo = 0.

    IF month(p_data_lim) = 01 THEN DO:

        for LAST histor_sal_func no-lock
            where histor_sal_func.cdn_empresa     = bfuncionario.cdn_empresa    
              and histor_sal_func.cdn_estab       = bfuncionario.cdn_estab      
              and histor_sal_func.cdn_funcionario = bfuncionario.cdn_funcionario
              and histor_sal_func.cdn_cargo_basic > 0 
              and histor_sal_func.dat_liber_sal <  p_data_lim,
            first cargo of histor_sal_func NO-LOCK,
            first ext_niv_hier_funcnal of cargo NO-LOCK
            BREAK BY histor_sal_func.dat_liber_sal DESC:

            
                if ext_niv_hier_funcnal.cdn_tip_grupo_cargo <> v_cdn_cargo_ant then do:
                    ASSIGN v_cont_histor = v_cont_histor + 1.
                   
                    IF LAST-OF(histor_sal_func.dat_liber_sal) THEN
                        ASSIGN v_data = histor_sal_func.dat_liber_sal. 
                end.
                assign v_cdn_cargo_ant   = ext_niv_hier_funcnal.cdn_tip_grupo_cargo.
        end.

        /*V*/
        for LAST histor_sal_func no-lock 
            where histor_sal_func.cdn_empresa     = bfuncionario.cdn_empresa    
              and histor_sal_func.cdn_estab       = bfuncionario.cdn_estab      
              and histor_sal_func.cdn_funcionario = bfuncionario.cdn_funcionario
              and histor_sal_func.dat_liber_sal <= v_data,
            first cargo of histor_sal_func no-lock:

            assign btt-tip-cash-awards.des_cargo_basic = cargo.des_cargo  
                   btt-tip-cash-awards.val_salario     = histor_sal_func.val_salario_mensal .
    
            
            FOR FIRST ext_niv_hier_funcnal of cargo no-lock :
                case ext_niv_hier_funcnal.cdn_tip_grupo_cargo :
                    when 1 then assign btt-tip-cash-awards.des_nivel_lotacao = "Diretor"
                                       btt-tip-cash-awards.cdn_tip_grupo_cargo = 1.
                    when 2 then assign btt-tip-cash-awards.des_nivel_lotacao = "Gerente"
                                       btt-tip-cash-awards.cdn_tip_grupo_cargo = 2.
                    when 3 then assign btt-tip-cash-awards.des_nivel_lotacao = "Coordenador"
                                       btt-tip-cash-awards.cdn_tip_grupo_cargo = 3.
                    when 4 then assign btt-tip-cash-awards.des_nivel_lotacao = "Supervisor e SR"
                                       btt-tip-cash-awards.cdn_tip_grupo_cargo = 4.
                    when 5 then assign btt-tip-cash-awards.des_nivel_lotacao = "Demais"
                                       btt-tip-cash-awards.cdn_tip_grupo_cargo = 5.
    
                end case.
            end.       
            
        end.
    END.
    ELSE DO:

        ASSIGN v_data = ?
               v_data_aux_ini = ?.
        for each histor_sal_func no-lock
            where histor_sal_func.cdn_empresa     = bfuncionario.cdn_empresa    
              and histor_sal_func.cdn_estab       = bfuncionario.cdn_estab      
              and histor_sal_func.cdn_funcionario = bfuncionario.cdn_funcionario
              and histor_sal_func.cdn_cargo_basic > 0 
              and (year(histor_sal_func.dat_liber_sal) = year(p_data_lim) 
                OR histor_sal_func.dat_liber_sal <  p_data_lim),
            first cargo of histor_sal_func NO-LOCK,
            first ext_niv_hier_funcnal of cargo NO-LOCK
            BREAK BY histor_sal_func.dat_liber_sal DESC:

            
            ASSIGN v_cont_histor_novo = v_cont_histor_novo + 1.

            IF v_cont_histor_novo =     1 OR 
               histor_sal_func.dat_liber_sal = DATE(01,01,year(p_data_lim)) THEN
                ASSIGN v_data_aux_ini = histor_sal_func.dat_liber_sal.

            if ext_niv_hier_funcnal.cdn_tip_grupo_cargo <> v_cdn_cargo_ant AND
               v_cdn_cargo_ant <> 0 AND
               year(v_data) >  year(histor_sal_func.dat_liber_sal)  then do:
                
                ASSIGN v_cont_histor = v_cont_histor + 1.

                IF LAST-OF(histor_sal_func.dat_liber_sal) THEN
                    ASSIGN v_data = histor_sal_func.dat_liber_sal. 

                
            end.

            IF v_cont_histor = 0  THEN
                ASSIGN v_data = histor_sal_func.dat_liber_sal.

            assign v_cdn_cargo_ant   = ext_niv_hier_funcnal.cdn_tip_grupo_cargo.
            
        end.

        IF v_cont_histor < 1  THEN
            ASSIGN v_data = p_data_lim.

        IF YEAR(v_data) < YEAR(p_data_lim) - 2 THEN
            ASSIGN v_data = p_data_lim.

        IF v_data_aux_ini = DATE(01,01,year(p_data_lim)) THEN
            ASSIGN v_data = p_data_lim.

                 
        /*V*/
        for LAST histor_sal_func no-lock 
            where histor_sal_func.cdn_empresa     = bfuncionario.cdn_empresa    
              and histor_sal_func.cdn_estab       = bfuncionario.cdn_estab      
              and histor_sal_func.cdn_funcionario = bfuncionario.cdn_funcionario
              and histor_sal_func.dat_liber_sal <= v_data,
            first cargo of histor_sal_func no-lock:

            assign btt-tip-cash-awards.des_cargo_basic = cargo.des_cargo  
                   btt-tip-cash-awards.val_salario     = histor_sal_func.val_salario_mensal .
    
            FOR FIRST ext_niv_hier_funcnal of cargo no-lock :
                case ext_niv_hier_funcnal.cdn_tip_grupo_cargo :
                    when 1 then assign btt-tip-cash-awards.des_nivel_lotacao = "Diretor"
                                       btt-tip-cash-awards.cdn_tip_grupo_cargo = 1.
                    when 2 then assign btt-tip-cash-awards.des_nivel_lotacao = "Gerente"
                                       btt-tip-cash-awards.cdn_tip_grupo_cargo = 2.
                    when 3 then assign btt-tip-cash-awards.des_nivel_lotacao = "Coordenador"
                                       btt-tip-cash-awards.cdn_tip_grupo_cargo = 3.
                    when 4 then assign btt-tip-cash-awards.des_nivel_lotacao = "Supervisor e SR"
                                       btt-tip-cash-awards.cdn_tip_grupo_cargo = 4.
                    when 5 then assign btt-tip-cash-awards.des_nivel_lotacao = "Demais"
                                       btt-tip-cash-awards.cdn_tip_grupo_cargo = 5.
    
                end case.
            end.       
            
        end.
    END.
end procedure.

procedure pi-atualiza-informacoes-cargo-2 :

    def param buffer bfuncionario for funcionario.
    def input param p_data_lim as date no-undo.
    def param buffer btt-tip-cash-awards for tt-tip-cash-awards.

    DEF VAR v_cont_histor AS INT NO-UNDO.
    DEF VAR v_data AS DATE NO-UNDO.
    DEF VAR v_cdn_cargo_ant AS INT NO-UNDO.

    IF NOT CAN-FIND(LAST histor_sal_func no-lock 
        where histor_sal_func.cdn_empresa     = bfuncionario.cdn_empresa    
          and histor_sal_func.cdn_estab       = bfuncionario.cdn_estab      
          and histor_sal_func.cdn_funcionario = bfuncionario.cdn_funcionario
          and histor_sal_func.dat_liber_sal < p_data_lim) THEN 
        ASSIGN p_data_lim = v_data_limite.
    

    /*V*/
    for LAST histor_sal_func no-lock 
        where histor_sal_func.cdn_empresa     = bfuncionario.cdn_empresa    
          and histor_sal_func.cdn_estab       = bfuncionario.cdn_estab      
          and histor_sal_func.cdn_funcionario = bfuncionario.cdn_funcionario
          and histor_sal_func.dat_liber_sal < p_data_lim,
        first cargo of histor_sal_func no-lock:

        assign btt-tip-cash-awards.des_cargo_basic = cargo.des_cargo  .

        FOR FIRST ext_niv_hier_funcnal of cargo no-lock :
            case ext_niv_hier_funcnal.cdn_tip_grupo_cargo :
                when 1 then assign btt-tip-cash-awards.des_nivel_lotacao = "Diretor"
                                   btt-tip-cash-awards.cdn_tip_grupo_cargo = 1.
                when 2 then assign btt-tip-cash-awards.des_nivel_lotacao = "Gerente"
                                   btt-tip-cash-awards.cdn_tip_grupo_cargo = 2.
                when 3 then assign btt-tip-cash-awards.des_nivel_lotacao = "Coordenador"
                                   btt-tip-cash-awards.cdn_tip_grupo_cargo = 3.
                when 4 then assign btt-tip-cash-awards.des_nivel_lotacao = "Supervisor e SR"
                                   btt-tip-cash-awards.cdn_tip_grupo_cargo = 4.
                when 5 then assign btt-tip-cash-awards.des_nivel_lotacao = "Demais"
                                   btt-tip-cash-awards.cdn_tip_grupo_cargo = 5.

            end case.
        end.          
    end.
    
end procedure.

procedure pi-atualiza-informacoes-cargo :

    def param buffer bfuncionario for funcionario.
    def input param p_data_lim as date no-undo.
    def param buffer btt-tip-cash-awards for tt-tip-cash-awards.

    DEF VAR v_cont_histor AS INT NO-UNDO.
    DEF VAR v_data AS DATE NO-UNDO.
    DEF VAR v_cdn_cargo_ant AS INT NO-UNDO.

    IF NOT CAN-FIND(LAST histor_sal_func no-lock 
        where histor_sal_func.cdn_empresa     = bfuncionario.cdn_empresa    
          and histor_sal_func.cdn_estab       = bfuncionario.cdn_estab      
          and histor_sal_func.cdn_funcionario = bfuncionario.cdn_funcionario
          and histor_sal_func.dat_liber_sal < p_data_lim) THEN 
        ASSIGN p_data_lim = v_data_limite.
    

    /*V*/
    for LAST histor_sal_func no-lock 
        where histor_sal_func.cdn_empresa     = bfuncionario.cdn_empresa    
          and histor_sal_func.cdn_estab       = bfuncionario.cdn_estab      
          and histor_sal_func.cdn_funcionario = bfuncionario.cdn_funcionario
          and histor_sal_func.dat_liber_sal < p_data_lim,
        first cargo of histor_sal_func no-lock:

         
        assign btt-tip-cash-awards.des_cargo_basic = cargo.des_cargo  .

        FOR FIRST ext_niv_hier_funcnal of cargo no-lock :
            case ext_niv_hier_funcnal.cdn_tip_grupo_cargo :
                when 1 then assign btt-tip-cash-awards.des_nivel_lotacao = "Diretor"
                                   btt-tip-cash-awards.cdn_tip_grupo_cargo = 1.
                when 2 then assign btt-tip-cash-awards.des_nivel_lotacao = "Gerente"
                                   btt-tip-cash-awards.cdn_tip_grupo_cargo = 2.
                when 3 then assign btt-tip-cash-awards.des_nivel_lotacao = "Coordenador"
                                   btt-tip-cash-awards.cdn_tip_grupo_cargo = 3.
                when 4 then assign btt-tip-cash-awards.des_nivel_lotacao = "Supervisor e SR"
                                   btt-tip-cash-awards.cdn_tip_grupo_cargo = 4.
                when 5 then assign btt-tip-cash-awards.des_nivel_lotacao = "Demais"
                                   btt-tip-cash-awards.cdn_tip_grupo_cargo = 5.

            end case.
        end.    
        
    end.
    
end procedure.

procedure pi-cria-planilha :

    def input param p_data_lim as date no-undo.
    
    define variable chExcel as office.iface.excel.ExcelWrapper no-undo.
    define variable chWorkBook as office.iface.excel.WorkBook no-undo.
    define variable chWorkSheet as office.iface.excel.WorkSheet no-undo.
    define variable chRange as office.iface.excel.Range no-undo.

    def var chExcelCom as com-handle no-undo.
    def var chWorkSheetCom as com-handle no-undo.
    def var v_num_linha as int no-undo.
    def var v_format as char no-undo.
    def var v_observacoes as char no-undo.
    def var v_desempenho as char no-undo.
    def var v_fator as dec no-undo.
    def var v_data_inicio as date no-undo.
    def var v_data_termino as date no-undo.

    find last tt-tip-cash-awards no-error.

    run utp/ut-perc.p persistent set h-acomp.

    run pi-inicializar in h-acomp (input "Exportando", input if avail tt-tip-cash-awards
                                                             then tt-tip-cash-awards.seq else 0).

    file-info:filename = v_arquivo_saida.

    v_arquivo_saida = file-info:full-pathname.

    {office/office.i Excel chExcel}

    chExcel:Workbooks:Open(v_arquivo_saida).

    chExcel:DisplayAlerts = false.

    chWorkBook = chExcel:Workbooks(1).

    chWorkSheet = chWorkBook:Sheets:item(2).

    session:numeric-format = "American".
    session:date-format = "mdy".

    assign v_data_inicio = date(tt-param.tt_num_mes_refer_calc_efetd, 1, tt-param.tt_num_ano_refer_calc_efetd) - 1
           v_data_termino = p_data_lim
           v_num_linha = 2.

    for each tip_indiv_factor no-lock
        where tip_indiv_factor.dt_inicio  < v_data_termino
          and tip_indiv_factor.dt_termino > v_data_inicio,
        first tt-indiv-factor-lists
        where tt-indiv-factor-lists.cdn_empresa     = tip_indiv_factor.cdn_empresa      
          and tt-indiv-factor-lists.cdn_estab       = tip_indiv_factor.cdn_estab      
          and tt-indiv-factor-lists.log_corporativo = tip_indiv_factor.log_corporativo
        break by tip_indiv_factor.cdn_empresa
              by tip_indiv_factor.cdn_estab 
              by tip_indiv_factor.log_corporativo 
              by tip_indiv_factor.cdn_indiv_factor :

        if first-of(tip_indiv_factor.log_corporativo) then do:

            assign tt-indiv-factor-lists.inicial         = substitute("&1 - &2", 
                                                                      tip_indiv_factor.cdn_indiv_factor, 
                                                                      tip_indiv_factor.desc_indiv_factor)
                   tt-indiv-factor-lists.num_linha_ini   = v_num_linha
                   tt-indiv-factor-lists.num_linha_fim   = v_num_linha.
        end.

        if last-of(tip_indiv_factor.log_corporativo) then do:

            assign tt-indiv-factor-lists.num_linha_fim = v_num_linha.
        end.

        assign chWorkSheet:cells(v_num_linha, 1):value = tip_indiv_factor.cdn_empresa
               chWorkSheet:cells(v_num_linha, 2):value = tip_indiv_factor.cdn_estab
               chWorkSheet:cells(v_num_linha, 3):value = string(tip_indiv_factor.log_corporativo, "Sim/NÆo")
               chWorkSheet:cells(v_num_linha, 4):value = substitute("&1 - &2", tip_indiv_factor.cdn_indiv_factor, tip_indiv_factor.desc_indiv_factor)
               chWorkSheet:cells(v_num_linha, 5):value = string(tip_indiv_factor.indice)
               v_num_linha = v_num_linha + 1.
    end.

    v_num_linha = 6.

    chWorkSheet = chWorkBook:Sheets:item(1).

    for each tt-tip-cash-awards 
       where tt-tip-cash-awards.cdn_tip_grupo_cargo > 0
         AND tt-tip-cash-awards.dat_fim_lotac_func > tt-tip-cash-awards.dat_inic_lotac_func on stop undo, leave :

        run pi-acompanhar in h-acomp.

        if can-find(first btt-tip-cash-awards
                    where btt-tip-cash-awards.seq > tt-tip-cash-awards.seq) then do:

            chWorkSheet:range(substitute("A&1:X&1", v_num_linha)):copy(chWorkSheet:range(substitute("A&1:X&1", v_num_linha + 1))).

        end.

        if tt-tip-cash-awards.principal THEN
            assign chWorkSheet:cells(v_num_linha, 1):value = tt-tip-cash-awards.des_unid_lotac
                   chWorkSheet:cells(v_num_linha, 2):value = tt-tip-cash-awards.cod_rh_ccusto
                   chWorkSheet:cells(v_num_linha, 2):NumberFormat = fill("0", length(tt-tip-cash-awards.cod_rh_ccusto))
                   chWorkSheet:cells(v_num_linha, 3):value = tt-tip-cash-awards.des_rh_ccusto
                   chWorkSheet:cells(v_num_linha, 6):value = tt-tip-cash-awards.cod_id_feder
                   chWorkSheet:cells(v_num_linha, 7):value = tt-tip-cash-awards.nom_pessoa_fisic
                   v_format = chWorkSheet:cells(v_num_linha, 2):NumberFormat.

        assign chWorkSheet:cells(v_num_linha, 4):value = tt-tip-cash-awards.cdn_estab
               chWorkSheet:cells(v_num_linha, 4):NumberFormat = fill("0", length(tt-tip-cash-awards.cdn_estab))
               chWorkSheet:cells(v_num_linha, 5):value = string(tt-tip-cash-awards.log_corporativo, "Sim/NÆo")
               chWorkSheet:cells(v_num_linha, 8):value = tt-tip-cash-awards.des_cargo_basic
               chWorkSheet:cells(v_num_linha, 9):value = string(tt-tip-cash-awards.dat_inic_lotac_func, "99/99/9999")
               chWorkSheet:cells(v_num_linha, 10):value = string(tt-tip-cash-awards.dat_fim_lotac_func, "99/99/9999")
               chWorkSheet:cells(v_num_linha, 11):value = tt-tip-cash-awards.des_nivel_lotacao
               chWorkSheet:cells(v_num_linha, 12):value = trim(string(tt-tip-cash-awards.val_salario))
               chWorkSheet:cells(v_num_linha, 15):value = trim(string(tt-tip-cash-awards.margem))
               chWorkSheet:cells(v_num_linha, 16):value = trim(string(tt-tip-cash-awards.fator_margem))
               chWorkSheet:cells(v_num_linha, 17):value = trim(string(tt-tip-cash-awards.resultado))
               chWorkSheet:cells(v_num_linha, 18):value = trim(string(tt-tip-cash-awards.result_sal))
               chWorkSheet:cells(v_num_linha, 21):value = trim(string(tt-tip-cash-awards.proporcao))
               chWorkSheet:cells(v_num_linha, 2):NumberFormat = v_format.

        &scoped-define NL chr(10)

        assign v_observacoes = "".

        for each tt-observacoes
            where tt-observacoes.row_table = rowid(tt-tip-cash-awards) 
              and tt-observacoes.mostra :

            assign v_observacoes = v_observacoes + tt-observacoes.observacoes + {&NL}.

        end.

        chWorkSheet:cells(v_num_linha, 24):value = trim(v_observacoes).

        assign v_num_linha = v_num_linha + 1.

    end.

    assign chWorkSheet:Range("I1"):Formula = substitute("=SUM(V6:V&1)", if v_num_linha > 6 then v_num_linha - 1 else 6)
           chWorkSheet:Range("I2"):Formula = substitute("=I1/&1", if v_qti_funcionarios > 0 then v_qti_funcionarios else 1).

    assign chWorkSheet:cells(1, 1):value = trim(param_calc_ppr.des_param_calc_ppr)
           chWorkSheet:name              = substitute("Ref. &1 &2", fn_nome_mes(input tt-param.tt_num_mes_refer_calc_efetd),
                                                                    tt-param.tt_num_ano_refer_calc_efetd)

           chWorkSheet:cells(1, 1):value = chWorkSheet:cells(1, 1):value + " - " + tt-param.tt_des_ind_exporta.

    chWorkBook:Save().

    chWorkBook:close().

    chExcel:Quit().

    run pi-verifica-cancelamento.

    if return-value = "NOK" then return "OK".

    /** Esta parte usa o estilo programa‡Æo COM 
        por nÆo haver suporte ao objeto Validation em office/office.i 
        Assim que dispon¡vel considere alterar este trecho */


    assign file-info:filename = v_arquivo_saida.

    create "Excel.Application" chExcelCom.

    chExcelCom:DisplayAlerts = false.

    chExcelCom:Workbooks:open(file-info:pathname).

    chWorkSheetCom = chExcelCom:Sheets:item(1).

    v_num_linha = 6.

    &scoped-define xlValidateList 3
    &scoped-define xlValidAlertStop 1
    &scoped-define xlBetween 1

    for each tt-tip-cash-awards
        where tt-tip-cash-awards.cdn_tip_grupo_cargo > 0,
        first tt-indiv-factor-lists
        where tt-indiv-factor-lists.cdn_empresa     = tt-tip-cash-awards.cdn_empresa
          and tt-indiv-factor-lists.cdn_estab       = tt-tip-cash-awards.cdn_estab
          and tt-indiv-factor-lists.log_corporativo = tt-tip-cash-awards.log_corporativo on stop undo, leave :

        run pi-busca-desempenho (input p_data_lim, output v_desempenho, output v_fator).

        if v_desempenho = 'init' then do:

            chWorkSheetCom:Range(substitute("M&1", v_num_linha)):value = tt-indiv-factor-lists.inicial.

        end.

        else do:

            chWorkSheetCom:Range(substitute("M&1", v_num_linha)):value = v_desempenho.

        end.

        chWorkSheetCom:Range(substitute("M&1", v_num_linha)):Validation:delete().

        chWorkSheetCom:Range(substitute("M&1", v_num_linha)):Validation:add({&xlValidateList}, 
                                                                            {&xlValidAlertStop},
                                                                            {&xlBetween},
                                                                            substitute("='Individual Factor'!$D$&1:$D$&2", 
                                                                                       tt-indiv-factor-lists.num_linha_ini,
                                                                                       tt-indiv-factor-lists.num_linha_fim)).

        chWorkSheetCom:Range(substitute("N&1", v_num_linha)):Formula = substitute("=VLOOKUP(M&1,'Individual Factor'!D&2:E&3,2,0)", 
                                                                                  v_num_linha,
                                                                                  tt-indiv-factor-lists.num_linha_ini,
                                                                                  tt-indiv-factor-lists.num_linha_fim).


        assign v_num_linha = v_num_linha + 1.

    end.

    chExcelCom:ActiveWorkbook:Save.
    chExcelCom:ActiveWorkbook:close.

    chExcelCom:Quit().

    release object chExcelCom.
    release object chWorkSheetCom.

    assign chExcelCom     = ?
           chWorkSheetCom = ?.

    /* Fim da parte COM **/

    session:numeric-format = "European".
    session:date-format = "dmy".

    run pi-verifica-cancelamento.

    if valid-handle(h-acomp) then run pi-finalizar in h-acomp.

end procedure.

procedure pi-busca-ocorrencias :

    /** Busca por ocorrˆncias no per¡odo com impacto no c lculo como:
    
    Entradas
    Promo‡äes
    Afastamentos
    Entrada no Corporativo
    Sa¡da do Corporativo
    
    */                        

    def param buffer bfuncionario for funcionario.
    def input param p_data_lim as date no-undo.

    def buffer btt-ocorrencias for tt-ocorrencias.

    def var v_mes as int no-undo.
    def var v_mes_fim as int no-undo.
    def var v_data_ini as date no-undo.
    def var v_data_fim as date no-undo.
    def var v_terminos_corporativos as date extent 12 no-undo.

    empty temp-table tt-ocorrencias.

    run pi-busca-afastamentos (buffer bfuncionario, input p_data_lim).

    IF tt-param.tt_num_ano_refer_calc_efetd < param_empres_rh.num_ano_refer_calc_efetd THEN DO: 
        run pi-busca-promocoes-2 (buffer bfuncionario, input p_data_lim).
    END.
    ELSE DO:
        run pi-busca-promocoes (buffer bfuncionario, input p_data_lim).
    END.

    run pi-busca-corporativos (input p_data_lim).

    assign v_mes_fim = month(p_data_lim - 1).

    do v_mes = 1 to v_mes_fim :

        assign v_data_ini = date(v_mes, 1, tt-param.tt_num_ano_refer_calc_efetd)
               v_data_fim = date(if v_mes < 12 then v_mes + 1 else 1, 1,                           
                                 if v_mes < 12 then tt-param.tt_num_ano_refer_calc_efetd           
                                 else tt-param.tt_num_ano_refer_calc_efetd + 1) - 1. 

        create tt-ocorrencias.
        assign tt-ocorrencias.cdn_empresa         = bfuncionario.cdn_empresa
               tt-ocorrencias.cdn_estab           = bfuncionario.cdn_estab
               tt-ocorrencias.cdn_funcionario     = bfuncionario.cdn_funcionario
               tt-ocorrencias.mes_processo        = v_mes
               tt-ocorrencias.dat_inic_lotac_func = v_data_fim
               tt-ocorrencias.tipo                = {&SOMA-AVO}.

        for each tt-entradas
           where tt-entradas.mes = v_mes
             and tt-entradas.ano = tt-param.tt_num_ano_refer_calc_efetd
              by tt-entradas.dat_inic :

            create tt-ocorrencias.
            assign tt-ocorrencias.cdn_empresa         = tt-entradas.cdn_empresa    
                   tt-ocorrencias.cdn_estab           = tt-entradas.cdn_estab      
                   tt-ocorrencias.cdn_funcionario     = tt-entradas.cdn_funcionario
                   tt-ocorrencias.dat_inic_lotac_func = tt-entradas.dat_inic
                   tt-ocorrencias.dat_fim_lotac_func  = 12/31/9999
                   tt-ocorrencias.mes_processo        = v_mes
                   tt-ocorrencias.tipo                = {&ENTRADA}
                   tt-ocorrencias.observacoes         = fn_observacao("Transferido em &1 de &2. ", tt-entradas.dat_inic).

        end.

       
        for each tt-promocoes
            where tt-promocoes.mes = v_mes
              and tt-promocoes.ano = tt-param.tt_num_ano_refer_calc_efetd
              by tt-promocoes.dat_inic :

            create tt-ocorrencias.
            assign tt-ocorrencias.cdn_empresa         = tt-promocoes.cdn_empresa    
                   tt-ocorrencias.cdn_estab           = tt-promocoes.cdn_estab      
                   tt-ocorrencias.cdn_funcionario     = tt-promocoes.cdn_funcionario
                   tt-ocorrencias.dat_inic_lotac_func = tt-promocoes.dat_inic
                   tt-ocorrencias.dat_fim_lotac_func  = 12/31/9999
                   tt-ocorrencias.mes_processo        = v_mes
                   tt-ocorrencias.tipo                = {&PROMOCAO}
                   tt-ocorrencias.observacoes         = fn_observacao("Promovido em &1 de &2. ", tt-promocoes.dat_inic).     

        end.
        

    end.

    for each tt-afastamentos
        where tt-afastamentos.num_dias_afastado > 13 :

        if fn_ultimo_dia_mes(input tt-afastamentos.mes) - tt-afastamentos.num_dias_afastado < 15 then do:
            create tt-ocorrencias.
            assign tt-ocorrencias.cdn_empresa         = tt-afastamentos.cdn_empresa    
                   tt-ocorrencias.cdn_estab           = tt-afastamentos.cdn_estab      
                   tt-ocorrencias.cdn_funcionario     = tt-afastamentos.cdn_funcionario
                   tt-ocorrencias.mes_processo        = tt-afastamentos.mes
                   tt-ocorrencias.tipo                = {&AFASTAMENTO}
                   tt-ocorrencias.dat_inic_lotac_func = v_data_fim
                   tt-ocorrencias.observacoes         = substitute("Afastado por &1 dias em &2. ",
                                                                  tt-afastamentos.num_dias_afastado,
                                                                  fn_nome_mes(input tt-afastamentos.mes)).
        end.

    end.

end procedure.

procedure pi-busca-entradas :

    def param buffer bfuncionario for funcionario.
    def input param p_data_fim as date no-undo.

    def buffer btt-entradas for tt-entradas.

    def var v_privez as logical no-undo init true.

    empty temp-table tt-entradas.

    empty temp-table tt-funcionarios.

    for each sit_afast_func of bfuncionario no-lock
        where sit_afast_func.cdn_func_orig > 0
          and sit_afast_func.dat_inic_sit_afast < p_data_fim,
         
        first sit_afast of sit_afast_func no-lock
        where sit_afast.idi_signif_sit = 3 :

        create tt-entradas.
        assign tt-entradas.cdn_empresa     = sit_afast_func.cdn_empresa    
               tt-entradas.cdn_estab       = sit_afast_func.cdn_estab      
               tt-entradas.cdn_funcionario = sit_afast_func.cdn_funcionario
               tt-entradas.cdn_empres_orig = sit_afast_func.cdn_empres_orig 
               tt-entradas.cdn_estab_orig  = sit_afast_func.cdn_estab_orig  
               tt-entradas.cdn_func_orig   = sit_afast_func.cdn_func_orig   
               tt-entradas.dat_inic        = sit_afast_func.dat_inic_sit_afast
               tt-entradas.mes             = month(sit_afast_func.dat_inic_sit_afast)
               tt-entradas.ano             = year(sit_afast_func.dat_inic_sit_afast)
               tt-entradas.done            = false.

       
        if not can-find(first tt-funcionarios
                        where tt-funcionarios.cdn_empresa     = tt-entradas.cdn_empresa    
                          and tt-funcionarios.cdn_estab       = tt-entradas.cdn_estab      
                          and tt-funcionarios.cdn_funcionario = tt-entradas.cdn_funcionario) then do:
            create tt-funcionarios.
            assign tt-funcionarios.cdn_empresa     = tt-entradas.cdn_empresa    
                   tt-funcionarios.cdn_estab       = tt-entradas.cdn_estab      
                   tt-funcionarios.cdn_funcionario = tt-entradas.cdn_funcionario.
        end.

    end.

    do while true :

        find next tt-entradas
            where tt-entradas.done = false no-error.

        if not avail tt-entradas then do:

            if v_privez then do:
                create tt-funcionarios.
                assign tt-funcionarios.cdn_empresa     = bfuncionario.cdn_empresa    
                       tt-funcionarios.cdn_estab       = bfuncionario.cdn_estab      
                       tt-funcionarios.cdn_funcionario = bfuncionario.cdn_funcionario.
            end.

            leave.
        end.

        if v_privez then do:
            if tt-entradas.cdn_func_orig > 0 then do: 
                create tt-funcionarios.
                assign tt-funcionarios.cdn_empresa     = tt-entradas.cdn_empres_orig
                       tt-funcionarios.cdn_estab       = tt-entradas.cdn_estab_orig 
                       tt-funcionarios.cdn_funcionario = tt-entradas.cdn_func_orig  .
            end.
            assign v_privez = false.
        end.

        for each sit_afast_func no-lock
            where sit_afast_func.cdn_empresa        = tt-entradas.cdn_empres_orig
              and sit_afast_func.cdn_estab          = tt-entradas.cdn_estab_orig 
              and sit_afast_func.cdn_funcionario    = tt-entradas.cdn_func_orig,
            first sit_afast of sit_afast_func no-lock
            where sit_afast.idi_signif_sit = 3 :

            if not can-find(first btt-entradas
                            where btt-entradas.cdn_empresa     = sit_afast_func.cdn_empresa       
                              and btt-entradas.cdn_estab       = sit_afast_func.cdn_estab         
                              and btt-entradas.cdn_funcionario = sit_afast_func.cdn_funcionario   
                              and btt-entradas.dat_inic        = sit_afast_func.dat_inic_sit_afast) then do:

                create btt-entradas.
                assign btt-entradas.cdn_empresa     = sit_afast_func.cdn_empresa    
                       btt-entradas.cdn_estab       = sit_afast_func.cdn_estab      
                       btt-entradas.cdn_funcionario = sit_afast_func.cdn_funcionario
                       btt-entradas.cdn_empres_orig = sit_afast_func.cdn_empres_orig 
                       btt-entradas.cdn_estab_orig  = sit_afast_func.cdn_estab_orig  
                       btt-entradas.cdn_func_orig   = sit_afast_func.cdn_func_orig   
                       btt-entradas.dat_inic        = sit_afast_func.dat_inic_sit_afast
                       btt-entradas.mes             = month(sit_afast_func.dat_inic_sit_afast)
                       btt-entradas.ano             = year(sit_afast_func.dat_inic_sit_afast)
                       btt-entradas.done            = false.

                if not can-find(first tt-funcionarios
                                where tt-funcionarios.cdn_empresa     = btt-entradas.cdn_empresa    
                                  and tt-funcionarios.cdn_estab       = btt-entradas.cdn_estab      
                                  and tt-funcionarios.cdn_funcionario = btt-entradas.cdn_funcionario) then do:
                    create tt-funcionarios.
                    assign tt-funcionarios.cdn_empresa     = btt-entradas.cdn_empresa    
                           tt-funcionarios.cdn_estab       = btt-entradas.cdn_estab      
                           tt-funcionarios.cdn_funcionario = btt-entradas.cdn_funcionario.
                end.

            end.

        end.

        assign tt-entradas.done = true.

    end.

end procedure.

procedure pi-busca-afastamentos :

    def param buffer bfuncionario for funcionario.
    def input param p_data_lim as date no-undo.

    def var v_data_ini as date no-undo.
    def var v_data_fim as date no-undo.
    def var v_dia_ini as int no-undo.
    def var v_dia_fim as int no-undo.
    def var v_mes_ini as int no-undo.
    def var v_mes_fim as int no-undo.
    def var v_mes as int no-undo.

    empty temp-table tt-afastamentos.

    assign v_data_ini = date(1, 1, tt-param.tt_num_ano_refer_calc_efetd)
           v_data_fim = p_data_lim - 1.

    for each sit_afast_func no-lock 
        where sit_afast_func.cdn_empresa     = bfuncionario.cdn_empresa    
          and sit_afast_func.cdn_estab       = bfuncionario.cdn_estab      
          and sit_afast_func.cdn_funcionario = bfuncionario.cdn_funcionario
          and sit_afast_func.dat_inic_sit_afast < p_data_lim 
          and sit_afast_func.dat_term_sit_afast >= v_data_ini,
        first tt-situacoes no-lock
        where tt-situacoes.cdn_sit_afast_func = sit_afast_func.cdn_sit_afast_func
          and tt-situacoes.influi_plr
        by sit_afast_func.dat_inic_sit_afast :

        if sit_afast_func.dat_inic_sit_afast >= v_data_ini then
            assign v_mes_ini = month(sit_afast_func.dat_inic_sit_afast)
                   v_dia_ini = day(sit_afast_func.dat_inic_sit_afast).
        else
            assign v_mes_ini = month(v_data_ini)
                   v_dia_ini = 1.

        if sit_afast_func.dat_term_sit_afast <= v_data_fim then
            assign v_mes_fim = month(sit_afast_func.dat_term_sit_afast)
                   v_dia_fim = day(sit_afast_func.dat_term_sit_afast).
        else
            assign v_mes_fim = month(v_data_fim)
                   v_dia_fim = fn_ultimo_dia_mes(input v_mes_fim).

        if v_mes_ini < v_mes_fim then do:
            fn_atualiza_dados_afastamentos(input bfuncionario.cdn_empresa,
                                           input bfuncionario.cdn_estab,
                                           input v_mes_ini, 
                                           input fn_ultimo_dia_mes(input v_mes_ini) - v_dia_ini + 1).

            fn_atualiza_dados_afastamentos(input bfuncionario.cdn_empresa,
                                           input bfuncionario.cdn_estab,
                                           input v_mes_fim, 
                                           input v_dia_fim).


        end.
        else do:

            fn_atualiza_dados_afastamentos(input bfuncionario.cdn_empresa,
                                           input bfuncionario.cdn_estab,
                                           input v_mes_ini, 
                                           input v_dia_fim - v_dia_ini + 1).

        end.

        do v_mes = v_mes_ini + 1 to v_mes_fim - 1 :

            fn_atualiza_dados_afastamentos(input bfuncionario.cdn_empresa,
                                           input bfuncionario.cdn_estab,
                                           input v_mes, 
                                           input fn_ultimo_dia_mes(input v_mes)).

        end.

    end.

end procedure.

procedure pi-busca-promocoes :

    def param buffer bfuncionario for funcionario.
    def input param p_data_lim as date no-undo.

    def buffer bhistor_sal_func for histor_sal_func.

    def var v_cdn_cargo_ant like histor_sal_func.cdn_cargo_basic no-undo.
    def var v_cdn_niv_ant like histor_sal_func.cdn_niv_cargo no-undo.
    def var v_val_salario_ant like histor_sal_func.val_salario_mensal no-undo.

    empty temp-table tt-promocoes.

    ASSIGN v_cdn_cargo_ant = 0.

    for LAST histor_sal_func no-lock
        where histor_sal_func.cdn_empresa     = bfuncionario.cdn_empresa    
          and histor_sal_func.cdn_estab       = bfuncionario.cdn_estab      
          and histor_sal_func.cdn_funcionario = bfuncionario.cdn_funcionario
          and histor_sal_func.cdn_cargo_basic > 0 
          and histor_sal_func.dat_liber_sal < p_data_lim,
        first cargo of histor_sal_func NO-LOCK,
        first ext_niv_hier_funcnal of cargo NO-LOCK
        BREAK BY histor_sal_func.dat_liber_sal DESC:

        
            if ext_niv_hier_funcnal.cdn_tip_grupo_cargo <> v_cdn_cargo_ant /*AND
               v_cdn_cargo_ant <> 0*/ then do:

                IF CAN-FIND(FIRST bhistor_sal_func OF histor_sal_func
                            WHERE bhistor_sal_func.dat_liber_sal < histor_sal_func.dat_liber_sal) THEN DO:

                    create tt-promocoes.
                    assign tt-promocoes.cdn_empresa      = bfuncionario.cdn_empresa    
                           tt-promocoes.cdn_estab        = bfuncionario.cdn_estab      
                           tt-promocoes.cdn_funcionario  = bfuncionario.cdn_funcionario
                           tt-promocoes.dat_inic         = histor_sal_func.dat_liber_sal
                           tt-promocoes.cdn_cargo_basic  = histor_sal_func.cdn_cargo_basic
                           tt-promocoes.mes              = month(histor_sal_func.dat_liber_sal)
                           tt-promocoes.ano              = year(histor_sal_func.dat_liber_sal)
                            .
        
                    ASSIGN tt-promocoes.tipo             = 1.
                END.
                ELSE DO:
                    
                    create tt-promocoes.
                    assign tt-promocoes.cdn_empresa      = bfuncionario.cdn_empresa    
                           tt-promocoes.cdn_estab        = bfuncionario.cdn_estab      
                           tt-promocoes.cdn_funcionario  = bfuncionario.cdn_funcionario
                           tt-promocoes.dat_inic         = histor_sal_func.dat_liber_sal
                           tt-promocoes.cdn_cargo_basic  = histor_sal_func.cdn_cargo_basic
                           tt-promocoes.mes              = month(histor_sal_func.dat_liber_sal)
                           tt-promocoes.ano              = year(histor_sal_func.dat_liber_sal)
                            .
        
                    ASSIGN tt-promocoes.tipo             = 3.

                END.
            end.
            
            assign v_cdn_cargo_ant   = ext_niv_hier_funcnal.cdn_tip_grupo_cargo
                   v_cdn_niv_ant     = histor_sal_func.cdn_niv_cargo
                   v_val_salario_ant = histor_sal_func.val_salario_mensal.
        
    end.

end procedure.

procedure pi-busca-promocoes-2 :

    def param buffer bfuncionario for funcionario.
    def input param p_data_lim as date no-undo.

    def buffer bhistor_sal_func for histor_sal_func.

    def var v_cdn_cargo_ant like histor_sal_func.cdn_cargo_basic no-undo.
    def var v_cdn_niv_ant like histor_sal_func.cdn_niv_cargo no-undo.
    def var v_val_salario_ant like histor_sal_func.val_salario_mensal no-undo.

    empty temp-table tt-promocoes.

    ASSIGN v_cdn_cargo_ant = 0.

    for LAST histor_sal_func no-lock
        where histor_sal_func.cdn_empresa     = bfuncionario.cdn_empresa    
          and histor_sal_func.cdn_estab       = bfuncionario.cdn_estab      
          and histor_sal_func.cdn_funcionario = bfuncionario.cdn_funcionario
          and histor_sal_func.cdn_cargo_basic > 0 
          and histor_sal_func.dat_liber_sal < p_data_lim 
          AND histor_sal_func.cdn_motiv_liber_sal <> 10 
          AND histor_sal_func.cdn_motiv_liber_sal <> 5,
        first cargo of histor_sal_func NO-LOCK,
        first ext_niv_hier_funcnal of cargo NO-LOCK
        BREAK BY histor_sal_func.dat_liber_sal  :
        
            IF histor_sal_func.cdn_motiv_liber_sal = 100 THEN
                ASSIGN p_data_lim  = histor_sal_func.dat_liber_sal
                       .

    END.

    
    for LAST histor_sal_func no-lock
        where histor_sal_func.cdn_empresa     = bfuncionario.cdn_empresa    
          and histor_sal_func.cdn_estab       = bfuncionario.cdn_estab      
          and histor_sal_func.cdn_funcionario = bfuncionario.cdn_funcionario
          and histor_sal_func.cdn_cargo_basic > 0 
          and histor_sal_func.dat_liber_sal < p_data_lim 
          AND histor_sal_func.cdn_motiv_liber_sal <> 10 
          AND histor_sal_func.cdn_motiv_liber_sal <> 5,
        first cargo of histor_sal_func NO-LOCK,
        first ext_niv_hier_funcnal of cargo NO-LOCK
        BREAK BY histor_sal_func.dat_liber_sal  :
        
            if ext_niv_hier_funcnal.cdn_tip_grupo_cargo <> v_cdn_cargo_ant  then do:
               
                IF CAN-FIND(FIRST bhistor_sal_func OF histor_sal_func
                            WHERE bhistor_sal_func.dat_liber_sal < histor_sal_func.dat_liber_sal) THEN DO:

                    create tt-promocoes.
                    assign tt-promocoes.cdn_empresa      = bfuncionario.cdn_empresa    
                           tt-promocoes.cdn_estab        = bfuncionario.cdn_estab      
                           tt-promocoes.cdn_funcionario  = bfuncionario.cdn_funcionario
                           tt-promocoes.dat_inic         = histor_sal_func.dat_liber_sal
                           tt-promocoes.cdn_cargo_basic  = histor_sal_func.cdn_cargo_basic
                           tt-promocoes.mes              = month(histor_sal_func.dat_liber_sal)
                           tt-promocoes.ano              = year(histor_sal_func.dat_liber_sal)
                            .
        
                    ASSIGN tt-promocoes.tipo             = 1.
                END.
                ELSE DO:
                    
                    create tt-promocoes.
                    assign tt-promocoes.cdn_empresa      = bfuncionario.cdn_empresa    
                           tt-promocoes.cdn_estab        = bfuncionario.cdn_estab      
                           tt-promocoes.cdn_funcionario  = bfuncionario.cdn_funcionario
                           tt-promocoes.dat_inic         = histor_sal_func.dat_liber_sal
                           tt-promocoes.cdn_cargo_basic  = histor_sal_func.cdn_cargo_basic
                           tt-promocoes.mes              = month(histor_sal_func.dat_liber_sal)
                           tt-promocoes.ano              = year(histor_sal_func.dat_liber_sal)
                            .
        
                    ASSIGN tt-promocoes.tipo             = 3.

                END.  
            end.
            
            assign v_cdn_cargo_ant   = ext_niv_hier_funcnal.cdn_tip_grupo_cargo
                   v_cdn_niv_ant     = histor_sal_func.cdn_niv_cargo
                   v_val_salario_ant = histor_sal_func.val_salario_mensal.
        
    end.

end procedure.

procedure pi-busca-corporativos :

    def input param p_data_lim as date no-undo.

    def buffer btt-ocorrencias for tt-ocorrencias.

    def var v_data_ini as date no-undo.
                  
    assign v_data_ini = date(1, 1, tt-param.tt_num_ano_refer_calc_efetd).

    for each tt-funcionarios no-lock,
        each tip_func_corporativo no-lock
        where tip_func_corporativo.cdn_empresa     = tt-funcionarios.cdn_empresa    
          and tip_func_corporativo.cdn_estab       = tt-funcionarios.cdn_estab      
          and tip_func_corporativo.cdn_funcionario = tt-funcionarios.cdn_funcionario
          and tip_func_corporativo.dt_inicio_corp < p_data_lim
          and tip_func_corporativo.dt_fim_corp >= v_data_ini :

        create tt-ocorrencias.
        assign tt-ocorrencias.cdn_empresa         = tip_func_corporativo.cdn_empresa    
               tt-ocorrencias.cdn_estab           = tip_func_corporativo.cdn_estab      
               tt-ocorrencias.cdn_funcionario     = tip_func_corporativo.cdn_funcionario
               tt-ocorrencias.dat_inic_lotac_func = tip_func_corporativo.dt_inicio_corp
               tt-ocorrencias.dat_fim_lotac_func  = tip_func_corporativo.dt_fim_corp   
               tt-ocorrencias.mes_processo        = if year(tip_func_corporativo.dt_inicio_corp) = tt-param.tt_num_ano_refer_calc_efetd
                                                     then month(tip_func_corporativo.dt_inicio_corp)
                                                     else 1
               tt-ocorrencias.tipo                = {&CORP-ENTRADA}
               tt-ocorrencias.observacoes         = fn_observacao("Corporativo iniciado em &1 de &2. ", 
                                                                  tip_func_corporativo.dt_inicio_corp). 

        if tip_func_corporativo.dt_fim_corp < p_data_lim then do:

            create btt-ocorrencias.
            buffer-copy tt-ocorrencias except mes_processo tipo observacoes to btt-ocorrencias.

            assign btt-ocorrencias.mes_processo = month(tip_func_corporativo.dt_fim_corp)
                   btt-ocorrencias.tipo         = {&CORP-SAIDA}
                   btt-ocorrencias.observacoes  = fn_observacao("Corporativo finalizado em &1 de &2. ", 
                                                                tip_func_corporativo.dt_fim_corp). 

        end.

    end.

end procedure.

procedure pi-busca-desempenho :

    def input param p_data_lim as date no-undo.
    def output param p_desempenho as char no-undo.
    def output param p_fator as dec no-undo.

    if tt-param.tt_individual_factor then do:

        if tt-param.tt_ind_tip_exporta = 1 then do:

            find last tip_func_indiv_factor_prov no-lock
                where tip_func_indiv_factor_prov.cdn_empresa     = tt-tip-cash-awards.cdn_empresa    
                  and tip_func_indiv_factor_prov.cdn_estab       = tt-tip-cash-awards.cdn_estab      
                  and tip_func_indiv_factor_prov.cdn_funcionario = tt-tip-cash-awards.cdn_funcionario
                  and tip_func_indiv_factor_prov.log_corporativo = tt-tip-cash-awards.log_corporativo
                  and tip_func_indiv_factor_prov.dt_inicio       < p_data_lim no-error.

            if avail tip_func_indiv_factor_prov then do:

                find first tiph_func_indiv_factor_prov of tip_func_indiv_factor_prov no-lock no-error.

                assign p_desempenho = substitute("&1 - &2",                        
                                                 tip_func_indiv_factor_prov.cdn_indiv_factor, 
                                                 tiph_func_indiv_factor_prov.desc_indiv_factor)

                       p_fator      = tip_func_indiv_factor_prov.indice. 

                return.

            end.

        end.
        else do:

            find last tip_func_indiv_factor_pag no-lock
                where tip_func_indiv_factor_pag.cdn_empresa     = tt-tip-cash-awards.cdn_empresa    
                  and tip_func_indiv_factor_pag.cdn_estab       = tt-tip-cash-awards.cdn_estab      
                  and tip_func_indiv_factor_pag.cdn_funcionario = tt-tip-cash-awards.cdn_funcionario
                  and tip_func_indiv_factor_pag.log_corporativo = tt-tip-cash-awards.log_corporativo
                  and tip_func_indiv_factor_pag.dt_inicio       < p_data_lim 
                 
                no-error.

            if avail tip_func_indiv_factor_pag then do:

                find first tiph_func_indiv_factor_pag of tip_func_indiv_factor_pag no-lock no-error.

                assign p_desempenho = substitute("&1 - &2",                        
                                                 tip_func_indiv_factor_pag.cdn_indiv_factor, 
                                                 tiph_func_indiv_factor_pag.desc_indiv_factor)

                       p_fator      = tip_func_indiv_factor_pag.indice. 

                return.

            end.

        end.

    end.
    ELSE DO:

        for LAST tip_indiv_factor no-lock
            where tip_indiv_factor.dt_termino >= p_data_lim
              AND tip_indiv_factor.dt_inicio <= p_data_lim
              AND tip_indiv_factor.cdn_indiv_factor = tt-param.v_cdn_individual_factor
              AND tip_indiv_factor.cdn_empresa      = tt-tip-cash-awards.cdn_empresa                   
              AND tip_indiv_factor.cdn_estab        = tt-tip-cash-awards.cdn_estab                     
              AND tip_indiv_factor.log_corporativo  = tt-tip-cash-awards.log_corporativo   :

                assign p_desempenho = substitute("&1 - &2",                        
                                                 tip_indiv_factor.cdn_indiv_factor, 
                                                 tip_indiv_factor.desc_indiv_factor)
        
                       p_fator      = tip_indiv_factor.indice. 
        
                return.
        END.
        
    END.

    

end procedure.

procedure pi-verifica-cancelamento :

    def var v_estado as char no-undo.

    run pi-retorna-status in h-acomp (output v_estado).

    if v_estado = "NOK" then do:

        run pi-finalizar in h-acomp.

        &scoped-define NL chr(10)
        &scoped-define MSG "Processo abortado pelo usu rio"
        &scoped-define HLP "Aten‡Æo:" + {&NL} + {&NL} + ~
                           "Como a gera‡Æo da planilha foi interrompida neste ponto a planilha gerada" + {&NL} + ~
                           "at‚ esse momento est  com valores incompletos"

        put {&MSG} format "x(120)" skip(1)
            {&HLP} format "x(350)".

        {include/i-rpclo.i}

        if i-num-ped-exec-rpw = 0 then do:

            run utp/ut-msgs.p (input "show":U, input 15825, input {&MSG} + "~~" + {&HLP}).

        end.

    end.

    return v_estado.

end procedure.

function fn_ultimo_dia_mes returns int (input p_mes as int) :

    def var v_data as date no-undo.

    if substr(param_calc_ppr.cod_livre_1,046,001) = "1" then do:
        assign v_data = date(if p_mes < 12 then p_mes + 1 else 1, 1,
                             if p_mes < 12 then tt-param.tt_num_ano_refer_calc_efetd
                                           else tt-param.tt_num_ano_refer_calc_efetd + 1) - 1.

        return day(v_data).
    end.
    else return 30.

end function.

function fn_dias_trabalhados return int (input p_data as date) :
    DEF VAR v_ult_dia_mes AS INT NO-UNDO.

    ASSIGN v_ult_dia_mes = fn_ultimo_dia_mes(input month(p_data)).

    IF v_ult_dia_mes <= 30 THEN DO:
        RETURN (30 -  day(p_data)).
    END.
    ELSE DO:
        RETURN (30 -  day(p_data)).

    END.
end function.

/*
function fn_dias_trabalhados return int (input p_data as date) :
   
    return fn_ultimo_dia_mes(input month(p_data)) - day(p_data) + 1.

end function.
*/

function fn_atualiza_dados_afastamentos returns logical (input p_cdn_empresa as char,
                                                         input p_cdn_estab as char,
                                                         input p_mes as int, 
                                                         input p_num_dias as int) :

    find first tt-afastamentos
        where tt-afastamentos.cdn_empresa = p_cdn_empresa
          and tt-afastamentos.cdn_estab   = p_cdn_estab
          and tt-afastamentos.mes         = p_mes no-error.

    if not avail tt-afastamentos then do:
        create tt-afastamentos.
        assign tt-afastamentos.cdn_empresa = p_cdn_empresa  
               tt-afastamentos.cdn_estab   = p_cdn_estab    
               tt-afastamentos.mes         = p_mes no-error.
    end.

    assign tt-afastamentos.num_dias_afastado = tt-afastamentos.num_dias_afastado + p_num_dias.

end function.

function fn_observacao returns char (input p_texto as char, input p_data as date) :

    def var v_mes as int no-undo.

    assign v_mes = month(p_data).

    return substitute(p_texto, day(p_data), fn_nome_mes(v_mes)).

end function.

function fn_proporcao_com_aviso returns int (buffer bfuncionario for funcionario,
                                             input p_proporcao as int) :

    def var v_mes as int no-undo.
    def var v_mes_fim as int no-undo.
    def var v_proporcao as int no-undo.

    assign v_proporcao = p_proporcao.

    find first habilit_rescis of bfuncionario no-lock no-error.

    if year(habilit_rescis.dat_term_aviso_previo) = year(bfuncionario.dat_desligto_func)
    then do:
        assign v_mes_fim = month(habilit_rescis.dat_term_aviso_previo).
        if day(habilit_rescis.dat_term_aviso_previo) < 15 then
            assign v_mes_fim = v_mes_fim - 1.

    end.
    else assign v_mes_fim = 12.

    do v_mes = month(bfuncionario.dat_desligto_func) to v_mes_fim :

        if v_proporcao < 12 then
            assign v_proporcao = v_proporcao + 1.
        else leave.

    end.

    return v_proporcao.

end function.



procedure pi-forma-tip-cash-awards-retro :

    def output param p_data_limite as date no-undo. 

    def buffer bfuncionario for funcionario.

    def var v_retroativo as logical no-undo.
    def var v_data_limite as date no-undo.
    def var v_data_inicio as date no-undo.
    def var v_cancelado as logical no-undo.
    def var v_cdn_empresa like funcionario.cdn_empresa no-undo.
    def var v_cdn_estab like funcionario.cdn_estab no-undo.
    def var v_cdn_funcionario like funcionario.cdn_funcionario no-undo.
    DEF VAR v_data_ini_param AS DATE NO-UNDO.
    DEF VAR v_data_fim_param AS DATE NO-UNDO.
    DEF VAR v_prox           AS LOG NO-UNDO.

    run utp/ut-acomp.p persistent set h-acomp.

    run pi-inicializar in h-acomp (input "Buscando Informa‡äes").

    assign v_data_limite = date(if tt-param.tt_num_mes_refer_calc_efetd < 12 
                                then tt-param.tt_num_mes_refer_calc_efetd + 1 else 1, 1,
                                if tt-param.tt_num_mes_refer_calc_efetd < 12 
                                then tt-param.tt_num_ano_refer_calc_efetd
                                else tt-param.tt_num_ano_refer_calc_efetd + 1)
           v_data_inicio = date(tt-param.tt_num_mes_refer_calc_efetd, 1, 
                                tt-param.tt_num_ano_refer_calc_efetd).

    assign v_retroativo = tt-param.tt_num_mes_refer_calc_efetd < param_empres_rh.num_mes_refer_calc_efetd
                          and tt-param.tt_num_ano_refer_calc_efetd = param_empres_rh.num_ano_refer_calc_efetd
                          or tt-param.tt_num_ano_refer_calc_efetd < param_empres_rh.num_ano_refer_calc_efetd.

    ASSIGN v_data_ini_param = date(param_calc_ppr.num_mes_inic_vigenc,01,param_calc_ppr.num_ano_inic_vigenc). 

    for each sit_afast no-lock :
        create tt-situacoes.
        assign tt-situacoes.cdn_sit_afast_func = sit_afast.cdn_sit_afast_func
               tt-situacoes.influi_plr         = substr(sit_afast.cod_livre_1,004,001) = "1".
    end.

    FOR EACH funcionario
       WHERE funcionario.cod_id_feder >= tt-param.v_cod_id_feder_ini
         AND funcionario.cod_id_feder <= tt-param.v_cod_id_feder_fim
         AND funcionario.cdn_empresa      = tt-param.v_cdn_empres_usuar
         AND funcionario.dat_admis_func   < v_data_limite
         and funcionario.log_consid_calc_ppr = YES
         
           on stop undo, leave 
        BY funcionario.cod_id_feder
        BY funcionario.dat_admis_transf_func :

        ASSIGN v_prox = NO.

        IF tt-param.tp_func = 1 AND 
            (funcionario.dat_desligto_func <> ? AND
             funcionario.dat_desligto_func < v_data_inicio) THEN
            NEXT.

        
        IF tt-param.tp_func = 1 AND 
           (funcionario.dat_desligto_func <> ? AND
            MONTH(funcionario.dat_desligto_func) = MONTH(v_data_inicio)) THEN DO:
            IF DAY(funcionario.dat_desligto_func) < 15 THEN
                NEXT.
        END.
        

        IF tt-param.tp_func = 1 THEN DO:

            for each sit_afast_func of funcionario no-lock
               where sit_afast_func.cdn_func_orig > 0
                 and sit_afast_func.dat_inic_sit_afast >= v_data_limite
                 AND sit_afast_func.cdn_empres_orig = funcionario.cdn_empresa
                 /*AND sit_afast_func.dat_term_sit_afast >  v_data_ini_param*/ ,
               first sit_afast of sit_afast_func no-lock
               where sit_afast.idi_signif_sit = 3 :

                ASSIGN v_prox = YES.
            END.

            for each sit_afast_func of funcionario no-lock
               where sit_afast_func.cdn_func_dest > 0
                 and sit_afast_func.dat_inic_sit_afast >= v_data_limite
                 AND sit_afast_func.cdn_empres_dest <> funcionario.cdn_empresa
                 /*AND sit_afast_func.dat_term_sit_afast >  v_data_ini_param*/ ,
               first sit_afast of sit_afast_func no-lock
               where sit_afast.idi_signif_sit = 4 :
                
                ASSIGN v_prox = YES.
            END.

            IF v_prox = YES THEN
                NEXT.
            
        END.

        IF tt-param.tp_func = 2 THEN do:

            IF (funcionario.dat_desligto_func = ?) THEN
                NEXT.

            IF NOT CAN-FIND(FIRST habilit_rescis of funcionario NO-LOCK where
                                  habilit_rescis.dat_desligto_func  >= v_data_ini_param and
                                  habilit_rescis.dat_desligto_func  < v_data_limite ) THEN
                NEXT.
           
        END.

        IF tt-param.tp_func = 3 THEN DO:


            IF funcionario.dat_desligto_func <> ? THEN DO:
                
                IF funcionario.dat_desligto_func < v_data_ini_param THEN
                    NEXT.
            END.
            
            IF v_prox = YES THEN
                NEXT.
        END.

            
         
        assign v_cancelado = true.

        run pi-acompanhar in h-acomp (input funcionario.cdn_funcionario).

        run pi-busca-entradas (buffer funcionario, input (v_data_limite)).

        find bfuncionario no-lock
              where rowid(bfuncionario) = rowid(funcionario) no-error.

        run pi-cria-tip-cash-funcionario (buffer bfuncionario, input v_data_limite).

        assign v_cancelado = false.

    end.

    if v_cancelado = false then do:
        run pi-finaliza-tip-cash-awards (input v_data_limite).

        assign p_data_limite = v_data_limite.

    end.

    else os-delete value(v_arquivo_saida).

    run pi-finalizar in h-acomp.

    return string(v_cancelado, "NOK/OK").

end procedure.

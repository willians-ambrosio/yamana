/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/buffers_RH.i}

{include/i-prgvrs.i YMCA0001RP 1.02.00.000}  /*** 010000 ***/

/********************************************************************************************************************************************
**       Programa: YMCA0001rp.p
**       Data....: 2008
**       Autor...: Roberto Fernandes - DATASUL S.A.
**       Objetivo: Realizar Importa‡Æo de dados referente a cargos para realizar um DE x PARA
*********************************************************************************************************************************************/

/* ---------------------------------- Defini‡äes -------------------------------- */
/* ---------------------- Temp-Tables utilizadas pelo programa ------------------ */
{esp/ymca0001tt.i}

/* ----------------------------------- Includes ----------------------------------*/
/* --- Global --- */
{utp/ut-glob.i}
{include/i-rpvar.i}

/* Inicializa‡Æo da Temp-table tt-param carregada */
define temp-table tt-raw-digita field raw-digita as raw.

define input parameter raw-param as raw no-undo.
define input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

define stream str-imp.
define stream str-rp.

define buffer b_cargo               for cargo.
define buffer b_cargo_basic         for cargo_basic.
define buffer b_cargo_basic_descr   for cargo_basic_descr.
define buffer b_cargo_basic_familia for cargo_basic_familia.
define buffer b_competit_regiao_sal for competit_regiao_sal.
define buffer b_lin_aces_cargo      for lin_aces_cargo.
define buffer b_cargo_habcomp       for cargo_habcomp.
define buffer b_cargo_conhecto      for cargo_conhecto.
define buffer b_cargo_descr         for cargo_descr.
define buffer b_cargo_avaliac_descr for cargo_avaliac_descr.
define buffer b_histor_sal_func     for histor_sal_func.

define variable v_des_dados as character no-undo.
define variable v_han_acomp as handle    no-undo.
define variable v_des_sit   as character no-undo format "x(30)".

form
    funcionario.cdn_empresa
    funcionario.cdn_estab
    funcionario.cdn_funcionario
    cargo.cdn_cargo_basic        column-label "Cargo Orig"
    cargo.cdn_niv_cargo          column-label "Niv Orig"
    b_cargo.cdn_cargo_basic      column-label "Cargo Dest"
    b_cargo.cdn_niv_cargo        column-label "Niv Dest"
    v_des_sit                    column-label "Situa‡Æo" format "x(30)"
    with stream-io no-attr-space no-box width 132 down frame f-sit.

run utp/ut-trfrrp.p (input frame f-sit:handle).

{include/i-rpcab.i &stream="str-rp"}
{include/i-rpout.i &stream="stream str-rp"}

view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.

run pi-importa.

{include/i-rpclo.i &stream="stream str-rp"}
return "OK".

procedure pi-importa:

    run utp/ut-acomp.p persistent set v_han_acomp.

    find empresa  no-lock where empresa.ep-codigo = v_cdn_empres_usuar no-error.
         assign c-programa     = "YMCA0001"
                c-versao       = "0.00"
                c-revisao      = "01"
                c-empresa      = if avail empresa then empresa.nome else "".
                c-sistema      = "CARGOS E SALµRIOS".

    input stream str-imp from value(tt-param.arq-entrada).

    repeat:

        assign v_des_dados = "".
        import stream str-imp unformatted v_des_dados.

        run pi-inicializar in v_han_acomp(input "Carregando...").

        /* Busca cargo DE */
        find first cargo no-lock where
                   cargo.cdn_cargo_basic           = int(entry(1,v_des_dados,";")) and
                   cargo.cdn_niv_cargo             = int(entry(2,v_des_dados,";")) no-error.
        if avail cargo then do:

            /* Busca o cargo b sico DE */
            find first cargo_basic no-lock where
                       cargo_basic.cdn_cargo_basic = cargo.cdn_cargo_basic no-error.
            if avail cargo_basic then do:

                /* Verifica a existˆncia do cargo PARA */
                find first b_cargo no-lock where
                           b_cargo.cdn_cargo_basic = int(entry(3,v_des_dados,";")) and
                           b_cargo.cdn_niv_cargo   = int(entry(4,v_des_dados,";")) no-error.
                if not avail b_cargo then do:
    
                    /* Verifica a existˆncia do cargo b sico PARA */
                    find first b_cargo_basic no-lock where
                               b_cargo_basic.cdn_cargo_basic = int(entry(3,v_des_dados,";")) no-error.
                    if not avail b_cargo_basic then do:
    
                        /* Cria a tabela cargo b sico para o PARA */
                        create b_cargo_basic.
                        buffer-copy cargo_basic except cdn_cargo_basic des_cargo_basic to b_cargo_basic.
                        assign b_cargo_basic.cdn_cargo_basic = int(entry(3,v_des_dados,";"))
                               b_cargo_basic.des_cargo_basic = if num-entries(v_des_dados,";") > 4 then entry(5,v_des_dados,";") else cargo_basic.des_cargo_basic.

                        RELEASE b_cargo_basic.

                        FIND FIRST b_cargo_basic NO-LOCK WHERE
                                   b_cargo_basic.cdn_cargo_basic = int(entry(3,v_des_dados,";")) no-error.
                        IF AVAIL b_cargo_basic THEN DO:

                            /* Cria descri‡äes filhas do PARA */
                            for each cargo_basic_descr no-lock where
                                     cargo_basic_descr.cdn_cargo_basic = cargo_basic.cdn_cargo_basic:
    
                                create b_cargo_basic_descr.
                                buffer-copy cargo_basic_descr except cdn_cargo_basic to b_cargo_basic_descr.
                                assign b_cargo_basic_descr.cdn_cargo_basic = b_cargo_basic.cdn_cargo_basic.
    
                            end.
    
                            /* Cria fam¡lias filhas do PARA */
                            for each cargo_basic_familia no-lock where
                                     cargo_basic_familia.cdn_cargo_basic = cargo_basic.cdn_cargo_basic:
    
                                create b_cargo_basic_familia.
                                buffer-copy cargo_basic_familia except cdn_cargo_basic to b_cargo_basic_familia.
                                assign b_cargo_basic_familia.cdn_cargo_basic = b_cargo_basic.cdn_cargo_basic.
    
                            end.
                        END.
                    end.

                    /* Cria o cargo PARA */
                    create b_cargo.
                    buffer-copy cargo except cdn_cargo_basic cdn_niv_cargo des_cargo to b_cargo.
                    assign b_cargo.cdn_cargo_basic = int(entry(3,v_des_dados,";"))
                           b_cargo.cdn_niv_cargo   = int(entry(4,v_des_dados,";"))
                           b_cargo.des_cargo       = if num-entries(v_des_dados,";") > 4 then entry(5,v_des_dados,";") else cargo.des_cargo.

                    RELEASE b_cargo.

                    FIND FIRST b_cargo NO-LOCK WHERE
                               b_cargo.cdn_cargo_basic = int(entry(3,v_des_dados,";")) AND
                               b_cargo.cdn_niv_cargo   = int(entry(4,v_des_dados,";")) NO-ERROR.
                    IF AVAIL b_cargo THEN do:

                        /* Cria a competitividade PARA */
                        for each competit_regiao_sal no-lock where
                                 competit_regiao_sal.cdn_cargo_basic = cargo.cdn_cargo_basic and
                                 competit_regiao_sal.cdn_niv_cargo   = cargo.cdn_niv_cargo:
    
                            create b_competit_regiao_sal.
                            buffer-copy competit_regiao_sal except cdn_cargo_basic cdn_niv_cargo to b_competit_regiao_sal.
                            assign b_competit_regiao_sal.cdn_cargo_basic = b_cargo.cdn_cargo_basic
                                   b_competit_regiao_sal.cdn_niv_cargo   = b_cargo.cdn_niv_cargo.
    
                        end.
    
                        /* Cria a linha de acesso PARA */
                        for each lin_aces_cargo no-lock where
                                 lin_aces_cargo.cdn_cargo_basic_orig = cargo.cdn_cargo_basic and
                                 lin_aces_cargo.cdn_niv_cargo_orig   = cargo.cdn_niv_cargo:
    
                            create b_lin_aces_cargo.
                            buffer-copy lin_aces_cargo except cdn_cargo_basic_orig cdn_niv_cargo_orig to b_lin_aces_cargo.
                            assign b_lin_aces_cargo.cdn_cargo_basic_orig = b_cargo.cdn_cargo_basic
                                   b_lin_aces_cargo.cdn_niv_cargo_orig   = b_cargo.cdn_niv_cargo.
    
                        end.
    
                        /* Cria a habilidade PARA */
                        for each cargo_habcomp no-lock where
                                 cargo_habcomp.cdn_cargo_basic = cargo.cdn_cargo_basic and
                                 cargo_habcomp.cdn_niv_cargo   = cargo.cdn_niv_cargo:
    
                            create b_cargo_habcomp.
                            buffer-copy cargo_habcomp except cdn_cargo_basic cdn_niv_cargo to b_cargo_habcomp.
                            assign b_cargo_habcomp.cdn_cargo_basic = b_cargo.cdn_cargo_basic
                                   b_cargo_habcomp.cdn_niv_cargo   = b_cargo.cdn_niv_cargo.
    
                        end.
    
                        /* Cria o conhecimento PARA */
                        for each cargo_conhecto no-lock where
                                 cargo_conhecto.cdn_cargo_basic = cargo.cdn_cargo_basic and
                                 cargo_conhecto.cdn_niv_cargo   = cargo.cdn_niv_cargo:
    
                            create b_cargo_conhecto.
                            buffer-copy cargo_conhecto except cdn_cargo_basic cdn_niv_cargo to b_cargo_conhecto.
                            assign b_cargo_conhecto.cdn_cargo_basic = b_cargo.cdn_cargo_basic
                                   b_cargo_conhecto.cdn_niv_cargo   = b_cargo.cdn_niv_cargo.
    
                        end.
    
                        /* Cria a descri‡Æo PARA */
                        for each cargo_descr no-lock where
                                 cargo_descr.cdn_cargo_basic = cargo.cdn_cargo_basic and
                                 cargo_descr.cdn_niv_cargo   = cargo.cdn_niv_cargo:
    
                            create b_cargo_descr.
                            buffer-copy cargo_descr except cdn_cargo_basic cdn_niv_cargo to b_cargo_descr.
                            assign b_cargo_descr.cdn_cargo_basic = b_cargo.cdn_cargo_basic
                                   b_cargo_descr.cdn_niv_cargo   = b_cargo.cdn_niv_cargo.
    
                        end.
    
                        /* Cria a avalia‡Æo PARA */
                        for each cargo_avaliac_descr no-lock where
                                 cargo_avaliac_descr.cdn_cargo_basic = cargo.cdn_cargo_basic and
                                 cargo_avaliac_descr.cdn_niv_cargo   = cargo.cdn_niv_cargo:
    
                            create b_cargo_avaliac_descr.
                            buffer-copy cargo_avaliac_descr except cdn_cargo_basic cdn_niv_cargo to b_cargo_avaliac_descr.
                            assign b_cargo_avaliac_descr.cdn_cargo_basic = b_cargo.cdn_cargo_basic
                                   b_cargo_avaliac_descr.cdn_niv_cargo   = b_cargo.cdn_niv_cargo.
    
                        end.
                    END.
                end.

                release b_cargo.

                find first b_cargo no-lock where
                           b_cargo.cdn_cargo_basic = int(entry(3,v_des_dados,";")) and
                           b_cargo.cdn_niv_cargo   = int(entry(4,v_des_dados,";")) no-error.

                /* ----- Atualiza o valor do funcionario de acordo com o cargo novo que ele ocupara ----- */
                for each funcionario exclusive-lock where
                         funcionario.cdn_empresa              >= tt-param.cdn_empresa_ini   and
                         funcionario.cdn_empresa              <= tt-param.cdn_empresa_fim   and
                         funcionario.cdn_estab                >= tt-param.cdn_estab_ini     and
                         funcionario.cdn_estab                <= tt-param.cdn_estab_fim     and
                         funcionario.cod_rh_ccusto            >= tt-param.cod_rh_ccusto_ini and
                         funcionario.cod_rh_ccusto            <= tt-param.cod_rh_ccusto_fim and
                         funcionario.cdn_cargo_basic           = cargo.cdn_cargo_basic      and
                         funcionario.cdn_niv_cargo             = cargo.cdn_niv_cargo        and
                         funcionario.dat_desligto              = ?:

                    if not can-find(first histor_sal_func where
                                          histor_sal_func.cdn_empresa         = funcionario.cdn_empresa and
                                          histor_sal_func.cdn_estab           = funcionario.cdn_estab   and
                                          histor_sal_func.cdn_funcionario     = funcionario.cdn_funcionario and
                                          histor_sal_func.dat_liber_sal       = tt-param.dat_liber_sal) then do:

                        disable triggers for load of funcionario.
                        disable triggers for load of histor_sal_func.

                        find last b_histor_sal_func no-lock where
                                  b_histor_sal_func.cdn_empresa     = funcionario.cdn_empresa and
                                  b_histor_sal_func.cdn_estab       = funcionario.cdn_estab   and
                                  b_histor_sal_func.cdn_funcionario = funcionario.cdn_funcionario no-error.

                        create histor_sal_func.
                        assign histor_sal_func.cdn_empresa         = funcionario.cdn_empresa
                               histor_sal_func.cdn_estab           = funcionario.cdn_estab
                               histor_sal_func.cdn_funcionario     = funcionario.cdn_funcionario
                               histor_sal_func.dat_liber_sal       = tt-param.dat_liber_sal
                               histor_sal_func.dat_livre_1         = tt-param.dat_liber_sal
                               histor_sal_func.cdn_cargo_basic     = b_cargo.cdn_cargo_basic
                               histor_sal_func.cdn_niv_cargo       = b_cargo.cdn_niv_cargo
                               histor_sal_func.cdn_motiv_liber_sal = 76
                               histor_sal_func.num_seq_histor_sal  = 1
                               funcionario.cdn_cargo_basic         = b_cargo.cdn_cargo_basic
                               funcionario.cdn_niv_cargo           = b_cargo.cdn_niv_cargo
                               v_des_sit                           = "Alterado com sucesso".

                        if avail b_histor_sal_func then do:
                            assign histor_sal_func.val_salario_categ  = b_histor_sal_func.val_salario_categ
                                   histor_sal_func.val_salario_mensal = b_histor_sal_func.val_salario_mensal
                                   histor_sal_func.val_salario_hora   = b_histor_sal_func.val_salario_hora.
                        end.

                        find first turno_trab no-lock where
                                   turno_trab.cdn_turno_trab = funcionario.cdn_turno_trab no-error.
                        if avail turno_trab then do:
                            if (funcionario.cdn_categ_sal = 2 or 
                                funcionario.cdn_categ_sal = 5) then
                                assign histor_sal_func.qtd_hrs_categ_sal = 1.
                             else if funcionario.cdn_categ_sal = 1 then 
                                     assign histor_sal_func.qtd_hrs_categ_sal = turno_trab.qtd_hrs_padr_mes_rh.
                                  else if funcionario.cdn_categ_sal = 6 then
                                          assign histor_sal_func.qtd_hrs_categ_sal = turno_trab.qtd_hrs_padr_dia_rh.
                                       else if funcionario.cdn_categ_sal = 3 then
                                               assign  histor_sal_func.qtd_hrs_categ_sal = turno_trab.qtd_hrs_padr_sema_rh.
                                            else assign  histor_sal_func.qtd_hrs_categ_sal = turno_trab.qtd_hrs_padr_quinz_rh.

                        end.

                        assign histor_sal_func.qtd_hrs_padr_mes_rh = turno_trab.qtd_hrs_padr_mes_rh.

                        release histor_sal_func.

                        on find of funcionario     revert.
                        on find of histor_sal_func revert.

                        disp stream str-rp funcionario.cdn_empresa
                             funcionario.cdn_estab
                             funcionario.cdn_funcionario
                             cargo.cdn_Cargo_basic
                             cargo.cdn_niv_cargo
                             b_cargo.cdn_cargo_basic
                             b_cargo.cdn_niv_cargo
                             v_des_sit
                            with frame f-sit.
                        down stream str-rp with frame f-sit.

                    end.
                    else do:
                        assign v_des_sit = "Registro j  existe".
                        disp stream str-rp funcionario.cdn_empresa
                             funcionario.cdn_estab
                             funcionario.cdn_funcionario
                             cargo.cdn_Cargo_basic
                             cargo.cdn_niv_cargo
                             b_cargo.cdn_cargo_basic
                             b_cargo.cdn_niv_cargo
                             v_des_sit
                            with frame f-sit.
                        down stream str-rp with frame f-sit.
                    end.
                end.
            end.
        end.
    end.

    input stream str-imp close.
    run pi-finalizar in v_han_acomp.

end procedure.

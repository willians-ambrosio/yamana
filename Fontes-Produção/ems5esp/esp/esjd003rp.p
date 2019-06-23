/*****************************************************************************
** Programa..............: esjd003rp
** Descricao.............: Valida‡Æo Mapa EMS5 X JDE
** Versao................: 1.00.00.000
** Procedimento..........: 
** Nome Externo..........: 
** Criado por............: Bruno Bertulli (DSC)
** Criado em.............: 17/10/2014
*****************************************************************************/
def new global shared var v_cod_usuar_corren    like usuar_mestre.cod_usuario   no-undo.

DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.

DEFINE BUFFER bf-estabelecimento FOR estabelecimento.

DEFINE TEMP-TABLE tt-param
    FIELD cod_cta_ctbl-ini AS CHARACTER
    FIELD cod_cta_ctbl-fim AS CHARACTER
    FIELD arquivo          AS CHARACTER
    FIELD saida            AS CHARACTER.

/* ===> Main Block <=== */

FIND FIRST dwb_rpt_param EXCLUSIVE-LOCK
     WHERE dwb_rpt_param.cod_dwb_program = "esjd003rp"
     AND   dwb_rpt_param.cod_dwb_user    = v_cod_usuar_corren No-error.
IF AVAILABLE dwb_rpt_param THEN DO:
    CREATE tt-param.
    ASSIGN tt-param.cod_cta_ctbl-ini = ENTRY(1,dwb_rpt_param.cod_dwb_parameters,";")
           tt-param.cod_cta_ctbl-fim = ENTRY(2,dwb_rpt_param.cod_dwb_parameters,";")
           tt-param.arquivo          = ENTRY(3,dwb_rpt_param.cod_dwb_parameters,";")
           tt-param.saida            = dwb_rpt_param.ind_dwb_run_mode.
END.

FIND FIRST tt-param NO-LOCK NO-ERROR.
IF  AVAIL tt-param THEN
    RUN pi-executar.

RETURN "OK".

/* ===> Procedures <=== */

PROCEDURE pi-executar:

    def var ch-Excel            as component-handle                     no-undo.

    run utp/ut-acomp.p persistent set h-acomp.

    run pi-inicializar in h-acomp (input "Valida Mapa EMS5 X JDE...").

    OUTPUT TO VALUE(tt-param.arquivo) NO-CONVERT.

    PUT UNFORMATTED "LOG DE ERRO MAPA EMS5 X JDE" SKIP(2).

    PUT UNFORMAT 
         "Conta"
         ";Inicio Validade Cta"
         ";Fim Validade Cta"
         ";Estab"
         ";Mapa"
         ";Inicio Validade Mapa"
         ";Fim Validade Mapa"
         ";CCusto"
         ";ERRO" SKIP.

    FOR EACH cta_ctbl NO-LOCK
        WHERE cta_ctbl.cod_plano_cta = "CONTSOC"
        AND   cta_ctbl.cod_cta_ctbl   >= tt-param.cod_cta_ctbl-ini
        AND   cta_ctbl.cod_cta_ctbl   <= tt-param.cod_cta_ctbl-fim
        AND   cta_ctbl.dat_inic_valid <= TODAY
        AND   cta_ctbl.dat_fim_valid  >= TODAY
        AND   cta_ctbl.log_cta_ctbl_exclus_analit = YES:

        FIND FIRST es-cross-reference-jde NO-LOCK
            WHERE es-cross-reference-jde.cod_cta_ctbl          = cta_ctbl.cod_cta_ctbl
            AND   es-cross-reference-jde.log-erro              = 0 NO-ERROR.
        IF NOT AVAILABLE es-cross-reference-jde THEN DO:
            PUT UNFORMAT 
                 cta_ctbl.cod_cta_ctbl
                 ";" cta_ctbl.dat_inic_valid
                 ";" cta_ctbl.dat_fim_valid
                 ";"
                 ";"
                 ";"
                 ";"
                 ";"
                 ";Conta nÆo encontrada no JDE" SKIP.
        END.

        IF cta_ctbl.cod_cta_ctbl >= '30000000' AND
           cta_ctbl.cod_cta_ctbl <= '79999999' THEN DO:
            FOR EACH estabelecimento NO-LOCK
                WHERE estabelecimento.cod_empresa    <> "CAN":

                RUN pi-acompanhar IN h-acomp (INPUT 'Processando mapa: ' + estabelecimento.cod_empresa + "-" + cta_ctbl.cod_cta_ctbl).

                FIND LAST mapa_distrib_ccusto NO-LOCK
                    WHERE mapa_distrib_ccusto.cod_estab               = estabelecimento.cod_estab
                    AND   mapa_distrib_ccusto.cod_mapa_distrib_ccusto = cta_ctbl.cod_cta_ctbl NO-ERROR.
                IF AVAILABLE mapa_distrib_ccusto THEN DO:
                    FOR EACH item_lista_ccusto EXCLUSIVE-LOCK
                        WHERE item_lista_ccusto.cod_empresa             = mapa_distrib_ccusto.cod_empresa
                        AND   item_lista_ccusto.cod_estab               = mapa_distrib_ccusto.cod_estab
                        AND   item_lista_ccusto.cod_mapa_distrib_ccusto = mapa_distrib_ccusto.cod_mapa_distrib_ccusto
                        AND   item_lista_ccusto.cod_plano_ccusto        = mapa_distrib_ccusto.cod_plano_ccusto:

                        /* Busca sempre o estab principal no JDE, devido a regra de exce‡Æo de CCusto */
                        FIND FIRST bf-estabelecimento NO-LOCK
                            WHERE bf-estabelecimento.cod_empresa     = estabelecimento.cod_empresa
                            AND   bf-estabelecimento.log_estab_princ = YES NO-ERROR.

                        FIND FIRST es-cross-reference-jde NO-LOCK
                            WHERE es-cross-reference-jde.Legacy-Company-Number = bf-estabelecimento.cod_estab
                            AND   es-cross-reference-jde.cod_cta_ctbl          = mapa_distrib_ccusto.cod_mapa_distrib_ccusto
                            AND   es-cross-reference-jde.cod_ccusto            = item_lista_ccusto.cod_ccusto 
                            AND   es-cross-reference-jde.log-erro              = 0 NO-ERROR.
                        IF NOT AVAILABLE es-cross-reference-jde THEN DO:
                            PUT UNFORMAT 
                                 cta_ctbl.cod_cta_ctbl
                                 ";" cta_ctbl.dat_inic_valid
                                 ";" cta_ctbl.dat_fim_valid
                                 ";" estabelecimento.cod_estab
                                 ";" mapa_distrib_ccusto.cod_mapa_distrib_ccusto
                                 ";" mapa_distrib_ccusto.dat_inic_valid
                                 ";" mapa_distrib_ccusto.dat_fim_valid
                                 ";" item_lista_ccusto.cod_ccusto
                                 ";NÆo encontrado no JDE" SKIP.
                        END.
                    END.
                END.
                ELSE DO:
                    PUT UNFORMAT 
                         cta_ctbl.cod_cta_ctbl
                         ";" cta_ctbl.dat_inic_valid
                         ";" cta_ctbl.dat_fim_valid
                         ";" estabelecimento.cod_estab
                         ";"
                         ";"
                         ";"
                         ";"
                         ";NÆo encontrado Mapa para a Conta." SKIP.
                END.
            END.
        END.
    END.

    OUTPUT CLOSE.

    run pi-finalizar in h-acomp. 

    IF tt-param.saida = "On-Line" THEN DO:
        Create "Excel.Application" ch-Excel no-error.

        ch-Excel:workbooks:open(tt-param.arquivo).
        ch-Excel:visible = TRUE.
        ch-Excel:application:DisplayAlerts = FALSE.
        ch-Excel:sheets:item(1).

        release object ch-Excel.
    END.

END PROCEDURE.


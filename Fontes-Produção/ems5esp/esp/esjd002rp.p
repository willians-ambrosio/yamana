/*****************************************************************************
** Programa..............: esjd002rp
** Descricao.............: Cria Mapa e Ajusta CritÇrio de Distribuiá∆o
** Versao................: 1.00.00.000
** Procedimento..........: 
** Nome Externo..........: 
** Criado por............: Bruno Bertulli (DSC)
** Criado em.............: 17/10/2014
*****************************************************************************/
def new global shared var v_cod_usuar_corren    like usuar_mestre.cod_usuario   no-undo.

DEFINE BUFFER bf-criter_distrib_cta_ctbl FOR criter_distrib_cta_ctbl.

DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.

def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.

def var v_cod_dwb_file
    as character
    format "x(40)":U
    label "Arquivo"
    column-label "Arquivo"
    no-undo.

DEFINE TEMP-TABLE tt-param
    FIELD cria-mapas       AS CHARACTER
    FIELD cod_cta_ctbl-ini AS CHARACTER
    FIELD cod_cta_ctbl-fim AS CHARACTER
    FIELD mapa             AS CHARACTER
    FIELD fim-validade     AS CHARACTER
    FIELD arquivo          AS CHARACTER
    FIELD saida            AS CHARACTER.

/* ===> Main Block <=== */

FIND FIRST dwb_rpt_param EXCLUSIVE-LOCK
     WHERE dwb_rpt_param.cod_dwb_program = "esjd002rp"
     AND   dwb_rpt_param.cod_dwb_user    = v_cod_usuar_corren No-error.
IF AVAILABLE dwb_rpt_param THEN DO:
    CREATE tt-param.
    ASSIGN tt-param.cria-mapas       = ENTRY(1,dwb_rpt_param.cod_dwb_parameters,";")
           tt-param.cod_cta_ctbl-ini = ENTRY(2,dwb_rpt_param.cod_dwb_parameters,";")
           tt-param.cod_cta_ctbl-fim = ENTRY(3,dwb_rpt_param.cod_dwb_parameters,";")
           tt-param.mapa             = ENTRY(4,dwb_rpt_param.cod_dwb_parameters,";")
           tt-param.fim-validade     = ENTRY(5,dwb_rpt_param.cod_dwb_parameters,";")
           tt-param.arquivo          = ENTRY(6,dwb_rpt_param.cod_dwb_parameters,";")
           tt-param.saida            = dwb_rpt_param.ind_dwb_run_mode.
END.

FIND FIRST tt-param NO-LOCK NO-ERROR.
IF  AVAIL tt-param THEN
    RUN pi-executar.

RETURN "OK".

/* ===> Procedures <=== */

PROCEDURE pi-executar :
    DISABLE TRIGGERS FOR LOAD OF criter_distrib_cta_ctbl.
    DISABLE TRIGGERS FOR DUMP OF criter_distrib_cta_ctbl.

    def var ch-Excel as component-handle no-undo.

    IF LOGICAL (tt-param.cria-mapas) THEN
        RUN esp/esjd002a.p (INPUT tt-param.cod_cta_ctbl-ini,
                            INPUT tt-param.cod_cta_ctbl-fim).

    run utp/ut-acomp.p persistent set h-acomp.

    run pi-inicializar in h-acomp(input "CritÇrio de Distribuiá∆o...").

    do on error undo, return error
       on stop  undo, return error:
        output to value(tt-param.arquivo) no-echo no-convert.

        PUT UNFORMAT "Empresa" +
                     ";Estab" +
                     ";Plano Contas" +
                     ";Conta Cont†bil" +
                     ";Cta data valid ini" +
                     ";Cta data valid fim" +
                     ";CritÇrio CCusto" +
                     ";Mapa Dist CCusto" +
                     ";Inic Validade" +
                     ";Fim Validade" +
                     ";Sequància" +
                     ";NOVO Fim Validade Mapa"
                     ";NOVO Mapa"
                     ";NOVA Dt.Ini.Val."
                     ";NOVA Dt.Fim.Val."
                     ";NOVA Seq."
                     SKIP.

        FOR EACH cta_ctbl NO-LOCK
            WHERE cta_ctbl.cod_plano_cta_ctbl = 'CONTSOC'
            AND   cta_ctbl.cod_cta_ctbl      >= '30000000'
            AND   cta_ctbl.cod_cta_ctbl      <= '79999999'
            AND   cta_ctbl.cod_cta_ctbl      >= tt-param.cod_cta_ctbl-ini
            AND   cta_ctbl.cod_cta_ctbl      <= tt-param.cod_cta_ctbl-fim
            AND   cta_ctbl.dat_inic_valid    <= TODAY
            AND   cta_ctbl.dat_fim_valid     >= TODAY
            AND   cta_ctbl.log_cta_ctbl_exclus_analit:
        /* Despresar faixa de contas que n∆o utiliza CCUSTO */
        IF  TODAY >= 11/01/2014 AND
            cta_ctbl.cod_cta_ctbl >= '54100000' AND
            cta_ctbl.cod_cta_ctbl <= '54299999' THEN NEXT.

            FOR EACH estabelecimento NO-LOCK
                WHERE estabelecimento.cod_empresa <> "CAN":
                RUN pi-acompanhar IN h-acomp(INPUT 'CritÇrio ' + estabelecimento.cod_empresa + " - Conta : " + cta_ctbl.cod_cta_ctbl).

                FIND LAST criter_distrib_cta_ctbl EXCLUSIVE-LOCK
                     where criter_distrib_cta_ctbl.cod_empresa        = estabelecimento.cod_empresa
                       AND criter_distrib_cta_ctbl.cod_plano_cta_ctbl = cta_ctbl.cod_plano_cta_ctbl
                       and criter_distrib_cta_ctbl.cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl
                       and criter_distrib_cta_ctbl.cod_estab          = estabelecimento.cod_estab NO-ERROR.
                IF AVAILABLE criter_distrib_cta_ctbl THEN DO:
                    /* !!! Troca o critÇrio somente quando o Mapa for diferente da conta !!! */
                    IF criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto <> cta_ctbl.cod_cta_ctbl AND
                       criter_distrib_cta_ctbl.ind_criter_distrib_ccusto <> "N∆o Utiliza" THEN DO:
                        PUT UNFORMATTED
                                 estabelecimento.cod_empresa
                             ";" estabelecimento.cod_estab
                             ";" cta_ctbl.cod_plano_cta_ctbl
                             ";" cta_ctbl.cod_cta_ctbl
                             ";" cta_ctbl.dat_inic_valid
                             ";" cta_ctbl.dat_fim_valid
                             ";" criter_distrib_cta_ctbl.ind_criter_distrib_ccusto
                             ";" criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto
                             ";" criter_distrib_cta_ctbl.dat_inic_valid
                             ";" criter_distrib_cta_ctbl.dat_fim_valid
                             ";" criter_distrib_cta_ctbl.num_seq_criter_distrib_ctbl.

                        PUT UNFORMATTED
                             ";" DATE(tt-param.fim-validade)
                             ";" cta_ctbl.cod_cta_ctbl
                             ";" DATE(tt-param.fim-validade) + 1
                             ";" DATE("31/12/9999")
                             ";" criter_distrib_cta_ctbl.num_seq_criter_distrib_ctbl + 10.

                        IF INTEGER (tt-param.mapa) = 2 THEN DO:
                            /* Fim do critÇrio */
                            ASSIGN criter_distrib_cta_ctbl.dat_fim_valid = DATE(tt-param.fim-validade).

                            RUN pi-cria-criterio(INPUT DATE(tt-param.fim-validade) + 1,
                                                 INPUT DATE("31/12/9999"),
                                                 INPUT criter_distrib_cta_ctbl.num_seq_criter_distrib_ctbl + 10,
                                                 INPUT cta_ctbl.cod_cta_ctbl).
                        END.
                    END.
                END.
                ELSE DO:
                    PUT UNFORMATTED
                             estabelecimento.cod_empresa
                         ";" estabelecimento.cod_estab
                         ";" cta_ctbl.cod_plano_cta_ctbl
                         ";" cta_ctbl.cod_cta_ctbl
                         ";" cta_ctbl.dat_inic_valid
                         ";" cta_ctbl.dat_fim_valid
                         ";" "N«O ENCONTRADO CRITêRIO DE DISTRIBUIÄ«O"
                         ";"
                         ";"
                         ";"
                         ";" .

                    IF INTEGER (tt-param.mapa) = 2 THEN DO:
                        RUN pi-cria-criterio(INPUT DATE(tt-param.fim-validade) + 1,
                                             INPUT DATE("31/12/9999"),
                                             INPUT 10,
                                             INPUT cta_ctbl.cod_cta_ctbl).
                    END.
                END.
                PUT UNFORMATTED SKIP.
            END.
        END.
        OUTPUT CLOSE.
    end.

    run pi-finalizar in h-acomp.

    IF tt-param.saida = "On-Line" THEN DO:
        Create "Excel.Application" ch-Excel no-error.

        ch-Excel:workbooks:open(tt-param.arquivo).
        ch-Excel:visible = YES.
        ch-Excel:application:DisplayAlerts = NO.
        ch-Excel:sheets:item(1).

        release object ch-Excel.
    END.
END PROCEDURE.

PROCEDURE pi-cria-criterio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAM p-dat_inic_valid LIKE criter_distrib_cta_ctbl.dat_inic_valid NO-UNDO.
    DEF INPUT PARAM p-dat_fim_valid  LIKE criter_distrib_cta_ctbl.dat_fim_valid  NO-UNDO.
    DEF INPUT PARAM p-seq            AS INT  NO-UNDO.
    DEF INPUT PARAM p-mapa           LIKE criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto NO-UNDO.

    DISABLE TRIGGERS FOR LOAD OF bf-criter_distrib_cta_ctbl.
    DISABLE TRIGGERS FOR DUMP OF bf-criter_distrib_cta_ctbl.

    CREATE bf-criter_distrib_cta_ctbl.
    ASSIGN bf-criter_distrib_cta_ctbl.cod_empresa                 = estabelecimento.cod_empresa
           bf-criter_distrib_cta_ctbl.cod_estab                   = estabelecimento.cod_estab
           bf-criter_distrib_cta_ctbl.cod_plano_cta_ctbl          = cta_ctbl.cod_plano_cta_ctbl
           bf-criter_distrib_cta_ctbl.cod_cta_ctbl                = cta_ctbl.cod_cta_ctbl
           bf-criter_distrib_cta_ctbl.num_seq_criter_distrib_ctbl = p-seq
           bf-criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto     = p-mapa
           bf-criter_distrib_cta_ctbl.ind_criter_distrib_ccusto   = "Definidos"
           bf-criter_distrib_cta_ctbl.dat_inic_valid              = p-dat_inic_valid
           bf-criter_distrib_cta_ctbl.dat_fim_valid               = p-dat_fim_valid.
END PROCEDURE.

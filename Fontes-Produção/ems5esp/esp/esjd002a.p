DEFINE INPUT PARAM p-cod_cta_ctbl-ini   LIKE cta_ctbl.cod_cta_ctbl NO-UNDO.
DEFINE INPUT PARAM p-cod_cta_ctbl-fim   LIKE cta_ctbl.cod_cta_ctbl NO-UNDO.

DEFINE VARIABLE h-acomp     AS HANDLE      NO-UNDO.
DEFINE VARIABLE cEmpresa    LIKE estabelecimento.cod_empresa NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF mapa_distrib_ccusto.
DISABLE TRIGGERS FOR DUMP OF mapa_distrib_ccusto.
DISABLE TRIGGERS FOR LOAD OF item_lista_ccusto.
DISABLE TRIGGERS FOR DUMP OF item_lista_ccusto.

run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp(input "Criando Mapas...").

FOR EACH cta_ctbl NO-LOCK
    WHERE cta_ctbl.cod_plano_cta   = "CONTSOC"
    AND   cta_ctbl.cod_cta_ctbl   >= '30000000'
    AND   cta_ctbl.cod_cta_ctbl   <= '79999999'
    AND   cta_ctbl.cod_cta_ctbl   >= p-cod_cta_ctbl-ini
    AND   cta_ctbl.cod_cta_ctbl   <= p-cod_cta_ctbl-fim
    AND   cta_ctbl.dat_inic_valid <= TODAY
    AND   cta_ctbl.dat_fim_valid  >= TODAY
    AND   cta_ctbl.log_cta_ctbl_exclus_analit:
    /* Despresar faixa de contas que nÆo utiliza CCUSTO */
    IF  TODAY >= 11/01/2014 AND
        cta_ctbl.cod_cta_ctbl >= '54100000' AND
        cta_ctbl.cod_cta_ctbl <= '54299999' THEN NEXT.

    RUN pi-acompanhar IN h-acomp(INPUT 'Criando mapa: ' + cta_ctbl.cod_cta_ctbl).

    FOR EACH estabelecimento NO-LOCK
        WHERE estabelecimento.cod_empresa <> "CAN":
        FIND FIRST mapa_distrib_ccusto NO-LOCK
            WHERE mapa_distrib_ccusto.cod_estab               = estabelecimento.cod_estab
            AND   mapa_distrib_ccusto.cod_mapa_distrib_ccusto = cta_ctbl.cod_cta_ctbl NO-ERROR.
        IF NOT AVAILABLE mapa_distrib_ccusto THEN DO:
            CREATE mapa_distrib_ccusto.
            ASSIGN mapa_distrib_ccusto.cod_empresa                 = estabelecimento.cod_empresa
                   mapa_distrib_ccusto.cod_estab                   = estabelecimento.cod_estab
                   mapa_distrib_ccusto.cod_mapa_distrib_ccusto     = cta_ctbl.cod_cta_ctbl
                   mapa_distrib_ccusto.cod_plano_ccusto            = "PLCCUNI"
                   mapa_distrib_ccusto.dat_inic_valid              = DATE('01/' + STRING(MONTH(TODAY)) + '/' + STRING(YEAR(TODAY)))
                   mapa_distrib_ccusto.dat_fim_valid               = 12/31/9999
                   mapa_distrib_ccusto.des_mapa_distrib_ccusto     = "JDE-" + cta_ctbl.des_tit_ctbl
                   mapa_distrib_ccusto.ind_tip_mapa_distrib_ccusto = "Lista".
        END.
    END.
END.

RUN pi-seta-titulo in h-acomp("Criando CCusto no Mapa...").

FOR EACH es-cross-reference-jde NO-LOCK
    WHERE es-cross-reference-jde.cod_cta_ctbl >= p-cod_cta_ctbl-ini
    AND   es-cross-reference-jde.cod_cta_ctbl <= p-cod_cta_ctbl-fim
    AND   es-cross-reference-jde.log-erro      = 0:

    IF es-cross-reference-jde.cod_ccusto = "" OR
       es-cross-reference-jde.cod_ccusto = "000000" THEN NEXT.

    RUN pi-acompanhar IN h-acomp(INPUT 'Criando Ccusto Mapa: ' + es-cross-reference-jde.legacy-company-number + "-" +
                                       es-cross-reference-jde.cod_cta_ctbl + "-" + es-cross-reference-jde.cod_ccusto).

    ASSIGN cEmpresa = "".
    FIND FIRST estabelecimento NO-LOCK
        WHERE estabelecimento.cod_estab = es-cross-reference-jde.Legacy-Company-Number NO-ERROR.
    IF AVAILABLE estabelecimento THEN
        ASSIGN cEmpresa = estabelecimento.cod_empresa.

    LoopEstab:
    FOR EACH estabelecimento NO-LOCK
        WHERE estabelecimento.cod_empresa = cEmpresa:
        FIND FIRST mapa_distrib_ccusto NO-LOCK
            WHERE mapa_distrib_ccusto.cod_empresa             = estabelecimento.cod_empresa
            AND   mapa_distrib_ccusto.cod_estab               = estabelecimento.cod_estab
            AND   mapa_distrib_ccusto.cod_mapa_distrib_ccusto = es-cross-reference-jde.cod_cta_ctbl NO-ERROR.
        IF AVAILABLE mapa_distrib_ccusto THEN DO:
            FIND FIRST ems5.ccusto NO-LOCK
                WHERE ccusto.cod_empresa = mapa_distrib_ccusto.cod_empresa
                AND   ccusto.cod_ccusto  = es-cross-reference-jde.cod_ccusto NO-ERROR.
            IF AVAILABLE ems5.ccusto THEN DO:
                FOR EACH es-cross-ref-regra-mapa NO-LOCK
                    WHERE es-cross-ref-regra-mapa.cod_empresa = estabelecimento.cod_empresa
                    AND   es-cross-ref-regra-mapa.cod_estab   = estabelecimento.cod_estab:
                    IF es-cross-reference-jde.cod_ccusto >= es-cross-ref-regra-mapa.cod_ccusto_ini AND
                       es-cross-reference-jde.cod_ccusto <= es-cross-ref-regra-mapa.cod_ccusto_fin THEN
                        NEXT LoopEstab.
                END.

                FIND FIRST item_lista_ccusto NO-LOCK
                    WHERE item_lista_ccusto.cod_empresa             = mapa_distrib_ccusto.cod_empresa
                    AND   item_lista_ccusto.cod_estab               = mapa_distrib_ccusto.cod_estab
                    AND   item_lista_ccusto.cod_mapa_distrib_ccusto = mapa_distrib_ccusto.cod_mapa_distrib_ccusto
                    AND   item_lista_ccusto.cod_plano_ccusto        = mapa_distrib_ccusto.cod_plano_ccusto
                    AND   item_lista_ccusto.cod_ccusto              = es-cross-reference-jde.cod_ccusto NO-ERROR.
                IF NOT AVAILABLE item_lista_ccusto THEN DO:
                    CREATE item_lista_ccusto.
                    ASSIGN item_lista_ccusto.cod_empresa             = mapa_distrib_ccusto.cod_empresa
                           item_lista_ccusto.cod_estab               = mapa_distrib_ccusto.cod_estab
                           item_lista_ccusto.cod_mapa_distrib_ccusto = mapa_distrib_ccusto.cod_mapa_distrib_ccusto
                           item_lista_ccusto.cod_plano_ccusto        = mapa_distrib_ccusto.cod_plano_ccusto
                           item_lista_ccusto.cod_ccusto              = es-cross-reference-jde.cod_ccusto.
                END.
            END.
        END.
    END.
END.

run pi-finalizar in h-acomp.

RETURN "OK".

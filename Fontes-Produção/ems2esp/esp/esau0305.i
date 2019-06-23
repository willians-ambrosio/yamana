    FOR  EACH tabela_vrf_monitor NO-LOCK
        WHERE tabela_vrf_monitor.cod_base_dados         >= tt-param.c-base-ini
          AND tabela_vrf_monitor.cod_base_dados         <= tt-param.c-base-fim
          AND tabela_vrf_monitor.cod_tabela             >= tt-param.c-tab-ini
          AND tabela_vrf_monitor.cod_tabela             <= tt-param.c-tab-fim
          AND tabela_vrf_monitor.dat_atualiz            >= tt-param.c-dat-ini
          AND tabela_vrf_monitor.dat_atualiz            <= tt-param.c-dat-fim
          AND tabela_vrf_monitor.cod_usuario            >= tt-param.c-user-ini
          AND tabela_vrf_monitor.cod_usuario            <= tt-param.c-user-fim,
         EACH atrib_vrf_monitor OF tabela_vrf_monitor
        WHERE atrib_vrf_monitor.cod_atributo >= tt-param.c-atr-ini
          AND atrib_vrf_monitor.cod_atributo <= tt-param.c-atr-fim
        /*{1}*/
        :
    
        IF (tabela_vrf_monitor.cod_evento = 'C' AND tt-param.l-create = NO)
        OR (tabela_vrf_monitor.cod_evento = 'W' AND tt-param.l-write  = NO)
        OR (tabela_vrf_monitor.cod_evento = 'D' AND tt-param.l-delete = NO)
        THEN NEXT.
    
        /* Oracle Nativo possui apenas um programa, entÆo deve verificar o primeiro item */
        find bf-base_dados
            where bf-base_dados.cod_base_dados = tabela_vrf_monitor.cod_base_dados
            no-lock no-error.
        if  avail bf-base_dados THEN DO:
            /* Se Oracle ou SQL Server nativo ent’o desativa o bot’o de check syntax */
            ASSIGN l-oracle = ({&cdn_tip_banco} = 3 OR {&cdn_tip_banco} = 5).
    
            IF l-oracle THEN DO:
                IF NOT (tabela_vrf_monitor.des_prog_atualiz[1]    >= tt-param.c-prog-ini
                    AND tabela_vrf_monitor.des_prog_atualiz[1]    <= tt-param.c-prog-fim) THEN
                    NEXT.
            END.
            ELSE DO:
                IF NOT (tabela_vrf_monitor.des_prog_atualiz[2]    >= tt-param.c-prog-ini
                    AND tabela_vrf_monitor.des_prog_atualiz[2]    <= tt-param.c-prog-fim) THEN
                    NEXT.
            END.
        END.

/******************************************************************************
**
**   ESRE0508.I3 - Include para emitir o relatorio pela classif. 2
**             . Item
**             . Lista Saldos Zerados
**
******************************************************************************/

    for each saldo-terc use-index &if "{&bf_dis_versao_ems}" >= "2.05" &then item-emit &else item &endif
        where saldo-terc.serie-docto  >= tt-param.c-ini-serie-docto
        and   saldo-terc.serie-docto  <= tt-param.c-fim-serie-docto
        and   saldo-terc.nro-docto    >= tt-param.c-ini-nro-docto
        and   saldo-terc.nro-docto    <= tt-param.c-fim-nro-docto
        and   saldo-terc.cod-emitente >= tt-param.i-ini-cod-emitente
        and   saldo-terc.cod-emitente <= tt-param.i-fim-cod-emitente
        and   saldo-terc.nat-operacao >= tt-param.c-ini-nat-operacao
        and   saldo-terc.nat-operacao <= tt-param.c-fim-nat-operacao
        and   saldo-terc.it-codigo    >= tt-param.c-ini-it-codigo
        and   saldo-terc.it-codigo    <= tt-param.c-fim-it-codigo
        and   saldo-terc.dt-retorno   >= tt-param.da-ini-per
        and   saldo-terc.dt-retorno   <= tt-param.da-fim-per 
    /************************************************************ 
     *  Pr‚-Processador para - 2.03
     ************************************************************/      
      &if defined (bf_mat_versao_ems)  &then
        &if "{&bf_mat_versao_ems}" >= "2.03" &then
            and   saldo-terc.nr-ato-concessorio >= tt-param.nr-ato-conce        
            and   saldo-terc.nr-ato-concessorio <= tt-param.nr-ato-conce-fim
        &endif
      &endif       no-lock:
      
        /*************************************************************************/
        /*** INÖCIO CHAMADA EPC - Cliente: SHV / FO: 1437.139                  ***/
        /*************************************************************************/
        FOR EACH tt-epc
            WHERE tt-epc.cod-event = "verifica-saldo-terc":
            DELETE tt-epc.
        END.

        {include/i-epc200.i2 &CodEvent='"verifica-saldo-terc"'
                             &CodParameter='"saldo-terc rowid"'
                             &ValueParameter="STRING(ROWID(saldo-terc))"}
                             
        {include/i-epc201.i "verifica-saldo-terc"}

        IF  RETURN-VALUE = "NOK" THEN NEXT.
        /*--------------------------------------------------------------------------------*/

        run pi-imprime-terc-excel.
    end.

/* fim do include */

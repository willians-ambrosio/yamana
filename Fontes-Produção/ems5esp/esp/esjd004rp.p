/*****************************************************************************
** Programa..............: esjd004rp
** Descricao.............: Lista Contas Sint‚ticas com movimenta‡Æo
** Versao................: 1.00.00.000
** Procedimento..........: 
** Nome Externo..........: 
** Criado por............: Bruno Bertulli (DSC)
** Criado em.............: 17/10/2014
*****************************************************************************/
def new global shared var v_cod_usuar_corren    like usuar_mestre.cod_usuario   no-undo.

DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.

DEFINE TEMP-TABLE tt-conta LIKE cta_ctbl
    FIELD num_lote_ctbl     LIKE lancto_ctbl.num_lote_ctbl
    FIELD cod_modul_dts     LIKE lancto_ctbl.cod_modul_dts
    FIELD dat_lancto_ctbl   LIKE lancto_ctbl.dat_lancto_ctbl
    FIELD cod_empresa       LIKE lancto_ctbl.cod_empresa.

DEFINE TEMP-TABLE tt-param
    FIELD cod_cta_ctbl-ini AS CHARACTER
    FIELD cod_cta_ctbl-fim AS CHARACTER
    FIELD data-ini         AS CHARACTER
    FIELD data-fim         AS CHARACTER
    FIELD arquivo          AS CHARACTER
    FIELD saida            AS CHARACTER.

/* ===> Main Block <=== */

FIND FIRST dwb_rpt_param EXCLUSIVE-LOCK
     WHERE dwb_rpt_param.cod_dwb_program = "esjd004rp"
     AND   dwb_rpt_param.cod_dwb_user    = v_cod_usuar_corren No-error.
IF AVAILABLE dwb_rpt_param THEN DO:
    CREATE tt-param.
    ASSIGN tt-param.cod_cta_ctbl-ini = ENTRY(1,dwb_rpt_param.cod_dwb_parameters,";")
           tt-param.cod_cta_ctbl-fim = ENTRY(2,dwb_rpt_param.cod_dwb_parameters,";")
           tt-param.data-ini         = ENTRY(3,dwb_rpt_param.cod_dwb_parameters,";")
           tt-param.data-fim         = ENTRY(4,dwb_rpt_param.cod_dwb_parameters,";")
           tt-param.arquivo          = ENTRY(5,dwb_rpt_param.cod_dwb_parameters,";")
           tt-param.saida            = dwb_rpt_param.ind_dwb_run_mode.
END.

FIND FIRST tt-param NO-LOCK NO-ERROR.
IF  AVAIL tt-param THEN
    RUN pi-executar.

RETURN "OK".

/* ===> Procedures <=== */

PROCEDURE pi-executar :
    run utp/ut-acomp.p persistent set h-acomp.

    run pi-inicializar in h-acomp (input "Lista Contas Sint‚ticas com Movimenta‡Æo...").

    EMPTY TEMP-TABLE tt-conta.

    FOR EACH cta_ctbl NO-LOCK
        WHERE cta_ctbl.cod_plano_cta = "CONTSOC"
        AND   cta_ctbl.cod_cta_ctbl   >= tt-param.cod_cta_ctbl-ini
        AND   cta_ctbl.cod_cta_ctbl   <= tt-param.cod_cta_ctbl-fim
        AND   cta_ctbl.log_cta_ctbl_exclus_analit = NO:
        RUN pi-acompanhar IN h-acomp (INPUT 'Conta : ' + cta_ctbl.cod_cta_ctbl).

        CREATE tt-conta.
        BUFFER-COPY cta_ctbl TO tt-conta.
    END.

    run pi-seta-titulo in h-acomp ("Processando Lotes...").

    FOR EACH lancto_ctbl NO-LOCK
        WHERE lancto_ctbl.dat_lancto_ctbl >= DATE(tt-param.data-ini)
        AND   lancto_ctbl.dat_lancto_ctbl <= DATE(tt-param.data-fim)
        ,EACH item_lancto_ctbl OF lancto_ctbl NO-LOCK:
        RUN pi-acompanhar IN h-acomp (INPUT 'Lotes : ' + STRING (lancto_ctbl.dat_lancto_ctbl,"99/99/9999") + " " + lancto_ctbl.cod_empresa).

        FIND FIRST tt-conta EXCLUSIVE-LOCK
            WHERE tt-conta.cod_cta_ctbl = item_lancto_ctbl.cod_cta_ctbl NO-ERROR.
        IF AVAILABLE tt-conta THEN
            ASSIGN tt-conta.cod_empresa     = lancto_ctbl.cod_empresa
                   tt-conta.num_lote_ctbl   = lancto_ctbl.num_lote_ctbl
                   tt-conta.cod_modul_dts   = lancto_ctbl.cod_modul_dts
                   tt-conta.dat_lancto_ctbl = lancto_ctbl.dat_lancto_ctbl.
    END.

    OUTPUT TO VALUE(tt-param.arquivo) NO-CONVERT.
    PUT UNFORMATTED "LISTA CONTAS SINTETICAS COM MOVIMENTACAO" SKIP(2).

    FOR EACH tt-conta NO-LOCK
       WHERE tt-conta.num_lote_ctbl <> 0:
        RUN pi-acompanhar IN h-acomp (INPUT 'Imprimindo : ' + tt-conta.cod_cta_ctbl).

        DISP tt-conta.cod_cta_ctbl
             tt-conta.dat_inic_valid
             tt-conta.dat_fim_valid
             tt-conta.cod_empresa
             tt-conta.num_lote_ctbl
             tt-conta.cod_modul_dts
             tt-conta.dat_lancto_ctbl
             WITH WIDTH 333.
    END.
    OUTPUT CLOSE.

    run pi-finalizar in h-acomp. 

    IF tt-param.saida = "On-Line" THEN
        RUN pi-abre-arq (INPUT tt-param.arquivo).
END PROCEDURE.

PROCEDURE pi-abre-arq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def Input param p_cod_dwb_file
        as character
        format "x(40)"
        no-undo.

    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_cod_key_value
        as character
        format "x(8)":U
        no-undo.

    /************************** Variable Definition End *************************/

    get-key-value section 'EMS' key 'Show-Report-Program' value v_cod_key_value.
    if  v_cod_key_value = ""
    or   v_cod_key_value = ?
    then do:
        assign v_cod_key_value = 'notepad.exe'.
        put-key-value section 'EMS' key 'Show-Report-Program' value v_cod_key_value no-error.
    end /* if */.

    run winexec (input v_cod_key_value + chr(32) + p_cod_dwb_file, input 1).

    END PROCEDURE.

    PROCEDURE WinExec EXTERNAL 'kernel32.dll':
      DEF INPUT  PARAM prg_name                          AS CHARACTER.
      DEF INPUT  PARAM prg_style                         AS SHORT.

END PROCEDURE.


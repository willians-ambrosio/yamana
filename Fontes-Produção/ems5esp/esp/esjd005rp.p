/*****************************************************************************
** Programa..............: esjd005rp
** Descricao.............: Lista Contas com Saldo
** Versao................: 1.00.00.000
** Procedimento..........: 
** Nome Externo..........: 
** Criado por............: Bruno Bertulli (DSC)
** Criado em.............: 17/10/2014
*****************************************************************************/
def new global shared var v_cod_usuar_corren    like usuar_mestre.cod_usuario   no-undo.

DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.

DEFINE TEMP-TABLE tt-param
    FIELD cod_cta_ctbl-ini AS CHARACTER
    FIELD cod_cta_ctbl-fim AS CHARACTER
    FIELD data             AS CHARACTER
    FIELD arquivo          AS CHARACTER
    FIELD saida            AS CHARACTER.

/* ===> Main Block <=== */

FIND FIRST dwb_rpt_param EXCLUSIVE-LOCK
     WHERE dwb_rpt_param.cod_dwb_program = "esjd005rp"
     AND   dwb_rpt_param.cod_dwb_user    = v_cod_usuar_corren No-error.
IF AVAILABLE dwb_rpt_param THEN DO:
    CREATE tt-param.
    ASSIGN tt-param.cod_cta_ctbl-ini = ENTRY(1,dwb_rpt_param.cod_dwb_parameters,";")
           tt-param.cod_cta_ctbl-fim = ENTRY(2,dwb_rpt_param.cod_dwb_parameters,";")
           tt-param.data             = ENTRY(3,dwb_rpt_param.cod_dwb_parameters,";")
           tt-param.arquivo          = ENTRY(4,dwb_rpt_param.cod_dwb_parameters,";")
           tt-param.saida            = dwb_rpt_param.ind_dwb_run_mode.
END.

FIND FIRST tt-param NO-LOCK NO-ERROR.
IF  AVAIL tt-param THEN
    RUN pi-executar.

RETURN "OK".

/* ===> Procedures <=== */
    
PROCEDURE pi-executar:

    run utp/ut-acomp.p persistent set h-acomp.

    run pi-inicializar in h-acomp (input "Lista Contas com Saldo...").

    OUTPUT TO VALUE(tt-param.arquivo) NO-CONVERT.

    PUT UNFORMATTED "LISTA CONTAS COM SALDO X JDE" SKIP(2).

    FOR EACH cta_ctbl NO-LOCK
        WHERE cta_ctbl.cod_plano_cta   = "CONTSOC"
        AND   cta_ctbl.cod_cta_ctbl   >= tt-param.cod_cta_ctbl-ini
        AND   cta_ctbl.cod_cta_ctbl   <= tt-param.cod_cta_ctbl-fim
        AND   cta_ctbl.dat_inic_valid <= TODAY
        AND   cta_ctbl.dat_fim_valid  >= TODAY
        AND   cta_ctbl.log_cta_ctbl_exclus_analit = YES:

        RUN pi-acompanhar IN h-acomp (INPUT 'Conta : ' + cta_ctbl.cod_cta_ctbl).

        FOR EACH estabelecimento NO-LOCK
            WHERE estabelecimento.cod_empresa <> "CAN"
            AND   estabelecimento.log_estab_princ:

            for EACH sdo_ctbl OF cta_ctbl NO-LOCK
                WHERE sdo_ctbl.cod_empresa      = estabelecimento.cod_empresa
                AND   sdo_ctbl.cod_finalid_econ = "Corrente"
                AND   sdo_ctbl.dat_sdo_ctbl    >= DATE (tt-param.data):

                IF sdo_ctbl.val_sdo_ctbl_db  = 0 AND
                   sdo_ctbl.val_sdo_ctbl_cr  = 0 AND
                   sdo_ctbl.val_sdo_ctbl_fim = 0 THEN NEXT.

                IF YEAR (sdo_ctbl.dat_sdo_ctbl) <> YEAR(TODAY) THEN NEXT.

                FIND FIRST es-cross-reference-jde NO-LOCK
                    WHERE es-cross-reference-jde.Legacy-Company-Number = estabelecimento.cod_estab
                    AND   es-cross-reference-jde.cod_cta_ctbl          = sdo_ctbl.cod_cta_ctbl
                    AND   es-cross-reference-jde.cod_ccusto            = sdo_ctbl.cod_ccusto
                    AND   es-cross-reference-jde.log-erro              = 0 NO-ERROR.
                IF AVAILABLE es-cross-reference-jde THEN NEXT.

                FIND LAST criter_distrib_cta_ctbl NO-LOCK
                     where criter_distrib_cta_ctbl.cod_empresa        = estabelecimento.cod_empresa
                       AND criter_distrib_cta_ctbl.cod_plano_cta_ctbl = cta_ctbl.cod_plano_cta_ctbl
                       and criter_distrib_cta_ctbl.cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl
                       and criter_distrib_cta_ctbl.cod_estab          = estabelecimento.cod_estab NO-ERROR.
                IF AVAILABLE criter_distrib_cta_ctbl THEN DO:
                    IF criter_distrib_cta_ctbl.ind_criter_distrib_ccusto = "Definidos" AND sdo_ctbl.cod_ccusto = "" THEN NEXT.
                END.

                disp sdo_ctbl.cod_empresa
                     sdo_ctbl.dat_sdo_ctbl
                     sdo_ctbl.cod_cenar_ctbl
                     sdo_ctbl.cod_estab
                     sdo_ctbl.cod_finalid_econ
                     sdo_ctbl.cod_cta_ctbl
                     sdo_ctbl.cod_ccusto
                     sdo_ctbl.cod_unid_negoc
                     sdo_ctbl.val_sdo_ctbl_db
                     sdo_ctbl.val_sdo_ctbl_cr
                     sdo_ctbl.val_sdo_ctbl_fim
                     with width 333.

            end.
        END.
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



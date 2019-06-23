/*****************************************************************************
** Programa..............: esjd001rp
** Descricao.............: Carga planilha JDE
** Versao................: 1.00.00.000
** Procedimento..........: 
** Nome Externo..........: 
** Criado por............: Bruno Bertulli (DSC)
** Criado em.............: 17/10/2014
*****************************************************************************/
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
        
def new global shared var v_cod_usuar_corren    like usuar_mestre.cod_usuario   no-undo.

DEF temp-table tt-erro no-undo
    field i-sequen      as int             
    field cd-erro       as int
    field mensagem      as char format "x(155)"
    FIELD cod_cta_ctbl  LIKE es-cross-reference-jde.cod_cta_ctbl
    FIELD cod_ccusto    LIKE es-cross-reference-jde.cod_ccusto
    FIELD cod_estab     LIKE estabelecimento.cod_estab
    FIELD tipo          AS   CHARACTER format "x(20)"
    .

DEFINE TEMP-TABLE tt-param
    FIELD Arquivo-JDE  AS CHARACTER
    FIELD Arquivo-log  AS CHARACTER
    FIELD saida        AS CHARACTER
    .

/* ===> Main Block <=== */
Find First dwb_rpt_param No-lock
     Where dwb_rpt_param.cod_dwb_program = "esjd001rp"
       And dwb_rpt_param.cod_dwb_user    = v_cod_usuar_corren No-error.
If Avail dwb_rpt_param Then Do:
    CREATE tt-param.
    ASSIGN tt-param.Arquivo-JDE = ENTRY (1, dwb_rpt_param.cod_dwb_parameters, ";")
           tt-param.Arquivo-LOG = ENTRY (2, dwb_rpt_param.cod_dwb_parameters, ";")
           tt-param.saida       = dwb_rpt_param.ind_dwb_run_mode.
END.

FIND FIRST tt-param NO-LOCK NO-ERROR.
IF AVAILABLE tt-param THEN DO:
    RUN pi-carrega-planilha.
    RUN pi-valida-planilha.
END.

RETURN "OK".

/* ===> Procedures <=== */

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



PROCEDURE pi-carrega-planilha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def var cLinha      as char no-undo.
    def var cDelimiter  as char no-undo init ";".
    def var l-rpw-ativo as log no-undo.

    find first es-param-prog no-lock
         where es-param-prog.cod_prog_dtsul = "ESJD001"
           and es-param-prog.cod-param      = "RPW Ativo"
           and es-param-prog.txt-valor      = "Sim" no-error.
    assign l-rpw-ativo =(if  avail es-param-prog then yes else no).

    run utp/ut-acomp.p persistent set h-acomp.
    run pi-inicializar in h-acomp (input "Carregando...").

    RUN pi-desabilita-cancela IN h-acomp.

    FOR EACH es-cross-reference-jde:
        RUN pi-acompanhar IN h-acomp (INPUT 'Limpando ' + es-cross-reference-jde.Source-Account-Number ).
        DELETE es-cross-reference-jde.
    END.

    INPUT FROM VALUE(tt-param.Arquivo-JDE).
    REPEAT:
        IMPORT UNFORMATTED cLinha.

        IF ENTRY(01,cLinha,cDelimiter) BEGINS "SOURCE" THEN NEXT.

        RUN pi-acompanhar IN h-acomp (INPUT 'Processando Excel ' + TRIM(ENTRY(03,cLinha,cDelimiter))).

        CREATE es-cross-reference-jde.
        ASSIGN es-cross-reference-jde.SOURCE                  = TRIM(ENTRY(01,cLinha,cDelimiter))
               es-cross-reference-jde.Map-Code                = TRIM(ENTRY(02,cLinha,cDelimiter))
               es-cross-reference-jde.Source-Account-Number   = TRIM(ENTRY(03,cLinha,cDelimiter))
               es-cross-reference-jde.JDE-Account-No          = TRIM(ENTRY((if  l-rpw-ativo then 04 else 05),cLinha,cDelimiter))
               es-cross-reference-jde.Business-Unit           = TRIM(ENTRY((if  l-rpw-ativo then 05 else 06),cLinha,cDelimiter))
               es-cross-reference-jde.Legacy-Company-Number   = TRIM(ENTRY((if  l-rpw-ativo then 06 else 04),cLinha,cDelimiter))
               es-cross-reference-jde.OBJECT                  = TRIM(ENTRY(07,cLinha,cDelimiter))
               es-cross-reference-jde.Subsidiary              = TRIM(ENTRY(08,cLinha,cDelimiter))
               es-cross-reference-jde.Company                 = TRIM(ENTRY(09,cLinha,cDelimiter))
               es-cross-reference-jde.Account-Desc            = TRIM(ENTRY(10,cLinha,cDelimiter))
               es-cross-reference-jde.Account-Desc-Spanish    = TRIM(ENTRY(11,cLinha,cDelimiter))
               es-cross-reference-jde.Account-Desc-Portuguese = TRIM(ENTRY(12,cLinha,cDelimiter))
               es-cross-reference-jde.LOD                     = TRIM(ENTRY(13,cLinha,cDelimiter))
               es-cross-reference-jde.PEC                     = TRIM(ENTRY(14,cLinha,cDelimiter))
               es-cross-reference-jde.Curr-CODE               = TRIM(ENTRY(15,cLinha,cDelimiter))
               es-cross-reference-jde.USER-ID                 = TRIM(ENTRY(16,cLinha,cDelimiter))
               es-cross-reference-jde.Date-Created            = DATE (ENTRY(17,cLinha,cDelimiter))
               es-cross-reference-jde.Time-Created            = INTEGER (ENTRY(18,cLinha,cDelimiter))
               es-cross-reference-jde.Last-Modified-BY        = ENTRY(19,cLinha,cDelimiter)
               es-cross-reference-jde.Date-Last-MODIFIED      = DATE (ENTRY(20,cLinha,cDelimiter))
               es-cross-reference-jde.Time-Last-Updated       = INTEGER (ENTRY(21,cLinha,cDelimiter))
               es-cross-reference-jde.Notes-1                 = TRIM(ENTRY(22,cLinha,cDelimiter))
               es-cross-reference-jde.Notes-2                 = TRIM(ENTRY(23,cLinha,cDelimiter))
               es-cross-reference-jde.cod_cta_ctbl            = SUBSTRING (es-cross-reference-jde.Source-Account-Number,1,8)
               es-cross-reference-jde.cod_ccusto              = SUBSTRING (es-cross-reference-jde.Source-Account-Number,9,6)
               es-cross-reference-jde.log-erro                = 0.

        IF es-cross-reference-jde.Legacy-Company-Number = '1'  THEN ASSIGN es-cross-reference-jde.Legacy-Company-Number = '01'.
        IF es-cross-reference-jde.Legacy-Company-Number = '1A' THEN ASSIGN es-cross-reference-jde.Legacy-Company-Number = '01A'.
        IF es-cross-reference-jde.Legacy-Company-Number = '2'  THEN ASSIGN es-cross-reference-jde.Legacy-Company-Number = '02'.
        IF es-cross-reference-jde.Legacy-Company-Number = '2A' THEN ASSIGN es-cross-reference-jde.Legacy-Company-Number = '02A'.
        IF es-cross-reference-jde.Legacy-Company-Number = '3'  THEN ASSIGN es-cross-reference-jde.Legacy-Company-Number = '03'.
    END.
    INPUT CLOSE.

    run pi-finalizar in h-acomp. 
END PROCEDURE.


PROCEDURE pi-valida-planilha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lErro   AS LOGICAL     NO-UNDO.

    run utp/ut-acomp.p persistent set h-acomp.
    run pi-inicializar in h-acomp (input "Validando...").

    RUN pi-desabilita-cancela IN h-acomp.

    EMPTY TEMP-TABLE tt-erro.

    FOR EACH es-cross-reference-jde EXCLUSIVE-LOCK:
        RUN pi-acompanhar IN h-acomp (INPUT 'Validando ' + es-cross-reference-jde.cod_cta_ctbl + "-" + es-cross-reference-jde.cod_ccusto).

        ASSIGN lErro = NO.

        FIND FIRST estabelecimento NO-LOCK
            WHERE estabelecimento.cod_estab = es-cross-reference-jde.Legacy-Company-Number NO-ERROR.
        IF NOT AVAILABLE estabelecimento THEN DO:
            CREATE tt-erro.
            ASSIGN tt-erro.i-sequen     = 0
                   tt-erro.cod_estab    = es-cross-reference-jde.Legacy-Company-Number
                   tt-erro.cod_cta_ctbl = es-cross-reference-jde.cod_cta_ctbl
                   tt-erro.cod_ccusto   = es-cross-reference-jde.cod_ccusto
                   tt-erro.tipo         = "Erro"
                   tt-erro.cd-erro      = 1
                   tt-erro.mensagem     = "Estabelecimento < " + es-cross-reference-jde.Legacy-Company-Number + " > n∆o encontrado.".
            ASSIGN lErro = YES.
        END.

        FIND FIRST cta_ctbl NO-LOCK
            WHERE cta_ctbl.cod_plano_cta_ctbl = "CONTSOC"
            AND   cta_ctbl.cod_cta_ctbl       = es-cross-reference-jde.cod_cta_ctbl NO-ERROR.
        IF NOT AVAILABLE cta_ctbl THEN DO:
            CREATE tt-erro.
            ASSIGN tt-erro.i-sequen     = 0
                   tt-erro.cod_estab    = es-cross-reference-jde.Legacy-Company-Number
                   tt-erro.cod_cta_ctbl = es-cross-reference-jde.cod_cta_ctbl
                   tt-erro.cod_ccusto   = es-cross-reference-jde.cod_ccusto
                   tt-erro.tipo         = "Advertencia"
                   tt-erro.cd-erro      = 2
                   tt-erro.mensagem     = "Conta < " + es-cross-reference-jde.cod_cta_ctbl + " > n∆o encontrada no plano de contas CONTSOC.".
            ASSIGN lErro = YES.
        END.
        ELSE DO:
            IF cta_ctbl.dat_inic_valid > TODAY OR
               cta_ctbl.dat_fim_valid  < TODAY THEN DO:
                CREATE tt-erro.
                ASSIGN tt-erro.i-sequen     = 0
                       tt-erro.cod_estab    = es-cross-reference-jde.Legacy-Company-Number
                       tt-erro.cod_cta_ctbl = es-cross-reference-jde.cod_cta_ctbl
                       tt-erro.cod_ccusto   = es-cross-reference-jde.cod_ccusto
                       tt-erro.tipo         = "Advertencia"
                       tt-erro.cd-erro      = 3
                       tt-erro.mensagem     = "Conta < " + es-cross-reference-jde.cod_cta_ctbl + " > fora da validade.".
                ASSIGN lErro = YES.
            END.
        END.

        IF AVAILABLE estabelecimento AND 
           es-cross-reference-jde.cod_ccusto <> "" AND
           es-cross-reference-jde.cod_ccusto <> "000000" THEN DO:
            FIND FIRST ems5.ccusto NO-LOCK
                WHERE ccusto.cod_empresa = estabelecimento.cod_empresa
                AND   ccusto.cod_ccusto  = es-cross-reference-jde.cod_ccusto NO-ERROR.
            IF NOT AVAILABLE ems5.ccusto THEN DO:
                CREATE tt-erro.
                ASSIGN tt-erro.i-sequen     = 0
                       tt-erro.cod_estab    = es-cross-reference-jde.Legacy-Company-Number
                       tt-erro.cod_cta_ctbl = es-cross-reference-jde.cod_cta_ctbl
                       tt-erro.cod_ccusto   = es-cross-reference-jde.cod_ccusto
                       tt-erro.tipo         = "Erro"
                       tt-erro.cd-erro      = 4
                       tt-erro.mensagem     = "Centro de Custo < " + es-cross-reference-jde.cod_ccusto + " > n∆o cadastrado para a empresa < " + estabelecimento.cod_empresa + " > .".
                ASSIGN lErro = YES.
            END.
        END.

        IF lErro = YES THEN 
            ASSIGN es-cross-reference-jde.log-erro = 1.
    END.

    RUN pi-seta-titulo in h-acomp ("Imprimindo Log de erro...").

    OUTPUT TO VALUE(tt-param.Arquivo-LOG) NO-ECHO NO-CONVERT.
    PUT UNFORMATTED "LOG DE ERRO DA CARGA DO JDE" SKIP(2).

    FOR EACH tt-erro BREAK BY tt-erro.cd-erro:
        DISP tt-erro.cod_estab     COLUMN-LABEL "Estab"
             tt-erro.cod_cta_ctbl  COLUMN-LABEL "Conta Cont†bil"
             tt-erro.cod_ccusto    COLUMN-LABEL "Centro Custo"
             tt-erro.tipo          COLUMN-LABEL "Tipo"
             tt-erro.cd-erro       COLUMN-LABEL "Erro"
             tt-erro.mensagem      COLUMN-LABEL "Descriá∆o Erro"
             WITH WIDTH 333.
    END.
    OUTPUT CLOSE.

    run pi-finalizar in h-acomp. 

    IF tt-param.saida = "On-Line" THEN
        RUN pi-abre-arq (INPUT tt-param.Arquivo-LOG).
END PROCEDURE.


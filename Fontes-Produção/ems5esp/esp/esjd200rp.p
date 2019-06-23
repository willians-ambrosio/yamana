/*****************************************************************************
** Programa..............: esjd200rp
** Descricao.............: Desabilita contas EMS2
** Versao................: 1.00.00.000
** Procedimento..........: 
** Nome Externo..........: 
** Criado por............: Bruno Bertulli (DSC)
** Criado em.............: 17/10/2014
*****************************************************************************/
def buffer ccusto  for ems5.ccusto.
def buffer empresa for ems5.empresa.

def temp-table tt-conta-contab no-undo
    field ep-codigo      like ccusto.cod_empresa
    field cod_empresa    like ccusto.cod_empresa
    field cod_cta_ctbl   like cta_ctbl.cod_cta_ctbl
    field desc-conta     like cta_ctbl.des_tit_ctbl
    field cod_unid_negoc as char format "x(02)"
    field cod_ccusto     like ccusto.cod_ccusto
    field desc-ccusto    like ccusto.des_tit_ctbl
    field id             as char format "x(19)"
    index ix as primary unique id.

def temp-table tt-param-connect no-undo
    field c-param as char
    index ix as primary unique c-param.

{utp/ut-glob.i}
{esp/esjd200.i}

def new global shared var v_cod_usuar_corren    like usuar_mestre.cod_usuario   no-undo.

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

DEF temp-table tt-erro no-undo
    field i-sequen as int             
    field cd-erro  as int
    field mensagem as char format "x(255)".

DEFINE TEMP-TABLE tt-param
    FIELD cod_cta_ctbl-ini AS CHARACTER
    FIELD cod_cta_ctbl-fim AS CHARACTER
    FIELD data-ini         AS CHARACTER
    FIELD data-fim         AS CHARACTER
    FIELD mapa             AS CHARACTER
    FIELD diretorio        AS CHARACTER
    FIELD arquivo          AS CHARACTER
    FIELD saida            AS CHARACTER.

/* ===> Main Block <=== */

FIND FIRST dwb_rpt_param EXCLUSIVE-LOCK
     WHERE dwb_rpt_param.cod_dwb_program = "esjd200rp"
     AND   dwb_rpt_param.cod_dwb_user    = v_cod_usuar_corren No-error.
IF AVAILABLE dwb_rpt_param THEN DO:
    CREATE tt-param.
    ASSIGN tt-param.cod_cta_ctbl-ini = ENTRY(1,dwb_rpt_param.cod_dwb_parameters,";")
           tt-param.cod_cta_ctbl-fim = ENTRY(2,dwb_rpt_param.cod_dwb_parameters,";")
           tt-param.data-ini         = ENTRY(3,dwb_rpt_param.cod_dwb_parameters,";")
           tt-param.data-fim         = ENTRY(4,dwb_rpt_param.cod_dwb_parameters,";")
           tt-param.diretorio        = ENTRY(6,dwb_rpt_param.cod_dwb_parameters,";")
           tt-param.arquivo          = ENTRY(7,dwb_rpt_param.cod_dwb_parameters,";")
           tt-param.saida            = dwb_rpt_param.ind_dwb_run_mode.
END.

FIND FIRST tt-param NO-LOCK NO-ERROR.
IF  AVAIL tt-param THEN
    RUN pi-executar.

RETURN "OK".

/* ===> Procedures <=== */

PROCEDURE pi-executar :
    DEFINE VARIABLE cCta-Sistema AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cCta-Estoque AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lPossuiMovto AS LOGICAL     NO-UNDO.

    def var ch-Excel            as component-handle                     no-undo.

    ASSIGN cCta-Sistema = "N∆o gera lanáamentos,Gera lanáamentos,Conta de Saldo".
    ASSIGN cCta-Estoque = "Consumo,N∆o Consumo,Ordem serviáo,Transferencia,M∆o-de-Obra,Custo Mercadoria vendida,Transit¢ria de Compras,Transit¢ria de devoluá∆o,Saldo Por Grupo de Estoque,N∆o gera lanáamentos".

    run utp/ut-acomp.p persistent set h-acomp.

    run pi-inicializar in h-acomp(input "Processando EMS2").

    EMPTY TEMP-TABLE tt-cross-reference.

    run pi-create-tt-cross-reference.

/*     RUN pi-contas-pagar. */
/*     RUN pi-contas-receb. */
    RUN pi-faturamento.
    RUN pi-banco-ext.

    OUTPUT TO VALUE(tt-param.arquivo) NO-CONVERT.

    PUT UNFORMATTED 
     "Emp"
     ";Conta"
     ";Descriá∆o"
     ";Centro de Custo"
     ";Descriá∆o"
     ";Situaá∆o"
     ";Contabilidade"
     ";Ctas Pagar"
     ";Ctas Receber"
     ";Patrimonio"
     ";Caixa Bancos"
     ";Investimentos"
     ";Faturamento"
     ";Obrigaá‰es Fiscais"
     ";Estoque"
     ";Folha"
     ";Lacnto Conta Pagar"
     ";Lacnto Contas Receber"
     ";Lancto Faturamento"
     ";Lancto Estoque"
     ";Lancto Frotas"
     ";Observacao"
     SKIP.

    FOR EACH tt-cross-reference EXCLUSIVE-LOCK:
        RUN pi-acompanhar IN h-acomp(INPUT 'Imprimindo ' + tt-cross-reference.cod_unid_organ + " - Conta : " + tt-cross-reference.ct-codigo).

        PUT UNFORMATTED 
             tt-cross-reference.cod_unid_organ
         ";" tt-cross-reference.ct-codigo
         ";" tt-cross-reference.des_tit_ctbl
         ";" tt-cross-reference.sc-codigo
         ";" tt-cross-reference.des_ccusto
         ";" tt-cross-reference.estado
         ";" ENTRY (tt-cross-reference.contab       , cCta-Sistema)
         ";" ENTRY (tt-cross-reference.contas-pagar , cCta-Sistema)
         ";" ENTRY (tt-cross-reference.contas-receb , cCta-Sistema)
         ";" ENTRY (tt-cross-reference.patrimonio   , cCta-Sistema)
         ";" ENTRY (tt-cross-reference.caixa-bancos , cCta-Sistema)
         ";" ENTRY (tt-cross-reference.investimento , cCta-Sistema)
         ";" ENTRY (tt-cross-reference.faturamento  , cCta-Sistema)
         ";" ENTRY (tt-cross-reference.obrig-fisc   , cCta-Sistema)
         ";" ENTRY (tt-cross-reference.estoque      , cCta-Estoque)
         ";" ENTRY (tt-cross-reference.folha-pagto  , cCta-Sistema)
         ";" tt-cross-reference.lancto-contas-pagar
         ";" tt-cross-reference.lancto-contas-receber
         ";" tt-cross-reference.lancto-faturamento
         ";" tt-cross-reference.lancto-estoque
         ";" tt-cross-reference.lancto-frotas
         ";" tt-cross-reference.observacao
         SKIP.
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

PROCEDURE pi-banco-ext :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    OS-DELETE VALUE(tt-param.diretorio + "esjd200b-movto-estoq.csv")   NO-ERROR.
    OS-DELETE VALUE(tt-param.diretorio + "esjd200b-ordem-compra.csv")  NO-ERROR.
    OS-DELETE VALUE(tt-param.diretorio + "esjd200b-requisicao.csv")    NO-ERROR.
    OS-DELETE VALUE(tt-param.diretorio + "esjd200-frotas-ordem.csv")   NO-ERROR.
    OS-DELETE VALUE(tt-param.diretorio + "esjd200-frotas-abastec.csv") NO-ERROR.

    OUTPUT TO VALUE(tt-param.diretorio + "esjd200b-movto-estoq.csv") NO-CONVERT APPEND.
    PUT UNFORMAT "Conta;CCusto;Emp;Estab;Item;Data" SKIP(2).
    OUTPUT CLOSE.

    OUTPUT TO VALUE(tt-param.diretorio + "esjd200b-ordem-compra.csv") NO-CONVERT APPEND.
    PUT UNFORMAT "Conta;CCusto;Emp;Ordem Compra;Item;Data Emiss∆o;Data Pedido;Situaá∆o" SKIP(2).
    OUTPUT CLOSE.

    OUTPUT TO VALUE(tt-param.diretorio + "esjd200b-requisicao.csv") NO-CONVERT APPEND.
    PUT UNFORMAT "Conta;CCusto;Emp;Requisicao;Item;Data Requisicao;Situacao" SKIP(2).
    OUTPUT CLOSE.

    OUTPUT TO VALUE(tt-param.diretorio + "esjd200-frotas-ordem.csv") NO-CONVERT APPEND.
    PUT UNFORMATTED "Empresa;Ordem;Conta;CCusto" SKIP(2).
    OUTPUT CLOSE.

    OUTPUT TO VALUE(tt-param.diretorio + "esjd200-frotas-abastec.csv") NO-CONVERT APPEND.
    PUT UNFORMATTED "Empresa;Docto;Data Movto;Conta;CCusto" SKIP(2).
    OUTPUT CLOSE.

    empty temp-table tt-param-connect.

    for each bco_empres no-lock
       where bco_empres.log_bco_ativ
         and bco_empres.cod_empresa   < "A"
         and bco_empres.cod_bco_dados = "ems2cademp":
        RUN pi-acompanhar IN h-acomp(INPUT "Conectando banco " + STRING(bco_empres.cod_empresa) + "-CRCAD").

        if  bco_empres.cod_empresa = v_cdn_empres_usuar then
            run pi-param-connect.

        find first tt-param-connect
             where tt-param-connect.c-param = bco_empres.cod_param_conex no-error.
        if  not avail tt-param-connect then
            run pi-param-connect.
        else
            next.

        IF  CONNECTED('CRCAD') THEN
            DISCONNECT 'CRCAD' NO-ERROR.

        connect -db value(bco_empres.cod_bco_fisic)
                -ld 'CRCAD' value(bco_empres.cod_param_conex).

        RUN esp/esjd200c.p(INPUT bco_empres.cod_empresa,
                           INPUT DATE(tt-param.data-ini),
                           INPUT DATE(tt-param.data-fim),
                           INPUT-OUTPUT TABLE tt-cross-reference).

        IF  CONNECTED('CRCAD') THEN
            DISCONNECT 'CRCAD' NO-ERROR.
    end.

    for each bco_empres no-lock
       where bco_empres.log_bco_ativ
         and bco_empres.cod_empresa   < "A"
         and bco_empres.cod_bco_dados = "ems2movemp":
        RUN pi-acompanhar IN h-acomp(INPUT "Conectando banco " + STRING(bco_empres.cod_empresa) + "-CRCAD").

        if  bco_empres.cod_empresa = v_cdn_empres_usuar then
            run pi-param-connect.

        find first tt-param-connect
             where tt-param-connect.c-param = bco_empres.cod_param_conex no-error.
        if  not avail tt-param-connect then
            run pi-param-connect.
        else
            next.

        IF  CONNECTED('CRMOV') THEN
            DISCONNECT VALUE('CRMOV') NO-ERROR.

        connect -db value(bco_empres.cod_bco_fisic)
                -ld 'CRMOV' value(bco_empres.cod_param_conex).

        RUN esp/esjd200b.p(INPUT bco_empres.cod_empresa,
                           INPUT DATE(tt-param.data-ini),
                           INPUT DATE(tt-param.data-fim),
                           INPUT-OUTPUT TABLE tt-cross-reference,
                           INPUT tt-param.diretorio).

        IF  CONNECTED('CRMOV') THEN
            DISCONNECT VALUE('CRMOV') NO-ERROR.
    end.
END PROCEDURE.

PROCEDURE pi-param-connect :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    create tt-param-connect no-error.
    assign tt-param-connect.c-param = bco_empres.cod_param_conex.
END PROCEDURE.

PROCEDURE pi-contas-pagar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH tit-ap NO-LOCK
        WHERE (tit-ap.dt-transacao >= DATE(tt-param.data-ini)
        AND    tit-ap.dt-transacao <= DATE(tt-param.data-fim))
        OR     tit-ap.vl-saldo-me  > 0:

        RUN pi-acompanhar IN h-acomp(INPUT 'Contas a Pagar ' + STRING(tit-ap.dt-transacao,"99/99/9999")).

        FIND FIRST tt-cross-reference EXCLUSIVE-LOCK
            WHERE tt-cross-reference.ep-codigo = tit-ap.ep-codigo
            AND   tt-cross-reference.ct-codigo = tit-ap.ct-conta-db
            AND   tt-cross-reference.sc-codigo = tit-ap.sc-conta-db NO-ERROR.
        IF AVAILABLE tt-cross-reference THEN
            ASSIGN tt-cross-reference.lancto-contas-pagar = YES
                   tt-cross-reference.observacao          = tt-cross-reference.observacao 
                                                       + "Empresa " + STRING(tit-ap.ep-codigo) 
                                                       + " Estab " + tit-ap.cod-estabel 
                                                       + " Serie " + tit-ap.serie 
                                                       + " Titulo " + tit-ap.nr-docto.
    
        FIND FIRST tt-cross-reference EXCLUSIVE-LOCK
            WHERE tt-cross-reference.ep-codigo = tit-ap.ep-codigo
            AND   tt-cross-reference.ct-codigo = tit-ap.ct-conta-for
            AND   tt-cross-reference.sc-codigo = tit-ap.sc-conta-for NO-ERROR.
        IF AVAILABLE tt-cross-reference THEN
            ASSIGN tt-cross-reference.lancto-contas-pagar = YES
                   tt-cross-reference.observacao          = tt-cross-reference.observacao 
                                                       + "Empresa " + STRING(tit-ap.ep-codigo) 
                                                       + " Estab " + tit-ap.cod-estabel 
                                                       + " Serie " + tit-ap.serie 
                                                       + " Titulo " + tit-ap.nr-docto.
    END.

    FOR EACH impto-tit-ap NO-LOCK
        WHERE impto-tit-ap.dt-transacao >= DATE(tt-param.data-ini)
        AND   impto-tit-ap.dt-transacao <= DATE(tt-param.data-fim):

        RUN pi-acompanhar IN h-acomp(INPUT 'Contas a Pagar Impto ' + STRING(impto-tit-ap.dt-transacao,"99/99/9999")).

        FIND FIRST tt-cross-reference EXCLUSIVE-LOCK
            WHERE tt-cross-reference.ep-codigo = impto-tit-ap.ep-codigo
            AND   tt-cross-reference.ct-codigo = impto-tit-ap.ct-imposto
            AND   tt-cross-reference.sc-codigo = impto-tit-ap.sc-imposto NO-ERROR.
        IF AVAILABLE tt-cross-reference THEN
            ASSIGN tt-cross-reference.lancto-contas-pagar = YES
                   tt-cross-reference.observacao          = tt-cross-reference.observacao 
                                                       + "Empresa " + STRING(impto-tit-ap.ep-codigo) 
                                                       + " Estab " + impto-tit-ap.cod-estabel 
                                                       + " Serie " + impto-tit-ap.serie 
                                                       + " Titulo " + impto-tit-ap.nr-docto.

        FIND FIRST tt-cross-reference EXCLUSIVE-LOCK
            WHERE tt-cross-reference.ep-codigo = impto-tit-ap.ep-codigo
            AND   tt-cross-reference.ct-codigo = impto-tit-ap.ct-percepcao
            AND   tt-cross-reference.sc-codigo = impto-tit-ap.sc-percepcao NO-ERROR.
        IF AVAILABLE tt-cross-reference THEN
            ASSIGN tt-cross-reference.lancto-contas-pagar = YES
                   tt-cross-reference.observacao          = tt-cross-reference.observacao 
                                                       + "Empresa " + STRING(impto-tit-ap.ep-codigo) 
                                                       + " Estab " + impto-tit-ap.cod-estabel 
                                                       + " Serie " + impto-tit-ap.serie 
                                                       + " Titulo " + impto-tit-ap.nr-docto.
        
        FIND FIRST tt-cross-reference EXCLUSIVE-LOCK
            WHERE tt-cross-reference.ep-codigo = impto-tit-ap.ep-codigo
            AND   tt-cross-reference.ct-codigo = impto-tit-ap.ct-retencao
            AND   tt-cross-reference.sc-codigo = impto-tit-ap.sc-retencao NO-ERROR.
        IF AVAILABLE tt-cross-reference THEN
            ASSIGN tt-cross-reference.lancto-contas-pagar = YES
                   tt-cross-reference.observacao          = tt-cross-reference.observacao 
                                                       + "Empresa " + STRING(impto-tit-ap.ep-codigo) 
                                                       + " Estab " + impto-tit-ap.cod-estabel 
                                                       + " Serie " + impto-tit-ap.serie 
                                                       + " Titulo " + impto-tit-ap.nr-docto.
    END.

    FOR EACH mov-ap NO-LOCK
        WHERE mov-ap.dt-transacao >= DATE(tt-param.data-ini)
        AND   mov-ap.dt-transacao <= DATE(tt-param.data-fim):
    
        RUN pi-acompanhar IN h-acomp(INPUT 'Contas a Pagar Movto ' + STRING(mov-ap.dt-transacao,"99/99/9999")).

        FIND FIRST tt-cross-reference EXCLUSIVE-LOCK
            WHERE tt-cross-reference.ep-codigo = mov-ap.ep-codigo
            AND   tt-cross-reference.ct-codigo = mov-ap.ct-conta
            AND   tt-cross-reference.sc-codigo = mov-ap.sc-conta NO-ERROR.
        IF AVAILABLE tt-cross-reference THEN
            ASSIGN tt-cross-reference.lancto-contas-pagar = YES
                   tt-cross-reference.observacao          = tt-cross-reference.observacao 
                                                       + "Empresa " + STRING(mov-ap.ep-codigo) 
                                                       + " Estab " + mov-ap.cod-estabel 
                                                       + " Serie " + mov-ap.serie 
                                                       + " Titulo " + mov-ap.nr-docto.

        FIND FIRST tt-cross-reference EXCLUSIVE-LOCK
            WHERE tt-cross-reference.ep-codigo = mov-ap.ep-codigo
            AND   tt-cross-reference.ct-codigo = mov-ap.ct-cre-antec
            AND   tt-cross-reference.sc-codigo = mov-ap.sc-cre-antec NO-ERROR.
        IF AVAILABLE tt-cross-reference THEN
            ASSIGN tt-cross-reference.lancto-contas-pagar = YES
                   tt-cross-reference.observacao          = tt-cross-reference.observacao 
                                                       + "Empresa " + STRING(mov-ap.ep-codigo) 
                                                       + " Estab " + mov-ap.cod-estabel 
                                                       + " Serie " + mov-ap.serie 
                                                       + " Titulo " + mov-ap.nr-docto.

        FIND FIRST tt-cross-reference EXCLUSIVE-LOCK
            WHERE tt-cross-reference.ep-codigo = mov-ap.ep-codigo
            AND   tt-cross-reference.ct-codigo = mov-ap.ct-diversos
            AND   tt-cross-reference.sc-codigo = mov-ap.sc-diversos NO-ERROR.
        IF AVAILABLE tt-cross-reference THEN
            ASSIGN tt-cross-reference.lancto-contas-pagar = YES
                   tt-cross-reference.observacao          = tt-cross-reference.observacao 
                                                       + "Empresa " + STRING(mov-ap.ep-codigo) 
                                                       + " Estab " + mov-ap.cod-estabel 
                                                       + " Serie " + mov-ap.serie 
                                                       + " Titulo " + mov-ap.nr-docto.
    
        FIND FIRST tt-cross-reference EXCLUSIVE-LOCK
            WHERE tt-cross-reference.ep-codigo = mov-ap.ep-codigo
            AND   tt-cross-reference.ct-codigo = mov-ap.ct-frete
            AND   tt-cross-reference.sc-codigo = mov-ap.sc-frete NO-ERROR.
        IF AVAILABLE tt-cross-reference THEN
            ASSIGN tt-cross-reference.lancto-contas-pagar = YES
                   tt-cross-reference.observacao          = tt-cross-reference.observacao 
                                                       + "Empresa " + STRING(mov-ap.ep-codigo) 
                                                       + " Estab " + mov-ap.cod-estabel 
                                                       + " Serie " + mov-ap.serie 
                                                       + " Titulo " + mov-ap.nr-docto.
    
        FIND FIRST tt-cross-reference EXCLUSIVE-LOCK
            WHERE tt-cross-reference.ep-codigo = mov-ap.ep-codigo
            AND   tt-cross-reference.ct-codigo = mov-ap.ct-conta-cam
            AND   tt-cross-reference.sc-codigo = mov-ap.sc-conta-cam NO-ERROR.
        IF AVAILABLE tt-cross-reference THEN
            ASSIGN tt-cross-reference.lancto-contas-pagar = YES
                   tt-cross-reference.observacao          = tt-cross-reference.observacao 
                                                       + "Empresa " + STRING(mov-ap.ep-codigo) 
                                                       + " Estab " + mov-ap.cod-estabel 
                                                       + " Serie " + mov-ap.serie 
                                                       + " Titulo " + mov-ap.nr-docto.
    
        FIND FIRST tt-cross-reference EXCLUSIVE-LOCK
            WHERE tt-cross-reference.ep-codigo = mov-ap.ep-codigo
            AND   tt-cross-reference.ct-codigo = mov-ap.ct-conta-var
            AND   tt-cross-reference.sc-codigo = mov-ap.sc-conta-var NO-ERROR.
        IF AVAILABLE tt-cross-reference THEN
            ASSIGN tt-cross-reference.lancto-contas-pagar = YES
                   tt-cross-reference.observacao          = tt-cross-reference.observacao 
                                                       + "Empresa " + STRING(mov-ap.ep-codigo) 
                                                       + " Estab " + mov-ap.cod-estabel 
                                                       + " Serie " + mov-ap.serie 
                                                       + " Titulo " + mov-ap.nr-docto.

        FOR EACH tit-conta-ap NO-LOCK
             where tit-conta-ap.ep-codigo   = mov-ap.ep-codigo
             and   tit-conta-ap.cod-estabel = mov-ap.cod-estabel
             and   tit-conta-ap.cod-esp     = mov-ap.cod-esp
             and   tit-conta-ap.serie       = mov-ap.serie
             and   tit-conta-ap.nr-docto    = mov-ap.nr-docto
             and   tit-conta-ap.parcela     = mov-ap.parcela
             and   tit-conta-ap.cod-fornec  = mov-ap.cod-fornec:

            FIND FIRST tt-cross-reference EXCLUSIVE-LOCK 
                WHERE tt-cross-reference.ep-codigo = tit-conta-ap.ep-codigo
                AND   tt-cross-reference.ct-codigo = tit-conta-ap.ct-codigo
                AND   tt-cross-reference.sc-codigo = tit-conta-ap.sc-codigo NO-ERROR.
            IF AVAILABLE tt-cross-reference THEN
                ASSIGN tt-cross-reference.lancto-contas-pagar = YES
                       tt-cross-reference.observacao          = tt-cross-reference.observacao 
                                                           + "Empresa " + STRING(mov-ap.ep-codigo) 
                                                           + " Estab " + mov-ap.cod-estabel 
                                                           + " Serie " + mov-ap.serie 
                                                           + " Titulo " + mov-ap.nr-docto.
        END.

    END.

END PROCEDURE.

PROCEDURE pi-contas-receb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH titulo NO-LOCK
        WHERE (titulo.dt-emissao >= DATE(tt-param.data-ini)
        AND    titulo.dt-emissao <= DATE(tt-param.data-fim))
        OR     titulo.vl-saldo    > 0:
    
        RUN pi-acompanhar IN h-acomp(INPUT "Contas a Receber " + STRING(titulo.dt-emissao)).
    
        FIND FIRST tt-cross-reference EXCLUSIVE-LOCK
            WHERE tt-cross-reference.ep-codigo = titulo.ep-codigo
            AND   tt-cross-reference.ct-codigo = titulo.ct-conta-cr
            AND   tt-cross-reference.sc-codigo = titulo.sc-conta-cr NO-ERROR.
        IF AVAILABLE tt-cross-reference THEN
            ASSIGN tt-cross-reference.lancto-contas-receber = YES
                   tt-cross-reference.observacao            = tt-cross-reference.observacao 
                                                         + "Empresa " + STRING(titulo.ep-codigo) 
                                                         + " Estab " + titulo.cod-estabel 
                                                         + " Serie " + titulo.serie 
                                                         + " Titulo " + titulo.nr-docto.
    END.

    FOR EACH mov-tit NO-LOCK
        WHERE mov-tit.dt-trans >= DATE(tt-param.data-ini)
        AND   mov-tit.dt-trans <= DATE(tt-param.data-fim):

        RUN pi-acompanhar IN h-acomp(INPUT "Contas a Receber Movto " + STRING(mov-tit.dt-trans)).

        FIND FIRST tt-cross-reference EXCLUSIVE-LOCK
            WHERE tt-cross-reference.ep-codigo = mov-tit.ep-codigo
            AND   tt-cross-reference.ct-codigo = mov-tit.ct-conta-cr
            AND   tt-cross-reference.sc-codigo = mov-tit.sc-conta-cr NO-ERROR.
        IF AVAILABLE tt-cross-reference THEN
            ASSIGN tt-cross-reference.lancto-contas-receber = YES
                   tt-cross-reference.observacao            = tt-cross-reference.observacao 
                                                         + "Empresa " + STRING(mov-tit.ep-codigo) 
                                                         + " Estab " + mov-tit.cod-estabel 
                                                         + " Serie " + mov-tit.serie 
                                                         + " Titulo " + mov-tit.nr-docto.
    END.

END PROCEDURE.

PROCEDURE pi-cria-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    def input param p-ccusto     as char no-undo.
    def input param p-desc-custo as char no-undo.

    def var c-id as char no-undo.

    assign c-id = empresa.cod_empresa + cta_ctbl.cod_cta_ctbl + "00" + p-ccusto.
    find first tt-conta-contab no-lock
         where tt-conta-contab.id = c-id no-error.
    if  not avail tt-conta-contab then do:
        find first trad_org_ext no-lock
             where trad_org_ext.cod_matriz_trad_org_ext = 'GERAL'
               and trad_org_ext.cod_unid_organ          = empresa.cod_empresa no-error.
        if  not avail trad_org_ext then next.

        find first trad_org_ext no-lock
             where trad_org_ext.cod_matriz_trad_org_ext = "EMS2"
               and trad_org_ext.cod_tip_unid_organ      = "998" /* Empresa */
               and trad_org_ext.cod_unid_organ          = empresa.cod_empresa no-error.
        if  not avail trad_org_ext then next.

        create tt-conta-contab.
        assign tt-conta-contab.ep-codigo      = trad_org_ext.cod_unid_organ_ext
               tt-conta-contab.cod_empresa    = empresa.cod_empresa
               tt-conta-contab.cod_cta_ctbl   = cta_ctbl.cod_cta_ctbl
               tt-conta-contab.desc-conta     = cta_ctbl.des_tit_ctbl
               tt-conta-contab.cod_unid_negoc = "00"
               tt-conta-contab.cod_ccusto     = p-ccusto
               tt-conta-contab.desc-ccusto    = p-desc-custo
               tt-conta-contab.id             = c-id.
    end.
END PROCEDURE.

PROCEDURE pi-create-tt-conta-contab :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    for each cta_ctbl no-lock use-index ctactbl_id
       where cta_ctbl.cod_plano_cta_ctbl = "CONTSOC"
         and cta_ctbl.cod_cta_ctbl      >= tt-param.cod_cta_ctbl-ini
         and cta_ctbl.cod_cta_ctbl      <= tt-param.cod_cta_ctbl-fim
         and cta_ctbl.dat_inic_valid    <= today
         and cta_ctbl.dat_fim_valid     >= today:
        for each empresa no-lock use-index empresa_id
           where empresa.cod_empresa <> "CAN",
           first estabelecimento no-lock use-index stblcmnt_empresa
           where estabelecimento.cod_empresa = empresa.cod_empresa:
            find first criter_distrib_cta_ctbl no-lock use-index crtrdsta_id
                 where criter_distrib_cta_ctbl.cod_plano_cta_ctbl = cta_ctbl.cod_plano_cta_ctbl
                   and criter_distrib_cta_ctbl.cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl
                   and criter_distrib_cta_ctbl.cod_estab          = estabelecimento.cod_estab
                   and criter_distrib_cta_ctbl.dat_inic_valid    <= today
                   and criter_distrib_cta_ctbl.dat_fim_valid     >= today no-error.
            if  not avail criter_distrib_cta_ctbl or
                criter_distrib_cta_ctbl.ind_criter_distrib_ccusto = "N∆o Utiliza" then do:
                run pi-cria-tt(input "000000",
                               input "").
            end.
            else do:
                find first plano_ccusto no-lock use-index plnccst_id
                     where plano_ccusto.cod_empresa     = empresa.cod_empresa
                       and plano_ccusto.dat_inic_valid <= today
                       and plano_ccusto.dat_fim_valid  >= today no-error.

                if  criter_distrib_cta_ctbl.ind_criter_distrib_ccusto = "Utiliza Todos" then
                    for each ccusto no-lock use-index ccusto_id
                       where ccusto.cod_empresa      = plano_ccusto.cod_empresa
                         and ccusto.cod_plano_ccusto = plano_ccusto.cod_plano_ccusto
/*                          and ccusto.cod_ccusto       = */
                         and ccusto.dat_inic_valid  <= today
                         and ccusto.dat_fim_valid   >= today:
                        run pi-cria-tt(input ccusto.cod_ccusto,
                                       input ccusto.des_tit_ctbl).
                    end.
                else do:
                    for each item_lista_ccusto no-lock use-index itmlstcc_id
                       where item_lista_ccusto.cod_estab               = criter_distrib_cta_ctbl.cod_estab
                         and item_lista_ccusto.cod_mapa_distrib_ccusto = criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto
                         and item_lista_ccusto.cod_empresa             = criter_distrib_cta_ctbl.cod_empresa
                         and item_lista_ccusto.cod_plano_ccusto        = plano_ccusto.cod_plano_ccusto,
                        each ccusto no-lock use-index ccusto_id
                       where ccusto.cod_empresa      = item_lista_ccusto.cod_empresa
                         and ccusto.cod_plano_ccusto = item_lista_ccusto.cod_plano_ccusto
                         and ccusto.cod_ccusto       = item_lista_ccusto.cod_ccusto
                         and ccusto.dat_inic_valid  <= today
                         and ccusto.dat_fim_valid   >= today:
                        run pi-cria-tt(input ccusto.cod_ccusto,
                                       input ccusto.des_tit_ctbl).
                    end.
                end.
            end.
        end.
    end.
END PROCEDURE.

PROCEDURE pi-create-tt-cross-reference :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH tt-conta-contab NO-LOCK:
        RUN pi-acompanhar IN h-acomp(INPUT tt-conta-contab.cod_empresa + " - Conta : " + tt-conta-contab.cod_cta_ctbl).

        FIND FIRST estabelecimento NO-LOCK
             WHERE estabelecimento.cod_empresa     = tt-conta-contab.cod_empresa
               AND estabelecimento.log_estab_princ = YES NO-ERROR.
        IF  AVAIL estabelecimento THEN DO:
            FIND FIRST es-cross-reference-jde NO-LOCK
                 WHERE es-cross-reference-jde.Legacy-Company-Number = estabelecimento.cod_estab
                   AND es-cross-reference-jde.cod_cta_ctbl          = tt-conta-contab.cod_cta_ctbl
                   AND es-cross-reference-jde.cod_ccusto            = tt-conta-contab.cod_ccusto
                   AND es-cross-reference-jde.log-erro              = 0 NO-ERROR.
            IF  AVAIL es-cross-reference-jde THEN NEXT.
        END.

        CREATE tt-cross-reference.
        BUFFER-COPY tt-conta-contab TO tt-cross-reference.
        ASSIGN tt-cross-reference.ep-codigo      = tt-conta-contab.ep-codigo
               tt-cross-reference.ct-codigo      = tt-conta-contab.cod_cta_ctbl
               tt-cross-reference.sc-codigo      = tt-conta-contab.cod_ccusto
               tt-cross-reference.cod_unid_organ = tt-conta-contab.cod_empresa
               tt-cross-reference.des_tit_ctbl   = tt-conta-contab.desc-conta
               tt-cross-reference.des_ccusto     = tt-conta-contab.desc-ccusto.
    END.
END PROCEDURE.

PROCEDURE pi-faturamento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH ordem-inv no-lock
       WHERE ordem-inv.cod-situacao-inv = 1:
        FIND FIRST tt-cross-reference EXCLUSIVE-LOCK
             WHERE tt-cross-reference.ep-codigo = ordem-inv.ep-codigo
               AND tt-cross-reference.ct-codigo = ordem-inv.ct-codigo
               AND tt-cross-reference.sc-codigo = ordem-inv.sc-codigo NO-ERROR.
        IF  AVAIL tt-cross-reference THEN
            ASSIGN tt-cross-reference.lancto-faturamento = YES
                   tt-cross-reference.observacao         = tt-cross-reference.observacao 
                                                         + "Ordem Investimento " + STRING(ordem-inv.num-ordem).
    END.
END PROCEDURE.



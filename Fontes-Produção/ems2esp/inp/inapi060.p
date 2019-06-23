/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i INAPI060 2.00.00.007 } /*** 010007 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i inapi060 MIN}
&ENDIF

/****************************************************************************
** 
** Programa: inapi060.P
** 
** Objetivo: Valida?’o do movimento X conta informada
**
** Par?metros Entrada: cod-estabel: estabelecimento
**                     ct-codigo: conta
**                     sc-codigo: centro custo
**                     num-ordem-invest: ordem de investimento
** 
** Par?metros Sa­da  : tt-erro: temp-table de erro
**
** Autor...: Datasul S.A.
**
****************************************************************************/
{cdp/cd0669.i} /* defini?’o tt-erro */
{utp/ut-glob.i}
{cdp/cdcfgmat.i}

DEFINE VARIABLE c-modulo AS CHARACTER NO-UNDO INITIAL "".

DEFINE VARIABLE c-programa AS CHARACTER FORMAT "X(2000)" NO-UNDO.

ASSIGN c-programa = program-name(01) + " " +                                                                                      
                    program-name(02) + " " +                                                                           
                    program-name(03) + " " +                                                                          
                    program-name(04) + " " +                                                                 
                    program-name(05) + " " +                                                                      
                    program-name(06) + " " +                                                                                      
                    program-name(07) + " " +                                                                      
                    program-name(08) + " " +                                                                                      
                    program-name(09) + " " +                                                                           
                    program-name(10) + " " +                                                                          
                    program-name(11) + " " +                                                                 
                    program-name(12) + " " +                                                                      
                    program-name(13) + " ".

IF index(c-programa,"abp/ab") <> 0 THEN
   ASSIGN c-modulo = "ABP".


DEF INPUT  PARAMETER c-esta-exec      AS CHARACTER NO-UNDO.
DEF INPUT  PARAMETER ct-codigo        AS CHARACTER NO-UNDO.
DEF INPUT  PARAMETER sc-codigo        AS CHARACTER NO-UNDO.
DEF INPUT  PARAMETER i-num-ordem      AS INTEGER   NO-UNDO.
DEF INPUT  PARAMETER l-log-valida     AS LOGICAL   NO-UNDO.
DEF OUTPUT PARAMETER TABLE FOR tt-erro.

def temp-table tt_log_erro no-undo
    field ttv_num_cod_erro  as integer   format ">>>>,>>9" label "Nœmero"         column-label "Nœmero"
    field ttv_des_msg_ajuda as character format "x(40)"    label "Mensagem Ajuda" column-label "Mensagem Ajuda"
    field ttv_des_msg_erro  as character format "x(60)"    label "Mensagem Erro"  column-label "Inconsist?ncia".

DEF VAR h_api_cta_ctbl  AS HANDLE  NO-UNDO.
DEF VAR l-valida        AS LOGICAL NO-UNDO.
&IF "{&mguni_version}" >= "2.071" &THEN
DEF VAR i-empresa  LIKE ems2cadme.empresa.ep-codigo NO-UNDO.
&ELSE
DEF VAR i-empresa  AS INTEGER NO-UNDO.
&ENDIF

run prgint/utb/utb743za.py persistent set h_api_cta_ctbl.


FOR EACH tt-erro:
    DELETE tt-erro.
END.

if c-esta-exec = "0" then
    assign i-empresa = i-ep-codigo-usuario.
else do:
    find first estabelec no-lock 
         where estabelec.cod-estabel = c-esta-exec no-error.
    assign i-empresa = estabelec.ep-codigo when avail estabelec.
end.
FIND FIRST param-inv NO-LOCK 
     WHERE param-inv.ep-codigo = i-empresa NO-ERROR.

IF NOT AVAIL(param-inv) THEN DO:
   IF  VALID-HANDLE(h_api_cta_ctbl) THEN DO:
        DELETE PROCEDURE h_api_cta_ctbl NO-ERROR.
        ASSIGN h_api_cta_ctbl = ?.
    END. 
   RETURN "OK":U.
END.    

run valida_cta_ctbl_integr_outros_modulos_ems2 (input 2).

if can-find(tt_log_erro) then
    RETURN "OK":U.


IF  l-log-valida THEN
    ASSIGN l-valida = YES.
ELSE DO:
    &IF "{&bf_mat_versao_ems}" >= "2.06" &THEN
        IF  param-inv.log-valid-cta THEN
            ASSIGN l-valida = YES.
    &ELSE
        &IF "{&bf_mat_versao_ems}" >= "2.04" &THEN
            IF  SUBSTRING(param-inv.char-1,1,1) = "1" THEN
                ASSIGN l-valida = YES.
        &ENDIF
    &ENDIF
END.

IF c-modulo = "ABP" THEN l-valida = NO.
    
IF l-valida THEN DO:
    IF i-num-ordem = 0 THEN DO:
        run valida_cta_ctbl_integr_outros_modulos_ems2 (input 2).
        IF CAN-FIND(tt_log_erro) THEN DO:
            CREATE tt-erro.
            ASSIGN tt-erro.i-sequen = 1
                   tt-erro.cd-erro  = 28723
                   tt-erro.mensagem = "Conta " + ct-codigo + " ? de Investimentos."
                   tt-erro.c-param  = ct-codigo.
  
            RETURN "NOK":U.
        END.
    END.
    ELSE DO:
        run valida_cta_ctbl_integr_outros_modulos_ems2 (input 1).
        IF CAN-FIND(tt_log_erro) THEN DO:
            CREATE tt-erro.
            ASSIGN tt-erro.i-sequen = 1
                   tt-erro.cd-erro  = 28722
                   tt-erro.mensagem = "Conta " + ct-codigo + " n’o ? de Investimentos."
                   tt-erro.c-param  = ct-codigo.
                       
            RETURN "NOK":U.
        END.
    END.
END.
ELSE
   RETURN "OK":U.
   

/***** PROCEDURE *****/

procedure valida_cta_ctbl_integr_outros_modulos_ems2:
    define input param cod-finalidade as integer   no-undo.
    
    DEFINE VARIABLE v_cod_finalid     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v_cod_modul       AS CHARACTER NO-UNDO.

    /* FINALIDADES PARA OS OUTROS M…DULOS DO EMS2 
       1 - N’o gera lan?amentos / Investimento
       2 - Gera lan?amentos     / Investimento
    */
    assign v_cod_finalid = {adinc/i02ad049.i 4 cod-finalidade}. /* 2 - Gera lan?amentos */

   /* M…DULO POSS™VEIS  
       INP - Investimento 
       FTP - Faturamento
       OFP - Obriga?„es Fiscais
       RHP - Recursos Humanos
   */
   assign v_cod_modul = "INP".
   
   
   
   run prgint/utb/utb743za.py persistent set h_api_cta_ctbl.
   
   run pi_valida_cta_ctbl_integr in h_api_cta_ctbl (input  i-empresa,          /* EMPRESA EMS2 */
                                                    input  v_cod_modul,        /* MODULO */
                                                    input  "",                 /* PLANO CONTAS */
                                                    input  ct-codigo,          /* CONTA */
                                                    input  v_cod_finalid,      /* FINALIDADES */
                                                    input  today,              /* DATA DE TRANSACAO */  
                                                    output table tt_log_erro). /* ERROS */
                                                    
   IF  VALID-HANDLE(h_api_cta_ctbl) THEN DO:
        DELETE PROCEDURE h_api_cta_ctbl NO-ERROR.
        ASSIGN h_api_cta_ctbl = ?.
   END.                                                 

end procedure.

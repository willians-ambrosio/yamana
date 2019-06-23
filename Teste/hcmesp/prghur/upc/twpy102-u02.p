/*****************************************************************************************
***
**       Programa: prghur/upc/twpy102-u02.p
**
**       Data....: Out/2018
**
**       Cliente.: YAMANA
**
**       Autor...: Willians Ambrosio - Grupo DKP
**
**       Objetivo: Envio de Email x Alteraá∆o de Cargo
******************************************************************************************/
{include/buffers_RH.i}

{include/i-prgvrs.i twpy102-u02 12.1.21.001}

DEFINE INPUT PARAMETER p_rwd_funcionario   AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER p-mensagem          AS CHARACTER NO-UNDO.

 /*Include*/
{utp/ut-glob.i}
/* {include/tt-edit.i} */
/* {include/pi-edit.i} */

def var v_remetente_email                    like rh_pessoa_fisic.nom_e_mail     no-undo.
def VAR v_retorno                            as CHAR NO-UNDO.
def VAR v_grupo_contato                      as CHAR NO-UNDO.
def VAR v_mensagem                           as CHAR NO-UNDO.

def VAR v_cdn_avpes_pdr_relat                like avpes_emitid.cdn_avpes_padr    no-undo.
def VAR v_dat_refer1                         as date                             no-undo.
def buffer b_cargo                           for cargo.
def buffer b_niv_cargo                       for niv_cargo. 
def buffer b_rh_estab                        for rh_estab.
def buffer b_funcionario                     for funcionario.
def var v_dat_admis_func                     LIKE funcionario.dat_admis_func     no-undo.
def var v_cdn_empresa                        LIKE funcionario.cdn_empresa        no-undo.
def var v_cod_unid_lotac                     LIKE funcionario.cod_unid_lotac     no-undo.
def var v_cod_rh_ccusto                      LIKE funcionario.cod_rh_ccusto      no-undo.
def var v_dat_desligto_func                  LIKE funcionario.dat_desligto_func  no-undo.
def var v_cdn_cargo_basic                    LIKE funcionario.cdn_cargo_basic    no-undo.
def var v_cdn_estab_avaliador                LIKE funcionario.cdn_estab          no-undo.
def var v_cdn_funcionario_avaliador          LIKE funcionario.cdn_funcionario    no-undo.
def var v_remetente                          as char format "x(40)"              no-undo.
def var v_cdn_campo_impres like inform_impres_anexo.cdn_campo_impres_avpes       no-undo. 



/*Include*/
{utp/utapi019.i}
run utp/utapi019.p persistent set h-utapi019.
/*--------------Pega Usuario Logado no Sistema------------------------------*/
FIND FIRST usuar_mestre NO-LOCK WHERE 
           usuar_mestre.cod_usuario = v_cod_usuar_corren NO-ERROR.
IF avail usuar_mestre and usuar_mestre.cod_e_mail_local <> "" AND usuar_mestre.cod_e_mail_local <> ?  THEN
    ASSIGN v_remetente_email = cod_e_mail_local.
ELSE DO:
   FIND FIRST usuar_mestre_ext
        WHERE usuar_mestre_ext.cod_usuario = v_cod_usuar_corren
        NO-LOCK NO-ERROR.
   IF AVAILABLE(usuar_mestre_ext) THEN DO:
      ASSIGN v_remetente_email = usuar_mestre_ext.cod_usuar_so + "@yamana.com".
   END.
   ELSE
      ASSIGN v_retorno = "Endereáo Eletrìnico do Avaliador n∆o cadastrado". 
END.

FIND FIRST es_parametros 
     WHERE es_parametros.cod_prog_dtsul = "FP1620"          AND 
           es_parametros.cod_referencia = "ALTERACAO-CARGO" NO-LOCK NO-ERROR.
IF AVAIL es_parametros THEN 
   ASSIGN v_grupo_contato = TRIM(es_parametros.cod_parametro).
/*-------------------------------------------------------------------------*/
ASSIGN v_mensagem    = "".

FIND FIRST param_email NO-LOCK NO-ERROR.

create tt-envio2.
assign tt-envio2.versao-integracao = 1 
       tt-envio2.Servidor          = param_email.cod_servid_e_mail
       tt-envio2.Porta             = param_email.num_porta
       tt-envio2.Exchange          = param_email.log_servid_exchange
       tt-envio2.remetente         = v_remetente_email
       tt-envio2.destino           = v_grupo_contato
       tt-envio2.assunto           = "Departamento de TI - Tecnologia da Informaá∆o"
       tt-envio2.importancia       = 1
       tt-envio2.log-enviada       = no 
       tt-envio2.log-lida          = no 
       tt-envio2.acomp             = no 
       tt-envio2.mensagem          = v_mensagem
       tt-envio2.formato           = "txt".


FIND funcionario
  NO-LOCK
  WHERE ROWID(funcionario) = p_rwd_funcionario
  NO-ERROR.
IF NOT AVAIL funcionario THEN RETURN "NOK".
FIND FIRST rh_pessoa_fisic 
  NO-LOCK 
  WHERE rh_pessoa_fisic.num_pessoa_fisic = funcionario.num_pessoa_fisic
  NO-ERROR.
IF NOT AVAIL rh_pessoa_fisic THEN 
   RETURN "NOK".  

assign v_mensagem = "".

run pi-monta-mensagem. 

create tt-mensagem.
assign tt-mensagem.seq-mensagem = 1
       tt-mensagem.mensagem     = v_mensagem.

/*output to value(session:temp-directory + "envemail.txt").*/
run pi-execute2 in h-utapi019 (input table tt-envio2,
                               input table tt-mensagem,
                               output table tt-erros).

delete procedure h-utapi019.

PROCEDURE pi-monta-mensagem :
    assign v_mensagem = CHR(13) + CHR(13)  + CHR(13)
           v_mensagem = v_mensagem + "Ao"  + CHR(13) + 
                        "Departamento de TI - Tecnologia da Informaá∆o"  + CHR(13) +
                        "Informamos a alteraá∆o do cargo do(a) colaborador(a) abaixo, conforme segue:" + CHR(13) + CHR(13) + CHR(13) +
                        p-mensagem         + CHR(13) + CHR(13) + CHR(13) +
                        "Atenciosamente"   + CHR(13) +
                        "RH".    
END PROCEDURE.
/*-------------------------------------------------------------------------------------------*/

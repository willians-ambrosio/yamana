{include/buffers_RH.i}
{include/i-prgvrs.i TWPY267YMEP 1.00.00.000}
/*****************************************************************************************
***
**       Programa: prghur/upc/fp1500ymupa.p
**
**       Data....: Junho 2010
**
**       Cliente.: YAMANA
**       Objetivo: Envio de email

******************************************************************************************/
DEF INPUT PARAM p_rwd_funcionario   AS ROWID NO-UNDO.
DEF INPUT PARAM p_rwd_func_desligto AS ROWID NO-UNDO.

 /*Include*/
{utp/ut-glob.i}
/* {include/tt-edit.i} */
/* {include/pi-edit.i} */

def var v_remetente_email                    like rh_pessoa_fisic.nom_e_mail     no-undo.
def VAR v_retorno                            as CHAR NO-UNDO.
def VAR v_grupo_contato                            as CHAR NO-UNDO.
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
    FIND FIRST PARAM_desenv_pessoal NO-LOCK WHERE PARAM_desenv_pessoal.cdn_empresa = v_cdn_empres_usuar NO-ERROR.
    IF AVAIL PARAM_desenv_pessoal THEN DO:
        
        FIND FIRST funcionario no-lock WHERE
                   funcionario.cdn_empresa     = v_cdn_empres_usuar             AND
                   funcionario.cdn_estab       = PARAM_desenv_pessoal.cdn_estab AND
                   funcionario.cdn_funcionario = PARAM_desenv_pessoal.cdn_func_respos_avpes NO-ERROR.
        IF AVAIL funcionario THEN DO:
            
            FIND FIRST rh_pessoa_fisic NO-LOCK WHERE 
                       rh_pessoa_fisic.num_pessoa_fisic = funcionario.num_pessoa_fisic AND 
                       rh_pessoa_fisic.nom_e_mail       <> ""                          AND
                       rh_pessoa_fisic.nom_e_mail       <> ?   NO-ERROR.
            IF AVAIL rh_pessoa_fisic THEN
                ASSIGN v_remetente_email = rh_pessoa_fisic.nom_e_mail.
            ELSE
                ASSIGN v_retorno = "Endereáo Eletrìnico do Avaliador n∆o cadastrado".
        END.
    END.
END.


/*-------------------------------------------------------------------------*/


/*Verificar O codigo do Grupo de email*/
for each grp_mail_func WHERE grp_mail_func.cod_grp_mail = "func_desligto":
    find first rh_pessoa_fisic where 
               rh_pessoa_fisic.num_pessoa_fisic = grp_mail_func.num_pessoa_fisic no-lock no-error.
    if avail rh_pessoa_fisic then
        assign v_grupo_contato = v_grupo_contato + grp_mail_func.nom_mail_conta + ",".
END.
assign substring(v_grupo_contato,length(v_grupo_contato),01) = "".

ASSIGN v_mensagem    = "".

find first param_glob_rh no-lock no-error.
create tt-envio2.
assign tt-envio2.versao-integracao = 1 
       tt-envio2.Servidor          = trim(substr(param_glob_rh.cod_livre_2,01,40))
       tt-envio2.Porta             = param_glob_rh.num_livre_1
       tt-envio2.Exchange          = param_glob_rh.log_livre_1
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
IF NOT AVAIL rh_pessoa_fisic THEN RETURN "NOK".
find first avpes_emitid no-lock 
    where avpes_emitid.cdn_empresa       = funcionario.cdn_empresa    
    and   avpes_emitid.cdn_estab         = funcionario.cdn_estab      
    and   avpes_emitid.cdn_funcionario   = funcionario.cdn_funcionario 
    no-error.
if avail avpes_emitid then do:
   
   FIND func_desligto
     WHERE ROWID(func_desligto) = p_rwd_func_desligto
     NO-ERROR.

   IF AVAIL func_desligto THEN
     ASSIGN tem-avaliacao = YES.
   
end.    

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

    FIND FIRST func_desligto NO-LOCK
         WHERE func_desligto.cdn_empresa     = funcionario.cdn_empresa    
           AND func_desligto.cdn_estab       = funcionario.cdn_estab      
           AND func_desligto.cdn_funcionario = funcionario.cdn_funcionario NO-ERROR.


    assign 
      v_mensagem = CHR(13) + CHR(13) + CHR(13).
      v_mensagem = v_mensagem +
        "Ao" + CHR(13) +
        "Departamento de TI - Tecnologia da Informaá∆o"  + CHR(13) +
        "Informamos que o colaborador(a) abaixo foi desligado(a) da empresa, conforme segue:".
    assign 
      v_mensagem = v_mensagem + CHR(13) + CHR(13) + CHR(13) +
      "Empresa: " + funcionario.cdn_empresa  + CHR(13) +
      "Estabeleciento: " + funcionario.cdn_estab  + CHR(13) +
      "Funcion†rio: " + TRIM(STRING(funcionario.cdn_funcionario)) + " - " + rh_pessoa_fisic.nom_pessoa_fisic  + CHR(13) +
      "Data Desligamento: " + (IF AVAIL(func_desligto) THEN string(func_desligto.dat_desligto) ELSE "") + CHR(13) +
      "CPF: " + STRING(rh_pessoa_fisic.cod_id_feder) + CHR(13) + CHR(13) + CHR(13) +
      "Atenciosamente"   + CHR(13) +
      "RH".
    

END PROCEDURE.
/*-------------------------------------------------------------------------------------------*/

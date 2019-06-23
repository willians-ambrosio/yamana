/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/buffers_RH.i}

{include/i-prgvrs.i TWPY267YMEP 1.00.00.000}
/*****************************************************************************************
***
**       Programa: prghur/upc/twpy267ymep.p
**
**       Data....: Junho 2010
**
**       Cliente.: YAMANA
**       Objetivo: Envio de email

******************************************************************************************/

def parameter buffer p-table     for sit_afast_func.
def parameter buffer p-old-table for sit_afast_func.

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


{utp/ut-glob.i}
{utp/utapi019.i}

IF p-table.cdn_sit_afast_func = 45 THEN DO:
   RUN pi-envia-email.   
END.


PROCEDURE pi-envia-email:
   RUN utp/utapi019.p PERSISTENT SET h-utapi019.
   
   
   /*--------------Pega Usuario Logado no Sistema------------------------------*/
   FIND FIRST usuar_mestre 
        WHERE usuar_mestre.cod_usuario = v_cod_usuar_corren 
        NO-LOCK NO-ERROR.
   IF AVAILABLE(usuar_mestre) AND 
      usuar_mestre.cod_e_mail_local <> "" AND usuar_mestre.cod_e_mail_local <> ?  THEN
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
        WHERE es_parametros.cod_prog_dtsul = "FP1840" AND 
              es_parametros.cod_referencia = "FUNCIONARIO-TRANSFERIDO"
        NO-LOCK NO-ERROR.
   IF AVAILABLE es_parametros THEN DO:
      ASSIGN v_grupo_contato = TRIM(es_parametros.cod_parametro).
   END.
   
   FIND FIRST param_email NO-LOCK NO-ERROR.
   
   CREATE tt-envio2.
   ASSIGN tt-envio2.versao-integracao = 1 
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

/*    ASSIGN  tt-envio2.Servidor          = "ydmrelay.yamana.local"    */
/*            tt-envio2.Porta             = 25                         */
/*            tt-envio2.Exchange          = NO                         */
/*            tt-envio2.remetente         = "v-ssilveira@yamana.com"   */
/*            tt-envio2.destino           = "v-ssilveira@yamana.com" . */

   
   FIND FIRST funcionario
        WHERE funcionario.cdn_empresa     = p-table.cdn_empresa     AND
              funcionario.cdn_estab       = p-table.cdn_estab       AND
              funcionario.cdn_funcionario = p-table.cdn_funcionario 
        NO-LOCK NO-ERROR.
   IF NOT AVAILABLE(funcionario) THEN 
      RETURN "NOK".
   
   FIND FIRST rh_pessoa_fisic 
        WHERE rh_pessoa_fisic.num_pessoa_fisic = funcionario.num_pessoa_fisic
        NO-LOCK NO-ERROR.
   IF NOT AVAILABLE(rh_pessoa_fisic) THEN 
      RETURN "NOK".

   ASSIGN v_mensagem = "".
   
   RUN pi-monta-mensagem. 

/*    MESSAGE                                              */
/*        "tt-envio2.Servidor  " tt-envio2.Servidor   skip */
/*        "tt-envio2.Porta     " tt-envio2.Porta      skip */
/*        "tt-envio2.Exchange  " tt-envio2.Exchange   skip */
/*        "tt-envio2.remetente " tt-envio2.remetente  skip */
/*        "tt-envio2.destino   " tt-envio2.destino    skip */
/*        "v_mensagem" v_mensagem                          */
/*                                                         */
/*        VIEW-AS ALERT-BOX INFO BUTTONS OK.               */

   CREATE tt-mensagem.
   ASSIGN tt-mensagem.seq-mensagem = 1
          tt-mensagem.mensagem     = v_mensagem.
   
   /*output to value(session:temp-directory + "envemail.txt").*/
   RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                  INPUT  TABLE tt-mensagem,
                                  OUTPUT TABLE tt-erros).
   
   DELETE PROCEDURE h-utapi019.
END PROCEDURE.

PROCEDURE pi-monta-mensagem :
   ASSIGN v_mensagem = CHR(13) + CHR(13) + CHR(13).
     
   ASSIGN v_mensagem = v_mensagem + "Ao" + CHR(13) +
                                    "Departamento de TI - Tecnologia da Informaá∆o"  + CHR(13) +
                                    "Informamos que o colaborador(a) abaixo foi transferido(a) , conforme segue:".
   
   ASSIGN v_mensagem = v_mensagem + CHR(13) + CHR(13) + CHR(13) +
                                    "Empresa Origem: "          + funcionario.cdn_empresa                                                               + CHR(13) +
                                    "Estabelecimento Origem: "  + funcionario.cdn_estab                                                                 + CHR(13) +
                                    "Funcion†rio Origem: "      + TRIM(STRING(funcionario.cdn_funcionario)) + " - " + rh_pessoa_fisic.nom_pessoa_fisic  + CHR(13) + CHR(13) +
                                    "Empresa Destino: "         + p-table.cdn_empres_dest                                                               + CHR(13) + 
                                    "Estabelecimento Destino: " + p-table.cdn_estab_dest                                                                + CHR(13) + 
                                    "Funcion†rio Destino: "     + TRIM(STRING(p-table.cdn_func_dest)) + " - " + rh_pessoa_fisic.nom_pessoa_fisic        + CHR(13) + 
                                    "Data Transferencia: "      + STRING(p-table.dat_inic_proces_sit_afast,'99/99/9999')                                + CHR(13) + 
                                    "CPF: "                     + STRING(rh_pessoa_fisic.cod_id_feder)                                                  + CHR(13) + CHR(13) + CHR(13) +
                                    "Atenciosamente"            + CHR(13) +
                                    "RH".
   

END PROCEDURE.
/*-------------------------------------------------------------------------------------------*/




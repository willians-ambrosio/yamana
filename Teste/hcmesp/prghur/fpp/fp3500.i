/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

/*Include criada para Encripta‡Æo de arquivo zip e envio de e-mail utilizado 
pelos programas FP3500, FP3501 e FP2601*/
DEF NEW GLOBAL SHARED VAR c-seg-usuario        AS CHAR FORMAT "x(12)" NO-UNDO.

FOR EACH tt-envio2:
    DELETE tt-envio2.
END.

FOR EACH tt-listFiles:
    DELETE tt-listFiles.
END.

FOR EACH tt-erros:
    DELETE tt-erros.
END.

FOR EACH tt-erros-zip:
    DELETE tt-erros-zip.
END.

CREATE tt-listFiles.
ASSIGN tt-listFiles.cFile = tt-param.arquivo
       c_status_zip       = "OK".

/*Verifica se deve ser criado o arquivo zip com senha */
IF l-compactar THEN DO:
    RUN zipFilesEncrypt IN h-zip (INPUT c_anexo,
                                  INPUT TABLE tt-listFiles,
                                  INPUT c_senha,
                                  INPUT TRUE,
                                  OUTPUT TABLE tt-erros-zip).
    ASSIGN c_status_zip = RETURN-VALUE.
END.
else do:
    /* cria arquivo .zip sem senha */
    RUN zipFiles IN h-zip (INPUT c_anexo,
                           INPUT TABLE tt-listFiles,
                           INPUT TRUE,
                           OUTPUT TABLE tt-erros-zip).
    ASSIGN c_status_zip = RETURN-VALUE.
end.

/*Processo de envio de email*/
IF c_status_zip = "OK" THEN DO:
    IF NOT AVAIL param_glob_rh THEN
        FIND FIRST param_glob_rh NO-LOCK NO-ERROR.

find usuar_aplicat_rh no-lock where /* faz a procura da pessoa fisica do usuario logado pois necessita-se do e-mail para ser o remetente da mensagem */
     usuar_aplicat_rh.cod_usuario = c-seg-usuario no-error.
if avail usuar_aplicat_rh then do:
    find b2funcionario no-lock where 
         b2funcionario.cdn_empresa     = usuar_aplicat_rh.cdn_empresa and
         b2funcionario.cdn_estab       = usuar_aplicat_rh.cdn_estab   and
         b2funcionario.cdn_funcionario = usuar_aplicat_rh.cdn_funcionario no-error.
    if avail funcionario then
        find rh_pessoa_fisic no-lock where
             rh_pessoa_fisic.num_pessoa_fisic = b2funcionario.num_pessoa_fisic no-error.
    if avail rh_pessoa_fisic then
        assign v_remetente = trim(substring(rh_pessoa_fisic.cod_livre_1,26,40)).
end.
if v_remetente = "" then do:
    find first usuar_mestre no-lock where
               usuar_mestre.cod_usuario = c-seg-usuario no-error.
    if avail usuar_mestre then
        assign v_remetente = trim(usuar_mestre.cod_e_mail_local).
end.
    create tt-envio2.
    assign tt-envio2.versao-integracao = 1
           tt-envio2.servidor    = trim(substring(param_glob_rh.cod_livre_2,01,40))
           tt-envio2.porta       = param_glob_rh.num_livre_1
           tt-envio2.exchange    = param_glob_rh.log_livre_1
           tt-envio2.remetente   = v_remetente   
           tt-envio2.destino     = trim(c_destino)
           tt-envio2.assunto     = c_assunto
           tt-envio2.importancia = 2
           tt-envio2.log-enviada = NO
           tt-envio2.log-lida    = NO
           tt-envio2.acomp       = NO
           tt-envio2.arq-anexo   = TRIM(c_anexo)
           tt-envio2.formato     = "texto".
    
    run pi-execute2 in h-utapi019 (input  table tt-envio2,
                                   input  table tt-mensagem,
            	                   output table tt-erros).   
    /*l-ErroZip verifica qual a temp table deve ser lida caso haja erro.*/
    IF RETURN-VALUE = "NOK" THEN 
        ASSIGN l-ErroZip = NO.
END.
ELSE
    ASSIGN l-ErroZip = YES.

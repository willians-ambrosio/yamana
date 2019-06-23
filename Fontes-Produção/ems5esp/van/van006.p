/****************************************************************************************** 
** 	   Programa: van006.p
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 10/09/2018
** Change/Chamado: 
**      Objetivo: Consome webservice para comunicaá∆o com Van de Pagamentos - Accesstage
**               Funá‰es envia arquivos de remessa e busca retornos - baixas e extratos banc†rios
**                PROGRAMA EXECUTADO SEMPRE DE FORMA PERSISTENTE
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
/* Variables definitions */
DEFINE VARIABLE c_token AS CHARACTER NO-UNDO.
DEFINE VARIABLE c_user  AS CHARACTER NO-UNDO.
DEFINE VARIABLE c_url   AS CHARACTER NO-UNDO.
DEFINE VARIABLE i-seq   AS INTEGER   NO-UNDO.
DEFINE VARIABLE c_destino AS CHARACTER  NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR cTipoOrigem AS CHAR NO-UNDO.
DEFINE SHARED VAR r_row_webserver    AS ROWID NO-UNDO.
DEFINE SHARED VAR r_row_param_dir    AS ROWID NO-UNDO.

DEF TEMP-TABLE tt-arquivos
    FIELD cFile AS CHAR
    FIELD dTotal AS DEC
    FIELD cConta AS CHAR
    FIELD formaPg AS CHAR.

{utp/utapi019.i}
{utp/ut-glob.i}
{van/van006.i}
/* Definicao das Variaveis */
DEFINE VARIABLE ha-WebService       AS HANDLE NO-UNDO.
DEFINE VARIABLE hRetornoRemessaSoap AS HANDLE NO-UNDO.

DEFINE VARIABLE cfileremessa        AS LONGCHAR NO-UNDO.
DEFINE VARIABLE l-ok                AS LOGICAL  NO-UNDO.

/* Vari†veis para a manipulaá∆o dos arquivos */
DEFINE VARIABLE c_enviados AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c_pendente AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c_erro     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c_cod_intercambio AS INT  NO-UNDO.

/* Utilizada para o envio de arquivo remessa */
DEF TEMP-TABLE tt-arquivo
  FIELD  arquivo AS CHAR.

/* Lista dos arquivos Remessa */
DEFINE TEMP-TABLE tt-listFiles NO-UNDO
    FIELD cFile     AS CHAR    FORMAT "X(100)"
    FIELD lSearch   AS LOGICAL.


/* Tempor†ria de erros */
DEFINE TEMP-TABLE tt-erro
    FIELD iseq  AS INT
    FIELD cmsg  AS CHAR
    FIELD cproc AS CHAR.

/* Temp-table e Dataset para envio de msg */

DEFINE TEMP-TABLE EnviaRemessaResult NO-UNDO
    XML-NODE-NAME "EnviaRemessaResult"
	FIELD codIntercambio    AS CHARACTER 
	FIELD dscStatusMensagem AS CHARACTER 
	FIELD dscErroEnvio      AS CHARACTER.

DEFINE DATASET EnviaRemessaResultDset 
    NAMESPACE-URI "http://www.accesstage.com.br/ASTrafegoWS/EnvioMensagemResponse" 
	XML-NODE-TYPE "HIDDEN" 
	FOR EnviaRemessaResult.

DEFINE TEMP-TABLE RetornoRecuperacaoMensagensResul NO-UNDO
	NAMESPACE-URI "http://www.accesstage.com.br/ASTrafegoWS/RecuperacaoMensagemResponse" 
	XML-NODE-NAME "RetornoRecuperacaoMensagensResult" 
	FIELD dscConteudoMensagem AS RAW 
	FIELD flgCompactacao      AS LOGICAL 
	FIELD dscStatusMensagem   AS CHARACTER 
	FIELD dscErroEnvio        AS CHARACTER.
    
DEFINE DATASET RetornoRecuperacaoMensagensRDset 
  NAMESPACE-URI "http://www.accesstage.com.br/ASTrafegoWS/ConfirmacaoRetiradaResponse" 
    XML-NODE-TYPE "HIDDEN" 
    FOR RetornoRecuperacaoMensagensResul.

/* Utilizada para listar as mensagens retornadas da Accesstage */
DEFINE TEMP-TABLE RetornoListaMensagensResponse
    XML-NODE-NAME "RetornoListaMensagensResponse"
    FIELD Id AS INT XML-NODE-TYPE "HIDDEN"
    INDEX idx1 Id. 

DEFINE TEMP-TABLE RetornoListaMensagensResult
        XML-NODE-NAME "RetornoListaMensagensResult"
        FIELD Id   AS INT XML-NODE-TYPE "HIDDEN"
    INDEX Idx1 Id.

DEF TEMP-TABLE ListaMsgDisponiveisMsgDisponivel 
     XML-NODE-NAME "ListaMsgDisponiveisMsgDisponivel" 
        FIELD trkIdIn          AS INT64
        FIELD tipoDocumento    AS CHAR 
        FIELD nmeEmpresaOrigem AS CHAR 
        FIELD dtaHoraDisp      AS CHAR     
        FIELD totalBytes       AS INT64.

DEFINE TEMP-TABLE RecuperacaoConteudoMensagemResul NO-UNDO
	XML-NODE-NAME "RecuperacaoConteudoMensagemResult" 
	FIELD dscConteudoMensagem AS RAW 
	FIELD flgCompactacao      AS LOGICAL 
	FIELD dscStatusMensagem   AS CHARACTER 
	FIELD dscErroEnvio        AS CHARACTER.

DEFINE DATASET RecuperacaoConteudoMensagemRDset 
    NAMESPACE-URI "http://www.accesstage.com.br/ASTrafegoWS/RecuperacaoMensagemResponse" 
	XML-NODE-TYPE "HIDDEN" 
	FOR RecuperacaoConteudoMensagemResul.

DEFINE TEMP-TABLE ConfirmacaoRetiradaMensagemResul NO-UNDO
    XML-NODE-NAME "ConfirmacaoRetiradaMensagemResult" 
    FIELD dataHoraRetirada AS CHARACTER 
    FIELD dscStatusRetirada AS CHARACTER.

DEFINE DATASET ConfirmacaoRetiradaMensagemRDset 
        NAMESPACE-URI "http://www.accesstage.com.br/ASTrafegoWS/ConfirmacaoRetiradaResponse" 
    XML-NODE-TYPE "HIDDEN" 
    FOR ConfirmacaoRetiradaMensagemResul.

DEFINE TEMP-TABLE tt-SoapFault
    FIELD cDescErro AS CHAR.

DEFINE DATASET hDSet
    XML-NODE-TYPE "HIDDEN" 
    FOR tt-SoapFault.

FUNCTION RetornoListaMensagens RETURNS LONGCHAR
  IN hRetornoRemessaSoap.

/* In°cio - Leitura dos arquivos - Remessa*/
/* Rotina para envio de remessas */
/*RUN pi-remessa.*/
        
/* Verifica mensagens dispon°veis */
/*RUN pi-listamsg.*/

/* PROCEDURES DO PROGRAMA *************************************************************************/

/* Busca Webservice */
PROCEDURE pi-buscawebservice:
    
    DEFINE VARIABLE c-erro AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE iError AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cError AS CHARACTER NO-UNDO.

    /* Get Token and password */
    FIND FIRST es_param_webserver NO-LOCK 
        WHERE ROWID(es_param_webserver) = r_row_webserver NO-ERROR.
    IF NOT AVAIL es_param_webserver THEN DO:

        RUN pi-erro (INPUT "N∆o encontrada tabela de parÉmetros de conex∆o DO Web Service. Verificar o cadastro VAN001-W01",
                     INPUT es_param_webserver.nomeserv).

        RETURN "NOK".
    END.
   
    ASSIGN c_token    = es_param_webserver.token 
           c_user     = es_param_webserver.usu
           c_url      = es_param_webserver.url.
  
    /* Cria conexao ou exibe msg de erro */
    CREATE SERVER ha-WebService.
    ha-WebService:CONNECT(" -WSDL '" + c_url + "'") NO-ERROR.
    
    IF NOT ha-WebService:CONNECTED() 
    THEN DO:
        ASSIGN c-erro = TRIM(ERROR-STATUS:GET-MESSAGE(1)).
        DELETE OBJECT ha-WebService NO-ERROR.

        RUN pi-erro (INPUT SUBSTITUTE("Falha ao conectar no webservice! (&1)", c-erro),
                     INPUT "Conecta WebService").

        RETURN "NOK".
    END.
    ELSE RUN RetornoRemessaSoap SET hRetornoRemessaSoap ON ha-WebService. 
    
    RETURN "OK".
  
END PROCEDURE.

PROCEDURE pi-listamsg:

    DEFINE VARIABLE RetornoListaMensagensResult AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE rPwd AS RAW NO-UNDO.
    
    /* Conecta webservice */
    FIND FIRST  es_param_webserver NO-LOCK 
        WHERE es_param_webserver.identificador = "02"
          AND es_param_webserver.nomeserv      = "RetornoListaMensagens" NO-ERROR. 
    ASSIGN r_row_webserver = ROWID(es_param_webserver).
    
    RUN pi-buscawebservice.
    IF RETURN-VALUE <> "OK" THEN DO:

        RUN pi-erro (INPUT RETURN-VALUE,
                     INPUT es_param_webserver.nomeserv).

        RETURN "NOK".
    END.
    
    /* Function invocation of RetornoListaMensagens operation. */
    RetornoListaMensagensResult = RetornoListaMensagens() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO: 
        RUN pi-retornoerro (INPUT "RetornoListaMensagensResult").
        RETURN "nok".
    END.

    /* Procedure invocation of RetornoListaMensagens operation. */
    RUN RetornoListaMensagens IN hRetornoRemessaSoap(OUTPUT RetornoListaMensagensResult) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO: 
        RUN pi-retornoerro (INPUT "RetornoListaMensagens").

        RETURN "NOK".

    END.

    RUN pi-RetornoRecuperacaoMensagens (INPUT RetornoListaMensagensResult).

    RUN pi-disconecta.

    IF CAN-FIND(FIRST tt-erro) THEN
            RUN pi-enviaemail.
END.

PROCEDURE pi-remessa:
                    
    DEFINE INPUT PARAMETER TABLE FOR tt-listFiles.

    DEFINE VARIABLE c_arq_base64 AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE c_arq_mptr   AS MEMPTR   NO-UNDO.
    DEFINE VARIABLE c_arq_raw    AS RAW      NO-UNDO.
    DEFINE VARIABLE c_arq_data   AS LONGCHAR NO-UNDO.

    DEFINE VARIABLE codIntercambio AS CHARACTER NO-UNDO.
    DEFINE VARIABLE conteudoMensagem AS RAW NO-UNDO.

    /* Conecta webservice */
    RUN pi-buscawebservice.
    IF RETURN-VALUE <> "OK" THEN DO:
       RUN pi-erro (INPUT RETURN-VALUE,
                    INPUT "EnviaRemessa").
       RETURN "NOK".
    END.

    FIND es_param_van_dir NO-LOCK 
        WHERE ROWID(es_param_van_dir) = r_row_param_dir NO-ERROR.
    IF AVAIL es_Param_van_dir THEN
    ASSIGN c_enviados        = es_param_van_dir.dir_sucesso  
           c_pendente        = es_param_van_dir.dir_remessa             
           c_erro            = es_param_van_dir.dir_erro             
           c_cod_intercambio = es_param_van_dir.psdid.

    /* Processa remessas */
    /* Faz um envio para cada arquivo */
    FOR EACH tt-listFiles:

        EMPTY TEMP-TABLE EnviaRemessaResult.

        IF SEARCH(tt-listFiles.cFile) = ? THEN NEXT.

        RUN pi-arqRaw (INPUT tt-listFiles.cFile,
                       OUTPUT c_Arq_Data).
        
        ASSIGN c_destino = c_enviados + SUBSTR(tt-listFiles.cFile,LENGTH(c_pendente) + 1,LENGTH(tt-listFiles.cFile)).
                         
        RUN pi-enviaremessa (INPUT c_Arq_Data,
                             INPUT tt-listFiles.cFile).  /* Conte£do do Arquivo */
    END.

    RUN pi-disconecta.

    /* Envia log de erro */
    IF CAN-FIND(FIRST tt-erro) THEN DO:

        RUN pi-enviaemail.
        RETURN "ERROR".
    END.
    ELSE RETURN "ok".
END.

PROCEDURE pi-enviaremessa:
    DEFINE INPUT  PARAMETER conteudoMensagem AS LONGCHAR NO-UNDO. /* RAW */
    DEFINE INPUT  PARAMETER ipc_file AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE rTest AS RAW        NO-UNDO.
                      
    DEFINE VARIABLE c_retorno AS LONGCHAR   NO-UNDO.
    DEFINE VARIABLE l-retok   AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE cIdMovto  AS CHARACTER  NO-UNDO.
   
    RUN EnviaRemessa IN hRetornoRemessaSoap(INPUT c_cod_Intercambio, 
                                            INPUT STRING(conteudoMensagem), 
                                            OUTPUT TABLE EnviaRemessaResult).
    IF ERROR-STATUS:ERROR THEN DO:
        RUN pi-retornoErro (INPUT "EnviaRemessa").
        RETURN "NOK".
    END.
            
    /* O retorno do Dataset j† s∆o os dados da tempor†ria */
    FOR EACH EnviaRemessaResult:
        
        IF EnviaRemessaResult.dscStatusMensagem = "OK" THEN DO:
            
            ASSIGN c_destino = c_enviados + SUBSTR(ipc_file,LENGTH(c_pendente) + 1,LENGTH(ipc_file)). 

            /* Copiar arquivo da pasta a enviar para a pasta enviado */ 
            OS-COMMAND SILENT MOVE VALUE(ipc_file) VALUE(c_destino).
            /* Se arquivo de transferencia entre contas, atualiza a tabela */
            /* Extrai o nr do Id do movimento e grava a remessa como enviada*/
            IF ipc_file MATCHES "*Transf_Conta*" THEN DO TRANS:

                ASSIGN cIdMovto = SUBSTR(ipc_file,R-INDEX(ipc_file,"_") + 1,R-INDEX(ipc_file,".") - R-INDEX(ipc_file,"_") - 1).
                              
                FIND FIRST es_rem_movto_cta_corren WHERE
                           es_rem_movto_cta_corren.num_id_movto_cta_corren = INT(cIdMovto) NO-ERROR.
                IF AVAIL es_rem_movto_cta_corren 
                    THEN ASSIGN es_rem_movto_cta_corren.arq_gerado = YES.
            END.
            NEXT.
        END.

        /* Gera Erro e copia o arquivo para o diret¢rio de erro */
        RUN pi-erro (INPUT EnviaRemessaResult.dscErroEnvio,
                     INPUT "EnviaRemessaResult").

        ASSIGN c_destino = c_erro + SUBSTR(ipc_file,LENGTH(c_pendente) + 1,LENGTH(ipc_file)).

        /* Copia Arquivo para a Pasta de Erro */
        OS-COMMAND SILENT MOVE VALUE(ipc_file) VALUE(c_destino).
            
    END.
END.

PROCEDURE pi-retornoRecuperacaoMensagens:
    
    DEFINE INPUT  PARAMETER ipc-RetornoListaMsg AS LONGCHAR NO-UNDO.
    
    DEFINE VARIABLE lRetorno  AS LOG       NO-UNDO.
    DEFINE VARIABLE c-dtRet   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE c-arquivo AS CHARACTER NO-UNDO.

    lRetorno = TEMP-TABLE ListaMsgDisponiveisMsgDisponivel:READ-XML("LONGCHAR", ipc-RetornoListaMsg, "EMPTY", ?, ?, ?, ?) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        RUN pi-retornoerro (INPUT "ListaMsgDisponiveisMsgDisponivel:READ-XML").

        RETURN "NOK".
    END.
    
    blk:
    FOR EACH ListaMsgDisponiveisMsgDisponivel:
/*          WHERE                                                      */
/*         ListaMsgDisponiveisMsgDisponivel.trkIdIn = 201810268289663: */

        /* Caso esteja sendo executado a partir da folha n∆o baixa os arquivos financeiros */
        IF ListaMsgDisponiveisMsgDisponivel.tipoDocumento <> "FOLHA DE PAGAMENTO" AND cTipoOrigem = "HCM" THEN NEXT.

        /* Caso esteja sendo executado a partir do ERP n∆o baixa os arquivos financeiros */
        IF ListaMsgDisponiveisMsgDisponivel.tipoDocumento = "FOLHA DE PAGAMENTO" AND cTipoOrigem <> "HCM" THEN NEXT.

           
         EMPTY TEMP-TABLE RecuperacaoConteudoMensagemResul.
    
        RUN RecuperacaoConteudoMensagem IN hRetornoRemessaSoap(INPUT ListaMsgDisponiveisMsgDisponivel.trkIdIn, 
                                                               OUTPUT TABLE RecuperacaoConteudoMensagemResul) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            RUN pi-retornoErro (INPUT "RecuperacaoConteudoMensagem").
            NEXT blk.
        END.
        
        blk_RecMsg:
        FOR EACH RecuperacaoConteudoMensagemResul:

           EMPTY TEMP-TABLE ConfirmacaoRetiradaMensagemResul.

           IF RecuperacaoConteudoMensagemResul.dscStatusMensagem = "OK" THEN DO:
                   
               /* Cria o arquivo recebido nas pastas parametrizadas */
               RUN van\van002.p (INPUT RecuperacaoConteudoMensagemResul.flgCompactacao,
                                 INPUT RecuperacaoConteudoMensagemResul.dscConteudoMensagem,
                                 INPUT ListaMsgDisponiveisMsgDisponivel.tipoDocumento,   
                                 INPUT ListaMsgDisponiveisMsgDisponivel.nmeEmpresaOrigem,
                                 INPUT ListaMsgDisponiveisMsgDisponivel.trkIdIn,
                                 INPUT 0, /*RecuperacaoConteudoMensagemResul.psdid, - Liberar qdo o param estiver retornando da accesstage*/
                                 OUTPUT l-ok,
                                 OUTPUT c-arquivo).
               
               IF NOT l-ok OR RETURN-VALUE <> "OK" THEN DO:
                   RUN pi-erro (INPUT "Falha ao gravar arquivo TrackId " + STRING(ListaMsgDisponiveisMsgDisponivel.trkIdIn),
                                INPUT "Verifique a convers∆o do arquivo no programa van002.p e parÉmetros van001-w01").
                   NEXT blk_RecMsg.

               END.
               ELSE DO:

                  /* Realiza a chamada de retorno e baixa dos arquivos */ 
                  ASSIGN c-dtRet = STRING(YEAR(TODAY),'9999') + "-" + STRING(MONTH(TODAY),'99') + "-" + STRING(DAY(TODAY),'99') + "T" + STRING(TIME,'hh:mm:ss') + ".000-03:00". 
                  
/*                   RUN ConfirmacaoRetiradaMensagem IN hRetornoRemessaSoap(INPUT ListaMsgDisponiveisMsgDisponivel.trkIdIn,          */
/*                                                                          INPUT c-arquivo,                                         */
/*                                                                          INPUT c-dtRet,                                           */
/*                                                                          OUTPUT TABLE ConfirmacaoRetiradaMensagemResul) NO-ERROR. */
                  IF ERROR-STATUS:ERROR THEN DO:
                      
                      RUN pi-retornoErro (INPUT "ConfirmacaoRetiradaMensagem").
                      NEXT blk_RecMsg.
                  END.

                  FOR EACH ConfirmacaoRetiradaMensagemResul:

                      IF ConfirmacaoRetiradaMensagemResul.dscStatusRetirada <> "OK"
                           THEN DO:
                          RUN pi-erro (INPUT "Falha na confirmaá∆o de Retirada TrackId " + STRING(ListaMsgDisponiveisMsgDisponivel.trkIdIn),
                                       INPUT "ConfirmacaoRetiradaMensagem").
                          NEXT blk_RecMsg.
                      END.
                  END.
               END.
           END.
           /* Gera log de erro para envia e-mail */
           ELSE RUN pi-erro (INPUT RetornoRecuperacaoMensagensResul.dscErroEnvio,
                             INPUT "RetornoRecuperacaoMensagensResul").
           
        END.
    END.
END.

PROCEDURE pi-disconecta:

    ha-WebService:DISCONNECT()  NO-ERROR.
    DELETE OBJECT ha-WebService NO-ERROR.

END.

/* Procedure envia erros */
PROCEDURE pi-enviaemail:
    DEFINE VARIABLE c-assunto   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-corpo     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-arq-anexo AS CHARACTER  NO-UNDO.

    ASSIGN c-arq-anexo = SESSION:TEMP-DIRECTORY + "Log_Van.txt".

    /* Gera arquivo de log */
    OUTPUT TO VALUE(c-arq-anexo). 

        FOR EACH tt-erro:     
             
            DISP tt-erro.iseq  COLUMN-LABEL "Id"   FORMAT '>>9'
                 tt-erro.cproc VIEW-AS editor SIZE 50 BY 3 COLUMN-LABEL "Rotina do Erro" FORMAT 'X(200)'
                 tt-erro.cmsg  VIEW-AS editor SIZE 50 BY 3 COLUMN-LABEL "Descriá∆o do Erro" FORMAT "X(180)"
                WITH WIDTH 500 DOWN FRAME f.
            DOWN WITH FRAME f.
        END.
    
    OUTPUT CLOSE.

    ASSIGN c-assunto = "Log processamento Van de Pagamento " + STRING(NOW, "99/99/9999 HH:MM:SS.SSS")
           c-corpo   = CHR(10) + "Em anexo relat¢rio com o log de erro processamento referente ao dia " + STRING(TODAY,"99/99/9999").
    
    EMPTY TEMP-TABLE tt-envio2.
    EMPTY TEMP-TABLE tt-mensagem.

    FIND FIRST param_email NO-LOCK NO-ERROR.

    FIND FIRST es_parametros NO-LOCK 
        WHERE es_parametros.cod_prog_dtsul = "VAN006"
        AND   es_parametros.cod_referencia BEGINS "email" NO-ERROR.

    CREATE tt-envio2.
    ASSIGN tt-envio2.versao-integracao = 1
           tt-envio2.exchange          = param_email.log_servid_exchange
           tt-envio2.servidor          = param_email.cod_servid_e_mail
           tt-envio2.porta             = param_email.num_porta
           tt-envio2.destino           = es_parametros.cod_parametro
           tt-envio2.assunto           = "Relat¢rio Log de erro Van "
           tt-envio2.remetente         = "van@yamana.com"
           tt-envio2.copia             = ""
           tt-envio2.mensagem          = "Prezados(a/as)," +  CHR(13) + CHR(13) +
                                         "Segue em anexo o log de processamento do Pagamento Eletrìnico " + STRING(TODAY,"99/99/9999") + "." + CHR(13) + CHR(13) + 
                                         "Atenciosamente," + CHR(13) + CHR(13) + "Sustentaá∆o Yamana"    
           tt-envio2.importancia       = 1
           tt-envio2.log-enviada       = NO 
           tt-envio2.log-lida          = NO 
           tt-envio2.acomp             = NO 
           tt-envio2.arq-anexo         = c-arq-anexo.

    CREATE tt-mensagem.
    ASSIGN tt-mensagem.seq-mensagem = 1
           tt-mensagem.mensagem     = "Prezados(a/as)," +  CHR(13) + CHR(13) +                                                                                          
                                      "Segue em anexo o log de processamento do Pagamento Eletrìnico " + STRING(TODAY,"99/99/9999") + "." + CHR(13) + CHR(13) +         
                                      "Atenciosamente," + CHR(13) + CHR(13) + "Sustentaá∆o Yamana".

    RUN utp/utapi019.p PERSISTENT SET h-utapi019.

    RUN pi-execute2 IN h-utapi019 (INPUT  TABLE tt-envio2,
                                   INPUT  TABLE tt-mensagem,
                                   OUTPUT TABLE tt-erros).
    
    DELETE PROCEDURE h-utapi019.
    
END PROCEDURE.


PROCEDURE pi-erro:

    DEFINE INPUT  PARAMETER ipc-msg  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipc-tipo AS CHARACTER   NO-UNDO.

    ASSIGN i-seq = i-seq + 1.

    CREATE tt-erro.
    ASSIGN tt-erro.iseq  = i-seq
           tt-erro.cmsg  = ipc-msg
           tt-erro.cproc = ipc-tipo.
END.

PROCEDURE pi-retornoErro:
           
    DEFINE INPUT  PARAMETER ipc-metodo AS CHARACTER   NO-UNDO.
    /* Busca Retorno de Erro das chamadas dos mÇtodos do WebService */

    IF VALID-HANDLE(ERROR-STATUS:ERROR-OBJECT-DETAIL) THEN DO:
        DEFINE VARIABLE hSoapFault AS HANDLE.
        DEFINE VARIABLE iError     AS INTEGER     NO-UNDO.
        DEFINE VARIABLE c-error    AS CHARACTER   NO-UNDO.
          
        hSoapFault = ERROR-STATUS:ERROR-OBJECT-DETAIL.
        DO iError = 1 TO hSoapFault:NUM-MESSAGES:

            c-error = hSoapFault:GET-MESSAGE(iError). 

            RUN pi-erro (INPUT c-Error,
                         INPUT ipc-metodo).
        END.

        IF INDEX( "Receiver", hSoapFault:SOAP-FAULT-CODE ) > 0 THEN DO:

            IF VALID-HANDLE( hSoapFault:SOAP-FAULT-DETAIL ) THEN DO:
    
                 DEFINE VARIABLE hSoapFaultDetail AS HANDLE.
                 DEFINE VARIABLE hxnoderef        AS HANDLE.
                 DEFINE VARIABLE cFaultDetail     AS MEMPTR  NO-UNDO.
                 DEFINE VARIABLE l-lok            AS LOGICAL NO-UNDO.
    
                 hSoapFaultDetail = hSoapFault:SOAP-FAULT-DETAIL.
                 CREATE X-NODEREF hxnoderef.
                 hSoapFaultDetail:GET-NODE( hxnoderef ).
                 l-ok = hxnoderef:NODE-VALUE-TO-MEMPTR(cFaultDetail).

            END. /* Examine SOAP-FAULT-DETAIL */
        END. /* Return SOAP-FAULT-CODE info */
    END. /* Examine ERROR-OBJECT-DETAIL */
END.


/* Converte o arquivo a ser enviado em tipo RAW */
PROCEDURE pi-arqRaw:
    DEFINE INPUT  PARAMETER carq AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER cData AS LONGCHAR   NO-UNDO.
    /*DEFINE OUTPUT PARAMETER carqRaw AS Raw      NO-UNDO.*/
    
    /*
    EMPTY TEMP-TABLE tt-arquivo.

    INPUT FROM VALUE(carq).
    
    REPEAT:
       CREATE tt-arquivo.
       IMPORT UNFORMATTED tt-arquivo.
    END.

    INPUT CLOSE.

    COPY-LOB FROM FILE VALUE(carq) TO carqRaw.

    RAW-TRANSFER tt-arquivo TO carqRaw.
    */



    COPY-LOB FROM FILE carq TO cData.
    

    
END.

PROCEDURE pi-folha:
    /* Processa os arquivos de pagamento da folha conforme os parÉmetros definidos na tela van007.w */

    DEFINE INPUT  PARAMETER ipc_dir AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE c_Arq_Raw AS RAW NO-UNDO.
    DEFINE VARIABLE c_Arq_Data AS LONGCHAR NO-UNDO.
    
    /* Conecta webservice */
    RUN pi-buscawebservice.

    IF RETURN-VALUE <> "OK" THEN DO:
       RUN pi-erro (INPUT RETURN-VALUE,
                    INPUT "EnviaRemessa").
       RETURN "NOK".
    END.
            
    FIND FIRST es_param_folha NO-LOCK 
        WHERE es_param_folha.cdn_empresa = v_cod_empres_usuar NO-ERROR.
    IF NOT AVAIL es_param_folha THEN DO:
        RUN pi-erro (INPUT "N∆o encontrada tabela de parÉmetros de diret¢rio da folha - es_param_folha",
                     INPUT "pi-folha").

        RETURN ERROR.
    END.

    IF R-INDEX(ipc_dir,"/") > 0 
         THEN ASSIGN ipc_dir = REPLACE(ipc_dir,"/","\").

    ASSIGN c_enviados = es_param_folha.dir_sucesso     
           c_erro     = es_param_folha.dir_erro        
           c_pendente = ipc_dir.

    EMPTY TEMP-TABLE EnviaRemessaResult.

    IF SEARCH(c_pendente) = ? THEN RETURN.
      
    RUN pi-arqRaw (INPUT c_pendente,
                   OUTPUT c_Arq_Data).

    ASSIGN c_destino = c_enviados + '\' + SUBSTR(c_pendente,R-INDEX(c_pendente,"\") + 1,LENGTH(c_pendente)).
        
    RUN pi-enviaremessa (INPUT c_Arq_Data,
                         INPUT c_pendente).  /* Conte£do do Arquivo */
                           
    RUN pi-disconecta.

    IF CAN-FIND(FIRST tt-erro) THEN
        RUN pi-enviaemail.

END.
/****************************************** FINAL DO PROGRAMA ******************************************************************************/


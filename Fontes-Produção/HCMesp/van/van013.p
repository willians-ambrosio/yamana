/****************************************************************************************** 
** 	   Programa: van013.p
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 05/11/2018
** Change/Chamado: 
**      Objetivo: Trata os arquivos recebidos da Van de Pagamentos Accesstage. - Extratos, Retornos de Pagamento 
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: Id do Tipo de serviáo que ser† consumido, nome do serviáo que ser† consumido
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
{utp\ut-glob.i}

DEFINE INPUT  PARAMETER ipl_compactado AS LOGICAL    NO-UNDO.
DEFINE INPUT  PARAMETER ipr_conteudo  AS RAW         NO-UNDO.
DEFINE INPUT  PARAMETER ipc_tipo      AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipc_empresa   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipc_track     AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipr_row_data  AS ROWID       NO-UNDO.  
DEFINE OUTPUT PARAMETER ipl-ok        AS LOGICAL     NO-UNDO.
DEFINE OUTPUT PARAMETER opc-arquivo   AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cdata     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cdata1    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExt      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLongData AS LONGCHAR  NO-UNDO.

DEFINE VARIABLE c_dir          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-dir_arq      AS CHARACTER   NO-UNDO FORMAT "X(250)".
DEFINE VARIABLE l_cripto       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE encdmptr       AS MEMPTR      NO-UNDO.

DEF BUFFER bf-es_param_dir_van FOR es_param_dir_van.

cdata = GET-STRING(IPR_conteudo,1).

/* Se n∆o encontrar a parametrizaá∆o o arquivo cai na pasta genÇrica - Van\Recebe */
RUN pi_monta_dir.

IF SUBSTR(cdata,2,3) = "PDF" THEN
    RUN pi_comprovante.
ELSE RUN pi_outros_arq.

IF ipl-ok = YES
    THEN RETURN "OK".              
ELSE RETURN RETURN-VALUE.

PROCEDURE pi_outros_arq:

      /* Gera o arquivo na pasta recebe */
    IF l_cripto  /* Criptografa arquivos antes de gravar na pasta */
       THEN RUN pi-encrypt (INPUT cdata).

    OUTPUT TO VALUE(opc-arquivo).

        PUT UNFORMATTED 
            cdata.
    OUTPUT CLOSE.

    ASSIGN ipl-ok = YES.

END.

PROCEDURE pi_comprovante:
    /* Recebe os comprovantes em formato PDF */
    DEFINE VARIABLE cMemptr AS MEMPTR NO-UNDO.
    DEFINE VARIABLE cfile   AS CHARACTER NO-UNDO.
                                                                                           
    SET-SIZE(cMemptr) = LENGTH(IPR_conteudo) + 1.

    cLongData = BASE64-ENCODE(IPR_conteudo). 

    cMemptr = BASE64-DECODE(cLongData).
   
    COPY-LOB FROM cMemptr TO FILE opc-arquivo.

    SET-SIZE(cMemptr) = 0.

    ASSIGN ipl-ok = YES.

END.

PROCEDURE pi_monta_dir: 

    FIND bf-es_param_dir_van NO-LOCK WHERE
         ROWID(bf-es_param_dir_van) = ipr_row_data NO-ERROR.

    ASSIGN c_dir       = SUBSTR(bf-es_param_dir_van.dir_retorno,1,6) + "\Recebe" WHEN AVAIL bf-es_param_dir_van
           opc-arquivo = "".
    
    IF ipc_tipo MATCHES "*COMPROVANTE*" THEN 
        ASSIGN c_dir  = bf-es_param_dir_van.dir_retorno WHEN AVAIL bf-es_param_dir_van
               cExt   = ".pdf".
    
    IF ipc_tipo MATCHES "*EXTRATO*" THEN
        ASSIGN c_dir = bf-es_param_dir_van.dir_extrato WHEN AVAIL bf-es_param_dir_van
               cExt  = ".txt".
    
    CASE ipc_tipo:
    
        WHEN "ARRECADACAO - CODIGO DE BARRAS"
          THEN ASSIGN c_dir  = bf-es_param_dir_van.dir_retorno WHEN AVAIL bf-es_param_dir_van
                      cExt   = ".ret".
                      
        WHEN "ARRECADACAO - DEBITO AUTOMATICO"
          THEN ASSIGN c_dir = bf-es_param_dir_van.dir_retorno WHEN AVAIL bf-es_param_dir_van.
    
        WHEN "COBRANCA" /* Cobranáa n∆o Ç tratada */
            THEN .
    
        WHEN "EXTRATO"
            THEN ASSIGN c_dir  = bf-es_param_dir_van.dir_extrato WHEN AVAIL bf-es_param_dir_van
                        cExt   = ".txt".
    
        WHEN "DEPOSITO IDENTIFICADO"
            THEN ASSIGN c_dir = bf-es_param_dir_van.dir_retorno WHEN AVAIL bf-es_param_dir_van 
                        cExt  = ".txt".
    
        WHEN "OUTROS" /* Deposito Identificado */
            THEN ASSIGN c_dir = bf-es_param_dir_van.dir_retorno WHEN AVAIL bf-es_param_dir_van
                        cExt  = ".txt".
        
        WHEN "PAGAMENTO"
            THEN ASSIGN c_dir = bf-es_param_dir_van.dir_retorno WHEN AVAIL bf-es_param_dir_van  
                        cExt  = ".ret".
                       
        WHEN "PAGAMENTO - COMPROVANTE" 
            THEN ASSIGN c_dir = bf-es_param_dir_van.dir_comprovante WHEN AVAIL bf-es_param_dir_van
                        cExt  = ".pdf"
                        l_cripto = YES.
    
       WHEN "FOLHA DE PAGAMENTO" THEN 
           /* Là parÉmetro da folha para n∆o incluir arquivos de folha no mesmo diret¢rio dos restantes */
           ASSIGN c_dir    = bf-es_param_dir_van.dir_retorno WHEN AVAIL bf-es_param_dir_van
                  cExt     = ".ret".
    
        WHEN "PAGAMENTO FOLHA - CAIXA"
            THEN ASSIGN c_dir    = bf-es_param_dir_van.dir_retorno WHEN AVAIL bf-es_param_dir_van
                        cExt     = ".ret"
                        l_cripto = YES.
    END CASE.                    

    IF SUBSTR(cData,2,3) = "PDF" THEN
        ASSIGN cExt = ".pdf".

    ASSIGN opc-arquivo = c_dir + '\' + ipc_empresa + "_" + ipc_tipo + "_" + ipc_track + cExt.

    /* Qdo estiver na base no HCM sempre ir† criptografar os arquivos */
    IF  CONNECTED("hresp")
         THEN ASSIGN l_cripto = YES.
    
END.

PROCEDURE pi-encrypt:

    DEFINE INPUT  PARAMETER cConteudo AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE rBinaryKey        AS RAW        NO-UNDO.
    DEFINE VARIABLE rEncryptedValue   AS RAW        NO-UNDO.
    DEFINE VARIABLE cEncryptedText    AS CHARACTER  NO-UNDO.

    ASSIGN rBinaryKey = GENERATE-RANDOM-KEY
           SECURITY-POLICY:SYMMETRIC-ENCRYPTION-ALGORITHM = "AES_OFB_128"
           SECURITY-POLICY:SYMMETRIC-ENCRYPTION-KEY       = rBinaryKey
           SECURITY-POLICY:SYMMETRIC-ENCRYPTION-IV        = ?
           rEncryptedValue                                = ENCRYPT(cConteudo)
           cEncryptedText                                 = BASE64-ENCODE(rEncryptedValue).
END.

PROCEDURE pi-decrypt:
    
    DEFINE INPUT PARAMETER iprBinaryKey      AS RAW      NO-UNDO.
    DEFINE INPUT PARAMETER iprEncryptedValue AS RAW      NO-UNDO.
    DEFINE VARIABLE        cDecrypted        AS CHARACTER NO-UNDO.

    ASSIGN  SECURITY-POLICY:SYMMETRIC-ENCRYPTION-ALGORITHM = "AES_OFB_128"
            SECURITY-POLICY:SYMMETRIC-ENCRYPTION-KEY       = iprBinaryKey
            SECURITY-POLICY:SYMMETRIC-ENCRYPTION-IV        = ?
            cDecrypted                                     = GET-STRING(DECRYPT (iprEncryptedValue),1).
END.

/****************************************************************************************** 
** 	   Programa: van002.p
**   	  Autor: Daniela Campos
** 	 Fornecedor: DKP
**         Data: 12/09/2018
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
DEFINE INPUT  PARAMETER ipi_pdsid     AS INTEGER     NO-UNDO.  
DEFINE OUTPUT PARAMETER ipl-ok        AS LOGICAL     NO-UNDO.
DEFINE OUTPUT PARAMETER opc-arquivo   AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cdata     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cdata1    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExt      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLongData AS LONGCHAR  NO-UNDO.

DEFINE TEMP-TABLE tt-erros-zip NO-UNDO
    FIELD cod-erro  AS INTEGER FORMAT ">>>>9"
    FIELD desc-erro AS CHAR    FORMAT "x(70)".

DEFINE VARIABLE c_cnpj         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c_dir          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE h-zip          AS HANDLE      NO-UNDO.
DEFINE VARIABLE c-arquivo-zip  AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEFINE VARIABLE c-dir_arq      AS CHARACTER   NO-UNDO FORMAT "X(250)".
DEFINE VARIABLE encdmptr       AS MEMPTR      NO-UNDO.
DEFINE VARIABLE c_banco        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l_cripto       AS LOGICAL     NO-UNDO.

DEF BUFFER banco FOR ems5.banco.

FUNCTION fnc_cnpj RETURNS CHAR (INPUT ipcinfo AS CHAR ):  
    
    IF ipc_tipo MATCHES "*EXTRATO*" THEN
           c_cnpj = SUBSTR(cData,19,14).
    IF ipc_tipo MATCHES "*ARRECADACAO*" THEN
           c_cnpj = SUBSTR(cData,19,14).
    IF ipc_tipo MATCHES "*PAGAMENTO*" THEN
           c_cnpj = SUBSTR(cData,19,14).
    IF ipc_tipo MATCHES "*COMPROVANTE*" THEN /* Verificar a info no comprovante */
           c_cnpj = SUBSTR(cData,19,14).
   
    RETURN c_cnpj.

END FUNCTION.

/* Descompacta arquivo */
IF ipl_compactado THEN DO:
    RUN utp/ut-zip.p PERSISTENT SET h-zip.
    RUN pi-zip-files.
END.

cdata = GET-STRING(IPR_conteudo,1).

/* Est∆o retornando arquivos de todos os tipos com o formato em PDF */
IF SUBSTR(cdata,2,3) <> "PDF" AND ipc_tipo <> "" THEN DO: /* Identifica a empresa para onde ser† direcionado o arquivo */
   RUN pi_Localiza_Empresa.
  
                    
/*    IF RETURN-VALUE <> "OK"      */
/*       THEN RETURN RETURN-VALUE. */
END.
                       
/* Se n∆o encontrar a parametrizaá∆o o arquivo cai na pasta genÇrica - Van\Recebe */
RUN pi_monta_dir.

ASSIGN l_cripto = NO.

IF ipc_tipo MATCHES "*COMPROVANTE*" OR SUBSTR(cdata,2,3) = "PDF" THEN
    RUN pi_comprovante.
ELSE RUN pi_outros_arq.


IF ipl-ok = YES
    THEN RETURN "OK".              
ELSE RETURN RETURN-VALUE.

PROCEDURE pi_outros_arq:
    /* Gera o arquivo na pasta recebe */
    IF l_cripto
       THEN RUN pi-encrypt (INPUT cdata).

    OUTPUT TO VALUE(opc-arquivo).

        PUT UNFORMATTED 
            cdata.
    OUTPUT CLOSE.

    ASSIGN ipl-ok = YES.

END.
    
PROCEDURE pi-zip-file:
  
   IF SEARCH(c-arquivo-zip) <> ? THEN
      OS-DELETE VALUE(c-arquivo-zip) NO-ERROR.
    
   RUN zipFiles IN h-zip (INPUT c-arquivo-zip, 
                          INPUT ipr_conteudo,
                          INPUT NO,
                          OUTPUT TABLE tt-erros-zip).

   IF CAN-FIND(FIRST tt-erros-zip) THEN
       ASSIGN ipl-ok = NO.
   ELSE ASSIGN ipl-ok = YES.

/*    FOR EACH tt-erros-zip:          */
/*       DISP tt-erros-zip.cod-erro   */
/*            tt-erros-zip.desc-erro. */
/*    END.                            */
END.

PROCEDURE pi_localiza_empresa:

    c_cnpj  = fnc_cnpj(cdata).
    c_banco = STRING(int(SUBSTR(cdata,1,3))).

    /* Localiza estabelecimento/empresa */
    FIND pessoa_Jurid NO-LOCK WHERE
         pessoa_jurid.cod_id_feder = c_cnpj NO-ERROR.
    IF NOT AVAIL pessoa_jurid THEN 
        RETURN SUBSTITUTE("CNPJ &1 n∆o encontrado!",c_cnpj).
    
    FIND estabelecimento NO-LOCK WHERE
         estabelecimento.num_pessoa_jurid = pessoa_jurid.num_pessoa_jurid NO-ERROR.
             
    blk:
    FOR EACH cta_corren NO-LOCK WHERE
             cta_corren.cod_estab = estabelecimento.cod_estab:
    
        IF cta_corren.cod_banco <> c_banco THEN NEXT.

        FIND FIRST es_param_van_dir NO-LOCK                                       
                   WHERE es_param_van_dir.identificador  = "02"                    
                     AND es_param_van_dir.nomeserv       = "RetornoListaMensagens" 
                     AND es_param_van_dir.cod_cta_corren = cta_corren.cod_cta_corren NO-ERROR.
        IF NOT AVAIL es_param_van_dir  THEN NEXT.
    
        LEAVE blk.
    END.

    /* ParÉmetro exclusivo para os arquivos de folha */
    FIND FIRST es_param_folha NO-LOCK WHERE
               es_param_folha.cdn_empresa = v_cod_empres_usuar NO-ERROR.

    IF NOT AVAIL es_param_van_dir THEN DO:
                    
        ASSIGN ipl-ok = NO.
        RETURN "N∆o encontrado parÉmetro de diret¢rios! N∆o ser† poss°vel retornar arquivos".
    END.
    ELSE DO:
         ASSIGN ipl-ok = YES.
         RETURN "OK".
    END.
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

    ASSIGN c_dir       = SUBSTR(es_param_van_dir.dir_retorno,1,6) + "\Recebe" WHEN AVAIL es_param_van_dir
           opc-arquivo = "".
           
    
    IF ipc_tipo MATCHES "*COMPROVANTE*" THEN 
        ASSIGN c_dir  = es_param_van_dir.dir_retorno WHEN AVAIL es_param_van_dir
               cExt   = ".pdf".
    
    IF ipc_tipo MATCHES "*EXTRATO*" THEN
        ASSIGN c_dir = es_param_van_dir.dir_extrato WHEN AVAIL es_param_van_dir
               cExt  = ".txt".
    
    CASE ipc_tipo:
    
        WHEN "ARRECADACAO - CODIGO DE BARRAS"
          THEN ASSIGN c_dir  = es_param_van_dir.dir_retorno WHEN AVAIL es_param_van_dir
                      cExt   = ".ret".
                      
        WHEN "ARRECADACAO - DEBITO AUTOMATICO"
          THEN ASSIGN c_dir = es_param_van_dir.dir_retorno WHEN AVAIL es_param_van_dir.
    
        WHEN "COBRANCA" /* Cobranáa n∆o Ç tratada */
            THEN .
    
        WHEN "EXTRATO"
            THEN ASSIGN c_dir  = es_param_van_dir.dir_extrato WHEN AVAIL es_param_van_dir
                        cExt   = ".txt".
    
        WHEN "DEPOSITO IDENTIFICADO"
            THEN ASSIGN c_dir = es_param_van_dir.dir_retorno WHEN AVAIL es_param_van_dir 
                        cExt  = ".txt".
    
        WHEN "OUTROS" /* Deposito Identificado */
            THEN ASSIGN c_dir = es_param_van_dir.dir_retorno WHEN AVAIL es_param_van_dir
                        cExt  = ".txt".
        
        WHEN "PAGAMENTO"
            THEN ASSIGN c_dir = es_param_van_dir.dir_retorno WHEN AVAIL es_param_van_dir  
                        cExt  = ".ret".
                       
        WHEN "PAGAMENTO - COMPROVANTE" 
            THEN ASSIGN c_dir = es_param_van_dir.dir_comprovante WHEN AVAIL es_param_van_dir
                        cExt  = ".pdf".
    
       WHEN "FOLHA DE PAGAMENTO" THEN 
           /* Là parÉmetro da folha para n∆o incluir arquivos de folha no mesmo diret¢rio dos restantes */
           ASSIGN c_dir    = es_param_folha.dir_retorno WHEN AVAIL es_param_folha
                  cExt     = ".ret"
                  l_cripto = YES.
    
        WHEN "PAGAMENTO FOLHA - CAIXA"
            THEN ASSIGN c_dir    = es_param_folha.dir_retorno WHEN AVAIL es_param_folha
                        cExt     = ".ret"
                        l_cripto = YES.
    END CASE.                    

    IF NOT AVAIL es_param_van_dir THEN DO:

        FIND FIRST es_param_van_dir NO-LOCK                                       
           WHERE es_param_van_dir.identificador  = "02"                    
             AND es_param_van_dir.nomeserv       = "RetornoListaMensagens" NO-ERROR.
        ASSIGN c_dir = SUBSTR(es_param_van_dir.dir_retorno,1,6) + "\Recebe".
    END.
           
    IF SUBSTR(cData,2,3) = "PDF" THEN
        ASSIGN cExt = ".pdf".

    ASSIGN opc-arquivo = c_dir + '\' + ipc_empresa + "_" + ipc_tipo + "_" + ipc_track + cExt.
    
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

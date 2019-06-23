/* ---------------------------------------------------------------------------------------- *
|                                                                                           |
|  Sistema................: GRUPO DKP                                                       |
|  Modulo.................: MANUFATURA                                                      |
|  Programa...............: RETORNOWS                                                      |
|  Sub Programa...........:                                                                 |
|  Descricao..............: CONSUMO DE WEBSERVICE KLASSMATT                                 |
|  Entidade Desenvemaiolvedora: DKP                                                         |
|                                                                                           |
|  Historico Programa -------------------------------------------------------------------+  |
|  | Data       | Autor               | Descricao                                        |  |
|  +----------- +---------------------+--------------------------------------------------+  |
|  | 11|2018    | Willians Ambrosio   | Desenvolvimento do Programa                      |  |
|  +------------+---------------------+--------------------------------------------------+  |
|  | Parametros :                                                                        |  |
|  |                                                                                     |  |
|  | Observacao :                                                                        |  |
|  |                                                                                     |  |
|  +-------------------------------------------------------------------------------------+  |
|  Versao: 1.00.000                                                                         |
   --------------------------------------------------------------------------------------- */ 

DEFINE VARIABLE hWebService          AS HANDLE      NO-UNDO.
DEFINE VARIABLE hwsIntYamanaGoldSoap AS HANDLE      NO-UNDO.
DEFINE VARIABLE pTokenSeguranca      AS CHARACTER INIT "Y@M@NAG0LD_PRDA" NO-UNDO.
DEFINE VARIABLE pIdKlassmatt         AS INTEGER     NO-UNDO.
DEFINE VARIABLE pCodigoProtheus      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE pStatusRetorno       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE pMensagemRetorno     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE pEmpresa             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE pIDSIN               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ret                  AS LOG         NO-UNDO.
DEFINE VARIABLE cTargetType          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFile                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lFormatted           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cEncoding            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSchemaLocation      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lWriteSchema         AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lMinSchema           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lRetOK               AS LOGICAL     NO-UNDO. 
DEFINE VARIABLE l-conectado          AS LOGICAL     NO-UNDO.
DEFINE VARIABLE GetDesafioResult     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dt-trans-k           AS DATE        NO-UNDO.
DEFINE VARIABLE c-hr-trans           AS CHARACTER   NO-UNDO.

DEF BUFFER b-es-integra-retorno    FOR es-integra-retorno.
DEF BUFFER bdel-es-integra-retorno FOR es-integra-retorno.

FIND FIRST es-param-tax NO-LOCK NO-ERROR.

CREATE SERVER hWebService.

l-conectado = hWebService:CONNECT(es-param-tax.url-webservice) NO-ERROR. 

IF NOT hWebService:CONNECTED() THEN RETURN.

ASSIGN dt-trans-k      = TODAY
       c-hr-trans      = STRING(TIME,"HH:MM:SS")
       pTokenSeguranca = "Y@M@NAG0LD_PRDA".

IF hWebService:CONNECTED() THEN DO:
  
    RUN wsIntYamanaGoldSoap SET hwsIntYamanaGoldSoap ON hWebService.
/*     RUN GetDesafio IN hwsIntYamanaGoldSoap(OUTPUT GetDesafioResult).        */
/*     RUN esp\desafio-md5.p (INPUT GetDesafioResult, OUTPUT pTokenSeguranca). */

    FOR EACH b-es-integra-retorno WHERE  
             b-es-integra-retorno.dt-ret = ? NO-LOCK:

        FIND FIRST es-klassmatt-integr WHERE
                   es-klassmatt-integr.idklassmatt = b-es-integra-retorno.IdKlassmatt AND
                   es-klassmatt-integr.dt-trans    = dt-trans-k                       AND
                   es-klassmatt-integr.hr-trans    = c-hr-trans                       EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL es-klassmatt-integr THEN
        DO:
           CREATE es-klassmatt-integr.
           BUFFER-COPY b-es-integra-retorno     TO es-klassmatt-integr
                                            ASSIGN es-klassmatt-integr.idklassmatt = b-es-integra-retorno.idklassmatt
                                                   es-klassmatt-integr.dt-trans    = dt-trans-k                 
                                                   es-klassmatt-integr.hr-trans    = c-hr-trans.
        END.

        ASSIGN es-klassmatt-integr.log-retorno   = "Integraá∆o n∆o realizada. Dados da Tabela Retorno n∆o localizados."  
               es-klassmatt-integr.statusRetorno = "N".
 
        FIND FIRST es-integra-retorno WHERE es-integra-retorno.ep-codigo      = b-es-integra-retorno.ep-codigo
                                      AND   es-integra-retorno.IdKlassmatt    = b-es-integra-retorno.IdKlassmatt
                                      AND   es-integra-retorno.idsin          = b-es-integra-retorno.idsin
                                      AND   es-integra-retorno.codigo         = b-es-integra-retorno.codigo
                                      AND   es-integra-retorno.StatusRetorno  = b-es-integra-retorno.StatusRetorno
                                      AND   es-integra-retorno.log-retorno    = b-es-integra-retorno.log-retorno
                                      AND   es-integra-retorno.dt-ret         = ?  
                                      EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

        IF AVAIL es-integra-retorno THEN 
        DO:
            IF LOCKED es-integra-retorno  THEN.
            ELSE DO:
                ASSIGN  pIdKlassmatt     =  es-integra-retorno.IdKlassmatt                                               
                        pempresa         =  es-integra-retorno.ep-codigo                                                 
                        pidsin           =  string(es-integra-retorno.idsin)                                             
                        pCodigoProtheus  =  es-integra-retorno.codigo                                                    
                        pStatusRetorno   =  es-integra-retorno.StatusRetorno                                             
                        pMensagemRetorno =  es-integra-retorno.log-retorno.     
                                                                                                                           
                RUN InformaRespostaProcessamentoItem IN hwsIntYamanaGoldSoap(INPUT  pTokenSeguranca,                      
                                                                             INPUT  pIdKlassmatt,                         
                                                                             INPUT  pidsin,                               
                                                                             INPUT  pempresa,                             
                                                                             INPUT  pCodigoProtheus,                      
                                                                             INPUT  pStatusRetorno ,                      
                                                                             INPUT  pMensagemRetorno,                     
                                                                             OUTPUT ret).   
                  IF ret THEN DO:                                                                                          
                      ASSIGN es-integra-retorno.dt-ret         = TODAY
                             es-klassmatt-integr.log-retorno   = "Integraá∆o realizada com sucesso! Transaá∆o enviada ao Klassmatt."  
                             es-klassmatt-integr.statusRetorno = "S".

                      FOR EACH bdel-es-integra-retorno 
                         WHERE bdel-es-integra-retorno.dt-ret < (TODAY - 30) SHARE-LOCK:
                          DELETE bdel-es-integra-retorno.
                      END.

                  END.
            END.
        END.
    END.
END.



IF VALID-HANDLE(hWebService) THEN
    DELETE OBJECT hWebService NO-ERROR.

IF VALID-HANDLE(hwsIntYamanaGoldSoap) THEN
    DELETE OBJECT hwsIntYamanaGoldSoap NO-ERROR.






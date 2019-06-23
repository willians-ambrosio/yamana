&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DBOProgram 
/*:T--------------------------------------------------------------------------
    File       : dbo.p
    Purpose    : O DBO (Datasul Business Objects) ² um programa PROGRESS 
                 que cont²m a l½gica de neg½cio e acesso a dados para uma 
                 tabela do banco de dados.

    Parameters : 

    Notes      : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  **************************** */

/*:T--- Diretrizes de defini»’o ---*/
&GLOBAL-DEFINE DBOName BOCONT-ESP
&GLOBAL-DEFINE DBOVersion 
&GLOBAL-DEFINE DBOCustomFunctions 
&GLOBAL-DEFINE TableName controle-inv-esp
&GLOBAL-DEFINE TableLabel 
&GLOBAL-DEFINE QueryName qr{&TableName} 

/* DBO-XML-BEGIN */
/*:T Pre-processadores para ativar XML no DBO */
/*:T Retirar o comentario para ativar 
&GLOBAL-DEFINE XMLProducer YES    /*:T DBO atua como producer de mensagens para o Message Broker */
&GLOBAL-DEFINE XMLTopic           /*:T Topico da Mensagem enviada ao Message Broker, geralmente o nome da tabela */
&GLOBAL-DEFINE XMLTableName       /*:T Nome da tabela que deve ser usado como TAG no XML */ 
&GLOBAL-DEFINE XMLTableNameMult   /*:T Nome da tabela no plural. Usado para multiplos registros */ 
&GLOBAL-DEFINE XMLPublicFields    /*:T Lista dos campos (c1,c2) que podem ser enviados via XML. Ficam fora da listas os campos de especializacao da tabela */ 
&GLOBAL-DEFINE XMLKeyFields       /*:T Lista dos campos chave da tabela (c1,c2) */
&GLOBAL-DEFINE XMLExcludeFields   /*:T Lista de campos a serem excluidos do XML quando PublicFields = "" */

&GLOBAL-DEFINE XMLReceiver YES    /*:T DBO atua como receiver de mensagens enviado pelo Message Broker (m²todo Receive Message) */
&GLOBAL-DEFINE QueryDefault       /*:T Nome da Query que dÿ acessos a todos os registros, exceto os exclu­dos pela constraint de seguran»a. Usada para receber uma mensagem XML. */
&GLOBAL-DEFINE KeyField1 cust-num /*:T Informar os campos da chave quando o Progress n’o conseguir resolver find {&TableName} OF RowObject. */
*/
/* DBO-XML-END */

/*:T--- Include com defini»’o da temptable RowObject ---*/
/*:T--- Este include deve ser copiado para o diret½rio do DBO e, ainda, seu nome
      deve ser alterado a fim de ser id¼ntico ao nome do DBO mas com 
      extens’o .i ---*/
{esbo/boes001.i RowObject}


/*:T--- Include com defini»’o da query para tabela {&TableName} ---*/
/*:T--- Em caso de necessidade de altera»’o da defini»’o da query, pode ser retirada
      a chamada ao include a seguir e em seu lugar deve ser feita a defini»’o 
      manual da query ---*/
{method/dboqry.i}


/*:T--- Defini»’o de buffer que serÿ utilizado pelo m²todo goToKey ---*/
DEFINE BUFFER bf{&TableName} FOR {&TableName}.

def var v-cod-est-exec-ini like {&TableName}.cod-est-exec no-undo.
def var v-cod-est-exec-fim like {&TableName}.cod-est-exec no-undo.
def var v-num-ord-inv-ini  like {&TableName}.num-ord-inv  no-undo.
def var v-num-ord-inv-fim  like {&TableName}.num-ord-inv  no-undo.
def var v-num-ordem-ini    like {&TableName}.num-ordem    no-undo.
def var v-num-ordem-fim    like {&TableName}.num-ordem    no-undo.
def var v-num-projeto-ini  like {&TableName}.num-projeto  no-undo.
def var v-num-projeto-fim  like {&TableName}.num-projeto  no-undo.
def var v-sequencia-ini    like {&TableName}.sequencia    no-undo.
def var v-sequencia-fim    like {&TableName}.sequencia    no-undo.
def var v-dt-trans-ini     like {&TableName}.dt-trans     no-undo.
def var v-dt-trans-fim     like {&TableName}.dt-trans     no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DBOProgram
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DBOProgram
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW DBOProgram ASSIGN
         HEIGHT             = 18.92
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "DBO 2.0 Wizard" DBOProgram _INLINE
/* Actions: wizard/dbowizard.w ? ? ? ? */
/* DBO 2.0 Wizard (DELETE)*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB DBOProgram 
/* ************************* Included-Libraries *********************** */

{method/dbo.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DBOProgram 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterNewRecord DBOProgram 
PROCEDURE afterNewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    find last bfcontrole-inv-esp no-lock no-error.
    if avail bfcontrole-inv-esp then
        assign RowObject.sequencia = bfcontrole-inv-esp.sequencia + 1.
    else 
        assign RowObject.sequencia = 1.

    
    return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCharField DBOProgram 
PROCEDURE getCharField :
/*------------------------------------------------------------------------------
  Purpose:     Retorna valor de campos do tipo caracter
  Parameters:  
               recebe nome do campo
               retorna valor do campo
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pFieldName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pFieldValue AS CHARACTER NO-UNDO.

    /*--- Verifica se temptable RowObject estÿ dispon­vel, caso n’o esteja serÿ
          retornada flag "NOK":U ---*/
    IF NOT AVAILABLE RowObject THEN 
        RETURN "NOK":U.

    CASE pFieldName:
        WHEN "cod-est-exec":U THEN ASSIGN pFieldValue = RowObject.cod-est-exec.
        WHEN "embarque":U THEN ASSIGN pFieldValue = RowObject.embarque.
        WHEN "ep-codigo":U THEN ASSIGN pFieldValue = RowObject.ep-codigo.
        WHEN "narrativa":U THEN ASSIGN pFieldValue = RowObject.narrativa.
        WHEN "nat-operacao":U THEN ASSIGN pFieldValue = RowObject.nat-operacao.
        WHEN "nro-docto":U THEN ASSIGN pFieldValue = RowObject.nro-docto.
        WHEN "serie-docto":U THEN ASSIGN pFieldValue = RowObject.serie-docto.
        WHEN "tipo-doc":U THEN ASSIGN pFieldValue = RowObject.tipo-doc.
        OTHERWISE RETURN "NOK":U.
    END CASE.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDateField DBOProgram 
PROCEDURE getDateField :
/*------------------------------------------------------------------------------
  Purpose:     Retorna valor de campos do tipo data
  Parameters:  
               recebe nome do campo
               retorna valor do campo
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pFieldName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pFieldValue AS DATE NO-UNDO.

    /*--- Verifica se temptable RowObject estÿ dispon­vel, caso n’o esteja serÿ
          retornada flag "NOK":U ---*/
    IF NOT AVAILABLE RowObject THEN 
        RETURN "NOK":U.

    CASE pFieldName:
        WHEN "dt-trans":U THEN ASSIGN pFieldValue = RowObject.dt-trans.
        OTHERWISE RETURN "NOK":U.
    END CASE.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDecField DBOProgram 
PROCEDURE getDecField :
/*------------------------------------------------------------------------------
  Purpose:     Retorna valor de campos do tipo decimal
  Parameters:  
               recebe nome do campo
               retorna valor do campo
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pFieldName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pFieldValue AS DECIMAL NO-UNDO.

    /*--- Verifica se temptable RowObject estÿ dispon­vel, caso n’o esteja serÿ
          retornada flag "NOK":U ---*/
    IF NOT AVAILABLE RowObject THEN 
        RETURN "NOK":U.

    CASE pFieldName:
        WHEN "ent-comp":U THEN ASSIGN pFieldValue = RowObject.ent-comp.
        WHEN "ent-real":U THEN ASSIGN pFieldValue = RowObject.ent-real.
        WHEN "sai-comp":U THEN ASSIGN pFieldValue = RowObject.sai-comp.
        WHEN "sai-real":U THEN ASSIGN pFieldValue = RowObject.sai-real.
        WHEN "valor-origem":U THEN ASSIGN pFieldValue = RowObject.valor-origem.
        OTHERWISE RETURN "NOK":U.
    END CASE.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getIntField DBOProgram 
PROCEDURE getIntField :
/*------------------------------------------------------------------------------
  Purpose:     Retorna valor de campos do tipo inteiro
  Parameters:  
               recebe nome do campo
               retorna valor do campo
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pFieldName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pFieldValue AS INTEGER NO-UNDO.

    /*--- Verifica se temptable RowObject estÿ dispon­vel, caso n’o esteja serÿ
          retornada flag "NOK":U ---*/
    IF NOT AVAILABLE RowObject THEN 
        RETURN "NOK":U.

    CASE pFieldName:
        WHEN "cod-desp":U THEN ASSIGN pFieldValue = RowObject.cod-desp.
        WHEN "cod-emitente":U THEN ASSIGN pFieldValue = RowObject.cod-emitente.
        WHEN "cod-emitente-desp":U THEN ASSIGN pFieldValue = RowObject.cod-emitente-desp.
        WHEN "cod-itiner":U THEN ASSIGN pFieldValue = RowObject.cod-itiner.
        WHEN "cod-pto-contr":U THEN ASSIGN pFieldValue = RowObject.cod-pto-contr.
        WHEN "item":U THEN ASSIGN pFieldValue = RowObject.item.
        WHEN "nr-contrato":U THEN ASSIGN pFieldValue = RowObject.nr-contrato.
        WHEN "num-ord-inv":U THEN ASSIGN pFieldValue = RowObject.num-ord-inv.
        WHEN "num-ordem":U THEN ASSIGN pFieldValue = RowObject.num-ordem.
        WHEN "num-pedido":U THEN ASSIGN pFieldValue = RowObject.num-pedido.
        WHEN "num-projeto":U THEN ASSIGN pFieldValue = RowObject.num-projeto.
        WHEN "num-seq-item":U THEN ASSIGN pFieldValue = RowObject.num-seq-item.
        WHEN "numero-ordem":U THEN ASSIGN pFieldValue = RowObject.numero-ordem.
        WHEN "sequencia":U THEN ASSIGN pFieldValue = RowObject.sequencia.
        OTHERWISE RETURN "NOK":U.
    END CASE.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getKey DBOProgram 
PROCEDURE getKey :
/*------------------------------------------------------------------------------
  Purpose:     Retorna valores dos campos do ­ndice codigo
  Parameters:  
               retorna valor do campo cod-est-exec
               retorna valor do campo num-projeto
               retorna valor do campo num-ordem
               retorna valor do campo num-ord-inv
               retorna valor do campo sequencia
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER pcod-est-exec LIKE controle-inv-esp.cod-est-exec NO-UNDO.
    DEFINE OUTPUT PARAMETER pnum-projeto LIKE controle-inv-esp.num-projeto NO-UNDO.
    DEFINE OUTPUT PARAMETER pnum-ordem LIKE controle-inv-esp.num-ordem NO-UNDO.
    DEFINE OUTPUT PARAMETER pnum-ord-inv LIKE controle-inv-esp.num-ord-inv NO-UNDO.
    DEFINE OUTPUT PARAMETER psequencia LIKE controle-inv-esp.sequencia NO-UNDO.

    /*--- Verifica se temptable RowObject estÿ dispon­vel, caso n’o esteja serÿ
          retornada flag "NOK":U ---*/
    IF NOT AVAILABLE RowObject THEN 
       RETURN "NOK":U.

    ASSIGN pcod-est-exec = RowObject.cod-est-exec
           pnum-projeto = RowObject.num-projeto
           pnum-ordem = RowObject.num-ordem
           pnum-ord-inv = RowObject.num-ord-inv
           psequencia = RowObject.sequencia.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getLogField DBOProgram 
PROCEDURE getLogField :
/*------------------------------------------------------------------------------
  Purpose:     Retorna valor de campos do tipo l½gico
  Parameters:  
               recebe nome do campo
               retorna valor do campo
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pFieldName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pFieldValue AS LOGICAL NO-UNDO.

    /*--- Verifica se temptable RowObject estÿ dispon­vel, caso n’o esteja serÿ
          retornada flag "NOK":U ---*/
    IF NOT AVAILABLE RowObject THEN 
        RETURN "NOK":U.

    CASE pFieldName:
        OTHERWISE RETURN "NOK":U.
    END CASE.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getRawField DBOProgram 
PROCEDURE getRawField :
/*------------------------------------------------------------------------------
  Purpose:     Retorna valor de campos do tipo raw
  Parameters:  
               recebe nome do campo
               retorna valor do campo
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pFieldName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pFieldValue AS RAW NO-UNDO.

    /*--- Verifica se temptable RowObject estÿ dispon­vel, caso n’o esteja serÿ
          retornada flag "NOK":U ---*/
    IF NOT AVAILABLE RowObject THEN 
        RETURN "NOK":U.

    CASE pFieldName:
        OTHERWISE RETURN "NOK":U.
    END CASE.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getRecidField DBOProgram 
PROCEDURE getRecidField :
/*------------------------------------------------------------------------------
  Purpose:     Retorna valor de campos do tipo recid
  Parameters:  
               recebe nome do campo
               retorna valor do campo
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pFieldName AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pFieldValue AS RECID NO-UNDO.

    /*--- Verifica se temptable RowObject estÿ dispon­vel, caso n’o esteja serÿ
          retornada flag "NOK":U ---*/
    IF NOT AVAILABLE RowObject THEN 
        RETURN "NOK":U.

    CASE pFieldName:
        OTHERWISE RETURN "NOK":U.
    END CASE.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goToKey DBOProgram 
PROCEDURE goToKey :
/*------------------------------------------------------------------------------
  Purpose:     Reposiciona registro com base no ­ndice codigo
  Parameters:  
               recebe valor do campo cod-est-exec
               recebe valor do campo num-projeto
               recebe valor do campo num-ordem
               recebe valor do campo num-ord-inv
               recebe valor do campo sequencia
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pcod-est-exec LIKE controle-inv-esp.cod-est-exec NO-UNDO.
    DEFINE INPUT PARAMETER pnum-projeto LIKE controle-inv-esp.num-projeto NO-UNDO.
    DEFINE INPUT PARAMETER pnum-ordem LIKE controle-inv-esp.num-ordem NO-UNDO.
    DEFINE INPUT PARAMETER pnum-ord-inv LIKE controle-inv-esp.num-ord-inv NO-UNDO.
    DEFINE INPUT PARAMETER psequencia LIKE controle-inv-esp.sequencia NO-UNDO.

    FIND FIRST bfcontrole-inv-esp WHERE 
        bfcontrole-inv-esp.cod-est-exec = pcod-est-exec AND 
        bfcontrole-inv-esp.num-projeto = pnum-projeto AND 
        bfcontrole-inv-esp.num-ordem = pnum-ordem AND 
        bfcontrole-inv-esp.num-ord-inv = pnum-ord-inv AND 
        bfcontrole-inv-esp.sequencia = psequencia NO-LOCK NO-ERROR.

    /*--- Verifica se registro foi encontrado, em caso de erro serÿ retornada flag "NOK":U ---*/
    IF NOT AVAILABLE bfcontrole-inv-esp THEN 
        RETURN "NOK":U.

    /*--- Reposiciona query atrav²s de rowid e verifica a ocorr¼ncia de erros, caso
          existam erros serÿ retornada flag "NOK":U ---*/
    RUN repositionRecord IN THIS-PROCEDURE (INPUT ROWID(bfcontrole-inv-esp)).
    IF RETURN-VALUE = "NOK":U THEN
        RETURN "NOK":U.

    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQueryChave DBOProgram 
PROCEDURE OpenQueryChave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    OPEN QUERY qr{&TableName}
      FOR EACH   {&TableName} NO-LOCK 
          where   {&TableName}.cod-est-exec >= v-cod-est-exec-ini
          and     {&TableName}.cod-est-exec <= v-cod-est-exec-fim
          and     {&TableName}.num-ord-inv  >= v-num-ord-inv-ini
          and     {&TableName}.num-ord-inv  <= v-num-ord-inv-fim
          and     {&TableName}.num-ordem    >= v-num-ordem-ini
          and     {&TableName}.num-ordem    <= v-num-ordem-fim
          and     {&TableName}.num-projeto  >= v-num-projeto-ini
          and     {&TableName}.num-projeto  <= v-num-projeto-fim
          and     {&TableName}.sequencia    >= v-sequencia-ini
          and     {&TableName}.sequencia    <= v-sequencia-fim
          by {&TableName}.sequencia
          INDEXED-REPOSITION.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQueryData DBOProgram 
PROCEDURE OpenQueryData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    OPEN QUERY qr{&TableName}
      FOR EACH   {&TableName} NO-LOCK 
          where   {&TableName}.dt-trans >= v-dt-trans-ini
          and     {&TableName}.dt-trans <= v-dt-trans-fim
          by {&TableName}.sequencia
          INDEXED-REPOSITION.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQueryMain DBOProgram 
PROCEDURE OpenQueryMain :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    OPEN QUERY qr{&TableName}
      FOR EACH   {&TableName} NO-LOCK 
          where  {&TableName}.tipo-doc = "Apontamento MOB":U
          by {&TableName}.sequencia INDEXED-REPOSITION.

    RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetConstraintChave DBOProgram 
PROCEDURE SetConstraintChave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER p-cod-est-exec-ini like {&TableName}.cod-est-exec NO-UNDO.
    DEFINE INPUT  PARAMETER p-cod-est-exec-fim like {&TableName}.cod-est-exec NO-UNDO.
    DEFINE INPUT  PARAMETER p-num-ord-inv-ini  like {&TableName}.num-ord-inv  NO-UNDO.
    DEFINE INPUT  PARAMETER p-num-ord-inv-fim  like {&TableName}.num-ord-inv  NO-UNDO.
    DEFINE INPUT  PARAMETER p-num-ordem-ini    like {&TableName}.num-ordem    NO-UNDO.
    DEFINE INPUT  PARAMETER p-num-ordem-fim    like {&TableName}.num-ordem    NO-UNDO.
    DEFINE INPUT  PARAMETER p-num-projeto-ini  like {&TableName}.num-projeto  NO-UNDO.
    DEFINE INPUT  PARAMETER p-num-projeto-fim  like {&TableName}.num-projeto  NO-UNDO.
    DEFINE INPUT  PARAMETER p-sequencia-ini    like {&TableName}.sequencia    NO-UNDO.
    DEFINE INPUT  PARAMETER p-sequencia-fim    like {&TableName}.sequencia    NO-UNDO.

    assign v-cod-est-exec-ini = p-cod-est-exec-ini
           v-cod-est-exec-fim = p-cod-est-exec-fim
           v-num-ord-inv-ini  = p-num-ord-inv-ini
           v-num-ord-inv-fim  = p-num-ord-inv-fim
           v-num-ordem-ini    = p-num-ordem-ini
           v-num-ordem-fim    = p-num-ordem-fim
           v-num-projeto-ini  = p-num-projeto-ini
           v-num-projeto-fim  = p-num-projeto-fim
           v-sequencia-ini    = p-sequencia-ini
           v-sequencia-fim    = p-sequencia-fim.

    return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetConstraintData DBOProgram 
PROCEDURE SetConstraintData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER p-dt-trans-ini like {&TableName}.dt-trans NO-UNDO.
    DEFINE INPUT  PARAMETER p-dt-trans-fim like {&TableName}.dt-trans NO-UNDO.

    assign v-dt-trans-ini = p-dt-trans-ini
           v-dt-trans-fim = p-dt-trans-fim.

    return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateRecord DBOProgram 
PROCEDURE validateRecord :
/*:T------------------------------------------------------------------------------
  Purpose:     Valida»„es pertinentes ao DBO
  Parameters:  recebe o tipo de valida»’o (Create, Delete, Update)
  Notes:       
------------------------------------------------------------------------------*/
    
    DEFINE INPUT PARAMETER pType AS CHARACTER NO-UNDO.
    
    /*:T--- Utilize o par³metro pType para identificar quais as valida»„es a serem
          executadas ---*/
    /*:T--- Os valores poss­veis para o par³metro s’o: Create, Delete e Update ---*/
    /*:T--- Devem ser tratados erros PROGRESS e erros do Produto, atrav²s do 
          include: method/svc/errors/inserr.i ---*/
    /*:T--- Inclua aqui as valida»„es ---*/

    if pType = "Create" then do:
        if can-find (first bf{&TableName}
                     where bf{&TableName}.num-ordem    = RowObject.num-ordem   
                     and   bf{&TableName}.num-ord-inv  = RowObject.num-ord-inv 
                     and   bf{&TableName}.num-projeto  = RowObject.num-projeto 
                     and   bf{&TableName}.cod-est-exec = RowObject.cod-est-exec
                     and   bf{&TableName}.sequencia    = RowObject.sequencia) then do:
            {method/svc/errors/inserr.i &ErrorNumber     = "8"
                                        &ErrorType       = "EMS"
                                        &ErrorSubType    = "ERROR"
                                        &ErrorParameters = "'Ordem Investimento Espec­fica'"}   
    
        end.

        if RowObject.num-ordem    = 0  then do:
            {method/svc/errors/inserr.i &ErrorNumber     = "2"
                                        &ErrorType       = "EMS"
                                        &ErrorSubType    = "ERROR"
                                        &ErrorParameters = "'Ordem EMS'"}
        end.
        if RowObject.num-ord-inv  = 0  then do:
            {method/svc/errors/inserr.i &ErrorNumber     = "2"
                                        &ErrorType       = "EMS"
                                        &ErrorSubType    = "ERROR"
                                        &ErrorParameters = "'Ordem Investimento'"}
        end.
        if RowObject.num-projeto  = 0  then do:
            {method/svc/errors/inserr.i &ErrorNumber     = "2"
                                        &ErrorType       = "EMS"
                                        &ErrorSubType    = "ERROR"
                                        &ErrorParameters = "'Projeto'"}
        end.
        if RowObject.cod-est-exec = "" then do:
            {method/svc/errors/inserr.i &ErrorNumber     = "2"
                                        &ErrorType       = "EMS"
                                        &ErrorSubType    = "ERROR"
                                        &ErrorParameters = "'Estabelecimento Executor'"}
        end.

    end.

    /*:T--- Verifica ocorr¼ncia de erros ---*/
    IF CAN-FIND(FIRST RowErrors WHERE RowErrors.ErrorSubType = "ERROR":U) THEN
        RETURN "NOK":U.
    
    RETURN "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




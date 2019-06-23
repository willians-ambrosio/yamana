/* ---------------------------------------------------------------------------------------- *
|                                                                                           |
|  Sistema................: GRUPO DKP                                                       |
|  Modulo.................: MANUFATURA                                                      |
|  Programa...............: CONSOME-WS                                                      |
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
DEFINE VARIABLE hWebService             AS HANDLE       NO-UNDO.
DEFINE VARIABLE hwsIntYamanaGoldSoap    AS HANDLE       NO-UNDO.
DEFINE VARIABLE ListarItensResult       AS LONGCHAR     NO-UNDO.
DEFINE VARIABLE ListarItensResult1      AS LONGCHAR     NO-UNDO.
DEFINE VARIABLE cSourceType             AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cReadMode               AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lOverrideDefaultMapping AS LOGICAL      NO-UNDO.
DEFINE VARIABLE cFile                   AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cSchemaLocation         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cFieldTypeMapping       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cVerifySchemaMode       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lRetOK                  AS LOGICAL      NO-UNDO.
DEFINE VARIABLE httCust                 AS HANDLE       NO-UNDO. 
DEFINE VARIABLE lok                     AS LOGICAL      NO-UNDO.
DEFINE VARIABLE pTokenSeguranca         AS CHARACTER INIT "Y@M@NAG0LD_PRDA" NO-UNDO.
DEFINE VARIABLE retorno                 AS LOGICAL.
DEFINE VARIABLE hDoc                    AS HANDLE       NO-UNDO.
DEFINE VARIABLE lService                AS LOGICAL      NO-UNDO.
DEFINE VARIABLE cActiveNode             AS CHARACTER    NO-UNDO.
DEFINE VARIABLE hSax                    AS HANDLE       NO-UNDO.
DEFINE VARIABLE l-conectado             AS LOGICAL      NO-UNDO.
DEFINE VARIABLE GetDesafioResult        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE i-ep-codigo-usuario1    AS CHARACTER    NO-UNDO.
DEFINE VARIABLE dt-data-trans           AS DATE         NO-UNDO.
DEFINE VARIABLE c-hr-trans              AS CHARACTER    NO-UNDO.
/* ------------------------------------------------------------------------------------------------------ */
def new global shared var i-ep-codigo-usuario  like mguni.empresa.ep-codigo no-undo.
/* ------------------------------------------------------------------------------------------------------ */
FIND FIRST trad_org_ext WHERE trad_org_ext.cod_tip_unid_organ = "998"                  
                        AND   trad_org_ext.cod_matriz_trad_org_ext = "ems2"            
                        AND   trad_org_ext.cod_unid_organ = i-ep-codigo-usuario NO-LOCK NO-ERROR.    
IF AVAIL trad_org_ext THEN
   ASSIGN  i-ep-codigo-usuario1 =  trad_org_ext.cod_unid_organ_ext.
ELSE 
   ASSIGN i-ep-codigo-usuario1 = i-ep-codigo-usuario.
/* ------------------------------------------------------------------------------------------------------ */
DEFINE VARIABLE valor AS CHARACTER   NO-UNDO.
/* ------------------------------------------------------------------------------------------------------ */
FIND FIRST usuar_mestre NO-LOCK
    WHERE usuar_mestre.cod_usuario = USER("mguni") NO-ERROR.
/* ------------------------------------------------------------------------------------------------------ */
DEF TEMP-TABLE tabklass          
     FIELD IdKlassmatt                     AS INT  
     FIELD modo                            AS INT
     FIELD IdSIN                           AS INT
     FIELD Codigo                          AS CHAR FORMAT "x(16)"
     FIELD CodigoCompl                     AS CHAR FORMAT "x(20)"
     FIELD Origem                          AS INT  FORMAT ">9"
     FIELD Tipo                            AS CHAR FORMAT "99"
     FIELD UM                              AS CHAR FORMAT "xx"
     FIELD NCM                             AS CHAR FORMAT "9999.99.99"
     FIELD NCMExc                          AS DEC  FORMAT ">,>>9.99999999"
     FIELD Subgrupo                        AS CHAR FORMAT "x(8)"
     FIELD DsRes                           AS CHAR FORMAT "x(60)"
     FIELD DsComp                          AS CHAR FORMAT "x(2000)"
     FIELD GrEstoque                       AS INT  FORMAT ">9"
     FIELD Familia                         AS CHAR 
     FIELD EmpresaSol                      AS CHAR FORMAT "x(5)"
     FIELD EstabelecimentoSol              AS CHAR FORMAT "x(5)"
     FIELD QuestFiscalUsu                  AS CHAR FORMAT "x(12)"
     FIELD QuestFiscalData                 AS CHAR
     FIELD QuestFiscalHora                 AS CHAR
     FIELD FormSupCtrleQtdeEstoque         AS CHAR
     FIELD FormSupQtdeMinEstoque           AS CHAR
     FIELD FormSupFamComEqpto              AS CHAR
     FIELD FormSupDepart                   AS CHAR.
/* ------------------------------------------------------------------------------------------------------ */
DEF TEMP-TABLE QuestFiscal   
      FIELD seq                            AS INT FORMAT "999"
      FIELD IdKlassmatt                    AS INT
      FIELD estabelecimentosol             AS CHAR
      FIELD EmpresaSol                     AS CHAR 
      FIELD pergunta                       AS CHAR FORMAT "x(400)"                                
      FIELD resposta                       AS CHAR FORMAT "x(400)"
      FIELD DescricaoResposta              AS CHAR FORMAT "x(200)".       
/* ------------------------------------------------------------------------------------------------------ *
 * Limpa tabela de registro conexao Sintegra                                                              *
 * ------------------------------------------------------------------------------------------------------ */
FOR EACH es-integra-item WHERE 
         es-integra-item.empresasol  = i-ep-codigo-usuario1:
    DELETE es-integra-item.
END.
/* ------------------------------------------------------------------------------------------------------ */
/* FIM */
/* ------------------------------------------------------------------------------------------------------ */
CREATE SERVER hWebService.
/* ------------------------------------------------------------------------------------------------------ */
FIND FIRST es-param-tax NO-LOCK NO-ERROR.
/* ------------------------------------------------------------------------------------------------------ */
l-conectado = hWebService:CONNECT(es-param-tax.url-webservice). 
/* ------------------------------------------------------------------------------------------------------ */
IF l-conectado = NO THEN  RETURN.
/* ------------------------------------------------------------------------------------------------------ */
RUN wsIntYamanaGoldSoap SET hwsIntYamanaGoldSoap ON hWebService.
/* ------------------------------------------------------------------------------------------------------ */
ASSIGN pTokenSeguranca = "Y@M@NAG0LD_PRDA".
/* /* ------------------------------------------------------------------------------------------------------ */ */
/* RUN GetDesafio IN hwsIntYamanaGoldSoap(OUTPUT GetDesafioResult).                                             */
/* /* ------------------------------------------------------------------------------------------------------ */ */
/* RUN esp\desafio-md5.p (INPUT GetDesafioResult, OUTPUT pTokenSeguranca).                                      */
/* ------------------------------------------------------------------------------------------------------ */
/* Procedure invocation of ListarItens operation. */
/* ------------------------------------------------------------------------------------------------------ */
RUN ListarItens IN hwsIntYamanaGoldSoap(INPUT pTokenSeguranca, INPUT i-ep-codigo-usuario1, OUTPUT ListarItensResult).

/* ------------------------------------------------------------------------------------------------------ */
EMPTY TEMP-TABLE tabklass.
EMPTY TEMP-TABLE QuestFiscal.
/* ------------------------------------------------------------------------------------------------------ */
RUN esp\le-xml-com-acento.p (INPUT-OUTPUT TABLE tabklass,
                             INPUT-OUTPUT TABLE QuestFiscal,
                             INPUT ListarItensResult ).
/* ------------------------------------------------------------------------------------------------------ */
ASSIGN dt-data-trans = TODAY
       c-hr-trans    = STRING(TIME,"HH:MM:SS").

FOR EACH tabklass:

/*     IF tabklass.IdKlassmatt = 0 THEN NEXT. */

    FIND FIRST es-klassmatt-log-tab WHERE
               es-klassmatt-log-tab.IdKlassmatt = tabklass.IdKlassmatt AND
               es-klassmatt-log-tab.dt-trans    = dt-data-trans        AND 
               es-klassmatt-log-tab.hr-trans    = c-hr-trans           EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAIL es-klassmatt-log-tab THEN
    DO:
       CREATE es-klassmatt-log-tab.
       BUFFER-COPY tabklass EXCEPT IdKlassmatt
                                TO es-klassmatt-log-tab
                            ASSIGN es-klassmatt-log-tab.IdKlassmatt = tabklass.IdKlassmatt  
                                   es-klassmatt-log-tab.dt-trans    = dt-data-trans        
                                   es-klassmatt-log-tab.hr-trans    = c-hr-trans.
    END.
END.

FOR EACH QuestFiscal:

/*     IF QuestFiscal.IdKlassmatt = 0 THEN NEXT. */

    FIND FIRST es-klassmatt-log-qf WHERE
               es-klassmatt-log-qf.seq         = QuestFiscal.seq         AND  
               es-klassmatt-log-qf.IdKlassmatt = QuestFiscal.IdKlassmatt AND
               es-klassmatt-log-qf.dt-trans    = dt-data-trans           AND 
               es-klassmatt-log-qf.hr-trans    = c-hr-trans              EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAIL es-klassmatt-log-qf THEN
    DO:
       CREATE es-klassmatt-log-qf.
       BUFFER-COPY QuestFiscal EXCEPT IdKlassmatt seq
                                   TO es-klassmatt-log-qf
                               ASSIGN es-klassmatt-log-qf.seq         = QuestFiscal.seq
                                      es-klassmatt-log-qf.IdKlassmatt = QuestFiscal.IdKlassmatt  
                                      es-klassmatt-log-qf.dt-trans    = dt-data-trans        
                                      es-klassmatt-log-qf.hr-trans    = c-hr-trans.
    END.
END.
/* ------------------------------------------------------------------------------------------------------ */
FOR EACH tabklass WHERE tabklass.empresasol = i-ep-codigo-usuario1   :
    FIND FIRST es-integra-item WHERE es-integra-item.IdKlassmatt = tabklass.IdKlassmatt
                               AND   es-integra-item.empresasol  = tabklass.empresasol NO-LOCK  NO-ERROR.
    IF AVAIL es-integra-item THEN NEXT.
    CREATE es-integra-item.
    BUFFER-COPY tabklass TO es-integra-item.
END.
/* ------------------------------------------------------------------------------------------------------ */
FOR EACH QuestFiscal WHERE questfiscal.empresasol = i-ep-codigo-usuario1  BY QuestFiscal.IdKlassmatt  BY QuestFiscal.seq  :
    FIND FIRST es-integra-questionario WHERE es-integra-questionario.IdKlassmatt  = QuestFiscal.IdKlassmatt
                                       AND   es-integra-questionario.seq          = QuestFiscal.seq
                                       AND   es-integra-questionario.EmpresaSol   = QuestFiscal.EmpresaSol
                                       NO-LOCK NO-ERROR.
    IF AVAIL es-integra-questionario THEN NEXT.
    CREATE es-integra-questionario.
    BUFFER-COPY questfiscal TO es-integra-questionario.
END.
/* ------------------------------------------------------------------------------------------------------ *
 * handling methods - See OpenEdge Development: Working with XML guide for more info.                     *
 * warnings and errors                                                                                    *
 * ------------------------------------------------------------------------------------------------------ */
PROCEDURE Warning:
   DEFINE INPUT PARAMETER errMessage AS CHARACTER.
   MESSAGE errMessage VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.
/* ------------------------------------------------------------------------------------------------------ */
PROCEDURE Error:
   DEFINE INPUT PARAMETER errMessage AS CHARACTER.
   MESSAGE errMessage VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.
/* ------------------------------------------------------------------------------------------------------ */
PROCEDURE FatalError:
   DEFINE INPUT PARAMETER errMessage AS CHARACTER.
   MESSAGE errMessage VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.
/* ------------------------------------------------------------------------------------------------------ *
 * the common processing callbacks - these are used to identify where you are in                          *
 * the XML document, and what you're reading.                                                             *
 * ------------------------------------------------------------------------------------------------------ */
PROCEDURE Characters:
   DEFINE INPUT PARAMETER charData AS LONGCHAR.
   DEFINE INPUT PARAMETER numChars AS INTEGER.
 
   IF TRIM(chardata) = "" THEN LEAVE. /* discard whitespace used to format the XML */  

   CASE cActiveNode:                                                                                                                
      WHEN "IdKlassmatt"                      THEN ASSIGN tabklass.IdKlassmatt                = int(charData).
      when "IdKlassmatt"                      THEN ASSIGN QuestFiscal.IdKlassmatt             = int(chardata). 
      WHEN "Pergunta"                         THEN ASSIGN QuestFiscal.pergunta                = charData.
      WHEN "seq"                              THEN ASSIGN QuestFiscal.seq                     = int(chardata).
      WHEN "resposta"                         THEN ASSIGN QuestFiscal.resposta                = chardata .
      WHEN "DescricaoResposta"                THEN ASSIGN QuestFiscal.DescricaoResposta       = chardata.
      WHEN "Codigo"                           THEN ASSIGN tabklass.codigo                     = chardata.
      WHEN "DsRes"                            THEN ASSIGN tabklass.DsRes                      = chardata.
      WHEN "Subgrupo"                         THEN ASSIGN tabklass.Subgrupo                   = chardata.
      WHEN "CodigoCompl"                      THEN ASSIGN tabklass.CodigoCompl                = chardata.
      WHEN "Origem"                           THEN ASSIGN tabklass.Origem                     = int(chardata).
      WHEN "Tipo"                             THEN ASSIGN tabklass.Tipo                       = chardata.
      WHEN "UM"                               THEN ASSIGN tabklass.UM                         = chardata.
      WHEN "EstabelecimentoSol"               THEN ASSIGN tabklass.EstabelecimentoSol         = chardata.
      WHEN "NCM"                              THEN ASSIGN tabklass.NCM                        = chardata.
      WHEN "NCMExc"                           THEN ASSIGN tabklass.NCMExc                     = int(chardata).
      WHEN "DsComp"                           THEN ASSIGN tabklass.DsComp                     = chardata.
      WHEN "GrEstoque"                        THEN ASSIGN tabklass.GrEstoque                  = int(chardata).
      WHEN "Familia"                          THEN ASSIGN tabklass.familia                    = chardata.
      WHEN "EmpresaSol"                       THEN ASSIGN tabklass.EmpresaSol                 = chardata.
      WHEN "QuestFiscalUsu"                   THEN ASSIGN tabklass.QuestFiscalUsu             = chardata.
      WHEN "QuestFiscalData"                  THEN ASSIGN tabklass.QuestFiscalData            = chardata.
      WHEN "QuestFiscalHora"                  THEN ASSIGN tabklass.QuestFiscalHora            = chardata.
      WHEN "FormSupCtrleQtdeEstoque"          THEN ASSIGN tabklass.FormSupCtrleQtdeEstoque    = chardata.
      WHEN "FormSupQtdeMinEstoque"            THEN ASSIGN tabklass.FormSupQtdeMinEstoque      = chardata.
      WHEN "FormSupFamComEqpto"               THEN ASSIGN tabklass.FormSupFamComEqpto         = chardata.
      WHEN "FormSupDepart"                    THEN ASSIGN tabklass.FormSupDepart              = chardata.
      WHEN "modo"                             THEN ASSIGN tabklass.modo                       = int(chardata).
      WHEN "IdSIN"                            THEN ASSIGN TABklass.IdSIN                      = INT(chardata).
   END CASE. 

   IF cActiveNode =  "IdKlassmatt" THEN 
   DO: 
       IF AVAIL QuestFiscal  THEN
          ASSIGN QuestFiscal.IdKlassmatt = int(chardata). 
   END.

   IF cActiveNode =  "EmpresaSol" THEN 
   DO:               
       IF AVAIL QuestFiscal  THEN                         
       ASSIGN QuestFiscal.EmpresaSol = chardata.    
   END.                                                   
END PROCEDURE.
/* ------------------------------------------------------------------------------------------------------ */
PROCEDURE EndDocument:
END PROCEDURE.
/* ------------------------------------------------------------------------------------------------------ */
PROCEDURE EndElement:
   DEFINE INPUT PARAMETER namespaceURI AS CHARACTER.
   DEFINE INPUT PARAMETER localName AS CHARACTER.
   DEFINE INPUT PARAMETER qName AS CHARACTER.

   /* not actually used in this example */
END PROCEDURE.
/* ------------------------------------------------------------------------------------------------------ */
PROCEDURE EndPrefixMapping:
   DEFINE INPUT PARAMETER prefix AS CHARACTER.
   
   /* not actually used in this example */
END PROCEDURE.
/* ------------------------------------------------------------------------------------------------------ */
PROCEDURE IgnorableWhitespace:
   DEFINE INPUT PARAMETER charData AS CHARACTER.
   DEFINE INPUT PARAMETER numChars AS INTEGER.
   
   /* not actually used in this example */
END PROCEDURE.
/* ------------------------------------------------------------------------------------------------------ */
PROCEDURE NotationDecl:
   DEFINE INPUT PARAMETER name AS CHARACTER.
   DEFINE INPUT PARAMETER publicID AS CHARACTER.
   DEFINE INPUT PARAMETER systemID AS CHARACTER.
   
   /* not actually used in this example */
END PROCEDURE.
/* ------------------------------------------------------------------------------------------------------ */    
PROCEDURE StartDocument:
   /* not actually used in this example */
END PROCEDURE.
/* ------------------------------------------------------------------------------------------------------ */
PROCEDURE StartElement:
   DEFINE INPUT PARAMETER namespaceURI AS CHARACTER.
   DEFINE INPUT PARAMETER localName AS CHARACTER.
   DEFINE INPUT PARAMETER qName AS CHARACTER.
   DEFINE INPUT PARAMETER attributes AS HANDLE.

   /*MESSAGE qName VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
   CASE qName:

       /* New Product element = new record in the temp-table */
       WHEN "Material" THEN CREATE tabklass.

       WHEN "clsQuestionarioFiscal" THEN CREATE QuestFiscal.
    
       OTHERWISE cActiveNode = qName.
   END CASE.

END PROCEDURE.
/* ------------------------------------------------------------------------------------------------------ */
PROCEDURE StartPrefixMapping:
   DEFINE INPUT PARAMETER prefix AS CHARACTER.
   DEFINE INPUT PARAMETER uri AS CHARACTER.
   
   /* not actually used in this example */
END PROCEDURE.
/* ------------------------------------------------------------------------------------------------------ */
PROCEDURE UnparsedEntityDecl:
   DEFINE INPUT PARAMETER name AS CHARACTER.
   DEFINE INPUT PARAMETER publicID AS CHARACTER.
   DEFINE INPUT PARAMETER systemID AS CHARACTER.
   DEFINE INPUT PARAMETER notationName AS CHARACTER.
   
   /* not actually used in this example */
END PROCEDURE.
/* ------------------------------------------------------------------------------------------------------ */
PROCEDURE ProcessingInstruction:
   DEFINE INPUT PARAMETER target AS CHARACTER.
   DEFINE INPUT PARAMETER data AS CHARACTER.
   
   /* not actually used in this example */
END PROCEDURE.
/* ------------------------------------------------------------------------------------------------------ */
PROCEDURE ResolveEntity:
   DEFINE INPUT PARAMETER publicID AS CHARACTER.
   DEFINE INPUT PARAMETER systemID AS CHARACTER.
   DEFINE OUTPUT PARAMETER filePath AS CHARACTER.
   DEFINE OUTPUT PARAMETER memPointer AS LONGCHAR.

   /* not actually used in this example */
END PROCEDURE.
/* ------------------------------------------------------------------------------------------------------ */

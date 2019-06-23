&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ymof0117RP 12.01.19.002}
/* --------------------------------------------------------------------------- *
 *                               ALTERAÄÂES                                    *
 * --------------------------------------------------------------------------- * 
 * DATA.....: 08/06/2018                                                       *
 * AUTOR....: WILLIANS M AMBROSIO - GRUPO DKP                                  *
 * DESCRIÄ«O: Esse ponto foi comentado a pedido do Daniel, o tipo de controle  *
 *            n∆o pode ser alterado, o mesmo estava gerando um tipo erroneo    *
 * REVIS«O..: REV01                                                            *
 * --------------------------------------------------------------------------- *
 * DATA.....: SET/2018                                                         *
 * AUTOR....: WILLIANS M AMBROSIO - GRUPO DKP                                  *
 * DESCRIÄ«O: Adequaá∆o da rotina de importaá∆o para receber a inclus∆o de item*
 *........... oriundos do JDE via Klasmatt sem a regra de xx                   *
 * REVIS«O..: REV02                                                            *
 * --------------------------------------------------------------------------- */ 
 
{utp/ut-glob.i}

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> <m¢dulo>}
&ENDIF

/* ***************************  Definitions  ************************** */
&global-define programa nome-do-programa

def var c-liter-par                  as character format "x(13)":U.
def var c-liter-sel                  as character format "x(10)":U.
def var c-liter-imp                  as character format "x(12)":U.    
def var c-destino                    as character format "x(15)":U.

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG.

define temp-table tt-digita
    field ordem            as integer   format ">>>>9":U
    field exemplo          as character format "x(30)":U
    index id is primary unique
        ordem.

def temp-table tt-raw-digita
    field raw-digita as raw.
 
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

def var h-acomp         as handle no-undo.    

form
/*form-selecao-ini*/
    skip(1)
    c-liter-sel         no-label
    skip(1)
    /*form-selecao-usuario*/
    skip(1)
/*form-selecao-fim*/
/*form-parametro-ini*/
    skip(1)
    c-liter-par         no-label
    skip(1)
    /*form-parametro-usuario*/
    skip(1)
/*form-parametro-fim*/
/*form-impressao-ini*/
    skip(1)
    c-liter-imp         no-label
    skip(1)
    c-destino           colon 40 "-"
    tt-param.arquivo    no-label
    tt-param.usuario    colon 40
    skip(1)
/*form-impressao-fim*/
    with stream-io side-labels no-attr-space no-box width 132 frame f-impressao.

form
    /*campos-do-relatorio*/
     with no-box width 132 down stream-io frame f-relat.

create tt-param.
raw-transfer raw-param to tt-param.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

/*inicio-traducao*/
/*traducao-default*/
{utp/ut-liter.i PAR¬METROS * r}
assign c-liter-par = return-value.
{utp/ut-liter.i SELE«√O * r}
assign c-liter-sel = return-value.
{utp/ut-liter.i IMPRESS√O * r}
assign c-liter-imp = return-value.
{utp/ut-liter.i Destino * l}
assign c-destino:label in frame f-impressao = return-value.
{utp/ut-liter.i Usu·rio * l}
assign tt-param.usuario:label in frame f-impressao = return-value.   
/*fim-traducao*/

{include/i-rpvar.i}

/* find empresa                                     */
/*     where empresa.ep-codigo = v_cdn_empres_usuar */
/*     no-lock no-error.                            */
find first param-global no-lock no-error.

{utp/ut-liter.i titulo_sistema * }
assign c-sistema = return-value.
{utp/ut-liter.i titulo_relatorio * }
assign c-titulo-relat = return-value.
assign c-empresa     = param-global.grupo
       c-programa    = "{&programa}":U
       c-versao      = "1.00":U
       c-revisao     = "000"
       c-destino     = {varinc/var00002.i 04 tt-param.destino}.


def temp-table tt-versao-integr no-undo
       field cod-versao-integracao as integer format "999"
       field ind-origem-msg        as integer format "99".
 
def temp-table tt-erros-geral no-undo
       field identif-msg           as char    format "x(60)"
       field num-sequencia-erro    as integer format "999"
       field cod-erro              as integer format "99999"   
       field des-erro              as char    format "x(60)"
       field cod-maq-origem        as integer format "999"
       field num-processo          as integer format "999999999".
 

/* {cdp/cdapi244.i} */
def temp-table tt-item no-undo like item 
    field cod-maq-origem   as   integer format "9999"
    field num-processo     as   integer format ">>>>>>>>9" initial 0
    field num-sequencia    as   integer format ">>>>>9"    initial 0
    field ind-tipo-movto   as   integer format "99"        initial 1
    field cod-erro         as   integer format "99999" 
    field des-erro         as   char    format "x(60)"
    INDEX ch-codigo IS PRIMARY  cod-maq-origem
                                num-processo
                                num-sequencia.

DEFINE VARIABLE C-HORA AS CHARACTER   NO-UNDO.
DEFINE VARIABLE C-DATA AS CHARACTER   NO-UNDO.


DEFINE VARIABLE c-log       AS CHARACTER  FORMAT "x(400)"  NO-UNDO.
DEFINE VARIABLE i-contapipe AS INTEGER     NO-UNDO.


DEFINE VARIABLE  i-ep-codigo-usuario1 AS CHARACTER   NO-UNDO.
def new global shared var i-ep-codigo-usuario  like mguni.empresa.ep-codigo no-undo.

FIND FIRST trad_org_ext WHERE trad_org_ext.cod_tip_unid_organ = "998"                  
                        AND   trad_org_ext.cod_matriz_trad_org_ext = "ems2"            
                        AND   trad_org_ext.cod_unid_organ = i-ep-codigo-usuario NO-LOCK NO-ERROR.    
IF AVAIL trad_org_ext THEN DO:                                                         
    ASSIGN  i-ep-codigo-usuario1 =  trad_org_ext.cod_unid_organ_ext.
END.
ELSE ASSIGN i-ep-codigo-usuario1 = i-ep-codigo-usuario.

DEF TEMP-TABLE tt-mov-ext-item-cfa LIKE es-movto-ext-item-cfa.


define temp-table tt-paramCD0205 no-undo
    field destino          as integer
    field arquivo          as char
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field l-todos          as logical.

define temp-table tt-digitaCD0205 no-undo
    field it-codigo     like item.it-codigo
    field descricao     as char FORMAT "x(60)"
    field un            like item.un
    field new-it-codigo like item.it-codigo
    field new-un        like item.un
    field fator-conv    as decimal FORMAT ">,>>9.99999"
    field ge-codigo     like item.ge-codigo
    field old-ge-codigo like item.ge-codigo
    field tipo-operacao as char FORMAT "x".


def temp-table tt-raw-digitaCD0205
   field raw-digita      as raw.

define temp-table tt-param1 no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure Template
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 6.54
         WIDTH              = 39.86.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{include/i-rpcab.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

do on stop undo, leave:
    {include/i-rpout.i}
   /*  view frame f-cabec.                                                                       */
   /*  view frame f-rodape.                                                                      */
   /*  run utp/ut-acomp.p persistent set h-acomp.                                                */
   /*                                                                                            */
   /*  {utp/ut-liter.i aaaaaaaaaaaaaaaaaa bbb c}                                                 */
   /*                                                                                            */
   /*  run pi-inicializar in h-acomp (input "xxxxxxxxxxxxxx":U).                                 */
   /*                                                                                            */
   /*  /*:T --- Colocar aqui o cÛdigo de impress„o --- */                                        */
   /*  for each [TABELA] no-lock                                                                 */
   /*      where [WHERE].                                                                        */
   /*                                                                                            */
   /*      run pi-acompanhar in h-acomp (input "xxxxxxxxxxxxxx":U).                              */
   /*  end.                                                                                      */
   /*                                                                                            */
   /*  run pi-finalizar in h-acomp.                                                              */
                                                                           

    RUN pi-processa.

   {include/i-rpclo.i} 


end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-altera-item) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-altera-item Procedure 
PROCEDURE pi-altera-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR FIRST ITEM fields(it-codigo char-2 cod-servico codigo-orig data-implant dec-1 fm-cod-com fm-codigo ge-codigo quant-segur tipo-contr) NO-LOCK
    WHERE ITEM.it-codigo = es-integra-item.codigo .
END.

IF NOT AVAIL ITEM THEN 
DO:
   CREATE es-integra-retorno.                                                           
   ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.
          es-integra-retorno.ep-codigo       = es-integra-item.empresasol.          
          es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
          es-integra-retorno.idsin           = es-integra-item.idsin.
          es-integra-retorno.dt-carga        = TODAY.                                   
          es-integra-retorno.dt-int-erp      = TODAY.                                   
          es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.             
          es-integra-retorno.log-retorno     = "Item nao existe no ERP".        
          es-integra-retorno.statusRetorno   = "N".                                     
                                                                                        
   RUN pi-grava-log-monitor.

   FOR EACH es-integra-questionario OF es-integra-item:                                 
       DELETE es-integra-questionario.                                                  
   END.
   DELETE es-integra-item.
   NEXT.
END.

RUN pi-replica-item.
/* RUN esp/ymcd0203.p. */


CREATE es-integra-retorno.                                                                
ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.
       es-integra-retorno.ep-codigo       = es-integra-item.empresasol.          
       es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
       es-integra-retorno.idsin           = es-integra-item.idsin.
       es-integra-retorno.dt-carga        = TODAY.                                        
       es-integra-retorno.dt-int-erp      = TODAY.                                        
       es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.                  
       es-integra-retorno.log-retorno     = "Alteracao de Item submetita ao ERP com sucesso!!!".                     
       es-integra-retorno.statusRetorno   = "S".                                          
                                                                                          
RUN pi-grava-log-monitor.

FOR EACH es-integra-questionario OF es-integra-item:                                      
    DELETE es-integra-questionario.                                                       
END.                                                                                      
DELETE es-integra-item.
NEXT.                                                                                     



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-carrega) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega Procedure 
PROCEDURE pi-carrega :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE c-modo      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCaminhoLog AS CHARACTER   NO-UNDO.

ASSIGN cCaminhoLog = SESSION:TEMP-DIRECTORY + "LOG-YMOF0117RP" + STRING(TODAY,"99999999") + REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".log".

OUTPUT TO VALUE(cCaminhoLog) NO-CONVERT.
PUT UNFORMATTED "es-integra-item.codigo; es-integra-item.CodigoCompl ;es-integra-item.DsComp ;es-integra-item.DsRes ;es-integra-item.dt-carga ;es-integra-item.dt-int-erp ;es-integra-item.EmpresaSol ;es-integra-item.EstabelecimentoSol ;es-integra-item.Familia ;es-integra-item.FormSupCtrleQtdeEstoque ;es-integra-item.FormSupDepart ;es-integra-item.FormSupFamComEqpto ;es-integra-item.FormSupQtdeMinEstoque ;es-integra-item.GrEstoque ;es-integra-item.IdKlassmatt ;es-integra-item.IdSIN ;es-integra-item.modo ;es-integra-item.NCM ;es-integra-item.NCMExc ;es-integra-item.Origem ;es-integra-item.QuestFiscalData ;es-integra-item.QuestFiscalHora ;es-integra-item.QuestFiscalUsu ;es-integra-item.Subgrupo ;es-integra-item.Tipo ;es-integra-item.UM;VALIDO?" SKIP.
FOR EACH es-integra-item WHERE es-integra-item.empresasol = i-ep-codigo-usuario1 NO-LOCK: 

    FIND FIRST es-integra-retorno WHERE es-integra-retorno.ep-codigo   = es-integra-item.empresasol
                                  AND   es-integra-retorno.idklassmatt = es-integra-item.idklassmatt
                                  AND   es-integra-retorno.dt-ret = ?
                                  NO-LOCK NO-ERROR.
    EXPORT DELIMITER ";"         
        es-integra-item.codigo es-integra-item.CodigoCompl es-integra-item.DsComp es-integra-item.DsRes es-integra-item.dt-carga es-integra-item.dt-int-erp es-integra-item.EmpresaSol es-integra-item.EstabelecimentoSol es-integra-item.Familia es-integra-item.FormSupCtrleQtdeEstoque es-integra-item.FormSupDepart es-integra-item.FormSupFamComEqpto es-integra-item.FormSupQtdeMinEstoque es-integra-item.GrEstoque es-integra-item.IdKlassmatt es-integra-item.IdSIN es-integra-item.modo es-integra-item.NCM es-integra-item.NCMExc es-integra-item.Origem es-integra-item.QuestFiscalData es-integra-item.QuestFiscalHora es-integra-item.QuestFiscalUsu es-integra-item.Subgrupo es-integra-item.Tipo es-integra-item.UM AVAIL es-integra-retorno.
END.
OUTPUT CLOSE.

Gravar:
FOR EACH es-integra-item WHERE es-integra-item.empresasol = i-ep-codigo-usuario1 SHARE-LOCK 
    TRANSACTION ON ERROR UNDO Gravar, NEXT Gravar:

    FIND FIRST es-integra-retorno WHERE es-integra-retorno.ep-codigo   = es-integra-item.empresasol
                                  AND   es-integra-retorno.idklassmatt = es-integra-item.idklassmatt
                                  AND   es-integra-retorno.dt-ret = ?
                                  NO-LOCK NO-ERROR.

    IF AVAIL es-integra-retorno THEN NEXT.

    IF substring(es-integra-item.codigo,1,2) = "XX"
    AND (es-integra-item.modo    = 2 OR es-integra-item.modo = 3
         /*OR es-integra-item.modo = 4*/ )  THEN DO:

        ASSIGN c-modo = "".

        CASE es-integra-item.modo:
            WHEN 2 THEN ASSIGN c-modo = "Altera Item" .
            WHEN 3 THEN ASSIGN c-modo = "Expans∆o Item" .
            WHEN 4 THEN ASSIGN c-modo = "Reenvio Item" .
        END CASE.         

        CREATE es-integra-retorno.                                                           
        ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.
               es-integra-retorno.ep-codigo       = es-integra-item.empresasol.          
               es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
               es-integra-retorno.idsin           = es-integra-item.idsin.
               es-integra-retorno.dt-carga        = TODAY.                                   
               es-integra-retorno.dt-int-erp      = TODAY.                                   
               es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.             
               es-integra-retorno.log-retorno     = "Item em processo de cadastramento, n∆o pode ser enviado com o modo: " + c-modo.        
               es-integra-retorno.statusRetorno   = "N".                                     
                                                                                             
        RUN pi-grava-log-monitor.

        FOR EACH es-integra-questionario OF es-integra-item:                                 
            DELETE es-integra-questionario.                                                  
        END.
        DELETE es-integra-item.
        NEXT.
    END.

    CASE es-integra-item.modo:
        WHEN 1 THEN RUN pi-cria-item (INPUT "inclusao").
        WHEN 2 THEN RUN pi-altera-item.
        WHEN 3 THEN RUN pi-expansao.
        WHEN 4 THEN RUN pi-ret-tax.
        /*Begins REV02*/
        WHEN 5 THEN RUN pi-cria-item-jde (INPUT "inclusao").
        /* End REV02 */
    END CASE.

    IF AVAIL es-integra-item THEN DELETE es-integra-item.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-console-xml-integra) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-console-xml-integra Procedure 
PROCEDURE pi-console-xml-integra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN esp\consome-ws.p.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-cria-item) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-item Procedure 
PROCEDURE pi-cria-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE i-ident1 AS INTEGER     NO-UNDO.

DEFINE INPUT PARAMETER c-modo AS CHARACTER FORMAT "x(25)"   NO-UNDO.
DEFINE VARIABLE c-tp-item AS CHARACTER   NO-UNDO.

  RUN pi-erros-cria-item.
 
  criar-item:
  DO TRANSACTION ON ERROR UNDO criar-item:

      EMPTY TEMP-TABLE tt-item.
      EMPTY TEMP-TABLE tt-erros-geral.
    
      FIND FIRST PARAM-estoq NO-LOCK NO-ERROR.
    
      CREATE tt-item.                                                                                      
      ASSIGN tt-item.it-codigo            = es-integra-item.codigo.
             tt-item.fm-cod-com           = IF es-integra-item.FormSupFamComEqpto = "NA" THEN "" ELSE  es-integra-item.FormSupFamComEqpto.
             tt-item.desc-item            = es-integra-item.DsRes. 
             tt-item.narrativa            = replace(es-integra-item.dscomp,"CHR(10)",CHR(10)).
             tt-item.codigo-refer         = es-integra-item.CodigoCompl.
             SUBSTR(tt-item.char-2,212,1) = es-integra-item.tipo.
             tt-item.dec-1                = es-integra-item.NCMExc.
             tt-item.un                   = es-integra-item.um.
             tt-item.codigo-orig          = es-integra-item.origem.
             tt-item.class-fiscal         = es-integra-item.ncm.
             tt-item.fm-codigo            = es-integra-item.Familia.                                                               
             tt-item.ge-codigo            = es-integra-item.GrEstoque.                                                                  
             tt-item.cod-estabel          =  IF AVAIL PARAM-estoq  THEN param-estoq.estabel-pad ELSE "".
             tt-item.cod-obsoleto         = 4.                                                                   
             tt-item.un                   = es-integra-item.um .                                                                
             tt-item.ind-tipo-movto       = IF c-modo = "inclusao" THEN 1 ELSE  2. /*1 inclusao / 2 altera?ío*/                                      
    
      
      CREATE tt-versao-integr.                                    
      ASSIGN tt-versao-integr.cod-versao-integracao   =   001     
             tt-versao-integr.ind-origem-msg          =   01.   
                                                                   
      RUN cdp/cdapi344.p ( INPUT  TABLE  tt-versao-integr,          
                           OUTPUT TABLE tt-erros-geral,           
                           INPUT-OUTPUT TABLE tt-item).
    
    
      FIND FIRST  tt-erros-geral NO-LOCK  NO-ERROR.
      IF AVAIL tt-erros-geral THEN DO:
    
          ASSIGN i-contapipe = 1
                 c-log       = "".
    
          FOR EACH tt-erros-geral:
    
              IF i-contapipe = 1 THEN 
                  ASSIGN c-log = c-log + STRING(tt-erros-geral.cod-erro) + "-" + tt-erros-geral.des-erro.
              ELSE IF i-contapipe = 2 THEN c-log = c-log  + "|" + STRING(tt-erros-geral.cod-erro) + "-" + tt-erros-geral.des-erro + "|".
              ELSE c-log = c-log  + STRING(tt-erros-geral.cod-erro) + "-" + tt-erros-geral.des-erro + "|".
              ASSIGN i-contapipe = i-contapipe + 1.
    
          END.
          CREATE es-integra-retorno.                                                                  
          ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.
                 es-integra-retorno.ep-codigo       = es-integra-item.empresasol.          
                 es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
                 es-integra-retorno.idsin           = es-integra-item.idsin.
                 es-integra-retorno.dt-carga        = TODAY.                                          
                 es-integra-retorno.dt-int-erp      = TODAY.                                          
                 es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.                    
                 es-integra-retorno.log-retorno     =  c-log. 
                 es-integra-retorno.statusRetorno   = "N".      

          RUN pi-grava-log-monitor.

          FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:      
              DELETE es-integra-questionario.                       
          END.
          DELETE es-integra-item.
          NEXT.
      END.
  
      /*FIND ITEM WHERE ITEM.it-codigo = es-integra-item.codigo EXCLUSIVE-LOCK NO-ERROR.*/
      
      FOR FIRST ITEM FIELDS( class-fiscal tipo-contr char-2 it-codigo aliquota-ipi codigo-refer) EXCLUSIVE-LOCK
          WHERE ITEM.it-codigo = es-integra-item.codigo:
      END.
      IF AVAIL ITEM THEN DO:
          ASSIGN ITEM.class-fiscal = es-integra-item.ncm.
          /* Begins Rev01 - Esse ponto foi comentado a pedido do Daniel, o tipo de controle n∆o pode ser alterado, o mesmo estava gerando um tipo erroneo. */
/*                  ITEM.tipo-contr = IF es-integra-item.FormSupCtrleQtdeEstoque = "0" THEN 4 ELSE 2. */
          /* end Rev01 */
          CASE es-integra-item.tipo:
               WHEN "10" THEN ASSIGN c-tp-item = "a".
               WHEN "99" THEN ASSIGN c-tp-item = "B".
               OTHERWISE ASSIGN c-tp-item = es-integra-item.tipo.
          END CASE.

          ASSIGN SUBSTR(ITEM.char-2,212,1) = c-tp-item. 
    
          FIND classif-fisc WHERE classif-fisc.class-fisc = es-integra-item.ncm NO-LOCK NO-ERROR.
          IF AVAIL classif-fisc THEN DO:
              FIND item-dist WHERE item-dist.it-codigo = ITEM.it-codigo EXCLUSIVE-LOCK NO-ERROR.
              IF AVAIL item-dist THEN DO:
                  ASSIGN item-dist.idi-tip-apurac-ipi = classif-fisc.idi-tip-apurac-ipi.
              END.
    
              ASSIGN ITEM.aliquota-ipi = classif-fisc.aliquota-ipi.
          END.
    
          FOR EACH item-uni-estab WHERE item-uni-estab.it-codigo = ITEM.it-codigo:
              ASSIGN item-uni-estab.cod-obsoleto = 4.
          END.
    
          ASSIGN ITEM.codigo-refer = es-integra-item.CodigoCompl. 
    
          /* ------------------------------------------------------------------------------ */
          /* Begins Jan/2019 - Willians Ambrosio DKP                                        */             
          /* ------------------------------------------------------------------------------ */
          FIND FIRST es-klassmatt-integr WHERE
                     es-klassmatt-integr.idklassmatt = es-integra-item.idklassmatt                 AND
                     es-klassmatt-integr.dt-trans    = TODAY                                       AND
                     es-klassmatt-integr.hr-trans    = REPLACE(STRING(TIME,"HH:MM:SS"),":","")     EXCLUSIVE-LOCK NO-ERROR.
          IF NOT AVAIL es-klassmatt-integr THEN
          DO:
             CREATE es-klassmatt-integr.
             BUFFER-COPY es-integra-item      TO es-klassmatt-integr
                                          ASSIGN es-klassmatt-integr.idklassmatt = es-integra-item.idklassmatt                                
                                                 es-klassmatt-integr.dt-trans    = TODAY                                   
                                                 es-klassmatt-integr.hr-trans    = STRING(TIME,"HH:MM:SS") 
                                                 es-klassmatt-integr.log-retorno   = "YMOF0117RP.P - TIPO DE CONTROLE ATUALIZADO Tipo-Contr = "  + STRING(ITEM.tipo-contr)      
                                                 es-klassmatt-integr.statusRetorno = "N".             
          END.   
          /* ------------------------------------------------------------------------------ */
          /* End Jan/2019 - Willians Ambrosio DKP                                           */
          /* ------------------------------------------------------------------------------ */
    
      END.
  
      FIND es-it-depto WHERE es-it-depto.it-codigo = es-integra-item.codigo NO-LOCK NO-ERROR.
      IF NOT AVAIL es-it-depto THEN DO:
          CREATE es-it-depto.
                 es-it-depto.cod-depto      =  int(es-integra-item.FormSupDepart).
                 es-it-depto.data           = TODAY.
                 es-it-depto.data-altera    = TODAY.
                 es-it-depto.hora           = STRING(TIME,"hh:mm").
                 es-it-depto.hr-altera      = STRING(TIME,"hh:mm").
                 es-it-depto.it-codigo      = es-integra-item.codigo.
                 es-it-depto.origem         = "Carga via Web Service".
                 es-it-depto.usuar-altera   = "Sistema".
                 es-it-depto.usuario        = "Sistema".
      END.
    
    
      ASSIGN c-hora = STRING(TIME,"hh:mm").
             c-data = STRING(TODAY,"99/99/9999").
    
      CREATE ES-MOVTO-EXT-ITEM-CFA.                                                            
       ASSIGN es-movto-ext-item-cfa.classe            = "9999" .        
              es-movto-ext-item-cfa.dt-movto          = TODAY.                                 
              es-movto-ext-item-cfa.ep-codigo         = es-integra-item.empresasol.        
              es-movto-ext-item-cfa.hr-movto          = c-hora.                                 
              es-movto-ext-item-cfa.identificador     = 1 .                                     
              es-movto-ext-item-cfa.it-codigo         = es-integra-item.codigo.        
              es-movto-ext-item-cfa.pergunta-literal  = IF c-modo = "inclusao" THEN  "IMPLANTAÄ«O DO ITEM NO DATASUL" ELSE "IMPLATAÄAO DE EXPANSAO NO DATASUL".                      
              es-movto-ext-item-cfa.resposta-literal  = IF c-modo = "inclusao" THEN  "IMPLANTAÄ«O DO ITEM NO DATASUL" ELSE "IMPLATAÄAO DE EXPANSAO NO DATASUL".                     
              es-movto-ext-item-cfa.usuario           = "SISTEMA".
    
      FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item BY es-integra-questionario.seq:
    
          FIND FIRST es-movto-ext-item-cfa  WHERE es-movto-ext-item-cfa.it-codigo     = es-integra-item.codigo
                                            AND   es-movto-ext-item-cfa.ep-codigo     = es-integra-item.empresasol
                                            AND   es-movto-ext-item-cfa.nr-seq        = es-integra-questionario.seq
                                            AND   es-movto-ext-item-cfa.identificador = 2  EXCLUSIVE-LOCK NO-ERROR.
    
          IF NOT AVAIL es-movto-ext-item-cfa THEN DO:
              CREATE es-movto-ext-item-cfa.
                     es-movto-ext-item-cfa.classe                 = "9999".
                     es-movto-ext-item-cfa.dt-movto               = date(es-integra-item.QuestFiscalData).
                     es-movto-ext-item-cfa.ep-codigo              = es-integra-item.empresasol.
                     es-movto-ext-item-cfa.hr-movto               = es-integra-item.QuestFiscalHora.
                     es-movto-ext-item-cfa.identificador          = 2.
                     es-movto-ext-item-cfa.it-codigo              = es-integra-item.codigo.
                     es-movto-ext-item-cfa.nr-seq                 = es-integra-questionario.seq.
                     es-movto-ext-item-cfa.pergunta-literal       = es-integra-questionario.pergunta.
                     es-movto-ext-item-cfa.resposta-literal       = IF es-integra-questionario.seq = 204 THEN  es-integra-questionario.resposta  ELSE es-integra-questionario.resposta + "|" + es-integra-questionario.DescricaoResposta.
                     es-movto-ext-item-cfa.usuario                = es-integra-item.QuestFiscalUsu.
          END.
          ELSE DO:
                ASSIGN es-movto-ext-item-cfa.pergunta-literal = es-integra-questionario.pergunta.
                       es-movto-ext-item-cfa.resposta-literal = IF es-integra-questionario.seq = 204 THEN  es-integra-questionario.resposta  ELSE es-integra-questionario.resposta + "|" + es-integra-questionario.DescricaoResposta.
                           
          END.
      END.
    
      FIND FIRST ext-item-cfa WHERE ext-item-cfa.it-codigo   = es-integra-item.codigo
                              AND   ext-item-cfa.ep-codigo   = es-integra-item.empresasol
                              AND   ext-item-cfa.IdKlassmatt = es-integra-item.IdKlassmatt
                              EXCLUSIVE-LOCK NO-ERROR.
    
      IF NOT AVAIL ext-item-cfa  THEN DO:
            CREATE ext-item-cfa.                                                        
            ASSIGN ext-item-cfa.it-codigo   = es-integra-item.codigo.                   
                   ext-item-cfa.ep-codigo   = es-integra-item.empresasol.               
                   ext-item-cfa.IdKlassmatt = es-integra-item.IdKlassmatt.              
                   ext-item-cfa.IdSIN       = es-integra-item.IdSIN.                    
                   ext-item-cfa.classe      = "9999".
      END.
      ELSE DO:
           ext-item-cfa.classe      = "9999".
      END.
    
      CREATE es-integra-retorno.                                                                                              
      ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo. 
             es-integra-retorno.ep-codigo       = es-integra-item.empresasol.        
             es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
             es-integra-retorno.idsin           = es-integra-item.idsin.
             es-integra-retorno.dt-carga        = TODAY.                                                                      
             es-integra-retorno.dt-int-erp      = TODAY.                                                                      
             es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.                                                
             es-integra-retorno.log-retorno     =  IF c-modo = "inclusao" THEN  "Item integrado com sucesso no ERP" ELSE  "Expans∆o do item " + es-integra-item.codigo + " integrada com sucesso!!!" .          
             es-integra-retorno.statusRetorno   = "S".

      RUN pi-grava-log-monitor.
    
      FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:
          DELETE es-integra-questionario.                                   
      END.                                                                  
      DELETE es-integra-item.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-cria-item-jde) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-item-jde Procedure 
PROCEDURE pi-cria-item-jde :
/*------------------------------------------------------------------------------
  Purpose:  Begins REV02
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE i-ident1 AS INTEGER     NO-UNDO.

DEFINE INPUT PARAMETER c-modo AS CHARACTER FORMAT "x(25)"   NO-UNDO.
DEFINE VARIABLE c-tp-item AS CHARACTER   NO-UNDO.

  RUN pi-erros-cria-item-jde.
 
  criar-item:
  DO TRANSACTION ON ERROR UNDO criar-item:

      EMPTY TEMP-TABLE tt-item.
      EMPTY TEMP-TABLE tt-erros-geral.
    
      FIND FIRST PARAM-estoq NO-LOCK NO-ERROR.

    
      CREATE tt-item.                                                                                      
      ASSIGN tt-item.it-codigo            = es-integra-item.codigo.
             tt-item.fm-cod-com           = IF es-integra-item.FormSupFamComEqpto = "NA" THEN "" ELSE  es-integra-item.FormSupFamComEqpto.
             tt-item.desc-item            = es-integra-item.DsRes. 
             tt-item.narrativa            = replace(es-integra-item.dscomp,"CHR(10)",CHR(10)) + "ORIGEM JDE".
             tt-item.codigo-refer         = es-integra-item.CodigoCompl.
             SUBSTR(tt-item.char-2,212,1) = es-integra-item.tipo.
             tt-item.dec-1                = es-integra-item.NCMExc.
             tt-item.un                   = es-integra-item.um.
             tt-item.codigo-orig          = es-integra-item.origem.
             tt-item.class-fiscal         = es-integra-item.ncm.
             tt-item.fm-codigo            = es-integra-item.Familia.                                                               
             tt-item.ge-codigo            = es-integra-item.GrEstoque.                                                                  
             tt-item.cod-estabel          =  IF AVAIL PARAM-estoq  THEN param-estoq.estabel-pad ELSE "".
             tt-item.cod-obsoleto         = 4.                                                                   
             tt-item.un                   = es-integra-item.um .                                                                
             tt-item.ind-tipo-movto       = IF c-modo = "inclusao" THEN 1 ELSE  2. /*1 inclusao / 2 altera?ío*/                                      
    
      
      CREATE tt-versao-integr.                                    
      ASSIGN tt-versao-integr.cod-versao-integracao   =   001     
             tt-versao-integr.ind-origem-msg          =   01.   
                                                                   
      RUN cdp/cdapi344.p ( INPUT  TABLE  tt-versao-integr,          
                           OUTPUT TABLE tt-erros-geral,           
                           INPUT-OUTPUT TABLE tt-item).
    
    
      FIND FIRST  tt-erros-geral NO-LOCK  NO-ERROR.
      IF AVAIL tt-erros-geral THEN DO:
    
          ASSIGN i-contapipe = 1
                 c-log       = "".
    
          FOR EACH tt-erros-geral:
    
              IF i-contapipe = 1 THEN 
                  ASSIGN c-log = c-log + STRING(tt-erros-geral.cod-erro) + "-" + tt-erros-geral.des-erro.
              ELSE IF i-contapipe = 2 THEN c-log = c-log  + "|" + STRING(tt-erros-geral.cod-erro) + "-" + tt-erros-geral.des-erro + "|".
              ELSE c-log = c-log  + STRING(tt-erros-geral.cod-erro) + "-" + tt-erros-geral.des-erro + "|".
              ASSIGN i-contapipe = i-contapipe + 1.
    
          END.

          CREATE es-integra-retorno.                                                                  
          ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.
                 es-integra-retorno.ep-codigo       = es-integra-item.empresasol.          
                 es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
                 es-integra-retorno.idsin           = es-integra-item.idsin.
                 es-integra-retorno.dt-carga        = TODAY.                                          
                 es-integra-retorno.dt-int-erp      = TODAY.                                          
                 es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.                    
                 es-integra-retorno.log-retorno     =  c-log. 
                 es-integra-retorno.statusRetorno   = "N".                                            
          
          RUN pi-grava-log-monitor.

          FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:      
              DELETE es-integra-questionario.                       
          END.
          DELETE es-integra-item.
          NEXT.
      END.
  
      /*FIND ITEM WHERE ITEM.it-codigo = es-integra-item.codigo EXCLUSIVE-LOCK NO-ERROR.*/
      
      FOR FIRST ITEM FIELDS( class-fiscal tipo-contr char-2 it-codigo aliquota-ipi codigo-refer) EXCLUSIVE-LOCK
          WHERE ITEM.it-codigo = es-integra-item.codigo:
      END.
      IF AVAIL ITEM THEN DO:
          ASSIGN ITEM.class-fiscal = es-integra-item.ncm.
          /* Begins Rev01 - Esse ponto foi comentado a pedido do Daniel, o tipo de controle n∆o pode ser alterado, o mesmo estava gerando um tipo erroneo. */
/*                  ITEM.tipo-contr = IF es-integra-item.FormSupCtrleQtdeEstoque = "0" THEN 4 ELSE 2. */
          /* end Rev01 */
          CASE es-integra-item.tipo:
               WHEN "10" THEN ASSIGN c-tp-item = "a".
               WHEN "99" THEN ASSIGN c-tp-item = "B".
               OTHERWISE ASSIGN c-tp-item = es-integra-item.tipo.
          END CASE.

          ASSIGN SUBSTR(ITEM.char-2,212,1) = c-tp-item. 
    
          FIND classif-fisc WHERE classif-fisc.class-fisc = es-integra-item.ncm NO-LOCK NO-ERROR.
          IF AVAIL classif-fisc THEN DO:
              FIND item-dist WHERE item-dist.it-codigo = ITEM.it-codigo EXCLUSIVE-LOCK NO-ERROR.
              IF AVAIL item-dist THEN DO:
                  ASSIGN item-dist.idi-tip-apurac-ipi = classif-fisc.idi-tip-apurac-ipi.
              END.
    
              ASSIGN ITEM.aliquota-ipi = classif-fisc.aliquota-ipi.
          END.
    
          FOR EACH item-uni-estab WHERE item-uni-estab.it-codigo = ITEM.it-codigo:
              ASSIGN item-uni-estab.cod-obsoleto = 4.
          END.
    
          ASSIGN ITEM.codigo-refer = es-integra-item.CodigoCompl. 
    
    
    
      END.
  
      FIND es-it-depto WHERE es-it-depto.it-codigo = es-integra-item.codigo NO-LOCK NO-ERROR.
      IF NOT AVAIL es-it-depto THEN DO:
          CREATE es-it-depto.
                 es-it-depto.cod-depto      =  int(es-integra-item.FormSupDepart).
                 es-it-depto.data           = TODAY.
                 es-it-depto.data-altera    = TODAY.
                 es-it-depto.hora           = STRING(TIME,"hh:mm").
                 es-it-depto.hr-altera      = STRING(TIME,"hh:mm").
                 es-it-depto.it-codigo      = es-integra-item.codigo.
                 es-it-depto.origem         = "Carga via Web Service".
                 es-it-depto.usuar-altera   = "Sistema".
                 es-it-depto.usuario        = "Sistema".
      END.
    
    
      ASSIGN c-hora = STRING(TIME,"hh:mm").
             c-data = STRING(TODAY,"99/99/9999").
    
      CREATE ES-MOVTO-EXT-ITEM-CFA.                                                            
       ASSIGN es-movto-ext-item-cfa.classe            = "9999" .        
              es-movto-ext-item-cfa.dt-movto          = TODAY.                                 
              es-movto-ext-item-cfa.ep-codigo         = es-integra-item.empresasol.        
              es-movto-ext-item-cfa.hr-movto          = c-hora.                                 
              es-movto-ext-item-cfa.identificador     = 1 .                                     
              es-movto-ext-item-cfa.it-codigo         = es-integra-item.codigo.        
              es-movto-ext-item-cfa.pergunta-literal  = IF c-modo = "inclusao" THEN  "IMPLANTAÄ«O DO ITEM NO DATASUL ORIGEM JDE" ELSE "IMPLATAÄAO DE EXPANSAO NO DATASUL ORIGEM JDE".                      
              es-movto-ext-item-cfa.resposta-literal  = IF c-modo = "inclusao" THEN  "IMPLANTAÄ«O DO ITEM NO DATASUL ORIGEM JDE" ELSE "IMPLATAÄAO DE EXPANSAO NO DATASUL ORIGEM JDE".                     
              es-movto-ext-item-cfa.usuario           = "SISTEMA".
    
      FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item BY es-integra-questionario.seq:
    
          FIND FIRST es-movto-ext-item-cfa  WHERE es-movto-ext-item-cfa.it-codigo     = es-integra-item.codigo
                                            AND   es-movto-ext-item-cfa.ep-codigo     = es-integra-item.empresasol
                                            AND   es-movto-ext-item-cfa.nr-seq        = es-integra-questionario.seq
                                            AND   es-movto-ext-item-cfa.identificador = 2  EXCLUSIVE-LOCK NO-ERROR.
    
          IF NOT AVAIL es-movto-ext-item-cfa THEN DO:
              CREATE es-movto-ext-item-cfa.
                     es-movto-ext-item-cfa.classe                 = "9999".
                     es-movto-ext-item-cfa.dt-movto               = date(es-integra-item.QuestFiscalData).
                     es-movto-ext-item-cfa.ep-codigo              = es-integra-item.empresasol.
                     es-movto-ext-item-cfa.hr-movto               = es-integra-item.QuestFiscalHora.
                     es-movto-ext-item-cfa.identificador          = 2.
                     es-movto-ext-item-cfa.it-codigo              = es-integra-item.codigo.
                     es-movto-ext-item-cfa.nr-seq                 = es-integra-questionario.seq.
                     es-movto-ext-item-cfa.pergunta-literal       = es-integra-questionario.pergunta.
                     es-movto-ext-item-cfa.resposta-literal       = IF es-integra-questionario.seq = 204 THEN  es-integra-questionario.resposta  ELSE es-integra-questionario.resposta + "|" + es-integra-questionario.DescricaoResposta.
                     es-movto-ext-item-cfa.usuario                = es-integra-item.QuestFiscalUsu.
          END.
          ELSE DO:
                ASSIGN es-movto-ext-item-cfa.pergunta-literal = es-integra-questionario.pergunta.
                       es-movto-ext-item-cfa.resposta-literal = IF es-integra-questionario.seq = 204 THEN  es-integra-questionario.resposta  ELSE es-integra-questionario.resposta + "|" + es-integra-questionario.DescricaoResposta.
                           
          END.
      END.
    
      FIND FIRST ext-item-cfa WHERE ext-item-cfa.it-codigo   = es-integra-item.codigo
                              AND   ext-item-cfa.ep-codigo   = es-integra-item.empresasol
                              AND   ext-item-cfa.IdKlassmatt = es-integra-item.IdKlassmatt
                              EXCLUSIVE-LOCK NO-ERROR.
    
      IF NOT AVAIL ext-item-cfa  THEN DO:
            CREATE ext-item-cfa.                                                        
            ASSIGN ext-item-cfa.it-codigo   = es-integra-item.codigo.                   
                   ext-item-cfa.ep-codigo   = es-integra-item.empresasol.               
                   ext-item-cfa.IdKlassmatt = es-integra-item.IdKlassmatt.              
                   ext-item-cfa.IdSIN       = es-integra-item.IdSIN.                    
                   ext-item-cfa.classe      = "9999".
      END.
      ELSE 
          ASSIGN ext-item-cfa.classe      = "9999".
    
      CREATE es-integra-retorno.                                                                                              
      ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo. 
             es-integra-retorno.ep-codigo       = es-integra-item.empresasol.        
             es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
             es-integra-retorno.idsin           = es-integra-item.idsin.
             es-integra-retorno.dt-carga        = TODAY.                                                                      
             es-integra-retorno.dt-int-erp      = TODAY.                                                                      
             es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.                                                
             es-integra-retorno.log-retorno     =  IF c-modo = "inclusao" THEN  "Item integrado com sucesso no ERP ORIGEM JDE." ELSE  "Expans∆o do item " + es-integra-item.codigo + " integrada com sucesso!!! ORIGEM JDE." .          
             es-integra-retorno.statusRetorno   = "S".
    

      RUN pi-grava-log-monitor.

      FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:   
          DELETE es-integra-questionario.                                   
      END.                                                                  
      DELETE es-integra-item.
  END.
/*end REV02*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-erros-cria-item) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-erros-cria-item Procedure 
PROCEDURE pi-erros-cria-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF es-integra-item.modo = 1  THEN DO:
    IF SUBSTR(es-integra-item.codigo,1,2) <> "XX" AND SUBSTRING(es-integra-item.codigo,LENGTH(es-integra-item.codigo),1) <> "R"  THEN DO:
       
        CREATE es-integra-retorno.
        ASSIGN  es-integra-retorno.codigo          = es-integra-item.codigo.     
                es-integra-retorno.ep-codigo       = es-integra-item.empresasol.
                es-integra-retorno.idsin           = es-integra-item.idsin.
                es-integra-retorno.dt-carga        = TODAY.  
                es-integra-retorno.dt-int-erp      = TODAY.  
                es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.      
                es-integra-retorno.log-retorno     = "Item " + es-integra-item.codigo +  "nao pode ser inclusao". 
                es-integra-retorno.statusRetorno   = "N". 

      RUN pi-grava-log-monitor.

      FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:      
          DELETE es-integra-questionario.                       
      END.                                                      
      DELETE es-integra-item.
      NEXT.                                                     

    END.
END.


IF es-integra-item.modo = 1  THEN DO:
    IF SUBSTR(es-integra-item.codigo,1,2) = "XX" AND SUBSTRING(es-integra-item.codigo,LENGTH(es-integra-item.codigo),1) = "R"  THEN DO:
       
      CREATE es-integra-retorno.
      ASSIGN  es-integra-retorno.codigo          = es-integra-item.codigo.     
              es-integra-retorno.ep-codigo       = es-integra-item.empresasol.
              es-integra-retorno.idsin           = es-integra-item.idsin.
              es-integra-retorno.dt-carga        = TODAY.  
              es-integra-retorno.dt-int-erp      = TODAY.  
              es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.      
              es-integra-retorno.log-retorno     = "Item " + es-integra-item.codigo +  "nao pode ser inclusao". 
              es-integra-retorno.statusRetorno   = "N". 

      RUN pi-grava-log-monitor.

      FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:      
          DELETE es-integra-questionario.                       
      END.                                                      
      DELETE es-integra-item.
      NEXT.                                                     

    END.
END.





IF es-integra-item.modo = 2  THEN DO:                                                                                        
    IF SUBSTR(es-integra-item.codigo,1,2) = "XX" THEN 
    DO:                                                                                                                                                                                             
      CREATE es-integra-retorno.                                                                                           
      ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.                                                   
             es-integra-retorno.ep-codigo       = es-integra-item.empresasol.
             es-integra-retorno.idsin           = es-integra-item.idsin.
             es-integra-retorno.dt-carga        = TODAY.                                                                    
             es-integra-retorno.dt-int-erp      = TODAY.                                                                    
             es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.                                              
             es-integra-retorno.log-retorno     = "Item " + es-integra-item.codigo +  "nao pode ser alterado".              
             es-integra-retorno.statusRetorno   = "N".  

      RUN pi-grava-log-monitor.
      
      FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:                                                           
          DELETE es-integra-questionario.                                                                            
      END.                                                                                                           
      DELETE es-integra-item.
      NEXT.                                                                                                                                                                                                                                        
    END.                                                                                                                     
END.                                                                                                                         

FIND FIRST  es-integra-questionario WHERE es-integra-questionario.IdKlassmatt = es-integra-item.IdKlassmatt  NO-LOCK NO-ERROR.
IF NOT AVAIL es-integra-questionario THEN 
DO:
   CREATE es-integra-retorno.
   ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.
          es-integra-retorno.ep-codigo       = es-integra-item.empresasol.
          es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
          es-integra-retorno.idsin           = es-integra-item.idsin.
          es-integra-retorno.dt-carga        = TODAY.
          es-integra-retorno.dt-int-erp      = TODAY.
          es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.
          es-integra-retorno.log-retorno     = "Item sem requestionario de tax".
          es-integra-retorno.statusRetorno   = "N". 

   RUN pi-grava-log-monitor.

   FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:                                                           
       DELETE es-integra-questionario.                                                                            
   END.                                                                                                           
   DELETE es-integra-item.
   NEXT.                                                                                                                                                                                                                                        

END.

IF es-integra-item.Codigo  = ""  THEN 
DO:
   CREATE es-integra-retorno.                                                       
   ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.
          es-integra-retorno.ep-codigo       = es-integra-item.empresasol.          
          es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
          es-integra-retorno.idsin           = es-integra-item.idsin.
          es-integra-retorno.dt-carga        = TODAY.                               
          es-integra-retorno.dt-int-erp      = TODAY.                               
          es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.         
          es-integra-retorno.log-retorno     = "Item sem codigo".    
          es-integra-retorno.statusRetorno   = "N".                                 
   
   RUN pi-grava-log-monitor.
   
   FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:                                                           
       DELETE es-integra-questionario.                                                                            
   END.                                                                                                           
   DELETE es-integra-item.
   NEXT.                                                                                                                                                                                                                                     
END.

IF es-integra-item.FormSupDepart = "" THEN DO:
   CREATE es-integra-retorno.                                                                     
   ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.
          es-integra-retorno.ep-codigo       = es-integra-item.empresasol.          
          es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
          es-integra-retorno.idsin           = es-integra-item.idsin.
          es-integra-retorno.dt-carga        = TODAY.                                             
          es-integra-retorno.dt-int-erp      = TODAY.                                             
          es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.                       
          es-integra-retorno.log-retorno     = "Item sem Codigo de Departamento".      
          es-integra-retorno.statusRetorno   = "N".                                               
                                                                        
   RUN pi-grava-log-monitor.

   FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:                                                           
       DELETE es-integra-questionario.                                                                            
   END.                                                                                                           
   DELETE es-integra-item.
   NEXT.                                                                                                                                                                                                                                        

END.

IF es-integra-item.ncm = "" AND es-integra-item.tipo <> "9" THEN DO:
   CREATE es-integra-retorno.                                                                 
   ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.
          es-integra-retorno.ep-codigo       = es-integra-item.empresasol.          
          es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
          es-integra-retorno.idsin           = es-integra-item.idsin.
          es-integra-retorno.dt-carga        = TODAY.                                         
          es-integra-retorno.dt-int-erp      = TODAY.                                         
          es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.                   
          es-integra-retorno.log-retorno     = "Item sem NCM".             
          es-integra-retorno.statusRetorno   = "N".                                           
                                                                                              
   RUN pi-grava-log-monitor.

   FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:                                       
       DELETE es-integra-questionario.                                                        
   END.                                                                                       
   DELETE es-integra-item.
   NEXT.
END.

IF es-integra-item.ncm <> "" THEN DO:

    FIND classif-fisc WHERE classif-fisc.class-fiscal = es-integra-item.ncm NO-LOCK NO-ERROR.
    IF NOT AVAIL classif-fisc  THEN DO:
         CREATE es-integra-retorno.                                                        
         ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.               
                es-integra-retorno.ep-codigo       = es-integra-item.empresasol.           
                es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.   
                es-integra-retorno.idsin           = es-integra-item.idsin.                
                es-integra-retorno.dt-carga        = TODAY.                                
                es-integra-retorno.dt-int-erp      = TODAY.                                
                es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.          
                es-integra-retorno.log-retorno     = "NCM n∆o cadastrada no ERP".                       
                es-integra-retorno.statusRetorno   = "N". 

         RUN pi-grava-log-monitor.
                                                                                           
         FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:               
             DELETE es-integra-questionario.                                               
         END.                                                                              
         DELETE es-integra-item.
         NEXT.
    END.
END.


IF es-integra-item.DsRes = "" THEN DO:                                                 
       CREATE es-integra-retorno.                                                    
       ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.
              es-integra-retorno.ep-codigo       = es-integra-item.empresasol.          
              es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
              es-integra-retorno.idsin           = es-integra-item.idsin.
              es-integra-retorno.dt-carga        = TODAY.                            
              es-integra-retorno.dt-int-erp      = TODAY.                            
              es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.      
              es-integra-retorno.log-retorno     = "Item sem descricao resumida".                   
              es-integra-retorno.statusRetorno   = "N".     

       RUN pi-grava-log-monitor.
                                                                                     
       FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:                          
           DELETE es-integra-questionario.                                           
       END.                                                                          
       DELETE es-integra-item.
       NEXT.                                                                         
END.
 IF es-integra-item.DsComp = "" THEN DO:                                                            
        CREATE es-integra-retorno.                                                                 
        ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.
               es-integra-retorno.ep-codigo       = es-integra-item.empresasol.          
               es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
               es-integra-retorno.idsin           = es-integra-item.idsin.
               es-integra-retorno.dt-carga        = TODAY.                                         
               es-integra-retorno.dt-int-erp      = TODAY.                                         
               es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.                   
               es-integra-retorno.log-retorno     = "Item sem descricao completa".                 
               es-integra-retorno.statusRetorno   = "N".    

        RUN pi-grava-log-monitor.
                                                                                                   
        FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:                                       
            DELETE es-integra-questionario.                                                        
        END.                                                                                       
        DELETE es-integra-item.
        NEXT.                                                                                      
 END.
    
IF es-integra-item.EmpresaSol = "" OR es-integra-item.EstabelecimentoSol = "" THEN DO:
      CREATE es-integra-retorno.                                                     
      ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.
             es-integra-retorno.ep-codigo       = es-integra-item.empresasol.          
             es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
             es-integra-retorno.idsin           = es-integra-item.idsin.
             es-integra-retorno.dt-carga        = TODAY.                             
             es-integra-retorno.dt-int-erp      = TODAY.                             
             es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.       
             es-integra-retorno.log-retorno     = "Item sem Codigo de empresa/Estabelecimento".                 
             es-integra-retorno.statusRetorno   = "N".                               
      
      RUN pi-grava-log-monitor.

      FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:      
          DELETE es-integra-questionario.                       
      END.
      DELETE es-integra-item.

      NEXT.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-erros-cria-item-jde) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-erros-cria-item-jde Procedure 
PROCEDURE pi-erros-cria-item-jde :
/*------------------------------------------------------------------------------
  Purpose:  Begins REV02
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND FIRST  es-integra-questionario WHERE es-integra-questionario.IdKlassmatt = es-integra-item.IdKlassmatt  NO-LOCK NO-ERROR.
IF NOT AVAIL es-integra-questionario THEN DO:
      CREATE es-integra-retorno.
      ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.
             es-integra-retorno.ep-codigo       = es-integra-item.empresasol.
             es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
             es-integra-retorno.idsin           = es-integra-item.idsin.
             es-integra-retorno.dt-carga        = TODAY.
             es-integra-retorno.dt-int-erp      = TODAY.
             es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.
             es-integra-retorno.log-retorno     = "Item sem requestionario de tax".
             es-integra-retorno.statusRetorno   = "N". 

      RUN pi-grava-log-monitor.

      FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:
          DELETE es-integra-questionario.
      END.
      DELETE es-integra-item.

      NEXT.
END.

IF es-integra-item.Codigo  = ""  THEN DO:
    CREATE es-integra-retorno.                                                       
    ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.
           es-integra-retorno.ep-codigo       = es-integra-item.empresasol.          
           es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
           es-integra-retorno.idsin           = es-integra-item.idsin.
           es-integra-retorno.dt-carga        = TODAY.                               
           es-integra-retorno.dt-int-erp      = TODAY.                               
           es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.         
           es-integra-retorno.log-retorno     = "Item sem codigo".    
           es-integra-retorno.statusRetorno   = "N".                                 
    
    RUN pi-grava-log-monitor.

    FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:  
        DELETE es-integra-questionario.                   
    END.
    DELETE es-integra-item.
    NEXT.
END.

IF es-integra-item.FormSupDepart = "" THEN DO:
     CREATE es-integra-retorno.                                                                     
     ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.
            es-integra-retorno.ep-codigo       = es-integra-item.empresasol.          
            es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
            es-integra-retorno.idsin           = es-integra-item.idsin.
            es-integra-retorno.dt-carga        = TODAY.                                             
            es-integra-retorno.dt-int-erp      = TODAY.                                             
            es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.                       
            es-integra-retorno.log-retorno     = "Item sem Codigo de Departamento".      
            es-integra-retorno.statusRetorno   = "N".                                               
                                                                          
     RUN pi-grava-log-monitor.

     FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:                                           
         DELETE es-integra-questionario.                                                            
     END.
     DELETE es-integra-item.
     NEXT.
END.

IF es-integra-item.ncm = "" AND es-integra-item.tipo <> "9" THEN DO:
       CREATE es-integra-retorno.                                                                 
       ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.
              es-integra-retorno.ep-codigo       = es-integra-item.empresasol.          
              es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
              es-integra-retorno.idsin           = es-integra-item.idsin.
              es-integra-retorno.dt-carga        = TODAY.                                         
              es-integra-retorno.dt-int-erp      = TODAY.                                         
              es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.                   
              es-integra-retorno.log-retorno     = "Item sem NCM".             
              es-integra-retorno.statusRetorno   = "N".                                           
                                                                                                  
       RUN pi-grava-log-monitor.

       FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:                                       
           DELETE es-integra-questionario.                                                        
       END.                                                                                       
       DELETE es-integra-item.
       NEXT.
END.

IF es-integra-item.ncm <> "" THEN DO:

    FIND classif-fisc WHERE classif-fisc.class-fiscal = es-integra-item.ncm NO-LOCK NO-ERROR.
    IF NOT AVAIL classif-fisc  THEN DO:
         CREATE es-integra-retorno.                                                        
         ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.               
                es-integra-retorno.ep-codigo       = es-integra-item.empresasol.           
                es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.   
                es-integra-retorno.idsin           = es-integra-item.idsin.                
                es-integra-retorno.dt-carga        = TODAY.                                
                es-integra-retorno.dt-int-erp      = TODAY.                                
                es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.          
                es-integra-retorno.log-retorno     = "NCM n∆o cadastrada no ERP".                       
                es-integra-retorno.statusRetorno   = "N".                                  
                                                                                           
         RUN pi-grava-log-monitor.

         FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:               
             DELETE es-integra-questionario.                                               
         END.                                                                              
         DELETE es-integra-item.
         NEXT.
    END.
END.


IF es-integra-item.DsRes = "" THEN DO:                                                 
       CREATE es-integra-retorno.                                                    
       ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.
              es-integra-retorno.ep-codigo       = es-integra-item.empresasol.          
              es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
              es-integra-retorno.idsin           = es-integra-item.idsin.
              es-integra-retorno.dt-carga        = TODAY.                            
              es-integra-retorno.dt-int-erp      = TODAY.                            
              es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.      
              es-integra-retorno.log-retorno     = "Item sem descricao resumida".                   
              es-integra-retorno.statusRetorno   = "N".                              
                                                                                     
       RUN pi-grava-log-monitor.

       FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:                          
           DELETE es-integra-questionario.                                           
       END.                                                                          
       DELETE es-integra-item.
       NEXT.                                                                         
END.

IF es-integra-item.DsComp = "" THEN DO:                                                            
       CREATE es-integra-retorno.                                                                 
       ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.
              es-integra-retorno.ep-codigo       = es-integra-item.empresasol.          
              es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
              es-integra-retorno.idsin           = es-integra-item.idsin.
              es-integra-retorno.dt-carga        = TODAY.                                         
              es-integra-retorno.dt-int-erp      = TODAY.                                         
              es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.                   
              es-integra-retorno.log-retorno     = "Item sem descricao completa".                 
              es-integra-retorno.statusRetorno   = "N".                                           
                                                                                                  

       RUN pi-grava-log-monitor.

       FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:                                       
           DELETE es-integra-questionario.                                                        
       END.                                                                                       
       DELETE es-integra-item.
       NEXT.                                                                                      
END.
   
IF es-integra-item.EmpresaSol = "" OR es-integra-item.EstabelecimentoSol = "" THEN DO:
      CREATE es-integra-retorno.                                                     
      ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.
             es-integra-retorno.ep-codigo       = es-integra-item.empresasol.          
             es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
             es-integra-retorno.idsin           = es-integra-item.idsin.
             es-integra-retorno.dt-carga        = TODAY.                             
             es-integra-retorno.dt-int-erp      = TODAY.                             
             es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.       
             es-integra-retorno.log-retorno     = "Item sem Codigo de empresa/Estabelecimento".                 
             es-integra-retorno.statusRetorno   = "N".                               
      
      RUN pi-grava-log-monitor.

      FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:      
          DELETE es-integra-questionario.                       
      END.
      DELETE es-integra-item.

      NEXT.
END.

/*end REV02*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-expansao) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-expansao Procedure 
PROCEDURE pi-expansao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE l-emilidado                       AS LOGICAL     NO-UNDO.

/*FIND ITEM WHERE ITEM.it-codigo = es-integra-item.codigo NO-LOCK NO-ERROR.*/

FOR FIRST ITEM FIELDS (it-codigo cod-obsoleto) NO-LOCK
    WHERE ITEM.it-codigo = es-integra-item.codigo :
END.

IF NOT  AVAIL ITEM THEN DO:
     CREATE es-integra-retorno.                                                       
     ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.
            es-integra-retorno.idsin           = es-integra-item.idsin.
            es-integra-retorno.ep-codigo       = es-integra-item.empresasol.          
            es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
            es-integra-retorno.idsin           = es-integra-item.idsin.
            es-integra-retorno.dt-carga        = TODAY.                               
            es-integra-retorno.dt-int-erp      = TODAY.                               
            es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.         
            es-integra-retorno.log-retorno     = "Item nao existe no ERP para expansao".    
            es-integra-retorno.statusRetorno   = "N". 

       RUN pi-grava-log-monitor.

       FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:              
           DELETE es-integra-questionario.                               
       END.                                                              
       DELETE es-integra-item.

     
     NEXT.                                                   

END.
ELSE DO:
    IF ITEM.cod-obsoleto <> 4 THEN DO:
        CREATE es-integra-retorno.                                                             
        ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.
               es-integra-retorno.ep-codigo       = es-integra-item.empresasol.
               es-integra-retorno.idsin           = es-integra-item.idsin.
               es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
               es-integra-retorno.idsin           = es-integra-item.idsin.
               es-integra-retorno.dt-carga        = TODAY.                                     
               es-integra-retorno.dt-int-erp      = TODAY.                                     
               es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.               
               es-integra-retorno.log-retorno     = "Item para expansao nao esta obsoleto".    
               es-integra-retorno.statusRetorno   = "N".                                       
                                                                                               
        RUN pi-grava-log-monitor.

        FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:                                   
            DELETE es-integra-questionario.                                                    
        END.
        DELETE es-integra-item.
        NEXT.
    END.
    
    
/*          RUN pi-elimina-item (OUTPUT l-emilidado). */
/*          IF l-emilidado THEN DO: */
             RUN pi-cria-item (INPUT "expansao").
/*          END. */
         /* ELSE DO:                                                                                           */
         /*    CREATE es-integra-retorno.                                                                      */
         /*    ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.                             */
         /*           es-integra-retorno.ep-codigo       = es-integra-item.empresasol.                         */
         /*           es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.                 */
         /*           es-integra-retorno.dt-carga        = TODAY.                                              */
         /*           es-integra-retorno.dt-int-erp      = TODAY.                                              */
         /*           es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.                        */
         /*           es-integra-retorno.log-retorno     = "Nao foi possivel eliminar o item para expansao".   */
         /*           es-integra-retorno.statusRetorno   = "N".                                                */
         /*                                                                                                    */
         /*    FOR EACH es-integra-questionario OF es-integra-item:                                            */
         /*        DELETE es-integra-questionario.                                                             */
         /*    END.                                                                                            */
         /*    DELETE es-integra-item.                                                                         */
         /*    NEXT.                                                                                           */
/*          END. */






    

END.






END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-processa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa Procedure 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN pi-processa-fila.
RUN pi-console-xml-integra.

RUN pi-carrega.

RUN pi-retorno-integra.

RETURN "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-processa-fila) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa-fila Procedure 
PROCEDURE pi-processa-fila :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN esp\ymcd0203-01.p.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-replica-item) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-replica-item Procedure 
PROCEDURE pi-replica-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH ems2cadme.empresa FIELDS(ep-codigo)NO-LOCK,
    EACH bco_empres WHERE bco_empres.cod_bco_logic = "ems2cademp"
                    AND   bco_empres.cod_empresa   = empresa.ep-codigo NO-LOCK:


    FIND FIRST es-emp-desativada 
         WHERE es-emp-desativada.ep-codigo = bco_empres.cod_empresa NO-ERROR.
   IF AVAIL(es-emp-desativada) THEN NEXT.


    FIND FIRST trad_org_ext WHERE trad_org_ext.cod_tip_unid_organ      = "998"                  
                            AND   trad_org_ext.cod_matriz_trad_org_ext = "ems2"            
                            AND   trad_org_ext.cod_unid_organ_ext      = bco_empres.cod_empresa NO-LOCK NO-ERROR. 
    IF AVAIL(trad_org_ext) THEN DO:
        FIND FIRST ems5.unid_organ NO-LOCK
             WHERE ems5.unid_organ.cod_tip_unid_organ      = "998"
               AND ems5.unid_organ.cod_unid_organ = trad_org_ext.cod_unid_organ NO-ERROR.
        IF AVAIL(ems5.unid_organ) THEN DO:
    
            IF ems5.unid_organ.dat_fim_valid < TODAY THEN NEXT.
    
        END.
    END.


    FIND FIRST es-fila-rep-item WHERE es-fila-rep-item.ep-codigo  = bco_empres.cod_empresa
                                AND   es-fila-rep-item.it-codigo  = ITEM.it-codigo NO-ERROR.

    IF NOT AVAIL es-fila-rep-item  THEN DO:
    

     
     CREATE es-fila-rep-item.                                                      
         es-fila-rep-item.char-2        = ITEM.char-2.                             
         es-fila-rep-item.class-fiscal  = es-integra-item.ncm.                       
         es-fila-rep-item.cod-depto     = int(es-integra-item.FormSupDepart).      
         es-fila-rep-item.cod-servico   = ITEM.cod-servico.                        
         es-fila-rep-item.codigo-orig   = ITEM.codigo-orig.                        
         es-fila-rep-item.codigo-refer  = es-integra-item.CodigoCompl.                      
         es-fila-rep-item.data-implant  = ITEM.data-implant.                       
         es-fila-rep-item.dec-1         = ITEM.dec-1 .                             
         es-fila-rep-item.desc-item     = es-integra-item.dsres.                          
         es-fila-rep-item.ep-codigo     = bco_empres.cod_empresa.  

         /* Sergio - 12/01/2017 */
         es-fila-rep-item.fm-cod-com   = IF es-integra-item.FormSupFamComEqpto = "NA" THEN "" ELSE  es-integra-item.FormSupFamComEqpto.
         es-fila-rep-item.fm-codigo    = es-integra-item.Familia.

/*          es-fila-rep-item.fm-cod-com    = ITEM.fm-cod-com. */
/*          es-fila-rep-item.fm-codigo     = ITEM.fm-codigo.  */
         es-fila-rep-item.ge-codigo     = 99. /*ITEM.ge-codigo.*/
         es-fila-rep-item.it-codigo     = ITEM.it-codigo.                          
         es-fila-rep-item.narrativa     = replace(es-integra-item.dscomp,"CHR(10)",CHR(10)).                          
         es-fila-rep-item.quant-segur   = ITEM.quant-segur.                        
         es-fila-rep-item.tipo-contr    = ITEM.tipo-contr.                         
         es-fila-rep-item.un            = es-integra-item.um.
    END.
    ELSE DO:
         es-fila-rep-item.char-2        = ITEM.char-2.                             
         es-fila-rep-item.class-fiscal  = es-integra-item.ncm.                     
         es-fila-rep-item.cod-depto     = int(es-integra-item.FormSupDepart).      
         es-fila-rep-item.cod-servico   = ITEM.cod-servico.                        
         es-fila-rep-item.codigo-orig   = ITEM.codigo-orig.                        
         es-fila-rep-item.codigo-refer  = es-integra-item.CodigoCompl.                       
         es-fila-rep-item.data-implant  = ITEM.data-implant.                       
         es-fila-rep-item.dec-1         = ITEM.dec-1 .                             
         es-fila-rep-item.desc-item     = es-integra-item.dsres.  

         /* Sergio - 12/01/2017 */
         es-fila-rep-item.fm-cod-com   = IF es-integra-item.FormSupFamComEqpto = "NA" THEN "" ELSE  es-integra-item.FormSupFamComEqpto.
         es-fila-rep-item.fm-codigo    = es-integra-item.Familia.

/*          es-fila-rep-item.fm-cod-com    = ITEM.fm-cod-com. */
/*          es-fila-rep-item.fm-codigo     = ITEM.fm-codigo.  */
         es-fila-rep-item.ge-codigo     = ITEM.ge-codigo.
         es-fila-rep-item.narrativa     = replace(es-integra-item.dscomp,"CHR(10)",CHR(10)).                  
         es-fila-rep-item.quant-segur   = ITEM.quant-segur.                        
         es-fila-rep-item.tipo-contr    = ITEM.tipo-contr.                         
         es-fila-rep-item.un            = es-integra-item.um.
    END.
         
             
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-ret-tax) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ret-tax Procedure 
PROCEDURE pi-ret-tax :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE i-ident AS INTEGER     NO-UNDO.

/*FIND ITEM WHERE ITEM.it-codigo = es-integra-item.codigo NO-LOCK NO-ERROR.*/
FOR FIRST ITEM FIELDS (it-codigo cod-obsoleto) NO-LOCK
    WHERE ITEM.it-codigo = es-integra-item.codigo:
END.

IF NOT AVAIL ITEM THEN DO:
     CREATE es-integra-retorno.                                                                        
     ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.
            es-integra-retorno.ep-codigo       = es-integra-item.empresasol.          
            es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
            es-integra-retorno.idsin           = es-integra-item.idsin.
            es-integra-retorno.dt-carga        = TODAY.                                                
            es-integra-retorno.dt-int-erp      = TODAY.                                                
            es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.                          
            es-integra-retorno.log-retorno     = "Item nao cadastro no ERP ".                     
            es-integra-retorno.statusRetorno   = "N".                                                 
                                                                                                       
     RUN pi-grava-log-monitor.

     FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:                                              
         DELETE es-integra-questionario.                                                               
     END.                                                                                              
     DELETE es-integra-item.
     NEXT.
END.
ELSE DO:
    FIND ext-item-cfa WHERE ext-item-cfa.it-codigo   = ITEM.it-codigo
                      AND   ext-item-cfa.ep-codigo   = es-integra-item.empresasol
                      NO-LOCK NO-ERROR.


    IF es-integra-item.MODO <> 4 THEN  DO:
    
        IF ITEM.cod-obsoleto <> 4             OR
           SUBSTR(ITEM.it-codigo,1,2) <> "XX" OR 
           NOT AVAIL  ext-item-cfa            OR
           ext-item-cfa.classe <> "9999"  THEN DO:
    
    
            CREATE es-integra-retorno.                                                       
            ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.
                   es-integra-retorno.ep-codigo       = es-integra-item.empresasol.          
                   es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
                   es-integra-retorno.idsin           = es-integra-item.idsin.
                   es-integra-retorno.dt-carga        = TODAY.                               
                   es-integra-retorno.dt-int-erp      = TODAY.                               
                   es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.         
                   es-integra-retorno.log-retorno     = "Item nao cadastro no ERP ".         
                   es-integra-retorno.statusRetorno   = "N".                                 
                                                                                             
            RUN pi-grava-log-monitor.

            FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:                             
                DELETE es-integra-questionario.                                              
            END.                                                                             
            DELETE es-integra-item.
            NEXT.
    
        END.
    END.
END.


RUN pi-retorna-ident(INPUT  es-integra-item.codigo,
                     INPUT  es-integra-item.empresasol,
                     OUTPUT i-ident).


ASSIGN i-ident = i-ident + 1.

FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item BY es-integra-questionario.seq:                                                                            
                                                                                                                                                               
                                                                                                                                                               
    FIND es-movto-ext-item-cfa WHERE es-movto-ext-item-cfa.ep-codigo     = es-integra-item.empresasol                                                          
                               AND   es-movto-ext-item-cfa.it-codigo     = es-integra-item.codigo                                                              
                               AND   es-movto-ext-item-cfa.identificador = i-ident                                                                                    
                               AND   es-movto-ext-item-cfa.nr-seq        = es-integra-questionario.seq NO-LOCK NO-ERROR.                                       
                                                                                                                                                               
    IF NOT AVAIL es-movto-ext-item-cfa THEN DO:                                                                                                                
                                                                                                                                                               
        CREATE es-movto-ext-item-cfa.                                                                                                                          
               es-movto-ext-item-cfa.classe                 = "9999".                                                                                          
               es-movto-ext-item-cfa.dt-movto               = date(es-integra-item.QuestFiscalData).                                                           
               es-movto-ext-item-cfa.ep-codigo              = es-integra-item.empresasol.                                                                      
               es-movto-ext-item-cfa.hr-movto               = es-integra-item.QuestFiscalHora.                                                                 
               es-movto-ext-item-cfa.identificador          = i-ident.                                                                                               
               es-movto-ext-item-cfa.it-codigo              = es-integra-item.codigo.                                                                          
               es-movto-ext-item-cfa.nr-seq                 = es-integra-questionario.seq.                                                                     
               es-movto-ext-item-cfa.pergunta-literal       = es-integra-questionario.pergunta.                                                                
               es-movto-ext-item-cfa.resposta-literal       = IF es-integra-questionario.seq = 204 THEN  es-integra-questionario.resposta  ELSE es-integra-questionario.resposta + "|" + es-integra-questionario.DescricaoResposta.              
               es-movto-ext-item-cfa.usuario                = es-integra-item.QuestFiscalUsu.                                                                  
    END.                                                                                                                                                       
                                                                                                                                                               
END.                                                                                                                                                                                                                                                                                                                  

CREATE es-integra-retorno.                                                       
ASSIGN es-integra-retorno.codigo          = es-integra-item.codigo.
       es-integra-retorno.ep-codigo       = es-integra-item.empresasol.          
       es-integra-retorno.cod-estabel     = es-integra-item.estabelecimentosol.
       es-integra-retorno.idsin           = es-integra-item.idsin.
       es-integra-retorno.dt-carga        = TODAY.                               
       es-integra-retorno.dt-int-erp      = TODAY.                               
       es-integra-retorno.IdKlassmatt     = es-integra-item.IdKlassmatt.         
       es-integra-retorno.log-retorno     = "Reenvio de TAX recebido com sucesso ".         
       es-integra-retorno.statusRetorno   = "S". 

RUN pi-grava-log-monitor.

FIND ext-item-cfa WHERE ext-item-cfa.it-codigo   = es-integra-item.codigo
                  AND   ext-item-cfa.ep-codigo   = es-integra-item.empresasol EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL ext-item-cfa  THEN 
DO:
   ASSIGN ext-item-cfa.classe = "9999".
   IF i-ep-codigo-usuario1 = c-empresa THEN DO:
   
       /*FIND ITEM WHERE ITEM.it-codigo = es-integra-item.codigo EXCLUSIVE-LOCK NO-ERROR.*/
       
       FOR FIRST ITEM FIELDS (it-codigo cod-obsoleto) EXCLUSIVE-LOCK
           WHERE ITEM.it-codigo = es-integra-item.codigo:
       END.
       IF AVAIL ITEM THEN DO:
           ITEM.cod-obsoleto = 4.
       END.
   
       /*FIND ITEM-uni-estab WHERE ITEM-uni-estab.it-codigo   = es-integra-item.codigo
                           AND   ITEM-uni-estab.cod-estabel = es-integra-item.estabelecimentosol exclusive-lock NO-ERROR.*/
   
       FOR FIRST item-uni-estab FIELDS (it-codigo cod-estabel cod-obsoleto) EXCLUSIVE-LOCK
           WHERE item-uni-estab.it-codigo   = es-integra-item.codigo
             AND item-uni-estab.cod-estabel = es-integra-item.estabelecimentosol:
       END.
   
       IF AVAIL item-uni-estab THEN DO:
           ASSIGN item-uni-estab.cod-obsoleto = 4.
       END.
   END.
END.

                                                                                     
FOR EACH es-integra-questionario EXCLUSIVE-LOCK OF es-integra-item:                             
    DELETE es-integra-questionario.                                              
END.
DELETE es-integra-item.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-retorna-ident) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-retorna-ident Procedure 
PROCEDURE pi-retorna-ident :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE i-conta2 AS INTEGER                NO-UNDO.
DEFINE INPUT  PARAMETER p-item      AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER p-empresa   AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER p-ident     AS INTEGER     NO-UNDO.

ASSIGN p-ident = 0.

FOR EACH es-movto-ext-item-cfa FIELDS(it-codigo ep-codigo nr-seq identificador) NO-LOCK
   WHERE es-movto-ext-item-cfa.it-codigo   = p-item
     AND es-movto-ext-item-cfa.ep-codigo   = p-empresa  
      BY es-movto-ext-item-cfa.identificador 
 DESC BY es-movto-ext-item-cfa.nr-seq :
         
    ASSIGN i-conta2 = i-conta2 + 1.
    IF i-conta2 = 1 THEN DO:
         ASSIGN p-ident = es-movto-ext-item-cfa.identificador.
         LEAVE.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-retorno-integra) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-retorno-integra Procedure 
PROCEDURE pi-retorno-integra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN esp\retornows.p.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-grava-log-monitor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava-log-monitor Procedure 
PROCEDURE pi-grava-log-monitor:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* ------------------------------------------------------------------------------ */
/* Begins Jan/2019 - Willians Ambrosio DKP                                        */             
/* ------------------------------------------------------------------------------ */
FIND FIRST es-klassmatt-integr WHERE
           es-klassmatt-integr.idklassmatt = es-integra-retorno.idklassmatt          AND
           es-klassmatt-integr.dt-trans    = TODAY                                   AND
           es-klassmatt-integr.hr-trans    = REPLACE(STRING(TIME,"HH:MM:SS"),":","") EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAIL es-klassmatt-integr THEN
DO:
   CREATE es-klassmatt-integr.
   BUFFER-COPY es-integra-retorno   TO es-klassmatt-integr
                                ASSIGN es-klassmatt-integr.dt-trans    = TODAY                                   
                                       es-klassmatt-integr.hr-trans    = STRING(TIME,"HH:MM:SS"). 
END.   
/* ------------------------------------------------------------------------------ */
/* End Jan/2019 - Willians Ambrosio DKP                                           */
/* ------------------------------------------------------------------------------ */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
